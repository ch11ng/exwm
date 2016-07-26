;;; exwm-input.el --- Input Module for EXWM  -*- lexical-binding: t -*-

;; Copyright (C) 2015-2016 Free Software Foundation, Inc.

;; Author: Chris Feng <chris.w.feng@gmail.com>

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This module deals with key/mouse matters, including:
;; + Input focus,
;; + Key/Button event handling,
;; + Key events filtering and simulation.

;; Todo:
;; + Pointer simulation mode (e.g. 'C-c 1'/'C-c 2' for single/double click,
;;   move with arrow keys).
;; + Simulation keys to mimic Emacs key bindings for text edit (redo, select,
;;   cancel, clear, etc). Some of them are not present on common keyboard
;;   (keycode = 0). May need to use XKB extension.

;;; Code:

(require 'xcb-keysyms)
(require 'exwm-core)

(defvar exwm-input-move-event 's-down-mouse-1
  "Emacs event to start moving a window.")
(defvar exwm-input-resize-event 's-down-mouse-3
  "Emacs event to start resizing a window.")

(defvar exwm-input--move-keysym nil)
(defvar exwm-input--move-mask nil)
(defvar exwm-input--resize-keysym nil)
(defvar exwm-input--resize-mask nil)

(defvar exwm-input--timestamp xcb:Time:CurrentTime
  "A recent timestamp received from X server.

It's updated in several occasions, and only used by `exwm-input--set-focus'.")

(defun exwm-input--set-focus (id)
  "Set input focus to window ID in a proper way."
  (when (exwm--id->buffer id)
    (with-current-buffer (exwm--id->buffer id)
      (if (and (not exwm--hints-input)
               (memq xcb:Atom:WM_TAKE_FOCUS exwm--protocols))
          (progn
            (exwm--log "Focus on #x%x with WM_TAKE_FOCUS" id)
            (xcb:+request exwm--connection
                (make-instance 'xcb:icccm:SendEvent
                               :destination id
                               :event (xcb:marshal
                                       (make-instance 'xcb:icccm:WM_TAKE_FOCUS
                                                      :window id
                                                      :time
                                                      exwm-input--timestamp)
                                       exwm--connection))))
        (exwm--log "Focus on #x%x with SetInputFocus" id)
        (xcb:+request exwm--connection
            (make-instance 'xcb:SetInputFocus
                           :revert-to xcb:InputFocus:PointerRoot
                           :focus id
                           :time xcb:Time:CurrentTime)))
      (exwm-input--set-active-window id)
      (xcb:flush exwm--connection))))

(defvar exwm-input--update-focus-window nil "The (Emacs) window to be focused.

This value should always be overwritten.")

(defun exwm-input--on-buffer-list-update ()
  "Run in `buffer-list-update-hook' to track input focus."
  (when (and (not (minibufferp)) ;Do not set input focus on minibuffer window.
             (eq (current-buffer) (window-buffer)) ;e.g. `with-temp-buffer'.
             (frame-parameter nil 'exwm-outer-id)) ;e.g. emacsclient frame.
    (setq exwm-input--update-focus-window (selected-window))
    (exwm-input--update-focus-defer)))

;; Input focus update requests should be accumulated for a short time
;; interval so that only the last one need to be processed.  This not
;; improves the overall performance, but avoids the problem of input
;; focus loop, which is a result of the interaction with Emacs frames.
;;
;; FIXME: The time interval is hard to decide and perhaps machine-dependent.
;;        A value too small can cause redundant updates of input focus,
;;        and even worse, dead loops.  OTOH a large value would bring
;;        laggy experience.
(defconst exwm-input--update-focus-interval 0.01
  "Time interval (in seconds) for accumulating input focus update requests.")

(defvar exwm-input--update-focus-lock nil
  "Lock for solving input focus update contention.")
(defvar exwm-input--update-focus-defer-timer nil "Timer for polling the lock.")
(defvar exwm-input--update-focus-timer nil
  "Timer for deferring the update of input focus.")

(defun exwm-input--update-focus-defer ()
  "Defer updating input focus."
  (when exwm-input--update-focus-defer-timer
    (cancel-timer exwm-input--update-focus-defer-timer))
  (if exwm-input--update-focus-lock
      (setq exwm-input--update-focus-defer-timer
            (run-with-idle-timer 0 nil
                                 #'exwm-input--update-focus-defer))
    (setq exwm-input--update-focus-defer-timer nil)
    (when exwm-input--update-focus-timer
      (cancel-timer exwm-input--update-focus-timer))
    (setq exwm-input--update-focus-timer
          (run-with-idle-timer exwm-input--update-focus-interval nil
                               #'exwm-input--update-focus
                               exwm-input--update-focus-window))))

(defvar exwm-workspace--current)
(defvar exwm-workspace--switch-history-outdated)
(defvar exwm-workspace-current-index)
(defvar exwm-workspace--minibuffer)

(declare-function exwm-layout--iconic-state-p "exwm-layout.el" (&optional id))
(declare-function exwm-layout--set-state "exwm-layout.el" (id state))
(declare-function exwm-workspace--minibuffer-own-frame-p "exwm-workspace.el")
(declare-function exwm-workspace-switch "exwm-workspace.el"
                  (frame-or-index &optional force))

(defun exwm-input--update-focus (window)
  "Update input focus."
  (setq exwm-input--update-focus-lock t)
  (when (and (window-live-p window)
             ;; Do not update input focus when there's an active minibuffer.
             (not (active-minibuffer-window)))
    (with-current-buffer (window-buffer window)
      (if (eq major-mode 'exwm-mode)
          (if (not (eq exwm--frame exwm-workspace--current))
              ;; Do not focus X windows on other workspace.
              (progn
                (set-frame-parameter exwm--frame 'exwm-urgency t)
                (setq exwm-workspace--switch-history-outdated t)
                (force-mode-line-update)
                ;; The application may have changed its input focus
                (select-window
                 (frame-selected-window exwm-workspace--current)))
            (exwm--log "Set focus on #x%x" exwm--id)
            (exwm-input--set-focus exwm--id)
            (when exwm--floating-frame
              ;; Adjust stacking orders of the floating container.
              (xcb:+request exwm--connection
                  (make-instance 'xcb:ConfigureWindow
                                 :window exwm--container
                                 :value-mask xcb:ConfigWindow:StackMode
                                 :stack-mode xcb:StackMode:Above))
              ;; This floating X window might be hide by `exwm-floating-hide'.
              (when (exwm-layout--iconic-state-p)
                (exwm-layout--set-state exwm--id
                                        xcb:icccm:WM_STATE:NormalState))
              (xcb:flush exwm--connection)))
        (when (eq (selected-window) window)
          (exwm--log "Focus on %s" window)
          (select-frame-set-input-focus (window-frame window) t)
          (exwm-input--set-active-window)
          (xcb:flush exwm--connection)))))
  (setq exwm-input--update-focus-lock nil))

(defun exwm-input--on-minibuffer-setup ()
  "Run in `minibuffer-setup-hook' to set input focus."
  ;; Set input focus on the Emacs frame
  (x-focus-frame (window-frame (minibuffer-selected-window))))

(defun exwm-input--set-active-window (&optional id)
  "Set _NET_ACTIVE_WINDOW."
  (xcb:+request exwm--connection
      (make-instance 'xcb:ewmh:set-_NET_ACTIVE_WINDOW
                     :window exwm--root
                     :data (or id xcb:Window:None))))

(defvar exwm-input--during-key-sequence nil
  "Non-nil indicates Emacs is waiting for more keys to form a key sequence.")
(defvar exwm-input--temp-line-mode nil
  "Non-nil indicates it's in temporary line-mode for char-mode.")

(defun exwm-input--finish-key-sequence ()
  "Mark the end of a key sequence (with the aid of `pre-command-hook')."
  (when (and exwm-input--during-key-sequence
             (not (equal [?\C-u] (this-single-command-keys))))
    (setq exwm-input--during-key-sequence nil)
    (when exwm-input--temp-line-mode
      (setq exwm-input--temp-line-mode nil)
      (exwm-input--release-keyboard))))

(declare-function exwm-floating--start-moveresize "exwm-floating.el"
                  (id &optional type))
(declare-function exwm-workspace--position "exwm-workspace.el" (frame))
(declare-function exwm-workspace--workspace-p "exwm-workspace.el" (workspace))

(defvar exwm-workspace--list)

(defun exwm-input--on-ButtonPress (data _synthetic)
  "Handle ButtonPress event."
  (let ((obj (make-instance 'xcb:ButtonPress))
        (mode xcb:Allow:SyncPointer)
        window buffer frame)
    (xcb:unmarshal obj data)
    (with-slots (detail time event state) obj
      (setq exwm-input--timestamp time)
      (setq window (get-buffer-window (exwm--id->buffer event) t)
            buffer (window-buffer window))
      (cond ((and (= state exwm-input--move-mask)
                  (= detail exwm-input--move-keysym)
                  ;; Either an undecorated or a floating X window.
                  (with-current-buffer buffer
                    (or (not (eq major-mode 'exwm-mode))
                        exwm--floating-frame)))
             ;; Move
             (exwm-floating--start-moveresize
              event xcb:ewmh:_NET_WM_MOVERESIZE_MOVE))
            ((and (= state exwm-input--resize-mask)
                  (= detail exwm-input--resize-keysym)
                  (with-current-buffer buffer
                    (or (not (eq major-mode 'exwm-mode))
                        exwm--floating-frame)))
             ;; Resize
             (exwm-floating--start-moveresize event))
            (t
             ;; Click to focus
             (unless (eq window (selected-window))
               (setq frame (window-frame window))
               (unless (eq frame exwm-workspace--current)
                 (if (exwm-workspace--workspace-p frame)
                     ;; The X window is on another workspace
                     (exwm-workspace-switch frame)
                   (with-current-buffer buffer
                     (when (and (eq major-mode 'exwm-mode)
                                (not (eq exwm--frame
                                         exwm-workspace--current)))
                       ;; The floating X window is on another workspace
                       (exwm-workspace-switch exwm--frame)))))
               ;; It has been reported that the `window' may have be deleted
               (if (window-live-p window)
                   (select-window window)
                 (setq window
                       (get-buffer-window (exwm--id->buffer event) t))
                 (when window (select-window window))))
             ;; The event should be replayed
             (setq mode xcb:Allow:ReplayPointer))))
    (xcb:+request exwm--connection
        (make-instance 'xcb:AllowEvents :mode mode :time xcb:Time:CurrentTime))
    (xcb:flush exwm--connection)))

(defun exwm-input--on-KeyPress (data _synthetic)
  "Handle KeyPress event."
  (let ((obj (make-instance 'xcb:KeyPress)))
    (xcb:unmarshal obj data)
    (setq exwm-input--timestamp (slot-value obj 'time))
    (if (eq major-mode 'exwm-mode)
        (funcall exwm--on-KeyPress obj)
      (exwm-input--on-KeyPress-char-mode obj))))

(defvar exwm-input--global-keys nil "Global key bindings.")
(defvar exwm-input--global-prefix-keys nil
  "List of prefix keys of global key bindings.")
(defvar exwm-input-prefix-keys
  '(?\C-c ?\C-x ?\C-u ?\C-h ?\M-x ?\M-` ?\M-& ?\M-:)
  "List of prefix keys EXWM should forward to Emacs when in line-mode.")
(defvar exwm-input--simulation-keys nil "Simulation keys in line-mode.")
(defvar exwm-input--simulation-prefix-keys nil
  "List of prefix keys of simulation keys in line-mode.")

(defun exwm-input--update-global-prefix-keys ()
  "Update `exwm-input--global-prefix-keys'."
  (when exwm--connection
    (let ((original exwm-input--global-prefix-keys)
          keysym keycode ungrab-key grab-key workspace)
      (setq exwm-input--global-prefix-keys nil)
      (dolist (i exwm-input--global-keys)
        (cl-pushnew (elt i 0) exwm-input--global-prefix-keys))
      (unless (equal original exwm-input--global-prefix-keys)
        (setq ungrab-key (make-instance 'xcb:UngrabKey
                                        :key xcb:Grab:Any :grab-window nil
                                        :modifiers xcb:ModMask:Any)
              grab-key (make-instance 'xcb:GrabKey
                                      :owner-events 0
                                      :grab-window nil
                                      :modifiers nil
                                      :key nil
                                      :pointer-mode xcb:GrabMode:Async
                                      :keyboard-mode xcb:GrabMode:Async))
        (dolist (w exwm-workspace--list)
          (setq workspace (frame-parameter w 'exwm-workspace))
          (setf (slot-value ungrab-key 'grab-window) workspace)
          (if (xcb:+request-checked+request-check exwm--connection ungrab-key)
              (exwm--log "Failed to ungrab keys")
            (dolist (k exwm-input--global-prefix-keys)
              (setq keysym (xcb:keysyms:event->keysym exwm--connection k)
                    keycode (xcb:keysyms:keysym->keycode exwm--connection
                                                         (car keysym)))
              (setf (slot-value grab-key 'grab-window) workspace
                    (slot-value grab-key 'modifiers) (cadr keysym)
                    (slot-value grab-key 'key) keycode)
              (when (or (not keycode)
                        (xcb:+request-checked+request-check exwm--connection
                            grab-key))
                (user-error "[EXWM] Failed to grab key: %s"
                            (single-key-description k))))))))))

(defun exwm-input-set-key (key command)
  "Set a global key binding."
  (interactive "KSet key globally: \nCSet key %s to command: ")
  (global-set-key key command)
  (cl-pushnew key exwm-input--global-keys)
  (when (called-interactively-p 'any)
    (exwm-input--update-global-prefix-keys)))

;; FIXME: Putting (t . EVENT) into `unread-command-events' does not really work
;;        as documented in Emacs 24.  Since inserting a conventional EVENT does
;;        add it into (this-command-keys) there, we use `unread-command-events'
;;        differently on Emacs 24 and 25.
(eval-and-compile
  (if (< emacs-major-version 26)
      (defsubst exwm-input--unread-event (event)
        (setq unread-command-events
              (append unread-command-events (list event))))
    (defsubst exwm-input--unread-event (event)
      (setq unread-command-events
            (append unread-command-events `((t . ,event)))))))

(defvar exwm-input-command-whitelist nil
  "A list of commands that when active all keys should be forwarded to Emacs.")
(make-obsolete-variable 'exwm-input-command-whitelist
                        "This variable can be safely removed." "25.1")

(defvar exwm-input--during-command nil
  "Indicate whether between `pre-command-hook' and `post-command-hook'.")

(defun exwm-input--on-KeyPress-line-mode (key-press)
  "Parse X KeyPress event to Emacs key event and then feed the command loop."
  (with-slots (detail state) key-press
    (let ((keysym (xcb:keysyms:keycode->keysym exwm--connection detail state))
          event minibuffer-window mode)
      (when (and keysym
                 (setq event (xcb:keysyms:keysym->event exwm--connection
                                                        keysym state))
                 (or exwm-input--during-key-sequence
                     exwm-input--during-command
                     (setq minibuffer-window (active-minibuffer-window))
                     (memq event exwm-input--global-prefix-keys)
                     (memq event exwm-input-prefix-keys)
                     (memq event exwm-input--simulation-prefix-keys)))
        (setq mode xcb:Allow:AsyncKeyboard)
        (unless minibuffer-window (setq exwm-input--during-key-sequence t))
        ;; Feed this event to command loop.  Also force it to be added to
        ;; `this-command-keys'.
        (exwm-input--unread-event event))
      (xcb:+request exwm--connection
          (make-instance 'xcb:AllowEvents
                         :mode (or mode xcb:Allow:ReplayKeyboard)
                         :time xcb:Time:CurrentTime))
      (xcb:flush exwm--connection))))

(defun exwm-input--on-KeyPress-char-mode (key-press)
  "Handle KeyPress event in char-mode."
  (with-slots (detail state) key-press
    (let ((keysym (xcb:keysyms:keycode->keysym exwm--connection detail state))
          event)
      (when (and keysym
                 (setq event (xcb:keysyms:keysym->event exwm--connection
                                                        keysym state)))
        (when (eq major-mode 'exwm-mode)
          ;; FIXME: This functionality seems not working, e.g. when this
          ;;        command would activate the minibuffer, the temporary
          ;;        line-mode would actually quit before the minibuffer
          ;;        becomes active.
          (setq exwm-input--temp-line-mode t
                exwm-input--during-key-sequence t)
          (exwm-input--grab-keyboard))  ;grab keyboard temporarily
        (setq unread-command-events
              (append unread-command-events (list event))))))
  (xcb:+request exwm--connection
      (make-instance 'xcb:AllowEvents
                     :mode xcb:Allow:AsyncKeyboard
                     :time xcb:Time:CurrentTime))
  (xcb:flush exwm--connection))

(defun exwm-input--update-mode-line (id)
  "Update the propertized `mode-line-process' for window ID."
  (let (help-echo cmd mode)
    (cl-case exwm--on-KeyPress
      ((exwm-input--on-KeyPress-line-mode)
       (setq mode "line"
             help-echo "mouse-1: Switch to char-mode"
             cmd `(lambda ()
                    (interactive)
                    (exwm-input-release-keyboard ,id))))
      ((exwm-input--on-KeyPress-char-mode)
       (setq mode "char"
             help-echo "mouse-1: Switch to line-mode"
             cmd `(lambda ()
                    (interactive)
                    (exwm-input-grab-keyboard ,id)))))
    (with-current-buffer (exwm--id->buffer id)
      (setq mode-line-process
            `(": "
              (:propertize ,mode
                           help-echo ,help-echo
                           mouse-face mode-line-highlight
                           local-map
                           (keymap
                            (mode-line
                             keymap
                             (down-mouse-1 . ,cmd)))))))))

(defun exwm-input--grab-keyboard (&optional id)
  "Grab all key events on window ID."
  (unless id (setq id (exwm--buffer->id (window-buffer))))
  (when id
    (when (xcb:+request-checked+request-check exwm--connection
              (make-instance 'xcb:GrabKey
                             :owner-events 0
                             :grab-window id
                             :modifiers xcb:ModMask:Any
                             :key xcb:Grab:Any
                             :pointer-mode xcb:GrabMode:Async
                             :keyboard-mode xcb:GrabMode:Sync))
      (exwm--log "Failed to grab keyboard for #x%x" id))
    (with-current-buffer (exwm--id->buffer id)
      (setq exwm--on-KeyPress #'exwm-input--on-KeyPress-line-mode))))

(defun exwm-input--release-keyboard (&optional id)
  "Ungrab all key events on window ID."
  (unless id (setq id (exwm--buffer->id (window-buffer))))
  (when id
    (when (xcb:+request-checked+request-check exwm--connection
              (make-instance 'xcb:UngrabKey
                             :key xcb:Grab:Any
                             :grab-window id
                             :modifiers xcb:ModMask:Any))
      (exwm--log "Failed to release keyboard for #x%x" id))
    (with-current-buffer (exwm--id->buffer id)
      (setq exwm--on-KeyPress #'exwm-input--on-KeyPress-char-mode))))

;;;###autoload
(defun exwm-input-grab-keyboard (&optional id)
  "Switch to line-mode."
  (interactive (list (exwm--buffer->id (window-buffer))))
  (when id
    (with-current-buffer (exwm--id->buffer id)
      (exwm-input--grab-keyboard id)
      (setq exwm--keyboard-grabbed t)
      (exwm-input--update-mode-line id)
      (force-mode-line-update))))

;;;###autoload
(defun exwm-input-release-keyboard (&optional id)
  "Switch to char-mode."
  (interactive (list (exwm--buffer->id (window-buffer))))
  (when id
    (with-current-buffer (exwm--id->buffer id)
      (exwm-input--release-keyboard id)
      (setq exwm--keyboard-grabbed nil)
      (exwm-input--update-mode-line id)
      (force-mode-line-update))))

(defun exwm-input--fake-key (event)
  "Fake a key event equivalent to Emacs event EVENT."
  (let* ((keysym (xcb:keysyms:event->keysym exwm--connection event))
         keycode id)
    (unless keysym
      (user-error "[EXWM] Invalid key: %s" (single-key-description event)))
    (setq keycode (xcb:keysyms:keysym->keycode exwm--connection
                                               (car keysym)))
    (when keycode
      (setq id (exwm--buffer->id (window-buffer (selected-window))))
      (dolist (class '(xcb:KeyPress xcb:KeyRelease))
        (xcb:+request exwm--connection
            (make-instance 'xcb:SendEvent
                           :propagate 0 :destination id
                           :event-mask xcb:EventMask:NoEvent
                           :event (xcb:marshal
                                   (make-instance class
                                                  :detail keycode
                                                  :time xcb:Time:CurrentTime
                                                  :root exwm--root :event id
                                                  :child 0
                                                  :root-x 0 :root-y 0
                                                  :event-x 0 :event-y 0
                                                  :state (cadr keysym)
                                                  :same-screen 1)
                                   exwm--connection)))))
    (xcb:flush exwm--connection)))

;;;###autoload
(defun exwm-input-send-next-key (times)
  "Send next key to client window."
  (interactive "p")
  (when (> times 12) (setq times 12))
  (let (key keys)
    (dotimes (i times)
      ;; Skip events not from keyboard
      (setq exwm-input--during-key-sequence t)
      (catch 'break
        (while t
          (setq key (read-key (format "Send key: %s (%d/%d)"
                                      (key-description keys)
                                      (1+ i) times)))
          (when (and (listp key) (eq (car key) t))
            (setq key (cdr key)))
          (unless (listp key) (throw 'break nil))))
      (setq exwm-input--during-key-sequence nil)
      (setq keys (vconcat keys (vector key)))
      (exwm-input--fake-key key))))

;; (defun exwm-input-send-last-key ()
;;   (interactive)
;;   (unless (listp last-input-event)      ;not a key event
;;     (exwm-input--fake-key last-input-event)))

(defvar exwm-input--local-simulation-keys nil
  "Whether simulation keys are local.")

(defun exwm-input--update-simulation-prefix-keys ()
  "Update the list of prefix keys of simulation keys."
  (setq exwm-input--simulation-prefix-keys nil)
  (dolist (i exwm-input--simulation-keys)
    (if exwm-input--local-simulation-keys
        (local-set-key (car i) #'exwm-input-send-simulation-key)
      (define-key exwm-mode-map (car i) #'exwm-input-send-simulation-key))
    (cl-pushnew (elt (car i) 0) exwm-input--simulation-prefix-keys)))

(defun exwm-input-set-simulation-keys (simulation-keys)
  "Set simulation keys.

SIMULATION-KEYS is an alist of the form (original-key . simulated-key)."
  (setq exwm-input--simulation-keys nil)
  (dolist (i simulation-keys)
    (cl-pushnew `(,(vconcat (car i)) . ,(cdr i)) exwm-input--simulation-keys))
  (exwm-input--update-simulation-prefix-keys))

(defun exwm-input-set-local-simulation-keys (simulation-keys)
  "Set buffer-local simulation keys.

Its usage is the same with `exwm-input-set-simulation-keys'."
  (make-local-variable 'exwm-input--simulation-keys)
  (make-local-variable 'exwm-input--simulation-prefix-keys)
  (use-local-map (copy-keymap exwm-mode-map))
  (let ((exwm-input--local-simulation-keys t))
    (exwm-input-set-simulation-keys simulation-keys)))

;;;###autoload
(defun exwm-input-send-simulation-key (times)
  "Fake a key event according to last input key sequence."
  (interactive "p")
  (let ((pair (assoc (this-single-command-keys) exwm-input--simulation-keys)))
    (when pair
      (setq pair (cdr pair))
      (unless (listp pair)
        (setq pair (list pair)))
      (dotimes (_ times)
        (dolist (j pair)
          (exwm-input--fake-key j))))))

(defun exwm-input--on-pre-command ()
  "Run in `pre-command-hook'."
  (setq exwm-input--during-command t))

(defun exwm-input--on-post-command ()
  "Run in `post-command-hook'."
  (setq exwm-input--during-command nil))

(declare-function exwm-floating--stop-moveresize "exwm-floating.el"
                  (&rest _args))
(declare-function exwm-floating--do-moveresize "exwm-floating.el"
                  (data _synthetic))

(defun exwm-input--init ()
  "Initialize the keyboard module."
  ;; Refresh keyboard mapping
  (xcb:keysyms:init exwm--connection)
  ;; Convert move/resize buttons
  (let ((move-key (xcb:keysyms:event->keysym exwm--connection
                                             exwm-input-move-event))
        (resize-key (xcb:keysyms:event->keysym exwm--connection
                                               exwm-input-resize-event)))
    (unless move-key
      (user-error "[EXWM] Invalid key: %s"
                  (single-key-description exwm-input-move-event)))
    (unless resize-key
      (user-error "[EXWM] Invalid key: %s"
                  (single-key-description exwm-input-resize-event)))
    (setq exwm-input--move-keysym (car move-key)
          exwm-input--move-mask (cadr move-key)
          exwm-input--resize-keysym (car resize-key)
          exwm-input--resize-mask (cadr resize-key)))
  ;; Attach event listeners
  (xcb:+event exwm--connection 'xcb:KeyPress #'exwm-input--on-KeyPress)
  (xcb:+event exwm--connection 'xcb:ButtonPress #'exwm-input--on-ButtonPress)
  (xcb:+event exwm--connection 'xcb:ButtonRelease
              #'exwm-floating--stop-moveresize)
  (xcb:+event exwm--connection 'xcb:MotionNotify
              #'exwm-floating--do-moveresize)
  ;; The input focus should be set on the frame when minibuffer is active.
  (add-hook 'minibuffer-setup-hook #'exwm-input--on-minibuffer-setup)
  ;; `pre-command-hook' marks the end of a key sequence (existing or not)
  (add-hook 'pre-command-hook #'exwm-input--finish-key-sequence)
  ;; Control `exwm-input--during-command'
  (add-hook 'pre-command-hook #'exwm-input--on-pre-command)
  (add-hook 'post-command-hook #'exwm-input--on-post-command)
  ;; Update focus when buffer list updates
  (add-hook 'buffer-list-update-hook #'exwm-input--on-buffer-list-update)
  ;; Update prefix keys for global keys
  (exwm-input--update-global-prefix-keys))

(defun exwm-input--exit ()
  "Exit the input module."
  (remove-hook 'pre-command-hook #'exwm-input--finish-key-sequence)
  (remove-hook 'pre-command-hook #'exwm-input--on-pre-command)
  (remove-hook 'post-command-hook #'exwm-input--on-post-command)
  (remove-hook 'buffer-list-update-hook #'exwm-input--on-buffer-list-update))



(provide 'exwm-input)

;;; exwm-input.el ends here
