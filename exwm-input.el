;;; exwm-input.el --- Input Module for EXWM  -*- lexical-binding: t -*-

;; Copyright (C) 2015-2017 Free Software Foundation, Inc.

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
;;   cancel, clear, etc).  Some of them are not present on common keyboard
;;   (keycode = 0).  May need to use XKB extension.

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

(defvar exwm-input--timestamp-window nil)
(defvar exwm-input--timestamp-atom nil)
(defvar exwm-input--timestamp-callback nil)

(defvar exwm-workspace--current)
(defvar exwm-workspace--switch-history-outdated)
(defvar exwm-workspace-current-index)
(defvar exwm-workspace--minibuffer)
(defvar exwm-workspace--list)

(defun exwm-input--set-focus (id)
  "Set input focus to window ID in a proper way."
  (when (exwm--id->buffer id)
    (with-current-buffer (exwm--id->buffer id)
      (cond
       ((and (not exwm--hints-input)
             (memq xcb:Atom:WM_TAKE_FOCUS exwm--protocols))
        (when (= (frame-parameter nil 'exwm-id)
                 (slot-value (xcb:+request-unchecked+reply exwm--connection
                                 (make-instance 'xcb:GetInputFocus))
                             'focus))
          (exwm--log "Focus on #x%x with WM_TAKE_FOCUS" id)
          (exwm-input--update-timestamp
           (lambda (timestamp id)
             (let ((event (make-instance 'xcb:icccm:WM_TAKE_FOCUS
                                         :window id
                                         :time timestamp)))
               (setq event (xcb:marshal event exwm--connection))
               (xcb:+request exwm--connection
                   (make-instance 'xcb:icccm:SendEvent
                                  :destination id
                                  :event event))
               (exwm-input--set-active-window id)
               (xcb:flush exwm--connection)))
           id)))
       (t
        (exwm--log "Focus on #x%x with SetInputFocus" id)
        (xcb:+request exwm--connection
            (make-instance 'xcb:SetInputFocus
                           :revert-to xcb:InputFocus:Parent
                           :focus id
                           :time xcb:Time:CurrentTime))
        (exwm-input--set-active-window id)
        (xcb:flush exwm--connection))))))

(defun exwm-input--update-timestamp (callback &rest args)
  "Fetch the latest timestamp from the server and feed it to CALLBACK.

ARGS are additional arguments to CALLBACK."
  (setq exwm-input--timestamp-callback (cons callback args))
  (xcb:+request exwm--connection
      (make-instance 'xcb:ChangeProperty
                     :mode xcb:PropMode:Replace
                     :window exwm-input--timestamp-window
                     :property exwm-input--timestamp-atom
                     :type xcb:Atom:CARDINAL
                     :format 32
                     :data-len 0
                     :data nil))
  (xcb:flush exwm--connection))

(defun exwm-input--on-PropertyNotify (data _synthetic)
  "Handle PropertyNotify events."
  (when exwm-input--timestamp-callback
    (let ((obj (make-instance 'xcb:PropertyNotify)))
      (xcb:unmarshal obj data)
      (when (= exwm-input--timestamp-window
               (slot-value obj 'window))
        (apply (car exwm-input--timestamp-callback)
               (slot-value obj 'time)
               (cdr exwm-input--timestamp-callback))
        (setq exwm-input--timestamp-callback nil)))))

(defun exwm-input--on-FocusIn (&rest _args)
  "Handle FocusIn events."
  ;; Not sure if this is the right thing to do but the point is the
  ;; input focus should not stay at the root window or any container,
  ;; or the result would be unpredictable.  `x-focus-frame' would
  ;; first set the input focus to the (previously) selected frame, and
  ;; then `select-window' would further update the input focus if the
  ;; selected window is displaying an `exwm-mode' buffer.  Perhaps we
  ;; should carefully filter out FocusIn events with certain 'detail'
  ;; and 'mode' combinations, but this just works.
  (x-focus-frame (selected-frame))
  (select-window (selected-window)))

(defun exwm-input--on-workspace-list-change ()
  "Run in `exwm-input--update-global-prefix-keys'."
  (dolist (f exwm-workspace--list)
    ;; Reuse the 'exwm-grabbed' frame parameter set in
    ;; `exwm-input--update-global-prefix-keys'.
    (unless (frame-parameter f 'exwm-grabbed)
      (xcb:+request exwm--connection
          (make-instance 'xcb:ChangeWindowAttributes
                         :window (frame-parameter f 'exwm-workspace)
                         :value-mask xcb:CW:EventMask
                         ;; There should no other event selected there.
                         :event-mask xcb:EventMask:FocusChange))))
  (exwm-input--update-global-prefix-keys)
  (xcb:flush exwm--connection))

(declare-function exwm-workspace--client-p "exwm-workspace.el"
                  (&optional frame))

(defvar exwm-input--update-focus-window nil "The (Emacs) window to be focused.

This value should always be overwritten.")

(defun exwm-input--on-buffer-list-update ()
  "Run in `buffer-list-update-hook' to track input focus."
  (when (and (eq (current-buffer) (window-buffer)) ;e.g. `with-temp-buffer'.
             (not (eq this-command #'handle-switch-frame))
             (not (exwm-workspace--client-p)))
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

(declare-function exwm-layout--iconic-state-p "exwm-layout.el" (&optional id))
(declare-function exwm-layout--set-state "exwm-layout.el" (id state))
(declare-function exwm-workspace--minibuffer-own-frame-p "exwm-workspace.el")
(declare-function exwm-workspace-switch "exwm-workspace.el"
                  (frame-or-index &optional force))
(declare-function exwm-workspace--workspace-p "exwm-workspace.el" (workspace))

(defun exwm-input--update-focus (window)
  "Update input focus."
  (setq exwm-input--update-focus-lock t)
  (when (window-live-p window)
    (with-current-buffer (window-buffer window)
      (if (eq major-mode 'exwm-mode)
          (if (not (eq exwm--frame exwm-workspace--current))
              (progn
                (set-frame-parameter exwm--frame 'exwm-selected-window window)
                (run-with-idle-timer 0 nil #'exwm-workspace-switch
                                     exwm--frame))
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
          (if (and (exwm-workspace--workspace-p (selected-frame))
                   (not (eq (selected-frame) exwm-workspace--current)))
              ;; The focus is on another workspace (e.g. it got clicked)
              ;; so switch to it.
              (progn
                (set-frame-parameter (selected-frame) 'exwm-selected-window
                                     window)
                (run-with-idle-timer 0 nil #'exwm-workspace-switch
                                     (selected-frame)))
            ;; The focus is still on the current workspace.
            (if (not (and (exwm-workspace--minibuffer-own-frame-p)
                          (minibufferp)))
                (select-frame-set-input-focus (window-frame window) t)
              ;; X input focus should be set on the previously selected
              ;; frame.
              (select-frame-set-input-focus (window-frame
                                             (minibuffer-selected-window))
                                            t)
              (select-frame (window-frame window) t))
            (exwm-input--set-active-window)
            (xcb:flush exwm--connection))))))
  (setq exwm-input--update-focus-lock nil))

(defun exwm-input--on-minibuffer-setup ()
  "Run in `minibuffer-setup-hook' to set input focus."
  (unless (exwm-workspace--client-p)
    ;; Set input focus on the Emacs frame
    (x-focus-frame (window-frame (minibuffer-selected-window)))))

(defun exwm-input--set-active-window (&optional id)
  "Set _NET_ACTIVE_WINDOW."
  (xcb:+request exwm--connection
      (make-instance 'xcb:ewmh:set-_NET_ACTIVE_WINDOW
                     :window exwm--root
                     :data (or id xcb:Window:None))))

(declare-function exwm-floating--start-moveresize "exwm-floating.el"
                  (id &optional type))
(declare-function exwm-workspace--position "exwm-workspace.el" (frame))

(defun exwm-input--on-ButtonPress (data _synthetic)
  "Handle ButtonPress event."
  (let ((obj (make-instance 'xcb:ButtonPress))
        (mode xcb:Allow:SyncPointer)
        window buffer frame)
    (xcb:unmarshal obj data)
    (with-slots (detail time event state) obj
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
    (if (eq major-mode 'exwm-mode)
        (funcall exwm--on-KeyPress obj data)
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
      ;; Stop here if the global prefix keys are update-to-date and
      ;; there's no new workspace.
      (unless (and (equal original exwm-input--global-prefix-keys)
                   (cl-every (lambda (w) (frame-parameter w 'exwm-grabbed))
                             exwm-workspace--list))
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
            ;; Label this frame.
            (set-frame-parameter w 'exwm-grabbed t)
            (dolist (k exwm-input--global-prefix-keys)
              (setq keysym (xcb:keysyms:event->keysym exwm--connection k)
                    keycode (xcb:keysyms:keysym->keycode exwm--connection
                                                         (car keysym)))
              (setf (slot-value grab-key 'grab-window) workspace
                    (slot-value grab-key 'modifiers) (cdr keysym)
                    (slot-value grab-key 'key) keycode)
              (when (or (= 0 keycode)
                        (xcb:+request-checked+request-check exwm--connection
                            grab-key)
                        ;; Also grab this key with num-lock mask set.
                        (when (/= 0 xcb:keysyms:num-lock-mask)
                          (setf (slot-value grab-key 'modifiers)
                                (logior (cdr keysym)
                                        xcb:keysyms:num-lock-mask))
                          (xcb:+request-checked+request-check exwm--connection
                              grab-key)))
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
  (if (< emacs-major-version 27)
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

(defvar exwm-input-line-mode-passthrough nil
  "Non-nil makes 'line-mode' forwards all events to Emacs.")

(defvar exwm-input--line-mode-cache nil "Cache for incomplete key sequence.")

(defvar exwm-input--temp-line-mode nil
  "Non-nil indicates it's in temporary line-mode for char-mode.")

(defun exwm-input--cache-event (event)
  "Cache EVENT."
  (setq exwm-input--line-mode-cache
        (vconcat exwm-input--line-mode-cache (vector event)))
  ;; When the key sequence is complete.
  (unless (keymapp (key-binding exwm-input--line-mode-cache))
    (setq exwm-input--line-mode-cache nil)
    (when exwm-input--temp-line-mode
      (setq exwm-input--temp-line-mode nil)
      (exwm-input--release-keyboard)))
  (exwm-input--unread-event event))

(defun exwm-input--on-KeyPress-line-mode (key-press raw-data)
  "Parse X KeyPress event to Emacs key event and then feed the command loop."
  (with-slots (detail state) key-press
    (let ((keysym (xcb:keysyms:keycode->keysym exwm--connection detail state))
          event mode)
      (when (and (/= 0 (car keysym))
                 (setq event (xcb:keysyms:keysym->event
                              exwm--connection (car keysym)
                              (logand state (lognot (cdr keysym)))))
                 (or exwm-input-line-mode-passthrough
                     exwm-input--during-command
                     ;; Forward the event when there is an incomplete key
                     ;; sequence or when the minibuffer is active.
                     exwm-input--line-mode-cache
                     (eq (active-minibuffer-window) (selected-window))
                     ;;
                     (memq event exwm-input--global-prefix-keys)
                     (memq event exwm-input-prefix-keys)
                     (memq event exwm-input--simulation-prefix-keys)))
        (setq mode xcb:Allow:AsyncKeyboard)
        (exwm-input--cache-event event))
      (unless mode
        (if (= 0 (logand #x6000 state)) ;Check the 13~14 bits.
            ;; Not an XKB state; just replay it.
            (setq mode xcb:Allow:ReplayKeyboard)
          ;; An XKB state; sent it with SendEvent.
          ;; FIXME: Can this also be replayed?
          ;; FIXME: KeyRelease events are lost.
          (setq mode xcb:Allow:AsyncKeyboard)
          (xcb:+request exwm--connection
              (make-instance 'xcb:SendEvent
                             :propagate 0
                             :destination (slot-value key-press 'event)
                             :event-mask xcb:EventMask:NoEvent
                             :event raw-data)))
        ;; Make Emacs aware of this event when defining keyboard macros.
        (when (and defining-kbd-macro event)
          (set-transient-map '(keymap (t . (lambda () (interactive)))))
          (exwm-input--unread-event event)))
      (xcb:+request exwm--connection
          (make-instance 'xcb:AllowEvents
                         :mode mode
                         :time xcb:Time:CurrentTime))
      (xcb:flush exwm--connection))))

(defun exwm-input--on-KeyPress-char-mode (key-press &optional _raw-data)
  "Handle KeyPress event in char-mode."
  (with-slots (detail state) key-press
    (let ((keysym (xcb:keysyms:keycode->keysym exwm--connection detail state))
          event)
      (when (and (/= 0 (car keysym))
                 (setq event (xcb:keysyms:keysym->event
                              exwm--connection (car keysym)
                              (logand state (lognot (cdr keysym))))))
        (if (not (eq major-mode 'exwm-mode))
            (exwm-input--unread-event event)
          ;; Grab keyboard temporarily.
          (setq exwm-input--temp-line-mode t)
          (exwm-input--grab-keyboard)
          (exwm-input--cache-event event)))))
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

;;;###autoload
(defun exwm-input-toggle-keyboard (&optional id)
  "Toggle between 'line-mode' and 'char-mode'."
  (interactive (list (exwm--buffer->id (window-buffer))))
  (when id
    (with-current-buffer (exwm--id->buffer id)
      (if exwm--keyboard-grabbed
          (exwm-input-release-keyboard id)
        (exwm-reset)))))

(defun exwm-input--fake-key (event)
  "Fake a key event equivalent to Emacs event EVENT."
  (let* ((keysym (xcb:keysyms:event->keysym exwm--connection event))
         keycode id)
    (when (= 0 (car keysym))
      (user-error "[EXWM] Invalid key: %s" (single-key-description event)))
    (setq keycode (xcb:keysyms:keysym->keycode exwm--connection
                                               (car keysym)))
    (when (/= 0 keycode)
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
                                                  :state (cdr keysym)
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
      (let ((exwm-input-line-mode-passthrough t))
        (catch 'break
          (while t
            (setq key (read-key (format "Send key: %s (%d/%d)"
                                        (key-description keys)
                                        (1+ i) times)))
            (when (and (listp key) (eq (car key) t))
              (setq key (cdr key)))
            (unless (listp key) (throw 'break nil)))))
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
    (when (= 0 (car move-key))
      (user-error "[EXWM] Invalid key: %s"
                  (single-key-description exwm-input-move-event)))
    (when (= 0 (car resize-key))
      (user-error "[EXWM] Invalid key: %s"
                  (single-key-description exwm-input-resize-event)))
    (setq exwm-input--move-keysym (car move-key)
          exwm-input--move-mask (cdr move-key)
          exwm-input--resize-keysym (car resize-key)
          exwm-input--resize-mask (cdr resize-key)))
  ;; Create the X window and intern the atom used to fetch timestamp.
  (setq exwm-input--timestamp-window (xcb:generate-id exwm--connection))
  (xcb:+request exwm--connection
      (make-instance 'xcb:CreateWindow
                     :depth 0
                     :wid exwm-input--timestamp-window
                     :parent exwm--root
                     :x -1
                     :y -1
                     :width 1
                     :height 1
                     :border-width 0
                     :class xcb:WindowClass:CopyFromParent
                     :visual 0
                     :value-mask xcb:CW:EventMask
                     :event-mask xcb:EventMask:PropertyChange))
  (let ((atom "_TIME"))
    (setq exwm-input--timestamp-atom
          (slot-value (xcb:+request-unchecked+reply exwm--connection
                          (make-instance 'xcb:InternAtom
                                         :only-if-exists 0
                                         :name-len (length atom)
                                         :name atom))
                      'atom)))
  ;; Attach event listeners
  (xcb:+event exwm--connection 'xcb:PropertyNotify
              #'exwm-input--on-PropertyNotify)
  (xcb:+event exwm--connection 'xcb:KeyPress #'exwm-input--on-KeyPress)
  (xcb:+event exwm--connection 'xcb:ButtonPress #'exwm-input--on-ButtonPress)
  (xcb:+event exwm--connection 'xcb:ButtonRelease
              #'exwm-floating--stop-moveresize)
  (xcb:+event exwm--connection 'xcb:MotionNotify
              #'exwm-floating--do-moveresize)
  (xcb:+event exwm--connection 'xcb:FocusIn #'exwm-input--on-FocusIn)
  ;; The input focus should be set on the frame when minibuffer is active.
  (add-hook 'minibuffer-setup-hook #'exwm-input--on-minibuffer-setup)
  ;; Control `exwm-input--during-command'
  (add-hook 'pre-command-hook #'exwm-input--on-pre-command)
  (add-hook 'post-command-hook #'exwm-input--on-post-command)
  ;; Update focus when buffer list updates
  (add-hook 'buffer-list-update-hook #'exwm-input--on-buffer-list-update)
  ;; Re-grab global keys.
  (add-hook 'exwm-workspace-list-change-hook
            #'exwm-input--on-workspace-list-change)
  (exwm-input--on-workspace-list-change)
  ;; Prevent frame parameters introduced by this module from being
  ;; saved/restored.
  (dolist (i '(exwm-grabbed))
    (push (cons i :never) frameset-filter-alist)))

(defun exwm-input--exit ()
  "Exit the input module."
  (remove-hook 'pre-command-hook #'exwm-input--on-pre-command)
  (remove-hook 'post-command-hook #'exwm-input--on-post-command)
  (remove-hook 'buffer-list-update-hook #'exwm-input--on-buffer-list-update)
  (remove-hook 'exwm-workspace-list-change-hook
               #'exwm-input--on-workspace-list-change)
  (when exwm-input--update-focus-defer-timer
    (cancel-timer exwm-input--update-focus-defer-timer))
  (when exwm-input--update-focus-timer
    (cancel-timer exwm-input--update-focus-timer)))



(provide 'exwm-input)

;;; exwm-input.el ends here
