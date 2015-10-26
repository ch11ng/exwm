;;; exwm-input.el --- Input Module for EXWM  -*- lexical-binding: t -*-

;; Copyright (C) 2015 Free Software Foundation, Inc.

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
(eval-when-compile (require 'exwm-workspace))

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
                           :revert-to xcb:InputFocus:Parent :focus id
                           :time xcb:Time:CurrentTime)))
      (xcb:flush exwm--connection))))

(defvar exwm-input--focus-window nil "The (Emacs) window to be focused.")
(defvar exwm-input--redirected nil
  "Indicate next update on buffer list is actually a result of redirection.")
(defvar exwm-input--timer nil "Currently running timer.")

(defun exwm-input--on-buffer-list-update ()
  "Run in buffer-list-update-hook to track input focus."
  (let ((frame (selected-frame))
        (window (selected-window))
        (buffer (current-buffer)))
    (when (and (not (minibufferp buffer))
               (frame-parameter frame 'exwm-window-id) ;e.g. emacsclient frame
               (eq buffer (window-buffer))) ;e.g. `with-temp-buffer'
      (unless (and exwm-input--redirected
                   exwm-input--focus-window
                   (with-current-buffer (window-buffer
                                         exwm-input--focus-window)
                     exwm--floating-frame))
        (setq exwm-input--focus-window window)
        (when exwm-input--timer (cancel-timer exwm-input--timer))
        (setq exwm-input--timer
              (run-with-timer 0.01 nil #'exwm-input--update-focus)))
      (setq exwm-input--redirected nil))))

(defun exwm-input--on-focus-in ()
  "Run in focus-in-hook to remove redirected focus on frame."
  (let ((frame (selected-frame)))
    (when (and (frame-parameter frame 'exwm-window-id)
               (not (memq frame exwm-workspace--list)))
      (setq exwm-input--redirected t))))

(defun exwm-input--update-focus ()
  "Update input focus."
  (when (window-live-p exwm-input--focus-window)
    (with-current-buffer (window-buffer exwm-input--focus-window)
      (if (eq major-mode 'exwm-mode)
          (if (not (eq exwm--frame exwm-workspace--current))
              ;; Do not focus X windows on other workspace
              (progn
                (set-frame-parameter exwm--frame 'exwm--urgency t)
                (setq exwm-workspace--switch-history-outdated t)
                (force-mode-line-update)
                ;; The application may have changed its input focus
                (exwm-workspace-switch exwm-workspace-current-index t))
            (when exwm--floating-frame
              (redirect-frame-focus exwm--floating-frame nil)
              (select-frame-set-input-focus exwm--floating-frame t))
            (exwm--log "Set focus on #x%x" exwm--id)
            (exwm-input--set-focus exwm--id))
        (when (eq (selected-window) exwm-input--focus-window)
          (exwm--log "Focus on %s" exwm-input--focus-window)
          (select-frame-set-input-focus (window-frame exwm-input--focus-window)
                                        t)
          (dolist (pair exwm--id-buffer-alist)
            (with-current-buffer (cdr pair)
              (when (and exwm--floating-frame
                         (eq exwm--frame exwm-workspace--current))
                (redirect-frame-focus exwm--floating-frame exwm--frame))))))
      (setq exwm-input--focus-window nil))))

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

(defun exwm-input--on-MappingNotify (data _synthetic)
  "Handle MappingNotify event."
  (let ((obj (make-instance 'xcb:MappingNotify)))
    (xcb:unmarshal obj data)
    (with-slots (request first-keycode count) obj
      (cond
       ((= request xcb:Mapping:Modifier)
        ;; Modifier keys changed
        (exwm--log "Update modifier mapping")
        (xcb:keysyms:update-modifier-mapping exwm--connection))
       ((= request xcb:Mapping:Keyboard)
        ;; Only update changed keys
        (exwm--log "Update keyboard mapping: %d ~ %d"
                   first-keycode (+ first-keycode count))
        (xcb:keysyms:update-keyboard-mapping exwm--connection
                                             first-keycode count))))))

(defun exwm-input--on-ButtonPress (data _synthetic)
  "Handle ButtonPress event."
  (let ((obj (make-instance 'xcb:ButtonPress))
        (mode xcb:Allow:SyncPointer))
    (xcb:unmarshal obj data)
    (with-slots (detail time event state) obj
      (setq exwm-input--timestamp time)
      (cond ((and (= state exwm-input--move-mask)
                  (= detail exwm-input--move-keysym))
             ;; Move
             (exwm-floating--start-moveresize
              event xcb:ewmh:_NET_WM_MOVERESIZE_MOVE))
            ((and (= state exwm-input--resize-mask)
                  (= detail exwm-input--resize-keysym))
             ;; Resize
             (exwm-floating--start-moveresize event))
            (t
             ;; Click to focus
             (let ((window (get-buffer-window (exwm--id->buffer event) t))
                   frame)
               (unless (eq window (selected-window))
                 (setq frame (window-frame window))
                 (unless (eq frame exwm-workspace--current)
                   (if (memq frame exwm-workspace--list)
                       ;; The X window is on another workspace
                       (exwm-workspace-switch
                        (cl-position frame exwm-workspace--list))
                     (with-current-buffer (window-buffer window)
                       (when (and (eq major-mode 'exwm-mode)
                                  (not (eq exwm--frame
                                           exwm-workspace--current)))
                         ;; The floating X window is on another workspace
                         (exwm-workspace-switch
                          (cl-position exwm--frame exwm-workspace--list))))))
                 ;; It has been reported that the `window' may have be deleted
                 (if (window-live-p window)
                     (select-window window)
                   (setq window
                         (get-buffer-window (exwm--id->buffer event) t))
                   (when window (select-window window)))))
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
          keysym)
      (setq exwm-input--global-prefix-keys nil)
      (dolist (i exwm-input--global-keys)
        (cl-pushnew (elt i 0) exwm-input--global-prefix-keys))
      (unless (equal original exwm-input--global-prefix-keys)
        ;; Grab global keys on root window
        (if (xcb:+request-checked+request-check exwm--connection
                (make-instance 'xcb:UngrabKey
                               :key xcb:Grab:Any :grab-window exwm--root
                               :modifiers xcb:ModMask:Any))
            (exwm--log "Failed to ungrab keys")
          (dolist (i exwm-input--global-prefix-keys)
            (setq keysym (xcb:keysyms:event->keysym i))
            (when (or (not keysym)
                      (xcb:+request-checked+request-check exwm--connection
                          (make-instance 'xcb:GrabKey
                                         :owner-events 0
                                         :grab-window exwm--root
                                         :modifiers (cadr keysym)
                                         :key (xcb:keysyms:keysym->keycode
                                               exwm--connection (car keysym))
                                         :pointer-mode xcb:GrabMode:Async
                                         :keyboard-mode xcb:GrabMode:Async)))
              (user-error "[EXWM] Failed to grab key: %s" i))))))))

(defun exwm-input-set-key (key command)
  "Set a global key binding."
  (global-set-key key command)
  (cl-pushnew key exwm-input--global-keys))

;; These commands usually call something like `read-char' without using the
;; minibuffer, so they will not get inputs after invoked.  It'd be better if we
;; can determine whether there's a command waiting for input so that this
;; variable can be removed.
(defvar exwm-input-command-whitelist nil
  "A list of commands that when active all keys should be forwarded to Emacs.")

;;;###autoload
(defun exwm-input--on-KeyPress-line-mode (key-press)
  "Parse X KeyPress event to Emacs key event and then feed the command loop."
  (with-slots (detail state) key-press
    (let ((keysym (xcb:keysyms:keycode->keysym exwm--connection detail state))
          event minibuffer-window mode)
      (when (and keysym
                 (setq event (xcb:keysyms:keysym->event keysym state))
                 (or exwm-input--during-key-sequence
                     (setq minibuffer-window (active-minibuffer-window))
                     (memq real-this-command exwm-input-command-whitelist)
                     (memq event exwm-input--global-prefix-keys)
                     (memq event exwm-input-prefix-keys)
                     (memq event exwm-input--simulation-prefix-keys)))
        (setq mode xcb:Allow:AsyncKeyboard)
        (unless minibuffer-window (setq exwm-input--during-key-sequence t))
        (push event unread-command-events))
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
      ;; Compensate FocusOut event (prevent cursor style change)
      (unless (eq major-mode 'exwm-mode)
        (let ((id (frame-parameter nil 'exwm-window-id)))
          (xcb:+request exwm--connection
              (make-instance 'xcb:SendEvent
                             :propagate 0
                             :destination id
                             :event-mask xcb:EventMask:StructureNotify
                             :event
                             (xcb:marshal
                              (make-instance 'xcb:FocusIn
                                             :detail xcb:NotifyDetail:Inferior
                                             :event id
                                             :mode xcb:NotifyMode:Normal)
                              exwm--connection)))))
      (when (and keysym (setq event (xcb:keysyms:keysym->event keysym state)))
        (when (eq major-mode 'exwm-mode)
          (setq exwm-input--temp-line-mode t
                exwm-input--during-key-sequence t)
          (exwm-input--grab-keyboard))  ;grab keyboard temporarily
        (push event unread-command-events))))
  (xcb:+request exwm--connection
      (make-instance 'xcb:AllowEvents
                     :mode xcb:Allow:AsyncKeyboard
                     :time xcb:Time:CurrentTime))
  (xcb:flush exwm--connection))

(defun exwm-input--grab-keyboard (&optional id)
  "Grab all key events on window ID."
  (unless id (setq id (exwm--buffer->id (window-buffer))))
  (when id
    (when (xcb:+request-checked+request-check exwm--connection
              (make-instance 'xcb:GrabKey
                             :owner-events 0 :grab-window id
                             :modifiers xcb:ModMask:Any
                             :key xcb:Grab:Any
                             :pointer-mode xcb:GrabMode:Async
                             :keyboard-mode xcb:GrabMode:Sync))
      (exwm--log "Failed to grab keyboard for #x%x" id))
    (setq exwm--on-KeyPress #'exwm-input--on-KeyPress-line-mode)))

(defun exwm-input--release-keyboard (&optional id)
  "Ungrab all key events on window ID."
  (unless id (setq id (exwm--buffer->id (window-buffer))))
  (when id
    (when (xcb:+request-checked+request-check exwm--connection
              (make-instance 'xcb:UngrabKey
                             :key xcb:Grab:Any :grab-window id
                             :modifiers xcb:ModMask:Any))
      (exwm--log "Failed to release keyboard for #x%x" id))
    (setq exwm--on-KeyPress #'exwm-input--on-KeyPress-char-mode)))

;;;###autoload
(defun exwm-input-grab-keyboard (&optional id)
  "Switch to line-mode."
  (interactive)
  (exwm-input--grab-keyboard id)
  (setq mode-line-process
        '(": "
          (:propertize "line"
                       help-echo "mouse-1: Switch to char-mode"
                       mouse-face mode-line-highlight
                       local-map
                       (keymap
                        (mode-line
                         keymap
                         (down-mouse-1 . exwm-input-release-keyboard))))))
  (force-mode-line-update))

;;;###autoload
(defun exwm-input-release-keyboard (&optional id)
  "Switch to char-mode."
  (interactive)
  (exwm-input--release-keyboard id)
  (setq mode-line-process
        '(": "
          (:propertize "char"
                       help-echo "mouse-1: Switch to line-mode"
                       mouse-face mode-line-highlight
                       local-map
                       (keymap
                        (mode-line
                         keymap
                         (down-mouse-1 . exwm-input-grab-keyboard))))))
  (force-mode-line-update))

(defun exwm-input--fake-key (event)
  "Fake a key event equivalent to Emacs event EVENT."
  (let* ((keysym (xcb:keysyms:event->keysym event))
         (keycode (xcb:keysyms:keysym->keycode exwm--connection (car keysym)))
         (id (exwm--buffer->id (window-buffer (selected-window)))))
    (when keycode
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
          (unless (listp key) (throw 'break nil))))
      (setq exwm-input--during-key-sequence nil)
      (setq keys (vconcat keys (vector key)))
      (exwm-input--fake-key key))))

;; (defun exwm-input-send-last-key ()
;;   (interactive)
;;   (unless (listp last-input-event)      ;not a key event
;;     (exwm-input--fake-key last-input-event)))

(defun exwm-input--update-simulation-prefix-keys ()
  "Update the list of prefix keys of simulation keys."
  (setq exwm-input--simulation-prefix-keys nil)
  (dolist (i exwm-input--simulation-keys)
    (define-key exwm-mode-map (car i) #'exwm-input-send-simulation-key)
    (cl-pushnew (elt (car i) 0) exwm-input--simulation-prefix-keys)))

(defun exwm-input-set-simulation-keys (simulation-keys)
  "Set simulation keys.

SIMULATION-KEYS is a list of alist (key-sequence1 . key-sequence2)."
  (setq exwm-input--simulation-keys nil)
  (dolist (i simulation-keys)
    (cl-pushnew `(,(vconcat (car i)) . ,(cdr i)) exwm-input--simulation-keys))
  (exwm-input--update-simulation-prefix-keys))

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

(defun exwm-input--init ()
  "Initialize the keyboard module."
  ;; Refresh keyboard mapping
  (xcb:keysyms:init exwm--connection)
  ;; Convert move/resize buttons
  (let ((move-key (xcb:keysyms:event->keysym exwm-input-move-event))
        (resize-key (xcb:keysyms:event->keysym exwm-input-resize-event)))
    (setq exwm-input--move-keysym (car move-key)
          exwm-input--move-mask (cadr move-key)
          exwm-input--resize-keysym (car resize-key)
          exwm-input--resize-mask (cadr resize-key)))
  ;; Attach event listeners
  (xcb:+event exwm--connection 'xcb:MappingNotify
              #'exwm-input--on-MappingNotify)
  (xcb:+event exwm--connection 'xcb:KeyPress #'exwm-input--on-KeyPress)
  (xcb:+event exwm--connection 'xcb:ButtonPress #'exwm-input--on-ButtonPress)
  (xcb:+event exwm--connection 'xcb:ButtonRelease
              #'exwm-floating--stop-moveresize)
  (xcb:+event exwm--connection 'xcb:MotionNotify
              #'exwm-floating--do-moveresize)
  ;; `pre-command-hook' marks the end of a key sequence (existing or not)
  (add-hook 'pre-command-hook #'exwm-input--finish-key-sequence)
  ;; Update focus when buffer list updates
  (add-hook 'buffer-list-update-hook #'exwm-input--on-buffer-list-update)
  (add-hook 'focus-in-hook #'exwm-input--on-focus-in)
  ;; Update prefix keys for global keys
  (exwm-input--update-global-prefix-keys))



(provide 'exwm-input)

;;; exwm-input.el ends here
