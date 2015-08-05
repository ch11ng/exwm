;;; exwm-input.el --- Input Module for EXWM  -*- lexical-binding: t -*-

;; Copyright (C) 2015 Chris Feng

;; Author: Chris Feng <chris.w.feng@gmail.com>
;; Keywords: unix

;; This file is not part of GNU Emacs.

;; This file is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <http://www.gnu.org/licenses/>.

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
(require 'exwm-floating)

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
  (exwm--with-current-id id
    (setq exwm-input--focus-id id)
    (if (and (not exwm--hints-input)
             (memq xcb:Atom:WM_TAKE_FOCUS exwm--protocols))
        (xcb:+request exwm--connection
            (make-instance 'xcb:icccm:SendEvent
                           :destination id
                           :event (xcb:marshal
                                   (make-instance 'xcb:icccm:WM_TAKE_FOCUS
                                                  :window id
                                                  :time exwm-input--timestamp)
                                   exwm--connection)))
      (xcb:+request exwm--connection
          (make-instance 'xcb:SetInputFocus
                         :revert-to xcb:InputFocus:Parent :focus id
                         :time xcb:Time:CurrentTime)))
    (xcb:flush exwm--connection)))

(defvar exwm-input--focus-id xcb:Window:None
  "The window that is theoretically focused.")
(defvar exwm-input--focus-lock nil
  "Non-nil when input focus should stay unchanged.")

(defun exwm-input--update-focus ()
  "Update input focus."
  (unless exwm-input--focus-lock
    (setq exwm-input--focus-lock t)
    (when (eq (current-buffer) (window-buffer)) ;e.g. with-temp-buffer
      (if (eq major-mode 'exwm-mode)
          (progn (setq exwm-input--focus-id exwm--id)
                 (when exwm--floating-frame
                   (if (eq (selected-frame) exwm--floating-frame)
                       ;; Cancel the possible input focus redirection
                       (redirect-frame-focus exwm--floating-frame nil)
                     ;; Focus the floating frame
                     (x-focus-frame exwm--floating-frame)))
                 ;; Finally focus the window
                 (exwm-input--set-focus exwm-input--focus-id))
        (exwm--with-current-id exwm-input--focus-id
          (setq exwm-input--focus-id xcb:Window:None)
          (let ((frame (selected-frame)))
            (if exwm--floating-frame
                (unless (or (eq frame exwm--floating-frame)
                            (active-minibuffer-window))
                  ;; Redirect input focus to the workspace frame
                  (redirect-frame-focus exwm--floating-frame frame))
              ;; Focus the workspace frame
              (x-focus-frame frame))))))
    (setq exwm-input--focus-lock nil)))

(defun exwm-input--finish-key-sequence ()
  "Mark the end of a key sequence (with the aid of `pre-command-hook')."
  (when (and exwm-input--during-key-sequence
             (not (equal [?\C-u] (this-single-command-keys))))
    (setq exwm-input--during-key-sequence nil)
    (when exwm-input--temp-line-mode
      (setq exwm-input--temp-line-mode nil)
      (exwm-input--release-keyboard))))

(defun exwm-input--on-MappingNotify (data synthetic)
  "Handle MappingNotify event."
  (let ((obj (make-instance 'xcb:MappingNotify)))
    (xcb:unmarshal obj data)
    (with-slots (request first-keycode count) obj
      (cond ((= request xcb:Mapping:Modifier)
             ;; Modifier keys changed
             (xcb:keysyms:update-modifier-mapping exwm--connection))
            ((= request xcb:Mapping:Keyboard)
             ;; Only updated changed keys
             (xcb:keysyms:update-keyboard-mapping exwm--connection
                                                  first-keycode count))))))

(defun exwm-input--on-ButtonPress (data synthetic)
  "Handle ButtonPress event."
  (let ((obj (make-instance 'xcb:ButtonPress))
        (mode xcb:Allow:SyncPointer))
    (xcb:unmarshal obj data)
    (with-slots (detail time event state) obj
      (setq exwm-input--timestamp time)
      (cond ((and (= state exwm-input--move-mask)
                  (= detail exwm-input--move-keysym))
             ;; Move
             (exwm-floating--start-moveresize event
                                             xcb:ewmh:_NET_WM_MOVERESIZE_MOVE))
            ((and (= state exwm-input--resize-mask)
                  (= detail exwm-input--resize-keysym))
             ;; Resize
             (exwm-floating--start-moveresize event))
            (t
             ;; Click to focus
             (unless (and (boundp 'exwm--id) (= event exwm--id))
               (exwm--with-current-id event
                 (raise-frame (or exwm--floating-frame exwm--frame))
                 (select-window (get-buffer-window nil 'visible))))
             ;; The event should be replayed
             (setq mode xcb:Allow:ReplayPointer))))
    (xcb:+request exwm--connection
        (make-instance 'xcb:AllowEvents :mode mode :time xcb:Time:CurrentTime))
    (xcb:flush exwm--connection)))

(defun exwm-input--on-KeyPress (data synthetic)
  "Handle KeyPress event."
  (let ((obj (make-instance 'xcb:KeyPress)))
    (xcb:unmarshal obj data)
    (setq exwm-input--timestamp (slot-value obj 'time))
    (funcall 'exwm-input--handle-KeyPress obj)))

(defvar exwm-input--global-keys nil "Global key bindings.")
(defvar exwm-input--global-prefix-keys nil
  "List of prefix keys of global key bindings.")

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

(defvar exwm-input--during-key-sequence nil
  "Non-nil indicates Emacs is waiting for more keys to form a key sequence.")
(defvar exwm-input--temp-line-mode nil
  "Non-nil indicates it's in temporary line-mode for char-mode.")

;; ;; This implementation has a problem that it also releases queued keys after
;; ;; requesting AllowEvent. The client window will capture unexpected key events
;; ;; in this case.
;; ;; P.S.; to use this implementation, comment out the KeyRelease listener
;; ;;       together with this one and make GrabKey in Sync mode.
;; (cl-defmethod exwm-input--handle-KeyPress-line-mode ((obj xcb:KeyPress))
;;   "Parse X KeyPress event to Emacs key event and then feed the command loop."
;;   (with-slots (detail state) obj
;;     (let ((keysym (xcb:keysyms:keycode->keysym exwm--connection detail state))
;;           event window mode)
;;       (when (and keysym
;;                  (setq event (xcb:keysyms:keysym->event keysym state))
;;                  (or exwm-input--during-key-sequence
;;                      (= exwm-input--focus-id xcb:Window:None)
;;                      (setq window (active-minibuffer-window))
;;                      (eq event ?\C-c)   ;mode-specific key
;;                      (memq event exwm-input--global-prefix-keys)
;;                      (memq event exwm-input-prefix-keys)
;;                      (memq event
;;                            exwm-input--simulation-prefix-keys)))
;;         (setq mode xcb:Allow:SyncKeyboard)
;;         (unless window (setq exwm-input--during-key-sequence t))
;;         (push event unread-command-events))
;;       (xcb:+request exwm--connection
;;           (make-instance 'xcb:AllowEvents
;;                          :mode (or mode xcb:Allow:ReplayKeyboard)
;;                          :time xcb:Time:CurrentTime))
;;       (xcb:flush exwm--connection))))

;; This implementation has a drawback that some (legacy) applications
;; (e.g. xterm) ignore the synthetic key events, making it only viable for EXWM
;; to work in char-mode in such case.
(cl-defmethod exwm-input--handle-KeyPress-line-mode ((obj xcb:KeyPress))
  "Parse X KeyPress event to Emacs key event and then feed the command loop."
  (with-slots (detail state) obj
    (let ((keysym (xcb:keysyms:keycode->keysym exwm--connection detail state))
          event minibuffer-window)
      (if (and keysym
               (setq event (xcb:keysyms:keysym->event keysym state))
               (or exwm-input--during-key-sequence
                   (= exwm-input--focus-id xcb:Window:None)
                   (setq minibuffer-window (active-minibuffer-window))
                   (eq event ?\C-c)   ;mode-specific key
                   (memq event exwm-input--global-prefix-keys)
                   (memq event exwm-input-prefix-keys)
                   (memq event exwm-input--simulation-prefix-keys)))
          ;; Forward key to Emacs frame
          (progn (unless minibuffer-window
                   (setq exwm-input--during-key-sequence t))
                 (push event unread-command-events))
        ;; Forward key to window
        (xcb:+request exwm--connection
            (make-instance 'xcb:SendEvent
                           :propagate 0 :destination (slot-value obj 'event)
                           :event-mask xcb:EventMask:NoEvent
                           :event (xcb:marshal obj exwm--connection)))
        (xcb:flush exwm--connection)))))

(cl-defmethod exwm-input--handle-KeyPress-char-mode ((obj xcb:KeyPress))
  "Handle KeyPress event in char-mode."
  (with-slots (detail state) obj
    (let ((keysym (xcb:keysyms:keycode->keysym exwm--connection detail state))
          event)
      (when (and keysym (setq event (xcb:keysyms:keysym->event keysym state)))
        (setq exwm-input--temp-line-mode t
              exwm-input--during-key-sequence t)
        (push event unread-command-events)
        (exwm-input--grab-keyboard))))) ;grab keyboard temporarily

(defalias 'exwm-input--handle-KeyPress 'exwm-input--handle-KeyPress-line-mode
  "Generic function for handling KeyPress event.")

(defun exwm-input--grab-keyboard (&optional id)
  "Grab all key events on window ID."
  (unless id (setq id (exwm--buffer->id (window-buffer))))
  (when (xcb:+request-checked+request-check exwm--connection
            (make-instance 'xcb:GrabKey
                           :owner-events 0 :grab-window id
                           :modifiers xcb:ModMask:Any
                           :key xcb:Grab:Any
                           :pointer-mode xcb:GrabMode:Async
                           :keyboard-mode xcb:GrabMode:Async))
    (exwm--log "Failed to grab keyboard for #x%x" id))
  (defalias 'exwm-input--handle-KeyPress
    'exwm-input--handle-KeyPress-line-mode))

(defun exwm-input--release-keyboard (&optional id)
  "Ungrab all key events on window ID."
  (unless id (setq id (exwm--buffer->id (window-buffer))))
  (when (xcb:+request-checked+request-check exwm--connection
            (make-instance 'xcb:UngrabKey
                           :key xcb:Grab:Any :grab-window id
                           :modifiers xcb:ModMask:Any))
    (exwm--log "Failed to release keyboard for #x%x" id))
  (defalias 'exwm-input--handle-KeyPress
    'exwm-input--handle-KeyPress-char-mode))

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
      (xcb:+request exwm--connection
          (make-instance 'xcb:SendEvent
                         :propagate 0 :destination id
                         :event-mask xcb:EventMask:NoEvent
                         :event (xcb:marshal
                                 (make-instance 'xcb:KeyPress
                                                :detail keycode
                                                :time xcb:Time:CurrentTime
                                                :root exwm--root :event id
                                                :child 0
                                                :root-x 0 :root-y 0
                                                :event-x 0 :event-y 0
                                                :state (cadr keysym)
                                                :same-screen 1)
                                 exwm--connection))))
    (xcb:flush exwm--connection)))

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

(defvar exwm-input-prefix-keys
  '(?\C-x ?\C-u ?\C-h ?\M-x ?\M-` ?\M-! ?\M-& ?\M-:)
  "List of prefix keys EXWM should forward to Emacs when in line-mode.")

(defvar exwm-input--simulation-keys nil "Simulation keys in line-mode.")
(defvar exwm-input--simulation-prefix-keys nil
  "List of prefix keys of simulation keys in line-mode.")

(defun exwm-input--update-simulation-prefix-keys ()
  "Update the list of prefix keys of simulation keys."
  (setq exwm-input--simulation-prefix-keys nil)
  (dolist (i exwm-input--simulation-keys)
    (define-key exwm-mode-map (car i) 'exwm-input-send-simulation-key)
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
  (let ((pair (assoc (this-single-command-keys)
                     exwm-input--simulation-keys))
        key)
    (when pair
      (setq pair (cdr pair))
      (unless (listp pair)
        (setq pair (list pair)))
      (dotimes (i times)
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
              'exwm-input--on-MappingNotify)
  (xcb:+event exwm--connection 'xcb:KeyPress 'exwm-input--on-KeyPress)
  (xcb:+event exwm--connection 'xcb:ButtonPress 'exwm-input--on-ButtonPress)
  (xcb:+event exwm--connection 'xcb:ButtonRelease
              'exwm-floating--stop-moveresize)
  (xcb:+event exwm--connection 'xcb:MotionNotify 'exwm-floating--do-moveresize)
  ;; `pre-command-hook' marks the end of a key sequence (existing or not)
  (add-hook 'pre-command-hook 'exwm-input--finish-key-sequence)
  ;; Update focus when buffer list updates
  (add-hook 'buffer-list-update-hook 'exwm-input--update-focus)
  ;; Update prefix keys for global keys
  (exwm-input--update-global-prefix-keys))



(provide 'exwm-input)

;;; exwm-input.el ends here
