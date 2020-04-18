;;; exwm-input.el --- Input Module for EXWM  -*- lexical-binding: t -*-

;; Copyright (C) 2015-2020 Free Software Foundation, Inc.

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

(defgroup exwm-input nil
  "Input."
  :version "25.3"
  :group 'exwm)

(defcustom exwm-input-prefix-keys
  '(?\C-x ?\C-u ?\C-h ?\M-x ?\M-` ?\M-& ?\M-:)
  "List of prefix keys EXWM should forward to Emacs when in line-mode.

The point is to make keys like 'C-x C-f' forwarded to Emacs in line-mode.
There is no need to add prefix keys for global/simulation keys or those
defined in `exwm-mode-map' here."
  :type '(repeat key-sequence)
  :get (lambda (symbol)
         (mapcar #'vector (default-value symbol)))
  :set (lambda (symbol value)
         (set symbol (mapcar (lambda (i)
                               (if (sequencep i)
                                   (aref i 0)
                                 i))
                             value))))

(defcustom exwm-input-move-event 's-down-mouse-1
  "Emacs event to start moving a window."
  :type 'key-sequence
  :get (lambda (symbol)
         (let ((value (default-value symbol)))
           (if (mouse-event-p value)
               value
             (vector value))))
  :set (lambda (symbol value)
         (set symbol (if (sequencep value)
                         (aref value 0)
                       value))))

(defcustom exwm-input-resize-event 's-down-mouse-3
  "Emacs event to start resizing a window."
  :type 'key-sequence
  :get (lambda (symbol)
         (let ((value (default-value symbol)))
           (if (mouse-event-p value)
               value
             (vector value))))
  :set (lambda (symbol value)
         (set symbol (if (sequencep value)
                         (aref value 0)
                       value))))

(defcustom exwm-input-line-mode-passthrough nil
  "Non-nil makes 'line-mode' forward all events to Emacs."
  :type 'boolean)

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

(defvar exwm-input--during-command nil
  "Indicate whether between `pre-command-hook' and `post-command-hook'.")

(defvar exwm-input--global-keys nil "Global key bindings.")

(defvar exwm-input--global-prefix-keys nil
  "List of prefix keys of global key bindings.")

(defvar exwm-input--line-mode-cache nil "Cache for incomplete key sequence.")

(defvar exwm-input--local-simulation-keys nil
  "Whether simulation keys are local.")

(defvar exwm-input--simulation-keys nil "Simulation keys in line-mode.")

(defvar exwm-input--temp-line-mode nil
  "Non-nil indicates it's in temporary line-mode for char-mode.")

(defvar exwm-input--timestamp-atom nil)

(defvar exwm-input--timestamp-callback nil)

(defvar exwm-input--timestamp-window nil)

(defvar exwm-input--update-focus-defer-timer nil "Timer for polling the lock.")

(defvar exwm-input--update-focus-lock nil
  "Lock for solving input focus update contention.")

(defvar exwm-input--update-focus-timer nil
  "Timer for deferring the update of input focus.")

(defvar exwm-input--update-focus-window nil "The (Emacs) window to be focused.
It also helps us discern whether a `buffer-list-update-hook' was caused by a
different window having been selected.

This value should always be overwritten.")

(defvar exwm-input--update-focus-window-buffer nil
  "Buffer displayed in `exwm-input--update-focus-window'.
Helps us discern whether a `buffer-list-update-hook' was caused by the selected
window switching to a different buffer.")

(defvar exwm-input--echo-area-timer nil "Timer for detecting echo area dirty.")

(defvar exwm-input--event-hook nil
  "Hook to run when EXWM receives an event.")

(defvar exwm-workspace--current)
(declare-function exwm-floating--do-moveresize "exwm-floating.el"
                  (data _synthetic))
(declare-function exwm-floating--start-moveresize "exwm-floating.el"
                  (id &optional type))
(declare-function exwm-floating--stop-moveresize "exwm-floating.el"
                  (&rest _args))
(declare-function exwm-layout--iconic-state-p "exwm-layout.el" (&optional id))
(declare-function exwm-layout--show "exwm-layout.el" (id &optional window))
(declare-function exwm-reset "exwm.el" ())
(declare-function exwm-workspace--client-p "exwm-workspace.el"
                  (&optional frame))
(declare-function exwm-workspace--minibuffer-own-frame-p "exwm-workspace.el")
(declare-function exwm-workspace--workspace-p "exwm-workspace.el" (workspace))
(declare-function exwm-workspace-switch "exwm-workspace.el"
                  (frame-or-index &optional force))

(defun exwm-input--set-focus (id)
  "Set input focus to window ID in a proper way."
  (let ((from (slot-value (xcb:+request-unchecked+reply exwm--connection
                              (make-instance 'xcb:GetInputFocus))
                          'focus))
        tree)
    (if (or (exwm--id->buffer from)
            (eq from id))
        (exwm--log "#x%x => #x%x" (or from 0) (or id 0))
      ;; Attempt to find the top-level X window for a 'focus proxy'.
      (unless (= from xcb:Window:None)
        (setq tree (xcb:+request-unchecked+reply exwm--connection
                       (make-instance 'xcb:QueryTree
                                      :window from)))
        (when tree
          (setq from (slot-value tree 'parent))))
      (exwm--log "#x%x (corrected) => #x%x" (or from 0) (or id 0)))
    (when (and (exwm--id->buffer id)
               ;; Avoid redundant input focus transfer.
               (not (eq from id)))
      (with-current-buffer (exwm--id->buffer id)
        (exwm-input--update-timestamp
         (lambda (timestamp id send-input-focus wm-take-focus)
           (when send-input-focus
             (xcb:+request exwm--connection
                 (make-instance 'xcb:SetInputFocus
                                :revert-to xcb:InputFocus:Parent
                                :focus id
                                :time timestamp)))
           (when wm-take-focus
             (let ((event (make-instance 'xcb:icccm:WM_TAKE_FOCUS
                                         :window id
                                         :time timestamp)))
               (setq event (xcb:marshal event exwm--connection))
               (xcb:+request exwm--connection
                   (make-instance 'xcb:icccm:SendEvent
                                  :destination id
                                  :event event))))
           (exwm-input--set-active-window id)
           (xcb:flush exwm--connection))
         id
         (or exwm--hints-input
             (not (memq xcb:Atom:WM_TAKE_FOCUS exwm--protocols)))
         (memq xcb:Atom:WM_TAKE_FOCUS exwm--protocols))))))

(defun exwm-input--update-timestamp (callback &rest args)
  "Fetch the latest timestamp from the server and feed it to CALLBACK.

ARGS are additional arguments to CALLBACK."
  (setq exwm-input--timestamp-callback (cons callback args))
  (exwm--log)
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
  (exwm--log)
  (when exwm-input--timestamp-callback
    (let ((obj (make-instance 'xcb:PropertyNotify)))
      (xcb:unmarshal obj data)
      (when (= exwm-input--timestamp-window
               (slot-value obj 'window))
        (apply (car exwm-input--timestamp-callback)
               (slot-value obj 'time)
               (cdr exwm-input--timestamp-callback))
        (setq exwm-input--timestamp-callback nil)))))

(defvar exwm-input--last-enter-notify-position nil)

(defun exwm-input--on-EnterNotify (data _synthetic)
  "Handle EnterNotify events."
  (let ((evt (make-instance 'xcb:EnterNotify))
        buffer window frame frame-xid edges fake-evt)
    (xcb:unmarshal evt data)
    (with-slots (time root event root-x root-y event-x event-y state) evt
      (setq buffer (exwm--id->buffer event)
            window (get-buffer-window buffer t))
      (exwm--log "buffer=%s; window=%s" buffer window)
      (when (and buffer window (not (eq window (selected-window)))
                 (not (equal exwm-input--last-enter-notify-position
                             (vector root-x root-y))))
        (setq frame (window-frame window)
              frame-xid (frame-parameter frame 'exwm-id))
        (unless (eq frame exwm-workspace--current)
          (if (exwm-workspace--workspace-p frame)
              ;; The X window is on another workspace.
              (exwm-workspace-switch frame)
            (with-current-buffer buffer
              (when (and (derived-mode-p 'exwm-mode)
                         (not (eq exwm--frame exwm-workspace--current)))
                ;; The floating X window is on another workspace.
                (exwm-workspace-switch exwm--frame)))))
        ;; Send a fake MotionNotify event to Emacs.
        (setq edges (window-inside-pixel-edges window)
              fake-evt (make-instance 'xcb:MotionNotify
                                      :detail 0
                                      :time time
                                      :root root
                                      :event frame-xid
                                      :child xcb:Window:None
                                      :root-x root-x
                                      :root-y root-y
                                      :event-x (+ event-x (elt edges 0))
                                      :event-y (+ event-y (elt edges 1))
                                      :state state
                                      :same-screen 1))
        (xcb:+request exwm--connection
            (make-instance 'xcb:SendEvent
                           :propagate 0
                           :destination frame-xid
                           :event-mask xcb:EventMask:NoEvent
                           :event (xcb:marshal fake-evt exwm--connection)))
        (xcb:flush exwm--connection))
      (setq exwm-input--last-enter-notify-position (vector root-x root-y)))))

(defun exwm-input--on-keysyms-update ()
  (exwm--log)
  (let ((exwm-input--global-prefix-keys nil))
    (exwm-input--update-global-prefix-keys)))

(defun exwm-input--on-buffer-list-update ()
  "Run in `buffer-list-update-hook' to track input focus."
  ;; `buffer-list-update-hook' is invoked by several functions
  ;; (`get-buffer-create', `select-window', `with-temp-buffer', etc.), but we
  ;; just want to notice when a different window has been selected, or when the
  ;; selected window displays a different buffer, so that we can set the focus
  ;; to the associated X window (in case of an `exwm-mode' buffer).  In order to
  ;; differentiate, we keep track of the last selected window and buffer in the
  ;; `exwm-input--update-focus-window' and
  ;; `exwm-input--update-focus-window-buffer' variables.
  (let* ((win (selected-window))
         (buf (window-buffer win)))
    (when (and (not (exwm-workspace--client-p))
               (not (and (eq exwm-input--update-focus-window win)
                         (eq exwm-input--update-focus-window-buffer buf))))
      (exwm--log "selected-window=%S current-buffer=%S" win buf)
      (setq exwm-input--update-focus-window win)
      (setq exwm-input--update-focus-window-buffer buf)
      (redirect-frame-focus (selected-frame) nil)
      (exwm-input--update-focus-defer))))

(defun exwm-input--update-focus-defer ()
  "Defer updating input focus."
  (when exwm-input--update-focus-defer-timer
    (cancel-timer exwm-input--update-focus-defer-timer))
  (if exwm-input--update-focus-lock
      (setq exwm-input--update-focus-defer-timer
            (exwm--defer 0 #'exwm-input--update-focus-defer))
    (setq exwm-input--update-focus-defer-timer nil)
    (when exwm-input--update-focus-timer
      (cancel-timer exwm-input--update-focus-timer))
    (setq exwm-input--update-focus-timer
          ;; Attempt to accumulate successive events close enough.
          (run-with-timer exwm-input--update-focus-interval
                          nil
                          #'exwm-input--update-focus-commit
                          exwm-input--update-focus-window))))

(defun exwm-input--update-focus-commit (window)
  "Commit updating input focus."
  (setq exwm-input--update-focus-lock t)
  (unwind-protect
      (exwm-input--update-focus window)
    (setq exwm-input--update-focus-lock nil)))

(defun exwm-input--update-focus (window)
  "Update input focus."
  (when (window-live-p window)
    (exwm--log "focus-window=%s focus-buffer=%s" window (window-buffer window))
    (with-current-buffer (window-buffer window)
      (if (derived-mode-p 'exwm-mode)
          (if (not (eq exwm--frame exwm-workspace--current))
              (progn
                (set-frame-parameter exwm--frame 'exwm-selected-window window)
                (exwm--defer 0 #'exwm-workspace-switch exwm--frame))
            (exwm--log "Set focus on #x%x" exwm--id)
            (when exwm--floating-frame
              ;; Adjust stacking orders of the floating X window.
              (xcb:+request exwm--connection
                  (make-instance 'xcb:ConfigureWindow
                                 :window exwm--id
                                 :value-mask xcb:ConfigWindow:StackMode
                                 :stack-mode xcb:StackMode:TopIf))
              (xcb:+request exwm--connection
                  (make-instance 'xcb:ConfigureWindow
                                 :window (frame-parameter exwm--floating-frame
                                                          'exwm-container)
                                 :value-mask (logior
                                              xcb:ConfigWindow:Sibling
                                              xcb:ConfigWindow:StackMode)
                                 :sibling exwm--id
                                 :stack-mode xcb:StackMode:Below))
              ;; This floating X window might be hide by `exwm-floating-hide'.
              (when (exwm-layout--iconic-state-p)
                (exwm-layout--show exwm--id window))
              (xcb:flush exwm--connection))
            (exwm-input--set-focus exwm--id))
        (when (eq (selected-window) window)
          (exwm--log "Focus on %s" window)
          (if (and (exwm-workspace--workspace-p (selected-frame))
                   (not (eq (selected-frame) exwm-workspace--current)))
              ;; The focus is on another workspace (e.g. it got clicked)
              ;; so switch to it.
              (progn
                (exwm--log "Switching to %s's workspace %s (%s)"
                           window
                           (window-frame window)
                           (selected-frame))
                (set-frame-parameter (selected-frame) 'exwm-selected-window
                                     window)
                (exwm--defer 0 #'exwm-workspace-switch (selected-frame)))
            ;; The focus is still on the current workspace.
            (if (not (and (exwm-workspace--minibuffer-own-frame-p)
                          (minibufferp)))
                (x-focus-frame (window-frame window))
              ;; X input focus should be set on the previously selected
              ;; frame.
              (x-focus-frame (window-frame (minibuffer-window))))
            (exwm-input--set-active-window)
            (xcb:flush exwm--connection)))))))

(defun exwm-input--set-active-window (&optional id)
  "Set _NET_ACTIVE_WINDOW."
  (exwm--log)
  (xcb:+request exwm--connection
      (make-instance 'xcb:ewmh:set-_NET_ACTIVE_WINDOW
                     :window exwm--root
                     :data (or id xcb:Window:None))))

(defun exwm-input--on-ButtonPress (data _synthetic)
  "Handle ButtonPress event."
  (let ((obj (make-instance 'xcb:ButtonPress))
        (mode xcb:Allow:SyncPointer)
        button-event window buffer frame)
    (xcb:unmarshal obj data)
    (exwm--log "major-mode=%s buffer=%s"
               major-mode (buffer-name (current-buffer)))
    (with-slots (detail event state) obj
      (setq button-event (xcb:keysyms:keysym->event exwm--connection
                                                    detail state)
            buffer (exwm--id->buffer event)
            window (get-buffer-window buffer t))
      (cond ((and (eq button-event exwm-input-move-event)
                  buffer
                  ;; Either an undecorated or a floating X window.
                  (with-current-buffer buffer
                    (or (not (derived-mode-p 'exwm-mode))
                        exwm--floating-frame)))
             ;; Move
             (exwm-floating--start-moveresize
              event xcb:ewmh:_NET_WM_MOVERESIZE_MOVE))
            ((and (eq button-event exwm-input-resize-event)
                  buffer
                  (with-current-buffer buffer
                    (or (not (derived-mode-p 'exwm-mode))
                        exwm--floating-frame)))
             ;; Resize
             (exwm-floating--start-moveresize event))
            (buffer
             ;; Click to focus
             (unless (eq window (selected-window))
               (setq frame (window-frame window))
               (unless (eq frame exwm-workspace--current)
                 (if (exwm-workspace--workspace-p frame)
                     ;; The X window is on another workspace
                     (exwm-workspace-switch frame)
                   (with-current-buffer buffer
                     (when (and (derived-mode-p 'exwm-mode)
                                (not (eq exwm--frame
                                         exwm-workspace--current)))
                       ;; The floating X window is on another workspace
                       (exwm-workspace-switch exwm--frame)))))
               ;; It has been reported that the `window' may have be deleted
               (if (window-live-p window)
                   (select-window window)
                 (setq window (get-buffer-window buffer t))
                 (when window (select-window window))))
             ;; Also process keybindings.
             (with-current-buffer buffer
               (when (derived-mode-p 'exwm-mode)
                 (cl-case exwm--input-mode
                   (line-mode
                    (setq mode (exwm-input--on-ButtonPress-line-mode
                                buffer button-event)))
                   (char-mode
                    (setq mode (exwm-input--on-ButtonPress-char-mode)))))))
            (t
             ;; Replay this event by default.
             (setq mode xcb:Allow:ReplayPointer))))
    (xcb:+request exwm--connection
        (make-instance 'xcb:AllowEvents :mode mode :time xcb:Time:CurrentTime))
    (xcb:flush exwm--connection))
  (run-hooks 'exwm-input--event-hook))

(defun exwm-input--on-KeyPress (data _synthetic)
  "Handle KeyPress event."
  (with-current-buffer (window-buffer (selected-window))
    (let ((obj (make-instance 'xcb:KeyPress)))
      (xcb:unmarshal obj data)
      (exwm--log "major-mode=%s buffer=%s"
                 major-mode (buffer-name (current-buffer)))
      (if (derived-mode-p 'exwm-mode)
          (cl-case exwm--input-mode
            (line-mode
             (exwm-input--on-KeyPress-line-mode obj data))
            (char-mode
             (exwm-input--on-KeyPress-char-mode obj data)))
        (exwm-input--on-KeyPress-char-mode obj)))
    (run-hooks 'exwm-input--event-hook)))

(defun exwm-input--on-CreateNotify (data _synthetic)
  "Handle CreateNotify events."
  (exwm--log)
  (let ((evt (make-instance 'xcb:CreateNotify)))
    (xcb:unmarshal evt data)
    (with-slots (window) evt
      (exwm-input--grab-global-prefix-keys window))))

(defun exwm-input--update-global-prefix-keys ()
  "Update `exwm-input--global-prefix-keys'."
  (exwm--log)
  (when exwm--connection
    (let ((original exwm-input--global-prefix-keys))
      (setq exwm-input--global-prefix-keys nil)
      (dolist (i exwm-input--global-keys)
        (cl-pushnew (elt i 0) exwm-input--global-prefix-keys))
      (unless (equal original exwm-input--global-prefix-keys)
        (apply #'exwm-input--grab-global-prefix-keys
               (slot-value (xcb:+request-unchecked+reply exwm--connection
                               (make-instance 'xcb:QueryTree
                                              :window exwm--root))
                           'children))))))

(defun exwm-input--grab-global-prefix-keys (&rest xwins)
  (exwm--log)
  (let ((req (make-instance 'xcb:GrabKey
                            :owner-events 0
                            :grab-window nil
                            :modifiers nil
                            :key nil
                            :pointer-mode xcb:GrabMode:Async
                            :keyboard-mode xcb:GrabMode:Async))
        keysyms keycode alt-modifier)
    (dolist (k exwm-input--global-prefix-keys)
      (setq keysyms (xcb:keysyms:event->keysyms exwm--connection k))
      (if (not keysyms)
          (warn "Key unavailable: %s" (key-description (vector k)))
        (setq keycode (xcb:keysyms:keysym->keycode exwm--connection
                                                   (caar keysyms)))
        (exwm--log "Grabbing key=%s (keysyms=%s keycode=%s)"
                   (single-key-description k) keysyms keycode)
        (dolist (keysym keysyms)
          (setf (slot-value req 'modifiers) (cdr keysym)
                (slot-value req 'key) keycode)
          ;; Also grab this key with num-lock mask set.
          (when (and (/= 0 xcb:keysyms:num-lock-mask)
                     (= 0 (logand (cdr keysym) xcb:keysyms:num-lock-mask)))
            (setf alt-modifier (logior (cdr keysym)
                                       xcb:keysyms:num-lock-mask)))
          (dolist (xwin xwins)
            (setf (slot-value req 'grab-window) xwin)
            (xcb:+request exwm--connection req)
            (when alt-modifier
              (setf (slot-value req 'modifiers) alt-modifier)
              (xcb:+request exwm--connection req))))))
    (xcb:flush exwm--connection)))

(defun exwm-input--set-key (key command)
  (exwm--log "key: %s, command: %s" key command)
  (global-set-key key command)
  (cl-pushnew key exwm-input--global-keys))

(defcustom exwm-input-global-keys nil
  "Global keys.

It is an alist of the form (key . command), meaning giving KEY (a key
sequence) a global binding as COMMAND.

Notes:
* Setting the value directly (rather than customizing it) after EXWM
  finishes initialization has no effect."
  :type '(alist :key-type key-sequence :value-type function)
  :set (lambda (symbol value)
         (when (boundp symbol)
           (dolist (i (symbol-value symbol))
             (global-unset-key (car i))))
         (set symbol value)
         (setq exwm-input--global-keys nil)
         (dolist (i value)
           (exwm-input--set-key (car i) (cdr i)))
         (when exwm--connection
           (exwm-input--update-global-prefix-keys))))

;;;###autoload
(defun exwm-input-set-key (key command)
  "Set a global key binding.

The new key binding only takes effect in real time when this command is
called interactively, and is lost when this session ends unless it's
specifically saved in the Customize interface for `exwm-input-global-keys'.

In configuration you should customize or set `exwm-input-global-keys'
instead."
  (interactive "KSet key globally: \nCSet key %s to command: ")
  (exwm--log)
  (setq exwm-input-global-keys (append exwm-input-global-keys
                                       (list (cons key command))))
  (exwm-input--set-key key command)
  (when (called-interactively-p 'any)
    (exwm-input--update-global-prefix-keys)))

;; Putting (t . EVENT) into `unread-command-events' does not really work
;; as documented for Emacs < 26.2.
(eval-and-compile
  (if (or (< emacs-major-version 26)
          (and (= emacs-major-version 26)
               (< emacs-minor-version 2)))
      (defsubst exwm-input--unread-event (event)
        (setq unread-command-events
              (append unread-command-events (list event))))
    (defsubst exwm-input--unread-event (event)
      (setq unread-command-events
            (append unread-command-events `((t . ,event)))))))

(defun exwm-input--mimic-read-event (event)
  "Process EVENT as if it were returned by `read-event'."
  (exwm--log)
  (unless (eq 0 extra-keyboard-modifiers)
    (setq event (event-convert-list (append (event-modifiers
                                             extra-keyboard-modifiers)
                                            event))))
  (when (characterp event)
    (let ((event* (when keyboard-translate-table
                    (aref keyboard-translate-table event))))
      (when event*
        (setq event event*))))
  event)

(cl-defun exwm-input--translate (key)
  (let (translation)
    (dolist (map (list input-decode-map
                       local-function-key-map
                       key-translation-map))
      (setq translation (lookup-key map key))
      (if (functionp translation)
          (cl-return-from exwm-input--translate (funcall translation nil))
        (when (vectorp translation)
          (cl-return-from exwm-input--translate translation)))))
  key)

(defun exwm-input--cache-event (event &optional temp-line-mode)
  "Cache EVENT."
  (exwm--log "%s" event)
  (setq exwm-input--line-mode-cache
        (vconcat exwm-input--line-mode-cache (vector event)))
  ;; Attempt to translate this key sequence.
  (setq exwm-input--line-mode-cache
        (exwm-input--translate exwm-input--line-mode-cache))
  ;; When the key sequence is complete (not a keymap).
  ;; Note that `exwm-input--line-mode-cache' might get translated to nil, for
  ;; example 'mouse--down-1-maybe-follows-link' does this.
  (if (and exwm-input--line-mode-cache
           (keymapp (key-binding exwm-input--line-mode-cache)))
      ;; Grab keyboard temporarily to intercept the complete key sequence.
      (when temp-line-mode
        (setq exwm-input--temp-line-mode t)
        (exwm-input--grab-keyboard))
    (setq exwm-input--line-mode-cache nil)
    (when exwm-input--temp-line-mode
      (setq exwm-input--temp-line-mode nil)
      (exwm-input--release-keyboard))))

(defun exwm-input--event-passthrough-p (event)
  "Whether EVENT should be passed to Emacs.
Current buffer must be an `exwm-mode' buffer."
  (or exwm-input-line-mode-passthrough
      exwm-input--during-command
      ;; Forward the event when there is an incomplete key
      ;; sequence or when the minibuffer is active.
      exwm-input--line-mode-cache
      (eq (active-minibuffer-window) (selected-window))
      ;;
      (memq event exwm-input--global-prefix-keys)
      (memq event exwm-input-prefix-keys)
      (when overriding-terminal-local-map
        (lookup-key overriding-terminal-local-map
                    (vector event)))
      (lookup-key (current-local-map) (vector event))
      (gethash event exwm-input--simulation-keys)))

(defun exwm-input--noop (&rest _args)
  "A placeholder command."
  (interactive))

(defun exwm-input--on-KeyPress-line-mode (key-press raw-data)
  "Parse X KeyPress event to Emacs key event and then feed the command loop."
  (with-slots (detail state) key-press
    (let ((keysym (xcb:keysyms:keycode->keysym exwm--connection detail state))
          event raw-event mode)
      (exwm--log "%s" keysym)
      (when (and (/= 0 (car keysym))
                 (setq raw-event (xcb:keysyms:keysym->event
                                  exwm--connection (car keysym)
                                  (logand state (lognot (cdr keysym)))))
                 (setq event (exwm-input--mimic-read-event raw-event))
                 (exwm-input--event-passthrough-p event))
        (setq mode xcb:Allow:AsyncKeyboard)
        (exwm-input--cache-event event)
        (exwm-input--unread-event raw-event))
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
        (if defining-kbd-macro
            (when event
              ;; Make Emacs aware of this event when defining keyboard macros.
              (set-transient-map `(keymap (t . ,#'exwm-input--noop)))
              (exwm-input--unread-event event))
          ;; Fool some packages into thinking there is a change in the buffer.
          (when event
            (setq last-command #'exwm-input--noop)
            (run-hooks 'pre-command-hook)
            (run-hooks 'post-command-hook))))
      (xcb:+request exwm--connection
          (make-instance 'xcb:AllowEvents
                         :mode mode
                         :time xcb:Time:CurrentTime))
      (xcb:flush exwm--connection))))

(defun exwm-input--on-KeyPress-char-mode (key-press &optional _raw-data)
  "Handle KeyPress event in char-mode."
  (with-slots (detail state) key-press
    (let ((keysym (xcb:keysyms:keycode->keysym exwm--connection detail state))
          event raw-event)
      (exwm--log "%s" keysym)
      (when (and (/= 0 (car keysym))
                 (setq raw-event (xcb:keysyms:keysym->event
                                  exwm--connection (car keysym)
                                  (logand state (lognot (cdr keysym)))))
                 (setq event (exwm-input--mimic-read-event raw-event)))
        (if (not (derived-mode-p 'exwm-mode))
            (exwm-input--unread-event raw-event)
          (exwm-input--cache-event event t)
          (exwm-input--unread-event raw-event)))))
  (xcb:+request exwm--connection
      (make-instance 'xcb:AllowEvents
                     :mode xcb:Allow:AsyncKeyboard
                     :time xcb:Time:CurrentTime))
  (xcb:flush exwm--connection))

(defun exwm-input--on-ButtonPress-line-mode (buffer button-event)
  "Handle button events in line mode.
BUFFER is the `exwm-mode' buffer the event was generated
on. BUTTON-EVENT is the X event converted into an Emacs event.

The return value is used as event_mode to release the original
button event."
  (with-current-buffer buffer
    (let ((read-event (exwm-input--mimic-read-event button-event)))
      (exwm--log "%s" read-event)
      (if (and read-event
               (exwm-input--event-passthrough-p read-event))
          ;; The event should be forwarded to emacs
          (progn
            (exwm-input--cache-event read-event)
            (exwm-input--unread-event button-event)
            xcb:Allow:SyncPointer)
        ;; The event should be replayed
        xcb:Allow:ReplayPointer))))

(defun exwm-input--on-ButtonPress-char-mode ()
  "Handle button events in char-mode.
The return value is used as event_mode to release the original
button event."
  (exwm--log)
  xcb:Allow:ReplayPointer)

(defun exwm-input--update-mode-line (id)
  "Update the propertized `mode-line-process' for window ID."
  (exwm--log "#x%x" id)
  (let (help-echo cmd mode)
    (with-current-buffer (exwm--id->buffer id)
      (cl-case exwm--input-mode
        (line-mode
         (setq mode "line"
               help-echo "mouse-1: Switch to char-mode"
               cmd (lambda ()
                     (interactive)
                     (exwm-input-release-keyboard id))))
        (char-mode
         (setq mode "char"
               help-echo "mouse-1: Switch to line-mode"
               cmd (lambda ()
                     (interactive)
                     (exwm-input-grab-keyboard id)))))
      (setq mode-line-process
            `(": "
              (:propertize ,mode
                           help-echo ,help-echo
                           mouse-face mode-line-highlight
                           local-map
                           (keymap
                            (mode-line
                             keymap
                             (down-mouse-1 . ,cmd))))))
      (force-mode-line-update))))

(defun exwm-input--grab-keyboard (&optional id)
  "Grab all key events on window ID."
  (unless id (setq id (exwm--buffer->id (window-buffer))))
  (when id
    (exwm--log "id=#x%x" id)
    (when (xcb:+request-checked+request-check exwm--connection
              (make-instance 'xcb:GrabKey
                             :owner-events 0
                             :grab-window id
                             :modifiers xcb:ModMask:Any
                             :key xcb:Grab:Any
                             :pointer-mode xcb:GrabMode:Async
                             :keyboard-mode xcb:GrabMode:Sync))
      (exwm--log "Failed to grab keyboard for #x%x" id))
    (let ((buffer (exwm--id->buffer id)))
      (when buffer
        (with-current-buffer buffer
          (setq exwm--input-mode 'line-mode))))))

(defun exwm-input--release-keyboard (&optional id)
  "Ungrab all key events on window ID."
  (unless id (setq id (exwm--buffer->id (window-buffer))))
  (when id
    (exwm--log "id=#x%x" id)
    (when (xcb:+request-checked+request-check exwm--connection
              (make-instance 'xcb:UngrabKey
                             :key xcb:Grab:Any
                             :grab-window id
                             :modifiers xcb:ModMask:Any))
      (exwm--log "Failed to release keyboard for #x%x" id))
    (exwm-input--grab-global-prefix-keys id)
    (let ((buffer (exwm--id->buffer id)))
      (when buffer
        (with-current-buffer buffer
          (setq exwm--input-mode 'char-mode))))))

;;;###autoload
(defun exwm-input-grab-keyboard (&optional id)
  "Switch to line-mode."
  (interactive (list (when (derived-mode-p 'exwm-mode)
                       (exwm--buffer->id (window-buffer)))))
  (when id
    (exwm--log "id=#x%x" id)
    (setq exwm--selected-input-mode 'line-mode)
    (exwm-input--grab-keyboard id)
    (exwm-input--update-mode-line id)))

;;;###autoload
(defun exwm-input-release-keyboard (&optional id)
  "Switch to char-mode."
  (interactive (list (when (derived-mode-p 'exwm-mode)
                       (exwm--buffer->id (window-buffer)))))
  (when id
    (exwm--log "id=#x%x" id)
    (setq exwm--selected-input-mode  'char-mode)
    (exwm-input--release-keyboard id)
    (exwm-input--update-mode-line id)))

;;;###autoload
(defun exwm-input-toggle-keyboard (&optional id)
  "Toggle between 'line-mode' and 'char-mode'."
  (interactive (list (when (derived-mode-p 'exwm-mode)
                       (exwm--buffer->id (window-buffer)))))
  (when id
    (exwm--log "id=#x%x" id)
    (with-current-buffer (exwm--id->buffer id)
      (cl-case exwm--input-mode
        (line-mode
         (exwm-input-release-keyboard id))
        (char-mode
         (exwm-reset))))))

(defun exwm-input--fake-key (event)
  "Fake a key event equivalent to Emacs event EVENT."
  (let* ((keysyms (xcb:keysyms:event->keysyms exwm--connection event))
         keycode id)
    (when (= 0 (caar keysyms))
      (user-error "[EXWM] Invalid key: %s" (single-key-description event)))
    (setq keycode (xcb:keysyms:keysym->keycode exwm--connection
                                               (caar keysyms)))
    (when (/= 0 keycode)
      (setq id (exwm--buffer->id (window-buffer (selected-window))))
      (exwm--log "id=#x%x event=%s keycode" id event keycode)
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
                                                  :state (cdar keysyms)
                                                  :same-screen 1)
                                   exwm--connection)))))
    (xcb:flush exwm--connection)))

;;;###autoload
(cl-defun exwm-input-send-next-key (times &optional end-key)
  "Send next key to client window.

EXWM will prompt for the key to send.  This command can be prefixed to send
multiple keys.  If END-KEY is non-nil, stop sending keys if it's pressed."
  (interactive "p")
  (exwm--log)
  (unless (derived-mode-p 'exwm-mode)
    (cl-return-from exwm-input-send-next-key))
  (when (> times 12) (setq times 12))
  (let (key keys)
    (dotimes (i times)
      ;; Skip events not from keyboard
      (let ((exwm-input-line-mode-passthrough t))
        (catch 'break
          (while t
            (setq key (read-key (format "Send key: %s (%d/%d) %s"
                                        (key-description keys)
                                        (1+ i) times
                                        (if end-key
                                            (concat "To exit, press: "
                                                    (key-description
                                                     (list end-key)))
                                          ""))))
            (unless (listp key) (throw 'break nil)))))
      (setq keys (vconcat keys (vector key)))
      (when (eq key end-key) (cl-return-from exwm-input-send-next-key))
      (exwm-input--fake-key key))))

(defun exwm-input--set-simulation-keys (simulation-keys &optional no-refresh)
  "Set simulation keys."
  (exwm--log "%s" simulation-keys)
  (unless no-refresh
    ;; Unbind simulation keys.
    (let ((hash (buffer-local-value 'exwm-input--simulation-keys
                                    (current-buffer))))
      (when (hash-table-p hash)
        (maphash (lambda (key _value)
                   (when (sequencep key)
                     (if exwm-input--local-simulation-keys
                         (local-unset-key key)
                       (define-key exwm-mode-map key nil))))
                 hash)))
    ;; Abandon the old hash table.
    (setq exwm-input--simulation-keys (make-hash-table :test #'equal)))
  (dolist (i simulation-keys)
    (let ((original (vconcat (car i)))
          (simulated (cdr i)))
      (setq simulated (if (sequencep simulated)
                          (append simulated nil)
                        (list simulated)))
      ;; The key stored is a key sequence (vector).
      ;; The value stored is a list of key events.
      (puthash original simulated exwm-input--simulation-keys)
      ;; Also mark the prefix key as used.
      (puthash (aref original 0) t exwm-input--simulation-keys)))
  ;; Update keymaps.
  (maphash (lambda (key _value)
             (when (sequencep key)
               (if exwm-input--local-simulation-keys
                   (local-set-key key #'exwm-input-send-simulation-key)
                 (define-key exwm-mode-map key
                   #'exwm-input-send-simulation-key))))
           exwm-input--simulation-keys))

(defun exwm-input-set-simulation-keys (simulation-keys)
  "Please customize or set `exwm-input-simulation-keys' instead."
  (declare (obsolete nil "26"))
  (exwm-input--set-simulation-keys simulation-keys))

(defcustom exwm-input-simulation-keys nil
  "Simulation keys.

It is an alist of the form (original-key . simulated-key), where both
original-key and simulated-key are key sequences.  Original-key is what you
type to an X window in line-mode which then gets translated to simulated-key
by EXWM and forwarded to the X window.

Notes:
* Setting the value directly (rather than customizing it) after EXWM
  finishes initialization has no effect.
* Original-keys consist of multiple key events are only supported in Emacs
  26.2 and later.
* A minority of applications do not accept simulated keys by default.  It's
  required to customize them to accept events sent by SendEvent.
* The predefined examples in the Customize interface are not guaranteed to
  work for all applications.  This can be tweaked on a per application basis
  with `exwm-input-set-local-simulation-keys'."
  :type '(alist :key-type (key-sequence :tag "Original")
                :value-type (choice (key-sequence :tag "User-defined")
                                    (key-sequence :tag "Move left" [left])
                                    (key-sequence :tag "Move right" [right])
                                    (key-sequence :tag "Move up" [up])
                                    (key-sequence :tag "Move down" [down])
                                    (key-sequence :tag "Move to BOL" [home])
                                    (key-sequence :tag "Move to EOL" [end])
                                    (key-sequence :tag "Page up" [prior])
                                    (key-sequence :tag "Page down" [next])
                                    (key-sequence :tag "Copy" [C-c])
                                    (key-sequence :tag "Paste" [C-v])
                                    (key-sequence :tag "Delete" [delete])
                                    (key-sequence :tag "Delete to EOL"
                                                  [S-end delete])))
  :set (lambda (symbol value)
         (set symbol value)
         (exwm-input--set-simulation-keys value)))

(defcustom exwm-input-pre-post-command-blacklist '(exit-minibuffer
                                                   abort-recursive-edit
                                                   minibuffer-keyboard-quit)
  "Commands impossible to detect with `post-command-hook'."
  :type '(repeat function))

(cl-defun exwm-input--read-keys (prompt stop-key)
  (let ((cursor-in-echo-area t)
        keys key)
    (while (not (eq key stop-key))
      (setq key (read-key (format "%s (terminate with %s): %s"
                                  prompt
                                  (key-description (vector stop-key))
                                  (key-description keys)))
            keys (vconcat keys (vector key))))
    (when (> (length keys) 1)
      (substring keys 0 -1))))

;;;###autoload
(defun exwm-input-set-simulation-key (original-key simulated-key)
  "Set a simulation key.

The simulation key takes effect in real time, but is lost when this session
ends unless it's specifically saved in the Customize interface for
`exwm-input-simulation-keys'."
  (interactive
   (let (original simulated)
     (setq original (exwm-input--read-keys "Translate from" ?\C-g))
     (when original
       (setq simulated (exwm-input--read-keys
                        (format "Translate from %s to"
                                (key-description original))
                        ?\C-g)))
     (list original simulated)))
  (exwm--log "original: %s, simulated: %s" original-key simulated-key)
  (when (and original-key simulated-key)
    (let ((entry `((,original-key . ,simulated-key))))
      (setq exwm-input-simulation-keys (append exwm-input-simulation-keys
                                               entry))
      (exwm-input--set-simulation-keys entry t))))

(defun exwm-input--unset-simulation-keys ()
  "Clear simulation keys and key bindings defined."
  (exwm--log)
  (when (hash-table-p exwm-input--simulation-keys)
    (maphash (lambda (key _value)
               (when (sequencep key)
                 (define-key exwm-mode-map key nil)))
             exwm-input--simulation-keys)
    (clrhash exwm-input--simulation-keys)))

(defun exwm-input-set-local-simulation-keys (simulation-keys)
  "Set buffer-local simulation keys.

SIMULATION-KEYS is an alist of the form (original-key . simulated-key),
where both ORIGINAL-KEY and SIMULATED-KEY are key sequences."
  (exwm--log)
  (make-local-variable 'exwm-input--simulation-keys)
  (use-local-map (copy-keymap exwm-mode-map))
  (let ((exwm-input--local-simulation-keys t))
    (exwm-input--set-simulation-keys simulation-keys)))

;;;###autoload
(cl-defun exwm-input-send-simulation-key (times)
  "Fake a key event according to the last input key sequence."
  (interactive "p")
  (exwm--log)
  (unless (derived-mode-p 'exwm-mode)
    (cl-return-from exwm-input-send-simulation-key))
  (let ((keys (gethash (this-single-command-keys)
                       exwm-input--simulation-keys)))
    (dotimes (_ times)
      (dolist (key keys)
        (exwm-input--fake-key key)))))

;;;###autoload
(defmacro exwm-input-invoke-factory (keys)
  "Make a command that invokes KEYS when called.

One use is to access the keymap bound to KEYS (as prefix keys) in char-mode."
  (let* ((keys (kbd keys))
         (description (key-description keys)))
    `(defun ,(intern (concat "exwm-input--invoke--" description)) ()
       ,(format "Invoke `%s'." description)
       (interactive)
       (mapc (lambda (key)
               (exwm-input--cache-event key t)
               (exwm-input--unread-event key))
             ',(listify-key-sequence keys)))))

(defun exwm-input--on-pre-command ()
  "Run in `pre-command-hook'."
  (unless (or (eq this-command #'exwm-input--noop)
              (memq this-command exwm-input-pre-post-command-blacklist))
    (setq exwm-input--during-command t)))

(defun exwm-input--on-post-command ()
  "Run in `post-command-hook'."
  (unless (eq this-command #'exwm-input--noop)
    (setq exwm-input--during-command nil)))

(defun exwm-input--on-minibuffer-setup ()
  "Run in `minibuffer-setup-hook' to grab keyboard if necessary."
  (exwm--log)
  (with-current-buffer
      (window-buffer (frame-selected-window exwm-workspace--current))
    (when (and (derived-mode-p 'exwm-mode)
               (not (exwm-workspace--client-p))
               (eq exwm--selected-input-mode 'char-mode))
      (exwm-input--grab-keyboard exwm--id))))

(defun exwm-input--on-minibuffer-exit ()
  "Run in `minibuffer-exit-hook' to release keyboard if necessary."
  (exwm--log)
  (with-current-buffer
      (window-buffer (frame-selected-window exwm-workspace--current))
    (when (and (derived-mode-p 'exwm-mode)
               (not (exwm-workspace--client-p))
               (eq exwm--selected-input-mode 'char-mode)
               (eq exwm--input-mode 'line-mode))
      (exwm-input--release-keyboard exwm--id))))

(defun exwm-input--on-echo-area-dirty ()
  "Run when new message arrives to grab keyboard if necessary."
  (exwm--log)
  (when (and (not (active-minibuffer-window))
             (not (exwm-workspace--client-p))
             cursor-in-echo-area)
    (exwm-input--on-minibuffer-setup)))

(defun exwm-input--on-echo-area-clear ()
  "Run in `echo-area-clear-hook' to release keyboard if necessary."
  (exwm--log)
  (unless (current-message)
    (exwm-input--on-minibuffer-exit)))

(defun exwm-input--init ()
  "Initialize the keyboard module."
  (exwm--log)
  ;; Refresh keyboard mapping
  (xcb:keysyms:init exwm--connection #'exwm-input--on-keysyms-update)
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
  (xcb:+request exwm--connection
      (make-instance 'xcb:ewmh:set-_NET_WM_NAME
                     :window exwm-input--timestamp-window
                     :data "EXWM: exwm-input--timestamp-window"))
  (setq exwm-input--timestamp-atom (exwm--intern-atom "_TIME"))
  ;; Initialize global keys.
  (dolist (i exwm-input-global-keys)
    (exwm-input--set-key (car i) (cdr i)))
  ;; Initialize simulation keys.
  (when exwm-input-simulation-keys
    (exwm-input--set-simulation-keys exwm-input-simulation-keys))
  ;; Attach event listeners
  (xcb:+event exwm--connection 'xcb:PropertyNotify
              #'exwm-input--on-PropertyNotify)
  (xcb:+event exwm--connection 'xcb:CreateNotify #'exwm-input--on-CreateNotify)
  (xcb:+event exwm--connection 'xcb:KeyPress #'exwm-input--on-KeyPress)
  (xcb:+event exwm--connection 'xcb:ButtonPress #'exwm-input--on-ButtonPress)
  (xcb:+event exwm--connection 'xcb:ButtonRelease
              #'exwm-floating--stop-moveresize)
  (xcb:+event exwm--connection 'xcb:MotionNotify
              #'exwm-floating--do-moveresize)
  (when mouse-autoselect-window
    (xcb:+event exwm--connection 'xcb:EnterNotify
                #'exwm-input--on-EnterNotify))
  ;; Control `exwm-input--during-command'
  (add-hook 'pre-command-hook #'exwm-input--on-pre-command)
  (add-hook 'post-command-hook #'exwm-input--on-post-command)
  ;; Grab/Release keyboard when minibuffer/echo becomes active/inactive.
  (add-hook 'minibuffer-setup-hook #'exwm-input--on-minibuffer-setup)
  (add-hook 'minibuffer-exit-hook #'exwm-input--on-minibuffer-exit)
  (setq exwm-input--echo-area-timer
        (run-with-idle-timer 0 t #'exwm-input--on-echo-area-dirty))
  (add-hook 'echo-area-clear-hook #'exwm-input--on-echo-area-clear)
  ;; Update focus when buffer list updates
  (add-hook 'buffer-list-update-hook #'exwm-input--on-buffer-list-update))

(defun exwm-input--post-init ()
  "The second stage in the initialization of the input module."
  (exwm--log)
  (exwm-input--update-global-prefix-keys))

(defun exwm-input--exit ()
  "Exit the input module."
  (exwm--log)
  (exwm-input--unset-simulation-keys)
  (remove-hook 'pre-command-hook #'exwm-input--on-pre-command)
  (remove-hook 'post-command-hook #'exwm-input--on-post-command)
  (remove-hook 'minibuffer-setup-hook #'exwm-input--on-minibuffer-setup)
  (remove-hook 'minibuffer-exit-hook #'exwm-input--on-minibuffer-exit)
  (when exwm-input--echo-area-timer
    (cancel-timer exwm-input--echo-area-timer)
    (setq exwm-input--echo-area-timer nil))
  (remove-hook 'echo-area-clear-hook #'exwm-input--on-echo-area-clear)
  (remove-hook 'buffer-list-update-hook #'exwm-input--on-buffer-list-update)
  (when exwm-input--update-focus-defer-timer
    (cancel-timer exwm-input--update-focus-defer-timer))
  (when exwm-input--update-focus-timer
    (cancel-timer exwm-input--update-focus-timer))
  ;; Make input focus working even without a WM.
  (xcb:+request exwm--connection
      (make-instance 'xcb:SetInputFocus
                     :revert-to xcb:InputFocus:PointerRoot
                     :focus exwm--root
                     :time xcb:Time:CurrentTime))
  (xcb:flush exwm--connection))



(provide 'exwm-input)

;;; exwm-input.el ends here
