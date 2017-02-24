;;; exwm-core.el --- Core definitions  -*- lexical-binding: t -*-

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

;; This module includes core definitions of variables, macros, functions, etc
;; shared by various other modules.

;;; Code:

(require 'kmacro)

(require 'xcb)
(require 'xcb-icccm)
(require 'xcb-ewmh)

(eval-and-compile
  (defvar exwm-debug-on nil "Non-nil to turn on debug for EXWM."))

(defmacro exwm--log (format-string &rest args)
  "Print debug message."
  (when exwm-debug-on
    `(message (concat "[EXWM] " ,format-string) ,@args)))

(defmacro exwm--debug (&rest forms)
  (when exwm-debug-on `(progn ,@forms)))

(defvar exwm--connection nil "X connection.")
(defvar exwm--root nil "Root window.")
(defvar exwm--id-buffer-alist nil "Alist of (<X window ID> . <Emacs buffer>).")

(defsubst exwm--id->buffer (id)
  "X window ID => Emacs buffer."
  (cdr (assoc id exwm--id-buffer-alist)))

(defsubst exwm--buffer->id (buffer)
  "Emacs buffer BUFFER => X window ID."
  (car (rassoc buffer exwm--id-buffer-alist)))

(defun exwm--lock (&rest _args)
  "Lock (disable all events)."
  (xcb:+request exwm--connection
      (make-instance 'xcb:ChangeWindowAttributes
                     :window exwm--root
                     :value-mask xcb:CW:EventMask
                     :event-mask xcb:EventMask:NoEvent))
  (xcb:flush exwm--connection))

(defun exwm--unlock (&rest _args)
  "Unlock (enable all events)."
  (xcb:+request exwm--connection
      (make-instance 'xcb:ChangeWindowAttributes
                     :window exwm--root
                     :value-mask xcb:CW:EventMask
                     :event-mask (eval-when-compile
                                   (logior xcb:EventMask:SubstructureRedirect
                                           xcb:EventMask:StructureNotify))))
  (xcb:flush exwm--connection))

(defconst exwm--client-event-mask
  (eval-when-compile
    (logior xcb:EventMask:StructureNotify xcb:EventMask:PropertyChange))
  "Event mask set on all managed windows.")

(defvar exwm-input-line-mode-passthrough)
(defvar exwm-input--global-prefix-keys)
(defvar exwm-input-prefix-keys)
(defvar exwm-input--simulation-prefix-keys)

(declare-function exwm-input--fake-key "exwm-input.el" (event))
(declare-function exwm-input--on-KeyPress-line-mode "exwm-input.el"
                  (key-press raw-data))

;; Internal variables
(defvar-local exwm--id nil)               ;window ID
(defvar-local exwm--container nil)        ;container
(defvar-local exwm--frame nil)            ;workspace frame
(defvar-local exwm--floating-frame nil)   ;floating frame
(defvar-local exwm--mode-line-format nil) ;save mode-line-format
(defvar-local exwm--floating-frame-position nil) ;used in fullscreen
(defvar-local exwm--fixed-size nil)              ;fixed size
(defvar-local exwm--keyboard-grabbed nil)        ;Keyboard grabbed.
(defvar-local exwm--on-KeyPress         ;KeyPress event handler
  #'exwm-input--on-KeyPress-line-mode)
;; Properties
(defvar-local exwm-window-type nil "_NET_WM_WINDOW_TYPE.")
(defvar-local exwm--geometry nil)
(defvar-local exwm-class-name nil "Class name in WM_CLASS.")
(defvar-local exwm-instance-name nil "Instance name in WM_CLASS.")
(defvar-local exwm-title nil "Window title (either _NET_WM_NAME or WM_NAME)")
(defvar-local exwm--title-is-utf8 nil)
(defvar-local exwm-transient-for nil "WM_TRANSIENT_FOR.")
(defvar-local exwm--protocols nil)
(defvar-local exwm-state xcb:icccm:WM_STATE:NormalState "WM_STATE.")
(defvar-local exwm--ewmh-state nil "_NET_WM_STATE.")
;; _NET_WM_NORMAL_HINTS
(defvar-local exwm--normal-hints-x nil)
(defvar-local exwm--normal-hints-y nil)
(defvar-local exwm--normal-hints-width nil)
(defvar-local exwm--normal-hints-height nil)
(defvar-local exwm--normal-hints-min-width nil)
(defvar-local exwm--normal-hints-min-height nil)
(defvar-local exwm--normal-hints-max-width nil)
(defvar-local exwm--normal-hints-max-height nil)
;; (defvar-local exwm--normal-hints-win-gravity nil)
;; WM_HINTS
(defvar-local exwm--hints-input nil)
(defvar-local exwm--hints-urgency nil)
;; _MOTIF_WM_HINTS
(defvar-local exwm--mwm-hints-decorations t)

(declare-function exwm-floating-hide "exwm-floating.el")
(declare-function exwm-floating-toggle-floating "exwm-floating.el")
(declare-function exwm-input-release-keyboard "exwm-input.el")
(declare-function exwm-input-send-next-key "exwm-input.el" (times))
(declare-function exwm-layout-set-fullscreen "exwm-layout.el" (&optional id))
(declare-function exwm-layout-toggle-mode-line "exwm-layout.el")
(declare-function exwm-workspace-move-window "exwm-workspace.el"
                  (frame-or-index &optional id))

(defvar exwm-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-f" #'exwm-layout-set-fullscreen)
    (define-key map "\C-c\C-h" #'exwm-floating-hide)
    (define-key map "\C-c\C-k" #'exwm-input-release-keyboard)
    (define-key map "\C-c\C-m" #'exwm-workspace-move-window)
    (define-key map "\C-c\C-q" #'exwm-input-send-next-key)
    (define-key map "\C-c\C-t\C-f" #'exwm-floating-toggle-floating)
    (define-key map "\C-c\C-t\C-m" #'exwm-layout-toggle-mode-line)
    map)
  "Keymap for `exwm-mode'.")

(defvar exwm--kmacro-map
  (let ((map (make-sparse-keymap)))
    (define-key map [t]
      (lambda ()
        (interactive)
        (cond
         ((or exwm-input-line-mode-passthrough
              ;; Do not test `exwm-input--during-command'.
              (active-minibuffer-window)
              (memq last-input-event exwm-input--global-prefix-keys)
              (memq last-input-event exwm-input-prefix-keys)
              (memq last-input-event exwm-input--simulation-prefix-keys))
          (set-transient-map (make-composed-keymap (list exwm-mode-map
                                                         global-map)))
          (push last-input-event unread-command-events))
         (t
          (exwm-input--fake-key last-input-event)))))
    map)
  "Keymap used when executing keyboard macros.")

;; This menu mainly acts as an reminder for users.  Thus it should be as
;; detailed as possible, even some entries do not make much sense here.
;; Also, inactive entries should be disabled rather than hidden.
(easy-menu-define exwm-mode-menu exwm-mode-map
  "Menu for `exwm-mode'."
  '("EXWM"
    "---"
    "*General*"
    "---"
    ["Toggle floating" exwm-floating-toggle-floating]
    ["Toggle fullscreen mode" exwm-layout-toggle-fullscreen]
    ["Hide window" exwm-floating-hide exwm--floating-frame]
    ["Close window" (kill-buffer (current-buffer))]

    "---"
    "*Resizing*"
    "---"
    ["Toggle mode-line" exwm-layout-toggle-mode-line :keys "C-c C-t C-m"]
    ["Enlarge window vertically" exwm-layout-enlarge-window]
    ["Enlarge window horizontally" exwm-layout-enlarge-window-horizontally]
    ["Shrink window vertically" exwm-layout-shrink-window]
    ["Shrink window horizontally" exwm-layout-shrink-window-horizontally]

    "---"
    "*Keyboard*"
    "---"
    ["Toggle keyboard mode" exwm-input-toggle-keyboard]
    ["Send key" exwm-input-send-next-key exwm--keyboard-grabbed]
    ;; This is merely a reference.
    ("Send simulation key" :filter
     (lambda (&rest _args)
       (mapcar (lambda (i)
                 (let ((keys (cdr i)))
                   (if (vectorp keys)
                       (setq keys (append keys))
                     (unless (sequencep keys)
                       (setq keys (list keys))))
                   (vector (key-description keys)
                           `(lambda ()
                              (interactive)
                              (dolist (key ',keys)
                                (exwm-input--fake-key key)))
                           :keys (key-description (car i)))))
               exwm-input--simulation-keys)))

    ["Define global binding" exwm-input-set-key]

    "---"
    "*Workspace*"
    "---"
    ["Add workspace" exwm-workspace-add]
    ["Delete current workspace" exwm-workspace-delete]
    ["Move workspace to" exwm-workspace-move]
    ["Swap workspaces" exwm-workspace-swap]
    ["Move X window to" exwm-workspace-move-window :keys "C-c C-m"]
    ["Move X window from" exwm-workspace-switch-to-buffer]
    ["Toggle minibuffer" exwm-workspace-toggle-minibuffer]
    ["Switch workspace" exwm-workspace-switch]
    ;; Place this entry at bottom to avoid selecting others by accident.
    ("Switch to" :filter
     (lambda (&rest _args)
       (mapcar (lambda (i)
                 `[,(format "workspace %d" i)
                   (lambda ()
                     (interactive)
                     (exwm-workspace-switch ,i))
                   (/= ,i exwm-workspace-current-index)])
               (number-sequence 0 (1- (exwm-workspace--count))))))))

(declare-function exwm-manage--kill-buffer-query-function "exwm-manage.el")

(define-derived-mode exwm-mode nil "EXWM"
  "Major mode for managing X windows.

\\{exwm-mode-map}"
  ;;
  (setq mode-name
        '(:eval (propertize "EXWM" 'face
                            (when (cl-some (lambda (i)
                                             (frame-parameter i 'exwm-urgency))
                                           exwm-workspace--list)
                              'font-lock-warning-face))))
  ;; Change major-mode is not allowed
  (add-hook 'change-major-mode-hook #'kill-buffer nil t)
  ;; Kill buffer -> close window
  (add-hook 'kill-buffer-query-functions
            #'exwm-manage--kill-buffer-query-function nil t)
  ;; Redirect events when executing keyboard macros.
  (push `(executing-kbd-macro . ,exwm--kmacro-map)
        minor-mode-overriding-map-alist)
  (setq buffer-read-only t
        left-margin-width nil
        right-margin-width nil
        left-fringe-width 0
        right-fringe-width 0
        vertical-scroll-bar nil))



(provide 'exwm-core)

;;; exwm-core.el ends here
