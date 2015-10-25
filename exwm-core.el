;;; exwm-core.el --- Core definitions  -*- lexical-binding: t -*-

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

;; This module includes core definitions of variables, macros, functions, etc
;; shared by various other modules.

;;; Code:

(require 'xcb)
(require 'xcb-icccm)
(require 'xcb-ewmh)

(eval-and-compile
  (defvar exwm-debug-on nil "Non-nil to turn on debug for EXWM."))

(defmacro exwm--log (format-string &rest args)
  "Print debug message."
  (when exwm-debug-on
    `(message (concat "[EXWM] " ,format-string) ,@args)))

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

;; Internal variables
(defvar-local exwm--id nil)                        ;window ID
(defvar-local exwm--frame nil)                     ;workspace frame
(defvar-local exwm--floating-frame nil)            ;floating frame
(defvar-local exwm--floating-edges nil)            ;four edges
(defvar-local exwm--floating-mode-line-format nil) ;save mode-line-format
(defvar-local exwm--fullscreen nil)                ;used in fullscreen
(defvar-local exwm--floating-frame-geometry nil)   ;in fullscreen
(defvar-local exwm--fixed-size nil)                ;fixed size
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
(defvar-local exwm-state nil "WM_STATE.")
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
(defvar-local exwm--hints-input nil)    ;FIXME
(defvar-local exwm--hints-urgency nil)
;; _MOTIF_WM_HINTS
(defvar-local exwm--mwm-hints nil)

(defvar exwm-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-ck" #'exwm-input-release-keyboard)
    (define-key map "\C-cf" #'exwm-layout-set-fullscreen)
    (define-key map "\C-cm" #'exwm-floating-toggle-floating)
    (define-key map "\C-cq" #'exwm-input-send-next-key)
    (define-key map "\C-cv" #'exwm-workspace-move-window)
    map)
  "Keymap for `exwm-mode'.")

(define-derived-mode exwm-mode nil "EXWM"
  "Major mode for managing X windows.

\\{exwm-mode-map}"
  ;;
  (setq mode-name
        '(:eval (propertize "EXWM" 'face
                            (when (cl-some (lambda (i)
                                             (frame-parameter i
                                                              'exwm--urgency))
                                           exwm-workspace--list)
                              'font-lock-warning-face))))
  ;; Change major-mode is not allowed
  (add-hook 'change-major-mode-hook #'kill-buffer nil t)
  ;; Kill buffer -> close window
  (add-hook 'kill-buffer-query-functions
            (lambda ()
              (exwm-manage--close-window exwm--id (current-buffer))
              nil)
            nil t)
  (setq buffer-read-only t
        left-margin-width nil
        right-margin-width nil
        left-fringe-width 0
        right-fringe-width 0
        vertical-scroll-bar nil))



(provide 'exwm-core)

;;; exwm-core.el ends here
