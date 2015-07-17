;;; exwm-workspace.el --- Workspace Module for EXWM  -*- lexical-binding: t -*-

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

;; This module adds workspace support for EXWM.

;; Todo:
;; + prevent from deleting frames of Emacs client (`frame-delete-functions')

;;; Code:

(defvar exwm-workspace-number 4 "Number of workspaces (1 ~ 10).")
(defvar exwm-workspace--list nil "List of all workspaces (Emacs frames).")
(defvar exwm-workspace--switch-map
  (let ((map (make-sparse-keymap)))
    (define-key map [t] (lambda () (interactive)))
    (dotimes (i 10)
      (define-key map (int-to-string i)
        `(lambda ()
           (interactive)
           (when (< ,i exwm-workspace-number)
             (goto-history-element ,(1+ i))
             (exit-minibuffer)))))
    (define-key map "\C-a" (lambda () (interactive) (goto-history-element 1)))
    (define-key map "\C-e" (lambda ()
                             (interactive)
                             (goto-history-element exwm-workspace-number)))
    (define-key map "\C-g" 'abort-recursive-edit)
    (define-key map "\C-]" 'abort-recursive-edit)
    (define-key map "\C-j" 'exit-minibuffer)
    ;; (define-key map "\C-m" 'exit-minibuffer) ;not working
    (define-key map [return] 'exit-minibuffer)
    (define-key map " " 'exit-minibuffer)
    (define-key map "\C-f" 'previous-history-element)
    (define-key map "\C-b" 'next-history-element)
    ;; Alternative keys
    (define-key map [right] 'previous-history-element)
    (define-key map [left] 'next-history-element)
    map)
  "Keymap used for interactively switch workspace.")

(defvar exwm-workspace--switch-history nil
  "History for `read-from-minibuffer' to interactively switch workspace.")

(defun exwm-workspace--update-switch-history ()
  "Update the history for switching workspace to reflect the latest status."
  (let ((sequence (number-sequence 0 (1- exwm-workspace-number)))
        (not-empty (make-vector exwm-workspace-number nil)))
    (dolist (i exwm--id-buffer-alist)
      (with-current-buffer (cdr i)
        (setf (elt not-empty (cl-position exwm--frame exwm-workspace--list))
              t)))
    (setq exwm-workspace--switch-history
          (mapcar
           (lambda (i)
             (mapconcat
              (lambda (j)
                (format (if (= i j) "[%s]" " %s ")
                        (propertize
                         (int-to-string j)
                         'face
                         (cond ((frame-parameter (elt exwm-workspace--list j)
                                                 'exwm--urgency)
                                '(:foreground "orange"))
                               ((elt not-empty j) '(:foreground "green"))
                               (t nil)))))
              sequence ""))
           sequence))))

(defvar exwm-workspace--current nil "Current active workspace.")
(defvar exwm-workspace-current-index 0 "Index of current active workspace.")

(defun exwm-workspace-switch (index &optional force)
  "Switch to workspace INDEX. Query for INDEX if it's not specified.

The optional FORCE option is for internal use only "
  (interactive
   (list
    (let* ((history-add-new-input nil)  ;prevent modifying history
           (idx (read-from-minibuffer
                 "Workspace: " (elt exwm-workspace--switch-history
                                    exwm-workspace-current-index)
                 exwm-workspace--switch-map nil
                 `(exwm-workspace--switch-history
                   . ,(1+ exwm-workspace-current-index)))))
      (cl-position idx exwm-workspace--switch-history :test 'equal))))
  (unless (and (<= 0 index) (< index exwm-workspace-number))
    (user-error "[EXWM] Workspace index out of range: %d" index))
  (when (or force (/= exwm-workspace-current-index index))
    (select-frame-set-input-focus (elt exwm-workspace--list index))
    ;; Hide all workspaces but the selected one
    (dotimes (i exwm-workspace-number)
      (unless (= i index) (make-frame-invisible (elt exwm-workspace--list i))))
    (setq exwm-workspace--current (elt exwm-workspace--list index)
          exwm-workspace-current-index index)
    (setq default-minibuffer-frame (selected-frame))
    ;; Hide windows in other workspaces by preprending a space
    (dolist (i exwm--id-buffer-alist)
      (with-current-buffer (cdr i)
        (let ((name (replace-regexp-in-string "^\\s-*" "" (buffer-name))))
          (rename-buffer (if (eq (selected-frame) exwm--frame)
                             name
                           (concat " " name))))))
    ;; Update demands attention flag
    (set-frame-parameter (selected-frame) 'exwm--urgency nil)
    ;; Update switch workspace history
    (exwm-workspace--update-switch-history)
    ;; Update _NET_CURRENT_DESKTOP
    (xcb:+request exwm--connection
        (make-instance 'xcb:ewmh:set-_NET_CURRENT_DESKTOP
                       :window exwm--root :data index))
    (xcb:flush exwm--connection)))

(defun exwm-workspace-move-window (index &optional id)
  "Move window ID to workspace INDEX."
  (interactive
   (list
    (let* ((history-add-new-input nil)  ;prevent modifying history
           (idx (read-from-minibuffer
                 "Workspace: " (elt exwm-workspace--switch-history
                                    exwm-workspace-current-index)
                 exwm-workspace--switch-map nil
                 `(exwm-workspace--switch-history
                   . ,(1+ exwm-workspace-current-index)))))
      (cl-position idx exwm-workspace--switch-history :test 'equal))))
  (unless id (setq id (exwm--buffer->id (window-buffer))))
  (unless (and (<= 0 index) (< index exwm-workspace-number))
    (user-error "[EXWM] Workspace index out of range: %d" index))
  (when (/= exwm-workspace-current-index index)
    (set-window-buffer (get-buffer-window (exwm--id->buffer id))
                       (other-buffer))
    (let ((frame (elt exwm-workspace--list index)))
      (with-current-buffer (exwm--id->buffer id)
        (setq exwm--frame frame)
        (rename-buffer
         (concat " " (replace-regexp-in-string "^\\s-*" "" (buffer-name))))
        (if exwm--floating-frame
            ;; Move the floating frame is enough
            (xcb:+request exwm--connection
            (make-instance 'xcb:ReparentWindow
                           :window (frame-parameter exwm--floating-frame
                                                    'exwm-outer-id)
                           :parent (frame-parameter frame 'exwm-window-id)
                           :x 0 :y 0))
          ;; Move the window itself
          (xcb:+request exwm--connection
              (make-instance 'xcb:ChangeWindowAttributes
                             :window id :value-mask xcb:CW:EventMask
                             :event-mask xcb:EventMask:NoEvent))
          (xcb:+request exwm--connection
              (make-instance 'xcb:ReparentWindow
                             :window id
                             :parent (frame-parameter frame 'exwm-window-id)
                             :x 0 :y 0))
          (xcb:+request exwm--connection
              (make-instance 'xcb:ChangeWindowAttributes
                             :window id :value-mask xcb:CW:EventMask
                             :event-mask exwm--client-event-mask)))))
    (xcb:flush exwm--connection)
    (exwm-workspace--update-switch-history)))

(defun exwm-workspace--init ()
  "Initialize workspace module."
  (cl-assert (and (< 0 exwm-workspace-number) (>= 10 exwm-workspace-number)))
  ;; Prevent unexpected exit
  (setq confirm-kill-emacs
        (lambda (prompt)
          (pcase (length exwm--id-buffer-alist)
            (0 (y-or-n-p prompt))
            (x (yes-or-no-p (format "[EXWM] %d window%s currently alive. %s"
                                    x (if (= x 1) "" "s") prompt))))))
  ;; Initialize workspaces
  (setq exwm-workspace--list (frame-list))
  (when (< 1 (length exwm-workspace--list))
    ;; Emacs client creates an extra (but unusable) frame
    (dolist (i exwm-workspace--list)
      (unless (frame-parameter i 'window-id)
        (setq exwm-workspace--list (delq i exwm-workspace--list)))))
  (cl-assert (= 1 (length exwm-workspace--list)))
  ;; Configure the existing frame
  (set-frame-parameter (car exwm-workspace--list) 'fullscreen 'fullboth)
  ;; Create remaining frames
  (dotimes (i (1- exwm-workspace-number))
    (nconc exwm-workspace--list
           (list (make-frame '((window-system . x) (fullscreen . fullboth))))))
  ;; Configure workspaces
  (dolist (i exwm-workspace--list)
    (let ((window-id (string-to-int (frame-parameter i 'window-id)))
          (outer-id (string-to-int (frame-parameter i 'outer-window-id))))
      ;; Save window IDs
      (set-frame-parameter i 'exwm-window-id window-id)
      (set-frame-parameter i 'exwm-outer-id outer-id)
      ;; Set OverrideRedirect on all frames
      (xcb:+request exwm--connection
          (make-instance 'xcb:ChangeWindowAttributes
                         :window outer-id :value-mask xcb:CW:OverrideRedirect
                         :override-redirect 1))
      ;; Select events on all virtual roots
      (xcb:+request exwm--connection
          (make-instance 'xcb:ChangeWindowAttributes
                         :window window-id :value-mask xcb:CW:EventMask
                         :event-mask xcb:EventMask:SubstructureRedirect))))
  ;; Switch to the first workspace
  (exwm-workspace-switch 0 t))



(provide 'exwm-workspace)

;;; exwm-workspace.el ends here
