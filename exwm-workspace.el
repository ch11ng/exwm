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

;;; Code:

(defvar exwm-workspace--list nil "List of all workspaces (Emacs frames).")

(defun exwm-workspace--count ()
  "Retrieve total number of workspaces."
  (length exwm-workspace--list))

(defvar exwm-workspace--switch-map
  (let ((map (make-sparse-keymap)))
    (define-key map [t] (lambda () (interactive)))
    (dotimes (i 10)
      (define-key map (int-to-string i) #'exwm-workspace--switch-map-by-number))
    (define-key map "\C-a" (lambda () (interactive) (goto-history-element 1)))
    (define-key map "\C-e" (lambda ()
                             (interactive)
                             (goto-history-element (exwm-workspace--count))))
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
  (let* ((num (exwm-workspace--count))
         (sequence (number-sequence 0 (1- num)))
         (not-empty (make-vector num nil)))
    (dolist (i exwm--id-buffer-alist)
      (with-current-buffer (cdr i)
        (when exwm--frame
          (setf (elt not-empty (cl-position exwm--frame exwm-workspace--list))
                t))))
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

(defun exwm-workspace--switch-map-by-number (&optional prefix-digits)
  "Allow selecting a workspace by number.

PREFIX-DIGITS is a list of the digits introduced so far."
  (interactive)
  (let* ((ev (this-command-keys-vector))
         (off (1- (length ev)))
         (k (elt ev off))
         ;; 0 is ASCII 48.
         (d (- k 48))
         ;; Convert prefix-digits to number.  For example, '(2 1) to 120.
         (o 1)
         (pn (apply #'+ (mapcar (lambda (x)
                                  (setq o (* 10 o))
                                  (* o x))
                                prefix-digits)))
         (n (+ pn d))
         (num-workspaces (exwm-workspace--count)))
    (if (= (length prefix-digits)           ; Go ahead if there are enough
           (floor (log num-workspaces 10))) ; digits to select any workspace.
        (exwm-workspace--switch-map-select-number n)
      (set-transient-map
       (let ((map (make-sparse-keymap))
             (cmd `(lambda ()
                     (interactive)
                     (exwm-workspace--switch-map-by-number ',(cons d prefix-digits))
                     )))
         (dotimes (i 10)
           (define-key map (int-to-string i) cmd))
         ;; Accept
         (define-key map [return]
           `(lambda ()
              (interactive)
              (exwm-workspace--switch-map-select-number ,n)))
         map)))))

(defun exwm-workspace--switch-map-select-number (n)
  "Select Nth workspace."
  (interactive)
  (goto-history-element (1+ n))
  (exit-minibuffer))

(defun exwm-workspace-switch (index &optional force)
  "Switch to workspace INDEX. Query for INDEX if it's not specified.

The optional FORCE option is for internal use only."
  (interactive
   (list
    (unless (and (eq major-mode 'exwm-mode) exwm--fullscreen) ;it's invisible
      (let* ((history-add-new-input nil)  ;prevent modifying history
             (idx (read-from-minibuffer
                   "Workspace: " (elt exwm-workspace--switch-history
                                      exwm-workspace-current-index)
                   exwm-workspace--switch-map nil
                   `(exwm-workspace--switch-history
                     . ,(1+ exwm-workspace-current-index)))))
        (cl-position idx exwm-workspace--switch-history :test 'equal)))))
  (when index
    (unless (and (<= 0 index) (< index (exwm-workspace--count)))
      (user-error "[EXWM] Workspace index out of range: %d" index))
    (when (or force (/= exwm-workspace-current-index index))
      (let ((frame (elt exwm-workspace--list index)))
        (setq exwm-workspace--current frame
              exwm-workspace-current-index index)
        (select-frame-set-input-focus frame)
        ;; Move mouse when necessary
        (let ((position (mouse-pixel-position))
              x y w h)
          (unless (eq frame (car position))
            (setq x (cadr position)
                  y (cddr position)
                  w (frame-pixel-width frame)
                  h (frame-pixel-height frame))
            (when (or (> x w) (> y h))
              (setq x (/ w 2)
                    y (/ h 2)))
            (set-mouse-pixel-position frame x y)))
        (setq default-minibuffer-frame frame)
        ;; Hide windows in other workspaces by preprending a space
        (dolist (i exwm--id-buffer-alist)
          (with-current-buffer (cdr i)
            (let ((name (replace-regexp-in-string "^\\s-*" "" (buffer-name))))
              (exwm-workspace-rename-buffer (if (eq frame exwm--frame)
                                                name
                                              (concat " " name))))))
        ;; Update demands attention flag
        (set-frame-parameter frame 'exwm--urgency nil)
        ;; Update switch workspace history
        (exwm-workspace--update-switch-history)
        (exwm--make-emacs-idle-for 0.1) ;FIXME
        ;; Update _NET_CURRENT_DESKTOP
        (xcb:+request exwm--connection
            (make-instance 'xcb:ewmh:set-_NET_CURRENT_DESKTOP
                           :window exwm--root :data index))
        (xcb:flush exwm--connection)))))

(defun exwm-workspace--on-focus-in ()
  "Fix unexpected frame switch."
  (let ((index (cl-position (selected-frame) exwm-workspace--list)))
    (exwm--log "Focus on workspace %s" index)
    (when (and index (/= index exwm-workspace-current-index))
      (exwm--log "Workspace was switched unexpectedly")
      (exwm-workspace-switch index))))

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
  (unless (and (<= 0 index) (< index (exwm-workspace--count)))
    (user-error "[EXWM] Workspace index out of range: %d" index))
  (with-current-buffer (exwm--id->buffer id)
    (let ((frame (elt exwm-workspace--list index)))
      (when (not (equal exwm--frame frame))
        (let ((name (replace-regexp-in-string "^\\s-*" "" (buffer-name))))
          (exwm-workspace-rename-buffer (if (= index exwm-workspace-current-index)
                                            name
                                          (concat " " name))))
        (setq exwm--frame frame)
        (if exwm--floating-frame
            ;; Move the floating frame is enough
            (progn
              (xcb:+request exwm--connection
                  (make-instance 'xcb:ReparentWindow
                                 :window (frame-parameter exwm--floating-frame
                                                          'exwm-outer-id)
                                 :parent (frame-parameter frame
                                                          'exwm-window-id)
                                 :x 0 :y 0))
              (xcb:flush exwm--connection))
          ;; Move the window itself
          (bury-buffer)
          (exwm-layout--hide id)
          (xcb:+request exwm--connection
              (make-instance 'xcb:ReparentWindow
                             :window id
                             :parent (frame-parameter frame 'exwm-window-id)
                             :x 0 :y 0))
          (xcb:flush exwm--connection)
          (set-window-buffer (frame-first-window frame)
                             (exwm--id->buffer id)))))
    (exwm-workspace--update-switch-history)))

(defun exwm-workspace-rename-buffer (newname)
  "Rename a buffer."
  (let ((hidden (= ?\s (aref newname 0)))
        (basename (replace-regexp-in-string "<[0-9]+>$" "" newname))
        (counter 1)
        tmp)
    (when hidden (setq basename (substring basename 1)))
    (setq newname basename)
    (while (and (setq tmp (or (get-buffer newname)
                              (get-buffer (concat " " newname))))
                (not (eq tmp (current-buffer))))
      (setq newname (format "%s<%d>" basename (cl-incf counter))))
    (rename-buffer (concat (and hidden " ") newname))))

(defun exwm-workspace--add-frame-as-workspace (frame)
  "Configure frame FRAME to be treated as a workspace."
  (cond
   ((memq frame exwm-workspace--list)
    (exwm--log "Frame is already a workspace: %s" frame))
   (t
    (exwm--log "Adding workspace: %s" frame)
    (setq exwm-workspace--list (nconc exwm-workspace--list (list frame)))
    (let ((window-id (string-to-number (frame-parameter frame 'window-id)))
          (outer-id (string-to-number (frame-parameter frame 'outer-window-id))))
      ;; Save window IDs
      (set-frame-parameter frame 'exwm-window-id window-id)
      (set-frame-parameter frame 'exwm-outer-id outer-id)
      ;; Set OverrideRedirect on all frames
      (xcb:+request exwm--connection
          (make-instance 'xcb:ChangeWindowAttributes
                         :window outer-id :value-mask xcb:CW:OverrideRedirect
                         :override-redirect 1))
      ;; Select events on all virtual roots
      (xcb:+request exwm--connection
          (make-instance 'xcb:ChangeWindowAttributes
                         :window window-id :value-mask xcb:CW:EventMask
                         :event-mask xcb:EventMask:SubstructureRedirect)))
    (xcb:flush exwm--connection)
    ;; We have to delay making the frame visible until the
    ;; override-redirect flag has been set.
    (set-frame-parameter frame 'visibility t)
    (lower-frame frame)
    (set-frame-parameter frame 'fullscreen 'fullboth)
    ;; Update EWMH properties.
    (exwm-workspace--update-ewmh-props)
    ;; Update switch history.
    (exwm-workspace--update-switch-history))))

(defun exwm-workspace--remove-frame-as-workspace (frame)
  "Stop treating frame FRAME as a workspace."
  (cond
   ((= 1 (exwm-workspace--count))
    (exwm--log "Cannot remove last workspace"))
   ((not (memq frame exwm-workspace--list))
    (exwm--log "Frame is not a workspace: %s" frame))
   (t
    (exwm--log "Removing workspace: %s" frame)
    ;; When removing current workspace (common case), switch to the one that
    ;; will receive clients on the removed one.  Do this before removing the
    ;; frame from the workspace list, so as not to confuse the indices.
    (let* ((index (cl-position frame exwm-workspace--list))
           (lastp (= index (1- (length exwm-workspace--list)))))
      (when (= index exwm-workspace-current-index)
        ;; As we are removing this workspace, the one on its left is its natural
        ;; substitutes... except when this is already the last one and there is
        ;; none on its left.
        (exwm-workspace-switch (+ index (if lastp -1 +1)))
      ;; Now delete it from the workspace list, and update current index.
      (setq exwm-workspace--list (delete frame exwm-workspace--list))
      (cl-incf exwm-workspace-current-index (if lastp 0 -1))))
    ;; Update EWMH properties.
    (exwm-workspace--update-ewmh-props)
    ;; Update switch history.
    (exwm-workspace--update-switch-history))))

(defun exwm-workspace--update-ewmh-props ()
  "Update EWMH properties to match the workspace list."
  ;; Set _NET_VIRTUAL_ROOTS
  (let ((num-workspaces (exwm-workspace--count)))
    (xcb:+request exwm--connection
        (make-instance 'xcb:ewmh:set-_NET_VIRTUAL_ROOTS
                       :window exwm--root
                       :data (vconcat (mapcar
                                       (lambda (i)
                                         (frame-parameter i 'exwm-window-id))
                                       exwm-workspace--list))))
    ;; Set _NET_NUMBER_OF_DESKTOPS
    (xcb:+request exwm--connection
        (make-instance 'xcb:ewmh:set-_NET_NUMBER_OF_DESKTOPS
                       :window exwm--root
                       :data num-workspaces))
    ;; Set _NET_DESKTOP_VIEWPORT
    (xcb:+request exwm--connection
        (make-instance 'xcb:ewmh:set-_NET_DESKTOP_VIEWPORT
                       :window exwm--root
                       :data (make-vector (* 2 num-workspaces) 0)))

    ;; Set _NET_WORKAREA (with minibuffer and bottom mode-line excluded)
    (let* ((workareas
            (vector 0 0 (x-display-pixel-width) (x-display-pixel-height)))
           (workareas (mapconcat (lambda (i) workareas)
                                 (make-list num-workspaces 0) [])))
      (xcb:+request exwm--connection
          (make-instance 'xcb:ewmh:set-_NET_WORKAREA
                         :window exwm--root
                         :data workareas)))
    (xcb:flush exwm--connection)))

(defun exwm-workspace--init ()
  "Initialize workspace module."
  ;; Prevent unexpected exit
  (setq confirm-kill-emacs
        (lambda (prompt)
          (pcase (length exwm--id-buffer-alist)
            (0 (y-or-n-p prompt))
            (x (yes-or-no-p (format "[EXWM] %d window%s currently alive. %s"
                                    x (if (= x 1) "" "s") prompt))))))
  ;; Initialize workspaces
  (let ((initial-workspaces (frame-list)))
    (when (< 1 (length initial-workspaces))
      ;; Emacs client creates an extra (but unusable) frame
      (dolist (i initial-workspaces)
        (unless (frame-parameter i 'window-id)
          (setq initial-workspaces (delq i initial-workspaces))))
      (cl-assert (= 1 (exwm-workspace--count)))
      ;; Prevent user from deleting this frame by accident
      (set-frame-parameter (car initial-workspaces) 'client nil))
    ;; TODO: this prevents user having a creating initial workspaces by making
    ;;       frames in their configuration before launching EXWM.
    (cl-assert (= 1 (length initial-workspaces)))
    ;; Configure workspaces
    (dolist (i initial-workspaces)
      (exwm-workspace--add-frame-as-workspace i))
    (select-frame-set-input-focus (car initial-workspaces)))
  ;; Handle unexpected frame switch
  (add-hook 'focus-in-hook 'exwm-workspace--on-focus-in)
  ;; Make new frames create new workspaces.
  (setq window-system-default-frame-alist '((x . ((visibility . nil)))))
  (add-hook 'after-make-frame-functions #'exwm-workspace--add-frame-as-workspace)
  (add-hook 'delete-frame-functions #'exwm-workspace--remove-frame-as-workspace)
  ;; Switch to the first workspace
  (exwm-workspace-switch 0 t))



(provide 'exwm-workspace)

;;; exwm-workspace.el ends here
