;;; exwm-workspace.el --- Workspace Module for EXWM  -*- lexical-binding: t -*-

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

;; This module adds workspace support for EXWM.

;; Todo:
;; + Add system tray support.

;;; Code:

(require 'exwm-core)

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
    (define-key map "\C-g" #'abort-recursive-edit)
    (define-key map "\C-]" #'abort-recursive-edit)
    (define-key map "\C-j" #'exit-minibuffer)
    ;; (define-key map "\C-m" #'exit-minibuffer) ;not working
    (define-key map [return] #'exit-minibuffer)
    (define-key map " " #'exit-minibuffer)
    (define-key map "\C-f" #'previous-history-element)
    (define-key map "\C-b" #'next-history-element)
    ;; Alternative keys
    (define-key map [right] #'previous-history-element)
    (define-key map [left] #'next-history-element)
    map)
  "Keymap used for interactively switch workspace.")

(defvar exwm-workspace--switch-history nil
  "History for `read-from-minibuffer' to interactively switch workspace.")
(defvar exwm-workspace--switch-history-outdated nil
  "Non-nil to indicate `exwm-workspace--switch-history' is outdated.")

;;;###autoload
(defun exwm-workspace--update-switch-history ()
  "Update the history for switching workspace to reflect the latest status."
  (when exwm-workspace--switch-history-outdated
    (setq exwm-workspace--switch-history-outdated nil)
    (let ((sequence (number-sequence 0 (1- exwm-workspace-number)))
          (not-empty (make-vector exwm-workspace-number nil)))
      (dolist (i exwm--id-buffer-alist)
        (with-current-buffer (cdr i)
          (when exwm--frame
            (setf (aref not-empty
                        (cl-position exwm--frame exwm-workspace--list))
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
                                 ((aref not-empty j) '(:foreground "green"))
                                 (t nil)))))
                sequence ""))
             sequence)))))

(defvar exwm-workspace--current nil "Current active workspace.")
(defvar exwm-workspace-current-index 0 "Index of current active workspace.")
(defvar exwm-workspace-show-all-buffers nil
  "Non-nil to show buffers on other workspaces.")
(defvar exwm-workspace--minibuffer nil
  "The minibuffer frame shared among all frames.")
(defvar exwm-workspace-minibuffer-position nil
  "Position of the minibuffer frame.

Value nil means to use the default position which is fixed at bottom, while
'top and 'bottom mean to use an auto-hiding minibuffer.")
(defvar exwm-workspace-display-echo-area-timeout 1
  "Timeout for displaying echo area.")
(defvar exwm-workspace--display-echo-area-timer nil
  "Timer for auto-hiding echo area.")

;;;###autoload
(defun exwm-workspace--minibuffer-own-frame-p ()
  "Reports whether the minibuffer is displayed in its own frame."
  (memq exwm-workspace-minibuffer-position '(top bottom)))

;;;###autoload
(defun exwm-workspace--resize-minibuffer-frame (&optional width height)
  "Resize minibuffer (and its container) to fit the size of workspace.

If WIDTH and HEIGHT of the workspace is not specified, they're get from the
workspace frame."
  (cl-assert (exwm-workspace--minibuffer-own-frame-p))
  (let ((y (if (eq exwm-workspace-minibuffer-position 'top)
               0
             (- (or height (frame-pixel-height exwm-workspace--current))
                (frame-pixel-height exwm-workspace--minibuffer))))
        (width (or width (frame-pixel-width exwm-workspace--current)))
        (container (frame-parameter exwm-workspace--minibuffer
                                    'exwm-container)))
    (xcb:+request exwm--connection
        (make-instance 'xcb:ConfigureWindow
                       :window container
                       :value-mask (logior xcb:ConfigWindow:Y
                                           xcb:ConfigWindow:Width
                                           xcb:ConfigWindow:StackMode)
                       :y y
                       :width width
                       :stack-mode xcb:StackMode:Above))
    (set-frame-width exwm-workspace--minibuffer width nil t)))

;;;###autoload
(defun exwm-workspace-switch (index &optional force)
  "Switch to workspace INDEX. Query for INDEX if it's not specified.

The optional FORCE option is for internal use only."
  (interactive
   (list
    (unless (and (eq major-mode 'exwm-mode) exwm--fullscreen) ;it's invisible
      (exwm-workspace--update-switch-history)
      (let* ((history-add-new-input nil) ;prevent modifying history
             (idx (read-from-minibuffer
                   "Workspace: " (elt exwm-workspace--switch-history
                                      exwm-workspace-current-index)
                   exwm-workspace--switch-map nil
                   `(exwm-workspace--switch-history
                     . ,(1+ exwm-workspace-current-index)))))
        (cl-position idx exwm-workspace--switch-history :test #'equal)))))
  (when index
    (unless (and (<= 0 index) (< index exwm-workspace-number))
      (user-error "[EXWM] Workspace index out of range: %d" index))
    (when (or force (/= exwm-workspace-current-index index))
      (let* ((frame (elt exwm-workspace--list index))
             (workspace (frame-parameter frame 'exwm-workspace)))
        (xcb:+request exwm--connection
            (make-instance 'xcb:ConfigureWindow
                           :window workspace
                           :value-mask xcb:ConfigWindow:StackMode
                           :stack-mode xcb:StackMode:Above))
        (setq exwm-workspace--current frame
              exwm-workspace-current-index index)
        (select-window (frame-selected-window frame))
        ;; Close the (possible) active minibuffer
        (when (active-minibuffer-window)
          (run-with-idle-timer 0 nil (lambda () (abort-recursive-edit))))
        (if (not (exwm-workspace--minibuffer-own-frame-p))
            (setq default-minibuffer-frame frame)
          ;; Resize/reposition the minibuffer frame
          (xcb:+request exwm--connection
              (make-instance 'xcb:ReparentWindow
                             :window
                             (frame-parameter exwm-workspace--minibuffer
                                              'exwm-container)
                             :parent (frame-parameter frame 'exwm-workspace)
                             :x 0 :y 0))
          (exwm-workspace--resize-minibuffer-frame))
        ;; Hide windows in other workspaces by preprending a space
        (unless exwm-workspace-show-all-buffers
          (dolist (i exwm--id-buffer-alist)
            (with-current-buffer (cdr i)
              (let ((name (replace-regexp-in-string "^\\s-*" ""
                                                    (buffer-name))))
                (exwm-workspace-rename-buffer (if (eq frame exwm--frame)
                                                  name
                                                (concat " " name)))))))
        ;; Update demands attention flag
        (set-frame-parameter frame 'exwm--urgency nil)
        ;; Update switch workspace history
        (setq exwm-workspace--switch-history-outdated t)
        ;; Update _NET_CURRENT_DESKTOP
        (xcb:+request exwm--connection
            (make-instance 'xcb:ewmh:set-_NET_CURRENT_DESKTOP
                           :window exwm--root :data index))
        (xcb:flush exwm--connection)))))

;;;###autoload
(defun exwm-workspace-move-window (index &optional id)
  "Move window ID to workspace INDEX."
  (interactive
   (list
    (progn
      (exwm-workspace--update-switch-history)
      (let* ((history-add-new-input nil)  ;prevent modifying history
             (idx (read-from-minibuffer
                   "Workspace: " (elt exwm-workspace--switch-history
                                      exwm-workspace-current-index)
                   exwm-workspace--switch-map nil
                   `(exwm-workspace--switch-history
                     . ,(1+ exwm-workspace-current-index)))))
        (cl-position idx exwm-workspace--switch-history :test #'equal)))))
  (unless id (setq id (exwm--buffer->id (window-buffer))))
  (unless (and (<= 0 index) (< index exwm-workspace-number))
    (user-error "[EXWM] Workspace index out of range: %d" index))
  (with-current-buffer (exwm--id->buffer id)
    (let ((frame (elt exwm-workspace--list index)))
      (unless (eq exwm--frame frame)
        (unless exwm-workspace-show-all-buffers
          (let ((name (replace-regexp-in-string "^\\s-*" "" (buffer-name))))
            (exwm-workspace-rename-buffer
             (if (= index exwm-workspace-current-index)
                 name
               (concat " " name)))))
        (setq exwm--frame frame)
        (if exwm--floating-frame
            ;; Move the floating frame is enough
            (progn
              (xcb:+request exwm--connection
                  (make-instance 'xcb:ReparentWindow
                                 :window exwm--container
                                 :parent
                                 (frame-parameter frame 'exwm-workspace)
                                 :x 0 :y 0))
              (xcb:flush exwm--connection))
          ;; Move the window itself
          (if (/= index exwm-workspace-current-index)
              (bury-buffer)
            (set-window-buffer (get-buffer-window (current-buffer) t)
                               (or (get-buffer "*scratch*")
                                   (progn
                                     (set-buffer-major-mode
                                      (get-buffer-create "*scratch*"))
                                     (get-buffer "*scratch*")))))
          (exwm-layout--hide id)
          (xcb:+request exwm--connection
              (make-instance 'xcb:ReparentWindow
                             ;; (current-buffer) is changed.
                             :window (with-current-buffer (exwm--id->buffer id)
                                       exwm--container)
                             :parent (frame-parameter frame 'exwm-workspace)
                             :x 0 :y 0))
          (xcb:flush exwm--connection)
          (set-window-buffer (frame-selected-window frame)
                             (exwm--id->buffer id)))))
    (setq exwm-workspace--switch-history-outdated t)))

(defun exwm-workspace-switch-to-buffer ()
  "Make the current Emacs window display another buffer."
  (interactive)
  ;; Show all buffers
  (unless exwm-workspace-show-all-buffers
    (dolist (pair exwm--id-buffer-alist)
      (with-current-buffer (cdr pair)
        (when (= ?\s (aref (buffer-name) 0))
          (rename-buffer (substring (buffer-name) 1))))))
  (let ((buffer (read-buffer "Switch to buffer: " nil t)))
    (when buffer
      (with-current-buffer buffer
        (if (and (eq major-mode 'exwm-mode)
                 (not (eq exwm--frame exwm-workspace--current)))
            (exwm-workspace-move-window exwm-workspace-current-index
                                        exwm--id)
          (switch-to-buffer buffer)))))
  ;; Hide buffers on other workspaces
  (unless exwm-workspace-show-all-buffers
    (dolist (pair exwm--id-buffer-alist)
      (with-current-buffer (cdr pair)
        (unless (or (eq exwm--frame exwm-workspace--current)
                    (= ?\s (aref (buffer-name) 0)))
          (rename-buffer (concat " " (buffer-name))))))))

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

(defun exwm-workspace--x-create-frame (orig-fun params)
  "Set override-redirect on the frame created by `x-create-frame'."
  (let ((frame (funcall orig-fun params)))
    (xcb:+request exwm--connection
        (make-instance 'xcb:ChangeWindowAttributes
                       :window (string-to-number
                                (frame-parameter frame 'outer-window-id))
                       :value-mask xcb:CW:OverrideRedirect
                       :override-redirect 1))
    (xcb:flush exwm--connection)
    frame))

(defun exwm-workspace--update-minibuffer (&optional echo-area)
  "Update the minibuffer frame."
  (let ((height
         (with-current-buffer
             (window-buffer (minibuffer-window exwm-workspace--minibuffer))
           (max 1
                (if echo-area
                    (let ((width (frame-width exwm-workspace--minibuffer))
                          (result 0))
                      (mapc (lambda (i)
                              (setq result
                                    (+ result
                                       (ceiling (1+ (length i)) width))))
                            (split-string (current-message) "\n"))
                      result)
                  (count-screen-lines))))))
    (when (and (integerp max-mini-window-height)
               (> height max-mini-window-height))
      (setq height max-mini-window-height))
    (set-frame-height exwm-workspace--minibuffer height)))

(defun exwm-workspace--on-ConfigureNotify (data _synthetic)
  "Adjust the container to fit the minibuffer frame."
  (let ((obj (make-instance 'xcb:ConfigureNotify))
        value-mask y)
    (xcb:unmarshal obj data)
    (with-slots (window height) obj
      (when (eq (frame-parameter exwm-workspace--minibuffer 'exwm-outer-id)
                window)
        (when (and (floatp max-mini-window-height)
                   (> height (* max-mini-window-height
                                (frame-pixel-height exwm-workspace--current))))
          (setq height (floor
                        (* max-mini-window-height
                           (frame-pixel-height exwm-workspace--current))))
          (xcb:+request exwm--connection
              (make-instance 'xcb:ConfigureWindow
                             :window window
                             :value-mask xcb:ConfigWindow:Height
                             :height height)))
        (if (eq exwm-workspace-minibuffer-position 'top)
            (setq value-mask xcb:ConfigWindow:Height
                  y 0)
          (setq value-mask (logior xcb:ConfigWindow:Y xcb:ConfigWindow:Height)
                y (- (frame-pixel-height exwm-workspace--current) height)))
        (xcb:+request exwm--connection
            (make-instance 'xcb:ConfigureWindow
                           :window (frame-parameter exwm-workspace--minibuffer
                                                    'exwm-container)
                           :value-mask value-mask
                           :y y
                           :height height))
        (xcb:flush exwm--connection)))))

(defun exwm-workspace--display-buffer (buffer alist)
  "Display buffer in the current workspace frame.

This functions is modified from `display-buffer-reuse-window' and
`display-buffer-pop-up-window'."
  (when (eq (selected-frame) exwm-workspace--minibuffer)
    (setq buffer (get-buffer buffer))   ;convert string to buffer.
    (let (window)
      (if (setq window (get-buffer-window buffer exwm-workspace--current))
          (window--display-buffer buffer window 'reuse alist)
        (when (setq window (or (window--try-to-split-window
                                (get-largest-window exwm-workspace--current t)
                                alist)
                               (window--try-to-split-window
                                (get-lru-window exwm-workspace--current t)
                                alist)))
          (window--display-buffer
           buffer window 'window alist display-buffer-mark-dedicated))))))

(defun exwm-workspace--show-minibuffer ()
  "Show the minibuffer frame."
  ;; Cancel pending timer.
  (when exwm-workspace--display-echo-area-timer
    (cancel-timer exwm-workspace--display-echo-area-timer)
    (setq exwm-workspace--display-echo-area-timer nil))
  ;; Show the minibuffer frame.
  (xcb:+request exwm--connection
      (make-instance 'xcb:MapWindow
                     :window (frame-parameter exwm-workspace--minibuffer
                                              'exwm-container)))
  (xcb:flush exwm--connection)
  ;; Unfortunately we need the following lines to workaround a cursor
  ;; flickering issue for line-mode floating X windows.  They just make the
  ;; minibuffer appear to be focused.
  (with-current-buffer (window-buffer (minibuffer-window
                                       exwm-workspace--minibuffer))
    (setq cursor-in-non-selected-windows
          (frame-parameter exwm-workspace--minibuffer 'cursor-type))))

(defun exwm-workspace--hide-minibuffer ()
  "Hide the minibuffer frame."
  ;; Hide the minibuffer frame.
  (xcb:+request exwm--connection
      (make-instance 'xcb:UnmapWindow
                     :window (frame-parameter exwm-workspace--minibuffer
                                              'exwm-container)))
  (xcb:flush exwm--connection))

(defun exwm-workspace--on-minibuffer-setup ()
  "Run in minibuffer-setup-hook to show the minibuffer and its container."
  (unless (> -1 (minibuffer-depth))
    (add-hook 'post-command-hook #'exwm-workspace--update-minibuffer)
    (exwm-workspace--show-minibuffer)
    ;; Set input focus on the Emacs frame
    (x-focus-frame (window-frame (minibuffer-selected-window)))))

(defun exwm-workspace--on-minibuffer-exit ()
  "Run in minibuffer-exit-hook to hide the minibuffer container."
  (unless (> -1 (minibuffer-depth))
    (remove-hook 'post-command-hook #'exwm-workspace--update-minibuffer)
    (exwm-workspace--hide-minibuffer)))

(defvar exwm-input--during-command)

(defun exwm-workspace--on-echo-area-dirty ()
  "Run when new message arrives to show the echo area and its container."
  (when (and (not (active-minibuffer-window))
             (or (current-message)
                 cursor-in-echo-area))
    (exwm-workspace--update-minibuffer t)
    (exwm-workspace--show-minibuffer)
    (unless (or (not exwm-workspace-display-echo-area-timeout)
                exwm-input--during-command ;e.g. read-event
                input-method-use-echo-area)
      (setq exwm-workspace--display-echo-area-timer
            (run-with-timer exwm-workspace-display-echo-area-timeout nil
                            #'exwm-workspace--on-echo-area-clear)))))

(defun exwm-workspace--on-echo-area-clear ()
  "Run in echo-area-clear-hook to hide echo area container."
  (unless (active-minibuffer-window)
    (exwm-workspace--hide-minibuffer))
  (when exwm-workspace--display-echo-area-timer
    (cancel-timer exwm-workspace--display-echo-area-timer)
    (setq exwm-workspace--display-echo-area-timer nil)))

(defun exwm-workspace--confirm-kill-emacs (prompt)
  "Confirm before exiting Emacs."
  (when (pcase (length exwm--id-buffer-alist)
          (0 (y-or-n-p prompt))
          (x (yes-or-no-p (format "[EXWM] %d window%s currently alive. %s"
                                  x (if (= x 1) "" "s") prompt))))
    (dolist (i exwm--id-buffer-alist)
      (exwm-manage--unmanage-window (car i) t)
      (xcb:+request exwm--connection
          (make-instance 'xcb:MapWindow :window (car i))))
    (xcb:flush exwm--connection)
    t))

(defun exwm-workspace--init ()
  "Initialize workspace module."
  (cl-assert (and (< 0 exwm-workspace-number) (>= 10 exwm-workspace-number)))
  ;; Prevent unexpected exit
  (setq confirm-kill-emacs #'exwm-workspace--confirm-kill-emacs)
  (if (not (exwm-workspace--minibuffer-own-frame-p))
      ;; Initialize workspaces with minibuffers.
      (progn
        (setq exwm-workspace--list (frame-list))
        (when (< 1 (length exwm-workspace--list))
          ;; Emacs client creates an extra (but unusable) frame.
          (dolist (i exwm-workspace--list)
            (unless (frame-parameter i 'window-id)
              (setq exwm-workspace--list (delq i exwm-workspace--list))))
          (cl-assert (= 1 (length exwm-workspace--list)))
          ;; Prevent user from deleting this frame by accident.
          (set-frame-parameter (car exwm-workspace--list) 'client nil))
        ;; Create remaining frames.
        (dotimes (_ (1- exwm-workspace-number))
          (nconc exwm-workspace--list
                 (list (make-frame '((window-system . x)))))))
    ;; Initialize workspaces without minibuffers.
    (let ((old-frames (frame-list)))
      (setq exwm-workspace--minibuffer
            (make-frame '((window-system . x) (minibuffer . only)
                          (left . 10000) (right . 10000)
                          (width . 0) (height . 0)))
            default-minibuffer-frame exwm-workspace--minibuffer)
      ;; Remove/hide existing frames.
      (dolist (f old-frames)
        (if (frame-parameter f 'client)
            (make-frame-invisible f)
          (delete-frame f))))
    (let ((outer-id (string-to-number
                     (frame-parameter exwm-workspace--minibuffer
                                      'outer-window-id)))
          (container (xcb:generate-id exwm--connection)))
      (set-frame-parameter exwm-workspace--minibuffer 'exwm-outer-id outer-id)
      (set-frame-parameter exwm-workspace--minibuffer 'exwm-container
                           container)
      (xcb:+request exwm--connection
          (make-instance 'xcb:CreateWindow
                         :depth 0 :wid container :parent exwm--root
                         :x -1 :y -1 :width 1 :height 1
                         :border-width 0 :class xcb:WindowClass:CopyFromParent
                         :visual 0        ;CopyFromParent
                         :value-mask xcb:CW:OverrideRedirect
                         :override-redirect 1))
      (exwm--debug
       (xcb:+request exwm--connection
           (make-instance 'xcb:ewmh:set-_NET_WM_NAME
                          :window container
                          :data "Minibuffer container")))
      (xcb:+request exwm--connection
          (make-instance 'xcb:ReparentWindow
                         :window outer-id :parent container :x 0 :y 0))
      ;; Attach event listener for monitoring the frame
      (xcb:+request exwm--connection
          (make-instance 'xcb:ChangeWindowAttributes
                         :window outer-id
                         :value-mask xcb:CW:EventMask
                         :event-mask xcb:EventMask:StructureNotify))
      (xcb:+event exwm--connection 'xcb:ConfigureNotify
                  #'exwm-workspace--on-ConfigureNotify))
    ;; Show/hide minibuffer / echo area when they're active/inactive.
    (add-hook 'minibuffer-setup-hook #'exwm-workspace--on-minibuffer-setup)
    (add-hook 'minibuffer-exit-hook #'exwm-workspace--on-minibuffer-exit)
    (run-with-idle-timer 0 t #'exwm-workspace--on-echo-area-dirty)
    (add-hook 'echo-area-clear-hook #'exwm-workspace--on-echo-area-clear)
    ;; Create workspace frames.
    (dotimes (_ exwm-workspace-number)
      (push (make-frame `((window-system . x)
                          (minibuffer . ,(minibuffer-window
                                          exwm-workspace--minibuffer))))
            exwm-workspace--list))
    ;; The default behavior of `display-buffer' (indirectly called by
    ;; `minibuffer-completion-help') is not correct here.
    (cl-pushnew '(exwm-workspace--display-buffer) display-buffer-alist))
  ;; Configure workspaces
  (dolist (i exwm-workspace--list)
    (let ((outer-id (string-to-number (frame-parameter i 'outer-window-id)))
          (workspace (xcb:generate-id exwm--connection)))
      ;; Save window IDs
      (set-frame-parameter i 'exwm-outer-id outer-id)
      (set-frame-parameter i 'exwm-workspace workspace)
      (xcb:+request exwm--connection
          (make-instance 'xcb:CreateWindow
                         :depth 0 :wid workspace :parent exwm--root
                         :x 0 :y 0
                         :width (x-display-pixel-width)
                         :height (x-display-pixel-height)
                         :border-width 0 :class xcb:WindowClass:CopyFromParent
                         :visual 0      ;CopyFromParent
                         :value-mask (logior xcb:CW:OverrideRedirect
                                             xcb:CW:EventMask)
                         :override-redirect 1
                         :event-mask xcb:EventMask:SubstructureRedirect))
      (exwm--debug
       (xcb:+request exwm--connection
           (make-instance 'xcb:ewmh:set-_NET_WM_NAME
                          :window workspace
                          :data
                          (format "EXWM workspace %d"
                                  (cl-position i exwm-workspace--list)))))
      (xcb:+request exwm--connection
          (make-instance 'xcb:ReparentWindow
                         :window outer-id :parent workspace :x 0 :y 0))
      (xcb:+request exwm--connection
          (make-instance 'xcb:MapWindow :window workspace))))
  (xcb:flush exwm--connection)
  ;; We have to advice `x-create-frame' or every call to it would hang EXWM
  (advice-add 'x-create-frame :around #'exwm-workspace--x-create-frame)
  ;; Delay making the workspaces fullscreen until Emacs becomes idle
  (run-with-idle-timer 0 nil
                       (lambda ()
                         (dolist (i exwm-workspace--list)
                           (set-frame-parameter i 'fullscreen 'fullboth))))
  ;; Set _NET_VIRTUAL_ROOTS
  (xcb:+request exwm--connection
      (make-instance 'xcb:ewmh:set-_NET_VIRTUAL_ROOTS
                     :window exwm--root
                     :data (vconcat (mapcar
                                     (lambda (i)
                                       (frame-parameter i 'exwm-workspace))
                                     exwm-workspace--list))))
  ;; Switch to the first workspace
  (exwm-workspace-switch 0 t))



(provide 'exwm-workspace)

;;; exwm-workspace.el ends here
