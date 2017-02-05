;;; exwm-config.el --- Predefined configurations  -*- lexical-binding: t -*-

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

;; This module contains typical (yet minimal) configurations of EXWM.

;;; Code:

(require 'exwm)
(require 'ido)

(defun exwm-config-default ()
  "Default configuration of EXWM."
  ;; Set the initial workspace number.
  (setq exwm-workspace-number 4)
  ;; Make class name the buffer name
  (add-hook 'exwm-update-class-hook
            (lambda ()
              (exwm-workspace-rename-buffer exwm-class-name)))
  ;; 's-r': Reset
  (exwm-input-set-key (kbd "s-r") #'exwm-reset)
  ;; 's-w': Switch workspace
  (exwm-input-set-key (kbd "s-w") #'exwm-workspace-switch)
  ;; 's-N': Switch to certain workspace
  (dotimes (i 10)
    (exwm-input-set-key (kbd (format "s-%d" i))
                        `(lambda ()
                           (interactive)
                           (exwm-workspace-switch-create ,i))))
  ;; 's-&': Launch application
  (exwm-input-set-key (kbd "s-&")
                      (lambda (command)
                        (interactive (list (read-shell-command "$ ")))
                        (start-process-shell-command command nil command)))
  ;; Line-editing shortcuts
  (exwm-input-set-simulation-keys
   '(([?\C-b] . left)
     ([?\C-f] . right)
     ([?\C-p] . up)
     ([?\C-n] . down)
     ([?\C-a] . home)
     ([?\C-e] . end)
     ([?\M-v] . prior)
     ([?\C-v] . next)
     ([?\C-d] . delete)
     ([?\C-k] . (S-end delete))))
  ;; Enable EXWM
  (exwm-enable)
  ;; Configure Ido
  (exwm-config-ido)
  ;; Other configurations
  (exwm-config-misc))

(defun exwm-config--fix/ido-buffer-window-other-frame ()
  "Fix `ido-buffer-window-other-frame'."
  (defalias 'exwm-config-ido-buffer-window-other-frame
    (symbol-function #'ido-buffer-window-other-frame))
  (defun ido-buffer-window-other-frame (buffer)
    "This is a version redefined by EXWM.

You can find the original one at `exwm-config-ido-buffer-window-other-frame'."
    (with-current-buffer (window-buffer (selected-window))
      (if (and (eq major-mode 'exwm-mode)
               exwm--floating-frame)
          ;; Switch from a floating frame.
          (with-current-buffer buffer
            (if (and (eq major-mode 'exwm-mode)
                     exwm--floating-frame
                     (eq exwm--frame exwm-workspace--current))
                ;; Switch to another floating frame.
                (frame-root-window exwm--floating-frame)
              ;; Do not switch if the buffer is not on the current workspace.
              (or (get-buffer-window buffer exwm-workspace--current)
                  (selected-window))))
        (with-current-buffer buffer
          (when (eq major-mode 'exwm-mode)
            (if (eq exwm--frame exwm-workspace--current)
                (when exwm--floating-frame
                  ;; Switch to a floating frame on the current workspace.
                  (frame-selected-window exwm--floating-frame))
              ;; Do not switch to exwm-mode buffers on other workspace (which
              ;; won't work unless `exwm-layout-show-all-buffers' is set)
              (unless exwm-layout-show-all-buffers
                (selected-window)))))))))

(defun exwm-config-ido ()
  "Configure Ido to work with EXWM."
  (ido-mode 1)
  (add-hook 'exwm-init-hook #'exwm-config--fix/ido-buffer-window-other-frame))

(defun exwm-config-misc ()
  "Other configurations."
  ;; Make more room
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (fringe-mode 1))



(provide 'exwm-config)

;;; exwm-config.el ends here
