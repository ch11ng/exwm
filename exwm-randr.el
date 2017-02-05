;;; exwm-randr.el --- RandR Module for EXWM  -*- lexical-binding: t -*-

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

;; This module adds RandR support for EXWM.  Currently it requires external
;; tools such as xrandr(1) to properly configure RandR first.  This
;; dependency may be removed in the future, but more work is needed before
;; that.

;; To use this module, load, enable it and configure
;; `exwm-randr-workspace-output-plist' and `exwm-randr-screen-change-hook'
;; as follows:
;;
;;   (require 'exwm-randr)
;;   (setq exwm-randr-workspace-output-plist '(0 "VGA1"))
;;   (add-hook 'exwm-randr-screen-change-hook
;;             (lambda ()
;;               (start-process-shell-command
;;                "xrandr" nil "xrandr --output VGA1 --left-of LVDS1 --auto")))
;;   (exwm-randr-enable)
;;
;; With above lines, workspace 0 should be assigned to the output named "VGA1",
;; staying at the left of other workspaces on the output "LVDS1".  Please refer
;; to xrandr(1) for the configuration of RandR.

;; References:
;; + RandR (http://www.x.org/archive/X11R7.7/doc/randrproto/randrproto.txt)

;;; Code:

(require 'xcb-randr)
(require 'exwm-core)

(defvar exwm-randr-workspace-output-plist nil)

(defvar exwm-randr-refresh-hook nil
  "Normal hook run when the RandR module just refreshed.")

(defvar exwm-workspace--fullscreen-frame-count)
(defvar exwm-workspace--list)

(declare-function exwm-workspace--count "exwm-workspace.el")
(declare-function exwm-workspace--set-fullscreen "exwm-workspace.el" (frame))
(declare-function exwm-workspace--update-workareas "exwm-workspace.el" ())
(declare-function exwm-workspace--show-minibuffer "exwm-workspace.el" ())
(declare-function exwm-workspace--set-desktop-geometry "exwm-workspace.el" ())

(defun exwm-randr--refresh ()
  "Refresh workspaces according to the updated RandR info."
  (let (output-name geometry output-plist default-geometry)
    ;; Query all outputs
    (with-slots (config-timestamp outputs)
        (xcb:+request-unchecked+reply exwm--connection
            (make-instance 'xcb:randr:GetScreenResources
                           :window exwm--root))
      (dolist (output outputs)
        (with-slots (crtc connection name)
            (xcb:+request-unchecked+reply exwm--connection
                (make-instance 'xcb:randr:GetOutputInfo
                               :output output
                               :config-timestamp config-timestamp))
          (setf output-name             ;UTF-8 encoded
                (decode-coding-string (apply #'unibyte-string name) 'utf-8))
          (if (or (/= connection xcb:randr:Connection:Connected)
                  (= 0 crtc))           ;FIXME
              (plist-put output-plist output-name nil)
            (with-slots (x y width height)
                (xcb:+request-unchecked+reply exwm--connection
                    (make-instance 'xcb:randr:GetCrtcInfo
                                   :crtc crtc
                                   :config-timestamp config-timestamp))
              (setq geometry (make-instance 'xcb:RECTANGLE
                                            :x x :y y
                                            :width width :height height)
                    output-plist (plist-put output-plist output-name geometry))
              (unless default-geometry ;assume the first output as primary
                (setq default-geometry geometry)))))))
    (exwm--log "(randr) outputs: %s" output-plist)
    (when output-plist
      (when exwm-workspace--fullscreen-frame-count
        ;; Not all workspaces are fullscreen; reset this counter.
        (setq exwm-workspace--fullscreen-frame-count 0))
      (dotimes (i (exwm-workspace--count))
        (let* ((output (plist-get exwm-randr-workspace-output-plist i))
               (geometry (lax-plist-get output-plist output))
               (frame (elt exwm-workspace--list i)))
          (unless geometry
            (setq geometry default-geometry
                  output nil))
          (set-frame-parameter frame 'exwm-randr-output output)
          (set-frame-parameter frame 'exwm-geometry geometry)))
      ;; Update workareas.
      (exwm-workspace--update-workareas)
      ;; Resize workspace.
      (dolist (f exwm-workspace--list)
        (exwm-workspace--set-fullscreen f))
      ;; Raise the minibuffer if it's active.
      (when (and (active-minibuffer-window)
                 (exwm-workspace--minibuffer-own-frame-p))
        (exwm-workspace--show-minibuffer))
      ;; Set _NET_DESKTOP_GEOMETRY.
      (exwm-workspace--set-desktop-geometry)
      (xcb:flush exwm--connection)
      (run-hooks 'exwm-randr-refresh-hook))))

(defvar exwm-randr-screen-change-hook nil
  "Normal hook run when screen changes.")

(defun exwm-randr--init ()
  "Initialize RandR extension and EXWM RandR module."
  (if (= 0 (slot-value (xcb:get-extension-data exwm--connection 'xcb:randr)
                       'present))
      (error "[EXWM] RandR extension is not supported by the server")
    (with-slots (major-version minor-version)
        (xcb:+request-unchecked+reply exwm--connection
            (make-instance 'xcb:randr:QueryVersion
                           :major-version 1 :minor-version 2))
      (if (or (/= major-version 1) (< minor-version 2))
          (error "[EXWM] The server only support RandR version up to %d.%d"
                 major-version minor-version)
        ;; External monitor(s) may already be connected.
        (run-hooks 'exwm-randr-screen-change-hook)
        (exwm-randr--refresh)
        (xcb:+event exwm--connection 'xcb:randr:ScreenChangeNotify
                    (lambda (_data _synthetic)
                      (exwm--log "(RandR) ScreenChangeNotify")
                      (run-hooks 'exwm-randr-screen-change-hook)
                      (exwm-randr--refresh)))
        ;; (xcb:+event exwm--connection 'xcb:randr:Notify
        ;;             (lambda (_data _synthetic)
        ;;               (exwm--log "(RandR) Notify")
        ;;               (exwm-randr--refresh)))
        (xcb:+request exwm--connection
            (make-instance 'xcb:randr:SelectInput
                           :window exwm--root
                           :enable xcb:randr:NotifyMask:ScreenChange
                           ;; :enable (eval-when-compile
                           ;;           (logior
                           ;;            xcb:randr:NotifyMask:ScreenChange
                           ;;            xcb:randr:NotifyMask:OutputChange
                           ;;            xcb:randr:NotifyMask:OutputProperty
                           ;;            xcb:randr:NotifyMask:CrtcChange))
                           ))
        (xcb:flush exwm--connection)
        (add-hook 'exwm-workspace-list-change-hook #'exwm-randr--refresh))))
  ;; Prevent frame parameters introduced by this module from being
  ;; saved/restored.
  (dolist (i '(exwm-randr-output exwm-geometry))
    (push (cons i :never) frameset-filter-alist)))

(defun exwm-randr--exit ()
  "Exit the RandR module."
  (remove-hook 'exwm-workspace-list-change-hook #'exwm-randr--refresh))

(defun exwm-randr-enable ()
  "Enable RandR support for EXWM."
  (add-hook 'exwm-init-hook #'exwm-randr--init)
  (add-hook 'exwm-exit-hook #'exwm-randr--exit))



(provide 'exwm-randr)

;;; exwm-randr.el ends here
