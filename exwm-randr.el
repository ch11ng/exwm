;;; exwm-randr.el --- RandR Module for EXWM  -*- lexical-binding: t -*-

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

;; This module adds RandR support for EXWM. Currently it requires external
;; tools such as xrandr(1) to properly configure RandR first. This dependency
;; may be removed in the future, but more work is needed before that.

;; To use this module, first load/enable it and properly configure the variable
;; `exwm-randr-workspace-output-plist':
;;   (require 'exwm-randr)
;;   (setq exwm-randr-workspace-output-plist '(0 "VGA1"))
;;   (exwm-randr-enable)
;; Then configure RandR with 'xrandr':
;;   $ xrandr --output VGA1 --left-of LVDS1 --auto
;; With above lines, workspace 0 should be assigned to the output named "VGA1",
;; staying at the left of other workspaces on the output "LVDS1".

;; References:
;; + RandR (http://www.x.org/archive/X11R7.7/doc/randrproto/randrproto.txt)

;;; Code:

(require 'xcb-randr)

(defvar exwm-randr-workspace-output-plist nil)

(defun exwm-randr--refresh ()
  "Refresh workspaces according to the updated RandR info."
  (let (geometry output-plist default-geometry workareas viewports)
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
          (setq name                    ;UTF-8 encoded
                (decode-coding-string (apply 'unibyte-string name) 'utf-8))
          (if (or (/= connection xcb:randr:Connection:Connected)
                  (= 0 crtc))           ;FIXME
              (plist-put output-plist name nil)
            (with-slots (x y width height)
                (xcb:+request-unchecked+reply exwm--connection
                    (make-instance 'xcb:randr:GetCrtcInfo
                                   :crtc crtc
                                   :config-timestamp config-timestamp))
              (setq geometry (make-instance 'xcb:RECTANGLE
                                            :x x :y y
                                            :width width :height height)
                    output-plist (plist-put output-plist name geometry))
              (unless default-geometry ;assume the first output as primary
                (setq default-geometry geometry)))))))
    (cl-assert (<= 2 (length output-plist)))
    (dotimes (i (length exwm-workspace--list))
      (let* ((output (plist-get exwm-randr-workspace-output-plist i))
             (geometry (lax-plist-get output-plist output))
             (frame (elt exwm-workspace--list i)))
        (unless geometry
          (setq geometry default-geometry
                output nil))
        (set-frame-parameter frame 'exwm-randr-output output)
        (set-frame-parameter frame 'exwm-geometry geometry)
        (with-slots (x y width height) geometry
          (xcb:+request exwm--connection
              (make-instance 'xcb:ConfigureWindow
                             :window (frame-parameter frame 'exwm-outer-id)
                             :value-mask (logior xcb:ConfigWindow:X
                                                 xcb:ConfigWindow:Y
                                                 xcb:ConfigWindow:Width
                                                 xcb:ConfigWindow:Height)
                             :x x :y y :width width :height height))
          (setq workareas (nconc workareas (list x y width height))
                viewports (nconc viewports (list x y))))))
    ;; Update _NET_WORKAREA
    (xcb:+request exwm--connection
        (make-instance 'xcb:ewmh:set-_NET_WORKAREA
                       :window exwm--root :data (vconcat workareas)))
    ;; Update _NET_DESKTOP_VIEWPORT
    (xcb:+request exwm--connection
      (make-instance 'xcb:ewmh:set-_NET_DESKTOP_VIEWPORT
                     :window exwm--root
                     :data (vconcat viewports)))
    (xcb:flush exwm--connection)))

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
        (exwm-randr--refresh)
        (xcb:+event exwm--connection 'xcb:randr:ScreenChangeNotify
                    (lambda (data synthetic)
                      (exwm--log "(RandR) ScreenChangeNotify")
                      (exwm-randr--refresh)))
        ;; (xcb:+event exwm--connection 'xcb:randr:Notify
        ;;             (lambda (data synthetic)
        ;;               (exwm--log "(RandR) Notify")
        ;;               (exwm-randr--refresh)))
        (xcb:+request exwm--connection
            (make-instance 'xcb:randr:SelectInput
                           :window exwm--root
                           :enable xcb:randr:NotifyMask:ScreenChange
                           ;; :enable (logior
                           ;;          xcb:randr:NotifyMask:ScreenChange
                           ;;          xcb:randr:NotifyMask:OutputChange
                           ;;          xcb:randr:NotifyMask:OutputProperty
                           ;;          xcb:randr:NotifyMask:CrtcChange)
                           ))
        (xcb:flush exwm--connection)))))

(defun exwm-randr-enable ()
  "Enable RandR support for EXWM."
  (add-hook 'exwm-init-hook 'exwm-randr--init))



(provide 'exwm-randr)

;;; exwm-randr.el ends here
