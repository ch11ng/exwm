;;; exwm-background.el --- X Background Module for EXWM  -*- lexical-binding: t -*-

;; Copyright (C) 2022-2024 Free Software Foundation, Inc.

;; Author: Steven Allen <steven@stebalien.com>

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

;; This module adds X background color setting support to EXWM.

;; To use this module, load and enable it as follows:
;;   (require 'exwm-background)
;;   (exwm-background-enable)
;;
;; By default, this will apply the theme's background color.  However, that
;; color can be customized via the `exwm-background-color' setting.

;;; Code:

(require 'exwm-core)

(defcustom exwm-background-color nil
  "Background color for Xorg."
  :type '(choice
          (color :tag "Background Color")
          (const :tag "Default" nil))
  :group 'exwm
  :initialize #'custom-initialize-default
  :set (lambda (symbol value)
         (set-default-toplevel-value symbol value)
         (exwm-background--update)))

(defconst exwm-background--properties '("_XROOTPMAP_ID" "_XSETROOT_ID" "ESETROOT_PMAP_ID")
  "The background properties to set.
We can't need to set these so that compositing window managers
can correctly display the background color.")

(defvar exwm-background--connection nil
  "The X connection used for setting the background.
We use a separate connection as other background-setting tools
may kill this connection when they replace it.")

(defvar exwm-background--pixmap nil
  "Cached background pixmap.")

(defvar exwm-background--atoms nil
  "Cached background atoms.")

(defun exwm-background--update (&rest _)
  "Update the EXWM background."

  ;; Always reconnect as any tool that sets the background may have disconnected us (to force X to
  ;; free resources).
  (exwm-background--connect)

  (let ((gc (xcb:generate-id exwm-background--connection))
        (color (exwm--color->pixel (or exwm-background-color
                                       (face-background 'default)))))
    ;; Fill the pixmap.
    (xcb:+request exwm-background--connection
        (make-instance 'xcb:CreateGC
                       :cid gc :drawable exwm-background--pixmap
                       :value-mask (logior xcb:GC:Foreground
                                           xcb:GC:GraphicsExposures)
                       :foreground color
                       :graphics-exposures 0))

    (xcb:+request exwm-background--connection
        (make-instance 'xcb:PolyFillRectangle
                       :gc gc :drawable exwm-background--pixmap
                       :rectangles
                       (list
                        (make-instance
                         'xcb:RECTANGLE
                         :x 0 :y 0 :width 1 :height 1))))
    (xcb:+request exwm-background--connection (make-instance 'xcb:FreeGC :gc gc)))

  ;; Reapply it to force an update (also clobber anyone else who may have set it).
  (xcb:+request exwm-background--connection
      (make-instance 'xcb:ChangeWindowAttributes
                     :window exwm--root
                     :value-mask xcb:CW:BackPixmap
                     :background-pixmap exwm-background--pixmap))

  (let (old)
    ;; Collect old pixmaps so we can kill other background clients (all the background setting tools
    ;; seem to do this).
    (dolist (atom exwm-background--atoms)
      (when-let* ((reply (xcb:+request-unchecked+reply exwm-background--connection
                             (make-instance 'xcb:GetProperty
                                            :delete 0
                                            :window exwm--root
                                            :property atom
                                            :type xcb:Atom:PIXMAP
                                            :long-offset 0
                                            :long-length 1)))
                  (value (vconcat (slot-value reply 'value)))
                  ((length= value 4))
                  (pixmap (funcall (if xcb:lsb #'xcb:-unpack-u4-lsb #'xcb:-unpack-u4)
                                   value 0))
                  ((not (or (= pixmap exwm-background--pixmap)
                            (member pixmap old)))))
        (push pixmap old)))

    ;; Change the background.
    (dolist (atom exwm-background--atoms)
      (xcb:+request exwm-background--connection
          (make-instance 'xcb:ChangeProperty
                         :window exwm--root
                         :property atom
                         :type xcb:Atom:PIXMAP
                         :format 32
                         :mode xcb:PropMode:Replace
                         :data-len 1
                         :data
                         (funcall (if xcb:lsb
                                      #'xcb:-pack-u4-lsb
                                    #'xcb:-pack-u4)
                                  exwm-background--pixmap))))

    ;; Kill the old background clients.
    (dolist (pixmap old)
      (xcb:+request exwm-background--connection
          (make-instance 'xcb:KillClient :resource pixmap))))

  (xcb:flush exwm-background--connection))

(defun exwm-background--connected-p ()
  (and exwm-background--connection
       (process-live-p (slot-value exwm-background--connection 'process))))

(defun exwm-background--connect ()
  (unless (exwm-background--connected-p)
    (setq exwm-background--connection (xcb:connect))
    ;;prevent query message on exit
    (set-process-query-on-exit-flag (slot-value exwm-background--connection 'process) nil)

    ;; Intern the background property atoms.
    (setq exwm-background--atoms
          (mapcar
           (lambda (prop) (exwm--intern-atom prop exwm-background--connection))
           exwm-background--properties))

    ;; Create the pixmap.
    (setq exwm-background--pixmap (xcb:generate-id exwm-background--connection))
    (xcb:+request exwm-background--connection
        (make-instance 'xcb:CreatePixmap
                       :depth
                       (slot-value
                        (xcb:+request-unchecked+reply exwm-background--connection
                            (make-instance 'xcb:GetGeometry :drawable exwm--root))
                        'depth)
                       :pid exwm-background--pixmap
                       :drawable exwm--root
                       :width 1 :height 1))))

(defun exwm-background--init ()
  "Initialize background module."
  (exwm--log)
  (add-hook 'enable-theme-functions 'exwm-background--update)
  (add-hook 'disable-theme-functions 'exwm-background--update)
  (exwm-background--update))

(defun exwm-background--exit ()
  "Uninitialize the background module."
  (exwm--log)
  (remove-hook 'enable-theme-functions 'exwm-background--update)
  (remove-hook 'disable-theme-functions 'exwm-background--update)
  (when (and exwm-background--connection
             (slot-value exwm-background--connection 'connected))
    (xcb:disconnect exwm-background--connection))
  (setq exwm-background--pixmap nil
        exwm-background--connection nil
        exwm-background--atoms nil))

(defun exwm-background-enable ()
  "Enable background support for EXWM."
  (exwm--log)
  (add-hook 'exwm-init-hook #'exwm-background--init)
  (add-hook 'exwm-exit-hook #'exwm-background--exit))

(provide 'exwm-background)

;;; exwm-background.el ends here
