;;; exwm-systemtray.el --- System Tray Module for  -*- lexical-binding: t -*-
;;;                        EXWM

;; Copyright (C) 2016-2017 Free Software Foundation, Inc.

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

;; This module adds system tray support for EXWM.

;; To use this module, load and enable it as follows:
;;   (require 'exwm-systemtray)
;;   (exwm-systemtray-enable)

;;; Code:

(require 'xcb-icccm)
(require 'xcb-xembed)
(require 'xcb-systemtray)
(require 'exwm-core)

(defclass exwm-systemtray--icon ()
  ((width :initarg :width)
   (height :initarg :height)
   (visible :initarg :visible))
  :documentation "Attributes of a system tray icon.")

(defclass xcb:systemtray:-ClientMessage
  (xcb:icccm:--ClientMessage xcb:ClientMessage)
  ((format :initform 32)
   (type :initform xcb:Atom:MANAGER)
   (time :initarg :time :type xcb:TIMESTAMP)      ;new slot
   (selection :initarg :selection :type xcb:ATOM) ;new slot
   (owner :initarg :owner :type xcb:WINDOW))      ;new slot
  :documentation "A systemtray client message.")

;; GTK icons require at least 16 pixels to show normally.
(defconst exwm-systemtray--icon-min-size 16 "Minimum icon size.")

(defvar exwm-systemtray-height nil "System tray height.

You shall use the default value if using auto-hide minibuffer.")

(defvar exwm-systemtray-icon-gap 2 "Gap between icons.")

(defvar exwm-systemtray--connection nil "The X connection.")
(defvar exwm-systemtray--list nil "The icon list.")
(defvar exwm-systemtray--selection-owner-window nil
  "The selection owner window.")
(defvar exwm-systemtray--embedder nil "The embedder window.")

(defvar exwm-workspace--current)
(declare-function exwm-workspace--current-height "exwm-workspace.el")
(declare-function exwm-workspace--current-width  "exwm-workspace.el")
(declare-function exwm-workspace--minibuffer-own-frame-p "exwm-workspace.el")

(defun exwm-systemtray--embed (icon)
  "Embed an icon."
  (exwm--log "(System Tray) Try to embed #x%x" icon)
  (let ((info (xcb:+request-unchecked+reply exwm-systemtray--connection
                  (make-instance 'xcb:xembed:get-_XEMBED_INFO
                                 :window icon)))
        width* height* visible)
    (when info
      (exwm--log "(System Tray) Embed #x%x" icon)
      (with-slots (width height)
          (xcb:+request-unchecked+reply exwm-systemtray--connection
              (make-instance 'xcb:GetGeometry :drawable icon))
        (setq height* exwm-systemtray-height
              width* (round (* width (/ (float height*) height))))
        (when (< width* exwm-systemtray--icon-min-size)
          (setq width* exwm-systemtray--icon-min-size
                height* (round (* height (/ (float width*) width)))))
        (exwm--log "(System Tray) Resize from %dx%d to %dx%d"
                   width height width* height*))
      ;; Add this icon to save-set.
      (xcb:+request exwm-systemtray--connection
          (make-instance 'xcb:ChangeSaveSet
                         :mode xcb:SetMode:Insert
                         :window icon))
      ;; Reparent to the embedder.
      (xcb:+request exwm-systemtray--connection
          (make-instance 'xcb:ReparentWindow
                         :window icon
                         :parent exwm-systemtray--embedder
                         :x 0
                         ;; Vertically centered.
                         :y (/ (- exwm-systemtray-height height*) 2)))
      ;; Resize the icon.
      (xcb:+request exwm-systemtray--connection
          (make-instance 'xcb:ConfigureWindow
                         :window icon
                         :value-mask (logior xcb:ConfigWindow:Width
                                             xcb:ConfigWindow:Height
                                             xcb:ConfigWindow:BorderWidth)
                         :width width*
                         :height height*
                         :border-width 0))
      ;; Set event mask.
      (xcb:+request exwm-systemtray--connection
          (make-instance 'xcb:ChangeWindowAttributes
                         :window icon
                         :value-mask xcb:CW:EventMask
                         :event-mask (logior xcb:EventMask:ResizeRedirect
                                             xcb:EventMask:KeyPress
                                             xcb:EventMask:PropertyChange)))
      ;; Grab all keys and forward them to Emacs frame.
      (unless (exwm-workspace--minibuffer-own-frame-p)
        (xcb:+request exwm-systemtray--connection
            (make-instance 'xcb:GrabKey
                           :owner-events 0
                           :grab-window icon
                           :modifiers xcb:ModMask:Any
                           :key xcb:Grab:Any
                           :pointer-mode xcb:GrabMode:Async
                           :keyboard-mode xcb:GrabMode:Async)))
      (setq visible (slot-value info 'flags))
      (if visible
          (setq visible
                (/= 0 (logand (slot-value info 'flags) xcb:xembed:MAPPED)))
        ;; Default to visible.
        (setq visible t))
      (when visible
        (exwm--log "(System Tray) Map the window")
        (xcb:+request exwm-systemtray--connection
            (make-instance 'xcb:MapWindow :window icon)))
      (xcb:+request exwm-systemtray--connection
          (make-instance 'xcb:xembed:SendEvent
                         :destination icon
                         :event
                         (xcb:marshal
                          (make-instance 'xcb:xembed:EMBEDDED-NOTIFY
                                         :window icon
                                         :time xcb:Time:CurrentTime
                                         :embedder exwm-systemtray--embedder
                                         :version 0)
                          exwm-systemtray--connection)))
      (push `(,icon . ,(make-instance 'exwm-systemtray--icon
                                      :width width*
                                      :height height*
                                      :visible visible))
            exwm-systemtray--list)
      (exwm-systemtray--refresh))))

(defun exwm-systemtray--unembed (icon)
  "Unembed an icon."
  (exwm--log "(System Tray) Unembed #x%x" icon)
  (xcb:+request exwm-systemtray--connection
      (make-instance 'xcb:UnmapWindow :window icon))
  (xcb:+request exwm-systemtray--connection
      (make-instance 'xcb:ReparentWindow
                     :window icon
                     :parent exwm--root
                     :x 0 :y 0))
  (setq exwm-systemtray--list
        (assq-delete-all icon exwm-systemtray--list))
  (exwm-systemtray--refresh))

(defun exwm-systemtray--refresh ()
  "Refresh the system tray."
  ;; Make sure to redraw the embedder.
  (xcb:+request exwm-systemtray--connection
      (make-instance 'xcb:UnmapWindow :window exwm-systemtray--embedder))
  (let ((x exwm-systemtray-icon-gap)
        map)
    (dolist (pair exwm-systemtray--list)
      (when (slot-value (cdr pair) 'visible)
        (xcb:+request exwm-systemtray--connection
            (make-instance 'xcb:ConfigureWindow
                           :window (car pair)
                           :value-mask xcb:ConfigWindow:X
                           :x x))
        (setq x (+ x (slot-value (cdr pair) 'width)
                   exwm-systemtray-icon-gap))
        (setq map t)))
    (xcb:+request exwm-systemtray--connection
        (make-instance 'xcb:ConfigureWindow
                       :window exwm-systemtray--embedder
                       :value-mask (logior xcb:ConfigWindow:X
                                           xcb:ConfigWindow:Width)
                       :x (- (exwm-workspace--current-width) x)
                       :width x))
    (when map
      (xcb:+request exwm-systemtray--connection
          (make-instance 'xcb:MapWindow :window exwm-systemtray--embedder))))
  (xcb:flush exwm-systemtray--connection))

(defun exwm-systemtray--on-DestroyNotify (data _synthetic)
  "Unembed icons on DestroyNotify."
  (let ((obj (make-instance 'xcb:DestroyNotify)))
    (xcb:unmarshal obj data)
    (with-slots (window) obj
      (when (assoc window exwm-systemtray--list)
        (exwm-systemtray--unembed window)))))

(defun exwm-systemtray--on-ReparentNotify (data _synthetic)
  "Unembed icons on ReparentNotify."
  (let ((obj (make-instance 'xcb:ReparentNotify)))
    (xcb:unmarshal obj data)
    (with-slots (window parent) obj
      (when (and (/= parent exwm-systemtray--embedder)
                 (assoc window exwm-systemtray--list))
        (exwm-systemtray--unembed window)))))

(defun exwm-systemtray--on-ResizeRequest (data _synthetic)
  "Resize the tray icon on ResizeRequest."
  (let ((obj (make-instance 'xcb:ResizeRequest))
        attr)
    (xcb:unmarshal obj data)
    (with-slots (window width height) obj
      (when (setq attr (cdr (assoc window exwm-systemtray--list)))
        (with-slots ((width* width)
                     (height* height))
            attr
          (setq height* exwm-systemtray-height
                width* (round (* width (/ (float height*) height))))
          (when (< width* exwm-systemtray--icon-min-size)
            (setq width* exwm-systemtray--icon-min-size
                  height* (round (* height (/ (float width*) width)))))
          (xcb:+request exwm-systemtray--connection
              (make-instance 'xcb:ConfigureWindow
                             :window window
                             :value-mask (logior xcb:ConfigWindow:Y
                                                 xcb:ConfigWindow:Width
                                                 xcb:ConfigWindow:Height)
                             ;; Vertically centered.
                             :y (/ (- exwm-systemtray-height height*) 2)
                             :width width*
                             :height height*)))
        (exwm-systemtray--refresh)))))

(defun exwm-systemtray--on-PropertyNotify (data _synthetic)
  "Map/Unmap the tray icon on PropertyNotify."
  (let ((obj (make-instance 'xcb:PropertyNotify))
        attr info visible)
    (xcb:unmarshal obj data)
    (with-slots (window atom state) obj
      (when (and (eq state xcb:Property:NewValue)
                 (eq atom xcb:Atom:_XEMBED_INFO)
                 (setq attr (cdr (assoc window exwm-systemtray--list))))
        (setq info (xcb:+request-unchecked+reply exwm-systemtray--connection
                       (make-instance 'xcb:xembed:get-_XEMBED_INFO
                                      :window window)))
        (when info
          (setq visible (/= 0 (logand (slot-value info 'flags)
                                      xcb:xembed:MAPPED)))
          (exwm--log "(System Tray) #x%x visible? %s" window visible)
          (if visible
              (xcb:+request exwm-systemtray--connection
                  (make-instance 'xcb:MapWindow :window window))
            (xcb:+request exwm-systemtray--connection
                (make-instance 'xcb:UnmapWindow :window window)))
          (setf (slot-value attr 'visible) visible)
          (exwm-systemtray--refresh))))))

(defun exwm-systemtray--on-ClientMessage (data _synthetic)
  "Handle client messages."
  (let ((obj (make-instance 'xcb:ClientMessage))
        opcode data32)
    (xcb:unmarshal obj data)
    (with-slots (window type data) obj
      (when (eq type xcb:Atom:_NET_SYSTEM_TRAY_OPCODE)
        (setq data32 (slot-value data 'data32)
              opcode (elt data32 1))
        (cond ((= opcode xcb:systemtray:opcode:REQUEST-DOCK)
               (unless (assoc (elt data32 2) exwm-systemtray--list)
                 (exwm-systemtray--embed (elt data32 2))))
              ;; Not implemented (rarely used nowadays).
              ((or (= opcode xcb:systemtray:opcode:BEGIN-MESSAGE)
                   (= opcode xcb:systemtray:opcode:CANCEL-MESSAGE)))
              (t
               (exwm--log "(System Tray) Unknown opcode message: %s" obj)))))))

(defun exwm-systemtray--on-KeyPress (data _synthetic)
  "Forward all KeyPress events to Emacs frame."
  ;; This function is only executed when there's no autohide minibuffer,
  ;; a workspace frame has the input focus and the pointer is over a
  ;; tray icon.
  (let ((dest (frame-parameter (selected-frame) 'exwm-outer-id))
        (obj (make-instance 'xcb:KeyPress)))
    (xcb:unmarshal obj data)
    (setf (slot-value obj 'event) dest)
    (xcb:+request exwm-systemtray--connection
        (make-instance 'xcb:SendEvent
                       :propagate 0
                       :destination dest
                       :event-mask xcb:EventMask:NoEvent
                       :event (xcb:marshal obj exwm-systemtray--connection))))
  (xcb:flush exwm-systemtray--connection))

(defvar exwm-workspace--workareas)
(defvar exwm-workspace-current-index)

(defun exwm-systemtray--on-workspace-switch ()
  "Reparent/Refresh the system tray in `exwm-workspace-switch-hook'."
  (unless (exwm-workspace--minibuffer-own-frame-p)
    (xcb:+request exwm-systemtray--connection
        (make-instance 'xcb:ReparentWindow
                       :window exwm-systemtray--embedder
                       :parent (string-to-number
                                (frame-parameter exwm-workspace--current
                                                 'window-id))
                       :x 0
                       :y (- (elt (elt exwm-workspace--workareas
                                       exwm-workspace-current-index)
                                  3)
                             exwm-systemtray-height))))
  (exwm-systemtray--refresh))

(defun exwm-systemtray--on-randr-refresh ()
  "Reposition/Refresh the system tray in `exwm-randr-refresh-hook'."
  (unless (exwm-workspace--minibuffer-own-frame-p)
    (xcb:+request exwm-systemtray--connection
        (make-instance 'xcb:ConfigureWindow
                       :window exwm-systemtray--embedder
                       :value-mask xcb:ConfigWindow:Y
                       :y (- (elt (elt exwm-workspace--workareas
                                       exwm-workspace-current-index)
                                  3)
                             exwm-systemtray-height))))
  (exwm-systemtray--refresh))

(defalias 'exwm-systemtray--on-struts-update
  #'exwm-systemtray--on-randr-refresh)

(defvar xcb:Atom:_NET_SYSTEM_TRAY_S0)
(defvar exwm-workspace--minibuffer)

(cl-defun exwm-systemtray--init ()
  "Initialize system tray module."
  (cl-assert (not exwm-systemtray--connection))
  (cl-assert (not exwm-systemtray--list))
  (cl-assert (not exwm-systemtray--selection-owner-window))
  (cl-assert (not exwm-systemtray--embedder))
  (unless exwm-systemtray-height
    (setq exwm-systemtray-height (max exwm-systemtray--icon-min-size
                                      (line-pixel-height))))
  ;; Create a new connection.
  (setq exwm-systemtray--connection (xcb:connect))
  (set-process-query-on-exit-flag (slot-value exwm-systemtray--connection
                                              'process)
                                  nil)
  ;; Initialize XELB modules.
  (xcb:xembed:init exwm-systemtray--connection t)
  (xcb:systemtray:init exwm-systemtray--connection t)
  ;; Acquire the manager selection _NET_SYSTEM_TRAY_S0.
  (with-slots (owner)
      (xcb:+request-unchecked+reply exwm-systemtray--connection
          (make-instance 'xcb:GetSelectionOwner
                         :selection xcb:Atom:_NET_SYSTEM_TRAY_S0))
    (when (/= owner xcb:Window:None)
      (xcb:disconnect exwm-systemtray--connection)
      (warn "[EXWM] Other system tray detected")
      (cl-return-from exwm-systemtray--init)))
  (let ((id (xcb:generate-id exwm-systemtray--connection)))
    (setq exwm-systemtray--selection-owner-window id)
    (xcb:+request exwm-systemtray--connection
        (make-instance 'xcb:CreateWindow
                       :depth 0
                       :wid id
                       :parent exwm--root
                       :x 0
                       :y 0
                       :width 1
                       :height 1
                       :border-width 0
                       :class xcb:WindowClass:InputOnly
                       :visual 0
                       :value-mask xcb:CW:OverrideRedirect
                       :override-redirect 1))
    ;; Get the selection ownership.
    (xcb:+request exwm-systemtray--connection
        (make-instance 'xcb:SetSelectionOwner
                       :owner id
                       :selection xcb:Atom:_NET_SYSTEM_TRAY_S0
                       :time xcb:Time:CurrentTime))
    ;; Send a client message to announce the selection.
    (xcb:+request exwm-systemtray--connection
        (make-instance 'xcb:SendEvent
                       :propagate 0
                       :destination exwm--root
                       :event-mask xcb:EventMask:StructureNotify
                       :event (xcb:marshal
                               (make-instance 'xcb:systemtray:-ClientMessage
                                              :window exwm--root
                                              :time xcb:Time:CurrentTime
                                              :selection
                                              xcb:Atom:_NET_SYSTEM_TRAY_S0
                                              :owner id)
                               exwm-systemtray--connection)))
    ;; Set _NET_WM_NAME.
    (xcb:+request exwm-systemtray--connection
        (make-instance 'xcb:ewmh:set-_NET_WM_NAME
                       :window id :data "EXWM system tray selection owner"))
    ;; Set the _NET_SYSTEM_TRAY_ORIENTATION property.
    (xcb:+request exwm-systemtray--connection
        (make-instance 'xcb:xembed:set-_NET_SYSTEM_TRAY_ORIENTATION
                       :window id
                       :data xcb:systemtray:ORIENTATION:HORZ)))
  ;; Create the embedder.
  (let ((id (xcb:generate-id exwm-systemtray--connection))
        frame parent depth y)
    (setq exwm-systemtray--embedder id)
    (if (exwm-workspace--minibuffer-own-frame-p)
        (setq frame exwm-workspace--minibuffer
              y (if (>= (line-pixel-height) exwm-systemtray-height)
                    ;; Bottom aligned.
                    (- (line-pixel-height) exwm-systemtray-height)
                  ;; Vertically centered.
                  (/ (- (line-pixel-height) exwm-systemtray-height) 2)))
      (setq frame exwm-workspace--current
            ;; Bottom aligned.
            y (- (exwm-workspace--current-height) exwm-systemtray-height)))
    (setq parent (string-to-number (frame-parameter frame 'window-id))
          depth (slot-value (xcb:+request-unchecked+reply
                                exwm-systemtray--connection
                                (make-instance 'xcb:GetGeometry
                                               :drawable parent))
                            'depth))
    (xcb:+request exwm-systemtray--connection
        (make-instance 'xcb:CreateWindow
                       :depth depth
                       :wid id
                       :parent parent
                       :x 0
                       :y y
                       :width 1
                       :height exwm-systemtray-height
                       :border-width 0
                       :class xcb:WindowClass:InputOutput
                       :visual 0
                       :value-mask (logior xcb:CW:BackPixmap xcb:CW:EventMask)
                       :background-pixmap xcb:BackPixmap:ParentRelative
                       :event-mask xcb:EventMask:SubstructureNotify))
    ;; Set _NET_WM_NAME.
    (xcb:+request exwm-systemtray--connection
        (make-instance 'xcb:ewmh:set-_NET_WM_NAME
                       :window id :data "EXWM system tray embedder")))
  (xcb:flush exwm-systemtray--connection)
  ;; Attach event listeners.
  (xcb:+event exwm-systemtray--connection 'xcb:DestroyNotify
              #'exwm-systemtray--on-DestroyNotify)
  (xcb:+event exwm-systemtray--connection 'xcb:ReparentNotify
              #'exwm-systemtray--on-ReparentNotify)
  (xcb:+event exwm-systemtray--connection 'xcb:ResizeRequest
              #'exwm-systemtray--on-ResizeRequest)
  (xcb:+event exwm-systemtray--connection 'xcb:PropertyNotify
              #'exwm-systemtray--on-PropertyNotify)
  (xcb:+event exwm-systemtray--connection 'xcb:ClientMessage
              #'exwm-systemtray--on-ClientMessage)
  (unless (exwm-workspace--minibuffer-own-frame-p)
    (xcb:+event exwm-systemtray--connection 'xcb:KeyPress
                #'exwm-systemtray--on-KeyPress))
  ;; Add hook to move/reparent the embedder.
  (add-hook 'exwm-workspace-switch-hook #'exwm-systemtray--on-workspace-switch)
  (add-hook 'exwm-workspace--update-workareas-hook
            #'exwm-systemtray--on-struts-update)
  (when (boundp 'exwm-randr-refresh-hook)
    (add-hook 'exwm-randr-refresh-hook #'exwm-systemtray--on-randr-refresh)))

(defun exwm-systemtray--exit ()
  "Exit the systemtray module."
  (when exwm-systemtray--connection
    (xcb:disconnect exwm-systemtray--connection)
    (setq exwm-systemtray--connection nil
          exwm-systemtray--list nil
          exwm-systemtray--selection-owner-window nil
          exwm-systemtray--embedder nil)
    (remove-hook 'exwm-workspace-switch-hook
                 #'exwm-systemtray--on-workspace-switch)
    (remove-hook 'exwm-workspace--update-workareas-hook
                 #'exwm-systemtray--on-struts-update)
    (when (boundp 'exwm-randr-refresh-hook)
      (remove-hook 'exwm-randr-refresh-hook
                   #'exwm-systemtray--on-randr-refresh))))

(defun exwm-systemtray-enable ()
  "Enable system tray support for EXWM."
  (add-hook 'exwm-init-hook #'exwm-systemtray--init)
  (add-hook 'exwm-exit-hook #'exwm-systemtray--exit))



(provide 'exwm-systemtray)

;;; exwm-systemtray.el ends here
