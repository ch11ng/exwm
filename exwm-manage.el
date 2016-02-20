;;; exwm-manage.el --- Window Management Module for  -*- lexical-binding: t -*-
;;;                    EXWM

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

;; This is the fundamental module of EXWM that deals with window management.

;;; Code:

(require 'exwm-core)

(defvar exwm-manage-finish-hook nil
  "Normal hook run after a window is just managed, in the context of the
corresponding buffer.")

(defun exwm-manage--update-geometry (id &optional force)
  "Update window geometry."
  (with-current-buffer (exwm--id->buffer id)
    (unless (and exwm--geometry (not force))
      (let ((reply (xcb:+request-unchecked+reply exwm--connection
                       (make-instance 'xcb:GetGeometry :drawable id))))
        (when reply                     ;nil when destroyed
          (setq exwm--geometry reply))))))

;; The _MOTIF_WM_HINTS atom (see <Xm/MwmUtil.h> for more details)
;; It's currently only used in 'exwm-manage' module
(defvar exwm-manage--_MOTIF_WM_HINTS nil "_MOTIF_WM_HINTS atom.")

(defun exwm-manage--update-mwm-hints (id &optional force)
  "Update _MOTIF_WM_HINTS."
  (with-current-buffer (exwm--id->buffer id)
    (unless (and exwm--mwm-hints (not force))
      (let ((reply (xcb:+request-unchecked+reply exwm--connection
                       (make-instance 'xcb:icccm:-GetProperty
                                      :window id
                                      :property exwm-manage--_MOTIF_WM_HINTS
                                      :type exwm-manage--_MOTIF_WM_HINTS
                                      :long-length 5))))
        (when reply
          (setq exwm--mwm-hints (append (slot-value reply 'value) nil)))))))

(defvar exwm-workspace--current)
(defvar exwm-workspace--switch-history-outdated)

(declare-function exwm--update-window-type "exwm.el" (id &optional force))
(declare-function exwm--update-class "exwm.el" (id &optional force))
(declare-function exwm--update-transient-for "exwm.el" (id &optional force))
(declare-function exwm--update-normal-hints "exwm.el" (id &optional force))
(declare-function exwm--update-title "exwm.el" (id))
(declare-function exwm--update-hints "exwm.el" (id &optional force))
(declare-function exwm--update-protocols "exwm.el" (id &optional force))
(declare-function exwm--update-state "exwm.el" (id &optional force))
(declare-function exwm-floating--set-floating "exwm-floating.el" (id))
(declare-function exwm-floating--unset-floating "exwm-floating.el" (id))

(defun exwm-manage--manage-window (id)
  "Manage window ID."
  (exwm--log "Try to manage #x%x" id)
  (catch 'return
    ;; Ensure it's alive
    (when (xcb:+request-checked+request-check exwm--connection
              (make-instance 'xcb:ChangeWindowAttributes
                             :window id :value-mask xcb:CW:EventMask
                             :event-mask exwm--client-event-mask))
      (throw 'return 'dead))
    (with-current-buffer (generate-new-buffer "*EXWM*")
      (push `(,id . ,(current-buffer)) exwm--id-buffer-alist)
      (exwm-mode)
      (setq exwm--id id)
      (exwm--update-window-type id)
      (exwm--update-class id)
      (exwm--update-transient-for id)
      (exwm--update-normal-hints id)
      (exwm-manage--update-geometry id)
      (exwm-manage--update-mwm-hints id)
      ;; No need to manage (please check OverrideRedirect outside)
      (when (or
             (not
              (or (not exwm-window-type)
                  (memq xcb:Atom:_NET_WM_WINDOW_TYPE_UTILITY exwm-window-type)
                  (memq xcb:Atom:_NET_WM_WINDOW_TYPE_DIALOG exwm-window-type)
                  (memq xcb:Atom:_NET_WM_WINDOW_TYPE_NORMAL exwm-window-type)))
             ;; Check _MOTIF_WM_HINTS for Java applications
             ;; See <Xm/MwmUtil.h> for the definitions of these fields
             (and exwm--mwm-hints
                  exwm-instance-name
                  (/= 0 (logand (elt exwm--mwm-hints 0) ;MotifWmHints.flags
                                2))             ;MWM_HINTS_DECORATIONS
                  (= 0 (elt exwm--mwm-hints 2)) ;MotifWmHints.decorations
                  ;; Java applications only
                  (string-prefix-p "sun-awt-X11-" exwm-instance-name)
                  ;; Floating windows only
                  (or exwm-transient-for exwm--fixed-size
                      (memq xcb:Atom:_NET_WM_WINDOW_TYPE_UTILITY
                            exwm-window-type)
                      (memq xcb:Atom:_NET_WM_WINDOW_TYPE_DIALOG
                            exwm-window-type))))
        (exwm--log "No need to manage #x%x" id)
        ;; Remove all events
        (xcb:+request exwm--connection
            (make-instance 'xcb:ChangeWindowAttributes
                           :window id :value-mask xcb:CW:EventMask
                           :event-mask xcb:EventMask:NoEvent))
        ;; The window needs to be mapped
        (xcb:+request exwm--connection
            (make-instance 'xcb:MapWindow :window id))
        (with-slots (x y width height) exwm--geometry
          ;; Reparent to virtual root
          (unless (or (memq xcb:Atom:_NET_WM_WINDOW_TYPE_DESKTOP
                            exwm-window-type)
                      (memq xcb:Atom:_NET_WM_WINDOW_TYPE_DOCK
                            exwm-window-type))
            (xcb:+request exwm--connection
                (make-instance 'xcb:ReparentWindow
                               :window id
                               :parent (frame-parameter exwm-workspace--current
                                                        'exwm-workspace)
                               :x x :y y)))
          ;; Center window of type _NET_WM_WINDOW_TYPE_SPLASH
          (when (memq xcb:Atom:_NET_WM_WINDOW_TYPE_SPLASH exwm-window-type)
            (xcb:+request exwm--connection
                (make-instance 'xcb:ConfigureWindow
                               :window id
                               :value-mask (eval-when-compile
                                             (logior xcb:ConfigWindow:X
                                                     xcb:ConfigWindow:Y))
                               :x (/ (- (exwm-workspace--current-width) width)
                                     2)
                               :y (/ (- (exwm-workspace--current-height)
                                        height)
                                     2)))))
        (xcb:flush exwm--connection)
        (setq exwm--id-buffer-alist (assq-delete-all id exwm--id-buffer-alist))
        (let ((kill-buffer-query-functions nil))
          (kill-buffer (current-buffer)))
        (throw 'return 'ignored))
      ;; Manage the window
      (exwm--log "Manage #x%x" id)
      ;; Create a new container as the parent of this X window
      (setq exwm--container (xcb:generate-id exwm--connection))
      (xcb:+request exwm--connection
          (make-instance 'xcb:CreateWindow
                         :depth 0 :wid exwm--container
                         :parent (frame-parameter exwm-workspace--current
                                                  'exwm-workspace)
                         :x 0 :y 0 :width 1 :height 1 :border-width 0
                         :class xcb:WindowClass:CopyFromParent
                         :visual 0      ;CopyFromParent
                         :value-mask (logior xcb:CW:OverrideRedirect
                                             xcb:CW:EventMask)
                         :override-redirect 1
                         :event-mask xcb:EventMask:SubstructureRedirect))
      (exwm--debug
       (xcb:+request exwm--connection
           (make-instance 'xcb:ewmh:set-_NET_WM_NAME
                          :window exwm--container
                          :data (format "EXWM container for 0x%x" id))))
      (xcb:+request exwm--connection
          (make-instance 'xcb:ReparentWindow
                         :window id :parent exwm--container :x 0 :y 0))
      (xcb:+request exwm--connection    ;remove border
          (make-instance 'xcb:ConfigureWindow
                         :window id :value-mask xcb:ConfigWindow:BorderWidth
                         :border-width 0))
      (dolist (button       ;grab buttons to set focus / move / resize
               (list xcb:ButtonIndex:1 xcb:ButtonIndex:2 xcb:ButtonIndex:3))
        (xcb:+request exwm--connection
            (make-instance 'xcb:GrabButton
                           :owner-events 0 :grab-window id
                           :event-mask xcb:EventMask:ButtonPress
                           :pointer-mode xcb:GrabMode:Sync
                           :keyboard-mode xcb:GrabMode:Async
                           :confine-to xcb:Window:None :cursor xcb:Cursor:None
                           :button button :modifiers xcb:ModMask:Any)))
      (xcb:+request exwm--connection    ;update _NET_CLIENT_LIST
          (make-instance 'xcb:ewmh:set-_NET_CLIENT_LIST
                         :window exwm--root
                         :data (vconcat (mapcar #'car exwm--id-buffer-alist))))
      (xcb:flush exwm--connection)
      (exwm--update-title id)
      (exwm--update-hints id)
      (exwm--update-protocols id)
      (exwm--update-state id)
      (if (or exwm-transient-for exwm--fixed-size
              (memq xcb:Atom:_NET_WM_WINDOW_TYPE_UTILITY exwm-window-type)
              (memq xcb:Atom:_NET_WM_WINDOW_TYPE_DIALOG exwm-window-type))
          (exwm-floating--set-floating id)
        (exwm-floating--unset-floating id))
      (exwm-input-grab-keyboard id)
      (setq exwm-workspace--switch-history-outdated t)
      (with-current-buffer (exwm--id->buffer id)
        (run-hooks 'exwm-manage-finish-hook)))))

(defun exwm-manage--unmanage-window (id &optional withdraw-only)
  "Unmanage window ID."
  (let ((buffer (exwm--id->buffer id)))
    (exwm--log "Unmanage #x%x (buffer: %s, widthdraw: %s)"
               id buffer withdraw-only)
    (setq exwm--id-buffer-alist (assq-delete-all id exwm--id-buffer-alist))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        ;; Flickering seems unavoidable here if the DestroyWindow request is
        ;; not initiated by us.
        ;; What we can do is to hide the its container ASAP.
        (xcb:+request exwm--connection
            (make-instance 'xcb:UnmapWindow :window exwm--container))
        (xcb:flush exwm--connection)
        ;; Unmap the X window.
        (xcb:+request exwm--connection
            (make-instance 'xcb:UnmapWindow :window id))
        ;;
        (setq exwm-workspace--switch-history-outdated t)
        ;;
        (when withdraw-only
          ;; Reparent back to root
          (xcb:+request exwm--connection
              (make-instance 'xcb:ChangeWindowAttributes
                             :window id :value-mask xcb:CW:EventMask
                             :event-mask xcb:EventMask:NoEvent))
          (let (x y geometry geometry-parent)
            (if (not exwm--floating-frame)
                (setq x 0 y 0)          ;the position does not matter
              (setq geometry-parent
                    (xcb:+request-unchecked+reply exwm--connection
                        (make-instance 'xcb:GetGeometry
                                       :drawable exwm--container))
                    geometry (xcb:+request-unchecked+reply exwm--connection
                                 (make-instance 'xcb:GetGeometry
                                                :drawable id)))
              (if (not (and geometry-parent geometry))
                  (setq x 0 y 0)        ;e.g. have been destroyed
                (setq x (+ (slot-value geometry-parent 'x)
                           (slot-value geometry 'x))
                      y (+ (slot-value geometry-parent 'y)
                           (slot-value geometry 'y)))))
            (xcb:+request exwm--connection
                (make-instance 'xcb:ReparentWindow
                               :window id :parent exwm--root :x x :y y)))
          ;; Delete WM_STATE property
          (xcb:+request exwm--connection
              (make-instance 'xcb:DeleteProperty
                             :window id :property xcb:Atom:WM_STATE)))
        (when exwm--floating-frame
          ;; Unmap the floating frame before destroying the containers.
          (let ((window (frame-parameter exwm--floating-frame 'exwm-outer-id)))
            (xcb:+request exwm--connection
                (make-instance 'xcb:UnmapWindow :window window))
            (xcb:+request exwm--connection
                (make-instance 'xcb:ReparentWindow
                               :window window :parent exwm--root :x 0 :y 0))))
        ;; Destroy the X window container (and the frame container if any).
        (xcb:+request exwm--connection
            (make-instance 'xcb:DestroyWindow :window exwm--container))
        (let ((kill-buffer-query-functions nil)
              (floating exwm--floating-frame))
          (kill-buffer)
          (when floating
            (select-window
             (frame-selected-window exwm-workspace--current)))))
      (xcb:+request exwm--connection    ;update _NET_CLIENT_LIST
          (make-instance 'xcb:ewmh:set-_NET_CLIENT_LIST
                         :window exwm--root
                         :data (vconcat (mapcar #'car exwm--id-buffer-alist))))
      (xcb:flush exwm--connection))))

(defun exwm-manage--scan ()
  "Search for existing windows and try to manage them."
  (let* ((tree (xcb:+request-unchecked+reply exwm--connection
                   (make-instance 'xcb:QueryTree :window exwm--root))))
    (dolist (i (slot-value tree 'children))
      (with-slots (override-redirect map-state)
          (xcb:+request-unchecked+reply exwm--connection
              (make-instance 'xcb:GetWindowAttributes :window i))
        (when (and (= 0 override-redirect) (= xcb:MapState:Viewable map-state))
          (xcb:+request exwm--connection
              (make-instance 'xcb:UnmapWindow :window i))
          (xcb:flush exwm--connection)
          (exwm-manage--manage-window i))))))

(defvar exwm-manage--ping-lock nil
  "Non-nil indicates EXWM is pinging a window.")
(defvar exwm-manage-ping-timeout 3 "Seconds to wait before killing a client.")

(defun exwm-manage--kill-buffer-query-function ()
  "Run in `kill-buffer-query-functions'."
  (catch 'return
    (when (xcb:+request-checked+request-check exwm--connection
              (make-instance 'xcb:MapWindow :window exwm--id))
      ;; The X window is no longer alive so just close the buffer.
      ;; Destroy the container.
      ;; Hide the container to prevent flickering.
      (xcb:+request exwm--connection
          (make-instance 'xcb:UnmapWindow :window exwm--container))
      (xcb:flush exwm--connection)
      (when exwm--floating-frame
        (let ((window (frame-parameter exwm--floating-frame 'exwm-outer-id)))
          (xcb:+request exwm--connection
              (make-instance 'xcb:UnmapWindow :window window))
          (xcb:+request exwm--connection
              (make-instance 'xcb:ReparentWindow
                             :window window
                             :parent exwm--root
                             :x 0 :y 0))))
      (xcb:+request exwm--connection
          (make-instance 'xcb:DestroyWindow :window exwm--container))
      (xcb:flush exwm--connection)
      (throw 'return t))
    (unless (memq xcb:Atom:WM_DELETE_WINDOW exwm--protocols)
      ;; The X window does not support WM_DELETE_WINDOW; destroy it.
      ;; Hide the container to prevent flickering.
      (xcb:+request exwm--connection
          (make-instance 'xcb:UnmapWindow :window exwm--container))
      (xcb:+request exwm--connection
          (make-instance 'xcb:DestroyWindow :window exwm--id))
      (xcb:flush exwm--connection)
      ;; Wait for DestroyNotify event.
      (throw 'return nil))
    ;; Try to close the X window with WM_DELETE_WINDOW client message.
    (xcb:+request exwm--connection
        (make-instance 'xcb:icccm:SendEvent
                       :destination exwm--id
                       :event (xcb:marshal
                               (make-instance 'xcb:icccm:WM_DELETE_WINDOW
                                              :window exwm--id)
                               exwm--connection)))
    (xcb:flush exwm--connection)
    ;;
    (unless (memq xcb:Atom:_NET_WM_PING exwm--protocols)
      ;; The window does not support _NET_WM_PING.  To make sure it'll die,
      ;; kill it after the time runs out.
      ;; Hide the container to prevent flickering.
      (xcb:+request exwm--connection
          (make-instance 'xcb:UnmapWindow :window exwm--container))
      (xcb:flush exwm--connection)
      (run-with-timer exwm-manage-ping-timeout nil
                      `(lambda () (exwm-manage--kill-client ,exwm--id)))
      ;; Wait for DestroyNotify event.
      (throw 'return nil))
    ;; Try to determine if the X window is dead with _NET_WM_PING.
    (setq exwm-manage--ping-lock t)
    (xcb:+request exwm--connection
        (make-instance 'xcb:SendEvent
                       :propagate 0
                       :destination exwm--id
                       :event-mask xcb:EventMask:NoEvent
                       :event (xcb:marshal
                               (make-instance 'xcb:ewmh:_NET_WM_PING
                                              :window exwm--id
                                              :timestamp 0
                                              :client-window exwm--id)
                               exwm--connection)))
    (xcb:flush exwm--connection)
    (with-timeout (exwm-manage-ping-timeout
                   (if (yes-or-no-p (format "'%s' is not responding. \
Would you like to kill it? "
                                            (buffer-name)))
                       (progn (exwm-manage--kill-client exwm--id)
                              ;; Kill the unresponsive X window and
                              ;; wait for DestroyNotify event.
                              (throw 'return nil))
                     ;; Give up.
                     (throw 'return nil)))
      (while (and exwm-manage--ping-lock
                  (exwm--id->buffer exwm--id)) ;may have been destroyed.
        (accept-process-output nil 0.1))
      ;; Give up.
      (throw 'return nil))))

(defun exwm-manage--kill-client (&optional id)
  "Kill an X client."
  (unless id (setq id (exwm--buffer->id (current-buffer))))
  ;; Hide the container to prevent flickering.
  (let ((buffer (exwm--id->buffer id)))
    (when buffer
      (with-current-buffer buffer
        (xcb:+request exwm--connection
            (make-instance 'xcb:UnmapWindow :window exwm--container))
        (xcb:flush exwm--connection))))
  (let* ((response (xcb:+request-unchecked+reply exwm--connection
                       (make-instance 'xcb:ewmh:get-_NET_WM_PID :window id)))
         (pid (and response (slot-value response 'value)))
         (request (make-instance 'xcb:KillClient :resource id)))
    (if (not pid)
        (xcb:+request exwm--connection request)
      ;; What if the PID is fake/wrong?
      (signal-process pid 'SIGKILL)
      ;; Ensure it's dead
      (run-with-timer exwm-manage-ping-timeout nil
                      `(lambda ()
                         (xcb:+request exwm--connection ,request))))
    (xcb:flush exwm--connection)))

(defun exwm-manage--on-ConfigureRequest (data _synthetic)
  "Handle ConfigureRequest event."
  (let ((obj (make-instance 'xcb:ConfigureRequest))
        buffer edges)
    (xcb:unmarshal obj data)
    (with-slots (window x y width height
                        border-width sibling stack-mode value-mask)
        obj
      (exwm--log "ConfigureRequest from #x%x (#x%x) @%dx%d%+d%+d; \
border-width: %d; sibling: #x%x; stack-mode: %d"
                 window value-mask width height x y
                 border-width sibling stack-mode)
      (if (setq buffer (exwm--id->buffer window))
          ;; Send client message for managed windows
          (with-current-buffer buffer
            (setq edges
                  (if exwm--fullscreen
                      (list 0 0
                            (exwm-workspace--current-width)
                            (exwm-workspace--current-height))
                    (window-inside-absolute-pixel-edges
                     (get-buffer-window buffer t))))
            (exwm--log "Reply with ConfigureNotify (edges): %s" edges)
            (xcb:+request exwm--connection
                (make-instance 'xcb:SendEvent
                               :propagate 0 :destination window
                               :event-mask xcb:EventMask:StructureNotify
                               :event (xcb:marshal
                                       (make-instance
                                        'xcb:ConfigureNotify
                                        :event window :window window
                                        :above-sibling xcb:Window:None
                                        :x (elt edges 0) :y (elt edges 1)
                                        :width (- (elt edges 2) (elt edges 0))
                                        :height (- (elt edges 3) (elt edges 1))
                                        :border-width 0 :override-redirect 0)
                                       exwm--connection))))
        (exwm--log "ConfigureWindow (preserve geometry)")
        ;; Configure the unmanaged window.
        (xcb:+request exwm--connection
            (make-instance 'xcb:ConfigureWindow
                           :window window
                           :value-mask value-mask
                           :x x :y y :width width :height height
                           :border-width border-width
                           :sibling sibling
                           :stack-mode stack-mode)))))
  (xcb:flush exwm--connection))

(defun exwm-manage--on-MapRequest (data _synthetic)
  "Handle MapRequest event."
  (let ((obj (make-instance 'xcb:MapRequest)))
    (xcb:unmarshal obj data)
    (with-slots (parent window) obj
      (if (assoc window exwm--id-buffer-alist)
          (exwm--log "#x%x is already managed" window)
        (if (/= exwm--root parent)
            (progn (xcb:+request exwm--connection
                       (make-instance 'xcb:MapWindow :window window))
                   (xcb:flush exwm--connection))
          (exwm--log "MapRequest from #x%x" window)
          (exwm-manage--manage-window window))))))

(defun exwm-manage--on-UnmapNotify (data _synthetic)
  "Handle UnmapNotify event."
  (let ((obj (make-instance 'xcb:UnmapNotify)))
    (xcb:unmarshal obj data)
    (with-slots (window) obj
      (exwm--log "UnmapNotify from #x%x" window)
      (exwm-manage--unmanage-window window t))))

(defun exwm-manage--on-DestroyNotify (data synthetic)
  "Handle DestroyNotify event."
  (unless synthetic
    (let ((obj (make-instance 'xcb:DestroyNotify)))
      (xcb:unmarshal obj data)
      (exwm--log "DestroyNotify from #x%x" (slot-value obj 'window))
      (exwm-manage--unmanage-window (slot-value obj 'window)))))

(defun exwm-manage--init ()
  "Initialize manage module."
  ;; Intern _MOTIF_WM_HINTS
  (let ((atom-name "_MOTIF_WM_HINTS"))
    (setq exwm-manage--_MOTIF_WM_HINTS
          (slot-value (xcb:+request-unchecked+reply exwm--connection
                          (make-instance 'xcb:InternAtom
                                         :only-if-exists 0
                                         :name-len (length atom-name)
                                         :name atom-name))
                      'atom)))
  (xcb:+event exwm--connection 'xcb:ConfigureRequest
              #'exwm-manage--on-ConfigureRequest)
  (xcb:+event exwm--connection 'xcb:MapRequest #'exwm-manage--on-MapRequest)
  (xcb:+event exwm--connection 'xcb:UnmapNotify #'exwm-manage--on-UnmapNotify)
  (xcb:+event exwm--connection 'xcb:DestroyNotify
              #'exwm-manage--on-DestroyNotify))



(provide 'exwm-manage)

;;; exwm-manage.el ends here
