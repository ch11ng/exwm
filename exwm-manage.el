;;; exwm-manage.el --- Window Management Module for  -*- lexical-binding: t -*-
;;;                    EXWM

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

;; This is the fundamental module of EXWM that deals with window management.

;;; Code:

(require 'exwm-core)

(defvar exwm-manage-force-tiling nil
  "Non-nil to force managing all X windows in tiling layout.

You can still make the X windows floating afterwards.")

(defvar exwm-manage-finish-hook nil
  "Normal hook run after a window is just managed, in the context of the
corresponding buffer.")

(defvar exwm-manage--desktop nil "The desktop X window.")

(defun exwm-manage--update-geometry (id &optional force)
  "Update window geometry."
  (with-current-buffer (exwm--id->buffer id)
    (unless (and exwm--geometry (not force))
      (let ((reply (xcb:+request-unchecked+reply exwm--connection
                       (make-instance 'xcb:GetGeometry :drawable id))))
        (when reply                     ;nil when destroyed
          (setq exwm--geometry reply))))))

(defun exwm-manage--update-ewmh-state (id)
  "Update _NET_WM_STATE."
  (with-current-buffer (exwm--id->buffer id)
    (unless exwm--ewmh-state
      (let ((reply (xcb:+request-unchecked+reply exwm--connection
                       (make-instance 'xcb:ewmh:get-_NET_WM_STATE
                                      :window id))))
        (when reply
          (setq exwm--ewmh-state (append (slot-value reply 'value) nil)))))))

;; The _MOTIF_WM_HINTS atom (see <Xm/MwmUtil.h> for more details)
;; It's currently only used in 'exwm-manage' module
(defvar exwm-manage--_MOTIF_WM_HINTS nil "_MOTIF_WM_HINTS atom.")

(defun exwm-manage--update-mwm-hints (id &optional force)
  "Update _MOTIF_WM_HINTS."
  (with-current-buffer (exwm--id->buffer id)
    (unless (and (not exwm--mwm-hints-decorations) (not force))
      (let ((reply (xcb:+request-unchecked+reply exwm--connection
                       (make-instance 'xcb:icccm:-GetProperty
                                      :window id
                                      :property exwm-manage--_MOTIF_WM_HINTS
                                      :type exwm-manage--_MOTIF_WM_HINTS
                                      :long-length 5))))
        (when reply
          ;; Check MotifWmHints.decorations.
          (with-slots (value) reply
            (setq value (append value nil))
            (when (and value
                       ;; See <Xm/MwmUtil.h> for fields definitions.
                       (/= 0 (logand
                              (elt value 0) ;MotifWmHints.flags
                              2))           ;MWM_HINTS_DECORATIONS
                       (= 0
                          (elt value 2))) ;MotifWmHints.decorations
              (setq exwm--mwm-hints-decorations nil))))))))

(defun exwm-manage--set-client-list ()
  "Set _NET_CLIENT_LIST."
  (xcb:+request exwm--connection
      (make-instance 'xcb:ewmh:set-_NET_CLIENT_LIST
                     :window exwm--root
                     :data (vconcat (mapcar #'car exwm--id-buffer-alist)))))

(defvar exwm-floating--border-colormap)
(defvar exwm-floating--border-pixel)
(defvar exwm-workspace--current)
(defvar exwm-workspace--switch-history-outdated)
(defvar exwm-workspace-current-index)
(defvar exwm-workspace--workareas)

(declare-function exwm--update-window-type "exwm.el" (id &optional force))
(declare-function exwm--update-class "exwm.el" (id &optional force))
(declare-function exwm--update-transient-for "exwm.el" (id &optional force))
(declare-function exwm--update-normal-hints "exwm.el" (id &optional force))
(declare-function exwm--update-title "exwm.el" (id))
(declare-function exwm--update-hints "exwm.el" (id &optional force))
(declare-function exwm--update-protocols "exwm.el" (id &optional force))
(declare-function exwm--update-struts "exwm.el" (id))
(declare-function exwm-floating--set-floating "exwm-floating.el" (id))
(declare-function exwm-floating--unset-floating "exwm-floating.el" (id))
(declare-function exwm-input-grab-keyboard "exwm-input.el")
(declare-function exwm-workspace--current-height "exwm-workspace.el")
(declare-function exwm-workspace--current-width  "exwm-workspace.el")
(declare-function exwm-workspace--set-desktop "exwm-workspace.el" (id))
(declare-function exwm-workspace--count "exwm-workspace.el" ())
(declare-function exwm-workspace-move-window "exwm-workspace.el"
                  (frame-or-index &optional id))

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
    ;; Add this X window to save-set.
    (xcb:+request exwm--connection
        (make-instance 'xcb:ChangeSaveSet
                       :mode xcb:SetMode:Insert
                       :window id))
    (with-current-buffer (generate-new-buffer "*EXWM*")
      ;; Keep the oldest X window first.
      (setq exwm--id-buffer-alist
            (nconc exwm--id-buffer-alist `((,id . ,(current-buffer)))))
      (exwm-mode)
      (setq exwm--id id)
      (exwm--update-window-type id)
      (exwm--update-class id)
      (exwm--update-transient-for id)
      (exwm--update-normal-hints id)
      (exwm--update-hints id)
      (exwm-manage--update-geometry id)
      (exwm-manage--update-mwm-hints id)
      ;; No need to manage (please check OverrideRedirect outside)
      (when (or
             (not
              (or (not exwm-window-type)
                  (memq xcb:Atom:_NET_WM_WINDOW_TYPE_UTILITY exwm-window-type)
                  (memq xcb:Atom:_NET_WM_WINDOW_TYPE_DIALOG exwm-window-type)
                  (memq xcb:Atom:_NET_WM_WINDOW_TYPE_NORMAL exwm-window-type)))
             ;; Check the _MOTIF_WM_HINTS property.
             (and (not exwm--mwm-hints-decorations)
                  (not exwm--hints-input)
                  ;; Floating windows only
                  (or exwm-transient-for exwm--fixed-size
                      (memq xcb:Atom:_NET_WM_WINDOW_TYPE_UTILITY
                            exwm-window-type)
                      (memq xcb:Atom:_NET_WM_WINDOW_TYPE_DIALOG
                            exwm-window-type))))
        (exwm--log "No need to manage #x%x" id)
        ;; Update struts.
        (when (memq xcb:Atom:_NET_WM_WINDOW_TYPE_DOCK exwm-window-type)
          (exwm--update-struts id))
        ;; Remove all events
        (xcb:+request exwm--connection
            (make-instance 'xcb:ChangeWindowAttributes
                           :window id :value-mask xcb:CW:EventMask
                           :event-mask
                           (if (memq xcb:Atom:_NET_WM_WINDOW_TYPE_DOCK
                                     exwm-window-type)
                               ;; Listen for PropertyChange (struts) and
                               ;; UnmapNotify/DestroyNotify event of the dock.
                               exwm--client-event-mask
                             xcb:EventMask:NoEvent)))
        ;; The window needs to be mapped
        (xcb:+request exwm--connection
            (make-instance 'xcb:MapWindow :window id))
        (with-slots (x y width height) exwm--geometry
          ;; Reparent to virtual root
          (unless (or (memq xcb:Atom:_NET_WM_WINDOW_TYPE_DESKTOP
                            exwm-window-type)
                      (memq xcb:Atom:_NET_WM_WINDOW_TYPE_DOCK
                            exwm-window-type))
            (let ((workspace (frame-parameter exwm-workspace--current
                                              'exwm-workspace))
                  workarea)
              (when (and (/= x 0)
                         (/= y 0))
                (setq workarea (elt exwm-workspace--workareas
                                    exwm-workspace-current-index)
                      x (- x (aref workarea 0))
                      y (- y (aref workarea 1))))
              (xcb:+request exwm--connection
                  (make-instance 'xcb:ReparentWindow
                                 :window id
                                 :parent workspace
                                 :x x :y y))))
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
        ;; Check for desktop.
        (when (memq xcb:Atom:_NET_WM_WINDOW_TYPE_DESKTOP exwm-window-type)
          ;; There should be only one desktop X window.
          (setq exwm-manage--desktop id)
          ;; Put it at bottom.
          (xcb:+request exwm--connection
              (make-instance 'xcb:ConfigureWindow
                             :window id
                             :value-mask xcb:ConfigWindow:StackMode
                             :stack-mode xcb:StackMode:Below)))
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
                         :depth 0
                         :wid exwm--container
                         :parent (frame-parameter exwm-workspace--current
                                                  'exwm-workspace)
                         :x 0
                         :y 0
                         :width 1
                         :height 1
                         :border-width 0
                         :class xcb:WindowClass:InputOutput
                         :visual 0
                         :value-mask (logior xcb:CW:BackPixmap
                                             (if exwm-floating--border-pixel
                                                 xcb:CW:BorderPixel 0)
                                             xcb:CW:OverrideRedirect
                                             xcb:CW:EventMask
                                             (if exwm-floating--border-colormap
                                                 xcb:CW:Colormap 0))
                         :background-pixmap xcb:BackPixmap:ParentRelative
                         :border-pixel exwm-floating--border-pixel
                         :override-redirect 1
                         :event-mask xcb:EventMask:SubstructureRedirect
                         :colormap exwm-floating--border-colormap))
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
      (exwm-manage--set-client-list)
      (xcb:flush exwm--connection)
      (exwm--update-title id)
      (exwm--update-protocols id)
      (if (and (not exwm-manage-force-tiling)
               (or exwm-transient-for exwm--fixed-size
                   (memq xcb:Atom:_NET_WM_WINDOW_TYPE_UTILITY exwm-window-type)
                   (memq xcb:Atom:_NET_WM_WINDOW_TYPE_DIALOG
                         exwm-window-type)))
          (exwm-floating--set-floating id)
        (exwm-floating--unset-floating id))
      (exwm-input-grab-keyboard id)
      (setq exwm-workspace--switch-history-outdated t)
      ;; Set _NET_WM_DESKTOP or move window.
      (let ((reply (xcb:+request-unchecked+reply exwm--connection
                       (make-instance 'xcb:ewmh:get-_NET_WM_DESKTOP
                                      :window id)))
            desktop)
        (when reply
          (setq desktop (slot-value reply 'value)))
        (if (and desktop
                 (/= desktop exwm-workspace-current-index)
                 ;; Check the range.
                 (< desktop (exwm-workspace--count)))
            (exwm-workspace-move-window desktop id)
          (exwm-workspace--set-desktop id)))
      (exwm-manage--update-ewmh-state id)
      (with-current-buffer (exwm--id->buffer id)
        (when (memq xcb:Atom:_NET_WM_STATE_FULLSCREEN exwm--ewmh-state)
          (setq exwm--ewmh-state
                (delq xcb:Atom:_NET_WM_STATE_FULLSCREEN exwm--ewmh-state))
          (exwm-layout-set-fullscreen id))
        (run-hooks 'exwm-manage-finish-hook)))))

(defvar exwm-workspace--id-struts-alist)
(defvar exwm-workspace--list)

(declare-function exwm-workspace--update-struts "exwm-workspace.el" ())
(declare-function exwm-workspace--update-workareas "exwm-workspace.el" ())
(declare-function exwm-workspace--set-fullscreen "exwm-workspace.el" (frame))

(defun exwm-manage--unmanage-window (id &optional withdraw-only)
  "Unmanage window ID.

If WITHDRAW-ONLY is non-nil, the X window will be properly placed back to the
root window.  Set WITHDRAW-ONLY to 'quit if this functions is used when window
manager is shutting down."
  (let ((buffer (exwm--id->buffer id)))
    (exwm--log "Unmanage #x%x (buffer: %s, widthdraw: %s)"
               id buffer withdraw-only)
    (setq exwm--id-buffer-alist (assq-delete-all id exwm--id-buffer-alist))
    ;; Update workspaces when a dock is destroyed.
    (when (and (null withdraw-only)
               (assq id exwm-workspace--id-struts-alist))
      (setq exwm-workspace--id-struts-alist
            (assq-delete-all id exwm-workspace--id-struts-alist))
      (exwm-workspace--update-struts)
      (exwm-workspace--update-workareas)
      (dolist (f exwm-workspace--list)
        (exwm-workspace--set-fullscreen f)))
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
                             :window id :property xcb:Atom:WM_STATE))
          (unless (eq withdraw-only 'quit)
            ;; Remove _NET_WM_DESKTOP.
            (xcb:+request exwm--connection
                (make-instance 'xcb:DeleteProperty
                               :window id
                               :property xcb:Atom:_NET_WM_DESKTOP))))
        (when exwm--floating-frame
          ;; Unmap the floating frame before destroying the containers.
          (let ((window (frame-parameter exwm--floating-frame 'exwm-outer-id)))
            (xcb:+request exwm--connection
                (make-instance 'xcb:UnmapWindow :window window))
            (xcb:+request exwm--connection
                (make-instance 'xcb:ReparentWindow
                               :window window :parent exwm--root :x 0 :y 0))))
        ;; Restore the workspace if this X window is currently fullscreen.
        (when (memq xcb:Atom:_NET_WM_STATE_FULLSCREEN exwm--ewmh-state)
          (exwm-workspace--set-fullscreen exwm--frame))
        ;; Destroy the X window container (and the frame container if any).
        (xcb:+request exwm--connection
            (make-instance 'xcb:DestroyWindow :window exwm--container))
        (exwm-manage--set-client-list)
        (xcb:flush exwm--connection))
      (let ((kill-buffer-func
             (lambda (buffer)
               (with-current-buffer buffer
                 (let ((kill-buffer-query-functions nil)
                       (floating exwm--floating-frame))
                   (kill-buffer)
                   (when floating
                     (select-window
                      (frame-selected-window exwm-workspace--current))))))))
        (if (not (active-minibuffer-window))
            ;; Kill the buffer as usual.
            (funcall kill-buffer-func buffer)
          ;; This can happen when this buffer was requested to be killed
          ;; from the minibuffer (e.g. with `ido-kill-buffer-at-head').
          ;; We have to exit the minibuffer first or there'll be a
          ;; "selecting deleted buffer" error.
          (run-with-idle-timer 0 nil kill-buffer-func buffer)
          (exit-minibuffer))))))

(defun exwm-manage--scan ()
  "Search for existing windows and try to manage them."
  (let* ((tree (xcb:+request-unchecked+reply exwm--connection
                   (make-instance 'xcb:QueryTree
                                  :window exwm--root)))
         reply)
    (dolist (i (slot-value tree 'children))
      (setq reply (xcb:+request-unchecked+reply exwm--connection
                      (make-instance 'xcb:GetWindowAttributes
                                     :window i)))
      ;; It's possible the X window has been destroyed.
      (when reply
        (with-slots (override-redirect map-state) reply
          (when (and (= 0 override-redirect)
                     (= xcb:MapState:Viewable map-state))
            (xcb:+request exwm--connection
                (make-instance 'xcb:UnmapWindow
                               :window i))
            (xcb:flush exwm--connection)
            (exwm-manage--manage-window i)))))))

(defvar exwm-manage--ping-lock nil
  "Non-nil indicates EXWM is pinging a window.")
(defvar exwm-manage-ping-timeout 3 "Seconds to wait before killing a client.")

(defun exwm-manage--kill-buffer-query-function ()
  "Run in `kill-buffer-query-functions'."
  (catch 'return
    (when (or (not exwm--id)
              (not exwm--container)
              (xcb:+request-checked+request-check exwm--connection
                  (make-instance 'xcb:MapWindow
                                 :window exwm--id)))
      ;; The X window is no longer alive so just close the buffer.
      ;; Destroy the container.
      ;; Hide the container to prevent flickering.
      (when exwm--container
        (xcb:+request exwm--connection
            (make-instance 'xcb:UnmapWindow
                           :window exwm--container))
        (xcb:flush exwm--connection))
      (when exwm--floating-frame
        (let ((window (frame-parameter exwm--floating-frame 'exwm-outer-id)))
          (xcb:+request exwm--connection
              (make-instance 'xcb:UnmapWindow :window window))
          (xcb:+request exwm--connection
              (make-instance 'xcb:ReparentWindow
                             :window window
                             :parent exwm--root
                             :x 0 :y 0))))
      (when exwm--container
        (xcb:+request exwm--connection
            (make-instance 'xcb:DestroyWindow
                           :window exwm--container)))
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
    (let ((id exwm--id))
      ;; Try to close the X window with WM_DELETE_WINDOW client message.
      (xcb:+request exwm--connection
          (make-instance 'xcb:icccm:SendEvent
                         :destination id
                         :event (xcb:marshal
                                 (make-instance 'xcb:icccm:WM_DELETE_WINDOW
                                                :window id)
                                 exwm--connection)))
      (xcb:flush exwm--connection)
      ;;
      (unless (memq xcb:Atom:_NET_WM_PING exwm--protocols)
        ;; For X windows without _NET_WM_PING support, we'd better just
        ;; wait for DestroyNotify events.
        (throw 'return nil))
      ;; Try to determine if the X window is dead with _NET_WM_PING.
      (setq exwm-manage--ping-lock t)
      (xcb:+request exwm--connection
          (make-instance 'xcb:SendEvent
                         :propagate 0
                         :destination id
                         :event-mask xcb:EventMask:NoEvent
                         :event (xcb:marshal
                                 (make-instance 'xcb:ewmh:_NET_WM_PING
                                                :window id
                                                :timestamp 0
                                                :client-window id)
                                 exwm--connection)))
      (xcb:flush exwm--connection)
      (with-timeout (exwm-manage-ping-timeout
                     (if (y-or-n-p (format "'%s' is not responding.  \
Would you like to kill it? "
                                              (buffer-name)))
                         (progn (exwm-manage--kill-client id)
                                ;; Kill the unresponsive X window and
                                ;; wait for DestroyNotify event.
                                (throw 'return nil))
                       ;; Give up.
                       (throw 'return nil)))
        (while (and exwm-manage--ping-lock
                    (exwm--id->buffer id)) ;may have been destroyed.
          (accept-process-output nil 0.1))
        ;; Give up.
        (throw 'return nil)))))

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

;; FIXME: Make the following values as small as possible.
(defconst exwm-manage--width-delta-min 5)
(defconst exwm-manage--height-delta-min 5)

(defvar exwm-manage--frame-outer-id-list nil
  "List of window-outer-id's of all frames.")

(defun exwm-manage--add-frame (frame)
  "Run in `after-make-frame-functions'."
  (when (display-graphic-p frame)
    (push (string-to-number (frame-parameter frame 'outer-window-id))
          exwm-manage--frame-outer-id-list)))

(defun exwm-manage--remove-frame (frame)
  "Run in `delete-frame-functions'."
  (when (display-graphic-p frame)
    (setq exwm-manage--frame-outer-id-list
          (delq (string-to-number (frame-parameter frame 'outer-window-id))
                exwm-manage--frame-outer-id-list))))

(defun exwm-manage--on-ConfigureRequest (data _synthetic)
  "Handle ConfigureRequest event."
  (let ((obj (make-instance 'xcb:ConfigureRequest))
        buffer edges width-delta height-delta)
    (xcb:unmarshal obj data)
    (with-slots (window x y width height
                        border-width sibling stack-mode value-mask)
        obj
      (exwm--log "ConfigureRequest from #x%x (#x%x) @%dx%d%+d%+d; \
border-width: %d; sibling: #x%x; stack-mode: %d"
                 window value-mask width height x y
                 border-width sibling stack-mode)
      (if (and (setq buffer (exwm--id->buffer window))
               (with-current-buffer buffer
                 (or (memq xcb:Atom:_NET_WM_STATE_FULLSCREEN exwm--ewmh-state)
                     ;; Make sure it's a floating X window wanting to resize
                     ;; itself.
                     (or (not exwm--floating-frame)
                         (progn
                           (setq edges
                                 (window-inside-pixel-edges
                                  (get-buffer-window buffer t))
                                 width-delta (- width (- (elt edges 2)
                                                         (elt edges 0)))
                                 height-delta (- height (- (elt edges 3)
                                                           (elt edges 1))))
                           ;; We cannot do resizing precisely for now.
                           (and (if (= 0 (logand value-mask
                                                 xcb:ConfigWindow:Width))
                                    t
                                  (< (abs width-delta)
                                     exwm-manage--width-delta-min))
                                (if (= 0 (logand value-mask
                                                 xcb:ConfigWindow:Height))
                                    t
                                  (< (abs height-delta)
                                     exwm-manage--height-delta-min))))))))
          ;; Send client message for managed windows
          (with-current-buffer buffer
            (setq edges
                  (if (memq xcb:Atom:_NET_WM_STATE_FULLSCREEN exwm--ewmh-state)
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
        (if buffer
            (with-current-buffer buffer
              (exwm--log "ConfigureWindow (resize floating X window)")
              (when (and (/= 0 (logand value-mask xcb:ConfigWindow:Width))
                         (>= (abs width-delta) exwm-manage--width-delta-min))
                (set-frame-width exwm--floating-frame
                                 (+ (frame-pixel-width exwm--floating-frame)
                                    width-delta)
                                 nil t))
              (when (and (/= 0 (logand value-mask xcb:ConfigWindow:Height))
                         (>= (abs height-delta) exwm-manage--height-delta-min))
                (set-frame-height exwm--floating-frame
                                  (+ (frame-pixel-height exwm--floating-frame)
                                     height-delta)
                                  nil t)))
          (exwm--log "ConfigureWindow (preserve geometry)")
          ;; Configure the unmanaged window.
          ;; But Emacs frames should be excluded.  Generally we don't
          ;; receive ConfigureRequest events from Emacs frames since we
          ;; have set OverrideRedirect on them, but this is not true for
          ;; Lucid build (as of 25.1).
          (unless (memq window exwm-manage--frame-outer-id-list)
            (xcb:+request exwm--connection
                (make-instance 'xcb:ConfigureWindow
                               :window window
                               :value-mask value-mask
                               :x x :y y :width width :height height
                               :border-width border-width
                               :sibling sibling
                               :stack-mode stack-mode)))))))
  (xcb:flush exwm--connection))

(declare-function exwm-layout--iconic-state-p "exwm-layout.el" (&optional id))

(defun exwm-manage--on-MapRequest (data _synthetic)
  "Handle MapRequest event."
  (let ((obj (make-instance 'xcb:MapRequest)))
    (xcb:unmarshal obj data)
    (with-slots (parent window) obj
      (if (assoc window exwm--id-buffer-alist)
          (with-current-buffer (exwm--id->buffer window)
            (if (exwm-layout--iconic-state-p)
                ;; State change: iconic => normal.
                (when (eq exwm--frame exwm-workspace--current)
                  (set-window-buffer (frame-selected-window exwm--frame)
                                     (current-buffer))
                  (select-window (frame-selected-window exwm--frame)))
              (exwm--log "#x%x is already managed" window)))
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
  (add-hook 'after-make-frame-functions #'exwm-manage--add-frame)
  (add-hook 'delete-frame-functions #'exwm-manage--remove-frame)
  (xcb:+event exwm--connection 'xcb:ConfigureRequest
              #'exwm-manage--on-ConfigureRequest)
  (xcb:+event exwm--connection 'xcb:MapRequest #'exwm-manage--on-MapRequest)
  (xcb:+event exwm--connection 'xcb:UnmapNotify #'exwm-manage--on-UnmapNotify)
  (xcb:+event exwm--connection 'xcb:DestroyNotify
              #'exwm-manage--on-DestroyNotify))

(defun exwm-manage--exit ()
  "Exit the manage module."
  (setq exwm-manage--_MOTIF_WM_HINTS nil))



(provide 'exwm-manage)

;;; exwm-manage.el ends here
