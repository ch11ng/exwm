;;; exwm-manage.el --- Window Management Module for  -*- lexical-binding: t -*-
;;;                    EXWM

;; Copyright (C) 2015 Free Software Foundation, Inc.

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
(eval-when-compile (require 'exwm-workspace))

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

(defun exwm-manage--manage-window (id)
  "Manage window ID."
  (exwm--log "Try to manage #x%x" id)
  (catch 'return
    ;; Ensure it's not managed
    (when (assoc id exwm--id-buffer-alist)
      (exwm--log "#x%x is already managed" id)
      (throw 'return 'managed))
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
        (xcb:+request-checked+request-check exwm--connection
            (make-instance 'xcb:ChangeWindowAttributes
                           :window id :value-mask xcb:CW:EventMask
                           :event-mask xcb:EventMask:NoEvent))
        ;; The window needs to be mapped
        (xcb:+request exwm--connection
            (make-instance 'xcb:MapWindow :window id))
        (with-slots (x y width height) exwm--geometry
          ;; Reparent to virtual root (essential)
          (xcb:+request exwm--connection
              (make-instance 'xcb:ReparentWindow
                             :window id
                             :parent (frame-parameter exwm-workspace--current
                                                      'exwm-window-id)
                             :x x :y y))
          ;; Center window of type _NET_WM_WINDOW_TYPE_SPLASH
          (when (memq xcb:Atom:_NET_WM_WINDOW_TYPE_SPLASH exwm-window-type)
            (xcb:+request exwm--connection
                (make-instance 'xcb:ConfigureWindow
                               :window id
                               :value-mask (eval-when-compile
                                             (logior xcb:ConfigWindow:X
                                                     xcb:ConfigWindow:Y))
                               :x (/ (- (frame-pixel-width
                                         exwm-workspace--current)
                                        width)
                                     2)
                               :y (/ (- (frame-pixel-height
                                         exwm-workspace--current)
                                        height)
                                     2)))))
        (xcb:flush exwm--connection)
        (setq exwm--id-buffer-alist (assq-delete-all id exwm--id-buffer-alist))
        (let ((kill-buffer-query-functions nil))
          (kill-buffer (current-buffer)))
        (throw 'return 'ignored))
      ;; Manage the window
      (exwm--log "Manage #x%x" id)
      (xcb:+request exwm--connection    ;remove border
          (make-instance 'xcb:ConfigureWindow
                         :window id :value-mask xcb:ConfigWindow:BorderWidth
                         :border-width 0))
      (dolist (button       ;grab buttons to set focus / move / resize
               (list xcb:ButtonIndex:1 xcb:ButtonIndex:2 xcb:ButtonIndex:3))
        (xcb:+request-checked+request-check exwm--connection
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
    (exwm--log "Unmanage #x%x (buffer: %s)" id buffer)
    (setq exwm--id-buffer-alist (assq-delete-all id exwm--id-buffer-alist))
    (xcb:+request exwm--connection      ;update _NET_CLIENT_LIST
        (make-instance 'xcb:ewmh:set-_NET_CLIENT_LIST
                       :window exwm--root
                       :data (vconcat (mapcar #'car exwm--id-buffer-alist))))
    (xcb:flush exwm--connection)
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (when exwm--floating-frame
          (make-frame-invisible exwm--floating-frame)
          (redisplay))
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
                                       :drawable
                                       (frame-parameter exwm--floating-frame
                                                        'exwm-outer-id)))
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
          (xcb:flush exwm--connection))
        (let ((kill-buffer-query-functions nil)
              (floating exwm--floating-frame))
          (kill-buffer)
          (when floating
            (select-window
             (frame-selected-window exwm-workspace--current))))))))

(defun exwm-manage--scan ()
  "Search for existing windows and try to manage them."
  (let* ((tree (xcb:+request-unchecked+reply exwm--connection
                   (make-instance 'xcb:QueryTree :window exwm--root))))
    (dolist (i (slot-value tree 'children))
      (with-slots (override-redirect map-state)
          (xcb:+request-unchecked+reply exwm--connection
              (make-instance 'xcb:GetWindowAttributes :window i))
        (when (and (= 0 override-redirect) (= xcb:MapState:Viewable map-state))
          (exwm-manage--manage-window i))))))

(defvar exwm-manage--ping-lock nil
  "Non-nil indicates EXWM is pinging a window.")
(defvar exwm-manage-ping-timeout 3 "Seconds to wait before killing a client.")

;;;###autoload
(defun exwm-manage--close-window (id &optional buffer)
  "Close window ID in a proper way."
  (catch 'return
    (unless buffer (setq buffer (exwm--id->buffer id)))
    ;; Destroy the client window if it does not support WM_DELETE_WINDOW
    (unless (and (buffer-live-p buffer)
                 (with-current-buffer buffer
                   (memq xcb:Atom:WM_DELETE_WINDOW exwm--protocols)))
      (xcb:+request exwm--connection
          (make-instance 'xcb:DestroyWindow :window id))
      (xcb:flush exwm--connection)
      (throw 'return nil))
    ;; Try to close the window with WM_DELETE_WINDOW client message
    (xcb:+request exwm--connection
        (make-instance 'xcb:icccm:SendEvent
                       :destination id
                       :event (xcb:marshal
                               (make-instance 'xcb:icccm:WM_DELETE_WINDOW
                                              :window id)
                               exwm--connection)))
    (xcb:flush exwm--connection)
    ;; Try to determine if the client stop responding
    ;; FIXME: check
    (with-current-buffer buffer
      (when (memq xcb:Atom:_NET_WM_PING exwm--protocols)
        (setq exwm-manage--ping-lock t)
        (xcb:+request exwm--connection
            (make-instance 'xcb:SendEvent
                           :propagate 0 :destination id
                           :event-mask xcb:EventMask:NoEvent
                           :event (xcb:marshal
                                   (make-instance 'xcb:ewmh:_NET_WM_PING
                                                  :window id :timestamp 0
                                                  :client-window id)
                                   exwm--connection)))
        (xcb:flush exwm--connection)
        (with-timeout (exwm-manage-ping-timeout
                       (if (yes-or-no-p (format "\
`%s' is not responding. Would you like to kill it? " (buffer-name buffer)))
                           (progn (exwm-manage--kill-client id)
                                  (throw 'return nil))
                         (throw 'return nil)))
          (while (and exwm-manage--ping-lock
                      (exwm--id->buffer id)) ;may have be destroyed
            (accept-process-output nil 0.1)))
        (throw 'return nil)))
    (throw 'return nil)))

(defun exwm-manage--kill-client (&optional id)
  "Kill an X client."
  (interactive)
  (unless id (setq id (exwm--buffer->id (current-buffer))))
  (let* ((response (xcb:+request-unchecked+reply exwm--connection
                       (make-instance 'xcb:ewmh:get-_NET_WM_PID :window id)))
         (pid (and response (slot-value response 'value))))
    (if pid
        (signal-process pid 'SIGKILL)
      (xcb:+request exwm--connection
          (make-instance 'xcb:KillClient :resource id))
      (xcb:flush exwm--connection))))

(defun exwm-manage--on-ConfigureRequest (data _synthetic)
  "Handle ConfigureRequest event."
  (let ((obj (make-instance 'xcb:ConfigureRequest))
        buffer edges)
    (xcb:unmarshal obj data)
    (with-slots (stack-mode window sibling x y width height border-width
                            value-mask)
        obj
      (exwm--log "ConfigureRequest from #x%x (#x%x) @%dx%d%+d%+d, border: %d"
                 window value-mask width height x y border-width)
      (if (setq buffer (exwm--id->buffer window))
          ;; Send client message for managed windows
          (with-current-buffer buffer
            (setq edges
                  (if exwm--fullscreen
                      (list 0 0
                            (frame-pixel-width exwm-workspace--current)
                            (frame-pixel-height exwm-workspace--current))
                    (or exwm--floating-edges
                        (window-inside-absolute-pixel-edges
                         (get-buffer-window buffer t)))))
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
        ;; Configure unmanaged windows
        (xcb:+request exwm--connection
            (make-instance 'xcb:ConfigureWindow
                           :window window
                           :value-mask value-mask
                           :x x :y y :width width :height height
                           :border-width border-width
                           :sibling sibling :stack-mode stack-mode)))))
  (xcb:flush exwm--connection))

(defun exwm-manage--on-MapRequest (data _synthetic)
  "Handle MapRequest event."
  (let ((obj (make-instance 'xcb:MapRequest)))
    (xcb:unmarshal obj data)
    (with-slots (parent window) obj
      (if (/= exwm--root parent)
          (progn (xcb:+request exwm--connection
                     (make-instance 'xcb:MapWindow :window window))
                 (xcb:flush exwm--connection))
        (exwm--log "MapRequest from #x%x" window)
        (exwm-manage--manage-window window)))))

(defun exwm-manage--on-UnmapNotify (data synthetic)
  "Handle UnmapNotify event."
  (unless synthetic
    (let ((obj (make-instance 'xcb:UnmapNotify)))
      (xcb:unmarshal obj data)
      (exwm--log "UnmapNotify from #x%x" (slot-value obj 'window))
      (exwm-manage--unmanage-window (slot-value obj 'window) t))))

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
