;;; exwm-manage.el --- Window Management Module for  -*- lexical-binding: t -*-
;;;                    EXWM

;; Copyright (C) 2015-2021 Free Software Foundation, Inc.

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

(defgroup exwm-manage nil
  "Manage."
  :version "25.3"
  :group 'exwm)

(defcustom exwm-manage-finish-hook nil
  "Normal hook run after a window is just managed, in the context of the
corresponding buffer."
  :type 'hook)

(defcustom exwm-manage-force-tiling nil
  "Non-nil to force managing all X windows in tiling layout.
You can still make the X windows floating afterwards."
  :type 'boolean)

(defcustom exwm-manage-ping-timeout 3
  "Seconds to wait before killing a client."
  :type 'integer)

(defcustom exwm-manage-configurations nil
  "Per-application configurations.

Configuration options allow to override various default behaviors of EXWM
and only take effect when they are present.  Note for certain options
specifying nil is not exactly the same as leaving them out.  Currently
possible choices:
* floating: Force floating (non-nil) or tiling (nil) on startup.
* x/y/width/height: Override the initial geometry (floating X window only).
* border-width: Override the border width (only visible when floating).
* fullscreen: Force full screen (non-nil) on startup.
* floating-mode-line: `mode-line-format' used when floating.
* tiling-mode-line: `mode-line-format' used when tiling.
* floating-header-line: `header-line-format' used when floating.
* tiling-header-line: `header-line-format' used when tiling.
* char-mode: Force char-mode (non-nil) on startup.
* prefix-keys: `exwm-input-prefix-keys' local to this X window.
* simulation-keys: `exwm-input-simulation-keys' local to this X window.
* workspace: The initial workspace.
* managed: Force to manage (non-nil) or not manage (nil) the X window.

For each X window managed for the first time, matching criteria (sexps) are
evaluated sequentially and the first configuration with a non-nil matching
criterion would be applied.  Apart from generic forms, one would typically
want to match against EXWM internal variables such as `exwm-title',
`exwm-class-name' and `exwm-instance-name'."
  :type '(alist :key-type (sexp :tag "Matching criterion" nil)
                :value-type
                (plist :tag "Configurations"
                       :options
                       (((const :tag "Floating" floating) boolean)
                        ((const :tag "X" x) number)
                        ((const :tag "Y" y) number)
                        ((const :tag "Width" width) number)
                        ((const :tag "Height" height) number)
                        ((const :tag "Border width" border-width) integer)
                        ((const :tag "Fullscreen" fullscreen) boolean)
                        ((const :tag "Floating mode-line" floating-mode-line)
                         sexp)
                        ((const :tag "Tiling mode-line" tiling-mode-line) sexp)
                        ((const :tag "Floating header-line"
                                floating-header-line)
                         sexp)
                        ((const :tag "Tiling header-line" tiling-header-line)
                         sexp)
                        ((const :tag "Char-mode" char-mode) boolean)
                        ((const :tag "Prefix keys" prefix-keys)
                         (repeat key-sequence))
                        ((const :tag "Simulation keys" simulation-keys)
                         (alist :key-type (key-sequence :tag "From")
                                :value-type (key-sequence :tag "To")))
                        ((const :tag "Workspace" workspace) integer)
                        ((const :tag "Managed" managed) boolean)
                        ;; For forward compatibility.
                        ((other) sexp))))
  ;; TODO: This is admittedly ugly.  We'd be better off with an event type.
  :get (lambda (symbol)
         (mapcar (lambda (pair)
                   (let* ((match (car pair))
                          (config (cdr pair))
                          (prefix-keys (plist-get config 'prefix-keys)))
                     (when prefix-keys
                       (setq config (copy-tree config)
                             config (plist-put config 'prefix-keys
                                               (mapcar (lambda (i)
                                                         (if (sequencep i)
                                                             i
                                                           (vector i)))
                                                       prefix-keys))))
                     (cons match config)))
                 (default-value symbol)))
  :set (lambda (symbol value)
         (set symbol
              (mapcar (lambda (pair)
                        (let* ((match (car pair))
                               (config (cdr pair))
                               (prefix-keys (plist-get config 'prefix-keys)))
                          (when prefix-keys
                            (setq config (copy-tree config)
                                  config (plist-put config 'prefix-keys
                                                    (mapcar (lambda (i)
                                                              (if (sequencep i)
                                                                  (aref i 0)
                                                                i))
                                                            prefix-keys))))
                          (cons match config)))
                      value))))

;; FIXME: Make the following values as small as possible.
(defconst exwm-manage--height-delta-min 5)
(defconst exwm-manage--width-delta-min 5)

;; The _MOTIF_WM_HINTS atom (see <Xm/MwmUtil.h> for more details)
;; It's currently only used in 'exwm-manage' module
(defvar exwm-manage--_MOTIF_WM_HINTS nil "_MOTIF_WM_HINTS atom.")

(defvar exwm-manage--desktop nil "The desktop X window.")

(defvar exwm-manage--frame-outer-id-list nil
  "List of window-outer-id's of all frames.")

(defvar exwm-manage--ping-lock nil
  "Non-nil indicates EXWM is pinging a window.")

(defvar exwm-input-prefix-keys)
(defvar exwm-workspace--current)
(defvar exwm-workspace--id-struts-alist)
(defvar exwm-workspace--list)
(defvar exwm-workspace--switch-history-outdated)
(defvar exwm-workspace--workareas)
(defvar exwm-workspace-current-index)
(declare-function exwm--update-class "exwm.el" (id &optional force))
(declare-function exwm--update-hints "exwm.el" (id &optional force))
(declare-function exwm--update-normal-hints "exwm.el" (id &optional force))
(declare-function exwm--update-protocols "exwm.el" (id &optional force))
(declare-function exwm--update-struts "exwm.el" (id))
(declare-function exwm--update-title "exwm.el" (id))
(declare-function exwm--update-transient-for "exwm.el" (id &optional force))
(declare-function exwm--update-desktop "exwm.el" (id &optional force))
(declare-function exwm--update-window-type "exwm.el" (id &optional force))
(declare-function exwm-floating--set-floating "exwm-floating.el" (id))
(declare-function exwm-floating--unset-floating "exwm-floating.el" (id))
(declare-function exwm-input-grab-keyboard "exwm-input.el")
(declare-function exwm-input-set-local-simulation-keys "exwm-input.el")
(declare-function exwm-layout--fullscreen-p "exwm-layout.el" ())
(declare-function exwm-layout--iconic-state-p "exwm-layout.el" (&optional id))
(declare-function exwm-workspace--position "exwm-workspace.el" (frame))
(declare-function exwm-workspace--set-fullscreen "exwm-workspace.el" (frame))
(declare-function exwm-workspace--update-struts "exwm-workspace.el" ())
(declare-function exwm-workspace--update-workareas "exwm-workspace.el" ())

(defun exwm-manage--update-geometry (id &optional force)
  "Update window geometry."
  (exwm--log "id=#x%x" id)
  (with-current-buffer (exwm--id->buffer id)
    (unless (and exwm--geometry (not force))
      (let ((reply (xcb:+request-unchecked+reply exwm--connection
                       (make-instance 'xcb:GetGeometry :drawable id))))
        (setq exwm--geometry
              (or reply
                  ;; Provide a reasonable fallback value.
                  (make-instance 'xcb:RECTANGLE
                                 :x 0
                                 :y 0
                                 :width (/ (x-display-pixel-width) 2)
                                 :height (/ (x-display-pixel-height) 2))))))))

(defun exwm-manage--update-ewmh-state (id)
  "Update _NET_WM_STATE."
  (exwm--log "id=#x%x" id)
  (with-current-buffer (exwm--id->buffer id)
    (unless exwm--ewmh-state
      (let ((reply (xcb:+request-unchecked+reply exwm--connection
                       (make-instance 'xcb:ewmh:get-_NET_WM_STATE
                                      :window id))))
        (when reply
          (setq exwm--ewmh-state (append (slot-value reply 'value) nil)))))))

(defun exwm-manage--update-mwm-hints (id &optional force)
  "Update _MOTIF_WM_HINTS."
  (exwm--log "id=#x%x" id)
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
  (exwm--log)
  (xcb:+request exwm--connection
      (make-instance 'xcb:ewmh:set-_NET_CLIENT_LIST
                     :window exwm--root
                     :data (vconcat (mapcar #'car exwm--id-buffer-alist)))))

(cl-defun exwm-manage--get-configurations ()
  "Retrieve configurations for this buffer."
  (exwm--log)
  (when (derived-mode-p 'exwm-mode)
    (dolist (i exwm-manage-configurations)
      (save-current-buffer
        (when (with-demoted-errors "Problematic configuration: %S"
                (eval (car i) t))
          (cl-return-from exwm-manage--get-configurations (cdr i)))))))

(defun exwm-manage--manage-window (id)
  "Manage window ID."
  (exwm--log "Try to manage #x%x" id)
  (catch 'return
    ;; Ensure it's alive
    (when (xcb:+request-checked+request-check exwm--connection
              (make-instance 'xcb:ChangeWindowAttributes
                             :window id :value-mask xcb:CW:EventMask
                             :event-mask (exwm--get-client-event-mask)))
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
      (setq exwm--id id
            exwm--frame exwm-workspace--current)
      (exwm--update-window-type id)
      (exwm--update-class id)
      (exwm--update-transient-for id)
      (exwm--update-normal-hints id)
      (exwm--update-hints id)
      (exwm-manage--update-geometry id)
      (exwm-manage--update-mwm-hints id)
      (exwm--update-title id)
      (exwm--update-protocols id)
      (setq exwm--configurations (exwm-manage--get-configurations))
      ;; OverrideRedirect is not checked here.
      (when (and
             ;; The user has specified to manage it.
             (not (plist-get exwm--configurations 'managed))
             (or
              ;; The user has specified not to manage it.
              (plist-member exwm--configurations 'managed)
              ;; This is not a type of X window we can manage.
              (and exwm-window-type
                   (not (cl-intersection
                         exwm-window-type
                         (list xcb:Atom:_NET_WM_WINDOW_TYPE_UTILITY
                               xcb:Atom:_NET_WM_WINDOW_TYPE_DIALOG
                               xcb:Atom:_NET_WM_WINDOW_TYPE_NORMAL))))
              ;; Check the _MOTIF_WM_HINTS property to not manage floating X
              ;; windows without decoration.
              (and (not exwm--mwm-hints-decorations)
                   (not exwm--hints-input)
                   ;; Floating windows only
                   (or exwm-transient-for exwm--fixed-size
                       (memq xcb:Atom:_NET_WM_WINDOW_TYPE_UTILITY
                             exwm-window-type)
                       (memq xcb:Atom:_NET_WM_WINDOW_TYPE_DIALOG
                             exwm-window-type)))))
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
                               (exwm--get-client-event-mask)
                             xcb:EventMask:NoEvent)))
        ;; The window needs to be mapped
        (xcb:+request exwm--connection
            (make-instance 'xcb:MapWindow :window id))
        (with-slots (x y width height) exwm--geometry
          ;; Center window of type _NET_WM_WINDOW_TYPE_SPLASH
          (when (memq xcb:Atom:_NET_WM_WINDOW_TYPE_SPLASH exwm-window-type)
            (let* ((workarea (elt exwm-workspace--workareas
                                  (exwm-workspace--position exwm--frame)))
                   (x* (aref workarea 0))
                   (y* (aref workarea 1))
                   (width* (aref workarea 2))
                   (height* (aref workarea 3)))
              (exwm--set-geometry id
                                  (+ x* (/ (- width* width) 2))
                                  (+ y* (/ (- height* height) 2))
                                  nil
                                  nil))))
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
      (let ((index (plist-get exwm--configurations 'workspace)))
        (when (and index (< index (length exwm-workspace--list)))
          (setq exwm--frame (elt exwm-workspace--list index))))
      ;; Manage the window
      (exwm--log "Manage #x%x" id)
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
      (if (plist-member exwm--configurations 'floating)
          ;; User has specified whether it should be floating.
          (if (plist-get exwm--configurations 'floating)
              (exwm-floating--set-floating id)
            (with-selected-window (frame-selected-window exwm--frame)
              (exwm-floating--unset-floating id)))
        ;; Try to determine if it should be floating.
        (if (and (not exwm-manage-force-tiling)
                 (or exwm-transient-for exwm--fixed-size
                     (memq xcb:Atom:_NET_WM_WINDOW_TYPE_UTILITY
                           exwm-window-type)
                     (memq xcb:Atom:_NET_WM_WINDOW_TYPE_DIALOG
                           exwm-window-type)))
            (exwm-floating--set-floating id)
          (with-selected-window (frame-selected-window exwm--frame)
            (exwm-floating--unset-floating id))))
      (if (plist-get exwm--configurations 'char-mode)
          (exwm-input-release-keyboard id)
        (exwm-input-grab-keyboard id))
      (let ((simulation-keys (plist-get exwm--configurations 'simulation-keys))
            (prefix-keys (plist-get exwm--configurations 'prefix-keys)))
        (with-current-buffer (exwm--id->buffer id)
          (when simulation-keys
            (exwm-input-set-local-simulation-keys simulation-keys))
          (when prefix-keys
            (setq-local exwm-input-prefix-keys prefix-keys))))
      (setq exwm-workspace--switch-history-outdated t)
      (exwm--update-desktop id)
      (exwm-manage--update-ewmh-state id)
      (with-current-buffer (exwm--id->buffer id)
        (when (or (plist-get exwm--configurations 'fullscreen)
                  (exwm-layout--fullscreen-p))
          (setq exwm--ewmh-state (delq xcb:Atom:_NET_WM_STATE_FULLSCREEN
                                       exwm--ewmh-state))
          (exwm-layout-set-fullscreen id))
        (run-hooks 'exwm-manage-finish-hook)))))

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
        ;; Unmap the X window.
        (xcb:+request exwm--connection
            (make-instance 'xcb:UnmapWindow :window id))
        ;;
        (setq exwm-workspace--switch-history-outdated t)
        ;;
        (when withdraw-only
          (xcb:+request exwm--connection
              (make-instance 'xcb:ChangeWindowAttributes
                             :window id :value-mask xcb:CW:EventMask
                             :event-mask xcb:EventMask:NoEvent))
          ;; Delete WM_STATE property
          (xcb:+request exwm--connection
              (make-instance 'xcb:DeleteProperty
                             :window id :property xcb:Atom:WM_STATE))
          (cond
           ((eq withdraw-only 'quit)
            ;; Remap the window when exiting.
            (xcb:+request exwm--connection
                (make-instance 'xcb:MapWindow :window id)))
           (t
            ;; Remove _NET_WM_DESKTOP.
            (xcb:+request exwm--connection
                (make-instance 'xcb:DeleteProperty
                               :window id
                               :property xcb:Atom:_NET_WM_DESKTOP)))))
        (when exwm--floating-frame
          ;; Unmap the floating frame before destroying its container.
          (let ((window (frame-parameter exwm--floating-frame 'exwm-outer-id))
                (container (frame-parameter exwm--floating-frame
                                            'exwm-container)))
            (xcb:+request exwm--connection
                (make-instance 'xcb:UnmapWindow :window window))
            (xcb:+request exwm--connection
                (make-instance 'xcb:ReparentWindow
                               :window window :parent exwm--root :x 0 :y 0))
            (xcb:+request exwm--connection
                (make-instance 'xcb:DestroyWindow :window container))))
        (when (exwm-layout--fullscreen-p)
          (let ((window (get-buffer-window)))
            (when window
              (set-window-dedicated-p window nil))))
        (exwm-manage--set-client-list)
        (xcb:flush exwm--connection))
      (let ((kill-buffer-func
             (lambda (buffer)
               (when (buffer-local-value 'exwm--floating-frame buffer)
                 (select-window
                  (frame-selected-window exwm-workspace--current)))
               (with-current-buffer buffer
                 (let ((kill-buffer-query-functions nil))
                   (kill-buffer buffer))))))
        (exwm--defer 0 kill-buffer-func buffer)
        (when (active-minibuffer-window)
          (exit-minibuffer))))))

(defun exwm-manage--scan ()
  "Search for existing windows and try to manage them."
  (exwm--log)
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

(defun exwm-manage--kill-buffer-query-function ()
  "Run in `kill-buffer-query-functions'."
  (exwm--log "id=#x%x; buffer=%s" exwm--id (current-buffer))
  (catch 'return
    (when (or (not exwm--id)
              (xcb:+request-checked+request-check exwm--connection
                  (make-instance 'xcb:ChangeWindowAttributes
                                 :window exwm--id
                                 :value-mask xcb:CW:EventMask
                                 :event-mask (exwm--get-client-event-mask))))
      ;; The X window is no longer alive so just close the buffer.
      (when exwm--floating-frame
        (let ((window (frame-parameter exwm--floating-frame 'exwm-outer-id))
              (container (frame-parameter exwm--floating-frame
                                          'exwm-container)))
          (xcb:+request exwm--connection
              (make-instance 'xcb:UnmapWindow :window window))
          (xcb:+request exwm--connection
              (make-instance 'xcb:ReparentWindow
                             :window window
                             :parent exwm--root
                             :x 0 :y 0))
          (xcb:+request exwm--connection
              (make-instance 'xcb:DestroyWindow
                             :window container))))
      (xcb:flush exwm--connection)
      (throw 'return t))
    (unless (memq xcb:Atom:WM_DELETE_WINDOW exwm--protocols)
      ;; The X window does not support WM_DELETE_WINDOW; destroy it.
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
  (exwm--log "id=#x%x" id)
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
                      (lambda ()
                        (xcb:+request exwm--connection request))))
    (xcb:flush exwm--connection)))

(defun exwm-manage--add-frame (frame)
  "Run in `after-make-frame-functions'."
  (exwm--log "frame=%s" frame)
  (when (display-graphic-p frame)
    (push (string-to-number (frame-parameter frame 'outer-window-id))
          exwm-manage--frame-outer-id-list)))

(defun exwm-manage--remove-frame (frame)
  "Run in `delete-frame-functions'."
  (exwm--log "frame=%s" frame)
  (when (display-graphic-p frame)
    (setq exwm-manage--frame-outer-id-list
          (delq (string-to-number (frame-parameter frame 'outer-window-id))
                exwm-manage--frame-outer-id-list))))

(defun exwm-manage--on-ConfigureRequest (data _synthetic)
  "Handle ConfigureRequest event."
  (exwm--log)
  (let ((obj (make-instance 'xcb:ConfigureRequest))
        buffer edges width-delta height-delta)
    (xcb:unmarshal obj data)
    (with-slots (window x y width height
                        border-width sibling stack-mode value-mask)
        obj
      (exwm--log "#x%x (#x%x) @%dx%d%+d%+d; \
border-width: %d; sibling: #x%x; stack-mode: %d"
                 window value-mask width height x y
                 border-width sibling stack-mode)
      (if (and (setq buffer (exwm--id->buffer window))
               (with-current-buffer buffer
                 (or (exwm-layout--fullscreen-p)
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
                  (if (exwm-layout--fullscreen-p)
                      (with-slots (x y width height)
                          (exwm-workspace--get-geometry exwm--frame)
                        (list x y width height))
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
              (exwm--set-geometry (frame-parameter exwm--floating-frame
                                                   'exwm-outer-id)
                                  nil
                                  nil
                                  (+ (frame-pixel-width exwm--floating-frame)
                                     width-delta)
                                  (+ (frame-pixel-height exwm--floating-frame)
                                     height-delta)))
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

(defun exwm-manage--on-MapRequest (data _synthetic)
  "Handle MapRequest event."
  (let ((obj (make-instance 'xcb:MapRequest)))
    (xcb:unmarshal obj data)
    (with-slots (parent window) obj
      (exwm--log "id=#x%x parent=#x%x" window parent)
      (if (assoc window exwm--id-buffer-alist)
          (with-current-buffer (exwm--id->buffer window)
            (if (exwm-layout--iconic-state-p)
                ;; State change: iconic => normal.
                (when (eq exwm--frame exwm-workspace--current)
                  (pop-to-buffer-same-window (current-buffer)))
              (exwm--log "#x%x is already managed" window)))
        (if (/= exwm--root parent)
            (progn (xcb:+request exwm--connection
                       (make-instance 'xcb:MapWindow :window window))
                   (xcb:flush exwm--connection))
          (exwm--log "#x%x" window)
          (exwm-manage--manage-window window))))))

(defun exwm-manage--on-UnmapNotify (data _synthetic)
  "Handle UnmapNotify event."
  (let ((obj (make-instance 'xcb:UnmapNotify)))
    (xcb:unmarshal obj data)
    (with-slots (window) obj
      (exwm--log "id=#x%x" window)
      (exwm-manage--unmanage-window window t))))

(defun exwm-manage--on-MapNotify (data _synthetic)
  "Handle MapNotify event."
  (let ((obj (make-instance 'xcb:MapNotify)))
    (xcb:unmarshal obj data)
    (with-slots (window) obj
      (when (assoc window exwm--id-buffer-alist)
        (exwm--log "id=#x%x" window)
        ;; With this we ensure that a "window hierarchy change" happens after
        ;; mapping the window, as some servers (XQuartz) do not generate it.
        (with-current-buffer (exwm--id->buffer window)
          (if exwm--floating-frame
              (xcb:+request exwm--connection
                  (make-instance 'xcb:ConfigureWindow
                                 :window window
                                 :value-mask xcb:ConfigWindow:StackMode
                                 :stack-mode xcb:StackMode:Above))
            (xcb:+request exwm--connection
                (make-instance 'xcb:ConfigureWindow
                               :window window
                               :value-mask (logior xcb:ConfigWindow:Sibling
                                                   xcb:ConfigWindow:StackMode)
                               :sibling exwm--guide-window
                               :stack-mode xcb:StackMode:Above))))
        (xcb:flush exwm--connection)))))

(defun exwm-manage--on-DestroyNotify (data synthetic)
  "Handle DestroyNotify event."
  (unless synthetic
    (exwm--log)
    (let ((obj (make-instance 'xcb:DestroyNotify)))
      (xcb:unmarshal obj data)
      (exwm--log "#x%x" (slot-value obj 'window))
      (exwm-manage--unmanage-window (slot-value obj 'window)))))

(defun exwm-manage--init ()
  "Initialize manage module."
  ;; Intern _MOTIF_WM_HINTS
  (exwm--log)
  (setq exwm-manage--_MOTIF_WM_HINTS (exwm--intern-atom "_MOTIF_WM_HINTS"))
  (add-hook 'after-make-frame-functions #'exwm-manage--add-frame)
  (add-hook 'delete-frame-functions #'exwm-manage--remove-frame)
  (xcb:+event exwm--connection 'xcb:ConfigureRequest
              #'exwm-manage--on-ConfigureRequest)
  (xcb:+event exwm--connection 'xcb:MapRequest #'exwm-manage--on-MapRequest)
  (xcb:+event exwm--connection 'xcb:UnmapNotify #'exwm-manage--on-UnmapNotify)
  (xcb:+event exwm--connection 'xcb:MapNotify #'exwm-manage--on-MapNotify)
  (xcb:+event exwm--connection 'xcb:DestroyNotify
              #'exwm-manage--on-DestroyNotify))

(defun exwm-manage--exit ()
  "Exit the manage module."
  (exwm--log)
  (dolist (pair exwm--id-buffer-alist)
    (exwm-manage--unmanage-window (car pair) 'quit))
  (remove-hook 'after-make-frame-functions #'exwm-manage--add-frame)
  (remove-hook 'delete-frame-functions #'exwm-manage--remove-frame)
  (setq exwm-manage--_MOTIF_WM_HINTS nil))



(provide 'exwm-manage)

;;; exwm-manage.el ends here
