;;; exwm.el --- Emacs X Window Manager  -*- lexical-binding: t -*-

;; Copyright (C) 2015-2016 Free Software Foundation, Inc.

;; Author: Chris Feng <chris.w.feng@gmail.com>
;; Maintainer: Chris Feng <chris.w.feng@gmail.com>
;; Version: 0.4
;; Package-Requires: ((xelb "0.6"))
;; Keywords: unix
;; URL: https://github.com/ch11ng/exwm

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

;; Overview
;; --------
;; EXWM (Emacs X Window Manager) is a full-featured tiling X window manager for
;; Emacs built on top of XELB.  It features:
;; + Fully keyboard-driven operations
;; + Hybrid layout modes (tiling & stacking)
;; + Workspace support
;; + ICCCM/EWMH compliance
;; ++ (Optional) RandR (multi-monitor) support
;; ++ (Optional) system tray

;; Installation & configuration
;; ----------------------------
;; Here are the minimal steps to get EXWM working:
;; 1. Install XELB and EXWM, and make sure they are in `load-path'.
;; 2. In '~/.emacs', add following lines (please modify accordingly):
;;
;;    (require 'exwm)
;;    (require 'exwm-config)
;;    (exwm-config-default)
;;
;; 3. Link or copy the file 'xinitrc' to '~/.xinitrc'.
;; 4. Launch EXWM in a console (e.g. tty1) with
;;
;;    xinit -- vt01
;;
;; You should additionally hide the menu-bar, tool-bar, etc to increase the
;; usable space.  Please check the wiki (https://github.com/ch11ng/exwm/wiki)
;; for more detailed instructions on installation, configuration, usage, etc.

;; References:
;; + dwm (http://dwm.suckless.org/)
;; + i3 wm (https://i3wm.org/)
;; + Also see references within each required library.

;;; Code:

(require 'server)
(require 'exwm-core)
(require 'exwm-workspace)
(require 'exwm-layout)
(require 'exwm-floating)
(require 'exwm-manage)
(require 'exwm-input)

;;;###autoload
(defun exwm-reset ()
  "Reset window to standard state: non-fullscreen, line-mode."
  (interactive)
  (with-current-buffer (window-buffer)
    (when (eq major-mode 'exwm-mode)
      (when exwm--fullscreen (exwm-layout-unset-fullscreen))
      ;; Force refresh
      (exwm-layout--refresh)
      (exwm-input-grab-keyboard))))

(defun exwm--update-window-type (id &optional force)
  "Update _NET_WM_WINDOW_TYPE."
  (with-current-buffer (exwm--id->buffer id)
    (unless (and exwm-window-type (not force))
      (let ((reply (xcb:+request-unchecked+reply exwm--connection
                       (make-instance 'xcb:ewmh:get-_NET_WM_WINDOW_TYPE
                                      :window id))))
        (when reply                     ;nil when destroyed
          (setq exwm-window-type (append (slot-value reply 'value) nil)))))))

(defvar exwm-update-class-hook nil
  "Normal hook run when window class is updated.")

(defun exwm--update-class (id &optional force)
  "Update WM_CLASS."
  (with-current-buffer (exwm--id->buffer id)
    (unless (and exwm-instance-name exwm-class-name (not force))
      (let ((reply (xcb:+request-unchecked+reply exwm--connection
                       (make-instance 'xcb:icccm:get-WM_CLASS :window id))))
        (when reply                     ;nil when destroyed
          (setq exwm-instance-name (slot-value reply 'instance-name)
                exwm-class-name (slot-value reply 'class-name))
          (when (and exwm-instance-name exwm-class-name)
            (run-hooks 'exwm-update-class-hook)))))))

(defvar exwm-update-title-hook nil
  "Normal hook run when window title is updated.")

(defun exwm--update-utf8-title (id &optional force)
  "Update _NET_WM_NAME."
  (with-current-buffer (exwm--id->buffer id)
    (when (or force (not exwm-title))
      (let ((reply (xcb:+request-unchecked+reply exwm--connection
                       (make-instance 'xcb:ewmh:get-_NET_WM_NAME :window id))))
        (when reply                     ;nil when destroyed
          (setq exwm-title (slot-value reply 'value))
          (when exwm-title
            (setq exwm--title-is-utf8 t)
            (run-hooks 'exwm-update-title-hook)))))))

(defun exwm--update-ctext-title (id &optional force)
  "Update WM_NAME."
  (with-current-buffer (exwm--id->buffer id)
    (unless (or exwm--title-is-utf8
                (and exwm-title (not force)))
      (let ((reply (xcb:+request-unchecked+reply exwm--connection
                       (make-instance 'xcb:icccm:get-WM_NAME :window id))))
        (when reply                     ;nil when destroyed
          (setq exwm-title (slot-value reply 'value))
          (when exwm-title
            (run-hooks 'exwm-update-title-hook)))))))

(defun exwm--update-title (id)
  "Update _NET_WM_NAME or WM_NAME."
  (exwm--update-utf8-title id)
  (exwm--update-ctext-title id))

(defun exwm--update-transient-for (id &optional force)
  "Update WM_TRANSIENT_FOR."
  (with-current-buffer (exwm--id->buffer id)
    (unless (and exwm-transient-for (not force))
      (let ((reply (xcb:+request-unchecked+reply exwm--connection
                       (make-instance 'xcb:icccm:get-WM_TRANSIENT_FOR
                                      :window id))))
        (when reply                     ;nil when destroyed
          (setq exwm-transient-for (slot-value reply 'value)))))))

(defun exwm--update-normal-hints (id &optional force)
  "Update WM_NORMAL_HINTS."
  (with-current-buffer (exwm--id->buffer id)
    (unless (and (not force)
                 (or exwm--normal-hints-x exwm--normal-hints-y
                     exwm--normal-hints-width exwm--normal-hints-height
                     exwm--normal-hints-min-width exwm--normal-hints-min-height
                     exwm--normal-hints-max-width exwm--normal-hints-max-height
                     ;; FIXME: other fields
                     ))
      (let ((reply (xcb:+request-unchecked+reply exwm--connection
                       (make-instance 'xcb:icccm:get-WM_NORMAL_HINTS
                                      :window id))))
        (when (and reply (slot-value reply 'flags)) ;nil when destroyed
          (with-slots (flags x y width height min-width min-height max-width
                             max-height base-width base-height ;; win-gravity
                             )
              reply
            (unless (= 0 (logand flags xcb:icccm:WM_SIZE_HINTS:USPosition))
              (setq exwm--normal-hints-x x exwm--normal-hints-y y))
            (unless (= 0 (logand flags xcb:icccm:WM_SIZE_HINTS:USSize))
              (setq exwm--normal-hints-width width
                    exwm--normal-hints-height height))
            (unless (= 0 (logand flags xcb:icccm:WM_SIZE_HINTS:PMinSize))
              (setq exwm--normal-hints-min-width min-width
                    exwm--normal-hints-min-height min-height))
            (unless (= 0 (logand flags xcb:icccm:WM_SIZE_HINTS:PMaxSize))
              (setq exwm--normal-hints-max-width max-width
                    exwm--normal-hints-max-height max-height))
            (unless (or exwm--normal-hints-min-width
                        (= 0 (logand flags xcb:icccm:WM_SIZE_HINTS:PBaseSize)))
              (setq exwm--normal-hints-min-width base-width
                    exwm--normal-hints-min-height base-height))
            ;; (unless (= 0 (logand flags xcb:icccm:WM_SIZE_HINTS:PWinGravity))
            ;;   (setq exwm--normal-hints-win-gravity win-gravity))
            (setq exwm--fixed-size
                  (and exwm--normal-hints-min-width
                       exwm--normal-hints-min-height
                       exwm--normal-hints-max-width
                       exwm--normal-hints-max-height
                       (/= 0 exwm--normal-hints-min-width)
                       (/= 0 exwm--normal-hints-min-height)
                       (= exwm--normal-hints-min-width
                          exwm--normal-hints-max-width)
                       (= exwm--normal-hints-min-height
                          exwm--normal-hints-max-height)))))))))

(defun exwm--update-hints (id &optional force)
  "Update WM_HINTS."
  (with-current-buffer (exwm--id->buffer id)
    (unless (and (not force) exwm--hints-input exwm--hints-urgency)
      (let ((reply (xcb:+request-unchecked+reply exwm--connection
                       (make-instance 'xcb:icccm:get-WM_HINTS :window id))))
        (when (and reply (slot-value reply 'flags)) ;nil when destroyed
          (with-slots (flags input) reply
            (when flags
              (unless (= 0 (logand flags xcb:icccm:WM_HINTS:InputHint))
                (setq exwm--hints-input (when input (= 1 input))))
              (unless (= 0 (logand flags xcb:icccm:WM_HINTS:UrgencyHint))
                (setq exwm--hints-urgency t))))
          (when (and exwm--hints-urgency
                     (not (eq exwm--frame exwm-workspace--current)))
            (unless (frame-parameter exwm--frame 'exwm--urgency)
              (set-frame-parameter exwm--frame 'exwm--urgency t)
              (setq exwm-workspace--switch-history-outdated t))))))))

(defun exwm--update-protocols (id &optional force)
  "Update WM_PROTOCOLS."
  (with-current-buffer (exwm--id->buffer id)
    (unless (and exwm--protocols (not force))
      (let ((reply (xcb:+request-unchecked+reply exwm--connection
                       (make-instance 'xcb:icccm:get-WM_PROTOCOLS
                                      :window id))))
        (when reply                     ;nil when destroyed
          (setq exwm--protocols (append (slot-value reply 'value) nil)))))))

(defun exwm--update-state (id &optional force)
  "Update WM_STATE."
  (with-current-buffer (exwm--id->buffer id)
    (unless (and exwm-state (not force))
      (let ((reply (xcb:+request-unchecked+reply exwm--connection
                       (make-instance 'xcb:icccm:get-WM_STATE :window id))))
        (when reply                     ;nil when destroyed
          (setq exwm-state (or (slot-value reply 'state)
                               ;; Default to normal state
                               xcb:icccm:WM_STATE:NormalState)))))))

(defun exwm--on-PropertyNotify (data _synthetic)
  "Handle PropertyNotify event."
  (let ((obj (make-instance 'xcb:PropertyNotify))
        atom id buffer)
    (xcb:unmarshal obj data)
    (setq id (slot-value obj 'window)
          atom (slot-value obj 'atom)
          exwm-input--timestamp (slot-value obj 'time))
    (setq buffer (exwm--id->buffer id))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (cond ((= atom xcb:Atom:_NET_WM_WINDOW_TYPE)
               (exwm--update-window-type id t))
              ((= atom xcb:Atom:WM_CLASS)
               (exwm--update-class id t))
              ((= atom xcb:Atom:_NET_WM_NAME)
               (exwm--update-utf8-title id t))
              ((= atom xcb:Atom:WM_NAME)
               (exwm--update-ctext-title id t))
              ((= atom xcb:Atom:WM_TRANSIENT_FOR)
               (exwm--update-transient-for id t))
              ((= atom xcb:Atom:WM_NORMAL_HINTS)
               (exwm--update-normal-hints id t))
              ((= atom xcb:Atom:WM_HINTS)
               (exwm--update-hints id t))
              ((= atom xcb:Atom:WM_PROTOCOLS)
               (exwm--update-protocols id t))
              ((= atom xcb:Atom:WM_STATE)
               (exwm--update-state id t))
              ((= atom xcb:Atom:_NET_WM_USER_TIME)) ;ignored
              (t (exwm--log "Unhandled PropertyNotify: %s(%d)"
                            (x-get-atom-name atom exwm-workspace--current)
                            atom)))))))

(defun exwm--on-ClientMessage (raw-data _synthetic)
  "Handle ClientMessage event."
  (let ((obj (make-instance 'xcb:ClientMessage))
        type id data)
    (xcb:unmarshal obj raw-data)
    (setq type (slot-value obj 'type)
          id (slot-value obj 'window)
          data (slot-value (slot-value obj 'data) 'data32))
    (cond
     ;; _NET_WM_MOVERESIZE
     ((= type xcb:Atom:_NET_WM_MOVERESIZE)
      (let ((direction (elt data 2))
            (buffer (exwm--id->buffer id)))
        (unless (and buffer (with-current-buffer buffer
                              (not exwm--floating-frame)))
          (cond ((= direction
                    xcb:ewmh:_NET_WM_MOVERESIZE_SIZE_KEYBOARD)
                 ;; FIXME
                 )
                ((= direction
                    xcb:ewmh:_NET_WM_MOVERESIZE_MOVE_KEYBOARD)
                 ;; FIXME
                 )
                ((= direction xcb:ewmh:_NET_WM_MOVERESIZE_CANCEL)
                 (exwm-floating--stop-moveresize))
                (t (exwm-floating--start-moveresize id direction))))))
     ;; _NET_REQUEST_FRAME_EXTENTS
     ((= type xcb:Atom:_NET_REQUEST_FRAME_EXTENTS)
      (let ((buffer (exwm--id->buffer id))
            left right top btm)
        (if (or (not buffer)
                (with-current-buffer buffer
                  (not exwm--floating-frame)))
            (setq left 0 right 0 top 0 btm 0)
          (setq left exwm-floating-border-width
                right exwm-floating-border-width
                top (+ exwm-floating-border-width (window-header-line-height))
                btm (+ exwm-floating-border-width
                       (window-mode-line-height))))
        (xcb:+request exwm--connection
            (make-instance 'xcb:ewmh:set-_NET_FRAME_EXTENTS
                           :window id :left left :right right
                           :top top :bottom btm)))
      (xcb:flush exwm--connection))
     ;; _NET_WM_STATE
     ((= type xcb:Atom:_NET_WM_STATE)
      (let ((action (elt data 0))
            (props (list (elt data 1) (elt data 2)))
            (buffer (exwm--id->buffer id))
            props-new)
        ;; only support _NET_WM_STATE_FULLSCREEN / _NET_WM_STATE_ADD for frames
        (when (and (not buffer)
                   (memq xcb:Atom:_NET_WM_STATE_FULLSCREEN props)
                   (= action xcb:ewmh:_NET_WM_STATE_ADD))
          (dolist (f exwm-workspace--list)
            (when (equal (frame-parameter f 'exwm-outer-id) id)
	      (exwm-layout--set-frame-fullscreen f)
	      (xcb:+request
                  exwm--connection
                  (make-instance 'xcb:ewmh:set-_NET_WM_STATE
                                 :window id
                                 :data (vector
                                        xcb:Atom:_NET_WM_STATE_FULLSCREEN)))
	      (xcb:flush exwm--connection))))
        (when buffer                    ;ensure it's managed
          (with-current-buffer buffer
            ;; _NET_WM_STATE_MODAL
            (when (memq xcb:Atom:_NET_WM_STATE_MODAL props)
              (cond ((= action xcb:ewmh:_NET_WM_STATE_ADD)
                     (unless exwm--floating-frame
                       (exwm-floating--set-floating id))
                     (push xcb:Atom:_NET_WM_STATE_MODAL props-new))
                    ((= action xcb:ewmh:_NET_WM_STATE_REMOVE)
                     (when exwm--floating-frame
                       (exwm-floating--unset-floating id)))
                    ((= action xcb:ewmh:_NET_WM_STATE_TOGGLE)
                     (if exwm--floating-frame
                         (exwm-floating--unset-floating id)
                       (exwm-floating--set-floating id)
                       (push xcb:Atom:_NET_WM_STATE_MODAL props-new)))))
            ;; _NET_WM_STATE_FULLSCREEN
            (when (or (memq xcb:Atom:_NET_WM_STATE_FULLSCREEN props)
                      (memq xcb:Atom:_NET_WM_STATE_ABOVE props))
              (cond ((= action xcb:ewmh:_NET_WM_STATE_ADD)
                     (unless exwm--fullscreen (exwm-layout-set-fullscreen id))
                     (push xcb:Atom:_NET_WM_STATE_FULLSCREEN props-new))
                    ((= action xcb:ewmh:_NET_WM_STATE_REMOVE)
                     (when exwm--fullscreen (exwm-layout-unset-fullscreen id)))
                    ((= action xcb:ewmh:_NET_WM_STATE_TOGGLE)
                     (if exwm--fullscreen
                         (exwm-layout-unset-fullscreen id)
                       (exwm-layout-set-fullscreen id)
                       (push xcb:Atom:_NET_WM_STATE_FULLSCREEN props-new)))))
            ;; _NET_WM_STATE_DEMANDS_ATTENTION
            ;; FIXME: check (may require other properties set)
            (when (memq xcb:Atom:_NET_WM_STATE_DEMANDS_ATTENTION props)
              (when (= action xcb:ewmh:_NET_WM_STATE_ADD)
                (let ((idx (cl-position exwm--frame exwm-workspace--list)))
                  (unless (= idx exwm-workspace-current-index)
                    (set-frame-parameter exwm--frame 'exwm--urgency t)
                    (setq exwm-workspace--switch-history-outdated t))))
              ;; xcb:ewmh:_NET_WM_STATE_REMOVE?
              ;; xcb:ewmh:_NET_WM_STATE_TOGGLE?
              )
            (xcb:+request exwm--connection
                (make-instance 'xcb:ewmh:set-_NET_WM_STATE
                               :window id :data (vconcat props-new)))
            (xcb:flush exwm--connection)))))
     ((= type xcb:Atom:WM_PROTOCOLS)
      (let ((type (elt data 0)))
        (cond ((= type xcb:Atom:_NET_WM_PING)
               (setq exwm-manage--ping-lock nil))
              (t (exwm--log "Unhandled WM_PROTOCOLS of type: %d" type)))))
     (t (exwm--log "Unhandled client message: %s" obj)))))

(defun exwm--init-icccm-ewmh ()
  "Initialize ICCCM/EWMH support."
  ;; Handle PropertyNotify event
  (xcb:+event exwm--connection 'xcb:PropertyNotify #'exwm--on-PropertyNotify)
  ;; Handle relevant client messages
  ;; FIXME: WM_STATE client messages (normal => iconic)
  ;;        WM_COLORMAP_NOTIFY
  (xcb:+event exwm--connection 'xcb:ClientMessage #'exwm--on-ClientMessage)
  ;; Set _NET_SUPPORTED
  (xcb:+request exwm--connection
      (make-instance 'xcb:ewmh:set-_NET_SUPPORTED
                     :window exwm--root
                     :data (vector xcb:Atom:_NET_SUPPORTED
                                   xcb:Atom:_NET_CLIENT_LIST
                                   xcb:Atom:_NET_CLIENT_LIST_STACKING
                                   xcb:Atom:_NET_NUMBER_OF_DESKTOPS
                                   xcb:Atom:_NET_DESKTOP_VIEWPORT
                                   xcb:Atom:_NET_CURRENT_DESKTOP
                                   xcb:Atom:_NET_WORKAREA
                                   xcb:Atom:_NET_SUPPORTING_WM_CHECK
                                   xcb:Atom:_NET_VIRTUAL_ROOTS
                                   xcb:Atom:_NET_WM_MOVERESIZE
                                   xcb:Atom:_NET_REQUEST_FRAME_EXTENTS
                                   xcb:Atom:_NET_FRAME_EXTENTS
                                   xcb:Atom:_NET_WM_NAME
                                   ;;
                                   xcb:Atom:_NET_WM_WINDOW_TYPE
                                   xcb:Atom:_NET_WM_WINDOW_TYPE_TOOLBAR
                                   xcb:Atom:_NET_WM_WINDOW_TYPE_MENU
                                   xcb:Atom:_NET_WM_WINDOW_TYPE_UTILITY
                                   xcb:Atom:_NET_WM_WINDOW_TYPE_SPLASH
                                   xcb:Atom:_NET_WM_WINDOW_TYPE_DIALOG
                                   xcb:Atom:_NET_WM_WINDOW_TYPE_DROPDOWN_MENU
                                   xcb:Atom:_NET_WM_WINDOW_TYPE_POPUP_MENU
                                   xcb:Atom:_NET_WM_WINDOW_TYPE_TOOLTIP
                                   xcb:Atom:_NET_WM_WINDOW_TYPE_NOTIFICATION
                                   xcb:Atom:_NET_WM_WINDOW_TYPE_COMBO
                                   xcb:Atom:_NET_WM_WINDOW_TYPE_DND
                                   xcb:Atom:_NET_WM_WINDOW_TYPE_NORMAL
                                   ;;
                                   xcb:Atom:_NET_WM_STATE
                                   xcb:Atom:_NET_WM_STATE_MODAL
                                   xcb:Atom:_NET_WM_STATE_FULLSCREEN
                                   xcb:Atom:_NET_WM_STATE_DEMANDS_ATTENTION
                                   ;; FIXME: more?
                                   )))
  ;; Create a child window for setting _NET_SUPPORTING_WM_CHECK
  (let ((new-id (xcb:generate-id exwm--connection)))
    (xcb:+request exwm--connection
        (make-instance 'xcb:CreateWindow
                       :depth 0 :wid new-id :parent exwm--root
                       :x -1 :y -1 :width 1 :height 1
                       :border-width 0 :class xcb:WindowClass:CopyFromParent
                       :visual 0 :value-mask xcb:CW:OverrideRedirect
                       :override-redirect 1))
    (dolist (i (list exwm--root new-id))
      ;; Set _NET_SUPPORTING_WM_CHECK
      (xcb:+request exwm--connection
          (make-instance 'xcb:ewmh:set-_NET_SUPPORTING_WM_CHECK
                         :window i :data new-id))
      ;; Set _NET_WM_NAME
      (xcb:+request exwm--connection
          (make-instance 'xcb:ewmh:set-_NET_WM_NAME
                         :window i :data "EXWM"))))
  ;; Set _NET_NUMBER_OF_DESKTOPS
  (xcb:+request exwm--connection
      (make-instance 'xcb:ewmh:set-_NET_NUMBER_OF_DESKTOPS
                     :window exwm--root :data exwm-workspace-number))
  ;; Set _NET_DESKTOP_VIEWPORT
  (xcb:+request exwm--connection
      (make-instance 'xcb:ewmh:set-_NET_DESKTOP_VIEWPORT
                     :window exwm--root
                     :data (make-vector (* 2 exwm-workspace-number) 0)))
  ;; Set _NET_WORKAREA (with minibuffer excluded)
  (let* ((workareas
          (vector 0 0 (x-display-pixel-width)
                  (- (x-display-pixel-height)
                     (if (exwm-workspace--minibuffer-own-frame-p)
                         0
                       (window-pixel-height (minibuffer-window))))))
         (workareas (mapconcat (lambda (_) workareas)
                               (make-list exwm-workspace-number 0) [])))
    (xcb:+request exwm--connection
        (make-instance 'xcb:ewmh:set-_NET_WORKAREA
                       :window exwm--root :data workareas)))
  (xcb:flush exwm--connection))

(defvar exwm-init-hook nil
  "Normal hook run when EXWM has just finished initialization.")

(defun exwm-init (&optional frame)
  "Initialize EXWM."
  (if (not (eq 'x (framep (or frame (selected-frame)))))
      (exwm--log "Not running under X environment")
    (unless exwm--connection
      (exwm-enable 'undo)               ;never initialize again
      (setq exwm--connection (xcb:connect-to-socket))
      (set-process-query-on-exit-flag (slot-value exwm--connection 'process)
                                      nil) ;prevent query message on exit
      (setq exwm--root
            (slot-value (car (slot-value
                              (xcb:get-setup exwm--connection) 'roots))
                        'root))
      (if (xcb:+request-checked+request-check exwm--connection
              (make-instance 'xcb:ChangeWindowAttributes
                             :window exwm--root :value-mask xcb:CW:EventMask
                             :event-mask xcb:EventMask:SubstructureRedirect))
          ;; Other window manager is running
          (progn (xcb:disconnect exwm--connection)
                 (setq exwm--connection nil)
                 (exwm--log "Other window manager detected"))
        ;; Disable some features not working well with EXWM
        (setq use-dialog-box nil)
        ;; Initialize ICCCM/EWMH support
        ;; (xcb:icccm:init exwm--connection)
        (xcb:ewmh:init exwm--connection)
        (exwm--lock)
        (exwm--init-icccm-ewmh)
        (exwm-layout--init)
        (exwm-floating--init)
        (exwm-manage--init)
        (exwm-workspace--init)
        (exwm-input--init)
        (exwm--unlock)
        (exwm-workspace--post-init)
        ;; Manage existing windows
        (exwm-manage--scan)
        (run-hooks 'exwm-init-hook)))))

(defvar exwm-blocking-subrs '(x-file-dialog x-popup-dialog x-select-font)
  "Subrs (primitives) that would normally block EXWM.")

(defun exwm-enable (&optional undo)
  "Enable/Disable EXWM."
  (pcase undo
    (`undo                              ;prevent reinitialization
     (remove-hook 'window-setup-hook #'exwm-init)
     (remove-hook 'after-make-frame-functions #'exwm-init))
    (`undo-all                          ;attempt to revert everything
     (remove-hook 'window-setup-hook #'exwm-init)
     (remove-hook 'after-make-frame-functions #'exwm-init)
     (remove-hook 'kill-emacs-hook #'exwm--server-stop)
     (dolist (i exwm-blocking-subrs)
       (advice-remove i #'exwm--server-eval-at)))
    (_                                  ;enable EXWM
     (setq frame-resize-pixelwise t)    ;mandatory; before init
     (add-hook 'window-setup-hook #'exwm-init t)          ;for Emacs
     (add-hook 'after-make-frame-functions #'exwm-init t) ;for Emacs Client
     (add-hook 'kill-emacs-hook #'exwm--server-stop)
     (dolist (i exwm-blocking-subrs)
       (advice-add i :around #'exwm--server-eval-at)))))

(defconst exwm--server-name "server-exwm"
  "Name of the subordinate Emacs server.")
(defvar exwm--server-process nil "Process of the subordinate Emacs server.")

(defun exwm--server-stop ()
  "Stop the subordinate Emacs server."
  (server-force-delete exwm--server-name)
  (when exwm--server-process
    (delete-process exwm--server-process)
    (setq exwm--server-process nil)))

(defun exwm--server-eval-at (&rest args)
  "Wrapper of `server-eval-at' used to advice subrs."
  ;; Start the subordinate Emacs server if it's not alive
  (unless (server-running-p exwm--server-name)
    (when exwm--server-process (delete-process exwm--server-process))
    (setq exwm--server-process
          (start-process exwm--server-name
                         nil
                         (car command-line-args) ;The executable file
                         "-d" x-display-name
                         "-Q"
                         (concat "--daemon=" exwm--server-name)
                         "--eval"
                         ;; Create an invisible frame
                         "(make-frame '((window-system . x) (visibility)))"))
    (while (not (server-running-p exwm--server-name))
      (sit-for 0.001)))
  (server-eval-at
   exwm--server-name
   `(progn (select-frame (car (frame-list)))
           (let ((result ,(nconc (list (make-symbol (subr-name (car args))))
                                 (cdr args))))
             (pcase (type-of result)
               ;; Return the name of a buffer
               (`buffer (buffer-name result))
               ;; We blindly convert all font objects to their XLFD names. This
               ;; might cause problems of course, but it still has a chance to
               ;; work (whereas directly passing font objects would merely
               ;; raise errors).
               ((or `font-entity `font-object `font-spec)
                (font-xlfd-name result))
               ;; Passing following types makes little sense
               ((or `compiled-function `finalizer `frame `hash-table `marker
                    `overlay `process `window `window-configuration))
               ;; Passing the name of a subr
               (`subr (make-symbol (subr-name result)))
               ;; For other types, return the value as-is.
               (t result))))))

(define-obsolete-function-alias 'exwm-enable-ido-workaround 'exwm-config-ido
  "25.1" "Enable workarounds for Ido.")

(defun exwm-disable-ido-workaround ()
  "This function does nothing actually."
  (declare (obsolete nil "25.1")))



(provide 'exwm)

;;; exwm.el ends here
