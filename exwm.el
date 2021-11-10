;;; exwm.el --- Emacs X Window Manager  -*- lexical-binding: t -*-

;; Copyright (C) 2015-2021 Free Software Foundation, Inc.

;; Author: Chris Feng <chris.w.feng@gmail.com>
;; Maintainer: Adrián Medraño Calvo <adrian@medranocalvo.com>
;; Version: 0.26
;; Package-Requires: ((xelb "0.18"))
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
;; EXWM (Emacs X Window Manager) is a full-featured tiling X window manager
;; for Emacs built on top of [XELB](https://github.com/ch11ng/xelb).
;; It features:
;; + Fully keyboard-driven operations
;; + Hybrid layout modes (tiling & stacking)
;; + Dynamic workspace support
;; + ICCCM/EWMH compliance
;; + (Optional) RandR (multi-monitor) support
;; + (Optional) Built-in system tray

;; Installation & configuration
;; ----------------------------
;; Here are the minimal steps to get EXWM working:
;; 1. Install XELB and EXWM, and make sure they are in `load-path'.
;; 2. In '~/.emacs', add following lines (please modify accordingly):
;;
;;    (require 'exwm)
;;    (require 'exwm-config)
;;    (exwm-config-example)
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

(defgroup exwm nil
  "Emacs X Window Manager."
  :tag "EXWM"
  :version "25.3"
  :group 'applications
  :prefix "exwm-")

(defcustom exwm-init-hook nil
  "Normal hook run when EXWM has just finished initialization."
  :type 'hook)

(defcustom exwm-exit-hook nil
  "Normal hook run just before EXWM exits."
  :type 'hook)

(defcustom exwm-update-class-hook nil
  "Normal hook run when window class is updated."
  :type 'hook)

(defcustom exwm-update-title-hook nil
  "Normal hook run when window title is updated."
  :type 'hook)

(defcustom exwm-blocking-subrs '(x-file-dialog x-popup-dialog x-select-font)
  "Subrs (primitives) that would normally block EXWM."
  :type '(repeat function))

(defcustom exwm-replace 'ask
  "Whether to replace existing window manager."
  :type '(radio (const :tag "Ask" ask)
                (const :tag "Replace by default" t)
                (const :tag "Do not replace" nil)))

(defconst exwm--server-name "server-exwm"
  "Name of the subordinate Emacs server.")

(defvar exwm--server-process nil "Process of the subordinate Emacs server.")

(defun exwm-reset ()
  "Reset the state of the selected window (non-fullscreen, line-mode, etc)."
  (interactive)
  (exwm--log)
  (with-current-buffer (window-buffer)
    (when (derived-mode-p 'exwm-mode)
      (when (exwm-layout--fullscreen-p)
        (exwm-layout-unset-fullscreen))
      ;; Force refresh
      (exwm-layout--refresh)
      (call-interactively #'exwm-input-grab-keyboard))))

;;;###autoload
(defun exwm-restart ()
  "Restart EXWM."
  (interactive)
  (exwm--log)
  (when (exwm--confirm-kill-emacs "[EXWM] Restart? " 'no-check)
    (let* ((attr (process-attributes (emacs-pid)))
           (args (cdr (assq 'args attr)))
           (ppid (cdr (assq 'ppid attr)))
           (pargs (cdr (assq 'args (process-attributes ppid)))))
      (cond
       ((= ppid 1)
        ;; The parent is the init process.  This probably means this
        ;; instance is an emacsclient.  Anyway, start a control instance
        ;; to manage the subsequent ones.
        (call-process (car command-line-args))
        (kill-emacs))
       ((string= args pargs)
        ;; This is a subordinate instance.  Return a magic number to
        ;; inform the parent (control instance) to start another one.
        (kill-emacs ?R))
       (t
        ;; This is the control instance.  Keep starting subordinate
        ;; instances until told to exit.
        ;; Run `server-force-stop' if it exists.
        (run-hooks 'kill-emacs-hook)
        (with-temp-buffer
          (while (= ?R (shell-command-on-region (point) (point) args))))
        (kill-emacs))))))

(defun exwm--update-desktop (xwin)
  "Update _NET_WM_DESKTOP."
  (exwm--log "#x%x" xwin)
  (with-current-buffer (exwm--id->buffer xwin)
    (let ((reply (xcb:+request-unchecked+reply exwm--connection
                     (make-instance 'xcb:ewmh:get-_NET_WM_DESKTOP
                                    :window xwin)))
          desktop)
      (when reply
        (setq desktop (slot-value reply 'value))
        (cond
         ((eq desktop 4294967295.)
          (unless (or (not exwm--floating-frame)
                      (eq exwm--frame exwm-workspace--current)
                      (and exwm--desktop
                           (= desktop exwm--desktop)))
            (exwm-layout--show xwin (frame-root-window exwm--floating-frame)))
          (setq exwm--desktop desktop))
         ((and desktop
               (< desktop (exwm-workspace--count))
               (if exwm--desktop
                   (/= desktop exwm--desktop)
                 (/= desktop (exwm-workspace--position exwm--frame))))
          (exwm-workspace-move-window desktop xwin))
         (t
          (exwm-workspace--set-desktop xwin)))))))

(defun exwm--update-window-type (id &optional force)
  "Update _NET_WM_WINDOW_TYPE."
  (exwm--log "#x%x" id)
  (with-current-buffer (exwm--id->buffer id)
    (unless (and exwm-window-type (not force))
      (let ((reply (xcb:+request-unchecked+reply exwm--connection
                       (make-instance 'xcb:ewmh:get-_NET_WM_WINDOW_TYPE
                                      :window id))))
        (when reply                     ;nil when destroyed
          (setq exwm-window-type (append (slot-value reply 'value) nil)))))))

(defun exwm--update-class (id &optional force)
  "Update WM_CLASS."
  (exwm--log "#x%x" id)
  (with-current-buffer (exwm--id->buffer id)
    (unless (and exwm-instance-name exwm-class-name (not force))
      (let ((reply (xcb:+request-unchecked+reply exwm--connection
                       (make-instance 'xcb:icccm:get-WM_CLASS :window id))))
        (when reply                     ;nil when destroyed
          (setq exwm-instance-name (slot-value reply 'instance-name)
                exwm-class-name (slot-value reply 'class-name))
          (when (and exwm-instance-name exwm-class-name)
            (run-hooks 'exwm-update-class-hook)))))))

(defun exwm--update-utf8-title (id &optional force)
  "Update _NET_WM_NAME."
  (exwm--log "#x%x" id)
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
  (exwm--log "#x%x" id)
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
  (exwm--log "#x%x" id)
  (exwm--update-utf8-title id)
  (exwm--update-ctext-title id))

(defun exwm--update-transient-for (id &optional force)
  "Update WM_TRANSIENT_FOR."
  (exwm--log "#x%x" id)
  (with-current-buffer (exwm--id->buffer id)
    (unless (and exwm-transient-for (not force))
      (let ((reply (xcb:+request-unchecked+reply exwm--connection
                       (make-instance 'xcb:icccm:get-WM_TRANSIENT_FOR
                                      :window id))))
        (when reply                     ;nil when destroyed
          (setq exwm-transient-for (slot-value reply 'value)))))))

(defun exwm--update-normal-hints (id &optional force)
  "Update WM_NORMAL_HINTS."
  (exwm--log "#x%x" id)
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
  (exwm--log "#x%x" id)
  (with-current-buffer (exwm--id->buffer id)
    (unless (and (not force) exwm--hints-input exwm--hints-urgency)
      (let ((reply (xcb:+request-unchecked+reply exwm--connection
                       (make-instance 'xcb:icccm:get-WM_HINTS :window id))))
        (when (and reply (slot-value reply 'flags)) ;nil when destroyed
          (with-slots (flags input initial-state) reply
            (when flags
              (unless (= 0 (logand flags xcb:icccm:WM_HINTS:InputHint))
                (setq exwm--hints-input (when input (= 1 input))))
              (unless (= 0 (logand flags xcb:icccm:WM_HINTS:StateHint))
                (setq exwm-state initial-state))
              (unless (= 0 (logand flags xcb:icccm:WM_HINTS:UrgencyHint))
                (setq exwm--hints-urgency t))))
          (when (and exwm--hints-urgency
                     (not (eq exwm--frame exwm-workspace--current)))
            (unless (frame-parameter exwm--frame 'exwm-urgency)
              (set-frame-parameter exwm--frame 'exwm-urgency t)
              (setq exwm-workspace--switch-history-outdated t))))))))

(defun exwm--update-protocols (id &optional force)
  "Update WM_PROTOCOLS."
  (exwm--log "#x%x" id)
  (with-current-buffer (exwm--id->buffer id)
    (unless (and exwm--protocols (not force))
      (let ((reply (xcb:+request-unchecked+reply exwm--connection
                       (make-instance 'xcb:icccm:get-WM_PROTOCOLS
                                      :window id))))
        (when reply                     ;nil when destroyed
          (setq exwm--protocols (append (slot-value reply 'value) nil)))))))

(defun exwm--update-struts-legacy (id)
  "Update _NET_WM_STRUT."
  (exwm--log "#x%x" id)
  (let ((pair (assq id exwm-workspace--id-struts-alist))
        reply struts)
    (unless (and pair (< 4 (length (cdr pair))))
      (setq reply (xcb:+request-unchecked+reply exwm--connection
                      (make-instance 'xcb:ewmh:get-_NET_WM_STRUT
                                     :window id)))
      (when reply
        (setq struts (slot-value reply 'value))
        (if pair
            (setcdr pair struts)
          (push (cons id struts) exwm-workspace--id-struts-alist))
        (exwm-workspace--update-struts))
      ;; Update workareas.
      (exwm-workspace--update-workareas)
      ;; Update workspaces.
      (dolist (f exwm-workspace--list)
        (exwm-workspace--set-fullscreen f)))))

(defun exwm--update-struts-partial (id)
  "Update _NET_WM_STRUT_PARTIAL."
  (exwm--log "#x%x" id)
  (let ((reply (xcb:+request-unchecked+reply exwm--connection
                   (make-instance 'xcb:ewmh:get-_NET_WM_STRUT_PARTIAL
                                  :window id)))
        struts pair)
    (when reply
      (setq struts (slot-value reply 'value)
            pair (assq id exwm-workspace--id-struts-alist))
      (if pair
          (setcdr pair struts)
        (push (cons id struts) exwm-workspace--id-struts-alist))
      (exwm-workspace--update-struts))
    ;; Update workareas.
    (exwm-workspace--update-workareas)
    ;; Update workspaces.
    (dolist (f exwm-workspace--list)
      (exwm-workspace--set-fullscreen f))))

(defun exwm--update-struts (id)
  "Update _NET_WM_STRUT_PARTIAL or _NET_WM_STRUT."
  (exwm--log "#x%x" id)
  (exwm--update-struts-partial id)
  (exwm--update-struts-legacy id))

(defun exwm--on-PropertyNotify (data _synthetic)
  "Handle PropertyNotify event."
  (let ((obj (make-instance 'xcb:PropertyNotify))
        atom id buffer)
    (xcb:unmarshal obj data)
    (setq id (slot-value obj 'window)
          atom (slot-value obj 'atom))
    (exwm--log "atom=%s(%s)" (x-get-atom-name atom exwm-workspace--current) atom)
    (setq buffer (exwm--id->buffer id))
    (if (not (buffer-live-p buffer))
        ;; Properties of unmanaged X windows.
        (cond ((= atom xcb:Atom:_NET_WM_STRUT)
               (exwm--update-struts-legacy id))
              ((= atom xcb:Atom:_NET_WM_STRUT_PARTIAL)
               (exwm--update-struts-partial id)))
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
              ((= atom xcb:Atom:_NET_WM_USER_TIME)) ;ignored
              (t
               (exwm--log "Unhandled: %s(%d)"
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
    (exwm--log "atom=%s(%s)" (x-get-atom-name type exwm-workspace--current)
               type)
    (cond
     ;; _NET_NUMBER_OF_DESKTOPS.
     ((= type xcb:Atom:_NET_NUMBER_OF_DESKTOPS)
      (let ((current (exwm-workspace--count))
            (requested (elt data 0)))
        ;; Only allow increasing/decreasing the workspace number by 1.
        (cond
         ((< current requested)
          (make-frame))
         ((and (> current requested)
               (> current 1))
          (let ((frame (car (last exwm-workspace--list))))
            (exwm-workspace--get-remove-frame-next-workspace frame)
            (delete-frame frame))))))
     ;; _NET_CURRENT_DESKTOP.
     ((= type xcb:Atom:_NET_CURRENT_DESKTOP)
      (exwm-workspace-switch (elt data 0)))
     ;; _NET_ACTIVE_WINDOW.
     ((= type xcb:Atom:_NET_ACTIVE_WINDOW)
      (let ((buffer (exwm--id->buffer id))
            iconic window)
        (when (buffer-live-p buffer)
          (with-current-buffer buffer
            (when (eq exwm--frame exwm-workspace--current)
              (if exwm--floating-frame
                  (select-frame exwm--floating-frame)
                (setq iconic (exwm-layout--iconic-state-p))
                (when iconic
                  ;; State change: iconic => normal.
                  (set-window-buffer (frame-selected-window exwm--frame)
                                     (current-buffer)))
                ;; Focus transfer.
                (setq window (get-buffer-window nil t))
                (when (or iconic
                          (not (eq window (selected-window))))
                  (select-window window))))))))
     ;; _NET_CLOSE_WINDOW.
     ((= type xcb:Atom:_NET_CLOSE_WINDOW)
      (let ((buffer (exwm--id->buffer id)))
        (when (buffer-live-p buffer)
          (exwm--defer 0 #'kill-buffer buffer))))
     ;; _NET_WM_MOVERESIZE
     ((= type xcb:Atom:_NET_WM_MOVERESIZE)
      (let ((direction (elt data 2))
            (buffer (exwm--id->buffer id)))
        (unless (and buffer
                     (not (buffer-local-value 'exwm--floating-frame buffer)))
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
                ;; In case it's a workspace frame.
                ((and (not buffer)
                      (catch 'break
                        (dolist (f exwm-workspace--list)
                          (when (or (eq id (frame-parameter f 'exwm-outer-id))
                                    (eq id (frame-parameter f 'exwm-id)))
                            (throw 'break t)))
                        nil)))
                (t
                 ;; In case it's a floating frame,
                 ;; move the corresponding X window instead.
                 (unless buffer
                   (catch 'break
                     (dolist (pair exwm--id-buffer-alist)
                       (with-current-buffer (cdr pair)
                         (when
                             (and exwm--floating-frame
                                  (or (eq id
                                          (frame-parameter exwm--floating-frame
                                                           'exwm-outer-id))
                                      (eq id
                                          (frame-parameter exwm--floating-frame
                                                           'exwm-id))))
                           (setq id exwm--id)
                           (throw 'break nil))))))
                 ;; Start to move it.
                 (exwm-floating--start-moveresize id direction))))))
     ;; _NET_REQUEST_FRAME_EXTENTS
     ((= type xcb:Atom:_NET_REQUEST_FRAME_EXTENTS)
      (let ((buffer (exwm--id->buffer id))
            top btm)
        (if (or (not buffer)
                (not (buffer-local-value 'exwm--floating-frame buffer)))
            (setq top 0
                  btm 0)
          (setq top (window-header-line-height)
                btm (window-mode-line-height)))
        (xcb:+request exwm--connection
            (make-instance 'xcb:ewmh:set-_NET_FRAME_EXTENTS
                           :window id
                           :left 0
                           :right 0
                           :top top
                           :bottom btm)))
      (xcb:flush exwm--connection))
     ;; _NET_WM_DESKTOP.
     ((= type xcb:Atom:_NET_WM_DESKTOP)
      (let ((buffer (exwm--id->buffer id)))
        (when (buffer-live-p buffer)
          (exwm-workspace-move-window (elt data 0) id))))
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
          (xcb:+request
              exwm--connection
              (make-instance 'xcb:ewmh:set-_NET_WM_STATE
                             :window id
                             :data (vector xcb:Atom:_NET_WM_STATE_FULLSCREEN)))
          (xcb:flush exwm--connection))
        (when buffer                    ;ensure it's managed
          (with-current-buffer buffer
            ;; _NET_WM_STATE_FULLSCREEN
            (when (or (memq xcb:Atom:_NET_WM_STATE_FULLSCREEN props)
                      (memq xcb:Atom:_NET_WM_STATE_ABOVE props))
              (cond ((= action xcb:ewmh:_NET_WM_STATE_ADD)
                     (unless (exwm-layout--fullscreen-p)
                       (exwm-layout-set-fullscreen id))
                     (push xcb:Atom:_NET_WM_STATE_FULLSCREEN props-new))
                    ((= action xcb:ewmh:_NET_WM_STATE_REMOVE)
                     (when (exwm-layout--fullscreen-p)
                       (exwm-layout-unset-fullscreen id)))
                    ((= action xcb:ewmh:_NET_WM_STATE_TOGGLE)
                     (if (exwm-layout--fullscreen-p)
                         (exwm-layout-unset-fullscreen id)
                       (exwm-layout-set-fullscreen id)
                       (push xcb:Atom:_NET_WM_STATE_FULLSCREEN props-new)))))
            ;; _NET_WM_STATE_DEMANDS_ATTENTION
            ;; FIXME: check (may require other properties set)
            (when (memq xcb:Atom:_NET_WM_STATE_DEMANDS_ATTENTION props)
              (when (= action xcb:ewmh:_NET_WM_STATE_ADD)
                (unless (eq exwm--frame exwm-workspace--current)
                  (set-frame-parameter exwm--frame 'exwm-urgency t)
                  (setq exwm-workspace--switch-history-outdated t)))
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
     ((= type xcb:Atom:WM_CHANGE_STATE)
      (let ((buffer (exwm--id->buffer id)))
        (when (and (buffer-live-p buffer)
                   (= (elt data 0) xcb:icccm:WM_STATE:IconicState))
          (with-current-buffer buffer
            (if exwm--floating-frame
                (call-interactively #'exwm-floating-hide)
              (bury-buffer))))))
     (t
      (exwm--log "Unhandled: %s(%d)"
                 (x-get-atom-name type exwm-workspace--current) type)))))

(defun exwm--on-SelectionClear (data _synthetic)
  "Handle SelectionClear events."
  (exwm--log)
  (let ((obj (make-instance 'xcb:SelectionClear))
        owner selection)
    (xcb:unmarshal obj data)
    (setq owner (slot-value obj 'owner)
          selection (slot-value obj 'selection))
    (when (and (eq owner exwm--wmsn-window)
               (eq selection xcb:Atom:WM_S0))
      (exwm-exit))))

(defun exwm--init-icccm-ewmh ()
  "Initialize ICCCM/EWMH support."
  (exwm--log)
  ;; Handle PropertyNotify event
  (xcb:+event exwm--connection 'xcb:PropertyNotify #'exwm--on-PropertyNotify)
  ;; Handle relevant client messages
  (xcb:+event exwm--connection 'xcb:ClientMessage #'exwm--on-ClientMessage)
  ;; Handle SelectionClear
  (xcb:+event exwm--connection 'xcb:SelectionClear #'exwm--on-SelectionClear)
  ;; Set _NET_SUPPORTED
  (xcb:+request exwm--connection
      (make-instance 'xcb:ewmh:set-_NET_SUPPORTED
                     :window exwm--root
                     :data (vector
                            ;; Root windows properties.
                            xcb:Atom:_NET_SUPPORTED
                            xcb:Atom:_NET_CLIENT_LIST
                            xcb:Atom:_NET_CLIENT_LIST_STACKING
                            xcb:Atom:_NET_NUMBER_OF_DESKTOPS
                            xcb:Atom:_NET_DESKTOP_GEOMETRY
                            xcb:Atom:_NET_DESKTOP_VIEWPORT
                            xcb:Atom:_NET_CURRENT_DESKTOP
                            ;; xcb:Atom:_NET_DESKTOP_NAMES
                            xcb:Atom:_NET_ACTIVE_WINDOW
                            ;; xcb:Atom:_NET_WORKAREA
                            xcb:Atom:_NET_SUPPORTING_WM_CHECK
                            ;; xcb:Atom:_NET_VIRTUAL_ROOTS
                            ;; xcb:Atom:_NET_DESKTOP_LAYOUT
                            ;; xcb:Atom:_NET_SHOWING_DESKTOP

                            ;; Other root window messages.
                            xcb:Atom:_NET_CLOSE_WINDOW
                            ;; xcb:Atom:_NET_MOVERESIZE_WINDOW
                            xcb:Atom:_NET_WM_MOVERESIZE
                            ;; xcb:Atom:_NET_RESTACK_WINDOW
                            xcb:Atom:_NET_REQUEST_FRAME_EXTENTS

                            ;; Application window properties.
                            xcb:Atom:_NET_WM_NAME
                            ;; xcb:Atom:_NET_WM_VISIBLE_NAME
                            ;; xcb:Atom:_NET_WM_ICON_NAME
                            ;; xcb:Atom:_NET_WM_VISIBLE_ICON_NAME
                            xcb:Atom:_NET_WM_DESKTOP
                            ;;
                            xcb:Atom:_NET_WM_WINDOW_TYPE
                            ;; xcb:Atom:_NET_WM_WINDOW_TYPE_DESKTOP
                            xcb:Atom:_NET_WM_WINDOW_TYPE_DOCK
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
                            ;; xcb:Atom:_NET_WM_STATE_MODAL
                            ;; xcb:Atom:_NET_WM_STATE_STICKY
                            ;; xcb:Atom:_NET_WM_STATE_MAXIMIZED_VERT
                            ;; xcb:Atom:_NET_WM_STATE_MAXIMIZED_HORZ
                            ;; xcb:Atom:_NET_WM_STATE_SHADED
                            ;; xcb:Atom:_NET_WM_STATE_SKIP_TASKBAR
                            ;; xcb:Atom:_NET_WM_STATE_SKIP_PAGER
                            xcb:Atom:_NET_WM_STATE_HIDDEN
                            xcb:Atom:_NET_WM_STATE_FULLSCREEN
                            ;; xcb:Atom:_NET_WM_STATE_ABOVE
                            ;; xcb:Atom:_NET_WM_STATE_BELOW
                            xcb:Atom:_NET_WM_STATE_DEMANDS_ATTENTION
                            ;; xcb:Atom:_NET_WM_STATE_FOCUSED
                            ;;
                            xcb:Atom:_NET_WM_ALLOWED_ACTIONS
                            xcb:Atom:_NET_WM_ACTION_MOVE
                            xcb:Atom:_NET_WM_ACTION_RESIZE
                            xcb:Atom:_NET_WM_ACTION_MINIMIZE
                            ;; xcb:Atom:_NET_WM_ACTION_SHADE
                            ;; xcb:Atom:_NET_WM_ACTION_STICK
                            ;; xcb:Atom:_NET_WM_ACTION_MAXIMIZE_HORZ
                            ;; xcb:Atom:_NET_WM_ACTION_MAXIMIZE_VERT
                            xcb:Atom:_NET_WM_ACTION_FULLSCREEN
                            xcb:Atom:_NET_WM_ACTION_CHANGE_DESKTOP
                            xcb:Atom:_NET_WM_ACTION_CLOSE
                            ;; xcb:Atom:_NET_WM_ACTION_ABOVE
                            ;; xcb:Atom:_NET_WM_ACTION_BELOW
                            ;;
                            xcb:Atom:_NET_WM_STRUT
                            xcb:Atom:_NET_WM_STRUT_PARTIAL
                            ;; xcb:Atom:_NET_WM_ICON_GEOMETRY
                            ;; xcb:Atom:_NET_WM_ICON
                            xcb:Atom:_NET_WM_PID
                            ;; xcb:Atom:_NET_WM_HANDLED_ICONS
                            ;; xcb:Atom:_NET_WM_USER_TIME
                            ;; xcb:Atom:_NET_WM_USER_TIME_WINDOW
                            xcb:Atom:_NET_FRAME_EXTENTS
                            ;; xcb:Atom:_NET_WM_OPAQUE_REGION
                            ;; xcb:Atom:_NET_WM_BYPASS_COMPOSITOR

                            ;; Window manager protocols.
                            xcb:Atom:_NET_WM_PING
                            ;; xcb:Atom:_NET_WM_SYNC_REQUEST
                            ;; xcb:Atom:_NET_WM_FULLSCREEN_MONITORS

                            ;; Other properties.
                            xcb:Atom:_NET_WM_FULL_PLACEMENT)))
  ;; Create a child window for setting _NET_SUPPORTING_WM_CHECK
  (let ((new-id (xcb:generate-id exwm--connection)))
    (setq exwm--guide-window new-id)
    (xcb:+request exwm--connection
        (make-instance 'xcb:CreateWindow
                       :depth 0
                       :wid new-id
                       :parent exwm--root
                       :x -1
                       :y -1
                       :width 1
                       :height 1
                       :border-width 0
                       :class xcb:WindowClass:InputOnly
                       :visual 0
                       :value-mask xcb:CW:OverrideRedirect
                       :override-redirect 1))
    ;; Set _NET_WM_NAME.  Must be set to the name of the window manager, as
    ;; required by wm-spec.
    (xcb:+request exwm--connection
        (make-instance 'xcb:ewmh:set-_NET_WM_NAME
                       :window new-id :data "EXWM"))
    (dolist (i (list exwm--root new-id))
      ;; Set _NET_SUPPORTING_WM_CHECK
      (xcb:+request exwm--connection
          (make-instance 'xcb:ewmh:set-_NET_SUPPORTING_WM_CHECK
                         :window i :data new-id))))
  ;; Set _NET_DESKTOP_VIEWPORT (we don't support large desktop).
  (xcb:+request exwm--connection
      (make-instance 'xcb:ewmh:set-_NET_DESKTOP_VIEWPORT
                     :window exwm--root
                     :data [0 0]))
  (xcb:flush exwm--connection))

(defun exwm--wmsn-acquire (replace)
  "Acquire the WM_Sn selection.

REPLACE specifies what to do in case there already is a window
manager.  If t, replace it, if nil, abort and ask the user if `ask'."
  (exwm--log "%s" replace)
  (with-slots (owner)
      (xcb:+request-unchecked+reply exwm--connection
          (make-instance 'xcb:GetSelectionOwner
                         :selection xcb:Atom:WM_S0))
    (when (/= owner xcb:Window:None)
      (when (eq replace 'ask)
        (setq replace (yes-or-no-p "Replace existing window manager? ")))
      (when (not replace)
        (user-error "Other window manager detected")))
    (let ((new-owner (xcb:generate-id exwm--connection)))
      (xcb:+request exwm--connection
          (make-instance 'xcb:CreateWindow
                         :depth 0
                         :wid new-owner
                         :parent exwm--root
                         :x -1
                         :y -1
                         :width 1
                         :height 1
                         :border-width 0
                         :class xcb:WindowClass:CopyFromParent
                         :visual 0
                         :value-mask 0
                         :override-redirect 0))
      (xcb:+request exwm--connection
          (make-instance 'xcb:ewmh:set-_NET_WM_NAME
                         :window new-owner :data "EXWM: exwm--wmsn-window"))
      (xcb:+request-checked+request-check exwm--connection
          (make-instance 'xcb:SetSelectionOwner
                         :selection xcb:Atom:WM_S0
                         :owner new-owner
                         :time xcb:Time:CurrentTime))
      (with-slots (owner)
          (xcb:+request-unchecked+reply exwm--connection
              (make-instance 'xcb:GetSelectionOwner
                             :selection xcb:Atom:WM_S0))
        (unless (eq owner new-owner)
          (error "Could not acquire ownership of WM selection")))
      ;; Wait for the other window manager to terminate.
      (when (/= owner xcb:Window:None)
        (let (reply)
          (cl-dotimes (i exwm--wmsn-acquire-timeout)
            (setq reply (xcb:+request-unchecked+reply exwm--connection
                            (make-instance 'xcb:GetGeometry :drawable owner)))
            (when (not reply)
              (cl-return))
            (message "Waiting for other window manager to quit... %ds" i)
            (sleep-for 1))
          (when reply
            (error "Other window manager did not release selection in time"))))
      ;; announce
      (let* ((cmd (make-instance 'xcb:ClientMessageData
                                 :data32 (vector xcb:Time:CurrentTime
                                                 xcb:Atom:WM_S0
                                                 new-owner
                                                 0
                                                 0)))
             (cm (make-instance 'xcb:ClientMessage
                                               :window exwm--root
                                               :format 32
                                               :type xcb:Atom:MANAGER
                                               :data cmd))
             (se (make-instance 'xcb:SendEvent
                         :propagate 0
                         :destination exwm--root
                         :event-mask xcb:EventMask:NoEvent
                         :event (xcb:marshal cm exwm--connection))))
        (xcb:+request exwm--connection se))
      (setq exwm--wmsn-window new-owner))))

;;;###autoload
(cl-defun exwm-init (&optional frame)
  "Initialize EXWM."
  (interactive)
  (exwm--log "%s" frame)
  (if frame
      ;; The frame might not be selected if it's created by emacslicnet.
      (select-frame-set-input-focus frame)
    (setq frame (selected-frame)))
  (when (not (eq 'x (framep frame)))
    (message "[EXWM] Not running under X environment")
    (cl-return-from exwm-init))
  (when exwm--connection
    (exwm--log "EXWM already running")
    (cl-return-from exwm-init))
  (condition-case err
      (progn
        (exwm-enable 'undo)               ;never initialize again
        (setq exwm--connection (xcb:connect))
        (set-process-query-on-exit-flag (slot-value exwm--connection 'process)
                                        nil) ;prevent query message on exit
        (setq exwm--root
              (slot-value (car (slot-value
                                (xcb:get-setup exwm--connection) 'roots))
                          'root))
        ;; Initialize ICCCM/EWMH support
        (xcb:icccm:init exwm--connection t)
        (xcb:ewmh:init exwm--connection t)
        ;; Try to register window manager selection.
        (exwm--wmsn-acquire exwm-replace)
        (when (xcb:+request-checked+request-check exwm--connection
                  (make-instance 'xcb:ChangeWindowAttributes
                                 :window exwm--root
                                 :value-mask xcb:CW:EventMask
                                 :event-mask
                                 xcb:EventMask:SubstructureRedirect))
          (error "Other window manager is running"))
        ;; Disable some features not working well with EXWM
        (setq use-dialog-box nil
              confirm-kill-emacs #'exwm--confirm-kill-emacs)
        (exwm--lock)
        (exwm--init-icccm-ewmh)
        (exwm-layout--init)
        (exwm-floating--init)
        (exwm-manage--init)
        (exwm-workspace--init)
        (exwm-input--init)
        (exwm--unlock)
        (exwm-workspace--post-init)
        (exwm-input--post-init)
        (run-hooks 'exwm-init-hook)
        ;; Manage existing windows
        (exwm-manage--scan))
    (user-error)
    ((quit error)
     (exwm-exit)
     ;; Rethrow error
     (warn "[EXWM] EXWM fails to start (%s: %s)" (car err) (cdr err)))))


;;;###autoload
(defun exwm-exit ()
  "Exit EXWM."
  (interactive)
  (exwm--log)
  (run-hooks 'exwm-exit-hook)
  (setq confirm-kill-emacs nil)
  ;; Exit modules.
  (exwm-input--exit)
  (exwm-manage--exit)
  (exwm-workspace--exit)
  (exwm-floating--exit)
  (exwm-layout--exit)
  (when exwm--connection
    (xcb:flush exwm--connection)
    (xcb:disconnect exwm--connection))
  (setq exwm--connection nil))

;;;###autoload
(defun exwm-enable (&optional undo)
  "Enable/Disable EXWM."
  (exwm--log "%s" undo)
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
     (setq frame-resize-pixelwise t     ;mandatory; before init
           window-resize-pixelwise t)
     ;; Ignore unrecognized command line arguments.  This can be helpful
     ;; when EXWM is launched by some session manager.
     (push #'vector command-line-functions)
     ;; In case EXWM is to be started from a graphical Emacs instance.
     (add-hook 'window-setup-hook #'exwm-init t)
     ;; In case EXWM is to be started with emacsclient.
     (add-hook 'after-make-frame-functions #'exwm-init t)
     ;; Manage the subordinate Emacs server.
     (add-hook 'kill-emacs-hook #'exwm--server-stop)
     (dolist (i exwm-blocking-subrs)
       (advice-add i :around #'exwm--server-eval-at)))))

(defun exwm--server-stop ()
  "Stop the subordinate Emacs server."
  (exwm--log)
  (server-force-delete exwm--server-name)
  (when exwm--server-process
    (delete-process exwm--server-process)
    (setq exwm--server-process nil)))

(defun exwm--server-eval-at (&rest args)
  "Wrapper of `server-eval-at' used to advice subrs."
  ;; Start the subordinate Emacs server if it's not alive
  (exwm--log "%s" args)
  (unless (server-running-p exwm--server-name)
    (when exwm--server-process (delete-process exwm--server-process))
    (setq exwm--server-process
          (start-process exwm--server-name
                         nil
                         (car command-line-args) ;The executable file
                         "-d" (frame-parameter nil 'display)
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

(defun exwm--confirm-kill-emacs (prompt &optional force)
  "Confirm before exiting Emacs."
  (exwm--log)
  (when (cond
         ((and force (not (eq force 'no-check)))
          ;; Force killing Emacs.
          t)
         ((or (eq force 'no-check) (not exwm--id-buffer-alist))
          ;; Check if there's any unsaved file.
          (pcase (catch 'break
                   (let ((kill-emacs-query-functions
                          (append kill-emacs-query-functions
                                  (list (lambda ()
                                          (throw 'break 'break))))))
                     (save-buffers-kill-emacs)))
            (`break (y-or-n-p prompt))
            (x x)))
         (t
          (yes-or-no-p (format "[EXWM] %d window(s) will be destroyed.  %s"
                               (length exwm--id-buffer-alist) prompt))))
    ;; Run `kill-emacs-hook' (`server-force-stop' excluded) before Emacs
    ;; frames are unmapped so that errors (if any) can be visible.
    (if (memq #'server-force-stop kill-emacs-hook)
        (progn
          (setq kill-emacs-hook (delq #'server-force-stop kill-emacs-hook))
          (run-hooks 'kill-emacs-hook)
          (setq kill-emacs-hook (list #'server-force-stop)))
      (run-hooks 'kill-emacs-hook)
      (setq kill-emacs-hook nil))
    ;; Exit each module, destroying all resources created by this connection.
    (exwm-exit)
    ;; Set the return value.
    t))



(provide 'exwm)

;;; exwm.el ends here
