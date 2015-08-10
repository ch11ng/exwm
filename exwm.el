;;; exwm.el --- Emacs X Window Manager  -*- lexical-binding: t -*-

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

;; Overview
;; --------
;; EXWM (Emacs X Window Manager) turns Emacs into a full-featured tiling X
;; window manager. It's built on top of XELB, thus purely consists of Elisp
;; codes. Some features include:
;; + It's fully keyboard-driven.
;;   - You have full access to all key bindings in Emacs.
;;   - It allows you to bind additional key sequences with `exwm-input-set-key'
;;     (just like `global-set-key').
;;   - It supports simulation keys (i.e., map one key sequence to another).
;; + Workspace support.
;;   - EXWM support up to 10 workspaces.
;; + ICCCM/EWMH compliance.
;;   - Note that the support for EWMH can never be complete since EXWM is not a
;;     conventional window manager.

;; How it works
;; ------------
;; Emacs itself is a tiling window manager, though unfortunately not for
;; managing X things. EXWM has therefore been created to overcome this limitation
;; by relating X concepts to Emacs ones as shown in the following table.
;;
;; +=============+=========+
;; | X Window    | Emacs   |
;; +=============+=========+
;; | window      | buffer  |
;; +-------------+---------+
;; | container*  | window  |
;; +-------------+---------+
;; | workspace / | frame** |
;; | decoration  |         |
;; +=============+=========+
;; *  Here a container means the parent of an X client window created by window
;;    manager for layout management.
;; ** In EXWM, A frame usually acts as a workspace. But for a floating window,
;;    it's the decoration around the top-level window.
;;
;; Each X client window corresponds to a dedicated buffer in EXWM mode. When
;; such a buffer is buried or unburied, the attached X client window is hide or
;; shown accordingly. The position and size of the X client window is then
;; determined by the Emacs window its corresponding buffer displayed in.
;;
;; A buffer in EXWM mode also records which workspace it belongs to, and its
;; attached X client window is made a child (in the sense of X) of the
;; workspace frame. The switch between workspaces is simply done by switching
;; corresponding Emacs frames.

;; Installation & configuration
;; ----------------------------
;; Here are the minimal steps to get EXWM working:
;; 0. Install xelb and xelb-util first.
;; 1. Put EXWM somewhere on your disk and make sure it's in `load-path'.
;; 2. In your Emacs init file, add following lines (modify accordingly):
;;
;;    (require 'exwm)
;;    ;; We always need a way to go back from char-mode to line-mode
;;    (exwm-input-set-key (kbd "s-r") 'exwm-reset)
;;    ;; Bind a key to switch workspace interactively
;;    (exwm-input-set-key (kbd "s-w") 'exwm-workspace-switch)
;;    ;; Use class name to name an EXWM buffer
;;    (add-hook 'exwm-update-class-hook
;;              (lambda () (rename-buffer exwm-class-name t)))
;;    ;; Enable EXWM
;;    (exwm-enable)
;;
;; 3. Make a file '~/.xinitrc' with the content
;;
;;    exec emacs
;;
;; 4. Launch EXWM in a console with
;;
;;    xinit
;;
;; You should refer to other resources on how to further configure '~/.xinitrc'
;; and other init scripts to improve the working experience. Besides, you
;; should hide the menu-bar, tool-bar, etc to increase the usable space.

;; Interactive modes
;; -----------------
;; There are two modes in EXWM to interact with an X client window: line mode
;; and char mode. They are analogous to those concepts in `ansi-term'. EXWM
;; buffers are created in line mode by default.
;;
;; In line mode, all key events are intercepted and then forwarded to the X
;; client window except
;; + it forms a mode-specific key sequence (which begins with 'C-c'), or
;; + it forms a key sequence bound with `exwm-input-set-key', or
;; + it forms a key sequence starting with a line mode prefix key, or
;; + it forms a key sequence in line mode simulation keys.
;; You can use 'C-c q' (bound to `exwm-input-send-next-key', can be 'C-u'
;; prefixed) to send these keys to the client.
;;
;; In char mode however, no key event is intercepted except those bound with
;; `exwm-input-set-key'. Therefore you will almost always need to
;; 'exwm-input-set-key' a key sequence to go from char mode to line mode.

;; Related projects
;; ----------------
;; EXWM is not the first attempt to make Emacs an X window manger; there is
;; another ancient project called XWEM (http://www.nongnu.org/xwem/) for
;; XEmacs, though it seems nobody have ever got it working on GNU Emacs.

;; Todo:
;; + Investigate DnD support (e.g. drag a chromium tab to another window).
;; + Auto hide minibuffer, or allow users to place it elsewhere.
;; + Add system tray support.

;; References:
;; + dwm (http://dwm.suckless.org/)
;; + i3 wm (https://i3wm.org/)
;; + Also see references within each required library.

;;; Code:

(require 'xcb)
(require 'xcb-icccm)
(require 'xcb-ewmh)
(require 'exwm-workspace)
(require 'exwm-layout)
(require 'exwm-floating)
(require 'exwm-manage)
(require 'exwm-input)
(require 'exwm-randr)

(defvar exwm-debug-on nil "Non-nil to turn on debug for EXWM.")

(defmacro exwm--log (format-string &rest args)
  "Print debug message."
  (when exwm-debug-on
    `(message (concat "[EXWM] " ,format-string) ,@args)))

(defconst exwm--client-event-mask
  (logior xcb:EventMask:StructureNotify xcb:EventMask:PropertyChange)
  "Event mask set on all managed windows.")

(defvar exwm--connection nil "X connection.")
(defvar exwm--root nil "Root window.")
(defvar exwm--id-buffer-alist nil "Alist of (<X window ID> . <Emacs buffer>)")

(defsubst exwm--id->buffer (id)
  "X window ID => Emacs buffer."
  (cdr (assoc id exwm--id-buffer-alist)))

(defsubst exwm--buffer->id (buffer)
  "Emacs buffer => X window ID."
  (car (rassoc buffer exwm--id-buffer-alist)))

(defun exwm--lock (&rest args)          ;args are for abnormal hook
  "Lock (disable all events)."
  (xcb:+request exwm--connection
      (make-instance 'xcb:ChangeWindowAttributes
                     :window exwm--root
                     :value-mask xcb:CW:EventMask
                     :event-mask xcb:EventMask:NoEvent))
  (xcb:flush exwm--connection))

(defun exwm--unlock (&rest args)        ;args are for abnormal hook
  "Unlock (enable all events)."
  (xcb:+request exwm--connection
      (make-instance 'xcb:ChangeWindowAttributes
                     :window exwm--root
                     :value-mask xcb:CW:EventMask
                     :event-mask (logior xcb:EventMask:StructureNotify
                                         xcb:EventMask:SubstructureRedirect)))
  (xcb:flush exwm--connection))

(defun exwm--make-emacs-idle-for (seconds)
  "Put Emacs in idle state for SECONDS seconds."
  (with-timeout (seconds) (read-event)))

(defun exwm-reset ()
  "Reset window to standard state: non-fullscreen, line-mode."
  (interactive)
  (unless (frame-parameter nil 'exwm-window-id)
    ;; Move focus away form a non-EXWM frame
    (x-focus-frame exwm-workspace--current))
  (with-current-buffer (window-buffer)
    (when (eq major-mode 'exwm-mode)
      (when exwm--fullscreen (exwm-layout-unset-fullscreen))
      ;; Force update input focus
      (setq exwm-input--focus-id xcb:Window:None)
      (exwm-input--update-focus)
      ;; Force refresh
      (exwm-layout--refresh)
      (exwm-input-grab-keyboard))))

(defmacro exwm--with-current-id (id &rest body)
  "Evaluate BODY in the context of the buffer corresponding to window ID."
  (declare (indent 1))
  `(when ,id
     (let ((buffer (exwm--id->buffer ,id)))
       (when buffer
         (with-current-buffer buffer ,@body)))))

(defun exwm--update-window-type (id &optional force)
  "Update _NET_WM_WINDOW_TYPE."
  (exwm--with-current-id id
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
  (exwm--with-current-id id
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
  (exwm--with-current-id id
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
  (exwm--with-current-id id
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
  (exwm--with-current-id id
    (unless (and exwm-transient-for (not force))
      (let ((reply (xcb:+request-unchecked+reply exwm--connection
                       (make-instance 'xcb:icccm:get-WM_TRANSIENT_FOR
                                      :window id))))
        (when reply                     ;nil when destroyed
          (setq exwm-transient-for (slot-value reply 'value)))))))

(defun exwm--update-normal-hints (id &optional force)
  "Update WM_NORMAL_HINTS."
  (exwm--with-current-id id
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
  (exwm--with-current-id id
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
              (exwm-workspace--update-switch-history))))))))

(defun exwm--update-protocols (id &optional force)
  "Update WM_PROTOCOLS."
  (exwm--with-current-id id
    (unless (and exwm--protocols (not force))
      (let ((reply (xcb:+request-unchecked+reply exwm--connection
                       (make-instance 'xcb:icccm:get-WM_PROTOCOLS
                                      :window id))))
        (when reply                     ;nil when destroyed
          (setq exwm--protocols (append (slot-value reply 'value) nil)))))))

(defun exwm--update-state (id &optional force)
  "Update WM_STATE."
  (exwm--with-current-id id
    (unless (and exwm-state (not force))
      (let ((reply (xcb:+request-unchecked+reply exwm--connection
                       (make-instance 'xcb:icccm:get-WM_STATE :window id))))
        (when reply                     ;nil when destroyed
          (setq exwm-state (or (slot-value reply 'state)
                               ;; Default to normal state
                               xcb:icccm:WM_STATE:NormalState)))))))

(defun exwm--on-PropertyNotify (data synthetic)
  "Handle PropertyNotify event."
  (let ((obj (make-instance 'xcb:PropertyNotify))
        atom window state
        buffer)
    (xcb:unmarshal obj data)
    (setq id (slot-value obj 'window)
          atom (slot-value obj 'atom)
          exwm-input--timestamp (slot-value obj 'time)
          state (slot-value obj 'state))
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

(defun exwm--on-ClientMessage (raw-data synthetic)
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
            left right top bottom)
        (if (or (not buffer)
                (with-current-buffer buffer
                  (not exwm--floating-frame)))
            (setq left 0 right 0 top 0 bottom 0)
          (setq left exwm-floating-border-width
                right exwm-floating-border-width
                top (+ exwm-floating-border-width (window-header-line-height))
                bottom (+ exwm-floating-border-width
                          (window-mode-line-height))))
        (xcb:+request exwm--connection
            (make-instance 'xcb:ewmh:set-_NET_FRAME_EXTENTS
                           :window id :left left :right right
                           :top top :bottom bottom)))
      (xcb:flush exwm--connection))
     ;; _NET_WM_STATE
     ((= type xcb:Atom:_NET_WM_STATE)
      (let ((action (elt data 0))
            (props (list (elt data 1) (elt data 2)))
            (buffer (exwm--id->buffer id))
            props-new)
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
                    (exwm-workspace--update-switch-history))))
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
               (setq exwm--ping-lock nil))
              (t (exwm--log "Unhandled WM_PROTOCOLS of type: %d" type)))))
     (t (exwm--log "Unhandled client message: %s" obj)))))

(defun exwm--init-icccm-ewmh ()
  "Initialize ICCCM/EWMH support."
  ;; Handle PropertyNotify event
  (xcb:+event exwm--connection 'xcb:PropertyNotify 'exwm--on-PropertyNotify)
  ;; Handle relevant client messages
  ;; FIXME: WM_STATE client messages (normal => iconic)
  ;;        WM_COLORMAP_NOTIFY
  (xcb:+event exwm--connection 'xcb:ClientMessage 'exwm--on-ClientMessage)
  ;; Set _NET_SUPPORTED
  (xcb:+request exwm--connection
      (make-instance 'xcb:ewmh:set-_NET_SUPPORTED
                     :window exwm--root
                     :data (vector xcb:Atom:_NET_SUPPORTED
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
  ;; Set _NET_WORKAREA (with minibuffer and bottom mode-line excluded)
  (let* ((workareas
          (vconcat (window-absolute-pixel-edges (get-largest-window t))))
         (workareas (mapconcat (lambda (i) workareas)
                               (make-list exwm-workspace-number 0) [])))
    (xcb:+request exwm--connection
        (make-instance 'xcb:ewmh:set-_NET_WORKAREA
                       :window exwm--root :data workareas)))
  ;; Set _NET_VIRTUAL_ROOTS
  (xcb:+request exwm--connection
      (make-instance 'xcb:ewmh:set-_NET_VIRTUAL_ROOTS
                     :window exwm--root
                     :data (vconcat (mapcar
                                     (lambda (i)
                                       (frame-parameter i 'exwm-window-id))
                                     exwm-workspace--list))))
  (xcb:flush exwm--connection))

(defvar exwm-init-hook nil
  "Normal hook run when EXWM has just finished initialization.")

(defun exwm-init (&optional frame)
  "Initialize EXWM."
  (if (not (eq 'x (framep (or frame (selected-frame)))))
      (exwm--log "Not running under X environment")
    (unless exwm--connection
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
                 (exwm-enable 'undo)
                 (exwm--log "Other window manager detected"))
        ;; Initialize ICCCM/EWMH support
        ;; (xcb:icccm:init exwm--connection)
        (xcb:ewmh:init exwm--connection)
        (exwm--lock)
        (exwm-workspace--init)
        (exwm--init-icccm-ewmh)
        (exwm-layout--init)
        (exwm-floating--init)
        (exwm-manage--init)
        (exwm-input--init)
        (exwm-randr--init)
        (exwm--unlock)
        ;; Manage exiting windows
        (exwm-manage--scan)
        (run-hooks 'exwm-init-hook)))))

(defvar exwm-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-ck" 'exwm-input-release-keyboard)
    (define-key map "\C-cf" 'exwm-layout-set-fullscreen)
    (define-key map "\C-cm" 'exwm-floating-toggle-floating)
    (define-key map "\C-cq" 'exwm-input-send-next-key)
    (define-key map "\C-cv" 'exwm-workspace-move-window)
    map)
  "Keymap for `exwm-mode'.")

(define-derived-mode exwm-mode nil "EXWM"
  "Major mode for managing X windows.

\\{exwm-mode-map}"
  ;; Internal variables
  (make-local-variable 'exwm--id)              ;window id
  (set (make-local-variable 'exwm--frame) nil) ;workspace frame
  (set (make-local-variable 'exwm--floating-frame) nil) ;floating frame
  (set (make-local-variable 'exwm--floating-edges) nil) ;four edges
  (set (make-local-variable 'exwm--fullscreen) nil) ;used in fullscreen
  (set (make-local-variable 'exwm--floating-frame-geometry) nil) ;in fullscreen
  (set (make-local-variable 'exwm--fixed-size) nil) ;fixed size
  (set (make-local-variable 'exwm--on-KeyPress) ;KeyPress event handler
       'exwm-input--on-KeyPress-line-mode)
  ;; Properties
  (set (make-local-variable 'exwm-window-type) nil)
  (set (make-local-variable 'exwm--geometry) nil)
  (set (make-local-variable 'exwm-class-name) nil)
  (set (make-local-variable 'exwm-instance-name) nil)
  (set (make-local-variable 'exwm-title) nil)
  (set (make-local-variable 'exwm--title-is-utf8) nil)
  (set (make-local-variable 'exwm-transient-for) nil)
  ;; _NET_WM_NORMAL_HINTS
  (set (make-local-variable 'exwm--normal-hints-x) nil)
  (set (make-local-variable 'exwm--normal-hints-y) nil)
  (set (make-local-variable 'exwm--normal-hints-width) nil)
  (set (make-local-variable 'exwm--normal-hints-height) nil)
  (set (make-local-variable 'exwm--normal-hints-min-width) nil)
  (set (make-local-variable 'exwm--normal-hints-min-height) nil)
  (set (make-local-variable 'exwm--normal-hints-max-width) nil)
  (set (make-local-variable 'exwm--normal-hints-max-height) nil)
  ;; (set (make-local-variable 'exwm--normal-hints-win-gravity) nil)
  ;; WM_HINTS
  (set (make-local-variable 'exwm--hints-input) nil) ;FIXME
  (set (make-local-variable 'exwm--hints-urgency) nil)
  ;;
  (set (make-local-variable 'exwm--protocols) nil)
  (set (make-local-variable 'exwm-state) nil)
  ;; Change major-mode is not allowed
  (set (make-local-variable 'change-major-mode-hook) 'kill-buffer)
  ;;
  (setq mode-name
        '(:eval (propertize "EXWM" 'face
                            (when (cl-some (lambda (i)
                                             (frame-parameter i
                                                              'exwm--urgency))
                                           exwm-workspace--list)
                              'font-lock-warning-face))))
  ;; Kill buffer -> close window
  (set (make-local-variable 'kill-buffer-query-functions)
       (list (lambda ()
               (exwm-manage--close-window exwm--id (current-buffer))
               nil)))
  (setq buffer-read-only t
        left-margin-width nil
        right-margin-width nil
        left-fringe-width 0
        right-fringe-width 0
        vertical-scroll-bar nil))

(defun exwm-enable (&optional undo)
  "Enable/Disable EXWM"
  (setq frame-resize-pixelwise t)       ;mandatory; before init
  (if (eq undo 'undo)
      (progn (remove-hook 'window-setup-hook 'exwm-init)
             (remove-hook 'after-make-frame-functions 'exwm-init))
    (add-hook 'window-setup-hook 'exwm-init t)            ;for Emacs
    (add-hook 'after-make-frame-functions 'exwm-init t))) ;for Emacs Client



(provide 'exwm)

;;; exwm.el ends here
