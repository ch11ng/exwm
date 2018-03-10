;;; exwm-floating.el --- Floating Module for EXWM  -*- lexical-binding: t -*-

;; Copyright (C) 2015-2018 Free Software Foundation, Inc.

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

;; This module deals with the conversion between floating and non-floating
;; states and implements moving/resizing operations on floating windows.

;;; Code:

(require 'xcb-cursor)
(require 'exwm-core)

(defgroup exwm-floating nil
  "Floating."
  :version "25.3"
  :group 'exwm)

(defcustom exwm-floating-setup-hook nil
  "Normal hook run when an X window has been made floating, in the
context of the corresponding buffer."
  :type 'hook)

(defcustom exwm-floating-exit-hook nil
  "Normal hook run when an X window has exited floating state, in the
context of the corresponding buffer."
  :type 'hook)

(defcustom exwm-floating-border-color "navy"
  "Border color of floating windows."
  :type 'color)

(defcustom exwm-floating-border-width 1
  "Border width of floating windows."
  :type 'integer)

(defvar exwm-floating--border-colormap nil
  "Colormap used by the border pixel.

This is also used by X window containers.")
(defvar exwm-floating--border-pixel nil
  "Border pixel drawn around floating X windows.")

;; Cursors for moving/resizing a window
(defvar exwm-floating--cursor-move nil)
(defvar exwm-floating--cursor-top-left nil)
(defvar exwm-floating--cursor-top nil)
(defvar exwm-floating--cursor-top-right nil)
(defvar exwm-floating--cursor-right nil)
(defvar exwm-floating--cursor-bottom-right nil)
(defvar exwm-floating--cursor-bottom nil)
(defvar exwm-floating--cursor-bottom-left nil)
(defvar exwm-floating--cursor-left nil)

(defvar exwm-floating--moveresize-calculate nil
  "Calculate move/resize parameters [buffer event-mask x y width height].")

(defvar exwm-workspace--current)
(defvar exwm-workspace--workareas)
(declare-function exwm-layout--hide "exwm-layout.el" (id))
(declare-function exwm-layout--iconic-state-p "exwm-layout.el" (&optional id))
(declare-function exwm-layout--refresh "exwm-layout.el" ())
(declare-function exwm-layout--show "exwm-layout.el" (id &optional window))
(declare-function exwm-workspace--position "exwm-workspace.el" (frame))

(defun exwm-floating--set-allowed-actions (id tilling)
  "Set _NET_WM_ALLOWED_ACTIONS."
  (xcb:+request exwm--connection
      (make-instance 'xcb:ewmh:set-_NET_WM_ALLOWED_ACTIONS
                     :window id
                     :data (if tilling
                               (vector xcb:Atom:_NET_WM_ACTION_MINIMIZE
                                       xcb:Atom:_NET_WM_ACTION_FULLSCREEN
                                       xcb:Atom:_NET_WM_ACTION_CHANGE_DESKTOP
                                       xcb:Atom:_NET_WM_ACTION_CLOSE)
                             (vector xcb:Atom:_NET_WM_ACTION_MOVE
                                     xcb:Atom:_NET_WM_ACTION_RESIZE
                                     xcb:Atom:_NET_WM_ACTION_MINIMIZE
                                     xcb:Atom:_NET_WM_ACTION_FULLSCREEN
                                     xcb:Atom:_NET_WM_ACTION_CHANGE_DESKTOP
                                     xcb:Atom:_NET_WM_ACTION_CLOSE)))))

(defun exwm-floating--set-floating (id)
  "Make window ID floating."
  (let ((window (get-buffer-window (exwm--id->buffer id))))
    (when window
      ;; Hide the non-floating X window first.
      (set-window-buffer window (other-buffer nil t))))
  (let* ((original-frame (buffer-local-value 'exwm--frame
                                             (exwm--id->buffer id)))
         ;; Create new frame
         (frame (with-current-buffer
                    (or (get-buffer "*scratch*")
                        (progn
                          (set-buffer-major-mode
                           (get-buffer-create "*scratch*"))
                          (get-buffer "*scratch*")))
                  (make-frame
                   `((minibuffer . ,(minibuffer-window exwm--frame))
                     (left . ,(* window-min-width -100))
                     (top . ,(* window-min-height -100))
                     (width . ,window-min-width)
                     (height . ,window-min-height)
                     (unsplittable . t))))) ;and fix the size later
         (outer-id (string-to-number (frame-parameter frame 'outer-window-id)))
         (window-id (string-to-number (frame-parameter frame 'window-id)))
         (frame-container (xcb:generate-id exwm--connection))
         (window (frame-first-window frame)) ;and it's the only window
         (x (slot-value exwm--geometry 'x))
         (y (slot-value exwm--geometry 'y))
         (width (slot-value exwm--geometry 'width))
         (height (slot-value exwm--geometry 'height)))
    (exwm--log "Floating geometry (original): %dx%d%+d%+d" width height x y)
    ;; Save frame parameters.
    (set-frame-parameter frame 'exwm-outer-id outer-id)
    (set-frame-parameter frame 'exwm-id window-id)
    (set-frame-parameter frame 'exwm-container frame-container)
    ;; Fix illegal parameters
    ;; FIXME: check normal hints restrictions
    (let* ((workarea (elt exwm-workspace--workareas
                          (exwm-workspace--position original-frame)))
           (x* (aref workarea 0))
           (y* (aref workarea 1))
           (width* (aref workarea 2))
           (height* (aref workarea 3)))
      ;; Center floating windows
      (when (and (or (= x 0) (= x x*))
                 (or (= y 0) (= y y*)))
        (let ((buffer (exwm--id->buffer exwm-transient-for))
              window edges)
          (when (and buffer (setq window (get-buffer-window buffer)))
            (setq edges (window-inside-absolute-pixel-edges window))
            (unless (and (<= width (- (elt edges 2) (elt edges 0)))
                         (<= height (- (elt edges 3) (elt edges 1))))
              (setq edges nil)))
          (if edges
              ;; Put at the center of leading window
              (setq x (+ x* (/ (- (elt edges 2) (elt edges 0) width) 2))
                    y (+ y* (/ (- (elt edges 3) (elt edges 1) height) 2)))
            ;; Put at the center of screen
            (setq x (/ (- width* width) 2)
                  y (/ (- height* height) 2)))))
      (if (> width width*)
          ;; Too wide
          (progn (setq x x*
                       width width*))
        ;; Invalid width
        (when (= 0 width) (setq width (/ width* 2)))
        ;; Make sure at least half of the window is visible
        (unless (< x* (+ x (/ width 2)) (+ x* width*))
          (setq x (+ x* (/ (- width* width) 2)))))
      (if (> height height*)
          ;; Too tall
          (setq y y*
                height height*)
        ;; Invalid height
        (when (= 0 height) (setq height (/ height* 2)))
        ;; Make sure at least half of the window is visible
        (unless (< y* (+ y (/ height 2)) (+ y* height*))
          (setq y (+ y* (/ (- height* height) 2)))))
      ;; The geometry can be overridden by user options.
      (let ((x** (plist-get exwm--configurations 'x))
            (y** (plist-get exwm--configurations 'y))
            (width** (plist-get exwm--configurations 'width))
            (height** (plist-get exwm--configurations 'height)))
        (if (integerp x**)
            (setq x (+ x* x**))
          (when (and (floatp x**)
                     (>= 1 x** 0))
            (setq x (+ x* (round (* x** width*))))))
        (if (integerp y**)
            (setq y (+ y* y**))
          (when (and (floatp y**)
                     (>= 1 y** 0))
            (setq y (+ y* (round (* y** height*))))))
        (if (integerp width**)
            (setq width width**)
          (when (and (floatp width**)
                     (> 1 width** 0))
            (setq width (max 1 (round (* width** width*))))))
        (if (integerp height**)
            (setq height height**)
          (when (and (floatp height**)
                     (> 1 height** 0))
            (setq height (max 1 (round (* height** height*))))))))
    (exwm--set-geometry id x y nil nil)
    (xcb:flush exwm--connection)
    (exwm--log "Floating geometry (corrected): %dx%d%+d%+d" width height x y)
    ;; Fit frame to client
    ;; It seems we have to make the frame invisible in order to resize it
    ;; timely.
    ;; The frame will be made visible by `select-frame-set-input-focus'.
    (make-frame-invisible frame)
    (let* ((edges (window-inside-pixel-edges window))
           (frame-width (+ width (- (frame-pixel-width frame)
                                    (- (elt edges 2) (elt edges 0)))))
           (frame-height (+ height (- (frame-pixel-height frame)
                                      (- (elt edges 3) (elt edges 1)))))
           (floating-mode-line (plist-get exwm--configurations
                                          'floating-mode-line))
           (floating-header-line (plist-get exwm--configurations
                                            'floating-header-line)))
      (if floating-mode-line
          (setq exwm--mode-line-format (or exwm--mode-line-format
                                           mode-line-format)
                mode-line-format floating-mode-line)
        (if (and (not (plist-member exwm--configurations 'floating-mode-line))
                 exwm--mwm-hints-decorations)
            (when exwm--mode-line-format
              (setq mode-line-format exwm--mode-line-format))
          ;; The mode-line need to be hidden in floating mode.
          (setq frame-height (- frame-height (window-mode-line-height
                                              (frame-root-window frame)))
                exwm--mode-line-format (or exwm--mode-line-format
                                           mode-line-format)
                mode-line-format nil)))
      (if floating-header-line
          (setq header-line-format floating-header-line)
        (if (and (not (plist-member exwm--configurations
                                    'floating-header-line))
                 exwm--mwm-hints-decorations)
            (setq header-line-format nil)
          ;; The header-line need to be hidden in floating header.
          (setq frame-height (- frame-height (window-header-line-height
                                              (frame-root-window frame)))
                header-line-format nil)))
      (set-frame-size frame frame-width frame-height t)
      ;; Create the frame container as the parent of the frame.
      (xcb:+request exwm--connection
          (make-instance 'xcb:CreateWindow
                         :depth 0
                         :wid frame-container
                         :parent exwm--root
                         :x (- x (elt edges 0))
                         :y (- y (elt edges 1))
                         :width width
                         :height height
                         :border-width
                         (with-current-buffer (exwm--id->buffer id)
                           (let ((border-witdh (plist-get exwm--configurations
                                                          'border-width)))
                             (if (and (integerp border-witdh)
                                      (>= border-witdh 0))
                                 border-witdh
                               exwm-floating-border-width)))
                         :class xcb:WindowClass:InputOutput
                         :visual 0
                         :value-mask (logior xcb:CW:BackPixmap
                                             (if exwm-floating--border-pixel
                                                 xcb:CW:BorderPixel 0)
                                             xcb:CW:OverrideRedirect
                                             (if exwm-floating--border-colormap
                                                 xcb:CW:Colormap 0))
                         :background-pixmap xcb:BackPixmap:ParentRelative
                         :border-pixel exwm-floating--border-pixel
                         :override-redirect 1
                         :colormap exwm-floating--border-colormap))
      (xcb:+request exwm--connection
          (make-instance 'xcb:ewmh:set-_NET_WM_NAME
                         :window frame-container
                         :data
                         (format "EXWM floating frame container for 0x%x" id)))
      ;; Map it.
      (xcb:+request exwm--connection
          (make-instance 'xcb:MapWindow :window frame-container))
      ;; Put the X window right above this frame container.
      (xcb:+request exwm--connection
          (make-instance 'xcb:ConfigureWindow
                         :window id
                         :value-mask (logior xcb:ConfigWindow:Sibling
                                             xcb:ConfigWindow:StackMode)
                         :sibling frame-container
                         :stack-mode xcb:StackMode:Above)))
    ;; Reparent this frame to its container.
    (xcb:+request exwm--connection
        (make-instance 'xcb:ReparentWindow
                       :window outer-id :parent frame-container :x 0 :y 0))
    (exwm-floating--set-allowed-actions id nil)
    (xcb:flush exwm--connection)
    ;; Set window/buffer
    (with-current-buffer (exwm--id->buffer id)
      (setq window-size-fixed exwm--fixed-size
            exwm--floating-frame frame)
      ;; Do the refresh manually.
      (remove-hook 'window-configuration-change-hook #'exwm-layout--refresh)
      (set-window-buffer window (current-buffer)) ;this changes current buffer
      (add-hook 'window-configuration-change-hook #'exwm-layout--refresh)
      (set-window-dedicated-p window t)
      (exwm-layout--show id window))
    (with-current-buffer (exwm--id->buffer id)
      (if (exwm-layout--iconic-state-p id)
          ;; Hide iconic floating X windows.
          (exwm-floating-hide)
        (with-selected-frame exwm--frame
          (exwm-layout--refresh)))
      (select-frame-set-input-focus frame))
    ;; FIXME: Strangely, the Emacs frame can move itself at this point
    ;;        when there are left/top struts set.  Force resetting its
    ;;        position seems working, but it'd better to figure out why.
    ;; FIXME: This also happens in another case (#220) where the cause is
    ;;        still unclear.
    (exwm--set-geometry outer-id 0 0 nil nil)
    (xcb:flush exwm--connection))
  (with-current-buffer (exwm--id->buffer id)
    (run-hooks 'exwm-floating-setup-hook))
  ;; Redraw the frame.
  (redisplay))

(defun exwm-floating--unset-floating (id)
  "Make window ID non-floating."
  (let ((buffer (exwm--id->buffer id)))
    (with-current-buffer buffer
      (when exwm--floating-frame
        ;; The X window is already mapped.
        ;; Unmap the X window.
        (xcb:+request exwm--connection
            (make-instance 'xcb:ChangeWindowAttributes
                           :window id :value-mask xcb:CW:EventMask
                           :event-mask xcb:EventMask:NoEvent))
        (xcb:+request exwm--connection
            (make-instance 'xcb:UnmapWindow :window id))
        (xcb:+request exwm--connection
            (make-instance 'xcb:ChangeWindowAttributes
                           :window id :value-mask xcb:CW:EventMask
                           :event-mask exwm--client-event-mask))
        ;; Reparent the floating frame back to the root window.
        (let ((frame-id (frame-parameter exwm--floating-frame 'exwm-outer-id))
              (frame-container (frame-parameter exwm--floating-frame
                                                'exwm-container)))
          (xcb:+request exwm--connection
              (make-instance 'xcb:UnmapWindow :window frame-id))
          (xcb:+request exwm--connection
              (make-instance 'xcb:ReparentWindow
                             :window frame-id
                             :parent exwm--root
                             :x 0 :y 0))
          ;; Also destroy its container.
          (xcb:+request exwm--connection
              (make-instance 'xcb:DestroyWindow :window frame-container))))
      ;; Place the X window just above the reference X window.
      ;; (the stacking order won't change from now on).
      ;; Also hide the possible floating border.
      (xcb:+request exwm--connection
          (make-instance 'xcb:ConfigureWindow
                         :window id
                         :value-mask (logior xcb:ConfigWindow:BorderWidth
                                             xcb:ConfigWindow:Sibling
                                             xcb:ConfigWindow:StackMode)
                         :border-width 0
                         :sibling exwm--guide-window
                         :stack-mode xcb:StackMode:Above)))
    (exwm-floating--set-allowed-actions id t)
    (xcb:flush exwm--connection)
    (with-current-buffer buffer
      (when exwm--floating-frame        ;from floating to non-floating
        (set-window-dedicated-p (frame-first-window exwm--floating-frame) nil)
        ;; Select a tiling window and delete the old frame.
        (select-window (frame-selected-window exwm-workspace--current))
        (with-current-buffer buffer
          (delete-frame exwm--floating-frame))))
    (with-current-buffer buffer
      (setq window-size-fixed nil
            exwm--floating-frame nil)
      (if (not (plist-member exwm--configurations 'tiling-mode-line))
          (when exwm--mode-line-format
            (setq mode-line-format exwm--mode-line-format))
        (setq exwm--mode-line-format (or exwm--mode-line-format
                                         mode-line-format)
              mode-line-format (plist-get exwm--configurations
                                          'tiling-mode-line)))
      (if (not (plist-member exwm--configurations 'tiling-header-line))
          (setq header-line-format nil)
        (setq header-line-format (plist-get exwm--configurations
                                            'tiling-header-line))))
    ;; Only show X windows in normal state.
    (unless (exwm-layout--iconic-state-p)
      (pop-to-buffer-same-window buffer)))
  (with-current-buffer (exwm--id->buffer id)
    (run-hooks 'exwm-floating-exit-hook)))

;;;###autoload
(cl-defun exwm-floating-toggle-floating ()
  "Toggle the current window between floating and non-floating states."
  (interactive)
  (unless (derived-mode-p 'exwm-mode)
    (cl-return-from exwm-floating-toggle-floating))
  (with-current-buffer (window-buffer)
    (if exwm--floating-frame
        (exwm-floating--unset-floating exwm--id)
      (exwm-floating--set-floating exwm--id))))

;;;###autoload
(defun exwm-floating-hide ()
  "Hide the current floating X window (which would show again when selected)."
  (interactive)
  (when (and (eq major-mode 'exwm-mode)
             exwm--floating-frame)
    (exwm-layout--hide exwm--id)
    (select-frame-set-input-focus exwm-workspace--current)))

(defun exwm-floating--start-moveresize (id &optional type)
  "Start move/resize."
  (let ((buffer-or-id (or (exwm--id->buffer id) id))
        frame container-or-id x y width height cursor)
    (if (bufferp buffer-or-id)
        ;; Managed.
        (with-current-buffer buffer-or-id
          (setq frame exwm--floating-frame
                container-or-id (frame-parameter exwm--floating-frame
                                                 'exwm-container)))
      ;; Unmanaged.
      (setq container-or-id id))
    (when (and container-or-id
               ;; Test if the pointer can be grabbed
               (= xcb:GrabStatus:Success
                  (slot-value
                   (xcb:+request-unchecked+reply exwm--connection
                       (make-instance 'xcb:GrabPointer
                                      :owner-events 0
                                      :grab-window container-or-id
                                      :event-mask xcb:EventMask:NoEvent
                                      :pointer-mode xcb:GrabMode:Async
                                      :keyboard-mode xcb:GrabMode:Async
                                      :confine-to xcb:Window:None
                                      :cursor xcb:Cursor:None
                                      :time xcb:Time:CurrentTime))
                   'status)))
      (with-slots (root-x root-y win-x win-y)
          (xcb:+request-unchecked+reply exwm--connection
              (make-instance 'xcb:QueryPointer :window id))
        (if (not (bufferp buffer-or-id))
            ;; Unmanaged.
            (unless (eq type xcb:ewmh:_NET_WM_MOVERESIZE_MOVE)
              (with-slots ((width* width)
                           (height* height))
                  (xcb:+request-unchecked+reply exwm--connection
                      (make-instance 'xcb:GetGeometry :drawable id))
                (setq width width*
                      height height*)))
          ;; Managed.
          (select-window (frame-first-window frame)) ;transfer input focus
          (setq width (frame-pixel-width frame)
                height (frame-pixel-height frame))
          (unless type
            ;; Determine the resize type according to the pointer position
            ;; Clicking the center 1/3 part to resize has not effect
            (setq x (/ (* 3 win-x) (float width))
                  y (/ (* 3 win-y) (float height))
                  type (cond ((and (< x 1) (< y 1))
                              xcb:ewmh:_NET_WM_MOVERESIZE_SIZE_TOPLEFT)
                             ((and (> x 2) (< y 1))
                              xcb:ewmh:_NET_WM_MOVERESIZE_SIZE_TOPRIGHT)
                             ((and (> x 2) (> y 2))
                              xcb:ewmh:_NET_WM_MOVERESIZE_SIZE_BOTTOMRIGHT)
                             ((and (< x 1) (> y 2))
                              xcb:ewmh:_NET_WM_MOVERESIZE_SIZE_BOTTOMLEFT)
                             ((> x 2) xcb:ewmh:_NET_WM_MOVERESIZE_SIZE_RIGHT)
                             ((> y 2) xcb:ewmh:_NET_WM_MOVERESIZE_SIZE_BOTTOM)
                             ((< x 1) xcb:ewmh:_NET_WM_MOVERESIZE_SIZE_LEFT)
                             ((< y 1) xcb:ewmh:_NET_WM_MOVERESIZE_SIZE_TOP)))))
        (if (not type)
            (exwm-floating--stop-moveresize)
          (cond ((= type xcb:ewmh:_NET_WM_MOVERESIZE_MOVE)
                 (setq cursor exwm-floating--cursor-move
                       exwm-floating--moveresize-calculate
                       `(lambda (x y)
                          (vector ,buffer-or-id
                                  ,(eval-when-compile
                                     (logior xcb:ConfigWindow:X
                                             xcb:ConfigWindow:Y))
                                  (- x ,win-x) (- y ,win-y) 0 0))))
                ((= type xcb:ewmh:_NET_WM_MOVERESIZE_SIZE_TOPLEFT)
                 (setq cursor exwm-floating--cursor-top-left
                       exwm-floating--moveresize-calculate
                       `(lambda (x y)
                          (vector ,buffer-or-id
                                  ,(eval-when-compile
                                     (logior xcb:ConfigWindow:X
                                             xcb:ConfigWindow:Y
                                             xcb:ConfigWindow:Width
                                             xcb:ConfigWindow:Height))
                                  (- x ,win-x) (- y ,win-y)
                                  (- ,(+ root-x width) x)
                                  (- ,(+ root-y height) y)))))
                ((= type xcb:ewmh:_NET_WM_MOVERESIZE_SIZE_TOP)
                 (setq cursor exwm-floating--cursor-top
                       exwm-floating--moveresize-calculate
                       `(lambda (x y)
                          (vector ,buffer-or-id
                                  ,(eval-when-compile
                                     (logior xcb:ConfigWindow:Y
                                             xcb:ConfigWindow:Height))
                                  0 (- y ,win-y) 0 (- ,(+ root-y height) y)))))
                ((= type xcb:ewmh:_NET_WM_MOVERESIZE_SIZE_TOPRIGHT)
                 (setq cursor exwm-floating--cursor-top-right
                       exwm-floating--moveresize-calculate
                       `(lambda (x y)
                          (vector ,buffer-or-id
                                  ,(eval-when-compile
                                     (logior xcb:ConfigWindow:Y
                                             xcb:ConfigWindow:Width
                                             xcb:ConfigWindow:Height))
                                  0 (- y ,win-y) (- x ,(- root-x width))
                                  (- ,(+ root-y height) y)))))
                ((= type xcb:ewmh:_NET_WM_MOVERESIZE_SIZE_RIGHT)
                 (setq cursor exwm-floating--cursor-right
                       exwm-floating--moveresize-calculate
                       `(lambda (x y)
                          (vector ,buffer-or-id
                                  ,xcb:ConfigWindow:Width
                                  0 0 (- x ,(- root-x width)) 0))))
                ((= type xcb:ewmh:_NET_WM_MOVERESIZE_SIZE_BOTTOMRIGHT)
                 (setq cursor exwm-floating--cursor-bottom-right
                       exwm-floating--moveresize-calculate
                       `(lambda (x y)
                          (vector ,buffer-or-id
                                  ,(eval-when-compile
                                     (logior xcb:ConfigWindow:Width
                                             xcb:ConfigWindow:Height))
                                  0 0 (- x ,(- root-x width))
                                  (- y ,(- root-y height))))))
                ((= type xcb:ewmh:_NET_WM_MOVERESIZE_SIZE_BOTTOM)
                 (setq cursor exwm-floating--cursor-bottom
                       exwm-floating--moveresize-calculate
                       `(lambda (x y)
                          (vector ,buffer-or-id
                                  ,xcb:ConfigWindow:Height
                                  0 0 0 (- y ,(- root-y height))))))
                ((= type xcb:ewmh:_NET_WM_MOVERESIZE_SIZE_BOTTOMLEFT)
                 (setq cursor exwm-floating--cursor-bottom-left
                       exwm-floating--moveresize-calculate
                       `(lambda (x y)
                          (vector ,buffer-or-id
                                  ,(eval-when-compile
                                     (logior xcb:ConfigWindow:X
                                             xcb:ConfigWindow:Width
                                             xcb:ConfigWindow:Height))
                                  (- x ,win-x)
                                  0
                                  (- ,(+ root-x width) x)
                                  (- y ,(- root-y height))))))
                ((= type xcb:ewmh:_NET_WM_MOVERESIZE_SIZE_LEFT)
                 (setq cursor exwm-floating--cursor-left
                       exwm-floating--moveresize-calculate
                       `(lambda (x y)
                          (vector ,buffer-or-id
                                  ,(eval-when-compile
                                     (logior xcb:ConfigWindow:X
                                             xcb:ConfigWindow:Width))
                                  (- x ,win-x) 0 (- ,(+ root-x width) x) 0)))))
          ;; Select events and change cursor (should always succeed)
          (xcb:+request-unchecked+reply exwm--connection
              (make-instance 'xcb:GrabPointer
                             :owner-events 0 :grab-window container-or-id
                             :event-mask (eval-when-compile
                                           (logior xcb:EventMask:ButtonRelease
                                                   xcb:EventMask:ButtonMotion))
                             :pointer-mode xcb:GrabMode:Async
                             :keyboard-mode xcb:GrabMode:Async
                             :confine-to xcb:Window:None
                             :cursor cursor
                             :time xcb:Time:CurrentTime)))))))

(defun exwm-floating--stop-moveresize (&rest _args)
  "Stop move/resize."
  (xcb:+request exwm--connection
      (make-instance 'xcb:UngrabPointer :time xcb:Time:CurrentTime))
  (when exwm-floating--moveresize-calculate
    (let (result buffer-or-id)
      (setq result (funcall exwm-floating--moveresize-calculate 0 0)
            buffer-or-id (aref result 0))
      (when (bufferp buffer-or-id)
        (with-current-buffer buffer-or-id
          (exwm-layout--show exwm--id
                             (frame-root-window exwm--floating-frame)))))
    (setq exwm-floating--moveresize-calculate nil)))

(defun exwm-floating--do-moveresize (data _synthetic)
  "Perform move/resize."
  (when exwm-floating--moveresize-calculate
    (let* ((obj (make-instance 'xcb:MotionNotify))
           result value-mask x y width height buffer-or-id container-or-id)
      (xcb:unmarshal obj data)
      (setq result (funcall exwm-floating--moveresize-calculate
                            (slot-value obj 'root-x) (slot-value obj 'root-y))
            buffer-or-id (aref result 0)
            value-mask (aref result 1)
            x (aref result 2)
            y (aref result 3)
            width (max 1 (aref result 4))
            height (max 1 (aref result 5)))
      (setq container-or-id
            (if (bufferp buffer-or-id)
                ;; Managed.
                (with-current-buffer buffer-or-id
                  (frame-parameter exwm--floating-frame 'exwm-container))
              ;; Unmanaged.
              buffer-or-id))
      (xcb:+request exwm--connection
          (make-instance 'xcb:ConfigureWindow
                         :window container-or-id
                         :value-mask (aref result 1)
                         :x x
                         :y y
                         :width width
                         :height height))
      (when (bufferp buffer-or-id)
        ;; Managed.
        (setq value-mask (logand value-mask (logior xcb:ConfigWindow:Width
                                                    xcb:ConfigWindow:Height)))
        (when (/= 0 value-mask)
          (with-current-buffer buffer-or-id
            (xcb:+request exwm--connection
                (make-instance 'xcb:ConfigureWindow
                               :window (frame-parameter exwm--floating-frame
                                                        'exwm-outer-id)
                               :value-mask value-mask
                               :width width
                               :height height)))))
      (xcb:flush exwm--connection))))

(defun exwm-floating-move (&optional delta-x delta-y)
  "Move a floating window right by DELTA-X pixels and down by DELTA-Y pixels.

Both DELTA-X and DELTA-Y default to 1.  This command should be bound locally."
  (unless (and (eq major-mode 'exwm-mode) exwm--floating-frame)
    (user-error "[EXWM] `exwm-floating-move' is only for floating X windows"))
  (unless delta-x (setq delta-x 1))
  (unless delta-y (setq delta-y 1))
  (unless (and (= 0 delta-x) (= 0 delta-y))
    (let* ((floating-container (frame-parameter exwm--floating-frame
                                                'exwm-container))
           (geometry (xcb:+request-unchecked+reply exwm--connection
                         (make-instance 'xcb:GetGeometry
                                        :drawable floating-container)))
           (edges (window-inside-absolute-pixel-edges)))
      (with-slots (x y) geometry
        (exwm--set-geometry floating-container
                            (+ x delta-x) (+ y delta-y) nil nil))
      (exwm--set-geometry exwm--id
                          (+ (pop edges) delta-x)
                          (+ (pop edges) delta-y)
                          nil nil))
    (xcb:flush exwm--connection)))

(defun exwm-floating--init ()
  "Initialize floating module."
  ;; Check border width.
  (unless (and (integerp exwm-floating-border-width)
               (> exwm-floating-border-width 0))
    (setq exwm-floating-border-width 0))
  ;; Initialize border pixel.
  (when (> exwm-floating-border-width 0)
    (setq exwm-floating--border-colormap
          (slot-value (car (slot-value
                            (xcb:get-setup exwm--connection) 'roots))
                      'default-colormap))
    (unless (stringp exwm-floating-border-color)
      (setq exwm-floating-border-color ""))
    (let* ((color (x-color-values exwm-floating-border-color))
           reply)
      (when color
        (setq reply (xcb:+request-unchecked+reply exwm--connection
                        (make-instance 'xcb:AllocColor
                                       :cmap exwm-floating--border-colormap
                                       :red (pop color)
                                       :green (pop color)
                                       :blue (pop color))))
        (when reply
          (setq exwm-floating--border-pixel (slot-value reply 'pixel))))))
  ;; Initialize cursors for moving/resizing a window
  (xcb:cursor:init exwm--connection)
  (setq exwm-floating--cursor-move
        (xcb:cursor:load-cursor exwm--connection "fleur")
        exwm-floating--cursor-top-left
        (xcb:cursor:load-cursor exwm--connection "top_left_corner")
        exwm-floating--cursor-top
        (xcb:cursor:load-cursor exwm--connection "top_side")
        exwm-floating--cursor-top-right
        (xcb:cursor:load-cursor exwm--connection "top_right_corner")
        exwm-floating--cursor-right
        (xcb:cursor:load-cursor exwm--connection "right_side")
        exwm-floating--cursor-bottom-right
        (xcb:cursor:load-cursor exwm--connection "bottom_right_corner")
        exwm-floating--cursor-bottom
        (xcb:cursor:load-cursor exwm--connection "bottom_side")
        exwm-floating--cursor-bottom-left
        (xcb:cursor:load-cursor exwm--connection "bottom_left_corner")
        exwm-floating--cursor-left
        (xcb:cursor:load-cursor exwm--connection "left_side")))

(defun exwm-floating--exit ()
  "Exit the floating module.")



(provide 'exwm-floating)

;;; exwm-floating.el ends here
