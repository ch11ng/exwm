;;; exwm-floating.el --- Floating Module for EXWM  -*- lexical-binding: t -*-

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

;; This module deals with the conversion between floating and non-floating
;; states and implements moving/resizing operations on floating windows.

;;; Code:

(require 'xcb-cursor)
(require 'exwm-core)
(eval-when-compile (require 'exwm-workspace))

(defvar exwm-floating-border-width 1 "Border width of the floating window.")
(defvar exwm-floating-border-color "navy"
  "Border color of the floating window.")

(defvar exwm-floating-setup-hook nil
  "Normal hook run when a window has been made floating.")
(defvar exwm-floating-exit-hook nil
  "Normal hook run when a window has exited floating state.")

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

;;;###autoload
(defun exwm-floating--set-floating (id)
  "Make window ID floating."
  (interactive)
  (let ((window (get-buffer-window (exwm--id->buffer id))))
    (when window                        ;window in non-floating state
      (set-window-buffer window (other-buffer)))) ;hide it first
  (let* ((original-frame
          (with-current-buffer (exwm--id->buffer id)
            (if (and exwm-transient-for (exwm--id->buffer exwm-transient-for))
                ;; Place a modal in the same workspace with its leading window
                (with-current-buffer (exwm--id->buffer exwm-transient-for)
                  exwm--frame)
              ;; Fallback to current workspace
              exwm-workspace--current)))
         (original-id (frame-parameter original-frame 'exwm-window-id))
         ;; Create new frame
         (frame (with-current-buffer
                    (or (get-buffer "*scratch*")
                        (progn
                          (set-buffer-major-mode
                           (get-buffer-create "*scratch*"))
                          (get-buffer "*scratch*")))
                  (prog2
                      (exwm--lock)
                      (make-frame
                       `((minibuffer . nil) ;use the one on workspace
                         (background-color . ,exwm-floating-border-color)
                         (internal-border-width . ,exwm-floating-border-width)
                         (left . 10000)
                         (top . 10000)
                         (unsplittable . t))) ;and fix the size later
                    (exwm--unlock))))
         (frame-id (string-to-number (frame-parameter frame 'window-id)))
         (outer-id (string-to-number (frame-parameter frame 'outer-window-id)))
         (window (frame-first-window frame)) ;and it's the only window
         (x (slot-value exwm--geometry 'x))
         (y (slot-value exwm--geometry 'y))
         (width (slot-value exwm--geometry 'width))
         (height (slot-value exwm--geometry 'height))
         (frame-geometry (frame-parameter original-frame 'exwm-geometry)))
    (exwm--log "Floating geometry (original, absolute): %dx%d%+d%+d"
               width height x y)
    (when frame-geometry
      (setq x (- x (slot-value frame-geometry 'x))
            y (- y (slot-value frame-geometry 'y))))
    (exwm--log "Floating geometry (original, relative): %dx%d%+d%+d"
               width height x y)
    ;; Save window IDs
    (set-frame-parameter frame 'exwm-window-id frame-id)
    (set-frame-parameter frame 'exwm-outer-id outer-id)
    ;; Set urgency flag if it's not appear in the active workspace
    (let ((idx (cl-position original-frame exwm-workspace--list)))
      (when (/= idx exwm-workspace-current-index)
        (set-frame-parameter original-frame 'exwm--urgency t)
        (setq exwm-workspace--switch-history-outdated t)))
    ;; Fix illegal parameters
    ;; FIXME: check normal hints restrictions
    (let* ((display-width (frame-pixel-width original-frame))
           (display-height (- (frame-pixel-height original-frame)
                              (window-pixel-height (minibuffer-window
                                                    original-frame))
                              (* 2 (window-mode-line-height))
                              (window-header-line-height window)
                              (* 2 exwm-floating-border-width)))
           (display-height (* 2 (/ display-height 2)))) ;round to even
      (if (> width display-width)
          ;; Too wide
          (progn (setq x 0
                       width display-width))
        ;; Invalid width
        (when (= 0 width) (setq width (/ display-width 2)))
        ;; Make sure at least half of the window is visible
        (when (or (> (+ x (/ width 2)) display-width) (> 0 (+ x (/ width 2))))
          (setq x (/ (- display-width width) 2))))
      (if (> height display-height)
          ;; Too tall
          (setq y 0
                height display-height)
        ;; Invalid height
        (when (= 0 height) (setq height (/ display-height 2)))
        ;; Make sure at least half of the window is visible
        (when (or (> (+ y (/ height 2)) display-height)
                  (> 0 (+ y (/ height 2))))
          (setq y (/ (- display-height height) 2))))
      ;; Center floating windows
      (when (and (= x 0) (= y 0))
        (let ((buffer (exwm--id->buffer exwm-transient-for))
              window edges)
          (when (and buffer (setq window (get-buffer-window buffer)))
            (setq edges (window-inside-absolute-pixel-edges window))
            (unless (and (<= width (- (elt edges 2) (elt edges 0)))
                         (<= height (- (elt edges 3) (elt edges 1))))
              (setq edges nil)))
          (if edges
              ;; Put at the center of leading window
              (setq x (/ (- (elt edges 2) (elt edges 0) width) 2)
                    y (/ (- (elt edges 3) (elt edges 1) height) 2))
            ;; Put at the center of screen
            (setq x (/ (- display-width width) 2)
                  y (/ (- display-height height) 2))))))
    (exwm--log "Floating geometry (corrected): %dx%d%+d%+d" width height x y)
    ;; Set event mask
    (xcb:+request exwm--connection
        (make-instance 'xcb:ChangeWindowAttributes
                       :window frame-id :value-mask xcb:CW:EventMask
                       :event-mask xcb:EventMask:SubstructureRedirect))
    ;; Save the geometry
    ;; Rationale: the frame will not be ready for some time, thus we cannot
    ;;            infer the correct window size from its geometry.
    (with-current-buffer (exwm--id->buffer id)
      (setq exwm--floating-edges (vector x y (+ width x) (+ height y))))
    ;; Fit frame to client
    (exwm-floating--fit-frame-to-window outer-id width height)
    ;; Reparent window to this frame
    (xcb:+request exwm--connection
        (make-instance 'xcb:ChangeWindowAttributes
                       :window id :value-mask xcb:CW:EventMask
                       :event-mask xcb:EventMask:NoEvent))
    (xcb:+request exwm--connection
        (make-instance 'xcb:ReparentWindow
                       :window id :parent frame-id
                       :x exwm-floating-border-width
                       :y exwm-floating-border-width))
    (xcb:+request exwm--connection
        (make-instance 'xcb:ChangeWindowAttributes
                       :window id :value-mask xcb:CW:EventMask
                       :event-mask exwm--client-event-mask))
    ;; Reparent this frame to the original one
    (xcb:+request exwm--connection
        (make-instance 'xcb:ReparentWindow
                       :window outer-id :parent original-id
                       :x (- x exwm-floating-border-width)
                       :y (- y exwm-floating-border-width)))
    (xcb:flush exwm--connection)
    ;; Set window/buffer
    (with-current-buffer (exwm--id->buffer id)
      (setq window-size-fixed exwm--fixed-size
            exwm--frame original-frame
            exwm--floating-frame frame)
      (set-window-buffer window (current-buffer)) ;this changes current buffer
      (set-window-dedicated-p window t))
    (select-window window))
  (run-hooks 'exwm-floating-setup-hook))

;;;###autoload
(defun exwm-floating--unset-floating (id)
  "Make window ID non-floating."
  (interactive)
  (let ((buffer (exwm--id->buffer id)))
    ;; Reparent to workspace frame
    (xcb:+request exwm--connection
        (make-instance 'xcb:ChangeWindowAttributes
                       :window id :value-mask xcb:CW:EventMask
                       :event-mask xcb:EventMask:NoEvent))
    (xcb:+request exwm--connection
        (make-instance 'xcb:ReparentWindow
                       :window id
                       :parent (frame-parameter exwm-workspace--current
                                                'exwm-window-id)
                       :x 0 :y 0))      ;temporary position
    (xcb:+request exwm--connection
        (make-instance 'xcb:ChangeWindowAttributes
                       :window id :value-mask xcb:CW:EventMask
                       :event-mask exwm--client-event-mask))
    (xcb:flush exwm--connection)
    (with-current-buffer buffer
      (when exwm--floating-frame        ;from floating to non-floating
        (setq exwm--floating-edges nil) ;invalid by now
        (set-window-dedicated-p (frame-first-window exwm--floating-frame) nil)
        (delete-frame exwm--floating-frame))) ;remove the floating frame
    (with-current-buffer buffer
      (setq window-size-fixed nil
            exwm--floating-frame nil
            exwm--frame exwm-workspace--current))
    (let ((window (frame-selected-window exwm-workspace--current)))
      (set-window-buffer window buffer)
      (select-window window)))
  (run-hooks 'exwm-floating-exit-hook))

;;;###autoload
(defun exwm-floating-toggle-floating ()
  "Toggle the current window between floating and non-floating states."
  (interactive)
  (with-current-buffer (window-buffer)
    (if exwm--floating-frame
        (exwm-floating--unset-floating exwm--id)
      (exwm-floating--set-floating exwm--id))))

(defun exwm-floating--fit-frame-to-window (&optional frame-outer-id
                                                     width height)
  "Resize a floating frame to make it fit the size of the window.

Default to resize `exwm--floating-frame' unless FRAME-OUTER-ID is non-nil.
This function will issue an `xcb:GetGeometry' request unless WIDTH and HEIGHT
are provided. You should call `xcb:flush' and restore the value of
`window-size-fixed' afterwards."
  (setq window-size-fixed nil)
  (unless (and width height)
    (let ((geometry (xcb:+request-unchecked+reply exwm--connection
                        (make-instance 'xcb:GetGeometry :drawable exwm--id))))
      (setq width (slot-value geometry 'width)
            height (slot-value geometry 'height))))
  (xcb:+request exwm--connection
      (make-instance 'xcb:ConfigureWindow
                     :window (or frame-outer-id
                                 (frame-parameter exwm--floating-frame
                                                  'exwm-outer-id))
                     :value-mask (eval-when-compile
                                   (logior xcb:ConfigWindow:Width
                                           xcb:ConfigWindow:Height
                                           xcb:ConfigWindow:StackMode))
                     :width (+ width (* 2 exwm-floating-border-width))
                     :height (+ height (* 2 exwm-floating-border-width)
                                (window-mode-line-height)
                                (window-header-line-height))
                     :stack-mode xcb:StackMode:Above))) ;top-most

(defun exwm-floating-hide-mode-line ()
  "Hide mode-line of a floating frame."
  (interactive)
  (unless (eq major-mode 'exwm-mode)
    (user-error "[EXWM] Please use this command with EXWM buffers"))
  (when (and exwm--floating-frame mode-line-format)
    (setq exwm--floating-mode-line-format mode-line-format
          mode-line-format nil)
    (exwm-floating--fit-frame-to-window)
    (xcb:flush exwm--connection)
    (setq window-size-fixed exwm--fixed-size)))

(defun exwm-floating-show-mode-line ()
  "Show mode-line of a floating frame."
  (interactive)
  (unless (eq major-mode 'exwm-mode)
    (user-error "[EXWM] Please use this command with EXWM buffers"))
  (when (and exwm--floating-frame (not mode-line-format))
    (setq mode-line-format exwm--floating-mode-line-format
          exwm--floating-mode-line-format nil)
    (exwm-floating--fit-frame-to-window)
    (exwm-input-grab-keyboard)       ;mode-line-format may be outdated
    (xcb:flush exwm--connection)
    (setq window-size-fixed exwm--fixed-size)))

(defvar exwm-floating--moveresize-calculate nil
  "Calculate move/resize parameters [frame-id event-mask x y width height].")

;;;###autoload
(defun exwm-floating--start-moveresize (id &optional type)
  "Start move/resize."
  (let ((buffer (exwm--id->buffer id))
        frame frame-id x y width height cursor)
    (when (and buffer
               (setq frame (with-current-buffer buffer exwm--floating-frame))
               (setq frame-id (frame-parameter frame 'exwm-outer-id))
               ;; Test if the pointer can be grabbed
               (= xcb:GrabStatus:Success
                  (slot-value
                   (xcb:+request-unchecked+reply exwm--connection
                       (make-instance 'xcb:GrabPointer
                                      :owner-events 0 :grab-window frame-id
                                      :event-mask xcb:EventMask:NoEvent
                                      :pointer-mode xcb:GrabMode:Async
                                      :keyboard-mode xcb:GrabMode:Async
                                      :confine-to xcb:Window:None
                                      :cursor xcb:Cursor:None
                                      :time xcb:Time:CurrentTime))
                   'status)))
      (setq exwm--floating-edges nil)   ;invalid by now
      (with-slots (root-x root-y win-x win-y)
          (xcb:+request-unchecked+reply exwm--connection
              (make-instance 'xcb:QueryPointer :window id))
        (select-frame-set-input-focus frame) ;raise and focus it
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
                           ((< y 1) xcb:ewmh:_NET_WM_MOVERESIZE_SIZE_TOP)
                           ((> x 2) xcb:ewmh:_NET_WM_MOVERESIZE_SIZE_RIGHT)
                           ((> y 2) xcb:ewmh:_NET_WM_MOVERESIZE_SIZE_BOTTOM)
                           ((< x 1) xcb:ewmh:_NET_WM_MOVERESIZE_SIZE_LEFT))))
        (if (not type)
            (exwm-floating--stop-moveresize)
          (cond ((= type xcb:ewmh:_NET_WM_MOVERESIZE_MOVE)
                 (setq cursor exwm-floating--cursor-move
                       exwm-floating--moveresize-calculate
                       `(lambda (x y)
                          (vector ,frame-id
                                  ,(eval-when-compile
                                     (logior xcb:ConfigWindow:X
                                             xcb:ConfigWindow:Y))
                                  (- x ,win-x) (- y ,win-y) 0 0))))
                ((= type xcb:ewmh:_NET_WM_MOVERESIZE_SIZE_TOPLEFT)
                 (setq cursor exwm-floating--cursor-top-left
                       exwm-floating--moveresize-calculate
                       `(lambda (x y)
                          (vector ,frame-id
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
                          (vector ,frame-id
                                  ,(eval-when-compile
                                     (logior xcb:ConfigWindow:Y
                                             xcb:ConfigWindow:Height))
                                  0 (- y ,win-y) 0 (- ,(+ root-y height) y)))))
                ((= type xcb:ewmh:_NET_WM_MOVERESIZE_SIZE_TOPRIGHT)
                 (setq cursor exwm-floating--cursor-top-right
                       exwm-floating--moveresize-calculate
                       `(lambda (x y)
                          (vector ,frame-id
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
                          (vector ,frame-id ,xcb:ConfigWindow:Width
                                  0 0 (- x ,(- root-x width)) 0))))
                ((= type xcb:ewmh:_NET_WM_MOVERESIZE_SIZE_BOTTOMRIGHT)
                 (setq cursor exwm-floating--cursor-bottom-right
                       exwm-floating--moveresize-calculate
                       `(lambda (x y)
                          (vector ,frame-id
                                  ,(eval-when-compile
                                     (logior xcb:ConfigWindow:Width
                                             xcb:ConfigWindow:Height))
                                  0 0 (- x ,(- root-x width))
                                  (- y ,(- root-y height))))))
                ((= type xcb:ewmh:_NET_WM_MOVERESIZE_SIZE_BOTTOM)
                 (setq cursor exwm-floating--cursor-bottom
                       exwm-floating--moveresize-calculate
                       `(lambda (x y)
                          (vector ,frame-id
                                  ,xcb:ConfigWindow:Height
                                  0 0 0 (- y ,(- root-y height))))))
                ((= type xcb:ewmh:_NET_WM_MOVERESIZE_SIZE_BOTTOMLEFT)
                 (setq cursor exwm-floating--cursor-bottom-left
                       exwm-floating--moveresize-calculate
                       `(lambda (x y)
                          (vector ,frame-id
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
                          (vector ,frame-id
                                  ,(eval-when-compile
                                     (logior xcb:ConfigWindow:X
                                             xcb:ConfigWindow:Width))
                                  (- x ,win-x) 0 (- ,(+ root-x width) x) 0)))))
          ;; Select events and change cursor (should always succeed)
          (xcb:+request-unchecked+reply exwm--connection
              (make-instance 'xcb:GrabPointer
                             :owner-events 0 :grab-window frame-id
                             :event-mask (eval-when-compile
                                           (logior xcb:EventMask:ButtonRelease
                                                   xcb:EventMask:ButtonMotion))
                             :pointer-mode xcb:GrabMode:Async
                             :keyboard-mode xcb:GrabMode:Async
                             :confine-to xcb:Window:None
                             :cursor cursor
                             :time xcb:Time:CurrentTime)))))))

;;;###autoload
(defun exwm-floating--stop-moveresize (&rest _args)
  "Stop move/resize."
  (xcb:+request exwm--connection
      (make-instance 'xcb:UngrabPointer :time xcb:Time:CurrentTime))
  ;; Inform the X window that its absolute position is changed
  (when exwm-floating--moveresize-calculate
    (let ((edges (window-inside-absolute-pixel-edges (frame-selected-window)))
          (id (with-current-buffer (window-buffer (frame-selected-window))
                exwm--id)))
      (xcb:+request exwm--connection
          (make-instance 'xcb:SendEvent
                         :propagate 0 :destination id
                         :event-mask xcb:EventMask:StructureNotify
                         :event (xcb:marshal
                                 (make-instance 'xcb:ConfigureNotify
                                                :event id :window id
                                                :above-sibling xcb:Window:None
                                                :x (elt edges 0)
                                                :y (elt edges 1)
                                                :width (- (elt edges 2)
                                                          (elt edges 0))
                                                :height (- (elt edges 3)
                                                           (elt edges 1))
                                                :border-width 0
                                                :override-redirect 0)
                                 exwm--connection)))))
  (xcb:flush exwm--connection)
  (setq exwm-floating--moveresize-calculate nil))

;;;###autoload
(defun exwm-floating--do-moveresize (data _synthetic)
  "Perform move/resize."
  (when exwm-floating--moveresize-calculate
    (let ((obj (make-instance 'xcb:MotionNotify))
          (geometry (frame-parameter exwm-workspace--current 'exwm-geometry))
          (frame-x 0)
          (frame-y 0)
          result)
      (when geometry
        (setq frame-x (slot-value geometry 'x)
              frame-y (slot-value geometry 'y)))
      (xcb:unmarshal obj data)
      (setq result (funcall exwm-floating--moveresize-calculate
                            (slot-value obj 'root-x) (slot-value obj 'root-y)))
      (xcb:+request exwm--connection
          (make-instance 'xcb:ConfigureWindow
                         :window (elt result 0) :value-mask (elt result 1)
                         :x (- (elt result 2) frame-x)
                         :y (- (elt result 3) frame-y)
                         :width (elt result 4) :height (elt result 5)))
      (xcb:flush exwm--connection))))

(defun exwm-floating-move (&optional delta-x delta-y)
  "Move a floating window right by DELTA-X pixels and down by DELTA-Y pixels.

Both DELTA-X and DELTA-Y default to 1.  This command should be bound locally."
  (unless (and (eq major-mode 'exwm-mode) exwm--floating-frame)
    (user-error "[EXWM] `exwm-floating-move' is only for floating X windows"))
  (unless delta-x (setq delta-x 1))
  (unless delta-y (setq delta-y 1))
  (unless (and (= 0 delta-x) (= 0 delta-y))
    (let* ((id (frame-parameter exwm--floating-frame 'exwm-outer-id))
           (geometry (xcb:+request-unchecked+reply exwm--connection
                         (make-instance 'xcb:GetGeometry :drawable id)))
           (edges (window-inside-absolute-pixel-edges)))
      (xcb:+request exwm--connection
          (make-instance 'xcb:ConfigureWindow
                         :window id
                         :value-mask (eval-when-compile
                                       (logior xcb:ConfigWindow:X
                                               xcb:ConfigWindow:Y))
                         :x (+ (slot-value geometry 'x) delta-x)
                         :y (+ (slot-value geometry 'y) delta-y)))
      ;; Inform the X window that its absolute position is changed
      (xcb:+request exwm--connection
          (make-instance 'xcb:SendEvent
                         :propagate 0 :destination exwm--id
                         :event-mask xcb:EventMask:StructureNotify
                         :event (xcb:marshal
                                 (make-instance 'xcb:ConfigureNotify
                                                :event exwm--id
                                                :window exwm--id
                                                :above-sibling xcb:Window:None
                                                :x (+ (elt edges 0) delta-x)
                                                :y (+ (elt edges 1) delta-y)
                                                :width (- (elt edges 2)
                                                          (elt edges 0))
                                                :height (- (elt edges 3)
                                                           (elt edges 1))
                                                :border-width 0
                                                :override-redirect 0)
                                 exwm--connection))))
    (xcb:flush exwm--connection)))

(defun exwm-floating--init ()
  "Initialize floating module."
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



(provide 'exwm-floating)

;;; exwm-floating.el ends here
