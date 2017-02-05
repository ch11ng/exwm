;;; exwm-layout.el --- Layout Module for EXWM  -*- lexical-binding: t -*-

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

;; This module is responsible for keeping X client window properly displayed.

;;; Code:

(require 'exwm-core)

(defvar exwm-floating-border-width)

(defun exwm-layout--resize-container (id container x y width height
                                         &optional container-only)
  "Resize a container (and its content unless CONTAINER-ONLY is non-nil)."
  (xcb:+request exwm--connection
      (make-instance 'xcb:ConfigureWindow
                     :window container
                     :value-mask (eval-when-compile
                                   (logior xcb:ConfigWindow:X
                                           xcb:ConfigWindow:Y
                                           xcb:ConfigWindow:Width
                                           xcb:ConfigWindow:Height))
                     :x x :y y :width width :height height))
  (unless container-only
    (xcb:+request exwm--connection
        (make-instance 'xcb:ConfigureWindow
                       :window id
                       :value-mask (eval-when-compile
                                     (logior xcb:ConfigWindow:Width
                                             xcb:ConfigWindow:Height))
                       :width width :height height))))

(defun exwm-layout--set-state (id state)
  "Set WM_STATE."
  (xcb:+request exwm--connection
      (make-instance 'xcb:icccm:set-WM_STATE
                     :window id :state state :icon xcb:Window:None))
  (with-current-buffer (exwm--id->buffer id)
    (setq exwm-state state)))

(defun exwm-layout--iconic-state-p (&optional id)
  (= xcb:icccm:WM_STATE:IconicState
     (if id
         (buffer-local-value 'exwm-state (exwm--id->buffer id))
       exwm-state)))

(defun exwm-layout--show (id &optional window)
  "Show window ID exactly fit in the Emacs window WINDOW."
  (exwm--log "Show #x%x in %s" id window)
  (let* ((edges (window-inside-absolute-pixel-edges window))
         (x (pop edges))
         (y (pop edges))
         (width (- (pop edges) x))
         (height (- (pop edges) y))
         (edges (window-inside-pixel-edges window))
         (relative-x (pop edges))
         (relative-y (pop edges))
         frame-width frame-height)
    (with-current-buffer (exwm--id->buffer id)
      (if (not exwm--floating-frame)
          (exwm-layout--resize-container id exwm--container
                                         relative-x relative-y width height
                                         ;; Keep the size of the X window if
                                         ;; it's the minibuffer that resized.
                                         (and
                                          (active-minibuffer-window)
                                          (< 1 (window-height
                                                (active-minibuffer-window)))))
        ;; A floating X window is of the same size as the Emacs window,
        ;; whereas its container is of the same size as the Emacs frame.
        (setq frame-width (frame-pixel-width exwm--floating-frame)
              frame-height (frame-pixel-height exwm--floating-frame))
        (xcb:+request exwm--connection
            (make-instance 'xcb:ConfigureWindow
                           :window exwm--container
                           :value-mask (logior xcb:ConfigWindow:Width
                                               xcb:ConfigWindow:Height)
                           :width frame-width
                           :height frame-height))
        (xcb:+request exwm--connection
            (make-instance 'xcb:ConfigureWindow
                           :window (frame-parameter exwm--floating-frame
                                                    'exwm-container)
                           :value-mask (logior xcb:ConfigWindow:Width
                                               xcb:ConfigWindow:Height)
                           :width frame-width
                           :height frame-height))
        (xcb:+request exwm--connection
            (make-instance 'xcb:ConfigureWindow
                           :window exwm--id
                           :value-mask (logior xcb:ConfigWindow:X
                                               xcb:ConfigWindow:Y
                                               xcb:ConfigWindow:Width
                                               xcb:ConfigWindow:Height)
                           :x relative-x
                           :y relative-y
                           :width width
                           :height height)))
      ;; Make the resizing take effect.
      (xcb:flush exwm--connection)
      (xcb:+request exwm--connection (make-instance 'xcb:MapWindow :window id))
      (xcb:+request exwm--connection
          (make-instance 'xcb:MapWindow :window exwm--container))
      (exwm-layout--set-state id xcb:icccm:WM_STATE:NormalState))
    (xcb:+request exwm--connection
        (make-instance 'xcb:SendEvent
                       :propagate 0 :destination id
                       :event-mask xcb:EventMask:StructureNotify
                       :event (xcb:marshal
                               (make-instance 'xcb:ConfigureNotify
                                              :event id
                                              :window id
                                              :above-sibling xcb:Window:None
                                              :x x
                                              :y y
                                              :width width
                                              :height height
                                              :border-width 0
                                              :override-redirect 0)
                               exwm--connection))))
  (xcb:flush exwm--connection))

(defun exwm-layout--hide (id)
  "Hide window ID."
  (with-current-buffer (exwm--id->buffer id)
    (unless (exwm-layout--iconic-state-p) ;already hidden
      (exwm--log "Hide #x%x" id)
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
      (xcb:+request exwm--connection
          (make-instance 'xcb:UnmapWindow :window exwm--container))
      (exwm-layout--set-state id xcb:icccm:WM_STATE:IconicState)
      (xcb:flush exwm--connection))))

(defvar exwm-workspace--current)

(declare-function exwm-input-grab-keyboard "exwm-input.el")
(declare-function exwm-input-release-keyboard "exwm-input.el")
(declare-function exwm-workspace--current-height "exwm-workspace.el")
(declare-function exwm-workspace--current-width  "exwm-workspace.el")
(declare-function exwm-workspace--get-geometry "exwm-workspace.el" (frame))
(declare-function exwm-workspace--minibuffer-own-frame-p "exwm-workspace.el")
(declare-function exwm-workspace--set-fullscreen "exwm-workspace.el" (frame))
(declare-function exwm-workspace-move-window "exwm-workspace.el"
                  (frame-or-index &optional id))

;;;###autoload
(defun exwm-layout-set-fullscreen (&optional id)
  "Make window ID fullscreen."
  (interactive)
  (with-current-buffer (if id (exwm--id->buffer id) (window-buffer))
    (when (memq xcb:Atom:_NET_WM_STATE_FULLSCREEN exwm--ewmh-state)
      (user-error "Already in full-screen mode"))
    ;; Save the position of floating frame.
    (when exwm--floating-frame
      (let* ((geometry (xcb:+request-unchecked+reply exwm--connection
                           (make-instance 'xcb:GetGeometry
                                          :drawable exwm--container))))
        (setq exwm--floating-frame-position
              (vector (slot-value geometry 'x) (slot-value geometry 'y)))))
    ;; Expand the workspace to fill the whole screen.
    (with-slots (x y width height) (exwm-workspace--get-geometry exwm--frame)
      (exwm-layout--resize-container nil
                                     (frame-parameter exwm--frame
                                                      'exwm-workspace)
                                     x y width height
                                     t))
    ;; Raise the workspace container (in case there are docks).
    (xcb:+request exwm--connection
        (make-instance 'xcb:ConfigureWindow
                       :window (frame-parameter exwm--frame 'exwm-workspace)
                       :value-mask xcb:ConfigWindow:StackMode
                       :stack-mode xcb:StackMode:Above))
    ;; Expand the X window and its container to fill the whole screen.
    ;; Rationale: Floating X windows may not be positioned at (0, 0)
    ;; due to the extra border.
    (exwm-layout--resize-container nil exwm--container 0 0
                                   (exwm-workspace--current-width)
                                   (exwm-workspace--current-height)
                                   t)
    (exwm-layout--resize-container nil exwm--id 0 0
                                   (exwm-workspace--current-width)
                                   (exwm-workspace--current-height)
                                   t)
    ;; Raise the X window.
    (xcb:+request exwm--connection
        (make-instance 'xcb:ConfigureWindow
                       :window exwm--container
                       :value-mask (logior xcb:ConfigWindow:BorderWidth
                                           xcb:ConfigWindow:StackMode)
                       :border-width 0
                       :stack-mode xcb:StackMode:Above))
    (xcb:+request exwm--connection
        (make-instance 'xcb:ewmh:set-_NET_WM_STATE
                       :window exwm--id
                       :data (vector xcb:Atom:_NET_WM_STATE_FULLSCREEN)))
    (xcb:flush exwm--connection)
    (cl-pushnew xcb:Atom:_NET_WM_STATE_FULLSCREEN exwm--ewmh-state)
    (call-interactively #'exwm-input-release-keyboard)))

;;;###autoload
(defun exwm-layout-unset-fullscreen (&optional id)
  "Restore window from fullscreen state."
  (interactive)
  (with-current-buffer (if id (exwm--id->buffer id) (window-buffer))
    (unless (memq xcb:Atom:_NET_WM_STATE_FULLSCREEN exwm--ewmh-state)
      (user-error "Not in full-screen mode"))
    ;; Restore the size of this workspace.
    (exwm-workspace--set-fullscreen exwm--frame)
    (if exwm--floating-frame
        ;; Restore the floating frame.
        (xcb:+request exwm--connection
            (make-instance 'xcb:ConfigureWindow
                           :window exwm--container
                           :value-mask (eval-when-compile
                                         (logior xcb:ConfigWindow:X
                                                 xcb:ConfigWindow:Y
                                                 xcb:ConfigWindow:BorderWidth))
                           :x (elt exwm--floating-frame-position 0)
                           :y (elt exwm--floating-frame-position 1)
                           :border-width exwm-floating-border-width))
      ;; Put the X window just above the Emacs frame.
      (xcb:+request exwm--connection
          (make-instance 'xcb:ConfigureWindow
                         :window exwm--container
                         :value-mask (logior xcb:ConfigWindow:Sibling
                                             xcb:ConfigWindow:StackMode)
                         :sibling (frame-parameter exwm-workspace--current
                                                   'exwm-container)
                         :stack-mode xcb:StackMode:Above)))
    (exwm-layout--show exwm--id)
    (xcb:+request exwm--connection
        (make-instance 'xcb:ewmh:set-_NET_WM_STATE :window exwm--id :data []))
    (xcb:flush exwm--connection)
    (setq exwm--ewmh-state
          (delq xcb:Atom:_NET_WM_STATE_FULLSCREEN exwm--ewmh-state))
    (call-interactively #'exwm-input-grab-keyboard)))

;;;###autoload
(defun exwm-layout-toggle-fullscreen (&optional id)
  "Toggle fullscreen mode."
  (interactive (list (exwm--buffer->id (window-buffer))))
  (when id
    (with-current-buffer (exwm--id->buffer id)
      (if (memq xcb:Atom:_NET_WM_STATE_FULLSCREEN exwm--ewmh-state)
          (exwm-reset)
        (exwm-layout-set-fullscreen id)))))

(defvar exwm-layout--other-buffer-exclude-exwm-mode-buffers nil
  "When non-nil, prevent EXWM buffers from being selected by `other-buffer'.")

(defvar exwm-layout--other-buffer-exclude-buffers nil
  "List of buffers that should not be selected by `other-buffer'.")

(defun exwm-layout--other-buffer-predicate (buffer)
  "Return non-nil when the BUFFER may be displayed in selected frame.

Prevents EXWM-mode buffers already being displayed on some other window from
being selected.

Should be set as `buffer-predicate' frame parameter for all
frames.  Used by `other-buffer'.

When variable `exwm-layout--other-buffer-exclude-exwm-mode-buffers'
is t EXWM buffers are never selected by `other-buffer'.

When variable `exwm-layout--other-buffer-exclude-buffers' is a
list of buffers, EXWM buffers belonging to that list are never
selected by `other-buffer'."
  (or (not (eq 'exwm-mode (buffer-local-value 'major-mode buffer)))
      (and (not exwm-layout--other-buffer-exclude-exwm-mode-buffers)
           (not (memq buffer exwm-layout--other-buffer-exclude-buffers))
           ;; Do not select if already shown in some window.
           (not (get-buffer-window buffer t)))))

(defvar exwm-layout-show-all-buffers nil
  "Non-nil to allow switching to buffers on other workspaces.")
(declare-function exwm-workspace--workspace-p "exwm-workspace.el"
                  (workspace))

(defun exwm-layout--set-client-list-stacking ()
  "Set _NET_CLIENT_LIST_STACKING."
  (let (id clients-floating clients clients-iconic clients-other)
    (dolist (pair exwm--id-buffer-alist)
      (setq id (car pair))
      (with-current-buffer (cdr pair)
        (if (eq exwm--frame exwm-workspace--current)
            (if exwm--floating-frame
                ;; A floating X window on the current workspace.
                (setq clients-floating (cons id clients-floating))
              (if (get-buffer-window (cdr pair) exwm-workspace--current)
                  ;; A normal tilling X window on the current workspace.
                  (setq clients (cons id clients))
                ;; An iconic tilling X window on the current workspace.
                (setq clients-iconic (cons id clients-iconic))))
          ;; X window on other workspaces.
          (setq clients-other (cons id clients-other)))))
    (xcb:+request exwm--connection
        (make-instance 'xcb:ewmh:set-_NET_CLIENT_LIST_STACKING
                       :window exwm--root
                       :data (vconcat (append clients-other clients-iconic
                                              clients clients-floating))))))

(defun exwm-layout--refresh (&optional frame)
  "Refresh layout."
  ;; `window-size-change-functions' sets this argument while
  ;; `window-configuration-change-hook' makes the frame selected.
  (unless frame
    (setq frame (selected-frame)))
  (let (covered-buffers   ;EXWM-buffers covered by a new X window.
        vacated-windows   ;Windows previously displaying EXWM-buffers.
        windows)
    (if (not (exwm-workspace--workspace-p frame))
        (if (frame-parameter frame 'exwm-outer-id)
            ;; Refresh a floating frame
            (let ((window (frame-first-window frame)))
              (with-current-buffer (window-buffer window)
                (when (and (eq major-mode 'exwm-mode)
                           ;; It may be a buffer waiting to be killed.
                           (exwm--id->buffer exwm--id))
                  (exwm--log "Refresh floating window #x%x" exwm--id)
                  (exwm-layout--show exwm--id window))))
          ;; Other frames (e.g. terminal/graphical frame of emacsclient)
          ;; We shall bury all `exwm-mode' buffers in this case
          (setq windows (window-list frame 0)) ;exclude minibuffer
          (let ((exwm-layout--other-buffer-exclude-exwm-mode-buffers t))
            (dolist (window windows)
              (with-current-buffer (window-buffer window)
                (when (eq major-mode 'exwm-mode)
                  (switch-to-prev-buffer window))))))
      ;; Refresh the whole workspace
      ;; Workspaces other than the active one can also be refreshed (RandR)
      (exwm--log "Refresh workspace %s" frame)
      (dolist (pair exwm--id-buffer-alist)
        (with-current-buffer (cdr pair)
          (when (and (not exwm--floating-frame) ;exclude floating X windows
                     (or exwm-layout-show-all-buffers
                         ;; Exclude X windows on other workspaces
                         (eq frame exwm--frame)))
            (setq windows (get-buffer-window-list (current-buffer) 0))
            (if (not windows)
                (when (eq frame exwm--frame) ;for exwm-layout-show-all-buffers
                  (exwm-layout--hide exwm--id))
              (let ((window (car windows)))
                (if (eq frame exwm--frame)
                    (exwm-layout--show exwm--id window)
                  (exwm-workspace-move-window frame exwm--id))
                ;; Make sure this buffer is not displayed elsewhere.  Note down
                ;; windows displaying an EXWM-buffer now displayed elsewhere; we
                ;; need to display with some other buffer there.
                (setq vacated-windows
                      (append vacated-windows (cdr (get-buffer-window-list
                                                    (current-buffer) 0 t))))
                ;; Note down when an EXWM-buffer is being covered by this
                ;; buffer; we don't want it to reappear in some vacated window.
                (let ((prev-buffer (car-safe
                                    (car-safe (window-prev-buffers window)))))
                  (and
                   prev-buffer
                   (eq 'exwm-mode (buffer-local-value 'major-mode prev-buffer))
                   (push prev-buffer covered-buffers))))))))
      ;; Set some sensible buffer to vacated windows.
      (let ((exwm-layout--other-buffer-exclude-buffers covered-buffers))
        (dolist (window vacated-windows)
          (switch-to-prev-buffer window)))
      ;; Make sure windows floating / on other workspaces are excluded
      (let ((exwm-layout--other-buffer-exclude-exwm-mode-buffers t))
        (dolist (window (window-list frame 0))
          (with-current-buffer (window-buffer window)
            (when (and (eq major-mode 'exwm-mode)
                       (or exwm--floating-frame (not (eq frame exwm--frame))))
              (switch-to-prev-buffer window)))))
      (exwm-layout--set-client-list-stacking)
      (xcb:flush exwm--connection))))

(declare-function exwm-workspace--client-p "exwm-workspace.el"
                  (&optional frame))

(defun exwm-layout--on-minibuffer-setup ()
  "Refresh layout when minibuffer grows."
  (unless (exwm-workspace--client-p)
    (run-with-idle-timer 0.01 nil         ;FIXME
                         (lambda ()
                           (when (< 1 (window-height (minibuffer-window)))
                             (exwm-layout--refresh))))))

(defun exwm-layout--on-echo-area-change (&optional dirty)
  "Run when message arrives or in `echo-area-clear-hook' to refresh layout."
  (when (and (current-message)
             (not (exwm-workspace--client-p))
             (or (cl-position ?\n (current-message))
                 (> (length (current-message))
                    (frame-width exwm-workspace--current))))
    (if dirty
        (exwm-layout--refresh)
      (run-with-idle-timer 0.01 nil #'exwm-layout--refresh)))) ;FIXME

;;;###autoload
(defun exwm-layout-enlarge-window (delta &optional horizontal)
  "Make the selected window DELTA pixels taller.

If no argument is given, make the selected window one pixel taller.  If the
optional argument HORIZONTAL is non-nil, make selected window DELTA pixels
wider.  If DELTA is negative, shrink selected window by -DELTA pixels.

Normal hints are checked and regarded if the selected window is displaying an
`exwm-mode' buffer.  However, this may violate the normal hints set on other X
windows."
  (interactive "p")
  (cond
   ((zerop delta))                     ;no operation
   ((window-minibuffer-p))             ;avoid resize minibuffer-window
   ((not (and (eq major-mode 'exwm-mode) exwm--floating-frame))
    ;; Resize on tiling layout
    (unless (= 0 (window-resizable nil delta horizontal nil t)) ;not resizable
      (let ((window-resize-pixelwise t))
        (window-resize nil delta horizontal nil t))))
   ;; Resize on floating layout
   (exwm--fixed-size)                   ;fixed size
   (horizontal
    (let* ((width (frame-pixel-width))
           (edges (window-inside-pixel-edges))
           (inner-width (- (elt edges 2) (elt edges 0)))
           (margin (- width inner-width)))
      (if (> delta 0)
          (if (not exwm--normal-hints-max-width)
              (cl-incf width delta)
            (if (>= inner-width exwm--normal-hints-max-width)
                (setq width nil)
              (setq width (min (+ exwm--normal-hints-max-width margin)
                               (+ width delta)))))
        (if (not exwm--normal-hints-min-width)
            (cl-incf width delta)
          (if (<= inner-width exwm--normal-hints-min-width)
              (setq width nil)
            (setq width (max (+ exwm--normal-hints-min-width margin)
                             (+ width delta))))))
      (when width
        (setf (slot-value exwm--geometry 'width) width)
        (xcb:+request exwm--connection
            (make-instance 'xcb:ConfigureWindow
                           :window (frame-parameter exwm--floating-frame
                                                    'exwm-outer-id)
                           :value-mask xcb:ConfigWindow:Width
                           :width width))
        (xcb:+request exwm--connection
            (make-instance 'xcb:ConfigureWindow
                           :window (frame-parameter exwm--floating-frame
                                                    'exwm-container)
                           :value-mask xcb:ConfigWindow:Width
                           :width width))
        (xcb:flush exwm--connection))))
   (t
    (let* ((height (frame-pixel-height))
           (edges (window-inside-pixel-edges))
           (inner-height (- (elt edges 3) (elt edges 1)))
           (margin (- height inner-height)))
      (if (> delta 0)
          (if (not exwm--normal-hints-max-height)
              (cl-incf height delta)
            (if (>= inner-height exwm--normal-hints-max-height)
                (setq height nil)
              (setq height (min (+ exwm--normal-hints-max-height margin)
                                (+ height delta)))))
        (if (not exwm--normal-hints-min-height)
            (cl-incf height delta)
          (if (<= inner-height exwm--normal-hints-min-height)
              (setq height nil)
            (setq height (max (+ exwm--normal-hints-min-height margin)
                              (+ height delta))))))
      (when height
        (setf (slot-value exwm--geometry 'height) height)
        (xcb:+request exwm--connection
            (make-instance 'xcb:ConfigureWindow
                           :window (frame-parameter exwm--floating-frame
                                                    'exwm-outer-id)
                           :value-mask xcb:ConfigWindow:Height
                           :height height))
        (xcb:+request exwm--connection
            (make-instance 'xcb:ConfigureWindow
                           :window (frame-parameter exwm--floating-frame
                                                    'exwm-container)
                           :value-mask xcb:ConfigWindow:Height
                           :height height))
        (xcb:flush exwm--connection))))))

;;;###autoload
(defun exwm-layout-enlarge-window-horizontally (delta)
  "Make the selected window DELTA pixels wider.

See also `exwm-layout-enlarge-window'."
  (interactive "p")
  (exwm-layout-enlarge-window delta t))

;;;###autoload
(defun exwm-layout-shrink-window (delta)
  "Make the selected window DELTA pixels lower.

See also `exwm-layout-enlarge-window'."
  (interactive "p")
  (exwm-layout-enlarge-window (- delta)))

;;;###autoload
(defun exwm-layout-shrink-window-horizontally (delta)
  "Make the selected window DELTA pixels narrower.

See also `exwm-layout-enlarge-window'."
  (interactive "p")
  (exwm-layout-enlarge-window (- delta) t))

;;;###autoload
(defun exwm-layout-hide-mode-line ()
  "Hide mode-line."
  (interactive)
  (when (and (eq major-mode 'exwm-mode) mode-line-format)
    (let (mode-line-height)
      (when exwm--floating-frame
        (setq mode-line-height (window-mode-line-height
                                (frame-root-window exwm--floating-frame))))
      (setq exwm--mode-line-format mode-line-format
            mode-line-format nil)
      (if (not exwm--floating-frame)
          (exwm-layout--show exwm--id)
        (set-frame-height exwm--floating-frame
                          (- (frame-pixel-height exwm--floating-frame)
                             mode-line-height)
                          nil t)))))

;;;###autoload
(defun exwm-layout-show-mode-line ()
  "Show mode-line."
  (interactive)
  (when (and (eq major-mode 'exwm-mode) (not mode-line-format))
    (setq mode-line-format exwm--mode-line-format
          exwm--mode-line-format nil)
    (if (not exwm--floating-frame)
        (exwm-layout--show exwm--id)
      (set-frame-height exwm--floating-frame
                        (+ (frame-pixel-height exwm--floating-frame)
                           (window-mode-line-height (frame-root-window
                                                     exwm--floating-frame)))
                        nil t)
      (call-interactively #'exwm-input-grab-keyboard))
    (force-mode-line-update)))

;;;###autoload
(defun exwm-layout-toggle-mode-line ()
  "Toggle the display of mode-line."
  (interactive)
  (when (eq major-mode 'exwm-mode)
    (if mode-line-format
        (exwm-layout-hide-mode-line)
      (exwm-layout-show-mode-line))))

(defvar exwm-layout--timer nil "Timer used to track echo area changes.")

(defun exwm-layout--init ()
  "Initialize layout module."
  ;; Auto refresh layout
  (add-hook 'window-configuration-change-hook #'exwm-layout--refresh)
  ;; The behavior of `window-configuration-change-hook' will be changed.
  (when (fboundp 'window-pixel-width-before-size-change)
    (add-hook 'window-size-change-functions #'exwm-layout--refresh))
  (unless (exwm-workspace--minibuffer-own-frame-p)
    ;; Refresh when minibuffer grows
    (add-hook 'minibuffer-setup-hook #'exwm-layout--on-minibuffer-setup t)
    (setq exwm-layout--timer
          (run-with-idle-timer 0 t #'exwm-layout--on-echo-area-change t))
    (add-hook 'echo-area-clear-hook #'exwm-layout--on-echo-area-change)))

(defun exwm-layout--exit ()
  "Exit the layout module."
  (remove-hook 'window-configuration-change-hook #'exwm-layout--refresh)
  (remove-hook 'minibuffer-setup-hook #'exwm-layout--on-minibuffer-setup)
  (when exwm-layout--timer
    (cancel-timer exwm-layout--timer)
    (setq exwm-layout--timer nil))
  (remove-hook 'echo-area-clear-hook #'exwm-layout--on-echo-area-change))



(provide 'exwm-layout)

;;; exwm-layout.el ends here
