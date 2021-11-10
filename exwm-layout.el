;;; exwm-layout.el --- Layout Module for EXWM  -*- lexical-binding: t -*-

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

;; This module is responsible for keeping X client window properly displayed.

;;; Code:

(require 'exwm-core)

(defgroup exwm-layout nil
  "Layout."
  :version "25.3"
  :group 'exwm)

(defcustom exwm-layout-auto-iconify t
  "Non-nil to automatically iconify unused X windows when possible."
  :type 'boolean)

(defcustom exwm-layout-show-all-buffers nil
  "Non-nil to allow switching to buffers on other workspaces."
  :type 'boolean)

(defconst exwm-layout--floating-hidden-position -101
  "Where to place hidden floating X windows.")

(defvar exwm-layout--other-buffer-exclude-buffers nil
  "List of buffers that should not be selected by `other-buffer'.")

(defvar exwm-layout--other-buffer-exclude-exwm-mode-buffers nil
  "When non-nil, prevent EXWM buffers from being selected by `other-buffer'.")

(defvar exwm-layout--timer nil "Timer used to track echo area changes.")

(defvar exwm-workspace--current)
(defvar exwm-workspace--frame-y-offset)
(declare-function exwm-input--release-keyboard "exwm-input.el")
(declare-function exwm-input--grab-keyboard "exwm-input.el")
(declare-function exwm-input-grab-keyboard "exwm-input.el")
(declare-function exwm-workspace--active-p "exwm-workspace.el" (frame))
(declare-function exwm-workspace--client-p "exwm-workspace.el"
                  (&optional frame))
(declare-function exwm-workspace--minibuffer-own-frame-p "exwm-workspace.el")
(declare-function exwm-workspace--workspace-p "exwm-workspace.el"
                  (workspace))
(declare-function exwm-workspace-move-window "exwm-workspace.el"
                  (frame-or-index &optional id))

(defun exwm-layout--set-state (id state)
  "Set WM_STATE."
  (exwm--log "id=#x%x" id)
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

(defun exwm-layout--set-ewmh-state (xwin)
  "Set _NET_WM_STATE."
  (with-current-buffer (exwm--id->buffer xwin)
    (xcb:+request exwm--connection
        (make-instance 'xcb:ewmh:set-_NET_WM_STATE
                       :window exwm--id
                       :data exwm--ewmh-state))))

(defun exwm-layout--fullscreen-p ()
  (when (derived-mode-p 'exwm-mode)
    (memq xcb:Atom:_NET_WM_STATE_FULLSCREEN exwm--ewmh-state)))

(defun exwm-layout--auto-iconify ()
  (when (and exwm-layout-auto-iconify
             (not exwm-transient-for))
    (let ((xwin exwm--id)
          (state exwm-state))
      (dolist (pair exwm--id-buffer-alist)
        (with-current-buffer (cdr pair)
          (when (and exwm--floating-frame
                     (eq exwm-transient-for xwin)
                     (not (eq exwm-state state)))
            (if (eq state xcb:icccm:WM_STATE:NormalState)
                (exwm-layout--refresh-floating exwm--floating-frame)
              (exwm-layout--hide exwm--id))))))))

(defun exwm-layout--show (id &optional window)
  "Show window ID exactly fit in the Emacs window WINDOW."
  (exwm--log "Show #x%x in %s" id window)
  (let* ((edges (window-inside-absolute-pixel-edges window))
         (x (pop edges))
         (y (pop edges))
         (width (- (pop edges) x))
         (height (- (pop edges) y))
         frame-x frame-y frame-width frame-height)
    (with-current-buffer (exwm--id->buffer id)
      (when exwm--floating-frame
        (setq frame-width (frame-pixel-width exwm--floating-frame)
              frame-height (+ (frame-pixel-height exwm--floating-frame)
                              ;; Use `frame-outer-height' in the future.
                              exwm-workspace--frame-y-offset))
        (when exwm--floating-frame-position
          (setq frame-x (elt exwm--floating-frame-position 0)
                frame-y (elt exwm--floating-frame-position 1)
                x (+ x frame-x (- exwm-layout--floating-hidden-position))
                y (+ y frame-y (- exwm-layout--floating-hidden-position)))
          (setq exwm--floating-frame-position nil))
        (exwm--set-geometry (frame-parameter exwm--floating-frame
                                             'exwm-container)
                            frame-x frame-y frame-width frame-height))
      (when (exwm-layout--fullscreen-p)
        (with-slots ((x* x)
                     (y* y)
                     (width* width)
                     (height* height))
            (exwm-workspace--get-geometry exwm--frame)
          (setq x x*
                y y*
                width width*
                height height*)))
      (exwm--set-geometry id x y width height)
      (xcb:+request exwm--connection (make-instance 'xcb:MapWindow :window id))
      (exwm-layout--set-state id xcb:icccm:WM_STATE:NormalState)
      (setq exwm--ewmh-state
            (delq xcb:Atom:_NET_WM_STATE_HIDDEN exwm--ewmh-state))
      (exwm-layout--set-ewmh-state id)
      (exwm-layout--auto-iconify)))
  (xcb:flush exwm--connection))

(defun exwm-layout--hide (id)
  "Hide window ID."
  (with-current-buffer (exwm--id->buffer id)
    (unless (or (exwm-layout--iconic-state-p)
                (and exwm--floating-frame
                     (eq 4294967295. exwm--desktop)))
      (exwm--log "Hide #x%x" id)
      (when exwm--floating-frame
        (let* ((container (frame-parameter exwm--floating-frame
                                           'exwm-container))
               (geometry (xcb:+request-unchecked+reply exwm--connection
                             (make-instance 'xcb:GetGeometry
                                            :drawable container))))
          (setq exwm--floating-frame-position
                (vector (slot-value geometry 'x) (slot-value geometry 'y)))
          (exwm--set-geometry container exwm-layout--floating-hidden-position
                              exwm-layout--floating-hidden-position
                              1
                              1)))
      (xcb:+request exwm--connection
          (make-instance 'xcb:ChangeWindowAttributes
                         :window id :value-mask xcb:CW:EventMask
                         :event-mask xcb:EventMask:NoEvent))
      (xcb:+request exwm--connection
          (make-instance 'xcb:UnmapWindow :window id))
      (xcb:+request exwm--connection
          (make-instance 'xcb:ChangeWindowAttributes
                         :window id :value-mask xcb:CW:EventMask
                         :event-mask (exwm--get-client-event-mask)))
      (exwm-layout--set-state id xcb:icccm:WM_STATE:IconicState)
      (cl-pushnew xcb:Atom:_NET_WM_STATE_HIDDEN exwm--ewmh-state)
      (exwm-layout--set-ewmh-state id)
      (exwm-layout--auto-iconify)
      (xcb:flush exwm--connection))))

;;;###autoload
(cl-defun exwm-layout-set-fullscreen (&optional id)
  "Make window ID fullscreen."
  (interactive)
  (exwm--log "id=#x%x" (or id 0))
  (unless (and (or id (derived-mode-p 'exwm-mode))
               (not (exwm-layout--fullscreen-p)))
    (cl-return-from exwm-layout-set-fullscreen))
  (with-current-buffer (if id (exwm--id->buffer id) (window-buffer))
    ;; Expand the X window to fill the whole screen.
    (with-slots (x y width height) (exwm-workspace--get-geometry exwm--frame)
      (exwm--set-geometry exwm--id x y width height))
    ;; Raise the X window.
    (xcb:+request exwm--connection
        (make-instance 'xcb:ConfigureWindow
                       :window exwm--id
                       :value-mask (logior xcb:ConfigWindow:BorderWidth
                                           xcb:ConfigWindow:StackMode)
                       :border-width 0
                       :stack-mode xcb:StackMode:Above))
    (cl-pushnew xcb:Atom:_NET_WM_STATE_FULLSCREEN exwm--ewmh-state)
    (exwm-layout--set-ewmh-state exwm--id)
    (xcb:flush exwm--connection)
    (set-window-dedicated-p (get-buffer-window) t)
    (exwm-input--release-keyboard exwm--id)))

;;;###autoload
(cl-defun exwm-layout-unset-fullscreen (&optional id)
  "Restore window from fullscreen state."
  (interactive)
  (exwm--log "id=#x%x" (or id 0))
  (unless (and (or id (derived-mode-p 'exwm-mode))
               (exwm-layout--fullscreen-p))
    (cl-return-from exwm-layout-unset-fullscreen))
  (with-current-buffer (if id (exwm--id->buffer id) (window-buffer))
    ;; `exwm-layout--show' relies on `exwm--ewmh-state' to decide whether to
    ;; fullscreen the window.
    (setq exwm--ewmh-state
          (delq xcb:Atom:_NET_WM_STATE_FULLSCREEN exwm--ewmh-state))
    (exwm-layout--set-ewmh-state exwm--id)
    (if exwm--floating-frame
        (exwm-layout--show exwm--id (frame-root-window exwm--floating-frame))
      (xcb:+request exwm--connection
          (make-instance 'xcb:ConfigureWindow
                         :window exwm--id
                         :value-mask (logior xcb:ConfigWindow:Sibling
                                             xcb:ConfigWindow:StackMode)
                         :sibling exwm--guide-window
                         :stack-mode xcb:StackMode:Above))
      (let ((window (get-buffer-window nil t)))
        (when window
          (exwm-layout--show exwm--id window))))
    (xcb:flush exwm--connection)
    (set-window-dedicated-p (get-buffer-window) nil)
    (when (eq 'line-mode exwm--selected-input-mode)
      (exwm-input--grab-keyboard exwm--id))))

;;;###autoload
(cl-defun exwm-layout-toggle-fullscreen (&optional id)
  "Toggle fullscreen mode."
  (interactive (list (exwm--buffer->id (window-buffer))))
  (exwm--log "id=#x%x" (or id 0))
  (unless (or id (derived-mode-p 'exwm-mode))
    (cl-return-from exwm-layout-toggle-fullscreen))
  (when id
    (with-current-buffer (exwm--id->buffer id)
      (if (exwm-layout--fullscreen-p)
          (exwm-layout-unset-fullscreen id)
        (exwm-layout-set-fullscreen id)))))

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
  (or (not (with-current-buffer buffer (derived-mode-p 'exwm-mode)))
      (and (not exwm-layout--other-buffer-exclude-exwm-mode-buffers)
           (not (memq buffer exwm-layout--other-buffer-exclude-buffers))
           ;; Do not select if already shown in some window.
           (not (get-buffer-window buffer t)))))

(defun exwm-layout--set-client-list-stacking ()
  "Set _NET_CLIENT_LIST_STACKING."
  (exwm--log)
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
  (exwm--log "frame=%s" frame)
  (if (not (exwm-workspace--workspace-p frame))
      (if (frame-parameter frame 'exwm-outer-id)
          (exwm-layout--refresh-floating frame)
        (exwm-layout--refresh-other frame))
    (exwm-layout--refresh-workspace frame)))

(defun exwm-layout--refresh-floating (frame)
  "Refresh floating frame FRAME."
  (exwm--log "Refresh floating %s" frame)
  (let ((window (frame-first-window frame)))
    (with-current-buffer (window-buffer window)
      (when (and (derived-mode-p 'exwm-mode)
                 ;; It may be a buffer waiting to be killed.
                 (exwm--id->buffer exwm--id))
        (exwm--log "Refresh floating window #x%x" exwm--id)
        (if (exwm-workspace--active-p exwm--frame)
            (exwm-layout--show exwm--id window)
          (exwm-layout--hide exwm--id))))))

(defun exwm-layout--refresh-other (frame)
  "Refresh client or nox frame FRAME."
  ;; Other frames (e.g. terminal/graphical frame of emacsclient)
  ;; We shall bury all `exwm-mode' buffers in this case
  (exwm--log "Refresh other %s" frame)
  (let ((windows (window-list frame 'nomini)) ;exclude minibuffer
        (exwm-layout--other-buffer-exclude-exwm-mode-buffers t))
    (dolist (window windows)
      (with-current-buffer (window-buffer window)
        (when (derived-mode-p 'exwm-mode)
          (if (window-prev-buffers window)
              (switch-to-prev-buffer window)
            (switch-to-next-buffer window)))))))

(defun exwm-layout--refresh-workspace (frame)
  "Refresh workspace frame FRAME."
  (exwm--log "Refresh workspace %s" frame)
  ;; Workspaces other than the active one can also be refreshed (RandR)
  (let (covered-buffers   ;EXWM-buffers covered by a new X window.
        vacated-windows)  ;Windows previously displaying EXWM-buffers.
    (dolist (pair exwm--id-buffer-alist)
      (with-current-buffer (cdr pair)
        (when (and (not exwm--floating-frame) ;exclude floating X windows
                   (or exwm-layout-show-all-buffers
                       ;; Exclude X windows on other workspaces
                       (eq frame exwm--frame)))
          (let (;; List of windows in current frame displaying the `exwm-mode'
                ;; buffers.
                (windows (get-buffer-window-list (current-buffer) 'nomini
                                                 frame)))
            (if (not windows)
                (when (eq frame exwm--frame)
                  ;; Hide it if it was being shown in this workspace.
                  (exwm-layout--hide exwm--id))
              (let ((window (car windows)))
                (if (eq frame exwm--frame)
                    ;; Show it if `frame' is active, hide otherwise.
                    (if (exwm-workspace--active-p frame)
                        (exwm-layout--show exwm--id window)
                      (exwm-layout--hide exwm--id))
                  ;; It was last shown in other workspace; move it here.
                  (exwm-workspace-move-window frame exwm--id))
                ;; Vacate any other windows (in any workspace) showing this
                ;; `exwm-mode' buffer.
                (setq vacated-windows
                      (append vacated-windows (remove
                                               window
                                               (get-buffer-window-list
                                                (current-buffer) 'nomini t))))
                ;; Note any `exwm-mode' buffer is being covered by another
                ;; `exwm-mode' buffer.  We want to avoid that `exwm-mode'
                ;; buffer to be reappear in any of the vacated windows.
                (let ((prev-buffer (car-safe
                                    (car-safe (window-prev-buffers window)))))
                  (and
                   prev-buffer
                   (with-current-buffer prev-buffer
                     (derived-mode-p 'exwm-mode))
                   (push prev-buffer covered-buffers)))))))))
    ;; Set some sensible buffer to vacated windows.
    (let ((exwm-layout--other-buffer-exclude-buffers covered-buffers))
      (dolist (window vacated-windows)
        (if (window-prev-buffers window)
            (switch-to-prev-buffer window)
          (switch-to-next-buffer window))))
    ;; Make sure windows floating / on other workspaces are excluded
    (let ((exwm-layout--other-buffer-exclude-exwm-mode-buffers t))
      (dolist (window (window-list frame 'nomini))
        (with-current-buffer (window-buffer window)
          (when (and (derived-mode-p 'exwm-mode)
                     (or exwm--floating-frame (not (eq frame exwm--frame))))
            (if (window-prev-buffers window)
                (switch-to-prev-buffer window)
              (switch-to-next-buffer window))))))
    (exwm-layout--set-client-list-stacking)
    (xcb:flush exwm--connection)))

(defun exwm-layout--on-minibuffer-setup ()
  "Refresh layout when minibuffer grows."
  (exwm--log)
  (unless (exwm-workspace--client-p)
    (exwm--defer 0 (lambda ()
                     (when (< 1 (window-height (minibuffer-window)))
                       (exwm-layout--refresh))))))

(defun exwm-layout--on-echo-area-change (&optional dirty)
  "Run when message arrives or in `echo-area-clear-hook' to refresh layout."
  (when (and (current-message)
             (not (exwm-workspace--client-p))
             (or (cl-position ?\n (current-message))
                 (> (length (current-message))
                    (frame-width exwm-workspace--current))))
    (exwm--log)
    (if dirty
        (exwm-layout--refresh)
      (exwm--defer 0 #'exwm-layout--refresh))))

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
  (exwm--log)
  (cond
   ((zerop delta))                     ;no operation
   ((window-minibuffer-p))             ;avoid resize minibuffer-window
   ((not (and (derived-mode-p 'exwm-mode) exwm--floating-frame))
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
      (when (and width (> width 0))
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
    (let* ((height (+ (frame-pixel-height) exwm-workspace--frame-y-offset))
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
      (when (and height (> height 0))
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
  (exwm--log "%s" delta)
  (exwm-layout-enlarge-window delta t))

;;;###autoload
(defun exwm-layout-shrink-window (delta)
  "Make the selected window DELTA pixels lower.

See also `exwm-layout-enlarge-window'."
  (interactive "p")
  (exwm--log "%s" delta)
  (exwm-layout-enlarge-window (- delta)))

;;;###autoload
(defun exwm-layout-shrink-window-horizontally (delta)
  "Make the selected window DELTA pixels narrower.

See also `exwm-layout-enlarge-window'."
  (interactive "p")
  (exwm--log "%s" delta)
  (exwm-layout-enlarge-window (- delta) t))

;;;###autoload
(defun exwm-layout-hide-mode-line ()
  "Hide mode-line."
  (interactive)
  (exwm--log)
  (when (and (derived-mode-p 'exwm-mode) mode-line-format)
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
  (exwm--log)
  (when (and (derived-mode-p 'exwm-mode) (not mode-line-format))
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
  (exwm--log)
  (when (derived-mode-p 'exwm-mode)
    (if mode-line-format
        (exwm-layout-hide-mode-line)
      (exwm-layout-show-mode-line))))

(defun exwm-layout--init ()
  "Initialize layout module."
  ;; Auto refresh layout
  (exwm--log)
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
  (exwm--log)
  (remove-hook 'window-configuration-change-hook #'exwm-layout--refresh)
  (when (fboundp 'window-pixel-width-before-size-change)
    (remove-hook 'window-size-change-functions #'exwm-layout--refresh))
  (remove-hook 'minibuffer-setup-hook #'exwm-layout--on-minibuffer-setup)
  (when exwm-layout--timer
    (cancel-timer exwm-layout--timer)
    (setq exwm-layout--timer nil))
  (remove-hook 'echo-area-clear-hook #'exwm-layout--on-echo-area-change))



(provide 'exwm-layout)

;;; exwm-layout.el ends here
