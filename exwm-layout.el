;;; exwm-layout.el --- Layout Module for EXWM  -*- lexical-binding: t -*-

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

;; This module is responsible for keeping X client window properly displayed.

;;; Code:

(require 'exwm-core)
(eval-when-compile (require 'exwm-workspace))

;;;###autoload
(defun exwm-layout--show (id &optional window)
  "Show window ID exactly fit in the Emacs window WINDOW."
  (exwm--log "Show #x%x in %s" id window)
  (xcb:+request exwm--connection (make-instance 'xcb:MapWindow :window id))
  (xcb:+request exwm--connection
      (make-instance 'xcb:icccm:set-WM_STATE
                     :window id :state xcb:icccm:WM_STATE:NormalState
                     :icon xcb:Window:None))
  (let* ((buffer (exwm--id->buffer id))
         (edges (or (and buffer
                         (with-current-buffer buffer exwm--floating-edges))
                    (window-inside-pixel-edges window)))
         (x (elt edges 0))
         (y (elt edges 1))
         (width (- (elt edges 2) (elt edges 0)))
         (height (- (elt edges 3) (elt edges 1))))
    (xcb:+request exwm--connection
        (make-instance 'xcb:ConfigureWindow
                       :window id
                       :value-mask (logior xcb:ConfigWindow:X
                                           xcb:ConfigWindow:Y
                                           xcb:ConfigWindow:Width
                                           xcb:ConfigWindow:Height
                                           xcb:ConfigWindow:StackMode)
                       :x x :y y :width width :height height
                       ;; In order to put non-floating window at bottom
                       :stack-mode xcb:StackMode:Below))
    (xcb:+request exwm--connection
        (make-instance 'xcb:SendEvent
                       :propagate 0 :destination id
                       :event-mask xcb:EventMask:StructureNotify
                       :event (xcb:marshal
                               (make-instance 'xcb:ConfigureNotify
                                              :event id :window id
                                              :above-sibling xcb:Window:None
                                              :x x :y y
                                              :width width :height height
                                              :border-width 0
                                              :override-redirect 0)
                               exwm--connection))))
  (xcb:flush exwm--connection))

;;;###autoload
(defun exwm-layout--hide (id)
  "Hide window ID."
  (unless (eq xcb:icccm:WM_STATE:IconicState ;already hidden
              (with-current-buffer (exwm--id->buffer id) exwm-state))
    (exwm--log "Hide #x%x" id)
    (xcb:+request exwm--connection
        (make-instance 'xcb:ChangeWindowAttributes
                       :window id :value-mask xcb:CW:EventMask
                       :event-mask xcb:EventMask:NoEvent))
    (xcb:+request exwm--connection (make-instance 'xcb:UnmapWindow :window id))
    (xcb:+request exwm--connection
        (make-instance 'xcb:ChangeWindowAttributes
                       :window id :value-mask xcb:CW:EventMask
                       :event-mask exwm--client-event-mask))
    (xcb:+request exwm--connection
        (make-instance 'xcb:icccm:set-WM_STATE
                       :window id
                       :state xcb:icccm:WM_STATE:IconicState
                       :icon xcb:Window:None))
    (xcb:flush exwm--connection)))

;;;###autoload
(defun exwm-layout-set-fullscreen (&optional id)
  "Make window ID fullscreen."
  (interactive)
  (with-current-buffer (if id (exwm--id->buffer id) (window-buffer))
    (when exwm--fullscreen
      (user-error "Already in full-screen mode."))
    ;; Set the floating frame fullscreen first when the client is floating
    (when exwm--floating-frame
      (let* ((outer-id (frame-parameter exwm--floating-frame 'exwm-outer-id))
             (geometry (xcb:+request-unchecked+reply exwm--connection
                           (make-instance 'xcb:GetGeometry
                                          :drawable outer-id))))
        (setq exwm--floating-frame-geometry
              (vector (slot-value geometry 'x) (slot-value geometry 'y)
                      (slot-value geometry 'width)
                      (slot-value geometry 'height)))
        (xcb:+request exwm--connection
            (make-instance 'xcb:ConfigureWindow
                           :window outer-id
                           :value-mask (logior xcb:ConfigWindow:X
                                               xcb:ConfigWindow:Y
                                               xcb:ConfigWindow:Width
                                               xcb:ConfigWindow:Height)
                           :x 0 :y 0
                           :width (frame-pixel-width exwm-workspace--current)
                           :height (frame-pixel-height
                                    exwm-workspace--current))))
      (xcb:flush exwm--connection))
    (xcb:+request exwm--connection
        (make-instance 'xcb:ConfigureWindow
                       :window exwm--id
                       :value-mask (logior xcb:ConfigWindow:X
                                           xcb:ConfigWindow:Y
                                           xcb:ConfigWindow:Width
                                           xcb:ConfigWindow:Height)
                       :x 0 :y 0
                       :width (frame-pixel-width exwm-workspace--current)
                       :height (frame-pixel-height exwm-workspace--current)))
    (xcb:+request exwm--connection
        (make-instance 'xcb:ewmh:set-_NET_WM_STATE
                       :window exwm--id
                       :data (vector xcb:Atom:_NET_WM_STATE_FULLSCREEN)))
    (xcb:flush exwm--connection)
    (setq exwm--fullscreen t)
    (exwm-input-release-keyboard)))

(defun exwm-layout-unset-fullscreen (&optional id)
  "Restore window from fullscreen state."
  (interactive)
  (with-current-buffer (if id (exwm--id->buffer id) (window-buffer))
    (unless exwm--fullscreen
      (user-error "Not in full-screen mode."))
    ;; Restore the floating frame if the client is floating
    (when exwm--floating-frame
      (xcb:+request exwm--connection
          (make-instance 'xcb:ConfigureWindow
                         :window (frame-parameter exwm--floating-frame
                                                  'exwm-outer-id)
                         :value-mask (logior xcb:ConfigWindow:X
                                             xcb:ConfigWindow:Y
                                             xcb:ConfigWindow:Width
                                             xcb:ConfigWindow:Height)
                         :x (elt exwm--floating-frame-geometry 0)
                         :y (elt exwm--floating-frame-geometry 1)
                         :width (elt exwm--floating-frame-geometry 2)
                         :height (elt exwm--floating-frame-geometry 3))))
    (exwm-layout--show exwm--id)
    (xcb:+request exwm--connection
        (make-instance 'xcb:ewmh:set-_NET_WM_STATE :window exwm--id :data []))
    (xcb:flush exwm--connection)
    (setq exwm--fullscreen nil)
    (exwm-input-grab-keyboard)))

;; This function is superficially similar to `exwm-layout-set-fullscreen', but
;; they do very different things: `exwm-layout--set-frame-fullscreen' resizes a
;; frame to the actual monitor size, `exwm-layout-set-fullscreen' resizes an X
;; window to the frame size.
(defun exwm-layout--set-frame-fullscreen (frame)
  "Make frame FRAME fullscreen, with regard to its RandR output if applicable."
  (let ((geometry (or (frame-parameter frame 'exwm-geometry)
                      (xcb:+request-unchecked+reply exwm--connection
                          (make-instance 'xcb:GetGeometry
                                         :drawable exwm--root))
                      (make-instance 'xcb:RECTANGLE :x 0 :y 0
                                     :width (x-display-pixel-width)
                                     :height (x-display-pixel-height))))
        (id (frame-parameter frame 'exwm-outer-id)))
    (with-slots (x y width height) geometry
      (xcb:+request exwm--connection
          (make-instance 'xcb:ConfigureWindow
                         :window id
                         :value-mask (logior xcb:ConfigWindow:X
                                             xcb:ConfigWindow:Y
                                             xcb:ConfigWindow:Width
                                             xcb:ConfigWindow:Height)
                         :x x :y y
                         :width width
                         :height height))
      (xcb:flush exwm--connection))))

(defun exwm-layout--refresh ()
  "Refresh layout."
  (let ((frame (selected-frame))
        (placeholder (get-buffer "*scratch*"))
        windows)
    (if (not (memq frame exwm-workspace--list))
        (if (frame-parameter frame 'exwm-window-id)
            ;; Refresh a floating frame
            (progn
              (when (eq major-mode 'exwm-mode)
                (let ((window (frame-first-window frame)))
                  (with-current-buffer (window-buffer window)
                    (exwm--log "Refresh floating window #x%x" exwm--id)
                    (exwm-layout--show exwm--id window)))))
          ;; Other frames (e.g. terminal/graphical frame of emacsclient)
          ;; We shall bury all `exwm-mode' buffers in this case
          (unless placeholder ;create the *scratch* buffer if it's killed
            (setq placeholder (get-buffer-create "*scratch*"))
            (set-buffer-major-mode placeholder))
          (setq windows (window-list frame 0)) ;exclude minibuffer
          (dolist (window windows)
            (with-current-buffer (window-buffer window)
              (when (eq major-mode 'exwm-mode)
                (set-window-buffer window placeholder)))))
      ;; Refresh the whole workspace
      ;; Workspaces other than the active one can also be refreshed (RandR)
      (exwm--log "Refresh workspace %s" frame)
      (unless placeholder  ;create the *scratch* buffer if it's killed
        (setq placeholder (get-buffer-create "*scratch*"))
        (set-buffer-major-mode placeholder))
      (dolist (pair exwm--id-buffer-alist)
        (with-current-buffer (cdr pair)
          ;; Exclude windows on other workspaces and floating frames
          (when (and (eq frame exwm--frame) (not exwm--floating-frame))
            (setq windows (get-buffer-window-list (current-buffer) 0))
            (if (not windows)
                (exwm-layout--hide exwm--id)
              (exwm-layout--show exwm--id (car windows))
              (dolist (i (cdr windows))
                (set-window-buffer i placeholder))))))
      ;; Make sure windows floating / on other workspaces are excluded
      (dolist (window (window-list frame 0))
        (with-current-buffer (window-buffer window)
          (when (and (eq major-mode 'exwm-mode)
                     (or exwm--floating-frame (not (eq frame exwm--frame))))
            (set-window-buffer window placeholder))))
      ;; Update _NET_CLIENT_LIST_STACKING
      (xcb:+request exwm--connection
          (make-instance 'xcb:ewmh:set-_NET_CLIENT_LIST_STACKING
                         :window exwm--root
                         :data (vconcat
                                (delq nil
                                      (mapcar
                                       (lambda (buffer)
                                         (with-current-buffer buffer
                                           (when (eq major-mode 'exwm-mode)
                                             exwm--id)))
                                       (buffer-list))))))
      (xcb:flush exwm--connection))))

(defun exwm-layout--on-minibuffer-setup ()
  "Refresh layout when minibuffer grows."
  (run-with-idle-timer 0.01 nil         ;FIXME
                       (lambda ()
                         (when (and (< 1 (window-height (minibuffer-window)))
                                    (not (and (eq major-mode 'exwm-mode)
                                              exwm--floating-frame)))
                           (exwm-layout--refresh)))))

(defun exwm-layout--init ()
  "Initialize layout module."
  ;; Auto refresh layout
  (add-hook 'window-configuration-change-hook #'exwm-layout--refresh)
  ;; Refresh when minibuffer grows
  (add-hook 'minibuffer-setup-hook #'exwm-layout--on-minibuffer-setup t))



(provide 'exwm-layout)

;;; exwm-layout.el ends here
