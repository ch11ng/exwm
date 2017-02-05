;;; exwm-cm.el --- Compositing Manager for EXWM  -*- lexical-binding: t -*-

;; Copyright (C) 2016-2017 Free Software Foundation, Inc.

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

;; This module provides a compositing manager (CM) for EXWM, mainly to
;; enable transparency support.

;; Usage:
;; Add following lines to .emacs and modify accordingly:
;;
;; (require 'exwm-cm)
;; ;; Make all Emacs frames opaque.
;; (setq window-system-default-frame-alist '((x . ((alpha . 100)))))
;; ;; Assign everything else a 80% opacity.
;; (setq exwm-cm-opacity 80)
;; (exwm-cm-enable)
;;
;; With the last line this CM would be started with EXWM.  You can also
;; start and stop this CM with `exwm-cm-start' and `exwm-cm-stop' at any
;; time.

;; Theory:
;; Due to its unique way of managing X windows, EXWM can not work with
;; any existing CMs.  And this CM, designed specifically for EXWM,
;; probably won't work well with other WMs, too.  The theories behind
;; all CMs are basically the same, some peculiarities of this CM are
;; summarized as the following sections.

;; + Data structures:
;;   This CM organizes all X windows concerned with compositing in a
;;   tree hierarchy.  Below is a stripped-down version of such tree with
;;   each node representing an X window (except the root placeholder),
;;
;;   (nil
;;    (root-xwin
;;     (unmanaged-xwin)
;;     (workspace-container
;;      (unmanaged-xwin)
;;      (xwin-container
;;       (xwin)
;;       (floating-frame-container
;;        (floating-frame)))
;;      (xwin-container
;;       (xwin))
;;      (workspace-frame-container
;;       (workspace-frame)))
;;     (minibuffer-frame-container
;;      (minibuffer-frame))))
;;
;;   where
;;   - nodes with non-nil CDRs are containers,
;;   - siblings are arranged in stacking order (top to bottom),
;;   - and "managed" and "unmanaged" are in WM's sense.
;;
;;   During a painting process, the tree is traversed starting from the
;;   root node, with each leaf visited and painted.  The attributes of
;;   each X window (position, size, etc) are recorded as an instance of
;;   class `exwm-cm--attr'.  Such instance is associated with the
;;   corresponding X window ID through a hash table.  The instance also
;;   contains a slot pointing to a subtree of the aforementioned tree,
;;   with the root node being the parent of the X window.  This makes it
;;   convenient to carry out operations such as insertion, deletion,
;;   restacking and reparenting.

;; + Compositing strategies:
;;   - Only leaves are painted, since branches (containers) are always
;;     invisible.
;;   - The root X window is painted separately.
;;   - Siblings below a workspace frame container are not painted; they
;;     are considered hidden.
;;   - Only the top workspace in one (RandR) output is painted.
;;   - Workspace frames and floating frames are always clipped by its
;;     Emacs windows displaying `exwm-mode' buffers, therefore they
;;     don't block X windows.

;; Reference:
;; + xcompmgr (http://cgit.freedesktop.org/xorg/app/xcompmgr/)

;;; Code:

(require 'xcb-composite)
(require 'xcb-damage)
(require 'xcb-ewmh)
(require 'xcb-icccm)
(require 'xcb-renderutil)
(require 'xcb-shape)

(require 'exwm-core)
(require 'exwm-workspace)
(require 'exwm-manage)

(defconst exwm-cm--OPAQUE (float #xFFFFFFFF)
  "The opacity value of the _NET_WM_WINDOW_OPACITY property.")
(defvar exwm-cm--_NET_WM_WINDOW_OPACITY nil "The _NET_WM_WINDOW_OPACITY atom.")
(defvar exwm-cm-opacity nil
  "The default value of opacity when it's not explicitly specified.

The value should be a floating number between 0 (transparent) and 100
\(opaque).  A value of nil also means opaque.")

(defvar exwm-cm--hash nil
  "The hash table associating X window IDs to their attributes.")

(defvar exwm-cm--conn nil "The X connection used by the CM.")
(defvar exwm-cm--buffer nil "The rendering buffer.")
(defvar exwm-cm--depth nil "Default depth.")
(defvar exwm-cm--clip-changed t "Whether clip has changed.")
(defvar exwm-cm--damages nil "All damaged regions.")
(defvar exwm-cm--expose-rectangles nil
  "Used by Expose event handler to collect exposed regions.")

(defvar exwm-cm--background nil "The background (render) picture.")
(defvar exwm-cm--background-atom-names '("_XROOTPMAP_ID" "_XSETROOT_ID")
  "Property names for background pixmap.")
(defvar exwm-cm--background-atoms nil "Interned atoms of the property names.")

(defun exwm-cm--get-opacity (xwin)
  "Get the opacity of X window XWIN.

The value is between 0 (fully transparent) to #xFFFFFFFF (opaque)."
  (let ((reply (xcb:+request-unchecked+reply exwm-cm--conn
                   (make-instance 'xcb:icccm:-GetProperty-single
                                  :window xwin
                                  :property exwm-cm--_NET_WM_WINDOW_OPACITY
                                  :type xcb:Atom:CARDINAL))))
    ;; The X window might have already been destroyed.
    (when reply
      (slot-value reply 'value))))

(defun exwm-cm-set-opacity (xwin opacity)
  "Set the opacity of X window XWIN to OPACITY.

The value is between 0 (fully transparent) to 100 (opaque).

If called interactively, XWIN would be the selected X window."
  (interactive
   (list (exwm--buffer->id (window-buffer))
         (read-number "Opacity (0 ~ 100): " 100)))
  (when (and xwin
             (<= 0 opacity 100))
    (setq opacity (round (* exwm-cm--OPAQUE (/ opacity 100.0))))
    (xcb:+request exwm-cm--conn
        (make-instance 'xcb:icccm:-ChangeProperty-single
                       :window xwin
                       :property exwm-cm--_NET_WM_WINDOW_OPACITY
                       :type xcb:Atom:CARDINAL
                       :data opacity))
    (xcb:flush exwm-cm--conn)))

(defclass exwm-cm--attr ()
  (
   ;; The entity associated with this X window; can be a frame, a buffer
   ;; or nil.
   (entity :initform nil)
   ;; The subtree of which the root node is the parent of this X window.
   (tree :initarg :tree)
   ;; Geometry.
   (x :initarg :x)
   (y :initarg :y)
   (width :initarg :width)
   (height :initarg :height)
   ;; X window attributes.
   (visual :initarg :visual)
   (class :initarg :class)
   ;; The opacity of this X window; can be 0 ~ #xFFFE or nil.
   (opacity :initform nil)
   ;; Determine whether this X window should be treated as opaque or
   ;; transparent; can be nil (opaque), 'argb or 'transparent (both
   ;; should be treated as transparent).
   (mode :initform nil)
   ;; The (render) picture of this X window.
   (picture :initform nil)
   ;; The 1x1 (render) picture with only alpha channel.
   (alpha-picture :initform nil)
   ;; Whether this X window is ever damaged.
   (damaged :initform nil)
   ;; The damage object monitoring this X window.
   (damage :initarg :damage)
   ;; The bounding region of this X window (can be irregular).
   (border-size :initform nil)
   ;; The rectangular bounding region of this X window.
   (extents :initform nil)
   ;; The region require repainting (used for transparent X windows).
   (border-clip :initform nil)
   ;; Shape-related parameters.
   (shaped :initform nil)
   (shape-x :initarg :shape-x)
   (shape-y :initarg :shape-y)
   (shape-width :initarg :shape-width)
   (shape-height :initarg :shape-height))
  :documentation "Attributes of an X window.")

(defsubst exwm-cm--xwin->attr (xwin)
  "Get the attributes of X window XWIN."
  (gethash xwin exwm-cm--hash))

(defsubst exwm-cm--get-tree (xwin)
  "Get the subtree of the parent of X window XWIN."
  (slot-value (exwm-cm--xwin->attr xwin) 'tree))

(defsubst exwm-cm--set-tree (xwin tree)
  "Reparent X window XWIN to another tree TREE."
  (setf (slot-value (exwm-cm--xwin->attr xwin) 'tree) tree))

(defsubst exwm-cm--get-parent (xwin)
  "Get the parent of X window XWIN."
  (car (exwm-cm--get-tree xwin)))

(defsubst exwm-cm--get-siblings (xwin)
  "Get a list of subtrees of the siblings of X window XWIN."
  (cdr (exwm-cm--get-tree xwin)))

(defsubst exwm-cm--get-subtree (xwin)
  "Get the subtree of which the X window XWIN is the root node."
  (assq xwin (exwm-cm--get-siblings xwin)))

(defun exwm-cm--create-attr (xwin tree x y width height)
  "Create attributes for X window XWIN.

TREE is the subtree and the parent of this X window is the tree's root.
X and Y specify the position with regard to the root X window.  WIDTH
and HEIGHT specify the size of the X window."
  (let (visual class map-state damage attr)
    (cond
     ((= xwin exwm--root)
      ;; Redirect all subwindows to off-screen storage.
      (xcb:+request exwm-cm--conn
          (make-instance 'xcb:composite:RedirectSubwindows
                         :window exwm--root
                         :update xcb:composite:Redirect:Manual))
      (xcb:+request exwm-cm--conn
          (make-instance 'xcb:ChangeWindowAttributes
                         :window xwin
                         :value-mask xcb:CW:EventMask
                         :event-mask (logior xcb:EventMask:StructureNotify
                                             xcb:EventMask:PropertyChange
                                             xcb:EventMask:SubstructureNotify
                                             xcb:EventMask:Exposure)))
      (setq visual (slot-value (car (slot-value (xcb:get-setup exwm-cm--conn)
                                                'roots))
                               'root-visual)
            class xcb:WindowClass:InputOutput))
     ((eq xwin exwm-manage--desktop)
      ;; Ignore any desktop; paint the background ourselves.
      (setq visual 0
            class xcb:WindowClass:InputOnly
            map-state xcb:MapState:Unmapped))
     (t
      ;; Redirect this window to off-screen storage, or the content
      ;; would be mirrored to its parent.
      (xcb:+request exwm-cm--conn
          (make-instance 'xcb:composite:RedirectWindow
                         :window xwin
                         :update xcb:composite:Redirect:Manual))
      (xcb:+request exwm-cm--conn
          (make-instance 'xcb:ChangeWindowAttributes
                         :window xwin
                         :value-mask xcb:CW:EventMask
                         :event-mask (logior xcb:EventMask:StructureNotify
                                             xcb:EventMask:PropertyChange)))
      (let ((reply (xcb:+request-unchecked+reply exwm-cm--conn
                       (make-instance 'xcb:GetWindowAttributes
                                      :window xwin))))
        (if reply
            (with-slots ((visual* visual)
                         (class* class)
                         (map-state* map-state))
                reply
              (setq visual visual*
                    class class*
                    map-state map-state*))
          ;; The X window has been destroyed actually.  It'll get
          ;; removed by a DestroyNotify event.
          (setq visual 0
                class xcb:WindowClass:InputOnly
                map-state xcb:MapState:Unmapped)))
      (when (/= class xcb:WindowClass:InputOnly)
        (setq damage (xcb:generate-id exwm-cm--conn))
        (xcb:+request exwm-cm--conn
            (make-instance 'xcb:damage:Create
                           :damage damage
                           :drawable xwin
                           :level xcb:damage:ReportLevel:NonEmpty))
        (xcb:+request exwm-cm--conn
            (make-instance 'xcb:shape:SelectInput
                           :destination-window xwin
                           :enable 1)))))
    (setq attr (make-instance 'exwm-cm--attr
                              :tree tree
                              :x x
                              :y y
                              :width width
                              :height height
                              :visual visual
                              :class class
                              :damage damage
                              :shape-x x
                              :shape-y y
                              :shape-width width
                              :shape-height height))
    (puthash xwin attr exwm-cm--hash)
    (unless (or (= xwin exwm--root)
                (= class xcb:WindowClass:InputOnly))
      (exwm-cm--update-opacity xwin)
      (when (= map-state xcb:MapState:Viewable)
        (exwm-cm--map-xwin xwin t)))))

(defun exwm-cm--update-geometry (xwin x y width height &optional above-sibling)
  "Update the geometry of X window XWIN.

X, Y, WIDTH and HEIGHT have the same meaning with the arguments used in
`exwm-cm--create-attr'.  If ABOVE-SIBLING is non-nil, restack XWIN with
`exwm-cm--restack.'"
  (with-slots ((x* x)
               (y* y)
               (width* width)
               (height* height)
               extents shaped shape-x shape-y shape-width shape-height)
      (exwm-cm--xwin->attr xwin)
    (let ((stack-changed (and above-sibling
                              (exwm-cm--restack xwin above-sibling)))
          (position-changed (or (and x (/= x x*))
                                (and y (/= y y*))))
          (size-changed (or (and width (/= width width*))
                            (and height (/= height height*))))
          subtree dx dy damage new-extents)
      (when position-changed
        (setq subtree (exwm-cm--get-subtree xwin)
              dx (- x x*)
              dy (- y y*))
        (dolist (node (cdr subtree))
          (with-slots (x y) (exwm-cm--xwin->attr (car node))
            (exwm--log "(CM) #x%X(*): @%+d%+d => @%+d%+d"
                       (car node) x y (+ x dx) (+ y dy))
            (exwm-cm--update-geometry (car node) (+ x dx) (+ y dy) nil nil)))
        (exwm--log "(CM) #x%X: @%+d%+d => @%+d%+d" xwin x* y* x y)
        (setf x* x
              y* y)
        (cl-incf shape-x dx)
        (cl-incf shape-y dy))
      (when size-changed
        (setf width* width
              height* height)
        (unless shaped
          (setf shape-width width
                shape-height height)))
      (when (or stack-changed position-changed size-changed)
        (setq damage (xcb:generate-id exwm-cm--conn)
              new-extents (xcb:generate-id exwm-cm--conn))
        (xcb:+request exwm-cm--conn
            (make-instance 'xcb:xfixes:CreateRegion
                           :region damage
                           :rectangles nil))
        (when extents
          (xcb:+request exwm-cm--conn
              (make-instance 'xcb:xfixes:CopyRegion
                             :source extents
                             :destination damage)))
        (xcb:+request exwm-cm--conn
            (make-instance 'xcb:xfixes:CreateRegion
                           :region new-extents
                           :rectangles (list (make-instance 'xcb:RECTANGLE
                                                            :x x*
                                                            :y y*
                                                            :width width*
                                                            :height height*))))
        (xcb:+request exwm-cm--conn
            (make-instance 'xcb:xfixes:UnionRegion
                           :source1 damage
                           :source2 new-extents
                           :destination damage))
        (xcb:+request exwm-cm--conn
            (make-instance 'xcb:xfixes:DestroyRegion
                           :region new-extents))
        (exwm-cm--add-damage damage)))))

(defun exwm-cm--update-opacity (xwin)
  "Update the opacity of X window XWIN."
  (with-slots (visual opacity mode alpha-picture extents)
      (exwm-cm--xwin->attr xwin)
    (let (format forminfo)
      ;; Get the opacity.
      (setf opacity (exwm-cm--get-opacity xwin))
      (if opacity
          (setf opacity (round (* #xFFFF (/ opacity exwm-cm--OPAQUE))))
        (when (numberp exwm-cm-opacity)
          (setf opacity (round (* #xFFFF (/ exwm-cm-opacity 100.0))))))
      (when (and opacity
                 (>= opacity #xFFFF))
        (setf opacity nil))
      ;; Determine the mode of the X window.
      (setq format (xcb:renderutil:find-visual-format
                    (xcb:renderutil:query-formats exwm-cm--conn) visual))
      (when format
        (catch 'break
          (dolist (f (slot-value (xcb:renderutil:query-formats exwm-cm--conn)
                                 'formats))
            (when (eq format (slot-value f 'id))
              (setq forminfo f)
              (throw 'break nil)))))
      (if (and forminfo
               (eq xcb:render:PictType:Direct (slot-value forminfo 'type))
               (/= 0 (slot-value (slot-value forminfo 'direct) 'alpha-mask)))
          (setf mode 'argb)
        (if opacity
            (setf mode 'transparent)
          (setf mode nil)))
      ;; Clear resources.
      (when alpha-picture
        (xcb:+request exwm-cm--conn
            (make-instance 'xcb:render:FreePicture
                           :picture alpha-picture))
        (setf alpha-picture nil))
      (when extents
        (let ((damage (xcb:generate-id exwm-cm--conn)))
          (xcb:+request exwm-cm--conn
              (make-instance 'xcb:xfixes:CreateRegion
                             :region damage
                             :rectangles nil))
          (xcb:+request exwm-cm--conn
              (make-instance 'xcb:xfixes:CopyRegion
                             :source extents
                             :destination damage))
          (exwm-cm--add-damage damage))))))

(defsubst exwm-cm--push (newelt place)
  "Similar to `push' but preserve the reference."
  (let ((oldelt (car place)))
    (setf (car place) newelt
          (cdr place) (cons oldelt (cdr place)))))

(defsubst exwm-cm--delq (elt list)
  "Similar to `delq' but preserve the reference."
  (if (eq elt (car list))
      (setf (car list) (cadr list)
            (cdr list) (cddr list))
    (delq elt list)))

(defsubst exwm-cm--assq-delete-all (key alist)
  "Similar to `assq-delete-all' but preserve the reference."
  (when (eq key (caar alist))
    (setf (car alist) (cadr alist)
          (cdr alist) (cddr alist)))
  (assq-delete-all key alist))

(defun exwm-cm--create-tree (&optional xwin)
  "Create a tree with XWIN being the root node."
  (let (tree0 x0 y0 children containers)
    ;; Get the position of this branch.
    (if xwin
        (with-slots (tree x y) (exwm-cm--xwin->attr xwin)
          (setq tree0 (assq xwin (cdr tree))
                x0 x
                y0 y))
      (setq tree0 (list nil)
            x0 0
            y0 0))
    ;; Get children nodes.
    (if (null xwin)
        (setq children (list exwm--root))
      (setq children
            (reverse (slot-value (xcb:+request-unchecked+reply exwm-cm--conn
                                     (make-instance 'xcb:QueryTree
                                                    :window xwin))
                                 'children))))
    ;; Get container nodes.
    ;; Floating frame containers are determined dynamically.
    (cond
     ((null xwin)
      (setq containers `((,exwm--root))))
     ((= xwin exwm--root)
      ;; Workspace containers and the minibuffer frame container.
      (setq containers (mapcar (lambda (f)
                                 (cons (frame-parameter f 'exwm-workspace) f))
                               exwm-workspace--list))
      (when (exwm-workspace--minibuffer-own-frame-p)
        (push (cons
               (frame-parameter exwm-workspace--minibuffer 'exwm-container)
               exwm-workspace--minibuffer)
              containers)))
     ;; No containers in the minibuffer container.
     ((and (exwm-workspace--minibuffer-own-frame-p)
           (= xwin
              (frame-parameter exwm-workspace--minibuffer 'exwm-container))))
     ((= exwm--root
         (slot-value (xcb:+request-unchecked+reply exwm-cm--conn
                         (make-instance 'xcb:QueryTree
                                        :window xwin))
                     'parent))
      ;; Managed X window containers and the workspace frame container.
      (let (frame)
        (catch 'break
          (dolist (f exwm-workspace--list)
            (when (= xwin (frame-parameter f 'exwm-workspace))
              (setq frame f)
              (throw 'break nil))))
        (cl-assert frame)
        (dolist (pair exwm--id-buffer-alist)
          (with-current-buffer (cdr pair)
            (when (eq frame exwm--frame)
              (push (cons exwm--container (cdr pair)) containers))))
        (push (cons (frame-parameter frame 'exwm-container) frame)
              containers))))
    ;; Create subnodes.
    (dolist (xwin children)
      ;; Create attributes.
      (let ((reply (xcb:+request-unchecked+reply exwm-cm--conn
                       (make-instance 'xcb:GetGeometry
                                      :drawable xwin))))
        ;; It's possible the X window has been destroyed.
        (if (null reply)
            (setq xwin nil)
          (when reply
            (with-slots (x y width height) reply
              (exwm-cm--create-attr xwin tree0
                                    (+ x x0) (+ y y0) width height))
            ;; Insert the node.
            (setcdr (or (last (cdr tree0)) tree0) `((,xwin))))))
      (cond
       ((null xwin))
       ((assq xwin containers)
        ;; A branch.  Repeat the process.
        (exwm-cm--create-tree xwin)
        (let ((entity (cdr (assq xwin containers)))
              entity-xwin)
          (when entity
            (setq entity-xwin (if (framep entity)
                                  (frame-parameter entity 'exwm-outer-id)
                                (buffer-local-value 'exwm--id entity)))
            (setf (slot-value (exwm-cm--xwin->attr entity-xwin) 'entity) entity
                  (slot-value (exwm-cm--xwin->attr xwin) 'entity) entity)
            (let ((tmp (exwm-cm--get-parent entity-xwin)))
              (when (/= xwin tmp)
                ;; Workspace frame container.
                (setf (slot-value (exwm-cm--xwin->attr tmp) 'entity)
                      entity))))))
       ((and (null containers)
             (exwm--id->buffer xwin))
        ;; A leaf but a floating frame container might follow.
        (with-current-buffer (exwm--id->buffer xwin)
          (when exwm--floating-frame
            (push (cons (frame-parameter exwm--floating-frame 'exwm-container)
                        exwm--floating-frame)
                  containers))))))))

(defun exwm-cm--restack (xwin above-sibling)
  "Restack X window XWIN so as to it's exactly on top of ABOVE-SIBLING."
  (let ((siblings (exwm-cm--get-siblings xwin))
        node tmp)
    (unless (= 1 (length siblings))
      (setq node (assq xwin siblings))
      (if (= above-sibling xcb:Window:None)
          ;; Put at bottom.
          (unless (eq node (cdr (last siblings)))
            (exwm-cm--delq node siblings)
            (setcdr (last siblings) (list node))
            ;; Set the return value.
            t)
        ;; Insert before the sibling.
        (setq tmp siblings)
        (while (and tmp
                    (/= above-sibling (caar tmp)))
          (setq tmp (cdr tmp)))
        (cl-assert tmp)
        ;; Check if it's already at the requested position.
        (unless (eq tmp (cdr siblings))
          (exwm-cm--delq node siblings)
          (exwm-cm--push node tmp)
          ;; Set the return value.
          t)))))

(declare-function exwm-layout--iconic-state-p "exwm-layout.el" (&optional id))

(defun exwm-cm--paint-tree (tree region &optional force-opaque frame-clip)
  "Paint the tree TREE, with REGION specifying the clipping region.

If FORCE-OPAQUE is non-nil, all X windows painted in this tree is
assumed opaque.  FRAME-CLIP specifies the region should be clipped when
painting a frame."
  (unless tree
    (setq tree (exwm-cm--get-tree exwm--root)))
  (let ((root (car tree))
        xwin attr entity current output outputs queue rectangles)
    ;; Paint subtrees.
    (catch 'break
      (dolist (subtree (cdr tree))
        (setq xwin (car subtree)
              attr (exwm-cm--xwin->attr xwin))
        (cond
         ;; Skip destroyed X windows.
         ((null attr))
         ;; Skip InputOnly X windows.
         ((= xcb:WindowClass:InputOnly
             (slot-value attr 'class)))
         ((and (eq root exwm--root)
               (frame-live-p (setq entity (slot-value attr 'entity)))
               (if (eq entity exwm-workspace--minibuffer)
                   ;; Skip the minibuffer if the current workspace is
                   ;; already painted.
                   (unless (exwm-workspace--minibuffer-attached-p)
                     current)
                 ;; Skip lower workspaces on visited RandR output.
                 ;; If RandR is not enabled, it'll just paint the first.
                 (memq (setq output (frame-parameter entity
                                                     'exwm-randr-output))
                       outputs))))
         ((cdr subtree)
          ;; Paint the subtree.
          (setq entity (slot-value attr 'entity))
          (let (fullscreen clip)
            (cond
             ((buffer-live-p entity)
              (with-current-buffer entity
                ;; Collect frame clip but exclude fullscreen and
                ;; floating X windows.
                (setq fullscreen (memq xcb:Atom:_NET_WM_STATE_FULLSCREEN
                                       exwm--ewmh-state))
                (when (and (null fullscreen)
                           ;; In case it's hidden.
                           (null (exwm-layout--iconic-state-p))
                           ;; The buffer of a floating X windows is not
                           ;; displayed on a workspace frame.
                           (null exwm--floating-frame)
                           ;; Opaque regions are always clipped.
                           (slot-value (exwm-cm--xwin->attr xwin) 'mode))
                  ;; Prepare rectangles to clip the workspace frame.
                  (with-slots (x y width height) (exwm-cm--xwin->attr xwin)
                    (push (make-instance 'xcb:RECTANGLE
                                         :x x
                                         :y y
                                         :width width
                                         :height height)
                          rectangles)))))
             ((and rectangles
                   (frame-live-p entity))
              ;; Prepare region to clip the frame.
              (setq clip (xcb:generate-id exwm-cm--conn))
              (xcb:+request exwm-cm--conn
                  (make-instance 'xcb:xfixes:CreateRegion
                                 :region clip
                                 :rectangles rectangles))))
            (setq queue
                  (nconc (exwm-cm--paint-tree subtree region fullscreen clip)
                         queue))
            (when fullscreen
              ;; Fullscreen X windows are always opaque thus occludes
              ;; anything in this workspace.
              (throw 'break 'fullscreen))
            (when clip
              (xcb:+request exwm-cm--conn
                  (make-instance 'xcb:xfixes:DestroyRegion
                                 :region clip))))
          (if (not (eq root exwm--root))
              ;; Avoid painting any siblings below the workspace frame
              ;; container.
              (when (exwm-workspace--workspace-p (slot-value attr 'entity))
                (throw 'break nil))
            ;; Save some status.
            (when (and (frame-live-p entity)
                       (not (eq entity exwm-workspace--minibuffer)))
              (push output outputs)
              (when (eq entity exwm-workspace--current)
                (setq current t)))))
         ((and force-opaque
               (slot-value attr 'damaged))
          (exwm-cm--paint-opaque xwin region t))
         ((slot-value attr 'damaged)
          ;; Paint damaged leaf.
          (setq entity (slot-value attr 'entity))
          (when (slot-value attr 'mode)
            (push xwin queue))
          (cond
           ((buffer-live-p entity)
            (with-current-buffer entity
              (cl-assert (= xwin exwm--id))
              (when (and exwm--floating-frame
                         ;; Opaque regions are always clipped.
                         (slot-value (exwm-cm--xwin->attr xwin) 'mode))
                ;; Prepare rectangles to clip the floating frame.
                (with-slots (x y width height) (exwm-cm--xwin->attr xwin)
                  (push (make-instance 'xcb:RECTANGLE
                                       :x x
                                       :y y
                                       :width width
                                       :height height)
                        rectangles)))))
           ((and frame-clip
                 (frame-live-p entity))
            ;; Apply frame clip.
            (xcb:+request exwm-cm--conn
                (make-instance 'xcb:xfixes:IntersectRegion
                               :source1 region
                               :source2 frame-clip
                               :destination frame-clip))
            (xcb:+request exwm-cm--conn
                (make-instance 'xcb:xfixes:SubtractRegion
                               :source1 region
                               :source2 frame-clip
                               :destination region))))
          (exwm-cm--paint-opaque xwin region)
          (when (and frame-clip
                     (frame-live-p entity))
            ;; Restore frame clip.
            (xcb:+request exwm-cm--conn
                (make-instance 'xcb:xfixes:UnionRegion
                               :source1 region
                               :source2 frame-clip
                               :destination region)))))))
    ;; Return the queue.
    queue))

(defun exwm-cm--paint-opaque (xwin region &optional force-opaque)
  "Paint an X window XWIN clipped by region REGION if XWIN is opaque.

Also update the attributes of XWIN and clip the region."
  (with-slots (x y width height visual mode picture
                 border-size extents border-clip)
      (exwm-cm--xwin->attr xwin)
    ;; Prepare the X window picture.
    (unless picture
      (setf picture (xcb:generate-id exwm-cm--conn))
      (xcb:+request exwm-cm--conn
          (make-instance 'xcb:render:CreatePicture
                         :pid picture
                         :drawable xwin
                         :format (xcb:renderutil:find-visual-format
                                  (xcb:renderutil:query-formats exwm-cm--conn)
                                  visual)
                         :value-mask 0)))
    ;; Clear cached resources if clip changed.
    (when exwm-cm--clip-changed
      (when border-size
        (xcb:+request exwm-cm--conn
            (make-instance 'xcb:xfixes:DestroyRegion
                           :region border-size))
        (setf border-size nil))
      (when extents
        (xcb:+request exwm-cm--conn
            (make-instance 'xcb:xfixes:DestroyRegion
                           :region extents))
        (setf extents nil))
      (when border-clip
        (xcb:+request exwm-cm--conn
            (make-instance 'xcb:xfixes:DestroyRegion
                           :region border-clip))
        (setf border-clip nil)))
    ;; Retrieve the border.
    (unless border-size
      (setf border-size (xcb:generate-id exwm-cm--conn))
      (xcb:+request exwm-cm--conn
          (make-instance 'xcb:xfixes:CreateRegionFromWindow
                         :region border-size
                         :window xwin
                         :kind xcb:shape:SK:Bounding))
      (xcb:+request exwm-cm--conn
          (make-instance 'xcb:xfixes:TranslateRegion
                         :region border-size
                         :dx x
                         :dy y)))
    ;; Retrieve the extents.
    (unless extents
      (setf extents (xcb:generate-id exwm-cm--conn))
      (xcb:+request exwm-cm--conn
          (make-instance 'xcb:xfixes:CreateRegion
                         :region extents
                         :rectangles (list (make-instance 'xcb:RECTANGLE
                                                          :x x
                                                          :y y
                                                          :width width
                                                          :height height)))))
    (cond
     ((and mode
           (null force-opaque))
      ;; Calculate clipped border for the transparent X window.
      (unless border-clip
        (setf border-clip (xcb:generate-id exwm-cm--conn))
        (xcb:+request exwm-cm--conn
            (make-instance 'xcb:xfixes:CreateRegion
                           :region border-clip
                           :rectangles nil))
        (xcb:+request exwm-cm--conn
            (make-instance 'xcb:xfixes:CopyRegion
                           :source region
                           :destination border-clip))
        (xcb:+request exwm-cm--conn
            (make-instance 'xcb:xfixes:IntersectRegion
                           :source1 border-clip
                           :source2 border-size
                           :destination border-clip))))
     (t
      ;; Clip & render for the opaque X window.
      ;; Set the clip region for the rendering buffer.
      (xcb:+request exwm-cm--conn
          (make-instance 'xcb:xfixes:SetPictureClipRegion
                         :picture exwm-cm--buffer
                         :region region
                         :x-origin 0
                         :y-origin 0))
      ;; Clip the region with border.
      (xcb:+request exwm-cm--conn
          (make-instance 'xcb:xfixes:SubtractRegion
                         :source1 region
                         :source2 border-size
                         :destination region))
      ;; Render the picture to the buffer.
      (xcb:+request exwm-cm--conn
          (make-instance 'xcb:render:Composite
                         :op xcb:render:PictOp:Src
                         :src picture
                         :mask xcb:render:Picture:None
                         :dst exwm-cm--buffer
                         :src-x 0
                         :src-y 0
                         :mask-x 0
                         :mask-y 0
                         :dst-x x
                         :dst-y y
                         :width width
                         :height height))))))

(defun exwm-cm--paint-transparent (xwin)
  "Paint a transparent X window XWIN."
  (with-slots (x y width height opacity picture alpha-picture border-clip)
      (exwm-cm--xwin->attr xwin)
    ;; Prepare the alpha picture for transparent X windows.
    (when (and opacity (null alpha-picture))
      (setf alpha-picture (xcb:generate-id exwm-cm--conn))
      (let ((pixmap (xcb:generate-id exwm-cm--conn)))
        (xcb:+request exwm-cm--conn
            (make-instance 'xcb:CreatePixmap
                           :depth 8
                           :pid pixmap
                           :drawable exwm--root
                           :width 1
                           :height 1))
        (xcb:+request exwm-cm--conn
            (make-instance 'xcb:render:CreatePicture
                           :pid alpha-picture
                           :drawable pixmap
                           :format (xcb:renderutil:find-standard
                                    (xcb:renderutil:query-formats
                                     exwm-cm--conn)
                                    xcb:renderutil:PICT_STANDARD:A_8)
                           :value-mask xcb:render:CP:Repeat
                           :repeat 1))
        (xcb:+request exwm-cm--conn
            (make-instance 'xcb:render:FillRectangles
                           :op xcb:render:PictOp:Src
                           :dst alpha-picture
                           :color (make-instance 'xcb:render:COLOR
                                                 :red 0
                                                 :green 0
                                                 :blue 0
                                                 :alpha opacity)
                           :rects (list (make-instance 'xcb:RECTANGLE
                                                       :x 0
                                                       :y 0
                                                       :width 1
                                                       :height 1))))))
    ;; Set the clip region for the rendering buffer.
    (xcb:+request exwm-cm--conn
        (make-instance 'xcb:xfixes:SetPictureClipRegion
                       :picture exwm-cm--buffer
                       :region border-clip
                       :x-origin 0
                       :y-origin 0))
    ;; Render the picture to the buffer.
    (xcb:+request exwm-cm--conn
        (make-instance 'xcb:render:Composite
                       :op xcb:render:PictOp:Over
                       :src picture
                       :mask (or alpha-picture xcb:render:Picture:None)
                       :dst exwm-cm--buffer
                       :src-x 0
                       :src-y 0
                       :mask-x 0
                       :mask-y 0
                       :dst-x x
                       :dst-y y
                       :width width
                       :height height))
    (xcb:+request exwm-cm--conn
        (make-instance 'xcb:xfixes:DestroyRegion
                       :region border-clip))
    (setf border-clip nil)))

(defun exwm-cm--paint (&optional region)
  "Paint the whole tree within clipping region REGION.

If REGION is omitted, `exwm-cm--damages' is assumed.  If it's t, paint
the whole screen."
  ;; Prepare the clipping region.
  (cond
   ((null region)
    (when exwm-cm--damages
      (setq region exwm-cm--damages)))
   ((eq region t)
    (with-slots (width height) (exwm-cm--xwin->attr exwm--root)
      (let ((rect (make-instance 'xcb:RECTANGLE
                                 :x 0
                                 :y 0
                                 :width width
                                 :height height)))
        (setq region (xcb:generate-id exwm-cm--conn))
        (xcb:+request exwm-cm--conn
            (make-instance 'xcb:xfixes:CreateRegion
                           :region region
                           :rectangles (list rect)))))))
  (when region
    ;; Prepare the rendering buffer.
    (unless exwm-cm--buffer
      (let ((pixmap (xcb:generate-id exwm-cm--conn))
            (picture (xcb:generate-id exwm-cm--conn)))
        (setq exwm-cm--buffer picture)
        (with-slots (width height visual) (exwm-cm--xwin->attr exwm--root)
          (xcb:+request exwm-cm--conn
              (make-instance 'xcb:CreatePixmap
                             :depth exwm-cm--depth
                             :pid pixmap
                             :drawable exwm--root
                             :width width
                             :height height))
          (xcb:+request exwm-cm--conn
              (make-instance 'xcb:render:CreatePicture
                             :pid picture
                             :drawable pixmap
                             :format (xcb:renderutil:find-visual-format
                                      (xcb:renderutil:query-formats
                                       exwm-cm--conn)
                                      visual)
                             :value-mask 0)))
        (xcb:+request exwm-cm--conn
            (make-instance 'xcb:FreePixmap
                           :pixmap pixmap))))
    (let (queue)
      ;; Paint opaque X windows and update clipping region.
      (setq queue (exwm-cm--paint-tree nil region))
      ;; Paint the background.
      (exwm-cm--paint-background region)
      ;; Paint transparent X windows.
      (while queue
        (exwm-cm--paint-transparent (pop queue))))
    ;; Submit changes.
    (with-slots (width height picture) (exwm-cm--xwin->attr exwm--root)
      (xcb:+request exwm-cm--conn
          (make-instance 'xcb:xfixes:SetPictureClipRegion
                         :picture exwm-cm--buffer
                         :region xcb:xfixes:Region:None
                         :x-origin 0
                         :y-origin 0))
      (xcb:+request exwm-cm--conn
          (make-instance 'xcb:render:Composite
                         :op xcb:render:PictOp:Src
                         :src exwm-cm--buffer
                         :mask xcb:render:Picture:None
                         :dst picture
                         :src-x 0
                         :src-y 0
                         :mask-x 0
                         :mask-y 0
                         :dst-x 0
                         :dst-y 0
                         :width width
                         :height height)))
    ;; Cleanup.
    (xcb:+request exwm-cm--conn
        (make-instance 'xcb:xfixes:DestroyRegion
                       :region region))
    (when (eq region exwm-cm--damages)
      (setq exwm-cm--damages nil))
    (setq exwm-cm--clip-changed nil)
    (xcb:flush exwm-cm--conn)))

(defun exwm-cm--paint-background (region)
  "Paint the background."
  (unless exwm-cm--background
    (setq exwm-cm--background (xcb:generate-id exwm-cm--conn))
    (let (pixmap exist)
      (catch 'break
        (dolist (atom exwm-cm--background-atoms)
          (with-slots (~lsb format value-len value)
              (xcb:+request-unchecked+reply exwm-cm--conn
                  (make-instance 'xcb:GetProperty
                                 :delete 0
                                 :window exwm--root
                                 :property atom
                                 :type xcb:Atom:PIXMAP
                                 :long-offset 0
                                 :long-length 4))
            (when (and (= format 32)
                       (= 1 value-len))
              (setq pixmap (if ~lsb
                               (xcb:-unpack-u4-lsb value 0)
                             (xcb:-unpack-u4 value 0)))
              (setq exist t)
              (throw 'break nil)))))
      (unless pixmap
        (setq pixmap (xcb:generate-id exwm-cm--conn))
        (xcb:+request exwm-cm--conn
            (make-instance 'xcb:CreatePixmap
                           :depth exwm-cm--depth
                           :pid pixmap
                           :drawable exwm--root
                           :width 1
                           :height 1)))
      (xcb:+request exwm-cm--conn
          (make-instance 'xcb:render:CreatePicture
                         :pid exwm-cm--background
                         :drawable pixmap
                         :format (xcb:renderutil:find-visual-format
                                  (xcb:renderutil:query-formats exwm-cm--conn)
                                  (slot-value (exwm-cm--xwin->attr exwm--root)
                                              'visual))
                         :value-mask xcb:render:CP:Repeat
                         :repeat 1))
      (unless exist
        (xcb:+request exwm-cm--conn
            (make-instance 'xcb:render:FillRectangles
                           :op xcb:render:PictOp:Src
                           :dst exwm-cm--background
                           :color (make-instance 'xcb:render:COLOR
                                                 :red #x8080
                                                 :green #x8080
                                                 :blue #x8080
                                                 :alpha #xFFFF)
                           :rects (list (make-instance 'xcb:RECTANGLE
                                                       :x 0
                                                       :y 0
                                                       :width 1
                                                       :height 1)))))))
  (xcb:+request exwm-cm--conn
      (make-instance 'xcb:xfixes:SetPictureClipRegion
                     :picture exwm-cm--buffer
                     :region region
                     :x-origin 0
                     :y-origin 0))
  (with-slots (width height) (exwm-cm--xwin->attr exwm--root)
    (xcb:+request exwm-cm--conn
        (make-instance 'xcb:render:Composite
                       :op xcb:render:PictOp:Src
                       :src exwm-cm--background
                       :mask xcb:render:Picture:None
                       :dst exwm-cm--buffer
                       :src-x 0
                       :src-y 0
                       :mask-x 0
                       :mask-y 0
                       :dst-x 0
                       :dst-y 0
                       :width width
                       :height height))))

(defun exwm-cm--map-xwin (xwin &optional silent)
  "Prepare to map X window XWIN."
  (let ((attr (exwm-cm--xwin->attr xwin)))
    (setf (slot-value attr 'damaged) nil)
    ;; Add to damage.
    (when (slot-value attr 'extents)
      (let ((damage (xcb:generate-id exwm-cm--conn)))
        (xcb:+request exwm-cm--conn
            (make-instance 'xcb:xfixes:CreateRegion
                           :region damage
                           :rectangles nil))
        (xcb:+request exwm-cm--conn
            (make-instance 'xcb:xfixes:CopyRegion
                           :source (slot-value attr 'extents)
                           :destination damage))
        (exwm-cm--add-damage damage))
      (unless silent
        (exwm-cm--paint)))))

(defun exwm-cm--on-MapNotify (data _synthetic)
  "Handle MapNotify events."
  (let ((obj (make-instance 'xcb:MapNotify))
        attr)
    (xcb:unmarshal obj data)
    (with-slots (event window) obj
      (exwm--log "(CM) MapNotify: Try to map #x%X" window)
      (setq attr (exwm-cm--xwin->attr window))
      (when (and attr
                 (/= (slot-value attr 'class) xcb:WindowClass:InputOnly)
                 (or (= event exwm--root)
                     ;; Filter out duplicated events.
                     (/= exwm--root (exwm-cm--get-parent window))))
        (exwm--log "(CM) MapNotify: Map")
        (exwm-cm--map-xwin window)))))

(defun exwm-cm--on-UnmapNotify (data _synthetic)
  "Handle UnmapNotify events."
  (let ((obj (make-instance 'xcb:UnmapNotify))
        attr)
    (xcb:unmarshal obj data)
    (with-slots (event window) obj
      (exwm--log "(CM) UnmapNotify: Try to unmap #x%X" window)
      (setq attr (exwm-cm--xwin->attr window))
      (when (and attr
                 (or (= event exwm--root)
                     ;; Filter out duplicated events.
                     (/= exwm--root (exwm-cm--get-parent window))))
        (exwm--log "(CM) UnmapNotify: Unmap")
        (with-slots (picture damaged border-size extents border-clip) attr
          (setf damaged nil)
          (when picture
            (xcb:+request exwm-cm--conn
                (make-instance 'xcb:render:FreePicture
                               :picture picture))
            (setf picture nil))
          (when border-size
            (xcb:+request exwm-cm--conn
                (make-instance 'xcb:xfixes:DestroyRegion
                               :region border-size))
            (setf border-size nil))
          (when extents
            (exwm-cm--add-damage extents)
            (setf extents nil))
          (when border-clip
            (xcb:+request exwm-cm--conn
                (make-instance 'xcb:xfixes:DestroyRegion
                               :region border-clip))
            (setf border-clip nil)))
        (setq exwm-cm--clip-changed t)
        (exwm-cm--paint)))))

(defun exwm-cm--on-CreateNotify (data _synthetic)
  "Handle CreateNotify events."
  (let ((obj (make-instance 'xcb:CreateNotify))
        tree0)
    (xcb:unmarshal obj data)
    (with-slots (window parent x y width height) obj
      (exwm--log "(CM) CreateNotify: Create #x%X on #x%X @%sx%s%+d%+d"
                 window parent width height x y)
      (cl-assert (= parent exwm--root))
      (cl-assert (null (exwm-cm--xwin->attr window)))
      (setq tree0 (exwm-cm--get-subtree parent))
      (exwm-cm--create-attr window tree0 x y width height)
      (if (cdr tree0)
          (exwm-cm--push (list window) (cdr tree0))
        (setcdr tree0 `((,window)))))))

(defun exwm-cm--on-ConfigureNotify (data synthetic)
  "Handle ConfigureNotify events."
  ;; Ignore synthetic ConfigureNotify events sent by the WM.
  (unless synthetic
    (let ((obj (make-instance 'xcb:ConfigureNotify)))
      (xcb:unmarshal obj data)
      (with-slots (event window above-sibling x y width height) obj
        (exwm--log
         "(CM) ConfigureNotify: Try to configure #x%X @%sx%s%+d%+d, above #x%X"
         window width height x y above-sibling)
        (cond
         ((= window exwm--root)
          (exwm--log "(CM) ConfigureNotify: Configure the root X window")
          (when exwm-cm--buffer
            (xcb:+request exwm-cm--conn
                (make-instance 'xcb:render:FreePicture
                               :picture exwm-cm--buffer))
            (setq exwm-cm--buffer nil))
          (with-slots ((x* x)
                       (y* y)
                       (width* width)
                       (height* height))
              (exwm-cm--xwin->attr exwm--root)
            (setf x* x
                  y* y
                  width* width
                  height* height))
          (exwm-cm--paint))
         ((null (exwm-cm--xwin->attr window)))
         ((or (= event exwm--root)
              ;; Filter out duplicated events.
              (/= exwm--root (exwm-cm--get-parent window)))
          (exwm--log "(CM) ConfigureNotify: Configure")
          (with-slots ((x0 x)
                       (y0 y))
              (exwm-cm--xwin->attr (exwm-cm--get-parent window))
            (exwm-cm--update-geometry window (+ x x0) (+ y y0) width height
                                      above-sibling))
          (setq exwm-cm--clip-changed t)
          (exwm-cm--paint))
         (t
          (exwm--log "(CM) ConfigureNotify: Skip event from #x%X" event)))))))

(defun exwm-cm--destroy (xwin)
  "Prepare to destroy X window XWIN."
  (with-slots (tree picture alpha-picture damage
                    border-size extents border-clip)
      (exwm-cm--xwin->attr xwin)
    (cl-assert (assq xwin (cdr tree)))
    (if (= 1 (length (cdr tree)))
        (setcdr tree nil)
      (exwm-cm--assq-delete-all xwin (cdr tree)))
    (remhash xwin exwm-cm--hash)
    ;; Release resources.
    (when picture
      (xcb:+request exwm-cm--conn
          (make-instance 'xcb:render:FreePicture
                         :picture picture))
      (setf picture nil))
    (when alpha-picture
      (xcb:+request exwm-cm--conn
          (make-instance 'xcb:render:FreePicture
                         :picture alpha-picture))
      (setf alpha-picture nil))
    (when damage
      (xcb:+request exwm-cm--conn
          (make-instance 'xcb:damage:Destroy
                         :damage damage))
      (setf damage nil))
    (when border-size
      (xcb:+request exwm-cm--conn
          (make-instance 'xcb:xfixes:DestroyRegion
                         :region border-size))
      (setf border-size nil))
    (when extents
      (exwm-cm--add-damage extents)
      (setf extents nil))
    (when border-clip
      (xcb:+request exwm-cm--conn
          (make-instance 'xcb:xfixes:DestroyRegion
                         :region border-clip))
      (setf border-clip nil))))

(defun exwm-cm--on-DestroyNotify (data _synthetic)
  "Handle DestroyNotify events."
  (let ((obj (make-instance 'xcb:DestroyNotify))
        xwin)
    (xcb:unmarshal obj data)
    (setq xwin (slot-value obj 'window))
    (exwm--log "(CM) DestroyNotify: Try to destroy #x%X" xwin)
    (when (exwm-cm--xwin->attr xwin)
      (exwm--log "(CM) DestroyNotify: Destroy")
      (exwm-cm--destroy xwin))))

(defun exwm-cm--on-CirculateNotify (data _synthetic)
  "Handle CirculateNotify events."
  (let ((obj (make-instance 'xcb:CirculateNotify))
        attr)
    (xcb:unmarshal obj data)
    (with-slots (event window place) obj
      (setq attr (exwm-cm--xwin->attr window))
      (exwm--log "(CM) CirculateNotify: Try to circulate #x%X to %s"
                 window place)
      (when (and attr
                 (or (= event exwm--root)
                     ;; Filter out duplicated events.
                     (/= exwm--root (exwm-cm--get-parent window))))
        (exwm--log "(CM) CirculateNotify: Circulate")
        (exwm-cm--update-geometry window nil nil nil nil
                                  (if (= place xcb:Circulate:LowerHighest)
                                      xcb:Window:None
                                    (caar (exwm-cm--get-siblings window))))
        (setq exwm-cm--clip-changed t)
        (exwm-cm--paint)))))

(defun exwm-cm--on-Expose (data _synthetic)
  "Handle Expose events."
  (let ((obj (make-instance 'xcb:Expose)))
    (xcb:unmarshal obj data)
    (with-slots (window x y width height count) obj
      (when (eq window exwm--root)
        (push (make-instance 'xcb:RECTANGLE
                             :x x
                             :y y
                             :width width
                             :height height)
              exwm-cm--expose-rectangles))
      (when (= count 0)
        (let ((region (xcb:generate-id exwm-cm--conn)))
          (xcb:+request exwm-cm--conn
              (xcb:xfixes:CreateRegion
               :region region
               :rectangles exwm-cm--expose-rectangles))
          (exwm-cm--add-damage region))
        (setq exwm-cm--expose-rectangles nil)
        (exwm-cm--paint)))))

(defun exwm-cm--on-PropertyNotify (data _synthetic)
  "Handle PropertyNotify events."
  (let ((obj (make-instance 'xcb:PropertyNotify)))
    (xcb:unmarshal obj data)
    (with-slots (window atom) obj
      (cond
       ((and (= window exwm--root)
             (memq atom exwm-cm--background-atoms))
        (exwm--log "(CM) PropertyNotify: Update background")
        (when exwm-cm--background
          (xcb:+request exwm-cm--conn
              (make-instance 'xcb:render:FreePicture
                             :picture exwm-cm--background))
          (setq exwm-cm--background nil)
          (xcb:+request exwm-cm--conn
              (make-instance 'xcb:ClearArea
                             :exposures 1
                             :window exwm--root
                             :x 0
                             :y 0
                             :width 0
                             :height 0))
          (xcb:flush exwm-cm--conn)))
       ((and (= atom exwm-cm--_NET_WM_WINDOW_OPACITY)
             ;; Some applications also set this property on their parents.
             (null (cdr (exwm-cm--get-subtree window))))
        (when (exwm-cm--xwin->attr window)
          (exwm--log "(CM) PropertyNotify: Update opacity for #x%X" window)
          (exwm-cm--update-opacity window)
          (exwm-cm--paint)))))))

(defun exwm-cm--prepare-container (xwin)
  "Make X window XWIN a container by deselecting unnecessary events."
  (with-slots (damage) (exwm-cm--xwin->attr xwin)
    (when damage
      (xcb:+request exwm-cm--conn
          (make-instance 'xcb:damage:Destroy
                         :damage damage)))
    (xcb:+request exwm-cm--conn
        (make-instance 'xcb:shape:SelectInput
                       :destination-window xwin
                       :enable 0))))

(defun exwm-cm--on-ReparentNotify (data _synthetic)
  "Handle ReparentNotify events."
  (let ((obj (make-instance 'xcb:ReparentNotify))
        tree tree0 grandparent great-grandparent entity)
    (xcb:unmarshal obj data)
    (with-slots (window parent x y) obj
      (exwm--log "(CM) ReparentNotify: Try to reparent #x%X to #x%X @%+d%+d"
                 window parent x y)
      (cond
       ((null (exwm-cm--xwin->attr window))
        (when (eq parent exwm--root)
          (exwm--log "(CM) ReparentNotify: Create on the root X window")
          (let ((reply (xcb:+request-unchecked+reply exwm-cm--conn
                           (make-instance 'xcb:GetGeometry
                                          :drawable window))))
            (when reply
              (with-slots (width height) reply
                (setq tree0 (exwm-cm--get-subtree exwm--root))
                (exwm-cm--create-attr window tree0 x y width height)
                (if (cdr tree0)
                    (exwm-cm--push (list window) (cdr tree0))
                  (setcdr tree0 `((,window)))))
              (exwm-cm--paint)))))
       ((= parent (exwm-cm--get-parent window)))
       (t
        (unless (exwm-cm--xwin->attr parent)
          ;; Only allow workspace frame here.
          (setq grandparent
                (slot-value (xcb:+request-unchecked+reply exwm-cm--conn
                                (make-instance 'xcb:QueryTree
                                               :window parent))
                            'parent))
          (cond
           ((null (exwm-cm--xwin->attr grandparent))
            (exwm--log "(CM) ReparentNotify: Destroy (too deep)"))
           ((and (= exwm--root
                    (setq great-grandparent (exwm-cm--get-parent grandparent)))
                 (setq tree0 (exwm-cm--get-subtree grandparent))
                 (or (setq entity (exwm--id->buffer window))
                     (null (cdr tree0))))
            ;; Reparent a workspace frame or an X window into its
            ;; container.
            (exwm--debug
             (if entity
                 (exwm--log "(CM) ReparentNotify: \
Create implicit X window container")
               (exwm--log "(CM) ReparentNotify: \
Create implicit workspace frame container")))
            (unless entity
              (setq entity 'workspace-frame))
            (with-slots ((x0 x)
                         (y0 y))
                (exwm-cm--xwin->attr grandparent)
              (with-slots (x y width height)
                  (xcb:+request-unchecked+reply exwm-cm--conn
                      (make-instance 'xcb:GetGeometry
                                     :drawable parent))
                (exwm-cm--create-attr parent tree0
                                      (+ x x0) (+ y y0) width height)))
            (if (null (cdr tree0))
                (setcdr tree0 `((,parent)))
              ;; The stacking order of the parent is unknown.
              (let* ((siblings
                      (slot-value (xcb:+request-unchecked+reply exwm-cm--conn
                                      (make-instance 'xcb:QueryTree
                                                     :window grandparent))
                                  'children)))
                (cl-assert (memq parent siblings))
                (if (= parent (car siblings))
                    ;; At the bottom.
                    (setcdr (last (cdr tree0)) `((,parent)))
                  ;; Insert it.
                  (exwm-cm--push (list parent)
                                 ;; The stacking order is reversed.
                                 (nthcdr (- (length siblings) 1
                                            (cl-position parent siblings))
                                         (cdr tree0)))))))
           ((and (= exwm--root
                    (exwm-cm--get-parent great-grandparent))
                 (setq tree0 (exwm-cm--get-subtree grandparent))
                 (= 1 (length (cdr tree0)))
                 (exwm--id->buffer (caar (cdr tree0))))
            ;; Reparent a floating frame into its container.
            (exwm--log "(CM) ReparentNotify: Create floating frame container")
            (setq entity 'floating-frame)
            (with-slots ((x0 x)
                         (y0 y))
                (exwm-cm--xwin->attr grandparent)
              (with-slots (x y width height)
                  (xcb:+request-unchecked+reply exwm-cm--conn
                      (make-instance 'xcb:GetGeometry
                                     :drawable parent))
                (exwm-cm--create-attr parent tree0
                                      (+ x x0) (+ y y0) width height)))
            (nconc (cdr tree0) `((,parent))))
           (t
            (exwm--log "(CM) ReparentNotify: Destroy")
            (exwm-cm--destroy window))))
        ;; Ensure there's a valid parent.
        (when (exwm-cm--xwin->attr parent)
          (exwm--log "(CM) ReparentNotify: Reparent")
          (when (null (cdr (exwm-cm--get-subtree parent)))
            ;; The parent is a new container.
            (exwm-cm--prepare-container parent))
          (setq tree (exwm-cm--get-subtree window))
          (let ((tree (exwm-cm--get-tree window)))
            (if (= 1 (length (cdr tree)))
                (setcdr tree nil)
              (exwm-cm--assq-delete-all window (cdr tree))))
          (setq tree0 (exwm-cm--get-subtree parent))
          (exwm-cm--set-tree window tree0)
          ;; The size might have already changed (e.g. when reparenting
          ;; a workspace frame).
          (let ((reply (xcb:+request-unchecked+reply exwm-cm--conn
                           (make-instance 'xcb:GetGeometry
                                          :drawable window))))
            ;; The X window might have already been destroyed.
            (when reply
              (with-slots (width height) reply
                (with-slots ((x0 x)
                             (y0 y))
                    (exwm-cm--xwin->attr parent)
                  (exwm-cm--update-geometry window (+ x x0) (+ y y0)
                                            width height)))))
          (when entity
            ;; Decide frame entity.
            (when (symbolp entity)
              (catch 'break
                (dolist (f (if (eq entity 'workspace-frame)
                               exwm-workspace--list
                             (frame-list)))
                  (when (eq window (frame-parameter f 'exwm-outer-id))
                    (setq entity f)
                    (throw 'break nil))))
              (when (exwm-workspace--workspace-p entity)
                ;; The grandparent is a new workspace container.
                (exwm-cm--prepare-container grandparent)
                (setf (slot-value (exwm-cm--xwin->attr grandparent) 'entity)
                      entity)))
            (setf (slot-value (exwm-cm--xwin->attr parent) 'entity) entity)
            (setf (slot-value (exwm-cm--xwin->attr window) 'entity) entity))
          (if (cdr tree0)
              (exwm-cm--push tree (cdr tree0))
            (setcdr tree0 `(,tree)))
          (exwm-cm--paint)))))))

(defun exwm-cm--add-damage (damage)
  "Add region DAMAGE to `exwm-cm--damages'."
  (if (not exwm-cm--damages)
      (setq exwm-cm--damages damage)
    (xcb:+request exwm-cm--conn
        (make-instance 'xcb:xfixes:UnionRegion
                       :source1 exwm-cm--damages
                       :source2 damage
                       :destination exwm-cm--damages))
    (xcb:+request exwm-cm--conn
        (make-instance 'xcb:xfixes:DestroyRegion
                       :region damage))))

(defun exwm-cm--on-DamageNotify (data _synthetic)
  "Handle DamageNotify events."
  (let ((obj (make-instance 'xcb:damage:Notify))
        parts)
    (xcb:unmarshal obj data)
    (cl-assert (exwm-cm--xwin->attr (slot-value obj 'drawable)))
    (with-slots (x y width height damaged damage)
        (exwm-cm--xwin->attr (slot-value obj 'drawable))
      (setq parts (xcb:generate-id exwm-cm--conn))
      (cond
       (damaged
        (xcb:+request exwm-cm--conn
            (make-instance 'xcb:xfixes:CreateRegion
                           :region parts
                           :rectangles nil))
        (xcb:+request exwm-cm--conn
            (make-instance 'xcb:damage:Subtract
                           :damage damage
                           :repair xcb:xfixes:Region:None
                           :parts parts))
        (xcb:+request exwm-cm--conn
            (make-instance 'xcb:xfixes:TranslateRegion
                           :region parts
                           :dx x
                           :dy y)))
       (t
        (setf damaged t)
        (xcb:+request exwm-cm--conn
            (make-instance 'xcb:xfixes:CreateRegion
                           :region parts
                           :rectangles (list (make-instance 'xcb:RECTANGLE
                                                            :width width
                                                            :height height
                                                            :x x
                                                            :y y))))
        (xcb:+request exwm-cm--conn
            (make-instance 'xcb:damage:Subtract
                           :damage damage
                           :repair xcb:xfixes:Region:None
                           :parts xcb:xfixes:Region:None))))
      (exwm-cm--add-damage parts))
    ;; Check if there are more damages immediately followed.
    (unless (/= 0 (logand #x80 (slot-value obj 'level)))
      (exwm-cm--paint))))

(defun exwm-cm--on-ShapeNotify (data _synthetic)
  "Handle ShapeNotify events."
  (let ((obj (make-instance 'xcb:shape:Notify))
        attr region1 region2)
    (xcb:unmarshal obj data)
    (with-slots (shape-kind affected-window shaped
                            extents-x extents-y extents-width extents-height)
        obj
      (exwm--log "(CM) ShapeNotify: #x%X" affected-window)
      (when (and (or (eq shape-kind xcb:shape:SK:Clip)
                     (eq shape-kind xcb:shape:SK:Bounding))
                 (setq attr (exwm-cm--xwin->attr affected-window)))
        (with-slots ((shaped* shaped)
                     x y width height
                     shape-x shape-y shape-width shape-height)
            attr
          (setq region1 (xcb:generate-id exwm-cm--conn)
                region2 (xcb:generate-id exwm-cm--conn))
          (xcb:+request exwm-cm--conn
              (make-instance 'xcb:xfixes:CreateRegion
                             :region region1
                             :rectangles `(,(make-instance 'xcb:RECTANGLE
                                                           :width shape-width
                                                           :height shape-height
                                                           :x shape-x
                                                           :y shape-y))))
          (if shaped
              (setf shaped* t
                    shape-x (+ x extents-x)
                    shape-y (+ y extents-y)
                    shape-width (+ width extents-width)
                    shape-height (+ height extents-height))
            (setf shaped* nil
                  shape-x x
                  shape-y y
                  shape-width width
                  shape-height height))
          (xcb:+request exwm-cm--conn
              (make-instance 'xcb:xfixes:CreateRegion
                             :region region2
                             :rectangles `(,(make-instance 'xcb:RECTANGLE
                                                           :width shape-width
                                                           :height shape-height
                                                           :x shape-x
                                                           :y shape-y))))
          (xcb:+request exwm-cm--conn
              (make-instance 'xcb:xfixes:UnionRegion
                             :source1 region1
                             :source2 region2
                             :destination region1))
          (xcb:+request exwm-cm--conn
              (make-instance 'xcb:xfixes:DestroyRegion
                             :region region2))
          (setq exwm-cm--clip-changed t)
          (exwm-cm--paint region1))))))

(defun exwm-cm--init ()
  "Initialize EXWM compositing manager."
  ;; Create a new connection.
  (setq exwm-cm--conn (xcb:connect))
  (set-process-query-on-exit-flag (slot-value exwm-cm--conn 'process) nil)
  ;; Initialize ICCCM/EWMH support.
  (xcb:icccm:init exwm-cm--conn)
  (xcb:ewmh:init exwm-cm--conn)
  ;; Check for Render extension.
  (let ((version (xcb:renderutil:query-version exwm-cm--conn)))
    (unless (and version
                 (= 0 (slot-value version 'major-version))
                 (<= 2 (slot-value version 'minor-version)))
      (error "[EXWM] The server does not support Render extension")))
  ;; Check for Composite extension.
  (when (or (= 0
               (slot-value (xcb:get-extension-data exwm-cm--conn
                                                   'xcb:composite)
                           'present))
            (with-slots (major-version minor-version)
                (xcb:+request-unchecked+reply exwm-cm--conn
                    (make-instance 'xcb:composite:QueryVersion
                                   :client-major-version 0
                                   :client-minor-version 1))
              (or (/= major-version 0) (< minor-version 1))))
    (error "[EXWM] The server does not support Composite extension"))
  ;; Check for Damage extension.
  (when (or (= 0 (slot-value (xcb:get-extension-data exwm-cm--conn 'xcb:damage)
                             'present))
            (with-slots (major-version minor-version)
                (xcb:+request-unchecked+reply exwm-cm--conn
                    (make-instance 'xcb:damage:QueryVersion
                                   :client-major-version 1
                                   :client-minor-version 1))
              (or (/= major-version 1) (< minor-version 1))))
    (error "[EXWM] The server does not support Damage extension"))
  ;; Check for XFixes extension.
  (when (or (= 0 (slot-value (xcb:get-extension-data exwm-cm--conn 'xcb:xfixes)
                             'present))
            (with-slots (major-version minor-version)
                (xcb:+request-unchecked+reply exwm-cm--conn
                    (make-instance 'xcb:xfixes:QueryVersion
                                   :client-major-version 2
                                   :client-minor-version 0))
              (or (/= major-version 2) (/= minor-version 0))))
    (error "[EXWM] The server does not support XFixes extension"))
  ;; Check for Shape extension.
  (when (or (= 0 (slot-value (xcb:get-extension-data exwm-cm--conn 'xcb:shape)
                             'present))
            (with-slots (major-version minor-version)
                (xcb:+request-unchecked+reply exwm-cm--conn
                    (make-instance 'xcb:shape:QueryVersion))
              (or (/= major-version 1) (< minor-version 1))))
    (error "[EXWM] The server does not support Shape extension"))
  ;; Intern atoms.
  (let ((atom-name "_NET_WM_WINDOW_OPACITY"))
    (setq exwm-cm--_NET_WM_WINDOW_OPACITY
          (slot-value (xcb:+request-unchecked+reply exwm-cm--conn
                          (make-instance 'xcb:InternAtom
                                         :only-if-exists 0
                                         :name-len (length atom-name)
                                         :name atom-name))
                      'atom)))
  (setq exwm-cm--background-atoms
        (mapcar (lambda (atom-name)
                  (slot-value (xcb:+request-unchecked+reply exwm-cm--conn
                                  (make-instance 'xcb:InternAtom
                                                 :only-if-exists 0
                                                 :name-len (length atom-name)
                                                 :name atom-name))
                              'atom))
                exwm-cm--background-atom-names))
  ;; Register CM.
  (with-slots (owner)
      (xcb:+request-unchecked+reply exwm-cm--conn
          (make-instance 'xcb:GetSelectionOwner
                         :selection xcb:Atom:_NET_WM_CM_S0))
    (when (/= owner xcb:Window:None)
      (error "[EXWM] Other compositing manager detected")))
  (let ((id (xcb:generate-id exwm-cm--conn)))
    (xcb:+request exwm-cm--conn
        (make-instance 'xcb:CreateWindow
                       :depth 0
                       :wid id
                       :parent exwm--root
                       :x 0
                       :y 0
                       :width 1
                       :height 1
                       :border-width 0
                       :class xcb:WindowClass:InputOnly
                       :visual 0
                       :value-mask xcb:CW:OverrideRedirect
                       :override-redirect 1))
    ;; Set _NET_WM_NAME.
    (xcb:+request exwm-cm--conn
        (make-instance 'xcb:ewmh:set-_NET_WM_NAME
                       :window id
                       :data "EXWM-CM"))
    ;; Get the selection ownership.
    (xcb:+request exwm-cm--conn
        (make-instance 'xcb:SetSelectionOwner
                       :owner id
                       :selection xcb:Atom:_NET_WM_CM_S0
                       :time xcb:Time:CurrentTime)))
  ;; Attach event listeners.
  (xcb:+event exwm-cm--conn 'xcb:MapNotify #'exwm-cm--on-MapNotify)
  (xcb:+event exwm-cm--conn 'xcb:UnmapNotify #'exwm-cm--on-UnmapNotify)
  (xcb:+event exwm-cm--conn 'xcb:CreateNotify #'exwm-cm--on-CreateNotify)
  (xcb:+event exwm-cm--conn 'xcb:ConfigureNotify #'exwm-cm--on-ConfigureNotify)
  (xcb:+event exwm-cm--conn 'xcb:DestroyNotify #'exwm-cm--on-DestroyNotify)
  (xcb:+event exwm-cm--conn 'xcb:ReparentNotify #'exwm-cm--on-ReparentNotify)
  (xcb:+event exwm-cm--conn 'xcb:CirculateNotify #'exwm-cm--on-CirculateNotify)
  (xcb:+event exwm-cm--conn 'xcb:Expose #'exwm-cm--on-Expose)
  (xcb:+event exwm-cm--conn 'xcb:PropertyNotify #'exwm-cm--on-PropertyNotify)
  (xcb:+event exwm-cm--conn 'xcb:damage:Notify #'exwm-cm--on-DamageNotify)
  (xcb:+event exwm-cm--conn 'xcb:shape:Notify #'exwm-cm--on-ShapeNotify)
  ;; Scan the window tree.
  (setq exwm-cm--hash (make-hash-table))
  (exwm-cm--create-tree)
  ;; Set up the root X window.
  (setq exwm-cm--depth
        (slot-value (car (slot-value (xcb:get-setup exwm-cm--conn) 'roots))
                    'root-depth))
  (with-slots (visual picture) (exwm-cm--xwin->attr exwm--root)
    (setf picture (xcb:generate-id exwm-cm--conn))
    (xcb:+request exwm-cm--conn
        (make-instance 'xcb:render:CreatePicture
                       :pid picture
                       :drawable exwm--root
                       :format (xcb:renderutil:find-visual-format
                                (xcb:renderutil:query-formats exwm-cm--conn)
                                visual)
                       :value-mask xcb:render:CP:SubwindowMode
                       :subwindowmode xcb:SubwindowMode:IncludeInferiors)))
  (xcb:flush exwm-cm--conn)
  ;; Paint once.
  (exwm-cm--paint t))

(defun exwm-cm--exit ()
  "Exit EXWM compositing manager."
  (when exwm-cm--conn
    (xcb:disconnect exwm-cm--conn)
    (clrhash exwm-cm--hash)
    (setq exwm-cm--hash nil
          exwm-cm--conn nil
          exwm-cm--buffer nil
          exwm-cm--clip-changed t
          exwm-cm--damages nil
          exwm-cm--expose-rectangles nil
          exwm-cm--background nil)))

(defun exwm-cm-enable ()
  "Enable compositing support for EXWM."
  (add-hook 'exwm-init-hook #'exwm-cm--init t)
  (add-hook 'exwm-exit-hook #'exwm-cm--exit t))

(defun exwm-cm-start ()
  "Start EXWM compositing manager."
  (interactive)
  (unless exwm-cm--conn
    (exwm-cm--init)))

(defun exwm-cm-stop ()
  "Stop EXWM compositing manager."
  (interactive)
  (exwm-cm--exit))

(defun exwm-cm-toggle ()
  "Toggle the running state of EXWM compositing manager."
  (interactive)
  (if exwm-cm--conn
      (exwm-cm-stop)
    (exwm-cm-start)))



(provide 'exwm-cm)

;;; exwm-cm.el ends here
