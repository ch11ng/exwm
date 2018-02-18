;;; exwm-cm.el --- Compositing Manager for EXWM  -*- lexical-binding: t -*-

;; Copyright (C) 2016-2018 Free Software Foundation, Inc.

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

;; This module is obsolete since EXWM now supports third-party compositors.

;;; Code:

(make-obsolete-variable 'exwm-cm-opacity
                        "This variable should no longer be used." "26")

(defun exwm-cm-set-opacity (&rest _args)
  (declare (obsolete nil "26")))

(defun exwm-cm-enable ()
  (declare (obsolete nil "26")))

(defun exwm-cm-start ()
  (declare (obsolete nil "26")))

(defun exwm-cm-stop ()
  (declare (obsolete nil "26")))

(defun exwm-cm-toggle ()
  (declare (obsolete nil "26")))



(provide 'exwm-cm)

;;; exwm-cm.el ends here
