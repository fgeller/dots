;;; porg-mode.el --- Simplistic minor mode for presenting with org-mode.

;; Copyright (C) Humans.

;; Author: Felix Geller <fgeller@gmail.com>
;; URL: https://github.com/fgeller/porg.el

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; See README.md.

;;; Code:

(defun porg/slide-motion (direction)
  (widen)
  (outline-next-visible-heading direction)
  (org-narrow-to-subtree))

(defun porg/previous-slide ()
  (interactive)
  (porg/slide-motion -1))

(defun porg/next-slide ()
  (interactive)
  (porg/slide-motion 1))

(defun porg/quit ()
  (interactive)
  (porg-mode -1)
  (widen))

(defvar porg-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "]") 'porg/next-slide)
    (define-key map (kbd "[") 'porg/previous-slide)
    (define-key map (kbd "q") 'porg/quit)
    map)
  "Keymap for porg-mode.")

(define-minor-mode porg-mode
  "Simplistic minor mode for presentations in org-mode.

\\{porg-mode-map}"
  :init-value nil
  :lighter "porg"
  :keymap porg-mode-map

  (org-display-inline-images)
  (highlight-thing-mode -1)
  (org-narrow-to-subtree))
