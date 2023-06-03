;;; spacious-padding.el --- Increase the padding/spacing of frames and windows -*- lexical-binding: t -*-

;; Copyright (C) 2023  Free Software Foundation, Inc.

;; Author: Protesilaos Stavrou <info@protesilaos.com>
;; Maintainer: Protesilaos Stavrou General Issues <~protesilaos/general-issues@lists.sr.ht>
;; URL: https://git.sr.ht/~protesilaos/spacious-padding
;; Mailing-List: https://lists.sr.ht/~protesilaos/general-issues
;; Version: 0.0.0
;; Package-Requires: ((emacs "28.1"))
;; Keywords: convenience, focus, writing, presentation

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; This package provides a global minor mode to increase the
;; spacing/padding of Emacs windows and frames.  The idea is to make
;; editing and reading feel more comfortable.  Enable the
;; `spacious-padding-mode'.  Adjust the exact spacing values by
;; modifying the user option `spacious-padding-widths'.
;;
;; While obvious to everyone, here are the backronyms for this
;; package: Space Perception Adjusted Consistently Impacts Overall
;; Usability State ... padding; Spacious ... Precise Adjustments to
;; Desktop Divider Internals Neatly Generated.

;;; Code:

(defgroup spacious-padding ()
  "Increase the padding/spacing of frames and windows."
  :group 'frames)

(defcustom spacious-padding-widths
  '( :internal-border-width 15
     :right-divider-width 30
     :scroll-bar-width 8)
  "Number of pixels for frame and window divider border width."
  :type '(plist
          :key-type (choice (const :internal-border-width)
                            (const :right-divider-width)
                            (const :scroll-bar-width))
          :value-type natnum)
  :group 'spacious-padding)

(defun spacious-padding-set-invisible-dividers (_theme)
  "Make window dividers for THEME invisible."
  (let ((bg (face-background 'default)))
    (custom-set-faces
     `(fringe ((t :background ,bg :foreground ,bg)))
     `(window-divider ((t :background ,bg :foreground ,bg)))
     `(window-divider-first-pixel ((t :background ,bg :foreground ,bg)))
     `(window-divider-last-pixel ((t :background ,bg :foreground ,bg))))))

(defun spacious-padding-unset-invisible-dividers ()
  "Make window dividers for THEME invisible."
  (custom-set-faces
   '(fringe (( )))
   '(window-divider (( )))
   '(window-divider-first-pixel (( )))
   '(window-divider-last-pixel (( )))))

(defvar spacious-padding--internal-border-width nil
  "Default value of frame parameter `internal-border-width'.")

(defvar spacious-padding--right-divider-width nil
  "Default value of frame parameter `right-divider-width'.")

(defvar spacious-padding--scroll-bar-width nil
  "Default value of frame parameter `scroll-bar-width'.")

(defun spacious-padding--store-default-parameters ()
  "Store default frame parameter values."
  (unless spacious-padding--internal-border-width
    (setq spacious-padding--internal-border-width
          (frame-parameter nil 'internal-border-width)))
  (unless spacious-padding--right-divider-width
    (setq spacious-padding--right-divider-width
          (frame-parameter nil 'right-divider-width)))
  (unless spacious-padding--scroll-bar-width
    (setq spacious-padding--scroll-bar-width
          (frame-parameter nil 'scroll-bar-width))))

(defun spacious-padding--get-internal-border-width (&optional reset)
  "Return value of frame parameter `internal-border-width'.
With optional RESET argument as non-nil, restore the default
parameter value."
  (if reset
      (or spacious-padding--internal-border-width 0)
    (plist-get spacious-padding-widths :internal-border-width)))

(defun spacious-padding--get-right-divider-width (&optional reset)
  "Return value of frame parameter `right-divider-width'.
With optional RESET argument as non-nil, restore the default
parameter value."
  (if reset
      (or spacious-padding--right-divider-width 1)
    (plist-get spacious-padding-widths :right-divider-width)))

(defun spacious-padding--get-scroll-bar-width (&optional reset)
  "Return value of frame parameter `scroll-bar-width'.
With optional RESET argument as non-nil, restore the default
parameter value."
  (if reset
      (or spacious-padding--scroll-bar-width 16)
    (plist-get spacious-padding-widths :scroll-bar-width)))

(defun spacious-padding-modify-frame-parameters (reset)
  "Modify all frame parameters to specify spacing.
With optional RESET argument as non-nil, restore the default
parameter values."
  (modify-all-frames-parameters
   `((internal-border-width . ,(spacious-padding--get-internal-border-width reset))
     (right-divider-width . ,(spacious-padding--get-right-divider-width reset))
     (scroll-bar-width  . ,(spacious-padding--get-scroll-bar-width reset)))))

;;;###autoload
(define-minor-mode spacious-padding-mode
  "Increase the padding/spacing of frames and windows."
  :global t
  (if spacious-padding-mode
      (spacious-padding--enable-mode)
    (spacious-padding--disable-mode)))

(defun spacious-padding--enable-mode ()
  "Enable `spacious-padding-mode'."
  (spacious-padding--store-default-parameters)
  (spacious-padding-modify-frame-parameters nil)
  (spacious-padding-set-invisible-dividers nil)
  (add-hook 'enable-theme-functions #'spacious-padding-set-invisible-dividers))

(defun spacious-padding--disable-mode ()
  "Disable `spacious-padding-mode'."
  (spacious-padding-modify-frame-parameters :reset)
  (spacious-padding-unset-invisible-dividers)
  (remove-hook 'enable-theme-functions #'spacious-padding-set-invisible-dividers))

(provide 'spacious-padding)
;;; spacious-padding.el ends here
