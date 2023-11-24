;;; spacious-padding.el --- Increase the padding/spacing of frames and windows -*- lexical-binding: t -*-

;; Copyright (C) 2023  Free Software Foundation, Inc.

;; Author: Protesilaos Stavrou <info@protesilaos.com>
;; Maintainer: Protesilaos Stavrou General Issues <~protesilaos/general-issues@lists.sr.ht>
;; URL: https://git.sr.ht/~protesilaos/spacious-padding
;; Mailing-List: https://lists.sr.ht/~protesilaos/general-issues
;; Version: 0.2.0
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
;; Inspiration for this package comes from Nicolas Rougier's
;; impressive designs[1] and Daniel Mendler's `org-modern`
;; package[2]
;;
;; 1. <https://github.com/rougier>
;; 2. <https://github.com/minad/org-modern>
;;
;; While obvious to everyone, here are the backronyms for this
;; package: Space Perception Adjusted Consistently Impacts Overall
;; Usability State ... padding; Spacious ... Precise Adjustments to
;; Desktop Divider Internals Neatly Generated.

;;; Code:

(defgroup spacious-padding ()
  "Increase the padding/spacing of frames and windows."
  :group 'faces
  :group 'frames)

(defcustom spacious-padding-widths
  '( :internal-border-width 15
     :header-line-width 4
     :mode-line-width 6
     :tab-width 4
     :right-divider-width 30
     :scroll-bar-width 8)
  "Set the pixel width of individual User Interface elements.
This is a plist of the form (:key1 value1 :key2 value2).  The
value is always a natural number.  Keys are keywords among the
following:

- `:internal-border-width' refers to the space between the
  boundaries of the Emacs frame and where the text contents
  start.

- `:right-divider-width' is the space between two side-by-side
  windows.

- `:tab-width' concerns the padding around tab buttons.  For the
  time being, `tab-bar-mode' is the only one affected.

- `:header-line-width', `mode-line-width', `scroll-bar-width'
  point to the header-line, mode-line, and scroll-bar,
  respectively."
  :type '(plist
          :key-type (choice (const :internal-border-width)
                            (const :right-divider-width)
                            (const :tab-width)
                            (const :header-line-width)
                            (const :mode-line-width)
                            (const :scroll-bar-width))
          :value-type natnum)
  :package-version '(spacious-padding . "0.2.0")
  :group 'spacious-padding)

(defvar spacious-padding--mode-line-faces
  '(mode-line mode-line-active mode-line-inactive mode-line-highlight)
  "Mode line faces relevant to `spacious-padding-mode'.")

(defvar spacious-padding--header-line-faces
  '(header-line header-line-highlight)
  "Header line faces relevant to `spacious-padding-mode'.")

;; TODO 2023-11-16: Cover `tab-line-mode'.
(defvar spacious-padding--tab-faces
  '(tab-bar tab-bar-tab tab-bar-tab-inactive)
  "Tab faces relevant to `spacious-padding-mode'.")

(defun spacious-padding--get-box-width (key)
  "Get width for :box of face represented by KEY in `spacious-padding-widths'.
Return 4 if KEY does not have a value."
  (or (plist-get spacious-padding-widths key) 4))

(defun spacious-padding--get-face-width (face)
  "Return width of FACE from `spacious-padding-widths'."
  (cond
   ((memq face spacious-padding--mode-line-faces)
    (spacious-padding--get-box-width :mode-line-width))
   ((memq face spacious-padding--header-line-faces)
    (spacious-padding--get-box-width :header-line-width))
   ((memq face spacious-padding--tab-faces)
    (spacious-padding--get-box-width :tab-width))
   (t (error "`%s' is not relevant to `spacious-padding-mode'" face))))

(defun spacious-padding-set-face-box-padding (face fallback)
  "Return appropriate face attributes for FACE with FALLBACK face background."
  (list :box
        (list
         :line-width (spacious-padding--get-face-width face)
         :color (face-background face nil fallback)
         :style nil)))

(defun spacious-padding-set-invisible-dividers (_theme)
  "Make window dividers for THEME invisible."
  (let ((bg-main (face-background 'default))
        (fg-main (face-foreground 'default)))
    (custom-set-faces
     `(fringe ((t :background ,bg-main)))
     `(line-number ((t :background ,bg-main)))
     `(header-line ((t ,@(spacious-padding-set-face-box-padding 'header-line 'default))))
     `(header-line-highlight ((t :box (:color ,fg-main))))
     `(mode-line ((t ,@(spacious-padding-set-face-box-padding 'mode-line 'default))))
     ;; We cannot use :inherit mode-line because it does not get our version of it...
     `(mode-line-active ((t ,@(spacious-padding-set-face-box-padding 'mode-line-active 'mode-line))))
     `(mode-line-inactive ((t ,@(spacious-padding-set-face-box-padding 'mode-line-inactive 'mode-line))))
     `(mode-line-highlight ((t :box (:color ,fg-main))))
     `(tab-bar-tab ((t ,@(spacious-padding-set-face-box-padding 'tab-bar-tab 'tab-bar))))
     `(tab-bar-tab-inactive ((t ,@(spacious-padding-set-face-box-padding 'tab-bar-tab-inactive 'tab-bar))))
     `(window-divider ((t :background ,bg-main :foreground ,bg-main)))
     `(window-divider-first-pixel ((t :background ,bg-main :foreground ,bg-main)))
     `(window-divider-last-pixel ((t :background ,bg-main :foreground ,bg-main))))))

(defun spacious-padding-unset-invisible-dividers ()
  "Make window dividers for THEME invisible."
  (custom-set-faces
   '(fringe (( )))
   '(line-number (( )))
   '(header-line (( )))
   '(header-line-highlight (( )))
   '(mode-line (( )))
   '(mode-line-active (( )))
   '(mode-line-inactive (( )))
   '(mode-line-highlight (( )))
   '(tab-bar-tab (( )))
   '(tab-bar-tab-inactive (( )))
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

(defmacro spacious-padding--define-get-frame-param (parameter fallback)
  "Define function to return frame PARAMETER or reset it with FALLBACK value."
  `(defun ,(intern (format "spacious-padding--get-%s" parameter)) (&optional reset)
     ,(format "Return value of frame parameter `%s'.
With optional RESET argument as non-nil, restore the default
parameter value."
             parameter)
     (or
      (if reset
          ,(intern (format "spacious-padding--%s" parameter))
        (plist-get spacious-padding-widths ,(intern (concat ":" parameter))))
      ,fallback)))

(spacious-padding--define-get-frame-param "internal-border-width" 0)
(spacious-padding--define-get-frame-param "right-divider-width" 1)
(spacious-padding--define-get-frame-param "scroll-bar-width" 16)

(defun spacious-padding-modify-frame-parameters (reset)
  "Modify all frame parameters to specify spacing.
With optional RESET argument as non-nil, restore the default
parameter values."
  (modify-all-frames-parameters
   `((internal-border-width . ,(spacious-padding--get-internal-border-width reset))
     (right-divider-width . ,(spacious-padding--get-right-divider-width reset))
     (scroll-bar-width  . ,(spacious-padding--get-scroll-bar-width reset)))))

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

;;;###autoload
(define-minor-mode spacious-padding-mode
  "Increase the padding/spacing of frames and windows."
  :global t
  (if spacious-padding-mode
      (spacious-padding--enable-mode)
    (spacious-padding--disable-mode)))

(provide 'spacious-padding)
;;; spacious-padding.el ends here
