;;; spacious-padding.el --- Increase the padding/spacing of frames and windows -*- lexical-binding: t -*-

;; Copyright (C) 2023-2024  Free Software Foundation, Inc.

;; Author: Protesilaos Stavrou <info@protesilaos.com>
;; Maintainer: Protesilaos Stavrou <info@protesilaos.com>
;; URL: https://github.com/protesilaos/spacious-padding
;; Version: 0.5.0
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
;; modifying the user option `spacious-padding-widths'.  To have a
;; more subtle mode line, with nothing but an overline, configure the
;; user option `spacious-padding-subtle-mode-line'.
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
     :scroll-bar-width 8
     :fringe-width 8)
  "Set the pixel width of individual User Interface elements.
This is a plist of the form (:key1 value1 :key2 value2).  The
value is always a natural number.  Keys are keywords among the
following:

- `:internal-border-width' refers to the space between the
  boundaries of the Emacs frame and where the text contents
  start.

- `:right-divider-width' is the space between two side-by-side
  windows.  If the value is 1, the border is not hidden when
  `spacious-padding-mode' is enabled.

- `:fringe-width' applies to the fringes on either side of the
  window.  The more specific keys `:left-fringe-width' and
  `:right-fringe-wdith' can be used for finer control.  If those
  are not specified (or set to a nil value), they fall back to
  `:fringe-width'.  (The fringes are the window sides where line
  wrapping and other indicators are displayed).

- `:tab-width' concerns the padding around buttons of all tabbed
  interfaces (`tab-bar-mode', `tab-line-mode').

- `:tab-bar-width' concerns the padding around buttons of the
  `tab-bar-mode'.  If not specified (or set to nil) it uses the
  value of `:tab-width'.

- `:tab-line-width' concerns the padding around buttons of the
  tab-line.  If not specified (or set to nil) it uses the value
  of `:tab-width'.

- `:header-line-width', `mode-line-width', `scroll-bar-width'
  point to the header-line, mode-line, and scroll-bar,
  respectively.

For the technicalities, read Info node `(elisp) Frame Layout'.

When the value is nil, fall back to reasonable defaults."
  :type '(plist
          :key-type (choice (const :internal-border-width)
                            (const :right-divider-width)
                            (const :fringe-width)
                            (const :left-fringe-width)
                            (const :right-fringe-width)
                            (const :tab-width)
                            (const :tab-bar-width)
                            (const :tab-line-width)
                            (const :header-line-width)
                            (const :mode-line-width)
                            (const :scroll-bar-width))
          :value-type (choice natnum (const nil)))
  :package-version '(spacious-padding . "0.4.0")
  :group 'spacious-padding)

(defcustom spacious-padding-subtle-mode-line nil
  "Remove the background from the mode lines and add overlines.

Preserve whatever padding is specified in `spacious-padding-widths'.

The value is either a boolean type or a plist.  If it is non-nil,
use the foreground of the underlying mode line face to derive the
color of the overline.

If the non-nil value is a plist read the following keys to
determine the exact style of the overlines.

- `:mode-line-active' refers to the active/current mode line.

- `:mode-line-inactive' refers to the inactive/non-current mode
  lines.

Each key accepts either a face or a string representing a color
as its associated value:

- The face is an unquoted symbol, such as `success' or `shadow',
  whose `:foreground' attribute is queried to extract the desired
  color value.

- The color is a name among those listed in the output of the
  command `list-colors-display' or a hexadecimal RGB value, such
  as #123456.

If the key is missing or its value is not one of the above, fall
back to reading the foreground of the underlying mode line face
to determine the color of the overline.

Examples of valid configurations:

    ;; Use the foreground of the underlying face to determine the color of
    ;; the overline (e.g. the inactive mode line has gray text, so render
    ;; the overline in the same gray).
    (setq spacious-padding-subtle-mode-line t)

    ;; Use the foreground of the `error' face (typically a red hue) for
    ;; the active mode line's overline.  For the inactive mode line, fall
    ;; back to the foreground color of the underlying face (as in the case
    ;; of the t shown above).
    (setq spacious-padding-subtle-mode-line
          \\='(:mode-line-active error))

    ;; As above, but now use the foreground of the `shadow' face for the
    ;; inactive mode line.
    (setq spacious-padding-subtle-mode-line
          \\='(:mode-line-active error :mode-line-inactive shadow))

    ;; Use color values directly.
    (setq spacious-padding-subtle-mode-line
          \\='(:mode-line-active \"#0000ff\" :mode-line-inactive \"gray50\"))"
  :type '(choice boolean
                 (plist
                  :key-type (choice (const :mode-line-active)
                                    (const :mode-line-inactive))
                  :value-type (choice string face)))
  :package-version '(spacious-padding . "0.3.0")
  :group 'spacious-padding)

;; NOTE 2023-12-05: The `keycast-key' should preferably be
;; disambiguated into separate faces for all the places where keycast
;; can be displayed (mode line, header line, tab bar).  For now I am
;; treating it as a mode line face which means that the mode line
;; padding will be applied elsewhere if keycast is shown there.  Not a
;; huge problem, but I am aware of it.
(defvar spacious-padding--mode-line-faces
  '(mode-line mode-line-active mode-line-inactive mode-line-highlight keycast-key)
  "Mode line faces relevant to `spacious-padding-mode'.")

(defvar spacious-padding--header-line-faces
  '(header-line header-line-highlight)
  "Header line faces relevant to `spacious-padding-mode'.")

(defvar spacious-padding--tab-bar-faces
  '(tab-bar tab-bar-tab tab-bar-tab-inactive)
  "Tab faces relevant to `spacious-padding-mode'.")

(defvar spacious-padding--tab-line-faces
  '(tab-line tab-line-tab tab-line-tab-inactive)
  "Tab faces relevant to `spacious-padding-mode'.")

(defun spacious-padding--get-right-divider-width (&optional no-fallback)
  "Get the width of window divider.
With optional NO-FALLBACK return nil if there is no value.  Else return
a reasonable fallback value."
  (cond
   ((plist-get spacious-padding-widths :right-divider-width))
   (no-fallback nil)
   (t 30)))

(defun spacious-padding--get-box-width (key &optional no-fallback)
  "Get width for :box of face represented by KEY in `spacious-padding-widths'.
Return 4 if KEY does not have a value.  If optional NO-FALLBACK
is non-nil, do not return a fallback value: just nil."
  (cond
   ((plist-get spacious-padding-widths key))
   (no-fallback nil)
   (t 4)))

(defun spacious-padding--get-face-width (face)
  "Return width of FACE from `spacious-padding-widths'."
  (cond
   ((memq face spacious-padding--mode-line-faces)
    (spacious-padding--get-box-width :mode-line-width))
   ((memq face spacious-padding--header-line-faces)
    (spacious-padding--get-box-width :header-line-width))
   ((memq face spacious-padding--tab-bar-faces)
    (or (spacious-padding--get-box-width :tab-bar-width :fall-back-to-tab-width)
        (spacious-padding--get-box-width :tab-width)))
   ((memq face spacious-padding--tab-line-faces)
    (or (spacious-padding--get-box-width :tab-line-width :fall-back-to-tab-width)
        (spacious-padding--get-box-width :tab-width)))
   (t (error "`%s' is not relevant to `spacious-padding-mode'" face))))

(defun spacious-padding--get-face-overline-color (face fallback subtle-key)
  "Get overline foreground value for FACE with FALLBACK face if needed.
Use SUBTLE-KEY to determine the value based on
`spacious-padding-subtle-mode-line', falling back to FACE, then
FALLBACK."
  (let ((subtle-value (plist-get spacious-padding-subtle-mode-line subtle-key)))
    (cond
     ((stringp subtle-value) subtle-value)
     ((facep subtle-value) (face-foreground subtle-value nil face))
     (t (face-foreground face nil fallback)))))

(defun spacious-padding-set-face-box-padding (face fallback &optional subtle-key)
  "Return face attributes for FACE with FALLBACK face background.
With optional SUBTLE-KEY, read its value from the
`spacious-padding-subtle-mode-line' and apply it to FACE as an
overline."
  (when (facep face)
    (let* ((original-bg (face-background face nil fallback))
           (subtle-bg (face-background 'default))
           (subtlep (and subtle-key spacious-padding-subtle-mode-line))
           (bg (if subtlep subtle-bg original-bg))
           (face-width (spacious-padding--get-face-width face)))
      `(,@(when subtlep
            (list
             :background bg
             :overline (spacious-padding--get-face-overline-color face fallback subtle-key)))
        ,@(unless (eq face-width 0)
            (list
             :box
             `( :line-width ,face-width
                :color ,bg
                :style nil)))))))

(defun spacious-padding-set-window-divider (face color)
  "Set window divider FACE to COLOR its width is greater than 1."
  (list
   face
   `((t
      ,(when (> (spacious-padding--get-right-divider-width) 1)
         (list :background color :foreground color))))))

(define-obsolete-function-alias
  'spacious-padding-set-invisible-dividers
  'spacious-padding-set-faces
  "0.5.0")

;;;###autoload
(defun spacious-padding-set-faces (&rest _)
  "Make window dividers invisible and add padding.
Ignore any arguments.  This is useful to add the function to abnormal
hooks that pass one or more arguments to it, such as
`after-make-frame-functions'."
  (let ((bg-main (face-background 'default))
        (fg-main (face-foreground 'default)))
    (custom-set-faces
     `(fringe ((t :background ,bg-main)))
     `(line-number ((t :background ,bg-main)))
     `(header-line ((t ,@(spacious-padding-set-face-box-padding 'header-line 'default))))
     `(header-line-highlight ((t :box (:color ,fg-main))))
     `(keycast-key ((t ,@(spacious-padding-set-face-box-padding 'keycast-key 'default))))
     `(mode-line ((t ,@(spacious-padding-set-face-box-padding 'mode-line 'default :mode-line-active))))
     ;; We cannot use :inherit mode-line because it does not get our version of it...
     `(mode-line-active ((t ,@(spacious-padding-set-face-box-padding 'mode-line-active 'mode-line :mode-line-active))))
     `(mode-line-inactive ((t ,@(spacious-padding-set-face-box-padding 'mode-line-inactive 'mode-line :mode-line-inactive))))
     `(mode-line-highlight ((t :box (:color ,fg-main))))
     `(tab-bar-tab ((t ,@(spacious-padding-set-face-box-padding 'tab-bar-tab 'tab-bar))))
     `(tab-bar-tab-inactive ((t ,@(spacious-padding-set-face-box-padding 'tab-bar-tab-inactive 'tab-bar))))
     `(tab-line-tab ((t ,@(spacious-padding-set-face-box-padding 'tab-line-tab 'tab-line))))
     `(tab-line-tab-inactive ((t ,@(spacious-padding-set-face-box-padding 'tab-line-tab-inactive 'tab-line))))
     `(tab-line-tab-active ((t ,@(spacious-padding-set-face-box-padding 'tab-line-tab-active 'tab-line))))
     `(vertical-border ((t :background ,bg-main :foreground ,bg-main)))
     `(,@(spacious-padding-set-window-divider 'window-divider bg-main))
     `(,@(spacious-padding-set-window-divider 'window-divider-first-pixel bg-main))
     `(,@(spacious-padding-set-window-divider 'window-divider-last-pixel bg-main)))))

(defun spacious-padding-unset-invisible-dividers ()
  "Make window dividers for THEME invisible."
  (custom-set-faces
   '(fringe (( )))
   '(line-number (( )))
   '(header-line (( )))
   '(keycast-key (( )))
   '(header-line-highlight (( )))
   '(mode-line (( )))
   '(mode-line-active (( )))
   '(mode-line-inactive (( )))
   '(mode-line-highlight (( )))
   '(tab-bar-tab (( )))
   '(tab-bar-tab-inactive (( )))
   '(tab-line-tab (( )))
   '(tab-line-tab-inactive (( )))
   `(vertical-border (( )))
   '(window-divider (( )))
   '(window-divider-first-pixel (( )))
   '(window-divider-last-pixel (( )))))

(defvar spacious-padding--internal-border-width nil
  "Default value of frame parameter `internal-border-width'.")

(defvar spacious-padding--right-divider-width nil
  "Default value of frame parameter `right-divider-width'.")

(defvar spacious-padding--fringe-width nil
  "Default value of frame parameters `left-fringe' and `right-fringe'.")

(defvar spacious-padding--left-fringe-width nil
  "Default value of frame parameter `left-fringe'.")

(defvar spacious-padding--right-fringe-width nil
  "Default value of frame parameter `right-fringe'.")

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
  (unless spacious-padding--fringe-width
    (setq spacious-padding--fringe-width 8)) ; 8 is the default per `fringe-mode'
  (unless spacious-padding--left-fringe-width
    (setq spacious-padding--left-fringe-width
          (frame-parameter nil 'left-fringe-width)))
  (unless spacious-padding--right-fringe-width
    (setq spacious-padding--right-fringe-width
          (frame-parameter nil 'right-fringe-width)))
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

(spacious-padding--define-get-frame-param "internal-border-width" 15)
(spacious-padding--define-get-frame-param "right-divider-width" 30)
(spacious-padding--define-get-frame-param "fringe-width" 8)
(spacious-padding--define-get-frame-param "left-fringe-width" nil)
(spacious-padding--define-get-frame-param "right-fringe-width" nil)
(spacious-padding--define-get-frame-param "scroll-bar-width" 8)

(defun spacious-padding-modify-frame-parameters (&optional reset)
  "Modify all frame parameters to specify spacing.
With optional RESET argument as non-nil, restore the default
parameter values."
  (modify-all-frames-parameters
   `((internal-border-width . ,(spacious-padding--get-internal-border-width reset))
     (right-divider-width . ,(spacious-padding--get-right-divider-width reset))
     (left-fringe . ,(or (spacious-padding--get-left-fringe-width reset)
                         (spacious-padding--get-fringe-width reset)))
     (right-fringe . ,(or (spacious-padding--get-right-fringe-width reset)
                          (spacious-padding--get-fringe-width reset)))
     (scroll-bar-width  . ,(spacious-padding--get-scroll-bar-width reset)))))

;;;###autoload
(defun spacious-padding-set-parameters-of-frame (frame)
  "Set the layout parameters of FRAME and update the faces."
  (with-selected-frame frame
    (spacious-padding-modify-frame-parameters)
    (spacious-padding-set-faces)))

(defun spacious-padding--enable-mode ()
  "Enable `spacious-padding-mode'."
  (spacious-padding--store-default-parameters)
  (spacious-padding-modify-frame-parameters)
  (spacious-padding-set-faces)
  (add-hook 'enable-theme-functions #'spacious-padding-set-faces)
  (add-hook 'after-make-frame-functions #'spacious-padding-set-parameters-of-frame))

(defun spacious-padding--disable-mode ()
  "Disable `spacious-padding-mode'."
  (spacious-padding-modify-frame-parameters :reset)
  (spacious-padding-unset-invisible-dividers)
  (remove-hook 'enable-theme-functions #'spacious-padding-set-faces)
  (remove-hook 'after-make-frame-functions #'spacious-padding-set-parameters-of-frame))

;;;###autoload
(define-minor-mode spacious-padding-mode
  "Increase the padding/spacing of frames and windows."
  :global t
  (if spacious-padding-mode
      (spacious-padding--enable-mode)
    (spacious-padding--disable-mode)))

(provide 'spacious-padding)
;;; spacious-padding.el ends here
