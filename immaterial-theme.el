;;; immaterial-theme.el --- A family of themes loosely based on material colors -*- lexical-binding: t -*-

;; Copyright (C) 2019-2025 Peter Gardfjäll

;; Author: Peter Gardfjäll
;; Keywords: themes
;; URL: https://github.com/petergardfjall/emacs-immaterial-theme
;; Version: 0.10.0
;; Package-Requires: ((emacs "29")(modus-themes "5.0.0"))
;; Keywords: faces, theme

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;; Commentary:

;; To use the theme, put any of the following two lines in your Emacs
;; configuration file:
;;
;;   (load-theme 'immaterial-dark t)    ;; dark variant
;;   (load-theme 'immaterial-light t)   ;; light variant
;;
;; Requirements: Emacs 29.
;;

;;; Code:

(require 'modus-themes)

(defgroup immaterial-theme ()
  "Immaterial themes.
The `immaterial-theme' themes are built on top of the `modus-themes'."
  :group 'faces
  :group 'modus-themes
  :link '(info-link "(immaterial-theme) Top")
  :link '(info-link "(modus-themes) Top")
  :prefix "immaterial-theme-"
  :tag "Immaterial Themes")


;; Common semantic color mappings for all immaterial theme variants.
;;
;; These mappings take the named color values that need to be defined by every
;; immaterial theme variant and applies them to the semantic colors used by the
;; `modus-themes'.
(defconst immaterial-theme-common-palette
  '(
    ;; Graphs

    (bg-graph-red-0     bg-red-intense)
    (bg-graph-red-1     bg-red-subtle)
    (bg-graph-green-0   bg-green-intense)
    (bg-graph-green-1   bg-green-subtle)
    (bg-graph-yellow-0  bg-yellow-intense)
    (bg-graph-yellow-1  bg-yellow-subtle)
    (bg-graph-blue-0    bg-blue-intense)
    (bg-graph-blue-1    bg-blue-subtle)
    (bg-graph-magenta-0 bg-magenta-intense)
    (bg-graph-magenta-1 bg-magenta-subtle)
    (bg-graph-cyan-0    bg-cyan-intense)
    (bg-graph-cyan-1    bg-cyan-subtle)

    ;; Special purpose

    (bg-completion       bg-selected) ;; Used for `modus-themes-completion-selected'.
    (bg-hover            bg-active)
    (bg-hover-secondary  bg-active)
    (bg-hl-line          bg-active)
    (bg-region           bg-active)
    (fg-region           unspecified)

    (modeline-err     red)            ;; Same as err.
    (modeline-warning yellow-intense) ;; Same as warning
    (modeline-info    accent-1)       ;; Same as info

    (bg-tab-bar      bg-mode-line-inactive)
    (bg-tab-current  bg-mode-line-active)
    (bg-tab-other    bg-inactive)

    ;; Paren match

    (bg-paren-match       unspecified)
    (bg-paren-expression  bg-selected) ;; When `show-paren-style' is set to 'expression.

    ;; General mappings

    (cursor warning)
    (keybind warning)
    (name accent-0)
    (identifier accent-0)

    (err red)
    (warning yellow-intense)
    (info accent-1)

    (underline-err     err)
    (underline-warning warning)
    (underline-note    info)

    (bg-prominent-err bg-red-intense)
    (fg-prominent-err unspecified)
    (bg-prominent-warning unspecified)
    (fg-prominent-warning warning)
    (bg-prominent-note unspecified)
    (fg-prominent-note info)

    ;; E.g. when param at point in a function is highlighted in echo area.
    (bg-active-argument unspecified)
    (fg-active-argument fg-prompt)
    (bg-active-value unspecified)
    (fg-active-value cyan-cooler)

    ;; Code mappings

    (builtin accent-0)
    (comment fg-dim)
    (constant accent-2)
    (docstring fg-dim)
    (fnname unspecified)
    (fnname-call unspecified)
    (keyword accent-0)
    (preprocessor red-cooler)
    (property unspecified)
    (rx-backslash accent-3)
    (rx-construct green-cooler)
    (string accent-2)
    (type unspecified)
    (variable accent-1)
    (variable-use unspecified)

    ;; Accent mappings

    (accent-0 magenta)
    (accent-1 green)
    (accent-2 blue)
    (accent-3 magenta-intense)

    ;; Completion mappings

    (fg-completion-match-0 warning)
    (fg-completion-match-1 warning)
    (fg-completion-match-2 warning)
    (fg-completion-match-3 warning)

    ;; Date mappings

    (date-common accent-1)
    (date-deadline err)
    (date-deadline-subtle warning)
    (date-event fg-alt)
    (date-holiday fg-dim)
    (date-holiday-other fg-dim)
    (date-range accent-1)
    (date-scheduled fg-main)
    (date-scheduled-subtle fg-main)
    (date-weekday fg-alt)
    (date-weekend fg-alt)

    ;; Link mappings

    (fg-link accent-2) ;; A link like a URL.
    (bg-link unspecified)
    (underline-link unspecified) ;; Underline link color.
    (fg-link-symbolic fg-link)
    (underline-link-symbolic bg-link)
    (fg-link-visited fg-link)
    (underline-link-visited underline-link)

    ;; Mail mappings

    (mail-cite-0 fg-dim)
    (mail-cite-1 fg-dim)
    (mail-cite-2 fg-dim)
    (mail-cite-3 fg-dim)
    (mail-part accent-1)
    (mail-recipient accent-0)
    (mail-subject accent-0)
    (mail-other accent-0)

    ;; Mark mappings

    (bg-mark-delete bg-removed) ;; For example 'd'eleting a `dired' file.
    (fg-mark-delete unspecified)
    (bg-mark-select bg-added) ;; For example 'm'arking a `dired' file.
    (fg-mark-select unspecified)
    (bg-mark-other bg-changed)
    (fg-mark-other unspecified)

    ;; Prompt mappings

    (fg-prompt warning)

    ;; Prose mappings

    (fg-prose-code accent-1)
    (fg-prose-macro accent-1)
    (fg-prose-verbatim accent-1)
    (prose-done fg-dim)
    (prose-tag accent-1) ;; org task priority.
    (prose-todo accent-0)
    (prose-metadata fg-dim)
    (prose-metadata-value accent-1)
    (prose-table accent-0)
    (prose-table-formula accent-1)

    ;; Rainbow mappings

    (rainbow-0 fg-main)
    (rainbow-1 magenta-intense)
    (rainbow-2 cyan-intense)
    (rainbow-3 red-warmer)
    (rainbow-4 yellow-intense)
    (rainbow-5 magenta-cooler)
    (rainbow-6 green-intense)
    (rainbow-7 blue-warmer)
    (rainbow-8 magenta-warmer)

    ;; Search mappings

    (bg-search-current bg-green-intense) ;; Current item in `isearch'.
    (bg-search-lazy    bg-green-intense) ;; Non-current items in an `isearch'.
    (bg-search-static  bg-active) ;; Used for `match' face and similar.
    (bg-search-replace bg-red-intense)    ;; Highlight to-be-replaced item.

    ;; Used for capture groups in `isearch-forward-regexp'.
    (bg-search-rx-group-0 bg-green-subtle)
    (bg-search-rx-group-1 bg-yellow-subtle)
    (bg-search-rx-group-2 bg-red-subtle)
    (bg-search-rx-group-3 unspecified)

    ;; Heading mappings

    (fg-heading-0 fg-alt)
    (fg-heading-1 fg-alt)
    (fg-heading-2 fg-alt)
    (fg-heading-3 fg-alt)
    (fg-heading-4 fg-alt)
    (fg-heading-5 fg-alt)
    (fg-heading-6 fg-alt)
    (fg-heading-7 fg-alt)
    (fg-heading-8 fg-alt)

    ;; Diff
    (bg-diff-context    bg-main) ;; The surrounding context lines in diffs.
    )
  "Semantic color mappings applicable for all immaterial theme variants.")

;; Defines semantic color mappings common to all immaterial variants.
;; Based on `modus-themes-common-palette-mappings'.
(defconst immaterial-theme-common-palette-mappings
  (append
   ;; Overrides to 'modus-themes-common-palette-mappings' go into this list.
   '((fg-paren-match warning))
   modus-themes-common-palette-mappings)
  "Common palette mappings for the `immaterial-theme' themes.")


;; Face definitions to override the ones in `modus-themes-faces'.
(defconst immaterial-theme-custom-faces
  '(
    '(button ((t :underline nil)))
    `(dape-breakpoint-face ((,c (:inherit warning) )))
    `(dape-exception-description-face ((,c (:inherit err :italic t) )))
    `(dape-repl-error-face ((,c (:inherit err :italic t) )))
    ;; Add code colorization to pre blocks in markdown.
    `(markdown-pre-face ((t (:foreground ,fg-prose-code))))
    ;; Color of icons shown on minibuffer completions.
    `(nerd-icons-completion-dir-face ((,c :foreground ,fg-main)))
    ;; Make visited hunk in magit stand out more.
    `(magit-diff-hunk-heading ((t :background ,bg-inactive :foreground ,fg-dim))) ;; Not visited hunk.
    `(magit-diff-hunk-heading-highlight ((,c :weight bold :foreground ,accent-0))) ;; Visited hunk.
    `(magit-diff-added ((,c :background ,bg-added-faint :foreground ,fg-dim))) ;; Not visited hunk.
    `(magit-diff-added-highlight ((,c :background ,bg-added :foreground ,fg-added))) ;; Visited hunk.
    `(magit-diff-removed ((,c :background ,bg-removed-faint :foreground ,fg-dim))) ;; Not visited hunk.
    `(magit-diff-removed-highlight ((,c :background ,bg-removed :foreground ,fg-removed))) ;; Visited hunk.
    ;; Date selection when selecing a date in `org-timestamp'.
    `(org-date-selected ((,c (:foreground ,fg-main :background ,bg-selected :box (:line-width -1 :color ,bg-added-refine)))))
    ;; Projtree.
    `(projtree-dir ((t :inherit dired-directory)))
    `(projtree-git-modified ((t :background ,bg-changed :box (:line-width 1 :color ,bg-changed-fringe :style nil))))
    `(projtree-git-added ((t :background ,bg-added :box (:line-width 1 :color ,bg-added-fringe :style nil))))
    `(projtree-git-conflict ((t :background ,bg-removed :box (:line-width 1 :color ,bg-removed-fringe :style nil) :italic t)))
    `(projtree-highlight ((t :inherit region)))
    ;; Highlight to use for value at point when a code template is inserted.
    `(yas-field-highlight-face ((,c :background ,bg-region)))
    )
  "Face definitions to override the ones in `modus-themes-faces'.")


(defun immaterial-color (color shade)
  "Get a material COLOR of a given SHADE as a hex color.
For example COLOR can be 'red and shade can be 400 for a value of #EF5350."
  (unless (and (symbolp color))
    (error "COLOR must be given as symbol"))
  (unless (or (stringp shade))
    (error "SHADE must be given as string"))
  (cdr (assoc shade (alist-get color immaterial-colors))))


(defconst immaterial-colors
  '((red .         (("50"   . "#FFEBEE") ("100"  . "#FFCDD2") ("200"  . "#EF9A9A")
                    ("300"  . "#E57373") ("400"  . "#EF5350") ("500"  . "#F44336")
                    ("600"  . "#E53935") ("700"  . "#D32F2F") ("800"  . "#C62828")
                    ("900"  . "#B71C1C") ("A100" . "#FF8A80") ("A200" . "#FF5252")
                    ("A400" . "#FF1744") ("A700" . "#D50000")))
    (pink .        (("50"   . "#FCE4EC") ("100"  . "#F8BBD0") ("200"  . "#F48FB1")
                    ("300"  . "#F06292") ("400"  . "#EC407A") ("500"  . "#E91E63")
                    ("600"  . "#D81B60") ("700"  . "#C2185B") ("800"  . "#AD1457")
                    ("900"  . "#880E4F") ("A100" . "#FF80AB") ("A200" . "#FF4081")
                    ("A400" . "#F50057") ("A700" . "#C51162")))
    (purple .      (("50"   . "#F3E5F5") ("100"  . "#E1BEE7") ("200"  . "#CE93D8")
                    ("300"  . "#BA68C8") ("400"  . "#AB47BC") ("500"  . "#9C27B0")
                    ("600"  . "#8E24AA") ("700"  . "#7B1FA2") ("800"  . "#6A1B9A")
                    ("900"  . "#4A148C") ("A100" . "#EA80FC") ("A200" . "#E040FB")
                    ("A400" . "#D500F9") ("A700" . "#AA00FF")))
    (deep-purple . (("50"   . "#EDE7F6") ("100"  . "#D1C4E9") ("200"  . "#B39DDB")
                    ("300"  . "#9575CD") ("400"  . "#7E57C2") ("500"  . "#673AB7")
                    ("600"  . "#5E35B1") ("700"  . "#512DA8") ("800"  . "#4527A0")
                    ("900"  . "#311B92") ("A100" . "#B388FF") ("A200" . "#7C4DFF")
                    ("A400" . "#651FFF") ("A700" . "#6200EA")))
    (indigo .      (("50"   . "#E8EAF6") ("100"  . "#C5CAE9") ("200"  . "#9FA8DA")
                    ("300"  . "#7986CB") ("400"  . "#5C6BC0") ("500"  . "#3F51B5")
                    ("600"  . "#3949AB") ("700"  . "#303F9F") ("800"  . "#283593")
                    ("900"  . "#1A237E") ("A100" . "#8C9EFF") ("A200" . "#536DFE")
                    ("A400" . "#3D5AFE") ("A700" . "#304FFE")))
    (blue .        (("50"  . "#E3F2FD")  ("100"  . "#BBDEFB") ("200"  . "#90CAF9")
                    ("300"  . "#64B5F6") ("400"  . "#42A5F5") ("500"  . "#2196F3")
                    ("600"  . "#1E88E5") ("700"  . "#1976D2") ("800"  . "#1565C0")
                    ("900"  . "#0D47A1") ("A100" . "#82B1FF") ("A200" . "#448AFF")
                    ("A400" . "#2979FF") ("A700" . "#2962FF")))
    (light-blue .  (("50"   . "#E1F5FE") ("100"  . "#B3E5FC") ("200"  . "#81D4FA")
                    ("300"  . "#4FC3F7") ("400"  . "#29B6F6") ("500"  . "#03A9F4")
                    ("600"  . "#039BE5") ("700"  . "#0288D1") ("800"  . "#0277BD")
                    ("900"  . "#01579B") ("A100" . "#80D8FF") ("A200" . "#40C4FF")
                    ("A400" . "#00B0FF") ("A700" . "#0091EA")))
    (cyan .        (("50"   . "#E0F7FA") ("100"  . "#B2EBF2") ("200"  . "#80DEEA")
                    ("300"  . "#4DD0E1") ("400"  . "#26C6DA") ("500"  . "#00BCD4")
                    ("600"  . "#00ACC1") ("700"  . "#0097A7") ("800"  . "#00838F")
                    ("900"  . "#006064") ("A100" . "#84FFFF") ("A200" . "#18FFFF")
                    ("A400" . "#00E5FF") ("A700" . "#00B8D4")))
    (teal .        (("50"   . "#E0F2F1") ("100"  . "#B2DFDB") ("200"  . "#80CBC4")
                    ("300"  . "#4DB6AC") ("400"  . "#26A69A") ("500"  . "#009688")
                    ("600"  . "#00897B") ("700"  . "#00796B") ("800"  . "#00695C")
                    ("900"  . "#004D40") ("A100" . "#A7FFEB") ("A200" . "#64FFDA")
                    ("A400" . "#1DE9B6") ("A700" . "#00BFA5")))
    (green .       (("50"   . "#E8F5E9") ("100" . "#C8E6C9") ("200"  . "#A5D6A7")
                    ("300"  . "#81C784") ("400"  . "#66BB6A") ("500"  . "#4CAF50")
                    ("600"  . "#43A047") ("700"  . "#388E3C") ("800"  . "#2E7D32")
                    ("900"  . "#1B5E20") ("A100" . "#B9F6CA") ("A200" . "#69F0AE")
                    ("A400" . "#00E676") ("A700" . "#00C853")))
    (light-green . (("50"   . "#F1F8E9") ("100"  . "#DCEDC8") ("200"  . "#C5E1A5")
                    ("300"  . "#AED581") ("400"  . "#9CCC65") ("500"  . "#8BC34A")
                    ("600"  . "#7CB342") ("700"  . "#689F38") ("800"  . "#558B2F")
                    ("900"  . "#33691E") ("A100" . "#CCFF90") ("A200" . "#B2FF59")
                    ("A400" . "#76FF03") ("A700" . "#64DD17")))
    (lime .        (("50"   . "#F9FBE7") ("100"  . "#F0F4C3") ("200"  . "#E6EE9C")
                    ("300"  . "#DCE775") ("400"  . "#D4E157") ("500"  . "#CDDC39")
                    ("600"  . "#C0CA33") ("700"  . "#AFB42B") ("800"  . "#9E9D24")
                    ("900"  . "#827717") ("A100" . "#F4FF81") ("A200" . "#EEFF41")
                    ("A400" . "#C6FF00") ("A700" . "#AEEA00")))
    (yellow .      (("50"   . "#FFFDE7") ("100"  . "#FFF9C4") ("200"  . "#FFF59D")
                    ("300"  . "#FFF176") ("400"  . "#FFEE58") ("500"  . "#FFEB3B")
                    ("600"  . "#FDD835") ("700"  . "#FBC02D") ("800"  . "#F9A825")
                    ("900"  . "#F57F17") ("A100" . "#FFFF8D") ("A200" . "#FFFF00")
                    ("A400" . "#FFEA00") ("A700" . "#FFD600")))
    (amber .       (("50"   . "#FFF8E1") ("100"  . "#FFECB3") ("200"  . "#FFE082")
                    ("300"  . "#FFD54F") ("400"  . "#FFCA28") ("500"  . "#FFC107")
                    ("600"  . "#FFB300") ("700"  . "#FFA000") ("800"  . "#FF8F00")
                    ("900"  . "#FF6F00") ("A100" . "#FFE57F") ("A200" . "#FFD740")
                    ("A400" . "#FFC400") ("A700" . "#FFAB00")))
    (orange .      (("50"   . "#FFF3E0") ("100"  . "#FFE0B2") ("200"  . "#FFCC80")
                    ("300"  . "#FFB347") ("400"  . "#FFA726") ("500"  . "#FF9800")
                    ("600"  . "#FB8C00") ("700"  . "#F57C00") ("800"  . "#EF6C00")
                    ("900"  . "#E65100") ("A100" . "#FFD180") ("A200" . "#FFAB40")
                    ("A400" . "#FF9100") ("A700" . "#FF6D00")))
    (deep-orange . (("50"   . "#FBE9E7") ("100"  . "#FFCCBC") ("200"  . "#FFAB91")
                    ("300"  . "#FF8A65") ("400"  . "#FF7043") ("500"  . "#FF5722")
                    ("600"  . "#F4511E") ("700"  . "#E64A19") ("800"  . "#D84315")
                    ("900"  . "#BF360C") ("A100" . "#FF9E80") ("A200" . "#FF6E40")
                    ("A400" . "#FF3D00") ("A700" . "#DD2C00")))))


(defun immaterial-color-lighten (hex-color percent)
  "Determines a brighter/darker shade of a hex color.
For a HEX-COLOR (such as `#3cb878`) return the hex color that is
PERCENT percent brighter (or darker if percent value is
negative)."
  (let ((color-transform-fn (if (> percent 0)
				'color-lighten-hsl
			      'color-darken-hsl))
	(percent-unsigned (abs percent)))
     (apply 'color-rgb-to-hex
	    (append
	     (apply 'color-hsl-to-rgb
		    (apply color-transform-fn
			   (append
			    (apply 'color-rgb-to-hsl (color-name-to-rgb hex-color))
			    (list percent-unsigned))))
	     (list 2)))))


(defun immaterial-linear-rgb-component (col)
  "Calculates the linear RGB value for an sRGB color component COL.
COL must be a real value in the range [0.0, 1.0]."
  (when (or (< col 0) (> col 1))
    (error "Color value must be in range [0.0, 1.0]"))
  (if (> col 0.03928)
      (expt (/ (+ col 0.055) 1.055) 2.4)
    (/ col 12.92)))


(defun immaterial-relative-luminance (hex-color)
  "Calculates the relative luminance of color HEX-COLOR.
The relative luminance is the relative brightness of any point in
a colorspace, normalized to 0 for darkest black and 1 for
lightest white."
  (let* ((srgb (color-name-to-rgb hex-color))
	 (r (immaterial-linear-rgb-component (nth 0 srgb)))
	 (g (immaterial-linear-rgb-component (nth 1 srgb)))
	 (b (immaterial-linear-rgb-component (nth 2 srgb))))
    (+ (* 0.2126 r) (* 0.7152 g) (* 0.0722 b))))


(defun immaterial-contrast-ratio (hex-color1 hex-color2)
  "Calculates the contrast ratio between two colors HEX-COLOR1 and HEX-COLOR2.
The contrast ratio gives an indication of how easily a foreground
text in HEX-COLOR1 is perceived on a background in HEX-COLOR2.
To be easily readable, the contrast must be 7:1 according to the
Web Content Accessibility Guidelines.  The contrast ratio is a
value between 1 and 21."
  (let ((c1-lum (immaterial-relative-luminance hex-color1))
	(c2-lum (immaterial-relative-luminance hex-color2)))
    (if (> c1-lum c2-lum)
	(/ (+ c1-lum 0.05) (+ c2-lum 0.05))
      (/ (+ c2-lum 0.05) (+ c1-lum 0.05)))))


;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide 'immaterial-theme)

;;; immaterial-theme.el ends here
