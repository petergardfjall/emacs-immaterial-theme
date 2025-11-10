;;; immaterial-dark-theme.el --- A dark immaterial theme variant

;; Copyright (C) 2019-2025 Peter Gardfjäll
;; Author: Peter Gardfjäll

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

;; A dark variant of the `immaterial-theme'.

;;; Code:

(require 'immaterial-theme)

;; Named colors specific to the `immaterial-dark' theme.
;;
;; These colors are given semantic meaning by `immaterial-theme-common-palette'.
(defconst immaterial-theme-dark-specific-palette
  `(
     ;; Basic values

    (bg-main         "#012027")
    (bg-dim          "#001b21") ;; Fringe, in-buffer completion popup bg.
    (fg-main         "#dddddd")
    (fg-dim          "#848484")
    (fg-alt          "#c8c8c8")
    (bg-active       "#01343f") ;; hl-line, in-buffer completion popup border.
    (bg-inactive     "#001b21")
    (border          "#001b21") ;; modeline border, window border.
    ;; Highligh selected items (not in `modus-themes-vivendi-palette').
    (bg-selected     "#004346")
    ;; Common accent foregrounds
    ;;
    ;; For example, use seeds from "2014 Material Design color palettes" and
    ;; create dark (faint) and light (intense) variants with
    ;; https://mui.com/material-ui/customization/color/.
    (red             ,(immaterial-color 'red "200"))
    (red-warmer      ,(immaterial-color 'deep-orange "200"))
    (red-cooler      ,(immaterial-color 'pink "200"))
    (red-faint       ,(immaterial-color-lighten (immaterial-color 'red "200") -20))
    (red-intense     ,(immaterial-color 'red "600"))
    (green           ,(immaterial-color 'light-green "200"))
    (green-warmer    ,(immaterial-color 'green "400"))
    (green-cooler    ,(immaterial-color 'lime "400"))
    (green-faint     ,(immaterial-color 'light-green "400"))
    (green-intense   ,(immaterial-color 'light-green "600"))
    (yellow          ,(immaterial-color 'amber "400"))
    (yellow-warmer   ,(immaterial-color 'orange "400"))
    (yellow-cooler   ,(immaterial-color 'yellow "400"))
    (yellow-faint    ,(immaterial-color-lighten (immaterial-color 'amber "400") -20))
    (yellow-intense  ,(immaterial-color 'amber "800"))
    (blue            ,(immaterial-color 'blue "200"))
    (blue-warmer     ,(immaterial-color 'indigo "200"))
    (blue-cooler     ,(immaterial-color 'light-blue "200"))
    (blue-faint      ,(immaterial-color-lighten (immaterial-color 'blue "200") -20))
    (blue-intense    ,(immaterial-color 'blue "400"))
    (magenta         ,(immaterial-color 'deep-purple "200"))
    (magenta-warmer  ,(immaterial-color 'purple "200"))
    (magenta-cooler  ,(immaterial-color 'indigo "200"))
    (magenta-faint   ,(immaterial-color-lighten (immaterial-color 'deep-purple "200") -20))
    (magenta-intense ,(immaterial-color-lighten (immaterial-color 'deep-purple "200") 20))
    (cyan            ,(immaterial-color 'cyan "200"))
    (cyan-warmer     ,(immaterial-color 'teal "200"))
    (cyan-cooler     ,(immaterial-color 'light-blue "200"))
    (cyan-faint      ,(immaterial-color-lighten (immaterial-color 'cyan "200") -20))
    (cyan-intense    ,(immaterial-color-lighten (immaterial-color 'cyan "200") 20))

    ;; Common accent backgrounds

    (bg-red-intense     "#8d2323") ;; Same as bg-removed-refine
    (bg-green-intense   "#175b2b") ;; Same as bg-added-refine
    (bg-yellow-intense  "#555500") ;; Same as bg-changed-refine
    (bg-blue-intense    "#074c84")
    (bg-magenta-intense "#642b9d")
    (bg-cyan-intense    "#105962")

    (bg-red-subtle      "#260910") ;; Same as bg-removed-faint
    (bg-green-subtle    "#012215") ;; Same as bg-added-faint
    (bg-yellow-subtle   "#2a1f00") ;; Same as bg-changed-faint
    (bg-blue-subtle     "#043358")
    (bg-magenta-subtle  "#451e6d")
    (bg-cyan-subtle     "#0e4b53")

    (bg-red-nuanced     unspecified) ;; Unused in `modus-themes-common-palette-mappings'.
    (bg-green-nuanced   unspecified) ;; Unused in `modus-themes-common-palette-mappings'.
    (bg-yellow-nuanced  unspecified) ;; Unused in `modus-themes-common-palette-mappings'.
    (bg-blue-nuanced    unspecified) ;; Unused in `modus-themes-common-palette-mappings'.
    (bg-magenta-nuanced unspecified) ;; Unused in `modus-themes-common-palette-mappings'.
    (bg-cyan-nuanced    unspecified) ;; Unused in `modus-themes-common-palette-mappings'.

    ;; Uncommon accent background and foreground pairs

    (bg-clay     "#49191a") ;; Appears constant for all dark modus themes.
    (fg-clay     "#f1b090") ;; Appears constant for all dark modus themes.

    (bg-ochre    "#462f20") ;; Appears constant for all dark modus themes.
    (fg-ochre    "#e0d38c") ;; Appears constant for all dark modus themes.

    (bg-lavender "#38325c") ;; Appears constant for all dark modus themes.
    (fg-lavender "#dfc0f0") ;; Appears constant for all dark modus themes.

    (bg-sage     "#143e32") ;; Appears constant for all dark modus themes.
    (fg-sage     "#c3e7d4") ;; Appears constant for all dark modus themes.

    (bg-mode-line-active       "#005662")
    (fg-mode-line-active       "#ffffff")
    (border-mode-line-active   border)
    (bg-mode-line-inactive     "#001017")
    (fg-mode-line-inactive     "#848484")
    (border-mode-line-inactive border)

    ;; Diffs

    (bg-added           "#033521")
    (bg-added-faint     "#012215") ;; Less pronounced (closer to bg color).
    (bg-added-refine    "#175b2b") ;; More pronounced.
    (bg-added-fringe    bg-added-refine)
    (fg-added           unspecified)
    (fg-added-intense   unspecified)

    (bg-changed         "#363300")
    (bg-changed-faint   "#2a1f00") ;; Less pronounced (closer to bg color).
    (bg-changed-refine  "#555500") ;; More pronounced.
    (bg-changed-fringe  bg-changed-refine)
    (fg-changed         unspecified)
    (fg-changed-intense unspecified)

    (bg-removed         "#3b0f19")
    (bg-removed-faint   "#260910") ;; Less pronounced (closer to bg color).
    (bg-removed-refine  "#8d2323")
    (bg-removed-fringe  bg-removed-refine)
    (fg-removed         unspecified)
    (fg-removed-intense unspecified)
    )
  "Color palette specific to `immaterial-dark-theme'.")

(defcustom immaterial-dark-palette-overrides nil
  "Overrides for `immaterial-dark-palette'.

Mirror the elements of the aforementioned palette, overriding their
value."
  :group 'immaterial-theme
  :package-version '(immaterial-theme . "1.0.0")
  :type '(repeat (list symbol (choice symbol string)))
  :link '(info-link "(immaterial-theme) Dark palette overrides"))

(defconst immaterial-dark-palette
  (append immaterial-theme-dark-specific-palette
          immaterial-theme-common-palette
          immaterial-theme-common-palette-mappings)
  "The entire palette of the `immaterial-dark-theme' theme.")

;; Instantiates the theme object.
(modus-themes-theme
 'immaterial-dark
 'immaterial-theme
 "A dark theme based on material colors."
 'dark
 ;; Core palette (named colors and semantic mappings).
 'modus-themes-vivendi-palette
 ;; User palette. Arbitrary named colors that, like the core palette of
 ;; `modus-themes-vivendi-palette' (above) can be used to define the custom
 ;; faces in `immaterial-theme-custom-faces' (below).
 'immaterial-dark-palette
 ;; Overrides palette. Can hold overrides for the core and user palettes above.
 'immaterial-dark-palette-overrides
 ;; Face definitions that extend/override the ones supplied by
 ;; `modus-themes-faces'.
 'immaterial-theme-custom-faces)

;;; immaterial-dark-theme.el ends here
