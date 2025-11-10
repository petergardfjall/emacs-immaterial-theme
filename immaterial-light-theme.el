;;; immaterial-light-theme.el --- A light immaterial theme variant

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

;; A light variant of the `immaterial-theme'.

;;; Code:

(require 'immaterial-theme)

;; Named colors specific to the `immaterial-light' theme.
;;
;; These colors are given semantic meaning by `immaterial-theme-common-palette'.
(defconst immaterial-theme-light-specific-palette
  `(
     ;; Basic values

    (bg-main         "#fcfcfb")
    (bg-dim          "#f5f8f8") ;; Fringe, in-buffer completion popup bg, markdown code sections.
    (fg-main         "#222222")
    (fg-dim          "#727272")
    (fg-alt          "#484848")
    (bg-active       "#eaf3f2") ;; hl-line, in-buffer completion popup border.
    (bg-inactive     "#f5f5f2")
    (border          "#d9d9bc") ;; modeline border, window border.
    ;; Highligh selected items (not in `modus-themes-vivendi-palette').
    (bg-selected     ,(immaterial-color 'green "100"))
    ;; Common accent foregrounds
    ;;
    ;; For example, use seeds from "2014 Material Design color palettes" and
    ;; create dark (faint) and light (intense) variants with
    ;; https://mui.com/material-ui/customization/color/.
    (red             ,(immaterial-color 'red "900"))
    (red-warmer      ,(immaterial-color 'deep-orange "900"))
    (red-cooler      ,(immaterial-color 'pink "900"))
    (red-faint       ,(immaterial-color-lighten (immaterial-color 'red "900") -20))
    (red-intense     ,(immaterial-color 'red "600"))
    (green           ,(immaterial-color 'light-green "900"))
    (green-warmer    ,(immaterial-color 'green "900"))
    (green-cooler    ,(immaterial-color 'lime "900"))
    (green-faint     ,(immaterial-color 'light-green "700"))
    (green-intense   ,(immaterial-color 'light-green "900"))
    (yellow          ,(immaterial-color 'amber "400"))
    (yellow-warmer   ,(immaterial-color 'orange "400"))
    (yellow-cooler   ,(immaterial-color 'yellow "400"))
    (yellow-faint    ,(immaterial-color-lighten (immaterial-color 'amber "400") -20))
    (yellow-intense  ,(immaterial-color 'deep-orange "900"))
    (blue            ,(immaterial-color 'blue "900"))
    (blue-warmer     ,(immaterial-color 'indigo "900"))
    (blue-cooler     ,(immaterial-color 'light-blue "900"))
    (blue-faint      ,(immaterial-color-lighten (immaterial-color 'blue "900") -20))
    (blue-intense    ,(immaterial-color 'blue "900"))
    (magenta         ,(immaterial-color 'deep-purple "900"))
    (magenta-warmer  ,(immaterial-color 'purple "900"))
    (magenta-cooler  ,(immaterial-color 'indigo "900"))
    (magenta-faint   ,(immaterial-color-lighten (immaterial-color 'deep-purple "900") -20))
    (magenta-intense ,(immaterial-color-lighten (immaterial-color 'deep-purple "900") 20))
    (cyan            ,(immaterial-color 'cyan "800")) ;; Used as cursor.
    (cyan-warmer     ,(immaterial-color 'teal "800"))
    (cyan-cooler     ,(immaterial-color 'light-blue "800"))
    (cyan-faint      ,(immaterial-color-lighten (immaterial-color 'cyan "800") -20))
    (cyan-intense    ,(immaterial-color-lighten (immaterial-color 'cyan "800") 20))

    ;; Common accent backgrounds

    (bg-red-intense     "#ffc0c0") ;; Same as bg-removed-refine
    (bg-green-intense   "#acf2bd") ;; Same as bg-added-refine
    (bg-yellow-intense  "#fffad3") ;; Same as bg-changed-refine
    (bg-blue-intense    "#074c84")
    (bg-magenta-intense "#642b9d")
    (bg-cyan-intense    "#105962")

    (bg-red-subtle      "#ffeaea") ;; Same as bg-removed-faint
    (bg-green-subtle    "#eeffee") ;; Same as bg-added-faint
    (bg-yellow-subtle   "#feebb7") ;; Same as bg-changed-faint
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

    (bg-clay     "#f1c8b5") ;; Appears constant for all dark modus themes.
    (fg-clay     "#63192a") ;; Appears constant for all dark modus themes.

    (bg-ochre    "#f0e3c0") ;; Appears constant for all dark modus themes.
    (fg-ochre    "#573a30") ;; Appears constant for all dark modus themes.

    (bg-lavender "#dfcdfa") ;; Appears constant for all dark modus themes.
    (fg-lavender "#443379") ;; Appears constant for all dark modus themes.

    (bg-sage     "#c0e7d4") ;; Appears constant for all dark modus themes.
    (fg-sage     "#124b41") ;; Appears constant for all dark modus themes.

    (bg-mode-line-active       ,(immaterial-color 'teal "50"))
    (fg-mode-line-active       fg-alt)
    (border-mode-line-active   border)
    (bg-mode-line-inactive     bg-inactive)
    (fg-mode-line-inactive     "#757575")
    (border-mode-line-inactive border)

    ;; Diffs

    (bg-added           "#e6ffed")
    (bg-added-faint     "#eeffee") ;; Less pronounced (closer to bg color).
    (bg-added-refine    "#acf2bd") ;; More pronounced.
    (bg-added-fringe    bg-added-refine)
    (fg-added           unspecified)
    (fg-added-intense   unspecified)

    (bg-changed         "#fddf8c")
    (bg-changed-faint   "#feebb7") ;; Less pronounced (closer to bg color).
    (bg-changed-refine  "#fac835") ;; More pronounced.
    (bg-changed-fringe  bg-changed-refine)
    (fg-changed         unspecified)
    (fg-changed-intense unspecified)

    (bg-removed         "#ffd6d6")
    (bg-removed-faint   "#ffeaea") ;; Less pronounced (closer to bg color).
    (bg-removed-refine  "#ffc0c0")
    (bg-removed-fringe  bg-removed-refine)
    (fg-removed         unspecified)
    (fg-removed-intense unspecified)
    )
  "Color palette specific to `immaterial-light-theme'.")

(defcustom immaterial-light-palette-overrides nil
  "Overrides for `immaterial-light-palette'.

Mirror the elements of the aforementioned palette, overriding their
value."
  :group 'immaterial-theme
  :package-version '(immaterial-theme . "1.0.0")
  :type '(repeat (list symbol (choice symbol string)))
  :link '(info-link "(immaterial-theme) Light palette overrides"))

(defconst immaterial-light-palette
  (append immaterial-theme-light-specific-palette
          immaterial-theme-common-palette
          immaterial-theme-common-palette-mappings)
  "The entire palette of the `immaterial-light-theme' theme.")

;; Instantiates the theme object.
(modus-themes-theme
 'immaterial-light
 'immaterial-theme
 "A light theme based on material colors."
 'dark
 ;; Core palette (named colors and semantic mappings).
 'modus-themes-operandi-palette
 ;; User palette. Arbitrary named colors that, like the core palette of
 ;; `modus-themes-operandi-palette' (above) can be used to define the custom
 ;; faces in `immaterial-theme-custom-faces' (below).
 'immaterial-light-palette
 ;; Overrides palette. Can hold overrides for the core and user palettes above.
 'immaterial-light-palette-overrides
 ;; Face definitions that extend/override the ones supplied by
 ;; `modus-themes-faces'.
 'immaterial-theme-custom-faces)

;;; immaterial-light-theme.el ends here
