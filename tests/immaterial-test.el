;;; immaterial-test.el -- tests for immaterial-theme.el -*- lexical-binding: t -*-
;;
;; Copyright © 2020 Peter Gardfjäll <peter.gardfjall.work@gmail.com>
;;
;; Author: Peter Gardfjäll <peter.gardfjall.work@gmail.com>
;;
;; Package-Requires: ((cl-lib "0.5"))
;;
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; This file is part of immaterial-theme.el
;;
;;; Code:

(require 'immaterial-theme)
(require 'immaterial-dark-theme)
(require 'immaterial-light-theme)


(defun immaterial--rel-lum (hex-col)
  "Calculate relative luminance of HEX-COL and return result as a string with four decimals precision."
  (format "%.4f" (immaterial-relative-luminance hex-col)))

(defun immaterial--contrast-ratio (col1 col2)
  "Calculate contrast ratio between COL1 and COL2 and return result as a string with two decimals precision."
  (format "%.2f" (immaterial-contrast-ratio col1 col2)))

(defun immaterial--dark-color-get (color)
  "Get a COLOR symbol from the `immaterial-dark-palette'."
  ;; Resolves colors recursively.
  (modus-themes--retrieve-palette-value color immaterial-dark-palette))

(defun immaterial--light-color-get (color)
  "Get a COLOR symbol from the `immaterial-light-palette'."
  ;; Resolves colors recursively.
  (modus-themes--retrieve-palette-value color immaterial-light-palette))



(defun immaterial-relative-luminance-test ()
  "Exercise the relative luminance calculations."
  (cl-assert (string= "0.0000" (immaterial--rel-lum "#000000"))
	     "unexpected relative luminance")
  (cl-assert (string= "1.0000" (immaterial--rel-lum "#ffffff"))
	     "unexpected relative luminance")


  (cl-assert (equal "0.0855" (immaterial--rel-lum "#aa0000"))
             "unexpected relative luminance")
  (cl-assert (string= "0.3554" (immaterial--rel-lum "#00bb00"))
	     "unexpected relative luminance")
  (cl-assert (string= "0.0436" (immaterial--rel-lum "#0000cc"))
	     "unexpected relative luminance")
  (cl-assert (string= "0.4845" (immaterial--rel-lum "#aabbcc"))
	     "unexpected relative luminance")
    (cl-assert (string= "0.7072" (immaterial--rel-lum "#ccddee"))
	     "unexpected relative luminance"))


(defun immaterial-contrast-ratio-test ()
  "Exercise the contrast ratio calculations."

  ;; black on white => highest possible contrast ratio
  (cl-assert (string= "21.00" (immaterial--contrast-ratio "#ffffff" "#000000"))
	     "unexpected contrast ratio")
  ;; should be reflexive
  (cl-assert (string= "21.00" (immaterial--contrast-ratio "#000000" "#ffffff"))
	     "unexpected contrast ratio")

  ;; black on black => lowest possible contrast ratio
  (cl-assert (string= "1.00" (immaterial--contrast-ratio "#000000" "#000000"))
	     "unexpected contrast ratio")
  ;; gray on gray
  (cl-assert (string= "1.00" (immaterial--contrast-ratio "#aaaaaa" "#aaaaaa"))
	     "unexpected contrast ratio")


  (cl-assert (string= "13.08" (immaterial--contrast-ratio "#cccccc" "#000000"))
	     "unexpected contrast ratio")

  ;; red and green
  (cl-assert (string= "2.99" (immaterial--contrast-ratio "#aa0000" "#00bb00"))
	     "unexpected contrast ratio")

  ;; red orange on slate blue
  (cl-assert (string= "1.54" (immaterial--contrast-ratio "#ff4500" "#6a5acd"))
	     "unexpected contrast ratio")

  ;; deep pink on crimson
  (cl-assert (string= "1.37" (immaterial--contrast-ratio "#ff1493" "#dc143c"))
	     "unexpected contrast ratio")

  ;; medium blue on gainsboro
  (cl-assert (string= "8.14" (immaterial--contrast-ratio "#0000cd" "#dcdcdc"))
	     "unexpected contrast ratio"))

(defun immaterial-dark-palette-sufficient-contrast-test ()
  "Ensure that the immaterial dark palette (foregrounds and backgrounds) foreground colors satisfy a minimum ratio of 4.5:1 (WCAG success criterion 1.4.3) and the primary foreground color satisfies a minimum ratio of 7:1 (WCAG success criterion 1.4.6)."
  (message "verifying color contrast for dark theme")

  (cl-assert (string= "#012027" (immaterial--dark-color-get 'bg-main))
	     "should be using dark color variants")

  (cl-assert (<= 7 (immaterial-contrast-ratio
                    (immaterial--dark-color-get 'fg-main)
                    (immaterial--dark-color-get 'bg-main)))
             "insuffient contrast")

  ;; Primary accent color.
  (cl-assert (<= 4.5 (immaterial-contrast-ratio
                    (immaterial--dark-color-get 'accent-0)
                    (immaterial--dark-color-get 'bg-main)))
             "insuffient contrast")
  ;; Secondary accent color.
  (cl-assert (<= 4.5 (immaterial-contrast-ratio
                    (immaterial--dark-color-get 'accent-1)
                    (immaterial--dark-color-get 'bg-main)))
             "insuffient contrast")
  ;; Tertiary accent color.
  (cl-assert (<= 4.5 (immaterial-contrast-ratio
                    (immaterial--dark-color-get 'accent-2)
                    (immaterial--dark-color-get 'bg-main)))
             "insuffient contrast")

  ;; Comment color.
  (cl-assert (<= 4.5 (immaterial-contrast-ratio
        	      (immaterial--dark-color-get 'fg-dim)
                      (immaterial--dark-color-get 'bg-main)))
             "insuffient contrast")
  ;; Error color.
  (cl-assert (<= 4.5 (immaterial-contrast-ratio
        	      (immaterial--dark-color-get 'err)
                      (immaterial--dark-color-get 'bg-main)))
             "insuffient contrast")
  ;; Warning color.
  (cl-assert (<= 4.5 (immaterial-contrast-ratio
        	      (immaterial--dark-color-get 'warning)
                      (immaterial--dark-color-get 'bg-main)))
             "insuffient contrast")
)

(defun immaterial-light-palette-sufficient-contrast-test ()
  "Ensure that the immaterial light palette (foregrounds and backgrounds) foreground colors satisfy a minimum ratio of 4.5:1 (WCAG success criterion 1.4.3) and the primary foreground color satisfies a minimum ratio of 7:1 (WCAG success criterion 1.4.6)."
  (message "verifying color contrast for light theme")

  (cl-assert (string= "#fcfcfb" (immaterial--light-color-get 'bg-main))
	     "should be using light color variants")

  (cl-assert (<= 7 (immaterial-contrast-ratio
                    (immaterial--light-color-get 'fg-main)
                    (immaterial--light-color-get 'bg-main)))
             "insuffient contrast")

  ;; Primary accent color.
  (cl-assert (<= 4.5 (immaterial-contrast-ratio
                    (immaterial--light-color-get 'accent-0)
                    (immaterial--light-color-get 'bg-main)))
             "insuffient contrast")
  ;; Secondary accent color.
  (cl-assert (<= 4.5 (immaterial-contrast-ratio
                    (immaterial--light-color-get 'accent-1)
                    (immaterial--light-color-get 'bg-main)))
             "insuffient contrast")
  ;; Tertiary accent color.
  (cl-assert (<= 4.5 (immaterial-contrast-ratio
                    (immaterial--light-color-get 'accent-2)
                    (immaterial--light-color-get 'bg-main)))
             "insuffient contrast")

  ;; Comment color.
  (cl-assert (<= 4.5 (immaterial-contrast-ratio
        	      (immaterial--light-color-get 'fg-dim)
                      (immaterial--light-color-get 'bg-main)))
             "insuffient contrast")
  ;; Error color.
  (cl-assert (<= 4.5 (immaterial-contrast-ratio
        	      (immaterial--light-color-get 'err)
                      (immaterial--light-color-get 'bg-main)))
             "insuffient contrast")
  ;; Warning color.
  (cl-assert (<= 4.5 (immaterial-contrast-ratio
        	      (immaterial--light-color-get 'warning)
                      (immaterial--light-color-get 'bg-main)))
             "insuffient contrast")
  )


;; (defun immaterial--print-contrast-ratios ()
;;   "Print relevant contrast ratios."
;;   (defun ratio (color-name-1 color-name-2 variant)
;;     (let ((ratio (immaterial-contrast-ratio (immaterial-color color-name-1 variant)
;; 					    (immaterial-color color-name-2 variant))))
;;       (format "%s / %s (%s): %.2f" color-name-1 color-name-2 variant ratio)))

;;   (print (ratio "foreground-primary"   "background-primary" 'dark) #'external-debugging-output)
;;   (print (ratio "foreground-secondary" "background-primary" 'dark) #'external-debugging-output)
;;   (print (ratio "foreground-tertiary"  "background-primary" 'dark) #'external-debugging-output)
;;   (print (ratio "primary"              "background-primary" 'dark) #'external-debugging-output)
;;   (print (ratio "primary-hi"           "background-primary" 'dark) #'external-debugging-output)
;;   (print (ratio "primary-lo"           "background-primary" 'dark) #'external-debugging-output)
;;   (print (ratio "secondary"            "background-primary" 'dark) #'external-debugging-output)
;;   (print (ratio "secondary-hi"         "background-primary" 'dark) #'external-debugging-output)
;;   (print (ratio "secondary-lo"         "background-primary" 'dark) #'external-debugging-output)
;;   (print (ratio "tertiary"             "background-primary" 'dark) #'external-debugging-output)
;;   (print (ratio "tertiary-hi"          "background-primary" 'dark) #'external-debugging-output)
;;   (print (ratio "tertiary-lo"          "background-primary" 'dark) #'external-debugging-output)
;;   (print (ratio "error"                "background-primary" 'dark) #'external-debugging-output)
;;   (print (ratio "warning"              "background-primary" 'dark) #'external-debugging-output)
;;   (print (ratio "discrete"             "background-primary" 'dark) #'external-debugging-output)

;;   (print (ratio "foreground-primary"   "background-primary" 'light) #'external-debugging-output)
;;   (print (ratio "foreground-secondary" "background-primary" 'light) #'external-debugging-output)
;;   (print (ratio "foreground-tertiary"  "background-primary" 'light) #'external-debugging-output)
;;   (print (ratio "primary"              "background-primary" 'light) #'external-debugging-output)
;;   (print (ratio "primary-hi"           "background-primary" 'light) #'external-debugging-output)
;;   (print (ratio "primary-lo"           "background-primary" 'light) #'external-debugging-output)
;;   (print (ratio "secondary"            "background-primary" 'light) #'external-debugging-output)
;;   (print (ratio "secondary-hi"         "background-primary" 'light) #'external-debugging-output)
;;   (print (ratio "secondary-lo"         "background-primary" 'light) #'external-debugging-output)
;;   (print (ratio "tertiary"             "background-primary" 'light) #'external-debugging-output)
;;   (print (ratio "tertiary-hi"          "background-primary" 'light) #'external-debugging-output)
;;   (print (ratio "tertiary-lo"          "background-primary" 'light) #'external-debugging-output)
;;   (print (ratio "error"                "background-primary" 'light) #'external-debugging-output)
;;   (print (ratio "warning"              "background-primary" 'light) #'external-debugging-output)
;;   (print (ratio "discrete"             "background-primary" 'light) #'external-debugging-output))

(defun immaterial-test-suite ()
  "Run the entire suite of test functions."
  (immaterial-relative-luminance-test)
  (immaterial-contrast-ratio-test)
  (immaterial-dark-palette-sufficient-contrast-test)
  (immaterial-light-palette-sufficient-contrast-test)
  ;; (immaterial--print-contrast-ratios)
  (immaterial-test-end))

(defun immaterial-test-end ()
  "Any code to run when a test function ends."
  (print "all tests completed successfully." #'external-debugging-output)
  (kill-emacs))


(provide 'immaterial-test)
;;; immaterial-test.el ends here
