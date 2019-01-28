;;; themacs-theme.el --- Theme

;; Copyright (C) 2016 ,

;; Author: Peter Gardfjäll
;; Version: 0.1
;; Package-Requires: ((emacs "24"))
;; Created with ThemeCreator, https://github.com/mswift42/themecreator.


;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;; This file is not part of Emacs.

;;; Commentary:

;;; Code:

(deftheme themacs)

(defun themacs-color (color-name)
  (let ((colmap '(("white"       . "#FFFFFF")
		  ("white-1"     . "#EEEEEE") ;; off-white
		  ("gray+2"      . "#dbdbdb")
		  ("gray+1"      . "#c8c8c8")
		  ("gray"        . "#b5b5b5")
		  ("gray-1"      . "#aaaaaa")
		  ("gray-2"      . "#777777")
		  ("gray-3"      . "#555555")
		  ("gray-4"      . "#333333")
		  ("blue+2"      . "#51a4f7")
		  ("blue+1"      . "#4790d8")
		  ("blue"        . "#366ba0")
		  ("blue-1"      . "#224466")
		  ("blue-2"      . "#112233")
		  ("lightblue"   . "#84BDF4")
		  ("limegreen"   .  "#55FF55")
		  ("cyan"        .  "#00D7FF")
		  ("gray"        .  "#aaaaaa")
		  ("darkgray"    .  "#555555")
		  ("paleyellow"  .  "#FFBF5A")
		  ("pink"        .  "#e75480")
		  ("orange"      .  "#E86310")
		  ("red"         .  "#E81022"))))
    (cdr (assoc color-name colmap))))


(let ((class '((class color) (min-colors 89)))
      (fg1 (themacs-color "white-1"))
      (fg2 (themacs-color "gray+2"))
      (fg3 (themacs-color "gray+1"))
      (fg4 (themacs-color "gray"))
      (bg1 (themacs-color "blue-2"))
      (bg2 (themacs-color "blue-1"))
      (bg3 (themacs-color "blue"))
      (bg4 (themacs-color "blue+1"))
      (builtin  (themacs-color "lightblue"))
      (keyword  (themacs-color "lightblue"))
      (const    (themacs-color "cyan"))
      (comment  (themacs-color "gray-1"))
      (discrete (themacs-color "gray-2"))
      (func     (themacs-color "limegreen"))
      (str      (themacs-color "pale-yellow"))
      (type     (themacs-color "limegreen"))
      (var      (themacs-color "limegreen"))
      (warning  (themacs-color "orange"))
      (warning2 (themacs-color "pink"))
      (error1   (themacs-color "red"))
      (cursor   (themacs-color "orange")))
  (custom-theme-set-faces
   'themacs
   `(default ((,class (:background ,bg1 :foreground ,fg1))))
   `(font-lock-builtin-face ((,class (:foreground ,builtin))))
   `(font-lock-comment-face ((,class (:foreground ,comment))))
   `(font-lock-negation-char-face ((,class (:foreground ,const))))
   `(font-lock-reference-face ((,class (:foreground ,const))))
   `(font-lock-constant-face ((,class (:foreground ,const))))
   `(font-lock-doc-face ((,class (:foreground ,comment))))
   `(font-lock-function-name-face ((,class (:foreground ,func ))))
   `(font-lock-keyword-face ((,class (:bold ,class :foreground ,keyword))))
   `(font-lock-string-face ((,class (:foreground ,str))))
   `(font-lock-type-face ((,class (:foreground ,type :bold))))
   `(font-lock-variable-name-face ((,class (:foreground ,var))))
   `(font-lock-warning-face ((,class (:foreground ,warning :background ,bg2))))
   `(term-color-black ((,class (:foreground ,fg2 :background nil))))
   ;; region selection
   `(region ((,class (:background ,bg2))))
   `(highlight ((,class (:background ,bg2))))
   `(hl-line ((,class (:background  ,bg2))))
   ;; linum-mode column
   `(linum  ((t (:foreground ,discrete :background ,bg1 :height 1.0 :weight normal))))
   `(fringe ((,class (:background ,bg1))))
   `(cursor ((,class (:background ,orange))))
   `(show-paren-match-face ((,class (:background ,func))))
   `(show-paren-mismatch-face ((,class (:background ,error1))))
   `(isearch ((,class (:bold t :foreground ,fg1 :background ,bg2))))
   `(mode-line ((,class (:box (:line-width 1 :color ,keyword) :bold t :foreground ,fg1 :background ,bg1))))
   ;; mode-line
   `(mode-line           ((,class (:foreground ,fg1 :background ,bg1 :box (:color ,keyword :line-width 1)))))
   `(mode-line-buffer-id ((,class (:inherit bold :foreground ,keyword))))
   `(mode-line-inactive  ((,class (:foreground ,fg1 :background ,bg1  :box (:color ,keyword :line-width 1)))))
   ;; powerline
   `(powerline-active1 ((,class (:background ,discrete :foreground ,fg1))))
   `(powerline-inactive1 ((,class (:background ,bg2 :foreground ,fg1))))

   `(vertical-border ((,class (:foreground ,fg3))))
   `(minibuffer-prompt ((,class (:bold t :foreground ,keyword))))
   `(default-italic ((,class (:italic t))))
   `(link ((,class (:foreground ,const :underline t))))

   `(gnus-header-content ((,class (:foreground ,keyword))))
   `(gnus-header-from ((,class (:foreground ,var))))
   `(gnus-header-name ((,class (:foreground ,type))))
   `(gnus-header-subject ((,class (:foreground ,func :bold t))))
   `(warning ((,class (:foreground ,warning))))
   `(ac-completion-face ((,class (:underline t :foreground ,keyword))))
   `(info-quoted-name ((,class (:foreground ,builtin))))
   `(info-string ((,class (:foreground ,str))))
   `(icompletep-determined ((,class :foreground ,builtin)))
   `(undo-tree-visualizer-current-face ((,class :foreground ,builtin)))
   `(undo-tree-visualizer-default-face ((,class :foreground ,fg2)))
   `(undo-tree-visualizer-unmodified-face ((,class :foreground ,var)))
   `(undo-tree-visualizer-register-face ((,class :foreground ,type)))
   `(slime-repl-inputed-output-face ((,class (:foreground ,type))))
   `(trailing-whitespace ((,class :foreground nil :background ,warning)))
   `(lazy-highlight ((,class (:foreground ,fg2 :background ,bg3))))
   `(term ((,class (:foreground ,fg1 :background ,bg1))))
   `(term-color-black ((,class (:foreground ,bg3 :background ,bg3))))
   `(term-color-blue ((,class (:foreground ,func :background ,func))))
   `(term-color-red ((,class (:foreground ,keyword :background ,bg3))))
   `(term-color-green ((,class (:foreground ,type :background ,bg3))))
   `(term-color-yellow ((,class (:foreground ,var :background ,var))))
   `(term-color-magenta ((,class (:foreground ,builtin :background ,builtin))))
   `(term-color-cyan ((,class (:foreground ,str :background ,str))))
   `(term-color-white ((,class (:foreground ,fg2 :background ,fg2))))
   `(company-echo-common ((,class (:foreground ,bg1 :background ,fg1))))
   `(company-preview ((,class (:background ,bg1 :foreground ,var))))
   `(company-preview-common ((,class (:foreground ,bg2 :foreground ,fg3))))
   `(company-preview-search ((,class (:foreground ,type :background ,bg1))))
   `(company-scrollbar-bg ((,class (:background ,bg3))))
   `(company-scrollbar-fg ((,class (:foreground ,keyword))))
   `(company-tooltip ((,class (:foreground ,fg2 :background ,bg1 :bold t))))
   `(company-tooltop-annotation ((,class (:foreground ,const))))
   `(company-tooltip-common ((,class ( :foreground ,fg3))))
   `(company-tooltip-common-selection ((,class (:foreground ,str))))
   `(company-tooltip-mouse ((,class (:inherit highlight))))
   `(company-tooltip-selection ((,class (:background ,bg3 :foreground ,fg3))))
   `(company-template-field ((,class (:inherit region))))))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'themacs)

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; themacs-theme.el ends here
