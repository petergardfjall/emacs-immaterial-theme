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

(defvar themacs-color-override-alist
  '(())
  "Values provided here will override values in themacs-color-alist.
The material color tool https://material.io/tools/color/ is recommended
for constructing primary and secondary color schemes.")

;; Tip: enable rainbow-mode to preview the colors.
(defconst themacs-color-alist
  '(
    ;;("background-primary"    . "#112233")
    ;;("background-secondary"  . "#224466")
    ;;("background-tertiary"   . "#366ba0")
    ("background-primary"    . "#102027")
    ("background-secondary"  . "#37474f")
    ("background-tertiary"   . "#62727b")
    ("foreground-primary"    . "#eeeeee")
    ("foreground-secondary"  . "#dbdbdb")
    ("foreground-tertiary"   . "#c8c8c8")
    ("primary"         . "#4db6ac")
    ("primary-light"   . "#82e9de")
    ("primary-dark"    . "#00867d")
    ("secondary"       . "#aed581")
    ("secondary-light" . "#e1ffb1")
    ("secondary-dark"  . "#7da453")
    ("error"           . "#ff5555")
    ("warning"         . "#e86310")
    ("discrete"        . "#777777")
    ("cursor"          . "#e86310")
    )
  "The default color palette to use for the theme.
Values can be overridden via themacs-color-override-alist).
The palette was created using the https://material.io/tools/color/ tool.")

(defun themacs-color (color-name)
  "Retrieves the hex color value registered for a ´COLOR-NAME´.
The overrides in themacs-color-override-alist take precedence
over the default ones defined in themacs-color-alist."
  (let ((colmap (append themacs-color-override-alist themacs-color-alist)))
    (cdr (assoc color-name colmap))))


(let ((class '((class color) (min-colors 89)))
      (fg1      (themacs-color "foreground-primary"))
      (fg2      (themacs-color "foreground-secondary"))
      (fg3      (themacs-color "foreground-tertiary"))
      (bg1      (themacs-color "background-primary"))
      (bg2      (themacs-color "background-secondary"))
      (bg3      (themacs-color "background-tertiary"))
      (keyword  (themacs-color "primary"))
      (builtin  (themacs-color "primary-light"))
      (const    (themacs-color "primary-dark"))
      (type     (themacs-color "secondary"))
      (var      (themacs-color "secondary-light"))
      (func     (themacs-color "secondary-dark"))
      (str      (themacs-color "secondary-dark"))
      (comment  (themacs-color "discrete"))
      (linum-fg (themacs-color "discrete"))
      (negation (themacs-color "warning"))
      (warning  (themacs-color "warning"))
      (error    (themacs-color "error"))
      (cursor   (themacs-color "cursor")))
  (custom-theme-set-faces
   'themacs
   `(default ((,class (:background ,bg1 :foreground ,fg1))))

   ;;
   ;; Syntax higlighting/font-lock minor mode. (syntax rules are provided by
   ;; the particular major-mode).
   ;;
   ;; for the names of built-in functions.
   `(font-lock-builtin-face ((,class (:foreground ,builtin))))
   ;; for comments
   `(font-lock-comment-face ((,class (:foreground ,comment))))
   ;; for comment delimiters, like ‘/*’ and ‘*/’ in C.
   `(font-lock-comment-delimiter-face ((,class (:foreground ,comment))))
   ;; for easily-overlooked negation characters.
   `(font-lock-negation-char-face ((,class (:foreground ,negation))))
   ;; for the names of constants, like ‘NULL’ in C.
   `(font-lock-constant-face ((,class (:foreground ,const))))
   ;; for documentation strings in the code.
   `(font-lock-doc-face ((,class (:foreground ,comment))))
   ;; for the name of a function being defined or declared.
   `(font-lock-function-name-face ((,class (:foreground ,func ))))
   ;; for a keyword with special syntactic significance, like ‘if’.
   `(font-lock-keyword-face ((,class (:bold t :foreground ,keyword))))
   ;; for string literals.
   `(font-lock-string-face ((,class (:foreground ,str))))
   ;; for the names of user-defined data types.
   `(font-lock-type-face ((,class (:foreground ,type))))
   ;; for the name of a variable being defined or declared.
   `(font-lock-variable-name-face ((,class (:foreground ,var))))
   ;; for a construct that is peculiar, or that greatly changes the meaning of
   ;; other text, like ‘;;;###autoload’ in Emacs Lisp and ‘#error’ in C.
   `(font-lock-warning-face ((,class (:foreground ,warning :background ,bg2))))
   ;; region selection
   `(region ((,class (:foreground ,fg1 :background ,bg2))))
   `(highlight ((,class (:background ,bg2))))
   `(hl-line ((,class (:background  ,bg2))))
   ;; linum-mode column
   `(linum  ((t (:foreground ,linum-fg :background ,bg1 :height 1.0 :weight normal))))
   `(fringe ((,class (:background ,bg1))))
   `(cursor ((,class (:background ,cursor))))
   `(show-paren-match-face ((,class (:background ,fg1 :foreground ,bg1))))
   `(show-paren-mismatch-face ((,class (:background ,error))))
   `(isearch ((,class (:bold t :foreground ,fg1 :background ,bg2))))
   `(mode-line ((,class (:box (:line-width 1 :color ,fg1) :bold t :foreground ,fg1 :background ,bg1))))

   ;;
   ;; mode-line
   ;;
   ;; mode-line of the active buffer (e.g. in case of split window)
   `(mode-line           ((,class (:foreground ,fg1 :background ,bg2 :box (:color ,bg1 :line-width 1)))))
   ;; mode-line of the inactive buffer (e.g. in case of split window)
   `(mode-line-inactive  ((,class (:foreground ,fg1 :background ,bg1  :box (:color ,bg1 :line-width 1)))))
   `(mode-line-buffer-id ((,class (:inherit bold :foreground ,fg1))))
   ;; powerline
   `(powerline-active1 ((,class (:background ,bg3 :foreground ,fg1))))
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
   ;;
   ;; ansi-term/term
   ;;
   `(term ((,class (:foreground nil :background nil :inherit default))))
   `(term-color-black   ((,class (:foreground ,fg1 :background ,fg1))))
   `(term-color-red     ((,class (:foreground ,(themacs-color "error") :background ,(themacs-color "red")))))
   `(term-color-blue   ((,class (:foreground ,(themacs-color "primary-light") :background ,(themacs-color "primary-light")))))
   `(term-color-yellow  ((,class (:foreground ,(themacs-color "primary") :background ,(themacs-color "primary")))))
   `(term-color-magenta ((,class (:foreground ,(themacs-color "warning") :background ,(themacs-color "warning")))))
   `(term-color-cyan    ((,class (:foreground ,(themacs-color "secondary-dark") :background ,(themacs-color "secondary-dark")))))
   `(term-color-green    ((,class (:foreground ,(themacs-color "secondary") :background ,(themacs-color "secondary")))))
   `(term-color-white   ((,class (:foreground ,bg1 :background ,bg1))))
   ;;
   ;; company -- "complete any" completion engine
   ;;
   ;; Face used for the common part of completions in the echo area
   `(company-echo-common ((,class (:foreground ,bg1 :background ,fg1))))
   ;; display (single remaining) suggestion while typing
   `(company-preview ((,class (:background ,bg2 :foreground ,fg1))))
   `(company-preview-common ((,class (:background ,bg2 :foreground ,fg1))))
   `(company-preview-search ((,class (:foreground ,bg2 :background ,fg1))))
   ;; scrollbar style in company tooltip
   `(company-scrollbar-bg ((,class (:background ,bg3))))
   `(company-scrollbar-fg ((,class (:foreground ,bg1))))
   ;; general style of tooltip popup
   `(company-tooltip ((,class (:foreground ,bg2 :background ,fg2 :bold t))))
   ;; annotation appearance (could be the return-type of a function)
   `(company-tooltip-annotation ((,class (:weight normal :foreground ,bg2 :background ,fg2))))
   ;; annotation appearance for the selected item in the completion list
   `(company-tooltip-annotation-selection ((,class (:weight normal :inherit company-tooltip-selection))))
   `(company-tooltip-search ((,class (:weight normal :inherit company-tooltip-selection))))
   ;; the highlight style to use when typing and showing common search prefix
   `(company-tooltip-common ((,class (:weight extra-bold :foreground ,bg1))))
   `(company-tooltip-common-selection ((,class (:foreground ,str))))
   ;; style for item mouse is hovering over
   `(company-tooltip-mouse ((,class (:inherit company-tooltip-selection))))
   `(company-tooltip-selection ((,class (:background ,bg3 :foreground ,fg3))))
   `(company-tooltip-selection ((,class (:weight bold :foreground ,fg3 :background ,bg3))))
   ;;
   ;; sh-mode
   ;;
   `(sh-heredoc ((,class (:foreground nil :inherit font-lock-string-face :weight normal))))
   `(sh-quoted-exec ((,class (:foreground nil :inherit font-lock-function-name-face))))
   ;;
   ;; neotree
   ;;
   `(neo-dir-link-face ((,class (:foreground ,keyword :inherit bold))))
   `(neo-expand-btn-face ((,class (:foreground ,fg1))))
   `(neo-file-link-face ((,class (:foreground ,fg1))))
   `(neo-root-dir-face ((,class (:foreground ,func :inherit bold))))

   ))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'themacs)

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; themacs-theme.el ends here
