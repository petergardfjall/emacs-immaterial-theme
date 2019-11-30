;;; immaterial-theme.el --- A flexible theme based on material design principles

;; Copyright (C) 2019 Peter Gardfjäll

;; Author: Peter Gardfjäll
;; Keywords: themes
;; URL: https://github.com/petergardfjall/emacs-immaterial-theme
;; Version: 0.2.0
;; Package-Requires: ((emacs "25"))

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

;; To use the theme, put the following in your Emacs configuration file:
;;
;;   (load-theme 'immaterial t)
;;
;; Requirements: Emacs 25.
;;

;;; Code:

(deftheme immaterial
  "A customizable theme based on Material design principles.")

(defface immaterial-small-face
  '((t :height 0.95))
  "Face that can be used via :inherit on faces that should have a smaller font size."
  :group 'immaterial-faces)

(defvar immaterial-color-override-alist
  '(())
  "Values provided here will override values in immaterial-color-alist.
The material color tool https://material.io/tools/color/ is recommended
for constructing primary and secondary color schemes.")

;; Tip: enable rainbow-mode to preview the colors.
(defconst immaterial-color-alist
  '(("background-primary"    . "#102027")
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
    ("cursor"          . "#e86310"))
  "The default color palette to use for the theme.
Values can be overridden via immaterial-color-override-alist).
The palette was created using the https://material.io/tools/color/ tool.")

(defun immaterial-color (color-name)
  "Retrieves the hex color value registered for a ´COLOR-NAME´.
The overrides in immaterial-color-override-alist take precedence
over the default ones defined in immaterial-color-alist."
  (let ((colmap (append immaterial-color-override-alist immaterial-color-alist)))
    (cdr (assoc color-name colmap))))


(let ((class '((class color) (min-colors 89)))
      (fg1        (immaterial-color "foreground-primary"))
      (fg2        (immaterial-color "foreground-secondary"))
      (fg3        (immaterial-color "foreground-tertiary"))
      (bg1        (immaterial-color "background-primary"))
      (bg2        (immaterial-color "background-secondary"))
      (bg3        (immaterial-color "background-tertiary"))
      (prim       (immaterial-color "primary"))
      (prim-light (immaterial-color "primary-light"))
      (prim-dark  (immaterial-color "primary-dark"))
      (sec        (immaterial-color "secondary"))
      (sec-light  (immaterial-color "secondary-light"))
      (sec-dark   (immaterial-color "secondary-dark"))
      (discrete   (immaterial-color "discrete"))

      (keyword    (immaterial-color "primary"))
      (builtin    (immaterial-color "primary-light"))
      (const      (immaterial-color "primary-dark"))
      (str        (immaterial-color "primary"))
      (type       (immaterial-color "secondary"))
      (var        (immaterial-color "secondary-dark"))
      (func       (immaterial-color "secondary-dark"))
      (linum-fg   (immaterial-color "discrete"))
      (negation   (immaterial-color "warning"))
      (warning    (immaterial-color "warning"))
      (error      (immaterial-color "error"))
      (cursor     (immaterial-color "cursor")))
  (custom-theme-set-faces
   'immaterial
   `(default ((,class (:background ,bg1 :foreground ,fg1))))
   ;;
   ;; Syntax higlighting/font-lock minor mode. (syntax rules are provided by
   ;; the particular major-mode).
   ;;

   ;; for a keyword with special syntactic significance, like ‘if’.
   `(font-lock-keyword-face ((,class (:bold t :foreground ,keyword))))
   ;; for the names of built-in functions.
   `(font-lock-builtin-face ((,class (:foreground ,builtin))))
   ;; for the names of constants, like ‘NULL’ in C.
   `(font-lock-constant-face ((,class (:foreground ,const))))
   ;; for string literals.
   `(font-lock-string-face ((,class (:foreground ,str))))

   ;; for the names of user-defined data types.
   `(font-lock-type-face ((,class (:foreground ,type))))
   ;; for the name of a variable being defined or declared.
   `(font-lock-variable-name-face ((,class (:foreground ,var))))
   ;; for the name of a function being defined or declared.
   `(font-lock-function-name-face ((,class (:foreground ,func ))))

   ;; for comments
   `(font-lock-comment-face ((,class (:foreground ,discrete, :italic t))))
   ;; for comment delimiters, like ‘/*’ and ‘*/’ in C.
   `(font-lock-comment-delimiter-face ((,class (:foreground ,discrete :italic t))))
   ;; for documentation strings in the code.
   `(font-lock-doc-face ((,class (:foreground ,discrete :italic t))))

   ;; for easily-overlooked negation characters.
   `(font-lock-negation-char-face ((,class (:foreground ,negation))))
   ;; for a construct that is peculiar, or that greatly changes the meaning of
   ;; other text, like ‘;;;###autoload’ in Emacs Lisp and ‘#error’ in C.
   `(font-lock-warning-face ((,class (:foreground ,warning :background ,bg2))))

   ;;
   ;; Buttons and links
   ;;
   `(button ((,class (:foreground ,sec :weight bold :underline t))))
   `(link ((,class (:foreground ,sec :weight bold :underline t))))
   `(link-visited ((,class (:foreground ,sec :weight bold :underline t))))

   ;;
   ;; region selection
   ;;
   `(region ((,class (:foreground ,fg1 :background ,bg2))))
   `(highlight ((,class (:background ,bg2))))
   ;; hl-line-mode background
   `(hl-line ((,class (:background  ,bg2))))
   ;; linum-mode column
   `(linum  ((t (:foreground ,linum-fg :background ,bg1 :height 1.0 :weight normal))))
   ;; display-line-numbers-mode (emacs26+)
   `(line-number  ((t (:foreground ,linum-fg :background ,bg1 :height 1.0 :weight normal))))
   `(line-number-current-line  ((t (:foreground ,fg1 :background ,bg1 :height 1.0 :weight normal))))
   `(fringe ((,class (:background ,bg1))))
   `(cursor ((,class (:background ,cursor))))
   `(show-paren-match-face ((,class (:background ,fg1 :foreground ,bg1))))
   `(show-paren-mismatch-face ((,class (:background ,error))))
   `(isearch ((,class (:bold t :foreground ,fg1 :background ,bg2))))
   ;;
   ;; mode-line
   ;;
   ;; mode-line of the active buffer (e.g. in case of split window)
   `(mode-line ((,class (:foreground ,fg1 :background ,bg2))))
   ;; mode-line of the inactive buffer (e.g. in case of split window)
   `(mode-line-inactive  ((,class (:foreground ,discrete :background ,bg1))))
   `(mode-line-buffer-id ((,class (:foreground ,fg1 :weight bold))))

   ;;
   ;; powerline
   ;;
   ;; for active buffer in the frame
   `(powerline-active1 ((,class (:background ,bg3 :foreground ,fg1))))
   `(powerline-active2 ((,class (:background ,bg2 :foreground ,fg1))))
   ;; for inactive buffers in the frame
   `(powerline-inactive1 ((,class (:background ,bg1 :foreground ,discrete))))
   `(powerline-inactive2 ((,class (:background ,bg1 :foreground ,discrete))))

   `(vertical-border ((,class (:foreground ,fg3))))
   `(minibuffer-prompt ((,class (:bold t :foreground ,prim))))
   `(default-italic ((,class (:italic t))))
   `(link ((,class (:foreground ,prim-dark :underline t))))

   `(gnus-header-content ((,class (:foreground ,prim))))
   `(gnus-header-from ((,class (:foreground ,sec-dark))))
   `(gnus-header-name ((,class (:foreground ,sec))))
   `(gnus-header-subject ((,class (:foreground ,sec-dark :bold t))))
   `(warning ((,class (:foreground ,warning))))
   `(ac-completion-face ((,class (:underline t :foreground ,prim))))
   `(info-quoted-name ((,class (:foreground ,prim-light))))
   `(info-string ((,class (:foreground ,str))))
   `(icompletep-determined ((,class :foreground ,prim-light)))
   ;;
   ;; undo-tree
   ;;
   `(undo-tree-visualizer-current-face ((,class :foreground ,prim-light)))
   `(undo-tree-visualizer-default-face ((,class :foreground ,fg2)))
   `(undo-tree-visualizer-unmodified-face ((,class :foreground ,sec-dark)))
   `(undo-tree-visualizer-register-face ((,class :foreground ,sec)))

   `(slime-repl-inputed-output-face ((,class (:foreground ,sec))))
   `(trailing-whitespace ((,class :foreground nil :background ,warning)))
   `(lazy-highlight ((,class (:foreground ,fg2 :background ,bg3))))
   ;;
   ;; ansi-term/term
   ;;
   `(term ((,class (:foreground nil :background nil :inherit default))))
   `(term-color-black   ((,class (:foreground ,fg1 :background ,fg1))))
   `(term-color-red     ((,class (:foreground ,(immaterial-color "error") :background ,(immaterial-color "red")))))
   `(term-color-blue   ((,class (:foreground ,(immaterial-color "primary-light") :background ,(immaterial-color "primary-light")))))
   `(term-color-yellow  ((,class (:foreground ,(immaterial-color "primary") :background ,(immaterial-color "primary")))))
   `(term-color-magenta ((,class (:foreground ,(immaterial-color "warning") :background ,(immaterial-color "warning")))))
   `(term-color-cyan    ((,class (:foreground ,(immaterial-color "secondary-dark") :background ,(immaterial-color "secondary-dark")))))
   `(term-color-green    ((,class (:foreground ,(immaterial-color "secondary") :background ,(immaterial-color "secondary")))))
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
   `(neo-dir-link-face ((,class (:foreground ,prim :inherit bold))))
   `(neo-expand-btn-face ((,class (:foreground ,fg1))))
   `(neo-file-link-face ((,class (:foreground ,fg1))))
   `(neo-root-dir-face ((,class (:foreground ,sec-dark :inherit bold))))
   ;;
   ;; markdown-mode
   ;;
   `(markdown-header-face-1 ((,class (:foreground ,sec-dark :weight bold))))
   `(markdown-header-face-2 ((,class (:foreground ,sec-dark :weight bold))))
   `(markdown-header-face-3 ((,class (:foreground ,sec-dark :weight bold))))
   `(markdown-header-face-4 ((,class (:foreground ,sec-dark :weight bold))))
   `(markdown-header-face-5 ((,class (:foreground ,sec-dark :weight bold))))
   `(markdown-header-face-6 ((,class (:foreground ,sec-dark :weight bold))))
   `(markdown-code-face ((,class (:foreground ,prim))))
   `(markdown-link-face ((,class (:foreground ,sec))))
   `(markdown-url-face ((,class (:foreground ,sec))))
   `(markdown-plain-url-face ((,class (:foreground ,sec))))
   ;;
   ;; treemacs
   ;;

   `(treemacs-root-face ((,class (:foreground ,sec-dark :inherit bold))))
   `(treemacs-directory-face ((,class (:foreground ,sec-dark))))
   `(treemacs-file-face ((,class (:inherit immaterial-small-face))))
   `(treemacs-term-node-face ((,class (:foreground ,sec-dark :weight bold))))
   `(treemacs-git-modified-face ((,class (:foreground ,fg1 :weight bold))))
   `(treemacs-git-added-face ((,class (:foreground ,prim-dark :weight bold))))
   `(treemacs-git-renamed-face ((,class (:foreground ,prim-dark :italic t))))
   `(treemacs-git-ignored-face ((,class (:foreground ,discrete))))
   `(treemacs-git-untracked-face ((,class (:foreground ,discrete))))
   `(treemacs-git-conflict-face ((,class (:foreground ,error :weight bold))))
   ))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'immaterial)

;;; immaterial-theme.el ends here
