;;; immaterial-theme.el --- A flexible theme based on material design principles -*- lexical-binding: t -*-

;; Copyright (C) 2019-2020 Peter Gardfjäll

;; Author: Peter Gardfjäll
;; Keywords: themes
;; URL: https://github.com/petergardfjall/emacs-immaterial-theme
;; Version: 0.7.1
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

;; To use the theme, put any of the following two lines in your Emacs
;; configuration file:
;;
;;   (load-theme 'immaterial-dark t)    ;; dark variant
;;   (load-theme 'immaterial-light t)   ;; light variant
;;
;; Requirements: Emacs 25.
;;

;;; Code:

(defface immaterial-small-face
  '((t :height 0.95))
  "Face that can be used via :inherit on faces that should have a smaller font size."
  :group 'immaterial-faces)

(defvar immaterial-color-override-alist
  '(())
  "Values provided here will override values in immaterial-color-alist.
The material color tool https://material.io/resources/color/ is
recommended for constructing primary and secondary color
schemes.")

(defvar immaterial-color-alist nil
  "Color alist to be used to define the different faces of the theme variant.
Initialized when either of the theme variants is loaded.")


;; Tip: enable rainbow-mode to preview the colors.
(defun immaterial-create-color-alist (variant)
  "Create a color palette for a given VARIANT of the theme.
VARIANT can either be 'dark of 'light.  Values can be overridden
via immaterial-color-override-alist).  The palette was created
using the https://material.io/resources/color/ tool."
  `(("background-primary"    . ,(if (eq variant 'dark) "#012027" "#fdfefe"))
    ("background-off"        . ,(if (eq variant 'dark) "#001b21" "#f9fafa"))
    ("background-on"         . ,(if (eq variant 'dark) "#01343f" "#f3f3ec"))
    ("foreground-primary"    . ,(if (eq variant 'dark) "#dddddd" "#222222"))
    ("foreground-secondary"  . ,(if (eq variant 'dark) "#c8c8c8" "#333333"))
    ("foreground-tertiary"   . ,(if (eq variant 'dark) "#aaaaaa" "#444444"))
    ("primary"               . ,(if (eq variant 'dark) "#b39ddb" "#4527a0"))
    ("primary-light"         . ,(if (eq variant 'dark) "#e6ceff" "#7953d2"))
    ("primary-dark"          . ,(if (eq variant 'dark) "#836fa9" "#000070"))
    ("secondary"             . ,(if (eq variant 'dark) "#c5e1a5" "#33691e"))
    ("secondary-light"       . ,(if (eq variant 'dark) "#f8ffd7" "#85bb5c"))
    ("secondary-dark"        . ,(if (eq variant 'dark) "#94af76" "#255d00"))
    ("tertiary"              . ,(if (eq variant 'dark) "#90caf9" "#0d47a1"))
    ("tertiary-light"        . ,(if (eq variant 'dark) "#c3fdff" "#5472d3"))
    ("tertiary-dark"         . ,(if (eq variant 'dark) "#5d99c6" "#002171"))

    ;; colors to use for popup-like UI behavior such as `company-mode`
    ;; completions, ivy/counsel, and lsp-ui.
    ("popup-bg-border"        . ,(if (eq variant 'dark) "#024858" "#e3e3d3"))
    ("popup-bg-prim"          . ,(if (eq variant 'dark) "#012830" "#f8f8f4"))
    ("popup-bg-on"            . ,(if (eq variant 'dark) "#01343e" "#fdfdfd"))


    ("error"                 . ,(if (eq variant 'dark) "#ef9a9a" "#b00202"))
    ("warning"               . ,(if (eq variant 'dark) "#ff9800" "#bf360c"))
    ("discrete"              . ,(if (eq variant 'dark) "#848484" "#757575"))
    ("vertical-border"       . ,(if (eq variant 'dark) "#001b21" "#f8f8f4"))
    ("cursor"                . ,(if (eq variant 'dark) "#64d8cb" "#64d8cb"))
    ("modeline-active-fg"    . ,(if (eq variant 'dark) "#ffffff" "#ffffff"))
    ("modeline-active-bg"    . ,(if (eq variant 'dark) "#005662" "#007b94"))
    ("modeline-inactive-fg"  . ,(if (eq variant 'dark) "#777777" "#9e9e9e"))
    ("modeline-inactive-bg"  . ,(if (eq variant 'dark) "#001017" "#f9fafa"))
    ;; various task-specific colors
    ("diff-added"            . ,(if (eq variant 'dark) "#063314" "#e6ffed"))
    ("diff-added-refined"    . ,(if (eq variant 'dark) "#115c24" "#acf2bd"))
    ("diff-removed"          . ,(if (eq variant 'dark) "#450a0d" "#ffeef0"))
    ("diff-removed-refined"  . ,(if (eq variant 'dark) "#7f1217" "#fdb8c0"))
    ("diff-changed"          . ,(if (eq variant 'dark) "#07275a" "#e1f0fe"))
    ("diff-changed-refined"  . ,(if (eq variant 'dark) "#0c4296" "#a8d3ff"))
    ))


(defun immaterial-color (color-name)
  "Retrieves the hex color value registered for a ´COLOR-NAME´.
The overrides in immaterial-color-override-alist take precedence
over the default ones defined in immaterial-color-alist."
  (let ((colmap (append immaterial-color-override-alist immaterial-color-alist)))
    (cdr (assoc color-name colmap))))


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



(defun immaterial-create-theme (name variant)
  "Initialize immaterial-theme for the given NAME and VARIANT.
NAME and VARIANT should be symbols."
  (progn
    (setq immaterial-color-alist (immaterial-create-color-alist variant))
    ;; not sure why this isn't a custom face in lsp-ui-doc
    (setq lsp-ui-doc-border (immaterial-color "popup-bg-border"))
    (let ((class '((class color) (min-colors 89)))
	  (fg1                  (immaterial-color "foreground-primary"))
	  (fg2                  (immaterial-color "foreground-secondary"))
	  (fg3                  (immaterial-color "foreground-tertiary"))
	  (bg-prim              (immaterial-color "background-primary"))
	  (bg-on                (immaterial-color "background-on"))
	  (bg-off               (immaterial-color "background-off"))
	  (prim                 (immaterial-color "primary"))
	  (prim-light           (immaterial-color "primary-light"))
	  (prim-dark            (immaterial-color "primary-dark"))
	  (sec                  (immaterial-color "secondary"))
	  (sec-light            (immaterial-color "secondary-light"))
	  (sec-dark             (immaterial-color "secondary-dark"))
	  (tert                 (immaterial-color "tertiary"))
	  (tert-light           (immaterial-color "tertiary-light"))
	  (tert-dark            (immaterial-color "tertiary-dark"))
	  (discrete             (immaterial-color "discrete"))

	  (popup-bg-border      (immaterial-color "popup-bg-border"))
	  (popup-bg-prim        (immaterial-color "popup-bg-prim"))
	  (popup-bg-on          (immaterial-color "popup-bg-on"))

	  (keyword              (immaterial-color "primary"))
	  (builtin              (immaterial-color "primary"))
	  (const                (immaterial-color "primary"))
	  (type                 (immaterial-color "foreground-primary"))
	  (var                  (immaterial-color "secondary"))
	  (func                 (immaterial-color "foreground-primary"))
	  (str                  (immaterial-color "tertiary"))
	  (comment              (immaterial-color "discrete"))
	  (negation             (immaterial-color "warning"))
	  (warning              (immaterial-color "warning"))
	  (error                (immaterial-color "error"))
	  (cursor               (immaterial-color "cursor"))

	  (v-border             (immaterial-color "vertical-border"))
	  (modeline-active-bg   (immaterial-color "modeline-active-bg"))
	  (modeline-active-fg   (immaterial-color "modeline-active-fg"))
	  (modeline-inactive-bg (immaterial-color "modeline-inactive-bg"))
	  (modeline-inactive-fg (immaterial-color "modeline-inactive-fg"))

	  (diff-added           (immaterial-color "diff-added"))
	  (diff-added-refined   (immaterial-color "diff-added-refined"))
	  (diff-changed         (immaterial-color "diff-changed"))
	  (diff-changed-refined (immaterial-color "diff-changed-refined"))
	  (diff-removed         (immaterial-color "diff-removed"))
	  (diff-removed-refined (immaterial-color "diff-removed-refined")))

      (custom-theme-set-faces
       name
       `(default ((,class (:background ,bg-prim :foreground ,fg1))))
       ;;
       ;; Syntax higlighting/font-lock minor mode. (syntax rules are provided by
       ;; the particular major-mode).
       ;;

       ;; for a keyword with special syntactic significance, like ‘if’.
       `(font-lock-keyword-face ((,class (:foreground ,keyword))))
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
       `(font-lock-comment-face ((,class (:foreground ,comment :slant italic))))
       ;; for comment delimiters, like ‘/*’ and ‘*/’ in C.
       `(font-lock-comment-delimiter-face ((,class (:foreground ,comment :slant italic))))
       ;; for documentation strings in the code.
       `(font-lock-doc-face ((,class (:foreground ,comment :slant italic))))

       ;; for easily-overlooked negation characters.
       `(font-lock-negation-char-face ((,class (:foreground ,negation))))
       ;; for a construct that is peculiar, or that greatly changes the meaning of
       ;; other text, like ‘;;;###autoload’ in Emacs Lisp and ‘#error’ in C.
       `(font-lock-warning-face ((,class (:foreground ,warning :background ,bg-on))))

       ;;
       ;; Buttons and links
       ;;
       `(button ((,class (:foreground ,tert))))
       ;; face for unvisited links
       `(link ((,class (:foreground ,tert))))
       `(link-visited ((,class (:foreground ,tert))))

       ;;
       ;; region selection
       ;;
       `(region ((,class (:background ,bg-on))))
       ;; used for secondary selections and selected date/time in org-mode
       `(secondary-selection ((,class (:background ,bg-on :foreground ,sec-dark))))
       ;; face used for text highlighting in various contexts (e.g. ivy search)
       `(highlight ((,class (:background ,bg-on :foreground ,fg2 :extend t))))
       ;; hl-line-mode background
       `(hl-line ((,class (:background ,bg-on :extend t))))
       ;; linum-mode column
       `(linum ((t (:foreground ,discrete :background ,bg-prim :height 0.8 :weight normal))))
       ;; display-line-numbers-mode (emacs26+)
       `(line-number ((t (:foreground ,discrete :background ,bg-off :height 0.8 :weight normal))))
       `(line-number-current-line ((t (:foreground ,fg1 :background ,bg-off :height 0.8 :weight normal))))
       `(fringe ((,class (:background ,bg-off))))
       `(cursor ((,class (:background ,cursor))))
       ;; show-paren-mode: how to highlight matching/mismatching parenthesis
       `(show-paren-match ((,class (:weight bold :background ,bg-on :foreground ,warning))))
       `(show-paren-mismatch ((,class (:background ,error))))
       ;; current match of an on-going incremental search (isearch-forward)
       `(isearch ((,class (:foreground ,warning :weight semi-bold))))
       ;; other matches for the search string that are visible on display
       `(lazy-highlight ((,class (:background ,bg-on :foreground ,warning))))
       ;;
       ;; mode-line
       ;;
       ;; mode-line of the active buffer (e.g. in case of split window)
       `(mode-line ((,class (:background ,modeline-active-bg :foreground ,modeline-active-fg))))
       ;; mode-line of the inactive buffer (e.g. in case of split window)
       `(mode-line-inactive  ((,class (:background ,modeline-inactive-bg :foreground ,modeline-inactive-fg))))
       `(mode-line-buffer-id ((,class (:weight bold))))

       ;;
       ;; powerline
       ;;
       ;; for active buffer in the frame
       `(powerline-active1 ((,class (:background ,modeline-active-bg :foreground ,modeline-active-fg))))
       `(powerline-active2 ((,class (:background ,modeline-active-bg :foreground ,modeline-active-fg))))
       ;; for inactive buffers in the frame
       `(powerline-inactive1 ((,class (:background ,modeline-inactive-bg :foreground ,modeline-inactive-fg))))
       `(powerline-inactive2 ((,class (:background ,modeline-inactive-bg :foreground ,modeline-inactive-fg))))

       ;; the vertical line that separates windows in a frame
       `(vertical-border ((,class (:foreground ,bg-off))))
       `(minibuffer-prompt ((,class (:foreground ,warning :weight semi-bold))))
       `(default-italic ((,class (:italic t))))

       `(gnus-header-content ((,class (:foreground ,prim))))
       `(gnus-header-from ((,class (:foreground ,sec-dark))))
       `(gnus-header-name ((,class (:foreground ,sec))))
       `(gnus-header-subject ((,class (:foreground ,sec-dark :bold t))))
       `(warning ((,class (:foreground ,warning))))
       `(ac-completion-face ((,class (:underline t :foreground ,prim))))
       `(info-quoted-name ((,class (:foreground ,prim-light))))
       `(info-string ((,class (:foreground ,prim))))
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
       ;;
       ;; ansi-term/term: set up colors that work well with the theme at large
       ;;
       `(term-default-fg-color ((,class (:foreground ,fg1, :background ,bg-prim))))
       `(term-default-bg-color ((,class (:foreground ,fg1 :background ,bg-prim))))
       `(term-color-red        ((,class (:foreground ,error :background ,bg-prim))))
       `(term-color-blue       ((,class (:foreground ,prim-dark))))
       `(term-color-yellow     ((,class (:foreground ,prim))))
       `(term-color-magenta    ((,class (:foreground ,prim-light))))
       `(term-color-black      ((,class (:foreground ,sec-dark))))
       `(term-color-green      ((,class (:foreground ,sec))))
       `(term-color-cyan       ((,class (:foreground ,sec-light))))
       `(term-color-white      ((,class (:foreground ,fg1))))
       ;;
       ;; company -- "complete any" completion engine
       ;;
       ;; Face used for the common part of completions in the echo area (appears
       ;; to only be used with the echo area frontend).
       `(company-echo-common ((,class (:foreground ,warning))))
       ;; scrollbar style in company tooltip
       `(company-scrollbar-bg ((,class (:background ,popup-bg-prim))))
       `(company-scrollbar-fg ((,class (:background ,popup-bg-on))))
       ;; ;; general style of tooltip popup candidate list
       `(company-tooltip ((,class (:foreground ,discrete :background ,popup-bg-prim))))
       ;; ;; annotation appearance (right-hand side text; could be the signature of a function)
       `(company-tooltip-annotation ((,class (:foreground ,sec :italic t))))
       ;; the style to use for showing the common matched search prefix in candidates
       `(company-tooltip-common ((,class (:foreground ,warning))))
       ;; the style to use for showing the common matched search prefix in the *selected* candidate
       `(company-tooltip-common-selection ((,class (:foreground ,warning :bold t))))
       ;; style to use to highlight the *selected* candidate
       `(company-tooltip-selection ((,class (:foreground ,fg1 :background ,popup-bg-on))))
       ;; annotation (i.e. RHS) appearance for the *selected* item in the completion list
       `(company-tooltip-annotation-selection ((,class (:inherit company-tooltip-annotation :bold nil))))
       ;; style to use for candidate over which mouse pointer is hovering
       `(company-tooltip-mouse ((,class (:inherit highlight))))
       ;; when using `company-search-mode` this is the face to use for the
       ;; matches of the entered search phrase
       `(company-tooltip-search ((,class (:bold t :foreground ,warning))))
       ;; same as above but for the *selected* candidate
       `(company-tooltip-search-selection ((,class (:inherit company-tooltip-selection))))


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
       ;; face to use for leading #:s
       `(markdown-header-delimiter-face ((,class (:foreground ,prim :weight bold))))
       `(markdown-header-face-1 ((,class (:foreground ,prim :weight bold))))
       `(markdown-header-face-2 ((,class (:foreground ,prim :weight bold))))
       `(markdown-header-face-3 ((,class (:foreground ,prim :weight bold))))
       `(markdown-header-face-4 ((,class (:foreground ,prim :weight bold))))
       `(markdown-header-face-5 ((,class (:foreground ,prim :weight bold))))
       `(markdown-header-face-6 ((,class (:foreground ,prim :weight bold))))
       `(markdown-code-face ((,class (:foreground ,sec))))
       `(markdown-table-face ((,class (:foreground ,sec))))
       `(markdown-list-face ((,class (:foreground ,sec))))
       `(markdown-link-face ((,class (:foreground ,fg1))))
       `(markdown-reference-face ((,class (:foreground ,tert))))
       `(markdown-blockquote-face ((,class (:inherit font-lock-doc-face))))
       `(markdown-html-tag-face ((,class (:foreground ,sec))))
       `(markdown-url-face ((,class (:foreground ,tert))))
       `(markdown-plain-url-face ((,class (:foreground ,tert))))

       ;;
       ;; treemacs
       ;;
       `(treemacs-root-face ((,class (:foreground ,sec-dark :inherit bold))))
       `(treemacs-directory-face ((,class (:foreground ,sec-dark))))
       `(treemacs-file-face ((,class (:inherit immaterial-small-face))))
       `(treemacs-term-node-face ((,class (:foreground ,sec-dark :weight bold))))
       `(treemacs-git-modified-face ((,class (:background ,diff-changed :box (:line-width 1 :color ,diff-changed-refined :style nil)))))
       `(treemacs-git-added-face ((,class (:background ,diff-added :box (:line-width 1 :color ,diff-added-refined :style nil)))))
       `(treemacs-git-renamed-face ((,class (:background ,diff-changed :box (:line-width 1 :color ,diff-changed-refined :style nil) :italic t))))
       `(treemacs-git-ignored-face ((,class (:foreground ,discrete))))
       `(treemacs-git-untracked-face ((,class (:foreground ,discrete))))
       `(treemacs-git-conflict-face ((,class (:background ,diff-removed :box (:line-width 1 :color ,diff-removed-refined :style nil) :italic t))))

       ;;
       ;; lsp-ui
       ;;
       ;; ui-doc popup
       `(lsp-ui-doc-background ((,class (:background ,popup-bg-prim))))

       ;; TODO: what's this?
       ;; `(lsp-face-highlight-textual ((,class (:inherit highlight :bold t :foreground "red"))))

       ;;
       ;; lsp-ui-peek
       ;;
       ;; face to use for the file being peeked (to the left)
       `(lsp-ui-peek-peek ((,class (:background ,popup-bg-prim))))
       ;; ;; face to use for the peek file listing (to the right)
       `(lsp-ui-peek-list ((,class (:background ,popup-bg-prim))))
       ;; face for current selection in peek file listing (to the right). note:
       ;; only background face attribute appears to work.
       `(lsp-ui-peek-selection ((,class (:background ,popup-bg-on))))
       ;; face for file names in file listing (to the right)
       `(lsp-ui-peek-filename ((,class (:foreground ,discrete))))
       ;; face for the type/object being peeked at in listing to the right
       `(lsp-ui-peek-highlight ((,class (:foreground ,warning :bold t))))
       ;; face for line numbers in listing to the right
       `(lsp-ui-peek-line-number ((,class (:foreground ,discrete))))
       ;; face for header line above entire peek frame
       `(lsp-ui-peek-header ((,class (:foreground ,discrete :background ,popup-bg-prim :weight semi-bold ))))
       ;; face for footer line below entire peek frame
       `(lsp-ui-peek-footer ((,class (:background ,popup-bg-prim))))

       ;;
       ;; ido
       ;;
       `(ido-first-match ((,class (:weight bold))))
       `(ido-only-match ((,class (:weight bold))))
       `(ido-subdir ((,class (:foreground ,sec-dark))))

       ;;
       ;; ivy/swiper
       ;;
       ;; highlight current match under cursor
       `(ivy-current-match ((,class (:weight semi-bold :background ,bg-on :extend t))))
       ;; highlight match under mouse pointer
       `(ivy-minibuffer-match-highlight ((,class (:inherit highlight))))
       ;; how to highlight the matching part of the search expression on presented
       ;; search candidates in the minibuffer.
       ;; The background face for ‘ivy’ minibuffer matches.
       `(ivy-minibuffer-match-face-1 ((,class (:inherit isearch))))
       ;; Face for ‘ivy’ minibuffer matches numbered 1 modulo 3.
       `(ivy-minibuffer-match-face-2 ((,class (:inherit isearch))))
       ;; Face for ‘ivy’ minibuffer matches numbered 2 modulo 3.
       `(ivy-minibuffer-match-face-3 ((,class (:inherit isearch))))
       ;; Face for ‘ivy’ minibuffer matches numbered 3 modulo 3.
       `(ivy-minibuffer-match-face-4 ((,class (:inherit isearch))))
       ;; ivy information for grep-like searches (such as `counsel-ag`)
       `(ivy-grep-info ((,class (:foreground ,sec-dark))))
       `(ivy-grep-line-number ((,class (:foreground ,sec-dark))))

       ;;
       ;; ivy-posframe
       ;;
       `(ivy-posframe ((,class (:background ,popup-bg-prim))))
       `(ivy-posframe-border ((,class (:background ,popup-bg-border))))

       ;;
       ;; org-mode
       ;;
       ;; face to use for #+TITLE: document info keyword
       `(org-document-title ((,class (:foreground ,prim :weight bold))))
       ;; face to use for value following #+DATE:, #+AUTHOR:, #+EMAIL:
       `(org-document-info ((,class (:foreground ,prim))))
       ;; face to use for keywords #+DATE:, #+AUTHOR:, #+EMAIL:
       `(org-document-info-keyword ((,class (:foreground ,prim))))
       ;; face for lines starting with "#+"
       `(org-meta-line ((,class (:foreground ,discrete))))
       ;; face used for headlines at different levels
       `(org-level-1 ((,class (:weight semi-bold :foreground ,prim))))
       `(org-level-2 ((,class (:weight semi-bold :foreground ,prim))))
       `(org-level-3 ((,class (:weight semi-bold :foreground ,prim))))
       `(org-level-4 ((,class (:weight semi-bold :foreground ,prim))))
       `(org-level-5 ((,class (:weight semi-bold :foreground ,prim))))
       `(org-level-6 ((,class (:weight semi-bold :foreground ,prim))))
       `(org-level-7 ((,class (:weight semi-bold :foreground ,prim))))
       `(org-level-8 ((,class (:weight semi-bold :foreground ,prim))))
       ;; face used to indicate that a headline is DONE.
       `(org-headline-done ((,class (:weight semi-bold :foreground ,discrete))))
       ;; face for the ellipsis in folded text
       `(org-ellipsis ((,class (:foreground ,tert))))
       ;; face to use for TODO keyword
       `(org-todo ((,class (:weight bold :foreground ,prim-dark :box (:line-width -1 :color ,bg-on)))))
       ;; face to use for DONE keyword
       `(org-done ((,class (:weight bold :foreground ,discrete :box (:line-width -1 :color ,bg-on)))))
       ;; face to use for :tag: markers
       `(org-tag ((,class (:foreground ,prim-light))))
       ;; face used for priority cookies `[#A]`
       `(org-priority ((,class (:foreground ,prim-light :weight bold))))
       ;; face for special keywords such as SCHEDULED, DEADLINE and properties.
       `(org-special-keyword ((,class (:foreground ,discrete))))
       ;; face used for outline metadata :DRAWER: and :END: markers
       `(org-drawer ((,class (:foreground ,discrete))))
       ;; face for org-mode tables
       `(org-table ((,class (:foreground ,sec))))
       ;; face used for [[links][description]]
       `(org-link ((,class (:foreground ,tert))))
       ;; face used for footnotes: [fn:1]
       `(org-footnote  ((,class (:foreground ,tert))))
       ;; face for =verbatim= items
       `(org-verbatim ((,class (:foreground ,sec))))
       ;; face for ~code~ text
       `(org-code ((,class (:foreground ,sec))))
       ;; diary-like sexp date specifications like `%%(org-calendar-holiday)`
       `(org-sexp-date ((,class (:foreground ,prim-light))))
       ;; face to use for content between #+BEGIN_SRC and #+END_SRC (unless a
       ;; language syntax is specified via e.g. `#BEGIN_SRC emacs_lisp`)
       `(org-block ((,class (:background ,bg-prim :foreground ,sec :extend t))))
       ;; source code block #+BEGIN_SRC line
       `(org-block-begin-line ((,class (:background ,bg-prim :foreground ,sec :extend t))))
       ;; source code block #+END_SRC line
       `(org-block-end-line ((,class (:background ,bg-prim :foreground ,sec :extend t))))
       ;; face for #+BEGIN_VERSE blocks when `org-fontify-quote-and-verse-blocks` is set.
       `(org-verse ((,class (:slant italic))))
       ;; face for #+BEGIN_QUOTE blocks when `org-fontify-quote-and-verse-blocks` is set.
       `(org-quote ((,class (:slant italic))))
       ;; face to use for <date> occurences
       `(org-date ((,class (:foreground ,sec-dark))))
       ;; face for highlighting date under cursor in calendar selections
       `(org-date-selected ((,class (:foreground ,fg1 :background ,diff-added :box (:line-width -1 :color ,diff-added-refined)))))

       ;; face for Monday-Friday entries in agenda view
       `(org-agenda-date ((,class (:foreground ,sec-dark))))
       ;; face for today in agenda view
       `(org-agenda-date-today ((,class (:foreground ,sec :weight bold :extend t :background ,bg-on))))
       ;; face for Saturday and Sunday entries in agenda view
       `(org-agenda-date-weekend ((,class (:foreground ,discrete))))
       ;; face used in agenda to indicate lines switched to DONE
       `(org-agenda-done ((,class (:foreground ,discrete))))
       ;; face used in agenda for captions and dates
       `(org-agenda-structure ((,class (:inherit bold :foreground ,sec-dark))))
       ;; face used for time grid shown in agenda
       `(org-time-grid ((,class (:foreground ,sec))))
       ;; agenda face for items scheduled for a certain day
       `(org-scheduled ((,class (:foreground ,fg1))))
       ;; agenda face for items scheduled today
       `(org-scheduled-today ((,class (:foreground ,fg1))))
       ;; agenda face for items scheduled previously, and not yet done.
       `(org-scheduled-previously ((,class (:foreground ,fg1))))
       ;; face used for org-agenda deadlines
       `(org-warning ((,class (:foreground ,error))))
       ;; upcoming deadlines
       `(org-upcoming-deadline ((,class (:foreground ,warning))))
       ;; distant deadlines
       `(org-upcoming-distant-deadline ((,class (:foreground ,warning))))
       ;; face when header-line is used
       `(header-line ((,class (:background ,bg-on :foreground ,prim :weight bold))))
       ;; face to use for column view columns
       `(org-column ((,class (:background ,bg-on))))
       ;; face to use for top-row in column view
       `(org-column-title ((,class (:inherit header-line))))
       ;; face for clock display overrun tasks in mode line
       `(org-mode-line-clock-overrun ((,class (:foreground ,error))))

       ;;
       ;; diff-mode
       ;;
       ;; used to highlight file header lines in diffs
       `(diff-file-header ((,class (:foreground ,prim :weight bold))))
       `(diff-header ((,class (:foreground ,discrete))))
       ;; used to highlight function names produced by `diff -p`
       `(diff-function ((,class (:foreground ,discrete))))
       ;; used to highlight added lines
       `(diff-added ((,class (:background ,(immaterial-color "diff-added") :extend t))))
       ;; face used for added characters shown by ‘diff-refine-hunk’.
       `(diff-refine-added ((,class (:background ,(immaterial-color "diff-added-refined")))))
       ;; used to highlight indicator of added lines (+, >)
       `(diff-indicator-added ((,class (:background ,(immaterial-color "diff-added")))))
       ;; used to highlight added lines
       `(diff-removed ((,class (:background ,(immaterial-color "diff-removed")))))
       ;; face used for removed characters shown by ‘diff-refine-hunk’.
       `(diff-refine-removed ((,class (:background ,(immaterial-color "diff-removed-refined")))))
       ;; used to highlight indicator of changed lines (-, <)
       `(diff-indicator-removed ((,class (:background ,(immaterial-color "diff-removed")))))
       ;; face used to highlight changed lines
       `(diff-changed ((,class (:background ,(immaterial-color "diff-changed")))))
       ;; face used for char-based changes shown by ‘diff-refine-hunk’.
       `(diff-refine-changed ((,class (:background ,(immaterial-color "diff-changed-refined")))))
       ;; used to highlight indicator of changed lines
       `(diff-indicator-changed ((,class (:background ,(immaterial-color "diff-changed") :foreground ,(immaterial-color "diff-changed-refined")))))

       ;;
       ;; diff-hl
       ;;
       `(diff-hl-insert ((,class (:background ,(immaterial-color "diff-added") :foreground ,(immaterial-color "diff-added-refined")))))
       `(diff-hl-delete ((,class (:background ,(immaterial-color "diff-removed") :foreground ,(immaterial-color "diff-removed-refined")))))
       `(diff-hl-change ((,class (:background ,(immaterial-color "diff-changed") :foreground ,(immaterial-color "diff-changed-refined")))))

       ;;
       ;; smerge-mode
       ;;
       ;; face for conflict markers
       `(smerge-markers ((,class (:foreground ,error :weight bold))))
       ;; face for upper version in conflict
       `(smerge-upper ((,class (:background ,diff-changed))))
       ;; face for lower version in conflict
       `(smerge-lower ((,class (:background ,diff-changed))))
       ;; face for added characters shown by smerge-refine
       `(smerge-refined-added ((,class (:background ,(immaterial-color "diff-added-refined")))))
       ;; face for removed characters shown by smerge-refine
       `(smerge-refined-removed ((,class (:background ,(immaterial-color "diff-removed-refined")))))

       ;;
       ;; xref
       ;;
       `(xref-file-header ((,class (:foreground ,prim))))
       `(xref-line-number ((,class (:foreground ,discrete))))

       ;;
       ;; compilation mode
       ;;
       ;; face used to highlight compiler information (and commonly in other
       ;; moodes to display grep-like output such as file names)
       `(compilation-info ((,class (:foreground ,prim))))
       ;; face used to highlight compiler line numbers (and commonly in other
       ;; moodes to display grep-like output such as file line numbers)
       `(compilation-line-number ((,class (:foreground ,discrete))))


       ;;
       ;; completion (minibuffer.el)
       ;;
       ;; face for the parts of completions which matched the pattern
       `(completions-common-part ((,class (:foreground ,warning))))
       `(completions-annotations ((,class (:inherit font-lock-comment-face))))
       ;; face for the first character after point in completions
       `(completions-first-difference ((,class (:foreground ,error :weight bold))))
       ))))


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
