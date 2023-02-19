;;; immaterial-theme.el --- A flexible theme based on material design principles -*- lexical-binding: t -*-

;; Copyright (C) 2019-2020 Peter Gardfjäll

;; Author: Peter Gardfjäll
;; Keywords: themes
;; URL: https://github.com/petergardfjall/emacs-immaterial-theme
;; Version: 0.9.1
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

(require 'cl-lib)

(defface immaterial-small-face
  '((t :height 0.95))
  "Face that can be used via :inherit on faces that should have a smaller font size."
  :group 'immaterial-faces)

(defvar immaterial-color-override-alist
  '(())
  "Values provided here will override values in immaterial-color-alist.
The material color tool https://material.io/resources/color/ can
be helpful when constructing primary, secondary, tertiary color
schemes.")

(defvar immaterial-color-alist
  '(("background-primary"    . ((dark . "#012027") (light . "#fcfcfb")))
    ("background-off"        . ((dark . "#001b21") (light . "#f5f5f2")))
    ("background-on"         . ((dark . "#01343f") (light . "#f5f2ea")))
    ("foreground-primary"    . ((dark . "#dddddd") (light . "#333333")))
    ("foreground-secondary"  . ((dark . "#c8c8c8") (light . "#484848")))
    ("foreground-tertiary"   . ((dark . "#aaaaaa") (light . "#555555")))
    ("primary"               . ((dark . "#b39ddb") (light . "#4527a0")))
    ("primary-hi"            . ((dark . "#e6ceff") (light . "#000070")))
    ("primary-lo"            . ((dark . "#836fa9") (light . "#7953d2")))
    ("secondary"             . ((dark . "#c5e1a5") (light . "#295518")))
    ("secondary-hi"          . ((dark . "#f8ffd7") (light . "#19350f")))
    ("secondary-lo"          . ((dark . "#94af76") (light . "#4f7641")))
    ("tertiary"              . ((dark . "#90caf9") (light . "#0d47a1")))
    ("tertiary-hi"           . ((dark . "#c3fdff") (light . "#002171")))
    ("tertiary-lo"           . ((dark . "#5d99c6") (light . "#5472d3")))
    ;; colors to use for popup-like UI behavior such as `company-mode`and lsp-ui
    ("popup-bg-border"       . ((dark . "#01586c") (light . "#d9d9bc")))
    ("popup-bg-prim"         . ((dark . "#012830") (light . "#f8f8f4")))
    ("popup-bg-on"           . ((dark . "#014453") (light . "#f1ede3")))
    ("error"                 . ((dark . "#ef9a9a") (light . "#b00202")))
    ("warning"               . ((dark . "#ff9800") (light . "#bf360c")))
    ("match"                 . ((dark . "#ff9800") (light . "#bf360c")))
    ("discrete"              . ((dark . "#848484") (light . "#757575")))
    ("vertical-border"       . ((dark . "#001b21") (light . "#f8f8f4")))
    ("cursor"                . ((dark . "#64d8cb") (light . "#64d8cb")))
    ("modeline-active-fg"    . ((dark . "#ffffff") (light . "#ffffff")))
    ("modeline-active-bg"    . ((dark . "#005662") (light . "#6a9a5e")))
    ("modeline-active-border". ((dark . "#008295") (light . "#95dd84")))
    ("modeline-inactive-fg"  . ((dark . "#848484") (light . "#757575")))
    ("modeline-inactive-bg"  . ((dark . "#001017") (light . "#cfdfcb")))
    ;; various task-specific colors
    ("diff-added"            . ((dark . "#033521") (light . "#e6ffed")))
    ("diff-added-refined"    . ((dark . "#175b2b") (light . "#acf2bd")))
    ("diff-removed"          . ((dark . "#3b0f19") (light . "#ffebe9")))
    ("diff-removed-refined"  . ((dark . "#8d2323") (light . "#ffc0c0")))
    ("diff-changed"          . ((dark . "#082145") (light . "#e1f0fe")))
    ("diff-changed-refined"  . ((dark . "#103d7f") (light . "#a8d3ff")))
    "Defines the color palette for the different theme variants.
It is a two-level association list with the first level keys
being theme color elements and the second level associating a
theme variant, such as dark or light, with a hex color value."))

(defun immaterial--get-alist-color (color-alist color-name variant)
  "Get the COLOR-NAME for a theme VARIANT registered in a COLOR-ALIST.
If no such COLOR-NAME or VARIANT combination exists in
COLOR-ALIST, nil is returned."
  (cdr (assoc variant (cdr (assoc color-name color-alist)))))

(defun immaterial-color (color-name variant)
  "Retrieves the hex color value registered for a COLOR-NAME under a theme VARIANT.
The overrides in `immaterial-color-override-alist' take precedence
over the default ones defined in `immaterial-color-alist'."
  (or (immaterial--get-alist-color immaterial-color-override-alist color-name variant)
      (immaterial--get-alist-color immaterial-color-alist color-name variant)))

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
  (cl-flet ( (color-get (color-name) (immaterial-color color-name variant)) )
    (let ((class '((class color) (min-colors 89)))
	  (fg1                  (color-get "foreground-primary"))
	  (fg2                  (color-get "foreground-secondary"))
	  (fg3                  (color-get "foreground-tertiary"))
	  (bg-prim              (color-get "background-primary"))
	  (bg-on                (color-get "background-on"))
	  (bg-off               (color-get "background-off"))
	  (prim                 (color-get "primary"))
	  (prim-hi              (color-get "primary-hi"))
	  (prim-lo              (color-get "primary-lo"))
	  (sec                  (color-get "secondary"))
	  (sec-hi               (color-get "secondary-hi"))
	  (sec-lo               (color-get "secondary-lo"))
	  (tert                 (color-get "tertiary"))
	  (tert-hi              (color-get "tertiary-hi"))
	  (tert-lo              (color-get "tertiary-lo"))
	  (discrete             (color-get "discrete"))

	  (popup-bg-border      (color-get "popup-bg-border"))
	  (popup-bg-prim        (color-get "popup-bg-prim"))
	  (popup-bg-on          (color-get "popup-bg-on"))

	  (keyword              (color-get "primary"))
	  (builtin              (color-get "primary"))
	  (const                (color-get "primary"))
	  (type                 (color-get "foreground-primary"))
	  (var                  (color-get "secondary"))
	  (func                 (color-get "foreground-primary"))
	  (str                  (color-get "tertiary"))
	  (comment              (color-get "discrete"))
	  (negation             (color-get "warning"))
	  (warning              (color-get "warning"))
	  (match                (color-get "match"))
	  (error                (color-get "error"))
	  (cursor               (color-get "cursor"))

	  (v-border               (color-get "vertical-border"))
	  (modeline-active-bg     (color-get "modeline-active-bg"))
	  (modeline-active-border (color-get "modeline-active-border"))
	  (modeline-active-fg     (color-get "modeline-active-fg"))
	  (modeline-inactive-bg   (color-get "modeline-inactive-bg"))
	  (modeline-inactive-fg   (color-get "modeline-inactive-fg"))

	  (diff-added           (color-get "diff-added"))
	  (diff-added-refined   (color-get "diff-added-refined"))
	  (diff-changed         (color-get "diff-changed"))
	  (diff-changed-refined (color-get "diff-changed-refined"))
	  (diff-removed         (color-get "diff-removed"))
	  (diff-removed-refined (color-get "diff-removed-refined")))

      (custom-theme-set-variables
       name
       ;; note: this color vector controls the appearance of shell mode. It is
       ;; set up to mimic the term-color-* faces. ["black" "red3" "green3"
       ;; "yellow3" "blue2" "magenta3" "cyan3" "gray90"]
       `(ansi-color-names-vector (vector ,fg1 ,sec ,prim ,warning ,sec-lo ,tert ,error ,bg-prim))
       ;; not sure why this isn't a custom face in lsp-ui-doc
       `(lsp-ui-doc-border ,popup-bg-border))

      (custom-theme-set-faces
       name
       `(default ((,class (:background ,bg-prim :foreground ,fg1))))
       `(shadow ((,class (:foreground ,discrete))))
       `(match ((,class (:foreground ,match :weight semi-bold))))

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
       `(secondary-selection ((,class (:background ,bg-on :foreground ,sec-lo))))
       ;; face used for text highlighting in various contexts (e.g. ivy search)
       `(highlight ((,class (:background ,bg-on))))
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
       `(isearch ((,class (:inherit match))))
       ;; other matches for the search string that are visible on display
       `(lazy-highlight ((,class (:inherit isearch))))
       ;; for highlighting failed part in isearch echo-area message.
       `(isearch-fail ((,class (:foreground ,error :underline t))))
       ;;
       ;; mode-line
       ;;
       ;; mode-line of the active buffer (e.g. in case of split window)
       `(mode-line ((,class (:inherit default :background ,modeline-active-bg :foreground ,modeline-active-fg :box (:line-width 1 :color ,modeline-active-border :style nil) ))))
       ;; mode-line of the inactive buffer (e.g. in case of split window)
       `(mode-line-inactive  ((,class (:background ,modeline-inactive-bg :foreground ,modeline-inactive-fg))))
       `(mode-line-buffer-id ((,class (:weight bold))))
       ;;
       ;; buffer menu buffer (C-x C-b)
       ;;
       ;; face for buffer names in the buffer menu
       `(buffer-menu-buffer ((,class (:foreground ,sec))))
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
       `(gnus-header-from ((,class (:foreground ,sec-lo))))
       `(gnus-header-name ((,class (:foreground ,sec))))
       `(gnus-header-subject ((,class (:foreground ,sec-lo :bold t))))
       `(warning ((,class (:foreground ,warning))))
       `(ac-completion-face ((,class (:underline t :foreground ,prim))))
       `(info-quoted-name ((,class (:foreground ,prim-hi))))
       `(info-string ((,class (:foreground ,prim))))
       `(icompletep-determined ((,class :foreground ,prim-hi)))
       ;;
       ;; undo-tree
       ;;
       `(undo-tree-visualizer-current-face ((,class :foreground ,prim-hi)))
       `(undo-tree-visualizer-default-face ((,class :foreground ,fg2)))
       `(undo-tree-visualizer-unmodified-face ((,class :foreground ,sec-lo)))
       `(undo-tree-visualizer-register-face ((,class :foreground ,sec)))

       `(slime-repl-inputed-output-face ((,class (:foreground ,sec))))
       `(trailing-whitespace ((,class :background ,warning)))
       ;;
       ;; ansi-term/term/vterm
       ;;
       `(term-default-fg-color ((,class (:foreground ,fg1, :background ,bg-prim))))
       `(term-default-bg-color ((,class (:foreground ,fg1 :background ,bg-prim))))
       ;; used for most terminal text
       `(term-color-black      ((,class (:foreground ,fg1))))
       `(term-color-white      ((,class (:foreground ,bg-prim))))
       ;; for example used for directories
       `(term-color-blue       ((,class (:foreground ,sec-lo))))
       ;; for example used for symlinks
       `(term-color-cyan       ((,class (:foreground ,error))))
       ;; for example used for scripts (.sh)
       `(term-color-green      ((,class (:foreground ,prim))))
       ;; for example used for archives (zip, deb, ...)
       `(term-color-red        ((,class (:foreground ,sec))))
       `(term-color-yellow     ((,class (:foreground ,warning))))
       ;; for example used for media (images, audio, video)
       `(term-color-magenta    ((,class (:foreground ,tert))))
       ;;
       ;; company -- "complete any" completion engine
       ;;
       ;; Face used for the common part of completions in the echo area (appears
       ;; to only be used with the echo area frontend).
       `(company-echo-common ((,class (:inherit match))))
       ;; scrollbar style in company tooltip
       `(company-tooltip-scrollbar-track ((,class (:background ,popup-bg-on ))))
       `(company-tooltip-scrollbar-thumb ((,class (:background ,popup-bg-border))))
       ;; ;; general style of tooltip popup candidate list
       `(company-tooltip ((,class (:foreground ,discrete :background ,popup-bg-prim))))
       ;; ;; annotation appearance (right-hand side text; could be the signature of a function)
       `(company-tooltip-annotation ((,class (:foreground ,sec :italic t))))
       ;; the style to use for showing the common matched search prefix in
       ;; non-selected candidates
       `(company-tooltip-common ((,class (:inherit match :bold nil))))
       ;; the style to use for showing the common matched search prefix in the *selected* candidate
       `(company-tooltip-common-selection ((,class (:inherit match :bold t))))
       ;; style to use to highlight the *selected* candidate
       `(company-tooltip-selection ((,class (:foreground ,fg1 :background ,popup-bg-on))))
       ;; annotation (i.e. RHS) appearance for the *selected* item in the completion list
       `(company-tooltip-annotation-selection ((,class (:inherit company-tooltip-annotation :bold nil))))
       ;; style to use for candidate over which mouse pointer is hovering
       `(company-tooltip-mouse ((,class (:inherit highlight))))
       ;; when using `company-search-mode` this is the face to use for the
       ;; matches of the entered search phrase
       `(company-tooltip-search ((,class (:inherit match))))
       ;; same as above but for the *selected* candidate
       `(company-tooltip-search-selection ((,class (:inherit company-tooltip-selection))))

       ;;
       ;; corfu - completion-at-point UI
       ;;
       ;; background and foreground color to use for popup
       `(corfu-default ((,class (:background ,popup-bg-prim :foreground ,discrete))))
       ;; background color is used for the thin border around the popup
       `(corfu-border ((,class (:background ,popup-bg-border))))
       ;; face used to highlight the currently selected candidate
       `(corfu-current ((,class (:background ,popup-bg-on :foreground ,fg1))))
       ;; background color is used for scrollbar
       `(corfu-bar ((,class (:background ,popup-bg-border))))

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
       `(neo-root-dir-face ((,class (:foreground ,sec-lo :inherit bold))))
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
       `(treemacs-root-face ((,class (:foreground ,sec-lo :inherit bold))))
       `(treemacs-directory-face ((,class (:foreground ,sec-lo))))
       `(treemacs-file-face ((,class (:inherit immaterial-small-face))))
       `(treemacs-term-node-face ((,class (:foreground ,sec-lo :weight bold))))
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
       `(lsp-ui-peek-header ((,class (:foreground ,discrete :background ,popup-bg-prim :weight semi-bold  :overline ,popup-bg-border))))
       ;; face for footer line below entire peek frame
       `(lsp-ui-peek-footer ((,class (:background ,popup-bg-prim))))

       ;;
       ;; ido
       ;;
       `(ido-first-match ((,class (:weight bold))))
       `(ido-only-match ((,class (:weight bold))))
       `(ido-subdir ((,class (:foreground ,sec-lo))))

       ;;
       ;; consult
       ;;
       ;; face used if async process (e.g. search) has failed
       `(consult-async-failed ((,class (:inherit error :background ,bg-on))))
       ;; face used while async process (e.g. search) is running
       `(consult-async-running ((,class (:inherit warning :background ,bg-on))))
       ;; face used for marking cursor in a live preview of a search candidate
       `(consult-preview-cursor ((,class (:inherit match))))
       ;; face used to for match previews in ‘consult-grep’.
       `(consult-preview-match ((,class (:inherit match))))
       `(consult-line-number-wrapped ((,class (:inherit consult-line-number-prefix))))

       ;;
       ;; orderless
       ;;
       ;; match faces for search terms (modulo 4)
       `(orderless-match-face-0 ((,class (:inherit match))))
       `(orderless-match-face-1 ((,class (:inherit match))))
       `(orderless-match-face-2 ((,class (:inherit match))))
       `(orderless-match-face-3 ((,class (:inherit match))))

       ;;
       ;; vertico
       ;;
       `(vertico-group-title ((,class (:foreground ,discrete))))
       `(vertico-group-separator ((,class (:foreground ,popup-bg-border :strike-through t))))

       ;;
       ;; ivy/swiper
       ;;
       ;; highlight current match under cursor
       `(ivy-current-match ((,class (:weight semi-bold :background ,bg-on :extend t))))
       ;; highlight match under mouse pointer
       `(ivy-minibuffer-match-highlight ((,class (:inherit highlight))))
       ;; how to highlight first search term matches in candidate lines
       `(ivy-minibuffer-match-face-1 ((,class (:inherit match))))
       ;; how to highlight second search term matches in candidate lines
       `(ivy-minibuffer-match-face-2 ((,class (:inherit match))))
       ;; how to highlight third search term matches in candidate lines
       `(ivy-minibuffer-match-face-3 ((,class (:inherit match))))
       ;; how to highlight fourth search term matches in candidate lines
       `(ivy-minibuffer-match-face-4 ((,class (:inherit match))))
       ;; ivy information for grep-like searches (such as `counsel-ag`)
       `(ivy-grep-info ((,class (:foreground ,sec-lo))))
       `(ivy-grep-line-number ((,class (:foreground ,sec-lo))))

       ;; used to highlight search term matches for candidate line under cursor
       `(swiper-match-face-1 ((,class (:inherit (highlight match)))))
       `(swiper-match-face-2 ((,class (:inherit (highlight match)))))
       `(swiper-match-face-3 ((,class (:inherit (highlight match)))))
       `(swiper-match-face-4 ((,class (:inherit (highlight match)))))
       ;; used to highlight search term matches for candidate lines not under cursor
       `(swiper-background-match-face-1 ((,class (:inherit (highlight match)))))
       `(swiper-background-match-face-2 ((,class (:inherit (highlight match)))))
       `(swiper-background-match-face-3 ((,class (:inherit (highlight match)))))
       `(swiper-background-match-face-4 ((,class (:inherit (highlight match)))))

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
       `(org-todo ((,class (:weight bold :foreground ,prim-lo :box (:line-width -1 :color ,bg-on)))))
       ;; face to use for DONE keyword
       `(org-done ((,class (:weight bold :foreground ,discrete :box (:line-width -1 :color ,bg-on)))))
       ;; face to use for :tag: markers
       `(org-tag ((,class (:foreground ,prim-hi))))
       ;; face used for priority cookies `[#A]`
       `(org-priority ((,class (:foreground ,prim-hi :weight bold))))
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
       `(org-sexp-date ((,class (:foreground ,prim-hi))))
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
       `(org-date ((,class (:foreground ,sec-lo))))
       ;; face for highlighting date under cursor in calendar selections
       `(org-date-selected ((,class (:foreground ,fg1 :background ,diff-added :box (:line-width -1 :color ,diff-added-refined)))))

       ;; face for Monday-Friday entries in agenda view
       `(org-agenda-date ((,class (:foreground ,sec-lo))))
       ;; face for today in agenda view
       `(org-agenda-date-today ((,class (:foreground ,sec :weight bold :extend t :background ,bg-on))))
       ;; face for Saturday and Sunday entries in agenda view
       `(org-agenda-date-weekend ((,class (:foreground ,discrete))))
       ;; face used in agenda to indicate lines switched to DONE
       `(org-agenda-done ((,class (:foreground ,discrete))))
       ;; face used in agenda for captions and dates
       `(org-agenda-structure ((,class (:inherit bold :foreground ,sec-lo))))
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
       ;; face inherited by hunk and index header faces
       `(diff-header ((,class (:foreground ,fg1))))
       ;; used to highlight file header lines in diffs
       `(diff-file-header ((,class (:foreground ,prim :weight bold))))
       ;; face of context (text surrounding a hunk)
       `(diff-context ((,class (:foreground ,discrete))))
       ;; used to highlight function names produced by `diff -p`
       `(diff-function ((,class (:foreground ,fg1))))
       ;; used to highlight added lines
       `(diff-added ((,class (:background ,(color-get "diff-added") :extend t))))
       ;; face used for added characters shown by ‘diff-refine-hunk’.
       `(diff-refine-added ((,class (:background ,(color-get "diff-added-refined")))))
       ;; used to highlight indicator of added lines (+, >)
       `(diff-indicator-added ((,class (:background ,(color-get "diff-added")))))
       ;; used to highlight added lines
       `(diff-removed ((,class (:background ,(color-get "diff-removed")))))
       ;; face used for removed characters shown by ‘diff-refine-hunk’.
       `(diff-refine-removed ((,class (:background ,(color-get "diff-removed-refined")))))
       ;; used to highlight indicator of changed lines (-, <)
       `(diff-indicator-removed ((,class (:background ,(color-get "diff-removed")))))
       ;; face used to highlight changed lines
       `(diff-changed ((,class (:background ,(color-get "diff-changed")))))
       ;; face used for char-based changes shown by ‘diff-refine-hunk’.
       `(diff-refine-changed ((,class (:background ,(color-get "diff-changed-refined")))))
       ;; used to highlight indicator of changed lines
       `(diff-indicator-changed ((,class (:background ,(color-get "diff-changed") :foreground ,(color-get "diff-changed-refined")))))

       ;;
       ;; diff-hl
       ;;
       `(diff-hl-insert ((,class (:background ,(color-get "diff-added") :foreground ,(color-get "diff-added-refined")))))
       `(diff-hl-delete ((,class (:background ,(color-get "diff-removed") :foreground ,(color-get "diff-removed-refined")))))
       `(diff-hl-change ((,class (:background ,(color-get "diff-changed") :foreground ,(color-get "diff-changed-refined")))))

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
       `(smerge-refined-added ((,class (:background ,diff-added-refined))))
       ;; face for removed characters shown by smerge-refine
       `(smerge-refined-removed ((,class (:background ,diff-removed-refined))))

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

       ;; face to highlight leading space in Makefiles
       `(makefile-space ((,class (:background ,diff-removed))))

       ;;
       ;; completion (minibuffer.el)
       ;;
       ;; face for the parts of completions which matched the pattern
       `(completions-common-part ((,class (:inherit match))))
       `(completions-annotations ((,class (:foreground ,sec :italic t))))
       ;; face for the first character after point in completions
       `(completions-first-difference ((,class ()))) ;; no styling

       ;;
       ;; magit
       ;;
       `(magit-section-heading ((,class (:foreground ,prim :weight bold))))
       `(magit-section-secondary-heading ((,class (:foreground ,prim))))
       `(magit-section-heading-selection ((,class (:foreground ,prim-hi :weight bold :background ,bg-on))))
       `(magit-section-highlight ((,class (:background ,bg-on))))
       `(magit-header-line ((,class (:inherit header-line))))
       `(magit-tag ((,class (:foreground ,sec))))
       `(magit-hash ((,class (:foreground ,discrete))))
       `(magit-filename ((,class (:foreground ,fg1 :weight semi-bold))))
       ;; branch faces
       `(magit-branch-local ((,class (:foreground ,sec))))
       `(magit-branch-current ((,class (:foreground ,sec :background ,bg-on))))
       `(magit-branch-remote ((,class (:foreground ,tert))))
       `(magit-branch-remote-head ((,class (:foreground ,tert :background ,bg-on))))
       ;; NOTE: magit-diff-hunk-heading, should be different from both
       ;; magit-diff-hunk-heading-highlight and magit-section-highlight as well
       ;; as from magit-diff-context and magit-diff-context-highlight
       `(magit-diff-context ((,class (:foreground ,discrete :background ,bg-off))))
       `(magit-diff-context-highlight ((,class (:foreground ,discrete :background ,bg-on))))
       `(magit-diff-hunk-heading ((,class (:foreground ,discrete :background ,diff-changed))))
       `(magit-diff-hunk-heading-highlight ((,class (:foreground ,discrete :background ,diff-changed-refined))))
       `(magit-diff-hunk-heading-selection ((,class (:foreground ,fg1 :background ,diff-changed-refined))))
       `(magit-diff-added ((,class (:background ,diff-added :extend t))))
       `(magit-diff-added-highlight ((,class (:background ,diff-added-refined :extend t))))
       `(magit-diff-removed ((,class (:background ,diff-removed :extend t))))
       `(magit-diff-removed-highlight ((,class (:background ,diff-removed-refined))))
       ;; face for file names in diffs
       `(magit-diff-file-heading ((,class (:foreground ,fg3))))
       `(magit-diff-file-heading-highlight ((,class (:foreground ,fg1 :background ,bg-on :extend t))))
       ;; face to use when warning of trailing whitespace
       `(magit-diff-whitespace-warning ((,class (:background ,diff-removed-refined))))
       ;; NOTE: there are different `magit-blame` styles (heading, margin,
       ;; highlight, lines). These can be cycled with "c". Some of these faces
       ;; only apply to certain such styles.
       `(magit-blame-heading ((,class (:foreground ,fg1 :slant normal :background ,diff-changed))))
       `(magit-blame-highlight ((,class (:foreground ,fg1 :background ,diff-changed))))
       ;; face used in `magit-blame` "margin mode" (use 'c' to cycle)
       `(magit-blame-margin ((,class (:foreground ,fg1 :background ,diff-changed))))
       ;; faces for formatting blame lines
       `(magit-blame-name ((,class (:foreground ,fg1))))
       `(magit-blame-date ((,class (:foreground ,fg1))))
       `(magit-blame-summary ((,class (:foreground ,fg1))))
       `(magit-blame-hash ((,class (:foreground ,fg1))))
       `(magit-blame-dimmed ((,class (:foreground ,discrete))))
       ;; magit-log
       `(magit-log-graph ((,class (:foreground ,prim))))
       `(magit-log-author ((,class (:foreground ,tert))))
       `(magit-log-date ((,class (:foreground ,sec-lo))))
       ;; magit-reflog
       `(magit-reflog-amend ((,class (:foreground ,sec))))
       `(magit-reflog-cherry-pick ((,class (:foreground ,sec-hi))))
       `(magit-reflog-commit ((,class (:foreground ,sec-lo))))
       `(magit-reflog-merge ((,class (:foreground ,prim-lo))))
       `(magit-reflog-rebase ((,class (:foreground ,prim-hi))))
       `(magit-reflog-checkout ((,class (:foreground ,tert))))
       `(magit-reflog-remote ((,class (:foreground ,tert-lo))))
       `(magit-reflog-other ((,class (:foreground ,tert-hi))))
       `(magit-reflog-reset ((,class (:foreground ,error))))
       ;; face non-zero exit status
       `(magit-process-ng ((,class (:foreground ,error))))
       `(magit-process-ok ((,class (:foreground ,prim))))
       ;; magit-cherry
       ;; used for "+"
       `(magit-cherry-unmatched ((,class (:foreground ,sec))))
       ;; used for "-"
       `(magit-cherry-equivalent ((,class (:foreground ,tert))))
       ;; process status
       `(magit-mode-line-process ((,class (:foreground ,fg1))))
       `(magit-mode-line-process-error ((,class (:foreground ,error :weight bold))))
       ;;
       ;; dired
       ;;
       `(dired-directory ((,class (:foreground ,sec-lo))))
       ;; face used for files flagged for deletion
       `(dired-flagged ((,class (:foreground ,fg1 :background ,diff-removed))))
       `(dired-header ((,class (:foreground ,prim))))
       ;; face used for files suffixed with ‘completion-ignored-extensions’.
       `(dired-ignored ((,class (:foreground ,discrete))))
       ;; face used for asterisk of marked files
       `(dired-mark ((,class (:foreground ,warning :weight bold))))
       ;; face used for files marked
       `(dired-marked ((,class (:foreground ,fg1 :background ,diff-changed))))
       ;; face used to highlight permissions of group- and world-writable files.
       `(dired-perm-write ((,class (:foreground ,error))))
       ;; face used to highlight permissions of suid and guid files.
       `(dired-set-id ((,class (:foreground ,warning))))
       ;; face used for sockets, pipes, block devices and char devices.
       `(dired-special ((,class (:foreground ,tert-hi))))
       ;; face used for symlinks
       `(dired-symlink ((,class (:foreground ,error))))
       ;; face used to highlight broken symblinks
       `(dired-broken-symlink ((,class (:foreground ,fg1 :background ,diff-removed-refined))))
       `(dired-warning ((,class (:foreground ,warning :background ,bg-on))))
       ;;
       ;; keycast
       ;;
       ;; face for the last key combination
       `(keycast-key ((,class (:foreground ,warning :weight bold :height 1.1))))
       ;; face for the last command
       `(keycast-command ((,class (:foreground ,warning :weight bold :slant italic :height 1.1))))

       ;;
       ;; projtree
       ;;
       `(projtree-highlight ((,class (:inherit highlight))))
       `(projtree-file ((,class (:inherit default))))
       `(projtree-dir ((,class (:foreground ,sec-lo))))
       `(projtree-git-modified ((,class (:background ,diff-changed :box (:line-width 1 :color ,diff-changed-refined :style nil)))))
       `(projtree-git-added ((,class (:background ,diff-added :box (:line-width 1 :color ,diff-added-refined :style nil)))))
       `(projtree-git-ignored ((,class (:foreground ,discrete :strike-through t))))
       `(projtree-git-untracked ((,class (:foreground ,discrete))))
       `(projtree-git-conflict ((,class (:background ,diff-removed :box (:line-width 1 :color ,diff-removed-refined :style nil) :italic t))))))))


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
