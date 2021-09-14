[![MELPA](https://melpa.org/packages/immaterial-theme-badge.svg)](https://melpa.org/#/immaterial-theme)
[![MELPA Stable](https://stable.melpa.org/packages/immaterial-theme-badge.svg)](https://stable.melpa.org/#/immaterial-theme)


# Emacs immaterial-theme

*Note: for good results in a pure terminal environment, consider installing
Emacs 26 and [enable
truecolors](https://github.com/syl20bnr/spacemacs/wiki/Terminal) in your
terminal.*

Immaterial is an emacs color theme that comes in two flavors (`immateral-dark`
and `immaterial-light`) and is loosely based on the principles of Google's
[Material
design](https://material.io/design/color/the-color-system.html#color-theme-creation). More
specifically, it defines (and allows users to _redefine_) the following coloring
elements:

- `background`: used for the background of buffers, modeline, etc.

  Comes in three flavors: `background-primary`, `background-on`,
  `background-off`. Most of the background will be colored using
  `background-primary`. `background-on` is used for highlighting or attracting
  attention (such as the active modeline, search candidates,
  etc). `background-off` is used to tone down parts of the screen (such as the
  inactive modeline in a split frame with several buffers).

  For dark themes, it is recommended to define `background-on` as a bit brigher
  than `background-primary`, and `background-off` as darker than
  `background-primary`.

- `foreground`: used for plain text and editor decorations of different kinds.

  Comes in three flavors: `foreground-primary`, `foreground-secondary`,
  `foreground-tertiary` with falling degree of use in the theme.

- `primary`: used for certain parts of the syntax highlighting (for example,
  keywords). Refer to [the code](immaterial-theme.el) for details.

  Comes in three variants, which are used to add some slight variation in the
  syntactic highlighting: `primary`, `primary-dark`, `primary-light`.

- `secondary`: used for certain parts of the syntax highlighting (for example,
  types and variables). Refer to [the code](immaterial-theme.el) for details.

  Comes in three variants, which are used to add some slight variation in the
  syntactic highlighting: `secondary`, `secondary-dark`, `secondary-light`.

- some additional colors for specific types of highlighting:

    - `error`: for highligting erroneous code.
    - `warning`: for highligting suspicious code.
    - `discrete`: for text that should be less pronounced (code comments, line
      numbers).
    - `cursor`: the color of the cursor.
    - `vertical-border`: the color of the vertical line that separates windows
      in a frame.
    - `modeline-active-fg`: foreground color to use for an active buffer
      modeline.
    - `modeline-active-bg`: background color to use for on an active buffer
      modeline.
    - `modeline-inactive-fg`: foreground color to use for on an inactive buffer
      modeline.
    - `modeline-inactive-bg`: background color to use for on an inactive buffer
      modeline.

All colors are defined in the default `immaterial-color-alist` (tip: enable `rainbow-mode` when exploring).

Each color can be overridden through the `immaterial-color-override-alist`
variable, which overrides the defaults in the `immaterial-color-alist`. As an
example, to provide a different primary color palette:

    (setq immaterial-color-override-alist
      '(("primary"         . "#ffa726")
        ("primary-light"   . "#ffd95b")
        ("primary-dark"    . "#c77800")
        ))

**Note**: the [Material color tool](https://material.io/resources/color) is
useful when experimenting with color palettes and variants of a certain
color. For dark themes, less saturated colors (200 and less) from the color
palette improves readability.

**Note**: emacs [rainbow-mode](https://elpa.gnu.org/packages/rainbow-mode.html)
comes in handy for highlighting each hex color being edited in you emacs init
file.



## Screenshots

### immaterial-dark

![dark theme](screenshots/immaterial-dark.png)


### immaterial-light

![light theme](screenshots/immaterial-light.png)


## Install

- From MELPA (or MELPA stable) via:

        M-x package-install RET immaterial-theme

        ;; load dark theme
        (load-theme 'immaterial-dark t)

        ;; ... or load light theme
        (load-theme 'immaterial-light t)

- Via `use-package`:

        (use-package immaterial-theme
          :ensure t
          :config
          (load-theme 'immaterial-dark t))

- By adding `immaterial-theme.el` to `~/.emacs.d/themes` and the following to
  your `init.el`:

        (add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
        (load-theme 'immaterial-dark t)
