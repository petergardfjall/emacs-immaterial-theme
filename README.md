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

The following are the full list of colors defined in the default
`immaterial-color-alist`:


 | Property               | immaterial-dark                                                    | immaterial-light                                                   |
 | --------               | -----                                                              | ----                                                               |
 | `background-primary`   | ![#012027](https://placehold.it/15/012027/000000?text=+) `#012027` | ![#fdfdfa](https://placehold.it/15/fdfdfa/000000?text=+) `#fdfdfa` |
 | `background-on`        | ![#01343f](https://placehold.it/15/01343f/000000?text=+) `#01343f` | ![#f5f2fa](https://placehold.it/15/f5f2fa/000000?text=+) `#f5f2fa` |
 | `background-off`       | ![#001b21](https://placehold.it/15/001b21/000000?text=+) `#001b21` | ![#fbfbf8](https://placehold.it/15/fbfbf8/000000?text=+) `#fbfbf8` |
 | `foreground-primary`   | ![#dddddd](https://placehold.it/15/dddddd/000000?text=+) `#dddddd` | ![#242424](https://placehold.it/15/242424/000000?text=+) `#242424` |
 | `foreground-secondary` | ![#c8c8c8](https://placehold.it/15/c8c8c8/000000?text=+) `#c8c8c8` | ![#505055](https://placehold.it/15/505055/000000?text=+) `#505055` |
 | `foreground-tertiary`  | ![#b0b0b0](https://placehold.it/15/b0b0b0/000000?text=+) `#b0b0b0` | ![#8e8e8e](https://placehold.it/15/8e8e8e/000000?text=+) `#8e8e8e` |
 | `primary`              | ![#9fa8da](https://placehold.it/15/9fa8da/000000?text=+) `#9fa8da` | ![#7e57c2](https://placehold.it/15/7e57c2/000000?text=+) `#7e57c2` |
 | `primary-light`        | ![#d1d9ff](https://placehold.it/15/d1d9ff/000000?text=+) `#d1d9ff` | ![#b085f5](https://placehold.it/15/b085f5/000000?text=+) `#b085f5` |
 | `primary-dark`         | ![#6f79a8](https://placehold.it/15/6f79a8/000000?text=+) `#6f79a8` | ![#4d2c91](https://placehold.it/15/4d2c91/000000?text=+) `#4d2c91` |
 | `secondary`            | ![#c5e1a5](https://placehold.it/15/c5e1a5/000000?text=+) `#c5e1a5` | ![#558b2f](https://placehold.it/15/558b2f/000000?text=+) `#558b2f` |
 | `secondary-light`      | ![#f8ffd7](https://placehold.it/15/f8ffd7/000000?text=+) `#f8ffd7` | ![#85bb5c](https://placehold.it/15/85bb5c/000000?text=+) `#85bb5c` |
 | `secondary-dark`       | ![#94af76](https://placehold.it/15/94af76/000000?text=+) `#94af76` | ![#255d00](https://placehold.it/15/255d00/000000?text=+) `#255d00` |
 | `error`                | ![#ff5555](https://placehold.it/15/ff5555/000000?text=+) `#ff5555` | ![#b0003a](https://placehold.it/15/b0003a/000000?text=+) `#b0003a` |
 | `warning`              | ![#ff9800](https://placehold.it/15/ff9800/000000?text=+) `#ff9800` | ![#ff9800](https://placehold.it/15/ff9800/000000?text=+) `#ff9800` |
 | `discrete`             | ![#777777](https://placehold.it/15/777777/000000?text=+) `#777777` | ![#888888](https://placehold.it/15/888888/000000?text=+) `#888888` |
 | `vertical-border`      | ![#012830](https://placehold.it/15/012830/000000?text=+) `#012830` | ![#dddddd](https://placehold.it/15/dddddd/000000?text=+) `#dddddd` |
 | `cursor`               | ![#64d8cb](https://placehold.it/15/64d8cb/000000?text=+) `#64d8cb` | ![#64d8cb](https://placehold.it/15/64d8cb/000000?text=+) `#64d8cb` |
 | `modeline-active-fg`   | ![#ffffff](https://placehold.it/15/ffffff/000000?text=+) `#ffffff` | ![#ffffff](https://placehold.it/15/ffffff/000000?text=+) `#ffffff` |
 | `modeline-active-bg`   | ![#005662](https://placehold.it/15/005662/000000?text=+) `#005662` | ![#9575cd](https://placehold.it/15/9575cd/000000?text=+) `#9575cd` |
 | `modeline-inactive-fg` | ![#777777](https://placehold.it/15/777777/000000?text=+) `#777777` | ![#9e9e9e](https://placehold.it/15/9e9e9e/000000?text=+) `#9e9e9e` |
 | `modeline-inactive-bg` | ![#001017](https://placehold.it/15/001017/000000?text=+) `#001017` | ![#ede7f6](https://placehold.it/15/ede7f6/000000?text=+) `#ede7f6` |

Each of these values can be overridden through the
`immaterial-color-override-alist` variable, which overrides the defaults in the
`immaterial-color-alist`. As an example, to provide a different primary color
palette:

    (setq immaterial-color-override-alist
      '(("primary"         . "#ffa726")
        ("primary-light"   . "#ffd95b")
        ("primary-dark"    . "#c77800")
        ))

**Note**: it is highly recommended to make use of the [Material color
tool](https://material.io/resources/color) to experiment with color palettes and
variants of a certain color. For dark themes, less saturated colors (200 and
less) from the color palette improves readability.

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
