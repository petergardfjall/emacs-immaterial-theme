# themacs

*Note: for good results in a pure terminal environment, consider installing
Emacs 26 and [enable
truecolors](https://github.com/syl20bnr/spacemacs/wiki/Terminal) in your
terminal.*

Themacs `/theme-acs/` is an emacs color theme, loosely based on the principles
of Google's [Material
design](https://material.io/design/color/the-color-system.html#color-theme-creation). More
specifically, it defines (and allows users to _redefine_) the following coloring
elements:

- `background`: used for the background of buffers, modeline, etc.

  Comes in three flavors: `background-primary`, `background-secondary`,
  `background-tertiary` with falling degree of use in the theme.

- `foreground`: used for plain text and editor decorations of different kinds.

  Comes in three flavors: `foreground-primary`, `foreground-secondary`,
  `foreground-tertiary` with falling degree of use in the theme.

- `primary`: used for certain parts of the syntax highlighting (for example,
  keywords). Refer to [the code](themacs-theme.el) for details.

  Comes in three variants, which are used to add some slight variation in the
  syntactic highlighting: `primary`, `primary-dark`, `primary-light`.

- `secondary`: used for certain parts of the syntax highlighting (for example,
  types and variables). Refer to [the code](themacs-theme.el) for details.

  Comes in three variants, which are used to add some slight variation in the
  syntactic highlighting: `secondary`, `secondary-dark`, `secondary-light`.

- some additional colors for specific types of highlighting:

    - `error`: for highligting erroneous code.
    - `warning`: for highligting suspicious code.
    - `discrete`: for text that should be less pronounced (code comments, line
      numbers).
    - `cursor`: the color of the cursor.

The following are the full list of colors defined in the default
`themacs-color-alist`:

 
 | Property               | Color                                                              |
 | --------               | -----                                                              |
 | `background-primary`   | ![#102027](https://placehold.it/15/102027/000000?text=+) `#102027` |
 | `background-secondary` | ![#37474f](https://placehold.it/15/37474f/000000?text=+) `#37474f` |
 | `background-tertiary`  | ![#62727b](https://placehold.it/15/62727b/000000?text=+) `#62727b` |
 | `foreground-primary`   | ![#eeeeee](https://placehold.it/15/eeeeee/000000?text=+) `#eeeeee` |
 | `foreground-secondary` | ![#dbdbdb](https://placehold.it/15/dbdbdb/000000?text=+) `#dbdbdb` |
 | `foreground-tertiary`  | ![#c8c8c8](https://placehold.it/15/c8c8c8/000000?text=+) `#c8c8c8` |
 | `primary`              | ![#4db6ac](https://placehold.it/15/4db6ac/000000?text=+) `#4db6ac` |
 | `primary-light`        | ![#82e9de](https://placehold.it/15/82e9de/000000?text=+) `#82e9de` |
 | `primary-dark`         | ![#00867d](https://placehold.it/15/00867d/000000?text=+) `#00867d` |
 | `secondary`            | ![#aed581](https://placehold.it/15/aed581/000000?text=+) `#aed581` |
 | `secondary-light`      | ![#e1ffb1](https://placehold.it/15/e1ffb1/000000?text=+) `#e1ffb1` |
 | `secondary-dark`       | ![#7da453](https://placehold.it/15/7da453/000000?text=+) `#7da453` |
 | `error`                | ![#FF5555](https://placehold.it/15/FF5555/000000?text=+) `#ff5555` |
 | `warning`              | ![#e86310](https://placehold.it/15/e86310/000000?text=+) `#e86310` |
 | `discrete`             | ![#777777](https://placehold.it/15/777777/000000?text=+) `#777777` |
 | `cursor`               | ![#e86310](https://placehold.it/15/e86310/000000?text=+) `#e86310` |

Each of these values can be overridden through the
`themacs-color-override-alist` variable, which overrides the defaults in the
`themacs-color-alist`. As an example, to provide a different primary color
palette:

    (setq themacs-color-override-alist
      '(("primary"         . "#ffa726")
        ("primary-light"   . "#ffd95b")
        ("primary-dark"    . "#c77800")
        ))

**Note**: it is highly recommended to make use of the [Material color
tool](https://material.io/tools/color) to experiment with color palettes and
variants of a certain color.

**Note**: emacs [rainbow-mode](https://elpa.gnu.org/packages/rainbow-mode.html)
comes in handy for highlighting each hex color being edited in you emacs init
file.


## Install

Add the `themacs-theme.el` file to `~/.emacs.d/themes` and add the following to
your `init.el`:

     (add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
     (load-theme 'themacs t)

You may optionally customize the theme via `themacs-color-override-alist`. For
example:

    (setq themacs-color-override-alist
          '(("primary"         . "#ffa726")
            ("primary-light"   . "#ffd95b")
            ("primary-dark"    . "#c77800")
            ))
