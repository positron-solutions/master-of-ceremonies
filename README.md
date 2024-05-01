# Master of Ceremonies

Tools for display and presentation

- hide org markup mode `mc-hide-mode`
- set resolution with `mc-set-resolution`
- fullscreen focus with highlight and playback with `mc-focus`
- subtle cursor mode `mc-subtle-cursor-mode`
- no messages `mc-quiet-mode`

Many of these modes and some integrations with other packages are wrapped up
into:

- `mc-live-present-mode` for cleaning up a presentation buffer but remaining
  interactive
- `mc-present-mode` for a truly minimal display with subtle cursor, quiet mode,
  as close as possible to just a pure presentation

This package is still in development but being published to make it available.
Subscribe to [Positron's YouTube
channel](https://www.youtube.com/@Positron-gv7do) to catch updates on when it's
added to package archives and more information about how to use it.

## Installation

```elisp
   ;; package-vc
   (package-vc-install
    '(master-of-ceremonies
      :url "https://github.com/positron-solutions/master-of-ceremonies.git"))

   ;; using elpaca's with explicit recipe
   (use-package master-of-ceremonies
     :elpaca (master-of-ceremonies 
              :host github
              :repo "positron-solutions/master-of-ceremonies"))

   ;; straight with explicit recipe
   (use-package master-of-ceremonies
     :straight (master-of-ceremonies 
                :type git :host github
                :repo "positron-solutions/master-of-ceremonies"))

   ;; or use manual load-path & require, you brave yak shaver
```

## Extending

This package was developed alongside Macro Slides.  Macro Slides is focused on
the structure of presentation sequences and using Org documents to declare the
structure.

Master of Ceremonies should be useful for any presentation package, not just
Macro Slides.

## Planned Features

- Displaying notes in a separate buffer, a complement to hiding elements for
  presentation.
  
- Settings profiles for quickly changing many settings at once.

