<!-- !!!THIS FILE HAS BEEN GENERATED!!! Edit README.org -->


# Tools for Display & Presenting

<a href="https://melpa.org/#/moc"><img src="https://melpa.org/packages/moc-badge.svg" alt="melpa package"></a><a href="https://stable.melpa.org/#/moc"><img src="https://stable.melpa.org/packages/moc-badge.svg" alt="melpa stable package"></a><a href="https://elpa.nongnu.org/nongnu/moc.html"><img src="https://elpa.nongnu.org/nongnu/moc.svg" alt="Non-GNU ELPA"></a>

Master of Ceremonies 🎤 is a collection of tools for presenting. Use it to configure Emacs for live demonstrations and presentations. It can also be used to author content for richer presentations.

-   🎛️ `moc-dispatch` is a collection of tools to quickly configure a frame for screen sharing or presenting on a projector.

-   🔎 `moc-focus` is a tool to create, both live and pre-recorded, illustrations of code and other text in Emacs.


## Status 👷

This package is still pre-1.0. Read the NEWS.org and release notes

The `moc-focus` command and others are super useful and is used in almost every video or presentation I make! Therefore, it is made available in this early state.

Subscribe to [Positron's YouTube channel](https://www.youtube.com/@Positron-gv7do) to catch updates on when it's added to package archives and more information about how to use it.


## Installation

```elisp
;; Via Non-GNU ELPA or MELPA
(use-package moc)

;; package-vc
(package-vc-install
 '(moc
   :url "https://github.com/positron-solutions/moc.git"))

;; using elpaca's with explicit recipe
(use-package moc
  :ensure (moc
           :host github
           :repo "positron-solutions/moc"))

;; straight with explicit recipe
(use-package moc
  :straight (moc
             :type git :host github
             :repo "positron-solutions/moc"))

;; or use manual load-path & require, you brave yak shaver
```


# Authoring 🖋️

`moc-focus` is very powerful for pre-recording sequences of images to build up larger expressions for the audience, highlighting key points in the content.

Several workflows are supported:

-   🎬 **live presentation**: focus some text and use the cursor and highlights to point out key parts of the contents
-   ▶️ **persisted playback**: save focused text and all configured highlights, perhaps to use as a sequence of steps in a [dslide](info:dslide#Top) presentation.
-   📸 **saving screenshots**: save screenshots for use in external applications such as Blender or another video authoring tool.


## Live Presentation 🎬

1.  Select a region of text you wish to display
2.  Call `moc-focus`. (this is a command worth binding)

Now you are within the focus buffer. It uses a modal interface, like magit, to quickly author changes and reference the controls. To see the transient modal interface, call `moc-focus-dispatch` or press the `h` binding.

By default the cursor is hidden. This is because usually you want to make screenshots or do playback. To activate an ephemeral cursor, press `.` for `moc-subtle-cursor-mode`. (this mode may be useful for presentation in general). The subtle cursor disappears whenever you aren't using it! Very beneficial.

You can also modify the results several ways:

-   Select a region and highlight it with `l`
-   Remap the faces with `r`
-   Clear highlights with `c`

💡 Maneuvering the point with packages like `avy` does not require a cursor. Highlighting by simply using the region is also viable for live demonstration.

💡 While the buffer defaults to read-only to be less touchy, you can of course turn this off with `read-only-mode` to make quick touch-ups to the text. Keep in mind, the backing data structures may become off depending on the latest implementation 👷


## Persisted Playback ▶️

When you have arrived at a starting or intermediate point you want to play back exactly within Emacs, the `w` key calls `moc-focus-kill-ring-save`. An expression will be saved to your kill ring that re-creates the currently visible state.

Copy this expression wherever appropriate, such as the body of a babel block configured as a step in [dslide](info:dslide#Top).

🚧 Playback is not yet 100% the same as doing a fresh capture with `moc-focus`.


## Saving Screenshots 📸

It is of great convenience if the files are saved in the correct place. Configure `moc-screenshot-dir` to be a function that calculates the correct location based on your current project directory.

Here's an example that calculates where to save screenshots based on the project directory.

`moc-focus-base-buffer` is bound to a source buffer when the current buffer is an `MoC Focus` buffer. You can use this value to decided based on the source buffer.

```elisp
;; Add this to your use-package :config section
 (defun my-screenshots-dir ()
  (interactive)
  ;; When taking screenshots from a focus buffer, it sets
  ;; `moc-focus-base-buffer' so that you can decide based on the base
  ;; buffer, not the focus buffer, which has no associated file.
  (with-current-buffer (or moc-focus-base-buffer
                           (current-buffer))
    (expand-file-name
     "screenshots/"
     (or (when-let ((p (project-current))) (project-root p))
         (temporary-file-directory)))))

;; configure the function to be called to calculate the correct options at
;; runtime
(setopt moc-screenshot-dir #'my-screenshots-dir)

```

Now just configure the save type, `moc-screenshot-type`, which uses the same types as supported by `x-export-frames`.


# Presenting 🎛️

It is recommended to bind the `moc-dispatch` interface to a key. This interface shows many relevant configuration states and provides options to change them. It also includes some built-in Emacs behaviors, consolidating these controls into a transient interface.

-   `moc-quiet-mode` suppresses some but not all messages. User commands that generate `user-error` and some other messages may still get through. ⚠️ Leaving this on can result in confusion, but it is on by default when using `moc-focus` because such messages almost always interrupt taking screenshots.
-   `moc-face-remap` can apply multiple face remappings according to preset profiles.
-   `moc-subtle-cursor-mode` is similar to `blink-cursor-mode` but hides itself after motion. You can use it like a laser pointer during presentation and it gets back out of the way on its own.
-   `moc-fixed-frame` creates hooks on a target frame that preserve its dimensions. It has preset resolutions, so if you always present or capture the screen at target dimensions, it's great.
    
    🚧 There is an issue with the size maintenance when a transient prefix is active. Resizing the default text scale can exhibit the issue. Dismissing the transient fixes it for now.

**Built-in Tools**

-   Controls for `default-text-scale-mode` and `text-scale-mode`
-   `hide-mode-line-mode`

🚧 It is planned to add for convenience, extra groups to this interface using [transient](info:transient#Top) support for menu modification. Transient supports this, so knock yourself out. PR's welcome.


# Customizing ✨

You can start with the normal configuration of `defcustom` variables and hooks. That's easy mode.


## Face Remapping

Add new presets to `moc-face-remap-presets`. It is an alist of alists. Each element is `(NAME . PRESET)`. Each `PRESET` is `(FACE . SPECS)`. `SPECS` will be used by `face-remap-add-releative` and is just a plist of face attributes.

You need an arbitrary symbol that names your preset, such as `box-land`. If you wanted to remap the default face to always have the `:box` attribute turned on, do something like this:

```elisp
;; Add a new preset
(push '(puke-land . ((default . (:background "#004400"))))
      moc-face-remap-presets)

;; activate from an Elisp program
(moc-face-remap 'puke-land)

;; turn off all remaps
(moc-face-remap-clear)

;; To remove a badly configured preset
(setq moc-face-remap-presets (assq-delete-all 'puke-land moc-face-remap-presets))
```

When using `moc-focus`, you likely want to configure some remaps. You can do this manually using the `moc-focus-mode-hook` or by adding your remap to `moc-focus-default-remaps`. Both approaches are equivalent.


## Updating Transient Menus

It's impossible for menus like `moc-dispatch` to support every package that every user installs. Instead, you can add a custom transient group or suffix to `moc-dispatch` for your use case.

It is recommended to read the Transient manual section on [Modifying Existing Transients](info:transient#Modifying Existing Transients). You may also want to see the [Transient Showcase](https://github.com/positron-solutions/transient-showcase?tab=readme-ov-file#Groups-&-Layouts) section on layouts to understand how vectors are used to define groups. This sheds light on how coordinates work.


### Finding the Address

We are eventually going to call functions like `transient-replace-suffix`, but first depending on the change you want to make, such as inserting an extra command or replacing a group, you will need to know how to properly target the location.

Transient layouts can have nested groups. To make things easy, you can use `transient-get-suffix` to *interrogate* the existing prefix.

```elisp
(transient-get-suffix 'moc-dispatch '(1)) ; Fixed Frame group

(transient-get-suffix 'moc-dispatch '(0 1)) ; Buffer Text Scale group

(transient-get-suffix 'moc-dispatch "m") ; hide (mode line)
```

-   When adding a command around an existing command or replacing that command, it is sufficient to use its key as an address. The `m` key can be used as an address to replace


### Adding a Group

The functions `transient-insert-suffix` and `transient-append-suffix` etc accept prefix symbol, such as `moc-dispatch` or `moc-focus-dispatch`, an address (found in previous section) and a group or suffix, just as it would be written for use in `transient-define-prefix`.

Here's an example of defining a new group for spell-checking. It has one command, `global-jinx-mode`. The command will be bound to `j`. It uses the `my-jinx-desc` function to generate a description. This will update the description to show us the current state.

```elisp
(defun my-jinx-desc ()
  "Describe jinx mode state."
  (format "jinx: %s" (if global-jinx-mode
                         (propertize "on" 'face 'success)
                       (propertize "off" 'face 'transient-value))))

(transient-append-suffix 'moc-dispatch '(3 2)
    ["Spelling"
     ("j" global-jinx-mode :description my-jinx-desc :transient t)])
```

If you mess something up and get into an inconsistent state, you can reload the prefix to obtain a fresh copy and proceed to try again. Just evaluate the definition of `moc-dispatch` again.

If you changed `moc-focus-dispatch`, now you just need to finish up by adding any new keys to `moc-focus-mode-map`.


# Contributing 🍔

-   Since you likely just need something to magically happen, the recommended option is to place a hamburger in the [hamburger jar](https://github.com/sponsors/positron-solutions) and file an issue.
-   If you do have time, excellent. Happy to support your PR's and provide context about the architecture and behavior.

This package is brought to you by [dslide](https://github.com/positron-solutions/dslide), which is brought to you by [prizeforge.com](https://prizeforge.com), brought to you by [positron.solutions](https://positron.solutions). If our Github sponsors page is still up, you can sign up there and we will invite you to move over to PrizeForge when it is launched.


## Work In Progress 🚧

Open issues and give feedback on feature requests. Contributions welcome. This is a non-exhaustive list of things being considered or even planned.

MoC was developed alongside [dslide](https://github.com/positron-solutions/dslide) when dslide was still called Macro Slides. Features where Dslide can benefit from the out-of-the-box behavior will be integrated into dslide. Features thave have strong standalone value **and** are not tedious to integrate with dslide via hooks can live in MoC.


### Settings Profiles

It can be rather annoying when switching between multiple sets of coherent settings. For example, it is tedious to change from a live demonstration style setup to a full-screen projector slide show style setup. One set of hooks cannot do the job.


### Recording Integration

Whenever you change the resolution of a frame for recording, you need to configure the output resolution in OBS. To start recording, you have to click on OBS. This kind of work could be tighter and integrated into `moc-dispatch`.


### Extensible Transient UI

A lot of the features a person might want depends on what they have installed. Not everyone uses jinx. Not everyone needs a control to turn it off. Either we can support everything as an option or make the options themselves programmable.
