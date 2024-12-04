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

Here's an example that employs a variety of techniques to calculate and persist the user's choice, per-buffer, of where to save screenshots.

`moc--base-buffer` exists when the current buffer is an `MoC Focus` buffer and

```elisp
;; Add this to your use-package :config section
(defun my-screenshots-dir ()
  (interactive)
  (let ((dir (or (buffer-local-value
                  'moc--screenshot-dir moc--base-buffer)
                 (expand-file-name
                  "screenshots/"
                  (or (project-root (project-current))
                      (temporary-file-directory))))))
    ;; Creating directories can signal an error.  That's a good time to ask the
    ;; user for a fallback directory.
    (condition-case nil
        (unless (file-exists-p dir)
          (make-directory dir t))
      (error (let ((fallback (read-directory-name
                              "screenshot directory: ")))
               (setq dir fallback))))

    ;; Persist this choice buffer locally, using whatever buffer MoC was invoked
    ;; from if we're in an MoC buffer.
    (when moc--base-buffer (set-buffer moc--base-buffer))
    (setq moc--screenshot-dir dir)))

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
