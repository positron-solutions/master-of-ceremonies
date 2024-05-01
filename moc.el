;;; moc.el --- Master of Ceremonies -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Positron Solutions <contact@positron.solutions>

;; Author: Positron Solutions <contact@positron.solutions>
;; Keywords: convenience, outline
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))
;; Homepage: http://github.com/positron-solutions/master-of-ceremonies

;;; License:

;; Permission is hereby granted, free of charge, to any person obtaining a copy of
;; this software and associated documentation files (the "Software"), to deal in
;; the Software without restriction, including without limitation the rights to
;; use, copy, modify, merge, publish, distribute, sub-license, and/or sell copies of
;; the Software, and to permit persons to whom the Software is furnished to do so,
;; subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
;; FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
;; COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
;; IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

;;; Commentary:
;;
;; Master of ceremonies.  Tools for display, screen capture, and presentation:
;;
;; - hide org markup mode `moc-hide-mode'
;; - set resolution with `moc-set-resolution'
;; - fullscreen focus with highlight and playback with `moc-focus'
;; - subtle cursor mode `moc-subtle-cursor-mode'
;; - no messages `moc-quiet-mode'

;;; Code:
(require 'org-element)
(require 'transient)

(defgroup moc nil "Master of ceremonies." :prefix 'moc :group 'outline)

(defcustom moc-note-block-types
  '(comment-block
    quote-block
    verse-block)
  "Element types from `org-element-all-elements'.
Even if `special-block' is omitted, but `moc-note-special-block-types' is
populated, some `special-blocks' will be treated as notes."
  :type '(repeat symbol)
  :group 'moc)

(defcustom moc-note-special-block-types
  '("note")
  "Types of special `special-block' elements to treat as notes.
`special-block' org elements have an additional type property."
  :type '(repeat string)
  :group 'moc)

(defcustom moc-hide-element-types '(property-drawer keyword)
  "Hide these element types.
See `org-element-all-elements'."
  :type '(repeat symbol)
  :group 'moc)

(defcustom moc-hide-block-header-and-footer t
  "Hide the header and footer of blocks.
This is mainly to further clean up source blocks."
  :type 'boolean
  :group 'moc)

(defcustom moc-present-fullscreen t
  "Switch to single fullscreen window"
  :group 'moc
  :type 'boolean)

(defcustom moc-present-window-margins '(2 . 2)
  "Add margins to presentation window."
  :group 'moc
  :type '(choice cons function))

(defcustom moc-screenshot-path #'temporary-file-directory
  "Directory path or function that returns a directory path.
Directory path is a string."
  :type '(choice string function)
  :group 'moc)

;; TODO focus namespace
(defcustom moc-after-make-frame-function nil
  "A function with the signature NEW-FRAME.
Use this for deep customization of the frame behavior."
  :type 'function
  :group 'moc)

;; TODO implement notes
(defface moc-note-face '((t :size 2.0
                            :foreground "#ffffff"
                            :distant-foreground "#000000"
                            :inherit default))
  "Default face for notes buffer."
  :group 'moc)

(defcustom moc-cap-resolutions
  '((youtube-short . (1080 . 1920))
    (1080p . (1920 . 1080))
    (2k . (2560 . 1440))
    (4k . (3840 . 2160))
    (fullscreen . fullboth))
  "Frequent screen capture resolutions.
Form is one of:

- (NAME . (WIDTH . HEIGHT))

- (NAME . FULLSCREEN)

NAME is a symbol, WIDTH and HEIGHT are integers, and FULLSCREEN
is valid value for the `fullscreen' frame parameter."
  :type '(cons (choice cons symbol))
  :group 'moc)

(defvar moc--quiet-old-inhibit-message nil)

(defvar-local moc--note-face-cookie nil
  "Memento for remapping the note buffer face.")

;; TODO which overlays are these again?
(defvar-local moc--overlays nil
  "Overlays used to hide notes.")

(defvar-local moc--focus-highlight-overlays nil
  "Overlays used to focus text.")

(defvar-local moc--focus-highlights nil
  "List of highlight regions for playback.")

(defvar-local moc--focus-cleaned-text nil
  "Copy of cleaned input text for replay expressions.")

(defvar-local moc--present-old-window-config nil
  "Restore configuration for fullscreen presentation.
See `moc-present-fullscreen'.")

(defvar-local moc--focus-margin-left nil)
(defvar-local moc--focus-margin-right nil)
(defvar moc--focus-old-window-config nil)

;; TODO use keep-lines logic from macro-slides
(defun moc-hide (element &optional display)
  "Display ELEMENT with a zero-length overlay.
Optional DISPLAY can be used to provide replacement text, such as
a newline, for when vertical lines should be preserved."
  (let* ((start (org-element-property :begin element))
         (end (org-element-property :end element))
         (overlay (make-overlay start end)))
    (push overlay moc--overlays)
    (if display
        (overlay-put overlay 'display display)
      (overlay-put overlay 'invisible t))
    (overlay-put overlay 'moc t)))

(defun moc--hide-pattern (regex &optional extra-chars)
  "EXTRA-CHARS is for patterns that don't include their newline."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward regex nil t)
      (let ((overlay (make-overlay (match-beginning 0)
                                   (+ (or extra-chars 0) (match-end 0)))))
        (push overlay moc--overlays)
        (overlay-put overlay 'invisible t)
        (overlay-put overlay 'moc t)))))

(defun moc--special-note-p (special-block)
  "Return SPECIAL-BLOCK if it is matched by `moc-note-special-block-types'."
  (when (member (org-element-property :type special-block)
                moc-note-special-block-types)
    special-block))

(defun moc--match-notes (data)
  "Return elements from DATA should be treated as notes."
  (cons (org-element-map data moc-note-block-types
          #'identity)
        (when moc-note-special-block-types
          (org-element-map data 'special-block
            #'moc--special-note-p))))

(defun moc--hide-keyword-labeled (data)
  "Searches for elements labeled: #+attr_hide: t.
Value is ignored, but generally should be t for clarity."
  (org-element-map data t
    (lambda (e) (when (org-element-property :attr_hide e)
             (moc-hide e)))))

(defun moc-hide-refresh ()
  "Refresh hiding of all configured types.
Will remove any existing hiding overlays.  You will usually add
this to a hook that is called after the buffer is narrowed to the
content you want to display."
  (interactive)
  ;; when parsing is done after narrowing, only the
  (let ((data (org-element-parse-buffer)))
    (org-element-map data (append moc-hide-element-types
                                  moc-note-block-types)
      #'moc-hide)

    (moc--hide-keyword-labeled data)

    ;; Affiliated keywords need a regex approach
    (when (member 'keyword moc-hide-element-types)
      (moc--hide-pattern org-keyword-regexp 1))

    (when moc-hide-block-header-and-footer
      (moc--hide-pattern "^[ 	]*#\\+begin_[a-z]+.*\n")
      (moc--hide-pattern "^[ 	]*#\\+end_[a-z]+.*\n"))))

;;;###autoload
(define-minor-mode moc-hide-mode
  "Hide all configured elements.
This should be enabled after narrowing to the displayed content
to avoid creating an unnecessarily large amount of overlays in
large org documents."
  :group 'moc
  (cond (moc-hide-mode
         (moc-hide-refresh))
        (t
         (mapc #'delete-overlay moc--overlays))))

;; TODO not implemented yet
;;;###autoload
(defun moc-notes ()
  "Display content matched by notes in an indirect buffer."
  (interactive)
  (user-error "Not finished implementing")
  (let* ((base-buffer (or (buffer-base-buffer (current-buffer))
                          (current-buffer)))
         (base-buffer-name (buffer-name base-buffer))
         (note-buffer-name (format "*notes: %s*" base-buffer-name))
         (note-buffer (make-indirect-buffer base-buffer
                                            note-buffer-name
                                            nil t))
         (note-frame (make-frame))
         (window (car (window-list note-frame))))
    (set-window-buffer window note-buffer)
    (run-hook-with-args moc-after-make-frame-function
                        (list note-frame))))

;;; Subtle Cursor mode

;;;###autoload
(define-minor-mode moc-subtle-cursor-mode
  "Make cursor subtle.
If `blink-cursor-mode' is off, there will be no visible cursor at all."
  :keymap nil
  (cond
   (moc-subtle-cursor-mode
    ;; TODO customize
    (setq-local blink-cursor-alist '(((hbar . 0) . bar)))
    (setq-local cursor-type '(hbar . 0))
    (setq-local blink-cursor-blinks 4)
    (setq-local blink-cursor-interval 0.1)
    (setq-local blink-cursor-delay 0.0))
   (t
    (setq-local blink-cursor-alist (default-value 'blink-cursor-alist))
    (setq-local cursor-type (default-value 'cursor-type))
    (setq-local blink-cursor-blinks (default-value 'blink-cursor-blinks))
    (setq-local blink-cursor-mode (default-value 'blink-cursor-mode)))))

;;; Present-mode

;; TODO push modified values onto a stack for restoration
(define-minor-mode moc-present-mode
  "Make the screen as clean as possible."
  :group 'moc
  :global t
  (cond (moc-present-mode
         (when (featurep 'git-gutter)
           (git-gutter-mode -1))
         (when (featurep 'jinx)
           (jinx-mode -1))
         (when (featurep 'org-appear-mode)
           (org-appear-mode -1))

         (hide-mode-line-mode 1)

         ;; TODO Default images to inline display

         (when moc-present-fullscreen
           (setq moc--present-old-window-config
                 (current-window-configuration))
           (delete-other-windows))

         (when moc-present-window-margins
           (set-window-margins nil
                               (car moc-present-window-margins)
                               (cdr moc-present-window-margins)))

         (moc-subtle-cursor-mode 1)
         (moc-quiet-mode 1)
         (moc-clean-mode 1))

        ;; Reverse everything above
        (t
         (moc-quiet-mode -1)
         (moc-subtle-cursor-mode -1)
         (moc-clean-mode -1)

         (when (and moc--present-old-window-config
                    moc-present-fullscreen)
           (set-window-configuration
            moc--present-old-window-config)
           (setq moc--present-old-window-config nil))

         ;; TODO restore image display

         (when moc-present-window-margins
           (set-window-margins nil 0 0))

         (hide-mode-line-mode -1)

         ;; Features back on
         (when (featurep 'org-appear-mode)
           (org-appear-mode 1))
         (when (featurep 'git-gutter)
           (git-gutter-mode 1))
         (when (featurep 'jinx)
           (jinx-mode 1)))))

(define-minor-mode moc-clean-mode
  "Clean but still interactive.
See `moc-present-mode' for additional changes that are better
suited for pure presentations."
  :group 'moc
  :global t
  (cond (moc-clean-mode
         (moc-hide-mode 1))
        (t
         (moc-hide-mode -1))))

;;; Quiet mode

(define-minor-mode moc-quiet-mode
  "Inhibit messages in the echo area."
  :group 'moc
  :global t
  (cond (moc-quiet-mode

         ;; ⚠️ TODO inhibiting messages is a bit dangerous.  If anything fails,
         ;; messages will remain disabled ☠️

         ;; Naturally the manual sets not to set this, but the point is that the
         ;; user doesn't want to have messages for a while.  If it is never to
         ;; be turned off, how else can messages be avoided except case by case?
         (unless inhibit-message
           (setq moc--quiet-old-inhibit-message inhibit-message
                 inhibit-message t)))
        (t
         (setq inhibit-message moc--quiet-old-inhibit-message))))

;; * Focus fullscreen text

(defsubst moc--focus-assert-mode ()
  (unless (eq major-mode 'moc-focus-mode)
    (user-error "Not in focus buffer")))

;; TODO why is this interactive?
(defun moc-focus-quit ()
  "Fullscreen quit command."
  (interactive)
  (moc--focus-assert-mode)
  (kill-buffer)
  (set-window-configuration moc--focus-old-window-config)
  (setq moc--focus-old-window-config nil
        moc--focus-cleaned-text nil))

;; only add to the `buffer-list-update-hook' locally so we don't need to unhook
(defun moc--maintain-margins ()
  (when moc--focus-margin-left
    (set-window-margins (selected-window)
                        moc--focus-margin-left
                        moc--focus-margin-right)))

;; only add to the `kill-buffer-hook' locally so we don't need to unhook.  We
;; could rely on the quit command, but the hook is more reliable for things that
;; absolutely should not remain after the buffer is dead.
(defun moc--kill-cleanup ()
  (moc-quiet-mode -1))

(defun moc--focus-clean-properties (text)
  (let ((dirty-props (object-intervals text))
        (clean-string (substring-no-properties text)))
    (mapc
     (lambda (interval)
       (let ((begin (pop interval))
             (end (pop interval)))
         (put-text-property
          begin end
          'face (plist-get (car interval) 'face)
          clean-string)

         ;; TODO pass along overlays and extract their 'face and 'display to
         ;; compute the buffer visible string.

         (put-text-property
          begin end
          'display (plist-get (car interval) 'display)
          clean-string)))
     dirty-props)
    clean-string))

(defun moc--display-fullscreen (text)
  "Show TEXT with properties in a fullscreen window."
  (setq moc--focus-old-window-config (current-window-configuration))
  (let ((buffer (get-buffer-create "*MOC*" t))
        (text (moc--focus-clean-properties text)))
    (delete-other-windows)
    (let ((inhibit-message t))
      (switch-to-buffer buffer))
    (moc-focus-mode)
    (setq-local mode-line-format nil)
    (show-paren-local-mode -1)
    (moc-subtle-cursor-mode 1)
    (read-only-mode -1)

    ;; Before we start adding properties, save the input text without additional
    ;; properties.
    (setq-local moc--focus-cleaned-text text)

    (insert (propertize text
                        'line-prefix nil
                        'wrap-prefix nil))

    ;; TODO this is insufficient and I had to use face remapping for org blocks.
    (add-face-text-property (point-min) (point-max)
                            '(:background nil))

    (let* ((h (window-pixel-height))
           (w (window-pixel-width))
           (text-size (window-text-pixel-size))
           ;; TODO customize limits and minimums
           (max-text-scale (min (/ (* w 0.8) (car text-size))
                                (/ (* h 0.75) (cdr text-size))))
           (goal-scale (max (/ (* 0.6 w) (car text-size))
                            (/ (* 0.4 h)) (cdr text-size)))
           (scale-overlay (make-overlay 1 (point-max))))
      (overlay-put scale-overlay 'face
                   `(:height ,(min max-text-scale goal-scale)))
      (let* ((h (window-pixel-height))
             (w (window-pixel-width))
             (text-size (window-text-pixel-size))
             (margin-left (/ (- w (car text-size)) 2.0))
             (margin-cols (1- (floor (/ margin-left (frame-char-width)))))
             (margin-top (/ (- h (cdr text-size)) 2.0))
             (margin-lines (/ margin-top (frame-char-height))))
        (set-window-margins nil margin-cols margin-cols)
        (setq moc--focus-margin-left margin-cols
              moc--focus-margin-right margin-cols)

        (add-hook 'buffer-list-update-hook
                  #'moc--maintain-margins
                  nil t)

        (add-hook 'kill-buffer-hook
                  #'moc--kill-cleanup
                  nil t)

        (goto-char 0)
        (insert (propertize "\n" 'face `(:height ,margin-lines)))
        (setf (overlay-start scale-overlay) 2)
        (setf (overlay-end scale-overlay) (point-max))))
    (read-only-mode 1)))

(defvar-keymap moc-focus-mode-map
  :parent special-mode-map
  "q" #'moc-focus-quit
  "l" #'moc-focus-highlight
  "c" #'moc-subtle-cursor-mode
  "w" #'moc-focus-kill-ring-save
  "s" #'moc-focus-screenshot
  "u" #'moc-focus-highlight-clear)

;;;###autoload
(define-derived-mode moc-focus-mode special-mode
  "Modal controls for focus windows."
  :interactive nil)

(defun moc-focus-highlight-clear ()
  "Delete overlays."
  (interactive)
  (moc--focus-assert-mode)
  (setq moc--focus-highlights nil)
  (mapc #'delete-overlay moc--focus-highlight-overlays)
  (setq moc--focus-highlight-overlays nil))

(defun moc-focus-highlight (beg end)
  "Use the shadow face around BEG and END."
  (interactive "r")
  (moc--focus-assert-mode)
  (when moc--focus-highlight-overlays
    (moc-focus-highlight-clear))
  (push (list beg end) moc--focus-highlights)
  (let ((ov-beg (make-overlay (point-min) beg))
        (ov-end (make-overlay end (point-max))))
    ;; TODO customize
    (overlay-put ov-beg 'face 'shadow)
    (overlay-put ov-end 'face 'shadow)
    (push ov-beg moc--focus-highlight-overlays)
    (push ov-end moc--focus-highlight-overlays))
  ;; unnecessary to deactivate the mark when called any other way
  (when (called-interactively-p 't)
    (deactivate-mark)))

(defun moc--focus-apply-highlights (highlights)
  "Use to replay highlight from Elisp programs.
HIGHLIGHTS is a list of lists of BEG END to be highlighted.  Regions not
contained by some BEG END will have the shadow face applied."
  ;; TODO support multi-region highlight
  (when (> (length highlights) 1)
    (error "Multiple highlights not supported yet."))
  (apply #'moc-focus-highlight (car highlights)))

(defun moc-focus-kill-ring-save ()
  "Save the focused text and highlights to a playback expression."
  (interactive)
  (moc--focus-assert-mode)
  (let ((expression
         `(moc-focus
           ,moc--focus-cleaned-text
           ',moc--focus-highlights)))
    (kill-new (prin1-to-string expression)))
  (message "saved focus to kill ring"))

;;;###autoload
(defun moc-focus-region (beg end)
  (interactive "r")
  (moc--display-fullscreen (buffer-substring beg end)))

(defun moc-focus-screenshot ()
  "Save a screenshot of the current frame as an SVG image."
  (interactive)
  (moc--focus-assert-mode)
  (let* ((timestamp (format-time-string "%F-%H:%M:%S" (current-time)))
         (filename (format "screenshot-%s.svg" timestamp))
         (dir (if (stringp moc-screenshot-path)
                  moc-screenshot-path
                (funcall moc-screenshot-path)))
         (path (concat dir filename))
         (data (x-export-frames nil 'svg)))
    (unless (file-exists-p dir)
      (make-directory dir t))
    (with-temp-file path
      (insert data))
    (message "Saved to: %s" filename)))

;;;###autoload
(defun moc-focus-string (text)
  (interactive "Menter focus text: ")
  (moc--display-fullscreen text))

;;;###autoload
(defun moc-focus (text &optional highlights)
  "Focus selected region or prompt for TEXT.
Optional HIGHLIGHTS is a list of (BEG END)."
  (interactive
   (list (if (region-active-p)
             (buffer-substring (region-beginning) (region-end))
           (read-string "enter focus text: "))))
  (moc--display-fullscreen text)
  (when highlights
    (moc--focus-apply-highlights highlights)))

;; * Frame Resolution

(transient-define-infix moc--cap-select-res ()
  "Select the resolution."
  :key "r" :argument "resolution=" :format "%k %v"
  :choices moc-cap-resolutions
  :class transient-option)

(transient-define-suffix moc--cap-set-frame-resolution (resolution)
  "Set selected frame dimensions to RESOLUTION.
RESOLUTION is a key into `moc-cap-resolutions'."

  ;; TODO I think I wrote this to warm up at using the transient library.  It
  ;; could have just as easily worked using standard interactive and even
  ;; supported values outside the options.
  (interactive
   (if transient-current-command
       (list
        (transient-arg-value
         "resolution="
         (transient-args transient-current-command)))
     nil))

  (pcase-let ((`(,key . ,value)
               (assoc-string resolution moc-cap-resolutions)))
    (let* ((resolution (consp value))
           (width (and resolution (car value)))
           (height (and resolution (cdr value))))
      (if (eq key 'fullscreen)
          ;; TODO this doesn't actually care about the value of fullscreen
          (set-frame-parameter nil 'fullscreen 'fullboth)
        (set-frame-parameter nil 'fullscreen nil)

        ;; Even with `frame-resize-pixelwise' enabled, some discrepancies have
        ;; been observed.  Setting and then correcting based on the outcome
        ;; should fix most cases as the observed discrepancy is consistent
        ;; between calls.  This will still work even if there is no discrepancy.

        ;; Who requests a pixel resolution when they don't enable a pixel size?
        (unless frame-resize-pixelwise
          (warn "`frame-resize-pixelwise' not enabled"))

        (let ((frame-resize-pixelwise t))
          (set-frame-size nil width height t)
          (unless (and (eq (frame-pixel-width) width)
                       (eq (frame-pixel-height) height))
            (let ((width-correction (- width (frame-pixel-width)))
                  (height-correction (- height (frame-pixel-height))))
              (set-frame-size nil
                              (+ width width-correction)
                              (+ height height-correction)
                              t)))))

      (if resolution
          (message "%s width: %s height: %s"
                   key (frame-pixel-width) (frame-pixel-height))
        (message "%s value: %s" key (frame-parameter nil 'fullscreen))))))

(transient-define-prefix moc-set-resolution ()
  "Configure frames for screen capture."
  ["Options"
   (moc--cap-select-res)]
  ["Screen Aspect and Resolution"
   ("s" "toggle resolution" moc--cap-set-frame-resolution)])

;; TODO not every binary setting needs a mode
;; TODO multiple-settings transient
;; TODO multiple saved values, profiles
;; TODO toggle values within profiles
;; TODO reload composite value into dynamic transient

(provide 'moc)
;;; moc.el ends here
