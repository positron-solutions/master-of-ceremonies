;;; master-of-ceremonies.el --- Master of Ceremonies -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Positron Solutions <contact@positron.solutions>

;; Author: Positron Solutions <contact@positron.solutions>
;; Keywords: convenience, outline
;; Version: 0.2.0
;; Package-Requires: ((emacs "29.1"))
;; Homepage: http://github.com/positron-solutions/master-of-ceremonies

;;; License:

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sub-license, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;; Commentary:
;;
;; Master of ceremonies.  Tools for display, screen capture, and presentation:
;;
;; - fullscreen focus with highlight and playback with `mc-focus'
;; - set an exact frame resolution for capture with `mc-fixed-frame-set'
;; - subtle, disappearing cursor with `mc-subtle-cursor-mode'
;; - hide cursor entirely with `mc-hide-cursor-mode'
;; - supress all messages with `mc-quiet-mode'
;; - remap many faces with `mc-face-remap'
;; - set many options at once with `mc-dispatch'
;;
;; To all the MCs out there who go by MC Focus, my sincerest apologies for the
;; unfortunate naming collision.  We will attempt to bring glory to your name.

;;; Code:
(require 'default-text-scale)
(require 'frame)
(require 'face-remap)
(require 'rect)
(require 'transient)

(defgroup master-of-ceremonies nil "Master of ceremonies."
  :prefix 'mc
  :group 'outline)

(defcustom mc-subtle-cursor-blinks 3
  "The number of blinks of the subtle cursor.
When using a transient cursor effect, the duration of cursor visibility
is the product of this and `mc-subtle-cursor-interval'.

\\[info] elisp::Cursor Parameters."
  :type 'integer)

(defcustom mc-subtle-cursor-interval 0.2
  "Length of cursor blink interval in seconds.
Values smaller than 0.013 will be treated as 0.013."
  :type 'number)

(defcustom mc-focus-width-factor-max 0.7
  "Focused text maximum width fraction.
This is never exceeded"
  :group 'master-of-ceremonies
  :type 'float)

(defcustom mc-focus-width-factor-min 0.5
  "Focused text minimum width fraction.
This will be achieved unless another maximum is violated."
  :group 'master-of-ceremonies
  :type 'float)

(defcustom mc-focus-height-factor-max 0.7
  "Focused text maximum height fraction.
This is never exceeded."
  :group 'master-of-ceremonies
  :type 'float)

;; TODO what we need is a goal based on the height of lines that is constrained
;; by the maximum based on visible area
(defcustom mc-focus-height-factor-min 0.2
  "Focused text minimum height fraction.
This will be achieved unless another maximum is violated"
  :group 'master-of-ceremonies
  :type 'float)

(defcustom mc-screenshot-path #'temporary-file-directory
  "Directory path or function that returns a directory path.
Directory path is a string."
  :type '(choice string function))

(defcustom mc-fixed-frame-sizes
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
is valid value for the `fullscreen' frame parameter.

\\[info] elisp::Frame Parameters"
  :type '(cons symbol
               (choice (cons number number)
                       symbol)))

(defcustom mc-face-remap-presets '((bold . ((default :weight bold))))
  "Face remapping presets.
Value is an alist.  Each entry should be a cons of SYMBOL PRESET.
SYMBOL will be used to choose the PRESET. PRESET is an ALIST where each
element of PRESET is a cons of FACE SPECS where SPECS is one of the
forms understood by `face-remap-add-relative'.

\\[info] elisp::Face Remapping"
  :type 'alist)


(defvar mc--quiet-old-inhibit-message nil)

(defvar mc--blink-cursor-old nil)
(defvar mc--subtle-cursor-dead-windows nil
  "Store windows where the cursor was left off.")
(defvar mc-subtle-cursor-timer nil
  "Timer started from `mc-subtle-cursor-start'.
This timer calls `mc-subtle-cursor-timer-function' every
`mc-subtle-cursor-interval' seconds.")
(defvar mc-subtle-cursor-blinks-done 0
  "Number of blinks done since we started blinking on NS, X, and MS-Windows.")

(defvar-local mc--focus-highlight-overlays nil
  "Overlays used to focus text.")

(defvar-local mc--focus-highlights nil
  "List of highlight regions for playback.")

(defvar-local mc--focus-cleaned-text nil
  "Copy of cleaned input text for replay expressions.")
(defvar-local mc--focus-margin-left nil)
(defvar-local mc--focus-margin-right nil)
(defvar mc--focus-old-window-config nil)

(defvar-local mc--face-remap-cookies nil)

;; * Mass Face Remapping

(defun mc--read-remap (&optional preset)
  (when-let ((key (or preset
                      (completing-read
                       "Choose a remap preset: "
                       mc-face-remap-presets))))
    (cdr (assoc-string key mc-face-remap-presets))))

;;;###autoload
(defun mc-face-remap-clear ()
  (interactive)
  (while-let ((cookie (pop mc--face-remap-cookies)))
    (face-remap-remove-relative cookie)))

;;;###autoload
(defun mc-face-remap (remap &optional keep-existing)
  "Remap many faces at once.
REMAP can be a symbol specifying a preset or an alist of FACE REMAP
pairs.  If any faces have already been remapped, you can pass non-nil
KEEP-EXISTING"
  (interactive (list (mc--read-remap) current-prefix-arg))
  (unless keep-existing
    (mc-face-remap-clear))
  (let ((remap (if (symbolp remap)
                   (or (mc--read-remap remap)
                       (user-error "Remapping not found"))
                 remap)))
    (mapc (lambda (r)
            (let ((face (car r))
                  (specs (cdr r)))
              (push (face-remap-add-relative face specs)
                    mc--face-remap-cookies)))
          remap)))

;; * Hide Cursor Mode

(defvar mc-subtle-cursor-mode)          ; compiler appeasement

(define-minor-mode mc-hide-cursor-mode
  "Make cursor completely hidden."
  :group 'master-of-ceremonies
  (cond
   (mc-hide-cursor-mode
    (when mc-subtle-cursor-mode
      (mc-subtle-cursor-mode -1))
    (setq-local cursor-type nil))
   (t
    (setq-local cursor-type (default-value 'cursor-type)))))

;; * Subtle Cursor Mode

(defun mc-subtle-cursor-start ()
  "Start the `mc-subtle-cursor-timer'.
This starts the timer `mc-subtle-cursor-timer', which makes the cursor
blink if appropriate."
  (cond
   ;; stale hook fired
   ((null mc-subtle-cursor-mode) (mc-subtle-cursor-mode -1))
   (t
    (when mc-subtle-cursor-timer
      (cancel-timer mc-subtle-cursor-timer))
    ;; TODO figure out the termination for 1 blink
    (setq mc-subtle-cursor-blinks-done 1)
    (setq mc-subtle-cursor-timer
          (run-with-timer (max 0.013 mc-subtle-cursor-interval)
                          (max 0.013 mc-subtle-cursor-interval)
                          #'mc-subtle-cursor-timer-function))
    ;; Use the `cursor-type' ON-STATE
    (internal-show-cursor nil t))))

(defun mc-subtle-cursor-timer-function ()
  "Timer function of timer `mc-subtle-cursor-timer'."
  (when mc-subtle-cursor-mode
    (internal-show-cursor nil (not (internal-show-cursor-p))))
  ;; Suspend counting blinks when the w32 menu-bar menu is displayed,
  ;; since otherwise menu tooltips will behave erratically.
  (or (and (fboundp 'w32--menu-bar-in-use)
           (w32--menu-bar-in-use))
      ;; XXX guarding this expression upsets the blink count and I don't know
      ;; how it's supposd to work.
      (setq mc-subtle-cursor-blinks-done (1+ mc-subtle-cursor-blinks-done)))
  ;; Each blink is two calls to this function.
  (when (and (> mc-subtle-cursor-blinks 0)
             (>= mc-subtle-cursor-blinks-done (* 2 mc-subtle-cursor-blinks)))
    (when mc-subtle-cursor-timer (cancel-timer mc-subtle-cursor-timer)
          (setq mc-subtle-cursor-timer nil))
    (push (selected-window) mc--subtle-cursor-dead-windows)
    (when (internal-show-cursor-p)
      (message "Subtle cursor cancelled timer in ON-STATE"))))

(defun mc-subtle-cursor--should-blink ()
  "Determine whether we should be blinking.
Returns whether we have any focused non-TTY frame."
  (and mc-subtle-cursor-mode
       (let ((frame-list (frame-list))
             (any-graphical-focused nil))
         (while frame-list
           (let ((frame (pop frame-list)))
             (when (and (display-graphic-p frame) (frame-focus-state frame))
               (setf any-graphical-focused t)
               (setf frame-list nil))))
         any-graphical-focused)))

(defun mc-subtle-cursor-check ()
  "Check if cursor blinking shall be restarted.."
  (when (mc-subtle-cursor--should-blink)
    (mc-subtle-cursor-start)))

(define-minor-mode mc-subtle-cursor-mode
  "Like `blink-cursor-mode' but leaves cursor off.
This is a modification of `blink-cursor-mode' that immediately
transitions to the ON-STATE when commands are entered and finishes
blinking in the OFF-STATE, enabling customization of `cursor-type' and
`blink-cursor-alist' to achieve a transient cursor or a very subtle
cursor when the user is not moving the point.

\\[info] elisp::Cursor Parameters.

When you do anything to move the cursor, it will remain visible for the
product of `mc-subtle-cursor-blinks' and `mc-subtle-cursor-duration'.

Because this mode conflicts with `blink-cursor-mode', it is turned off when
found active.

🚧 The mode is experimental."
  :global t
  :init-value (not (or noninteractive
		       no-blinking-cursor
		       (eq system-type 'ms-dos)))
  (cond
   (mc-subtle-cursor-mode
    (setq mc--blink-cursor-old blink-cursor-mode)
    (when blink-cursor-mode
      (blink-cursor-mode -1))
    (when mc-hide-cursor-mode
      (mc-hide-cursor-mode -1))
    (add-function :after after-focus-change-function
                  #'mc-subtle-cursor-check)
    (add-hook 'after-delete-frame-functions #'mc-subtle-cursor-check)
    (add-hook 'post-command-hook #'mc-subtle-cursor-check)
    (mc-subtle-cursor-check))
   (t
    (remove-hook 'post-command-hook #'mc-subtle-cursor-check)
    (remove-hook 'after-delete-frame-functions #'mc-subtle-cursor-check)
    (remove-function after-focus-change-function
                     #'mc-subtle-cursor-check)
    (when mc-subtle-cursor-timer
      (cancel-timer mc-subtle-cursor-timer))
    ;; Make sure to leave the cursor in the ON-STATE in all windows when
    ;; quitting.
    ;; TODO seems like this never actually happens.  Cursor has an alternate
    ;; state when left around in another window, regardless of whether it was
    ;; blink on or off when the window changed.
    (while-let ((win (pop mc--subtle-cursor-dead-windows)))
      (internal-show-cursor win t))
    ;; Selected window likely not in above dead window cleanup and could be in
    ;; blink off state.
    (internal-show-cursor nil t)
    (when mc--blink-cursor-old
      (blink-cursor-mode 1)
      (setq mc--blink-cursor-old nil)))))

;; * Quiet mode

(define-minor-mode mc-quiet-mode
  "Inhibit messages in the echo area.
⚠️ Inhibiting messages is a bit dangerous.  If anything fails, because messages
are disabled, there may be no obvious user feedback ☠️"
  :group 'master-of-ceremonies
  :global t
  (cond
   (mc-quiet-mode
    ;; Naturally the manual sets not to set this, but the point is that the user
    ;; doesn't want to have messages for a while.  If it is never to be turned
    ;; off, how else can messages be avoided except case by case with
    ;; let-binding?
    (unless inhibit-message
      (setq mc--quiet-old-inhibit-message inhibit-message
            inhibit-message t)))
   (t
    (setq inhibit-message mc--quiet-old-inhibit-message))))

;; * Focus fullscreen text

(defsubst mc--focus-assert-mode ()
  (unless (eq major-mode 'mc-focus-mode)
    (user-error "Not in focus buffer")))

(defun mc-focus-quit ()
  "Fullscreen quit command."
  (interactive)
  (mc--focus-assert-mode)
  (kill-buffer "*MC Focus*"))

;; only add to the `buffer-list-update-hook' locally so we don't need to unhook
(defun mc--maintain-margins ()
  (when mc--focus-margin-left
    (set-window-margins (selected-window)
                        mc--focus-margin-left
                        mc--focus-margin-right)))

(defun mc--focus-clean-properties (text)
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

(defun mc--focus-cleanup ()
  (when mc--focus-old-window-config
    (set-window-configuration mc--focus-old-window-config))
  (setq mc--focus-old-window-config nil
        mc--focus-cleaned-text nil))

(defun mc--display-fullscreen (text)
  "Show TEXT with properties in a fullscreen window."
  (when-let ((old (get-buffer "*MC Focus*")))
    (kill-buffer old))
  (setq mc--focus-old-window-config (current-window-configuration))
  (let ((buffer (get-buffer-create "*MC Focus*"))
        (text (mc--focus-clean-properties text)))
    (delete-other-windows)
    (let ((inhibit-message t))
      (switch-to-buffer buffer))
    (add-hook 'kill-buffer-hook #'mc--focus-cleanup nil t)
    (mc-focus-mode)
    (setq-local mode-line-format nil)
    (show-paren-local-mode -1)
    (mc-hide-cursor-mode 1)
    (read-only-mode -1)

    ;; Before we start adding properties, save the input text without additional
    ;; properties.
    (setq-local mc--focus-cleaned-text text)

    (insert (propertize text
                        'line-prefix nil
                        'wrap-prefix nil))

    (let* ((h (window-pixel-height))
           (w (window-pixel-width))
           (text-size (window-text-pixel-size))
           ;; Not larger than any maximum
           (max-text-scale (min (/ (* w mc-focus-width-factor-max)
                                   (float (car text-size)))
                                (/ (* h mc-focus-height-factor-max)
                                   (float (cdr text-size)))))
           ;; At least as big a the minimum
           (min-scale (max (/ (* w mc-focus-width-factor-min)
                              (float (car text-size)))
                           (/ (* h mc-focus-height-factor-min)
                              (float (cdr text-size)))))
           ;; At least as big as the goal, but without exceeding the max
           (scale (min max-text-scale min-scale))
           (scale-overlay (make-overlay 1 (point-max)))
           (default-background (face-attribute 'default :background)))

      ;; Overrides faces but not inverse colors, which actually is kind of
      ;; desirable for org-modern's TODO's
      (overlay-put scale-overlay 'face
                   `(:height ,scale :background ,default-background :extend t))

      (let* ((h (window-pixel-height))
             (w (window-pixel-width))
             (text-size (window-text-pixel-size))
             (margin-left (/ (- w (car text-size)) 2.0))
             (margin-cols (1- (floor (/ margin-left (frame-char-width)))))
             (margin-top (/ (- h (cdr text-size)) 2.0))
             (margin-lines (/ margin-top (frame-char-height))))
        (set-window-margins nil margin-cols margin-cols)
        (setq mc--focus-margin-left margin-cols
              mc--focus-margin-right margin-cols)

        (add-hook 'buffer-list-update-hook
                  #'mc--maintain-margins
                  nil t)

        (goto-char 0)
        (insert (propertize "\n" 'face `(:height ,margin-lines)))
        (setf (overlay-start scale-overlay) 2)
        (setf (overlay-end scale-overlay) (point-max))))
    (read-only-mode 1)))

(defvar-keymap mc-focus-mode-map
  :parent special-mode-map
  "q" #'mc-focus-quit
  "l" #'mc-focus-highlight
  "c" #'mc-hide-cursor-mode
  "w" #'mc-focus-kill-ring-save
  "s" #'mc-focus-screenshot
  "u" #'mc-focus-highlight-clear)

;;;###autoload
(define-derived-mode mc-focus-mode special-mode
  "Modal controls for focus windows."
  :interactive nil)

(defun mc-focus-highlight-clear ()
  "Delete overlays."
  (interactive)
  (mc--focus-assert-mode)
  (setq mc--focus-highlights nil)
  (mapc #'delete-overlay mc--focus-highlight-overlays)
  (setq mc--focus-highlight-overlays nil))

(defun mc-focus-highlight (beg end)
  "Use the shadow face around BEG and END."
  (interactive "r")
  (mc--focus-assert-mode)
  (when mc--focus-highlight-overlays
    (mc-focus-highlight-clear))
  (push (list beg end) mc--focus-highlights)
  (let ((ov-beg (make-overlay (point-min) beg))
        (ov-end (make-overlay end (point-max))))
    ;; TODO customize
    (overlay-put ov-beg 'face 'shadow)
    (overlay-put ov-end 'face 'shadow)
    (push ov-beg mc--focus-highlight-overlays)
    (push ov-end mc--focus-highlight-overlays))
  ;; unnecessary to deactivate the mark when called any other way
  (when (called-interactively-p 't)
    (deactivate-mark)))

(defun mc--focus-apply-highlights (highlights)
  "Use to replay highlight from Elisp programs.
HIGHLIGHTS is a list of lists of BEG END to be highlighted.  Regions not
contained by some BEG END will have the shadow face applied."
  ;; TODO support multi-region highlight
  (when (> (length highlights) 1)
    (error "Multiple highlights not supported yet."))
  (apply #'mc-focus-highlight (car highlights)))

(defun mc-focus-kill-ring-save ()
  "Save the focused text and highlights to a playback expression."
  (interactive)
  (mc--focus-assert-mode)
  (let ((expression
         `(mc-focus
           ,mc--focus-cleaned-text
           ',mc--focus-highlights)))
    (kill-new (prin1-to-string expression)))
  (message "saved focus to kill ring"))

;;;###autoload
(defun mc-focus-region (beg end)
  (interactive "r")
  (mc--display-fullscreen (buffer-substring beg end)))

(defun mc-focus-screenshot ()
  "Save a screenshot of the current frame as an SVG image."
  (interactive)
  (mc--focus-assert-mode)
  (let* ((timestamp (format-time-string "%F-%H:%M:%S" (current-time)))
         (filename (format "screenshot-%s.svg" timestamp))
         (dir (if (stringp mc-screenshot-path)
                  mc-screenshot-path
                (funcall mc-screenshot-path)))
         (path (concat dir filename))
         (data (x-export-frames nil 'svg)))
    (unless (file-exists-p dir)
      (make-directory dir t))
    (with-temp-file path
      (insert data))
    (message "Saved to: %s" filename)))

;;;###autoload
(defun mc-focus-string (text)
  (interactive "Menter focus text: ")
  (mc--display-fullscreen text))

;;;###autoload
(defun mc-focus (text &optional highlights)
  "Focus selected region or prompt for TEXT.
Optional HIGHLIGHTS is a list of (BEG END)."
  (interactive
   (list (if (region-active-p)
             (funcall (if rectangle-mark-mode
                          (lambda (beg end)
                            (string-join (extract-rectangle beg end)
                                         "\n"))
                        #'buffer-substring)
                      (region-beginning) (region-end))
           (read-string "enter focus text: "))))
  (mc--display-fullscreen text)
  (when highlights
    (mc--focus-apply-highlights highlights)))

;; * Fixed Frame Size

(defun mc--fixed-frame-check-cleanup ()
  "Clean up hook if not guarding any more frames."
  (let ((frames (frame-list))
        guarded)
    (while (and frames (not guarded))
      (when (frame-parameter (pop frames) 'mc--fixed-frame-notify)
        (setq guarded t)))
    (unless guarded
      (remove-hook 'window-size-change-functions #'mc--fixed-frame-notify))))

(defun mc--fixed-frame-release (frame)
  (set-frame-parameter frame 'mc--fixed-frame-goal nil)
  (mc--fixed-frame-check-cleanup))

(defun mc--fixed-frame-notify (frame)
  "Check if FRAME has the right size."
  (if (frame-parameter frame 'fullscreen)
      ;; Only frames with a non-fullscreen size are guarded, so we bail if they
      ;; have acquired a fullscreen parameter.
      (message "Frame: %s has become fullscreen.  Releasing." frame)
      (mc--fixed-frame-release frame)
    (when-let ((size (frame-parameter frame 'mc--fixed-frame-goal)))
      (mc--fixed-frame-verify frame size))))

(defun mc--fixed-frame-verify (frame size)
  "Verify FRAME is SIZE or schedule correction."
  (let ((width-correction (- (car size) (frame-pixel-width frame)))
        (height-correction (- (cdr size) (frame-pixel-height frame))))
    (unless (and (= width-correction 0)
                 (= height-correction 0))
      (add-hook 'post-command-hook #'mc--fixed-frame-correct-all))))

;;;###autoload
(defun mc-fixed-frame-release-all ()
  "Release all guarded frames."
  (interactive)
  (let ((frames (frame-list)))
    (while-let ((frame (pop frames)))
      (set-frame-parameter frame 'mc--fixed-frame-goal nil))
    (mc--fixed-frame-check-cleanup)))

(defun mc--fixed-frame-correct (frame size &optional no-set)
  "Check and correct that FRAME is SIZE.
When optional NO-SET is non-nil, only check and set once.  Otherwise
set, check and set."
  ;; Its necessary to set once to find the correction needed to get the exact
  ;; frame size we want.  This same function can set up for itself and will not
  ;; do unnecssary work if no correction is needed.
  (unless no-set (mc--fixed-frame-set frame size))
  (let ((width-correction (- (car size) (frame-pixel-width frame)))
        (height-correction (- (cdr size) (frame-pixel-height frame))))
    (unless (and (= width-correction 0)
                 (= height-correction 0))
      (let ((frame-resize-pixelwise t))
        (message "making corrections: %sw %sh"
                 width-correction height-correction)
        (set-frame-size frame
                        (+ (car size) width-correction)
                        (+ (cdr size) height-correction)
                        t))
      (message "size: %sw %sh"
               (frame-pixel-width frame)
               (frame-pixel-height frame)))))

(defun mc--fixed-frame-correct-all ()
  "Used as a single-call post-command hook to avoid thrashing."
  ;; Updating the frame size during the `window-size-change-functions' is not a
  ;; good idea.  Temporarily removing the hook was an ineffective strategy in
  ;; this case.  Instead, this function runs in the post command hook and, if
  ;; added, corrects all frames and removes itself.
  (dolist (frame (frame-list))
    (if (frame-parameter frame 'fullscreen)
        ;; Only frames with a non-fullscreen size are guarded, so we bail if they
        ;; have acquired a fullscreen parameter.
        (message "Frame: %s has become fullscreen.  Releasing." frame)
        (mc--fixed-frame-release frame)
      (when-let ((size (frame-parameter frame 'mc--fixed-frame-goal)))
        (mc--fixed-frame-correct frame size))))
  (remove-hook 'post-command-hook #'mc--fixed-frame-correct-all))

(defun mc--fixed-frame-set (frame size)
  "Set SIZE on FRAME.
SIZE is either a (H . W) cons or a symbol that can be used as a frame
parameter for `fullscreen'."
  (if (consp size)
      (unless (and (= (car size) (frame-pixel-width frame))
                   (= (cdr size) (frame-pixel-height frame)))
        (let ((frame-resize-pixelwise t))
          (set-frame-parameter nil 'fullscreen nil)
          (set-frame-size nil (car size) (cdr size) t)
          (message "size: %sw %sh"
                   (frame-pixel-width frame)
                   (frame-pixel-height frame))
          (mc--fixed-frame-correct frame size t)))
    (set-frame-parameter nil 'fullscreen size)
    (message "fullscreen: %s" size)))

;;;###autoload
(defun mc-fixed-frame-set (frame-size)
  "Set and maintain a fixed FRAME-SIZE.
FRAME-SIZE is either a key for `mc-fixed-frame-sizes' or a valid value
of it.

Will correct the frame size if any window manager silliness attempts to
make your frame another size.  Adds a hook to preserve the desired frame
size.

🚧 This feature is experimental and has some behaviors that may be
confusing.  A fixed frame will be released if it is converted to full
screen.  Only fixed frames have their size maintained.  When resizing
with a mouse, the resize will appear successful, but then the size will
revert after the first command.  With the right comination of hooks,
these behaviors may become more consistent."
  (interactive (list (completing-read
                      "Select size: "
                      (if (frame-parameter (selected-frame)
                                           'mc--fixed-frame-revert)
                          (cons 'revert mc-fixed-frame-sizes)
                        mc-fixed-frame-sizes))))
  (let* ((frame (selected-frame))
         (revert (string= frame-size "revert"))
         (new (cond
               (revert
                (frame-parameter (selected-frame) 'mc--fixed-frame-revert))
               ((stringp frame-size)
                (cdr (assoc-string frame-size mc-fixed-frame-sizes)))
               ((symbolp frame-size)
                (cdr (assq frame-size mc-fixed-frame-sizes)))
               ((consp frame-size) frame-size)
               (t (error "Unrecognized size: %s" frame-size))))
         (current (if-let ((fullscreen (frame-parameter nil 'fullscreen)))
                      fullscreen
                    (cons (frame-pixel-width)
                          (frame-pixel-height)))))
    (set-frame-parameter nil 'mc--fixed-frame-revert (if revert nil current))
    (mc--fixed-frame-set frame new)
    (when (consp new)
      (if revert
          (set-frame-parameter frame 'mc--fixed-frame-goal nil)
        (set-frame-parameter frame 'mc--fixed-frame-goal new)
        (add-hook 'window-size-change-functions #'mc--fixed-frame-notify)))))

;; * Master of Ceremonies Dispatch
;; Let us tie everything together into.  A transient.

;; There isn't a ton of consistency in how these are used.  Still in the
;; trial-and-error phase of building up an in-transient UI

(defun mc--dispatch-frame-size ()
  (format
   "set %s"
   (if-let ((full (frame-parameter nil 'fullscreen)))
       (propertize (symbol-name full) 'face 'transient-value)
     (propertize (format "%s %s" (frame-pixel-width) (frame-pixel-height))))))

(defun mc--dispatch-fixed-frames ()
  (let ((frames (frame-list))
        (fixed 0))
    (while-let ((frame (pop frames)))
      (when (frame-parameter frame 'mc--fixed-frame-goal)
        (setq fixed (1+ fixed))))
    (format
     "release %-3s"
     (if (> fixed 0)
         (propertize (format "%3s frames" fixed) 'face 'success)
       ""))))

(defun mc--dispatch-cursor-mode ()
  (if-let ((cursor (if (consp cursor-type)
                       (car cursor-type)
                     (if (eq cursor-type t)
                         (frame-parameter nil 'cursor-type)
                       cursor-type))))
      (if mc-subtle-cursor-mode
          (propertize (format "subtle %-4s" cursor)
                      'face 'transient-value)
        (propertize (format "%-11s" (symbol-name cursor))
                    'face 'transient-value))
    (propertize "hidden     " 'face 'shadow)))

(defun mc--dispatch-faces-remapped ()
  (let ((remaps (length mc--face-remap-cookies)))
    (format
     "clear %s"
     (if (> remaps 0)
         (propertize (format "remaps %-4d" remaps) 'face 'success)
       ""))))

(defun mc--dispatch-default-text-scale ()
  (if default-text-scale-mode
      (propertize (format "scale: %s" (face-attribute 'default :height))
                  'face 'transient-value)
    (propertize "off" 'face 'shadow)))

(defun mc--dispatch-text-scale ()
  (if text-scale-mode
      (propertize (format "scale: %s" text-scale-mode-amount)
                  'face 'transient-value)
    (propertize "off" 'face 'shadow)))

(defun mc--dispatch-default-text-scale-mode-p ()
  default-text-scale-mode)

(defun mc--dispatch-text-scale-mode-p ()
  text-scale-mode)

(defun mc--dispatch-quiet-mode ()
  (format
   "quiet %s"
   (if mc-quiet-mode
       (propertize "on " 'face 'success)
     (propertize "off" 'face 'shadow))))

(transient-define-prefix mc-dispatch ()
  "You are the MC.
This is likely the command you want to bind globally to become familiar
with MC commands and to make many adjustments at once."
  :refresh-suffixes t
  [["Default Text Scale"
    (:info #'mc--dispatch-default-text-scale)
    ("+" "increase" default-text-scale-increase :transient t)
    ("-" "decrease" default-text-scale-decrease :transient t)
    ("=" "reset" default-text-scale-reset :transient transient--do-call
     :inapt-if-nil default-text-scale-mode)]
   ["Buffer Text Scale"
    (:info #'mc--dispatch-text-scale)
    ("t+" "increase" text-scale-increase :transient t)
    ("t-" "decrease" text-scale-decrease :transient t)
    ("t=" "reset" text-scale-mode :transient transient--do-call
     :inapt-if-not-nil text-scale-mode)]]
  ["Fixed Frame"
   ("s" mc-fixed-frame-set :transient t
    :description mc--dispatch-frame-size)
   ("R" mc-fixed-frame-release-all :transient t
    :description mc--dispatch-fixed-frames)]
  ["Face Remapping"
   ("r" "remap" mc-face-remap :transient t)
   ("c" mc-face-remap-clear :transient t
    :description mc--dispatch-faces-remapped)]
  [["Cursor"
    (:info #'mc--dispatch-cursor-mode)
    ("h" "hide" mc-hide-cursor-mode :transient t)
    ("." "subtle" mc-subtle-cursor-mode :transient t)]
   ["Mode Line"
    ("m" "hide" hide-mode-line-mode :transient t)]
   ["Echo area"
    ("q" mc-quiet-mode :transient t
     :description mc--dispatch-quiet-mode)]])

(provide 'master-of-ceremonies)
;;; master-of-ceremonies.el ends here

;; Local Variables:
;; outline-regexp: ";; \\(*+\\)"
;; End:
