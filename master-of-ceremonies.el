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

(defcustom mc-focus-max-height-factor 0.75
  "Focused text maximum height fraction.
This is never exceeded."
  :type 'float)

(defcustom mc-focus-max-width-factor 0.75
  "Focused text maximum width fraction.
This is never exceeded."
  :type 'float)

(defcustom mc-focus-max-area-factor 0.40
  "Focused text goal area.
Area conveniently expresses the dependency between height and width.
Text that is extremely long or extremely tall will be limited by
`mc-focus-height-factor-max' and `mc-focus-width-factor-max'.  Text that
is approximately screen-shaped will often be limited by this factor
first.  Screen proportions are taken into account, so width usually has
a larger effect on screen area than height."
  :type 'float)

(defcustom mc-focus-max-scale 20.0
  "Maximum scale of focused text.
When focusing extremely small regions, this value prevents the text from
being scaled comically large.  If you just want to render single symbols
or extremely short expressions, this setting can be used to control
excessively large results."
  :type 'float)

(defcustom mc-focus-default-remaps '(org-block-no-background)
  "A list of remap presets to apply to focused text.
Each symbol is a key of `mc-face-remap-presets'.  You can still manually
apply or clear remaps using `mc-face-remap' and `mc-face-remap-clear'.
The defaults will just be turned on to save time in the usual cases."
  :type '(list symbol))

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

(defcustom mc-face-remap-presets
  '((bold . ((default :weight bold)))
    (org-block-no-background . ((org-block :background nil :extend nil))))
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
;; TODO specified space is better
(defvar-local mc--focus-margin-left nil)
(defvar-local mc--focus-margin-right nil)
(defvar-local mc--focus-old-subtle-cursor nil
  "Whether subtle cursor was active before running MC.")
(defvar-local mc--focus-old-quiet nil
  "Whehter quiet mode was active before running MC.")
(defvar-local mc--focus-old-window-config nil)

(defvar mc--fixed-frame-timer nil)

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
  ;; TODO anonymous remapping, perhaps informed by text properties at point to
  ;; select the correct face?
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
    (if (minibufferp)
        (mc-hide-cursor-mode -1)
      (when mc-subtle-cursor-mode
        (mc-subtle-cursor-mode -1))
      (setq-local cursor-type nil)))
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
      ;; how it's supposed to work.
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
      (progn (message "Frame: %s has become fullscreen.  Releasing." frame)
             (mc--fixed-frame-release frame))
    (when-let ((size (frame-parameter frame 'mc--fixed-frame-goal)))
      (mc--fixed-frame-verify frame size))))

(defun mc--fixed-frame-verify (frame size)
  "Verify FRAME is SIZE or schedule correction."
  (let ((width-correction (- (car size) (frame-pixel-width frame)))
        (height-correction (- (cdr size) (frame-pixel-height frame))))
    (unless (and (= width-correction 0)
                 (= height-correction 0)
                 (null mc--fixed-frame-timer))
      (setq mc--fixed-frame-timer
            (run-with-timer 0.0 nil #'mc--fixed-frame-correct-all)))))

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
      (message "corrected size: %sw %sh"
               (frame-pixel-width frame)
               (frame-pixel-height frame)))))

(defun mc--fixed-frame-correct-all ()
  "Used as a single-call post-command hook to avoid thrashing."
  ;; Updating the frame size during the `window-size-change-functions' is not a
  ;; good idea.  Temporarily removing the hook was an ineffective strategy in
  ;; this case.  Instead, this function runs in the post command hook and, if
  ;; added, corrects all frames and removes itself.
  (setq mc--fixed-frame-timer nil)
  (dolist (frame (frame-list))
    (if (frame-parameter frame 'fullscreen)
        ;; Only frames with a non-fullscreen size are guarded, so we bail if they
        ;; have acquired a fullscreen parameter.
        (progn (message "Frame: %s has become fullscreen.  Releasing." frame)
               (mc--fixed-frame-release frame))
      (when-let ((size (frame-parameter frame 'mc--fixed-frame-goal)))
        (mc--fixed-frame-correct frame size)))))

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
          (message "set size: %sw %sh"
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
   "current: %s"
   (propertize
    (if-let ((full (frame-parameter nil 'fullscreen)))
        (symbol-name full)
      (format "%s %s" (frame-pixel-width) (frame-pixel-height)))
    'face 'transient-value)))

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

;;;###autoload
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
     :inapt-if-non-nil text-scale-mode)]]
  ["Fixed Frame"
   (:info #'mc--dispatch-frame-size)
   ("s" "set" mc-fixed-frame-set :transient t)
   ("R" mc-fixed-frame-release-all :transient t
    :description mc--dispatch-fixed-frames)]
  ["Face Remapping"
   ("r" "remap" mc-face-remap :transient t)
   ("c" mc-face-remap-clear :transient t
    :description mc--dispatch-faces-remapped)]
  [["Cursor"
    (:info #'mc--dispatch-cursor-mode)
    ("?" "hide" mc-hide-cursor-mode :transient t)
    ("." "subtle" mc-subtle-cursor-mode :transient t)]
   ["Mode Line"
    ("m" "hide" hide-mode-line-mode :transient t)]
   ["Echo area"
    ("e" mc-quiet-mode :transient t
     :description mc--dispatch-quiet-mode)]])

;; * Screenshot

;; 🚧 If you consider working on this feature, support for other file type
;; support and naming support for workflows like animation are good to add along
;; the way.  There are other packages for building gifs etc that would be
;; welcome in MC as optional dependencies.

(defun mc--screenshot-save-path ()
  (if (stringp mc-screenshot-path)
      mc-screenshot-path
    (if (functionp mc-screenshot-path)
        (or (funcall mc-screenshot-path)
            default-directory)
      default-directory)))

(defun mc-screenshot ()
  "Save a screenshot of the current frame as an SVG image.
This just provides minor conveniences like pre-configured save path with
`mc-screenshot-path'."
  (interactive)
  (let* ((timestamp (format-time-string "%F-%H:%M:%S" (current-time)))
         (filename (format "screenshot-%s.svg" timestamp))
         (dir (mc--screenshot-save-path))
         (path (concat dir filename))
         (data (x-export-frames nil 'svg)))
    (unless (file-exists-p dir)
      (make-directory dir t))
    (with-temp-file path
      (insert data))
    (message "Saved to: %s" filename)))

;; * Focus fullscreen text

;; 🚧 This feature has mostly been implemented out of a collection of proofs of
;; concept.  Do not trust or respect this code.  Most of it will need to be
;; rewritten while adding new features.

;; Only add to the `buffer-list-update-hook' locally so we don't need to unhook
(defun mc--focus-refresh (window)
  (if (eq (window-buffer window) (get-buffer "*MC Focus*"))
      (set-window-margins
       window mc--focus-margin-left mc--focus-margin-right)))

(defun mc--focus-clean-properties (text)
  (let ((dirty-props (object-intervals text))
        (clean-string (substring-no-properties text)))
    (mapc
     (lambda (interval)
       (let ((begin (pop interval))
             (end (pop interval)))
         (mapc
          (lambda (prop-name)
            (when-let ((prop (plist-get (car interval) prop-name)))
              (put-text-property begin end prop-name prop clean-string)))
          '(face font-lock-face button invisible display))))
     dirty-props)
    clean-string))

(defun mc--focus-translate-overlays (text overlays beg _end buffer)
  (let ((max-pos (1+ (length text))))
    (mapcar
     (lambda (o)
       (when-let* ((old-start (overlay-start o))
                   (old-end (overlay-end o))
                   (new-start (max (- old-start (1- beg)) 1))
                   (new-end (min (- old-end (1- beg)) max-pos))
                   (clone (make-overlay new-start new-end buffer))
                   (props (overlay-properties o)))
         (while-let ((key (pop props))
                     (val (pop props)))
           (overlay-put clone key val))
         (overlay-put clone 'evaporate t)
         clone))
     (cdr overlays))))

(defun mc--focus-cleanup ()
  (remove-hook 'window-state-change-functions #'mc--focus-refresh)
  ;; hidden cursor is buffer local and naturally goes away, but subtle cursor is
  ;; global and needs to be turned off if it wasn't on when focusing began.
  (if (not mc--focus-old-quiet)
      (when mc-quiet-mode
        (mc-quiet-mode -1))
    (setq mc--focus-old-quiet nil)
    (unless mc-quiet-mode
      (mc-quiet-mode 1)))
  (if (not mc--focus-old-subtle-cursor)
      (when mc-subtle-cursor-mode
        (mc-subtle-cursor-mode -1))
    (setq mc--focus-old-subtle-cursor nil)
    (unless mc-subtle-cursor-mode
      (mc-subtle-cursor-mode 1)))

  (when mc--focus-old-window-config
    (set-window-configuration mc--focus-old-window-config))
  (setq mc--focus-old-window-config nil
        mc--focus-cleaned-text nil))

;; ⚠️ This code is very much a collection of proofs of concept.  Very little of
;; it will likely be stable or is of high quality.  You may want to reduce to
;; just your use case before attempting to implement a new feature.
(defun mc--display-fullscreen (text &optional invisibility-spec overlays beg end)
  "Show TEXT with properties in a fullscreen window.
🚧 This function is under active development.  The signature is likely
to change to plist style, using keyword arguments to be more stable /
user-friendly."
  (when-let ((old (get-buffer "*MC Focus*")))
    (kill-buffer old))
  (setq mc--focus-old-window-config (current-window-configuration))
  (let* ((buffer (get-buffer-create "*MC Focus*"))
         (text (mc--focus-clean-properties text)))
    (delete-other-windows)
    (let ((inhibit-message t))
      (switch-to-buffer buffer))
    (add-hook 'kill-buffer-hook #'mc--focus-cleanup nil t)
    (mc-focus-mode)
    (setq-local mode-line-format nil)
    (setq buffer-invisibility-spec invisibility-spec)
    (setq mc--focus-old-quiet
          mc-quiet-mode)
    (setq mc--focus-old-subtle-cursor
          mc-subtle-cursor-mode)
    (mc-hide-cursor-mode 1)
    (mc-quiet-mode 1)
    (read-only-mode -1)

    ;; Before we start adding properties, save the input text without additional
    ;; properties.
    (setq-local mc--focus-cleaned-text text)

    (insert (propertize text
                        'line-prefix nil
                        'wrap-prefix nil))

    ;; apply translated overlays after buffer has text
    ;; ⚠️ This method is totally not going to work.  Translation, rectangle, and
    ;; trimming all have to work together.  Also max line length support is
    ;; needed for visual lines.
    (when (and overlays beg end)
      (mc--focus-translate-overlays text overlays beg end buffer))
    ;; TODO serialize overlays for playback

    (let* ((w (window-pixel-width))
           (h (window-pixel-height))
           (window-pixel-area (* h w))
           (text-pixel-size (window-text-pixel-size))
           (text-pixel-w (float (car text-pixel-size)))
           (text-pixel-h (float (cdr text-pixel-size)))
           (text-pixel-area (* text-pixel-w text-pixel-h))
           (max-scale-horizontal (/ (* w mc-focus-max-width-factor)
                                    text-pixel-w))
           (max-scale-vertical (/ (* h mc-focus-max-height-factor)
                                  text-pixel-h))
           (max-scale-by-area (/ (* window-pixel-area
                                    mc-focus-max-area-factor)
                                 text-pixel-area))
           (scale (min max-scale-horizontal
                       max-scale-vertical
                       max-scale-by-area
                       mc-focus-max-scale))
           (scale-overlay (make-overlay 1 (point-max))))
      (overlay-put scale-overlay 'face `(:height ,scale))

      (mapc (lambda (remap) (mc-face-remap remap t))
            mc-focus-default-remaps)

      ;; Now that the text is its final size, adjust the margins and vertical
      ;; spacing
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

        (add-hook 'window-state-change-functions #'mc--focus-refresh)

        (goto-char 0)
        (insert (propertize "\n" 'face `(:height ,margin-lines)))
        (setf (overlay-start scale-overlay) 2)
        (setf (overlay-end scale-overlay) (point-max))))
    (read-only-mode 1)))

(defvar-keymap mc-focus-mode-map
  :suppress 'nodigits
  "." #'mc--focus-cursor-toggle
  "c" #'mc-face-remap-clear
  "e" #'mc-quiet-mode
  "h" #'mc-focus-dispatch
  "l" #'mc-focus-highlight
  "q" #'mc-focus-quit
  "r" #'mc-face-remap
  "s" #'mc-screenshot
  "u" #'mc-focus-un-highlight
  "U" #'mc-focus-highlight-clear
  "w" #'mc-focus-kill-ring-save)

(define-derived-mode mc-focus-mode special-mode
  "Modal controls for focus windows."
  :interactive nil)

(defsubst mc--focus-assert-mode ()
  (if-let ((buffer (get-buffer "*MC Focus*")))
      (set-buffer buffer)
    (user-error "No MC buffer found")))

(defun mc-focus-highlight-clear ()
  "Delete overlays."
  (interactive)
  (mc--focus-assert-mode)
  (unless mc--focus-highlights
    (user-error "No highlights to un-highlight"))
  (setq mc--focus-highlights nil)
  (mapc #'delete-overlay mc--focus-highlight-overlays)
  (setq mc--focus-highlight-overlays nil))

(put 'mc-focus-highlight-clear 'mode 'mc-focus-mode)

(defun mc-focus-quit ()
  "Fullscreen quit command."
  (interactive)
  (if-let ((buffer (get-buffer "*MC Focus*")))
      (kill-buffer buffer)
    (user-error "No MC buffer found")))

(put 'mc-focus-quit 'mode 'mc-focus-mode)

(defun mc-focus-highlight (beg end)
  "Highlight region between BEG and END.
The shadow face will be applied to remaining unhighlighted regions."
  (interactive "r")
  (mc--focus-assert-mode)
  (mc--focus-highlight beg end)
  ;; unnecessary to deactivate the mark when called any other way
  (when (called-interactively-p 't)
    (deactivate-mark))
  (mc--focus-apply-highlights mc--focus-highlights))

(put 'mc-focus-highlight 'mode 'mc-focus-mode)

(defun mc-focus-un-highlight (beg end)
  "Remove highlight in region between BEG and END.
The shadow face will be added to the region between BEG and END."
  (interactive "r")
  (mc--focus-assert-mode)
  (unless mc--focus-highlights
    (user-error "No highlights to un-highlight"))
  (mc--focus-un-highlight beg end)
  ;; unnecessary to deactivate the mark when called any other way
  (when (called-interactively-p 't)
    (deactivate-mark))
  (mc--focus-apply-highlights mc--focus-highlights))

(put 'mc-focus-un-highlight 'mode 'mc-focus-mode)

(defun mc--focus-apply-highlights (highlights)
  "Use to replay HIGHLIGHTS from Elisp programs.
HIGHLIGHTS is a list of conses of BEG END to be highlighted.  Regions
not contained by some BEG END will have the shadow face applied.
HIGHLIGHTS must be partially ordered and with no overlaps or else
behavior is not guaranteed."
  (let (un-highlights left right)
    (mapc #'delete-overlay mc--focus-highlight-overlays) ; 🤡 almost forgot
    ;; no highlights means shadow everything
    (unless highlights
      (push (cons (point-min) (point-max))
            un-highlights))
    ;; before the first highlight
    (unless (or (null highlights)
                (and highlights
                     (= (caar highlights) (point-min))))
      (push (cons (point-min) (caar highlights))
            un-highlights))
    ;; un-highlight in between every two highlights
    (setq left (pop highlights))
    (setq right (pop highlights))
    (while right
      (push (cons (cdr left) (car right))
            un-highlights)
      (setq left right)
      (setq right (pop highlights)))
    ;; after the last highlight
    (unless (or (null left)
                (and left (= (cdr left) (point-max))))
      (push (cons (cdr left) (point-max))
            un-highlights))
    ;; apply all unhighlights
    (dolist (h un-highlights)
      (let ((o (make-overlay (car h) (cdr h))))
        ;; TODO customize un-highlight face
        (overlay-put o 'face 'shadow)
        (push o mc--focus-highlight-overlays)))))

(defun mc--focus-un-highlight (beg end)
  "Remove region between BEG and END from highlights.
Preserves total ordering of highlighted spans."
  (let ((highlights mc--focus-highlights)
        keep)
    (while-let ((h (pop highlights)))
      ;; If BEG and END include either or both ends of a highlight, we have to
      ;; modify spans.
      (let ((h-beg-interior (and (>= (car h) beg)
                                 (<= (car h) end)))
            (h-end-interior (and (>= (cdr h) beg)
                                 (<= (cdr h) end)))
            (h-beg-before (< (car h) beg))
            (h-end-after (> (cdr h) end)))
        (cond
         ;; fully contained highlights are omitted
         ((and h-beg-interior h-end-interior) nil)
         ;; intersected highlights are trimmed
         (h-beg-interior (push (cons end (cdr h)) keep))
         (h-end-interior (push (cons (car h) beg) keep))
         ;; split highlights that contain un-highlight
         ((and h-beg-before h-end-after)
          (push (cons (car h) beg) keep)
          (push (cons end (cdr h)) keep))
         (t (push h keep)))))
    (setq mc--focus-highlights (nreverse keep))))

(defun mc--focus-highlight (beg end)
  "Add region between BEG and END to highlights.
Preserves total ordering of highlighted spans."
  (let ((highlights mc--focus-highlights)
        keep merge)
    ;; push all regions ending before beg
    (while (and (car highlights)
                (< (cdar highlights) beg))
      (push (pop highlights) keep))
    ;; merge all regions that overlap or are adjacent
    (setq merge (cons beg end))
    (while (and highlights
                (or (and (>= (caar highlights) beg)
                         (<= (caar highlights) end))
                    (and (>= (cdar highlights) beg)
                         (<= (cdar highlights) end))))
      (setq merge (cons (min (caar highlights) beg)
                        (max (cdar highlights) end)))
      (pop highlights))
    (push merge keep)
    ;; push remaining regions
    (while highlights
      (push (pop highlights) keep))
    (setq mc--focus-highlights (nreverse keep))))

(defun mc-focus-kill-ring-save ()
  "Save the focused text and highlights to a playback expression."
  (interactive)
  ;; TODO kill ring save needs a lot of updates for playback.  overlays and
  ;; invisibility spec are completely unsupported.
  (mc--focus-assert-mode)
  (let ((expression
         `(mc-focus
           ,mc--focus-cleaned-text
           ',mc--focus-highlights)))
    (kill-new (prin1-to-string expression)))
  (message "saved focus to kill ring"))

(put 'mc-focus-kill-ring-save  'mode 'mc-focus-mode)

;;;###autoload
(defun mc-focus (text &optional highlights)
  "Focus selected region or prompt for TEXT.
Optional HIGHLIGHTS is a list of (BEG END).

🚧 This function and its signature are likely to evolve.  The base
interactive use case of highlighting a region is stable and very useful.
Expect playback of saved focuses to be unstable."
  ;; 🚧 The code looks ugly.  It is.  It just needs a bit of re-architecture to
  ;; handle rectangles and overlay merging.  Invisibility and text property
  ;; support were recently hacked in.  As a POC, it even kind of does images.
  ;; With face remapping, it can re-style focused regions to remove annoying
  ;; artifacts of the area the text was extracted from.  Hell maybe we can +/-
  ;; support for diffs by merging more text properties.  It's bad, but it's so
  ;; good, I had to ship it bad.
  (interactive
   (list (if (region-active-p)
             (funcall (if rectangle-mark-mode
                          (lambda (beg end)
                            (string-join (extract-rectangle beg end)
                                         "\n"))
                        #'buffer-substring)
                      (region-beginning) (region-end))
           (read-string "enter focus text: "))))

  ;; TODO translating overlays to rectangles....  Oh god.  What you need is to
  ;; gather up each string and extract the properties and overlays and use the
  ;; rectangle case as the base case for merging / toggling inside the focus
  ;; buffer.

  (if rectangle-mark-mode
      (mc--display-fullscreen
       text buffer-invisibility-spec)
    (if (region-active-p)
        (mc--display-fullscreen
         text buffer-invisibility-spec
         (overlays-in (region-beginning) (region-end))
         (region-beginning) (region-end))
      (mc--display-fullscreen
       text buffer-invisibility-spec)))
  (when highlights
    (mc--focus-apply-highlights highlights)))

(defun mc--focus-dispatch-screenshot-path ()
  (propertize (mc--screenshot-save-path) 'face 'transient-value))

(defun mc--focus-dispatch-highlights ()
  (if mc--focus-highlights
      (concat
       "clear "
       (propertize
        (format "%s highlights" (length mc--focus-highlights))
        'face 'transient-value))
    "clear all"))

(defun mc--focus-cursor-toggle ()
  (interactive)
  (if mc-subtle-cursor-mode
      (mc-subtle-cursor-mode -1)
    (mc-subtle-cursor-mode 1))
  (unless mc-subtle-cursor-mode
    (mc-hide-cursor-mode 1)))

(put 'mc--focus-cursor-toggle 'mode 'mc-focus-mode)

;; Keep this in sync with `mc-focus-mode-map`!
(transient-define-prefix mc-focus-dispatch ()
  "Transient menu for MC Focus mode."
  :transient-non-suffix t
  [["Highlights"
    ("l" "highlight" mc-focus-highlight)
    ("u" "un-highlight" mc-focus-highlight-clear
     :inapt-if-nil mc--focus-highlights)
    ("U" mc-focus-highlight-clear
     :inapt-if-nil mc--focus-highlights
     :description mc--focus-dispatch-highlights)]
   ["Face Remapping"
    ("r" "remap" mc-face-remap)
    ("c" mc-face-remap-clear
     :description mc--dispatch-faces-remapped)]
   ["Save"
    (:info #'mc--focus-dispatch-screenshot-path)
    ("s" "screenshot" mc-screenshot)
    ("w" "kill ring" mc-focus-kill-ring-save)]]
  [["Cursor"
    (:info #'mc--dispatch-cursor-mode)
    ("." "toggle" mc--focus-cursor-toggle)]
   ["Echo area"
    ("e" mc-quiet-mode
     :description mc--dispatch-quiet-mode)]]
  ["Quit"
   ("q" "quit" mc-focus-quit)])

(put 'mc-focus-dispatch 'mode 'mc-focus-mode)

(provide 'master-of-ceremonies)
;;; master-of-ceremonies.el ends here

;; Local Variables:
;; outline-regexp: ";; \\(*+\\)"
;; End:
