;; (defvar def '(a b c d))
;; (mc/dump-list 'def(setq def
;;                         '(
;;                           a
;;                           b
;;                           c
;;                           d
;;                           )))

;; (:inverse-video)
;; (make-symbol "def")

;; (defface emc-cursor-face
;;   '((t (:inverse-video t)))
;;   "The face used for fake cursors"
;;   :group 'multiple-cursors)

(defface emc-cursor-face
  '((((class color) (min-colors 88) (background dark))
     :background "#389867")
    (((class color) (min-colors 88) (background light) (type gtk))
     :distant-foreground "gtk_selection_fg_color"
     :background "gtk_selection_bg_color")
    (((class color) (min-colors 88) (background light) (type ns))
     :distant-foreground "ns_selection_fg_color"
     :background "ns_selection_bg_color")
    (((class color) (min-colors 88) (background light))
     :background "lightgoldenrod2")
    (((class color) (min-colors 16) (background dark))
     :background "blue3")
    (((class color) (min-colors 16) (background light))
     :background "lightgoldenrod2")
    (((class color) (min-colors 8))
     :background "blue" :foreground "white")
    (((type tty) (class mono))
     :inverse-video t)
    (t :background "gray"))
  "Basic face for highlighting the region."
  :version "21.1"
  :group 'basic-faces)

(defface emc-cursor-inverse-face
  '((t (:inverse-video t :line-width 1)))
  "The face used for fake cursors"
  :group 'emc-multiple-cursors)

(defface emc-cursor-normal-state
  '((((background dark))
     (:box (:line-width -1 :color "#D13A82") :height 1 :background "#0A3641"))
    (t (:box (:line-width -1 :color "#D13A82") :height 1 :background "gray90")))
  "The face used for fake cursors in normal state.")

(defface emc-cursor-insert-state
  '((((background dark))
     (:underline (:line-width -1 :color "#D13A82") :height 1 :background "#0A3641"))
    (t (:underline (:line-width -1 :color "#D13A82") :height 1 :background "gray90")))
  "The face used for fake cursors in insert state.")

;; (insert-char (string-to-char "|"))

(defvar emc-running-command nil
  "True when running a command for all cursors.")

(defvar emc-cursor-command nil
  "True if the current command is an emc cursor command.")

(defvar emc-debug nil
  "If true print debug information.")

(defface emc-region-face
  '((t :inherit region))
  "The face used for fake regions"
  :group 'multiple-cursors)

(evil-define-local-var emc-cursor-list nil
  "The list of current cursors")

(evil-define-local-var emc-pattern nil
  "The current pattern")

(defun emc-make-cursor-at-eol (pos)
  "Make a cursor overlay at POS assuming pos is at the end of line."
  (let ((overlay (make-overlay pos pos nil nil nil)))
    (overlay-put overlay 'after-string (propertize " " 'face 'emc-cursor-normal-state))
    (overlay-put overlay 'type 'emc-fake-cursor)
    (overlay-put overlay 'priority 100)
    overlay))

(defun emc-make-cursor-inline (pos)
  "Make a cursor at POS assuming pos is not at the end of line."
  (let ((overlay (make-overlay pos (1+ pos) nil nil nil)))
    (overlay-put overlay 'face 'emc-cursor-normal-state)
    (overlay-put overlay 'type 'emc-fake-cursor)
    (overlay-put overlay 'priority 100)
    overlay))

(defun emc-make-cursor-at-point ()
  "Create a cursor overlay at point"
  (interactive)
  (if (eolp)
      (emc-make-cursor-at-eol (point))
    (emc-make-cursor-inline (point))))

(defun emc-remove-overlay-at-point ()
  "Remove the overlay at point."
  (interactive)
  (remove-overlays (point) (1+ (point))))

(defun emc-remove-cursor-at-point ()
  "Remove the cursor at point."
  (emc-remove-overlay-at-point))

(defun emc-goto-next-match (pattern &optional direction)
  "Go to the next match of PATTERN optionally in DIRECTION or 'forward."
  (let ((pat (evil-ex-make-search-pattern pattern))
        (dir (or direction 'forward)))
    (setq evil-ex-search-pattern pat)
    (setq evil-ex-search-direction dir)
    (evil-ex-find-next pat dir t)))

(defun emc-create-next-cursor (pattern &optional direction)
  "Create next cursor for PATTERN in DIRECTION."
  (interactive "r")
  (let* ((dir (or direction 'forward))
         (next (emc-goto-next-match pattern dir)))
    (if next (emc-make-cursor-at-point)
      (error "No more matches"))))

(defun emc-get-visual-selection ()
  "Gets the current visual selection"
  (if (evil-visual-state-p)
      (let* ((range (evil-visual-range))
             (start (car range))
             (end (car (cdr range))))
        (buffer-substring-no-properties start end))))

(defun emc-print-visual-selection ()
  "Prints the current visual selection."
  (interactive)
  (let ((visual (emc-get-visual-selection)))
    (if visual (message visual)
      (error "No visual selection found"))))

;; (emc-remove-cursors "next")

(defun emc-create-next-cursor-for-visual-selection ()
  "Create next cursor for the selected text."
  (interactive)
  (let ((pattern (emc-get-visual-selection)))
    (if pattern (emc-create-next-cursor pattern)
      (error "No visual selection found"))))

(defun emc-set-pattern-from-visual-selection ()
  "Stores the pattern delimited by the current visual region along with its position."
  (let* ((range (evil-visual-range))
         (start (car range))
         (end (car (cdr range)))
         (pattern (buffer-substring-no-properties start end)))
    (setq emc-pattern (cons pattern (cons end start)))))

(defun emc-get-pattern ()
  "Gets the current pattern if any."
  (when emc-pattern (car emc-pattern)))

(defun emc-get-position ()
  "Gets the position of the current pattern if any."
  (when emc-pattern (cdr emc-pattern)))

(defun emc-add-cursor (cursor)
  "Add CURSOR to the cursors list."
  (setq emc-cursor-list (cons cursor emc-cursor-list)))

;; (emc-goto-next-match (emc-get-pattern))

(defun emc-goto-next ()
  "Go to next pattern given by `emc-get-pattern'."
  (interactive)
  (when (emc-get-pattern)
    (emc-go-to-next-match (emc-get-pattern))))

(defun emc-maybe-goto-next-match ()
  "Create a cursor at point and go to next match if any."
  (let ((cursor (emc-make-cursor-at-point)))
    (evil-exit-visual-state)
    (if (emc-goto-next-match (emc-get-pattern) 'forward)
        (emc-add-cursor cursor)
      (delete-overlay cursor)
      (error "No more matches found"))))

(evil-define-command emc-make-next-cursor ()
  "Make the next cursor."
  :repeat ignore
  (interactive)
  (let ((emc-cursor-command t)) ;; TODO: this is no longer needed, but need to ensure that no commands are recorded during cursors creatio
    (setq emc-command-info nil)
    (cond ((evil-visual-state-p)
           (emc-set-pattern-from-visual-selection)
           (emc-maybe-goto-next-match))
          ((emc-get-pattern) (emc-maybe-goto-next-match))
          (t (error "No more matches or no visual selection found")))))

(evil-define-command emc-remove-all-cursors ()
  "Remove all cursor overlays."
  :repeat ignore
  (interactive)
  (remove-overlays)
  (setq emc-cursor-list nil)
  (setq emc-pattern nil))

(defun emc-setup-key-maps ()
  "Sets up all key bindings for working with multiple cursors."
  (define-key evil-visual-state-local-map (kbd "C-n") 'emc-make-next-cursor)
  (define-key evil-normal-state-local-map (kbd "C-n") 'emc-make-next-cursor)
  (define-key evil-normal-state-local-map (kbd "C-,") 'emc-remove-all-cursors))

(emc-setup-key-maps)

;; (remove-overlays)
;; (setq emc-pattern nil)

;; (evil-visual-state-p)
;; (evil-visual-range)
;; (buffer-substring-no-properties 2513 2518)
;; (buffer-substring-no-properties 2517 2566)
;; (emc-remove-cursors "make")
;; (emc-create-next-cursor "make" 'forward)
;; (emc-create-cursors "make")
;; (emc-create-cursors "not-found")
;; (emc-goto-next-match "[0-9]+")

(defun emc-create-cursors (pattern)
  "Creates cursors for all matches of PATTERN."
  (let ((orig (point)))
    (while (emc-goto-next-match pattern 'forward)
      (emc-make-cursor-at-point))
    (goto-char orig)))

(defun emc-remove-cursors (pattern)
  "Removes cursors for all matches of PATTERN."
  (let ((orig (point)))
    (while (emc-goto-next-match pattern 'forward)
      (emc-remove-cursor-at-point))
    (goto-char orig)))

;;; Run command

;; this-command
;; (this-command-keys)

(defsubst emc-command-recording-p ()
  "Return t only if a recording is in progress."
  (eq emc-command-recording t))

(defun emc-command-record (info)
  "Add INFO to the end of `emc-command-info'."
  (when (emc-command-recording-p)
    (setq emc-command-info (nconc emc-command-info (list info)))))


(defun emc-run-keystrokes (flag)
  "Runer for commands that are run by keystrokes."
  (cond ((eq flag 'pre)
         (when evil-this-register
           (emc-command-record `(set evil-this-register ,evil-this-register))))
        ((eq flag 'post)
         (emc-command-record (if (zerop (length (this-command-keys)))
                                 evil-repeat-keys
                               (this-command-keys)))))
  ;; Unfinished, see `evil-repeat-keystrokes'
  )

(defun emc-run-motion (flag)
  "Runner for motions."
  (when (memq evil-state '(insert replace normal))
    (emc-run-keystrokes flag)))

(defun emc-run-changes (flag)
  "Runner for buffer changes."
  (cond ((eq flag 'pre)
         (add-hook 'after-change-functions #'emc-run-change-hook nil t)
         (emc-run-start-record-changes))
        ((eq flag 'post)
         (remove-hook 'after-change-functions #'emc-run-change-hook t)
         (emc-run-finish-record-changes))))

(defun emc-run-change-hook (beg end len)
  "Record change information for the current command."
  (message "Not implemented, see `evil-repeat-change-hook'."))

(defun emc-run-start-record-changes ()
  "Starts the recording of a new set of buffer changes."
  (message "Not implemented, see `evil-repeat-start-record-changes'."))

(defun emc-run-finish-record-changes ()
  "Starts the recording of a new set of buffer changes."
  (message "Not implemented, see `evil-repeat-finish-record-changes'."))

(defvar emc-command-types
  '((t . emc-run-keystrokes)
    (change . emc-run-changes)
    (motion . emc-run-motion)
    (insert-at-point . emc-run-insert-at-point)
    (ignore . nil))
  "An alist of defined command-types.")

(defvar emc-command-recording nil
  "Whether we are recording a command.")

(defvar emc-command-info nil
  "Information accumulated during the current command.")

(defvar emc-command-buffer nil
  "The buffer in which the command started.
If the buffer is change, the command is cancelled.")

(defun emc-command-abort-p (type)
  "Returns t if the current command of TYPE should not run for all cursors."
  ;; TODO: add more checks, see `evil-repeat-force-abort-p'
  (or (eq type 'abort)
      (evil-emacs-state-p)
      (and (evil-mouse-events-p (this-command-keys))
           (null type))
      (minibufferp)))

(defun emc-command-reset (flag)
  "Clear all command recording variables and set `emc-command-recording' to FLAG."
  (setq emc-command-recording flag
        emc-command-info nil
        emc-command-buffer nil))

(defun emc-command-abort ()
  "Mark the current command as aborted."
  (emc-command-reset 'abort))

(defun emc-command-record-buffer ()
  "Set `emc-command-buffer' to the current buffer."
  (unless (minibufferp)
    (setq emc-command-buffer (current-buffer))))

(defun emc-command-start ()
  "Start recording a new command into `emc-command-info'."
  (emc-command-reset t)
  (emc-command-record-buffer))

(defun emc-command-type (command &optional default)
  "Return the :repeat property of a COMMAND or DEFAULT."
  (when (functionp command)
    (let* ((type (evil-get-command-property command :repeat default))
           (command-type (assq type emc-command-types)))
      (if command-type (cdr command-type) type))))


(defun emc-print-point ()
  "Print the point location."
  (interactive)
  (message "Point is at %s" (point)))

;; (let ((overlay (make-overlay 0 0)))
;;   (move-overlay overlay 11088 11089)
;;   (overlay-put overlay 'face 'emc-cursor-face))

;; (overlays-at 11088)

;; (remove-overlays)

(defun emc-run-on-pre-command ()
  "Prepare for recording the current command."
  (when evil-local-mode
    (let ((command-type (emc-command-type this-command t)))
      (cond ((emc-command-abort-p command-type) (emc-command-abort))
            ((null command-type))
            (t (when (or (evil-normal-state-p)
                         (evil-visual-state-p))
                 (emc-command-start))
               (setq emc-command-recording t)
               (funcall command-type 'pre))))))
(put 'emc-run-on-pre-command 'permanent-local-hook t)

;; (evil-get-command-properties 'evil-repeat-visual-char)
;; (evil-get-command-properties 'evil-execute-change)
;; (evil-get-command-properties 'evil-snipe-f)

(defun emc-run-on-post-command ()
  "Apply the current command to all cursors."
  (message "TODO: implement emc-run-on-post-command"))

(defun emc-run-last-command-for-all-cursors ()
  "Runs the last command for all cursors."
  (interactive)
  (unless (or emc-running-command (evil-visual-state-p))
    (setq emc-running-command t)
    (save-excursion
      (dolist (cursor emc-cursor-list)
        (let ((start (overlay-start cursor)))
          (goto-char start)
          (evil-repeat 1))))
    (setq emc-running-command nil)))

(defun emc-print-command ()
  (prin1 (cons this-command (cons (emc-command-type this-command)
                                  (this-command-keys)))))

(defun emc-print-evil-register ()
  (setq emc-command-recording t)
  (when evil-this-register
    (setq emc-command-info `(set evil-this-register ,evil-this-register)))
  (setq emc-command-recording nil))

(defun emc-debug-on ()
  "Turn debug on."
  (interactive)
  (setq emc-debug t))

(defun emc-debug-off ()
  "Turn debug off."
  (interactive)
  (setq emc-debug nil))

(defun emc-print-this-command (msg)
  "Print info about `this-command' prefixed with MSG."
  (when emc-debug
    (message "%s: command %s keys %s vector %s raw %s last event %s last command %s"
             msg
             this-command
             (this-command-keys)
             (this-command-keys-vector)
             (this-single-command-raw-keys)
             (cons last-input-event (emc-key-to-string last-input-event))
             last-command)))

(defun emc-clear-this-command ()
  "Remove all info saved for the current command."
  (setq emc-command-info nil))

(defun emc-get-this-command ()
  "Return the name of the current command."
  (when emc-command-info (car emc-command-info)))

(defun emc-get-last-command ()
  "Return the name of the last command."
  (when emc-command-info (car (cdr emc-command-info))))

(defun emc-get-this-command-keys ()
  "Return the keys of the current command."
  (when emc-command-info
    (let ((rest (cdr emc-command-info)))
      (when rest (car (cdr rest))))))

(defun emc-set-command-info (this last keys)
  "Sets the command info to THIS command LAST command and KEYS."
  (setq emc-command-info (cons this (cons last (cons keys nil)))))

(defun emc-set-this-command (name)
  "Set the current command NAME."
  (let ((last (emc-get-last-command))
        (keys (emc-get-this-command-keys)))
    (emc-set-command-info name last keys)))

(defun emc-set-last-command (name)
  "Set the last command NAME."
  (let ((this (emc-get-this-command))
        (keys (emc-get-this-command-keys)))
    (emc-set-command-info this name keys)))

(defun emc-normalize-keys (keys)
  "Normalize the given keys."
  (cond ((null keys) nil)
        ((vectorp keys) (listify-key-sequence keys))
        ((atom keys) (cons keys nil))
        (t keys)))

(defun emc-append-this-comand-keys (new-keys)
  "Adds NEW-KEYS to the current comand."
  (let* ((this (emc-get-this-command))
         (last (emc-get-last-command))
         (keys (emc-get-this-command-keys))
         (more (emc-normalize-keys new-keys)))
    (emc-set-command-info
     this last (if (null keys) more (append keys more)))))

;; TODO: save command and parameters
;; command, last-command, (keys: vector-pre vector-post)
(defun emc-begin-save-command ()
  "Begin saving the current command if it is a supported command in `emc-command-info'."
  (when (not emc-running-command)
    (emc-print-this-command "PRE")
    (let ((cmd (or (command-remapping this-original-command) this-original-command)))
      (when (emc-supported-command-p cmd)
        (emc-clear-this-command)
        (emc-set-this-command cmd)
        (emc-set-last-command last-command)
        (emc-append-this-comand-keys (this-command-keys-vector))))))

(defun emc-finish-save-command ()
  "Finish saving the current command. This should be called from `post-command-hook'."
  (when (not emc-running-command)
    (emc-print-this-command "POST")
    (emc-append-this-comand-keys (this-command-keys-vector))))

(defun emc-position-cursors-after-insert ()
  "Re-position the cursors when exiting insert state."
  (when (not emc-running-command)
    (emc-clear-this-command)
    (emc-set-this-command 'evil-backward-char)
    (emc-run-motion-for-all-cursors)))

;; (evil-get-register ?-)

(defun emc-running-command-p ()
  "Return true when running a command for all fake cursors."
  (eq emc-running-command t))

;; TODO: find motions interfere with the cursors commands (especially insert f)
;; TODO: abort comand hooks functinality if (evil-emacs-state-p)

;; TODO: make delete char work
;; (evil-delete-char (1- (point)) (point))

;; (evil-replace (point) (1+ (point)) nil (string-to-char "k"))

;; TODO: (self-insert-command . f) causes an infinite loop

;; (progn
;;   (evil-forward-word-end)
;;   (evil-delete-backward-word))

;; (execute-kbd-macro "bbbbbcw")

;; add command properties with (evil-add-command-properties)

;; keys this-command-keys this-single-command-raw-keys last-in

(defun emc-command-info-p ()
  "True if there is a command stored in `emc-command-info'."
  (emc-get-this-command))

(defun emc-key-to-string (key)
  "Converts a key to a string."
  (cond ((characterp key) (char-to-string key))
        ((null key) nil)
        (t key)))

(defun emc-supported-command-p (cmd)
  "Return true if CMD is supported for multiple cursors."
  (or (eq (emc-command-type cmd) 'emc-run-motion)
      (eq cmd 'backward-delete-char-untabify)
      (eq cmd 'delete-backward-char)
      (eq cmd 'newline-and-indent)
      (eq cmd 'evil-delete-backward-char-and-join)
      (eq cmd 'evil-delete-backward-word)
      (eq cmd 'evil-delete)
      (eq cmd 'evil-repeat)
      (eq cmd 'evil-change)
      (eq cmd 'evil-change-line)
      (eq cmd 'evil-delete-char)
      (eq cmd 'evil-append)
      (eq cmd 'evil-replace)
      (eq cmd 'evil-open-above)
      (eq cmd 'evil-open-below)
      (eq cmd 'self-insert-command)))

(evil-define-operator emc-evil-change (beg end type register yank-handler delete-func)
  "TODO: doc"
  (interactive "<R><x><y>")
  (list beg end type register))

;; (point)(call-interactively 'emc-evil-change nil [119])

(defun emc-run-last-command ()
  "Run the last stored command."
  (when (emc-command-info-p)
    (let* ((cmd (emc-get-this-command))
           (keys-vector (emc-get-this-command-keys))
           (keys (mapcar 'char-to-string keys-vector)))
      (when emc-debug (message "CMD %s keys-vector %s keys %s" cmd keys-vector keys))
      (cond ((or (eq cmd 'evil-snipe-f)
                 (eq cmd 'evil-snipe-t)) (evil-snipe-repeat))
            ((eq cmd 'evil-find-char) (evil-repeat-find-char))
            ((eq cmd 'newline-and-indent) (newline-and-indent))
            ((eq cmd 'self-insert-command) (self-insert-command 1))
            ((eq cmd 'evil-append) (evil-forward-char))
            ((eq cmd 'evil-delete-backward-char-and-join) (evil-delete-backward-char-and-join 1))
            ((eq cmd 'evil-delete-char) (evil-delete-char (point) (1+ (point))))
            ((eq cmd 'backward-delete-char-untabify) (delete-backward-char 1))
            ((eq cmd 'delete-backward-char) (delete-char -1))
            ((eq cmd 'evil-delete) (execute-kbd-macro (apply 'concat keys)))
            ((eq cmd 'evil-replace) (evil-repeat 1))
            ((eq cmd 'evil-open-below) (evil-insert-newline-below))
            ((eq cmd 'evil-open-above) (evil-insert-newline-above))
            ((eq cmd 'evil-change-line) (evil-delete-line (point) (1+ (point))))

            ((eq cmd 'evil-change)
             (evil-with-state normal
               ;; TODO until we find a better way just map the keys
               ;; cw -> lde
               ;; dtv -> ldv
               ;; c2ti -> ld2ti
               (execute-kbd-macro "ld2ti")))

            ;; ((eq cmd 'evil-change)
            ;;  (evil-with-state normal
            ;;    (let* ((motion (emc-evil-read-motion (cdr keys-vector)))
            ;;           (range (evil-motion-range (car motion))))
            ;;      (message "range %s point %s motion %s" range (point) motion)
            ;;      (evil-delete (car range) (car (cdr range))))))

            ;; TODO fix
            ;; ((eq cmd 'evil-change)
            ;;  (call-interactively 'evil-change nil (vconcat (cdr keys))))

            ;; ((eq cmd 'evil-change)
            ;;  (let ((range (evil-motion-range 'evil-forward-word-end)))
            ;;    (evil-delete (car range) (car (cdr range)))))

            ;; ((eq cmd 'evil-repeat) (evil-repeat 1)) ;; TODO make this work
            (t (funcall cmd))))))

(defun emc-evil-read-motion (keys &optional motion count type modifier)
  "Read a MOTION, motion COUNT and motion TYPE from the keyboard.
The type may be overridden with MODIFIER, which may be a type
or a Visual selection as defined by `evil-define-visual-selection'.
Return a list (MOTION COUNT [TYPE])."
  (let ((modifiers '((evil-visual-char . char)
                     (evil-visual-line . line)
                     (evil-visual-block . block)))
        command prefix)
    (unless motion
      (dolist (key keys)
        (setq command (evil-keypress-parser (list key))
              motion (pop command)
              prefix (pop command))
        (when prefix
          (if count
              (setq count (string-to-number
                           (concat (number-to-string count)
                                   (number-to-string prefix))))
            (setq count prefix)))
        ;; if the command is a type modifier, read more
        (when (rassq motion evil-visual-alist)
          (setq modifier
                (or modifier
                    (car (rassq motion evil-visual-alist)))))))
    (when modifier
      (setq type (or type (evil-type motion 'exclusive)))
      (cond
       ((eq modifier 'char)
        ;; TODO: this behavior could be less hard-coded
        (if (eq type 'exclusive)
            (setq type 'inclusive)
          (setq type 'exclusive)))
       (t
        (setq type modifier))))
    (list motion count type)))

;; (call-interactively 'evil-change nil [99 119 99])

;; (defun emc-get-motion (keys state)
;;   "Returns the motion associated with KEYS for STATE."
;;   (let ((motion (or evil-operator-range-motion
;;                     (when (evil-ex-p) 'evil-line)))
;;         (type evil-operator-range-type)
;;         (range (evil-range (point) (point)))
;;         command count modifier)
;;     (evil-save-state
;;       (unless motion
;;         (evil-change-state state)
;;         (let ((keys (nth 2 (evil-extract-count (this-command-keys)))))
;;           (setq keys (listify-key-sequence keys))
;;           (dotimes (var (length keys))
;;             (define-key evil-operator-shortcut-map
;;               (vconcat (nthcdr var keys)) 'evil-line)))
;;         (setq command (evil-keypress-parser keys)
;;               motion (nth 0 command)
;;               count (nth 1 command)
;;               type (or type (nth 2 command)))))
;;     motion))

;; (emc-get-motion "w" 'normal)
;; (evil-save-state (evil-change-state 'motion) (evil-keypress-parser "b"))

(defun emc-execute-last-command ()
  "Executes the command stored in `emc-command-info'."
  (when (and emc-command-info (not emc-cursor-command))
    (when emc-debug
      (message "Executing %s command (running %s)" emc-command-info emc-running-command))
    (condition-case error (emc-run-last-command)
      (error (message "Command %s failed with error %s"
                      emc-command-info (error-message-string error))))))

;; (setq emc-cursor-list (cons cursor emc-cursor-list)))
;; (evil-snipe-f 1 (kbd "e"))
;; (char-to-string 119)

(defun emc-run-motion-for-all-cursors ()
  "Runs the current motion command for all cursors."
  (interactive)
  (unless (or emc-running-command (evil-visual-state-p) (not emc-command-info))
    (let ((emc-running-command t))
      (evil-with-single-undo
        (save-excursion
          (let ((cursor-list nil))
            (dolist (cursor emc-cursor-list)
              (let ((start (overlay-start cursor)))
                (goto-char start)
                (emc-execute-last-command)
                (delete-overlay cursor)
                (setq cursor-list (cons (emc-make-cursor-at-point) cursor-list))))
            ;; TODO: instead of replacing emc-cursor-list replace individual cursors
            ;;       inside emc-cursor-list
            (setq emc-cursor-list cursor-list))
          (setq emc-command-info nil))))))

(defun emc-add-hooks ()
  "Adds all emc related hooks."
  (interactive)
  (setq emc-command-info nil)
  (add-hook 'evil-insert-state-exit-hook 'emc-position-cursors-after-insert)
  (add-hook 'pre-command-hook 'emc-begin-save-command t t)

  ;; Add this as the first hook, to run before evil-repeat post hook which clears the command keys
  (add-hook 'post-command-hook 'emc-finish-save-command nil t)
  (add-hook 'post-command-hook 'emc-run-motion-for-all-cursors t t))

;; (execute-kbd-macro "bbbbdw")

(defun emc-remove-hooks ()
  "Removes all emc related hooks."
  (interactive)
  (remove-hook 'evil-insert-state-exit-hook 'emc-position-cursors-after-insert)
  (remove-hook 'pre-command-hook 'emc-begin-save-command t)
  (remove-hook 'post-command-hook 'emc-finish-save-command t)
  (remove-hook 'post-command-hook 'emc-run-motion-for-all-cursors t))

(defun emc-print-command-vars ()
  "Prints command variables."
  (interactive)
  (prin1 (cons emc-command-info emc-running-command)))

;; (defun emc-record-command (info)
;;   (setq emc-command-info info))

;; (defun emc-pre-command-hook ()
;;   (message "REGISTER %s" evil-this-register)
;;   (when evil-this-register
;;     (emc-record-command
;;      `(set evil-this-register ,evil-this-register))))

;; (add-hook 'pre-command-hook 'emc-pre-command-hook t t)
;; (remove-hook 'pre-command-hook 'emc-pre-command-hook t)

;; (add-hook 'pre-command-hook 'emc-begin-save-command t t)
;; (remove-hook 'pre-command-hook 'emc-begin-save-command t)

;; (add-hook 'post-command-hook 'emc-run-motion-for-all-cursors t t)
;; (remove-hook 'post-command-hook 'emc-run-motion-for-all-cursors t t)

;; emc-command-info

;; (add-hook 'evil-normal-state-entry-hook 'emc-run-last-command-for-all-cursors t t)
;; (remove-hook 'evil-normal-state-entry-hook 'emc-run-last-command-for-all-cursors t)

;; (add-hook 'post-command-hook 'emc-print-command t t)
;; (remove-hook 'post-command-hook 'emc-print-command t)

;; (add-hook 'post-command-hook 'emc-run-last-command-for-all-cursors t t)
;; (remove-hook 'post-command-hook 'emc-run-last-command-for-all-cursors t)

;; (evil-repeat 1)
;; (ring-ref evil-repeat-ring 0)

;; (add-hook 'evil-motion-state-entry-hook (lambda () (message "ENTERED MOTION")))
;; (add-hook 'evil-motion-state-exit-hook (lambda () (message "EXIT MOTION")))

;; move the cursor in 'post-command-hook
;; (add-hook 'post-command-hook (lambda () (message "COMMAND DONE")))
;; (remove-hook 'post-command-hook (lambda () (message "COMMAND DONE")))

;; (let ((o1 (make-overlay 0 0))
;;       (o2 (make-overlay 0 0)))
;;   (move-overlay o1 9 10)
;;   (move-overlay o2 33 34)
;;   (overlay-put o1 'face 'region-face)
;;   (overlay-put o2 'face 'region-face))

;; (let ((o (make-overlay 9 10)))
;;   (delete-overlay o))

;; (progn
;;   (remove-overlays 9 10)
;;   (remove-overlays 33 34))

;; (evil-ex-hl-face hl)

;; (let ((o (make-overlay (point-min) (+ (point-min) 1) nil nil t)))
;;   (overlay-put o 'face 'cursor-face) o)

;; (let ((o (make-overlay (+ (point-min) 30) (+ (point-min) 50) nil nil t)))
;;   (overlay-put o 'face 'region-face)
;;   (overlay-put o 'type 'additional-region)
;;   o)

;; (mapc 'delete-overlay (overlays-at (point-min) (+ (point-min) 1)))
;; (mapc 'delete-overlay (overlays-at (+ (point-min) 30) (+ (point-min) 50)))

;; (evil-visualstar/begin-search 9 12 t)

;; (let* ((selection (regexp-quote (buffer-substring-no-properties 9 12)))
;;        (pattern (evil-ex-make-search-pattern selection)))
;;   (message selection)
;;   (evil-ex-search-activate-highlight pattern))

;; (let ((pattern (evil-ex-make-search-pattern "\\<def\\>")))
;;   (evil-ex-hl-change 'evil-ex-search pattern))

;; evil-ex-active-highlights-alist
;; (evil-ex-hl-update-highlights)

;; clear highlights
;; (evil-ex-search-activate-highlight "")

;; (evil-ex-make-hl 'evil-ex-search)

;; (progn
;;   (setq evil-ex-make-search-pattern "<def>")
;;   (setq evil-ex-search-direction 'forward)
;;   (evil-ex-search 2))

;; (evil-ex-search-abort)
;; (evil-ex-hl-set-overlays)
;; (rot13-region 9 11)
