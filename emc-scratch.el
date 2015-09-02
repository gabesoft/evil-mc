;;; Code:

(add-to-load-path (file-name-directory (buffer-file-name)))
;; (load-file "emc-common.el")
;; (load-file "emc-cursor-state.el")
;; (load-file "emc-command-info.el")

(require 'emc-vars)
(require 'emc-common)
(require 'emc-cursor-state)
(require 'emc-command-info)
(require 'emc-region)

(evil-define-local-var emc-watch-buffers
  '("emc-common"
    "emc-cursor-state"
    "emc-command-info"
    "emc-region"
    "emc-scratch"))

;; (remove-hook 'auto-save-mode-hook (lambda () (message "ASMH")))
;; (remove-hook 'auto-save-hook (lambda () (message "ASH")))

(defun emc-eval-buffer-on-save ()
  (when (and (buffer-file-name)
             (member (file-name-base) emc-watch-buffers))
    (run-at-time 1 nil (lambda ()
                         (message "eval buffer %s" (buffer-file-name))
                         (ignore-errors
                           (eval-buffer (current-buffer)))))))

(defun emc-add-eval-hooks ()
  "Add the eval buffer on save hooks."
  (interactive)
  (add-hook 'auto-save-hook 'emc-eval-buffer-on-save nil nil))

(defun emc-remove-eval-hooks ()
  "Remove the eval buffer on save hooks."
  (interactive)
  (remove-hook 'auto-save-hook 'emc-eval-buffer-on-save))

;; Add to emc-init-mode
;; (remove-hook 'auto-save-hook 'emc-eval-buffer-on-save nil)

;; (file-name-base (buffer-file-name))
;; (file-name-base)
;; (buffer-file-name (window-buffer))
;; (get-file-buffer (current-buffer))

(defun emc-make-cursor-at-eol (pos)
  "Make a cursor overlay at POS assuming pos is at the end of line."
  (let ((overlay (make-overlay pos pos nil nil nil)))
    (overlay-put overlay 'after-string (propertize " " 'face 'emc-cursor-simple))
    (overlay-put overlay 'type 'emc-fake-cursor)
    (overlay-put overlay 'priority 99)
    overlay))

(defun emc-make-cursor-inline (pos)
  "Make a cursor at POS assuming pos is not at the end of line."
  (let ((overlay (make-overlay pos (1+ pos) nil nil nil)))
    (overlay-put overlay 'face 'emc-cursor-simple)
    (overlay-put overlay 'type 'emc-fake-cursor)
    (overlay-put overlay 'priority 99)
    overlay))

(defun emc-remove-all-overlays ()
  "Remove all overlays."
  (interactive)
  (remove-overlays)
  (setq emc-cursor-list nil))

;; (defun emc-get-region-overlay-type (region)
;;   "Return the REGION's type."
;;   (when region (overlay-get region 'type)))

;; (defun emc-get-region-overlay-mark (region)
;;   "Return the REGION's mark."
;;   (when region (overlay-get region 'mark)))

;; (defun emc-get-region-overlay-point (region)
;;   "Return the REGION's point."
;;   (when region (overlay-get region 'point)))

;; (defun emc-get-region-overlay-prev-mark (region)
;;   "Return the REGION's prev mark."
;;   (when region (overlay-get region 'prev-mark)))

;; (defun emc-get-region-overlay-prev-point (region)
;;   "Return the REGION's prev point."
;;   (when region (overlay-get region 'prev-point)))

;; (defun emc-get-region-overlay-direction (region)
;;   "Return the direction of a visual region."
;;   (let ((mark (emc-get-region-overlay-mark region))
;;         (point (emc-get-region-overlay-point region)))
;;     (if (< point mark) -1 1)))

;; (emc-make-region-overlay (point) (+ (point) 30))

(defun emc-print-cursor-list ()
  "Return the cursor list."
  (interactive)
  (if emc-cursor-list
      (message "Cursors %s: %s" (length emc-cursor-list) emc-cursor-list)
    (message "No cursors found")))

;; (defun emc-print-region-list ()
;;   "Return the region list."
;;   (interactive)
;;   (if emc-region-list
;;       (message "Regions %s: %s" (length emc-region-list) emc-region-list)
;;     (message "No region found")))

(defun emc-draw-cursor-at-point ()
  "Create a cursor overlay at point"
  (interactive)
  (if (eolp)
      (emc-make-cursor-at-eol (point))
    (emc-make-cursor-inline (point))))

(defun emc-remove-overlay-at-point ()
  "Remove the overlay at point."
  (interactive)
  (when emc-cursor-list
    (setq emc-cursor-list
          (remove-if (lambda (cursor)
                       (let ((overlay (emc-get-cursor-overlay cursor)))
                         (eq (overlay-start overlay) (point))))
                     emc-cursor-list)))
  (remove-overlays (point) (1+ (point))))

(defun emc-remove-cursor-at-point ()
  "Remove the cursor at point."
  (emc-remove-overlay-at-point))

(defun emc-goto-next-match (pattern &optional direction)
  "Go to the next match of PATTERN optionally in DIRECTION or 'forward. The search does not wrap."
  (let ((pat (evil-ex-make-search-pattern pattern))
        (dir (or direction 'forward)))
    (setq evil-ex-search-pattern pat)
    (setq evil-ex-search-direction dir)
    (let ((next (evil-ex-find-next pat dir t)))
      (when (eq next t) (goto-char (1- (point))))
      next)))

;; (defun emc-get-visual-selection ()
;;   "Gets the current visual selection"
;;   (if (evil-visual-state-p)
;;       (let* ((range (evil-visual-range))
;;              (start (car range))
;;              (end (car (cdr range))))
;;         (buffer-substring-no-properties start end))))

;; (defun emc-print-visual-selection ()
;;   "Prints the current visual selection."
;;   (interactive)
;;   (let ((visual (emc-get-visual-selection)))
;;     (if visual (message visual)
;;       (error "No visual selection found"))))

;; (emc-remove-cursors "next")

(defun emc-set-pattern-from-visual-selection ()
  "Stores the pattern delimited by the current visual region along with its position."
  (let* ((range (evil-visual-range))
         (start (car range))
         (end (car (cdr range)))
         (pattern (buffer-substring-no-properties start end)))
    (if (< (length pattern) 2)
        (error "At least 2 characters required for creating a cursor")
      (setq emc-pattern (cons pattern (cons end start))))))

(defun emc-print-pattern ()
  "Print the curent pattern."
  (interactive)
  (message "%s" emc-pattern))

(defun emc-get-pattern ()
  "Get the current pattern if any."
  (when emc-pattern (car emc-pattern)))

(defun emc-get-position ()
  "Get the position of the current pattern if any."
  (when emc-pattern (cdr emc-pattern)))

(defun emc-add-cursor (overlay)
  "Create a cursor object from OVERLAY and add it to the cursors list."
  (setq emc-cursor-list
        (cons (emc-put-cursor-property nil
                                       :overlay overlay
                                       :kill-ring (copy-sequence kill-ring)
                                       :kill-ring-yank-pointer nil)
              emc-cursor-list)))

;; (defun emc-add-region (region)
;;   "Add REGION to the region list."
;;   (setq emc-region-list (cons region emc-region-list)))

;; (emc-goto-next-match (emc-get-pattern))

(defun emc-goto-next ()
  "Go to next pattern given by `emc-get-pattern'."
  (interactive)
  (when (emc-get-pattern)
    (emc-goto-next-match (emc-get-pattern))))

(defun emc-goto-prev ()
  "Go to prev pattern given by `emc-get-pattern'."
  (interactive)
  (when (emc-get-pattern)
    (emc-goto-next-match (emc-get-pattern) 'backward)
    (emc-goto-next-match (emc-get-pattern) 'backward)
    (emc-goto-next-match (emc-get-pattern) 'forward)))

(defun emc-remove-cursor-region (cursor)
  "Deletes CURSOR's region and returns a new cursor with the region removed."
  (emc-delete-region-overlay (emc-get-cursor-region cursor))
  (emc-put-cursor-region cursor nil))

(defun emc-make-cursor-and-goto-next-match ()
  "Create a cursor at point and go to next match if any."
  (let ((cursor (emc-draw-cursor-at-point)))
    (setq emc-cursor-list
          (mapcar 'emc-remove-cursor-region emc-cursor-list))
    (evil-exit-visual-state)
    (if (emc-goto-next-match (emc-get-pattern) 'forward)
        (emc-add-cursor cursor)
      (delete-overlay cursor)
      (error "No more matches found"))))

(defun emc-goto-prev-match-and-undo-cursor ()
  "Move point to a previous match and undo the cursor there if any."
  (if (and (emc-goto-next-match (emc-get-pattern) 'backward)
           (emc-goto-next-match (emc-get-pattern) 'backward))
      (progn
        (emc-goto-next-match (emc-get-pattern) 'forward)
        (emc-remove-overlay-at-point)
        (when (null emc-cursor-list) (setq emc-pattern nil)))
    (error "No more matches found")))

(defun emc-skip-cursor-and-goto-next-match ()
  "Skip making a cursor at point and go to next match if any."
  (evil-exit-visual-state)
  (unless (emc-goto-next-match (emc-get-pattern) 'forward) ;; TODO support wrap
    (error "No more matches found")))

(evil-define-command emc-make-next-cursor ()
  "Make the next cursor."
  :repeat ignore
  (interactive)
  (let ((emc-cursor-command t)) ;; TODO: this is no longer needed, but need to ensure that no commands are recorded during cursors creation
    (emc-command-reset)
    (when (and (evil-visual-state-p)
               (null (emc-get-pattern)))
      (emc-set-pattern-from-visual-selection))
    (when (evil-visual-state-p)
      (when (< (point) (mark)) (evil-exchange-point-and-mark))
      (goto-char (1- (point))))
    (if (emc-get-pattern)
        (emc-make-cursor-and-goto-next-match)
      (error "No more matches or no visual selection found"))))

;; (cond ((evil-visual-state-p)
;;        (emc-set-pattern-from-visual-selection)
;;        (emc-make-cursor-and-goto-next-match))
;;       ((emc-get-pattern) (emc-make-cursor-and-goto-next-match))
;;       (t (error "No more matches or no visual selection found")))))

(evil-define-command emc-skip-next-cursor ()
  "Skip the next cursor."
  :repeat ignore
  (interactive)
  (let ((emc-cursor-command t))
    (emc-command-reset)
    (cond ((evil-visual-state-p)
           (emc-set-pattern-from-visual-selection)
           (emc-skip-cursor-and-goto-next-match))
          ((emc-get-pattern) (emc-skip-cursor-and-goto-next-match))
          (t (error "No more matches or no visual selection found")))))

;; TODO should allow moving to the prev match or to the prev cursor
;; maybe C-p : emc-goto-prev-match
;; maybe C-P : emc-undo-prev-cursor
(evil-define-command emc-undo-prev-cursor ()
  "Move point to the prev match and remove the cursor if any."
  :repeat ignore
  (interactive)
  (let ((emc-cursor-command t))
    (emc-command-reset)
    (cond ((evil-visual-state-p)
           (emc-set-pattern-from-visual-selection)
           (emc-goto-prev-match-and-undo-cursor))
          ((emc-get-pattern) (emc-goto-prev-match-and-undo-cursor))
          (t (error "No more matches or no visual selection found")))))

;; (defun emc-delete-all-overlays (overlays)
;;   "Delete all OVERLAYS."
;;   (dolist (overlay overlays)
;;     (when (overlayp overlay)
;;       (delete-overlay overlay))))

;; (defun emc-delete-all-cursors ()
;;   "Delete all cursor overlays."
;;   (emc-delete-all-overlays emc-cursor-list)
;;   (setq emc-cursor-list nil))

;; (defun emc-delete-all-regions ()
;;   "Delete all region overlays."
;;   (emc-delete-all-overlays emc-region-list)
;;   (setq emc-region-list nil))

(evil-define-command emc-destroy-all-cursors ()
  "Destroy all cursor overlays and any related regions."
  :repeat ignore
  (interactive)
  (dolist (cursor emc-cursor-list)
    (let ((overlay (emc-get-cursor-overlay cursor))
          (region (emc-get-cursor-region cursor)))
      (when overlay (delete-overlay overlay))
      (emc-delete-region-overlay region)))
  (setq emc-cursor-list nil)
  (setq emc-pattern nil))

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

;; (defun emc-create-cursors (pattern)
;;   "Creates cursors for all matches of PATTERN."
;;   (let ((orig (point)))
;;     (while (emc-goto-next-match pattern 'forward)
;;       (emc-draw-cursor-at-point))
;;     (goto-char orig)))

;; (defun emc-remove-cursors (pattern)
;;   "Removes cursors for all matches of PATTERN."
;;   (let ((orig (point)))
;;     (while (emc-goto-next-match pattern 'forward)
;;       (emc-remove-cursor-at-point))
;;     (goto-char orig)))

;;; Run command

;; this-command
;; (this-command-keys)

;; (defsubst emc-command-recording-p ()
;;   "Return t only if a recording is in progress."
;;   (eq emc-command-recording t))

;; (defun emc-command-record (info)
;;   "Add INFO to the end of `emc-command-info'."
;;   (when (emc-command-recording-p)
;;     (setq emc-command-info (nconc emc-command-info (list info)))))


;; (defun emc-run-keystrokes (flag)
;;   "Runer for commands that are run by keystrokes."
;;   (cond ((eq flag 'pre)
;;          (when evil-this-register
;;            (emc-command-record `(set evil-this-register ,evil-this-register))))
;;         ((eq flag 'post)
;;          (emc-command-record (if (zerop (length (this-command-keys)))
;;                                  evil-repeat-keys
;;                                (this-command-keys)))))
;;   ;; Unfinished, see `evil-repeat-keystrokes'
;;   )

;; (defun emc-run-motion (flag)
;;   "Runner for motions."
;;   (when (memq evil-state '(insert replace normal))
;;     (emc-run-keystrokes flag)))

;; (defun emc-run-changes (flag)
;;   "Runner for buffer changes."
;;   (cond ((eq flag 'pre)
;;          (add-hook 'after-change-functions #'emc-run-change-hook nil t)
;;          (emc-run-start-record-changes))
;;         ((eq flag 'post)
;;          (remove-hook 'after-change-functions #'emc-run-change-hook t)
;;          (emc-run-finish-record-changes))))

;; (defun emc-run-change-hook (beg end len)
;;   "Record change information for the current command."
;;   (message "Not implemented, see `evil-repeat-change-hook'."))

;; (defun emc-run-start-record-changes ()
;;   "Starts the recording of a new set of buffer changes."
;;   (message "Not implemented, see `evil-repeat-start-record-changes'."))

;; (defun emc-run-finish-record-changes ()
;;   "Starts the recording of a new set of buffer changes."
;;   (message "Not implemented, see `evil-repeat-finish-record-changes'."))

;; (defvar emc-command-types
;;   '((t . emc-run-keystrokes)
;;     (change . emc-run-changes)
;;     (motion . emc-run-motion)
;;     (insert-at-point . emc-run-insert-at-point)
;;     (ignore . nil))
;;   "An alist of defined command-types.")

;; (defvar emc-command-recording nil
;;   "Whether we are recording a command.")

;; (defvar emc-command-info nil
;;   "Information accumulated during the current command.")

;; (defvar emc-command-buffer nil
;;   "The buffer in which the command started.
;; If the buffer is change, the command is cancelled.")

;; (defun emc-command-abort-p (type)
;;   "Returns t if the current command of TYPE should not run for all cursors."
;;   ;; TODO: add more checks, see `evil-repeat-force-abort-p'
;;   (or (eq type 'abort)
;;       (evil-emacs-state-p)
;;       (and (evil-mouse-events-p (this-command-keys))
;;            (null type))
;;       (minibufferp)))

;; (defun emc-command-reset (flag)
;;   "Clear all command recording variables and set `emc-command-recording' to FLAG."
;;   (setq emc-command-recording flag
;;         emc-command-info nil
;;         emc-command-buffer nil))

;; (defun emc-command-abort ()
;;   "Mark the current command as aborted."
;;   (emc-command-reset 'abort))

;; (defun emc-command-record-buffer ()
;;   "Set `emc-command-buffer' to the current buffer."
;;   (unless (minibufferp)
;;     (setq emc-command-buffer (current-buffer))))

;; (defun emc-command-start ()
;;   "Start recording a new command into `emc-command-info'."
;;   (emc-command-reset t)
;;   (emc-command-record-buffer))

;; (defun emc-print-point ()
;;   "Print the point location."
;;   (interactive)
;;   (message "Point is at %s" (point)))

;; (let ((overlay (make-overlay 0 0)))
;;   (move-overlay overlay 11088 11089)
;;   (overlay-put overlay 'face 'emc-cursor-face))

;; (overlays-at 11088)

;; (remove-overlays)

;; (evil-get-mmand-properties 'evil-repeat-visual-char)
;; (evil-get-mmand-properties 'evil-execute-change)
;; (evil-get-mmand-properties 'evil-snipe-f)

;; (defun emc-run-on-post-command ()
;;   "Apply the current command to all cursors."
;;   (message "TODO: implement emc-run-on-post-command"))

;; (defun emc-run-last-command-for-all-cursors ()
;;   "Runs the last command for all cursors."
;;   (interactive)
;;   (unless (or emc-running-command)
;;     (setq emc-running-command t)
;;     (save-excursion
;;       (dolist (cursor emc-cursor-list)
;;         (let ((start (overlay-start cursor)))
;;           (goto-char start)
;;           (evil-repeat 1))))
;;     (setq emc-running-command nil)))

;; (defun emc-print-evil-register ()
;;   (setq emc-command-recording t)
;;   (when evil-this-register
;;     (setq emc-command-info `(set evil-this-register ,evil-this-register)))
;;   (setq emc-command-recording nil))

(defun emc-debug-on ()
  "Turn debug on."
  (interactive)
  (setq emc-debug t))

(defun emc-debug-off ()
  "Turn debug off."
  (interactive)
  (setq emc-debug nil))

;; (defun emc-print-this-command (msg)
;;   "Print info about `this-command' prefixed with MSG."
;;   (when emc-debug
;;     (message "%s: command %s keys %s vector %s raw %s last event %s last command %s"
;;              msg
;;              this-command
;;              (this-command-keys)
;;              (this-command-keys-vector)
;;              (this-single-command-raw-keys)
;;              (cons last-input-event (emc-key-to-string last-input-event))
;;              last-command)))

;; (defun emc-clear-this-command ()
;;   "Remove all info saved for the current command."
;;   (setq emc-command-info nil))

;; (defun emc-get-this-command ()
;;   "Return the name of the current command."
;;   (when emc-command-info (car emc-command-info)))

;; (defun emc-get-last-command ()
;;   "Return the name of the last command."
;;   (when emc-command-info (car (cdr emc-command-info))))

;; (defun emc-get-this-command-keys ()
;;   "Return the keys of the current command."
;;   (when emc-command-info
;;     (let ((rest (cdr emc-command-info)))
;;       (when rest (car (cdr rest))))))

;; (defun emc-this-command-p ()
;;   "True if there is a command stored in `emc-command-info'."
;;   (not (null (emc-get-this-command))))

;; (defun emc-command-info-p ()
;;   "True if there is a command stored in `emc-command-info'."
;;   (emc-get-this-command))

;; (defun emc-set-command-info (this last keys)
;;   "Sets the command info to THIS command LAST command and KEYS."
;;   (setq emc-command-info (cons this (cons last (cons keys nil)))))

;; (defun emc-set-this-command (name)
;;   "Set the current command NAME."
;;   (let ((last (emc-get-last-command))
;;         (keys (emc-get-this-command-keys)))
;;     (emc-set-command-info name last keys)))

;; (defun emc-set-last-command (name)
;;   "Set the last command NAME."
;;   (let ((this (emc-get-this-command))
;;         (keys (emc-get-this-command-keys)))
;;     (emc-set-command-info this name keys)))

;; (defun emc-normalize-keys (keys)
;;   "Normalize the given keys."
;;   (cond ((null keys) nil)
;;         ((vectorp keys) (listify-key-sequence keys))
;;         ((atom keys) (cons keys nil))
;;         (t keys)))

;; (defun emc-append-unique (keys raw last)
;;   "Return a list of keys with only the unique values from KEYS, RAW, and LAST."
;;   (let ((normalized-keys (emc-normalize-keys keys))
;;         (normalized-raw (emc-normalize-keys raw))
;;         (normalized-last (emc-normalize-keys last)))
;;     (remove-duplicates (append normalized-keys normalized-raw normalized-last) :from-end t)))

;; (defun emc-set-this-command-keys (new-keys)
;;   "Adds NEW-KEYS to the current command."
;;   (let* ((this (emc-get-this-command))
;;          (last (emc-get-last-command))
;;          (keys (emc-normalize-keys new-keys)))
;;     (emc-set-command-info this last keys)))

;; (defun emc-append-this-command-keys (new-keys)
;;   "Adds NEW-KEYS to the current command."
;;   (let* ((this (emc-get-this-command))
;;          (last (emc-get-last-command))
;;          (keys (emc-get-this-command-keys))
;;          (more (emc-normalize-keys new-keys)))
;;     (emc-set-command-info
;;      this last (if (null keys) more (append keys more)))))

;; (defun emc-begin-save-command ()
;;   "Begin saving the current command if it is a supported command in `emc-command-info'."
;;   (when (not emc-running-command)
;;     (emc-print-this-command "PRE")
;;     (let ((cmd (or (command-remapping this-original-command) this-original-command)))
;;       (when (emc-supported-command-p cmd)
;;         (emc-clear-this-command)
;;         (emc-set-this-command cmd)
;;         (emc-set-last-command last-command)
;;         (emc-append-this-command-keys (this-command-keys-vector))))))

;; (defun emc-record-key-sequence (prompt &optional continue-echo dont-downcase-last
;;                                        can-return-switch-frame cmd-loop)
;;   "Record the current command keys before they are reset."
;;   (when (not emc-running-command)
;;     (let* ((orig (emc-get-this-command-keys))
;;            (new (emc-normalize-keys (this-command-keys-vector)))
;;            (all (append orig new)))
;;       (emc-set-this-command-keys (remove-duplicates all :from-end t)))
;;     (when emc-debug
;;       (message "CMD-KEY-SEQ-OLD %s %s %s"
;;                (this-command-keys)
;;                (this-command-keys-vector)
;;                (emc-get-this-command-keys)))))

;; (defun emc-finish-save-command ()
;;   "Finish saving the current command. This should be called from `post-command-hook'."
;;   (when (not emc-running-command)
;;     (emc-print-this-command "POST")
;;     (emc-append-this-command-keys (emc-append-unique
;;                                    (this-command-keys-vector)
;;                                    (this-single-command-raw-keys)
;;                                    last-input-event))))

;; (defun emc-position-cursors-after-insert ()
;;   "Re-position the cursors when exiting insert state."
;;   (when (not emc-running-command)
;;     (emc-clear-this-command)
;;     (emc-set-this-command 'evil-backward-char)
;;     (emc-run-command-for-all-cursors)))

;; (evil-get-register ?-)

;; TODO: find motions interfere with the cursors commands (especially insert f)
;; TODO: abort command hooks functinality if (evil-emacs-state-p)

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

;; (defun emc-key-to-string (key)
;;   "Converts a key to a string."
;;   (cond ((characterp key) (char-to-string key))
;;         ((null key) nil)
;;         (t key)))

;; (defun emc-change-operator-sequence (keys-string)
;;   "Return the sequence of keys to run an `evil-change' keyboard macro for fake cursors based on KEYS-STRING."
;;   (let* ((keys-with-count (evil-extract-count keys-string))
;;          (cnt (nth 0 keys-with-count))
;;          (fst (nth 2 keys-with-count))
;;          (snd (nth 3 keys-with-count))
;;          (res (apply 'concat (list "d" (cond ((string-equal snd "w") "e")  ;; TODO use a map
;;                                              ((string-equal snd "W") "E")
;;                                              (t snd))))))
;;     (cond ((string-equal keys-string "cc") "^C")
;;           (t (concat "l" (if (null cnt)
;;                              res
;;                            (concat (number-to-string cnt) res)))))))

;; (defun emc-find-region (pos)
;;   "Find the region that ends or starts with POS according to its direction."
;;   (catch 'emc-region-found
;;     (dolist (region emc-region-list)
;;       (let ((mark (emc-get-region-overlay-mark region))
;;             (point (emc-get-region-overlay-point region))
;;             (dir (emc-get-region-overlay-direction region))
;;             (start (overlay-start region))
;;             (end (overlay-end region)))
;;         (when (or (and (eq dir 1) (eq end (1+ pos)))   ;; TODO abstract out the 1+ for the region end
;;                   (and (eq dir -1) (eq start pos)))
;;           (throw 'emc-region-found region))))))

;; (defun emc-refresh-region (region orig)
;;   "Refresh the visual REGION when point moved from ORIG to current location."
;;   (let ((mark (or (and region (emc-get-region-overlay-mark region)) orig)))
;;     (when region (delete-overlay region))
;;     (cond ((and (< mark (point))) (emc-make-region-overlay mark (1+ (point))))
;;           ((and (< (point) mark)) (emc-make-region-overlay mark (point)))
;;           (t (emc-make-region-overlay (point) (1+ (point)))))))

;; ((and (eq mark orig) (< (point) mark)) (setq mark (1+ mark)))

;; (emc-line-region)
;; (evil-visual-make-region (point) (point) evil-visual-line)
;; (evil-visual-line)
;; (evil-visual-highlight -1)
;; (list (point) (point-at-bol) (point-at-eol))

;; TODO move variables to own file
;; TODO move overlay functions to own file
;; TODO move region functions to own file

;; (defun emc-make-region-overlay-old (mark point &optional type prev-mark prev-point)
;;   "Make a visual region overlay from MARK to POINT."
;;   (let* ((start (if (< mark point) mark point))
;;          (end (if (< mark point) point mark))
;;          (overlay (make-overlay start end nil nil nil)))
;;     (overlay-put overlay 'face 'emc-region-face)
;;     (overlay-put overlay 'priority 99)
;;     (overlay-put overlay 'mark mark)
;;     (overlay-put overlay 'point point)
;;     (overlay-put overlay 'type (or type 'char))
;;     (overlay-put overlay 'prev-mark prev-mark)
;;     (overlay-put overlay 'prev-point prev-point)
;;     overlay))

;; (defun emc-make-line-region (&optional prev-mark prev-point)
;;   "Creates a visual region line for the line at point
;; optionally storing PREV-MARK and PREV-POINT."
;;   (let ((start (point-at-bol))
;;         (end (point-at-eol)))
;;     (emc-make-region-overlay-old start (1+ end) 'line prev-mark prev-point)))

;; (defun emc-char-region-bounds (orig-mark orig-point)
;;   "Calculates the new bounds for a char region based on ORIG-MARK and ORIG-POINT."
;;   (let ((mark (or orig-mark orig-point)))
;;     (cond ((and (<= mark orig-point) (< (point) mark)) (setq mark (1+ mark)))
;;           ((and (< orig-point mark) (<= mark (point))) (setq mark (1- mark))))
;;     (cond ((< mark (point)) (cons mark (1+ (point))))
;;           ((< (point) mark) (cons mark (point)))
;;           (t (cons (point) (1+ (point)))))))

;; (defun emc-refresh-char-region (orig-mark orig-point)
;;   "Create a new char region based on ORIG-MARK and ORIG-POINT."
;;   (let ((bounds (emc-char-region-bounds orig-mark orig-point)))
;;     (emc-make-region-overlay-old (car bounds) (cdr bounds))))

;; (defun emc-refresh-line-region (orig-mark orig-point)
;;   "Create a new line region based on ORIG-MARK and ORIG-POINT."
;;   (let* ((mark (or orig-mark orig-point))
;;          (mark-line (line-number-at-pos mark))
;;          (point (point))
;;          (point-line (line-number-at-pos point)))
;;     (cond ((eq point-line mark-line) (emc-make-line-region orig-mark orig-point))
;;           (t (message "TODO: implement") nil))))

;; (defun emc-refresh-region (region orig-point)
;;   "Refresh the visual REGION when point moved from ORIG-POINT to current location."
;;   (cond
;;    ((eq (emc-get-region-overlay-type region) 'line)
;;     (emc-refresh-line-region (emc-get-region-overlay-mark region) orig-point))
;;    (t
;;     (emc-refresh-char-region (emc-get-region-overlay-mark region) orig-point))))

;; (defun emc-exchange-point-and-mark-old (region)
;;   "Exchange point and mark for a fake REGION."
;;   (let* ((mark (emc-get-region-overlay-mark region))
;;          (point (emc-get-region-overlay-point region)))
;;     (when region
;;       (let (new-region)
;;         (setq new-region (emc-make-region-overlay-old point mark))
;;         ;; (goto-char (if (< mark point) mark (1- mark)))
;;         new-region))))

(defun emc-run-last-command-visual (cursor)
  "Run the last stored command in visual mode given CURSOR and REGION."
  (let* ((cmd (emc-get-command-name))
         (region (emc-get-cursor-region cursor))
         (region-type (emc-get-region-type region))
         (repeat-type (evil-get-command-property cmd :repeat)))
    (when emc-debug (message "CMD-VISUAL %s" cmd))
    (cond ((or (eq cmd 'exchange-point-and-mark)
               (eq cmd 'evil-exchange-point-and-mark))
           (let* ((new-region (emc-exchange-region-point-and-mark region))
                  (mark (emc-get-region-mark new-region))
                  (point (emc-get-region-point new-region)))
             (goto-char (if (< mark point) (1- point) point))
             (setq region new-region)))

          ((or (eq cmd 'evil-snipe-f)
               (eq cmd 'evil-snipe-F)
               (eq cmd 'evil-snipe-t)
               (eq cmd 'evil-snipe-T))
           (evil-snipe-repeat)
           (setq region (emc-update-region region)))

          ((or (eq cmd 'evil-find-char)
               (eq cmd 'evil-find-char-to)
               (eq cmd 'evil-find-char-backward)
               (eq cmd 'evil-find-char-to-backward))
           (evil-repeat-find-char)
           (setq region (emc-update-region region)))

          ;; TODO add all text objects from ev
          ((or (eq cmd 'evil-inner-paren)
               (eq cmd 'evil-inner-word)
               (eq cmd 'evil-inner-WORD)
               (eq cmd 'evil-inner-paragraph)
               (eq cmd 'evil-inner-bracket)
               (eq cmd 'evil-inner-curly)
               (eq cmd 'evil-inner-double-quote)
               (eq cmd 'evil-inner-sentence)
               (eq cmd 'evil-a-paren)
               (eq cmd 'evil-a-word)
               (eq cmd 'evil-a-WORD)
               (eq cmd 'evil-a-paragraph)
               (eq cmd 'evil-a-bracket)
               (eq cmd 'evil-a-curly)
               (eq cmd 'evil-a-double-quote)
               (eq cmd 'evil-a-sentence))
           (let* ((limits (funcall cmd))
                  (start (nth 0 limits))
                  (end (1- (nth 1 limits))))
             (goto-char end)
             (setq region (emc-create-region start end 'char))))

          ((eq repeat-type 'motion)
           (ignore-errors
             (condition-case error
                 (funcall cmd)
               (error (message "%s failed with %s" cmd
                               (error-message-string error)))))
           (setq region (emc-update-region region)))

          ((eq cmd 'evil-visual-block)
           (evil-force-normal-state)
           (error "visual block is not supported"))

          ((eq cmd 'evil-visual-line)
           (cond ((or (null region) (eq region-type 'line))
                  (setq region (emc-create-region (point) (point) 'line)))
                 (t
                  (setq region (emc-change-region-type region 'line)))))

          ((eq cmd 'evil-visual-char)
           (cond ((or (null region) (eq region-type 'char))
                  (setq region (emc-create-region (point) (point) 'char)))
                 (t
                  (setq region (emc-change-region-type region 'char)))))

          (t (message "not implemented")))
    (emc-put-cursor-region cursor region)))

(defmacro emc-with-region (region fn &rest body)
  "When the REGION exists and has an overlay execute FN
otherwise execute BODY."
  `(if region
       (let ((start (emc-get-region-start ,region))
             (end (emc-get-region-end ,region)))
         (funcall ,fn start end))
     ,@body))

(defun emc-ends-with-newline-p (text)
  "True if TEXT ends with a newline character."
  (string-match-p "\n$" text))

(defun emc-run-last-command (cursor)
  "Run the last stored command for CURSOR."
  (let* ((cmd (emc-get-command-name))
         (region (emc-get-cursor-region cursor))
         (kill-ring (emc-get-cursor-kill-ring cursor))
         (kill-ring-yank-pointer (emc-get-cursor-kill-ring-yank-pointer cursor))
         (keys-string (emc-get-command-keys-string)))
    (when emc-debug (message "CMD %s keys %s" cmd keys-string))

    (cond ((eq cmd 'yaml-electric-dash-and-dot) (yaml-electric-dash-and-dot 1))
          ((eq cmd 'yaml-electric-bar-and-angle) (yaml-electric-bar-and-angle 1))
          ((eq cmd 'org-self-insert-command) (self-insert-command 1))
          ((eq cmd 'transpose-chars-before-point) (transpose-chars-before-point 1))


          ((or (eq cmd 'evil-snipe-f)
               (eq cmd 'evil-snipe-F)
               (eq cmd 'evil-snipe-t)
               (eq cmd 'evil-snipe-T))
           (evil-snipe-repeat 1))

          ((or (eq cmd 'evil-find-char)
               (eq cmd 'evil-find-char-to)
               (eq cmd 'evil-find-char-backward)
               (eq cmd 'evil-find-char-to-backward))
           (evil-repeat-find-char))

          ((eq cmd 'evil-commentary)
           (emc-with-region region 'evil-commentary
                            (execute-kbd-macro keys-string)))

          ((eq cmd 'evil-find-char) (evil-repeat-find-char))
          ((eq cmd 'newline-and-indent) (newline-and-indent))
          ((eq cmd 'evil-append) (evil-append 1))

          ((eq cmd 'evil-delete-backward-char-and-join) (evil-delete-backward-char-and-join 1))
          ((eq cmd 'evil-complete-next) (evil-complete-next))

          ((eq cmd 'evil-delete-char)
           (emc-with-region region 'evil-delete-char
                            (execute-kbd-macro keys-string)))

          ((eq cmd 'evil-delete-line) (execute-kbd-macro keys-string))
          ((eq cmd 'evil-join) (execute-kbd-macro keys-string))

          ((eq cmd 'evil-insert-line) (evil-insert-line 1))
          ((eq cmd 'evil-append-line) (evil-append-line 1))

          ((eq cmd 'backward-delete-char-untabify) (delete-backward-char 1))
          ((eq cmd 'delete-backward-char) (delete-backward-char 1))
          ((eq cmd 'evil-replace) (evil-repeat 1))

          ;; TODO fix undo after paste
          ;;      all fake cursors and the real one should undo as one step
          ((or (eq cmd 'evil-paste-after)
               (eq cmd 'evil-paste-before))

           (cond ((null region) (funcall cmd 1))
                 ((emc-char-region-p region)
                  (let (new-kill-ring new-kill-ring-yank-pointer)
                    (let ((kill-ring (copy-sequence kill-ring))
                          (kill-ring-yank-pointer nil))

                      (emc-with-region region 'evil-delete)
                      (setq new-kill-ring kill-ring)
                      (setq new-kill-ring-yank-pointer kill-ring-yank-pointer))

                    ;; execute paste with the old key ring
                    (evil-paste-before 1)

                    ;; update the kill ring with the overwritten text
                    (setq kill-ring new-kill-ring)
                    (setq kill-ring-yank-pointer new-kill-ring-yank-pointer)))
                 ((emc-line-region-p region)
                  (let ((text (substring-no-properties (current-kill 0 t)))
                        (start (emc-get-region-start region))
                        (end (emc-get-region-end region)))
                    (unless (emc-ends-with-newline-p text)
                      (evil-insert-newline-below))
                    (evil-paste-after 1)
                    (save-excursion (evil-delete start end 'line))))))

          ((eq cmd 'paste-before-current-line) (paste-before-current-line 1))
          ((eq cmd 'paste-after-current-line) (paste-after-current-line 1))

          ;; TODO this depends on the cursor order
          ;;      moving up or down have to be done in order of cursor positions
          ;;      but the real cursor always happens first which may break the others
          ;;      determine if this is worth supporting
          ((eq cmd 'move-text-down) (move-text-down 1))
          ((eq cmd 'move-text-up) (move-text-up 1))


          ((eq cmd 'evil-normal-state)
           (evil-insert 1)
           (evil-normal-state))

          ((eq cmd 'yank) (yank))

          ((eq cmd 'evil-open-below) (evil-insert-newline-below))
          ((eq cmd 'evil-open-above) (evil-insert-newline-above))
          ((eq cmd 'evil-change-line) (evil-delete-line (point) (1+ (point))))
          ((eq cmd 'evil-invert-char) (execute-kbd-macro keys-string))
          ((eq cmd 'evil-upcase) (execute-kbd-macro keys-string))
          ((eq cmd 'evil-downcase) (execute-kbd-macro keys-string))
          ((eq cmd 'keyboard-quit) nil)
          ((eq cmd 'evil-visual-char) (evil-force-normal-state))
          ((eq cmd 'evil-visual-line) (evil-force-normal-state))

          ((eq cmd 'evil-surround-region)
           (emc-with-region
            region
            (lambda (start end)
              (evil-surround-region
               start end nil (emc-get-command-key :last-input)))))

          ((eq cmd 'evil-yank)
           (cond ((null region)
                  (execute-kbd-macro keys-string))
                 ((emc-char-region-p region)
                  (emc-with-region region
                                   (lambda (start end)
                                     (goto-char start)
                                     (evil-yank start end))))
                 ((emc-line-region-p region)
                  (execute-kbd-macro "yy"))))

          ((eq cmd 'evil-delete)
           (cond ((null region)
                  (execute-kbd-macro keys-string))
                 ((emc-char-region-p region)
                  (emc-with-region region 'evil-delete))
                 ((emc-line-region-p region)
                  (execute-kbd-macro "dd")))
           (when (eolp) (evil-end-of-line)))

          ((eq cmd 'evil-change)
           (evil-with-state normal
             (cond ((null region)
                    (evil-forward-char)
                    (execute-kbd-macro keys-string))
                   ((emc-char-region-p region)
                    (emc-with-region region
                                     (lambda (start end)
                                       (evil-forward-char)
                                       (evil-delete start end))))
                   ((emc-line-region-p region)
                    (evil-forward-char)
                    (execute-kbd-macro "cc")))))

          ;; End of line behavior test
          ;; cmd-null
          ;; cmd-null
          ;; cmd null
          ;; (null 'a)
          ;; (void-null 'b)
          ;; (null)

          ;;  "< CMD-DONE %s pre %s seq %s post %s raw %s last %s -> %s"
          ;;  "< CMD-DONE cms pre cms seq cms post cms raw cms last cms -> %s"
          ;; (sexecute-kbd-macro "3cw")(execute-kbd-macro "J2")
          ;; (sexecute-kbd-macro "3cw")(execute-kbd-macro "J2")
          ;; (emc-get-command-property "test")
          ;; (sexecute-kbd-macro "3cw")(execute-kbd-macro "J3")
          ;; (sexecute-kbd-macro "3cw")(execute-kbd-macro "J3")
          ;; (emc-get-command-property "test")
          ;; (sexecute-kbd-macro "3cw")(execute-kbd-macro "J4")
          ;; (sexecute-kbd-macro "3cw")(execute-kbd-macro "J4")
          ;; (emc-get-command-property "test")
          ;; (sexecute-kbd-macro "3cw")(execute-kbd-macro "f-7")
          ;; (sexecute-kbd-macro "3cw")(execute-kbd-macro "f-7")
          ;; (sexecute-kbd-macro "3cw")(execute-kbd-macro "f-7")
          ;; (sexecute-kbd-macro "3cw")(execute-kbd-macro "f-7")
          ;; (sexecute-kbd-macro "4cw")(execute-kbd-macro "f-8")
          ;; (sexecute-kbd-macro "4cw")(execute-kbd-macro "f-8")
          ;; (sexecute-kbd-macro "4cw")(execute-kbd-macro "f-8")
          ;; (sexecute-kbd-macro "4cw")(execute-kbd-macro "f-8")
          ;; (sexecute-kbd-macro "3cw")(execute-kbd-macro "f-9")
          ;; (sexecute-kbd-macro "3cw")(execute-kbd-macro "f-9")
          ;; (sexecute-kbd-macro "3cw")(execute-kbd-macro "ft 1")
          ;; (sexecute-kbd-macro "3cw")(execute-kbd-macro "ft 1")
          ;; (sexecute-kbd-macro "3cw")(execute-kbd-macro "ft 2")
          ;; (sexecute-kbd-macro "3cw")(execute-kbd-macro "ft 2")
          ;; (sexecute-kbd-macro "3cw")(execute-kbd-macro "ft 3")
          ;; (sexecute-kbd-macro "3cw")(execute-kbd-macro "ft 3")
          ;; (eq cmd 'evil-upcase)
          ;; (eq cmd 'evil-invert-char)
          ;; (eq cmd 'evil-upcase)
          ;; (eq cmd 'evil-change-line)
          ;; (eq cmd 'evil-open-above)

          ;; TODO make this work
          ;; ((eq cmd 'evil-repeat) (evil-repeat 1))
          ;; evil-surround integration cs'" etc

          (t (execute-kbd-macro keys-string)))
    (emc-put-object-property cursor
                             :kill-ring kill-ring
                             :kill-ring-yank-pointer kill-ring-yank-pointer
                             :region nil)))

;; (defun emc-process-last-command-result (input)
;;   "Convert the INPUT of the last command into the expected format."
;;   (let* ((result (if (or (null input) (listp input)) input (list input)))
;;          (cursor (nth 0 result))
;;          (region (nth 1 result)))
;;     ;; (message "PROCESS %s" result)
;;     (list (if (overlayp cursor) cursor (emc-draw-cursor-at-point))
;;           (if (overlayp region) region nil))))

(defun emc-run-command (cursor)
  "Executes the command stored in `emc-command' for CURSOR."
  (when (and (emc-command-p) (not emc-cursor-command))
    (when emc-debug
      (message "Executing %s command" (emc-get-command-name)))
    (ignore-errors
      (condition-case error
          (if (eq 'visual (emc-get-command-state))
              (emc-run-last-command-visual cursor)
            (emc-run-last-command cursor))
        (error (message "Running command %s failed with %s"
                        (emc-get-command-name)
                        (error-message-string error))
               cursor)))))

;; (emc-run-command nil nil)
;; (setq emc-cursor-list (cons cursor emc-cursor-list))
;; (evil-snipe-f 1 (kbd "e"))
;; (char-to-string 119)

(defun emc-run-command-for-all-cursors ()
  "Runs the current command for all cursors."
  (unless (or emc-running-command (not (emc-command-p)))
    (let ((emc-running-command t))
      (evil-with-single-undo
        ;; (evil-with-transient-mark-mode)
        ;; (message "BEFORE-VISUAL %s" (evil-visual-state-p))
        ;; (evil-without-repeat)
        ;; (evil-repeat-abort)
        (save-excursion
          (let ((cursor-list nil))
            ;; (message "RUN COMMAND FOR ALL %s" (length emc-cursor-list))
            (dolist (cursor emc-cursor-list)
              ;; (message "CURSOR %s %s"
              ;;          (emc-get-cursor-overlay cursor)
              ;;          (emc-get-command-name))
              (let ((region (emc-get-cursor-region cursor))
                    (overlay (emc-get-cursor-overlay cursor)))
                ;; (message "START %s" start)
                (let ((new-cursor nil))
                  ;; (message "cmd %s" emc-command-info)
                  ;; (message "cursor %s -> %s" cursor new-cursor)


                  (goto-char (overlay-start overlay))
                  (setq new-cursor (emc-run-command cursor))

                  ;; (message "region %s -> %s" region
                  ;;          (emc-get-cursor-region new-cursor))

                  ;; (message "OLD CURSOR %s NEW CURSOR %s" cursor new-cursor)

                  (when overlay (delete-overlay overlay))
                  (emc-delete-region-overlay region)
                  ;; (when region (delete-overlay (emc-get-region-overlay region)))

                  (setq new-cursor (emc-put-cursor-overlay
                                    new-cursor (emc-draw-cursor-at-point)))
                  (setq cursor-list (cons new-cursor cursor-list)))))

            (setq emc-cursor-list cursor-list))
          ;; (emc-command-reset)
          ;; (message "AFTER-VISUAL %s" (evil-visual-state-p))
          )))))

(defun emc-setup-key-maps ()
  "Sets up all key bindings for working with multiple cursors."
  (interactive)
  (define-key evil-visual-state-local-map (kbd "C-n") 'emc-make-next-cursor)
  (define-key evil-normal-state-local-map (kbd "C-n") 'emc-make-next-cursor)
  (define-key evil-visual-state-local-map (kbd "C-t") 'emc-skip-next-cursor)
  (define-key evil-normal-state-local-map (kbd "C-t") 'emc-skip-next-cursor)
  (define-key evil-visual-state-local-map (kbd "C-p") 'emc-undo-prev-cursor)
  (define-key evil-normal-state-local-map (kbd "C-p") 'emc-undo-prev-cursor)
  (define-key evil-normal-state-local-map (kbd "C-,") 'emc-destroy-all-cursors))

;; (emc-setup-key-maps)

(defun emc-add-hooks ()
  "Adds all emc related hooks."
  (interactive)
  (emc-command-reset)
  (emc-add-command-hooks)
  (add-hook 'post-command-hook 'emc-run-command-for-all-cursors t t))

(defun emc-remove-hooks ()
  "Removes all emc related hooks."
  (interactive)
  (emc-remove-command-hooks)
  (remove-hook 'post-command-hook 'emc-run-command-for-all-cursors t))

(defun emc-init-mode ()
  "Initialize the evil-multiple-cursors mode."
  (interactive)
  (emc-setup-key-maps)
  (emc-add-hooks))

(defun emc-print-mark-and-point ()
  "Print mark and point."
  (interactive)
  (message "Mark %s Point %s" (mark) (point)))

;; (defun emc-print-command-vars ()
;;   "Prints command variables."
;;   (interactive)
;;   (prin1 (cons emc-command-info emc-running-command)))

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

;; (add-hook 'post-command-hook 'emc-run-command-for-all-cursors t t)
;; (remove-hook 'post-command-hook 'emc-run-command-for-all-cursors t t)

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
