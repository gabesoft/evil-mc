;;; evil-mc-cursor-make.el --- Fake cursor creation and deletion

;;; Commentary:

;; This file contains functions for creating and deleting fake cursors

;;; Code:

(require 'cl-lib)
(require 'evil-types)
(require 'evil-mc-common)
(require 'evil-mc-vars)
(require 'evil-mc-cursor-state)
(require 'evil-mc-region)

(defun evil-mc-cursor-is-of-type (type)
  "Return true if the cursor is of TYPE"
  (or (eq cursor-type type)
      (and (listp cursor-type) (eq (car cursor-type) type))))

(defun evil-mc-cursor-is-bar ()
  "Return true if the cursor is of type vertical bar
and `evil-mc-enable-bar-cursor' is not `nil'."
  (and evil-mc-enable-bar-cursor (evil-mc-cursor-is-of-type 'bar)))

(defun evil-mc-cursor-is-hbar ()
  "Return true if the cursor is of type horizontal bar"
  (evil-mc-cursor-is-of-type 'hbar))

(defun evil-mc-get-cursor-face ()
  "Get the current cursor face."
  (or evil-mc-cursor-current-face '(evil-mc-cursor-default-face)))

(defun evil-mc-get-hbar-cursor-face ()
  "Get the horizontal bar cursor face."
  (or evil-mc-cursor-current-face '(evil-mc-cursor-hbar-face)))

(defun evil-mc-get-bar-cursor-face ()
  "Get the vertical bar cursor face."
  (or evil-mc-cursor-current-face '(evil-mc-cursor-bar-face)))

(defun evil-mc-set-cursor-face (face)
  "Set the current cursor FACE."
  (setq evil-mc-cursor-current-face face))

(defun evil-mc-print-cursors-info (&optional msg)
  "Prints information about the current cursors preceded by MSG."
  (when (evil-mc-has-cursors-p)
    (evil-mc-message "%s %s cursors matching \"%s\""
                     (or msg "There are")
                     (1+ (length evil-mc-cursor-list))
                     (evil-mc-get-pattern-text))))

(defun evil-mc-cursor-overlay (start end)
  "Make an overlay for a cursor from START to END."
  (let ((overlay (make-overlay start end nil nil nil)))
    (overlay-put overlay 'type 'evil-mc-cursor)
    (overlay-put overlay 'priority evil-mc-cursor-overlay-priority)
    overlay))

(defun evil-mc-setup-cursor-overlay (overlay default-setup)
  "Setup the cursor OVERLAY"
  (cond ((evil-mc-cursor-is-bar)
         (overlay-put overlay 'before-string (propertize "|" 'face (evil-mc-get-bar-cursor-face))))
        ((evil-mc-cursor-is-hbar)
         (funcall default-setup overlay (evil-mc-get-hbar-cursor-face)))
        (t (funcall default-setup overlay (evil-mc-get-cursor-face)))))

(defun evil-mc-setup-cursor-overlay-at-eol (overlay face)
  "Setup the OVERLAY with the default cursor face at end of line"
  (overlay-put overlay 'after-string (propertize " " 'face face)))

(defun evil-mc-setup-cursor-overlay-inline (overlay face)
  "Setup the OVERLAY with the default cursor face at inline"
  (overlay-put overlay 'face face))

(defun evil-mc-cursor-overlay-at-eol (pos)
  "Make a cursor overlay at POS assuming pos is at the end of line."
  (let ((overlay (evil-mc-cursor-overlay pos pos)))
    (evil-mc-setup-cursor-overlay overlay 'evil-mc-setup-cursor-overlay-at-eol)
    overlay))

(defun evil-mc-cursor-overlay-inline (pos)
  "Make a cursor overlay at POS assuming pos is not at the end of line."
  (let ((overlay (evil-mc-cursor-overlay pos (1+ pos))))
    (evil-mc-setup-cursor-overlay overlay 'evil-mc-setup-cursor-overlay-inline)
    overlay))

(defun evil-mc-cursor-overlay-at-pos (&optional pos)
  "Make a cursor overlay at POS."
  (let ((pos (or pos (point))))
    (save-excursion
      (goto-char pos)
      (if (eolp)
          (evil-mc-cursor-overlay-at-eol pos)
        (evil-mc-cursor-overlay-inline pos)))))

(defun evil-mc-sort-cursors ()
  "Sort the cursors list by position."
  (setq evil-mc-cursor-list
        (sort evil-mc-cursor-list
              (lambda (x y)
                (< (evil-mc-get-cursor-start x)
                   (evil-mc-get-cursor-start y))))))

(defun evil-mc-copy-cursor-state (from &optional to)
  "Copy all state FROM cursor to TO cursor."
  (let ((names (evil-mc-get-cursor-variables)))
    (dolist (name names)
      (setq to (evil-mc-put-cursor-property
                to
                name
                (copy-tree (evil-mc-get-cursor-property from name)))))
    to))

(defun evil-mc-read-cursor-state (&optional state)
  "Read the state of the real cursor into STATE."
  (let ((names (evil-mc-get-cursor-variables)))
    (dolist (name names)
      (when (boundp name)
        (setq state (evil-mc-put-cursor-property state
                                                 name
                                                 (symbol-value name)))))
    state))

(defun evil-mc-write-cursor-state (state)
  "Write the state of the real cursor with values from STATE."
  (let ((names (evil-mc-get-cursor-variables)))
    (dolist (name names)
      (when (boundp name)
        (set name (evil-mc-get-cursor-property state name))))))

(defun evil-mc-insert-cursor-into-list (cursor cursor-list)
  "Insert CURSOR into CURSOR-LIST at the correct location so that
the cursors are ordered by the cursor overlay start position."
  (cond ((null cursor-list)
         (setq cursor-list (list cursor)))
        ((> (evil-mc-get-cursor-start (car cursor-list))
            (evil-mc-get-cursor-start cursor))
         (setq cursor-list (cons cursor cursor-list)))
        (t (let ((start (evil-mc-get-cursor-start cursor))
                 (list cursor-list))
             (while (and (cdr list)
                         (> start (evil-mc-get-cursor-start (cadr list))))
               (setq list (cdr list)))
             (setcdr list (cons cursor (cdr list))))))
  cursor-list)

(defun evil-mc-insert-cursor (cursor)
  "Insert CURSOR into `evil-mc-cursor-list' at the correct location so that
the cursors are ordered by the cursor overlay start position."
  (setq evil-mc-cursor-list (evil-mc-insert-cursor-into-list
                             cursor
                             evil-mc-cursor-list)))

(defun evil-mc-delete-cursor (cursor)
  "Delete all overlays associated with CURSOR."
  (evil-mc-delete-cursor-overlay cursor)
  (evil-mc-delete-region-overlay (evil-mc-get-cursor-region cursor)))

(defun evil-mc-delete-all-regions ()
  "Clear all visual regions and exit visual state."
  (when (evil-visual-state-p)
    (dolist (cursor evil-mc-cursor-list)
      (evil-mc-delete-region-overlay (evil-mc-get-cursor-region cursor)))
    (evil-exit-visual-state)))

(defun evil-mc-undo-cursor (cursor)
  "Delete CURSOR and remove it from `evil-mc-cursor-list'."
  (when (and cursor (evil-mc-has-cursors-p))
    (let ((start (evil-mc-get-cursor-start cursor)))
      (evil-mc-delete-cursor cursor)
      (setq evil-mc-cursor-list (delq cursor evil-mc-cursor-list))
      cursor)))

(defun evil-mc-get-default-cursor ()
  "Return a new cursor with all default properties initialized."
  (evil-mc-put-cursor-property
   nil
   'evil-markers-alist (default-value 'evil-markers-alist)
   'evil-repeat-ring (make-ring 10)
   'evil-jumper--window-jumps (make-hash-table)
   'evil-jump-list nil
   'kill-ring (copy-tree kill-ring)
   'undo-stack nil
   'undo-stack-pointer nil))

(defun evil-mc-make-cursor-at-pos (pos &optional source-cursor)
  "Make a cursor at POS and add it to `evil-mc-cursor-list'.
If SOURCE-CURSOR is specified copy its state onto the new cursor"
  (unless (cl-some (lambda (cursor)
                     (= pos (evil-mc-get-cursor-start cursor)))
                   evil-mc-cursor-list)
    (let* ((source (evil-mc-copy-cursor-state
                    (or source-cursor (evil-mc-get-default-cursor))))
           (cursor (evil-mc-put-cursor-property
                    source
                    'last-position pos
                    'order (if (null evil-mc-cursor-list) 1 ; ordered "chronologically"
                             (1+ (apply #'max
                                        (mapcar (lambda (cursor)
                                                  (evil-mc-get-cursor-property cursor 'order))
                                                evil-mc-cursor-list))))
                    'temporary-goal-column (evil-mc-column-number pos)
                    'overlay (evil-mc-cursor-overlay-at-pos pos))))
      (evil-mc-insert-cursor cursor)
      cursor)))

(defun evil-mc-undo-cursor-at-pos (pos)
  "Delete the cursor at POS from `evil-mc-cursor-list' and remove its overlay.
Return the deleted cursor."
  (let ((pos (or pos (point)))
        (found nil))
    (when evil-mc-cursor-list
      (setq evil-mc-cursor-list
            (cl-remove-if (lambda (cursor)
                            (when (eq pos (evil-mc-get-cursor-start cursor))
                              (evil-mc-delete-cursor cursor)
                              (setq found cursor)
                              t))
                          evil-mc-cursor-list)))
    found))

(defun evil-mc-undo-last-added-cursor ()
  "Delete the latest added cursor from `evil-mc-cursor-list' and remove its overlay.
Move the point to its position."
  (interactive)
  (when (evil-mc-has-cursors-p)
    (let ((latest-cursor nil)
          (order 0))
      (dolist (c evil-mc-cursor-list)
        (if (> (evil-mc-get-cursor-property c 'order) order)
            (setq latest-cursor c)))
      (goto-char (evil-mc-get-cursor-start latest-cursor))
      (setq evil-mc-cursor-list (remove latest-cursor evil-mc-cursor-list))
      (evil-mc-delete-cursor latest-cursor)
      (evil-mc-run-cursors-after t))))


(defun evil-mc-find-prev-cursor (&optional pos)
  "Find the cursor closest to POS when searching backwards."
  (let ((prev nil) (pos (or pos (point))))
    (catch 'evil-mc-undo-prev-cursor-done
      (dolist (cursor evil-mc-cursor-list)
        (if (> (evil-mc-get-cursor-start cursor) pos)
            (throw 'evil-mc-undo-prev-cursor-done t)
          (setq prev cursor))))
    prev))

(defun evil-mc-find-next-cursor (&optional pos)
  "Find the cursor closest to POS when searching forwards."
  (let ((next nil) (pos (or pos (point))))
    (catch 'evil-mc-undo-next-cursor-done
      (dolist (cursor evil-mc-cursor-list)
        (when (>= (evil-mc-get-cursor-start cursor) pos)
          (setq next cursor)
          (throw 'evil-mc-undo-next-cursor-done t))))
    next))

(defun evil-mc-find-first-cursor ()
  "Return the cursor with the lowest position."
  (car evil-mc-cursor-list))

(defun evil-mc-find-last-cursor ()
  "Return the cursor with the highest position."
  (car (last evil-mc-cursor-list)))

(defun evil-mc-make-pattern (text whole-word)
  "Make a search pattern for TEXT, that optionally matches only WHOLE-WORDs."
  (let ((literal (regexp-quote text)))
    (evil-ex-make-search-pattern
     (if whole-word (concat "\\_<" literal "\\_>") literal))))

(defun evil-mc-set-pattern-for-range (range whole-word)
  "Set `evil-mc-pattern' to the text given by RANGE, optionally matching only WHOLE-WORDs."
  (let ((start (car range)) (end (cadr range)))
    (if (and (<= (point-min) start)
             (>= (point-max) end)
             (< start end))
        (setq evil-mc-pattern
              (cons (evil-mc-make-pattern (buffer-substring-no-properties start end)
                                          whole-word)
                    range))
      (error "Invalid range %s" range))))

(defun evil-mc-set-pattern ()
  "Move the cursor to the end of the selected text or symbol at point and initialize `evil-mc-pattern'."
  (let ((whole-word (not (evil-visual-state-p))))
    (if (evil-visual-state-p)
        (let ((end (cadr (evil-visual-range))))
          (when (not (eq (point) end))
            (goto-char (1- end))))
      (let ((range (evil-inner-symbol)))
        (evil-visual-char (car range) (1- (cadr range)))))
    (setq evil-mc-pattern nil)
    (evil-mc-set-pattern-for-range (evil-visual-range) whole-word)))

(defun evil-mc-make-cursors-for-all ()
  "Make a cursor for all matches of `evil-mc-pattern'."
  (when (evil-mc-has-pattern-p)
    (let ((point (point)))
      (save-excursion
        (goto-char (point-min))
        (while (eq (evil-ex-find-next (evil-mc-get-pattern) 'forward t) t)
          (goto-char (1- (point)))
          (when (/= point (point))
            (evil-mc-run-cursors-before)
            (evil-mc-make-cursor-at-pos (point)))
          (goto-char (1+ (point))))))))

(defun evil-mc-goto-cursor (cursor create)
  "Move point to CURSOR and optionally CREATE a cursor at point."
  (when (evil-mc-has-cursors-p)
    (let ((start (evil-mc-get-cursor-start cursor))
          (point (point)))
      (when (and cursor (/= start point))
        (goto-char start)
        (when create
          (evil-mc-run-cursors-before)
          (evil-mc-make-cursor-at-pos point (evil-mc-read-cursor-state)))
        (evil-mc-write-cursor-state (evil-mc-undo-cursor cursor))
        (evil-mc-run-cursors-after t)))))

(defun evil-mc-goto-match (direction create)
  "Move point to the next match according to DIRECTION
and optionally CREATE a cursor at point."
  (when (evil-mc-has-pattern-p)
    (let ((point (point))
          (had-cursors (evil-mc-has-cursors-p))
          (found (evil-ex-find-next (evil-mc-get-pattern) direction nil)))
      (cond ((eq (evil-mc-get-pattern-length) 1)
             (cl-ecase direction
               (forward
                (setq found (evil-ex-find-next (evil-mc-get-pattern) direction nil)))
               (backward
                (setq found (evil-ex-find-next (evil-mc-get-pattern) 'forward nil)))))
            (t
             (when (and found (eq direction 'backward))
               (setq found (evil-ex-find-next (evil-mc-get-pattern) direction nil))
               (setq found (evil-ex-find-next (evil-mc-get-pattern) 'forward nil)))))
      (if found
          (goto-char (1- (point)))
        (goto-char point)
        (evil-mc-message "No more matches found for %s" (evil-mc-get-pattern-text)))
      (when (and found create (/= point (point)))
        (evil-mc-run-cursors-before)
        (evil-mc-make-cursor-at-pos point (evil-mc-read-cursor-state)))
      (evil-mc-write-cursor-state (or (evil-mc-undo-cursor-at-pos (point))
                                      (evil-mc-put-cursor-last-position
                                       (evil-mc-get-default-cursor)
                                       (point))))
      (unless (evil-mc-has-cursors-p) (evil-mc-clear-pattern))
      (evil-mc-run-cursors-after had-cursors))))

(defun evil-mc-find-and-goto-cursor (find create)
  "FIND a cursor, go to it, and optionally CREATE a cursor at point."
  (when (evil-mc-has-cursors-p)
    (evil-mc-delete-all-regions)
    (let* ((cursor (funcall find))
           (start (evil-mc-get-cursor-start cursor)))
      (when cursor
        (cond ((eq find 'evil-mc-find-first-cursor)
               (when (> (point) start) (evil-mc-goto-cursor cursor create)))
              ((eq find 'evil-mc-find-last-cursor)
               (when (< (point) start) (evil-mc-goto-cursor cursor create)))
              (t
               (evil-mc-goto-cursor cursor create))))))
  (evil-mc-print-cursors-info))

(defun evil-mc-find-and-goto-match (direction create)
  "Find the next match in DIRECTION and optionally CREATE a cursor at point."
  (unless (evil-mc-has-pattern-p) (evil-mc-set-pattern))
  (evil-mc-delete-all-regions)
  (evil-mc-goto-match direction create)
  (evil-mc-print-cursors-info))

(defun evil-mc-run-cursors-before ()
  "Runs `evil-mc-cursors-before' if there are no cursors created yet."
  (when (not (evil-mc-has-cursors-p))
    (evil-mc-cursors-before)))

(defun evil-mc-run-cursors-after (had-cursors)
  "Runs `evil-mc-cursors-after' if HAD-CURSORS and there are no more cursors."
  (when (and had-cursors (not (evil-mc-has-cursors-p)))
    (evil-mc-cursors-after)))

(defun evil-mc-cursors-before ()
  "Actions to be executed before any cursors are created."
  (setq evil-mc-cursor-state (evil-mc-read-cursor-state nil))
  (evil-mc-write-cursor-state
   (evil-mc-put-cursor-last-position (evil-mc-get-default-cursor) (point)))
  (run-hooks 'evil-mc-before-cursors-created))

(defun evil-mc-cursors-after ()
  "Actions to be executed after all cursors are deleted."
  (evil-mc-clear-pattern)
  (evil-mc-clear-cursor-list)
  (evil-mc-clear-cursor-state)
  (run-hooks 'evil-mc-after-cursors-deleted))

(defun evil-mc-before-undo-all-cursors ()
  "Actions to be executed before all cursors are deleted."
  (evil-mc-set-killed-rectangle))

(defun evil-mc-set-killed-rectangle ()
  "Add the latest kill-ring entry for each cursor to `killed-rectangle'.
So it can be pasted later with `yank-rectangle'."
  (let ((entries (evil-mc-kill-ring-entries)))
    (unless (evil-mc-all-equal entries)
      (setq killed-rectangle entries))))

(defun evil-mc-kill-ring-entries ()
  "Return the latest `kill-ring' entry for each cursor
fake or real sorted by their position in the buffer."
  (let (entries)
    (setq entries (cons (cons (car kill-ring) (point)) entries))
    (dolist (cursor evil-mc-cursor-list)
      (setq entries
            (cons (cons (car (evil-mc-get-cursor-kill-ring cursor))
                        (evil-mc-get-cursor-start cursor))
                  entries)))
    (mapcar 'car
            (sort entries
                  (lambda (x y)
                    (< (cdr x)
                       (cdr y)))))))

(defun evil-mc-make-cursor-move-by-line (dir count)
  "Create COUNT cursors one for each line moving in the direction DIR.
DIR should be 1 or -1 and COUNT should be a positive integer or nil."
  (evil-force-normal-state)
  (setq count (max 0 (or count 1)))
  (dotimes (i count)
    (evil-mc-run-cursors-before)
    (evil-mc-make-cursor-at-pos (point))
    (let ((line-move-visual t))
      (evil-line-move dir))))

(evil-define-command evil-mc-make-cursor-here ()
  "Create a cursor at point."
  :repeat ignore
  :evil-mc t
  (evil-mc-run-cursors-before)
  (evil-mc-make-cursor-at-pos (point)))

(defun evil-mc-toggle-cursor-on-click (event)
  "Create a cursor were the mouse is clicked, or remove a
cursor that is already there."
  (interactive "e")
  (mouse-minibuffer-check event)
  (let ((position (event-end event)))
    (when (not (windowp (posn-window position)))
      (error "Position not in text area of window"))
    (select-window (posn-window position))
    (let ((pt (posn-point position)))
      (when (numberp pt)
        (let* ((existing (evil-mc-find-next-cursor pt))
               (start (evil-mc-get-cursor-start existing)))
          (if (and existing (eq pt start))
              (evil-mc-undo-cursor existing)
            (save-excursion
              (goto-char pt)
              (evil-mc-make-cursor-here))))))))

(defun evil-mc-make-cursor-in-visual-selection-block (column)
  "Create cursors at COLUMN column."
  (evil-visual-rotate 'upper-left)
  (dotimes (num (- (count-lines evil-visual-beginning evil-visual-end) 1))
    (if (not (= (move-to-column column) column))
        (progn
          (indent-to-column column)
          (move-to-column column)))
    (evil-mc-run-cursors-before)
    (evil-mc-make-cursor-at-pos (point))
    (forward-line))
  (move-to-column column))

(defun evil-mc-make-cursor-in-visual-selection (beg)
  "If BEG is non-nil, create cursors at beginning of line."
  (evil-visual-rotate 'upper-left)
  (dotimes (num (- (count-lines evil-visual-beginning evil-visual-end) 1))
    (evil-mc-run-cursors-before)
    (evil-mc-make-cursor-at-pos
     (if beg
         (line-beginning-position)
       (line-end-position)))
    (forward-line))
  (if beg
      (beginning-of-line)
    (end-of-line)))

(evil-define-command evil-mc-make-cursor-in-visual-selection-beg ()
  "Create cursor in beginning of every visually selected line.
Acts like the I key in evil-visual-state."
  :repeat ignore
  :evil-mc t
  (if (evil-visual-state-p)
      (progn
        (if (eq (evil-visual-type) 'block)
            (evil-mc-make-cursor-in-visual-selection-block
             (min (evil-column evil-visual-beginning)
                  (evil-column evil-visual-end)))
          (evil-mc-make-cursor-in-visual-selection t))
        (evil-mc-execute-for-all-cursors #'evil-insert-state))
    (message "%s: Only works with a selection" this-command)))

(evil-define-command evil-mc-make-cursor-in-visual-selection-end ()
  "Create cursor at end of every visually selected line.
Acts like the A key in evil-visual-state."
  :repeat ignore
  :evil-mc t
  (if (evil-visual-state-p)
      (progn
        (if (eq (evil-visual-type) 'block)
            (evil-mc-make-cursor-in-visual-selection-block
             (max (evil-column evil-visual-beginning)
                  (evil-column evil-visual-end)))
          (evil-mc-make-cursor-in-visual-selection nil))
        (evil-mc-execute-for-all-cursors #'evil-insert-state))
    (message "%s: Only works with a selection" this-command)))

(evil-define-command evil-mc-make-cursor-move-next-line (count)
  "Create a cursor at point and move to next line."
  :repeat ignore
  :evil-mc t
  (interactive "<c>")
  (evil-mc-make-cursor-move-by-line 1 count))

(evil-define-command evil-mc-make-cursor-move-prev-line (count)
  "Create a cursor at point and move to previous line."
  :repeat ignore
  :evil-mc t
  (interactive "<c>")
  (evil-mc-make-cursor-move-by-line -1 count))

(evil-define-command evil-mc-make-and-goto-first-cursor ()
  "Make a cursor at point and move point to the cursor with the lowest position."
  :repeat ignore
  :evil-mc t
  (evil-mc-find-and-goto-cursor 'evil-mc-find-first-cursor t))

(evil-define-command evil-mc-make-and-goto-last-cursor ()
  "Make a cursor at point and move point to the cursor with the last position."
  :repeat ignore
  :evil-mc t
  (evil-mc-find-and-goto-cursor 'evil-mc-find-last-cursor t))

(evil-define-command evil-mc-make-and-goto-prev-cursor ()
  "Make a cursor at point and move point to the cursor
closest to it when searching backwards."
  :repeat ignore
  :evil-mc t
  (evil-mc-find-and-goto-cursor 'evil-mc-find-prev-cursor t))

(evil-define-command evil-mc-make-and-goto-next-cursor ()
  "Make a cursor at point and move point to the cursor
closest to it when searching forwards."
  :repeat ignore
  :evil-mc t
  (evil-mc-find-and-goto-cursor 'evil-mc-find-next-cursor t))

(evil-define-command evil-mc-skip-and-goto-prev-cursor ()
  "Move point to the cursor closest to it when searching backwards."
  :repeat ignore
  :evil-mc t
  (evil-mc-find-and-goto-cursor 'evil-mc-find-prev-cursor nil))

(evil-define-command evil-mc-skip-and-goto-next-cursor ()
  "Move point to the cursor closest to it when searching forwards."
  :repeat ignore
  :evil-mc t
  (evil-mc-find-and-goto-cursor 'evil-mc-find-next-cursor nil))

(evil-define-command evil-mc-skip-and-goto-next-match ()
  "Initialize `evil-mc-pattern' and go to the next match."
  :repeat ignore
  :evil-mc t
  (evil-mc-find-and-goto-match 'forward nil))

(evil-define-command evil-mc-skip-and-goto-prev-match ()
  "Initialize `evil-mc-pattern' and go to the previous match."
  :repeat ignore
  :evil-mc t
  (evil-mc-find-and-goto-match 'backward nil))

(evil-define-command evil-mc-make-and-goto-next-match ()
  "Initialize `evil-mc-pattern', make a cursor at point, and go to the next match."
  :repeat ignore
  :evil-mc t
  (evil-mc-find-and-goto-match 'forward t))

(evil-define-command evil-mc-make-and-goto-prev-match ()
  "Initialize `evil-mc-pattern', make a cursor at point, and go to the previous match."
  :repeat ignore
  :evil-mc t
  (evil-mc-find-and-goto-match 'backward t))

(evil-define-command evil-mc-undo-all-cursors ()
  "Delete all cursors and remove them from `evil-mc-cursor-list'."
  :repeat ignore
  :evil-mc t
  (when (evil-mc-has-cursors-p)
    (evil-mc-before-undo-all-cursors)
    (mapc 'evil-mc-delete-cursor evil-mc-cursor-list)
    (evil-exit-visual-state)
    (evil-mc-cursors-after)))

(evil-define-command evil-mc-make-all-cursors ()
  "Initialize `evil-mc-pattern' and make cursors for all matches."
  :repeat ignore
  :evil-mc t
  (if (evil-mc-has-cursors-p) (user-error "Cursors already exist.")
    (evil-mc-set-pattern)
    (evil-exit-visual-state)
    (evil-mc-make-cursors-for-all)
    (evil-mc-print-cursors-info "Created")))

(provide 'evil-mc-cursor-make)

;;; evil-mc-cursor-make.el ends here
