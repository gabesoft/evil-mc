;;; emc-cursor-make.el --- Fake cursor creation and deletion

;;; Commentary:

;; This file contains functions for creating and deleting fake cursors

(require 'emc-common)
(require 'emc-vars)
(require 'emc-cursor-state)
(require 'emc-region)

;;; Code:

(defun emc-get-cursor-face ()
  "Get the current cursor face."
  (or emc-cursor-current-face '(emc-cursor-default-face)))

(defun emc-set-cursor-face (face)
  "Set the current cursor FACE."
  (setq emc-cursor-current-face face))

(defun emc-print-cursors-info (&optional msg)
  "Prints information about the current cursors preceded by MSG."
  (when (emc-has-cursors-p)
    (message "%s %s cursors matching \"%s\""
             (or msg "There are")
             (1+ (length emc-cursor-list))
             (emc-get-pattern-text))))

(defun emc-cursor-overlay (start end)
  "Make an overlay for a cursor from START to END."
  (let ((overlay (make-overlay start end nil nil nil)))
    (overlay-put overlay 'type 'emc-cursor)
    (overlay-put overlay 'priority emc-cursor-overlay-priority)
    overlay))

(defun emc-cursor-overlay-at-eol (pos)
  "Make a cursor overlay at POS assuming pos is at the end of line."
  (let ((overlay (emc-cursor-overlay pos pos))
        (face (emc-get-cursor-face)))
    (overlay-put overlay 'after-string (propertize " " 'face face))
    overlay))

(defun emc-cursor-overlay-inline (pos)
  "Make a cursor overlay at POS assuming pos is not at the end of line."
  (let ((overlay (emc-cursor-overlay pos (1+ pos)))
        (face (emc-get-cursor-face)))
    (overlay-put overlay 'face face)
    overlay))

(defun emc-cursor-overlay-at-pos (&optional pos)
  "Make a cursor overlay at POS."
  (let ((pos (or pos (point))))
    (save-excursion
      (goto-char pos)
      (if (eolp)
          (emc-cursor-overlay-at-eol (point))
        (emc-cursor-overlay-inline (point))))))

(defun emc-sort-cursors ()
  "Sort the cursors list by position."
  (setq emc-cursor-list
        (sort emc-cursor-list
              (lambda (x y)
                (< (emc-get-cursor-start x)
                   (emc-get-cursor-start y))))))

(defun emc-insert-cursor-into-list (cursor cursor-list)
  "Insert CURSOR into CURSOR-LIST at the correct location so that
the cursors are ordered by the cursor overlay start position."
  (cond ((null cursor-list)
         (setq cursor-list (list cursor)))
        ((> (emc-get-cursor-start (car cursor-list))
            (emc-get-cursor-start cursor))
         (setq cursor-list (cons cursor cursor-list)))
        (t (let ((start (emc-get-cursor-start cursor))
                 (list cursor-list))
             (while (and (cdr list)
                         (> start (emc-get-cursor-start (cadr list))))
               (setq list (cdr list)))
             (setcdr list (cons cursor (cdr list))))))
  cursor-list)

(defun emc-insert-cursor (cursor)
  "Insert CURSOR into `emc-cursor-list' at the correct location so that
the cursors are ordered by the cursor overlay start position."
  (setq emc-cursor-list (emc-insert-cursor-into-list
                         cursor
                         emc-cursor-list)))

(defun emc-delete-cursor (cursor)
  "Delete all overlays associated with CURSOR."
  (emc-delete-cursor-overlay cursor)
  (emc-delete-region-overlay (emc-get-cursor-region cursor)))

(defun emc-delete-all-regions ()
  "Clear all visual regions and exit visual state."
  (when (evil-visual-state-p)
    (dolist (cursor emc-cursor-list)
      (emc-delete-region-overlay (emc-get-cursor-region cursor)))
    (evil-exit-visual-state)))

(defun emc-undo-cursor (cursor)
  "Delete CURSOR and remove it from `emc-cursor-list'."
  (when (and cursor (emc-has-cursors-p))
    (let ((start (emc-get-cursor-start cursor)))
      (emc-delete-cursor cursor)
      (setq emc-cursor-list (delq cursor emc-cursor-list))
      start)))

(defun emc-make-cursor-at-pos (&optional pos)
  "Make a cursor at POS and add it to `emc-cursor-list'."
  (let ((evil-jump-list nil)
        (first-cursor (not (emc-has-cursors-p)))
        (pos (or pos (point))))
    (when first-cursor (emc-cursors-before))
    (let ((cursor (emc-put-cursor-property
                   nil
                   'overlay (emc-cursor-overlay-at-pos pos)
                   'column (emc-column-number pos)
                   'evil-markers-alist (default-value 'evil-markers-alist)
                   'evil-repeat-ring (make-ring 10)
                   'kill-ring (copy-sequence kill-ring)
                   'kill-ring-yank-pointer nil)))
      (emc-insert-cursor cursor)
      cursor)))

(defun emc-undo-cursor-at-pos (&optional pos)
  "Delete the cursor at POS from `emc-cursor-list' and remove its overlay."
  (interactive)
  (let ((pos (or pos (point))))
    (when emc-cursor-list
      (setq emc-cursor-list
            (remove-if (lambda (cursor)
                         (let* ((overlay (emc-get-cursor-overlay cursor))
                                (match (eq pos (overlay-start overlay))))
                           (when match (delete-overlay overlay) t)))
                       emc-cursor-list)))))

(defun emc-find-prev-cursor (&optional pos)
  "Find the cursor closest to POS when searching backwards."
  (let ((prev nil) (pos (or pos (point))))
    (catch 'emc-undo-prev-cursor-done
      (dolist (cursor emc-cursor-list)
        (if (> (emc-get-cursor-start cursor) pos)
            (throw 'emc-undo-prev-cursor-done t)
          (setq prev cursor))))
    prev))

(defun emc-find-next-cursor (&optional pos)
  "Find the cursor closest to POS when searching forwards."
  (let ((next nil) (pos (or pos (point))))
    (catch 'emc-undo-next-cursor-done
      (dolist (cursor emc-cursor-list)
        (when (>= (emc-get-cursor-start cursor) pos)
          (setq next cursor)
          (throw 'emc-undo-next-cursor-done t))))
    next))

(defun emc-find-first-cursor ()
  "Return the cursor with the lowest position."
  (car emc-cursor-list))

(defun emc-find-last-cursor ()
  "Return the cursor with the highest position."
  (car (last emc-cursor-list)))

(defun emc-make-pattern (text whole-word)
  "Make a search pattern for TEXT, that optionally matches only WHOLE-WORDs."
  (evil-ex-make-search-pattern (if whole-word
                                   (concat "\\_<" text "\\_>")
                                 text)))

(defun emc-set-pattern-for-range (range whole-word)
  "Set `emc-pattern' to the text given by RANGE, optionally matching only WHOLE-WORDs."
  (let ((start (car range)) (end (cadr range)))
    (if (and (<= (point-min) start)
             (>= (point-max) end)
             (< start end))
        (setq emc-pattern
              (cons (emc-make-pattern (buffer-substring-no-properties start end)
                                      whole-word)
                    range))
      (error "Invalid range %s" range))))

(defun emc-set-pattern ()
  "Set `emc-pattern' to the selected text."
  (let ((whole-word (not (evil-visual-state-p))))
    (unless (evil-visual-state-p)
      (let ((range (evil-inner-symbol)))
        (evil-visual-char (car range) (1- (cadr range)))))
    (setq emc-pattern nil)
    (emc-set-pattern-for-range (evil-visual-range) whole-word)))

(defun emc-make-cursors-for-all ()
  "Make a cursor for all matches of `emc-pattern'."
  (when (emc-has-pattern-p)
    (let ((point (point)))
      (save-excursion
        (goto-char (point-min))
        (while (eq (evil-ex-find-next (emc-get-pattern) 'forward t) t)
          (goto-char (1- (point)))
          (when (/= point (point)) (emc-make-cursor-at-pos))
          (goto-char (1+ (point))))))))

(defun emc-goto-cursor (cursor create)
  "Move point to CURSOR and optionally CREATE a cursor at point."
  (when (emc-has-cursors-p)
    (let ((start (emc-get-cursor-start cursor))
          (point (point)))
      (when (and cursor (/= start point))
        (goto-char start)
        (when create (emc-make-cursor-at-pos point))
        (emc-undo-cursor cursor)
        (unless (emc-has-cursors-p) (emc-cursors-after))))))

(defun emc-goto-match (direction create)
  "Move point to the next match according to DIRECTION
and optionally CREATE a cursor at point."
  (when (emc-has-pattern-p)
    (let ((point (point))
          (had-cursors (emc-has-cursors-p))
          (found (evil-ex-find-next (emc-get-pattern) direction nil)))
      (cond ((eq (emc-get-pattern-length) 1)
             (ecase direction
               (forward
                (setq found (evil-ex-find-next (emc-get-pattern) direction nil)))
               (backward
                (setq found (evil-ex-find-next (emc-get-pattern) 'forward nil)))))
            (t
             (when (and found (eq direction 'backward))
               (setq found (evil-ex-find-next (emc-get-pattern) direction nil))
               (setq found (evil-ex-find-next (emc-get-pattern) 'forward nil)))))
      (goto-char (1- (point)))
      (when (and found create (/= point (point)))
        (emc-make-cursor-at-pos point))
      (emc-undo-cursor-at-pos)
      (unless (emc-has-cursors-p) (emc-clear-pattern))
      (when (and had-cursors (not (emc-has-cursors-p))) (emc-cursors-after)))))

(defun emc-find-and-goto-cursor (find create)
  "FIND a cursor, go to it, and optionally CREATE a cursor at point."
  (when (emc-has-cursors-p)
    (emc-delete-all-regions)
    (let* ((cursor (funcall find))
           (start (emc-get-cursor-start cursor)))
      (when cursor
        (cond ((eq find 'emc-find-first-cursor)
               (when (> (point) start) (emc-goto-cursor cursor create)))
              ((eq find 'emc-find-last-cursor)
               (when (< (point) start) (emc-goto-cursor cursor create)))
              (t
               (emc-goto-cursor cursor create))))))
  (emc-print-cursors-info))

(defun emc-find-and-goto-match (direction create)
  "Find the next match in DIRECTION and optionally CREATE a cursor at point."
  (unless (emc-has-pattern-p) (emc-set-pattern))
  (emc-delete-all-regions)
  (emc-goto-match direction create)
  (emc-print-cursors-info))

(evil-define-command emc-make-cursor-here ()
  "Create a cursor at point."
  :repeat ignore
  (emc-make-cursor-at-pos))

(evil-define-command emc-make-and-goto-first-cursor ()
  "Make a cursor at point and move point to the cursor with the lowest position."
  :repeat ignore
  (emc-find-and-goto-cursor 'emc-find-first-cursor t))

(evil-define-command emc-make-and-goto-last-cursor ()
  "Make a cursor at point and move point to the cursor with the last position."
  :repeat ignore
  (emc-find-and-goto-cursor 'emc-find-last-cursor t))

(evil-define-command emc-make-and-goto-prev-cursor ()
  "Make a cursor at point and move point to the cursor
closest to it when searching backwards."
  :repeat ignore
  (emc-find-and-goto-cursor 'emc-find-prev-cursor t))

(evil-define-command emc-make-and-goto-next-cursor ()
  "Make a cursor at point and move point to the cursor
closest to it when searching forwards."
  :repeat ignore
  (emc-find-and-goto-cursor 'emc-find-next-cursor t))

(evil-define-command emc-skip-and-goto-prev-cursor ()
  "Move point to the cursor closest to it when searching backwards."
  :repeat ignore
  (emc-find-and-goto-cursor 'emc-find-prev-cursor nil))

(evil-define-command emc-skip-and-goto-next-cursor ()
  "Move point to the cursor closest to it when searching forwards."
  :repeat ignore
  (emc-find-and-goto-cursor 'emc-find-next-cursor nil))

(evil-define-command emc-skip-and-goto-next-match ()
  "Initialize `emc-pattern' and go to the next match."
  :repeat ignore
  (emc-find-and-goto-match 'forward nil))

(evil-define-command emc-skip-and-goto-prev-match ()
  "Initialize `emc-pattern' and go to the previous match."
  :repeat ignore
  (emc-find-and-goto-match 'backward nil))

(evil-define-command emc-make-and-goto-next-match ()
  "Initialize `emc-pattern', make a cursor at point, and go to the next match."
  :repeat ignore
  (emc-find-and-goto-match 'forward t))

(evil-define-command emc-make-and-goto-prev-match ()
  "Initialize `emc-pattern', make a cursor at point, and go to the previous match."
  :repeat ignore
  (emc-find-and-goto-match 'backward t))

(defun emc-cursors-before ()
  "Actions to be executed before any cursors are created."
  (run-hooks 'emc-before-cursors-created))

(defun emc-cursors-after ()
  "Actions to be executed after all cursors are deleted."
  (emc-clear-pattern)
  (emc-clear-cursors)
  (run-hooks 'emc-after-cursors-deleted)
  (message "All cursors cleared"))

(evil-define-command emc-undo-all-cursors ()
  "Delete all cursors and remove them from `emc-cursor-list'."
  :repeat ignore
  (when (emc-has-cursors-p)
    (mapc 'emc-delete-cursor emc-cursor-list)
    (evil-exit-visual-state)
    (emc-cursors-after)))

(evil-define-command emc-make-all-cursors ()
  "Initialize `emc-pattern' and make cursors for all matches."
  :repeat ignore
  (if (emc-has-cursors-p) (user-error "Cursors already exist.")
    (emc-set-pattern)
    (evil-exit-visual-state)
    (emc-make-cursors-for-all)
    (emc-print-cursors-info "Created")))

(provide 'emc-cursor-make)

;;; emc-cursor-make.el ends here
