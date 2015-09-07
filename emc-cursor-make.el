;;; emc-cursor-make.el --- Fake cursor creation and deletion

;;; Commentary:

;; This file contains functions for creating and deleting fake cursors

;; Functionality:
;; - allow cursors for one letter
;; - don't create cursors next to each other unless on the same letter
;; - next/prev should wrap around
;; - create/undo on next as well as prev
;; - create for all word/WORD/selection at cursor
;; - skip functionality
;; - create a cursor at an arbitrary point (not related to a pattern)
;; - navigate through cursors
;; - delete a cursor at point
;; - temporarily freeze all fake cursors
;; - fake cursors should reflect the current evil state
;; - the real cursor should be visible in relation to the fake ones

;; Implementation:
;; - use `emc-cursor-state' to create cursors and add to `emc-cursor-list'
;; - cursor navigation using `emc-pattern'
;;    - the pattern should contain the text as well as type
;;      region, word, WORD if necessary
;;
;; - goto-next
;; - goto-prev
;; - goto-first
;; - goto-last
;; - make-next
;; - make-prev
;; - undo-next
;; - undo-prev
;; - skip-next
;; - skip-prev
;; - make-here
;; - undo-here
;; - make-all
;; - undo-all

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

(defun emc-cursor-overlay (start end)
  "Make an overlay for a cursor from START to END."
  (let ((overlay (make-overlay start end nil nil nil)))
    (overlay-put overlay 'type 'emc-cursor)
    (overlay-put overlay 'priority 99)
    overlay))

(defun emc-cursor-overlay-at-eol (pos)
  "Make a cursor overlay at POS assuming pos is at the end of line."
  (let ((overlay (emc-cursor-overlay pos pos))
        (face (or emc-cursor-current-face '(emc-cursor-default-face))))
    (overlay-put overlay 'after-string (propertize " " 'face face))
    overlay))

(defun emc-cursor-overlay-inline (pos)
  "Make a cursor overlay at POS assuming pos is not at the end of line."
  (let ((overlay (emc-cursor-overlay pos (1+ pos)))
        (face (or emc-cursor-current-face '(emc-cursor-default-face))))
    (overlay-put overlay 'face face)
    overlay))

(defun emc-cursor-overlay-at-pos (&optional pos)
  "Make a cursor overlay at POS."
  (interactive)
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

(defun emc-insert-cursor (cursor)
  "Insert CURSOR into `emc-cursor-list' at the correct location so that
the cursors are ordered by the cursor overlay start position."
  (cond ((null emc-cursor-list)
         (setq emc-cursor-list (list cursor)))
        ((> (emc-get-cursor-start (car emc-cursor-list))
            (emc-get-cursor-start cursor))
         (setq emc-cursor-list (cons cursor emc-cursor-list)))
        (t (let ((start (emc-get-cursor-start cursor))
                 (list emc-cursor-list))
             (while (and (cdr list)
                         (> start (emc-get-cursor-start (cadr list))))
               (setq list (cdr list)))
             (setcdr list (cons cursor (cdr list))))))
  emc-cursor-list)

(defun emc-delete-cursor (cursor)
  "Delete all overlays associated with CURSOR."
  (emc-delete-cursor-overlay cursor)
  (emc-delete-region-overlay (emc-get-cursor-region cursor)))

(defun emc-undo-cursor (cursor)
  "Delete CURSOR and remove it from `emc-cursor-list'."
  (when cursor
    (let ((start (emc-get-cursor-start cursor)))
      (emc-delete-cursor cursor)
      (setq emc-cursor-list (delq cursor emc-cursor-list))
      start)))

(defun emc-make-cursor-at-pos (&optional pos)
  "Makes a cursor at POS and adds it to `emc-cursor-list'."
  (emc-insert-cursor (emc-put-cursor-property
                      nil
                      :overlay (emc-cursor-overlay-at-pos pos)
                      :kill-ring (copy-sequence kill-ring)
                      :kill-ring-yank-pointer nil)))

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

(defun emc-undo-prev-cursor (&optional pos)
  "Delete the cursor closest to POS when searching backwards."
  (emc-undo-cursor (emc-find-prev-cursor pos)))

(defun emc-undo-next-cursor (&optional pos)
  "Delete the cursor closest to POS when searching forwards."
  (emc-undo-cursor (emc-find-next-cursor pos)))

(defun emc-goto-cursor (cursor create)
  "Move point to CURSOR and optionally CREATE a cursor at point."
  (let ((start (emc-get-cursor-start cursor))
        (point (point)))
    (when (and cursor (/= start point))
      (goto-char start)
      (when create (emc-make-cursor-at-pos point))
      (emc-undo-cursor cursor))))

(evil-define-command emc-goto-prev-cursor ()
  "Move point to the cursor closest to it when searching backwards."
  (interactive)
  (emc-goto-cursor (emc-find-prev-cursor) t))

(evil-define-command emc-goto-next-cursor ()
  "Move point to the cursor closest to it when searching forwards."
  (interactive)
  (emc-goto-cursor (emc-find-next-cursor) t))

(evil-define-command emc-goto-first-cursor ()
  "Move point to the cursor with the lowest position."
  :repeat ignore
  (interactive)
  (let* ((cursor (emc-find-first-cursor))
         (start (emc-get-cursor-start cursor)))
    (when (and cursor (> (point ) start))
      (emc-goto-cursor cursor t))))

(evil-define-command emc-goto-last-cursor ()
  "Move point to the cursor with the last position."
  :repeat ignore
  (interactive)
  (let* ((cursor (emc-find-last-cursor))
         (start (emc-get-cursor-start cursor)))
    (when (and cursor (< (point) start))
      (emc-goto-cursor cursor t))))

(evil-define-command emc-undo-all-cursors ()
  "Delete all cursors and remove them from `emc-cursor-list'."
  :repeat ignore
  (interactive)
  (mapc 'emc-delete-cursor emc-cursor-list)
  (setq emc-cursor-list nil)
  (setq emc-pattern nil))

(defun emc-make-pattern (text whole-word)
  "Make a search pattern for TEXT. If WHOLE-WORD is not nil
then the pattern will only match whole words."
  (evil-ex-make-search-pattern (if whole-word
                                   (concat "\\_<" text "\\_>")
                                 text)))

(defun emc-set-pattern (start end whole-word)
  "Set `emc-pattern' to the text given by START and END. If WHOLE-WORD
is not nil the pattern will only match whole words."
  (if (and (<= (point-min) start)
           (>= (point-max) end)
           (< start end))
      (setq emc-pattern
            (cons (emc-make-pattern (buffer-substring-no-properties start end)
                                    whole-word)
                  (cons start end)))
    (error "Invalid bounds %s %s" start end)))

(defun emc-set-pattern-from-selection ()
  "Set `emc-pattern' to the selected text."
  (let ((whole-word (not (evil-visual-state-p))))
    (unless (evil-visual-state-p)
      (let ((range (evil-inner-symbol)))
        (evil-visual-char (car range) (1- (cadr range)))))
    (setq emc-pattern nil)
    (let ((range (evil-visual-range)))
      (emc-set-pattern (car range) (cadr range) whole-word)))
  (message "pattern %s" emc-pattern))

(defun emc-make-cursor-for-all-matches ()
  "Make a cursor for all matches of `emc-pattern'."
  (when (and (emc-pattern-p)
             (not (emc-has-cursors-p)))
    (let ((point (point)))
      (save-excursion
        (goto-char (point-min))
        (while (eq (evil-ex-find-next (emc-get-pattern) 'forward t) t)
          (goto-char (1- (point)))
          (when (/= point (point)) (emc-make-cursor-at-pos))
          (goto-char (1+ (point))))))))

(evil-define-command emc-set-pattern-and-make-cursors ()
  "Set `emc-pattern' and make cursors for all matches."
  :repeat ignore
  (interactive)
  (emc-set-pattern-from-selection)
  (evil-exit-visual-state)
  (emc-make-cursor-for-all-matches)
  (message "%s cursors created at %s"
           (length emc-cursor-list)
           (mapcar 'emc-get-cursor-start emc-cursor-list)))

(defun emc-setup-cursor-key-maps ()
  "Set up key maps for cursor operations."
  (interactive)
  (define-key evil-normal-state-map (kbd "grm") 'emc-set-pattern-and-make-cursors)
  (define-key evil-visual-state-map (kbd "grm") 'emc-set-pattern-and-make-cursors)

  (define-key evil-normal-state-map (kbd "grn") 'emc-goto-next-cursor)
  (define-key evil-normal-state-map (kbd "grp") 'emc-goto-prev-cursor)
  (define-key evil-normal-state-map (kbd "grf") 'emc-goto-first-cursor)
  (define-key evil-normal-state-map (kbd "grl") 'emc-goto-last-cursor)
  )

;; TODO implement make/skip next prev based on pattern
;; - emc-make-for-all-matches
;;    - the cursor must not move
;; - emc-make-for-next-match
;; - emc-make-for-prev-match
;; - emc-skip-for-next-match
;; - emc-skip-for-prev-match

;; (emc-make-cursor-for-all-matches)
;; (setq emc-pattern '("emc" nil))
;; (emc-set-pattern-from-selection)
;; (emc-find-next 'forward nil)
;; (emc-set-cursor-face '(emc-region-face))
;; (emc-set-cursor-face nil)
;; (emc-cursor-overlay-at-eol (point))
;; (emc-cursor-overlay-inline (point))

(provide 'emc-cursor-make)

;;; emc-cursor-make.el ends here
