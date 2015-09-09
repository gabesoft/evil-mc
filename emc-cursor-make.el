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
  (let ((start (emc-get-cursor-start cursor))
        (point (point)))
    (when (and cursor (/= start point))
      (goto-char start)
      (when create (emc-make-cursor-at-pos point))
      (emc-undo-cursor cursor))))

(defun emc-goto-match (direction create)
  "Move point to the next match according to DIRECTION
and optionally CREATE a cursor at point."
  (when (emc-has-pattern-p)
    (let ((point (point))
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
      (emc-undo-cursor-at-pos))
    (message "%s cursors for \"%s\""
             (1+ (length emc-cursor-list))
             (emc-get-pattern-text))))

;; All interactive cursor commands should
;; - (emc-command-reset)
;; - set pattern if necessary
;; - if visual-state and (emc-has-cursors-p)
;;   - if point < mark (evil-exchange-point-and-mark for all)
;;   - delete all regions
;; - exit visual mode

;; (defmacro emc-define-cursor-command (command documentation &rest body)
;;   "Define a cursor COMMAND documented by DOCUMENTATION that executes BODY."
;;   `(evil-define-command ,command ()
;;      'documentation
;;      :repeat ignore
;;      (interactive)
;;      ,@body))

;; TODO make this a macro
(defun emc-initialize-cursors
    "Sets up cursors state before an interactive command."
  (emc-command-reset)
  (unless (emc-has-pattern-p) (emc-set-pattern))
  (when (and (evil-visual-state-p) (emc-has-cursors-p))
    (when (< (point) (mark)) (evil-exchange-point-and-mark))
    (dolist (cursor emc-cursor-list)
      (emc-delete-region-overlay cursor)
      ;; TODO remove the region from cursor
      ;; (emc-put-object-property cursor :region nil)
      ))
  (evil-exit-visual-state))

(evil-define-command emc-make-and-goto-first-cursor ()
  "Make a cursor at point and move point to the cursor with the lowest position."
  :repeat ignore
  (interactive)
  (let* ((cursor (emc-find-first-cursor))
         (start (emc-get-cursor-start cursor)))
    (when (and cursor (> (point ) start))
      (emc-goto-cursor cursor t))))

(evil-define-command emc-make-and-goto-last-cursor ()
  "Make a cursor at point and move point to the cursor with the last position."
  :repeat ignore
  (interactive)
  (let* ((cursor (emc-find-last-cursor))
         (start (emc-get-cursor-start cursor)))
    (when (and cursor (< (point) start))
      (emc-goto-cursor cursor t))))

(evil-define-command emc-make-and-goto-prev-cursor ()
  "Make a cursor at point and move point to the cursor
closest to it when searching backwards."
  (interactive)
  (emc-goto-cursor (emc-find-prev-cursor) t))

(evil-define-command emc-make-and-goto-next-cursor ()
  "Make a cursor at point and move point to the cursor
closest to it when searching forwards."
  (interactive)
  (emc-goto-cursor (emc-find-next-cursor) t))

(evil-define-command emc-skip-and-goto-prev-cursor ()
  "Move point to the cursor closest to it when searching backwards."
  (interactive)
  (emc-goto-cursor (emc-find-prev-cursor) nil))

(evil-define-command emc-skip-and-goto-next-cursor ()
  "Move point to the cursor closest to it when searching forwards."
  (interactive)
  (emc-goto-cursor (emc-find-next-cursor) nil))

;; TODO refactor 4 methods below
(evil-define-command emc-skip-cursor-and-goto-next-match ()
  "Initialize `emc-pattern' and go to the next match."
  (interactive)
  (unless (emc-has-pattern-p) (emc-set-pattern))
  (evil-exit-visual-state)
  (emc-goto-match 'forward nil))

(evil-define-command emc-skip-cursor-and-goto-prev-match ()
  "Initialize `emc-pattern' and go to the previous match."
  (interactive)
  (unless (emc-has-pattern-p) (emc-set-pattern))
  (evil-exit-visual-state)
  (emc-goto-match 'backward nil))

(evil-define-command emc-make-cursor-and-goto-next-match ()
  "Initialize `emc-pattern', make a cursor at point, and go to the next match."
  (interactive)
  (unless (emc-has-pattern-p) (emc-set-pattern))
  (evil-exit-visual-state)
  (emc-goto-match 'forward t))

(evil-define-command emc-make-cursor-and-goto-prev-match ()
  "Initialize `emc-pattern', make a cursor at point, and go to the previous match."
  (interactive)
  (unless (emc-has-pattern-p) (emc-set-pattern))
  (evil-exit-visual-state)
  (emc-goto-match 'backward t))

(evil-define-command emc-undo-all-cursors ()
  "Delete all cursors and remove them from `emc-cursor-list'."
  :repeat ignore
  (interactive)
  (mapc 'emc-delete-cursor emc-cursor-list)
  (evil-exit-visual-state)
  (setq emc-cursor-list nil)
  (setq emc-pattern nil))

(evil-define-command emc-make-cursors-for-all-matches ()
  "Initialize `emc-pattern' and make cursors for all matches."
  :repeat ignore
  (interactive)
  (if (emc-has-cursors-p) (error "Cursors already exist.")
    (emc-set-pattern)
    (evil-exit-visual-state)
    (emc-make-cursors-for-all)
    (message "%s matches for \"%s\""
             (1+ (length emc-cursor-list))
             (emc-get-pattern-text))))

(defun emc-setup-cursor-key-maps ()
  "Set up key maps for cursor operations."
  (interactive)
  (define-key evil-normal-state-map (kbd "grm") 'emc-make-cursors-for-all-matches)
  (define-key evil-visual-state-map (kbd "grm") 'emc-make-cursors-for-all-matches)

  ;; TODO should escape with ESC or C-g
  (define-key evil-normal-state-map (kbd "grc") 'emc-undo-all-cursors)
  (define-key evil-visual-state-map (kbd "grc") 'emc-undo-all-cursors)

  (define-key evil-normal-state-map (kbd "grn") 'emc-make-and-goto-next-cursor)
  (define-key evil-normal-state-map (kbd "grp") 'emc-make-and-goto-prev-cursor)
  (define-key evil-normal-state-map (kbd "grf") 'emc-make-and-goto-first-cursor)
  (define-key evil-normal-state-map (kbd "grl") 'emc-make-and-goto-last-cursor)

  (define-key evil-normal-state-map (kbd "gpn") 'emc-make-cursor-and-goto-next-match)
  (define-key evil-normal-state-map (kbd "gpp") 'emc-make-cursor-and-goto-prev-match)
  (define-key evil-normal-state-map (kbd "gsn") 'emc-skip-cursor-and-goto-next-match)
  (define-key evil-normal-state-map (kbd "gsp") 'emc-skip-cursor-and-goto-prev-match)
  )

;; TODO handle visual

(provide 'emc-cursor-make)

;;; emc-cursor-make.el ends here
