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

(defun emc-cursor-overlay-at-eol (position)
  "Make a cursor overlay at POSITION assuming position is at the end of line."
  (let ((overlay (emc-cursor-overlay position position))
        (face (or emc-cursor-current-face '(emc-cursor-default-face))))
    (overlay-put overlay 'after-string (propertize " " 'face face))
    overlay))

(defun emc-cursor-overlay-inline (position)
  "Make a cursor overlay at POSITION assuming position is not at the end of line."
  (let ((overlay (emc-cursor-overlay position (1+ position)))
        (face (or emc-cursor-current-face '(emc-cursor-default-face))))
    (overlay-put overlay 'face face)
    overlay))

(defun emc-make-cursor-overlay-at-point ()
  "Make a cursor overlay at point."
  (interactive)
  (if (eolp)
      (emc-cursor-overlay-at-eol (point))
    (emc-cursor-overlay-inline (point))))

(defun emc-remove-cursor-at-point ()
  "Remove the cursor at point and removes its overlay."
  (interactive)
  (when emc-cursor-list
    (setq emc-cursor-list
          (remove-if (lambda (cursor)
                       (let ((overlay (emc-get-cursor-overlay cursor)))
                         (eq (overlay-start overlay) (point))))
                     emc-cursor-list)))
  (remove-overlays (point) (1+ (point))))

(defun emc-make-cursor-at-position (&optional position)
  "Makes a cursor at POSITION and adds it to `emc-cursor-list'."
  (let ((pos (or position (point))))
    ))


;; (emc-set-cursor-face '(emc-region-face))
;; (emc-set-cursor-face nil)
;; (emc-cursor-overlay-at-eol (point))
;; (emc-cursor-overlay-inline (point))

(provide 'emc-cursor-make)

;;; emc-cursor-make.el ends here
