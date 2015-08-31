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


(provide 'emc-cursor-make)

;;; emc-cursor-make.el ends here
