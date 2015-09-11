;;; emc-cursor-state.el --- State saved for each fake cursor

;;; Commentary:

;; This file contains functions to interact with the state of a fake cursor

(require 'emc-common)

;;; Code:

(defun emc-get-cursor-property (cursor name)
  "Return the value of the property with NAME from CURSOR."
  (when cursor (emc-get-object-property cursor name)))

(defun emc-put-cursor-property (cursor &rest properties)
  "Return a new cursor that has one or more PROPERTIES
set to the specified values."
  (apply 'emc-put-object-property (cons cursor properties)))

(defun emc-get-cursor-overlay (cursor)
  "Get the overlay for CURSOR."
  (emc-get-cursor-property cursor :overlay))

(defun emc-put-cursor-overlay (cursor overlay)
  "Set the overlay for CURSOR to OVERLAY."
  (emc-put-cursor-property cursor :overlay overlay))

(defun emc-get-cursor-region (cursor)
  "Get the region for CURSOR."
  (emc-get-cursor-property cursor :region))

(defun emc-put-cursor-region (cursor region)
  "Set the region for CURSOR to REGION."
  (emc-put-cursor-property cursor :region region))

(defun emc-get-cursor-kill-ring (cursor)
  "Get the `kill-ring' for CURSOR."
  (emc-get-cursor-property cursor :kill-ring))

(defun emc-put-cursor-kill-ring (cursor kill-ring)
  "Set the `kill-ring' for CURSOR to KILL-RING."
  (emc-put-cursor-property cursor :kill-ring kill-ring))

(defun emc-get-cursor-kill-ring-yank-pointer (cursor)
  "Get the `kill-ring-yank-pointer' for CURSOR."
  (emc-get-cursor-property
   cursor :kill-ring-yank-pointer))

(defun emc-put-cursor-kill-ring-yank-pointer (cursor kill-ring-yank-pointer)
  "Set the `kill-ring-yank-pointer' for CURSOR to KILL-RING-YANK-POINTER."
  (emc-put-cursor-property
   cursor :kill-ring-yank-pointer kill-ring-yank-pointer))

(defun emc-get-cursor-column (cursor)
  "Get the column for CURSOR."
  (emc-get-cursor-property cursor :column))

(defun emc-put-cursor-column (cursor column)
  "Set the column for CURSOR to COLUMN."
  (emc-put-cursor-property cursor :column column))

(defun emc-get-cursor-evil-markers-alist (cursor)
  "Get the evil-markers-alist for CURSOR."
  (emc-get-cursor-property cursor :evil-markers-alist))

(defun emc-put-cursor-evil-markers-alist (cursor evil-markers-alist)
  "Set the evil-markers-alist for CURSOR to EVIL-MARKERS-ALIST."
  (emc-put-cursor-property cursor :evil-markers-alist evil-markers-alist))

(defun emc-get-cursor-evil-jump-list (cursor)
  "Get the evil-jump-list for CURSOR."
  (emc-get-cursor-property cursor :evil-jump-list))

(defun emc-put-cursor-evil-jump-list (cursor evil-jump-list)
  "Set the evil-jump-list for CURSOR to EVIL-JUMP-LIST."
  (emc-put-cursor-property cursor :evil-jump-list evil-jump-list))

(defun emc-get-cursor-mark-ring (cursor)
  "Get the mark-ring for CURSOR."
  (emc-get-cursor-property cursor :mark-ring))

(defun emc-put-cursor-mark-ring (cursor mark-ring)
  "Set the mark-ring for CURSOR to MARK-RING."
  (emc-put-cursor-property cursor :mark-ring mark-ring))

(defun emc-get-cursor-mark-active (cursor)
  "Get the mark-active for CURSOR."
  (emc-get-cursor-property cursor :mark-active))

(defun emc-put-cursor-mark-active (cursor mark-active)
  "Set the mark-active for CURSOR to MARK-active."
  (emc-put-cursor-property cursor :mark-active mark-active))

(defun emc-get-cursor-start (cursor)
  "Get the CURSOR overlay start."
  (when cursor
    (overlay-start (emc-get-cursor-overlay cursor))))

(defun emc-get-cursor-end (cursor)
  "Get the CURSOR overlay end."
  (when cursor
    (overlay-end (emc-get-cursor-overlay cursor))))

(defun emc-delete-cursor-overlay (cursor)
  "Deletes the overlay associated with CURSOR."
  (when cursor
    (let ((overlay (emc-get-cursor-overlay cursor)))
      (when overlay (delete-overlay overlay)))))

(provide 'emc-cursor-state)

;;; emc-cursor-state.el ends here