;;; emc-cursor-state.el --- State saved for each fake cursor

;;; Commentary:

;; This file contains functions to interact with the state of a fake cursor

(require 'emc-common)

;;; Code:

(defun emc-put-cursor-property (cursor &rest properties)
  "Return a new cursor that has one or more PROPERTIES
set to the specified values."
  (apply 'emc-put-object-property (cons cursor properties)))

(defun emc-get-cursor-property (cursor name)
  "Return the value of the property with NAME from CURSOR."
  (when cursor (emc-get-object-property cursor name)))

(defun emc-get-cursor-keys (cursor)
  "Get the property keys from CURSOR."
  (mapcar 'car cursor))

(defun emc-get-cursor-values (cursor)
  "Get the property values from CURSOR."
  (mapcar 'cdr cursor))

(defun emc-get-cursor-overlay (cursor)
  "Get the overlay for CURSOR."
  (emc-get-cursor-property cursor :overlay))

(defun emc-get-cursor-region (cursor)
  "Get the region for CURSOR."
  (emc-get-cursor-property cursor :region))

(defun emc-get-cursor-kill-ring (cursor)
  "Get the `kill-ring' for CURSOR."
  (emc-get-cursor-property cursor :kill-ring))

(defun emc-get-cursor-kill-ring-yank-pointer (cursor)
  "Get the `kill-ring-yank-pointer' for CURSOR."
  (emc-get-cursor-property cursor :kill-ring-yank-pointer))

(defun emc-put-cursor-overlay (cursor overlay)
  "Set the overlay for CURSOR to OVERLAY."
  (emc-put-cursor-property cursor :overlay overlay))

(defun emc-put-cursor-region (cursor region)
  "Set the region for CURSOR to REGION."
  (emc-put-cursor-property cursor :region region))

(defun emc-put-cursor-kill-ring (cursor kill-ring)
  "Set the `kill-ring' for CURSOR to KILL-RING."
  (emc-put-cursor-property cursor :kill-ring kill-ring))

(defun emc-put-cursor-kill-ring-yank-pointer (cursor kill-ring-yank-pointer)
  "Set the `kill-ring-yank-pointer' for CURSOR to KILL-RING-YANK-POINTER."
  (emc-put-cursor-property cursor :kill-ring-yank-pointer kill-ring-yank-pointer))

(provide 'emc-cursor-state)

;;; emc-cursor-state.el ends here