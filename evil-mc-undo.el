;;; evil-mc-undo.el --- Undo related functions

;;; Commentary:

;; This file contains functions related to undo/redo functionality when there are
;; multiple cursors active.

;;; Code:

(require 'cl-lib)
(require 'evil-mc-common)
(require 'evil-mc-vars)

(defmacro evil-mc-with-single-undo (&rest body)
  "Execute BODY as a single undo step combined with the current command step."
  (declare (indent defun)
           (debug t))
  `(unwind-protect
       (let (buffer-undo-list)
         (prog1
             (evil-with-single-undo ,@body)
           (setq evil-mc-temporary-undo buffer-undo-list)))
     (unless (eq buffer-undo-list t)
       (let ((has-undo-boundary (evil-mc-ensure-one-undo-step)))
         (setq buffer-undo-list
               (if (cdr evil-mc-temporary-undo)
                   (nconc evil-mc-temporary-undo buffer-undo-list)
                 buffer-undo-list)
               evil-mc-temporary-undo nil)
         (evil-mc-remove-last-undo-boundary)
         (when has-undo-boundary
           (undo-boundary))))
     (setq evil-undo-list-pointer nil)))

(defun evil-mc-has-undo-boundary-p ()
  "Return true if the `buffer-undo-list' ends with an undo boundary."
  (and buffer-undo-list
       (not (eq buffer-undo-list t))
       (null (car-safe buffer-undo-list))))

(defun evil-mc-ensure-one-undo-step ()
  "Combine `buffer-undo-list' entries for the current command to
make up only one undo step."
  (let ((has-undo-boundary (evil-mc-has-undo-boundary-p))
        (evil-undo-list-pointer (or (evil-mc-get-command-undo-list-pointer-pre)
                                    (last buffer-undo-list))))
    (evil-refresh-undo-step)
    has-undo-boundary))

(defun evil-mc-remove-last-undo-boundary ()
  "Remove the last undo marker so that future commands
are undone in the same step as the current command."
  (when (evil-mc-has-undo-boundary-p)
    (pop buffer-undo-list)))

(provide 'evil-mc-undo)

;;; evil-mc-undo.el ends here