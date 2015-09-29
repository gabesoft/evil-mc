;;; emc-common.el --- Common functions

;;; Commentary:

;; This file contains common functionality

;;; Code:

;; TODO replace these with
;; evil-put-property evil-get-property
(defun emc-get-object-property (obj prop)
  "Get the value of PROP from OBJ."
  (let ((item (assq prop obj)))
    (when item (cdr item))))

(defun emc-put-object-property (obj prop val &rest properties)
  "Return a new OBJ that has PROP set to VAL and any other PROPERTIES specified."
  (let ((obj (assq-delete-all prop obj)))
    (setq obj (cons (cons prop val) obj))
    (while properties
      (setq obj (emc-put-object-property obj
                                         (pop properties)
                                         (pop properties))))
    obj))

(defun emc-put-object-properties (obj &rest properties)
  "Return a new OBJ that has all the PROPERTIES specified."
  (when properties
    (apply #'emc-put-object-property
           (cons obj properties))))


(defun emc-column-number (pos)
  "Return the column number at POS."
  (save-excursion
    (goto-char pos)
    (current-column)))

(provide 'evil-mc-common)

;;; emc-common.el ends here