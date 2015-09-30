;;; evil-mc-common.el --- Common functions

;;; Commentary:

;; This file contains common functionality

;;; Code:

;; TODO replace these with
;; evil-put-property evil-get-property
(defun evil-mc-get-object-property (obj prop)
  "Get the value of PROP from OBJ."
  (let ((item (assq prop obj)))
    (when item (cdr item))))

(defun evil-mc-put-object-property (obj prop val &rest properties)
  "Return a new OBJ that has PROP set to VAL and any other PROPERTIES specified."
  (let ((obj (assq-delete-all prop obj)))
    (setq obj (cons (cons prop val) obj))
    (while properties
      (setq obj (evil-mc-put-object-property obj
                                         (pop properties)
                                         (pop properties))))
    obj))

(defun evil-mc-put-object-properties (obj &rest properties)
  "Return a new OBJ that has all the PROPERTIES specified."
  (when properties
    (apply #'evil-mc-put-object-property
           (cons obj properties))))


(defun evil-mc-column-number (pos)
  "Return the column number at POS."
  (save-excursion
    (goto-char pos)
    (current-column)))

(provide 'evil-mc-common)

;;; evil-mc-common.el ends here