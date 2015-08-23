;;; emc-common.el --- Common functions

;;; Commentary:

;; This file contains common functionality

;;; Code:

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

(provide 'emc-common)

;;; emc-common.el ends here