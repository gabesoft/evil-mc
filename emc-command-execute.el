;;; emc-command-execute.el --- Execute commands for every fake cursor

;;; Commentary:

;; This file contains functions for executing a command for every fake cursor

(require 'cl)
(require 'evil)
(require 'emc-common)
(require 'emc-vars)
(require 'emc-cursor-state)
(require 'emc-cursor-make)
(require 'emc-command-record)
(require 'emc-region)

;;; Code:

(defmacro emc-define-handler (command &rest body)
  (declare (indent defun)
           (debug (&define name
                           [&optional lambda-list]
                           [&optional stringp]
                           [&rest keyword sexp]
                           def-body)))
  (let (arg args doc doc-form key keys)
    (when (listp (car-safe body)) (setq args (pop body)))
    (when (> (length body) 1)
      (if (eq (car-safe (car-safe body)) 'format)
          (setq doc-form (pop body))
        (when (stringp (car-safe body))
          (setq doc (pop body)))))
    (while (keywordp (car-safe body))
      (setq key (pop body)
            arg (pop body))
      (unless nil (setq keys (plist-put keys key arg))))
    `(progn
       ,(when (and command body)
          `(defun ,command ,args
             ,@(when doc `(,doc))
             ,@body))
       ,(when (and command doc-form)
          `(put ',command 'function-documentation ,doc-form))
       (let ((func ',(if (and (null command) body)
                         `(lambda ,args
                            ,@body)
                       command)))
         (apply #'evil-set-command-properties func ',keys)
         func))))

(defun emc-get-command-handler (cmd state)
  "Get the handler function for CMD and evil STATE."
  ;; TODO add a default for visual and return acccording to state
  ;; or determine what to do if no default is found
  (let* ((handler-data (emc-get-object-property emc-known-commands cmd))
         (handler (emc-get-object-property handler-data state)))
    (or handler (emc-get-object-property handler-data :default))))

(emc-define-handler emc-execute-complete (cursor)
  "Execute a completion command."
  :cursor-state :complete
  (funcall (emc-get-command-name)))

(emc-define-handler emc-execute-hippie-expand (cursor)
  "Execute a completion command."
  :cursor-state :complete
  (hippie-expand 1))

(emc-define-handler emc-execute-find-char (cursor)
  "Execute a `evil-find-char' command."
  (evil-repeat-find-char (emc-get-command-keys-count)))

(emc-define-handler emc-execute-evil-snipe (cursor)
  "Execute a `evil-snipe' command"
  (evil-snipe-repeat (emc-get-command-keys-count)))

(emc-define-handler emc-execute-default (cursor)
  "Execute a generic command for CURSOR."
  (execute-kbd-macro (emc-get-command-keys-vector)))

(defun emc-execute-for (cursor)
  "Execute the current command for CURSOR."
  (let ((cmd (emc-get-command-name))
        (state (emc-get-command-state))
        (handler (emc-get-command-handler cmd state))
        (cursor-state (evil-get-command-property handler :cursor-state)))
    ;; TODO set up cursor state
    ;; get the default cursor state as well
    (ignore-errors
      (condition-case error
          (funcall handler cursor)
        (error (message "failed to execute %s with error %s"
                        cmd
                        (error-message-string error))
               cursor)))
    ;; TODO update cursor state
    ))


(defun emc-execute-for-all ()
  "Execute the current command, stored at `emc-command', for all fake cursors."
  (when (and (emc-has-command-p)
             (not (emc-running-command-p))
             (not (emc-frozen-p)))
    (when (emc-executing-debug-p)
      (message "execute %s for all cursors" (emc-get-command-name)))
    (let ((emc-running-command t) (cursor-list nil))
      (emc-remove-last-undo-marker)
      (evil-with-single-undo
        (save-excursion
          (dolist (cursor emc-cursor-list)
            (setq cursor-list (emc-insert-cursor-into-list
                               (emc-execute-for cursor)
                               cursor-list)))
          (setq emc-cursor-list cursor-list))))))

(defun emc-remove-last-undo-marker ()
  "Remove the last undo marker so that future commands
are undone in the same step as the current command."
  (let ((undo-list (if (eq buffer-undo-list t)
                       evil-temporary-undo
                     buffer-undo-list)))
    (unless (or (not undo-list) (car undo-list))
      (while (and undo-list (null (car undo-list)))
        (pop undo-list)))
    (if (eq buffer-undo-list t)
        (setq evil-temporary-undo undo-list)
      (setq buffer-undo-list undo-list))))

(provide 'emc-command-execute)

;;; emc-command-execute.el ends here