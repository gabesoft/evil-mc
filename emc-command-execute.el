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
  (let* ((handler-data (emc-get-object-property emc-known-commands cmd))
         (handler (emc-get-object-property handler-data state)))
    (or handler
        (emc-get-object-property handler-data :default)
        (cond ((eq (evil-get-command-property cmd :repeat) 'motion)
               (cond ((eq state :visual) 'emc-execute-visual-call-count)
                     (t 'emc-execute-call-count)))))))

;; normal state

(emc-define-handler emc-execute-use-register (cursor)
  "Execute an `evil-use-register' command for CURSOR."
  (evil-use-register (emc-get-command-last-input))
  (emc-clear-current-region))

(emc-define-handler emc-execute-complete (cursor)
  "Execute a completion command."
  :cursor-state :complete
  (funcall (emc-get-command-name))
  (emc-clear-current-region))

(emc-define-handler emc-execute-hippie-expand (cursor)
  "Execute a completion command."
  :cursor-state :complete
  (hippie-expand 1)
  (emc-clear-current-region))

(emc-define-handler emc-execute-find-char (cursor)
  "Execute a `evil-find-char' command."
  (evil-repeat-find-char (emc-get-command-keys-count))
  (emc-clear-current-region))

(emc-define-handler emc-execute-evil-snipe (cursor)
  "Execute a `evil-snipe' command."
  (evil-snipe-repeat (emc-get-command-keys-count))
  (emc-clear-current-region))

;; NOTE use as last resort due to performance
(emc-define-handler emc-execute-macro (cursor)
  "Execute a generic command for CURSOR as a keyboard macro."
  (execute-kbd-macro (emc-get-command-keys-vector))
  (emc-clear-current-region))

(emc-define-handler emc-execute-call (cursor)
  "Execute a generic command for CURSOR as a function call without parameters."
  (funcall (emc-get-command-name))
  (emc-clear-current-region))

(emc-define-handler emc-execute-call-count (cursor)
  "Execute a generic command for CURSOR as a function call with count."
  (funcall (emc-get-command-name) (emc-get-command-keys-count))
  (emc-clear-current-region))

(emc-define-handler emc-execute-not-supported (cursor)
  "Throw an error for a not supported command for CURSOR."
  (error ("%s is not supported" (emc-get-command-name)))
  (emc-clear-current-region))

(defun emc-clear-current-region ()
  "Clears the current region."
  (setq region nil))

;; TODO look at all evil motions (evil-define-motion)
;; distinguish between the ones that don't take a count

;; visual state

(emc-define-handler emc-execute-visual-line (cursor)
  "Execute an `evil-visual-line' command for CURSOR."
  (emc-execute-visual-region cursor 'line))

(emc-define-handler emc-execute-visual-char (cursor)
  "Execute an `evil-visual-char' command for CURSOR."
  (emc-execute-visual-region cursor 'char))

(defun emc-execute-visual-region (cursor type)
  "Execute an `evil-visual-char' or `evil-visual-line'
command for CURSOR according to TYPE."
  (cond ((or (null region) (eq region-type type))
         (setq region (emc-create-region (point) (point) type)))
        (t
         (setq region (emc-change-region-type region type)))))

(emc-define-handler emc-execute-visual-text-object (cursor)
  "Execute an text object command in visual state for CURSOR."
  (let* ((limits (funcall cmd))
         (start (nth 0 limits))
         (end (1- (nth 1 limits))))
    (goto-char end)
    (setq region (emc-create-region start end 'char))))

(emc-define-handler emc-execute-visual-use-register (cursor)
  "Execute an `evil-use-register' command in visual state for CURSOR."
  ;; TODO cannot reuse the normal state handlers here as
  ;; they clear the current region
  ;; extract into common methods
  (emc-execute-use-register cursor)
  (emc-update-current-region))

(emc-define-handler emc-execute-exchange-point-and-mark (cursor)
  "Execute ann exchange point and mark command for CURSOR."
  (let* ((next-region (emc-exchange-region-point-and-mark region))
         (mark (emc-get-region-mark next-region))
         (point (emc-get-region-point next-region)))
    (goto-char (if (< mark point) (1- point) point))
    (setq region next-region)))

(emc-define-handler emc-execute-visual-find-char (cursor)
  "Execute an `evil-find-char' command when in visual state for CURSOR."
  (emc-execute-find-char cursor)
  (emc-update-current-region))

(emc-define-handler emc-execute-visual-evil-snipe (cursor)
  "Execute an `evil-snipe' command in visual state for CURSOR."
  (emc-execute-evil-snipe cursor)
  (emc-update-current-region))

(emc-define-handler emc-execute-visual-macro (cursor)
  "Execute a generic command as a keyboard macro for CURSOR in visual state."
  (emc-execute-macro cursor)
  (emc-update-current-region))

(emc-define-handler emc-execute-visual-call (cursor)
  "Execute a generic command as a function call for CURSOR in visual state."
  (emc-execute-call cursor)
  (emc-update-current-region))

(emc-define-handler emc-execute-visual-call-count (cursor)
  "Execute a generic command as a function call with count for CURSOR in visual state."
  (emc-execute-call-count cursor)
  (emc-update-current-region))

(defun emc-update-current-region ()
  "Updates the current region."
  (setq region (emc-update-region region)))

;; generic

(defun emc-execute-for (cursor)
  "Execute the current command for CURSOR."
  (let ((cmd (emc-get-command-name))
        (state (emc-get-command-state))
        (handler (emc-get-command-handler cmd state))
        (cursor-state (evil-get-command-property handler :cursor-state)))
    ;; TODO set up cursor state
    ;; get the default cursor state as well
    (if handler
        (ignore-errors
          (condition-case error
              (funcall handler cursor)
            (error (message "Failed to execute %s with error %s"
                            cmd
                            (error-message-string error))
                   cursor)))
      (message "No handler found for command %s" cmd)
      cursor)

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
