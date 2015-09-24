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
  "Define a COMMAND handler with BODY.

\(fn COMMAND BODY...)"
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

(defun emc-execute-hippie-expand ()
  "Execute a completion command."
  (hippie-expand 1))

(defun emc-execute-evil-find-char ()
  "Execute an `evil-find-char' command."
  (evil-repeat-find-char (emc-get-command-keys-count)))

(defun emc-execute-evil-snipe ()
  "Execute an `evil-snipe' command."
  (evil-snipe-repeat (emc-get-command-keys-count)))

;; TODO refactor all region methods below
(defun emc-execute-evil-commentary ()
  "Execute an `evil-commentary' command."
  (if region
      (let ((start (emc-get-region-start region)))
        (goto-char start)
        (evil-commentary start (emc-get-region-end region)))
    (emc-execute-macro)))

(defun emc-execute-evil-join ()
  "Execute an `evil-join' command."
  (if region
      (let ((start (emc-get-region-start region)))
        (goto-char start)
        (evil-join start (emc-get-region-end region)))
    (emc-execute-macro)))

(defun emc-execute-evil-surround-region ()
  "Execute an `evil-surround-region' command."
  (if region
      (let ((start (emc-get-region-start region)))
        (goto-char start)
        (evil-surround-region
         start
         (emc-get-region-end region)
         (emc-get-region-type region)
         (emc-get-command-last-input)))
    (emc-execute-macro)))

(defun emc-execute-change-case (cmd)
  "Execute an `evil-invert-char', `evil-invert-case' `evil-upcase' or `evil-downcase' command."
  (if region
      (let ((start (emc-get-region-start region)))
        (goto-char start)
        (funcall cmd
                 start
                 (emc-get-region-end region)
                 (emc-get-region-type region)))
    (emc-execute-macro)))

(defun emc-execute-evil-replace ()
  "Execute an `evil-replace' command."
  (if region
      (evil-replace (emc-get-region-start region)
                    (emc-get-region-end region)
                    (emc-get-region-type region)
                    (emc-get-command-last-input))
    (emc-execute-macro)))

(defun emc-execute-with-region-and-register (cmd)
  "Execute CMD with the current register and region."
  (if region
      (funcall cmd
               (emc-get-region-start region)
               (emc-get-region-end region)
               (emc-get-region-type region)
               evil-this-register)
    (emc-execute-macro t)))

(defun emc-execute-evil-change-line ()
  "Execute an `evil-change-line' comand."
  (if region
      (evil-delete-line
       (emc-get-region-start region)
       (emc-get-region-end region)
       (emc-get-region-type region)
       evil-this-register)
    (evil-delete-line (point) (1+ (point)))))

(defun emc-execute-evil-yank ()
  "Execute an `evil-yank' comand."
  (emc-execute-with-region-and-register 'evil-yank)
  (when region
    (goto-char (min (emc-get-region-mark region)
                    (emc-get-region-point region)))))

(defun emc-execute-evil-delete ()
  "Execute an `evil-delete' comand."
  (emc-execute-with-region-and-register 'evil-delete))

(defun emc-execute-evil-change ()
  "Execute an `evil-change' comand."
  (let ((point (point)))
    (evil-with-state normal
      (unless (and region (eq point (point-at-bol)))
        (evil-forward-char))
      (emc-execute-with-region-and-register 'evil-change))))

(defun emc-execute-evil-paste ()
  "Execute an `evil-paste-before' or `evil-paste-after' command."
  (cond ((null region)
         (funcall (emc-get-command-name)
                  (emc-get-command-keys-count)
                  evil-this-register))
        ((emc-char-region-p region)
         (let (new-kill-ring new-kill-ring-yank-pointer)
           (let ((kill-ring (copy-sequence kill-ring))
                 (kill-ring-yank-pointer nil))

             (emc-execute-evil-delete)
             (setq new-kill-ring kill-ring)
             (setq new-kill-ring-yank-pointer kill-ring-yank-pointer))

           ;; execute paste with the old key ring
           (evil-paste-before (emc-get-command-keys-count) evil-this-register)

           ;; update the kill ring with the overwritten text
           (setq kill-ring new-kill-ring)
           (setq kill-ring-yank-pointer new-kill-ring-yank-pointer)))
        ((emc-line-region-p region)
         (let ((text (substring-no-properties (current-kill 0 t)))
               (start (emc-get-region-start region))
               (end (emc-get-region-end region)))
           (unless (emc-ends-with-newline-p text)
             (evil-insert-newline-below))
           (evil-paste-after (emc-get-command-keys-count) evil-this-register)
           (save-excursion (evil-delete start end 'line))))))

(defun emc-execute-macro (&optional add-register)
  "Execute a generic command as a keyboard macro.
If ADD-REGISTER is not nil add the current `evil-this-register'
to the keys vector"
  (execute-kbd-macro
   (if add-register
       (emc-get-command-keys-vector-with-register)
     (emc-get-command-keys-vector))))

(defun emc-execute-evil-goto-line ()
  "Execute an `evil-goto-line' command."
  (let ((count (emc-get-command-property :keys-count)))
    (if count
        (evil-goto-line count)
      (evil-goto-line))))

(defun emc-execute-call ()
  "Execute a generic command as a function call without parameters."
  (funcall (emc-get-command-name)))

(defun emc-execute-call-with-last-input ()
  "Executed a generic command as a function call with the last input character."
  (funcall (emc-get-command-name) (emc-get-command-last-input)))

(defun emc-execute-call-with-count ()
  "Execute a generic command as a function call with count."
  (funcall (emc-get-command-name) (emc-get-command-keys-count)))

(defun emc-execute-move-to-line (dir)
  "Execute a move to line command in DIR."
  (let* ((keys-count (emc-get-command-keys-count))
         (count (ecase dir (next keys-count) (prev (- keys-count)))))
    (setq column (or column (emc-column-number (point))))
    (forward-line count)
    (goto-char (min (+ (point) column) (point-at-eol)))))

(defun emc-execute-not-supported ()
  "Throw an error for a not supported command."
  (evil-force-normal-state)
  (error ("%s is not supported" (emc-get-command-name))))

(defun emc-clear-current-region ()
  "Clears the current region."
  (setq region nil))

(defun emc-update-current-region ()
  "Update the current region."
  (setq region (emc-update-region region)))

(defun emc-execute-visual-region (type)
  "Execute an `evil-visual-char' or `evil-visual-line'
command according to TYPE."
  (cond ((or (null region)
             (eq (emc-get-region-type region) type))
         (setq region (emc-create-region (point) (point) type)))
        (t
         (setq region (emc-change-region-type region type)))))

(defun emc-get-command-keys-vector-with-register ()
  "Return the keys-vector of current command prepended
by the value of `evil-this-register'."
  (if evil-this-register
      (vconcat [?\"]
               (vector evil-this-register)
               (emc-get-command-keys-vector))
    (emc-get-command-keys-vector)))


;; handlers for normal state

(emc-define-handler emc-execute-normal-complete ()
  :cursor-clear (region column)
  :cursor-state :complete
  (emc-execute-call))

(emc-define-handler emc-execute-normal-hippie-expand ()
  :cursor-clear (region column)
  :cursor-state :complete
  (hippie-expand 1))

(emc-define-handler emc-execute-normal-evil-find-char ()
  :cursor-clear (region column)
  (emc-execute-evil-find-char))

(emc-define-handler emc-execute-normal-evil-snipe ()
  :cursor-clear (region column)
  (emc-execute-evil-snipe))

(emc-define-handler emc-execute-normal-evil-commentary ()
  :cursor-clear (region column)
  (emc-execute-evil-commentary))

(emc-define-handler emc-execute-normal-evil-join ()
  :cursor-clear (region column)
  (emc-execute-evil-join))

(emc-define-handler emc-execute-normal-evil-surround-region ()
  :cursor-clear (region column)
  (emc-execute-evil-surround-region))

(emc-define-handler emc-execute-normal-evil-replace ()
  :cursor-clear (region column)
  (emc-execute-evil-replace))

(emc-define-handler emc-execute-normal-evil-delete ()
  :cursor-clear (region column)
  (emc-execute-evil-delete))

(emc-define-handler emc-execute-normal-evil-yank ()
  :cursor-clear (region column)
  (emc-execute-evil-yank))

(emc-define-handler emc-execute-normal-evil-change ()
  :cursor-clear (region column)
  (emc-execute-evil-change))

(emc-define-handler emc-execute-normal-evil-paste ()
  :cursor-clear (region column)
  (emc-execute-evil-paste))

(emc-define-handler emc-execute-normal-evil-invert-char ()
  :cursor-clear (region column)
  (emc-execute-change-case 'evil-invert-char))

(emc-define-handler emc-execute-normal-evil-invert-case ()
  :cursor-clear (region column)
  (emc-execute-change-case 'evil-invert-case))

(emc-define-handler emc-execute-normal-evil-upcase ()
  :cursor-clear (region column)
  (emc-execute-change-case 'evil-upcase))

(emc-define-handler emc-execute-normal-evil-downcase ()
  :cursor-clear (region column)
  (emc-execute-change-case 'evil-downcase))

(emc-define-handler emc-execute-normal-evil-delete-char ()
  :cursor-clear (region column)
  (emc-execute-with-region-and-register 'evil-delete-char))

(emc-define-handler emc-execute-normal-evil-delete-line ()
  :cursor-clear (region column)
  (emc-execute-with-region-and-register 'evil-delete-line))

(emc-define-handler emc-execute-normal-evil-change-line ()
  :cursor-clear (region column)
  (emc-execute-evil-change-line))

(emc-define-handler emc-execute-normal-evil-goto-line ()
  :cursor-clear (region column)
  (emc-execute-evil-goto-line))

(emc-define-handler emc-execute-normal-next-line ()
  :cursor-clear region
  (emc-execute-move-to-line 'next))

(emc-define-handler emc-execute-normal-prev-line ()
  :cursor-clear region
  (emc-execute-move-to-line 'prev))

(emc-define-handler emc-execute-normal-force-normal-state ()
  :cursor-clear region
  (evil-force-normal-state))

(emc-define-handler emc-execute-normal-evil-normal-state ()
  :cursor-clear region
  (evil-insert 1)
  (evil-normal-state))

(emc-define-handler emc-execute-normal-macro ()
  :cursor-clear (region column)
  (emc-execute-macro))

(emc-define-handler emc-execute-normal-call ()
  :cursor-clear (region column)
  (emc-execute-call))

(emc-define-handler emc-execute-normal-call-with-last-input ()
  :cursor-clear (region column)
  (emc-execute-call-with-last-input))

(emc-define-handler emc-execute-normal-call-with-count ()
  :cursor-clear (region column)
  (emc-execute-call-with-count))

(emc-define-handler emc-execute-normal-keyboard-quit ()
  :cursor-clear (region column)
  (ignore))

(emc-define-handler emc-execute-normal-not-supported ()
  :cursor-clear (region column)
  (emc-execute-not-supported))

;; handlers for visual state

(emc-define-handler emc-execute-visual-line ()
  (emc-execute-visual-region 'line))

(emc-define-handler emc-execute-visual-char ()
  (emc-execute-visual-region 'char))

(emc-define-handler emc-execute-visual-text-object ()
  (let* ((limits (funcall (emc-get-command-name)))
         (start (nth 0 limits))
         (end (1- (nth 1 limits))))
    (goto-char end)
    (setq region (emc-create-region start end 'char))))

(emc-define-handler emc-execute-exchange-point-and-mark ()
  (let* ((next-region (emc-exchange-region-point-and-mark region))
         (mark (emc-get-region-mark next-region))
         (point (emc-get-region-point next-region)))
    (goto-char (if (< mark point) (1- point) point))
    (setq region next-region)))

(emc-define-handler emc-execute-visual-evil-find-char ()
  (emc-execute-evil-find-char)
  (emc-update-current-region))

(emc-define-handler emc-execute-visual-evil-snipe ()
  (emc-execute-evil-snipe)
  (emc-update-current-region))

(emc-define-handler emc-execute-visual-evil-goto-line ()
  (emc-execute-evil-goto-line)
  (emc-update-current-region))

(emc-define-handler emc-execute-visual-next-line ()
  (emc-execute-move-to-line 'next)
  (emc-update-current-region))

(emc-define-handler emc-execute-visual-prev-line ()
  (emc-execute-move-to-line 'prev)
  (emc-update-current-region))

(emc-define-handler emc-execute-visual-macro ()
  (emc-execute-macro)
  (emc-update-current-region))

(emc-define-handler emc-execute-visual-call-with-last-input ()
  (emc-execute-call-with-last-input)
  (emc-update-current-region))

(emc-define-handler emc-execute-visual-call ()
  (emc-execute-call)
  (emc-update-current-region))

(emc-define-handler emc-execute-visual-call-count ()
  (emc-execute-call-with-count)
  (emc-update-current-region))

;; ----

(defun emc-get-command-handler (cmd state)
  "Get the handler function for CMD and evil STATE."
  (let* ((handler-data (emc-get-object-property emc-known-commands cmd))
         (handler (emc-get-object-property handler-data state)))
    (or handler
        (emc-get-object-property handler-data :default)
        (cond ((eq (evil-get-command-property cmd :repeat) 'motion)
               (cond ((eq state :visual) 'emc-execute-visual-call-count)
                     (t 'emc-execute-normal-call-with-count)))))))

(defun emc-get-state-variables (handler)
  "Get all cursor variables required to hold state for HANDLER."
  (let ((names (evil-get-command-property handler :cursor-state)))
    (when (atom names)
      (setq names (list names)))
    (when (not (memq :default names))
      (push :default names))
    (apply 'append (mapcar (lambda (name)
                             (emc-get-object-property emc-cursor-state name))
                           names))))

(defun emc-get-clear-variables (handler)
  "Get all cursor variables that should be cleared after HANDLER."
  (let ((names (evil-get-command-property handler :cursor-clear)))
    (if (atom names) (list names) names)))

(defun emc-get-var-name-value (var)
  "Gets the current name and value pair of VAR or nil if it needs to be cleared."
  (list var (unless (memq var clear-variables) (symbol-value var))))

(defun emc-execute-for (cursor state-variables clear-variables)
  "Execute the current command for CURSOR in the context of STATE-VARIABLES and
ensuring to set CLEAR-VARIABLES to nil after the execution is complete."
  (when (emc-executing-debug-p)
    (message "Execute %s with %s" (emc-get-command-name) handler))
  (ignore-errors
    (condition-case error
        (cl-progv
            state-variables
            (emc-get-cursor-properties cursor state-variables)

          (goto-char (emc-get-cursor-start cursor))
          (funcall handler)
          (emc-delete-cursor-overlay cursor)
          (emc-delete-region-overlay (emc-get-cursor-region cursor))

          ;; TODO determine why the repeat ring is not populated
          ;; (message "REPEAT RING %s" evil-repeat-ring)

          (apply 'emc-put-cursor-property
                 (emc-put-cursor-overlay cursor (emc-cursor-overlay-at-pos))
                 (mapcan 'emc-get-var-name-value state-variables)))
      (error (message "Failed to execute %s with error %s"
                      (emc-get-command-name)
                      (error-message-string error))
             cursor))))

(defun emc-execute-for-all ()
  "Execute the current command, stored at `emc-command', for all fake cursors."
  (when (and (emc-has-command-p)
             (not (emc-executing-command-p))
             (not (emc-frozen-p)))
    (when (emc-executing-debug-p)
      (message "Execute %s for all cursors" (emc-get-command-name)))
    (let* ((emc-executing-command t)
           (cursor-list nil)
           (handler (emc-get-command-handler
                     (emc-get-command-name)
                     (emc-get-command-state)))
           (state-variables (emc-get-state-variables handler))
           (clear-variables (emc-get-clear-variables handler)))
      (unless handler
        (message "No handler found for command %s" (emc-get-command-name)))
      (when handler
        ;; (evil-repeat-stop)
        (emc-remove-last-undo-marker)
        (evil-with-single-undo
          (save-excursion
            (dolist (cursor emc-cursor-list)
              ;; (evil-repeat-start)
              (setq cursor-list (emc-insert-cursor-into-list
                                 (emc-execute-for cursor
                                                  state-variables
                                                  clear-variables)
                                 cursor-list))
              ;; (evil-repeat-stop)
              )
            (setq emc-cursor-list cursor-list)))))))

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

(when (fboundp 'font-lock-add-keywords)
  (font-lock-add-keywords
   'emacs-lisp-mode
   '(("(\\(emc-define-handler\\)" . font-lock-keyword-face))))

(provide 'emc-command-execute)

;;; emc-command-execute.el ends here
