;;; evil-mc-command-execute.el --- Execute commands for every fake cursor

;;; Commentary:

;; This file contains functions for executing a command for every fake cursor

(require 'cl)
(require 'evil)
(require 'evil-mc-common)
(require 'evil-mc-vars)
(require 'evil-mc-cursor-state)
(require 'evil-mc-cursor-make)
(require 'evil-mc-command-record)
(require 'evil-mc-region)

;;; Code:

(defmacro evil-mc-define-handler (command &rest body)
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

(defmacro evil-mc-define-visual-handler (command &rest body)
  "Define a visual COMMAND handler with BODY that updates the
current region after executing BODY.

  \(fn COMMAND BODY ...)"
  (declare (indent 2) (debug t))
  `(evil-mc-define-handler ,command ()
     (ignore-errors ,@body)
     (evil-mc-update-current-region)))

(defmacro evil-mc-with-region (region form &rest body)
  "Execute FORM if there is a REGION. Otherwise execute BODY."
  (declare (indent 2) (debug t))
  `(if ,region
       (let ((region-start  (evil-mc-get-region-start ,region))
             (region-end  (evil-mc-get-region-end ,region))
             (region-type  (evil-mc-get-region-type ,region)))
         ,form)
     ,@body))

(defmacro evil-mc-with-region-or-execute-macro (region add-register &rest body)
  "Execute BODY if there is a REGION.
Otherwise, run `evil-mc-execute-macro' with ADD-REGISTER."
  (declare (indent 2) (debug t))
  `(evil-mc-with-region ,region
       (progn ,@body)
     (evil-mc-execute-macro ,add-register)))

(defun evil-mc-execute-hippie-expand ()
  "Execute a completion command."
  (hippie-expand 1))

(defun evil-mc-execute-evil-find-char ()
  "Execute an `evil-find-char' command."
  (evil-repeat-find-char (evil-mc-get-command-keys-count)))

(defun evil-mc-execute-evil-snipe ()
  "Execute an `evil-snipe' command."
  (evil-snipe-repeat (evil-mc-get-command-keys-count)))

(defun evil-mc-execute-evil-commentary ()
  "Execute an `evil-commentary' command."
  (evil-mc-with-region-or-execute-macro region nil
    (when (eq region-type 'char) (goto-char region-start))
    (evil-commentary region-start region-end)))

(defun evil-mc-execute-evil-join ()
  "Execute an `evil-join' command."
  (evil-mc-with-region-or-execute-macro region nil
    (goto-char region-start)
    (evil-join region-start region-end)))

(defun evil-mc-execute-evil-shift (cmd)
  "Execute an `evil-shift-left' or `evil-shift-right'."
  (evil-mc-with-region-or-execute-macro region nil
    (funcall cmd
             region-start
             region-end
             (evil-mc-get-command-keys-count))))

(defun evil-mc-execute-evil-surround-region ()
  "Execute an `evil-surround-region' command."
  (evil-mc-with-region-or-execute-macro region nil
    (goto-char region-start)
    (evil-surround-region region-start
                          region-end
                          region-type
                          (evil-mc-get-command-last-input))))

(defun evil-mc-execute-change-case (cmd)
  "Execute an `evil-invert-char', `evil-invert-case' `evil-upcase'
or `evil-downcase' command."
  (evil-mc-with-region-or-execute-macro region nil
    (goto-char region-start)
    (funcall cmd region-start region-end region-type)))

(defun evil-mc-execute-evil-replace ()
  "Execute an `evil-replace' command."
  (evil-mc-with-region-or-execute-macro region nil
    (evil-replace region-start
                  region-end
                  region-type
                  (evil-mc-get-command-last-input))))

(defun evil-mc-execute-evil-exchange ()
  "Execute an `evil-exchange' command."
  (evil-mc-with-region-or-execute-macro region nil
    (goto-char region-start)
    (evil-exchange region-start region-end region-type)))

(defun evil-mc-execute-with-region-and-register (cmd)
  "Execute CMD with the current register and region."
  (evil-mc-with-region-or-execute-macro region t
    (funcall cmd region-start region-end region-type evil-this-register)))

(defun evil-mc-execute-evil-change-line ()
  "Execute an `evil-change-line' comand."
  (evil-mc-with-region region
      (evil-delete-line region-start
                        region-end
                        region-type
                        evil-this-register)
    (evil-delete-line (point) (1+ (point)))))

(defun evil-mc-execute-evil-yank ()
  "Execute an `evil-yank' comand."
  (evil-mc-with-region-or-execute-macro region t
    (evil-yank region-start region-end region-type evil-this-register)
    (goto-char (min (evil-mc-get-region-mark region)
                    (evil-mc-get-region-point region)))))

(defun evil-mc-execute-evil-delete ()
  "Execute an `evil-delete' comand."
  (evil-mc-execute-with-region-and-register 'evil-delete))

(defun evil-mc-execute-evil-change ()
  "Execute an `evil-change' comand."
  (let ((point (point)))
    (evil-with-state normal
      (unless (and region (eq point (point-at-bol)))
        (evil-forward-char))
      (evil-mc-execute-with-region-and-register 'evil-change))))

(defun evil-mc-execute-evil-paste ()
  "Execute an `evil-paste-before' or `evil-paste-after' command."
  (cond ((null region)
         (funcall (evil-mc-get-command-name)
                  (evil-mc-get-command-keys-count)
                  evil-this-register))
        ((evil-mc-char-region-p region)
         (let (new-kill-ring new-kill-ring-yank-pointer)
           (let ((kill-ring (copy-tree kill-ring))
                 (kill-ring-yank-pointer nil))

             (evil-mc-execute-evil-delete)
             (setq new-kill-ring kill-ring)
             (setq new-kill-ring-yank-pointer kill-ring-yank-pointer))

           ;; execute paste with the old key ring
           (evil-paste-before (evil-mc-get-command-keys-count) evil-this-register)

           ;; update the kill ring with the overwritten text
           (setq kill-ring new-kill-ring)
           (setq kill-ring-yank-pointer new-kill-ring-yank-pointer)))
        ((evil-mc-line-region-p region)
         (let ((text (substring-no-properties (current-kill 0 t)))
               (start (evil-mc-get-region-start region))
               (end (evil-mc-get-region-end region)))
           (unless (evil-mc-ends-with-newline-p text)
             (evil-insert-newline-below))
           (evil-paste-after (evil-mc-get-command-keys-count) evil-this-register)
           (save-excursion (evil-delete start end 'line))))))

(defun evil-mc-execute-macro (&optional add-register)
  "Execute a generic command as a keyboard macro.
If ADD-REGISTER is not nil add the current `evil-this-register'
to the keys vector"
  (execute-kbd-macro
   (if add-register
       (evil-mc-get-command-keys-vector-with-register)
     (evil-mc-get-command-keys-vector))))

(defun evil-mc-execute-evil-goto-line ()
  "Execute an `evil-goto-line' command."
  (let ((count (evil-mc-get-command-property :keys-count)))
    (if count
        (evil-goto-line count)
      (evil-goto-line))))

(defun evil-mc-execute-call ()
  "Execute a generic command as a function call without parameters."
  (funcall (evil-mc-get-command-name)))

(defun evil-mc-execute-call-with-last-input ()
  "Executed a generic command as a function call with the last input character."
  (funcall (evil-mc-get-command-name) (evil-mc-get-command-last-input)))

(defun evil-mc-execute-call-with-count ()
  "Execute a generic command as a function call with count."
  (funcall (evil-mc-get-command-name) (evil-mc-get-command-keys-count)))

(defun evil-mc-execute-move-to-line (dir)
  "Execute a move to line command in DIR."
  (let* ((keys-count (evil-mc-get-command-keys-count))
         (count (ecase dir (next keys-count) (prev (- keys-count)))))
    (setq column (or column (evil-mc-column-number (point))))
    (forward-line count)
    (goto-char (min (+ (point) column) (point-at-eol)))))

(defun evil-mc-execute-not-supported ()
  "Throw an error for a not supported command."
  (evil-force-normal-state)
  (error (message "%s is not supported" (evil-mc-get-command-name))))

(defun evil-mc-clear-current-region ()
  "Clears the current region."
  (setq region nil))

(defun evil-mc-update-current-region ()
  "Update the current region."
  (setq region (evil-mc-update-region region)))

(defun evil-mc-execute-visual-region (type)
  "Execute an `evil-visual-char' or `evil-visual-line'
command according to TYPE."
  (cond ((or (null region)
             (eq (evil-mc-get-region-type region) type))
         (setq region (evil-mc-create-region (point) (point) type)))
        (t
         (setq region (evil-mc-change-region-type region type)))))

(defun evil-mc-get-command-keys-vector-with-register ()
  "Return the keys-vector of current command prepended
by the value of `evil-this-register'."
  (if evil-this-register
      (vconcat [?\"]
               (vector evil-this-register)
               (evil-mc-get-command-keys-vector))
    (evil-mc-get-command-keys-vector)))


;; default handlers

(evil-mc-define-handler evil-mc-execute-default-complete ()
  :cursor-clear (region column)
  :cursor-state :dabbrev
  (evil-mc-execute-call))

(evil-mc-define-handler evil-mc-execute-default-hippie-expand ()
  :cursor-clear (region column)
  :cursor-state :dabbrev
  (hippie-expand 1))

(evil-mc-define-handler evil-mc-execute-default-evil-find-char ()
  :cursor-clear (region column)
  (evil-mc-execute-evil-find-char))

(evil-mc-define-handler evil-mc-execute-default-evil-snipe ()
  :cursor-clear (region column)
  (evil-mc-execute-evil-snipe))

(evil-mc-define-handler evil-mc-execute-default-evil-commentary ()
  :cursor-clear (region column)
  (evil-mc-execute-evil-commentary))

(evil-mc-define-handler evil-mc-execute-default-evil-join ()
  :cursor-clear (region column)
  (evil-mc-execute-evil-join))

(evil-mc-define-handler evil-mc-execute-default-evil-shift-left ()
  :cursor-clear (region column)
  (evil-mc-execute-evil-shift 'evil-shift-left))

(evil-mc-define-handler evil-mc-execute-default-evil-shift-right ()
  :cursor-clear (region column)
  (evil-mc-execute-evil-shift 'evil-shift-right))

(evil-mc-define-handler evil-mc-execute-default-evil-surround-region ()
  :cursor-clear (region column)
  (evil-mc-execute-evil-surround-region))

(evil-mc-define-handler evil-mc-execute-default-evil-replace ()
  :cursor-clear (region column)
  (evil-mc-execute-evil-replace))

(evil-mc-define-handler evil-mc-execute-default-evil-exchange ()
  :cursor-clear (region column)
  (evil-mc-execute-evil-exchange))

(evil-mc-define-handler evil-mc-execute-default-evil-delete ()
  :cursor-clear (region column)
  (evil-mc-execute-evil-delete))

(evil-mc-define-handler evil-mc-execute-default-evil-yank ()
  :cursor-clear (region column)
  (evil-mc-execute-evil-yank))

(evil-mc-define-handler evil-mc-execute-default-evil-change ()
  :cursor-clear (region column)
  (evil-mc-execute-evil-change))

(evil-mc-define-handler evil-mc-execute-default-evil-paste ()
  :cursor-clear (region column)
  (evil-mc-execute-evil-paste))

(evil-mc-define-handler evil-mc-execute-default-evil-invert-char ()
  :cursor-clear (region column)
  (evil-mc-execute-change-case 'evil-invert-char))

(evil-mc-define-handler evil-mc-execute-default-evil-invert-case ()
  :cursor-clear (region column)
  (evil-mc-execute-change-case 'evil-invert-case))

(evil-mc-define-handler evil-mc-execute-default-evil-upcase ()
  :cursor-clear (region column)
  (evil-mc-execute-change-case 'evil-upcase))

(evil-mc-define-handler evil-mc-execute-default-evil-downcase ()
  :cursor-clear (region column)
  (evil-mc-execute-change-case 'evil-downcase))

(evil-mc-define-handler evil-mc-execute-default-evil-delete-char ()
  :cursor-clear (region column)
  (evil-mc-execute-with-region-and-register 'evil-delete-char))

(evil-mc-define-handler evil-mc-execute-default-evil-delete-line ()
  :cursor-clear (region column)
  (evil-mc-execute-with-region-and-register 'evil-delete-line))

(evil-mc-define-handler evil-mc-execute-default-evil-change-line ()
  :cursor-clear (region column)
  (evil-mc-execute-evil-change-line))

(evil-mc-define-handler evil-mc-execute-default-evil-goto-line ()
  :cursor-clear (region column)
  (evil-mc-execute-evil-goto-line))

(evil-mc-define-handler evil-mc-execute-default-next-line ()
  :cursor-clear region
  (evil-mc-execute-move-to-line 'next))

(evil-mc-define-handler evil-mc-execute-default-prev-line ()
  :cursor-clear region
  (evil-mc-execute-move-to-line 'prev))

(evil-mc-define-handler evil-mc-execute-default-force-normal-state ()
  :cursor-clear region
  (evil-force-normal-state))

(evil-mc-define-handler evil-mc-execute-default-evil-normal-state ()
  :cursor-clear region
  (evil-insert 1)
  (evil-normal-state))

(evil-mc-define-handler evil-mc-execute-default-macro ()
  :cursor-clear (region column)
  (evil-mc-execute-macro))

(evil-mc-define-handler evil-mc-execute-default-call ()
  :cursor-clear (region column)
  (evil-mc-execute-call))

(evil-mc-define-handler evil-mc-execute-default-call-with-last-input ()
  :cursor-clear (region column)
  (evil-mc-execute-call-with-last-input))

(evil-mc-define-handler evil-mc-execute-default-call-with-count ()
  :cursor-clear (region column)
  (evil-mc-execute-call-with-count))

(evil-mc-define-handler evil-mc-execute-default-ignore ()
  :cursor-clear (region column)
  (ignore))

(evil-mc-define-handler evil-mc-execute-default-not-supported ()
  :cursor-clear (region column)
  (evil-mc-execute-not-supported))

;; handlers for visual state

(evil-mc-define-handler evil-mc-execute-visual-line ()
  (evil-mc-execute-visual-region 'line))

(evil-mc-define-handler evil-mc-execute-visual-char ()
  (evil-mc-execute-visual-region 'char))

(evil-mc-define-handler evil-mc-execute-visual-text-object ()
  (let* ((limits (funcall (evil-mc-get-command-name)))
         (start (nth 0 limits))
         (end (1- (nth 1 limits))))
    (goto-char end)
    (setq region (evil-mc-create-region start end 'char))))

(evil-mc-define-handler evil-mc-execute-visual-exchange-point-and-mark ()
  (let* ((next-region (evil-mc-exchange-region-point-and-mark region))
         (mark (evil-mc-get-region-mark next-region))
         (point (evil-mc-get-region-point next-region)))
    (goto-char (if (< mark point) (1- point) point))
    (setq region next-region)))

(evil-mc-define-visual-handler evil-mc-execute-visual-evil-find-char ()
  (evil-mc-execute-evil-find-char))

(evil-mc-define-visual-handler evil-mc-execute-visual-evil-snipe ()
  (evil-mc-execute-evil-snipe))

(evil-mc-define-visual-handler evil-mc-execute-visual-evil-goto-line ()
  (evil-mc-execute-evil-goto-line))

(evil-mc-define-visual-handler evil-mc-execute-visual-next-line ()
  (evil-mc-execute-move-to-line 'next))

(evil-mc-define-visual-handler evil-mc-execute-visual-prev-line ()
  (evil-mc-execute-move-to-line 'prev))

(evil-mc-define-visual-handler evil-mc-execute-visual-shift-left ()
  (evil-mc-execute-evil-shift 'evil-shift-left))

(evil-mc-define-visual-handler evil-mc-execute-visual-shift-right ()
  (evil-mc-execute-evil-shift 'evil-shift-right))

(evil-mc-define-visual-handler evil-mc-execute-visual-macro ()
  (evil-mc-execute-macro))

(evil-mc-define-visual-handler evil-mc-execute-visual-call-with-last-input ()
  (evil-mc-execute-call-with-last-input))

(evil-mc-define-visual-handler evil-mc-execute-visual-call ()
  (evil-mc-execute-call))

(evil-mc-define-visual-handler evil-mc-execute-visual-call-count ()
  (evil-mc-execute-call-with-count))

;; ----

(defun evil-mc-get-command-handler (cmd state)
  "Get the handler function for CMD and evil STATE."
  (let* ((handler-data (evil-mc-get-object-property evil-mc-known-commands cmd))
         (handler (evil-mc-get-object-property handler-data state)))
    (or handler
        (evil-mc-get-object-property handler-data :default)
        (cond ((eq (evil-get-command-property cmd :repeat) 'motion)
               (cond ((eq state :visual) 'evil-mc-execute-visual-call-count)
                     (t 'evil-mc-execute-default-call-with-count)))))))

(defun evil-mc-get-state-variables (handler)
  "Get all cursor variables required to hold state for HANDLER."
  (let ((categories (evil-get-command-property handler :cursor-state)))
    (when (atom categories) (setq categories (list categories)))
    (when (not (memq :default categories)) (push :default categories))
    (evil-mc-get-cursor-state-names categories)))

(defun evil-mc-get-clear-variables (handler)
  "Get all cursor variables that should be cleared after HANDLER."
  (let ((names (evil-get-command-property handler :cursor-clear)))
    (if (atom names) (list names) names)))

(defun evil-mc-get-var-name-value (var)
  "Gets the current name and value pair of VAR or nil if it needs to be cleared."
  (list var (unless (memq var clear-variables) (symbol-value var))))

(defun evil-mc-execute-for (cursor state-variables clear-variables)
  "Execute the current command for CURSOR in the context of STATE-VARIABLES and
ensuring to set CLEAR-VARIABLES to nil after the execution is complete."
  (when (evil-mc-executing-debug-p)
    (message "Execute %s with %s" (evil-mc-get-command-name) handler))
  (ignore-errors
    (condition-case error
        (cl-progv
            state-variables
            (evil-mc-get-cursor-properties cursor state-variables)

          (goto-char (evil-mc-get-cursor-start cursor))

          (evil-jump-hook (evil-mc-get-command-name))
          (evil-repeat-pre-hook)
          (funcall handler)
          (evil-repeat-post-hook)

          (evil-mc-delete-cursor-overlay cursor)
          (evil-mc-delete-region-overlay (evil-mc-get-cursor-region cursor))
          (apply 'evil-mc-put-cursor-property
                 (evil-mc-put-cursor-overlay cursor (evil-mc-cursor-overlay-at-pos))
                 (mapcan 'evil-mc-get-var-name-value state-variables)))
      (error (message "Failed to execute %s with error %s"
                      (evil-mc-get-command-name)
                      (error-message-string error))
             (cond ((eq :normal (evil-mc-get-command-state))
                    (evil-mc-delete-region-overlay (evil-mc-get-cursor-region cursor))
                    (evil-mc-put-cursor-region cursor nil))
                   (t cursor))))))

(defun evil-mc-execute-for-all ()
  "Execute the current command, stored at `evil-mc-command', for all fake cursors."
  (when (and (evil-mc-has-command-p)
             (not (evil-mc-executing-command-p))
             (not (evil-mc-frozen-p)))
    (when (evil-mc-executing-debug-p)
      (message "Execute %s for all cursors" (evil-mc-get-command-name)))
    (let* ((evil-mc-executing-command t)
           (cursor-list nil)
           (handler (evil-mc-get-command-handler
                     (evil-mc-get-command-name)
                     (evil-mc-get-command-state)))
           (state-variables (evil-mc-get-state-variables handler))
           (clear-variables (evil-mc-get-clear-variables handler)))
      (unless handler
        (message "No handler found for command %s" (evil-mc-get-command-name)))
      (when handler
        (evil-repeat-post-hook)
        (evil-mc-remove-last-undo-marker)
        (evil-with-single-undo
          (save-excursion
            (dolist (cursor evil-mc-cursor-list)
              (setq cursor-list (evil-mc-insert-cursor-into-list
                                 (evil-mc-execute-for cursor
                                                      state-variables
                                                      clear-variables)
                                 cursor-list)))
            (setq evil-mc-cursor-list cursor-list)))))))

(defun evil-mc-remove-last-undo-marker ()
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
   '(("(\\(evil-mc-define-handler\\|evil-mc-define-visual-handler\\)" . font-lock-keyword-face))))

(provide 'evil-mc-command-execute)

;;; evil-mc-command-execute.el ends here
