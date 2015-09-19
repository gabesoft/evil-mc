;;; emc-command-record.el --- Record info for the currently running command

;;; Commentary:

;; This file contains functions for recording information about
;; the currently running command

(require 'cl)
(require 'evil)
(require 'emc-common)

;;; Code:

(defun emc-command-reset ()
  "Clear the currently saved command info."
  (setq emc-command nil)
  (setq emc-recording-command nil))

(defun emc-supported-command-p (cmd)
  "Return true if cmd is supported for multiple cursors."
  (let ((repeat-type (evil-get-command-property cmd :repeat)))
    (or (eq repeat-type 'motion)

        ;; extended commands (should be configurable by user)
        ;; add smartchr commands or disable smartchr in emc mode

        (eq cmd 'evil-commentary)
        (eq cmd 'org-self-insert-command)
        (eq cmd 'orgtbl-self-insert-command)
        (eq cmd 'spacemacs/evil-numbers-increase)
        (eq cmd 'spacemacs/evil-numbers-decrease)
        (eq cmd 'transpose-chars-before-point)
        (eq cmd 'yaml-electric-dash-and-dot)
        (eq cmd 'yaml-electric-bar-and-angle)
        (eq cmd 'electric-newline-and-maybe-indent)

        ;; emacs commands

        (eq cmd 'backward-delete-char-untabify)
        (eq cmd 'copy-to-the-end-of-line)
        (eq cmd 'delete-backward-char)
        (eq cmd 'hippie-expand)
        (eq cmd 'keyboard-quit)
        (eq cmd 'move-text-down)
        (eq cmd 'move-text-up)
        (eq cmd 'newline-and-indent)
        (eq cmd 'paste-after-current-line)
        (eq cmd 'paste-before-current-line)
        (eq cmd 'self-insert-command)
        (eq cmd 'yank)

        ;; evil commands

        (eq cmd 'evil-append)
        (eq cmd 'evil-append-line)
        (eq cmd 'evil-change)
        (eq cmd 'evil-change-line)
        (eq cmd 'evil-complete-next)
        (eq cmd 'evil-complete-next-line)
        (eq cmd 'evil-complete-previous)
        (eq cmd 'evil-complete-previous-line)
        (eq cmd 'evil-delete)
        (eq cmd 'evil-delete-backward-char-and-join)
        (eq cmd 'evil-delete-backward-word)
        (eq cmd 'evil-delete-char)
        (eq cmd 'evil-delete-line)
        (eq cmd 'evil-digit-argument-or-evil-beginning-of-line)
        (eq cmd 'evil-downcase)
        (eq cmd 'evil-goto-mark)
        (eq cmd 'evil-goto-mark-line)
        (eq cmd 'evil-insert-line)
        (eq cmd 'evil-invert-char)
        (eq cmd 'evil-join)
        (eq cmd 'evil-normal-state)
        (eq cmd 'evil-open-above)
        (eq cmd 'evil-open-below)
        (eq cmd 'evil-paste-after)
        (eq cmd 'evil-paste-before)
        (eq cmd 'evil-paste-from-register)
        (eq cmd 'evil-repeat)
        (eq cmd 'evil-replace)
        (eq cmd 'evil-set-marker)
        (eq cmd 'evil-shift-left)
        (eq cmd 'evil-shift-right)
        (eq cmd 'evil-surround-region)
        (eq cmd 'evil-upcase)
        (eq cmd 'evil-use-register)
        (eq cmd 'evil-visual-block)
        (eq cmd 'evil-visual-char)
        (eq cmd 'evil-visual-line)
        (eq cmd 'evil-yank)

        )))

(defun emc-get-evil-state ()
  "Get the current evil state."
  (cond ((evil-insert-state-p) :insert)
        ((evil-motion-state-p) :motion)
        ((evil-visual-state-p) :visual)
        ((evil-normal-state-p) :normal)
        ((evil-replace-state-p) :replace)
        ((evil-operator-state-p) :operator)
        ((evil-emacs-state-p) :emacs)))

(defun emc-get-command-property (name)
  "Return the current command property with NAME."
  (emc-get-object-property emc-command name))

(defun emc-set-command-property (&rest properties)
  "Set one or more command PROPERTIES and their values into `emc-command'."
  (setq emc-command (apply 'emc-put-object-property
                           (cons emc-command properties))))

(defun emc-add-command-property (&rest properties)
  "Append to values of one or more PROPERTIES into `emc-command'."
  (while properties
    (let* ((name (pop properties))
           (new-value (pop properties))
           (old-value (emc-get-command-property name)))
      (cond ((null old-value)
             (emc-set-command-property name new-value))
            ((vectorp old-value)
             (emc-set-command-property name (vconcat old-value new-value)))
            ((listp old-value)
             (emc-set-command-property name (nconc old-value new-value)))
            (t
             (error "Current value is not a sequence %s" old-value))))))

(defun emc-get-command-keys-vector (&optional name)
  "Get the command keys, stored at the property with NAME as a vector."
  (emc-get-command-property (or name :keys)))

(defun emc-get-command-keys-count ()
  "Get the current command numeric prefix."
  (emc-get-command-property :keys-count))

(defun emc-get-command-keys-string (&optional name)
  "Get the command keys, stored at the property with NAME, as a string."
  (when emc-command
    (let* ((keys (emc-get-command-property (or name :keys)))
           (keys-string (mapcar (lambda (k) (if (characterp k)
                                                (char-to-string k) ""))
                                keys)))
      (apply 'concat keys-string))))

(defun emc-get-command-name ()
  "Return the current command name."
  (when emc-command
    (emc-get-command-property :name)))

(defun emc-get-command-state ()
  "Return the current command end evil state."
  (when emc-command
    (emc-get-command-property :evil-state-end)))

(defun emc-get-command-last-input ()
  "Return the last input for the current command."
  (when emc-command
    (emc-get-command-property :last-input)))

(defun emc-save-keys (flag pre-name post-name keys)
  "Save KEYS at PRE-NAME or POST-NAME according to FLAG."
  (ecase flag
    (pre (emc-add-command-property pre-name keys))
    (post (emc-add-command-property post-name keys))))

(defun emc-begin-command-save ()
  "Initialize all variables at the start of saving a command."
  (when (emc-recording-debug-p) (message "command %s %s" this-command (this-command-keys)))
  (when (and (not (emc-executing-command-p))
             (not (emc-recording-command-p)))
    (setq emc-command nil)
    (when (and (emc-has-cursors-p)
               (not (evil-emacs-state-p))
               (emc-supported-command-p this-command))
      (setq emc-recording-command t)
      (emc-set-command-property :name this-command
                                :keys-pre (this-command-keys-vector)
                                :evil-state-begin (emc-get-evil-state))
      (when (emc-recording-debug-p) (message "record-begin %s" emc-command)))))
(put 'emc-begin-command-save 'permanent-local-hook t)

(defun emc-save-keys-motion (flag)
  "Save the current evil motion key sequence."
  (when (emc-recording-command-p)
    (emc-save-keys flag
                   :keys-motion-pre
                   :keys-motion-post
                   (this-command-keys-vector))
    (when (emc-recording-debug-p)
      (message "record-motion %s %s %s %s"
               flag (this-command-keys) (this-command-keys-vector) evil-state))))

(defun emc-save-keys-operator (flag)
  "Save the current evil operator key sequence."
  (when (and (emc-recording-command-p)
             (memq evil-state '(operator)))
    (emc-save-keys flag
                   :keys-operator-pre
                   :keys-operator-post
                   (this-command-keys-vector))
    (when (emc-recording-debug-p)
      (message "record-operator %s %s %s %s"
               flag (this-command-keys) (this-command-keys-vector) evil-state))))

(defun emc-finish-command-save ()
  "Completes the save of a command."
  (when (emc-recording-command-p)
    (emc-set-command-property :evil-state-end (emc-get-evil-state)
                              :last-input last-input-event
                              :keys-post (this-command-keys-vector)
                              :keys-post-raw (this-single-command-raw-keys))
    (when (emc-recording-debug-p)
      (message "record-finish %s %s" emc-command this-command))
    (ignore-errors
      (condition-case error
          (emc-finalize-command)
        (error (message "Saving command %s failed with %s"
                        (emc-get-command-name)
                        (error-message-string error))
               nil))))
  (setq emc-recording-command nil))
(put 'emc-finish-command-save 'permanent-local-hook t)

(defun emc-finalize-command ()
  "Make the command data ready for use, after a save."
  (let* ((keys-pre (emc-get-command-property :keys-pre))
         (keys-pre-with-count (evil-extract-count keys-pre))
         (keys-pre-count (nth 0 keys-pre-with-count))
         (keys-pre-cmd (vconcat (nth 2 keys-pre-with-count)))
         (keys-post (emc-get-command-property :keys-post))
         (keys-motion-pre (emc-get-command-property :keys-motion-pre))
         (keys-motion-post (emc-get-command-property :keys-motion-post))
         (keys-operator-pre (emc-get-command-property :keys-operator-pre))
         (keys-operator-post (emc-get-command-property :keys-operator-post)))
    (emc-set-command-property :keys-count (or keys-pre-count 1))
    (emc-set-command-property
     :keys (cond ((or keys-motion-post keys-motion-pre)
                  (or keys-motion-post keys-motion-pre))
                 ((or keys-operator-pre keys-operator-post)
                  (vconcat (if keys-pre-count keys-pre keys-pre-cmd)
                           (if (or (equal keys-operator-pre keys-pre-cmd)
                                   (and (equal keys-operator-pre
                                               keys-operator-post)
                                        (not (or
                                              (equal keys-operator-pre [?t])
                                              (equal keys-operator-pre [?f]))))
                                   (> (length keys-operator-pre) 1))
                               keys-operator-post
                             (vconcat keys-operator-pre
                                      keys-operator-post))))
                 (t (or keys-post keys-pre)))))
  (when (emc-recording-debug-p)
    (message "record-done %s pre %s post %s keys-motion %s keys-operator %s keys %s"
             (emc-get-command-name)
             (emc-get-command-keys-string :keys-pre)
             (emc-get-command-keys-string :keys-post)
             (emc-get-command-keys-string :keys-motion-post)
             (emc-get-command-keys-string :keys-operator-post)
             (emc-get-command-keys-string :keys))))

(defun emc-add-command-hooks ()
  "Add hooks used for saving the current command."
  (interactive)
  (add-hook 'pre-command-hook 'emc-begin-command-save nil t)
  (add-hook 'post-command-hook 'emc-finish-command-save t t)
  (advice-add 'evil-repeat-keystrokes :before #'emc-save-keys-motion)
  (advice-add 'evil-repeat-motion :before #'emc-save-keys-operator))

(defun emc-remove-command-hooks ()
  "Remove hooks used for saving the current command."
  (interactive)
  (remove-hook 'pre-command-hook 'emc-begin-command-save t)
  (remove-hook 'post-command-hook 'emc-finish-command-save t)
  (advice-remove 'evil-repeat-keystrokes #'emc-save-keys-motion)
  (advice-remove 'evil-repeat-motion #'emc-save-keys-operator))

(provide 'emc-command-record)

;;; emc-command-record.el ends here
