;;; emc-command-info.el --- Info for the currently running command

;;; Commentary:

;; This file contains functions for storing and interacting with
;; the currently running command info

(require 'evil)
(require 'emc-common)

;;; Code:

(defun emc-command-reset ()
  "Clear the currently saved command info."
  (setq emc-command nil)
  (setq emc-command-recording nil))

(defun emc-listify-keys (value)
  "Convert VALUE to a list of keys."
  (listify-key-sequence (if (sequencep value) value (list value))))

(defun emc-key-to-char (key)
  "Converts KEY to a character if it is not one already."
  (cond ((characterp key) key)
        ((eq 'escape key) 27)
        ((eq 'return key) 13)
        ((eq 'backspace key) 127)
        ((and (stringp key) (string-equal key "escape")) 27)
        ((and (stringp key) (string-equal key "return")) 13)
        ((and (stringp key) (string-equal key "backspace")) 127)
        (t (message "Invalid key %s %s" key (type-of key)) 0)))

(defun emc-supported-command-p (cmd)
  "Return true if CMD is supported for multiple cursors."
  (let ((repeat-type (evil-get-command-property cmd :repeat)))
    (or (eq repeat-type 'motion)

        ;; extended commands (should be configurable by user)
        ;; add smartchr commands or disable smartchr in emc mode

        (eq cmd 'evil-commentary)
        (eq cmd 'org-self-insert-command)
        (eq cmd 'spacemacs/evil-numbers-increase)
        (eq cmd 'spacemacs/evil-numbers-decrease)
        (eq cmd 'transpose-chars-before-point)
        (eq cmd 'yaml-electric-dash-and-dot)
        (eq cmd 'yaml-electric-bar-and-angle)

        ;; core evil + emacs commands

        (eq cmd 'backward-delete-char-untabify)
        (eq cmd 'copy-to-the-end-of-line)
        (eq cmd 'delete-backward-char)
        (eq cmd 'evil-append)
        (eq cmd 'evil-append-line)
        (eq cmd 'evil-change)
        (eq cmd 'evil-change-line)
        (eq cmd 'evil-complete-next)
        (eq cmd 'evil-delete)
        (eq cmd 'evil-delete-backward-char-and-join)
        (eq cmd 'evil-delete-backward-word)
        (eq cmd 'evil-delete-char)
        (eq cmd 'evil-delete-line)
        (eq cmd 'evil-digit-argument-or-evil-beginning-of-line)
        (eq cmd 'evil-downcase)
        (eq cmd 'evil-insert-line)
        (eq cmd 'evil-invert-char)
        (eq cmd 'evil-join)
        (eq cmd 'evil-normal-state)
        (eq cmd 'evil-open-above)
        (eq cmd 'evil-open-below)
        (eq cmd 'evil-paste-after)
        (eq cmd 'evil-paste-before)
        (eq cmd 'evil-repeat)
        (eq cmd 'evil-replace)
        (eq cmd 'evil-surround-region)
        (eq cmd 'evil-upcase)
        (eq cmd 'evil-visual-char)
        (eq cmd 'evil-visual-line)
        (eq cmd 'evil-visual-block)
        (eq cmd 'evil-yank)
        (eq cmd 'keyboard-quit)
        (eq cmd 'move-text-down)
        (eq cmd 'move-text-up)
        (eq cmd 'newline-and-indent)
        (eq cmd 'paste-after-current-line)
        (eq cmd 'paste-before-current-line)
        (eq cmd 'self-insert-command)
        (eq cmd 'yank)

        )))

(defun emc-get-evil-state ()
  "Get the current evil state."
  (cond ((evil-insert-state-p) 'insert)
        ((evil-motion-state-p) 'motion)
        ((evil-visual-state-p) 'visual)
        ((evil-normal-state-p) 'normal)
        ((evil-replace-state-p) 'replace)
        ((evil-operator-state-p) 'operator)
        ((evil-emacs-state-p) 'emacs)))

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
      (emc-set-command-property name (nconc old-value new-value)))))

(defun emc-get-command-keys (&optional name)
  "Get the command keys, stored at the property with NAME, as a list."
  (mapcar 'emc-key-to-char
          (emc-get-command-property (or name :keys))))

(defun emc-get-command-key (&optional name index)
  "Get the command key, stored at the property with NAME, at INDEX."
  (nth (or index 0) (emc-get-command-keys name)))

(defun emc-get-command-keys-string (&optional name)
  "Get the command keys, stored at the property with NAME, as a string."
  (when emc-command
    (let* ((keys (emc-get-command-keys (or name :keys)))
           (keys-string (mapcar 'char-to-string keys)))
      (apply 'concat keys-string))))

(defun emc-set-command-keys (&rest keys)
  "Set the command KEYS to the corresponding values into `emc-command'."
  (while keys
    (let ((name (pop keys))
          (value (pop keys)))
      (emc-set-command-property name (emc-listify-keys value)))))

(defun emc-add-command-keys (&rest keys)
  "Append to the values of KEYS into `emc-command'."
  (while keys
    (let ((name (pop keys))
          (value (pop keys)))
      (emc-add-command-property name (emc-listify-keys value)))))

(defun emc-get-command-name ()
  "Return the current command name."
  (when emc-command
    (emc-get-command-property :name)))

(defun emc-get-command-state ()
  "Return the current command end evil state."
  (when emc-command
    (emc-get-command-property :evil-state-end)))

(defun emc-save-keys (flag pre-name post-name keys)
  "Save KEYS at PRE-NAME or POST-NAME according to FLAG."
  (cond ((eq flag 'pre) (emc-add-command-keys pre-name keys))
        ((eq flag 'post) (emc-add-command-keys post-name keys))
        (t (error (message "unknown flag %s" flag)))))

(defun emc-begin-command-save ()
  "Initialize all variables at the start of saving a command."
  (when (emc-command-debug-p) (message "CMD %s %s" this-command (this-command-keys)))
  (when (and (not (emc-running-command-p))
             (not (emc-command-recording-p)))
    (setq emc-command nil)
    (when (and (emc-has-cursors-p)
               (not (evil-emacs-state-p))
               (emc-supported-command-p this-command))
      (setq emc-command-recording t)
      (emc-set-command-property :name this-command
                                :evil-state-begin (emc-get-evil-state))
      (emc-set-command-keys :keys-pre (this-single-command-raw-keys))
      (when (emc-command-debug-p) (message "CMD-BEGIN %s" emc-command)))))
(put 'emc-begin-command-save 'permanent-local-hook t)

(defun emc-save-keys-motion (flag)
  "Save the current evil motion key sequence."
  (when (emc-command-recording-p)
    (emc-save-keys flag
                   :keys-motion-pre
                   :keys-motion-post
                   (this-command-keys-vector))
    (when (emc-command-debug-p)
      (message "CMD-MOTION %s %s %s %s"
               flag (this-command-keys) (this-command-keys-vector) evil-state))))

(defun emc-save-keys-operator (flag)
  "Save the current evil operator key sequence."
  (when (and (emc-command-recording-p) (memq evil-state '(operator)))
    (emc-save-keys flag
                   :keys-operator-pre
                   :keys-operator-post
                   (this-command-keys-vector))
    (when (emc-command-debug-p)
      (message "CMD-OPERATOR %s %s %s %s"
               flag (this-command-keys) (this-command-keys-vector) evil-state))))

(defun emc-finish-command-save ()
  "Completes the save of a command."
  (when (emc-command-recording-p)
    (emc-set-command-property :evil-state-end (emc-get-evil-state))
    (emc-set-command-keys :last-input (emc-key-to-char last-input-event)
                          :keys-post (this-command-keys-vector)
                          :keys-post-raw (this-single-command-raw-keys))
    (when (emc-command-debug-p)
      (message "CMD-FINISH %s %s" emc-command this-command))
    (ignore-errors
      (condition-case error
          (emc-finalize-command)
        (error (message "Saving command %s failed with %s"
                        (emc-get-command-name)
                        (error-message-string error))
               nil))))
  (setq emc-command-recording nil))
(put 'emc-finish-command-save 'permanent-local-hook t)

(defun emc-finalize-command ()
  "Makes the command data ready for use, after a save."
  (let ((keys-pre (emc-get-command-keys :keys-pre))
        (keys-post (emc-get-command-keys :keys-post))
        (keys-motion-pre (emc-get-command-keys :keys-motion-pre))
        (keys-motion-post (emc-get-command-keys :keys-motion-post))
        (keys-operator-pre (emc-get-command-keys :keys-operator-pre))
        (keys-operator-post (emc-get-command-keys :keys-operator-post)))
    (emc-set-command-property
     :keys (cond ((or keys-motion-pre keys-motion-post)
                  (or keys-motion-post keys-motion-pre))
                 ((or keys-operator-pre keys-operator-post)
                  (append keys-pre (if (equal keys-operator-pre
                                              keys-operator-post)
                                       keys-operator-post
                                     (append keys-operator-pre
                                             keys-operator-post))))
                 (t (or keys-post keys-pre)))))
  (when (emc-command-debug-p)
    (message "CMD-DONE %s pre %s keys-motion %s keys-operator %s keys %s"
             (emc-get-command-name)
             (emc-get-command-keys-string :keys-pre)
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

(provide 'emc-command-info)

;;; emc-command-info.el ends here
