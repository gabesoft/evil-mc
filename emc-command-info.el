;;; emc-command-info.el --- Info for commands to be run at fake cursors

;;; Commentary:

;; This file contains functions for storing and interacting with the command info
;; to be run for each fake cursor

(require 'evil)
(require 'emc-common)

;;; Code:

(evil-define-local-var emc-command nil
  "Data for the current command to be executed by the fake cursors.")

(evil-define-local-var emc-command-recording nil
  "True if recording `this-command' data.")

(evil-define-local-var emc-command-debug nil
  "If true display debug messages about the current command being recorded.")

(defun emc-command-reset ()
  "Clear the currently saved command info."
  (setq emc-command nil)
  (setq emc-command-recording nil))

(defun emc-command-debug-on ()
  "Show debug messages about the current command being recorded."
  (interactive)
  (setq emc-command-debug t))

(defun emc-command-debug-off ()
  "Hide debug messages about the current command being recorded."
  (interactive)
  (setq emc-command-debug nil))

(defun emc-command-recording-p ()
  "True if recording a command."
  (eq emc-command-recording t))

(defun emc-supported-command-p (cmd)
  "Return true if CMD is supported for multiple cursors."
  (let ((repeat-type (evil-get-command-property cmd :repeat)))
    (or (eq repeat-type 'motion)

        ;; extended commands (should be configurable by user)
        (eq cmd 'evil-commentary)
        (eq cmd 'org-self-insert-command)
        (eq cmd 'transpose-chars-before-point)

        ;; core evil + emacs commands
        (eq cmd 'backward-delete-char-untabify)
        (eq cmd 'copy-to-the-end-of-line)
        (eq cmd 'delete-backward-char)
        (eq cmd 'evil-append)
        (eq cmd 'evil-append-line)
        (eq cmd 'evil-change)
        (eq cmd 'evil-change-line)
        (eq cmd 'evil-delete)
        (eq cmd 'evil-delete-backward-char-and-join)
        (eq cmd 'evil-delete-backward-word)
        (eq cmd 'evil-delete-char)
        (eq cmd 'evil-delete-line)
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
        (eq cmd 'evil-upcase)
        (eq cmd 'evil-visual-char)
        (eq cmd 'evil-visual-line)
        (eq cmd 'evil-yank)
        (eq cmd 'keyboard-quit)
        (eq cmd 'move-text-down)
        (eq cmd 'move-text-up)
        (eq cmd 'newline-and-indent)
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

(defun emc-set-command-properties (&rest properties)
  "Store PROPERTIES and their values into `emc-command'."
  (while properties
    (setq emc-command (emc-put-object-property
                       emc-command
                       (pop properties)
                       (pop properties)))))

(defun emc-get-command-property (name)
  "Return the current command property with NAME."
  (emc-get-object-property emc-command name))

(defun emc-begin-command-save ()
  "Initialize all variables at the start of saving a command."
  (when (and (not (emc-command-recording-p))
             (not emc-running-command)
             (not (evil-emacs-state-p)))
    (setq emc-command nil)
    (let ((cmd this-command))
      (when (emc-supported-command-p cmd)
        (setq emc-command-recording t)
        (emc-set-command-properties
         :name cmd
         :last last-command
         :operator-pending (evil-operator-state-p)
         :evil-state-begin (emc-get-evil-state)
         :keys-pre (this-command-keys-vector))
        (when emc-command-debug
          (message "> CMD-BEGIN %s" emc-command))))))

(defun emc-save-key-sequence (prompt &optional continue-echo dont-downcase-last
                                     can-return-switch-frame cmd-loop)
  "Save the current command key sequence."
  (when (emc-command-recording-p)
    (emc-set-command-properties
     :keys-seq (vconcat
                (emc-get-command-property :keys-seq)
                (this-command-keys-vector)))
    (when emc-command-debug
      (message "+ CMD-KEY-SEQ %s %s %s"
               (this-command-keys)
               (this-command-keys-vector)
               this-command))))

(defun emc-finish-command-save ()
  "Completes the save of a command."
  (when (emc-command-recording-p)
    (emc-set-command-properties
     :keys-post (this-command-keys-vector)
     :last-input (vector last-input-event)
     :evil-state-end (emc-get-evil-state)
     :keys-post-raw (this-single-command-raw-keys))
    (when emc-command-debug
      (message "| CMD-FINISH %s %s" emc-command this-command))
    (ignore-errors (emc-finalize-command)))
  (setq emc-command-recording nil))

(defun emc-get-command-keys-string (name)
  "Get the current command keys with NAME as a string."
  (when emc-command
    (let* ((keys (emc-get-command-keys name))
           (keys-string (mapcar 'char-to-string keys)))
      (apply 'concat keys-string))))

(defun emc-get-command-keys (name)
  "Get the current command keys with NAME as a list."
  (listify-key-sequence (emc-get-command-property name)))

(defun emc-finalize-command ()
  "Makes the command data ready for use, after a save.."
  (let ((pre (emc-get-command-keys :keys-pre))
        (seq (emc-get-command-keys :keys-seq))
        (post (emc-get-command-keys :keys-post))
        (last (emc-get-command-keys :last-input))
        (keys nil))
    (setq keys (or seq pre))
    (unless (null seq)
      (setq keys (append keys (or post last))))
    (emc-set-command-properties :keys keys))
  (message "< CMD-DONE %s %s %s %s %s -> %s"
           (emc-get-object-property emc-command :name)
           (emc-get-command-keys-string :keys-pre)
           (emc-get-command-keys-string :keys-seq)
           (emc-get-command-keys-string :keys-post)
           (emc-get-command-keys-string :last-input)
           (emc-get-command-keys-string :keys)))

(defun emc-add-command-hooks ()
  "Add hooks used for saving the current command."
  (interactive)
  (add-hook 'pre-command-hook 'emc-begin-command-save t t)

  ;; this hook must run before evil-repeat post hook
  ;; which clears the command keys
  (add-hook 'post-command-hook 'emc-finish-command-save nil t)
  (advice-add 'read-key-sequence :before #'emc-save-key-sequence))

(defun emc-remove-command-hooks ()
  "Remove hooks used for saving the current command."
  (interactive)
  (remove-hook 'pre-command-hook 'emc-begin-command-save t)
  (remove-hook 'post-command-hook 'emc-finish-command-save t)
  (advice-remove 'read-key-sequence #'emc-save-key-sequence))

(provide'emc-command-info)

;;; emc-command-info.el ends here
