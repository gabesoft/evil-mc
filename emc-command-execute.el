;;; emc-command-execute.el --- Execute commands for every fake cursor

;;; Commentary:

;; This file contains functions for executing a command for every fake cursor

(require 'cl)
(require 'evil)
(require 'emc-common)
(require 'emc-cursor-state)
(require 'emc-cursor-make)
(require 'emc-command-record)
(require 'emc-region)

;;; Code:


;; TODO move to vars
(defvar emc-cursor-state
  '((:default . (column
                 evil-jump-list
                 evil-last-paste
                 evil-last-register
                 evil-markers-alist
                 evil-this-register
                 evil-was-yanked-without-register
                 kill-ring
                 kill-ring-yank-pointer
                 mark-evil-active
                 mark-ring
                 region
                 register-alist))
    (:complete . (dabbrev--friend-buffer-list
                  dabbrev--last-buffer
                  dabbrev--last-buffer-found
                  dabbrev--last-table
                  dabbrev--last-abbrev-location
                  dabbrev--last-abbreviation
                  dabbrev--last-expansion
                  dabbrev--last-expansion-location
                  dabbrev--last-direction)))
  "State tracked per cursor.")

(defun emc-execute-for (cursor)
  "Execute the current command for CURSOR."
  ())


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