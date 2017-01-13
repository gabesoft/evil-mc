;;; evil-mc-vars.el --- Variables for evil-mc

;;; Commentary:

;; This file contains variables used by evil-mc

(require 'evil-mc-known-commands)
(require 'evil-states)

;;; Code:

(defgroup evil-mc nil
  "Multiple cursors implementation for evil mode."
  :prefix "evil-mc-"
  :group 'evil)

(defun evil-mc-cursor-color (state-cursor)
  "Get the cursor color for a STATE-CURSOR"
  (cond ((and (listp state-cursor) (stringp (car state-cursor)))
         (car state-cursor))
        ((not (eq (face-attribute 'cursor :background) 'unspecified))
         (face-attribute 'cursor :background))
        (t "gray")))

(defface evil-mc-cursor-default-face
  '((t (:inherit cursor :inverse-video nil)))
  "The face used for fake cursors."
  :group 'evil-mc)

(defface evil-mc-region-face
  '((t :inherit region))
  "The face used for fake regions"
  :group 'evil-mc)

(defface evil-mc-cursor-bar-face
  `((t (:height 1 :background ,(evil-mc-cursor-color evil-insert-state-cursor))))
  "The face used for fake cursors if the cursor-type is bar"
  :group 'evil-mc)

(defface evil-mc-cursor-hbar-face
  `((t (:underline (:color ,(evil-mc-cursor-color evil-replace-state-cursor)))))
  "The face used for fake cursors if the cursor-type is horizontal bar"
  :group 'evil-mc)

(defcustom evil-mc-cursor-overlay-priority 201
  "The priority of the fake cursors overlay."
  :type 'integer
  :group 'evil-mc)

(defcustom evil-mc-region-overlay-priority 99
  "The priority of the fake regions overlay."
  :type 'integer
  :group 'evil-mc)

(defvar evil-mc-cursor-variables
  '((:default . (evil-exchange--overlays
                 evil-exchange--position
                 evil-jumper--window-jumps
                 evil-jumper--jumping
                 evil-jump-list
                 evil-last-paste
                 evil-last-register
                 evil-last-repeat
                 evil-markers-alist
                 evil-recording-repeat
                 evil-repeat-count
                 evil-repeat-info
                 evil-repeat-keys
                 evil-repeat-pos
                 evil-repeat-ring
                 evil-this-register
                 evil-was-yanked-without-register
                 kill-ring
                 kill-ring-yank-pointer
                 mark-evil-active
                 mark-ring
                 last-position
                 region
                 register-alist
                 undo-stack
                 undo-stack-pointer
                 temporary-goal-column))
    (:replace . (evil-replace-alist))
    (:dabbrev . (dabbrev--friend-buffer-list
                 dabbrev--last-buffer
                 dabbrev--last-buffer-found
                 dabbrev--last-table
                 dabbrev--last-abbrev-location
                 dabbrev--last-abbreviation
                 dabbrev--last-expansion
                 dabbrev--last-expansion-location
                 dabbrev--last-direction)))
  "Names of variables tracked per cursor during the execution of a command.")

(evil-define-local-var evil-mc-cursor-state nil
  "The state of the real cursor saved while there are active cursors.")

(evil-define-local-var evil-mc-executing-command nil
  "True when executing a command for all cursors.")

(evil-define-local-var evil-mc-recording-command nil
  "True when recording `this-command' data.")

(evil-define-local-var evil-mc-cursor-current-face nil
  "The face to use when making fake cursors.")

(evil-define-local-var evil-mc-cursor-list nil
  "The list of current fake cursors.")

(evil-define-local-var evil-mc-frozen nil
  "If true the fake cursors are frozen.")

(evil-define-local-var evil-mc-pattern nil
  "The current pattern.")

(evil-define-local-var evil-mc-command nil
  "The current command to be executed.")

(evil-define-local-var evil-mc-command-count nil
  "The count for the current command")

(evil-define-local-var evil-mc-temporary-undo nil
  "Variable for saving the `buffer-undo-list' temporarily.")

(evil-define-local-var evil-mc-executing-debug nil
  "If true display debug messages during the execution of a command.")

(evil-define-local-var evil-mc-recording-debug nil
  "If true display debug messages during the recording of a command.")

(evil-define-local-var evil-mc-paused-modes nil
  "List of temporarily disabled minor modes.")

(defun evil-mc-known-command-p (cmd)
  "True if CMD is a supported command."
  (or (not (null (assq cmd evil-mc-known-commands)))
      (not (null (assq cmd evil-mc-custom-known-commands)))
      (eq (evil-get-command-property cmd :repeat) 'motion)))

(defun evil-mc-has-cursors-p ()
  "True if there are any fake cursors."
  (not (null evil-mc-cursor-list)))

(defun evil-mc-has-command-p ()
  "True if there is data saved for the current command."
  (not (null evil-mc-command)))

(defun evil-mc-has-pattern-p ()
  "True if there is a saved pattern."
  (not (null evil-mc-pattern)))

(defun evil-mc-executing-command-p ()
  "True when executing a command for all fake cursors."
  (eq evil-mc-executing-command t))

(defun evil-mc-recording-command-p ()
  "True when recording a command."
  (eq evil-mc-recording-command t))

(defun evil-mc-executing-debug-p ()
  "True if debugging is enabled during the execution of a command."
  (eq evil-mc-executing-debug t))

(defun evil-mc-recording-debug-p ()
  "True if debugging is enabled during the recording of a command."
  (eq evil-mc-recording-debug t))

(defun evil-mc-debug (state executing recording)
  "Enable debugging according to STATE for command EXECUTING or RECORDING or both."
  (when recording (setq evil-mc-recording-debug state))
  (when executing (setq evil-mc-executing-debug state)))

(defun evil-mc-executing-debug-on ()
  "Turn debug on while executing a command."
  (interactive)
  (evil-mc-debug t t nil))

(defun evil-mc-executing-debug-off ()
  "Turn debug off while executing a command."
  (interactive)
  (evil-mc-debug nil t nil))

(defun evil-mc-recording-debug-on ()
  "Turn debug on while recording a command."
  (interactive)
  (evil-mc-debug t nil t))

(defun evil-mc-recording-debug-off ()
  "Turn debug off while recording a command."
  (interactive)
  (evil-mc-debug nil nil t))

(defun evil-mc-all-debug-on ()
  "Turn all debug on."
  (interactive)
  (evil-mc-debug t t t))

(defun evil-mc-all-debug-off ()
  "Turn all debug off."
  (interactive)
  (evil-mc-debug nil t t))

(defun evil-mc-print-pattern ()
  "Print the curent pattern."
  (interactive)
  (evil-mc-message "%s" evil-mc-pattern))

(defun evil-mc-print-cursor-list ()
  "Return the cursor list."
  (interactive)
  (if evil-mc-cursor-list
      (evil-mc-message "%s: %s" (length evil-mc-cursor-list) evil-mc-cursor-list)
    (evil-mc-message "No cursors found")))

(defun evil-mc-print-command ()
  "Print the information saved for the current command."
  (interactive)
  (evil-mc-message "%s" evil-mc-command))

(defun evil-mc-frozen-p ()
  "True if the fake cursors are frozen."
  (eq evil-mc-frozen t))

(defun evil-mc-pause-cursors ()
  "Freeze the fake cursors."
  (interactive)
  (setq evil-mc-frozen t))

(defun evil-mc-resume-cursors ()
  "Unfreeze the fake cursors."
  (interactive)
  (setq evil-mc-frozen nil))

(defun evil-mc-clear-pattern ()
  "Clear the currently saved pattern."
  (setq evil-mc-pattern nil))

(defun evil-mc-clear-command ()
  "Clear the current command."
  (setq evil-mc-command nil))

(defun evil-mc-clear-command-count ()
  "Clear the current command count."
  (setq evil-mc-command-count nil))

(defun evil-mc-clear-cursor-list ()
  "Clear the cursor list."
  (setq evil-mc-cursor-list nil))

(defun evil-mc-update-cursor-list (cursors)
  "Updates the `evil-mc-cursor-list' to CURSORS."
  (setq evil-mc-cursor-list cursors))

(defun evil-mc-clear-executing-command ()
  "Clear the `evil-mc-executing-command' variable."
  (setq evil-mc-executing-command nil))

(defun evil-mc-clear-recording-command ()
  "Clear the `evil-mc-recording-command' variable."
  (setq evil-mc-recording-command nil))

(defun evil-mc-clear-executing-debug ()
  "Clear the `evil-mc-executing-debug' variable."
  (setq evil-mc-executing-debug nil))

(defun evil-mc-clear-recording-debug ()
  "Clear the `evil-mc-recording-debug' variable."
  (setq evil-mc-recording-debug nil))

(defun evil-mc-clear-paused-modes ()
  "Clear the `evil-mc-paused-modes' variable."
  (setq evil-mc-paused-modes nil))

(defun evil-mc-clear-cursor-state ()
  "Clear the `evil-mc-cursor-state' variable."
  (setq evil-mc-cursor-state nil))

(defun evil-mc-get-pattern ()
  "Return the current pattern."
  (when evil-mc-pattern (car evil-mc-pattern)))

(defun evil-mc-get-pattern-text ()
  "Return the current pattern text."
  (when evil-mc-pattern (car (evil-mc-get-pattern))))

(defun evil-mc-get-pattern-start ()
  "Return the current pattern start position."
  (when evil-mc-pattern (nth 1 evil-mc-pattern)))

(defun evil-mc-get-pattern-end ()
  "Return the current pattern end position."
  (when evil-mc-pattern (nth 2 evil-mc-pattern)))

(defun evil-mc-get-pattern-length ()
  "Return the current pattern length."
  (when evil-mc-pattern
    (- (evil-mc-get-pattern-end) (evil-mc-get-pattern-start))))

(defun evil-mc-get-cursor-count ()
  "Return the count of active cursors."
  (1+ (length evil-mc-cursor-list)))

(provide 'evil-mc-vars)

;;; evil-mc-vars.el ends here
