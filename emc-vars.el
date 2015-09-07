;;; emc-vars.el --- Settings and variables for emc

;;; Commentary:

;; This file contains all variables and settings used by emc

;;; Code:

(defface emc-cursor-default-face
  '((t (:background "#D13A82")))
  "The face used for simple fake cursors."
  :group 'evil-multiple-cursors)

(defface emc-region-face
  '((t :inherit region))
  "The face used for fake regions"
  :group 'evil-multiple-cursors)

(evil-define-local-var emc-running-command nil
  "True when running a command for all cursors.")

(evil-define-local-var emc-cursor-command nil
  "True if the current command is an emc cursor command.")

(evil-define-local-var emc-cursor-current-face nil
  "The face to use when making fake cursors.")

(evil-define-local-var emc-cursor-list nil
  "The list of current fake cursors")

(evil-define-local-var emc-frozen nil
  "If true the fake cursors are frozen.")

(evil-define-local-var emc-pattern nil
  "The current pattern")

(evil-define-local-var emc-command nil
  "Data for the current command to be executed by the fake cursors.")

(evil-define-local-var emc-command-recording nil
  "True if recording `this-command' data.")

(evil-define-local-var emc-command-debug nil
  "If true display debug messages about the current command being recorded.")

(evil-define-local-var emc-debug nil
  "If true print debug information.")

(defun emc-has-cursors-p ()
  "True if there are any fake cursors."
  (not (null emc-cursor-list)))

(defun emc-command-recording-p ()
  "True if recording a command."
  (eq emc-command-recording t))

(defun emc-running-command-p ()
  "True when running a command for all fake cursors."
  (eq emc-running-command t))

(defun emc-command-p ()
  "True if there is data saved for the current command."
  (not (null emc-command)))

(defun emc-command-debug-p ()
  "True if debug for command recording is on."
  (eq emc-command-debug t))

(defun emc-command-debug-on ()
  "Show debug messages about the current command being recorded."
  (interactive)
  (setq emc-command-debug t))

(defun emc-command-debug-off ()
  "Hide debug messages about the current command being recorded."
  (interactive)
  (setq emc-command-debug nil))

(defun emc-print-pattern ()
  "Print the curent pattern."
  (interactive)
  (message "%s" emc-pattern))

(defun emc-print-cursor-list ()
  "Return the cursor list."
  (interactive)
  (if emc-cursor-list
      (message "%s: %s" (length emc-cursor-list) emc-cursor-list)
    (message "No cursors found")))

(defun emc-print-command ()
  "Print the information saved for the current command."
  (interactive)
  (message "%s" emc-command))

(defun emc-frozen-p ()
  "True if the fake cursors are frozen."
  (eq emc-frozen t))

(defun emc-freeze ()
  "Freeze the fake cursors."
  (interactive)
  (setq emc-frozen t))

(defun emc-unfreeze ()
  "Unfreeze the fake cursors."
  (interactive)
  (setq emc-frozen nil))

(defun emc-clear-pattern ()
  "Remove the currently saved pattern."
  (setq emc-pattern nil))

(defun emc-get-pattern ()
  "Return the current pattern text."
  (when emc-pattern (car emc-pattern)))

(defun emc-get-pattern-start ()
  "Return the current pattern start position."
  (when emc-pattern (car (cdr emc-pattern))))

(defun emc-get-pattern-end ()
  "Return the current pattern end position."
  (when emc-pattern (cdr (cdr emc-pattern))))

(defun emc-get-pattern-length ()
  "Return the current pattern length."
  (when emc-pattern
    (- (emc-get-pattern-end) (emc-get-pattern-start))))

(defun emc-pattern-p ()
  "True if there is a saved pattern."
  (not (null emc-pattern)))

(provide'emc-vars)

;;; emc-vars.el ends here
