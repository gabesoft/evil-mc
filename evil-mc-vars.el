;;; evil-mc-vars.el --- Settings and variables for evil-mc

;;; Commentary:

;; This file contains all variables and settings used by evil-mc

;;; Code:

(defface evil-mc-cursor-default-face
  '((t (:inherit cursor :inverse-video nil)))
  "The face used for fake cursors."
  :group 'evil-mc)

(defface evil-mc-region-face
  '((t :inherit region))
  "The face used for fake regions"
  :group 'evil-mc)

(defcustom evil-mc-cursor-overlay-priority 201
  "The priority of the fake cursors overlay."
  :type 'integer
  :group 'evil-mc)

(defcustom evil-mc-region-overlay-priority 99
  "The priority of the fake regions overlay."
  :type 'integer
  :group 'evil-mc)

(defvar evil-mc-cursor-state
  '((:default . (column
                 evil-exchange--overlays
                 evil-exchange--position
                 evil-jump-list
                 evil-last-paste
                 evil-last-register
                 evil-last-repeat
                 evil-markers-alist
                 evil-recording-repeat
                 evil-repeat-count
                 evil-repeat-info
                 evil-repeat-keys
                 evil-repeat-move-cursor
                 evil-repeat-pos
                 evil-repeat-ring
                 evil-this-register
                 evil-was-yanked-without-register
                 kill-ring
                 kill-ring-yank-pointer
                 mark-evil-active
                 mark-ring
                 region
                 register-alist))
    (:dabbrev . (dabbrev--friend-buffer-list
                 dabbrev--last-buffer
                 dabbrev--last-buffer-found
                 dabbrev--last-table
                 dabbrev--last-abbrev-location
                 dabbrev--last-abbreviation
                 dabbrev--last-expansion
                 dabbrev--last-expansion-location
                 dabbrev--last-direction)))
  "State tracked per cursor.")

(defvar evil-mc-known-commands
  '(

    (backward-delete-char-untabify . ((:default . evil-mc-execute-default-call-with-count)))
    (company-complete-selection . ((:default . evil-mc-execute-default-call)))
    (company-select-next . ((:default . evil-mc-execute-default-ignore)))
    (copy-to-the-end-of-line . ((:default . evil-mc-execute-default-call)))
    (delete-backward-char . ((:default . evil-mc-execute-default-call-with-count)))
    (electric-newline-and-maybe-indent . ((:default . evil-mc-execute-default-call)))
    (evil-a-WORD . ((:default . evil-mc-execute-default-call-with-count) (:visual . evil-mc-execute-visual-text-object)))
    (evil-a-back-quote . ((:default . evil-mc-execute-default-call-with-count) (:visual . evil-mc-execute-visual-text-object)))
    (evil-a-bracket . ((:default . evil-mc-execute-default-call-with-count) (:visual . evil-mc-execute-visual-text-object)))
    (evil-a-curly . ((:default . evil-mc-execute-default-call-with-count) (:visual . evil-mc-execute-visual-text-object)))
    (evil-a-double-quote . ((:default . evil-mc-execute-default-call-with-count) (:visual . evil-mc-execute-visual-text-object)))
    (evil-a-paragraph . ((:default . evil-mc-execute-default-call-with-count) (:visual . evil-mc-execute-visual-text-object)))
    (evil-a-paren . ((:default . evil-mc-execute-default-call-with-count) (:visual . evil-mc-execute-visual-text-object)))
    (evil-a-sentence . ((:default . evil-mc-execute-default-call-with-count) (:visual . evil-mc-execute-visual-text-object)))
    (evil-a-single-quote . ((:default . evil-mc-execute-default-call-with-count) (:visual . evil-mc-execute-visual-text-object)))
    (evil-a-symbol . ((:default . evil-mc-execute-default-call-with-count) (:visual . evil-mc-execute-visual-text-object)))
    (evil-a-tag . ((:default . evil-mc-execute-default-call-with-count) (:visual . evil-mc-execute-visual-text-object)))
    (evil-a-word . ((:default . evil-mc-execute-default-call-with-count) (:visual . evil-mc-execute-visual-text-object)))
    (evil-an-angle . ((:default . evil-mc-execute-default-call-with-count) (:visual . evil-mc-execute-visual-text-object)))
    (evil-append . ((:default . evil-mc-execute-default-call-with-count)))
    (evil-append-line . ((:default . evil-mc-execute-default-call-with-count)))
    (evil-beginning-of-line . ((:default . evil-mc-execute-default-call ) (:visual . evil-mc-execute-visual-call)))
    (evil-beginning-of-line-or-digit-argument . ((:default . evil-mc-execute-default-call) (:visual . evil-mc-execute-visual-call)))
    (evil-beginning-of-visual-line . ((:default . evil-mc-execute-default-call) (:visual . evil-mc-execute-visual-call)))
    (evil-change . ((:default . evil-mc-execute-default-evil-change)))
    (evil-change-line . ((:default . evil-mc-execute-default-evil-change-line)))
    (evil-commentary . ((:default . evil-mc-execute-default-evil-commentary)))
    (evil-complete-next . ((:default . evil-mc-execute-default-complete)))
    (evil-complete-next-line . ((:default . evil-mc-execute-default-complete)))
    (evil-complete-previous . ((:default . evil-mc-execute-default-complete)))
    (evil-complete-previous-line . ((:default . evil-mc-execute-default-complete)))
    (evil-delete . ((:default . evil-mc-execute-default-evil-delete)))
    (evil-delete-backward-char-and-join . ((:default . evil-mc-execute-default-call-with-count)))
    (evil-delete-backward-word . ((:default . evil-mc-execute-default-call)))
    (evil-delete-char . ((:default . evil-mc-execute-default-evil-delete-char)))
    (evil-delete-line . ((:default . evil-mc-execute-default-evil-delete-line)))
    (evil-digit-argument-or-evil-beginning-of-line . ((:default . evil-mc-execute-default-call) (:visual . evil-mc-execute-visual-call)))
    (evil-downcase . ((:default . evil-mc-execute-default-evil-downcase)))
    (evil-exchange-point-and-mark . ((:visual . evil-mc-execute-visual-exchange-point-and-mark)))
    (evil-find-char . ((:default . evil-mc-execute-default-evil-find-char) (:visual . evil-mc-execute-visual-evil-find-char)))
    (evil-find-char-backward . ((:default . evil-mc-execute-default-evil-find-char) (:visual . evil-mc-execute-visual-evil-find-char)))
    (evil-find-char-to . ((:default . evil-mc-execute-default-evil-find-char) (:visual . evil-mc-execute-visual-evil-find-char)))
    (evil-find-char-to-backward . ((:default . evil-mc-execute-default-evil-find-char) (:visual . evil-mc-execute-visual-evil-find-char)))
    (evil-first-non-blank . ((:default . evil-mc-execute-default-call) (:visual . evil-mc-execute-visual-call)))
    (evil-first-non-blank-of-visual-line . ((:default . evil-mc-execute-default-call) (:visual . evil-mc-execute-visual-call)))
    (evil-goto-definition . ((:default . evil-mc-execute-default-call) (:visual . evil-mc-execute-visual-call)))
    (evil-goto-line . ((:default . evil-mc-execute-default-evil-goto-line) (:visual . evil-mc-execute-visual-evil-goto-line)))
    (evil-goto-mark . ((:default . evil-mc-execute-default-call-with-last-input) (:visual . evil-mc-execute-visual-call-with-last-input)))
    (evil-goto-mark-line . ((:default . evil-mc-execute-default-call-with-last-input) (:visual . evil-mc-execute-visual-call-with-last-input)))
    (evil-inner-WORD . ((:default . evil-mc-execute-default-call-with-count) (:visual . evil-mc-execute-visual-text-object)))
    (evil-inner-angle . ((:default . evil-mc-execute-default-call-with-count) (:visual . evil-mc-execute-visual-text-object)))
    (evil-inner-back-quote . ((:default . evil-mc-execute-default-call-with-count) (:visual . evil-mc-execute-visual-text-object)))
    (evil-inner-bracket . ((:default . evil-mc-execute-default-call-with-count) (:visual . evil-mc-execute-visual-text-object)))
    (evil-inner-curly . ((:default . evil-mc-execute-default-call-with-count) (:visual . evil-mc-execute-visual-text-object)))
    (evil-inner-double-quote . ((:default . evil-mc-execute-default-call-with-count) (:visual . evil-mc-execute-visual-text-object)))
    (evil-inner-paragraph . ((:default . evil-mc-execute-default-call-with-count) (:visual . evil-mc-execute-visual-text-object)))
    (evil-inner-paren . ((:default . evil-mc-execute-default-call-with-count) (:visual . evil-mc-execute-visual-text-object)))
    (evil-inner-sentence . ((:default . evil-mc-execute-default-call-with-count) (:visual . evil-mc-execute-visual-text-object)))
    (evil-inner-single-quote . ((:default . evil-mc-execute-default-call-with-count) (:visual . evil-mc-execute-visual-text-object)))
    (evil-inner-symbol . ((:default . evil-mc-execute-default-call-with-count) (:visual . evil-mc-execute-visual-text-object)))
    (evil-inner-tag . ((:default . evil-mc-execute-default-call-with-count) (:visual . evil-mc-execute-visual-text-object)))
    (evil-inner-word . ((:default . evil-mc-execute-default-call-with-count) (:visual . evil-mc-execute-visual-text-object)))
    (evil-insert . ((:default . evil-mc-execute-default-call-with-count)))
    (evil-insert-line . ((:default . evil-mc-execute-default-call-with-count)))
    (evil-invert-case . ((:default . evil-mc-execute-default-evil-invert-case)))
    (evil-invert-char . ((:default . evil-mc-execute-default-evil-invert-char)))
    (evil-join . ((:default . evil-mc-execute-default-evil-join)))
    (evil-jump-item . ((:default . evil-mc-execute-default-call) (:visual . evil-mc-execute-visual-call)))
    (evil-lookup . ((:default . evil-mc-execute-default-call) (:visual . evil-mc-execute-visual-call)))
    (evil-middle-of-visual-line . ((:default . evil-mc-execute-default-call) (:visual evil-mc-execute-visual-call)))
    (evil-next-line . ((:default . evil-mc-execute-default-next-line) (:visual . evil-mc-execute-visual-next-line)))
    (evil-next-match . ((:default . evil-mc-execute-default-call-with-count) (:visual . evil-mc-execute-visual-text-object)))
    (evil-normal-state . ((:default . evil-mc-execute-default-evil-normal-state)))
    (evil-open-above . ((:default . evil-mc-execute-default-call-with-count)))
    (evil-open-below . ((:default . evil-mc-execute-default-call-with-count)))
    (evil-paste-after . ((:default . evil-mc-execute-default-evil-paste)))
    (evil-paste-before . ((:default . evil-mc-execute-default-evil-paste)))
    (evil-previous-line . ((:default . evil-mc-execute-default-prev-line) (:visual . evil-mc-execute-visual-prev-line)))
    (evil-previous-match . ((:default . evil-mc-execute-default-call-with-count) (:visual . evil-mc-execute-visual-text-object)))
    (evil-repeat . ((:default . evil-mc-execute-default-call-with-count)))
    (evil-repeat-pop . ((:default . evil-mc-execute-default-call-with-count)))
    (evil-repeat-pop-next . ((:default . evil-mc-execute-default-call-with-count)))
    (evil-replace . ((:default . evil-mc-execute-default-evil-replace)))
    (evil-exchange . ((:default . evil-mc-execute-default-evil-exchange)))
    (evil-exchange-cancel . ((:default . evil-mc-execute-default-call)))
    (evil-search-backward . ((:default . evil-mc-execute-default-call) (:visual . evil-mc-execute-visual-call)))
    (evil-search-forward . ((:default . evil-mc-execute-default-call) (:visual . evil-mc-execute-visual-call)))
    (evil-set-marker . ((:default . evil-mc-execute-default-call-with-last-input) (:visual . evil-mc-execute-visual-call-with-last-input)))
    (evil-shift-left . ((:default . evil-mc-execute-default-evil-shift-left) (:visual . evil-mc-execute-visual-evil-shift-left)))
    (evil-shift-right . ((:default . evil-mc-execute-default-evil-shift-right) (:visual . evil-mc-execute-visual-evil-shift-right)))
    (evil-snipe-F . ((:default . evil-mc-execute-default-evil-snipe) (:visual . evil-mc-execute-visual-evil-snipe)))
    (evil-snipe-S . ((:default . evil-mc-execute-default-evil-snipe) (:visual . evil-mc-execute-visual-evil-snipe)))
    (evil-snipe-T . ((:default . evil-mc-execute-default-evil-snipe) (:visual . evil-mc-execute-visual-evil-snipe)))
    (evil-snipe-f . ((:default . evil-mc-execute-default-evil-snipe) (:visual . evil-mc-execute-visual-evil-snipe)))
    (evil-snipe-s . ((:default . evil-mc-execute-default-evil-snipe) (:visual . evil-mc-execute-visual-evil-snipe)))
    (evil-snipe-t . ((:default . evil-mc-execute-default-evil-snipe) (:visual . evil-mc-execute-visual-evil-snipe)))
    (evil-surround-region . ((:default . evil-mc-execute-default-evil-surround-region)))
    (evil-upcase . ((:default . evil-mc-execute-default-evil-upcase)))
    (evil-use-register . ((:default . evil-mc-execute-default-call-with-last-input) (:visual . evil-mc-execute-visual-call-with-last-input)))
    (evil-visual-block . ((:visual . evil-mc-execute-default-not-supported)))
    (evil-visual-char . ((:default . evil-mc-execute-default-force-normal-state) (:visual . evil-mc-execute-visual-char)))
    (evil-visual-exchange-corners . ((:default . evil-mc-execute-default-call) (:visual . evil-mc-execute-visual-call)))
    (evil-visual-line . ((:default . evil-mc-execute-default-force-normal-state) (:visual . evil-mc-execute-visual-line)))
    (evil-visual-restore . ((:default . evil-mc-execute-default-call) (:visual . evil-mc-execute-visual-call)))
    (evil-window-middle . ((:default . evil-mc-execute-default-call) (:visual . evil-mc-execute-visual-call)))
    (evil-yank . ((:default . evil-mc-execute-default-evil-yank)))
    (exchange-point-and-mark . ((:visual . evil-mc-execute-visual-exchange-point-and-mark)))
    (hippie-expand . ((:default . evil-mc-execute-default-hippie-expand)))
    (indent-for-tab-command . ((:default . evil-mc-execute-default-call)))
    (unimpaired/paste-below . ((:default . evil-mc-execute-default-call)))
    (unimpaired/paste-above . ((:default . evil-mc-execute-default-call)))
    (indent-region-or-buffer . ((:default . evil-mc-execute-default-ignore)))
    (keyboard-quit . ((:default . evil-mc-execute-default-ignore)))
    (move-text-down . ((:default . evil-mc-execute-default-call-with-count)))
    (move-text-up . ((:default . evil-mc-execute-default-call-with-count)))
    (newline-and-indent . ((:default . evil-mc-execute-default-call)))
    (org-self-insert-command . ((:default . evil-mc-execute-default-call-with-count)))
    (orgtbl-self-insert-command . ((:default . evil-mc-execute-default-call-with-count)))
    (paste-after-current-line . ((:default . evil-mc-execute-default-call-with-count)))
    (paste-before-current-line . ((:default . evil-mc-execute-default-call-with-count)))
    (self-insert-command . ((:default . evil-mc-execute-default-call-with-count)))
    (spacemacs/evil-numbers-decrease . ((:default . evil-mc-execute-default-call-with-count)))
    (spacemacs/evil-numbers-increase . ((:default . evil-mc-execute-default-call-with-count)))
    (transpose-chars-before-point . ((:default . evil-mc-execute-default-call-with-count)))
    (undo . ((:default . evil-mc-execute-default-ignore)))
    (undo-tree-redo . ((:default . evil-mc-execute-default-ignore)))
    (undo-tree-undo . ((:default . evil-mc-execute-default-ignore)))
    (yaml-electric-backspace . ((:default . evil-mc-execute-default-call-with-count)))
    (yaml-electric-bar-and-angle . ((:default . evil-mc-execute-default-call-with-count)))
    (yaml-electric-dash-and-dot . ((:default . evil-mc-execute-default-call-with-count)))
    (yank . ((:default . evil-mc-execute-default-call)))

    )
  "A list of the supported commands and their handlers")

(evil-define-local-var evil-mc-executing-command nil
  "True when executing a command for all cursors.")

(evil-define-local-var evil-mc-recording-command nil
  "True when recording `this-command' data.")

(evil-define-local-var evil-mc-cursor-command nil
  "True if the current command is an evil-mc cursor command.")

(evil-define-local-var evil-mc-cursor-current-face nil
  "The face to use when making fake cursors.")

(evil-define-local-var evil-mc-cursor-list nil
  "The list of current fake cursors")

(evil-define-local-var evil-mc-frozen nil
  "If true the fake cursors are frozen.")

(evil-define-local-var evil-mc-pattern nil
  "The current pattern")

(evil-define-local-var evil-mc-command nil
  "Data for the current command to be executed by the fake cursors.")

(evil-define-local-var evil-mc-executing-debug nil
  "If true display debug messages during the execution of a command.")

(evil-define-local-var evil-mc-recording-debug nil
  "If true display debug messages during the recording of a command.")

(defun evil-mc-known-command-p (cmd)
  "True if CMD is a supported command"
  (or (not (null (assq cmd evil-mc-known-commands)))
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
  (message "%s" evil-mc-pattern))

(defun evil-mc-print-cursor-list ()
  "Return the cursor list."
  (interactive)
  (if evil-mc-cursor-list
      (message "%s: %s" (length evil-mc-cursor-list) evil-mc-cursor-list)
    (message "No cursors found")))

(defun evil-mc-print-command ()
  "Print the information saved for the current command."
  (interactive)
  (message "%s" evil-mc-command))

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

(defun evil-mc-clear-cursor-list ()
  "Clear the cursor list."
  (setq evil-mc-cursor-list nil))

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

(provide'evil-mc-vars)

;;; evil-mc-vars.el ends here
