;;; emc-vars.el --- Settings and variables for emc

;;; Commentary:

;; This file contains all variables and settings used by emc

;;; Code:

(defface emc-cursor-face
  '((((class color) (min-colors 88) (background dark))
     :background "#389867")
    (((class color) (min-colors 88) (background light) (type gtk))
     :distant-foreground "gtk_selection_fg_color"
     :background "gtk_selection_bg_color")
    (((class color) (min-colors 88) (background light) (type ns))
     :distant-foreground "ns_selection_fg_color"
     :background "ns_selection_bg_color")
    (((class color) (min-colors 88) (background light))
     :background "lightgoldenrod2")
    (((class color) (min-colors 16) (background dark))
     :background "blue3")
    (((class color) (min-colors 16) (background light))
     :background "lightgoldenrod2")
    (((class color) (min-colors 8))
     :background "blue" :foreground "white")
    (((type tty) (class mono))
     :inverse-video t)
    (t :background "gray"))
  "Basic face for highlighting the region."
  :version "21.1"
  :group 'basic-faces)

(defface emc-cursor-simple
  '((t (:background "#D13A82")))
  "The face used for simple fake cursors."
  :group 'emc-multiple-cursors)

(defface emc-cursor-inverse-face
  '((t (:inverse-video t :line-width 1)))
  "The face used for fake cursors."
  :group 'emc-multiple-cursors)

(defface emc-cursor-normal-state
  '((((background dark))
     (:box (:line-width -1 :color "#D13A82") :height 1 :background "#0A3641"))
    (t (:box (:line-width -1 :color "#D13A82") :height 1 :background "gray90")))
  "The face used for fake cursors in normal state.")

(defface emc-cursor-insert-state
  '((((background dark))
     (:underline (:line-width -1 :color "#D13A82") :height 1 :background "#D13A82"))
    (t (:underline (:line-width -1 :color "#D13A82") :height 1 :background "#D13A82")))
  "The face used for fake cursors in insert state.")

(defvar emc-running-command nil
  "True when running a command for all cursors.")

(defvar emc-cursor-command nil
  "True if the current command is an emc cursor command.")

(defface emc-region-face
  '((t :inherit region))
  "The face used for fake regions"
  :group 'evil-multiple-cursors)

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

(provide'emc-vars)

;;; emc-vars.el ends here
