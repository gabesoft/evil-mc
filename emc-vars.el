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

(defface emc-cursor-inverse-face
  '((t (:inverse-video t :line-width 1)))
  "The face used for fake cursors"
  :group 'emc-multiple-cursors)

(defface emc-cursor-normal-state
  '((((background dark))
     (:box (:line-width -1 :color "#D13A82") :height 1 :background "#0A3641"))
    (t (:box (:line-width -1 :color "#D13A82") :height 1 :background "gray90")))
  "The face used for fake cursors in normal state.")

(defface emc-cursor-insert-state
  '((((background dark))
     (:underline (:line-width -1 :color "#D13A82") :height 1 :background "#0A3641"))
    (t (:underline (:line-width -1 :color "#D13A82") :height 1 :background "gray90")))
  "The face used for fake cursors in insert state.")

(defvar emc-running-command nil
  "True when running a command for all cursors.")

(defvar emc-cursor-command nil
  "True if the current command is an emc cursor command.")

(defun emc-has-cursors-p ()
  "True if there are any fake cursors."
  (not (null emc-cursor-list)))

(defface emc-region-face
  '((t :inherit region))
  "The face used for fake regions"
  :group 'evil-multiple-cursors)

(evil-define-local-var emc-cursor-list nil
  "The list of current fake cursors")

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

(provide'emc-vars)

;;; emc-vars.el ends here
