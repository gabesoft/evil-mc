;;; evil-mc.el --- Multiple cursors for evil-mode

;; Copyright © 2015 Gabriel Adomnicai <gabesoft@gmail.com>

;; Author: Gabriel Adomnicai <gabesoft@gmail.com>
;; Maintainer: Gabriel Adomnicai <gabesoft@gmail.com>
;; Version: 0.0.1
;; Keywords: evil editing multiple-cursors vim evil-multiple-cursors evil-mc evil-mc
;; Homepage: https://github.com/gabesoft/evil-mc
;; Package-Requires: ((emacs "24") (evil "1.2.5"))
;;
;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; This library provides multiple cursors functionality for evil-mode
;;
;; Install:
;;
;; (require 'evil-mc)
;;
;;
;; Usage:
;;
;; (evil-mc-mode 1)        ; enable for a single buffer
;;
;; (global-evil-mc-mode 1) ; enable for all buffers
;;
;;
;; See the README for more details

;;; Code:

(require 'evil)

(require 'evil-mc-common)
(require 'evil-mc-vars)
(require 'evil-mc-cursor-state)
(require 'evil-mc-cursor-make)
(require 'evil-mc-command-record)
(require 'evil-mc-command-execute)
(require 'evil-mc-region)

(eval-when-compile (require 'cl-lib))

(defcustom evil-mc-mode-line
  `(:eval (if (> (evil-mc-get-cursor-count) 1)
              (format ,(propertize " %s:%d" 'face 'cursor)
                      evil-mc-mode-line-prefix
                      (evil-mc-get-cursor-count))
            (format ,(propertize " %s") evil-mc-mode-line-prefix)))
  "Cursors indicator in the mode line."
  :group 'evil-mc)

(put 'evil-mc-mode-line 'risky-local-variable t)

;;;###autoload
(define-minor-mode evil-mc-mode
  "Toggle evil multiple cursors in a single buffer."
  :group 'evil-mc
  :init-value nil
  :lighter evil-mc-mode-line
  (cond (evil-mc-mode
         (evil-mc-define-vars)
         (evil-mc-initialize-vars)
         (evil-mc-initialize-keys)
         (evil-mc-initialize-hooks))
        (t
         (evil-mc-teardown-keys)
         (evil-mc-teardown-hooks))))

(put 'evil-mc-mode 'permanent-local t)

;;;###autoload
(define-globalized-minor-mode global-evil-mc-mode
  evil-mc-mode evil-mc-initialize)

;;;###autoload
(defun evil-mc-initialize ()
  "Enable `evil-mc-mode' in the current buffer."
  (evil-mc-mode 1))

;;;###autoload
(defun turn-on-evil-mc-mode ()
  "Turn on evil-mc mode in the current buffer."
  (interactive)
  (evil-mc-mode 1))

;;;###autoload
(defun turn-off-evil-mc-mode ()
  "Turn off evil-mc mode in the current buffer."
  (interactive)
  (evil-mc-mode -1))

(defun evil-mc-define-vars ()
  "Define vars that can be overridden before enabling `evil-mc-mode'."

  (defvar evil-mc-mode-line-prefix "emc"
    "The string used in the mode line to identify `evil-mc-mode'.")

  (defvar evil-mc-incompatible-minor-modes
    '(flyspell-mode aggressive-indent-mode yas-minor-mode)
    "Minor modes that are incompatible with `evil-mc-mode'.")

  (defvar evil-mc-custom-known-commands nil
    "Custom command handlers. The entries here should have
the same form as those in `evil-mc-known-commands'.
This variable can be used to override default command handlers
implementations.")

  (defvar evil-mc-keys
    '(("grm" . evil-mc-make-all-cursors)
      ("gru" . evil-mc-undo-all-cursors)
      ("grp" . evil-mc-pause-cursors)
      ("grr" . evil-mc-resume-cursors)
      ("grf" . evil-mc-make-and-goto-first-cursor)
      ("grl" . evil-mc-make-and-goto-last-cursor)
      ("grh" . evil-mc-make-cursor-here)
      ("M-n" . evil-mc-make-and-goto-next-cursor)
      (",N" . evil-mc-skip-and-goto-next-cursor)
      ("M-p" . evil-mc-make-and-goto-prev-cursor)
      (",P" . evil-mc-skip-and-goto-prev-cursor)
      ("C-n" . evil-mc-make-and-goto-next-match)
      (",n" . evil-mc-skip-and-goto-next-match)
      ("C-t" . evil-mc-skip-and-goto-next-match)
      ("C-p" . evil-mc-make-and-goto-prev-match)
      (",p" . evil-mc-skip-and-goto-prev-match))
    "Association list of key maps.
Entries have the form (KEY . DEF), where KEY is the key
that would trigger the `evil-mc' DEF.  The keys defined here
will be set up in `normal' and `visual' mode."))

(defun evil-mc-initialize-vars ()
  "Initialize all variables used by `evil-mc'."
  (evil-mc-clear-pattern)
  (evil-mc-clear-command)
  (evil-mc-clear-executing-command)
  (evil-mc-clear-recording-command)
  (evil-mc-clear-executing-debug)
  (evil-mc-clear-recording-debug)
  (evil-mc-clear-cursor-list)
  (evil-mc-resume-cursors))

(defun evil-mc-define-key (map key def)
  "In MAP define key sequence KEY as DEF."
  (when (and (boundp map) (keymapp (symbol-value map)))
    (define-key (symbol-value map) key def)))

(defun evil-mc-initialize-keys ()
  "Setup the `evil-mc' keys for normal and visual states."

  (when (bound-and-true-p evil-mode)
    (dolist (key evil-mc-keys)
      (evil-mc-define-key 'evil-normal-state-local-map (kbd (car key)) (cdr key))
      (evil-mc-define-key 'evil-visual-state-local-map (kbd (car key)) (cdr key)))))

(defun evil-mc-teardown-keys ()
  "Remove the `evil-mc' keys for normal and visual states."
  (when (and (bound-and-true-p evil-mode) (boundp 'evil-mc-keys))
    (dolist (key evil-mc-keys)
      (evil-mc-define-key 'evil-normal-state-local-map (kbd (car key)) nil)
      (evil-mc-define-key 'evil-visual-state-local-map (kbd (car key)) nil))))

(defun evil-mc-pause-incompatible-modes ()
  "Temporarily disable incompatible minor modes."
  (dolist (mode evil-mc-incompatible-minor-modes)
    (when (and (boundp mode) (eval mode))
      (push mode evil-mc-paused-modes)
      (funcall mode -1))))

(defun evil-mc-resume-incompatible-modes ()
  "Re-enable incompatible minor modes."
  (dolist (mode evil-mc-paused-modes) (funcall mode 1))
  (evil-mc-clear-paused-modes))

(defun evil-mc-initialize-hooks ()
  "Initialize all hooks used by `evil-mc'."
  (add-hook 'evil-mc-before-cursors-created 'evil-mc-pause-incompatible-modes t t)
  (add-hook 'evil-mc-before-cursors-created 'evil-mc-initialize-active-state t t)
  (add-hook 'evil-mc-after-cursors-deleted 'evil-mc-teardown-active-state t t)
  (add-hook 'evil-mc-after-cursors-deleted 'evil-mc-resume-incompatible-modes t t))

(defun evil-mc-teardown-hooks ()
  "Teardown all hooks used by `evil-mc'."
  (remove-hook 'evil-mc-before-cursors-created 'evil-mc-pause-incompatible-modes t)
  (remove-hook 'evil-mc-before-cursors-created 'evil-mc-initialize-active-state t)
  (remove-hook 'evil-mc-after-cursors-deleted 'evil-mc-teardown-active-state t)
  (remove-hook 'evil-mc-after-cursors-deleted 'evil-mc-resume-incompatible-modes t))

(defun evil-mc-initialize-active-state ()
  "Initialize all variables and hooks used while there are active cursors."
  (when (bound-and-true-p evil-mode)
    (evil-mc-clear-command)
    (evil-mc-clear-executing-command)
    (evil-mc-clear-recording-command)
    (add-hook 'pre-command-hook 'evil-mc-begin-command-save nil t)
    (add-hook 'post-command-hook 'evil-mc-finish-command-save t t)
    (add-hook 'post-command-hook 'evil-mc-execute-for-all t t)
    (advice-add 'evil-repeat-keystrokes :before #'evil-mc-save-keys-motion)
    (advice-add 'evil-repeat-motion :before #'evil-mc-save-keys-operator)))

(defun evil-mc-teardown-active-state ()
  "Teardown all variables and hooks used while there are active cursors."
  (when (bound-and-true-p evil-mode)
    (remove-hook 'pre-command-hook 'evil-mc-begin-command-save t)
    (remove-hook 'post-command-hook 'evil-mc-finish-command-save t)
    (remove-hook 'post-command-hook 'evil-mc-execute-for-all t)
    (advice-remove 'evil-repeat-keystrokes #'evil-mc-save-keys-motion)
    (advice-remove 'evil-repeat-motion #'evil-mc-save-keys-operator)))

(provide 'evil-mc)

;;; evil-mc.el ends here
