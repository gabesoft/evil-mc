;;; evil-mc.el --- Multiple cursors minor mode for evil

;; Author: Gabriel Adomnicai <gabesoft@gmail.com>
;; Version: 0.0.1
;; Keywords: evil editing cursors vim evil-multiple-cursors evil-mc evil-mc
;; Package-Requires ((emacs "24") (evil "1.2.5"))
;; Homepage: https://github.com/gabesoft/evil-mc
;;
;; This file is not part of GNU Emacs.

;;; Commentary:

(require 'evil)

(require 'evil-mc-common)
(require 'evil-mc-vars)
(require 'evil-mc-cursor-state)
(require 'evil-mc-cursor-make)
(require 'evil-mc-command-record)
(require 'evil-mc-command-execute)
(require 'evil-mc-region)

(eval-when-compile (require 'cl-lib))

;;; Code:

(defgroup evil-mc nil
  "Multiple cursors implementation for evil mode."
  :prefix "evil-mc-"
  :group 'evil)

(defcustom evil-mc-mode-line
  `(:eval (if (> (evil-mc-get-cursor-count) 1)
              (format ,(propertize " %s:%d" 'face 'cursor)
                      evil-mc-mode-line-prefix
                      (evil-mc-get-cursor-count))
            (format ,(propertize " %s") evil-mc-mode-line-prefix)))
  "Cursors indicator in the mode line."
  :group 'evil-mc)

(put 'evil-mc-mode-line 'risky-local-variable t)

(define-minor-mode evil-mc-mode
  "Minor mode for evil multiple cursors in a single uuffer."
  :group 'evil-mc
  :init-value nil
  :lighter evil-mc-mode-line
  (cond (evil-mc-mode
         (evil-mc-initialize-vars)
         (evil-mc-initialize-keys)
         (evil-mc-initialize-hooks))
        (t
         (evil-mc-teardown-keys)
         (evil-mc-teardown-hooks))))

(put 'evil-mc-mode 'permanent-local t)

(define-globalized-minor-mode global-evil-mc-mode
  evil-mc-mode evil-mc-initialize)

(defun evil-mc-initialize ()
  "Enable `evil-mc-mode' in the current buffer."
  (evil-mc-mode 1))

(defun turn-on-evil-mc-mode (&optional arg)
  "Turn on evil-mc mode in the current buffer."
  (interactive)
  (evil-mc-mode (or arg 1)))

(defun turn-off-evil-mc-mode (&optional arg)
  "Turn off evil-mc mode in the current buffer."
  (interactive)
  (evil-mc-mode (or arg -1)))

(defun evil-mc-initialize-vars ()
  "Initialize all variables used by `evil-mc'."

  (defvar evil-mc-mode-line-prefix "emc"
    "The string used in the mode line to identify `evil-mc-mode'.")

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
  (when (bound-and-true-p evil-mode)
    (dolist (key evil-mc-keys)
      (evil-mc-define-key 'evil-normal-state-local-map (kbd (car key)) nil)
      (evil-mc-define-key 'evil-visual-state-local-map (kbd (car key)) nil))))

(defun evil-mc-initialize-hooks ()
  "Initialize all hooks used by `evil-mc'."
  (when (bound-and-true-p evil-mode)
    (add-hook 'pre-command-hook 'evil-mc-begin-command-save nil t)
    (add-hook 'post-command-hook 'evil-mc-finish-command-save t t)
    (add-hook 'post-command-hook 'evil-mc-execute-for-all t t)
    (advice-add 'evil-repeat-keystrokes :before #'evil-mc-save-keys-motion)
    (advice-add 'evil-repeat-motion :before #'evil-mc-save-keys-operator)))

(defun evil-mc-teardown-hooks ()
  "Teardown all hooks used by `evil-mc'."
  (when (bound-and-true-p evil-mode)
    (remove-hook 'pre-command-hook 'evil-mc-begin-command-save t)
    (remove-hook 'post-command-hook 'evil-mc-finish-command-save t)
    (remove-hook 'post-command-hook 'evil-mc-execute-for-all t)
    (advice-remove 'evil-repeat-keystrokes #'evil-mc-save-keys-motion)
    (advice-remove 'evil-repeat-motion #'evil-mc-save-keys-operator)))

(provide 'evil-mc)

;;; evil-mc.el ends here
