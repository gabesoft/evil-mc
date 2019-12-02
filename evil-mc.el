;;; evil-mc.el --- Multiple cursors for evil-mode

;; Copyright © 2015-2016 Gabriel Adomnicai <gabesoft@gmail.com>

;; Author: Gabriel Adomnicai <gabesoft@gmail.com>
;; Maintainer: Gabriel Adomnicai <gabesoft@gmail.com>
;; Version: 0.0.4
;; Keywords: evil editing multiple-cursors vim evil-multiple-cursors evil-mc evil-mc
;; Homepage: https://github.com/gabesoft/evil-mc
;; Package-Requires: ((emacs "24.3") (evil "1.2.14") (cl-lib "0.5"))
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
(require 'evil-mc-undo)
(require 'evil-mc-cursor-state)
(require 'evil-mc-cursor-make)
(require 'evil-mc-command-record)
(require 'evil-mc-command-execute)
(require 'evil-mc-region)

(defun evil-mc-active-mode-line (prefix)
  "Get the mode-line text to be displayed when there are active cursors"
  (let ((mode-line-text
         (concat mode-line-text-prefix
                 (when (and (evil-mc-frozen-p)
                            evil-mc-mode-line-text-paused)
                   "(paused)")
                 (format ":%d" (evil-mc-get-cursor-count)))))
    ;; mode line text colors
    (cond ((and (evil-mc-frozen-p)
                evil-mc-mode-line-text-inverse-colors)
           (propertize mode-line-text 'face '(:inverse-video t)))
          ;; resumed (unfrozen) cursors
          (evil-mc-mode-line-text-cursor-color
           (propertize
            mode-line-text
            'face
            '(:inherit cursor :foreground "black" :distant-foreground "white")))
          ;; default colors
          (t mode-line-text))))

(defcustom evil-mc-mode-line
  `(:eval
    (let ((mode-line-text-prefix (concat " " evil-mc-mode-line-prefix)))
      (if (> (evil-mc-get-cursor-count) 1)
          (evil-mc-active-mode-line mode-line-text-prefix)
        (when evil-mc-one-cursor-show-mode-line-text
          mode-line-text-prefix))))
  "The evil-mc mode line text. It shows the number of cursors,
 when there are more than one and whether the cursors are paused."
  :group 'evil-mc
  :type '(string)
  :risky t)

(defvar evil-mc-cursors-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "m") 'evil-mc-make-all-cursors)
    (define-key map (kbd "u") 'evil-mc-undo-last-added-cursor)
    (define-key map (kbd "q") 'evil-mc-undo-all-cursors)
    (define-key map (kbd "s") 'evil-mc-pause-cursors)
    (define-key map (kbd "r") 'evil-mc-resume-cursors)
    (define-key map (kbd "f") 'evil-mc-make-and-goto-first-cursor)
    (define-key map (kbd "l") 'evil-mc-make-and-goto-last-cursor)
    (define-key map (kbd "h") 'evil-mc-make-cursor-here)
    (define-key map (kbd "j") 'evil-mc-make-cursor-move-next-line)
    (define-key map (kbd "k") 'evil-mc-make-cursor-move-prev-line)
    (define-key map (kbd "N") 'evil-mc-skip-and-goto-next-cursor)
    (define-key map (kbd "P") 'evil-mc-skip-and-goto-prev-cursor)
    (define-key map (kbd "n") 'evil-mc-skip-and-goto-next-match)
    (define-key map (kbd "p") 'evil-mc-skip-and-goto-prev-match)
    (define-key map (kbd "I") 'evil-mc-make-cursor-in-visual-selection-beg)
    (define-key map (kbd "A") 'evil-mc-make-cursor-in-visual-selection-end)
    map))

(defvar evil-mc-key-map
  (let ((map (make-sparse-keymap)))
    (evil-define-key* '(normal visual) map
                      (kbd "gr") evil-mc-cursors-map
                      (kbd "M-n") 'evil-mc-make-and-goto-next-cursor
                      (kbd "M-p") 'evil-mc-make-and-goto-prev-cursor
                      (kbd "C-n") 'evil-mc-make-and-goto-next-match
                      (kbd "C-t") 'evil-mc-skip-and-goto-next-match
                      (kbd "C-p") 'evil-mc-make-and-goto-prev-match)
    map))

;;;###autoload
(define-minor-mode evil-mc-mode
  "Toggle evil multiple cursors in a single buffer."
  :group 'evil-mc
  :init-value nil
  :keymap evil-mc-key-map
  :lighter evil-mc-mode-line
  (cond (evil-mc-mode
         (evil-mc-define-vars)
         (evil-mc-initialize-vars)
         (evil-mc-initialize-hooks))
        (t
         (evil-mc-teardown-hooks)))
  (evil-normalize-keymaps))

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

  (defvar evil-mc-enable-bar-cursor t
    "Flag that determines whether to attempt to display the fake cursors
as bar when the real cursor is of type `bar'")

  (defvar evil-mc-mode-line-prefix "emc"
    "The string used in the mode line to identify `evil-mc-mode'.")

  (defvar evil-mc-incompatible-minor-modes
    '(aggressive-indent-mode
      flycheck-mode
      flyspell-mode
      haskell-indent-mode
      haskell-indentation-mode
      yas-minor-mode)
    "Minor modes that are incompatible with `evil-mc-mode'.
These modes will be paused while the cursors are active.")

  (defvar evil-mc-custom-known-commands nil
    "Custom command handlers. The entries here should have
the same form as those in `evil-mc-known-commands'.
This variable can be used to override default command handlers
implementations.")

  (defvar evil-mc-undo-cursors-on-keyboard-quit nil
    "Flag that determines whether to delete all cursors on `keyboard-quit'."))

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
  (evil-mc-clear-command)
  (evil-mc-clear-executing-command)
  (evil-mc-clear-recording-command)
  (add-hook 'pre-command-hook 'evil-mc-begin-command-save nil t)
  (add-hook 'post-command-hook 'evil-mc-finish-command-save t t)
  (add-hook 'post-command-hook 'evil-mc-execute-for-all t t)

  (defadvice evil-repeat-keystrokes (before evil-mc-repeat-keystrokes (flag) activate)
    (evil-mc-save-keys-motion flag))
  (defadvice evil-repeat-motion (before evil-mc-repeat-motion (flag) activate)
    (evil-mc-save-keys-operator flag))
  (when evil-mc-undo-cursors-on-keyboard-quit
    (defadvice keyboard-quit (before evil-mc-keyboard-quit activate)
      (evil-mc-undo-all-cursors))))

(defun evil-mc-teardown-active-state ()
  "Teardown all variables and hooks used while there are active cursors."
  (remove-hook 'pre-command-hook 'evil-mc-begin-command-save t)
  (remove-hook 'post-command-hook 'evil-mc-finish-command-save t)
  (remove-hook 'post-command-hook 'evil-mc-execute-for-all t)

  (ad-remove-advice 'evil-repeat-keystrokes 'before 'evil-mc-repeat-keystrokes)
  (ad-remove-advice 'evil-repeat-motion 'before 'evil-mc-repeat-motion)
  (when evil-mc-undo-cursors-on-keyboard-quit
    (ad-remove-advice 'keyboard-quit 'before 'evil-mc-keyboard-quit)))

(provide 'evil-mc)

;;; evil-mc.el ends here
