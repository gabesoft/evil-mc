;;; evil-mc-mode.el --- Multiple cursors minor mode for evil

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

(define-minor-mode evil-mc-mode
  "Minor mode for evil multiple cursors in a single buffer."
  :group 'evil-mc
  :init-value nil
  :lighter " mc"
  (cond (evil-mc-mode
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

(defvar evil-mc-keys
  '(("grm" . 'evil-mc-make-all-cursors)
    ("gru" . 'evil-mc-undo-all-cursors)
    ("grp" . 'evil-mc-pause-cursors)
    ("grr" . 'evil-mc-resume-cursors)
    ("grf" . 'evil-mc-make-and-goto-first-cursor)
    ("grl" . 'evil-mc-make-and-goto-last-cursor)
    ("grh" . 'evil-mc-make-cursor-here)
    ("C-m" . 'evil-mc-make-and-goto-next-cursor)
    (",m" . 'evil-mc-skip-and-goto-next-cursor)
    ("C-l" . 'evil-mc-make-and-goto-prev-cursor)
    (",l" . 'evil-mc-skip-and-goto-prev-cursor)
    ("C-n" . 'evil-mc-make-and-goto-next-match)
    (",n" . 'evil-mc-skip-and-goto-next-match)
    ("C-t" . 'evil-mc-skip-and-goto-next-match)
    ("C-p" . 'evil-mc-make-and-goto-prev-match)
    (",p" . 'evil-mc-skip-and-goto-prev-match))
  "Association list of key maps.
Entries have the form (KEY . DEF), where KEY is the key
that would trigger the evil-mc DEF. The keys defined here
will be set up in `normal' and `visual' mode.")

(defun evil-mc-initialize-keys ()
  "Initialize the `evil-mc' keys."
  (dolist (key evil-mc-keys)
    (evil-local-set-key 'normal (kbd (car key)) (cdr key))
    (evil-local-set-key 'visual (kbd (car key)) (cdr key))))

(defun evil-mc-teardown-keys ()
  "Initialize the `evil-mc' keys."
  (dolist (key evil-mc-keys)
    (evil-local-set-key 'normal (kbd (car key)) nil)
    (evil-local-set-key 'visual (kbd (car key)) nil)))

(defun evil-mc-initialize-hooks ()
  "Initializes all hooks used by `evil-mc'."
  (message "TODO: implement"))

(defun evil-mc-teardown-hooks ()
  "Teardown all hooks used by `evil-mc'."
  (message "TODO: implement"))

(provide 'evil-mc)

;;; evil-mc.el ends here
