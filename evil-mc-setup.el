;;; evil-mc-setup.el --- Sample setup for evil-mc

;;; Commentary: Example of setting up evil-mc

;;; Code:

(when (fboundp 'add-to-load-path)
  (add-to-load-path (file-name-directory (buffer-file-name))))

(require 'evil-mc)

(evil-define-local-var evil-mc-paused-modes nil
  "Modes paused before the cursors have been created.")

(defun evil-mc-before-cursors-setup-hook ()
  "Hook to run before any cursor is created."
  (when evil-mc-executing-debug (message "Before cursors hook"))
  (setq evil-mc-paused-modes nil)
  (when (bound-and-true-p flyspell-mode)
    (flyspell-mode -1)
    (push (lambda () (flyspell-mode 1)) evil-mc-paused-modes))
  (when (bound-and-true-p aggressive-indent-mode)
    (aggressive-indent-mode -1)
    (push (lambda () (aggressive-indent-mode 1)) evil-mc-paused-modes))
  (when (bound-and-true-p evil-jumper-mode)
    (evil-jumper-mode 0)
    (push (lambda () (evil-jumper-mode t)) evil-mc-paused-modes))
  (when (bound-and-true-p yas-minor-mode)
    (yas-minor-mode 0)
    (push (lambda () (yas-minor-mode t)) evil-mc-paused-modes ))
  (when (or (bound-and-true-p web-mode) (eq major-mode 'web-mode))
    (smartchr/undo-web-mode)
    (push (lambda () (smartchr/init-web-mode)) evil-mc-paused-modes))
  (when (or (bound-and-true-p js2-mode) (eq major-mode 'js2-mode))
    (smartchr/undo-js2-mode)
    (push (lambda () (smartchr/init-js2-mode)) evil-mc-paused-modes)))

(defun evil-mc-after-cursors-teardown-hook ()
  "Hook to run after all cursors are deleted."
  (when evil-mc-executing-debug (message "After cursors hook %s" evil-mc-paused-modes))
  (dolist (fn evil-mc-paused-modes) (funcall fn))
  (setq evil-mc-paused-modes nil))

(add-hook 'evil-mc-after-cursors-deleted 'evil-mc-after-cursors-teardown-hook)
(add-hook 'evil-mc-before-cursors-created 'evil-mc-before-cursors-setup-hook)

;; (evil-mc-mode 1)
;; (global-evil-mc-mode 1)

;; (evil-mc-mode -1)
;; (global-evil-mc-mode -1)