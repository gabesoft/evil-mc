;;; evil-mc-setup.el --- Sample setup for evil-mc

;;; Commentary: Example of setting up evil-mc

;;; Code:

(when (fboundp 'add-to-load-path)
  (add-to-load-path (file-name-directory (buffer-file-name))))

(require 'evil-mc)

(evil-define-local-var evil-mc-custom-paused nil
  "Paused functionality when there are multiple cursors active.")

(defun evil-mc-before-cursors-setup-hook ()
  "Hook to run before any cursor is created.
Can be used to temporarily disable any functionality that doesn't
play well with `evil-mc'."
  (when (or (bound-and-true-p web-mode) (eq major-mode 'web-mode))
    (smartchr/undo-web-mode)
    (push (lambda () (smartchr/init-web-mode)) evil-mc-custom-paused))
  (when (or (bound-and-true-p js2-mode) (eq major-mode 'js2-mode))
    (smartchr/undo-js2-mode)
    (push (lambda () (smartchr/init-js2-mode)) evil-mc-custom-paused)))

(defun evil-mc-after-cursors-teardown-hook ()
  "Hook to run after all cursors are deleted."
  (dolist (fn evil-mc-custom-paused) (funcall fn))
  (setq evil-mc-custom-paused nil))

(add-hook 'evil-mc-before-cursors-created 'evil-mc-before-cursors-setup-hook)
(add-hook 'evil-mc-after-cursors-deleted 'evil-mc-after-cursors-teardown-hook)

(defvar evil-mc-mode-line-prefix "ⓜ"
  "Override of the default mode line string for `evil-mc-mode'.")

(global-evil-mc-mode 1)

;; (evil-mc-mode 1)
;; (global-evil-mc-mode 1)

;; (evil-mc-mode -1)
;; (global-evil-mc-mode -1)