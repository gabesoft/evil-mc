;;; evil-mc-setup.el --- Sample setup for evil-mc

;;; Commentary: Example of setting up evil-mc

;;; Code:

(when (fboundp 'add-to-load-path)
  (add-to-load-path (file-name-directory (buffer-file-name))))

(require 'evil-mc)

(evil-define-local-var evil-mc-custom-paused nil
  "Paused functionality when there are multiple cursors active.")

(defun evil-mc-pause-smartchr-for-mode (mode)
  "Temporarily disables the smartchr keys for MODE."
  (let ((init (intern (concat "smartchr/init-" (symbol-name mode))))
        (undo (intern (concat "smartchr/undo-" (symbol-name mode)))))
    (when (eq major-mode mode)
      (funcall undo)
      (push `(lambda () (,init)) evil-mc-custom-paused))))

(defun evil-mc-before-cursors-setup-hook ()
  "Hook to run before any cursor is created.
Can be used to temporarily disable any functionality that doesn't
play well with `evil-mc'."
  (mapc 'evil-mc-pause-smartchr-for-mode
        '(web-mode js2-mode java-mode css-mode)))

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
