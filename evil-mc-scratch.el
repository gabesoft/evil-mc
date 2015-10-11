;;; evil-mc-scratch.el --- Functions used during development

;;; Commentary:

;;; Code:

;; (transpose-chars-before-point 1)
(transpose-chars -1)

(progn
  ;; (evil-forward-char)
  (transpose-chars -1))

;; (global-evil-mc-mode  1)
;; (global-evil-mc-mode -1)

;; (evil-mc-mode  1)
;; (evil-mc-mode -1)

(defun evil-mc-clear-buffer-undo-list ()
  "Clear the `buffer-undo-list'."
  (interactive)
  (setq buffer-undo-list nil))

(defun evil-mc-clear-buffer-undo-tree ()
  "Clear the `buffer-undo-tree'."
  (interactive)
  (setq buffer-undo-tree nil))

(defun evil-mc-remove-all-overlays ()
  "Remove all overlays."
  (interactive)
  (remove-overlays)
  (setq evil-mc-cursor-list nil))

(provide 'evil-mc-scratch)

;;; evil-mc-scratch.el ends here