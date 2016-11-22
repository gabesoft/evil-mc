;;; evil-mc-scratch.el --- Functions used during development

;;; Commentary:

;;; Code:

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

(defun evil-mc-insert-current-date-at-each-cursor ()
  "Insert the current date at each cursor position."
  (interactive)
  (evil-mc-execute-for-all-cursors
   (lambda (cursor)
     (insert
      (format-time-string "%d/%m/%Y" (current-time))))))

(provide 'evil-mc-scratch)

;;; evil-mc-scratch.el ends here
