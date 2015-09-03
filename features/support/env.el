(require 'f)

(defvar evil-multiple-cursors-support-path
  (f-dirname load-file-name))

(defvar evil-multiple-cursors-features-path
  (f-parent evil-multiple-cursors-support-path))

(defvar evil-multiple-cursors-root-path
  (f-parent evil-multiple-cursors-features-path))

(add-to-list 'load-path evil-multiple-cursors-root-path)

(require 'evil)
;; (require 'emc-mode)
(require 'emc-scratch)
(require 'espuds)
(require 'ert)

(Setup
 ;; Before anything has run
 )

(Before
 (switch-to-buffer (get-buffer-create "*emc*"))
 (evil-mode 1)
 (emc-init-mode)
 (erase-buffer)
 (transient-mark-mode 1)
 (deactivate-mark))

(After
 (emc-remove-hooks)
 )

(Teardown
 ;; After when everything has been run
 )
