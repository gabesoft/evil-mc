(require 'f)

(defvar evil-multiple-cursors-support-path
  (f-dirname load-file-name))

(defvar evil-multiple-cursors-features-path
  (f-parent evil-multiple-cursors-support-path))

(defvar evil-multiple-cursors-root-path
  (f-parent evil-multiple-cursors-features-path))

(add-to-list 'load-path evil-multiple-cursors-root-path)

(require 'cl)
(require 'evil)
(require 'evil-surround)
(require 'evil-numbers)
(require 'evil-mc)
(require 'evil-mc-scratch)
(require 'espuds)
(require 'ert)

(Setup
 ;; Before anything has run
 )

(Before
 (switch-to-buffer (get-buffer-create "*evil-mc*"))
 (evil-surround-mode 1)
 (evil-mode 1)
 (evil-mc-mode 1)
 (erase-buffer)
 (transient-mark-mode 1)
 (deactivate-mark))

(After
 (evil-mc-mode -1)
 )

(Teardown
 ;; After when everything has been run
 )
