(require 'f)

(defvar evil-multiple-cursors-support-path
  (f-dirname load-file-name))

(defvar evil-multiple-cursors-features-path
  (f-parent evil-multiple-cursors-support-path))

(defvar evil-multiple-cursors-root-path
  (f-parent evil-multiple-cursors-features-path))

(add-to-list 'load-path evil-multiple-cursors-root-path)

(require 'evil-multiple-cursors)
(require 'espuds)
(require 'ert)

(Setup
 ;; Before anything has run
 )

(Before
 ;; Before each scenario is run
 )

(After
 ;; After each scenario is run
 )

(Teardown
 ;; After when everything has been run
 )
