;; This file contains your project specific step definitions. All
;; files in this directory whose names end with "-steps.el" will be
;; loaded automatically by Ecukes.

(Then "^The recorded command name should be \"\\([^\"]+\\)\"$"
      (lambda (cmd)
        (should (eq (emc-get-command-name) (intern cmd)))))

(And "^The recorded command keys should be \"\\([^\"]+\\)\"$"
     (lambda (keys)
       (should (equal (emc-get-command-keys-string :keys) keys))))

(Given "^I have at least one cursor$"
       (lambda ()
         (emc-add-cursor (emc-draw-cursor-at-point))))

(Given "^I have one cursor at \"\\([^\"]+\\)\" in \"\\([^\"]+\\)\"$"
       (lambda (pattern text)
         (insert text)
         (goto-char (point-min))
         (search-forward pattern)
         (emc-add-cursor (emc-draw-cursor-at-point))))

(When "^These examples should pass:$"
      (lambda (table)
        (let ((header (car table))
              (rows (cdr table)))
          (dolist (row rows)
            (let ((key (nth 0 row))
                  (cmd (nth 1 row)))
              (execute-kbd-macro key)
              ;; (When "I insert \"%s\"" key)
              ;; (Then "The recorded command name should be \"%s\"" cmd)
              ;; (Then "The recorded command keys should be \"%s\"" key)
              (should (equal (emc-get-command-keys-string :keys) key))
              (should (equal (emc-get-command-name) (intern cmd)))
              )))))

(When "^These examples with undo should pass:$"
      (lambda (table)
        (let ((header (car table))
              (rows (cdr table)))
          (dolist (row rows)
            (let ((key (nth 0 row))
                  (cmd (nth 1 row)))
              (When "I press \"%s\"" key)
              (Then "The recorded command name should be \"%s\"" cmd)
              (Then "The recorded command keys should be \"%s\"" key)
              (evil-force-normal-state)
              (execute-kbd-macro "u"))))))

(And "^I go to the beginning of buffer$"
     (lambda ()
       (goto-char (point-min))))

(And "^The cursors are frozen$"
     (lambda ()
       (emc-freeze)))
