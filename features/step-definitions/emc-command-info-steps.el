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
      (lambda (table) (run-and-verify table)))

(When "^These examples with undo should pass:$"
      (lambda (table) (run-and-verify table t)))

(And "^I go to the beginning of buffer$"
     (lambda () (goto-char (point-min))))

(And "^The cursors are frozen$"
     (lambda () (emc-freeze)))

(defun run-and-verify (table &optional undo)
  "Runs all the key sequences in TABLE and verifies them,
optionally running UNDO after each one."
  (let ((header (car table))
        (rows (cdr table)))
    (dolist (row rows)
      (let* ((key (nth 0 row))
             (cmd (nth 1 row))
             (out (nth 2 row)))
        (when (or (null out) (eq 0 (length out))) (setq out key))
        (When "I press \"%s\"" key)
        (Then "The recorded command name should be \"%s\"" cmd)
        (Then "The recorded command keys should be \"%s\"" out)
        (when undo
          (When "I press \"ESC\"")
          (When "I press \"u\""))))))