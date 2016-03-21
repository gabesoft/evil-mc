(And "^I press \"\\([^\"]+\\)\" followed by enter$"
     "Press ARG followed by a new line."
     (lambda (arg)
       (execute-kbd-macro
        (vconcat (edmacro-parse-keys arg)
                 (edmacro-parse-keys "<return>")))))

(When "^I replace the buffer text with\\(?: \"\\(.+\\)\"\\|:\\)$"
      "Replace the current buffer text with CONTENTS.
Also enter normal state and go to the beginning of buffer."
      (lambda (contents)
        (erase-buffer)
        (evil-normal-state)
        (insert contents)
        (goto-char (point-min))))

(And "^I set the register to \"\\([^\"]+\\)\" then type \"\\([^\"]+\\)\"$"
     (lambda (register input)
       (execute-kbd-macro (vconcat [34]
                                   (string-to-vector register)
                                   (string-to-vector input)))))

(Then "^The recorded command name should be \"\\([^\"]+\\)\"$"
      (lambda (cmd)
        (should (eq (evil-mc-get-command-name) (intern cmd)))))

(And "^The recorded command keys should be \"\\([^\"]+\\)\"$"
     (lambda (keys)
       (should (equal (evil-mc-get-command-keys-string :keys) keys))))

(Given "^I have at least one cursor$"
       (lambda ()
         (evil-mc-make-cursor-here)))

(Given "^I have one cursor at \"\\([^\"]+\\)\" in \"\\([^\"]+\\)\"$"
       (lambda (pattern text)
         (insert text)
         (goto-char (point-min))
         (search-forward pattern)
         (evil-mc-make-cursor-here)))

(When "^These examples should pass:$"
      (lambda (table) (run-and-verify table)))

(When "^These examples with undo should pass:$"
      (lambda (table) (run-and-verify table t)))

(And "^I go to the beginning of buffer$"
     (lambda () (goto-char (point-min))))

(And "^The cursors are frozen$"
     (lambda () (evil-mc-pause-cursors)))

(And "^I enable debugging$"
     (lambda() (evil-mc-recording-debug-on)))

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
        (evil-force-normal-state)
        (When "I press \"%s\"" key)
        (Then "The recorded command name should be \"%s\"" cmd)
        (Then "The recorded command keys should be \"%s\"" out)
        (when undo
          (evil-force-normal-state)
          (When "I press \"u\""))))))
