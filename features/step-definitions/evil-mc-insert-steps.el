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
