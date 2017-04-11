;;; evil-mc-known-commands.el --- A list of supported commands and their handlers

;;; Commentary:

;; This file contains a list of supported commands and their handlers
;; specified by evil state

;;; Code:

(defvar evil-mc-known-commands
  '((backward-delete-char-untabify . ((:default . evil-mc-execute-default-call-with-count)))
    (delete-forward-char . ((:default . evil-mc-execute-default-call-with-count)))
    (company-complete-selection . ((:default . evil-mc-execute-default-call)))
    (company-select-next . ((:default . evil-mc-execute-default-ignore)))
    (copy-to-the-end-of-line . ((:default . evil-mc-execute-default-call)))
    (delete-backward-char . ((:default . evil-mc-execute-default-call-with-count)))
    (electric-newline-and-maybe-indent . ((:default . evil-mc-execute-default-call)))
    (evil-a-WORD . ((:default . evil-mc-execute-default-call-with-count) (visual . evil-mc-execute-visual-text-object)))
    (evil-a-back-quote . ((:default . evil-mc-execute-default-call-with-count) (visual . evil-mc-execute-visual-text-object)))
    (evil-a-bracket . ((:default . evil-mc-execute-default-call-with-count) (visual . evil-mc-execute-visual-text-object)))
    (evil-a-curly . ((:default . evil-mc-execute-default-call-with-count) (visual . evil-mc-execute-visual-text-object)))
    (evil-a-double-quote . ((:default . evil-mc-execute-default-call-with-count) (visual . evil-mc-execute-visual-text-object)))
    (evil-a-paragraph . ((:default . evil-mc-execute-default-call-with-count) (visual . evil-mc-execute-visual-text-object)))
    (evil-a-paren . ((:default . evil-mc-execute-default-call-with-count) (visual . evil-mc-execute-visual-text-object)))
    (evil-a-sentence . ((:default . evil-mc-execute-default-call-with-count) (visual . evil-mc-execute-visual-text-object)))
    (evil-a-single-quote . ((:default . evil-mc-execute-default-call-with-count) (visual . evil-mc-execute-visual-text-object)))
    (evil-a-symbol . ((:default . evil-mc-execute-default-call-with-count) (visual . evil-mc-execute-visual-text-object)))
    (evil-a-tag . ((:default . evil-mc-execute-default-call-with-count) (visual . evil-mc-execute-visual-text-object)))
    (evil-a-word . ((:default . evil-mc-execute-default-call-with-count) (visual . evil-mc-execute-visual-text-object)))
    (evil-an-angle . ((:default . evil-mc-execute-default-call-with-count) (visual . evil-mc-execute-visual-text-object)))
    (evil-append . ((:default . evil-mc-execute-default-evil-insert-state)))
    (evil-append-line . ((:default . evil-mc-execute-default-evil-insert-state)))
    (evil-beginning-of-line . ((:default . evil-mc-execute-default-call) (visual . evil-mc-execute-visual-call)))
    (evil-beginning-of-line-or-digit-argument . ((:default . evil-mc-execute-default-call) (visual . evil-mc-execute-visual-call)))
    (evil-beginning-of-visual-line . ((:default . evil-mc-execute-default-call) (visual . evil-mc-execute-visual-call)))
    (evil-change . ((:default . evil-mc-execute-default-evil-change)))
    (evil-change-line . ((:default . evil-mc-execute-default-evil-change-line)))
    (evil-commentary . ((:default . evil-mc-execute-default-evil-commentary)))
    (evil-complete-next . ((:default . evil-mc-execute-default-complete)))
    (evil-complete-next-line . ((:default . evil-mc-execute-default-complete)))
    (evil-complete-previous . ((:default . evil-mc-execute-default-complete)))
    (evil-complete-previous-line . ((:default . evil-mc-execute-default-complete)))
    (evil-delete . ((:default . evil-mc-execute-default-evil-delete)))
    (evil-delete-backward-char-and-join . ((:default . evil-mc-execute-default-call-with-count)))
    (evil-delete-backward-word . ((:default . evil-mc-execute-default-call)))
    (evil-delete-backward-char . ((:default . evil-mc-execute-default-evil-delete)))
    (evil-delete-char . ((:default . evil-mc-execute-default-evil-delete)))
    (evil-delete-line . ((:default . evil-mc-execute-default-evil-delete)))
    (evil-digit-argument-or-evil-beginning-of-line . ((:default . evil-mc-execute-default-call) (visual . evil-mc-execute-visual-call)))
    (evil-downcase . ((:default . evil-mc-execute-default-change-case)))
    (evil-exchange . ((:default . evil-mc-execute-default-evil-exchange)))
    (evil-exchange-cancel . ((:default . evil-mc-execute-default-call)))
    (evil-exchange-point-and-mark . ((visual . evil-mc-execute-visual-exchange-point-and-mark)))
    (evil-exit-visual-state . ((:default . evil-mc-execute-default-call) (visual . evil-mc-execute-visual-call)))
    (evil-find-char . ((:default . evil-mc-execute-default-evil-find-char) (visual . evil-mc-execute-visual-evil-find-char)))
    (evil-find-char-backward . ((:default . evil-mc-execute-default-evil-find-char) (visual . evil-mc-execute-visual-evil-find-char)))
    (evil-find-char-to . ((:default . evil-mc-execute-default-evil-find-char) (visual . evil-mc-execute-visual-evil-find-char)))
    (evil-find-char-to-backward . ((:default . evil-mc-execute-default-evil-find-char) (visual . evil-mc-execute-visual-evil-find-char)))
    (evil-first-non-blank . ((:default . evil-mc-execute-default-call) (visual . evil-mc-execute-visual-call)))
    (evil-first-non-blank-of-visual-line . ((:default . evil-mc-execute-default-call) (visual . evil-mc-execute-visual-call)))
    (evil-goto-definition . ((:default . evil-mc-execute-default-call) (visual . evil-mc-execute-visual-call)))
    (evil-goto-line . ((:default . evil-mc-execute-default-evil-goto-line) (visual . evil-mc-execute-visual-evil-goto-line)))
    (evil-goto-mark . ((:default . evil-mc-execute-default-call-with-last-input) (visual . evil-mc-execute-visual-call-with-last-input)))
    (evil-goto-mark-line . ((:default . evil-mc-execute-default-call-with-last-input) (visual . evil-mc-execute-visual-call-with-last-input)))
    (evil-inner-WORD . ((:default . evil-mc-execute-default-call-with-count) (visual . evil-mc-execute-visual-text-object)))
    (evil-inner-angle . ((:default . evil-mc-execute-default-call-with-count) (visual . evil-mc-execute-visual-text-object)))
    (evil-inner-back-quote . ((:default . evil-mc-execute-default-call-with-count) (visual . evil-mc-execute-visual-text-object)))
    (evil-inner-bar . ((:default . evil-mc-execute-default-call-with-count) (visual . evil-mc-execute-visual-text-object)))
    (evil-inner-block-star . ((:default . evil-mc-execute-default-call-with-count) (visual . evil-mc-execute-visual-text-object)))
    (evil-inner-bracket . ((:default . evil-mc-execute-default-call-with-count) (visual . evil-mc-execute-visual-text-object)))
    (evil-inner-curly . ((:default . evil-mc-execute-default-call-with-count) (visual . evil-mc-execute-visual-text-object)))
    (evil-inner-dollar . ((:default . evil-mc-execute-default-call-with-count) (visual . evil-mc-execute-visual-text-object)))
    (evil-inner-double-quote . ((:default . evil-mc-execute-default-call-with-count) (visual . evil-mc-execute-visual-text-object)))
    (evil-inner-paragraph . ((:default . evil-mc-execute-default-call-with-count) (visual . evil-mc-execute-visual-text-object)))
    (evil-inner-paren . ((:default . evil-mc-execute-default-call-with-count) (visual . evil-mc-execute-visual-text-object)))
    (evil-inner-percent . ((:default . evil-mc-execute-default-call-with-count) (visual . evil-mc-execute-visual-text-object)))
    (evil-inner-sentence . ((:default . evil-mc-execute-default-call-with-count) (visual . evil-mc-execute-visual-text-object)))
    (evil-inner-single-quote . ((:default . evil-mc-execute-default-call-with-count) (visual . evil-mc-execute-visual-text-object)))
    (evil-inner-star . ((:default . evil-mc-execute-default-call-with-count) (visual . evil-mc-execute-visual-text-object)))
    (evil-inner-symbol . ((:default . evil-mc-execute-default-call-with-count) (visual . evil-mc-execute-visual-text-object)))
    (evil-inner-tag . ((:default . evil-mc-execute-default-call-with-count) (visual . evil-mc-execute-visual-text-object)))
    (evil-inner-word . ((:default . evil-mc-execute-default-call-with-count) (visual . evil-mc-execute-visual-text-object)))
    (evil-insert . ((:default . evil-mc-execute-default-evil-insert-state)))
    (evil-insert-line . ((:default . evil-mc-execute-default-call-with-count)))
    (evil-invert-case . ((:default . evil-mc-execute-default-change-case)))
    (evil-invert-char . ((:default . evil-mc-execute-default-change-case)))
    (evil-join . ((:default . evil-mc-execute-default-evil-join)))
    (evil-jump-item . ((:default . evil-mc-execute-default-call) (visual . evil-mc-execute-visual-call)))
    (evil-lookup . ((:default . evil-mc-execute-default-call) (visual . evil-mc-execute-visual-call)))
    (evil-middle-of-visual-line . ((:default . evil-mc-execute-default-call) (visual evil-mc-execute-visual-call)))
    (evil-next-line . ((:default . evil-mc-execute-default-call-with-count) (visual . evil-mc-execute-visual-call-with-count)))
    (evil-next-match . ((:default . evil-mc-execute-default-call-with-count) (visual . evil-mc-execute-visual-text-object)))
    (evil-normal-state . ((:default . evil-mc-execute-default-evil-normal-state)))
    (evil-open-above . ((:default . evil-mc-execute-default-call-with-count)))
    (evil-open-below . ((:default . evil-mc-execute-default-call-with-count)))
    (evil-outer-bar . ((:default . evil-mc-execute-default-call-with-count) (visual . evil-mc-execute-visual-text-object)))
    (evil-outer-block-star . ((:default . evil-mc-execute-default-call-with-count) (visual . evil-mc-execute-visual-text-object)))
    (evil-outer-dollar . ((:default . evil-mc-execute-default-call-with-count) (visual . evil-mc-execute-visual-text-object)))
    (evil-outer-percent . ((:default . evil-mc-execute-default-call-with-count) (visual . evil-mc-execute-visual-text-object)))
    (evil-outer-star . ((:default . evil-mc-execute-default-call-with-count) (visual . evil-mc-execute-visual-text-object)))
    (evil-paste-after . ((:default . evil-mc-execute-default-evil-paste)))
    (evil-paste-before . ((:default . evil-mc-execute-default-evil-paste)))
    (evil-paste-from-register . ((:default . evil-mc-execute-default-macro)))
    (evil-paste-pop . ((:default . evil-mc-execute-default-not-supported)))
    (evil-paste-pop-next . ((:default . evil-mc-execute-default-not-supported)))
    (evil-previous-line . ((:default . evil-mc-execute-default-call-with-count) (visual . evil-mc-execute-visual-call-with-count)))
    (evil-previous-match . ((:default . evil-mc-execute-default-call-with-count) (visual . evil-mc-execute-visual-text-object)))
    (evil-repeat . ((:default . evil-mc-execute-default-evil-repeat)))
    (evil-repeat-pop . ((:default . evil-mc-execute-default-call-with-count)))
    (evil-repeat-pop-next . ((:default . evil-mc-execute-default-call-with-count)))
    (evil-replace . ((:default . evil-mc-execute-default-evil-replace)))
    (evil-replace-backspace . ((replace . evil-mc-execute-default-call)))
    (evil-search-backward . ((:default . evil-mc-execute-default-call) (visual . evil-mc-execute-visual-call)))
    (evil-search-forward . ((:default . evil-mc-execute-default-call) (visual . evil-mc-execute-visual-call)))
    (evil-set-marker . ((:default . evil-mc-execute-default-call-with-last-input) (visual . evil-mc-execute-visual-call-with-last-input)))
    (evil-shift-left . ((:default . evil-mc-execute-default-evil-shift-left) (visual . evil-mc-execute-visual-evil-shift-left)))
    (evil-shift-right . ((:default . evil-mc-execute-default-evil-shift-right) (visual . evil-mc-execute-visual-evil-shift-right)))
    (evil-snipe-F . ((:default . evil-mc-execute-default-evil-snipe) (visual . evil-mc-execute-visual-evil-snipe)))
    (evil-snipe-S . ((:default . evil-mc-execute-default-evil-snipe) (visual . evil-mc-execute-visual-evil-snipe)))
    (evil-snipe-T . ((:default . evil-mc-execute-default-evil-snipe) (visual . evil-mc-execute-visual-evil-snipe)))
    (evil-snipe-f . ((:default . evil-mc-execute-default-evil-snipe) (visual . evil-mc-execute-visual-evil-snipe)))
    (evil-snipe-s . ((:default . evil-mc-execute-default-evil-snipe) (visual . evil-mc-execute-visual-evil-snipe)))
    (evil-snipe-t . ((:default . evil-mc-execute-default-evil-snipe) (visual . evil-mc-execute-visual-evil-snipe)))
    (evil-snipe-repeat-reverse . ((:default . evil-mc-execute-default-evil-snipe-repeat-reverse) (visual . evil-mc-execute-visual-evil-snipe-repeat-reverse)))
    (evil-sp-change-line . ((:default . evil-mc-execute-default-evil-sp-change-line)))
    (evil-sp-delete . ((:default . evil-mc-execute-default-evil-sp-delete)))
    (evil-sp-delete-char . ((:default . evil-mc-execute-default-evil-sp-delete)))
    (evil-sp-delete-line . ((:default . evil-mc-execute-default-evil-sp-delete)))
    (evil-substitute . ((:default . evil-mc-execute-default-evil-substitute)))
    (evil-surround-region . ((:default . evil-mc-execute-default-evil-surround-region)))
    (evil-upcase . ((:default . evil-mc-execute-default-change-case)))
    (evil-use-register . ((:default . evil-mc-execute-default-call-with-last-input) (visual . evil-mc-execute-visual-call-with-last-input)))
    (evil-visual-block . ((visual . evil-mc-execute-default-not-supported)))
    (evil-visual-char . ((:default . evil-mc-execute-default-force-normal-state) (visual . evil-mc-execute-visual-char)))
    (evil-visual-exchange-corners . ((:default . evil-mc-execute-default-call) (visual . evil-mc-execute-visual-call)))
    (evil-visual-line . ((:default . evil-mc-execute-default-force-normal-state) (visual . evil-mc-execute-visual-line)))
    (evil-visual-restore . ((:default . evil-mc-execute-default-call) (visual . evil-mc-execute-visual-call)))
    (evil-window-middle . ((:default . evil-mc-execute-default-call) (visual . evil-mc-execute-visual-call)))
    (evil-yank . ((:default . evil-mc-execute-default-evil-yank)))
    (exchange-point-and-mark . ((visual . evil-mc-execute-visual-exchange-point-and-mark)))
    (hippie-expand . ((:default . evil-mc-execute-default-hippie-expand)))
    (indent-for-tab-command . ((:default . evil-mc-execute-default-call)))
    (indent-region-or-buffer . ((:default . evil-mc-execute-default-ignore)))
    (keyboard-quit . ((:default . evil-mc-execute-default-ignore)))
    (move-text-down . ((:default . evil-mc-execute-default-call-with-count)))
    (move-text-up . ((:default . evil-mc-execute-default-call-with-count)))
    (newline . ((:default . evil-mc-execute-default-call)))
    (newline-and-indent . ((:default . evil-mc-execute-default-call)))
    (paste-after-current-line . ((:default . evil-mc-execute-default-call-with-count)))
    (paste-before-current-line . ((:default . evil-mc-execute-default-call-with-count)))
    (redo . ((:default . evil-mc-execute-default-redo)))
    (self-insert-command . ((:default . evil-mc-execute-default-call-with-count)))
    (sp-backward-delete-char . ((:default . evil-mc-execute-default-call)))
    (transpose-chars-before-point . ((:default . evil-mc-execute-default-call-with-count)))
    (transpose-chars . ((:default . evil-mc-execute-default-call-with-count)))
    (undo . ((:default . evil-mc-execute-default-undo)))
    (undo-tree-undo . ((:default . evil-mc-execute-default-undo)))
    (undo-tree-redo . ((:default . evil-mc-execute-default-redo)))
    (yank . ((:default . evil-mc-execute-default-call)))

    ;; cc-mode
    (c-electric-backspace . ((:default . evil-mc-execute-default-call-with-count)))
    (c-electric-brace . ((:default . evil-mc-execute-default-call-with-count)))
    (c-electric-colon . ((:default . evil-mc-execute-default-call-with-count)))
    (c-electric-continued-statement . ((:default . evil-mc-execute-default-call)))
    (c-electric-delete . ((:default . evil-mc-execute-default-call-with-count)))
    (c-electric-delete-forward . ((:default . evil-mc-execute-default-call-with-count)))
    (c-electric-lt-gt . ((:default . evil-mc-execute-default-call-with-count)))
    (c-electric-paren . ((:default . evil-mc-execute-default-call-with-count)))
    (c-electric-pound . ((:default . evil-mc-execute-default-call-with-count)))
    (c-electric-semi&comma . ((:default . evil-mc-execute-default-call-with-count)))
    (c-electric-slash . ((:default . evil-mc-execute-default-call-with-count)))
    (c-electric-star . ((:default . evil-mc-execute-default-call-with-count)))
    (c-indent-new-comment-line . ((:default . evil-mc-execute-default-call)))

    ;; haskell
    (haskell-indentation-common-electric-command . ((:default . evil-mc-execute-default-macro)))
    (haskell-indentation-newline-and-indent . ((:default . evil-mc-execute-default-call)))
    (haskell-interactive-mode-space . ((:default . evil-mc-execute-default-call-with-count)))

    ;; ruby mode
    (ruby-tools-interpolate . ((:default . evil-mc-execute-default-call)))

    ;; shell mode
    (sh-assignment . ((:default . evil-mc-execute-default-call-with-count)))

    ;; java
    (spacemacs/java-completing-dot . ((:default . evil-mc-execute-default-call)))
    (spacemacs/java-completing-double-colon . ((:default . evil-mc-execute-default-call)))

    ;; c

    ;; python-mode
    (python-indent-dedent-line-backspace . ((:default . evil-mc-execute-default-call-with-count)))

    ;; scala
    (scala/newline-and-indent-with-asterisk . ((:default . evil-mc-execute-default-call)))
    (scala/completing-dot . ((:default . evil-mc-execute-default-call)))

    ;; org-mode
    (org-beginning-of-line . ((:default . evil-mc-execute-default-call-with-count)))
    (org-end-of-line . ((:default . evil-mc-execute-org-end-of-line)))
    (org-force-self-insert . ((:default . evil-mc-execute-default-call-with-count)))
    (org-return . ((:default . evil-mc-execute-default-call)))
    (org-self-insert-command . ((:default . evil-mc-execute-default-call-with-count)))
    (org-todo . ((:default . evil-mc-execute-default-call)))
    (orgtbl-self-insert-command . ((:default . evil-mc-execute-default-call-with-count)))
    (orgtbl-hijacker-command-100 . ((:default . evil-mc-execute-default-call-with-count)))
    (orgtbl-hijacker-command-109 . ((:default . evil-mc-execute-default-call-with-count)))
    (org-delete-backward-char . ((:default . evil-mc-execute-default-call-with-count)))

    ;; unimpaired
    (unimpaired/paste-above . ((:default . evil-mc-execute-default-call)))
    (unimpaired/paste-below . ((:default . evil-mc-execute-default-call)))

    ;; yaml
    (yaml-electric-backspace . ((:default . evil-mc-execute-default-call-with-count)))
    (yaml-electric-bar-and-angle . ((:default . evil-mc-execute-default-call-with-count)))
    (yaml-electric-dash-and-dot . ((:default . evil-mc-execute-default-call-with-count)))

    ;; evil-matchit
    (evilmi-jump-items . ((:default . evil-mc-execute-default-call)))

    ;; evil-numbers
    (evil-numbers/inc-at-pt . ((:default . evil-mc-execute-default-call-with-count)))
    (evil-numbers/dec-at-pt . ((:default . evil-mc-execute-default-call-with-count)))
    (spacemacs/evil-numbers-decrease . ((:default . evil-mc-execute-default-call-with-count)))
    (spacemacs/evil-numbers-increase . ((:default . evil-mc-execute-default-call-with-count)))

    ;; spacemacs
    (spacemacs/smart-closing-parenthesis  . ((:default . evil-mc-execute-default-call)))
    (spacemacs/evil-mc-paste-after . ((:default . evil-mc-execute-default-evil-paste)))
    (spacemacs/evil-mc-paste-before . ((:default . evil-mc-execute-default-evil-paste)))

    ;; auctex
    (TeX-insert-backslash . ((:default . evil-mc-execute-default-call-with-count)))
    (LaTeX-insert-left-brace . ((:default . evil-mc-execute-default-call-with-count)))
    (LaTeX-insert-right-brace . ((:default . evil-mc-execute-default-call-with-count)))
    (LaTeX-babel-insert-hyphen . ((:default . evil-mc-execute-default-call-with-count)))
    (TeX-insert-sub-or-superscript . ((:default . evil-mc-execute-default-call-with-count)))
    (TeX-insert-dollar . ((:default . evil-mc-execute-default-call-with-count)))

    ;; evil-cleverparens
    (evil-cp-append ; a
     (:default . evil-mc-execute-default-call-with-count))
    (evil-cp-change ; c
     (:default . evil-mc-execute-default-evil-change))
    (evil-cp-change-line ; C
     (:default . evil-mc-execute-default-evil-change-line))
    (evil-cp-delete ; d
     (:default . evil-mc-execute-default-evil-delete))
    (evil-cp-delete-line ; D
     (:default . evil-mc-execute-default-evil-delete))
    (evil-cp-change-sexp ; M-c
     (:default . evil-mc-execute-default-evil-change))
    (evil-cp-change-enclosing ; M-C
     (:default . evil-mc-execute-default-evil-change))
    (evil-cp-delete-sexp ; M-d
     (:default . evil-mc-execute-default-evil-delete))
    (evil-cp-delete-enclosing ; M-D
     (:default . evil-mc-execute-default-evil-delete))
    (evil-cp-delete-char-or-splice ; x
     (:default . evil-mc-execute-default-evil-delete))
    (evil-cp-insert ; i
     (:default . evil-mc-execute-default-call-with-count))
    (evil-cp-substitute ; s
     (:default . evil-mc-execute-default-evil-substitute))
    (evil-cp-yank ; y
     (:default . evil-mc-execute-default-evil-yank))
    (evil-cp-first-non-blank-non-opening ; _
     (:default . evil-mc-execute-default-call)
     (visual . evil-mc-execute-visual-call))
    (evil-cp-< ; <
     (:default . evil-mc-execute-default-evil-shift-left)
     (visual . evil-mc-execute-visual-shift-left))
    (evil-cp-> ; >
     (:default . evil-mc-execute-default-evil-shift-right)
     (visual . evil-mc-execute-visual-shift-right))
    (evil-cp-wrap-next-round ; M-(
     (:default . evil-mc-execute-default-call-with-count))
    (evil-cp-wrap-previous-round ; M-)
     (:default . evil-mc-execute-default-call-with-count))
    (evil-cp-wrap-next-square ; M-[
     (:default . evil-mc-execute-default-call-with-count))
    (evil-cp-wrap-previous-square ; M-]
     (:default . evil-mc-execute-default-call-with-count))
    (evil-cp-wrap-next-curly ; M-{
     (:default . evil-mc-execute-default-call-with-count))
    (evil-cp-wrap-previous-curly ; M-}
     (:default . evil-mc-execute-default-call-with-count))
    (evil-cp-open-below-form ; M-o
     (:default . evil-mc-execute-default-call-with-count))
    (evil-cp-open-above-form ; M-O
     (:default . evil-mc-execute-default-call-with-count))
    (evil-cp-insert-at-end-of-form ; M-a
     (:default . evil-mc-execute-default-call-with-count))
    (evil-cp-insert-at-beginning-of-form ; M-i
     (:default . evil-mc-execute-default-call-with-count))
    (evil-cp-copy-paste-form ; M-w
     (:default . evil-mc-execute-default-call-with-count))
    (evil-cp-drag-forward ; M-j
     (:default . evil-mc-execute-default-call-with-count))
    (evil-cp-drag-backward ; M-k
     (:default . evil-mc-execute-default-call-with-count))
    (evil-cp-raise-form ; M-R
     (:default . evil-mc-execute-default-call-with-count))
    ;; note: couldn't actually get this one to work, so I set it the same as
    ;; `evil-delete-backward-word'
    (evil-cp-delete-backward-word ; C-w in insert state
     (:default . evil-mc-execute-default-call))

    ;; not supported for now, because normal `evil-change-whole-line' also is
    ;; not supported
    ;; evil-cp-change-whole-line ; S

    ;; not supported for now, because normal `evil-yank-line' also is not
    ;; supported
    ;; evil-cp-yank-line ; Y

    ;; not supported: `evil-cp-override' needs to be called once for each cursor,
    ;; right before calling the next evil-cp command. For example, if the user
    ;; has 2 cursors and calls `evil-cp-override' and then
    ;; `evil-cp-delete-char-or-splice', evil-mc should call them in this order:
    ;; override, delete-or-splice, override, delete-or-splice
    ;; instead of: override, override, delete-or-splice, delete-or-splice
    ;; evil-cp-override ; M-z
    )
  "A list of the supported commands and their handlers.
Entries have the form (NAME . HANDLERS), where handlers is a list of entries of
the form (STATE . HANDLER).  The state can be any evil state name or `:default'
which will be used if no entry matching the current state is found.")

(provide 'evil-mc-known-commands)

;;; evil-mc-known-commands.el ends here
