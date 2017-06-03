(require 'cl) ;; For lexical-let

(When "^I mark next like this$"
      (lambda () (call-interactively 'mc/mark-next-like-this)))

(When "^I mark next like this word$"
      (lambda () (call-interactively 'mc/mark-next-like-this-word)))

(When "^I mark next like this symbol$"
      (lambda () (call-interactively 'mc/mark-next-like-this-symbol)))

(When "^I mark previous like this$"
      (lambda () (call-interactively 'mc/mark-previous-like-this)))

(When "^I mark previous like this word$"
      (lambda () (call-interactively 'mc/mark-previous-like-this-word)))

(When "^I mark previous like this symbol$"
      (lambda () (call-interactively 'mc/mark-previous-like-this-symbol)))

(When "^I mark all like this$"
      (lambda () (call-interactively 'mc/mark-all-like-this)))

(When "^I mark all like this dwim$"
      (lambda () (call-interactively 'mc/mark-all-like-this-dwim)))

(When "^I mark all in region$"
      (lambda () (call-interactively 'mc/mark-all-in-region)))

(When "^I insert numbers$"
      (lambda () (call-interactively 'mc/insert-numbers)))

(When "^I insert letters$"
      (lambda () (call-interactively 'mc/insert-letters)))

(When "^I reverse regions$"
      (lambda () (call-interactively 'mc/reverse-regions)))

(When "^I sort regions$"
      (lambda () (call-interactively 'mc/sort-regions)))

(When "^I edit lines$"
      (lambda () (call-interactively 'mc/edit-lines)))

(When "^I set rectangular region anchor$"
      (lambda () (call-interactively 'set-rectangular-region-anchor)))

(And "^delete-selection-mode is active$"
     (lambda ()
       (delete-selection-mode 1)))

(Given "^I turn off transient-mark-mode$"
       (lambda ()
         (transient-mark-mode -1)))

(Then "^I should have \\([0-9]+\\) cursors$"
      (lambda (num)
        (let ((actual (mc/num-cursors)))
          (cl-assert (eq (string-to-number num) actual) nil
                     "Expected to have %s cursors, but was %d." num actual))))

(Then "^I should have one cursor$"
      (lambda ()
        (cl-assert (not multiple-cursors-mode) nil
                   "Expected to have one cursor, but multiple-cursors-mode is still active.")
        (cl-assert (eq 1 (mc/num-cursors)) nil
                   "Expected to have one cursor, but there are still fake cursor overlays.")))

(Then "^rectangular-region-mode should be off$"
      (lambda ()
        (cl-assert (not rectangular-region-mode) nil
                   "Expected rectangular-region-mode mode to be off, but wasn't.")))

(Then "^rectangular-region-mode should be on$"
      (lambda ()
        (cl-assert rectangular-region-mode nil
                   "Expected rectangular-region-mode mode to be on, but wasn't.")))

(When "^I press \"\\(.+\\)\"$"
      (lambda (keybinding)
        (let ((macro (edmacro-parse-keys keybinding)))
          (if espuds-chain-active
              (setq espuds-action-chain (vconcat espuds-action-chain macro))
            (if (and (equal keybinding "C-g")
                     (eq (key-binding (kbd "C-g")) 'keyboard-quit))
                (espuds-quit)
              (execute-kbd-macro macro))))))

(Given "^I have cursors at \"\\(.+\\)\" in \"\\(.+\\)\"$"
       (lambda (needle haystack)
         (insert haystack)
         (goto-char (point-min))
         (search-forward needle)
         (set-mark (point))
         (goto-char (match-beginning 0))
         (mc/mark-all-like-this)
         (mc/keyboard-quit)))

(Given "^I have cursors at \"\\(.+\\)\" in \\(?: \"\\(.+\\)\"\\|:\\)$"
       (lambda (needle haystack)
         (insert haystack)
         (goto-char (point-min))
         (search-forward needle)
         (set-mark (point))
         (goto-char (match-beginning 0))
         (mc/mark-all-like-this)
         (mc/keyboard-quit)))


(When "^I copy \"\\(.+\\)\" in another program$"
       (lambda (text)
         (lexical-let ((text text))
           (setq interprogram-paste-function
                 #'(lambda () (let ((r text)) (setq text nil) r))))))

(Given "^I have bound C-! to a lambda that inserts \"\\(.+\\)\"$"
       (lambda (ins)
         (lexical-let ((ins ins))
           (global-set-key (kbd "C-!") #'(lambda () (interactive) (insert ins))))))

(Given "^I have bound C-! to a new command that inserts \"\\(.+\\)\"$"
       (lambda (ins)
         (lexical-let ((ins ins))
           (defun mc-test-temp-command () (interactive) (insert ins))
           (global-set-key (kbd "C-!") 'mc-test-temp-command))))

(Given "^I have bound C-! to another new command that inserts \"\\(.+\\)\"$"
       (lambda (ins)
         (lexical-let ((ins ins))
           (defun mc-test-temp-command-2 () (interactive) (insert ins))
           (global-set-key (kbd "C-!") 'mc-test-temp-command-2))))

(Given "^I have bound C-! to a keyboard macro that insert \"_\"$"
       (lambda ()
         (fset 'mc-test-temp-kmacro "\C-q_")
         (global-set-key (kbd "C-!") 'mc-test-temp-kmacro)))

(When "^I go to character \"\\(.+\\)\"$"
      (lambda (char)
        (goto-char (point-min))
        (let ((search (re-search-forward (format "%s" char) nil t))
              (message "Can not go to character '%s' since it does not exist in the current buffer: %s"))
          (cl-assert search nil message char (espuds-buffer-contents)))))

(When "^I go to the \\(front\\|end\\) of the word \"\\(.+\\)\"$"
      (lambda (pos word)
        (goto-char (point-min))
        (let ((search (re-search-forward (format "%s" word) nil t))
              (message "Can not go to character '%s' since it does not exist in the current buffer: %s"))
          (cl-assert search nil message word (espuds-buffer-contents))
          (if (string-equal "front" pos) (backward-word)))))

(When "^I go to last word \"\\(.+\\)\"$"
      (lambda (text)
        (goto-char (point-max))
        (let ((search (re-search-backward text nil t)))
          (cl-assert search nil "The text '%s' was not found in the current buffer." text))))

(When "^I select the last \"\\(.+\\)\"$"
      (lambda (text)
        (goto-char (point-max))
        (let ((search (re-search-backward text nil t)))
          (cl-assert search nil "The text '%s' was not found in the current buffer." text))
        (set-mark (point))
        (re-search-forward text)))

(When "^I mark all \\(.+\\)$"
      (lambda (rest)
        (let ((func (intern (mapconcat 'identity
                                       (cons  "mc/mark-all"
                                              (split-string rest))
                                       "-"))))
          (call-interactively func))))

(Then "^I should see exactly\\(?: \"\\(.+\\)\"\\|:\\)$"
      "Asserts that the current buffer does not include some text with
       respect of text hidden by overlays"
      (lambda (expected)
        (let ((p (point-min))
              (visible-text "")
              (message "Expected buffer to be: \n%s\n but was actually:\n%s\n")
              )
          (while (not (= p (point-max)))
            (if (not (invisible-p p))
                (setq visible-text (concat visible-text  (buffer-substring p (1+ p))))
              )
            (setq p (1+ p))
            )
          (cl-assert (s-equals? expected visible-text) nil message expected visible-text))))

(defun marker-assert (e a)
  (if (marker-position a)
      (= e a)
    (eq e nil)))

(Then "The cursors should have these properties:"
      (lambda (cursor-props-table)
        (-let (((header .  rows) cursor-props-table))
          (-map (lambda (cursor-props)
                  (-let* (((cursor-type cursor-id cursor-point cursor-mark cursor-evil-state) cursor-props)
                          (type (intern cursor-type))
                          (id (if (string-equal "nil" cursor-id) nil (string-to-number cursor-id)))
                          (expected-point (string-to-number cursor-point))
                          (expected-mark (if (string-equal "nil" cursor-mark)
                                             nil
                                           (string-to-number cursor-mark)))
                          (expected-evil-state (intern cursor-evil-state)))
                    (cond
                     (id
                      (let* ((fc (mc/cursor-with-id id))
                             (actual-point (overlay-get fc 'point))
                             (actual-mark (overlay-get fc 'mark))
                             (actual-evil-state (overlay-get fc 'evil-state)))
                        (cl-assert (= expected-point actual-point) nil "Expected fake cursor with id '%s' to be at point '%s', but is actually at point '%s'" id expected-point actual-point)
                        (cl-assert (marker-assert expected-mark actual-mark) nil "Expected fake cursor with id '%s' to be at mark '%s', but is actually at mark '%s'" id expected-mark actual-mark)
                        (cl-assert (eq expected-evil-state actual-evil-state) nil "Expected fake cursor with id '%s' to have evil state '%s', but is actually at evil state '%s'" id expected-evil-state actual-evil-state)))
                     (t
                      (cl-assert (= expected-point (point)) nil "Expected main cursor to be at point '%s', but is actually at point '%s'" expected-point (point))
                      (cl-assert (eq expected-mark (mark)) nil "Expected main cursor to be at mark '%s', but is actually at mark '%s'" expected-mark (mark))
                      (cl-assert (eq expected-evil-state evil-state) nil "Expected main cursor to have evil state '%s', but is actually at evil state '%s'" expected-evil-state evil-state)))))
                rows))))

(Given "^I turn on evil-mode$"
       (lambda ()
         (evil-mode 1)
         (when (mark) (set-mark nil))))

(When "^I select \"\\(.+\\)\"$"
      "Selects TEXT if found. Otherwise signal an error."
      (lambda (text)
        (goto-char (point-min))
        (let ((search (re-search-forward text nil t)))
          (cl-assert search nil "The text '%s' was not found in the current buffer." text))
        (set-mark (point))
        (re-search-backward text)
        (when (mc/evil-p)
          (evil-visual-state))))

(When "^I replace the buffer text with\\(?: \"\\(.+\\)\"\\|:\\)$"
      "Replace the current buffer text with CONTENTS.
If testing with evil, enter normal state.
Disable multiple cursors before new insertion to get rid of any active cursors, then re-enable.
Go to the beginning of buffer and disable mark."
      (lambda (contents)
        (multiple-cursors-mode 0)
        (erase-buffer)
        (multiple-cursors-mode 1)
        (when (mc/evil-p) (evil-normal-state))
        (insert contents)
        (message "stuff inserted")
        (goto-char (point-min))
        (set-mark nil)))

(And "^I bind evil keys for multiple-cursors mode$"
     (lambda ()
       (define-key evil-normal-state-map "gr" nil)
       (define-key evil-normal-state-map "grm" 'mc/mark-all-like-this-dwim)
       (define-key evil-visual-state-map "gr" nil)
       (define-key evil-visual-state-map "grm" 'mc/mark-all-like-this)))

(And "^I press \"\\([^\"]+\\)\" followed by enter$"
     "Press ARG followed by a new line."
     (lambda (arg)
       (execute-kbd-macro
        (vconcat (edmacro-parse-keys arg)
                 (edmacro-parse-keys "C-j")))))

(And "^I set the register to \"\\([^\"]+\\)\" then type \"\\([^\"]+\\)\"$"
     (lambda (register input)
       (execute-kbd-macro (vconcat [34]
                                   (string-to-vector register)
                                   (string-to-vector input)))))

;; Local Variables:
;; byte-compile-warnings: (not cl-functions)
;; End:
