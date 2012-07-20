(And "^delete-selection-mode is active$"
     (lambda ()
       (delete-selection-mode 1)))

(defun num-cursors ()
  (1+ (count-if 'mc/fake-cursor-p
                (overlays-in (point-min) (point-max)))))

(Then "^I should have \\([0-9]+\\) cursors$"
      (lambda (num)
        (let ((actual (num-cursors)))
          (assert (eq (string-to-number num) actual) nil
                  "Expected to have %s cursors, but was %d." num actual))))

(Then "^I should have one cursor$"
      (lambda ()
        (assert (not multiple-cursors-mode) nil
                "Expected to have one cursor, but multiple-cursors-mode is still active.")
        (assert (eq 1 (num-cursors)) nil
                "Expected to have one cursor, but there are still fake cursor overlays.")))

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
         (mark-all-like-this)
         (mc/switch-from-mark-multiple-to-cursors)))

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
