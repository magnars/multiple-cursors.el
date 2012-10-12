Feature: Multiple cursors core
  In order to make efficient changes
  As an Emacs user with multiple-cursors
  I want to change multiple parts of the buffer at once

  Scenario: Exiting multiple-cursors mode with return
    Given I have cursors at "text" in "This text contains the word text twice"
    And I press "<return>"
    Then I should have one cursor

  Scenario: Exiting multiple-cursors mode with C-g
    Given I have cursors at "text" in "This text contains the word text twice"
    And I press "C-g"
    Then I should have one cursor

  Scenario: Separate kill-rings
    Given I have cursors at "text" in "This text contains the word text twice"
    When I press "M-f"
    And I press "M-d"
    And I press "M-b"
    And I press "C-y"
    Then I should see "This  containstext the word  twicetext"

  Scenario: Separate kill-rings, yank-pop
    Given I have cursors at "text" in "This text contains the word text twice"
    When I press "M-d"
    And I press "C-f"
    And I press "M-d"
    And I press "C-y M-y"
    Then I should see "This  text the word  text"

  Scenario: Multiple lambdas
    Given I have bound C-! to a lambda that inserts "a"
    And I have cursors at "text" in "This text contains the word text twice"
    When I press "C-!"
    Then I should see "This atext contains the word atext twice"

  Scenario: Multiple supported command (forward-word in this case)
    Given I have cursors at "text" in "This text contains the word text twice"
    And I type "("
    And I press "M-f"
    And I press "M-f"
    And I type ")"
    Then I should see "This (text contains) the word (text twice)"

  Scenario: Unknown command: yes, do for all
    Given I have bound C-! to a new command that inserts "a"
    And I have cursors at "text" in "This text contains the word text twice"
    When I press "C-! y"
    And I press "C-!"
    Then I should see "This aatext contains the word aatext twice"

  Scenario: Unknown command: no, don't do for all
    Given I have bound C-! to another new command that inserts "a"
    And I have cursors at "text" in "This text contains the word text twice"
    When I press "C-! n"
    And I press "C-!"
    Then I should see "This aatext contains the word text twice"

  Scenario: Undo
    Given I have cursors at "text" in "This text contains the word text twice"
    When I press "M-f"
    And I press "M-DEL"
    And I press "C-_"
    And I type "!"
    Then I should see "This text! contains the word text! twice"

  Scenario: Setting and popping mark
    Given I have cursors at "text" in "This text contains the word text twice"
    And I press "C-SPC"
    And I press "M-f"
    And I press "C-u C-SPC"
    And I type "!"
    Then I should see "This !text contains the word !text twice"

  Scenario: delete-selection-mode (self-insert-command)
    Given I turn on delete-selection-mode
    And I have cursors at "text" in "This text contains the word text twice"
    And I press "C-SPC"
    And I press "M-f"
    And I type "!"
    Then I should see "This ! contains the word ! twice"

  Scenario: delete-selection-mode (delete-char)
    Given I turn on delete-selection-mode
    And I have cursors at "text" in "This text contains the word text twice"
    And I press "C-SPC"
    And I press "M-f"
    And I press "C-d"
    Then I should see "This  contains the word  twice"

  Scenario: delete-selection-mode (yank)
    Given I turn on delete-selection-mode
    And I have cursors at "text" in "This text contains the word text twice"
    And I press "M-b"
    And I press "C-SPC"
    And I press "M-f"
    And I press "M-w"
    And I press "C-SPC"
    And I press "M-f"
    And I press "C-y"
    Then I should see "ThisThis contains the wordword twice"

  Scenario: subword-mode
    Given I turn on subword-mode
    And I have cursors at "textSnippet" in "This textSnippet contains the word textSnippet twice"
    And I press "M-f"
    And I type "_"
    And I press "M-l"
    Then I should see "This text_snippet contains the word text_snippet twice"

  Scenario: cua-mode
    Given I turn on cua-mode
    And I insert "This text contains the word text twice"
    And I go to the front of the word "text"
    And I press "C-SPC"
    And I press "M-f"
    And I press "C->"
    And I type "!"
    Then I should see "This ! contains the word ! twice"

  Scenario: wrap-region (function turns to keyboard macros)
    Given I turn on wrap-region-mode
    And I insert "This text contains the word text twice"
    And I go to the front of the word "text"
    And I press "C-M-SPC"
    And I press "C->"
    And I press "C-g"
    And I type "("
    Then I should see "This (text contains the word (text twice"

  Scenario: Bound keyboard macros
    Given I have bound C-! to a keyboard macro that insert "_"
    And I have cursors at "text" in "This text contains the word text twice"
    When I press "C-!"
    When I press "C-!"
    Then I should see "This __text contains the word __text twice"

  Scenario: Interprogram paste
    Given I have cursors at "text" in "This text contains the word text twice"
    When I copy "external" in another program
    And I press "C-y"
    Then I should see "This externaltext contains the word externaltext twice"

  Scenario: Added to killed-rectangle after exiting multiple-cursors-mode
    Given I have cursors at "text" in "This text contains the word text twice"
    When I press "M-f"
    And I press "C-f"
    And I press "C-SPC"
    And I press "M-f"
    And I press "M-w"
    And I press "<return>"
    And I press "C-a"
    And I press "C-k"
    And I press "C-x r y"
    Then I should see:
    """
    contains
    twice
    """

  Scenario: Looping forwards around cursors
    Given I have cursors at "_" in "1_34567_9"
    And I press "C-v"
    And I press "C-v"
    And I press "C-v"
    Then the cursor should be at point "8"

  Scenario: Looping backwards around cursors
    Given I have cursors at "_" in "1_34567_9"
    And I press "M-v"
    And I press "M-v"
    Then the cursor should be at point "2"

