Feature: Multiple cursors core
  In order to make efficient changes
  As an Emacs user with multiple-cursors
  I want to change multiple parts of the buffer at once

  Scenario: Exiting multiple-cursors mode with return
    Given there is no region selected
    When I insert "This text contains the word text twice"
    And I select "text"
    And I press "C->"
    And I press "C-g"
    And I press "<return>"
    Then I should have one cursor

  Scenario: Exiting multiple-cursors mode with C-g
    Given there is no region selected
    When I insert "This text contains the word text twice"
    And I select "text"
    And I press "C->"
    And I press "C-g"
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

  Scenario: Interprogram paste
    Given I have cursors at "text" in "This text contains the word text twice"
    When I copy "external" in another program
    And I press "C-y"
    Then I should see "This externaltext contains the word externaltext twice"

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
