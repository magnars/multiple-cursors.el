Feature: Popping cursors off of the mark stack

  Scenario: Single pop
    Given I turn off transient-mark-mode
    And I insert:
    """
    hello
    there
    """
    And I go to the front of the word "hello"
    And I set the mark
    And I go to the front of the word "there"
    And I press "C-S-p"
    Then I should have 2 cursors

  Scenario: Multiple pops
    Given I turn off transient-mark-mode
    And I insert:
    """
    hello
    there, my friend
    """
    And I go to the front of the word "hello"
    And I set the mark
    And I go to the front of the word "my"
    And I set the mark
    And I go to the front of the word "friend"
    And I press "C-S-p C-S-p"
    Then I should have 3 cursors

  Scenario: Discard identical mark and point
    Given I turn off transient-mark-mode
    And I insert:
    """
    hello
    there, my friend
    """
    And I go to the front of the word "hello"
    And I set the mark
    And I go to the front of the word "my"
    And I set the mark
    And I go to the front of the word "friend"
    And I set the mark
    And I press "C-S-p C-S-p"
    Then I should have 3 cursors

