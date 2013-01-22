Feature: Switching from a multiline region to multiple cursors

  Scenario: Single line region
    When I insert "hello there"
    And I select "there"
    And I press "C-S-c C-S-c"
    Then I should have one cursor

  Scenario: Edit lines
    When I insert:
    """
    hello
    there
    """
    And I go to the front of the word "hello"
    And I set the mark
    And I go to the front of the word "there"
    And I press "C-S-c C-S-c"
    Then I should have 2 cursors

  Scenario: Edit lines from bottom up
    When I insert:
    """
    hello
    there
    """
    And I go to the front of the word "there"
    And I set the mark
    And I go to the front of the word "hello"
    And I press "C-S-c C-S-c"
    Then I should have one cursor

  Scenario: Edit only real lines, even in visual-line-mode
    Given I turn on visual-line-mode
    And I insert:
    """
    some very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very
    long text
    """
    And I go to the front of the word "some"
    And I set the mark
    And I go to the front of the word "long"
    And I press "C-S-c C-S-c"
    Then I should have 2 cursors

  Scenario: Edit without using transient mark mode
    Given I turn off transient-mark-mode
    And I insert:
    """
    hello
    there
    """
    And I go to the front of the word "hello"
    And I set the mark
    And I go to the front of the word "there"
    And I press "C-S-c C-S-c"
    Then I should have 2 cursors
