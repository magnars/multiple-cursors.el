@evil-ways
Feature: mark-next-like-this stores state correctly when using evil
  Background:
    Given I turn on evil-mode
    And I insert:
    """
    First  line
    Second line
    """
    And I select "line"
    And I press "C->"
    Then I should have 2 cursors
    Then The cursors should have these properties:
      | type        | id  | point | mark | evil-state |
      | main-cursor | nil |     8 |   11 | visual     |
      | fake-cursor | 1   |    20 |   23 | visual     |
      
  @visual-state-mark-next @change-word
  Scenario: Change word
    And I press "cfine"
    Then I should see exactly:
    """
    First  fine
    Second fine
    """

  @visual-state-mark-next @delete-word
  Scenario: Delete word
    And I press "d"
    Then I should see exactly:
    """
    First  
    Second 
    """

  @visual-state-mark-next @end-of-word
  Scenario: Move end of word
    And I press "e"
    And I press "An"
    Then I should see exactly:
    """
    First  linen
    Second linen
    """

  @visual-state-mark-next @forward-char
  Scenario: Forward char
    And I press "lll"
    And I press "xat"
    Then I should see exactly:
    """
    First  lint
    Second lint
    """

  @visual-state-mark-next @forward-char-with-count
  Scenario: Forward char with count prefix
    And I press "3l"
    And I press "xat"
    Then I should see exactly:
    """
    First  lint
    Second lint
    """

  @visual-state-mark-next @cursors-to-normal-state
  Scenario: Switch all cursors to normal state on keyboard-quit
    And I press "C-g"
    Then I should have 2 cursors
    Then The cursors should have these properties:
      | type        | id  | point | mark | evil-state |
      | main-cursor | nil |     8 |   11 | normal     |
      | fake-cursor | 1   |    20 |   23 | normal     |
    And I press "dw"
    Then I should see exactly:
    """
    First  
    Second 
    """
