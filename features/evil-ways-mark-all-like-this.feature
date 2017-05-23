@evil-ways
Feature: mark-all-like-this stores state correctly when using evil
  Background:
    Given I turn on evil-mode
    And I insert:
    """
    First  line
    Second line
    Third  line
    """
    And I select "line"
    And I press "M-!"
    Then I should have 3 cursors
    Then The cursors should have these properties:
      | type        |  id | point | mark | evil-state |
      | main-cursor | nil |     8 |   11 | visual     |
      | fake-cursor |   1 |    20 |   23 | visual     |
      | fake-cursor |   2 |    32 |   35 | visual     |
      
  @visual-state-mark-all @change-word
  Scenario: Change word
    And I press "cfine"
    Then I should see exactly:
    """
    First  fine
    Second fine
    Third  fine
    """

  @visual-state-mark-all @delete-word
  Scenario: Delete word
    And I press "d"
    Then I should see exactly:
    """
    First  
    Second 
    Third  
    """

  @visual-state-mark-all @end-of-word
  Scenario: Move end of word
    And I press "e"
    And I press "An"
    Then I should see exactly:
    """
    First  linen
    Second linen
    Third  linen
    """

  @visual-state-mark-all @forward-char
  Scenario: Forward char
    And I press "lll"
    And I press "xat"
    Then I should see exactly:
    """
    First  lint
    Second lint
    Third  lint
    """

  @visual-state-mark-all @forward-char-with-count
  Scenario: Forward char with count prefix
    And I press "3l"
    And I press "xat"
    Then I should see exactly:
    """
    First  lint
    Second lint
    Third  lint
    """

  @visual-state-mark-all @cursors-to-normal-state
  Scenario: Switch all cursors to normal state on keyboard-quit
    And I press "C-g"
    Then I should have 3 cursors
    Then The cursors should have these properties:
      | type        |  id | point | mark | evil-state |
      | main-cursor | nil |     8 |   11 | normal     |
      | fake-cursor |   1 |    20 |   23 | normal     |
      | fake-cursor |   2 |    32 |   35 | normal     |
    And I press "dw"
    Then I should see exactly:
    """
    First  
    Second 
    Third  
    """
