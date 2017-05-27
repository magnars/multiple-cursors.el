@evil-ways @visual-state-mark-previous
Feature: mark-previous-like-this stores state correctly when using evil
  Background:
    Given I turn on evil-mode
    And I insert:
    """
    First  a line
    Second a line
    Third  a line
    """
    And I select the last "line"

  @change-word
  Scenario: Change word
    And I press "C-<"
    And I press "C-<"
    And I press "cfine"
    Then I should see exactly:
    """
    First  a fine
    Second a fine
    Third  a fine
    """

  @delete-word
  Scenario: Delete word
    And I press "C-<"
    And I press "C-<"
    And I press "d"
    Then I should see exactly:
    """
    First  a 
    Second a 
    Third  a 
    """

  @append-to-word
  Scenario: Append to word
    And I press "C-<"
    And I press "C-<"
    And I press "An"
    Then I should see exactly:
    """
    First  a linen
    Second a linen
    Third  a linen
    """

  @backward-char
  Scenario: Backward char
    And I press "C-<"
    And I press "C-<"
    And I press "h"
    And I press "xidim"
    Then I should see exactly:
    """
    First  a dime
    Second a dime
    Third  a dime
    """

  @backward-char-with-count
  Scenario: Backward char with count prefix
    And I press "C-<"
    And I press "C-<"
    And I press "3h"
    And I press "xid"
    Then I should see exactly:
    """
    First  a dine
    Second a dine
    Third  a dine
    """

  @cursors-to-normal-state
  Scenario: Switch all cursors to normal state on keyboard-quit
    And I press "C-<"
    And I press "C-<"
    And I press "C-g"
    Then I should have 3 cursors
    Then The cursors should have these properties:
      | type        |  id | point | mark | evil-state |
      | main-cursor | nil |    41 |   38 | normal     |
      | fake-cursor |   1 |    27 |   24 | normal     |
      | fake-cursor |   2 |    13 |   10 | normal     |
    And I press "bdw"
    Then I should see exactly:
    """
    First  a 
    Second a 
    Third  a 
    """

  @look-thrice-mark-single-letter
  Scenario: Mark a single letter 3 times
    When I select the last "a"
    And I press "C-<"
    And I press "C-<"
    And I press "C-g"
    # TODO shouldn'd bC work? but get "End of Buffer" error
    And I press "bc$"
    And I type "feline"
    Then I should see exactly:
    """
    First  feline
    Second feline
    Third  feline
    """
