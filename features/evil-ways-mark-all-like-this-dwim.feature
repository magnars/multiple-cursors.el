@evil-ways @mark-all-like-this-dwim
Feature: mark-all-like-this-dwim stores state correctly when using evil
  Background:
    Given I turn on evil-mode
    And I insert:
    """
    First  a line
    Second a line
    Third  a line
    """
    And I go to the front of the word "line"
      
  @correct-cursor-positions
  Scenario: Correct cursor positions
    And I press "M-$"
    Then I should have 3 cursors
    Then The cursors should have these properties:
      | type        |  id | point | mark | evil-state |
      | main-cursor | nil |    10 |   13 | visual     |
      | fake-cursor |   1 |    24 |   27 | visual     |
      | fake-cursor |   2 |    38 |   41 | visual     |

  @change-word
  Scenario: Change word
    And I press "M-$"
    And I press "cfine"
    Then I should see exactly:
    """
    First  a fine
    Second a fine
    Third  a fine
    """

  @delete-word
  Scenario: Delete word
    And I press "M-$"
    And I press "d"
    Then I should see exactly:
    """
    First  a 
    Second a 
    Third  a 
    """

  @append-to-end-of-word
  Scenario: Append to end of word
    And I press "M-$"
    And I press "An"
    Then I should see exactly:
    """
    First  a linen
    Second a linen
    Third  a linen
    """

  @forward-char
  Scenario: Forward char
    And I press "M-$"
    And I press "lll"
    And I press "xat"
    Then I should see exactly:
    """
    First  a lint
    Second a lint
    Third  a lint
    """

  @forward-char-with-count
  Scenario: Forward char with count prefix
    And I press "M-$"
    And I press "3l"
    And I press "xat"
    Then I should see exactly:
    """
    First  a lint
    Second a lint
    Third  a lint
    """

  @cursors-to-normal-state
  Scenario: Switch all cursors to normal state on keyboard-quit
    And I press "M-$"
    And I press "C-g"
    Then I should have 3 cursors
    Then The cursors should have these properties:
      | type        |  id | point | mark | evil-state |
      | main-cursor | nil |    10 |   13 | normal     |
      | fake-cursor |   1 |    24 |   27 | normal     |
      | fake-cursor |   2 |    38 |   41 | normal     |
    And I press "dw"
    Then I should see exactly:
    """
    First  a 
    Second a 
    Third  a 
    """

  @look-thrice-mark-single-letter @failing-infinite-loop
  Scenario: mark a single letter 3 times
    And I go to the front of the word "a"
    And I press "M-$"
    And I press "C-g"
    And I press "wife"
    Then I should see exactly:
    """
    First  a feline
    Second a feline
    Third  a feline
    """
