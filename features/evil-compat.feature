@evil
Feature: Evil compatibility
  Background:
    Given I turn on evil-mode

  Scenario: normal -> insert -> normal transitions moves all cursors the same amount
    When I insert:
    """
    First  line
    Second line
    """
    And I go to the front of the word "line"
    When I mark next like this
    And I press "i"
    And I press "<escape>"
    And I press "x"
    Then I should see exactly:
    """
    First line
    Secondline
    """

  Scenario: normal -> append -> normal transitions moves all cursors the same amount
    When I insert:
    """
    First  line
    Second line
    """
    And I go to the front of the word "line"
    When I mark next like this
    And I press "a"
    And I press "<escape>"
    And I press "x"
    Then I should see exactly:
    """
    First  ine
    Second ine
    """

  Scenario: enabling mc from insert state allows to enter normal state
    When I insert:
    """
    First  line
    Second line
    """
    And I go to the front of the word "line"
    And I press "i"
    When I mark next like this
    And I press "<escape>"
    And I press "x"
    Then I should see exactly:
    """
    First line
    Secondline
    """

  @failing
  Scenario: All cursors deactivate region on when pressing ESC
    Given I have cursors at "text" in "This text contains the word text twice"
    And I press "i"
    And I press "<escape>"
    And I press "x"
    Then I should see "Thistext contains the wordtext twice"

  Scenario: Operator-motions
    When I insert:
    """
    First  line
    Second line
    """
    And I go to the front of the word "line"
    When I mark next like this
    And I press "de"
    Then I should see exactly:
    """
    First  
    Second 
    """

  # TODO: I don't know why this fails, the error message seems to be about
  # keymap problems.
  @failing
  Scenario: Visual mode
    When I insert:
    """
    First  line
    Second line
    """
    And I go to the front of the word "line"
    When I mark next like this
    When I start an action chain
    And I press "v"
    And I press "e"
    And I press "d"
    When I execute the action chain
    Then I should see exactly:
    """
    First  
    Second 
    """

  @failing
  Scenario: evil-find-char-to
    When I insert:
    """
    First  line
    Second line
    """
    And I go to the front of the word "line"
    When I mark next like this
    And I press "tex"
    Then I should see exactly:
    """
    First  lie
    Second lie
    """

  @failing
  Scenario: evil-repeat
    When I insert:
    """
    First  line and some words
    Second line and some words
    """
    And I go to the front of the word "line"
    When I mark next like this
    And I press "dw"
    Then I should see exactly:
    """
    First  and some words
    Second and some words
    """
    And I press "."
    Then I should see exactly:
    """
    First  some words
    Second some words
    """

