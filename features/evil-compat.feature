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

  Scenario: All cursors deactivate region on when pressing ESC
    Given I have cursors at "text" in "This text contains the word text twice"
    And I press "i"
    And I press "<escape>"
    And I press "x"
    Then I should see "Thistext contains the wordtext twice"
