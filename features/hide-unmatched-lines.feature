Feature: Hiding lines without cursor

  Scenario: Hiding lines when three cursor active
    Given I have cursors at "line" in :
    """
    0
    line
    2
    3
    line
    5
    6
    7
    8
    9
    10
    11
    line
    13
    14
    15
    """
    And I press "C-'"
    Then I should have 3 cursors
    Then I should see exactly:
    """
    0
    line
    2
    3
    line
    5
    6

    10
    11
    line
    13
    14

   """


  Scenario: Hiding lines when only two cursor active
    When I insert:
    """
      1
      2
      3
      4
      5
      text
      6
      7
      8
      9
      10
    """
    And I go to the front of the word "text"
    And I press "C->"
    And I press "C-'"
    Then I should have 2 cursors
    Then I should see exactly:
    """

      4
      5
      text
      6
      7
      8

    """
