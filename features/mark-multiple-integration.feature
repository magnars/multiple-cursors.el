Feature: Mark multiple integration
  In order to quickly and precisely get multiple cursors
  As an Emacs user with mark-multiple
  I want to mark multiple regions and then go to multiple-cursors-mode

  Scenario: Two cursors
    Given there is no region selected
    When I insert "This text contains the word text twice"
    And I select "text"
    And I press "C->"
    And I press "C-g"
    Then I should have 2 cursors

  Scenario: Three cursors
    Given there is no region selected
    When I insert:
    """
    This text contains the word text thrice, and
    one of the instances of the word text is on the second line.
    """
    And I select "text"
    And I press "C->"
    And I press "C->"
    And I press "C-g"
    Then I should have 3 cursors
