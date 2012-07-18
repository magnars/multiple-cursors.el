Feature: Multiple cursors core
  In order to make efficient changes
  As an Emacs user with multiple-cursors
  I want to change multiple parts of the buffer at once

  Scenario: Two cursors
    Given there is no region selected
    When I insert "This text contains the word text twice"
    And I select "text"
    And I press "C->"
    And I switch to multiple-cursors mode
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
    And I switch to multiple-cursors mode
    Then I should have 3 cursors

  Scenario: Exiting multiple-cursors mode with return
    Given there is no region selected
    When I insert "This text contains the word text twice"
    And I select "text"
    And I press "C->"
    And I switch to multiple-cursors mode
    And I press "<return>"
    Then I should have one cursor
