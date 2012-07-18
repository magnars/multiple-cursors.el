Feature: Multiple cursors core
  In order to make efficient changes
  As an Emacs user with multiple-cursors
  I want to change multiple parts of the buffer at once

  Scenario: Two cursors
    Given there is no region selected
    When I insert "This text contains the word text thrice (text)"
    And I select "text"
    And I press "C->"
    And I press "C->"
    And I press "C-g"
    Then I should have 3 cursors
