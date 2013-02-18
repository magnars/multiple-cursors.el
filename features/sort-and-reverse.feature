Feature: Sorting and reversing cursor regions

  Scenario: Reversing regions
    Given I have cursors at "text" in "This text contains the word text thrice (text here)"
    When I press "M-f"
    And I press "C-f"
    And I press "C-SPC"
    And I press "M-f"
    And I press "H-1"
    Then I should see "This text here the word text thrice (text contains)"

  Scenario: Sorting regions
    Given I have cursors at "text" in "This text contains the word text thrice (text here)"
    When I press "M-f"
    And I press "C-f"
    And I press "C-SPC"
    And I press "M-f"
    And I press "H-2"
    Then I should see "This text contains the word text here (text thrice)"
