Feature: Insert increasing numbers

  Scenario: Three cursors, 0-1-2
    Given I have cursors at "text" in "This text contains the word text thrice (text)"
    When I press "H-0"
    And I press "SPC"
    Then I should see "This 0 text contains the word 1 text thrice (2 text)"

  Scenario: Three cursors, 9-10-11
    Given I have cursors at "text" in "This text contains the word text thrice (text)"
    When I press "C-9 H-0"
    And I press "SPC"
    Then I should see "This 9 text contains the word 10 text thrice (11 text)"

  Scenario: Three cursors, 9-10-11
    Given I have cursors at "text" in "This text contains the word text thrice (text)"
    When I press "C-u H-0"
    And I press "SPC"
    Then I should see "This 4 text contains the word 5 text thrice (6 text)"
