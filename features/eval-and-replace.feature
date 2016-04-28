Feature: eval and replace

  Scenario: Three cursors, 0-1-2
    Given I have cursors at "text" in "This (+ 2 3) text contains the word (+ 2 3) text thrice ((+ 2 (- 3 4)) text)"
    When I press "H-4"
    And I press "SPC"
    Then I should see "This 5 text contains the word 5 text thrice (1 text)"

  Scenario: Three cursors, 0-1-2 test mc/id
    Given I have cursors at "text" in "This (+ 2 mc/id) text contains the word (identity mc/id) text thrice ((* mc/id 4) text)"
    When I press "H-4"
    And I press "SPC"
    Then I should see "This 2 text contains the word 1 text thrice (8 text)"

  Scenario: Three cursors, 4-5-6
    Given I have cursors at "text" in "This (+ 2 mc/id) text contains the word (identity mc/id) text thrice ((* mc/id 4) text)"
    When I press "C-u H-4"
    And I press "SPC"
    Then I should see "This 6 text contains the word 5 text thrice (24 text)"
