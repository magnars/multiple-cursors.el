Feature: Insert increasing letters

  Scenario: Three cursors, a-b-c
    Given I have cursors at "text" in "This text contains the word text thrice (text)"
    When I press "H-3"
    And I press "SPC"
    Then I should see "This a text contains the word b text thrice (c text)"

  Scenario: Three cursors, j-k-l
    Given I have cursors at "text" in "This text contains the word text thrice (text)"
    When I press "C-9 H-3"
    And I press "SPC"
    Then I should see "This j text contains the word k text thrice (l text)"

  Scenario: Three cursors, z-aa-ab
    Given I have cursors at "text" in "This text contains the word text thrice (text)"
    When I press "C-u 2 5 H-3"
    And I press "SPC"
    Then I should see "This z text contains the word aa text thrice (ab text)"

  Scenario: Three cursors, a-b-c
    Given I have cursors at "text" in "This text contains the word text thrice (text)"
    When I press "C-u H-3"
    And I press "SPC"
    Then I should see "This e text contains the word f text thrice (g text)"
