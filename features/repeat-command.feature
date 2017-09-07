Feature: Repeat last interactive command for fake cursors (mc/repeat-command)

  Scenario: Clone insert-char from M-x
    Given I have cursors at "text" in "This text contains the word text thrice (text)"
    When I start an action chain
    When I press "M-x"
    And I type "insert-char"
    And I press "RET"
    And I type "21"
    And I press "RET"
    And I press "C-:"
    And I press "y"
    And I execute the action chain
    Then I should see "This !text contains the word !text thrice (!text)"

  Scenario: Clone insert-char from M-:
    Given I have cursors at "text" in "This text contains the word text thrice (text)"
    When I start an action chain
    When I press "M-:"
    And I type "(insert-char (+ 40 2))"
    And I press "RET"
    And I press "C-:"
    And I press "y"
    And I execute the action chain
    Then I should see "This *text contains the word *text thrice (*text)"

  Scenario: Disable prompt
    Given I have cursors at "text" in "This text/0000 contains the word text/1111 thrice (text/2222)"
    When I set mc/always-repeat-command to t
    When I start an action chain
    And I press "M-x"
    And I type "zap-to-char"
    And I press "RET"
    And I press "/"
    And I press "C-:"
    And I execute the action chain
    Then I should see "This 0000 contains the word 1111 thrice (2222)"
