Feature: Rectangular region

  Scenario: Works like regular region when on one line
    When I insert "some text"
    And I press "H-SPC"
    And I press "M-b"
    Then the region should be "text"
    And rectangular-region-mode should be on

  Scenario: Works like regular region when on one line, insert
    Given I turn on delete-selection-mode
    When I insert "some text"
    And I press "H-SPC"
    And I press "M-b"
    And I type "replacement"
    Then I should see "some replacement"
    And rectangular-region-mode should be off

  Scenario: Works like regular region when on one line, copy 1
    Given I turn on delete-selection-mode
    When I insert "some text"
    And I press "H-SPC"
    And I press "M-b"
    And I press "M-w"
    Then rectangular-region-mode should be off

  Scenario: Works like regular region when on one line, copy 2
    Given I turn on delete-selection-mode
    When I insert "some text"
    And I press "H-SPC"
    And I press "M-b"
    And I press "M-w"
    And I press "C-y"
    Then I should see "some texttext"

  Scenario: Changing multiple lines
    Given I turn on delete-selection-mode
    When I insert:
    """
    This is some text
    This is more text
    """
    And I go to point "6"
    And I press "H-SPC"
    And I press "M-f"
    And I press "C-n"
    And I type "was"
    Then I should see:
    """
    This was some text
    This was more text
    """
    And I should have 2 cursors

  Scenario: Changing multiple lines with gaps
    Given I turn on delete-selection-mode
    When I insert:
    """
    This is some text

    This is more text
    """
    And I go to point "6"
    And I press "H-SPC"
    And I go to the end of the word "more"
    And I type "was"
    Then I should see:
    """
    This was text

    This was text
    """
    And I should have 2 cursors
