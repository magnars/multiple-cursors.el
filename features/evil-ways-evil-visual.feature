@evil-ways @evil-visual
Feature: Visual region
  Background:
    Given I turn on evil-mode
    And I bind evil keys for multiple-cursors mode 

  @visual-change-text @failing
  Scenario: Change selected text with the cursor at the end of the region
    When I replace the buffer text with:
    """
    This is a line.
    This is a line.
    This is a line.
    """
    And I press "grm"
    And I press "C-g"
    And I press "wve"
    And I press "cisn't"
    Then I should see exactly:
    """
    This isn't a line.
    This isn't a line.
    This isn't a line.
    """

  Scenario: Change selected text with the cursor at the beginning of the region
    When I replace the buffer text with:
    """
    This is a line.
    This is a line.
    This is a line.
    """
    And I press "grm"
    And I press "C-g"
    And I press "wveo"
    And I press "cisn't"
    Then I should see exactly:
    """
    This isn't a line.
    This isn't a line.
    This isn't a line.
    """

  # points are right but something is off still
  @visual-paste-over @failing
  Scenario: Paste over a visual selection at end of line
    When I replace the buffer text with:
    """
    This is a line
    This is a line
    This is a line
    """
    And I press "grm"
    And I press "C-g"
    And I type "yw"
    And I type "wwwveo"
    Then I should have 3 cursors
    Then The cursors should have these properties:
      | type        |  id | point | mark | evil-state |
      | main-cursor | nil |    11 |   14 | visual     |
      | fake-cursor |   5 |    26 |   29 | visual     |
      | fake-cursor |   6 |    41 |   44 | visual     |
    And I press "p"
    Then I should see exactly:
    """
    This is a This 
    This is a This 
    This is a This 
    """
