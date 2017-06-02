@evil-ways @evil-visual
Feature: Visual region
  Background:
    Given I turn on evil-mode
    And I bind evil keys for multiple-cursors mode 

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
    And I press "p"
    Then I should see exactly:
    """
    This is a This 
    This is a This 
    This is a This 
    """
