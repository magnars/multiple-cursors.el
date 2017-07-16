@evil-ways @evil-visual
Feature: Visual region
  Background:
    Given I turn on evil-mode
    And I bind evil keys for multiple-cursors mode 

  @visual-change-text
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

  @visual-change-text-at-beginning
  Scenario: Change selected text with the cursor at the beginning of the region
    When I replace the buffer text with:
    """
    This is a line.
    This is a line.
    This is a line.
    """
    And I press "grm"
    And I press "C-g"
    # TODO this wont work
    # And I press "wveo"
    And I press "wve"
    And I press "cisn't"
    Then I should see exactly:
    """
    This isn't a line.
    This isn't a line.
    This isn't a line.
    """

  @visual-paste-over
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
    # TODO this won't work idk why
    # And I type "wwwveo"
    And I type "wwwve"
    Then I should have 3 cursors
    Then The cursors should have these properties:
      | type        |  id | point | mark | evil-state |
      | main-cursor | nil |    14 |   11 | visual     |
      | fake-cursor |   5 |    29 |   26 | visual     |
      | fake-cursor |   6 |    44 |   41 | visual     |
    And I press "p"
    Then I should see exactly:
    """
    This is a This 
    This is a This 
    This is a This 
    """

 Scenario: Delete backwards from visual state
    When I replace the buffer text with:
    """
    test
    test
    """
    And I press "C->"
    And I press "ev2hx"
    Then I should see exactly:
    """
    t
    t
    """

 @pr-bugs @failing @evil-visual-restore
 Scenario: evil-visual-restore
    #I don't know, is it possible to use evil-visual-restore with multiple-cursors
    When I replace the buffer text with:
    """
    test
    test
    """
    And I press "C->"
    And I press "v2l"
    And I press "<escape>"
    And I press "gvx"
    Then I should see exactly:
    """
    t
    t
    """
