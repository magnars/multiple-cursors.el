@evil-ways @evil-ways-mark-all-dwim
Feature: Mark All DWIM
  Background:
    Given I turn on evil-mode

  Scenario: mark-all-dwim quit visual selection via keyboard quit
    When I replace the buffer text with:
    """
    test
    test
    """
    And I press "C-$"
    And I press "C-g"
    And I press "x"
    Then I should see exactly:
    """
    est
    est
    """

  Scenario: mark-all-dwim quit visual selection via evil-exit-visual-state
    When I replace the buffer text with:
    """
    test
    test
    """
    And I press "C-$"
    And I press "<escape>"
    And I press "x"
    Then I should see exactly:
    """
    est
    est
    """
