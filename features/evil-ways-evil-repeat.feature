@evil-ways @evil-repeat
Feature: Evil repeat functionality
  Background:
    Given I turn on evil-mode
    And I bind evil keys for multiple-cursors mode 

  Scenario: Replace a character
    When I replace the buffer text with:
    """
    This is a line of text with words
    This is a line of text with words
    This is a line of text with words
    """
    And I type "grm"
    And I press "C-g"
    And I type "rx"
    And I type "e.e.e.e.e."
    Then I should see exactly:
    """
    Thix ix x linx ox texx with words
    Thix ix x linx ox texx with words
    Thix ix x linx ox texx with words
    """

  Scenario: Change a word
    When I replace the buffer text with:
    """
    This is a line of text with words
    This is a line of text with words
    This is a line of text with words
    """
    And I type "grm"
    And I press "C-g"
    And I type "cwabc"
    And I press "<escape>"
    And I type "w..."
    And I type "ww."
    And I type "ww.."
    Then I should see exactly:
    """
    abc abababc a abc of ababc with words
    abc abababc a abc of ababc with words
    abc abababc a abc of ababc with words
    """

  Scenario: Replace a word
    When I replace the buffer text with:
    """
    This is a line of text with words
    This is a line of text with words
    This is a line of text with words
    """
    And I type "grm"
    And I press "C-g"
    And I type "wRxy"
    And I press "<escape>"
    And I type "ww.w.w..."
    Then I should see exactly:
    """
    This xy a xyne xy xxxy with words
    This xy a xyne xy xxxy with words
    This xy a xyne xy xxxy with words
    """

  Scenario: Replace a whole line
    When I replace the buffer text with:
    """
    This is a line of text with words
    Space
    This is a line of text with words
    This is a line of text with words
    Space
    This is a line of text with words
    """
    And I press "C->"
    And I type "ccLine changed"
    And I press "<escape>"
    And I type "jjj."
    Then I should see exactly:
    """
    Line changed
    Space
    Line changed
    Line changed
    Space
    Line changed
    """

  Scenario: Insert text
    When I replace the buffer text with:
    """
    This
    This
    This
    """
    And I type "grm"
    And I press "C-g"
    And I type "a abc d"
    And I press "<escape>"
    And I type "..."
    And I type "a xy"
    Then I should see exactly:
    """
    This abc d abc d abc d abc d xy
    This abc d abc d abc d abc d xy
    This abc d abc d abc d abc d xy
    """
