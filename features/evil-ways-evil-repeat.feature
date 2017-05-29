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
    And I type "fsrx"
    And I type "e.e.e.e.e."
    Then I should see exactly:
    """
    Thix ix x linx ox texx with words
    Thix ix x linx ox texx with words
    Thix ix x linx ox texx with words
    """

  @repeat-change-word
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

  @repeat-replace-word
  Scenario: Replace a word
    When I replace the buffer text with:
    """
    This is a line of text with words
    This is a line of text with words
    This is a line of text with words
    """
    And I type "grm"
    And I press "C-g"
    Then The cursors should have these properties:
      | type        |  id | point | mark | evil-state |
      | main-cursor | nil |     1 |    4 | normal     |
      | fake-cursor |   5 |    35 |   38 | normal     |
      | fake-cursor |   6 |    69 |   72 | normal     |
    And I type "wRxy"
    Then The cursors should have these properties:
      | type        |  id | point | mark | evil-state |
      | main-cursor | nil |     8 |    4 | replace    |
      | fake-cursor |   5 |    42 |   38 | replace    |
      | fake-cursor |   6 |    76 |   72 | replace    |
    And I press "<escape>"
    And I type "ww.w.w..."
    Then I should see exactly:
    """
    This xy a xyne xy xxxy with words
    This xy a xyne xy xxxy with words
    This xy a xyne xy xxxy with words
    """

  # TODO cc doesn't work
  @repeat-replace-line @todo-outstanding
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
    And I press "ve"
    And I press "C->"
    And I press "C-g"
    # TODO cc doesn't work
    # And I type "ccLine changed"
    And I type "b"
    And I type "CLine changed"
    And I press "<escape>"
    And I type "jjj4b"
    Then The cursors should have these properties:
      | type        | id  | point | mark | evil-state |
      | main-cursor | nil |    33 |    1 | normal     |
      | fake-cursor | 3   |    73 |   20 | normal     |
    And I type "."
    Then I should see exactly:
    """
    Line changed
    Space
    Line changed
    Line changed
    Space
    Line changed
    """

  @repeat-insert-text
  Scenario: Insert text
    When I replace the buffer text with:
    """
    This
    This
    This
    """
    And I type "grm"
    And I press "C-g"
    And I type "ea abc d"
    And I press "<escape>"
    And I type "..."
    And I type "a xy"
    Then I should see exactly:
    """
    This abc d abc d abc d abc d xy
    This abc d abc d abc d abc d xy
    This abc d abc d abc d abc d xy
    """
