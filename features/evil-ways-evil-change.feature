@evil-ways @evil-change
Feature: Changing text should be reflected for all cursors in the buffer
  Background:
    Given I turn on evil-mode
    And I bind evil keys for multiple-cursors mode 

  @evil-change-letter-mark-all-dwim
  Scenario: Change a letter
    When I replace the buffer text with "xyz xyz xyz"
    And I press "grm"
    And I press "C-g"
    And I type "clw"

  @evil-change-word-mark-all-dwim
  Scenario: Change a word
    When I replace the buffer text with "xyz xyz xyz"
    And I press "grm"
    And I press "C-g"
    And I type "cwabc"
    Then I should see "abc abc abc"

  @evil-change-word-backwards-mark-all-dwim
  Scenario: Change a word backwards
    When I replace the buffer text with "xyz xyz xyz"
    And I press "grm"
    And I press "C-g"
    And I type "ecbabc"
    Then I should see "abcz abcz abcz"

  @evil-change-word-bol-mark-all-dwim
  Scenario: Change a word at the beginning of line
    When I replace the buffer text with:
    """
    This is a line
    This is a line
    This is a line
    """
    And I press "grm"
    And I press "C-g"
    And I type "cwabc"
    Then I should see exactly:
    """
    abc is a line
    abc is a line
    abc is a line
    """

  @evil-change-word-bol-backwards-mark-all-dwim
  Scenario: Change a word at the beginning of line backwards
    When I replace the buffer text with:
    """
    This is a line of text with words
    This is a line of text with words
    This is a line of text with words
    """
    And I type "grm"
    And I press "C-g"
    And I type "ecbx"
    Then I should see exactly:
    """
    xs is a line of text with words
    xs is a line of text with words
    xs is a line of text with words
    """

  @evil-change-multi-words-mark-all-dwim
  Scenario: Change multiple words (multiple lines)
    When I replace the buffer text with:
    """
    This is a line of text with words
    This is a line of text with words
    This is a line of text with words
    """
    And I type "grm"
    And I press "C-g"
    And I type "3cwk"
    Then I should see exactly:
    """
    k line of text with words
    k line of text with words
    k line of text with words
    """

  @evil-change-multi-words-mark-all-dwim
  Scenario: Change multiple words backwards (multiple lines)
    When I replace the buffer text with:
    """
    This is a line of text with words
    This is a line of text with words
    This is a line of text with words
    """
    And I type "grm"
    And I press "C-g"
    And I type "4e3cbk"
    Then I should see exactly:
    """
    This ke of text with words
    This ke of text with words
    This ke of text with words
    """

  @evil-change-to-end-of-word-mark-all-dwim
  Scenario: Change to the end of word
    When I replace the buffer text with "xyz xyz xyz"
    And I press "grm"
    And I press "C-g"
    And I type "ceabc"
    Then I should see "abc abc abc"

  @evil-change-to-end-of-word-with-count-mark-all-dwim
  Scenario: Change to the end of word with count
    When I replace the buffer text with:
    """
    xyz yyz yyz xyz yyz yyz xyz yyz yyz xyz yyz yyz
    """
    And I press "grm"
    And I press "C-g"
    And I type "2ceabc"
    Then I should see "abc yyz abc yyz abc yyz abc yyz"

  Scenario: Change up to a letter (f)
    When I replace the buffer text with "another-test another-test another-test"
    And I press "grm"
    And I press "C-g"
    And I type "cftxyz"
    Then I should see "xyzher-test xyzher-test xyzher-test"

  Scenario: Change up to a letter (f) with count
    When I replace the buffer text with "another-test another-test another-test"
    And I press "grm"
    And I press "C-g"
    And I type "2cftxyz"
    Then I should see "xyzest xyzest xyzest"

  Scenario: Change up till before a letter (t)
    When I replace the buffer text with "another-test another-test another-test"
    And I press "grm"
    And I press "C-g"
    And I type "cttxyz"
    Then I should see "xyzther-test xyzther-test xyzther-test"

  @evil-change-viz-selection-mark-all-dwim @failing
  Scenario: Change a visual selection
    When I replace the buffer text with "another-test another-test another-test"
    And I press "grm"
    And I press "C-g"
    And I type "v4lcxyz"
    Then I should see "xyzer-test xyzer-test xyzer-test"

  @evil-change-viz-selection-mark-all-dwim @failing
  Scenario: Change a visual selection 2
    When I replace the buffer text with:
    """
    This is a simple line.
    This is a simple line.
    This is a simple line.
    That is a simple line.
    """
    And I press "grm"
    And I press "C-g"
    And I type "vt.cChanged text"
    Then I should see exactly:
    """
    Changed text.
    Changed text.
    Changed text.
    That is a simple line.
    """

  Scenario: Change until the end of line
    When I replace the buffer text with:
    """
    This is a line.
    This is a line.
    This is a line.
    """
    And I press "grm"
    And I press "C-g"
    And I press "wC"
    And I type "line has changed."
    Then I should see exactly:
    """
    This line has changed.
    This line has changed.
    This line has changed.
    """

  # TODO cc doesn't work, but C
  @evil-change-line-mark-all-dwim @todo-outstanding
  Scenario: Change a whole line
    When I replace the buffer text with:
    """
    This is a line.
    That is a line.
    This is a line.
    That is a line.
    That is a line.
    """
    And I press "grm"
    And I press "C-g"
    And I type "C"
    And I type "The line has changed."
    Then I should see exactly:
    """
    The line has changed.
    That is a line.
    The line has changed.
    That is a line.
    That is a line.
    """

  # TODO cc doesn't work, but C and c$ work
  @evil-change-lines-selection-mark-all-dwim @outstanding-todo
  Scenario: Change a whole line (consecutive lines)
    When I replace the buffer text with:
    """
    That is a line.
    That is a line.
    That is a line.
    That is a line.
    That is a line.
    """
    And I press "grm"
    Then I should have 5 cursors
    And I press "C-g"
    And I type "C"
    And I type "The line has changed."
    Then I should see exactly:
    """
    The line has changed.
    The line has changed.
    The line has changed.
    The line has changed.
    The line has changed.
    """

  @evil-change-whole-viz-line-mark-all-dwim @failing
  Scenario: Change a whole visual line
    When I replace the buffer text with:
    """
    This is a line.
    That is a line.
    This is a line.
    That is a line.
    """
    And I press "grm"
    And I press "C-g"
    And I press "Vc"
    And I type "The line has changed."
    Then I should see exactly:
    """
    The line has changed.
    That is a line.
    The line has changed.
    That is a line.
    """

  @evil-change-whole-line-with-count-mark-all-dwim @failing
  Scenario: Change a whole line with count
    When I replace the buffer text with:
    """
    This is a line.
    The next line.
    The last line.
    This is a line.
    The next line.
    The last line.
    The last line.
    """
    And I press "grm"
    And I press "C-g"
    And I press "2cc"
    And I type "The first two lines have changed."
    Then I should see exactly:
    """
    The first two lines have changed.
    The last line.
    The first two lines have changed.
    The last line.
    """
