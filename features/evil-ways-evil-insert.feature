@evil-ways @evil-insert
Feature: Insert and change text commands from normal and visual state should be reflected in the buffer
  Background:
    Given I turn on evil-mode
    And I bind evil keys for multiple-cursors mode 

  @change-text-visual-state
  Scenario: Change text from cursors in visual state
    When I replace the buffer text with "aaa"
    And I press "vgrm"
    And I type "cfirst text "
    Then I should see exactly:
    """
    first text first text first text 
    """

  @change-text-normal-state
  Scenario: Change text from cursors in normal state
    When I replace the buffer text with "bbb"
    And I press "vgrm"
    And I press "C-g"
    And I type "clfirst text "
    Then I should see exactly:
    """
    first text first text first text 
    """

  @enter-new-lines-visual
  Scenario: Enter new lines from cursors in visual state
    When I replace the buffer text with "bbb"
    And I press "vgrm"
    And I press "c"
    And I press "word" followed by enter
    Then I should see exactly:
    """
    word
    word
    word

    """

  @enter-new-lines-normal
  Scenario: Enter new lines from cursors in normal state
    When I replace the buffer text with "bbb"
    And I press "vgrm"
    And I press "C-g"
    And I press "cl"
    And I press "word" followed by enter
    Then I should see exactly:
    """
    word
    word
    word

    """

  Scenario: Open line below from cursors in normal state
    When I replace the buffer text with "bbb"
    And I press "vgrm"
    And I press "C-g"
    And I press "oabc"
    Then I should see exactly:
    """
    bbb
    abc
    abc
    abc
    """

  Scenario: Open line above from cursors in normal state
    When I replace the buffer text with "bbb"
    And I press "vgrm"
    And I press "C-g"
    And I press "Oabc"
    Then I should see exactly:
    """
    abc
    abc
    abc
    bbb
    """

  @insert-at-cursor
  Scenario: Insert at cursor
    When I replace the buffer text with "a a a"
    And I press "vgrm"
    And I press "C-g"
    And I press "i-y-"
    Then I should see "-y-a -y-a -y-a"

  @insert-after-cursor
  Scenario: Insert after cursor
    When I replace the buffer text with "a a a"
    And I press "vgrm"
    Then The cursors should have these properties:
      | type        |  id | point | mark | evil-state |
      | main-cursor | nil |     1 |    1 | visual     |
      | fake-cursor |   3 |     3 |    3 | visual     |
      | fake-cursor |   4 |     5 |    5 | visual     |
    And I press "C-g"
    Then The cursors should have these properties:
      | type        |  id | point | mark | evil-state |
      | main-cursor | nil |     1 |    1 | normal     |
      | fake-cursor |   3 |     3 |    3 | normal     |
      | fake-cursor |   4 |     5 |    5 | normal     |
    And I press "a-x-"
    Then I should see "a-x- a-x- a-x-"

  Scenario: Insert at the beginning of line
    When I replace the buffer text with:
    """
    This is a line
    This is a line
    This is a line
    """
    And I go to word "line"
    And I press "vgrm"
    And I press "C-g"
    And I type "Istart "
    Then I should see exactly:
    """
    start This is a line
    start This is a line
    start This is a line
    """

  Scenario: Insert at the end of line
    When I replace the buffer text with:
    """
    This is a line
    This is a line
    This is a line
    """
    And I go to word "line"
    And I press "vgrm"
    And I press "C-g"
    And I type "A end"
    Then I should see exactly:
    """
    This is a line end
    This is a line end
    This is a line end
    """

  Scenario: Insert with several cursors on a empty line
    When I replace the buffer text with:
    """
                   x
    """
    And I type "fxxhhhhv"
    And I press "C->"
    And I press "C->"
    And I type "Iabc "
    Then I should see exactly:
    """
              abc  abc  abc    
    """

  # TODO: make these work with evil-append and evil-append-line as well
  @evil-insert-on-empty-lines-mark-all-like-this
  Scenario: Insert with cursors on multiple empty lines
    When I replace the buffer text with:
    """
    line
    line
    line
    line
    """
    And I type "vegrm"
    And I press "C-g"
    Then The cursors should have these properties:
      | type        |  id | point | mark | evil-state |
      | main-cursor | nil |     4 |    1 | normal     |
      | fake-cursor |   3 |     9 |    6 | normal     |
      | fake-cursor |   4 |    14 |   11 | normal     |
      | fake-cursor |   5 |    19 |   16 | normal     |
    And I type "bC"
    Then The cursors should have these properties:
      | type        |  id | point | mark | evil-state |
      | main-cursor | nil |     1 |    1 | insert     |
      | fake-cursor |   3 |     2 |    2 | insert     |
      | fake-cursor |   4 |     3 |    3 | insert     |
      | fake-cursor |   5 |     4 |    4 | insert     |
    And I type "       "
    And I press "<escape>"
    Then The cursors should have these properties:
      | type        |  id | point | mark | evil-state |
      | main-cursor | nil |     7 |    1 | normal     |
      | fake-cursor |   3 |    15 |    9 | normal     |
      | fake-cursor |   4 |    23 |   17 | normal     |
      | fake-cursor |   5 |    31 |   25 | normal     |
    And I type "iabc"
    Then I should see exactly:
    """
          abc 
          abc 
          abc 
          abc 
    """

  @evil-insert-on-empty-lines-mark-all-dwim
  Scenario: Insert with cursors on multiple empty lines
    When I replace the buffer text with:
    """
    line
    line
    line
    line
    """
    And I type "grm"
    Then The cursors should have these properties:
      | type        |  id | point | mark | evil-state |
      | main-cursor | nil |     1 |    4 | visual     |
      | fake-cursor |   5 |     6 |    9 | visual     |
      | fake-cursor |   6 |    11 |   14 | visual     |
      | fake-cursor |   7 |    16 |   19 | visual     |
    And I press "C-g"
    Then The cursors should have these properties:
      | type        |  id | point | mark | evil-state |
      | main-cursor | nil |     1 |    4 | normal     |
      | fake-cursor |   5 |     6 |    9 | normal     |
      | fake-cursor |   6 |    11 |   14 | normal     |
      | fake-cursor |   7 |    16 |   19 | normal     |
    And I type "C"
    Then The cursors should have these properties:
      | type        |  id | point | mark | evil-state |
      | main-cursor | nil |     1 |    1 | insert     |
      | fake-cursor |   5 |     2 |    2 | insert     |
      | fake-cursor |   6 |     3 |    3 | insert     |
      | fake-cursor |   7 |     4 |    4 | insert     |
    And I type "       "
    Then The cursors should have these properties:
      | type        |  id | point | mark | evil-state |
      | main-cursor | nil |     8 |    1 | insert |
      | fake-cursor |   5 |    16 |    9 | insert |
      | fake-cursor |   6 |    24 |   17 | insert |
      | fake-cursor |   7 |    32 |   25  | insert |
    And I press "<escape>"
    Then The cursors should have these properties:
      | type        |  id | point | mark | evil-state |
      | main-cursor | nil |     7 |    1 | normal     |
      | fake-cursor |   5 |    15 |    9 | normal     |
      | fake-cursor |   6 |    23 |   17 | normal     |
      | fake-cursor |   7 |    31 |   25 | normal     |
    And I type "iabc"
    Then I should see exactly:
    """
          abc 
          abc 
          abc 
          abc 
    """
