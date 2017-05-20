@evil-ways
Feature: Evil Ways
  Background:
    Given I turn on evil-mode
    And I insert:
    """
    First  line
    Second line
    """
    And I go to the front of the word "line"
    And I press "C->"
    Then I should have 2 cursors
    Then The cursors should have these properties:
      | type        | id  | point | mark | evil-state |
      | main-cursor | nil |     8 | nil  | normal     |
      | fake-cursor | 1   |    20 | nil  | normal     |

  @state-transitions @nini
  Scenario: normal -> insert -> normal -> insert state transition
    And I press "i"
    Then I should have 2 cursors
    Then The cursors should have these properties:
      | type        | id  | point | mark | evil-state |
      | main-cursor | nil |     8 | nil  | insert     |
      | fake-cursor | 1   |    20 | nil  | insert     |
    And I press "<escape>"
    Then I should have 2 cursors
    Then The cursors should have these properties:
      | type        | id  | point | mark | evil-state |
      | main-cursor | nil |     7 | nil  | normal |
      | fake-cursor | 1   |    19 | nil  | normal |
    And I press "xxif"
    Then I should have 2 cursors
    Then The cursors should have these properties:
      | type        | id  | point | mark | evil-state |
      | main-cursor | nil |     8 | nil  | insert     |
      | fake-cursor | 1   |    19 | nil  | insert     |
    Then I should see exactly:
    """
    First fine
    Secondfine
    """

  @state-transitions @nvn
  Scenario: normal -> visual -> normal state transition
    And I press "v"
    Then I should have 2 cursors
    Then The cursors should have these properties:
      | type        | id  | point | mark | evil-state |
      | main-cursor | nil |     8 |    8 | visual     |
      | fake-cursor | 1   |    20 |   20 | visual     |
    And I press "<escape>"
    Then I should have 2 cursors
    Then The cursors should have these properties:
      | type        | id  | point | mark | evil-state |
      | main-cursor | nil |     8 |    8 | normal     |
      | fake-cursor | 1   |    20 |   20 | normal     |
    And I press "xx"
    Then I should have 2 cursors
    Then The cursors should have these properties:
      | type        | id  | point | mark | evil-state |
      | main-cursor | nil |     8 |    8 | normal     |
      | fake-cursor | 1   |    18 |   18 | normal     |
    Then I should see exactly:
    """
    First  ne
    Second ne
    """

  @state-transitions @nvin
  Scenario: normal -> visual -> insert -> normal state transition
    And I press "v"
    Then I should have 2 cursors
    Then The cursors should have these properties:
      | type        | id  | point | mark | evil-state |
      | main-cursor | nil |     8 |    8 | visual     |
      | fake-cursor | 1   |    20 |   20 | visual     |
    And I press "I"
    Then I should have 2 cursors
    Then The cursors should have these properties:
      | type        | id  | point | mark | evil-state |
      | main-cursor | nil |     8 |    9 | insert     |
      | fake-cursor | 1   |    20 |   21 | insert     |
    And I press "<escape>"
    Then I should have 2 cursors
    Then The cursors should have these properties:
      | type        | id  | point | mark | evil-state |
      | main-cursor | nil |     7 |    9 | normal     |
      | fake-cursor | 1   |    19 |   21 | normal     |
    And I press "x"
    Then I should see exactly:
    """
    First line
    Secondline
    """

  @state-transitions @nvnin
  Scenario: normal -> visual -> normal -> insert -> normal state transition
    And I press "v"
    And I type "xif"
    # TODO find out why?
    # This wont work
    # And I type "vxif"
    # idk why the (20,21) coordinates go to evil char twice
    # insted of (8,9) followed by (20,21)
    Then I should have 2 cursors
    Then The cursors should have these properties:
      | type        | id  | point | mark | evil-state |
      | main-cursor | nil |     9 |    8 | insert |
      | fake-cursor | 1   |    21 |   20 | insert |
    And I press "<escape>"
    Then I should have 2 cursors
    Then The cursors should have these properties:
      | type        | id  | point | mark | evil-state |
      | main-cursor | nil |     8 |    8 | normal   |
      | fake-cursor | 1   |    20 |   20 | normal   |
    And I press "x"
    Then I should have 2 cursors
    Then The cursors should have these properties:
      | type        | id  | point | mark | evil-state |
      | main-cursor | nil |     8 |    8 | normal   |
      | fake-cursor | 1   |    19 |   19 | normal   |
    And I press "icoral"
    Then I should see exactly:
    """
    First  coraline
    Second coraline
    """

  @state-transitions @nvninvi
  Scenario: normal -> visual -> normal -> insert -> normal -> visual -> insert state transition
    And I press "v"
    Then I should have 2 cursors
    Then The cursors should have these properties:
      | type        | id  | point | mark | evil-state |
      | main-cursor | nil |     8 |    8 | visual |
      | fake-cursor | 1   |    20 |   20 | visual |
    And I press "xia"
    Then I should have 2 cursors
    Then The cursors should have these properties:
      | type        | id  | point | mark | evil-state |
      | main-cursor | nil |     9 |    8 | insert     |
      | fake-cursor | 1   |    21 |   20 | insert     |
    And I press "<escape>"
    Then I should have 2 cursors
    Then The cursors should have these properties:
      | type        | id  | point | mark | evil-state |
      | main-cursor | nil |     8 |    8 | normal   |
      | fake-cursor | 1   |    20 |   20 | normal   |
    # TODO find out why?
    # This should work but doesnt
    # And I press "xvI"
    # This should work too, but doesn't because of above
    # And I press "xvIcoral"
    And I press "xv"
    Then I should have 2 cursors
    Then The cursors should have these properties:
      | type        | id  | point | mark | evil-state |
      | main-cursor | nil |     8 |    8 | visual     |
      | fake-cursor | 1   |    19 |   19 | visual     |
    And I press "I"
    Then I should have 2 cursors
    Then The cursors should have these properties:
      | type        | id  | point | mark | evil-state |
      | main-cursor | nil |     8 |    9 | insert     |
      | fake-cursor | 1   |    19 |   20 | insert     |
    And I press "coral"
    Then I should see exactly:
    """
    First  coraline
    Second coraline
    """

  @normal-state-motions @find-char-to
  Scenario: Motion find char to 'e' from normal state
    And I press "tex"
    Then I should see exactly:
    """
    First  lie
    Second lie
    """

  @normal-state-motions @end-of-word
  Scenario: Motion move end of word from normal state
    And I press "ean"
    Then I should see exactly:
    """
    First  linen
    Second linen
    """

  @normal-state-motions @change-word
  Scenario: Motion change word from normal state
    And I press "cwlinen"
    Then I should see exactly:
    """
    First  linen
    Second linen
    """

  @normal-state-motions @delete-end-of-word
  Scenario: Motion delete end of word from normal state
    And I press "de"
    Then I should see exactly:
    """
    First  
    Second 
    """

  @visual-state-motions @end-of-word
  Scenario: Motion move end of word from visual state
    And I press "v"
    And I press "e"
    And I press "An"
    Then I should see exactly:
    """
    First  linen
    Second linen
    """

  @visual-state-motions @change-word
  Scenario: Motion change word from visual state
    And I press "v"
    And I press "e"
    And I press "clinen"
    Then I should see exactly:
    """
    First  linen
    Second linen
    """

  @visual-state-motions @forward-char-motion
  Scenario: Motion forward char then delete from visual state
    And I press "v"
    And I press "lll"
    And I press "d"
    Then I should see exactly:
    """
    First  
    Second 
    """

  @visual-state-motions @forward-char-motion-with-count
  Scenario: Motion forward char with prefix then delete from visual state
    And I press "v"
    And I press "3l"
    And I press "d"
    Then I should see exactly:
    """
    First  
    Second 
    """
