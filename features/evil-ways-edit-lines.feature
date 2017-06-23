@evil-ways @evil-ways-edit-lines
Feature: Evil Ways Edit Lines Going Up And Down In Buffer
  Background:
    Given I turn on evil-mode
    And I insert:
    """
    First  line
    Second line
    Third  line
    Fourth line
    Fifth  line
    No     line
    """
    And I go to beginning of buffer

  @edit-bols-down
  Scenario: Multiple cursors can edit lines in visual state moving down in buffer
    And I press "vj"
    And I press "C-S-c C-S-c"
    And I press "i"
    And I type "and "
    Then I should see exactly:
    """
    and First  line
    and Second line
    Third  line
    Fourth line
    Fifth  line
    No     line
    """

  @edit-bols-down
  Scenario: Multiple cursors can edit lines in visual state moving down in buffer one line above end of buffer
    And I press "v4j"
    And I press "C-S-c C-S-c"
    And I press "i"
    And I type "and "
    Then I should see exactly:
    """
    and First  line
    and Second line
    and Third  line
    and Fourth line
    and Fifth  line
    No     line
    """

  @edit-bols-up
  Scenario: Multiple cursors can edit lines in visual state moving up in buffer
    And I go to end of buffer
    And I go to beginning of line
    And I press "vk"
    And I press "C-S-c C-S-c"
    And I press "i"
    And I type "and "
    Then I should see exactly:
    """
    First  line
    Second line
    Third  line
    Fourth line
    and Fifth  line
    and No     line
    """

  @edit-bols-up
  Scenario: Multiple cursors can edit lines in visual state moving up in buffer one line below beginning of buffer
    And I go to end of buffer
    And I go to beginning of line
    And I press "v4k"
    And I press "C-S-c C-S-c"
    And I press "i"
    And I type "and "
    Then I should see exactly:
    """
    First  line
    and Second line
    and Third  line
    and Fourth line
    and Fifth  line
    and No     line
    """

  @edit-bols-viz-line-down
  Scenario: Multiple cursors can edit lines in visual state line selection moving down in buffer
    And I press "Vj"
    And I press "C-S-c C-S-c"
    Then I should have 2 cursors
    And I press "i"
    And I type "and "
    Then I should see exactly:
    """
    and First  line
    and Second line
    Third  line
    Fourth line
    Fifth  line
    No     line
    """

  @edit-bols-viz-line-down
  Scenario: Multiple cursors can edit lines in visual state line selection moving down in buffer
    And I press "V3j"
    And I press "C-S-c C-S-c"
    Then I should have 4 cursors
    And I press "i"
    And I type "and "
    Then I should see exactly:
    """
    and First  line
    and Second line
    and Third  line
    and Fourth line
    Fifth  line
    No     line
    """

  @edit-bols-viz-line-down
  Scenario: Multiple cursors can edit lines in visual state line selection moving down in buffer one line above end of buffer
    And I press "V4j"
    And I press "C-S-c C-S-c"
    Then I should have 5 cursors
    And I press "i"
    And I type "and "
    Then I should see exactly:
    """
    and First  line
    and Second line
    and Third  line
    and Fourth line
    and Fifth  line
    No     line
    """

  @edit-bols-viz-line-down
  Scenario: Multiple cursors can edit lines in visual state line selection moving down in buffer
    And I press "V"
    And I press "C-S-c C-S-c"
    Then I should have one cursor
    And I press "i"
    And I type "and "
    Then I should see exactly:
    """
    and First  line
    Second line
    Third  line
    Fourth line
    Fifth  line
    No     line
    """

  @edit-bols-viz-line-down
  Scenario: Multiple cursors can edit lines in visual state line selection moving down to end of buffer
    And I press "VG"
    And I press "C-S-c C-S-c"
    Then I should have 6 cursors
    And I press "i"
    And I type "and "
    Then I should see exactly:
    """
    and First  line
    and Second line
    and Third  line
    and Fourth line
    and Fifth  line
    and No     line
    """

  @edit-bols-viz-line-up
  Scenario: Multiple cursors can edit lines in visual state line selection moving up in buffer
    And I go to end of buffer
    And I go to beginning of line
    And I press "Vk"
    And I press "C-S-c C-S-c"
    Then I should have 2 cursors
    And I press "i"
    And I type "and "
    Then I should see exactly:
    """
    First  line
    Second line
    Third  line
    Fourth line
    and Fifth  line
    and No     line
    """

  @edit-bols-viz-line-up
  Scenario: Multiple cursors can edit lines in visual state line selection moving up in buffer
    And I go to end of buffer
    And I go to beginning of line
    And I press "V3k"
    And I press "C-S-c C-S-c"
    Then I should have 4 cursors
    And I press "i"
    And I type "and "
    Then I should see exactly:
    """
    First  line
    Second line
    and Third  line
    and Fourth line
    and Fifth  line
    and No     line
    """

  @edit-bols-viz-line-up
  Scenario: Multiple cursors can edit lines in visual state line selection moving up in buffer one line below beginning of buffer
    And I go to end of buffer
    And I go to beginning of line
    And I press "V4k"
    And I press "C-S-c C-S-c"
    Then I should have 5 cursors
    And I press "i"
    And I type "and "
    Then I should see exactly:
    """
    First  line
    and Second line
    and Third  line
    and Fourth line
    and Fifth  line
    and No     line
    """

  @edit-bols-viz-line-up
  Scenario: Multiple cursors can edit lines in visual state line selection moving up in buffer
    And I go to end of buffer
    And I go to beginning of line
    And I press "V"
    And I press "C-S-c C-S-c"
    Then I should have one cursor
    And I press "i"
    And I type "and "
    Then I should see exactly:
    """
    First  line
    Second line
    Third  line
    Fourth line
    Fifth  line
    and No     line
    """

  @edit-bols-viz-line-up
  Scenario: Multiple cursors can edit lines in visual state line selection moving up to beginning of buffer
    And I go to end of buffer
    And I go to beginning of line
    And I press "V5k"
    And I press "C-S-c C-S-c"
    Then I should have 6 cursors
    And I press "i"
    And I type "and "
    Then I should see exactly:
    """
    and First  line
    and Second line
    and Third  line
    and Fourth line
    and Fifth  line
    and No     line
    """

  @edit-bols-viz-line-up
  Scenario: Multiple cursors can edit lines in visual state line selection moving up to beginning of buffer
    And I go to end of buffer
    And I go to beginning of line
    And I press "Vgg"
    And I press "C-S-c C-S-c"
    Then I should have 6 cursors
    And I press "i"
    And I type "and "
    Then I should see exactly:
    """
    and First  line
    and Second line
    and Third  line
    and Fourth line
    and Fifth  line
    and No     line
    """

  Scenario: Multiple cursors can edit lines in visual state line selection moving down in buffer
    And I press "flVjjj"
    And I press "C-S-c C-S-c"
    And I press "i"
    And I type "fe"
    Then I should see exactly:
    """
    First  feline
    Second feline
    Third  feline
    Fourth feline
    Fifth  line
    No     line
    """

  Scenario: Multiple cursors can edit lines in visual state moving down in buffer
    And I press "flvjj"
    And I press "C-S-c C-S-c"
    And I press "i"
    And I type "fe"
    Then I should see exactly:
    """
    First  feline
    Second feline
    Third  feline
    Fourth line
    Fifth  line
    No     line
    """
