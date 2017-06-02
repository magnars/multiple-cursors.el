@evil-ways @evil-replace
Feature: Replace text should be reflected in buffer
  Background:
    Given I turn on evil-mode
    And I bind evil keys for multiple-cursors mode 

  Scenario: Should replace a character
    When I replace the buffer text with:
    """
    This is the start of text -1 -1 -1 t t t f f f k k k
    This is the second line -1 -1 -1 t t t f f f k k k
    xhis is the third line -1 -1 -1 t t t f f f k k k
    """
    And I press "vgrm"
    And I press "C-g"
    And I type "rx"
    Then I should see exactly:
    """
    xhis is the start of text -1 -1 -1 t t t f f f k k k
    xhis is the second line -1 -1 -1 t t t f f f k k k
    xhis is the third line -1 -1 -1 t t t f f f k k k
    """

  Scenario: Should replace a character with count
    When I replace the buffer text with:
    """
    This is the start of text -1 -1 -1 t t t f f f k k k
    This is the second line -1 -1 -1 t t t f f f k k k
    This is the third line -1 -1 -1 t t t f f f k k k
    """
    And I press "vgrm"
    And I press "C-g"
    And I type "3rx"
    Then I should see exactly:
    """
    xxxs is the start of text -1 -1 -1 t t t f f f k k k
    xxxs is the second line -1 -1 -1 t t t f f f k k k
    xxxs is the third line -1 -1 -1 t t t f f f k k k
    """

  Scenario: Should replace multiple characters
    When I replace the buffer text with:
    """
    This is the start of text -1 -1 -1 t t t f f f k k k
    This is the second line -1 -1 -1 t t t f f f k k k
    This is the third line -1 -1 -1 t t t f f f k k k
    """
    And I press "vgrm"
    And I press "C-g"
    And I type "Rabc"
    Then I should see exactly:
    """
    abcs is the start of text -1 -1 -1 t t t f f f k k k
    abcs is the second line -1 -1 -1 t t t f f f k k k
    abcs is the third line -1 -1 -1 t t t f f f k k k
    """

  Scenario: Should undo replace on delete
    When I replace the buffer text with:
    """
    This is the start of text -1 -1 -1 t t t f f f k k k
    This is the second line -1 -1 -1 t t t f f f k k k
    This is the third line -1 -1 -1 t t t f f f k k k
    """
    And I press "vgrm"
    And I press "C-g"
    And I type "Rabcdefg"
    And I press "<DEL><DEL><DEL><DEL><DEL><DEL>"
    Then I should see exactly:
    """
    ahis is the start of text -1 -1 -1 t t t f f f k k k
    ahis is the second line -1 -1 -1 t t t f f f k k k
    ahis is the third line -1 -1 -1 t t t f f f k k k
    """
