@evil-ways @evil-find
Feature: Find a character
  Background:
    Given I turn on evil-mode
    And I bind evil keys for multiple-cursors mode 

  Scenario: Should find character to
    When I replace the buffer text with:
    """
    This is the start of text -1 -1 -1 t t t f f f k k k
    This is the second line -1 -1 -1 t t t f f f k k k
    This is the third line -1 -1 -1 t t t f f f k k k
    """
    And I press "vgrm"
    And I press "C-g"
    And I type "tkrx"
    Then I should see exactly:
    """
    This is the start of text -1 -1 -1 t t t f f fxk k k
    This is the second line -1 -1 -1 t t t f f fxk k k
    This is the third line -1 -1 -1 t t t f f fxk k k
    """

  Scenario: Should find character
    When I replace the buffer text with:
    """
    This is the start of text -1 -1 -1 t t t f f f k k k
    This is the second line -1 -1 -1 t t t f f f k k k
    This is the third line -1 -1 -1 t t t f f f k k k
    """
    And I press "vgrm"
    And I press "C-g"
    And I type "fkrx"
    Then I should see exactly:
    """
    This is the start of text -1 -1 -1 t t t f f f x k k
    This is the second line -1 -1 -1 t t t f f f x k k
    This is the third line -1 -1 -1 t t t f f f x k k
    """

  Scenario: Should find character to with count
    When I replace the buffer text with:
    """
    This is the start of text -1 -1 -1 t t t f f f k k k
    This is the second line -1 -1 -1 t t t f f f k k k
    This is the third line -1 -1 -1 t t t f f f k k k
    """
    And I press "vgrm"
    And I press "C-g"
    And I type "2tkrx"
    Then I should see exactly:
    """
    This is the start of text -1 -1 -1 t t t f f f kxk k
    This is the second line -1 -1 -1 t t t f f f kxk k
    This is the third line -1 -1 -1 t t t f f f kxk k
    """

  Scenario: Should find character with count
    When I replace the buffer text with:
    """
    This is the start of text -1 -1 -1 t t t f f f k k k
    This is the second line -1 -1 -1 t t t f f f k k k
    This is the third line -1 -1 -1 t t t f f f k k k
    """
    And I press "vgrm"
    And I press "C-g"
    And I type "2fkrx"
    Then I should see exactly:
    """
    This is the start of text -1 -1 -1 t t t f f f k x k
    This is the second line -1 -1 -1 t t t f f f k x k
    This is the third line -1 -1 -1 t t t f f f k x k
    """

  Scenario: Should find character to with repeat
    When I replace the buffer text with:
    """
    This is the start of text -1 -1 -1 t t t f f f k k k
    This is the second line -1 -1 -1 t t t f f f k k k
    This is the third line -1 -1 -1 t t t f f f k k k
    """
    And I press "vgrm"
    And I press "C-g"
    And I type "tk;;rx"
    Then I should see exactly:
    """
    This is the start of text -1 -1 -1 t t t f f f k kxk
    This is the second line -1 -1 -1 t t t f f f k kxk
    This is the third line -1 -1 -1 t t t f f f k kxk
    """

  Scenario: Should find character with repeat
    When I replace the buffer text with:
    """
    This is the start of text -1 -1 -1 t t t f f f k k k
    This is the second line -1 -1 -1 t t t f f f k k k
    This is the third line -1 -1 -1 t t t f f f k k k
    """
    And I press "vgrm"
    And I press "C-g"
    And I type "fk;;rx"
    Then I should see exactly:
    """
    This is the start of text -1 -1 -1 t t t f f f k k x
    This is the second line -1 -1 -1 t t t f f f k k x
    This is the third line -1 -1 -1 t t t f f f k k x
    """

  Scenario: Should find character to with repeat backwards
    When I replace the buffer text with:
    """
    This is the start of text -1 -1 -1 t t t f f f k k k
    This is the second line -1 -1 -1 t t t f f f k k k
    This is the third line -1 -1 -1 t t t f f f k k k
    """
    And I press "vgrm"
    And I press "C-g"
    And I type "tk;;,rx"
    Then I should see exactly:
    """
    This is the start of text -1 -1 -1 t t t f f f kxk k
    This is the second line -1 -1 -1 t t t f f f kxk k
    This is the third line -1 -1 -1 t t t f f f kxk k
    """

  Scenario: Should find character with repeat backwards
    When I replace the buffer text with:
    """
    This is the start of text -1 -1 -1 t t t f f f k k k
    This is the second line -1 -1 -1 t t t f f f k k k
    This is the third line -1 -1 -1 t t t f f f k k k
    """
    And I press "vgrm"
    And I press "C-g"
    And I type "fk;;,rx"
    Then I should see exactly:
    """
    This is the start of text -1 -1 -1 t t t f f f k x k
    This is the second line -1 -1 -1 t t t f f f k x k
    This is the third line -1 -1 -1 t t t f f f k x k
    """

  Scenario: Should find character to with repeat and count
    When I replace the buffer text with:
    """
    This is the start of text -1 -1 -1 t t t f f f k k k k k k k k k
    This is the second line -1 -1 -1 t t t f f f k k k k k k k k k
    This is the third line -1 -1 -1 t t t f f f k k k k k k k k k
    """
    And I press "vgrm"
    And I press "C-g"
    And I type "2tk;;rx"
    Then I should see exactly:
    """
    This is the start of text -1 -1 -1 t t t f f f k k kxk k k k k k
    This is the second line -1 -1 -1 t t t f f f k k kxk k k k k k
    This is the third line -1 -1 -1 t t t f f f k k kxk k k k k k
    """

  Scenario: Should find character with repeat and count
    When I replace the buffer text with:
    """
    This is the start of text -1 -1 -1 t t t f f f k k k k k k k k k
    This is the second line -1 -1 -1 t t t f f f k k k k k k k k k
    This is the third line -1 -1 -1 t t t f f f k k k k k k k k k
    """
    And I press "vgrm"
    And I press "C-g"
    And I type "2fk;;rx"
    Then I should see exactly:
    """
    This is the start of text -1 -1 -1 t t t f f f k k k x k k k k k
    This is the second line -1 -1 -1 t t t f f f k k k x k k k k k
    This is the third line -1 -1 -1 t t t f f f k k k x k k k k k
    """
