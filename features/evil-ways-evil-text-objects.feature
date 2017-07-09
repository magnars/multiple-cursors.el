@evil-ways @evil-text-objects
Feature: Text objects -- surround and exchange
  Background:
    Given I turn on evil-mode
    And I bind evil keys for multiple-cursors mode 

  @surround-change-bracket-type
  Scenario: Change the bracket type
    When I replace the buffer text with:
    """
    This is a (very) very (long) line with (lots of) words.
    """
    And I press "f(v"
    And I press "grm"
    And I press "C-g"
    And I press "csbB"
    Then I should see exactly:
    """
    This is a {very} very {long} line with {lots of} words.
    """

  Scenario: Delete brackets inner
    When I replace the buffer text with:
    """
    This is a (very) very (long) line with (lots of) words.
    """
    And I press "f(v"
    And I press "grm"
    And I press "C-g"
    And I press "dib"
    Then I should see exactly:
    """
    This is a () very () line with () words.
    """

  Scenario: Delete brackets outer
    When I replace the buffer text with:
    """
    This is a (very) very (long) line with (lots of) words.
    """
    And I press "f(v"
    And I press "grm"
    And I press "C-g"
    And I press "dab"
    Then I should see exactly:
    """
    This is a  very  line with  words.
    """

  Scenario: Copy brackets inner
    When I replace the buffer text with:
    """
    This is a (normal) line.
    This is a (normal) line.
    This is a (normal) line.
    """
    And I press "fn"
    And I press "grm"
    And I press "C-g"
    And I press "yib"
    And I press "$p"
    Then I should see exactly:
    """
    This is a (normal) line.normal
    This is a (normal) line.normal
    This is a (normal) line.normal
    """

  Scenario: Copy brackets outer
    When I replace the buffer text with:
    """
    This is a (normal) line.
    This is a (normal) line.
    This is a (normal) line.
    """
    And I press "fn"
    And I press "grm"
    And I press "C-g"
    And I press "yab"
    And I press "$p"
    Then I should see exactly:
    """
    This is a (normal) line.(normal)
    This is a (normal) line.(normal)
    This is a (normal) line.(normal)
    """

  Scenario: Change a parenthesis expression inner
    When I replace the buffer text with:
    """
    This is a (sentence) with brackets.
    This is a (sentence) with brackets.
    This is a (sentence) with brackets.
    """
    And I press "grm"
    And I press "C-g"
    And I type "f(cibchanged"
    Then I should see exactly:
    """
    This is a (changed) with brackets.
    This is a (changed) with brackets.
    This is a (changed) with brackets.
    """

  Scenario: Change a parenthesis expression outer
    When I replace the buffer text with:
    """
    This is a (sentence) with brackets.
    This is a (sentence) with brackets.
    This is a (sentence) with brackets.
    """
    And I press "grm"
    And I press "C-g"
    And I type "f(cabchanged"
    Then I should see exactly:
    """
    This is a changed with brackets.
    This is a changed with brackets.
    This is a changed with brackets.
    """

  @surround-with-quotes
  Scenario: Surround a word with quotes
    When I replace the buffer text with:
    """
    This is a simple line.
    This is a simple line.
    That is a simple line.
    This is a simple line.
    """
    And I press "grm"
    And I press "C-g"
    And I type "fmviwS'"
    Then I should see exactly:
    """
    This is a 'simple' line.
    This is a 'simple' line.
    That is a simple line.
    This is a 'simple' line.
    """

  @surround-with-quotes
  Scenario: Surround a word with quotes
    When I replace the buffer text with:
    """
    This is a simple line.
    This is a simple line.
    That is a simple line.
    This is a simple line.
    """
    And I press "grm"
    And I press "C-g"
    And I type "fmbveS'"
    Then I should see exactly:
    """
    This is a 'simple' line.
    This is a 'simple' line.
    That is a simple line.
    This is a 'simple' line.
    """

  @evil-exchange
  Scenario: exchange two words
    When I replace the buffer text with:
    """
    test string
    test string
    """
    And I press "C->"
    And I press "gxe"
    And I press "w"
    And I press "gxe"
    Then I should see exactly:
    """
    string test
    string test
    """
