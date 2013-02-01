Feature: Marking multiple parts of the buffer

  Scenario: Marking next like this, cursors
    When I insert "This text has the word text in it"
    And I select "text"
    And I press "C->"
    Then I should have 2 cursors

  Scenario: Marking next like this, region
    Given I turn on delete-selection-mode
    When I insert "This text has the word text in it"
    And I select "text"
    And I press "C->"
    And I type "sentence"
    Then I should see "This sentence has the word sentence in it"

  Scenario: Skipping a mark
    Given I turn on delete-selection-mode
    When I insert "Here's text, text and text"
    And I select "text"
    And I press "C->"
    And I press "C-0 C->"
    And I type "more"
    Then I should see "Here's more, text and more"

  Scenario: Removing last fake
    When I insert "Here's text, text and text"
    And I select "text"
    And I press "C->"
    And I press "C-- C->"
    Then I should have one cursor

  Scenario: Removing furthest mark
    Given I turn on delete-selection-mode
    When I insert "Here's text, text and text"
    And I select "text"
    And I press "C->"
    And I press "C->"
    And I press "C-- C->"
    And I type "more"
    Then I should see "Here's more, more and text"

  Scenario: Marking prev like this, cursors
    When I insert "This text has the word text in it"
    And I select the last "text"
    And I press "C-<"
    Then I should have 2 cursors

  Scenario: Marking prev like this, region
    Given I turn on delete-selection-mode
    When I insert "This text has the word text in it"
    And I select the last "text"
    And I press "C-<"
    And I type "sentence"
    Then I should see "This sentence has the word sentence in it"

  Scenario: Skipping a prev mark
    Given I turn on delete-selection-mode
    When I insert "Here's text, text and text"
    And I select the last "text"
    And I press "C-<"
    And I press "C-0 C-<"
    And I type "more"
    Then I should see "Here's more, text and more"

  Scenario: Removing first fake
    When I insert "Here's text, text and text"
    And I select the last "text"
    And I press "C-<"
    And I press "C-- C-<"
    Then I should have one cursor

  Scenario: Removing first mark
    Given I turn on delete-selection-mode
    When I insert "Here's text, text and text"
    And I select the last "text"
    And I press "C-<"
    And I press "C-<"
    And I press "C-- C-<"
    And I type "more"
    Then I should see "Here's text, more and more"

  Scenario: Marking all
    When I insert "Here's text, text and text"
    And I select "text"
    And I press "M-!"
    Then I should have 3 cursors

  Scenario: Marking in region
    Given I turn on delete-selection-mode
    When I insert "Here's text, text and text"
    And I select "text, text"
    And I press "M-# text <return>"
    And I type "more"
    Then I should have 2 cursors
    And I should see "Here's more, more and text"

  Scenario: Marking without an active region
    When I insert:
    """
    aaa
    bbb
    ccc
    """
    And I go to the front of the word "bbb"
    And I press "C->"
    And I type "_"
    Then I should have 2 cursors
    And I should see:
    """
    aaa
    _bbb
    _ccc
    """

  Scenario: Increasing number of cursors without an active region
    When I insert:
    """
    aaa
    bbb
    ccc
    """
    And I go to the front of the word "bbb"
    And I press "C->"
    And I press "C-<"
    And i press "C-f"
    And I type "_"
    Then I should have 3 cursors
    And I should see:
    """
    a_aa
    b_bb
    c_cc
    """

  Scenario: Multiple cursor with shift selection
    When I insert "This text contains the word text twice"
    And I go to the front of the word "text"
    And I press "M-S-f"
    And I press "C->"
    And I press "C-f"
    And I press "<deletechar>"
    Then I should see "This text ontains the word text wice"

