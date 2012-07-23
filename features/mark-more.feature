Feature: Marking multiple parts of the buffer

  Scenario: Marking next like this, cursors
    When I insert "This text has the word text in it"
    And I select "text"
    And I press "M->"
    Then I should have 2 cursors

  Scenario: Marking next like this, region
    Given I turn on delete-selection-mode
    When I insert "This text has the word text in it"
    And I select "text"
    And I press "M->"
    And I type "sentence"
    Then I should see "This sentence has the word sentence in it"

  Scenario: Skipping a mark
    Given I turn on delete-selection-mode
    When I insert "Here's text, text and text"
    And I select "text"
    And I press "M->"
    And I press "C-0 M->"
    And I type "more"
    Then I should see "Here's more, text and more"

  Scenario: Removing last fake
    When I insert "Here's text, text and text"
    And I select "text"
    And I press "M->"
    And I press "C-- M->"
    Then I should have one cursor

  Scenario: Removing furthest mark
    Given I turn on delete-selection-mode
    When I insert "Here's text, text and text"
    And I select "text"
    And I press "M->"
    And I press "M->"
    And I press "C-- M->"
    And I type "more"
    Then I should see "Here's more, more and text"
