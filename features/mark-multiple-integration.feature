Feature: Mark multiple integration
  In order to quickly and precisely get multiple cursors
  As an Emacs user with mark-multiple
  I want to mark multiple regions and then go to multiple-cursors-mode

  Scenario: Mark two words and change them
    Given there is no region selected
    And delete-selection-mode is active
    When I insert "This text contains the word text twice"
    And I select "text"
    And I press "C->"
    And I type "sentence"
    Then I should see "This sentence contains the word sentence twice"

  Scenario: Mark two words and go to multiple cursors
    Given there is no region selected
    When I insert "This text contains the word text twice"
    And I select "text"
    And I press "C->"
    And I press "C-g"
    And I type "'"
    And I press "M-f"
    And I type "'"
    Then I should see "This 'text' contains the word 'text' twice"
