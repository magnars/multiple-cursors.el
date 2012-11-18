Feature: Mark all do-what-I-mean (html)

  Background:
    Given I turn on html-mode
    And I turn on delete-selection-mode
    And I insert:
    """
    <body>
      <div class="abc"> def </div>
      <div class="ghi"> jkl </div>
    </body>
    """

  Scenario: Marks tags in html-mode, from front
    When I go to the front of the word "abc"
    And I press "M-b"
    And I press "M-b"
    And I press "M-$"
    And I type "h1"
    Then I should see:
    """
    <body>
      <h1 class="abc"> def </h1>
      <div class="ghi"> jkl </div>
    </body>
    """

  Scenario: Marks tags in html-mode, from back
    When I go to the end of the word "jkl"
    And I press "M-f"
    And I press "M-$"
    And I type "h1"
    Then I should see:
    """
    <body>
      <div class="abc"> def </div>
      <h1 class="ghi"> jkl </h1>
    </body>
    """

  Scenario: Marks tags in html-mode, from outside front
    When I go to the front of the word "abc"
    And I press "M-b"
    And I press "M-b"
    And I press "C-b"
    And I press "M-$"
    And I type "h1"
    Then I should see:
    """
    <body>
      <h1 class="abc"> def </h1>
      <div class="ghi"> jkl </div>
    </body>
    """

  Scenario: Marks tags in html-mode, from outside back
    When I go to the end of the word "jkl"
    And I press "M-f"
    And I press "C-f"
    And I press "M-$"
    And I type "h1"
    Then I should see:
    """
    <body>
      <div class="abc"> def </div>
      <h1 class="ghi"> jkl </h1>
    </body>
    """

  Scenario: Marks words in html-mode
    When I go to the front of the word "abc"
    And I press "M-$"
    And I type "def"
    Then I should see:
    """
    <body>
      <div class="def"> def </div>
      <div class="ghi"> jkl </div>
    </body>
    """

  Scenario: Marks words in html-mode
    When I go to the front of the word "abc"
    And I press "M-$"
    And I type "def"
    And I press "M-$"
    And I type "hah"
    Then I should see:
    """
    <body>
      <div class="hah"> hah </div>
      <div class="ghi"> jkl </div>
    </body>
    """
