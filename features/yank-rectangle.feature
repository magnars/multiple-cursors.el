Feature: Yank rectangle

  # Rectangle

  Scenario: Yanking Rectangle with same number
    When I insert:
    """
    This is some text
    This is more text, and so is this.
    This is even more text
    """
    And I go to point "9"
    And I press "C-SPC"
    And I press "M-f"
    And I press "C-n"
    And I press "C-n"
    And I press "C-x r k"
    And I select the last "text"
    And I mark all like this
    And I press "H-3"
    Then I should see:
    """
    This is  some
    This is  more, and so is this.
    This is  more even
    """
    And I should have 3 cursors

  Scenario: Yanking Rectangle with More Cursors (wrap around)
    When I insert:
    """
    This is some text
    This is more text, and so is this.
    This is even more text
    """
    And I go to point "9"
    And I press "C-SPC"
    And I press "M-f"
    And I press "C-n"
    And I press "C-n"
    And I press "C-x r k"
    And I select the last "is"
    And I mark all like this
    And I press "H-3 y"
    Then I should see:
    """
    Thsome more  text
    Theven some  text, and so more theven.
    Thsome more  more text
    """
    And I should have 8 cursors

  Scenario: Yanking Rectangle with More Cursors (not wrap around)
    When I insert:
    """
    This is some text
    This is more text, and so is this.
    This is even more text
    """
    And I go to point "9"
    And I press "C-SPC"
    And I press "M-f"
    And I press "C-n"
    And I press "C-n"
    And I press "C-x r k"
    And I select the last "is"
    And I mark all like this
    And I press "H-3 n"
    Then I should see:
    """
    Thsome more  text
    Theven   text, and so  th.
    Th   more text
    """
    And I should have 8 cursors

  Scenario: Yanking Rectangle with fewer cursors
    When I insert:
    """
    This is some text
    This is more text, and so is this.
    This is even more text
    """
    And I go to point "9"
    And I press "C-SPC"
    And I press "M-f"
    And I press "C-n"
    And I press "C-n"
    And I press "C-x r k"
    And I select the last "and"
    And I mark all like this
    And I press "H-3"
    Then I should see:
    """
    This is  text
    This is  text, some so is this.
    This is  more text
    """
    And I should have one cursor

  # Kill-ring

  Scenario: Yanking kill-ring with same number
    When I insert:
    """
    (Here is
     some text
     later)
    This is some text
    This is more text, and so is this.
    This is even more text
    """
    And I go to point "0"
    And I press "C-M-k"
    And I select the last "text"
    And I mark all like this
    And I press "C-u H-3"
    Then I should see:
    """

    This is some (Here is
    This is more  some text, and so is this.
    This is even more  later)
    """
    And I should have 3 cursors

  Scenario: Yanking kill-ring with More Cursors (wrap around)
    When I insert:
    """
    (Here is
     some text
     for use
     later)
    This is some text
    This is more text, and so is this.
    This is even more text
    """
    And I go to point "0"
    And I press "C-M-k"
    And I select the last "is"
    And I mark all like this
    And I press "C-u H-3 y"
    Then I should see:
    """

    Th(Here is  some text some text
    Th for use  later) more text, and so (Here is th some text.
    Th for use  later) even more text
    """
    And I should have 8 cursors

  Scenario: Yanking kill-ring with More Cursors (not wrap around)
    When I insert:
    """
    (Here is
     some text
     for use
     later)
    This is some text
    This is more text, and so is this.
    This is even more text
    """
    And I go to point "0"
    And I press "C-M-k"
    And I select the last "is"
    And I mark all like this
    And I press "C-u H-3 n"
    Then I should see:
    """

    Th(Here is  some text some text
    Th for use  later) more text, and so  th.
    Th  even more text
    """
    And I should have 8 cursors

  Scenario: Yanking kill-ring with fewer cursors
    When I insert:
    """
    (Here is
     some text
     for use
     later)
    This is some text
    This is more text, and so is this.
    This is even more text
    """
    And I go to point "0"
    And I press "C-M-k"
    And I select the last "text"
    And I mark all like this
    And I press "C-u H-3"
    Then I should see:
    """

    This is some (Here is
    This is more  some text, and so is this.
    This is even more  for use
    """
    And I should have 3 cursors
