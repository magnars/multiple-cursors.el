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

  Scenario: Marking S-expressions
    When I insert:
    """
    (let ((x 3) (y (+ 1 1))
          (z 1))
      (+ x y z))
    """
    And I go to the front of the word "x"
    And I press "C-b"
    And I press "C-2 M-x mc/mark-next-sexps"
    Then I should have 3 cursors

  Scenario: Marking S-expressions
    When I insert:
    """
    (let ((x 3) (y (+ 1 1))
          (z 1))
      (+ x y z))
    """
    And I go to the front of the word "x"
    And I press "C-b"
    And I press "C-2 M-x mc/mark-next-sexps"
    And I type "!"
    Then I should have 3 cursors
    And I should see:
    """
    (let (!(x 3)! (y (+ 1 1))!
          (z 1))
      (+ x y z))
    """

  Scenario: Marking S-expressions with comments
    Given I turn on lisp-mode
    When I insert:
    """
    (let (x
          ;; y is bound to 2
          (y (+ 1 1))
          ;; z is bound to 1
          (z 1))
      (+ x y z))
    """
    And I go to the front of the word "x"
    And I press "C-2 M-x mc/mark-next-sexps"
    And I press "C-M-k"
    Then I should have 3 cursors
    And I should see:
    """
    (let ()
      (+ x y z))
    """

  Scenario: Marking S-expressions with comments backwards
    Given I turn on lisp-mode
    When I insert:
    """
    (let (x
          ;; y is bound to 2
          (y (+ 1 1))
          ;; z is bound to 1
          (z 1))
      (+ x y z))
    """
    And I go to the front of the word "z"
    And I press "C-b"
    And I press "C-2 M-x mc/mark-previous-sexps"
    And I press "C-M-k"
    Then I should have 3 cursors
    And I should see:
    """
    (let (
          ;; y is bound to 2

          ;; z is bound to 1
          )
    """

  Scenario: Editing S-expressions
    When I insert:
    """
    (let (x y z)
      (or x y z))
    """
    And I go to the front of the word "x"
    And I press "M-x mc/mark-next-sexps"
    And I press "C-M-f C-j C-g"
    And I press "C-2 C-M-u"
    And I press "M-x indent-sexp"
    Then I should see:
    """
    (let (x
          y
          z)
      (or x y z))
    """

  Scenario: Editing S-expressions
    When I insert:
    """
    (let (x y z)
      (+ x y z))
    """
    And I go to the front of the word "x"
    And I press "M-x mc/mark-next-sexps"
    And I press "C-M-f C-j"
    And I press "M-x mc/mark-next-sexps"
    And I press "C-M-b"
    And I type "("
    And I press "C-M-f"
    And I type " "
    And I press "M-x mc/insert-numbers"
    And I type ")"
    And I press "C-g C-2 C-M-u"
    And I press "M-x indent-sexp"
    Then I should see:
    """
    (let ((x 0)
          (y 1)
          (z 2))
      (+ x y z))
    """

  Scenario: Multiple cursor with shift selection
    When I insert "This text contains the word text twice"
    And I go to the front of the word "text"
    And I press "M-S-f"
    And I press "C->"
    And I press "C-f"
    And I press "<deletechar>"
    Then I should see "This text ontains the word text wice"

