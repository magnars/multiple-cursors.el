Feature: Mark things

  Scenario: Mark all symbols like this with select
    Given I turn on emacs-lisp-mode
    And I turn on delete-selection-mode
    And I insert:
    """
    (defun abc (ghi) (message ghi))
    (defun def (ghi) (message some-other-ghi))
    """
    When I select "ghi"
    And I mark all symbols like this
    And I type "hmm"
    Then I should see:
    """
    (defun abc (hmm) (message hmm))
    (defun def (hmm) (message some-other-ghi))
    """

  Scenario: Mark all words like this with select
    Given I turn on emacs-lisp-mode
    And I turn on delete-selection-mode
    And I insert:
    """
    (defun abc (ghi) (message ghi))
    (defun def (ghi) (message some-other-ghi))
    """
    When I select "ghi"
    And I mark all words like this
    And I type "hmm"
    Then I should see:
    """
    (defun abc (hmm) (message hmm))
    (defun def (hmm) (message some-other-hmm))
    """

  Scenario: Mark all symbols like this in defun with select
    Given I turn on emacs-lisp-mode
    And I turn on delete-selection-mode
    And I insert:
    """
    (defun abc (ghi) (message ghi))
    (defun def (ghi) (message some-other-ghi))
    """
    When I select "ghi"
    And I mark all symbols like this in defun
    And I type "hmm"
    Then I should see:
    """
    (defun abc (hmm) (message hmm))
    (defun def (ghi) (message some-other-ghi))
    """

  Scenario: Mark all words like this in defun with select
    Given I turn on emacs-lisp-mode
    And I turn on delete-selection-mode
    And I insert:
    """
    (defun abc (ghi) (message ghi))
    (defun def (ghi) (message some-other-ghi))
    """
    When I select "ghi"
    And I mark all words like this in defun
    And I type "hmm"
    Then I should see:
    """
    (defun abc (hmm) (message hmm))
    (defun def (ghi) (message some-other-ghi))
    """

  Scenario: Mark all symbols like this with no select
    Given I turn on emacs-lisp-mode
    And I turn on delete-selection-mode
    And I insert:
    """
    (defun abc (ghi) (message ghi))
    (defun def (ghi) (message some-other-ghi))
    """
    When I go to word "ghi"
    And I mark all symbols like this
    And I type "hmm"
    Then I should see:
    """
    (defun abc (hmm) (message hmm))
    (defun def (hmm) (message some-other-ghi))
    """

  Scenario: Mark all words like this with no select
    Given I turn on emacs-lisp-mode
    And I turn on delete-selection-mode
    And I insert:
    """
    (defun abc (ghi) (message ghi))
    (defun def (ghi) (message some-other-ghi))
    """
    When I go to word "ghi"
    And I mark all words like this
    And I type "hmm"
    Then I should see:
    """
    (defun abc (hmm) (message hmm))
    (defun def (hmm) (message some-other-hmm))
    """

  Scenario: Mark all symbols like this in defun with no select
    Given I turn on emacs-lisp-mode
    And I turn on delete-selection-mode
    And I insert:
    """
    (defun abc (ghi) (message ghi))
    (defun def (ghi) (message some-other-ghi))
    """
    When I go to word "ghi"
    And I mark all symbols like this in defun
    And I type "hmm"
    Then I should see:
    """
    (defun abc (hmm) (message hmm))
    (defun def (ghi) (message some-other-ghi))
    """

  Scenario: Mark all words like this in defun with no select
    Given I turn on emacs-lisp-mode
    And I turn on delete-selection-mode
    And I insert:
    """
    (defun abc (ghi) (message ghi))
    (defun def (ghi) (message some-other-ghi))
    """
    When I go to word "ghi"
    And I mark all words like this in defun
    And I type "hmm"
    Then I should see:
    """
    (defun abc (hmm) (message hmm))
    (defun def (ghi) (message some-other-ghi))
    """
