Feature: Mark all do-what-I-mean

  Scenario: Mark symbols in defun
    Given I turn on emacs-lisp-mode
    And I turn on delete-selection-mode
    And I insert:
    """
    (defun abc (ghi) (message ghi))
    (defun def (ghi) (message some-other-ghi))
    """
    When I go to the end of the word "abc"
    And I press "M-f"
    And I press "M-$"
    And I type "hmm"
    Then I should see:
    """
    (defun abc (hmm) (message hmm))
    (defun def (ghi) (message some-other-ghi))
    """

  Scenario: Mark all symbols by pressing twice
    Given I turn on emacs-lisp-mode
    And I turn on delete-selection-mode
    And I insert:
    """
    (defun abc (ghi) (message ghi))
    (defun def (ghi) (message ghi))
    """
    When I go to the end of the word "abc"
    And I press "M-f"
    And I press "M-$"
    And I press "M-$"
    And I type "hmm"
    Then I should see:
    """
    (defun abc (hmm) (message hmm))
    (defun def (hmm) (message hmm))
    """
