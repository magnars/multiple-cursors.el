Feature: Mark things

  Scenario: Mark symbols in defun with select
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
