Feature: Mark all do-what-I-mean

  Background:
    Given I turn on emacs-lisp-mode
    And I turn on delete-selection-mode
    And I insert:
    """
    (defun abc (ghi) (message ghi))
    (defun def (ghi) (message some-other-ghi))

    """

  Scenario: Mark symbols in defun
    When I go to the end of the word "abc"
    And I press "M-f"
    And I press "M-$"
    And I type "hmm"
    Then I should see:
    """
    (defun abc (hmm) (message hmm))
    (defun def (ghi) (message some-other-ghi))
    """
    When I press "C-g"
    And I go to the front of the word "hmm"
    And I press "C-$"
    And I type "foo"
    Then I should see:
    """
    (defun abc (foo) (message foo))
    (defun def (ghi) (message some-other-ghi))
    """
    
  Scenario: Mark all symbols by pressing twice
    When I go to the end of the word "abc"
    And I press "M-f"
    And I press "M-$"
    And I press "M-$"
    And I type "hmm"
    Then I should see:
    """
    (defun abc (hmm) (message hmm))
    (defun def (hmm) (message some-other-hmm))
    """
    When I press "C-g"
    And I press "M->"
    And I insert:
    """
    (defun def (hmm-hmm) (message hmm))
    """
    And I go to the front of the word "hmm"
    And I press "C-$"
    And I press "C-$"
    And I type "humm"
    Then I should see:
    """
    (defun abc (humm) (message humm))
    (defun def (humm) (message some-other-humm))
    (defun def (humm-humm) (message humm))
    """

  Scenario: Mark dwim from selection
    When I press "M-<"
    And I press "S-M->"
    And I press "C-$ ghi RET"
    And I type "xyz"
    Then I should see:
    """
    (defun abc (xyz) (message xyz))
    (defun def (xyz) (message some-other-xyz))
    """
    When I press "C-g"
    And I go to the front of the word "xyz"
    And I press "C-M-SPC"
    And I press "C-$"
    And I type "foo"
    Then I should see:
    """
    (defun abc (foo) (message foo))
    (defun def (xyz) (message some-other-xyz))
    """
    When I press "C-g"
    And I press "M-<"
    And I press "S-M->"
    And I press "C-u C-$"
    And I type ";;"
    Then I should see:
    """
    ;;(defun abc (foo) (message foo))
    ;;(defun def (xyz) (message some-other-xyz))
    """
