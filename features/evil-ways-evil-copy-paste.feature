@evil-ways @evil-copy-paste
Feature: Copy paste
  Background:
    Given I turn on evil-mode
    And I bind evil keys for multiple-cursors mode 

  Scenario: Copy paste a word (before)
    When I replace the buffer text with:
    """
    Here is a list of words, some are small, some big and some huge.
    """
    And I press "4fs"
    And I press "grm"
    And I press "C-g"
    And I press "ywbbP"
    Then I should see exactly:
    """
    Here is a list of some words, some are some small, some some big and some huge.
    """

  Scenario: Copy paste a word (after)
    When I replace the buffer text with:
    """
    Here is a list of words, some are small, some big and some huge.
    """
    And I press "4fs"
    And I press "grm"
    And I press "C-g"
    And I press "ywbbp"
    Then I should see exactly:
    """
    Here is a list of wsome ords, some are ssome mall, some bsome ig and some huge.
    """

  @copy-paste-word-with-count
  Scenario: Copy paste a word with count
    When I replace the buffer text with:
    """
    blue and big and purple and big and pink and big and small
    """
    And I press "fg"
    And I press "grm"
    And I press "C-g"
    And I press "b3ywP"
    Then I should see exactly:
    """
    blue and big and and big and purple and big and and big and pink and big and and big and small
    """

  @copy-paste-up-to-letter
  Scenario: Copy paste up to a letter
    When I replace the buffer text with:
    """
    Here are some words.
    Here are some words.
    Here are some words.
    """
    And I press "grm"
    And I press "C-g"
    And I press "eytmbP"
    Then I should see exactly:
    """
    e are soHere are some words.
    e are soHere are some words.
    e are soHere are some words.
    """

  @copy-paste-till-before-letter
  Scenario: Copy paste till before a letter
    When I replace the buffer text with:
    """
    Here are some words.
    Here are some words.
    Here are some words.
    """
    And I press "grm"
    And I press "C-g"
    And I press "eyfmbP"
    Then I should see exactly:
    """
    e are somHere are some words.
    e are somHere are some words.
    e are somHere are some words.
    """

  Scenario: Copy paste until the end of the line
    When I replace the buffer text with:
    """
    Here are some words.
    Here are some words.
    Here are some words.
    """
    And I press "grm"
    And I press "C-g"
    And I type "fsy$$p"
    Then I should see exactly:
    """
    Here are some words.some words.
    Here are some words.some words.
    Here are some words.some words.
    """

  @copy-paste-a-line @failing
  Scenario: Copy paste a line
    When I replace the buffer text with:
    """
    Here are some words.
    There are some words.
    Here are some words.
    There are some words.
    """
    And I press "grm"
    And I press "C-g"
    And I press "yyp"
    Then I should see exactly:
    """
    Here are some words.
    Here are some words.
    There are some words.
    Here are some words.
    Here are some words.
    There are some words.
    """

  @copy-paste-a-line-with-count @failing
  Scenario: Copy paste a line with count
    When I replace the buffer text with:
    """
    Here are some words.
    There are some words.
    Here are some words.
    There are some words.
    Here are some words.
    """
    And I press "grm"
    And I press "C-g"
    And I press "2yyP"
    Then I should see exactly:
    """
    Here are some words.
    There are some words.
    Here are some words.
    There are some words.
    Here are some words.
    There are some words.
    Here are some words.
    There are some words.
    """

  @copy-paste-with-registers @failing
  Scenario: Copy paste with registers
    When I replace the buffer text with:
    """
    Here are some words.
    Here are some words.
    Here are some words.
    """
    And I press "grm"
    And I press "C-g"
    And I set the register to "a" then type "yw"
    And I press "w"
    And I set the register to "b" then type "yw"
    And I press "w"
    And I set the register to "c" then type "yw"
    And I press "$"
    And I set the register to "a" then type "p"
    And I set the register to "b" then type "p"
    And I set the register to "c" then type "p"
    Then I should see exactly:
    """
    Here are some words.Here are some 
    Here are some words.Here are some 
    Here are some words.Here are some 
    """
