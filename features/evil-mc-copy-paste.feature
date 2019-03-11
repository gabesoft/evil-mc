Feature: Copy paste

  Scenario: Copy paste a word (before)
    When I replace the buffer text with:
    """
    Here is a list of words, some are small, some big and some huge.
    """
    And I press "4fs"
    And I press "grm"
    And I press "bywbbP"
    Then I should see:
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
    And I press "bywbbp"
    Then I should see:
    """
    Here is a list of wsome ords, some are ssome mall, some bsome ig and some huge.
    """

  Scenario: Copy paste a word with count
    When I replace the buffer text with:
    """
    blue and big and purple and big and pink and big and small
    """
    And I press "fg"
    And I press "grm"
    And I press "bb3ywP"
    Then I should see:
    """
    blue and big and and big and purple and big and and big and pink and big and and big and small
    """

  Scenario: Copy paste up to a letter
    When I replace the buffer text with:
    """
    Here are some words.
    Here are some words.
    Here are some words.
    """
    And I press "grm"
    And I press "ytmbP"
    Then I should see:
    """
    e are soHere are some words.
    e are soHere are some words.
    e are soHere are some words.
    """

  Scenario: Copy paste till before a letter
    When I replace the buffer text with:
    """
    Here are some words.
    Here are some words.
    Here are some words.
    """
    And I press "grm"
    And I press "yfmbP"
    Then I should see:
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
    And I type "fsy$$p"
    Then I should see:
    """
    Here are some words.some words.
    Here are some words.some words.
    Here are some words.some words.
    """

  Scenario: Copy paste a line
    When I replace the buffer text with:
    """
    Here are some words.
    There are some words.
    Here are some words.
    There are some words.
    """
    And I press "grm"
    And I press "yyp"
    Then I should see:
    """
    Here are some words.
    Here are some words.
    There are some words.
    Here are some words.
    Here are some words.
    There are some words.
    """

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
    And I press "2yyP"
    Then I should see:
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

  Scenario: Copy paste with registers
    When I replace the buffer text with:
    """
    Here are some words.
    Here are some words.
    Here are some words.
    """
    And I press "grm"
    And I press "b"
    And I set the register to "a" then type "yw"
    And I press "w"
    And I set the register to "b" then type "yw"
    And I press "w"
    And I set the register to "c" then type "yw"
    And I press "$"
    And I set the register to "a" then type "p"
    And I set the register to "b" then type "p"
    And I set the register to "c" then type "p"
    Then I should see:
    """
    Here are some words.Here are some 
    Here are some words.Here are some 
    Here are some words.Here are some 
    """

  Scenario: Copy paste with kill-ring
    When I replace the buffer text with:
    """
    This is the first line
    This is the second line
    This is the third line
    This is the fourth line

    """
    And I press "grm"
    And I press "C-n"
    And I press "w"
    And I press "3yw"
    And I press "grq"
    And I press "G"
    And I press "C-x r y"
    Then I should see:
    """
    This is the first line
    This is the second line
    This is the third line
    This is the fourth line
    is the first 
    is the second 
    is the third 
    is the fourth 
    """