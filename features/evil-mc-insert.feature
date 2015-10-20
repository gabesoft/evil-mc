Feature: Insert and change text

  Scenario: Text typed in insert state should be entered into the buffer
    When I replace the buffer text with "aaa"
    And I press "vgrm"
    And I type "clfirst text "
    Then I should see:
    """
    first text first text first text
    """

  Scenario: Enter new lines
    When I replace the buffer text with "bbb"
    And I press "vgrm"
    And I press "cl"
    And I press "word" followed by enter
    Then I should see:
    """
    word
    word
    word
    """

  Scenario: Open line below
    When I replace the buffer text with "bbb"
    And I press "vgrm"
    And I press "oabc"
    Then I should see:
    """
    bbb
    abc
    abc
    abc
    """

  Scenario: Open line above
    When I replace the buffer text with "bbb"
    And I press "vgrm"
    And I press "Oabc"
    Then I should see:
    """
    abc
    abc
    abc
    bbb
    """

  Scenario: Insert at cursor
    When I replace the buffer text with "a a a"
    And I press "vgrm"
    And I press "i-y-"
    Then I should see "-y-a -y-a -y-a"

  Scenario: Insert after cursor
    When I replace the buffer text with "a a a"
    And I press "vgrm"
    And I press "a-x-"
    Then I should see "a-x- a-x- a-x-"

  Scenario: Insert at the beginning of line
    When I replace the buffer text with:
    """
    This is a line
    This is a line
    This is a line
    """
    And I go to word "line"
    And I press "vgrm"
    And I type "Istart "
    Then I should see:
    """
    start This is a line
    start This is a line
    start This is a line
    """

  Scenario: Insert at the end of line
    When I replace the buffer text with:
    """
    This is a line
    This is a line
    This is a line
    """
    And I go to word "line"
    And I press "vgrm"
    And I type "A end"
    Then I should see:
    """
    This is a line end
    This is a line end
    This is a line end
    """

  Scenario: Change a letter
    When I replace the buffer text with "xyz xyz xyz"
    And I press "grm"
    And I type "clw"
    Then I should see "xyw xyw xyw"

  Scenario: Change a word
    When I replace the buffer text with "xyz xyz xyz"
    And I press "grm"
    And I type "bcwabc"
    Then I should see "abc abc abc"

  Scenario: Change a word at the beginning of line
    When I replace the buffer text with:
    """
    This is a line
    This is a line
    This is a line
    """
    And I press "grm"
    And I type "bcwabc"
    Then I should see:
    """
    abc is a line
    abc is a line
    abc is a line
    """

  Scenario: Change to the end of word
    When I replace the buffer text with "xyz xyz xyz"
    And I press "grm"
    And I type "bceabc"
    Then I should see "abc abc abc"

  Scenario: Change to the end of word with count
    When I replace the buffer text with:
    """
    xyz yyz yyz xyz yyz yyz xyz yyz yyz xyz yyz yyz
    """
    And I press "grm"
    And I type "b2ceabc"
    Then I should see "abc yyz abc yyz abc yyz abc yyz"

  Scenario: Change up to a letter (f)
    When I replace the buffer text with "another-test another-test another-test"
    And I press "grm"
    And I type "bbbcftxyz"
    Then I should see "xyzher-test xyzher-test xyzher-test"

  Scenario: Change up to a letter (f) with count
    When I replace the buffer text with "another-test another-test another-test"
    And I press "grm"
    And I type "bbb2cftxyz"
    Then I should see "xyzest xyzest xyzest"

  Scenario: Change up till before a letter (t)
    When I replace the buffer text with "another-test another-test another-test"
    And I press "grm"
    And I type "bbbcttxyz"
    Then I should see "xyzther-test xyzther-test xyzther-test"

  Scenario: Change a visual selection
    When I replace the buffer text with "another-test another-test another-test"
    And I press "grm"
    And I type "bbbv4lcxyz"
    Then I should see "xyzer-test xyzer-test xyzer-test"

  Scenario: Change a visual selection 2
    When I replace the buffer text with:
    """
    This is a simple line.
    This is a simple line.
    This is a simple line.
    That is a simple line.
    """
    And I press "grm"
    And I type "bvt.cChanged text"
    Then I should see:
    """
    Changed text.
    Changed text.
    Changed text.
    That is a simple line.
    """

  Scenario: Change until the end of line
    When I replace the buffer text with:
    """
    This is a line.
    This is a line.
    This is a line.
    """
    And I press "grm"
    And I press "wC"
    And I type "line has changed."
    Then I should see:
    """
    This line has changed.
    This line has changed.
    This line has changed.
    """

  # TODO ensure this works for consecutive lines and
  #      passes for [ "cc" "^cc" "$cc" ]
  Scenario: Change a whole line
    When I replace the buffer text with:
    """
    This is a line.
    That is a line.
    This is a line.
    That is a line.
    That is a line.
    """
    And I press "grm"
    And I type "cc"
    And I type "The line has changed."
    Then I should see:
    """
    The line has changed.
    That is a line.
    The line has changed.
    That is a line.
    """

  Scenario: Change a whole visual line
    When I replace the buffer text with:
    """
    This is a line.
    That is a line.
    This is a line.
    That is a line.
    """
    And I press "grm"
    And I press "Vc"
    And I type "The line has changed."
    Then I should see:
    """
    The line has changed.
    That is a line.
    The line has changed.
    That is a line.
    """

  Scenario: Change a whole line with count
    When I replace the buffer text with:
    """
    This is a line.
    The next line.
    The last line.
    This is a line.
    The next line.
    The last line.
    The last line.
    """
    And I press "grm"
    And I press "2cc"
    And I type "The first two lines have changed."
    Then I should see:
    """
    The first two lines have changed.
    The last line.
    The first two lines have changed.
    The last line.
    """