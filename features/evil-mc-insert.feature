Feature: Insert text

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

  Scenario: Insert with several cursors on a empty line
    When I replace the buffer text with:
    """
                   x
    """
    And I type "fxxhhhhv"
    And I press "C-n"
    And I press "C-n"
    And I type "iabc "
    Then I should see:
    """
              abc  abc  abc    
    """

  # TODO: make this work with evil-append and evil-append-line as well
  Scenario: Insert with cursors on multiple empty lines
    When I replace the buffer text with:
    """
    line
    line
    line
    line
    """
    And I type "grm"
    And I type "bC"
    And I type "        "
    And I press "<escape>"
    And I type "i"
    And I press "<escape>"
    And I type "iabc"
    Then I should see:
    """
          abc  
          abc  
          abc  
          abc  
    """
