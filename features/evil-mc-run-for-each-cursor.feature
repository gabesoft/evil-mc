Feature Run custom command for each cursor

  Scenario: The custom command should run at each cursor real and fake
    When I replace the buffer text with:
    """
    There is a 0 on this line
    There is a 0 on this line
    There is a 0 on this line
    There is a 0 on this line
    There is a 0 on this line
    There is a 0 on this line
    """
    And I press "vgrm"
    And I press "f0"
    And I call "evil-mc-inc-num-at-each-cursor"
    Then I should see:
    """
    There is a 0 on this line
    There is a 1 on this line
    There is a 2 on this line
    There is a 3 on this line
    There is a 4 on this line
    There is a 5 on this line
    """