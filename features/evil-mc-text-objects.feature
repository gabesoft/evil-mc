Feature: Text objects
  Scenario: Should surround a word with quotes
    When I replace the buffer text with:
    """
    This is a simple line.
    This is a simple line.
    That is a simple line.
    This is a simple line.
    """
    And I press "grm"
    And I type "fmviwS'"
    Then I should see:
    """
    This is a 'simple' line.
    This is a 'simple' line.
    That is a simple line.
    This is a 'simple' line.
    """

  Scenario: Should change a parenthesis expression inner
    When I replace the buffer text with:
    """
    This is a (sentence) with brackets.
    This is a (sentence) with brackets.
    This is a (sentence) with brackets.
    """
    And I press "grm"
    And I type "f(cibchanged"
    Then I should see:
    """
    This is a (changed) with brackets.
    This is a (changed) with brackets.
    This is a (changed) with brackets.
    """

  Scenario: Should change a parenthesis expression outer
    When I replace the buffer text with:
    """
    This is a (sentence) with brackets.
    This is a (sentence) with brackets.
    This is a (sentence) with brackets.
    """
    And I press "grm"
    And I type "f(cabchanged"
    Then I should see:
    """
    This is a changed with brackets.
    This is a changed with brackets.
    This is a changed with brackets.
    """
