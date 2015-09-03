Feature: Record current command info
  

  Scenario: Record commands to copy lines
    Given I have at least one cursor
    When I insert:
    """
    This is the start of text
    This is the second line
    This is the third line
    This is the fourth line
    This is the fifth line
    """
    And I go to the beginning of buffer
    Then These examples should pass:
    | keys | command     |
    | yy   | evil-yank   |
    | dd   | evil-delete |
    | cc   | evil-change |
    | 3yy  | evil-yank   |
    | 3dd  | evil-delete |
    | 3cc  | evil-change |

  Scenario: Record the command to copy a full line
    Given I have at least one cursor
    When I press "yy"
    Then The recorded command name should be "evil-yank"
    And The recorded command keys should be "yy"

  Scenario: Record the command to copy the next 3 lines
    Given I have at least one cursor
    When I press "3yy"
    Then The recorded command name should be "evil-yank"
    And The recorded command keys should be "3yy"

  Scenario: Record the command to copy to up to a character
    Given I have one cursor at "text" in "This text contains a -1."
    When I press "yt-"
    Then The recorded command name should be "evil-yank"
    And The recorded command keys should be "yt-"

  Scenario: Record the command to copy to and including a character
    Given I have one cursor at "text" in "This text contains a -1."
    When I press "yf-"
    Then The recorded command name should be "evil-yank"
    And The recorded command keys should be "yf-"

  Scenario: Record the command to copy upto t
    Given I have one cursor at "text" in "This text contains a t."
    When I press "ytt"
    Then The recorded command name should be "evil-yank"
    And The recorded command keys should be "ytt"

  Scenario: Record the command to copy to and including f
    Given I have one cursor at "text" in "This text contains a f."
    When I press "yff"
    Then The recorded command name should be "evil-yank"
    And The recorded command keys should be "yff"

  Scenario: Record the command to join two lines
    Given I have one cursor at "line" in "First line.\nSecond line."
    When I press "J"
    Then The recorded command name should be "evil-join"
    And The recorded command keys should be "J"