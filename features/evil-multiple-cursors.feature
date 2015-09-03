Feature: Record current command info
  
  Scenario: Record commands to copy lines
    When I insert:
    """
    This is the start of text -1 -1 -1 t t t f f f k k k
    This is the second line -1 -1 -1 t t t f f f k k k
    This is the third line -1 -1 -1 t t t f f f k k k
    """
    And I go to the beginning of buffer
    Given I have at least one cursor
    Then these examples should pass:
    | keys | command   |
    | yy   | evil-yank |
    | 3yy  | evil-yank |
    | yt-  | evil-yank |
    | ytk  | evil-yank |
    | 3yt- | evil-yank |
    | ytt  | evil-yank |
    | yff  | evil-yank |

  Scenario: Record commands to change lines
    When I insert:
    """
    This is the start of text -1 -1 -1 t t t f f f k k k
    This is the second line -1 -1 -1 t t t f f f k k k
    This is the third line -1 -1 -1 t t t f f f k k k
    """
    And I go to the beginning of buffer
    Given I have at least one cursor
    And The cursors are frozen
    Then These examples with undo should pass:
    | keys | command     |
    | cc   | evil-change |
    | ctk  | evil-change |
    | cfk  | evil-change |
    | ctt  | evil-change |
    | cff  | evil-change |
    | 3ctk | evil-change |
    | 3cfk | evil-change |
    | 3ctt | evil-change |
    | 3cff | evil-change |
    | 3cc  | evil-change |

  Scenario: Record the command to join two lines
    Given I have one cursor at "line" in "First line.\nSecond line."
    When I press "J"
    Then The recorded command name should be "evil-join"
    And The recorded command keys should be "J"