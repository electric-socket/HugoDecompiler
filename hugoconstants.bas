$Let DEBUG = 0
$Console:Only
Option _Explicit
' CHAR_TRANSLATION is simply a value that is added to an ASCII character
'   in order to encode the text, i.e., make it unreadable to casual
'   browsing.

Const CHAR_TRANSLATION = &H14

Dim Shared As String Keywords(0 To 255)
'
'    Token values

' &h00 - &h0f

Const NULL_T = 0
Const OPEN_BRACKET_T = NULL_T + 1
Const CLOSE_BRACKET_T = OPEN_BRACKET_T + 1
Const DECIMAL_T = CLOSE_BRACKET_T + 1
Const COLON_T = DECIMAL_T + 1
Const EQUALS_T = COLON_T + 1
Const MINUS_T = EQUALS_T + 1
Const PLUS_T = MINUS_T + 1
Const ASTERISK_T = PLUS_T + 1
Const FORWARD_SLASH_T = ASTERISK_T + 1
Const PIPE_T = FORWARD_SLASH_T + 1
Const SEMICOLON_T = PIPE_T + 1
Const OPEN_BRACE_T = SEMICOLON_T + 1
Const CLOSE_BRACE_T = OPEN_BRACE_T + 1
Const OPEN_SQUARE_T = CLOSE_BRACE_T + 1
Const CLOSE_SQUARE_T = OPEN_SQUARE_T + 1

' &h10 - &h1f

Const POUND_T = CLOSE_SQUARE_T + 1
Const TILDE_T = POUND_T + 1
Const GREATER_EQUAL_T = TILDE_T + 1
Const LESS_EQUAL_T = GREATER_EQUAL_T + 1
Const NOT_EQUAL_T = LESS_EQUAL_T + 1
Const AMPERSAND_T = NOT_EQUAL_T + 1
Const GREATER_T = AMPERSAND_T + 1
Const LESS_T = GREATER_T + 1
Const IF_T = LESS_T + 1
Const COMMA_T = IF_T + 1
Const ELSE_T = COMMA_T + 1
Const ELSEIF_T = ELSE_T + 1
Const WHILE_T = ELSEIF_T + 1
Const DO_T = WHILE_T + 1
Const SELECT_T = DO_T + 1
Const CASE_T = SELECT_T + 1

' &h20 - &h2f

Const FOR_T = CASE_T + 1
Const RETURN_T = FOR_T + 1
Const BREAK_T = RETURN_T + 1
Const AND_T = BREAK_T + 1
Const OR_T = AND_T + 1
Const JUMP_T = OR_T + 1
Const RUN_T = JUMP_T + 1
Const IS_T = RUN_T + 1
Const NOT_T = IS_T + 1
Const TRUE_T = NOT_T + 1
Const FALSE_T = TRUE_T + 1
Const LOCAL_T = FALSE_T + 1
Const VERB_T = LOCAL_T + 1
Const XVERB_T = VERB_T + 1
Const HELD_T = XVERB_T + 1
Const MULTI_T = HELD_T + 1

' &h30 - &h3f

Const MULTIHELD_T = MULTI_T + 1
Const NEWLINE_T = MULTIHELD_T + 1
Const ANYTHING_T = NEWLINE_T + 1
Const PRINT_T = ANYTHING_T + 1
Const NUMBER_T = PRINT_T + 1
Const CAPITAL_T = NUMBER_T + 1
Const TEXT_T = CAPITAL_T + 1
Const GRAPHICS_T = TEXT_T + 1
Const COLOR_T = GRAPHICS_T + 1
Const REMOVE_T = COLOR_T + 1
Const MOVE_T = REMOVE_T + 1
Const TO_T = MOVE_T + 1
Const PARENT_T = TO_T + 1
Const SIBLING_T = PARENT_T + 1
Const CHILD_T = SIBLING_T + 1
Const YOUNGEST_T = CHILD_T + 1

' &h40 - &h4f

Const ELDEST_T = YOUNGEST_T + 1
Const YOUNGER_T = ELDEST_T + 1
Const ELDER_T = YOUNGER_T + 1
Const PROP_T = ELDER_T + 1
Const ATTR_T = PROP_T + 1
Const VAR_T = ATTR_T + 1
Const DICTENTRY_T = VAR_T + 1
Const TEXTDATA_T = DICTENTRY_T + 1
Const ROUTINE_T = TEXTDATA_T + 1
Const DEBUGDATA_T = ROUTINE_T + 1
Const OBJECTNUM_T = DEBUGDATA_T + 1
Const VALUE_T = OBJECTNUM_T + 1
Const EOL_T = VALUE_T + 1
Const SYSTEM_T = EOL_T + 1
Const NOTHELD_T = SYSTEM_T + 1
Const MULTINOTHELD_T = NOTHELD_T + 1

' &h50 - &h5f

Const WINDOW_T = MULTINOTHELD_T + 1
Const RANDOM_T = WINDOW_T + 1
Const WORD_T = RANDOM_T + 1
Const LOCATE_T = WORD_T + 1
Const PARSE_T = LOCATE_T + 1
Const CHILDREN_T = PARSE_T + 1
Const IN_T = CHILDREN_T + 1
Const PAUSE_T = IN_T + 1
Const RUNEVENTS_T = PAUSE_T + 1
Const ARRAYDATA_T = RUNEVENTS_T + 1
Const CALL_T = ARRAYDATA_T + 1
Const STRINGDATA_T = CALL_T + 1
Const SAVE_T = STRINGDATA_T + 1
Const RESTORE_T = SAVE_T + 1
Const QUIT_T = RESTORE_T + 1
Const INPUT_T = QUIT_T + 1

' &h60 - &h6f

Const SERIAL_T = INPUT_T + 1
Const CLS_T = SERIAL_T + 1
Const SCRIPTON_T = CLS_T + 1
Const SCRIPTOFF_T = SCRIPTON_T + 1
Const RESTART_T = SCRIPTOFF_T + 1
Const HEX_T = RESTART_T + 1
Const OBJECT_T = HEX_T + 1
Const XOBJECT_T = OBJECT_T + 1
Const STRING_T = XOBJECT_T + 1
Const ARRAY_T = STRING_T + 1
Const PRINTCHAR_T = ARRAY_T + 1
Const UNDO_T = PRINTCHAR_T + 1
Const DICT_T = UNDO_T + 1
Const RECORDON_T = DICT_T + 1
Const RECORDOFF_T = RECORDON_T + 1
Const WRITEFILE_T = RECORDOFF_T + 1

' &h70 -

Const READFILE_T = WRITEFILE_T + 1
Const WRITEVAL_T = READFILE_T + 1
Const READVAL_T = WRITEVAL_T + 1
Const PLAYBACK_T = READVAL_T + 1
Const COLOUR_T = PLAYBACK_T + 1
Const PICTURE_T = COLOUR_T + 1
Const LABEL_T = PICTURE_T + 1
Const SOUND_T = LABEL_T + 1
Const MUSIC_T = SOUND_T + 1
Const REPEAT_T = MUSIC_T + 1
Const ADDCONTEXT_T = REPEAT_T + 1
Const VIDEO_T = ADDCONTEXT_T + 1

Const ENDVERB = &HFF

Keywords(NULL_T) = "(not used)"
Keywords(OPEN_BRACKET_T) = "("
Keywords(CLOSE_BRACKET_T) = ")"
Keywords(DECIMAL_T) = "."
Keywords(COLON_T) = ":"
Keywords(EQUALS_T) = "="
Keywords(MINUS_T) = "-"
Keywords(PLUS_T) = "+"
Keywords(ASTERISK_T) = "*"
Keywords(FORWARD_SLASH_T) = "/"
Keywords(PIPE_T) = "|"
Keywords(SEMICOLON_T) = ";"
Keywords(OPEN_BRACE_T) = "{"
Keywords(CLOSE_BRACE_T) = "}"
Keywords(OPEN_SQUARE_T) = "["
Keywords(CLOSE_SQUARE_T) = "]"

' &h10 - &h1f

Keywords(POUND_T) = "#"
Keywords(TILDE_T) = "~"
Keywords(GREATER_EQUAL_T) = ">="
Keywords(LESS_EQUAL_T) = "<="
Keywords(NOT_EQUAL_T) = "~="
Keywords(AMPERSAND_T) = "&"
Keywords(GREATER_T) = ">"
Keywords(LESS_T) = "<"
Keywords(IF_T) = "if"
Keywords(COMMA_T) = ","
Keywords(ELSE_T) = "else"
Keywords(ELSEIF_T) = "elseif"
Keywords(WHILE_T) = "while"
Keywords(DO_T) = "do"
Keywords(SELECT_T) = "select"
Keywords(CASE_T) = "case"

' &h20 - &h2f

Keywords(FOR_T) = "for"
Keywords(RETURN_T) = "return"
Keywords(BREAK_T) = "break"
Keywords(AND_T) = "and"
Keywords(OR_T) = "or"
Keywords(JUMP_T) = "jump"
Keywords(RUN_T) = "run"
Keywords(IS_T) = "is"
Keywords(NOT_T) = "not"
Keywords(TRUE_T) = "true"
Keywords(FALSE_T) = "false"
Keywords(LOCAL_T) = "local"
Keywords(VERB_T) = "verb"
Keywords(XVERB_T) = "xverb"
Keywords(HELD_T) = "held"
Keywords(MULTI_T) = "multi"

' &h30 - &h3f

Keywords(MULTIHELD_T) = "multiheld"
Keywords(NEWLINE_T) = "newline"
Keywords(ANYTHING_T) = "anything"
Keywords(PRINT_T) = "print"
Keywords(NUMBER_T) = "number"
Keywords(CAPITAL_T) = "capital"
Keywords(TEXT_T) = "text"
Keywords(GRAPHICS_T) = "graphics"
Keywords(COLOR_T) = "color"
Keywords(REMOVE_T) = "remove"
Keywords(MOVE_T) = "move"
Keywords(TO_T) = "to"
Keywords(PARENT_T) = "parent"
Keywords(SIBLING_T) = "sibling"
Keywords(CHILD_T) = "child"
Keywords(YOUNGEST_T) = "youngest"

' &h40 - &h4f

Keywords(ELDEST_T) = "eldest"
Keywords(YOUNGER_T) = "younger"
Keywords(ELDER_T) = "elder"
Keywords(PROP_T) = "property #"
Keywords(ATTR_T) = "attrIbute #"
Keywords(VAR_T) = "var #"
Keywords(DICTENTRY_T) = "dictentry #"
Keywords(TEXTDATA_T) = "textdata #"
Keywords(ROUTINE_T) = "routine #"
Keywords(DEBUGDATA_T) = "debugdata #"
Keywords(OBJECTNUM_T) = "objectnum #"
Keywords(VALUE_T) = "integer constant"
Keywords(EOL_T) = " EOL" 'eol#
Keywords(SYSTEM_T) = "system"
Keywords(NOTHELD_T) = "notheld"
Keywords(MULTINOTHELD_T) = "multinotheld"

' &h50 - &h5f

Keywords(WINDOW_T) = "window"
Keywords(RANDOM_T) = "random"
Keywords(WORD_T) = "word"
Keywords(LOCATE_T) = "locate"
Keywords(PARSE_T) = "parse$"
Keywords(CHILDREN_T) = "children"
Keywords(IN_T) = "in"
Keywords(PAUSE_T) = "pause"
Keywords(RUNEVENTS_T) = "runevents"
Keywords(ARRAYDATA_T) = "arraydata#"
Keywords(CALL_T) = "call"
Keywords(STRINGDATA_T) = "literal" 'literal string
Keywords(SAVE_T) = "save"
Keywords(RESTORE_T) = "restore"
Keywords(QUIT_T) = "quit"
Keywords(INPUT_T) = "input"

' &h60 - &h6f

Keywords(SERIAL_T) = "serial$"
Keywords(CLS_T) = "cls"
Keywords(SCRIPTON_T) = "scripton"
Keywords(SCRIPTOFF_T) = "scriptoff"
Keywords(RESTART_T) = "restart"
Keywords(HEX_T) = "hex"
Keywords(OBJECT_T) = "object"
Keywords(XOBJECT_T) = "xobject"
Keywords(STRING_T) = "string"
Keywords(ARRAY_T) = "array"
Keywords(PRINTCHAR_T) = "printchar"
Keywords(UNDO_T) = "undo"
Keywords(DICT_T) = "dict"
Keywords(RECORDON_T) = "recordon"
Keywords(RECORDOFF_T) = "recordoff"
Keywords(WRITEFILE_T) = "writefile"
Keywords(READFILE_T) = "readfile"
Keywords(WRITEVAL_T) = "writeval"
Keywords(READVAL_T) = "readval"
Keywords(PLAYBACK_T) = "playback"
Keywords(COLOUR_T) = "colour"
Keywords(PICTURE_T) = "picture"
Keywords(LABEL_T) = "label#"
Keywords(SOUND_T) = "sound"
Keywords(MUSIC_T) = "music"
Keywords(REPEAT_T) = "repeat"
Keywords(ADDCONTEXT_T) = "addcontext"
Keywords(VIDEO_T) = "video"

