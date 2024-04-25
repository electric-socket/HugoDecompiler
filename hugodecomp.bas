$Let DEBUG = 0
$Console:Only
Option _Explicit
ReDim Shared As _Unsigned _Byte Fi(0), Fo(0)
Const GrammarStart = &H40
' Indexed addresses -  Value is *16
Const I_InitAddr = &H19, InitSize = 2
Const I_Main = &H1B, MainSize = 2
Const I_Parse = &H1D, ParseSize = 2
Const I_ParseError = &H1F, ErrorSize = 2
Const I_FindObject = &H21, FindSize = 2
Const I_EndGame = &H32, EndSize = 2
Const I_Speakto = &H25, SpeakSize = 2
Const I_Perform = &H27, PerformSize = 2
Const I_TextBank = &H29, TextSize = 2

' CHAR_TRANSLATION is simply a value that is added to an ASCII character
'   in order to encode the text, i.e., make it unreadable to casual
'   browsing.

Const CHAR_TRANSLATION = &H14

Dim As _Byte Version

Dim As Integer StartA, ObjectA, PropertyA, EventA, ArrayA, DictionaryA, SpecialA
Dim Shared As Integer GrammarCount, VerbCount, XVerbCount, DictEntry
Dim As Long StartAddr, ObjectAddr, PropertyAddr
Dim As Long EventAddr, ArraySpace, Dictionary, SpecialWords
Dim As Long DictLen, DictAddr, SkipAddr

Dim Shared As _Byte Token, TRAP, WordCount
Dim Shared As Long G, H, I, J, K, L, M
Dim Shared As String Keywords(0 To 255), Flag(0 To 255), text, temp, VerbType
ReDim Shared As String FullText, L1, L2, L3

Dim As Long InitAddr, Main, Parse, ParseError, FindObject
Dim As Long EndGame, Speakto, Perform, TextBank


Const FNum = 5
Dim FN$
Dim As _Unsigned _Integer64 FSize
'Input "File Name"; FN$
' FN$ = "L:\Recovered from 16 GB Computer\Documents\Programming\Hugo\Samples\tripkey\TRIPKEY.HEX"
FN$ = "L:\Programming\Hugo\Librarian\the_librarian.hex"


Open FN$ For Binary Access Read As #FNum
FSize = LOF(FNum)
ReDim Fi(FSize), Fo(FSize)
Get #FNum, , Fi()
Close #FNum

For I = 1 To FSize
    K = Fi(I)
    If K < 32 + CHAR_TRANSLATION Then K = 32 Else K = K - CHAR_TRANSLATION
    Fo(I) = K
    FullText = FullText + Chr$(K)
Next
'
Open "L:\tripkey.bin" For Binary Access Write As #FNum
Put #FNum, , Fo()
Close #FNum

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

Flag(VERB_T) = " -> routine # "
Flag(XVERB_T) = " -> routine # "
Flag(DICTENTRY_T) = " -> entry # "

' HEX file info

Const L_Version = 0, VersionSize = 1
Const L_ID = 1, IDSize = 2
Const L_Serial = 3, SerialSize = 8
Const L_StartAddr = &H0B, StartSize = 2
Const L_ObjectAddr = &H0D, ObjectSize = 2
Const L_PropertyAddr = &H0F, PropertySize = 2
Const L_EventAddr = &H11, EventSize = 2
Const L_ArraySpace = &H13, ArraySize = 2
Const L_Dictionary = &H15, DictionarySize = 2
Const L_SpecialWords = &H17, SpecialSize = 2
Const DQuote = Chr$(34)
Dim As String * SerialSize Serial
Dim As String IDHex, SerialHex, ID

Const NULL_T = 0, OPEN_BRACKET_T = NULL_T + 1

' Start reading
Version = Fi(L_Version)

For I = 0 To 7
    If I < 2 Then
        If Fi(L_ID + I) < 32 Then
            ID = ID + " "
        Else
            ID = ID + Chr$(Fi(L_ID + I))
        End If
        If Fi(L_ID + I) < 16 Then IDHex = IDHex + "0"
        IDHex = IDHex + Hex$(Fi(L_ID + I))
    End If

    If Fi(L_Serial + I) < 32 Then
        Serial = Serial + " "
    Else
        Serial = Serial + Chr$(Fi(L_Serial + I))
    End If
    If Fi(L_Serial + I) < 16 Then SerialHex = SerialHex + "0"
    SerialHex = SerialHex + Hex$(Fi(L_Serial + I))
Next

StartA = Fi(L_StartAddr + 1) * 256 + Fi(L_StartAddr): StartAddr = StartA * 16
ObjectA = Fi(L_ObjectAddr + 1) * 256 + Fi(L_ObjectAddr): ObjectAddr = ObjectA * 16
PropertyA = Fi(L_PropertyAddr + 1) * 256 + Fi(L_PropertyAddr): PropertyAddr = PropertyA * 16
EventA = Fi(L_EventAddr + 1) * 256 + Fi(L_EventAddr): EventAddr = EventA * 16
ArrayA = Fi(L_ArraySpace + 1) * 256 + Fi(L_ArraySpace): ArraySpace = ArrayA * 16
DictionaryA = Fi(L_Dictionary + 1) * 256 + Fi(L_Dictionary): Dictionary = DictionaryA * 16
SpecialA = Fi(L_SpecialWords + 1) * 256 + Fi(L_SpecialWords): SpecialWords = SpecialA * 16

InitAddr = (Fi(I_InitAddr + 1) * 256 + Fi(I_InitAddr)) * 16
Main = (Fi(I_Main + 1) * 256 + Fi(I_Main)) * 16
Parse = (Fi(I_Parse + 1) * 256 + Fi(I_Parse)) * 16
ParseError = (Fi(I_ParseError + 1) * 256 + Fi(I_ParseError)) * 16
FindObject = (Fi(I_FindObject + 1) * 256 + Fi(I_FindObject)) * 16
EndGame = (Fi(I_EndGame + 1) * 256 + Fi(I_EndGame)) * 16
Speakto = (Fi(I_Speakto + 1) * 256 + Fi(I_Speakto)) * 16
Perform = (Fi(I_Perform + 1) * 256 + Fi(I_Perform)) * 16
TextBank = (Fi(I_TextBank + 1) * 256 + Fi(I_TextBank)) * 16

' Display Values
Print "Size in bytes  "; FSize
Print "Version      = "; Version
Print "                Segment Value / Hex (original/hex)"
Print "ID           = '"; ID; "/"; IDHex
Print "Serial       = '"; Serial; "/"; SerialHex; "'"
Print "StartAddr    = "; StartAddr; "/"; Hex$(StartAddr); " ("; StartA; "/"; Hex$(StartA); " )"
Print "ObjectAddr   = "; ObjectAddr; "/"; Hex$(ObjectAddr); " ("; ObjectA; "/"; Hex$(ObjectA); " )"
Print "PropertyAddr = "; PropertyAddr; "/"; Hex$(PropertyAddr); " ("; PropertyA; "/"; Hex$(PropertyA); " )"
Print "EventAddr    = "; EventAddr; "/"; Hex$(EventAddr); " ("; EventA; "/"; Hex$(EventA); " )"
Print "ArraySpace   = "; ArraySpace; "/"; Hex$(ArraySpace); " ("; ArrayA; "/"; Hex$(ArrayA); ")"
Print "Dictionary   = "; Dictionary; "/"; Hex$(Dictionary); " ("; DictionaryA; "/"; Hex$(DictionaryA); ")"
Print "Special Words= "; SpecialWords; "/"; Hex$(SpecialWords); " ("; SpecialA; "/"; Hex$(SpecialA); ")"

Print "InitAddr   = "; InitAddr; "/"; Hex$(InitAddr)
Print "Main       = "; Main; "/"; Hex$(Main)
Print "Parse      = "; Parse; "/"; Hex$(Parse)
Print "ParseError = "; ParseError; "/"; Hex$(ParseError)
Print "FindObject = "; FindObject; "/"; Hex$(FindObject)
Print "EndGame    = "; EndGame; "/"; Hex$(EndGame)
Print "Speakto    = "; Speakto; "/"; Hex$(Speakto)
Print "Perform    = "; Perform; "/"; Hex$(Perform)
Print "TextBank   = "; TextBank; "/"; Hex$(TextBank)

Input "more"; temp$

K = InitAddr
Print "initaddr "; K; " / "; Hex$(K); ":"
Print
Print Space$(5);
Do
    G = Fi(K)
    K = K + 1

    Print
    ' First pass: identify and dispatch
    Select Case G
        Case NULL_T: _Continue ' Nulls used for alignment

        Case OPEN_BRACKET_T To LESS_T, COMMA_T, AND_T, OR_T, IS_T, NOT_T, TRUE_T, FALSE_T,  _
             HELD_T To ANYTHING_T, NOTHELD_T, MULTINOTHELD_T, PARSE_T, IN_T
            Print Keywords(G); " ";
            _Continue

        Case BREAK_T, EOL_T, PAUSE_T ' symbol ends line
            Print Keywords(G)
            Print Space$(5);
            _Continue

            ' Followed by skip address
        Case IF_T, ELSE_T, ELSEIF_T, DO_T, ROUTINE_T, JUMP_T
            Print Keywords(G); " ";
            GoSub Skip

            ' followed by number
        Case OBJECTNUM_T, VALUE_T
            Print Keywords(G); " ";
            GoSub Number




    End Select

    Input "more"; temp$

Loop














Print: Input "Dict dump follows - press enter: ", temp$
Dim ddChar As String, ddHex As String
K = Dictionary
'Dictionay len is always 2 bytes
DictLen = Fi(K + 1) * 256 + Fi(K)
Print "Dictionary Entries "; DictLen
If Fi(K + 2) <> 0 Then Print "Dictionay table may not be accurate."
K = K + 3
Dim Adr(0 To 6) As Integer, A As Integer

For J = 0 To 12
    G = Fi(K + J)
    If G < 16 Then Print "0";
    Print Hex$(G);
Next

Print
Print "---"
A = 0
For I = 0 To 10 Step 2
    Adr(A) = Fi(K + I) * 256 + Fi(K + I + 1)
    Print "#"; A; Adr(A); "/"; Hex$(Adr(A))
    Print Using "At ##### "; Adr(A);
    K = Adr(A)
    L1 = Space$(9) + "raw" + Space$(5)
    L2 = Space$(9) + "decrypt "
    GoSub HexDump
    K = L_Dictionary + Adr(A)
    Print "At Dict ("; L_Dictionary; "+"; Adr(A); ")="; K
    L1 = "R ": L2 = "D "
    GoSub HexDump
    Input "more", temp$




    A = A + 1
Next



Input "more", temp$







' Ols xgecks
L1 = "raw" + Space$(5)
L2 = "decrypt "
Print "hex"; Space$(5);
For J = 1 To 35
    G = Fi(K + J)
    If G < 32 Then
        L1 = L1 + "  "
    Else
        L1 = L1 + " " + Chr$(G)
    End If
    I = G - 14
    If I < 32 Then
        L2 = L2 + "  "
    Else L2 = L2 + " " + Chr$(I)
    End If
    If G < 16 Then Print "0";
    Print Hex$(G);
Next
Print
Print L1
Print L2
Input "More: ", temp$
H = K + 1
L1 = ""
L2 = ""
For J = 1 To 5 ' DictLen
    '   DictAddr = Fi(K + J + 1) * 256 + Fi(K + J)
    DictAddr = Fi(H + 1) * 256 + Fi(H)
    H = H + 2
    Print DictAddr; "/"; Hex$(DictAddr);: Locate , 15
    K = H
    H = DictAddr
    For J = 0 To 34
        G = Fi(H + J)
        If G < 32 Then
            L1 = L1 + "  "
        Else
            L1 = L1 + " " + Chr$(G)
        End If
        I = G - 14
        If I < 32 Then
            L2 = L2 + "  "
        Else L2 = L2 + " " + Chr$(I)
        End If
        If G < 16 Then Print "0";
        Print Hex$(G);
    Next
    Print
    Print " raw";: Locate , 15: Print L1
    Print " decrypted";: Locate , 15: Print L2
    L1 = "": L2 = ""
    Print "Indexed";: Locate , 15

    H = DictAddr + Dictionary
    For J = 0 To 34
        G = Fi(H + J)
        If G < 32 Then
            L1 = L1 + "  "
        Else
            L1 = L1 + " " + Chr$(G)
        End If
        I = G - 14
        If I < 32 Then
            L2 = L2 + "  "
        Else L2 = L2 + " " + Chr$(I)
        End If
        If G < 16 Then Print "0";
        Print Hex$(G);
    Next
    Print " raw";: Locate , 15: Print L1
    Print " decrypted";: Locate , 15: Print L2
    L1 = "R ": L2 = "D "
    Print "  ";



    Input "More", temp$



    Print "       "; L1
    Print "       "; L2

Next
Input "More", temp$



Print
For J = 0 To 19
    G = Fi(DictAddr + J)
    If G < 32 Then
        L1 = L1 + "  "
    Else
        L1 = L1 + " " + Chr$(G)
        If G < CHAR_TRANSLATION Then
            L3 = L3 + "__"
        Else
            L3 = L3 + Chr$(G - CHAR_TRANSLATION)
        End If
    End If
    If G < 16 Then L2 = L2 + "0"
    L2 = L2 + Hex$(G)
Next
Print L1
Print L2
Print "decryupted"
Print L3

Input "More: "; temp$




'For I = 1 To 10
ddChar = ""
ddHex = ""
For J = 1 To 16
    If Fi(K) < 16 Then ddHex = ddHex + "0"
    ddHex = ddHex + Hex$(Fi(K))
    If Fi(K) < 32 Then
        ddChar = ddChar + "."
    Else
        ddChar = ddChar + Chr$(Fi(K))
    End If
    K = K + 1
Next
Print ddChar; "   "; ddHex
'Next

Print: Input "Grammar follows - press enter: ", temp$
' Grammar follows header

G = GrammarStart
Token = Fi(G)

' consists of action verbs $2C or non-action Xverbs $2D
If Token <> ENDVERB Then

    If Token = VERB_T Or Token = XVERB_T Then
        Do
            ProcessVerbs
            Token = Fi(G)
        Loop Until Token = ENDVERB
    Else
        ' Bad form+
        Print "Invalid value"; Fi(G); " at byte"; G
        End
    End If
Else
    Print "No verbs or Xverbs."
End If



'G = G + 1
'GrammarCount = GrammarCount + 1

'_Delay 6 ' **

'Print

'Print: Print "More follows - press enter: ", temp$
' Grammar follows header

'Token = Fi(G)
'While Token <> ENDVERB
'    If Token = 0 Then G = G + 1 Else ProcessToken
'    Token = Fi(G)
'Wend
' The endverb signals end of this entry; a new verb/xverb may follow
'Else

'End If
'G = G + 1    +

'    Else

'End If





End

HexDump:

For J = 1 To 20
    G = Fi(K + J)
    If G < 32 Then
        L1 = L1 + "  "
    Else
        L1 = L1 + " " + Chr$(G)
    End If
    I = G - 14
    If I < 32 Then
        L2 = L2 + "  "
    Else L2 = L2 + " " + Chr$(I)
    End If
    If G < 16 Then Print "0";
    Print Hex$(G);
Next
Print
Print L1
Print L2
L1 = ""
L2 = ""


Return
Skip: ' Generate skip addr
SkipAddr = Fi(K) * 256 + Fi(K + 1): K = K + 2
Print "Skip by "; SkipAddr;
Return

Number:
SkipAddr = Fi(K) * 256 + Fi(K + 1): K = K + 2
Print "number"; SkipAddr;
Return


'xyzzy


'  Handle token, advance to next
Sub ProcessToken
    Dim As Long Routine, Entry

    Print "**("; G; ") ";
    ' Weve read a token, handle it
    Print Keywords(Token); " ";
    If Token <= LESS_T Or Token = COMMA_T Then
        G = G + 1
        Exit Sub
    End If
    Select Case Token
        Case Is < LESS_T, COMMA_T
            ' Don't have to do anything after print token
            G = G + 1
            Print
            Exit Sub
        Case VERB_T, XVERB_T, ATTR_T, DICTENTRY_T, OBJECT_T, ROUTINE_T, VALUE_T
            G = G + 1
            ' Pull address / entry / value
            Routine = Fi(G) * 256 + Fi(G + 1)
            G = G + 2
            Print Flag(Token); Routine
            Exit Sub

        Case VAR_T
            ' Print  1 byte value following token
            G = G + 1
            Print Fi(G)
            G = G + 1
            Exit Sub
        Case STRINGDATA_T
            'String constant
            G = G + 1
            'len of string
            Entry = Fi(G) * 256 + Fi(G + 1)
            G = G + 2
            text$ = ""
            For I = 0 To Entry - 1
                text$ = text$ + Chr$(Fi(G + I))
            Next
            G = G + Entry
            Print DQuote; text$; DQuote

    End Select

End Sub

Sub ProcessVerbs
    Dim PrintCount As Integer
    Select Case Token
        Case VERB_T
            VerbCount = VerbCount + 1
            VerbType = "V"
        Case XVERB_T
            XVerbCount = XVerbCount + 1
            VerbType = "XV"
    End Select
    WordCount = Fi(G)
    G = G + 1
    Print WordCount; " "; VerbType; "erb";
    If WordCount <> 1 Then Print "s";
    Print "; Dictionary entries:";
    For I = 1 To WordCount
        DictEntry = Fi(G) * 256 + Fi(G + 1)
        G = G + 2
        If DictEntry < 0 Then
            Print "(indirect) ";
            PrintCount = PrintCount + 1
            Token = Fi(G)
            Print " ["; Keywords(Token); "] ";
            G = G + 1
            DictEntry = Fi(G) * 256 + Fi(G + 1)
            G = G + 2
        End If
        Print DictEntry;
        PrintCount = PrintCount + 1
        If PrintCount > 5 Then
            Print
            PrintCount = 0
        End If

    Next
    Print "*** stop 4"
    End
End Sub




