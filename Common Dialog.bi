
' Dialog flag constants (use + or OR to use more than 1 flag value)
Const OFN_ALLOWMULTISELECT = &H200& '  Allows the user to select more than one file, not recommended!
Const OFN_CREATEPROMPT = &H2000& '     Prompts if a file not found should be created(GetOpenFileName only).
Const OFN_EXTENSIONDIFFERENT = &H400& 'Allows user to specify file extension other than default extension.
Const OFN_FILEMUSTEXIST = &H1000& '    Chechs File name exists(GetOpenFileName only).
Const OFN_HIDEREADONLY = &H4& '        Hides read-only checkbox(GetOpenFileName only)
Const OFN_NOCHANGEDIR = &H8& '         Restores the current directory to original value if user changed
Const OFN_NODEREFERENCELINKS = &H100000& 'Returns path and file name of selected shortcut(.LNK) file instead of file referenced.
Const OFN_NONETWORKBUTTON = &H20000& ' Hides and disables the Network button.
Const OFN_NOREADONLYRETURN = &H8000& ' Prevents selection of read-only files, or files in read-only subdirectory.
Const OFN_NOVALIDATE = &H100& '        Allows invalid file name characters.
Const OFN_OVERWRITEPROMPT = &H2& '     Prompts if file already exists(GetSaveFileName only)
Const OFN_PATHMUSTEXIST = &H800& '     Checks Path name exists (set with OFN_FILEMUSTEXIST).
Const OFN_READONLY = &H1& '            Checks read-only checkbox. Returns if checkbox is checked
Const OFN_SHAREAWARE = &H4000& '       Ignores sharing violations in networking
Const OFN_SHOWHELP = &H10& '           Shows the help button (useless!)
'--------------------------------------------------------------------------------------------

DefInt A-Z 'not recommended to use this statement in a final application!

Type FILEDIALOGTYPE
    $If 32BIT Then
    lStructSize AS LONG '        For the DLL call
    hwndOwner AS LONG '          Dialog will hide behind window when not set correctly
    hInstance AS LONG '          Handle to a module that contains a dialog box template.
    lpstrFilter AS _OFFSET '     Pointer of the string of file filters
    lpstrCustFilter AS _OFFSET
    nMaxCustFilter AS LONG
    nFilterIndex AS LONG '       One based starting filter index to use when dialog is called
    lpstrFile AS _OFFSET '       String full of 0's for the selected file name
    nMaxFile AS LONG '           Maximum length of the string stuffed with 0's minus 1
    lpstrFileTitle AS _OFFSET '  Same as lpstrFile
    nMaxFileTitle AS LONG '      Same as nMaxFile
    lpstrInitialDir AS _OFFSET ' Starting directory
    lpstrTitle AS _OFFSET '      Dialog title
    flags AS LONG '              Dialog flags
    nFileOffset AS INTEGER '     Zero-based offset from path beginning to file name string pointed to by lpstrFile
    nFileExtension AS INTEGER '  Zero-based offset from path beginning to file extension string pointed to by lpstrFile.
    lpstrDefExt AS _OFFSET '     Default/selected file extension
    lCustData AS LONG
    lpfnHook AS LONG
    lpTemplateName AS _OFFSET
    $Else
        lStructSize As _Offset '      For the DLL call
        hwndOwner As _Offset '        Dialog will hide behind window when not set correctly
        hInstance As _Offset '        Handle to a module that contains a dialog box template.
        lpstrFilter As _Offset '      Pointer of the string of file filters
        lpstrCustFilter As Long
        nMaxCustFilter As Long
        nFilterIndex As _Integer64 '  One based starting filter index to use when dialog is called
        lpstrFile As _Offset '        String full of 0's for the selected file name
        nMaxFile As _Offset '         Maximum length of the string stuffed with 0's minus 1
        lpstrFileTitle As _Offset '   Same as lpstrFile
        nMaxFileTitle As _Offset '    Same as nMaxFile
        lpstrInitialDir As _Offset '  Starting directory
        lpstrTitle As _Offset '       Dialog title
        flags As _Integer64 '         Dialog flags
        nFileOffset As _Integer64 '   Zero-based offset from path beginning to file name string pointed to by lpstrFile
        nFileExtension As _Integer64 'Zero-based offset from path beginning to file extension string pointed to by lpstrFile.
        lpstrDefExt As _Offset '      Default/selected file extension
        lCustData As _Integer64
        lpfnHook As _Integer64
        lpTemplateName As _Offset
    $End If
End Type

Declare Dynamic Library "comdlg32" ' Library declarations using _OFFSET types
    Function GetOpenFileNameA& (DIALOGPARAMS As FILEDIALOGTYPE) ' The Open file dialog
    '  Function GetOpenFileNameW& (DIALOGPARAMS As FILEDIALOGTYPE) ' The Open file dialog
    Function GetSaveFileNameA& (DIALOGPARAMS As FILEDIALOGTYPE) ' The Save file dialog
End Declare

Declare Library
    Function FindWindow& (ByVal ClassName As _Offset, WindowName$) ' To get hWnd handle
    Function MultiByteToWideChar~&& (CodePage As _Unsigned _Integer64, _
    dwFlags as  _Unsigned  _Integer64, _
    In_NLS_string as _offset, _
    cbMultiByte as _Integer64, _
    lpWideCharStr  as _offset, _
    cchWideChar as _Integer64) ' unsigned Int func    Kernel32
End Declare

hWnd& = _WindowHandle 'FindWindow(0, "Open and Save Dialog demo" + CHR$(0)) 'get window handle using _TITLE string

' Invoke Open Read File dialog
Filter$ = "Text files (*.txt)|*.TXT|List files (*.lst)|*.LST|Data files (*.dat)|*.DAT|All files (*.*)|*.*"
Flags& = OFN_FILEMUSTEXIST + OFN_NOCHANGEDIR + OFN_READONLY '    add flag constants here
OFile$ = GetOpenFileName$("YEAH! Common Dialogs in QB64!!!", ".\", Filter$, 1, Flags&, hWnd&)

Out1$ = ""
Out2$ = ""
K% = 0
If OFile$ = "" Then ' Display Open dialog results
    Print "Shame on you! You didn't pick any file..."
Else
    Print "You picked this file: "
    For i% = 1 To Len(OFile$)
        K% = K% + 1
        Out1$ = Out1$ + " " + Mid$(OFile$, i%, 1)
        Out2$ = Out2$ + Right$("0" + Hex$(Asc(Mid$(OFile$, i%, 1))), 2)
        '        If K% = 35 Then Exit For
    Next
    Print OFile$
    Print
    Print Out1$
    Print Out2$
    If (Flags& And OFN_READONLY) Then Print "Read-only checkbox checked." 'read-only value in return
End If

_Delay 5 ' Do the Save File dialog call!
Filter$ = "Basic files (*.bas)|*.BAS|All files (*.*)|*.*"
Flags& = OFN_OVERWRITEPROMPT + OFN_NOCHANGEDIR '   add flag constants here
SFile$ = GetSaveFileName$("Save will not create a file!!!", ".\", Filter$, 1, Flags&, hWnd&)

If SFile$ = "" Then ' Display Save dialog results
    Print "You didn't save the file..."
Else
    Print "You saved this file: "
    Print SFile$
End If
End

$Let COMMONDIALOG = 0
Function GetOpenFileName$ (Title$, InitialDir$, Filter$, FilterIndex, Flags&, hWnd&)
    '  Title$      - The dialog title.
    '  InitialDir$ - If this left blank, it will use the directory where the last opened file is
    '  located. Specify ".\" if you want to always use the current directory.
    '  Filter$     - File filters separated by pipes (|) in the same format as using VB6 common dialogs.
    '  FilterIndex - The initial file filter to use. Will be altered by user during the call.
    '  Flags&      - Dialog flags. Will be altered by the user during the call.
    '  hWnd&       - Your program's window handle that should be aquired by the FindWindow function.
    '
    ' Returns: Blank when cancel is clicked otherwise, the file name selected by the user.
    ' FilterIndex and Flags& will be changed depending on the user's selections.

    Dim OpenCall As FILEDIALOGTYPE ' Needed for dialog call

    fFilter$ = Filter$
    For R = 1 To Len(fFilter$) ' Replace the pipes with character zero
        If Mid$(fFilter$, R, 1) = "|" Then Mid$(fFilter$, R, 1) = Chr$(0)
    Next R
    fFilter$ = fFilter$ + Chr$(0)

    lpstrFile$ = String$(2048, 0) ' For the returned file name
    lpstrDefExt$ = String$(10, 0) ' Extension will not be added when this is not specified
    OpenCall.lStructSize = Len(OpenCall)
    OpenCall.hwndOwner = hWnd&
    OpenCall.lpstrFilter = _Offset(fFilter$)
    OpenCall.nFilterIndex = FilterIndex
    OpenCall.lpstrFile = _Offset(lpstrFile$)
    OpenCall.nMaxFile = Len(lpstrFile$) - 1
    OpenCall.lpstrFileTitle = OpenCall.lpstrFile
    OpenCall.nMaxFileTitle = OpenCall.nMaxFile
    OpenCall.lpstrInitialDir = _Offset(InitialDir$)
    OpenCall.lpstrTitle = _Offset(Title$)
    OpenCall.lpstrDefExt = _Offset(lpstrDefExt$)
    OpenCall.flags = Flags&

    Result = GetOpenFileNameA&(OpenCall) '            Do Open File dialog call!

    If Result Then ' Trim the remaining zeros
        GetOpenFileName$ = Left$(lpstrFile$, InStr(lpstrFile$, Chr$(0)) - 1)
        Flags& = OpenCall.flags
        FilterIndex = OpenCall.nFilterIndex
    End If

End Function

Function GetSaveFileName$ (Title$, InitialDir$, Filter$, FilterIndex, Flags&, hWnd&)
    '  Title$      - The dialog title.
    '  InitialDir$ - If this left blank, it will use the directory where the last opened file is
    '     located. Specify ".\" if you want to always use the current directory.
    '  Filter$     - File filters separated by pipes (|) in the same format as VB6 common dialogs.
    '  FilterIndex - The initial file filter to use. Will be altered by user during the call.
    '  Flags&      - Dialog flags. Will be altered by the user during the call.
    '  hWnd&       - Your program's window handle that should be aquired by the FindWindow function.

    ' Returns: Blank when cancel is clicked otherwise, the file name entered by the user.
    ' FilterIndex and Flags& will be changed depending on the user's selections.

    Dim SaveCall As FILEDIALOGTYPE ' Needed for dialog call

    fFilter$ = Filter$
    For R = 1 To Len(fFilter$) ' Replace the pipes with zeros
        If Mid$(fFilter$, R, 1) = "|" Then Mid$(fFilter$, R, 1) = Chr$(0)
    Next R
    fFilter$ = fFilter$ + Chr$(0)

    lpstrFile$ = String$(2048, 0) ' For the returned file name
    lpstrDefExt$ = String$(10, 0) ' Extension will not be added when this is not specified
    SaveCall.lStructSize = Len(SaveCall)
    SaveCall.hwndOwner = hWnd&
    SaveCall.lpstrFilter = _Offset(fFilter$)
    SaveCall.nFilterIndex = FilterIndex
    SaveCall.lpstrFile = _Offset(lpstrFile$)
    SaveCall.nMaxFile = Len(lpstrFile$) - 1
    SaveCall.lpstrFileTitle = SaveCall.lpstrFile
    SaveCall.nMaxFileTitle = SaveCall.nMaxFile
    SaveCall.lpstrInitialDir = _Offset(InitialDir$)
    SaveCall.lpstrTitle = _Offset(Title$)
    SaveCall.lpstrDefExt = _Offset(lpstrDefExt$)
    SaveCall.flags = Flags&

    Result& = GetSaveFileNameA&(SaveCall) ' Do dialog call!

    If Result& Then ' Trim the remaining zeros
        GetSaveFileName$ = Left$(lpstrFile$, InStr(lpstrFile$, Chr$(0)) - 1)
        Flags& = SaveCall.flags
        FilterIndex = SaveCall.nFilterIndex
    End If
End Function


