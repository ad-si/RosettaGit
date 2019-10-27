+++
title = "Execute Brain****/AutoHotkey"
description = ""
date = 2010-05-16T15:21:02Z
aliases = []
[extra]
id = 7296
[taxonomies]
categories = []
tags = []
+++


```autohotkey
;---------------------------------------------------------------------------
; Brainf*** Interpreter.ahk
; by wolf_II
;---------------------------------------------------------------------------
; interpreter for Brainf*** code
;---------------------------------------------------------------------------



;---------------------------------------------------------------------------
Code = ; Goodbye, World!
;---------------------------------------------------------------------------
    (LTrim
        Example taken from RosettaCode:org

        ++++++++++[>+>+++>++++>+++++++>++++++++>+++++++++>++++++++++
        >+++++++++++>++++++++++++<<<<<<<<<-]>>>>+.>>>>+..<.<++++++++
        .>>>+.<<+.<<<<++++.<++.>>>+++++++.>>>.+++.<+++++++.--------.
        <<<<<+.<+++.---.
    )



;---------------------------------------------------------------------------
AutoExecute: ; auto-execute section of the script
;---------------------------------------------------------------------------
    #SingleInstance, Force          ; only one instance allowed
    #NoEnv                          ; don't check empty variables
    ;-----------------------------------------------------------------------
    AppName := "Brainf***"
    Gosub, GuiCreate
    Gui, Show,, %AppName%

Return



;---------------------------------------------------------------------------
GuiCreate: ; create the main window
;---------------------------------------------------------------------------
    ; GUI options
    Gui, Margin,, 10
    Gui, Add, Edit, y0 h0 ; catch the focus

    ; Brainf*** Code
    Gui, Add, GroupBox, w462 h248, Brainf*** Code
    Gui, Font, s8, Courier New
    Gui, Add, Edit, xp+10 yp+20 w442 r15 vCode, %Code%
    Gui, Font ; normal

    ; buttons
    Gui, Add, Button, xm w101 h30, &Load Code
    Gui, Add, Button, x+19 wp hp, &Save Code As
    Gui, Add, Button, x+19 wp hp Default, &Run Code
    Gui, Add, Button, x+19 wp hp, E&xit

    ; Output
    Gui, Add, GroupBox, xm w462 h248, Output
    Gui, Font, s8, Courier New
    Gui, Add, Edit, xp+10 yp+20 w442 r15 ReadOnly vOutput HwndhOut
    Gui, Font ; normal

Return



;---------------------------------------------------------------------------
ButtonLoadCode: ; load code from file
;---------------------------------------------------------------------------
    Gui, +OwnDialogs
    FileSelectFile, CodeFile,,, Load Brain*** Code, *.bf
    If (ErrorLevel = 1) {
        ; user dismissed the dialog
        Gui, Show
        Return
    }
    If Not SubStr(CodeFile, -2) = ".bf"
        CodeFile .= ".bf"
    If FileExist(CodeFile) {
        FileRead, Code, %CodeFile%
        GuiControl,, Code, %Code%
    } Else
        MsgBox, 16, Error - %AppName%, File not found:`n`n"%CodeFile%"
    GuiControl,, Output ; clear all output
    Gui, Show

Return



;---------------------------------------------------------------------------
ButtonSaveCodeAs: ; save code to file
;---------------------------------------------------------------------------
    Gui, +OwnDialogs
    Gui, Submit, NoHide
    FileSelectFile, CodeFile, S16,, Brain*** Code, *.bf
    If (ErrorLevel = 1) {
        ; user dismissed the dialog
        Gui, Show
        Return
    }
    If Not SubStr(CodeFile, -2) = ".bf"
        CodeFile .= ".bf"
    FileDelete, %CodeFile%
    FileAppend, %Code%, %CodeFile%
    Gui, Show

Return



;---------------------------------------------------------------------------
GuiClose:   ; {Alt-F4} pressed, [X] clicked
;---------------------------------------------------------------------------
ButtonExit: ; Exit button clicked
;---------------------------------------------------------------------------
    ExitApp

Return



;---------------------------------------------------------------------------
ButtonRunCode: ; run the code from the edit field
;---------------------------------------------------------------------------
    Gui, Submit, NoHide         ; get code from GUI
    Gosub, set_JumpPointers     ; set jump pointers
    GuiControl, Disable, Output
    GuiControl, Disable, &Load
    GuiControl, Disable, &Save
    GuiControl, Disable, &Run
    Gosub, Interpreter          ; do your stuff, kid
    GuiControl, Enable, Output
    GuiControl, Enable, &Load
    GuiControl, Enable, &Save
    GuiControl, Enable, &Run

Return



;---------------------------------------------------------------------------
Interpreter: ; This is the Brainf*** Interpreter
;---------------------------------------------------------------------------
    ; setup BF environment
    VarSetCapacity(BF_MEM, 4096, 0)
    DataPtr := InstrPtr := 0

    Loop {

        ; read next instruction
        CMD := SubStr(Code, ++InstrPtr, 1)
        Data@Ptr := NumGet(BF_MEM, DataPtr, "Char")

        If (CMD = "+") ; increment the data at DataPointer
            NumPut(Data@Ptr + 1, BF_MEM, DataPtr, "Char")

        Else If (CMD = "-") ; decrement the data at DataPointer
            NumPut(Data@Ptr - 1, BF_MEM, DataPtr, "Char")

        Else If (CMD = ">") ; increment the DataPointer
            DataPtr++

        Else If (CMD = "<") ; decrement the DataPointer
            DataPtr--

        Else If (CMD = ".") ; print the char at DataPointer
            Output(Chr(Data@Ptr))

        Else If (CMD = ",") ; get a char from stdin (keyboard)
            NumPut(GetChar(), BF_MEM, DataPtr, "Char")

        Else If (CMD = "[") { ; conditional jump
            If (Data@Ptr = 0)
                ; continue after the corresponding "]"
                InstrPtr := Jump[%InstrPtr%]

        } Else If (CMD = "]") { ; conditional jump
            If (Data@Ptr != 0)
                ; continue after the corresponding "["
                InstrPtr := Jump[%InstrPtr%]

        } Else If (CMD = "") ; this is the end of code
            Break
    }

Return



;---------------------------------------------------------------------------
set_JumpPointers: ; setup all the jump pointers in two passes
;---------------------------------------------------------------------------
    Loop, % Len := StrLen(Code) ; first pass
        If (CMD := SubStr(Code, A_Index, 1)) = "["
            Push("Stack", A_Index)
        Else If (CMD = "]")
            Jump[%A_Index%] := Pop("Stack")

    Loop, % Len ; second pass goes backwards
        If (CMD := SubStr(Code, Index:=Len-A_Index, 1)) = "]"
            Push("Stack", Index)
        Else If (CMD = "[")
            Jump[%Index%] := Pop("Stack")

Return



;---------------------------------------------------------------------------
GetChar() { ; retrieve a single char from StdIn (keyboard)
;---------------------------------------------------------------------------
    Progress, b2 w150 zh0 fs9, Brainf*** Interpreter waits for input ...
    Input, InKey$, L1
    Progress, Off
    Return, Asc(InKey$)
}



;---------------------------------------------------------------------------
Push(Stack, x) { ; push x onto stack named "Stack"
;---------------------------------------------------------------------------
    local pos
    %Stack%_0 := pos := %Stack%_0 ? %Stack%_0 + 1 : 1
    %Stack%_%pos% := x
}



;---------------------------------------------------------------------------
Pop(Stack) { ; pop value from stack named "Stack"
;---------------------------------------------------------------------------
    local pos
    If (pos := %Stack%_0) < 1 ; already empty
        Return,, ErrorLevel := -1
    Return, %Stack%_%pos%, %Stack%_0--
}



;---------------------------------------------------------------------------
Output(Text) { ; append text to output
;---------------------------------------------------------------------------
    ; for non-interruptable output
    ;-----------------------------------------------------------------------
    global hOut
    SendMessage, 0xC2,, &Text,, ahk_id %hOut% ; EM_REPLACESEL
}



;---------- end of file ----------------------------------------------------

```

[[Image:BrainfGUI.png]]
