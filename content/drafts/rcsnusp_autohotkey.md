+++
title = "RCSNUSP/AutoHotkey"
description = ""
date = 2014-04-15T20:29:40Z
aliases = []
[extra]
id = 7328
[taxonomies]
categories = []
tags = []
+++


```autohotkey
;---------------------------------------------------------------------------
; SNUSP Interpreter.ahk
; by wolf_II
;---------------------------------------------------------------------------
; interpreter for SNUSP code
;---------------------------------------------------------------------------



;---------------------------------------------------------------------------
Code = ; Goodbye, World!
;---------------------------------------------------------------------------
    (
Example taken from RosettaCode.org

$@\G.@\o.o.@\d.--b.@\y.@\e.>@\comma.@\.<-@\W.+@\o.+++r.------l.@\d.>+.! #
  |   |     \@------|#  |    \@@+@@++|+++#-    \\               -
  |   \@@@@=+++++#  |   \===--------!\===!\-----|-------#-------/
  \@@+@@@+++++#     \!#+++++++++++++++++++++++#!/
    )



;---------------------------------------------------------------------------
AutoExecute: ; auto-execute section of the script
;---------------------------------------------------------------------------
    #SingleInstance, Force          ; only one instance allowed
    #NoEnv                          ; don't check empty variables
    ;-----------------------------------------------------------------------
    AppName := "SNUSP"
    Gosub, GuiCreate
    Gui, Show,, %AppName%

Return



;---------------------------------------------------------------------------
GuiCreate: ; create the main window
;---------------------------------------------------------------------------
    ; GUI options
    Gui, Margin,, 10
    Gui, Add, Edit, y0 h0 ; catch the focus
    Gui, Add, Checkbox, vWatch, Watch

    ; SNUSP Code
    Gui, Add, GroupBox, w554 h265, SNUSP Code
    Gui, Font, s8, Courier New
    Gui, Add, Edit, xp+10 yp+20 w534 r15 -Wrap +HScroll vCode HwndhCode, %Code%
    Gui, Font ; normal

    ; buttons
    Gui, Add, Button, xm w101 h30, &Load Code
    Gui, Add, Button, x+19 wp hp, &Save Code As
    Gui, Add, Button, x+19 wp hp Default, &Run Code
    Gui, Add, Button, x+19 wp hp, E&xit

    ; Output
    Gui, Add, GroupBox, xm w554 h248, Output
    Gui, Font, s8, Courier New
    Gui, Add, Edit, xp+10 yp+20 w534 r15 ReadOnly vOutput HwndhOut
    Gui, Font ; normal

Return



;---------------------------------------------------------------------------
ButtonLoadCode: ; load code from file
;---------------------------------------------------------------------------
    Gui, +OwnDialogs
    FileSelectFile, CodeFile,,, Load SNUSP Code, *.snusp
    If (ErrorLevel = 1) {
        ; user dismissed the dialog
        Gui, Show
        Return
    }
    If Not SubStr(CodeFile, -5) = ".snusp"
        CodeFile .= ".snusp"
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
    FileSelectFile, CodeFile, S16,, SNUSP Code, *.snusp
    If (ErrorLevel = 1) {
        ; user dismissed the dialog
        Gui, Show
        Return
    }
    If Not SubStr(CodeFile, -5) = ".snusp"
        CodeFile .= ".snusp"
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
    Gui, Submit, NoHide ; get code from GUI
    ;~ GuiControl, Disable, Code
    GuiControl, Disable, Output
    GuiControl, Disable, &Load
    GuiControl, Disable, &Save
    GuiControl, Disable, &Run
    Gosub, PadCode
    ;~ GuiControl, Enable, Code
    GuiControl, Enable, Output
    GuiControl, Enable, &Load
    GuiControl, Enable, &Save
    GuiControl, Enable, &Run

Return



;---------------------------------------------------------------------------
PadCode: ; pad code with spaces
;---------------------------------------------------------------------------
    mL := 0 ; max length
    Loop, Parse, Code, `n
        If (Len := StrLen(A_LoopField)) > mL
            mL := Len
    PaddedCode := ""
    Loop, Parse, Code, `n
        PaddedCode .= A_LoopField Repeat(" ", mL - StrLen(A_LoopField)) "`r`n"
    GuiControl,, Code, % Code := PaddedCode
    ml += 2

    ; find code entry point
    InstrPtr := (pos := InStr(Code, "$")) ? pos : 1

;---------------------------------------------------------------------------
Interpreter: ; this is the SNUSP Interpreter
;---------------------------------------------------------------------------
    ; setup SNUSP environment
    VarSetCapacity(SNUSP_MEM, 4096, 0)
    DataPtr := InstrDir := 0
    ControlFocus,, ahk_id %hCode%
    Loop {

        ; read next instruction
        getNext(InstrPtr)
        CMD := SubStr(Code, InstrPtr, 1)
        Data@Ptr := NumGet(SNUSP_MEM, DataPtr, "Char")
        If Watch {
            Highlight(InstrPtr)
            Sleep, 25
            ;~ ListVars
            ;~ Pause
        }

        If (CMD = "+") ; increment the data at DataPointer
            NumPut(Data@Ptr + 1, SNUSP_MEM, DataPtr, "Char")

        Else If (CMD = "-") ; decrement the data at DataPointer
            NumPut(Data@Ptr - 1, SNUSP_MEM, DataPtr, "Char")

        Else If (CMD = ">") ; increment the DataPointer
            DataPtr++

        Else If (CMD = "<") ; decrement the DataPointer
            DataPtr--

        Else If (CMD = ".") ; print the char at DataPointer
            Output(Chr(Data@Ptr))

        Else If (CMD = ",") ; get a char from stdin (keyboard)
            NumPut(GetChar(), SNUSP_MEM, DataPtr, "Char")

        Else If (CMD = "\") ; LURD
            LURD(InstrDir)

        Else If (CMD = "/") ; RULD
            RULD(InstrDir)

        Else If (CMD = "!") ; SKIP
            getNext(InstrPtr)

        Else If (CMD = "?") { ; SKIPZ
            If (Data@Ptr = 0)
                getNext(InstrPtr)

        } Else If (CMD = "@") { ; ENTER
            Push("Stack", InstrPtr)
            Push("Stack", InstrDir)

        } Else If (CMD = "#") { ; LEAVE
            InstrDir := Pop("Stack")
            InstrPtr := Pop("Stack")
            If (ErrorLevel = -1)
                Break
            Else
                getNext(InstrPtr)

        } Else If (CMD = "") ; this is the end of code
            Break
    }

Return



;---------------------------------------------------------------------------
getNext(ByRef InstrPtr) { ; get next instruction pointer
;---------------------------------------------------------------------------
    ;    instruction directions
    ;         Up = 3
    ;     Left = 2   0 = Right
    ;              1 = Down
    ;-----------------------------------------------------------------------
    global InstrDir, mL
    InstrPtr += (InstrDir > 1 ? -1 : 1) * (InstrDir & 1 ? mL : 1)
}



;---------------------------------------------------------------------------
LURD(ByRef InstrDir) { ; reflect the instruction direction "\"
;---------------------------------------------------------------------------
    ;~ case 0 -> 1
    ;~ case 1 -> 0
    ;~ case 2 -> 3
    ;~ case 3 -> 2
    ;-----------------------------------------------------------------------
    InstrDir += (InstrDir & 1 ? -1 : 1)
}



;---------------------------------------------------------------------------
RULD(ByRef InstrDir) { ; reflect the instruction direction "/"
;---------------------------------------------------------------------------
    ;~ case 0 -> 3
    ;~ case 1 -> 2
    ;~ case 2 -> 1
    ;~ case 3 -> 0
    ;-----------------------------------------------------------------------
    InstrDir := 3 - InstrDir
}



;---------------------------------------------------------------------------
Repeat(String,Count) {
;---------------------------------------------------------------------------
    Loop, %Count%
        Result .= String
    Return, Result
}



;---------------------------------------------------------------------------
GetChar() { ; retrieve a single char from StdIn (keyboard)
;---------------------------------------------------------------------------
    Progress, b2 w150 zh0 fs9, SNUSP Interpreter waits for input ...
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
Highlight(InstrPtr) { ; select current command in code
;---------------------------------------------------------------------------
    global hCode
    SendMessage, 0xB1, InstrPtr-1, InstrPtr,, ahk_id %hCode% ; EM_SETSEL
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

