+++
title = "Execute Brainfuck/PureBasic"
description = ""
date = 2012-08-29T18:11:18Z
aliases = []
[extra]
id = 7477
[taxonomies]
categories = []
tags = []
+++

{{implementation|Brainfuck}}{{collection|RCBF}}
{{trans|BASIC}} - uses an integer cell size and memory size is allowed to grow as needed.

```PureBasic
Procedure displayEndingMsg()
  Print(#CRLF$ + #CRLF$ + "Press ENTER to exit")
  Input()
  CloseConsole()
  End
EndProcedure

Procedure displayErrorThenEnd(msg.s)
  PrintN(msg)
  displayEndingMsg()
EndProcedure

Macro bracketSearch(drx = 1) ;drx = -1 to search backwards
  bktCnt = drx ;start count with the current bracket
  ;count nested loops till matching one is found
  Repeat
    i + drx ;move the code pointer
    If Mid(code$, i, 1) = "]"
      bktCnt - 1
    ElseIf Mid(code$, i, 1) = "["
      bktCnt + 1
    EndIf
  Until bktCnt = 0
EndMacro

If Not OpenConsole()
  MessageRequester("Error", "Unable to open console.")
  End
EndIf

Define memsize = 1000          ;this may grow as needed
Define instChars$ = "+-<>.,[]" ;valid characters
Define ptr = 0                 ;memory pointer

Print("Filename (blank to use std in)...? ")
filename$ = Input()
If filename$ = ""
  Repeat
    line$ = Input()
    source$ = source$ + line$
  Until line$ = ""
Else
  OpenFile(1, filename$)
  Repeat
    line$ = ReadString(1)
    source$ = source$ + line$
  Until Eof(1)
  CloseFile(1)
EndIf

;remove non-code and validate number of brackets
bktCnt = 0
For i = 1 To Len(source$)
  char$ = Mid(source$, i, 1)
  ;validate instruction character
  If FindString(instChars$, char$, 1)
    code$ + char$
    ;count brackets
    Select char$
      Case "["
        bktCnt + 1
      Case "]"
        bktCnt - 1
    EndSelect
  EndIf
Next

If bktCnt  ;mismatched brackets
  displayErrorThenEnd("Uneven brackets")
EndIf

Dim memory(memsize) ;use integer cell size
Define inLine$ = "" ;input buffer
For i = 1 To Len(code$) ;loop through the code
  Select Mid(code$, i, 1) ;examine the current instruction
    Case "+"
      memory(ptr) + 1
    Case "-"
      memory(ptr) - 1
    Case "."
      Print(Chr(memory(ptr)))
    Case ","
      If inLine$ = "": inLine$ = Input(): EndIf ;buffer input
      memory(ptr) = Asc(Left(inLine$, 1))       ;store first char off the buffer
      inLine$ = Mid(inLine$, 2)                 ;delete first char from the buffer
    Case ">"
      ptr + 1
      If ptr > memsize
        memsize + 1000
        Redim memory(memsize)
      EndIf
    Case "<"
      ptr - 1
      If ptr < 0
        displayErrorThenEnd("Memory pointer out of range")
      EndIf
    Case "["
      If memory(ptr) = 0
        bracketSearch()
      EndIf
    Case "]"
      If memory(ptr) <> 0
        bracketSearch(-1)
      EndIf
  EndSelect
Next
displayEndingMsg()
```

