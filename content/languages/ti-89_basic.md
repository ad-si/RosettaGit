+++
title = "TI-89 BASIC"
description = ""
date = 2011-08-01T18:36:15Z
aliases = []
[extra]
id = 4200
[taxonomies]
categories = []
tags = []
+++
[TI-89 BASIC](https://rosettacode.org/wiki/:Category:TI-89_BASIC) is the programming language of the [Texas Instruments TI-89 graphing calculator](https://en.wikipedia.org/wiki/TI-89_series), as well as the TI-92+ and Voyage 200 models<!-- got this last info from http://everything2.com/title/TI+Basic; please revise with better list -->.

TI-89 BASIC is derived from [derived from::TI-83 BASIC](https://rosettacode.org/wiki/derived_from::TI-83_BASIC), but has some incompatible syntax and is much more powerful. For example, it supports arbitrary variable names, user-defined functions, local subroutines, comments, and working with symbolic expressions as well as numbers.

<br clear=all><!-- prevent language sidebar from spilling into columnar member list -->

## See Also
* [Wikipedia: TI-BASIC](https://en.wikipedia.org/wiki/TI-BASIC)

[Category:Mathematical programming languages](https://rosettacode.org/wiki/Category:Mathematical_programming_languages)


## Merged content



This implementation (in [TI-89 BASIC](https://rosettacode.org/wiki/TI-89_BASIC)) is basically a direct translation of the [TI-83 BASIC example](https://rosettacode.org/wiki/RCBF/TI-83_BASIC). It makes use of some TI-89 features; the program and memory size are given as parameters, and all variables are declared local.

IO is performed with numbers; character IO could be done (using char() and ord() to convert) but the TI-89 has no cursor for user program IO, so it would be necessary to either implement one or buffer output until the program exits or waits for input, in order to display more than one character per line.


```ti89b
Define bf(Raw) = Prgm
  Local valid, raw, BFprog, inst, memory, ip, brackets, memp
  "+-.,[]<>" → valid
  "" → BFprog
  {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0} → memory
  0 → brackets
  1 → memp
  For ip, 1, dim(raw)
    If inString(valid,mid(raw,ip,1))>0 Then
      BFprog & mid(raw,ip,1) → BFprog
      If mid(raw,ip,1) = "["
        brackets+1 → brackets
      If mid(raw,ip,1) = "]"
        brackets-1 → brackets
    EndIf
  EndFor
  If brackets ≠ 0 Then
    Disp "Uneven brackets"
    Stop
  EndIf
  For ip, 2, dim(BFprog)
    mid(BFprog,ip,1) → inst
    If inst = "+" Then
      memory[memp]+1 → memory[memp]
    ElseIf inst = "-" Then
      memory[memp]-1 → memory[memp]
    ElseIf inst = "." Then
      Disp memory[memp]
    ElseIf inst = "," Then
      Input value
      value → memory[memp]
    ElseIf inst = "[" and memory[memp] = 0 Then
      1 → brackets
      ip+1 → ip
      While ip ≤ dim(BFprog) and brackets ≠ 0
        If mid(BFprog,ip,1) = "[" Then
          brackets+1 → brackets
        ElseIf mid(BFprog,ip,1) = "]" Then
          brackets-1 → brackets
        EndIf
        ip+1 → ip
      EndWhile
      ip-1 → ip
    ElseIf inst = "]" and memory[memp] ≠ 0 Then
      1 → brackets
      ip-1 → ip
      While ip≥0 and brackets ≠ 0
        If mid(BFprog,ip,1) = "[" Then
          brackets+1 → brackets
        ElseIf mid(BFprog,ip,1) = "]" Then
          brackets-1 → brackets
        EndIf
        ip-1 → ip
      EndWhile
    ElseIf inst = "<" Then
      memp-1 → memp
      If memp ≤ 0 Then
        Disp "Memory pointer out of range"
        Stop
      EndIf
    ElseIf inst = ">" Then
      memp+1 → memp
      If memp > dim(memory)
        0 → memory[memp]
    EndIf
  EndFor
EndPrgm
```

