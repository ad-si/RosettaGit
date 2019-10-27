+++
title = "Doubly-linked list/AutoHotkey"
description = ""
date = 2010-05-26T00:02:17Z
aliases = []
[extra]
id = 7421
[taxonomies]
categories = []
tags = []
+++

{{collection | Linked_List}}

== Doubly-Linked List solutions ==

[http://www.autohotkey.com/forum/viewtopic.php?t=44657&postdays=0&postorder=asc&start=333 discussion]

```AutoHotkey
Build(D, L) { ; Double Linked list "D": D_{Head,Tail} = number>0 id of node
   Local i    ; D_%number%_{P,N,V}: Prev, Next, Value, D_%Free%: next free idx
   Loop Parse, L, `,
      i := A_Index
     ,%D%_%i%_P := i-1
     ,%D%_%i%_N := i+1
     ,%D%_%i%_V := A_LoopField
   %D%_%i%_N:= 0, %D%_Free := i+1
   %D%_Head := 1, %D%_Tail := i
}
Head(D) {
   Return %D%_Head
}
Tail(D) {
   Return %D%_Tail
}
Value(D,node) {
   Return %D%_%node%_V
}
Next(D,node) {
   Return %D%_%node%_N
}
Prev(D,node) {
   Return %D%_%node%_P
}
Traverse(D,FW=1) {
   Local i, t
   i := (FW:=!InStr("B.BW.Back.Backward",FW) || FW=1) ? Head(D) : Tail(D)
   While i
      t .= Value(D,i) "  ", i := FW ? Next(D,i) : Prev(D,i)
   Return t
}
AddBefore(D,n,V) {
   Local i, p
   %D%_Free := 1 + i := %D%_Free, %D%_%i%_V := V
   If (p := Prev(D,n)) ; n != head
      %D%_%p%_N := i,  %D%_%n%_P := i
     ,%D%_%i%_P := p,  %D%_%i%_N := n
   Else                ; add before head
      %D%_Head  := i,  %D%_%n%_P := i
     ,%D%_%i%_P := 0,  %D%_%i%_N := n
   Return i  
}
AddAfter(D,n,V) {
   Local i, t
   %D%_Free := 1 + i := %D%_Free, %D%_%i%_V := V
   If (t := Next(D,n)) ; n != tail
      %D%_%t%_P := i,  %D%_%n%_N := i
     ,%D%_%i%_P := n,  %D%_%i%_N := t
   Else                ; add after tail
      %D%_Tail  := i,  %D%_%n%_N := i
     ,%D%_%i%_P := n,  %D%_%i%_N := 0
   Return i  
}
Delete(D,n) {
   Local p, t
   p := Prev(D,n), t := Next(D,n)
   If (p && t)         ; in the middle
      %D%_%p%_N := t,  %D%_%t%_P := p
   Else If (p)         ; tail
      %D%_Tail  := p,  %D%_%p%_N := 0
   Else If (t)         ; head
      %D%_Head  := t,  %D%_%t%_P := 0
   Else                ; -> empty list
      %D%_Head  := 0,  %D%_Tail  := 0
}
; TESTS ---------------------------------------------------------------
Build("D",3)
n := AddBefore("D",Head("D"),2)
AddBefore("D",n,1)
MsgBox % Traverse("D","FW") ; 1  2  3

Build("D",3)
AddAfter("D",Head("D"),2)
n := AddAfter("D",Head("D"),1)
MsgBox % Traverse("D") ; 3  1  2

Delete("D",n)
MsgBox % Traverse("D") ; 3  2

Delete("D",Head("D"))
MsgBox % Traverse("D") ; 2

Delete("D",Tail("D"))
MsgBox % Traverse("D") ; empty

L = a,bb,1,2,x-y,$
msgbox % "build: " L
Build("D", L)
MsgBox % "traverse forward: `n" Traverse("D","FW")
MsgBox % "traverse backward: `n" Traverse("D","Back")
```

