+++
title = "Word wrap"
description = ""
date = 2019-10-12T22:51:07Z
aliases = []
[extra]
id = 11601
[taxonomies]
categories = []
tags = []
+++

{{task}}
[[Category:Text processing]]

Even today, with proportional fonts and complex layouts, there are still [[Template:Lines_too_long|cases]] where you need to wrap text at a specified column.


;Basic task:
The basic task is to wrap a paragraph of text in a simple way in your language.

If there is a way to do this that is built-in, trivial, or provided in a standard library, show that.  Otherwise implement the [http://en.wikipedia.org/wiki/Word_wrap#Minimum_length minimum length greedy algorithm from Wikipedia.]

Show your routine working on a sample of text at two different wrap columns.


;Extra credit:
Wrap text using a more sophisticated algorithm such as the Knuth and Plass TeX algorithm.
If your language provides this, you get easy extra credit,
but you ''must reference documentation'' indicating that the algorithm
is something better than a simple minimimum length algorithm.

If you have both basic and extra credit solutions, show an example where
the two algorithms give different results.





## 360 Assembly

The program uses one ASSIST macro (XPRNT) to keep the code as short as possible.

```360asm
*        Word wrap                 29/01/2017
WORDWRAP CSECT
         USING  WORDWRAP,R13
         B      72(R15)            skip savearea
         DC     17F'0'             savearea
         STM    R14,R12,12(R13)    prolog
         ST     R13,4(R15)         " <-
         ST     R15,8(R13)         " ->
         LR     R13,R15            " addressability
         MVC    S2,=CL96' '        s2=''
         SR     R0,R0
         STH    R0,LENS2           lens2=0
         LA     R8,1               i=1
LOOPI    CH     R8,=AL2(NTS)       do i=1 to hbound(ts)
         BH     ELOOPI             --
         LH     R4,LENS2
         LTR    R4,R4              if lens2=0
         BNZ    IFLENS2            then
         LR     R1,R8                i
         MH     R1,=H'48'
         LA     R14,TS-48(R1)
         MVC    S(48),0(R14)         s=ts(i)
         MVC    S+48(48),=CL48' '
         LA     R12,L'TS             jmax=length(ts)
         B      EIFLENS2           else
IFLENS2  MVC    S,=CL96' '           s=''
         LA     R6,S                 @s
         LH     R7,LENS2
         LA     R4,S2                @s2
         LH     R5,LENS2
         MVCL   R6,R4              substr(s,1,lens2)=substr(s2,1,lens2)
         LH     R2,LENS2
         LA     R2,1(R2)             lens2+1
         LR     R1,R8                i
         MH     R1,=H'48'
         LA     R14,TS-48(R1)        @ts(i)
         LA     R15,S-1
         AR     R15,R2
         MVC    0(48,R15),0(R14)     substr(s,lens2+1,48)=ts(i)
         LA     R12,L'S              jmax=length(s)
EIFLENS2 MVI    OKS2,X'01'         oks2=true
WHILEOK  CLI    OKS2,X'01'         do while(oks2)
         BNE    EWHILEOK           --
         LR     R9,R12             j=jmax  /*loop1*/
LOOPJ1   CH     R9,=H'1'           do j=jmax to 1 by -1
         BL     ELOOPJ1            --
         LA     R14,S-1              @s-1
         AR     R14,R9               j
         MVC    CJ(1),0(R14)         cj=substr(s,j,1)
         CLI    CJ,C' '              if cj^=' '
         BNE    ELOOPJ1              then leave j
         BCTR   R9,0                 j=j-1
         B      LOOPJ1             end do j
ELOOPJ1  STH    R9,LENS            lens=j  {length of s}
         MVI    OKJ,X'00'          okj=false  /*loop2*/
         LH     R11,W              js=w
         LH     R4,W
         CH     R4,LENS            if w>lens
         BNH    IFWLENS
         LH     R11,LENS           js=lens
IFWLENS  LR     R9,R11             j=js
LOOPJ2   CH     R9,=H'1'           do j=js to 1 by -1
         BL     ELOOPJ2            --
         LA     R14,S-1              @s-1
         AR     R14,R9               +j
         MVC    CJ(1),0(R14)         cj=substr(s,j,1)
         CLI    CJ,C' '              if cj=' '
         BNE    ITERJ2               then
         MVI    OKJ,X'01'              okj=true
         B      ELOOPJ2                leave j
ITERJ2   BCTR   R9,0                 j=j-1
         B      LOOPJ2             end do j
ELOOPJ2  CLI    OKJ,X'00'          if ^okj
         BNE    ELOOPK
         MVI    OKK,X'00'          okk=false  /*loop3*/
         LH     R10,W              k=w
LOOPK    CH     R10,LENS           do k=w to lens
         BH     ELOOPK             --
         LA     R14,S-1              @s-1
         AR     R14,R10              +k
         MVC    CK(1),0(R14)         ck=substr(s,k,1)
         CLI    CK,C' '              if ck=' '
         BNE    ITERK                then
         MVI    OKK,X'01'              okk=true
         B      ELOOPK                 leave k
ITERK    LA     R10,1(R10)           k=k+1
         B      LOOPK              end do k
ELOOPK   MVC    S2,=CL96' '        s2=' '
         SR     R0,R0
         STH    R0,LENS2           lens2=0
         MVI    CAS,X'01'          cas=true
         LH     R1,LENS
         CH     R1,W               lens<w
         BL     IFLENSLW
         MVI    CAS,X'00'          cas=false
IFLENSLW CLI    CAS,X'00'          if ^cas
         BNE    IFNOTCAS           then
         CLI    OKJ,X'01'            if okj
         BNE    NOKJ                 then
         STH    R9,LENS1               lens1=j
         LH     R2,LENS
         SR     R2,R9                  -j
         LA     R2,1(R2)
         STH    R2,LENS2               lens2=lens-j+1
         LA     R6,S1
         LR     R7,R9                  j
         LA     R4,S
         LR     R5,R7
         MVCL   R6,R4                  s1=substr(s,1,j)
         LH     R4,LENS2
         LTR    R4,R4                  if lens2>0
         BNP    ELJLENS2               then
         LA     R6,S2
         LH     R7,LENS2
         LA     R4,S(R9)                 @s(j+1)
         LR     R5,R7
         MVCL   R6,R4                    s2=substr(s,j+1,lens2)
         B      EFJLENS2
ELJLENS2 SR     R0,R0                  else
         STH    R0,LENS2                 lens2=0
EFJLENS2 B      IFNOTCAS
NOKJ     CLI    OKK,X'01'            else if okk
         BNE    NOTOKK
         STH    R10,LENS1              lens1=k
         LH     R2,LENS
         SR     R2,R10                 -k
         LA     R2,1(R2)
         STH    R2,LENS2               lens2=lens-k+1
         LA     R6,S1
         LR     R7,R10                 k
         LA     R4,S
         LR     R5,R7
         MVCL   R6,R4                  s1=substr(s,1,k)
         LH     R4,LENS2
         LTR    R4,R4                  if lens2>0
         BNP    ELKLENS2               then
         LA     R6,S2
         LH     R7,LENS2
         LA     R4,S(R10)                @s(k+1)
         LR     R5,R7
         MVCL   R6,R4                    s2=substr(s,k+1,lens2)
         B      EFKLENS2               else
ELKLENS2 SR     R0,R0
         STH    R0,LENS2                 lens2=0
EFKLENS2 B      IFNOTCAS             else
NOTOKK   LH     R0,LENS
         STH    R0,LENS1               lens1=lens
         MVC    S1,S                   s1=s
IFNOTCAS CLI    CAS,X'01'          if cas
         BNE    ELCAS              then
         LH     R7,LENS
         LA     R7,1(R7)
         LA     R6,S2
         LA     R4,S
         LR     R5,R7
         MVCL   R6,R4                s2=substr(s,1,lens+1)
         LH     R2,LENS
         LA     R2,1(R2)
         STH    R2,LENS2             lens2=lens+1
         B      EFCAS              else
ELCAS    LA     R6,PG
         LA     R7,L'PG
         LA     R4,S1
         LH     R5,LENS1
         ICM    R5,B'1000',=C' '     padding
         MVCL   R6,R4                pg=substr(s1,1,lens1)
         XPRNT  PG,L'PG              put skip list(pg)
EFCAS    MVI    OKS2,X'00'         oks2=false
         LH     R4,LENS2
         CH     R4,W               if lens2>w
         BNH    EFWLENS2           then
         MVI    OKS2,X'01'           oks2=true
         LH     R0,LENS2
         STH    R0,LENS              lens=lens2
         MVC    S,S2                 s=s2
EFWLENS2 B      WHILEOK            end while
EWHILEOK LA     R8,1(R8)           i=i+1
         B      LOOPI              end do i
ELOOPI   LH     R4,LENS2
         LTR    R4,R4              if lens2^=0
         BZ     EFLENS2N           then
         LA     R6,PG
         LA     R7,L'PG
         LA     R4,S2
         LH     R5,LENS2
         ICM    R5,B'1000',=C' '     padding
         MVCL   R6,R4                pg=substr(s2,1,lens2)
         XPRNT  PG,L'PG              put skip list(pg)
EFLENS2N L      R13,4(0,R13)       epilog
         LM     R14,R12,12(R13)    " restore
         XR     R15,R15            " rc=0
         BR     R14                exit
TS       DC     CL48'In olden times when wishing still helped one,'
         DC     CL48'there lived a king whose daughters were all,'
         DC     CL48'beautiful, but the youngest was so beautiful'
         DC     CL48'that the sun itself, which has seen so much,'
         DC     CL48'was astonished whenever it shone in her face.'
         DC     CL48'Close by the king''s castle lay a great dark'
         DC     CL48'forest, and under an old lime tree in the'
         DC     CL48'forest was a well, and when the day was very'
         DC     CL48'warm, the king''s child went out into the forest'
         DC     CL48'and sat down by the side of the cool fountain,'
         DC     CL48'and when she was bored she took a golden ball,'
         DC     CL48'and threw it up on high and caught it, and this'
         DC     CL48'ball was her favorite plaything.'
TSE      DC     0C
NTS      EQU    (TSE-TS)/L'TS
W        DC     H'36'              <-- input width  12<=w<=80
LENS     DS     H
S        DS     CL96
LENS1    DS     H
S1       DS     CL96
LENS2    DS     H
S2       DS     CL96
OKJ      DS     X
OKK      DS     X
OKS2     DS     X
CAS      DS     X
CJ       DS     CL1
CK       DS     CL1
PG       DS     CL80
         YREGS
         END    WORDWRAP
```

{{out}}

```txt

In olden times when wishing still
helped one, there lived a king
whose daughters were all,
beautiful, but the youngest was so
beautiful that the sun itself,
which has seen so much, was
astonished whenever it shone in her
face. Close by the king's castle
lay a great dark forest, and under
an old lime tree in the forest was
a well, and when the day was very
warm, the king's child went out
into the forest and sat down by the
side of the cool fountain, and when
she was bored she took a golden
ball, and threw it up on high and
caught it, and this ball was her
favorite plaything.

```



## Ada

The specification of a class '''Word_Wrap.Basic''' in a package '''Word_Wrap''':

```Ada
generic
   with procedure Put_Line(Line: String);
package Word_Wrap is

   type Basic(Length_Of_Output_Line: Positive) is tagged private;

   procedure Push_Word(State: in out Basic; Word: String);
   procedure New_Paragraph(State: in out Basic);
   procedure Finish(State: in out Basic);

private
   type Basic(Length_Of_Output_Line: Positive) is tagged record
      Line: String(1 .. Length_Of_Output_Line);
      Size: Natural := 0; -- Line(1 .. Size) is relevant
      Top_Of_Paragraph: Boolean := True;
   end record;

end Word_Wrap;
```


The implementation of that package:


```Ada
package body Word_Wrap is

   procedure Push_Word(State: in out Basic; Word: String) is
   begin
      if Word'Length + State.Size >= State.Length_Of_Output_Line then
         Put_Line(State.Line(1 .. State.Size));
         State.Line(1 .. Word'Length) := Word; -- may raise CE if Word too long
         State.Size := Word'Length;
      elsif State.Size > 0 then
         State.Line(State.Size+1 .. State.Size+1+Word'Length) := ' ' & Word;
         State.Size := State.Size + 1 + Word'Length;
      else
         State.Line(1 .. Word'Length) := Word;
         State.Size := Word'Length;
      end if;
      State.Top_Of_Paragraph := False;
   end Push_Word;

   procedure New_Paragraph(State: in out Basic) is
   begin
      Finish(State);
      if not State.Top_Of_Paragraph then
         Put_Line("");
         State.Top_Of_Paragraph := True;
      end if;
   end New_Paragraph;

   procedure Finish(State: in out Basic) is
   begin
      if State.Size > 0 then
         Put_Line(State.Line(1 .. State.Size));
         State.Size := 0;
      end if;
   end Finish;

end Word_Wrap;
```


Finally, the main program:


```Ada
with Ada.Text_IO, Word_Wrap, Ada.Strings.Unbounded, Ada.Command_Line;

procedure Wrap is

   use  Ada.Strings.Unbounded;

   Line: Unbounded_String;
   Word: Unbounded_String;

   function "+"(S: String) return Unbounded_String renames To_Unbounded_String;
   function "-"(U: Unbounded_String) return String renames To_String;

   package IO renames Ada.Text_IO;

   procedure Split(S: Unbounded_String; First, Rest: out Unbounded_String) is

      function Skip_Leading_Spaces(S: String) return String is
      begin
         if S="" then return "";
         elsif S(S'First) = ' ' then return S(S'First+1 .. S'Last);
         else return S;
         end if;
      end Skip_Leading_Spaces;

      Str: String := Skip_Leading_Spaces(-S);
      I: Positive := Str'First;
      J: Natural;
   begin
      -- read nonspaces for First output param
      J := I-1;
      while J < Str'Last and then Str(J+1) /= ' ' loop
         J := J + 1;
      end loop;
      First := + Str(I .. J);

      -- write output param Rest
      Rest  := + Skip_Leading_Spaces(Str(J+1 .. Str'Last));
   end Split;

   procedure Print(S: String) is
   begin
      IO.Put_Line(S);
   end Print;

   package WW is new Word_Wrap(Print);

   Wrapper: WW.Basic(Integer'Value(Ada.Command_Line.Argument(1)));

begin
   while not IO.End_Of_File loop
      Line := +IO.Get_Line;
      if Line = +"" then
         Wrapper.New_Paragraph;
         Line := +IO.Get_Line;
      end if;
      while Line /= +"" loop
         Split(Line, First => Word, Rest => Line);
         Wrapper.Push_Word(-Word);
      end loop;
   end loop;
   Wrapper.Finish;
end Wrap;
```


{{out}} set to 72 lines (with input picked by cut-and-paste from the task description):

```txt
Even today, with proportional fonts and complex layouts, there are still
cases where you need to wrap text at a specified column. The basic task
is to wrap a paragraph of text in a simple way in your language. If
there is a way to do this that is built-in, trivial, or provided in a
standard library, show that. Otherwise implement the minimum length
greedy algorithm from Wikipedia.

Show your routine working on a sample of text at two different wrap
columns.

Extra credit! Wrap text using a more sophisticated algorithm such as the
Knuth and Plass TeX algorithm. If your language provides this, you get
easy extra credit, but you must reference documentation indicating that
the algorithm is something better than a simple minimimum length
algorithm.

If you have both basic and extra credit solutions, show an example where
the two algorithms give different results.
```


Note that this solution properly deals with multi-paragraph inputs.
For more sophisticated algorithms (the extra credit), one could derive
a class '''Word_Wrap.<something>''' from '''Word_Wrap.Basic'''.


## AutoHotkey

Basic word-wrap. Formats text that has been copied to the clipboard.

```AutoHotkey
MsgBox, % "72`n" WrapText(Clipboard, 72) "`n`n80`n" WrapText(Clipboard, 80)
return

WrapText(Text, LineLength) {
	StringReplace, Text, Text, `r`n, %A_Space%, All
	while (p := RegExMatch(Text, "(.{1," LineLength "})(\s|\R+|$)", Match, p ? p + StrLen(Match) : 1))
		Result .= Match1 ((Match2 = A_Space || Match2 = A_Tab) ? "`n" : Match2)
	return, Result
}
```

{{Out}}

```txt
72
In olden times when wishing still helped one, there lived a king whose
daughters were all beautiful, but the youngest was so beautiful that the
sun itself, which has seen so much, was astonished whenever it shone in
her face.  Close by the king's castle lay a great dark forest, and under
an old lime-tree in the forest was a well, and when the day was very
warm, the king's child went out into the forest and sat down by the side
of the cool fountain, and when she was bored she took a golden ball, and
threw it up on high and caught it, and this ball was her favorite
plaything.

80
In olden times when wishing still helped one, there lived a king whose daughters
were all beautiful, but the youngest was so beautiful that the sun itself, which
has seen so much, was astonished whenever it shone in her face.  Close by the
king's castle lay a great dark forest, and under an old lime-tree in the forest
was a well, and when the day was very warm, the king's child went out into the
forest and sat down by the side of the cool fountain, and when she was bored she
took a golden ball, and threw it up on high and caught it, and this ball was her
favorite plaything.
```

Note: AutoHotkey can automatically word-wrap text in GUI controls such as text, edit boxes, buttons, etc. But, the word-wrap width is based on pixels, not characters.


## AWK

Basic word wrap.


```awk
function wordwrap_paragraph(p)
{
  if ( length(p) < 1 ) return

  split(p, words)
  spaceLeft = lineWidth
  line = words[1]
  delete words[1]

  for (i = 1; i <= length(words); i++) {
    word = words[i]
    if ( (length(word) + 1) > spaceLeft ) {
      print line
      line = word
      spaceLeft = lineWidth -  length(word)
    } else {
      spaceLeft -= length(word) + 1
      line = line " " word
    }
  }
  print line
}

BEGIN {
  lineWidth = width
  par = ""
}

/^[ \t]*$/ {
  wordwrap_paragraph(par)
  par = ""
}

!/^[ \t]*$/ {
  par = par " " $0
}

END {
  wordwrap_paragraph(par)
}
```


To test it,


```txt

awk -f wordwrap.awk -v width=80 &lt; text.txt

```



## BaCon


```qbasic
paragraph$ = "In olden times when wishing still helped one," \
" there lived a king whose daughters were all beautiful, but" \
" the youngest was so beautiful that the sun itself, which has" \
" seen so much, was astonished whenever it shone in her face." \
" Close by the king's castle lay a great dark forest, and under" \
" an old lime tree in the forest was a well, and when the day" \
" was very warm, the king's child went out into the forest and" \
" sat down by the side of the cool fountain, and when she was" \
" bored she took a golden ball, and threw it up on high and" \
" caught it, and this ball was her favorite plaything."

PRINT ALIGN$(paragraph$, 72, 0)
PRINT ALIGN$(paragraph$, 90, 0)
```

BaCon has the ALIGN$ function which can align text left-side, right-side, centered or both sides at any given column.
{{out}}

```txt

In olden times when wishing still helped one, there lived a king whose
daughters were all beautiful, but the youngest was so beautiful that
the sun itself, which has seen so much, was astonished whenever it
shone in her face. Close by the king's castle lay a great dark forest,
and under an old lime tree in the forest was a well, and when the day
was very warm, the king's child went out into the forest and sat down
by the side of the cool fountain, and when she was bored she took a
golden ball, and threw it up on high and caught it, and this ball was
her favorite plaything.
In olden times when wishing still helped one, there lived a king whose daughters were all
beautiful, but the youngest was so beautiful that the sun itself, which has seen so much,
was astonished whenever it shone in her face. Close by the king's castle lay a great dark
forest, and under an old lime tree in the forest was a well, and when the day was very
warm, the king's child went out into the forest and sat down by the side of the cool
fountain, and when she was bored she took a golden ball, and threw it up on high and
caught it, and this ball was her favorite plaything.

```



## Batch File

Basic word wrap.

```dos
@echo off

set "input=Lorem ipsum dolor sit amet, consectetur adipiscing elit. Donec a diam lectus. Sed sit amet ipsum mauris. Maecenas congue ligula ac quam viverra nec consectetur ante hendrerit. Donec et mollis dolor. Praesent et diam eget libero egestas mattis sit amet vitae augue. Nam tincidunt congue enim, ut porta lorem lacinia consectetur!"
rem call the function (the second parameter is the line width)
call :wrap "%input%" 40
echo(
call :wrap "%input%" 70
pause>nul
exit /b 0

:: The procedure
:wrap
set "line="
set "tmp_str=%~1"
set /a "width=%2", "width-=1"

:proc_loop
rem check if we are done already
if "%tmp_str%"=="" (
    setlocal enabledelayedexpansion
    if defined line echo(!line!
    endlocal & goto :EOF
)

rem not yet done, so take a word and process it
for /f "tokens=1,* delims= " %%A in ("%tmp_str%") do (
    set "word=%%A"
    set "tmp_str=%%B"

    setlocal enabledelayedexpansion
    if "!line!"=="" (set "testline=!word!") else (set "testline=!line! !word!")
    if "!testline:~%width%,1!" == "" (
        set "line=!testline!"
    ) else (
        echo(!line!
        set "line=!word!"
    )
)
endlocal & set "line=%line%"
goto proc_loop
```

{{Out}}

```txt
Lorem ipsum dolor sit amet, consectetur
adipiscing elit. Donec a diam lectus.
Sed sit amet ipsum mauris. Maecenas
congue ligula ac quam viverra nec
consectetur ante hendrerit. Donec et
mollis dolor. Praesent et diam eget
libero egestas mattis sit amet vitae
augue. Nam tincidunt congue enim, ut
porta lorem lacinia consectetur!

Lorem ipsum dolor sit amet, consectetur adipiscing elit. Donec a diam
lectus. Sed sit amet ipsum mauris. Maecenas congue ligula ac quam
viverra nec consectetur ante hendrerit. Donec et mollis dolor.
Praesent et diam eget libero egestas mattis sit amet vitae augue. Nam
tincidunt congue enim, ut porta lorem lacinia consectetur!
```



## Bracmat


```bracmat
(     str
    $ ( "In olden times when wishing still helped one, there lived a king "
        "whose daughters were all beautiful, but the youngest was so beautiful "
        "that the sun itself, which has seen so much, was astonished whenever "
        "it shone in her face.  Close by the king's castle lay a great dark "
        "forest, and under an old lime tree in the forest was a well, and when "
        "the day was very warm, the king's child went out into the forest and "
        "sat down by the side of the cool fountain, and when she was bored she "
        "took a golden ball, and threw it up on high and caught it, and this "
        "ball was her favorite plaything."
      )
  : ?Text
& ( wrap
  =   txt length line output q rem
    .   !arg:(?txt.?length)
      & :?output
      &   whl
        ' ( @( str$!txt
             :   ?line
                 (" " %?lastword [?q " " ?rem&!q:~<!length)
             )
          & !lastword " " !rem:?txt
          & !output !line \n:?output
          )
      & str$(!output !txt)
  )
& out$(str$("72 columns:\n" wrap$(!Text.72)))
& out$(str$("\n80 columns:\n" wrap$(!Text.80)))
);
```

{{out}}

```txt
72 columns:
In olden times when wishing still helped one, there lived a king whose
daughters were all beautiful, but the youngest was so beautiful that
the sun itself, which has seen so much, was astonished whenever it
shone in her face.  Close by the king's castle lay a great dark forest,
and under an old lime tree in the forest was a well, and when the day
was very warm, the king's child went out into the forest and sat down
by the side of the cool fountain, and when she was bored she took a
golden ball, and threw it up on high and caught it, and this ball was
her favorite plaything.

80 columns:
In olden times when wishing still helped one, there lived a king whose
daughters were all beautiful, but the youngest was so beautiful that the sun
itself, which has seen so much, was astonished whenever it shone in her face.
 Close by the king's castle lay a great dark forest, and under an old lime tree
in the forest was a well, and when the day was very warm, the king's child went
out into the forest and sat down by the side of the cool fountain, and when she
was bored she took a golden ball, and threw it up on high and caught it, and
this ball was her favorite plaything.
```



## C


```c
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

/* nonsensical hyphens to make greedy wrapping method look bad */
const char *string = "In olden times when wishing still helped one, there lived a king "
	"whose daughters were all beautiful, but the youngest was so beautiful "
	"that the sun itself, which has seen so much, was astonished whenever "
	"it shone-in-her-face.  Close-by-the-king's castle lay a great dark "
	"forest, and under an old lime-tree in the forest was a well, and when "
	"the day was very warm, the king's child went out into the forest and "
	"sat down by the side of the cool-fountain, and when she was bored she "
	"took a golden ball, and threw it up on high and caught it, and this "
	"ball was her favorite plaything.";

/*	Each but the last of wrapped lines comes with some penalty as the square
	of the diff between line length and desired line length.  If the line
	is longer than desired length, the penalty is multiplied by 100.  This
	pretty much prohibits the wrapping routine from going over right margin.
	If is ok to exceed the margin just a little, something like 20 or 40 will
	do.

	Knuth uses a per-paragraph penalty for line-breaking in TeX, which is--
	unlike what I have here--probably bug-free.
*/

#define PENALTY_LONG	100
#define PENALTY_SHORT	1

typedef struct word_t {
	const char *s;
	int len;
} *word;

word make_word_list(const char *s, int *n)
{
	int max_n = 0;
	word words = 0;

	*n = 0;
	while (1) {
		while (*s && isspace(*s)) s++;
		if (!*s) break;

		if (*n >= max_n) {
			if (!(max_n *= 2)) max_n = 2;
			words = realloc(words, max_n * sizeof(*words));
		}
		words[*n].s = s;
		while (*s && !isspace(*s)) s++;
		words[*n].len = s - words[*n].s;
		(*n) ++;
	}

	return words;
}

int greedy_wrap(word words, int count, int cols, int *breaks)
{
	int score = 0, line, i, j, d;

	i = j = line = 0;
	while (1) {
		if (i == count) {
			breaks[j++] = i;
			break;
		}

		if (!line) {
			line = words[i++].len;
			continue;
		}

		if (line + words[i].len < cols) {
			line += words[i++].len + 1;
			continue;
		}

		breaks[j++] = i;
		if (i < count) {
			d = cols - line;
			if (d > 0)	score += PENALTY_SHORT * d * d;
			else if (d < 0)	score += PENALTY_LONG * d * d;
		}

		line = 0;
	}
	breaks[j++] = 0;

	return score;
}

/* tries to make right margin more even; pretty sure there's an off-by-one bug
	here somewhere */
int balanced_wrap(word words, int count, int cols, int *breaks)
{
	int *best = malloc(sizeof(int) * (count + 1));

	/* do a greedy wrap to have some baseline score to work with, else
	   we'll end up with O(2^N) behavior */
	int best_score = greedy_wrap(words, count, cols, breaks);

	void test_wrap(int line_no, int start, int score) {
		int line = 0, current_score = -1, d;

		while (start <= count) {
			if (line) line ++;
			line += words[start++].len;
			d = cols - line;
			if (start < count || d < 0) {
				if (d > 0)
					current_score = score + PENALTY_SHORT * d * d;
				else
					current_score = score + PENALTY_LONG * d * d;
			} else {
				current_score = score;
			}

			if (current_score >= best_score) {
				if (d <= 0) return;
				continue;
			}

			best[line_no] = start;
			test_wrap(line_no + 1, start, current_score);
		}
		if (current_score >= 0 && current_score < best_score) {
			best_score = current_score;
			memcpy(breaks, best, sizeof(int) * (line_no));
		}
	}
	test_wrap(0, 0, 0);
	free(best);

	return best_score;
}

void show_wrap(word list, int count, int *breaks)
{
	int i, j;
	for (i = j = 0; i < count && breaks[i]; i++) {
		while (j < breaks[i]) {
			printf("%.*s", list[j].len, list[j].s);
			if (j < breaks[i] - 1)
				putchar(' ');
			j++;
		}
		if (breaks[i]) putchar('\n');
	}
}

int main(void)
{
	int len, score, cols;
	word list = make_word_list(string, &len);
	int *breaks = malloc(sizeof(int) * (len + 1));

	cols = 80;
	score = greedy_wrap(list, len, cols, breaks);
	printf("\n== greedy wrap at %d (score %d) ==\n\n", cols, score);
	show_wrap(list, len, breaks);

	score = balanced_wrap(list, len, cols, breaks);
	printf("\n== balanced wrap at %d (score %d) ==\n\n", cols, score);
	show_wrap(list, len, breaks);


	cols = 32;
	score = greedy_wrap(list, len, cols, breaks);
	printf("\n== greedy wrap at %d (score %d) ==\n\n", cols, score);
	show_wrap(list, len, breaks);

	score = balanced_wrap(list, len, cols, breaks);
	printf("\n== balanced wrap at %d (score %d) ==\n\n", cols, score);
	show_wrap(list, len, breaks);

	return 0;
}
```



## C++

Basic task.
{{trans|Go}}

```cpp
#include <iostream>
#include <sstream>
#include <string>

const char *text =
{
    "In olden times when wishing still helped one, there lived a king "
    "whose daughters were all beautiful, but the youngest was so beautiful "
    "that the sun itself, which has seen so much, was astonished whenever "
    "it shone in her face.  Close by the king's castle lay a great dark "
    "forest, and under an old lime tree in the forest was a well, and when "
    "the day was very warm, the king's child went out into the forest and "
    "sat down by the side of the cool fountain, and when she was bored she "
    "took a golden ball, and threw it up on high and caught it, and this "
    "ball was her favorite plaything."
};

std::string wrap(const char *text, size_t line_length = 72)
{
    std::istringstream words(text);
    std::ostringstream wrapped;
    std::string word;

    if (words >> word) {
        wrapped << word;
        size_t space_left = line_length - word.length();
        while (words >> word) {
            if (space_left < word.length() + 1) {
                wrapped << '\n' << word;
                space_left = line_length - word.length();
            } else {
                wrapped << ' ' << word;
                space_left -= word.length() + 1;
            }
        }
    }
    return wrapped.str();
}

int main()
{
    std::cout << "Wrapped at 72:\n" << wrap(text) << "\n\n";
    std::cout << "Wrapped at 80:\n" << wrap(text, 80) << "\n";
}
```

{{out}}

```txt

Wrapped at 72:
In olden times when wishing still helped one, there lived a king whose
daughters were all beautiful, but the youngest was so beautiful that the
sun itself, which has seen so much, was astonished whenever it shone in
her face. Close by the king's castle lay a great dark forest, and under
an old lime tree in the forest was a well, and when the day was very
warm, the king's child went out into the forest and sat down by the side
of the cool fountain, and when she was bored she took a golden ball, and
threw it up on high and caught it, and this ball was her favorite
plaything.

Wrapped at 80:
In olden times when wishing still helped one, there lived a king whose daughters
were all beautiful, but the youngest was so beautiful that the sun itself, which
has seen so much, was astonished whenever it shone in her face. Close by the
king's castle lay a great dark forest, and under an old lime tree in the forest
was a well, and when the day was very warm, the king's child went out into the
forest and sat down by the side of the cool fountain, and when she was bored she
took a golden ball, and threw it up on high and caught it, and this ball was her
favorite plaything.
```



## C#

Greedy algorithm:

```c#
namespace RosettaCode.WordWrap
{
    using System;
    using System.Collections.Generic;

    internal static class Program
    {
        private const string LoremIpsum = @"
Lorem ipsum dolor sit amet, consectetur adipiscing elit. Maecenas varius sapien
vel purus hendrerit vehicula. Integer hendrerit viverra turpis, ac sagittis arcu
pharetra id. Sed dapibus enim non dui posuere sit amet rhoncus tellus
consectetur. Proin blandit lacus vitae nibh tincidunt cursus. Cum sociis natoque
penatibus et magnis dis parturient montes, nascetur ridiculus mus. Nam tincidunt
purus at tortor tincidunt et aliquam dui gravida. Nulla consectetur sem vel
felis vulputate et imperdiet orci pharetra. Nam vel tortor nisi. Sed eget porta
tortor. Aliquam suscipit lacus vel odio faucibus tempor. Sed ipsum est,
condimentum eget eleifend ac, ultricies non dui. Integer tempus, nunc sed
venenatis feugiat, augue orci pellentesque risus, nec pretium lacus enim eu
nibh.";

        private static void Main()
        {
            foreach (var lineWidth in new[] { 72, 80 })
            {
                Console.WriteLine(new string('-', lineWidth));
                Console.WriteLine(Wrap(LoremIpsum, lineWidth));
            }
        }

        private static string Wrap(string text, int lineWidth)
        {
            return string.Join(string.Empty,
                               Wrap(
                                   text.Split(new char[0],
                                              StringSplitOptions
                                                  .RemoveEmptyEntries),
                                   lineWidth));
        }

        private static IEnumerable<string> Wrap(IEnumerable<string> words,
                                                int lineWidth)
        {
            var currentWidth = 0;
            foreach (var word in words)
            {
                if (currentWidth != 0)
                {
                    if (currentWidth + word.Length < lineWidth)
                    {
                        currentWidth++;
                        yield return " ";
                    }
                    else
                    {
                        currentWidth = 0;
                        yield return Environment.NewLine;
                    }
                }
                currentWidth += word.Length;
                yield return word;
            }
        }
    }
}
```

{{out}}

```txt
------------------------------------------------------------------------
Lorem ipsum dolor sit amet, consectetur adipiscing elit. Maecenas varius
sapien vel purus hendrerit vehicula. Integer hendrerit viverra turpis,
ac sagittis arcu pharetra id. Sed dapibus enim non dui posuere sit amet
rhoncus tellus consectetur. Proin blandit lacus vitae nibh tincidunt
cursus. Cum sociis natoque penatibus et magnis dis parturient montes,
nascetur ridiculus mus. Nam tincidunt purus at tortor tincidunt et
aliquam dui gravida. Nulla consectetur sem vel felis vulputate et
imperdiet orci pharetra. Nam vel tortor nisi. Sed eget porta tortor.
Aliquam suscipit lacus vel odio faucibus tempor. Sed ipsum est,
condimentum eget eleifend ac, ultricies non dui. Integer tempus, nunc
sed venenatis feugiat, augue orci pellentesque risus, nec pretium lacus
enim eu nibh.
--------------------------------------------------------------------------------
Lorem ipsum dolor sit amet, consectetur adipiscing elit. Maecenas varius sapien
vel purus hendrerit vehicula. Integer hendrerit viverra turpis, ac sagittis arcu
pharetra id. Sed dapibus enim non dui posuere sit amet rhoncus tellus
consectetur. Proin blandit lacus vitae nibh tincidunt cursus. Cum sociis natoque
penatibus et magnis dis parturient montes, nascetur ridiculus mus. Nam tincidunt
purus at tortor tincidunt et aliquam dui gravida. Nulla consectetur sem vel
felis vulputate et imperdiet orci pharetra. Nam vel tortor nisi. Sed eget porta
tortor. Aliquam suscipit lacus vel odio faucibus tempor. Sed ipsum est,
condimentum eget eleifend ac, ultricies non dui. Integer tempus, nunc sed
venenatis feugiat, augue orci pellentesque risus, nec pretium lacus enim eu
nibh.
```



## Clojure


```Clojure
;; Wrap line naive version
(defn wrap-line [size text]
  (loop [left size line [] lines []
         words (clojure.string/split text #"\s+")]
    (if-let [word (first words)]
      (let [wlen (count word)
            spacing (if (== left size) "" " ")
            alen (+ (count spacing) wlen)]
        (if (<= alen left)
          (recur (- left alen) (conj line spacing word) lines (next words))
          (recur (- size wlen) [word] (conj lines (apply str line)) (next words))))
      (when (seq line)
        (conj lines (apply str line))))))
```



```Clojure
;; Wrap line base on regular expression
(defn wrap-line [size text]
  (re-seq (re-pattern (str ".{1," size "}\\s|.{1," size "}"))
          (clojure.string/replace text #"\n" " ")))
```



```Clojure
;; cl-format based version
(defn wrap-line [size text]
  (clojure.pprint/cl-format nil (str "~{~<~%~1," size ":;~A~> ~}") (clojure.string/split text #" ")))
```


Usage example :

```Clojure
(def text "In olden times when wishing still helped one, there lived
a king whose daughters were all beautiful, but the youngest was so
beautiful that the sun itself, which has seen so much, was astonished
whenever it shone in her face.  Close by the king's castle lay a great
dark forest, and under an old lime-tree in the forest was a well, and
when the day was very warm, the king's child went out into the forest
and sat down by the side of the cool fountain, and when she was bored
she took a golden ball, and threw it up on high and caught it, and
this ball was her favorite plaything.")

(doseq [line (wrap-line 72 text)]
  (println line))
```


{{out}}

```txt
In olden times when wishing still helped one, there lived a king whose
daughters were all beautiful, but the youngest was so beautiful that the
sun itself, which has seen so much, was astonished whenever it shone in
her face. Close by the king's castle lay a great dark forest, and under
an old lime-tree in the forest was a well, and when the day was very
warm, the king's child went out into the forest and sat down by the side
of the cool fountain, and when she was bored she took a golden ball, and
threw it up on high and caught it, and this ball was her favorite
plaything.
```



## Common Lisp


```Lisp
;; Greedy wrap line

(defun greedy-wrap (str width)
  (setq str (concatenate 'string str " ")) ; add sentinel
  (do* ((len (length str))
        (lines nil)
        (begin-curr-line 0)
        (prev-space 0 pos-space)
        (pos-space (position #\Space str) (when (< (1+ prev-space) len) (position #\Space str :start (1+ prev-space)))) )
       ((null pos-space) (progn (push (subseq str begin-curr-line (1- len)) lines) (nreverse lines)) )
    (when (> (- pos-space begin-curr-line) width)
      (push (subseq str begin-curr-line prev-space) lines)
      (setq begin-curr-line (1+ prev-space)) )))

```


{{out}}

```txt
(setq str "In olden times when wishing still helped one, there lived a king whose daughters were all beautiful, but
 the youngest was so beautiful that the sun itself, which has seen so much, was astonished whenever it shone in her
 face. Close by the king's castle lay a great dark forest, and under an old lime tree in the forest was a well, and
 when the day was very warm, the king's child went out into the forest and sat down by the side of the cool fountain,
 and when she was bored she took a golden ball, and threw it up on high and caught it, and this ball was her favorite
 plaything.")

(greedy-wrap str 72)

("In olden times when wishing still helped one, there lived a king whose"
 "daughters were all beautiful, but the youngest was so beautiful that the"
 "sun itself, which has seen so much, was astonished whenever it shone in"
 "her face. Close by the king's castle lay a great dark forest, and under"
 "an old lime tree in the forest was a well, and when the day was very"
 "warm, the king's child went out into the forest and sat down by the side"
 "of the cool fountain, and when she was bored she took a golden ball, and"
 "threw it up on high and caught it, and this ball was her favorite"
 "plaything.")

(greedy-wrap str 80)

("In olden times when wishing still helped one, there lived a king whose daughters"
 "were all beautiful, but the youngest was so beautiful that the sun itself, which"
 "has seen so much, was astonished whenever it shone in her face. Close by the"
 "king's castle lay a great dark forest, and under an old lime tree in the forest"
 "was a well, and when the day was very warm, the king's child went out into the"
 "forest and sat down by the side of the cool fountain, and when she was bored she"
 "took a golden ball, and threw it up on high and caught it, and this ball was her"
 "favorite plaything.")
```



## D


### Standard Version


```d
void main() {
    immutable frog =
"In olden times when wishing still helped one, there lived a king
whose daughters were all beautiful, but the youngest was so beautiful
that the sun itself, which has seen so much, was astonished whenever
it shone in her face.  Close by the king's castle lay a great dark
forest, and under an old lime-tree in the forest was a well, and when
the day was very warm, the king's child went out into the forest and
sat down by the side of the cool fountain, and when she was bored she
took a golden ball, and threw it up on high and caught it, and this
ball was her favorite plaything.";

    import std.stdio, std.string;
    foreach (width; [72, 80])
        writefln("Wrapped at %d:\n%s\n", width, frog.wrap(width));
}
```

{{out}}

```txt
Wrapped at 72:
In olden times when wishing still helped one, there lived a king whose
daughters were all beautiful, but the youngest was so beautiful that the
sun itself, which has seen so much, was astonished whenever it shone in
her face. Close by the king's castle lay a great dark forest, and under
an old lime-tree in the forest was a well, and when the day was very
warm, the king's child went out into the forest and sat down by the side
of the cool fountain, and when she was bored she took a golden ball, and
threw it up on high and caught it, and this ball was her favorite
plaything.


Wrapped at 80:
In olden times when wishing still helped one, there lived a king whose daughters
were all beautiful, but the youngest was so beautiful that the sun itself, which
has seen so much, was astonished whenever it shone in her face. Close by the
king's castle lay a great dark forest, and under an old lime-tree in the forest
was a well, and when the day was very warm, the king's child went out into the
forest and sat down by the side of the cool fountain, and when she was bored she
took a golden ball, and threw it up on high and caught it, and this ball was her
favorite plaything.
```



### An Implementation

Basic algorithm. The text splitting is lazy.
{{trans|Go}}

```d
import std.algorithm;

string wrap(in string text, in int lineWidth) {
    auto words = text.splitter;
    if (words.empty) return null;
    string wrapped = words.front;
    words.popFront();
    int spaceLeft = lineWidth - wrapped.length;
    foreach (word; words)
        if (word.length + 1 > spaceLeft) {
            wrapped ~= "\n" ~ word;
            spaceLeft = lineWidth - word.length;
        } else {
            wrapped ~= " " ~ word;
            spaceLeft -= 1 + word.length;
        }
    return wrapped;
}

void main() {
    immutable frog =
"In olden times when wishing still helped one, there lived a king
whose daughters were all beautiful, but the youngest was so beautiful
that the sun itself, which has seen so much, was astonished whenever
it shone in her face.  Close by the king's castle lay a great dark
forest, and under an old lime-tree in the forest was a well, and when
the day was very warm, the king's child went out into the forest and
sat down by the side of the cool fountain, and when she was bored she
took a golden ball, and threw it up on high and caught it, and this
ball was her favorite plaything.";

    import std.stdio;
    foreach (width; [72, 80])
        writefln("Wrapped at %d:\n%s\n", width, frog.wrap(width));
}
```

{{out}}

```txt
Wrapped at 72:
In olden times when wishing still helped one, there lived a king whose
daughters were all beautiful, but the youngest was so beautiful that the
sun itself, which has seen so much, was astonished whenever it shone in
her face. Close by the king's castle lay a great dark forest, and under
an old lime-tree in the forest was a well, and when the day was very
warm, the king's child went out into the forest and sat down by the side
of the cool fountain, and when she was bored she took a golden ball, and
threw it up on high and caught it, and this ball was her favorite
plaything.

Wrapped at 80:
In olden times when wishing still helped one, there lived a king whose daughters
were all beautiful, but the youngest was so beautiful that the sun itself, which
has seen so much, was astonished whenever it shone in her face. Close by the
king's castle lay a great dark forest, and under an old lime-tree in the forest
was a well, and when the day was very warm, the king's child went out into the
forest and sat down by the side of the cool fountain, and when she was bored she
took a golden ball, and threw it up on high and caught it, and this ball was her
favorite plaything.
```



## Elena

ELENA 4.x :

```elena
import extensions;
import system'routines;
import extensions'text;

string text =
    "In olden times when wishing still helped one, there lived a king
whose daughters were all beautiful, but the youngest was so beautiful
that the sun itself, which has seen so much, was astonished whenever
it shone in her face.  Close by the king's castle lay a great dark
forest, and under an old lime tree in the forest was a well, and when
the day was very warm, the king's child went out into the forest and
sat down by the side of the cool fountain, and when she was bored she
took a golden ball, and threw it up on high and caught it, and this
ball was her favorite plaything.";

extension wrapOp
{
    wrap(int lineWidth)
    {
        int currentWidth := 0;

        ^ TokenEnumerator
            .new(self)
            .selectBy:(word)
            {
                currentWidth += word.Length;
                if (currentWidth > lineWidth)
                {
                    currentWidth := word.Length + 1;

                    ^ newLine + word + " "
                }
                else
                {
                    currentWidth += 1;

                    ^  word + " "
                }
            }
            .summarize(new StringWriter())
    }
}

public program()
{
    console.printLine(new StringWriter("-", 72));
    console.printLine(text.wrap(72));
    console.printLine(new StringWriter("-", 80));
    console.printLine(text.wrap(80));
}
```

{{out}}

```txt

------------------------------------------------------------------------
In olden times when wishing still helped one, there lived a king whose
daughters were all beautiful, but the youngest was so beautiful that the
sun itself, which has seen so much, was astonished whenever it shone in
her face. Close by the king's castle lay a great dark forest, and under
an old lime tree in the forest was a well, and when the day was very
warm, the king's child went out into the forest and sat down by the side
of the cool fountain, and when she was bored she took a golden ball, and
threw it up on high and caught it, and this ball was her favorite
plaything.
--------------------------------------------------------------------------------
In olden times when wishing still helped one, there lived a king whose daughters
were all beautiful, but the youngest was so beautiful that the sun itself, which
has seen so much, was astonished whenever it shone in her face. Close by the
king's castle lay a great dark forest, and under an old lime tree in the forest
was a well, and when the day was very warm, the king's child went out into the
forest and sat down by the side of the cool fountain, and when she was bored she
took a golden ball, and threw it up on high and caught it, and this ball was her
favorite plaything.

```



## Elixir

{{trans|Erlang}}

```elixir
defmodule Word_wrap do
  def paragraph( string, max_line_length ) do
    [word | rest] = String.split( string, ~r/\s+/, trim: true )
    lines_assemble( rest, max_line_length, String.length(word), word, [] )
      |> Enum.join( "\n" )
  end

  defp lines_assemble( [], _, _, line, acc ), do: [line | acc] |> Enum.reverse
  defp lines_assemble( [word | rest], max, line_length, line, acc ) do
    if line_length + 1 + String.length(word) > max do
      lines_assemble( rest, max, String.length(word), word, [line | acc] )
    else
      lines_assemble( rest, max, line_length + 1 + String.length(word), line <> " " <> word, acc )
    end
  end
end

text = """
Even today, with proportional fonts and complex layouts, there are still cases where you need to
wrap text at a specified column. The basic task is to wrap a paragraph of text in a simple way in
your language. If there is a way to do this that is built-in, trivial, or provided in a standard
library, show that. Otherwise implement the minimum length greedy algorithm from Wikipedia.
"""
Enum.each([72, 80], fn len ->
  IO.puts String.duplicate("-", len)
  IO.puts Word_wrap.paragraph(text, len)
end)
```


{{out}}

```txt

------------------------------------------------------------------------
Even today, with proportional fonts and complex layouts, there are still
cases where you need to wrap text at a specified column. The basic task
is to wrap a paragraph of text in a simple way in your language. If
there is a way to do this that is built-in, trivial, or provided in a
standard library, show that. Otherwise implement the minimum length
greedy algorithm from Wikipedia.
--------------------------------------------------------------------------------
Even today, with proportional fonts and complex layouts, there are still cases
where you need to wrap text at a specified column. The basic task is to wrap a
paragraph of text in a simple way in your language. If there is a way to do this
that is built-in, trivial, or provided in a standard library, show that.
Otherwise implement the minimum length greedy algorithm from Wikipedia.

```



## Erlang


```Erlang

-module( word_wrap ).

-export( [paragraph/2, task/0] ).

paragraph( String, Max_line_length ) ->
	Lines = lines( string:tokens(String, " "), Max_line_length ),
	string:join( Lines, "\n" ).

task() ->
	Paragraph = "Even today, with proportional fonts and complex layouts, there are still cases where you need to wrap text at a specified column. The basic task is to wrap a paragraph of text in a simple way in your language. If there is a way to do this that is built-in, trivial, or provided in a standard library, show that. Otherwise implement the minimum length greedy algorithm from Wikipedia.",
	io:fwrite( "~s~n~n", [paragraph(Paragraph, 72)] ),
	io:fwrite( "~s~n~n", [paragraph(Paragraph, 80)] ).



lines( [Word | T], Max_line_length ) ->
	{Max_line_length, _Length, Last_line, Lines} = lists:foldl( fun lines_assemble/2, {Max_line_length, erlang:length(Word), Word, []}, T ),
	lists:reverse( [Last_line | Lines] ).

lines_assemble( Word, {Max, Line_length, Line, Acc} ) when erlang:length(Word) + Line_length > Max -> {Max, erlang:length(Word), Word, [Line | Acc]};
lines_assemble( Word, {Max, Line_length, Line, Acc} ) -> {Max, Line_length + 1 + erlang:length(Word), Line ++ " " ++ Word, Acc}.

```

{{out}}

```txt

15> word_wrap:task().
Even today, with proportional fonts and complex layouts, there are still
cases where you need to wrap text at a specified column. The basic task
is to wrap a paragraph of text in a simple way in your language. If there
is a way to do this that is built-in, trivial, or provided in a standard
library, show that. Otherwise implement the minimum length greedy
algorithm from Wikipedia.

Even today, with proportional fonts and complex layouts, there are still cases
where you need to wrap text at a specified column. The basic task is to wrap a
paragraph of text in a simple way in your language. If there is a way to do this
that is built-in, trivial, or provided in a standard library, show that.
Otherwise implement the minimum length greedy algorithm from Wikipedia.

```


=={{header|F_Sharp|F#}}==
{{trans|C#}}

```fsharp
open System

let LoremIpsum = "
Lorem ipsum dolor sit amet, consectetur adipiscing elit. Maecenas varius sapien
vel purus hendrerit vehicula. Integer hendrerit viverra turpis, ac sagittis arcu
pharetra id. Sed dapibus enim non dui posuere sit amet rhoncus tellus
consectetur. Proin blandit lacus vitae nibh tincidunt cursus. Cum sociis natoque
penatibus et magnis dis parturient montes, nascetur ridiculus mus. Nam tincidunt
purus at tortor tincidunt et aliquam dui gravida. Nulla consectetur sem vel
felis vulputate et imperdiet orci pharetra. Nam vel tortor nisi. Sed eget porta
tortor. Aliquam suscipit lacus vel odio faucibus tempor. Sed ipsum est,
condimentum eget eleifend ac, ultricies non dui. Integer tempus, nunc sed
venenatis feugiat, augue orci pellentesque risus, nec pretium lacus enim eu
nibh."

let Wrap words lineWidth =
    let rec loop words currentWidth = seq {
        match (words : string list) with
        | word :: rest ->
            let (stuff, pos) =
                if currentWidth > 0 then
                    if currentWidth + word.Length < lineWidth then
                        (" ", (currentWidth + 1))
                    else
                        ("\n", 0)
                else ("", 0)
            yield stuff + word
            yield! loop rest (pos + word.Length)
        | _ -> ()
    }
    loop words 0

[<EntryPoint>]
let main argv =
    for n in [72; 80] do
        printfn "%s" (String('-', n))
        let l = Seq.toList (LoremIpsum.Split((null:char[]), StringSplitOptions.RemoveEmptyEntries))
        Wrap l n |> Seq.iter (printf "%s")
        printfn ""
    0
```

{{out}}
<pre style="font-size:smaller">------------------------------------------------------------------------
Lorem ipsum dolor sit amet, consectetur adipiscing elit. Maecenas varius
sapien vel purus hendrerit vehicula. Integer hendrerit viverra turpis,
ac sagittis arcu pharetra id. Sed dapibus enim non dui posuere sit amet
rhoncus tellus consectetur. Proin blandit lacus vitae nibh tincidunt
cursus. Cum sociis natoque penatibus et magnis dis parturient montes,
nascetur ridiculus mus. Nam tincidunt purus at tortor tincidunt et
aliquam dui gravida. Nulla consectetur sem vel felis vulputate et
imperdiet orci pharetra. Nam vel tortor nisi. Sed eget porta tortor.
Aliquam suscipit lacus vel odio faucibus tempor. Sed ipsum est,
condimentum eget eleifend ac, ultricies non dui. Integer tempus, nunc
sed venenatis feugiat, augue orci pellentesque risus, nec pretium lacus
enim eu nibh.
--------------------------------------------------------------------------------
Lorem ipsum dolor sit amet, consectetur adipiscing elit. Maecenas varius sapien
vel purus hendrerit vehicula. Integer hendrerit viverra turpis, ac sagittis arcu
pharetra id. Sed dapibus enim non dui posuere sit amet rhoncus tellus
consectetur. Proin blandit lacus vitae nibh tincidunt cursus. Cum sociis natoque
penatibus et magnis dis parturient montes, nascetur ridiculus mus. Nam tincidunt
purus at tortor tincidunt et aliquam dui gravida. Nulla consectetur sem vel
felis vulputate et imperdiet orci pharetra. Nam vel tortor nisi. Sed eget porta
tortor. Aliquam suscipit lacus vel odio faucibus tempor. Sed ipsum est,
condimentum eget eleifend ac, ultricies non dui. Integer tempus, nunc sed
venenatis feugiat, augue orci pellentesque risus, nec pretium lacus enim eu
nibh.
```



## Factor


```factor
USE: wrap.strings
IN: scratchpad "Most languages in widespread use today are applicative languages
: the central construct in the language is some form of function call, where a f
unction is applied to a set of parameters, where each parameter is itself the re
sult of a function call, the name of a variable, or a constant. In stack languag
es, a function call is made by simply writing the name of the function; the para
meters are implicit, and they have to already be on the stack when the call is m
ade. The result of the function call (if any) is then left on the stack after th
e function returns, for the next function to consume, and so on. Because functio
ns are invoked simply by mentioning their name without any additional syntax, Fo
rth and Factor refer to functions as words, because in the syntax they really ar
e just words." [ 60 wrap-string print nl ] [ 45 wrap-string print ] bi
```

{{out}}

```txt

Most languages in widespread use today are applicative
languages: the central construct in the language is some
form of function call, where a function is applied to a set
of parameters, where each parameter is itself the result of
a function call, the name of a variable, or a constant. In
stack languages, a function call is made by simply writing
the name of the function; the parameters are implicit, and
they have to already be on the stack when the call is made.
The result of the function call (if any) is then left on
the stack after the function returns, for the next function
to consume, and so on. Because functions are invoked simply
by mentioning their name without any additional syntax,
Forth and Factor refer to functions as words, because in the
syntax they really are just words.

Most languages in widespread use today are
applicative languages: the central construct
in the language is some form of function
call, where a function is applied to a set
of parameters, where each parameter is itself
the result of a function call, the name of a
variable, or a constant. In stack languages,
a function call is made by simply writing
the name of the function; the parameters are
implicit, and they have to already be on the
stack when the call is made. The result of
the function call (if any) is then left on
the stack after the function returns, for the
next function to consume, and so on. Because
functions are invoked simply by mentioning
their name without any additional syntax,
Forth and Factor refer to functions as words,
because in the syntax they really are just
words.

```



## Forth


```forth
\ wrap text
\ usage: gforth wrap.f in.txt 72

0. argc @ 1- arg >number 2drop drop constant maxLine

: .wrapped ( buf len -- )
  begin
    dup maxLine >
  while
    over maxLine
    begin 1- 2dup + c@ bl = until
    dup 1+ >r
    begin 1- 2dup + c@ bl <> until
    1+ type cr
    r> /string
  repeat type cr ;

: strip-nl ( buf len -- )
  bounds do
    i c@ 10 = if bl i c! then
  loop ;

argc @ 2 - arg slurp-file
2dup strip-nl
.wrapped
bye
```



## Fortran

Early Fortran provided no facility for manipulating text until the A format code was introduced by Fortran 4 that allowed characters to be read into variables, which could then be manipulated and written out. F77 introduced the CHARACTER data type which however did not have a notion of a variable-length string, other than via the programmer keeping track with auxiliary variables. F90 enabled the introduction via user-written functions and data types of a string-like facility, whereby a CHARACTER type variable would be resized on assignment. F95 formalised this facility as a part of the language.

There are no facilities for "flowing" text on output according to a specified width, though various direct methods are possible. For instance, given a variable containing thousands of characters,
```Fortran
      CHARACTER*12345 TEXT
       ...
      DO I = 0,120
        WRITE (6,*) TEXT(I*80 + 1:(I + 1)*80)
      END DO
```

would write forth the text with eighty characters per line, paying no attention to the content when it splits a line.

The following is in the style of F77 except for the use of the MODULE facility to simplify the usage of auxiliary variables. Otherwise, if there is not to be a simple mainline only, scratchpads would have to be shared via COMMON or the proliferation of a tedious number of parameters. The specification calls for the flowing of a single paragraph of text, but this routine is based on one written in the 1980s for the printing of programme source files (in Fortran or pl/i) whereby large blocks of comments would be recognised and re-flowed so as to fill more of the width of a 132-column lineprinter, thus allowing a broader canvas for documentation and yet, whenever there were changes, the source file did not have to be reformatted each time.

The basic ploy is that FLOW receives a wad of text and sends it forth without exceeding a specified WIDTH, holding any tail end until the next blob is supplied to be tacked on the end, with a separating space supplied. However, if a blob starts with a space this is deemed to be the start of a new paragraph, so any waiting text is rolled first even if a short line. To flush out any waiting text at the end, invoke FLOW with a blank (or null) parameter.

The source-listing programmes simply appended the incoming text to the end of the scratchpad, knowing that source files do not present long records, but here an incoming text may be much larger than any reasonable scratchpad so there is assessment of the available space first. Choosing a cut position is problematic. The scheme here is to split at spaces only; a more accomplished method might classify letters, digits and decimal points as being sequences that ought not be split. And should it be O'-hara or O-'hara? Still more difficult is proper hyphenation: des-ert (noun) or de-sert (verb) - the grammar of human languages being non-computable.

Should there be no suitable split in the fragment being appended, then, arbitrarily, if that fragment is short then it is not appended: the line is rolled with trailing spaces. But if it has more than six characters, it will be placed and a crude chop made.

```Fortran

      MODULE RIVERRUN	!Schemes for re-flowing wads of text to a specified line length.
       INTEGER BL,BLIMIT,BM	!Fingers for the scratchpad.
       PARAMETER (BLIMIT = 222)	!This should be enough for normal widths.
       CHARACTER*(BLIMIT) BUMF	!The scratchpad, accumulating text.
       INTEGER OUTBUMF		!Output unit number.
       DATA OUTBUMF/0/		!Thus detect inadequate initialisation.
       PRIVATE BL,BLIMIT,BM	!These names are not so unusual
       PRIVATE BUMF,OUTBUMF	!That no other routine will use them.
       CONTAINS
       INTEGER FUNCTION LSTNB(TEXT)  !Sigh. Last Not Blank.
Concocted yet again by R.N.McLean (whom God preserve) December MM.
Code checking reveals that the Compaq compiler generates a copy of the string and then finds the length of that when using the latter-day intrinsic LEN_TRIM. Madness!
Can't   DO WHILE (L.GT.0 .AND. TEXT(L:L).LE.' ')	!Control chars. regarded as spaces.
Curse the morons who think it good that the compiler MIGHT evaluate logical expressions fully.
Crude GO TO rather than a DO-loop, because compilers use a loop counter as well as updating the index variable.
Comparison runs of GNASH showed a saving of ~3% in its mass-data reading through the avoidance of DO in LSTNB alone.
Crappy code for character comparison of varying lengths is avoided by using ICHAR which is for single characters only.
Checking the indexing of CHARACTER variables for bounds evoked astounding stupidities, such as calculating the length of TEXT(L:L) by subtracting L from L!
Comparison runs of GNASH showed a saving of ~25-30% in its mass data scanning for this, involving all its two-dozen or so single-character comparisons, not just in LSTNB.
        CHARACTER*(*),INTENT(IN):: TEXT	!The bumf. If there must be copy-in, at least there need not be copy back.
        INTEGER L		!The length of the bumf.
         L = LEN(TEXT)		!So, what is it?
    1    IF (L.LE.0) GO TO 2	!Are we there yet?
         IF (ICHAR(TEXT(L:L)).GT.ICHAR(" ")) GO TO 2	!Control chars are regarded as spaces also.
         L = L - 1		!Step back one.
         GO TO 1		!And try again.
    2    LSTNB = L		!The last non-blank, possibly zero.
        RETURN			!Unsafe to use LSTNB as a variable.
       END FUNCTION LSTNB	!Compilers can bungle it.

        SUBROUTINE STARTFLOW(OUT,WIDTH)	!Preparation.
         INTEGER OUT	!Output device.
         INTEGER WIDTH	!Width limit.
          OUTBUMF = OUT		!Save these
          BM = WIDTH		!So that they don't have to be specified every time.
          IF (BM.GT.BLIMIT) STOP "Too wide!"	!Alas, can't show the values BLIMIT and WIDTH.
          BL = 0		!No text already waiting in BUMF
        END SUBROUTINE STARTFLOW!Simple enough.

        SUBROUTINE FLOW(TEXT)	!Add to the ongoing BUMF.
         CHARACTER*(*) TEXT	!The text to append.
         INTEGER TL		!Its last non-blank.
         INTEGER T1,T2		!Fingers to TEXT.
         INTEGER L		!A length.
          IF (OUTBUMF.LT.0) STOP "Call STARTFLOW first!"	!Paranoia.
          TL = LSTNB(TEXT)	!No trailing spaces, please.
          IF (TL.LE.0) THEN	!A blank (or null) line?
            CALL FLUSH		!Thus end the paragraph.
            RETURN		!Perhaps more text will follow, later.
          END IF		!Curse the (possible) full evaluation of .OR. expressions!
          IF (TEXT(1:1).LE." ") CALL FLUSH	!This can't be checked above in case LEN(TEXT) = 0.
Chunks of TEXT are to be appended to BUMF.
          T1 = 1		!Start at the start, blank or not.
   10     IF (BL.GT.0) THEN	!If there is text waiting in BUMF,
            BL = BL + 1		!Then this latest text is to be appended
            BUMF(BL:BL) = " "	!After one space.
          END IF		!So much for the join.
Consider the amount of text to be placed, TEXT(T1:TL)
          L = TL - T1 + 1	!Length of text to be placed.
          IF (BM - BL .GE. L) THEN	!Sufficient space available?
            BUMF(BL + 1:BM + L) = TEXT(T1:TL)	!Yes. Copy all the remaining text.
            BL = BL + L				!Advance the finger.
            IF (BL .GE. BM - 1) CALL FLUSH	!If there is no space for an addendum.
            RETURN				!Done.
          END IF		!Otherwise, there is an overhang.
Calculate the available space up to the end of a line. BUMF(BL + 1:BM)
          L = BM - BL		!The number of characters available in BUMF.
          T2 = T1 + L		!Finger the first character beyond the take.
          IF (TEXT(T2:T2) .LE. " ") GO TO 12	!A splitter character? Happy chance!
          T2 = T2 - 1		!Thus the last character of TEXT that could be placed in BUMF.
   11     IF (TEXT(T2:T2) .GT. " ") THEN	!Are we looking at a space yet?
            T2 = T2 - 1				!No. step back one.
            IF (T2 .GT. T1) GO TO 11		!And try again, if possible.
            IF (L .LE. 6) THEN	!No splitter found. For short appendage space,
              CALL FLUSH		!Starting a new line gives more scope.
              GO TO 10			!At the cost of spaces at the end.
            END IF		!But splitting words is unsavoury too.
            T2 = T1 + L - 1		!Alas, no split found.
          END IF		!So the end-of-line will force a split.
          L = T2 - T1 + 1	!The length I settle on.
   12     BUMF(BL + 1:BL + L) = TEXT(T1:T1 + L - 1)	!I could add a hyphen at the arbitrary chop...
          BL = BL + L		!The last placed.
          CALL FLUSH		!The line being full.
Consider what the flushed line didn't take. TEXT(T1 + L:TL)
          T1 = T1 + L		!Advance to fresh grist.
   13     IF (T1.GT.TL) RETURN	!Perhaps there is no more. No compound testing, alas.
          IF (TEXT(T1:T1).LE." ") THEN	!Does a space follow a line split?
            T1 = T1 + 1		!Yes. It would appear as a leading space in the output.
            GO TO 13          	!But the line split stands in for all that.
          END IF		!So, speed past all such.
          IF (T1.LE.TL) GO TO 10!Does anything remain?
          RETURN		!Nope.
         CONTAINS	!A convenience.
          SUBROUTINE FLUSH	!Save on repetition.
            IF (BL.GT.0) WRITE (OUTBUMF,"(A)") BUMF(1:BL)	!Roll the bumf, if any.
            BL = 0		!And be ready for more.
          END SUBROUTINE FLUSH	!Thus avoid the verbosity of repeated begin ... end blocks.
        END SUBROUTINE FLOW	!Invoke with one large blob, or, pieces.
      END MODULE RIVERRUN	!Flush the tail end with a null text.

      PROGRAM TEST
      USE RIVERRUN
      INTEGER MSG,IN
      CHARACTER*222 BUMF
      MSG = 6
      IN = 10
      CALL STARTFLOW(MSG,36)
      CALL FLOW("Fifteen men on a dead man's chest!")
      CALL FLOW(" Yo ho ho and a bottle of rum!")
      CALL FLOW("Drink and the devil have done for the rest!")
      CALL FLOW(" Yo ho ho and a bottle of rum!")
      CALL FLOW("")
      WRITE (MSG,*)
Chew into my source file for a second example.
      OPEN (IN,FILE="TextFlow.for",ACTION = "READ")
    1 READ (IN,2) BUMF
    2 FORMAT (A)
      IF (BUMF(1:1).NE."C") GO TO 1	!No comment block yet.
      CALL STARTFLOW(MSG,66)		!Found it!
    3 CALL FLOW(BUMF)			!Roll its text.
      READ (IN,2) BUMF			!Grab another line.
      IF (BUMF(1:1).EQ."C") GO TO 3	!And if a comment, append.
      CALL FLOW("")
      CLOSE (IN)
      END
```

Output: note that the chorus is presented with a leading space so as to force a new line start for it.

```txt

Fifteen men on a dead man's chest!
 Yo ho ho and a bottle of rum! Drink
and the devil have done for the
rest!
 Yo ho ho and a bottle of rum!

Concocted yet again by R.N.McLean (whom God preserve) December MM.
Code checking reveals that the Compaq compiler generates a copy of
the string and then finds the length of that when using the
latter-day intrinsic LEN_TRIM. Madness! Can't   DO WHILE (L.GT.0
.AND. TEXT(L:L).LE.' ') !Control chars. regarded as spaces. Curse
the morons who think it good that the compiler MIGHT evaluate
logical expressions fully. Crude GO TO rather than a DO-loop,
because compilers use a loop counter as well as updating the index
variable. Comparison runs of GNASH showed a saving of ~3% in its
mass-data reading through the avoidance of DO in LSTNB alone.
Crappy code for character comparison of varying lengths is avoided
by using ICHAR which is for single characters only. Checking the
indexing of CHARACTER variables for bounds evoked astounding
stupidities, such as calculating the length of TEXT(L:L) by
subtracting L from L! Comparison runs of GNASH showed a saving of
~25-30% in its mass data scanning for this, involving all its
two-dozen or so single-character comparisons, not just in LSTNB.

```

For text flowing purposes the actual source lister expected to find block comments with a space after the C (so that column three was the first character of the text to be flowed), so the above source would be listed as-is - except for overprinting key words and underlining, easy with a lineprinter but much more difficult on modern printers that expect a markup language instead.


## Go

Basic task, no extra credit.

```go
package main

import (
    "fmt"
    "strings"
)

func wrap(text string, lineWidth int) (wrapped string) {
    words := strings.Fields(text)
    if len(words) == 0 {
        return
    }
    wrapped = words[0]
    spaceLeft := lineWidth - len(wrapped)
    for _, word := range words[1:] {
        if len(word)+1 > spaceLeft {
            wrapped += "\n" + word
            spaceLeft = lineWidth - len(word)
        } else {
            wrapped += " " + word
            spaceLeft -= 1 + len(word)
        }
    }
    return
}

var frog = `
In olden times when wishing still helped one, there lived a king
whose daughters were all beautiful, but the youngest was so beautiful
that the sun itself, which has seen so much, was astonished whenever
it shone in her face.  Close by the king's castle lay a great dark
forest, and under an old lime-tree in the forest was a well, and when
the day was very warm, the king's child went out into the forest and
sat down by the side of the cool fountain, and when she was bored she
took a golden ball, and threw it up on high and caught it, and this
ball was her favorite plaything.`

func main() {
    fmt.Println("wrapped at 80:")
    fmt.Println(wrap(frog, 80))
    fmt.Println("wrapped at 72:")
    fmt.Println(wrap(frog, 72))
}
```

{{out}}

```txt

wrapped at 80:
In olden times when wishing still helped one, there lived a king whose daughters
were all beautiful, but the youngest was so beautiful that the sun itself, which
has seen so much, was astonished whenever it shone in her face. Close by the
king's castle lay a great dark forest, and under an old lime-tree in the forest
was a well, and when the day was very warm, the king's child went out into the
forest and sat down by the side of the cool fountain, and when she was bored she
took a golden ball, and threw it up on high and caught it, and this ball was her
favorite plaything.
wrapped at 72:
In olden times when wishing still helped one, there lived a king whose
daughters were all beautiful, but the youngest was so beautiful that the
sun itself, which has seen so much, was astonished whenever it shone in
her face. Close by the king's castle lay a great dark forest, and under
an old lime-tree in the forest was a well, and when the day was very
warm, the king's child went out into the forest and sat down by the side
of the cool fountain, and when she was bored she took a golden ball, and
threw it up on high and caught it, and this ball was her favorite
plaything.

```



## Groovy


'''Solution 1: Imperative Style'''

```groovy
def wordWrap(text, length = 80) {
    def sb = new StringBuilder()
    def line = ''

    text.split(/\s/).each { word ->
        if (line.size() + word.size() > length) {
            sb.append(line.trim()).append('\n')
            line = ''
        }
        line += " $word"
    }
    sb.append(line.trim()).toString()
}
```

Testing:

```groovy
def text = """\
    In olden times when wishing still helped one, there lived a king
    whose daughters were all beautiful, but the youngest was so beautiful
    that the sun itself, which has seen so much, was astonished whenever
    it shone in her face.  Close by the king's castle lay a great dark
    forest, and under an old lime tree in the forest was a well, and when
    the day was very warm, the king's child went out into the forest and
    sat down by the side of the cool fountain, and when she was bored she
    took a golden ball, and threw it up on high and caught it, and this
    ball was her favorite plaything.""".stripIndent().split('\n').join(' ')

println wordWrap(text)
println wordWrap(text, 120)
```

{{out}}

```txt
In olden times when wishing still helped one, there lived a king whose daughters
were all beautiful, but the youngest was so beautiful that the sun itself, which
has seen so much, was astonished whenever it shone in her face.  Close by the
king's castle lay a great dark forest, and under an old lime tree in the forest
was a well, and when the day was very warm, the king's child went out into the
forest and sat down by the side of the cool fountain, and when she was bored she
took a golden ball, and threw it up on high and caught it, and this ball was her
favorite plaything.
In olden times when wishing still helped one, there lived a king whose daughters were all beautiful, but the youngest
was so beautiful that the sun itself, which has seen so much, was astonished whenever it shone in her face.  Close by
the king's castle lay a great dark forest, and under an old lime tree in the forest was a well, and when the day was
very warm, the king's child went out into the forest and sat down by the side of the cool fountain, and when she was
bored she took a golden ball, and threw it up on high and caught it, and this ball was her favorite plaything.
```


'''Solution 2: Using Inject - Functional Style'''

A solution using the groovy list.inject method which corresponds to foldLeft in other languages.


```groovy

String wordWrap(str, width=80) {
  str.tokenize(' ').inject([[]]) { rows, word ->
    if (rows.last().join(' ').length() + word.length() <= width) rows.last() << word else rows << [word]
    rows
  }.collect { it.join(' ') }.join('\n')
}

```


this solution shows off the more functional aspects of groovy.

'''Solution 3: Ninja Style - @TailRecursive and performant'''

For very large strings (say Shakespeare's complete works which comes in around 7MB in text), the two above solutions are not very performant as they copy large portions of the input string multiple times.

Throwing away all readability, using a number of groovy tricks (abusing default parameter values etc) and just going for performance and terseness of code we get the following:


```groovy

import groovy.transform.TailRecursive
import static java.lang.Math.min

@TailRecursive
String wordWrap(str, w, i=w, b=''<<'', len=str.length()-1, x=0) {
  b.setCharAt(x = (b << str[b.length()..i]).lastIndexOf(' '), '\n' as char)
  b.length()+w >= len ? b << str[i..-1] : wordWrap(str, w, min(x+w+1, len), b, len, 0)
}

```


Should be noted that this is not idiomatic groovy or a recommended way of programming, but it is interesting as an exercise.

Assuming width of 80, we essentially jump 80 characters forward in the text, look backwards for the first space we find, replace that space with a newline, jump forwards 80 characters from the newly inserted newline, look backwards for a space etc.

This means we never look at every character in the input text and we just replace spaces with newlines as we go.

Note that this solution uses recursion and the @TailRecursive annotation which expands the recursive calls into a non-recursive loop at runtime, thus avoiding stack overflow exceptions for large data sets. Note also that the following expressions are equivalent:


```groovy

def a = new StringBuilder()
def a = '' << ''

```


Should also be noted that this solution ignores and breaks for the case where words are longer than a line. I have a version which takes this case into account but I figured this was unreadable enough.

Testing can be done as above with the exception that the second wrap width parameter is required.

As an anecdotal baseline for a performance comparison, running 7MB of English text (Shakespeare) through the first algorithm 10 times takes around 3100ms on my current workstation (with a number of warm-up iterations excluded) and running the last algorithm through the same exercise takes around 230ms.


## Haskell

Greedy wrapping:

```haskell
ss =
  concat
    [ "In olden times when wishing still helped one, there lived a king"
    , "whose daughters were all beautiful, but the youngest was so beautiful"
    , "that the sun itself, which has seen so much, was astonished whenever"
    , "it shone in her face.  Close by the king's castle lay a great dark"
    , "forest, and under an old lime-tree in the forest was a well, and when"
    , "the day was very warm, the king's child went out into the forest and"
    , "sat down by the side of the cool fountain, and when she was bored she"
    , "took a golden ball, and threw it up on high and caught it, and this"
    , "ball was her favorite plaything."
    ]

wordwrap maxlen = wrap_ 0 . words
  where
    wrap_ _ [] = "\n"
    wrap_ pos (w:ws)
              -- at line start: put down the word no matter what
      | pos == 0 = w ++ wrap_ (pos + lw) ws
      | pos + lw + 1 > maxlen = '\n' : wrap_ 0 (w : ws)
      | otherwise = ' ' : w ++ wrap_ (pos + lw + 1) ws
      where
        lw = length w

main = mapM_ putStr [wordwrap 72 ss, "\n", wordwrap 32 ss]
```



Alternative greedy wrapping:
```haskell
import Data.List (inits, tails, tail)

testString =
  concat
    [ "In olden times when wishing still helped one, there lived a king"
    , " whose daughters were all beautiful, but the youngest was so beautiful"
    , " that the sun itself, which has seen so much, was astonished whenever"
    , " it shone in her face.  Close by the king's castle lay a great dark"
    , " forest, and under an old lime-tree in the forest was a well, and when"
    , " the day was very warm, the king's child went out into the forest and"
    , " sat down by the side of the cool fountain, and when she was bored she"
    , " took a golden ball, and threw it up on high and caught it, and this"
    , " ball was her favorite plaything."
    ]

wWrap'' _ [] = []
wWrap'' i ss =
  (\(a, b) -> a : wWrap'' i b) $
  last . filter ((<= i) . length . unwords . fst) $ zip (inits ss) (tails ss)

wWrap :: Int -> String -> String
wWrap i = unlines . map unwords . wWrap'' i . words . concat . lines

main = putStrLn $ wWrap 80 testString
```


=={{header|Icon}} and {{header|Unicon}}==

The following works in both languages.


```unicon

procedure main(A)
    ll := integer(A[1]) | 72
    wordWrap(&input, ll)
end

procedure wordWrap(f, ll)
    every (sep := "", s := "", w := words(f)) do
       if w == "\n" then write(1(.s, s := sep := ""),"\n")
       else if (*s + *w) >= ll then write(1(.s, s := w, sep := " "))
                               else (s ||:= .sep||("\n" ~== w), sep := " ")
    if *s > 0 then write(s)
end

procedure words(f)
    static wc
    initial wc := &cset -- ' \t'   # Loose definition of a 'word'...
    while l := !f do {
        l ? while tab(upto(wc)) do suspend tab(many(wc))\1
        if *trim(l) = 0 then suspend "\n"   # Paragraph boundary
        }
end
```


Sample runs:

```txt

->ww <ww.icn
procedure main(A) ll := integer(A[1]) | 72 wordWrap(&input, ll) end

procedure wordWrap(f, ll) every (sep := "", s := "", w := words(f)) do
if w == "\n" then write(1(.s, s := sep := ""),"\n") else if (*s + *w) >=
ll then write(1(.s, s := w, sep := " ")) else (s ||:= .sep||("\n" ~==
w), sep := " ") if *s > 0 then write(s) end

procedure words(f) static wc initial wc := &cset -- ' \t' # Loose
definition of a 'word'... while l := !f do { l ? while tab(upto(wc)) do
suspend tab(many(wc))\1 if *trim(l) = 0 then suspend "\n" # Paragraph
boundary } end
->ww 50 <ww.icn
procedure main(A) ll := integer(A[1]) | 72
wordWrap(&input, ll) end

procedure wordWrap(f, ll) every (sep := "", s :=
"", w := words(f)) do if w == "\n" then
write(1(.s, s := sep := ""),"\n") else if (*s +
*w) >= ll then write(1(.s, s := w, sep := " "))
else (s ||:= .sep||("\n" ~== w), sep := " ") if *s
> 0 then write(s) end

procedure words(f) static wc initial wc := &cset
-- ' \t' # Loose definition of a 'word'... while l
:= !f do { l ? while tab(upto(wc)) do suspend
tab(many(wc))\1 if *trim(l) = 0 then suspend "\n"
# Paragraph boundary } end
->

```


=={{header|IS-BASIC}}==
The word warp, any kind of text alignment, specifying tab positions are basic services of the EXOS operating system.
<lang IS-BASIC>100 TEXT 80
110 CALL WRITE(12,68,0)
120 PRINT :CALL WRITE(10,70,1)
130 DEF WRITE(LEFTMARGIN,RIGHTMARGIN,JUSTIFIED)
140   STRING S$*254
150   RESTORE
160   PRINT TAB(LEFTMARGIN);CHR$(243);
170   PRINT TAB(RIGHTMARGIN-1);CHR$(251)
180   DO
190     READ IF MISSING EXIT DO:S$
200     PRINT S$;
210   LOOP
220   IF JUSTIFIED THEN PRINT CHR$(248) ! <- Extra credit :-)
230   PRINT
240 END DEF
250 DATA "In olden times when wishing still helped one, there lived a king whose daughters were all beautiful, but the youngest was so beautiful that the sun itself, which has seen so much, was astonished whenever it shone in her face. "
260 DATA "Close by the king's castle lay a great dark forest, and under an old lime-tree in the forest was a well, and when the day was very warm, the king's child went out into the forest and sat down by the side of the cool fountain, "
270 DATA "and when she was bored she took a golden ball, and threw it up on high and caught it, and this ball was her favorite plaything."

```


{{out}}

```txt
           In olden times when wishing still helped one, there
           lived a king whose daughters were all beautiful, but the
           youngest was so beautiful that the sun itself, which has
           seen so much, was astonished whenever it shone in her
           face. Close by the king's castle lay a great dark
           forest, and under an old lime-tree in the forest was a
           well, and when the day was very warm, the king's child
           went out into the forest and sat down by the side of the
           cool fountain, and when she was bored she took a golden
           ball, and threw it up on high and caught it, and this
           ball was her favorite plaything.

         In  olden times when wishing still helped one, there lived a
         king  whose  daughters  were all beautiful, but the youngest
         was  so  beautiful  that  the sun  itself, which has seen so
         much, was astonished whenever it shone in her face. Close by
         the  king's castle lay a great dark forest, and under an old
         lime-tree  in  the  forest  was a well, and when the day was
         very warm, the king's child went out into the forest and sat
         down  by  the  side  of  the cool fountain, and when she was
         bored  she  took  a golden ball, and threw it up on high and
         caught it, and this ball was her favorite plaything.
```



## J

'''Solution''':
```j
ww        =:  75&$: : wrap
  wrap    =: (] turn edges) ,&' '
    turn  =:  LF"_`]`[}
    edges =: (_1 + ] #~ 1 ,~ 2 >/\ |) [: +/\ #;.2
```

'''Example''':
```j
   GA =: 'Four score and seven years ago, our forefathers brought forth upon this continent a new nation, dedicated to the proposition that all men were created equal.'

   ww GA  NB.  Wrap at 75 chars by default
Four score and seven years ago, our forefathers brought forth upon this
continent a new nation, dedicated to the proposition that all men were
created equal.

   20 ww GA  NB.  Specify different length
Four score and
seven years ago, our
forefathers brought
forth upon this
continent a new nation,
dedicated to the
proposition that all men
were created equal.
```



## Java


```java

package rosettacode;

import java.util.StringTokenizer;

public class WordWrap
{
	int defaultLineWidth=80;
	int defaultSpaceWidth=1;
	void minNumLinesWrap(String text)
	{
		minNumLinesWrap(text,defaultLineWidth);
	}
	void minNumLinesWrap(String text,int LineWidth)
	{
		StringTokenizer st=new StringTokenizer(text);
		int SpaceLeft=LineWidth;
		int SpaceWidth=defaultSpaceWidth;
		while(st.hasMoreTokens())
		{
			String word=st.nextToken();
			if((word.length()+SpaceWidth)>SpaceLeft)
			{
				System.out.print("\n"+word+" ");
				SpaceLeft=LineWidth-word.length();
			}
			else
			{
				System.out.print(word+" ");
				SpaceLeft-=(word.length()+SpaceWidth);
			}
		}
	}
	public static void main(String[] args)
	{
		WordWrap now=new WordWrap();
		String wodehouse="Old Mr MacFarland (_said Henry_) started the place fifteen years ago. He was a widower with one son and what you might call half a daughter. That's to say, he had adopted her. Katie was her name, and she was the child of a dead friend of his. The son's name was Andy. A little freckled nipper he was when I first knew him--one of those silent kids that don't say much and have as much obstinacy in them as if they were mules. Many's the time, in them days, I've clumped him on the head and told him to do something; and he didn't run yelling to his pa, same as most kids would have done, but just said nothing and went on not doing whatever it was I had told him to do. That was the sort of disposition Andy had, and it grew on him. Why, when he came back from Oxford College the time the old man sent for him--what I'm going to tell you about soon--he had a jaw on him like the ram of a battleship. Katie was the kid for my money. I liked Katie. We all liked Katie.";
		System.out.println("DEFAULT:");
		now.minNumLinesWrap(wodehouse);
		System.out.println("\n\nLINEWIDTH=120");
		now.minNumLinesWrap(wodehouse,120);
	}

}


```




## JavaScript



### Recursive

'''Solution''':
```javascript

function wrap (text, limit) {
  if (text.length > limit) {
    // find the last space within limit
    var edge = text.slice(0, limit).lastIndexOf(' ');
    if (edge > 0) {
      var line = text.slice(0, edge);
      var remainder = text.slice(edge + 1);
      return line + '\n' + wrap(remainder, limit);
    }
  }
  return text;
}

```

'''Example''':
```javascript

console.log(wrap(text, 80));

```


{{out}}

```txt

Wrap text using a more sophisticated algorithm such as the Knuth and Plass TeX
algorithm. If your language provides this, you get easy extra credit, but you
must reference documentation indicating that the algorithm is something better
than a simple minimimum length algorithm.

```


'''Example''':
```javascript

console.log(wrap(text, 42));

```


{{out}}

```txt

Wrap text using a more sophisticated
algorithm such as the Knuth and Plass TeX
algorithm. If your language provides
this, you get easy extra credit, but you
must reference documentation indicating
that the algorithm is something better
than a simple minimimum length algorithm.

```



### Simple regex


A simple regex suffices (and proves fastest) for the greedy version:


```javascript
(function (width) {
    'use strict';

    function wrapByRegex(n, s) {
        return s.match(
                RegExp('.{1,' + n + '}(\\s|$)', 'g')
            )
            .join('\n');
    }

    return wrapByRegex(width,
'Even today, with proportional fonts and compl\
ex layouts, there are still cases where you ne\
ed to wrap text at a specified column. The bas\
ic task is to wrap a paragraph of text in a si\
mple way in your language. If there is a way t\
o do this that is built-in, trivial, or provid\
ed in a standard library, show that. Otherwise\
 implement the minimum length greedy algorithm\
 from Wikipedia.'
    )

})(60);
```


{{Out}}

```txt
Even today, with proportional fonts and complex layouts,
there are still cases where you need to wrap text at a
specified column. The basic task is to wrap a paragraph of
text in a simple way in your language. If there is a way to
do this that is built-in, trivial, or provided in a standard
library, show that. Otherwise implement the minimum length
greedy algorithm from Wikipedia.
```



###  EcmaScript 6


```javascript

/**
 * [wordwrap description]
 * @param  {[type]}  text  [description]
 * @param  {Number}  width [description]
 * @param  {String}  br    [description]
 * @param  {Boolean} cut   [description]
 * @return {[type]}        [description]
 */
function wordwrap(text, width = 80, br = '\n', cut = false) {
  // Приводим к uint
  // 0..2^32-1 либо 0..2^64-1
  width >>>= 0;
  // Длина текста меньше или равна максимальной
  if (0 === width || text.length <= width) {
    return text;
  }
  // Разбиваем текст на строки
  return text.split('\n').map(line => {
    if (line.length <= width) {
      return line;
    }
    // Разбиваем строку на слова
    let words = line.split(' ');
    // Если требуется, то обрезаем длинные слова
    if (cut) {
      let temp = [];
      for (const word of words) {
        if (word.length > width) {
          let i = 0;
          const length = word.length;
          while (i < length) {
            temp.push(word.slice(i, Math.min(i + width, length)));
            i += width;
          }
        } else {
          temp.push(word);
        }
      }
      words = temp;
    }
    // console.log(words);
    // Собираем новую строку
    let wrapped = words.shift();
    let spaceLeft = width - wrapped.length;
    for (const word of words) {
      if (word.length + 1 > spaceLeft) {
        wrapped += br + word;
        spaceLeft = width - word.length;
      } else {
        wrapped += ' ' + word;
        spaceLeft -= 1 + word.length;
      }
    }
    return wrapped;
  }).join('\n'); // Объединяем элементы массива по LF
}

```


'''Example'''
```javascript

console.log(wordwrap("The quick brown fox jumped over the lazy dog.", 20, "<br />\n"));

```


{{out}}

```txt
The quick brown fox<br />
jumped over the lazy<br />
dog.
```



## jq

{{works with|jq|>1.4}}
The following implementation requires a version of jq with splits/1, for splitting on whitespace.

In jq, all strings are Unicode strings, for which the length is calculated as the number of codepoints.

```jq
# Simple greedy algorithm.
# Note: very long words are not truncated.
# input: a string
# output: an array of strings
def wrap_text(width):
  reduce splits("\\s+") as $word
    ([""];
     .[length-1] as $current
     | ($word|length) as $wl
     | (if $current == "" then 0 else 1 end) as $pad
     | if $wl + $pad + ($current|length) <= width
       then .[-1] += ($pad * " ") + $word
       else . + [ $word]
       end );
```

'''Task 1''':

```jq
"aaa bb cc ddddd" | wrap_text(6)[]  # wikipedia example
```

{{Out}}
 aaa bb
 cc
 ddddd
'''Task 2''':

```jq
"aaa bb cc ddddd" | wrap_text(5)[]
```

{{Out}}
 aaa
 bb cc
 ddddd

'''With input from a file''': Russian.txt
<div style="overflow:scroll; height:100px;">

```sh
советских военных судов и самолетов была отмечена в Японском море после появления там двух американских авианосцев. Не
менее 100 советских самолетов поднялись в воздух, когдаамериканские
авианосцы "Уинсон" и "Мидуэй" приблизились на 50 миль к Владивостоку.

```
</div>
'''Main''':
 wrap_text(40)[]
{{Out}}

```sh
$ jq -M -R -s -r -f Word_wrap.jq Russian.txt
советских военных судов и самолетов была
отмечена в Японском море после появления
там двух американских авианосцев. Не
менее 100 советских самолетов поднялись
в воздух, когдаамериканские авианосцы
"Уинсон" и "Мидуэй" приблизились на 50
миль к Владивостоку.

```



## Julia

{{works with|Julia|0.6}}

Using [https://github.com/carlobaldassi/TextWrap.jl TextWrap.jl] library.


```julia
using TextWrap

text = """Reformat the single paragraph in 'text' to fit in lines of no more
    than 'width' columns, and return a new string containing the entire
    wrapped paragraph.  As with wrap(), tabs are expanded and other
    whitespace characters converted to space.  See TextWrapper class for
    available keyword args to customize wrapping behaviour."""

println("# Wrapped at 80 chars")
print_wrapped(text, width=80)
println("\n\n# Wrapped at 70 chars")
print_wrapped(text, width=70)
```


{{out}}

```txt
# Wrapped at 80 chars
Reformat the single paragraph in 'text' to fit in lines of no more than 'width'
columns, and return a new string containing the entire wrapped paragraph.  As
with wrap(), tabs are expanded and other whitespace characters converted to
space.  See TextWrapper class for available keyword args to customize wrapping
behaviour.

# Wrapped at 70 chars
Reformat the single paragraph in 'text' to fit in lines of no more
than 'width' columns, and return a new string containing the entire
wrapped paragraph.  As with wrap(), tabs are expanded and other
whitespace characters converted to space.  See TextWrapper class for
available keyword args to customize wrapping behaviour.
```



## Kotlin


```scala
// version 1.1.3

val text =
    "In olden times when wishing still helped one, there lived a king " +
    "whose daughters were all beautiful, but the youngest was so beautiful " +
    "that the sun itself, which has seen so much, was astonished whenever " +
    "it shone in her face.  Close by the king's castle lay a great dark " +
    "forest, and under an old lime tree in the forest was a well, and when " +
    "the day was very warm, the king's child went out into the forest and " +
    "sat down by the side of the cool fountain, and when she was bored she " +
    "took a golden ball, and threw it up on high and caught it, and this " +
    "ball was her favorite plaything."

fun greedyWordwrap(text: String, lineWidth: Int): String {
    val words = text.split(' ')
    val sb = StringBuilder(words[0])
    var spaceLeft = lineWidth - words[0].length
    for (word in words.drop(1)) {
        val len = word.length
        if (len + 1 > spaceLeft) {
            sb.append("\n").append(word)
            spaceLeft = lineWidth - len
        }
        else {
            sb.append(" ").append(word)
            spaceLeft -= (len + 1)
        }
    }
    return sb.toString()
}

fun main(args: Array<String>) {
    println("Greedy algorithm - wrapped at 72:")
    println(greedyWordwrap(text, 72))
    println("\nGreedy algorithm - wrapped at 80:")
    println(greedyWordwrap(text, 80))
}
```


{{out}}

```txt

Greedy algorithm - wrapped at 72:
In olden times when wishing still helped one, there lived a king whose
daughters were all beautiful, but the youngest was so beautiful that the
sun itself, which has seen so much, was astonished whenever it shone in
her face.  Close by the king's castle lay a great dark forest, and under
an old lime tree in the forest was a well, and when the day was very
warm, the king's child went out into the forest and sat down by the side
of the cool fountain, and when she was bored she took a golden ball, and
threw it up on high and caught it, and this ball was her favorite
plaything.

Greedy algorithm - wrapped at 80:
In olden times when wishing still helped one, there lived a king whose daughters
were all beautiful, but the youngest was so beautiful that the sun itself, which
has seen so much, was astonished whenever it shone in her face.  Close by the
king's castle lay a great dark forest, and under an old lime tree in the forest
was a well, and when the day was very warm, the king's child went out into the
forest and sat down by the side of the cool fountain, and when she was bored she
took a golden ball, and threw it up on high and caught it, and this ball was her
favorite plaything.

```



## Lasso


```Lasso
define wordwrap(
	text::string,
	row_length::integer = 75
) => {
	return regexp(`(?is)(.{1,` + #row_length + `})(?:$|\W)+`, '$1<br />\n', #text, true) -> replaceall
}

local(text = 'Lorem ipsum dolor sit amet, consectetur adipiscing elit. Mauris consequat ornare lectus, dignissim iaculis libero consequat sed. Proin quis magna in arcu sagittis consequat sed ac risus. Ut a pharetra dui. Phasellus molestie, mauris eget scelerisque laoreet, diam dolor vulputate nulla, in porta sem sem sit amet lacus.')

wordwrap(#text, 40)
'<hr />'
wordwrap(#text)
'<hr />'
wordwrap(#text, 90)
```


->
```txt
Lorem ipsum dolor sit amet, consectetur
adipiscing elit. Mauris consequat ornare
lectus, dignissim iaculis libero
consequat sed. Proin quis magna in arcu
sagittis consequat sed ac risus. Ut a
pharetra dui. Phasellus molestie, mauris
eget scelerisque laoreet, diam dolor
vulputate nulla, in porta sem sem sit
amet lacus.
Lorem ipsum dolor sit amet, consectetur adipiscing elit. Mauris consequat
ornare lectus, dignissim iaculis libero consequat sed. Proin quis magna in
arcu sagittis consequat sed ac risus. Ut a pharetra dui. Phasellus molestie
mauris eget scelerisque laoreet, diam dolor vulputate nulla, in porta sem
sem sit amet lacus.
Lorem ipsum dolor sit amet, consectetur adipiscing elit. Mauris consequat ornare lectus,
dignissim iaculis libero consequat sed. Proin quis magna in arcu sagittis consequat sed ac
risus. Ut a pharetra dui. Phasellus molestie, mauris eget scelerisque laoreet, diam dolor
vulputate nulla, in porta sem sem sit amet lacus.

```



## LFE



###  Naive Implementation


{{trans|Erlang}}


```Lisp

(defun wrap-text (text)
  (wrap-text text 78))

(defun wrap-text (text max-len)
  (string:join
    (make-wrapped-lines
      (string:tokens text " ") max-len)
    "\n"))

(defun make-wrapped-lines
  (((cons word rest) max-len)
    (let ((`#(,_ ,_ ,last-line ,lines) (assemble-lines max-len word rest)))
      (lists:reverse (cons last-line lines)))))

(defun assemble-lines (max-len word rest)
  (lists:foldl
    #'assemble-line/2
    `#(,max-len ,(length word) ,word ())
    rest))

(defun assemble-line
  ((word `#(,max ,line-len ,line ,acc)) (when (> (+ (length word) line-len) max))
    `#(,max ,(length word) ,word ,(cons line acc)))
  ((word `#(,max ,line-len ,line ,acc))
    `#(,max ,(+ line-len 1 (length word)) ,(++ line " " word) ,acc)))

```



###  Regex Implementation



```lisp

(defun make-regex-str (max-len)
  (++ "(.{1," (integer_to_list max-len) "}|\\S{"
      (integer_to_list (+ max-len 1)) ",})(?:\\s[^\\S\\r\\n]*|\\Z)"))

(defun wrap-text (text max-len)
  (let ((find-patt (make-regex-str max-len))
        (replace-patt "\\1\\\n"))
    (re:replace text find-patt replace-patt
                '(global #(return list)))))

```


Usage examples:


```Lisp

> (set test-text (++ "Even today, with proportional fonts and complex layouts, there are still cases where you need to wrap text at a specified column. "
                     "The basic task is to wrap a paragraph of text in a simple way in your language. If there is a way to do this that is built-in, trivial, or "
                     "provided in a standard library, show that. Otherwise implement the minimum length greedy algorithm from Wikipedia.")
> (io:format (wrap-text text 80))

```


```txt

Even today, with proportional fonts and complex layouts, there are still cases
where you need to wrap text at a specified column. The basic task is to wrap a
paragraph of text in a simple way in your language. If there is a way to do this
that is built-in, trivial, or provided in a standard library, show that.
Otherwise implement the minimum length greedy algorithm from Wikipedia.
ok

```


```lisp

> (io:format (wrap-text text 50))

```


```txt

Even today, with proportional fonts and complex
layouts, there are still cases where you need to
wrap text at a specified column. The basic task is
to wrap a paragraph of text in a simple way in
your language. If there is a way to do this that
is built-in, trivial, or provided in a standard
library, show that. Otherwise implement the
minimum length greedy algorithm from Wikipedia.
ok

```



## Lingo

Lingo/Director has 2 visual components for displaying text, text and field members. Both can soft-wrap text directly. In cases where you need a hard-wrapped representation of a text, this could e.g. be implemented like this:
(Note: this solution is meant for proportional fonts and based on actual text rendering. For the more trivial case of non-proportial font word wrapping, just pass a non-proportinal font like e.g. Courier in the 'style' argument)

```lingo
-- in some movie script

----------------------------------------
-- Wraps specified text into lines of specified width (in px), returns lines as list of strings
-- @param {string} str
-- @param {integer} pixelWidth
-- @param {propList} [style]
-- @return {list}
----------------------------------------
on hardWrapText (str, pixelWidth, style)
  if voidP(style) then style = [:]
  lines = []

  -- create a new field member
  m = new(#field)
  m.text = str
  m.rect = rect(0,0,pixelWidth,0)

  -- assign style props (if not specified, defaults are used)
  repeat with i = 1 to style.count
    m.setProp(style.getPropAt(i), style[i])
  end repeat

  -- create an invisible temporary sprite
  s = channel(1).makeScriptedSprite(m)
  s.loc = point(0,0)
  s.visible = false
  _movie.updateStage()

  -- get the wrapped lines
  charPos = 0
  repeat with y = 0 to s.height-1
    n = s.pointToChar(point(pixelWidth-1, y))
    if n<>charPos then
      lines.add(str.char[charPos+1..n])
      charPos = n
    end if
  end repeat

  channel(1).removeScriptedSprite()
  return lines
end
```

Usage:

```lingo
str = "Lorem ipsum dolor sit amet, consectetur adipisici elit, sed "&\
"eiusmod tempor incidunt ut labore et dolore magna aliqua. Ut enim ad minim "&\
"veniam, quis nostrud exercitation ullamco laboris nisi ut aliquid ex ea commodi "&\
"consequat. Quis aute iure reprehenderit in voluptate velit esse cillum dolore eu "&\
"fugiat nulla pariatur. Excepteur sint obcaecat cupiditat non proident, sunt in "&\
"culpa qui officia deserunt mollit anim id est laborum."

lines = hardWrapText(str, 320, [#font: "Arial", #fontSize:24])

repeat with l in lines
  put l
end repeat
```

{{Out}}

```txt

-- "Lorem ipsum dolor sit amet, "
-- "consectetur adipisici elit, sed "
-- "eiusmod tempor incidunt ut "
-- "labore et dolore magna "
-- "aliqua. Ut enim ad minim "
-- "veniam, quis nostrud "
-- "exercitation ullamco laboris "
-- "nisi ut aliquid ex ea commodi "
-- "consequat. Quis aute iure "
-- "reprehenderit in voluptate "
-- "velit esse cillum dolore eu "
-- "fugiat nulla pariatur. "
-- "Excepteur sint obcaecat "
-- "cupiditat non proident, sunt "
-- "in culpa qui officia deserunt "
-- "mollit anim id est laborum."

```



## Lua



```lua
function splittokens(s)
    local res = {}
    for w in s:gmatch("%S+") do
        res[#res+1] = w
    end
    return res
end

function textwrap(text, linewidth)
    if not linewidth then
        linewidth = 75
    end

    local spaceleft = linewidth
    local res = {}
    local line = {}

    for _, word in ipairs(splittokens(text)) do
        if #word + 1 > spaceleft then
            table.insert(res, table.concat(line, ' '))
            line = {word}
            spaceleft = linewidth - #word
        else
            table.insert(line, word)
            spaceleft = spaceleft - (#word + 1)
        end
    end

    table.insert(res, table.concat(line, ' '))
    return table.concat(res, '\n')
end

local example1 = [[
Even today, with proportional fonts and complex layouts,
there are still cases where you need to wrap text at a
specified column. The basic task is to wrap a paragraph
of text in a simple way in your language. If there is a
way to do this that is built-in, trivial, or provided in
a standard library, show that. Otherwise implement the
minimum length greedy algorithm from Wikipedia.
]]

print(textwrap(example1))
print()
print(textwrap(example1, 60))
```


{{out}}

```txt
Even today, with proportional fonts and complex layouts, there are still
cases where you need to wrap text at a specified column. The basic task is
to wrap a paragraph of text in a simple way in your language. If there is a
way to do this that is built-in, trivial, or provided in a standard
library, show that. Otherwise implement the minimum length greedy algorithm
from Wikipedia.

Even today, with proportional fonts and complex
layouts, there are still cases where you need to wrap
text at a specified column. The basic task is to wrap a
paragraph of text in a simple way in your language. If
there is a way to do this that is built-in, trivial, or
provided in a standard library, show that. Otherwise
implement the minimum length greedy algorithm from
Wikipedia.

```


## M2000 Interpreter

M2000 Environment (where actual a M2000 script executed) provide the means to print proportional text, in console, in layer in console form, in user windows layers, and in printer paper. To render proportional text we can use:

Print statement with styles from 4 to 10 (so 0 to 3 are non proportional, by default we use 0). Styles applied with $() print's internal function. So a Print $(4),  set  from current position the proportional style. This has word wrap, but stop at wrapping.  Print statement not used for documents (or strings with paragraphs). We can print text with diacritics.

Report statement is the default renderer for text. Not only render text, but can be used to calculate the number of lines before actual render text. Report also prints in characters row, and can be used with Double statement to change temporary to Double height font. Also use Italic and Bold state. Also can print text with diacritics as Print statement can. Lead space from paragraph stay there, and spaces can be get bigger or smaller from original size, but not to small. Because we can give the width of the output (in character units, or in twips units), we can find easy the bounding box. Report statement when rendering to anything except console, stop rendering, after scrolling the 3/4 of layer height (the lower part which can scroll, maybe all the height, depends on setting split screen function), waiting for user to press space or a mouse button.

Legend Statement use Font and font size (Mode in M2000) for each call, and print text in graphic position, using alignment but not word warp, and we can use rotation. Functions size.x() and size.y() can return then dimension of the bounding box,

We can use controls on forms to render text, and EditBox has word wrap, and a state when no edit allowed, we can view/scroll text only.

This example use a text in a$, where first remove line breaks. Then set font to Tahoma, and  console size to 80 characters by 50 lines, and display the current font size (automatic environment insert line space between text rows). Also display state for Bold and Italic. Then we get the text from 9th row, as proportional text, italic, centered with a width for lines as 60 characters width. Then ask for a keypress.

At the second phase, in a loop, render text twice, the first as is, the second changing color in each line. To perform that, it render once without displaying, get from ReportLines the number of lines, and then execute a for loop and do a partial display, but each time rendering starts from first line, and display from the line we choose, for lines we choose (first choose lines and next using Line choose the first line). Until now we didn't get somewhere in memory the actual lines, only in layer, displayed.

At the third phase, we can get in a document variable all lines (where actual made each line break). We can find the length in twips for each line, but this is with spaces with standard width. Report use spaces smaller or bigger than normal space, and do a distribution of pixels that can't be fit in all spaces chunks, mostly at the right chunks. So even using a non proportional font, we get from Report at rendering proportional spacings.

The inner editor of M2000 environment also works with Word Warp and can change it with F1 any time. Also EditBox can change with F1 word wrap, in user forms (windows).

All of these statements not handle tab character (9) as tab. Editor change tab with spaces, and Print/report/Legend prints tab as a square character (as for 9.4 version of M2000 Environment and Interpreter).


```M2000 Interpreter

Module Checkit {
      \\ leading space from begin of paragraph stay as is
      a$={     In olden times when wishing still helped one, there
      lived a king whose daughters were all beautiful, but the youngest
      was so beautiful that the sun itself, which has seen so
      much, was astonished whenever it shone in her face.
      }
      const crlf$=chr$(13)+chr$(10)
      a$=replace$(crlf$, " ", a$)+crlf$
      const justify=0
      const flushright=1
      const centered=2
      const flushleft=3
      \\ set layer font
      Font "Tahoma"
      Form 80, 50
      Print "Font:";Fontname$
      Print "Font size:";Mode;"pt"
      Print "Bold:";Bold
      Print "Italic:";Italic
      \\ set left margin for Report
      Cursor 10, 8 ' pos 10 row 8 (11 9 - it is 0 based)
      m=Italic
      Italic 1
      Report centered, trim$(A$), Width-10-10
      Italic m

      Print  @(0,79),"Press any key";
      wait$=key$
      Refresh 5000
      charwidth=scale.x div width
      For i=2000 to scale.x-charwidth step 150
            \\ clear screen with 14pt fonts

            Mode 12.75
            \\ by default use justify, word wrap
            Report a$, i
            \\  we can calculate only using a negative parameter
            Report a$, i, -10000
            k=ReportLines
            \\ print any line in differnet color
            Dim a(2)
            a(0)=11, 15   ' 0 to 15 are vb6 colors, we can use html colors #aabbcc, #ff2211
            For j=1 to k {
                Pen a(j mod 2) {
                        Report a$, i, 1 line j
                  }
            }
            Refresh 5000
            wait$=key$
      Next i

      Report a$, scale.x/2, -1000
      k=ReportLines
      Document  Doc$
      Report a$, scale.x/2, k as Doc$
      \\ Print document without expanding spaces
      Print $(4), ' 4=proportional printing using columns, on line online, word wrap, expand to fit in columns
      For i=1 to k {
      Print "*";Paragraph$(Doc$, i);"*"
      }
      Print $(0),   ' restore to non proportional printing
      For i=1 to k {
            Print i, size.x(Paragraph$(Doc$, i), Fontname$, Mode), size.y(Paragraph$(Doc$, i), Fontname$, Mode)
      }
      \\ scale.x unit in twips
      Report a$, scale.x/2
      \\ width unit in characters
      Report a$, width/2
      Print @(width div 2),
      Report flushright, a$, width/2
      Cursor 0, Row
      I=Italic
      Double
      Italic 1
      Report centered, a$
      Italic I
      Double
      Normal
}
Checkit

```



## Mathematica


```Mathematica
string="In olden times when wishing still helped one, there lived a king whose daughters were all beautiful, but the youngest was so beautiful that the sun itself, which has seen so much, was astonished whenever it shone in her face.  Close by the king's castle lay a great dark forest, and under an old lime-tree in the forest was a well, and when the day was very warm, the king's child went out into the forest and sat down by the side of the cool fountain, and when she was bored she took a golden ball, and threw it up on high and caught it, and this ball was her favorite plaything.";
wordWrap[textWidth_,spaceWidth_,string_]:=Module[{start,spaceLeft,masterString},
 spaceLeft=textWidth;
 start=1;
 masterString={};
 Do[
  If[i+1>Length@StringSplit@string
   ,
   p=StringSplit[string][[start;;i]];
   AppendTo[masterString,{StringJoin@@Riffle[p,StringJoin@ConstantArray[" ",spaceWidth]]}]
   ,
   If[StringLength[StringSplit@string][[i+1]]+spaceWidth>spaceLeft
     ,
     spaceLeft=textWidth-StringLength[StringSplit@string][[i]];
     start=i;
     AppendTo[masterString,{StringJoin@@Riffle[p,StringJoin@ConstantArray[" ",spaceWidth]]}]
     ,
     spaceLeft-=StringLength[StringSplit@string][[i]];
     spaceLeft-=spaceWidth;
     p=StringSplit[string][[start;;i]]
    ]
   ]
  ,
  {i,1,Length@StringSplit@string}
 ];
 StringJoin@@Riffle[masterString,"\n"]
];
```

{{out}} for width 72 and 80:
<lang>wordWrap[72, 1, string]
wordWrap[80, 1, string]
```

{{out}}

```txt
In olden times when wishing still helped one, there lived a king
whose daughters were all beautiful, but the youngest was so beautiful
that the sun itself, which has seen so much, was astonished whenever
it shone in her face. Close by the king's castle lay a great dark forest,
and under an old lime-tree in the forest was a well, and when the day
was very warm, the king's child went out into the forest and sat down
by the side of the cool fountain, and when she was bored she took a
golden ball, and threw it up on high and caught it, and this ball was
her favorite plaything.

In olden times when wishing still helped one, there lived a king whose daughters
were all beautiful, but the youngest was so beautiful that the sun itself, which
has seen so much, was astonished whenever it shone in her face. Close by the
king's castle lay a great dark forest, and under an old lime-tree in the forest
was a well, and when the day was very warm, the king's child went out into
the forest and sat down by the side of the cool fountain, and when she was bored
she took a golden ball, and threw it up on high and caught it, and this ball was
her favorite plaything.
```



## MiniScript


```MiniScript
str = "one two three four five six seven eight nine ten eleven!"
width = 15
words = str.split
pos = 0
line = ""
for word in words
    pos = pos + word.len + 1
    if pos <= width then
        line = line + word + " "
    else
        print line[:-1]
        line = word + " "
        pos = word.len
    end if
end for
print line[:-1]
```

{{out}}

```txt

one two three
four five six
seven eight
nine ten
eleven!

```




## NetRexx


### version 1


```NetRexx
/* NetRexx */
options replace format comments java crossref symbols

runSample(arg)
return

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
/*
   @see http://en.wikipedia.org/wiki/Word_wrap#Minimum_length

   SpaceLeft := LineWidth
   for each Word in Text
       if (Width(Word) + SpaceWidth) > SpaceLeft
           insert line break before Word in Text
           SpaceLeft := LineWidth - Width(Word)
       else
           SpaceLeft := SpaceLeft - (Width(Word) + SpaceWidth)
 */
method wordWrap(text, lineWidth = 80) public static
  if lineWidth > 0 then do
    NL = '\n'
    SP = ' '
    wrapped = ''
    spaceWidth = SP.length()
    spaceLeft = lineWidth
    loop w_ = 1 to text.words()
      nextWord = text.word(w_)
      if (nextWord.length() + spaceWidth) > spaceLeft then do
        wrapped = wrapped || NL || nextWord
        spaceLeft = lineWidth - nextWord.length()
        end
      else do
        wrapped = wrapped || SP || nextWord
        spaceLeft = spaceLeft - (nextWord.length() + spaceWidth)
        end
      end w_
    end
  else do
    wrapped = text
    end

  return wrapped.strip() -- clean w/s from front & back

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
method runSample(arg) public static
  parse arg lineLen .
  if lineLen  = '' then lineLen = 80
  text = getText()
  wrappedLines = wordWrap(text, lineLen)
  say 'Wrapping text at' lineLen 'characters'
  say ('....+....|'.copies((lineLen + 9) % 10)).left(lineLen)
  say wrappedLines

  return

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
method getText() public static

  -- ....+....|....+....|....+....|....+....|....+....|....+....|
  speech01 = -
    'She should have died hereafter;' -
    'There would have been a time for such a word.' -
    'Tomorrow, and tomorrow, and tomorrow,' -
    'Creeps in this petty pace from day to day,' -
    'To the last syllable of recorded time;' -
    'And all our yesterdays have lighted fools' -
    'The way to dusty death. Out, out, brief candle!' -
    'Life''s but a walking shadow, a poor player' -
    'That struts and frets his hour upon the stage' -
    'And then is heard no more. It is a tale' -
    'Told by an idiot, full of sound and fury' -
    'Signifying nothing.' -
    '' -
    '—-Macbeth (Act 5, Scene 5, lines 17-28)' -
    ''
  return speech01

```

{{out}}
<pre style="height:15em; overflow:scroll">
Wrapping text at 64 characters
....+....|....+....|....+....|....+....|....+....|....+....|....
She should have died hereafter; There would have been a time
for such a word. Tomorrow, and tomorrow, and tomorrow, Creeps in
this petty pace from day to day, To the last syllable of
recorded time; And all our yesterdays have lighted fools The way
to dusty death. Out, out, brief candle! Life's but a walking
shadow, a poor player That struts and frets his hour upon the
stage And then is heard no more. It is a tale Told by an idiot,
full of sound and fury Signifying nothing. —-Macbeth (Act 5,
Scene 5, lines 17-28)

Wrapping text at 132 characters
....+....|....+....|....+....|....+....|....+....|....+....|....+....|....+....|....+....|....+....|....+....|....+....|....+....|..
She should have died hereafter; There would have been a time for such a word. Tomorrow, and tomorrow, and tomorrow, Creeps in this
petty pace from day to day, To the last syllable of recorded time; And all our yesterdays have lighted fools The way to dusty death.
Out, out, brief candle! Life's but a walking shadow, a poor player That struts and frets his hour upon the stage And then is heard
no more. It is a tale Told by an idiot, full of sound and fury Signifying nothing. —-Macbeth (Act 5, Scene 5, lines 17-28)
```



### version 2


```NetRexx
/* NetRexx ************************************************************
* 23.08.2013 Walter Pachl  translated from REXX version 2
**********************************************************************/
options replace format comments java crossref symbols

runSample(arg)

method runSample(arg) public static
  s='She should have died hereafter;' -
  'There would have been a time for such a word.' -
  'Tomorrow, and tomorrow, and tomorrow, and so on'
  w=72
  Say s.length
  loop while s>' '
    Loop i=w+1 to 1 by -1
      If s.substr(i,1)='' Then
        Leave
      End
    If i=0 Then
      p=s.pos(' ')
    Else
      p=i
    say s.left(p)
    s=s.substr(p+1)
    End
  If s>'' Then
    say s
  return
```



## Nim


```nim
import strutils

let txt = "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Donec a diam lectus. Sed sit amet ipsum mauris. Maecenas congue ligula ac quam viverra nec consectetur ante hendrerit. Donec et mollis dolor. Praesent et diam eget libero egestas mattis sit amet vitae augue. Nam tincidunt congue enim, ut porta lorem lacinia consectetur."
echo wordWrap(txt)
echo ""
echo wordWrap(txt, 45)

```

{{out}}

```txt
Lorem ipsum dolor sit amet, consectetur adipiscing elit. Donec a diam lectus.
Sed sit amet ipsum mauris. Maecenas congue ligula ac quam viverra nec
consectetur ante hendrerit. Donec et mollis dolor. Praesent et diam eget libero
egestas mattis sit amet vitae augue. Nam tincidunt congue enim, ut porta lorem
lacinia consectetur.

Lorem ipsum dolor sit amet, consectetur
adipiscing elit. Donec a diam lectus. Sed sit
amet ipsum mauris. Maecenas congue ligula ac
quam viverra nec consectetur ante hendrerit.
Donec et mollis dolor. Praesent et diam eget
libero egestas mattis sit amet vitae augue.
Nam tincidunt congue enim, ut porta lorem
lacinia consectetur.
```



## OCaml



```ocaml
#load "str.cma"

let txt = "In olden times when wishing still helped one, there lived
a king whose daughters were all beautiful, but the youngest was so
beautiful that the sun itself, which has seen so much, was astonished
whenever it shone in her face.  Close by the king's castle lay a great
dark forest, and under an old lime-tree in the forest was a well, and
when the day was very warm, the king's child went out into the forest
and sat down by the side of the cool fountain, and when she was bored
she took a golden ball, and threw it up on high and caught it, and
this ball was her favorite plaything."

let () =
  let line_width = int_of_string Sys.argv.(1) in
  let words = Str.split (Str.regexp "[ \n]+") txt in
  let buf = Buffer.create 10 in
  let _ =
    List.fold_left (fun (width, sep) word ->
      let wlen = String.length word in
      let len = width + wlen + 1 in
      if len > line_width then
      begin
        Buffer.add_char buf '\n';
        Buffer.add_string buf word;
        (wlen, " ")
      end else begin
        Buffer.add_string buf sep;
        Buffer.add_string buf word;
        (len, " ")
      end
    ) (0, "") words
  in
  print_endline (Buffer.contents buf)
```


Testing:

```txt
$ ocaml word_wrap.ml 80 | wc -L
79
$ ocaml word_wrap.ml 72 | wc -L
72
$ ocaml word_wrap.ml 50 | wc -L
50
```



## Ol


```scheme

(define (get-one-word start)
(let loop ((chars #null) (end start))
   (let ((char (car end)))
      (if (has? (list #\space #\newline) char)
         (values (reverse chars) (force (cdr end)))
         (loop (cons char chars) (force (cdr end)))))))

(define (get-all-words string)
(let loop ((words #null) (start (str-iter string)))
   (let* ((word next (get-one-word start)))
      (if (null? next)
         (reverse words)
         (loop (cons (runes->string word) words) next)))))

(define (get-one-line words n)
(let loop ((line #null) (words words) (i 0))
   (let*((word (car words))
         (len (string-length word)))
      (if (null? (cdr words))
         (values (reverse (cons word line)) #null)
         (if (> (+ i len) n 1)
            (values (reverse line) words)
            (loop (cons word line) (cdr words) (+ i len 1)))))))

(define (get-all-lines words n)
(let loop ((lines #null) (words words))
   (let* ((line words (get-one-line words n)))
      (if (null? words)
         (reverse (cons line lines))
         (loop (cons line lines) words)))))

(define (hyphenation width string)
(let*((words (get-all-words string))
      (lines (get-all-lines words width)))
   lines))

```


{{out}}

```txt

; <== (print "{0---------1+++++++++2---------3+++++++++4---------5+++++++++6---------7+++++++++}")
; <== (for-each print (hyphenation 80 "In olden times when wishing still helped one, there lived a king whose daughters were all beautiful, but the youngest was so beautiful that the sun itself, which has seen so much, was astonished whenever it shone in her face.  Close by the king's castle lay a great dark forest, and under an old lime-tree in the forest was a well, and when the day was very warm, the king's child went out into the forest and sat down by the side of the cool fountain, and when she was bored she took a golden ball, and threw it up on high and caught it, and this ball was her favorite plaything. "))

; ==> {0---------1+++++++++2---------3+++++++++4---------5+++++++++6---------7+++++++++}
; ==> (In olden times when wishing still helped one, there lived a king whose daughters)
; ==> (were all beautiful, but the youngest was so beautiful that the sun itself, which)
; ==> (has seen so much, was astonished whenever it shone in her face.  Close by the)
; ==> (king's castle lay a great dark forest, and under an old lime-tree in the forest)
; ==> (was a well, and when the day was very warm, the king's child went out into the)
; ==> (forest and sat down by the side of the cool fountain, and when she was bored she)
; ==> (took a golden ball, and threw it up on high and caught it, and this ball was her)
; ==> (favorite)

```



## PARI/GP


```parigp
wrap(s,len)={
  my(t="",cur);
  s=Vec(s);
  for(i=1,#s,
    if(s[i]==" ",
      if(cur>#t,
        print1(" "t);
        cur-=#t+1
      ,
        print1("\n"t);
        cur=len-#t
      );
      t=""
    ,
      t=concat(t,s[i])
    )
  );
  if(cur>#t,
    print1(" "t)
  ,
    print1("\n"t)
  )
};
King="And so let freedom ring from the prodigious hilltops of New Hampshire; let freedom ring from the mighty mountains of New York; let freedom ring from the heightening Alleghenies of Pennsylvania; let freedom ring from the snow-capped Rockies of Colorado; let freedom ring from the curvaceous slopes of California. But not only that: let freedom ring from Stone Mountain of Georgia; let freedom ring from Lookout Mountain of Tennessee; let freedom ring from every hill and molehill of Mississippi. From every mountainside, let freedom ring.";
wrap(King, 75)
wrap(King, 50)
```


{{out}}

```txt

And so let freedom ring from the prodigious hilltops of New Hampshire; let
freedom ring from the mighty mountains of New York; let freedom ring from
the heightening Alleghenies of Pennsylvania; let freedom ring from the
snow-capped Rockies of Colorado; let freedom ring from the curvaceous
slopes of California. But not only that: let freedom ring from Stone
Mountain of Georgia; let freedom ring from Lookout Mountain of Tennessee;
let freedom ring from every hill and molehill of Mississippi. From every
mountainside, let freedom ring.

And so let freedom ring from the prodigious
hilltops of New Hampshire; let freedom ring from
the mighty mountains of New York; let freedom ring
from the heightening Alleghenies of Pennsylvania;
let freedom ring from the snow-capped Rockies of
Colorado; let freedom ring from the curvaceous
slopes of California. But not only that: let
freedom ring from Stone Mountain of Georgia; let
freedom ring from Lookout Mountain of Tennessee;
let freedom ring from every hill and molehill of
Mississippi. From every mountainside, let freedom
ring.
```



## Perl

Regex. Also showing degraded behavior on very long words:

```perl
my $s = "In olden times when wishing still helped one, there lived a king
whose daughters were all beautiful, but the youngest was so beautiful
that the sun itself, which has seen so much, was astonished whenever
it shone in her face.  Close-by-the-king's-castle-lay-a-great-dark
forest, and under an old lime-tree in the forest was a well, and when
the day was very warm, the king's child went out into the forest and
sat down by the side of the cool fountain, and when she was bored she
took a golden ball, and threw it up on high and caught it, and this
ball was her favorite plaything.";

$s =~ s/\b\s+/ /g;
$s =~ s/\s*$/\n\n/;

my $_ = $s;
s/\s*(.{1,66})\s/$1\n/g, print;

$_ = $s;
s/\s*(.{1,25})\s/$1\n/g, print;
```


## Perl 6


```perl6
my $s = "In olden times when wishing still helped one, there lived a king
whose daughters were all beautiful, but the youngest was so beautiful
that the sun itself, which has seen so much, was astonished whenever
it shone in her face.  Close-by-the-king's-castle-lay-a-great-dark
forest, and under an old lime-tree in the forest was a well, and when
the day was very warm, the king's child went out into the forest and
sat down by the side of the cool fountain, and when she was bored she
took a golden ball, and threw it up on high and caught it, and this
ball was her favorite plaything.";

$s ~~ s:g/»\s+/ /;
$s ~~ s/\s*$/\n\n/;

say $s.subst(/ \s* (. ** 1..66) \s /, -> $/ { "$0\n" }, :g);
say $s.subst(/ \s* (. ** 1..25) \s /, -> $/ { "$0\n" }, :g);
```



## Phix


```Phix
string s = substitute("""In olden times when wishing still helped one, there lived a king
whose daughters were all beautiful, but the youngest was so beautiful that the sun itself,
which has seen so much, was astonished whenever it shone in her face. Close by the king's
castle lay a great dark forest, and under an old lime-tree in the forest was a well, and
when the day was very warm, the king's child went out into the forest and sat down by the
side of the cool fountain, and when she was bored she took a golden ball, and threw it up
on high and caught it, and this ball was her favorite plaything.""","\n"," ")

procedure word_wrap(string s, integer maxwidth)
    sequence words = split(s)
    string line = words[1]
    for i=2 to length(words) do
        string word = words[i]
        if length(line)+length(word)+1>maxwidth then
            puts(1,line&"\n")
            line = word
        else
            line &= " "&word
        end if
    end for
    puts(1,line&"\n")
end procedure

word_wrap(s,72)
word_wrap(s,80)
```

{{Out}}

```txt

In olden times when wishing still helped one, there lived a king whose
daughters were all beautiful, but the youngest was so beautiful that the
sun itself, which has seen so much, was astonished whenever it shone in
her face. Close by the king's castle lay a great dark forest, and under
an old lime-tree in the forest was a well, and when the day was very
warm, the king's child went out into the forest and sat down by the side
of the cool fountain, and when she was bored she took a golden ball, and
threw it up on high and caught it, and this ball was her favorite
plaything.
In olden times when wishing still helped one, there lived a king whose daughters
were all beautiful, but the youngest was so beautiful that the sun itself, which
has seen so much, was astonished whenever it shone in her face. Close by the
king's castle lay a great dark forest, and under an old lime-tree in the forest
was a well, and when the day was very warm, the king's child went out into the
forest and sat down by the side of the cool fountain, and when she was bored she
took a golden ball, and threw it up on high and caught it, and this ball was her
favorite plaything.

```



## PHP


```php
<?php

$text = <<<ENDTXT
If there's anything you need
All you have to do is say
You know you satisfy everything in me
We shouldn't waste a single day

So don't stop me falling
It's destiny calling
A power I just can't deny
It's never changing
Can't you hear me, I'm saying
I want you for the rest of my life

Together forever and never to part
Together forever we two
And don't you know
I would move heaven and earth
To be together forever with you
ENDTXT;

// remove preexisting line breaks
$text = str_replace( PHP_EOL,  " " , $text );

echo wordwrap( $text, 20 ), PHP_EOL, PHP_EOL;

echo wordwrap( $text, 40 ), PHP_EOL, PHP_EOL;

echo wordwrap( $text, 80 ), PHP_EOL, PHP_EOL;

```

{{Out}}
<pre style="font-size:84%;height:55ex">
If there's anything
you need All you
have to do is say
You know you satisfy
everything in me We
shouldn't waste a
single day  So don't
stop me falling It's
destiny calling A
power I just can't
deny It's never
changing Can't you
hear me, I'm saying
I want you for the
rest of my life
Together forever and
never to part
Together forever we
two And don't you
know I would move
heaven and earth To
be together forever
with you

If there's anything you need All you
have to do is say You know you satisfy
everything in me We shouldn't waste a
single day  So don't stop me falling
It's destiny calling A power I just
can't deny It's never changing Can't you
hear me, I'm saying I want you for the
rest of my life  Together forever and
never to part Together forever we two
And don't you know I would move heaven
and earth To be together forever with
you

If there's anything you need All you have to do is say You know you satisfy
everything in me We shouldn't waste a single day  So don't stop me falling It's
destiny calling A power I just can't deny It's never changing Can't you hear me,
I'm saying I want you for the rest of my life  Together forever and never to
part Together forever we two And don't you know I would move heaven and earth To
be together forever with you

```



## PicoLisp

'[http://software-lab.de/doc/refW.html#wrap wrap]' is a built-in.

```PicoLisp
: (prinl (wrap 12 (chop "The quick brown fox jumps over the lazy dog")))
The quick
brown fox
jumps over
the lazy dog
-> "The quick^Jbrown fox^Jjumps over^Jthe lazy dog"
```



## PL/I


```pli
*process source attributes xref or(!);
 ww: proc Options(main);
 /*********************************************************************
 * 21.08-2013 Walter Pachl  derived from REXX version 2
 *********************************************************************/
 Dcl in  record input;
 Dcl out record output;
 On Endfile(in) z=' ';
 Dcl z char(32767) Var;
 Dcl s char(32767) Var Init('');
 dcl o Char(200) Var;
 Dcl (i,w,p) Bin Fixed(31) Init(0);
 w=72;
 Read File(in) Into(z);
 s=z;
 Do Until(s='');
   Do i=w+1 to 1 by -1;
     If substr(s,i,1)='' Then Leave;
     End;
   If i=0 Then
     p=index(s,' ');
   Else
     p=i;
   o=left(s,p);
   Write file(out) From(o);
   s=substr(s,p+1);
   If length(s)<200 Then Do;
     Read File(in) Into(z);
     s=s!!z;
     End;
   End;
 End;
```

Test result using this:

```txt

/* REXX */
Call time 'R'
'set dd:in=h:\long2.txt,recsize(30000)' /* 1000036 characters with random length words */
'set dd:out=h:\longp.72,recsize(300)'
'ww'
Say time('E')

```

{{out}}

```txt

A nnnnnnnnnnnnnn ooooooooooooooo nnnnnnnnnnnnnn
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
iiiiiiiii LLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLL
etc.

```



## PowerShell

Basic word wrap.

```powershell
function wrap{
$divide=$args[0] -split " "
$width=$args[1]
$spaceleft=$width

foreach($word in $divide){
	if($word.length+1 -gt $spaceleft){
		$output+="`n$word "
		$spaceleft=$width-($word.length+1)
	} else {
		$output+="$word "
		$spaceleft-=$word.length+1
	}
}

return "$output`n"
}

### The Main Thing...

$paragraph="Lorem ipsum dolor sit amet, consectetur adipiscing elit. Donec a diam lectus. Sed sit amet ipsum mauris. Maecenas congue ligula ac quam viverra nec consectetur ante hendrerit. Donec et mollis dolor. Praesent et diam eget libero egestas mattis sit amet vitae augue. Nam tincidunt congue enim, ut porta lorem lacinia consectetur."

"`nLine width:30`n"
wrap $paragraph 30
"
### ===================================================
"
"Line width:100`n"
wrap $paragraph 100

### End script
```

{{Out}}

```txt

Line width:30

Lorem ipsum dolor sit amet,
consectetur adipiscing elit.
Donec a diam lectus. Sed sit
amet ipsum mauris. Maecenas
congue ligula ac quam viverra
nec consectetur ante
hendrerit. Donec et mollis
dolor. Praesent et diam eget
libero egestas mattis sit
amet vitae augue. Nam
tincidunt congue enim, ut
porta lorem lacinia
consectetur.


### ===================================================

Line width:100

Lorem ipsum dolor sit amet, consectetur adipiscing elit. Donec a diam lectus. Sed sit amet ipsum
mauris. Maecenas congue ligula ac quam viverra nec consectetur ante hendrerit. Donec et mollis
dolor. Praesent et diam eget libero egestas mattis sit amet vitae augue. Nam tincidunt congue enim,
ut porta lorem lacinia consectetur.

```



### Pipeline Version

Slightly modified the previous to become the guts of this version.  Now there is a default (80 characters) and a lower and upper limit for the -Width parameter.  An unlimited number of strings may be passed to the helper function, New-WordWrap, through the pipeline.

```PowerShell

function Out-WordWrap
{
    [CmdletBinding()]
    [OutputType([string])]
    Param
    (
        [Parameter(Mandatory=$true,
                   ValueFromPipeline=$true,
                   Position=0)]
        [string]
        $Text,

        [Parameter(Mandatory=$false,
                   Position=1)]
        [ValidateRange(16,160)]
        [int]
        $Width = 80
    )

    Begin
    {
        function New-WordWrap ([string]$Text, [int]$Width)
        {
            [string[]]$words = $Text.Split()
            [string]$output  = ""
            [int]$remaining  = $Width

            foreach ($word in $words)
            {
	        if($word.Length + 1 -gt $remaining)
                {
		    $output    += "`n$word "
		    $remaining  = $Width - ($word.Length + 1)
	        }
                else
                {
		    $output    += "$word "
		    $remaining -= $word.Length + 1
	        }
            }

            return "$output`n"
        }
    }
    Process
    {
        foreach ($paragraph in $Text)
        {
            New-WordWrap -Text $paragraph -Width $Width
        }
    }
}

```

Grab some data and send it down the pipeline:

```PowerShell

[string[]]$paragraphs = "Rebum everti delicata an vel, quo ut temporibus interpretaris, mea debet mnesarchum disputando ad. Id has dolorum contentiones, mel ea noster adipisci. Id persius appareat eos, aeque dolorum fastidii eam in. Partem assentior contentiones ut mea. Cu augue facilis fabellas cum, vix eu sanctus denique imperdiet, appareat percipit qui ex.",
                        "Nihil discere phaedrum at duo, no eum adhuc autem error. Quo aliquam delicata contentiones et, in sed ferri legimus sententiae, nihil solet docendi id eum. Ius ut meliore vulputate adipiscing, sea cu virtute praesent. Euripidis instructior est eu. Veri cotidieque ex vel, aliquam eruditi nusquam sea ne, eu wisi ubique ullamcorper est. Qui doctus epicuri ei. Cum esse detracto concludaturque ea, veri erant per ad, vide ancillae principes ius id.",
                        "Id disputando signiferumque nam, mei illud aeterno ut. Facilisis evertitur mei at. Qui in wisi fugit, eirmod comprehensam duo ei. Ea mel omnium nusquam, causae consequat appellantur per te.",
                        "Denique deseruisse ea his. Mundi scripta adolescens te ius, cum error persius cotidieque cu. Nobis apeirian ad his. Ius omnes gloriatur at, has eu tamquam inciderint, ubique commodo pro ad. Ex veri ceteros quo, duo an labores adolescens. Sed id quod verterem prodesset, magna eloquentiam ea eum.",
                        "Qui sanctus oportere quaerendum ex, usu vivendo accusamus posidonium an. Quo cu graece reprimique. Ea cum purto quando referrentur, tritani perfecto ne sit. Ne sit iusto ludus, ea ius eruditi dissentiunt, fabellas disputando eu vix. Te vim eripuit debitis tincidunt, in vim nonumes consetetur.",
                        "Affert exerci aperiri pri ea. Ut dicant essent corrumpit sit. Sea saepe nullam referrentur ut, vis dolores perfecto cu. At nam inimicus evertitur vulputate.",
                        "Dolor volutpat praesent vix ne, at soluta oblique admodum eum. Duis adipisci mea in, nam ut tota choro theophrastus. Ex scripta definitiones mei, augue doctus ne sed, munere posidonium eum id. Ad graeco audire per.",
                        "Sale salutatus et mei, mea elit illud adipiscing ei, cum ea sumo melius forensibus. Eu inani iusto oporteat eum, ei vix iisque saperet detraxit. Fabulas perpetua similique eam ne, noster corpora dissentiet qui ex, et qui integre graecis. Eripuit nonumes deterruisset an pro, ei ferri similique cum. Odio dolores inciderint ei vim, an est dolorum delicata temporibus, eu mea quis accumsan. Vel stet affert option at.",
                        "In gubergren voluptaria reprimique pro, option fuisset id est. Rebum delicata ad sea, ex vidit errem vis, mei at duis dicam sensibus. Nibh debet iudicabit has no, vim te dicit libris possim. Debet viderer consequuntur ea pro. Ex dicat iriure scripta pro.",
                        "An dicat diceret eligendi duo. Est cu equidem deterruisset, usu ad regione equidem, vim amet vero possim ex. Theophrastus conclusionemque ad quo, inimicus deseruisse voluptatibus eum et. Duis delectus mandamus an mei, usu timeam nostrum suscipiantur id."

$paragraphs | Out-WordWrap -Width 100

```

{{Out}}

```txt

Rebum everti delicata an vel, quo ut temporibus interpretaris, mea debet mnesarchum disputando ad.
Id has dolorum contentiones, mel ea noster adipisci. Id persius appareat eos, aeque dolorum
fastidii eam in. Partem assentior contentiones ut mea. Cu augue facilis fabellas cum, vix eu
sanctus denique imperdiet, appareat percipit qui ex.

Nihil discere phaedrum at duo, no eum adhuc autem error. Quo aliquam delicata contentiones et, in
sed ferri legimus sententiae, nihil solet docendi id eum. Ius ut meliore vulputate adipiscing, sea
cu virtute praesent. Euripidis instructior est eu. Veri cotidieque ex vel, aliquam eruditi nusquam
sea ne, eu wisi ubique ullamcorper est. Qui doctus epicuri ei. Cum esse detracto concludaturque ea,
veri erant per ad, vide ancillae principes ius id.

Id disputando signiferumque nam, mei illud aeterno ut. Facilisis evertitur mei at. Qui in wisi
fugit, eirmod comprehensam duo ei. Ea mel omnium nusquam, causae consequat appellantur per te.

Denique deseruisse ea his. Mundi scripta adolescens te ius, cum error persius cotidieque cu. Nobis
apeirian ad his. Ius omnes gloriatur at, has eu tamquam inciderint, ubique commodo pro ad. Ex veri
ceteros quo, duo an labores adolescens. Sed id quod verterem prodesset, magna eloquentiam ea eum.

Qui sanctus oportere quaerendum ex, usu vivendo accusamus posidonium an. Quo cu graece reprimique.
Ea cum purto quando referrentur, tritani perfecto ne sit. Ne sit iusto ludus, ea ius eruditi
dissentiunt, fabellas disputando eu vix. Te vim eripuit debitis tincidunt, in vim nonumes
consetetur.

Affert exerci aperiri pri ea. Ut dicant essent corrumpit sit. Sea saepe nullam referrentur ut, vis
dolores perfecto cu. At nam inimicus evertitur vulputate.

Dolor volutpat praesent vix ne, at soluta oblique admodum eum. Duis adipisci mea in, nam ut tota
choro theophrastus. Ex scripta definitiones mei, augue doctus ne sed, munere posidonium eum id. Ad
graeco audire per.

Sale salutatus et mei, mea elit illud adipiscing ei, cum ea sumo melius forensibus. Eu inani iusto
oporteat eum, ei vix iisque saperet detraxit. Fabulas perpetua similique eam ne, noster corpora
dissentiet qui ex, et qui integre graecis. Eripuit nonumes deterruisset an pro, ei ferri similique
cum. Odio dolores inciderint ei vim, an est dolorum delicata temporibus, eu mea quis accumsan. Vel
stet affert option at.

In gubergren voluptaria reprimique pro, option fuisset id est. Rebum delicata ad sea, ex vidit
errem vis, mei at duis dicam sensibus. Nibh debet iudicabit has no, vim te dicit libris possim.
Debet viderer consequuntur ea pro. Ex dicat iriure scripta pro.

An dicat diceret eligendi duo. Est cu equidem deterruisset, usu ad regione equidem, vim amet vero
possim ex. Theophrastus conclusionemque ad quo, inimicus deseruisse voluptatibus eum et. Duis
delectus mandamus an mei, usu timeam nostrum suscipiantur id.

```



## PureBasic


```purebasic

DataSection
  Data.s "In olden times when wishing still helped one, there lived a king "+
	"whose daughters were all beautiful, but the youngest was so beautiful "+
	"that the sun itself, which has seen so much, was astonished whenever "+
	"it shone-in-her-face.  Close-by-the-king's castle lay a great dark "+
	"forest, and under an old lime-tree in the forest was a well, and when "+
	"the day was very warm, the king's child went out into the forest and "+
	"sat down by the side of the cool-fountain, and when she was bored she "+
	"took a golden ball, and threw it up on high and caught it, and this "+
	"ball was her favorite plaything."
EndDataSection

Procedure.i ww_pos(txt$,l.i)
  While Mid(txt$,l,1)<>Chr(32) And l>0 And Len(txt$)>l : l-1 : Wend
  If l>0 : ProcedureReturn l : Else : ProcedureReturn Len(Trim(txt$)) : EndIf
EndProcedure

Procedure WriteLine(txt$,ls.i)
  Shared d$,lw
  Select LCase(d$)
    Case "l" : PrintN(Mid(txt$,1,ls))
    Case "r" : PrintN(RSet(Trim(Mid(txt$,1,ls)),lw,Chr(32)))
  EndSelect
EndProcedure

Procedure main(txt$,lw.i)
  If Len(txt$)
    p=ww_pos(txt$,lw) : WriteLine(txt$,p) : ProcedureReturn main(LTrim(Right(txt$,Len(txt$)-p)),lw)
  EndIf
EndProcedure

Procedure.i MaxWordLen(txt$)
  For i=1 To CountString(txt$,Chr(32))+1
    wrd$=LTrim(StringField(txt$,i,Chr(32)))
    wrdl=Len(wrd$)+1 : If wrdl>l : l=wrdl : EndIf
  Next
  ProcedureReturn l
EndProcedure

OpenConsole()
Read.s t$
Print("Input line width: ") : lw=Val(Input()) : minL=MaxWordLen(t$)
If lw<minL : lw=minL : PrintN("Min. line width "+Str(lw-1)) : EndIf
Print("Input direction l:left r:rigth ")
Repeat : d$=Inkey() : Delay(50) : Until FindString("lr",d$,1,#PB_String_NoCase) : PrintN(d$+#CRLF$)
main(t$,lw) : Input()

```

{{out}}

```txt

Input line width: 40
Input direction l:left r:rigth l

In olden times when wishing still
helped one, there lived a king whose
daughters were all beautiful, but the
youngest was so beautiful that the sun
itself, which has seen so much, was
astonished whenever it
shone-in-her-face.  Close-by-the-king's
castle lay a great dark forest, and
under an old lime-tree in the forest
was a well, and when the day was very
warm, the king's child went out into
the forest and sat down by the side of
the cool-fountain, and when she was
bored she took a golden ball, and threw
it up on high and caught it, and this
ball was her favorite plaything.

```


```txt

Input line width: 40
Input direction l:left r:rigth r

       In olden times when wishing still
    helped one, there lived a king whose
   daughters were all beautiful, but the
  youngest was so beautiful that the sun
     itself, which has seen so much, was
                  astonished whenever it
 shone-in-her-face.  Close-by-the-king's
     castle lay a great dark forest, and
    under an old lime-tree in the forest
   was a well, and when the day was very
    warm, the king's child went out into
  the forest and sat down by the side of
     the cool-fountain, and when she was
 bored she took a golden ball, and threw
   it up on high and caught it, and this
        ball was her favorite plaything.

```



## Python


```python>>>
 import textwrap
>>> help(textwrap.fill)
Help on function fill in module textwrap:

fill(text, width=70, **kwargs)
    Fill a single paragraph of text, returning a new string.

    Reformat the single paragraph in 'text' to fit in lines of no more
    than 'width' columns, and return a new string containing the entire
    wrapped paragraph.  As with wrap(), tabs are expanded and other
    whitespace characters converted to space.  See TextWrapper class for
    available keyword args to customize wrapping behaviour.

>>> txt = '''\
Reformat the single paragraph in 'text' to fit in lines of no more
than 'width' columns, and return a new string containing the entire
wrapped paragraph.  As with wrap(), tabs are expanded and other
whitespace characters converted to space.  See TextWrapper class for
available keyword args to customize wrapping behaviour.'''
>>> print(textwrap.fill(txt, width=75))
Reformat the single paragraph in 'text' to fit in lines of no more than
'width' columns, and return a new string containing the entire wrapped
paragraph.  As with wrap(), tabs are expanded and other whitespace
characters converted to space.  See TextWrapper class for available keyword
args to customize wrapping behaviour.
>>> print(textwrap.fill(txt, width=45))
Reformat the single paragraph in 'text' to
fit in lines of no more than 'width' columns,
and return a new string containing the entire
wrapped paragraph.  As with wrap(), tabs are
expanded and other whitespace characters
converted to space.  See TextWrapper class
for available keyword args to customize
wrapping behaviour.
>>> print(textwrap.fill(txt, width=85))
Reformat the single paragraph in 'text' to fit in lines of no more than 'width'
columns, and return a new string containing the entire wrapped paragraph.  As with
wrap(), tabs are expanded and other whitespace characters converted to space.  See
TextWrapper class for available keyword args to customize wrapping behaviour.
>>>
```



## R



###  Using the base library


Use <code>strwrap()</code>:

```rsplus>
 x <- "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Donec a diam lectus. Sed sit amet ipsum mauris. Maecenas congue ligula ac quam viverra nec consectetur ante hendrerit. Donec et mollis dolor. Praesent et diam eget libero egestas mattis sit amet vitae augue. Nam tincidunt congue enim, ut porta lorem lacinia consectetur. "
> cat(paste(strwrap(x=c(x, "\n"), width=80), collapse="\n"))
Lorem ipsum dolor sit amet, consectetur adipiscing elit. Donec a diam lectus.
Sed sit amet ipsum mauris. Maecenas congue ligula ac quam viverra nec
consectetur ante hendrerit. Donec et mollis dolor. Praesent et diam eget libero
egestas mattis sit amet vitae augue. Nam tincidunt congue enim, ut porta lorem
lacinia consectetur.
> cat(paste(strwrap(x=c(x, "\n"), width=60), collapse="\n"))
Lorem ipsum dolor sit amet, consectetur adipiscing elit.
Donec a diam lectus. Sed sit amet ipsum mauris. Maecenas
congue ligula ac quam viverra nec consectetur ante
hendrerit. Donec et mollis dolor. Praesent et diam eget
libero egestas mattis sit amet vitae augue. Nam tincidunt
congue enim, ut porta lorem lacinia consectetur.
```



###  Using the stringr tidyverse library


Another option, using <code>stringr::str_wrap</code>

```rsplus

> x <- "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Donec a diam lectus. Sed sit amet ipsum mauris. Maecenas congue ligula ac quam viverra nec consectetur ante hendrerit. Donec et mollis dolor. Praesent et diam eget libero egestas mattis sit amet vitae augue. Nam tincidunt congue enim, ut porta lorem lacinia consectetur. "
> cat(stringr::str_wrap(x, 60))
Lorem ipsum dolor sit amet, consectetur adipiscing elit.
Donec a diam lectus. Sed sit amet ipsum mauris. Maecenas
congue ligula ac quam viverra nec consectetur ante
hendrerit. Donec et mollis dolor. Praesent et diam eget
libero egestas mattis sit amet vitae augue. Nam tincidunt
congue enim, ut porta lorem lacinia consectetur.

```



## Racket


Using a library function:

```Racket

#lang at-exp racket
(require scribble/text/wrap)
(define text
  @(λ xs (regexp-replace* #rx" *\n *" (string-append* xs) " ")){
    In olden times when wishing still helped one, there lived a king whose
    daughters were all beautiful, but the youngest was so beautiful that the
    sun itself, which has seen so much, was astonished whenever it shone in her
    face.  Close by the king's castle lay a great dark forest, and under an old
    lime-tree in the forest was a well, and when the day was very warm, the
    king's child went out into the forest and sat down by the side of the cool
    fountain, and when she was bored she took a golden ball, and threw it up on
    high and caught it, and this ball was her favorite plaything.})
(for-each displayln (wrap-line text 60))

```


Explicit (and simple) implementation:

```racket

#lang racket

(define (wrap words width)
  (define (maybe-cons xs xss)
    (if (empty? xs) xss (cons xs xss)))
  (match/values
    (for/fold ([lines '()] [line '()] [left width]) ([w words])
      (define n (string-length w))
      (cond
        [(> n width) ; word longer than line => line on its own
         (values (cons (list w) (maybe-cons line lines)) '() width)]
        [(> n left)  ; not enough space left => new line
         (values (cons line lines) (list w) (- width n 1))]
        [else
         (values lines (cons w line) (- left n 1))]))
    [(lines line _)
     (apply string-append
            (for/list ([line (reverse (cons line lines))])
              (string-join line #:after-last "\n")))]))

;;; Usage:
(wrap (string-split text) 70)

```



## REXX


### version 0

This version was the original (of version 1) and has no error checking and only does left-margin justification.

```rexx
/*REXX program  reads  a file  and  displays  it to the screen  (with word wrap).       */
parse arg iFID width .                           /*obtain optional arguments from the CL*/
if  iFID=='' |  iFID==","  then  iFID='LAWS.TXT' /*Not specified?  Then use the default.*/
if width=='' | width==","  then width=linesize() /* "      "         "   "   "     "    */
@=                                               /*number of words in the file (so far).*/
            do  while lines(iFID)\==0            /*read from the file until End-Of-File.*/
            @=@ linein(iFID)                     /*get a record  (line of text).        */
            end   /*while*/
$=word(@,1)                                      /*initialize  $  with the first word.  */
            do k=2  for words(@)-1;  x=word(@,k) /*parse until text (@) exhausted.      */
            _=$ x                                /*append it to the  $  list and test.  */
            if length(_)>=width  then do;  say $ /*this word a bridge too far?    > w.  */
                                           _=x   /*assign this word to the next line.   */
                                     end
            $=_                                  /*new words (on a line)  are OK so far.*/
            end   /*m*/
if $\==''  then say $                            /*handle any residual words (overflow).*/
                                                 /*stick a fork in it,  we're all done. */
```

'''output'''   is the same as version 1 using the   '''L'''eft   option (the default).


### version 1

The input for this program is in a file   (named '''LAWS.TXT''').

The default width of the output is the current terminal width   (normally, this would be the window's width).

If the terminal width (or window's width) is indeterminable, then   80   is used.

The width can be expressed as a percentage (i.e.:   50%)   which signifies to use ½ of the terminal's width).

No hyphenation (or de-hyphenation) is attempted.

Words longer than the width of the output are acceptable and are shown (with no truncation), a simple change could be made to issue a notification.

Some rudimentary error checking is performed.


Types of word wrapping (justification) are   (only the first character is significant):

```txt

                 Center:       ◄centered►
                 Right:  ────────►right margin
                 Left:   left margin◄─────────
                 Both:   ◄───both margins────►

```

('''L'''eft   is the default.)

This version was modified (for speed at the expense of simplicity) to accommodate faster processing of large files.

Instead of appending lines of a file to a character string, the words are picked off and stored in a stemmed array.

This decreases the amount of work that REXX has to do to retrieve (get) the next word in the (possibly) ginormous string.

```rexx
/*REXX program  reads  a  file  and  displays  it to the screen  (with word wrap).      */
parse arg iFID width kind _ .                    /*obtain optional arguments from the CL*/
if  iFID=='' | iFID==","  then iFID = 'LAWS.TXT' /*Not specified?  Then use the default.*/
if width=='' |width==","  then width= linesize() /* "      "         "   "   "     "    */
if right(width, 1) =='%'  then width= linesize() * translate(width, , "%")  %  100
if kind=='' | kind==","   then kind= 'Left'      /*Default?  Then use the default: LEFT */
just= left(kind, 1);  upper just                 /*use 1st char of JUSTIFY,  uppercased.*/
if pos(just, 'BCLR')==0  then call err "KIND  (3rd arg) is illegal:"      kind
if _\==''                then call err "too many arguments specified."      _
if \datatype(width,'W')  then call err "WIDTH (2nd arg) isn't an integer:"  width
n=0                                              /*the number of words in the file.     */
            do j=0  while lines(iFID)\==0        /*read from the file until End-Of-File.*/
            _=linein(iFID)                       /*get a record  (line of text).        */
                 do  until _=='';      n= n + 1  /*extract some words  (or maybe not).  */
                 parse var _   @.n  _            /*obtain and assign next word in text. */
                 end   /*until*/                 /*parse 'til the line of text is null. */
            end        /*j*/

if j==0   then call err  'file'  iFID  "not found."
if n==0   then call err  'file'  iFID  "is empty  (or has no words)"
$=@.1                                            /*initialize  $  string with first word*/
            do m=2  for n-1;           x= @.m    /*parse until text  (@)  is exhausted. */
            _= $ x                               /*append it to the  $  string and test.*/
            if length(_)>= width  then call tell /*this word a bridge too far?   > w    */
            $= _                                 /*the new words are OK  (so far).      */
            end   /*m*/
call tell                                        /*handle any residual words  (if any). */
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
err:  say;    say '***error***';     say;     say arg(1);     say;      say;       exit 13
/*──────────────────────────────────────────────────────────────────────────────────────*/
tell: if $==''  then return                      /* [↓]  the first word may be too long.*/
      w=max(width, length($) )                   /*don't truncate long words  (> w).    */
            select
            when just=='L'  then $=  strip($)    /*left ◄────────                       */
            when just=='R'  then $=  right($, w) /*──────► right                        */
            when just=='B'  then $=justify($, w) /*◄────both────►                       */
            when just=='C'  then $= center($, w) /*  ◄centered►                         */
            end   /*select*/
      say $                                      /*display the line of words to terminal*/
      _= x                                       /*handle any word overflow.            */
      return                                     /*go back and keep truckin'.           */
```

This REXX program makes use of   '''LINESIZE'''   REXX program (or BIF) which is used to determine the screen width (or linesize) of the terminal (console).

The   '''LINESIZE.REX'''   REXX program is included here   ──►   [[LINESIZE.REX]].


'''input file''':
<pre style="height:165ex">
     ────────── Computer programming laws ──────────
 The Primal Scenario  -or-  Basic Datum of Experience:
    ∙ Systems in general work poorly or not at all.
    ∙ Nothing complicated works.
    ∙ Complicated systems seldom exceed 5% efficiency.
    ∙ There is always a fly in the ointment.
 The Fundamental Theorem:
    ∙ New systems generate new problems.
 Occam's Razor:
    ∙ Systems should not be unnecessarily multiplied.
 The Law of Conservation of Energy:
    ∙ The total amount of energy in the universe is constant.
    ∙ Systems operate by redistributing energy into different forms and into accumulations of different sizes.
 Laws of Growth:
    ∙ Systems tend to grow, and as they grow, they encroach.
 The Big-Bang Theorem of Systems-Cosmology:
    ∙ Systems tend to expand to fill the known universe.
 Parkinson's Extended Law:
    ∙ The system itself tends to expand at 5-6% per annum.
 The Generalized Uncertainty Principle:
    ∙ Systems display antics.
    ∙ Complicated systems produce unexpected outcomes.
    ∙ The total behavior of large systems cannot be predicted.
 The Non-Additivity Theorem of Systems-Behavior -or- Climax Design Theorem:
    ∙ A large system, produced by expanding the dimensions of a smaller system, does not behave like the smaller system.
 LeChateliers's Principle:
    ∙ Complex systems tend to oppose their own proper function.
    ∙ Systems get in the way.
    ∙ The system always kicks back.
    ∙ Positive feedback is dangerous.
 Functionary's Falsity:
    ∙ People in systems do not do what the system says they are doing.
    ∙ The function performed by a system is not operationally identical to the function of the same name performed by a man.
    ∙ A function performed by a larger system is not operationally identical to the function of the same name performed by a smaller system.
 The Fundamental Law of Administrative Workings:
    ∙ Things are what they are reported to be.
    ∙ The real world is whatever is reported to the system.
    ∙ If it isn't official; it didn't happen.
    ∙ If it's made in Detroit, it must be an automobile.
    ∙ A system is no better than its sensory organs.
    ∙ To those within a system, the outside reality tends to pale and disappear.
    ∙ Systems attract systems-people.
    ∙ For every human system, there is a type of person adapted to thrive on it or in it.
    ∙ The bigger the system, the narrower and more specialized the interface with individuals.
 Administrator's Anxiety:
    ∙ Pushing on the systems doesn't help.  It just makes things worse.
    ∙ A complex system cannot be "made" to work.  It either works or it doesn't.
    ∙ A simple system, designed from scratch, sometimes works.
    ∙ A simple system may or may not work.
    ∙ Some complex systems actually work.
    ∙ If a system is working, leave it alone.
    ∙ A complex system that works is invariably found to have evolved from a simple system that works.
    ∙ A complex system designed from scratch never works and cannot be patched up to make it work.  You have to start over, beginning with a working simple system.
    ∙ Programs never run the first time.
    ∙ Complex programs never run.
    ∙ Anything worth doing once will probably have to be done twice.
 The Functional indeterminacy Theorem:
    ∙ In complex systems, malfunction and even total nonfunction may not be detectable for long periods, if ever.
 The Kantian Hypothesis  -or-  Know-Nothing Theorem:
    ∙ Large complex systems are beyond human capacity to evaluate.
 The Newtonian Lay of Systems-Inertia:
    ∙ A system that performs a certain way will continue to operate in that way regardless of the need of of changed conditions.
    ∙ A system continues to do its thing, regardless of need.
    ∙ Systems develop goals of their own the instant they come into being.
    ∙ Intrasystem goals come first.
 Failure-Mode Theorems:
    ∙ Complex systems usually operate in failure mode.
    ∙ A complex system can fail in a infinite number of ways.
    ∙ If anything can go wrong, it will.
    ∙ The mode of failure of a complex system cannot ordinarily be predicted from its structure.
    ∙ The crucial variables are discovered by accident.
    ∙ The larger the system, the greater the probability of unexpected failure.
    ∙ "Success" or "function" in any system may be failure in the larger or smaller systems to which the system is connected.
    ∙ In setting up a new system, tread softly.  You may be disturbing another system that is actually working.
 The Fail-Safe Theorem:
    ∙ When a fail-safe system fails, it fails by failing to fail safe.
    ∙ Complex systems tend to produce complex responses (not solutions) to problems.
    ∙ Great advances are not produced by systems designed to produce great advances.
    ∙ Loose systems last longer and work better.
    ∙ Efficient systems are dangerous to themselves and to others.
 The Vector Theory of Systems:
    ∙ Systems run better when designed to run downhill.
    ∙ Systems aligned with human motivational vectors will sometimes work.  Systems opposing such vectors work poorly or not at all.
 Advanced Systems Theories:
    ∙ Everything is a system.
    ∙ Everything is a part of a larger system.
    ∙ The universe is infinitely systematized, both upward [larger systems] and downward [smaller systems].
    ∙ All systems are infinitely complex.  (The illusion of simplicity comes from focusing attention on one or a few variables.)
    ∙ Parameters are variables traveling under an assumed name.

```

{{out|output|text=  when using the input of:     <tt> ,   155 </tt>}}
<pre style="height:45ex">
────────── Computer programming laws ────────── The Primal Scenario -or- Basic Datum of Experience: ∙ Systems in general work poorly or not at all. ∙
Nothing complicated works. ∙ Complicated systems seldom exceed 5% efficiency. ∙ There is always a fly in the ointment. The Fundamental Theorem: ∙ New
systems generate new problems. Occam's Razor: ∙ Systems should not be unnecessarily multiplied. The Law of Conservation of Energy: ∙ The total amount of
energy in the universe is constant. ∙ Systems operate by redistributing energy into different forms and into accumulations of different sizes. Laws of
Growth: ∙ Systems tend to grow, and as they grow, they encroach. The Big-Bang Theorem of Systems-Cosmology: ∙ Systems tend to expand to fill the known
universe. Parkinson's Extended Law: ∙ The system itself tends to expand at 5-6% per annum. The Generalized Uncertainty Principle: ∙ Systems display
antics. ∙ Complicated systems produce unexpected outcomes. ∙ The total behavior of large systems cannot be predicted. The Non-Additivity Theorem of
Systems-Behavior -or- Climax Design Theorem: ∙ A large system, produced by expanding the dimensions of a smaller system, does not behave like the smaller
system. LeChateliers's Principle: ∙ Complex systems tend to oppose their own proper function. ∙ Systems get in the way. ∙ The system always kicks back. ∙
Positive feedback is dangerous. Functionary's Falsity: ∙ People in systems do not do what the system says they are doing. ∙ The function performed by a
system is not operationally identical to the function of the same name performed by a man. ∙ A function performed by a larger system is not operationally
identical to the function of the same name performed by a smaller system. The Fundamental Law of Administrative Workings: ∙ Things are what they are
reported to be. ∙ The real world is whatever is reported to the system. ∙ If it isn't official; it didn't happen. ∙ If it's made in Detroit, it must be an
automobile. ∙ A system is no better than its sensory organs. ∙ To those within a system, the outside reality tends to pale and disappear. ∙ Systems
attract systems-people. ∙ For every human system, there is a type of person adapted to thrive on it or in it. ∙ The bigger the system, the narrower and
more specialized the interface with individuals. Administrator's Anxiety: ∙ Pushing on the systems doesn't help. It just makes things worse. ∙ A complex
system cannot be "made" to work. It either works or it doesn't. ∙ A simple system, designed from scratch, sometimes works. ∙ A simple system may or may
not work. ∙ Some complex systems actually work. ∙ If a system is working, leave it alone. ∙ A complex system that works is invariably found to have
evolved from a simple system that works. ∙ A complex system designed from scratch never works and cannot be patched up to make it work. You have to start
over, beginning with a working simple system. ∙ Programs never run the first time. ∙ Complex programs never run. ∙ Anything worth doing once will probably
have to be done twice. The Functional indeterminacy Theorem: ∙ In complex systems, malfunction and even total nonfunction may not be detectable for long
periods, if ever. The Kantian Hypothesis -or- Know-Nothing Theorem: ∙ Large complex systems are beyond human capacity to evaluate. The Newtonian Lay of
Systems-Inertia: ∙ A system that performs a certain way will continue to operate in that way regardless of the need of of changed conditions. ∙ A system
continues to do its thing, regardless of need. ∙ Systems develop goals of their own the instant they come into being. ∙ Intrasystem goals come first.
Failure-Mode Theorems: ∙ Complex systems usually operate in failure mode. ∙ A complex system can fail in a infinite number of ways. ∙ If anything can go
wrong, it will. ∙ The mode of failure of a complex system cannot ordinarily be predicted from its structure. ∙ The crucial variables are discovered by
accident. ∙ The larger the system, the greater the probability of unexpected failure. ∙ "Success" or "function" in any system may be failure in the larger
or smaller systems to which the system is connected. ∙ In setting up a new system, tread softly. You may be disturbing another system that is actually
working. The Fail-Safe Theorem: ∙ When a fail-safe system fails, it fails by failing to fail safe. ∙ Complex systems tend to produce complex responses
(not solutions) to problems. ∙ Great advances are not produced by systems designed to produce great advances. ∙ Loose systems last longer and work better.
∙ Efficient systems are dangerous to themselves and to others. The Vector Theory of Systems: ∙ Systems run better when designed to run downhill. ∙ Systems
aligned with human motivational vectors will sometimes work. Systems opposing such vectors work poorly or not at all. Advanced Systems Theories: ∙
Everything is a system. ∙ Everything is a part of a larger system. ∙ The universe is infinitely systematized, both upward [larger systems] and downward
[smaller systems]. ∙ All systems are infinitely complex. (The illusion of simplicity comes from focusing attention on one or a few variables.) ∙

```


{{out|output|text=  when using the input:     <tt> ,   80 </tt>}}
<pre style="height:45ex">
────────── Computer programming laws ────────── The Primal Scenario -or- Basic
Datum of Experience: ∙ Systems in general work poorly or not at all. ∙ Nothing
complicated works. ∙ Complicated systems seldom exceed 5% efficiency. ∙ There
is always a fly in the ointment. The Fundamental Theorem: ∙ New systems
generate new problems. Occam's Razor: ∙ Systems should not be unnecessarily
multiplied. The Law of Conservation of Energy: ∙ The total amount of energy in
the universe is constant. ∙ Systems operate by redistributing energy into
different forms and into accumulations of different sizes. Laws of Growth: ∙
Systems tend to grow, and as they grow, they encroach. The Big-Bang Theorem of
Systems-Cosmology: ∙ Systems tend to expand to fill the known universe.
Parkinson's Extended Law: ∙ The system itself tends to expand at 5-6% per
annum. The Generalized Uncertainty Principle: ∙ Systems display antics. ∙
Complicated systems produce unexpected outcomes. ∙ The total behavior of large
systems cannot be predicted. The Non-Additivity Theorem of Systems-Behavior
-or- Climax Design Theorem: ∙ A large system, produced by expanding the
dimensions of a smaller system, does not behave like the smaller system.
LeChateliers's Principle: ∙ Complex systems tend to oppose their own proper
function. ∙ Systems get in the way. ∙ The system always kicks back. ∙ Positive
feedback is dangerous. Functionary's Falsity: ∙ People in systems do not do
what the system says they are doing. ∙ The function performed by a system is
not operationally identical to the function of the same name performed by a
man. ∙ A function performed by a larger system is not operationally identical
to the function of the same name performed by a smaller system. The Fundamental
Law of Administrative Workings: ∙ Things are what they are reported to be. ∙
The real world is whatever is reported to the system. ∙ If it isn't official;
it didn't happen. ∙ If it's made in Detroit, it must be an automobile. ∙ A
system is no better than its sensory organs. ∙ To those within a system, the
outside reality tends to pale and disappear. ∙ Systems attract systems-people.
∙ For every human system, there is a type of person adapted to thrive on it or
in it. ∙ The bigger the system, the narrower and more specialized the interface
with individuals. Administrator's Anxiety: ∙ Pushing on the systems doesn't
help. It just makes things worse. ∙ A complex system cannot be "made" to work.
It either works or it doesn't. ∙ A simple system, designed from scratch,
sometimes works. ∙ A simple system may or may not work. ∙ Some complex systems
actually work. ∙ If a system is working, leave it alone. ∙ A complex system
that works is invariably found to have evolved from a simple system that works.
∙ A complex system designed from scratch never works and cannot be patched up
to make it work. You have to start over, beginning with a working simple
system. ∙ Programs never run the first time. ∙ Complex programs never run. ∙
Anything worth doing once will probably have to be done twice. The Functional
indeterminacy Theorem: ∙ In complex systems, malfunction and even total
nonfunction may not be detectable for long periods, if ever. The Kantian
Hypothesis -or- Know-Nothing Theorem: ∙ Large complex systems are beyond human
capacity to evaluate. The Newtonian Lay of Systems-Inertia: ∙ A system that
performs a certain way will continue to operate in that way regardless of the
need of of changed conditions. ∙ A system continues to do its thing, regardless
of need. ∙ Systems develop goals of their own the instant they come into being.
∙ Intrasystem goals come first. Failure-Mode Theorems: ∙ Complex systems
usually operate in failure mode. ∙ A complex system can fail in a infinite
number of ways. ∙ If anything can go wrong, it will. ∙ The mode of failure of a
complex system cannot ordinarily be predicted from its structure. ∙ The crucial
variables are discovered by accident. ∙ The larger the system, the greater the
probability of unexpected failure. ∙ "Success" or "function" in any system may
be failure in the larger or smaller systems to which the system is connected. ∙
In setting up a new system, tread softly. You may be disturbing another system
that is actually working. The Fail-Safe Theorem: ∙ When a fail-safe system
fails, it fails by failing to fail safe. ∙ Complex systems tend to produce
complex responses (not solutions) to problems. ∙ Great advances are not
produced by systems designed to produce great advances. ∙ Loose systems last
longer and work better. ∙ Efficient systems are dangerous to themselves and to
others. The Vector Theory of Systems: ∙ Systems run better when designed to run
downhill. ∙ Systems aligned with human motivational vectors will sometimes
work. Systems opposing such vectors work poorly or not at all. Advanced Systems
Theories: ∙ Everything is a system. ∙ Everything is a part of a larger system.
∙ The universe is infinitely systematized, both upward [larger systems] and
downward [smaller systems]. ∙ All systems are infinitely complex. (The illusion
of simplicity comes from focusing attention on one or a few variables.) ∙
Parameters are variables traveling under an assumed name.

```


{{out|output|text=    [justified]   when specifying: <tt> , 80 both </tt>}}
<pre style="height:45ex">
────────── Computer programming laws ──────────  The Primal Scenario -or- Basic
Datum of Experience: ∙ Systems in general  work poorly or not at all. ∙ Nothing
complicated works. ∙ Complicated systems  seldom  exceed 5% efficiency. ∙ There
is always a  fly  in  the  ointment.  The  Fundamental  Theorem:  ∙ New systems
generate new problems. Occam's  Razor:  ∙  Systems  should not be unnecessarily
multiplied. The Law of Conservation of Energy:  ∙ The total amount of energy in
the universe is  constant.  ∙  Systems  operate  by  redistributing energy into
different forms and into accumulations  of  different  sizes. Laws of Growth: ∙
Systems tend to grow, and as they  grow, they encroach. The Big-Bang Theorem of
Systems-Cosmology: ∙  Systems  tend  to  expand  to  fill  the  known universe.
Parkinson's Extended Law: ∙  The  system  itself  tends  to  expand at 5-6% per
annum. The  Generalized  Uncertainty  Principle:  ∙  Systems  display antics. ∙
Complicated systems produce unexpected outcomes.  ∙ The total behavior of large
systems cannot be  predicted.  The  Non-Additivity  Theorem of Systems-Behavior
-or- Climax  Design  Theorem:  ∙  A  large  system,  produced  by expanding the
dimensions of a  smaller  system,  does  not  behave  like  the smaller system.
LeChateliers's Principle: ∙ Complex  systems  tend  to  oppose their own proper
function. ∙ Systems get in the way.  ∙ The system always kicks back. ∙ Positive
feedback is dangerous. Functionary's  Falsity:  ∙  People  in systems do not do
what the system says they are  doing.  ∙  The function performed by a system is
not operationally identical to the  function  of  the  same name performed by a
man. ∙ A function performed by  a  larger system is not operationally identical
to the function of the same name performed by a smaller system. The Fundamental
Law of Administrative Workings: ∙ Things  are  what  they are reported to be. ∙
The real world is whatever is reported  to  the system. ∙ If it isn't official;
it didn't happen. ∙ If it's  made  in  Detroit,  it  must be an automobile. ∙ A
system is no better than its  sensory  organs.  ∙ To those within a system, the
outside reality tends to pale  and disappear. ∙ Systems attract systems-people.
∙ For every human system, there is a  type of person adapted to thrive on it or
in it. ∙ The bigger the system, the narrower and more specialized the interface
with individuals. Administrator's  Anxiety:  ∙  Pushing  on the systems doesn't
help. It just makes things worse. ∙  A complex system cannot be "made" to work.
It either works  or  it  doesn't.  ∙  A  simple  system, designed from scratch,
sometimes works. ∙ A simple system may  or may not work. ∙ Some complex systems
actually work. ∙ If a system  is  working,  leave  it alone. ∙ A complex system
that works is invariably found to have evolved from a simple system that works.
∙ A complex system designed from  scratch  never works and cannot be patched up
to make it work.  You  have  to  start  over,  beginning  with a working simple
system. ∙ Programs never run the  first  time.  ∙ Complex programs never run. ∙
Anything worth doing once will probably  have  to be done twice. The Functional
indeterminacy  Theorem:  ∙  In  complex  systems,  malfunction  and  even total
nonfunction may not  be  detectable  for  long  periods,  if  ever. The Kantian
Hypothesis -or- Know-Nothing Theorem: ∙  Large complex systems are beyond human
capacity to evaluate. The  Newtonian  Lay  of  Systems-Inertia: ∙ A system that
performs a certain way will continue  to  operate in that way regardless of the
need of of changed conditions. ∙ A system continues to do its thing, regardless
of need. ∙ Systems develop goals of their own the instant they come into being.
∙ Intrasystem  goals  come  first.  Failure-Mode  Theorems:  ∙  Complex systems
usually operate in failure mode.  ∙  A  complex  system  can fail in a infinite
number of ways. ∙ If anything can go wrong, it will. ∙ The mode of failure of a
complex system cannot ordinarily be predicted from its structure. ∙ The crucial
variables are discovered by accident. ∙  The larger the system, the greater the
probability of unexpected failure. ∙ "Success"  or "function" in any system may
be failure in the larger or smaller systems to which the system is connected. ∙
In setting up a new system, tread  softly. You may be disturbing another system
that is actually working.  The  Fail-Safe  Theorem:  ∙  When a fail-safe system
fails, it fails by failing  to  fail  safe.  ∙  Complex systems tend to produce
complex responses  (not  solutions)  to  problems.  ∙  Great  advances  are not
produced by systems designed to  produce  great  advances. ∙ Loose systems last
longer and work better. ∙ Efficient  systems are dangerous to themselves and to
others. The Vector Theory of Systems: ∙ Systems run better when designed to run
downhill. ∙ Systems  aligned  with  human  motivational  vectors will sometimes
work. Systems opposing such vectors work poorly or not at all. Advanced Systems
Theories: ∙ Everything is a system. ∙  Everything is a part of a larger system.
∙ The universe is  infinitely  systematized,  both  upward [larger systems] and
downward [smaller systems]. ∙ All systems are infinitely complex. (The illusion
of simplicity comes from  focusing  attention  on  one  or  a few variables.) ∙
Parameters    are    variables    traveling     under    an    assumed    name.

```



### version 2


```rexx
/* REXX ***************************************************************
* 20.08.2013 Walter Pachl  "my way"
* 23.08.2013 Walter Pachl changed to use lastpos bif
**********************************************************************/
Parse Arg w
oid=w'.xxx'; 'erase' oid
Call o left(copies('123456789.',20),w)
s='She should have died hereafter;' ,
  'There would have been a time for such a word.' ,
  'Tomorrow, and tomorrow, and tomorrow, and so on'
Call ow s
Exit
ow:
  Parse Arg s
  s=s' '
  Do While length(s)>w
    i=lastpos(' ',s,w+1) /* instead of loop */
    If i=0 Then
      p=pos(' ',s)
    Else
      p=i
    Call o left(s,p)
    s=substr(s,p+1)
    End
  If s>'' Then
    Call o s
  Return
o:Return lineout(oid,arg(1))
```

{{out}} for widths 72 and 9

```txt

123456789.123456789.123456789.123456789.123456789.123456789.123456789.12
She should have died hereafter; There would have been a time for such a
word. Tomorrow, and tomorrow, and tomorrow, and so on

123456789
She
should
have died
hereafter;
There
would
have been
a time
for such
a word.
Tomorrow,
and
tomorrow,
and
tomorrow,
and so on

```



## Ring


```ring

# Project : Word wrap

load "stdlib.ring"

doc = "In olden times when wishing still helped one, there lived a king
whose daughters were all beautiful, but the youngest was so beautiful
that the sun itself, which has seen so much, was astonished whenever
it shone in her face.  Close by the king's castle lay a great dark
forest, and under an old lime-tree in the forest was a well, and when
the day was very warm, the king's child went out into the forest and
sat down by the side of the cool fountain, and when she was bored she
took a golden ball, and threw it up on high and caught it, and this
ball was her favorite plaything."

wordwrap(doc,72)
wordwrap(doc,80)

func wordwrap(doc, maxline)
        words = split(doc, " ")
        line = words[1]
        for i=2 to len(words)
             word = words[i]
            if len(line)+len(word)+1 > maxline
               see line + nl
               line = word
            else
               line = line + " " + word
           ok
        next
        see line + nl + nl

```

Output:

```txt

In olden times when wishing still helped one, there lived a king whose
daughters were all beautiful, but the youngest was so beautiful that the
sun itself, which has seen so much, was astonished whenever it shone in
her face. Close by the king's castle lay a great dark forest, and under
an old lime-tree in the forest was a well, and when the day was very
warm, the king's child went out into the forest and sat down by the side
of the cool fountain, and when she was bored she took a golden ball, and
threw it up on high and caught it, and this ball was her favorite
plaything.

In olden times when wishing still helped one, there lived a king whose daughters
were all beautiful, but the youngest was so beautiful that the sun itself, which
has seen so much, was astonished whenever it shone in her face. Close by the
king's castle lay a great dark forest, and under an old lime-tree in the forest
was a well, and when the day was very warm, the king's child went out into the
forest and sat down by the side of the cool fountain, and when she was bored she
took a golden ball, and threw it up on high and caught it, and this ball was her
favorite plaything.

```



## Ruby


```ruby
class String
  def wrap(width)
    txt = gsub("\n", " ")
    para = []
    i = 0
    while i < length
      j = i + width
      j -= 1 while j != txt.length && j > i + 1 && !(txt[j] =~ /\s/)
      para << txt[i ... j]
      i = j + 1
    end
    para
  end
end

text = <<END
In olden times when wishing still helped one, there lived a king
whose daughters were all beautiful, but the youngest was so beautiful
that the sun itself, which has seen so much, was astonished whenever
it shone in her face.  Close by the king's castle lay a great dark
forest, and under an old lime-tree in the forest was a well, and when
the day was very warm, the king's child went out into the forest and
sat down by the side of the cool fountain, and when she was bored she
took a golden ball, and threw it up on high and caught it, and this
ball was her favorite plaything.
END

[72,80].each do |w|
  puts "." * w
  puts text.wrap(w)
end
```


{{out}}

```txt

........................................................................
In olden times when wishing still helped one, there lived a king whose
daughters were all beautiful, but the youngest was so beautiful that the
sun itself, which has seen so much, was astonished whenever it shone in
her face.  Close by the king's castle lay a great dark forest, and under
an old lime-tree in the forest was a well, and when the day was very
warm, the king's child went out into the forest and sat down by the side
of the cool fountain, and when she was bored she took a golden ball, and
threw it up on high and caught it, and this ball was her favorite
plaything.
................................................................................
In olden times when wishing still helped one, there lived a king whose daughters
were all beautiful, but the youngest was so beautiful that the sun itself, which
has seen so much, was astonished whenever it shone in her face.  Close by the
king's castle lay a great dark forest, and under an old lime-tree in the forest
was a well, and when the day was very warm, the king's child went out into the
forest and sat down by the side of the cool fountain, and when she was bored she
took a golden ball, and threw it up on high and caught it, and this ball was her
favorite plaything.

```



## Run BASIC

Word Wrap style for different browsers.
This automatically adjusts the text if the browser window is stretched in any direction

```runbasic
doc$ = "In olden times when wishing still helped one, there lived a king ";_
"whose daughters were all beautiful, but the youngest was so beautiful ";_
"that the sun itself, which has seen so much, was astonished whenever ";_
"it shone in her face."

wrap$ = " style='white-space: pre-wrap;white-space: -moz-pre-wrap;white-space: -pre-wrap;";_
                "white-space: -o-pre-wrap;word-wrap: break-word'"

html "<table border=1 cellpadding=2 cellspacing=0><tr" + wrap$ +" valign=top>"
html "<td width=60%>" + doc$ + "</td><td width=40%>" + doc$ + "</td></tr></table>"
```

output will adjust as you stretch the browser and maintain a 60 to 40 ratio of the width of the screen.

```txt

---------- at 60%-----------------------                         | -------- at 40%----------------------
In olden times when wishing still helped one, there lived a king | In olden times when wishing still helped
whose daughters were all beautiful, but the youngest was so      | one, there lived a king whose daughters
beautiful that the sun itself, which has seen so much, was       | were all beautiful, but the youngest was
astonished whenever it shone in her face.	                 | so beautiful that the sun itself, which
								 | has seen so much, was astonished whenever
								 | it shone in her face.

```

Without Browser

```runbasic
doc$ = "In olden times when wishing still helped one, there lived a king
whose daughters were all beautiful, but the youngest was so beautiful
that the sun itself, which has seen so much, was astonished whenever
it shone in her face.  Close by the king's castle lay a great dark
forest, and under an old lime-tree in the forest was a well, and when
the day was very warm, the king's child went out into the forest and
sat down by the side of the cool fountain, and when she was bored she
took a golden ball, and threw it up on high and caught it, and this
ball was her favorite plaything."

input "Width"; width                                         ' user specifies width

while word$(doc$,i + 1," ") <> ""
  i = i + 1
  thisWord$ = word$(doc$,i," ") + " "
  if word$(thisWord$,2,chr$(13)) <> "" then thisWord$ = word$(thisWord$,2,chr$(13)) + " " ' strip the <CR>
  if len(docOut$) + len(thisWord$) > width then
    print docOut$
    docOut$ = ""
  end if
  docOut$ = docOut$ + thisWord$
wend
print docOut$
```



## Scala


### Intuitive approach

{{libheader|Scala}}
```scala
import java.util.StringTokenizer

object WordWrap extends App {
  final val defaultLineWidth = 80
  final val spaceWidth = 1

  def letsWrap(text: String, lineWidth: Int = defaultLineWidth) = {
    println(s"\n\nWrapped at: $lineWidth")
    println("." * lineWidth)
    minNumLinesWrap(ewd, lineWidth)
  }

  final def ewd = "Vijftig jaar geleden publiceerde Edsger Dijkstra zijn kortstepadalgoritme. Daarom een kleine ode" +
    " aan de in 2002 overleden Dijkstra, iemand waar we als Nederlanders best wat trotser op mogen zijn. Dijkstra was" +
    " een van de eerste programmeurs van Nederland. Toen hij in 1957 trouwde, werd het beroep computerprogrammeur door" +
    " de burgerlijke stand nog niet erkend en uiteindelijk gaf hij maar `theoretische natuurkundige’ op.\nZijn" +
    " beroemdste resultaat is het kortstepadalgoritme, dat de kortste verbinding vindt tussen twee knopen in een graaf" +
    " (een verzameling punten waarvan sommigen verbonden zijn). Denk bijvoorbeeld aan het vinden van de kortste route" +
    " tussen twee steden. Het slimme van Dijkstra’s algoritme is dat het niet alle mogelijke routes met elkaar" +
    " vergelijkt, maar dat het stap voor stap de kortst mogelijke afstanden tot elk punt opbouwt. In de eerste stap" +
    " kijk je naar alle punten die vanaf het beginpunt te bereiken zijn en markeer je al die punten met de afstand tot" +
    " het beginpunt. Daarna kijk je steeds vanaf het punt dat op dat moment de kortste afstand heeft tot het beginpunt" +
    " naar alle punten die je vanaf daar kunt bereiken. Als je een buurpunt via een nieuwe verbinding op een snellere" +
    " manier kunt bereiken, schrijf je de nieuwe, kortere afstand tot het beginpunt bij zo’n punt. Zo ga je steeds een" +
    " stukje verder tot je alle punten hebt gehad en je de kortste route tot het eindpunt hebt gevonden."

  def minNumLinesWrap(text: String, LineWidth: Int) {
    val tokenizer = new StringTokenizer(text)
    var SpaceLeft = LineWidth
    while (tokenizer.hasMoreTokens) {
      val word: String = tokenizer.nextToken
      if ((word.length + spaceWidth) > SpaceLeft) {
        print("\n" + word + " ")
        SpaceLeft = LineWidth - word.length
      } else {
        print(word + " ")
        SpaceLeft -= (word.length + spaceWidth)
      }
    }
  }

  letsWrap(ewd)
  letsWrap(ewd, 120)
} // 44 lines
```
{{out}}
```txt
Wrapped at: 80
................................................................................
Vijftig jaar geleden publiceerde Edsger Dijkstra zijn kortstepadalgoritme.
Daarom een kleine ode aan de in 2002 overleden Dijkstra, iemand waar we als
Nederlanders best wat trotser op mogen zijn. Dijkstra was een van de eerste
programmeurs van Nederland. Toen hij in 1957 trouwde, werd het beroep
computerprogrammeur door de burgerlijke stand nog niet erkend en uiteindelijk
gaf hij maar `theoretische natuurkundige’ op. Zijn beroemdste resultaat is het
kortstepadalgoritme, dat de kortste verbinding vindt tussen twee knopen in een
graaf (een verzameling punten waarvan sommigen verbonden zijn). Denk
bijvoorbeeld aan het vinden van de kortste route tussen twee steden. Het slimme
van Dijkstra’s algoritme is dat het niet alle mogelijke routes met elkaar
vergelijkt, maar dat het stap voor stap de kortst mogelijke afstanden tot elk
punt opbouwt. In de eerste stap kijk je naar alle punten die vanaf het beginpunt
te bereiken zijn en markeer je al die punten met de afstand tot het beginpunt.
Daarna kijk je steeds vanaf het punt dat op dat moment de kortste afstand heeft
tot het beginpunt naar alle punten die je vanaf daar kunt bereiken. Als je een
buurpunt via een nieuwe verbinding op een snellere manier kunt bereiken, schrijf
je de nieuwe, kortere afstand tot het beginpunt bij zo’n punt. Zo ga je steeds
een stukje verder tot je alle punten hebt gehad en je de kortste route tot het
eindpunt hebt gevonden.

Wrapped at: 120
........................................................................................................................
Vijftig jaar geleden publiceerde Edsger Dijkstra zijn kortstepadalgoritme. Daarom een kleine ode aan de in 2002
overleden Dijkstra, iemand waar we als Nederlanders best wat trotser op mogen zijn. Dijkstra was een van de eerste
programmeurs van Nederland. Toen hij in 1957 trouwde, werd het beroep computerprogrammeur door de burgerlijke stand nog
niet erkend en uiteindelijk gaf hij maar `theoretische natuurkundige’ op. Zijn beroemdste resultaat is het
kortstepadalgoritme, dat de kortste verbinding vindt tussen twee knopen in een graaf (een verzameling punten waarvan
sommigen verbonden zijn). Denk bijvoorbeeld aan het vinden van de kortste route tussen twee steden. Het slimme van
Dijkstra’s algoritme is dat het niet alle mogelijke routes met elkaar vergelijkt, maar dat het stap voor stap de kortst
mogelijke afstanden tot elk punt opbouwt. In de eerste stap kijk je naar alle punten die vanaf het beginpunt te bereiken
zijn en markeer je al die punten met de afstand tot het beginpunt. Daarna kijk je steeds vanaf het punt dat op dat
moment de kortste afstand heeft tot het beginpunt naar alle punten die je vanaf daar kunt bereiken. Als je een buurpunt
via een nieuwe verbinding op een snellere manier kunt bereiken, schrijf je de nieuwe, kortere afstand tot het beginpunt
bij zo’n punt. Zo ga je steeds een stukje verder tot je alle punten hebt gehad en je de kortste route tot het eindpunt
hebt gevonden.
Process finished with exit code 0
```



## Scheme


The simple, greedy algorithm:


```scheme

(import (scheme base)
        (scheme write)
        (only (srfi 13) string-join string-tokenize))

;; word wrap, using greedy algorithm with minimum lines
(define (simple-word-wrap str width)
  (let loop ((words (string-tokenize str))
             (line-length 0)
             (line '())
             (lines '()))
    (cond ((null? words)
           (reverse (cons (reverse line) lines)))
          ((> (+ line-length (string-length (car words)))
              width)
           (if (null? line)
             (loop (cdr words) ; case where word exceeds line length
                   0
                   '()
                   (cons (list (car words)) lines))
             (loop words ; word must go to next line, so finish current line
                   0
                   '()
                   (cons (reverse line) lines))))
          (else
            (loop (cdr words) ; else, add word to current line
                  (+ 1 line-length (string-length (car words)))
                  (cons (car words) line)
                  lines)))))

;; run examples - text from RnRS report
(define *text* "Programming languages should be designed not by piling feature on top of feature, but by removing the weaknesses and restrictions that make additional features appear necessary. Scheme demonstrates that a very small number of rules for forming expressions, with no restrictions on how they are composed, suffice to form a practical and efficient programming language that is flexible enough to support most of the major programming paradigms in use today.")

(define (show-para algorithm width)
  (display (make-string width #\-)) (newline)
  (for-each (lambda (line) (display (string-join line " ")) (newline))
            (algorithm *text* width)))

(show-para simple-word-wrap 50)
(show-para simple-word-wrap 60)

```


{{out}}

(The line of hyphens shows the target width.)


```txt

--------------------------------------------------
Programming languages should be designed not by
piling feature on top of feature, but by removing
the weaknesses and restrictions that make
additional features appear necessary. Scheme
demonstrates that a very small number of rules for
forming expressions, with no restrictions on how
they are composed, suffice to form a practical and
efficient programming language that is flexible
enough to support most of the major programming
paradigms in use today.
------------------------------------------------------------
Programming languages should be designed not by piling
feature on top of feature, but by removing the weaknesses
and restrictions that make additional features appear
necessary. Scheme demonstrates that a very small number of
rules for forming expressions, with no restrictions on how
they are composed, suffice to form a practical and efficient
programming language that is flexible enough to support most
of the major programming paradigms in use today.

```



## Seed7


```seed7
$ include "seed7_05.s7i";

const func string: wrap (in string: aText, in integer: lineWidth) is func
  result
    var string: wrapped is "";
  local
    var array string: words is 0 times "";
    var string: word is "";
    var integer: spaceLeft is 0;
  begin
    words := split(aText, " ");
    if length(words) <> 0 then
      wrapped := words[1];
      words := words[2 ..];
      spaceLeft := lineWidth - length(wrapped);
      for word range words do
        if length(word) + 1 > spaceLeft then
          wrapped &:= "\n" & word;
          spaceLeft := lineWidth - length(word);
        else
          wrapped &:= " " & word;
          spaceLeft -:= 1 + length(word);
        end if;
      end for;
    end if;
  end func;

const proc: main is func
  local
    const string: frog is "In olden times when wishing still helped one, there lived \
        \a king whose daughters were all beautiful, but the youngest was so beautiful \
        \that the sun itself, which has seen so much, was astonished whenever it \
        \shone in her face. Close by the king's castle lay a great dark forest, and \
        \under an old lime-tree in the forest was a well, and when the day was very \
        \warm, the king's child went out into the forest and sat down by the side of \
        \the cool fountain, and when she was bored she took a golden ball, and threw \
        \it up on high and caught it, and this ball was her favorite plaything.";
    var integer: width is 0;
  begin
    for width range [] (72, 80) do
      writeln("Wrapped at " <& width <& ":");
      writeln(wrap(frog, width));
    end for;
  end func;
```
{{out}}
```txt
Wrapped at 72:
In olden times when wishing still helped one, there lived a king whose
daughters were all beautiful, but the youngest was so beautiful that the
sun itself, which has seen so much, was astonished whenever it shone in
her face. Close by the king's castle lay a great dark forest, and under
an old lime-tree in the forest was a well, and when the day was very
warm, the king's child went out into the forest and sat down by the side
of the cool fountain, and when she was bored she took a golden ball, and
threw it up on high and caught it, and this ball was her favorite
plaything.

Wrapped at 80:
In olden times when wishing still helped one, there lived a king whose daughters
were all beautiful, but the youngest was so beautiful that the sun itself, which
has seen so much, was astonished whenever it shone in her face. Close by the
king's castle lay a great dark forest, and under an old lime-tree in the forest
was a well, and when the day was very warm, the king's child went out into the
forest and sat down by the side of the cool fountain, and when she was bored she
took a golden ball, and threw it up on high and caught it, and this ball was her
favorite plaything.

```



## Sidef


### Greedy word wrap


```ruby
class String {
    method wrap(width) {
        var txt = self.gsub(/\s+/, " ");
        var len = txt.len;
        var para = [];
        var i = 0;
        while (i < len) {
            var j = (i + width);
            while ((j < len) && (txt.char_at(j) != ' ')) { --j };
            para.append(txt.substr(i, j-i));
            i = j+1;
        };
        return para.join("\n");
    }
}

var text = 'aaa bb cc ddddd';
say text.wrap(6);
```


{{out}}

```txt

aaa bb
cc
ddddd

```



### Smart word wrap


```ruby
class SmartWordWrap {

    has width = 80

    method prepare_words(array, depth=0, callback) {

        var root = []
        var len = 0
        var i = -1

        var limit = array.end
        while (++i <= limit) {
            len += (var word_len = array[i].len)

            if (len > width) {
                if (word_len > width) {
                    len -= word_len
                    array.splice(i, 1, array[i].split(width)...)
                    limit = array.end
                    --i; next
                }
                break
            }

            root << [
                array.first(i+1).join(' '),
                self.prepare_words(array.ft(i+1), depth+1, callback)
            ]

            if (depth.is_zero) {
                callback(root[0])
                root = []
            }

            break if (++len >= width)
        }

        root
    }

    method combine(root, path, callback) {
        var key = path.shift
        path.each { |value|
            root << key
            if (value.is_empty) {
                callback(root)
            }
            else {
                value.each { |item|
                    self.combine(root, item, callback)
                }
            }
            root.pop
        }
    }

    method wrap(text, width) {

        self.width = width
        var words = (text.kind_of(Array) ? text : text.words)

        var best = Hash(
            score => Inf,
            value => [],
        )

        self.prepare_words(words, callback: { |path|
            self.combine([], path, { |combination|
                var score = 0
                combination.ft(0, -2).each { |line|
                    score += (width - line.len -> sqr)
                }

                if (score < best{:score}) {
                    best{:score} = score
                    best{:value} = []+combination
                }
            })
        })

        best{:value}.join("\n")
    }
}
 
var sww = SmartWordWrap();
 
var words = %w(aaa bb cc ddddd);
var wrapped = sww.wrap(words, 6);
 
say wrapped;
```

{{out}}

```txt

aaa
bb cc
ddddd

```



## Tcl

Using a simple greedy algorithm to wrap the same text as used in the [[#Go|Go]] solution. Note that it assumes that the line length is longer than the longest word length.

```tcl
package require Tcl 8.5

proc wrapParagraph {n text} {
    regsub -all {\s+} [string trim $text] " " text
    set RE "^(.{1,$n})(?:\\s+(.*))?$"
    for {set result ""} {[regexp $RE $text -> line text]} {} {
	append result $line "\n"
    }
    return [string trimright $result "\n"]
}

set txt \
"In olden times when wishing still helped one, there lived a king
whose daughters were all beautiful, but the youngest was so beautiful
that the sun itself, which has seen so much, was astonished whenever
it shone in her face.  Close by the king's castle lay a great dark
forest, and under an old lime-tree in the forest was a well, and when
the day was very warm, the king's child went out into the forest and
sat down by the side of the cool fountain, and when she was bored she
took a golden ball, and threw it up on high and caught it, and this
ball was her favorite plaything."

puts "[string repeat - 80]"
puts [wrapParagraph 80 $txt]
puts "[string repeat - 72]"
puts [wrapParagraph 72 $txt]
```

{{out}}

```txt
--------------------------------------------------------------------------------
In olden times when wishing still helped one, there lived a king whose daughters
were all beautiful, but the youngest was so beautiful that the sun itself, which
has seen so much, was astonished whenever it shone in her face. Close by the
king's castle lay a great dark forest, and under an old lime-tree in the forest
was a well, and when the day was very warm, the king's child went out into the
forest and sat down by the side of the cool fountain, and when she was bored she
took a golden ball, and threw it up on high and caught it, and this ball was her
favorite plaything.
------------------------------------------------------------------------
In olden times when wishing still helped one, there lived a king whose
daughters were all beautiful, but the youngest was so beautiful that the
sun itself, which has seen so much, was astonished whenever it shone in
her face. Close by the king's castle lay a great dark forest, and under
an old lime-tree in the forest was a well, and when the day was very
warm, the king's child went out into the forest and sat down by the side
of the cool fountain, and when she was bored she took a golden ball, and
threw it up on high and caught it, and this ball was her favorite
plaything.
```


## TPP

The text presentation program automatically provides word wrap:


```tpp
 The kings youngest daughter was so beautiful that the sun itself, which has seen so much, was astonished whenever it shone in her face.
```



## TUSCRIPT


```tuscript

$$ MODE TUSCRIPT
text="In olden times when wishing still helped one, there lived a king whose daughters were all beautiful, but the youngest was so beautiful that the sun itself, which has seen so much, was astonished whenever it shone in her face.  Close by the king's castle lay a great dark forest, and under an old lime-tree in the forest was a well, and when the day was very warm, the king's child went out into the forest and sat down by the side of the cool fountain, and when she was bored she took a golden ball, and threw it up on high and caught it, and this ball was her favorite plaything."

ERROR/STOP CREATE ("text",seq-E,-std-)

length=80
line=REPEAT ("-",length)
FILE "text" = line
firstline=nextlines=""
wrappedtext=FORMAT(text,length,firstline,nextlines)
FILE "text" = wrappedtext

length=72
line=REPEAT ("-",length)
FILE "text" = line
firstline=CONCAT ("Length ",length,": ")
wrappedtext=FORMAT(text,length,firstline,nextlines)
FILE "text" = wrappedtext

```

{{out}}
<pre style='height:30ex;overflow:scroll'>
--------------------------------------------------------------------------------
In olden times when wishing still helped one, there lived a king whose daughters
were all beautiful, but the youngest was so beautiful that the sun itself, which
has seen so much, was astonished whenever it shone in her face.  Close by the
king's castle lay a great dark forest, and under an old lime-tree in the forest
was a well, and when the day was very warm, the king's child went out into the
forest and sat down by the side of the cool fountain, and when she was bored she
took a golden ball, and threw it up on high and caught it, and this ball was her
favorite plaything.
------------------------------------------------------------------------
Length 72: In olden times when wishing still helped one, there lived a
king whose daughters were all beautiful, but the youngest was so
beautiful that the sun itself, which has seen so much, was astonished
whenever it shone in her face.  Close by the king's castle lay a great
dark forest, and under an old lime-tree in the forest was a well, and
when the day was very warm, the king's child went out into the forest
and sat down by the side of the cool fountain, and when she was bored
she took a golden ball, and threw it up on high and caught it, and this
ball was her favorite plaything.

```



## VBScript


```vb

column = 60
text = "In olden times when wishing still helped one, there lived a king " &_
	"whose daughters were all beautiful, but the youngest was so beautiful "&_
	"that the sun itself, which has seen so much, was astonished whenever "&_
	"it shone-in-her-face.  Close-by-the-king's castle lay a great dark "&_
	"forest, and under an old lime-tree in the forest was a well, and when "&_
	"the day was very warm, the king's child went out into the forest and "&_
	"sat down by the side of the cool-fountain, and when she was bored she "&_
	"took a golden ball, and threw it up on high and caught it, and this "&_
	"ball was her favorite plaything."

Call wordwrap(text,column)

Sub wordwrap(s,n)
	word = Split(s," ")
	row = ""
	For i = 0 To UBound(word)
		If Len(row) = 0 Then
			row = row & word(i)
		ElseIf Len(row & " " & word(i)) <= n Then
			row = row & " " & word(i)
		Else
			WScript.StdOut.WriteLine row
			row = word(i)
		End If
	Next
	If Len(row) > 0 Then
		WScript.StdOut.WriteLine row
	End If
End Sub

```


{{Out}}
Wrapped at 60.

```txt

In olden times when wishing still helped one, there lived a
king whose daughters were all beautiful, but the youngest
was so beautiful that the sun itself, which has seen so
much, was astonished whenever it shone-in-her-face.
Close-by-the-king's castle lay a great dark forest, and
under an old lime-tree in the forest was a well, and when
the day was very warm, the king's child went out into the
forest and sat down by the side of the cool-fountain, and
when she was bored she took a golden ball, and threw it up
on high and caught it, and this ball was her favorite
plaything.

```



## Yabasic


```Yabasic
t$ = "In olden times when wishing still helped one, there lived a king "
t$ = t$ + "whose daughters were all beautiful, but the youngest was so beautiful "
t$ = t$ + "that the sun itself, which has seen so much, was astonished whenever "
t$ = t$ + "it shone in her face.\n\n"
t$ = t$ + t$

t$ = trim$(t$)

input "Width: " width // user specifies width

dim p$(1)

d$ = " .,\n"

n = words(t$, p$(), d$)

for i = 1 to n
  if p$(i) = "\n" or len(docOut$) + len(p$(i)) > width then
    print docOut$
    docOut$ = ""
  end if
  if p$(i) <> "\n" docOut$ = docOut$ + p$(i)
next i
print docOut$

sub words(w$, p$(), d$)
	local n, i, p

	n = split(w$, p$(), d$)
	p = 1

	for i = 1 to n
		p$(i) = p$(i) + mid$(w$, p + len(p$(i)), 1)
		p = p + len(p$(i))
	next i
	return n
end sub
```


{{trans|Run BASIC}}

```Yabasic
t$ = "In olden times when wishing still helped one, there lived a king "
t$ = t$ + "whose daughters were all beautiful, but the youngest was so beautiful "
t$ = t$ + "that the sun itself, which has seen so much, was astonished whenever "
t$ = t$ + "it shone in her face."

wrap$ = " style='white-space: pre-wrap;white-space: -moz-pre-wrap;white-space: -pre-wrap;white-space: -o-pre-wrap;word-wrap: break-word'"

f = open("WordWrap.html", "w")

print #f "<table border=1 cellpadding=2 cellspacing=0><tr" + wrap$ +" valign=top>"
print #f "<td width=60%>" + t$ + "</td><td width=40%>" + t$ + "</td></tr></table>"

close #f

void = system("explorer WordWrap.html")
```



## zkl

This is a greedy algorithm for mono spaced text with ragged right. In addition, it can look at the first two lines to do "smart indenting". It handles multiple paragraphs and a variety of input sources.

```zkl
fcn formatText(text,   // text can be String,Data,File, -->Data
       length=72, calcIndents=True){
   sink:=Data();
   getIndents:='wrap(w){	// look at first two lines to indent paragraph
      reg lines=L(), len=0, prefix="", one=True;
      do(2){
	 if(w._next()){
	    lines.append(line:=w.value);
	    word:=line.split(Void,1)[0,1]; // get first word, if line !blank
	    if(word){
	       p:=line[0,line.find(word[0]]);
	       if(one){ sink.write(p); len=p.len(); one=False; }
	       else prefix=p;
	    }
	 }
      }
      w.push(lines.xplode());  // put first two lines back to be formated
      return(len,prefix);
   };

   reg len=0, prefix="", w=text.walker(1);	// lines
   if(calcIndents) len,prefix=getIndents(w);
   foreach line in (w){
      if(not line.strip()){ // blank line
	 sink.write("\n",line); // blank line redux
	 if(calcIndents) len,prefix=getIndents(w);
	 else len=0;  // restart formating
      }else
	 len=line.split().reduce('wrap(len,word){
	    n:=word.len();
	    if(len==0) { sink.write(word); return(n); }
	    nn:=n+1+len; if(nn<=length) { sink.write(" ",word); return(nn); }
	    sink.write("\n",prefix,word); return(prefix.len()+word.len());
	 },len);
   }
   sink
}
```


```zkl
formatText(File("frog.txt")).text.println();
```

{{out}}

```txt

In olden times when wishing still helped one, there lived a king whose
daughters were all beautiful, but the youngest was so beautiful that the
...

```

Putting a bit of spit shine on the above and editing the source text to look like:

```txt

    In olden
times ...

```


```zkl
[1..].zipWith("%2d: %s".fmt,formatText(File("frog.txt")).walker(1))
.pump(String).println();
```

{{out}}

```txt

 1:      In olden times when wishing still helped one, there lived a king
 2: whose daughters were all beautiful, but the youngest was so beautiful
...
 9: favorite plaything.

```


```zkl
formatText("this\n   is a test foo bar\n\ngreen eggs and spam",10).text.println();
```

{{out}}

```txt

this is a
   test
   foo bar

green eggs
and spam

```

