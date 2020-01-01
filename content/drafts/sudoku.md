+++
title = "Sudoku"
description = ""
date = 2019-09-04T18:45:41Z
aliases = []
[extra]
id = 4567
[taxonomies]
categories = []
tags = []
+++

{{task|Games}}

;Task:
Solve a partially filled-in normal   9x9   [[wp:Sudoku|Sudoku]] grid   and display the result in a human-readable format.


[[wp:Algorithmics_of_sudoku|Algorithmics of Sudoku]]   may help implement this.





## Ada

{{trans|C++}}

```ada

with Ada.Text_IO;

procedure Sudoku is
   type sudoku_ar_t is array ( integer range 0..80 ) of integer range 0..9;
   FINISH_EXCEPTION : exception;

   procedure prettyprint(sudoku_ar: sudoku_ar_t);
   function checkValidity( val : integer; x : integer; y : integer;  sudoku_ar: in  sudoku_ar_t) return Boolean;
   procedure placeNumber(pos: Integer; sudoku_ar: in out sudoku_ar_t);
   procedure solve(sudoku_ar: in out sudoku_ar_t);


   function checkValidity( val : integer; x : integer; y : integer;  sudoku_ar: in  sudoku_ar_t) return Boolean
   is
   begin
      for i in 0..8 loop

         if ( sudoku_ar( y * 9 + i ) = val or sudoku_ar( i * 9 + x ) = val ) then
            return False;
         end if;
      end loop;

      declare
         startX : constant integer := ( x / 3 ) * 3;
         startY : constant integer := ( y / 3 ) * 3;
      begin
         for i in startY..startY+2 loop
            for j in startX..startX+2 loop
               if ( sudoku_ar( i * 9 +j ) = val ) then
                  return False;
               end if;
            end loop;
         end loop;
         return True;
      end;
   end checkValidity;



   procedure placeNumber(pos: Integer; sudoku_ar: in out sudoku_ar_t)
   is
   begin
      if ( pos = 81 ) then
         raise FINISH_EXCEPTION;
      end if;
      if (  sudoku_ar(pos) > 0 ) then
         placeNumber(pos+1, sudoku_ar);
         return;
      end if;
      for n in 1..9 loop
         if( checkValidity( n,  pos mod 9, pos / 9 , sudoku_ar ) ) then
            sudoku_ar(pos) := n;
            placeNumber(pos + 1, sudoku_ar );
            sudoku_ar(pos) := 0;
         end if;
      end loop;
   end placeNumber;


   procedure solve(sudoku_ar: in out sudoku_ar_t)
   is
   begin
      placeNumber( 0, sudoku_ar );
      Ada.Text_IO.Put_Line("Unresolvable !");
   exception
      when FINISH_EXCEPTION =>
         Ada.Text_IO.Put_Line("Finished !");
         prettyprint(sudoku_ar);
   end solve;




   procedure prettyprint(sudoku_ar: sudoku_ar_t)
   is
      line_sep   : constant String  := "------+------+------";
   begin
      for i in sudoku_ar'Range loop
         Ada.Text_IO.Put(sudoku_ar(i)'Image);
         if (i+1) mod 3 = 0 and not((i+1) mod 9 = 0) then
            Ada.Text_IO.Put("|");
         end if;
         if (i+1) mod 9 = 0 then
            Ada.Text_IO.Put_Line("");
         end if;
         if (i+1) mod 27 = 0 then
            Ada.Text_IO.Put_Line(line_sep);
         end if;
      end loop;
   end prettyprint;


   sudoku_ar : sudoku_ar_t :=
     (
      8,5,0,0,0,2,4,0,0,
      7,2,0,0,0,0,0,0,9,
      0,0,4,0,0,0,0,0,0,
      0,0,0,1,0,7,0,0,2,
      3,0,5,0,0,0,9,0,0,
      0,4,0,0,0,0,0,0,0,
      0,0,0,0,8,0,0,7,0,
      0,1,7,0,0,0,0,0,0,
      0,0,0,0,3,6,0,4,0
     );

begin
   solve( sudoku_ar );
end Sudoku;

```


{{out}}

```txt

Finished !
 8 5 9| 6 1 2| 4 3 7
 7 2 3| 8 5 4| 1 6 9
 1 6 4| 3 7 9| 5 2 8
------+------+------
 9 8 6| 1 4 7| 3 5 2
 3 7 5| 2 6 8| 9 1 4
 2 4 1| 5 9 3| 7 8 6
------+------+------
 4 3 2| 9 8 1| 6 7 5
 6 1 7| 4 2 5| 8 9 3
 5 9 8| 7 3 6| 2 4 1

```



## ALGOL 68

{{trans|D}} Note: This specimen retains the original [[#D|D]] coding style.
{{works with|ALGOL 68|Revision 1 - no extensions to language used.}}
{{works with|ALGOL 68G|Any - tested with release [http://sourceforge.net/projects/algol68/files/algol68g/algol68g-1.18.0/algol68g-1.18.0-9h.tiny.el5.centos.fc11.i386.rpm/download 1.18.0-9h.tiny].}}
{{wont work with|ELLA ALGOL 68|Any (with appropriate job cards) - tested with release [http://sourceforge.net/projects/algol68/files/algol68toc/algol68toc-1.8.8d/algol68toc-1.8-8d.fc9.i386.rpm/download 1.8-8d] - due to extensive use of '''format'''[ted] ''transput''.}}

```algol68
MODE AVAIL = [9]BOOL;
MODE BOX = [3, 3]CHAR;

FORMAT row fmt = $"|"3(" "3(g" ")"|")l$;
FORMAT line = $"+"3(7"-","+")l$;
FORMAT puzzle fmt = $f(line)3(3(f(row fmt))f(line))$;

AVAIL gen full = (TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE);

OP REPR = (AVAIL avail)STRING: (
  STRING out := "";
  FOR i FROM LWB avail TO UPB avail DO
    IF avail[i] THEN out +:= REPR(ABS "0" + i) FI
  OD;
  out
);

CHAR empty = "_";

OP -:= = (REF AVAIL set, CHAR index)VOID: (
  set[ABS index - ABS "0"]:=FALSE
);

#  these two functions assume that the number has not already been found #
PROC avail slice = (REF[]CHAR slice, REF AVAIL available)REF AVAIL:(
        FOR ele FROM LWB slice TO UPB slice DO
                IF slice[ele] /= empty THEN available-:=slice[ele] FI
        OD;
        available
);

PROC avail box = (INT x, y, REF AVAIL available)REF AVAIL:(
        #  x designates row, y designates column #
        #  get a base index for the boxes #
        INT bx := x - (x-1) MOD 3;
        INT by := y - (y-1) MOD 3;
        REF BOX box = puzzle[bx:bx+2, by:by+2];
        FOR i FROM LWB box TO UPB box DO
          FOR j FROM 2 LWB box TO 2 UPB box DO
                IF box[i, j] /= empty THEN available-:=box[i, j] FI
          OD
        OD;
        available
);

[9, 9]CHAR puzzle;
PROC solve = ([,]CHAR in puzzle)VOID:(
        puzzle := in puzzle;
        TO UPB puzzle UP 2 DO
                BOOL done := TRUE;
                FOR i FROM LWB puzzle TO UPB puzzle DO
                  FOR j FROM 2 LWB puzzle TO 2 UPB puzzle DO
                    CHAR ele := puzzle[i, j];
                    IF ele = empty THEN
                        #  poke at the elements that are "_" #
                        AVAIL remaining := avail box(i, j,
                                           avail slice(puzzle[i, ],
                                           avail slice(puzzle[, j],
                                           LOC AVAIL := gen full)));
                        STRING s = REPR remaining;
                        IF UPB s = 1 THEN puzzle[i, j] := s[LWB s]
                        ELSE done := FALSE
                        FI
                    FI
                  OD
                OD;
                IF done THEN break FI
        OD;
break:
        #  write out completed puzzle #
        printf(($gl$, "Completed puzzle:"));
        printf((puzzle fmt, puzzle))
);
main:(
   solve(("394__267_",
          "___3__4__",
          "5__69__2_",
          "_45___9__",
          "6_______7",
          "__7___58_",
          "_1__67__8",
          "__9__8___",
          "_264__735"))
CO # note: This codes/algorithm does not [yet] solve: #
   solve(("9__2__5__",
          "_4__6__3_",
          "__3_____6",
          "___9__2__",
          "____5__8_",
          "__7__4__3",
          "7_____1__",
          "_5__2__4_",
          "__1__6__9"))
END CO
)
```

{{out}}

```txt

Completed puzzle:
+-------+-------+-------+
| 3 9 4 | 8 5 2 | 6 7 1 |
| 2 6 8 | 3 7 1 | 4 5 9 |
| 5 7 1 | 6 9 4 | 8 2 3 |
+-------+-------+-------+
| 1 4 5 | 7 8 3 | 9 6 2 |
| 6 8 2 | 9 4 5 | 3 1 7 |
| 9 3 7 | 1 2 6 | 5 8 4 |
+-------+-------+-------+
| 4 1 3 | 5 6 7 | 2 9 8 |
| 7 5 9 | 2 3 8 | 1 4 6 |
| 8 2 6 | 4 1 9 | 7 3 5 |
+-------+-------+-------+

```



## AutoHotkey


```AutoHotkey
#SingleInstance, Force
SetBatchLines, -1
SetTitleMatchMode, 3

    Loop 9 {
       r := A_Index, y := r*17-8 + (A_Index >= 7 ? 4 : A_Index >= 4 ? 2 : 0)
       Loop 9 {
          c := A_Index, x := c*17+5 + (A_Index >= 7 ? 4 : A_Index >= 4 ? 2 : 0)
          Gui, Add, Edit, x%x% y%y% w17 h17 v%r%_%c% Center Number Limit1 gNext
       }
    }
    Gui, Add, Button, vButton gSolve w175 x10 Center, Solve
    Gui, Add, Text, vMsg r3, Enter Sudoku puzzle and click Solve
    Gui, Show,, Sudoku Solver
Return

Solve:
    Gui, Submit, NoHide
    Loop 9
    {
       r := A_Index
       Loop 9
          If (%r%_%A_Index% = "")
             puzzle .= "@"
          Else
             puzzle .= %r%_%A_Index%
    }
    s := A_TickCount
    answer := Sudoku(puzzle)
    iterations := ErrorLevel
    e := A_TickCount
    seconds := (e-s)/1000
    StringSplit, a, answer, |
    Loop 9
    {
       r := A_Index
       Loop 9
       {
          b := (r*9)+A_Index-9
          GuiControl,, %r%_%A_Index%, % a%b%
          GuiControl, +ReadOnly, %r%_%A_Index%
        }
    }
    if answer
        GuiControl,, Msg, Solved!`nTime: %seconds%s`nIterations: %iterations%
    else
        GuiControl,, Msg, Failed! :(`nTime: %seconds%s`nIterations: %iterations%
    GuiControl,, Button, Again!
    GuiControl, +gAgain, Button
return

GuiClose:
    ExitApp

Again:
    Reload

#IfWinActive, Sudoku Solver
~*Enter::GoSub % GetKeyState( "Shift", "P" ) ? "~Up" : "~Down"
~Up::
    GuiControlGet, f, focus
    StringTrimLeft, f, f, 4
    f := ((f >= 1 && f <= 9) ? f+72 : f-9)
    GuiControl, Focus, Edit%f%
return
~Down::
    GuiControlGet, f, focus
    StringTrimLeft, f, f, 4
    f := ((f >= 73 && f <= 81) ? f-72 : f + 9)
    GuiControl, Focus, Edit%f%
return
~Left::
    GuiControlGet, f, focus
    StringTrimLeft, f, f, 4
    f := Mod(f + 79, 81) + 1
    GuiControl, Focus, Edit%f%
return
Next:
~Right::
    GuiControlGet, f, focus
    StringTrimLeft, f, f, 4
    f := Mod(f, 81) + 1
    GuiControl, Focus, Edit%f%
return
#IfWinActive

; Functions Start here

Sudoku( p ) { ;ErrorLevel contains the number of iterations
   p := RegExReplace(p, "[^1-9@]"), ErrorLevel := 0 ;format puzzle as single line string
   return Sudoku_Display(Sudoku_Solve(p))
}

Sudoku_Solve( p, d = 0 ) { ;d is 0-based
;   http://www.autohotkey.com/forum/topic46679.html
;   p: 81 character puzzle string
;      (concat all 9 rows of 9 chars each)
;      givens represented as chars 1-9
;      fill-ins as any non-null, non 1-9 char
;   d: used internally. omit on initial call
;
;   returns: 81 char string with non-givens replaced with valid solution
;
   If (d >= 81), ErrorLevel++
      return p  ;this is 82nd iteration, so it has successfully finished iteration 81
   If InStr( "123456789", SubStr(p, d+1, 1) ) ;this depth is a given, skip through
      return Sudoku_Solve(p, d+1)
   m := Sudoku_Constraints(p,d) ;a string of this level's constraints.
   ; (these will not change for all 9 loops)
   Loop 9
   {
      If InStr(m, A_Index)
         Continue
      NumPut(Asc(A_Index), p, d, "Char")
      If r := Sudoku_Solve(p, d+1)
         return r
   }
   return 0
}

Sudoku_Constraints( ByRef p, d ) {
; returns a string of the constraints for a particular position
     c := Mod(d,9)
   , r := (d - c) // 9
   , b := r//3*27 + c//3*3 + 1
   ;convert to 1-based
   , c++
   return ""
   ; row:
      . SubStr(p, r * 9 + 1, 9)
   ; column:
      . SubStr(p,c   ,1) SubStr(p,c+9 ,1) SubStr(p,c+18,1)
      . SubStr(p,c+27,1) SubStr(p,c+36,1) SubStr(p,c+45,1)
      . SubStr(p,c+54,1) SubStr(p,c+63,1) SubStr(p,c+72,1)
   ;box
      . SubStr(p, b, 3) SubStr(p, b+9, 3) SubStr(p, b+18, 3)
}

Sudoku_Display( p ) {
   If StrLen(p) = 81
      loop 81
         r .= SubStr(p, A_Index, 1) . "|"
   return r
}
```



## BBC BASIC

{{works with|BBC BASIC for Windows}}
[[Image:sudoku_bbc.gif|right]]

```bbcbasic
      VDU 23,22,453;453;8,20,16,128
      *FONT Arial,28

      DIM Board%(8,8)
      Board%() = %111111111

      FOR L% = 0 TO 9:P% = L%*100
        LINE 2,P%+2,902,P%+2
        IF (L% MOD 3)=0 LINE 2,P%,902,P% : LINE 2,P%+4,902,P%+4
        LINE P%+2,2,P%+2,902
        IF (L% MOD 3)=0 LINE P%,2,P%,902 : LINE P%+4,2,P%+4,902
      NEXT

      DATA "  4 5  6 "
      DATA " 6 1  8 9"
      DATA "3    7   "
      DATA " 8    5  "
      DATA "   4 3   "
      DATA "  6    7 "
      DATA "   2    6"
      DATA "1 5  4 3 "
      DATA " 2  7 1  "

      FOR R% = 8 TO 0 STEP -1
        READ A$
        FOR C% = 0 TO 8
          A% = ASCMID$(A$,C%+1) AND 15
          IF A% Board%(R%,C%) = 1 << (A%-1)
        NEXT
      NEXT R%

      GCOL 4
      PROCshow
      WAIT 200
      dummy% = FNsolve(Board%(), TRUE)
      GCOL 2
      PROCshow
      REPEAT WAIT 1 : UNTIL FALSE
      END

      DEF PROCshow
      LOCAL C%,P%,R%
      FOR C% = 0 TO 8
        FOR R% = 0 TO 8
          P% = Board%(R%,C%)
          IF (P% AND (P%-1)) = 0 THEN
            IF P% P% = LOGP%/LOG2+1.5
            MOVE C%*100+30,R%*100+90
            VDU 5,P%+48,4
          ENDIF
        NEXT
      NEXT
      ENDPROC

      DEF FNsolve(P%(),F%)
      LOCAL C%,D%,M%,N%,R%,X%,Y%,Q%()
      DIM Q%(8,8)
      REPEAT
        Q%() = P%()
        FOR R% = 0 TO 8
          FOR C% = 0 TO 8
            D% = P%(R%,C%)
            IF (D% AND (D%-1))=0 THEN
              M% = NOT D%
              FOR X% = 0 TO 8
                IF X%<>C% P%(R%,X%) AND= M%
                IF X%<>R% P%(X%,C%) AND= M%
              NEXT
              FOR X% = C%DIV3*3 TO C%DIV3*3+2
                FOR Y% = R%DIV3*3 TO R%DIV3*3+2
                  IF X%<>C% IF Y%<>R% P%(Y%,X%) AND= M%
                NEXT
              NEXT
            ENDIF
          NEXT
        NEXT
        Q%() -= P%()
      UNTIL SUMQ%()=0
      M% = 10
      FOR R% = 0 TO 8
        FOR C% = 0 TO 8
          D% = P%(R%,C%)
          IF D%=0 M% = 0
          IF D% AND (D%-1) THEN
            N% = 0
            REPEAT N% += D% AND 1
              D% DIV= 2
            UNTIL D% = 0
            IF N%<M% M% = N% : X% = C% : Y% = R%
          ENDIF
        NEXT
      NEXT
      IF M%=0 THEN = 0
      IF M%=10 THEN = 1
      D% = 0
      FOR M% = 0 TO 8
        IF P%(Y%,X%) AND (2^M%) THEN
          Q%() = P%()
          Q%(Y%,X%) = 2^M%
          C% = FNsolve(Q%(),F%)
          D% += C%
          IF C% IF F% P%() = Q%() : = D%
        ENDIF
      NEXT
      = D%
```



## BCPL


```BCPL
// This can be run using Cintcode BCPL freely available from www.cl.cam.ac.uk/users/mr10.
// Implemented by Martin Richards.

// This is a really naive program to solve SuDoku problems. Even so it is usually quite fast.

// SuDoku consists of a 9x9 grid of cells. Each cell should contain
// a digit in the range 1..9. Every row, column and major 3x3
// square should contain all the digits 1..9. Some cells have
// given values. The problem is to find digits to place in
// the unspecified cells satisfying the constraints.

// A typical problem is:

//  - - -   6 3 8   - - -
//  7 - 6   - - -   3 - 5
//  - 1 -   - - -   - 4 -

//  - - 8   7 1 2   4 - -
//  - 9 -   - - -   - 5 -
//  - - 2   5 6 9   1 - -

//  - 3 -   - - -   - 1 -
//  1 - 5   - - -   6 - 8
//  - - -   1 8 4   - - -

SECTION "sudoku"

GET "libhdr"

GLOBAL { count:ug

// The 9x9 board

a1; a2; a3; a4; a5; a6; a7; a8; a9
b1; b2; b3; b4; b5; b6; b7; b8; b9
c1; c2; c3; c4; c5; c6; c7; c8; c9
d1; d2; d3; d4; d5; d6; d7; d8; d9
e1; e2; e3; e4; e5; e6; e7; e8; e9
f1; f2; f3; f4; f5; f6; f7; f8; f9
g1; g2; g3; g4; g5; g6; g7; g8; g9
h1; h2; h3; h4; h5; h6; h7; h8; h9
i1; i2; i3; i4; i5; i6; i7; i8; i9
}

MANIFEST {
N1=1<<0; N2=1<<1; N3=1<<2;
N4=1<<3; N5=1<<4; N6=1<<5;
N7=1<<6; N8=1<<7; N9=1<<8
}

LET start() = VALOF
{ count := 0
  initboard()
  prboard()
  ta1()
  writef("*n*nTotal number of solutions: %n*n", count)
  RESULTIS 0
}

AND initboard() BE {
a1, a2, a3, a4, a5, a6, a7, a8, a9 :=  0, 0, 0, N6,N3,N8,  0, 0, 0
b1, b2, b3, b4, b5, b6, b7, b8, b9 := N7, 0,N6,  0, 0, 0, N3, 0,N5
c1, c2, c3, c4, c5, c6, c7, c8, c9 :=  0,N1, 0,  0, 0, 0,  0,N4, 0
d1, d2, d3, d4, d5, d6, d7, d8, d9 :=  0, 0,N8, N7,N1,N2, N4, 0, 0
e1, e2, e3, e4, e5, e6, e7, e8, e9 :=  0,N9, 0,  0, 0, 0,  0,N5, 0
f1, f2, f3, f4, f5, f6, f7, f8, f9 :=  0, 0,N2, N5,N6,N9, N1, 0, 0
g1, g2, g3, g4, g5, g6, g7, g8, g9 :=  0,N3, 0,  0, 0, 0,  0,N1, 0
h1, h2, h3, h4, h5, h6, h7, h8, h9 := N1, 0,N5,  0, 0, 0, N6, 0,N8
i1, i2, i3, i4, i5, i6, i7, i8, i9 :=  0, 0, 0, N1,N8,N4,  0, 0, 0

// Un-comment the following to test that the backtracking works
// giving 184 solutions.
//h1, h2, h3, h4, h5, h6, h7, h8, h9 := N1, 0,N5,  0, 0, 0, N6, 0, 0
//i1, i2, i3, i4, i5, i6, i7, i8, i9 :=  0, 0, 0,  0, 0, 0,  0, 0, 0
}

AND c(n) = VALOF SWITCHON n INTO
{ DEFAULT:    RESULTIS '?'
  CASE  0:    RESULTIS '-'
  CASE N1:    RESULTIS '1'
  CASE N2:    RESULTIS '2'
  CASE N3:    RESULTIS '3'
  CASE N4:    RESULTIS '4'
  CASE N5:    RESULTIS '5'
  CASE N6:    RESULTIS '6'
  CASE N7:    RESULTIS '7'
  CASE N8:    RESULTIS '8'
  CASE N9:    RESULTIS '9'
}

AND prboard() BE
{ LET form = "%c %c %c   %c %c %c   %c %c %c*n"
  writef("*ncount = %n*n", count)
  newline()
  writef(form, c(a1),c(a2),c(a3),c(a4),c(a5),c(a6),c(a7),c(a8),c(a9))
  writef(form, c(b1),c(b2),c(b3),c(b4),c(b5),c(b6),c(b7),c(b8),c(b9))
  writef(form, c(c1),c(c2),c(c3),c(c4),c(c5),c(c6),c(c7),c(c8),c(c9))
  newline()
  writef(form, c(d1),c(d2),c(d3),c(d4),c(d5),c(d6),c(d7),c(d8),c(d9))
  writef(form, c(e1),c(e2),c(e3),c(e4),c(e5),c(e6),c(e7),c(e8),c(e9))
  writef(form, c(f1),c(f2),c(f3),c(f4),c(f5),c(f6),c(f7),c(f8),c(f9))
  newline()
  writef(form, c(g1),c(g2),c(g3),c(g4),c(g5),c(g6),c(g7),c(g8),c(g9))
  writef(form, c(h1),c(h2),c(h3),c(h4),c(h5),c(h6),c(h7),c(h8),c(h9))
  writef(form, c(i1),c(i2),c(i3),c(i4),c(i5),c(i6),c(i7),c(i8),c(i9))

  newline()

//abort(1000)
}

AND try(p, f, row, col, sq) BE
{ LET x = !p
  TEST x
  THEN f()
  ELSE { LET bits = row|col|sq
//prboard()
//              writef("x=%n %b9*n", x, bits)
//abort(1000)
         IF (N1&bits)=0 DO { !p:=N1; f() }
         IF (N2&bits)=0 DO { !p:=N2; f() }
         IF (N3&bits)=0 DO { !p:=N3; f() }
         IF (N4&bits)=0 DO { !p:=N4; f() }
         IF (N5&bits)=0 DO { !p:=N5; f() }
         IF (N6&bits)=0 DO { !p:=N6; f() }
         IF (N7&bits)=0 DO { !p:=N7; f() }
         IF (N8&bits)=0 DO { !p:=N8; f() }
         IF (N9&bits)=0 DO { !p:=N9; f() }
         !p := 0
       }
}

AND ta1() BE try(@a1, ta2, a1+a2+a3+a4+a5+a6+a7+a8+a9,
                           a1+b1+c1+d1+e1+f1+g1+h1+i1,
                           a1+a2+a3+b1+b2+b3+c1+c2+c3)
AND ta2() BE try(@a2, ta3, a1+a2+a3+a4+a5+a6+a7+a8+a9,
                           a2+b2+c2+d2+e2+f2+g2+h2+i2,
                           a1+a2+a3+b1+b2+b3+c1+c2+c3)
AND ta3() BE try(@a3, ta4, a1+a2+a3+a4+a5+a6+a7+a8+a9,
                           a3+b3+c3+d3+e3+f3+g3+h3+i3,
                           a1+a2+a3+b1+b2+b3+c1+c2+c3)
AND ta4() BE try(@a4, ta5, a1+a2+a3+a4+a5+a6+a7+a8+a9,
                           a4+b4+c4+d4+e4+f4+g4+h4+i4,
                           a4+a5+a6+b4+b5+b6+c4+c5+c6)
AND ta5() BE try(@a5, ta6, a1+a2+a3+a4+a5+a6+a7+a8+a9,
                           a5+b5+c5+d5+e5+f5+g5+h5+i5,
                           a4+a5+a6+b4+b5+b6+c4+c5+c6)
AND ta6() BE try(@a6, ta7, a1+a2+a3+a4+a5+a6+a7+a8+a9,
                           a6+b6+c6+d6+e6+f6+g6+h6+i6,
                           a4+a5+a6+b4+b5+b6+c4+c5+c6)
AND ta7() BE try(@a7, ta8, a1+a2+a3+a4+a5+a6+a7+a8+a9,
                           a7+b7+c7+d7+e7+f7+g7+h7+i7,
                           a7+a8+a9+b7+b8+b9+c7+c8+c9)
AND ta8() BE try(@a8, ta9, a1+a2+a3+a4+a5+a6+a7+a8+a9,
                           a8+b8+c8+d8+e8+f8+g8+h8+i8,
                           a7+a8+a9+b7+b8+b9+c7+c8+c9)
AND ta9() BE try(@a9, tb1, a1+a2+a3+a4+a5+a6+a7+a8+a9,
                           a9+b9+c9+d9+e9+f9+g9+h9+i9,
                           a7+a8+a9+b7+b8+b9+c7+c8+c9)

AND tb1() BE try(@b1, tb2, b1+b2+b3+b4+b5+b6+b7+b8+b9,
                           a1+b1+c1+d1+e1+f1+g1+h1+i1,
                           a1+a2+a3+b1+b2+b3+c1+c2+c3)
AND tb2() BE try(@b2, tb3, b1+b2+b3+b4+b5+b6+b7+b8+b9,
                           a2+b2+c2+d2+e2+f2+g2+h2+i2,
                           a1+a2+a3+b1+b2+b3+c1+c2+c3)
AND tb3() BE try(@b3, tb4, b1+b2+b3+b4+b5+b6+b7+b8+b9,
                           a3+b3+c3+d3+e3+f3+g3+h3+i3,
                           a1+a2+a3+b1+b2+b3+c1+c2+c3)
AND tb4() BE try(@b4, tb5, b1+b2+b3+b4+b5+b6+b7+b8+b9,
                           a4+b4+c4+d4+e4+f4+g4+h4+i4,
                           a4+a5+a6+b4+b5+b6+c4+c5+c6)
AND tb5() BE try(@b5, tb6, b1+b2+b3+b4+b5+b6+b7+b8+b9,
                           a5+b5+c5+d5+e5+f5+g5+h5+i5,
                           a4+a5+a6+b4+b5+b6+c4+c5+c6)
AND tb6() BE try(@b6, tb7, b1+b2+b3+b4+b5+b6+b7+b8+b9,
                           a6+b6+c6+d6+e6+f6+g6+h6+i6,
                           a4+a5+a6+b4+b5+b6+c4+c5+c6)
AND tb7() BE try(@b7, tb8, b1+b2+b3+b4+b5+b6+b7+b8+b9,
                           a7+b7+c7+d7+e7+f7+g7+h7+i7,
                           a7+a8+a9+b7+b8+b9+c7+c8+c9)
AND tb8() BE try(@b8, tb9, b1+b2+b3+b4+b5+b6+b7+b8+b9,
                           a8+b8+c8+d8+e8+f8+g8+h8+i8,
                           a7+a8+a9+b7+b8+b9+c7+c8+c9)
AND tb9() BE try(@b9, tc1, b1+b2+b3+b4+b5+b6+b7+b8+b9,
                           a9+b9+c9+d9+e9+f9+g9+h9+i9,
                           a7+a8+a9+b7+b8+b9+c7+c8+c9)

AND tc1() BE try(@c1, tc2, c1+c2+c3+c4+c5+c6+c7+c8+c9,
                           a1+b1+c1+d1+e1+f1+g1+h1+i1,
                           a1+a2+a3+b1+b2+b3+c1+c2+c3)
AND tc2() BE try(@c2, tc3, c1+c2+c3+c4+c5+c6+c7+c8+c9,
                           a2+b2+c2+d2+e2+f2+g2+h2+i2,
                           a1+a2+a3+b1+b2+b3+c1+c2+c3)
AND tc3() BE try(@c3, tc4, c1+c2+c3+c4+c5+c6+c7+c8+c9,
                           a3+b3+c3+d3+e3+f3+g3+h3+i3,
                           a1+a2+a3+b1+b2+b3+c1+c2+c3)
AND tc4() BE try(@c4, tc5, c1+c2+c3+c4+c5+c6+c7+c8+c9,
                           a4+b4+c4+d4+e4+f4+g4+h4+i4,
                           a4+a5+a6+b4+b5+b6+c4+c5+c6)
AND tc5() BE try(@c5, tc6, c1+c2+c3+c4+c5+c6+c7+c8+c9,
                           a5+b5+c5+d5+e5+f5+g5+h5+i5,
                           a4+a5+a6+b4+b5+b6+c4+c5+c6)
AND tc6() BE try(@c6, tc7, c1+c2+c3+c4+c5+c6+c7+c8+c9,
                           a6+b6+c6+d6+e6+f6+g6+h6+i6,
                           a4+a5+a6+b4+b5+b6+c4+c5+c6)
AND tc7() BE try(@c7, tc8, c1+c2+c3+c4+c5+c6+c7+c8+c9,
                           a7+b7+c7+d7+e7+f7+g7+h7+i7,
                           a7+a8+a9+b7+b8+b9+c7+c8+c9)
AND tc8() BE try(@c8, tc9, c1+c2+c3+c4+c5+c6+c7+c8+c9,
                           a8+b8+c8+d8+e8+f8+g8+h8+i8,
                           a7+a8+a9+b7+b8+b9+c7+c8+c9)
AND tc9() BE try(@c9, td1, c1+c2+c3+c4+c5+c6+c7+c8+c9,
                           a9+b9+c9+d9+e9+f9+g9+h9+i9,
                           a7+a8+a9+b7+b8+b9+c7+c8+c9)

AND td1() BE try(@d1, td2, d1+d2+d3+d4+d5+d6+d7+d8+d9,
                           a1+b1+c1+d1+e1+f1+g1+h1+i1,
                           d1+d2+d3+e1+e2+e3+f1+f2+f3)
AND td2() BE try(@d2, td3, d1+d2+d3+d4+d5+d6+d7+d8+d9,
                           a2+b2+c2+d2+e2+f2+g2+h2+i2,
                           d1+d2+d3+e1+e2+e3+f1+f2+f3)
AND td3() BE try(@d3, td4, d1+d2+d3+d4+d5+d6+d7+d8+d9,
                           a3+b3+c3+d3+e3+f3+g3+h3+i3,
                           d1+d2+d3+e1+e2+e3+f1+f2+f3)
AND td4() BE try(@d4, td5, d1+d2+d3+d4+d5+d6+d7+d8+d9,
                           a4+b4+c4+d4+e4+f4+g4+h4+i4,
                           d4+d5+d6+e4+e5+e6+f4+f5+f6)
AND td5() BE try(@d5, td6, d1+d2+d3+d4+d5+d6+d7+d8+d9,
                           a5+b5+c5+d5+e5+f5+g5+h5+i5,
                           d4+d5+d6+e4+e5+e6+f4+f5+f6)
AND td6() BE try(@d6, td7, d1+d2+d3+d4+d5+d6+d7+d8+d9,
                           a6+b6+c6+d6+e6+f6+g6+h6+i6,
                           d4+d5+d6+e4+e5+e6+f4+f5+f6)
AND td7() BE try(@d7, td8, d1+d2+d3+d4+d5+d6+d7+d8+d9,
                           a7+b7+c7+d7+e7+f7+g7+h7+i7,
                           d7+d8+d9+e7+e8+e9+f7+f8+f9)
AND td8() BE try(@d8, td9, d1+d2+d3+d4+d5+d6+d7+d8+d9,
                           a8+b8+c8+d8+e8+f8+g8+h8+i8,
                           d7+d8+d9+e7+e8+e9+f7+f8+f9)
AND td9() BE try(@d9, te1, d1+d2+d3+d4+d5+d6+d7+d8+d9,
                           a9+b9+c9+d9+e9+f9+g9+h9+i9,
                           d7+d8+d9+e7+e8+e9+f7+f8+f9)

AND te1() BE try(@e1, te2, e1+e2+e3+e4+e5+e6+e7+e8+e9,
                           a1+b1+c1+d1+e1+f1+g1+h1+i1,
                           d1+d2+d3+e1+e2+e3+f1+f2+f3)
AND te2() BE try(@e2, te3, e1+e2+e3+e4+e5+e6+e7+e8+e9,
                           a2+b2+c2+d2+e2+f2+g2+h2+i2,
                           d1+d2+d3+e1+e2+e3+f1+f2+f3)
AND te3() BE try(@e3, te4, e1+e2+e3+e4+e5+e6+e7+e8+e9,
                           a3+b3+c3+d3+e3+f3+g3+h3+i3,
                           d1+d2+d3+e1+e2+e3+f1+f2+f3)
AND te4() BE try(@e4, te5, e1+e2+e3+e4+e5+e6+e7+e8+e9,
                           a4+b4+c4+d4+e4+f4+g4+h4+i4,
                           d4+d5+d6+e4+e5+e6+f4+f5+f6)
AND te5() BE try(@e5, te6, e1+e2+e3+e4+e5+e6+e7+e8+e9,
                           a5+b5+c5+d5+e5+f5+g5+h5+i5,
                           d4+d5+d6+e4+e5+e6+f4+f5+f6)
AND te6() BE try(@e6, te7, e1+e2+e3+e4+e5+e6+e7+e8+e9,
                           a6+b6+c6+d6+e6+f6+g6+h6+i6,
                           d4+d5+d6+e4+e5+e6+f4+f5+f6)
AND te7() BE try(@e7, te8, e1+e2+e3+e4+e5+e6+e7+e8+e9,
                           a7+b7+c7+d7+e7+f7+g7+h7+i7,
                           d7+d8+d9+e7+e8+e9+f7+f8+f9)
AND te8() BE try(@e8, te9, e1+e2+e3+e4+e5+e6+e7+e8+e9,
                           a8+b8+c8+d8+e8+f8+g8+h8+i8,
                           d7+d8+d9+e7+e8+e9+f7+f8+f9)
AND te9() BE try(@e9, tf1, e1+e2+e3+e4+e5+e6+e7+e8+e9,
                           a9+b9+c9+d9+e9+f9+g9+h9+i9,
                           d7+d8+d9+e7+e8+e9+f7+f8+f9)

AND tf1() BE try(@f1, tf2, f1+f2+f3+f4+f5+f6+f7+f8+f9,
                           a1+b1+c1+d1+e1+f1+g1+h1+i1,
                           d1+d2+d3+e1+e2+e3+f1+f2+f3)
AND tf2() BE try(@f2, tf3, f1+f2+f3+f4+f5+f6+f7+f8+f9,
                           a2+b2+c2+d2+e2+f2+g2+h2+i2,
                           d1+d2+d3+e1+e2+e3+f1+f2+f3)
AND tf3() BE try(@f3, tf4, f1+f2+f3+f4+f5+f6+f7+f8+f9,
                           a3+b3+c3+d3+e3+f3+g3+h3+i3,
                           d1+d2+d3+e1+e2+e3+f1+f2+f3)
AND tf4() BE try(@f4, tf5, f1+f2+f3+f4+f5+f6+f7+f8+f9,
                           a4+b4+c4+d4+e4+f4+g4+h4+i4,
                           d4+d5+d6+e4+e5+e6+f4+f5+f6)
AND tf5() BE try(@f5, tf6, f1+f2+f3+f4+f5+f6+f7+f8+f9,
                           a5+b5+c5+d5+e5+f5+g5+h5+i5,
                           d4+d5+d6+e4+e5+e6+f4+f5+f6)
AND tf6() BE try(@f6, tf7, f1+f2+f3+f4+f5+f6+f7+f8+f9,
                           a6+b6+c6+d6+e6+f6+g6+h6+i6,
                           d4+d5+d6+e4+e5+e6+f4+f5+f6)
AND tf7() BE try(@f7, tf8, f1+f2+f3+f4+f5+f6+f7+f8+f9,
                           a7+b7+c7+d7+e7+f7+g7+h7+i7,
                           d7+d8+d9+e7+e8+e9+f7+f8+f9)
AND tf8() BE try(@f8, tf9, f1+f2+f3+f4+f5+f6+f7+f8+f9,
                           a8+b8+c8+d8+e8+f8+g8+h8+i8,
                           d7+d8+d9+e7+e8+e9+f7+f8+f9)
AND tf9() BE try(@f9, tg1, f1+f2+f3+f4+f5+f6+f7+f8+f9,
                           a9+b9+c9+d9+e9+f9+g9+h9+i9,
                           d7+d8+d9+e7+e8+e9+f7+f8+f9)

AND tg1() BE try(@g1, tg2, g1+g2+g3+g4+g5+g6+g7+g8+g9,
                           a1+b1+c1+d1+e1+f1+g1+h1+i1,
                           g1+g2+g3+h1+h2+h3+i1+i2+i3)
AND tg2() BE try(@g2, tg3, g1+g2+g3+g4+g5+g6+g7+g8+g9,
                           a2+b2+c2+d2+e2+f2+g2+h2+i2,
                           g1+g2+g3+h1+h2+h3+i1+i2+i3)
AND tg3() BE try(@g3, tg4, g1+g2+g3+g4+g5+g6+g7+g8+g9,
                           a3+b3+c3+d3+e3+f3+g3+h3+i3,
                           g1+g2+g3+h1+h2+h3+i1+i2+i3)
AND tg4() BE try(@g4, tg5, g1+g2+g3+g4+g5+g6+g7+g8+g9,
                           a4+b4+c4+d4+e4+f4+g4+h4+i4,
                           g4+g5+g6+h4+h5+h6+i4+i5+i6)
AND tg5() BE try(@g5, tg6, g1+g2+g3+g4+g5+g6+g7+g8+g9,
                           a5+b5+c5+d5+e5+f5+g5+h5+i5,
                           g4+g5+g6+h4+h5+h6+i4+i5+i6)
AND tg6() BE try(@g6, tg7, g1+g2+g3+g4+g5+g6+g7+g8+g9,
                           a6+b6+c6+d6+e6+f6+g6+h6+i6,
                           g4+g5+g6+h4+h5+h6+i4+i5+i6)
AND tg7() BE try(@g7, tg8, g1+g2+g3+g4+g5+g6+g7+g8+g9,
                           a7+b7+c7+d7+e7+f7+g7+h7+i7,
                           g7+g8+g9+h7+h8+h9+i7+i8+i9)
AND tg8() BE try(@g8, tg9, g1+g2+g3+g4+g5+g6+g7+g8+g9,
                           a8+b8+c8+d8+e8+f8+g8+h8+i8,
                           g7+g8+g9+h7+h8+h9+i7+i8+i9)
AND tg9() BE try(@g9, th1, g1+g2+g3+g4+g5+g6+g7+g8+g9,
                           a9+b9+c9+d9+e9+f9+g9+h9+i9,
                           g7+g8+g9+h7+h8+h9+i7+i8+i9)

AND th1() BE try(@h1, th2, h1+h2+h3+h4+h5+h6+h7+h8+h9,
                           a1+b1+c1+d1+e1+f1+g1+h1+i1,
                           g1+g2+g3+h1+h2+h3+i1+i2+i3)
AND th2() BE try(@h2, th3, h1+h2+h3+h4+h5+h6+h7+h8+h9,
                           a2+b2+c2+d2+e2+f2+g2+h2+i2,
                           g1+g2+g3+h1+h2+h3+i1+i2+i3)
AND th3() BE try(@h3, th4, h1+h2+h3+h4+h5+h6+h7+h8+h9,
                           a3+b3+c3+d3+e3+f3+g3+h3+i3,
                           g1+g2+g3+h1+h2+h3+i1+i2+i3)
AND th4() BE try(@h4, th5, h1+h2+h3+h4+h5+h6+h7+h8+h9,
                           a4+b4+c4+d4+e4+f4+g4+h4+i4,
                           g4+g5+g6+h4+h5+h6+i4+i5+i6)
AND th5() BE try(@h5, th6, h1+h2+h3+h4+h5+h6+h7+h8+h9,
                           a5+b5+c5+d5+e5+f5+g5+h5+i5,
                           g4+g5+g6+h4+h5+h6+i4+i5+i6)
AND th6() BE try(@h6, th7, h1+h2+h3+h4+h5+h6+h7+h8+h9,
                           a6+b6+c6+d6+e6+f6+g6+h6+i6,
                           g4+g5+g6+h4+h5+h6+i4+i5+i6)
AND th7() BE try(@h7, th8, h1+h2+h3+h4+h5+h6+h7+h8+h9,
                           a7+b7+c7+d7+e7+f7+g7+h7+i7,
                           g7+g8+g9+h7+h8+h9+i7+i8+i9)
AND th8() BE try(@h8, th9, h1+h2+h3+h4+h5+h6+h7+h8+h9,
                           a8+b8+c8+d8+e8+f8+g8+h8+i8,
                           g7+g8+g9+h7+h8+h9+i7+i8+i9)
AND th9() BE try(@h9, ti1, h1+h2+h3+h4+h5+h6+h7+h8+h9,
                           a9+b9+c9+d9+e9+f9+g9+h9+i9,
                           g7+g8+g9+h7+h8+h9+i7+i8+i9)

AND ti1() BE try(@i1, ti2, i1+i2+i3+i4+i5+i6+i7+i8+i9,
                           a1+b1+c1+d1+e1+f1+g1+h1+i1,
                           g1+g2+g3+h1+h2+h3+i1+i2+i3)
AND ti2() BE try(@i2, ti3, i1+i2+i3+i4+i5+i6+i7+i8+i9,
                           a2+b2+c2+d2+e2+f2+g2+h2+i2,
                           g1+g2+g3+h1+h2+h3+i1+i2+i3)
AND ti3() BE try(@i3, ti4, i1+i2+i3+i4+i5+i6+i7+i8+i9,
                           a3+b3+c3+d3+e3+f3+g3+h3+i3,
                           g1+g2+g3+h1+h2+h3+i1+i2+i3)
AND ti4() BE try(@i4, ti5, i1+i2+i3+i4+i5+i6+i7+i8+i9,
                           a4+b4+c4+d4+e4+f4+g4+h4+i4,
                           g4+g5+g6+h4+h5+h6+i4+i5+i6)
AND ti5() BE try(@i5, ti6, i1+i2+i3+i4+i5+i6+i7+i8+i9,
                           a5+b5+c5+d5+e5+f5+g5+h5+i5,
                           g4+g5+g6+h4+h5+h6+i4+i5+i6)
AND ti6() BE try(@i6, ti7, i1+i2+i3+i4+i5+i6+i7+i8+i9,
                           a6+b6+c6+d6+e6+f6+g6+h6+i6,
                           g4+g5+g6+h4+h5+h6+i4+i5+i6)
AND ti7() BE try(@i7, ti8, i1+i2+i3+i4+i5+i6+i7+i8+i9,
                           a7+b7+c7+d7+e7+f7+g7+h7+i7,
                           g7+g8+g9+h7+h8+h9+i7+i8+i9)
AND ti8() BE try(@i8, ti9, i1+i2+i3+i4+i5+i6+i7+i8+i9,
                           a8+b8+c8+d8+e8+f8+g8+h8+i8,
                           g7+g8+g9+h7+h8+h9+i7+i8+i9)
AND ti9() BE try(@i9, suc, i1+i2+i3+i4+i5+i6+i7+i8+i9,
                           a9+b9+c9+d9+e9+f9+g9+h9+i9,
                           g7+g8+g9+h7+h8+h9+i7+i8+i9)

AND suc() BE
{ count := count + 1
  prboard()
}
```



## Befunge


Input should be provided as a sequence of 81 digits (optionally separated by whitespace), with zero representing an unknown value.


```befunge
99*>1-:0>:#$"0"\# #~`#$_"0"-\::9%:9+00p3/\9/:99++10p3vv%2g\g01<
2%v|:p+9/9\%9:\p\g02\1p\g01\1:p\g00\1:+8:\p02+*93+*3/<>\20g\g#:
v<+>:0\`>v >\::9%:9+00p3/\9/:99++10p3/3*+39*+20p\:8+::00g\g2%\^
v^+^pppp$_:|v<::<_>1-::9%\9/9+g.::9%!\3%+>>#v_>" "v..v,<<<+55<<
03!$v9:_>1v$>9%\v^|:<_v#<%<9<:<<_v#+%*93\!::<,,"|"<\/>:#^_>>>v^
p|<$0.0^!g+:#9/9<^@ ^,>#+5<5_>#!<>#$0"------+-------+-----":#<^
<>v$v1:::0<>"P"`!^>0g#0v#p+9/9\%9:p04:\pg03g021pg03g011pg03g001
::>^_:#<0#!:p#-\#1:#g0<>30g010g30g020g30g040g:9%\:9/9+\01-\1+0:
```


{{in}}


```txt
8 5 0 0 0 2 4 0 0
7 2 0 0 0 0 0 0 9
0 0 4 0 0 0 0 0 0
0 0 0 1 0 7 0 0 2
3 0 5 0 0 0 9 0 0
0 4 0 0 0 0 0 0 0
0 0 0 0 8 0 0 7 0
0 1 7 0 0 0 0 0 0
0 0 0 0 3 6 0 4 0
```


{{out}}


```txt
8 5 9 | 6 1 2 | 4 3 7
7 2 3 | 8 5 4 | 1 6 9
1 6 4 | 3 7 9 | 5 2 8
------+-------+------
9 8 6 | 1 4 7 | 3 5 2
3 7 5 | 2 6 8 | 9 1 4
2 4 1 | 5 9 3 | 7 8 6
------+-------+------
4 3 2 | 9 8 1 | 6 7 5
6 1 7 | 4 2 5 | 8 9 3
5 9 8 | 7 3 6 | 2 4 1
```



## Bracmat

The program:

```bracmat
{sudokuSolver.bra

Solves any 9x9 sudoku, using backtracking.
Not a simple brute force algorithm!}

sudokuSolver=
  ( sudoku
  =   ( new
      =   create
        .   ( create
            =   a
              .     !arg:%(<3:?a) ?arg
                  &   ( !a
                      .     !arg:
                          & 1 2 3 4 5 6 7 8 9
                        | create$!arg
                      )
                      create$(!a+1 !arg)
                |
            )
          & create$(0 0 0 0):?(its.Tree)
          & ( init
            =   cell remainingCells remainingRows x y
              .       !arg
                    : ( ?y
                      . ?x
                      . (.%?cell ?remainingCells) ?remainingRows
                      )
                  &   (   !cell:#
                        & ( !cell
                          .   mod$(!x,3)
                              div$(!x,3)
                              mod$(!y,3)
                              div$(!y,3)
                          )
                      |
                      )
                      (   !remainingCells:
                        & init$(!y+1.0.!remainingRows)
                      |   init
                        $ ( !y
                          . !x+1
                          . (.!remainingCells) !remainingRows
                          )
                      )
                |
            )
          & out$!arg
          &   (its.Set)$(!(its.Tree).init$(0.0.!arg))
            : ?(its.Tree)
      )
      ( Display
      =   val
        .     put$(str$("|~~~|~~~|~~~|" \n))
            &   !(its.Tree)
              :   ?
                  ( ?
                  .     ?
                        ( ?&put$"|"
                        .     ?
                              ( ?
                              .     ?
                                    ( ( ?
                                      .     ?val
                                          & !val:% %
                                          & put$"-"
                                        |   !val:
                                          & put$" "
                                        | put$!val
                                      )
                                    & ~
                                    )
                                    ?
                                | ?&put$"|"&~
                              )
                              ?
                          | ?&put$\n&~
                        )
                        ?
                    |   ?
                      & put$(str$("|~~~|~~~|~~~|" \n))
                      & ~
                  )
                  ?
          |
      )
      ( Set
      =     update certainValue a b c d
          , tree branch todo DOING loop dcba minlen len minp
        .   ( update
            =     path rempath value tr
                , k z x y trc p v branch s n
              .   !arg:(?path.?value.?tr.?trc)
                & (   !path:%?path ?rempath
                    & `(     !tr
                           : ?k (!path:?p.?branch) ?z
                         & `(   update$(!rempath.!value.!branch.!p !trc)
                              : ?s
                            &     update
                                $ (!path !rempath.!value.!z.!trc)
                              : ?n
                            & !k (!p.!s) !n
                            )
                       | !tr
                       )
                  | !DOING:(?.!trc)&!value
                  |   !tr:?x !value ?y
                    & `( !x !y
                       : (   ~:@
                           & (   !todo:? (?v.!trc) ?
                               & ( !v:!x !y
                                 |     out
                                     $ (mismatch v !v "<>" x y !x !y)
                                   & get'
                                 )
                             | (!x !y.!trc) !todo:?todo
                             )
                         | % %
                         | &!DOING:(?.!trc)
                         )
                       )
                  | !tr
                  )
            )
          & !arg:(?tree.?todo)
          & ( loop
            =   !todo:
              |     !todo
                  : ((?certainValue.%?d %?c %?b %?a):?DOING) ?todo
                &   update$(!a ? !c ?.!certainValue.!tree.)
                  : ?tree
                &   update$(!a !b <>!c ?.!certainValue.!tree.)
                  : ?tree
                &   update$(<>!a ? !c !d.!certainValue.!tree.)
                  : ?tree
                & !loop
            )
          & !loop
          & ( ~( !tree
               :   ?
                   (?.? (?.? (?.? (?.% %) ?) ?) ?)
                   ?
               )
            |   9:?minlen
              & :?minp
              & ( len
                =
                  .   !arg:% %?arg&1+len$!arg
                    | 1
                )
              & (   !tree
                  :   ?
                      ( ?a
                      .   ?
                          ( ?b
                          .   ?
                              ( ?c
                              .   ?
                                  ( ?d
                                  .   % %:?p
                                    & len$!p:<!minlen:?minlen
                                    & !d !c !b !a:?dcba
                                    & !p:?:?minp
                                    & ~
                                  )
                                  ?
                              )
                              ?
                          )
                          ?
                      )
                      ?
                |   !minp
                  :   ?
                      ( %@?n
                      & (its.Set)$(!tree.!n.!dcba):?tree
                      )
                      ?
                )
            )
          & !tree
      )
      (Tree=)
  )
  ( new
  =   puzzle
    .   new$((its.sudoku),!arg):?puzzle
      & (puzzle..Display)$
  );
```

Solve a sudoku that is hard for a brute force solver:

```bracmat
new'( sudokuSolver
    , (.- - - - - - - - -)
      (.- - - - - 3 - 8 5)
      (.- - 1 - 2 - - - -)
      (.- - - 5 - 7 - - -)
      (.- - 4 - - - 1 - -)
      (.- 9 - - - - - - -)
      (.5 - - - - - - 7 3)
      (.- - 2 - 1 - - - -)
      (.- - - - 4 - - - 9)
    );
```

Solution:

```txt
|~~~|~~~|~~~|
|987|654|321|
|246|173|985|
|351|928|746|
|~~~|~~~|~~~|
|128|537|694|
|634|892|157|
|795|461|832|
|~~~|~~~|~~~|
|519|286|473|
|472|319|568|
|863|745|219|
|~~~|~~~|~~~|
```



## C

See e.g. [http://www.techfinesse.com/game/sudoku_solver.php this GPLed solver] written in C.

The following code is really only good for size 3 puzzles. A longer, even less readable version [[Sudoku/C|here]] could handle size 4s.

```c
#include <stdio.h>

void show(int *x)
{
	int i, j;
	for (i = 0; i < 9; i++) {
		if (!(i % 3)) putchar('\n');
		for (j = 0; j < 9; j++)
			printf(j % 3 ? "%2d" : "%3d", *x++);
		putchar('\n');
	}
}

int trycell(int *x, int pos)
{
	int row = pos / 9;
	int col = pos % 9;
	int i, j, used = 0;

	if (pos == 81) return 1;
	if (x[pos]) return trycell(x, pos + 1);

	for (i = 0; i < 9; i++)
		used |= 1 << (x[i * 9 + col] - 1);

	for (j = 0; j < 9; j++)
		used |= 1 << (x[row * 9 + j] - 1);

	row = row / 3 * 3;
	col = col / 3 * 3;
	for (i = row; i < row + 3; i++)
		for (j = col; j < col + 3; j++)
			used |= 1 << (x[i * 9 + j] - 1);

	for (x[pos] = 1; x[pos] <= 9; x[pos]++, used >>= 1)
		if (!(used & 1) && trycell(x, pos + 1)) return 1;

	x[pos] = 0;
	return 0;
}

void solve(const char *s)
{
	int i, x[81];
	for (i = 0; i < 81; i++)
		x[i] = s[i] >= '1' && s[i] <= '9' ? s[i] - '0' : 0;

	if (trycell(x, 0))
		show(x);
	else
		puts("no solution");
}

int main(void)
{
	solve(	"5x..7...."
		"6..195..."
		".98....6."
		"8...6...3"
		"4..8.3..1"
		"7...2...6"
		".6....28."
		"...419..5"
		"....8..79"	);

	return 0;
}
```



## C#

===“Manual” Solution===
{{trans|Java}}

```csharp
using System;

class SudokuSolver
{
    private int[] grid;

    public SudokuSolver(String s)
    {
        grid = new int[81];
        for (int i = 0; i < s.Length; i++)
        {
            grid[i] = int.Parse(s[i].ToString());
        }
    }

    public void solve()
    {
        try
        {
            placeNumber(0);
            Console.WriteLine("Unsolvable!");
        }
        catch (Exception ex)
        {
            Console.WriteLine(ex.Message);
            Console.WriteLine(this);
        }
    }

    public void placeNumber(int pos)
    {
        if (pos == 81)
        {
            throw new Exception("Finished!");
        }
        if (grid[pos] > 0)
        {
            placeNumber(pos + 1);
            return;
        }
        for (int n = 1; n <= 9; n++)
        {
            if (checkValidity(n, pos % 9, pos / 9))
            {
                grid[pos] = n;
                placeNumber(pos + 1);
                grid[pos] = 0;
            }
        }
    }

    public bool checkValidity(int val, int x, int y)
    {
        for (int i = 0; i < 9; i++)
        {
            if (grid[y * 9 + i] == val || grid[i * 9 + x] == val)
                return false;
        }
        int startX = (x / 3) * 3;
        int startY = (y / 3) * 3;
        for (int i = startY; i < startY + 3; i++)
        {
            for (int j = startX; j < startX + 3; j++)
            {
                if (grid[i * 9 + j] == val)
                    return false;
            }
        }
        return true;
    }

    public override string ToString()
    {
        string sb = "";
        for (int i = 0; i < 9; i++)
        {
            for (int j = 0; j < 9; j++)
            {
                sb += (grid[i * 9 + j] + " ");
                if (j == 2 || j == 5)
                    sb += ("| ");
            }
            sb += ('\n');
            if (i == 2 || i == 5)
                sb += ("------+-------+------\n");
        }
        return sb;
    }

    public static void Main(String[] args)
    {
        new SudokuSolver("850002400" +
                         "720000009" +
                         "004000000" +
                         "000107002" +
                         "305000900" +
                         "040000000" +
                         "000080070" +
                         "017000000" +
                         "000036040").solve();
        Console.Read();
    }
}
```

===“Amb” Solution===
This uses the second version of the [https://rosettacode.org/wiki/Amb#C.23 Amb C# class] in the Amb challenge
{{works with|C sharp|C#|7.1}}
<!-- By Martin Freedman, 9/02/2018 -->

```csharp
using System.Linq;
using static System.Linq.Enumerable;
using static System.Console;
using  Amb;

namespace Sudoku
{
    static class Program
    {
        static void Main(string[] args)
        {
            var challenge = new string[]{ "970 340 060",
                                          "861 750 000",
                                          "324 168 957",
                                          "219 584 673",
                                          "487 236 519",
                                          "653 971 284",
                                          "738 425 196",
                                          "002 010 048",
                                          "040 007 300" };

            var Solve = challenge
                .Select(r => r.Where(c => c != ' ').Select(c => (int)c - '0').ToArray())
                .ToArray();

            var amb = new Amb.Amb();

            var matrix = new IValue<int>[9][];
            var domain = new []{0, 1, 2, 3, 4, 5, 6, 7, 8, 9 };

            void IsUnequal(IValue<int>[] values) => amb.Require(()=> values.Select(v => v.Value).Distinct().Count() == 9);

            IValue<int>[] GetBox(IValue<int>[][] sudoku, int Ra, int Rb, int Ca, int Cb)
            {
                var slice = new IValue<int>[9];

                int i = 0;
                for (int row = Ra; row <= Rb; row++)
                    for (int col = Ca; col <= Cb; col++)
                        slice[i++] = sudoku[row][col];
                return slice;
            }

            // rows
            foreach (var r in Range(0,9))
            {
                matrix[r] = new IValue<int>[9];
                var exclude = Solve[r].Where(i => i > 0).Prepend(0).ToArray(); // dont forget 0!
                foreach (var c  in Range(0,9))
                {
                    // int cell and cell constraints
                    matrix[r][c] = amb.Choose<int>(domain);
                    if (Solve[r][c] > 0)
                        amb.Require(() => matrix[r][c].Value == Solve[r][c]);
                    else
                        amb.Require(() => !exclude.Contains(matrix[r][c].Value));

                    // boxes
                    if ((r ==2 || r==5 || r==8) && (c==2 || c==5 || c==8))
                        IsUnequal(GetBox(matrix, r -2, r, c -2, c));
                }
                IsUnequal(Range(0,9).Select(i=>matrix[r][i]).ToArray());
            }
            //cols
            foreach (var c in Range(0, 9))
            {
                IsUnequal(Range(0, 9).Select(j => matrix[j][c]).ToArray());
            }

            if (amb.Disambiguate())
            {
                for (var r = 0; r < 9; r++)
                {
                    for (var c = 0; c < 9; c++)
                    {
                        Write($"{matrix[r][c].Value}");
                    }
                    Write("\n");
                }
            }
            else
            {
                WriteLine("Amb is angry!");
            }
            Read();
        }
    }
}
```

Output

```txt
975342861
861759432
324168957
219584673
487236519
653971284
738425196
592613748
146897325

```



===“Automatic” Solution===
{{libheader|Microsoft Solver Foundation}}
<!-- By Nigel Galloway, Jan 29, 2012 -->

```csharp
using Microsoft.SolverFoundation.Solvers;

namespace Sudoku
{
    class Program
    {
        private static int[,] B = new int[,] {{9,7,0, 3,0,0, 0,6,0},
                                              {0,6,0, 7,5,0, 0,0,0},
                                              {0,0,0, 0,0,8, 0,5,0},

                                              {0,0,0, 0,0,0, 6,7,0},
                                              {0,0,0, 0,3,0, 0,0,0},
                                              {0,5,3, 9,0,0, 2,0,0},

                                              {7,0,0, 0,2,5, 0,0,0},
                                              {0,0,2, 0,1,0, 0,0,8},
                                              {0,4,0, 0,0,7, 3,0,0}};

        private static CspTerm[] GetSlice(CspTerm[][] sudoku, int Ra, int Rb, int Ca, int Cb)
        {
            CspTerm[] slice = new CspTerm[9];
            int i = 0;
            for (int row = Ra; row < Rb + 1; row++)
                for (int col = Ca; col < Cb + 1; col++)
                {
                    {
                        slice[i++] = sudoku[row][col];
                    }
                }
            return slice;
        }

        static void Main(string[] args)
        {
            ConstraintSystem S = ConstraintSystem.CreateSolver();
            CspDomain Z = S.CreateIntegerInterval(1, 9);
            CspTerm[][] sudoku = S.CreateVariableArray(Z, "cell", 9, 9);
            for (int row = 0; row < 9; row++)
            {
                for (int col = 0; col < 9; col++)
                {
                    if (B[row, col] > 0)
                    {
                        S.AddConstraints(S.Equal(B[row, col], sudoku[row][col]));
                    }
                }
                S.AddConstraints(S.Unequal(GetSlice(sudoku, row, row, 0, 8)));
            }
            for (int col = 0; col < 9; col++)
            {
                S.AddConstraints(S.Unequal(GetSlice(sudoku, 0, 8, col, col)));
            }
            for (int a = 0; a < 3; a++)
            {
                for (int b = 0; b < 3; b++)
                {
                    S.AddConstraints(S.Unequal(GetSlice(sudoku, a * 3, a * 3 + 2, b * 3, b * 3 + 2)));
                }
            }
            ConstraintSolverSolution soln = S.Solve();
            object[] h = new object[9];
            for (int row = 0; row < 9; row++)
            {
                if ((row % 3) == 0) System.Console.WriteLine();
                for (int col = 0; col < 9; col++)
                {
                    soln.TryGetValue(sudoku[row][col], out h [col]);
                }
                System.Console.WriteLine("{0}{1}{2} {3}{4}{5} {6}{7}{8}", h[0],h[1],h[2],h[3],h[4],h[5],h[6],h[7],h[8]);
            }
        }
    }
}
```

Produces:

```txt

975 342 861
861 759 432
324 168 957

219 584 673
487 236 519
653 971 284

738 425 196
592 613 748
146 897 325

```


===Using the "Dancing Links" algorithm===

```csharp
using System;
using System.Collections.Generic;
using System.Text;
using static System.Linq.Enumerable;

public class Sudoku
{
    public static void Main2() {
        string puzzle = "....7.94.....9...53....5.7...74..1..463...........7.8.8........7......28.5.26....";
        string solution = new Sudoku().Solutions(puzzle).FirstOrDefault() ?? puzzle;
        Print(puzzle, solution);
    }

    private DLX dlx;

    public void Build() {
        const int rows = 9 * 9 * 9, columns = 4 * 9 * 9;
        dlx = new DLX(rows, columns);
        for (int i = 0; i < columns; i++) dlx.AddHeader();

        for (int cell = 0, row = 0; row < 9; row++) {
            for (int column = 0; column < 9; column++) {
                int box = row / 3 * 3 + column / 3;
                for (int digit = 0; digit < 9; digit++) {
                    dlx.AddRow(cell, 81 + row * 9 + digit, 2 * 81 + column * 9 + digit, 3 * 81 + box * 9 + digit);
                }
                cell++;
            }
        }
    }

    public IEnumerable<string> Solutions(string puzzle) {
        if (puzzle == null) throw new ArgumentNullException(nameof(puzzle));
        if (puzzle.Length != 81) throw new ArgumentException("The input is not of the correct length.");
        if (dlx == null) Build();

        for (int i = 0; i < puzzle.Length; i++) {
            if (puzzle[i] == '0' || puzzle[i] == '.') continue;
            if (puzzle[i] < '1' && puzzle[i] > '9') throw new ArgumentException($"Input contains an invalid character: ({puzzle[i]})");
            int digit = puzzle[i] - '0' - 1;
            dlx.Give(i * 9 + digit);
        }
        return Iterator();

        IEnumerable<string> Iterator() {
            var sb = new StringBuilder(new string('.', 81));
            foreach (int[] rows in dlx.Solutions()) {
                foreach (int r in rows) {
                    sb[r / 81 * 9 + r / 9 % 9] = (char)(r % 9 + '1');
                }
                yield return sb.ToString();
            }
        }
    }

    static void Print(string left, string right) {
        foreach (string line in GetPrintLines(left).Zip(GetPrintLines(right), (l, r) => l + "\t" + r)) {
            Console.WriteLine(line);
        }

        IEnumerable<string> GetPrintLines(string s) {
            int r = 0;
            foreach (string row in s.Cut(9)) {
                yield return r == 0
                    ? "╔═══╤═══╤═══╦═══╤═══╤═══╦═══╤═══╤═══╗"
                    : r % 3 == 0
                    ? "╠═══╪═══╪═══╬═══╪═══╪═══╬═══╪═══╪═══╣"
                    : "╟───┼───┼───╫───┼───┼───╫───┼───┼───╢";
                yield return "║ " + row.Cut(3).Select(segment => segment.DelimitWith(" │ ")).DelimitWith(" ║ ") + " ║";
                r++;
            }
            yield return "╚═══╧═══╧═══╩═══╧═══╧═══╩═══╧═══╧═══╝";
        }
    }

}

public class DLX //Some functionality elided
{
    private readonly Header root = new Header(null, null) { Size = int.MaxValue };
    private readonly List<Header> columns;
    private readonly List<Node> rows;
    private readonly Stack<Node> solutionNodes = new Stack<Node>();
    private int initial = 0;

    public DLX(int rowCapacity, int columnCapacity) {
        columns = new List<Header>(columnCapacity);
        rows = new List<Node>(rowCapacity);
    }

    public void AddHeader() {
        Header h = new Header(root.Left, root);
        h.AttachLeftRight();
        columns.Add(h);
    }

    public void AddRow(params int[] newRow) {
        Node first = null;
        if (newRow != null) {
            for (int i = 0; i < newRow.Length; i++) {
                if (newRow[i] < 0) continue;
                if (first == null) first = AddNode(rows.Count, newRow[i]);
                else AddNode(first, newRow[i]);
            }
        }
        rows.Add(first);
    }

    private Node AddNode(int row, int column) {
        Node n = new Node(null, null, columns[column].Up, columns[column], columns[column], row);
        n.AttachUpDown();
        n.Head.Size++;
        return n;
    }

    private void AddNode(Node firstNode, int column) {
        Node n = new Node(firstNode.Left, firstNode, columns[column].Up, columns[column], columns[column], firstNode.Row);
        n.AttachLeftRight();
        n.AttachUpDown();
        n.Head.Size++;
    }

    public void Give(int row) {
        solutionNodes.Push(rows[row]);
        CoverMatrix(rows[row]);
        initial++;
    }

    public IEnumerable<int[]> Solutions() {
        try {
            Node node = ChooseSmallestColumn().Down;
            do {
                if (node == node.Head) {
                    if (node == root) {
                        yield return solutionNodes.Select(n => n.Row).ToArray();
                    }
                    if (solutionNodes.Count > initial) {
                        node = solutionNodes.Pop();
                        UncoverMatrix(node);
                        node = node.Down;
                    }
                } else {
                    solutionNodes.Push(node);
                    CoverMatrix(node);
                    node = ChooseSmallestColumn().Down;
                }
            } while(solutionNodes.Count > initial || node != node.Head);
        } finally {
            Restore();
        }
    }

    private void Restore() {
        while (solutionNodes.Count > 0) UncoverMatrix(solutionNodes.Pop());
        initial = 0;
    }

    private Header ChooseSmallestColumn() {
        Header traveller = root, choice = root;
        do {
            traveller = (Header)traveller.Right;
            if (traveller.Size < choice.Size) choice = traveller;
        } while (traveller != root && choice.Size > 0);
        return choice;
    }

    private void CoverRow(Node row) {
        Node traveller = row.Right;
        while (traveller != row) {
            traveller.DetachUpDown();
            traveller.Head.Size--;
            traveller = traveller.Right;
        }
    }

    private void UncoverRow(Node row) {
        Node traveller = row.Left;
        while (traveller != row) {
            traveller.AttachUpDown();
            traveller.Head.Size++;
            traveller = traveller.Left;
        }
    }

    private void CoverColumn(Header column) {
        column.DetachLeftRight();
        Node traveller = column.Down;
        while (traveller != column) {
            CoverRow(traveller);
            traveller = traveller.Down;
        }
    }

    private void UncoverColumn(Header column) {
        Node traveller = column.Up;
        while (traveller != column) {
            UncoverRow(traveller);
            traveller = traveller.Up;
        }
        column.AttachLeftRight();
    }

    private void CoverMatrix(Node node) {
        Node traveller = node;
        do {
            CoverColumn(traveller.Head);
            traveller = traveller.Right;
        } while (traveller != node);
    }

    private void UncoverMatrix(Node node) {
        Node traveller = node;
        do {
            traveller = traveller.Left;
            UncoverColumn(traveller.Head);
        } while (traveller != node);
    }

    private class Node
    {
        public Node(Node left, Node right, Node up, Node down, Header head, int row) {
            Left = left ?? this;
            Right = right ?? this;
            Up = up ?? this;
            Down = down ?? this;
            Head = head ?? this as Header;
            Row = row;
        }

        public Node Left   { get; set; }
        public Node Right  { get; set; }
        public Node Up     { get; set; }
        public Node Down   { get; set; }
        public Header Head { get; }
        public int Row     { get; }

        public void AttachLeftRight() {
            this.Left.Right = this;
            this.Right.Left = this;
        }

        public void AttachUpDown() {
            this.Up.Down = this;
            this.Down.Up = this;
        }

        public void DetachLeftRight() {
            this.Left.Right = this.Right;
            this.Right.Left = this.Left;
        }

        public void DetachUpDown() {
            this.Up.Down = this.Down;
            this.Down.Up = this.Up;
        }

    }

    private class Header : Node
    {
        public Header(Node left, Node right) : base(left, right, null, null, null, -1) { }

        public int Size { get; set; }
    }

}

static class Extensions
{
    public static IEnumerable<string> Cut(this string input, int length)
    {
        for (int cursor = 0; cursor < input.Length; cursor += length) {
            if (cursor + length > input.Length) yield return input.Substring(cursor);
            else yield return input.Substring(cursor, length);
        }
    }

    public static string DelimitWith<T>(this IEnumerable<T> source, string separator) => string.Join(separator, source);
}
```

{{out}}

```txt

╔═══╤═══╤═══╦═══╤═══╤═══╦═══╤═══╤═══╗	╔═══╤═══╤═══╦═══╤═══╤═══╦═══╤═══╤═══╗
║ . │ . │ . ║ . │ 7 │ . ║ 9 │ 4 │ . ║	║ 2 │ 1 │ 5 ║ 8 │ 7 │ 6 ║ 9 │ 4 │ 3 ║
╟───┼───┼───╫───┼───┼───╫───┼───┼───╢	╟───┼───┼───╫───┼───┼───╫───┼───┼───╢
║ . │ . │ . ║ . │ 9 │ . ║ . │ . │ 5 ║	║ 6 │ 7 │ 8 ║ 3 │ 9 │ 4 ║ 2 │ 1 │ 5 ║
╟───┼───┼───╫───┼───┼───╫───┼───┼───╢	╟───┼───┼───╫───┼───┼───╫───┼───┼───╢
║ 3 │ . │ . ║ . │ . │ 5 ║ . │ 7 │ . ║	║ 3 │ 4 │ 9 ║ 1 │ 2 │ 5 ║ 8 │ 7 │ 6 ║
╠═══╪═══╪═══╬═══╪═══╪═══╬═══╪═══╪═══╣	╠═══╪═══╪═══╬═══╪═══╪═══╬═══╪═══╪═══╣
║ . │ . │ 7 ║ 4 │ . │ . ║ 1 │ . │ . ║	║ 5 │ 8 │ 7 ║ 4 │ 3 │ 2 ║ 1 │ 6 │ 9 ║
╟───┼───┼───╫───┼───┼───╫───┼───┼───╢	╟───┼───┼───╫───┼───┼───╫───┼───┼───╢
║ 4 │ 6 │ 3 ║ . │ . │ . ║ . │ . │ . ║	║ 4 │ 6 │ 3 ║ 9 │ 8 │ 1 ║ 7 │ 5 │ 2 ║
╟───┼───┼───╫───┼───┼───╫───┼───┼───╢	╟───┼───┼───╫───┼───┼───╫───┼───┼───╢
║ . │ . │ . ║ . │ . │ 7 ║ . │ 8 │ . ║	║ 1 │ 9 │ 2 ║ 6 │ 5 │ 7 ║ 3 │ 8 │ 4 ║
╠═══╪═══╪═══╬═══╪═══╪═══╬═══╪═══╪═══╣	╠═══╪═══╪═══╬═══╪═══╪═══╬═══╪═══╪═══╣
║ 8 │ . │ . ║ . │ . │ . ║ . │ . │ . ║	║ 8 │ 2 │ 6 ║ 7 │ 4 │ 3 ║ 5 │ 9 │ 1 ║
╟───┼───┼───╫───┼───┼───╫───┼───┼───╢	╟───┼───┼───╫───┼───┼───╫───┼───┼───╢
║ 7 │ . │ . ║ . │ . │ . ║ . │ 2 │ 8 ║	║ 7 │ 3 │ 4 ║ 5 │ 1 │ 9 ║ 6 │ 2 │ 8 ║
╟───┼───┼───╫───┼───┼───╫───┼───┼───╢	╟───┼───┼───╫───┼───┼───╫───┼───┼───╢
║ . │ 5 │ . ║ 2 │ 6 │ . ║ . │ . │ . ║	║ 9 │ 5 │ 1 ║ 2 │ 6 │ 8 ║ 4 │ 3 │ 7 ║
╚═══╧═══╧═══╩═══╧═══╧═══╩═══╧═══╧═══╝	╚═══╧═══╧═══╩═══╧═══╧═══╩═══╧═══╧═══╝
```



## C++

{{trans|Java}}

```cpp
#include <iostream>
using namespace std;

class SudokuSolver {
private:
    int grid[81];

public:

    SudokuSolver(string s) {
        for (unsigned int i = 0; i < s.length(); i++) {
            grid[i] = (int) (s[i] - '0');
        }
    }

    void solve() {
        try {
            placeNumber(0);
            cout << "Unsolvable!" << endl;
        } catch (char* ex) {
            cout << ex << endl;
            cout << this->toString() << endl;
        }
    }

    void placeNumber(int pos) {
        if (pos == 81) {
            throw (char*) "Finished!";
        }
        if (grid[pos] > 0) {
            placeNumber(pos + 1);
            return;
        }
        for (int n = 1; n <= 9; n++) {
            if (checkValidity(n, pos % 9, pos / 9)) {
                grid[pos] = n;
                placeNumber(pos + 1);
                grid[pos] = 0;
            }
        }
    }

    bool checkValidity(int val, int x, int y) {
        for (int i = 0; i < 9; i++) {
            if (grid[y * 9 + i] == val || grid[i * 9 + x] == val)
                return false;
        }
        int startX = (x / 3) * 3;
        int startY = (y / 3) * 3;
        for (int i = startY; i < startY + 3; i++) {
            for (int j = startX; j < startX + 3; j++) {
                if (grid[i * 9 + j] == val)
                    return false;
            }
        }
        return true;
    }

    string toString() {
        string sb;
        for (int i = 0; i < 9; i++) {
            for (int j = 0; j < 9; j++) {
                char c[2];
                c[0] = grid[i * 9 + j] + '0';
                c[1] = '\0';
                sb.append(c);
                sb.append(" ");
                if (j == 2 || j == 5)
                    sb.append("| ");
            }
            sb.append("\n");
            if (i == 2 || i == 5)
                sb.append("------+-------+------\n");
        }
        return sb;
    }

};

int main() {
    SudokuSolver ss(
            (string) "850002400" +
            (string) "720000009" +
            (string) "004000000" +
            (string) "000107002" +
            (string) "305000900" +
            (string) "040000000" +
            (string) "000080070" +
            (string) "017000000" +
            (string) "000036040"
            );
    ss.solve();
}
```



## Clojure


```clojure
(ns rosettacode.sudoku
  (:use [clojure.pprint :only (cl-format)]))

(defn- compatible? [m x y n]
  (let [n= #(= n (get-in m [%1 %2]))]
    (or (n= y x)
      (let [c (count m)]
        (and (zero? (get-in m [y x]))
             (not-any? #(or (n= y %) (n= % x)) (range c))
             (let [zx (* c (quot x c)), zy (* c (quot y c))]
               (every? false?
                 (map n= (range zy (+ zy c)) (range zx (+ zx c))))))))))

(defn solve [m]
  (let [c (count m)]
    (loop [m m, x 0, y 0]
      (if (= y c) m
        (let [ng (->> (range 1 c)
                      (filter #(compatible? m x y %))
                      first
                      (assoc-in m [y x]))]
          (if (= x (dec c))
            (recur ng 0 (inc y))
            (recur ng (inc x) y)))))))
```



```clojure>sudoku
(cl-format true "~{~{~a~^ ~}~%~}"
 (solve [[3 9 4 0 0 2 6 7 0]
         [0 0 0 3 0 0 4 0 0]
         [5 0 0 6 9 0 0 2 0]
         [0 4 5 0 0 0 9 0 0]
         [6 0 0 0 0 0 0 0 7]
         [0 0 7 0 0 0 5 8 0]
         [0 1 0 0 6 7 0 0 8]
         [0 0 9 0 0 8 0 0 0]
         [0 2 6 4 0 0 7 3 5]])
3  9  4  8  5  2  6  7  1
2  6  8  3  7  1  4  5  9
5  7  1  6  9  4  8  2  3
1  4  5  7  8  3  9  6  2
6  8  2  9  4  5  3  1  7
9  3  7  1  2  6  5  8  4
4  1  3  5  6  7  2  9  8
7  5  9  2  3  8  1  4  6
8  2  6  4  1  9  7  3  5

nil
```



## Common Lisp

A simple solver without optimizations (except for pre-computing the possible entries of a cell).

```lisp
(defun row-neighbors (row column grid &aux (neighbors '()))
  (dotimes (i 9 neighbors)
    (let ((x (aref grid row i)))
      (unless (or (eq '_ x) (= i column))
        (push x neighbors)))))

(defun column-neighbors (row column grid &aux (neighbors '()))
  (dotimes (i 9 neighbors)
    (let ((x (aref grid i column)))
      (unless (or (eq x '_) (= i row))
        (push x neighbors)))))

(defun square-neighbors (row column grid &aux (neighbors '()))
  (let* ((rmin (* 3 (floor row 3)))    (rmax (+ rmin 3))
         (cmin (* 3 (floor column 3))) (cmax (+ cmin 3)))
    (do ((r rmin (1+ r))) ((= r rmax) neighbors)
      (do ((c cmin (1+ c))) ((= c cmax))
        (let ((x (aref grid r c)))
          (unless (or (eq x '_) (= r row) (= c column))
            (push x neighbors)))))))

(defun choices (row column grid)
  (nset-difference
   (list 1 2 3 4 5 6 7 8 9)
   (nconc (row-neighbors row column grid)
          (column-neighbors row column grid)
          (square-neighbors row column grid))))

(defun solve (grid &optional (row 0) (column 0))
  (cond
   ((= row 9)
    grid)
   ((= column 9)
    (solve grid (1+ row) 0))
   ((not (eq '_ (aref grid row column)))
    (solve grid row (1+ column)))
   (t (dolist (choice (choices row column grid) (setf (aref grid row column) '_))
        (setf (aref grid row column) choice)
        (when (eq grid (solve grid row (1+ column)))
          (return grid))))))
```

Example:

```txt
> (defparameter *puzzle*
  #2A((3 9 4    _ _ 2    6 7 _)
      (_ _ _    3 _ _    4 _ _)
      (5 _ _    6 9 _    _ 2 _)

      (_ 4 5    _ _ _    9 _ _)
      (6 _ _    _ _ _    _ _ 7)
      (_ _ 7    _ _ _    5 8 _)

      (_ 1 _    _ 6 7    _ _ 8)
      (_ _ 9    _ _ 8    _ _ _)
      (_ 2 6    4 _ _    7 3 5)))
*PUZZLE*

> (pprint (solve *puzzle*))

#2A((3 9 4 8 5 2 6 7 1)
    (2 6 8 3 7 1 4 5 9)
    (5 7 1 6 9 4 8 2 3)
    (1 4 5 7 8 3 9 6 2)
    (6 8 2 9 4 5 3 1 7)
    (9 3 7 1 2 6 5 8 4)
    (4 1 3 5 6 7 2 9 8)
    (7 5 9 2 3 8 1 4 6)
    (8 2 6 4 1 9 7 3 5))
```



## Curry

Copied from [http://www.informatik.uni-kiel.de/~curry/examples/ Curry: Example Programs].

```curry
-----------------------------------------------------------------------------
--- Solving Su Doku puzzles in Curry with FD constraints
---
--- @author Michael Hanus
--- @version December 2005
-----------------------------------------------------------------------------

import CLPFD
import List

-- Solving a Su Doku puzzle represented as a matrix of numbers (possibly free
-- variables):
sudoku :: [[Int]] -> Success
sudoku m =
 domain (concat m) 1 9 &                         -- define domain of all digits
 foldr1 (&) (map allDifferent m)  &             -- all rows contain different digits
 foldr1 (&) (map allDifferent (transpose m))  & -- all columns have different digits
 foldr1 (&) (map allDifferent (squaresOfNine m)) & -- all 3x3 squares are different
 labeling [FirstFailConstrained] (concat m)

-- translate a matrix into a list of small 3x3 squares
squaresOfNine :: [[a]] -> [[a]]
squaresOfNine [] = []
squaresOfNine (l1:l2:l3:ls) = group3Rows [l1,l2,l3] ++ squaresOfNine ls

group3Rows l123 = if null (head l123) then [] else
 concatMap (take 3) l123 : group3Rows (map (drop 3) l123)

-- read a Su Doku specification written as a list of strings containing digits
-- and spaces
readSudoku :: [String] -> [[Int]]
readSudoku s = map (map transDigit) s
 where
   transDigit c = if c==' ' then x else ord c - ord '0'
      where x free

-- show a solved Su Doku matrix
showSudoku :: [[Int]] -> String
showSudoku = unlines . map (concatMap (\i->[chr (i + ord '0'),' ']))

-- the main function, e.g., evaluate (main s1):
main s | sudoku m = putStrLn (showSudoku m)
 where m = readSudoku s

s1 = ["9  2  5  ",
      " 4  6  3 ",
      "  3     6",
      "   9  2  ",
      "    5  8 ",
      "  7  4  3",
      "7     1  ",
      " 5  2  4 ",
      "  1  6  9"]

s2 = ["819  5   ",
      "  2   75 ",
      " 371 4 6 ",
      "4  59 1  ",
      "7  3 8  2",
      "  3 62  7",
      " 5 7 921 ",
      " 64   9  ",
      "   2  438"]
```




### Alternative version

{{Works with|PAKCS}}
Minimal w/o read or show utilities.

```curry
import CLPFD
import Constraint (allC)
import List (transpose)


sudoku :: [[Int]] -> Success
sudoku rows =
    domain (concat rows) 1 9
  & different rows
  & different (transpose rows)
  & different blocks
  & labeling [] (concat rows)
  where
    different = allC allDifferent

    blocks = [concat ys | xs <- each3 rows
                        , ys <- transpose $ map each3 xs
             ]
    each3 xs = case xs of
        (x:y:z:rest) -> [x,y,z] : each3 rest
        rest         -> [rest]


test = [ [_,_,3,_,_,_,_,_,_]
       , [4,_,_,_,8,_,_,3,6]
       , [_,_,8,_,_,_,1,_,_]
       , [_,4,_,_,6,_,_,7,3]
       , [_,_,_,9,_,_,_,_,_]
       , [_,_,_,_,_,2,_,_,5]
       , [_,_,4,_,7,_,_,6,8]
       , [6,_,_,_,_,_,_,_,_]
       , [7,_,_,6,_,_,5,_,_]
       ]
main | sudoku xs = xs where xs = test
```

{{Out}}

```txt
Execution time: 0 msec. / elapsed: 10 msec.
[[1,2,3,4,5,6,7,8,9],[4,5,7,1,8,9,2,3,6],[9,6,8,3,2,7,1,5,4],[2,4,9,5,6,1,8,7,3],[5,7,6,9,3,8,4,1,2],[8,3,1,7,4,2,6,9,5],[3,1,4,2,7,5,9,6,8],[6,9,5,8,1,4,3,2,7],[7,8,2,6,9,3,5,4,1]]
```



## D

{{trans|C++}}
A little over-engineered solution, that shows some strong static typing useful in larger programs.

```d
import std.stdio, std.range, std.string, std.algorithm, std.array,
       std.ascii, std.typecons;

struct Digit {
    immutable char d;

    this(in char d_) pure nothrow @safe @nogc
    in { assert(d_ >= '0' && d_ <= '9'); }
    body { this.d = d_; }

    this(in int d_) pure nothrow @safe @nogc
    in { assert(d_ >= '0' && d_ <= '9'); }
    body { this.d = cast(char)d_; } // Required cast.

    alias d this;
}

enum size_t sudokuUnitSide = 3;
enum size_t sudokuSide = sudokuUnitSide ^^ 2; // Sudoku grid side.
alias SudokuTable = Digit[sudokuSide ^^ 2];


Nullable!SudokuTable sudokuSolver(in ref SudokuTable problem)
pure nothrow {
    alias Tgrid = uint;
    Tgrid[SudokuTable.length] grid = void;
    problem[].map!(c => c - '0').copy(grid[]);

    // DMD doesn't inline this function. Performance loss.
    Tgrid access(in size_t x, in size_t y) nothrow @safe @nogc {
        return grid[y * sudokuSide + x];
    }

    // DMD doesn't inline this function. If you want to retain
    // the same performance as the C++ entry and you use the DMD
    // compiler then this function must be manually inlined.
    bool checkValidity(in Tgrid val, in size_t x, in size_t y)
    pure nothrow @safe @nogc {
        /*static*/ foreach (immutable i; staticIota!(0, sudokuSide))
            if (access(i, y) == val || access(x, i) == val)
                return false;

        immutable startX = (x / sudokuUnitSide) * sudokuUnitSide;
        immutable startY = (y / sudokuUnitSide) * sudokuUnitSide;

        /*static*/ foreach (immutable i; staticIota!(0, sudokuUnitSide))
            /*static*/ foreach (immutable j; staticIota!(0, sudokuUnitSide))
                if (access(startX + j, startY + i) == val)
                    return false;

        return true;
    }

    bool canPlaceNumbers(in size_t pos=0) nothrow @safe @nogc {
        if (pos == SudokuTable.length)
            return true;
        if (grid[pos] > 0)
            return canPlaceNumbers(pos + 1);

        foreach (immutable n; 1 .. sudokuSide + 1)
            if (checkValidity(n, pos % sudokuSide, pos / sudokuSide)) {
                grid[pos] = n;
                if (canPlaceNumbers(pos + 1))
                    return true;
                grid[pos] = 0;
            }

        return false;
    }

    if (canPlaceNumbers) {
        //return typeof(return)(grid[]
        //                      .map!(c => Digit(c + '0'))
        //                      .array);
        immutable SudokuTable result = grid[]
                                       .map!(c => Digit(c + '0'))
                                       .array;
        return typeof(return)(result);
    } else
        return typeof(return)();
}

string representSudoku(in ref SudokuTable sudo)
pure nothrow @safe out(result) {
    assert(result.countchars("1-9") == sudo[].count!q{a != '0'});
    assert(result.countchars(".") == sudo[].count!q{a == '0'});
} body {
    static assert(sudo.length == 81,
        "representSudoku works only with a 9x9 Sudoku.");
    string result;

    foreach (immutable i; 0 .. sudokuSide) {
        foreach (immutable j; 0 .. sudokuSide) {
            result ~= sudo[i * sudokuSide + j];
            result ~= ' ';
            if (j == 2 || j == 5)
                result ~= "| ";
        }
        result ~= "\n";
        if (i == 2 || i == 5)
            result ~= "------+-------+------\n";
    }

    return result.replace("0", ".");
}

void main() {
    enum ValidateCells(string s) = s.map!Digit.array;

    immutable SudokuTable problem = ValidateCells!("
        850002400
        720000009
        004000000
        000107002
        305000900
        040000000
        000080070
        017000000
        000036040".removechars(whitespace));
    problem.representSudoku.writeln;

    immutable solution = problem.sudokuSolver;
    if (solution.isNull)
        writeln("Unsolvable!");
    else
        solution.get.representSudoku.writeln;
}
```

{{out}}

```txt
8 5 . | . . 2 | 4 . .
7 2 . | . . . | . . 9
. . 4 | . . . | . . .
------+-------+------
. . . | 1 . 7 | . . 2
3 . 5 | . . . | 9 . .
. 4 . | . . . | . . .
------+-------+------
. . . | . 8 . | . 7 .
. 1 7 | . . . | . . .
. . . | . 3 6 | . 4 .

8 5 9 | 6 1 2 | 4 3 7
7 2 3 | 8 5 4 | 1 6 9
1 6 4 | 3 7 9 | 5 2 8
------+-------+------
9 8 6 | 1 4 7 | 3 5 2
3 7 5 | 2 6 8 | 9 1 4
2 4 1 | 5 9 3 | 7 8 6
------+-------+------
4 3 2 | 9 8 1 | 6 7 5
6 1 7 | 4 2 5 | 8 9 3
5 9 8 | 7 3 6 | 2 4 1
```



### Short Version

Adapted from: http://code.activestate.com/recipes/576725-brute-force-sudoku-solver/

```d
import std.stdio, std.algorithm, std.range;

const(int)[] solve(immutable int[] s) pure nothrow @safe {
    immutable i = s.countUntil(0);
    if (i == -1)
        return s;

    enum B = (int i, int j) => i / 27 ^ j / 27 | (i%9 / 3 ^ j%9 / 3);
    immutable c = iota(81)
                  .filter!(j => !((i - j) % 9 * (i/9 ^ j/9) * B(i, j)))
                  .map!(j => s[j]).array;

    foreach (immutable v; 1 .. 10)
        if (!c.canFind(v)) {
            const r = solve(s[0 .. i] ~ v ~ s[i + 1 .. $]);
            if (!r.empty)
                return r;
        }
    return null;
}

void main() {
    immutable problem = [
        8, 5, 0, 0, 0, 2, 4, 0, 0,
        7, 2, 0, 0, 0, 0, 0, 0, 9,
        0, 0, 4, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 1, 0, 7, 0, 0, 2,
        3, 0, 5, 0, 0, 0, 9, 0, 0,
        0, 4, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 8, 0, 0, 7, 0,
        0, 1, 7, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 3, 6, 0, 4, 0];
    writefln("%(%s\n%)", problem.solve.chunks(9));
}
```

{{out}}

```txt
[8, 5, 9, 6, 1, 2, 4, 3, 7]
[7, 2, 3, 8, 5, 4, 1, 6, 9]
[1, 6, 4, 3, 7, 9, 5, 2, 8]
[9, 8, 6, 1, 4, 7, 3, 5, 2]
[3, 7, 5, 2, 6, 8, 9, 1, 4]
[2, 4, 1, 5, 9, 3, 7, 8, 6]
[4, 3, 2, 9, 8, 1, 6, 7, 5]
[6, 1, 7, 4, 2, 5, 8, 9, 3]
[5, 9, 8, 7, 3, 6, 2, 4, 1]
```


===No-Heap Version===
This version is similar to the precedent one, but it shows idioms to avoid memory allocations on the heap. This is enforced by the use of the @nogc attribute.

```d
import std.stdio, std.algorithm, std.range, std.typecons;

Nullable!(const ubyte[81]) solve(in ubyte[81] s) pure nothrow @safe @nogc {
    immutable i = s[].countUntil(0);
    if (i == -1)
        return typeof(return)(s);

    static immutable B = (in int i, in int j) pure nothrow @safe @nogc =>
        i / 27 ^ j / 27 | (i % 9 / 3 ^ j % 9 / 3);

    ubyte[81] c = void;
    size_t len = 0;
    foreach (immutable int j; 0 .. c.length)
        if (!((i - j) % 9 * (i/9 ^ j/9) * B(i, j)))
            c[len++] = s[j];

    foreach (immutable ubyte v; 1 .. 10)
        if (!c[0 .. len].canFind(v)) {
            ubyte[81] s2 = void;
            s2[0 .. i] = s[0 .. i];
            s2[i] = v;
            s2[i + 1 .. $] = s[i + 1 .. $];
            const r = solve(s2);
            if (!r.isNull)
                return typeof(return)(r);
        }
    return typeof(return)();
}

void main() {
    immutable ubyte[81] problem = [
        8, 5, 0, 0, 0, 2, 4, 0, 0,
        7, 2, 0, 0, 0, 0, 0, 0, 9,
        0, 0, 4, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 1, 0, 7, 0, 0, 2,
        3, 0, 5, 0, 0, 0, 9, 0, 0,
        0, 4, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 8, 0, 0, 7, 0,
        0, 1, 7, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 3, 6, 0, 4, 0];
    writefln("%(%s\n%)", problem.solve.get[].chunks(9));
}
```

Same output.


## Delphi

Example taken from C++

```delphi
type
  TIntArray = array of Integer;

  { TSudokuSolver }

  TSudokuSolver = class
  private
    FGrid: TIntArray;

    function CheckValidity(val: Integer; x: Integer; y: Integer): Boolean;
    function ToString: string; reintroduce;
    function PlaceNumber(pos: Integer): Boolean;
  public
    constructor Create(s: string);

    procedure Solve;
  end;

implementation

uses
  Dialogs;

{ TSudokuSolver }

function TSudokuSolver.CheckValidity(val: Integer; x: Integer; y: Integer
  ): Boolean;
var
  i: Integer;
  j: Integer;
  StartX: Integer;
  StartY: Integer;
begin
  for i := 0 to 8 do
  begin
    if (FGrid[y * 9 + i] = val) or
       (FGrid[i * 9 + x] = val) then
    begin
      Result := False;
      Exit;
    end;
  end;
  StartX := (x div 3) * 3;
  StartY := (y div 3) * 3;
  for i := StartY to Pred(StartY + 3) do
  begin
    for j := StartX to Pred(StartX + 3) do
    begin
      if FGrid[i * 9 + j] = val then
      begin
        Result := False;
        Exit;
      end;
    end;
  end;
  Result := True;
end;

function TSudokuSolver.ToString: string;
var
  sb: string;
  i: Integer;
  j: Integer;
  c: char;
begin
  sb := '';
  for i := 0 to 8 do
  begin
    for j := 0 to 8 do
    begin
      c := (IntToStr(FGrid[i * 9 + j]) + '0')[1];
      sb := sb + c + ' ';
      if (j = 2) or (j = 5) then sb := sb + '| ';
    end;
    sb := sb + #13#10;
    if (i = 2) or (i = 5) then
      sb := sb + '-----+-----+-----' + #13#10;
  end;
  Result := sb;
end;

function TSudokuSolver.PlaceNumber(pos: Integer): Boolean;
var
  n: Integer;
begin
  Result := False;
  if Pos = 81 then
  begin
    Result := True;
    Exit;
  end;
  if FGrid[pos] > 0 then
  begin
    Result := PlaceNumber(Succ(pos));
    Exit;
  end;
  for n := 1 to 9 do
  begin
    if CheckValidity(n, pos mod 9, pos div 9) then
    begin
      FGrid[pos] := n;
      Result := PlaceNumber(Succ(pos));
      if not Result then
        FGrid[pos] := 0;
    end;
  end;
end;

constructor TSudokuSolver.Create(s: string);
var
  lcv: Cardinal;
begin
  SetLength(FGrid, 81);
  for lcv := 0 to Pred(Length(s)) do
    FGrid[lcv] := StrToInt(s[Succ(lcv)]);
end;

procedure TSudokuSolver.Solve;
begin
  if not PlaceNumber(0) then
    ShowMessage('Unsolvable')
  else
    ShowMessage('Solved!');
  end;
end;
```

Usage:

```delphi
var
  SudokuSolver: TSudokuSolver;
begin
  SudokuSolver := TSudokuSolver.Create('850002400' +
                                       '720000009' +
                                       '004000000' +
                                       '000107002' +
                                       '305000900' +
                                       '040000000' +
                                       '000080070' +
                                       '017000000' +
                                       '000036040');
  try
    SudokuSolver.Solve;
  finally
    FreeAndNil(SudokuSolver);
  end;
end;
```



## Elixir

{{trans|Erlang}}

```elixir
defmodule Sudoku do
  def display( grid ), do: ( for y <- 1..9, do: display_row(y, grid) )

  def start( knowns ), do: Enum.into( knowns, Map.new )

  def solve( grid ) do
    sure = solve_all_sure( grid )
    solve_unsure( potentials(sure), sure )
  end

  def task( knowns ) do
    IO.puts "start"
    start = start( knowns )
    display( start )
    IO.puts "solved"
    solved = solve( start )
    display( solved )
    IO.puts ""
  end

  defp bt( grid ), do: bt_reject( is_not_allowed(grid), grid )

  defp bt_accept( true, board ), do: throw( {:ok, board} )
  defp bt_accept( false, grid ), do: bt_loop( potentials_one_position(grid), grid )

  defp bt_loop( {position, values}, grid ), do: ( for x <- values, do: bt( Map.put(grid, position, x) ) )

  defp bt_reject( true, _grid ), do: :backtrack
  defp bt_reject( false, grid ), do: bt_accept( is_all_correct(grid), grid )

  defp display_row( row, grid ) do
    for x <- [1, 4, 7], do: display_row_group( x, row, grid )
    display_row_nl( row )
  end

  defp display_row_group( start, row, grid ) do
    Enum.each(start..start+2, &IO.write " #{Map.get( grid, {&1, row}, ".")}")
    IO.write " "
  end

  defp display_row_nl( n ) when n in [3,6,9], do: IO.puts "\n"
  defp display_row_nl( _n ), do: IO.puts ""

  defp is_all_correct( grid ), do: map_size( grid ) == 81

  defp is_not_allowed( grid ) do
    is_not_allowed_rows( grid ) or is_not_allowed_columns( grid ) or is_not_allowed_groups( grid )
  end

  defp is_not_allowed_columns( grid ), do: values_all_columns(grid) |> Enum.any?(&is_not_allowed_values/1)

  defp is_not_allowed_groups( grid ),  do: values_all_groups(grid)  |> Enum.any?(&is_not_allowed_values/1)

  defp is_not_allowed_rows( grid ),    do: values_all_rows(grid)    |> Enum.any?(&is_not_allowed_values/1)

  defp is_not_allowed_values( values ), do: length( values ) != length( Enum.uniq(values) )

  defp group_positions( {x, y} ) do
    for colum <- group_positions_close(x), row <- group_positions_close(y), do: {colum, row}
  end

  defp group_positions_close( n ) when n < 4, do: [1,2,3]
  defp group_positions_close( n ) when n < 7, do: [4,5,6]
  defp group_positions_close( _n )          , do: [7,8,9]

  defp positions_not_in_grid( grid ) do
    keys = Map.keys( grid )
    for x <- 1..9, y <- 1..9, not {x, y} in keys, do: {x, y}
  end

  defp potentials_one_position( grid ) do
    Enum.min_by( potentials( grid ), fn {_position, values} -> length(values) end )
  end

  defp potentials( grid ), do: List.flatten( for x <- positions_not_in_grid(grid), do: potentials(x, grid) )

  defp potentials( position, grid ) do
    useds = potentials_used_values( position, grid )
    {position, Enum.to_list(1..9) -- useds }
  end

  defp potentials_used_values( {x, y}, grid ) do
    row_values    = (for row <- 1..9, row != x, do: {row, y})          |> potentials_values( grid )
    column_values = (for column <- 1..9, column != y, do: {x, column}) |> potentials_values( grid )
    group_values  = group_positions({x, y}) -- [ {x, y} ]              |> potentials_values( grid )
    row_values ++ column_values ++ group_values
  end

  defp potentials_values( keys, grid ) do
    for x <- keys, val = grid[x], do: val
  end

  defp values_all_columns( grid ) do
    for x <- 1..9, do:
      ( for y <- 1..9, do: {x, y} ) |> potentials_values( grid )
  end

  defp values_all_groups( grid ) do
    [[g1,g2,g3], [g4,g5,g6], [g7,g8,g9]] = for x <- [1,4,7], do: values_all_groups(x, grid)
    [g1,g2,g3,g4,g5,g6,g7,g8,g9]
  end

  defp values_all_groups( x, grid ) do
    for x_offset <- x..x+2, do: values_all_groups(x, x_offset, grid)
  end

  defp values_all_groups( _x, x_offset, grid ) do
    ( for y_offset <- group_positions_close(x_offset), do: {x_offset, y_offset} )
    |> potentials_values( grid )
  end

  defp values_all_rows( grid ) do
    for y <- 1..9, do:
      ( for x <- 1..9, do: {x, y} ) |> potentials_values( grid )
  end

  defp solve_all_sure( grid ), do: solve_all_sure( solve_all_sure_values(grid), grid )

  defp solve_all_sure( [], grid ), do: grid
  defp solve_all_sure( sures, grid ) do
    solve_all_sure( Enum.reduce(sures, grid, &solve_all_sure_store/2) )
  end

  defp solve_all_sure_values( grid ), do: (for{position, [value]} <- potentials(grid), do: {position, value} )

  defp solve_all_sure_store( {position, value}, acc ), do: Map.put( acc, position, value )

  defp solve_unsure( [], grid ), do: grid
  defp solve_unsure( _potentials, grid ) do
    try do
      bt( grid )
    catch
      {:ok, board} -> board
    end
  end
end

simple = [{{1, 1}, 3}, {{2, 1}, 9}, {{3, 1},4}, {{6, 1}, 2}, {{7, 1}, 6}, {{8, 1}, 7},
          {{4, 2}, 3}, {{7, 2}, 4},
          {{1, 3}, 5}, {{4, 3}, 6}, {{5, 3}, 9}, {{8, 3}, 2},
          {{2, 4}, 4}, {{3, 4}, 5}, {{7, 4}, 9},
          {{1, 5}, 6}, {{9, 5}, 7},
          {{3, 6}, 7}, {{7, 6}, 5}, {{8, 6}, 8},
          {{2, 7}, 1}, {{5, 7}, 6}, {{6, 7}, 7}, {{9, 7}, 8},
          {{3, 8}, 9}, {{6, 8}, 8},
          {{2, 9}, 2}, {{3, 9}, 6}, {{4, 9}, 4}, {{7, 9}, 7}, {{8, 9}, 3}, {{9, 9}, 5}]
Sudoku.task( simple )

difficult = [{{6, 2}, 3}, {{8, 2}, 8}, {{9, 2}, 5},
             {{3, 3}, 1}, {{5, 3}, 2},
             {{4, 4}, 5}, {{6, 4}, 7},
             {{3, 5}, 4}, {{7, 5}, 1},
             {{2, 6}, 9},
             {{1, 7}, 5}, {{8, 7}, 7}, {{9, 7}, 3},
             {{3, 8}, 2}, {{5, 8}, 1},
             {{5, 9}, 4}, {{9, 9}, 9}]
Sudoku.task( difficult )
```


{{out}}

```txt

start
 3 9 4  . . 2  6 7 .
 . . .  3 . .  4 . .
 5 . .  6 9 .  . 2 .

 . 4 5  . . .  9 . .
 6 . .  . . .  . . 7
 . . 7  . . .  5 8 .

 . 1 .  . 6 7  . . 8
 . . 9  . . 8  . . .
 . 2 6  4 . .  7 3 5

solved
 3 9 4  8 5 2  6 7 1
 2 6 8  3 7 1  4 5 9
 5 7 1  6 9 4  8 2 3

 1 4 5  7 8 3  9 6 2
 6 8 2  9 4 5  3 1 7
 9 3 7  1 2 6  5 8 4

 4 1 3  5 6 7  2 9 8
 7 5 9  2 3 8  1 4 6
 8 2 6  4 1 9  7 3 5


start
 . . .  . . .  . . .
 . . .  . . 3  . 8 5
 . . 1  . 2 .  . . .

 . . .  5 . 7  . . .
 . . 4  . . .  1 . .
 . 9 .  . . .  . . .

 5 . .  . . .  . 7 3
 . . 2  . 1 .  . . .
 . . .  . 4 .  . . 9

solved
 9 8 7  6 5 4  3 2 1
 2 4 6  1 7 3  9 8 5
 3 5 1  9 2 8  7 4 6

 1 2 8  5 3 7  6 9 4
 6 3 4  8 9 2  1 5 7
 7 9 5  4 6 1  8 3 2

 5 1 9  2 8 6  4 7 3
 4 7 2  3 1 9  5 6 8
 8 6 3  7 4 5  2 1 9

```



## Erlang

I first try to solve the Sudoku grid without guessing. For the guessing part I eschew spawning a process for each guess, instead opting for backtracking. It is fun trying new things.

```Erlang

-module( sudoku ).

-export( [display/1, start/1, solve/1, task/0] ).

display( Grid ) -> [display_row(Y, Grid) || Y <- lists:seq(1, 9)].
%% A known value is {{Column, Row}, Value}
%% Top left corner is {1, 1}, Bottom right corner is {9,9}
start( Knowns ) -> dict:from_list( Knowns ).

solve( Grid ) ->
	Sure = solve_all_sure( Grid ),
	solve_unsure( potentials(Sure), Sure ).

task() ->
	Simple = [{{1, 1}, 3}, {{2, 1}, 9}, {{3, 1},4}, {{6, 1}, 2}, {{7, 1}, 6}, {{8, 1}, 7},
		{{4, 2}, 3}, {{7, 2}, 4},
		{{1, 3}, 5}, {{4, 3}, 6}, {{5, 3}, 9}, {{8, 3}, 2},
		{{2, 4}, 4}, {{3, 4}, 5}, {{7, 4}, 9},
		{{1, 5}, 6}, {{9, 5}, 7},
		{{3, 6}, 7}, {{7, 6}, 5}, {{8, 6}, 8},
		{{2, 7}, 1}, {{5, 7}, 6}, {{6, 7}, 7}, {{9, 7}, 8},
		{{3, 8}, 9}, {{6, 8}, 8},
		{{2, 9}, 2}, {{3, 9}, 6}, {{4, 9}, 4}, {{7, 9}, 7}, {{8, 9}, 3}, {{9, 9}, 5}],
	task( Simple ),
	Difficult = [{{6, 2}, 3}, {{8, 2}, 8}, {{9, 2}, 5},
		{{3, 3}, 1}, {{5, 3}, 2},
		{{4, 4}, 5}, {{6, 4}, 7},
		{{3, 5}, 4}, {{7, 5}, 1},
		{{2, 6}, 9},
		{{1, 7}, 5}, {{8, 7}, 7}, {{9, 7}, 3},
		{{3, 8}, 2}, {{5, 8}, 1},
		{{5, 9}, 4}, {{9, 9}, 9}],
	task( Difficult ).



bt( Grid ) -> bt_reject( is_not_allowed(Grid), Grid ).

bt_accept( true, Board ) -> erlang:throw( {ok, Board} );
bt_accept( false, Grid ) -> bt_loop( potentials_one_position(Grid), Grid ).

bt_loop( {Position, Values}, Grid ) -> [bt( dict:store(Position, X, Grid) ) || X <- Values].

bt_reject( true, _Grid ) -> backtrack;
bt_reject( false, Grid ) -> bt_accept( is_all_correct(Grid), Grid ).

display_row( Row, Grid ) ->
	[display_row_group( X, Row, Grid ) || X <- [1, 4, 7]],
	display_row_nl( Row ).

display_row_group( Start, Row, Grid ) ->
	[io:fwrite(" ~c", [display_value(X, Row, Grid)]) || X <- [Start, Start+1, Start+2]],
	io:fwrite( " " ).

display_row_nl( N ) when N =:= 3; N =:= 6; N =:= 9 -> io:nl(), io:nl();
display_row_nl( _N ) -> io:nl().

display_value( X, Y, Grid ) -> display_value( dict:find({X, Y}, Grid) ).

display_value( error ) -> $.;
display_value( {ok, Value} ) -> Value + $0.

is_all_correct( Grid ) -> dict:size( Grid ) =:= 81.

is_not_allowed( Grid ) ->
	is_not_allowed_rows( Grid )
	orelse is_not_allowed_columns( Grid )
	orelse is_not_allowed_groups( Grid ).

is_not_allowed_columns( Grid ) -> lists:any( fun is_not_allowed_values/1, values_all_columns(Grid) ).

is_not_allowed_groups( Grid ) -> lists:any( fun is_not_allowed_values/1, values_all_groups(Grid) ).

is_not_allowed_rows( Grid ) -> lists:any( fun is_not_allowed_values/1, values_all_rows(Grid) ).

is_not_allowed_values( Values ) -> erlang:length( Values ) =/= erlang:length( lists:usort(Values) ).

group_positions( {X, Y} ) -> [{Colum, Row} || Colum <- group_positions_close(X), Row <- group_positions_close(Y)].

group_positions_close( N ) when N < 4 -> [1,2,3];
group_positions_close( N ) when N < 7 -> [4,5,6];
group_positions_close( _N ) -> [7,8,9].

positions_not_in_grid( Grid ) ->
	Keys = dict:fetch_keys( Grid ),
	[{X, Y} || X <- lists:seq(1, 9), Y <- lists:seq(1, 9), not lists:member({X, Y}, Keys)].

potentials_one_position( Grid ) ->
	[{_Shortest, Position, Values} | _T] = lists:sort( [{erlang:length(Values), Position, Values} || {Position, Values} <- potentials( Grid )] ),
	{Position, Values}.

potentials( Grid ) -> lists:flatten( [potentials(X, Grid)  || X <- positions_not_in_grid(Grid)] ).

potentials( Position, Grid ) ->
	Useds = potentials_used_values( Position, Grid ),
	{Position, [Value || Value <- lists:seq(1, 9) -- Useds]}.

potentials_used_values( {X, Y}, Grid ) ->
	Row_positions = [{Row, Y} || Row <- lists:seq(1, 9), Row =/= X],
	Row_values = potentials_values( Row_positions, Grid ),
	Column_positions = [{X, Column} || Column <- lists:seq(1, 9), Column =/= Y],
	Column_values = potentials_values( Column_positions, Grid ),
	Group_positions = lists:delete( {X, Y}, group_positions({X, Y}) ),
	Group_values = potentials_values( Group_positions, Grid ),
	Row_values ++ Column_values ++ Group_values.

potentials_values( Keys, Grid ) ->
	Row_values_unfiltered = [dict:find(X, Grid) || X <- Keys],
	[Value || {ok, Value} <- Row_values_unfiltered].

values_all_columns( Grid ) -> [values_all_columns(X, Grid) || X <- lists:seq(1, 9)].

values_all_columns( X, Grid ) ->
	Positions = [{X, Y} || Y <- lists:seq(1, 9)],
	potentials_values( Positions, Grid ).

values_all_groups( Grid ) ->
	[G123, G456, G789] = [values_all_groups(X, Grid) || X <- [1, 4, 7]],
	[G1,G2,G3] = G123,
	[G4,G5,G6] = G456,
	[G7,G8,G9] = G789,
	[G1,G2,G3,G4,G5,G6,G7,G8,G9].

values_all_groups( X, Grid ) ->[values_all_groups(X, X_offset, Grid) || X_offset <- [X, X+1, X+2]].

values_all_groups( _X, X_offset, Grid ) ->
	Positions = [{X_offset, Y_offset} || Y_offset <- group_positions_close(X_offset)],
	potentials_values( Positions, Grid ).

values_all_rows( Grid ) ->[values_all_rows(Y, Grid) || Y <- lists:seq(1, 9)].

values_all_rows( Y, Grid ) ->
	Positions = [{X, Y} || X <- lists:seq(1, 9)],
	potentials_values( Positions, Grid ).

solve_all_sure( Grid ) -> solve_all_sure( solve_all_sure_values(Grid), Grid ).

solve_all_sure( [], Grid ) -> Grid;
solve_all_sure( Sures, Grid ) -> solve_all_sure( lists:foldl(fun solve_all_sure_store/2, Grid, Sures) ).

solve_all_sure_values( Grid ) -> [{Position, Value} || {Position, [Value]} <- potentials(Grid)].

solve_all_sure_store( {Position, Value}, Acc ) -> dict:store( Position, Value, Acc ).

solve_unsure( [], Grid ) -> Grid;
solve_unsure( _Potentials, Grid ) ->
    try
    bt( Grid )

    catch
    _:{ok, Board} -> Board

    end.

task( Knowns ) ->
	io:fwrite( "Start~n" ),
	Start = start( Knowns ),
	display( Start ),
	io:fwrite( "Solved~n" ),
	Solved = solve( Start ),
	display( Solved ),
	io:nl().

```

{{out}}

```txt

5> sudoku:task().
Start
 3 9 4  . . 2  6 7 .
 . . .  3 . .  4 . .
 5 . .  6 9 .  . 2 .

 . 4 5  . . .  9 . .
 6 . .  . . .  . . 7
 . . 7  . . .  5 8 .

 . 1 .  . 6 7  . . 8
 . . 9  . . 8  . . .
 . 2 6  4 . .  7 3 5

Solved
 3 9 4  8 5 2  6 7 1
 2 6 8  3 7 1  4 5 9
 5 7 1  6 9 4  8 2 3

 1 4 5  7 8 3  9 6 2
 6 8 2  9 4 5  3 1 7
 9 3 7  1 2 6  5 8 4

 4 1 3  5 6 7  2 9 8
 7 5 9  2 3 8  1 4 6
 8 2 6  4 1 9  7 3 5


Start
 . . .  . . .  . . .
 . . .  . . 3  . 8 5
 . . 1  . 2 .  . . .

 . . .  5 . 7  . . .
 . . 4  . . .  1 . .
 . 9 .  . . .  . . .

 5 . .  . . .  . 7 3
 . . 2  . 1 .  . . .
 . . .  . 4 .  . . 9

Solved
 9 8 7  6 5 4  3 2 1
 2 4 6  1 7 3  9 8 5
 3 5 1  9 2 8  7 4 6

 1 2 8  5 3 7  6 9 4
 6 3 4  8 9 2  1 5 7
 7 9 5  4 6 1  8 3 2

 5 1 9  2 8 6  4 7 3
 4 7 2  3 1 9  5 6 8
 8 6 3  7 4 5  2 1 9

```



## ERRE

Sudoku solver. Program solves Sudoku grid with an iterative method: it's taken from ERRE distribution disk and so comments are in Italian. Grid data are contained in the file
SUDOKU.TXT

Example of SUDOKU.TXT

503600009

010002600

900000080

000700005

006804100

200003000

030000008

004300050

800006702

0 is the empty cell.


```ERRE

!--------------------------------------------------------------------
! risolve Sudoku: in input il file SUDOKU.TXT
! Metodo seguito : cancellazioni successive e quando non possibile
!                  ricerca combinatoria sulle celle con due valori
!                  possibili - max. 30 livelli di ricorsione
!                  Non risolve se,dopo l'analisi per la cancellazione,
!                  restano solo celle a 4 valori
!--------------------------------------------------------------------

PROGRAM SUDOKU

LABEL 76,77,88,91,97,99

DIM TAV$[9,9]             ! 81 caselle in nove quadranti
                          ! cella non definita --> 0/. nel file SUDOKU.TXT
                          ! diventa 123456789 dopo LEGGI_SCHEMA

!---------------------------------------------------------------------------
! tabelle per gestire la ricerca combinatoria
! (primo indice--> livelli ricorsione)
!---------------------------------------------------------------------------
DIM TAV2$[30,9,9],INFO[30,4]

!$INCLUDE="PC.LIB"

PROCEDURE MESSAGGI(MEX%)
     CASE MEX% OF
       1-> LOCATE(21,1) PRINT("Cancellazione successiva - liv. 1") END ->
       2-> LOCATE(21,1) PRINT("Cancellazione successiva - liv. 2") END ->
       3-> LOCATE(22,1) PRINT("Ricerca combinatoria - liv.";LIVELLO;"   ") END ->
     END CASE
END PROCEDURE

PROCEDURE VISUALIZZA_SCHEMA
   LOCATE(1,1)
   PRINT("+---+---+---+---+---+---+---+---+----+")
   FOR I=1 TO 9 DO
       FOR J=1 TO 9 DO
            PRINT("|";)
            IF LEN(TAV$[I,J])=1 THEN
                  PRINT(" ";TAV$[I,J];" ";)
               ELSE
                  PRINT("   ";)
            END IF
       END FOR
       PRINT("³")
       IF I<>9 THEN PRINT("+---+---+---+---+---+---+---+---+----+") END IF
   END FOR
   PRINT("+---+---+---+---+---+---+---+---+----+")
END PROCEDURE

!------------------------------------------------------------------------
! in input  la cella (riga,colonna)
! in output se ha un valore definito
!------------------------------------------------------------------------
PROCEDURE VALORE_DEFINITO
   FLAG%=FALSE
   IF LEN(TAV$[RIGA,COLONNA])=1 THEN FLAG%=TRUE END IF
END PROCEDURE


PROCEDURE SALVA_CONFIG
     LIVELLO=LIVELLO+1
     FOR R=1 TO 9 DO
         FOR S=1 TO 9 DO
             TAV2$[LIVELLO,R,S]=TAV$[R,S]
         END FOR
     END FOR
     INFO[LIVELLO,0]=1 INFO[LIVELLO,1]=RIGA INFO[LIVELLO,2]=COLONNA
     INFO[LIVELLO,3]=SECOND INFO[LIVELLO,4]=THIRD
END PROCEDURE

PROCEDURE RIPRISTINA_CONFIG
91:
     LIVELLO=LIVELLO-1
     IF INFO[LIVELLO,0]=3 THEN GOTO 91 END IF
     FOR R=1 TO 9 DO
         FOR S=1 TO 9 DO
             TAV$[R,S]=TAV2$[LIVELLO,R,S]
         END FOR
     END FOR
     RIGA=INFO[LIVELLO,1] COLONNA=INFO[LIVELLO,2]
     SECOND=INFO[LIVELLO,3] THIRD=INFO[LIVELLO,4]
     IF INFO[LIVELLO,0]=1 THEN
         TAV$[RIGA,COLONNA]=MID$(STR$(SECOND),2)
     END IF
     IF INFO[LIVELLO,0]=2 THEN
         IF THIRD<>0 THEN
                TAV$[RIGA,COLONNA]=MID$(STR$(THIRD),2)
            ELSE
                GOTO 91
         END IF
     END IF
     INFO[LIVELLO,0]=INFO[LIVELLO,0]+1
     VISUALIZZA_SCHEMA
END PROCEDURE

PROCEDURE VERIFICA_SE_FINITO
    COMPLETO%=TRUE
    FOR RIGA=1 TO 9 DO
        PRD#=1
        FOR COLONNA=1 TO 9 DO
            PRD#=PRD#*VAL(TAV$[RIGA,COLONNA])
        END FOR
        IF PRD#<>362880 THEN COMPLETO%=FALSE EXIT END IF
    END FOR
    IF NOT COMPLETO% THEN EXIT PROCEDURE END IF
    FOR COLONNA=1 TO 9 DO
        PRD#=1
        FOR RIGA=1 TO 9 DO
            PRD#=PRD#*VAL(TAV$[RIGA,COLONNA])
        END FOR
        IF PRD#<>362880 THEN COMPLETO%=FALSE EXIT END IF
    END FOR
END PROCEDURE

!-------------------------------------------------------------------
! toglie i valore certi dalle celle sulla
! stessa riga-stessa colonna-stesso quadrante
!-------------------------------------------------------------------
PROCEDURE TOGLI_VALORE

!iniziamo a togliere il valore dalla stessa riga ....
     FOR J=1 TO 9 DO
         CH$=TAV$[RIGA,J] CH=VAL(Z$)
         IF LEN(CH$)<>1 THEN
            CHANGE(CH$,CH,"-"->CH$)
            TAV$[RIGA,J]=CH$
         END IF
     END FOR
!... iniziamo a togliere il valore dalla stessa colonna ...
     FOR I=1 TO 9 DO
         CH$=TAV$[I,COLONNA] CH=VAL(Z$)
         IF LEN(CH$)<>1 THEN
            CHANGE(CH$,CH,"-"->CH$)
            TAV$[I,COLONNA]=CH$
         END IF
     END FOR
!... iniziamo a togliere il valore dallo stesso quadrante
     R=INT(RIGA/3.1)*3+1
     S=INT(COLONNA/3.1)*3+1
     FOR I=R TO R+2 DO
        FOR J=S TO S+2 DO
          CH$=TAV$[I,J] CH=VAL(Z$)
          IF LEN(CH$)<>1 THEN
             CHANGE(CH$,CH,"-"->CH$)
             TAV$[I,J]=CH$
          END IF
        END FOR
     END FOR
     MESSAGGI(1)
END PROCEDURE

PROCEDURE ESAMINA_SCHEMA
     FOR RIGA=1 TO 9 DO
        FOR COLONNA=1 TO 9 DO
           VALORE_DEFINITO
           IF FLAG% THEN
               Z$=TAV$[RIGA,COLONNA]
               TOGLI_VALORE
           END IF
        END FOR
     END FOR
END PROCEDURE

PROCEDURE IDENTIFICA_UNICO
     FOR KL=1 TO 9 DO
        KL$=MID$(STR$(KL),2)
        NN=0
        FOR H=1 TO LEN(ZZ$) DO
           IF MID$(ZZ$,H,1)=KL$ THEN NN=NN+1 END IF
        END FOR
        IF NN=1 THEN Q=INSTR(ZZ$,KL$) KL=9 END IF
     END FOR
END PROCEDURE

!----------------------------------------------------------------------------
! intercetta i valori unici per le celle ancora non definite
!----------------------------------------------------------------------------
PROCEDURE TOGLI_VALORE2

     MESSAGGI(2)
! iniziamo dalle righe ....
     OK%=FALSE
     FOR RIGA=1 TO 9 DO
        ZZ$=""
        FOR COLONNA=1 TO 9 DO
            IF LEN(TAV$[RIGA,COLONNA])<>1 THEN
                 ZZ$=ZZ$+TAV$[RIGA,COLONNA]
              ELSE
                 ZZ$=ZZ$+STRING$(9," ")
            END IF
        END FOR
        Q=0 IDENTIFICA_UNICO
        IF Q<>0 THEN
            COLONNA=INT(Q/9.1)+1
            TAV$[RIGA,COLONNA]=KL$
            OK%=TRUE EXIT
        END IF
     END FOR
     IF OK% THEN GOTO 76 END IF

! .... poi dalle colonne ....
     FOR COLONNA=1 TO 9 DO
        ZZ$=""
        FOR RIGA=1 TO 9 DO
            IF LEN(TAV$[RIGA,COLONNA])<>1 THEN
                ZZ$=ZZ$+TAV$[RIGA,COLONNA]
              ELSE
                ZZ$=ZZ$+STRING$(9," ")
            END IF
        END FOR
        Q=0 IDENTIFICA_UNICO
        IF Q<>0 THEN
            RIGA=INT(Q/9.1)+1
            TAV$[RIGA,COLONNA]=KL$ OK%=TRUE EXIT
        END IF
     END FOR
     IF OK% THEN GOTO 76 END IF

!.... e infine i quadranti
     FOR QUADRANTE=1 TO 9 DO
         ZZ$=""
         CASE QUADRANTE OF
           1-> R=1 S=1 END ->
           2-> R=1 S=4 END ->
           3-> R=1 S=7 END ->
           4-> R=4 S=1 END ->
           5-> R=4 S=4 END ->
           6-> R=4 S=7 END ->
           7-> R=7 S=1 END ->
           8-> R=7 S=4 END ->
           9-> R=7 S=7 END ->
         END CASE
         FOR RIGA=R TO R+2 DO
            FOR COLONNA=S TO S+2 DO
                IF LEN(TAV$[RIGA,COLONNA])<>1 THEN
                    ZZ$=ZZ$+TAV$[RIGA,COLONNA]
                  ELSE
                    ZZ$=ZZ$+STRING$(9," ")
                END IF
            END FOR
         END FOR
         Q=0 IDENTIFICA_UNICO
         IF Q<>0 THEN
            CASE Q OF
              1..9->   ALFA=R   BETA=S   END ->
              10..18-> ALFA=R   BETA=S+1 END ->
              19..27-> ALFA=R   BETA=S+2 END ->
              28..36-> ALFA=R+1 BETA=S   END ->
              37..45-> ALFA=R+1 BETA=S+1 END ->
              46..54-> ALFA=R+1 BETA=S+2 END ->
              55..63-> ALFA=R+2 BETA=S   END ->
              64..72-> ALFA=R+2 BETA=S+1 END ->
              OTHERWISE
                 ALFA=R+2 BETA=S+2
            END CASE
77:
            TAV$[ALFA,BETA]=KL$ EXIT
         END IF
     END FOR
76:
     MESSAGGI(2)
END PROCEDURE

PROCEDURE CONVERTI_VALORE
    FINE%=TRUE NESSUNO%=TRUE
    FOR RIGA=1 TO 9 DO
        FOR COLONNA=1 TO 9 DO
           CH$=TAV$[RIGA,COLONNA]
           IF LEN(CH$)<>1 THEN
               FINE%=FALSE ! flag per fine partita -- trovati tutti
               Q=0         ! conta i '-' nella stringa se ce ne sono 8,
                           ! trovato valore
               FOR Z=1 TO LEN(CH$) DO
                  IF MID$(CH$,Z,1)="-" THEN Q=Q+1 ELSE LAST=Z END IF
               END FOR
               IF Q=8 THEN
                   CH$=MID$(STR$(LAST),2)
                   TAV$[RIGA,COLONNA]=CH$
                   NESSUNO%=FALSE
               END IF
           END IF
        END FOR
    END FOR
END PROCEDURE

PROCEDURE LEGGI_SCHEMA
    OPEN("I",1,"sudoku.txt")
    FOR I=1 TO 9 DO
       INPUT(LINE,#1,RIGA$)
       FOR J=1 TO 9 DO
           CH$=MID$(RIGA$,J,1)
           IF CH$="0" OR CH$="." THEN
                TAV$[I,J]="123456789"
             ELSE
                TAV$[I,J]=CH$
           END IF
       END FOR
    END FOR
CLOSE(1)
END PROCEDURE

!---------------------------------------------------------------------------
! Praticamente - visita di un albero binario (caso con cella a 2 valori
!                                             possibili)
!---------------------------------------------------------------------------
PROCEDURE RICERCA_COMBINATORIA
   TRE%=TRUE
   FOR RIGA=1 TO 9 DO
       FOR COLONNA=1 TO 9 DO
           CH$=TAV$[RIGA,COLONNA]
           IF LEN(CH$)<>1 THEN
               Q=0 FIRST=0 SECOND=0 THIRD=0
               FOR Z=1 TO LEN(CH$) DO
                  IF MID$(CH$,Z,1)="-" THEN
                     Q=Q+1
                    ELSE
                     IF FIRST=0 THEN
                         FIRST=Z
                       ELSE
                         SECOND=Z
                     END IF
                  END IF
               END FOR
               IF Q=7 THEN
                  SALVA_CONFIG
                  TAV$[RIGA,COLONNA]=MID$(STR$(FIRST),2)
                  TRE%=FALSE
                  GOTO 97
               END IF
           END IF
       END FOR
   END FOR
   IF TRE% THEN GOTO 88 END IF
97:
   MESSAGGI(3)
   EXIT PROCEDURE
88:
   QUATTRO%=TRUE
   FOR RIGA=1 TO 9 DO
       FOR COLONNA=1 TO 9 DO
           CH$=TAV$[RIGA,COLONNA]
           IF LEN(CH$)<>1 THEN
               Q=0 FIRST=0 SECOND=0 THIRD=0
               FOR Z=1 TO LEN(CH$) DO
                  IF MID$(CH$,Z,1)="-" THEN
                      Q=Q+1
                    ELSE
                      IF FIRST=0 THEN
                          FIRST=Z
                        ELSE
                          IF SECOND=0 THEN
                              SECOND=Z
                            ELSE
                              THIRD=Z
                          END IF
                      END IF
                  END IF
               END FOR
               IF Q=6 THEN
                   SALVA_CONFIG
                   TAV$[RIGA,COLONNA]=MID$(STR$(FIRST),2)
                   QUATTRO%=FALSE
                   GOTO 97
               END IF
           END IF
       END FOR
  END FOR
  IF QUATTRO% THEN
      LIVELLO=LIVELLO+1
      RIPRISTINA_CONFIG
      GOTO 97
  END IF
  ! se restano solo celle con 4 valori,forza la chiusura del ramo dell'albero
  !$RCODE="STOP"
END PROCEDURE

BEGIN
   CLS
   LIVELLO=1 NZ%=0
   LEGGI_SCHEMA
   WHILE TRUE DO
      VISUALIZZA_SCHEMA
99:
      NZ%=NZ%+1
      ESAMINA_SCHEMA
      CONVERTI_VALORE
      EXIT IF FINE%
      IF NESSUNO% THEN
          TOGLI_VALORE2
          IF OK%=0 THEN
             RICERCA_COMBINATORIA  ! cerca altri celle da assegnare
          END IF
      END IF
   END WHILE
   VISUALIZZA_SCHEMA
   VERIFICA_SE_FINITO
   IF NOT COMPLETO% THEN
       LIVELLO=LIVELLO+1
       RIPRISTINA_CONFIG
       GOTO 99
   END IF
END PROGRAM



```


=={{header|F_Sharp|F#}}==

### The Function SLPsolve


```fsharp

// Solve Sudoku Like Puzzles. Nigel Galloway: September 6th., 2018
let fN y n g=let _q n' g'=[for n in n*n'..n*n'+n-1 do for g in g*g'..g*g'+g-1 do yield (n,g)]
             let N=[|for n in 0..(y/n)-1 do for g in 0..(y/g)-1 do yield _q n g|]
             (fun n' g'->N.[((n'/n)*n+g'/g)])
let fI n=let _q g=[for n in 0..n-1 do yield (g,n)]
         let N=[|for n in 0..n-1 do yield _q n|]
         (fun g (_:int)->N.[g])
let fG n=let _q g=[for n in 0..n-1 do yield (n,g)]
         let N=[|for n in 0..n-1 do yield _q n|]
         (fun (_:int) n->N.[n])
let fE v z n g fn=let N,G,B,fn=fI z,fG z,fN z n g,readCSV ',' fn|>List.ofSeq
                  let fG n g mask=List.except (N n g@G n g@B n g) mask
                  let b=List.except(List.map(fun n->(n.row,n.col))fn)[for n in 0..z-1 do for g in 0..z-1 do yield (n,g)]
                  let q=Map.ofList[for v' in v do yield ((v'),List.choose(fun n->if n.value=v' then Some(n.row,n.col) else None)fn|>List.fold(fun z (n,g)->(n,g)::fG n g z)b)]
                  (fG,(fun n->Map.find n q),z,v)
let SLPsolve (N,G,z,l)=
  let rec nQueens col mask res=[
    if col=z then yield res else
    yield! List.filter(fun(n,_)->n=col)mask|>List.collect(fun(n,g)->nQueens (col+1) (N n g mask) ((n,g)::res))]
  let rec sudoku l res=seq{
    match l with
    |h::t->let n=nQueens 0 (List.except (List.concat res) (G h)) []
           yield! n|>Seq.collect(fun n->sudoku t (n::res))
    |_->yield res}
  sudoku l []
let printSLP n g=
  List.map2(fun n g->List.map(fun(n',g')->((n',g'),n))g) (List.rev n) g|>List.concat|>List.sortBy (fun ((_,n),_)->n)|>List.groupBy(fun ((n,_),_)->n)|>List.sortBy(fun(n,_)->n)
  |>List.iter(fun (_,n)->n|>Seq.fold(fun z ((_,g),v)->[z..g-1]|>Seq.iter(fun _->printf " |");printf "%s|" v; g+1 ) 0 |>ignore;printfn "")

```


### Demonstration

Given sud1.csv:

```txt

7,1,,4,,6,,2
,,,,7,,,,3
4,,,,,1,,8
,,,,1,3,,,9
,,1,,,,7
2,,,8,6
,2,,1,,,,,4
9,,,,8
,7,,6,,4,,5,2

```

then

```fsharp

let n=SLPsolve (fE ([1..9]|>List.map(string)) 9 3 3 "sud1.csv")
printSLP ([1..9]|>List.map(string)) (Seq.item 0 n)

```

{{out}}

```txt

7|1|8|4|3|6|9|2|5|
5|6|2|9|7|8|4|1|3|
4|3|9|5|2|1|6|8|7|
6|8|5|7|1|3|2|4|9|
3|9|1|2|4|5|7|6|8|
2|4|7|8|6|9|5|3|1|
8|2|6|1|5|7|3|9|4|
9|5|4|3|8|2|1|7|6|
1|7|3|6|9|4|8|5|2|

```


## Forth

{{works with|4tH|3.60.0}}

```forth
include lib/interprt.4th
include lib/istype.4th
include lib/argopen.4th

\  ---------------------
\  Variables
\  ---------------------

81 string sudokugrid
9 array sudoku_row
9 array sudoku_col
9 array sudoku_box

\  -------------
\  4tH interface
\  -------------

: >grid                                ( n2 a1 n1 -- n3)
  rot dup >r 9 chars * sudokugrid + dup >r swap
  0 do                                 ( a1 a2)
    over i chars + c@ dup is-digit     ( a1 a2 c f)
    if [char] 0 - over c! char+ else drop then
  loop                                 ( a1 a2)
  nip r> - 9 /  r> +                   ( n3)
;

0
s" 090004007" >grid
s" 000007900" >grid
s" 800000000" >grid
s" 405800000" >grid
s" 300000002" >grid
s" 000009706" >grid
s" 000000004" >grid
s" 003500000" >grid
s" 200600080" >grid
drop

\  ---------------------
\  Logic
\  ---------------------
\  Basically :
\     Grid is parsed. All numbers are put into sets, which are
\     implemented as bitmaps (sudoku_row, sudoku_col, sudoku_box)
\     which represent sets of numbers in each row, column, box.
\     only one specific instance of a number can exist in a
\     particular set.

\     SOLVER is recursively called
\     SOLVER looks for the next best guess using FINDNEXTSPACE
\     tries this trail down... if fails, backtracks... and tries
\     again.


\ Grid Related

: xy 9 * + ;                           \  x y -- offset ;
: getrow 9 / ;
: getcol 9 mod ;
: getbox dup getrow 3 / 3 * swap getcol 3 / + ;

\ Puts and gets numbers from/to grid only
: setnumber sudokugrid + c! ;          \ n position --
: getnumber sudokugrid + c@ ;

: cleargrid sudokugrid 81 bounds do 0 i c! loop ;

\ --------------
\ Set related: sets are sudoku_row, sudoku_col, sudoku_box

\ ie x y --   ;  adds x into bitmap y
: addbits_row cells sudoku_row + dup @ rot 1 swap lshift or swap ! ;
: addbits_col cells sudoku_col + dup @ rot 1 swap lshift or swap ! ;
: addbits_box cells sudoku_box + dup @ rot 1 swap lshift or swap ! ;

\ ie x y --  ; remove number x from bitmap y
: removebits_row cells sudoku_row + dup @ rot 1 swap lshift invert and swap ! ;
: removebits_col cells sudoku_col + dup @ rot 1 swap lshift invert and swap ! ;
: removebits_box cells sudoku_box + dup @ rot 1 swap lshift invert and swap ! ;

\ clears all bitsmaps to 0
: clearbitmaps 9 0 do i cells
                     0 over sudoku_row + !
                     0 over sudoku_col + !
                     0 swap sudoku_box + !
           loop ;

\ Adds number to grid and sets
: addnumber                            \ number position --
    2dup setnumber
    2dup getrow addbits_row
    2dup getcol addbits_col
         getbox addbits_box
;

\ Remove number from grid, and sets
: removenumber                         \ position --
    dup getnumber swap
    2dup getrow removebits_row
    2dup getcol removebits_col
    2dup getbox removebits_box
    nip 0 swap setnumber
;

\ gets bitmap at position, ie
\ position -- bitmap

: getrow_bits getrow cells sudoku_row + @ ;
: getcol_bits getcol cells sudoku_col + @ ;
: getbox_bits getbox cells sudoku_box + @ ;

\ position -- composite bitmap  (or'ed)
: getbits
    dup getrow_bits
    over getcol_bits
    rot getbox_bits or or
;

\ algorithm from c.l.f circa 1995 ? Will Baden
: countbits    ( number -- bits )
        [HEX] DUP  55555555 AND  SWAP  1 RSHIFT  55555555 AND  +
              DUP  33333333 AND  SWAP  2 RSHIFT  33333333 AND  +
              DUP  0F0F0F0F AND  SWAP  4 RSHIFT  0F0F0F0F AND  +
        [DECIMAL] 255 MOD
;

\ Try tests a number in a said position of grid
\ Returns true if it's possible, else false.
: try                                  \ number position -- true/false
      getbits 1 rot lshift and 0=
;

\ --------------
: parsegrid                            \ Parses Grid to fill sets.. Run before solver.
   sudokugrid                          \ to ensure all numbers are parsed into sets/bitmaps
   81 0 do
     dup i + c@
       dup if
         dup i try if
           i addnumber
         else
           unloop drop drop FALSE exit
         then
       else
         drop
       then
   loop
   drop
   TRUE
;

\ Morespaces? manually checks for spaces ...
\ Obviously this can be optimised to a count var, done initially
\ Any additions/subtractions made to the grid could decrement
\ a 'spaces' variable.

: morespaces?
     0  sudokugrid 81 bounds do i c@  0= if 1+ then loop ;

: findnextmove                         \  -- n ; n = index next item, if -1 finished.

   -1  10                              \  index  prev_possibilities  --
                                       \  err... yeah... local variables, kind of...

   81 0 do
      i sudokugrid + c@ 0= IF
             i getbits countbits 9 swap -

             \ get bitmap and see how many possibilities
             \ stack diagram:
             \ index prev_possibilities  new_possiblities --

             2dup > if
                                       \ if new_possibilities < prev_possibilities...
                 nip nip i swap
                                       \ new_index new_possibilies --

             else                      \ else prev_possibilities < new possibilities, so:

                 drop                  \ new_index new_possibilies --

             then
      THEN
   loop
   drop
;

\ findnextmove returns index of best next guess OR returns -1
\ if no more guesses. You then have to check to see if there are
\ spaces left on the board unoccupied. If this is the case, you
\ need to back up the recursion and try again.

: solver
     findnextmove
         dup 0< if
             morespaces? if
                drop false exit
             else
                drop true exit
             then
         then

     10 1 do
        i over try if
           i over addnumber
           recurse  if
                drop unloop TRUE EXIT
           else
                dup removenumber
           then
        then
     loop

     drop FALSE
;

\ SOLVER

: startsolving
   clearbitmaps                        \ reparse bitmaps and reparse grid
   parsegrid                           \ just in case..
   solver
   AND
;

\  ---------------------
\  Display Grid
\  ---------------------

\ Prints grid nicely

: .sudokugrid
  CR CR
  sudokugrid
  81 0 do
    dup i + c@ .
    i 1+
      dup 3 mod 0= if
         dup 9 mod 0= if
            CR
            dup 27 mod 0= if
              dup 81 < if ." ------+-------+------" CR then
            then
         else
           ." | "
         then
      then
    drop
  loop
  drop
  CR
;

\  ---------------------
\  Higher Level Words
\  ---------------------

: checkifoccupied                      ( offset -- t/f)
    sudokugrid + c@
;

: add                                  ( n x y --)
    xy 2dup
      dup checkifoccupied if
        dup removenumber
      then
    try if
      addnumber
      .sudokugrid
    else
      CR ." Not a valid move. " CR
      2drop
    then
;

: rm
    xy removenumber
    .sudokugrid
;

: clearit
    cleargrid
    clearbitmaps
    .sudokugrid
;

: solveit
  CR
  startsolving
  if
    ." Solution found!" CR .sudokugrid
  else
    ." No solution found!" CR CR
  then
;

: showit .sudokugrid ;

\ Print help menu
: help
  CR
  ." Type clearit     ; to clear grid " CR
  ."      1-9 x y add ; to add 1-9 to grid at x y (0 based) " CR
  ."      x y rm      ; to remove number at x y " CR
  ."      showit      ; redisplay grid " CR
  ."      solveit     ; to solve " CR
  ."      help        ; for help " CR
  CR
;

\  ---------------------
\  Execution starts here
\  ---------------------

: godoit
    clearbitmaps
    parsegrid if
      CR ." Grid valid!"
    else
      CR ." Warning: grid invalid!"
    then
    .sudokugrid
    help
;

\  -------------
\  4tH interface
\  -------------

: read-sudoku
  input 1 arg-open 0
  begin dup 9 < while refill while 0 parse >grid repeat
  drop close
;

: bye quit ;

create wordlist                        \ dictionary
  ," clearit" ' clearit ,
  ," add"     ' add ,
  ," rm"      ' rm ,
  ," showit"  ' showit ,
  ," solveit" ' solveit ,
  ," quit"    ' bye ,
  ," exit"    ' bye ,
  ," bye"     ' bye ,
  ," q"       ' bye ,
  ," help"    ' help ,
  NULL ,

wordlist to dictionary
:noname ." Unknown command '" type ." '" cr ; is NotFound
                                       \ sudoku interpreter
: sudoku
  argn 1 > if read-sudoku then
  godoit
  begin
    ." OK" cr
    refill drop ['] interpret
    catch if ." Error" cr then
  again
;

sudoku
```



## Fortran

{{works with|Fortran|90 and later}}
This implementation uses a brute force method. The subroutine <code>solve</code> recursively checks valid entries using the rules defined in the function <code>is_safe</code>. When <code>solve</code> is called beyond the end of the sudoku, we know that all the currently entered values are valid. Then the result is displayed.

```fortran
program sudoku

  implicit none
  integer, dimension (9, 9) :: grid
  integer, dimension (9, 9) :: grid_solved
  grid = reshape ((/               &
    & 0, 0, 3, 0, 2, 0, 6, 0, 0,   &
    & 9, 0, 0, 3, 0, 5, 0, 0, 1,   &
    & 0, 0, 1, 8, 0, 6, 4, 0, 0,   &
    & 0, 0, 8, 1, 0, 2, 9, 0, 0,   &
    & 7, 0, 0, 0, 0, 0, 0, 0, 8,   &
    & 0, 0, 6, 7, 0, 8, 2, 0, 0,   &
    & 0, 0, 2, 6, 0, 9, 5, 0, 0,   &
    & 8, 0, 0, 2, 0, 3, 0, 0, 9,   &
    & 0, 0, 5, 0, 1, 0, 3, 0, 0/), &
    & shape = (/9, 9/),            &
    & order = (/2, 1/))
  call pretty_print (grid)
  call solve (1, 1)
  write (*, *)
  call pretty_print (grid_solved)

contains

  recursive subroutine solve (i, j)
    implicit none
    integer, intent (in) :: i
    integer, intent (in) :: j
    integer :: n
    integer :: n_tmp
    if (i > 9) then
      grid_solved = grid
    else
      do n = 1, 9
        if (is_safe (i, j, n)) then
          n_tmp = grid (i, j)
          grid (i, j) = n
          if (j == 9) then
            call solve (i + 1, 1)
          else
            call solve (i, j + 1)
          end if
          grid (i, j) = n_tmp
        end if
      end do
    end if
  end subroutine solve

  function is_safe (i, j, n) result (res)
    implicit none
    integer, intent (in) :: i
    integer, intent (in) :: j
    integer, intent (in) :: n
    logical :: res
    integer :: i_min
    integer :: j_min
    if (grid (i, j) == n) then
      res = .true.
      return
    end if
    if (grid (i, j) /= 0) then
      res = .false.
      return
    end if
    if (any (grid (i, :) == n)) then
      res = .false.
      return
    end if
    if (any (grid (:, j) == n)) then
      res = .false.
      return
    end if
    i_min = 1 + 3 * ((i - 1) / 3)
    j_min = 1 + 3 * ((j - 1) / 3)
    if (any (grid (i_min : i_min + 2, j_min : j_min + 2) == n)) then
      res = .false.
      return
    end if
    res = .true.
  end function is_safe

  subroutine pretty_print (grid)
    implicit none
    integer, dimension (9, 9), intent (in) :: grid
    integer :: i
    integer :: j
    character (*), parameter :: bar = '+-----+-----+-----+'
    character (*), parameter :: fmt = '(3 ("|", i0, 1x, i0, 1x, i0), "|")'
    write (*, '(a)') bar
    do j = 0, 6, 3
      do i = j + 1, j + 3
        write (*, fmt) grid (i, :)
      end do
      write (*, '(a)') bar
    end do
  end subroutine pretty_print

end program sudoku
```

{{out}}
```txt

 +-----+-----+-----+
 |0 0 3|0 2 0|6 0 0|
 |9 0 0|3 0 5|0 0 1|
 |0 0 1|8 0 6|4 0 0|
 +-----+-----+-----+
 |0 0 8|1 0 2|9 0 0|
 |7 0 0|0 0 0|0 0 8|
 |0 0 6|7 0 8|2 0 0|
 +-----+-----+-----+
 |0 0 2|6 0 9|5 0 0|
 |8 0 0|2 0 3|0 0 9|
 |0 0 5|0 1 0|3 0 0|
 +-----+-----+-----+

 +-----+-----+-----+
 |4 8 3|9 2 1|6 5 7|
 |9 6 7|3 4 5|8 2 1|
 |2 5 1|8 7 6|4 9 3|
 +-----+-----+-----+
 |5 4 8|1 3 2|9 7 6|
 |7 2 9|5 6 4|1 3 8|
 |1 3 6|7 9 8|2 4 5|
 +-----+-----+-----+
 |3 7 2|6 8 9|5 1 4|
 |8 1 4|2 5 3|7 6 9|
 |6 9 5|4 1 7|3 8 2|
 +-----+-----+-----+
```



## FutureBasic

First is a short version:

```futurebasic

include "ConsoleWindow"
include "NSLog.incl"
include "Util_Containers.incl"

begin globals
dim as container gC
end globals

BeginCDeclaration
short solve_sudoku(short i);
short check_sudoku(short r, short c);
CFMutableStringRef print_sudoku();
EndC

BeginCFunction
short sudoku[9][9] = {
                       {3,0,0,0,0,1,4,0,9},
                       {7,0,0,0,0,4,2,0,0},
                       {0,5,0,2,0,0,0,1,0},
                       {5,7,0,0,4,3,0,6,0},
                       {0,9,0,0,0,0,0,3,0},
                       {0,6,0,7,9,0,0,8,5},
                       {0,8,0,0,0,5,0,4,0},
                       {0,0,6,4,0,0,0,0,7},
                       {9,0,5,6,0,0,0,0,3},
                     };


short check_sudoku( short r, short c )
{
  short i;
  short rr, cc;

  for (i = 0; i < 9; i++)
  {
    if (i != c && sudoku[r][i] == sudoku[r][c]) return 0;
    if (i != r && sudoku[i][c] == sudoku[r][c]) return 0;
    rr = r/3 * 3 + i/3;
    cc = c/3 * 3 + i%3;
    if ((rr != r || cc != c) && sudoku[rr][cc] == sudoku[r][c]) return 0;
  }
  return -1;
}


short solve_sudoku( short i )
{
  short r, c;

  if (i < 0) return 0;
  else if (i >= 81) return -1;

  r = i / 9;
  c = i % 9;

  if (sudoku[r][c])
    return check_sudoku(r, c) && solve_sudoku(i + 1);
  else
    for (sudoku[r][c] = 9; sudoku[r][c] > 0; sudoku[r][c]--)
    {
      if ( solve_sudoku(i) ) return -1;
    }
  return 0;
}


CFMutableStringRef print_sudoku()
{
  short i, j;
  CFMutableStringRef mutStr;

  mutStr = CFStringCreateMutable( kCFAllocatorDefault, 0 );

     for (i = 0; i < 9; i++)
     {
          for (j = 0; j < 9; j++)
          {
             CFStringAppendFormat( mutStr, NULL, (CFStringRef)@" %d", sudoku[i][j] );
          }
       CFStringAppendFormat( mutStr, NULL, (CFStringRef)@"\r" );
     }
  return( mutStr );
}
EndC

toolbox fn solve_sudoku( short i ) = short
toolbox fn check_sudoku( short r, short c ) = short
toolbox fn print_sudoku() = CFMutableStringRef

dim as short solution
dim as CFMutableStringRef cfRef

gC = " "
cfRef = fn print_sudoku()
fn ContainerCreateWithCFString( cfRef, gC )
print : print "Sudoku challenge:" : print : print gC

solution = fn solve_sudoku(0)

print : print "Sudoku solved:" : print
if ( solution )
gC = " "
cfRef = fn print_sudoku()
fn ContainerCreateWithCFString( cfRef, gC )
print gC
else
print "No solution found"
end if

```


Output:

```txt

Sudoku challenge:

 3 0 0 0 0 1 4 0 9
 7 0 0 0 0 4 2 0 0
 0 5 0 2 0 0 0 1 0
 5 7 0 0 4 3 0 6 0
 0 9 0 0 0 0 0 3 0
 0 6 0 7 9 0 0 8 5
 0 8 0 0 0 5 0 4 0
 0 0 6 4 0 0 0 0 7
 9 0 5 6 0 0 0 0 3


Sudoku solved:

 3 2 8 5 6 1 4 7 9
 7 1 9 3 8 4 2 5 6
 6 5 4 2 7 9 3 1 8
 5 7 1 8 4 3 9 6 2
 8 9 2 1 5 6 7 3 4
 4 6 3 7 9 2 1 8 5
 2 8 7 9 3 5 6 4 1
 1 3 6 4 2 8 5 9 7
 9 4 5 6 1 7 8 2 3

```


More code in this one, but faster execution:

```txt

include "ConsoleWindow"
include "Tlbx Timer.incl"

begin globals
_digits = 9
_setH = 3
_setV = 3
_nSetH = 3
_nSetV = 3

begin record Board
dim as boolean f(_digits,_digits,_digits)
dim as char    match(_digits,_digits)
dim as pointer previousBoard // singly-linked list used to discover repetitions
dim &&
end record

dim quiz as board
dim as long t
dim as double       sProgStartTime

end globals

// 'ordinary' timer used for playing
local fn Milliseconds as long // time in ms since prog start
'~'1
dim as UnsignedWide us

long if ( sProgStartTime == 0.0 )
Microseconds( @us )
sProgStartTime = 4294967296.0*us.hi + us.lo
end if
Microseconds( @us )
end fn = (4294967296.0*us.hi + us.lo - sProgStartTime)'*1e-3

local fn InitMilliseconds
'~'1
sProgStartTime = 0.0
fn Milliseconds
end fn

local mode
local fn CopyBoard( source as ^Board, dest as ^Board )
'~'1
BlockMoveData( source, dest, sizeof( Board ) )
dest.previousBoard = source // linked list
end fn

local fn prepare( b as ^Board )
'~'1
dim as short i, j, n

for i = 1 to _digits
for j = 1 to _digits
for n = 1 to _digits
b.match[i, j] = 0
b.f[i, j, n] = _true
next n
next j
next i
end fn

local fn printBoard( b as ^Board )
'~'1
dim as short i, j

for i = 1 to _digits
for j = 1 to _digits
Print b.match[i, j];
next j
print
next i
end fn

local fn verifica( b as ^Board )
'~'1
dim as short i, j, n, first, x, y, ii
dim as boolean check

check = _true

for i = 1 to _digits
for j = 1 to _digits
long if ( b.match[i, j] == 0 )
check = _false
for n = 1 to _digits
long if ( b.f[i, j, n] != _false )
check = _true
end if
next n
if ( check == _false ) then exit fn
end if
next j
next i

check = _true
for j = 1 to _digits
for n = 1 to _digits
first = 0
for i = 1 to _digits
long if ( b.match[i, j] == n )
long if ( first == 0 )
first = i
xelse
check = _false
exit fn
end if
end if
next i
next n
next j

for i = 1 to _digits
for n = 1 to _digits
first = 0
for j = 1 to _digits
long if ( b.match[i, j] == n )
long if ( first == 0 )
first = j
xelse
check = _false
exit fn
end if
end if
next j
next n
next i

for x = 0 to ( _nSetH - 1 )
for y = 0 to ( _nSetV - 1 )
first = 0
for ii = 0 to ( _digits - 1 )
i = x * _setH + ii mod _setH + 1
j = y * _setV + ii / _setH + 1
long if ( b.match[i, j] == n )
long if ( first == 0 )
first = j
xelse
check = _false
exit fn
end if
end if
next ii
next y
next x

end fn = check


local fn setCell( b as ^Board, x as short, y as short, n as short) as boolean
dim as short   i, j, rx, ry
dim as boolean check

b.match[x, y] = n
for i = 1 to _digits
b.f[x, i, n] = _false
b.f[i, y, n] = _false
next i

rx = (x - 1) / _setH
ry = (y - 1) / _setV

for i = 1 to _setH
for j = 1 to _setV
b.f[ rx * _setH + i, ry * _setV + j, n ] = _false
next j
next i

check = fn verifica( #b )
if ( check == _false ) then exit fn

end fn = check


local fn solve( b as ^Board )
dim as short i, j, n, first, x, y, ii, ppi, ppj
dim as boolean check

check = _true

for i = 1 to _digits
for j = 1 to _digits
long if ( b.match[i, j] == 0 )
first = 0
for n = 1 to _digits
long if ( b.f[i, j, n] != _false )
long if ( first == 0 )
first = n
xelse
first = -1
exit for
end if
end if
next n

long if ( first > 0 )
check = fn setCell( #b, i, j, first )
if ( check == _false ) then exit fn
check = fn solve(#b)
if ( check == _false ) then exit fn
end if

end if
next j
next i

for i = 1 to _digits
for n = 1 to _digits
first = 0

for j = 1 to _digits
if ( b.match[i, j] == n ) then exit for

long if ( b.f[i, j, n] != _false ) and ( b.match[i, j] == 0 )
long if ( first == 0 )
first = j
xelse
first = -1
exit for
end if

end if

next j

long if ( first > 0 )
check = fn setCell( #b, i, first, n )
if ( check == _false ) then exit fn
check = fn solve(#b)
if ( check == _false ) then exit fn
end if

next n
next i


for j = 1 to _digits
for n = 1 to _digits
first = 0

for i = 1 to _digits
if ( b.match[i, j] == n ) then exit for

long if ( b.f[i, j, n] != _false ) and ( b.match[i, j] == 0 )
long if ( first == 0 )
first = i
xelse
first = -1
exit for
end if

end if

next i

long if ( first > 0 )
check = fn setCell( #b, first, j, n )
if ( check == _false ) then exit fn
check = fn solve(#b)
if ( check == _false ) then exit fn
end if

next n
next j


for x = 0 to ( _nSetH - 1 )
for y = 0 to ( _nSetV - 1 )

for n = 1 to _digits
first = 0

for ii = 0 to ( _digits - 1 )

i = x * _setH + ii mod _setH + 1
j = y * _setV + ii / _setH + 1

if ( b.match[i, j] == n ) then exit for

long if ( b.f[i, j, n] != _false ) and ( b.match[i, j] == 0 )
long if ( first == 0 )
first = n
ppi = i
ppj = j
xelse
first = -1
exit for
end if
end if


next ii

long if ( first > 0 )
check = fn setCell( #b, ppi, ppj, n )
if ( check == _false ) then exit fn
check = fn solve(#b)
if ( check == _false ) then exit fn
end if

next n

next y
next x

end fn = check


local fn resolve( b as ^Board )
dim as boolean check, daFinire
dim as long i, j, n
dim as board localBoard

check = fn solve(b)

long if ( check == _false )
exit fn
end if

daFinire = _false

for i = 1 to _digits
for j = 1 to _digits
long if ( b.match[i, j] == 0 )

daFinire = _true

for n = 1 to _digits
long if ( b.f[i, j, n] != _false )

fn CopyBoard( b, @localBoard )

check = fn setCell(@localBoard, i, j, n)

long if ( check != _false )
check = fn resolve( @localBoard )
long if ( check == -1 )
fn CopyBoard( @localBoard, b )

exit fn
end if
end if

end if

next n

end if
next j
next i

long if daFinire
xelse
check = -1
end if

end fn = check


fn InitMilliseconds

fn prepare( @quiz )

DATA 0,0,0,0,2,9,0,8,7
DATA 0,9,7,3,0,0,0,0,0
DATA 0,0,2,0,0,0,4,0,9
DATA 0,0,3,9,0,1,0,0,6
DATA 0,4,0,0,0,0,0,9,0
DATA 9,0,0,7,0,3,1,0,0
DATA 0,0,9,0,0,0,6,0,0
DATA 0,0,0,0,0,5,8,2,0
DATA 2,8,0,1,3,0,0,0,0

dim as short i, j, d
for i = 1 to _digits
for j = 1 to _digits
read d
fn setCell(@quiz, j, i, d)
next j
next i

Print : print "quiz:"
fn printBoard( @quiz )
print : print "-------------------" : print
dim as boolean check

t = fn Milliseconds
check = fn resolve(@quiz)
t = fn Milliseconds - t

if ( check )
print "solution:"; str$( t/1000.0 ) + " ms"
else
print "No solution found"
end if
fn printBoard( @quiz )

```


Output:

```txt

quiz:
 0 0 0 0 0 9 0 0 2
 0 9 0 0 4 0 0 0 8
 0 7 2 3 0 0 9 0 0
 0 3 0 9 0 7 0 0 1
 2 0 0 0 0 0 0 0 3
 9 0 0 1 0 3 0 5 0
 0 0 4 0 0 1 6 8 0
 8 0 0 0 9 0 0 2 0
 7 0 9 6 0 0 0 0 0

-------------------

solution: 6.956 ms
 3 8 6 5 7 9 4 1 2
 1 9 5 2 4 6 3 7 8
 4 7 2 3 1 8 9 6 5
 6 3 8 9 5 7 2 4 1
 2 5 1 8 6 4 7 9 3
 9 4 7 1 2 3 8 5 6
 5 2 4 7 3 1 6 8 9
 8 6 3 4 9 5 1 2 7
 7 1 9 6 8 2 5 3 4

```



## Go

Solution using [http://en.wikipedia.org/wiki/Dancing_Links Knuth's DLX.]
This code follows his paper fairly closely.
Input to function solve is an 81 character string.
This seems to be a conventional computer representation for Sudoku puzzles.

```go
package main

import "fmt"

// sudoku puzzle representation is an 81 character string
var puzzle = "" +
    "394  267 " +
    "   3  4  " +
    "5  69  2 " +
    " 45   9  " +
    "6       7" +
    "  7   58 " +
    " 1  67  8" +
    "  9  8   " +
    " 264  735"

func main() {
    printGrid("puzzle:", puzzle)
    if s := solve(puzzle); s == "" {
        fmt.Println("no solution")
    } else {
        printGrid("solved:", s)
    }
}

// print grid (with title) from 81 character string
func printGrid(title, s string) {
    fmt.Println(title)
    for r, i := 0, 0; r < 9; r, i = r+1, i+9 {
        fmt.Printf("%c %c %c | %c %c %c | %c %c %c\n", s[i], s[i+1], s[i+2],
            s[i+3], s[i+4], s[i+5], s[i+6], s[i+7], s[i+8])
        if r == 2 || r == 5 {
            fmt.Println("------+-------+------")
        }
    }
}

// solve puzzle in 81 character string format.
// if solved, result is 81 character string.
// if not solved, result is the empty string.
func solve(u string) string {
    // construct an dlx object with 324 constraint columns.
    // other than the number 324, this is not specific to sudoku.
    d := newDlxObject(324)
    // now add constraints that define sudoku rules.
    for r, i := 0, 0; r < 9; r++ {
        for c := 0; c < 9; c, i = c+1, i+1 {
            b := r/3*3 + c/3
            n := int(u[i] - '1')
            if n >= 0 && n < 9 {
                d.addRow([]int{i, 81 + r*9 + n, 162 + c*9 + n,
                    243 + b*9 + n})
            } else {
                for n = 0; n < 9; n++ {
                    d.addRow([]int{i, 81 + r*9 + n, 162 + c*9 + n,
                        243 + b*9 + n})
                }
            }
        }
    }
    // run dlx.  not sudoku specific.
    d.search()
    // extract the sudoku-specific 81 character result from the dlx solution.
    return d.text()
}

// Knuth's data object
type x struct {
    c          *y
    u, d, l, r *x
    // except x0 is not Knuth's.  it's pointer to first constraint in row,
    // so that the sudoku string can be constructed from the dlx solution.
    x0 *x
}

// Knuth's column object
type y struct {
    x
    s int // size
    n int // name
}

// an object to hold the matrix and solution
type dlx struct {
    ch []y  // all column headers
    h  *y   // ch[0], the root node
    o  []*x // solution
}

// constructor creates the column headers but no rows.
func newDlxObject(nCols int) *dlx {
    ch := make([]y, nCols+1)
    h := &ch[0]
    d := &dlx{ch, h, nil}
    h.c = h
    h.l = &ch[nCols].x
    ch[nCols].r = &h.x
    nh := ch[1:]
    for i := range ch[1:] {
        hi := &nh[i]
        ix := &hi.x
        hi.n = i
        hi.c = hi
        hi.u = ix
        hi.d = ix
        hi.l = &h.x
        h.r = ix
        h = hi
    }
    return d
}

// rows define constraints
func (d *dlx) addRow(nr []int) {
    if len(nr) == 0 {
        return
    }
    r := make([]x, len(nr))
    x0 := &r[0]
    for x, j := range nr {
        ch := &d.ch[j+1]
        ch.s++
        np := &r[x]
        np.c = ch
        np.u = ch.u
        np.d = &ch.x
        np.l = &r[(x+len(r)-1)%len(r)]
        np.r = &r[(x+1)%len(r)]
        np.u.d, np.d.u, np.l.r, np.r.l = np, np, np, np
        np.x0 = x0
    }
}

// extracts 81 character sudoku string
func (d *dlx) text() string {
    b := make([]byte, len(d.o))
    for _, r := range d.o {
        x0 := r.x0
        b[x0.c.n] = byte(x0.r.c.n%9) + '1'
    }
    return string(b)
}

// the dlx algorithm
func (d *dlx) search() bool {
    h := d.h
    j := h.r.c
    if j == h {
        return true
    }
    c := j
    for minS := j.s; ; {
        j = j.r.c
        if j == h {
            break
        }
        if j.s < minS {
            c, minS = j, j.s
        }
    }

    cover(c)
    k := len(d.o)
    d.o = append(d.o, nil)
    for r := c.d; r != &c.x; r = r.d {
        d.o[k] = r
        for j := r.r; j != r; j = j.r {
            cover(j.c)
        }
        if d.search() {
            return true
        }
        r = d.o[k]
        c = r.c
        for j := r.l; j != r; j = j.l {
            uncover(j.c)
        }
    }
    d.o = d.o[:len(d.o)-1]
    uncover(c)
    return false
}

func cover(c *y) {
    c.r.l, c.l.r = c.l, c.r
    for i := c.d; i != &c.x; i = i.d {
        for j := i.r; j != i; j = j.r {
            j.d.u, j.u.d = j.u, j.d
            j.c.s--
        }
    }
}

func uncover(c *y) {
    for i := c.u; i != &c.x; i = i.u {
        for j := i.l; j != i; j = j.l {
            j.c.s++
            j.d.u, j.u.d = j, j
        }
    }
    c.r.l, c.l.r = &c.x, &c.x
}
```

{{out}}

```txt

puzzle:
3 9 4 |     2 | 6 7
      | 3     | 4
5     | 6 9   |   2
------+-------+------
  4 5 |       | 9
6     |       |     7
    7 |       | 5 8
------+-------+------
  1   |   6 7 |     8
    9 |     8 |
  2 6 | 4     | 7 3 5
solved:
3 9 4 | 8 5 2 | 6 7 1
2 6 8 | 3 7 1 | 4 5 9
5 7 1 | 6 9 4 | 8 2 3
------+-------+------
1 4 5 | 7 8 3 | 9 6 2
6 8 2 | 9 4 5 | 3 1 7
9 3 7 | 1 2 6 | 5 8 4
------+-------+------
4 1 3 | 5 6 7 | 2 9 8
7 5 9 | 2 3 8 | 1 4 6
8 2 6 | 4 1 9 | 7 3 5

```



## Groovy

'''Adaptive "Non-guessing Then Guessing" Solution'''

Non-guessing part is iterative. Guessing part is recursive. Implementation uses exception handling to back out of bad guesses.

I consider this a "brute force" solution of sorts, in that it is the same method I use when solving Sudokus manually.

```groovy
final CELL_VALUES = ('1'..'9')

class GridException extends Exception {
    GridException(String message) { super(message) }
}

def string2grid = { string ->
    assert string.size() == 81
    (0..8).collect { i -> (0..8).collect { j -> string[9*i+j] } }
}

def gridRow = { grid, slot -> grid[slot.i] as Set }

def gridCol = { grid, slot -> grid.collect { it[slot.j] } as Set }

def gridBox = { grid, slot ->
    def t, l; (t, l) = [slot.i.intdiv(3)*3, slot.j.intdiv(3)*3]
    (0..2).collect { row -> (0..2).collect { col -> grid[t+row][l+col] } }.flatten() as Set
}

def slotList = { grid ->
    def slots = (0..8).collect { i -> (0..8).findAll { j -> grid[i][j] == '.' } \
            .collect {j -> [i: i, j: j] } }.flatten()
}

def assignCandidates = { grid, slots = slotList(grid) ->
    slots.each { slot ->
        def unavailable = [gridRow, gridCol, gridBox].collect { it(grid, slot) }.sum() as Set
        slot.candidates = CELL_VALUES - unavailable
    }
    slots.sort { - it.candidates.size() }
    if (slots && ! slots[-1].candidates) {
        throw new GridException('Invalid Sudoku Grid, overdetermined slot: ' + slots[-1])
    }
    slots
}

def isSolved = { grid -> ! (grid.flatten().find { it == '.' }) }

def solve
solve = { grid ->
    def slots = assignCandidates(grid)
    if (! slots) { return grid }
    while (slots[-1].candidates.size() == 1) {
        def slot = slots.pop()
        grid[slot.i][slot.j] = slot.candidates[0]
        if (! slots) { return grid }
        slots = assignCandidates(grid, slots)
    }
    if (! slots) { return grid }
    def slot = slots.pop()
    slot.candidates.each {
        if (! isSolved(grid)) {
            try {
                def sGrid = grid.collect { row -> row.collect { cell -> cell } }
                sGrid[slot.i][slot.j] = it
                grid = solve(sGrid)
            } catch (GridException ge) {
                grid[slot.i][slot.j] = '.'
            }
        }
    }
    if (!isSolved(grid)) {
        slots = assignCandidates(grid)
        throw new GridException('Invalid Sudoku Grid, underdetermined slots: ' + slots)
    }
    grid
}
```

'''Test/Benchmark Cases'''

Mentions of ''"exceptionally difficult" example in Wikipedia'' refer to this (former) page: [[https://en.wikipedia.org/w/index.php?title=Sudoku_solving_algorithms&oldid=410240496#Exceptionally_difficult_Sudokus_.28hardest_Sudokus.29 Exceptionally difficult Sudokus]]

```groovy
def sudokus = [
  //Used in Curry solution:                             ~ 0.1 seconds
    '819..5.....2...75..371.4.6.4..59.1..7..3.8..2..3.62..7.5.7.921..64...9.....2..438',

  //Used in Perl and PicoLisp solutions:                ~ 0.1 seconds
    '53..247....2...8..1..7.39.2..8.72.49.2.98..7.79.....8.....3.5.696..1.3...5.69..1.',

  //Used in Fortran solution:                           ~ 0.1 seconds
    '..3.2.6..9..3.5..1..18.64....81.29..7.......8..67.82....26.95..8..2.3..9..5.1.3..',

  //Used in many other solutions, notably Algol 68:     ~ 0.1 seconds
    '394..267....3..4..5..69..2..45...9..6.......7..7...58..1..67..8..9..8....264..735',

  //Used in C# solution:                                ~ 0.2 seconds
    '97.3...6..6.75.........8.5.......67.....3.....539..2..7...25.....2.1...8.4...73..',

  //Used in Oz solution:                                ~ 0.2 seconds
    '4......6.5...8.9..3....1....2.7....1.9.....4.8....3.5....2....7..6.5...8.1......6',

  //Used in many other solutions, notably C++:          ~ 0.3 seconds
    '85...24..72......9..4.........1.7..23.5...9...4...........8..7..17..........36.4.',

  //Used in VBA solution:                               ~ 0.3 seconds
    '..1..5.7.92.6.......8...6...9..2.4.1.........3.4.8..9...7...3.......7.69.1.8..7..',

  //Used in Forth solution:                             ~ 0.8 seconds
    '.9...4..7.....79..8........4.58.....3.......2.....97.6........4..35.....2..6...8.',

  //3rd "exceptionally difficult" example in Wikipedia: ~ 2.3 seconds
    '12.3....435....1....4........54..2..6...7.........8.9...31..5.......9.7.....6...8',

  //Used in Curry solution:                             ~ 2.4 seconds
    '9..2..5...4..6..3...3.....6...9..2......5..8...7..4..37.....1...5..2..4...1..6..9',

  //"AL Escargot", so-called "hardest sudoku" (HA!):    ~ 3.0 seconds
    '1....7.9..3..2...8..96..5....53..9...1..8...26....4...3......1..4......7..7...3..',

  //1st "exceptionally difficult" example in Wikipedia: ~ 6.5 seconds
    '12.4..3..3...1..5...6...1..7...9.....4.6.3.....3..2...5...8.7....7.....5.......98',

  //Used in Bracmat and Scala solutions:                ~ 6.7 seconds
    '..............3.85..1.2.......5.7.....4...1...9.......5......73..2.1........4...9',

  //2nd "exceptionally difficult" example in Wikipedia: ~ 8.8 seconds
    '.......39.....1..5..3.5.8....8.9...6.7...2...1..4.......9.8..5..2....6..4..7.....',

  //Used in MATLAB solution:                            ~15   seconds
    '....839..1......3...4....7..42.3....6.......4....7..1..2........8...92.....25...6',

  //4th "exceptionally difficult" example in Wikipedia: ~29   seconds
    '..3......4...8..36..8...1...4..6..73...9..........2..5..4.7..686........7..6..5..']

sudokus.each { sudoku ->
    def grid = string2grid(sudoku)
    println '\nPUZZLE'
    grid.each { println it }

    println '\nSOLUTION'
    def start = System.currentTimeMillis()
    def solution = solve(grid)
    def elapsed = (System.currentTimeMillis() - start)/1000
    solution.each { println it }
    println "\nELAPSED: ${elapsed} seconds"
}
```


{{out}} (last only):

```txt
PUZZLE
[., ., 3, ., ., ., ., ., .]
[4, ., ., ., 8, ., ., 3, 6]
[., ., 8, ., ., ., 1, ., .]
[., 4, ., ., 6, ., ., 7, 3]
[., ., ., 9, ., ., ., ., .]
[., ., ., ., ., 2, ., ., 5]
[., ., 4, ., 7, ., ., 6, 8]
[6, ., ., ., ., ., ., ., .]
[7, ., ., 6, ., ., 5, ., .]

SOLUTION
[1, 2, 3, 4, 5, 6, 7, 8, 9]
[4, 5, 7, 1, 8, 9, 2, 3, 6]
[9, 6, 8, 3, 2, 7, 1, 5, 4]
[2, 4, 9, 5, 6, 1, 8, 7, 3]
[5, 7, 6, 9, 3, 8, 4, 1, 2]
[8, 3, 1, 7, 4, 2, 6, 9, 5]
[3, 1, 4, 2, 7, 5, 9, 6, 8]
[6, 9, 5, 8, 1, 4, 3, 2, 7]
[7, 8, 2, 6, 9, 3, 5, 4, 1]

ELAPSED: 28.978 seconds
```



## Haskell

Visit the Haskell wiki [http://haskell.org/haskellwiki/Sudoku Sudoku]


## J

See [[j:Essays/Sudoku|Solving Sudoku in J]].


## Java


```java
public class Sudoku
{
    private int mBoard[][];
    private int mBoardSize;
    private int mBoxSize;
    private boolean mRowSubset[][];
    private boolean mColSubset[][];
    private boolean mBoxSubset[][];

    public Sudoku(int board[][]) {
        mBoard = board;
        mBoardSize = mBoard.length;
        mBoxSize = (int)Math.sqrt(mBoardSize);
    }

    public void initSubsets() {
        mRowSubset = new boolean[mBoardSize][mBoardSize];
        mColSubset = new boolean[mBoardSize][mBoardSize];
        mBoxSubset = new boolean[mBoardSize][mBoardSize];
        for(int i = 0; i < mBoard.length; i++) {
            for(int j = 0; j < mBoard.length; j++) {
                int value = mBoard[i][j];
                if(value != 0) {
                    setSubsetValue(i, j, value, true);
                }
            }
        }
    }

    private void setSubsetValue(int i, int j, int value, boolean present) {
        mRowSubset[i][value - 1] = present;
        mColSubset[j][value - 1] = present;
        mBoxSubset[computeBoxNo(i, j)][value - 1] = present;
    }

    public boolean solve() {
        return solve(0, 0);
    }

    public boolean solve(int i, int j) {
        if(i == mBoardSize) {
            i = 0;
            if(++j == mBoardSize) {
                return true;
            }
        }
        if(mBoard[i][j] != 0) {
            return solve(i + 1, j);
        }
        for(int value = 1; value <= mBoardSize; value++) {
            if(isValid(i, j, value)) {
                mBoard[i][j] = value;
                setSubsetValue(i, j, value, true);
                if(solve(i + 1, j)) {
                    return true;
                }
                setSubsetValue(i, j, value, false);
            }
        }

        mBoard[i][j] = 0;
        return false;
    }

    private boolean isValid(int i, int j, int val) {
        val--;
        boolean isPresent = mRowSubset[i][val] || mColSubset[j][val] || mBoxSubset[computeBoxNo(i, j)][val];
        return !isPresent;
    }

    private int computeBoxNo(int i, int j) {
        int boxRow = i / mBoxSize;
        int boxCol = j / mBoxSize;
        return boxRow * mBoxSize + boxCol;
    }

    public void print() {
        for(int i = 0; i < mBoardSize; i++) {
            if(i % mBoxSize == 0) {
                System.out.println(" -----------------------");
            }
            for(int j = 0; j < mBoardSize; j++) {
                if(j % mBoxSize == 0) {
                    System.out.print("| ");
                }
                System.out.print(mBoard[i][j] != 0 ? ((Object) (Integer.valueOf(mBoard[i][j]))) : " ");
                System.out.print(' ');
            }

            System.out.println("|");
        }

        System.out.println(" -----------------------");
    }
}
```





## JavaScript



### =ES6=



```JavaScript
//-------------------------------------------[ Dancing Links and Algorithm X ]--
/**
 * The doubly-doubly circularly linked data object.
 * Data object X
 */
class DoX {
  /**
   * @param {string} V
   * @param {!DoX=} H
   */
  constructor(V, H) {
    this.V = V;
    this.L = this;
    this.R = this;
    this.U = this;
    this.D = this;
    this.S = 1;
    this.H = H || this;
    H && (H.S += 1);
  }
}

/**
 * Helper function to help build a horizontal doubly linked list.
 * @param {!DoX} e An existing node in the list.
 * @param {!DoX} n A new node to add to the right of the existing node.
 * @return {!DoX}
 */
const addRight = (e, n) => {
  n.R = e.R;
  n.L = e;
  e.R.L = n;
  return e.R = n;
};

/**
 * Helper function to help build a vertical doubly linked list.
 * @param {!DoX} e An existing node in the list.
 * @param {!DoX} n A new node to add below the existing node.
 */
const addBelow = (e, n) => {
  n.D = e.D;
  n.U = e;
  e.D.U = n;
  return e.D = n;
};

/**
 * Verbatim copy of DK's search algorithm. The meat of the DLX algorithm.
 * @param {!DoX} h The root node.
 * @param {!Array<!DoX>} s The solution array.
 */
const search = function(h, s) {
  if (h.R == h) {
    printSol(s);
  } else {
    let c = chooseColumn(h);
    cover(c);
    for (let r = c.D; r != c; r = r.D) {
      s.push(r);
      for (let j = r.R; r !=j; j = j.R) {
        cover(j.H);
      }
      search(h, s);
      r = s.pop();
      for (let j = r.R; j != r; j = j.R) {
        uncover(j.H);
      }
    }
    uncover(c);
  }
};

/**
 * Verbatim copy of DK's algorithm for choosing the next column object.
 * @param {!DoX} h
 * @return {!DoX}
 */
const chooseColumn = h => {
  let s = Number.POSITIVE_INFINITY;
  let c = h;
  for(let j = h.R; j != h; j = j.R) {
    if (j.S < s) {
      c = j;
      s = j.S;
    }
  }
  return c;
};


/**
 * Verbatim copy of DK's cover algorithm
 * @param {!DoX} c
 */
const cover = c => {
  c.L.R = c.R;
  c.R.L = c.L;
  for (let i = c.D; i != c; i = i.D) {
    for (let j = i.R; j != i; j = j.R) {
      j.U.D = j.D;
      j.D.U = j.U;
      j.H.S = j.H.S - 1;
    }
  }
};

/**
 * Verbatim copy of DK's cover algorithm
 * @param {!DoX} c
 */
const uncover = c => {
  for (let i = c.U; i != c; i = i.U) {
    for (let j = i.L; i != j; j = j.L) {
      j.H.S = j.H.S + 1;
      j.U.D = j;
      j.D.U = j;
    }
  }
  c.L.R = c;
  c.R.L = c;
};

//-----------------------------------------------------------[ Print Helpers ]--
/**
 * Given the standard string format of a grid, print a formatted view of it.
 * @param {!string|!Array} a
 */
const printGrid = function(a) {

  const getChar = c => {
    let r = Number(c);
    if (isNaN(r)) { return c }

    let o = 48;
    if (r > 9 && r < 36) { o = 55 }
    if (r >= 36) { o = 61 }
    return String.fromCharCode(r + o)
  };

  a = 'string' == typeof a ? a.split('') : a;

  let U = Math.sqrt(a.length);
  let N = Math.sqrt(U);
  let line = new Array(N).fill('+').reduce((p, c) => {
    p.push(... Array.from(new Array(1 + N*2).fill('-')));
    p.push(c);
    return p;
  }, ['\n+']).join('') + '\n';

  a = a.reduce(function(p, c, i) {
      let d = i && !(i % U), G = i && !(i % N);
      i = !(i % (U * N));
      d && !i && (p += '|\n| ');
      d && i && (p += '|');
      i && (p = '' + p + line + '| ');
      return '' + p + (G && !d ? '| ' : '') + getChar(c) + ' ';
    }, '') + '|' + line;
  console.log(a);

};

/**
 * Given a search solution, print the resultant grid.
 * @param {!Array<!DoX>} a An array of data objects
 */
const printSol = a => {
  printGrid(a.reduce((p, c) => {
    let [i, v] = c.V.split(':');
    p[i * 1] = v;
    return p;
  }, new Array(a.length).fill('.')));
};

//----------------------------------------------[ Grid to Exact cover Matrix ]--
/**
 * Helper to get some meta about the grid.
 * @param {!string} s The standard string representation of a grid.
 * @return {!Array}
 */
const gridMeta = s => {
  const g = s.split('');
  const cellCount = g.length;
  const tokenCount = Math.sqrt(cellCount);
  const N = Math.sqrt(tokenCount);
  const g2D = g.map(e => isNaN(e * 1) ?
    new Array(tokenCount).fill(1).map((_, i) => i + 1) :
    [e * 1]);
  return [cellCount, N, tokenCount, g2D];
};

/**
 * Given a cell grid index, return the row, column and box indexes.
 * @param {!number} n The n-value of the grid. 3 for a 9x9 sudoku.
 * @return {!function(!number): !Array<!number>}
 */
const indexesN = n => i => {
    let c = Math.floor(i / (n * n));
    i %= n * n;
    return [c, i, Math.floor(c / n) * n + Math.floor(i / n)];
};

/**
 * Given a puzzle string, reduce it to an exact-cover matrix and use
 * Donald Knuth's DLX algorithm to solve it.
 * @param puzString
 */
const reduceGrid = puzString => {

  printGrid(puzString);
  const [
    numCells,   // The total number of cells in a grid (81 for a 9x9 grid)
    N,          // the 'n' value of the grid. (3 for a 9x9 grid)
    U,          // The total number of unique tokens to be placed.
    g2D         // A 2D array representation of the grid, with each element
                // being an array of candidates for a cell. Known cells are
                // single element arrays.
  ] = gridMeta(puzString);

  const getIndex = indexesN(N);

  /**
   * The DLX Header row.
   * Its length is 4 times the grid's size. This is to be able to encode
   * each of the 4 Sudoku constrains, onto each of the cells of the grid.
   * The array is initialised with unlinked DoX nodes, but in the next step
   * those nodes are all linked.
   * @type {!Array.<!DoX>}
   */
  const headRow = new Array(4 * numCells)
    .fill('')
    .map((_, i) => new DoX(`H${i}`));

  /**
   * The header row root object. This is circularly linked to be to the left
   * of the first header object in the header row array.
   * It is used as the entry point into the DLX algorithm.
   * @type {!DoX}
   */
  let H = new DoX('ROOT');
  headRow.reduce((p, c) => addRight(p, c), H);

  /**
   * Transposed the sudoku puzzle into a exact cover matrix, so it can be passed
   * to the DLX algorithm to solve.
   */
  for (let i = 0; i < numCells; i++) {
    const [ri, ci, bi] = getIndex(i);
    g2D[i].forEach(num => {
      let id = `${i}:${num}`;
      let candIdx = num - 1;

      // The 4 columns that we will populate.
      const A = headRow[i];
      const B = headRow[numCells + candIdx + (ri * U)];
      const C = headRow[(numCells * 2) + candIdx + (ci * U)];
      const D = headRow[(numCells * 3) + candIdx + (bi * U)];

      // The Row-Column Constraint
      let rcc = addBelow(A.U, new DoX(id, A));

      // The Row-Number Constraint
      let rnc = addBelow(B.U, addRight(rcc, new DoX(id, B)));

      // The Column-Number Constraint
      let cnc = addBelow(C.U, addRight(rnc, new DoX(id, C)));

      // The Block-Number Constraint
      addBelow(D.U, addRight(cnc, new DoX(id, D)));
    });
  }
  search(H, []);
};

```



```JavaScript
[
  '819..5.....2...75..371.4.6.4..59.1..7..3.8..2..3.62..7.5.7.921..64...9.....2..438',
  '53..247....2...8..1..7.39.2..8.72.49.2.98..7.79.....8.....3.5.696..1.3...5.69..1.',
  '..3.2.6..9..3.5..1..18.64....81.29..7.......8..67.82....26.95..8..2.3..9..5.1.3..',
  '394..267....3..4..5..69..2..45...9..6.......7..7...58..1..67..8..9..8....264..735',
  '97.3...6..6.75.........8.5.......67.....3.....539..2..7...25.....2.1...8.4...73..',
  '4......6.5...8.9..3....1....2.7....1.9.....4.8....3.5....2....7..6.5...8.1......6',
  '85...24..72......9..4.........1.7..23.5...9...4...........8..7..17..........36.4.',
  '..1..5.7.92.6.......8...6...9..2.4.1.........3.4.8..9...7...3.......7.69.1.8..7..',
  '.9...4..7.....79..8........4.58.....3.......2.....97.6........4..35.....2..6...8.',
  '12.3....435....1....4........54..2..6...7.........8.9...31..5.......9.7.....6...8',
  '9..2..5...4..6..3...3.....6...9..2......5..8...7..4..37.....1...5..2..4...1..6..9',
  '1....7.9..3..2...8..96..5....53..9...1..8...26....4...3......1..4......7..7...3..',
  '12.4..3..3...1..5...6...1..7...9.....4.6.3.....3..2...5...8.7....7.....5.......98',
  '..............3.85..1.2.......5.7.....4...1...9.......5......73..2.1........4...9',
  '.......39.....1..5..3.5.8....8.9...6.7...2...1..4.......9.8..5..2....6..4..7.....',
  '....839..1......3...4....7..42.3....6.......4....7..1..2........8...92.....25...6',
  '..3......4...8..36..8...1...4..6..73...9..........2..5..4.7..686........7..6..5..'
].forEach(reduceGrid);

// Or of you want to create all the grids of a particular n-size.
// I run out of stack space at n = 9
let n = 2;
let s = new Array(Math.pow(n, 4)).fill('.').join('');
reduceGrid(s);

```



```txt
+-------+-------+-------+
| . . 3 | . . . | . . . |
| 4 . . | . 8 . | . 3 6 |
| . . 8 | . . . | 1 . . |
+-------+-------+-------+
| . 4 . | . 6 . | . 7 3 |
| . . . | 9 . . | . . . |
| . . . | . . 2 | . . 5 |
+-------+-------+-------+
| . . 4 | . 7 . | . 6 8 |
| 6 . . | . . . | . . . |
| 7 . . | 6 . . | 5 . . |
+-------+-------+-------+

+-------+-------+-------+
| 1 2 3 | 4 5 6 | 7 8 9 |
| 4 5 7 | 1 8 9 | 2 3 6 |
| 9 6 8 | 3 2 7 | 1 5 4 |
+-------+-------+-------+
| 2 4 9 | 5 6 1 | 8 7 3 |
| 5 7 6 | 9 3 8 | 4 1 2 |
| 8 3 1 | 7 4 2 | 6 9 5 |
+-------+-------+-------+
| 3 1 4 | 2 7 5 | 9 6 8 |
| 6 9 5 | 8 1 4 | 3 2 7 |
| 7 8 2 | 6 9 3 | 5 4 1 |
+-------+-------+-------+

```



## Julia


```julia
function check(i, j)
    id, im = div(i, 9), mod(i, 9)
    jd, jm = div(j, 9), mod(j, 9)

    jd == id && return true
    jm == im && return true

    div(id, 3) == div(jd, 3) &&
    div(jm, 3) == div(im, 3)
end

const lookup = zeros(Bool, 81, 81)

for i in 1:81
    for j in 1:81
        lookup[i,j] = check(i-1, j-1)
    end
end

function solve_sudoku(callback::Function, grid::Array{Int64})
    (function solve()
        for i in 1:81
            if grid[i] == 0
                t = Dict{Int64, Void}()

                for j in 1:81
                    if lookup[i,j]
                        t[grid[j]] = nothing
                    end
                end

                for k in 1:9
                    if !haskey(t, k)
                        grid[i] = k
                        solve()
                    end
                end

                grid[i] = 0
                return
            end
        end

        callback(grid)
    end)()
end

function display(grid)
    for i in 1:length(grid)
        print(grid[i], " ")
        i %  3 == 0 && print(" ")
        i %  9 == 0 && print("\n")
        i % 27 == 0 && print("\n")
    end
end

grid = Int64[5, 3, 0, 0, 2, 4, 7, 0, 0,
             0, 0, 2, 0, 0, 0, 8, 0, 0,
             1, 0, 0, 7, 0, 3, 9, 0, 2,
             0, 0, 8, 0, 7, 2, 0, 4, 9,
             0, 2, 0, 9, 8, 0, 0, 7, 0,
             7, 9, 0, 0, 0, 0, 0, 8, 0,
             0, 0, 0, 0, 3, 0, 5, 0, 6,
             9, 6, 0, 0, 1, 0, 3, 0, 0,
             0, 5, 0, 6, 9, 0, 0, 1, 0]

solve_sudoku(display, grid)
```

{{out}}

```txt

5 3 9  8 2 4  7 6 1
6 7 2  1 5 9  8 3 4
1 8 4  7 6 3  9 5 2

3 1 8  5 7 2  6 4 9
4 2 5  9 8 6  1 7 3
7 9 6  3 4 1  2 8 5

8 4 1  2 3 7  5 9 6
9 6 7  4 1 5  3 2 8
2 5 3  6 9 8  4 1 7

```



## Kotlin

{{trans|C++}}

```scala
// version 1.2.10

class Sudoku(rows: List<String>) {
    private val grid = IntArray(81)
    private var solved = false

    init {
        require(rows.size == 9 && rows.all { it.length == 9 }) {
            "Grid must be 9 x 9"
        }
        for (i in 0..8) {
            for (j in 0..8 ) grid[9 * i + j] = rows[i][j] - '0'
        }
    }

    fun solve() {
        println("Starting grid:\n\n$this")
        placeNumber(0)
        println(if (solved) "Solution:\n\n$this" else "Unsolvable!")
    }

    private fun placeNumber(pos: Int) {
        if (solved) return
        if (pos == 81) {
            solved = true
            return
        }
        if (grid[pos] > 0) {
            placeNumber(pos + 1)
            return
        }
        for (n in 1..9) {
            if (checkValidity(n, pos % 9, pos / 9)) {
                grid[pos] = n
                placeNumber(pos + 1)
                if (solved) return
                grid[pos] = 0
            }
        }
    }

    private fun checkValidity(v: Int, x: Int, y: Int): Boolean {
        for (i in 0..8) {
            if (grid[y * 9 + i] == v || grid[i * 9 + x] == v) return false
        }
        val startX = (x / 3) * 3
        val startY = (y / 3) * 3
        for (i in startY until startY + 3) {
            for (j in startX until startX + 3) {
                if (grid[i * 9 + j] == v) return false
            }
        }
        return true
    }

    override fun toString(): String {
        val sb = StringBuilder()
        for (i in 0..8) {
            for (j in 0..8) {
                sb.append(grid[i * 9 + j])
                sb.append(" ")
                if (j == 2 || j == 5) sb.append("| ")
            }
            sb.append("\n")
            if (i == 2 || i == 5) sb.append("------+-------+------\n")
        }
        return sb.toString()
    }
}

fun main(args: Array<String>) {
    val rows = listOf(
        "850002400",
        "720000009",
        "004000000",
        "000107002",
        "305000900",
        "040000000",
        "000080070",
        "017000000",
        "000036040"
    )
    Sudoku(rows).solve()
}
```


{{out}}

```txt

Starting grid:

8 5 0 | 0 0 2 | 4 0 0
7 2 0 | 0 0 0 | 0 0 9
0 0 4 | 0 0 0 | 0 0 0
------+-------+------
0 0 0 | 1 0 7 | 0 0 2
3 0 5 | 0 0 0 | 9 0 0
0 4 0 | 0 0 0 | 0 0 0
------+-------+------
0 0 0 | 0 8 0 | 0 7 0
0 1 7 | 0 0 0 | 0 0 0
0 0 0 | 0 3 6 | 0 4 0

Solution:

8 5 9 | 6 1 2 | 4 3 7
7 2 3 | 8 5 4 | 1 6 9
1 6 4 | 3 7 9 | 5 2 8
------+-------+------
9 8 6 | 1 4 7 | 3 5 2
3 7 5 | 2 6 8 | 9 1 4
2 4 1 | 5 9 3 | 7 8 6
------+-------+------
4 3 2 | 9 8 1 | 6 7 5
6 1 7 | 4 2 5 | 8 9 3
5 9 8 | 7 3 6 | 2 4 1

```



## Nim

{{trans|Kotlin}}

```nim
{.this: self.}

type
  Sudoku = ref object
    grid : array[81, int]
    solved : bool

proc `$`(self: Sudoku): string =
  var sb: string = ""
  for i in 0..8:
    for j in 0..8:
      sb &= $grid[i * 9 + j]
      sb &= " "
      if j == 2 or j == 5:
        sb &= "| "
    sb &= "\n"
    if i == 2 or i == 5:
      sb &= "------+------+------\n"
  sb

proc init(self: Sudoku, rows: array[9, string]) =
  for i in 0..8:
    for j in 0..8:
      grid[9 * i + j] = rows[i][j].int - '0'.int

proc checkValidity(self: Sudoku, v, x, y: int): bool =
  for i in 0..8:
    if grid[y * 9 + i] == v or grid[i * 9 + x] == v:
      return false
  var startX = (x div 3) * 3
  var startY = (y div 3) * 3
  for i in startY..startY + 2:
    for j in startX..startX + 2:
      if grid[i * 9 + j] == v:
        return false
  result = true

proc placeNumber(self: Sudoku, pos: int) =
  if solved:
    return
  if pos == 81:
    solved = true
    return
  if grid[pos] > 0:
    placeNumber(pos + 1)
    return
  for n in 1..9:
    if checkValidity(n, pos mod 9, pos div 9):
      grid[pos] = n
      placeNumber(pos + 1)
      if solved:
        return
      grid[pos] = 0

proc solve(self: Sudoku) =
  echo "Starting grid:\n\n", $self
  placeNumber(0)
  if solved:
    echo "Solution:\n\n", $self
  else:
    echo "Unsolvable!"

var rows = ["850002400",
            "720000009",
            "004000000",
            "000107002",
            "305000900",
            "040000000",
            "000080070",
            "017000000",
            "000036040"]

var puzzle = Sudoku()
puzzle.init(rows)
puzzle.solve()
```


{{out}}

```txt
Starting grid:

8 5 0 | 0 0 2 | 4 0 0
7 2 0 | 0 0 0 | 0 0 9
0 0 4 | 0 0 0 | 0 0 0
------+-------+------
0 0 0 | 1 0 7 | 0 0 2
3 0 5 | 0 0 0 | 9 0 0
0 4 0 | 0 0 0 | 0 0 0
------+-------+------
0 0 0 | 0 8 0 | 0 7 0
0 1 7 | 0 0 0 | 0 0 0
0 0 0 | 0 3 6 | 0 4 0

Solution:

8 5 9 | 6 1 2 | 4 3 7
7 2 3 | 8 5 4 | 1 6 9
1 6 4 | 3 7 9 | 5 2 8
------+-------+------
9 8 6 | 1 4 7 | 3 5 2
3 7 5 | 2 6 8 | 9 1 4
2 4 1 | 5 9 3 | 7 8 6
------+-------+------
4 3 2 | 9 8 1 | 6 7 5
6 1 7 | 4 2 5 | 8 9 3
5 9 8 | 7 3 6 | 2 4 1
```



## Lua


```lua
--9x9 sudoku solver in lua
--based on a branch and bound solution
--fields are not tried in plain order
--but in a way to detect dead ends earlier
concat=table.concat
insert=table.insert
constraints = { }   --contains a table with 3 constraints for every field
-- a contraint "cons" is a table containing all fields which must not have the same value
-- a field "f" is an integer from 1 to 81
columns = { }       --contains all column-constraints   variable "c"
rows = { }          --contains all row-constraints      variable "r"
blocks = { }        --contains all block-constraints    variable "b"

--initialize all constraints
for f = 1, 81 do
  constraints[f] = { }
end
all_constraints = { } --union of colums, rows and blocks
for i = 1, 9 do
  columns[i] = {
    unknown = 9, --number of fields not yet solved
    unknowns = { } --fields not yet solved
  }
  insert(all_constraints, columns[i])
  rows[i] = {
    unknown = 9, -- see l.15
    unknowns = { } -- see l.16
  }
  insert(all_constraints, rows[i])
  blocks[i] = {
    unknown = 9, --see l.15
    unknowns = { } --see l.16
  }
  insert(all_constraints, blocks[i])
end
constraints_by_unknown = { } --contraints sorted by their number of unknown fields
for i = 0, 9 do
  constraints_by_unknown[i] = {
    count = 0 --how many contraints are in here
  }
end
for r = 1, 9 do
  for c = 1, 9 do
    local f = (r - 1) * 9 + c
    insert(rows[r], f)
    insert(constraints[f], rows[r])
    insert(columns[c], f)
    insert(constraints[f], columns[c])
  end
end
for i = 1, 3 do
  for j = 1, 3 do
    local r = (i - 1) * 3 + j
    for k = 1, 3 do
      for l = 1, 3 do
        local c = (k - 1) * 3 + l
        local f = (r - 1) * 9 + c
        local b = (i - 1) * 3 + k
        insert(blocks[b], f)
        insert(constraints[f], blocks[b])
      end
    end
  end
end
working = { } --save the read values in here
function read() --read the values from stdin
  local f = 1
  local l = io.read("*a")
  for d in l:gmatch("(%d)") do
    local n = tonumber(d)
    if n > 0 then
      working[f] = n
      for _,cons in pairs(constraints[f]) do
        cons.unknown = cons.unknown - 1
      end
    else
      for _,cons in pairs(constraints[f]) do
        cons.unknowns[f] = f
      end
    end
    f = f + 1
  end
  assert((f == 82), "Wrong number of digits")
end
read()
function printer(t) --helper function for printing a 1-81 table
  local pattern = {1,2,3,false,4,5,6,false,7,8,9} --place seperators for better readability
  for _,r in pairs(pattern) do
    if r then
      local function p(c)
        return c and t[(r - 1) * 9 + c] or "|"
      end
      local line={}
      for k,v in pairs(pattern) do
        line[k]=p(v)
      end
      print(concat(line))
    else
      print("---+---+---")
    end
  end
end
order = { } --when to try a field
for _,cons in pairs(all_constraints) do --put all constraints in the corresponding constraints_by_unknown set
  local level = constraints_by_unknown[cons.unknown]
  level[cons] = cons
  level.count = level.count + 1
end
function first(t) --helper function to get a value from a set
  for k, v in pairs(t) do
    if k == v then
      return k
    end
  end
end
function establish_order() -- determine the sequence in which the fields are to be tried
  local solved = constraints_by_unknown[0].count
  while solved < 27 do --there 27 constraints
  --contraints with no unknown fields are considered "solved"
  --keep in mind the actual solving happens in function branch
    local i = 1
    while constraints_by_unknown[i].count == 0 do
      i = i + 1
      -- find a unsolved contraint with the least number of unsolved fields
    end
    local cons = first(constraints_by_unknown[i])
    local f = first(cons.unknowns)
    -- take one of its unknown fields and append it to "order"
    insert(order, f)
    for _,c in pairs(constraints[f]) do
    --each constraint "c" of "f" is moved up one "level"
    --delete "f" from the constraints unknown fields
    --decrease unknown of "c"
      c.unknowns[f] = nil
      local level = constraints_by_unknown[c.unknown]
      level[c] = nil
      level.count = level.count - 1
      c.unknown = c.unknown - 1
      level = constraints_by_unknown[c.unknown]
      level[c] = c
      level.count = level.count + 1
      constraints_by_unknown[c.unknown][c] = c
    end
    solved = constraints_by_unknown[0].count
  end
end
establish_order()
max = #order --how many fields are to be solved
function bound(f,i)
  for _,c in pairs(constraints[f]) do
    for _,x in pairs(c) do
      if i == working[x] then
        return false --i is already used in fs column/row/block
      end
    end
  end
  return true
end
function branch(n)
  local f = order[n] --recursively iterate over fields in order
  if n > max then
    return working --all fields solved without collision
  else
    for i = 1, 9 do --check all values
      if bound(f, i) then --if there is no collision
        working[f] = i
        local res = branch(n + 1) --try next field
        if res then
          return res --all fields solved without collision
        else
          working[f] = nil --this lead to a dead end
        end
      else
        working[f] = nil --reset field because of a collision
      end
    end
    return false --this is a dead end
  end
end
x = branch(1)
if x then
  return printer(x)
end
```

Input:

```txt

003 000 000
400 080 036
008 000 100

040 060 073
000 900 000
000 002 005

004 070 068
600 000 000
700 600 500

```


{{out}}

```txt

123|456|789
457|189|236
968|327|154
---+---+---
249|561|873
576|938|412
831|742|695
---+---+---
314|275|968
695|814|327
782|693|541

```

Time with luajit: 9.245s


## Mathematica


```mathematica
solve[sudoku_] :=
 NestWhile[
  Join @@ Table[
     Table[ReplacePart[s, #1 -> n], {n, #2}] & @@
      First@SortBy[{#,
           Complement[Range@9, s[[First@#]], s[[;; , Last@#]],
            Catenate@
             Extract[Partition[s, {3, 3}], Quotient[#, 3, -2]]]} & /@
         Position[s, 0, {2}],
        Length@Last@# &], {s, #}] &, {sudoku}, ! FreeQ[#, 0] &]
```

Example:
<lang>solve[{{9, 7, 0, 3, 0, 0, 0, 6, 0},
  {0, 6, 0, 7, 5, 0, 0, 0, 0},
  {0, 0, 0, 0, 0, 8, 0, 5, 0},
  {0, 0, 0, 0, 0, 0, 6, 7, 0},
  {0, 0, 0, 0, 3, 0, 0, 0, 0},
  {0, 5, 3, 9, 0, 0, 2, 0, 0},
  {7, 0, 0, 0, 2, 5, 0, 0, 0},
  {0, 0, 2, 0, 1, 0, 0, 0, 8},
  {0, 4, 0, 0, 0, 7, 3, 0, 0}}]
```

{{out}}

```txt
{{{9, 7, 5, 3, 4, 2, 8, 6, 1}, {8, 6, 1, 7, 5, 9, 4, 3, 2}, {3, 2, 4,
   1, 6, 8, 9, 5, 7}, {2, 1, 9, 5, 8, 4, 6, 7, 3}, {4, 8, 7, 2, 3, 6,
   5, 1, 9}, {6, 5, 3, 9, 7, 1, 2, 8, 4}, {7, 3, 8, 4, 2, 5, 1, 9,
   6}, {5, 9, 2, 6, 1, 3, 7, 4, 8}, {1, 4, 6, 8, 9, 7, 3, 2, 5}}}
```



## MATLAB

This solution impliments a recursive, depth-first search of the possible values unfilled sudoku cells can take. The search tree is pruned using logical deduction rules and takes about a minute to solve some of the more difficult puzzles. This code can be cleaned by making the main code blocks, denoted by "%% [Block Title]," into their own separate functions. This can also be further improved by implementing a Sudoku class and making this solver a member function. There are also several lines of code that can be vectorized to improve efficiency, but at the expense of readability.

For this to work, this code must be placed in a file named "sudokuSolver.m"

```MATLAB
function solution = sudokuSolver(sudokuGrid)

    %Define what each of the sub-boxes of the sudoku grid are by defining
    %the start and end coordinates of each sub-box. The indecies represent
    %the column and row of a grid coordinate on the actual sudoku grid.
    %The contents of each cell with the same grid coordinates contain the
    %information to determine which sub-box that grid coordinate is
    %contained in on the sudoku grid. The array in position 1, i.e.
    %subBoxes{row,column}(1), represents the row indecies of the subbox.
    %The array in position 2, i.e. subBoxes{row,column}(2),represents the
    %column indecies of the subbox.

    subBoxes(1:9,1:9) = {{(1:3),(1:3)}};
    subBoxes(4:6,:)= {{(4:6),(1:3)}};
    subBoxes(7:9,:)= {{(7:9),(1:3)}};

    for column = (4:6)
        for row = (1:9)
            subBoxes{row,column}(2)= {4:6};
        end
    end
    for column = (7:9)
        for row = (1:9)
            subBoxes{row,column}(2)= {7:9};
        end
    end

    %Generate a cell of arrays which contain the possible values of the
    %sudoku grid for each cell in the grid. The possible values a specific
    %grid coordinate can take share the same indices as the sudoku grid
    %coordinate they represent.
    %For example sudokuGrid(m,n) can be possibly filled in by the
    %values stored in the array at possibleValues(m,n).
    possibleValues(1:9,1:9) = { (1:9) };

    %Filter the possibleValues so that no entry exists for coordinates that
    %have already been filled in. This will replace any array with an empty
    %array in the possibleValues cell matrix at the coordinates of a grid
    %already filled in the sudoku grid.
    possibleValues( ~isnan(sudokuGrid) )={[]};

    %Iterate through each grid coordinate and filter out the possible
    %values for that grid point that aren't alowed by the rules given the
    %current values that are filled in. Or, if there is only one possible
    %value for the current coordinate, fill it in.

    solution = sudokuGrid; %so the original sudoku input isn't modified
    memory = 0; %contains the previous iterations possibleValues
    dontStop = true; %stops the while loop when nothing else can be reasoned about the sudoku

    while( dontStop )

%% Process of elimination deduction method

        while( ~isequal(possibleValues,memory) ) %Stops using the process of elimination deduction method when this deduction rule stops working

            memory = possibleValues; %Copies the current possibleValues into memory, for the above conditional on the next iteration.

            %Iterate through everything
            for row = (1:9)
                for column = (1:9)

                    if isnan( solution(row,column) ) %If grid coordinate hasn't been filled in, try to determine it's value.

                        %Look at column to see what values have already
                        %been filled in and thus the current grid
                        %coordinate can't be
                        removableValues = solution( ~isnan(solution(:,column)),column );

                        %If there are any values that have been assigned to
                        %other cells in the same column, filter those out
                        %of the current cell's possiblValues
                        if ~isempty(removableValues)
                            for m = ( 1:numel(removableValues) )
                                possibleValues{row,column}( possibleValues{row,column}==removableValues(m) )=[];
                            end
                        end

                        %If the current grid coordinate can only atain one
                        %possible value, assign it that value
                        if numel( possibleValues{row,column} ) == 1
                            solution(row,column) = possibleValues{row,column};
                            possibleValues(row,column)={[]};
                        end
                    end  %end if

                    if isnan( solution(row,column) ) %If grid coordinate hasn't been filled in, try to determine it's value.

                        %Look at row to see what values have already
                        %been filled in and thus the current grid
                        %coordinate can't be
                        removableValues = solution( row,~isnan(solution(row,:)) );

                        %If there are any values that have been assigned to
                        %other cells in the same row, filter those out
                        %of the current cell's possiblValues
                        if ~isempty(removableValues)
                            for m = ( 1:numel(removableValues) )
                                possibleValues{row,column}( possibleValues{row,column}==removableValues(m) )=[];
                            end
                        end

                        %If the current grid coordinate can only atain one
                        %possible value, assign it that value
                        if numel( possibleValues{row,column} ) == 1
                            solution(row,column) = possibleValues{row,column};
                            possibleValues(row,column)={[]};
                        end
                    end %end if

                    if isnan( solution(row,column) ) %If grid coordinate hasn't been filled in, try to determine it's value.

                        %Look at sub-box to see if any possible values can be
                        %filtered out. First pull the boundaries of the sub-box
                        %containing the current array coordinate
                        currentBoxBoundaries=subBoxes{row,column};

                        %Then pull the sub-boxes values out of the solution
                        box = solution(currentBoxBoundaries{:});

                        %Look at sub-box to see what values have already
                        %been filled in and thus the current grid
                        %coordinate can't be
                        removableValues = box( ~isnan(box) );

                        %If there are any values that have been assigned to
                        %other cells in the same sub-box, filter those out
                        %of the current cell's possiblValues
                        if ~isempty(removableValues)
                            for m = ( 1:numel(removableValues) )
                                possibleValues{row,column}( possibleValues{row,column}==removableValues(m) )=[];
                            end
                        end

                        %If the current grid coordinate can only atain one
                        %possible value, assign it that value
                        if numel( possibleValues{row,column} ) == 1
                            solution(row,column) = possibleValues{row,column};
                            possibleValues(row,column)={[]};
                        end
                    end %end if

                end %end for column
            end %end for row
        end %stop process of elimination

%% Check that there are no contradictions in the solved grid coordinates.

        %Check that each row at most contains one of each of the integers
        %from 1 to 9
        if ~isempty( find( histc( solution,(1:9),1 )>1 ) )
            solution = false;
            return
        end

        %Check that each column at most contains one of each of the integers
        %from 1 to 9
        if ~isempty( find( histc( solution,(1:9),2 )>1 ) )
            solution = false;
            return
        end

        %Check that each sub-box at most contains one of each of the integers
        %from 1 to 9
        subBoxBins = zeros(9,9);
        counter = 0;
        for row = [2 5 8]
            for column = [2 5 8]
                counter = counter +1;

                %because the sub-boxes are extracted as square matricies,
                %we need to reshape them into row vectors so all of the
                %boxes can be input into histc simultaneously
                subBoxBins(counter,:) = reshape( solution(subBoxes{row,column}{:}),1,9 );
            end
        end
        if ~isempty( find( histc( subBoxBins,(1:9),2 )>1 ) )
            solution = false;
            return
        end

        %Check to make sure there are no grid coordinates that are not
        %filled in and have no possible values.

        [rowStack,columnStack] = find(isnan(solution)); %extracts the indicies of the unsolved grid coordinates
        if (numel(rowStack) > 0)

            for counter = (1:numel(rowStack))
                if isempty(possibleValues{rowStack(counter),columnStack(counter)})
                    solution = false;
                    return
                end
            end

        %if there are no more grid coordinates to be filed in then the
        %sudoku is solved and we can return the solution without further
        %computation
        elseif (numel(rowStack) == 0)
            return
        end

%% Use the unique relative compliment of sets deduction method

        %Because no more information can be determined by the process of
        %ellimination we have to try a new method of reasoning. Now we will
        %look at the possible values a cell can take. If there is a value that
        %that grid coordinate can take but no other coordinates in the same row,
        %column or sub-box can take that value then we assign that coordinate
        %that value.

        keepGoing = true; %signals to keep applying rules to the current grid-coordinate because it hasn't been solved using previous rules
        dontStop = false; %if this method doesn't figure anything out, this will terminate the top level while loop

        [rowStack,columnStack] = find(isnan(solution)); %This will also take care of the case where the sudoku is solved
        counter = 0; %makes sure the loop terminates when there are no more cells to consider

        while( keepGoing && (counter < numel(rowStack)) ) %stop this method of reasoning when the value of one of the cells has been determined and return to the process of elimination method

            counter = counter + 1;

            row = rowStack(counter);
            column = columnStack(counter);

            gridPossibles = [possibleValues{row,column}];

            coords = (1:9);
            coords(column) = [];
            rowPossibles = [possibleValues{row,coords}]; %extract possible values for everything in the same row except the current grid coordinate

            totalMatches = zeros( numel(gridPossibles),1 ); %preallocate for speed

            %count how many times a possible value for the current cell
            %appears as a possible value for the cells in the same row
            for n = ( 1:numel(gridPossibles) )
                totalMatches(n) = sum( (rowPossibles == gridPossibles(n)) );
            end

            %remove any possible values for the current cell that have
            %matches in other cells
            gridPossibles = gridPossibles(totalMatches==0);

            %if there is only one possible value that the current cell can
            %take that aren't shared by other cells, assign that value to
            %the current cell.
            if numel(gridPossibles) == 1

                solution(row,column) = gridPossibles;
                possibleValues(row,column)={[]};
                keepGoing = false; %stop this method of deduction and return to the process of elimination
                dontStop = true; %keep the top level loop going

            end

            if(keepGoing) %do the same as above but for the current cell's column

                gridPossibles = [possibleValues{row,column}];

                coords = (1:9);
                coords(row) = [];
                columnPossibles = [possibleValues{coords,column}];

                totalMatches = zeros( numel(gridPossibles),1 );
                for n = ( 1:numel(gridPossibles) )
                    totalMatches(n) = sum( (columnPossibles == gridPossibles(n)) );
                end

                gridPossibles = gridPossibles(totalMatches==0);

                if numel(gridPossibles) == 1

                    solution(row,column) = gridPossibles;
                    possibleValues(row,column)={[]};
                    keepGoing = false;
                    dontStop = true;

                end
            end

            if(keepGoing) %do the same as above but for the current cell's sub-box

                gridPossibles = [possibleValues{row,column}];

                currentBoxBoundaries = subBoxes{row,column};
                subBoxPossibles = [];
                for m = currentBoxBoundaries{1}
                    for n = currentBoxBoundaries{2}
                        if ~((m == row) && (n == column))
                            subBoxPossibles = [subBoxPossibles possibleValues{m,n}];
                        end
                    end
                end

                totalMatches = zeros( numel(gridPossibles),1 );
                for n = ( 1:numel(gridPossibles) )
                    totalMatches(n) = sum( (subBoxPossibles == gridPossibles(n)) );
                end

                gridPossibles = gridPossibles(totalMatches==0);

                if numel(gridPossibles) == 1

                    solution(row,column) = gridPossibles;
                    possibleValues(row,column)={[]};
                    keepGoing = false;
                    dontStop = true;

                end
            end %end

        end %end  set comliment rule while loop
    end %end top-level while loop

%% Depth-first search of the solution tree

    %There is no more reasoning that can solve the puzzle so now it is time
    %for a depth-first search of the possible answers, basically
    %guess-and-check. This is implimented recursively.

    [rowStack,columnStack] = find(isnan(solution)); %Get all of the unsolved cells

    if (numel(rowStack) > 0) %If all of the above stuff terminates then there will be at least one grid coordinate not filled in

        %Treat the rowStack and columnStack like stacks, and pop the top
        %value off the stack to act as the current node whose
        %possibleValues to search through, then assign the possible values
        %of that grid coordinate to a variable that holds that values to
        %search through
        searchTreeNodes = possibleValues{rowStack(1),columnStack(1)};

        keepSearching = true; %used to continue the search
        counter = 0; %counts the amount of possible values searched for the current node
        tempSolution = solution; %used so that the solution is not overriden until a solution hase been found

        while( keepSearching && (counter < numel(searchTreeNodes)) ) %stop recursing if we run out of possible values for the current node

            counter = counter + 1;
            tempSolution(rowStack(1),columnStack(1)) = searchTreeNodes(counter); %assign a possible value to the current node in the tree
            tempSolution = sudokuSolver(tempSolution); %recursively call the solver with the current guess value for the current grid coordinate

            if ~islogical(tempSolution) %if tempSolution is not a boolean but a valid sudoku stop recursing and set solution to tempSolution
               keepSearching = false;
               solution = tempSolution;
            elseif counter == numel(searchTreeNodes) %if we have run out of guesses for the current node, stop recursing and return a value of "false" for the solution
               solution = false;
            else %reset tempSolution to the current state of the board and try the next guess for the possible value of the current cell
               tempSolution = solution;
            end

        end %end recursion
    end  %end if

%% End of program
end %end sudokuSolver
```

[http://www.menneske.no/sudoku/eng/showpuzzle.html?number=6903541 Test Input]:
All empty cells must have a value of NaN.

```MATLAB
sudoku = [NaN   NaN   NaN   NaN     8     3     9   NaN   NaN
     1   NaN   NaN   NaN   NaN   NaN   NaN     3   NaN
   NaN   NaN     4   NaN   NaN   NaN   NaN     7   NaN
   NaN     4     2   NaN     3   NaN   NaN   NaN   NaN
     6   NaN   NaN   NaN   NaN   NaN   NaN   NaN     4
   NaN   NaN   NaN   NaN     7   NaN   NaN     1   NaN
   NaN     2   NaN   NaN   NaN   NaN   NaN   NaN   NaN
   NaN     8   NaN   NaN   NaN     9     2   NaN   NaN
   NaN   NaN   NaN     2     5   NaN   NaN   NaN     6]
```

[http://www.menneske.no/sudoku/eng/solution.html?number=6903541 Output]:

```MATLAB
solution =

     7     6     5     4     8     3     9     2     1
     1     9     8     7     2     6     4     3     5
     2     3     4     9     1     5     6     7     8
     8     4     2     5     3     1     7     6     9
     6     1     7     8     9     2     3     5     4
     3     5     9     6     7     4     8     1     2
     9     2     6     1     4     7     5     8     3
     5     8     1     3     6     9     2     4     7
     4     7     3     2     5     8     1     9     6
```



## OCaml

uses the library [http://ocamlgraph.lri.fr/index.en.html ocamlgraph]

```ocaml
(* Ocamlgraph demo program: solving the Sudoku puzzle using graph coloring
   Copyright 2004-2007 Sylvain Conchon, Jean-Christophe Filliatre, Julien Signoles

   This software is free software; you can redistribute it and/or modify
   it under the terms of the GNU Library General Public License version 2,
   with the special exception on linking described in file LICENSE.

   This software is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. *)

open Format
open Graph

(* We use undirected graphs with nodes containing a pair of integers
   (the cell coordinates in 0..8 x 0..8).
   The integer marks of the nodes will store the colors. *)
module G = Imperative.Graph.Abstract(struct type t = int * int end)

(* The Sudoku grid = a graph with 9x9 nodes *)
let g = G.create ()

(* We create the 9x9 nodes, add them to the graph and keep them in a matrix
   for later access *)
let nodes =
  let new_node i j = let v = G.V.create (i, j) in G.add_vertex g v; v in
  Array.init 9 (fun i -> Array.init 9 (new_node i))

let node i j = nodes.(i).(j) (* shortcut for easier access *)

(* We add the edges:
   two nodes are connected whenever they can't have the same value,
   i.e. they belong to the same line, the same column or the same 3x3 group *)
let () =
  for i = 0 to 8 do for j = 0 to 8 do
    for k = 0 to 8 do
      if k <> i then G.add_edge g (node i j) (node k j);
      if k <> j then G.add_edge g (node i j) (node i k);
    done;
    let gi = 3 * (i / 3) and gj = 3 * (j / 3) in
    for di = 0 to 2 do for dj = 0 to 2 do
      let i' = gi + di and j' = gj + dj in
      if i' <> i || j' <> j then G.add_edge g (node i j) (node i' j')
    done done
  done done

(* Displaying the current state of the graph *)
let display () =
  for i = 0 to 8 do
    for j = 0 to 8 do printf "%d" (G.Mark.get (node i j)) done;
    printf "\n";
  done;
  printf "@?"

(* We read the initial constraints from standard input and we display g *)
let () =
  for i = 0 to 8 do
    let s = read_line () in
    for j = 0 to 8 do match s.[j] with
      | '1'..'9' as ch -> G.Mark.set (node i j) (Char.code ch - Char.code '0')
      | _ -> ()
    done
  done;
  display ();
  printf "---------@."

(* We solve the Sudoku by 9-coloring the graph g and we display the solution *)
module C = Coloring.Mark(G)

let () = C.coloring g 9; display ()
```



## Oz

Using built-in constraint propagation and search.

```oz
declare
  %% a puzzle is a function that returns an initial board configuration
  fun {Puzzle1}
     %% a board is a list of 9 rows
     [[4 _ _  _ _ _  _ 6 _]
      [5 _ _  _ 8 _  9 _ _]
      [3 _ _  _ _ 1  _ _ _]

      [_ 2 _  7 _ _  _ _ 1]
      [_ 9 _  _ _ _  _ 4 _]
      [8 _ _  _ _ 3  _ 5 _]

      [_ _ _  2 _ _  _ _ 7]
      [_ _ 6  _ 5 _  _ _ 8]
      [_ 1 _  _ _ _  _ _ 6]]
  end

  %% Returns a list of solutions for the given puzzle.
  fun {Solve Puzzle}
     {SearchAll {GetScript Puzzle}}
  end

  %% Creates a solver script for a puzzle.
  fun {GetScript Puzzle}
     proc {$ Board}
        %% Every row is a list of nine finite domain vars
        %% with the domain 1..9.
        Board = {MapRange fun {$ _} {FD.list 9 1#9} end}
        %% Post initial configuration.
        Board = {Puzzle}

        %% The core constraints:
        {ForAll {Rows Board} FD.distinct}
        {ForAll {Columns Board} FD.distinct}
        {ForAll {Boxes Board} FD.distinct}

        %% Search if necessary.
        {FD.distribute ff {Flatten Board}}
     end
  end

  %% Returns the board as a list of rows.
  fun {Rows Board}
     Board %% This is already the representation we have chosen.
  end

  %% Returns the board as a list of columns.
  fun {Columns Board}
     {MapRange fun {$ I} {Column Board I} end}
  end

  %% Returns the board as a list of boxes (sub-grids).
  fun {Boxes Board}
     {MapRange fun {$ I} {Box Board I} end}
  end

  %% Helper function: map the range 1..9 to something.
  fun {MapRange F}
     {Map [1 2 3 4 5 6 7 8 9] F}
  end

  %% Returns a column of the board as a list of fields.
  fun {Column Board Index}
     {Map Board
      fun {$ Row}
         {Nth Row Index}
      end
     }
  end

  %% Returns a box of the board as a list of fields.
  fun {Box Board Index}
     Index0 = Index-1
     Fields = {Flatten Board}
     Start = (Index0 div 3) * 27 + (Index0 mod 3)*3
  in
     {Flatten
      for I in 0..2 collect:C do
         {C {List.take {List.drop Fields Start+I*9} 3}}
      end
     }
  end
in
  {Inspect {Solve Puzzle1}.1}
```




## PARI/GP


Build plugin for PARI's function interface from C code: sudoku.c

```C>#include <pari/pari.h


typedef int SUDOKU [9][9];

static inline int check_num(SUDOKU s, int row, int col, int num)
{
  int i, r = (row/3)*3, c = (col/3)*3;

  for (i = 0; i < 9; i++)
    if (s[row][i] == num || s[i][col] == num || s[i%3 + r][i/3 + c] == num)
      return 0;

  return 1;
}

static int sudoku_solve(SUDOKU s, int row, int col)
{
  int num;

  if (row < 9 && col < 9) {
    if (s[row][col]) {
      if (col < 8)
	return sudoku_solve(s, row, col+1);
      if (row < 8)
	return sudoku_solve(s, row+1, 0);
      return 1;
    }
    else
      for (num = 1; num < 10; num++)
	if (check_num(s, row, col, num)) {
	  s[row][col] = num;
	  if (sudoku_solve(s, row, col))
	    return 1;
	  else
	    s[row][col] = 0;
	}
    return 0;
  }
  return 1;
}

GEN plug_sudoku(GEN M)
{
  SUDOKU s;
  GEN S;
  int i, k;

  if (typ(M) != t_MAT)
    pari_err(e_MISC, "parameter not matrix");

  S = matsize(M);

  if (itos(gel(S, 1)) < 9 || itos(gel(S, 2)) < 9)
    pari_err(e_MISC, "parameter not 9x9 matrix");

  for (i = 0; i < 9; i++)
    for (k = 0; k < 9; k++)
      s[i][k] = itos(gcoeff(M, i+1, k+1));	/* get sudoku */

  if (sudoku_solve(s, 0, 0)) {			/* solve sudoku */
    S = cgetg(10, t_MAT);
    for (k = 0; k < 9; k++) {			/* create 9x9 matrix */
      gel(S, k+1) = cgetg(10, t_COL);
      for (i = 0; i < 9; i++)
	gcoeff(S, i+1, k+1) = stoi(s[i][k]);	/* fill in elements */
    }
    return S;
  }
  return gen_0;		/* no solution */
}

```

Compile plugin: gcc -O2 -Wall -fPIC -shared sudoku.c -o libsudoku.so -lpari

Install plugin from home directory and play:

```parigp
install("plug_sudoku", "G", "sudoku", "~/libsudoku.so")
```


Output:
```txt
 gp > S=[5,3,0,0,7,0,0,0,0;6,0,0,1,9,5,0,0,0;0,9,8,0,0,0,0,6,0;8,0,0,0,6,0,0,0,3;4,0,0,8,0,3,0,0,1;7,0,0,0,2,0,0,0,6;0,6,0,0,0,0,2,8,0;0,0,0,4,1,9,0,0,5;0,0,0,0,8,0,0,7,9]
[5 3 0 0 7 0 0 0 0]
[6 0 0 1 9 5 0 0 0]
[0 9 8 0 0 0 0 6 0]
[8 0 0 0 6 0 0 0 3]
[4 0 0 8 0 3 0 0 1]
[7 0 0 0 2 0 0 0 6]
[0 6 0 0 0 0 2 8 0]
[0 0 0 4 1 9 0 0 5]
[0 0 0 0 8 0 0 7 9]

gp > sudoku(S)
[5 3 4 6 7 8 9 1 2]
[6 7 2 1 9 5 3 4 8]
[1 9 8 3 4 2 5 6 7]
[8 5 9 7 6 1 4 2 3]
[4 2 6 8 5 3 7 9 1]
[7 1 3 9 2 4 8 5 6]
[9 6 1 5 3 7 2 8 4]
[2 8 7 4 1 9 6 3 5]
[3 4 5 2 8 6 1 7 9]

```



## Pascal

{{works with|Free Pascal}}
Simple backtracking implimentation, therefor it must be fast to be competetive.With doReverse = true same sequence for trycell. nearly 5 times faster than [[Sudoku#C|C]]-Version.

```pascal
Program soduko;
{$IFDEF FPC}
  {$CODEALIGN proc=16,loop=8}
{$ENDIF}
uses
  sysutils,crt;
const
  carreeSize = 3;
  maxCoor = carreeSize*carreeSize;
  maxValue = maxCoor;
  maxMask = 1 shl (maxCoor+1)-1;
type
  tLimit = 0..maxCoor-1;
  tValue = 0..maxCoor;
  tSteps = 0..maxCoor*maxCoor;
  tValField = array[tLimit,tLimit] of NativeInt;//tValue;
  tBitrepr = 0..maxMask;
  tcol = array[tLimit] of NativeInt;// tBitrepr;
  trow = array[tLimit] of NativeInt;// tBitrepr;
  tcar = array[tLimit] of NativeInt;// tBitrepr;
  tpValue = ^NativeInt;//^tValue;
  tpLimit = ^tLimit;
  tpBitrepr=  ^NativeInt;//^tBitrepr;
  tchgVal = record
              cvCol,
              cvRow,
              cvCar : tpBitrepr;
              cvVal : tpValue;
            end;
  tpChgVal = ^tchgVal;
  tchgList = array[tSteps] of tchgVal;

  tField = record
             fdChgList: tchgList;
             fdCol : tcol;
             fdRow : trow;
             fdcar : tcar;
             fdVal : tValField;
             fdChgIdx : tSteps;

           end;
const
  Expl0:tValField = ((9,0,7,0,0,0,3,0,0),
                     (0,0,0,1,0,0,2,0,0),
                     (6,0,0,0,0,8,0,0,0),
                     (0,0,5,0,3,0,0,0,0),
                     (0,0,0,0,0,0,0,8,4),
                     (0,0,0,0,0,0,0,6,0),
                     (0,0,0,2,7,0,0,0,0),
                     (8,4,0,0,0,0,0,0,0),
                     (0,6,0,0,0,0,0,0,0));
  Expl1:tValField=((0,0,0,1,0,0,0,3,8),
                   (2,0,0,0,0,5,0,0,0),
                   (0,0,0,0,0,0,0,0,0),
                   (0,5,0,0,0,0,4,0,0),
                   (4,0,0,0,3,0,0,0,0),
                   (0,0,0,7,0,0,0,0,6),
                   (0,0,1,0,0,0,0,5,0),
                   (0,0,0,0,6,0,2,0,0),
                   (0,6,0,0,0,4,0,0,0));

var
  F,
  solF : TField;
  solCnt,
  callCnt: NativeUint;
  solFound : Boolean;

procedure OutField(const F:tField);
var
  rw,cl : tLimit;
  rowS: AnsiString;
Begin
  GotoXy(1,1);
  For rw := low(tLimit) to High(tLimit) do
  Begin
    rowS := '  ';
    For cl := low(tLimit) to High(tLimit) do
      RowS :=RowS+IntToStr(F.fdVal[rw,cl]);
    writeln(RowS);
  end;
end;

function CarIdx(rw,cl: NativeInt):NativeInt;
begin
  CarIdx:= (rw DIV carreeSize)*carreeSize +cl DIV carreeSize;
end;
function InsertTest(const F:tField;rw,cl:tLimit;value:tValue):boolean;
var
  msk: tBitrepr;
Begin
  result := (Value = 0);
  IF result then
    EXIT;
  msk := 1 shl (value-1);
  with F do
  Begin
    result := fdRow[rw] AND msk = 0;
    result := result AND (fdCol[cl] AND msk = 0);
    rw :=CarIdx(rw,cl);
    result := result AND (fdCar[rw] AND msk = 0);
  end;
end;

function InitField(var F:tField;const InFd:tValField;DoReverse:boolean):boolean;
var
  TmpchgVal:tchgVal;
  rw,cl,
  value,
  msk    : NativeInt;
  leftSteps:tSteps;
Begin
  Fillchar(F,SizeOf(F),#0);
  leftSteps := High(tSteps)-1;
  //unknown fields inserted from end
  For rw := low(tLimit) to High(tLimit) do
    For cl := low(tLimit) to High(tLimit) do
    Begin
      value := InFd[rw,cl];
      IF InsertTest(F,rw,cl,value) then
      Begin
        with F do
        Begin
          if value > 0 then
          Begin
            msk := 1 shl (value-1);
            //given state
            //use pointer to the relevant places and mark as occupied
            with fdChgList[fdChgIdx] do
            begin
               cvCol := @fdCol[cl];
               cvCol^ +=Msk;
               cvRow := @fdRow[rw];
               cvRow^ +=Msk;
               cvCar := @fdCar[CarIdx(rw,cl)];
               cvCar^ +=Msk;
               cvVal := @fdVal[rw,cl];
               cvVal^ := value;
            end;
            inc(fdChgIdx);
          end
          else
          Begin
            //use pointer to the relevant places
            with fdChgList[leftSteps] do
            begin
               cvCol := @fdCol[cl];
               cvRow := @fdRow[rw];
               cvCar := @fdCar[CarIdx(rw,cl)];
               cvVal := @fdVal[rw,cl];
            end;
            dec(leftSteps);
          end;
        end
      end
      else
      Begin
        writeln(rw:10,cl:10,value:10);
        Writeln(' not solvable SuDoKu ');
        delay(2000);
        result := false;
        EXIT;
      end;
    end;
  //reverse direction of left over
  IF DoReverse then
  Begin
    leftSteps := High(tSteps)-1;
    rw := F.fdChgIdx;
    repeat
      TmpchgVal:= F.fdChgList[leftSteps];
      F.fdChgList[leftSteps]:= F.fdChgList[rw];
      F.fdChgList[rw] :=TmpchgVal;
      dec(leftSteps);
      inc(rw);
    until rw>=leftSteps;
  end;
  //OutField(F);
  solFound := false;
  result := true;
end;
procedure SolIsFound;
begin
  solF := F;
  inc(solCnt);
  solFound := True;
end;

procedure TryCell(var ChgVal:tpchgVal);
var
  value :NativeInt;
  poss,msk: NativeInt;
Begin
  IF solFound then EXIT;
  with ChgVal^ do
    poss:= (cvRow^ OR cvCol^ OR cvCar^) XOR maxMask;
  IF Poss = 0 then
    EXIT;

  value := 1;
  msk   := 1;

  repeat
    IF Poss AND MSK <>0 then
    Begin
      inc(callCnt);
      //insert test value
      with ChgVal^ do
      Begin
        cvCol^ := cvCol^ OR msk;
        cvRow^ := cvRow^ OR msk;
        cvCar^ := cvCar^ OR msk;
        cvVAl^ := value;
      end;
      //try next in list, if beyond last
      inc(ChgVal);

      IF ChgVal^.cvCol <> NIL then
        TryCell(ChgVal)
      else
        SolIsFound;
      //remove test value
      dec(ChgVal);
      with ChgVal^  do
      Begin
        cvCol^ := cvCol^ XOR msk;
        cvRow^ := cvRow^ XOR msk;
        cvCar^ := cvCar^ XOR msk;
        cvVAl^ := 0;
      end;
    end;
    inc(msk,msk);
    inc(value);
  until value> maxValue;
end;

var
  ChangeBegin : tpChgVal;
  k : NativeInt;
  T1,T0: TDateTime;
begin
  randomize;
  ClrScr;
  solCnt := 0;
  callCnt:= 0;
  T0 := time;
  k := 0;
  repeat
    InitField(F,Expl1,FALSE);
    ChangeBegin := @F.fdChgList[F.fdChgIdx];
    TryCell(ChangeBegin);
    inc(k);
  until k >= 5;
  T1 := time;
  Outfield(solF);
  writeln(86400*1000*(T1-T0)/k:10:3,' ms Test calls :',callCnt/k:8:0);
end.
```

{{out}}

```txt


Expl0
  927465318
  458193276
  613728459
  185634792
  376912584
  294587163
  539276841
  841359627
  762841935
// InitField  doReverse = true
    9.850 ms Test calls :  532466
// InitField  doReverse = false
  2609.000 ms Test calls :135196346
...
Expl1
  594126738
  237895164
  618473925
  859612473
  476539812
  123748596
  341287659
  985361247
  762954381
// InitField  doReverse = true
   857.600 ms Test calls :40980572
// InitField  doReverse = false
    21.400 ms Test calls : 1089986

```



## Perl


```Perl
#!/usr/bin/perl
use integer;
use strict;

my @A = qw(
    5 3 0  0 2 4  7 0 0
    0 0 2  0 0 0  8 0 0
    1 0 0  7 0 3  9 0 2

    0 0 8  0 7 2  0 4 9
    0 2 0  9 8 0  0 7 0
    7 9 0  0 0 0  0 8 0

    0 0 0  0 3 0  5 0 6
    9 6 0  0 1 0  3 0 0
    0 5 0  6 9 0  0 1 0
);

sub solve {
    my $i;
    foreach $i ( 0 .. 80 ) {
	next if $A[$i];
	my %t = map {
		$_ / 9 == $i / 9 ||
		$_ % 9 == $i % 9 ||
		$_ / 27 == $i / 27 && $_ % 9 / 3 == $i % 9 / 3
		? $A[$_] : 0,
		1;
	    } 0 .. 80;
	solve( $A[$i] = $_ ) for grep !$t{$_}, 1 .. 9;
	return $A[$i] = 0;
    }
    $i = 0;
    foreach (@A) {
	print "-----+-----+-----\n" if !($i%27) && $i;
	print !($i%9) ? '': $i%3 ? ' ' : '|', $_;
	print "\n" unless ++$i%9;
    }
}
solve();
```

{{out}}

```txt

5 3 9|8 2 4|7 6 1
6 7 2|1 5 9|8 3 4
1 8 4|7 6 3|9 5 2
-----+-----+-----
3 1 8|5 7 2|6 4 9
4 2 5|9 8 6|1 7 3
7 9 6|3 4 1|2 8 5
-----+-----+-----
8 4 1|2 3 7|5 9 6
9 6 7|4 1 5|3 2 8
2 5 3|6 9 8|4 1 7

```



## Perl 6

{{trans|Perl}}


```perl6
use v6;
my @A = <
    5 3 0  0 2 4  7 0 0
    0 0 2  0 0 0  8 0 0
    1 0 0  7 0 3  9 0 2

    0 0 8  0 7 2  0 4 9
    0 2 0  9 8 0  0 7 0
    7 9 0  0 0 0  0 8 0

    0 0 0  0 3 0  5 0 6
    9 6 0  0 1 0  3 0 0
    0 5 0  6 9 0  0 1 0
>;

my &I = * div 9;  # line number
my &J = * % 9;    # column number
my &K = { ($_ div 27) * 3 + $_ % 9 div 3 }; # bloc number

sub solve {
    for ^@A -> $i {
	next if @A[$i];
	my @taken-values = @A[
	    grep {
		I($_) == I($i) || J($_) == J($i) || K($_) == K($i)
	    }, ^@A
	];
	for grep none(@taken-values), 1..9 {
	    @A[$i] = $_;
	    solve;
	}
	return @A[$i] = 0;
    }
    my $i = 1;
    for ^@A {
	print "@A[$_] ";
	print " "  if $i %% 3;
	print "\n" if $i %% 9;
	print "\n" if $i++ %% 27;
    }
}
solve;
```


{{out}}

```txt
5 3 9  8 2 4  7 6 1
6 7 2  1 5 9  8 3 4
1 8 4  7 6 3  9 5 2

3 1 8  5 7 2  6 4 9
4 2 5  9 8 6  1 7 3
7 9 6  3 4 1  2 8 5

8 4 1  2 3 7  5 9 6
9 6 7  4 1 5  3 2 8
2 5 3  6 9 8  4 1 7
```


This is an alternative solution that uses a more ellaborate set of choices instead of brute-forcing it.


```perl6
#!/usr/bin/env perl6
use v6;
#
# In this code, a sudoku puzzle is represented as a two-dimentional
# array. The cells that are not yet solved are represented by yet
# another array of all the possible values.
#
# This implementation is not a simple brute force evaluation of all
# the options, but rather makes four extra attempts to guide the
# solution:
#
#  1) For every change in the grid, usually made by an attempt at a
#     solution, we will reduce the search space of the possible values
#     in all the other cells before going forward.
#
#  2) When a cell that is not yet resolved is the only one that can
#     hold a specific value, resolve it immediately instead of
#     performing the regular search.
#
#  3) Instead of trying from cell 1,1 and moving in sequence, this
#     implementation will start trying on the cell that is the closest
#     to being solved already.
#
#  4) Instead of trying all possible values in sequence, start with
#     the value that is the most unique. I.e.: If the options for this
#     cell are 1,4,6 and 6 is only a candidate for two of the
#     competing cells, we start with that one.
#

# keep a list with all the cells, handy for traversal
my @cells = do for (flat 0..8 X 0..8) -> $x, $y { [ $x, $y ] };

#
# Try to solve this puzzle and return the resolved puzzle if it is at
# all solvable in this configuration.
sub solve($sudoku, Int $level) {
    # cleanup the impossible values first,
    if (cleanup-impossible-values($sudoku, $level)) {
        # try to find implicit answers
        while (find-implicit-answers($sudoku, $level)) {
            # and every time you find some, re-do the cleanup and try again
            cleanup-impossible-values($sudoku, $level);
        }
        # Now let's actually try to solve a new value. But instead of
        # going in sequence, we select the cell that is the closest to
        # being solved already. This will reduce the overall number of
        # guesses.
        for sort { solution-complexity-factor($sudoku, $_[0], $_[1]) },
        grep { $sudoku[$_[0]][$_[1]] ~~ Array },
        @cells -> $cell
        {
            my Int ($x, $y) = @($cell);
            # Now let's try the possible values in the order of
            # uniqueness.
            for sort { matches-in-competing-cells($sudoku, $x, $y, $_) }, @($sudoku[$x][$y]) -> $val {
                trace $level, "Trying $val on "~($x+1)~","~($y+1)~" "~$sudoku[$x][$y].perl;
                my $solution = clone-sudoku($sudoku);
                $solution[$x][$y] = $val;
                my $solved = solve($solution, $level+1);
                if $solved {
                    trace $level, "Solved... ($val on "~($x+1)~","~($y+1)~")";
                    return $solved;
                }
            }
            # if we fell through, it means that we found no valid
            # value for this cell
            trace $level, "Backtrack, path unsolvable... (on "~($x+1)~" "~($y+1)~")";
            return 0;
        }
        # all cells are already solved.
        return $sudoku;
    } else {
        # if the cleanup failed, it means this is an invalid grid.
        return False;
    }
}

# This function reduces the search space from values that are already
# assigned to competing cells.
sub cleanup-impossible-values($sudoku, Int $level = 1) {
    my Bool $resolved;
    repeat {
        $resolved = False;
        for grep { $sudoku[$_[0]][$_[1]] ~~ Array },
        @cells -> $cell {
            my Int ($x, $y) = @($cell);
            # which block is this cell in
            my Int $bx = Int($x / 3);
            my Int $by = Int($y / 3);

            # A unfilled cell is not resolved, so it shouldn't match
            my multi match-resolved-cell(Array $other, Int $this) {
                return 0;
            }
            my multi match-resolved-cell(Int $other, Int $this) {
                return $other == $this;
            }

            # Reduce the possible values to the ones that are still
            # valid
            my @r =
                grep { !match-resolved-cell($sudoku[any(0..2)+3*$bx][any(0..2)+3*$by], $_) }, # same block
                grep { !match-resolved-cell($sudoku[any(0..8)][$y], $_) }, # same line
                grep { !match-resolved-cell($sudoku[$x][any(0..8)], $_) }, # same column
                @($sudoku[$x][$y]);
            if (@r.elems == 1) {
                # if only one element is left, then make it resolved
                $sudoku[$x][$y] = @r[0];
                $resolved = True;
            } elsif (@r.elems == 0) {
                # This is an invalid grid
                return 0;
            } else {
                $sudoku[$x][$y] = @r;
            }
        }
    } while $resolved; # repeat if there was any change
    return 1;
}

sub solution-complexity-factor($sudoku, Int $x, Int $y) {
    my Int $bx = Int($x / 3); # this block
    my Int $by = Int($y / 3);
    my multi count-values(Array $val) {
        return $val.elems;
    }
    my multi count-values(Int $val) {
        return 1;
    }
    # the number of possible values should take precedence
    my Int $f = 1000 * count-values($sudoku[$x][$y]);
    for (flat 0..2 X 0..2) -> $lx, $ly {
        $f += count-values($sudoku[$lx+$bx*3][$ly+$by*3])
    }
    for 0..^($by*3), (($by+1)*3)..8 -> $ly {
        $f += count-values($sudoku[$x][$ly])
    }
    for 0..^($bx*3), (($bx+1)*3)..8 -> $lx {
        $f += count-values($sudoku[$lx][$y])
    }
    return $f;
}

sub matches-in-competing-cells($sudoku, Int $x, Int $y, Int $val) {
    my Int $bx = Int($x / 3); # this block
    my Int $by = Int($y / 3);
    # Function to decide which possible value to try first
    my multi cell-matching(Int $cell) {
        return $val == $cell ?? 1 !! 0;
    }
    my multi cell-matching(Array $cell) {
        return $cell.grep({ $val == $_ }) ?? 1 !! 0;
    }
    my Int $c = 0;
    for (flat 0..2 X 0..2) -> $lx, $ly {
        $c += cell-matching($sudoku[$lx+$bx*3][$ly+$by*3])
    }
    for 0..^($by*3), (($by+1)*3)..8 -> $ly {
        $c += cell-matching($sudoku[$x][$ly])
    }
    for 0..^($bx*3), (($bx+1)*3)..8 -> $lx {
        $c += cell-matching($sudoku[$lx][$y])
    }
    return $c;
}

sub find-implicit-answers($sudoku, Int $level) {
    my Bool $resolved = False;
    for grep { $sudoku[$_[0]][$_[1]] ~~ Array },
    @cells -> $cell {
        my Int ($x, $y) = @($cell);
        for @($sudoku[$x][$y]) -> $val {
            # If this is the only cell with this val as a possibility,
            # just make it resolved already
            if (matches-in-competing-cells($sudoku, $x, $y, $val) == 1) {
                $sudoku[$x][$y] = $val;
                $resolved = True;
            }
        }
    }
    return $resolved;
}

my $puzzle =
    map { [ map { $_ == 0 ?? [1..9] !! $_+0  }, @($_) ] },
    [ 0,0,0,0,3,7,6,0,0 ],
    [ 0,0,0,6,0,0,0,9,0 ],
    [ 0,0,8,0,0,0,0,0,4 ],
    [ 0,9,0,0,0,0,0,0,1 ],
    [ 6,0,0,0,0,0,0,0,9 ],
    [ 3,0,0,0,0,0,0,4,0 ],
    [ 7,0,0,0,0,0,8,0,0 ],
    [ 0,1,0,0,0,9,0,0,0 ],
    [ 0,0,2,5,4,0,0,0,0 ];

my $solved = solve($puzzle, 0);
if $solved {
    print-sudoku($solved,0);
} else {
    say "unsolvable.";
}

# Utility functions, not really part of the solution

sub trace(Int $level, Str $message) {
    say '.' x $level, $message;
}

sub clone-sudoku($sudoku) {
    my $clone;
    for (flat 0..8 X 0..8) -> $x, $y {
        $clone[$x][$y] = $sudoku[$x][$y];
    }
    return $clone;
}

sub print-sudoku($sudoku, Int $level = 1) {
    trace $level, '-' x 5*9;
    for @($sudoku) -> $row {
        trace $level, join " ", do for @($row) -> $cell {
            $cell ~~ Array ?? "#{$cell.elems}#" !! " $cell "
        }
    }
}
```


{{out}}

```txt

Trying 8 on 9,1 [8, 9]
.Trying 6 on 9,2 [3, 6]
..Trying 7 on 9,9 [3, 7]
...Trying 1 on 9,8 [1, 3]
....Trying 4 on 8,1 [4, 5]
.....Trying 3 on 7,2 [3, 5]
......Trying 3 on 8,7 [2, 3]
.......Trying 6 on 8,9 [2, 6]
.......Trying 2 on 8,9 [2, 6]
.......Backtrack, path unsolvable... (on 8 9)
......Trying 2 on 8,7 [2, 3]
.......Trying 3 on 8,9 [3, 6]
.......Trying 6 on 8,9 [3, 6]
.......Backtrack, path unsolvable... (on 8 9)
......Backtrack, path unsolvable... (on 8 7)
.....Trying 5 on 7,2 [3, 5]
......Trying 5 on 8,7 [2, 5]
.......Trying 6 on 8,9 [2, 6]
.......Trying 2 on 8,9 [2, 6]
.......Backtrack, path unsolvable... (on 8 9)
......Trying 2 on 8,7 [2, 5]
.......Trying 5 on 8,9 [5, 6]
.......Trying 6 on 8,9 [5, 6]
.......Backtrack, path unsolvable... (on 8 9)
......Backtrack, path unsolvable... (on 8 7)
.....Backtrack, path unsolvable... (on 7 2)
....Trying 5 on 8,1 [4, 5]
.....Trying 3 on 8,3 [3, 4]
......Trying 6 on 8,9 [2, 6]
.......Trying 3 on 7,9 [3, 5]
........Trying 5 on 1,9 [2, 5]
.........Trying 3 on 3,8 [3, 7]
..........Trying 4 on 2,1 [1, 4]
..........Trying 1 on 2,1 [1, 4]
..........Backtrack, path unsolvable... (on 2 1)
.........Trying 7 on 3,8 [3, 7]
..........Trying 3 on 3,7 [1, 3]
..........Trying 1 on 3,7 [1, 3]
..........Backtrack, path unsolvable... (on 3 7)
.........Backtrack, path unsolvable... (on 3 8)
........Trying 2 on 1,9 [2, 5]
.........Trying 3 on 3,8 [3, 7]
..........Trying 7 on 2,7 [1, 7]
...........Trying 9 on 3,1 [2, 9]
...........Trying 2 on 3,1 [2, 9]
...........Backtrack, path unsolvable... (on 3 1)
..........Trying 1 on 2,7 [1, 7]
..........Backtrack, path unsolvable... (on 2 7)
.........Trying 7 on 3,8 [3, 7]
..........Trying 3 on 3,7 [1, 3]
..........Trying 1 on 3,7 [1, 3]
...........Trying 9 on 3,1 [2, 9]
...........Trying 2 on 3,1 [2, 9]
...........Backtrack, path unsolvable... (on 3 1)
..........Backtrack, path unsolvable... (on 3 7)
.........Backtrack, path unsolvable... (on 3 8)
........Backtrack, path unsolvable... (on 1 9)
.......Trying 5 on 7,9 [3, 5]
........Trying 8 on 1,9 [2, 8]
.........Trying 2 on 3,7 [1, 2]
.........Trying 1 on 3,7 [1, 2]
.........Backtrack, path unsolvable... (on 3 7)
........Trying 2 on 1,9 [2, 8]
.........Trying 7 on 3,8 [5, 7]
..........Trying 5 on 3,7 [1, 5]
...........Trying 2 on 2,1 [2, 4]
...........Trying 4 on 2,1 [2, 4]
...........Backtrack, path unsolvable... (on 2 1)
..........Trying 1 on 3,7 [1, 5]
...........Trying 9 on 3,1 [2, 9]
...........Trying 2 on 3,1 [2, 9]
...........Backtrack, path unsolvable... (on 3 1)
..........Backtrack, path unsolvable... (on 3 7)
.........Trying 5 on 3,8 [5, 7]
..........Trying 7 on 3,7 [1, 7]
...........Trying 2 on 2,1 [2, 4]
...........Trying 4 on 2,1 [2, 4]
...........Backtrack, path unsolvable... (on 2 1)
..........Trying 1 on 3,7 [1, 7]
..........Backtrack, path unsolvable... (on 3 7)
.........Backtrack, path unsolvable... (on 3 8)
........Backtrack, path unsolvable... (on 1 9)
.......Backtrack, path unsolvable... (on 7 9)
......Trying 2 on 8,9 [2, 6]
.......Trying 3 on 7,9 [3, 5]
........Trying 8 on 1,9 [5, 8]
.........Trying 3 on 3,8 [3, 7]
..........Trying 4 on 1,3 [1, 4]
...........Trying 9 on 1,1 [1, 9]
............Trying 1 on 3,1 [1, 2]
............Trying 2 on 3,1 [1, 2]
............Backtrack, path unsolvable... (on 3 1)
...........Trying 1 on 1,1 [1, 9]
...........Backtrack, path unsolvable... (on 1 1)
..........Trying 1 on 1,3 [1, 4]
...........Trying 9 on 1,1 [4, 9]
...........Trying 4 on 1,1 [4, 9]
...........Backtrack, path unsolvable... (on 1 1)
..........Backtrack, path unsolvable... (on 1 3)
.........Trying 7 on 3,8 [3, 7]
..........Trying 3 on 3,7 [1, 3]
..........Trying 1 on 3,7 [1, 3]
...........Trying 9 on 3,1 [2, 9]
...........Trying 2 on 3,1 [2, 9]
...........Backtrack, path unsolvable... (on 3 1)
..........Backtrack, path unsolvable... (on 3 7)
.........Backtrack, path unsolvable... (on 3 8)
........Trying 5 on 1,9 [5, 8]
........Backtrack, path unsolvable... (on 1 9)
.......Trying 5 on 7,9 [3, 5]
........Trying 5 on 1,8 [2, 5]
.........Trying 7 on 3,8 [2, 7]
..........Trying 4 on 1,3 [1, 4]
..........Trying 1 on 1,3 [1, 4]
..........Backtrack, path unsolvable... (on 1 3)
.........Trying 2 on 3,8 [2, 7]
..........Trying 9 on 3,1 [1, 9]
..........Trying 1 on 3,1 [1, 9]
..........Backtrack, path unsolvable... (on 3 1)
.........Backtrack, path unsolvable... (on 3 8)
........Trying 2 on 1,8 [2, 5]
.........Trying 7 on 3,8 [5, 7]
..........Trying 4 on 1,3 [1, 4]
...........Trying 9 on 1,1 [1, 9]
............Trying 1 on 3,1 [1, 2]
............Trying 2 on 3,1 [1, 2]
............Backtrack, path unsolvable... (on 3 1)
...........Trying 1 on 1,1 [1, 9]
...........Backtrack, path unsolvable... (on 1 1)
..........Trying 1 on 1,3 [1, 4]
...........Trying 9 on 1,1 [4, 9]
...........Trying 4 on 1,1 [4, 9]
...........Backtrack, path unsolvable... (on 1 1)
..........Backtrack, path unsolvable... (on 1 3)
.........Trying 5 on 3,8 [5, 7]
..........Trying 7 on 3,7 [1, 7]
...........Trying 2 on 2,1 [2, 4]
...........Trying 4 on 2,1 [2, 4]
...........Backtrack, path unsolvable... (on 2 1)
..........Trying 1 on 3,7 [1, 7]
..........Backtrack, path unsolvable... (on 3 7)
.........Backtrack, path unsolvable... (on 3 8)
........Backtrack, path unsolvable... (on 1 8)
.......Backtrack, path unsolvable... (on 7 9)
......Backtrack, path unsolvable... (on 8 9)
.....Trying 4 on 8,3 [3, 4]
......Trying 3 on 8,7 [2, 3]
.......Trying 6 on 8,9 [2, 6]
.......Trying 2 on 8,9 [2, 6]
.......Backtrack, path unsolvable... (on 8 9)
......Trying 2 on 8,7 [2, 3]
.......Trying 3 on 8,9 [3, 6]
.......Trying 6 on 8,9 [3, 6]
.......Backtrack, path unsolvable... (on 8 9)
......Backtrack, path unsolvable... (on 8 7)
.....Backtrack, path unsolvable... (on 8 3)
....Backtrack, path unsolvable... (on 8 1)
...Trying 3 on 9,8 [1, 3]
....Trying 4 on 8,1 [4, 5]
.....Trying 3 on 7,2 [3, 5]
.....Trying 5 on 7,2 [3, 5]
......Trying 6 on 7,9 [2, 6]
......Trying 2 on 7,9 [2, 6]
......Backtrack, path unsolvable... (on 7 9)
.....Backtrack, path unsolvable... (on 7 2)
....Trying 5 on 8,1 [4, 5]
.....Trying 6 on 8,9 [2, 6]
......Trying 8 on 1,9 [2, 8]
.......Trying 1 on 3,7 [1, 2]
.......Trying 2 on 3,7 [1, 2]
.......Backtrack, path unsolvable... (on 3 7)
......Trying 2 on 1,9 [2, 8]
.......Trying 7 on 3,8 [5, 7]
........Trying 5 on 3,7 [1, 5]
.........Trying 9 on 3,1 [1, 9]
.........Trying 1 on 3,1 [1, 9]
.........Backtrack, path unsolvable... (on 3 1)
........Trying 1 on 3,7 [1, 5]
.........Trying 9 on 3,1 [2, 9]
.........Trying 2 on 3,1 [2, 9]
.........Backtrack, path unsolvable... (on 3 1)
........Backtrack, path unsolvable... (on 3 7)
.......Trying 5 on 3,8 [5, 7]
........Trying 7 on 3,7 [1, 7]
.........Trying 9 on 3,1 [1, 9]
.........Trying 1 on 3,1 [1, 9]
.........Backtrack, path unsolvable... (on 3 1)
........Trying 1 on 3,7 [1, 7]
........Backtrack, path unsolvable... (on 3 7)
.......Backtrack, path unsolvable... (on 3 8)
......Backtrack, path unsolvable... (on 1 9)
.....Trying 2 on 8,9 [2, 6]
......Trying 5 on 1,8 [2, 5]
.......Trying 7 on 3,8 [2, 7]
........Trying 4 on 2,1 [1, 4]
........Trying 1 on 2,1 [1, 4]
........Backtrack, path unsolvable... (on 2 1)
.......Trying 2 on 3,8 [2, 7]
........Trying 9 on 3,1 [1, 9]
........Trying 1 on 3,1 [1, 9]
........Backtrack, path unsolvable... (on 3 1)
.......Backtrack, path unsolvable... (on 3 8)
......Trying 2 on 1,8 [2, 5]
.......Trying 7 on 3,8 [5, 7]
........Trying 1 on 3,7 [1, 5]
.........Trying 9 on 3,1 [2, 9]
.........Trying 2 on 3,1 [2, 9]
.........Backtrack, path unsolvable... (on 3 1)
........Trying 5 on 3,7 [1, 5]
.........Trying 9 on 3,1 [1, 9]
.........Trying 1 on 3,1 [1, 9]
.........Backtrack, path unsolvable... (on 3 1)
........Backtrack, path unsolvable... (on 3 7)
.......Trying 5 on 3,8 [5, 7]
........Trying 9 on 3,1 [1, 9]
........Trying 1 on 3,1 [1, 9]
........Backtrack, path unsolvable... (on 3 1)
.......Backtrack, path unsolvable... (on 3 8)
......Backtrack, path unsolvable... (on 1 8)
.....Backtrack, path unsolvable... (on 8 9)
....Backtrack, path unsolvable... (on 8 1)
...Backtrack, path unsolvable... (on 9 8)
..Trying 3 on 9,9 [3, 7]
...Trying 4 on 8,1 [4, 5]
....Trying 3 on 7,2 [3, 5]
....Trying 5 on 7,2 [3, 5]
.....Trying 6 on 7,9 [2, 6]
.....Trying 2 on 7,9 [2, 6]
.....Backtrack, path unsolvable... (on 7 9)
....Backtrack, path unsolvable... (on 7 2)
...Trying 5 on 8,1 [4, 5]
....Trying 6 on 8,9 [2, 6]
.....Trying 8 on 1,9 [2, 8]
......Trying 7 on 2,9 [2, 7]
.......Trying 1 on 3,7 [1, 2]
.......Trying 2 on 3,7 [1, 2]
.......Backtrack, path unsolvable... (on 3 7)
......Trying 2 on 2,9 [2, 7]
.......Trying 4 on 2,1 [1, 4]
.......Trying 1 on 2,1 [1, 4]
.......Backtrack, path unsolvable... (on 2 1)
......Backtrack, path unsolvable... (on 2 9)
.....Trying 2 on 1,9 [2, 8]
......Trying 3 on 3,8 [3, 5]
.......Trying 5 on 2,7 [1, 5]
........Trying 9 on 3,1 [2, 9]
........Trying 2 on 3,1 [2, 9]
........Backtrack, path unsolvable... (on 3 1)
.......Trying 1 on 2,7 [1, 5]
.......Backtrack, path unsolvable... (on 2 7)
......Trying 5 on 3,8 [3, 5]
.......Trying 3 on 3,7 [1, 3]
.......Trying 1 on 3,7 [1, 3]
.......Backtrack, path unsolvable... (on 3 7)
......Backtrack, path unsolvable... (on 3 8)
.....Backtrack, path unsolvable... (on 1 9)
....Trying 2 on 8,9 [2, 6]
.....Trying 5 on 1,8 [2, 5]
......Trying 3 on 3,8 [2, 3]
.......Trying 4 on 2,1 [1, 4]
.......Trying 1 on 2,1 [1, 4]
.......Backtrack, path unsolvable... (on 2 1)
......Trying 2 on 3,8 [2, 3]
.......Trying 9 on 3,1 [1, 9]
.......Trying 1 on 3,1 [1, 9]
.......Backtrack, path unsolvable... (on 3 1)
......Backtrack, path unsolvable... (on 3 8)
.....Trying 2 on 1,8 [2, 5]
......Trying 3 on 3,8 [3, 5]
.......Trying 4 on 1,3 [1, 4]
.......Trying 1 on 1,3 [1, 4]
.......Backtrack, path unsolvable... (on 1 3)
......Trying 5 on 3,8 [3, 5]
.......Trying 9 on 3,1 [1, 9]
.......Trying 1 on 3,1 [1, 9]
.......Backtrack, path unsolvable... (on 3 1)
......Backtrack, path unsolvable... (on 3 8)
.....Backtrack, path unsolvable... (on 1 8)
....Backtrack, path unsolvable... (on 8 9)
...Backtrack, path unsolvable... (on 8 1)
..Backtrack, path unsolvable... (on 9 9)
.Trying 3 on 9,2 [3, 6]
..Trying 7 on 9,9 [6, 7]
...Trying 1 on 9,8 [1, 6]
....Trying 4 on 8,1 [4, 5]
.....Trying 6 on 7,2 [5, 6]
......Trying 3 on 8,7 [2, 3]
.......Trying 6 on 8,9 [2, 6]
.......Trying 2 on 8,9 [2, 6]
.......Backtrack, path unsolvable... (on 8 9)
......Trying 2 on 8,7 [2, 3]
.......Trying 6 on 8,9 [3, 6]
.......Trying 3 on 8,9 [3, 6]
.......Backtrack, path unsolvable... (on 8 9)
......Backtrack, path unsolvable... (on 8 7)
.....Trying 5 on 7,2 [5, 6]
......Trying 4 on 1,2 [2, 4]
.......Trying 1 on 1,3 [1, 5]
........Trying 2 on 2,1 [2, 5]
........Trying 5 on 2,1 [2, 5]
........Backtrack, path unsolvable... (on 2 1)
.......Trying 5 on 1,3 [1, 5]
........Trying 1 on 2,1 [1, 2]
.........Trying 9 on 1,1 [2, 9]
..........Trying 8 on 1,9 [2, 8]
..........Trying 2 on 1,9 [2, 8]
..........Backtrack, path unsolvable... (on 1 9)
.........Trying 2 on 1,1 [2, 9]
.........Backtrack, path unsolvable... (on 1 1)
........Trying 2 on 2,1 [1, 2]
.........Trying 9 on 1,1 [1, 9]
..........Trying 8 on 1,9 [2, 8]
...........Trying 2 on 8,9 [2, 3]
...........Trying 3 on 8,9 [2, 3]
...........Backtrack, path unsolvable... (on 8 9)
..........Trying 2 on 1,9 [2, 8]
..........Backtrack, path unsolvable... (on 1 9)
.........Trying 1 on 1,1 [1, 9]
..........Trying 8 on 1,9 [2, 8]
...........Trying 2 on 8,9 [2, 3]
...........Trying 3 on 8,9 [2, 3]
...........Backtrack, path unsolvable... (on 8 9)
..........Trying 2 on 1,9 [2, 8]
..........Backtrack, path unsolvable... (on 1 9)
.........Backtrack, path unsolvable... (on 1 1)
........Backtrack, path unsolvable... (on 2 1)
.......Backtrack, path unsolvable... (on 1 3)
......Trying 2 on 1,2 [2, 4]
.......Trying 1 on 2,1 [1, 5]
........Trying 9 on 1,1 [5, 9]
.........Trying 8 on 1,9 [5, 8]
.........Trying 5 on 1,9 [5, 8]
.........Backtrack, path unsolvable... (on 1 9)
........Trying 5 on 1,1 [5, 9]
........Backtrack, path unsolvable... (on 1 1)
.......Trying 5 on 2,1 [1, 5]
........Trying 9 on 1,1 [1, 9]
.........Trying 8 on 1,9 [5, 8]
..........Trying 6 on 7,9 [3, 6]
..........Trying 3 on 7,9 [3, 6]
..........Backtrack, path unsolvable... (on 7 9)
.........Trying 5 on 1,9 [5, 8]
.........Backtrack, path unsolvable... (on 1 9)
........Trying 1 on 1,1 [1, 9]
.........Trying 8 on 1,9 [5, 8]
..........Trying 5 on 8,9 [3, 5]
..........Trying 3 on 8,9 [3, 5]
..........Backtrack, path unsolvable... (on 8 9)
.........Trying 5 on 1,9 [5, 8]
.........Backtrack, path unsolvable... (on 1 9)
........Backtrack, path unsolvable... (on 1 1)
.......Backtrack, path unsolvable... (on 2 1)
......Backtrack, path unsolvable... (on 1 2)
.....Backtrack, path unsolvable... (on 7 2)
....Trying 5 on 8,1 [4, 5]
.....Trying 6 on 8,3 [4, 6]
......Trying 3 on 8,9 [2, 3]
.......Trying 6 on 7,9 [5, 6]
........Trying 5 on 1,9 [2, 5]
.........Trying 3 on 3,8 [3, 7]
..........Trying 4 on 2,1 [1, 4]
..........Trying 1 on 2,1 [1, 4]
..........Backtrack, path unsolvable... (on 2 1)
.........Trying 7 on 3,8 [3, 7]
..........Trying 3 on 3,7 [1, 3]
..........Trying 1 on 3,7 [1, 3]
..........Backtrack, path unsolvable... (on 3 7)
.........Backtrack, path unsolvable... (on 3 8)
........Trying 2 on 1,9 [2, 5]
.........Trying 3 on 3,8 [3, 7]
..........Trying 7 on 2,7 [1, 7]
..........Trying 1 on 2,7 [1, 7]
...........Trying 4 on 2,1 [2, 4]
...........Trying 2 on 2,1 [2, 4]
...........Solved... (2 on 2,1)
..........Solved... (1 on 2,7)
.........Solved... (3 on 3,8)
........Solved... (2 on 1,9)
.......Solved... (6 on 7,9)
......Solved... (3 on 8,9)
.....Solved... (6 on 8,3)
....Solved... (5 on 8,1)
...Solved... (1 on 9,8)
..Solved... (7 on 9,9)
.Solved... (3 on 9,2)
Solved... (8 on 9,1)
---------------------------------------------
 9   5   4   1   3   7   6   8   2
 2   7   3   6   8   4   1   9   5
 1   6   8   2   9   5   7   3   4
 4   9   5   7   2   8   3   6   1
 6   8   1   4   5   3   2   7   9
 3   2   7   9   6   1   5   4   8
 7   4   9   3   1   2   8   5   6
 5   1   6   8   7   9   4   2   3
 8   3   2   5   4   6   9   1   7

```



## PHP

{{trans|C++}}

```php
	class SudokuSolver {
		protected $grid = [];
		protected $emptySymbol;
		public static function parseString($str, $emptySymbol = '0')
		{
			$grid = str_split($str);
			foreach($grid as &$v)
			{
				if($v == $emptySymbol)
				{
					$v = 0;
				}
				else
				{
					$v = (int)$v;
				}
			}
			return $grid;
		}

		public function __construct($str, $emptySymbol = '0') {
			if(strlen($str) !== 81)
			{
				throw new \Exception('Error sudoku');
			}
			$this->grid = static::parseString($str, $emptySymbol);
			$this->emptySymbol = $emptySymbol;
		}

		public function solve()
		{
			try
			{
				$this->placeNumber(0);
				return false;
			}
			catch(\Exception $e)
			{
				return true;
			}
		}

		protected function placeNumber($pos)
		{
			if($pos == 81)
			{
				throw new \Exception('Finish');
			}
			if($this->grid[$pos] > 0)
			{
				$this->placeNumber($pos+1);
				return;
			}
			for($n = 1; $n <= 9; $n++)
			{
				if($this->checkValidity($n, $pos%9, floor($pos/9)))
				{
					$this->grid[$pos] = $n;
					$this->placeNumber($pos+1);
					$this->grid[$pos] = 0;
				}
			}
		}

		protected function checkValidity($val, $x, $y)
		{
			for($i = 0; $i < 9; $i++)
			{
				if(($this->grid[$y*9+$i] == $val) || ($this->grid[$i*9+$x] == $val))
				{
					return false;
				}
			}
			$startX = (int) ((int)($x/3)*3);
			$startY = (int) ((int)($y/3)*3);

			for($i = $startY; $i<$startY+3;$i++)
			{
				for($j = $startX; $j<$startX+3;$j++)
				{
					if($this->grid[$i*9+$j] == $val)
					{
						return false;
					}
				}
			}
			return true;
		}

		public function display() {
			$str = '';
			for($i = 0; $i<9; $i++)
			{
				for($j = 0; $j<9;$j++)
				{
					$str .= $this->grid[$i*9+$j];
					$str .= " ";
					if($j == 2 || $j == 5)
					{
						$str .= "| ";
					}
				}
				$str .= PHP_EOL;
				if($i == 2 || $i == 5)
				{
					$str .=  "------+-------+------".PHP_EOL;
				}
			}
			echo $str;
		}

		public function __toString() {
			foreach ($this->grid as &$item)
			{
				if($item == 0)
				{
					$item = $this->emptySymbol;
				}
			}
			return implode('', $this->grid);
		}
	}
	$solver = new SudokuSolver('009170000020600001800200000200006053000051009005040080040000700006000320700003900');
	$solver->solve();
	$solver->display();
```

{{out}}

```txt

3 6 9 | 1 7 5 | 8 4 2
4 2 7 | 6 8 9 | 5 3 1
8 5 1 | 2 3 4 | 6 9 7
------+-------+------
2 1 8 | 7 9 6 | 4 5 3
6 3 4 | 8 5 1 | 2 7 9
9 7 5 | 3 4 2 | 1 8 6
------+-------+------
1 4 3 | 9 2 8 | 7 6 5
5 9 6 | 4 1 7 | 3 2 8
7 8 2 | 5 6 3 | 9 1 4
(solved in 0.027s)

```



## Phix

Simple brute force solution. Generally quite good but will struggle on some puzzles (eg see "the beast" below)

```Phix
sequence board = split("""
.......39
.....1..5
..3.5.8..
..8.9...6
.7...2...
1..4.....
..9.8..5.
.2....6..
4..7.....""",'\n')

function valid_move(integer y, integer x, integer ch)
    for i=1 to 9 do
        if ch=board[i][x] then return 0 end if
        if ch=board[y][i] then return 0 end if
    end for
    y -= mod(y-1,3)
    x -= mod(x-1,3)
    for ys=y to y+2 do
        for xs=x to x+2  do
            if ch=board[ys][xs] then return 0 end if
        end for
    end for
    return 1
end function

sequence solution = {}

procedure brute_solve()
    for y=1 to 9 do
        for x=1 to 9 do
            if board[y][x]<='0' then
                for ch='1' to '9' do
                    if valid_move(y,x,ch) then
                        board[y][x] = ch
                        brute_solve()
                        board[y][x] = ' '
                        if length(solution) then return end if
                    end if
                end for
                return
            end if
        end for
    end for
    solution = board    -- (already solved case)
end procedure

atom t0 = time()
brute_solve()
printf(1,"%s\n(solved in %3.2fs)\n",{join(solution,"\n"),time()-t0})
```

{{out}}

```txt

751846239
892371465
643259871
238197546
974562318
165438927
319684752
527913684
486725193
(solved in 0.95s)

```

OTT solution. Implements line/col and set exclusion, and x-wings. Blisteringly fast<BR>
The included program demo\rosetta\Sudoku.exw is an extended version of this that performs extended validation,
contains 339 puzzles, can be run as a command-line or gui program, check for multiple solutions, and produce
a more readable single-puzzle output (example below).

```Phix
-- Working directly on 81-character strings ultimately proves easier: Originally I
--  just wanted to simplify the final display, but later I realised that a 9x9 grid
--  encourages laborious indexing/looping everwhere whereas using a flat 81-element
--  approach encourages precomputation of index sets, and once you commit to that,
--  the rest of the code starts to get a whole lot cleaner. Below we create 27+18
--  sets and 5 tables of lookup indexes to locate them quickly.

sequence nines = {},                -- will be 27 in total
         cols = repeat(0,9*9),      -- remainder(i-1,9)+1
         rows = repeat(0,9*9),      -- floor((i-1)/9)+10
         squares = repeat(0,9*9),
         sixes = {},                -- will be 18 in total
         dotcol = repeat(0,9*9),    -- same col, diff square
         dotrow = repeat(0,9*9)     -- same row, diff square

procedure set_nines()
sequence nine, six
integer idx, ndx
    for x=0 to 8 do                     -- columns
        nine = {}
        ndx = length(nines)+1
        for y=1 to 81 by 9 do
            idx = y+x
            nine = append(nine,idx)
            cols[idx] = ndx
        end for
        nines = append(nines,nine)
    end for
    for y=1 to 81 by 9 do               -- rows
        nine = {}
        ndx = length(nines)+1
        for x=0 to 8 do
            idx = y+x
            nine = append(nine,idx)
            rows[idx] = ndx
        end for
        nines = append(nines,nine)
    end for
    if length(nines)!=18 then ?9/0 end if
    for y=0 to 8 by 3 do                -- small squares [19..27]
        for x=0 to 8 by 3 do
            nine = {}
            ndx = length(nines)+1
            for sy=y*9 to y*9+18 by 9 do
                for sx=x to x+2 do
                    idx = sy+sx+1
                    nine = append(nine,idx)
                    squares[idx] = ndx
                end for
            end for
            nines = append(nines,nine)
        end for
    end for
    if length(nines)!=27 then ?9/0 end if
    for i=1 to 9*9 do
        six = {}
        nine = nines[cols[i]]           -- dotcol
        for j=1 to length(nine) do
            if squares[i]!=squares[nine[j]] then
                six = append(six,nine[j])
            end if
        end for
        ndx = find(six,sixes)
        if ndx=0 then
            sixes = append(sixes,six)
            ndx = length(sixes)
        end if
        dotcol[i] = ndx
        six = {}
        nine = nines[rows[i]]           -- dotrow
        for j=1 to length(nine) do
            if squares[i]!=squares[nine[j]] then
                six = append(six,nine[j])
            end if
        end for
        ndx = find(six,sixes)
        if ndx=0 then
            sixes = append(sixes,six)
            ndx = length(sixes)
        end if
        dotrow[i] = ndx
    end for
end procedure
set_nines()

integer improved = 0

function eliminate_in(sequence valid, sequence set, integer ch)
    for i=1 to length(set) do
        integer idx = set[i]
        if string(valid[idx]) then
            integer k = find(ch,valid[idx])
            if k!=0 then
                valid[idx][k..k] = ""
                improved = 1
            end if
        end if
    end for
    return valid
end function

function test_comb(sequence chosen, sequence pool, sequence valid)
--
-- (see deep_logic()/set elimination)
-- chosen is a sequence of length 2..4 of integers 1..9: ordered elements of pool.
-- pool is a set of elements of the sequence valid, each of which is a sequence.
-- (note that elements of valid in pool not in chosen are not necessarily sequences)
--
sequence contains = repeat(0,9)
integer ccount = 0, ch
object set

    for i=1 to length(chosen) do
        set = valid[pool[chosen[i]]]
        for j=1 to length(set) do
            ch = set[j]-'0'
            if contains[ch]=0 then
                contains[ch] = 1
                ccount += 1
            end if
        end for
    end for
    if ccount=length(chosen) then
        for i=1 to length(pool) do
            if find(i,chosen)=0 then
                set = valid[pool[i]]
                if sequence(set) then
                    -- (reverse order so deletions don't foul indexes)
                    for j=length(set) to 1 by -1 do
                        ch = set[j]-'0'
                        if contains[ch] then
                            valid[pool[i]][j..j] = ""
                            improved = 1
                        end if
                    end for
                end if
            end if
        end for
    end if
    return valid
end function

-- from [[Combinations#Phix|Combinations]]
-- from http://rosettacode.org/wiki/Combinations#Phix
function comb(sequence pool, valid, integer needed, done=0, sequence chosen={})
-- (used by deep_logic()/set elimination)
    if needed=0 then    -- got a full set
        return test_comb(chosen,pool,valid)
    end if
    if done+needed>length(pool) then return valid end if -- cannot fulfil
    -- get all combinations with and without the next item:
    done += 1
    if sequence(valid[pool[done]]) then
        valid = comb(pool,valid,needed-1,done,append(chosen,done))
    end if
    return comb(pool,valid,needed,done,chosen)
end function

function deep_logic(string board, sequence valid)
--
--  Create a grid of valid moves. Note this does not modify board, but instead creates
--  sets of permitted values for each cell, which can also be and are used for hints.
--  Apply standard eliminations of known cells, then try some more advanced tactics:
--
--  1) row/col elimination
--      If in any of the 9 small squares a number can only occur in one row or column,
--      then that number cannot occur in that row or column in two other corresponding
--      small squares. Example (this one with significant practical benefit):
--              000|000|036
--              840|000|000
--              000|000|020
--              ---+---+---
--              000|203|000
--              010|000|700
--              000|600|400
--              ---+---+---
--              000|410|050
--              003|000|200
--              600|000|000 <-- 3
--                        ^-- 3
--      Naively, the br can contain a 3 in the four corners, but looking at mid-right and
--      mid-bottom leads us to eliminating 3s in column 9 and row 9, leaving 7,7 as the
--      only square in the br that can be a 3. Uses dotcol and dotrow.
--      Without this, brute force on the above takes ~8s, but with it ~0s
--
--  2) set elimination
--      If in any 9-set there is a set of n blank squares that can only contain n digits,
--      then no other squares can contain those digits. Example (with some benefit):
--              75.|.9.|.46
--              961|...|352
--              4..|...|79.
--              ---+---+---
--              2..|6.1|..7
--              .8.|...|.2.
--              1..|328|.65
--              ---+---+---
--              ...|...|...     <-- [7,8] is {1,3,8}, [7,9] is {1,3,8}
--              3.9|...|2.4     <-- [8,8] is {1,8}
--              84.|.3.|.79
--      The three cells above the br 479 can only contain {1,3,8}, so the .. of the .2.
--      in column 7 of that square are {5,6} (not 1) and hence [9,4] must be a 1.
--      (Relies on plain_logic to spot that "must be a 1", and serves as a clear example
--       of why this routine should not bother to attempt updating the board itself - as
--       it spends almost all of its time looking in a completely different place.)
--      (One could argue that [7,7] and [9,7] are the only places that can hold {5,6} and
--       therefore we should eliminate all non-{5,6} from those squares, as an alternative
--       strategy. However I think that would be harder to code and cannot imagine a case
--       said complementary logic covers, that the above does not, cmiiw.)
--
--  3) x-wings
--      If a pair of rows or columns can only contain a given number in two matching places,
--      then once filled they will occupy opposite diagonal corners, hence that said number
--      cannot occur elsewhere in those two columns/rows. Example (with a benefit):
--              .43|98.|25. <-- 6 in [1,{6,9}]
--              6..|425|...
--              2..|..1|.94
--              ---+---+---
--              9..|..4|.7.                     <-- hence 6 not in [4,9]
--              3..|6.8|...
--              41.|2.9|..3
--              ---+---+---
--              82.|5..|...                     <-- hence 6 not in [7,6],[7,9]
--              ...|.4.|..5                     <-- hence 6 not in [8,6]
--              534|89.|71. <-- 6 in [9,{6,9}]
--      A 6 must be in [1,6] or [1,9] and [9,6] or [9,9], hence [7,9] is not 6 and must be 9.
--      (we also eliminate 6 from [4,9], [7,6] and [8,6] to no great use)
--      In practice this offers little benefit over a single trial-and-error step, as
--       obviously trying either 6 in row 1 or 9 immediately pinpoints that 9 anyway.
--
--  4) swordfish (not attempted)
--      There is an extension to x-wings known as swordfish: three (or more) pairs form
--      a staggered pair (or more) of rectangles that exhibit similar properties, eg:
--              8-1|-5-|-3-
--              953|-68|---
--              -4-|-*3|5*8
--              ---+---+---
--              6--|9-2|---
--              -8-|-3-|-4-
--              3*-|5-1|-*7     <-- hence [6,3] is not 9, must be 4
--              ---+---+---
--              5*2|-*-|-8-
--              --8|37-|--9
--              -3-|82-|1--
--               ^---^---^-- 3 pairs of 9s (marked with *) on 3 rows (only)
--      It is not a swordfish if the 3 pairs are on >3 rows, I trust that is obvious.
--      Logically you can extend this to N pairs on N rows, however I cannot imagine a
--      case where this is not immediately solved by a single trial-step being invalid.
--      (eg above if you try [3,5]:=9 it is quickly proved to be invalid, and the same
--       goes for [6,8]:=9 and [7,2]:=9, since they are all entirely inter-dependent.)
--      Obviously where I have said rows, the same concept can be applied to columns.
--      Likewise there are "Alternate Pairs" and "Hook or X-Y wing" strategies, which
--      are easily solved with a single trial-and-error step, and of course the brute
--      force algorithm is going to select pairs first anyway. [Erm, no it doesn't,
--      it selects shortest - I've noted the possible improvement below.]
--
integer col, row
sequence c, r
sequence nine, prevsets, set
object vj
integer ch, k, idx, sx, sy, count

    if length(valid)=0 then
        -- initialise/start again from scratch
        valid = repeat("123456789",9*9)
    end if
    --
    -- First perform standard eliminations of any known cells:
    --  (repeated every time so plain_logic() does not have to worry about it)
    --
    for i=1 to 9*9 do
        ch = board[i]
        if ch>'0'
        and string(valid[i]) then
            valid[i] = ch
            valid = eliminate_in(valid,nines[cols[i]],ch)
            valid = eliminate_in(valid,nines[rows[i]],ch)
            valid = eliminate_in(valid,nines[squares[i]],ch)
        end if
    end for
    --
    -- 1) row/col elimination
    --
    for s=19 to 27 do
        c = repeat(0,9) -- 0 = none seen, 1..9 this col only, -1: >1 col
        r = repeat(0,9) -- ""                       row              row
        nine = nines[s]
        for n=1 to 9 do
            k = nine[n]
            vj = valid[k]
            if string(vj) then
                for i=1 to length(vj) do
                    ch = vj[i]-'0'
                    col = dotcol[k]
                    row = dotrow[k]
                    c[ch] = iff(find(c[ch],{0,col})!=0?col:-1)
                    r[ch] = iff(find(r[ch],{0,row})!=0?row:-1)
                end for
            end if
        end for
        for i=1 to 9 do
            ch = i+'0'
            col = c[i]
            if col>0 then
                valid = eliminate_in(valid,sixes[col],ch)
            end if
            row = r[i]
            if row>0 then
                valid = eliminate_in(valid,sixes[row],ch)
            end if
        end for
    end for
    --
    -- 2) set elimination
    --
    for i=1 to length(nines) do
        --
        --  Practical note: Meticulously counting empties to eliminate larger set sizes
        --                  would at best reduce 6642 tests to 972, not deemed worth it.
        --
        for set_size=2 to 4 do
            --if floor(count_empties(nines[i])/2)>=set_size then -- (untested)
            valid = comb(nines[i],valid,set_size)
            --end if
        end for
    end for
    --
    -- 3) x-wings
    --
    for ch='1' to '9' do
        prevsets = repeat(0,9)
        for x=1 to 9 do
            count = 0
            set = repeat(0,9)
            for y=0 to 8 do
                idx = y*9+x
                if sequence(valid[idx]) and find(ch,valid[idx]) then
                    set[y+1] = 1
                    count += 1
                end if
            end for
            if count=2 then
                k = find(set,prevsets)
                if k!=0 then
                    for y=0 to 8 do
                        if set[y+1]=1 then
                            for sx=1 to 9 do
                                if sx!=k and sx!=x then
                                    valid = eliminate_in(valid,{y*9+sx},ch)
                                end if
                            end for
                        end if
                    end for
                else
                    prevsets[x] = set
                end if
            end if
        end for
        prevsets = repeat(0,9)
        for y=0 to 8 do
            count = 0
            set = repeat(0,9)
            for x=1 to 9 do
                idx = y*9+x
                if sequence(valid[idx]) and find(ch,valid[idx]) then
                    set[x] = 1
                    count += 1
                end if
            end for
            if count=2 then
                k = find(set,prevsets)
                if k!=0 then
                    for x=1 to 9 do
                        if set[x]=1 then
                            for sy=0 to 8 do
                                if sy+1!=k and sy!=y then
                                    valid = eliminate_in(valid,{sy*9+x},ch)
                                end if
                            end for
                        end if
                    end for
                else
                    prevsets[y+1] = set
                end if
            end if
        end for
    end for
    return valid
end function

function permitted_in(string board, sequence sets, sequence valid, integer ch)
sequence set
integer pos, idx, bch
    for i=1 to 9 do
        set = nines[sets[i]]
        pos = 0
        for j=1 to 9 do
            idx = set[j]
            bch = board[idx]
            if bch>'0' then
                if bch=ch then pos = -1 exit end if
            elsif find(ch,valid[idx]) then
                if pos!=0 then pos = -1 exit end if
                pos = idx
            end if
        end for
        if pos>0 then
            board[pos] = ch
            improved = 1
        end if
    end for
    return board
end function

enum INVALID = -1, INCOMPLETE = 0, SOLVED = 1, MULTIPLE = 2, BRUTE = 3

function plain_logic(string board)
--
-- Responsible for:
--  1) cells with only one option
--  2) numbers with only one home
--
integer solved
sequence valid = {}
object vi

    while 1 do
        solved = SOLVED
        improved = 0
        valid = deep_logic(board,valid)

        -- 1) cells with only one option:
        for i=1 to length(valid) do
            vi = valid[i]
            if string(vi) then
                if length(vi)=0 then return {board,{},INVALID} end if
                if length(vi)=1 then
                    board[i] = vi[1]
                    improved = 1
                end if
            end if
            if board[i]<='0' then
                solved = INCOMPLETE
            end if
        end for
        if solved=SOLVED then return {board,{},SOLVED} end if

        -- 2) numbers with only one home
        for ch='1' to '9' do
            board = permitted_in(board,cols,valid,ch)
            board = permitted_in(board,rows,valid,ch)
            board = permitted_in(board,squares,valid,ch)
        end for
        if not improved then exit end if
    end while
    return {board,valid,solved}
end function

function validate(string board)
-- (sum9 should be sufficient - if you want, get rid of nine/nines)
integer ch, sum9
sequence nine, nines = tagset(9)

    for x=0 to 8 do                 -- columns
        sum9 = 0
        nine = repeat(0,9)
        for y=1 to 81 by 9 do
            ch = board[y+x]-'0'
            if ch<1 or ch>9 then return 0 end if
            sum9 += ch
            nine[ch] = ch
        end for
        if sum9!=45 then return 0 end if
        if nine!=nines then return 0 end if
    end for
    for y=1 to 81 by 9 do           -- rows
        sum9 = 0
        nine = repeat(0,9)
        for x=0 to 8 do
            ch = board[y+x]-'0'
            sum9 += ch
            nine[ch] = ch
        end for
        if sum9!=45 then return 0 end if
        if nine!=nines then return 0 end if
    end for
    for y=0 to 8 by 3 do            -- small squares
        for x=0 to 8 by 3 do
            sum9 = 0
            nine = repeat(0,9)
            for sy=y*9 to y*9+18 by 9 do
                for sx=x to x+2 do
                    ch = board[sy+sx+1]-'0'
                    sum9 += ch
                    nine[ch] = ch
                end for
            end for
            if sum9!=45 then return 0 end if
            if nine!=nines then return 0 end if
        end for
    end for
    return 1
end function

function solve(string board, sequence valid={})
sequence solution, solutions
integer solved
integer minopt, mindx
object vi
    {solution,valid,solved} = plain_logic(board)
    if solved=INVALID then return {{},INVALID} end if
    if solved=SOLVED then return {{solution},SOLVED} end if
    if solved=BRUTE then return {{solution},BRUTE} end if
    if solved!=INCOMPLETE then ?9/0 end if
    -- find the cell with the fewest options:
    -- (a possible improvement here would be to select the shortest
    --  with the "most pairs" set, see swordfish etc above.)
    minopt = 10
    for i=1 to 9*9 do
        vi = valid[i]
        if string(vi) then
            if length(vi)<=1 then ?9/0 end if   -- should be caught above
            if length(vi)<minopt then
                minopt = length(vi)
                mindx = i
            end if
        end if
    end for
    solutions = {}
    for i=1 to minopt do
        board[mindx] = valid[mindx][i]
        {solution,solved} = solve(board,valid)
        if solved=MULTIPLE then
            return {solution,MULTIPLE}
        elsif solved=SOLVED
           or solved=BRUTE then
            if not find(solution[1],solutions)
            and validate(solution[1]) then
                solutions = append(solutions,solution[1])
            end if
            if length(solutions)>1 then
                return {solutions,MULTIPLE}
            elsif length(solutions) then
                return {solutions,BRUTE}
            end if
        end if
    end for
    if length(solutions)=1 then
        return {solutions,BRUTE}
    end if
    return {{},INVALID}
end function

function test_one(string board)
sequence solutions
string solution, desc
integer solved
    {solutions,solved} = solve(board)
    if solved=SOLVED then
        desc = "(logic)"
    elsif solved=BRUTE then
        desc = "(brute force)"
    else
        desc = "???" -- INVALID/INCOMPLETE/MULTIPLE
    end if
    if length(solutions)=0 then
        solution = board
        desc = "*** NO SOLUTIONS ***"
    elsif length(solutions)=1 then
        solution = solutions[1]
        if not validate(solution) then
            desc = "*** ERROR ***"  -- (should never happen)
        end if
    else
        solution = board
        desc = "*** MULTIPLE SOLUTIONS ***"
    end if
    return {solution,desc}
end function

--NB Blank cells can be represented by any character <'1'. Spaces are not recommended since
--   they can all too easily be converted to tabs by copy/paste/save. In particular, ? and
--   _ are NOT valid characters for representing a blank square. Use any of .0-* instead.

constant tests = {
    "..............3.85..1.2.......5.7.....4...1...9.......5......73..2.1........4...9",    -- (0.01s, (logic))
    -- row/col elimination (was 8s w/o logic first)
    "000000036840000000000000020000203000010000700000600400000410050003000200600000000",    -- (0.04s, (brute force))
    ".......39.....1..5..3.5.8....8.9...6.7...2...1..4.......9.8..5..2....6..4..7.....",    -- (1.12s, (brute force))
    "000037600000600090008000004090000001600000009300000040700000800010009000002540000",    -- (0.00s, (logic))
    "....839..1......3...4....7..42.3....6.......4....7..1..2........8...92.....25...6",    -- (0.04s, (brute force))
    "..1..5.7.92.6.......8...6...9..2.4.1.........3.4.8..9...7...3.......7.69.1.8..7..",    -- (0.00s, (logic))
    -- (the following takes ~8s when checking for multiple solutions)
    "--3------4---8--36--8---1---4--6--73---9----------2--5--4-7--686--------7--6--5--",    -- (0.01s, (brute force))
    "..3.2.6..9..3.5..1..18.64....81.29..7.......8..67.82....26.95..8..2.3..9..5.1.3..",    -- (0.00s, (logic))
    "--4-5--6--6-1--8-93----7----8----5-----4-3-----6----7----2----61-5--4-3--2--7-1--",    -- (0.00s, (logic))
    -- x-wings
    ".4398.25.6..425...2....1.949....4.7.3..6.8...41.2.9..382.5.........4...553489.71.",    -- (0.00s, (logic))
    ".9...4..7.....79..8........4.58.....3.......2.....97.6........4..35.....2..6...8.",    -- (0.00s, (logic))
    -- "AL Escargot", so-called "hardest sudoku"
    "1....7.9..3..2...8..96..5....53..9...1..8...26....4...3......1..4......7..7...3..",    -- (0.26s, (brute force))
    "12.3....435....1....4........54..2..6...7.........8.9...31..5.......9.7.....6...8",    -- (0.48s, (brute force))
    "12.4..3..3...1..5...6...1..7...9.....4.6.3.....3..2...5...8.7....7.....5.......98",    -- (1.07s, (brute force))
    "394..267....3..4..5..69..2..45...9..6.......7..7...58..1..67..8..9..8....264..735",    -- (0.00s, (logic))
    "4......6.5...8.9..3....1....2.7....1.9.....4.8....3.5....2....7..6.5...8.1......6",    -- (0.01s, (brute force))
    "5...7....6..195....98....6.8...6...34..8.3..17...2...6.6....28....419..5....8..79",    -- (0.00s, (logic))
    "503600009010002600900000080000700005006804100200003000030000008004300050800006702",    -- (0.00s, (logic))
    "53..247....2...8..1..7.39.2..8.72.49.2.98..7.79.....8.....3.5.696..1.3...5.69..1.",    -- (0.00s, (logic))
    "530070000600195000098000060800060003400803001700020006060000280000419005000080079",    -- (0.00s, (logic))
    -- set exclusion
    "75..9..46961...3524.....79.2..6.1..7.8.....2.1..328.65.........3.9...2.484..3..79",    -- (0.00s, (logic))
    -- Worlds hardest sudoku:
    "800000000003600000070090200050007000000045700000100030001000068008500010090000400",    -- (0.21s, (brute force))
    "819--5-----2---75--371-4-6-4--59-1--7--3-8--2--3-62--7-5-7-921--64---9-----2--438",    -- (0.00s, (logic))
    "85...24..72......9..4.........1.7..23.5...9...4...........8..7..17..........36.4.",    -- (0.01s, (logic))
    "9..2..5...4..6..3...3.....6...9..2......5..8...7..4..37.....1...5..2..4...1..6..9",    -- (0.17s, (brute force))
    "97.3...6..6.75.........8.5.......67.....3.....539..2..7...25.....2.1...8.4...73..",    -- (0.00s, (logic))
    -- "the beast" (an earlier algorithm took 318s (5min 18s) on this):
    "000060080020000000001000000070000102500030000000000400004201000300700600000000050",    -- (0.03s, (brute force))
    $},

    lt = length(tests),
    run_one_test = 0

constant l = " x x x | x x x | x x x ",
         s = "-------+-------+-------",
         l3 = join({l,l,l},"\n"),
         fmt = substitute(join({l3,s,l3,s,l3},"\n"),"x","%c")&"\n"

procedure print_board(string board)
    printf(1,fmt,board)
end procedure

procedure test()
string board    -- (81 characters)
string solution, desc
atom t0 = time()
    if run_one_test then
        board = tests[run_one_test]
        print_board(board)
        {solution,desc} = test_one(board)
        if length(solution)!=0 then
            printf(1,"solution:\n")
            print_board(solution)
        end if
        printf(1,"%s, %3.2fs\n",{desc,time()-t0})
    else
        for i=1 to lt do
            atom t1 = time()
            board = tests[i]
            {solution,desc} = test_one(board)
            printf(1,"    \"%s\",    -- (%3.2fs, %s)\n",{board,time()-t1,desc})
--          printf(1,"    \"%s\",    -- (%3.2fs, %s)\n",{solution,time()-t1,desc})
        end for
        t0 = time()-t0
        printf(1,"%d puzzles solved in %3.2fs (av %3.2fs)\n",{lt,t0,t0/lt})
    end if
end procedure
test()
```

{{out}}

```txt

    "..............3.85..1.2.......5.7.....4...1...9.......5......73..2.1........4...9",    -- (0.02s, (logic))
    "000000036840000000000000020000203000010000700000600400000410050003000200600000000",    -- (0.03s, (brute force))
    ".......39.....1..5..3.5.8....8.9...6.7...2...1..4.......9.8..5..2....6..4..7.....",    -- (1.31s, (brute force))
    "000037600000600090008000004090000001600000009300000040700000800010009000002540000",    -- (0.00s, (logic))
    "....839..1......3...4....7..42.3....6.......4....7..1..2........8...92.....25...6",    -- (0.03s, (brute force))
    "..1..5.7.92.6.......8...6...9..2.4.1.........3.4.8..9...7...3.......7.69.1.8..7..",    -- (0.00s, (logic))
    "--3------4---8--36--8---1---4--6--73---9----------2--5--4-7--686--------7--6--5--",    -- (0.03s, (brute force))
    "..3.2.6..9..3.5..1..18.64....81.29..7.......8..67.82....26.95..8..2.3..9..5.1.3..",    -- (0.00s, (logic))
    "--4-5--6--6-1--8-93----7----8----5-----4-3-----6----7----2----61-5--4-3--2--7-1--",    -- (0.01s, (logic))
    ".4398.25.6..425...2....1.949....4.7.3..6.8...41.2.9..382.5.........4...553489.71.",    -- (0.00s, (logic))
    ".9...4..7.....79..8........4.58.....3.......2.....97.6........4..35.....2..6...8.",    -- (0.00s, (logic))
    "1....7.9..3..2...8..96..5....53..9...1..8...26....4...3......1..4......7..7...3..",    -- (0.26s, (brute force))
    "12.3....435....1....4........54..2..6...7.........8.9...31..5.......9.7.....6...8",    -- (0.40s, (brute force))
    "12.4..3..3...1..5...6...1..7...9.....4.6.3.....3..2...5...8.7....7.....5.......98",    -- (1.12s, (brute force))
    "394..267....3..4..5..69..2..45...9..6.......7..7...58..1..67..8..9..8....264..735",    -- (0.00s, (logic))
    "4......6.5...8.9..3....1....2.7....1.9.....4.8....3.5....2....7..6.5...8.1......6",    -- (0.03s, (brute force))
    "5...7....6..195....98....6.8...6...34..8.3..17...2...6.6....28....419..5....8..79",    -- (0.00s, (logic))
    "503600009010002600900000080000700005006804100200003000030000008004300050800006702",    -- (0.01s, (logic))
    "53..247....2...8..1..7.39.2..8.72.49.2.98..7.79.....8.....3.5.696..1.3...5.69..1.",    -- (0.00s, (logic))
    "530070000600195000098000060800060003400803001700020006060000280000419005000080079",    -- (0.00s, (logic))
    "75..9..46961...3524.....79.2..6.1..7.8.....2.1..328.65.........3.9...2.484..3..79",    -- (0.01s, (logic))
    "800000000003600000070090200050007000000045700000100030001000068008500010090000400",    -- (0.21s, (brute force))
    "819--5-----2---75--371-4-6-4--59-1--7--3-8--2--3-62--7-5-7-921--64---9-----2--438",    -- (0.00s, (logic))
    "85...24..72......9..4.........1.7..23.5...9...4...........8..7..17..........36.4.",    -- (0.01s, (logic))
    "9..2..5...4..6..3...3.....6...9..2......5..8...7..4..37.....1...5..2..4...1..6..9",    -- (0.18s, (brute force))
    "97.3...6..6.75.........8.5.......67.....3.....539..2..7...25.....2.1...8.4...73..",    -- (0.00s, (logic))
    "000060080020000000001000000070000102500030000000000400004201000300700600000000050",    -- (0.01s, (brute force))
27 puzzles solved in 3.74s (av 0.14s)

```

Running the fuller version mentioned above:

```txt

    "008002000000600040064000092017005004200000008800100730470000910080001000000900200",    -- (0.05s, *** MULTIPLE SOLUTIONS ***)
338 puzzles solved in 36.20s (av 0.11s)
--or
338 puzzles solved in 16.46s (av 0.05s) (w/o check for multiple solutions)

```

Running a single puzzle (run_one_test set non-zero) produces:

```txt

 . . . | . . . | . . .
 . . . | . . 3 | . 8 5
 . . 1 | . 2 . | . . .
-------+-------+-------
 . . . | 5 . 7 | . . .
 . . 4 | . . . | 1 . .
 . 9 . | . . . | . . .
-------+-------+-------
 5 . . | . . . | . 7 3
 . . 2 | . 1 . | . . .
 . . . | . 4 . | . . 9
solution:
 9 8 7 | 6 5 4 | 3 2 1
 2 4 6 | 1 7 3 | 9 8 5
 3 5 1 | 9 2 8 | 7 4 6
-------+-------+-------
 1 2 8 | 5 3 7 | 6 9 4
 6 3 4 | 8 9 2 | 1 5 7
 7 9 5 | 4 6 1 | 8 3 2
-------+-------+-------
 5 1 9 | 2 8 6 | 4 7 3
 4 7 2 | 3 1 9 | 5 6 8
 8 6 3 | 7 4 5 | 2 1 9
(logic), 0.02s

```



## PicoLisp


```PicoLisp
(load "lib/simul.l")

### Fields/Board ###
# val lst

(setq
   *Board (grid 9 9)
   *Fields (apply append *Board) )

# Init values to zero (empty)
(for L *Board
   (for This L
      (=: val 0) ) )

# Build lookup lists
(for (X . L) *Board
   (for (Y . This) L
      (=: lst
         (make
            (let A (* 3 (/ (dec X) 3))
               (do 3
                  (inc 'A)
                  (let B (* 3 (/ (dec Y) 3))
                     (do 3
                        (inc 'B)
                        (unless (and (= A X) (= B Y))
                           (link
                              (prop (get *Board A B) 'val) ) ) ) ) ) )
            (for Dir '(`west `east `south `north)
               (for (This (Dir This)  This  (Dir This))
                  (unless (memq (:: val) (made))
                     (link (:: val)) ) ) ) ) ) ) )

# Cut connections (for display only)
(for (X . L) *Board
   (for (Y . This) L
      (when (member X (3 6))
         (con (car (val This))) )
      (when (member Y (4 7))
         (set (cdr (val This))) ) ) )

# Display board
(de display ()
   (disp *Board 0
      '((This)
         (if (=0 (: val))
            "   "
            (pack " " (: val) " ") ) ) ) )

# Initialize board
(de main (Lst)
   (for (Y . L) Lst
      (for (X . N) L
         (put *Board X (- 10 Y) 'val N) ) )
   (display) )

# Find solution
(de go ()
   (unless
      (recur (*Fields)
         (with (car *Fields)
            (if (=0 (: val))
               (loop
                  (NIL
                     (or
                        (assoc (inc (:: val)) (: lst))
                        (recurse (cdr *Fields)) ) )
                  (T (= 9 (: val)) (=: val 0)) )
               (recurse (cdr *Fields)) ) ) )
      (display) ) )

(main
   (quote
      (5 3 0 0 7 0 0 0 0)
      (6 0 0 1 9 5 0 0 0)
      (0 9 8 0 0 0 0 6 0)
      (8 0 0 0 6 0 0 0 3)
      (4 0 0 8 0 3 0 0 1)
      (7 0 0 0 2 0 0 0 6)
      (0 6 0 0 0 0 2 8 0)
      (0 0 0 4 1 9 0 0 5)
      (0 0 0 0 8 0 0 7 9) ) )
```

{{out}}

```txt
   +---+---+---+---+---+---+---+---+---+
 9 | 5   3     |     7     |           |
   +   +   +   +   +   +   +   +   +   +
 8 | 6         | 1   9   5 |           |
   +   +   +   +   +   +   +   +   +   +
 7 |     9   8 |           |     6     |
   +---+---+---+---+---+---+---+---+---+
 6 | 8         |     6     |         3 |
   +   +   +   +   +   +   +   +   +   +
 5 | 4         | 8       3 |         1 |
   +   +   +   +   +   +   +   +   +   +
 4 | 7         |     2     |         6 |
   +---+---+---+---+---+---+---+---+---+
 3 |     6     |           | 2   8     |
   +   +   +   +   +   +   +   +   +   +
 2 |           | 4   1   9 |         5 |
   +   +   +   +   +   +   +   +   +   +
 1 |           |     8     |     7   9 |
   +---+---+---+---+---+---+---+---+---+
     a   b   c   d   e   f   g   h   i
```


```PicoLisp
(go)
```

{{out}}

```txt
   +---+---+---+---+---+---+---+---+---+
 9 | 5   3   4 | 6   7   8 | 9   1   2 |
   +   +   +   +   +   +   +   +   +   +
 8 | 6   7   2 | 1   9   5 | 3   4   8 |
   +   +   +   +   +   +   +   +   +   +
 7 | 1   9   8 | 3   4   2 | 5   6   7 |
   +---+---+---+---+---+---+---+---+---+
 6 | 8   5   9 | 7   6   1 | 4   2   3 |
   +   +   +   +   +   +   +   +   +   +
 5 | 4   2   6 | 8   5   3 | 7   9   1 |
   +   +   +   +   +   +   +   +   +   +
 4 | 7   1   3 | 9   2   4 | 8   5   6 |
   +---+---+---+---+---+---+---+---+---+
 3 | 9   6   1 | 5   3   7 | 2   8   4 |
   +   +   +   +   +   +   +   +   +   +
 2 | 2   8   7 | 4   1   9 | 6   3   5 |
   +   +   +   +   +   +   +   +   +   +
 1 | 3   4   5 | 2   8   6 | 1   7   9 |
   +---+---+---+---+---+---+---+---+---+
     a   b   c   d   e   f   g   h   i
```



## PL/I

Working PL/I version, derived from the Rosetta Fortran version.

```pli
sudoku: procedure options (main); /* 27 July 2014 */

  declare grid (9,9) fixed (1) static initial (
      0, 0, 3, 0, 2, 0, 6, 0, 0,
      9, 0, 0, 3, 0, 5, 0, 0, 1,
      0, 0, 1, 8, 0, 6, 4, 0, 0,
      0, 0, 8, 1, 0, 2, 9, 0, 0,
      7, 0, 0, 0, 0, 0, 0, 0, 8,
      0, 0, 6, 7, 0, 8, 2, 0, 0,
      0, 0, 2, 6, 0, 9, 5, 0, 0,
      8, 0, 0, 2, 0, 3, 0, 0, 9,
      0, 0, 5, 0, 1, 0, 3, 0, 0 );

  declare grid_solved (9,9) fixed (1);

  call print_sudoku (grid);
  call solve (1, 1);
  put skip (2);
  call print_sudoku (grid_solved);

solve: procedure (i, j) recursive options (reorder);
    declare (i, j) fixed binary;
    declare (n, n_tmp) fixed binary;

    if i > 9 then
      grid_solved = grid;
    else
      do n = 1 to 9;
        if is_safe (i, j, n) then
          do;
             n_tmp = grid (i, j);
             grid (i, j) = n;
             if j = 9 then
               call solve (i + 1, 1);
             else
               call solve (i, j + 1);
             grid (i, j) = n_tmp;
          end;
      end;

  end solve;

is_safe: procedure (i, j, n) returns (bit(1) aligned) options (reorder);
    declare (i, j, n) fixed binary;
    declare (true value ('1'b), false value ('0'b) ) bit (1);
    declare (i_min, j_min, ii, jj) fixed binary;
    declare kk bit(1) aligned;

    if grid (i, j)  = n      then return (true);
    if grid (i, j) ^= 0      then return (false);
    if any (grid (i, *) = n) then return (false);
    if any (grid (*, j) = n) then return (false);

    /* i_min and j_min are the co-ordinates of the top left-hand corner */
    /* of 3 x 3 grid in which element (i,j) exists.                     */
    i_min = 1 + 3 * trunc((i - 1) / 3);
    j_min = 1 + 3 * trunc((j - 1) / 3);

    begin;
       declare sub_grid(3,3) fixed (1) defined grid(1sub+i_min-1,2sub+j_min-1);

       kk = true;
       if any(sub_grid = n) then kk = false;
    end;
    return (kk);
  end is_safe;

print_sudoku: procedure (grid);
    declare grid (*,*) fixed (1);
    declare ( i, j, ii) fixed binary;
    declare bar character (19) initial ( '+-----+-----+-----+' );
    declare frame (9) character (1) initial (' ', ' ', '|', ' ', ' ', '|', ' ', ' ', '|' );

    put skip list (bar);
    do i = 1 to 7 by 3;
       do ii = i to i + 2;
          put skip edit ( '|', (grid (ii, j), frame(j) do j = 1 to 9) ) (a, f(1));
       end;
       put skip list (bar);
    end;
  end print_sudoku;

end sudoku;

```

{{out}}

```txt

+-----+-----+-----+
|0 0 3|0 2 0|6 0 0|
|9 0 0|3 0 5|0 0 1|
|0 0 1|8 0 6|4 0 0|
+-----+-----+-----+
|0 0 8|1 0 2|9 0 0|
|7 0 0|0 0 0|0 0 8|
|0 0 6|7 0 8|2 0 0|
+-----+-----+-----+
|0 0 2|6 0 9|5 0 0|
|8 0 0|2 0 3|0 0 9|
|0 0 5|0 1 0|3 0 0|
+-----+-----+-----+


+-----+-----+-----+
|4 8 3|9 2 1|6 5 7|
|9 6 7|3 4 5|8 2 1|
|2 5 1|8 7 6|4 9 3|
+-----+-----+-----+
|5 4 8|1 3 2|9 7 6|
|7 2 9|5 6 4|1 3 8|
|1 3 6|7 9 8|2 4 5|
+-----+-----+-----+
|3 7 2|6 8 9|5 1 4|
|8 1 4|2 5 3|7 6 9|
|6 9 5|4 1 7|3 8 2|
+-----+-----+-----+

```


Another PL/I version, reads sudoku from the text data file as 81 character record.

```pli

*PROCESS MARGINS(1,120) LIBS(SINGLE,STATIC);
*PROCESS OPTIMIZE(2) DFT(REORDER);


 sudoku: proc(parms) options(main);
   dcl parms char (100) var;

   define alias bits bit (9) aligned;
   dcl total (81) type bits;
   dcl matrix (9, 9) type bits based(addr(total));
   dcl box (9, 3, 3) type bits defined (total(trunc((1sub-1) /3) * 27 + mod(1sub-1, 3) * 3 + (2sub-1) * 9 + 3sub));

   dcl posbit (0:9) type bits init('000000000'b, '100000000'b, '010000000'b, '001000000'b,
                                   '000100000'b, '000010000'b, '000001000'b, '000000100'b,
                                   '000000010'b, '000000001'b);

   dcl (i, j, k) fixed bin(31);
   dcl (start, finish) float(18);
   dcl result fixed dec(5,3);

   dcl buffer char(81);
   dcl in file;

   /* ON UNIT for the Sudoku data conversion */
   on conversion
     begin;
       put skip
         list('Sudoku data not valid.');
       stop;
     end;

   /* ON UNIT to display info about the usage */
   on undefinedfile(in)
     begin;
       put skip
         list('Usage: ' || procedurename() || ' /filename');
       stop;
     end;

   open file(in)
     title ('/'||parms||',type(fixed), recsize(81)') record input;

   /* Ignore the endfile condition */
   on endfile(in);

   /* Read the Sudoku data into buffer as one record */
   read file(in) into(buffer);
   close file(in);

   /* Convert numbers -> position bit presentation and assign into the Sudoku board */
   do k = 1 to 81;
     total(k) = posbit(substr(buffer, k, 1));
   end;

   /* Start solving the Sudoku */
   start = secs();
   if solve() then
     do;
       finish = secs();
       result = finish - start + 0.0005;
       put skip list('Sudoku solved! Time: ' || trim(result) || ' seconds');
       put skip(2);

   /* display the solved Sudoku if solution exist */
       do i = 1 to 9;
         do j = 1 to 9;
           put edit(trim(index(matrix(i, j), '1'b))) (a(3));
         end;
         put skip(2);
       end;
     end;
   else put skip list('Impossible!');


   /*************************************/
   /* Simple backtracking sudoku solver */
   /*************************************/
   solve: proc recursive returns(bit(1));
     dcl (i, j, k) fixed bin(31);
     dcl result type bits;

     /* find free cell */
     do i = 1 to 9;
       do j = 1 to 9;
         if matrix(i, j) = posbit(0) then goto skip;
       end;
     end;

     /* No more free cells. Check if the completed Sudoku is valid.      */
     /* Number in the cell is valid if the matching position bit is set. */
     do i = 1 to 9;
       do j = 1 to 9;
       k = index(matrix(i, j), '1'b);
       matrix(i, j) = posbit(0);
       result = ^(any(matrix(i, *)) | any(matrix(*, j)) | any(box(numbox(i, j), *, *)));
       if substr(result, k, 1) = '0'b then return('0'b);
       matrix(i, j) = posbit(k);
       end;
     end;

     return('1'b);
    skip:

     /* Go through and test possible values for the free cell untill the Sudoku is completed */
     result = ^(any(matrix(i, *)) | any(matrix(*, j)) | any(box(numbox(i, j), *, *)));
     k = 0;
     do forever;
       k = search(result, '1'b, k+1);
       if k = 0 then leave;
       matrix(i, j) = posbit(k);
       if solve() then return('1'b);
       else matrix(i, j) = posbit(0);
     end;

     return('0'b);
   end solve;


   /********************************************/
   /* Returns box number for the sudoku coords */
   /********************************************/
   numbox: proc(i, j) returns(fixed bin(31));
     dcl (i, j) fixed bin(31);

     dcl lookup (9, 9) fixed bin(31) static init( (3)1, (3)2, (3)3,
                                                  (3)1, (3)2, (3)3,
                                                  (3)1, (3)2, (3)3,
                                                  (3)4, (3)5, (3)6,
                                                  (3)4, (3)5, (3)6,
                                                  (3)4, (3)5, (3)6,
                                                  (3)7, (3)8, (3)9,
                                                  (3)7, (3)8, (3)9,
                                                  (3)7, (3)8, (3)9 );

     return(lookup(i, j));
   end numbox;

 end sudoku;

```



## Prolog


```Prolog
:- use_module(library(clpfd)).

sudoku(Rows) :-
        length(Rows, 9), maplist(length_(9), Rows),
        append(Rows, Vs), Vs ins 1..9,
        maplist(all_distinct, Rows),
        transpose(Rows, Columns), maplist(all_distinct, Columns),
        Rows = [A,B,C,D,E,F,G,H,I],
        blocks(A, B, C), blocks(D, E, F), blocks(G, H, I).

length_(L, Ls) :- length(Ls, L).

blocks([], [], []).
blocks([A,B,C|Bs1], [D,E,F|Bs2], [G,H,I|Bs3]) :-
        all_distinct([A,B,C,D,E,F,G,H,I]),
        blocks(Bs1, Bs2, Bs3).

problem(1, [[_,_,_,_,_,_,_,_,_],
            [_,_,_,_,_,3,_,8,5],
            [_,_,1,_,2,_,_,_,_],
            [_,_,_,5,_,7,_,_,_],
            [_,_,4,_,_,_,1,_,_],
            [_,9,_,_,_,_,_,_,_],
            [5,_,_,_,_,_,_,7,3],
            [_,_,2,_,1,_,_,_,_],
            [_,_,_,_,4,_,_,_,9]]).
```



### GNU Prolog version

{{works with|GNU Prolog|1.4.4}}

```Prolog
:- initialization(main).


solve(Rows) :-
    maplist(domain_1_9, Rows)
  , different(Rows)
  , transpose(Rows,Cols), different(Cols)
  , blocks(Rows,Blocks) , different(Blocks)
  , maplist(fd_labeling, Rows)
  .

domain_1_9(Rows) :- fd_domain(Rows,1,9).
different(Rows)  :- maplist(fd_all_different, Rows).

blocks(Rows,Blocks) :-
    maplist(split3,Rows,Xs), transpose(Xs,Ys)
  , concat(Ys,Zs), concat_map(split3,Zs,Blocks)
  . % where
    split3([X,Y,Z|L],[[X,Y,Z]|R]) :- split3(L,R).
    split3([],[]).


% utils/list
concat_map(F,Xs,Ys) :- call(F,Xs,Zs), maplist(concat,Zs,Ys).

concat([],[]).
concat([X|Xs],Ys) :- append(X,Zs,Ys), concat(Xs,Zs).

transpose([],[]).
transpose([[X]|Col], [[X|Row]]) :- transpose(Col,[Row]).
transpose([[X|Row]], [[X]|Col]) :- transpose([Row],Col).
transpose([[X|Row]|Xs], [[X|Col]|Ys]) :-
    maplist(bind_head, Row, Ys, YX)
  , maplist(bind_head, Col, Xs, XY)
  , transpose(XY,YX)
  . % where
    bind_head(H,[H|T],T).
    bind_head([],[],[]).


% tests
test([ [_,_,3,_,_,_,_,_,_]
     , [4,_,_,_,8,_,_,3,6]
     , [_,_,8,_,_,_,1,_,_]
     , [_,4,_,_,6,_,_,7,3]
     , [_,_,_,9,_,_,_,_,_]
     , [_,_,_,_,_,2,_,_,5]
     , [_,_,4,_,7,_,_,6,8]
     , [6,_,_,_,_,_,_,_,_]
     , [7,_,_,6,_,_,5,_,_]
     ]).

main :- test(T), solve(T), maplist(show,T), halt.
show(X) :- write(X), nl.
```

{{Out}}

```txt
[1,2,3,4,5,6,7,8,9]
[4,5,7,1,8,9,2,3,6]
[9,6,8,3,2,7,1,5,4]
[2,4,9,5,6,1,8,7,3]
[5,7,6,9,3,8,4,1,2]
[8,3,1,7,4,2,6,9,5]
[3,1,4,2,7,5,9,6,8]
[6,9,5,8,1,4,3,2,7]
[7,8,2,6,9,3,5,4,1]
```

[http://ideone.com/OCstmf Runs in: time: 0.02 memory: 68352 (adapted for gprolog 1.3.1)]


## PureBasic

A brute force method is used, it seemed the fastest as well as the simplest.

```PureBasic
DataSection
  puzzle:
  Data.s "394002670"
  Data.s "000300400"
  Data.s "500690020"
  Data.s "045000900"
  Data.s "600000007"
  Data.s "007000580"
  Data.s "010067008"
  Data.s "009008000"
  Data.s "026400735"
EndDataSection

#IsPossible = 0
#IsNotPossible = 1
#Unknown = 0
Global Dim sudoku(8, 8)
;-declarations
Declare readSudoku()
Declare displaySudoku()
Declare.s buildpossible(x, y, Array possible.b(1))
Declare solvePuzzle(x = 0, y = 0)

;-procedures
Procedure readSudoku()
  Protected a$, row, column

  Restore puzzle
  For row = 0 To 8
    Read.s a$
    For column = 0 To 8
      sudoku(column, row) = Val(Mid(a$, column + 1, 1))
    Next
  Next
EndProcedure

Procedure displaySudoku()
  Protected row, column
  Static border.s = "+-----+-----+-----+"
  For row = 0 To 8
    If row % 3 = 0: PrintN(border): EndIf
    For column = 0 To 8
      If column % 3 = 0: Print("|"): Else: Print(" "): EndIf
      If sudoku(column, row): Print(Str(sudoku(column, row))): Else: Print("."): EndIf
    Next
    PrintN("|")
  Next
  PrintN(border)
EndProcedure

Procedure.s buildpossible(x, y, Array possible.b(1))
  Protected index, column, row, boxColumn = (x / 3) * 3, boxRow = (y / 3) * 3
  Dim possible.b(9)

  For index = 0 To 8
    possible(sudoku(index, y)) = #IsNotPossible ;record possibles in column
    possible(sudoku(x, index)) = #IsNotPossible ;record possibles in row
  Next

  ;record possibles in box
  For row = boxRow To boxRow + 2
    For column = boxColumn To boxColumn + 2
      possible(sudoku(column, row)) = #IsNotPossible
    Next
  Next
EndProcedure

Procedure solvePuzzle(x = 0, y = 0)
  Protected row, column, spot, digit
  Dim possible.b(9)

  For row = y To 8
    For column = x To 8
      If sudoku(column, row) = #Unknown
        buildpossible(column, row, possible())

        For digit =  1 To 9
          If possible(digit) = #IsPossible
            sudoku(column, row) = digit
            spot = row * 9 + column + 1
            If solvePuzzle(spot % 9, spot / 9)
              Break 3
            EndIf
          EndIf
        Next

        If digit = 10
          sudoku(column, row) = #Unknown
          ProcedureReturn #False
        EndIf
      EndIf
    Next
    x = 0 ;reset column start point
  Next
  ProcedureReturn #True
EndProcedure

If OpenConsole()
  readSudoku()
  displaySudoku()
  If solvePuzzle()
    PrintN("Solved.")
    displaySudoku()
  Else
    PrintN("Unable to solve puzzle") ;due to bad starting data
  EndIf

  Print(#CRLF$ + #CRLF$ + "Press ENTER to exit")
  Input()
  CloseConsole()
EndIf
```

{{out}}

```txt
+-----+-----+-----+
|3 9 4|. . 2|6 7 .|
|. . .|3 . .|4 . .|
|5 . .|6 9 .|. 2 .|
+-----+-----+-----+
|. 4 5|. . .|9 . .|
|6 . .|. . .|. . 7|
|. . 7|. . .|5 8 .|
+-----+-----+-----+
|. 1 .|. 6 7|. . 8|
|. . 9|. . 8|. . .|
|. 2 6|4 . .|7 3 5|
+-----+-----+-----+
Solved.
+-----+-----+-----+
|3 9 4|8 5 2|6 7 1|
|2 6 8|3 7 1|4 5 9|
|5 7 1|6 9 4|8 2 3|
+-----+-----+-----+
|1 4 5|7 8 3|9 6 2|
|6 8 2|9 4 5|3 1 7|
|9 3 7|1 2 6|5 8 4|
+-----+-----+-----+
|4 1 3|5 6 7|2 9 8|
|7 5 9|2 3 8|1 4 6|
|8 2 6|4 1 9|7 3 5|
+-----+-----+-----+
```



## Python

See [http://www2.warwick.ac.uk/fac/sci/moac/currentstudents/peter_cock/python/sudoku/ Solving Sudoku puzzles with Python] for GPL'd solvers of increasing complexity of algorithm.

A simple backtrack algorithm -- Quick but may take longer if the grid had been more than 9 x 9

```python

def initiate():
    box.append([0, 1, 2, 9, 10, 11, 18, 19, 20])
    box.append([3, 4, 5, 12, 13, 14, 21, 22, 23])
    box.append([6, 7, 8, 15, 16, 17, 24, 25, 26])
    box.append([27, 28, 29, 36, 37, 38, 45, 46, 47])
    box.append([30, 31, 32, 39, 40, 41, 48, 49, 50])
    box.append([33, 34, 35, 42, 43, 44, 51, 52, 53])
    box.append([54, 55, 56, 63, 64, 65, 72, 73, 74])
    box.append([57, 58, 59, 66, 67, 68, 75, 76, 77])
    box.append([60, 61, 62, 69, 70, 71, 78, 79, 80])
    for i in range(0, 81, 9):
        row.append(range(i, i+9))
    for i in range(9):
        column.append(range(i, 80+i, 9))

def valid(n, pos):
    current_row = pos/9
    current_col = pos%9
    current_box = (current_row/3)*3 + (current_col/3)
    for i in row[current_row]:
        if (grid[i] == n):
            return False
    for i in column[current_col]:
        if (grid[i] == n):
            return False
    for i in box[current_box]:
        if (grid[i] == n):
            return False
    return True

def solve():
    i = 0
    proceed = 1
    while(i < 81):
        if given[i]:
            if proceed:
                    i += 1
            else:
                i -= 1
        else:
            n = grid[i]
            prev = grid[i]
            while(n < 9):
              if (n < 9):
                  n += 1
              if valid(n, i):
                  grid[i] = n
                  proceed = 1
                  break
            if (grid[i] == prev):
               grid[i] = 0
               proceed = 0
            if proceed:
               i += 1
            else:
               i -=1

def inputs():
    nextt = 'T'
    number = 0
    pos = 0
    while(not(nextt == 'N' or nextt == 'n')):
        print "Enter the position:",
        pos = int(raw_input())
        given[pos - 1] = True
        print "Enter the numerical:",
        number = int(raw_input())
        grid[pos - 1] = number
        print "Do you want to enter another given?(Y, for yes: N, for no)"
        nextt = raw_input()


grid = [0]*81
given = [False]*81
box = []
row = []
column = []
initiate()
inputs()
solve()
for i in range(9):
    print grid[i*9:i*9+9]
raw_input()

```



## Racket

A [http://schemecookbook.org/view/Cookbook/SudokuSolver Sudoku Solver in Racket].


## Rascal

[[File:sudoku.jpeg||200px|thumb|right]]
A sudoku is represented as a matrix, see Rascal solutions to matrix related problems for examples.


```Rascal
import Prelude;
import vis::Figure;
import vis::Render;

public rel[int,int,int] sudoku(rel[int x, int y, int v] sudoku){
	annotated= annotateGrid(sudoku);
	solved = {<0,0,0,0,{0}>};

	while(!isEmpty(solved)){
		for (n <- [0 ..8]){
			column = domainR(annotated, {n});
			annotated -= column;
			annotated += reduceOptions(column);

			row = {<x,y,v,g,p> | <x,y,v,g,p> <- annotated, y==n};
			annotated -= row;
			annotated += reduceOptions(row);

			grid1 = {<x,y,v,g,p> | <x,y,v,g,p> <- annotated, g==n};
			annotated -= grid1;
			annotated += reduceOptions(grid1);
		}

		solved = {<x,y,v,g,p> | <x,y,v,g,p> <- annotated, size(p)==1};
		annotated -= solved;
		annotated += {<x,y,getOneFrom(p),g,{*[1 .. 9]}> | <x,y,v,g,p> <- solved};
	}

	result = {<x,y,v> | <x,y,v,g,p> <- annotated};
	return result;
}


//adds gridnumber and default set of options
public rel[int,int,int,int,set[int]] annotateGrid(rel[int x, int y, int v] sudoku){
	result = {};
		for (<x, y, v> <- sudoku){
			g = 0;
			if (x<3 && y<3) g = 0;
			if (2<x && x<6 && y<3) g = 1;
			if (x>5 && y<3) g = 2;

			if (x<3 && 2<y && y<6) g = 3;
			if (2<x && x<6 && 2<y && y<6) g = 4;
			if (x>5 && 2<y && y<6) g = 5;

			if (x<3 && y>5) g=6;
			if (2<x && x<6 && y>5) g=7;
			if (x>5 && y>5) g=8;

			result += <x,y,v,g,{*[1 .. 9]}>;
			}
	return result;
}

//reduces set of options
public rel[int,int,int,int,set[int]] reduceOptions(rel[int x, int y, int v, int g, set[int] p] subSudoku){
	solved = {<x,y,v,g,p> | <x,y,v,g,p> <- subSudoku, v!=0};
	numbers = {*[1 .. 9]} - {v | <x,y,v,g,p> <- solved};
	remaining = {<x,y,v,g,numbers&p> | <x,y,v,g,p> <- subSudoku-solved};
	result = remaining + solved;
	return result;
}

//a function to visualize the result
public void displaySudoku(rel[int x, int y, int v] sudoku){
	points = [box(text("<v>"), align(0.111111*(x+1),0.111111*(y+1)),shrink(0.1)) | <x,y,v> <- sudoku];
	print(points);
	render(overlay([*points], aspectRatio(1.0)));
}

//a sudoku
public rel[int, int, int] sudokuA =
{
<0,0,3>, <1,0,9>, <2,0,4>, <3,0,0>, <4,0,0>, <5,0,2>, <6,0,6>, <7,0,7>, <8,0,0>,
<0,1,0>, <1,1,0>, <2,1,0>, <3,1,3>, <4,1,0>, <5,1,0>, <6,1,4>, <7,1,0>, <8,1,0>,
<0,2,5>, <1,2,0>, <2,2,0>, <3,2,6>, <4,2,9>, <5,2,0>, <6,2,0>, <7,2,2>, <8,2,0>,
<0,3,0>, <1,3,4>, <2,3,5>, <3,3,0>, <4,3,0>, <5,3,0>, <6,3,9>, <7,3,0>, <8,3,0>,
<0,4,6>, <1,4,0>, <2,4,0>, <3,4,0>, <4,4,0>, <5,4,0>, <6,4,0>, <7,4,0>, <8,4,7>,
<0,5,0>, <1,5,0>, <2,5,7>, <3,5,0>, <4,5,0>, <5,5,0>, <6,5,5>, <7,5,8>, <8,5,0>,
<0,6,0>, <1,6,1>, <2,6,0>, <3,6,0>, <4,6,6>, <5,6,7>, <6,6,0>, <7,6,0>, <8,6,8>,
<0,7,0>, <1,7,0>, <2,7,9>, <3,7,0>, <4,7,0>, <5,7,8>, <6,7,0>, <7,7,0>, <8,7,0>,
<0,8,0>, <1,8,2>, <2,8,6>, <3,8,4>, <4,8,0>, <5,8,0>, <6,8,7>, <7,8,3>, <8,8,5>
};
```


Example

```txt
rascal>displaySudoku(sudoku(sudokuA))

See picture
```



## REXX

The   '''SUDOKU'''   REXX programs (and output) are included here   ──►   [[Sudoku/REXX]].




=={{header|RPN (HP-15c)}}==

This is a back-tracking solver written in RPN for the HP-15C calculator. It is highly optimized for size, rather than speed, as the target platform only has 448 bytes of memory for code and data combined.

Latest version and usage notes kept at: [[http://neural-noise.blogspot.com/2013/01/a-sudoku-solver-for-hp-15c.html Sudoku Solver for the HP 15-C]]


```txt

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Register And Flag Usage
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;        0        General purpose variable used for miscelaneous purposes
;        1        Current index (0-80) in the pseudo-recursion
;        2        Row (0-8) of current index
;        3        Column (0-8) of current index
;        4        Block # (0-8) of current index
;        5        Power of 10 of current column index
;        6        Value in the test solution at current index
;        7        Value of start clue at current index (0 if not set)
;        8 – 16   Starting row data
;        17 – 25  Current test solution
;        26 – 34  Flag matrix (bit set if digit used in a row/column/block)
;
;        Flag 2   Indicates that a digit has been used in cur row/column/block
;        Flag 3   Input to Subroutine B (whether to set or clear flags)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; setU(x)
; Set/clear flag matrix values (show that x is used in a row/column/block)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
LBL D
        GSB 5       ; calc bit value we need to set/clear in existing row
        RCL 2       ; Get the current row index into x
        GSB B       ; set flag matrix value and calc new bit value for the column
        RCL 3       ; Get the current column index into x
        GSB B       ; set flag matrix values and calc new bit value for the block
        RCL 4       ; Get the current block index into x

; MUST IMMEDIATELY FOLLOW PRECEEDING SUBROUTINE
; utility subroutine for setting flag matrix values

LBL B
        GSB 1       ; get the current flag matrix row at index x

        RCL 0       ; get temp register (holds the bit value we will be setting)
        F? 3        ; flag 3 indicates if we are setting or clearing the flag
        CHS         ; if we are clearing, we will do a subtraction instead
        +           ; set/clear the flag

        X<>Y        ; bring the row index back into x
        2           ; 26 is the starting register for the flag matrix
        6
        GSB 3       ; set I so that we are ready to store the new value
        STO (i)     ; store the new value into the flag matrix
        RDN         ; get rid of the new value to restore the stack
        9           ; the next bit value will be 9 bits to the left
        +           ; set the next bit index
        GTO 5       ; calculate the value with that bit set
                    ; we GTO instead of GSB and it will do the RTN

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; putA(x)
; Set the value x into the current row/column in the trial solution.
; Does it by subtracting the previous value and adding the new one.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
LBL 7
        X<>6        ; swap new value with register that holds current value
        STO 0       ; store the old value in the temp register
        RCL 2       ; Get the current row index into x
        1           ; 17 is the starting register for the current trial solution
        7
        GSB 3       ; Set the indirect register
        RCL (i)     ; Get the current value for the entire row
        RCL 6       ; Get the new value
        RCL- 0      ; subtract the old value from the new value
        RCL* 5      ; shift the power of 10 to the appropriate column
        +           ; add to the old value
        STO (i)     ; store the new row value from where we got it
        RTN

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; change(x)
; Increments or decrements the current position in the trial solution.
; Updates the registers containing the current row, column and block index,
; and the one with the power of 10 factor for the current column and others
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
LBL 6
        STO+ 1      ; x holds +1 or -1; Register 1 is the current index

        RCL 1       ; get the current index (0 to 80)
        RCL 1       ; get the current index (0 to 80)
        9           ; integer divide by 9 to get the row index (0 to 8)
        /           ; no integer divide on 15c so do a floating point divide
        INT         ; use the INT operator to finish of the integer divide
        STO 2       ; register 2 contains the current row index

        9
        *
        -           ; col = index - 9 * row
        STO 3       ; register 3 contains the current column index

        3           ; calculate the block index from the row & column indexes
        /           ; TODO: save a couple of bytes in this section of code
        RCL 2
        3
        /
        INT
        3
        *
        +
        STO 4       ; register 4 holds the block index

        8           ; now calculate the power of 10 of the current column
        RCL- 3      ; Get the digit (from right) based on the column
        10^X        ; calculate the exponent
        STO 5       ; save in register 5 which is used throughout the code

        RCL 2       ; get the current row
        1           ; 17 is the start register of the current trial solution
        7
        GSB 4       ; extract the value at the current column
        STO 6       ; reg 6: the current trial value at the current row/column

        RCL 2       ; get the current row
        8           ; 8 is the start register of the input data from the user
        GSB 4       ; extract the value at the current column
        STO 7       ; reg 7: starting value at the current row/column (0 if none)
        RTN

; Extract value at the current column from the matrix indirectly specified by x&y
LBL 4
        GSB 3       ; set the indirect register based on x & y
        RCL (i)     ; get the row from the matrix passed in
        RCL / 5     ; shift the row to the right
        INT         ; trim off the digits shifted to the right of the decimal
        1           ; we will do a modulus 10 to extract the last digit
        0
        /           ; do the equivalent of a mod 10
        FRAC
        1
        0
        *
        RTN

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;  main()
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
LBL A
        CF 2        ; make sure flag 2 is unset - CLR REG does not do this
        CF 3        ; make sure flag 3 is unset - CLR REG does not do this
        1           ; start with a index in register 1 of -1 (0 to 80)
        CHS         ; that way we can start with an increment operation
        STO 1       ; and actually start at 0 where we want.

LBL 2               ; set the flags to show the input values are set
        1           ; go forward one position at a time
        GSB 6       ; go to the next position in the trial solution

        RCL 7       ; get the starting input value at this row/col
        GSB 7       ; set the value in the trial solution
        RCL 7       ; get starting input value because the last call destroyed it
        TEST 1      ; if > 0 then the user input a value for this row/col
        GSB D       ; set the flags to indicate this value is set

        8           ; 80 is the upper bound of the indexes (9x9 = 80 = 0:80)
        0
        RCL 1       ; get the current index
        TEST 6      ; if the current index hasn't reached 80
        GTO 2       ; do the next value
        1           ; reset the starting value
        CHS         ; to -1 as we did at the beginning of the program
        STO 1       ; register 1 holds the current index

LBL E               ; main solution loop
        8           ; when we reach the last index (80) we are done
        0
        RCL 1       ; register 1 holds the current index
        TEST 5      ; see if we are at the end
        RTN         ;  finished        ; woohoo - we are done!
        1           ; Go forward one spot
        GSB 6       ; Do the position increment
        RCL 7       ; get the starting input value at this row/col
        TEST 1      ; if it's > 0, the user specified a value here
        GTO E       ; go forward, since this value was specified by the user
        GSB 7       ; Set the value in the trial solution

LBL 8
        9           ; check the possible digits in order 1-9.
        RCL 6       ; Get the current trial solution value
        TEST 5      ; Check to see if it is 9
        GTO C       ; If it is, backup one step
        1           ; We weren't at 9 yet, so increment the value by 1
        +
        GSB 7       ; Set the value in the trial solution

        RCL 6       ; Get the current trial solution value
        GSB 5       ; Calc 2^x-1 to get the bit mask
        CF 2        ; Clear the flag thats used as a return value
        RCL 2       ; Get the current row index into x
        GSB 9       ; see if the current value has already been used in the row
        F? 2        ; If number has been used in the block, try the next value
        GTO 8
        RCL 3       ; Get the current column index into x
        GSB 9       ; see if current value has already been used in the column
        F? 2        ; If number has been used in the block, try the next value
        GTO 8
        RCL 4       ; Get the current block index into x
        GSB 9       ; see if the current value has already been used in the block
        F? 2        ; If number has been used in the block, try the next value
        GTO 8
        RCL 6       ; Get the current trial solution value
        GSB D       ; set the flags to indicate this value is set
        GTO E       ; move on to the next position in the puzzle

LBL C               ; Come here to back up to the previous position
        1           ; We will go one spot backwards
        CHS
        GSB 6       ; Set the new current position and all temp values
        TEST 1      ; previous call leaves the starting value in X
        GTO C       ; if value is > 0, it was set, backup one more spot
        RCL 6       ; Get the current trial solution value

        SF 3        ; flag 3: clear the flag matrix bits, instead of setting them
        GSB D       ; Set/Clear the flag matrix bits
        CF 3        ; unset the 3 flag
        GTO 8       ; check the next digit

LBL 9
        GSB 1       ; get the appropriate row (x) from the flag matrix
        RCL /  0    ; divide by the temp register - right shifts value
        INT
        2           ; if bit is set, fractional part will be non 0 when / 2
        /
        FRAC
        TEST 1      ; if bit is set, set flag 2 which is used as a return value
        SF 2
        RDN         ; move the stack down to prepare the caller for the next call
        RDN         ; move the stack down to prepare the caller for the next call
        9           ; bit flags for row/col/block are << by 9 from each other
        +           ; calculates the appropriate bit offset for the next call
        GTO 5       ; calc 2^x-1 to get the bit mask
                    ; do a GTO instead of GSB and it will return for us

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; setPow2(x)
; Sets the utility temp register to 2^(x-1). Leaves x in place.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
LBL 5
        STO 0       ; store the input X in the temp register
        1           ; we want to subtract 1 from the exponent
        -           ; calculate x-1
        2           ; set the base as 2
        X<>Y        ; the y^x function wants x and y reversed
        y^x         ; calculate the value
        X<>0        ; stuff result in temp register and restore the input x
        RTN

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; getPart(x)
; Returns the integer representing the entire Xth row of the flag matrix
; Row numbers start at 0.
; returns value in x - input parameter x ends up in y
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
LBL 1
        ENTER
        ENTER       ; duplicate the parameter so we can leave it for the caller
        2           ; 26 is the starting register for the flag matrix
        6
        GSB 3       ; set the indirect register to the row specified by x
        RCL (i)     ; retrieve the entire row from the flag matrix
        RTN

; Set the indirect register and remove the parameters from the stack
LBL 3
        +           ; x+y is the memory offset we want
        STO I       ; put it in the indirect register
        RDN         ; get rid of the sum from the stack
        RTN

```



## Ruby

Example of a back-tracking solver, from [[wp:Algorithmics of sudoku]]
{{works with|Ruby|2.0+}}

```ruby
def read_matrix(data)
  lines = data.lines
  9.times.collect { |i| 9.times.collect { |j| lines[i][j].to_i } }
end

def permissible(matrix, i, j)
  ok = [nil, *1..9]
  check = ->(x,y) { ok[matrix[x][y]] = nil  if matrix[x][y].nonzero? }
  # Same as another in the column isn't permissible...
  9.times { |x| check[x, j] }
  # Same as another in the row isn't permissible...
  9.times { |y| check[i, y] }
  # Same as another in the 3x3 block isn't permissible...
  xary = [ *(x = (i / 3) * 3) .. x + 2 ]        #=> [0,1,2], [3,4,5] or [6,7,8]
  yary = [ *(y = (j / 3) * 3) .. y + 2 ]
  xary.product(yary).each { |x, y| check[x, y] }
  # Gathering only permitted one
  ok.compact
end

def deep_copy_sudoku(matrix)
  matrix.collect { |row| row.dup }
end

def solve_sudoku(matrix)
  loop do
    options = []
    9.times do |i|
      9.times do |j|
        next if matrix[i][j].nonzero?
        p = permissible(matrix, i, j)
        # If nothing is permissible, there is no solution at this level.
        return if p.empty?              # return nil
        options << [i, j, p]
      end
    end
    # If the matrix is complete, we have a solution...
    return matrix if options.empty?

    i, j, permissible = options.min_by { |x| x.last.length }

    # If there is an option with only one solution, set it and re-check permissibility
    if permissible.length == 1
      matrix[i][j] = permissible[0]
      next
    end

    # We have two or more choices. We need to search both...
    permissible.each do |v|
      mtmp = deep_copy_sudoku(matrix)
      mtmp[i][j] = v
      ret = solve_sudoku(mtmp)
      return ret if ret
    end

    # We did an exhaustive search on this branch and nothing worked out.
    return
  end
end

def print_matrix(matrix)
  puts "Impossible" or return  unless matrix

  border = "+-----+-----+-----+"
  9.times do |i|
    puts border if i%3 == 0
    9.times do |j|
      print j%3 == 0 ? "|" : " "
      print matrix[i][j] == 0 ? "." : matrix[i][j]
    end
    puts "|"
  end
  puts border
end

data = <<EOS
394__267_
___3__4__
5__69__2_
_45___9__
6_______7
__7___58_
_1__67__8
__9__8___
_264__735
EOS

matrix = read_matrix(data)
print_matrix(matrix)
puts
print_matrix(solve_sudoku(matrix))
```


{{out}}

```txt

+-----+-----+-----+
|3 9 4|. . 2|6 7 .|
|. . .|3 . .|4 . .|
|5 . .|6 9 .|. 2 .|
+-----+-----+-----+
|. 4 5|. . .|9 . .|
|6 . .|. . .|. . 7|
|. . 7|. . .|5 8 .|
+-----+-----+-----+
|. 1 .|. 6 7|. . 8|
|. . 9|. . 8|. . .|
|. 2 6|4 . .|7 3 5|
+-----+-----+-----+

+-----+-----+-----+
|3 9 4|8 5 2|6 7 1|
|2 6 8|3 7 1|4 5 9|
|5 7 1|6 9 4|8 2 3|
+-----+-----+-----+
|1 4 5|7 8 3|9 6 2|
|6 8 2|9 4 5|3 1 7|
|9 3 7|1 2 6|5 8 4|
+-----+-----+-----+
|4 1 3|5 6 7|2 9 8|
|7 5 9|2 3 8|1 4 6|
|8 2 6|4 1 9|7 3 5|
+-----+-----+-----+
```



## Rust

{{trans|Ada}}

```rust
type Sudoku = [u8; 81];

fn is_valid(val: u8, x: usize, y: usize, sudoku_ar: &mut Sudoku) -> bool {
    (0..9).all(|i| sudoku_ar[y * 9 + i] != val && sudoku_ar[i * 9 + x] != val) && {
        let (start_x, start_y) = ((x / 3) * 3, (y / 3) * 3);
        (start_y..start_y + 3).all(|i| (start_x..start_x + 3).all(|j| sudoku_ar[i * 9 + j] != val))
    }
}

fn place_number(pos: usize, sudoku_ar: &mut Sudoku) -> bool {
    (pos..81).find(|&p| sudoku_ar[p] == 0).map_or(true, |pos| {
        let (x, y) = (pos % 9, pos / 9);
        for n in 1..10 {
            if is_valid(n, x, y, sudoku_ar) {
                sudoku_ar[pos] = n;
                if place_number(pos + 1, sudoku_ar) {
                    return true;
                }
                sudoku_ar[pos] = 0;
            }
        }
        false
    })
}

fn pretty_print(sudoku_ar: Sudoku) {
    let line_sep = "------+-------+------";
    println!("{}", line_sep);
    for (i, e) in sudoku_ar.iter().enumerate() {
        print!("{} ", e);
        if (i + 1) % 3 == 0 && (i + 1) % 9 != 0 {
            print!("| ");
        }
        if (i + 1) % 9 == 0 {
            println!(" ");
        }
        if (i + 1) % 27 == 0 {
            println!("{}", line_sep);
        }
    }
}

fn solve(sudoku_ar: &mut Sudoku) -> bool {
    place_number(0, sudoku_ar)
}

fn main() {
    let mut sudoku_ar: Sudoku = [
        8, 5, 0, 0, 0, 2, 4, 0, 0,
        7, 2, 0, 0, 0, 0, 0, 0, 9,
        0, 0, 4, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 1, 0, 7, 0, 0, 2,
        3, 0, 5, 0, 0, 0, 9, 0, 0,
        0, 4, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 8, 0, 0, 7, 0,
        0, 1, 7, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 3, 6, 0, 4, 0
    ];
    if solve(&mut sudoku_ar) {
        pretty_print(sudoku_ar);
    } else {
        println!("Unsolvable");
    }
}
```


{{out}}

```txt

------+-------+------
8 5 9 | 6 1 2 | 4 3 7
7 2 3 | 8 5 4 | 1 6 9
1 6 4 | 3 7 9 | 5 2 8
------+-------+------
9 8 6 | 1 4 7 | 3 5 2
3 7 5 | 2 6 8 | 9 1 4
2 4 1 | 5 9 3 | 7 8 6
------+-------+------
4 3 2 | 9 8 1 | 6 7 5
6 1 7 | 4 2 5 | 8 9 3
5 9 8 | 7 3 6 | 2 4 1
------+-------+------

```



## SAS

Use CLP solver in SAS/OR:


```sas
/* define SAS data set */
data Indata;
   input C1-C9;
   datalines;
. . 5 . . 7 . . 1
. 7 . . 9 . . 3 .
. . . 6 . . . . .
. . 3 . . 1 . . 5
. 9 . . 8 . . 2 .
1 . . 2 . . 4 . .
. . 2 . . 6 . . 9
. . . . 4 . . 8 .
8 . . 1 . . 5 . .
;

/* call OPTMODEL procedure in SAS/OR */
proc optmodel;
   /* declare variables */
   set ROWS = 1..9;
   set COLS = ROWS;
   var X {ROWS, COLS} >= 1 <= 9 integer;

   /* declare nine row constraints */
   con RowCon {i in ROWS}:
      alldiff({j in COLS} X[i,j]);

   /* declare nine column constraints */
   con ColCon {j in COLS}:
      alldiff({i in ROWS} X[i,j]);

   /* declare nine 3x3 block constraints */
   con BlockCon {s in 0..2, t in 0..2}:
      alldiff({i in 3*s+1..3*s+3, j in 3*t+1..3*t+3} X[i,j]);

   /* fix variables to cell values */
   /* X[i,j] = c[i,j] if c[i,j] is not missing */
   num c {ROWS, COLS};
   read data indata into [_N_] {j in COLS} <c[_N_,j]=col('C'||j)>;
   for {i in ROWS, j in COLS: c[i,j] ne .}
      fix X[i,j] = c[i,j];

   /* call CLP solver */
   solve;

   /* print solution */
   print X;
quit;
```


Output:

```txt

X
  1 2 3 4 5 6 7 8 9
1 9 8 5 3 2 7 6 4 1
2 6 7 1 5 9 4 2 3 8
3 3 2 4 6 1 8 9 5 7
4 2 4 3 7 6 1 8 9 5
5 5 9 7 4 8 3 1 2 6
6 1 6 8 2 5 9 4 7 3
7 4 5 2 8 3 6 7 1 9
8 7 1 6 9 4 5 3 8 2
9 8 3 9 1 7 2 5 6 4

```



## Scala

I use the following slightly modified code for creating new sudokus and it seems to me usable for solving given sudokus. It doesn't look like elegant and functional programming - so what! it works!
This solver works with normally 9x9 sudokus as well as with sudokus of jigsaw type or sudokus with additional condition like diagonal constraint.
{{works with|Scala|2.9.1}}

```scala
object SudokuSolver extends App {

  class Solver {

    var solution = new Array[Int](81)   //listOfFields toArray

    val fp2m: Int => Tuple2[Int,Int] = pos => Pair(pos/9+1,pos%9+1) //get row, col from array position
    val setAll = (1 to 9) toSet //all possibilities

    val arrayGroups = new Array[List[List[Int]]](81)
    val sv: Int => Int = (row: Int) => (row-1)*9 //start value group row
    val ev: Int => Int = (row: Int) => sv(row)+8 //end value group row
    val fgc: (Int,Int) => Int = (i,col) => i*9+col-1 //get group col
    val fgs: Int => (Int,Int) = p => Pair(p, p/(27)*3+p%9/3) //get group square box
    for (pos <- 0 to 80) {
      val (row,col) = fp2m(pos)
      val gRow = (sv(row) to ev(row)).toList
      val gCol = ((0 to 8) toList) map (fgc(_,col))
      val gSquare = (0 to 80 toList) map fgs filter (_._2==(fgs(pos))._2) map (_._1)
      arrayGroups(pos) = List(gRow,gCol,gSquare)
    }
    val listGroups = arrayGroups toList

    val fpv4s: (Int) => List[Int] = pos => {   //get possible values for solving
      val setRow = (listGroups(pos)(0) map (solution(_))).toSet
      val setCol = listGroups(pos)(1).map(solution(_)).toSet
      val setSquare = listGroups(pos)(2).map(solution(_)).toSet
      val setG = setRow++setCol++setSquare--Set(0)
      val setPossible = setAll--setG
      setPossible.toList.sortWith(_<_)
    }


    //solve the riddle: Nil ==> solution does not exist
    def solve(listOfFields: List[Int]): List[Int] = {
      solution = listOfFields toArray

      def checkSol(uncheckedSol: List[Int]): List[Int] = {
        if (uncheckedSol == Nil) return Nil
        solution = uncheckedSol toArray
        val check = (0 to 80).map(fpv4s(_)).filter(_.size>0)
        if (check == Nil) return uncheckedSol
        return Nil
      }

      val f1: Int => Pair[Int,Int] = p => Pair(p,listOfFields(p))
      val numFields = (0 to 80 toList) map f1 filter (_._2==0)
      val iter = numFields map ((_: (Int,Int))._1)
      var p_iter = 0

      val first: () => Int = () => {
        val ret = numFields match {
          case Nil => -1
          case _   => numFields(0)._1
        }
        ret
      }

      val last: () => Int = () => {
        val ret = numFields match {
          case Nil => -1
          case _   => numFields(numFields.size-1)._1
        }
        ret
      }

      val hasPrev: () => Boolean = () => p_iter > 0
      val prev: () => Int = () => {p_iter -= 1; iter(p_iter)}
      val hasNext: () => Boolean = () => p_iter < iter.size-1
      val next: () => Int = () => {p_iter += 1; iter(p_iter)}
      val fixed: Int => Boolean = pos => listOfFields(pos) != 0
      val possiArray = new Array[List[Int]](numFields.size)
      val firstUF = first() //first unfixed
      if (firstUF < 0) return checkSol(solution.toList) //that is it!
      var pif = iter(p_iter) //pos in fields
      val lastUF = last() //last unfixed
      val (row,col) = fp2m(pif)
      possiArray(p_iter) = fpv4s(pif).toList.sortWith(_<_)

      while(pif <= lastUF) {
        val (row,col) = fp2m(pif)
        if (possiArray(p_iter) == null) possiArray(p_iter) = fpv4s(pif).toList.sortWith(_<_)
        val possis = possiArray(p_iter)
        if (possis.isEmpty) {
          if (hasPrev()) {
            possiArray(p_iter) = null
            solution(pif) = 0
            pif = prev()
          } else {
            return Nil
          }
        } else {
          solution(pif) = possis(0)
          possiArray(p_iter) = (possis.toSet - possis(0)).toList.sortWith(_<_)
          if (hasNext()) {
            pif = next()
          } else {
            return checkSol(solution.toList)
          }
        }
      }
      checkSol(solution.toList)
    }
  }

  val f2Str: List[Int] => String = fields => {
    val sepLine = "+---+---+---+"
    val sepPoints = Set(2,5,8)
    val fs: (Int, Int) => String = (i, v) => v.toString.replace("0"," ")+(if (sepPoints.contains(i%9)) "|" else "")
    sepLine+"\n"+(0 to fields.size-1).map(i => (if (i%9==0) "|" else "")+fs(i,fields(i))+(if (i%9==8) if (sepPoints.contains(i/9)) "\n"+sepLine+"\n" else "\n" else "")).foldRight("")(_+_)
  }

  val solver = new Solver()

  val riddle = List(3,9,4,0,0,2,6,7,0,
                    0,0,0,3,0,0,4,0,0,
                    5,0,0,6,9,0,0,2,0,
                    0,4,5,0,0,0,9,0,0,
                    6,0,0,0,0,0,0,0,7,
                    0,0,7,0,0,0,5,8,0,
                    0,1,0,0,6,7,0,0,8,
                    0,0,9,0,0,8,0,0,0,
                    0,2,6,4,0,0,7,3,5)

  println("riddle:")
  println(f2Str(riddle))
  var solution = solver.solve(riddle)

  println("solution:")
  println(solution match {case Nil => "no solution!!!" case _ => f2Str(solution)})

}
```

{{out}}

```txt
riddle:
+---+---+---+
|394|  2|67 |
|   |3  |4  |
|5  |69 | 2 |
+---+---+---+
| 45|   |9  |
|6  |   |  7|
|  7|   |58 |
+---+---+---+
| 1 | 67|  8|
|  9|  8|   |
| 26|4  |735|
+---+---+---+

solution:
+---+---+---+
|394|852|671|
|268|371|459|
|571|694|823|
+---+---+---+
|145|783|962|
|682|945|317|
|937|126|584|
+---+---+---+
|413|567|298|
|759|238|146|
|826|419|735|
+---+---+---+
```


The implementation above doesn't work so effective for sudokus like Bracmat version, therefore I implemented a second version inspired by Java section:
{{works with|Scala|2.9.1}}

```scala
object SudokuSolver extends App {

  object Solver {
    var solution = new Array[Int](81)

    val fap: (Int, Int) => Int = (row, col) => (row)*9+col //function array position

    def solve(listOfFields: List[Int]): List[Int] = {
      solution = listOfFields toArray

      val mRowSubset = new Array[Boolean](81)
      val mColSubset = new Array[Boolean](81)
      val mBoxSubset = new Array[Boolean](81)

      def initSubsets: Unit = {
        for (row <- 0 to 8) {
          for (col <- 0 to 8) {
            val value = solution(fap(row, col))
            if (value != 0)
              setSubsetValue(row, col, value, true)
          }
        }
      }

      def setSubsetValue(r: Int, c: Int, value: Int, present: Boolean): Unit = {
        mRowSubset(fap(r, value - 1)) = present
        mColSubset(fap(c, value - 1)) = present
        mBoxSubset(fap(computeBoxNo(r, c), value - 1)) = present
      }

      def computeBoxNo(r: Int, c: Int): Int = {
        val boxRow = r / 3
        val boxCol = c / 3
        return boxRow * 3 + boxCol
      }

      def isValid(r: Int, c: Int, value: Int): Boolean = {
        val vVal = value - 1
        val isPresent = mRowSubset(fap(r, vVal)) || mColSubset(fap(c, vVal)) || mBoxSubset(fap(computeBoxNo(r, c), vVal))
        return !isPresent
      }

      def solve(row: Int, col: Int): Boolean = {
        var r = row
        var c = col

        if (r == 9) {
          r = 0
          c += 1
          if (c == 9)
            return true
        }

        if(solution(fap(r,c)) != 0)
          return solve(r+1,c)
        for(value <- 1 to 9)
          if(isValid(r, c, value)) {
            solution(fap(r,c)) = value
            setSubsetValue(r, c, value, true)
            if(solve(r+1,c))
              return true
            setSubsetValue(r, c, value, false)
          }
        solution(fap(r,c)) = 0
        return false
      }

      def checkSol: Boolean = {
        initSubsets
        if ((mRowSubset.exists(_==false)) || (mColSubset.exists(_==false)) || (mBoxSubset.exists(_==false))) return false
        true
      }

      initSubsets
      val ret = solve(0,0)
      if (ret)
        if (checkSol) return solution.toList else Nil
      else
        return Nil
    }
  }

  val f2Str: List[Int] => String = fields => {
    val f2Stri: List[Int] => String = fields => {
      val sepLine = "+---+---+---+"
      val sepPoints = Set(2,5,8)
      val fs: (Int, Int) => String = (i, v) => v.toString.replace("0"," ")+(if (sepPoints.contains(i%9)) "|" else "")
      val s = sepLine+"\n"+(0 to fields.size-1).map(i => (if (i%9==0) "|" else "")+fs(i,fields(i))+(if (i%9==8) if (sepPoints.contains(i/9)) "\n"+sepLine+"\n" else "\n" else "")).foldRight("")(_+_)
      s
    }
    val s = fields match {case Nil => "no solution!!!" case _ => f2Stri(fields)}
    s
  }

  val elapsedtime: (=> Unit) => Long = f => {val s = System.currentTimeMillis; f; (System.currentTimeMillis - s)/1000}

  var sol = List[Int]()

  val sudokus = List(
      ("riddle used in Ada section:",
       "394..267....3..4..5..69..2..45...9..6.......7..7...58..1..67..8..9..8....264..735"),
      ("riddle used in Bracmat section:",
       "..............3.85..1.2.......5.7.....4...1...9.......5......73..2.1........4...9"),
      ("riddle from Groovy section: 4th exceptionally difficult example in Wikipedia: ~80 seconds",
       "..3......4...8..36..8...1...4..6..73...9..........2..5..4.7..686........7..6..5.."),
      ("riddle used in Ada section with incorrect modifactions - it should fail:",
       "3943.267....3..4..5..69..2..45...9..6.......7..7...58..1..67..8..9..8....264..735"),
      ("riddle constructed with mess - it should fail too:",
       "123456789456789123789123456.45..89..6.......72.7...58.31..67..8..9..8....264..735"))

  for (sudoku <- sudokus) {
    val desc = sudoku._1
    val riddle = sudoku._2.replace(".","0").toList.map(_.toString.toInt)
    println(desc+"\n"+f2Str(riddle)+"\n"
      +"elapsed time: "+elapsedtime(sol = Solver.solve(riddle))+" sec"+"\n"+"solution:"+"\n"+f2Str(sol)
      +("\n"*2))
  }
}
```

{{out}}

```txt
riddle used in Ada section:
+---+---+---+
|394|  2|67 |
|   |3  |4  |
|5  |69 | 2 |
+---+---+---+
| 45|   |9  |
|6  |   |  7|
|  7|   |58 |
+---+---+---+
| 1 | 67|  8|
|  9|  8|   |
| 26|4  |735|
+---+---+---+
elapsed time: 0 sec
solution:
+---+---+---+
|394|852|671|
|268|371|459|
|571|694|823|
+---+---+---+
|145|783|962|
|682|945|317|
|937|126|584|
+---+---+---+
|413|567|298|
|759|238|146|
|826|419|735|
+---+---+---+

riddle used in Bracmat section:
+---+---+---+
|   |   |   |
|   |  3| 85|
|  1| 2 |   |
+---+---+---+
|   |5 7|   |
|  4|   |1  |
| 9 |   |   |
+---+---+---+
|5  |   | 73|
|  2| 1 |   |
|   | 4 |  9|
+---+---+---+
elapsed time: 43 sec
solution:
+---+---+---+
|987|654|321|
|246|173|985|
|351|928|746|
+---+---+---+
|128|537|694|
|634|892|157|
|795|461|832|
+---+---+---+
|519|286|473|
|472|319|568|
|863|745|219|
+---+---+---+

riddle from Groovy section: 4th exceptionally difficult example in Wikipedia: ~80 seconds
+---+---+---+
|  3|   |   |
|4  | 8 | 36|
|  8|   |1  |
+---+---+---+
| 4 | 6 | 73|
|   |9  |   |
|   |  2|  5|
+---+---+---+
|  4| 7 | 68|
|6  |   |   |
|7  |6  |5  |
+---+---+---+
elapsed time: 3 sec
solution:
+---+---+---+
|123|456|789|
|457|189|236|
|968|327|154|
+---+---+---+
|249|561|873|
|576|938|412|
|831|742|695|
+---+---+---+
|314|275|968|
|695|814|327|
|782|693|541|
+---+---+---+

riddle used in Ada section with incorrect modifactions - it should fail:
+---+---+---+
|394|3 2|67 |
|   |3  |4  |
|5  |69 | 2 |
+---+---+---+
| 45|   |9  |
|6  |   |  7|
|  7|   |58 |
+---+---+---+
| 1 | 67|  8|
|  9|  8|   |
| 26|4  |735|
+---+---+---+
elapsed time: 0 sec
solution:
no solution!!!

riddle constructed with mess - it should fail too:
+---+---+---+
|123|456|789|
|456|789|123|
|789|123|456|
+---+---+---+
| 45|  8|9  |
|6  |   |  7|
|2 7|   |58 |
+---+---+---+
|31 | 67|  8|
|  9|  8|   |
| 26|4  |735|
+---+---+---+
elapsed time: 0 sec
solution:
no solution!!!
```



## Scilab

The grid should be input in <code>Init_board</code> as a 9x9 matrix. The blanks should be represented by 0. A rule that the initial game should have at least 17 givens is enforced, for it guarantees a unique solution. It is also possible to set a maximum number of steps to the solver using <code>break_point</code>. If it is set to 0, there will be no break until it finds the solution.

<lang>Init_board=[...
5 3 0 0 7 0 0 0 0;...
6 0 0 1 9 5 0 0 0;...
0 9 8 0 0 0 0 6 0;...
8 0 0 0 6 0 0 0 3;...
4 0 0 8 0 3 0 0 1;...
7 0 0 0 2 0 0 0 6;...
0 6 0 0 0 0 2 8 0;...
0 0 0 4 1 9 0 0 5;...
0 0 0 0 8 0 0 7 9];

break_point=1.0d5; //if 0 there will be no break

function []=disp_board(board)
    StringBoard=string(board);
    for i=1:9
        for j=1:9
            if board(i,j)==0 then
                StringBoard(i,j)='×';
            end
        end
    end

    StringBoard=[StringBoard, string(zeros(9,2))];
    StringBoard=[StringBoard; string(zeros(2,11))];

    for i=1:9
        StringBoard(i,:)=[StringBoard(i,1:3), '|', StringBoard(i,4:6), '|', StringBoard(i,7:9)]
    end
    StringBoard(9:11,:)=StringBoard(7:9,:);
    StringBoard(8,:)=strsplit('-----------');
    StringBoard(5:7,:)=StringBoard(4:6,:);
    StringBoard(4,:)=strsplit('-----------');

    disp(StringBoard)
endfunction

function varargout=validate_input(input,position,board)
    row=board(position(1),:);
    column=board(:,position(2));
    block=zeros(3,3);
    if position(1)>=1 & position(1)<=3 then
        i=0;
    elseif position(1)>=4 & position(1)<=6 then
        i=3;
    else
        i=6;
    end
    if position(2)>=1 & position(2)<=3 then
        j=0;
    elseif position(2)>=4 & position(2)<=6 then
        j=3;
    else
        j=6;
    end
    block=board(i+1:i+3,j+1:j+3)

    valid_input=%F;
    valid_row=%F;
    valid_col=%F;
    valid_block=%F;

    if find(input==row)==[] then
        valid_row=%T;
    end
    if find(input==column)==[] then
        valid_col=%T;
    end
    if find(input==block)==[] then
        valid_block=%T;
    end
    if valid_row & valid_col & valid_block then
        valid_input=%T;
    end

    varargout=list(valid_input,valid_row,valid_col,valid_block)
endfunction

function varargout=validate_board(board)
    valid_flag1=%T;
    for i=1:9
        for j=1:9
            if board(i,j)~= 0 then
                check_board=Init_board;
                check_board(i,j)=0;
                valid_flag1=validate_input(board(i,j),[i j],check_board);
                if ~valid_flag1 then
                    break
                end
            end
        end
        if ~valid_flag1 then
            break
        end
    end

    valid_flag2 = (length( find(board) ) >= 17); //enforces rule of minimum of 17 givens
                                                 //set it to always %T to ignore this rule
    valid_board = (valid_flag1 & valid_flag2);

    varargout=list(valid_board)
endfunction

disp('Initial board:');
disp_board(Init_board);

valid_init_board=validate_board(Init_board);

if ~valid_init_board then
    error('Invalid initial board. Should follow sudoku rules and have at least 17 clues.');
end

blank=[];
for i=1:9
    for j=1:9
        if Init_board(i,j)== 0 then
            blank=[blank; i j];
        end
    end
end

Solved_board=Init_board;

tic();
i=0; counter=0; breaked=%F;
while i<size(blank,'r')
    i=i+1;
    counter=counter+1;
    pos=blank(i,:);

    value=Solved_board(pos(1),pos(2));

    valid_value=%F;

    while valid_value==%F
        value=value+1;
        if value>=10
            break
        else
            valid_value=validate_input(value,pos,Solved_board);
        end
    end

    if valid_value & value<10 then
        Solved_board(pos(1),pos(2))=value
    else
        Solved_board(pos(1),pos(2))=0;
        i=i-2;
    end

    if counter==break_point
        breaked=%T;
        break
    end
end

valid_solved_board=validate_board(Solved_board);
t2=toc();

if valid_solved_board & ~breaked then
    disp('Solved!');
    disp('Solution:');
    disp_board(Solved_board);
    disp('Time: '+string(t2)+'s.');
    disp('Steps: '+string(counter)+'.');
elseif breaked
    disp('Break point reached.');
    disp('Time: '+string(t2)+'s.');
    disp_board(Solved_board);
elseif ~valid_solved_board & ~breaked
    disp('Invalid solution found.');
    disp_board(Solved_board);
end
```


{{out}}


```txt
 Initial board:

!5  3  ×  |  ×  7  ×  |  ×  ×  ×  !
!                                 !
!6  ×  ×  |  1  9  5  |  ×  ×  ×  !
!                                 !
!×  9  8  |  ×  ×  ×  |  ×  6  ×  !
!                                 !
!-  -  -  -  -  -  -  -  -  -  -  !
!                                 !
!8  ×  ×  |  ×  6  ×  |  ×  ×  3  !
!                                 !
!4  ×  ×  |  8  ×  3  |  ×  ×  1  !
!                                 !
!7  ×  ×  |  ×  2  ×  |  ×  ×  6  !
!                                 !
!-  -  -  -  -  -  -  -  -  -  -  !
!                                 !
!×  6  ×  |  ×  ×  ×  |  2  8  ×  !
!                                 !
!×  ×  ×  |  4  1  9  |  ×  ×  5  !
!                                 !
!×  ×  ×  |  ×  8  ×  |  ×  7  9  !

 Solved!

 Solution:

!5  3  4  |  6  7  8  |  9  1  2  !
!                                 !
!6  7  2  |  1  9  5  |  3  4  8  !
!                                 !
!1  9  8  |  3  4  2  |  5  6  7  !
!                                 !
!-  -  -  -  -  -  -  -  -  -  -  !
!                                 !
!8  5  9  |  7  6  1  |  4  2  3  !
!                                 !
!4  2  6  |  8  5  3  |  7  9  1  !
!                                 !
!7  1  3  |  9  2  4  |  8  5  6  !
!                                 !
!-  -  -  -  -  -  -  -  -  -  -  !
!                                 !
!9  6  1  |  5  3  7  |  2  8  4  !
!                                 !
!2  8  7  |  4  1  9  |  6  3  5  !
!                                 !
!3  4  5  |  2  8  6  |  1  7  9  !

 Time: 1.7142502s.

 Steps: 8365.
```



## Sidef

{{trans|Perl 6}}

```ruby
func check(i, j) is cached {
    var (id, im) = i.divmod(9)
    var (jd, jm) = j.divmod(9)

    jd == id && return true
    jm == im && return true

    (id//3 == jd//3) &&
    (jm//3 == im//3)
}

func solve(grid) {
    for i in ^grid {
        grid[i] && next
        var t = [grid[{|j| check(i, j) }.grep(^grid)]].freq

        { |k|
            t.has_key(k) && next
            grid[i] = k
            solve(grid)
        } << 1..9

        grid[i] = 0
        return nil
    }

    for i in ^grid {
        print "#{grid[i]} "
        print " "  if (3  -> divides(i+1))
        print "\n" if (9  -> divides(i+1))
        print "\n" if (27 -> divides(i+1))
    }
}

var grid = %i(
    5 3 0  0 2 4  7 0 0
    0 0 2  0 0 0  8 0 0
    1 0 0  7 0 3  9 0 2

    0 0 8  0 7 2  0 4 9
    0 2 0  9 8 0  0 7 0
    7 9 0  0 0 0  0 8 0

    0 0 0  0 3 0  5 0 6
    9 6 0  0 1 0  3 0 0
    0 5 0  6 9 0  0 1 0
)

solve(grid)
```

{{out}}

```txt
5 3 9  8 2 4  7 6 1
6 7 2  1 5 9  8 3 4
1 8 4  7 6 3  9 5 2

3 1 8  5 7 2  6 4 9
4 2 5  9 8 6  1 7 3
7 9 6  3 4 1  2 8 5

8 4 1  2 3 7  5 9 6
9 6 7  4 1 5  3 2 8
2 5 3  6 9 8  4 1 7
```



## SQL

{{works with|oracle|11.2 and higher}}

The implementation below uses a recursive WITH clause (aka recursive CTE, recursive query, recursive factored subquery). This is supported - with minimal syntactical differences - by some (perhaps many) but not all SQL dialects. The code was written and tested in Oracle SQL; Oracle has supported recursive subqueries since version 11.2.

The code implements a brute force algorithm. It can solve most problems in less than half a second (depending on hardware too), although I found a few that take 2-3 and up to 6 seconds. The output may either be empty (when the problem is impossible), a single completed grid (for a "correct" problem), or all the correct solutions to problems that admit more than one solution.

The input and output are presented as strings of 81 characters, where each character is either a digit or a space (indicating an empty cell in the grid). The translation between grids and such strings is trivial (convert grid to string by concatenating rows, reading left to right and then top to bottom), and not covered in the solution. The input is given as a bind variable, ''':game'''.


```sql
with
  symbols (d) as (select to_char(level) from dual connect by level <=  9)
, board   (i) as (select level          from dual connect by level <= 81)
, neighbors (i, j) as (
    select b1.i, b2.i
    from   board b1 inner join board b2
      on   b1.i != b2.i
           and (
                    mod(b1.i - b2.i, 9) = 0
                 or ceil(b1.i /  9) = ceil(b2.i /  9)
                 or ceil(b1.i / 27) = ceil(b2.i / 27) and trunc(mod(b1.i - 1, 9) / 3) = trunc(mod(b2.i - 1, 9) / 3)
               )
  )
, r (str, pos) as (
    select  :game, instr(:game, ' ')
      from  dual
    union all
    select  substr(r.str, 1, r.pos - 1) || s.d || substr(r.str, r.pos + 1), instr(r.str, ' ', r.pos + 1)
      from  r inner join symbols s
        on  r.pos > 0 and not exists (
                                       select *
                                       from   neighbors n
                                       where  r.pos = n.i and s.d = substr(r.str, n.j, 1)
                                     )
  )
select str
from   r
where  pos = 0
;
```


A better (faster) approach - taking advantage of database-specific features - is to create a '''table''' NEIGHBORS (similar to the inline view in the WITH clause) and an index on column I of that table; then the query execution time drops by more than half in most cases.


## Stata


In this implementation, a Sudoku is a 9x9 matrix a, with either digits 1-9 or missing values. A cell is any element of a. A cell can be reference with indices i,j, or with a single index n between 1 and 81.

Three functions are defined:
* sudoku(a) will return 0 if there is no solution, 1 if there is '''at least''' one solution, and then a is modified in place with the solution found. This function builds several tables before calling the main "solver" function.
* solve(a,s,t,v,w) is made of two parts: first, all cells that can be filled without any assumption are considered known. Then, if not all cells are known, a cell with minimal number of choices is found, and a recursive call to solve is made with all possibilities in turn for this cell.
* push(a,s,t,v,w,n,z) is an auxiliary function, which pushes the value z in the nth cell of matrix a. Here n is a value between 1 and 81.

Fixed tables:
* t(n,.) is a row i,j,k: the cell n (1-81) has row index i, column index j, and square index k, where i, j, k are all in 1-9.
* s(n,.) is a row that contains the indices of the 20 "neighbors" of cell n: these are the cells in the same row, column or square.

Varying tables:
* v(n,z)=1 if the cell n may have the value z, otherwise 0. When a value z is pushed into the matrix, all neighboring cells are updated, as they can't take the value z anymore.
* w(n)=1 if cell n is not yet known, otherwise 0.
* In the initialization step of the sudoku() function, w has another meaning: it stores the column index of the last entry in row n of the array s while it is built. When it's done, every element of w is thus twenty.

The example grid given below is taken from [https://en.wikipedia.org/wiki/Sudoku_solving_algorithms Wikipedia]. It does not require any recursive call (it's entirely filled in the first step of ''solve''), as can be seen with additional ''printf'' in the code to follow the algorithm.


```stata
mata
function sudoku(a) {
	s = J(81,20,.)
	t = J(81,3,.)
	v = J(81,9,1)
	w = J(81,1,0)
	for (i=1; i<=9; i++) {
		for (j=1; j<=9; j++) {
			n = (i-1)*9+j
			k = floor((i-1)/3)*3+floor((j-1)/3)+1
			t[n,.] = i,j,k
		}
	}
	for (i=1; i<=81; i++) {
		for (j=i+1; j<=81; j++) {
			if (any(t[i,.]:==t[j,.])) {
				w[i]=w[i]+1
				w[j]=w[j]+1
				s[i,w[i]] = j
				s[j,w[j]] = i
			}
		}
	}
	w = J(81,1,1)
	for (i=1; i<=9; i++) {
		for (j=1; j<=9; j++) {
			if (a[i,j]<.) {
				push(a,s,t,v,w,(i-1)*9+j,a[i,j])
			}
		}
	}
	return(solve(a,s,t,v,w))
}

function solve(a,s,t,v,w) {
	for (q=1; q;) {
		q = 0
		for (n=1; n<=81; n++) {
			if (w[n]) {
				r = sum(v[n,.])
				if (r==0) {
					return(0)
				}
				else if (r==1) {
					q = 1
					push(a,s,t,v,w,n,selectindex(v[n,.]))
				}
			}
		}
	}

	if (all(w:==0)) {
		return(1)
	}
	else {
		m0 = n0 = .
		for (n=1; n<=81; n++) {
			m = sum(v[n,.])
			if (w[n] & m>1 & m<m0) {
				m0 = m
				n0 = n
			}
		}
		z = selectindex(v[n0,.])
		for (i=1; i<=m0; i++) {
			a2 = a
			v2 = v
			w2 = w
			push(a2,s,t,v2,w2,n0,z[i])
			if (solve(a2,s,t,v2,w2)) {
				a = a2
				return(1)
			}
		}
		return(0)
	}
}

function push(a,s,t,v,w,n,z) {
	w[n] = 0
	i = t[n,1]
	j = t[n,2]
	a[i,j] = z
	for (k=1; k<=20; k++) {
		v[s[n,k],z] = 0
	}
}

a = 5,3,.,.,7,.,.,.,.\
    6,.,.,1,9,5,.,.,.\
    .,9,8,.,.,.,.,6,.\
    8,.,.,.,6,.,.,.,3\
    4,.,.,8,.,3,.,.,1\
    7,.,.,.,2,.,.,.,6\
    .,6,.,.,.,.,2,8,.\
    .,.,.,4,1,9,.,.,5\
    .,.,.,.,8,.,.,7,9

sudoku(a)
a
end
```


'''Output'''


```txt
1

       1   2   3   4   5   6   7   8   9
    +-------------------------------------+
  1 |  5   3   4   6   7   8   9   1   2  |
  2 |  6   7   2   1   9   5   3   4   8  |
  3 |  1   9   8   3   4   2   5   6   7  |
  4 |  8   5   9   7   6   1   4   2   3  |
  5 |  4   2   6   8   5   3   7   9   1  |
  6 |  7   1   3   9   2   4   8   5   6  |
  7 |  9   6   1   5   3   7   2   8   4  |
  8 |  2   8   7   4   1   9   6   3   5  |
  9 |  3   4   5   2   8   6   1   7   9  |
    +-------------------------------------+
```


Two more examples, from [http://www.7sudoku.com/very-difficult here] and [http://www.extremesudoku.info/sudoku.html there].


```stata
a = 7,9,.,.,.,3,.,.,2\
    .,6,.,5,1,.,.,.,.\
    .,.,.,.,.,2,.,.,6\
    .,.,.,8,.,1,.,4,.\
    .,.,.,.,.,.,1,.,3\
    5,.,.,.,2,.,.,.,.\
    .,.,.,.,.,.,.,7,4\
    .,.,1,.,.,.,.,6,5\
    .,.,8,.,9,7,.,.,.

sudoku(a)
a


       1   2   3   4   5   6   7   8   9
    +-------------------------------------+
  1 |  7   9   5   6   8   3   4   1   2  |
  2 |  2   6   4   5   1   9   7   3   8  |
  3 |  1   8   3   7   4   2   5   9   6  |
  4 |  9   3   6   8   5   1   2   4   7  |
  5 |  8   4   2   9   7   6   1   5   3  |
  6 |  5   1   7   3   2   4   6   8   9  |
  7 |  3   2   9   1   6   5   8   7   4  |
  8 |  4   7   1   2   3   8   9   6   5  |
  9 |  6   5   8   4   9   7   3   2   1  |
    +-------------------------------------+

a = .,.,4,1,.,.,.,.,9\
    .,9,.,.,8,.,.,6,.\
    6,.,.,.,.,3,7,.,.\
    5,.,.,.,.,2,8,.,.\
    .,4,.,.,3,.,.,2,.\
    .,.,1,8,.,.,.,.,5\
    .,.,2,5,.,.,.,.,3\
    .,8,.,.,7,.,.,1,.\
    7,.,.,.,.,1,4,.,.

sudoku(a)
a


       1   2   3   4   5   6   7   8   9
    +-------------------------------------+
  1 |  2   7   4   1   5   6   3   8   9  |
  2 |  1   9   3   7   8   4   5   6   2  |
  3 |  6   5   8   2   9   3   7   4   1  |
  4 |  5   3   7   6   1   2   8   9   4  |
  5 |  8   4   6   9   3   5   1   2   7  |
  6 |  9   2   1   8   4   7   6   3   5  |
  7 |  4   1   2   5   6   8   9   7   3  |
  8 |  3   8   5   4   7   9   2   1   6  |
  9 |  7   6   9   3   2   1   4   5   8  |
    +-------------------------------------+
```



## Swift

{{trans|Java}}

```Swift
import Foundation

typealias SodukuPuzzle = [[Int]]

class Soduku {
    let mBoardSize:Int!
    let mBoxSize:Int!
    var mBoard:SodukuPuzzle!
    var mRowSubset:[[Bool]]!
    var mColSubset:[[Bool]]!
    var mBoxSubset:[[Bool]]!

    init(board:SodukuPuzzle) {
        mBoard = board
        mBoardSize = board.count
        mBoxSize = Int(sqrt(Double(mBoardSize)))
        mRowSubset = [[Bool]](count: mBoardSize, repeatedValue: [Bool](count: mBoardSize, repeatedValue: false))
        mColSubset = [[Bool]](count: mBoardSize, repeatedValue: [Bool](count: mBoardSize, repeatedValue: false))
        mBoxSubset = [[Bool]](count: mBoardSize, repeatedValue: [Bool](count: mBoardSize, repeatedValue: false))
        initSubsets()
    }

    func computeBoxNo(i:Int, _ j:Int) -> Int {
        let boxRow = i / mBoxSize
        let boxCol = j / mBoxSize

        return boxRow * mBoxSize + boxCol
    }

    func initSubsets() {
        for i in 0..<mBoard.count {
            for j in 0..<mBoard.count {
                let value = mBoard[i][j]

                if value != 0 {
                    setSubsetValue(i, j, value, true);
                }
            }
        }
    }

    func isValid(i:Int, _ j:Int, var _ val:Int) -> Bool {
        val--
        let isPresent = mRowSubset[i][val] || mColSubset[j][val] || mBoxSubset[computeBoxNo(i, j)][val]
        return !isPresent
    }

    func printBoard() {
        for i in 0..<mBoardSize {
            if i % mBoxSize == 0 {
                println(" -----------------------")
            }

            for j in 0..<mBoardSize {
                if j % mBoxSize == 0 {
                    print("| ")
                }

                print(mBoard[i][j] != 0 ? String(mBoard[i][j]) : " ")
                print(" ")
            }

            println("|")
        }

        println(" -----------------------")
    }

    func setSubsetValue(i:Int, _ j:Int, _ value:Int, _ present:Bool) {
        mRowSubset[i][value - 1] = present
        mColSubset[j][value - 1] = present
        mBoxSubset[computeBoxNo(i, j)][value - 1] = present
    }

    func solve() {
        solve(0, 0)
    }

    func solve(var i:Int, var _ j:Int) -> Bool {
        if i == mBoardSize {
            i = 0
            j++
            if j == mBoardSize {
                return true
            }
        }

        if mBoard[i][j] != 0 {
            return solve(i + 1, j)
        }

        for value in 1...mBoardSize {
            if isValid(i, j, value) {
                mBoard[i][j] = value
                setSubsetValue(i, j, value, true)

                if solve(i + 1, j) {
                    return true
                }

                setSubsetValue(i, j, value, false)
            }
        }

        mBoard[i][j] = 0
        return false
    }
}

let board = [
    [4, 0, 0, 0, 0, 0, 0, 6, 0],
    [5, 0, 0, 0, 8, 0, 9, 0, 0],
    [3, 0, 0, 0, 0, 1, 0, 0, 0],
    [0, 2, 0, 7, 0, 0, 0, 0, 1],
    [0, 9, 0, 0, 0, 0, 0, 4, 0],
    [8, 0, 0, 0, 0, 3, 0, 5, 0],
    [0, 0, 0, 2, 0, 0, 0, 0, 7],
    [0, 0, 6, 0, 5, 0, 0, 0, 8],
    [0, 1, 0, 0, 0, 0, 0, 0, 6]
]

let puzzle = Soduku(board: board)
puzzle.solve()
puzzle.printBoard()
```

{{out}}

```txt

 -----------------------
| 4 8 2 | 9 7 5 | 1 6 3 |
| 5 6 1 | 3 8 2 | 9 7 4 |
| 3 7 9 | 6 4 1 | 8 2 5 |
 -----------------------
| 6 2 5 | 7 9 4 | 3 8 1 |
| 1 9 3 | 5 6 8 | 7 4 2 |
| 8 4 7 | 1 2 3 | 6 5 9 |
 -----------------------
| 9 5 8 | 2 1 6 | 4 3 7 |
| 7 3 6 | 4 5 9 | 2 1 8 |
| 2 1 4 | 8 3 7 | 5 9 6 |
 -----------------------

```


{{works with|Swift 3}}


```Swift

func solving(board: [[Int]]) -> [[Int]] {
	var board = board
	var isSolved = false
	while !isSolved {
		for x in 0 ..< 9  {
			for y in 0 ..< 9 {
				if board[x][y] == 0 {
					let known = Set(board.map { $0[y] } + board[x] + subgrid(board, pos: (x, y)))
					let possible = Set(Array(1...9)).subtracting(known)
					if possible.count == 1  {
						board[x][y] = possible.first!
					}
				}
			}
			isSolved = 45 == board[x].reduce(0, +)
		}
	}
	return board
}

func subgrid(_ board: [[Int]], pos: (Int, Int)) -> [Int] {
	var r = [Int]()
	var (x, y) = pos
	x = x / 3 * 3
	y = y / 3 * 3
	for i in x ..< x + 3 {
		for j in y ..< y + 3 {
			r.append(board[i][j])
		}
	}
	return r
}

func print(_ board: [[Int]]) {
	for i in board.indices {
		if i % 9 == 0 {
			print(" -------------------")
		}
		for j in board.indices {
			if j % board.count == 0 {
				print("| ", terminator: "")
			}
			let digit = board[i][j]
			print(digit != 0 ? digit : " ", terminator: "")
			print(" ", terminator: "")
		}
		print("|")
	}
	print(" -------------------")
}

let puzzle = [
	[0,2,0,4,5,0,7,0,9],
	[0,0,0,1,0,9,0,3,0],
	[0,0,8,0,0,0,1,0,4],
	[0,4,0,0,6,1,0,7,0],
	[5,0,6,0,3,0,0,1,0],
	[0,3,0,0,0,2,0,9,0],
	[3,0,4,0,7,5,0,6,8],
	[0,9,0,0,1,0,3,0,7],
	[0,0,2,0,0,3,0,0,1]
]

print(solving(board: puzzle))

```

{{out}}

```txt

 -------------------
| 1 2 3 4 5 6 7 8 9 |
| 4 5 7 1 8 9 2 3 6 |
| 9 6 8 3 2 7 1 5 4 |
| 2 4 9 5 6 1 8 7 3 |
| 5 7 6 9 3 8 4 1 2 |
| 8 3 1 7 4 2 6 9 5 |
| 3 1 4 2 7 5 9 6 8 |
| 6 9 5 8 1 4 3 2 7 |
| 7 8 2 6 9 3 5 4 1 |
 -------------------

```



## SystemVerilog




```systemverilog


//////////////////////////////////////////////////////////////////////////////
/// SudokuSolver                                                           ///
///    A class that fills up a sudoku board, the initial board is given    ///
///    as an array preset_rows, the positions where preset_rows is zero    ///
///    are to be determined. Three views of the sudoku board are created   ///
///    and the uniqueness of its elements are defined by on constraint for ///
///    each view, one constraint ensures that the values are between 1 and ///
///    9, and two other constraints are used to ensure that the values in  ///
///    all three views agree to each other.                                ///
///                                                                        ///
///                                                                        ///
///    A solution using only the "rows" array would be possible, however   ///
///    this illustrates better how one can relate different variables in   ///
///    SystemVerilog Constrained randomization.                            ///
//////////////////////////////////////////////////////////////////////////////
class SudokuSolver;
  rand int tiles[0:8][0:8];
  rand int rows[0:8][0:8];
  rand int cols[0:8][0:8];
  int preset_rows[0:8][0:8];
  constraint board_input {
    foreach(preset_rows[i])foreach(preset_rows[i][j])
       if(preset_rows[i][j] != 0) rows[i][j] == preset_rows[i][j];
  }
  constraint range {
    foreach(rows[i]) foreach(rows[i][j])
        rows[i][j] inside {[1:9]};
  }
  ////////////////////////////////////////////////
  /// Every number in a row is unique          ///
  ////////////////////////////////////////////////
  constraint rows_permutation {
    foreach(rows[i]) foreach(rows[i][j1])
                     foreach(rows[i][j2])
      if(j1 != j2) rows[i][j1] != rows[i][j2];
  }
  ///////////////////////////////////////////////
  /// Every number in a column is unique      ///
  ///////////////////////////////////////////////
  constraint cols_permutation {
    foreach(cols[i]) foreach(cols[i][j1])
                     foreach(cols[i][j2])
      if(j1 != j2) cols[i][j1] != cols[i][j2];
  }
  /////////////////////////////////////////////////
  /// Every number in a tile (square) is unique ///
  /////////////////////////////////////////////////
  constraint tiles_permutation {
    foreach(tiles[i]) foreach(tiles[i][j1])
                     foreach(tiles[i][j2])
      if(j1 != j2) tiles[i][j1] != tiles[i][j2];
  }
  ///////////////////////////////////////////////////
  /// Makes sure that sure that the numbers in    ///
  /// each view agree with other views            ///
  ///////////////////////////////////////////////////
  constraint rows_vs_tiles {
    foreach(tiles[i]) foreach(tiles[i][j])
      tiles[i][j] == rows[(i/3) * 3 + (j/3)][3*(i%3)+(j%3)];
  }
  constraint rows_vs_cols {
    foreach(cols[i]) foreach(cols[i][j])
      cols[i][j] == rows[j][i];
  }
  ///////////////////////////////////////////////////
  /// Print the current state of the board in the ///
  /// standard output                             ///
  ///////////////////////////////////////////////////
  function void printBoard;
    int i, j;
    for(i = 0; i < 9; ++i) begin
      if(i % 3 == 0)$display("   -------------");
      $write("   ");
      for(j = 0; j < 9; ++j) begin
        if(j % 3 == 0) $write("|");
        $write("%c", "0" + rows[i][j]);
      end
      $display("|");
    end
    $display("   -------------");
  endfunction
  function void printInitial;
    int i, j;
    for(i = 0; i < 9; ++i) begin
      if(i % 3 == 0)$display("-------------");
      for(j = 0; j < 9; ++j) begin
        if(j % 3 == 0) $write("|");
        if(preset_rows[i][j]) begin
          $write("%c", "0" + preset_rows[i][j]);
        end
        else begin
          $write(".");
        end
      end
      $display("|");
    end
    $display("-------------");
  endfunction

endclass

//////////////////////////////////////////////////////
/// Simple program instantiating the sudoku object ///
//////////////////////////////////////////////////////
program SudokuTest;
  SudokuSolver board;
initial begin
  board = new;
  foreach(board.preset_rows[0][i]) board.preset_rows[i][i] = i+1;
  $display("Initial Board:");
  board.printInitial();
  // Generate two different solutions for the board
  if(board.randomize())begin
    $display("One solution:");
    board.printBoard();
  end
  else begin
    $display("ERROR: Failed to generate first solution");
  end
  if(board.randomize())begin
    $display("Another solution:");
    board.printBoard();
  end
  else begin
    $display("ERROR: Failed to generate second solution");
  end
end
endprogram

```


It can be seen that SystemVerilog randomization is a very powerfull tool, in this implementation I directly described the game constraints and the randomization engine takes care of producing solutions, and when multiple solutions are possible they will be chosen at random.


Running the above code using Cadence ncverilog I get


```txt

>  ncverilog +sv sudoku.sv

Initial Board:
-------------
|1..|...|...|
|.2.|...|...|
|..3|...|...|
-------------
|...|4..|...|
|...|.5.|...|
|...|..6|...|
-------------
|...|...|7..|
|...|...|.8.|
|...|...|..9|
-------------
One solution:
   -------------
   |168|547|392|
   |925|631|478|
   |743|928|156|
   -------------
   |832|479|561|
   |671|852|934|
   |459|316|827|
   -------------
   |396|284|715|
   |214|795|683|
   |587|163|249|
   -------------
Another solution:
   -------------
   |197|642|358|
   |425|389|176|
   |683|715|294|
   -------------
   |956|431|827|
   |842|957|631|
   |731|826|945|
   -------------
   |214|598|763|
   |569|173|482|
   |378|264|519|
   -------------

```



## Tcl

Adapted from [http://wiki.tcl.tk/19934 a page on the Tcler's Wiki] to use a standard object system.

Note that you can implement more rules if you want. Just make another subclass of <code>Rule</code> and the solver will pick it up and use it automatically.
{{works with|Tcl|8.6}} or {{libheader|TclOO}}

```tcl
package require Tcl 8.6
oo::class create Sudoku {
    variable idata

    method clear {} {
	for {set y 0} {$y < 9} {incr y} {
	    for {set x 0} {$x < 9} {incr x} {
		my set $x $y {}
	    }
	}
    }
    method load {data} {
	set error "data must be a 9-element list, each element also being a\
		list of 9 numbers from 1 to 9 or blank or an @ symbol."
	if {[llength $data] != 9} {
	    error $error
	}
	for {set y 0} {$y<9} {incr y} {
	    set row [lindex $data $y]
	    if {[llength $row] != 9} {
		error $error
	    }
	    for {set x 0} {$x<9} {incr x} {
		set d [lindex $row $x]
		if {![regexp {^[@1-9]?$} $d]} {
		    error $d-$error
		}
		if {$d eq "@"} {set d ""}
		my set $x $y $d
	    }
	}
    }
    method dump {} {
	set rows {}
	for {set y 0} {$y < 9} {incr y} {
	    lappend rows [my getRow 0 $y]
	}
	return $rows
    }

    method Log msg {
	# Chance to print message
    }

    method set {x y value} {
	if {[catch {set value [format %d $value]}]} {set value 0}
	if {$value<1 || $value>9} {
	    set idata(sq$x$y) {}
	} else {
	    set idata(sq$x$y) $value
	}
    }
    method get {x y} {
	if {![info exists idata(sq$x$y)]} {
	    return {}
	}
	return $idata(sq$x$y)
    }

    method getRow {x y} {
	set row {}
	for {set x 0} {$x<9} {incr x} {
	    lappend row [my get $x $y]
	}
	return $row
    }
    method getCol {x y} {
	set col {}
	for {set y 0} {$y<9} {incr y} {
	    lappend col [my get $x $y]
	}
	return $col
    }
    method getRegion {x y} {
	set xR [expr {($x/3)*3}]
	set yR [expr {($y/3)*3}]
	set regn {}
	for {set x $xR} {$x < $xR+3} {incr x} {
	    for {set y $yR} {$y < $yR+3} {incr y} {
		lappend regn [my get $x $y]
	    }
	}
	return $regn
    }
}

# SudokuSolver inherits from Sudoku, and adds the ability to filter
# possibilities for a square by looking at all the squares in the row, column,
# and region that the square is a part of. The method 'solve' contains a list
# of rule-objects to use, and iterates over each square on the board, applying
# each rule sequentially until the square is allocated.

oo::class create SudokuSolver {
    superclass Sudoku
    method validchoices {x y} {
	if {[my get $x $y] ne {}} {
	    return [my get $x $y]
	}

	set row [my getRow $x $y]
	set col [my getCol $x $y]
	set regn [my getRegion $x $y]
	set eliminate [list {*}$row {*}$col {*}$regn]
	set eliminate [lsearch -all -inline -not $eliminate {}]
	set eliminate [lsort -unique $eliminate]

	set choices {}
	for {set c 1} {$c < 10} {incr c} {
	    if {$c ni $eliminate} {
		lappend choices $c
	    }
	}
	if {[llength $choices]==0} {
	    error "No choices left for square $x,$y"
	}
	return $choices
    }
    method completion {} {
	return [expr {
	    81-[llength [lsearch -all -inline [join [my dump]] {}]]
	}]
    }
    method solve {} {
	foreach ruleClass [info class subclass Rule] {
	    lappend rules [$ruleClass new]
	}

	while {1} {
	    set begin [my completion]
	    for {set y 0} {$y < 9} {incr y} {
		for {set x 0} {$x < 9} {incr x} {
		    if {[my get $x $y] eq ""} {
			foreach rule $rules {
			    set c [$rule solve [self] $x $y]
			    if {$c} {
				my set $x $y $c
				my Log "[info object class $rule] solved [self] at $x,$y for $c"
				break
			    }
			}
		    }
		}
	    }
	    set end [my completion]
	    if {$end==81} {
		my Log "Finished solving!"
		break
	    } elseif {$begin==$end} {
		my Log "A round finished without solving any squares, giving up."
		break
	    }
	}
	foreach rule $rules {
	    $rule destroy
	}
    }
}

# Rule is the template for the rules used in Solver. The other rule-objects
# apply their logic to the values passed in and return either '0' or a number
# to allocate to the requested square.
oo::class create Rule {
    method solve {hSudoku x y} {
	if {![info object isa typeof $hSudoku SudokuSolver]} {
	    error "hSudoku must be an instance of class SudokuSolver."
	}

	tailcall my Solve $hSudoku $x $y [$hSudoku validchoices $x $y]
    }
}

# Get all the allocated numbers for each square in the the row, column, and
# region containing $x,$y. If there is only one unallocated number among all
# three groups, it must be allocated at $x,$y
oo::class create RuleOnlyChoice {
    superclass Rule
    method Solve {hSudoku x y choices} {
	if {[llength $choices]==1} {
	    return $choices
	} else {
	    return 0
	}
    }
}

# Test each column to determine if $choice is an invalid choice for all other
# columns in row $X. If it is, it must only go in square $x,$y.
oo::class create RuleColumnChoice {
    superclass Rule
    method Solve {hSudoku x y choices} {
	foreach choice $choices {
	    set failed 0
	    for {set x2 0} {$x2<9} {incr x2} {
		if {$x2 != $x && $choice in [$hSudoku validchoices $x2 $y]} {
		    set failed 1
		    break
		}
	    }
	    if {!$failed} {return $choice}
	}
	return 0
    }
}

# Test each row to determine if $choice is an invalid choice for all other
# rows in column $y. If it is, it must only go in square $x,$y.
oo::class create RuleRowChoice {
    superclass Rule
    method Solve {hSudoku x y choices} {
	foreach choice $choices {
	    set failed 0
	    for {set y2 0} {$y2<9} {incr y2} {
		if {$y2 != $y && $choice in [$hSudoku validchoices $x $y2]} {
		    set failed 1
		    break
		}
	    }
	    if {!$failed} {return $choice}
	}
	return 0
    }
}

# Test each square in the region occupied by $x,$y to determine if $choice is
# an invalid choice for all other squares in that region. If it is, it must
# only go in square $x,$y.
oo::class create RuleRegionChoice {
    superclass Rule
    method Solve {hSudoku x y choices} {
	foreach choice $choices {
	    set failed 0
	    set regnX [expr {($x/3)*3}]
	    set regnY [expr {($y/3)*3}]
	    for {set y2 $regnY} {$y2 < $regnY+3} {incr y2} {
		for {set x2 $regnX} {$x2 < $regnX+3} {incr x2} {
		    if {
			($x2!=$x || $y2!=$y)
			&& $choice in [$hSudoku validchoices $x2 $y2]
		    } then {
			set failed 1
			break
		    }
		}
	    }
	    if {!$failed} {return $choice}
	}
	return 0
    }
}
```

Demonstration code:

```tcl
SudokuSolver create sudoku
sudoku load {
    {3 9 4    @ @ 2    6 7 @}
    {@ @ @    3 @ @    4 @ @}
    {5 @ @    6 9 @    @ 2 @}

    {@ 4 5    @ @ @    9 @ @}
    {6 @ @    @ @ @    @ @ 7}
    {@ @ 7    @ @ @    5 8 @}

    {@ 1 @    @ 6 7    @ @ 8}
    {@ @ 9    @ @ 8    @ @ @}
    {@ 2 6    4 @ @    7 3 5}
}
sudoku solve
# Simple pretty-printer for completed sudokus
puts +-----+-----+-----+
foreach line [sudoku dump] postline {0 0 1 0 0 1 0 0 1} {
    puts |[lrange $line 0 2]|[lrange $line 3 5]|[lrange $line 6 8]|
    if {$postline} {
	puts +-----+-----+-----+
    }
}
sudoku destroy
```

{{out}}

```txt
+-----+-----+-----+
|3 9 4|8 5 2|6 7 1|
|2 6 8|3 7 1|4 5 9|
|5 7 1|6 9 4|8 2 3|
+-----+-----+-----+
|1 4 5|7 8 3|9 6 2|
|6 8 2|9 4 5|3 1 7|
|9 3 7|1 2 6|5 8 4|
+-----+-----+-----+
|4 1 3|5 6 7|2 9 8|
|7 5 9|2 3 8|1 4 6|
|8 2 6|4 1 9|7 3 5|
+-----+-----+-----+
```

If we'd added a logger method (after creating the <code>sudoku</code> object but before running the solver) like this:

```tcl
oo::objdefine sudoku method Log msg {puts $msg}
```

Then this additional logging output would have been produced prior to the result being printed:

```txt
::RuleOnlyChoice solved ::sudoku at 8,0 for 1
::RuleColumnChoice solved ::sudoku at 1,1 for 6
::RuleRegionChoice solved ::sudoku at 4,1 for 7
::RuleRowChoice solved ::sudoku at 7,1 for 5
::RuleOnlyChoice solved ::sudoku at 8,1 for 9
::RuleColumnChoice solved ::sudoku at 1,2 for 7
::RuleColumnChoice solved ::sudoku at 5,2 for 4
::RuleRowChoice solved ::sudoku at 6,2 for 8
::RuleOnlyChoice solved ::sudoku at 8,2 for 3
::RuleColumnChoice solved ::sudoku at 3,3 for 7
::RuleRowChoice solved ::sudoku at 1,4 for 8
::RuleRowChoice solved ::sudoku at 5,4 for 5
::RuleRowChoice solved ::sudoku at 6,4 for 3
::RuleRowChoice solved ::sudoku at 0,5 for 9
::RuleOnlyChoice solved ::sudoku at 1,5 for 3
::RuleOnlyChoice solved ::sudoku at 0,6 for 4
::RuleOnlyChoice solved ::sudoku at 2,6 for 3
::RuleColumnChoice solved ::sudoku at 3,6 for 5
::RuleOnlyChoice solved ::sudoku at 6,6 for 2
::RuleOnlyChoice solved ::sudoku at 7,6 for 9
::RuleOnlyChoice solved ::sudoku at 0,7 for 7
::RuleOnlyChoice solved ::sudoku at 1,7 for 5
::RuleColumnChoice solved ::sudoku at 4,7 for 3
::RuleOnlyChoice solved ::sudoku at 6,7 for 1
::RuleOnlyChoice solved ::sudoku at 0,8 for 8
::RuleOnlyChoice solved ::sudoku at 4,8 for 1
::RuleOnlyChoice solved ::sudoku at 5,8 for 9
::RuleOnlyChoice solved ::sudoku at 3,0 for 8
::RuleOnlyChoice solved ::sudoku at 4,0 for 5
::RuleColumnChoice solved ::sudoku at 2,1 for 8
::RuleOnlyChoice solved ::sudoku at 5,1 for 1
::RuleOnlyChoice solved ::sudoku at 2,2 for 1
::RuleRowChoice solved ::sudoku at 0,3 for 1
::RuleColumnChoice solved ::sudoku at 4,3 for 8
::RuleColumnChoice solved ::sudoku at 5,3 for 3
::RuleOnlyChoice solved ::sudoku at 7,3 for 6
::RuleOnlyChoice solved ::sudoku at 8,3 for 2
::RuleOnlyChoice solved ::sudoku at 2,4 for 2
::RuleColumnChoice solved ::sudoku at 3,4 for 9
::RuleOnlyChoice solved ::sudoku at 4,4 for 4
::RuleOnlyChoice solved ::sudoku at 7,4 for 1
::RuleColumnChoice solved ::sudoku at 3,5 for 1
::RuleOnlyChoice solved ::sudoku at 4,5 for 2
::RuleOnlyChoice solved ::sudoku at 5,5 for 6
::RuleOnlyChoice solved ::sudoku at 8,5 for 4
::RuleOnlyChoice solved ::sudoku at 3,7 for 2
::RuleOnlyChoice solved ::sudoku at 7,7 for 4
::RuleOnlyChoice solved ::sudoku at 8,7 for 6
::RuleOnlyChoice solved ::sudoku at 0,1 for 2
Finished solving!
```



## Ursala


```Ursala
#import std
#import nat

sudoku =

@FL mat0+ block3+ mat` *+ block3*+ block9+ -+
   ~&rSL+ (psort (nleq+)* <~&blrl,~&blrr>)+ ~&arg^& -+
      ~&al?\~&ar ~&aa^&~&afahPRPfafatPJPRY+ ~&farlthlriNCSPDPDrlCS2DlrTS2J,
      ^|J/~& ~&rt!=+ ^= ~&s+ ~&H(
         -+.|=&lrr;,|=&lrl;,|=&ll;+-,
         ~&rgg&& ~&irtPFXlrjrXPS; ~&lrK2tkZ2g&& ~&llrSL2rDrlPrrPljXSPTSL)+-,
   //~&p ^|DlrDSLlrlPXrrPDSL(~&,num*+ rep2 block3)*= num block27 ~&iiK0 iota9,
   * `0?=\~&iNC ! ~&t digits+-
```

test program:

```Ursala
#show+

example =

sudoku

-[
394002670
000300400
500690020
045000900
600000007
007000580
010067008
009008000
026400735]-
```

{{out}}

```txt

394 852 671
268 371 459
571 694 823

145 783 962
682 945 317
937 126 584

413 567 298
759 238 146
826 419 735

```


=={{Header|VBA}}==
{{trans|Fortran}}

```VB
Dim grid(9, 9)
Dim gridSolved(9, 9)

Public Sub Solve(i, j)
  If i > 9 Then
    'exit with gridSolved = Grid
    For r = 1 To 9
      For c = 1 To 9
        gridSolved(r, c) = grid(r, c)
      Next c
    Next r
    Exit Sub
  End If
  For n = 1 To 9
    If isSafe(i, j, n) Then
      nTmp = grid(i, j)
      grid(i, j) = n
      If j = 9 Then
        Solve i + 1, 1
      Else
        Solve i, j + 1
      End If
      grid(i, j) = nTmp
    End If
  Next n
End Sub

Public Function isSafe(i, j, n) As Boolean
Dim iMin As Integer
Dim jMin As Integer

If grid(i, j) <> 0 Then
  isSafe = (grid(i, j) = n)
  Exit Function
End If

'grid(i,j) is an empty cell. Check if n is OK
'first check the row i
For c = 1 To 9
  If grid(i, c) = n Then
    isSafe = False
    Exit Function
  End If
Next c

'now check the column j
For r = 1 To 9
 If grid(r, j) = n Then
   isSafe = False
   Exit Function
 End If
Next r

'finally, check the 3x3 subsquare containing grid(i,j)
iMin = 1 + 3 * Int((i - 1) / 3)
jMin = 1 + 3 * Int((j - 1) / 3)
For r = iMin To iMin + 2
  For c = jMin To jMin + 2
    If grid(r, c) = n Then
      isSafe = False
      Exit Function
    End If
  Next c
Next r

'all tests were OK
isSafe = True
End Function

Public Sub Sudoku()
  'main routine
  'to use, fill in the grid and
  'type "Sudoku" in the Immediate panel of the Visual Basic for Applications window

  Dim s(9) As String

  'initialise grid using 9 strings,one per row
  s(1) = "001005070"
  s(2) = "920600000"
  s(3) = "008000600"
  s(4) = "090020401"
  s(5) = "000000000"
  s(6) = "304080090"
  s(7) = "007000300"
  s(8) = "000007069"
  s(9) = "010800700"
  For i = 1 To 9
    For j = 1 To 9
      grid(i, j) = Int(Val(Mid$(s(i), j, 1)))
    Next j
  Next i
  'solve it!
  Solve 1, 1
  'print solution
  Debug.Print "Solution:"
  For i = 1 To 9
    For j = 1 To 9
      Debug.Print Format$(gridSolved(i, j)); " ";
    Next j
    Debug.Print
  Next i
End Sub
```

{{out}}

```txt

Sudoku
Solution:
6 3 1 2 4 5 9 7 8
9 2 5 6 7 8 1 4 3
4 7 8 3 1 9 6 5 2
7 9 6 5 2 3 4 8 1
1 8 2 9 6 4 5 3 7
3 5 4 7 8 1 2 9 6
8 6 7 4 9 2 3 1 5
2 4 3 1 5 7 8 6 9
5 1 9 8 3 6 7 2 4

```



## VBScript

{{trans|VBA}}
To run in console mode with cscript.

```vb
Dim grid(9, 9)
Dim gridSolved(9, 9)

Public Sub Solve(i, j)
    If i > 9 Then
        'exit with gridSolved = Grid
        For r = 1 To 9
	    For c = 1 To 9
	        gridSolved(r, c) = grid(r, c)
	    Next 'c
        Next 'r
        Exit Sub
    End If
    For n = 1 To 9
        If isSafe(i, j, n) Then
          nTmp = grid(i, j)
          grid(i, j) = n
          If j = 9 Then
                Solve i + 1, 1
          Else
                Solve i, j + 1
          End If
          grid(i, j) = nTmp
        End If
    Next 'n
End Sub 'Solve

Public Function isSafe(i, j, n)
    If grid(i, j) <> 0 Then
        isSafe = (grid(i, j) = n)
        Exit Function
    End If
    'grid(i,j) is an empty cell. Check if n is OK
    'first check the row i
    For c = 1 To 9
        If grid(i, c) = n Then
            isSafe = False
            Exit Function
        End If
    Next 'c
    'now check the column j
    For r = 1 To 9
        If grid(r, j) = n Then
            isSafe = False
            Exit Function
        End If
    Next 'r
    'finally, check the 3x3 subsquare containing grid(i,j)
    iMin = 1 + 3 * Int((i - 1) / 3)
    jMin = 1 + 3 * Int((j - 1) / 3)
    For r = iMin To iMin + 2
        For c = jMin To jMin + 2
            If grid(r, c) = n Then
                isSafe = False
                Exit Function
            End If
        Next 'c
    Next 'r
    'all tests were OK
    isSafe = True
End Function 'isSafe

Public Sub Sudoku()
    'main routine
   Dim s(9)
    s(1) = "001005070"
    s(2) = "920600000"
    s(3) = "008000600"
    s(4) = "090020401"
    s(5) = "000000000"
    s(6) = "304080090"
    s(7) = "007000300"
    s(8) = "000007069"
    s(9) = "010800700"
    For i = 1 To 9
        For j = 1 To 9
            grid(i, j) = Int(Mid(s(i), j, 1))
        Next 'j
    Next 'j
    'print problem
    Wscript.echo "Problem:"
    For i = 1 To 9
	    c=""
        For j = 1 To 9
            c=c & grid(i, j) & " "
        Next 'j
	    Wscript.echo c
    Next 'i
    'solve it!
    Solve 1, 1
    'print solution
    Wscript.echo "Solution:"
    For i = 1 To 9
	    c=""
        For j = 1 To 9
            c=c & gridSolved(i, j) & " "
        Next 'j
	    Wscript.echo c
    Next 'i
End Sub 'Sudoku

Call sudoku
```

{{out}}

```txt
Problem:
0 0 1 0 0 5 0 7 0
9 2 0 6 0 0 0 0 0
0 0 8 0 0 0 6 0 0
0 9 0 0 2 0 4 0 1
0 0 0 0 0 0 0 0 0
3 0 4 0 8 0 0 9 0
0 0 7 0 0 0 3 0 0
0 0 0 0 0 7 0 6 9
0 1 0 8 0 0 7 0 0
Solution:
6 3 1 2 4 5 9 7 8
9 2 5 6 7 8 1 4 3
4 7 8 3 1 9 6 5 2
7 9 6 5 2 3 4 8 1
1 8 2 9 6 4 5 3 7
3 5 4 7 8 1 2 9 6
8 6 7 4 9 2 3 1 5
2 4 3 1 5 7 8 6 9
5 1 9 8 3 6 7 2 4
```



## XPL0

This is a translation of the C example, but with a solution that
can be verified by several other examples.
{{trans|C}}

```XPL0
code    ChOut=8, CrLf=9, IntOut=11, Text=12;

proc    Show(X);
char    X;
int     I, J;
[for I:= 0 to 8 do
        [if rem(I/3) = 0 then CrLf(0);
        for J:= 0 to 8 do
                [if rem(J/3) = 0 then ChOut(0, ^ );
                ChOut(0, ^ );  IntOut(0, X(0));
                X:= X+1;
                ];
        CrLf(0);
        ];
];

func    TryCell(X, Pos);
char    X;
int     Pos;
int     Row, Col, I, J, Used;
[Row:= Pos/9;
Col:= rem(0);
Used:= 0;

if Pos = 81 then return true;
if X(Pos) then return TryCell(X, Pos+1);

for I:= 0 to 8 do Used:= Used ! 1 << (X(I*9+Col)-1);
for J:= 0 to 8 do Used:= Used ! 1 << (X(Row*9+J)-1);

Row:= Row/3*3;
Col:= Col/3*3;
for I:= Row to Row+2 do
        for J:= Col to Col+2 do
                Used:= Used ! 1 << (X(I*9+J)-1);

for I:= 1 to 9 do
        [X(Pos):= I;
        if (Used&1)=0 & TryCell(X, Pos+1) then return true;
        Used:= Used>>1;
        ];
X(Pos):= 0;
return false;
];

proc    Solve(S);
char    S;
int     I, J, C;
char    X(81);
[J:= 0;
for I:= 0 to 80 do
        [repeat C:= S(J);
                J:= J+1;
        until   C>=^1 & C<=^9 ! C=^.;
        X(I):= if C=^. then 0 else C-^0;
        ];

if TryCell(X, 0) then Show(X)
else Text(0, "No solution");
];

[Solve("394 ..2 67.
        ... 3.. 4..
        5.. 69. .2.
        .45 ... 9..
        6.. ... ..7
        ..7 ... 58.
        .1. .67 ..8
        ..9 ..8 ...
        .26 4.. 735 ");
]
```


{{out}}

```txt

  3 9 4  8 5 2  6 7 1
  2 6 8  3 7 1  4 5 9
  5 7 1  6 9 4  8 2 3

  1 4 5  7 8 3  9 6 2
  6 8 2  9 4 5  3 1 7
  9 3 7  1 2 6  5 8 4

  4 1 3  5 6 7  2 9 8
  7 5 9  2 3 8  1 4 6
  8 2 6  4 1 9  7 3 5

```



## zkl

{{trans|C}} Note: Unlike in the C solution, 1<<-1 is defined (as 0).

```zkl
fcn trycell(sdku,pos=0){
   row,col:=pos/9, pos%9;

   if(pos==81)   return(True);
   if(sdku[pos]) return(trycell(sdku, pos + 1));

   used:=0;
   foreach r in (9){ used=used.bitOr((1).shiftLeft(sdku[r*9 + col] - 1)) }
   foreach c in (9){ used=used.bitOr((1).shiftLeft(sdku[row*9 + c] - 1)) }

   row,col = row/3*3, col/3*3;
   foreach r,c in ([row..row+2], [col..col+2])
      { used=used.bitOr((1).shiftLeft(sdku[r*9 + c] - 1)) }

   sdku[pos]=1; while(sdku[pos]<=9){
      if(used.isEven and trycell(sdku, pos + 1)) return(True);
      sdku[pos]+=1; used/=2;
   }

   sdku[pos]=0;
   return(False);
}
```


```zkl
problem:=
#<<<
"       5 3 0 0 7 0 0 0 0
        6 0 0 1 9 5 0 0 0
        0 9 8 0 0 0 0 6 0
        8 0 0 0 6 0 0 0 3
        4 0 0 8 0 3 0 0 1
        7 0 0 0 2 0 0 0 6
        0 6 0 0 0 0 2 8 0
        0 0 0 4 1 9 0 0 5
        0 0 0 0 8 0 0 7 9";
#<<<
s:=problem.split().apply("toInt").copy();  // writable list of 81 ints
trycell(s).println();
println("+-----+-----+-----+");
foreach n in (3){
   s[n*27,27].pump(Console.println,T(Void.Read,8),("| " + "%s%s%s | "*3).fmt); // 3 lines
   println("+-----+-----+-----+");
}
```

{{out}}

```txt

True
+-----+-----+-----+
| 534 | 678 | 912 |
| 672 | 195 | 348 |
| 198 | 342 | 567 |
+-----+-----+-----+
| 859 | 761 | 423 |
| 426 | 853 | 791 |
| 713 | 924 | 856 |
+-----+-----+-----+
| 961 | 537 | 284 |
| 287 | 419 | 635 |
| 345 | 286 | 179 |
+-----+-----+-----+

```


{{omit from|GUISS}}
