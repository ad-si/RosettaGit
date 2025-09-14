+++
title = "Wireworld"
description = ""
date = 2018-12-06T01:06:20Z
aliases = []
[extra]
id = 4827
[taxonomies]
categories = ["task", "Games"]
tags = []
languages = [
  "ada",
  "algol_68",
  "autohotkey",
  "autoit",
  "bbc_basic",
  "c",
  "ceylon",
  "common_lisp",
  "cpp",
  "csharp",
  "d",
  "elena",
  "elixir",
  "forth",
  "fortran",
  "gml",
  "go",
  "haskell",
  "j",
  "java",
  "javascript",
  "jq",
  "julia",
  "liberty_basic",
  "logo",
  "lua",
  "mathematica",
  "nim",
  "ocaml",
  "oz",
  "pari_gp",
  "perl",
  "perl_6",
  "phix",
  "php",
  "picolisp",
  "purebasic",
  "python",
  "racket",
  "rexx",
  "ruby",
  "sidef",
  "smalltalk",
  "standard_ml",
  "tcl",
  "ursala",
  "xpl0",
  "yabasic",
]
+++

[[wp:Wireworld|Wireworld]] is a cellular automaton with some similarities to [[Conway's Game of Life]].

It is capable of doing sophisticated computations with appropriate programs
(it is actually [[wp:Turing-complete|Turing complete]]),
and is much simpler to program for.

A Wireworld arena consists of a Cartesian grid of cells,
each of which can be in one of four states.
All cell transitions happen simultaneously.

The cell transition rules are this:
{| class=wikitable
|-
! Input State
! Output State
! Condition
|-
| <tt>empty</tt>
| <tt>empty</tt>
|
|-
| <tt>electron head </tt>
| <tt>electron tail </tt>
|
|-
| <tt>electron tail </tt>
| <tt>conductor</tt>
|
|-
| valign=top | <tt>conductor</tt>
| valign=top | <tt>electron head </tt>
| if 1 or 2 cells in the [[wp:Moore neighborhood|neighborhood]] of the cell are in the state “<tt>electron head</tt>”
|-
| <tt>conductor</tt>
| <tt>conductor</tt>
| otherwise
|}


## Task

Create a program that reads a Wireworld program from a file and displays an animation of the processing. Here is a sample description file (using "<tt>H</tt>" for an electron head, "<tt>t</tt>" for a tail, "<tt>.</tt>" for a conductor and a space for empty) you may wish to test with, which demonstrates two cycle-3 generators and an inhibit gate:

```txt

tH.........
.   .
   ...
.   .
Ht.. ......

```

While text-only implementations of this task are possible, mapping cells to pixels is advisable if you wish to be able to display large designs. The logic is not significantly more complex.





## Ada


```Ada
with Ada.Text_IO;  use Ada.Text_IO;

procedure Test_Wireworld is
   type Cell is (' ', 'H', 't', '.');
   type Board is array (Positive range <>, Positive range <>) of Cell;
      -- Perform one transition of the cellular automation
   procedure Wireworld (State : in out Board) is
      function "abs" (Left : Cell) return Natural is
      begin
         if Left = 'H' then
            return 1;
         else
            return 0;
         end if;
      end "abs";
      Above   : array (State'Range (2)) of Cell := (others => ' ');
      Left    : Cell := ' ';
      Current : Cell;
   begin
      for I in State'First (1) + 1..State'Last (1) - 1 loop
         for J in State'First (2) + 1..State'Last (2) - 1 loop
            Current := State (I, J);
            case Current is
               when ' ' =>
                  null;
               when 'H' =>
                  State (I, J) := 't';
               when 't' =>
                  State (I, J) := '.';
               when '.' =>
                  if abs Above (       J - 1) + abs Above (       J) + abs Above (       J + 1) +
                     abs Left                                        + abs State (I,     J + 1) +
                     abs State (I + 1, J - 1) + abs State (I + 1, J) + abs State (I + 1, J + 1)
                  in 1..2 then
                     State (I, J) := 'H';
                  else
                     State (I, J) := '.';
                  end if;
            end case;
            Above (J - 1) := Left;
            Left := Current;
         end loop;
      end loop;
   end Wireworld;
      -- Print state of the automation
   procedure Put (State : Board) is
   begin
      for I in State'First (1) + 1..State'Last (1) - 1 loop
         for J in State'First (2) + 1..State'Last (2) - 1 loop
            case State (I, J) is
               when ' ' => Put (' ');
               when 'H' => Put ('H');
               when 't' => Put ('t');
               when '.' => Put ('.');
            end case;
         end loop;
         New_Line;
      end loop;
   end Put;
   Oscillator : Board := ("         ", "  tH     ", " .  .... ", "  ..     ", "         ");
begin
   for Step in 0..9 loop
      Put_Line ("Step" & Integer'Image (Step) & " ---------"); Put (Oscillator);
      Wireworld (Oscillator);
   end loop;
end Test_Wireworld;
```

The solution assumes that the border of the board is empty. When transition is performed these cells are not changed. Automation transition is an in-place operation that allocates memory for to keep one row of the board size.
<pre style="height:30ex;overflow:scroll">
Step 0 ---------
 tH
.  ....
 ..
Step 1 ---------
 .t
.  H...
 ..
Step 2 ---------
 ..
.  tH..
 .H
Step 3 ---------
 ..
.  .tH.
 Ht
Step 4 ---------
 ..
H  ..tH
 t.
Step 5 ---------
 H.
t  ...t
 ..
Step 6 ---------
 tH
.  ....
 ..
Step 7 ---------
 .t
.  H...
 ..
Step 8 ---------
 ..
.  tH..
 .H
Step 9 ---------
 ..
.  .tH.
 Ht

```



## ALGOL 68

{{trans|python}} - note: This specimen retains the original python coding style.
```algol68
CO
Wireworld implementation.
CO

PROC exception = ([]STRING args)VOID:(
  putf(stand error, ($"Exception"$, $", "g$, args, $l$));
  stop
);

PROC assertion error = (STRING message)VOID:exception(("assertion error", message));

MODE CELL = CHAR;
MODE WORLD = FLEX[0, 0]CELL;
CELL head="H", tail="t", conductor=".", empty = " ";
STRING all states := empty;

BOOL wrap = FALSE; # is the world round? #

STRING nl := REPR 10;

STRING in string :=
  "tH........."+nl+
  ".   ."+nl+
  "   ..."+nl+
  ".   ."+nl+
  "Ht.. ......"+nl
;

OP +:= = (REF FLEX[]FLEX[]CELL lines, FLEX[]CELL line)VOID:(
      [UPB lines + 1]FLEX[0]CELL new lines;
      new lines[:UPB lines]:=lines;
      lines := new lines;
      lines[UPB lines]:=line
);

PROC read file = (REF FILE in file)WORLD: (
    # file > initial world configuration" #
    FLEX[0]CELL line;
    FLEX[0]FLEX[0]CELL lines;
    INT upb x:=0, upb y := 0;
    BEGIN
      # on physical file end(in file, exit read line); #
      make term(in file, nl);
      FOR x TO 5 DO
        get(in file, (line, new line));
        upb x := x;
        IF UPB line > upb y THEN upb y := UPB line FI;
        lines +:= line
      OD;
    exit read line: SKIP
    END;
    [upb x, upb y]CELL out;
    FOR x TO UPB out DO
      out[x,]:=lines[x]+" "*(upb y-UPB lines[x])
    OD;
    out
);

PROC new cell = (WORLD current world, INT x, y)CELL: (
    CELL istate := current world[x, y];
    IF INT pos; char in string (istate, pos, all states); pos IS REF INT(NIL) THEN
        assertion error("Wireworld cell set to unknown value "+istate) FI;
    IF istate = head THEN
        tail
    ELIF istate = tail THEN
        conductor
    ELIF istate = empty THEN
        empty
    ELSE # istate = conductor #
        [][]INT dxy list = ( (-1,-1), (-1,+0), (-1,+1),
                             (+0,-1),          (+0,+1),
                             (+1,-1), (+1,+0), (+1,+1) );
        INT n := 0;
        FOR enum dxy TO UPB dxy list DO
          []INT dxy = dxy list[enum dxy];
          IF wrap THEN
            INT px = ( x + dxy[1] - 1 ) MOD 1 UPB current world + 1;
            INT py = ( y + dxy[2] - 1 ) MOD 2 UPB current world + 1;
            n +:= ABS (current world[px, py] = head)
          ELSE
            INT px = x + dxy[1];
            INT py = y + dxy[2];
            IF px >= 1 LWB current world AND px <= 1 UPB current world AND
               py >= 2 LWB current world AND py <= 2 UPB current world THEN
                 n +:= ABS (current world[px, py] = head)
            FI
          FI
        OD;
        IF 1 <= n AND n <= 2 THEN head ELSE conductor FI
    FI
);

PROC next gen = (WORLD world)WORLD:(
    # compute next generation of wireworld #
    WORLD new world := world;
    FOR x TO 1 UPB world DO
        FOR y TO 2 UPB world DO
            new world[x,y] := new cell(world, x, y)
        OD
    OD;
    new world
);

PROC world2string = (WORLD world) STRING:(
    STRING out:="";
    FOR x TO UPB world DO
      out +:= world[x,]+nl
    OD;
    out
);

FILE in file;
associate(in file, in string);

WORLD ww := read file(in file);
close(in file);

FOR gen TO 10 DO
    printf ( ($lg(-3)" "$, gen-1,  $g$,"="* (2 UPB ww-4), $l$));
    print ( world2string(ww) );
    ww := next gen(ww)
OD
```

<pre style="height:45ex;overflow:scroll">
  0
### =

tH.........
.   .
   ...
.   .
Ht.. ......

  1
### =

.tH........
H   .
   ...
H   .
t... ......

  2
### =

H.tH.......
t   .
   ...
t   .
.H.. ......

  3
### =

tH.tH......
.   H
   ...
.   .
HtH. ......

  4
### =

.tH.tH.....
H   t
   HHH
H   .
t.tH ......

  5
### =

H.tH.tH....
t   .
   ttt
t   .
.H.t ......

  6
### =

tH.tH.tH...
.   H
   ...
.   .
HtH. ......

  7
### =

.tH.tH.tH..
H   t
   HHH
H   .
t.tH ......

  8
### =

H.tH.tH.tH.
t   .
   ttt
t   .
.H.t ......

  9
### =

tH.tH.tH.tH
.   H
   ...
.   .
HtH. ......

```



## AutoHotkey

[http://i.imgur.com/FsLB8i1.gif Demo gif] - Link, since uploads seem to be disabled currently.
```AutoHotkey
#SingleInstance, Force
#NoEnv
SetBatchLines, -1
File := "Wireworld.txt"
CellSize := 20
CellSize2 := CellSize - 2
C1 := 0xff000000
C2 := 0xff0066ff
C3 := 0xffd40055
C4 := 0xffffcc00

if (!FileExist(File)) {
	MsgBox, % "File(" File ") is not present."
	ExitApp
}

; Uncomment if Gdip.ahk is not in your standard library
; #Include, Gdip.ahk
If !pToken := Gdip_Startup(){
	MsgBox, 48, Gdiplus error!, Gdiplus failed to start. Please ensure you have Gdiplus on your system.
	ExitApp
}
OnExit, Exit

A := [], Width := 0
Loop, Read, % File
{
	Row := A_Index
	Loop, Parse, A_LoopReadLine
	{
		if (A_Index > Width)
			Width := A_Index
		if (A_LoopField = A_Space)
			continue
		A[Row, A_Index] := A_LoopField
	}
}

Width := Width * CellSize + 2 * CellSize
, Height := Row * CellSize + 2 * CellSize
, Row := ""
, TopLeftX := (A_ScreenWidth - Width) // 2
, TopLeftY := (A_ScreenHeight - Height) // 2

Gui, 1: -Caption +E0x80000 +LastFound +AlwaysOnTop +ToolWindow +OwnDialogs
Gui, 1: Show, NA

hwnd1 := WinExist()
, hbm := CreateDIBSection(Width, Height)
, hdc := CreateCompatibleDC()
, obm := SelectObject(hdc, hbm)
, G := Gdip_GraphicsFromHDC(hdc)
, Gdip_SetSmoothingMode(G, 4)

Loop {
	pBrush := Gdip_BrushCreateSolid(C1)
	, Gdip_FillRectangle(G, pBrush, 0, 0, Width, Height)
	, Gdip_DeleteBrush(pBrush)

	for RowNum, Row in A
		for CellNum, Cell in Row
			C := Cell = "H" ? C2 : Cell = "t" ? C3 : C4
			, pBrush := Gdip_BrushCreateSolid(C)
			, Gdip_FillRectangle(G, pBrush, CellNum * CellSize + 1, RowNum * CellSize - 2, CellSize2, CellSize2)
			, Gdip_DeleteBrush(pBrush)


	UpdateLayeredWindow(hwnd1, hdc, TopLeftX, TopLeftY, Width, Height)
	, Gdip_GraphicsClear(G)
	, A := NextState(A)
	Sleep, 600
}

NextState(A) {
	B := {}
	for RowNum, Row in A {
		for CellNum, Cell in Row {
			if (Cell = "H")
				B[RowNum, CellNum] := "t"
			else if (Cell = "t")
				B[RowNum, CellNum] := "."
			else if (Cell = ".") {
				H_Count := 0
				Loop 3 {
					Y := RowNum - 2 + A_Index
					Loop, 3 {
						X := CellNum - 2 + A_Index
						if (A[Y, X] = "H")
							H_Count++
					}
				}
				if (H_Count = 1 || H_Count = 2)
					B[RowNum, CellNum] := "H"
				else
					B[RowNum, CellNum] := "."
			}
		}
	}
	return B
}

p::Pause

Esc::
Exit:
Gdip_Shutdown(pToken)
ExitApp
```



## AutoIt



```autoit

$ww = ""
$ww &= "tH........." & @CR
$ww &= ".   .      " & @CR
$ww &= "   ...     " & @CR
$ww &= ".   .      " & @CR
$ww &= "Ht.. ......"
$rows = StringSplit($ww, @CR)
$cols = StringSplit($rows[1], "")
Global $Wireworldarray[$rows[0]][$cols[0]]
For $I = 1 To $rows[0]
	$cols = StringSplit($rows[$I], "")
	For $k = 1 To $cols[0]
		$Wireworldarray[$I - 1][$k - 1] = $cols[$k]
	Next
Next
Wireworld($Wireworldarray)
Func Wireworld($array)
	Local $labelarray = $array
	Local $Top = 0, $Left = 0
	$hFui = GUICreate("Wireworld", UBound($array, 2) * 25, UBound($array) * 25)
	For $I = 0 To UBound($array) - 1
		For $k = 0 To UBound($array, 2) - 1
			Switch $array[$I][$k]
				Case "t" ; Tail
					$labelarray[$I][$k] = GUICtrlCreateButton("", $Left, $Top, 25, 25)
					GUICtrlSetBkColor($labelarray[$I][$k], 0xFF0000)
				Case "h" ; Head
					$labelarray[$I][$k] = GUICtrlCreateButton("", $Left, $Top, 25, 25)
					GUICtrlSetBkColor($labelarray[$I][$k], 0x0000FF)
				Case "." ; Conductor
					$labelarray[$I][$k] = GUICtrlCreateButton("", $Left, $Top, 25, 25)
					GUICtrlSetBkColor($labelarray[$I][$k], 0xFFFF00)
				Case " " ; Empty
					$labelarray[$I][$k] = GUICtrlCreateButton("", $Left, $Top, 25, 25)
					GUICtrlSetBkColor($labelarray[$I][$k], 0x000000)
			EndSwitch
			$Left += 25
		Next
		$Left = 0
		$Top += 25
	Next
	GUISetState()
	Local $nextsteparray = $array
	While 1
		$msg = GUIGetMsg()
		$array = $nextsteparray
		Sleep(250)
		For $I = 0 To UBound($array) - 1
			For $k = 0 To UBound($array, 2) - 1
				If $array[$I][$k] = " " Then ContinueLoop
				If $array[$I][$k] = "h" Then $nextsteparray[$I][$k] = "t"
				If $array[$I][$k] = "t" Then $nextsteparray[$I][$k] = "."
				If $array[$I][$k] = "." Then
					$counter = 0
					If $I - 1 >= 0 Then ; Top
						If $array[$I - 1][$k] = "h" Then $counter += 1
					EndIf
					If $k - 1 >= 0 Then ; left
						If $array[$I][$k - 1] = "h" Then $counter += 1
					EndIf
					If $I + 1 <= UBound($array) - 1 Then ; Bottom
						If $array[$I + 1][$k] = "h" Then $counter += 1
					EndIf
					If $k + 1 <= UBound($array, 2) - 1 Then ;Right
						If $array[$I][$k + 1] = "h" Then $counter += 1
					EndIf
					If $I - 1 >= 0 And $k - 1 >= 0 Then ; left Top
						If $array[$I - 1][$k - 1] = "h" Then $counter += 1
					EndIf
					If $I + 1 <= UBound($array) - 1 And $k + 1 <= UBound($array, 2) - 1 Then ; Right Bottom
						If $array[$I + 1][$k + 1] = "h" Then $counter += 1
					EndIf
					If $I + 1 <= UBound($array) - 1 And $k - 1 >= 0 Then ;Left Bottom
						If $array[$I + 1][$k - 1] = "h" Then $counter += 1
					EndIf
					If $I - 1 >= 0 And $k + 1 <= UBound($array, 2) - 1 Then ; Top Right
						If $array[$I - 1][$k + 1] = "h" Then $counter += 1
					EndIf
					If $counter = 1 Or $counter = 2 Then $nextsteparray[$I][$k] = "h"
				EndIf
			Next
		Next
		For $I = 0 To UBound($nextsteparray) - 1
			For $k = 0 To UBound($nextsteparray, 2) - 1
				Switch $nextsteparray[$I][$k]
					Case "t" ; Tail
						GUICtrlSetBkColor($labelarray[$I][$k], 0xFF0000)
					Case "h" ; Head
						GUICtrlSetBkColor($labelarray[$I][$k], 0x0000FF)
					Case "." ; Conductor
						GUICtrlSetBkColor($labelarray[$I][$k], 0xFFFF00)
					Case " " ; Empty
						GUICtrlSetBkColor($labelarray[$I][$k], 0x000000)
				EndSwitch
				$Left += 25
			Next
			$Left = 0
			$Top += 25
		Next
		If $msg = -3 Then Exit
	WEnd
EndFunc   ;==>Wireworld

```



## BBC BASIC

[[Image:wireworld_bbc.gif|right]]

```bbcbasic
      Size% = 20
      DIM P&(Size%-1,Size%-1), Q&(Size%-1,Size%-1)

      VDU 23,22,Size%*8;Size%*8;64,64,16,0
      OFF

      DATA "tH........."
      DATA ".   .      "
      DATA "   ...     "
      DATA ".   .      "
      DATA "Ht.. ......"

      FOR Y% = 12 TO 8 STEP -1
        READ A$
        FOR X% = 1 TO LEN(A$)
          P&(X%+4, Y%) = ASCMID$(A$, X%, 1) AND 15
        NEXT
      NEXT Y%

      COLOUR  8,0,0,255 : REM Electron head = blue
      COLOUR  4,255,0,0 : REM Electron tail = red
      COLOUR 14,255,200,0 : REM Conductor orange

      REPEAT
        FOR Y% = 1 TO Size%-2
          FOR X% = 1 TO Size%-2
            IF P&(X%,Y%)<>Q&(X%,Y%) GCOL P&(X%,Y%) : PLOT X%*16, Y%*16
            CASE P&(X%,Y%) OF
              WHEN 0: Q&(X%,Y%) = 0
              WHEN 8: Q&(X%,Y%) = 4
              WHEN 4: Q&(X%,Y%) = 14
              WHEN 14:
                T% = (P&(X%+1,Y%)=8) + (P&(X%+1,Y%+1)=8) + (P&(X%+1,Y%-1)=8) + \
                \    (P&(X%-1,Y%)=8) + (P&(X%-1,Y%+1)=8) + (P&(X%-1,Y%-1)=8) + \
                \    (P&(X%,Y%-1)=8) + (P&(X%,Y%+1)=8)
                IF T%=-1 OR T%=-2 THEN Q&(X%,Y%) = 8 ELSE Q&(X%,Y%) = 14
            ENDCASE
          NEXT
        NEXT Y%
        SWAP P&(), Q&()
        WAIT 50
      UNTIL FALSE
```



## C


For big graphics version, see: [[Wireworld/C]]


Text version with optional animation on POSIX systems:

Compile with <code>-D_POSIX_C_SOURCE=199309L</code> or greater to make <code>nanosleep</code> visible in <code><time.h></code>.


```c
/* 2009-09-27 <kaz@kylheku.com> */
#define ANIMATE_VT100_POSIX
#include <stdio.h>
#include <string.h>
#ifdef ANIMATE_VT100_POSIX
#include <time.h>
#endif

char world_7x14[2][512] = {
  {
    "+-----------+\n"
    "|tH.........|\n"
    "|.   .      |\n"
    "|   ...     |\n"
    "|.   .      |\n"
    "|Ht.. ......|\n"
    "+-----------+\n"
  }
};

void next_world(const char *in, char *out, int w, int h)
{
  int i;

  for (i = 0; i < w*h; i++) {
    switch (in[i]) {
    case ' ': out[i] = ' '; break;
    case 't': out[i] = '.'; break;
    case 'H': out[i] = 't'; break;
    case '.': {
      int hc = (in[i-w-1] == 'H') + (in[i-w] == 'H') + (in[i-w+1] == 'H') +
               (in[i-1]   == 'H')                    + (in[i+1]   == 'H') +
               (in[i+w-1] == 'H') + (in[i+w] == 'H') + (in[i+w+1] == 'H');
      out[i] = (hc == 1 || hc == 2) ? 'H' : '.';
      break;
    }
    default:
      out[i] = in[i];
    }
  }
  out[i] = in[i];
}

int main()
{
  int f;

  for (f = 0; ; f = 1 - f) {
    puts(world_7x14[f]);
    next_world(world_7x14[f], world_7x14[1-f], 14, 7);
#ifdef ANIMATE_VT100_POSIX
    printf("\x1b[%dA", 8);
    printf("\x1b[%dD", 14);
    {
      static const struct timespec ts = { 0, 100000000 };
      nanosleep(&ts, 0);
    }
#endif
  }

  return 0;
}
```



## C++

{{libheader|libggi}} (for graphics)

{{libheader|POSIX}} (for usleep)


```cpp
#include <ggi/ggi.h>
#include <set>
#include <map>
#include <utility>
#include <iostream>
#include <fstream>
#include <string>

#include <unistd.h> // for usleep

enum cell_type { none, wire, head, tail };

// *****************
// * display class *
// *****************

// this is just a small wrapper for the ggi interface

class display
{
public:
  display(int sizex, int sizey, int pixsizex, int pixsizey,
          ggi_color* colors);
  ~display()
  {
    ggiClose(visual);
    ggiExit();
  }

  void flush();
  bool keypressed() { return ggiKbhit(visual); }
  void clear();
  void putpixel(int x, int y, cell_type c);
private:
  ggi_visual_t visual;
  int size_x, size_y;
  int pixel_size_x, pixel_size_y;
  ggi_pixel pixels[4];
};

display::display(int sizex, int sizey, int pixsizex, int pixsizey,
                 ggi_color* colors):
  pixel_size_x(pixsizex),
  pixel_size_y(pixsizey)
{
  if (ggiInit() < 0)
  {
    std::cerr << "couldn't open ggi\n";
    exit(1);
  }

  visual = ggiOpen(NULL);
  if (!visual)
  {
    ggiPanic("couldn't open visual\n");
  }

  ggi_mode mode;
  if (ggiCheckGraphMode(visual, sizex, sizey,
                        GGI_AUTO, GGI_AUTO, GT_4BIT,
                        &mode) != 0)
  {
    if (GT_DEPTH(mode.graphtype) < 2) // we need 4 colors!
      ggiPanic("low-color displays are not supported!\n");
  }
  if (ggiSetMode(visual, &mode) != 0)
  {
    ggiPanic("couldn't set graph mode\n");
  }
  ggiAddFlags(visual, GGIFLAG_ASYNC);

  size_x = mode.virt.x;
  size_y = mode.virt.y;

  for (int i = 0; i < 4; ++i)
    pixels[i] = ggiMapColor(visual, colors+i);
}

void display::flush()
{
  // set the current display frame to the one we have drawn to
  ggiSetDisplayFrame(visual, ggiGetWriteFrame(visual));

  // flush the current visual
  ggiFlush(visual);

  // try to set a different frame for drawing (errors are ignored; if
  // setting the new frame fails, the current one will be drawn upon,
  // with the only adverse effect being some flickering).
  ggiSetWriteFrame(visual, 1-ggiGetDisplayFrame(visual));
}

void display::clear()
{
  ggiSetGCForeground(visual, pixels[0]);
  ggiDrawBox(visual, 0, 0, size_x, size_y);
}

void display::putpixel(int x, int y, cell_type cell)
{
  // this draws a logical pixel (i.e. a rectangle of size pixel_size_x
  // times pixel_size_y), not a physical pixel
  ggiSetGCForeground(visual, pixels[cell]);
  ggiDrawBox(visual,
             x*pixel_size_x, y*pixel_size_y,
             pixel_size_x, pixel_size_y);
}

// *****************
// * the wireworld *
// *****************

// initialized to an empty wireworld
class wireworld
{
public:
  void set(int posx, int posy, cell_type type);
  void draw(display& destination);
  void step();
private:
  typedef std::pair<int, int> position;
  typedef std::set<position> position_set;
  typedef position_set::iterator positer;
  position_set wires, heads, tails;
};

void wireworld::set(int posx, int posy, cell_type type)
{
  position p(posx, posy);
  wires.erase(p);
  heads.erase(p);
  tails.erase(p);
  switch(type)
  {
  case head:
    heads.insert(p);
    break;
  case tail:
    tails.insert(p);
    break;
  case wire:
    wires.insert(p);
    break;
  }
}

void wireworld::draw(display& destination)
{
  destination.clear();
  for (positer i = heads.begin(); i != heads.end(); ++i)
    destination.putpixel(i->first, i->second, head);
  for (positer i = tails.begin(); i != tails.end(); ++i)
    destination.putpixel(i->first, i->second, tail);
  for (positer i = wires.begin(); i != wires.end(); ++i)
    destination.putpixel(i->first, i->second, wire);
  destination.flush();
}

void wireworld::step()
{
  std::map<position, int> new_heads;
  for (positer i = heads.begin(); i != heads.end(); ++i)
    for (int dx = -1; dx <= 1; ++dx)
      for (int dy = -1; dy <= 1; ++dy)
      {
        position pos(i->first + dx, i->second + dy);
        if (wires.count(pos))
          new_heads[pos]++;
      }
  wires.insert(tails.begin(), tails.end());
  tails.swap(heads);
  heads.clear();
  for (std::map<position, int>::iterator i = new_heads.begin();
       i != new_heads.end();
       ++i)
  {
//     std::cout << i->second;
    if (i->second < 3)
    {
      wires.erase(i->first);
      heads.insert(i->first);
    }
  }
}

ggi_color colors[4] =
  {{ 0x0000, 0x0000, 0x0000 },  // background: black
   { 0x8000, 0x8000, 0x8000 },  // wire: white
   { 0xffff, 0xffff, 0x0000 },  // electron head: yellow
   { 0xffff, 0x0000, 0x0000 }}; // electron tail: red

int main(int argc, char* argv[])
{
  int display_x = 800;
  int display_y = 600;
  int pixel_x = 5;
  int pixel_y = 5;

  if (argc < 2)
  {
    std::cerr << "No file name given!\n";
    return 1;
  }

  // assume that the first argument is the name of a file to parse
  std::ifstream f(argv[1]);
  wireworld w;
  std::string line;
  int line_number = 0;
  while (std::getline(f, line))
  {
    for (int col = 0; col < line.size(); ++col)
    {
      switch (line[col])
      {
      case 'h': case 'H':
        w.set(col, line_number, head);
        break;
      case 't': case 'T':
        w.set(col, line_number, tail);
        break;
      case 'w': case 'W': case '.':
        w.set(col, line_number, wire);
        break;
      default:
        std::cerr << "unrecognized character: " << line[col] << "\n";
        return 1;
      case ' ':
        ; // no need to explicitly set this, so do nothing
      }
    }
    ++line_number;
  }

  display d(display_x, display_y, pixel_x, pixel_y, colors);

  w.draw(d);

  while (!d.keypressed())
  {
    usleep(100000);
    w.step();
    w.draw(d);
  }
  std::cout << std::endl;
}
```



## C#

See: [[Wireworld/C sharp]]


## Ceylon


```ceylon
abstract class Cell(shared Character char) of emptyCell | head | tail | conductor {

    shared Cell output({Cell*} neighbors) =>
            switch (this)
            case (emptyCell) emptyCell
            case (head) tail
            case (tail) conductor
            case (conductor) (neighbors.count(head.equals) in 1..2 then head else conductor);

    string => char.string;
}

object emptyCell extends Cell(' ') {}

object head extends Cell('H') {}

object tail extends Cell('t') {}

object conductor extends Cell('.') {}

Map<Character,Cell> cellsByChar = map { for (cell in `Cell`.caseValues) cell.char->cell };

class Wireworld(String data) {

    value lines = data.lines;

    value width = max(lines*.size);
    value height = lines.size;

    function toIndex(Integer x, Integer y) => x + y * width;

    variable value currentState = Array.ofSize(width * height, emptyCell);
    variable value nextState = Array.ofSize(width * height, emptyCell);

    for (j->line in lines.indexed) {
        for (i->char in line.indexed) {
            currentState[toIndex(i, j)] = cellsByChar[char] else emptyCell;
        }
    }

    value emptyGrid = Array.ofSize(width * height, emptyCell);
    void clear(Array<Cell> cells) => emptyGrid.copyTo(cells);

    shared void update() {
        clear(nextState);
        for(j in 0:height) {
            for(i in 0:width) {
                if(exists cell = currentState[toIndex(i, j)]) {
                    value nextCell = cell.output(neighborhood(currentState, i, j));
                    nextState[toIndex(i, j)] = nextCell;
                }
            }
        }
        value temp = currentState;
        currentState = nextState;
        nextState = temp;
    }

    shared void display() {
        for (row in currentState.partition(width)) {
            print("".join(row));
        }
    }

    shared {Cell*} neighborhood(Array<Cell> grid, Integer x, Integer y) => {
                for (j in y - 1..y + 1)
                for (i in x - 1..x + 1)
                if(i in 0:width && j in 0:height)
                grid[toIndex(i, j)]
            }.coalesced;

}

shared void run() {
    value data = "tH.........
                  .   .
                     ...
                  .   .
                  Ht.. ......";

    value world = Wireworld(data);

    variable value generation = 0;

    void display() {
        print("generation: ``generation``");
        world.display();
    }

    display();

    while (true) {
        if (exists input = process.readLine(), input.lowercased == "q") {
            return;
        }
        world.update();
        generation++;
        display();

    }
}

```



## Common Lisp



```lisp
(defun electron-neighbors (wireworld row col)
  (destructuring-bind (rows cols) (array-dimensions wireworld)
    (loop   for off-row from (max 0 (1- row)) to (min (1- rows) (1+ row)) sum
      (loop for off-col from (max 0 (1- col)) to (min (1- cols) (1+ col)) count
        (and (not (and (= off-row row) (= off-col col)))
             (eq 'electron-head (aref wireworld off-row off-col)))))))

(defun wireworld-next-generation (wireworld)
  (destructuring-bind (rows cols) (array-dimensions wireworld)
    (let ((backing (make-array (list rows cols))))
      (do ((c 0 (if (= c (1- cols)) 0 (1+ c)))
           (r 0 (if (= c (1- cols)) (1+ r) r)))
          ((= r rows))
        (setf (aref backing r c) (aref wireworld r c)))
      (do ((c 0 (if (= c (1- cols)) 0 (1+ c)))
           (r 0 (if (= c (1- cols)) (1+ r) r)))
          ((= r rows))
        (setf (aref wireworld r c)
              (case (aref backing r c)
                (electron-head 'electron-tail)
                (electron-tail 'conductor)
                (conductor (case (electron-neighbors backing r c)
                             ((1 2) 'electron-head)
                             (otherwise 'conductor)))
                (otherwise nil)))))))

(defun print-wireworld (wireworld)
  (destructuring-bind (rows cols) (array-dimensions wireworld)
    (do ((r 0 (1+ r)))
        ((= r rows))
      (do ((c 0 (1+ c)))
          ((= c cols))
        (format t "~C" (case (aref wireworld r c)
                         (electron-head #\H)
                         (electron-tail #\t)
                         (conductor #\.)
                         (otherwise #\Space))))
      (format t "~&"))))

(defun wireworld-show-gens (wireworld n)
  (dotimes (m n)
    (terpri)
    (wireworld-next-generation wireworld)
    (print-wireworld wireworld)))

(defun ww-char-to-symbol (char)
  (ecase char
    (#\Space 'nil)
    (#\.     'conductor)
    (#\t     'electron-tail)
    (#\H     'electron-head)))

(defun make-wireworld (image)
  "Make a wireworld grid from a list of strings (rows) of equal length
(columns), each character being ' ', '.', 'H', or 't'."
  (make-array (list (length image) (length (first image)))
              :initial-contents
              (mapcar (lambda (s) (map 'list #'ww-char-to-symbol s)) image)))

(defun make-rosetta-wireworld ()
  (make-wireworld '("tH........."
                    ".   .      "
                    "   ...     "
                    ".   .      "
                    "Ht.. ......")))
```

<pre style="height:30ex;overflow:scroll">CL-USER> (wireworld-show-gens (make-rosetta-wireworld) 12)

.tH........
H   .
   ...
H   .
t... ......

H.tH.......
t   .
   ...
t   .
.H.. ......

tH.tH......
.   H
   ...
.   .
HtH. ......

.tH.tH.....
H   t
   HHH
H   .
t.tH ......

H.tH.tH....
t   .
   ttt
t   .
.H.t ......

tH.tH.tH...
.   H
   ...
.   .
HtH. ......

.tH.tH.tH..
H   t
   HHH
H   .
t.tH ......

H.tH.tH.tH.
t   .
   ttt
t   .
.H.t ......

tH.tH.tH.tH
.   H
   ...
.   .
HtH. ......

.tH.tH.tH.t
H   t
   HHH
H   .
t.tH ......

H.tH.tH.tH.
t   .
   ttt
t   .
.H.t ......

tH.tH.tH.tH
.   H
   ...
.   .
HtH. ......
```



## D


```d
import std.stdio, std.algorithm;

void wireworldStep(char[][] W1, char[][] W2) pure nothrow @safe @nogc {
    foreach (immutable r; 1 .. W1.length - 1)
        foreach (immutable c; 1 .. W1[0].length - 1)
            switch (W1[r][c]) {
                case 'H': W2[r][c] = 't'; break;
                case 't': W2[r][c] = '.'; break;
                case '.':
                    int nH = 0;
                    foreach (sr; -1 .. 2)
                        foreach (sc; -1 .. 2)
                            nH += W1[r + sr][c + sc] == 'H';
                    W2[r][c] = (nH == 1 || nH == 2) ? 'H' : '.';
                    break;
                default:
            }
}

void main() {
    auto world = ["         ".dup,
                  "  tH     ".dup,
                  " .  .... ".dup,
                  "  ..     ".dup,
                  "         ".dup];

    char[][] world2;
    foreach (row; world)
        world2 ~= row.dup;

    foreach (immutable step; 0 .. 7) {
        writefln("\nStep %d: ------------", step);
        foreach (row; world[1 .. $ - 1])
            row[1 .. $ - 1].writeln;
        wireworldStep(world, world2);
        swap(world, world2);
    }
}
```

```txt

Step 0: ------------
 tH
.  ....
 ..

Step 1: ------------
 .t
.  H...
 ..

Step 2: ------------
 ..
.  tH..
 .H

Step 3: ------------
 ..
.  .tH.
 Ht

Step 4: ------------
 ..
H  ..tH
 t.

Step 5: ------------
 H.
t  ...t
 ..

Step 6: ------------
 tH
.  ....
 ..
```


## Elena

ELENA 3.4, using cellular library

```elena
import system'routines.
import extensions.
import cellular.

const literal sample =
" tH......
.        ......
 ...Ht...      .
              ....
              .  .....
              ....
 ......tH      .
.        ......
 ...Ht...".

const literal conductorLabel = ".".
const literal headLabel = "H".
const literal tailLabel = "t".
const literal emptyLabel = " ".

const int empty = 0.
const int conductor = 1.
const int electronHead = 2.
const int electronTail = 3.

wireWorldRuleSet = RuleSet::
{
    proceed(Space s, int x, int y, ref<int> retVal)
    [
        int cell := s getAt(x, y).

        cell =>
            conductor
            [
                int number := s getLiveCell(x, y, electronHead).
                if ((number == 1)||(number == 2))
                [
                    retVal value := electronHead
                ];
                [
                    retVal value := conductor
                ]
            ];
            electronHead
            [
                retVal value := electronTail
            ];
            electronTail
            [
                retVal value := conductor
            ];
            ![
                retVal value := cell
            ].
    ]
}.

sealed class Model
{
    T<Space>   theSpace.

    constructor load(LiteralValue stateString,int maxX, int maxY)
    [
        var strings := stateString split(newLine); selectBy(:s)(s toArray); toArray.

        theSpace := IntMatrixSpace new(maxX, maxY, RuleSet::
        {
            proceed(Space s, int x, int y, ref<int> retVal)
            [
                if (x < strings length)
                [
                    var l := strings[x].
                    if (y < l length)
                    [
                        (l[y]) =>
                            conductorLabel [ retVal value := conductor ];
                            headLabel      [ retVal value := electronHead ];
                            tailLabel      [ retVal value := electronTail ];
                            emptyLabel     [ retVal value := empty ].
                    ];
                    [
                        retVal value := empty
                    ]
                ];
                [
                    retVal value := empty
                ]

            ]
        }).
    ]

    run
    [
        theSpace update(wireWorldRuleSet).
    ]

    print
    [
        int columns := theSpace columns.
        int rows := theSpace rows.

        int i := 0.
        int j := 0.
        while (i < rows)
        [
            j := 0.

            while (j < columns)
            [
                var label := emptyLabel.
                int cell := theSpace getAt(i, j).

                cell =>
                    conductor    [ label := conductorLabel ];
                    electronHead [ label := headLabel ];
                    electronTail [ label := tailLabel ].

                console write(label).

                j := j + 1.
            ].

            i := i + 1.
            console writeLine.
        ].
    ]
}

public program
[
    T<Model> model := Model load(sample,10,30).
    0 to:10 do(:i)
    [
        console printLineFormatted("Iteration {0}",i).
        model print; run.
    ]
]
```

```txt

Iteration 0
 tH......
.        ......
 ...Ht...      .
              ....
              .  .....
              ....
 ......tH      .
.        ......
 ...Ht...

Iteration 1
 .tH.....
.        ......
 ..Ht....      .
              ....
              .  .....
              ....
 .......t      .
.        H.....
 ..Ht....

Iteration 2
 ..tH....
.        ......
 .Ht.....      .
              ....
              .  .....
              ....
 ........      .
.        tH....
 .Ht....H

Iteration 3
 ...tH...
.        ......
 Ht......      .
              ....
              .  .....
              ....
 ........      .
.        .tH...
 Ht....Ht

/* ... */

Iteration 9
 ...tH...
.        .tH...
 ......Ht      .
              ....
              H  H....
              tttH
 ...tH...      .
.        ......
 Ht......

Iteration 10
 ....tH..
.        ..tH..
 .....Ht.      .
              HHHH
              t  tH...
              ...t
 ....tH..      .
H        ......
 t.......

```



## Elixir

```elixir
defmodule Wireworld do
  @empty      " "
  @head       "H"
  @tail       "t"
  @conductor  "."
  @neighbours (for x<- -1..1, y <- -1..1, do: {x,y}) -- [{0,0}]

  def set_up(string) do
    lines = String.split(string, "\n", trim: true)
    grid = Enum.with_index(lines)
           |> Enum.flat_map(fn {line,i} ->
                String.codepoints(line)
                |> Enum.with_index
                |> Enum.map(fn {char,j} -> {{i, j}, char} end)
              end)
           |> Enum.into(Map.new)
    width = Enum.map(lines, fn line -> String.length(line) end) |> Enum.max
    height = length(lines)
    {grid, width, height}
  end

  # to string
  defp to_s(grid, width, height) do
    Enum.map_join(0..height-1, fn i ->
      Enum.map_join(0..width-1, fn j -> Map.get(grid, {i,j}, @empty) end) <> "\n"
    end)
  end

  # transition all cells simultaneously
  defp transition(grid) do
    Enum.into(grid, Map.new, fn {{x, y}, state} ->
      {{x, y}, transition_cell(grid, state, x, y)}
    end)
  end

  # how to transition a single cell
  defp transition_cell(grid, current, x, y) do
    case current do
      @empty -> @empty
      @head  -> @tail
      @tail  -> @conductor
      _      -> if neighbours_with_state(grid, x, y) in 1..2, do: @head, else: @conductor
    end
  end

  # given a position in the grid, find the neighbour cells with a particular state
  def neighbours_with_state(grid, x, y) do
    Enum.count(@neighbours, fn {dx,dy} -> Map.get(grid, {x+dx, y+dy}) == @head end)
  end

  # run a simulation up to a limit of transitions, or until a recurring
  # pattern is found
  # This will print text to the console
  def run(string, iterations\\25) do
    {grid, width, height} = set_up(string)
    Enum.reduce(0..iterations, {grid, %{}}, fn count,{grd, seen} ->
      IO.puts "Generation : #{count}"
      IO.puts to_s(grd, width, height)

      if seen[grd] do
        IO.puts "I've seen this grid before... after #{count} iterations"
        exit(:normal)
      else
        {transition(grd), Map.put(seen, grd, count)}
      end
    end)
    IO.puts "ran through #{iterations} iterations"
  end
end

# this is the "2 Clock generators and an XOR gate" example from the wikipedia page
text = """
 ......tH
.        ......
 ...Ht...      .
              ....
              .  .....
              ....
 tH......      .
.        ......
 ...Ht...
"""

Wireworld.run(text)
```


<pre style="height: 80ex; overflow: scroll">
Generation : 0
 ......tH
.        ......
 ...Ht...      .
              ....
              .  .....
              ....
 tH......      .
.        ......
 ...Ht...

Generation : 1
 .......t
.        H.....
 ..Ht....      .
              ....
              .  .....
              ....
 .tH.....      .
.        ......
 ..Ht....

Generation : 2
 ........
.        tH....
 .Ht....H      .
              ....
              .  .....
              ....
 ..tH....      .
.        ......
 .Ht.....

Generation : 3
 ........
.        .tH...
 Ht....Ht      .
              ....
              .  .....
              ....
 ...tH...      .
.        ......
 Ht......

Generation : 4
 ........
H        ..tH..
 t....Ht.      .
              ....
              .  .....
              ....
 ....tH..      .
H        ......
 t.......

Generation : 5
 H.......
t        ...tH.
 ....Ht..      .
              ....
              .  .....
              ....
 H....tH.      .
t        ......
 ........

Generation : 6
 tH......
.        ....tH
 ...Ht...      .
              ....
              .  .....
              ....
 tH....tH      .
.        ......
 ........

Generation : 7
 .tH.....
.        .....t
 ..Ht....      H
              ....
              .  .....
              ....
 .tH....t      .
.        H.....
 ........

Generation : 8
 ..tH....
.        ......
 .Ht.....      t
              HHH.
              .  .....
              ....
 ..tH....      .
.        tH....
 .......H

Generation : 9
 ...tH...
.        ......
 Ht......      .
              tttH
              H  H....
              ....
 ...tH...      .
.        .tH...
 ......Ht

Generation : 10
 ....tH..
H        ......
 t.......      .
              ...t
              t  tH...
              HHHH
 ....tH..      .
.        ..tH..
 .....Ht.

Generation : 11
 H....tH.
t        ......
 ........      .
              ....
              .  .tH..
              tttt
 .....tH.      .
.        ...tH.
 ....Ht..

Generation : 12
 tH....tH
.        ......
 ........      .
              ....
              .  ..tH.
              ....
 ......tH      .
.        ....tH
 ...Ht...

Generation : 13
 .tH....t
.        H.....
 ........      .
              ....
              .  ...tH
              ....
 .......t      H
.        H....t
 ..Ht....

Generation : 14
 ..tH....
.        tH....
 .......H      .
              ....
              .  ....t
              HHH.
 ........      t
.        tH....
 .Ht....H

Generation : 15
 ...tH...
.        .tH...
 ......Ht      .
              ....
              H  H....
              tttH
 ........      .
.        .tH...
 Ht....Ht

Generation : 16
 ....tH..
.        ..tH..
 .....Ht.      .
              HHHH
              t  tH...
              ...t
 ........      .
H        ..tH..
 t....Ht.

Generation : 17
 .....tH.
.        ...tH.
 ....Ht..      .
              tttt
              .  .tH..
              ....
 H.......      .
t        ...tH.
 ....Ht..

Generation : 18
 ......tH
.        ....tH
 ...Ht...      .
              ....
              .  ..tH.
              ....
 tH......      .
.        ....tH
 ...Ht...

Generation : 19
 .......t
.        H....t
 ..Ht....      H
              ....
              .  ...tH
              ....
 .tH.....      H
.        .....t
 ..Ht....

Generation : 20
 ........
.        tH....
 .Ht....H      t
              HHH.
              .  ....t
              HHH.
 ..tH....      t
.        ......
 .Ht.....

Generation : 21
 ........
.        .tH...
 Ht....Ht      .
              tttH
              .  H....
              tttH
 ...tH...      .
.        ......
 Ht......

Generation : 22
 ........
H        ..tH..
 t....Ht.      .
              ...t
              .  t....
              ...t
 ....tH..      .
H        ......
 t.......

Generation : 23
 H.......
t        ...tH.
 ....Ht..      .
              ....
              .  .....
              ....
 H....tH.      .
t        ......
 ........

I've seen this grid before... after 23 iterations

```



## Forth


```forth
16 constant w
 8 constant h

: rows    w * 2* ;
1 rows constant row
h rows constant size

create world size allot
world   value old
old w + value new

: init   world size erase ;
: age    new old to new to old ;

: foreachrow ( xt -- )
  size 0 do  I over execute  row +loop drop ;

0 constant EMPTY
1 constant          HEAD
2 constant                    TAIL
3 constant                              WIRE
create cstate bl c, char H c, char t c, char . c,

: showrow ( i -- ) cr
  old + w over + swap do I c@ cstate + c@ emit loop ;
: show  ['] showrow foreachrow  ;


: line ( row addr len -- )
  bounds do
    i c@
    case
    bl of EMPTY over c! endof
    'H of HEAD  over c! endof
    't of TAIL  over c! endof
    '. of WIRE  over c! endof
    endcase
    1+
  loop drop ;

: load ( filename -- )
  r/o open-file throw
  init  old row + 1+  ( file row )
  begin  over pad 80 rot read-line throw
  while  over pad rot line
         row +
  repeat
  2drop close-file throw
  show cr ;


: +head ( sum i -- sum )
  old + c@ HEAD = if 1+ then ;
: conductor ( i WIRE -- i HEAD|WIRE )
  drop 0
  over 1- row - +head
  over    row - +head
  over 1+ row - +head
  over 1-       +head
  over 1+       +head
  over 1- row + +head
  over    row + +head
  over 1+ row + +head
  1 3 within if HEAD else WIRE then ;

\ before:          empty    head   tail   wire

create transition  ' noop , ' 1+ , ' 1+ , ' conductor ,

\ after:           empty    tail   wire   head|wire

: new-state ( i -- )
  dup  old + c@
  dup cells transition + @ execute
  swap new + c! ;

: newrow ( i -- )
  w over + swap do I new-state loop ;
: gen  ['] newrow foreachrow  age ;

: wireworld begin gen 0 0 at-xy show key? until ;
```


<pre style="height:30ex;overflow:scroll">s" wireworld.diode" load

        ..
 tH...... .Ht
        ..




 ok
gen show

        ..
 .tH..... Ht.
        ..



                 ok
gen show

        .H
 ..tH.... t..
        .H



                 ok
gen show

        Ht
 ...tH..H ...
        Ht



                 ok
gen show

        t.
 ....tH.t ...
        t.



                 ok
gen show

        ..
 .....tH. ...
        ..



                 ok
gen show

        H.
 ......tH ...
        H.



                 ok
gen show

        tH
 .......t ...
        tH



                 ok
gen show

        .t
 ........ H..
        .t



                 ok
gen show

        ..
 ........ tH.
        ..



                 ok
gen show

        ..
 ........ .tH
        ..



                 ok
gen show

        ..
 ........ ..t
        ..



                 ok
gen show

        ..
 ........ ...
        ..



                 ok

```



## Fortran

```fortran
program Wireworld
  implicit none

  integer, parameter :: max_generations = 12
  integer :: nrows = 0, ncols = 0, maxcols = 0
  integer :: gen, ierr = 0
  integer :: i, j
  character(1), allocatable :: cells(:,:)
  character(10) :: form, sub
  character(80) :: buff

! open input file
  open(unit=8, file="wwinput.txt")

! find numbers of rows and columns in data
  do
    read(8, "(a)", iostat=ierr) buff
    if(ierr /= 0) exit
    nrows = nrows + 1
    ncols = len_trim(buff)
    if(ncols > maxcols) maxcols = ncols
  end do

! allcate enough space to hold the data
  allocate(cells(0:nrows+1, 0:maxcols+1))
  cells = " "

! load data
  rewind(8)
  do i = 1, nrows
    read(8, "(a)", iostat=ierr) buff
    if(ierr /= 0) exit
    do j = 1, maxcols
      cells(i, j) = buff(j:j)
    end do
  end do
  close(8)

! calculate format string for write statement
  write(sub, "(i8)") maxcols
  form = "(" // trim(adjustl(sub)) // "a1)"

  do gen = 0, max_generations
    write(*, "(/a, i0)") "Generation ", gen
    do i = 1, nrows
      write(*, form) cells(i, 1:maxcols)
    end do
    call nextgen(cells)
  end do
  deallocate(cells)

 contains

  subroutine Nextgen(cells)
    character, intent(in out) :: cells(0:,0:)
    character :: buffer(0:size(cells, 1)-1, 0:size(cells, 2)-1)
    integer :: i, j, h

     buffer = cells   ! Store current status
     do i = 1, size(cells, 1)-2
        do j = 1, size(cells, 2)-2
          select case (buffer(i, j))
            case(" ")
              ! no Change

            case("H")
              ! If a head change to tail
              cells(i, j) = "t"

            case("t")
              ! if a tail change to conductor
              cells(i, j) = "."

            case (".")
              ! Count number of electron heads in surrounding eight cells.
              ! We can ignore that fact that we count the centre cell as
              ! well because we already know it contains a conductor.
              ! If surrounded by 1 or 2 heads change to a head
              h = sum(count(buffer(i-1:i+1, j-1:j+1) == "H", 1))
              if(h == 1 .or. h == 2) cells(i, j) = "H"
          end select
        end do
     end do
  end subroutine Nextgen
end program Wireworld
```

<pre style="height:30ex;overflow:scroll">Generation 0
  tH...
 .     .
....... ......
 .     .
  tH...

Generation 1
  .tH..
 .     .
....... ......
 .     .
  .tH..

Generation 2
  ..tH.
 .     .
....... ......
 .     .
  ..tH.

Generation 3
  ...tH
 .     .
....... ......
 .     .
  ...tH

Generation 4
  ....t
 .     H
....... ......
 .     H
  ....t

Generation 5
  .....
 .     t
......H H.....
 .     t
  .....

Generation 6
  .....
 .     .
.....Ht tH....
 .     .
  .....

Generation 7
  .....
 .     .
....Ht. .tH...
 .     .
  .....

Generation 8
  .....
 .     .
...Ht.. ..tH..
 .     .
  .....

Generation 9
  .....
 .     .
..Ht... ...tH.
 .     .
  .....

Generation 10
  .....
 H     .
.Ht.... ....tH
 H     .
  .....

Generation 11
  H....
 t     .
.t..... .....t
 t     .
  H....

Generation 12
  tH...
 .     .
....... ......
 .     .
  tH...
```


## GML

Only visual output. Not an all-out simulator, but has some functions not on by default.

```GML
//Create event
/*
Wireworld first declares constants and then reads a wireworld from a textfile.
In order to implement wireworld in GML a single array is used.
To make it behave properly, there need to be states that are 'in-between' two states:
0 = empty
1 = conductor from previous state
2 = electronhead from previous state
5 = electronhead that was a conductor in the previous state
3 = electrontail from previous state
4 = electrontail that was a head in the previous state
*/
empty = 0;
conduc = 1;
eHead = 2;
eTail = 3;
eHead_to_eTail = 4;
coduc_to_eHead = 5;
working = true;//not currently used, but setting it to false stops wireworld. (can be used to pause)
toroidalMode = false;
factor = 3;//this is used for the display. 3 means a single pixel is multiplied by three in size.

var tempx,tempy ,fileid, tempstring, gridid, listid, maxwidth, stringlength;
tempx = 0;
tempy = 0;
tempstring = "";
maxwidth = 0;

//the next piece of code loads the textfile containing a wireworld.
//the program will not work correctly if there is no textfile.
if file_exists("WW.txt")
{
fileid = file_text_open_read("WW.txt");
gridid = ds_grid_create(0,0);
listid = ds_list_create();
    while !file_text_eof(fileid)
    {
    tempstring = file_text_read_string(fileid);
    stringlength = string_length(tempstring);
    ds_list_add(listid,stringlength);
        if maxwidth < stringlength
        {
        ds_grid_resize(gridid,stringlength,ds_grid_height(gridid) + 1)
        maxwidth = stringlength
        }
        else
        {
        ds_grid_resize(gridid,maxwidth,ds_grid_height(gridid) + 1)
        }

        for (i = 1; i <= stringlength; i +=1)
        {
            switch (string_char_at(tempstring,i))
            {
            case ' ': ds_grid_set(gridid,tempx,tempy,empty); break;
            case '.': ds_grid_set(gridid,tempx,tempy,conduc); break;
            case 'H': ds_grid_set(gridid,tempx,tempy,eHead); break;
            case 't': ds_grid_set(gridid,tempx,tempy,eTail); break;
            default: break;
            }
        tempx += 1;
        }
    file_text_readln(fileid);
    tempy += 1;
    tempx = 0;
    }
file_text_close(fileid);
//fill the 'open' parts of the grid
tempy = 0;
    repeat(ds_list_size(listid))
    {
    tempx = ds_list_find_value(listid,tempy);
        repeat(maxwidth - tempx)
        {
        ds_grid_set(gridid,tempx,tempy,empty);
        tempx += 1;
        }
    tempy += 1;
    }
boardwidth = ds_grid_width(gridid);
boardheight = ds_grid_height(gridid);
//the contents of the grid are put in a array, because arrays are faster.
//the grid was needed because arrays cannot be resized properly.
tempx = 0;
tempy = 0;
    repeat(boardheight)
    {
        repeat(boardwidth)
        {
        board[tempx,tempy] = ds_grid_get(gridid,tempx,tempy);
        tempx += 1;
        }
    tempy += 1;
    tempx = 0;
    }
//the following code clears memory
ds_grid_destroy(gridid);
ds_list_destroy(listid);
}
```

Now the step event

```GML

//Step event
/*
This step event executes each 1/speed seconds.
It checks everything on the board using an x and a y through two repeat loops.
The variables westN,northN,eastN,southN, resemble the space left, up, right and down respectively,
seen from the current x & y.
1 -> 5 (conductor is changing to head)
2 -> 4 (head is changing to tail)
3 -> 1 (tail became conductor)
*/

var tempx,tempy,assignhold,westN,northN,eastN,southN,neighbouringHeads,T;
tempx = 0;
tempy = 0;
westN = 0;
northN = 0;
eastN = 0;
southN = 0;
neighbouringHeads = 0;
T = 0;

if working = 1
{
    repeat(boardheight)
    {
        repeat(boardwidth)
        {
            switch board[tempx,tempy]
            {
            case empty: assignhold = empty; break;
            case conduc:
                neighbouringHeads = 0;
                if toroidalMode = true //this is disabled, but otherwise lets wireworld behave toroidal.
                {
                    if tempx=0
                    {
                    westN = boardwidth -1;
                    }
                    else
                    {
                    westN = tempx-1;
                    }
                    if tempy=0
                    {
                    northN = boardheight -1;
                    }
                    else
                    {
                    northN = tempy-1;
                    }
                    if tempx=boardwidth -1
                    {
                    eastN = 0;
                    }
                    else
                    {
                    eastN = tempx+1;
                    }
                    if tempy=boardheight -1
                    {
                    southN = 0;
                    }
                    else
                    {
                    southN = tempy+1;
                    }

                T=board[westN,northN];
                    if T=eHead or T=eHead_to_eTail
                    {
                    neighbouringHeads += 1;
                    }
                T=board[tempx,northN];
                    if T=eHead or T=eHead_to_eTail
                    {
                    neighbouringHeads += 1;
                    }
                T=board[eastN,northN];
                    if T=eHead or T=eHead_to_eTail
                    {
                    neighbouringHeads += 1;
                    }
                T=board[westN,tempy];
                    if T=eHead or T=eHead_to_eTail
                    {
                    neighbouringHeads += 1;
                    }
                T=board[eastN,tempy];
                    if T=eHead or T=eHead_to_eTail
                    {
                    neighbouringHeads += 1;
                    }
                T=board[westN,southN];
                    if T=eHead or T=eHead_to_eTail
                    {
                    neighbouringHeads += 1;
                    }
                T=board[tempx,southN];
                    if T=eHead or T=eHead_to_eTail
                    {
                    neighbouringHeads += 1;
                    }
                T=board[eastN,southN];
                    if T=eHead or T=eHead_to_eTail
                    {
                    neighbouringHeads += 1;
                    }
                }
                else//this is the default mode that works for the provided example.
                {//the next code checks whether coordinates fall outside the array borders.
                //and counts all the neighbouring electronheads.
                    if tempx=0
                    {
                    westN = -1;
                    }
                    else
                    {
                    westN = tempx - 1;
                    T=board[westN,tempy];
                        if T=eHead or T=eHead_to_eTail
                        {
                        neighbouringHeads += 1;
                        }
                    }
                    if tempy=0
                    {
                    northN = -1;
                    }
                    else
                    {
                    northN = tempy - 1;
                    T=board[tempx,northN];
                        if T=eHead or T=eHead_to_eTail
                        {
                        neighbouringHeads += 1;
                        }
                    }
                    if tempx = boardwidth -1
                    {
                    eastN = -1;
                    }
                    else
                    {
                    eastN = tempx + 1;
                    T=board[eastN,tempy];
                        if T=eHead or T=eHead_to_eTail
                        {
                        neighbouringHeads += 1;
                        }
                    }
                    if tempy = boardheight -1
                    {
                    southN = -1;
                    }
                    else
                    {
                    southN = tempy + 1;
                    T=board[tempx,southN];
                        if T=eHead or T=eHead_to_eTail
                        {
                        neighbouringHeads += 1;
                        }
                    }

                    if westN != -1 and northN != -1
                    {
                    T=board[westN,northN];
                        if T=eHead or T=eHead_to_eTail
                        {
                        neighbouringHeads += 1;
                        }
                    }
                    if eastN != -1 and northN != -1
                    {
                    T=board[eastN,northN];
                        if T=eHead or T=eHead_to_eTail
                        {
                        neighbouringHeads += 1;
                        }
                    }
                    if westN != -1 and southN != -1
                    {
                    T=board[westN,southN];
                        if T=eHead or T=eHead_to_eTail
                        {
                        neighbouringHeads += 1;
                        }
                    }
                    if eastN != -1 and southN != -1
                    {
                    T=board[eastN,southN];
                        if T=eHead or T=eHead_to_eTail
                        {
                        neighbouringHeads += 1;
                        }
                    }
                }
                    if neighbouringHeads = 1 or neighbouringHeads = 2
                    {
                    assignhold = coduc_to_eHead;
                    }
                    else
                    {
                    assignhold = conduc;
                    }
                break;

            case eHead: assignhold = eHead_to_eTail; break;
            case eTail: assignhold = conduc; break;
            default: break;
            }
        board[tempx,tempy] = assignhold;
        tempx += 1;
        }
    tempy += 1;
    tempx = 0;
    }
}

```

Now the draw event

```GML

//Draw event
/*
This event occurs whenever the screen is refreshed.
It checks everything on the board using an x and a y through two repeat loops and draws it.
It is an important step, because all board values are changed to the normal versions:
5 -> 2 (conductor changed to head)
4 -> 3 (head changed to tail)
*/
//draw sprites and text first

//now draw wireworld
var tempx,tempy;
tempx = 0;
tempy = 0;

repeat(boardheight)
{
    repeat(boardwidth)
    {
        switch board[tempx,tempy]
            {
            case empty:
            //draw_point_color(tempx,tempy,c_black);
            draw_set_color(c_black);
            draw_rectangle(tempx*factor,tempy*factor,(tempx+1)*factor-1,(tempy+1)*factor-1,false);
            break;
            case conduc:
            //draw_point_color(tempx,tempy,c_yellow);
            draw_set_color(c_yellow);
            draw_rectangle(tempx*factor,tempy*factor,(tempx+1)*factor-1,(tempy+1)*factor-1,false);
            break;
            case eHead:
            //draw_point_color(tempx,tempy,c_red);
            draw_set_color(c_blue);
            draw_rectangle(tempx*factor,tempy*factor,(tempx+1)*factor-1,(tempy+1)*factor-1,false);
            draw_rectangle_color(tempx*factor,tempy*factor,(tempx+1)*factor-1,(tempy+1)*factor-1,c_red,c_red,c_red,c_red,false);
            break;
            case eTail:
            //draw_point_color(tempx,tempy,c_blue);
            draw_set_color(c_red);
            draw_rectangle(tempx*factor,tempy*factor,(tempx+1)*factor-1,(tempy+1)*factor-1,false);
            break;
            case coduc_to_eHead:
            //draw_point_color(tempx,tempy,c_red);
            draw_set_color(c_blue);
            draw_rectangle(tempx*factor,tempy*factor,(tempx+1)*factor-1,(tempy+1)*factor-1,false);
            board[tempx,tempy] = eHead;
            break;
            case eHead_to_eTail:
            //draw_point_color(tempx,tempy,c_blue);
            draw_set_color(c_red);
            draw_rectangle(tempx*factor,tempy*factor,(tempx+1)*factor-1,(tempy+1)*factor-1,false);
            board[tempx,tempy] = eTail;
            break;
            default: break;
            }
    tempx += 1
    }
tempy += 1;
tempx = 0;
}
draw_set_color(c_black);

```


## Go

Text output.  Press Enter to compute and display successive generations.

```go
package main

import (
    "bytes"
    "fmt"
    "io/ioutil"
    "strings"
)

var rows, cols int // extent of input configuration
var rx, cx int     // grid extent (includes border)
var mn []int       // offsets of moore neighborhood

func main() {
    // read input configuration from file
    src, err := ioutil.ReadFile("ww.config")
    if err != nil {
        fmt.Println(err)
        return
    }
    srcRows := bytes.Split(src, []byte{'\n'})

    // compute package variables
    rows = len(srcRows)
    for _, r := range srcRows {
        if len(r) > cols {
            cols = len(r)
        }
    }
    rx, cx = rows+2, cols+2
    mn = []int{-cx-1, -cx, -cx+1, -1, 1, cx-1, cx, cx+1}

    // allocate two grids and copy input into first grid
    odd := make([]byte, rx*cx)
    even := make([]byte, rx*cx)
    for ri, r := range srcRows {
        copy(odd[(ri+1)*cx+1:], r)
    }

    // run
    for {
        print(odd)
        step(even, odd)
        fmt.Scanln()

        print(even)
        step(odd, even)
        fmt.Scanln()
    }
}

func print(grid []byte) {
    fmt.Println(strings.Repeat("__", cols))
    fmt.Println()
    for r := 1; r <= rows; r++ {
        for c := 1; c <= cols; c++ {
            if grid[r*cx+c] == 0 {
                fmt.Print("  ")
            } else {
                fmt.Printf(" %c", grid[r*cx+c])
            }
        }
        fmt.Println()
    }
}

func step(dst, src []byte) {
    for r := 1; r <= rows; r++ {
        for c := 1; c <= cols; c++ {
            x := r*cx + c
            dst[x] = src[x]
            switch dst[x] {
            case 'H':
                dst[x] = 't'
            case 't':
                dst[x] = '.'
            case '.':
                var nn int
                for _, n := range mn {
                    if src[x+n] == 'H' {
                        nn++
                    }
                }
                if nn == 1 || nn == 2 {
                    dst[x] = 'H'
                }
            }
        }
    }
}
```



## Haskell


```Haskell
import Data.List
import Control.Monad
import Control.Arrow
import Data.Maybe

states=" Ht."
shiftS=" t.."

borden bc xs = bs: (map (\x -> bc:(x++[bc])) xs) ++ [bs]
   where r = length $ head xs
         bs = replicate (r+2) bc

take3x3 = ap ((.). taken. length) (taken. length. head) `ap` borden '*'
   where taken n =  transpose. map (take n.map (take 3)).map tails

nwState xs | e =='.' && noH>0 && noH<3 = 'H'
           | otherwise = shiftS !! (fromJust $ elemIndex e states)
   where e = xs!!1!!1
         noH = length $ filter (=='H') $ concat xs

runCircuit = iterate (map(map nwState).take3x3)
```

Example executed in GHCi:

```Haskell
oscillator= [" tH    ",
             ".  ....",
             " ..    "
            ]

example = mapM_ (mapM_ putStrLn) .map (borden ' ').take 9 $ runCircuit oscillator
```

<pre style="height:30ex;overflow:scroll">
*Main> example

  tH
 .  ....
  ..


  .t
 .  H...
  ..


  ..
 .  tH..
  .H


  ..
 .  .tH.
  Ht


  ..
 H  ..tH
  t.


  H.
 t  ...t
  ..


  tH
 .  ....
  ..


  .t
 .  H...
  ..


  ..
 .  tH..
  .H

(0.01 secs, 541764 bytes)

```


=={{header|Icon}} and {{header|Unicon}}==
[[File:Wireworld-unicon.gif|thumb|right|Animated GIF enlarged 10x]]
This simulation starts in single step mode and can be switched to run uninterrupted.  The window can be saved at any point in single step mode.
This uses 1 pixel per cell so this animation looks tiny.  Also the orientation has been flipped.

```Icon
link graphics

$define EDGE  -1
$define EMPTY 0
$define HEAD 1
$define TAIL 2
$define COND 3

global Colours,Width,Height,World,oldWorld

procedure main()             # wire world modified from forestfire

    Height := 400            # Window height
    Width  := 400            # Window width
    Rounds := 500            # max Rounds
    Delay  := 5              # Runout Delay

    setup_world(read_world())
    every round := 1 to Rounds do {
       show_world()
       if \runout then
          delay(Delay)
       else
          case Event() of {
             "q" : break                            # q = quit
             "r" : runout := 1                      # r = run w/o stepping
             "s" : WriteImage("Wireworld-"||round)  # save
              }
       evolve_world()
       }
    WDone()
end

procedure read_world()  #: for demo in place of reading
   return [ "tH.........",
            ".   .",
            "   ...",
            ".   .",
            "Ht.. ......"]
end

procedure setup_world(L)     #: setup the world

    Colours := table()       # define colours
    Colours[EDGE]  := "grey"
    Colours[EMPTY] := "black"
    Colours[HEAD]  := "blue"
    Colours[TAIL]  := "red"
    Colours[COND]  := "yellow"

    States := table()
    States["t"] := TAIL
    States["H"] := HEAD
    States[" "] := EMPTY
    States["."] := COND

    WOpen("label=Wireworld", "bg=black",
          "size=" || Width+2 || "," || Height+2) | # add for border
             stop("Unable to open Window")
    every !(World := list(Height)) := list(Width,EMPTY)  # default
    every ( World[1,1 to Width]  | World[Height,1 to Width] |
            World[1 to Height,1] | World[1 to Height,Width] ) := EDGE

    every r := 1 to *L & c := 1 to *L[r] do {      # setup read in program
       World[r+1, c+1] :=  States[L[r,c]]
       }
end

procedure show_world()      #: show World - drawn changes only
   every r := 2 to *World-1 & c := 2 to *World[r]-1 do
      if /oldWorld | oldWorld[r,c] ~= World[r,c] then {
         WAttrib("fg=" || Colours[tr := World[r,c]])
         DrawPoint(r,c)
      }
end

procedure evolve_world()    #: evolve world
    old := oldWorld := list(*World)     # freeze copy
    every old[i := 1 to *World] := copy(World[i])  # deep copy

    every r := 2 to *World-1 & c := 2 to *World[r]-1 do
       World[r,c] := case old[r,c] of {   # apply rules
        # EMPTY : EMPTY
          HEAD  : TAIL
          TAIL  : COND
          COND  : {
              i := 0
              every HEAD = ( old[r-1,c-1 to c+1] | old[r,c-1|c+1] | old[r+1,c-1 to c+1] ) do i +:= 1
              if i := 1 | 2 then HEAD
          }
       }
end
```


[http://www.cs.arizona.edu/icon/library/src/procs/graphics.icn graphics.icn provides graphics]


## J

The example circuit:
```J
circ0=:}: ] ;. _1 LF, 0 : 0
tH........
.   .
   ...
.   .
Ht.. .....
)
```

A 'boarding' verb board and the next cell state verb nwS:

```J
board=: ' ' ,.~ ' ' ,. ' ' , ' ' ,~ ]

nwS=: 3 : 0
  e=. (<1 1){y
  if. ('.'=e)*. e.&1 2 +/'H'=,y do. 'H' return. end.
  ' t..' {~ ' Ht.' i. e
)
```

The 'most' powerful part is contained in the following iterating sentence, namely the dyad cut ;. [http://www.jsoftware.com/help/dictionary/d331.htm ]. In this way verb nwS can work on all the 3x3 matrices containing each cell surrounded by its 8 relevant neighbors.

```J
 process=: (3 3 nwS;. _3 board)^:
(<10) process circuit
```

Example run:
<pre style="height:30ex;overflow:scroll">
   (<10) process circ0
tH........
.   .
   ...
.   .
Ht.. .....

.tH.......
H   .
   ...
H   .
t... .....

H.tH......
t   .
   ...
t   .
.H.. .....

tH.tH.....
.   H
   ...
.   .
HtH. .....

.tH.tH....
H   t
   HHH
H   .
t.tH .....

H.tH.tH...
t   .
   ttt
t   .
.H.t .....

tH.tH.tH..
.   H
   ...
.   .
HtH. .....

.tH.tH.tH.
H   t
   HHH
H   .
t.tH .....

H.tH.tH.tH
t   .
   ttt
t   .
.H.t .....

tH.tH.tH.t
.   H
   ...
.   .
HtH. .....

```


Note also that a graphical presentation can be achieved using viewmat.  For example:


```j
require'viewmat'
viewmat"2 ' .tH'i. (<10) process circ0
```


(This example opens 10 windows, one for each generation.)


## Java

See: [[Wireworld/Java]]


## JavaScript

You have to search and open the file manually.<br />
This is the HTML you need to test.

```txt

<!DOCTYPE html><html><head><meta charset="UTF-8">
<title>Wireworld</title>
<script src="wireworld.js"></script></head><body>
<input type='file' accept='text/plain' onchange='openFile( event )' />
<br /></body></html>
```


```javascript

var ctx, sizeW, sizeH, scl = 10, map, tmp;
function getNeighbour( i, j ) {
    var ii, jj, c = 0;
    for( var b = -1; b < 2; b++ ) {
        for( var a = -1; a < 2; a++ ) {
            ii = i + a; jj = j + b;
            if( ii < 0 || ii >= sizeW || jj < 0 || jj >= sizeH ) continue;
            if( map[ii][jj] == 1 ) c++;
        }
    }
    return ( c == 1 || c == 2 );
}
function simulate() {
    drawWorld();
    for( var j = 0; j < sizeH; j++ ) {
        for( var i = 0; i < sizeW; i++ ) {
            switch( map[i][j] ) {
                case 0: tmp[i][j] = 0; break;
                case 1: tmp[i][j] = 2; break;
                case 2: tmp[i][j] = 3; break;
                case 3:
                    if( getNeighbour( i, j ) ) tmp[i][j] = 1;
                    else tmp[i][j] = 3;
                break;
            }
        }
    }
    [tmp, map] = [map, tmp];
    setTimeout( simulate, 200 );
}
function drawWorld() {
    ctx.fillStyle = "#000"; ctx.fillRect( 0, 0, sizeW * scl, sizeH * scl );
    for( var j = 0; j < sizeH; j++ ) {
        for( var i = 0; i < sizeW; i++ ) {
            switch( map[i][j] ) {
                case 0: continue;
                case 1: ctx.fillStyle = "#03f"; break;
                case 2: ctx.fillStyle = "#f30"; break;
                case 3: ctx.fillStyle = "#ff3"; break;
            }
            ctx.fillRect( i, j, 1, 1 );
        }
    }
}
function openFile( event ) {
    var input = event.target;
    var reader = new FileReader();
    reader.onload = function() {
        createWorld( reader.result );
    };
    reader.readAsText(input.files[0]);
}
function createWorld( txt ) {
    var l = txt.split( "\n" );
    sizeW = parseInt( l[0] );
    sizeH = parseInt( l[1] );
    map = new Array( sizeW );
    tmp = new Array( sizeW );
    for( var i = 0; i < sizeW; i++ ) {
        map[i] = new Array( sizeH );
        tmp[i] = new Array( sizeH );
        for( var j = 0; j < sizeH; j++ ) {
            map[i][j] = tmp[i][j] = 0;
        }
    }
    var t;
    for( var j = 0; j < sizeH; j++ ) {
        for( var i = 0; i < sizeW; i++ ) {
            switch( l[j + 2][i] ) {
                case " ": t = 0; break;
                case "H": t = 1; break;
                case "t": t = 2; break;
                case ".": t = 3; break;
            }
            map[i][j] = t;
        }
    }
    init();
}
function init() {
    var canvas = document.createElement( "canvas" );
    canvas.width = sizeW * scl;
    canvas.height = sizeH * scl;
    ctx = canvas.getContext( "2d" );
    ctx.scale( scl, scl );
    document.body.appendChild( canvas );
    simulate();
}

```



## jq

In this implementation, a "world" is simply a string as illustrated by world9 and world11 below. The "game" can be played either by creating separate frames (using frames(n)), or by calling animation(n; sleep) with sleep approximately equal to the number of milliseconds between refreshes.

"Animation" is based on the ANSI escape sequence for "clear screen".

'''Notes on the implementation''':
* For efficiency, the implementation requires that the world has boundaries, as illustrated by world11 below.
* For speed, the simulation uses the exploded string (an array).
* The ASCII values of the symbols used to display the state are hardcoded.

```jq
def lines: split("\n")|length;

def cols: split("\n")[0]|length + 1;  # allow for the newline

# Is there an "H" at [x,y] relative to position i, assuming the width is w?
# Input is an array; 72 is "H"
def isH(x; y; i; w): if .[i+ w*y + x] == 72 then 1 else 0 end;

def neighborhood(i;w):
  isH(-1; -1; i; w) + isH(0; -1; i; w) + isH(1; -1; i; w) +
  isH(-1;  0; i; w)                    + isH(1;  0; i; w) +
  isH(-1;  1; i; w) + isH(0;  1; i; w) + isH(1;  1; i; w) ;

# The basic rules:
# Input: a world
# Output: the next state of .[i]
def evolve(i; width) :
  # "Ht. " | explode => [ 72,  116,  46,  32 ]
  .[i] as $c
  | if   $c ==  32 then $c           # " " => " "
    elif $c == 116 then 46           # "t" => "."
    elif $c ==  72 then 116          # "H" => "t"
    elif $c ==  46 then              # "."
      # updates are "simultaneous" i.e. relative to $world
      neighborhood(i; width) as $sum
      | (if [1,2]|index($sum) then 72 else . end)  # "H"
    else $c
    end ;

# [world, lines, cols] | next(w) => [world, lines, cols]
def next:
  .[0] as $world | .[1] as $lines | .[2] as $w
  | reduce range(0; $world|length) as $i
    ($world;
      $world | evolve($i; $w) as $next
      | if  .[$i] == $next then . else .[$i] = $next end )
  | [., $lines, $w] ; #
```

'''Animation'''

```jq
# "clear screen":
def cls: "\u001b[2J";

# Input: an integer; 1000 ~ 1 sec
def spin:
  reduce range(1; 500 * .) as $i
    (0; . + ($i|cos)*($i|cos) + ($i|sin)*($i|sin) )
  |  "" ;

# Animate n steps;
# if "sleep" is non-negative then cls and
# sleep about "sleep" ms between frames.
def animate(n; sleep):
  if n == 0 then empty
  else (if sleep >= 0 then cls else "" end),
       (.[0]|implode), n, "\n",
       (sleep|spin),
       ( next|animate(n-1; sleep) )
  end ;

# Input: a string representing the initial state
def animation(n; sleep):
  [ explode, lines, cols] | animate(n; sleep) ;

# Input: a string representing the initial state
def frames(n):  animation(n; -1);#
```

'''Examples''':

```jq
def world11:
"+-----------+\n" +
"|tH.........|\n" +
"|.   .      |\n" +
"|   ...     |\n" +
"|.   .      |\n" +
"|Ht.. ......|\n" +
"+-----------+\n" ;

def world9:
"         \n" +
"  tH     \n" +
" .  .... \n" +
"  ..     \n" +
"         \n" ;
```

'''Illustration 1''':

```jq
# Ten-step animation with about 1 sec between frames
world9 | animation(10; 1000)
```

'''Illustration 2''':

```jq
# Ten frames in sequence:
world11 | frames(10)
```


To run: jq -n -r -f wireworld.rc


## Julia


```julia
function surround2D(b, i, j)
    h, w = size(b)
    [b[x,y] for x in i-1:i+1, y in j-1:j+1 if (0 < x <= h && 0 < y <= w)]
end

surroundhas1or2(b, i, j) = 0 < sum(map(x->Char(x)=='H', surround2D(b, i, j))) <= 2 ? 'H' : '.'

function boardstep!(currentboard, nextboard)
    x, y = size(currentboard)
    for j in 1:y, i in 1:x
        ch = Char(currentboard[i, j])
        if ch == ' '
            continue
        else
            nextboard[i, j] = (ch == 'H') ? 't' : (ch == 't' ? '.' :
                   surroundhas1or2(currentboard, i, j))
        end
    end
end

const b1 = "         " *
           "  tH     " *
           " .  .... " *
           "  ..     " *
           "         "
const mat = reshape(map(x->UInt8(x[1]), split(b1, "")), (9, 5))'
const mat2 = copy(mat)

function printboard(mat)
    for i in 1:size(mat)[1]
        println("\t", join([Char(c) for c in mat[i,:]], ""))
    end
end

println("Starting Wireworld board:")
printboard(mat)
for step in 1:8
    boardstep!(mat, mat2)
    println(" Step $step:")
    printboard(mat2)
    mat .= mat2
end

```
```txt

Starting Wireworld board:

          tH
         .  ....
          ..

 Step 1:

          .t
         .  H...
          ..

 Step 2:

          ..
         .  tH..
          .H

 Step 3:

          ..
         .  .tH.
          Ht

 Step 4:

          ..
         H  ..tH
          t.

 Step 5:

          H.
         t  ...t
          ..

 Step 6:

          tH
         .  ....
          ..

 Step 7:

          .t
         .  H...
          ..

 Step 8:

          ..
         .  tH..
          .H

```




## Liberty BASIC

[[File:AnimWW.gif]]

```lb

WindowWidth  = 840
WindowHeight = 600

dim p$( 40, 25), q$( 40, 25)

empty$     = " "    '   white
tail$      = "t"    '   yellow
head$      = "H"    '   black
conductor$ = "."    '   red

jScr       = 0

nomainwin

menu #m, "File", "Load", [load], "Quit", [quit]

open "wire world" for graphics_nf_nsb as #m
  #m "trapclose [quit]"
  'timer 1000, [tmr]
  wait

end

[quit]
  close #m
  end

[load]
  'timer 0
  filedialog "Open WireWorld File", "*.ww", file$
  open file$ for input as #in
    y =0
    while not( eof( #in))
      line input #in, lijn$
      ' print "|"; lijn$; "|"
      for x =0 to len( lijn$) -1
        p$( x, y) =mid$( lijn$, x +1, 1)

        select case p$( x, y)
            case " "
                clr$ ="white"
            case "t"
                clr$ ="yellow"
            case "H"
                clr$ ="black"
            case "."
                clr$ ="red"
        end select

        #m "goto " ; 4 +x *20; " "; 4 +y *20
        #m "backcolor "; clr$
        #m "down"
        #m "boxfilled "; 4 +x *20 +19; " "; 4 +y *20 +19
        #m "up ; flush"
      next x
      y =y +1
    wend
  close #in
  'notice "Ready to run."
  timer 1000, [tmr]
  wait

[tmr]
  timer 0
  scan

  for x =0 to 40                                '   copy temp array /current array
    for y =0 to 25
      q$( x, y) =p$( x, y)
    next y
  next x

  for y =0 to 25
    for x =0 to 40
      select case q$( x, y)
        case head$                              '   heads ( black) become tails ( yellow)
          p$( x, y ) =tail$
          clr$ ="yellow"

        case tail$                              '   tails ( yellow) become conductors ( red)
          p$( x, y ) =conductor$
          clr$ ="red"

        case conductor$                         '
          hCnt =0

          xL =x -1: if xL < 0 then xL =40       '   wrap-round edges at all four sides
          xR =x +1: if xR >40 then xR = 0
          yA =y -1: if yA < 0 then yA =25
          yB =y +1: if yB >40 then yB = 0

          if q$( xL, y ) =head$ then hCnt =hCnt +1  '   Moore environment- 6 neighbours
          if q$( xL, yA) =head$ then hCnt =hCnt +1  '   count all neighbours currently heads
          if q$( xL, yB) =head$ then hCnt =hCnt +1

          if q$( xR, y ) =head$ then hCnt =hCnt +1
          if q$( xR, yA) =head$ then hCnt =hCnt +1
          if q$( xR, yB) =head$ then hCnt =hCnt +1

          if q$( x,  yA) =head$ then hCnt =hCnt +1
          if q$( x,  yB) =head$ then hCnt =hCnt +1

          if ( hCnt =1) or ( hCnt =2) then       '      conductor ( red) becomes head ( yellow) in this case only
            p$( x, y ) =head$                    '          otherwise stays conductor ( red).
            clr$ ="black"
          else
            p$( x, y ) =conductor$
            clr$ ="red"
          end if

        case else
          clr$ ="white"
      end select

      #m "goto " ; 4 +x *20; " "; 4 +y *20
      #m "backcolor "; clr$
      #m "down"
      #m "boxfilled "; 4 +x *20 +19; " "; 4 +y *20 +19
      #m "up"
    next x
  next y
  #m "flush"
  #m "getbmp scr 0 0 400 300"

  'bmpsave "scr", "R:\scrJHF" +right$( "000" +str$( jScr), 3) +".bmp"
  jScr =jScr+1
  if jScr >20 then wait
  timer 1000, [tmr]
wait

```



## Lua

If ran using [[L%C3%96VE]], it will animate the simulation on a window. Otherwise it will print the first 10 steps on the console.

```Lua

local map = {{'t', 'H', '.', '.', '.', '.', '.', '.', '.', '.', '.'},
             {'.', ' ', ' ', ' ', '.'},
             {' ', ' ', ' ', '.', '.', '.'},
             {'.', ' ', ' ', ' ', '.'},
             {'H', 't', '.', '.', ' ', '.', '.', '.', '.', '.', '.'}}

function step(map)
    local next = {}
    for i = 1, #map do
        next[i] = {}
        for j = 1, #map[i] do
            next[i][j] = map[i][j]
            if map[i][j] == "H" then
                next[i][j] = "t"
            elseif map[i][j] == "t" then
                next[i][j] = "."
            elseif map[i][j] == "." then
                local count = 	((map[i-1] or {})[j-1] == "H" and 1 or 0) +
                                ((map[i-1] or {})[j] == "H" and 1 or 0) +
                                ((map[i-1] or {})[j+1] == "H" and 1 or 0) +
                                ((map[i] or {})[j-1] == "H" and 1 or 0) +
                                ((map[i] or {})[j+1] == "H" and 1 or 0) +
                                ((map[i+1] or {})[j-1] == "H" and 1 or 0) +
                                ((map[i+1] or {})[j] == "H" and 1 or 0) +
                                ((map[i+1] or {})[j+1] == "H" and 1 or 0)
                if count == 1 or count == 2 then
                    next[i][j] = "H"
                else
                    next[i][j] = "."
                end
            end
        end
    end
    return next
end

if not not love then
    local time, frameTime, size = 0, 0.25, 20
    local colors = {["."] = {255, 200, 0},
                    ["t"] = {255, 0, 0},
                    ["H"] = {0, 0, 255}}
    function love.update(dt)
        time = time + dt
        if time > frameTime then
            time = time - frameTime
            map = step(map)
        end
    end

    function love.draw()
        for i = 1, #map do
            for j = 1, #map[i] do
                love.graphics.setColor(colors[map[i][j]] or {0, 0, 0})
                love.graphics.rectangle("fill", j*size, i*size, size, size)
            end
        end
    end
else
    for iter = 1, 10 do
        print("\nstep "..iter.."\n")
        for i = 1, #map do
            for j = 1, #map[i] do
                io.write(map[i][j])
            end
            io.write("\n")
        end
        map = step(map)
    end
end

```


## Logo

(The wireworld given in the file must be bounded by spaces for the program to work. Also it is notable that the program takes the width as the longest of the lines.)

```Logo
to wireworld :filename :speed ;speed in n times per second, approximated
Make "speed 60/:speed
wireworldread :filename
Make "bufferfield (mdarray (list :height :width) 0)
for [i 0 :height-1] [for [j 0 :width-1] [mdsetitem (list :i :j) :bufferfield mditem (list :i :j) :field]]
pu ht
Make "gen 0
while ["true] [ ;The user will have to halt it :P
    ;clean
    seth 90
    setxy 0 20
    ;label :gen
    sety 0
    for [i 0 :height-1] [for [j 0 :width-1] [mdsetitem (list :i :j) :field mditem (list :i :j) :bufferfield]]
    for [i 0 :height-1] [
        for [j 0 :width-1] [
            if (mditem (list :i :j) :field)=[] [setpixel [255 255 255]] ;blank
            if (mditem (list :i :j) :field)=1 [setpixel [0 0 0] if wn :j :i 2 [mdsetitem (list :i :j) :bufferfield 2]] ;wire
            if (mditem (list :i :j) :field)=2 [setpixel [0 0 255] mdsetitem (list :i :j) :bufferfield 3] ;head
            if (mditem (list :i :j) :field)=3 [setpixel [255 0 0] mdsetitem (list :i :j) :bufferfield 1] ;tail
            setx xcor+1
        ]
        setxy 0 ycor-1
    ]
    Make "gen :gen+1
    wait :speed
]
end

to wireworldread :filename
local [line]
openread :filename
setread :filename
Make "width 0
Make "height 0
; first pass, take dimensions
while [not eofp] [
    Make "line readword
    if (count :line)>:width [Make "width count :line]
    Make "height :height+1
]
; second pass, load data
setreadpos 0
Make "field (mdarray (list :height :width) 0)
for [i 0 :height-1] [
    Make "line readword
    foreach :line [
        if ?=char 32 [mdsetitem (list :i #-1) :field []]
        if ?=".      [mdsetitem (list :i #-1) :field 1]
        if ?="H      [mdsetitem (list :i #-1) :field 2]
        if ?="t      [mdsetitem (list :i #-1) :field 3]
    ]
]
setread []
close :filename
end

to wn :x :y :thing ;WireNeighbourhood
Make "neighbours 0
if (mditem (list :y-1 :x) :field)=:thing [Make "neighbours :neighbours+1]
if (mditem (list :y-1 :x+1) :field)=:thing [Make "neighbours :neighbours+1]
if (mditem (list :y :x+1) :field)=:thing [Make "neighbours :neighbours+1]
if (mditem (list :y+1 :x+1) :field)=:thing [Make "neighbours :neighbours+1]
if (mditem (list :y+1 :x) :field)=:thing [Make "neighbours :neighbours+1]
if (mditem (list :y+1 :x-1) :field)=:thing [Make "neighbours :neighbours+1]
if (mditem (list :y :x-1) :field)=:thing [Make "neighbours :neighbours+1]
if (mditem (list :y-1 :x-1) :field)=:thing [Make "neighbours :neighbours+1]
ifelse OR :neighbours=1 :neighbours=2 [op "true] [op "false]
end
```



## Mathematica


```Mathematica
DynamicModule[{data =
   ArrayPad[PadRight[Characters /@ StringSplit["tH.........
        .   .
           ...
        .   .
        Ht.. ......", "\n"]] /. {" " -> 0, "t" -> 2, "H" -> 1,
      "." -> 3}, 1]},
 Dynamic@ArrayPlot[
   data = CellularAutomaton[{{{_, _, _}, {_, 0, _}, {_, _, _}} ->
       0, {{_, _, _}, {_, 1, _}, {_, _, _}} ->
       2, {{_, _, _}, {_, 2, _}, {_, _, _}} ->
       3, {{a_, b_, c_}, {d_, 3, e_}, {f_, g_, h_}} :>
       Switch[Count[{a, b, c, d, e, f, g, h}, 1], 1, 1, 2, 1, _, 3]},
     data], ColorRules -> {1 -> Yellow, 2 -> Red}]]
```



## Nim

```nim
import strutils, os

var world, world2 = """
+-----------+
|tH.........|
|.   .      |
|   ...     |
|.   .      |
|Ht.. ......|
+-----------+"""
let h = world.splitLines.len
let w = world.splitLines[0].len

template isH(x, y): int = int(s[i+ w*y + x] == 'H')

proc next(o: var string, s: string, w: int) =
  for i, c in s:
    o[i] = case c
      of ' ': ' '
      of 't': '.'
      of 'H': 't'
      of '.':
        if (isH(-1, -1) + isH(0, -1) + isH(1, -1) +
            isH(-1,  0)              + isH(1,  0) +
            isH(-1,  1) + isH(0,  1) + isH(1,  1)
           ) in 1..2: 'H' else: '.'
      else: c

while true:
  echo world
  stdout.write "\x1b[",h,"A"
  stdout.write "\x1b[",w,"D"
  sleep 100

  world2.next(world, w)
  swap world, world2
```



## OCaml



```ocaml
let w = [|
    "  ......tH              ";
    " .        ......        ";
    "  ...Ht...      .       ";
    "               ....     ";
    "               .  ..... ";
    "               ....     ";
    "  tH......      .       ";
    " .        ......        ";
    "  ...Ht...              ";
  |]

let is_head w x y =
  try if w.(x).[y] = 'H' then 1 else 0
  with _ -> 0

let neighborhood_heads w x y =
  let n = ref 0 in
  for _x = pred x to succ x do
    for _y = pred y to succ y do
      n := !n + (is_head w _x _y)
    done;
  done;
  (!n)

let step w =
  let n = Array.init (Array.length w) (fun i -> String.copy w.(i)) in
  let width = Array.length w
  and height = String.length w.(0)
  in
  for x = 0 to pred width do
    for y = 0 to pred height do
      n.(x).[y] <- (
        match w.(x).[y] with
        | ' ' -> ' '
        | 'H' -> 't'
        | 't' -> '.'
        | '.' ->
            (match neighborhood_heads w x y with
            | 1 | 2 -> 'H'
            | _ -> '.')
        | _ -> assert false)
    done;
  done;
  (n)

let print = (Array.iter print_endline)

let () =
  let rec aux w =
    Unix.sleep 1;
    let n = step w in
    print n;
    aux n
  in
  aux w
```



## Oz

Includes a simple animation, using a text widget.

```oz
declare
  Rules =
  [rule(&  & )
   rule(&H &t)
   rule(&t &.)
   rule(&. &H when:fun {$ Neighbours}
                      fun {IsHead X} X == &H end
                      Hs = {Filter Neighbours IsHead}
                      Len = {Length Hs}
                   in
                      Len == 1 orelse Len == 2
                   end)
   rule(&. &.)]

  Init = ["tH........."
          ".   .      "
          "   ...     "
          ".   .      "
          "Ht.. ......"]

  MaxGen = 100

  %% G(i) -> G(i+1)
  fun {Evolve Gi}
     fun {Get X#Y}
        Row = {CondSelect Gi Y unit}
     in
        {CondSelect Row X & } %% cells beyond boundaries are empty
     end
     fun {GetNeighbors X Y}
        {Map [X-1#Y-1  X#Y-1  X+1#Y-1
              X-1#Y           X+1#Y
              X-1#Y+1  X#Y+1  X+1#Y+1]
         Get}
     end
  in
     {Record.mapInd Gi
      fun {$ Y Row}
         {Record.mapInd Row
          fun {$ X C}
             for Rule in Rules return:Return do
                if C == Rule.1 then
		   When = {CondSelect Rule when {Const true}}
		in
		   if {When {GetNeighbors X Y}} then
		      {Return Rule.2}
		   end
		end
	     end
          end}
      end}
  end

  %% Create an arena from a list of strings.
  fun {ReadArena LinesList}
     {List.toTuple '#'
      {Map LinesList
       fun {$ Line}
          {List.toTuple row Line}
       end}}
  end

  %% Converts an arena to a virtual string
  fun {ShowArena G}
     {Record.map G
      fun {$ L} {Record.toList L}#"\n" end}
  end

  %% helpers
  fun lazy {Iterate F V} V|{Iterate F {F V}} end
  fun {Const X} fun {$ _} X end end

  %% prepare GUI
  [QTk]={Module.link ["x-oz://system/wp/QTk.ozf"]}
  GenDisplay
  Field
  GUI = td(label(handle:GenDisplay)
           label(handle:Field font:{QTk.newFont font(family:'Courier')})
          )
  {{QTk.build GUI} show}

  G0 = {ReadArena Init}
  Gn = {Iterate Evolve G0}
in
  for
     Gi in Gn
     I in 0..MaxGen
  do
     {GenDisplay set(text:"Gen. "#I)}
     {Field set(text:{ShowArena Gi})}
     {Delay 500}
  end
```



## PARI/GP


```parigp
\\ 0 = conductor, 1 = tail, 2 = head, 3 = empty
wireworldStep(M)={
	my(sz=matsize(M),t);
	matrix(sz[1],sz[2],x,y,
		t=M[x,y];
		if(t,
			[0,1,3][t]
		,
			t=sum(i=max(x-1,1),min(x+1,sz[1]),
				sum(j=max(y-1,1),min(y+1,sz[2]),
					M[i,j]==2
				)
			);
			if(t==1|t==2,2,3)
		)
	)
};
animate(M)={
	while(1,display(M=wireworldStep(M)))
};
display(M)={
	my(sz=matsize(M),t);
	for(i=1,sz[1],
		for(j=1,sz[2],
			t=M[i,j];
			print1([".","t","H"," "][t+1])
		);
		print
	)
};
animate(read("wireworld.gp"))
```



## Perl

Read the initial World from stdin and print 10 steps to stdout

```perl
my @f = ([],(map {chomp;['',( split // ),'']} <>),[]);

for (1 .. 10) {
	print join "", map {"@$_\n"} @f;
	my @a = ([]);
	for my $y (1 .. $#f-1) {
		my $r = $f[$y];
		my $rr = [''];
		for my $x (1 .. $#$r-1) {
			my $c = $r->[$x];
			push @$rr,
				$c eq 'H' ? 't' :
				$c eq 't' ? '.' :
				$c eq '.' ? (join('', map {"@{$f[$_]}[$x-1 .. $x+1]"=~/H/g} ($y-1 .. $y+1)) =~ /^H{1,2}$/ ? 'H' : '.') :
				$c;
		}
		push @$rr, '';
		push @a, $rr;
	}
	@f = (@a,[]);
}
```

Input:

```txt
tH.........
.   .
   ...
.   .
Ht.. ......
```

<pre style="height:30ex;overflow:scroll">
 t H . . . . . . . . .
 .       .
       . . .
 .       .
 H t . .   . . . . . .


 . t H . . . . . . . .
 H       .
       . . .
 H       .
 t . . .   . . . . . .


 H . t H . . . . . . .
 t       .
       . . .
 t       .
 . H . .   . . . . . .


 t H . t H . . . . . .
 .       H
       . . .
 .       .
 H t H .   . . . . . .


 . t H . t H . . . . .
 H       t
       H H H
 H       .
 t . t H   . . . . . .


 H . t H . t H . . . .
 t       .
       t t t
 t       .
 . H . t   . . . . . .


 t H . t H . t H . . .
 .       H
       . . .
 .       .
 H t H .   . . . . . .


 . t H . t H . t H . .
 H       t
       H H H
 H       .
 t . t H   . . . . . .


 H . t H . t H . t H .
 t       .
       t t t
 t       .
 . H . t   . . . . . .


 t H . t H . t H . t H
 .       H
       . . .
 .       .
 H t H .   . . . . . .

```



## Perl 6

```perl6
class Wireworld {
    has @.line;
    method height () { @!line.elems }
    has int $.width;

    multi method new(@line) { samewith :@line, :width(max @line».chars) }
    multi method new($str ) { samewith $str.lines }

    method gist { join "\n", @.line }

    method !neighbors($i where ^$.height, $j where ^$.width)
    {
        my @i = grep ^$.height, $i «+« (-1, 0, 1);
        my @j = grep ^$.width,  $j «+« (-1, 0, 1);
        gather for @i X @j -> (\i, \j) {
            next if [ i, j ] ~~ [ $i, $j ];
            take @!line[i].comb[j];
        }
    }
    method succ {
        my @succ;
        for ^$.height X ^$.width -> ($i, $j) {
            @succ[$i] ~=
            do given @!line[$i].comb[$j] {
                when 'H' { 't' }
                when 't' { '.' }
                when '.' {
                    grep('H', self!neighbors($i, $j)) == 1|2 ?? 'H' !! '.'
                }
                default { ' ' }
            }
        }
        return self.new: @succ;
    }
}

my %*SUB-MAIN-OPTS;
%*SUB-MAIN-OPTS<named-anywhere> = True;

multi sub MAIN (
    IO()      $filename,
    Numeric:D :$interval = 1/4,
    Bool      :$stop-on-repeat,
) {
    run-loop :$interval, :$stop-on-repeat, Wireworld.new: $filename.slurp;
}

#| run a built-in example
multi sub MAIN (
    Numeric:D :$interval = 1/4,
    Bool      :$stop-on-repeat,
) {
    run-loop :$interval, :$stop-on-repeat, Wireworld.new: Q:to/END/
    tH.........
    .   .
       ...
    .   .
    Ht.. ......
    END
}

sub run-loop (
    Wireworld:D     $initial,
    Real:D(Numeric) :$interval = 1/4,
    Bool            :$stop-on-repeat
){
    my %seen is SetHash;

    for $initial ...^ * eqv * { # generate a sequence (uses .succ)
        print "\e[2J";
        say '#' x $initial.width;
        .say;
        say '#' x $initial.width;

        if $stop-on-repeat {
            last if %seen{ .gist }++;
        }

        sleep $interval;
    }
}
```

When run with <code>--stop-on-repeat</code>
```txt
###########
H.tH.tH.tH.
t   .
   ttt
t   .
.H.t ......
###########
```



## Phix

```Phix
--
-- demo\rosetta\Wireworld.exw
--
### ====================

--
--  Invoke with file to read or let it read the one below (if compiled assumes source is in the same directory)
--
--  Note that tabs in description files are not supported - where necessary spaces can be replaced with _ chars.
--  (tab chars in text files should technically always represent (to-next) 8 spaces, but not many editors respect
--   that, and instead assume the file will only ever be read by the same program/with matching settings. </rant>)
--  (see also demo\edix\src\tabs.e\ExpandTabs() for what you'd need if you knew what tab chars really meant.)
--
/* -- default description:
tH.........
.___.
___...
.___.
Ht.. ......
*/
sequence lines, counts
integer longest

function valid_line(string line, integer l=0)
    if length(line)=0 then return 0 end if
    for i=1 to length(line) do
        integer ch = line[i]
        if not find(ch," _.tH") then
            if l and ch='\t' then
                -- as above
                printf(1,"error: tab char on line %d\n",{l})
                {} = wait_key()
                abort(0)
            end if
            return 0
        end if
    end for
    return 1
end function

procedure load_desc()
    string filename = substitute(command_line()[$],".exe",".exw")
    integer fn = open(filename,"r")
    if fn=-1 then
        printf(1,"error opening %s\n",{filename})
        {} = wait_key()
        abort(0)
    end if
    sequence text = get_text(fn,GT_LF_STRIPPED)
    close(fn)
    lines = {}
    for i=1 to length(text) do
        string line = text[i]
        if valid_line(line) then
            lines = {line}
            longest = length(line)
            for j=i+1 to length(text) do
                line = text[j]
                if not valid_line(line,j) then exit end if
                lines = append(lines,line)
                if longest<length(line) then
                    longest = length(line)
                end if
            end for
            exit
        end if
    end for
    counts = lines
end procedure

constant dxy = {{-1,-1}, {-1,+0}, {-1,+1},
                {+0,-1},          {+0,+1},
                {+1,-1}, {+1,+0}, {+1,+1}}

procedure set_counts()
    for y=1 to length(lines) do
        for x=1 to length(lines[y]) do
            if lines[y][x]='.' then
                integer count = 0
                for k=1 to length(dxy) do
                    integer {cx,cy} = sq_add({x,y},dxy[k])
                    if cy>=1 and cy<=length(lines)
                    and cx>=1 and cx<=length(lines[cy])
                    and lines[cy][cx]='H' then
                        count += 1
                    end if
                end for
                counts[y][x] = (count=1 or count=2)
            end if
        end for
    end for
end procedure

include pGUI.e

Ihandle dlg, canvas, timer
cdCanvas cddbuffer, cdcanvas

function redraw_cb(Ihandle /*ih*/, integer /*posx*/, integer /*posy*/)
    integer {w, h} = IupGetIntInt(canvas, "DRAWSIZE")
    integer dx = floor(w/(longest+2))
    integer dy = floor(h/(length(lines)+2))
    cdCanvasActivate(cddbuffer)
    cdCanvasClear(cddbuffer)
    set_counts()
    for y=1 to length(lines) do
        for x=1 to length(lines[y]) do
            integer c = lines[y][x], colour
            if find(c," _") then
                colour = CD_BLACK
            elsif c='.' then
                colour = CD_YELLOW
                if counts[y][x] then
                    lines[y][x] = 'H'
                end if
            elsif c='H' then
                colour = CD_BLUE
                lines[y][x] = 't'
            elsif c='t' then
                colour = CD_RED
                lines[y][x] = '.'
            end if
            cdCanvasSetForeground(cddbuffer, colour)
            cdCanvasBox(cddbuffer,x*dx,x*dx+dx,h-y*dy,h-(y*dy+dy))
        end for
    end for
    cdCanvasFlush(cddbuffer)
    return IUP_DEFAULT
end function

function timer_cb(Ihandle /*ih*/)
    IupUpdate(canvas)
    return IUP_IGNORE
end function

function map_cb(Ihandle ih)
    cdcanvas = cdCreateCanvas(CD_IUP, ih)
    cddbuffer = cdCreateCanvas(CD_DBUFFER, cdcanvas)
    cdCanvasSetBackground(cddbuffer, CD_BLACK)
    return IUP_DEFAULT
end function

function esc_close(Ihandle /*ih*/, atom c)
    if c=K_ESC then return IUP_CLOSE end if
    return IUP_CONTINUE
end function

procedure main()
    load_desc()
    IupOpen()

    canvas = IupCanvas(NULL)
    IupSetAttribute(canvas, "RASTERSIZE", "300x180")
    IupSetCallback(canvas, "MAP_CB", Icallback("map_cb"))
    IupSetCallback(canvas, "ACTION", Icallback("redraw_cb"))

    timer = IupTimer(Icallback("timer_cb"), 500)

    dlg = IupDialog(canvas)
    IupSetAttribute(dlg, "TITLE", "Wireworld")
    IupSetCallback(dlg, "K_ANY", Icallback("esc_close"))

    IupShow(dlg)
    IupSetAttribute(canvas, "RASTERSIZE", NULL)
    IupMainLoop()
    IupClose()
end procedure

main()
```



## PHP



```PHP

$desc = 'tH.........
.   .
  ........
.   .
Ht.. ......

      ..
tH.... .......
      ..

      ..
tH..... ......
      ..';

$steps = 30;

//fill in the world with the cells
$world = array(array());
$row = 0;
$col = 0;
foreach(str_split($desc) as $i){
    switch($i){
        case "\n":
            $row++;
            //if($col > $width) $width = $col;
            $col = 0;
            $world[] = array();
            break;
        case '.':
            $world[$row][$col] = 1;//conductor
            $col++;
            break;
        case 'H':
            $world[$row][$col] = 2;//head
            $col++;
            break;
        case 't':
            $world[$row][$col] = 3;//tail
            $col++;
            break;
        default:
            $world[$row][$col] = 0;//insulator/air
            $col++;
            break;
    };
};
function draw_world($world){
    foreach($world as $rowc){
        foreach($rowc as $cell){
            switch($cell){
                case 0:
                    echo ' ';
                    break;
                case 1:
                    echo '.';
                    break;
                case 2:
                    echo 'H';
                    break;
                case 3:
                    echo 't';
            };
        };
        echo "\n";
    };
    //var_export($world);
};
echo "Original world:\n";
draw_world($world);
for($i = 0; $i < $steps; $i++){
    $old_world = $world; //backup to look up where was an electron head
    foreach($world as $row => &$rowc){
        foreach($rowc as $col => &$cell){
            switch($cell){
                case 2:
                    $cell = 3;
                    break;
                case 3:
                    $cell = 1;
                    break;
                case 1:
                    $neigh_heads = (int) @$old_world[$row - 1][$col - 1] == 2;
                    $neigh_heads += (int) @$old_world[$row - 1][$col] == 2;
                    $neigh_heads += (int) @$old_world[$row - 1][$col + 1] == 2;
                    $neigh_heads += (int) @$old_world[$row][$col - 1] == 2;
                    $neigh_heads += (int) @$old_world[$row][$col + 1] == 2;
                    $neigh_heads += (int) @$old_world[$row + 1][$col - 1] == 2;
                    $neigh_heads += (int) @$old_world[$row + 1][$col] == 2;
                    if($neigh_heads == 1 || $neigh_heads == 2){
                        $cell = 2;
                    };
            };
        };
        unset($cell); //just to be safe
    };
    unset($rowc); //just to be safe
    echo "\nStep " . ($i + 1) . ":\n";
    draw_world($world);
};

```



## PicoLisp

This example uses 'grid' from "lib/simul.l", which maintains a two-dimensional
structure.

```PicoLisp
(load "@lib/simul.l")

(let
   (Data (in "wire.data" (make (while (line) (link @))))
      Grid (grid (length (car Data)) (length Data)) )
   (mapc
      '((G D) (mapc put G '(val .) D))
      Grid
      (apply mapcar (flip Data) list) )
   (loop
      (disp Grid T
         '((This) (pack " " (: val) " ")) )
      (wait 1000)
      (for Col Grid
         (for This Col
            (case (=: next (: val))
               ("H" (=: next "t"))
               ("t" (=: next "."))
               ("."
                  (when
                     (>=
                        2
                        (cnt # Count neighbors
                           '((Dir) (= "H" (get (Dir This) 'val)))
                           (quote
                              west east south north
                              ((X) (south (west X)))
                              ((X) (north (west X)))
                              ((X) (south (east X)))
                              ((X) (north (east X))) ) )
                        1 )
                     (=: next "H") ) ) ) ) )
      (for Col Grid  # Update
         (for This Col
            (=: val (: next)) ) )
      (prinl) ) )
```

```txt
   +---+---+---+---+---+---+---+---+---+---+---+
 5 | t | H | . | . | . | . | . | . | . | . | . |
   +---+---+---+---+---+---+---+---+---+---+---+
 4 | . |   |   |   | . |   |   |   |   |   |   |
   +---+---+---+---+---+---+---+---+---+---+---+
 3 |   |   |   | . | . | . |   |   |   |   |   |
   +---+---+---+---+---+---+---+---+---+---+---+
 2 | . |   |   |   | . |   |   |   |   |   |   |
   +---+---+---+---+---+---+---+---+---+---+---+
 1 | H | t | . | . |   | . | . | . | . | . | . |
   +---+---+---+---+---+---+---+---+---+---+---+
     a   b   c   d   e   f   g   h   i   j   k

   +---+---+---+---+---+---+---+---+---+---+---+
 5 | . | t | H | . | . | . | . | . | . | . | . |
   +---+---+---+---+---+---+---+---+---+---+---+
 4 | H |   |   |   | . |   |   |   |   |   |   |
   +---+---+---+---+---+---+---+---+---+---+---+
 3 |   |   |   | . | . | . |   |   |   |   |   |
   +---+---+---+---+---+---+---+---+---+---+---+
 2 | H |   |   |   | . |   |   |   |   |   |   |
   +---+---+---+---+---+---+---+---+---+---+---+
 1 | t | . | . | . |   | . | . | . | . | . | . |
   +---+---+---+---+---+---+---+---+---+---+---+
     a   b   c   d   e   f   g   h   i   j   k

   +---+---+---+---+---+---+---+---+---+---+---+
 5 | H | . | t | H | . | . | . | . | . | . | . |
   +---+---+---+---+---+---+---+---+---+---+---+
 4 | t |   |   |   | . |   |   |   |   |   |   |
   +---+---+---+---+---+---+---+---+---+---+---+
 3 |   |   |   | . | . | . |   |   |   |   |   |
   +---+---+---+---+---+---+---+---+---+---+---+
 2 | t |   |   |   | . |   |   |   |   |   |   |
   +---+---+---+---+---+---+---+---+---+---+---+
 1 | . | H | . | . |   | . | . | . | . | . | . |
   +---+---+---+---+---+---+---+---+---+---+---+
     a   b   c   d   e   f   g   h   i   j   k
```



## PureBasic


### Standalone version


```PureBasic
Enumeration
   #Empty
   #Electron_head
   #Electron_tail
   #Conductor
EndEnumeration

#Delay=100
#XSize=23
#YSize=12

Procedure Limit(n, min, max)
  If n<min
    n=min
  ElseIf n>max
    n=max
  EndIf
  ProcedureReturn n
EndProcedure

Procedure Moore_neighborhood(Array World(2),x,y)
  Protected cnt=0, i, j
  For i=Limit(x-1, 0, #XSize) To Limit(x+1, 0, #XSize)
    For j=Limit(y-1, 0, #YSize) To Limit(y+1, 0, #YSize)
      If World(i,j)=#Electron_head
        cnt+1
      EndIf
    Next
  Next
  ProcedureReturn cnt
EndProcedure

Procedure PresentWireWorld(Array World(2))
  Protected x,y
  ;ClearConsole()
  For y=0 To #YSize
    For x=0 To #XSize
      ConsoleLocate(x,y)
      Select World(x,y)
        Case #Electron_head
          ConsoleColor(12,0): Print("#")
        Case #Electron_tail
          ConsoleColor(4,0): Print("#")
        Case #Conductor
          ConsoleColor(6,0): Print("#")
        Default
          ConsoleColor(15,0): Print(" ")
      EndSelect
    Next
    PrintN("")
  Next
EndProcedure

Procedure UpdateWireWorld(Array World(2))
  Dim NewArray(#XSize,#YSize)
  Protected i, j
  For i=0 To #XSize
    For j=0 To #YSize
      Select World(i,j)
        Case #Electron_head
          NewArray(i,j)=#Electron_tail
        Case #Electron_tail
          NewArray(i,j)=#Conductor
        Case #Conductor
          Define m=Moore_neighborhood(World(),i,j)
          If m=1 Or m=2
            NewArray(i,j)=#Electron_head
          Else
            NewArray(i,j)=#Conductor
          EndIf
        Default ; e.g. should be Empty
          NewArray(i,j)=#Empty
      EndSelect
    Next
  Next
  CopyArray(NewArray(),World())
EndProcedure

If OpenConsole()
  EnableGraphicalConsole(#True)
  ConsoleTitle("XOR() WireWorld")
  ;- Set up the WireWorld
  Dim WW.i(#XSize,#YSize)
  Define x, y
  Restore StartWW
  For y=0 To #YSize
    For x=0 To #XSize
      Read.i WW(x,y)
    Next
  Next

  ;- Start the WireWorld simulation
  Repeat
    PresentWireWorld(WW())
    UpdateWireWorld(WW())
    Delay(#Delay)
  ForEver
EndIf

DataSection
  StartWW:
  Data.i  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
  Data.i  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
  Data.i  0,0,0,3,3,3,3,2,1,3,3,0,0,0,0,0,0,0,0,0,0,0,0,0
  Data.i  0,0,1,0,0,0,0,0,0,0,0,3,3,3,3,3,3,0,0,0,0,0,0,0
  Data.i  0,0,0,2,3,3,3,3,3,3,3,0,0,0,0,0,0,3,0,0,0,0,0,0
  Data.i  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,3,3,3,3,0,0,0,0
  Data.i  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,3,0,0,3,3,3,3,3
  Data.i  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,3,3,3,3,0,0,0,0
  Data.i  0,0,0,3,3,3,3,3,3,3,3,0,0,0,0,0,0,3,0,0,0,0,0,0
  Data.i  0,0,1,0,0,0,0,0,0,0,0,3,3,3,3,3,3,0,0,0,0,0,0,0
  Data.i  0,0,0,2,3,3,3,3,1,2,3,0,0,0,0,0,0,0,0,0,0,0,0,0
  Data.i  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
  Data.i  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
EndDataSection
```

===Load from external source, graphical presentations===

```PureBasic
CompilerIf #PB_Compiler_Unicode
  CompilerError "The file handling in this small program is only in ASCII."
CompilerEndIf

Enumeration
  #Empty
  #Electron_head
  #Electron_tail
  #Conductor
  #COL_Empty          = $000000
  #COL_Electron_head  = $5100FE
  #COL_Electron_tail  = $6A3595
  #COL_Conductor      = $62C4FF
  #WW_Window  = 0
  #WW_IGadget = 0
  #WW_Timer   = 0
  #WW_Image   = 0
EndEnumeration

#Delay=100
Global XSize, YSize

Procedure Limit(n, min, max)
  If     n<min: n=min
  ElseIf n>max: n=max
  EndIf
  ProcedureReturn n
EndProcedure

Procedure Moore_neighborhood(Array World(2),x,y)
  Protected cnt=0, i, j
  For i=Limit(x-1, 0, XSize) To Limit(x+1, 0, XSize)
    For j=Limit(y-1, 0, YSize) To Limit(y+1, 0, YSize)
      If World(i,j)=#Electron_head
        cnt+1
      EndIf
    Next
  Next
  ProcedureReturn cnt
EndProcedure

Procedure PresentWireWorld(Array World(2))
  Protected x,y
  StartDrawing(ImageOutput(#WW_Image))
  For y=0 To YSize-1
    For x=0 To XSize-1
      Select World(x,y)
        Case #Electron_head
          Plot(x,y,#COL_Electron_head)
        Case #Electron_tail
          Plot(x,y,#COL_Electron_tail)
        Case #Conductor
          Plot(x,y,#COL_Conductor)
        Default
          Plot(x,y,#COL_Empty)
      EndSelect
    Next
  Next
  StopDrawing()
  ImageGadget(#WW_IGadget,0,0,XSize,YSize,ImageID(#WW_Image))
EndProcedure

Procedure UpdateWireWorld(Array World(2))
  Dim NewArray(XSize,YSize)
  Protected i, j
  For i=0 To XSize
    For j=0 To YSize
      Select World(i,j)
        Case #Electron_head
          NewArray(i,j)=#Electron_tail
        Case #Electron_tail
          NewArray(i,j)=#Conductor
        Case #Conductor
          Define m=Moore_neighborhood(World(),i,j)
          If m=1 Or m=2
            NewArray(i,j)=#Electron_head
          Else
            NewArray(i,j)=#Conductor
          EndIf
        Default ; e.g. should be Empty
          NewArray(i,j)=#Empty
      EndSelect
    Next
  Next
  CopyArray(NewArray(),World())
EndProcedure

Procedure LoadDataFromFile(File$,Array A(2))
  Define Line$, x, y, *c.Character
  If OpenFile(0,File$)
    ;
    ; Count non-commented lines & length of the first line, e.g. get Array(x,y)
    While Not Eof(0)
      Line$=Trim(ReadString(0))
      *c=@Line$
      If Not PeekC(*c)=';'
        y+1
        If Not x
          While PeekC(*c)>='0' And PeekC(*c)<='3'
            x+1:  *c+1
          Wend
        EndIf
      EndIf
    Wend
    XSize=x:  YSize=y
    Dim A(XSize,YSize)
    ;
    ; Read in the Wire-World
    y=0
    FileSeek(0,0)
    While Not Eof(0)
      Line$=Trim(ReadString(0))
      *c=@Line$
      If Not PeekC(*c)=';'
        x=0
        While x<XSize
          A(x,y)=PeekC(*c)-'0'
          x+1: *c+1
        Wend
        y+1
      EndIf
    Wend
    CloseFile(0)
  EndIf
EndProcedure

#Title="WireWorld, PureBasic"
If OpenWindow(#WW_Window,0,0,XSize,YSize,#Title,#PB_Window_SystemMenu)
  Dim WW.i(0,0)
  Define Pattern$ = "Text (*.txt)|*.txt", Pattern = 0
  Define DefFile$ = "WireWorld.txt", Event
  Define Title$   = "Please choose file To load"
  Define File$ = OpenFileRequester(Title$, DefFile$, Pattern$, Pattern)
  AddWindowTimer(#WW_Window,#WW_Timer,#Delay)
  LoadDataFromFile(File$,WW())
  ResizeWindow(#WW_Window,0,0,XSize,YSize)
  CreateImage(#WW_Image,XSize,YSize)
  Repeat
    Event=WaitWindowEvent()
    If Event=#PB_Event_Timer
      PresentWireWorld(WW())
      UpdateWireWorld (WW())
    EndIf
  Until Event=#PB_Event_CloseWindow
EndIf
```

Example of data file to load

```txt
; Save as "WireWorld.txt"
;
; ;=Comment
; 0=Empty Cell
; 1=Electron Head
; 2=Electron Tail
; 3=Conductor
;
; All lines nees to be of the same length,
; and containing only the defined values.
;
;
; Start of World
;
000000000000000000000000000000000000000000030030000000000000000000000000
000000000000000000000000000000000000000000300030000000000000000000000000
000333321330000000000000000000000033330003000030000000000000000000000000
001000000003333330000000000000000030030030000030000000000000000000000000
000233333330000003000000000000000030003300033330000000000000000000000000
000000000000000033330000333000000030000000030003330000000000000000000000
000000000000000030033333300333333333333333333333003333333333333333333333
000000000000000033330000333000000000000000000003330000030000000000000000
000333333330000003000000000000000000000000000000000000030000000000000000
001000000003333330000000033333330033333330000000000000030000000000000000
000233331230000000000000030000030030000030000000000033333333300000000000
000000000000000000000000030000030030000300000000000300000003000000000000
000000000000000000000000033333330033333000000000000003000033000000000000
000000000000000000000000030000000030000300000000000000333030000000000000
000000000000000000000000030000000030000030000000000000300033333333333330
000333321330000000000000030000000030000030003330000000333000000000000000
001000000003333330000000030000000003333300003330000000000000000000000000
000233333330000003000000300000000003000000003000000000000000000000000000
000000000000000033330003000033000030000000030000000000000000000000000000
000000000000000030033333333330333333333333333333333333333333333333333333
000000000000000033330000000033003000000000000000300000000000000000000000
000333333330000003000000000000003000000000000000300000000000000000000000
001000000003333330000000000000003000000000000000300000000000000000000000
000233331230000000000000000000003330000000000000300000000000000000000000
000000000000000000000000000000000030000000000000300000000000000000000000
000000000000000000000000000000000030000000000003333000000000000000000000
000000000000000000000000000000000030000000000003003333333333333333333333
000000000000000000000000000000000030000000000003333000000000000000000000
000333321330000000000000000000000003333000000000300000000000000000000000
001000000003333330000000000000000003003000000000300000000000000000000000
000233333330000003000000000000000003003333333333300000000000000000000000
000000000000000033330000330000000003000000000000000000000000000000000000
000000000000000030033333303333333333333333333333333333333333333333333333
000000000000000033330000330000000000000000000000000000000000000000000003
000333333330000003000000000000000000000000000000000000000000000000000003
001000000003333330000000003333333333333333333333333333333333333333333003
000233331230000000000000030000000000000000000000000000000000000000000003
000000000000000333333333333333333333333333333333333333333333333333333333
000000000000000300000000000000000000000000000000000000000000000000000000
```



## Python



```python
'''
Wireworld implementation.
'''

from io import StringIO
from collections import namedtuple
from pprint import pprint as pp
import copy

WW = namedtuple('WW', 'world, w, h')
head, tail, conductor, empty = allstates = 'Ht. '


infile = StringIO('''\
tH.........
.   .
   ...
.   .
Ht.. ......\
''')

def readfile(f):
    '''file > initial world configuration'''
    world  = [row.rstrip('\r\n') for row in f]
    height = len(world)
    width  = max(len(row) for row in world)
    # fill right and frame in empty cells
    nonrow = [ " %*s " % (-width, "") ]
    world  = nonrow + \
               [ " %*s " % (-width, row) for row in world ] + \
               nonrow
    world = [list(row) for row in world]
    return WW(world, width, height)

def newcell(currentworld, x, y):
    istate = currentworld[y][x]
    assert istate in allstates, 'Wireworld cell set to unknown value "%s"' % istate
    if istate == head:
        ostate = tail
    elif istate == tail:
        ostate = conductor
    elif istate == empty:
        ostate = empty
    else: # istate == conductor
        n = sum( currentworld[y+dy][x+dx] == head
                 for dx,dy in ( (-1,-1), (-1,+0), (-1,+1),
                                (+0,-1),          (+0,+1),
                                (+1,-1), (+1,+0), (+1,+1) ) )
        ostate = head if 1 <= n <= 2 else conductor
    return ostate

def nextgen(ww):
    'compute next generation of wireworld'
    world, width, height = ww
    newworld = copy.deepcopy(world)
    for x in range(1, width+1):
        for y in range(1, height+1):
            newworld[y][x] = newcell(world, x, y)
    return WW(newworld, width, height)

def world2string(ww):
    return '\n'.join( ''.join(row[1:-1]).rstrip() for row in ww.world[1:-1] )

ww = readfile(infile)
infile.close()

for gen in range(10):
    print ( ("\n%3i " % gen) + '=' * (ww.w-4) + '\n' )
    print ( world2string(ww) )
    ww = nextgen(ww)
```


<pre style="height:45ex;overflow:scroll">
  0
### =


tH.........
.   .
   ...
.   .
Ht.. ......

  1
### =


.tH........
H   .
   ...
H   .
t... ......

  2
### =


H.tH.......
t   .
   ...
t   .
.H.. ......

  3
### =


tH.tH......
.   H
   ...
.   .
HtH. ......

  4
### =


.tH.tH.....
H   t
   HHH
H   .
t.tH ......

  5
### =


H.tH.tH....
t   .
   ttt
t   .
.H.t ......

  6
### =


tH.tH.tH...
.   H
   ...
.   .
HtH. ......

  7
### =


.tH.tH.tH..
H   t
   HHH
H   .
t.tH ......

  8
### =


H.tH.tH.tH.
t   .
   ttt
t   .
.H.t ......

  9
### =


tH.tH.tH.tH
.   H
   ...
.   .
HtH. ......
```



## Racket



```racket

#lang racket
(require 2htdp/universe)
(require 2htdp/image)
(require racket/fixnum)

; see the forest fire task, from which this is derived...
(define-struct wire-world (width height cells) #:prefab)

(define state:_ 0)
(define state:. 1)
(define state:H 2)
(define state:t 3)

(define (char->state c)
  (case c
    ((#\_ #\space) state:_)
    ((#\.) state:.)
    ((#\H) state:H)
    ((#\t) state:t)))

(define (initial-world l)
  (let ((h (length l))
        (w (string-length (first l))))
    (make-wire-world w h
                     (for*/fxvector
                      #:length (* h w)
                      ((row (in-list l))
                       (cell (in-string row)))
                      (char->state cell)))))

(define initial-list
  '("tH........."
    ".   .      "
    "   ...     "
    ".   .      "
    "Ht.. ......"))

(define-syntax-rule (count-neighbours-in-state ww wh wc r# c# state-to-match)
  (for/sum
      ((r (in-range (- r# 1) (+ r# 2)))
       #:when (< -1 r wh)
       (c (in-range (- c# 1) (+ c# 2)))
       #:when (< -1 c ww)
       ;; note, this will check cell at (r#, c#), too but it's not
       ;; worth checking that r=r# and c=c# each time in
       ;; this case, we know that (r#, c#) is a conductor:
       ; #:unless (and (= r# r) (= c# c))
       (i (in-value (+ (* r ww) c)))
       #:when (= state-to-match (fxvector-ref wc i)))
    1))

(define (cell-new-state ww wh wc row col)
  (let ((cell (fxvector-ref wc (+ col (* row ww)))))
    (cond
      ((= cell state:_) cell) ; empty -> empty
      ((= cell state:t) state:.) ; tail -> empty
      ((= cell state:H) state:t) ; head -> tail
      ((<= 1 (count-neighbours-in-state ww wh wc row col state:H) 2) state:H)
      (else cell))))

(define (wire-world-tick world)
  (define ww (wire-world-width world))
  (define wh (wire-world-height world))
  (define wc (wire-world-cells world))

  (define (/w x) (quotient x ww))
  (define (%w x) (remainder x ww))

  (make-wire-world
   ww wh
  (for/fxvector
   #:length (* ww wh)
   ((cell (in-fxvector wc))
    (r# (sequence-map /w (in-naturals)))
    (c# (sequence-map %w (in-naturals))))
   (cell-new-state ww wh wc r# c#))))

(define colour:_ (make-color   0   0   0))  ; black
(define colour:. (make-color  128 128 128)) ; grey
(define colour:H (make-color  128 255 255)) ; bright cyan
(define colour:t (make-color    0 128 128)) ; dark cyan

(define colour-vector (vector colour:_ colour:. colour:H colour:t))
(define (cell-state->colour state) (vector-ref colour-vector state))

(define render-scaling 20)
(define (render-world W)
  (define ww (wire-world-width W))
  (define wh (wire-world-height W))
  (define wc (wire-world-cells W))
   (let* ((flat-state
           (for/list ((cell (in-fxvector wc)))
             (cell-state->colour cell))))
     (place-image (scale render-scaling (color-list->bitmap flat-state ww wh))
                  (* ww (/ render-scaling 2))
                  (* wh (/ render-scaling 2))
                  (empty-scene (* render-scaling ww) (* render-scaling wh)))))

(define (run-wire-world #:initial-state W)
  (big-bang
   (initial-world W) ;; initial state
   [on-tick wire-world-tick
            1/8 ; tick time (seconds)
            ]
   [to-draw render-world]))

(run-wire-world #:initial-state initial-list)

```



## REXX


```rexx
/*REXX program  displays a   wire world   Cartesian grid   of  four─state  cells.       */
parse arg  iFID .  '('  generations  rows  cols  bare  head  tail  wire  clearScreen  reps
if iFID==''  then iFID= "WIREWORLD.TXT"          /*should default input file  be used?  */
        bla = 'BLANK'                            /*the "name" for a blank.              */
generations = p(generations     100   )          /*number generations that are allowed. */
       rows = p(rows            3     )          /*the number of cell  rows.            */
       cols = p(cols            3     )          /* "     "    "   "   columns.         */
       bare = pickChar(bare     bla   )          /*character used to show an empty cell.*/
clearScreen = p(clearScreen     0     )          /*1    means to clear the screen.      */
       head = pickChar(head     'H'   )          /*pick the character for the  head.    */
       tail = pickChar(tail     't'   )          /*  "   "      "      "   "   tail.    */
       wire = pickChar(wire     .     )          /*  "   "      "      "   "   wire.    */
       reps = p(reps            2     )          /*stop program  if there are 2 repeats.*/
fents=max(cols, linesize() - 1)                  /*the fence width used after displaying*/
#reps= 0;     $.= bare;   gens= abs(generations) /*at start, universe is new and barren.*/
                                                 /* [↓]     read the input file.        */
       do r=1  while lines(iFID)\==0             /*keep reading until the  End─Of─File. */
       q= strip( linein(iFID), 'T')              /*get a line from input file.          */
       L= length(q);      cols= max(cols, L)     /*calculate maximum number of columns. */
          do c=1  for L;  $.r.c= substr(q, c, 1) /*assign the cells for the   R   row.  */
          end   /*c*/
       end      /*r*/
!.= 0;                      signal on halt       /*initial state of cells;  handle halt.*/
rows= r-1;     life= 0;     call showCells       /*display initial state of the cells.  */
                                                 /*watch cells evolve, 4 possible states*/
  do   life=1  for gens;    @.= bare             /*perform for the number of generations*/
     do   r=1  for rows                          /*process each of the rows.            */
       do c=1  for cols;    ?= $.r.c;    ??= ?   /*   "      "   "  "  columns.         */
                 select                          /*determine the type of cell.          */
                 when ?==head  then ??= tail
                 when ?==tail  then ??= wire
                 when ?==wire  then do;  #= hood();   if #==1 | #==2  then ??= head;   end
                 otherwise     nop
                 end   /*select*/
       @.r.c= ??                                 /*possible assign a  cell  a new state.*/
       end             /*c*/
     end               /*r*/

  call assign$                                   /*assign alternate cells ──► real world*/
  if generations>0 | life==gens  then call showCells
  end   /*life*/
                                                 /*stop watching the universe (or life).*/
halt: if life-1\==gens  then say 'The  ───Wireworld───  program was interrupted by user.'
done: exit                                       /*stick a fork in it,  we are all done.*/
/*───────────────────────────────────────────────────────────────────────────────────────────────────────────────────*/
$:         parse arg _row,_col;                  return  $._row._col==head
assign$:   do r=1  for rows;   do c=1  for cols;   $.r.c= @.r.c;   end;   end;      return
hood:      return  $(r-1,c-1)  + $(r-1,c)  + $(r-1,c+1)  + $(r,c-1)  + $(r,c+1)  + $(r+1,c-1)  + $(r+1,c)  + $(r+1,c+1)
p:         return word(arg(1), 1)                           /*pick the 1st word in list.*/
pickChar:  parse arg _ .;arg U .;L=length(_);if U==bla then _=' '; if L==3 then _=d2c(_);if L==2 then _=x2c(_);return _
showRows:  _=;  do r=1  for rows; z=;  do c=1 for cols; z= z||$.r.c; end;  z= strip(z,'T'); say z; _= _||z; end; return
/*──────────────────────────────────────────────────────────────────────────────────────*/
showCells: if clearScreen  then 'CLS'                       /*◄──change CLS for the host*/
           call showRows                                    /*show rows in proper order.*/
           say right( copies('═', fents)life, fents)        /*display a title for cells.*/
           if _==''                   then signal done      /*No life?   Then stop run. */
           if !._                     then #reps= #reps + 1 /*detected repeated pattern.*/
           !._= 1                                           /*it is now an extant state.*/
           if reps\==0 & #reps<=reps  then return           /*so far, so good,  no reps.*/
           say '"Wireworld" repeated itself'    reps    "times,  the program is stopping."
           signal done                                      /*jump to this pgm's "exit".*/
```

Programming note:   the   '''hood'''   subroutine (above) could be optimized for speed by setting some short-circuit values   <code>('''r-1''', '''c-1''', '''r+1''', and '''c+1''') </code>   and using those values in the subsequent expressions.

This REXX program makes use of the   '''linesize'''   REXX program (or BIF) which is used to determine the screen width (or linesize) of the terminal (console).

The   '''LINESIZE.REX'''   REXX program is included here   ──►   [[LINESIZE.REX]].


(Cycle   '''0'''   (zero)   is essentially a copy of the input file.)
<pre style="height:60ex">
tH.........
.   .
   ...
.   .
Ht.. ......
════════════════════════════════════════════════════════════════════════════════════════0
.tH........
H   .
   ...
H   .
t... ......
════════════════════════════════════════════════════════════════════════════════════════1
H.tH.......
t   .
   ...
t   .
.H.. ......
════════════════════════════════════════════════════════════════════════════════════════2
tH.tH......
.   H
   ...
.   .
HtH. ......
════════════════════════════════════════════════════════════════════════════════════════3
.tH.tH.....
H   t
   HHH
H   .
t.tH ......
════════════════════════════════════════════════════════════════════════════════════════4
H.tH.tH....
t   .
   ttt
t   .
.H.t ......
════════════════════════════════════════════════════════════════════════════════════════5
tH.tH.tH...
.   H
   ...
.   .
HtH. ......
════════════════════════════════════════════════════════════════════════════════════════6
.tH.tH.tH..
H   t
   HHH
H   .
t.tH ......
════════════════════════════════════════════════════════════════════════════════════════7
H.tH.tH.tH.
t   .
   ttt
t   .
.H.t ......
════════════════════════════════════════════════════════════════════════════════════════8
tH.tH.tH.tH
.   H
   ...
.   .
HtH. ......
════════════════════════════════════════════════════════════════════════════════════════9
.tH.tH.tH.t
H   t
   HHH
H   .
t.tH ......
═══════════════════════════════════════════════════════════════════════════════════════10
H.tH.tH.tH.
t   .
   ttt
t   .
.H.t ......
═══════════════════════════════════════════════════════════════════════════════════════11
tH.tH.tH.tH
.   H
   ...
.   .
HtH. ......
═══════════════════════════════════════════════════════════════════════════════════════12
.tH.tH.tH.t
H   t
   HHH
H   .
t.tH ......
═══════════════════════════════════════════════════════════════════════════════════════13
"Wireworld" repeated itself 2 times,  the program is stopping.

```



## Ruby

See: [[Wireworld/Ruby]]


## Sidef

```ruby
var f = [[], DATA.lines.map {['', .chars..., '']}..., []];

10.times {
    say f.map { .join(" ") + "\n" }.join;
    var a = [[]];
    for y in (1 .. f.end-1) {
        var r = f[y];
        var rr = [''];
        for x in (1 .. r.end-1) {
            var c = r[x];
            rr << (
              given(c) {
                when('H') { 't' }
                when('t') { '.' }
                when('.') { <. H>[f.ft(y-1, y+1).map{.ft(x-1, x+1)...}.count('H') ~~ [1,2]] }
                default   { c }
              }
            )
        }
        rr << '';
        a << rr;
    }
    f = [a..., []];
}

__DATA__
tH.........
.   .
   ...
.   .
Ht.. ......
```



## Standard ML


```sml
(* Maximilian Wuttke 12.04.2016 *)

type world = char vector vector

fun getstate (w:world, (x, y)) = (Vector.sub (Vector.sub (w, y), x)) handle Subscript => #" "

fun conductor (w:world, (x, y)) =
	let
	  val s = [getstate (w, (x-1, y-1)) = #"H", getstate (w, (x-1, y)) = #"H", getstate (w, (x-1, y+1)) = #"H",
	           getstate (w, (x,   y-1)) = #"H",                                getstate (w, (x,   y+1)) = #"H",
	           getstate (w, (x+1, y-1)) = #"H", getstate (w, (x+1, y)) = #"H", getstate (w, (x+1, y+1)) = #"H"]
	  (* Count `true` in s *)
	  val count = List.length (List.filter (fn x => x=true) s)
	in
	  if count = 1 orelse count = 2 then #"H" else #"."
	end

fun translate (w:world, (x, y)) =
	case getstate (w, (x, y)) of
	   #" " => #" "
	 | #"H" => #"t"
	 | #"t" => #"."
	 | #"." => conductor (w, (x, y))
	 | s    => s

fun next_world (w : world) = Vector.mapi (fn (y, row) => Vector.mapi (fn (x, _) => translate (w, (x, y))) row) w


(* Test *)

(* makes a list of strings into a world *)
fun make_world (rows : string list) : world =
	Vector.fromList (map (fn (row : string) => Vector.fromList (explode row)) rows)


(* word_str reverses make_world *)
fun vec_str (r:char vector) = implode (List.tabulate (Vector.length r, fn x => Vector.sub (r, x)))
fun world_str (w:world)     = List.tabulate (Vector.length w, fn y => vec_str (Vector.sub (w, y)))
fun print_world (w:world)   = (map (fn row_str => print (row_str ^ "\n")) (world_str w); ())

val test = make_world [
	"tH.........",
	".   .      ",
	"   ...     ",
	".   .      ",
	"Ht.. ......"]
```



## Smalltalk

See: [[Wireworld/Smalltalk]]


## Tcl

See: [[Wireworld/Tcl]]


## Ursala

The board is represented as a list of character strings, and the
neighborhoods function uses the swin library function twice
to construct a two dimensional 3 by 3 sliding window.
The rule function maps a pair (cell,neighborhood) to a new cell.

```Ursala
#import std

rule          = case~&l\~&l {`H: `t!, `t: `.!,`.: @r ==`H*~; {'H','HH'}?</`H! `.!}

neighborhoods = ~&thth3hthhttPCPthPTPTX**K7S+ swin3**+ swin3@hNSPiCihNCT+ --<0>*+ 0-*

evolve "n"    = @iNC ~&x+ rep"n" ^C\~& rule**+ neighborhoods@h
```

test program:

```Ursala
diode =

<
   '        ..   ',
   'tH....... .Ht',
   '        ..   '>

#show+

example = mat0 evolve13 diode
```

<pre style="height:15ex;overflow:scroll">
        ..
tH....... .Ht
        ..

        ..
.tH...... Ht.
        ..

        .H
..tH..... t..
        .H

        Ht
...tH...H ...
        Ht

        t.
....tH..t ...
        t.

        ..
.....tH.. ...
        ..

        ..
......tH. ...
        ..

        H.
.......tH ...
        H.

        tH
........t ...
        tH

        .t
......... H..
        .t

        ..
......... tH.
        ..

        ..
......... .tH
        ..

        ..
......... ..t
        ..

        ..
......... ...
        ..

```



## XPL0

[[File:WireXPL0.gif|right]]

```XPL0
include c:\cxpl\codes;          \intrinsic 'code' declarations
char New(53,40), Old(53,40);

proc Block(X0, Y0, C);          \Display a colored block
int  X0, Y0, C;                 \big (6x5) coordinates, char
int  X, Y;
[case C of                      \convert char to color
  ^H:   C:= $9;                 \blue
  ^t:   C:= $C;                 \red
  ^.:   C:= $E                  \yellow
other   C:= 0;                  \black
for Y:= Y0*5 to Y0*5+4 do       \make square blocks by correcting aspect ratio
    for X:= X0*6 to X0*6+5 do   \ (6x5 = square)
        Point(X,Y,C);
];

int X, Y, C;
[SetVid($13);                   \set 320x200 graphics display
for Y:= 0 to 40-1 do            \initialize New with space (empty) characters
    for X:= 0 to 53-1 do
        New(X, Y):= ^ ;
X:= 1;  Y:= 1;                  \read file from command line, skipping borders
loop    [C:= ChIn(1);
        case C of
          $0D:  X:= 1;          \carriage return
          $0A:  Y:= Y+1;        \line feed
          $1A:  quit            \end of file
        other   [New(X,Y):= C;  X:= X+1];
        ];
repeat  C:= Old;  Old:= New;  New:= C;  \swap arrays, by swapping their pointers
        for Y:= 1 to 39-1 do            \generate New array from Old
            for X:= 1 to 52-1 do        \ (skipping borders)
                [case Old(X,Y) of
                  ^ :   New(X,Y):= ^ ;  \copy empty to empty
                  ^H:   New(X,Y):= ^t;  \convert head to tail
                  ^t:   New(X,Y):= ^.   \convert tail to conductor
                other   [C:= (Old(X-1,Y-1)=^H) + (Old(X+0,Y-1)=^H) + \head count
                             (Old(X+1,Y-1)=^H) + (Old(X-1,Y+0)=^H) + \ in neigh-
                             (Old(X+1,Y+0)=^H) + (Old(X-1,Y+1)=^H) + \ boring
                             (Old(X+0,Y+1)=^H) + (Old(X+1,Y+1)=^H);  \ cells
                        New(X,Y):= if C=-1 or C=-2 then ^H else ^.;  \ (true=-1)
                        ];
                Block(X, Y, New(X,Y));  \display result
                ];
        Sound(0, 6, 1);                 \delay about 1/3 second
until   KeyHit;                         \keystroke terminates program
SetVid(3);                              \restore normal text mode
]
```



## Yabasic


```Yabasic
open window 230,130
backcolor 0,0,0
clear window

label circuit
	DATA "             "
	DATA " tH......... "
	DATA " .   .       "
	DATA "    ...      "
	DATA " .   .       "
	DATA " Ht.. ...... "
	DATA "             "
	DATA ""

do
	read a$
	if a$ = "" break
	n = n + 1
	redim t$(n)
	t$(n) = a$+a$
loop

size = len(t$(1))/2
E2 = size
first = true
Orig = 0
Dest = E2

do
  for y = 2 to n-1
    for x = 2 to E2-1
      switch mid$(t$(y),x+Orig,1)
        case " ": color 32,32,32 : mid$(t$(y),x+Dest,1) = " " : break
        case "H": color 0,0,255 : mid$(t$(y),x+Dest,1) = "t" : break
        case "t": color 255,0,0 : mid$(t$(y),x+Dest,1) = "." : break
        case ".":
          color 255,200,0
          t = 0
          for y1 = y-1 to y+1
          	for x1 = x-1 to x+1
          		t = t + ("H" = mid$(t$(y1),x1+Orig,1))
          	next x1
          next y1
          if t=1 or t=2 then
          	mid$(t$(y),x+Dest,1) = "H"
          else
          	mid$(t$(y),x+Dest,1) = "."
          end if
      end switch
      fill circle x*16, y*16, 8
    next x
    print t$(y),"="
  next y
  first = not first
  if first then
  	Orig = 0 : Dest = E2
  else
  	Orig = E2 : Dest = 0
  end if
  wait .5
loop

```

