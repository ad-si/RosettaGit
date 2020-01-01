+++
title = "Universal Turing machine"
description = ""
date = 2019-10-12T13:32:23Z
aliases = []
[extra]
id = 12892
[taxonomies]
categories = []
tags = []
+++

{{task}}
One of the foundational mathematical constructs behind computer science
is the [[wp:Universal Turing machine|universal Turing Machine]].

Indeed one way to definitively prove that a language
is [[wp:Turing_completeness|turing-complete]]
is to implement a universal Turing machine in it.


;Task:

Simulate such a machine capable
of taking the definition of any other Turing machine and executing it.

Of course, you will not have an infinite tape,
but you should emulate this as much as is possible.

The three permissible actions on the tape are "left", "right" and "stay".

To test your universal Turing machine (and prove your programming language
is Turing complete!), you should execute the following two Turing machines
based on the following definitions.


'''Simple incrementer'''
* '''States:''' q0, qf
* '''Initial state:''' q0
* '''Terminating states:''' qf
* '''Permissible symbols:''' B, 1
* '''Blank symbol:''' B
* '''Rules:'''
** (q0, 1, 1, right, q0)
** (q0, B, 1, stay,  qf)



The input for this machine should be a tape of <code>1 1 1</code>


'''Three-state busy beaver'''
* '''States:''' a, b, c, halt
* '''Initial state:''' a
* '''Terminating states:''' halt
* '''Permissible symbols:''' 0, 1
* '''Blank symbol:''' 0
* '''Rules:'''
** (a, 0, 1, right, b)
** (a, 1, 1, left,  c)
** (b, 0, 1, left,  a)
** (b, 1, 1, right, b)
** (c, 0, 1, left,  b)
** (c, 1, 1, stay,  halt)



The input for this machine should be an empty tape.


'''Bonus:'''

'''5-state, 2-symbol probable Busy Beaver machine from Wikipedia'''
* '''States:''' A, B, C, D, E, H
* '''Initial state:''' A
* '''Terminating states:''' H
* '''Permissible symbols:''' 0, 1
* '''Blank symbol:''' 0
* '''Rules:'''
** (A, 0, 1, right, B)
** (A, 1, 1, left,  C)
** (B, 0, 1, right, C)
** (B, 1, 1, right, B)
** (C, 0, 1, right, D)
** (C, 1, 0, left,  E)
** (D, 0, 1, left,  A)
** (D, 1, 1, left,  D)
** (E, 0, 1, stay,  H)
** (E, 1, 0, left,  A)



The input for this machine should be an empty tape.

This machine runs for more than 47 millions steps.





## Ada



### The specification of the universal machine

Note that due to Ada's strict type system, a machine cannot be compiled
if there is not _exactly_ one rule for each state/symbol pair.
Thus, the specified machine is always deterministic.

The execution of the machine, i.e., the procedure Run, allows to define a number Max_Steps, after which the execution stops -- when, e.g., the specified machine runs infinitively. The procedure also allows to optionally output the configuration of the machine before every step.


```Ada
private with Ada.Containers.Doubly_Linked_Lists;

generic
   type State is (<>);   -- State'First is starting state
   type Symbol is (<>);  -- Symbol'First is blank
package Turing is

   Start: constant State := State'First;
   Halt:  constant State := State'Last;
   subtype Action_State is State range Start .. State'Pred(Halt);

   Blank: constant Symbol := Symbol'First;

   type Movement is (Left, Stay, Right);

   type Action is record
      New_State: State;
      Move_To: Movement;
      New_Symbol: Symbol;
   end record;

   type Rules_Type is array(Action_State, Symbol) of Action;

   type Tape_Type is limited private;

   type Symbol_Map is array(Symbol) of Character;

   function To_String(Tape: Tape_Type; Map: Symbol_Map) return String;
   function Position_To_String(Tape: Tape_Type; Marker: Character := '^')
                              return String;
   function To_Tape(Str: String; Map: Symbol_Map) return Tape_Type;

   procedure Single_Step(Current: in out State;
                         Tape: in out Tape_Type;
                         Rules: Rules_Type);

   procedure Run(The_Tape: in out Tape_Type;
                 Rules: Rules_Type;
                 Max_Steps: Natural := Natural'Last;
                 Print: access procedure(Tape: Tape_Type; Current: State));
   -- runs from Start State until either Halt or # Steps exceeds Max_Steps
   -- if # of steps exceeds Max_Steps, Constrained_Error is raised;
   -- if Print is not null, Print is called at the beginning of each step

private
   package Symbol_Lists is new Ada.Containers.Doubly_Linked_Lists(Symbol);
   subtype List is Symbol_Lists.List;

   type Tape_Type is record
      Left:  List;
      Here:  Symbol;
      Right: List;
   end record;
end Turing;
```



### The implementation of the universal machine



```Ada
package body Turing is

   function List_To_String(L: List; Map: Symbol_Map) return String is
      LL: List := L;
      use type List;
   begin
      if L = Symbol_Lists.Empty_List then
         return "";
      else
         LL.Delete_First;
         return Map(L.First_Element) & List_To_String(LL, Map);
      end if;
   end List_To_String;

   function To_String(Tape: Tape_Type; Map: Symbol_Map) return String is

   begin
      return List_To_String(Tape.Left, Map) & Map(Tape.Here) &
        List_To_String(Tape.Right, Map);
   end To_String;

   function Position_To_String(Tape: Tape_Type; Marker: Character := '^')
                              return String is
      Blank_Map: Symbol_Map := (others => ' ');
   begin
      return List_To_String(Tape.Left, Blank_Map) & Marker &
        List_To_String(Tape.Right, Blank_Map);
   end Position_To_String;

   function To_Tape(Str: String; Map: Symbol_Map) return Tape_Type is
      Char_Map: array(Character) of Symbol := (others => Blank);
      Tape: Tape_Type;
   begin
      if Str = "" then
         Tape.Here := Blank;
      else
         for S in Symbol loop
            Char_Map(Map(S)) := S;
         end loop;
         Tape.Here := Char_Map(Str(Str'First));
         for I in Str'First+1 .. Str'Last loop
            Tape.Right.Append(Char_Map(Str(I)));
         end loop;
      end if;
      return Tape;
      end To_Tape;

   procedure Single_Step(Current: in out State;
                         Tape: in out Tape_Type;
                         Rules: Rules_Type) is
      Act: Action := Rules(Current, Tape.Here);
      use type List; -- needed to compare Tape.Left/Right to the Empty_List
   begin
      Current := Act.New_State;     -- 1. update State
      Tape.Here := Act.New_Symbol;  -- 2. write Symbol to Tape
      case Act.Move_To is           -- 3. move Tape to the Left/Right or Stay
         when Left =>
            Tape.Right.Prepend(Tape.Here);
            if Tape.Left /= Symbol_Lists.Empty_List then
               Tape.Here := Tape.Left.Last_Element;
               Tape.Left.Delete_Last;
            else
               Tape.Here := Blank;
            end if;
        when Stay =>
            null; -- Stay where you are!
         when Right =>
            Tape.Left.Append(Tape.Here);
            if Tape.Right /= Symbol_Lists.Empty_List then
               Tape.Here := Tape.Right.First_Element;
               Tape.Right.Delete_First;
            else
               Tape.Here := Blank;
            end if;
      end case;
   end Single_Step;

   procedure Run(The_Tape: in out Tape_Type;
                 Rules: Rules_Type;
                 Max_Steps: Natural := Natural'Last;
                 Print: access procedure (Tape: Tape_Type; Current: State)) is
      The_State: State     := Start;
      Steps:     Natural   := 0;
   begin
      Steps := 0;
      while (Steps <= Max_Steps) and (The_State /= Halt) loop
         if Print /= null then
            Print(The_Tape, The_State);
         end if;
         Steps := Steps + 1;
         Single_Step(The_State, The_Tape, Rules);
      end loop;
      if The_State /= Halt then
         raise Constraint_Error;
      end if;
   end Run;

end Turing;
```




### The implementation of the simple incrementer



```Ada
with Ada.Text_IO, Turing;

procedure Simple_Incrementer is

   type States is (Start, Stop);
   type Symbols is (Blank, One);

   package UTM is new Turing(States, Symbols);
   use UTM;

   Map: Symbol_Map := (One => '1', Blank => '_');

   Rules: Rules_Type :=
     (Start => (One   => (Start, Right,  One),
                Blank => (Stop,  Stay,  One)));
   Tape:  Tape_Type := To_Tape("111", Map);

   procedure Put_Tape(Tape: Tape_Type; Current: States) is
   begin
     Ada.Text_IO.Put_Line(To_String(Tape, Map) & "  " & States'Image(Current));
     Ada.Text_IO.Put_Line(Position_To_String(Tape));
   end Put_Tape;

begin
   Run(Tape, Rules, 20, null); -- don't print the configuration during running
   Put_Tape(Tape, Stop);       -- print the final configuration
end Simple_Incrementer;
```


{{out}}


```txt
1111  STOP
   ^
```



### The implementation of the busy beaver



```Ada
with Ada.Text_IO, Turing;

procedure Busy_Beaver_3 is

   type States is (A, B, C, Stop);
   type Symbols is range 0 .. 1;
   package UTM is new Turing(States, Symbols); use UTM;

   Map: Symbol_Map := (1 => '1', 0 => '0');

   Rules: Rules_Type :=
     (A => (0 => (New_State => B, Move_To => Right, New_Symbol => 1),
            1 => (New_State => C, Move_To => Left,  New_Symbol => 1)),
      B => (0 => (New_State => A, Move_To => Left,  New_Symbol => 1),
            1 => (New_State => B, Move_To => Right, New_Symbol => 1)),
      C => (0 => (New_State => B, Move_To => Left,  New_Symbol => 1),
            1 => (New_State => Stop, Move_To => Stay, New_Symbol => 1)));

   Tape:  Tape_Type := To_Tape("", Map);

   procedure Put_Tape(Tape: Tape_Type; Current: States) is
   begin
      Ada.Text_IO.Put_Line(To_String(Tape, Map) & "  " &
                             States'Image(Current));
      Ada.Text_IO.Put_Line(Position_To_String(Tape));
   end Put_Tape;

begin
   Run(Tape, Rules, 20, Put_Tape'Access); -- print configuration before each step
   Put_Tape(Tape, Stop);                  -- and print the final configuration
end Busy_Beaver_3;
```


{{out}}

```txt
0  A
^
10  B
 ^
11  A
^
011  C
^
0111  B
^
01111  A
^
11111  B
 ^
11111  B
  ^
11111  B
   ^
11111  B
    ^
111110  B
     ^
111111  A
    ^
111111  C
   ^
111111  STOP
   ^
```



## AutoHotkey


```autohotkey
; By Uberi, http://www.autohotkey.com/board/topic/58599-turing-machine/
SetBatchLines, -1
OnExit, Exit
SaveFilePath := A_ScriptFullPath ".ini"
; Defaults are for a 2-state_3-symbol turning machine. Format:
; machine state symbol on tape, symbol on tape | tape shift (- is left, + is right, 0 is halt) | machine state
, Rule1 := "A0,1|1|B"
, Rule2 := "A1,2|-1|A"
, Rule3 := "A2,1|-1|A"
, Rule4 := "B0,2|-1|A"
, Rule5 := "B1,2|1|B"
, Rule6 := "B2,0|1|A"
; no error check is run on this input, so be sure states and symbols align with actions
IniRead, UseSaveFile, %SaveFilePath%, Global, UseSaveFile, 1 ; on exit, save state to text file so I can resume on next run
IniRead, MaxIterations, %SaveFilePath%, Global, MaxIterations, 100000 ; set as %A_Space% to run indefinitely
IniRead, Section, %SaveFilePath%, Global, Section, 2-state_3-symbol ; The name of the machine to run. Options defined:
; 2-state_3-symbol
; Simple_incrementer
; Three-state_busy_beaver
; Probable_busy_beaver_Wikipedia

IniRead, States, %SaveFilePath%, %Section%, States, A|B ; valid states
IniRead, InitialState, %SaveFilePath%, %Section%, InitialState, A ; start state
IniRead, TerminalState, %SaveFilePath%, %Section%, TerminalState, C ; end state
IniRead, Symbols, %SaveFilePath%, %Section%, Symbols, 0,1,2 ; valid symbols
IniRead, DefaultCell, %SaveFilePath%, %Section%, DefaultCell, 0 ; the default symbol of any cell not defined on input tape
IniRead, ProgramCode, %SaveFilePath%, %Section%, ProgramCode, 10101|01010 ; start tape
Iniread, RuleCount, %SaveFilePath%, %Section%, RuleCount, 6 ; number of actions to read
Loop, %RuleCount%
{
	IniRead, Temp1, %SaveFilePath%, %Section%, Rule%A_Index%, % Rule%A_Index%
	StringSplit, Temp, Temp1, `,
	Action%Temp1% := Temp2
}

IniRead, Index, %SaveFilePath%, SavedState, Index, 0
IniRead, IterationCount, %SaveFilePath%, SavedState, IterationCount, 0
IniRead, State, %SaveFilePath%, SavedState, State, %InitialState%
If IterationCount > 0
	IniRead, ProgramCode, %SaveFilePath%, SavedState, ProgramCode, %ProgramCode%

IfNotInString, ProgramCode, |
	ProgramCode := "|" ProgramCode
StringSplit, Temp, ProgramCode, |
NegativeCells := Temp1, PositiveCells := Temp2

Loop, Parse, Symbols, |
	Color%A_LoopField% := hex(mod((A_Index+1/(2**((A_Index-1)//7))-1)/7,1)*16777215) ; unlimited number of unique colors
Color%DefaultCell% := "White"

Gui, Color, Black
Gui, +ToolWindow +AlwaysOnTop +LastFound -Caption
WindowID := WinExist()
OnMessage(0x201, "WM_LBUTTONDOWN")
Gui, Font, s6 cWhite, Arial
Loop, 61 ; display 30 cell symbols on each side of current index
{
	Temp1 := ((A_Index - 1) * 15) + 1
	Gui, Add, Progress, x%Temp1% y1 w14 h40 vCell%A_Index% BackgroundWhite
	Gui, Add, Text, x%Temp1% y42 w15 h10 vLabel%A_Index% Center
}
Gui, Add, Text, x2 y54 w26 h10 vState
Gui, Add, Text, x35 y54 w50 h10 vCurrentCell
Gui, Add, Text, x350 y54 w158 h10 vActions
Gui, Add, Text, x844 y54 w33 h10, Iterations:
Gui, Add, Text, x884 y54 w29 h10 vIterations Right
Gui, Font, s4 cWhite Bold, Arial
Gui, Add, Text, x450 y1 w15 h10 Center, V
GuiControl, Move, Cell31, x451 y8 w14 h33
Gui, Show, y20 w916 h64, Wolfram's 2-State 3-Symbol Turing Machine ;'

;MaxIndex := ProgramOffset + StrLen(ProgramCode), MinIndex := ProgramOffset ; not implemented
While, ((MaxIterations = "") || IterationCount <= MaxIterations) ; process until limit is reached, if any
{
	Loop, 61 ; color each cell per its current symbol
	{ ; must run for all displayed cells because they are not directly mapped to shifting tape
		TempIndex := (Index + A_Index) - 31
		GuiControl, , Label%A_Index%, %TempIndex%
		CellColor := CellGet(TempIndex)
		, CellColor := Color%CellColor%
		GuiControl, +Background%CellColor%, Cell%A_Index%
	}
	CurrentCell := CellGet(Index)
	GuiControl, , State, State: %State%
	GuiControl, , CurrentCell, Current Cell: %CurrentCell%
	GuiControl, , Iterations, %IterationCount%
	If (State = TerminalState)
		Break

	StringSplit, Temp, Action%State%%CurrentCell%, |
	GuiControl, , Actions, % "Actions: Print " . Temp1 . ", Move " . ((Temp2 = -1) ? "left" : "right") . ", " . ((State <> Temp3) ? "Switch to state " . Temp3 : "Do not switch state")

	IterationCount++
	, CellPut(Index,Temp1)
	, Index += Temp2
	, State := Temp3
	;, (Index > MaxIndex) ? MaxIndex := Index : ""
	;, (Index < MinIndex) ? MinIndex := Index : ""

	Sleep, 0.1*1000
}
MsgBox, 64, Complete, Completed %IterationCount% iterations of the Turing machine.
Return


; Hotkeys and functions:
~Pause::Pause

GuiEscape:
GuiClose:
	ExitApp

Exit:
	If UseSaveFile
	{
		IniWrite, %Index%, %SaveFilePath%, %Section%, Index
		IniWrite, %IterationCount%, %SaveFilePath%, %Section%, IterationCount
		IniWrite, %State%, %SaveFilePath%, %Section%, State
		IniWrite, %NegativeCells%|%PositiveCells%, %SaveFilePath%, %Section%, ProgramCode
	}
	ExitApp

CellGet(Index)
{
	global NegativeCells, PositiveCells, DefaultCell
	Temp1 := (Index < 0) ? SubStr(NegativeCells,Abs(Index),1) : SubStr(PositiveCells,Index + 1,1)
	Return, (Temp1 = "") ? DefaultCell : Temp1
}

CellPut(Index,Char)
{
	global NegativeCells, PositiveCells, DefaultCell
	static StrGetFunc := "StrGet" ; workaround to hide function from AHK Basic (which does not have or require it)
	CharType := A_IsUnicode ? "UShort" : "UChar"
	, (Index < 0)
		? (Index := 0 - Index
		, Temp1 := Index - StrLen(NegativeCells)
		, (Temp1 > 0)
			? (VarSetCapacity(Pad,64) ; these three functions are quirks in AHK's memory management (not required)
			, VarSetCapacity(Pad,0)
			, VarSetCapacity(Pad,Temp1,Asc(DefaultCell))
			, NegativeCells .= A_IsUnicode ? %StrGetFunc%(&Pad,Temp1,"CP0") : Pad)
			: ""
		, NumPut(Asc(Char),NegativeCells,(Index - 1) << !!A_IsUnicode,CharType)		)
		: (Temp1 := Index - StrLen(PositiveCells) + 1
		, (Temp1 > 0)
			? (VarSetCapacity(Pad,64) ; these three functions are quirks in AHK's memory management (not required)
			, VarSetCapacity(Pad,0)
			, VarSetCapacity(Pad,Temp1,Asc(DefaultCell))
			, PositiveCells .= A_IsUnicode ? %StrGetFunc%(&Pad,Temp1,"CP0") : Pad)
			: ""
		, NumPut(Asc(Char),PositiveCells,Index << !!A_IsUnicode,CharType)		)
}

Hex(p_Integer)
{
	PtrType:=(A_PtrSize=8) ? "Ptr":"UInt"
	l_Format:="`%0" . 6 . "I64X"
	VarSetCapacity(l_Argument,8)
	NumPut(p_Integer,l_Argument,0,"Int64")
	VarSetCapacity(l_Buffer,A_IsUnicode ? 12:6,0)
	DllCall(A_IsUnicode ? "msvcrt\_vsnwprintf":"msvcrt\_vsnprintf"
		,"Str",l_Buffer ;-- Storage location for output
		,"UInt",6 ;-- Maximum number of characters to write
		,"Str",l_Format ;-- Format specification
		,PtrType,&l_Argument) ;-- Argument
	Return l_Buffer
}

WM_LBUTTONDOWN()
{
	If (A_Gui = 1)
	PostMessage, 0xA1, 2
}
```

<b>Input:</b>
Set Section below to desired machine, then save as <scriptname>.ini in the same folder.

```txt
[Global]
UseSaveFile=0
MaxIterations=100000
Section=2-state_3-symbol

[2-state_3-symbol]
States=A|B
InitialState=A
TerminalState=C
Symbols=0|1|2
DefaultCell=0
RuleCount=6
Rule1=A0,1|1|B
Rule2=A1,2|-1|A
Rule3=A2,1|-1|A
Rule4=B0,2|-1|A
Rule5=B1,2|1|B
Rule6=B2,0|1|A
ProgramCode=10101|01010

[Simple_incrementer]
States=q0|qf
InitialState=q0
TerminalState=qf
Symbols=B|1
DefaultCell=B
RuleCount=2
Rule1=q01,1|1|q0
Rule2=q0B,1|0|qf
ProgramCode=111

[Three-state_busy_beaver]
States=a|b|c|halt
InitialState=a
TerminalState=halt
Symbols=0|1
DefaultCell=0
RuleCount=6
Rule1=a0,1|1|b
Rule2=a1,1|-1|c
Rule3=b0,1|-1|a
Rule4=b1,1|1|b
Rule5=c0,1|-1|b
Rule6=c1,1|0|halt
ProgramCode=

[Probable_busy_beaver_Wikipedia]
States=A|B|C|D|E|H
InitialState=A
TerminalState=H
Symbols=0|1
DefaultCell=0
RuleCount=10
Rule1=A0,1|1|B
Rule2=A1,1|-1|C
Rule3=B0,1|1|C
Rule4=B1,1|1|B
Rule5=C0,1|1|D
Rule6=C1,0|-1|E
Rule7=D0,1|-1|A
Rule8=D1,1|-1|D
Rule9=E0,1|0|H
Rule10=E1,0|-1|A
ProgramCode=
```

{{out}} An animation of the chosen machine


## BASIC

=
## Sinclair ZX81 BASIC
=

### =The universal machine=

This program expects to find:
    • <tt>R$()</tt>, an array of rules;
    • <tt>T$</tt>, an input tape (where an empty string stands for a blank tape);
    • <tt>B$</tt>, a character to use as a blank;
    • <tt>S$</tt>, an initial state;
    • <tt>H$</tt>, a halting state.
It will execute the Turing machine these parameters describe, animating the process and highlighting the cell that is currently being read using inverse video. (See below for a link to a screenshot.) No attempt is made to check that the description is valid or that the rule set is complete.

<b>Non-universality:</b> as written, the program will fail if you try to use it with a Turing machine that has more than 256 distinct states or a tape that is longer than 704 cells. (In reality, of course, the ZX81's RAM would have been exhausted some time before you reached such a Goliath.) Allowing more states would be pretty trivial, assuming you had the memory space; just use as many bytes as you need. As for supporting a longer tape, the easiest way to do it would be to comment out the <code>PRINT</code> statements (sacrificing the animation) and add a few lines to display one screenful at a time at the very end.

```basic
1000 PRINT AT 0,0;T$
1010 LET P=1
1020 IF P>LEN T$ THEN LET T$=T$+B$
1030 PRINT AT INT (P/32),P-(32*INT (P/32)+1);CHR$ (CODE T$(P)+128)
1040 LET R=1
1050 IF R$(R,1)=S$ AND R$(R,2)=T$(P) THEN GOTO 1080
1060 LET R=R+1
1070 GOTO 1050
1080 LET T$(P)=R$(R,3)
1090 PRINT AT INT (P/32),P-(32*INT (P/32)+1);T$(P)
1100 IF R$(R,4)="L" THEN LET P=P-1
1110 IF R$(R,4)="R" THEN LET P=P+1
1120 LET S$=R$(R,5)
1130 IF S$=H$ THEN STOP
1140 IF P=0 THEN GOTO 1160
1150 GOTO 1020
1160 LET T$=B$+T$
1170 GOTO 1000
```



### =The incrementer=

Works with 1k of RAM.

```basic
10 DIM R$(2,5)
20 LET S$=CHR$ (CODE "Q"+CODE "0")
30 LET H$=CHR$ (CODE "Q"+CODE "F")
40 LET R$(1)=S$+"11R"+S$
50 LET R$(2)=S$+"B1S"+H$
60 LET B$="B"
70 LET T$="111"
```

{{out}}

```txt
1111
```


====The three-state beaver====
Requires at least 2k of RAM.

```basic
 10 DIM R$(6,5)
 20 LET R$(1)="A01RB"
 30 LET R$(2)="A11LC"
 40 LET R$(3)="B01LA"
 50 LET R$(4)="B11RB"
 60 LET R$(5)="C01LB"
 70 LET R$(6)="C11SH"
 80 LET T$=""
 90 LET S$="A"
100 LET B$="0"
110 LET H$="H"
```

{{out}}

```txt
111111
```

A screenshot from part-way through the execution of this machine can be found [http://www.edmundgriffiths.com/zx81turingmachine.jpg here].

If it is true that the <b>five-state probable beaver</b> runs for 47m cycles, then there is no point even attempting it on a slow computer like the ZX81. I don't know exactly how long it would take: but it would be months.



## C


```c
#include <stdio.h>
#include <stdarg.h>
#include <stdlib.h>
#include <string.h>

enum {
    LEFT,
    RIGHT,
    STAY
};

typedef struct {
    int state1;
    int symbol1;
    int symbol2;
    int dir;
    int state2;
} transition_t;

typedef struct tape_t tape_t;
struct tape_t {
    int symbol;
    tape_t *left;
    tape_t *right;
};

typedef struct {
    int states_len;
    char **states;
    int final_states_len;
    int *final_states;
    int symbols_len;
    char *symbols;
    int blank;
    int state;
    int tape_len;
    tape_t *tape;
    int transitions_len;
    transition_t ***transitions;
} turing_t;

int state_index (turing_t *t, char *state) {
    int i;
    for (i = 0; i < t->states_len; i++) {
        if (!strcmp(t->states[i], state)) {
            return i;
        }
    }
    return 0;
}

int symbol_index (turing_t *t, char symbol) {
    int i;
    for (i = 0; i < t->symbols_len; i++) {
        if (t->symbols[i] == symbol) {
            return i;
        }
    }
    return 0;
}

void move (turing_t *t, int dir) {
    tape_t *orig = t->tape;
    if (dir == RIGHT) {
        if (orig && orig->right) {
            t->tape = orig->right;
        }
        else {
            t->tape = calloc(1, sizeof (tape_t));
            t->tape->symbol = t->blank;
            if (orig) {
                t->tape->left = orig;
                orig->right = t->tape;
            }
        }
    }
    else if (dir == LEFT) {
        if (orig && orig->left) {
            t->tape = orig->left;
        }
        else {
            t->tape = calloc(1, sizeof (tape_t));
            t->tape->symbol = t->blank;
            if (orig) {
                t->tape->right = orig;
                orig->left = t->tape;
            }
        }
    }
}

turing_t *create (int states_len, ...) {
    va_list args;
    va_start(args, states_len);
    turing_t *t = malloc(sizeof (turing_t));
    t->states_len = states_len;
    t->states = malloc(states_len * sizeof (char *));
    int i;
    for (i = 0; i < states_len; i++) {
        t->states[i] = va_arg(args, char *);
    }
    t->final_states_len = va_arg(args, int);
    t->final_states = malloc(t->final_states_len * sizeof (int));
    for (i = 0; i < t->final_states_len; i++) {
        t->final_states[i] = state_index(t, va_arg(args, char *));
    }
    t->symbols_len = va_arg(args, int);
    t->symbols = malloc(t->symbols_len);
    for (i = 0; i < t->symbols_len; i++) {
        t->symbols[i] = va_arg(args, int);
    }
    t->blank = symbol_index(t, va_arg(args, int));
    t->state = state_index(t, va_arg(args, char *));
    t->tape_len = va_arg(args, int);
    t->tape = NULL;
    for (i = 0; i < t->tape_len; i++) {
        move(t, RIGHT);
        t->tape->symbol = symbol_index(t, va_arg(args, int));
    }
    if (!t->tape_len) {
        move(t, RIGHT);
    }
    while (t->tape->left) {
        t->tape = t->tape->left;
    }
    t->transitions_len = va_arg(args, int);
    t->transitions = malloc(t->states_len * sizeof (transition_t **));
    for (i = 0; i < t->states_len; i++) {
        t->transitions[i] = malloc(t->symbols_len * sizeof (transition_t *));
    }
    for (i = 0; i < t->transitions_len; i++) {
        transition_t *tran = malloc(sizeof (transition_t));
        tran->state1 = state_index(t, va_arg(args, char *));
        tran->symbol1 = symbol_index(t, va_arg(args, int));
        tran->symbol2 = symbol_index(t, va_arg(args, int));
        tran->dir = va_arg(args, int);
        tran->state2 = state_index(t, va_arg(args, char *));
        t->transitions[tran->state1][tran->symbol1] = tran;
    }
    va_end(args);
    return t;
}

void print_state (turing_t *t) {
    printf("%-10s ", t->states[t->state]);
    tape_t *tape = t->tape;
    while (tape->left) {
        tape = tape->left;
    }
    while (tape) {
        if (tape == t->tape) {
            printf("[%c]", t->symbols[tape->symbol]);
        }
        else {
            printf(" %c ", t->symbols[tape->symbol]);
        }
        tape = tape->right;
    }
    printf("\n");
}

void run (turing_t *t) {
    int i;
    while (1) {
        print_state(t);
        for (i = 0; i < t->final_states_len; i++) {
            if (t->final_states[i] == t->state) {
                return;
            }
        }
        transition_t *tran = t->transitions[t->state][t->tape->symbol];
        t->tape->symbol = tran->symbol2;
        move(t, tran->dir);
        t->state = tran->state2;
    }
}

int main () {
    printf("Simple incrementer\n");
    turing_t *t = create(
        /* states */        2, "q0", "qf",
        /* final_states */  1, "qf",
        /* symbols */       2, 'B', '1',
        /* blank */         'B',
        /* initial_state */ "q0",
        /* initial_tape */  3, '1', '1', '1',
        /* transitions */   2,
                            "q0", '1', '1', RIGHT, "q0",
                            "q0", 'B', '1', STAY, "qf"
    );
    run(t);
    printf("\nThree-state busy beaver\n");
    t = create(
        /* states */        4, "a", "b", "c", "halt",
        /* final_states */  1, "halt",
        /* symbols */       2, '0', '1',
        /* blank */         '0',
        /* initial_state */ "a",
        /* initial_tape */  0,
        /* transitions */   6,
                            "a", '0', '1', RIGHT, "b",
                            "a", '1', '1', LEFT, "c",
                            "b", '0', '1', LEFT, "a",
                            "b", '1', '1', RIGHT, "b",
                            "c", '0', '1', LEFT, "b",
                            "c", '1', '1', STAY, "halt"
    );
    run(t);
    return 0;
    printf("\nFive-state two-symbol probable busy beaver\n");
    t = create(
        /* states */        6, "A", "B", "C", "D", "E", "H",
        /* final_states */  1, "H",
        /* symbols */       2, '0', '1',
        /* blank */         '0',
        /* initial_state */ "A",
        /* initial_tape */  0,
        /* transitions */   10,
                            "A", '0', '1', RIGHT, "B",
                            "A", '1', '1', LEFT, "C",
                            "B", '0', '1', RIGHT, "C",
                            "B", '1', '1', RIGHT, "B",
                            "C", '0', '1', RIGHT, "D",
                            "C", '1', '0', LEFT, "E",
                            "D", '0', '1', LEFT, "A",
                            "D", '1', '1', LEFT, "D",
                            "E", '0', '1', STAY, "H",
                            "E", '1', '0', LEFT, "A"
    );
    run(t);
}

```


{{output}}


```txt
Simple incrementer
q0         [1] 1  1
q0          1 [1] 1
q0          1  1 [1]
q0          1  1  1 [B]
qf          1  1  1 [1]

Three-state busy beaver
a          [0]
b           1 [0]
a          [1] 1
c          [0] 1  1
b          [0] 1  1  1
a          [0] 1  1  1  1
b           1 [1] 1  1  1
b           1  1 [1] 1  1
b           1  1  1 [1] 1
b           1  1  1  1 [1]
b           1  1  1  1  1 [0]
a           1  1  1  1 [1] 1
c           1  1  1 [1] 1  1
halt        1  1  1 [1] 1  1
```



## C++


```cpp

#include <vector>
#include <string>
#include <iostream>
#include <algorithm>
#include <fstream>
#include <iomanip>
//--------------------------------------------------------------------------------------------------
typedef unsigned int uint;
using namespace std;
const uint TAPE_MAX_LEN = 49152;
//--------------------------------------------------------------------------------------------------
struct action { char write, direction; };
//--------------------------------------------------------------------------------------------------
class tape
{
public:
    tape( uint startPos = TAPE_MAX_LEN >> 1 ) : MAX_LEN( TAPE_MAX_LEN ) { _sp = startPos; reset(); }
    void reset() { clear( '0' ); headPos = _sp; }
    char read(){ return _t[headPos]; }
    void input( string a ){ if( a == "" ) return; for( uint s = 0; s < a.length(); s++ ) _t[headPos + s] = a[s]; }
    void clear( char c ) {  _t.clear(); blk = c; _t.resize( MAX_LEN, blk ); }
    void action( const action* a ) { write( a->write ); move( a->direction ); }
    void print( int c = 10 )
    {
	int ml = static_cast<int>( MAX_LEN ), st = static_cast<int>( headPos ) - c, ed = static_cast<int>( headPos ) + c + 1, tx;
	for( int x = st; x < ed; x++ )
	{ tx = x; if( tx < 0 ) tx += ml; if( tx >= ml ) tx -= ml; cout << _t[tx]; }
	cout << endl << setw( c + 1 ) << "^" << endl;
    }
private:
    void move( char d ) { if( d == 'N' ) return; headPos += d == 'R' ? 1 : -1; if( headPos >= MAX_LEN ) headPos = d == 'R' ? 0 : MAX_LEN - 1; }
    void write( char a ) { if( a != 'N' ) { if( a == 'B' ) _t[headPos] = blk; else _t[headPos] = a; } }
    string _t; uint headPos, _sp; char blk; const uint MAX_LEN;
};
//--------------------------------------------------------------------------------------------------
class state
{
public:
    bool operator ==( const string o ) { return o == name; }
    string name, next; char symbol, write, direction;
};
//--------------------------------------------------------------------------------------------------
class actionTable
{
public:
    bool loadTable( string file )
    {
	reset();
	ifstream mf; mf.open( file.c_str() ); if( mf.is_open() )
	{
	    string str; state stt;
	    while( mf.good() )
	    {
		getline( mf, str ); if( str[0] == '\'' ) break;
		parseState( str, stt ); states.push_back( stt );
	    }
	    while( mf.good() )
	    {
		getline( mf, str ); if( str == "" ) continue;
		if( str[0] == '!' ) blank = str.erase( 0, 1 )[0];
		if( str[0] == '^' ) curState = str.erase( 0, 1 );
		if( str[0] == '>' ) input = str.erase( 0, 1 );
	    }
	    mf.close(); return true;
	}
	cout << "Could not open " << file << endl; return false;
    }

    bool action( char symbol, action& a )
    {
	vector<state>::iterator f = states.begin();
	while( true )
	{
	    f = find( f, states.end(), curState );
	    if( f == states.end() ) return false;
	    if( ( *f ).symbol == '*' || ( *f ).symbol == symbol || ( ( *f ).symbol == 'B' && blank == symbol ) )
	    { a.direction = ( *f ).direction; a.write = ( *f ).write; curState = ( *f ).next; break; }
	    f++;
	}
	return true;
    }
    void reset() { states.clear(); blank = '0'; curState = input = ""; }
    string getInput() { return input; }
    char getBlank() { return blank; }
private:
    void parseState( string str, state& stt )
    {
	string a[5]; int idx = 0;
	for( string::iterator si = str.begin(); si != str.end(); si++ )
	{ if( ( *si ) == ';' ) idx++; else a[idx].append( &( *si ), 1 ); }
	stt.name = a[0]; stt.symbol = a[1][0]; stt.write = a[2][0]; stt.direction = a[3][0]; stt.next = a[4];
    }
    vector<state> states; char blank; string curState, input;
};
//--------------------------------------------------------------------------------------------------
class utm
{
public:
    utm() { files[0] = "incrementer.utm"; files[1] = "busy_beaver.utm"; files[2] = "sort.utm"; }
    void start()
    {
	while( true )
	{
	    reset(); int t = showMenu(); if( t == 0 ) return;
	    if( !at.loadTable( files[t - 1] ) ) return; startMachine();
	}
    }
private:
    void simulate()
    {
	char r; action a;
	while( true ) { tp.print(); r = tp.read(); if( !( at.action( r, a ) ) ) break; tp.action( &a ); }
	cout << endl << endl; system( "pause" );
    }

    int showMenu()
    {
	int t = -1;
	while( t < 0 || t > 3 )
	{
	    system( "cls" ); cout << "1. Incrementer\n2. Busy beaver\n3. Sort\n\n0. Quit";
	    cout << endl << endl << "Choose an action "; cin >> t;
	}
	return t;
    }

    void reset() { tp.reset(); at.reset(); }
    void startMachine() { system( "cls" ); tp.clear( at.getBlank() ); tp.input( at.getInput() ); simulate(); }

    tape tp; actionTable at; string files[7];
};
//--------------------------------------------------------------------------------------------------
int main( int a, char* args[] ){ utm mm; mm.start(); return 0; }
//--------------------------------------------------------------------------------------------------

```


'''These are the files you'll need'''<br />
File explanation:<br />
Each line contains one tuple of the form '<current state> <current symbol> <new symbol> <direction> <new state><br />
B = blank, H = halt, N = do nothing, * matches any current symbol<br />
' = marks the end of the action table<br />
! = blank symbol => eg: !0 => 0 is the blank symbol<br />
^ starting state<br />
> input

'''Incrementer'''
```txt
q0;1;1;R;q0
q0;B;1;H;qf
'
!0
^q0
>111


```

'''Busy beaver'''
```txt
A;0;1;R;B
A;1;1;L;C
B;0;1;L;A
B;1;1;R;B
C;0;1;L;B
C;1;1;N;H
'
!0
^A


```

'''Sort'''
```txt
A;1;1;R;A
A;2;3;R;B
A;0;0;L;E
B;1;1;R;B
B;2;2;R;B
B;0;0;L;C
C;1;2;L;D
C;2;2;L;C
C;3;2;L;E
D;1;1;L;D
D;2;2;L;D
D;3;1;R;A
E;1;1;L;E
E;0;0;R;H
'
!0
^A
>1221221211


```


{{out}}Busy beaver

```txt
000000000000000000000
          ^
000000000100000000000
          ^
000000000011000000000
          ^
000000000001100000000
          ^
000000000001110000000
          ^
000000000001111000000
          ^
000000000111110000000
          ^
000000001111100000000
          ^
000000011111000000000
          ^
000000111110000000000
          ^
000001111100000000000
          ^
000000111111000000000
          ^
000000011111100000000
          ^
000000011111100000000
          ^
```



## C#


```c#
using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Text;
using System.Threading;
using System.Threading.Tasks;

public class TuringMachine
{
    public static async Task Main() {
        var fiveStateBusyBeaver = new TuringMachine("A", '0', "H").WithTransitions(
            ("A", '0', '1', Right, "B"),
            ("A", '1', '1', Left,  "C"),
            ("B", '0', '1', Right, "C"),
            ("B", '1', '1', Right, "B"),
            ("C", '0', '1', Right, "D"),
            ("C", '1', '0', Left,  "E"),
            ("D", '0', '1', Left,  "A"),
            ("D", '1', '1', Left,  "D"),
            ("E", '0', '1', Stay,  "H"),
            ("E", '1', '0', Left,  "A")
        );
        var busyBeaverTask = fiveStateBusyBeaver.TimeAsync();

        var incrementer = new TuringMachine("q0", 'B', "qf").WithTransitions(
            ("q0", '1', '1', Right, "q0"),
            ("q0", 'B', '1', Stay,  "qf")
        )
        .WithInput("111");
        foreach (var _ in incrementer.Run()) PrintLine(incrementer);
        PrintResults(incrementer);

        var threeStateBusyBeaver = new TuringMachine("a", '0', "halt").WithTransitions(
            ("a", '0', '1', Right, "b"),
            ("a", '1', '1', Left,  "c"),
            ("b", '0', '1', Left,  "a"),
            ("b", '1', '1', Right, "b"),
            ("c", '0', '1', Left,  "b"),
            ("c", '1', '1', Stay,  "halt")
        );
        foreach (var _ in threeStateBusyBeaver.Run()) PrintLine(threeStateBusyBeaver);
        PrintResults(threeStateBusyBeaver);

        var sorter = new TuringMachine("A", '*', "X").WithTransitions(
            ("A", 'a', 'a', Right, "A"),
            ("A", 'b', 'B', Right, "B"),
            ("A", '*', '*', Left,  "E"),
            ("B", 'a', 'a', Right, "B"),
            ("B", 'b', 'b', Right, "B"),
            ("B", '*', '*', Left,  "C"),
            ("C", 'a', 'b', Left,  "D"),
            ("C", 'b', 'b', Left,  "C"),
            ("C", 'B', 'b', Left,  "E"),
            ("D", 'a', 'a', Left,  "D"),
            ("D", 'b', 'b', Left,  "D"),
            ("D", 'B', 'a', Right, "A"),
            ("E", 'a', 'a', Left,  "E"),
            ("E", '*', '*', Right, "X")
        )
        .WithInput("babbababaa");
        sorter.Run().Last();
        Console.WriteLine("Sorted: " + sorter.TapeString);
        PrintResults(sorter);

        sorter.Reset().WithInput("bbbababaaabba");
        sorter.Run().Last();
        Console.WriteLine("Sorted: " + sorter.TapeString);
        PrintResults(sorter);

        Console.WriteLine(await busyBeaverTask);
        PrintResults(fiveStateBusyBeaver);

        void PrintLine(TuringMachine tm) => Console.WriteLine(tm.TapeString + "\tState " + tm.State);

        void PrintResults(TuringMachine tm) {
            Console.WriteLine($"End state: {tm.State} = {(tm.Success ? "Success" : "Failure")}");
            Console.WriteLine(tm.Steps + " steps");
            Console.WriteLine("tape length: " + tm.TapeLength);
            Console.WriteLine();
        }
    }

    public const int Left = -1, Stay = 0, Right = 1;
    private readonly Tape tape;
    private readonly string initialState;
    private readonly HashSet<string> terminatingStates;
    private Dictionary<(string state, char read), (char write, int move, string toState)> transitions;

    public TuringMachine(string initialState, char blankSymbol, params string[] terminatingStates) {
        State = this.initialState = initialState;
        tape = new Tape(blankSymbol);
        this.terminatingStates = terminatingStates.ToHashSet();
    }

    public TuringMachine WithTransitions(
        params (string state, char read, char write, int move, string toState)[] transitions)
    {
        this.transitions = transitions.ToDictionary(k => (k.state, k.read), k => (k.write, k.move, k.toState));
        return this;
    }

    public TuringMachine Reset() {
        State = initialState;
        Steps = 0;
        tape.Reset();
        return this;
    }

    public TuringMachine WithInput(string input) {
        tape.Input(input);
        return this;
    }

    public int Steps { get; private set; }
    public string State { get; private set; }
    public bool Success => terminatingStates.Contains(State);
    public int TapeLength => tape.Length;
    public string TapeString => tape.ToString();

    public IEnumerable<string> Run() {
        yield return State;
        while (Step()) yield return State;
    }

    public async Task<TimeSpan> TimeAsync(CancellationToken cancel = default) {
        var chrono = Stopwatch.StartNew();
        await RunAsync(cancel);
        chrono.Stop();
        return chrono.Elapsed;
    }

    public Task RunAsync(CancellationToken cancel = default)
        => Task.Run(() => {
            while (Step()) cancel.ThrowIfCancellationRequested();
        });

    private bool Step() {
        if (!transitions.TryGetValue((State, tape.Current), out var action)) return false;
        tape.Current = action.write;
        tape.Move(action.move);
        State = action.toState;
        Steps++;
        return true;
    }


    private class Tape
    {
        private List<char> forwardTape = new List<char>(), backwardTape = new List<char>();
        private int head = 0;
        private char blank;

        public Tape(char blankSymbol) => forwardTape.Add(blank = blankSymbol);

        public void Reset() {
            backwardTape.Clear();
            forwardTape.Clear();
            head = 0;
            forwardTape.Add(blank);
        }

        public void Input(string input) {
            Reset();
            forwardTape.Clear();
            forwardTape.AddRange(input);
        }

        public void Move(int direction) {
            head += direction;
            if (head >= 0 && forwardTape.Count <= head) forwardTape.Add(blank);
            if (head < 0 && backwardTape.Count <= ~head) backwardTape.Add(blank);
        }

        public char Current {
            get => head < 0 ? backwardTape[~head] : forwardTape[head];
            set {
                if (head < 0) backwardTape[~head] = value;
                else forwardTape[head] = value;
            }
        }

        public int Length => backwardTape.Count + forwardTape.Count;

        public override string ToString() {
            int h = (head < 0 ? ~head : backwardTape.Count + head) * 2 + 1;
            var builder = new StringBuilder(" ", Length * 2 + 1);
            if (backwardTape.Count > 0) {
                builder.Append(string.Join(" ", backwardTape)).Append(" ");
                if (head < 0) (builder[h + 1], builder[h - 1]) = ('(', ')');
                for (int l = 0, r = builder.Length - 1; l < r; l++, r--) (builder[l], builder[r]) = (builder[r], builder[l]);
            }
            builder.Append(string.Join(" ", forwardTape)).Append(" ");
            if (head >= 0) (builder[h - 1], builder[h + 1]) = ('(', ')');
            return builder.ToString();
        }

    }

}
```

{{out}}
<pre style="height:30ex;overflow:scroll">
(1)1 1 	State q0
 1(1)1 	State q0
 1 1(1)	State q0
 1 1 1(B)	State q0
 1 1 1(1)	State qf
End state: qf = Success
4 steps
tape length: 4

(0)	State a
 1(0)	State b
(1)1 	State a
(0)1 1 	State c
(0)1 1 1 	State b
(0)1 1 1 1 	State a
 1(1)1 1 1 	State b
 1 1(1)1 1 	State b
 1 1 1(1)1 	State b
 1 1 1 1(1)	State b
 1 1 1 1 1(0)	State b
 1 1 1 1(1)1 	State a
 1 1 1(1)1 1 	State c
 1 1 1(1)1 1 	State halt
End state: halt = Success
13 steps
tape length: 6

Sorted:  *(a)a a a a b b b b b *
End state: X = Success
72 steps
tape length: 12

Sorted:  *(a)a a a a a b b b b b b b *
End state: X = Success
118 steps
tape length: 15

00:00:07.0626971
End state: H = Success
47176870 steps
tape length: 12289
```



## Clojure


```clojure

(defn tape
  "Creates a new tape with given blank character and tape contents"
  ([blank] (tape () blank () blank))
  ([right blank] (tape () (first right) (rest right) blank))
  ([left head right blank] [(reverse left) (or head blank) (into () right) blank]))

; Tape operations
(defn- left  [[[l & ls] _ rs       b] c] [ls          (or l b) (conj rs c) b])
(defn- right [[ls       _ [r & rs] b] c] [(conj ls c) (or r b) rs          b])
(defn- stay  [[ls       _ rs       b] c] [ls          c        rs          b])
(defn- head [[_ c _ b]] (or c b))
(defn- pretty [[ls c rs b]] (concat (reverse ls) [[(or c b)]] rs))

(defn new-machine
 "Returns a function that takes a tape as input, and returns the tape
  after running the machine specified in `machine`."
  [machine]
  (let [rules (into {} (for [[s c c' a s'] (:rules machine)]
                         [[s c] [c' (-> a name symbol resolve) s']]))
        finished? (into #{} (:terminating machine))]
    (fn [input-tape]
      (loop [state (:initial machine) tape input-tape]
        (if (finished? state)
          (pretty tape)
          (let [[out action new-state] (get rules [state (head tape)])]
            (recur new-state (action tape out))))))))

```



###  Tests



```clojure

(def simple-incrementer
  (new-machine {:initial :q0
                :terminating [:qf]
                :rules [[:q0 1   1 :right :q0]
                        [:q0 \B  1 :stay  :qf]]}))
(deftest simple-incrementer-test
  (is (= [1 1 1 [1]] (simple-incrementer (tape [1 1 1] \B)))))


(def three-state-two-symbol-busy-beaver
  (new-machine {:initial :a
                :terminating [:halt]
                :rules [[:a 0  1 :right :b]
                        [:a 1  1 :left  :c]
                        [:b 0  1 :left  :a]
                        [:b 1  1 :right :b]
                        [:c 0  1 :left  :b]
                        [:c 1  1 :stay  :halt]]}))
(deftest three-state-two-symbol-busy-beaver-test
  (is (= [1 1 1 [1] 1 1] (three-state-two-symbol-busy-beaver (tape 0)))))


(def five-state-two-symbol-busy-beaver
  (new-machine {:initial :A
                :terminating [:H]
                :rules [[:A 0  1 :right :B]
                        [:A 1  1 :left  :C]
                        [:B 0  1 :right :C]
                        [:B 1  1 :right :B]
                        [:C 0  1 :right :D]
                        [:C 1  0 :left  :E]
                        [:D 0  1 :left  :A]
                        [:D 1  1 :left  :D]
                        [:E 0  1 :stay  :H]
                        [:E 1  0 :left  :A]]}))
(deftest five-state-two-symbol-busy-beaver-test
  (let [result (flatten (five-state-two-symbol-busy-beaver (tape 0)))
        freq (frequencies result)]
    (is (= 4098 (get freq 1)))
    (is (= 8191 (get freq 0)))))

```


## Common Lisp


### Iterative version

The infinite tape is represented by two lists:
# <code>front</code> contains all cells before the current cell in reverse order (i.e. the first element in <code>front</code> is the direct predecessor of the current cell)
# <code>back</code> contains the current cell as its first element, followed by all successors.

```lisp
(defun turing (initial terminal blank rules tape &optional (verbose NIL))
  (labels ((combine (front back)
             (if front
               (combine (cdr front) (cons (car front) back))
               back))

           (update-tape (old-front old-back new-content move)
             (cond ((eq move 'right)
                    (list (cons new-content old-front)
                          (cdr old-back)))
                   ((eq move 'left)
                    (list (cdr old-front)
                          (list* (car old-front) new-content (cdr old-back))))
                   (T (list old-front
                            (cons new-content (cdr old-back))))))

           (show-tape (front back)
             (format T "~{~a~}[~a]~{~a~}~%"
                     (nreverse (subseq front 0 (min 10 (length front))))
                     (or (car back) blank)
                     (subseq (cdr back) 0 (min 10 (length (cdr back)))))))

    (loop for back = tape then new-back
          for front = '() then new-front
          for state = initial then new-state
          for content = (or (car back) blank)
          for (new-state new-content move) = (gethash (cons state content) rules)
          for (new-front new-back) = (update-tape front back new-content move)
          until (equal state terminal)
          do (when verbose
               (show-tape front back))
          finally (progn
                    (when verbose
                      (show-tape front back))
                    (return (combine front back))))))
```



### Recursive version

Using the same interface and general idea as the iterative version.

```lisp
(defun turing (initial terminal blank rules tape &optional (verbose NIL))
  (labels ((run (state front back)
             (if (equal state terminal)
               (progn
                 (when verbose
                   (show-tape front back))
                 (combine front back))
               (let ((current-content (or (car back) blank)))
                 (destructuring-bind
                   (new-state new-content move)
                   (gethash (cons state current-content) rules)
                   (when verbose
                     (show-tape front back))
                   (cond ((eq move 'right)
                          (run new-state
                               (cons new-content front)
                               (cdr back)))
                         ((eq move 'left)
                          (run new-state
                               (cdr front)
                               (list* (car front) new-content (cdr back))))
                         (T (run new-state
                                 front
                                 (cons new-content (cdr back)))))))))

            (show-tape (front back)
              (format T "~{~a~}[~a]~{~a~}~%"
                      (nreverse (subseq front 0 (min 10 (length front))))
                      (or (car back) blank)
                      (subseq (cdr back) 0 (min 10 (length (cdr back))))))

            (combine (front back)
             (if front
               (combine (cdr front) (cons (car front) back))
               back)))

    (run initial '() tape)))
```



### Usage


```lisp
;; Helper function for creating the rules table
(defun make-rules-table (rules-list)
  (let ((rules (make-hash-table :test 'equal)))
    (loop for (state content new-content dir new-state) in rules-list
          do (setf (gethash (cons state content) rules)
                   (list new-state new-content dir)))
    rules))

(format T "Simple incrementer~%")
(turing 'q0 'qf 'B (make-rules-table '((q0 1 1 right q0) (q0 B 1 stay qf))) '(1 1 1) T)

(format T "Three-state busy beaver~%")
(turing 'a 'halt 0
        (make-rules-table '((a 0 1 right b)
                            (a 1 1 left c)
                            (b 0 1 left a)
                            (b 1 1 right b)
                            (c 0 1 left b)
                            (c 1 1 stay halt)))
        '() T)

(format T "Sort (final tape)~%")
(format T "~{~a~}~%"
        (turing 'A 'H 0
                (make-rules-table '((A 1 1 right A)
                                    (A 2 3 right B)
                                    (A 0 0 left E)
                                    (B 1 1 right B)
                                    (B 2 2 right B)
                                    (B 0 0 left C)
                                    (C 1 2 left D)
                                    (C 2 2 left C)
                                    (C 3 2 left E)
                                    (D 1 1 left D)
                                    (D 2 2 left D)
                                    (D 3 1 right A)
                                    (E 1 1 left E)
                                    (E 0 0 right H)))
                '(2 1 2 2 2 1 1)))

(format T "5-state busy beaver (first 20 cells)~%")
(format T "~{~a~}...~%"
  (subseq (turing 'A 'H 0
                  (make-rules-table '((A 0 1 right B)
                                      (A 1 1 left  C)
                                      (B 0 1 right C)
                                      (B 1 1 right B)
                                      (C 0 1 right D)
                                      (C 1 0 left E)
                                      (D 0 1 left A)
                                      (D 1 1 left D)
                                      (E 0 1 stay H)
                                      (E 1 0 left A)))
                  '())
          0 20))
```


{{Out}}

```txt
Simple incrementer
[1]11
1[1]1
11[1]
111[B]
111[1]
Three-state busy beaver
[0]
1[0]
[1]1
[0]11
[0]111
[0]1111
1[1]111
11[1]11
111[1]1
1111[1]
11111[0]
1111[1]1
111[1]11
111[1]11
Sort (final tape)
011122220
5-state busy beaver (first 20 cells)
10100100100100100100...
```



## D


### Nearly Strongly Typed Version

This is typed a little less strongly than the Ada entry. It's fast and safe.

```d
import std.stdio, std.algorithm, std.string, std.conv, std.array,
       std.exception, std.traits, std.math, std.range;

struct UTM(State, Symbol, bool doShow=true)
if (is(State == enum) && is(Symbol == enum)) {
    static assert(is(typeof({ size_t x = State.init; })),
                  "State must to be usable as array index.");
    static assert([EnumMembers!State].equal(EnumMembers!State.length.iota),
                  "State must be a plain enum.");
    static assert(is(typeof({ size_t x = Symbol.init; })),
                  "Symbol must to be usable as array index.");
    static assert([EnumMembers!Symbol].equal(EnumMembers!Symbol.length.iota),
                  "Symbol must be a plain enum.");

    enum Direction { right, left, stay }

    private const TuringMachine tm;
    private TapeHead head;
    alias SymbolMap = string[EnumMembers!Symbol.length];

    // The first index of this 'rules' matrix is a subtype of State
    // because it can't contain H, but currently D can't enforce this,
    // statically unlike Ada language.
    Rule[EnumMembers!Symbol.length][EnumMembers!State.length - 1] mRules;

    static struct Rule {
        Symbol toWrite;
        Direction direction;
        State nextState;

        this(in Symbol toWrite_, in Direction direction_, in State nextState_)
        pure nothrow @safe @nogc {
            this.toWrite = toWrite_;
            this.direction = direction_;
            this.nextState = nextState_;
        }
    }

    // This is kept separated from the rest so it can be inialized
    // one field at a time in the main function, yet it will become
    // const.
    static struct TuringMachine {
        Symbol blank;
        State initialState;
        Rule[Symbol][State] rules;
        Symbol[] input;
        SymbolMap symbolMap;
    }

    static struct TapeHead {
        immutable Symbol blank;
        Symbol[] tapeLeft, tapeRight;
        int position;
        const SymbolMap sMap;
        size_t nSteps;

        this(in ref TuringMachine t) pure nothrow @safe {
            this.blank = EnumMembers!Symbol[0];
            //tapeRight = t.input.empty ? [this.blank] : t.input.dup;
            if (t.input.empty)
                this.tapeRight = [this.blank];
            else
                this.tapeRight = t.input.dup;
            this.position = 0;
            this.sMap = t.symbolMap;
        }

        pure nothrow @safe @nogc invariant {
            assert(this.tapeRight.length > 0);
            if (this.position >= 0)
                assert(this.position < this.tapeRight.length);
            else
                assert(this.position.abs <= this.tapeLeft.length);
        }

        Symbol readSymb() const pure nothrow @safe @nogc {
            if (this.position >= 0)
                return this.tapeRight[this.position];
            else
                return this.tapeLeft[this.position.abs - 1];
        }

        void showSymb() const @safe {
            this.write;
        }

        void writeSymb(in Symbol symbol) @safe {
            static if (doShow)
                showSymb;
            if (this.position >= 0)
                this.tapeRight[this.position] = symbol;
            else
                this.tapeLeft[this.position.abs - 1] = symbol;
        }

        void goRight() pure nothrow @safe {
            this.position++;
            if (position > 0 && position == tapeRight.length)
                tapeRight ~= blank;
        }

        void goLeft() pure nothrow @safe {
            this.position--;
            if (position < 0 && (position.abs - 1) == tapeLeft.length)
                tapeLeft ~= blank;
        }

        void move(in Direction dir) pure nothrow @safe {
            nSteps++;
            final switch (dir) with (Direction) {
                case left:  goLeft;        break;
                case right: goRight;       break;
                case stay:  /*Do nothing*/ break;
            }
        }

        string toString() const @safe {
            immutable pos = tapeLeft.length.signed + this.position + 4;
            return format("...%-(%)...", tapeLeft.retro.chain(tapeRight)
                                         .map!(s => sMap[s])) ~
                   '\n' ~
                   format("%" ~ pos.text ~ "s", "^") ~
                   '\n';
        }
    }

    void show() const @safe {
        head.showSymb;
    }

    this(in ref TuringMachine tm_) @safe {
        static assert(__traits(compiles, State.H), "State needs a 'H' (Halt).");
        immutable errMsg = "Invalid input.";
        auto runningStates = remove!(s => s == State.H)([EnumMembers!State]);
        enforce(!runningStates.empty, errMsg);
        enforce(tm_.rules.length == EnumMembers!State.length - 1, errMsg);
        enforce(State.H !in tm_.rules, errMsg);
        enforce(runningStates.canFind(tm_.initialState), errMsg);

        // Create a matrix to reduce running time.
        foreach (immutable State st, const rset; tm_.rules)
            foreach (immutable Symbol sy, immutable rule; rset)
                mRules[st][sy] = rule;

        this.tm = tm_;
        head = TapeHead(this.tm);

        State state = tm.initialState;
        while (state != State.H) {
            immutable next = mRules[state][head.readSymb];
            head.writeSymb(next.toWrite);
            head.move(next.direction);
            state = next.nextState;
        }
        static if (doShow)
            show;
        writeln("Performed ", head.nSteps, " steps.");
    }
}

void main() @safe {
    "Incrementer:".writeln;
    enum States1 : ubyte { A, H }
    enum Symbols1 : ubyte { s0, s1 }
    alias M1 = UTM!(States1, Symbols1);
    M1.TuringMachine tm1;
    with (tm1) with (States1) with (Symbols1) with (M1.Direction) {
        alias R = M1.Rule;
        initialState = A;
        rules = [A: [s0: R(s1, stay,  H), s1: R(s1, right, A)]];
        input = [s1, s1, s1];
        symbolMap = ["0", "1"];
    }
    M1(tm1);

    // http://en.wikipedia.org/wiki/Busy_beaver
    "\nBusy Beaver machine (3-state, 2-symbol):".writeln;
    enum States2 : ubyte { A, B, C, H }
    alias Symbols2 = Symbols1;
    alias M2 = UTM!(States2, Symbols2);
    M2.TuringMachine tm2;
    with (tm2) with (States2) with (Symbols2) with (M2.Direction) {
        alias R = M2.Rule;
        initialState = A;
        rules = [A: [s0: R(s1, right, B), s1: R(s1, left,  C)],
                 B: [s0: R(s1, left,  A), s1: R(s1, right, B)],
                 C: [s0: R(s1, left,  B), s1: R(s1, stay,  H)]];
        symbolMap = ["0", "1"];
    }
    M2(tm2);

    "\nSorting stress test (12212212121212):".writeln;
    enum States3 : ubyte { A, B, C, D, E, H }
    enum Symbols3 : ubyte { s0, s1, s2, s3 }
    alias M3 = UTM!(States3, Symbols3, false);
    M3.TuringMachine tm3;
    with (tm3) with (States3) with (Symbols3) with (M3.Direction) {
        alias R = M3.Rule;
        initialState = A;
        rules = [A: [s1: R(s1, right, A),
                     s2: R(s3, right, B),
                     s0: R(s0, left,  E)],
                 B: [s1: R(s1, right, B),
                     s2: R(s2, right, B),
                     s0: R(s0, left,  C)],
                 C: [s1: R(s2, left,  D),
                     s2: R(s2, left,  C),
                     s3: R(s2, left,  E)],
                 D: [s1: R(s1, left,  D),
                     s2: R(s2, left,  D),
                     s3: R(s1, right, A)],
                 E: [s1: R(s1, left,  E),
                     s0: R(s0, stay,  H)]];
        input = [s1, s2, s2, s1, s2, s2, s1,
                 s2, s1, s2, s1, s2, s1, s2];
        symbolMap = ["0", "1", "2", "3"];
    }
    M3(tm3).show;

    "\nPossible best Busy Beaver machine (5-state, 2-symbol):".writeln;
    alias States4 = States3;
    alias Symbols4 = Symbols1;
    alias M4 = UTM!(States4, Symbols4, false);
    M4.TuringMachine tm4;
    with (tm4) with (States4) with (Symbols4) with (M4.Direction) {
        alias R = M4.Rule;
        initialState = A;
        rules = [A: [s0: R(s1, right, B), s1: R(s1, left,  C)],
                 B: [s0: R(s1, right, C), s1: R(s1, right, B)],
                 C: [s0: R(s1, right, D), s1: R(s0, left,  E)],
                 D: [s0: R(s1, left,  A), s1: R(s1, left,  D)],
                 E: [s0: R(s1, stay,  H), s1: R(s0, left,  A)]];
        symbolMap = ["0", "1"];
    }
    M4(tm4);
}
```

{{out}}

```txt
Incrementer:
...111...
   ^
...111...
    ^
...111...
     ^
...1110...
      ^
...1111...
      ^
Performed 4 steps.

Busy Beaver machine (3-state, 2-symbol):
...0...
   ^
...10...
    ^
...11...
   ^
...011...
   ^
...0111...
   ^
...01111...
   ^
...11111...
    ^
...11111...
     ^
...11111...
      ^
...11111...
       ^
...111110...
        ^
...111111...
       ^
...111111...
      ^
...111111...
      ^
Performed 13 steps.

Sorting stress test (12212212121212):
Performed 118 steps.
...0111111222222220...
   ^

Possible best Busy Beaver machine (5-state, 2-symbol):
Performed 47176870 steps.
```

The total run-time is about 0.31 seconds.


### Simple Version

While the precedent version is Ada-like, this is more like a script.

```d
import std.stdio, std.typecons, std.algorithm, std.string, std.array;

void turing(Sy, St)(in St state, Sy[int] tape, in int pos,
                    in Tuple!(Sy, int, St)[Sy][St] rules) {
    if (state.empty) return;
    const r = rules[state][tape[pos] = tape.get(pos, Sy.init)];
    writefln("%-(%s%)", tape.keys.sort()
        .map!(i => format(i == pos ? "(%s)" : " %s ", tape[i])));
    tape[pos] = r[0];
    turing(r[2], tape, pos + r[1], rules);
}

void main() {
    turing("a", null, 0,
           ["a": [0: tuple(1,  1, "b"), 1: tuple(1, -1, "c")],
            "b": [0: tuple(1, -1, "a"), 1: tuple(1,  1, "b")],
            "c": [0: tuple(1, -1, "b"), 1: tuple(1,  0, "")]]);
}
```

{{out}}

```txt
(0)
 1 (0)
(1) 1
(0) 1  1
(0) 1  1  1
(0) 1  1  1  1
 1 (1) 1  1  1
 1  1 (1) 1  1
 1  1  1 (1) 1
 1  1  1  1 (1)
 1  1  1  1  1 (0)
 1  1  1  1 (1) 1
 1  1  1 (1) 1  1
```


=={{header|Déjà Vu}}==

```dejavu
transitions(:
	local :t {}
	while /= ) dup:
		set-to t swap & rot & rot rot &
	t drop

take-from tape:
	if tape:
		pop-from tape
	else:
		:B

paste-together a h b:
	push-to b h
	while a:
		push-to b pop-from a
	b

universal-turing-machine transitions initial final tape:
	local :tape-left []
	local :state initial

	local :head take-from tape

	local :move { :stay @pass }

	move!left:
		push-to tape head
		set :head take-from tape-left

	move!right:
		push-to tape-left head
		set :head take-from tape

	while /= state final:
		if opt-get transitions & state head:
			set :state &<>
			set :head &<>
			move!
		else:
			return paste-together tape-left head tape
	paste-together tape-left head tape
```



### Simple incrementer



```dejavu
:q0 :qf [ 1 1 1 ]
)
:q0 1 1 :right :q0
:q0 :B 1 :stay :qf
!. universal-turing-machine transitions(
```

{{out}}

```txt
[ 1 1 1 1 ]
```


===Three-state busy beaver===


```dejavu
:a :halt []
)
:a :B 1 :right :b
:a 1 1 :left :c
:b :B 1 :left :a
:b 1 1 :right :b
:c :B 1 :left :b
:c 1 1 :stay :halt
!. universal-turing-machine transitions(
```

{{out}}

```txt
[ 1 1 1 1 1 1 ]
```


===5-state, 2-symbol probable Busy Beaver machine===


```dejavu
:A :H []
)
:A :B 1 :right :B
:A 1 1 :left :C
:B :B 1 :right :C
:B 1 1 :right :B
:C :B 1 :right :D
:C 1 :B :left :E
:D :B 1 :left :A
:D 1 1 :left :D
:E :B 1 :stay :H
:E 1 :B :left :A
!. universal-turing-machine transitions(
```


(Output omitted because of length.)


## EchoLisp

We define a Turing machine as an instance of TM struct, which stores the definition values (states,symbols,rules) and the current state values (state, tape, position). It can be stopped, restarted, called as a sub-program, or transformed into a sequence or stream.'Huge' TM are run in the background. Rules are compiled into a vector indexed by state * symbol.

###  Turing Machines


```scheme

(require 'struct)

(struct TM (read-only: name states symbs final rules mem  state-values: tape pos state))

(define-syntax-rule (rule-idx state symb numstates)
    (+ state (* symb numstates)))

(define-syntax-rule (make-TM name states symbs rules)
    (_make-TM  name 'states 'symbs 'rules))

;; a rule is (state symbol --> write move new-state)
;; index for rule = state-num + (number of states)  * symbol-num
;; convert states/symbol into vector indices
(define (compile-rule T rule  into: rules)
    (define numstates (vector-length (TM-states T)))
    (define state (vector-index [rule 0](TM-states T) )) ; index
    (define symb (vector-index [rule 1](TM-symbs T) ))
    (define write-symb (vector-index [rule 2] (TM-symbs T)  ))
    (define move (1- (vector-index  [rule 3] #(left stay right) )))
    (define new-state (vector-index  [rule 4](TM-states T)))
    (define rulenum (rule-idx state symb numstates))
    (vector-set! rules rulenum (vector write-symb move new-state))
    ; (writeln 'rule  rulenum [rules rulenum])
    )

(define (_make-TM name states symbs rules)
    (define T (TM  name (list->vector states) (list->vector symbs) null null))
    (set-TM-final! T (1-  (length states))) ;; assume one final state
    (set-TM-rules! T (make-vector (* (length states) (length symbs))))
    (for ((rule rules)) (compile-rule T (list->vector rule) into: (TM-rules T)))
    T ) ; returns a TM

;;------------------
;; TM-trace
;;-------------------
(string-delimiter "")

(define (TM-print T symb-index: symb (hilite #f))
	(cond
	((= 0 symb) (if hilite "🔲"  "◽️" ))
	((= 1 symb) (if hilite  "🔳 " "◾️" ))
	(else "X")))

(define (TM-trace T tape pos state step)
	(if (= (TM-final T) state)
		(write "🔴")
		(write "🔵"))

    (for [(p (in-range  (- (TM-mem T) 7) (+ (TM-mem T) 8)))]
        (write (TM-print T [tape p] (= p pos))))
    (write step)
    (writeln))

;;---------------
;; TM-init : alloc and init tape
;;---------------
(define (TM-init T input-symbs (mem 20))
    ;; init state variables
    (set-TM-tape! T (make-vector (* 2 mem)))
    (set-TM-pos!  T  mem)
    (set-TM-state! T 0)
    (set-TM-mem! T mem)

    (for [(symb input-symbs) (i (in-naturals))]
        (vector-set! (TM-tape T) [+ i (TM-pos T)] (vector-index symb (TM-symbs T))))
    (TM-trace T  (TM-tape T) mem 0 0)
    mem )

;;---------------
;; TM-run : run at most maxsteps
;;---------------
(define (TM-run T  (verbose #f)  (maxsteps 1_000_000))
(define count 0)
    (define final (TM-final T))
    (define rules (TM-rules T))
    (define rule 0)
    (define numstates (vector-length (TM-states T)))
    ;; set current state vars
    (define pos (TM-pos T))
    (define state (TM-state T))
    (define tape (TM-tape T))

    (when (and (zero? state) (= pos (TM-mem T)))
    	 (writeln 'Starting (TM-name T))
    	 (TM-trace T tape pos 0 count))

    (while (and (!= state final) (< count maxsteps))
    (++ count)
;; The machine
        (set! rule [rules (rule-idx state [tape pos] numstates)])
        (when (= rule 0) (error "missing rule" (list state [tape pos])))
        (vector-set! tape pos [rule 0])
        (set! state [rule 2])
        (+= pos [rule 1])
;; end machine
        (when verbose (TM-trace  T tape pos state count )))
;; save TM state
    (set-TM-pos! T pos)
    (set-TM-state! T state)
    (when (= final state)  (writeln 'Stopping (TM-name T) 'at-pos (- pos (TM-mem T))))
    count)

```

{{out}}

```txt

(define T (make-TM "TM: incrementer"
    (q0 qf)
    (B 1)
    ((q0  1 1 right q0)
       (q0 B 1 stay qf))))

(TM-init T '(1 1 1) 20)
(TM-run T #t)

(TM-run T #t)
🔵 ◽️ ◽️ ◽️ ◽️ ◽️ ◽️ ◽️ 🔳 ◾️ ◾️ ◽️ ◽️ ◽️ ◽️ ◽️ 0
Starting     TM: incrementer
🔵 ◽️ ◽️ ◽️ ◽️ ◽️ ◽️ ◽️ 🔳 ◾️ ◾️ ◽️ ◽️ ◽️ ◽️ ◽️ 0
🔵 ◽️ ◽️ ◽️ ◽️ ◽️ ◽️ ◽️ ◾️ 🔳 ◾️ ◽️ ◽️ ◽️ ◽️ ◽️ 1
🔵 ◽️ ◽️ ◽️ ◽️ ◽️ ◽️ ◽️ ◾️ ◾️ 🔳 ◽️ ◽️ ◽️ ◽️ ◽️ 2
🔵 ◽️ ◽️ ◽️ ◽️ ◽️ ◽️ ◽️ ◾️ ◾️ ◾️ 🔲 ◽️ ◽️ ◽️ ◽️ 3
🔴 ◽️ ◽️ ◽️ ◽️ ◽️ ◽️ ◽️ ◾️ ◾️ ◾️ 🔳 ◽️ ◽️ ◽️ ◽️ 4
Stopping     TM: incrementer     at-pos     3

;; three-states busy beaver
(define T (make-TM "TM: three-states busy beaver"
    (a b c halt)
    (0 1)
     ((a 0 1 right b)
        (a 1 1 left c)
        (b 0 1 left a)
        (b 1 1 right b)
        (c 0 1 left b)
        (c 1 1 stay halt))))

(TM-init T null 100)
Starting     TM: three-states busy beaver
🔵 ◽️ ◽️ ◽️ ◽️ ◽️ ◽️ ◽️ 🔲 ◽️ ◽️ ◽️ ◽️ ◽️ ◽️ ◽️ 0
🔵 ◽️ ◽️ ◽️ ◽️ ◽️ ◽️ ◽️ ◾️ 🔲 ◽️ ◽️ ◽️ ◽️ ◽️ ◽️ 1
🔵 ◽️ ◽️ ◽️ ◽️ ◽️ ◽️ ◽️ 🔳 ◾️ ◽️ ◽️ ◽️ ◽️ ◽️ ◽️ 2
🔵 ◽️ ◽️ ◽️ ◽️ ◽️ ◽️ 🔲 ◾️ ◾️ ◽️ ◽️ ◽️ ◽️ ◽️ ◽️ 3
🔵 ◽️ ◽️ ◽️ ◽️ ◽️ 🔲 ◾️ ◾️ ◾️ ◽️ ◽️ ◽️ ◽️ ◽️ ◽️ 4
🔵 ◽️ ◽️ ◽️ ◽️ 🔲 ◾️ ◾️ ◾️ ◾️ ◽️ ◽️ ◽️ ◽️ ◽️ ◽️ 5
🔵 ◽️ ◽️ ◽️ ◽️ ◾️ 🔳 ◾️ ◾️ ◾️ ◽️ ◽️ ◽️ ◽️ ◽️ ◽️ 6
🔵 ◽️ ◽️ ◽️ ◽️ ◾️ ◾️ 🔳 ◾️ ◾️ ◽️ ◽️ ◽️ ◽️ ◽️ ◽️ 7
🔵 ◽️ ◽️ ◽️ ◽️ ◾️ ◾️ ◾️ 🔳 ◾️ ◽️ ◽️ ◽️ ◽️ ◽️ ◽️ 8
🔵 ◽️ ◽️ ◽️ ◽️ ◾️ ◾️ ◾️ ◾️ 🔳 ◽️ ◽️ ◽️ ◽️ ◽️ ◽️ 9
🔵 ◽️ ◽️ ◽️ ◽️ ◾️ ◾️ ◾️ ◾️ ◾️ 🔲 ◽️ ◽️ ◽️ ◽️ ◽️ 10
🔵 ◽️ ◽️ ◽️ ◽️ ◾️ ◾️ ◾️ ◾️ 🔳 ◾️ ◽️ ◽️ ◽️ ◽️ ◽️ 11
🔵 ◽️ ◽️ ◽️ ◽️ ◾️ ◾️ ◾️ 🔳 ◾️ ◾️ ◽️ ◽️ ◽️ ◽️ ◽️ 12
🔴 ◽️ ◽️ ◽️ ◽️ ◾️ ◾️ ◾️ 🔳 ◾️ ◾️ ◽️ ◽️ ◽️ ◽️ ◽️ 13
Stopping     TM: three-states busy beaver     at-pos     0

```


### Possible best busy beaver

We create a task to run it in the background.
{{out}}

```scheme

(define steps 0)
(define (TM-task T)
    (define count (TM-run T #f 1000000))
    (when (zero? steps) (writeln 'START (date)))
    (+= steps count)
    (writeln 'TM-steps steps (date))
    (when (zero? count) (writeln 'END steps (date)))
    (if (zero? count) #f T)) ;; return #f to signal end of task

```


```txt

;; 5-states 2-symbols busy beaver
;; Result: 4098 "1"s with 8191 "0"s interspersed in 47,176,870 steps.
(lib 'tasks)

(define T (make-TM "TM: 5-states 2-symbols busy beaver"
    (A B C D E H)
    (0 1)
    ((A 0 1 right B)
        (A 1 1 left C)
        (B 0 1 right C)
        (B 1 1 right B)
        (C 0 1 right D)
        (C 1 0 left E)
        (D 0 1 left A)
        (D 1 1 left D)
        (E 0 1 stay H)
        (E 1 0 left A))))

(TM-init T null 20000)
(task-run (make-task TM-task T) 1000)

;; Firefox : 192 sec Chrome:342 sec Safari: 232 sec
START     Mon Feb 08 2016 18:34:15 GMT+0100 (CET)
TM-steps     47176870     Mon Feb 08 2016 18:38:23 GMT+0100 (CET)
END     47176870     Mon Feb 08 2016 18:38:23 GMT+0100 (CET)

;; check result : count written "1"

(for/sum ((s (TM-tape T))) s)
    → 4098

```



## Erlang

The following code is an Escript which can be placed into a file and run as <code>escript filename</code> or simply marked as executable and run directly via the provided shebang header.  <code>-type</code> and <code>-spec</code> declarations have not been used; using the <code>typer</code> utility can get a head start on this process should a more robust solution be desired.

In this universal Turing machine simulator, a machine is defined by giving it a configuration function that returns the initial state, the halting states and the blank symbol, as well as a function for the rules.  These are passed in to the public interface <code>turing/3</code> as funs, together with the initial tape setup.


```erlang
#!/usr/bin/env escript

-module(turing).
-mode(compile).

-export([main/1]).

% Incrementer definition:
% States: a | halt
% Initial state: a
% Halting states: halt
% Symbols: b | '1'
% Blank symbol: b
incrementer_config() -> {a, [halt], b}.
incrementer(a, '1') -> {'1', right, a};
incrementer(a, b)   -> {'1', stay, halt}.

% Busy beaver definition:
% States: a | b | c | halt
% Initial state: a
% Halting states: halt
% Symbols: '0' | '1'
% Blank symbol: '0'
busy_beaver_config() -> {a, [halt], '0'}.
busy_beaver(a, '0') -> {'1', right, b};
busy_beaver(a, '1') -> {'1', left, c};
busy_beaver(b, '0') -> {'1', left, a};
busy_beaver(b, '1') -> {'1', right, b};
busy_beaver(c, '0') -> {'1', left, b};
busy_beaver(c, '1') -> {'1', stay, halt}.

% Mainline code.
main([]) ->
    io:format("
### ========================
~n"),
    io:format("Turing machine simulator test.~n"),
    io:format("
### ========================
~n"),

    Tape1 = turing(fun incrementer_config/0, fun incrementer/2, ['1','1','1']),
    io:format("~w~n", [Tape1]),

    Tape2 = turing(fun busy_beaver_config/0, fun busy_beaver/2, []),
    io:format("~w~n", [Tape2]).

% Universal Turing machine simulator.
turing(Config, Rules, Input) ->
    {Start, _, _} = Config(),
    {Left, Right} = perform(Config, Rules, Start, {[], Input}),
    lists:reverse(Left) ++ Right.

perform(Config, Rules, State, Input = {LeftInput, RightInput}) ->
    {_, Halts, Blank} = Config(),
    case lists:member(State, Halts) of
        true  -> Input;
        false ->
            {NewRight, Symbol} = symbol(RightInput, Blank),
            {NewSymbol, Action, NewState} = Rules(State, Symbol),
            NewInput = action(Action, Blank, {LeftInput, [NewSymbol| NewRight]}),
            perform(Config, Rules, NewState, NewInput)
    end.

symbol([], Blank) -> {[], Blank};
symbol([S|R], _)  -> {R, S}.

action(left, Blank, {[], Right}) -> {[], [Blank|Right]};
action(left, _, {[L|Ls], Right}) -> {Ls, [L|Right]};
action(stay, _, Tape)            -> Tape;
action(right, Blank, {Left, []}) -> {[Blank|Left], []};
action(right, _, {Left, [R|Rs]}) -> {[R|Left], Rs}.
```


=={{header|Fōrmulæ}}==

In [http://wiki.formulae.org/Universal_Turing_Machine this] page you can see the solution of this task.

Fōrmulæ programs are not textual, visualization/edition of programs is done showing/manipulating structures but not text ([http://wiki.formulae.org/Editing_F%C5%8Drmul%C3%A6_expressions more info]). Moreover, there can be multiple visual representations of the same program. Even though it is possible to have textual representation &mdash;i.e. XML, JSON&mdash; they are intended for transportation effects more than visualization and edition.

The option to show Fōrmulæ programs and their results is showing images. Unfortunately images cannot be uploaded in Rosetta Code.


## Fortran


### The plan

A Turing machine is the apotheosis of spaghetti code, because every action is followed by a GO TO another state. This is rather like SNOBOL, where every statement succeeds or fails, and is (optionally) followed by a GO TO for each condition. This in turn means that there is no special ordering of the states, aside from indicating which is to be the starting point. One can reorder them to taste. Accordingly, the plan is to number the states 1, 2, ''etc.'' with zero reserved to mean HALT and the starting state to be number one, no matter what names happen to be associated with a description of the states and their transitions. This has the advantage that the test for stopping is not <code>STATE = ENDSTATE</code> where STATE and ENDSTATE are some cute texts involving "HALT" or somesuch, but just <code>STATE = 0</code>, and not some arbitrary integer in a variable but a constant and a special constant at that. In short, the computer should be able to make this test without any subtraction, just inspecting the value of STATE as by loading into the accumulator whereby indicators are set. Alas, intel cpus employ <code>cmp  dword ptr [STATE (00476620)],0</code> so they do perform a subtraction anyway, and I wonder why I bother. But, ''principle is to be upheld!''

Similarly, various symbols are described as being on the tape but they can be deemed to be symbol number 0, symbol number 1, and so on, as numerical values and not character codes (be they ASCII or whatever), in whatever order is convenient.

With this in mind, after some cogitation it was clear that the transition tables could be specified in a basic way involving only numerical values, and that each state would have NSYMBOL entries. Thus, the table would be indexed as a two-dimensional array, (STATE,SYMBOL) where SYMBOL was the (number of) the symbol under the tape's read/write head. Since this project involves a variety of Turing machines that manipulate a varying collection of symbols, the transition table would have to be dimensioned according to the problem. But this is distasteful, because it means that the array indexing involves variables rather than constants, slowing the calculation. Similarly, one could define a compound data aggregate such as TRANSITION(state,symbol) with sub entries MARK, MOVE, and NEXT at the cost of more complex indexing again - plus requiring the facilities of F90 and later.

Instead the plan was to use one dimensional arrays, and to perform the indexing via <code>IT = STATE*NSYMBOL - S</code> for symbol number S after which the transition entries would be MARK(IT), MOVE(IT), and NEXT(IT). This scheme has ''no'' CASE-statements nor compound IF-statements to identify the individual states and the particular symbol under the read/write head so as to select the correct transition. Hopefully, the compiler will recognise the similar shape of each array and reduce its code accordingly. The subtraction of the symbol code rather than addition is due to the more normal <code>IT = (STATE - 1)*NSYMBOL + S</code> requiring an extra subtraction, which is to be avoided. Otherwise the transition table would have unused entries at the start (because state zero requires no transitions) and this waste causes an itch.

The input scheme initially required a numbers-only style of description (so free-format reading could be used), and translating to the reduced scheme from the different presentation styles of the example Turing machine descriptions was tricky. The initial format required that the transitions for each state be entered in reverse numerical order of the number of the symbol being recognised, with no mention of the symbol because it was deduced by the order of entry, just the MARK, MOVE, and NEXT values, as follows:

```txt

1          State 1
  1,-1, 3
  1,+1, 2
2          State 2
  1,+1, 2
  1,-1, 1

```

Since only one number was required on the STATE lines, a comment can follow the value. Although this avoided redundant data (there being no need to give the state number on every line as well) it proved to be troublesome to prepare, so for the larger descriptions (involving a conversion of state names to numbers, and symbol glyphs to symbol numbers), a retreat: present the symbol number, and allow the entries to be in any order - such as increasing. When the transition table as ingested is printed, this means that the entry numbers are in an odd order, but no matter. Should there be an error in the input, a mess will result but preparing a more flexible input arrangement would require a lot more code.

Specifying the initial state of the tape proved a bit tricky in the case when it was to be entirely blank, as end-of-file problems arose. So for that case a single zero was supplied and the problem went away... The tape's initial state is all zero anyway. Remember that in the scheme here this is symbol number zero, not "0".

After the first version was working, annoyance with the numerical symbol specification scheme prompted an escalation to the ability to specify an alphabet of arbitrary symbols, rather than symbol number 0, number 1, etc. but this could cause difficulty with the free-format reading, especially if one symbol were to be a blank. It transpired that character data could be read into character variables in free-format with or without quotes around them, provided that outside quotes a desired symbol is neither a space nor a comma. Thus the initial specification of two symbols might be "01" or "10" or "AB" or AB for the content of SYMBOLS, and when the transition table is read, the supplied symbol is sought within SYMBOLS. The internal working continues with the number of the symbol, not the symbol's numerical value.

The next escalation concerned attempts to keep track of the bounds of the read/write head's position. The original display was a bit disappointing as the great expanse of zero values was oppressive to the eye. By introducing variables HMIN and HMAX a track could be kept of what span of the tape had been explicitly initialised and seen by the read/write head. Tape cells outside those bounds still have symbol number zero (sometimes named as B) but blanks were shown. However, maintaining those bounds added to the effort of the simple interpretation loop, and after some thought it became clear that a special additional symbol could be introduced so that symbol number zero would have a space as its symbol, and the declared symbols would be numbered one to N rather than 0 to N - 1. By making its action entries the same as for the first symbol, all would be well. At the end of the run that tape (alas, a finite tape) could be scanned to ascertain the bounds of action.

In short, state one is special, it is the starting state. Symbol one (that would previously have been counted as symbol number zero) is special, it is synonymous with blank tape - which holds symbol(0), which when printed is a blank...


### Source

This is in F77 style, and just one mainline. The Turing action is effected by just five statements:
```Fortran
  200 I = STATE*NSYMBOL - ICHAR(TAPE(HEAD))	!Index the transition.
      TAPE(HEAD) = MARK(I)			!Do it. Possibly not changing the symbol.
      HEAD = HEAD + MOVE(I)			!Possibly not moving the head.
      STATE = ICHAR(NEXT(I))			!Hopefully, something has changed!
      IF (STATE.GT.0) GO TO 200			!Otherwise, we might loop forever...
```

Which manifests the ability to read and write memory, simple arithmetic, and a conditional branch. This is all it takes. And thanks to Kurt Gödel, it is known that simple arithmetic is enough to construct statements that are true, but unprovable.

The use of ICHAR() and CHAR() hopefully involves no machine code effort, they are merely a genuflection to the rules of type checking to calm the compiler. Most of the blather is concerned with acquiring the description (with minimal checking), and preparing some trace output.

Showing the head movement with a plus sign for positive movement but not zero proved a bit tricky, given that an uninitialised MOVE entry has the obtrusive value of -60 so that function SIGN doesn't help: SIGN(1,-60) gives -1, but SIGN(1,0) gives 1. SIGN(MAX(-1,i),i) would do and now this is becoming messy, so array indexing trickery is out unless a special function is to be supplied - but this is a mainline-only attempt. The SP format code causes the appearance of a plus for subsequent fields but alas, zero has a plus sign also. So, write to a text variable, place the plus sign if wanted, and write the resulting text.
```Fortran
      PROGRAM U		!Reads a specification of a Turing machine, and executes it.
Careful! Reserves a symbol #0 to represent blank tape as a blank.
      INTEGER MANY,FIRST,LAST	!Some sizes must be decided upon.
      PARAMETER (MANY = 66, FIRST = 1, LAST = 666)	!These should do.
      INTEGER HERE(MANY)
      CHARACTER*1	MARK(MANY)	!The transition table.
      INTEGER*1		MOVE(MANY)	!Three related arrays.
      CHARACTER*1	NEXT(MANY)	!All with the same indexing.
      CHARACTER*1	TAPE(FIRST:LAST)!Notionally, no final bound, in both directions - a potential infinity..
      INTEGER 		STATE		!Execution starts with state 1.
      INTEGER		HEAD		!And the tape read/write head at position 1.
      INTEGER STEP			!And we might as well keep count.
      INTEGER OFFSET			!An affine shift.
      INTEGER NSTATE			!Counts can be helpful.
      INTEGER NSYMBOL			!The count of recognised symbols.
      INTEGER S,S1			!Symbol numbers.
      CHARACTER*1 RS,WS			!Input scanning: read symbol, write symbol.
      CHARACTER*1 SYMBOL(0:MANY)	!I reserve SYMBOL(0).
      CHARACTER*(MANY) SYMBOLS		!Up to 255, for single character variables.
      EQUIVALENCE (SYMBOL(1),SYMBOLS)	!Individually or collectively.
      INTEGER I,J,K,L,IT	!Assistants.
      INTEGER LONG		!Now for some text scanning.
      PARAMETER (LONG = 80)	!This should suffice.
      CHARACTER*(LONG) ALINE	!A scratchpad.
      REAL T0,T1		!Some CPU time attempts.
      INTEGER KBD,MSG,INF	!Some I/O unit numbers.

      KBD = 5	!Standard input.
      MSG = 6	!Standard output
      INF = 10	!Suitable for a disc file.
      OPEN (INF,FILE = "TestAdd1.dat",ACTION="READ")	!Go for one.
      READ (INF,1) ALINE	!The first line is to be a heding.
    1 FORMAT (A)		!Just plain text.
      WRITE (MSG,2) ALINE	!Reveal it.
    2 FORMAT ("Turing machine simulation for... ",A)	!Announce the plan.
      READ (INF,*) SYMBOLS	!Allows a quoted string.
      NSYMBOL = LEN_TRIM(SYMBOLS)	!How many symbols? (Trailing spaces will be lost)
      WRITE (MSG,3) NSYMBOL,SYMBOLS(1:NSYMBOL)	!They will be symbol number 0, 1, ..., NSYMBOL - 1.
    3 FORMAT (I0," symbols: >",A,"<")	!And this is their count.
      IF (NSYMBOL.LE.1) STOP "Expect at least two symbols!"
      SYMBOL(0) = " "		!My special state meaning "never before seen".
      NSYMBOL = NSYMBOL + 1	!So, one more is in actual use.
      NSTATE = 0		!As for states, I haven't seen any.
      MOVE = -66		!This should cause trouble and be noticed!
      MARK = CHAR(0)		!In case a state is omitted.
      NEXT = CHAR(0)		!Like, mention state seven, but omit mention of state six.
      HERE = 0			!Clear the counts.

Collate the transition table.
   10 READ (INF,*) STATE	!Read this once, rather than for every transition.
      IF (STATE.LE.0) GO TO 20	!Ah, finished.
      WRITE (MSG,11) STATE	!But they can come in any order.
      NSTATE = MAX(STATE,NSTATE)!And I'd like to know how many.
   11 FORMAT ("Entry: Read Write Move Next. For state ",I0)	!Prepare a nice heading.
      IF (STATE.LE.0) STOP "Positive STATE numbers only!"	!It may not be followed.
      IF (STATE*NSYMBOL.GT.MANY) STOP"My transition table is too small!"	!But the value of STATE is shown.
      DO S = 0,NSYMBOL - 1	!Initialise the transitions for STATE.
        IT = STATE*NSYMBOL - S		!Finger the one for S.
        MARK(IT) = CHAR(S)		!No change to what's under the head.
        NEXT(IT) = CHAR(0)		!And this stops the run.
      END DO			!Just in case a symbol's number is omitted.
      DO S = 1,NSYMBOL - 1	!A transition for every symbol must be given or the read process will get out of step.
        READ(INF,*) RS,WS,K,L		!Read symbol, write symbol, move, next.
        I = INDEX(SYMBOLS(1:NSYMBOL - 1),RS)	!Convert the character to a symbol number.
        J = INDEX(SYMBOLS(1:NSYMBOL - 1),WS)	!To enable decorative glyphs, not just digits.
        IF (I.LE.0) STOP "Unrecognised read symbol!"	!This really should be more helpful.
        IF (J.LE.0) STOP "Unrecognised write symbol!"	!By reading into ALINE and showing it, etc.
        IT = STATE*NSYMBOL - I		!Locate the entry for the state x symbol pair.
        MARK(IT) = CHAR(J)		!The value to be written.
        MOVE(IT) = K			!The movement of the tape head.
        NEXT(IT) = CHAR(L)		!The next state.
        IF (I.EQ.1) S1 = IT		!This transition will be duplicated. SYMBOL(1) is for blank tape.
      END DO			!On to the next symbol's transition.
Copy SYMBOL(1)'s transition to the transition for the secret extra, SYMBOL(0).
      IT = STATE*NSYMBOL	!Finger the interpolated entry for SYMBOL(0).
      MARK(IT) = MARK(S1)	!Thus will SYMBOL(0), shown as a space, be overwritten.
      MOVE(IT) = MOVE(S1)	!And SYMBOL(0) treated
      NEXT(IT) = NEXT(S1)	!Exactly as if it were SYMBOL(1).
Cast forth the transition table for STATE, not mentioning SYMBOL(0) - but see label 911.
      DO S = 1,NSYMBOL - 1	!Roll them out in the order as given in SYMBOL.
        IT = STATE*NSYMBOL - S		!But the entry number will be odd.
        WRITE (ALINE,12) IT,SYMBOL(S),		!The character's code value is irrelevant.
     1   SYMBOL(ICHAR(MARK(IT))),MOVE(IT),ICHAR(NEXT(IT))	!Append the details just read.
   12   FORMAT (I5,":",2X,'"',A1,'"',3X'"',A1,'"',I5,I5,I13)	!Revealing the symbols, not their number.
        IF (MOVE(IT).GT.0) ALINE(21:21) = "+"	!I want a leading + for positive, not zero.
        WRITE (MSG,1) ALINE(1:27)	!The SP format code is unhelpful for zero.
      END DO			!Hopefully, I'm still in sync with the input.
      GO TO 10		!Perhaps another state follows.

Chew tape. The initial state is some sequence of symbols, starting at TAPE(1).
   20 TAPE = CHAR(0)		!Set every cell to zero. Not blank.
      OFFSET = 12		!Affine shift. The numerical value of HEAD is not seen.
      READ (INF,1) ALINE	!Get text, for the tape's initial state.
      L = LEN_TRIM(ALINE)	!Last non-blank. Flexible format this isn't.
      DO I = 1,L		!Step through cells 1 to L.
        TAPE(I + OFFSET - 1) = CHAR(INDEX(SYMBOLS,ALINE(I:I)))	!Character code to symbol number.
      END DO			!Rather than reading as I1.
      CLOSE (INF)		!Finished with the input, and not much checking either.
      WRITE (MSG,*)		!Take a breath.
Cast forth a heading..
      WRITE (MSG,99)		!Announce.
   99 FORMAT ("Starts with State 1 and the tape head at 1.")	!Positioned for OFFSET = 12.
      ALINE = " Step: Head State|Tape..."	!Prepare a heading for the trace.
      L = 18 + OFFSET*2		!Locate the start position.
      ALINE(L - 1:L + 1) = "<H>"!No underlining, no overprinting, no colour (neither background nor foreground). Sigh.
      WRITE (MSG,1) ALINE	!Take that!
      CALL CPU_TIME(T0)		!Start the clock.
      HEAD = OFFSET		!This is counted as position one.
      STATE = 1			!The initial state.
      STEP = 0			!No steps yet.

Chase through the transitions. Could check that HEAD is within bounds FIRST:LAST.
  100 IF (STEP.GE.200) GO TO 200	!Perhaps an extended campaign.
      STEP = STEP + 1			!Otherwise, here we go.
      DO I = 1,LONG/2		!Scan TAPE(1:LONG/2).
        IT = 2*I - 1			!Allowing two positions each.
        ALINE(IT:IT) = " "		!So a leading space.
        ALINE(IT + 1:IT + 1) = SYMBOL(ICHAR(TAPE(I)))	!And the indicated symbol.
      END DO				!On to the enxt.
      I = HEAD*2		!The head's location in the display span.
      IF (I.GT.1 .AND. I.LT.LONG) THEN	!Within range?
        IF (ALINE(I:I).EQ.SYMBOL(0)) ALINE(I:I) = SYMBOL(1)	!Yes. Am I looking at a new cell?
        ALINE(I - 1:I - 1) = "<"		!Bracket the head's cell.
        ALINE(I + 1:I + 1) = ">"		!In ALINE.
      END IF				!So much for showing the head's position.
      WRITE (MSG,102) STEP,HEAD - OFFSET + 1,STATE,ALINE	!Splot the state.
  102 FORMAT (I5,":",I5,I6,"|",A)	!Aligns with FORMAT 99.
      I = STATE*NSYMBOL - ICHAR(TAPE(HEAD))	!For this STATE and the symbol under TAPE(HEAD)
      HERE(I) = HERE(I) + 1			!Count my visits.
      TAPE(HEAD) = MARK(I)			!Place the new symbol.
      HEAD = HEAD + MOVE(I)			!Move the head.
      IF (HEAD.LT.FIRST .OR. HEAD.GT.LAST) GO TO 110	!Check the bounds.
      STATE = ICHAR(NEXT(I))			!The new state.
      IF (STATE.GT.0) GO TO 100			!Go to it.
Cease.
      I = HEAD*2			!Locate HEAD within ALINE.
      IF (I.GT.1 .AND. I.LT.LONG) ALINE(I:I) = SYMBOL(ICHAR(TAPE(HEAD)))	!The only change.
      WRITE (MSG,103) HEAD - OFFSET + 1,STATE,ALINE	!Show.
  103 FORMAT ("HALT!",I6,I6,"|",A)	!But, no step count to start with. See FORMAT 102.
      GO TO 900			!Done.
Can't continue! Insufficient tape, alas.
  110 WRITE (MSG,*) "Insufficient tape!"	!Oh dear.
      GO TO 900					!Give in.

Change into high gear: no trace and no test thereof neither.
  200 STEP = STEP + 1		!So, advance.
      IF (MOD(STEP,10000000).EQ.0) WRITE (MSG,201) STEP	!Ah, still some timewasting.
  201 FORMAT ("Step ",I0)				!No screen action is rather discouraging.
      I = STATE*NSYMBOL - ICHAR(TAPE(HEAD))	!Index the transition.
      HERE(I) = HERE(I) + 1			!Another visit.
      TAPE(HEAD) = MARK(I)			!Do it. Possibly not changing the symbol.
      HEAD = HEAD + MOVE(I)			!Possibly not moving the head.
      IF (HEAD.LT.FIRST .OR. HEAD.GT.LAST) GO TO 110	!But checking the bounds just in case.
      STATE = ICHAR(NEXT(I))			!Hopefully, something has changed!
      IF (STATE.GT.0) GO TO 200			!Otherwise, we might loop forever...

Closedown.
  900 CALL CPU_TIME(T1)		!Where did it all go?
      WRITE (MSG,901) STEP,STATE	!Announce the ending.
  901 FORMAT ("After step ",I0,", state = ",I0,".")	!Thus.
      DO I = FIRST,LAST		!Scan the tape.
        IF (ICHAR(TAPE(I)).NE.0) EXIT	!This is the whole point of SYMBOL(0).
      END DO				!So that the bounds
      DO J = LAST,FIRST,-1		!Of tape access
        IF (ICHAR(TAPE(J)).NE.0) EXIT	!(and placement of the initial state)
      END DO				!Can be found without tedious ongoing MIN and MAX.
      WRITE (MSG,902) HEAD - OFFSET + 1,	!Tediously,
     1                I - OFFSET + 1,		!Reverse the offset
     2                J - OFFSET + 1		!So as to seem that HEAD = 1, to start with.
  902 FORMAT ("The head is at position ",I0,	!Now announce the results.
     1 " and wandered over ",I0," to ",I0)	!This will affect the dimension chosen for TAPE.
      T1 = T1 - T0				!Some time may have been accurately measured.
      IF (T1.GT.0.1) WRITE (MSG,903) T1		!And this may be sort of correct.
  903 FORMAT ("CPU time",F9.3)			!Though distinct from elapsed time.
Curious about the usage of the transition table?
  910 WRITE (MSG,911)		!Possibly not,
  911 FORMAT (/,35X,"Usage.")	!But here it comes.
      DO STATE = 1,NSTATE	!For every state
        WRITE (MSG,11) STATE		!Name the state, as before.
        DO S = 0,NSYMBOL - 1		!But this time, roll every symbol.
          IT = STATE*NSYMBOL - S		!Including my "secret" symbol.
          WRITE (ALINE,12) IT,SYMBOL(S),		!The same sequence,
     1   SYMBOL(ICHAR(MARK(IT))),MOVE(IT),ICHAR(NEXT(IT)),HERE(IT)	!But with an addendum here.
        IF (MOVE(IT).GT.0) ALINE(21:21) = "+"	!SIGN(i,i) gives -1, 0, +1 but -60 for -60.
        WRITE (MSG,1) ALINE(1:40)		!When what I want is -1. SIGN(1,i) doesn't give zero.
        END DO				!On to the next symbol in the order as supplied.
      END DO			!And the next state, in numbers order.
      END	!That was fun.
```



### Results

Rather than show the input file and its possibly oddly-ordered entries, its presentation in the output is more regular if with oddly-ordered entry numbering... The symbols on the tape are presented in a two-character field so that the one under the tape read/write head can be shown <bracketed>.

### =Simple base one incrementer=


```txt

Turing machine simulation for... A simple base 1 incrementer.
2 symbols: >01<
Entry: Read Write Move Next. For state 1
    2:  "0"   "1"    0    0
    1:  "1"   "1"   +1    1

Starts with State 1 and the tape head at 1.
 Step: Head State|Tape...               <H>
    1:    1     1|                      <1>1 1
    2:    2     1|                       1<1>1
    3:    3     1|                       1 1<1>
    4:    4     1|                       1 1 1<0>
HALT!     4     0|                       1 1 1<1>
After step 4, state = 0.
The head is at position 4 and wandered over 1 to 4

                                   Usage.
Entry: Read Write Move Next. For state 1
    3:  " "   "1"    0    0            1
    2:  "0"   "1"    0    0            0
    1:  "1"   "1"   +1    1            3

```

After the run the transition table is shown again, augmented by counts for each entry, and with the extra symbol shown as well as those in the supplied table. Here is the same specification but with 01 replaced by BA:

```txt

A simple base 1 incrementer.
 BA
1          State 1
  A, A,+1, 1
  B, A, 0, 0
0          No more states.
AAA
```

With the equivalent output. No significance is ascribed to the glyphs, other than that the first symbol is taken as being synonymous with blank tape. For each state, the entry for that first symbol is duplicated for symbol zero, which is reserved for blank tape that will be depicted as blank not whatever the first symbol is - which can't be a space, given free-format input without quoting...

```txt

Turing machine simulation for... A simple base 1 incrementer.
2 symbols: >BA<
Entry: Read Write Move Next. For state 1
    2:  "B"   "A"    0    0
    1:  "A"   "A"   +1    1

Starts with State 1 and the tape head at 1.
 Step: Head State|Tape...               <H>
    1:    1     1|                      <A>A A
    2:    2     1|                       A<A>A
    3:    3     1|                       A A<A>
    4:    4     1|                       A A A<B>
HALT!     4     0|                       A A A<A>
After step 4, state = 0.
The head is at position 4 and wandered over 1 to 4

                                   Usage.
Entry: Read Write Move Next. For state 1
    3:  " "   "A"    0    0            1
    2:  "B"   "A"    0    0            0
    1:  "A"   "A"   +1    1            3

```

In other words, the glyphs chosen for symbol number one, symbol number two, etc. do not matter so long as they are used consistently.

====Three-state Busy Beaver====

```txt

Turing machine simulation for... A three-state Busy Beaver.
2 symbols: >01<
Entry: Read Write Move Next. For state 1
    2:  "0"   "1"   +1    2
    1:  "1"   "1"   -1    3
Entry: Read Write Move Next. For state 2
    5:  "0"   "1"   -1    1
    4:  "1"   "1"   +1    2
Entry: Read Write Move Next. For state 3
    8:  "0"   "1"   -1    2
    7:  "1"   "1"    0    0

Starts with State 1 and the tape head at 1.
 Step: Head State|Tape...               <H>
    1:    1     1|                      <0>
    2:    2     2|                       1<0>
    3:    1     1|                      <1>1
    4:    0     3|                    <0>1 1
    5:   -1     2|                  <0>1 1 1
    6:   -2     1|                <0>1 1 1 1
    7:   -1     2|                 1<1>1 1 1
    8:    0     2|                 1 1<1>1 1
    9:    1     2|                 1 1 1<1>1
   10:    2     2|                 1 1 1 1<1>
   11:    3     2|                 1 1 1 1 1<0>
   12:    2     1|                 1 1 1 1<1>1
   13:    1     3|                 1 1 1<1>1 1
HALT!     1     0|                 1 1 1<1>1 1
After step 13, state = 0.
The head is at position 1 and wandered over -2 to 3

                                   Usage.
Entry: Read Write Move Next. For state 1
    3:  " "   "1"   +1    2            1
    2:  "0"   "1"   +1    2            1
    1:  "1"   "1"   -1    3            2
Entry: Read Write Move Next. For state 2
    6:  " "   "1"   -1    1            3
    5:  "0"   "1"   -1    1            0
    4:  "1"   "1"   +1    2            4
Entry: Read Write Move Next. For state 3
    9:  " "   "1"   -1    2            1
    8:  "0"   "1"   -1    2            0
    7:  "1"   "1"    0    0            1

```


### =Sorting=

The TestSort.dat file is the last prepared in the reverse symbol order. Translating from the verbose description to the reduced style required care, especially because it did not specify some combinations, that presumably would never arise.

```txt

Turing machine simulation for... A sorting test.
4 symbols: >0123<
Entry: Read Write Move Next. For state 1
    4:  "0"   "0"   -1    5
    3:  "1"   "1"   +1    1
    2:  "2"   "3"   +1    2
    1:  "3"   "3"    0    0
Entry: Read Write Move Next. For state 2
    9:  "0"   "0"   -1    3
    8:  "1"   "1"   +1    2
    7:  "2"   "2"   +1    2
    6:  "3"   "3"    0    0
Entry: Read Write Move Next. For state 3
   14:  "0"   "0"    0    0
   13:  "1"   "2"   -1    4
   12:  "2"   "2"   -1    3
   11:  "3"   "2"   -1    5
Entry: Read Write Move Next. For state 4
   19:  "0"   "0"    0    0
   18:  "1"   "1"   -1    4
   17:  "2"   "2"   -1    4
   16:  "3"   "1"   +1    1
Entry: Read Write Move Next. For state 5
   24:  "0"   "0"   +1    0
   23:  "1"   "1"   -1    5
   22:  "2"   "2"    0    0
   21:  "3"   "3"    0    0

Starts with State 1 and the tape head at 1.
 Step: Head State|Tape...               <H>
    1:    1     1|                      <2>2 2 1 2 2 1 2 1 2 1 2 1 2
    2:    2     2|                       3<2>2 1 2 2 1 2 1 2 1 2 1 2
    3:    3     2|                       3 2<2>1 2 2 1 2 1 2 1 2 1 2
    4:    4     2|                       3 2 2<1>2 2 1 2 1 2 1 2 1 2
    5:    5     2|                       3 2 2 1<2>2 1 2 1 2 1 2 1 2
    6:    6     2|                       3 2 2 1 2<2>1 2 1 2 1 2 1 2
    7:    7     2|                       3 2 2 1 2 2<1>2 1 2 1 2 1 2
    8:    8     2|                       3 2 2 1 2 2 1<2>1 2 1 2 1 2
    9:    9     2|                       3 2 2 1 2 2 1 2<1>2 1 2 1 2
   10:   10     2|                       3 2 2 1 2 2 1 2 1<2>1 2 1 2
   11:   11     2|                       3 2 2 1 2 2 1 2 1 2<1>2 1 2
   12:   12     2|                       3 2 2 1 2 2 1 2 1 2 1<2>1 2
   13:   13     2|                       3 2 2 1 2 2 1 2 1 2 1 2<1>2
   14:   14     2|                       3 2 2 1 2 2 1 2 1 2 1 2 1<2>
   15:   15     2|                       3 2 2 1 2 2 1 2 1 2 1 2 1 2<0>
   16:   14     3|                       3 2 2 1 2 2 1 2 1 2 1 2 1<2>0
   17:   13     3|                       3 2 2 1 2 2 1 2 1 2 1 2<1>2 0
   18:   12     4|                       3 2 2 1 2 2 1 2 1 2 1<2>2 2 0
   19:   11     4|                       3 2 2 1 2 2 1 2 1 2<1>2 2 2 0
   20:   10     4|                       3 2 2 1 2 2 1 2 1<2>1 2 2 2 0
   21:    9     4|                       3 2 2 1 2 2 1 2<1>2 1 2 2 2 0
   22:    8     4|                       3 2 2 1 2 2 1<2>1 2 1 2 2 2 0
   23:    7     4|                       3 2 2 1 2 2<1>2 1 2 1 2 2 2 0
   24:    6     4|                       3 2 2 1 2<2>1 2 1 2 1 2 2 2 0
   25:    5     4|                       3 2 2 1<2>2 1 2 1 2 1 2 2 2 0
   26:    4     4|                       3 2 2<1>2 2 1 2 1 2 1 2 2 2 0
   27:    3     4|                       3 2<2>1 2 2 1 2 1 2 1 2 2 2 0
   28:    2     4|                       3<2>2 1 2 2 1 2 1 2 1 2 2 2 0
   29:    1     4|                      <3>2 2 1 2 2 1 2 1 2 1 2 2 2 0
   30:    2     1|                       1<2>2 1 2 2 1 2 1 2 1 2 2 2 0
   31:    3     2|                       1 3<2>1 2 2 1 2 1 2 1 2 2 2 0
   32:    4     2|                       1 3 2<1>2 2 1 2 1 2 1 2 2 2 0
   33:    5     2|                       1 3 2 1<2>2 1 2 1 2 1 2 2 2 0
   34:    6     2|                       1 3 2 1 2<2>1 2 1 2 1 2 2 2 0
   35:    7     2|                       1 3 2 1 2 2<1>2 1 2 1 2 2 2 0
   36:    8     2|                       1 3 2 1 2 2 1<2>1 2 1 2 2 2 0
   37:    9     2|                       1 3 2 1 2 2 1 2<1>2 1 2 2 2 0
   38:   10     2|                       1 3 2 1 2 2 1 2 1<2>1 2 2 2 0
   39:   11     2|                       1 3 2 1 2 2 1 2 1 2<1>2 2 2 0
   40:   12     2|                       1 3 2 1 2 2 1 2 1 2 1<2>2 2 0
   41:   13     2|                       1 3 2 1 2 2 1 2 1 2 1 2<2>2 0
   42:   14     2|                       1 3 2 1 2 2 1 2 1 2 1 2 2<2>0
   43:   15     2|                       1 3 2 1 2 2 1 2 1 2 1 2 2 2<0>
   44:   14     3|                       1 3 2 1 2 2 1 2 1 2 1 2 2<2>0
   45:   13     3|                       1 3 2 1 2 2 1 2 1 2 1 2<2>2 0
   46:   12     3|                       1 3 2 1 2 2 1 2 1 2 1<2>2 2 0
   47:   11     3|                       1 3 2 1 2 2 1 2 1 2<1>2 2 2 0
   48:   10     4|                       1 3 2 1 2 2 1 2 1<2>2 2 2 2 0
   49:    9     4|                       1 3 2 1 2 2 1 2<1>2 2 2 2 2 0
   50:    8     4|                       1 3 2 1 2 2 1<2>1 2 2 2 2 2 0
   51:    7     4|                       1 3 2 1 2 2<1>2 1 2 2 2 2 2 0
   52:    6     4|                       1 3 2 1 2<2>1 2 1 2 2 2 2 2 0
   53:    5     4|                       1 3 2 1<2>2 1 2 1 2 2 2 2 2 0
   54:    4     4|                       1 3 2<1>2 2 1 2 1 2 2 2 2 2 0
   55:    3     4|                       1 3<2>1 2 2 1 2 1 2 2 2 2 2 0
   56:    2     4|                       1<3>2 1 2 2 1 2 1 2 2 2 2 2 0
   57:    3     1|                       1 1<2>1 2 2 1 2 1 2 2 2 2 2 0
   58:    4     2|                       1 1 3<1>2 2 1 2 1 2 2 2 2 2 0
   59:    5     2|                       1 1 3 1<2>2 1 2 1 2 2 2 2 2 0
   60:    6     2|                       1 1 3 1 2<2>1 2 1 2 2 2 2 2 0
   61:    7     2|                       1 1 3 1 2 2<1>2 1 2 2 2 2 2 0
   62:    8     2|                       1 1 3 1 2 2 1<2>1 2 2 2 2 2 0
   63:    9     2|                       1 1 3 1 2 2 1 2<1>2 2 2 2 2 0
   64:   10     2|                       1 1 3 1 2 2 1 2 1<2>2 2 2 2 0
   65:   11     2|                       1 1 3 1 2 2 1 2 1 2<2>2 2 2 0
   66:   12     2|                       1 1 3 1 2 2 1 2 1 2 2<2>2 2 0
   67:   13     2|                       1 1 3 1 2 2 1 2 1 2 2 2<2>2 0
   68:   14     2|                       1 1 3 1 2 2 1 2 1 2 2 2 2<2>0
   69:   15     2|                       1 1 3 1 2 2 1 2 1 2 2 2 2 2<0>
   70:   14     3|                       1 1 3 1 2 2 1 2 1 2 2 2 2<2>0
   71:   13     3|                       1 1 3 1 2 2 1 2 1 2 2 2<2>2 0
   72:   12     3|                       1 1 3 1 2 2 1 2 1 2 2<2>2 2 0
   73:   11     3|                       1 1 3 1 2 2 1 2 1 2<2>2 2 2 0
   74:   10     3|                       1 1 3 1 2 2 1 2 1<2>2 2 2 2 0
   75:    9     3|                       1 1 3 1 2 2 1 2<1>2 2 2 2 2 0
   76:    8     4|                       1 1 3 1 2 2 1<2>2 2 2 2 2 2 0
   77:    7     4|                       1 1 3 1 2 2<1>2 2 2 2 2 2 2 0
   78:    6     4|                       1 1 3 1 2<2>1 2 2 2 2 2 2 2 0
   79:    5     4|                       1 1 3 1<2>2 1 2 2 2 2 2 2 2 0
   80:    4     4|                       1 1 3<1>2 2 1 2 2 2 2 2 2 2 0
   81:    3     4|                       1 1<3>1 2 2 1 2 2 2 2 2 2 2 0
   82:    4     1|                       1 1 1<1>2 2 1 2 2 2 2 2 2 2 0
   83:    5     1|                       1 1 1 1<2>2 1 2 2 2 2 2 2 2 0
   84:    6     2|                       1 1 1 1 3<2>1 2 2 2 2 2 2 2 0
   85:    7     2|                       1 1 1 1 3 2<1>2 2 2 2 2 2 2 0
   86:    8     2|                       1 1 1 1 3 2 1<2>2 2 2 2 2 2 0
   87:    9     2|                       1 1 1 1 3 2 1 2<2>2 2 2 2 2 0
   88:   10     2|                       1 1 1 1 3 2 1 2 2<2>2 2 2 2 0
   89:   11     2|                       1 1 1 1 3 2 1 2 2 2<2>2 2 2 0
   90:   12     2|                       1 1 1 1 3 2 1 2 2 2 2<2>2 2 0
   91:   13     2|                       1 1 1 1 3 2 1 2 2 2 2 2<2>2 0
   92:   14     2|                       1 1 1 1 3 2 1 2 2 2 2 2 2<2>0
   93:   15     2|                       1 1 1 1 3 2 1 2 2 2 2 2 2 2<0>
   94:   14     3|                       1 1 1 1 3 2 1 2 2 2 2 2 2<2>0
   95:   13     3|                       1 1 1 1 3 2 1 2 2 2 2 2<2>2 0
   96:   12     3|                       1 1 1 1 3 2 1 2 2 2 2<2>2 2 0
   97:   11     3|                       1 1 1 1 3 2 1 2 2 2<2>2 2 2 0
   98:   10     3|                       1 1 1 1 3 2 1 2 2<2>2 2 2 2 0
   99:    9     3|                       1 1 1 1 3 2 1 2<2>2 2 2 2 2 0
  100:    8     3|                       1 1 1 1 3 2 1<2>2 2 2 2 2 2 0
  101:    7     3|                       1 1 1 1 3 2<1>2 2 2 2 2 2 2 0
  102:    6     4|                       1 1 1 1 3<2>2 2 2 2 2 2 2 2 0
  103:    5     4|                       1 1 1 1<3>2 2 2 2 2 2 2 2 2 0
  104:    6     1|                       1 1 1 1 1<2>2 2 2 2 2 2 2 2 0
  105:    7     2|                       1 1 1 1 1 3<2>2 2 2 2 2 2 2 0
  106:    8     2|                       1 1 1 1 1 3 2<2>2 2 2 2 2 2 0
  107:    9     2|                       1 1 1 1 1 3 2 2<2>2 2 2 2 2 0
  108:   10     2|                       1 1 1 1 1 3 2 2 2<2>2 2 2 2 0
  109:   11     2|                       1 1 1 1 1 3 2 2 2 2<2>2 2 2 0
  110:   12     2|                       1 1 1 1 1 3 2 2 2 2 2<2>2 2 0
  111:   13     2|                       1 1 1 1 1 3 2 2 2 2 2 2<2>2 0
  112:   14     2|                       1 1 1 1 1 3 2 2 2 2 2 2 2<2>0
  113:   15     2|                       1 1 1 1 1 3 2 2 2 2 2 2 2 2<0>
  114:   14     3|                       1 1 1 1 1 3 2 2 2 2 2 2 2<2>0
  115:   13     3|                       1 1 1 1 1 3 2 2 2 2 2 2<2>2 0
  116:   12     3|                       1 1 1 1 1 3 2 2 2 2 2<2>2 2 0
  117:   11     3|                       1 1 1 1 1 3 2 2 2 2<2>2 2 2 0
  118:   10     3|                       1 1 1 1 1 3 2 2 2<2>2 2 2 2 0
  119:    9     3|                       1 1 1 1 1 3 2 2<2>2 2 2 2 2 0
  120:    8     3|                       1 1 1 1 1 3 2<2>2 2 2 2 2 2 0
  121:    7     3|                       1 1 1 1 1 3<2>2 2 2 2 2 2 2 0
  122:    6     3|                       1 1 1 1 1<3>2 2 2 2 2 2 2 2 0
  123:    5     5|                       1 1 1 1<1>2 2 2 2 2 2 2 2 2 0
  124:    4     5|                       1 1 1<1>1 2 2 2 2 2 2 2 2 2 0
  125:    3     5|                       1 1<1>1 1 2 2 2 2 2 2 2 2 2 0
  126:    2     5|                       1<1>1 1 1 2 2 2 2 2 2 2 2 2 0
  127:    1     5|                      <1>1 1 1 1 2 2 2 2 2 2 2 2 2 0
  128:    0     5|                    <0>1 1 1 1 1 2 2 2 2 2 2 2 2 2 0
HALT!     1     0|                    <0>1 1 1 1 1 2 2 2 2 2 2 2 2 2 0
After step 128, state = 0.
The head is at position 1 and wandered over 0 to 15

                                   Usage.
Entry: Read Write Move Next. For state 1
    5:  " "   "0"   -1    5            0
    4:  "0"   "0"   -1    5            0
    3:  "1"   "1"   +1    1            1
    2:  "2"   "3"   +1    2            5
    1:  "3"   "3"    0    0            0
Entry: Read Write Move Next. For state 2
   10:  " "   "0"   -1    3            1
    9:  "0"   "0"   -1    3            4
    8:  "1"   "1"   +1    2           13
    7:  "2"   "2"   +1    2           40
    6:  "3"   "3"    0    0            0
Entry: Read Write Move Next. For state 3
   15:  " "   "0"    0    0            0
   14:  "0"   "0"    0    0            0
   13:  "1"   "2"   -1    4            4
   12:  "2"   "2"   -1    3           24
   11:  "3"   "2"   -1    5            1
Entry: Read Write Move Next. For state 4
   20:  " "   "0"    0    0            0
   19:  "0"   "0"    0    0            0
   18:  "1"   "1"   -1    4            9
   17:  "2"   "2"   -1    4           16
   16:  "3"   "1"   +1    1            4
Entry: Read Write Move Next. For state 5
   25:  " "   "0"   +1    0            1
   24:  "0"   "0"   +1    0            0
   23:  "1"   "1"   -1    5            5
   22:  "2"   "2"    0    0            0
   21:  "3"   "3"    0    0            0

```


====Five-state Busy Beaver====
The TestBB5.dat run required modifying the source with FIRST = -12345 and OFFSET = 20 so that the trace didn't hit the bounds.

```txt

Turing machine simulation for... A five-state Busy Beaver.
2 symbols: >01<
Entry: Read Write Move Next. For state 1
    2:  "0"   "1"   +1    2
    1:  "1"   "1"   -1    3
Entry: Read Write Move Next. For state 2
    5:  "0"   "1"   +1    3
    4:  "1"   "1"   +1    2
Entry: Read Write Move Next. For state 3
    8:  "0"   "1"   +1    4
    7:  "1"   "0"   -1    5
Entry: Read Write Move Next. For state 4
   11:  "0"   "1"   -1    1
   10:  "1"   "1"   -1    4
Entry: Read Write Move Next. For state 5
   14:  "0"   "1"    0    0
   13:  "1"   "0"   -1    1

Starts with State 1 and the tape head at 1.
 Step: Head State|Tape...                               <H>
    1:    1     1|                                      <0>
    2:    2     2|                                       1<0>
    3:    3     3|                                       1 1<0>
    4:    4     4|                                       1 1 1<0>
    5:    3     1|                                       1 1<1>1
    6:    2     3|                                       1<1>1 1
    7:    1     5|                                      <1>0 1 1
    8:    0     1|                                    <0>0 0 1 1
    9:    1     2|                                     1<0>0 1 1
   10:    2     3|                                     1 1<0>1 1
   11:    3     4|                                     1 1 1<1>1
   12:    2     4|                                     1 1<1>1 1
   13:    1     4|                                     1<1>1 1 1
   14:    0     4|                                    <1>1 1 1 1
   15:   -1     4|                                  <0>1 1 1 1 1
   16:   -2     1|                                <0>1 1 1 1 1 1
   17:   -1     2|                                 1<1>1 1 1 1 1
   18:    0     2|                                 1 1<1>1 1 1 1
   19:    1     2|                                 1 1 1<1>1 1 1
   20:    2     2|                                 1 1 1 1<1>1 1
   21:    3     2|                                 1 1 1 1 1<1>1
   22:    4     2|                                 1 1 1 1 1 1<1>
   23:    5     2|                                 1 1 1 1 1 1 1<0>
   24:    6     3|                                 1 1 1 1 1 1 1 1<0>
   25:    7     4|                                 1 1 1 1 1 1 1 1 1<0>
   26:    6     1|                                 1 1 1 1 1 1 1 1<1>1
   27:    5     3|                                 1 1 1 1 1 1 1<1>1 1
   28:    4     5|                                 1 1 1 1 1 1<1>0 1 1
   29:    3     1|                                 1 1 1 1 1<1>0 0 1 1
   30:    2     3|                                 1 1 1 1<1>1 0 0 1 1
   31:    1     5|                                 1 1 1<1>0 1 0 0 1 1
   32:    0     1|                                 1 1<1>0 0 1 0 0 1 1
   33:   -1     3|                                 1<1>1 0 0 1 0 0 1 1
   34:   -2     5|                                <1>0 1 0 0 1 0 0 1 1
   35:   -3     1|                              <0>0 0 1 0 0 1 0 0 1 1
   36:   -2     2|                               1<0>0 1 0 0 1 0 0 1 1
   37:   -1     3|                               1 1<0>1 0 0 1 0 0 1 1
   38:    0     4|                               1 1 1<1>0 0 1 0 0 1 1
   39:   -1     4|                               1 1<1>1 0 0 1 0 0 1 1
   40:   -2     4|                               1<1>1 1 0 0 1 0 0 1 1
   41:   -3     4|                              <1>1 1 1 0 0 1 0 0 1 1
   42:   -4     4|                            <0>1 1 1 1 0 0 1 0 0 1 1
   43:   -5     1|                          <0>1 1 1 1 1 0 0 1 0 0 1 1
   44:   -4     2|                           1<1>1 1 1 1 0 0 1 0 0 1 1
   45:   -3     2|                           1 1<1>1 1 1 0 0 1 0 0 1 1
   46:   -2     2|                           1 1 1<1>1 1 0 0 1 0 0 1 1
   47:   -1     2|                           1 1 1 1<1>1 0 0 1 0 0 1 1
   48:    0     2|                           1 1 1 1 1<1>0 0 1 0 0 1 1
   49:    1     2|                           1 1 1 1 1 1<0>0 1 0 0 1 1
   50:    2     3|                           1 1 1 1 1 1 1<0>1 0 0 1 1
   51:    3     4|                           1 1 1 1 1 1 1 1<1>0 0 1 1
   52:    2     4|                           1 1 1 1 1 1 1<1>1 0 0 1 1
   53:    1     4|                           1 1 1 1 1 1<1>1 1 0 0 1 1
   54:    0     4|                           1 1 1 1 1<1>1 1 1 0 0 1 1
   55:   -1     4|                           1 1 1 1<1>1 1 1 1 0 0 1 1
   56:   -2     4|                           1 1 1<1>1 1 1 1 1 0 0 1 1
   57:   -3     4|                           1 1<1>1 1 1 1 1 1 0 0 1 1
   58:   -4     4|                           1<1>1 1 1 1 1 1 1 0 0 1 1
   59:   -5     4|                          <1>1 1 1 1 1 1 1 1 0 0 1 1
   60:   -6     4|                        <0>1 1 1 1 1 1 1 1 1 0 0 1 1
   61:   -7     1|                      <0>1 1 1 1 1 1 1 1 1 1 0 0 1 1
   62:   -6     2|                       1<1>1 1 1 1 1 1 1 1 1 0 0 1 1
   63:   -5     2|                       1 1<1>1 1 1 1 1 1 1 1 0 0 1 1
   64:   -4     2|                       1 1 1<1>1 1 1 1 1 1 1 0 0 1 1
   65:   -3     2|                       1 1 1 1<1>1 1 1 1 1 1 0 0 1 1
   66:   -2     2|                       1 1 1 1 1<1>1 1 1 1 1 0 0 1 1
   67:   -1     2|                       1 1 1 1 1 1<1>1 1 1 1 0 0 1 1
   68:    0     2|                       1 1 1 1 1 1 1<1>1 1 1 0 0 1 1
   69:    1     2|                       1 1 1 1 1 1 1 1<1>1 1 0 0 1 1
   70:    2     2|                       1 1 1 1 1 1 1 1 1<1>1 0 0 1 1
   71:    3     2|                       1 1 1 1 1 1 1 1 1 1<1>0 0 1 1
   72:    4     2|                       1 1 1 1 1 1 1 1 1 1 1<0>0 1 1
   73:    5     3|                       1 1 1 1 1 1 1 1 1 1 1 1<0>1 1
   74:    6     4|                       1 1 1 1 1 1 1 1 1 1 1 1 1<1>1
   75:    5     4|                       1 1 1 1 1 1 1 1 1 1 1 1<1>1 1
   76:    4     4|                       1 1 1 1 1 1 1 1 1 1 1<1>1 1 1
   77:    3     4|                       1 1 1 1 1 1 1 1 1 1<1>1 1 1 1
   78:    2     4|                       1 1 1 1 1 1 1 1 1<1>1 1 1 1 1
   79:    1     4|                       1 1 1 1 1 1 1 1<1>1 1 1 1 1 1
   80:    0     4|                       1 1 1 1 1 1 1<1>1 1 1 1 1 1 1
   81:   -1     4|                       1 1 1 1 1 1<1>1 1 1 1 1 1 1 1
   82:   -2     4|                       1 1 1 1 1<1>1 1 1 1 1 1 1 1 1
   83:   -3     4|                       1 1 1 1<1>1 1 1 1 1 1 1 1 1 1
   84:   -4     4|                       1 1 1<1>1 1 1 1 1 1 1 1 1 1 1
   85:   -5     4|                       1 1<1>1 1 1 1 1 1 1 1 1 1 1 1
   86:   -6     4|                       1<1>1 1 1 1 1 1 1 1 1 1 1 1 1
   87:   -7     4|                      <1>1 1 1 1 1 1 1 1 1 1 1 1 1 1
   88:   -8     4|                    <0>1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
   89:   -9     1|                  <0>1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
   90:   -8     2|                   1<1>1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
   91:   -7     2|                   1 1<1>1 1 1 1 1 1 1 1 1 1 1 1 1 1
   92:   -6     2|                   1 1 1<1>1 1 1 1 1 1 1 1 1 1 1 1 1
   93:   -5     2|                   1 1 1 1<1>1 1 1 1 1 1 1 1 1 1 1 1
   94:   -4     2|                   1 1 1 1 1<1>1 1 1 1 1 1 1 1 1 1 1
   95:   -3     2|                   1 1 1 1 1 1<1>1 1 1 1 1 1 1 1 1 1
   96:   -2     2|                   1 1 1 1 1 1 1<1>1 1 1 1 1 1 1 1 1
   97:   -1     2|                   1 1 1 1 1 1 1 1<1>1 1 1 1 1 1 1 1
   98:    0     2|                   1 1 1 1 1 1 1 1 1<1>1 1 1 1 1 1 1
   99:    1     2|                   1 1 1 1 1 1 1 1 1 1<1>1 1 1 1 1 1
  100:    2     2|                   1 1 1 1 1 1 1 1 1 1 1<1>1 1 1 1 1
  101:    3     2|                   1 1 1 1 1 1 1 1 1 1 1 1<1>1 1 1 1
  102:    4     2|                   1 1 1 1 1 1 1 1 1 1 1 1 1<1>1 1 1
  103:    5     2|                   1 1 1 1 1 1 1 1 1 1 1 1 1 1<1>1 1
  104:    6     2|                   1 1 1 1 1 1 1 1 1 1 1 1 1 1 1<1>1
  105:    7     2|                   1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1<1>
  106:    8     2|                   1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1<0>
  107:    9     3|                   1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1<0>
  108:   10     4|                   1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1<0>
  109:    9     1|                   1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1<1>1
  110:    8     3|                   1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1<1>1 1
  111:    7     5|                   1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1<1>0 1 1
  112:    6     1|                   1 1 1 1 1 1 1 1 1 1 1 1 1 1 1<1>0 0 1 1
  113:    5     3|                   1 1 1 1 1 1 1 1 1 1 1 1 1 1<1>1 0 0 1 1
  114:    4     5|                   1 1 1 1 1 1 1 1 1 1 1 1 1<1>0 1 0 0 1 1
  115:    3     1|                   1 1 1 1 1 1 1 1 1 1 1 1<1>0 0 1 0 0 1 1
  116:    2     3|                   1 1 1 1 1 1 1 1 1 1 1<1>1 0 0 1 0 0 1 1
  117:    1     5|                   1 1 1 1 1 1 1 1 1 1<1>0 1 0 0 1 0 0 1 1
  118:    0     1|                   1 1 1 1 1 1 1 1 1<1>0 0 1 0 0 1 0 0 1 1
  119:   -1     3|                   1 1 1 1 1 1 1 1<1>1 0 0 1 0 0 1 0 0 1 1
  120:   -2     5|                   1 1 1 1 1 1 1<1>0 1 0 0 1 0 0 1 0 0 1 1
  121:   -3     1|                   1 1 1 1 1 1<1>0 0 1 0 0 1 0 0 1 0 0 1 1
  122:   -4     3|                   1 1 1 1 1<1>1 0 0 1 0 0 1 0 0 1 0 0 1 1
  123:   -5     5|                   1 1 1 1<1>0 1 0 0 1 0 0 1 0 0 1 0 0 1 1
  124:   -6     1|                   1 1 1<1>0 0 1 0 0 1 0 0 1 0 0 1 0 0 1 1
  125:   -7     3|                   1 1<1>1 0 0 1 0 0 1 0 0 1 0 0 1 0 0 1 1
  126:   -8     5|                   1<1>0 1 0 0 1 0 0 1 0 0 1 0 0 1 0 0 1 1
  127:   -9     1|                  <1>0 0 1 0 0 1 0 0 1 0 0 1 0 0 1 0 0 1 1
  128:  -10     3|                <0>1 0 0 1 0 0 1 0 0 1 0 0 1 0 0 1 0 0 1 1
  129:   -9     4|                 1<1>0 0 1 0 0 1 0 0 1 0 0 1 0 0 1 0 0 1 1
  130:  -10     4|                <1>1 0 0 1 0 0 1 0 0 1 0 0 1 0 0 1 0 0 1 1
  131:  -11     4|              <0>1 1 0 0 1 0 0 1 0 0 1 0 0 1 0 0 1 0 0 1 1
  132:  -12     1|            <0>1 1 1 0 0 1 0 0 1 0 0 1 0 0 1 0 0 1 0 0 1 1
  133:  -11     2|             1<1>1 1 0 0 1 0 0 1 0 0 1 0 0 1 0 0 1 0 0 1 1
  134:  -10     2|             1 1<1>1 0 0 1 0 0 1 0 0 1 0 0 1 0 0 1 0 0 1 1
  135:   -9     2|             1 1 1<1>0 0 1 0 0 1 0 0 1 0 0 1 0 0 1 0 0 1 1
  136:   -8     2|             1 1 1 1<0>0 1 0 0 1 0 0 1 0 0 1 0 0 1 0 0 1 1
  137:   -7     3|             1 1 1 1 1<0>1 0 0 1 0 0 1 0 0 1 0 0 1 0 0 1 1
  138:   -6     4|             1 1 1 1 1 1<1>0 0 1 0 0 1 0 0 1 0 0 1 0 0 1 1
  139:   -7     4|             1 1 1 1 1<1>1 0 0 1 0 0 1 0 0 1 0 0 1 0 0 1 1
  140:   -8     4|             1 1 1 1<1>1 1 0 0 1 0 0 1 0 0 1 0 0 1 0 0 1 1
  141:   -9     4|             1 1 1<1>1 1 1 0 0 1 0 0 1 0 0 1 0 0 1 0 0 1 1
  142:  -10     4|             1 1<1>1 1 1 1 0 0 1 0 0 1 0 0 1 0 0 1 0 0 1 1
  143:  -11     4|             1<1>1 1 1 1 1 0 0 1 0 0 1 0 0 1 0 0 1 0 0 1 1
  144:  -12     4|            <1>1 1 1 1 1 1 0 0 1 0 0 1 0 0 1 0 0 1 0 0 1 1
  145:  -13     4|          <0>1 1 1 1 1 1 1 0 0 1 0 0 1 0 0 1 0 0 1 0 0 1 1
  146:  -14     1|        <0>1 1 1 1 1 1 1 1 0 0 1 0 0 1 0 0 1 0 0 1 0 0 1 1
  147:  -13     2|         1<1>1 1 1 1 1 1 1 0 0 1 0 0 1 0 0 1 0 0 1 0 0 1 1
  148:  -12     2|         1 1<1>1 1 1 1 1 1 0 0 1 0 0 1 0 0 1 0 0 1 0 0 1 1
  149:  -11     2|         1 1 1<1>1 1 1 1 1 0 0 1 0 0 1 0 0 1 0 0 1 0 0 1 1
  150:  -10     2|         1 1 1 1<1>1 1 1 1 0 0 1 0 0 1 0 0 1 0 0 1 0 0 1 1
  151:   -9     2|         1 1 1 1 1<1>1 1 1 0 0 1 0 0 1 0 0 1 0 0 1 0 0 1 1
  152:   -8     2|         1 1 1 1 1 1<1>1 1 0 0 1 0 0 1 0 0 1 0 0 1 0 0 1 1
  153:   -7     2|         1 1 1 1 1 1 1<1>1 0 0 1 0 0 1 0 0 1 0 0 1 0 0 1 1
  154:   -6     2|         1 1 1 1 1 1 1 1<1>0 0 1 0 0 1 0 0 1 0 0 1 0 0 1 1
  155:   -5     2|         1 1 1 1 1 1 1 1 1<0>0 1 0 0 1 0 0 1 0 0 1 0 0 1 1
  156:   -4     3|         1 1 1 1 1 1 1 1 1 1<0>1 0 0 1 0 0 1 0 0 1 0 0 1 1
  157:   -3     4|         1 1 1 1 1 1 1 1 1 1 1<1>0 0 1 0 0 1 0 0 1 0 0 1 1
  158:   -4     4|         1 1 1 1 1 1 1 1 1 1<1>1 0 0 1 0 0 1 0 0 1 0 0 1 1
  159:   -5     4|         1 1 1 1 1 1 1 1 1<1>1 1 0 0 1 0 0 1 0 0 1 0 0 1 1
  160:   -6     4|         1 1 1 1 1 1 1 1<1>1 1 1 0 0 1 0 0 1 0 0 1 0 0 1 1
  161:   -7     4|         1 1 1 1 1 1 1<1>1 1 1 1 0 0 1 0 0 1 0 0 1 0 0 1 1
  162:   -8     4|         1 1 1 1 1 1<1>1 1 1 1 1 0 0 1 0 0 1 0 0 1 0 0 1 1
  163:   -9     4|         1 1 1 1 1<1>1 1 1 1 1 1 0 0 1 0 0 1 0 0 1 0 0 1 1
  164:  -10     4|         1 1 1 1<1>1 1 1 1 1 1 1 0 0 1 0 0 1 0 0 1 0 0 1 1
  165:  -11     4|         1 1 1<1>1 1 1 1 1 1 1 1 0 0 1 0 0 1 0 0 1 0 0 1 1
  166:  -12     4|         1 1<1>1 1 1 1 1 1 1 1 1 0 0 1 0 0 1 0 0 1 0 0 1 1
  167:  -13     4|         1<1>1 1 1 1 1 1 1 1 1 1 0 0 1 0 0 1 0 0 1 0 0 1 1
  168:  -14     4|        <1>1 1 1 1 1 1 1 1 1 1 1 0 0 1 0 0 1 0 0 1 0 0 1 1
  169:  -15     4|      <0>1 1 1 1 1 1 1 1 1 1 1 1 0 0 1 0 0 1 0 0 1 0 0 1 1
  170:  -16     1|    <0>1 1 1 1 1 1 1 1 1 1 1 1 1 0 0 1 0 0 1 0 0 1 0 0 1 1
  171:  -15     2|     1<1>1 1 1 1 1 1 1 1 1 1 1 1 0 0 1 0 0 1 0 0 1 0 0 1 1
  172:  -14     2|     1 1<1>1 1 1 1 1 1 1 1 1 1 1 0 0 1 0 0 1 0 0 1 0 0 1 1
  173:  -13     2|     1 1 1<1>1 1 1 1 1 1 1 1 1 1 0 0 1 0 0 1 0 0 1 0 0 1 1
  174:  -12     2|     1 1 1 1<1>1 1 1 1 1 1 1 1 1 0 0 1 0 0 1 0 0 1 0 0 1 1
  175:  -11     2|     1 1 1 1 1<1>1 1 1 1 1 1 1 1 0 0 1 0 0 1 0 0 1 0 0 1 1
  176:  -10     2|     1 1 1 1 1 1<1>1 1 1 1 1 1 1 0 0 1 0 0 1 0 0 1 0 0 1 1
  177:   -9     2|     1 1 1 1 1 1 1<1>1 1 1 1 1 1 0 0 1 0 0 1 0 0 1 0 0 1 1
  178:   -8     2|     1 1 1 1 1 1 1 1<1>1 1 1 1 1 0 0 1 0 0 1 0 0 1 0 0 1 1
  179:   -7     2|     1 1 1 1 1 1 1 1 1<1>1 1 1 1 0 0 1 0 0 1 0 0 1 0 0 1 1
  180:   -6     2|     1 1 1 1 1 1 1 1 1 1<1>1 1 1 0 0 1 0 0 1 0 0 1 0 0 1 1
  181:   -5     2|     1 1 1 1 1 1 1 1 1 1 1<1>1 1 0 0 1 0 0 1 0 0 1 0 0 1 1
  182:   -4     2|     1 1 1 1 1 1 1 1 1 1 1 1<1>1 0 0 1 0 0 1 0 0 1 0 0 1 1
  183:   -3     2|     1 1 1 1 1 1 1 1 1 1 1 1 1<1>0 0 1 0 0 1 0 0 1 0 0 1 1
  184:   -2     2|     1 1 1 1 1 1 1 1 1 1 1 1 1 1<0>0 1 0 0 1 0 0 1 0 0 1 1
  185:   -1     3|     1 1 1 1 1 1 1 1 1 1 1 1 1 1 1<0>1 0 0 1 0 0 1 0 0 1 1
  186:    0     4|     1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1<1>0 0 1 0 0 1 0 0 1 1
  187:   -1     4|     1 1 1 1 1 1 1 1 1 1 1 1 1 1 1<1>1 0 0 1 0 0 1 0 0 1 1
  188:   -2     4|     1 1 1 1 1 1 1 1 1 1 1 1 1 1<1>1 1 0 0 1 0 0 1 0 0 1 1
  189:   -3     4|     1 1 1 1 1 1 1 1 1 1 1 1 1<1>1 1 1 0 0 1 0 0 1 0 0 1 1
  190:   -4     4|     1 1 1 1 1 1 1 1 1 1 1 1<1>1 1 1 1 0 0 1 0 0 1 0 0 1 1
  191:   -5     4|     1 1 1 1 1 1 1 1 1 1 1<1>1 1 1 1 1 0 0 1 0 0 1 0 0 1 1
  192:   -6     4|     1 1 1 1 1 1 1 1 1 1<1>1 1 1 1 1 1 0 0 1 0 0 1 0 0 1 1
  193:   -7     4|     1 1 1 1 1 1 1 1 1<1>1 1 1 1 1 1 1 0 0 1 0 0 1 0 0 1 1
  194:   -8     4|     1 1 1 1 1 1 1 1<1>1 1 1 1 1 1 1 1 0 0 1 0 0 1 0 0 1 1
  195:   -9     4|     1 1 1 1 1 1 1<1>1 1 1 1 1 1 1 1 1 0 0 1 0 0 1 0 0 1 1
  196:  -10     4|     1 1 1 1 1 1<1>1 1 1 1 1 1 1 1 1 1 0 0 1 0 0 1 0 0 1 1
  197:  -11     4|     1 1 1 1 1<1>1 1 1 1 1 1 1 1 1 1 1 0 0 1 0 0 1 0 0 1 1
  198:  -12     4|     1 1 1 1<1>1 1 1 1 1 1 1 1 1 1 1 1 0 0 1 0 0 1 0 0 1 1
  199:  -13     4|     1 1 1<1>1 1 1 1 1 1 1 1 1 1 1 1 1 0 0 1 0 0 1 0 0 1 1
  200:  -14     4|     1 1<1>1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 0 1 0 0 1 0 0 1 1
Step 10000000
Step 20000000
Step 30000000
Step 40000000
After step 47176870, state = 0.
The head is at position -12242 and wandered over -12242 to 46
CPU time    0.641

                                   Usage.
Entry: Read Write Move Next. For state 1
    3:  " "   "1"   +1    2         6121
    2:  "0"   "1"   +1    2            1
    1:  "1"   "1"   -1    3        10210
Entry: Read Write Move Next. For state 2
    6:  " "   "1"   +1    3           15
    5:  "0"   "1"   +1    3         6107
    4:  "1"   "1"   +1    2     23563940
Entry: Read Write Move Next. For state 3
    9:  " "   "1"   +1    4           22
    8:  "0"   "1"   +1    4         6107
    7:  "1"   "0"   -1    5        10203
Entry: Read Write Move Next. For state 4
   12:  " "   "1"   -1    1         6129
   11:  "0"   "1"   -1    1            0
   10:  "1"   "1"   -1    4     23557812
Entry: Read Write Move Next. For state 5
   15:  " "   "1"    0    0            1
   14:  "0"   "1"    0    0            0
   13:  "1"   "0"   -1    1        10202

```

Removing the code maintaining the bounds HMIN and HMAX as HEAD wandered about reduced the cpu time from 0·578 to 0·562, and reducing the progress report to every ten-millionth step reduced that to 0·516. But adding the code to count the usage of each transition raised  it to 0·641. Removing the usage count and the progress report got it down to 0·484. Removing the compiler's array bound checking and activating "maximum optimisations" achieved 0·312. Reverting to normal runs and introducing the check <code>IF (HEAD.LT.FIRST .OR. HEAD.GT.LAST)</code> changed the run time from 0·703 to 0·75. Thus, 47 million tests in 0·05 seconds - not much of a cost for peace of mind.


## Go


```go
package turing

type Symbol byte

type Motion byte

const (
        Left  Motion = 'L'
        Right Motion = 'R'
        Stay  Motion = 'N'
)

type Tape struct {
        data      []Symbol
        pos, left int
        blank     Symbol
}

// NewTape returns a new tape filled with 'data' and position set to 'start'.
// 'start' does not need to be range, the tape will be extended if required.
func NewTape(blank Symbol, start int, data []Symbol) *Tape {
        t := &Tape{
                data:  data,
                blank: blank,
        }
        if start < 0 {
                t.Left(-start)
        }
        t.Right(start)
        return t
}

func (t *Tape) Stay()          {}
func (t *Tape) Data() []Symbol { return t.data[t.left:] }
func (t *Tape) Read() Symbol   { return t.data[t.pos] }
func (t *Tape) Write(s Symbol) { t.data[t.pos] = s }

func (t *Tape) Dup() *Tape {
        t2 := &Tape{
                data:  make([]Symbol, len(t.Data())),
                blank: t.blank,
        }
        copy(t2.data, t.Data())
        t2.pos = t.pos - t.left
        return t2
}

func (t *Tape) String() string {
        s := ""
        for i := t.left; i < len(t.data); i++ {
                b := t.data[i]
                if i == t.pos {
                        s += "[" + string(b) + "]"
                } else {
                        s += " " + string(b) + " "
                }
        }
        return s
}

func (t *Tape) Move(a Motion) {
        switch a {
        case Left:
                t.Left(1)
        case Right:
                t.Right(1)
        case Stay:
                t.Stay()
        }
}

const minSz = 16

func (t *Tape) Left(n int) {
        t.pos -= n
        if t.pos < 0 {
                // Extend left
                var sz int
                for sz = minSz; cap(t.data[t.left:])-t.pos >= sz; sz <<= 1 {
                }
                newd := make([]Symbol, sz)
                newl := len(newd) - cap(t.data[t.left:])
                n := copy(newd[newl:], t.data[t.left:])
                t.data = newd[:newl+n]
                t.pos += newl - t.left
                t.left = newl
        }
        if t.pos < t.left {
                if t.blank != 0 {
                        for i := t.pos; i < t.left; i++ {
                                t.data[i] = t.blank
                        }
                }
                t.left = t.pos
        }
}

func (t *Tape) Right(n int) {
        t.pos += n
        if t.pos >= cap(t.data) {
                // Extend right
                var sz int
                for sz = minSz; t.pos >= sz; sz <<= 1 {
                }
                newd := make([]Symbol, sz)
                n := copy(newd[t.left:], t.data[t.left:])
                t.data = newd[:t.left+n]
        }
        if i := len(t.data); t.pos >= i {
                t.data = t.data[:t.pos+1]
                if t.blank != 0 {
                        for ; i < len(t.data); i++ {
                                t.data[i] = t.blank
                        }
                }
        }
}

type State string

type Rule struct {
        State
        Symbol
        Write Symbol
        Motion
        Next State
}

func (i *Rule) key() key       { return key{i.State, i.Symbol} }
func (i *Rule) action() action { return action{i.Write, i.Motion, i.Next} }

type key struct {
        State
        Symbol
}

type action struct {
        write Symbol
        Motion
        next State
}

type Machine struct {
        tape         *Tape
        start, state State
        transition   map[key]action
        l            func(string, ...interface{}) // XXX
}

func NewMachine(rules []Rule) *Machine {
        m := &Machine{transition: make(map[key]action, len(rules))}
        if len(rules) > 0 {
                m.start = rules[0].State
        }
        for _, r := range rules {
                m.transition[r.key()] = r.action()
        }
        return m
}

func (m *Machine) Run(input *Tape) (int, *Tape) {
        m.tape = input.Dup()
        m.state = m.start
        for cnt := 0; ; cnt++ {
                if m.l != nil {
                        m.l("%3d %4s: %v\n", cnt, m.state, m.tape)
                }
                sym := m.tape.Read()
                act, ok := m.transition[key{m.state, sym}]
                if !ok {
                        return cnt, m.tape
                }
                m.tape.Write(act.write)
                m.tape.Move(act.Motion)
                m.state = act.next
        }
}
```

An example program using the above package:

```go
package main

import (
        ".." // XXX path to above turing package
        "fmt"
)

func main() {
        var incrementer = turing.NewMachine([]turing.Rule{
                {"q0", '1', '1', turing.Right, "q0"},
                {"q0", 'B', '1', turing.Stay, "qf"},
        })
        input := turing.NewTape('B', 0, []turing.Symbol{'1', '1', '1'})
        cnt, output := incrementer.Run(input)
        fmt.Println("Turing machine halts after", cnt, "operations")
        fmt.Println("Resulting tape:", output)

        var beaver = turing.NewMachine([]turing.Rule{
                {"a", '0', '1', turing.Right, "b"},
                {"a", '1', '1', turing.Left, "c"},
                {"b", '0', '1', turing.Left, "a"},
                {"b", '1', '1', turing.Right, "b"},
                {"c", '0', '1', turing.Left, "b"},
                {"c", '1', '1', turing.Stay, "halt"},
        })
        cnt, output = beaver.Run(turing.NewTape('0', 0, nil))
        fmt.Println("Turing machine halts after", cnt, "operations")
        fmt.Println("Resulting tape:", output)

        beaver = turing.NewMachine([]turing.Rule{
                {"A", '0', '1', turing.Right, "B"},
                {"A", '1', '1', turing.Left, "C"},
                {"B", '0', '1', turing.Right, "C"},
                {"B", '1', '1', turing.Right, "B"},
                {"C", '0', '1', turing.Right, "D"},
                {"C", '1', '0', turing.Left, "E"},
                {"D", '0', '1', turing.Left, "A"},
                {"D", '1', '1', turing.Left, "D"},
                {"E", '0', '1', turing.Stay, "H"},
                {"E", '1', '0', turing.Left, "A"},
        })
        cnt, output = beaver.Run(turing.NewTape('0', 0, nil))
        fmt.Println("Turing machine halts after", cnt, "operations")
        fmt.Println("Resulting tape has", len(output.Data()), "cells")

        var sort = turing.NewMachine([]turing.Rule{
                // Moving right, first b→B;s1
                {"s0", 'a', 'a', turing.Right, "s0"},
                {"s0", 'b', 'B', turing.Right, "s1"},
                {"s0", ' ', ' ', turing.Left, "se"},
                // Conintue right to end of tape → s2
                {"s1", 'a', 'a', turing.Right, "s1"},
                {"s1", 'b', 'b', turing.Right, "s1"},
                {"s1", ' ', ' ', turing.Left, "s2"},
                // Continue left over b.  a→b;s3, B→b;se
                {"s2", 'a', 'b', turing.Left, "s3"},
                {"s2", 'b', 'b', turing.Left, "s2"},
                {"s2", 'B', 'b', turing.Left, "se"},
                // Continue left until B→a;s0
                {"s3", 'a', 'a', turing.Left, "s3"},
                {"s3", 'b', 'b', turing.Left, "s3"},
                {"s3", 'B', 'a', turing.Right, "s0"},
                // Move to tape start → halt
                {"se", 'a', 'a', turing.Left, "se"},
                {"se", ' ', ' ', turing.Right, "see"},
        })
        input = turing.NewTape(' ', 0, []turing.Symbol("abbabbabababab"))
        cnt, output = sort.Run(input)
        fmt.Println("Turing machine halts after", cnt, "operations")
        fmt.Println("Resulting tape:", output)
}
```

{{out}}

```txt
Turing machine halts after 4 operations
Resulting tape:  1  1  1 [1]
Turing machine halts after 13 operations
Resulting tape:  1  1  1 [1] 1  1
Turing machine halts after 47176870 operations
Resulting tape has 12289 cells
Turing machine halts after 118 operations
Resulting tape:    [a] a  a  a  a  a  b  b  b  b  b  b  b  b
```



## Haskell


### Simple Universal Turing Machine

In this program the tape is infinite, and the machines rules are coded in Haskell as a function from state and value to action, using Haskell as a DSL.


```haskell
-- Some elementary types for Turing Machine
data Move = MLeft | MRight | Stay deriving (Show, Eq)
data Tape a = Tape a [a] [a]
data Action state val = Action val Move state deriving (Show)

instance (Show a) => Show (Tape a) where
  show (Tape x lts rts) = concat $ left ++ [hd] ++ right
                          where hd = "[" ++ show x ++ "]"
                                left = map show $ reverse $ take 10 lts
                                right = map show $ take 10 rts

-- new tape
tape blank lts rts | null rts = Tape blank left blanks
                   | otherwise = Tape (head rts) left right
                   where blanks = repeat blank
                         left = reverse lts ++ blanks
                         right = tail rts ++ blanks

-- Turing Machine
step rules (state, Tape x (lh:lts) (rh:rts)) = (state', tape')
     where  Action x' dir state' = rules state x
            tape' = move dir
            move Stay = Tape x' (lh:lts) (rh:rts)
            move MLeft = Tape lh lts (x':rh:rts)
            move MRight = Tape rh (x':lh:lts) rts

runUTM rules stop start tape = steps ++ [final]
      where (steps, final:_) = break ((== stop) . fst) $ iterate (step rules) (start, tape)

```



### =Increment machine=


```haskell
incr "q0" 1 = Action 1 MRight "q0"
incr "q0" 0 = Action 1 Stay "qf"

tape1 = tape 0 [] [1,1, 1]
machine1 = runUTM incr "qf" "q0" tape1

```


The output of the increment machine :

```haskell
*Main> mapM_ print machine1
("q0",0000000000[1]1100000000)
("q0",0000000001[1]1000000000)
("q0",0000000011[1]0000000000)
("q0",0000000111[0]0000000000)
("qf",0000000111[1]0000000000)

```



### =Beaver machine=


```haskell
beaver "a" 0 = Action 1 MRight "b"
beaver "a" 1 = Action 1 MLeft  "c"
beaver "b" 0 = Action 1 MLeft  "a"
beaver "b" 1 = Action 1 MRight "b"
beaver "c" 0 = Action 1 MLeft  "b"
beaver "c" 1 = Action 1 Stay   "halt"

tape2 = tape 0 [] []
machine2 = runUTM beaver "halt" "a" tape2

```



### =Sorting test=


```haskell
sorting "A" 1 = Action 1 MRight "A"
sorting "A" 2 = Action 3 MRight "B"
sorting "A" 0 = Action 0 MLeft  "E"
sorting "B" 1 = Action 1 MRight "B"
sorting "B" 2 = Action 2 MRight "B"
sorting "B" 0 = Action 0 MLeft  "C"
sorting "C" 1 = Action 2 MLeft  "D"
sorting "C" 2 = Action 2 MLeft  "C"
sorting "C" 3 = Action 2 MLeft  "E"
sorting "D" 1 = Action 1 MLeft  "D"
sorting "D" 2 = Action 2 MLeft  "D"
sorting "D" 3 = Action 1 MRight "A"
sorting "E" 1 = Action 1 MLeft  "E"
sorting "E" 0 = Action 0 MRight "STOP"

tape3 = tape 0 [] [2,2,2,1,2,2,1,2,1,2,1,2,1,2]
machine3 = runUTM sorting "STOP" "A" tape3

```



### Using State Monad

A state monad represents the machine. It works with an arbitrary number of symbols and states, but all of them must be of the same type (integer, string...)
Intermediate states can be logged during execution, or they can be discarded. The initial and final states as well as errors are always logged.
Three functions are added so that machines can be written to a file and parsed/run from there. Examples are provided.

```haskell

import Control.Monad.State
import Data.List (intersperse, nub, find)

data TapeMovement = MoveLeft | MoveRight | Stay deriving (Show, Eq)
-- Rule = (state 1, input, output, movement, state 2)
type Rule a = (a, a, a, TapeMovement, a)
-- Execution = (tape position, current machine state, tape)
type Execution a = (Int, a, [a])
type Log a = [Execution a]
type UTM a b = State (Machine a) b

-- can work with data of any type
data Machine a = Machine
    { allStates :: [a] -- not used actually
    , initialState :: a  -- not used actually, initial state in "current"
    , finalStates :: [a]
    , symbols :: [a]     -- not used actually
    , blank :: a
    , noOpSymbol :: a    -- means: don't change input / don't shift tape
    , rules :: [Rule a]
    , current :: Execution a
    , machineLog :: Log a -- stores state changes from last to first
    , machineLogActive :: Bool -- if true, intermediate steps are stored
    , noRuleMsg :: a -- error symbol if no rule matches
    , stopMsg :: a } -- symbol to append to the end result
    deriving (Show)

-- it is not checked whether the input and output symbols are valid
apply :: Eq a => Rule a -> UTM a a
apply (_, _, output, direction, stateUpdate) = do
    m <- get
    let (pos, currentState, tape) = current m
        tapeUpdate = if output == noOpSymbol m
            then tape
            else take pos tape ++ [output] ++ drop (pos + 1) tape
        newTape
            | pos == 0 && direction == MoveLeft = blank m : tapeUpdate
            | succ pos == length tape && direction == MoveRight = tapeUpdate ++ [blank m]
            | otherwise = tapeUpdate
        newPosition = case direction of
            MoveLeft -> if pos == 0 then 0 else pred pos
            MoveRight -> succ pos
            Stay -> pos
        newState = if stateUpdate == noOpSymbol m
            then currentState
            else stateUpdate
    put $! m { current = (newPosition, newState, newTape) }
    return newState

-- rules with no-operation symbols and states must be underneath
-- rules with defined symbols and states
lookupRule :: Eq a => UTM a (Maybe (Rule a))
lookupRule = do
    m <- get
    let (pos, currentState, tape) = current m
        item = tape !! pos
        isValid (e, i, _, _, _) = e == currentState &&
            (i == item || i == noOpSymbol m)
    return $! find isValid (rules m)

msgToLog :: a -> UTM a ()
msgToLog e = do
    m <- get
    let (pos, currentState, tape) = current m
    put $! m { machineLog = (pos, currentState, tape ++ [e]) : machineLog m }

toLog :: UTM a ()
toLog = do
    m <- get
    put $! m { machineLog = current m : machineLog m }

-- execute the machine's program
execute :: Eq a => UTM a ()
execute = do
    toLog -- log the initial state
    loop
    where
        loop = do
            m <- get
            r <- lookupRule -- look for a matching rule
            case r of
                Nothing -> msgToLog (noRuleMsg m)
                Just rule -> do
                    stateUpdate <- apply rule
                    if stateUpdate `elem` finalStates m
                        then msgToLog (stopMsg m)
                        else do
                            when (machineLogActive m) toLog
                            loop

---------------------------
-- convenient functions
---------------------------

-- run execute, format and print the output
runMachine :: Machine String -> IO ()
runMachine m@(Machine { current = (_, _, tape) }) =
    if null tape
        then putStrLn "NO TAPE"
        else case machineLog $ execState execute m of
                [] -> putStrLn "NO OUTPUT"
                xs -> do
                    mapM_ (\(pos, _, output) -> do
                        let formatOutput = concat output
                        putStrLn formatOutput
                        putStrLn (replicate pos ' ' ++ "^")) $ reverse xs
                    putStrLn $ show (length xs) ++ " STEPS. FINAL STATE: " ++
                        let (_, finalState, _) = head xs in show finalState

-- convert a string with format state+space+input+space+output+space+
-- direction+space+new state to a rule
toRule :: String -> Rule String
toRule xs =
    let [a, b, c, d, e] = take 5 $ words xs
        dir = case d of
            "l" -> MoveLeft
            "r" -> MoveRight
            "*" -> Stay
    in  (a, b, c, dir, e)

-- load a text file and parse it to a machine.
-- see comments and examples
-- lines in the file starting with ';' are header lines or comments
-- header and input lines must contain a ':' and after that the content to be parsed
-- so there can be comments between ';' and ':' in those lines
loadMachine :: FilePath -> IO (Machine String)
loadMachine n = do
    f <- readFile n

    let ls = lines f
        -- header: first 4 lines
        ([e1, e2, e3, e4], rest) = splitAt 4 ls
        -- rules and input: rest of the file
        re = map toRule . filter (not . null) $ map (takeWhile (/= ';')) rest
        ei = head . words . tail . snd $ break (== ':') e1
        va = head . words . tail . snd $ break (== ':') e3
        ci = words . intersperse ' ' . tail . snd $ break (== ':') $ last rest

    return Machine
        { rules = re
        , initialState = ei
        , finalStates = words . tail . snd $ break (== ':') e2
        , blank = va
        , noOpSymbol = head . words . tail . snd $ break (== ':') e4
        , allStates = nub $ concatMap (\(a, _, _, _, e) -> [a, e]) re
        , symbols = nub $ concatMap (\(_, b, c, _, _) -> [b, c]) re
        , current = (0, ei, if null ci then [va] else ci)
        -- we assume
        , noRuleMsg = "\tNO RULE." -- error: no matching rule found
        , stopMsg = "\tHALT." -- message: machine reached a final state
        , machineLog = []
        , machineLogActive = True }

```

Examples for machine files:

```txt

; Initial state: q0
; Final states: qf
; Blank symbol: B
; No-op symbol: *

; Simple incrementer

q0 1 1 r q0
q0 B 1 * qf

; Initial tape: 111

```


```txt

; Initial state: a
; Final states: halt
; Blank symbol: 0
; No-op symbol: *

; Three-state busy beaver

a 0 1 r b
a 1 1 l c
b 0 1 l a
b 1 1 r b
c 0 1 l b
c 1 1 * halt

; Initial tape:

```

To run a machine:

    loadMachine "machine1" >>= runMachine
Output (simple incrementer):

```txt

111
^
111
 ^
111
  ^
111B
   ^
1111	HALT.
   ^
5 STEPS. FINAL STATE: "qf"

```

Output (three-state busy beaver):

```txt

0
^
10
 ^
11
^
011
^
0111
^
01111
^
11111
 ^
11111
  ^
11111
   ^
11111
    ^
111110
     ^
111111
    ^
111111
   ^
111111	HALT.
   ^
14 STEPS. FINAL STATE: "halt"

```


=={{header|Icon}} and {{header|Unicon}}==

The following works in both languages.  The state machine input format differs slightly from the
example given above.  Various options exist for tracing the actions of the machine.
This particular UTM halts when entering a final state or when a motion of 'halt' is acted on.


```unicon
record TM(start,final,delta,tape,blank)
record delta(old_state, input_symbol, new_state, output_symbol, direction)

global start_tape
global show_count, full_display, trace_list  # trace flags

procedure main(args)
    init(args)
    runTuringMachine(get_tm())
end

procedure init(args)
    trace_list := ":"
    while arg := get(args) do {
        if arg == "-f" then full_display := "yes"
        else if match("-t",arg) then trace_list ||:= arg[3:0]||":"
        else show_count := integer(arg)
        }
end

procedure get_tm()
    D := table()

    writes("What is the start state? ")
    start := !&input
    writes("What are the final states (colon separated)? ")
    finals := !&input
    (finals||":") ? every insert(fStates := set(), 1(tab(upto(':')),move(1)))
    writes("What is the tape blank symbol?")
    blank := !&input

    write("Enter the delta mappings, using the following format:")
    write("\tenter delta(curState,tapeSymbol) = (newState,newSymbol,direct) as")
    write("\t   curState:tapeSymbol:newState:newSymbol:direct");
    write("\t\twhere direct is left, right, stay, or halt")
    write("End with a blank line.")
    write("")
    every line := !&input do {
        if *line = 0 then break
        line ?
             if (os := tab(upto(':')), move(1), ic := tab(upto(':')), move(1),
                 ns := tab(upto(':')), move(1), oc := tab(upto(':')), move(1),
                 d  := map(tab(0))) then D[os||":"||ic] := delta(os,ic,ns,oc,d)
             else write(line, " is in bad form, correct it")
        }
    if /start_tape then {
        write("Enter the input tape")
        start_tape := !&input
        }
    return TM(start,fStates,D,start_tape,blank)
end

procedure runTuringMachine(tm)
    trans := tm.delta
    rightside := tm.tape
    if /rightside | (*rightside = 0) then rightside := tm.blank
    leftside := ""

    cur_state := tm.start
    write("Machine starts in ",cur_state," with tape:")
    show_tape(tm,leftside,rightside)
    while mapping := \trans[cur_state||":"||rightside[1]] do {
        rightside[1] := mapping.output_symbol
        case mapping.direction of {
            "left" : {
                 if *leftside = 0 then leftside := tm.blank
                 rightside := leftside[-1] || rightside
                 leftside[-1] := ""
                 }
            "right" : {
                 leftside ||:= rightside[1]
                 rightside[1] := ""
                 if *rightside = 0 then rightside := tm.blank
                 }
            "halt" : break
            }
        cur_state := mapping.new_state
        if member(tm.final,cur_state) then break
        trace(tm,cur_state,leftside,rightside)
        }
    write()
    write("Machine halts in ",cur_state," with tape:")
    show_tape(tm,leftside,rightside)
end

procedure trace(tm,cs,ls,rs)
    static count, last_state
    initial {
       count := 0
       last_state := ""
       }

    count +:= 1
    if \show_count & (count % show_count = 0) then show_tape(tm,ls,rs)
    if find(":"||cs||":",trace_list) & (last_state ~== cs) then {
        writes("\tnow in state: ",cs," ")
        if \full_display then show_delta(tm.delta[cs||":"||rs[1]])
        else write()
        }
    last_state := cs
    return
end

procedure show_delta(m)
    if /m then write("NO MOVE!")
    else {
        writes("\tnext move is ")
        writes("delta(",m.old_state,",",m.input_symbol,") ::= ")
        write("(",m.new_state,",",m.output_symbol,",",m.direction,")")
        }
end

procedure show_tape(tm,l,r)
    l := reverse(trim(reverse(l),tm.blank))
    r := trim(r,tm.blank)
    write(l,r)
    write(repl(" ",*l),"^")
end
```


First sample machine, with tape changes on each transition traced:


```txt

->turing 1
What is the start state? q0
What are the final states (colon separated)? qf
What is the tape blank symbol?B
Enter the delta mappings, using the following format:
        enter delta(curState,tapeSymbol) = (newState,newSymbol,direct) as
           curState:tapeSymbol:newState:newSymbol:direct
                where direct is left, right, stay, or halt
End with a blank line.

q0:1:q0:1:right
q0:B:qf:1:stay

Enter the input tape
111
Machine starts in q0 with tape:
111
^
111
 ^
111
  ^
111
   ^

Machine halts in qf with tape:
1111
   ^
->

```


Second sample machine, with all tracing off (only first and last tapes are displayed):


```txt

->turing
What is the start state? a
What are the final states (colon separated)? halt
What is the tape blank symbol?0
Enter the delta mappings, using the following format:
        enter delta(curState,tapeSymbol) = (newState,newSymbol,direct) as
           curState:tapeSymbol:newState:newSymbol:direct
                where direct is left, right, stay, or halt
End with a blank line.

a:0:b:1:right
a:1:c:1:left
b:0:a:1:left
b:1:b:1:right
c:0:b:1:left
c:1:halt:1:stay

Enter the input tape

Machine starts in a with tape:

^

Machine halts in halt with tape:
111111
   ^
->

```



## J

Source for this task was slightly adapted from http://www.2bestsystems.com/j/J_Conference_2012.  All the information for the Turing machines is represented by integers, the halting state is set as _1 (minus one), and head movements are mapped as (left, stay, right) ➜ (_1, 0, 1).    A Turing machine is executed until a halt state is issued or a trivial infinite regress in the form of a single changeless cycle is detected.  The transition table entry format is similar to the one in
http://drb9.drb.insel.de/~heiner/BB/simAB3Y_SB.html.
===The universal (stateless point-free) Turing machine===
The universal Turing machine is defined in terms of fixed tacit (stateless point-free) code, showing that this dialect of J is Turing complete.

```j
   ". noun define -. CRLF     NB. Fixed tacit universal Turing machine code...

utm=.
(((":@:(]&:>)@:(6&({::)) ,: (":@] 9&({::))) ,. ':'"_) ,. 2&({::) >@:(((48 + ]
) { a."_)@[ ; (] $ ' '"_) , '^'"_) 3&({::))@:([ (0 0 $ 1!:2&2)@:('A changeles
s cycle was detected!'"_)^:(-.@:(_1"_ = 1&({::))))@:((((3&({::) + 8&({::)) ;
1 + 9&({::)) 3 9} ])@:(<@:((0 (0 {:: ])`(<@:(1 {:: ]))`(2 {:: ])} ])@:(7 3 2&
{)) 2} ])@:(<"0@:(6&({::) (<@[ { ]) 0&({::)) 7 8 1} ])@:([ (0 0 $ 1!:2&2)@:((
(":@:(]&:>)@:(6&({::)) ,: (":@] 9&({::))) ,. ':'"_) ,. 2&({::) >@:(((48 + ])
{ a."_)@[ ; (] $ ' '"_) , '^'"_) 3&({::))^:(0 = 4&({::) | 9&({::)))@:(<@:(1&(
{::) ; 3&({::) { 2&({::)) 6} ])@:(<@:(3&({::) + _1 = 3&({::)) 3} ])@:(<@:(((_
1 = 3&({::)) {:: 5&({::)) , 2&({::) , (3&({::) = #@:(2&({::))) {:: 5&({::)) 2
} ])^:(-.@:(_1"_ = 1&({::)))^:_)@:((0 ; (({. , ({: % 3:) , 3:)@:$ $ ,)@:(}."1
)@:(".;._2)@:(0&({::))) 9 0} ])@:(<@:('' ; 0"_) 5} ])@:(,&(;:',,,,,'))@:(,~)

)

```



### The incrementer machine


```j
   Noun=. ".@('(0 : 0)'"_)

   NB. Simple Incrementer...
   NB.        0         1     Tape Symbol Scan
   NB. S   p  m  g   p  m  g  (p,m,g) → (print,move,goto)
   QS=. (Noun _) ; 0          NB. Reading the transition table and setting the initial state
       0   1  0 _1   1  1  0
)
   TPF=. 1 1 1 ; 0 ; 1        NB. Setting the tape, its pointer and the display frequency

   TPF utm QS                 NB. Running the Turing machine...
0 1:111
0  :^
0 1:111
1  : ^
0 1:111
2  :  ^
0 0:1110
3  :   ^
0 0:1111
4  :   ^
```


=== The three-state busy beaver machine===

```j
   NB. Three-state busy beaver..
   NB.        0         1     Tape Symbol Scan
   NB. S   p  m  g   p  m  g  (p,m,g) → (print,move,goto)
   QS=. (Noun _) ; 0          NB. Reading the transition table and setting the initial state
       0   1  1  1   1 _1  2
       1   1 _1  0   1  1  1
       2   1 _1  1   1  0 _1
)
   TPF=. 0 ; 0 ; 1            NB. Setting the tape, its pointer and the display frequency

   TPF utm QS                 NB. Running the Turing machine...
0 0:0
0  :^
1 0:10
1  : ^
0 1:11
2  :^
2 0:011
3  :^
1 0:0111
4  :^
0 0:01111
5  :^
1 1:11111
6  : ^
1 1:11111
7  :  ^
1 1:11111
8  :   ^
1 1:11111
9  :    ^
1 0:111110
10 :     ^
0 1:111111
11 :    ^
2 1:111111
12 :   ^
2 1:111111
13 :   ^
```


=== The probable 5-state, 2-symbol busy beaver machine===

```j
   NB. Probable 5-state, 2-symbol busy beaver...
   NB.        0         1      Tape Symbol Scan
   NB. S   p  m  g   p  m  g  (p,m,g) → (print,move,goto)
   QS=. (Noun _) ; 0          NB. Reading the transition table and setting the state
       0   1  1  1   1 _1  2
       1   1  1  2   1  1  1
       2   1  1  3   0 _1  4
       3   1 _1  0   1 _1  3
       4   1  1 _1   0 _1  0
)
   TPF=. 0 ; 0 ; _            NB. Setting the tape, its pointer and the display frequency

   TPF utm QS                 NB. Running the Turing machine...
0 0:0
0 :^
4 0     :101001001001001001001001001001001001001001001001001001001001001001001001001001001001001001001001001001...
47176870: ^
```



###  The sorting stress test machine


```j
   NB. Sorting stress test...
   NB.        0         1         2         3     Tape Symbol Scan
   NB. S   p  m  g   p  m  g   p  m  g   p  m  g  (p,m,g) ➜ (print,move,goto)
   QS=. (Noun _) ; 0          NB. Reading the transition table and setting the initial state
       0   0 _1  4   1  1  0   3  1  1   _  _  _
       1   0 _1  2   1  1  1   2  1  1   _  _  _
       2   _  _  _   2 _1  3   2 _1  2   2 _1  4
       3   _  _  _   1 _1  3   2 _1  3   1  1  0
       4   0  1 _1   1 _1  4   _  _  _   _  _  _
)
   TPF=. 1 2 2 1 2 2 1 2 1 2 1 2 1 2 ; 0 ; 50   NB. Setting the tape, its pointer and the display frequency

   TPF utm QS                 NB. Running the Turing machine...
0 1:12212212121212
0  :^
3 2:113122121222220
50 :    ^
1 2:111111322222220
100:            ^
4 0:0111111222222220
118: ^
```



###  The structured derivation of the universal Turing machine

The fixed tacit code was produced by means of an unorthodox tacit toolkit; however, the verb produced is orthodox (i.e., compliant with the language specifications):


```j
NB. Structured derivation of the universal Turing machine...

NB.--------------------------------------------------------------------------------------
NB. Quick and dirty tacit toolkit...

o=. @:
c=."_

ver=. (0:`)([:^:)

d=. (fix=. (;:'f.')ver) (train=.(;:'`:')ver&6) (an=. <@:((,'0') (,&<) ]))
ver=. (an f. o fix'ver')ver o an f.
z=. ((an'')`($ ,)`) (`:6)
d=. (a0=. `'') (a1=. (@:[) ((<'&')`) (`:6)) (a2=. (`(<(":0);_)) (`:6))
av=. ((an o fix'a0')`)  (`(an o fix'a1')) (`(an o fix'a2') ) (`:6)

Fetch=. (ver o train ;:'&{::')&.> o i. f.av
tie=. ver o train ;:'`'

indices=. (, $~ 1 -.~ $) o (train"0 o ((1 -: L.)S:1 # <S:1) o (tie&'') o fix :: ] @:[)
f=. ((ver o train ;:'&{')) o indices o train f.av

'A B'=. 2 Fetch
head=. (;:'<@:') {.~ 2 * 1 = # o [
h=. train o (indices o train o (A f) (head , (B f) o ] , < o an o [  , (;:'}]')c) ]) f.av

DropIfNB=. < o ('('"_ , ] , ')'"_) o ((}: ^: ('NB.' -: 3&{. o > o {:)) &. ;:)
pipe=. ([ , ' o ' , ])&:>/ o |.

is=. ". o (, o ": o > , '=. ' , pipe o (DropIfNB;._2) o ". o ('0 ( : 0)'c)) f.av

NB.--------------------------------------------------------------------------------------

NB. Producing the main (dyadic) verb utm...

Note 0
NB. X (boxed list)...
  Q     - Instruction table
  S     - Turing machine initial state

NB. Y (boxed list)...
  T     - Data tape
  P     - Head position pointer
  F     - Display frequency

NB. Local...
  B     - Blank defaults
  M     - State and tape symbol read
  PRINT - Printing symbol
  MOVE  - Tape head moving instruction
  C     - Step Counter
)

'Q S T P F B M PRINT MOVE C'=. 10 Fetch  NB. Fetching 10 Boxes

DisplayTape=. > o (((48 + ]) { a.c)@[ ; ((] $ ' 'c) , '^'c))
display=. ((((": o (]&:>) o M) ,: (":@] C)) ,. ':'c ) ,. (T DisplayTape P))
  NB. Displaying state, symbol, tape / step and pointer

amend=. 0 (0 {:: ])`(<@:(1 {:: ]))`(2 {:: ])} ]

NB. execute (monadic verb)...

FillLeft=.  (_1 = P    ) {:: B        NB. Expanding and filling the tape
FillRight=. ( P = # o T) {:: B        NB. with 0's (if necessary)
ia=. <@[ { ]                          NB. Selecting by the indices of an array

execute is
  T`(FillLeft , T , FillRight)h       NB. Adjusting the tape
  P`(P + _1 = P)                 h    NB. and the pointer (if necessary)
  M`(S ; P { T)                  h    NB. Updating the state and reading the tape symbol
  [ (smoutput o display)^:(0 = F | C) NB. Displaying intermediate cycles
  (PRINT MOVE S)`(<"0 o (M ia Q))h    NB. Performing the printing, moving and state actions
  T`(amend o ((PRINT P T)f))     h    NB. Printing symbol on tape at the pointer position
  (P C)`((P + MOVE) ; 1 + C)     h    NB. Updating the pointer and the counter
)

cc=. 'A changeless cycle was detected!'c
halt=. _1 c = S                        NB. Halting when the current state is _1
rt=. ((({. , ({: % 3:) , 3:) o $) $ ,) o (}."1) o (". ;. _2)
  NB. Reshaping the transition table as a 3D array (state,symbol,action)

utm is  NB. Universal Turing Machine (dyadic verb)
  ,~                                  NB. Appending the arguments in reverse order
  ,&(;:5$',')                         NB. Appending 5 local boxes (B M PRINT MOVE C)
  B`('' ; 0 c)      h                 NB. Setting empty blank defaults as 0
  (C Q)`(0 ; rt o Q)h                 NB. Setting the counter and the transition table
  execute^:(-. o halt)^:_             NB. Executing until a halt instruction is issued
  [ smoutput o cc ^: (-. o halt)      NB. or a changeless single cycle is detected
  display                             NB. Displaying (returning) the final status
)

utm=. utm f.   NB. Fixing the universal Turing machine code

NB. The simulation code is produced by  77 (-@:[ ]\ 5!:5@<@:]) 'utm'
```



## Java

{{works with|Java|5+}}

This is an implementation of the universal Turing machine in plain Java using standard libraries only. As generics are used, Java 5 is required. The examples (incrementer and busy beaver) are implemented directly in the main method and executed sequentially; as an additional third example, a sorting algorithm is implemented and executed in the end of the main method. During execution the complete tape and the current active transition are printed out in every step. The state names and tape symbols may contain several characters, so arbitrary strings such as "q1", "q2", ... can be valid state names or tape symbols. The machine is deterministic as the transitions are stored in a HashMap which uses state / tape symbol pairs as keys. This is self-coded, not a standard implementation, so there is no guarantee of correctness.


```Java5
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.ListIterator;
import java.util.List;
import java.util.Set;
import java.util.Map;

public class UTM {
    private List<String> tape;
    private String blankSymbol;
    private ListIterator<String> head;
    private Map<StateTapeSymbolPair, Transition> transitions = new HashMap<StateTapeSymbolPair, Transition>();
    private Set<String> terminalStates;
    private String initialState;

    public UTM(Set<Transition> transitions, Set<String> terminalStates, String initialState, String blankSymbol) {
        this.blankSymbol = blankSymbol;
        for (Transition t : transitions) {
            this.transitions.put(t.from, t);
        }
        this.terminalStates = terminalStates;
        this.initialState = initialState;
    }

    public static class StateTapeSymbolPair {
        private String state;
        private String tapeSymbol;

        public StateTapeSymbolPair(String state, String tapeSymbol) {
            this.state = state;
            this.tapeSymbol = tapeSymbol;
        }

        // These methods can be auto-generated by Eclipse.
        @Override
        public int hashCode() {
            final int prime = 31;
            int result = 1;
            result = prime * result
                    + ((state == null) ? 0 : state.hashCode());
            result = prime
                    * result
                    + ((tapeSymbol == null) ? 0 : tapeSymbol
                            .hashCode());
            return result;
        }

        // These methods can be auto-generated by Eclipse.
        @Override
        public boolean equals(Object obj) {
            if (this == obj)
                return true;
            if (obj == null)
                return false;
            if (getClass() != obj.getClass())
                return false;
            StateTapeSymbolPair other = (StateTapeSymbolPair) obj;
            if (state == null) {
                if (other.state != null)
                    return false;
            } else if (!state.equals(other.state))
                return false;
            if (tapeSymbol == null) {
                if (other.tapeSymbol != null)
                    return false;
            } else if (!tapeSymbol.equals(other.tapeSymbol))
                return false;
            return true;
        }

        @Override
        public String toString() {
            return "(" + state + "," + tapeSymbol + ")";
        }
    }

    public static class Transition {
        private StateTapeSymbolPair from;
        private StateTapeSymbolPair to;
        private int direction; // -1 left, 0 neutral, 1 right.

        public Transition(StateTapeSymbolPair from, StateTapeSymbolPair to, int direction) {
             this.from = from;
            this.to = to;
            this.direction = direction;
        }

        @Override
        public String toString() {
            return from + "=>" + to + "/" + direction;
        }
    }

    public void initializeTape(List<String> input) { // Arbitrary Strings as symbols.
        tape = input;
    }

    public void initializeTape(String input) { // Uses single characters as symbols.
        tape = new LinkedList<String>();
        for (int i = 0; i < input.length(); i++) {
            tape.add(input.charAt(i) + "");
        }
    }

    public List<String> runTM() { // Returns null if not in terminal state.
        if (tape.size() == 0) {
            tape.add(blankSymbol);
        }

        head = tape.listIterator();
        head.next();
        head.previous();

        StateTapeSymbolPair tsp = new StateTapeSymbolPair(initialState, tape.get(0));

        while (transitions.containsKey(tsp)) { // While a matching transition exists.
            System.out.println(this + " --- " + transitions.get(tsp));
            Transition trans = transitions.get(tsp);
            head.set(trans.to.tapeSymbol); // Write tape symbol.
            tsp.state = trans.to.state; // Change state.
            if (trans.direction == -1) { // Go left.
                if (!head.hasPrevious()) {
                    head.add(blankSymbol); // Extend tape.
                }
                tsp.tapeSymbol = head.previous(); // Memorize tape symbol.
            } else if (trans.direction == 1) { // Go right.
                head.next();
                if (!head.hasNext()) {
                    head.add(blankSymbol); // Extend tape.
                    head.previous();
                }
                tsp.tapeSymbol = head.next(); // Memorize tape symbol.
                head.previous();
            } else {
                tsp.tapeSymbol = trans.to.tapeSymbol;
            }
        }

        System.out.println(this + " --- " + tsp);

        if (terminalStates.contains(tsp.state)) {
            return tape;
        } else {
            return null;
        }
    }

    @Override
    public String toString() {
        try {
        	int headPos = head.previousIndex();
            String s = "[ ";

            for (int i = 0; i <= headPos; i++) {
                s += tape.get(i) + " ";
            }

            s += "[H] ";

            for (int i = headPos + 1; i < tape.size(); i++) {
                s += tape.get(i) + " ";
            }

            return s + "]";
        } catch (Exception e) {
            return "";
        }
    }

    public static void main(String[] args) {
        // Simple incrementer.
        String init = "q0";
        String blank = "b";

        Set<String> term = new HashSet<String>();
        term.add("qf");

        Set<Transition> trans = new HashSet<Transition>();

        trans.add(new Transition(new StateTapeSymbolPair("q0", "1"), new StateTapeSymbolPair("q0", "1"), 1));
        trans.add(new Transition(new StateTapeSymbolPair("q0", "b"), new StateTapeSymbolPair("qf", "1"), 0));

        UTM machine = new UTM(trans, term, init, blank);
        machine.initializeTape("111");
        System.out.println("Output (si): " + machine.runTM() + "\n");

        // Busy Beaver (overwrite variables from above).
        init = "a";

        term.clear();
        term.add("halt");

        blank = "0";

        trans.clear();

        // Change state from "a" to "b" if "0" is read on tape, write "1" and go to the right. (-1 left, 0 nothing, 1 right.)
        trans.add(new Transition(new StateTapeSymbolPair("a", "0"), new StateTapeSymbolPair("b", "1"), 1));
        trans.add(new Transition(new StateTapeSymbolPair("a", "1"), new StateTapeSymbolPair("c", "1"), -1));
        trans.add(new Transition(new StateTapeSymbolPair("b", "0"), new StateTapeSymbolPair("a", "1"), -1));
        trans.add(new Transition(new StateTapeSymbolPair("b", "1"), new StateTapeSymbolPair("b", "1"), 1));
        trans.add(new Transition(new StateTapeSymbolPair("c", "0"), new StateTapeSymbolPair("b", "1"), -1));
        trans.add(new Transition(new StateTapeSymbolPair("c", "1"), new StateTapeSymbolPair("halt", "1"), 0));

        machine = new UTM(trans, term, init, blank);
        machine.initializeTape("");
        System.out.println("Output (bb): " + machine.runTM());

        // Sorting test (overwrite variables from above).
        init = "s0";
        blank = "*";

        term = new HashSet<String>();
        term.add("see");

        trans = new HashSet<Transition>();

        trans.add(new Transition(new StateTapeSymbolPair("s0", "a"), new StateTapeSymbolPair("s0", "a"), 1));
        trans.add(new Transition(new StateTapeSymbolPair("s0", "b"), new StateTapeSymbolPair("s1", "B"), 1));
        trans.add(new Transition(new StateTapeSymbolPair("s0", "*"), new StateTapeSymbolPair("se", "*"), -1));
        trans.add(new Transition(new StateTapeSymbolPair("s1", "a"), new StateTapeSymbolPair("s1", "a"), 1));
        trans.add(new Transition(new StateTapeSymbolPair("s1", "b"), new StateTapeSymbolPair("s1", "b"), 1));
        trans.add(new Transition(new StateTapeSymbolPair("s1", "*"), new StateTapeSymbolPair("s2", "*"), -1));
        trans.add(new Transition(new StateTapeSymbolPair("s2", "a"), new StateTapeSymbolPair("s3", "b"), -1));
        trans.add(new Transition(new StateTapeSymbolPair("s2", "b"), new StateTapeSymbolPair("s2", "b"), -1));
        trans.add(new Transition(new StateTapeSymbolPair("s2", "B"), new StateTapeSymbolPair("se", "b"), -1));
        trans.add(new Transition(new StateTapeSymbolPair("s3", "a"), new StateTapeSymbolPair("s3", "a"), -1));
        trans.add(new Transition(new StateTapeSymbolPair("s3", "b"), new StateTapeSymbolPair("s3", "b"), -1));
        trans.add(new Transition(new StateTapeSymbolPair("s3", "B"), new StateTapeSymbolPair("s0", "a"), 1));
        trans.add(new Transition(new StateTapeSymbolPair("se", "a"), new StateTapeSymbolPair("se", "a"), -1));
        trans.add(new Transition(new StateTapeSymbolPair("se", "*"), new StateTapeSymbolPair("see", "*"), 1));

        machine = new UTM(trans, term, init, blank);
        machine.initializeTape("babbababaa");
        System.out.println("Output (sort): " + machine.runTM() + "\n");
    }
}
```


{{out}} -- [H] denotes the head; its position on the tape is over the symbol printed right from it.


```txt
[ [H] 1 1 1 ] --- (q0,1)=>(q0,1)/1
[ 1 [H] 1 1 ] --- (q0,1)=>(q0,1)/1
[ 1 1 [H] 1 ] --- (q0,1)=>(q0,1)/1
[ 1 1 1 [H] b ] --- (q0,b)=>(qf,1)/0
[ 1 1 1 [H] 1 ] --- (qf,1)
Output (si): [1, 1, 1, 1]

[ [H] 0 ] --- (a,0)=>(b,1)/1
[ 1 [H] 0 ] --- (b,0)=>(a,1)/-1
[ [H] 1 1 ] --- (a,1)=>(c,1)/-1
[ [H] 0 1 1 ] --- (c,0)=>(b,1)/-1
[ [H] 0 1 1 1 ] --- (b,0)=>(a,1)/-1
[ [H] 0 1 1 1 1 ] --- (a,0)=>(b,1)/1
[ 1 [H] 1 1 1 1 ] --- (b,1)=>(b,1)/1
[ 1 1 [H] 1 1 1 ] --- (b,1)=>(b,1)/1
[ 1 1 1 [H] 1 1 ] --- (b,1)=>(b,1)/1
[ 1 1 1 1 [H] 1 ] --- (b,1)=>(b,1)/1
[ 1 1 1 1 1 [H] 0 ] --- (b,0)=>(a,1)/-1
[ 1 1 1 1 [H] 1 1 ] --- (a,1)=>(c,1)/-1
[ 1 1 1 [H] 1 1 1 ] --- (c,1)=>(halt,1)/0
[ 1 1 1 [H] 1 1 1 ] --- (halt,1)
Output (bb): [1, 1, 1, 1, 1, 1]

[ [H] b a b b a b a b a a ] --- (s0,b)=>(s1,B)/1
[ B [H] a b b a b a b a a ] --- (s1,a)=>(s1,a)/1
[ B a [H] b b a b a b a a ] --- (s1,b)=>(s1,b)/1
[ B a b [H] b a b a b a a ] --- (s1,b)=>(s1,b)/1
[ B a b b [H] a b a b a a ] --- (s1,a)=>(s1,a)/1
[ B a b b a [H] b a b a a ] --- (s1,b)=>(s1,b)/1
[ B a b b a b [H] a b a a ] --- (s1,a)=>(s1,a)/1
[ B a b b a b a [H] b a a ] --- (s1,b)=>(s1,b)/1
[ B a b b a b a b [H] a a ] --- (s1,a)=>(s1,a)/1
[ B a b b a b a b a [H] a ] --- (s1,a)=>(s1,a)/1
[ B a b b a b a b a a [H] * ] --- (s1,*)=>(s2,*)/-1
[ B a b b a b a b a [H] a * ] --- (s2,a)=>(s3,b)/-1
[ B a b b a b a b [H] a b * ] --- (s3,a)=>(s3,a)/-1
[ B a b b a b a [H] b a b * ] --- (s3,b)=>(s3,b)/-1
[ B a b b a b [H] a b a b * ] --- (s3,a)=>(s3,a)/-1
[ B a b b a [H] b a b a b * ] --- (s3,b)=>(s3,b)/-1
[ B a b b [H] a b a b a b * ] --- (s3,a)=>(s3,a)/-1
[ B a b [H] b a b a b a b * ] --- (s3,b)=>(s3,b)/-1
[ B a [H] b b a b a b a b * ] --- (s3,b)=>(s3,b)/-1
[ B [H] a b b a b a b a b * ] --- (s3,a)=>(s3,a)/-1
[ [H] B a b b a b a b a b * ] --- (s3,B)=>(s0,a)/1
[ a [H] a b b a b a b a b * ] --- (s0,a)=>(s0,a)/1
[ a a [H] b b a b a b a b * ] --- (s0,b)=>(s1,B)/1
[ a a B [H] b a b a b a b * ] --- (s1,b)=>(s1,b)/1
[ a a B b [H] a b a b a b * ] --- (s1,a)=>(s1,a)/1
[ a a B b a [H] b a b a b * ] --- (s1,b)=>(s1,b)/1
[ a a B b a b [H] a b a b * ] --- (s1,a)=>(s1,a)/1
[ a a B b a b a [H] b a b * ] --- (s1,b)=>(s1,b)/1
[ a a B b a b a b [H] a b * ] --- (s1,a)=>(s1,a)/1
[ a a B b a b a b a [H] b * ] --- (s1,b)=>(s1,b)/1
[ a a B b a b a b a b [H] * ] --- (s1,*)=>(s2,*)/-1
[ a a B b a b a b a [H] b * ] --- (s2,b)=>(s2,b)/-1
[ a a B b a b a b [H] a b * ] --- (s2,a)=>(s3,b)/-1
[ a a B b a b a [H] b b b * ] --- (s3,b)=>(s3,b)/-1
[ a a B b a b [H] a b b b * ] --- (s3,a)=>(s3,a)/-1
[ a a B b a [H] b a b b b * ] --- (s3,b)=>(s3,b)/-1
[ a a B b [H] a b a b b b * ] --- (s3,a)=>(s3,a)/-1
[ a a B [H] b a b a b b b * ] --- (s3,b)=>(s3,b)/-1
[ a a [H] B b a b a b b b * ] --- (s3,B)=>(s0,a)/1
[ a a a [H] b a b a b b b * ] --- (s0,b)=>(s1,B)/1
[ a a a B [H] a b a b b b * ] --- (s1,a)=>(s1,a)/1
[ a a a B a [H] b a b b b * ] --- (s1,b)=>(s1,b)/1
[ a a a B a b [H] a b b b * ] --- (s1,a)=>(s1,a)/1
[ a a a B a b a [H] b b b * ] --- (s1,b)=>(s1,b)/1
[ a a a B a b a b [H] b b * ] --- (s1,b)=>(s1,b)/1
[ a a a B a b a b b [H] b * ] --- (s1,b)=>(s1,b)/1
[ a a a B a b a b b b [H] * ] --- (s1,*)=>(s2,*)/-1
[ a a a B a b a b b [H] b * ] --- (s2,b)=>(s2,b)/-1
[ a a a B a b a b [H] b b * ] --- (s2,b)=>(s2,b)/-1
[ a a a B a b a [H] b b b * ] --- (s2,b)=>(s2,b)/-1
[ a a a B a b [H] a b b b * ] --- (s2,a)=>(s3,b)/-1
[ a a a B a [H] b b b b b * ] --- (s3,b)=>(s3,b)/-1
[ a a a B [H] a b b b b b * ] --- (s3,a)=>(s3,a)/-1
[ a a a [H] B a b b b b b * ] --- (s3,B)=>(s0,a)/1
[ a a a a [H] a b b b b b * ] --- (s0,a)=>(s0,a)/1
[ a a a a a [H] b b b b b * ] --- (s0,b)=>(s1,B)/1
[ a a a a a B [H] b b b b * ] --- (s1,b)=>(s1,b)/1
[ a a a a a B b [H] b b b * ] --- (s1,b)=>(s1,b)/1
[ a a a a a B b b [H] b b * ] --- (s1,b)=>(s1,b)/1
[ a a a a a B b b b [H] b * ] --- (s1,b)=>(s1,b)/1
[ a a a a a B b b b b [H] * ] --- (s1,*)=>(s2,*)/-1
[ a a a a a B b b b [H] b * ] --- (s2,b)=>(s2,b)/-1
[ a a a a a B b b [H] b b * ] --- (s2,b)=>(s2,b)/-1
[ a a a a a B b [H] b b b * ] --- (s2,b)=>(s2,b)/-1
[ a a a a a B [H] b b b b * ] --- (s2,b)=>(s2,b)/-1
[ a a a a a [H] B b b b b * ] --- (s2,B)=>(se,b)/-1
[ a a a a [H] a b b b b b * ] --- (se,a)=>(se,a)/-1
[ a a a [H] a a b b b b b * ] --- (se,a)=>(se,a)/-1
[ a a [H] a a a b b b b b * ] --- (se,a)=>(se,a)/-1
[ a [H] a a a a b b b b b * ] --- (se,a)=>(se,a)/-1
[ [H] a a a a a b b b b b * ] --- (se,a)=>(se,a)/-1
[ [H] * a a a a a b b b b b * ] --- (se,*)=>(see,*)/1
[ * [H] a a a a a b b b b b * ] --- (see,a)
Output (sort): [*, a, a, a, a, a, b, b, b, b, b, *]
```


## JavaScript

{{works with|FireFox}}

```JavaScript
function tm(d,s,e,i,b,t,... r) {
	document.write(d, '
')
	if (i<0||i>=t.length) return
	var re=new RegExp(b,'g')
	write('*',s,i,t=t.split(''))
	var p={}; r.forEach(e=>((s,r,w,m,n)=>{p[s+'.'+r]={w,n,m:[0,1,-1][1+'RL'.indexOf(m)]}})(... e.split(/[ .:,]+/)))
	for (var n=1; s!=e; n+=1) {
		with (p[s+'.'+t[i]]) t[i]=w,s=n,i+=m
		if (i==-1) i=0,t.unshift(b)
		else if (i==t.length) t[i]=b
		write(n,s,i,t)
	}
	document.write('
')
	function write(n, s, i, t) {
		t = t.join('')
		t = t.substring(0,i) + '<u>' + t.charAt(i) + '</u>' + t.substr(i+1)
		document.write(('  '+n).slice(-3).replace(/ /g,' '), ': ', s, ' [', t.replace(re,' '), ']', '
')
	}
}

tm( 'Unary incrementer',
//	 s    e   i   b    t
	'a', 'h', 0, 'B', '111',
//	 s.r: w, m, n
	'a.1: 1, L, a',
	'a.B: 1, S, h'
)

tm( 'Unary adder',
	1, 0, 0, '0', '1110111',
	'1.1: 0, R, 2', // write 0 rigth goto 2
	'2.1: 1, R, 2', // while (1) rigth
	'2.0: 1, S, 0'  // write 1 stay halt
)

tm( 'Three-state busy beaver',
	1, 0, 0, '0', '0',
	'1.0: 1, R, 2',
	'1.1: 1, R, 0',
	'2.0: 0, R, 3',
	'2.1: 1, R, 2',
	'3.0: 1, L, 3',
	'3.1: 1, L, 1'
)
```

{{out}}
 Unary incrementer
  *: a [<u>1</u>11]
  1: a [<u> </u>111]
  2: h [<u>1</u>111]

Unary adder
   *: 1 [<u>1</u>11 111]
   1: 2 [ <u>1</u>1 111]
   2: 3 [ 1<u>1</u> 111]
   3: 3 [ 11<u> </u>111]
   4: 0 [ 11<u>1</u>111]

Three-state busy beaver
   *: 1 [<u> </u>]
   1: 2 [1<u> </u>]
   2: 3 [1 <u> </u>]
   3: 3 [1<u> </u>1]
   4: 3 [<u>1</u>11]
   5: 1 [<u> </u>111]
   6: 2 [1<u>1</u>11]
   7: 2 [11<u>1</u>1]
   8: 2 [111<u>1</u>]
   9: 2 [1111<u> </u>]
  10: 3 [1111 <u> </u>]
  11: 3 [1111<u> </u>1]
  12: 3 [111<u>1</u>11]
  13: 1 [11<u>1</u>111]
  14: 0 [111<u>1</u>11]


## Julia


```julia
import Base.show

@enum Move Left=1 Stay Right

mutable struct MachineState
    state::String
    tape::Dict{Int, String}
    headpos::Int
end

struct Rule
    instate::String
    s1::String
    s2::String
    move::Move
    outstate::String
end

struct Program
    title::String
    initial::String
    final::String
    blank::String
    rules::Vector{Rule}
end

const testprograms = [
    (Program("Simple incrementer", "q0", "qf", "B",
        [Rule("q0", "1", "1", Right, "q0"), Rule("q0", "B", "1", Stay, "qf")]),
     Dict(1 =>"1", 2 => "1", 3 => "1"), true),
    (Program("Three-state busy beaver", "a", "halt", "0",
        [Rule("a", "0", "1", Right, "b"), Rule("a", "1", "1", Left, "c"),
         Rule("b", "0", "1", Left, "a"), Rule("b", "1", "1", Right, "b"),
         Rule("c", "0", "1", Left, "b"), Rule("c", "1", "1", Stay, "halt")]),
     Dict(), true),
    (Program("Five-state busy beaver", "A", "H", "0",
        [Rule("A", "0", "1", Right, "B"), Rule("A", "1", "1", Left, "C"),
         Rule("B", "0", "1", Right, "C"), Rule("B", "1", "1", Right, "B"),
         Rule("C", "0", "1", Right, "D"), Rule("C", "1", "0", Left, "E"),
         Rule("D", "0", "1", Left, "A"), Rule("D", "1", "1", Left, "D"),
         Rule("E", "0", "1", Stay, "H"), Rule("E", "1", "0", Left, "A")]),
     Dict(), false)]

function show(io::IO, mstate::MachineState)
    ibracket(i, curpos, val) = (i == curpos) ? "[$val]" : " $val "
    print(io, rpad("($(mstate.state))", 12))
    for i in sort(collect(keys(mstate.tape)))
        print(io, "   $(ibracket(i, mstate.headpos, mstate.tape[i]))")
    end
end

function turing(program, tape, verbose)
    println("\n$(program.title)")
    verbose && println(" State       \tTape [head]\n--------------------------------------------------")
    mstate = MachineState(program.initial, tape, 1)
    stepcount = 0
    while true
        if !haskey(mstate.tape, mstate.headpos)
            mstate.tape[mstate.headpos] = program.blank
        end
        verbose && println(mstate)
        for rule in program.rules
            if rule.instate == mstate.state && rule.s1 == mstate.tape[mstate.headpos]
                mstate.tape[mstate.headpos] = rule.s2
                if rule.move == Left
                    mstate.headpos -= 1
                elseif rule.move == Right
                    mstate.headpos += 1
                end
                mstate.state = rule.outstate
                break
            end
        end
        stepcount += 1
        if mstate.state == program.final
            break
        end
    end
    verbose && println(mstate)
    println("Total steps: $stepcount")
end

for (prog, tape, verbose) in testprograms
        turing(prog, tape, verbose)
end

```
{{out}}

```txt

Simple incrementer
 State          Tape [head]
--------------------------------------------------
(q0)           [1]    1     1
(q0)            1    [1]    1
(q0)            1     1    [1]
(q0)            1     1     1    [B]
(qf)            1     1     1    [1]
Total steps: 4

Three-state busy beaver
 State          Tape [head]
--------------------------------------------------
(a)            [0]
(b)             1    [0]
(a)            [1]    1
(c)            [0]    1     1
(b)            [0]    1     1     1
(a)            [0]    1     1     1     1
(b)             1    [1]    1     1     1
(b)             1     1    [1]    1     1
(b)             1     1     1    [1]    1
(b)             1     1     1     1    [1]
(b)             1     1     1     1     1    [0]
(a)             1     1     1     1    [1]    1
(c)             1     1     1    [1]    1     1
(halt)          1     1     1    [1]    1     1
Total steps: 13

Five-state busy beaver
Total steps: 47176870

```



## Kotlin

{{trans|C}}

```scala
// version 1.2.10

enum class Dir { LEFT, RIGHT, STAY }

class Rule(
    val state1: String,
    val symbol1: Char,
    val symbol2: Char,
    val dir: Dir,
    val state2: String
)

class Tape(
    var symbol: Char,
    var left: Tape? = null,
    var right: Tape? = null
)

class Turing(
    val states: List<String>,
    val finalStates: List<String>,
    val symbols: CharArray,
    val blank: Char,
    var state: String,
    tapeInput: CharArray,
    rules: List<Rule>
) {
    var tape: Tape? = null
    val transitions = Array(states.size) { arrayOfNulls<Rule>(symbols.size) }

    init {
        for (i in 0 until tapeInput.size) {
            move(Dir.RIGHT)
            tape!!.symbol = tapeInput[i]
        }
        if (tapeInput.size == 0) move(Dir.RIGHT)
        while (tape!!.left != null) tape = tape!!.left
        for (i in 0 until rules.size) {
            val rule = rules[i]
            transitions[stateIndex(rule.state1)][symbolIndex(rule.symbol1)] = rule
        }
    }

    private fun stateIndex(state: String): Int {
        val i = states.indexOf(state)
        return if (i >= 0) i else 0
    }

    private fun symbolIndex(symbol: Char): Int {
        val i = symbols.indexOf(symbol)
        return if (i >= 0) i else 0
    }

    private fun move(dir: Dir) {
        val orig = tape
        when (dir) {
            Dir.RIGHT -> {
                if (orig != null && orig.right != null) {
                    tape = orig.right
                }
                else {
                    tape = Tape(blank)
                    if (orig != null) {
                        tape!!.left = orig
                        orig.right = tape
                    }
                }
            }

            Dir.LEFT -> {
                if (orig != null && orig.left != null) {
                    tape = orig.left
                }
                else {
                    tape = Tape(blank)
                    if (orig != null) {
                        tape!!.right = orig
                        orig.left = tape
                    }
                }
            }

            Dir.STAY -> {}
        }
    }

    fun printState() {
        print("%-10s ".format(state))
        var t = tape
        while (t!!.left != null ) t = t.left
        while (t != null) {
            if (t == tape) print("[${t.symbol}]")
            else           print(" ${t.symbol} ")
            t = t.right
        }
        println()
    }

    fun run(maxLines: Int = 20) {
        var lines = 0
        while (true) {
            printState()
            for (finalState in finalStates) {
                if (finalState == state) return
            }
            if (++lines == maxLines) {
                println("(Only the first $maxLines lines displayed)")
                return
            }
            val rule = transitions[stateIndex(state)][symbolIndex(tape!!.symbol)]
            tape!!.symbol = rule!!.symbol2
            move(rule.dir)
            state = rule.state2
        }
    }
}

fun main(args: Array<String>) {
    println("Simple incrementer")
    Turing(
        states      = listOf("q0", "qf"),
        finalStates = listOf("qf"),
        symbols     = charArrayOf('B', '1'),
        blank       = 'B',
        state       = "q0",
        tapeInput   = charArrayOf('1', '1', '1'),
        rules       = listOf(
            Rule("q0", '1', '1', Dir.RIGHT, "q0"),
            Rule("q0", 'B', '1', Dir.STAY, "qf")
        )
    ).run()

    println("\nThree-state busy beaver")
    Turing(
        states      = listOf("a", "b", "c", "halt"),
        finalStates = listOf("halt"),
        symbols     = charArrayOf('0', '1'),
        blank       = '0',
        state       = "a",
        tapeInput   = charArrayOf(),
        rules       = listOf(
            Rule("a", '0', '1', Dir.RIGHT, "b"),
            Rule("a", '1', '1', Dir.LEFT, "c"),
            Rule("b", '0', '1', Dir.LEFT, "a"),
            Rule("b", '1', '1', Dir.RIGHT, "b"),
            Rule("c", '0', '1', Dir.LEFT, "b"),
            Rule("c", '1', '1', Dir.STAY, "halt")
        )
    ).run()

    println("\nFive-state two-symbol probable busy beaver")
    Turing(
        states      = listOf("A", "B", "C", "D", "E", "H"),
        finalStates = listOf("H"),
        symbols     = charArrayOf('0', '1'),
        blank       = '0',
        state       = "A",
        tapeInput   = charArrayOf(),
        rules       = listOf(
            Rule("A", '0', '1', Dir.RIGHT, "B"),
            Rule("A", '1', '1', Dir.LEFT, "C"),
            Rule("B", '0', '1', Dir.RIGHT, "C"),
            Rule("B", '1', '1', Dir.RIGHT, "B"),
            Rule("C", '0', '1', Dir.RIGHT, "D"),
            Rule("C", '1', '0', Dir.LEFT, "E"),
            Rule("D", '0', '1', Dir.LEFT, "A"),
            Rule("D", '1', '1', Dir.LEFT, "D"),
            Rule("E", '0', '1', Dir.STAY, "H"),
            Rule("E", '1', '0', Dir.LEFT, "A")
        )
    ).run()
}
```


{{out}}

```txt

Simple incrementer
q0         [1] 1  1
q0          1 [1] 1
q0          1  1 [1]
q0          1  1  1 [B]
qf          1  1  1 [1]

Three-state busy beaver
a          [0]
b           1 [0]
a          [1] 1
c          [0] 1  1
b          [0] 1  1  1
a          [0] 1  1  1  1
b           1 [1] 1  1  1
b           1  1 [1] 1  1
b           1  1  1 [1] 1
b           1  1  1  1 [1]
b           1  1  1  1  1 [0]
a           1  1  1  1 [1] 1
c           1  1  1 [1] 1  1
halt        1  1  1 [1] 1  1

Five-state two-symbol probable busy beaver
A          [0]
B           1 [0]
C           1  1 [0]
D           1  1  1 [0]
A           1  1 [1] 1
C           1 [1] 1  1
E          [1] 0  1  1
A          [0] 0  0  1  1
B           1 [0] 0  1  1
C           1  1 [0] 1  1
D           1  1  1 [1] 1
D           1  1 [1] 1  1
D           1 [1] 1  1  1
D          [1] 1  1  1  1
D          [0] 1  1  1  1  1
A          [0] 1  1  1  1  1  1
B           1 [1] 1  1  1  1  1
B           1  1 [1] 1  1  1  1
B           1  1  1 [1] 1  1  1
B           1  1  1  1 [1] 1  1
(Only the first 20 lines displayed)

```



## Lua


```Lua
-- Machine definitions
local incrementer = {
    name = "Simple incrementer",
    initState = "q0",
    endState = "qf",
    blank = "B",
    rules = {
        {"q0", "1", "1", "right", "q0"},
        {"q0", "B", "1", "stay", "qf"}
    }
}

local threeStateBB = {
    name = "Three-state busy beaver",
    initState = "a",
    endState = "halt",
    blank = "0",
    rules = {
        {"a", "0", "1", "right", "b"},
        {"a", "1", "1", "left", "c"},
        {"b", "0", "1", "left", "a"},
        {"b", "1", "1", "right", "b"},
        {"c", "0", "1", "left", "b"},
        {"c", "1", "1", "stay", "halt"}
    }
}

local fiveStateBB = {
    name = "Five-state busy beaver",
    initState = "A",
    endState = "H",
    blank = "0",
    rules = {
        {"A", "0", "1", "right", "B"},
        {"A", "1", "1", "left", "C"},
        {"B", "0", "1", "right", "C"},
        {"B", "1", "1", "right", "B"},
        {"C", "0", "1", "right", "D"},
        {"C", "1", "0", "left", "E"},
        {"D", "0", "1", "left", "A"},
        {"D", "1", "1", "left", "D"},
        {"E", "0", "1", "stay", "H"},
        {"E", "1", "0", "left", "A"}
    }
}

-- Display a representation of the tape and machine state on the screen
function show (state, headPos, tape)
    local leftEdge = 1
    while tape[leftEdge - 1] do leftEdge = leftEdge - 1 end
    io.write(" " .. state .. "\t| ")
    for pos = leftEdge, #tape do
        if pos == headPos then io.write("[" .. tape[pos] .. "] ") else io.write(" " .. tape[pos] .. "  ") end
    end
    print()
end

-- Simulate a turing machine
function UTM (machine, tape, countOnly)
    local state, headPos, counter = machine.initState, 1, 0
    print("\n\n" .. machine.name)
    print(string.rep("=", #machine.name) .. "\n")
    if not countOnly then print(" State", "| Tape [head]\n---------------------") end
    repeat
        if not tape[headPos] then tape[headPos] = machine.blank end
        if not countOnly then show(state, headPos, tape) end
        for _, rule in ipairs(machine.rules) do
            if rule[1] == state and rule[2] == tape[headPos] then
                tape[headPos] = rule[3]
                if rule[4] == "left" then headPos = headPos - 1 end
                if rule[4] == "right" then headPos = headPos + 1 end
                state = rule[5]
                break
            end
        end
        counter = counter + 1
    until state == machine.endState
    if countOnly then print("Steps taken: " .. counter) else show(state, headPos, tape) end
end

-- Main procedure
UTM(incrementer, {"1", "1", "1"})
UTM(threeStateBB, {})
UTM(fiveStateBB, {}, "countOnly")
```

{{out}}

```txt


Simple incrementer

### ============


 State  | Tape [head]
---------------------
 q0     | [1]  1   1
 q0     |  1  [1]  1
 q0     |  1   1  [1]
 q0     |  1   1   1  [B]
 qf     |  1   1   1  [1]


Three-state busy beaver

### =================


 State  | Tape [head]
---------------------
 a      | [0]
 b      |  1  [0]
 a      | [1]  1
 c      | [0]  1   1
 b      | [0]  1   1   1
 a      | [0]  1   1   1   1
 b      |  1  [1]  1   1   1
 b      |  1   1  [1]  1   1
 b      |  1   1   1  [1]  1
 b      |  1   1   1   1  [1]
 b      |  1   1   1   1   1  [0]
 a      |  1   1   1   1  [1]  1
 c      |  1   1   1  [1]  1   1
 halt   |  1   1   1  [1]  1   1


Five-state busy beaver

### ================


Steps taken: 47176870
```



## Mathematica



### The universal machine


Updated to use dynamic definition of a function. Values computed for each input are saved.
Functionally equivalent to computing a matrix for a set of inputs.


```Mathematica

left = 1; right = -1; stay = 0;
cmp[s_] := ToExpression[StringSplit[s, ","]];
utm[rules_, initial_, head_] :=
  Module[{tape = initial, rh = head, n = 1},
   Clear[nxt];
   nxt[state_, field_] :=
    nxt[state, field] = Position[rules, {rules[[state, 5]], field, _, _, _}][[1, 1]];
   n = Position[rules, {rules[[n, 1]], BitGet[tape, rh], _, _, _}][[1,1]];
   While[rules[[n, 4]] != 0,
    If[rules[[n, 3]] != BitGet[tape, rh],
     If[rules[[n, 3]] == 1, tape = BitSet[tape, rh],
      tape = BitClear[tape, rh]]];
    rh = rh + rules[[n, 4]];
    If[rh < 0, rh = 0; tape = 2*tape];
    n = nxt[n, BitGet[tape, rh]];
    ]; {tape, rh}
   ];
];
```



### A print routine and test drivers



```Mathematica

printMachine[tape_,pos_]:=(mach=IntegerString[tape,2];
ptr=StringReplace[mach,{"0"-> " ","1"->" "}];
Print[mach];Print[StringInsert[ptr,"^",StringLength[ptr]-pos]];);

simpleIncr={"q0,1,1,right,q0","q0,B,1,stay,qf"};
simpleIncr=Map[cmp,simpleIncr]/.B->0;
fin=utm[simpleIncr,7,2];
printMachine[fin[[1]],fin[[2]]];

busyBeaver3S={
"a,0,1,right,b",
"a,1,1,left,c",
"b,0,1,left,a",
"b,1,1,right,b",
"c,0,1,left,b",
"c,1,1,stay,halt"};
fin=utm[Map[cmp,busyBeaver3S],0,0];
printMachine[fin[[1]],fin[[2]]];

```


Summary output from the 2 short machines
{{out}}

```txt

1110
   ^
111111
   ^

```

===A machine with 47,176,870 steps===
Runs in 4 minutes on an i5 desktop (with the dynamic function definiton).
The resulting tape is very long, we'll print the result of treating the value as a binary encoded integer.

```Mathematica

probable5S={
"A, 0, 1, right, B",
"A, 1, 1, left, C",
"B, 0, 1, right, C",
"B, 1, 1, right, B",
"C, 0, 1, right, D",
"C, 1, 0, left, E",
"D, 0, 1, left, A",
"D, 1, 1, left, D",
"E, 0, 1, stay, H",
"E, 1, 0, left, A"};
fin=utm[Map[cmp,probable5S],0,0];
]


fin[[1]]//N
3.254757786465838*10^3698

```



## Mercury


### The universal machine

Source for this example was lightly adapted from [https://bitbucket.org/ttmrichter/turing https://bitbucket.org/ttmrichter/turing].  Of particular interest in this implementation is that because of the type parameterisation of the <code>config</code> type, the machine being simulated cannot be compiled if there is any mistake in the states, symbols and actions.  Also, because of Mercury's determinism detection and enforcement, it's impossible to pass in a non-deterministic set of rules.  At most one answer can come back from the rules interface.

```mercury
:- module turing.

:- interface.

:- import_module list.
:- import_module set.

:- type config(State, Symbol)
    ---> config(initial_state  :: State,
                halting_states :: set(State),
                blank          :: Symbol ).

:- type action ---> left ; stay ; right.

:- func turing(config(State, Symbol),
               pred(State, Symbol, Symbol, action, State),
               list(Symbol)) = list(Symbol).
:- mode turing(in,
               pred(in, in, out, out, out) is semidet,
               in) = out is det.

:- implementation.

:- import_module pair.
:- import_module require.

turing(Config@config(Start, _, _), Rules, Input) = Output :-
    (Left-Right) = perform(Config, Rules, Start, ([]-Input)),
    Output = append(reverse(Left), Right).

:- func perform(config(State, Symbol),
                pred(State, Symbol, Symbol, action, State),
                State, pair(list(Symbol))) = pair(list(Symbol)).
:- mode perform(in, pred(in, in, out, out, out) is semidet,
                in, in) = out is det.
perform(Config@config(_, Halts, Blank), Rules, State,
        Input@(LeftInput-RightInput)) = Output :-
    symbol(RightInput, Blank, RightNew, Symbol),
    ( set.member(State, Halts) ->
        Output = Input
    ; Rules(State, Symbol, NewSymbol, Action, NewState) ->
        NewLeft  = pair(LeftInput, [NewSymbol|RightNew]),
        NewRight = action(Action, Blank, NewLeft),
        Output   = perform(Config, Rules, NewState, NewRight)
    ;
        error("an impossible state has apparently become possible") ).

:- pred symbol(list(Symbol), Symbol, list(Symbol), Symbol).
:- mode symbol(in, in, out, out) is det.
symbol([],        Blank, [],  Blank).
symbol([Sym|Rem], _,     Rem, Sym).

:- func action(action, State, pair(list(State))) = pair(list(State)).
action(left,  Blank, ([]-Right))            = ([]-[Blank|Right]).
action(left,  _,     ([Left|Lefts]-Rights)) = (Lefts-[Left|Rights]).
action(stay,  _,     Tape)                  = Tape.
action(right, Blank, (Left-[]))             = ([Blank|Left]-[]).
action(right, _,     (Left-[Right|Rights])) = ([Right|Left]-Rights).
```


### The incrementer machine

This machine has been stripped of the Mercury ceremony around modules, imports, etc.

```mercury
:- type incrementer_states  ---> a ; halt.
:- type incrementer_symbols ---> b ; '1'.

:- func incrementer_config = config(incrementer_states, incrementer_symbols).
incrementer_config = config(a,           % the initial state
                            set([halt]), % the set of halting states
                            b).          % the blank symbol

:- pred incrementer(incrementer_states::in,
                    incrementer_symbols::in,
                    incrementer_symbols::out,
                    action::out,
                    incrementer_states::out) is semidet.
incrementer(a, '1', '1', right, a).
incrementer(a, b,   '1', stay,  halt).

TapeOut = turing(incrementer_config, incrementer, [1, 1, 1]).
```

This will, on execution, fill TapeOut with [1, 1, 1, 1].

### The busy beaver machine

This machine has been stripped of the Mercury ceremony around modules, imports, etc.

```mercury
:- type busy_beaver_states  ---> a ; b ; c ; halt.
:- type busy_beaver_symbols ---> '0' ; '1'.

:- func busy_beaver_config = config(busy_beaver_states, busy_beaver_symbols).
busy_beaver_config = config(a,           % initial state
                            set([halt]), % set of terminating states
                            '0').        % blank symbol

:- pred busy_beaver(busy_beaver_states::in,
                    busy_beaver_symbols::in,
                    busy_beaver_symbols::out,
                    action::out,
                    busy_beaver_states::out) is semidet.
busy_beaver(a, '0', '1', right, b).
busy_beaver(a, '1', '1', left,  c).
busy_beaver(b, '0', '1', left,  a).
busy_beaver(b, '1', '1', right, b).
busy_beaver(c, '0', '1', left,  b).
busy_beaver(c, '1', '1', stay,  halt).

TapeOut = turing(busy_beaver_config, busy_beaver, []).
```

This will, on execution, fill TapeOut with [1, 1, 1, 1, 1, 1].


## NetLogo


The following is the Code section of the NetLogo file UTMachine_RSdan3dewey.nlogo which can be
downloaded from the page:

http://sites.google.com/site/dan3deweyscspaimsportfolio/extra-turing-machine

This page also has other information, screen shots, etc.


```netlogo

;; "A Turing Turtle": a Turing Machine implemented in NetLogo
;;    by Dan Dewey 1/16/2016
;;
;; This NetLogo code implements a Turing Machine, see, e.g.,
;;    http://en.wikipedia.org/wiki/Turing_machine
;; The Turing machine fits nicely into the NetLogo paradigm in which
;; there are agents (aka the turtles), that move around
;; in a world of "patches" (2D cells).
;; Here, a single agent represents the Turing machine read/write head
;; and the patches represent the Turing tape values via their colors.
;; The 2D array of patches is treated as a single long 1D tape in an
;; obvious way.

;; This program is presented as a NetLogo example on the page:
;;    http://rosettacode.org/wiki/Universal_Turing_machine
;;    This file may be larger than others on that page, note however
;;    that I include many comments in the code and I have made no
;;    effort to 'condense' the code, prefering clarity over compactness.
;; A demo and discussion of this program is on the web page:
;;    http://sites.google.com/site/dan3deweyscspaimsportfolio/extra-turing-machine
;; The Copy example machine was taken from:
;;    http://en.wikipedia.org/wiki/Turing_machine_examples
;; The "Busy Beaver" machines encoded below were taken from:
;;    http://www.logique.jussieu.fr/~michel/ha.html

;; The implementation here allows 3 symbols (blank, 0, 1) on the tape
;; and 3 head motions (left, stay, right).

;; The 2D world is nominally set to be 29x29, going from (-14,-14) to
;; (14,14) from lower left to upper right and with (0,0) at the center.
;; This gives a total Turing tape length of 29^2 = 841 cells, sufficient for the
;; "Lazy" Beaver 5,2 example.
;; Since the max-pxcor variable is used in the code below (as opposed to
;; a hard-coded number), the effective tape size can be changed by
;; changing the size of the 2D world with the Settings...  button on the interface.

;; The "Info" tab of the NetLogo interface contains some further comments.
;; - - - - - - -


;; - - - - - - - - - - - Global/Agent variables
;; These three 2D arrays (lists of lists) encode the Turing Machine rules:
;;    WhatToWrite:  -1 (Blank), 0, 1
;;    HowToMove:    -1 (left), 0(stay), 1 (right)
;;    NextState:    0 to N-1, negative value goes to a halt state.
;; The above are a function of the current state and the current tape (patch) value.
;; MachineState is used by the turtle to pass the current state of the Turing machine
;; (or the halt code) to the observer.
globals [ WhatToWrite HowToMove NextState MachineState
   ;; some other golobals of secondary importance...
   ;; set different patch colors to record the Turing tape values
    BlankColor ZeroColor OneColor
   ;; a delay constant to slow down the operation
    RealTimePerTick ]

;; We'll have one turtle which is the Turing machine read/write head
;; it will keep track of the current Turing state in its own MyState value
turtles-own [ MyState ]


;; - - - - - - - - - - -
to Setup  ;; sets up the world
  clear-all  ;; clears the world first

  ;; Try to not have (too many) ad hoc numbers in the code,
  ;; collect and set various values here especially if they might be used in multiple places:
  ;;    The colors for Blank, Zero and One :   (user can can change as desired)
  set BlankColor 2 ;; dark gray
  set OneColor green
  set ZeroColor red
  ;;    slow it down for the humans to watch
  set RealTimePerTick 0.2  ;; have simulation go at nice realtime speed

  create-turtles 1   ;; create the one Turing turtle
  [                  ;; set default parameters
    set size 2       ;; set a nominal size
    set color yellow ;; color of border
    ;; set the starting location, some Turing programs will adjust this if needed:
    setxy 0 0 ;; -1 * max-pxcor -1 * max-pxcor
    set shape "square2empty"   ;; edited version of "square 2" to have clear in middle

    ;; set the starting state - always 0
    set MyState 0
    set MachineState 0   ;; the turtle will update this global value from now on
  ]

  ;; Define the Turing machine rules with 2D lists.
  ;; Based on the selection made on interface panel, setting the string Turing_Program_Selection.
  ;; This routine has all the Turing 'programs' in it - it's at the very bottom of this file.
  LoadTuringProgram

  ;; the environment, e.g. the Turing tape
  ask patches
  [
    ;; all patches are set to the blank color
    set pcolor BlankColor
  ]

  ;; keep track of time; each tick is a Turing step
  reset-ticks
end


;; - - - - - - - - - - - - - - - -
to Go  ;; this  repeatedly does steps

  ;; The turtle does the main work
  ask turtles
  [
    DoOneStep
    wait RealTimePerTick
  ]

  tick

  ;; The Turing turtle will die if it tries to go beyond the cells,
  ;; in that case (no turtles left) we'll stop.
  ;; Also stop if the MachineState has been set to a negative number (a halt state).
  if ((count turtles = 0) or (MachineState < 0))
  [ stop ]

end

to DoOneStep
   ;; have the turtle do one Turing step
   ;; First, 'read the tape', i.e., based on the patch color here:
   let tapeValue GetTapeValue

   ;; using the tapeValue and MyState, get the desired actions here:
   ;; (the item commands extract the appropriate value from the list-of-lists)
   let myWrite item (tapeValue + 1) (item MyState WhatToWrite)
   let myMove item (tapeValue + 1) (item MyState HowToMove)
   let myNextState item (tapeValue + 1) (item MyState NextState)

   ;; Write to the tape as appropriate
   SetTapeValue myWrite

   ;; Move as appropriate
   if (myMove = 1) [MoveForward]
   if (myMove = -1) [MoveBackward]

   ;; Go to the next state; check if it is a halt state.
   ;; Update the global MachineState value
   set MachineState myNextState
   ifelse (myNextState < 0)
   [
     ;; It's a halt state.  The negative MachineState will signal the stop.
     ;; Go back to the starting state so it can be re-run if desired.
     set MyState 0]
   [
     ;; Not a halt state, so change to the desired next state
     set MyState myNextState
     ]
end

to MoveForward
  ;; move the turtle forward one cell, including line wrapping.
  set heading 90
  ifelse (xcor = max-pxcor)
    [set xcor -1 * max-pxcor
      ;; and go up a row if possible... otherwise die
      ifelse ycor = max-pxcor
      [ die ]  ;; tape too short - a somewhat crude end of things ;-)
      [set ycor ycor + 1]
    ]
    [jump 1]
end

to MoveBackward
  ;; move the turtle backward one cell, including line-wrapping.
  set heading -90
  ifelse (xcor = -1 * max-pxcor)
    [
      set xcor max-pxcor
      ;; and go down a row... or die
      ifelse ycor = -1 * max-pxcor
      [ die ]  ;; tape too short - a somewhat crude end of things ;-)
      [set ycor ycor - 1]
    ]
    [jump 1]
end

to-report GetTapeValue
  ;; report the tape color equivalent value
  if (pcolor = ZeroColor) [report 0]
  if (pcolor = OneColor) [report 1]
  report -1
end

to SetTapeValue [ value ]
  ;; write the appropriate color on the tape
  ifelse (value = 1)
  [set pcolor OneColor]
  [ ifelse (value = 0)
    [set pcolor ZeroColor][set pcolor BlankColor]]
end


;; - - - - - OK, here are the data for the various Turing programs...
;; Note that besdes settting the rules (array values) these sections can also
;; include commands to clear the tape, position the r/w head, adjust wait time, etc.
to LoadTuringProgram

  ;; A template of the rules structure: a list of lists
  ;; E.g. values are given for States 0 to 4, when looking at Blank, Zero, One:
  ;; For 2-symbol machines use Blank(-1) and One(1) and ignore the middle values (never see zero).
  ;; Normal Halt will be state -1, the -9 default shows an unexpected halt.
  ;;                       state 0       state 1       state 2       state 3       state 4
  set WhatToWrite (list (list -1 0 1) (list -1 0 1) (list -1 0 1) (list -1 0 1) (list -1 0 1) )
  set HowToMove    (list (list 0 0 0)  (list 0 0 0)  (list 0 0 0)  (list 0 0 0)  (list 0 0 0) )
  set NextState(list (list -9 -9 -9) (list -9 -9 -9) (list -9 -9 -9) (list -9 -9 -9) (list -9 -9 -9) )

  ;; Fill the rules based on the selected case
  if (Turing_Program_Selection = "Simple Incrementor")
  [
    ;; simple Incrementor - this is from the RosettaCode Universal Turing Machine page - very simple!
    set WhatToWrite (list (list 1 0 1) )
    set HowToMove   (list (list 0 0 1) )
    set NextState   (list (list -1 -9 0) )
  ]

  ;; Fill the rules based on the selected case
  if (Turing_Program_Selection = "Incrementor w/Return")
  [
    ;; modified Incrementor: it returns to the first 1 on the left.
    ;; This version allows the "Copy Ones to right" program to directly follow it.
    ;;                     move right    append one    back to beginning
    set WhatToWrite (list (list -1 0 1) (list 1 0 1)   (list -1 0 1) )
    set HowToMove   (list (list 1 0 1)  (list 0 0 1)   (list 1 0 -1) )
    set NextState   (list (list 1 -9 1) (list 2 -9 1)  (list -1 -9 2) )
  ]

  ;; Fill the rules based on the selected case
  if (Turing_Program_Selection = "Copy Ones to right")
  [
    ;; "Copy" from Wiki "Turing machine examples" page; slight mod so that it ends on first 1
    ;; of the copy allowing Copy to be re-executed to create another copy.
    ;; Has 5 states and uses Blank and 1 to make a copy of a string of ones;
    ;; this can be run after runs of the "Incrementor w/Return".
    ;;                       state 0       state 1        state 2       state 3       state 4
    set WhatToWrite (list (list -1 0 -1) (list -1 0 1)  (list 1 0 1)   (list -1 0 1)  (list 1 0 1) )
    set HowToMove   (list (list 1 0 1)   (list 1 0 1)   (list -1 0 1)   (list -1 0 -1)   (list 1 0 -1) )
    set NextState   (list (list -1 -9 1)  (list 2 -9 1)  (list 3 -9 2)  (list 4 -9 3)  (list 0 -9 4) )
  ]

  ;; Fill the rules based on the selected case
  if (Turing_Program_Selection = "Binary Counter")
  [
    ;; Count in binary - can start on a blank space.
    ;;         States:       start          carry-1          back-to-beginning
    set WhatToWrite (list (list 1 1 0)      (list 1 1 0)      (list -1 0 1)  )
    set HowToMove (list   (list 0 0 -1)     (list 0 0 -1)     (list -1 1 1)  )
    set NextState (list   (list -1 -1 1)    (list 2 2 1)      (list -1 2 2)  )
    ;; Select line above from these two:
    ;; can either count by 1 each time it is run:
    ;;    set NextState (list   (list -1 -1 1)    (list 2 2 1)      (list -1 2 2)  )
    ;; or count forever once started:
    ;;    set NextState (list   (list 0 0 1)      (list 2 2 1)      (list 0 2 2)  )
    set RealTimePerTick 0.2
  ]

  if (Turing_Program_Selection = "Busy-Beaver 3-State, 2-Sym")
  [
    ;; from the RosettaCode.org Universal Turing Machine page
    ;;      state name:           a             b             c
    set WhatToWrite (list (list 1 0 1) (list 1 0 1) (list 1 0 1) (list -1 0 1) (list -1 0 1) )
    set HowToMove (list (list 1 0 -1) (list -1 0 1) (list -1 0 0) (list 0 0 0) (list 0 0 0) )
    set NextState (list (list 1 -9 2) (list 0 -9 1) (list 1 -9 -1) (list -9 -9 -9) (list -9 -9 -9) )
    ;; Clear the tape
    ask Patches [set pcolor BlankColor]
  ]

  ;; should output 13 ones and take 107 steps to do it...
  if (Turing_Program_Selection = "Busy-Beaver 4-State, 2-Sym")
  [
    ;; from the RosettaCode.org Universal Turing Machine page
    ;;      state name:           A            B            C             D
    set WhatToWrite (list (list 1 0 1) (list 1 0 -1) (list 1 0 1) (list 1 0 -1) (list -1 0 1) )
    set HowToMove (list (list 1 0 -1) (list -1 0 -1) (list 1 0 -1) (list 1 0 1) (list 0 0 0) )
    set NextState (list (list 1 -9 1) (list 0 -9 2) (list -1 -9 3) (list 3 -9 0) (list -9 -9 -9) )
    ;; Clear the tape
    ask Patches [set pcolor BlankColor]
  ]

  ;; This takes 38 steps to write 9 ones/zeroes
  if (Turing_Program_Selection = "Busy-Beaver 2-State, 3-Sym")
  [
    ;;                            A            B
    set WhatToWrite (list (list 0 1 0) (list 1 1 0) (list -1 0 1) (list -1 0 1) (list -1 0 1) )
    set HowToMove    (list (list 1 -1 1)  (list -1 1 -1)  (list 0 0 0)  (list 0 0 0)  (list 0 0 0) )
    set NextState(list (list 1 1 -1) (list 0 1 1) (list -9 -9 -9) (list -9 -9 -9) (list -9 -9 -9) )
    ;; Clear the tape
    ask Patches [set pcolor BlankColor]
  ]

  ;; This only makes 501 ones and stops after 134,467 steps -- it does do that !!!
  if (Turing_Program_Selection = "Lazy-Beaver 5-State, 2-Sym")
  [
    ;; from the RosettaCode.org Universal Turing Machine page
    ;;      state name:           A0            B1           C2             D3           E4
    set WhatToWrite (list (list 1 0 -1) (list 1 0 1) (list 1 0 -1) (list -1 0 1) (list 1 0 1) )
    set HowToMove (list (list 1 0 -1) (list 1 0 1) (list -1 0 1) (list 1 0 1) (list -1 0 1) )
    set NextState (list (list 1 -9 2) (list 2 -9 3) (list 0 -9 1) (list 4 -9 -1) (list 2 -9 0) )
    ;; Clear the tape
    ask Patches [set pcolor BlankColor]
    ;; Looks like it goes much more forward than back on the tape
    ;; so start the head just a row from the bottom:
    ask turtles [setxy 0 -1 * max-pxcor + 1]
    ;; and go faster
    set RealTimePerTick 0.02
  ]

  ;; The rest have large outputs and run for a long time, so I haven't confirmed
  ;; that they work as advertised...

  ;; This is the 5,2 record holder: 4098 ones in 47,176,870 steps.
  ;; With max-pxcor of 14 and offset r/w head start (below), this will
  ;; run off the tape at about 150,000+steps...
  if (Turing_Program_Selection = "Busy-Beaver 5-State, 2-Sym")
  [
    ;; from the RosettaCode.org Universal Turing Machine page
    ;;      state name:           A            B            C             D             E
    set WhatToWrite (list (list 1 0 1) (list 1 0 1) (list 1 0 -1) (list 1 0 1) (list 1 0 -1) )
    set HowToMove (list (list 1 0 -1) (list 1 0 1) (list 1 0 -1) (list -1 0 -1) (list 1 0 -1) )
    set NextState (list (list 1 -9 2) (list 2 -9 1) (list 3 -9 4) (list 0 -9 3) (list -1 -9 0) )
    ;; Clear the tape
    ask Patches [set pcolor BlankColor]
    ;; Writes more backward than forward, so start a few rows from the top:
    ask turtles [setxy 0 max-pxcor - 3]
    ;; and go faster
    set RealTimePerTick 0.02
  ]

  if (Turing_Program_Selection = "Lazy-Beaver 3-State, 3-Sym")
  [
    ;; This should write 5600 ones/zeros and take 29,403,894 steps.
    ;; Ran it to 175,000+ steps and only covered 1/2 of the cells (w/max-pxcor = 14)...
    ;;      state name:           A            B            C
    set WhatToWrite (list (list 0 1 0) (list 1 -1 0) (list 0 1 0) (list -1 0 1) (list -1 0 1) )
    set HowToMove (list (list 1 1 -1) (list -1 1 1) (list 1 -1 1) (list 0 0 0) (list 0 0 0) )
    set NextState (list (list 1 0 0) (list 2 2 1) (list -1 0 1) (list -9 -9 -9) (list -9 -9 -9) )
    ;; Clear the tape
    ask Patches [set pcolor BlankColor]
    ;; It goes much more forward than back on the tape
    ;; so start the head just a row from the bottom:
    ask turtles [setxy 0 -1 * max-pxcor + 1]
    ;; and go faster
    set RealTimePerTick 0.02
  ]

  if (Turing_Program_Selection = "Busy-Beaver 3-State, 3-Sym")
  [
    ;; This should write 374,676,383 ones/zeros and take 119,112,334,170,342,540 (!!!) steps.
    ;; Rn it to ~ 175,000 steps covering about 2/3 of the max-pxcor=14 cells.
    ;;      state name:           A            B            C
    set WhatToWrite (list (list 0 1 0) (list -1 1 0) (list 0 0 0) (list -1 0 1) (list -1 0 1) )
    set HowToMove (list (list 1 -1 -1) (list -1 1 -1) (list 1 1 1) (list 0 0 0) (list 0 0 0) )
    set NextState (list (list 1 0 2) (list 0 1 1) (list -1 0 2) (list -9 -9 -9) (list -9 -9 -9) )
    ;; Clear the tape
    ask Patches [set pcolor BlankColor]
    ;; Writes more backward than forward, so start a rowish from the top:
    ask turtles [setxy 0 max-pxcor - 1]
    ;; and go faster
    set RealTimePerTick 0.02
  ]

  ;; in all cases reset the machine state to 0:
  ask turtles [set MyState 0]
  set MachineState 0
  ;; and the ticks
  reset-ticks

end

```



## Nim

{{trans|Python}}

```nim
import strutils, tables

proc runUTM(state, halt, blank: string, tape: seq[string] = @[],
            rules: seq[seq[string]]) =
  var
    st = state
    pos = 0
    tape = tape
    rulesTable = initTable[tuple[s0, v0: string], tuple[v1, dr, s1: string]]()

  if tape.len == 0: tape = @[blank]
  if pos < 0: pos += tape.len
  assert pos in 0..tape.high

  for r in rules:
    assert r.len == 5
    rulesTable[(r[0], r[1])] = (r[2], r[3], r[4])

  while true:
    stdout.write st,'\t'
    for i, v in tape:
      if i == pos: stdout.write '[',v,']'
      else:        stdout.write ' ',v,' '
    echo ""

    if st == halt: break
    if not rulesTable.hasKey((st, tape[pos])): break

    let (v1, dr, s1) = rulesTable[(st, tape[pos])]
    tape[pos] = v1
    if dr == "left":
      if pos > 0: dec pos
      else: tape.insert blank
    if dr == "right":
      inc pos
      if pos >= tape.len: tape.add blank
    st = s1

echo "incr machine\n"
runUTM(halt  = "qf",
       state = "q0",
       tape  = "1 1 1".split,
       blank = "B",
       rules = @["q0 1 1 right q0".split,
                 "q0 B 1 stay  qf".split])

echo "\nbusy beaver\n"
runUTM(halt  = "halt",
       state = "a",
       blank = "0",
       rules = @["a 0 1 right b".split,
                 "a 1 1 left  c".split,
                 "b 0 1 left  a".split,
                 "b 1 1 right b".split,
                 "c 0 1 left  b".split,
                 "c 1 1 stay  halt".split])

echo "\nsorting test\n"
runUTM(halt  = "STOP",
       state = "A",
       blank = "0",
       tape  = "2 2 2 1 2 2 1 2 1 2 1 2 1 2".split,
       rules = @["A 1 1 right A".split,
                 "A 2 3 right B".split,
                 "A 0 0 left  E".split,
                 "B 1 1 right B".split,
                 "B 2 2 right B".split,
                 "B 0 0 left  C".split,
                 "C 1 2 left  D".split,
                 "C 2 2 left  C".split,
                 "C 3 2 left  E".split,
                 "D 1 1 left  D".split,
                 "D 2 2 left  D".split,
                 "D 3 1 right A".split,
                 "E 1 1 left  E".split,
                 "E 0 0 right STOP".split])
```




## Pascal

The code uses a stringlist to store the rules.
It also uses a stringlist to store the output.
It should straightforward to follow.

The 47,000,000 step busy beaver executes in 40 secs on my computer. For greater speed, it would be better to use a fixed size array to store the output (rather than a stringlist) when simulating Turing machines programs which have a large number of steps.


### Simple Incrementer


```txt

program project1;
uses
 Classes, sysutils;

type
 TCurrent = record
   state : string;
   input : char;
 end;

 TMovesTo = record
   state : string;
   output : char;
   moves : char;
 end;

 var
   ST, ET: TDateTime;
   C:TCurrent;
   M:TMovesTo;
   Tape, Rules:TStringList;
   TP:integer; //TP = Tape position
   Blank : char;
   j:integer;
   FinalState, InitialState, R : string;
   Count:integer;

Function ApplyRule(C:TCurrent):TMovesTo;
var
  x,k:integer;
begin
  //Find the appropriate rule and pass it as the result
  For k:= 0 to Rules.Count-1 do
  begin
     If (k mod 5 = 0) and (Rules[k] = C.state) and (Rules[k+1] = C.input) then
     begin
         Result.output := Rules[k+2][1];
         Result.moves := Rules[k+3][1];
         Result.state := Rules[k+4];
     end;
  end;
end;

Procedure ChangeTape(var TapePosition:integer;N:TMovesTo);
begin
   Tape[TapePosition]:=N.output;
   Case N.moves of
   'l':begin
         TapePosition := TapePosition-1;
       end;
   'r':begin
         TapePosition := TapePosition+1;
       end;
   end;
end;

function GetInput(TapePosition:integer):char;
begin
  Result:=Tape[TapePosition][1];
end;

procedure ShowResult;
var
  k:integer;
begin
  writeln('Current State  :',C.state);
  writeln('Input          :',C.input);
  write(' Tape ');
  For k:= 0 to Tape.count-1 do
  begin
     write(Tape[k]);
  end;
  writeln;
  writeln('New    State  :',M.state);
  writeln('Tape position :',TP);
  writeln('-----------------------');
end;

begin
  writeln('Universal Turing Machine');
  writeln('------------------------');
  Count:=0;
  ST:=Time;

  //Set up the rules
  Rules := TStringList.create;
  R := 'q0,1,1,right,q0,q0,B,1,stay,qf';
  Rules.CommaText := R;
  InitialState := 'q0';
  FinalState := 'qf';

  //Set up the tape
  Tape:=TStringList.create;
  Tape.add('1');
  Tape.add('1');
  Tape.add('1');
  Blank := 'B';
  For j:= 1 to 10 do
  begin
     Tape.add(Blank);
  end;

  //Set up the initial state
  writeln('Initial state');
  C.state:=InitialState;
  C.input:=' ';
  M.state:='';
  M.output:=' ';
  M.moves:=' ';
  TP:=0;

  //Run the machine
  While (TP >= 0) and (M.State <> FinalState) do
  begin
    C.Input := GetInput(TP);
    If M.state <> '' then
    begin
      C.State := M.State;
    end;
    M:=ApplyRule(C);
    ChangeTape(TP,M);
    Count:=Count+1;
    ShowResult;
  end;

  //State the outcome.
  If TP < 0 then
  begin
     writeln('Error! Tape has slipped off at left!');
  end;
  If M.state = FinalState then
  begin
     writeln('Program has finished');
     ET:=Time;
     writeln('Time taken ');
     writeln(FormatDateTime('sss:zzz',ET-ST));
     writeln(Count, ' steps taken');
  end;

  Tape.free;
  Rules.free;
  readln;
end.

```



Output

```txt

Universal Turing Machine
------------------------
Initial state
Current State  :q0
Input          :1
 Tape 111BBBBBBBBBB
New    State  :q0
Tape position :1
-----------------------
Current State  :q0
Input          :1
 Tape 111BBBBBBBBBB
New    State  :q0
Tape position :2
-----------------------
Current State  :q0
Input          :1
 Tape 111BBBBBBBBBB
New    State  :q0
Tape position :3
-----------------------
Current State  :q0
Input          :B
 Tape 1111BBBBBBBBB
New    State  :qf
Tape position :3
-----------------------
Program has finished
Time taken
00:000
4 steps taken


```



### 3 state Busy Beaver


```txt

program project1;
uses
 Classes, sysutils;

type
 TCurrent = record
   state : string;
   input : char;
 end;

 TMovesTo = record
   state : string;
   output : char;
   moves : char;
 end;

 var
   ST, ET: TDateTime;
   C:TCurrent;
   M:TMovesTo;
   Tape, Rules:TStringList;
   TP:integer; //TP = Tape position
   Blank : char;
   j:integer;
   FinalState, InitialState, R : string;
   Count:integer;

Function ApplyRule(C:TCurrent):TMovesTo;
var
  x,k:integer;
begin
  //Find the appropriate rule and pass it as the result
  For k:= 0 to Rules.Count-1 do
  begin
     If (k mod 5 = 0) and (Rules[k] = C.state) and (Rules[k+1] = C.input) then
     begin
         Result.output := Rules[k+2][1];
         Result.moves := Rules[k+3][1];
         Result.state := Rules[k+4];
     end;
  end;
end;

Procedure ChangeTape(var TapePosition:integer;N:TMovesTo);
begin
   Tape[TapePosition]:=N.output;
   Case N.moves of
   'l':begin
         TapePosition := TapePosition-1;
       end;
   'r':begin
         TapePosition := TapePosition+1;
       end;
   end;
end;

function GetInput(TapePosition:integer):char;
begin
  Result:=Tape[TapePosition][1];
end;

procedure ShowResult;
var
  k:integer;
begin
  writeln('Current State  :',C.state);
  writeln('Input          :',C.input);
  write(' Tape ');
  For k:= 0 to Tape.count-1 do
  begin
     write(Tape[k]);
  end;
  writeln;
  writeln('New    State  :',M.state);
  writeln('Tape position :',TP);
  writeln('-----------------------');
end;

begin
  writeln('Universal Turing Machine');
  writeln('------------------------');
  Count:=0;
  ST:=Time;

  //Set up the rules
  Rules := TStringList.create;
  R:= 'a,0,1,right,b,a,1,1,left,c,b,0,1,left,a,b,1,1,right,b,c,0,1,left,b,c,1,1,stay,halt';
  Rules.CommaText := R;

  //Set up the state names
  InitialState := 'a';
  FinalState := 'halt';

  //Set up the tape
  Blank := '0';
  Tape:=TStringList.create;
  For j:= 1 to 20 do
  begin
     Tape.add(Blank);
  end;

  //Set up the initial state
  writeln('Initial state');
  C.state:=InitialState;
  C.input:=' ';
  M.state:='';
  M.output:=' ';
  M.moves:=' ';
  TP:=10;

  //Run the machine
  While (TP >= 0) and (M.State <> FinalState) do
  begin
    C.Input := GetInput(TP);
    If M.state <> '' then
    begin
      C.State := M.State;
    end;
    M:=ApplyRule(C);
    ChangeTape(TP,M);
    Count:=Count+1;
    ShowResult;
  end;

  //State the outcome.
  If TP < 0 then
  begin
     writeln('Error! Tape has slipped off at left!');
  end;
  If M.state = FinalState then
  begin
     writeln('Program has finished');
     ET:=Time;
     writeln('Time taken ');
     writeln(FormatDateTime('sss:zzz',ET-ST));
     writeln(Count, ' steps taken');
  end;

  Tape.free;
  Rules.free;
  readln;
end.

```


output

```txt

Universal Turing Machine
------------------------
Initial state
Current State  :a
Input          :0
 Tape 00000000001000000000
New    State  :b
Tape position :11
-----------------------
Current State  :b
Input          :0
 Tape 00000000001100000000
New    State  :a
Tape position :10
-----------------------
Current State  :a
Input          :1
 Tape 00000000001100000000
New    State  :c
Tape position :9
-----------------------
Current State  :c
Input          :0
 Tape 00000000011100000000
New    State  :b
Tape position :8
-----------------------
Current State  :b
Input          :0
 Tape 00000000111100000000
New    State  :a
Tape position :7
-----------------------
Current State  :a
Input          :0
 Tape 00000001111100000000
New    State  :b
Tape position :8
-----------------------
Current State  :b
Input          :1
 Tape 00000001111100000000
New    State  :b
Tape position :9
-----------------------
Current State  :b
Input          :1
 Tape 00000001111100000000
New    State  :b
Tape position :10
-----------------------
Current State  :b
Input          :1
 Tape 00000001111100000000
New    State  :b
Tape position :11
-----------------------
Current State  :b
Input          :1
 Tape 00000001111100000000
New    State  :b
Tape position :12
-----------------------
Current State  :b
Input          :0
 Tape 00000001111110000000
New    State  :a
Tape position :11
-----------------------
Current State  :a
Input          :1
 Tape 00000001111110000000
New    State  :c
Tape position :10
-----------------------
Current State  :c
Input          :1
 Tape 00000001111110000000
New    State  :halt
Tape position :10
-----------------------
Program has finished
Time taken
00:001
13 steps taken

```



### 5 state 2 symbol


```txt

//Busy Beaver 5 state 2-symbol

program project1;
uses
 Classes, sysutils;

type

 TCurrent = record
   state : string;
   input : char;
 end;

 TMovesTo = record
   state : string;
   output : char;
   moves : char;
 end;

 var
   C:TCurrent;
   M:TMovesTo;
   Tape, Rules : TStringList;
   TP:integer; // TP = Tape position
   Blank : char;
   FinalState , InitialState, R: string;
   ST, ET: TDateTime;
   j, Count:integer;

Function ApplyRule(C:TCurrent):TMovesTo;
var
  x,k:integer;
begin
  //Find the appropriate rule (given the state and input) and get the result
  For k:= 0 to Rules.Count-1 do
  begin
     If (k mod 5 = 0) and (Rules[k] = C.state) and (Rules[k+1] = C.input) then
     begin
        with result do
        begin
           output := Rules[k+2][1];
           moves := Rules[k+3][1];
           state := Rules[k+4];
        end;
     end;
  end;
end;

Procedure ChangeTape(var TapePosition:integer; N:TMovesTo);
begin
   Tape[TapePosition]:=N.output;
   Case N.moves of
   'l':begin
         TapePosition := TapePosition-1;
       end;
   'r':begin
         TapePosition := TapePosition+1;
       end;
   end;
end;

function GetInput(TapePosition:integer):char;
begin
  Result:=Tape[TapePosition][1];
end;

procedure ShowResult;
var
  k:integer;
begin
  writeln('Current State  :',C.state);
  writeln('Input          :',C.input);
  write(' Tape ');
  For k:= 0 to Tape.count-1 do
  begin
     write(Tape[k]);
  end;
  writeln;
  writeln('New    State  :',M.state);
  writeln('Tape position :',TP);
  writeln('-----------------------');
end;

begin
  writeln('Universal Turing Machine');
  writeln('------------------------');

  //Number of steos and time taken
  Count:=0;
  ST:=Time;

  //Set up the rules
  Rules := TStringList.create;
  R:='A, 0, 1, right, B,A, 1, 1, left, C, B, 0, 1, right, C,B, 1, 1, right, B,C, 0, 1, right, D,C, 1, 0, left, E,D, 0, 1, left, A,D, 1, 1, left, D,E, 0, 1, stay, H,E, 1, 0, left, A';
  Rules.CommaText := R;
  InitialState := 'A';
  FinalState := 'H';
  Blank := '0';

  //Set up the blank tape
  Tape:=TStringList.create;
  For j:= 1 to  15000 do
  begin
       Tape.add(Blank);
  end;

  //Set up the initial state
  //I discovered by trial and error a suitable start point (TP)
  writeln('Initial state');
  C.state:=InitialState;
  C.input:=' ';
  M.state:='';
  M.output:=' ';
  M.moves:=' ';
  TP:=14000;


  //Run the machine
  While (TP >= 0) and (M.State <> FinalState) do
  begin
    C.Input := GetInput(TP);
    If M.state <> '' then
    begin
      C.State := M.State;
    end;
    M := ApplyRule(C);
    ChangeTape(TP,M);
    Count:=Count+1;
  end;

  //State the outcome.
  If TP < 0 then
  begin
     writeln('Error! Tape has slipped off at left!');
  end;
  If M.state = FinalState then
  begin
     ShowResult;
     writeln('Program has finished');
     ET:=Time;
     writeln('Time taken ');
     writeln(FormatDateTime('sss:zzz',ET-ST));
     writeln(Count, ' steps taken');
  end;

  Tape.free;
  Rules.free;
  readln;
end.

```


Output

```txt

New    State  :H
Tape position :1757
-----------------------
Program has finished
Time taken
40:856
47176870 steps taken

```


The tape looks like this :
1010010010010010010010010010010010010010010 ...


## Perl


```perl
use strict;
use warnings;

sub run_utm {
	my %o = @_;
	my $st = $o{state}	// die "init head state undefined";
	my $blank = $o{blank}	// die "blank symbol undefined";
	my @rules = @{$o{rules}} or die "rules undefined";
	my @tape = $o{tape} ? @{$o{tape}} : ($blank);
	my $halt = $o{halt};

	my $pos = $o{pos} // 0;
	$pos += @tape if $pos < 0;
	die "bad init position" if $pos >= @tape || $pos < 0;

step:	while (1) {
		print "$st\t";
		for (0 .. $#tape) {
			my $v = $tape[$_];
			print $_ == $pos ? "[$v]" : " $v ";
		}
		print "\n";

		last if $st eq $halt;
		for (@rules) {
			my ($s0, $v0, $v1, $dir, $s1) = @$_;
			next unless $s0 eq $st and $tape[$pos] eq $v0;

			$tape[$pos] = $v1;

			if ($dir eq 'left') {
				if ($pos == 0) { unshift @tape, $blank}
				else { $pos-- }
			} elsif ($dir eq 'right') {
				push @tape, $blank if ++$pos >= @tape
			}

			$st = $s1;
			next step;
		}

		die "no matching rules";
	}
}

print "incr machine\n";
run_utm	halt=>'qf',
	state=>'q0',
	tape=>[1,1,1],
	blank=>'B',
	rules=>[[qw/q0 1 1 right q0/],
		[qw/q0 B 1 stay  qf/]];

print "\nbusy beaver\n";
run_utm halt=>'halt',
	state=>'a',
	blank=>'0',
	rules=>[[qw/a 0 1 right b/],
		[qw/a 1 1 left  c/],
		[qw/b 0 1 left  a/],
		[qw/b 1 1 right b/],
		[qw/c 0 1 left  b/],
		[qw/c 1 1 stay  halt/]];

print "\nsorting test\n";
run_utm halt=>'STOP',
	state=>'A',
	blank=>'0',
	tape=>[qw/2 2 2 1 2 2 1 2 1 2 1 2 1 2/],
	rules=>[[qw/A 1 1 right A/],
		[qw/A 2 3 right B/],
		[qw/A 0 0 left  E/],
		[qw/B 1 1 right B/],
		[qw/B 2 2 right B/],
		[qw/B 0 0 left  C/],
		[qw/C 1 2 left  D/],
		[qw/C 2 2 left  C/],
		[qw/C 3 2 left  E/],
		[qw/D 1 1 left  D/],
		[qw/D 2 2 left  D/],
		[qw/D 3 1 right A/],
		[qw/E 1 1 left  E/],
		[qw/E 0 0 right STOP/]];
```


## Perl 6

{{trans|Perl}}
{{works with|Rakudo|2018.03}}

```perl6
sub run_utm(:$state! is copy, :$blank!, :@rules!, :@tape = [$blank], :$halt, :$pos is copy = 0) {
    $pos += @tape if $pos < 0;
    die "Bad initial position" unless $pos ~~ ^@tape;

STEP: loop {
        print "$state\t";
        for ^@tape {
            my $v = @tape[$_];
            print $_ == $pos ?? "[$v]" !! " $v ";
        }
        print "\n";

        last if $state eq $halt;

        for @rules -> @rule {
            my ($s0, $v0, $v1, $dir, $s1) = @rule;
            next unless $s0 eq $state and @tape[$pos] eq $v0;

            @tape[$pos] = $v1;

            given $dir {
                when 'left' {
                    if $pos == 0 { unshift @tape, $blank }
                    else { $pos-- }
                }
                when 'right' {
                    push @tape, $blank if ++$pos >= @tape;
                }
            }

            $state = $s1;
            next STEP;

        }
        die 'No matching rules';
    }

}

say "incr machine";
run_utm	:halt<qf>,
    :state<q0>,
    :tape[1,1,1],
    :blank<B>,
    :rules[
        [< q0 1 1 right q0 >],
        [< q0 B 1 stay  qf >]
    ];

say "\nbusy beaver";
run_utm :halt<halt>,
    :state<a>,
    :blank<0>,
    :rules[
        [< a 0 1 right b >],
        [< a 1 1 left  c >],
        [< b 0 1 left  a >],
        [< b 1 1 right b >],
        [< c 0 1 left  b >],
        [< c 1 1 stay  halt >]
    ];

say "\nsorting test";
run_utm :halt<STOP>,
    :state<A>,
    :blank<0>,
    :tape[< 2 2 2 1 2 2 1 2 1 2 1 2 1 2 >],
    :rules[
        [< A 1 1 right A >],
        [< A 2 3 right B >],
        [< A 0 0 left  E >],
        [< B 1 1 right B >],
        [< B 2 2 right B >],
        [< B 0 0 left  C >],
        [< C 1 2 left  D >],
        [< C 2 2 left  C >],
        [< C 3 2 left  E >],
        [< D 1 1 left  D >],
        [< D 2 2 left  D >],
        [< D 3 1 right A >],
        [< E 1 1 left  E >],
        [< E 0 0 right STOP >]
    ];

```


{{out}}

```txt
incr machine
q0	[1] 1  1
q0	 1 [1] 1
q0	 1  1 [1]
q0	 1  1  1 [B]
qf	 1  1  1 [1]

busy beaver
a	[0]
b	 1 [0]
a	[1] 1
c	[0] 1  1
b	[0] 1  1  1
a	[0] 1  1  1  1
b	 1 [1] 1  1  1
b	 1  1 [1] 1  1
b	 1  1  1 [1] 1
b	 1  1  1  1 [1]
b	 1  1  1  1  1 [0]
a	 1  1  1  1 [1] 1
c	 1  1  1 [1] 1  1
halt	 1  1  1 [1] 1  1

sorting test
A	[2] 2  2  1  2  2  1  2  1  2  1  2  1  2
B	 3 [2] 2  1  2  2  1  2  1  2  1  2  1  2
B	 3  2 [2] 1  2  2  1  2  1  2  1  2  1  2
B	 3  2  2 [1] 2  2  1  2  1  2  1  2  1  2
B	 3  2  2  1 [2] 2  1  2  1  2  1  2  1  2
B	 3  2  2  1  2 [2] 1  2  1  2  1  2  1  2
B	 3  2  2  1  2  2 [1] 2  1  2  1  2  1  2
B	 3  2  2  1  2  2  1 [2] 1  2  1  2  1  2
B	 3  2  2  1  2  2  1  2 [1] 2  1  2  1  2
B	 3  2  2  1  2  2  1  2  1 [2] 1  2  1  2
B	 3  2  2  1  2  2  1  2  1  2 [1] 2  1  2
B	 3  2  2  1  2  2  1  2  1  2  1 [2] 1  2
B	 3  2  2  1  2  2  1  2  1  2  1  2 [1] 2
B	 3  2  2  1  2  2  1  2  1  2  1  2  1 [2]
B	 3  2  2  1  2  2  1  2  1  2  1  2  1  2 [0]
C	 3  2  2  1  2  2  1  2  1  2  1  2  1 [2] 0
C	 3  2  2  1  2  2  1  2  1  2  1  2 [1] 2  0
D	 3  2  2  1  2  2  1  2  1  2  1 [2] 2  2  0
D	 3  2  2  1  2  2  1  2  1  2 [1] 2  2  2  0
D	 3  2  2  1  2  2  1  2  1 [2] 1  2  2  2  0
D	 3  2  2  1  2  2  1  2 [1] 2  1  2  2  2  0
D	 3  2  2  1  2  2  1 [2] 1  2  1  2  2  2  0
D	 3  2  2  1  2  2 [1] 2  1  2  1  2  2  2  0
D	 3  2  2  1  2 [2] 1  2  1  2  1  2  2  2  0
D	 3  2  2  1 [2] 2  1  2  1  2  1  2  2  2  0
D	 3  2  2 [1] 2  2  1  2  1  2  1  2  2  2  0
D	 3  2 [2] 1  2  2  1  2  1  2  1  2  2  2  0
D	 3 [2] 2  1  2  2  1  2  1  2  1  2  2  2  0
D	[3] 2  2  1  2  2  1  2  1  2  1  2  2  2  0
A	 1 [2] 2  1  2  2  1  2  1  2  1  2  2  2  0
B	 1  3 [2] 1  2  2  1  2  1  2  1  2  2  2  0
B	 1  3  2 [1] 2  2  1  2  1  2  1  2  2  2  0
B	 1  3  2  1 [2] 2  1  2  1  2  1  2  2  2  0
B	 1  3  2  1  2 [2] 1  2  1  2  1  2  2  2  0
B	 1  3  2  1  2  2 [1] 2  1  2  1  2  2  2  0
B	 1  3  2  1  2  2  1 [2] 1  2  1  2  2  2  0
B	 1  3  2  1  2  2  1  2 [1] 2  1  2  2  2  0
B	 1  3  2  1  2  2  1  2  1 [2] 1  2  2  2  0
B	 1  3  2  1  2  2  1  2  1  2 [1] 2  2  2  0
B	 1  3  2  1  2  2  1  2  1  2  1 [2] 2  2  0
B	 1  3  2  1  2  2  1  2  1  2  1  2 [2] 2  0
B	 1  3  2  1  2  2  1  2  1  2  1  2  2 [2] 0
B	 1  3  2  1  2  2  1  2  1  2  1  2  2  2 [0]
C	 1  3  2  1  2  2  1  2  1  2  1  2  2 [2] 0
C	 1  3  2  1  2  2  1  2  1  2  1  2 [2] 2  0
C	 1  3  2  1  2  2  1  2  1  2  1 [2] 2  2  0
C	 1  3  2  1  2  2  1  2  1  2 [1] 2  2  2  0
D	 1  3  2  1  2  2  1  2  1 [2] 2  2  2  2  0
D	 1  3  2  1  2  2  1  2 [1] 2  2  2  2  2  0
D	 1  3  2  1  2  2  1 [2] 1  2  2  2  2  2  0
D	 1  3  2  1  2  2 [1] 2  1  2  2  2  2  2  0
D	 1  3  2  1  2 [2] 1  2  1  2  2  2  2  2  0
D	 1  3  2  1 [2] 2  1  2  1  2  2  2  2  2  0
D	 1  3  2 [1] 2  2  1  2  1  2  2  2  2  2  0
D	 1  3 [2] 1  2  2  1  2  1  2  2  2  2  2  0
D	 1 [3] 2  1  2  2  1  2  1  2  2  2  2  2  0
A	 1  1 [2] 1  2  2  1  2  1  2  2  2  2  2  0
B	 1  1  3 [1] 2  2  1  2  1  2  2  2  2  2  0
B	 1  1  3  1 [2] 2  1  2  1  2  2  2  2  2  0
B	 1  1  3  1  2 [2] 1  2  1  2  2  2  2  2  0
B	 1  1  3  1  2  2 [1] 2  1  2  2  2  2  2  0
B	 1  1  3  1  2  2  1 [2] 1  2  2  2  2  2  0
B	 1  1  3  1  2  2  1  2 [1] 2  2  2  2  2  0
B	 1  1  3  1  2  2  1  2  1 [2] 2  2  2  2  0
B	 1  1  3  1  2  2  1  2  1  2 [2] 2  2  2  0
B	 1  1  3  1  2  2  1  2  1  2  2 [2] 2  2  0
B	 1  1  3  1  2  2  1  2  1  2  2  2 [2] 2  0
B	 1  1  3  1  2  2  1  2  1  2  2  2  2 [2] 0
B	 1  1  3  1  2  2  1  2  1  2  2  2  2  2 [0]
C	 1  1  3  1  2  2  1  2  1  2  2  2  2 [2] 0
C	 1  1  3  1  2  2  1  2  1  2  2  2 [2] 2  0
C	 1  1  3  1  2  2  1  2  1  2  2 [2] 2  2  0
C	 1  1  3  1  2  2  1  2  1  2 [2] 2  2  2  0
C	 1  1  3  1  2  2  1  2  1 [2] 2  2  2  2  0
C	 1  1  3  1  2  2  1  2 [1] 2  2  2  2  2  0
D	 1  1  3  1  2  2  1 [2] 2  2  2  2  2  2  0
D	 1  1  3  1  2  2 [1] 2  2  2  2  2  2  2  0
D	 1  1  3  1  2 [2] 1  2  2  2  2  2  2  2  0
D	 1  1  3  1 [2] 2  1  2  2  2  2  2  2  2  0
D	 1  1  3 [1] 2  2  1  2  2  2  2  2  2  2  0
D	 1  1 [3] 1  2  2  1  2  2  2  2  2  2  2  0
A	 1  1  1 [1] 2  2  1  2  2  2  2  2  2  2  0
A	 1  1  1  1 [2] 2  1  2  2  2  2  2  2  2  0
B	 1  1  1  1  3 [2] 1  2  2  2  2  2  2  2  0
B	 1  1  1  1  3  2 [1] 2  2  2  2  2  2  2  0
B	 1  1  1  1  3  2  1 [2] 2  2  2  2  2  2  0
B	 1  1  1  1  3  2  1  2 [2] 2  2  2  2  2  0
B	 1  1  1  1  3  2  1  2  2 [2] 2  2  2  2  0
B	 1  1  1  1  3  2  1  2  2  2 [2] 2  2  2  0
B	 1  1  1  1  3  2  1  2  2  2  2 [2] 2  2  0
B	 1  1  1  1  3  2  1  2  2  2  2  2 [2] 2  0
B	 1  1  1  1  3  2  1  2  2  2  2  2  2 [2] 0
B	 1  1  1  1  3  2  1  2  2  2  2  2  2  2 [0]
C	 1  1  1  1  3  2  1  2  2  2  2  2  2 [2] 0
C	 1  1  1  1  3  2  1  2  2  2  2  2 [2] 2  0
C	 1  1  1  1  3  2  1  2  2  2  2 [2] 2  2  0
C	 1  1  1  1  3  2  1  2  2  2 [2] 2  2  2  0
C	 1  1  1  1  3  2  1  2  2 [2] 2  2  2  2  0
C	 1  1  1  1  3  2  1  2 [2] 2  2  2  2  2  0
C	 1  1  1  1  3  2  1 [2] 2  2  2  2  2  2  0
C	 1  1  1  1  3  2 [1] 2  2  2  2  2  2  2  0
D	 1  1  1  1  3 [2] 2  2  2  2  2  2  2  2  0
D	 1  1  1  1 [3] 2  2  2  2  2  2  2  2  2  0
A	 1  1  1  1  1 [2] 2  2  2  2  2  2  2  2  0
B	 1  1  1  1  1  3 [2] 2  2  2  2  2  2  2  0
B	 1  1  1  1  1  3  2 [2] 2  2  2  2  2  2  0
B	 1  1  1  1  1  3  2  2 [2] 2  2  2  2  2  0
B	 1  1  1  1  1  3  2  2  2 [2] 2  2  2  2  0
B	 1  1  1  1  1  3  2  2  2  2 [2] 2  2  2  0
B	 1  1  1  1  1  3  2  2  2  2  2 [2] 2  2  0
B	 1  1  1  1  1  3  2  2  2  2  2  2 [2] 2  0
B	 1  1  1  1  1  3  2  2  2  2  2  2  2 [2] 0
B	 1  1  1  1  1  3  2  2  2  2  2  2  2  2 [0]
C	 1  1  1  1  1  3  2  2  2  2  2  2  2 [2] 0
C	 1  1  1  1  1  3  2  2  2  2  2  2 [2] 2  0
C	 1  1  1  1  1  3  2  2  2  2  2 [2] 2  2  0
C	 1  1  1  1  1  3  2  2  2  2 [2] 2  2  2  0
C	 1  1  1  1  1  3  2  2  2 [2] 2  2  2  2  0
C	 1  1  1  1  1  3  2  2 [2] 2  2  2  2  2  0
C	 1  1  1  1  1  3  2 [2] 2  2  2  2  2  2  0
C	 1  1  1  1  1  3 [2] 2  2  2  2  2  2  2  0
C	 1  1  1  1  1 [3] 2  2  2  2  2  2  2  2  0
E	 1  1  1  1 [1] 2  2  2  2  2  2  2  2  2  0
E	 1  1  1 [1] 1  2  2  2  2  2  2  2  2  2  0
E	 1  1 [1] 1  1  2  2  2  2  2  2  2  2  2  0
E	 1 [1] 1  1  1  2  2  2  2  2  2  2  2  2  0
E	[1] 1  1  1  1  2  2  2  2  2  2  2  2  2  0
E	[0] 1  1  1  1  1  2  2  2  2  2  2  2  2  2  0
STOP	 0 [1] 1  1  1  1  2  2  2  2  2  2  2  2  2  0
```



## Phix

{{trans|Lua}}

```Phix
enum name, initState, endState, blank, rules

-- Machine definitions
constant incrementer = {
    /*name =*/ "Simple incrementer",
    /*initState =*/ "q0",
    /*endState =*/ "qf",
    /*blank =*/ "B",
    /*rules =*/ {
        {"q0", "1", "1", "right", "q0"},
        {"q0", "B", "1", "stay", "qf"}
    }
}

constant threeStateBB = {
    /*name =*/ "Three-state busy beaver",
    /*initState =*/ "a",
    /*endState =*/ "halt",
    /*blank =*/ "0",
    /*rules =*/ {
        {"a", "0", "1", "right", "b"},
        {"a", "1", "1", "left", "c"},
        {"b", "0", "1", "left", "a"},
        {"b", "1", "1", "right", "b"},
        {"c", "0", "1", "left", "b"},
        {"c", "1", "1", "stay", "halt"}
    }
}

constant fiveStateBB = {
    /*name =*/ "Five-state busy beaver",
    /*initState =*/ "A",
    /*endState =*/ "H",
    /*blank =*/ "0",
    /*rules =*/ {
        {"A", "0", "1", "right", "B"},
        {"A", "1", "1", "left", "C"},
        {"B", "0", "1", "right", "C"},
        {"B", "1", "1", "right", "B"},
        {"C", "0", "1", "right", "D"},
        {"C", "1", "0", "left", "E"},
        {"D", "0", "1", "left", "A"},
        {"D", "1", "1", "left", "D"},
        {"E", "0", "1", "stay", "H"},
        {"E", "1", "0", "left", "A"}
    }
}

procedure show(string state, integer headpos, sequence tape)
    printf(1," %-6s | ",{state})
    for p=1 to length(tape) do
        printf(1,iff(p=headpos?"[%s]":" %s "),{tape[p]})
    end for
    printf(1,"\n")
end procedure

-- a universal turing machine
procedure UTM(sequence machine, sequence tape, integer countOnly=0)
string state = machine[initState]
integer headpos = 1, counter = 0
    printf(1,"\n\n%s\n%s\n",{machine[name],repeat('=',length(machine[name]))})
    if not countOnly then printf(1," State  | Tape [head]\n---------------------\n") end if
    while 1 do
        if headpos>length(tape) then
            tape = append(tape,machine[blank])
        elsif headpos<1 then
            tape = prepend(tape,machine[blank])
            headpos = 1
        end if
        if not countOnly then show(state, headpos, tape) end if
        for i=1 to length(machine[rules]) do
            sequence rule = machine[rules][i]
            if rule[1]=state and rule[2]=tape[headpos] then
                tape[headpos] = rule[3]
                if rule[4] == "left" then headpos -= 1 end if
                if rule[4] == "right" then headpos += 1 end if
                state = rule[5]
                exit
            end if
        end for
        counter += 1
        if state=machine[endState] then exit end if
    end while
    if countOnly then
        printf(1,"Steps taken: %d\n",{counter})
    else
        show(state, headPos, tape)
    end if
end procedure

UTM(incrementer, {"1", "1", "1"})
UTM(threeStateBB, {})
UTM(fiveStateBB, {}, countOnly:=1)
```

{{Out}}

```txt

Simple incrementer

### ============

 State  | Tape [head]
---------------------
 q0     | [1] 1  1
 q0     |  1 [1] 1
 q0     |  1  1 [1]
 q0     |  1  1  1 [B]
 qf     |  1  1  1 [1]


Three-state busy beaver

### =================

 State  | Tape [head]
---------------------
 a      | [0]
 b      |  1 [0]
 a      | [1] 1
 c      | [0] 1  1
 b      | [0] 1  1  1
 a      | [0] 1  1  1  1
 b      |  1 [1] 1  1  1
 b      |  1  1 [1] 1  1
 b      |  1  1  1 [1] 1
 b      |  1  1  1  1 [1]
 b      |  1  1  1  1  1 [0]
 a      |  1  1  1  1 [1] 1
 c      |  1  1  1 [1] 1  1
 halt   |  1  1  1 [1] 1  1


Five-state busy beaver

### ================

Steps taken: 47176870

```



## PicoLisp


```PicoLisp
# Finite state machine
(de turing (Tape Init Halt Blank Rules Verbose)
   (let
      (Head 1
         State Init
         Rule NIL
         S 'start
         C (length Tape))
      (catch NIL
         (loop
            (state 'S
               (start 'print
                  (when (=0 C)
                     (setq Tape (insert Head Tape Blank))
                     (inc 'C) ) )
               (print 'lookup
                  (when Verbose
                     (for (N . I) Tape
                        (if (= N Head)
                           (print (list I))
                           (prin I) ) )
                     (prinl) )
                  (when (= State Halt) (throw NIL) ) )
               (lookup 'do
                  (setq Rule
                     (find
                        '((X)
                           (and
                              (= (car X) State)
                              (= (cadr X) (car (nth Tape Head))) ) )
                        Rules ) ) )
               (do 'step
                  (setq Tape (place Head Tape (caddr Rule))) )
               (step 'print
                  (cond
                     ((= (cadddr Rule) 'R) (inc 'Head))
                     ((= (cadddr Rule) 'L) (dec 'Head)) )
                  (cond
                     ((< Head 1)
                        (setq Tape (insert Head Tape Blank))
                        (inc 'C)
                        (one Head) )
                     ((> Head C)
                        (setq Tape (insert Head Tape Blank))
                        (inc 'C) ) )
                  (setq State (last Rule)) ) ) ) ) )
   Tape )

(println "Simple incrementer")
(turing '(1 1 1) 'A 'H 'B '((A 1 1 R A) (A B 1 S H)) T)

(println "Three-state busy beaver")
(turing '() 'A 'H 0
   '((A 0 1 R B)
     (A 1 1 L C)
     (B 0 1 L A)
     (B 1 1 R B)
     (C 0 1 L B)
     (C 1 1 S H)) T )

(println "Five-state busy beaver")
(let Tape (turing '() 'A 'H 0
   '((A 0 1 R B)
     (A 1 1 L C)
     (B 0 1 R C)
     (B 1 1 R B)
     (C 0 1 R D)
     (C 1 0 L E)
     (D 0 1 L A)
     (D 1 1 L D)
     (E 0 1 S H)
     (E 1 0 L A)) NIL)
   (println '0s: (cnt '((X) (= 0 X)) Tape))
   (println '1s: (cnt '((X) (= 1 X)) Tape)) )

(bye)
```

{{out}}

```txt
"Simple incrementer"
(1)11
1(1)1
11(1)
111(B)
111(1)
"Three-state busy beaver"
(0)
1(0)
(1)1
(0)11
(0)111
(0)1111
1(1)111
11(1)11
111(1)1
1111(1)
11111(0)
1111(1)1
111(1)11
111(1)11
"Five-state busy beaver"
0s: 8191
1s: 4098
```



## PHL



```phl
module turing;

extern printf;

struct @Command {
	field @Integer tape {get:tape,set:stape};
	field @Integer move {get:move,set:smove};
	field @Integer next {get:next,set:snext};

	@Command init(@Integer tape, @Integer move, @Integer next) [
		this.stape(tape);
		this.smove(move);
		this.snext(next);
		return this;
	]
};

doc 2 dimansional array structure;

struct @Rules {

	field @Integer maxstates { get: maxstates, set: smaxstates };
	field @Integer maxvalue  { get: maxvalue, set: smaxvalue };

	field @Array<@Array<@Command> > table {get: t, set: st};

	@Rules init(@Integer states, @Integer values)
	[
		this.smaxstates(states);
		this.smaxvalue(values);
		this.st(new @Array<@Array<@Command> >.init(states));
		return this;
	]

	@Void setRule(@Integer state, @Integer tape, @Command command)
	[
		if (null == this::t.get(state)) {
			this::t.set(state, new @Array<@Command>.init(this::maxvalue));
		}
		this::t.get(state).set(tape, command);
	]

	@Command getRule(@Integer state, @Integer tape)
	[
		return this::t.get(state).get(tape);
	]

};

@Void emulateTuring(@Rules rules, @Integer start, @Integer stop, @Array<@Integer> tape, @Integer blank) [
	var tapepointer = 0;
	var state = start;

	doc output;
	printf("Tape\tState\n");

	while (state != stop) {
		doc add more cells to the tape;
		if (tapepointer == tape::size) tape.add(blank);
		if (tapepointer == 0-1) { tape = (new @Array<@Integer>..blank).addAll(tape); tapepointer = 0; }

		doc output;
		for (var i = 0; i < tape::size; i=i+1) {
			printf("%i", tape.get(i));
		}
		printf("\t%i\n", state);
		for (var i = 0; i < tapepointer; i=i+1) {
			printf(" ");
		}
		printf("^\n");

		doc the value of the current cell;
		var tapeval = tape.get(tapepointer);

		doc the current state;
		var command = rules.getRule(state, tapeval);

		tape.set(tapepointer, command::tape);
		tapepointer = tapepointer + command::move;
		state = command::next;
	}

	doc output;
	for (var i = 0; i < tape::size; i=i+1) {
		printf("%i", tape.get(i));
	}
	printf("\t%i\n", state);
	for (var i = 0; i < tapepointer; i=i+1) {
		printf(" ");
	}
	printf("^\n");
]

@Integer main [

	doc incrementer;

	doc 2 states, 2 symbols;

	var rules = new @Rules.init(2, 2);

	doc q0, 1 -> 1, right, q0;
	doc q0, B -> 1, stay, qf;

	rules.setRule(0, 1, new @Command.init(1, 1, 0));
	rules.setRule(0, 0, new @Command.init(1, 0, 1));

	doc tape = [1, 1, 1];

	var tape = new @Array<@Integer>..1..1..1;

	doc start turing machine;

	emulateTuring(rules, 0, 1, tape, 0);

	doc ---------------------------------------------------;

	doc three state busy beaver;

	doc 4 states, 2 symbols;

	rules = new @Rules.init(4, 2);

	doc	a, 0 -> 1, right, b
		a, 1 -> 1, left, c
		b, 0 -> 1, left, a
		b, 1 -> 1, right, b
		c, 0 -> 1, left, b
		c, 1 -> 1, stay, halt
	;

	doc	a = 0,
		b = 1,
		c = 2,
		halt = 3;

	rules.setRule(0, 0, new @Command.init(1, 1, 1));
	rules.setRule(0, 1, new @Command.init(1, 0-1, 2));
	rules.setRule(1, 0, new @Command.init(1, 0-1, 0));
	rules.setRule(1, 1, new @Command.init(1, 1, 1));
	rules.setRule(2, 0, new @Command.init(1, 0-1, 1));
	rules.setRule(2, 1, new @Command.init(1, 0, 3));

	doc tape = [];

	tape = new @Array<@Integer>;

	doc start turing machine;

	emulateTuring(rules, 0, 3, tape, 0);
	return 0;
]
```


Output:

 Tape	State
 111	0
 ^
 111	0
  ^
 111	0
   ^
 1110	0
    ^
 1111	1
    ^
 Tape	State
 0	0
 ^
 10	1
  ^
 11	0
 ^
 011	2
 ^
 0111	1
 ^
 01111	0
 ^
 11111	1
  ^
 11111	1
   ^
 11111	1
    ^
 11111	1
     ^
 111110	1
      ^
 111111	0
     ^
 111111	2
    ^
 111111	3
    ^


## Prolog


### The universal machine

Source for this example was lightly adapted from [https://bitbucket.org/ttmrichter/turing https://bitbucket.org/ttmrichter/turing].  This machine, because of Prolog's dynamic nature, has to check its configuration and the rules' compliance to the same at run-time.  This is the role of all but the first of the <code>memberchk/2</code> predicates.  In addition, calling the user-supplied rules has to be wrapped in a <code>once/1</code> wrapper because there is no way to guarantee in advance that the rules provided are deterministic.  (An alternative to doing this is to simply allow <code>perform/5</code> to be non-deterministic or to check for multiple results and report an error on such.)

```prolog
turing(Config, Rules, TapeIn, TapeOut) :-
    call(Config, IS, _, _, _, _),
    perform(Config, Rules, IS, {[], TapeIn}, {Ls, Rs}),
    reverse(Ls, Ls1),
    append(Ls1, Rs, TapeOut).

perform(Config, Rules, State, TapeIn, TapeOut) :-
    call(Config, _, FS, RS, B, Symbols),
    ( memberchk(State, FS) ->
        TapeOut = TapeIn
    ; memberchk(State, RS) ->
        {LeftIn, RightIn} = TapeIn,
        symbol(RightIn, Symbol, RightRem, B),
        memberchk(Symbol, Symbols),
        once(call(Rules, State, Symbol, NewSymbol, Action, NewState)),
        memberchk(NewSymbol, Symbols),
        action(Action, {LeftIn, [NewSymbol|RightRem]}, {LeftOut, RightOut}, B),
        perform(Config, Rules, NewState, {LeftOut, RightOut}, TapeOut) ).

symbol([],       B,   [], B).
symbol([Sym|Rs], Sym, Rs, _).

action(left,  {Lin, Rin},  {Lout, Rout}, B) :- left(Lin, Rin, Lout, Rout, B).
action(stay,  Tape,        Tape,         _).
action(right, {Lin, Rin},  {Lout, Rout}, B) :- right(Lin, Rin, Lout, Rout, B).

left([],     Rs, [], [B|Rs], B).
left([L|Ls], Rs, Ls, [L|Rs], _).

right(L, [],     [B|L], [], B).
right(L, [S|Rs], [S|L], Rs, _).
```


### The incrementer machine


```prolog
incrementer_config(IS, FS, RS, B, S) :-
    IS = q0,      % initial state
    FS = [qf],    % halting states
    RS = [IS],    % running states
    B  = 0,       % blank symbol
    S  = [B, 1].  % valid symbols
incrementer(q0, 1, 1, right, q0).
incrementer(q0, b, 1, stay,  qf).

turing(incrementer_config, incrementer, [1, 1, 1], TapeOut).
```

This will, on execution, fill TapeOut with [1, 1, 1, 1].

### The busy beaver machine


```prolog
busy_beaver_config(IS, FS, RS, B, S) :-
    IS = 'A',               % initial state
    FS = ['HALT'],          % halting states
    RS = [IS, 'B', 'C'],    % running states
    B  = 0,                 % blank symbol
    S  = [B, 1].            % valid symbols
busy_beaver('A', 0, 1, right, 'B').
busy_beaver('A', 1, 1, left,  'C').
busy_beaver('B', 0, 1, left,  'A').
busy_beaver('B', 1, 1, right, 'B').
busy_beaver('C', 0, 1, left,  'B').
busy_beaver('C', 1, 1, stay,  'HALT').

turing(busy_beaver_config, busy_beaver, [], TapeOut).
```

This will, on execution, fill TapeOut with [1, 1, 1, 1, 1, 1].


## Python

{{trans|Perl}}

```python
from __future__ import print_function

def run_utm(
        state = None,
        blank = None,
        rules = [],
        tape = [],
        halt = None,
        pos = 0):
    st = state
    if not tape: tape = [blank]
    if pos < 0: pos += len(tape)
    if pos >= len(tape) or pos < 0: raise Error( "bad init position")
    rules = dict(((s0, v0), (v1, dr, s1)) for (s0, v0, v1, dr, s1) in rules)

    while True:
        print(st, '\t', end=" ")
        for i, v in enumerate(tape):
            if i == pos: print("[%s]" % (v,), end=" ")
            else: print(v, end=" ")
        print()

        if st == halt: break
        if (st, tape[pos]) not in rules: break

        (v1, dr, s1) = rules[(st, tape[pos])]
        tape[pos] = v1
        if dr == 'left':
            if pos > 0: pos -= 1
            else: tape.insert(0, blank)
        if dr == 'right':
            pos += 1
            if pos >= len(tape): tape.append(blank)
        st = s1


# EXAMPLES

print("incr machine\n")
run_utm(
    halt = 'qf',
	state = 'q0',
	tape = list("111"),
	blank = 'B',
	rules = map(tuple,
               ["q0 1 1 right q0".split(),
		        "q0 B 1 stay  qf".split()]
        )
    )

print("\nbusy beaver\n")
run_utm(
    halt = 'halt',
	state = 'a',
	blank = '0',
	rules = map(tuple,
        ["a 0 1 right b".split(),
         "a 1 1 left  c".split(),
         "b 0 1 left  a".split(),
         "b 1 1 right b".split(),
         "c 0 1 left  b".split(),
         "c 1 1 stay  halt".split()]
        )
    )

print("\nsorting test\n")
run_utm(halt = 'STOP',
	state = 'A',
	blank = '0',
	tape = "2 2 2 1 2 2 1 2 1 2 1 2 1 2".split(),
	rules = map(tuple,
       ["A 1 1 right A".split(),
		"A 2 3 right B".split(),
		"A 0 0 left  E".split(),
		"B 1 1 right B".split(),
		"B 2 2 right B".split(),
		"B 0 0 left  C".split(),
		"C 1 2 left  D".split(),
		"C 2 2 left  C".split(),
		"C 3 2 left  E".split(),
		"D 1 1 left  D".split(),
		"D 2 2 left  D".split(),
		"D 3 1 right A".split(),
		"E 1 1 left  E".split(),
		"E 0 0 right STOP".split()]
        )
    )

```



## Racket



```racket

#lang racket
;;;
### =======================================================

;;; Due to heavy use of pattern matching we define few macros
;;;
### =======================================================


(define-syntax-rule (define-m f m ...)
  (define f (match-lambda m ... (x x))))

(define-syntax-rule (define-m* f m ...)
  (define f (match-lambda** m ...)))

;;;
### =======================================================

;;; The definition of a functional type Tape,
;;; representing infinite tape with O(1) operations:
;;; put, get, shift-right and shift-left.
;;;
### =======================================================

(struct Tape (the-left-part      ; i-1 i-2 i-3 ...
              the-current-record ; i
              the-right-part))   ; i+1 i+2 i+3 ...

;; the initial record on the tape
(define-m initial-tape
  [(cons h t) (Tape '() h t)])

;; shifts caret to the right
(define (snoc a b) (cons b a))
(define-m shift-right
  [(Tape '() '() (cons h t)) (Tape '() h t)]      ; left end
  [(Tape  l x '()) (Tape (snoc l x) '() '())]     ; right end
  [(Tape  l x (cons h t)) (Tape (snoc l x) h t)]) ; general case

;; shifts caret to the left
(define-m flip-tape [(Tape l x r) (Tape r x l)])

(define shift-left
  (compose flip-tape shift-right flip-tape))

;; returns the current record on the tape
(define-m get [(Tape _ v _) v])

;; writes to the current position on the tape
(define-m* put
  [('() t) t]
  [(v (Tape l _ r)) (Tape l v r)])

;; Shows the list representation of the tape (≤ O(n)).
;; A tape is shown as (... a b c (d) e f g ...)
;; where (d) marks the current position of the caret.

(define (revappend a b) (foldl cons b a))

(define-m show-tape
  [(Tape '() '() '()) '()]
  [(Tape l '() r) (revappend l (cons '() r))]
  [(Tape l v r) (revappend l (cons (list v) r))])

;;;-------------------------------------------------------------------
;;; The Turing Machine interpreter
;;;

;; interpretation of output triple for a given tape
(define-m* interprete
  [((list v 'right S) tape) (list S (shift-right (put v tape)))]
  [((list v 'left S) tape) (list S (shift-left (put v tape)))]
  [((list v 'stay S) tape) (list S (put v tape))]
  [((list S _) tape) (list S tape)])

;; Runs the program.
;; The initial state is set to start.
;; The initial tape is given as a list of records.
;; The initial position is the leftmost symbol of initial record.
(define (run-turing prog t0 start)
  ((fixed-point
    (match-lambda
      [`(,S ,T) (begin
                  (printf "~a\t~a\n" S (show-tape T))
                  (interprete (prog `(,S ,(get T))) T))]))
   (list start (initial-tape t0))))

;; a general fixed point operator
(define ((fixed-point f) x)
  (let F ([x x] [fx (f x)])
    (if (equal? x fx)
        fx
        (F fx (f fx)))))

;; A macro for definition of a Turing-Machines.
;; Transforms to a function which accepts a list of initial
;; tape records as input and returns the tape after stopping.
(define-syntax-rule (Turing-Machine #:start start (a b c d e) ...)
  (λ (l)
    (displayln "STATE\tTAPE")
    ((match-lambda [(list _ t) (flatten (show-tape t))])
     (run-turing
      (match-lambda ['(a b) '(c d e)] ... [x x])
      l start))))

```


The resulting Turing Machine is a function that maps the initial tape record to the final one, so that several machines could run one after another or composed as any other functions

Examples:

The simple incrementer:

```racket

(define INC
  (Turing-Machine #:start 'q0
    [q0 1 1 right q0]
    [q0 () 1 stay qf]))

```


```txt

> (INC '(1 1 1))
STATE	TAPE
q0	((1) 1 1)
q0	(1 (1) 1)
q0	(1 1 (1))
q0	(1 1 1 ())
qf	(1 1 1 (1))
(1 1 1 1)

```



The incrementer for binary numbers

```racket

(define ADD1
  (Turing-Machine #:start 'Start
   [Start 1  1  right Start]
   [Start 0  0  right Start]
   [Start () () left  Add]
   [Add   0  1  stay  End]
   [Add   1  0  left  Add]
   [Add   () 1  stay  End]))

```


```txt

> (ADD1 '(1 1 0))
STATE	TAPE
Start	((1) 1 0)
Start	(1 (1) 0)
Start	(1 1 (0))
Start	(1 1 0 ())
Add	(1 1 (0))
End	(1 1 (1))
(1 1 1)
> (define ADD2 (compose ADD1 ADD1))
> (ADD2 '(1 1 0))
STATE	TAPE
Start	((1) 1 0)
Start	(1 (1) 0)
Start	(1 1 (0))
Start	(1 1 0 ())
Add	(1 1 (0))
End	(1 1 (1))
STATE	TAPE
Start	((1) 1 1)
Start	(1 (1) 1)
Start	(1 1 (1))
Start	(1 1 1 ())
Add	(1 1 (1))
Add	(1 (1) 0)
Add	((1) 0 0)
Add	(() 0 0 0)
End	((1) 0 0 0)
(1 0 0 0)

```


The busy beaver

```racket

(define BEAVER
  (Turing-Machine #:start 'a
   [a () 1 right b]
   [a  1 1 left  c]
   [b () 1 left  a]
   [b  1 1 right b]
   [c () 1 left  b]
   [c  1 1 stay  halt]))

```


```txt

> (BEAVER '(()))
STATE	TAPE
a	()
b	(1 ())
a	((1) 1)
c	(() 1 1)
b	(() 1 1 1)
a	(() 1 1 1 1)
b	(1 (1) 1 1 1)
b	(1 1 (1) 1 1)
b	(1 1 1 (1) 1)
b	(1 1 1 1 (1))
b	(1 1 1 1 1 ())
a	(1 1 1 1 (1) 1)
c	(1 1 1 (1) 1 1)
halt	(1 1 1 (1) 1 1)
(1 1 1 1 1 1)

```


The sorting machine

```racket

(define SORT
  (Turing-Machine #:start 'A
   [A 1  1  right A]
   [A 2  3  right B]
   [A () () left  E]
   [B 1  1  right B]
   [B 2  2  right B]
   [B () () left  C]
   [C 1  2  left  D]
   [C 2  2  left  C]
   [C 3  2  left  E]
   [D 1  1  left  D]
   [D 2  2  left  D]
   [D 3  1  right A]
   [E 1  1  left  E]
   [E () () right STOP]))

```


```txt

> (SORT '(2 1 2 2 2 1 1))
STATE	TAPE
A	((2) 1 2 2 2 1 1)
B	(3 (1) 2 2 2 1 1)
B	(3 1 (2) 2 2 1 1)
B	(3 1 2 (2) 2 1 1)
B	(3 1 2 2 (2) 1 1)
B	(3 1 2 2 2 (1) 1)
B	(3 1 2 2 2 1 (1))
B	(3 1 2 2 2 1 1 ())
C	(3 1 2 2 2 1 (1))
D	(3 1 2 2 2 (1) 2)
D	(3 1 2 2 (2) 1 2)
D	(3 1 2 (2) 2 1 2)
D	(3 1 (2) 2 2 1 2)
D	(3 (1) 2 2 2 1 2)
D	((3) 1 2 2 2 1 2)
A	(1 (1) 2 2 2 1 2)
A	(1 1 (2) 2 2 1 2)
B	(1 1 3 (2) 2 1 2)
B	(1 1 3 2 (2) 1 2)
B	(1 1 3 2 2 (1) 2)
B	(1 1 3 2 2 1 (2))
B	(1 1 3 2 2 1 2 ())
C	(1 1 3 2 2 1 (2))
C	(1 1 3 2 2 (1) 2)
D	(1 1 3 2 (2) 2 2)
D	(1 1 3 (2) 2 2 2)
D	(1 1 (3) 2 2 2 2)
A	(1 1 1 (2) 2 2 2)
B	(1 1 1 3 (2) 2 2)
B	(1 1 1 3 2 (2) 2)
B	(1 1 1 3 2 2 (2))
B	(1 1 1 3 2 2 2 ())
C	(1 1 1 3 2 2 (2))
C	(1 1 1 3 2 (2) 2)
C	(1 1 1 3 (2) 2 2)
C	(1 1 1 (3) 2 2 2)
E	(1 1 (1) 2 2 2 2)
E	(1 (1) 1 2 2 2 2)
E	((1) 1 1 2 2 2 2)
E	(() 1 1 1 2 2 2 2)
STOP	((1) 1 1 2 2 2 2)
(1 1 1 2 2 2 2)

```



## REXX

Programming notes:   the tape is essentially infinite in two directions, but the tape starts at location one (unity), and

may extend to   -<big>'''∞'''</big>   and   +<big>'''∞'''</big>   (subject to virtual memory limitations), something short of ≈ 2 billion bytes.

Minimal error checking is done, but if no rule is found to be applicable, an appropriate error message is issued.

### incrementer machine


```rexx
/*REXX program executes a  Turing machine  based on   initial state,  tape, and rules.  */
state = 'q0'                                     /*the initial Turing machine state.    */
term  = 'qf'                                     /*a state that is used for a  halt.    */
blank = 'B'                                      /*this character is a  "true"  blank.  */
call Turing_rule  'q0 1 1 right q0'              /*define a rule for the Turing machine.*/
call Turing_rule  'q0 B 1 stay  qf'              /*   "   "   "   "   "     "      "    */
call Turing_init   1 1 1                         /*initialize the tape to some string(s)*/
call TM                                          /*go and invoke the  Turning  machine. */
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
TM:  !=1;   bot=1;   top=1;   @er= '***error***' /*start at  the tape location   1.     */
     say                                         /*might as well display a blank line.  */
          do cycle=1  until  state==term         /*process Turing machine  instructions.*/
             do k=1  for rules                   /*   "       "       "        rules.   */
             parse var rule.k rState rTape rWrite rMove rNext .          /*pick pieces. */
             if state\==rState | @.!\==rTape  then iterate               /*wrong rule ? */
             @.!=rWrite                          /*right rule;  write it ───► the tape. */
             if rMove== 'left'  then !=!-1       /*Are we moving left?   Then subtract 1*/
             if rMove=='right'  then !=!+1       /* "   "    "   right?    "    add    1*/
             bot=min(bot, !);   top=max(top, !)  /*find the  tape  bottom and top.      */
             state=rNext;       iterate cycle    /*use this for the next  state;  and   */
             end   /*k*/
          say @er 'unknown state:' state;  leave /*oops, we have an unknown state error.*/
          end   /*cycle*/
     $=                                          /*start with empty string  (the tape). */
          do t=bot  to top;        _=@.t
          if _==blank  then _=' '                /*do we need to translate a true blank?*/
          $=$ || pad || _                        /*construct char by char, maybe pad it.*/
          end   /*t*/                            /* [↑]  construct  the  tape's contents*/
     L=length($)                                 /*obtain length of  "     "       "    */
     if L==0     then $= "[tape is blank.]"      /*make an  empty tape  visible to user.*/
     if L>1000   then $=left($, 1000) ...        /*truncate tape to 1k bytes, append ···*/
     say "tape's contents:"  $                   /*show the tape's contents (or 1st 1k).*/
     say "tape's   length: " L                   /*  "   "     "   length.              */
     say 'Turning machine used '    rules    " rules in "    cycle    ' cycles.'
     return
/*──────────────────────────────────────────────────────────────────────────────────────*/
Turing_init:  @.=blank;  parse arg x;    do j=1  for words(x);  @.j=word(x,j);  end  /*j*/
              return
/*──────────────────────────────────────────────────────────────────────────────────────*/
Turing_rule:  if symbol('RULES')=="LIT"  then rules=0;       rules=rules+1
              pad=left('', length( word( arg(1),2 ) ) \==1 )          /*padding for rule*/
              rule.rules=arg(1);         say right('rule' rules, 20)   "═══►"   rule.rules
              return
```

'''output'''

```txt

              rule 1 ═══► q0 1 1 right q0
              rule 2 ═══► q0 B 1 stay  qf

tape's contents: 1111
tape's   length: 4
Turning machine used  2  rules in  4  cycles.

```


===three-state busy beaver===

```rexx
/*REXX program executes a  Turing machine  based on   initial state,  tape, and rules.  */
state = 'a'                                      /*the initial  Turing machine  state.  */
term  = 'halt'                                   /*a state that is used for a  halt.    */
blank =  0                                       /*this character is a  "true"  blank.  */
call Turing_rule  'a 0 1 right b'                /*define a rule for the Turing machine.*/
call Turing_rule  'a 1 1 left  c'                /*   "   "   "   "   "     "      "    */
call Turing_rule  'b 0 1 left  a'                /*   "   "   "   "   "     "      "    */
call Turing_rule  'b 1 1 right b'                /*   "   "   "   "   "     "      "    */
call Turing_rule  'c 0 1 left  b'                /*   "   "   "   "   "     "      "    */
call Turing_rule  'c 1 1 stay  halt'             /*   "   "   "   "   "     "      "    */
call Turing_init                                 /*initialize the tape to some string(s)*/
call TM                                          /*go and invoke the  Turning machine.  */
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
TM: ∙∙∙
```

'''output'''

```txt

              rule 1 ═══► a 0 1 right b
              rule 2 ═══► a 1 1 left  c
              rule 3 ═══► b 0 1 left  a
              rule 4 ═══► b 1 1 right b
              rule 5 ═══► c 0 1 left  b
              rule 6 ═══► c 1 1 stay  halt

tape's contents: 111111
tape's   length: 6
Turning machine used  6  rules in  13  cycles.

```


===five-state busy beaver===

```rexx
/*REXX program executes a  Turing machine  based on   initial state,  tape, and rules.  */
state = 'A'                                      /*initialize the  Turing machine state.*/
term  = 'H'                                      /*a state that is used for the  halt.  */
blank =  0                                       /*this character is a  "true"  blank.  */
call Turing_rule  'A 0 1 right B'                /*define a rule for the Turing machine.*/
call Turing_rule  'A 1 1 left  C'                /*   "   "   "   "   "     "      "    */
call Turing_rule  'B 0 1 right C'                /*   "   "   "   "   "     "      "    */
call Turing_rule  'B 1 1 right B'                /*   "   "   "   "   "     "      "    */
call Turing_rule  'C 0 1 right D'                /*   "   "   "   "   "     "      "    */
call Turing_rule  'C 1 0 left  E'                /*   "   "   "   "   "     "      "    */
call Turing_rule  'D 0 1 left  A'                /*   "   "   "   "   "     "      "    */
call Turing_rule  'D 1 1 left  D'                /*   "   "   "   "   "     "      "    */
call Turing_rule  'E 0 1 stay  H'                /*   "   "   "   "   "     "      "    */
call Turing_rule  'E 1 0 left  A'                /*   "   "   "   "   "     "      "    */
call Turing_init                                 /*initialize the tape to some string(s)*/
call TM                                          /*go and invoke the  Turning machine.  */
exit                                             /*stick a fork in it, we're done.*/
/*──────────────────────────────────────────────────────────────────────────────────────*/
TM: ∙∙∙
```

'''output'''

```txt

              rule 1 ═══► A 0 1 right B
              rule 2 ═══► A 1 1 left  C
              rule 3 ═══► B 0 1 right C
              rule 4 ═══► B 1 1 right B
              rule 5 ═══► C 0 1 right D
              rule 6 ═══► C 1 0 left  E
              rule 7 ═══► D 0 1 left  A
              rule 8 ═══► D 1 1 left  D
              rule 9 ═══► E 0 1 stay  H
             rule 10 ═══► E 1 0 left  A

tape's contents:  1 1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1
  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1
  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1
  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1
  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1
  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1
  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1
  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1
  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1
  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1
  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1
  1  1  1  1  1  1  1  1  1  ...
tape's length is:  12289

Turning machine used  10  rules in  47176870  cycles.

```



### stress sort


```rexx
/*REXX program executes a  Turing machine  based on   initial state,  tape, and rules.  */
state = 'A'                                      /*the initial  Turing machine  state.  */
term  = 'halt'                                   /*a state that is used for the  halt.  */
blank =  0                                       /*this character is a  "true"  blank.  */
call Turing_rule  'A 1 1 right A'                /*define a rule for the Turing machine.*/
call Turing_rule  'A 2 3 right B'                /*   "   "   "   "   "     "      "    */
call Turing_rule  'A 0 0 left  E'                /*   "   "   "   "   "     "      "    */
call Turing_rule  'B 1 1 right B'                /*   "   "   "   "   "     "      "    */
call Turing_rule  'B 2 2 right B'                /*   "   "   "   "   "     "      "    */
call Turing_rule  'B 0 0 left  C'                /*   "   "   "   "   "     "      "    */
call Turing_rule  'C 1 2 left  D'                /*   "   "   "   "   "     "      "    */
call Turing_rule  'C 2 2 left  C'                /*   "   "   "   "   "     "      "    */
call Turing_rule  'C 3 2 left  E'                /*   "   "   "   "   "     "      "    */
call Turing_rule  'D 1 1 left  D'                /*   "   "   "   "   "     "      "    */
call Turing_rule  'D 2 2 left  D'                /*   "   "   "   "   "     "      "    */
call Turing_rule  'D 3 1 right A'                /*   "   "   "   "   "     "      "    */
call Turing_rule  'E 1 1 left  E'                /*   "   "   "   "   "     "      "    */
call Turing_rule  'E 0 0 right halt'             /*   "   "   "   "   "     "      "    */
call Turing_init   1 2 2 1 2 2 1 2 1 2 1 2 1 2   /*initialize the tape to some string(s)*/
call TM                                          /*go and invoke the Turning machine.   */
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
TM: ∙∙∙
```

'''output'''

```txt

              rule 1 ═══► A 1 1 right A
              rule 2 ═══► A 2 3 right B
              rule 3 ═══► A 0 0 left  E
              rule 4 ═══► B 1 1 right B
              rule 5 ═══► B 2 2 right B
              rule 6 ═══► B 0 0 left  C
              rule 7 ═══► C 1 2 left  D
              rule 8 ═══► C 2 2 left  C
              rule 9 ═══► C 3 2 left  E
             rule 10 ═══► D 1 1 left  D
             rule 11 ═══► D 2 2 left  D
             rule 12 ═══► D 3 1 right A
             rule 13 ═══► E 1 1 left  E
             rule 14 ═══► E 0 0 right halt

tape's contents:  11111122222222
tape's   length: 16
Turning machine used  14  rules in  118  cycles.

```



## Ruby


### The universal machine


```ruby
class Turing
    class Tape
        def initialize(symbols, blank, starting_tape)
            @symbols = symbols
            @blank = blank
            @tape = starting_tape
            @index = 0
        end
        def read
            retval = @tape[@index]
            unless retval
                retval = @tape[@index] = @blank
            end
            raise "invalid symbol '#{retval}' on tape" unless @tape.member?(retval)
            return retval
        end
        def write(symbol)
            @tape[@index] = symbol
        end
        def right
            @index += 1
        end
        def left
            if @index == 0
              @tape.unshift @blank
            else
              @index -= 1
            end
        end
        def stay
            # nop
        end
        def get_tape
            return @tape
        end
    end

    def initialize(symbols, blank,
                   initial_state, halt_states, running_states,
                   rules, starting_tape = [])
        @tape = Tape.new(symbols, blank, starting_tape)
        @initial_state = initial_state
        @halt_states = halt_states
        @running_states = running_states
        @rules = rules
        @halted = false
    end
    def run
        raise "machine already halted" if @halted
        state = @initial_state
        while (true)
            break if @halt_states.member? state
            raise "unknown state '#{state}'" unless @running_states.member? state
            symbol = @tape.read
            outsym, action, state = @rules[state][symbol]
            @tape.write outsym
            @tape.send action
        end
        @halted = true
        return @tape.get_tape
    end
end
```



### The incrementer machine


```ruby
incrementer_rules = {
    :q0 => { 1  => [1, :right, :q0],
             :b => [1, :stay,  :qf]}
}
t = Turing.new([:b, 1],           # permitted symbols
               :b,                # blank symbol
               :q0,               # starting state
               [:qf],             # terminating states
               [:q0],             # running states
               incrementer_rules, # operating rules
               [1, 1, 1])         # starting tape
print t.run, "\n"
```



### The busy beaver machine


```ruby
busy_beaver_rules = {
    :a => { 0 => [1, :right, :b],
            1 => [1, :left,  :c]},
    :b => { 0 => [1, :left,  :a],
            1 => [1, :right, :b]},
    :c => { 0 => [1, :left,  :b],
            1 => [1, :stay,  :halt]}
}
t = Turing.new([0, 1],            # permitted symbols
               0,                 # blank symbol
               :a,                # starting state
               [:halt],           # terminating states
               [:a, :b, :c],      # running states
               busy_beaver_rules, # operating rules
               [])                # starting tape
print t.run, "\n"
```



## Scala

Simple implementation of Universal Turing Machine in Scala:


```scala

package utm.scala

import scala.annotation.tailrec
import scala.language.implicitConversions

/**
  * Implementation of Universal Turing Machine in Scala that can simulate an arbitrary
  * Turing machine on arbitrary input
  *
  * @author Abdulla Abdurakhmanov (https://github.com/abdmob/utms)
  */
class UniversalTuringMachine[S](val rules: List[UTMRule[S]],
                                val initialState: S,
                                val finalStates: Set[S],
                                val blankSymbol: String,
                                val inputTapeVals: Seq[String],
                                printEveryIter: Int = 1) {

	private val initialTape = UTMTape(inputTapeVals, 0, blankSymbol)

	@tailrec
	private def iterate(state: S, curIteration: Int, tape: UTMTape): UTMTape = {
		val needToBePrinted = curIteration % printEveryIter == 0

		if (needToBePrinted) {
			print(s"${curIteration}: ${state}: ")
			tape.printTape()
		}

		if (finalStates.contains(state)) {
			println(s"Finished in the final state: ${state}")
			tape.printTape()
			tape
		}
		else {
			rules.find(rule => rule.state == state && rule.fromSymbol == tape.current()) match {
				case Some(rule) => {
					val updatedTape = tape.updated(
						rule.toSymbol,
						rule.action
					)

					iterate(
						rule.toState,
						curIteration + 1,
						updatedTape
					)
				}
				case _ => {
					println(s"Finished: no suitable rules found for ${state}/${tape.current()}")
					tape.printTape()
					tape
				}
			}
		}
	}

	def run(): UTMTape = iterate(state = initialState, curIteration = 0, tape = initialTape)

}

/**
  * Universal Turing Machine actions
  */
sealed trait UTMAction
case class UTMLeft() extends UTMAction
case class UTMRight() extends UTMAction
case class UTMStay() extends UTMAction

/**
  * Universal Turing Machine rule definition
  */
case class UTMRule[S](state: S,
                      fromSymbol: String,
                      toSymbol: String,
                      action: UTMAction,
                      toState: S)

/**
  * Universal Turing Machine Tape
  */
case class UTMTape(content: Seq[String], position: Int, blankSymbol: String) {

	private def updateContentAtPos(symbol: String) = {
		if (position >= content.length) {
			content :+ symbol
		}
		else if (position < 0) {
			symbol +: content
		}
		else
			content.updated(position, symbol)
	}

	private[scala] def updated(symbol: String, action: UTMAction): UTMTape = {
		val updatedTape =
			this.copy(
				content = updateContentAtPos(symbol),
				position = action match {
					case UTMLeft() => position - 1
					case UTMRight() => position + 1
					case UTMStay() => position
				}
			)

		if (updatedTape.position < 0) {
			updatedTape.copy(
				content = blankSymbol +: updatedTape.content,
				position = 0
			)
		}
		else if (updatedTape.position >= updatedTape.content.length) {
			updatedTape.copy(
				content = updatedTape.content :+ blankSymbol
			)
		}
		else
			updatedTape
	}


	private[scala] def current(): String = {
		if (content.isDefinedAt(position))
			content(position)
		else
			blankSymbol
	}

	def printTape(): Unit = {
		print("[")
		if (position < 0)
			print("˅")
		content.zipWithIndex.foreach { case (symbol, index) =>
			if (position == index)
				print("˅")
			else
				print(" ")
			print(s"$symbol")
		}
		if (position >= content.length)
			print("˅")
		println("]")
	}

}

object UniversalTuringMachine extends App {

	object dsl {

		final val right = UTMRight()
		final val left = UTMLeft()
		final val stay = UTMStay()

		implicit def tupleToUTMLRule[S](tuple: (S, String, String, UTMAction, S)): UTMRule[S] =
			UTMRule[S](tuple._1, tuple._2, tuple._3, tuple._4, tuple._5)
	}

        main()

	def main(): Unit = {
		import dsl._

		def createIncrementMachine() = {

			sealed trait IncrementStates
			case class q0() extends IncrementStates
			case class qf() extends IncrementStates

			new UniversalTuringMachine[IncrementStates](
				rules = List(
					(q0(), "1", "1", right, q0()),
					(q0(), "B", "1", stay, qf())
				),
				initialState = q0(),
				finalStates = Set(qf()),
				blankSymbol = "B",
				inputTapeVals = Seq("1", "1", "1")
			).run()

		}

		def createThreeStateBusyBeaver() = {

			sealed trait ThreeStateBusyStates
			case class a() extends ThreeStateBusyStates
			case class b() extends ThreeStateBusyStates
			case class c() extends ThreeStateBusyStates
			case class halt() extends ThreeStateBusyStates

			new UniversalTuringMachine[ThreeStateBusyStates](
				rules = List(
					(a(), "0", "1", right, b()),
					(a(), "1", "1", left, c()),
					(b(), "0", "1", left, a()),
					(b(), "1", "1", right, b()),
					(c(), "0", "1", left, b()),
					(c(), "1", "1", stay, halt())
				),
				initialState = a(),
				finalStates = Set(halt()),
				blankSymbol = "0",
				inputTapeVals = Seq()
			).run()

		}

		def createFiveState2SymBusyBeaverMachine() = {
			sealed trait FiveBeaverStates
			case class FA() extends FiveBeaverStates
			case class FB() extends FiveBeaverStates
			case class FC() extends FiveBeaverStates
			case class FD() extends FiveBeaverStates
			case class FE() extends FiveBeaverStates
			case class FH() extends FiveBeaverStates

			new UniversalTuringMachine[FiveBeaverStates](
				rules = List(
					(FA(), "0", "1", right, FB()),
					(FA(), "1", "1", left, FC()),
					(FB(), "0", "1", right, FC()),
					(FB(), "1", "1", right, FB()),
					(FC(), "0", "1", right, FD()),
					(FC(), "1", "0", left, FE()),
					(FD(), "0", "1", left, FA()),
					(FD(), "1", "1", left, FD()),
					(FE(), "0", "1", stay, FH()),
					(FE(), "1", "0", left, FA())
				),
				initialState = FA(),
				finalStates = Set(FH()),
				blankSymbol = "0",
				inputTapeVals = Seq(),
				printEveryIter = 100000
			).run()
		}

		createIncrementMachine()
		createThreeStateBusyBeaver()

		// careful here, 47 mln iterations,
		// so this is commented to save our nature (I checked it for you anyway):
		// createFiveState2SymBusyBeaverMachine()
	}
}

```

{{output}}

```txt

0: q0(): [˅1 1 1]
1: q0(): [ 1˅1 1]
2: q0(): [ 1 1˅1]
3: q0(): [ 1 1 1˅B]
4: qf(): [ 1 1 1˅1]
Finished in the final state: qf()
[ 1 1 1˅1]
0: a(): [˅]
1: b(): [ 1˅0]
2: a(): [˅1 1]
3: c(): [˅0 1 1]
4: b(): [˅0 1 1 1]
5: a(): [˅0 1 1 1 1]
6: b(): [ 1˅1 1 1 1]
7: b(): [ 1 1˅1 1 1]
8: b(): [ 1 1 1˅1 1]
9: b(): [ 1 1 1 1˅1]
10: b(): [ 1 1 1 1 1˅0]
11: a(): [ 1 1 1 1˅1 1]
12: c(): [ 1 1 1˅1 1 1]
13: halt(): [ 1 1 1˅1 1 1]
Finished in the final state: halt()

```



## SequenceL

This implemetnation is based on the Computing Machines introduced in Turing's 1936 paper [https://www.cs.virginia.edu/~robins/Turing_Paper_1936.pdf ON COMPUTABLE NUMBERS, WITH AN APPLICATION TO THE ENTSCHEIDUNGSPROBLEM]. With the exception of "skeleton tables".

'''SequenceL Code:'''


```sequencel
//region Imports

import <Utilities/Conversion.sl>;
import <Utilities/Sequence.sl>;

//endregion

//region Types

MCONFIG ::= (Label: char(1), Symbols: char(2), Operations: char(2), FinalConfig: char(1));
STATE ::= (CurrentConfig: char(1), CurrentPosition: int(0), Tape: char(1));
INPUT_DATA ::= (Iterations: int(0), InitialTape: char(1), StartingPosition: int(0), InitialConfig: char(1), MConfigs: MCONFIG(1));

//endregion

//region Constants

SPACE_CHAR := '_';
DELIMITTER := '|';

NULL_CONFIG := (Label: "", Symbols: [], Operations: [], FinalConfig: "");

TRACE_HEADER := ["Config:\t| Place:\t| Tape:"];

//endregion

//region Helpers

StateToString(state(0)) :=
    state.CurrentConfig ++
    "  \t\t| " ++ intToString(state.CurrentPosition) ++
    "  \t| " ++ state.Tape;

StateToArrowString(state(0)) :=
    state.Tape ++ "\n" ++
    duplicate(' ', state.CurrentPosition - 1) ++ "|\n" ++
    duplicate(' ', state.CurrentPosition - 1) ++ state.CurrentConfig ++ "\n";

HeadOfEach(strings(2))[i] :=
    head(strings[i]);

RemoveCharacter(character(0), string(1))[i] :=
    string[i] when not(string[i] = character);

GetFSquares(Tape(1))[i] :=
    Tape[i] when (i mod 2) = 1;

//endregion

//region Parsing

ParseConfig(Line(1)) :=
    let
        entries := split(Line, DELIMITTER);
        label := entries[1];
        symbols := split(entries[2], ',');
        operations := split(entries[3], ',');
        finalConfig := entries[4];
    in
        ((Label: label, Symbols: symbols, Operations: operations, FinalConfig: finalConfig) when not((Line[1] = '/') and (Line[2] = '/')))
            when size(Line) > 0;

ParseTextFile(Text(1)) :=
    let
        noSpaces := RemoveCharacter('\t', RemoveCharacter('\r', RemoveCharacter(' ', Text)));
        lines := split(noSpaces, '\n');
        iterations := stringToInt(lines[1]);
        initialTape := lines[2];
        initialPosition := stringToInt(lines[3]);
        initialConfig := lines[4];
        mConfigs := ParseConfig(lines[5 ... size(lines)]);
    in
        (Iterations: iterations, InitialTape: initialTape, StartingPosition: initialPosition, InitialConfig: initialConfig, MConfigs: mConfigs);

//endregion

//region Config Finding

Matches: char(0) * char(2) -> bool;
Matches(currentSymbol(0), symbols(2)) :=
        true when size(symbols) = 0 //some(equalListNT("", symbols))
    else
        true when currentSymbol = SPACE_CHAR and some(equalListNT("none", symbols))
    else
        true when not(currentSymbol = SPACE_CHAR) and some(equalListNT("any", symbols))
    else
        true when some(currentSymbol = HeadOfEach(symbols))
    else
        false;

GetCurrentSymbol(State(0)) :=
        State.Tape[State.CurrentPosition] when size(State.Tape) >= State.CurrentPosition and State.CurrentPosition > 0
    else
        SPACE_CHAR;

GetConfigHelper(label(1), symbol(0), mConfigs(1))[i] :=
    mConfigs[i] when equalList(mConfigs[i].Label, label) and Matches(symbol, mConfigs[i].Symbols);

GetConfig(label(1), symbol(0), mConfigs(1)) :=
    let
        searchResults := GetConfigHelper(label, symbol, mConfigs);
    in
            NULL_CONFIG when size(searchResults) = 0
        else
            searchResults[1];

//endregion

//region Operations

TrimTapeEnd(tape(1), position(0)) :=
        tape when position = size(tape)
    else
        tape when not(last(tape) = SPACE_CHAR)
    else
        TrimTapeEnd(allButLast(tape), position);

ApplyOperations(State(0), Operations(2)) :=
    let
        newState := ApplyOperation(State, head(Operations));
    in
            State when size(Operations) = 0
        else
            ApplyOperations(newState, tail(Operations));

ApplyOperation(State(0), Operation(1)) :=
    let
        newTape :=
                PrintOperation(head(tail(Operation)), State.CurrentPosition, State.Tape) when head(Operation) = 'P'
            else
                EraseOperation(State.CurrentPosition, State.Tape) when head(Operation) = 'E'
            else
                [SPACE_CHAR] ++ State.Tape when head(Operation) = 'L' and State.CurrentPosition = 1
            else
                State.Tape ++ [SPACE_CHAR] when head(Operation) = 'R' and State.CurrentPosition = size(State.Tape)
            else
                State.Tape;

        newPosition :=
                1 when head(Operation) = 'L' and State.CurrentPosition = 1
            else
                State.CurrentPosition + 1 when head(Operation) = 'R'
            else
                State.CurrentPosition - 1 when head(Operation) = 'L'
            else
                State.CurrentPosition;

        trimmedTape := TrimTapeEnd(newTape, newPosition);
    in
            State when size(Operation) = 0
        else
            (CurrentPosition: newPosition, Tape: trimmedTape);

PrintOperation(Symbol(0), Position(0), Tape(1)) :=
    let
        diff := Position - size(Tape) when Position > size(Tape) else 0;
        expandedTape := Tape ++ duplicate(SPACE_CHAR, diff);
        finalTape := setElementAt(expandedTape, Position, Symbol);
    in
        finalTape;

EraseOperation(Position(0), Tape(1)) :=
    PrintOperation(SPACE_CHAR, Position, Tape);

//endregion

//region Execution

RunMachine(Text(1), Flag(1)) :=
    let
        input := ParseTextFile(Text);
        initialState := (CurrentConfig: input.InitialConfig, CurrentPosition: input.StartingPosition, Tape: input.InitialTape);

        processed := Process(initialState, input.MConfigs, input.Iterations);
        processedWithTrace := ProcessWithTrace(initialState, input.MConfigs, input.Iterations);
    in
            "\n" ++ delimit(TRACE_HEADER ++ StateToString(processedWithTrace), '\n') when equalList(Flag, "trace")
        else
            "\n" ++ delimit(StateToArrowString(processedWithTrace), '\n') when equalList(Flag, "arrow-trace")
        else
            processed.Tape when equalList(Flag, "tape")
        else
            TrimTapeEnd(GetFSquares(processed.Tape), 1) when equalList(Flag, "f-squares")
        else
            boolToString(DoesMachineHalt(initialState, input.MConfigs, input.Iterations)) when equalList(Flag, "halts")
        else
            StateToString(processed);

DoesMachineHalt(InitialState(0), mConfigs(1), Iterations(0)) :=
    let
        resultState := Process(InitialState, mConfigs, Iterations);
    in
        equalList(resultState.CurrentConfig, "halt");

ProcessWithTrace(InitialState(0), mConfigs(1), Iterations(0)) :=
        [InitialState] when Iterations <= 0 or size(InitialState.CurrentConfig) = 0 or equalList(InitialState.CurrentConfig, "halt")
    else
        [InitialState] ++ ProcessWithTrace(Iterate(InitialState, mConfigs), mConfigs, Iterations - 1);

Process(InitialState(0), mConfigs(1), Iterations(0)) :=
        InitialState when Iterations = 0 or size(InitialState.CurrentConfig) = 0 or equalList(InitialState.CurrentConfig, "halt")
    else
        Process(Iterate(InitialState, mConfigs), mConfigs, Iterations - 1);

Iterate(State(0), mConfigs(1)) :=
    let
        currentConfig := GetConfig(State.CurrentConfig, GetCurrentSymbol(State), mConfigs);
        newState := Execute(State, currentConfig);
    in
        newState;

Execute(State(0), mConfig(0)) :=
    let
        newState := ApplyOperations(State, mConfig.Operations);
    in
        (CurrentConfig: mConfig.FinalConfig, CurrentPosition: newState.CurrentPosition, Tape: newState.Tape);

//endregion
```


'''C++ Driver Code:'''


```c
#include <iostream>
#include <fstream>
#include <string>
#include <cerrno>
#include "SL_Generated.h"

int cores = 0;
string fileName = "../../INPUT/irrational.tm";
string flag = "tape";
string fileContents = "";

using namespace std;

std::string get_file_contents(const char *filename);

int main( int argc, char** argv )
{
    if(argc >= 2)
    {
        fileName = argv[1];
    }

    if(argc >= 3)
    {
        flag = argv[2];
    }

    if(argc >= 4)
    {
        cores = atoi(argv[3]);
    }

    int flagDims[] = { flag.length(), 0};
    Sequence<char> flagSeq((void*)(flag.c_str()), flagDims);

    fileContents = get_file_contents(fileName.c_str());
    int inputDims[] = { fileContents.length(), 0};
    Sequence<char> input((void*)(fileContents.c_str()), inputDims);

    Sequence<char> result;

    sl_init(cores);

    sl_RunMachine(input, flagSeq, cores, result);

    cout<<result<<endl;

    sl_done();

    return 0;
}

std::string get_file_contents(const char *filename)
{
  std::ifstream in(filename, std::ios::in | std::ios::binary);
  if (in)
  {
    std::string contents;
    in.seekg(0, std::ios::end);
    contents.resize(in.tellg());
    in.seekg(0, std::ios::beg);
    in.read(&contents[0], contents.size());
    in.close();
    return(contents);
  }
  throw(errno);
}
```


'''Turing Machine Files'''<br />
File explanation:<br />
An input file consists of the following:

    a) The first line contains an integer specifying the number of iterations to execute the machine.

    b) The second line specifies the initial tape of the machine.

    c) The third line contains an integer specifying the initial position of the head of the machine.

    d) The fourth line specifies the initial m-config of the machine.

    e) The remaining lines contain the rules of the machine or a comment, one on each line. Blank lines are ignored.

        i. A line is specified to be a comment if it starts with two forward slashes (//).

Every rule consists of four parts separated by a pipe (|):

    a) The first part is the name of the m-config declaration.

    b) The second part is the specification of the scanned symbols. This is a, possibly empty, list of symbols, variable names, and keywords.

    c) The third part is the specification of the actions to perform. This is a, possibly empty, list of print commands, erase commands, and move commands.

    d) The fourth part specifies the target m-config.


'''Incrementer'''
```txt
60
111
1
q0
q0 | 1 | P1, R | q0
q0 | none | P1 | halt
```


'''Busy beaver'''
```txt
60

1
a
a| none | P1, R | b
a| 1    | P1, L | c
b| none | P1, L | a
b| 1    | P1, R | b
c| none | P1, L | b
c| 1    | P1    | halt
```

'''Sort'''
```txt
50000000
1221221211
1
A
A|1|P1,R|A
A|2|P3,R|B
A|none|E,L|E
B|1|P1,R|B
B|2|P2,R|B
B|none|E,L|C
C|1|P2,L|D
C|2|P2,L|C
C|3|P2,L|E
D|1|P1,L|D
D|2|P2,L|D
D|3|P1,R|A
E|1|P1,L|E
E|none|E,R|halt
```


'''Irrational Number'''
```txt
100

1
b
//Computes the Irrational Number .0010110111011110111110111111.....
//Sets up the beginning of the tape.
b | | Pe, R, Pe, R, P0, R, R, P0, L, L | o

//Marks all of the 1's with x
o | 1 | R, Px, L, L, L | o
o | 0 | | q

//Goes to the end and prints a '1'.
q | 0, 1 | R, R | q
q | none | P1, L | p

//Goes to the first x and then Handles erasing the 'x' that marks a '1'.
p | x | E, R | q
p | e | R | f
p | none | L, L | p

//Tacks a '0' to the end.
f | any | R, R | f
f | none | P0, L, L | o
```


{{out}}Busy beaver

```txt

utm.exe busybeaver.tm arrow-trace

|
a

1_
 |
 b

11
|
a

_11
|
c

_111
|
b

_1111
|
a

11111
 |
 b

11111
  |
  b

11111
   |
   b

11111
    |
    b

11111_
     |
     b

111111
    |
    a

111111
   |
   c

111111
   |
   halt
```




## Sidef

{{trans|Perl 6}}

```ruby
func run_utm(state="", blank="", rules=[], tape=[blank], halt="", pos=0) {

    if (pos < 0) {
        pos += tape.len;
    }

    if (pos !~ tape.range) {
        die "Bad initial position";
    }

    loop {
        print "#{state}\t";
        tape.range.each { |i|
            var v = tape[i];
            print (i == pos ? "[#{v}]" : " #{v} ");
        };
        print "\n";

        if (state == halt) {
            break;
        }

        rules.each { |rule|
            var (s0, v0, v1, dir, s1) = rule...;
            if ((s0 != state) || (tape[pos] != v0)) {
                next;
            }

            tape[pos] = v1;

            given(dir) {
                when ('left') {
                     if (pos == 0) { tape.unshift(blank) }
                     else          { --pos };
                }
                when ('right') {
                    if (++pos >= tape.len) {
                        tape.append(blank)
                    }
                }
            }

            state = s1;
            goto :NEXT;
        }

        die 'No matching rules';
        @:NEXT;
    }
}

print "incr machine\n";
run_utm(
    halt:  'qf',
    state: 'q0',
    tape:  %w(1 1 1),
    blank: 'B',
    rules: [
        %w(q0 1 1 right q0),
        %w(q0 B 1 stay  qf),
    ]);

say "\nbusy beaver";
run_utm(
    halt:  'halt',
    state: 'a',
    blank: '0',
    rules: [
        %w(a 0 1 right b),
        %w(a 1 1 left  c),
        %w(b 0 1 left  a),
        %w(b 1 1 right b),
        %w(c 0 1 left  b),
        %w(c 1 1 stay  halt),
    ]);

say "\nsorting test";
run_utm(
    halt:  'STOP',
    state: 'A',
    blank: '0',
    tape:  %w(2 2 2 1 2 2 1 2 1 2 1 2 1 2),
    rules: [
        %w(A 1 1 right A),
        %w(A 2 3 right B),
        %w(A 0 0 left  E),
        %w(B 1 1 right B),
        %w(B 2 2 right B),
        %w(B 0 0 left  C),
        %w(C 1 2 left  D),
        %w(C 2 2 left  C),
        %w(C 3 2 left  E),
        %w(D 1 1 left  D),
        %w(D 2 2 left  D),
        %w(D 3 1 right A),
        %w(E 1 1 left  E),
        %w(E 0 0 right STOP),
    ]);
```



## Standard ML


In this implementation, tapes are represented like this:
<ol>
<li>Either a tape is empty,
<li>the head points on a field where on the right and on the left hand side may be other symbols,
<li>the head may be n fields away (left or right) of the non-emty written area.
</ol>

Each cell may be written (SOME) or blank (NONE). A written field may not be written blank again.



```sml
(*** Signatures ***)

signature TAPE = sig
	datatype move = Left | Right | Stay

	type ''a tape
	val empty : ''a tape
	val moveLeft  : ''a tape -> ''a tape
	val moveRight : ''a tape -> ''a tape
	val move      : ''a tape -> move -> ''a tape
	val getSymbol : ''a tape -> ''a option
	val write     : ''a option -> ''a tape -> ''a tape
	val leftOf    : ''a tape -> ''a option list	(* Symbols left of the head in reverse order *)
	val rightOf   : ''a tape -> ''a option list	(* Symbols right of of the head *)
end

signature MACHINE = sig
	structure Tape : TAPE

	type state = int

	(* ''a is band alphabet type *)
	type ''a transitions = (state * ''a option Vector.vector) ->
	                       (state * (Tape.move * ''a option) Vector.vector)

	type ''a configuration = { state : state, tapes : ''a Tape.tape Vector.vector }

	type ''a machine = {
			alphabet : ''a Vector.vector,		(* (not used) *)
			states : state Vector.vector,		(* (not used) *)
			start  : state,				(* element of stats *)
			final  : state,				(* element of stats *)
			transitions : ''a transitions,		(* transitions *)
			tapes  : ''a Tape.tape Vector.vector	(* vector of the initial tapes *)
		}
end



signature UNIVERSALMACHINE = sig
	structure Machine : MACHINE

	val start : ''a Machine.machine -> ''a Machine.configuration

	(* Find a holding configuration (limited by n steps)
	 * Execute the handler for each step *)
	val simulate : (''a Machine.configuration -> unit) -> ''a Machine.machine -> int option -> ''a Machine.configuration option
end


(*** Implementation ***)

structure Tape :> TAPE = struct

	(*
	 * NONE   => blank field
	 * SOME a => written field
	 *)
	type ''a symbol = ''a option

	datatype move = Left | Right | Stay

	(*
	 * Four cases:
	 * 1   The tape is complete empty.
	 * 2   The head is in the written area.
	 *     On the right and on the left handside are symbols,
	 *     On the head is a symbol.
	 * 3/4 The head is (n-1) fields over the right/left edge of the written area.
	 *     There is at least one entry and a rest list of entries.
	 *)
	datatype ''a tape =
	  Empty
	| Middle  of ''a symbol list * ''a symbol * ''a symbol list
	| LeftOf  of ''a symbol * ''a symbol list * int
	| RightOf of ''a symbol * ''a symbol list * int

	val empty = Empty

	fun rep a 0 = nil
	  | rep a n = a :: rep a (n-1)

	fun leftOf  (Empty)              = nil
	  | leftOf  (Middle (ls, _, _))  = ls
	  | leftOf  (RightOf (r, rs, i)) = rep NONE i @ r :: rs
	  | leftOf  (LeftOf _)           = nil

	fun rightOf (Empty)              = nil
	  | rightOf (Middle (_, _, rs))  = rs
	  | rightOf (RightOf _)          = nil
	  | rightOf (LeftOf (l, ls, i))  = rep NONE i @ l :: ls

	fun write (NONE) t = t (* Cannot write a blank field! *)
	  | write a      t = Middle (leftOf t, a, rightOf t)

	fun getSymbol (Middle (_, m, _)) = m
	  | getSymbol _ = NONE (* blank *)


	fun moveRight (Empty) = Empty
	  | moveRight (Middle (ls, m,   nil)) = RightOf (m, ls, 0)
	  | moveRight (Middle (ls, m, r::rs)) = Middle (m::ls, r, rs)
	  | moveRight (RightOf (l, ls, n))    = RightOf (l, ls, n+1)
	  | moveRight (LeftOf (r, rs, 0))     = Middle (nil, r, rs)
	  | moveRight (LeftOf (r, rs,  n))    = LeftOf (r, rs, n-1)


	fun moveLeft (Empty) = Empty
	  | moveLeft (Middle (nil, m,   rs)) = LeftOf (m, rs, 0)
	  | moveLeft (Middle (l::ls, m, rs)) = Middle (ls, l, m::rs)
	  | moveLeft (RightOf (l, ls, 0))    = Middle (ls, l, nil)
	  | moveLeft (RightOf (l, ls, n))    = RightOf (l, ls, n-1)
	  | moveLeft (LeftOf (r, rs,  n))    = LeftOf (r, rs, n+1)


	fun move tape Stay  = tape
	  | move tape Right = moveRight tape
	  | move tape Left  = moveLeft  tape

	(* Test *)
	local
		val tape : int tape = empty		(* [] *)
		val tape = moveRight tape		(* [] *)
		val NONE = getSymbol tape
		val tape = write (SOME 42) tape		(* [42] *)
		val (SOME 42) = getSymbol tape
		val tape = moveRight tape		(* 42, [] *)
		val tape = moveRight tape		(* 42, , [] *)
		val NONE = getSymbol tape
		val tape = moveLeft tape		(* 42, [] *)
		val NONE = getSymbol tape
		val tape = moveLeft tape		(* [42] *)
		val (SOME 42) = getSymbol tape
		val tape = write NONE tape		(* [42] *) (* !!! *)
		val (SOME 42) = getSymbol tape
		val tape = moveLeft tape		(* [], 42 *)
		val tape = moveLeft tape		(* [], , 42 *)
		val tape = write (SOME 47) tape		(* [47], , 42 *)
		val (SOME 47) = getSymbol tape
		val tape = moveRight tape		(* 47, [], 42 *)
		val NONE = getSymbol tape
		val tape = moveRight tape		(* 47, , [42] *)
		val (SOME 42) = getSymbol tape
	in end
end

structure Machine :> MACHINE = struct
	structure Tape = Tape

	type state = int

	(* ''a is band alphabet type *)
	type ''a transitions = (state * ''a option Vector.vector) ->
	                       (state * (Tape.move * ''a option) Vector.vector)

	type ''a configuration = { state : state, tapes : ''a Tape.tape Vector.vector }

	type ''a machine = {
			alphabet : ''a Vector.vector,
			states : state Vector.vector,
			start  : state,
			final  : state,
			transitions : ''a transitions,
			tapes  : ''a Tape.tape Vector.vector
		}
end

structure UniversalMachine :> UNIVERSALMACHINE = struct

	structure Machine = Machine

	fun start ({ start, tapes, ... } : ''a Machine.machine) : ''a Machine.configuration = {
			state = start,
			tapes = tapes
		}

	fun doTransition ({ state, tapes } : ''a Machine.configuration)
	    ((state', actions) : (Machine.state * (Machine.Tape.move * ''a option) Vector.vector))
		: ''a Machine.configuration = {
			state = state',
			tapes = Vector.mapi (fn (i, tape) =>
					let val (move, write) = Vector.sub (actions, i)
					    val tape'  = Machine.Tape.write write tape
					    val tape'' = Machine.Tape.move tape' move
					in tape'' end) tapes
		}

	fun getSymbols ({ tapes, ... } : ''a Machine.configuration) : ''a option Vector.vector =
		Vector.map (Machine.Tape.getSymbol) tapes

	fun step ({ transitions, ... } : ''a Machine.machine) (conf : ''a Machine.configuration) : ''a Machine.configuration =
		doTransition conf (transitions (#state conf, getSymbols conf))

	fun isFinal ({final, ...} : ''a Machine.machine) ({state, ...} : ''a Machine.configuration) : bool =
		final = state

	fun iter term (SOME 0) f s = NONE
	  | iter term (SOME n) f s = if term s then SOME s else iter term (SOME (n-1)) f (f s)
	  | iter term NONE     f s = if term s then SOME s else iter term NONE f (f s)


	fun simulate handler (machine : ''a Machine.machine) optcount =
		let val endconf = iter (isFinal machine) optcount (fn conf => (handler conf; step machine conf)) (start machine)
		in case endconf of NONE => NONE | SOME conf => (handler conf; endconf) end

end


structure ExampleMachines = struct

	structure Machine = UniversalMachine.Machine

	(* Tranform the 5-Tuple notation into the vector function *)
	fun makeTransitions nil : ''a Machine.transitions = (fn (t, vec) => (print (Int.toString t); raise Subscript))
	  | makeTransitions ((s : Machine.state,
	  		      read : ''a option,
	                      write : ''a option,
			      move : Machine.Tape.move,
			      s' : Machine.state) :: ts) =
	  	fn (t, vec) =>
	  		if s=t andalso vec=(Vector.fromList [read])
		        then (s', Vector.fromList [(move, write)])
			else makeTransitions ts (t, vec)

	(* `createTape xs` creates an tape initialized by xs, where the head stands on the first element of xs *)
	fun createTape' nil     = Machine.Tape.empty
	  | createTape' (x::xs) = Machine.Tape.moveLeft (Machine.Tape.write x (createTape' xs))

	fun createTape xs = Machine.Tape.moveRight (createTape' (rev xs))


	(* Convert a tape into a string to print it. It needs a function that converts each symbol to string *)
	fun tapeToStr (symStr : ''a -> string) (tape : ''a Machine.Tape.tape) : string =
		let val left     : ''a option list = rev (Machine.Tape.leftOf tape)
		    val right    : ''a option list = Machine.Tape.rightOf tape
		    val current  : ''a option      = Machine.Tape.getSymbol tape
		    val symToStr : ''a option -> string = (fn (NONE) => "#" | (SOME a) => symStr a)
		in
		    String.concatWith " " ((map symToStr left) @ [ "|" ^ symToStr current ^ "|" ] @ (map symToStr right))
		end

	(* Convert a vector to a list *)
	fun vectToList vect = List.tabulate (Vector.length vect, fn i => Vector.sub (vect, i))


	(* Do this before every step and after the last step. *)
	fun handler (symToStr : ''a -> string) ({state, tapes} : ''a Machine.configuration) : unit =
		let
			val str = "State " ^ Int.toString state ^ "\n" ^
			String.concat (vectToList (Vector.mapi (fn (i, tape) => "Tape #" ^ Int.toString i ^ ": " ^
			tapeToStr symToStr tape ^ "\n") tapes))
		in
			print str
		end


	(* Simulate and make result into string *)
	fun simulate (symToStr : ''a -> string) (machine : ''a Machine.machine)
	    (optcount : int option) : string =
		case (UniversalMachine.simulate (handler symToStr) machine optcount) of
		  NONE => "Did not terminate."
		| SOME ({state, tapes} : ''a Machine.configuration) => "Terminated."



	(* Now finaly the machines! *)

	val incrementer : unit Machine.machine = {
		alphabet = Vector.fromList [()],
		states   = Vector.fromList [0, 1],
		start    = 0,
		final    = 1,
		tapes    = Vector.fromList [createTape (map SOME [(), (), (), ()])],
		transitions = makeTransitions [
			(0, SOME (), SOME (), Machine.Tape.Right, 0),
			(0, NONE,    SOME (), Machine.Tape.Stay,  1)]
	}

	val busybeaver : unit Machine.machine = {
		alphabet = Vector.fromList [()],
		states   = Vector.fromList [0, 1, 2, 3],
		start    = 0,
		final    = 3,
		tapes    = Vector.fromList [Machine.Tape.empty],
		transitions = makeTransitions [
			(0, NONE,    SOME (), Machine.Tape.Right, 1),
			(0, SOME (), SOME (), Machine.Tape.Left,  2),
			(1, NONE,    SOME (), Machine.Tape.Left,  0),
			(1, SOME (), SOME (), Machine.Tape.Right, 1),
			(2, NONE,    SOME (), Machine.Tape.Left,  1),
			(2, SOME (), SOME (), Machine.Tape.Stay,  3)]
	}

	val sorting : int Machine.machine = {
		alphabet = Vector.fromList [1,2,3],
		states   = Vector.fromList [0,1,2,3,4,5],
		start    = 1,
		final    = 0,
		tapes    = Vector.fromList [createTape (map SOME [2, 1, 2, 2, 1, 1])],
		transitions = makeTransitions [
			(1, SOME 1, SOME 1, Machine.Tape.Right, 1),
			(1, SOME 2, SOME 3, Machine.Tape.Right, 2),
			(1, NONE,   NONE,   Machine.Tape.Left,  5),
			(2, SOME 1, SOME 1, Machine.Tape.Right, 2),
			(2, SOME 2, SOME 2, Machine.Tape.Right, 2),
			(2, NONE,   NONE,   Machine.Tape.Left,  3),
			(3, SOME 1, SOME 2, Machine.Tape.Left,  3),
			(3, SOME 2, SOME 2, Machine.Tape.Left,  3),
			(3, SOME 3, SOME 2, Machine.Tape.Left,  5),
			(4, SOME 1, SOME 1, Machine.Tape.Left,  4),
			(4, SOME 2, SOME 2, Machine.Tape.Left,  4),
			(4, SOME 3, SOME 1, Machine.Tape.Right, 1),
			(5, SOME 1, SOME 1, Machine.Tape.Left,  5),
			(5, NONE,   NONE,   Machine.Tape.Right, 0)]
		}
end

(** Invoke Simulations **)
local
	open ExampleMachines
	val unitToString = (fn () => "()")
	fun simulate_unit machine optcount = print (simulate unitToString machine optcount ^ "\n")
	fun simulate_int  machine optcount = print (simulate Int.toString machine optcount ^ "\n")
in
	val () = print "Simulate incrementer...\n\n"
	val () = simulate_unit incrementer NONE
	val () = print "\nSimulate Busy Beaver...\n\n"
	val () = simulate_unit busybeaver NONE
	val () = print "\nSimulate Sorting...\n\n"
	val () = simulate_int sorting NONE
end

```


{{output}}

```txt
Simulate incrementer...

State 0
Tape #0: |()| () () ()
State 0
Tape #0: () |()| () ()
State 0
Tape #0: () () |()| ()
State 0
Tape #0: () () () |()|
State 0
Tape #0: () () () () |#|
State 1
Tape #0: () () () () |()|
Terminated.

Simulate Busy Beaver...

State 0
Tape #0: |#|
State 1
Tape #0: () |#|
State 0
Tape #0: |()| ()
State 2
Tape #0: |#| () ()
State 1
Tape #0: |#| () () ()
State 0
Tape #0: |#| () () () ()
State 1
Tape #0: () |()| () () ()
State 1
Tape #0: () () |()| () ()
State 1
Tape #0: () () () |()| ()
State 1
Tape #0: () () () () |()|
State 1
Tape #0: () () () () () |#|
State 0
Tape #0: () () () () |()| ()
State 2
Tape #0: () () () |()| () ()
State 3
Tape #0: () () () |()| () ()
Terminated.

Simulate Sorting...

State 1
Tape #0: |1| 1 2 2 1 2
State 1
Tape #0: 1 |1| 2 2 1 2
State 1
Tape #0: 1 1 |2| 2 1 2
State 2
Tape #0: 1 1 3 |2| 1 2
State 2
Tape #0: 1 1 3 2 |1| 2
State 2
Tape #0: 1 1 3 2 1 |2|
State 2
Tape #0: 1 1 3 2 1 2 |#|
State 3
Tape #0: 1 1 3 2 1 |2|
State 3
Tape #0: 1 1 3 2 |1| 2
State 3
Tape #0: 1 1 3 |2| 2 2
State 3
Tape #0: 1 1 |3| 2 2 2
State 5
Tape #0: 1 |1| 2 2 2 2
State 5
Tape #0: |1| 1 2 2 2 2
State 5
Tape #0: |#| 1 1 2 2 2 2
State 0
Tape #0: |1| 1 2 2 2 2
Terminated.

```




## Tcl


```tcl
proc turing {states initial terminating symbols blank tape rules {doTrace 1}} {
    set state $initial
    set idx 0
    set tape [split $tape ""]
    if {[llength $tape] == 0} {
	set tape [list $blank]
    }
    foreach rule $rules {
	lassign $rule state0 sym0 sym1 move state1
	set R($state0,$sym0) [list $sym1 $move $state1]
    }
    while {$state ni $terminating} {
	set sym [lindex $tape $idx]
	lassign $R($state,$sym) sym1 move state1
	if {$doTrace} {
	    ### Print the state, great for debugging
	    puts "[join $tape ""]\t$state->$state1"
	    puts "[string repeat { } $idx]^"
	}
	lset tape $idx $sym1
	switch $move {
	    left {
		if {[incr idx -1] < 0} {
		    set idx 0
		    set tape [concat [list $blank] $tape]
		}
	    }
	    right {
		if {[incr idx] == [llength $tape]} {
		    lappend tape $blank
		}
	    }
	}
	set state $state1
    }
    return [join $tape ""]
}
```

Demonstrating:

```tcl
puts "Simple incrementer"
puts TAPE=[turing {q0 qf} q0 qf {1 B} B "111" {
    {q0 1 1 right q0}
    {q0 B 1 stay qf}
}]
puts "Three-state busy beaver"
puts TAPE=[turing {a b c halt} a halt {0 1} 0 "" {
    {a 0 1 right b}
    {a 1 1 left c}
    {b 0 1 left a}
    {b 1 1 right b}
    {c 0 1 left b}
    {c 1 1 stay halt}
}]
puts "Sorting stress test"
# We suppress the trace output for this so as to keep the output short
puts TAPE=[turing {A B C D E H} A H {0 1 2 3} 0 "12212212121212" {
    {A 1 1 right A}
    {A 2 3 right B}
    {A 0 0 left E}
    {B 1 1 right B}
    {B 2 2 right B}
    {B 0 0 left C}
    {C 1 2 left D}
    {C 2 2 left C}
    {C 3 2 left E}
    {D 1 1 left D}
    {D 2 2 left D}
    {D 3 1 right A}
    {E 1 1 left E}
    {E 0 0 right H}
} no]
```

{{out}}

```txt

Simple incrementer
111	q0->q0
^
111	q0->q0
 ^
111	q0->q0
  ^
111B	q0->qf
   ^
TAPE=1111
Three-state busy beaver
0	a->b
^
10	b->a
 ^
11	a->c
^
011	c->b
^
0111	b->a
^
01111	a->b
^
11111	b->b
 ^
11111	b->b
  ^
11111	b->b
   ^
11111	b->b
    ^
111110	b->a
     ^
111111	a->c
    ^
111111	c->halt
   ^
TAPE=111111
Sorting stress test
TAPE=0111111222222220

```



## UNIX Shell

{{works with|Bourne Again Shell|4+}}

```sh
#!/usr/bin/env bash
main() {
  printf 'Simple Incrementer\n'
  printf '1 1 1' | run_utm q0 qf B q0,1,1,R,q0 q0,B,1,S,qf

  printf '\nThree-state busy beaver\n'
  run_utm a halt 0 \
    a,0,1,R,b a,1,1,L,c b,0,1,L,a b,1,1,R,b c,0,1,L,b c,1,1,S,halt \
    </dev/null
}

run_utm() {
  local initial=$1 final=$2 blank=$3
  shift 3
  local rules=("$@") tape
  mapfile -t -d' ' tape
  if (( ! ${#tape[@]} )); then
    tape=( "$blank" )
  fi
  local state=$initial
  local head=0
  while [[ $state != $final ]]; do
    print_state "$state" "$head" "${tape[@]}"
    local symbol=${tape[head]}
    local found=0 rule from input output move to
    for rule in "${rules[@]}"; do
      IFS=, read from input output move to <<<"$rule"
      if [[ $state == $from && $symbol == $input ]]; then
        found=1
        break
      fi
    done
    if (( ! found )); then
        printf >&2 "Configuration error: no match for state=$state input=$sym\n"
        return 1
    fi
    tape[head]=$output
    state=$to
    case "$move" in
     L) if (( ! head-- )); then
          head=0
          tape=("$blank" "${tape[@]}")
        fi
        ;;
     R) if (( ++head >= ${#tape[@]} )); then
          tape+=("$blank")
        fi
        ;;
    esac
  done
  print_state "$state" "$head" "${tape[@]}"
}

print_state() {
  local state=$1 head=$2
  shift 2
  local tape=("$@")
  printf '%s' "$state"
  printf '  %s' "${tape[@]}"
  printf '\r'
  (( t = ${#state} + 1 + 3 * head ))
  printf '\e['"$t"'C<\e[C>\n'
}

main "$@"

```

{{Output}}

```txt
Simple Incrementer
q0 <1> 1  1
q0  1 <1> 1
q0  1  1 <1>
q0  1  1  1 <B>
qf  1  1  1 <1>

Three-state busy beaver
a <0>
b  1 <0>
a <1> 1
c <0> 1  1
b <0> 1  1  1
a <0> 1  1  1  1
b  1 <1> 1  1  1
b  1  1 <1> 1  1
b  1  1  1 <1> 1
b  1  1  1  1 <1>
b  1  1  1  1  1 <0>
a  1  1  1  1 <1> 1
c  1  1  1 <1> 1  1
halt  1  1  1 <1> 1  1
```



## VBA

{{trans|Phix}}
```vb
Option Base 1
Public Enum sett
    name_ = 1
    initState
    endState
    blank
    rules
End Enum
Public incrementer As Variant, threeStateBB As Variant, fiveStateBB As Variant
'-- Machine definitions
Private Sub init()
    incrementer = Array("Simple incrementer", _
        "q0", _
        "qf", _
        "B", _
         Array( _
         Array("q0", "1", "1", "right", "q0"), _
         Array("q0", "B", "1", "stay", "qf")))
    threeStateBB = Array("Three-state busy beaver", _
       "a", _
       "halt", _
       "0", _
        Array( _
        Array("a", "0", "1", "right", "b"), _
        Array("a", "1", "1", "left", "c"), _
        Array("b", "0", "1", "left", "a"), _
        Array("b", "1", "1", "right", "b"), _
        Array("c", "0", "1", "left", "b"), _
        Array("c", "1", "1", "stay", "halt")))
    fiveStateBB = Array("Five-state busy beaver", _
        "A", _
        "H", _
        "0", _
         Array( _
         Array("A", "0", "1", "right", "B"), _
         Array("A", "1", "1", "left", "C"), _
         Array("B", "0", "1", "right", "C"), _
         Array("B", "1", "1", "right", "B"), _
         Array("C", "0", "1", "right", "D"), _
         Array("C", "1", "0", "left", "E"), _
         Array("D", "0", "1", "left", "A"), _
         Array("D", "1", "1", "left", "D"), _
         Array("E", "0", "1", "stay", "H"), _
         Array("E", "1", "0", "left", "A")))
End Sub

Private Sub show(state As String, headpos As Long, tape As Collection)
    Debug.Print " "; state; String$(7 - Len(state), " "); "| ";
    For p = 1 To tape.Count
        Debug.Print IIf(p = headpos, "[" & tape(p) & "]", " " & tape(p) & " ");
    Next p
    Debug.Print
End Sub

'-- a universal turing machine
Private Sub UTM(machine As Variant, tape As Collection, Optional countOnly As Long = 0)
    Dim state As String: state = machine(initState)
    Dim headpos As Long: headpos = 1
    Dim counter As Long, rule As Variant
    Debug.Print machine(name_); vbCrLf; String$(Len(machine(name_)), "=")
    If Not countOnly Then Debug.Print " State  | Tape [head]" & vbCrLf & "---------------------"
    Do While True
        If headpos > tape.Count Then
            tape.Add machine(blank)
        Else
            If headpos < 1 Then
                tape.Add machine(blank), Before:=1
                headpos = 1
            End If
        End If
        If Not countOnly Then show state, headpos, tape
        For i = LBound(machine(rules)) To UBound(machine(rules))
            rule = machine(rules)(i)
            If rule(1) = state And rule(2) = tape(headpos) Then
                tape.Remove headpos
                If headpos > tape.Count Then
                    tape.Add rule(3)
                Else
                    tape.Add rule(3), Before:=headpos
                End If
                If rule(4) = "left" Then headpos = headpos - 1
                If rule(4) = "right" Then headpos = headpos + 1
                state = rule(5)
                Exit For
            End If
        Next i
        counter = counter + 1
        If counter Mod 100000 = 0 Then
            Debug.Print counter
            DoEvents
            DoEvents
        End If
        If state = machine(endState) Then Exit Do
    Loop
    DoEvents
    If countOnly Then
        Debug.Print "Steps taken: ", counter
    Else
        show state, headpos, tape
        Debug.Print
    End If
End Sub

Public Sub main()
    init
    Dim tap As New Collection
    tap.Add "1": tap.Add "1": tap.Add "1"
    UTM incrementer, tap
    Set tap = New Collection
    UTM threeStateBB, tap
    Set tap = New Collection
    UTM fiveStateBB, tap, countOnly:=-1
End Sub
```
{{out}}

```txt
Simple incrementer

### ============

 State  | Tape [head]
---------------------
 q0     | [1] 1  1
 q0     |  1 [1] 1
 q0     |  1  1 [1]
 q0     |  1  1  1 [B]
 qf     |  1  1  1 [1]

Three-state busy beaver

### =================

 State  | Tape [head]
---------------------
 a      | [0]
 b      |  1 [0]
 a      | [1] 1
 c      | [0] 1  1
 b      | [0] 1  1  1
 a      | [0] 1  1  1  1
 b      |  1 [1] 1  1  1
 b      |  1  1 [1] 1  1
 b      |  1  1  1 [1] 1
 b      |  1  1  1  1 [1]
 b      |  1  1  1  1  1 [0]
 a      |  1  1  1  1 [1] 1
 c      |  1  1  1 [1] 1  1
 halt   |  1  1  1 [1] 1  1

Five-state busy beaver

### ================

Steps taken: 47176870
```



## Yabasic

{{trans|Lua}}

```Yabasic
// Machine definitions

name = 1 : initState = 2 : endState = 3 : blank = 4 : countOnly = true

incrementer$ = "Simple incrementer,q0,qf,B"
incrementer$ = incrementer$ + ",q0,1,1,right,q0,q0,B,1,stay,qf"

threeStateBB$ = "Three-state busy beaver,a,halt,0"
data "a,0,1,right,b"
data "a,1,1,left,c"
data "b,0,1,left,a"
data "b,1,1,right,b"
data "c,0,1,left,b"
data "c,1,1,stay,halt"
data ""

do
    read a$
    if a$ = "" break
    threeStateBB$ = threeStateBB$ + "," + a$
loop


fiveStateBB$ = "Five-state busy beaver,A,H,0"
data "A,0,1,right,B"
data "A,1,1,left,C"
data "B,0,1,right,C"
data "B,1,1,right,B"
data "C,0,1,right,D"
data "C,1,0,left,E"
data "D,0,1,left,A"
data "D,1,1,left,D"
data "E,0,1,stay,H"
data "E,1,0,left,A"
data ""

do
    read a$
    if a$ = "" break
    fiveStateBB$ = fiveStateBB$ + "," + a$
loop

clear screen

// Display a representation of the tape and machine state on the screen
sub show(state$, headPos, tape$)
    local pos

    print " ", state$, "\t| ";
    for pos = 1 to len(tape$)
        if pos = headPos then print "[", mid$(tape$, pos, 1), "] "; else print " ", mid$(tape$, pos, 1), "  "; end if
    next
    print
end sub

sub string.rep$(s$, n)
    local i, r$

    for i = 1 to n
        r$ = r$ + s$
    next

    return r$
end sub


// Simulate a turing machine
sub UTM(mach$, tape$, countOnly)
    local state$, headPos, counter, machine$(1), n, m, rule

    m = len(tape$)
    n = token(mach$, machine$(), ",")
    state$ = machine$(initState)
    n = n - blank
    headPos = 1

    print "\n\n", machine$(name)
    print string.rep$("=", len(machine$(name))), "\n"
    if not countOnly print " State", "\t| Tape [head]\n----------------------"

    repeat
        if mid$(tape$, headPos, 1) = " " mid$(tape$, headPos, 1) = machine$(blank)
        if not countOnly show(state$, headPos, tape$)
        for rule = blank + 1 to n step 5
            if machine$(rule) = state$ and machine$(rule + 1) = mid$(tape$, headPos, 1) then
                mid$(tape$, headPos, 1) = machine$(rule + 2)
                if machine$(rule + 3) = "left" then
                    headPos = headPos - 1
                    if headPos < 1  then
                        headPos = 1
                        tape$ = " " + tape$
                    end if
                end if
                if machine$(rule + 3) = "right" then
                    headPos = headPos + 1
                    if headPos > m then
                        m = m + 1
                        tape$ = tape$ + " "
                    end if
                end if
                state$ = machine$(rule + 4)
                break
            end if
        next
        counter = counter + 1
    until(state$ = machine$(endState))
    if countOnly then print "Steps taken: ", counter else show(state$, headPos, tape$) end if
end sub

// Main procedure
UTM(incrementer$, "111")
UTM(threeStateBB$, " ")
UTM(fiveStateBB$, " ", countOnly)
```



## zkl

This uses a dictionary/hash to hold the tape, limiting the length to 64k.
{{Trans|D}}

```zkl
var [const] D=Dictionary;	// short cut
// blank symbol and terminating state(s) are Void
var Lt=-1, Sy=0, Rt=1;  // Left, Stay, Right

fcn printTape(tape,pos){
   tape.keys.apply("toInt").sort()
      .pump(String,'wrap(i){ ((pos==i) and "(%s)" or " %s ").fmt(tape[i]) })
      .println();
}
fcn turing(state,[D]tape,[Int]pos,[D]rules,verbose=True,n=0){
   if(not state){
      print("%d steps. Length %d. Tape: ".fmt(n,tape.len()));
      printTape(tape,Void);
      return(tape);
   }
   r:=rules[state][tape[pos] = tape.find(pos)];
   if(verbose) printTape(tape,pos);
   tape[pos]=r[0];
   return(self.fcn(r[2],tape,pos+r[1],rules,verbose,n+1));
}
```

D is a dictionary, SD is a small fixed (at runtime) dictionary

```zkl
println("Simple incrementer");
turing("q0",D(0,Rt, 1,Rt, 2,Rt),0,  // Incrementer
        D("q0",D(1,T(1,Rt,"q0"), Void,T(1,Sy,Void)) ) );

println("\nThree-state busy beaver");
turing("a",D(),0,  // Three-state busy beaver
        SD("a",D(Void,T(1,Rt,"b"), 1,T(1,Lt,"c")),
	   "b",D(Void,T(1,Lt,"a"), 1,T(1,Rt,"b")),
	   "c",D(Void,T(1,Lt,"b"), 1,T(1,Sy,Void)) ) );

println("\nSort");
turing("A",D(T(2,2,2,1,2,2,1,2,1,2,1,2,1,2).enumerate()),0,
        SD("A",D(1,T(1,Rt,"A"), 2,T(3,Rt,"B"),  Void,T(Void,Lt,"E")),
	   "B",D(1,T(1,Rt,"B"), 2,T(2,Rt,"B"),  Void,T(Void,Lt,"C")),
	   "C",D(1,T(2,Lt,"D"), 2,T(2,Lt,"C"),  3,   T(2,   Lt,"E")),
	   "D",D(1,T(1,Lt,"D"), 2,T(2,Lt,"D"),  3,   T(1,   Rt,"A")),
	   "E",D(1,T(1,Lt,"E"),                 Void,T(Void,Rt,Void)) ) ,False);

println("\nFive-state busy beaver");
turing("A",D(),0,
        SD("A",D(Void,T(1,Rt,"B"),  1,T(1,   Lt,"C")),
	   "B",D(Void,T(1,Rt,"C"),  1,T(1,   Rt,"B")),
	   "C",D(Void,T(1,Rt,"D"),  1,T(Void,Lt,"E")),
	   "D",D(Void,T(1,Lt,"A"),  1,T(1,   Lt,"D")),
	   "E",D(Void,T(1,Sy,Void), 1,T(Void,Lt,"A")) ) ,False);
```

{{out}}

```txt

Simple incrementer
(1) 1  1
 1 (1) 1
 1  1 (1)
 1  1  1 (Void)
4 steps. Lenght 4. Tape:  1  1  1  1

Three-state busy beaver
(Void)
 1 (Void)
(1) 1
(Void) 1  1
(Void) 1  1  1
(Void) 1  1  1  1
 1 (1) 1  1  1
 1  1 (1) 1  1
 1  1  1 (1) 1
 1  1  1  1 (1)
 1  1  1  1  1 (Void)
 1  1  1  1 (1) 1
 1  1  1 (1) 1  1
13 steps. Lenght 6. Tape:  1  1  1  1  1  1

Sort
128 steps. Length 16. Tape:  Void  1  1  1  1  1  2  2  2  2  2  2  2  2  2  Void

Five-state busy beaver
47176870 steps. Length 12289. Tape:  1  Void  1  Void  Void  1  Void  Void  1
Void  Void  1  Void  Void  1  Void  Void  1  Void  Void  1  Void  Void  1  Void
Void  1  Void  Void  1  Void  Void  1  Void  Void  1  Void  Void  1  Void  Void
....
Read: Void==0 as 0 is the blank symbol

```

