+++
title = "One-dimensional cellular automata"
description = ""
date = 2019-10-09T18:49:34Z
aliases = []
[extra]
id = 3072
[taxonomies]
categories = ["task", "Games"]
tags = []
languages = [
  "8th",
  "acl2",
  "ada",
  "algol_68",
  "algol_w",
  "autohotkey",
  "awk",
  "basic",
  "batch_file",
  "bbc_basic",
  "befunge",
  "bracmat",
  "c",
  "cells_are_alive_if_true_dead_if_false",
  "ceylon",
  "clojure",
  "cobol",
  "common_lisp",
  "cpp",
  "csharp",
  "d",
  "dwscript",
  "e",
  "eiffel",
  "elixir",
  "elm",
  "erlang",
  "erre",
  "euphoria",
  "factor",
  "fantom",
  "forth",
  "fortran",
  "gfa_basic",
  "go",
  "groovy",
  "haskell",
  "in_which_x_stays_alive",
  "j",
  "java",
  "javascript",
  "jq",
  "julia",
  "k",
  "kotlin",
  "liberty_basic",
  "list_of_patterns_in_which_the_cell_stays_alive",
  "locomotive_basic",
  "logo",
  "lua",
  "m4",
  "map_list_of_length_3_logical_vectors_that_map_to_patterns",
  "montilang",
  "nial",
  "nim",
  "ocaml",
  "oforth",
  "oz",
  "pari_gp",
  "pascal",
  "perl",
  "perl_6",
  "phix",
  "picolisp",
  "prolog",
  "purebasic",
  "python",
  "r",
  "racket",
  "retro",
  "rexx",
  "ring",
  "ruby",
  "rust",
  "scala",
  "scheme",
  "seed7",
  "sequencel",
  "sidef",
  "sinclair_zx81_basic",
  "tcl",
  "ursala",
  "vedit_macro_language",
  "visual_basic_dotnet",
  "wart",
  "x_length_3_logical_vector",
  "xpl0",
  "yabasic",
  "zkl",
]
+++

## Task

{{task|Games}} [[Category:Cellular automata]]
Assume an array of cells with an initial distribution of live and dead cells,
and imaginary cells off the end of the array having fixed values.

Cells in the next generation of the array are calculated based on the value of the cell and its left and right nearest neighbours in the current generation.

If, in the following table, a live cell is represented by 1 and a dead cell by 0 then to generate the value of the cell at a particular index in the array of cellular values you use the following table:

 0'''0'''0 -> 0  #
 0'''0'''1 -> 0  #
 0'''1'''0 -> 0  # Dies without enough neighbours
 0'''1'''1 -> 1  # Needs one neighbour to survive
 1'''0'''0 -> 0  #
 1'''0'''1 -> 1  # Two neighbours giving birth
 1'''1'''0 -> 1  # Needs one neighbour to survive
 1'''1'''1 -> 0  # Starved to death.


## 8th


```forth

\ one-dimensional automaton

\ direct map of input state to output state:
{
  "   " : 32,
  "  #" : 32,
  " # " : 32,
  " ##" : 35,
  "#  " : 32,
  "# #" : 35,
  "## " : 35,
  "###" : 32,
} var, lifemap

: transition \ s ix (r:s') -- (r:s')
    >r dup r@ n:1- 3 s:slice
    lifemap @ swap caseof
    r> swap r@ -rot s:! >r ;

\ run over 'state' and generate new state
: gen \ s -- s'
  clone >r
  dup s:len 2 n:-
  ' transition 1 rot loop
  drop r> ;

: life \ s -- s'
  dup . cr gen  ;

" ### ## # # # #  #  " ' life 10 times
bye


```



## ACL2


```lisp
(defun rc-step-r (cells)
   (if (endp (rest cells))
       nil
       (cons (if (second cells)
                 (xor (first cells) (third cells))
                 (and (first cells) (third cells)))
             (rc-step-r (rest cells)))))

(defun rc-step (cells)
   (cons (and (first cells) (second cells))
         (rc-step-r cells)))

(defun rc-steps-r (cells n prev)
   (declare (xargs :measure (nfix n)))
   (if (or (zp n) (equal cells prev))
       nil
       (let ((new (rc-step cells)))
          (cons new (rc-steps-r new (1- n) cells)))))

(defun rc-steps (cells n)
  (cons cells (rc-steps-r cells n nil)))

(defun pretty-row (row)
   (if (endp row)
       (cw "~%")
       (prog2$ (cw (if (first row) "#" "-"))
               (pretty-row (rest row)))))

(defun pretty-output (out)
   (if (endp out)
       nil
       (prog2$ (pretty-row (first out))
               (pretty-output (rest out)))))
```



## Ada


```ada
with Ada.Text_IO;  use Ada.Text_IO;

procedure Cellular_Automata is
   type Petri_Dish is array (Positive range <>) of Boolean;

   procedure Step (Culture : in out Petri_Dish) is
      Left  : Boolean := False;
      This  : Boolean;
      Right : Boolean;
   begin
      for Index in Culture'First..Culture'Last - 1 loop
         Right := Culture (Index + 1);
         This  := Culture (Index);
         Culture (Index) := (This and (Left xor Right)) or (not This and Left and Right);
         Left := This;
      end loop;
      Culture (Culture'Last) := Culture (Culture'Last) and not Left;
   end Step;

   procedure Put (Culture : Petri_Dish) is
   begin
      for Index in Culture'Range loop
         if Culture (Index) then
            Put ('#');
         else
            Put ('_');
         end if;
      end loop;
   end Put;

   Culture : Petri_Dish :=
      (  False, True, True,  True, False, True,  True, False, True, False, True,
         False, True, False, True, False, False, True, False, False
      );
begin
   for Generation in 0..9 loop
      Put ("Generation" & Integer'Image (Generation) & ' ');
      Put (Culture);
      New_Line;
      Step (Culture);
   end loop;
end Cellular_Automata;
```


The implementation defines Petri dish type with Boolean items
identifying whether a place is occupied by a living cell.
State transition is determined by a simple Boolean expression of three arguments.
```txt

Generation 0 _###_##_#_#_#_#__#__
Generation 1 _#_#####_#_#_#______
Generation 2 __##___##_#_#_______
Generation 3 __##___###_#________
Generation 4 __##___#_##_________
Generation 5 __##____###_________
Generation 6 __##____#_#_________
Generation 7 __##_____#__________
Generation 8 __##________________
Generation 9 __##________________

```


## ALGOL 68


### Using the low level packed arrays of BITS manipulation operators

```algol68
INT stop generation = 9;
INT universe width = 20;
FORMAT alive or dead = $b("#","_")$;

BITS universe := 2r01110110101010100100;
   # universe := BIN ( ENTIER ( random * max int ) ); #
INT upb universe = bits width;
INT lwb universe = bits width - universe width + 1;

PROC couple = (BITS parent, INT lwb, upb)BOOL: (
  SHORT INT sum := 0;
  FOR bit FROM lwb TO upb DO
    sum +:= ABS (bit ELEM parent)
  OD;
  sum = 2
);

FOR generation FROM 0 WHILE
  printf(($"Generation "d": "$, generation,
         $f(alive or dead)$, []BOOL(universe)[lwb universe:upb universe],
         $l$));
# WHILE # generation < stop generation DO
  BITS next universe := 2r0;

  # process the first event horizon manually #
  IF couple(universe,lwb universe,lwb universe + 1) THEN
    next universe := 2r10
  FI;

  # process the middle kingdom in a loop #
  FOR bit FROM lwb universe + 1 TO upb universe - 1 DO
    IF couple(universe,bit-1,bit+1) THEN
      next universe := next universe OR 2r1
    FI;
    next universe := next universe SHL 1
  OD;

  # process the last event horizon manually #
  IF couple(universe, upb universe - 1, upb universe) THEN
    next universe := next universe OR 2r1
  FI;
  universe := next universe
OD
```

```txt

Generation 0: _###_##_#_#_#_#__#__
Generation 1: _#_#####_#_#_#______
Generation 2: __##___##_#_#_______
Generation 3: __##___###_#________
Generation 4: __##___#_##_________
Generation 5: __##____###_________
Generation 6: __##____#_#_________
Generation 7: __##_____#__________
Generation 8: __##________________
Generation 9: __##________________

```



### Using high level BOOL arrays

```algol68
INT stop generation = 9;
INT upb universe = 20;
FORMAT alive or dead = $b("#","_")$;

BITS bits universe := 2r01110110101010100100;
   # bits universe := BIN ( ENTIER ( random * max int ) ); #
[upb universe] BOOL universe := []BOOL(bits universe)[bits width - upb universe + 1:];

PROC couple = (REF[]BOOL parent)BOOL: (
  SHORT INT sum := 0;
  FOR bit FROM LWB parent TO UPB parent DO
    sum +:= ABS (parent[bit])
  OD;
  sum = 2
);

FOR generation FROM 0 WHILE
  printf(($"Generation "d": "$, generation,
         $f(alive or dead)$, universe,
         $l$));
# WHILE # generation < stop generation DO
  [UPB universe]BOOL next universe;

  # process the first event horizon manually #
  next universe[1] := couple(universe[:2]);

  # process the middle kingdom in a loop #
  FOR bit FROM LWB universe + 1 TO UPB universe - 1 DO
    next universe[bit] := couple(universe[bit-1:bit+1])
  OD;

  # process the last event horizon manually #
  next universe[UPB universe] := couple(universe[UPB universe - 1: ]);
  universe := next universe
OD
```

```txt

Generation 0: _###_##_#_#_#_#__#__
Generation 1: _#_#####_#_#_#______
Generation 2: __##___##_#_#_______
Generation 3: __##___###_#________
Generation 4: __##___#_##_________
Generation 5: __##____###_________
Generation 6: __##____#_#_________
Generation 7: __##_____#__________
Generation 8: __##________________
Generation 9: __##________________

```



## ALGOL W

Using a string to represent the cells and stopping when the next state is th same as the previous one.

```algolw
begin
    string(20) state;
    string(20) nextState;
    integer    generation;
    generation := 0;
    state := "_###_##_#_#_#_#__#__";
    while begin
        write( i_w := 1, s_w := 1, "Generation ", generation, state );
        nextState := "____________________";
        for cPos := 1 until 18 do begin
            string(3) curr;
            curr := state( cPos - 1 // 3 );
            nextState( cPos // 1 ) := if curr = "_##" or curr = "#_#" or curr = "##_" then "#" else "_"
        end for_cPos ;
        ( state not = nextState )
    end do begin
        state := nextState;
        generation := generation + 1
    end while_not_finished
end.
```

```txt

Generation 0 _###_##_#_#_#_#__#__
Generation 1 _#_#####_#_#_#______
Generation 2 __##___##_#_#_______
Generation 3 __##___###_#________
Generation 4 __##___#_##_________
Generation 5 __##____###_________
Generation 6 __##____#_#_________
Generation 7 __##_____#__________
Generation 8 __##________________

```



## AutoHotkey

ahk [http://www.autohotkey.com/forum/viewtopic.php?t=44657&postdays=0&postorder=asc&start=147 discussion]

```autohotkey
n := 22, n1 := n+1, v0 := v%n1% := 0        ; set grid dimensions, and fixed cells

Loop % n {                                  ; draw a line of checkboxes
   v%A_Index% := 0
   Gui Add, CheckBox, % "y10 w17 h17 gCheck x" A_Index*17-5 " vv" A_Index
}
Gui Add, Button, x+5 y6, step               ; button to step to next generation
Gui Show
Return

Check:
   GuiControlGet %A_GuiControl%             ; set cells by the mouse
Return

ButtonStep:                                 ; move to next generation
   Loop % n
      i := A_Index-1, j := i+2, w%A_Index% := v%i%+v%A_Index%+v%j% = 2
   Loop % n
      GuiControl,,v%A_Index%, % v%A_Index% := w%A_Index%
Return

GuiClose:                                   ; exit when GUI is closed
ExitApp
```



## AWK



```awk
#!/usr/bin/awk -f
BEGIN {
    edge = 1
    ruleNum = 104 # 01101000
    maxGen = 9
    mark = "@"
    space = "."
    initialState = ".@@@.@@.@.@.@.@..@.."
    width = length(initialState)
    delete rules
    delete state

    initRules(ruleNum)
    initState(initialState, mark)
    for (g = 0; g < maxGen; g++) {
        showState(g, mark, space)
        nextState()
    }
    showState(g, mark, space)
}

function nextState(    newState, i, n) {
    delete newState
    for (i = 1; i < width - 1; i++) {
        n = getRuleNum(i)
        newState[i] = rules[n]
    }
    for (i = 0; i < width; i++) { # copy, can't assign arrays
        state[i] = newState[i]
    }
}

# Convert a three cell neighborhood from binary to decimal
function getRuleNum(i,    rn, j, p) {
    rn = 0
    for (j = -1; j < 2; j++) {
        p = i + j
        rn = rn * 2 + (p < 0 || p > width ? edge : state[p])
    }
    return rn
}

function showState(gen, mark, space,    i) {
    printf("%3d: ", gen)
    for (i = 1; i <= width; i++) {
        printf(" %s", (state[i] ? mark : space))
    }
    print ""
}

# Make state transition lookup table from rule number.
function initRules(ruleNum,   i, r) {
    delete rules;
    r = ruleNum
    for (i = 0; i < 8; i++) {
        rules[i] = r % 2
        r = int(r / 2)
    }
}

function initState(init, mark,    i) {
    delete state
    srand()
    for (i = 0; i < width; i++) {
        state[i] = (substr(init, i, 1) == mark ? 1 : 0) # Given an initial string.
        # state[int(width/2)] = '@'  # middle cell
        # state[i] = int(rand() * 100) < 30 ? 1 : 0 # 30% of cells
    }
}

```


```txt

  0:  . @ @ @ . @ @ . @ . @ . @ . @ . . @ . .
  1:  . @ . @ @ @ @ @ . @ . @ . @ . . . . . .
  2:  . . @ @ . . . @ @ . @ . @ . . . . . . .
  3:  . . @ @ . . . @ @ @ . @ . . . . . . . .
  4:  . . @ @ . . . @ . @ @ . . . . . . . . .
  5:  . . @ @ . . . . @ @ @ . . . . . . . . .
  6:  . . @ @ . . . . @ . @ . . . . . . . . .
  7:  . . @ @ . . . . . @ . . . . . . . . . .
  8:  . . @ @ . . . . . . . . . . . . . . . .
  9:  . . @ @ . . . . . . . . . . . . . . . .

```



```awk
Another new solution (twice size as previous solution) :
cat automata.awk :

#!/usr/local/bin/gawk -f

# User defined functions
function ASCII_to_Binary(str_) {
	gsub("_","0",str_); gsub("@","1",str_)
	return str_
}

function Binary_to_ASCII(bit_) {
	gsub("0","_",bit_); gsub("1","@",bit_)
	return bit_
}

function automate(b1,b2,b3) {
	a = and(b1,b2,b3)
	b = or(b1,b2,b3)
	c = xor(b1,b2,b3)
	d = a + b + c
	return d == 1 ? 1 : 0
}

# For each line in input do
{
str_ = $0
gen = 0
taille = length(str_)
print "0: " str_
do {
	gen ? str_previous = str_ : str_previous = ""
	gen += 1
	str_ = ASCII_to_Binary(str_)
	split(str_,tab,"")
	str_ = and(tab[1],tab[2])
	for (i=1; i<=taille-2; i++) {
		str_ = str_ automate(tab[i],tab[i+1],tab[i+2])
		}
	str_ = str_ and(tab[taille-1],tab[taille])
	print gen ": " Binary_to_ASCII(str_)
   } while (str_ != str_previous)
}

```


```txt

$ echo ".@@@.@@.@.@.@.@..@.." | awk -f automata.awk
0: .@@@.@@.@.@.@.@..@..
1: _@_@@@@@_@_@_@______
2: __@@___@@_@_@_______
3: __@@___@@@_@________
4: __@@___@_@@_________
5: __@@____@@@_________
6: __@@____@_@_________
7: __@@_____@__________
8: __@@________________
9: __@@________________

```



## BASIC

```qbasic
DECLARE FUNCTION life$ (lastGen$)
DECLARE FUNCTION getNeighbors! (group$)
CLS
start$ = "_###_##_#_#_#_#__#__"
numGens = 10
FOR i = 0 TO numGens - 1
	PRINT "Generation"; i; ": "; start$
	start$ = life$(start$)
NEXT i

FUNCTION getNeighbors (group$)
		ans = 0
		IF (MID$(group$, 1, 1) = "#") THEN ans = ans + 1
		IF (MID$(group$, 3, 1) = "#") THEN ans = ans + 1
		getNeighbors = ans
END FUNCTION

FUNCTION life$ (lastGen$)
		newGen$ = ""
		FOR i = 1 TO LEN(lastGen$)
			neighbors = 0
			IF (i = 1) THEN 'left edge
				IF MID$(lastGen$, 2, 1) = "#" THEN
					neighbors = 1
				ELSE
					neighbors = 0
				END IF
			ELSEIF (i = LEN(lastGen$)) THEN 'right edge
				IF MID$(lastGen$, LEN(lastGen$) - 1, 1) = "#" THEN
					neighbors = 1
				ELSE
					neighbors = 0
				END IF
			ELSE 'middle
				neighbors = getNeighbors(MID$(lastGen$, i - 1, 3))
			END IF

			IF (neighbors = 0) THEN 'dies or stays dead with no neighbors
				newGen$ = newGen$ + "_"
			END IF
			IF (neighbors = 1) THEN 'stays with one neighbor
				newGen$ = newGen$ + MID$(lastGen$, i, 1)
			END IF
			IF (neighbors = 2) THEN 'flips with two neighbors
				IF MID$(lastGen$, i, 1) = "#" THEN
					newGen$ = newGen$ + "_"
				ELSE
					newGen$ = newGen$ + "#"
				END IF
			END IF
		NEXT i
		life$ = newGen$
END FUNCTION
```

```txt
Generation 0 : _###_##_#_#_#_#__#__
Generation 1 : _#_#####_#_#_#______
Generation 2 : __##___##_#_#_______
Generation 3 : __##___###_#________
Generation 4 : __##___#_##_________
Generation 5 : __##____###_________
Generation 6 : __##____#_#_________
Generation 7 : __##_____#__________
Generation 8 : __##________________
Generation 9 : __##________________
```


=
## Sinclair ZX81 BASIC
=
Works with the unexpanded (1k RAM) ZX81.

```basic
 10 LET N$="01110110101010100100"
 20 LET G=1
 30 PRINT N$
 40 LET O$=N$
 50 LET N$=""
 60 PRINT AT 0,28;G
 70 LET N=0
 80 FOR I=1 TO LEN O$
 90 IF I=1 THEN GOTO 120
100 LET N=VAL O$(I-1)
110 IF I=LEN O$ THEN GOTO 130
120 LET N=N+VAL O$(I+1)
130 IF N=0 THEN LET N$=N$+"0"
140 IF N=1 THEN LET N$=N$+O$(I)
150 IF N=2 THEN LET N$=N$+STR$ NOT VAL O$(I)
160 PRINT AT 0,I-1;N$(I)
170 NEXT I
180 LET G=G+1
190 IF N$<>O$ THEN GOTO 40
```

The program overwrites each cell on the screen as it updates it (which it does quite slowly—there is no difficulty about watching what it is doing), with a counter to the right showing the generation it is currently working on. When it is part of the way through, for example, the display looks like this:

```txt
00110001011000000000        5
```

It halts when a stable state has been reached:

```txt
00110000000000000000        9
```



## Batch File

This implementation will not stop showing generations, unless the cellular automata is already stable.

```dos
@echo off
setlocal enabledelayedexpansion

::THE MAIN THING
call :one-dca __###__##_#_##_###__######_###_#####_#__##_____#_#_#######__
pause>nul
exit /b
::/THE MAIN THING

::THE PROCESSOR
:one-dca
echo.&set numchars=0&set proc=%1

::COUNT THE NUMBER OF CHARS
set bef=%proc:_=_,%
set bef=%bef:#=#,%
set bef=%bef:~0,-1%
for %%x in (%bef%) do set /a numchars+=1

set /a endchar=%numchars%-1
:nextgen
echo.   ^| %proc% ^|
set currnum=0
set newgen=
:editeachchar
	set neigh=0
	set /a testnum2=%currnum%+1
	set /a testnum1=%currnum%-1
	if %currnum%==%endchar% (
		set testchar=!proc:~%testnum1%,1!
		if !testchar!==# (set neigh=1)
	) else (
		if %currnum%==0 (
			set testchar=%proc:~1,1%
			if !testchar!==# (set neigh=1)
		) else (
			set testchar1=!proc:~%testnum1%,1!
			set testchar2=!proc:~%testnum2%,1!
			if !testchar1!==# (set /a neigh+=1)
			if !testchar2!==# (set /a neigh+=1)
		)
	)
	if %neigh%==0 (set newgen=%newgen%_)
	if %neigh%==1 (
		set testchar=!proc:~%currnum%,1!
		set newgen=%newgen%!testchar!
	)
	if %neigh%==2 (
		set testchar=!proc:~%currnum%,1!
		if !testchar!==# (set newgen=%newgen%_) else (set newgen=%newgen%#)
	)
if %currnum%==%endchar% (goto :cond) else (set /a currnum+=1&goto :editeachchar)

:cond
if %proc%==%newgen% (echo.&echo          ...The sample is now stable.&goto :EOF)
set proc=%newgen%
goto :nextgen
::/THE (LLLLLLOOOOOOOOOOOOONNNNNNNNGGGGGG.....) PROCESSOR
```

```txt
   | __###__##_#_##_###__######_###_#####_#__##_____#_#_#######__ |
   | __#_#__###_#####_#__#____###_###___##___##______#_##_____#__ |
   | ___#___#_###___##________#_###_#___##___##_______###________ |
   | ________##_#___##_________##_##____##___##_______#_#________ |
   | ________###____##_________#####____##___##________#_________ |
   | ________#_#____##_________#___#____##___##__________________ |
   | _________#_____##__________________##___##__________________ |
   | _______________##__________________##___##__________________ |

         ...The sample is now stable.
```



## BBC BASIC


```bbcbasic
      DIM rule$(7)
      rule$() = "0", "0", "0", "1", "0", "1", "1", "0"

      now$ = "01110110101010100100"

      FOR generation% = 0 TO 9
        PRINT "Generation " ; generation% ":", now$
        next$ = ""
        FOR cell% = 1 TO LEN(now$)
          next$ += rule$(EVAL("%"+MID$("0"+now$+"0", cell%, 3)))
        NEXT cell%
        SWAP now$, next$
      NEXT generation%
```

```txt
Generation 0:       01110110101010100100
Generation 1:       01011111010101000000
Generation 2:       00110001101010000000
Generation 3:       00110001110100000000
Generation 4:       00110001011000000000
Generation 5:       00110000111000000000
Generation 6:       00110000101000000000
Generation 7:       00110000010000000000
Generation 8:       00110000000000000000
Generation 9:       00110000000000000000
```



## Befunge


```befunge
v
 " !!! !! ! ! ! !  !  "                                                          ,*25                    <v
 "                    "                                                           ,*25,,,,,,,,,,,,,,,,,,,,<v
 "                    "                                                            ,*25,,,,,,,,,,,,,,,,,,,,<v
 "                    "                                                             ,*25,,,,,,,,,,,,,,,,,,,,<v
 "                    "                                                              ,*25,,,,,,,,,,,,,,,,,,,,<v
 "                    "                                                               ,*25,,,,,,,,,,,,,,,,,,,,<v
 "                    "                                                                ,*25,,,,,,,,,,,,,,,,,,,,<v
 "                    "                                                                 ,*25,,,,,,,,,,,,,,,,,,,,<v
 "                    "                                                                  ,*25,,,,,,,,,,,,,,,,,,,,<v
                                                                      v$<                @,*25,,,,,,,,,,,,,,,,,,,,<
>110p3>:1-10gg" "-4* \:10gg" "-2* \:1+10gg" "-\:54*1+`#v_20p++ :2`#v_ >:4`#v_> >$" "v
                                                                   >:3`#^_v>:6`|
      ^                                                >$$$$320p10g1+:9`v >    >$"!"> 20g10g1+p 20g1+:20p
      ^                                                                v_10p10g
                                                                       >                                 ^
```



## Bracmat


```bracmat
  ( ( evolve
    =   n z
      .   @( !arg
           : %?n ? @?z
           :   ?
               ( (   ( 000
                     | 001
                     | 010
                     | 100
                     | 111
                     )
                   & 0 !n:?n
                 |   (011|101|110)
                   & 1 !n:?n
                 )
               & ~`
               )
               ?
           )
        | rev$(str$(!z !n))
    )
  & 11101101010101001001:?S
  & :?seen
  &   whl
    ' ( ~(!seen:? !S ?)
      & out$!S
      & !S !seen:?seen
      & evolve$!S:?S
      )
  );
```

```txt
11101101010101001001
10111110101010000001
11100011010100000001
10100011101000000001
11000010110000000001
11000001110000000001
11000001010000000001
11000000100000000001
11000000000000000001
```


## C


```c
#include <stdio.h>
#include <string.h>

char trans[] = "___#_##_";

#define v(i) (cell[i] != '_')
int evolve(char cell[], char backup[], int len)
{
	int i, diff = 0;

	for (i = 0; i < len; i++) {
		/* use left, self, right as binary number bits for table index */
		backup[i] = trans[ v(i-1) * 4 + v(i) * 2 + v(i + 1) ];
		diff += (backup[i] != cell[i]);
	}

	strcpy(cell, backup);
	return diff;
}

int main()
{
	char	c[] = "_###_##_#_#_#_#__#__\n",
		b[] = "____________________\n";

	do { printf(c + 1); } while (evolve(c + 1, b + 1, sizeof(c) - 3));
	return 0;
}
```

```txt
###_##_#_#_#_#__#__
#_#####_#_#_#______
_##___##_#_#_______
_##___###_#________
_##___#_##_________
_##____###_________
_##____#_#_________
_##_____#__________
_##________________
```


Similar to above, but without a backup string:

```c
#include <stdio.h>

char trans[] = "___#_##_";

int evolve(char c[], int len)
{
	int i, diff = 0;
#	define v(i) ((c[i] & 15) == 1)
#	define each for (i = 0; i < len; i++)

	each c[i]  = (c[i] == '#');
	each c[i] |= (trans[(v(i-1)*4 + v(i)*2 + v(i+1))] == '#') << 4;
	each diff += (c[i] & 0xf) ^ (c[i] >> 4);
	each c[i]  = (c[i] >> 4) ? '#' : '_';

#	undef each
#	undef v
	return diff;
}

int main()
{
	char c[] = "_###_##_#_#_#_#__#__\n";

	do { printf(c + 1); } while (evolve(c + 1, sizeof(c) - 3));
	return 0;
}
```



## C++

Uses std::bitset for efficient packing of bit values.

```cpp
#include <iostream>
#include <bitset>
#include <string>

const int ArraySize = 20;
const int NumGenerations = 10;
const std::string Initial = "0011101101010101001000";

int main()
{
    // + 2 for the fixed ends of the array
    std::bitset<ArraySize + 2> array(Initial);

    for(int j = 0; j < NumGenerations; ++j)
    {
        std::bitset<ArraySize + 2> tmpArray(array);
        for(int i = ArraySize; i >= 1 ; --i)
        {
            if(array[i])
                std::cout << "#";
            else
                std::cout << "_";
            int val = (int)array[i-1] << 2 | (int)array[i] << 1 | (int)array[i+1];
            tmpArray[i] = (val == 3 || val == 5 || val == 6);
        }
        array = tmpArray;
        std::cout << std::endl;
    }
}
```


```txt
_###_##_#_#_#_#__#__
_#_#####_#_#_#______
__##___##_#_#_______
__##___###_#________
__##___#_##_________
__##____###_________
__##____#_#_________
__##_____#__________
__##________________
__##________________
```



## C#


```c#
using System;
using System.Collections.Generic;

namespace prog
{
	class MainClass
	{
		const int n_iter = 10;
		static int[] f = { 0, 1, 1, 1, 0, 1, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 0, 1, 0, 0 };

		public static void Main (string[] args)
		{
			for( int i=0; i<f.Length; i++ )
				Console.Write( f[i]==0 ? "-" : "#" );
			Console.WriteLine("");

			int[] g = new int[f.Length];
			for( int n=n_iter; n!=0; n-- )
			{
				for( int i=1; i<f.Length-1; i++ )
				{
					if ( (f[i-1] ^ f[i+1]) == 1 ) g[i] = f[i];
					else if ( f[i] == 0 && (f[i-1] & f[i+1]) == 1 ) g[i] = 1;
					else g[i] = 0;
				}
				g[0] = ( (f[0] & f[1]) == 1 ) ? 1 : 0;
				g[g.Length-1] = ( (f[f.Length-1] & f[f.Length-2]) == 1 ) ? 1 : 0;

				int[] tmp = f;
				f = g;
				g = tmp;

				for( int i=0; i<f.Length; i++ )
					Console.Write( f[i]==0 ? "-" : "#" );
				Console.WriteLine("");
			}
		}
	}
}
```



## Ceylon


```ceylon
shared abstract class Cell(character) of alive | dead {
	shared Character character;
	string => character.string;
	shared formal Cell opposite;
}

shared object alive extends Cell('#') {
	opposite => dead;
}
shared object dead extends Cell('_') {
	opposite => alive;
}

shared Map<Character, Cell> cellsByCharacter = map { for (cell in `Cell`.caseValues) cell.character->cell };

shared class Automata1D({Cell*} initialCells) {


	value permanentFirstCell = initialCells.first else dead;
	value permanentLastCell = initialCells.last else dead;

	value cells = Array { *initialCells.rest.exceptLast };

	shared Boolean evolve() {

		value newCells = Array {
			for (index->cell in cells.indexed)
			let (left = cells[index - 1] else permanentFirstCell,
				right = cells[index + 1] else permanentLastCell,
				neighbours = [left, right],
				bothAlive = neighbours.every(alive.equals),
				bothDead = neighbours.every(dead.equals))
			if (bothAlive)
			then cell.opposite
			else if (cell == alive && bothDead)
			then dead
			else cell
		};

		if (newCells == cells) {
			return false;
		}

		newCells.copyTo(cells);
		return true;
	}

	string => permanentFirstCell.string + "".join(cells) + permanentLastCell.string;
}

shared Automata1D? automata1d(String string) =>
		let (cells = string.map((Character element) => cellsByCharacter[element]))
		if (cells.every((Cell? element) => element exists))
		then Automata1D(cells.coalesced)
		else null;

shared void run() {

	assert (exists automata = automata1d("__###__##_#_##_###__######_###_#####_#__##_____#_#_#######__"));

	variable value generation = 0;
	print("generation ``generation`` ``automata``");
	while (automata.evolve() && generation<10) {
		print("generation `` ++generation `` ``automata``");
	}
}
```



## Clojure


```clojure
(ns one-dimensional-cellular-automata
  (:require (clojure.contrib (string :as s))))

(defn next-gen [cells]
  (loop [cs cells ncs (s/take 1 cells)]
    (let [f3 (s/take 3 cs)]
      (if (= 3 (count f3))
        (recur (s/drop 1 cs)
               (str ncs (if (= 2 (count (filter #(= \# %) f3))) "#" "_")))
        (str ncs (s/drop 1 cs))))))

(defn generate [n cells]
  (if (= n 0)
    '()
    (cons cells (generate (dec n) (next-gen cells)))))

```


```clojure
one-dimensional-cellular-automata> (doseq [cells (generate 9 "_###_##_#_#_#_#__#__")]
  (println cells))
_###_##_#_#_#_#__#__
_#_#####_#_#_#______
__##___##_#_#_______
__##___###_#________
__##___#_##_________
__##____###_________
__##____#_#_________
__##_____#__________
__##________________
nil

```


Another way:


```clojure
#!/usr/bin/env lein-exec

(require '[clojure.string :as str])

(def first-genr "_###_##_#_#_#_#__#__")

(def hospitable #{"_##"
                  "##_"
                  "#_#"})

(defn compute-next-genr
  [genr]
  (let [genr      (str "_" genr "_")
        groups    (map str/join (partition 3 1 genr))
        next-genr (for [g groups]
                    (if (hospitable g) \# \_))]
    (str/join next-genr)))

;; ---------------- main -----------------
(loop [g  first-genr
       i  0]
  (if (not= i 10)
    (do (println g)
        (recur (compute-next-genr g)
               (inc i)))))
```


Yet another way, easier to understand


```clojure

(def rules
 {
    [0 0 0] 0
    [0 0 1] 0
    [0 1 0] 0
    [0 1 1] 1
    [1 0 0] 0
    [1 0 1] 1
    [1 1 0] 1
    [1 1 1] 0
  })

(defn nextgen [gen]
  (concat [0]
          (->> gen
               (partition 3 1)
               (map vec)
               (map rules))
          [0]))

; Output time!
(doseq [g (take 10 (iterate nextgen [0 1 1 1 0 1 1 0 1 0 1 0 1 0 1 0 0 1 0 0]))]
  (println g))

```



## COBOL



```cobol

 Identification division.
 Program-id. rc-1d-cell.

 Data division.
 Working-storage section.

*> "Constants."
 01 max-gens            pic  999  value   9.
 01 state-width         pic   99  value  20.
 01 state-table-init    pic x(20) value ".@@@.@@.@.@.@.@..@..".
 01 alive               pic    x  value "@".
 01 dead                pic    x  value ".".

*> The current state.
 01 state-gen           pic  999  value   0.
 01 state-row.
    05 state-row-gen   pic zz9.
    05 filler          pic  xx   value ": ".
    05 state-table.
        10 state-cells pic   x   occurs 20 times.

*> The new state.
 01 new-state-table.
    05 new-state-cells pic   x   occurs 20 times.

*> Pointer into cell table during generational production.
 01 cell-index          pic   99.
    88 at-beginning    value  1.
    88 is-inside       values 2 thru 19.
    88 at-end          value 20.

*> The cell's neighborhood.
 01 neighbor-count-def.
   03 neighbor-count      pic   9.
     88 is-comfy        value 1.
     88 is-ripe         value 2.

 Procedure division.
     Perform Init-state-table.
     Perform max-gens times
         perform Display-row
         perform Next-state
     end-perform.
     Perform Display-row.
     Stop run.

 Display-row.
     Move state-gen to state-row-gen.
     Display state-row.

*> Determine who lives and who dies.
 Next-state.
     Add 1 to state-gen.
     Move state-table to new-state-table.

     Perform with test after
         varying cell-index from 1 by 1
         until at-end
         perform Count-neighbors
         perform Die-off
         perform New-births
     end-perform

     move new-state-table to state-table.

*> Living cell with wrong number of neighbors...
 Die-off.
     if state-cells(cell-index) =
     alive and not is-comfy
         then move dead to new-state-cells(cell-index)
     end-if
     .

*> Empty cell with exactly two neighbors are...
 New-births.
     if state-cells(cell-index) = dead and is-ripe
         then move alive to new-state-cells(cell-index)
     end-if
    .
*> How many living neighbors does a cell have?
 Count-neighbors.
     Move 0 to neighbor-count
     if at-beginning or at-end then
         add 1 to neighbor-count
     else
       if is-inside and state-cells(cell-index - 1) = alive
       then
           add 1 to neighbor-count
       end-if
       if is-inside and state-cells(cell-index + 1) = alive
       then
           add 1 to neighbor-count
       end-if
     end-if
     .

*> String is easier to enter, but table is easier to work with,
*> so move each character of the initialization string to the
*> state table.

 Init-state-table.
     Perform with test after
         varying cell-index from 1 by 1
         until at-end
         move state-table-init(cell-index:1)
           to state-cells(cell-index)
      end-perform
      .

```


```txt

  0: .@@@.@@.@.@.@.@..@..
  1: .@.@@@@@.@.@.@......
  2: ..@@...@@.@.@.......
  3: ..@@...@@@.@........
  4: ..@@...@.@@.........
  5: ..@@....@@@.........
  6: ..@@....@.@.........
  7: ..@@.....@..........
  8: ..@@................
  9: ..@@................
```


=pre>###_##_#_#_#_#__#__
#_#####_#_#_#______
_##___##_#_#_______
_##___###_#________
_##___#_##_________
_##____###_________
_##____#_#_________
_##_____#__________
_##________________={{header|CoffeeScript}}==

```coffeescript

# We could cheat and count the bits, but let's keep this general.
# . = dead, # = alive, middle cells survives iff one of the configurations
# below is satisified.
survival_scenarios = [
  '.##' # happy neighbors
  '#.#' # birth
  '##.' # happy neighbors
]

b2c = (b) -> if b then '#' else '.'

cell_next_gen = (left_alive, me_alive, right_alive) ->
  fingerprint = b2c(left_alive) + b2c(me_alive) + b2c(right_alive)
  fingerprint in survival_scenarios

cells_for_next_gen = (cells) ->
  # This function assumes a finite array, i.e. cells can't be born outside
  # the original array.
  (cell_next_gen(cells[i-1], cells[i], cells[i+1]) for i in [0...cells.length])

display = (cells) ->
  (b2c(is_alive) for is_alive in cells).join ''

simulate = (cells) ->
  while true
    console.log display cells
    new_cells = cells_for_next_gen cells
    break if display(cells) == display(new_cells)
    cells = new_cells
  console.log "equilibrium achieved"

simulate (c == '#' for c in ".###.##.#.#.#.#..#..")

```

```txt

> coffee cellular_automata.coffee
.###.##.#.#.#.#..#..
.#.#####.#.#.#......
..##...##.#.#.......
..##...###.#........
..##...#.##.........
..##....###.........
..##....#.#.........
..##.....#..........
..##................
equilibrium achieved

```



## Common Lisp

Based upon the Ruby version.

```lisp
(defun value (x)
  (assert (> (length x) 1))
  (coerce x 'simple-bit-vector))

(defun count-neighbors-and-self (value i)
  (flet ((ref (i)
           (if (array-in-bounds-p value i)
               (bit value i)
               0)))
    (declare (inline ref))
    (+ (ref (1- i))
       (ref i)
       (ref (1+ i)))))

(defun next-cycle (value)
  (let ((new-value (make-array (length value) :element-type 'bit)))
    (loop for i below (length value)
          do (setf (bit new-value i)
                   (if (= 2 (count-neighbors-and-self value i))
                       1
                       0)))
    new-value))

(defun print-world (value &optional (stream *standard-output*))
  (loop for i below (length value)
        do (princ (if (zerop (bit value i)) #\. #\#)
                  stream))
  (terpri stream))
```



```lisp
CL-USER> (loop for previous-value = nil then value
               for value = #*01110110101010100100 then (next-cycle value)
               until (equalp value previous-value)
               do (print-world value))
.###.##.#.#.#.#..#..
.#.#####.#.#.#......
..##...##.#.#.......
..##...###.#........
..##...#.##.........
..##....###.........
..##....#.#.........
..##.....#..........
..##................
```



## D


```d
void main() {
   import std.stdio, std.algorithm;

   enum nGenerations = 10;
   enum initial = "0011101101010101001000";
   enum table = "00010110";

   char[initial.length + 2] A = '0', B = '0';
   A[1 .. $-1] = initial;
   foreach (immutable _; 0 .. nGenerations) {
      foreach (immutable i; 1 .. A.length - 1) {
         write(A[i] == '0' ? '_' : '#');
         const val = (A[i-1]-'0' << 2) | (A[i]-'0' << 1) | (A[i+1]-'0');
         B[i] = table[val];
      }
      A.swap(B);
      writeln;
   }
}
```

```txt
__###_##_#_#_#_#__#___
__#_#####_#_#_#_______
___##___##_#_#________
___##___###_#_________
___##___#_##__________
___##____###__________
___##____#_#__________
___##_____#___________
___##_________________
___##_________________
```



### Alternative Version

```d
void main() {
    import std.stdio, std.algorithm, std.range;

    auto A = "_###_##_#_#_#_#__#__".map!q{a == '#'}.array;
    auto B = A.dup;

    do {
        A.map!q{ "_#"[a] }.writeln;
        A.zip(A.cycle.drop(1), A.cycle.drop(A.length - 1))
        .map!(t => [t[]].sum == 2).copy(B);
        A.swap(B);
    } while (A != B);
}
```

```txt
_###_##_#_#_#_#__#__
_#_#####_#_#_#______
__##___##_#_#_______
__##___###_#________
__##___#_##_________
__##____###_________
__##____#_#_________
__##_____#__________
__##________________
```



### Alternative Version II

This version saves memory representing the state in an array of bits. For a higher performance a SWAR approach should be tried.
```d
void main() {
    import std.stdio, std.algorithm, std.range, std.bitmanip;

    immutable initial = "__###_##_#_#_#_#__#___";
    enum nGenerations = 10;
    BitArray A, B;
    A.init(initial.map!(c => c == '#').array);
    B.length = initial.length;

    foreach (immutable _; 0 .. nGenerations) {
        //A.map!(b => b ? '#' : '_').writeln;
        //foreach (immutable i, immutable b; A) {
        foreach (immutable i; 1 .. A.length - 1) {
            "_#"[A[i]].write;
            immutable val = (uint(A[i - 1]) << 2) |
                            (uint(A[i])     << 1) |
                             uint(A[i + 1]);
            B[i] = val == 3 || val == 5 || val == 6;
        }

        writeln;
        A.swap(B);
    }
}
```

The output is the same as the second version.

=={{header|Déjà Vu}}==


```dejavu
new-state size:
	0 ]
	repeat size:
		random-range 0 2
	[ 0

update s1 s2:
	for i range 1 - len s1 2:
		s1! -- i
		s1!    i
		s1! ++ i
		+ +
		set-to s2 i = 2
	s2 s1

print-state s:
	for i range 1 - len s 2:
		!print\ s! i
	!print ""

same-state s1 s2:
	for i range 1 - len s1 2:
		if /= s1! i s2! i:
			return false
	true

run size:
	new-state size
	new-state size
	while true:
		update
		print-state over
		if same-state over over:
			return print-state drop

run 60
```

```txt
001110011010110111001111110111011111010011000001010111111100
001010011101111101001000011101110001100011000000101100000100
000100010111000110000000010111010001100011000000011100000000
000000001101000110000000001101100001100011000000010100000000
000000001110000110000000001111100001100011000000001000000000
000000001010000110000000001000100001100011000000000000000000
000000000100000110000000000000000001100011000000000000000000
000000000000000110000000000000000001100011000000000000000000
000000000000000110000000000000000001100011000000000000000000
```



## DWScript


```delphi
const ngenerations = 10;
const table = [0, 0, 0, 1, 0, 1, 1, 0];

var a := [0, 0, 1, 1, 1, 0, 1, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 0, 1, 0, 0, 0];
var b := a;

var i, j : Integer;
for i := 1 to ngenerations do begin
   for j := a.low+1 to a.high-1 do begin
      if a[j] = 0 then
         Print('_')
      else Print('#');
      var val := (a[j-1] shl 2) or (a[j] shl 1) or a[j+1];
      b[j] := table[val];
   end;
   var tmp := a;
   a := b;
   b := tmp;
   PrintLn('');
end;

```

```txt
_###_##_#_#_#_#__#__
_#_#####_#_#_#______
__##___##_#_#_______
__##___###_#________
__##___#_##_________
__##____###_________
__##____#_#_________
__##_____#__________
__##________________
__##________________
```




## E



```e
def step(state, rule) {
    var result := state(0, 1) # fixed left cell
    for i in 1..(state.size() - 2) {
        # Rule function receives the substring which is the neighborhood
        result += E.toString(rule(state(i-1, i+2)))
    }
    result += state(state.size() - 1) # fixed right cell
    return result
}

def play(var state, rule, count, out) {
    out.print(`0 | $state$\n`)
    for i in 1..count {
        state := step(state, rosettaRule)
        out.print(`$i | $state$\n`)
    }
    return state
}
```



```e
def rosettaRule := [
    "   " => " ",
    "  #" => " ",
    " # " => " ",
    " ##" => "#",
    "#  " => " ",
    "# #" => "#",
    "## " => "#",
    "###" => " ",
].get

? play("  ### ## # # # #  #   ", rosettaRule, 9, stdout)
0 |   ### ## # # # #  #
1 |   # ##### # # #
2 |    ##   ## # #
3 |    ##   ### #
4 |    ##   # ##
5 |    ##    ###
6 |    ##    # #
7 |    ##     #
8 |    ##
9 |    ##
# value: "   ##                 "
```



## Eiffel


```Eiffel

class
	APPLICATION

create
	make

feature

	make
			-- First 10 states of the cellular automata.
		local
			r: RANDOM
			automata: STRING
		do
			create r.make
			create automata.make_empty
			across
				1 |..| 10 as c
			loop
				if r.double_item < 0.5 then
					automata.append ("0")
				else
					automata.append ("1")
				end
				r.forth
			end
			across
				1 |..| 10 as c
			loop
				io.put_string (automata + "%N")
				automata := update (automata)
			end
		end

	update (s: STRING): STRING
			-- Next state of the cellular automata 's'.
		require
			enough_states: s.count > 1
		local
			i: INTEGER
		do
			create Result.make_empty
				-- Dealing with the left border.
			if s [1] = '1' and s [2] = '1' then
				Result.append ("1")
			else
				Result.append ("0")
			end
				-- Dealing with the middle cells.
			from
				i := 2
			until
				i = s.count
			loop
				if (s [i] = '0' and (s [i - 1] = '0' or (s [i - 1] = '1' and s [i + 1] = '0'))) or ((s [i] = '1') and ((s [i - 1] = '1' and s [i + 1] = '1') or (s [i - 1] = '0' and s [i + 1] = '0'))) then
					Result.append ("0")
				else
					Result.append ("1")
				end
				i := i + 1
			end
				-- Dealing with the right border.
			if s [s.count] = '1' and s [s.count - 1] = '1' then
				Result.append ("1")
			else
				Result.append ("0")
			end
		ensure
			has_same_length: s.count = Result.count
		end

end

```

```txt

1011101110
0110111010
0111101100
0100111100
0000100100
0000000000
0000000000
0000000000
0000000000
0000000000

```



## Elixir

```elixir
defmodule RC do
  def run(list, gen \\ 0) do
    print(list, gen)
    next = evolve(list)
    if next == list, do: print(next, gen+1), else: run(next, gen+1)
  end

  defp evolve(list), do: evolve(Enum.concat([[0], list, [0]]), [])

  defp evolve([a,b,c],      next), do: Enum.reverse([life(a,b,c) | next])
  defp evolve([a,b,c|rest], next), do: evolve([b,c|rest], [life(a,b,c) | next])

  defp life(a,b,c), do: (if a+b+c == 2, do: 1, else: 0)

  defp print(list, gen) do
    str = "Generation #{gen}: "
    IO.puts Enum.reduce(list, str, fn x,s -> s <> if x==0, do: ".", else: "#" end)
  end
end

RC.run([0,1,1,1,0,1,1,0,1,0,1,0,1,0,1,0,0,1,0,0])
```


```txt

Generation 0: .###.##.#.#.#.#..#..
Generation 1: .#.#####.#.#.#......
Generation 2: ..##...##.#.#.......
Generation 3: ..##...###.#........
Generation 4: ..##...#.##.........
Generation 5: ..##....###.........
Generation 6: ..##....#.#.........
Generation 7: ..##.....#..........
Generation 8: ..##................
Generation 9: ..##................

```


## Elm


```elm
import Maybe exposing (withDefault)
import List exposing (length, tail, reverse, concat, head, append, map3)
import Html exposing (Html, div, h1, text)
import String exposing (join)
import Svg exposing (svg)
import Svg.Attributes exposing (version, width, height, viewBox,cx,cy, fill, r)
import Html.App exposing (program)
import Random exposing (step, initialSeed, bool, list)
import Matrix exposing (fromList, mapWithLocation, flatten)  -- chendrix/elm-matrix
import Time exposing (Time, second, every)

type alias Model = { history : List (List Bool)
                   , cols : Int
                   , rows : Int
                   }

view : Model -> Html Msg
view model =
  let
    circleInBox (row,col) value =
      if value
      then [ Svg.circle [ r "0.3"
                        , fill ("purple")
                        , cx (toString (toFloat col + 0.5))
                        , cy (toString (toFloat row + 0.5))
                        ]
                        []
           ]
      else []

    showHistory model =
      model.history
        |> reverse
        |> fromList
        |> mapWithLocation circleInBox
        |> flatten
        |> concat
  in
    div []
        [ h1 [] [text "One Dimensional Cellular Automata"]
        , svg [ version "1.1"
              , width "700"
              , height "700"
              , viewBox (join " "
                           [ 0 |> toString
                           , 0 |> toString
                           , model.cols |> toString
                           , model.rows |> toString
                           ]
                        )
              ]
              (showHistory model)
        ]

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  if length model.history == model.rows
  then (model, Cmd.none)
  else
    let s1 = model.history |> head |> withDefault []
        s0 = False :: s1
        s2 = append (tail s1 |> withDefault []) [False]

        gen d0 d1 d2 =
          case (d0,d1,d2) of
            (False,  True,  True) -> True
            ( True, False,  True) -> True
            ( True,  True, False) -> True
            _                     -> False

        updatedHistory = map3 gen s0 s1 s2 :: model.history
        updatedModel = {model | history = updatedHistory}
    in (updatedModel, Cmd.none)


init : Int -> (Model, Cmd Msg)
init n =
  let gen1 = fst (step (list n bool) (initialSeed 34))
  in ({ history = [gen1], rows = n, cols= n }, Cmd.none)

type Msg = Tick Time

subscriptions model = every (0.2 * second) Tick

main = program
         {  init = init 40
         ,  view = view
         ,  update = update
         ,  subscriptions = subscriptions
         }
```


Link to live demo: https://dc25.github.io/oneDimensionalCellularAutomataElm/

## Erlang


```erlang

-module(ca).
-compile(export_all).

run(N,G) ->
    run(N,G,0).

run(GN,G,GN) ->
    io:fwrite("~B: ",[GN]),
    print(G);
run(N,G,GN) ->
    io:fwrite("~B: ",[GN]),
    print(G),
    run(N,next(G),GN+1).

print([]) ->
    io:fwrite("~n");
print([0|T]) ->
    io:fwrite("_"),
    print(T);
print([1|T]) ->
    io:fwrite("#"),
    print(T).

next([]) ->
    [];
next([_]) ->
    [0];
next([H,1|_]=G) ->
    next(G,[H]);
next([_|_]=G) ->
    next(G,[0]).

next([],Acc) ->
    lists:reverse(Acc);
next([0,_],Acc) ->
    next([],[0|Acc]);
next([1,X],Acc) ->
    next([],[X|Acc]);
next([0,X,0|T],Acc) ->
    next([X,0|T],[0|Acc]);
next([1,X,0|T],Acc) ->
    next([X,0|T],[X|Acc]);
next([0,X,1|T],Acc) ->
    next([X,1|T],[X|Acc]);
next([1,0,1|T],Acc) ->
    next([0,1|T],[1|Acc]);
next([1,1,1|T],Acc) ->
    next([1,1|T],[0|Acc]).

```

Example execution:

```erlang

44> ca:run(9,[0, 0, 1, 1, 1, 0, 1, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 0, 1, 0, 0, 0]).
0: __###_##_#_#_#_#__#___
1: __#_#####_#_#_#_______
2: ___##___##_#_#________
3: ___##___###_#_________
4: ___##___#_##__________
5: ___##____###__________
6: ___##____#_#__________
7: ___##_____#___________
8: ___##_________________
9: ___##_________________

```



## ERRE


```ERRE

PROGRAM ONEDIM_AUTOMATA

! for rosettacode.org
!

!VAR I,J,N,W,K

!$DYNAMIC
DIM X[0],X2[0]

BEGIN

   DATA(20,0,1,1,1,0,1,1,0,1,0,1,0,1,0,1,0,0,1,0,0)

   PRINT(CHR$(12);)
   N=20      ! number of generation required
   READ(W)
   !$DIM X[W+1],X2[W+1]
   FOR I=1 TO W DO
      READ(X[I])
   END FOR
   FOR K=1 TO N DO
      PRINT("Generation";K;TAB(16);)
      FOR J=1 TO W DO
         IF X[J]=1 THEN PRINT("#";)  ELSE PRINT("_";) END IF
         IF X[J-1]+X[J]+X[J+1]=2 THEN X2[J]=1 ELSE X2[J]=0 END IF
      END FOR
      PRINT
      FOR J=1 TO W DO
         X[J]=X2[J]
      END FOR
   END FOR
END PROGRAM

```

```txt

Generation 1   _###_##_#_#_#_#__#__
Generation 2   _#_#####_#_#_#______
Generation 3   __##___##_#_#_______
Generation 4   __##___###_#________
Generation 5   __##___#_##_________
Generation 6   __##____###_________
Generation 7   __##____#_#_________
Generation 8   __##_____#__________
Generation 9   __##________________
Generation 10  __##________________
Generation 11  __##________________
Generation 12  __##________________
Generation 13  __##________________
Generation 14  __##________________
Generation 15  __##________________
Generation 16  __##________________
Generation 17  __##________________
Generation 18  __##________________
Generation 19  __##________________
Generation 20  __##________________

```



## Euphoria


```euphoria
include machine.e

function rules(integer tri)
    return tri = 3 or tri = 5 or tri = 6
end function

function next_gen(atom gen)
    atom new, bit
    new = rules(and_bits(gen,3)*2) -- work with the first bit separately
    bit = 2
    while gen > 0 do
        new += bit*rules(and_bits(gen,7))
        gen = floor(gen/2) -- shift right
        bit *= 2 -- shift left
    end while
    return new
end function

constant char_clear = '_', char_filled = '#'

procedure print_gen(atom gen)
    puts(1, int_to_bits(gen,32) * (char_filled - char_clear) + char_clear)
    puts(1,'\n')
end procedure

function s_to_gen(sequence s)
    s -= char_clear
    return bits_to_int(s)
end function

atom gen, prev
integer n

n = 0
prev = 0
gen = bits_to_int(rand(repeat(2,32))-1)
while gen != prev do
    printf(1,"Generation %d: ",n)
    print_gen(gen)
    prev = gen
    gen = next_gen(gen)
    n += 1
end while

printf(1,"Generation %d: ",n)
print_gen(gen)
```


 Generation 0: ####__#_###_#_#_#_#_##___##_##__
 Generation 1: ___#___##_##_#_#_#_###___#####__
 Generation 2: _______######_#_#_##_#___#___#__
 Generation 3: _______#____##_#_####___________
 Generation 4: ____________###_##__#___________
 Generation 5: ____________#_####______________
 Generation 6: _____________##__#______________
 Generation 7: _____________##_________________
 Generation 8: _____________##_________________


## Factor


```factor
USING: bit-arrays io kernel locals math sequences ;
IN: cellular

: bool-sum ( bool1 bool2 -- sum )
    [ [ 2 ] [ 1 ] if ]
    [ [ 1 ] [ 0 ] if ] if ;
:: neighbours ( index world -- # )
    index [ 1 - ] [ 1 + ] bi [ world ?nth ] bi@ bool-sum ;
: count-neighbours ( world -- neighbours )
    [ length iota ] keep [ neighbours ] curry map ;

: life-law ( alive? neighbours -- alive? )
    swap [ 1 = ] [ 2 = ] if ;
: step ( world -- world' )
    dup count-neighbours [ life-law ] ?{ } 2map-as ;
: print-cellular ( world -- )
    [ CHAR: # CHAR: _ ? ] "" map-as print ;
: main-cellular ( -- )
    ?{ f t t t f t t f t f t f t f t f f t f f }
    10 [ dup print-cellular step ] times print-cellular ;
MAIN: main-cellular

```


```txt
( scratchpad ) "cellular" run
_###_##_#_#_#_#__#__
_#_#####_#_#_#______
__##___##_#_#_______
__##___###_#________
__##___#_##_________
__##____###_________
__##____#_#_________
__##_____#__________
__##________________
__##________________
__##________________
```



## Fantom


```fantom

class Automaton
{
  static Int[] evolve (Int[] array)
  {
    return array.map |Int x, Int i -> Int|
    {
      if (i == 0)
        return ( (x + array[1] == 2) ? 1 : 0)
      else if (i == array.size-1)
        return ( (x + array[-2] == 2) ? 1 : 0)
      else if (x + array[i-1] + array[i+1] == 2)
        return 1
      else
        return 0
    }
  }

  public static Void main ()
  {
    Int[] array := [0,1,1,1,0,1,1,0,1,0,1,0,1,0,1,0,0,1,0,0]
    echo (array.join(""))
    Int[] newArray := evolve(array)
    while (newArray != array)
    {
      echo (newArray.join(""))
      array = newArray
      newArray = evolve(array)
    }
  }
}

```



## Forth


```forth
: init ( bits count -- )
  0 do dup 1 and c, 2/ loop drop ;

20 constant size
create state $2556e size init 0 c,

: .state
  cr size 0 do
    state i + c@ if ." #" else space then
  loop ;

: ctable create does> + c@ ;
ctable rules $68 8 init

: gen
  state c@ ( window )
  size 0 do
    2*  state i + 1+ c@ or  7 and
    dup rules state i + c!
  loop drop ;

: life1d ( n -- )
  .state 1 do gen .state loop ;

10 life1d
```


ouput
<lang>
 ### ## # # # #  #
 # ##### # # #
  ##   ## # #
  ##   ### #
  ##   # ##
  ##    ###
  ##    # #
  ##     #
  ##
  ##                 ok

```



## Fortran

```fortran
PROGRAM LIFE_1D

  IMPLICIT NONE

  LOGICAL :: cells(20) = (/ .FALSE., .TRUE., .TRUE., .TRUE., .FALSE., .TRUE., .TRUE., .FALSE., .TRUE., .FALSE., &
                            .TRUE., .FALSE., .TRUE., .FALSE., .TRUE., .FALSE., .FALSE., .TRUE., .FALSE., .FALSE. /)
  INTEGER :: i

  DO i = 0, 9
     WRITE(*, "(A,I0,A)", ADVANCE = "NO") "Generation ", i, ": "
     CALL Drawgen(cells)
     CALL Nextgen(cells)
  END DO

CONTAINS

  SUBROUTINE Nextgen(cells)
    LOGICAL, INTENT (IN OUT) :: cells(:)
    LOGICAL :: left, centre, right
    INTEGER :: i

    left = .FALSE.
    DO i = 1, SIZE(cells)-1
       centre = cells(i)
       right = cells(i+1)
       IF (left .AND. right) THEN
          cells(i) = .NOT. cells(i)
       ELSE IF (.NOT. left .AND. .NOT. right) THEN
          cells(i) = .FALSE.
       END IF
       left = centre
    END DO
    cells(SIZE(cells)) = left .AND. right
  END SUBROUTINE Nextgen

  SUBROUTINE Drawgen(cells)
    LOGICAL, INTENT (IN OUT) :: cells(:)
    INTEGER :: i

    DO i = 1, SIZE(cells)
       IF (cells(i)) THEN
          WRITE(*, "(A)", ADVANCE = "NO") "#"
       ELSE
          WRITE(*, "(A)", ADVANCE = "NO") "_"
       END IF
    END DO
    WRITE(*,*)
  END SUBROUTINE Drawgen

END PROGRAM LIFE_1D
```

```txt

 Generation 0: _###_##_#_#_#_#__#__
 Generation 1: _#_#####_#_#_#______
 Generation 2: __##___##_#_#_______
 Generation 3: __##___###_#________
 Generation 4: __##___#_##_________
 Generation 5: __##____###_________
 Generation 6: __##____#_#_________
 Generation 7: __##_____#__________
 Generation 8: __##________________
 Generation 9: __##________________
```



## GFA Basic


<lang>
'
' One Dimensional Cellular Automaton
'
start$="01110110101010100100"
max_cycles%=20 ! give a maximum depth
'
' Global variables hold the world, with two rows
' world! is set up with 2 extra cells width, so there is a FALSE on either side
' cur% gives the row for current world,
' new% gives the row for the next world.
'
size%=LEN(start$)
DIM world!(size%+2,2)
cur%=0
new%=1
clock%=0
'
@setup_world(start$)
OPENW 1
CLEARW 1
DO
  @display_world
  @update_world
  EXIT IF @same_state
  clock%=clock%+1
  EXIT IF clock%>max_cycles% ! safety net
LOOP
~INP(2)
CLOSEW 1
'
' parse given string to set up initial states in world
' -- assumes world! is of correct size
'
PROCEDURE setup_world(defn$)
  LOCAL i%
  ' clear out the array
  ARRAYFILL world!(),FALSE
  ' for each 1 in string, set cell to true
  FOR i%=1 TO LEN(defn$)
    IF MID$(defn$,i%,1)="1"
      world!(i%,0)=TRUE
    ENDIF
  NEXT i%
  ' set references to cur and new
  cur%=0
  new%=1
RETURN
'
' Display the world
'
PROCEDURE display_world
  LOCAL i%
  FOR i%=1 TO size%
    IF world!(i%,cur%)
      PRINT "#";
    ELSE
      PRINT ".";
    ENDIF
  NEXT i%
  PRINT ""
RETURN
'
' Create new version of world
'
PROCEDURE update_world
  LOCAL i%
  FOR i%=1 TO size%
    world!(i%,new%)=@new_state(@get_value(i%))
  NEXT i%
  ' reverse cur/new
  cur%=1-cur%
  new%=1-new%
RETURN
'
' Test if cur/new states are the same
'
FUNCTION same_state
  LOCAL i%
  FOR i%=1 TO size%
    IF world!(i%,cur%)<>world!(i%,new%)
      RETURN FALSE
    ENDIF
  NEXT i%
  RETURN TRUE
ENDFUNC
'
' Return new state of cell given value
'
FUNCTION new_state(value%)
  SELECT value%
  CASE 0,1,2,4,7
    RETURN FALSE
  CASE 3,5,6
    RETURN TRUE
  ENDSELECT
ENDFUNC
'
' Compute value for cell + neighbours
'
FUNCTION get_value(cell%)
  LOCAL result%
  result%=0
  IF world!(cell%-1,cur%)
    result%=result%+4
  ENDIF
  IF world!(cell%,cur%)
    result%=result%+2
  ENDIF
  IF world!(cell%+1,cur%)
    result%=result%+1
  ENDIF
  RETURN result%
ENDFUNC

```



## Go


### Sequential


```go
package main

import "fmt"

const (
    start    = "_###_##_#_#_#_#__#__"
    offLeft  = '_'
    offRight = '_'
    dead     = '_'
)

func main() {
    fmt.Println(start)
    g := newGenerator(start, offLeft, offRight, dead)
    for i := 0; i < 10; i++ {
        fmt.Println(g())
    }
}

func newGenerator(start string, offLeft, offRight, dead byte) func() string {
    g0 := string(offLeft) + start + string(offRight)
    g1 := []byte(g0)
    last := len(g0) - 1
    return func() string {
        for i := 1; i < last; i++ {
            switch l := g0[i-1]; {
            case l != g0[i+1]:
                g1[i] = g0[i]
            case g0[i] == dead:
                g1[i] = l
            default:
                g1[i] = dead
            }
        }
        g0 = string(g1)
        return g0[1:last]
    }
}
```

```txt

_###_##_#_#_#_#__#__
_#_#####_#_#_#______
__##___##_#_#_______
__##___###_#________
__##___#_##_________
__##____###_________
__##____#_#_________
__##_____#__________
__##________________
__##________________
__##________________

```



### Concurrent

Computations run on each cell concurrently.
Separate read and write phases.
Single array of cells.

```go
package main

import (
    "fmt"
    "sync"
)

const (
    start    = "_###_##_#_#_#_#__#__"
    offLeft  = '_'
    offRight = '_'
    dead     = '_'
)

func main() {
    fmt.Println(start)
    a := make([]byte, len(start)+2)
    a[0] = offLeft
    copy(a[1:], start)
    a[len(a)-1] = offRight
    var read, write sync.WaitGroup
    read.Add(len(start) + 1)
    for i := 1; i <= len(start); i++ {
        go cell(a[i-1:i+2], &read, &write)
    }
    for i := 0; i < 10; i++ {
        write.Add(len(start) + 1)
        read.Done()
        read.Wait()
        read.Add(len(start) + 1)
        write.Done()
        write.Wait()
        fmt.Println(string(a[1 : len(a)-1]))
    }
}

func cell(kernel []byte, read, write *sync.WaitGroup) {
    var next byte
    for {
        l, v, r := kernel[0], kernel[1], kernel[2]
        read.Done()
        switch {
        case l != r:
            next = v
        case v == dead:
            next = l
        default:
            next = dead
        }
        read.Wait()
        kernel[1] = next
        write.Done()
        write.Wait()
    }
}
```

Output is same as sequential version.


## Groovy

Solution:

```groovy
def life1D = { self ->
    def right = self[1..-1] + [false]
    def left = [false] + self[0..-2]
    [left, self, right].transpose().collect { hood -> hood.count { it } == 2 }
}
```


Test:

```groovy
def cells = ('_###_##_#_#_#_#__#__' as List).collect { it == '#' }
println "Generation 0: ${cells.collect { g -> g ? '#' : '_' }.join()}"
(1..9).each {
    cells = life1D(cells)
    println "Generation ${it}: ${cells.collect { g -> g ? '#' : '_' }.join()}"
}
```


```txt
Generation 0: _###_##_#_#_#_#__#__
Generation 1: _#_#####_#_#_#______
Generation 2: __##___##_#_#_______
Generation 3: __##___###_#________
Generation 4: __##___#_##_________
Generation 5: __##____###_________
Generation 6: __##____#_#_________
Generation 7: __##_____#__________
Generation 8: __##________________
Generation 9: __##________________
```



## Haskell


```haskell
import System.Random (newStdGen, randomRs)
import Data.List (unfoldr)

bnd :: String -> Char
bnd bs =
  case bs of
    "_##" -> '#'
    "#_#" -> '#'
    "##_" -> '#'
    _ -> '_'

donxt :: String -> String
donxt xs =
  unfoldr
    (\xs ->
        case xs of
          [_, _] -> Nothing
          _ -> Just (bnd $ take 3 xs, drop 1 xs)) $
  '_' : xs ++ "_"

lahmahgaan :: String -> [String]
lahmahgaan xs =
  init .
  until ((==) . last <*> (last . init)) ((++) <*> (return . donxt . last)) $
  [xs, donxt xs]

main :: IO ()
main = do
  g <- newStdGen
  let oersoep = map ("_#" !!) . take 36 $ randomRs (0, 1) g
  mapM_ print . lahmahgaan $ oersoep
```

```haskell
*Life1D> mapM_ print . lahmahgaan $ "_###_##_#_#_#_#__#__"
"_###_##_#_#_#_#__#__"
"_#_#####_#_#_#______"
"__##___##_#_#_______"
"__##___###_#________"
"__##___#_##_________"
"__##____###_________"
"__##____#_#_________"
"__##_____#__________"
"__##________________"

*Life1D> main
"__##_##__#____###__#__#_______#_#_##"
"__#####_______#_#______________#_###"
"__#___#________#________________##_#"
"________________________________###_"
"________________________________#_#_"
"_________________________________#__"
"____________________________________"
```


=={{header|Icon}} and {{header|Unicon}}==


```icon

# One dimensional Cellular automaton
record Automaton(size, cells)

procedure make_automaton (size, items)
  automaton := Automaton (size, items)
  while (*items < size) do push (automaton.cells, 0)
  return automaton
end

procedure automaton_display (automaton)
  every (write ! automaton.cells)
end

procedure automaton_evolve (automaton)
  revised := make_automaton (automaton.size, [])
  # do the left-most cell
  if ((automaton.cells[1] + automaton.cells[2]) = 2) then
    revised.cells[1] := 1
  # do the right-most cell
  if ((automaton.cells[automaton.size] + automaton.cells[automaton.size-1]) = 2) then
    revised.cells[revised.size] := 1
  # do the intermediate cells
  every (i := 2 to (automaton.size-1)) do {
    if ((automaton.cells[i-1] + automaton.cells[i] + automaton.cells[i+1]) = 2) then
      revised.cells[i] := 1
  }
  return revised
end

procedure main ()
  automaton := make_automaton (20, [0,1,1,1,0,1,1,0,1,0,1,0,1,0,1,0,0,1,0,0])
  every (1 to 10) do { # generations
    automaton_display (automaton)
    automaton := automaton_evolve (automaton)
  }
end

```


An alternative approach is to represent the automaton as a string.
The following solution takes advantage of the implicit type coercions
between string and numeric values in Icon and Unicon.
It also surrounds the automaton with a border of 'dead' (always 0) cells
to eliminate the need to special case the first and last cells
in the automaton.
Although the main procedure displays up to the first 10 generations,
the evolve procedure fails if a new generation is unchanged from the previous, stopping the generation cycle early.


```unicon
procedure main(A)
  A := if *A = 0 then ["01110110101010100100"]
  CA := show("0"||A[1]||"0")        # add always dead border cells
  every CA := show(|evolve(CA)\10)  # limit to max of 10 generations
end

procedure show(ca)
  write(ca[2:-1])                   # omit border cells
  return ca
end

procedure evolve(CA)
  newCA := repl("0",*CA)
  every newCA[i := 2 to (*CA-1)] := (CA[i-1]+CA[i]+CA[i+1] = 2, "1")
  return CA ~== newCA               # fail if no change
end
```


```txt
->odca
01110110101010100100
01011111010101000000
00110001101010000000
00110001110100000000
00110001011000000000
00110000111000000000
00110000101000000000
00110000010000000000
00110000000000000000
->odca 01110110
01110110
01011110
00110010
00110000
->
```



## J


```j
life1d=: '_#'{~ (2 = 3+/\ 0,],0:)^:a:
```


```j
   life1d ? 20 # 2
_###_##_#_#_#_#__#__
_#_#####_#_#_#______
__##___##_#_#_______
__##___###_#________
__##___#_##_________
__##____###_________
__##____#_#_________
__##_____#__________
__##________________
```


Alternative implementation:


```j
Rule=:2 :0 NB. , m: number of generations, n: rule number
  '_#'{~ (3 ((|.n#:~8#2) {~ #.)\ 0,],0:)^:(i.m)
)
```


```j
   9 Rule 104 '#'='_###_##_#_#_#_#__#__'
_###_##_#_#_#_#__#__
_#_#####_#_#_#______
__##___##_#_#_______
__##___###_#________
__##___#_##_________
__##____###_________
__##____#_#_________
__##_____#__________
__##________________
```




## Java

This example requires a starting generation of at least length two
(which is what you need for anything interesting anyway).

```java
public class Life{
	public static void main(String[] args) throws Exception{
		String start= "_###_##_#_#_#_#__#__";
		int numGens = 10;
		for(int i= 0; i < numGens; i++){
			System.out.println("Generation " + i + ": " + start);
			start= life(start);
		}
	}

	public static String life(String lastGen){
		String newGen= "";
		for(int i= 0; i < lastGen.length(); i++){
			int neighbors= 0;
			if (i == 0){//left edge
				neighbors= lastGen.charAt(1) == '#' ? 1 : 0;
			} else if (i == lastGen.length() - 1){//right edge
				neighbors= lastGen.charAt(i - 1) == '#' ? 1 : 0;
			} else{//middle
				neighbors= getNeighbors(lastGen.substring(i - 1, i + 2));
			}

			if (neighbors == 0){//dies or stays dead with no neighbors
				newGen+= "_";
			}
			if (neighbors == 1){//stays with one neighbor
				newGen+= lastGen.charAt(i);
			}
			if (neighbors == 2){//flips with two neighbors
				newGen+= lastGen.charAt(i) == '#' ? "_" : "#";
			}
		}
		return newGen;
	}

	public static int getNeighbors(String group){
		int ans= 0;
		if (group.charAt(0) == '#') ans++;
		if (group.charAt(2) == '#') ans++;
		return ans;
	}
}
```

```txt
Generation 0: _###_##_#_#_#_#__#__
Generation 1: _#_#####_#_#_#______
Generation 2: __##___##_#_#_______
Generation 3: __##___###_#________
Generation 4: __##___#_##_________
Generation 5: __##____###_________
Generation 6: __##____#_#_________
Generation 7: __##_____#__________
Generation 8: __##________________
Generation 9: __##________________
```

In this version, <code>b</code> is replaced by a <code>backup</code>
which is local to the <code>evolve</code> method,
and the <code>evolve</code> method returns a boolean.

```java
public class Life{
	private static char[] trans = "___#_##_".toCharArray();

	private static int v(StringBuilder cell, int i){
		return (cell.charAt(i) != '_') ? 1 : 0;
	}

	public static boolean evolve(StringBuilder cell){
		boolean diff = false;
		StringBuilder backup = new StringBuilder(cell.toString());

		for(int i = 1; i < cell.length() - 3; i++){
			/* use left, self, right as binary number bits for table index */
			backup.setCharAt(i, trans[v(cell, i - 1) * 4 + v(cell, i) * 2
			      					+ v(cell, i + 1)]);
			diff = diff || (backup.charAt(i) != cell.charAt(i));
		}

		cell.delete(0, cell.length());//clear the buffer
		cell.append(backup);//replace it with the new generation
		return diff;
	}

	public static void main(String[] args){
		StringBuilder  c = new StringBuilder("_###_##_#_#_#_#__#__\n");

		do{
			System.out.printf(c.substring(1));
		}while(evolve(c));
	}
}
```

```txt
###_##_#_#_#_#__#__
#_#####_#_#_#______
_##___##_#_#_______
_##___###_#________
_##___#_##_________
_##____###_________
_##____#_#_________
_##_____#__________
_##________________
```



## JavaScript

The example below expects an array of 1s or 0s, as in the example.
It also adds dead cells to both ends,
which aren't included in the returned next generation.

state[i-1] refers to the new cell in question,
(old[i] == 1) checks if the old cell was alive.

```javascript
function caStep(old) {
  var old = [0].concat(old, [0]); // Surround with dead cells.
  var state = []; // The new state.

  for (var i=1; i<old.length-1; i++) {
    switch (old[i-1] + old[i+1]) {
      case 0: state[i-1] = 0; break;
      case 1: state[i-1] = (old[i] == 1) ? 1 : 0; break;
      case 2: state[i-1] = (old[i] == 1) ? 0 : 1; break;
    }
  }
  return state;
}
```


```javascript
alert(caStep([0,1,1,1,0,1,1,0,1,0,1,0,1,0,1,0,0,1,0,0]));
```

shows an alert with "0,1,0,1,1,1,1,1,0,1,0,1,0,1,0,0,0,0,0,0".



## jq

The main point of interest in the following is perhaps the way the built-in function "recurse" is used to continue the simulation until quiescence.

```jq
# The 1-d cellular automaton:
def next:
   # Conveniently, jq treats null as 0 when it comes to addition
   # so there is no need to fiddle with the boundaries
  . as $old
  | reduce range(0; length) as $i
    ([];
     ($old[$i-1] + $old[$i+1]) as $s
     | if   $s == 0 then .[$i] = 0
       elif $s == 1 then .[$i] = (if $old[$i] == 1 then 1 else 0 end)
       else              .[$i] = (if $old[$i] == 1 then 0 else 1 end)
       end);


# pretty-print an array:
def pp: reduce .[] as $i (""; . + (if $i == 0 then " " else "*" end));

# continue until quiescence:
def go: recurse(. as $prev | next | if . == $prev then empty else . end) | pp;

# Example:
[0,1,1,1,0,1,1,0,1,0,1,0,1,0,1,0,0,1,0,0] | go
```

```sh
$ jq -c -r -n -f One-dimensional_cellular_automata.jq
 *** ** * * * *  *
 * ***** * * *
  **   ** * *
  **   *** *
  **   * **
  **    ***
  **    * *
  **     *
  **
```



## Julia

This solution creates an automaton with with either empty or periodic bounds.  The empty bounds case, is typical of many of the solutions here.  The periodic bounds case is a typical physics approach where, in effect, the beginning and end of the list touch each other to form a circular rather than linear array.  In practice, the effects of boundary conditions are subtle for long arrays.

```Julia

function next_gen(a::BitArray{1}, isperiodic=false)
    b = copy(a)
    if isperiodic
        ncnt = prepend!(a[1:end-1], [a[end]]) + append!(a[2:end], [a[1]])
    else
        ncnt = prepend!(a[1:end-1], [false]) + append!(a[2:end], [false])
    end
    b[ncnt .== 0] = false
    b[ncnt .== 2] = ~b[ncnt .== 2]
    return b
end

function show_gen(a::BitArray{1})
    s = join([i ? "\u2588" : " " for i in a], "")
    s = "\u25ba"*s*"\u25c4"
end

hi = 70
a = bitrand(hi)
b = falses(hi)
println("A 1D Cellular Atomaton with ", hi, " cells and empty bounds.")
while any(a) && any(a .!= b)
    println("    ", show_gen(a))
    b = copy(a)
    a = next_gen(a)
end
a = bitrand(hi)
b = falses(hi)
println()
println("A 1D Cellular Atomaton with ", hi, " cells and periodic bounds.")
while any(a) && any(a .!= b)
    println("    ", show_gen(a))
    b = copy(a)
    a = next_gen(a, true)
end

```


```txt

A 1D Cellular Atomaton with 70 cells and empty bounds.
    ► ███  ██  █ ██   ███ █  ███  ██  █ █   ██████ █   ██ █ █ █  █ ██  ███ ◄
    ► █ █  ██   ███   █ ██   █ █  ██   █    █    ██    ███ █ █    ███  █ █ ◄
    ►  █   ██   █ █    ███    █   ██             ██    █ ██ █     █ █   █  ◄
    ►      ██    █     █ █        ██             ██     ████       █       ◄
    ►      ██           █         ██             ██     █  █               ◄
    ►      ██                     ██             ██                        ◄

A 1D Cellular Atomaton with 70 cells and periodic bounds.
    ►████   ██ █    █ █  ██  ██ █ █      ████   █    ███  ███ ██     ██ ██ ◄
    ►█  █   ███      █   ██  ███ █       █  █        █ █  █ ████     ██████◄
    ►█      █ █          ██  █ ██                     █    ██  █     █     ◄
    ►        █           ██   ███                          ██              ◄
    ►                    ██   █ █                          ██              ◄
    ►                    ██    █                           ██              ◄
    ►                    ██                                ██              ◄

```



## K


```K
f:{2=+/(0,x,0)@(!#x)+/:!3}
```


```K
   `0:"_X"@f\0 1 1 1 0 1 1 0 1 0 1 0 1 0 1 0 0 1 0 0
_XXX_XX_X_X_X_X__X__
_X_XXXXX_X_X_X______
__XX___XX_X_X_______
__XX___XXX_X________
__XX___X_XX_________
__XX____XXX_________
__XX____X_X_________
__XX_____X__________
__XX________________

```



## Kotlin

```scala
// version 1.1.4-3

val trans = "___#_##_"

fun v(cell: StringBuilder, i: Int) = if (cell[i] != '_') 1 else 0

fun evolve(cell: StringBuilder, backup: StringBuilder): Boolean {
    val len = cell.length - 2
    var diff = 0
    for (i in 1 until len) {
        /* use left, self, right as binary number bits for table index */
        backup[i] = trans[v(cell, i - 1) * 4 + v(cell, i) * 2 + v(cell, i + 1)]
        diff += if (backup[i] != cell[i]) 1 else 0
    }
    cell.setLength(0)
    cell.append(backup)
    return diff != 0
}

fun main(args: Array<String>) {
    val c = StringBuilder("_###_##_#_#_#_#__#__")
    val b = StringBuilder("____________________")
    do {
       println(c.substring(1))
    }
    while (evolve(c,b))
}
```


```txt

###_##_#_#_#_#__#__
#_#####_#_#_#______
_##___##_#_#_______
_##___###_#________
_##___#_##_________
_##____###_________
_##____#_#_________
_##_____#__________
_##________________

```



## Liberty BASIC


```lb
'   [RC] 'One-dimensional cellular automata'

'    does not wrap so fails for some rules
rule$ ="00010110"   '   Rule 22 decimal

state$ ="0011101101010101001000"

for j =1 to 20
    print state$
    oldState$ =state$
    state$ ="0"
    for k =2 to len( oldState$) -1
        NHood$ =mid$( oldState$, k -1, 3)  '   pick 3 char neighbourhood and turn binary string to decimal
        vNHood =0
        for kk =3 to 1 step -1
            vNHood =vNHood +val( mid$( NHood$, kk, 1)) *2^( 3 -kk)
        next kk
                                        '  .... & use it to index into rule$ to find appropriate new value
        state$ =state$ +mid$( rule$, vNHood +1, 1)
    next k
    state$ =state$ +"0"

next j

end
```



## Locomotive Basic



```locobasic
10 MODE 1:n=10:READ w:DIM x(w+1),x2(w+1):FOR i=1 to w:READ x(i):NEXT
20 FOR k=1 TO n
30 FOR j=1 TO w
40 IF x(j) THEN PRINT "#"; ELSE PRINT "_";
50 IF x(j-1)+x(j)+x(j+1)=2 THEN x2(j)=1 ELSE x2(j)=0
60 NEXT:PRINT
70 FOR j=1 TO w:x(j)=x2(j):NEXT
80 NEXT
90 DATA 20,0,1,1,1,0,1,1,0,1,0,1,0,1,0,1,0,0,1,0,0
```


[[File:Cellular automaton locomotive basic.png]]


## Logo

```logo
make "cell_list [0 1 1 1 0 1 1 0 1 0 1 0 1 0 1 0 0 1 0 0]
make "generations 9

to evolve :n
ifelse :n=1 [make "nminus1 item :cell_count :cell_list][make "nminus1 item :n-1 :cell_list]
ifelse :n=:cell_count[make "nplus1 item 1 :cell_list][make "nplus1 item :n+1 :cell_list]
ifelse ((item :n :cell_list)=0) [
	ifelse (and (:nminus1=1) (:nplus1=1)) [output 1][output (item :n :cell_list)]
][
	ifelse (and (:nminus1=1) (:nplus1=1)) [output 0][
	   ifelse and (:nminus1=0) (:nplus1=0) [output 0][output (item :n :cell_list)]]
]
end

to CA_1D :cell_list :generations
make "cell_count count :cell_list
(print ")
make "printout "
repeat :cell_count [
make "printout word :printout ifelse (item repcount :cell_list)=1 ["#]["_]
]
(print "Generation "0: :printout)

repeat :generations [
       (make "cell_list_temp [])
       repeat :cell_count[
             (make "cell_list_temp (lput (evolve repcount) :cell_list_temp))
       ]
       make "cell_list :cell_list_temp
       make "printout "
       repeat :cell_count [
       	      make "printout word :printout ifelse (item repcount :cell_list)=1 ["#]["_]
       ]
       (print "Generation  word repcount ": :printout)
]
end

CA_1D :cell_list :generations
```

```txt

Generation 0: _###_##_#_#_#_#__#__
Generation 1: _#_#####_#_#_#______
Generation 2: __##___##_#_#_______
Generation 3: __##___###_#________
Generation 4: __##___#_##_________
Generation 5: __##____###_________
Generation 6: __##____#_#_________
Generation 7: __##_____#__________
Generation 8: __##________________
Generation 9: __##________________

```



## Lua


```lua
num_iterations = 9
f = { 0, 1, 1, 1, 0, 1, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 0, 1, 0, 0 }

function Output( f, l )
    io.write( l, ":  " )
    for i = 1, #f do
        local c
        if f[i] == 1 then c = '#' else c = '_' end
        io.write( c )
    end
    print ""
end

Output( f, 0 )

for l = 1, num_iterations do
    local g = {}
    for i = 2, #f-1 do
        if f[i-1] + f[i+1] == 1 then
            g[i] = f[i]
        elseif f[i] == 0 and f[i-1] + f[i+1] == 2 then
            g[i] = 1
        else
            g[i] = 0
        end
    end
    if f[1]  == 1 and f[2]    == 1 then g[1]  = 1 else g[1]  = 0 end
    if f[#f] == 1 and f[#f-1] == 1 then g[#f] = 1 else g[#f] = 0 end
    f, g = g, f

    Output( f, l )
end
```

```txt
0:  _###_##_#_#_#_#__#__
1:  _#_#####_#_#_#______
2:  __##___##_#_#_______
3:  __##___###_#________
4:  __##___#_##_________
5:  __##____###_________
6:  __##____#_#_________
7:  __##_____#__________
8:  __##________________
9:  __##________________
```



## M4


```M4
divert(-1)
define(`set',`define(`$1[$2]',`$3')')
define(`get',`defn(`$1[$2]')')
define(`setrange',`ifelse(`$3',`',$2,`define($1[$2],$3)`'setrange($1,
   incr($2),shift(shift(shift($@))))')')

dnl  throw in sentinels at each end (0 and size+1) to make counting easy
define(`new',`set($1,size,eval($#-1))`'setrange($1,1,
   shift($@))`'set($1,0,0)`'set($1,$#,0)')

define(`for',
   `ifelse($#,0,``$0'',
   `ifelse(eval($2<=$3),1,
   `pushdef(`$1',$2)$4`'popdef(`$1')$0(`$1',incr($2),$3,`$4')')')')
define(`show',
   `for(`k',1,get($1,size),`get($1,k) ')')

dnl  swap(`a',a,`b')  using arg stack for temp
define(`swap',`define(`$1',$3)`'define(`$3',$2)')
define(`nalive',
   `eval(get($1,decr($2))+get($1,incr($2)))')
setrange(`live',0,0,1,0)
setrange(`dead',0,0,0,1)
define(`nv',
   `ifelse(get($1,z),0,`get(dead,$3)',`get(live,$3)')')
define(`evolve',
   `for(`z',1,get($1,size),
      `set($2,z,nv($1,z,nalive($1,z)))')')
new(`a',0,1,1,1,0,1,1,0,1,0,1,0,1,0,1,0,0,1,0,0)
set(`b',size,get(`a',size))`'set(`b',0,0)`'set(`b',incr(get(`a',size)),0)
define(`x',`a')
define(`y',`b')
divert
for(`j',1,10,
   `show(x)`'evolve(`x',`y')`'swap(`x',x,`y')
')`'show(x)
```


```txt

0 1 1 1 0 1 1 0 1 0 1 0 1 0 1 0 0 1 0 0
0 1 0 1 1 1 1 1 0 1 0 1 0 1 0 0 0 0 0 0
0 0 1 1 0 0 0 1 1 0 1 0 1 0 0 0 0 0 0 0
0 0 1 1 0 0 0 1 1 1 0 1 0 0 0 0 0 0 0 0
0 0 1 1 0 0 0 1 0 1 1 0 0 0 0 0 0 0 0 0
0 0 1 1 0 0 0 0 1 1 1 0 0 0 0 0 0 0 0 0
0 0 1 1 0 0 0 0 1 0 1 0 0 0 0 0 0 0 0 0
0 0 1 1 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0
0 0 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0

```


=={{header|Mathematica}} / {{header|Wolfram Language}}==
Built-in function:

```Mathematica
CellularAutomaton[{{0,0,_}->0,{0,1,0}->0,{0,1,1}->1,{1,0,0}->0,{1,0,1}->1,{1,1,0}->1,{1,1,1}->0},{{1,1,1,0,1,1,0,1,0,1,0,1,0,1,0,0,1},0},12]
Print @@@ (% /. {1 -> "#", 0 -> "."});
```

For succinctness, an integral rule can be used:

```Mathematica
CellularAutomaton[2^^01101000 (* == 104 *), {{1,1,1,0,1,1,0,1,0,1,0,1,0,1,0,0,1}, 0}, 12];
```

```Mathematica
###.##.#.#.#.#..#
#.#####.#.#.#....
.##...##.#.#.....
.##...###.#......
.##...#.##.......
.##....###.......
.##....#.#.......
.##.....#........
.##..............
.##..............
.##..............
.##..............
.##..............
```


=={{header|MATLAB}} / {{header|Octave}}==

```MATLAB
function one_dim_cell_automata(v,n)
   V='_#';
   while n>=0;
	disp(V(v+1));
	n = n-1;
	v = filter([1,1,1],1,[0,v,0]);
	v = v(3:end)==2;
   end;
end
```

```txt
octave:27> one_dim_cell_automata('01110110101010100100'=='1',20);
_###_##_#_#_#_#__#__
_#_#####_#_#_#______
__##___##_#_#_______
__##___###_#________
__##___#_##_________
__##____###_________
__##____#_#_________
__##_____#__________
__##________________
__##________________
__##________________
...
```



=={{header|Modula-3}}==
Modula-3 provides a module <code>Word</code> for doing bitwise operations,
but it segfaults when trying to use <code>BOOLEAN</code> types,
so we use <code>INTEGER</code> instead.

```modula3
MODULE Cell EXPORTS Main;

IMPORT IO, Fmt, Word;

VAR culture := ARRAY [0..19] OF INTEGER {0, 1, 1, 1,
                                         0, 1, 1, 0,
                                         1, 0, 1, 0,
                                         1, 0, 1, 0,
                                         0, 1, 0, 0};

PROCEDURE Step(VAR culture: ARRAY OF INTEGER) =
  VAR left: INTEGER := 0;
      this, right: INTEGER;
  BEGIN
    FOR i := FIRST(culture) TO LAST(culture) - 1 DO
      right := culture[i + 1];
      this := culture[i];
      culture[i] :=
          Word.Or(Word.And(this, Word.Xor(left, right)), Word.And(Word.Not(this), Word.And(left, right)));
      left := this;
    END;
    culture[LAST(culture)] := Word.And(culture[LAST(culture)], Word.Not(left));
  END Step;

PROCEDURE Put(VAR culture: ARRAY OF INTEGER) =
  BEGIN
    FOR i := FIRST(culture) TO LAST(culture) DO
      IF culture[i] = 1 THEN
        IO.PutChar('#');
      ELSE
        IO.PutChar('_');
      END;
    END;
  END Put;

BEGIN
  FOR i := 0 TO 9 DO
    IO.Put("Generation " & Fmt.Int(i) & " ");
    Put(culture);
    IO.Put("\n");
    Step(culture);
  END;
END Cell.
```

```txt

Generation 0 _###_##_#_#_#_#__#__
Generation 1 _#_#####_#_#_#______
Generation 2 __##___##_#_#_______
Generation 3 __##___###_#________
Generation 4 __##___#_##_________
Generation 5 __##____###_________
Generation 6 __##____#_#_________
Generation 7 __##_____#__________
Generation 8 __##________________
Generation 9 __##________________

```



## MontiLang


```MontiLang
30 VAR length .
35 VAR height .
FOR length 0 ENDFOR 1 0 ARR VAR list .
length 1 - VAR topLen .
FOR topLen 0 ENDFOR 1 ARR VAR topLst .

DEF getNeighbors
    1 - VAR tempIndex .
    GET tempIndex SWAP
    tempIndex 1 + VAR tempIndex .
    GET tempIndex SWAP
    tempIndex 1 + VAR tempIndex .
    GET tempIndex SWAP .
    FOR 3 TOSTR ROT ENDFOR
    FOR 2 SWAP + ENDFOR
ENDDEF

DEF printArr
    LEN 1 - VAR stLen .
    0 VAR j .
    FOR stLen
        GET j
        TOSTR OUT .
        j 1 + VAR j .
    ENDFOR
    || PRINT .
ENDDEF

FOR height
    FOR length 0 ENDFOR ARR VAR next .
    1 VAR i .
    FOR length
        list i getNeighbors VAR last .
        i 1 - VAR ind .
        last |111| ==
        IF : .
            next 0 INSERT ind
        ENDIF

        last |110| ==
        IF : .
            next 1 INSERT ind
        ENDIF

        last |101| ==
        IF : .
            next 1 INSERT ind
        ENDIF

        last |100| ==
        IF : .
            next 0 INSERT ind
        ENDIF

        last |011| ==
        IF : .
            next 1 INSERT ind
        ENDIF

        last |010| ==
        IF : .
            next 1 INSERT ind
        ENDIF

        last |001| ==
        IF : .
            next 1 INSERT ind
        ENDIF

        last |000| ==
        IF : .
            next 0 INSERT ind
        ENDIF
        clear
        i 1 + VAR i .
    ENDFOR
    next printArr .
    next 0 ADD APPEND . VAR list .
ENDFOR
```



## Nial

(life.nial)

```nial
% we need a way to write a values and pass the same back
wi is rest link [write, pass]
% calculate the neighbors by rotating the array left and right and joining them
neighbors is pack [pass, sum [-1 rotate,  1 rotate]]
% calculate the individual birth and death of a single array element
igen is fork [ = [ + [first, second], 3 first], 0 first, = [ + [first, second], 2 first], 1 first, 0 first ]
% apply that to the array
nextgen is each igen neighbors
% 42
life is fork [ > [sum pass, 0 first], life nextgen wi, pass ]
```


```nial
|loaddefs 'life.nial'
|I := [0,1,1,1,0,1,1,0,1,0,1,0,1,0,1,0,0,1,0,0]
|life I
```



## Nim


```Nim
import random


type
  BoolArray  = array[30, bool]
  Symbols    = array[bool, char]


proc neighbours(map: BoolArray, i: int): int =
  if i > 0:             inc(result, int(map[i - 1]))
  if i + 1 < len(map):  inc(result, int(map[i + 1]))

proc print(map: BoolArray, symbols: Symbols) =
  for i in map: write(stdout, symbols[i])
  write(stdout, "\l")

proc randomMap: BoolArray =
  randomize()
  for i in mitems(result): i = rand([true, false])


const
  num_turns = 20
  symbols   = ['_', '#']

  T = true
  F = false

var map =
  [F, T, T, T, F, T, T, F, T, F, T, F, T, F, T,
    F, F, T, F, F, F, F, F, F, F, F, F, F, F, F]

# map = randomMap()  # uncomment for random start

print(map, symbols)

for _ in 0 ..< num_turns:
  var map2 = map

  for i, v in pairs(map):
    map2[i] =
      if v: neighbours(map, i) == 1
      else: neighbours(map, i) == 2

  print(map2, symbols)

  if map2 == map: break
  map = map2
```

```txt
_###_##_#_#_#_#__#____________
_#_#####_#_#_#________________
__##___##_#_#_________________
__##___###_#__________________
__##___#_##___________________
__##____###___________________
__##____#_#___________________
__##_____#____________________
__##__________________________
__##__________________________
```


'''Using a string character counting method''':

```nim
const
  s_init: string = "_###_##_#_#_#_#__#__"
  arrLen: int = 20

var q0: string = s_init & repeatChar(arrLen-20,'_')
var q1: string = q0

proc life(s:string): char =
   var str: string = s
   if len(normalize(str)) == 2:      # normalize eliminates underscores
      return '#'
   return '_'

proc evolve(q: string): string =
   result = repeatChar(arrLen,'_')
   #result[0] = '_'
   for i in 1 .. q.len-1:
      result[i] = life(substr(q & '_',i-1,i+1))

echo(q1)
q1 = evolve(q0)
echo(q1)
while q1 != q0:
   q0 = q1
   q1 = evolve(q0)
   echo(q1)
```

```txt
_###_##_#_#_#_#__#__
_#_#####_#_#_#______
__##___##_#_#_______
__##___###_#________
__##___#_##_________
__##____###_________
__##____#_#_________
__##_____#__________
__##________________
__##________________
```


'''Using nested functions and method calling style:'''

```txt

proc cell_automata =
    proc evolve_into(X, T : var string) =
        for i in X.low..X.high:
            let
                alive = X[i] == 'o'
                left  = if i == X.low:  false else: X[i - 1] == 'o'
                right = if i == X.high: false else: X[i + 1] == 'o'
            T[i] =
                if alive: if left xor right: 'o' else: '.'
                else:     if left and right: 'o' else: '.'

    var
        X = ".ooo.oo.o.o.o.o..o.."
        T = X

    for i in 1..10:
        X.echo
        X.evolve_into T
        T.swap X

```



## OCaml



```ocaml
let get g i =
  try g.(i)
  with _ -> 0

let next_cell g i =
  match get g (i-1), get g (i), get g (i+1) with
  | 0, 0, 0 -> 0
  | 0, 0, 1 -> 0
  | 0, 1, 0 -> 0
  | 0, 1, 1 -> 1
  | 1, 0, 0 -> 0
  | 1, 0, 1 -> 1
  | 1, 1, 0 -> 1
  | 1, 1, 1 -> 0
  | _ -> assert(false)

let next g =
  let old_g = Array.copy g in
  for i = 0 to pred(Array.length g) do
    g.(i) <- (next_cell old_g i)
  done

let print_g g =
  for i = 0 to pred(Array.length g) do
    if g.(i) = 0
    then print_char '_'
    else print_char '#'
  done;
  print_newline()
```


put the code above in a file named "life.ml",
and then use it in the ocaml toplevel like this:

<pre style="height:48ex;overflow:scroll">
#use "life.ml" ;;

let iter n g =
  for i = 0 to n do
    Printf.printf "Generation %d: " i; print_g g;
    next g;
  done
;;

let g_of_string str =
  let f = (function '_' -> 0 | '#' -> 1 | _ -> assert false) in
  Array.init (String.length str) (fun i -> f str.[i])
;;

# iter 9 (g_of_string "_###_##_#_#_#_#__#__") ;;
Generation 0: _###_##_#_#_#_#__#__
Generation 1: _#_#####_#_#_#______
Generation 2: __##___##_#_#_______
Generation 3: __##___###_#________
Generation 4: __##___#_##_________
Generation 5: __##____###_________
Generation 6: __##____#_#_________
Generation 7: __##_____#__________
Generation 8: __##________________
Generation 9: __##________________
- : unit = ()

```



## Oforth



```Oforth
: nextGen( l )
| i s |
   l byteSize dup ->s String newSize
   s loop: i [
      i 1 if=: [ 0 ] else: [ i 1- l byteAt '#' = ]
      i l byteAt '#' = +
      i s if=: [ 0 ] else: [ i 1+ l byteAt '#' = ] +
      2 if=: [ '#' ] else: [ '_' ] over add
      ]
;

: gen( l n -- )
    l dup .cr #[ nextGen dup .cr ] times( n ) drop ;
```


```txt

"_###_##_#_#_#_#__#__" 10 gen
_###_##_#_#_#_#__#__
_#_#####_#_#_#______
__##___##_#_#_______
__##___###_#________
__##___#_##_________
__##____###_________
__##____#_#_________
__##_____#__________
__##________________
__##________________
__##________________
ok

```



## Oz


```oz
declare
  A0 = {List.toTuple unit "_###_##_#_#_#_#__#__"}

  MaxGenerations = 9

  Rules = unit('___':&_
               '__#':&_
               '_#_':&_
               '_##':&#
               '#__':&_
               '#_#':&#
               '##_':&#
               '###':&_)

  fun {Evolve A}
     {Record.mapInd A
      fun {$ I V}
         Left = {CondSelect A I-1 &_}
         Right = {CondSelect A I+1 &_}
         Env = {String.toAtom [Left V Right]}
      in
         Rules.Env
      end
     }
  end

  fun lazy {Iterate X F}
     X|{Iterate {F X} F}
  end
in
  for
     I in 0..MaxGenerations
     A in {Iterate A0 Evolve}
  do
     {System.showInfo "Gen. "#I#": "#{Record.toList A}}
  end
```


```txt
Gen. 0: _###_##_#_#_#_#__#__
Gen. 1: _#_#####_#_#_#______
Gen. 2: __##___##_#_#_______
Gen. 3: __##___###_#________
Gen. 4: __##___#_##_________
Gen. 5: __##____###_________
Gen. 6: __##____#_#_________
Gen. 7: __##_____#__________
Gen. 8: __##________________
Gen. 9: __##________________

```



## PARI/GP

This version defines the fixed cells to the left and right as dead;
of course other versions are possible.
This function generates one generation from a previous one,
passed as a 0-1 vector.

```parigp
step(v)=my(u=vector(#v),k);u[1]=v[1]&v[2];u[#u]=v[#v]&v[#v-1];for(i=2,#v-1,k=v[i-1]+v[i+1];u[i]=if(v[i],k==1,k==2));u;
```


To simulate a run of 10 generations of the automaton, the function above can be put in a loop that spawns a new generation as a function of nth generations passed (n=0 is the initial state):


```parigp
cur = [0,1,1,1,0,1,1,0,1,0,1,0,1,0,1,0,0,1,0,0]; for(n=0, 9, print(cur); cur = step(cur));
```



###  Output

<lang>
[0, 1, 1, 1, 0, 1, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 0, 1, 0, 0]
[0, 1, 0, 1, 1, 1, 1, 1, 0, 1, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0]
[0, 0, 1, 1, 0, 0, 0, 1, 1, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0]
[0, 0, 1, 1, 0, 0, 0, 1, 1, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0]
[0, 0, 1, 1, 0, 0, 0, 1, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0]
[0, 0, 1, 1, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0]
[0, 0, 1, 1, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0]
[0, 0, 1, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
[0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
[0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]

```



## Pascal


```pascal
program Test;
{$IFDEF FPC}{$MODE DELPHI}{$ELSE}{$APPTYPE}{$ENDIF}
uses
  sysutils;
const
  cCHAR: array[0..1] of char = ('_','#');
type
  TRow =  array of byte;

function ConvertToRow(const s:string):tRow;
var
  i : NativeInt;
Begin
  i := length(s);
  setlength(Result,length(s));
  For i := i downto 0 do
    result[i-1]:= ORD(s[i]=cChar[1]);
end;

function OutRow(const row:tRow):string;
//create output string
var
  i: NativeInt;
Begin
  i := length(row);
  setlength(result,i);
  For i := i downto 1 do
    result[i]:= cChar[row[i-1]];
end;

procedure NextRow(row:pByteArray;MaxIdx:NativeInt);
//compute next row in place by the using a small storage for the
//2 values, that would otherwise be overridden
var
  leftValue,Value: NativeInt;
  i,trpCnt: NativeInt;
Begin
  leftValue := 0;
  trpCnt := row[0]+row[1];

  i := 0;
  while i < MaxIdx do
  Begin
    Value := row[i];
    //the rule for survive : PopCnt == 2
    row[i] := ORD(trpCnt= 2);
    //reduce popcnt of element before
    dec(trpCnt,leftValue);
    //goto next element
    inc(i);
    leftValue := Value;
    //increment popcnt by right element
    inc(trpCnt,row[i+1]);
    //move to next position in ring buffer
  end;
  row[MaxIdx] := ORD(trpCnt= 2);
end;

const
  TestString: string='  ### ## # # # #  #  ';
var
  s: string;
  row:tRow;
  i: NativeInt;
begin
  s := Teststring;
  row:= ConvertToRow(s);
  For i := 0 to 9 do
  Begin
    writeln(OutRow(row));
    NextRow(@row[0],High(row));
  end;
end.
```

```txt

__###_##_#_#_#_#__#__
__#_#####_#_#_#______
___##___##_#_#_______
___##___###_#________
___##___#_##_________
___##____###_________
___##____#_#_________
___##_____#__________
___##________________
___##________________
```



## Perl


Use regexp to extract and substitute cells while the string changes

Convert cells to zeros and ones to set complement state

```perl

$_="_###_##_#_#_#_#__#__\n";
do {
  y/01/_#/;
  print;
  y/_#/01/;
  s/(?<=(.))(.)(?=(.))/$1 == $3 ? $1 ? 1-$2 : 0 : $2/eg;
} while ($x ne $_ and $x=$_);

```


Use hash for complement state

```perl

$_="_###_##_#_#_#_#__#__\n";
%h=qw(# _ _ #);
do {
  print;
  s/(?<=(.))(.)(?=(.))/$1 eq $3 ? $1 eq "_" ? "_" : $h{$2} : $2/eg;
} while ($x ne $_ and $x=$_);

```


{{out}} for both versions:

```txt
_###_##_#_#_#_#__#__
_#_#####_#_#_#______
__##___##_#_#_______
__##___###_#________
__##___#_##_________
__##____###_________
__##____#_#_________
__##_____#__________
__##________________
```



## Perl 6

We'll make a general algorithm capable of computing any cellular automata
as defined by [[wp:Stephen Wolfram|Stephen Wolfram]]'s
famous book ''[[wp:A new kind of Science|A new kind of Science]]''.
We will take the liberty of wrapping the array of cells
as it does not affect the result much
and it makes the implementation a lot easier.


```perl6
class Automaton {
    has $.rule;
    has @.cells;
    has @.code = $!rule.fmt('%08b').flip.comb».Int;

    method gist { "|{ @!cells.map({+$_ ?? '#' !! ' '}).join }|" }

    method succ {
        self.new: :$!rule, :@!code, :cells(
            @!code[
                    4 «*« @!cells.rotate(-1)
                »+« 2 «*« @!cells
                »+«       @!cells.rotate(1)
            ]
        )
    }
}

#  The rule proposed for this task is rule 0b01101000 = 104

my @padding = 0 xx 5;
my Automaton $a .= new:
    rule  => 104,
    cells => flat @padding, '111011010101'.comb, @padding
;
say $a++ for ^10;


# Rule 104 is not particularly interesting so here is [[wp:Rule 90|Rule 90]],
# which shows a [[wp:Sierpinski Triangle|Sierpinski Triangle]].

say '';
@padding = 0 xx 25;
$a = Automaton.new: :rule(90), :cells(flat @padding, 1, @padding);

say $a++ for ^20;
```


```txt

|     ### ## # # #     |
|     # ##### # #      |
|      ##   ## #       |
|      ##   ###        |
|      ##   # #        |
|      ##    #         |
|      ##              |
|      ##              |
|      ##              |
|      ##              |

|                         #                         |
|                        # #                        |
|                       #   #                       |
|                      # # # #                      |
|                     #       #                     |
|                    # #     # #                    |
|                   #   #   #   #                   |
|                  # # # # # # # #                  |
|                 #               #                 |
|                # #             # #                |
|               #   #           #   #               |
|              # # # #         # # # #              |
|             #       #       #       #             |
|            # #     # #     # #     # #            |
|           #   #   #   #   #   #   #   #           |
|          # # # # # # # # # # # # # # # #          |
|         #                               #         |
|        # #                             # #        |
|       #   #                           #   #       |
|      # # # #                         # # # #      |

```



## Phix

Ludicrously optimised:

```Phix
string s = "_###_##_#_#_#_#__#__"
integer prev='_', curr, toggled = 1

while 1 do
    ?s
    for i=2 to length(s)-1 do
        curr = s[i]
        if prev=s[i+1]
        and (curr='#' or prev='#') then
            s[i] = 130-curr
            toggled = 1
        end if
        prev = curr
    end for
    if not toggled then ?s exit end if
    toggled = 0
end while
```

<pre style="font-size: 8px">
"_###_##_#_#_#_#__#__"
"_#_#####_#_#_#______"
"__##___##_#_#_______"
"__##___###_#________"
"__##___#_##_________"
"__##____###_________"
"__##____#_#_________"
"__##_____#__________"
"__##________________"
"__##________________"

```

And of course I had to have a crack at that Sierpinski_Triangle:

```Phix
string s = "________________________#________________________"
integer prev='_', curr, toggled = 1

for limit=1 to 24 do
    ?s
    for i=2 to length(s)-1 do
        curr = s[i]
        if (prev=s[i+1]) = (curr='#') then
            s[i] = 130-curr
        end if
        prev = curr
    end for
end for
```

<pre style="font-size: 6px">
"________________________#________________________"
"_______________________#_#_______________________"
"______________________#___#______________________"
"_____________________#_#_#_#_____________________"
"____________________#_______#____________________"
"___________________#_#_____#_#___________________"
"__________________#___#___#___#__________________"
"_________________#_#_#_#_#_#_#_#_________________"
"________________#_______________#________________"
"_______________#_#_____________#_#_______________"
"______________#___#___________#___#______________"
"_____________#_#_#_#_________#_#_#_#_____________"
"____________#_______#_______#_______#____________"
"___________#_#_____#_#_____#_#_____#_#___________"
"__________#___#___#___#___#___#___#___#__________"
"_________#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_________"
"________#_______________________________#________"
"_______#_#_____________________________#_#_______"
"______#___#___________________________#___#______"
"_____#_#_#_#_________________________#_#_#_#_____"
"____#_______#_______________________#_______#____"
"___#_#_____#_#_____________________#_#_____#_#___"
"__#___#___#___#___________________#___#___#___#__"
"_#_#_#_#_#_#_#_#_________________#_#_#_#_#_#_#_#_"

```



## PicoLisp


```PicoLisp
(let Cells (chop "_###_##_#_#_#_#__#__")
   (do 10
      (prinl Cells)
      (setq Cells
         (make
            (link "_")
            (map
               '((L)
                  (case (head 3 L)
                     (`(mapcar chop '("___" "__#" "_#_" "#__" "###"))
                         (link "_") )
                     (`(mapcar chop '("_##" "#_#" "##_"))
                        (link "#") ) ) )
               Cells )
            (link "_") ) ) ) )
```

```txt
_###_##_#_#_#_#__#__
_#_#####_#_#_#______
__##___##_#_#_______
__##___###_#________
__##___#_##_________
__##____###_________
__##____#_#_________
__##_____#__________
__##________________
__##________________
```



## Prolog

Works ith SWI-Prolog.

```Prolog
one_dimensional_cellular_automata(L) :-
	maplist(my_write, L), nl,
	length(L, N),
	length(LN, N),
	% there is a 0 before the beginning
	compute_next([0 |L], LN),
	(   L \= LN -> one_dimensional_cellular_automata(LN); true).

% All the possibilites
compute_next([0, 0, 0 | R], [0 | R1]) :-
	compute_next([0, 0 | R], R1).

compute_next([0, 0, 1 | R], [0 | R1]) :-
	compute_next([0, 1 | R], R1).

compute_next([0, 1, 0 | R], [0 | R1]) :-
	compute_next([1, 0 | R], R1).

compute_next([0, 1, 1 | R], [1 | R1]) :-
	compute_next([1, 1 | R], R1).

compute_next([1, 0, 0 | R], [0 | R1]) :-
	compute_next([0, 0 | R], R1).

compute_next([1, 0, 1 | R], [1 | R1]) :-
	compute_next([0, 1 | R], R1).

compute_next([1, 1, 0 | R], [1 | R1]) :-
	compute_next([1, 0 | R], R1).

compute_next([1, 1, 1 | R], [0 | R1]) :-
	compute_next([1, 1 | R], R1).

% the last four possibilies =>
% we consider that there is à 0  after the end
complang jq># The 1-d cellular automaton:
def next:
   # Conveniently, jq treats null as 0 when it comes to addition
   # so there is no need to fiddle with the boundaries
  . as $old
  | reduce range(0; length) as $i
    ([];
     ($old[$i-1] + $old[$i+1]) as $s
     | if   $s == 0 then .[$i] = 0
       elif $s == 1 then .[$i] = (if $old[$i] == 1 then 1 else 0 end)
       else              .[$i] = (if $old[$i] == 1 then 0 else 1 end)
       end);


# pretty-print an array:
def pp: reduce .[] as $i (""; . + (if $i == 0 then " " else "*" end));

# continue until quiescence:
def go: recurse(. as $prev | next | if . == $prev then empty else . end) | pp;

# Example:
[0,1,1,1,0,1,1,0,1,0,1,0,1,0,1,0,0,1,0,0] | goute_next([0, 0], [0]).

compute_next([1, 0], [0]).

compute_next([0, 1], [0]).

compute_next([1, 1], [1]).

my_write(0) :-
	write(.).

my_write(1) :-
	write(#).

one_dimensional_cellular_automata :-
	L = [0,1,1,1,0,1,1,0,1,0,1,0,1,0,1,0,0,1,0,0],
	one_dimensional_cellular_automata(L).

```

```txt
 ?- one_dimensional_cellular_automata.
.###.##.#.#.#.#..#..
.#.#####.#.#.#......
..##...##.#.#.......
..##...###.#........
..##...#.##.........
..##....###.........
..##....#.#.........
..##.....#..........
..##................
true .

```



## PureBasic


```PureBasic
EnableExplicit
Dim cG.i(21)
Dim nG.i(21)
Define.i n, Gen

DataSection
  Data.i 0,1,1,1,0,1,1,0,1,0,1,0,1,0,1,0,0,1,0,0
EndDataSection
For n=1 To 20
  Read.i cG(n)
Next

OpenConsole()
Repeat
  Print("Generation "+Str(Gen)+": ")
  For n=1 To 20
    Print(Chr(95-cG(n)*60))
  Next
  Gen +1
  PrintN("")
  For n=1 To 20
    If (cG(n) And (cG(n-1) XOr cg(n+1))) Or (Not cG(n) And (cG(n-1) And cg(n+1)))
     nG(n)=1
   Else
     nG(n)=0
   EndIf
  Next
  CopyArray(nG(), cG())
Until Gen > 9

PrintN("Press any key to exit"): Repeat: Until Inkey() <> ""
```

```txt
Generation 0: _###_##_#_#_#_#__#__
Generation 1: _#_#####_#_#_#______
Generation 2: __##___##_#_#_______
Generation 3: __##___###_#________
Generation 4: __##___#_##_________
Generation 5: __##____###_________
Generation 6: __##____#_#_________
Generation 7: __##_____#__________
Generation 8: __##________________
Generation 9: __##________________
```



## Python


### Procedural


### =Python: Straightforward interpretation of spec=


```python
import random

printdead, printlive = '_#'
maxgenerations = 10
cellcount = 20
offendvalue = '0'

universe = ''.join(random.choice('01') for i in range(cellcount))

neighbours2newstate = {
 '000': '0',
 '001': '0',
 '010': '0',
 '011': '1',
 '100': '0',
 '101': '1',
 '110': '1',
 '111': '0',
 }

for i in range(maxgenerations):
    print "Generation %3i:  %s" % ( i,
          universe.replace('0', printdead).replace('1', printlive) )
    universe = offendvalue + universe + offendvalue
    universe = ''.join(neighbours2newstate[universe[i:i+3]] for i in range(cellcount))
```

```txt
Generation   0:  _###_##_#_#_#_#__#__
Generation   1:  _#_#####_#_#_#______
Generation   2:  __##___##_#_#_______
Generation   3:  __##___###_#________
Generation   4:  __##___#_##_________
Generation   5:  __##____###_________
Generation   6:  __##____#_#_________
Generation   7:  __##_____#__________
Generation   8:  __##________________
Generation   9:  __##________________
```



### =Python: Using boolean operators on bits=

The following implementation uses boolean operations to realize the function.

```python
import random

nquads = 5
maxgenerations = 10
fmt = '%%0%ix'%nquads
nbits = 4*nquads
a = random.getrandbits(nbits)  << 1
#a = int('01110110101010100100', 2) << 1
endmask = (2<<nbits)-2;
endvals = 0<<(nbits+1) | 0
tr = ('____', '___#', '__#_', '__##', '_#__', '_#_#', '_##_', '_###',
      '#___', '#__#', '#_#_', '#_##', '##__', '##_#', '###_', '####' )
for i in range(maxgenerations):
   print "Generation %3i:  %s" % (i,(''.join(tr[int(t,16)] for t in (fmt%(a>>1)))))
   a |= endvals
   a = ((a&((a<<1) | (a>>1))) ^ ((a<<1)&(a>>1))) & endmask
```



### =Python: Sum neighbours == 2=

This example makes use of the observation that a cell is alive in the next generation if the sum with its current neighbours of alive cells is two.

```python
>>>
 gen = [ch == '#' for ch in '_###_##_#_#_#_#__#__']
>>> for n in range(10):
	print(''.join('#' if cell else '_' for cell in gen))
	gen = [0] + gen + [0]
	gen = [sum(gen[m:m+3]) == 2 for m in range(len(gen)-2)]


_###_##_#_#_#_#__#__
_#_#####_#_#_#______
__##___##_#_#_______
__##___###_#________
__##___#_##_________
__##____###_________
__##____#_#_________
__##_____#__________
__##________________
__##________________
>>>
```



### Composition of pure functions


Interpreting the rule shown in the task description as Wolfram rule 104, and generalising enough to allow for other rules of this kind:

```python
"""Cellular Automata"""

from itertools import (islice, repeat)
from random import randint


# ruleSample :: Int -> String
def ruleSample(intRule):
    '''16 steps in the evolution of a specified Wolfram rule.'''
    return 'Rule ' + str(intRule) + ':\n' + (
        unlines(map(
            showCells,
            take(16)(
                iterate(nextRowByRule(intRule))(
                    onePixelInLineOf(64) if bool(randint(0, 1)) else (
                        randomPixelsInLineOf(64)
                    )
                )
            )
        ))
    )


# nextRowByRule :: Int -> [Bool] -> [Bool]
def nextRowByRule(intRule):
    '''A row of booleans derived by Wolfram rule n
       from another boolean row of the same length.'''

    # step :: (Bool, Bool, Bool) -> Bool
    def step(l, x, r):
        return bool(intRule & 2**intFromBools([l, x, r]))

    # go :: [Bool] -> Bool
    def go(xs):
        return [False] + list(map(
            step,
            xs, xs[1:], xs[2:]
        )) + [False]
    return lambda xs: go(xs)


# TEST ----------------------------------------------------
# main :: IO ()
def main():
    '''Samples of Wolfram rule evolutions.'''

    print(
        unlines(map(ruleSample, [104, 30, 110]))
    )


# boolsFromInt :: Int -> [Bool]
def boolsFromInt(n):
    '''List of booleans derived by binary
       decomposition of an integer.'''
    def go(x):
        return Just((x // 2, bool(x % 2))) if x else Nothing()
    return unfoldl(go)(n)


# intFromBools :: [Bool] -> Int
def intFromBools(xs):
    '''Integer derived by binary interpretation
       of a list of booleans.'''
    def go(b, pn):
        power, n = pn
        return (2 * power, n + power if b else n)
    return foldr(go)([1, 0])(xs)[1]


# nBoolsFromInt :: Int -> Int -> [Bool]
def nBoolsFromInt(n):
    '''List of bools, left-padded to given length n,
       derived by binary decomposition of an integer x.'''
    def go(n, x):
        bs = boolsFromInt(x)
        return list(repeat(False, n - len(bs))) + bs
    return lambda x: go(n, x)


# onePixelInLineOf :: Int -> [Bool]
def onePixelInLineOf(n):
    '''A row of n (mainly False) booleans,
       with a single True value in the middle.'''
    return nBoolsFromInt(n)(
        2**(n // 2)
    )


# randomPixelsInLineOf :: Int -> [Bool]
def randomPixelsInLineOf(n):
    '''A row of n booleans with pseudorandom values.'''
    return [bool(randint(0, 1)) for _ in range(1, 1 + n)]


# showCells :: [Bool] -> String
def showCells(xs):
    '''A block string representation of a list of booleans.'''
    return ''.join([chr(9608) if x else ' ' for x in xs])


# GENERIC -------------------------------------------------

# Just :: a -> Maybe a
def Just(x):
    '''Constructor for an inhabited Maybe (option type) value.'''
    return {'type': 'Maybe', 'Nothing': False, 'Just': x}


# Nothing :: Maybe a
def Nothing():
    '''Constructor for an empty Maybe (option type) value.'''
    return {'type': 'Maybe', 'Nothing': True}


# foldr :: (a -> b -> b) -> b -> [a] -> b
def foldr(f):
    '''Right to left reduction of a list,
       using a binary operator.'''
    def go(v, xs):
        a = v
        for x in xs:
            a = f(x, a)
        return a
    return lambda acc: lambda xs: go(acc, xs[::-1])


# iterate :: (a -> a) -> a -> Gen [a]
def iterate(f):
    '''An infinite list of repeated applications of f to x.'''
    def go(x):
        v = x
        while True:
            yield v
            v = f(v)
    return lambda x: go(x)


# take :: Int -> [a] -> [a]
# take :: Int -> String -> String
def take(n):
    '''The prefix of xs of length n,
       or xs itself if n > length xs.'''
    return lambda xs: (
        xs[0:n]
        if isinstance(xs, list)
        else list(islice(xs, n))
    )


# unfoldl(lambda x: Just(((x - 1), x)) if 0 != x else Nothing())(10)
# -> [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
# unfoldl :: (b -> Maybe (b, a)) -> b -> [a]
def unfoldl(f):
    '''Dual to reduce or foldl.
       Where these reduce a list to a summary value, unfoldl
       builds a list from a seed value.
       Where f returns Just(a, b), a is appended to the list,
       and the residual b is used as the argument for the next
       application of f.
       When f returns Nothing, the completed list is returned.'''
    def go(v):
        xr = v, v
        xs = []
        while True:
            mb = f(xr[0])
            if mb.get('Nothing'):
                return xs
            else:
                xr = mb.get('Just')
                xs.insert(0, xr[1])
        return xs
    return lambda x: go(x)


# unlines :: [String] -> String
def unlines(xs):
    '''A single newline-delimited string derived
       from a list of strings.'''
    return '\n'.join(xs)


# MAIN -------------------------------------------------
if __name__ == '__main__':
    main()
```

```txt
Rule 104:
    █  █  ████  ██    █   █      █ █ █ ██    █████ ██  ██  █ ██
          █  █  ██                █ █ ███    █   ████  ██   ███
                ██                 █ ██ █        █  █  ██   █ █
                ██                  ████               ██    █
                ██                  █  █               ██
                ██                                     ██
                ██                                     ██
                ██                                     ██
                ██                                     ██
                ██                                     ██
                ██                                     ██
                ██                                     ██
                ██                                     ██
                ██                                     ██
                ██                                     ██
                ██                                     ██
Rule 30:
                               █
                              ███
                             ██  █
                            ██ ████
                           ██  █   █
                          ██ ████ ███
                         ██  █    █  █
                        ██ ████  ██████
                       ██  █   ███     █
                      ██ ████ ██  █   ███
                     ██  █    █ ████ ██  █
                    ██ ████  ██ █    █ ████
                   ██  █   ███  ██  ██ █   █
                  ██ ████ ██  ███ ███  ██ ███
                 ██  █    █ ███   █  ███  █  █
                ██ ████  ██ █  █ █████  ███████
Rule 110:
█  █  ██ ██  ██  █ █  ██  ███ █ █ ███     ██ ██    █    █   █
  ██ ██████ ███ ████ ███ ██ ███████ █    ██████   ██   ██  ██
 █████    ███ ███  ███ ██████     ███   ██    █  ███  ███ ███
 █   █   ██ ███ █ ██ ███    █    ██ █  ███   ██ ██ █ ██ ███ █
 █  ██  █████ ████████ █   ██   █████ ██ █  █████████████ ███
 █ ███ ██   ███      ███  ███  ██   ██████ ██           ███ █
 ███ ████  ██ █     ██ █ ██ █ ███  ██    ████          ██ ███
 █ ███  █ █████    ████████████ █ ███   ██  █         █████ █
 ███ █ ████   █   ██          █████ █  ███ ██        ██   ███
 █ █████  █  ██  ███         ██   ███ ██ ████       ███  ██ █
 ███   █ ██ ███ ██ █        ███  ██ ██████  █      ██ █ █████
 █ █  ███████ ██████       ██ █ █████    █ ██     ███████   █
 ███ ██     ███    █      ███████   █   █████    ██     █  ██
 █ ████    ██ █   ██     ██     █  ██  ██   █   ███    ██ ███
 ███  █   █████  ███    ███    ██ ███ ███  ██  ██ █   █████ █
 █ █ ██  ██   █ ██ █   ██ █   █████ ███ █ ███ █████  ██   ███
```



## R



```R
set.seed(15797, kind="Mersenne-Twister")

maxgenerations = 10
cellcount = 20
offendvalue = FALSE

## Cells are alive if TRUE, dead if FALSE
universe <- c(offendvalue,
              sample( c(TRUE, FALSE), cellcount, replace=TRUE),
              offendvalue)

## List of patterns in which the cell stays alive
stayingAlive <- lapply(list(c(1,1,0),
                            c(1,0,1),
                            c(0,1,0)), as.logical)

## x : length 3 logical vector
## map: list of length 3 logical vectors that map to patterns
##      in which x stays alive
deadOrAlive <- function(x, map) list(x) %in% map

cellularAutomata <- function(x, map) {
    c(x[1], apply(embed(x, 3), 1, deadOrAlive, map=map), x[length(x)])
}

deadOrAlive2string <- function(x) {
    paste(ifelse(x, '#', '_'), collapse="")
}

for (i in 1:maxgenerations) {
    universe <- cellularAutomata(universe, stayingAlive)
    cat(format(i, width=3), deadOrAlive2string(universe), "\n")
}
```


```txt

  1 _##_____####_#___#_#__
  2 _##_____#__##_____#___
  3 _##________##_________
  4 _##________##_________
  5 _##________##_________
  6 _##________##_________
  7 _##________##_________
  8 _##________##_________
  9 _##________##_________
 10 _##________##_________

```



## Racket


```racket
#lang racket

(define (update cells)
  (for/list ([crowding (map +
                            (append '(0) (drop-right cells 1))
                            cells
                            (append (drop cells 1) '(0)))])
    (if (= 2 crowding) 1 0)))

(define (life-of cells time)
  (unless (zero? time)
    (displayln cells)
    (life-of (update cells) (sub1 time))))

(life-of '(0 1 1 1 0 1 1 0 1 0 1 0 1 0 1 0 0 1 0 0)
         10)

#| (0 1 1 1 0 1 1 0 1 0 1 0 1 0 1 0 0 1 0 0)
   (0 1 0 1 1 1 1 1 0 1 0 1 0 1 0 0 0 0 0 0)
   (0 0 1 1 0 0 0 1 1 0 1 0 1 0 0 0 0 0 0 0)
   (0 0 1 1 0 0 0 1 1 1 0 1 0 0 0 0 0 0 0 0)
   (0 0 1 1 0 0 0 1 0 1 1 0 0 0 0 0 0 0 0 0)
   (0 0 1 1 0 0 0 0 1 1 1 0 0 0 0 0 0 0 0 0)
   (0 0 1 1 0 0 0 0 1 0 1 0 0 0 0 0 0 0 0 0)
   (0 0 1 1 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0)
   (0 0 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
   (0 0 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) |#
```


Below is an alternative implementation using graphical output in the Racket REPL.
It works with DrRacket and Emacs + Geiser.

```racket
#lang slideshow

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Simulation of cellular automata, as described by Stephen Wolfram in his 1983 paper.
;; Uses Racket's inline image display capability for visual presentation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require racket/draw)
(require slideshow)

(define *rules* '((1 1 1) (1 1 0) (1 0 1) (1 0 0)
		  (0 1 1) (0 1 0) (0 0 1) (0 0 0)))

(define (bordered-square n)
  (filled-rectangle n n #:draw-border? #t))

(define (draw-row lst)
  (apply hc-append 2 (map (λ (x) (colorize (bordered-square 10) (cond ((= x 0) "gray")
								      ((= x 1) "red")
								      (else "gray"))))
			  lst)))

(define (extract-neighborhood nth prev-row)
  (take (drop (append '(0) prev-row '(0)) nth) 3))

(define (automaton-to-bits n)
  (reverse (map (λ (y) (if (zero? (bitwise-and y n)) 0 1))
		(map (λ (x) (expt 2 x)) (range 0 8)))))

(define (get-rules bits)
  (map cdr (filter (λ (x) (= (car x) 1)) (map cons bits *rules*))))

(define (advance-row old-row rules)
  (let ([new '()])
    (for ([i (in-range 0 (length old-row))])
      (set! new (cons (if (member (extract-neighborhood i old-row)
				  rules) 1 0) new)))
    (reverse new)))

(define (draw-automaton automaton init-row row-number)
  (let* ([bit-representation (automaton-to-bits automaton)]
	 [rules (get-rules bit-representation)]
	 [rows (list init-row)])
    (for ([i (in-range 1 row-number)])
      (set! rows (cons (advance-row (car rows) rules)
		       rows)))
    (apply vc-append 2 (map draw-row (reverse rows)))))

(draw-automaton 104 '(0 1 1 1 0 1 1 0 1 0 1 0 1 0 1 0 0 1 0 0) 10)
```



## REXX

This REXX version will show (as a default)   40   generations,   or less if the generations of cellular automata repeat.

```rexx
/*REXX program generates & displays N generations of one─dimensional cellular automata. */
parse arg $ gens .                               /*obtain optional arguments from the CL*/
if    $=='' |    $==","  then $=001110110101010  /*Not specified?  Then use the default.*/
if gens=='' | gens==","  then gens=40            /* "      "         "   "   "     "    */

   do #=0  for gens                              /* process the  one-dimensional  cells.*/
   say  " generation"    right(#,length(gens))       ' '       translate($, "#·", 10)
   @=0                                                                /* [↓] generation.*/
          do j=2  for length($) - 1;          x=substr($, j-1, 3)     /*obtain the cell.*/
          if x==011 | x==101 | x==110  then @=overlay(1, @, j)        /*the cell lives. */
                                       else @=overlay(0, @, j)        /* "   "    dies. */
          end   /*j*/

   if $==@  then do;  say right('repeats', 40);  leave;  end          /*does it repeat? */
   $=@                                           /*now use the next generation of cells.*/
   end       /*#*/                               /*stick a fork in it,  we're all done. */
```

'''output''' when using the default input:

```txt

 generation  0   ··###·##·#·#·#·
 generation  1   ··#·#####·#·#··
 generation  2   ···##···##·#···
 generation  3   ···##···###····
 generation  4   ···##···#·#····
 generation  5   ···##····#·····
 generation  6   ···##··········
                                 repeats

```



## Retro


```Retro
# 1D Cellular Automota

Assume an array of cells with an initial distribution of live and
dead cells, and imaginary cells off the end of the array having
fixed values.

Cells in the next generation of the array are calculated based on
the value of the cell and its left and right nearest neighbors in
the current generation.

If, in the following table, a live cell is represented by 1 and a
dead cell by 0 then to generate the value of the cell at a particular
index in the array of cellular values you use the following table:

000 -> 0  #
001 -> 0  #
010 -> 0  # Dies without enough neighbours
011 -> 1  # Needs one neighbour to survive
100 -> 0  #
101 -> 1  # Two neighbours giving birth
110 -> 1  # Needs one neighbour to survive
111 -> 0  # Starved to death.

I had originally written an implementation of this in RETRO 11.
For RETRO 12 I took advantage of new language features and some
further considerations into the rules for this task.

The first word, `string,` inlines a string to `here`. I'll use
this to setup the initial input.

~~~
:string, (s-) [ , ] s:for-each #0 , ;
~~~

The next two lines setup an initial generation and a buffer for
the evolved generation. In this case, `This` is the current
generation and `Next` reflects the next step in the evolution.

~~~
'This d:create
  '.###.##.#.#.#.#..#.. string,

'Next d:create
  '.................... string,
~~~

I use `display` to show the current generation.

~~~
:display (-)
  &This s:put nl ;
~~~

As might be expected, `update` copies the `Next` generation to
the `This` generation, setting things up for the next cycle.

~~~
:update (-)
  &Next &This dup s:length copy ;
~~~

The word `group` extracts a group of three cells. This data will
be passed to `evolve` for processing.

~~~
:group (a-nnn)
  [ fetch ]
  [ n:inc fetch ]
  [ n:inc n:inc fetch ] tri ;
~~~

I use `evolve` to decide how a cell should change, based on its
initial state with relation to its neighbors.

In the prior implementation this part was much more complex as I
tallied things up and had separate conditions for each combination.
This time I take advantage of the fact that only cells with two
neighbors will be alive in the next generation. So the process is:

- take the data from `group`
- compare to `$#` (for living cells)
- add the flags
- if the result is `#-2`, the cell should live
- otherwise it'll be dead

~~~
:evolve (nnn-c)
  [ $# eq? ] tri@ + +
  #-2 eq? [ $# ] [ $. ] choose ;
~~~

For readability I separated out the next few things. `at` takes an
index and returns the address in `This` starting with the index.

~~~
:at (n-na)
  &This over + ;
~~~

The `record` word adds the evolved value to a buffer. In this case
my `generation` code will set the buffer to `Next`.

~~~
:record (c-)
  buffer:add n:inc ;
~~~

And now to tie it all together. Meet `generation`, the longest bit
of code in this sample. It has several bits:

- setup a new buffer pointing to `Next`

  - this also preserves the old buffer

- setup a loop for each cell in `This`

  - initial loop index at -1, to ensure proper dummy state for first cell
  - get length of `This` generation

- perform a loop for each item in the generation, updating `Next` as it goes

- copy `Next` to `This` using `update`.

~~~
:generation (-)
  [ &Next buffer:set
    #-1 &This s:length
    [ at group evolve record ] times drop
    update
  ] buffer:preserve ;
~~~

The last bit is a helper. It takes a number of generations and displays
the state, then runs a `generation`.

~~~
:generations (n-)
  [ display generation ] times ;
~~~

And a text. The output should be:

    .###.##.#.#.#.#..#..
    .#.#####.#.#.#......
    ..##...##.#.#.......
    ..##...###.#........
    ..##...#.##.........
    ..##....###.........
    ..##....#.#.........
    ..##.....#..........
    ..##................
    ..##................

~~~
#10 generations
~~~
```



## Ring


```ring

# Project : One-dimensional cellular automata

rule = ["0", "0", "0", "1", "0", "1", "1", "0"]
now = "01110110101010100100"

for generation = 0 to 9
    see "generation " + generation + ": " + now + nl
    nxt = ""
    for cell = 1 to len(now)
        str = "bintodec(" + '"' +substr("0"+now+"0", cell, 3) + '"' + ")"
        eval("p=" + str)
        nxt = nxt + rule[p+1]
    next
    temp = nxt
    nxt = now
    now = temp
next

func bintodec(bin)
     binsum = 0
     for n=1  to len(bin)
         binsum = binsum + number(bin[n]) *pow(2, len(bin)-n)
     next
     return binsum

```

Output:

```txt

generation 0: 01110110101010100100
generation 1: 01011111010101000000
generation 2: 00110001101010000000
generation 3: 00110001110100000000
generation 4: 00110001011000000000
generation 5: 00110000111000000000
generation 6: 00110000101000000000
generation 7: 00110000010000000000
generation 8: 00110000000000000000
generation 9: 00110000000000000000

```



## Ruby


```ruby
def evolve(ary)
  ([0]+ary+[0]).each_cons(3).map{|a,b,c| a+b+c == 2 ? 1 : 0}
end

def printit(ary)
  puts ary.join.tr("01",".#")
end

ary = [0,1,1,1,0,1,1,0,1,0,1,0,1,0,1,0,0,1,0,0]
printit ary
until ary == (new = evolve(ary))
  printit ary = new
end
```

```txt
.###.##.#.#.#.#..#..
.#.#####.#.#.#......
..##...##.#.#.......
..##...###.#........
..##...#.##.........
..##....###.........
..##....#.#.........
..##.....#..........
..##................
```



## Rust


```rust
fn get_new_state(windowed: &[bool]) -> bool {
    match windowed {
        [false, true, true] | [true, true, false] => true,
        _ => false
    }
}

fn next_gen(cell: &mut [bool]) {
    let mut v = Vec::with_capacity(cell.len());
    v.push(cell[0]);
    for i in cell.windows(3) {
        v.push(get_new_state(i));
    }
    v.push(cell[cell.len() - 1]);
    cell.copy_from_slice(&v);
}

fn print_cell(cell: &[bool]) {
    for v in cell {
        print!("{} ", if *v {'#'} else {' '});
    }
    println!();
}

fn main() {

    const MAX_GENERATION: usize = 10;
    const CELLS_LENGTH: usize = 30;

    let mut cell: [bool; CELLS_LENGTH] = rand::random();

    for i in 1..=MAX_GENERATION {
        print!("Gen {:2}: ", i);
        print_cell(&cell);
        next_gen(&mut cell);
    }
}

```



## Scala

```scala
def cellularAutomata(s: String) = {
  def it = Iterator.iterate(s) ( generation =>
    ("_%s_" format generation).iterator
    sliding 3
    map (_ count (_ == '#'))
    map Map(2 -> "#").withDefaultValue("_")
    mkString
  )

  (it drop 1) zip it takeWhile Function.tupled(_ != _) map (_._2) foreach println
}
```


Sample:


```txt

scala> cellularAutomata("_###_##_#_#_#_#__#__")
_###_##_#_#_#_#__#__
_#_#####_#_#_#______
__##___##_#_#_______
__##___###_#________
__##___#_##_________
__##____###_________
__##____#_#_________
__##_____#__________

```



## Scheme

```scheme
(define (next-generation left petri-dish right)
  (if (null? petri-dish)
      (list)
      (cons (if (= (+ left
                      (car petri-dish)
                      (if (null? (cdr petri-dish))
                          right
                          (cadr petri-dish)))
                   2)
                1
                0)
            (next-generation (car petri-dish) (cdr petri-dish) right))))

(define (display-evolution petri-dish generations)
  (if (not (zero? generations))
      (begin (display petri-dish)
             (newline)
             (display-evolution (next-generation 0 petri-dish 0)
                                (- generations 1)))))

(display-evolution (list 1 1 1 0 1 1 0 1 0 1 0 1 0 1 0 0 1 0) 10)
```

Output:

```txt
(1 1 1 0 1 1 0 1 0 1 0 1 0 1 0 0 1 0)
(1 0 1 1 1 1 1 0 1 0 1 0 1 0 0 0 0 0)
(0 1 1 0 0 0 1 1 0 1 0 1 0 0 0 0 0 0)
(0 1 1 0 0 0 1 1 1 0 1 0 0 0 0 0 0 0)
(0 1 1 0 0 0 1 0 1 1 0 0 0 0 0 0 0 0)
(0 1 1 0 0 0 0 1 1 1 0 0 0 0 0 0 0 0)
(0 1 1 0 0 0 0 1 0 1 0 0 0 0 0 0 0 0)
(0 1 1 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0)
(0 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
(0 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
```



## Seed7

A graphical cellular automaton can be found [http://seed7.sourceforge.net/algorith/graphic.htm#cellauto here].


```seed7
$ include "seed7_05.s7i";

const string: start is "_###_##_#_#_#_#__#__";

const proc: main is func
  local
    var string: g0 is start;
    var string: g1 is start;
    var integer: generation is 0;
    var integer: i is 0;
  begin
    writeln(g0);
    for generation range 0 to 9 do
      for i range 2 to pred(length(g0)) do
        if g0[i-1] <> g0[i+1] then
          g1 @:= [i] g0[i];
        elsif g0[i] = '_' then
          g1 @:= [i] g0[i-1];
        else
          g1 @:= [i] '_'
        end if;
      end for;
      writeln(g1);
      g0 := g1;
    end for;
  end func;
```


Output:

```txt

_###_##_#_#_#_#__#__
_#_#####_#_#_#______
__##___##_#_#_______
__##___###_#________
__##___#_##_________
__##____###_________
__##____#_#_________
__##_____#__________
__##________________
__##________________
__##________________

```



## SequenceL


```sequencel
import <Utilities/Conversion.sl>
;

main(args(2)) :=
    run(args[1], stringToInt(args[2])) when size(args) = 2
else
    "Usage error: exec <initialCells> <generations>";

stringToCells(string(1))[i] := 0 when string[i] = '_' else 1;
cellsToString(cells(1))[i] := '#' when cells[i] = 1 else '_';

run(cellsString(1), generations) :=
        runHelper(stringToCells(cellsString), generations, cellsString);

runHelper(cells(1), generations, result(1)) :=
    let
        nextCells := step(cells);
    in
        result when generations = 0
    else
        runHelper(nextCells, generations - 1,
                  result ++ "\n" ++ cellsToString(nextCells));

step(cells(1))[i] :=
    let
        left := cells[i-1] when i > 1 else 0;
        right := cells[i + 1] when i < size(cells) else 0;
    in
        1 when (left + cells[i] + right) = 2
    else
        0;
```


```txt

"_###_##_#_#_#_#__#__
_#_#####_#_#_#______
__##___##_#_#_______
__##___###_#________
__##___#_##_________
__##____###_________
__##____#_#_________
__##_____#__________
__##________________
__##________________
__##________________"

```



## Sidef

```ruby
var seq = "_###_##_#_#_#_#__#__";
var x = '';

loop {
    seq.tr!('01', '_#');
    say seq;
    seq.tr!('_#', '01');
    seq.gsub!(/(?<=(.))(.)(?=(.))/, {|s1,s2,s3| s1 == s3 ? (s1 ? 1-s2 : 0) : s2});
    (x != seq) && (x = seq) || break;
}
```


```txt

_###_##_#_#_#_#__#__
_#_#####_#_#_#______
__##___##_#_#_______
__##___###_#________
__##___#_##_________
__##____###_________
__##____#_#_________
__##_____#__________
__##________________

```


```ruby
class Automaton(rule, cells) {

    method init {
        rule = sprintf("%08b", rule).chars.map{.to_i}.reverse;
    }

    method next {
        var previous = cells.map{_};
        var len = previous.len;
        cells[] = rule[
                previous.range.map { |i|
                    4*previous[i-1 % len] +
                    2*previous[i]         +
                      previous[i+1 % len]
                }...
            ]
    }

    method to_s {
        cells.map { _ ? '#' : ' ' }.join;
    }
}

var size = 10;
var auto = Automaton(
    rule: 104,
    cells: [(size/2).of(0)..., 111011010101.digits..., (size/2).of(0)...],
);

size.times {
    say "|#{auto}|";
    auto.next;
}
```


```txt

|     ### ## # # #     |
|     # ##### # #      |
|      ##   ## #       |
|      ##   ###        |
|      ##   # #        |
|      ##    #         |
|      ##              |
|      ##              |
|      ##              |
|      ##              |

```



## Tcl


```tcl
proc evolve {a} {
    set new [list]
    for {set i 0} {$i < [llength $a]} {incr i} {
        lappend new [fate $a $i]
    }
    return $new
}

proc fate {a i} {
    return [expr {[sum $a $i] == 2}]
}

proc sum {a i} {
    set sum 0
    set start [expr {$i - 1 < 0 ? 0 : $i - 1}]
    set end [expr {$i + 1 >= [llength $a] ? $i : $i + 1}]
    for {set j $start} {$j <= $end} {incr j} {
        incr sum [lindex $a $j]
    }
    return $sum
}

proc print {a} {
    puts [string map {0 _ 1 #} [join $a ""]]
}

proc parse {s} {
    return [split [string map {_ 0 # 1} $s] ""]
}

set array [parse "_###_##_#_#_#_#__#__"]
print $array
while {[set new [evolve $array]] ne $array} {
    set array $new
    print $array
}
```



## Ursala

Three functions are defined. Rule takes a neighborhood of three cells to the
succeeding value of the middle one, step takes a list of cells to its successor
by applying the rule across a sliding window,
and evolve takes an initial list of cells to a list of those evolving from
it according to the rule. The cells are maintained as a list of booleans
(0 and &) but are converted to characters for presentation in the example code.

```Ursala
#import std
#import nat

rule = -$<0,0,0,&,0,&,&,0>@rSS zipp0*ziD iota8

step = rule*+ swin3+ :/0+ --<0>

evolve "n" = @iNC ~&x+ rep"n" ^C/step@h ~&

#show+

example =  ~&?(`#!,`.!)** evolve10 <0,&,&,&,0,&,&,0,&,0,&,0,&,0,0,&,0,0>
```

output:

```txt

.###.##.#.#.#..#..
.#.#####.#.#......
..##...##.#.......
..##...###........
..##...#.#........
..##....#.........
..##..............
..##..............
..##..............
..##..............
..##..............
```




## Vedit macro language


This implementation writes the calculated patterns into an edit buffer, where the results can viewed and saved into a file if required. The edit buffer also acts as storage during calculations.

```vedit
IT("Gen 0: ..###.##.#.#.#.#..#.....")     // initial pattern
#9  = Cur_Col

for (#8 = 1; #8 < 10; #8++) {             // 10 generations
    Goto_Col(7)
    Reg_Empty(20)
    while (Cur_Col < #9-1) {
        if (Match("|{##|!#,#.#,|!###}")==0) {
            Reg_Set(20, "#", APPEND)
        } else {
            Reg_Set(20, ".", APPEND)
        }
        Char
    }
    EOL IN
    IT("Gen ") Num_Ins(#8, LEFT+NOCR) IT(": ")
    Reg_Ins(20)
}
```


Sample output:

```vedit
Gen 0: ..###.##.#.#.#.#..#.....
Gen 1: ..#.#####.#.#.#.........
Gen 2: ...##...##.#.#..........
Gen 3: ...##...###.#...........
Gen 4: ...##...#.##............
Gen 5: ...##....###............
Gen 6: ...##....#.#............
Gen 7: ...##.....#.............
Gen 8: ...##...................
Gen 9: ...##...................
```



## Visual Basic .NET


This implementation is run from the command line.  The command is followed by a string of either 1's or #'s for an active cell, or 0's or _'s for an inactive one.


```Visual Basic .NET
Imports System.Text

Module CellularAutomata

    Private Enum PetriStatus
        Active
        Stable
        Dead
    End Enum

    Function Main(ByVal cmdArgs() As String) As Integer
        If cmdArgs.Length = 0 Or cmdArgs.Length > 1 Then
            Console.WriteLine("Command requires string of either 1s and 0s or #s and _s.")
            Return 1
        End If

        Dim petriDish As BitArray

        Try
            petriDish = InitialisePetriDish(cmdArgs(0))
        Catch ex As Exception
            Console.WriteLine(ex.Message)
            Return 1
        End Try

        Dim generation As Integer = 0
        Dim ps As PetriStatus = PetriStatus.Active

        Do While True
            If ps = PetriStatus.Stable Then
                Console.WriteLine("Sample stable after {0} generations.", generation - 1)
                Exit Do
            Else
                Console.WriteLine("{0}: {1}", generation.ToString("D3"), BuildDishString(petriDish))
                If ps = PetriStatus.Dead Then
                    Console.WriteLine("Sample dead after {0} generations.", generation)
                    Exit Do
                End If
            End If

            ps = GetNextGeneration(petriDish)
            generation += 1
        Loop

        Return 0
    End Function

    Private Function InitialisePetriDish(ByVal Sample As String) As BitArray
        Dim PetriDish As New BitArray(Sample.Length)
        Dim dead As Boolean = True

        For i As Integer = 0 To Sample.Length - 1
            Select Case Sample.Substring(i, 1)
                Case "1", "#"
                    PetriDish(i) = True
                    dead = False
                Case "0", "_"
                    PetriDish(i) = False
                Case Else
                    Throw New Exception("Illegal value in string position " & i)
                    Return Nothing
            End Select
        Next

        If dead Then
            Throw New Exception("Entered sample is dead.")
            Return Nothing
        End If

        Return PetriDish
    End Function

    Private Function GetNextGeneration(ByRef PetriDish As BitArray) As PetriStatus
        Dim petriCache = New BitArray(PetriDish.Length)
        Dim neighbours As Integer
        Dim stable As Boolean = True
        Dim dead As Boolean = True

        For i As Integer = 0 To PetriDish.Length - 1
            neighbours = 0
            If i > 0 AndAlso PetriDish(i - 1) Then neighbours += 1
            If i < PetriDish.Length - 1 AndAlso PetriDish(i + 1) Then neighbours += 1

            petriCache(i) = (PetriDish(i) And neighbours = 1) OrElse (Not PetriDish(i) And neighbours = 2)
            If PetriDish(i) <> petriCache(i) Then stable = False
            If petriCache(i) Then dead = False
        Next

        PetriDish = petriCache

        If dead Then Return PetriStatus.Dead
        If stable Then Return PetriStatus.Stable
        Return PetriStatus.Active

    End Function

    Private Function BuildDishString(ByVal PetriDish As BitArray) As String
        Dim sw As New StringBuilder()
        For Each b As Boolean In PetriDish
            sw.Append(IIf(b, "#", "_"))
        Next

        Return sw.ToString()
    End Function
End Module
```


Output:

```txt
C:\>CellularAutomata _###_##_#_#_#_#__#__
000: _###_##_#_#_#_#__#__
001: _#_#####_#_#_#______
002: __##___##_#_#_______
003: __##___###_#________
004: __##___#_##_________
005: __##____###_________
006: __##____#_#_________
007: __##_____#__________
008: __##________________
Sample stable after 8 generations.
```



## Wart


### Simple


```python
def (gens n l)
  prn l
  repeat n
    zap! gen l
    prn l

def (gen l)
  with (a nil  b nil  c l.0)
    collect nil  # won't insert paren without second token
      each x cdr.l
        shift! a b c x
        yield (next a b c)
      yield (next b c nil)

def (next a b c)  # next state of b given neighbors a and c
  if (and a c)  not.b
     (or a c)  b
```


Output looks a little ugly:


```txt
ready! type in an expression, then hit enter twice. ctrl-d exits.
gens 5 '(1 1 1 nil 1)

(1 1 1 nil 1)
(1 nil 1 1 nil)
(nil 1 1 1 nil)
(nil 1 nil 1 nil)
(nil nil 1 nil nil)
(nil nil nil nil nil)
```



### More sophisticated

Computing the next generation becomes much cleaner once you invest a few LoC in a new datatype.


```python
def (uca l)  # new datatype: Uni-dimensional Cellular Automaton
  (tag uca (list l len.l))

def (len l) :case (isa uca l)  # how to compute its length
  rep.l.1

defcoerce uca list  # how to convert it to a list
  (fn(_) rep._.0)

def (pr l) :case (isa uca l)  # how to print it
  each x l  # transparently coerces to a list for iterating over
    pr (if x "#" "_")

# (l i) returns ith cell when l is a uca, and nil when i is out-of-bounds
defcall uca (l i)
  if (0 <= i < len.l)
    rep.l.0.i

def (gens n l)
  prn l
  repeat n
    zap! gen l
    prn l

def (gen l)
  uca+collect+for i 0 (i < len.l) ++i
    yield (next  (l i-1)  l.i  (l i+1))

# next state of b, given neighbors a and c
def (next a b c)
  if (and a c) not.b
     (or a c)  b
```


Output is prettier now:


```txt
ready! type in an expression, then hit enter twice. ctrl-d exits.
gens 10 (uca '(nil 1 1 1 nil 1 1 nil 1 nil 1 nil 1 nil 1 nil nil 1 nil nil))

_###_##_#_#_#_#__#__
_#_#####_#_#_#______
__##___##_#_#_______
__##___###_#________
__##___#_##_________
__##____###_________
__##____#_#_________
__##_____#__________
__##________________
__##________________
__##________________
```



## XPL0


```XPL0
code ChOut=8, CrLf=9;
int  Gen, Now, New, I;
[Now:= $076A_A400;
for Gen:= 1 to 10 do
    [for I:= 31 downto 0 do ChOut(0, if Now & 1<<I then ^# else ^_);
    CrLf(0);
    New:= 0;
    for I:= 30 downto 1 do
        case Now>>(I-1) & 7 of %011, %101, %110: New:= New ! 1<<I other;
    Now:= New;
    ];
]
```


```txt

_____###_##_#_#_#_#__#__________
_____#_#####_#_#_#______________
______##___##_#_#_______________
______##___###_#________________
______##___#_##_________________
______##____###_________________
______##____#_#_________________
______##_____#__________________
______##________________________
______##________________________

```



## Yabasic

```Yabasic
10 n=10:READ w:DIM x(w+1),x2(w+1):FOR i=1 to w:READ x(i):NEXT
20 FOR k=1 TO n
30 FOR j=1 TO w
40 IF x(j) THEN PRINT "#"; ELSE PRINT "_"; END IF
50 IF x(j-1)+x(j)+x(j+1)=2 THEN x2(j)=1 ELSE x2(j)=0 END IF
60 NEXT:PRINT
70 FOR j=1 TO w:x(j)=x2(j):NEXT
80 NEXT
90 DATA 20,0,1,1,1,0,1,1,0,1,0,1,0,1,0,1,0,0,1,0,0
```


Other solution

```Yabasic
start$ = "0,1,1,1,0,1,1,0,1,0,1,0,1,0,1,0,0,1,0,0"

dim x$(1)

for k = 1 to 10
    n = token(start$, x$(), ",")
    redim x$(n+1)
    start$ = ""
    for j = 1 to n
        if val(x$(j)) then print "#"; else print "_"; end if
        test = abs(val(x$(j-1)) + val(x$(j)) + val(x$(j+1)) = 2)
        start$ = start$ + str$(test) + ","
    next j
    print
next k
```



## zkl

```zkl
fcn life1D(line){
   right:=line[1,*] + False;           // shift left, False fill
   left :=T(False).extend(line[0,-1]); // shift right
   left.zip(line,right).apply(fcn(hood){ hood.sum(0)==2 });
}
```


```zkl
chars:=T("_","#");
cells:="_###_##_#_#_#_#__#__".split("").apply('==("#")); //-->L(False,True,True,True,False...)
do(10){ cells.apply(chars.get).concat().println(); cells=life1D(cells); }
```

Or, using strings instead of lists:

```zkl
fcn life1D(line){
   right:=line[1,*] + "_";  // shift left, "_" fill
   left :="_" + line[0,-1]; // shift right
   Utils.Helpers.zipWith(
      fcn(a,b,c){ (String(a,b,c) - "_") == "##" and "#" or "_" },
      left,line,right).concat();
}
```


```zkl
cells:="_###_##_#_#_#_#__#__";
do(10){ cells.println(); cells=life1D(cells); }
```

```txt

_###_##_#_#_#_#__#__
_#_#####_#_#_#______
__##___##_#_#_______
__##___###_#________
__##___#_##_________
__##____###_________
__##____#_#_________
__##_____#__________
__##________________

```

/pre>


## Seed7

A graphical cellular automaton can be found [http://seed7.sourceforge.net/algorith/graphic.htm#cellauto here].

> petriCache(i) Then stable = False
            If petriCache(i) Then dead = False
        Next

        PetriDish = petriCache

        If dead Then Return PetriStatus.Dead
        If stable Then Return PetriStatus.Stable
        Return PetriStatus.Active

    End Function

    Private Function BuildDishString(ByVal PetriDish As BitArray) As String
        Dim sw As New StringBuilder()
        For Each b As Boolean In PetriDish
            sw.Append(IIf(b, "#", "_"))
        Next

        Return sw.ToString()
    End Function
End Module
