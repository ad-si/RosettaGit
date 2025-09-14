+++
title = "24 game"
description = ""
date = 2019-10-17T13:27:31Z
aliases = []
[extra]
id = 4938
[taxonomies]
categories = ["Games", "task"]
tags = []
languages = [
  "8th",
  "abap",
  "ada",
  "apl",
  "argile",
  "autohotkey",
  "autoit",
  "bbc_basic",
  "befunge",
  "bracmat",
  "c",
  "ceylon",
  "clojure",
  "cobol",
  "coffeescript",
  "common_lisp",
  "cpp",
  "csharp",
  "d",
  "echolisp",
  "elena",
  "elixir",
  "erlang",
  "f_sharp_f",
  "factor",
  "falcon",
  "fortran",
  "freebasic",
  "gap",
  "go",
  "gosu",
  "groovy",
  "haskell",
  "hicest",
  "huginn",
  "icon_and_unicon",
  "j",
  "java",
  "javascript",
  "julia",
  "kotlin",
  "lasso",
  "liberty_basic",
  "livecode",
  "locomotive_basic",
  "logo",
  "lua",
  "maple",
  "mathematica",
  "matlab_octave",
  "miniscript",
  "mirc_scripting_language",
  "modula_2",
  "mumps",
  "nim",
  "nit",
  "objeck",
  "ocaml",
  "oforth",
  "oorexx",
  "openedge_progress",
  "pari_gp",
  "perl",
  "perl_6",
  "phix",
  "php",
  "picolisp",
  "pl_i",
  "potion",
  "powershell",
  "prodos",
  "prolog",
  "purebasic",
  "python",
  "r",
  "racket",
  "red",
  "reference",
  "related_tasks",
  "rexx",
  "ring",
  "ruby",
  "rust",
  "scala",
  "scheme",
  "sidef",
  "simula",
  "swift",
  "tcl",
  "torquescript",
  "tuscript",
  "unix_shell",
  "vba",
  "yabasic",
  "zkl",
  "zx_spectrum_basic",
]
+++

## Description


The [24 Game](https://en.wikipedia.org/wiki/24 Game) tests one's mental arithmetic.


## Task
Write a program that randomly chooses and displays four digits, each from 1 ──► 9 (inclusive) with repetitions allowed.

The program should prompt for the player to enter an arithmetic expression using *just* those, and *all* of those four digits, used exactly *once* each. The program should *check* then evaluate the expression.

The goal is for the player to enter an expression that (numerically) evaluates to **24**.
*  Only the following operators/functions are allowed: multiplication, division, addition, subtraction
*  Division should use floating point or rational arithmetic, etc, to preserve remainders.
*  Brackets are allowed, if using an infix expression evaluator.
*  Forming multiple digit numbers from the supplied digits is *disallowed*. (So an answer of 12+12 when given 1, 2, 2, and 1 is wrong).
*  The order of the digits when given does not have to be preserved.



## Notes
* The type of expression evaluator used is not mandated. An [RPN](https://en.wikipedia.org/wiki/Reverse Polish notation) evaluator is equally acceptable for example.
* The task is not for the program to generate the expression, or test whether an expression is even possible.


## Related tasks
* [24 game/Solve](/tasks/24 game/Solve)


## Reference
* [The 24 Game](http://www.bbc.co.uk/dna/h2g2/A933121) on h2g2.





## 8th

This is a fully-worked sample of the game in 8th, showing error-detection and user-restriction techniques:

```forth

\ Generate four random digits and display to the user
\ then get an expression from the user using +, -, / and * and the digits
\ the result must equal 24
\ http://8th-dev.com/24game.html

\ Only the words in namespace 'game' are available to the player:
ns: game

: + n:+ ;
: - n:- ;
: * n:* ;
: / n:/ ;

ns: G

var random-digits
var user-input

: one-digit \ a -- a
	rand n:abs 9 n:mod n:1+ a:push ;

: gen-digits \ - a
	[] clone nip \ the clone nip is not needed in versions past 1.0.2...
	' one-digit 4 times
	' n:cmp a:sort
	random-digits !  ;

: prompt-user
	cr "The digits are: " .  random-digits @ . cr ;

: goodbye
	cr "Thanks for playing!\n" . cr 0 die ;

: get-input
	70 null con:accept dup user-input !
	null? if drop goodbye then ;

: compare-digits
	true swap
	(
		\ inputed-array index
		dup >r
		a:@
		random-digits @ r> a:@ nip
		n:= not if
			break
			swap drop false swap
		then
	) 0 3 loop drop ;

/^\D*(\d)\D+(\d)\D+(\d)\D+(\d)\D*$/  var, digits-regex

: all-digits?
	user-input @ digits-regex @ r:match
	null? if drop false else
		5 = not if
			false
		else
			\ convert the captured digits in the regex into a sorted array:
			digits-regex @
			( r:@ >n swap ) 1 4 loop drop
			4 a:close ' n:cmp a:sort
			compare-digits
		then
	then ;

: does-eval?
	0 user-input @ eval 24 n:=
	dup not if
		cr "Sorry, that expression is wrong" . cr
	then ;

: check-input
	reset
	all-digits?  if
		does-eval? if
			cr "Excellent!  Your expression: \"" .
			user-input @ .
			"\" worked!" . cr
		then
	else
		cr "You did not use the digits properly, try again." . cr
	then ;

: intro quote |

Welcome to the '24 game'!

You will be shown four digits each time.  Using only the + - * and / operators
and all the digits (and only the digits), produce the number '24'

Enter your result in 8th syntax, e.g.:  4 4 + 2 1 + *

To quit the game, just hit enter by itself. Enjoy!

	| . ;

: start
	\ don't allow anything but the desired words
	ns:game only
	intro
	repeat
		gen-digits
		prompt-user
		get-input
		check-input
	again ;

start

```



## ABAP

See [24 game/ABAP](/tasks/24 game/ABAP)


## Ada

game24.adb:

```Ada
with Ada.Text_IO;
with Ada.Numerics.Discrete_Random;
procedure Game_24 is
   subtype Operation is Character;
   type Op_Array is array (Positive range <>) of Operation;
   type Digit is range 1 .. 9;
   type Digit_Array is array (Positive range <>) of Digit;
   package Digit_IO is new Ada.Text_IO.Integer_IO (Digit);
   package Random_Digit is new Ada.Numerics.Discrete_Random (Digit);
   Digit_Generator : Random_Digit.Generator;
   Given_Digits : array (1 .. 4) of Digit;
begin
   Ada.Text_IO.Put_Line ("24 Game");
   Ada.Text_IO.Put_Line ("Generating 4 digits...");
   Random_Digit.Reset (Digit_Generator);
   for I in Given_Digits'Range loop
      Given_Digits (I) := Random_Digit.Random (Digit_Generator);
   end loop;
   Ada.Text_IO.Put ("Your Digits:");
   for I in Given_Digits'Range loop
      Digit_IO.Put (Given_Digits (I));
   end loop;
   Ada.Text_IO.New_Line;
   Ada.Text_IO.Put ("Enter your Expression: ");
   declare
      Value : Integer;
      Input_Operations : Op_Array (1 .. 3);
      Input_Digits : Digit_Array (1 .. 4);
      Unused_Digits : array (Given_Digits'Range) of Boolean :=
        (others => True);
   begin
      -- get input
      for I in 1 .. 4 loop
         Digit_IO.Get (Input_Digits (I));
         exit when I = 4;
         Ada.Text_IO.Get (Input_Operations (I));
      end loop;
      -- check input
      for I in Input_Digits'Range loop
         declare
            Found : Boolean := False;
         begin
            for J in Given_Digits'Range loop
               if Unused_Digits (J) and then
                 Given_Digits (J) = Input_Digits (I) then
                  Unused_Digits (J) := False;
                  Found := True;
                  exit;
               end if;
            end loop;
            if not Found then
               Ada.Text_IO.Put_Line ("Illegal Number used:" &
                                     Digit'Image (Input_Digits (I)));
               return;
            end if;
         end;
      end loop;
      -- check value
      Value := Integer (Input_Digits (Input_Digits'First));
      for I in Input_Operations'Range loop
         case Input_Operations (I) is
            when '+' =>
               Value := Value + Integer (Input_Digits (I + 1));
            when '-' =>
               Value := Value - Integer (Input_Digits (I + 1));
            when '*' =>
               Value := Value * Integer (Input_Digits (I + 1));
            when '/' =>
               Value := Value / Integer (Input_Digits (I + 1));
            when others =>
               Ada.Text_IO.Put_Line ("Illegal Op used:" &
                                     Input_Operations (I));
               return;
         end case;
      end loop;
      if Value /= 24 then
         Ada.Text_IO.Put_Line ("Value" & Integer'Image (Value) &
                               " is not 24!");
      else
         Ada.Text_IO.Put_Line ("You won!");
      end if;
   end;
end Game_24;
```


### Output

```txt
24 Game
Generating 4 digits...
Your Digits: 4 9 5 5
Enter your Expression: 4*5+9-5
You won!
```


```txt
24 Game
Generating 4 digits...
Your Digits: 4 1 9 7
Enter your Expression: 4*9-7+1
Value 30 is not 24!
```


## APL

*Works with: Dyalog APL*

```APL
tfgame←{⎕IO←1
    ⎕←d←?⍵⍴9
    i←⍞
    u[⍋u←{⍎¨⍣(0≠≢⍵)⊢⍵}(i∊'1234567890')⊆i]≢d[⍋d]:'nope'
    ~∧/((~b←i∊'1234567890')/i)∊'+-×÷()':'nope'
    24≠⍎i:'nope'
    'Yeah!'
}
```

### Output

```txt
      tfgame 4
6 9 4 5
6+9+4+5
Yeah!

      tfgame 6
4 9 7 9 1 1
Ummm... I'm too tired.
nope

```



## Argile

*Works with: Argile 1.0.0*

```Argile
use std, array, list

do
  generate random digits
  show random digits
  let result = parse expression (get input line)
  if result != ERROR
    if some digits are unused
      print "Wrong ! (you didn't use all digits)" ; failure++
    else if result == 24.0
      print "Correct !" ; success++
    else
      print "Wrong ! (you got "result")" ; failure++
 while play again ?
print "success:"success" failure:"failure" total:"(success+failure) as int

let success = 0, failure = 0.

.: generate random digits :.
   our nat seed = 0xc6f31 (: default seed when /dev/urandom doesn't exist :)
   let urandom = fopen "/dev/urandom" "r"
   if  urandom isn't nil
     fread &seed size of seed 1 urandom
     fclose urandom
   Cfunc srandom seed
   seed = (Cfunc random) as nat
   for each (val int d) from 0 to 3
     digits[d] = '1' + (seed % 9)
     seed /= 9

let digits be an array of 4 byte

.: show random digits :.
   print "Enter an expression that equates to 24 using only all these digits:"
   printf "%c , %c , %c , %c\n"(digits[0])(digits[1])(digits[2])(digits[3])
   printf "24 = "

.: some digits are unused :. -> bool
   for each (val int d) from 0 to 3
     return true if digits[d] != '\0'
   false

.: get input line :. -> text
   our array of 64 byte line
   Cfunc fgets (line) (size of line) (stdin)
   let int i
   for (i = 0) (line[i] != 0) (i++)
     line[i] = '\0' if (line[i] == '\n')
   line as text

.: play again ? :. -> bool
   while true
     printf "Play again ? (y/n) " ; Cfunc fflush stdout
     let answer = get input line
     switch answer[0]
       case 'n' {return false}
       case 'y' {return true }
       default  {continue    }
   false

=: ERROR := -> real {-32202.0}

.: parse expression <text expr> :. -> real
   let x = 0.0, x_is_set = false, op = ' '.
   let stack be a list of State ; class State {byte op; real x}
   for (stack = nil) (*expr != 0) (expr++)
     switch *expr
       case '+' ; case '-' ; case '*' ; case '/'
         error "bad syntax" if not x_is_set
	 op = *expr
       case '1' ; case '2' ; case '3' ; case '4' ; case '5'
       case '6' ; case '7' ; case '8' ; case '9'
	 error "missing operator" if (x_is_set and op == ' ')
	 error "unavailable digit" unless consume digit expr[0]
	 do operation with (expr[0] - '0') as real
       case (Cgen "'('")
	 error "missing operator" if (op == ' ' but x_is_set)
	 (new list (new State) (code of del State())) << stack
	 op = ' ' ; x_is_set = false (: start fresh state :)
       case (Cgen "')'")
         error "mismatched parenthesis" if stack is nil
	 error "wrong syntax" if not x_is_set
	 let y = x
	 x = stack.data.x ; op = stack.data.op
	 delete pop stack
	 do operation with y
       default {error "disallowed character"}

       .:new State          :. -> State {let s=new(State); s.x=x; s.op=op; s}
       .:del State <State s>:.          {               free s              }
       .:do operation with <real y>:.
  	 switch op
	   case '+' {x += y}
	   case '-' {x -= y}
	   case '*' {x *= y}
	   case '/' {x /= y}
	   default  {x  = y; x_is_set = true}
         op = ' '
   =:error<text msg>:= ->real {eprint "Error: "msg" at ["expr"]";return ERROR}
   .:consume digit <byte b>:. -> bool
     for each (val int d) from 0 to 3
       if digits[d] == b
         digits[d] = '\0'
       	 return true
     false

   if stack isn't nil
     delete all stack
     error "unclosed parenthesis"
   return x

```

compile with:
arc 24_game.arg -o 24_game.c && gcc 24_game.c -o 24_game /usr/lib/libargrt.a


## AutoHotkey


```autohotkey
AutoExecute:
    Title := "24 Game"
    Gui, -MinimizeBox
    Gui, Add, Text, w230 vPuzzle
    Gui, Add, Edit, wp vAnswer
    Gui, Add, Button, w70, &Generate
    Gui, Add, Button, x+10 wp Default, &Submit
    Gui, Add, Button, x+10 wp, E&xit


ButtonGenerate: ; new set of numbers
    Loop, 4
        Random, r%A_Index%, 1, 9
    Puzzle = %r1%, %r2%, %r3%, and %r4%
    GuiControl,, Puzzle, The numbers are:  %Puzzle%  - Good luck!
    GuiControl,, Answer ; empty the edit box
    ControlFocus, Edit1
    Gui, -Disabled
    Gui, Show,, %Title%
Return ; end of auto execute section


ButtonSubmit: ; check solution
    Gui, Submit, NoHide
    Gui, +Disabled

    ; check numbers used
    RegExMatch(Answer, "(\d)\D+(\d)\D+(\d)\D+(\d)", $)
    ListPuzzle := r1 "," r2 "," r3 "," r4
    ListAnswer := $1 "," $2 "," $3 "," $4
    Sort, ListPuzzle, D,
    Sort, ListAnswer, D,
    If Not ListPuzzle = ListAnswer {
        MsgBox, 48, Error - %Title%, Numbers used!`n%Answer%
        Goto, TryAgain
    }

    ; check operators used
    StringReplace, $, $, +,, All
    StringReplace, $, $, -,, All
    StringReplace, $, $, *,, All
    StringReplace, $, $, /,, All
    StringReplace, $, $, (,, All
    StringReplace, $, $, ),, All
    Loop, 9
        StringReplace, $, $, %A_Index%,, All
    If StrLen($) > 0
    Or InStr(Answer, "**")
    Or InStr(Answer, "//")
    Or InStr(Answer, "++")
    Or InStr(Answer, "--") {
        MsgBox, 48, Error - %Title%, Operators used!`n%Answer%
        Goto, TryAgain
    }

    ; check result
    Result := Eval(Answer)
    If Not Result = 24 {
        MsgBox, 48, Error - %Title%, Result incorrect!`n%Result%
        Goto, TryAgain
    }

    ; if we are sill here
    MsgBox, 4, %Title%, Correct solution! Play again?
    IfMsgBox, Yes
        Gosub, ButtonGenerate
    Else
        ExitApp
Return


TryAgain: ; alternative ending of routine ButtonSubmit
    ControlFocus, Edit1
    Gui, -Disabled
    Gui, Show
Return


GuiClose:
GuiEscape:
ButtonExit:
    ExitApp
Return


;---------------------------------------------------------------------------
Eval(Expr) { ; evaluate expression using separate AHK process
;---------------------------------------------------------------------------
    ; credit for this function goes to AutoHotkey forum member Laszlo
    ; http://www.autohotkey.com/forum/topic9578.html
    ;-----------------------------------------------------------------------
    static File := "24$Temp.ahk"

    ; delete old temporary file, and write new
    FileDelete, %File%
    FileContent := "#NoTrayIcon`r`n"
                .  "FileDelete, " File "`r`n"
                .  "FileAppend, `% " Expr ", " File "`r`n"
    FileAppend, %FileContent%, %File%

    ; run AHK to execute temp script, evaluate expression
    RunWait, %A_AhkPath% %File%

    ; get result
    FileRead, Result, %File%
    FileDelete, %File%
    Return, Result
}
```



## AutoIt


```AutoIt

;AutoIt Script Example
;by Daniel Barnes
;spam me at djbarnes at orcon dot net dot en zed
;13/08/2012

;Choose four random digits (1-9) with repetitions allowed:
global $digits
FOR $i = 1 TO 4
	$digits &= Random(1,9,1)
NEXT

While 1
	main()
WEnd

Func main()
	$text  = "Enter an equation (using all of, and only, the single digits "&$digits &")"&@CRLF
	$text &= "which evaluates to exactly 24. Only multiplication (*) division (/)"&@CRLF
	$text &= "addition (+) and subtraction (-) operations and parentheses are allowed:"
	$input = InputBox ("24 Game",$text,"","",400,150)
	If @error Then exit

	;remove any spaces in input
	$input = StringReplace($input," ","")

	;check correct characters were used
	For $i = 1 To StringLen($input)
		$chr = StringMid($input,$i,1)
		If Not StringInStr("123456789*/+-()",$chr) Then
			MsgBox (0, "ERROR","Invalid character used: '"&$chr&"'")
			return
		endif
	Next

	;validate the equation uses all of the 4 characters, and nothing else
	$test = $input
	$test = StringReplace($test,"(","")
	$test = StringReplace($test,")","")

	;validate the length of the input - if its not 7 characters long then the user has done something wrong
	If StringLen ($test) <> 7 Then
		MsgBox (0,"ERROR","The equation "&$test&" is invalid")
		return
	endif

	$test = StringReplace($test,"/","")
	$test = StringReplace($test,"*","")
	$test = StringReplace($test,"-","")
	$test = StringReplace($test,"+","")

	For $i = 1 To StringLen($digits)
		$digit = StringMid($digits,$i,1)
		For $ii = 1 To StringLen($test)
			If  StringMid($test,$ii,1) = $digit Then
				$test = StringLeft($test,$ii-1) & StringRight($test,StringLen($test)-$ii)
				ExitLoop
			endif
		Next
	Next
	If $test <> "" Then
		MsgBox (0, "ERROR", "The equation didn't use all 4 characters, and nothing else!")
		return
	endif

	$try = Execute($input)

	If $try = 24 Then
		MsgBox (0, "24 Game","Well done. Your equation ("&$input&") = 24!")
		Exit
	Else
		MsgBox (0, "24 Game","Fail. Your equation ("&$input&") = "&$try&"!")
		return
	endif
EndFunc

```



## BBC BASIC


```bbcbasic
      REM Choose four random digits (1-9) with repetitions allowed:
      DIM digits%(4), check%(4)
      FOR choice% = 1 TO 4
        digits%(choice%) = RND(9)
      NEXT choice%

      REM Prompt the player:
      PRINT "Enter an equation (using all of, and only, the single digits ";
      FOR index% = 1 TO 4
        PRINT ; digits%(index%) ;
        IF index%<>4 PRINT " " ;
      NEXT
      PRINT ")"
      PRINT "which evaluates to exactly 24.  Only multiplication (*), division (/),"
      PRINT "addition (+) & subtraction (-) operations and parentheses are allowed:"
      INPUT "24 = " equation$

      REPEAT

        REM Check that the correct digits are used:
        check%() = 0
        FOR char% = 1 TO LEN(equation$)
          digit% = INSTR("0123456789", MID$(equation$, char%, 1)) - 1
          IF digit% >= 0 THEN
            FOR index% = 1 TO 4
              IF digit% = digits%(index%) THEN
                IF NOT check%(index%) check%(index%) = TRUE : EXIT FOR
              ENDIF
            NEXT index%
            IF index% > 4 THEN
              PRINT "Sorry, you used the illegal digit "; digit%
              EXIT REPEAT
            ENDIF
          ENDIF
        NEXT char%

        FOR index% = 1 TO 4
          IF NOT check%(index%) THEN
            PRINT "Sorry, you failed to use the digit " ; digits%(index%)
            EXIT REPEAT
          ENDIF
        NEXT index%

        REM Check that no pairs of digits are used:
        FOR pair% = 11 TO 99
          IF INSTR(equation$, STR$(pair%)) THEN
            PRINT "Sorry, you may not use a pair of digits "; pair%
            EXIT REPEAT
          ENDIF
        NEXT pair%

        REM Check whether the equation evaluates to 24:
        ON ERROR LOCAL PRINT "Sorry, there was an error in the equation" : EXIT REPEAT
        result = EVAL(equation$)
        RESTORE ERROR
        IF result = 24 THEN
          PRINT "Congratulations, you succeeded in the task!"
        ELSE
          PRINT "Sorry, your equation evaluated to " ; result " rather than 24!"
        ENDIF

      UNTIL TRUE

      INPUT '"Play again", answer$
      IF LEFT$(answer$,1) = "y" OR LEFT$(answer$,1) = "Y" THEN CLS : RUN
      QUIT
```



## Befunge


```befunge>v         > > >
 v
2           2                   1234
4         ^1?3^4
>8*00p10p> >?  ?5> 68*+00g10gpv
          v9?7v6              0
            8                 0
          > > >> ^            g
         ^p00  _v#   `\*49:+1 <
_>"rorrE",,,,,$ >~:67*-!#v_:167*+-!#v_:95*-!#v_:295*+-!#v_:586*+\`#v_:97*2--!#v
                         $          $        $          $          :          $
                         *          +        -          /          1          :
		^        <          <        <          <          8          .
                                                                   6          6
                                                                   *          4
                                                                   +          *
                                                                   \          -
                                                                   `    >    v_v
                                                                             "
 ^                                       <                         _v        e
		^                       _^#+*28:p2\*84\-*86g2:-+*441<        s
                                                                             o
                                                                             L
                                                  >    1                |-*49"#<
                                                  |   -*84gg01g00<p00*84<v   <
                                                  >00g:1+00p66*`#^_ "niW">:#,_@

```

The code functions by placing the 4 randomly generated numbers into the points labelled 1,2,3,4. In order to play, press the corresponding label to draw that number onto the stack, then press the corresponding operation (+,-,*,/) to perform it on the stack elements postfix-wise according to the rules of befunge (i.e. pop the values operate and push the answer back to the stack). When you wish to check your answer enter "=" and it will perform the checks to ensure that you haven't performed any illegal operations, that you have used all four numbers and that your final value is 24.

Unfortunately, due to the lack of floating-point arithmetic in befunge, divide will result in the answer truncated to an integer.

Example:
6566

```txt

213/-4*=

```

### Output

```txt

24 Win

```



## Bracmat



```Bracmat
  ( 24-game
  =     m-w m-z 4numbers answer expr numbers
      , seed get-random convertBinaryMinusToUnary
      , convertDivisionToMultiplication isExpresssion reciprocal
    .   (seed=.!arg:(~0:~/#?m-w.~0:~/#?m-z))
      & seed$!arg
      & ( get-random
        =
          .   36969*mod$(!m-z.65536)+div$(!m-z.65536):?m-z
            & 18000*mod$(!m-w.65536)+div$(!m-w.65536):?m-w
            & mod$(!m-z*65536+!m-w.9)+1
        )
      & ( convertBinaryMinusToUnary
        =   a z
          .     @(!arg:%?a "-" ?z)
              & str$(!a "+-1*" convertBinaryMinusToUnary$!z)
            | !arg
        )
      & (reciprocal=.!arg^-1)
      & ( convertDivisionToMultiplication
        =   a z
          .     @(!arg:?a "/" ?z)
              & str$(!a "*reciprocal$" convertDivisionToMultiplication$!z)
            | !arg
        )
      & ( isExpresssion
        =   A Z expr
          .   @( !arg
               :   ?A
                   ("+"|"-"|"*"|"/")
                   ( ?Z
                   & isExpresssion$!A
                   & isExpresssion$!Z
                   )
               )
            |   !numbers:?A !arg ?Z
              & !A !Z:?numbers
            |   ( @(!arg:"(" ?expr ")")
                | @(!arg:(" "|\t) ?expr)
                | @(!arg:?expr (" "|\t))
                )
              & isExpresssion$!expr
        )
      &   out
        $ "Enter an expression that evaluates to 24 by combining the following numbers."
      & out$"You may only use the operators + - * /"
      & out$"Parentheses and spaces are allowed."
      &   whl
        ' (   get-random$() get-random$() get-random$() get-random$
            : ?4numbers
          & out$!4numbers
          &   whl
            ' ( get'(,STR):?expr:~
              & !4numbers:?numbers
              & ~(isExpresssion$!expr&!numbers:)
              &   out
                $ ( str
                  $ ( "["
                      !expr
                      "] is not a valid expression. Try another expression."
                    )
                  )
              )
          & !expr:~
          & convertBinaryMinusToUnary$!expr:?expr
          & convertDivisionToMultiplication$!expr:?expr
          & get$(!expr,MEM):?answer
          & out$(str$(!expr " = " !answer))
          &   !answer
            : ( 24&out$Right!
              | #&out$Wrong!
              )
          & out$"Try another one:"
          )
      & out$bye
  )
& 24-game$(13.14)
& ;
```


```txt
Enter an expression that evaluates to 24 by combining the following numbers.
You may only use the operators + - * /
Parentheses and spaces are allowed.
4 2 2 7
4*7 - 2-2
4*7 +-1* 2+-1*2 = 24
Right!
Try another one:
4 7 9 8
((4) *(8 - (9- 7))
[((4) *(8 - (9- 7))] is not a valid expression. Try another expression.
((4) *(8 - (9- 7)))
((4) *(8 +-1* (9+-1* 7))) = 24
Right!
Try another one:
9 5 8 5
5 * 5 - (9 - 8)
5 * 5 +-1* (9 +-1* 8) = 24
Right!
Try another one:
5 9 7 8
5*8 - 9 - 7
5*8 +-1* 9 +-1* 7 = 24
Right!
Try another one:
7 8 6 2
8 * ((7 - 6) + 2)
8 * ((7 +-1* 6) + 2) = 24
Right!
Try another one:
8 6 8 1
8 * (1 + 8 - 6)
8 * (1 + 8 +-1* 6) = 24
Right!
Try another one:
8 2 2 4
8 * (2 + 4)/2
8 * (2 + 4)*reciprocal$2 = 24
Right!
Try another one:
8 4 6 7

bye
```



## C

Simple recursive descent parser.  It doesn't have a real lexer, because all tokens are single character (digits, operators and parens).  Code is a little too long.

```c
#include <stdio.h>
#include <ctype.h>
#include <stdlib.h>
#include <setjmp.h>
#include <time.h>

jmp_buf ctx;
const char *msg;

enum { OP_NONE = 0, OP_NUM, OP_ADD, OP_SUB, OP_MUL, OP_DIV };

typedef struct expr_t *expr, expr_t;
struct expr_t {
	int op, val, used;
	expr left, right;
};

#define N_DIGITS 4
expr_t digits[N_DIGITS];

void gen_digits()
{
	int i;
	for (i = 0; i < N_DIGITS; i++)
		digits[i].val = 1 + rand() % 9;
}

#define MAX_INPUT 64
char str[MAX_INPUT];
int pos;

#define POOL_SIZE 8
expr_t pool[POOL_SIZE];
int pool_ptr;

void reset()
{
	int i;
	msg = 0;
	pool_ptr = pos = 0;
	for (i = 0; i < POOL_SIZE; i++) {
		pool[i].op = OP_NONE;
		pool[i].left = pool[i].right = 0;
	}
	for (i = 0; i < N_DIGITS; i++)
		digits[i].used = 0;
}

/* longish jumpish back to input cycle */
void bail(const char *s)
{
	msg = s;
	longjmp(ctx, 1);
}

expr new_expr()
{
	if (pool_ptr < POOL_SIZE)
		return pool + pool_ptr++;
	return 0;
}

/* check next input char */
int next_tok()
{
	while (isspace(str[pos])) pos++;
	return str[pos];
}

/* move input pointer forward */
int take()
{
	if (str[pos] != '\0') return ++pos;
	return 0;
}

/* BNF(ish)
expr = term { ("+")|("-") term }
term = fact { ("*")|("/") expr }
fact =	number
	| '(' expr ')'
*/

expr get_fact();
expr get_term();
expr get_expr();

expr get_expr()
{
	int c;
	expr l, r, ret;
	if (!(ret = get_term())) bail("Expected term");
	while ((c = next_tok()) == '+' || c == '-') {
		if (!take()) bail("Unexpected end of input");
		if (!(r = get_term())) bail("Expected term");

		l = ret;
		ret = new_expr();
		ret->op = (c == '+') ? OP_ADD : OP_SUB;
		ret->left = l;
		ret->right = r;
	}
	return ret;
}

expr get_term()
{
	int c;
	expr l, r, ret;
	ret = get_fact();
	while((c = next_tok()) == '*' || c == '/') {
		if (!take()) bail("Unexpected end of input");

		r = get_fact();
		l = ret;
		ret = new_expr();
		ret->op = (c == '*') ? OP_MUL : OP_DIV;
		ret->left = l;
		ret->right = r;
	}
	return ret;
}

expr get_digit()
{
	int i, c = next_tok();
	expr ret;
	if (c >= '0' && c <= '9') {
		take();
		ret = new_expr();
		ret->op = OP_NUM;
		ret->val = c - '0';
		for (i = 0; i < N_DIGITS; i++)
			if (digits[i].val == ret->val && !digits[i].used) {
				digits[i].used = 1;
				return ret;
			}
		bail("Invalid digit");
	}
	return 0;
}

expr get_fact()
{
	int c;
	expr l = get_digit();
	if (l) return l;
	if ((c = next_tok()) == '(') {
		take();
		l = get_expr();
		if (next_tok() != ')') bail("Unbalanced parens");
		take();
		return l;
	}
	return 0;
}

expr parse()
{
	int i;
	expr ret = get_expr();
	if (next_tok() != '\0')
		bail("Trailing garbage");
	for (i = 0; i < N_DIGITS; i++)
		if (!digits[i].used)
			bail("Not all digits are used");
	return ret;
}

typedef struct frac_t frac_t, *frac;
struct frac_t { int denom, num; };

int gcd(int m, int n)
{
	int t;
	while (m) {
		t = m; m = n % m; n = t;
	}
	return n;
}

/* evaluate expression tree.  result in fraction form */
void eval_tree(expr e, frac res)
{
	frac_t l, r;
	int t;
	if (e->op == OP_NUM) {
		res->num = e->val;
		res->denom = 1;
		return;
	}

	eval_tree(e->left, &l);
	eval_tree(e->right, &r);

	switch(e->op) {
	case OP_ADD:
		res->num = l.num * r.denom + l.denom * r.num;
		res->denom = l.denom * r.denom;
		break;
	case OP_SUB:
		res->num = l.num * r.denom - l.denom * r.num;
		res->denom = l.denom * r.denom;
		break;
	case OP_MUL:
		res->num = l.num * r.num;
		res->denom = l.denom * r.denom;
		break;
	case OP_DIV:
		res->num = l.num * r.denom;
		res->denom = l.denom * r.num;
		break;
	}
	if ((t = gcd(res->denom, res->num))) {
		res->denom /= t;
		res->num /= t;
	}
}

void get_input()
{
	int i;
reinput:
	reset();
	printf("\nAvailable digits are:");
	for (i = 0; i < N_DIGITS; i++)
		printf(" %d", digits[i].val);
	printf(". Type an expression and I'll check it for you, or make new numbers.\n"
		"Your choice? [Expr/n/q] ");

	while (1) {
		for (i = 0; i < MAX_INPUT; i++) str[i] = '\n';
		fgets(str, MAX_INPUT, stdin);
		if (*str == '\0') goto reinput;
		if (str[MAX_INPUT - 1] != '\n')
			bail("string too long");

		for (i = 0; i < MAX_INPUT; i++)
			if (str[i] == '\n') str[i] = '\0';
		if (str[0] == 'q') {
			printf("Bye\n");
			exit(0);
		}
		if (str[0] == 'n') {
			gen_digits();
			goto reinput;
		}
		return;
	}
}

int main()
{
	frac_t f;
	srand(time(0));

	gen_digits();
	while(1) {
		get_input();
		setjmp(ctx); /* if parse error, jump back here with err msg set */
		if (msg) {
			/* after error jump; announce, reset, redo */
			printf("%s at '%.*s'\n", msg, pos, str);
			continue;
		}

		eval_tree(parse(), &f);

		if (f.denom == 0) bail("Divide by zero");
		if (f.denom == 1 && f.num == 24)
			printf("You got 24.  Very good.\n");
		else {
			if (f.denom == 1)
				printf("Eval to: %d, ", f.num);
			else
				printf("Eval to: %d/%d, ", f.num, f.denom);
			printf("no good.  Try again.\n");
		}
	}
	return 0;
}
```

### Output

```txt
Available digits are: 5 2 3 9. Type an expression and I'll check it for you, or make new numbers.
Your choice? [Expr/n/q] 5*2*3/9
Eval to: 10/3, no good.  Try again.

Available digits are: 5 2 3 9. Type an expression and I'll check it for you, or make new numbers.
Your choice? [Expr/n/q] (5*(2+3)-9
Unbalanced parens at '(5*(2+3)-9'

Available digits are: 5 2 3 9. Type an expression and I'll check it for you, or make new numbers.
Your choice? [Expr/n/q] 3*9-(5-2)
You got 24.  Very good.

Available digits are: 5 2 3 9. Type an expression and I'll check it for you, or make new numbers.
Your choice? [Expr/n/q] n

Available digits are: 4 4 4 7. Type an expression and I'll check it for you, or make new numbers.
Your choice? [Expr/n/q] q
Bye
```

See [24 game/C](/tasks/24 game/C)


## C#

See [24 game/CSharp](/tasks/24 game/CSharp)


## C++

*Works with: C++11*

This uses the C++11 standard to simplify several parts of the code. Input is given in RPN format.


```cpp
#include <random>
#include <iostream>
#include <stack>
#include <set>
#include <string>
#include <functional>
using namespace std;

class RPNParse
{
public:
  stack<double> stk;
  multiset<int> digits;

  void op(function<double(double,double)> f)
  {
    if(stk.size() < 2)
      throw "Improperly written expression";
    int b = stk.top(); stk.pop();
    int a = stk.top(); stk.pop();
    stk.push(f(a, b));
  }

  void parse(char c)
  {
    if(c >= '0' && c <= '9')
    {
      stk.push(c - '0');
      digits.insert(c - '0');
    }
    else if(c == '+')
      op([](double a, double b) {return a+b;});
    else if(c == '-')
      op([](double a, double b) {return a-b;});
    else if(c == '*')
      op([](double a, double b) {return a*b;});
    else if(c == '/')
      op([](double a, double b) {return a/b;});
  }

  void parse(string s)
  {
    for(int i = 0; i < s.size(); ++i)
      parse(s[i]);
  }

  double getResult()
  {
    if(stk.size() != 1)
      throw "Improperly written expression";
    return stk.top();
  }
};

int main()
{
  random_device seed;
  mt19937 engine(seed());
  uniform_int_distribution<> distribution(1, 9);
  auto rnd = bind(distribution, engine);

  multiset<int> digits;
  cout << "Make 24 with the digits: ";
  for(int i = 0; i < 4; ++i)
  {
    int n = rnd();
    cout << " " << n;
    digits.insert(n);
  }
  cout << endl;

  RPNParse parser;

  try
  {
    string input;
    getline(cin, input);
    parser.parse(input);

    if(digits != parser.digits)
      cout << "Error: Not using the given digits" << endl;
    else
    {
      double r = parser.getResult();
      cout << "Result: " << r << endl;

      if(r > 23.999 && r < 24.001)
        cout << "Good job!" << endl;
      else
        cout << "Try again." << endl;
    }
  }
  catch(char* e)
  {
    cout << "Error: " << e << endl;
  }
  return 0;
}
```


### Output

```txt

Make 24 with the digits:  1 4 9 9
9 9 + 4 * 1 +
Result: 73
Try again.

Make 24 with the digits:  3 9 9 2
9 9 + 3 2 * +
Result: 24
Good job!

```



## Ceylon

Be sure to import ceylon.random in you ceylon.module file.

```ceylon
import ceylon.random {
	DefaultRandom
}

class Rational(shared Integer numerator, shared Integer denominator = 1) satisfies Numeric<Rational> {

	assert (denominator != 0);

	Integer gcd(Integer a, Integer b) => if (b == 0) then a else gcd(b, a % b);

	shared Rational inverted => Rational(denominator, numerator);

	shared Rational simplified =>
		let (largestFactor = gcd(numerator, denominator))
			Rational(numerator / largestFactor, denominator / largestFactor);

	divided(Rational other) => (this * other.inverted).simplified;

	negated => Rational(-numerator, denominator).simplified;

	plus(Rational other) =>
			let (top = numerator*other.denominator + other.numerator*denominator,
				bottom = denominator * other.denominator)
			Rational(top, bottom).simplified;

	times(Rational other) =>
		Rational(numerator * other.numerator, denominator * other.denominator).simplified;

	shared Integer integer => numerator / denominator;
	shared Float float => numerator.float / denominator.float;

	string => denominator == 1 then numerator.string else "``numerator``/``denominator``";

	shared actual Boolean equals(Object that) {
		if (is Rational that) {
			value simplifiedThis = this.simplified;
			value simplifiedThat = that.simplified;
			return simplifiedThis.numerator==simplifiedThat.numerator &&
					simplifiedThis.denominator==simplifiedThat.denominator;
		}
		else {
			return false;
		}
	}
}

interface Expression {
	shared formal Rational evaluate();
}

class NumberExpression(Rational number) satisfies Expression {
	evaluate() => number;
	string => number.string;
}

class OperatorExpression(Expression left, Character operator, Expression right) satisfies Expression {
	shared actual Rational evaluate() {
		switch (operator)
		case ('*') {
			return left.evaluate() * right.evaluate();
		}
		case ('/') {
			return left.evaluate() / right.evaluate();
		}
		case ('-') {
			return left.evaluate() - right.evaluate();
		}
		case ('+') {
			return left.evaluate() + right.evaluate();
		}
		else {
			throw Exception("unknown operator ``operator``");
		}
	}

	string => "(``left.string`` ``operator.string`` ``right.string``)";
}

"A simplified top down operator precedence parser. There aren't any right
 binding operators so we don't have to worry about that."
class PrattParser(String input) {

	value tokens = input.replace(" ", "");
	variable value index = -1;

	shared Expression expression(Integer precedence = 0) {
		value token = advance();
		variable value left = parseUnary(token);
		while (precedence < getPrecedence(peek())) {
			value nextToken = advance();
			left = parseBinary(left, nextToken);
		}
		return left;
	}

	Integer getPrecedence(Character op) =>
		switch (op)
			case ('*' | '/') 2
			case ('+' | '-') 1
			else 0;

	Character advance(Character? expected = null) {
		index++;
		value token = tokens[index] else ' ';
		if (exists expected, token != expected) {
			throw Exception("unknown character ``token``");
		}
		return token;
	}

	Character peek() => tokens[index + 1] else ' ';

	Expression parseBinary(Expression left, Character operator) =>
		let (right = expression(getPrecedence(operator)))
			OperatorExpression(left, operator, right);

	Expression parseUnary(Character token) {
		if (token.digit) {
			assert (is Integer int = Integer.parse(token.string));
			return NumberExpression(Rational(int));
		}
		else if (token == '(') {
			value exp = expression();
			advance(')');
			return exp;
		}
		else {
			throw Exception("unknown character ``token``");
		}
	}
}

shared void run() {

	value random = DefaultRandom();

	function random4Numbers() =>
		random.elements(1..9).take(4).sequence();

	function isValidGuess(String input, {Integer*} allowedNumbers) {
		value allowedOperators = set { *"()+-/*" };
		value extractedNumbers = input
			.split((Character ch) => ch in allowedOperators || ch.whitespace)
			.map((String element) => Integer.parse(element))
			.narrow<Integer>();
		if (extractedNumbers.any((Integer element) => element > 9)) {
			print("number too big!");
			return false;
		}
		if (extractedNumbers.any((Integer element) => element < 1)) {
			print("number too small!");
			return false;
		}
		if (extractedNumbers.sort(increasing) != allowedNumbers.sort(increasing)) {
			print("use all the numbers, please!");
			return false;
		}
		if (!input.every((Character element) => element in allowedOperators || element.digit || element.whitespace)) {
			print("only digits and mathematical operators, please");
			return false;
		}
		variable value leftParens = 0;
		for (c in input) {
			if (c == '(') {
				leftParens++;
			} else if (c == ')') {
				leftParens--;
				if (leftParens < 0) {
					break;
				}
			}
		}
		if (leftParens != 0) {
			print("unbalanced brackets!");
			return false;
		}
		return true;
	}

	function evaluate(String input) =>
		let (parser = PrattParser(input),
			exp = parser.expression())
			exp.evaluate();

	print("Welcome to The 24 Game.
	          Create a mathematical equation with four random
	          numbers that evaluates to 24.
	          You must use all the numbers once and only once,
	          but in any order.
	          Also, only + - / * and parentheses are allowed.
	          For example: (1 + 2 + 3) * 4
	          Also: enter n for new numbers and q to quit.
	          -----------------------------------------------");

	value twentyfour = Rational(24);

	while (true) {

		value chosenNumbers = random4Numbers();
		void pleaseTryAgain() => print("Sorry, please try again. (Your numbers are ``chosenNumbers``)");

		print("Your numbers are ``chosenNumbers``. Please turn them into 24.");

		while (true) {
			value line = process.readLine()?.trimmed;
			if (exists line) {
				if (line.uppercased == "Q") { // quit
					print("bye!");
					return;
				}
				if (line.uppercased == "N") { // new game
					break;
				}
				if (isValidGuess(line, chosenNumbers)) {
					try {
						value result = evaluate(line);
						print("= ``result``");
						if (result == twentyfour) {
							print("You did it!");
							break;
						}
						else {
							pleaseTryAgain();
						}
					}
					catch (Exception e) {
						print(e.message);
						pleaseTryAgain();
					}
				}
				else {
					pleaseTryAgain();
				}
			}
		}
	}
}
```



## Clojure


```Clojure

(ns rosettacode.24game)

(def ^:dynamic *luser*
"You guessed wrong, or your input was not in prefix notation.")

(def ^:private start #(println
"Your numbers are: " %1 ". Your goal is " %2 ".\n"
"Use the ops [+ - * /] in prefix notation to reach" %2 ".\n"
"q[enter] to quit."))

(defn play
  ([] (play 24))
  ([goal] (play goal (repeatedly 4 #(inc (rand-int 9)))))
  ([goal gns]
     (start gns goal)
     (let [input (read-string (read-line))
           flat  (flatten input)]
      (println
        (if (and (re-find #"^\([\d\s+*/-]+\d?\)$" (pr-str flat))
                 (= (set gns) (set (filter integer? flat)))
                 (= goal (eval input)))
         "You won the game!"
         *luser*))
      (when (not= input 'q) (recur goal gns)))))

; * checks prefix form, then checks to see that the numbers used
; and the numbers generated by the game are the same.

```



## COBOL


```COBOL>        >
SOURCE FORMAT FREE
*> This code is dedicated to the public domain
*> This is GNUCobol 2.0
identification division.
program-id. twentyfour.
environment division.
configuration section.
repository. function all intrinsic.
data division.
working-storage section.
01  p pic 999.
01  p1 pic 999.
01  p-max pic 999 value 38.
01  program-syntax pic x(494) value
*>statement = expression;
        '001 001 000 n'
    &   '002 000 004 ='
    &   '003 005 000 n'
    &   '004 000 002 ;'
*>expression = term, {('+'|'-') term,};
    &   '005 005 000 n'
    &   '006 000 016 ='
    &   '007 017 000 n'
    &   '008 000 015 {'
    &   '009 011 013 ('
    &   '010 001 000 t'
    &   '011 013 000 |'
    &   '012 002 000 t'
    &   '013 000 009 )'
    &   '014 017 000 n'
    &   '015 000 008 }'
    &   '016 000 006 ;'
*>term = factor, {('*'|'/') factor,};
    &   '017 017 000 n'
    &   '018 000 028 ='
    &   '019 029 000 n'
    &   '020 000 027 {'
    &   '021 023 025 ('
    &   '022 003 000 t'
    &   '023 025 000 |'
    &   '024 004 000 t'
    &   '025 000 021 )'
    &   '026 029 000 n'
    &   '027 000 020 }'
    &   '028 000 018 ;'
*>factor = ('(' expression, ')' | digit,);
    &   '029 029 000 n'
    &   '030 000 038 ='
    &   '031 035 037 ('
    &   '032 005 000 t'
    &   '033 005 000 n'
    &   '034 006 000 t'
    &   '035 037 000 |'
    &   '036 000 000 n'
    &   '037 000 031 )'
    &   '038 000 030 ;'.
01  filler redefines program-syntax.
    03  p-entry occurs 038.
        05  p-address pic 999.
        05  filler pic x.
        05  p-definition pic 999.
        05  p-alternate redefines p-definition pic 999.
        05  filler pic x.
        05  p-matching pic 999.
        05  filler pic x.
        05  p-symbol pic x.

01  t pic 999.
01  t-len pic 99 value 6.
01  terminal-symbols
    pic x(210) value
        '01 +                               '
    &   '01 -                               '
    &   '01 *                               '
    &   '01 /                               '
    &   '01 (                               '
    &   '01 )                               '.
01  filler redefines terminal-symbols.
    03  terminal-symbol-entry occurs 6.
        05  terminal-symbol-len pic 99.
        05  filler pic x.
        05  terminal-symbol pic x(32).

01  nt pic 999.
01  nt-lim pic 99 value 5.
01  nonterminal-statements pic x(294) value
        "000 ....,....,....,....,....,....,....,....,....,"
    &   "001 statement = expression;                      "
    &   "005 expression = term, {('+'|'-') term,};        "
    &   "017 term = factor, {('*'|'/') factor,};          "
    &   "029 factor = ('(' expression, ')' | digit,);     "
    &   "036 digit;                                       ".
01  filler redefines nonterminal-statements.
    03  nonterminal-statement-entry occurs 5.
        05  nonterminal-statement-number pic 999.
        05  filler pic x.
        05  nonterminal-statement pic x(45).

01  indent pic x(64) value all '|  '.
01  interpreter-stack.
    03  r pic 99. *> previous top of stack
    03  s pic 99. *> current top of stack
    03  s-max pic 99 value 32.
    03  s-entry occurs 32.
        05  filler pic x(2) value 'p='.
        05  s-p pic 999. *> callers return address
        05  filler pic x(4) value ' sc='.
        05  s-start-control pic 999. *> sequence start address
        05  filler pic x(4) value ' ec='.
        05  s-end-control pic 999. *> sequence end address
        05  filler pic x(4) value ' al='.
        05  s-alternate pic 999. *> the next alternate
        05  filler pic x(3) value ' r='.
        05  s-result pic x. *> S success, F failure, N no result
        05  filler pic x(3) value ' c='.
        05  s-count pic 99. *> successes in a sequence
        05  filler pic x(3) value ' x='.
        05  s-repeat pic 99. *> repeats in a {} sequence
        05  filler pic x(4) value ' nt='.
        05  s-nt pic 99. *> current nonterminal

01  language-area.
    03  l pic 99.
    03  l-lim pic 99.
    03  l-len pic 99 value 1.
    03  nd pic 9.
    03  number-definitions.
        05  n occurs 4 pic 9.
    03  nu pic 9.
    03  number-use.
        05  u occurs 4 pic x.
    03  statement.
        05  c occurs 32.
            07  c9 pic 9.

01  number-validation.
    03  p4 pic 99.
    03  p4-lim pic 99 value 24.
    03  permutations-4 pic x(96) value
          '1234'
        & '1243'
        & '1324'
        & '1342'
        & '1423'
        & '1432'
        & '2134'
        & '2143'
        & '2314'
        & '2341'
        & '2413'
        & '2431'
        & '3124'
        & '3142'
        & '3214'
        & '3241'
        & '3423'
        & '3432'
        & '4123'
        & '4132'
        & '4213'
        & '4231'
        & '4312'
        & '4321'.
     03  filler redefines permutations-4.
         05  permutation-4 occurs 24 pic x(4).
     03  current-permutation-4 pic x(4).
     03  cpx pic 9.
     03  od1 pic 9.
     03  od2 pic 9.
     03  odx pic 9.
     03  od-lim pic 9 value 4.
     03  operator-definitions pic x(4) value '+-*/'.
     03  current-operators pic x(3).
     03  co3 pic 9.
     03  rpx pic 9.
     03  rpx-lim pic 9 value 4.
     03  valid-rpn-forms pic x(28) value
          'nnonono'
        & 'nnnonoo'
        & 'nnnoono'
        & 'nnnnooo'.
    03  filler redefines valid-rpn-forms.
        05  rpn-form occurs 4 pic x(7).
    03  current-rpn-form pic x(7).

01  calculation-area.
    03  osx pic 99.
    03  operator-stack pic x(32).
    03  oqx pic 99.
    03  oqx1 pic 99.
    03  output-queue pic x(32).
    03  work-number pic s9999.
    03  top-numerator pic s9999 sign leading separate.
    03  top-denominator pic s9999 sign leading separate.
    03  rsx pic 9.
    03  result-stack occurs 8.
        05  numerator pic s9999.
        05  denominator pic s9999.

01  error-found pic x.
01  divide-by-zero-error pic x.

*>  diagnostics
01  NL pic x value x'0A'.
01  NL-flag pic x value space.
01  display-level pic x value '0'.
01  loop-lim pic 9999 value 1500.
01  loop-count pic 9999 value 0.
01  message-area value spaces.
    03  message-level pic x.
    03  message-value pic x(128).

*>  input and examples
01  instruction pic x(32) value spaces.
01  tsx pic 99.
01  tsx-lim pic 99 value 14.
01  test-statements.
    03  filler pic x(32) value '1234;1 + 2 + 3 + 4'.
    03  filler pic x(32) value '1234;1 * 2 * 3 * 4'.
    03  filler pic x(32) value '1234;((1)) * (((2 * 3))) * 4'.
    03  filler pic x(32) value '1234;((1)) * ((2 * 3))) * 4'.
    03  filler pic x(32) value '1234;(1 + 2 + 3 + 4'.
    03  filler pic x(32) value '1234;)1 + 2 + 3 + 4'.
    03  filler pic x(32) value '1234;1 * * 2 * 3 * 4'.
    03  filler pic x(32) value '5679;6 - (5 - 7) * 9'.
    03  filler pic x(32) value '1268;((1 * (8 * 6) / 2))'.
    03  filler pic x(32) value '4583;-5-3+(8*4)'.
    03  filler pic x(32) value '4583;8 * 4 - 5 - 3'.
    03  filler pic x(32) value '4583;8 * 4 - (5 + 3)'.
    03  filler pic x(32) value '1223;1 * 3 / (2 - 2)'.
    03  filler pic x(32) value '2468;(6 * 8) / 4 / 2'.
01  filler redefines test-statements.
    03  filler occurs 14.
        05  test-numbers pic x(4).
        05  filler pic x.
        05  test-statement pic x(27).

procedure division.
start-twentyfour.
    display 'start twentyfour'
    perform generate-numbers
    display 'type h <enter> to see instructions'
    accept instruction
    perform until instruction = spaces or 'q'
        evaluate true
        when instruction = 'h'
            perform display-instructions
        when instruction = 'n'
            perform generate-numbers
        when instruction(1:1) = 'm'
            move instruction(2:4) to number-definitions
            perform validate-number
            if divide-by-zero-error = space
            and 24 * top-denominator = top-numerator
                display number-definitions ' is solved by ' output-queue(1:oqx)
            else
                display number-definitions ' is not solvable'
            end-if
        when instruction = 'd0' or 'd1' or 'd2' or 'd3'
            move instruction(2:1) to display-level
        when instruction = 'e'
            display 'examples:'
            perform varying tsx from 1 by 1
            until tsx > tsx-lim
                move spaces to statement
                move test-numbers(tsx) to number-definitions
                move test-statement(tsx) to statement
                perform evaluate-statement
                perform show-result
            end-perform
        when other
            move instruction to statement
            perform evaluate-statement
            perform show-result
        end-evaluate
        move spaces to instruction
        display 'instruction? ' with no advancing
        accept instruction
    end-perform

    display 'exit twentyfour'
    stop run
    .
generate-numbers.
    perform with test after until divide-by-zero-error = space
    and 24 * top-denominator = top-numerator
        compute n(1) = random(seconds-past-midnight) * 10 *> seed
        perform varying nd from 1 by 1 until nd > 4
            compute n(nd) = random() * 10
            perform until n(nd) <> 0
                compute n(nd) = random() * 10
            end-perform
        end-perform
        perform validate-number
    end-perform
    display NL 'numbers:' with no advancing
    perform varying nd from 1 by 1 until nd > 4
        display space n(nd) with no advancing
    end-perform
    display space
    .
validate-number.
    perform varying p4 from 1 by 1 until p4 > p4-lim
        move permutation-4(p4) to current-permutation-4
        perform varying od1 from 1 by 1 until od1 > od-lim
            move operator-definitions(od1:1) to current-operators(1:1)
            perform varying od2 from 1 by 1 until od2 > od-lim
                move operator-definitions(od2:1) to current-operators(2:1)
                perform varying odx from 1 by 1 until odx > od-lim
                    move operator-definitions(odx:1) to current-operators(3:1)
                    perform varying rpx from 1 by 1 until rpx > rpx-lim
                        move rpn-form(rpx) to current-rpn-form
                        move 0 to cpx co3
                        move spaces to output-queue
                        move 7 to oqx
                        perform varying oqx1 from 1 by 1 until oqx1 > oqx
                            if current-rpn-form(oqx1:1) = 'n'
                                add 1 to cpx
                                move current-permutation-4(cpx:1) to nd
                                move n(nd) to output-queue(oqx1:1)
                            else
                                add 1 to co3
                                move current-operators(co3:1) to output-queue(oqx1:1)
                            end-if
                        end-perform
                    end-perform
                    perform evaluate-rpn
                    if divide-by-zero-error = space
                    and 24 * top-denominator = top-numerator
                        exit paragraph
                    end-if
                end-perform
            end-perform
        end-perform
    end-perform
    .
display-instructions.
    display '1)  Type h <enter> to repeat these instructions.'
    display '2)  The program will display four randomly-generated'
    display '    single-digit numbers and will then prompt you to enter'
    display '    an arithmetic expression followed by <enter> to sum'
    display '    the given numbers to 24.'
    display '    The four numbers may contain duplicates and the entered'
    display '    expression must reference all the generated numbers and duplicates.'
    display '    Warning:  the program converts the entered infix expression'
    display '    to a reverse polish notation (rpn) expression'
    display '    which is then interpreted from RIGHT to LEFT.'
    display '    So, for instance, 8*4 - 5 - 3 will not sum to 24.'
    display '3)  Type n <enter> to generate a new set of four numbers.'
    display '    The program will ensure the generated numbers are solvable.'
    display '4)  Type m#### <enter> (e.g. m1234) to create a fixed set of numbers'
    display '    for testing purposes.'
    display '    The program will test the solvability of the entered numbers.'
    display '    For example, m1234 is solvable and m9999 is not solvable.'
    display '5)  Type d0, d1, d2 or d3 followed by <enter> to display none or'
    display '    increasingly detailed diagnostic information as the program evaluates'
    display '    the entered expression.'
    display '6)  Type e <enter> to see a list of example expressions and results'
    display '7)  Type <enter> or q <enter> to exit the program'
    .
show-result.
    if error-found = 'y'
    or divide-by-zero-error = 'y'
        exit paragraph
    end-if
    display 'statement in RPN is' space output-queue
    evaluate true
    when top-numerator = 0
    when top-denominator = 0
    when 24 * top-denominator <> top-numerator
        display 'result (' top-numerator '/' top-denominator ') is not 24'
    when other
        display 'result is 24'
    end-evaluate
    .
evaluate-statement.
    compute l-lim = length(trim(statement))

    display NL 'numbers:' space n(1) space n(2) space n(3) space n(4)
    move number-definitions to number-use
    display 'statement is' space statement

    move 1 to l
    move 0 to loop-count
    move space to error-found

    move 0 to osx oqx
    move spaces to output-queue

    move 1 to p
    move 1 to nt
    move 0 to s
    perform increment-s
    perform display-start-nonterminal
    perform increment-p

    *>
### =============================

    *> interpret ebnf
    *>
### =============================

    perform until s = 0
    or error-found = 'y'

        evaluate true

        when p-symbol(p) = 'n'
        and p-definition(p) = 000 *> a variable
           perform test-variable
       if s-result(s) = 'S'
               perform increment-l
           end-if
           perform increment-p

       when p-symbol(p) = 'n'
       and p-address(p) <> p-definition(p) *> nonterminal reference
           move p to s-p(s)
           move p-definition(p) to p

       when p-symbol(p) = 'n'
       and p-address(p) = p-definition(p) *> nonterminal definition
           perform increment-s
           perform display-start-nonterminal
           perform increment-p

        when p-symbol(p) = '=' *> nonterminal control
            move p to s-start-control(s)
            move p-matching(p) to s-end-control(s)
            perform increment-p

        when p-symbol(p) = ';' *> end nonterminal
            perform display-end-control
            perform display-end-nonterminal
            perform decrement-s
            if s > 0
                evaluate true
                when s-result(r) = 'S'
                    perform set-success
                when s-result(r) = 'F'
                    perform set-failure
                end-evaluate
                move s-p(s) to p
                perform increment-p
                perform display-continue-nonterminal
            end-if

    when p-symbol(p) = '{' *> start repeat sequence
            perform increment-s
            perform display-start-control
            move p to s-start-control(s)
            move p-alternate(p) to s-alternate(s)
            move p-matching(p) to s-end-control(s)
            move 0 to s-count(s)
            perform increment-p

        when p-symbol(p) = '}' *> end repeat sequence
            perform display-end-control
            evaluate true
            when s-result(s) = 'S' *> repeat the sequence
                perform display-repeat-control
                perform set-nothing
                add 1 to s-repeat(s)
                move s-start-control(s) to p
                perform increment-p
           when other
               perform decrement-s
               evaluate true
               when s-result(r) = 'N'
               and s-repeat(r) = 0 *> no result
                   perform increment-p
               when s-result(r) = 'N'
               and s-repeat(r) > 0 *> no result after success
                   perform set-success
                   perform increment-p
               when other *> fail the sequence
                   perform increment-p
               end-evaluate
           end-evaluate

        when p-symbol(p) = '(' *> start sequence
            perform increment-s
            perform display-start-control
            move p to s-start-control(s)
            move p-alternate(p) to s-alternate(s)
            move p-matching(p) to s-end-control(s)
            move 0 to s-count(s)
            perform increment-p

       when p-symbol(p) = ')' *> end sequence
           perform display-end-control
           perform decrement-s
           evaluate true
           when s-result(r) = 'S' *> success
               perform set-success
               perform increment-p
           when s-result(r) = 'N' *> no result
               perform set-failure
               perform increment-p
            when other *> fail the sequence
               perform set-failure
               perform increment-p
           end-evaluate

        when p-symbol(p) = '|' *> alternate
            evaluate true
            when s-result(s) = 'S' *> exit the sequence
                perform display-skip-alternate
                move s-end-control(s) to p
            when other
                perform display-take-alternate
                move p-alternate(p) to s-alternate(s) *> the next alternate
                perform increment-p
                perform set-nothing
            end-evaluate

        when p-symbol(p) = 't' *> terminal
            move p-definition(p) to t
            move terminal-symbol-len(t) to t-len
            perform display-terminal
            evaluate true
            when statement(l:t-len) = terminal-symbol(t)(1:t-len) *> successful match
               perform set-success
               perform display-recognize-terminal
               perform process-token
               move t-len to l-len
               perform increment-l
               perform increment-p
            when s-alternate(s) <> 000 *> we are in an alternate sequence
               move s-alternate(s) to p
            when other *> fail the sequence
               perform set-failure
               move s-end-control(s) to p
            end-evaluate

        when other *> end control
            perform display-control-failure *> shouldnt happen

        end-evaluate

     end-perform

     evaluate true *> at end of evaluation
     when error-found = 'y'
         continue
     when l <= l-lim *> not all tokens parsed
         display 'error: invalid statement'
         perform statement-error
     when number-use <> spaces
         display 'error:  not all numbers were used: ' number-use
         move 'y' to error-found
     end-evaluate
    .
increment-l.
    evaluate true
    when l > l-lim *> end of statement
        continue
    when other
        add l-len to l
        perform varying l from l by 1
        until c(l) <> space
        or l > l-lim
            continue
        end-perform
        move 1 to l-len
        if l > l-lim
            perform end-tokens
        end-if
    end-evaluate
    .
increment-p.
    evaluate true
    when p >= p-max
        display 'at' space p ' parse overflow'
            space 's=<' s space s-entry(s) '>'
        move 'y' to error-found
    when other
        add 1 to p
        perform display-statement
    end-evaluate
    .
increment-s.
    evaluate true
    when s >= s-max
        display 'at' space p ' stack overflow '
            space 's=<' s space s-entry(s) '>'
        move 'y' to error-found
    when other
        move s to r
        add 1 to s
        initialize s-entry(s)
        move 'N' to s-result(s)
        move p to s-p(s)
        move nt to s-nt(s)
    end-evaluate
    .
decrement-s.
    if s > 0
        move s to r
        subtract 1 from s
        if s > 0
            move s-nt(s) to nt
        end-if
    end-if
    .
set-failure.
    move 'F' to s-result(s)
    if s-count(s) > 0
        display 'sequential parse failure'
        perform statement-error
    end-if
    .
set-success.
    move 'S' to s-result(s)
    add 1 to s-count(s)
    .
set-nothing.
    move 'N' to s-result(s)
    move 0 to s-count(s)
    .
statement-error.
    display statement
    move spaces to statement
    move '^ syntax error' to statement(l:)
    display statement
    move 'y' to error-found
    .
*>
### ===============

*> twentyfour semantics
*>
### ===============

test-variable.
    *> check validity
    perform varying nd from 1 by 1 until nd > 4
    or c(l) = n(nd)
        continue
    end-perform
    *> check usage
    perform varying nu from 1 by 1 until nu > 4
    or c(l) = u(nu)
        continue
    end-perform
    evaluate true
    when l > l-lim
        perform set-failure
    when c9(l) not numeric
        perform set-failure
    when nd > 4
        display 'invalid number'
        perform statement-error
    when nu > 4
        display 'number already used'
        perform statement-error
    when other
        move space to u(nu)
        perform set-success
        add 1 to oqx
        move c(l) to output-queue(oqx:1)
    end-evaluate
    .
*>
### ============================

*> Dijkstra Shunting-Yard Algorithm
*> to convert infix to rpn
*>
### ============================

process-token.
    evaluate true
    when c(l) = '('
        add 1 to osx
        move c(l) to operator-stack(osx:1)
    when c(l) = ')'
        perform varying osx from osx by -1 until osx < 1
        or operator-stack(osx:1) = '('
            add 1 to oqx
            move operator-stack(osx:1) to output-queue(oqx:1)
        end-perform
        if osx < 1
            display 'parenthesis error'
            perform statement-error
            exit paragraph
        end-if
        subtract 1 from osx
    when (c(l) = '+' or '-') and (operator-stack(osx:1) = '*' or '/')
        *> lesser operator precedence
        add 1 to oqx
        move operator-stack(osx:1) to output-queue(oqx:1)
        move c(l) to operator-stack(osx:1)
    when other
        *> greater operator precedence
        add 1 to osx
        move c(l) to operator-stack(osx:1)
    end-evaluate
    .
end-tokens.
    *> 1) copy stacked operators to the output-queue
    perform varying osx from osx by -1 until osx < 1
    or operator-stack(osx:1) = '('
        add 1 to oqx
        move operator-stack(osx:1) to output-queue(oqx:1)
    end-perform
    if osx > 0
        display 'parenthesis error'
        perform statement-error
        exit paragraph
    end-if
    *> 2) evaluate the rpn statement
    perform evaluate-rpn
    if divide-by-zero-error = 'y'
        display 'divide by zero error'
    end-if
    .
evaluate-rpn.
    move space to divide-by-zero-error
    move 0 to rsx *> stack depth
    perform varying oqx1 from 1 by 1 until oqx1 > oqx
        if output-queue(oqx1:1) >= '1' and <= '9'
            *> push current data onto the stack
            add 1 to rsx
            move top-numerator to numerator(rsx)
            move top-denominator to denominator(rsx)
            move output-queue(oqx1:1) to top-numerator
            move 1 to top-denominator
        else
            *> apply the operation
            evaluate true
            when output-queue(oqx1:1) = '+'
                compute top-numerator = top-numerator * denominator(rsx)
                    + top-denominator * numerator(rsx)
                compute top-denominator = top-denominator * denominator(rsx)
            when output-queue(oqx1:1) = '-'
                compute top-numerator = top-denominator * numerator(rsx)
                    - top-numerator * denominator(rsx)
                compute top-denominator = top-denominator * denominator(rsx)
            when output-queue(oqx1:1) = '*'
                compute top-numerator = top-numerator * numerator(rsx)
                compute top-denominator = top-denominator * denominator(rsx)
            when output-queue(oqx1:1) = '/'
                compute work-number = numerator(rsx) * top-denominator
                compute top-denominator = denominator(rsx) * top-numerator
                if top-denominator = 0
                    move 'y' to divide-by-zero-error
                    exit paragraph
                end-if
                move work-number to top-numerator
            end-evaluate
            *> pop the stack
            subtract 1 from rsx
        end-if
    end-perform
    .
*>
### ==============

*> diagnostic displays
*>
### ==============

display-start-nonterminal.
    perform varying nt from nt-lim by -1 until nt < 1
    or p-definition(p) = nonterminal-statement-number(nt)
        continue
    end-perform
    if nt > 0
        move '1' to NL-flag
        string '1' indent(1:s + s) 'at ' s space p ' start ' trim(nonterminal-statement(nt))
            into message-area perform display-message
        move nt to s-nt(s)
    end-if
    .
display-continue-nonterminal.
    move s-nt(s) to nt
    string '1' indent(1:s + s) 'at ' s space p space p-symbol(p) ' continue ' trim(nonterminal-statement(nt)) ' with result ' s-result(s)
            into message-area perform display-message
    .
display-end-nonterminal.
    move s-nt(s) to nt
    move '2' to NL-flag
    string '1' indent(1:s + s) 'at ' s space p ' end ' trim(nonterminal-statement(nt)) ' with result ' s-result(s)
            into message-area perform display-message
    .
display-start-control.
    string '2' indent(1:s + s) 'at ' s space p ' start ' p-symbol(p) ' in ' trim(nonterminal-statement(nt))
        into message-area perform display-message
    .
display-repeat-control.
    string '2' indent(1:s + s) 'at ' s space p ' repeat ' p-symbol(p) ' in ' trim(nonterminal-statement(nt))  ' with result ' s-result(s)
        into message-area perform display-message
    .
display-end-control.
    string '2' indent(1:s + s) 'at ' s space p ' end ' p-symbol(p)  ' in ' trim(nonterminal-statement(nt)) ' with result ' s-result(s)
        into message-area perform display-message
    .
display-take-alternate.
    string '2' indent(1:s + s) 'at ' s space p ' take alternate' ' in ' trim(nonterminal-statement(nt))
        into message-area perform display-message
    .
display-skip-alternate.
    string '2' indent(1:s + s) 'at ' s space p ' skip alternate' ' in ' trim(nonterminal-statement(nt))
        into message-area perform display-message
    .
display-terminal.
    string '1' indent(1:s + s) 'at ' s space p
        ' compare ' statement(l:t-len) ' to ' terminal-symbol(t)(1:t-len)
        ' in ' trim(nonterminal-statement(nt))
        into message-area perform display-message
    .
display-recognize-terminal.
    string '1' indent(1:s + s) 'at ' s space p ' recognize terminal: ' c(l) ' in ' trim(nonterminal-statement(nt))
        into message-area perform display-message
    .
display-recognize-variable.
    string '1' indent(1:s + s) 'at ' s space p ' recognize digit: ' c(l) ' in ' trim(nonterminal-statement(nt))
        into message-area perform display-message
    .
display-statement.
    compute p1 = p - s-start-control(s)
    string '3' indent(1:s + s) 'at ' s space p
        ' statement: ' s-start-control(s) '/' p1
        space p-symbol(p) space s-result(s)
        ' in ' trim(nonterminal-statement(nt))
        into message-area perform display-message
    .
display-control-failure.
    display loop-count space indent(1:s + s) 'at' space p ' control failure' ' in ' trim(nonterminal-statement(nt))
    display loop-count space indent(1:s + s) '   ' 'p=<' p p-entry(p) '>'
    display loop-count space indent(1:s + s) '   ' 's=<' s space s-entry(s) '>'
    display loop-count space indent(1:s + s) '   ' 'l=<' l space c(l)'>'
    perform statement-error
    .
display-message.
    if display-level = 1
        move space to NL-flag
    end-if
    evaluate true
    when loop-count > loop-lim *> loop control
        display 'display count exceeds ' loop-lim
        stop run
    when message-level <= display-level
        evaluate true
        when NL-flag = '1'
             display NL loop-count space trim(message-value)
        when NL-flag = '2'
             display loop-count space trim(message-value) NL
        when other
             display loop-count space trim(message-value)
        end-evaluate
    end-evaluate
    add 1 to loop-count
    move spaces to message-area
    move space to NL-flag
    .
end program twentyfour.

```



## CoffeeScript

*Works with: node.js*

```coffeescript
tty = require 'tty'
tty.setRawMode true

buffer  = ""
numbers = []

for n in [0...4]
    numbers.push Math.max 1, Math.floor(Math.random() * 9)

console.log "You can use the numbers: #{numbers.join ' '}"

process.stdin.on 'keypress', (char, key) ->

    # accept operator
    if char and isNaN(char) and /[()*\/+-]/.test(char) and buffer.substr(-1) isnt char
        buffer += char
        process.stdout.write char
    # accept number
    else if !isNaN(+char) and (buffer == '' or isNaN(buffer.substr -1))
        buffer += char
        process.stdout.write char

    # check then evaluate expression
    if key?.name is 'enter'
        result = calculate()
        process.stdout.write '\n'
        if result and result is 24
            console.log " = 24! congratulations."
        else
            console.log "#{result}. nope."
        process.exit 0

    # quit
    if key?.name is 'escape' or (key?.name == 'c' and key.ctrl)
        process.exit 0

calculate = () ->

    if /[^\d\s()+*\/-]/.test buffer
        console.log "invalid characters"
        process.exit 1

    used = buffer.match(/\d/g)
    if used?.length != 4 or used.sort().join() != numbers.sort().join()
        console.log "you must use the 4 numbers provided"
        process.exit 1

    res = try eval buffer catch e
    return res or 'invalid expression'


# begin taking input
process.stdin.resume()

```



## Common Lisp


```lisp
(define-condition choose-digits () ())
(define-condition bad-equation (error) ())

(defun 24-game ()
  (let (chosen-digits)
    (labels ((prompt ()
               (format t "Chosen digits: ~{~D~^, ~}~%~
                          Enter expression (or `bye' to quit, `!' to choose new digits): "
                       chosen-digits)
               (read))
             (lose () (error 'bad-equation))
             (choose () (setf chosen-digits (loop repeat 4 collecting (1+ (random 9)))))
             (check (e)
               (typecase e
                 ((eql bye) (return-from 24-game))
                 ((eql !) (signal 'choose-digits))
                 (atom (lose))
                 (cons (check-sub (car e) (check-sub (cdr e) chosen-digits)) e)))
             (check-sub (sub allowed-digits)
               (typecase sub
                 ((member nil + - * /) allowed-digits)
                 (integer
                  (if (member sub allowed-digits)
                      (remove sub allowed-digits :count 1)
                      (lose)))
                 (cons (check-sub (car sub) (check-sub (cdr sub) allowed-digits)))
                 (t (lose))))
             (win ()
               (format t "You win.~%")
               (return-from 24-game)))
      (choose)
      (loop
       (handler-case
           (if (= 24 (eval (check (prompt)))) (win) (lose))
         (error () (format t "Bad equation, try again.~%"))
         (choose-digits () (choose)))))))
```


**Verbose Implementation**

*Works with: clisp 2.47*

```lisp

(defconstant +ops+ '(* / + -))

(defun expr-numbers (e &optional acc)
  "Return all the numbers in argument positions in the expression."
  (cond
   ((numberp e) (cons e acc))
   ((consp e)
    (append (apply #'append
                   (mapcar #'expr-numbers (cdr e)))
            acc))))

(defun expr-well-formed-p (e)
  "Return non-nil if the given expression is well-formed."
  (cond
   ((numberp e) t)
   ((consp e)
    (and (member (car e) +ops+)
         (every #'expr-well-formed-p (cdr e))))
   (t nil)))

(defun expr-valid-p (e available-digits)
  "Return non-nil if the expression is well-formed and uses exactly
the digits specified."
  (and (expr-well-formed-p e)
       (equalp (sort (copy-seq available-digits) #'<)
               (sort (expr-numbers e) #'<))))

(defun expr-get (&optional using)
  (emit "Enter lisp form~@[ using the digit~P ~{~D~^ ~}~]: "
        (when using
          (length using)) using)
  (let (*read-eval*)
    (read)))

(defun digits ()
  (sort (loop repeat 4 collect (1+ (random 9))) #'<))

(defun emit (fmt &rest args)
  (format t "~&~?" fmt args))

(defun prompt (digits)
  (emit "Using only these operators:~%~%~
           ~2T~{~A~^ ~}~%~%~
         And exactly these numbers \(no repetition\):~%~%~
           ~2T~{~D~^ ~}~%~%~
         ~A"
        +ops+ digits (secondary-prompt)))

(defun secondary-prompt ()
  (fill-to 50 "Enter a lisp form which evaluates to ~
               the integer 24, or \"!\" to get fresh ~
               digits, or \"q\" to abort."))

(defun fill-to (n fmt &rest args)
  "Poor-man's text filling mechanism."
  (loop with s = (format nil "~?" fmt args)
        for c across s
        and i from 0
        and j = 0 then (1+ j) ; since-last-newline ctr

        when (char= c #\Newline)
        do (setq j 0)

        else when (and (not (zerop j))
                       (zerop (mod j n)))
        do (loop for k from i below (length s)
                 when (char= #\Space (schar s k))
                 do (progn
                      (setf (schar s k) #\Newline
                            j 0)
                      (loop-finish)))
        finally (return s)))

(defun 24-game ()
  (loop with playing-p = t
        and initial-digits = (digits)

        for attempts from 0
        and digits = initial-digits then (digits)

        while playing-p

        do (loop for e = (expr-get (unless (zerop attempts)
                                     digits))
                 do
                 (case e
                   (! (loop-finish))
                   (Q (setq playing-p nil)
                      (loop-finish))
                   (R (emit "Current digits: ~S" digits))
                   (t
                    (if (expr-valid-p e digits)
                        (let ((v (eval e)))
                          (if (eql v 24)
                              (progn
                                (emit "~%~%---> A winner is you! <---~%~%")
                                (setq playing-p nil)
                                (loop-finish))
                            (emit "Sorry, the form you entered ~
                                   computes to ~S, not 24.~%~%"
                                  v)))
                      (emit "Sorry, the form you entered did not ~
                             compute.~%~%")))))
        initially (prompt initial-digits)))
```


Example Usage:


```txt
CL-USER 97 > (24-game)
Using only these operators:

  * / + -

And exactly these numbers (no repetition):

  3 7 7 9

Enter a lisp form which evaluates to the integer 24,
or "!" to get fresh digits, or "q" to abort.
Enter lisp form: (eval (read-from-string "(/ 1 0)"))
Sorry, the form you entered did not compute.

Enter lisp form: !
Enter lisp form using the digits 4 5 7 8: !
Enter lisp form using the digits 1 2 4 5: (* 4 (* 5 (- 2 1)))
Sorry, the form you entered computes to 20, not 24.

Enter lisp form using the digits 1 2 4 5: (* 4 (+ 5 (- 2 1)))


---> A winner is you! <---

NIL
```



## D


```d
import std.stdio, std.random, std.math, std.algorithm, std.range,
       std.typetuple;

void main() {
    void op(char c)() {
        if (stack.length < 2)
            throw new Exception("Wrong expression.");
        stack[$ - 2] = mixin("stack[$ - 2]" ~ c ~ "stack[$ - 1]");
        stack.popBack();
    }

    const problem = iota(4).map!(_ => uniform(1, 10))().array();
    writeln("Make 24 with the digits: ", problem);

    double[] stack;
    int[] digits;
    foreach (const char c; readln())
        switch (c) {
            case ' ', '\t', '\n': break;
            case '1': .. case '9':
                stack ~= c - '0';
                digits ~= c - '0';
                break;
            foreach (o; TypeTuple!('+', '-', '*', '/')) {
                case o: op!o(); break;
            }
            break;
            default: throw new Exception("Wrong char: " ~ c);
        }

    if (!digits.sort().equal(problem.dup.sort()))
        throw new Exception("Not using the given digits.");
    if (stack.length != 1)
        throw new Exception("Wrong expression.");
    writeln("Result: ", stack[0]);
    writeln(abs(stack[0] - 24) < 0.001 ? "Good job!" : "Try again.");
}
```

Example:

```txt
Make 24 with the digits: [1, 8, 9, 8]
8 1 - 9 + 8 +
Result: 24
Good job!
```



## EchoLisp


```scheme

(string-delimiter "")
;; check that nums are in expr, and only once
(define (is-valid? expr sorted: nums)
    (when (equal? 'q expr) (error "24-game" "Thx for playing"))
    (unless (and
        (list? expr)
        (equal? nums (list-sort < (filter number? (flatten expr)))))
    (writeln "🎃 Please use" nums)
    #f))

;; 4 random  digits
(define (gen24)
     (->> (append (range 1 10)(range 1 10)) shuffle (take 4) (list-sort < )))

(define (is-24? num)
    (unless (= 24 num)
    (writeln "😧 Sorry - Result = " num)
    #f))

(define (check-24 expr)
    (if (and
        (is-valid? expr nums)
        (is-24?  (js-eval (string expr)))) ;; use js evaluator
        "🍀 🌸 Congrats - (play24) for another one."
        (input-expr check-24 (string nums))))

(define nums null)
(define (play24)
    (set! nums (gen24))
    (writeln "24-game - Can you combine" nums "to get 24 ❓ (q to exit)")
    (input-expr check-24 (string-append  (string nums) " -> 24 ❓")))

```

### Output

```txt

24-game - Can you combine     (2 5 6 7)     to get 24 ❓ (q to exit)
difficult game
🎃 Please use     (2 5 6 7)
12 * 2
🎃 Please use     (2 5 6 7)
6 * (7 - 5 + 2)
🍀 🌸 Congrats - (play24) for another one.

(play24)
24-game - Can you combine     (3 5 8 9)     to get 24 ❓ (q to exit)
3 + 5 + 8 * 9
😧 Sorry - Result =      80
9 * 3 - (8 - 5)
🍀 🌸 Congrats - (play24) for another one.

(play24)
24-game - Can you combine     (1 8 8 9)     to get 24 ❓ (q to exit)
9 + 8 + 8 - 1
🍀 🌸 Congrats - (play24) for another one.

```



## Elena

ELENA 4.1 :

```elena
import system'routines;
import system'collections;
import system'dynamic;
import extensions;

class ExpressionTree
{
    object theTree;

    constructor(s)
    {
        auto level := new Integer(0);

        s.forEach:(ch)
        {
            var node := new DynamicStruct();

            ch =>
                $43 { node.Level := level + 1; node.Operation := __subj add }        // +
                $45 { node.Level := level + 1; node.Operation := __subj subtract }   // -
                $42 { node.Level := level + 2; node.Operation := __subj multiply }   // *
                $47 { node.Level := level + 2; node.Operation := __subj divide }     // /
                $40 { level.append(10); ^ self }                               // (
                $41 { level.reduce(10); ^ self }                               // )
                : {
                    node.Leaf := ch.toString().toReal();
                    node.Level := level + 3
                };

            if (nil == theTree)
            {
                theTree := node
            }
            else
            {
                if (theTree.Level >= node.Level)
                {
                    node.Left := theTree;
                    node.Right := nilValue;

                    theTree := node
                }
                else
                {
                        var top := theTree;
                        while ((nilValue != top.Right)&&(top.Right.Level < node.Level))
                            { top := top.Right };

                        node.Left := top.Right;
                        node.Right := nilValue;

                        top.Right := node
                }
            }
        }
    }

    eval(node)
    {
        if (node.containsProperty(subjconst Leaf))
        {
            ^ node.Leaf
        }
        else
        {
            var left := self.eval(node.Left);
            var right := self.eval(node.Right);

            var op := node.Operation;

            ^ op(left, right)
        }
    }

    get Value()
        <= eval(theTree);

    readLeaves(list, node)
    {
        if (nil == node)
            { InvalidArgumentException.raise() };

        var s := subjconst Leaf;

        if (node.containsProperty(subjconst Leaf))
        {
            list.append(node.Leaf)
        }
        else
        {
            self.readLeaves(list, node.Left);
            self.readLeaves(list, node.Right)
        }
    }

    readLeaves(list)
        <= readLeaves(list,theTree);
}

class TwentyFourGame
{
    object theNumbers;

    constructor()
    {
        self.newPuzzle();
    }

    newPuzzle()
    {
        theNumbers := new object[]::(
                1 + randomGenerator.eval:9,
                1 + randomGenerator.eval:9,
                1 + randomGenerator.eval:9,
                1 + randomGenerator.eval:9
            )
    }

    help()
    {
        console
            .printLine:"------------------------------- Instructions ------------------------------"
            .printLine:"Four digits will be displayed."
            .printLine:"Enter an equation using all of those four digits that evaluates to 24"
            .printLine:"Only * / + - operators and () are allowed"
            .printLine:"Digits can only be used once, but in any order you need."
            .printLine:"Digits cannot be combined - i.e.: 12 + 12 when given 1,2,2,1 is not allowed"
            .printLine:"Submit a blank line to skip the current puzzle."
            .printLine:"Type 'q' to quit"
            .writeLine()
            .printLine:"Example: given 2 3 8 2, answer should resemble 8*3-(2-2)"
            .printLine:"------------------------------- --------------------------------------------"
    }

    prompt()
    {
        theNumbers.forEach:(n){ console.print(n," ") };

        console.print:": "
    }

    resolve(expr)
    {
        var tree := new ExpressionTree(expr);

        var leaves := new ArrayList();
        tree.readLeaves:leaves;

        ifnot (leaves.ascendant().sequenceEqual(theNumbers.ascendant()))
            { console.printLine:"Invalid input. Enter an equation using all of those four digits. Try again."; ^ self };

        var result := tree.Value;
        if (result == 24)
        {
            console.printLine("Good work. ",expr,"=",result);

            self.newPuzzle()
        }
        else
        {
            console.printLine("Incorrect. ",expr,"=",result)
        }
    }
}

extension gameOp
{
    playRound(expr)
    {
        if (expr == "q")
        {
            ^ false
        }
        else
        {
            if (expr == "")
            {
                console.printLine:"Skipping this puzzle"; self.newPuzzle()
            }
            else
            {
                try
                {
                    self.resolve(expr)
                }
                catch(Exception e)
                {
                    console.printLine:"An error occurred.  Check your input and try again."
                }
            };

            ^ true
        }
    }
}

public program()
{
    var game := new TwentyFourGame().help();

    while (game.prompt().playRound(console.readLine())) {}
}
```

### Output

```txt

------------------------------- Instructions ------------------------------
Four digits will be displayed.
Enter an equation using all of those four digits that evaluates to 24
Only * / + - operators and () are allowed
Digits can only be used once, but in any order you need.
Digits cannot be combined - i.e.: 12 + 12 when given 1,2,2,1 is not allowed
Submit a blank line to skip the current puzzle.
Type 'q' to quit

Example: given 2 3 8 2, answer should resemble 8*3-(2-2)
------------------------------- --------------------------------------------
7 6 9 6 :
Skipping this puzzle
8 6 2 6 : 6*6-8-2
Incorrect. 6*6-8-2=26.0
8 6 2 6 :
Skipping this puzzle
5 2 7 7 : 7+7+(5*2)
Good work. 7+7+(5*2)=24.0

```



## Elixir

{{trans|Erlang}}

```elixir
defmodule Game24 do
  def main do
    IO.puts "24 Game"
    play
  end

  defp play do
    IO.puts "Generating 4 digits..."
    digts = for _ <- 1..4, do: Enum.random(1..9)
    IO.puts "Your digits\t#{inspect digts, char_lists: :as_lists}"
    read_eval(digts)
    play
  end

  defp read_eval(digits) do
    exp = IO.gets("Your expression: ") |> String.strip
    if exp in ["","q"], do: exit(:normal)        # give up
    case {correct_nums(exp, digits), eval(exp)} do
      {:ok, x} when x==24 -> IO.puts "You Win!"
      {:ok, x} -> IO.puts "You Lose with #{inspect x}!"
      {err, _} -> IO.puts "The following numbers are wrong: #{inspect err, char_lists: :as_lists}"
    end
  end

  defp correct_nums(exp, digits) do
    nums = String.replace(exp, ~r/\D/, " ") |> String.split |> Enum.map(&String.to_integer &1)
    if length(nums)==4 and (nums--digits)==[], do: :ok, else: nums
  end

  defp eval(exp) do
    try do
      Code.eval_string(exp) |> elem(0)
    rescue
      e -> Exception.message(e)
    end
  end
end

Game24.main
```


### Output

```txt

24 Game
Generating 4 digits...
Your digits     [9, 6, 7, 4]
Your expression: (9+7)*6/4
You Win!
Generating 4 digits...
Your digits     [3, 2, 2, 4]
Your expression: 3*(2+2+4)
You Win!
```



## Erlang


```Erlang
-module(g24).
-export([main/0]).

main() ->
    random:seed(now()),
    io:format("24 Game~n"),
    play().

play() ->
    io:format("Generating 4 digits...~n"),
    Digts = [random:uniform(X) || X <- [9,9,9,9]],
    io:format("Your digits\t~w~n", [Digts]),
    read_eval(Digts),
    play().

read_eval(Digits) ->
    Exp = string:strip(io:get_line(standard_io, "Your expression: "), both, $\n),
    case {correct_nums(Exp, Digits), eval(Exp)} of
        {ok, X} when X == 24 -> io:format("You Win!~n");
        {ok, X} -> io:format("You Lose with ~p!~n",[X]);
        {List, _} -> io:format("The following numbers are wrong: ~p~n", [List])
    end.

correct_nums(Exp, Digits) ->
    case re:run(Exp, "([0-9]+)", [global, {capture, all_but_first, list}]) of
        nomatch ->
            "No number entered";
        {match, IntLs} ->
            case [X || [X] <- IntLs, not lists:member(list_to_integer(X), Digits)] of
                [] -> ok;
                L -> L
            end
    end.

eval(Exp) ->
    {X, _} = eval(re:replace(Exp, "\\s", "", [{return, list},global]),
                  0),
    X.

eval([], Val) ->
    {Val,[]};
eval([$(|Rest], Val) ->
    {NewVal, Exp} = eval(Rest, Val),
    eval(Exp, NewVal);
eval([$)|Rest], Val) ->
    {Val, Rest};
eval([$[|Rest], Val) ->
    {NewVal, Exp} = eval(Rest, Val),
    eval(Exp, NewVal);
eval([$]|Rest], Val) ->
    {Val, Rest};
eval([$+|Rest], Val) ->
    {NewOperand, Exp} = eval(Rest, 0),
    eval(Exp, Val + NewOperand);
eval([$-|Rest], Val) ->
    {NewOperand, Exp} = eval(Rest, 0),
    eval(Exp, Val - NewOperand);
eval([$*|Rest], Val) ->
    {NewOperand, Exp} = eval(Rest, 0),
    eval(Exp, Val * NewOperand);
eval([$/|Rest], Val) ->
    {NewOperand, Exp} = eval(Rest, 0),
    eval(Exp, Val / NewOperand);
eval([X|Rest], 0) when X >= $1, X =< $9 ->
    eval(Rest, X-$0).

```


The evaluator uses a simple infix scheme that doesn't care about operator precedence, but does support brackets and parentheses alike. Thus, <code>((9+1)*2)+2+2</code> is evaluated as:


```txt

9 + 1 = 10
10 * 2 = 20
2 + 2 = 4
20 + 4

```


Example:

```txt
1> c(g24).
{ok,g24}
2> g24:main().
24 Game
Generating 4 digits...
Your digits     [7,4,6,8]
Your expression: 6*4
You Win!
Generating 4 digits...
Your digits     [4,1,5,8]
Your expression: 6*4
The following numbers are wrong: ["6"]
Generating 4 digits...
Your digits     [8,5,8,2]
Your expression: 2*([8/5]*2)
You Lose with 6.4!
Generating 4 digits...
Your digits     [7,4,8,1]
```


## F_Sharp|F#

```fsharp
open System
open System.Text.RegularExpressions

// Some utilities
let (|Parse|_|) regex str =
   let m = Regex(regex).Match(str)
   if m.Success then Some ([for g in m.Groups -> g.Value]) else None
let rec gcd x y = if x = y || x = 0 then y else if x < y then gcd y x else gcd y (x-y)
let abs (x : int) = Math.Abs x
let sign (x: int) = Math.Sign x
let cint s = Int32.Parse(s)
let replace m (s : string) t = Regex.Replace(t, m, s)

// computing in Rationals
type Rat(x : int, y : int) =
    let g = if y <> 0 then gcd (abs x) (abs y) else raise <| DivideByZeroException()
    member this.n = sign y * x / g   // store a minus sign in the numerator
    member this.d =
        if y <> 0 then sign y * y / g else raise <| DivideByZeroException()
    static member (~-) (x : Rat) = Rat(-x.n, x.d)
    static member (+) (x : Rat, y : Rat) = Rat(x.n * y.d + y.n * x.d, x.d * y.d)
    static member (-) (x : Rat, y : Rat) = x + Rat(-y.n, y.d)
    static member (*) (x : Rat, y : Rat) = Rat(x.n * y.n, x.d * y.d)
    static member (/) (x : Rat, y : Rat) = x * Rat(y.d, y.n)
    override this.ToString() = sprintf @"<%d,%d>" this.n this.d
    new(x : string, y : string) = if y = "" then Rat(cint x, 1) else Rat(cint x, cint y)

// Due to the constraints imposed by the game (reduced set
// of operators, all left associativ) we can get away with a repeated reduction
// to evaluate the algebraic expression.
let rec reduce (str :string) =
    let eval (x : Rat) (y : Rat) = function
    | "*" -> x * y | "/" -> x / y | "+" -> x + y | "-" -> x - y | _ -> failwith "unknown op"
    let subst s r = str.Replace(s, r.ToString())
    let rstr =
        match str with
        | Parse @"\(<(-?\d+),(\d+)>([*/+-])<(-?\d+),(\d+)>\)" [matched; xn; xd; op; yn; yd] ->
            subst matched <| eval (Rat(xn,xd)) (Rat(yn,yd)) op
        | Parse @"<(-?\d+),(\d+)>([*/])<(-?\d+),(\d+)>" [matched; xn; xd; op; yn; yd] ->
            subst matched <| eval (Rat(xn,xd)) (Rat(yn,yd)) op
        | Parse @"<(-?\d+),(\d+)>([+-])<(-?\d+),(\d+)>" [matched; xn; xd; op; yn; yd] ->
            subst matched <| eval (Rat(xn,xd)) (Rat(yn,yd)) op
        | Parse @"\(<(-?\d+),(\d+)>\)" [matched; xn; xd] ->
            subst matched <| Rat(xn,xd)
        | Parse @"(?<!>)-<(-?\d+),(\d+)>" [matched; xn; xd] ->
            subst matched <| -Rat(xn,xd)
        | _ -> str
    if str = rstr then str else reduce rstr

let gameLoop() =
    let checkInput dddd input =
        match input with
        | "n" | "q" -> Some(input)
        | Parse @"[^1-9()*/+-]" [c] ->
            printfn "You used an illegal character in your expression: %s" c
            None
        | Parse @"^\D*(\d)\D+(\d)\D+(\d)\D+(\d)(?:\D*(\d))*\D*$" [m; d1; d2; d3; d4; d5] ->
            if d5 = "" && (String.Join(" ", Array.sort [|d1;d2;d3;d4|])) = dddd then Some(input)
            elif d5 = "" then
                printfn "Use this 4 digits with operators in between: %s." dddd
                None
            else
                printfn "Use only this 4 digits with operators in between: %s." dddd
                None
        | _ ->
            printfn "Use all 4 digits with operators in between: %s." dddd
            None

    let rec userLoop dddd  =
        let tryAgain msg =
            printfn "%s" msg
            userLoop dddd
        printf "[Expr|n|q]: "
        match Console.ReadLine() |> replace @"\s" "" |> checkInput dddd with
        | Some(input) ->
            let data = input |> replace @"((?<!\d)-)?\d+" @"<$&,1>"
            match data with
            | "n" -> true | "q" -> false
            | _ ->
                try
                    match reduce data with
                    | Parse @"^<(-?\d+),(\d+)>$" [_; x; y] ->
                        let n, d = (cint x), (cint y)
                        if n = 24 then
                            printfn "Correct!"
                            true
                        elif d=1 then tryAgain <| sprintf "Wrong! Value = %d." n
                        else tryAgain <| sprintf "Wrong! Value = %d/%d." n d
                    | _ -> tryAgain "Wrong! not a well-formed expression!"
                with
                    | :? System.DivideByZeroException ->
                        tryAgain "Wrong! Your expression results in a division by zero!"
                    | ex ->
                        tryAgain <| sprintf "There is an unforeseen problem with yout input: %s" ex.Message
        | None -> userLoop dddd

    let random = new Random(DateTime.Now.Millisecond)
    let rec loop() =
        let dddd = String.Join(" ", Array.init 4 (fun _ -> 1 + random.Next 9) |> Array.sort)
        printfn "\nCompute 24 from the following 4 numbers: %s" dddd
        printfn "Use them in any order with * / + - and parentheses; n = new numbers; q = quit"
        if userLoop dddd then loop()

    loop()

gameLoop()
```

### Output

```txt
Compute 24 from the following 4 numbers: 3 3 3 5
Use them in any order with * / + - and parentheses; n = new numbers; q = quit
[Expr|n|q]: n

Compute 24 from the following 4 numbers: 3 5 6 7
Use them in any order with * / + - and parentheses; n = new numbers; q = quit
[Expr|n|q]: (7 + 5) + 6/3
Wrong! Value = 14.
[Expr|n|q]: (7 + 5) * 6/3
Correct!

Compute 24 from the following 4 numbers: 3 3 4 5
Use them in any order with * / + - and parentheses; n = new numbers; q = quit
[Expr|n|q]: q

```



## Factor


```factor
USING:
    combinators.short-circuit
    continuations
    eval
    formatting
    fry
    kernel
    io
    math math.ranges
    prettyprint
    random
    sequences
    sets ;
IN: 24game

: choose4 ( -- seq )
    4 [ 9 [1,b] random ] replicate ;

: step ( numbers -- ? )
    readln
    [
        parse-string
        {
            ! Is only allowed tokens used?
            [ swap { + - / * } append subset? ]
            ! Digit count in expression should be equal to the given numbers.
            [ [ number? ] count swap length = ]
            ! Of course it must evaluate to 24
            [ nip call( -- x ) 24 = ]
        } 2&&
        [ f "You got it!" ]
        [ t "Expression isnt valid, or doesnt evaluate to 24." ]
        if
    ]
    [ 3drop f "Could not parse that." ]
    recover print flush ;

: main ( -- )
    choose4
    [ "Your numbers are %[%s, %], make an expression\n" printf flush ]
    [ '[ _ step ] loop ]
    bi ;

```

Sample:

```factor

IN: scratchpad main
Your numbers are { 4, 1, 8, 2 }, make an expression
8 4 + 2 * 1 /
You got it!

```



## Falcon


```falcon
load compiler

function genRandomNumbers( amount )
  rtn = []
  for i in [ 0 : amount ]: rtn += random( 1, 9 )
  return( rtn )
end

function getAnswer( exp )
  ic = ICompiler()
  ic.compileAll(exp)

  return( ic.result )
end

function validInput( str )
  for i in [ 0 : str.len() ]
    if str[i] notin ' ()[]0123456789-+/*'
      > 'INVALID Character = ', str[i]
      return( false )
    end
  end

  return( true )
end

printl('
The 24 Game

Given any four digits in the range 1 to 9, which may have repetitions,
Using just the +, -, *, and / operators; and the possible use of
brackets, (), show how to make an answer of 24.

An answer of "q" will quit the game.
An answer of "!" will generate a new set of four digits.
Otherwise you are repeatedly asked for an expression until it evaluates to 24

Note: you cannot form multiple digit numbers from the supplied digits,
so an answer of 12+12 when given 1, 2, 2, and 1 would not be allowed.
')

num = genRandomNumbers( 4 )

while( true )

  >>  "Here are the numbers to choose from: "
  map({ a => print(a, " ") }, num)
  >

  exp = input()

  switch exp
    case "q", "Q"
      exit()

    case "!"
      > 'Generating new numbers list'
      num = genRandomNumbers( 4 )

    default
      if not validInput( exp ): continue

      answer = getAnswer( exp )

      if answer == 24
        > "By George you GOT IT! Your expression equals 24"
      else
        > "Ahh Sorry, So Sorry your answer of ", answer, " does not equal 24."
      end
  end
end
```



## Fortran


### Clever implementation

Indicate operator precedence by parentheses; e.g. (3+(5*6))-9. No whitespace is admissible.
The program uses [Insertion_sort in Fortran](/tasks/Insertion_sort#Fortran).

```Fortran
program game_24
  implicit none
  real               :: vector(4), reals(11), result, a, b, c, d
  integer            :: numbers(4), ascii(11), i
  character(len=11)  :: expression
  character          :: syntax(11)
  ! patterns:
  character, parameter :: one(11)   = (/ '(','(','1','x','1',')','x','1',')','x','1' /)
  character, parameter :: two(11)   = (/ '(','1','x','(','1','x','1',')',')','x','1' /)
  character, parameter :: three(11) = (/ '1','x','(','(','1','x','1',')','x','1',')' /)
  character, parameter :: four(11)  = (/ '1','x','(','1','x','(','1','x','1',')',')' /)
  character, parameter :: five(11)  = (/ '(','1','x','1',')','x','(','1','x','1',')' /)

  do
    call random_number(vector)
    numbers = 9 * vector + 1
    write (*,*) 'Digits: ',numbers
    write (*,'(a)',advance='no') 'Your expression: '
    read (*,'(a11)') expression

    forall (i=1:11) syntax(i) = expression(i:i)
    ascii = iachar(syntax)
    where (syntax >= '0' .and. syntax <= '9')
      syntax = '1'  ! number
    elsewhere (syntax == '+' .or. syntax == '-' .or. syntax == '*' .or. syntax == '/')
      syntax = 'x'  ! op
    elsewhere (syntax /= '(' .and. syntax /= ')')
      syntax = '-'  ! error
    end where

    reals = real(ascii-48)
    if ( all(syntax == one) ) then
      a = reals(3); b = reals(5); c = reals(8); d = reals(11)
      call check_numbers(a,b,c,d)
      result = op(op(op(a,4,b),7,c),10,d)
    else if ( all(syntax == two) ) then
      a = reals(2); b = reals(5); c = reals(7); d = reals(11)
      call check_numbers(a,b,c,d)
      result = op(op(a,3,op(b,6,c)),10,d)
    else if ( all(syntax == three) ) then
      a = reals(1); b = reals(5); c = reals(7); d = reals(10)
      call check_numbers(a,b,c,d)
      result = op(a,2,op(op(b,6,c),9,d))
    else if ( all(syntax == four) ) then
      a = reals(1); b = reals(4); c = reals(7); d = reals(9)
      call check_numbers(a,b,c,d)
      result = op(a,2,op(b,5,op(c,8,d)))
    else if ( all(syntax == five) ) then
      a = reals(2); b = reals(4); c = reals(8); d = reals(10)
      call check_numbers(a,b,c,d)
      result = op(op(a,3,b),6,op(c,9,d))
    else
      stop 'Input string: incorrect syntax.'
    end if

    if ( abs(result-24.0) < epsilon(1.0) ) then
      write (*,*) 'You won!'
    else
      write (*,*) 'Your result (',result,') is incorrect!'
    end if

    write (*,'(a)',advance='no') 'Another one? [y/n] '
    read (*,'(a1)') expression
    if ( expression(1:1) == 'n' .or. expression(1:1) == 'N' ) then
      stop
    end if
  end do

contains

  pure real function op(x,c,y)
    integer, intent(in) :: c
    real, intent(in) :: x,y
    select case ( char(ascii(c)) )
      case ('+')
        op = x+y
      case ('-')
        op = x-y
      case ('*')
        op = x*y
      case ('/')
        op = x/y
    end select
  end function op

  subroutine check_numbers(a,b,c,d)
    real, intent(in) :: a,b,c,d
    integer          :: test(4)
    test = (/ nint(a),nint(b),nint(c),nint(d) /)
    call Insertion_Sort(numbers)
    call Insertion_Sort(test)
    if ( any(test /= numbers) ) then
      stop 'You cheat ;-) (Incorrect numbers)'
    end if
  end subroutine check_numbers

  pure subroutine Insertion_Sort(a)
    integer, intent(inout) :: a(:)
    integer                :: temp, i, j
    do i=2,size(a)
      j = i-1
      temp = a(i)
      do while ( j>=1 .and. a(j)>temp )
        a(j+1) = a(j)
        j = j - 1
      end do
      a(j+1) = temp
    end do
  end subroutine Insertion_Sort

end program game_24

```



### As a more general recursive descent parser:

Permits spaces and arbitrary parentheses.


```FORTRAN

! implement a recursive descent parser
module evaluate_algebraic_expression

  integer, parameter :: size = 124
  character, parameter :: statement_end = achar(0)
  character(len=size) :: text_to_parse
  integer :: position
  data position/0/,text_to_parse/' '/

contains

  character function get_token()
    ! return the current token
    implicit none
    if (position <= size) then
       get_token = text_to_parse(position:position)
       do while (get_token <= ' ')
          call advance
          if (size < position) exit
          get_token = text_to_parse(position:position)
       end do
    end if
    if (size < position) get_token = statement_end
  end function get_token

  subroutine advance ! consume a token.  Move to the next token.  consume_token would have been a better name.
    position = position + 1
  end subroutine advance

  logical function unfinished()
    unfinished = get_token() /= statement_end
  end function unfinished

  subroutine parse_error()
    write(6,*)'"'//get_token()//'" unexpected in expression at',position
    stop 1
  end subroutine parse_error

  function precedence3() result(a)
    implicit none
    real :: a
    character :: token
    character(len=10), parameter :: digits = '0123456789'
    token = get_token()
    if (verify(token,digits) /= 0) call parse_error()
    a = index(digits, token) - 1
    call advance()
  end function precedence3

  recursive function precedence2() result(a)
    real :: a
    character :: token
    token = get_token()
    if (token /= '(') then
       a = precedence3()
    else
       call advance
       a = precedence0()
       token = get_token()
       if (token /= ')') call parse_error()
       call advance
    end if
  end function precedence2

  recursive function precedence1() result(a)
    implicit none
    real :: a
    real, dimension(2) :: argument
    character(len=2), parameter :: tokens = '*/'
    character :: token
    a = 0
    token = get_token()
    argument(1) = precedence2()
    token = get_token()
    do while (verify(token,tokens) == 0)
       call advance()
       argument(2) = precedence2()
       if (token == '/') argument(2) = 1 / argument(2)
       argument(1) = product(argument)
       token = get_token()
    end do
    a = argument(1)
  end function precedence1

  recursive function precedence0() result(a)
    implicit none
    real :: a
    real, dimension(2) :: argument
    character(len=2), parameter :: tokens = '+-'
    character :: token
    a = 0
    token = get_token()
    argument(1) = precedence1()
    token = get_token()
    do while (verify(token,tokens) == 0)
       call advance()
       argument(2) = precedence1()
       if (token == '-') argument = argument * (/1, -1/)
       argument(1) = sum(argument)
       token = get_token()
    end do
    a = argument(1)
  end function precedence0

  real function statement()
    implicit none
    if (unfinished()) then
       statement = precedence0()
    else                        !empty okay
       statement = 0
    end if
    if (unfinished()) call parse_error()
  end function statement

  real function evaluate(expression)
    implicit none
    character(len=*), intent(in) :: expression
    text_to_parse = expression
    evaluate = statement()
  end function evaluate

end module evaluate_algebraic_expression


program g24
  use evaluate_algebraic_expression
  implicit none
  integer, dimension(4) :: digits
  character(len=78) :: expression
  real :: result
  ! integer :: i
  call random_seed!easily found internet examples exist to seed by /dev/urandom or time
  call deal(digits)
  ! do i=1, 9999 ! produce the data to test digit distribution
  !   call deal(digits)
  !   write(6,*) digits
  ! end do
  write(6,'(a13,4i2,a26)')'Using digits',digits,', and the algebraic dyadic'
  write(6,*)'operators +-*/() enter an expression computing 24.'
  expression = ' '
  read(5,'(a78)') expression
  if (invalid_digits(expression, digits)) then
     write(6,*)'invalid digits'
  else
     result = evaluate(expression)
     if (nint(result) == 24) then
        write(6,*) result, ' close enough'
     else
        write(6,*) result, ' no good'
     end if
  end if

contains

  logical function invalid_digits(e,d) !verify the digits
    implicit none
    character(len=*), intent(in) :: e
    integer, dimension(4), intent(inout) :: d
    integer :: i, j, k, count
    logical :: unfound
    count = 0
    invalid_digits = .false. !validity assumed
    !write(6,*)'expression:',e(1:len_trim(e))
    do i=1, len_trim(e)
       if (verify(e(i:i),'0123456789') == 0) then
          j = index('0123456789',e(i:i))-1
          unfound = .true.
          do k=1, 4
             if (j == d(k)) then
                unfound = .false.
                exit
             end if
          end do
          if (unfound) then
             invalid_digits = .true.
             !return or exit is okay here
          else
             d(k) = -99
             count = count + 1
          end if
       end if
    end do
    invalid_digits = invalid_digits .or. (count /= 4)
  end function invalid_digits

  subroutine deal(digits)
    implicit none
    integer, dimension(4), intent(out) :: digits
    integer :: i
    real :: harvest
    call random_number(harvest)
    do i=1, 4
       digits(i) = int(mod(harvest*9**i, 9.0))   + 1
    end do
    !    NB. computed with executable Iverson notation, www.jsoftware.oom
    !    #B NB. B are the digits from 9999 deals
    ! 39996
    !    ({.,#)/.~/:~B  # show the distribution of digits
    ! 0 4380
    ! 1 4542
    ! 2 4348
    ! 3 4395
    ! 4 4451
    ! 5 4474
    ! 6 4467
    ! 7 4413
    ! 8 4526
    !    NB. this also shows that I forgot to add 1.  Inserting now...
  end subroutine deal
end program g24

```


Compilation and too many examples.  Which would you cut?

```txt

$ gfortran -g -O0 -std=f2008 -Wall f.f08 -o f.exe && echo '8*(9/9+2)' | ./f.exe
 Using digits 9 9 8 2, and the algebraic dyadic
 operators +-*/() enter an expression computing 24.
   24.000000      close enough
$
$
$
$ ./f.exe
$  Using digits 9 9 8 2, and the algebraic dyadic
$  operators +-*/() enter an expression computing 24.
$     8 *   ( 9 / 9  +    2   )
$    24.000000      close enough
$
$
$ ./f.exe
 Using digits 9 9 8 2, and the algebraic dyadic
 operators +-*/() enter an expression computing 24.
(((2+8+9+9)))
   28.000000      no good
$
$
$ ./f.exe
 Using digits 9 9 8 2, and the algebraic dyadic
 operators +-*/() enter an expression computing 24.
(((8+9-2+9)))
   24.000000      close enough
$
$ ./f.exe
 Using digits 9 9 8 2, and the algebraic dyadic
 operators +-*/() enter an expression computing 24.
8929
 "9" unexpected in expression at           2
STOP 1
$
$
$ ./f.exe
 Using digits 9 9 8 2, and the algebraic dyadic
 operators +-*/() enter an expression computing 24.
12348
 invalid digits
$
$
$ ./f.exe
 Using digits 9 9 8 2, and the algebraic dyadic
 operators +-*/() enter an expression computing 24.
892
 invalid digits
$
$
$ ./f.exe
 Using digits 9 9 8 2, and the algebraic dyadic
 operators +-*/() enter an expression computing 24.
8921
 invalid digits
$
$
$
$ ./f.exe
 Using digits 9 9 8 2, and the algebraic dyadic
 operators +-*/() enter an expression computing 24.
89291
 invalid digits
$
$
$
$ ./f.exe
 Using digits 9 9 8 2, and the algebraic dyadic
 operators +-*/() enter an expression computing 24.
9+x-2+8+9
 "x" unexpected in expression at           3
STOP 1
$
$
$
$ ./f.exe
 Using digits 9 9 8 2, and the algebraic dyadic
 operators +-*/() enter an expression computing 24.
(9-2)+8+(9
 "^@" unexpected in expression at         125
STOP 1
$
$
$
$ ./f.exe
 Using digits 9 9 8 2, and the algebraic dyadic
 operators +-*/() enter an expression computing 24.
(9-2)+8+(9)
   24.000000      close enough
$
$
$
$ ./f.exe
 Using digits 9 9 8 2, and the algebraic dyadic
 operators +-*/() enter an expression computing 24.
(9-2)+8/(9)
   7.8888888      no good
$

```




## FreeBASIC

Solución en **RPN**:

```freebasic

' The 24 game en FreeBASIC

Const operaciones = "*/+-"

Declare Sub Encabezado
Declare Function escoge4() As String
Declare Function quitaEspacios(cadena As String, subcadena1 As String, subcadena2 As String) As String
Declare Function evaluaEntrada(cadena As String) As Integer
Declare Function evaluador(oper1 As Byte, oper2 As Byte, operacion As String) As Integer

Dim Shared As String serie, entrada, cadena
Dim As Integer resultado

Sub Encabezado
    Cls: Color 15
    Print "The 24 Game"
    Print "
### ======
" + Chr(13) + Chr(10)
    Print "Dados cuatro dígitos en el rango de 1 a 9, que pueden repetirse, "
    Print "usando solo los operadores aritméticos suma (+), resta (-), "
    Print "multiplicación (*) y división (/) intentar obtener un resultado de 24." + Chr(13) + Chr(10)
    Print "Use la notación polaca inversa (primero los operandos y luego los operadores)."
    Print "Por ejemplo: en lugar de 2 + 4, escriba 2 4 +" + Chr(13) + Chr(10)
End Sub

Function escoge4() As String
    Dim As Byte i
    Dim As String a, b

    Print "Los dígitos a utilizar son:   ";
    For i = 1 To 4
	a = Str(Int(Rnd*9)+1)
	Print a;"      ";
	b = b + a
    Next i
    escoge4 = b
End Function

Function evaluaEntrada(cadena As String) As Integer
    Dim As Byte oper1, oper2, n(4), i
    Dim As String op
    oper1 = 0: oper2 = 0: i = 0

    While cadena <> ""
	op = Left(cadena, 1)
	entrada = Mid(cadena, 2)
	If Instr(serie, op) Then
		i = i + 1
		n(i) = Val(op)
        Elseif Instr(operaciones, op) Then
		oper2 = n(i)
		n(i) = 0
		i = i - 1
		oper1 = n(i)
		n(i) = evaluador(oper1, oper2, op)
        Else
		Print "Signo no v lido"
        End If
    Wend
    evaluaEntrada = n(i)
End Function

Function evaluador(oper1 As Byte, oper2 As Byte, operacion As String) As Integer
    Dim As Integer t

    Select Case operacion
    Case "+": t = oper1 + oper2
    Case "-": t = oper1 - oper2
    Case "*": t = oper1 * oper2
    Case "/": t = oper1 / oper2
    End Select

    evaluador = t
End Function

Function quitaEspacios(cadena As String, subcadena1 As String, subcadena2 As String) As String
    Dim As Byte len1 = Len(subcadena1), len2 = Len(subcadena2)
    Dim As Byte i

    i = Instr(cadena, subcadena1)
    While i
        cadena = Left(cadena, i - 1) & subcadena2 & Mid(cadena, i + len1)
        i = Instr(i + len2, cadena, subcadena1)
    Wend
    quitaEspacios = cadena
End Function

'--- Programa Principal ---
Randomize Timer
Do
    Encabezado
    serie = escoge4
    Print: Line Input "Introduzca su fórmula en notación polaca inversa: ", entrada
    entrada = quitaEspacios(entrada, " ", "")
    If (Len(entrada) <> 7) Then
        Print "Error en la serie introducida."
    Else
        resultado = evaluaEntrada(entrada)
        Print "El resultado es = "; resultado
        If resultado = 24 Then
            Print "¡Correcto!"
        Else
            Print "¡Error!"
        End If
    End If
    Print "¿Otra ronda? (Pulsa S para salir, u otra tecla para continuar)"
Loop Until (Ucase(Input(1)) = "S")
End
'--------------------------

```

### Output

```txt

The 24 Game

### ======


Dados cuatro dígitos en el rango de 1 a 9, que pueden repetirse,
usando solo los operadores aritmÚticos suma (+), resta (-),
multiplicación (*) y división (/) intentar obtener un resultado de 24.

Use la notación polaca inversa (primero los operandos y luego los operadores).
Por ejemplo: en lugar de 2 + 4, escriba 2 4 +

Los dígitos a utilizar son:   4      9      7      5
Introduzca su fórmula en notación polaca inversa: 49*57+-
El resultado es =  24
Correcto!
¿Otra ronda? (Pulsa S para salir, u otra tecla para continuar)


```



## GAP

Solution in **RPN**:

```gap
Play24 := function()
	local input, digits, line, c, chars, stack, stackptr, cur, p, q, ok, a, b, run;
	input := InputTextUser();
	run := true;
	while run do
		digits := List([1 .. 4], n -> Random(1, 9));
		while true do
			Display(digits);
			line := ReadLine(input);
			line := Chomp(line);
			if line = "end" then
				run := false;
				break;
			elif line = "next" then
				break;
			else
				ok := true;
				stack := [ ];
				stackptr := 0;
				chars := "123456789+-*/ ";
				cur := ShallowCopy(digits);
				for c in line do
					if c = ' ' then
						continue;
					fi;
					p := Position(chars, c);
					if p = fail then
						ok := false;
						break;
					fi;
					if p < 10 then
						q := Position(cur, p);
						if q = fail then
							ok := false;
							break;
						fi;
						Unbind(cur[q]);
						stackptr := stackptr + 1;
						stack[stackptr] := p;
					else
						if stackptr < 2 then
							ok := false;
							break;
						fi;
						b := stack[stackptr];
						a := stack[stackptr - 1];
						stackptr := stackptr - 1;
						if c = '+' then
							a := a + b;
						elif c = '-' then
							a := a - b;
						elif c = '*' then
							a := a * b;
						elif c = '/' then
							if b = 0 then
								ok := false;
								break;
							fi;
							a := a / b;
						else
							ok := false;
							break;
						fi;
						stack[stackptr] := a;
					fi;
				od;
				if ok and stackptr = 1 and Size(cur) = 0 then
					if stack[1] = 24 then
						Print("Good !\n");
						break;
					else
						Print("Bad value: ", stack[1], "\n");
						continue;
					fi;
				fi;
				Print("Invalid expression\n");
			fi;
		od;
	od;
	CloseStream(input);
end;

# example session
# type "end" to quit the game, "next" to try another list of digits
gap> Play24();
[ 7, 6, 8, 5 ]
86*75-/
Good !
[ 5, 9, 2, 7 ]
end
gap>
```



## Go

RPN solution.

```go
package main

import (
    "fmt"
    "math"
    "math/rand"
    "time"
)

func main() {
    rand.Seed(time.Now().Unix())
    n := make([]rune, 4)
    for i := range n {
        n[i] = rune(rand.Intn(9) + '1')
    }
    fmt.Printf("Your numbers: %c\n", n)
    fmt.Print("Enter RPN: ")
    var expr string
    fmt.Scan(&expr)
    if len(expr) != 7 {
        fmt.Println("invalid. expression length must be 7." +
            " (4 numbers, 3 operators, no spaces)")
        return
    }
    stack := make([]float64, 0, 4)
    for _, r := range expr {
        if r >= '0' && r <= '9' {
            if len(n) == 0 {
                fmt.Println("too many numbers.")
                return
            }
            i := 0
            for n[i] != r {
                i++
                if i == len(n) {
                    fmt.Println("wrong numbers.")
                    return
                }
            }
            n = append(n[:i], n[i+1:]...)
            stack = append(stack, float64(r-'0'))
            continue
        }
        if len(stack) < 2 {
            fmt.Println("invalid expression syntax.")
            return
        }
        switch r {
        case '+':
            stack[len(stack)-2] += stack[len(stack)-1]
        case '-':
            stack[len(stack)-2] -= stack[len(stack)-1]
        case '*':
            stack[len(stack)-2] *= stack[len(stack)-1]
        case '/':
            stack[len(stack)-2] /= stack[len(stack)-1]
        default:
            fmt.Printf("%c invalid.\n", r)
            return
        }
        stack = stack[:len(stack)-1]
    }
    if math.Abs(stack[0]-24) > 1e-6 {
        fmt.Println("incorrect.", stack[0], "!= 24")
    } else {
        fmt.Println("correct.")
    }
}
```

Example game:

```txt

Your numbers: [5 8 1 3]
Enter RPN: 83-5*1-
correct.

```



## Gosu


```Gosu

uses java.lang.Double
uses java.lang.Integer
uses java.util.ArrayList
uses java.util.List
uses java.util.Scanner
uses java.util.Stack

function doEval( scanner : Scanner, allowed : List<Integer> ) : double {
    var stk = new Stack<Double>()

    while( scanner.hasNext() ) {
        if( scanner.hasNextInt() ) {
            var n = scanner.nextInt()

            // Make sure they're allowed to use n
            if( n <= 0 || n >= 10 ) {
                print( n + " isn't allowed" )
                return 0
            }
            var idx = allowed.indexOf( n )
            if( idx == -1 ) {
                print( "You aren't allowed to use so many " + n + "s!" )
                return 0
            }

            // Add the input number to the stack
            stk.push( new Double( n ) )

            // Mark n as used
            allowed.remove( idx )
        } else {
            // It has to be an operator...
            if( stk.size() < 2 ) {
                print( "Invalid Expression: Stack underflow!" )
                return 0
            }

            // Gets the next operator as a single character token
            var s = scanner.next("[\\+-/\\*]")

            // Get the operands
            var r = stk.pop().doubleValue()
            var l = stk.pop().doubleValue()

            // Determine which operator and invoke it
            if( s.equals( "+" ) ) {
                stk.push( new Double( l + r ) )
            } else if( s.equals( "-" ) ) {
                stk.push( new Double( l - r ) )
            } else if( s.equals( "*" ) ) {
                stk.push( new Double( l * r ) )
            } else if( s.equals( "/" ) ) {
                if( r == 0.0 ) {
                    print( "Invalid Expression: Division by zero!" )
                    return 0
                }
                stk.push( new Double( l / r ) )
            } else {
                print( "Internal Error: looking for operator yielded '" + s + "'" )
                return 0
            }
        }
    }

    // Did they skip any numbers?
    if( allowed.size() != 0 ) {
        print( "You didn't use ${allowed}" )
        return 0
    }

    // Did they use enough operators?
    if( stk.size() != 1 ) {
        print( "Invalid Expression: Not enough operators!" )
        return 0
    }

    return stk.pop().doubleValue()
}

// Pick 4 random numbers from [1..9]
var nums = new ArrayList<Integer>()
var gen = new java.util.Random( new java.util.Date().getTime() )
for( i in 0..3 ) {
    nums.add( gen.nextInt(9) + 1 )
}

// Prompt the user
print( "Using addition, subtraction, multiplication and division, write an" )
print( "expression that evaluates to 24 using" )
print( "${nums.get(0)}, ${nums.get(1)}, ${nums.get(2)} and ${nums.get(3)}" )
print( "" )
print( "Please enter your expression in RPN" )

// Build a tokenizer over a line of input
var sc = new Scanner( new java.io.BufferedReader( new java.io.InputStreamReader( java.lang.System.in ) ).readLine() )

// eval the expression
var val = doEval( sc, nums )

// winner?
if( java.lang.Math.abs( val - 24.0 ) < 0.001 ) {
    print( "You win!" )
} else {
    print( "You lose!" )
}

```



## Groovy

{{trans|Ruby}}
This solution breaks strict adherence to the rules in only one way: any line that starts with the letter "q" causes the game to quit.

```groovy
final random = new Random()
final input = new Scanner(System.in)


def evaluate = { expr ->
    if (expr == 'QUIT') {
        return 'QUIT'
    } else {
        try { Eval.me(expr.replaceAll(/(\d)/, '$1.0')) }
        catch (e) { 'syntax error' }
    }
}


def readGuess = { digits ->
    while (true) {
        print "Enter your guess using ${digits} (q to quit): "
        def expr = input.nextLine()

        switch (expr) {
            case ~/^[qQ].*/:
                return 'QUIT'

            case ~/.*[^\d\s\+\*\/\(\)-].*/:
                def badChars = expr.replaceAll(~/[\d\s\+\*\/\(\)-]/, '')
                println "invalid characters in input: ${(badChars as List) as Set}"
                break

            case { (it.replaceAll(~/\D/, '') as List).sort() != ([]+digits).sort() }:
                println '''you didn't use the right digits'''
                break

            case ~/.*\d\d.*/:
                println 'no multi-digit numbers allowed'
                break

            default:
                return expr
        }
    }
}


def digits = (1..4).collect { (random.nextInt(9) + 1) as String }

while (true) {
    def guess = readGuess(digits)
    def result = evaluate(guess)

    switch (result) {
        case 'QUIT':
            println 'Awwww. Maybe next time?'
            return

        case 24:
            println 'Yes! You got it.'
            return

        case 'syntax error':
            println "A ${result} was found in ${guess}"
            break

        default:
            println "Nope: ${guess} == ${result}, not 24"
            println 'One more try, then?'
    }
}
```


Sample Run:

```txt
$ groovy TwentyFour.gsh
Enter your guess using [4, 8, 3, 6] (q to quit): 4836
no multi-digit numbers allowed
Enter your guess using [4, 8, 3, 6] (q to quit): 4  ++ ++ 8/ 3-6
A syntax error was found in 4  ++ ++ 8/ 3-6
Enter your guess using [4, 8, 3, 6] (q to quit): btsjsb
invalid characters in input: [t, s, b, j]
Enter your guess using [4, 8, 3, 6] (q to quit): 1+3+2+2
you didn't use the right digits
Enter your guess using [4, 8, 3, 6] (q to quit): q
Awwww. Maybe next time?

$ groovy TwentyFour.gsh
Enter your guess using [6, 3, 2, 6] (q to quit): 6+6+3+2
Nope: 6+6+3+2 == 17.0, not 24
One more try, then?
Enter your guess using [6, 3, 2, 6] (q to quit): (6*3 - 6) * 2
Yes! You got it.
```



## Haskell


```Haskell
import Data.List (sort)
import Data.Char (isDigit)
import Data.Maybe (fromJust)
import Control.Monad (foldM)
import System.Random (randomRs, getStdGen)
import System.IO (hSetBuffering, stdout, BufferMode(NoBuffering))

main = do
  hSetBuffering stdout NoBuffering
  mapM_
    putStrLn
    [ "THE 24 GAME\n"
    , "Given four digits in the range 1 to 9"
    , "Use the +, -, *, and / operators in reverse polish notation"
    , "To show how to make an answer of 24.\n"
    ]
  digits <- fmap (sort . take 4 . randomRs (1, 9)) getStdGen :: IO [Int]
  putStrLn ("Your digits: " ++ unwords (fmap show digits))
  guessLoop digits
  where
    guessLoop digits =
      putStr "Your expression: " >> fmap (processGuess digits . words) getLine >>=
      either (\m -> putStrLn m >> guessLoop digits) putStrLn

processGuess _ [] = Right ""
processGuess digits xs
  | not matches = Left "Wrong digits used"
  where
    matches = digits == (sort . fmap read $ filter (all isDigit) xs)
processGuess digits xs = calc xs >>= check
  where
    check 24 = Right "Correct"
    check x = Left (show (fromRational (x :: Rational)) ++ " is wrong")

-- A Reverse Polish Notation calculator with full error handling
calc xs =
  foldM simplify [] xs >>=
  \ns ->
     (case ns of
        [n] -> Right n
        _ -> Left "Too few operators")

simplify (a:b:ns) s
  | isOp s = Right ((fromJust $ lookup s ops) b a : ns)
simplify _ s
  | isOp s = Left ("Too few values before " ++ s)
simplify ns s
  | all isDigit s = Right (fromIntegral (read s) : ns)
simplify _ s = Left ("Unrecognized symbol: " ++ s)

isOp v = elem v $ fmap fst ops

ops = [("+", (+)), ("-", (-)), ("*", (*)), ("/", (/))]
```



## HicEst


```HicEst
DIMENSION digits(4), input_digits(100), difference(4)
CHARACTER expression*100, prompt*100, answers='Wrong,Correct,', protocol='24 game.txt'

1  digits = CEILING( RAN(9) )
2  DLG(Edit=expression, Text=digits, TItle=prompt)

   READ(Text=expression, ItemS=n) input_digits
   IF(n == 4) THEN
     ALIAS(input_digits,1,  input,4)
     SORT(Vector=digits, Sorted=digits)
     SORT(Vector=input, Sorted=input)
     difference = ABS(digits - input)
     IF( SUM(difference) == 0 ) THEN
       EDIT(Text=expression, ScaNnot='123456789+-*/ ()', GetPos=i, CoPyto=prompt)
       IF( i > 0 ) THEN
         prompt = TRIM(expression) // ': ' //TRIM(prompt) // ' is an illegal character'
       ELSE
         prompt = TRIM(expression) // ': Syntax error'
         result = XEQ(expression, *2) ! on error branch to label 2
         EDIT(Text=answers, ITeM=(result==24)+1, Parse=answer)
         WRITE(Text=prompt, Name) TRIM(expression)//': ', answer, result
       ENDIF
     ELSE
       WRITE(Text=prompt) TRIM(expression), ': You used ', input, ' instead ', digits
     ENDIF
   ELSE
     prompt = TRIM(expression) // ': Instead 4 digits you used ' // n
   ENDIF

   OPEN(FIle=protocol, APPend)
   WRITE(FIle=protocol, CLoSe=1) prompt

   DLG(TItle=prompt, Button='>2:Try again', B='>1:New game', B='Quit')

END
```


```HicEst
4 + 8 + 7 + 5: You used 4 5 7 8  instead 4 4 7 8
4 + 8 + 7 + a: Instead 4 digits you used 3
4 + 8 + 7 + a + 4: a is an illegal character
4 + 8 + 7a + 4: a is an illegal character
4 + 8 + 7 + 4:; answer=Wrong; result=23;
4 * 7 - 8 + 4:; answer=Correct; result=24;
```



## Huginn


```huginn
#! /bin/sh
exec huginn --no-argv -E "${0}"
#! huginn

import Algorithms as algo;
import Mathematics as math;
import RegularExpressions as re;

make_game( rndGen_ ) {
  board = "";
  for ( i : algo.range( 4 ) ) {
    board += ( " " + string( character( rndGen_.next() + integer( '1' ) ) ) );
  }
  return ( board.strip() );
}

main() {
  rndGen = math.randomizer( 9 );
  no = 0;
  dd = re.compile( "\\d\\d" );
  while ( true ) {
    no += 1;
    board = make_game( rndGen );
    print( "Your four digits: {}\nExpression {}: ".format( board, no ) );
    line = input();
    if ( line == none ) {
      print( "\n" );
      break;
    }
    line = line.strip();
    try {
      if ( line == "q" ) {
        break;
      }
      if ( ( pos = line.find_other_than( "{}+-*/() ".format( board ) ) ) >= 0 ) {
        print( "Invalid input found at: {}, `{}`\n".format( pos, line ) );
        continue;
      }
      if ( dd.match( line ).matched() ) {
        print( "Digit concatenation is forbidden.\n" );
        continue;
      }
      res = real( line );
      if ( res == 24.0 ) {
        print( "Thats right!\n" );
      } else {
        print( "Bad answer!\n" );
      }
    } catch ( Exception e ) {
      print( "Not an expression: {}\n".format( e.what() ) );
    }
  }
  return ( 0 );
}
```


## Icon and ## Unicon
This plays the game of 24 using a simplified version of the code from the [Arithmetic evaluation](/tasks/Arithmetic_evaluation#Icon_and_Unicon) task.

```Icon
invocable all
link strings   # for csort, deletec

procedure main()
help()
repeat {
   every (n := "") ||:= (1 to 4, string(1+?8))
   writes("Your four digits are : ")
   every writes(!n," ")
   write()

   e := trim(read()) | fail
   case e of {
      "q"|"quit": return
      "?"|"help": help()
      default: {
         e := deletec(e,' \t')         # no whitespace
         d := deletec(e,~&digits)      # just digits
         if csort(n) ~== csort(d) then # and only the 4 given digits
            write("Invalid repsonse.") & next

         if e ? (ans := eval(E()), pos(0)) then # parse and evaluate
            if ans = 24 then write("Congratulations you win!")
            else write("Your answer was ",ans,". Try again.")
         else write("Invalid expression.")
         }
      }
   }
end

procedure eval(X)    #: return the evaluated AST
   if type(X) == "list" then {
      x := eval(get(X))
      while x := get(X)(real(x), real(eval(get(X) | stop("Malformed expression."))))
   }
   return \x | X
end

procedure E()    #: expression
   put(lex := [],T())
   while put(lex,tab(any('+-*/'))) do
      put(lex,T())
   suspend if *lex = 1 then lex[1] else lex     # strip useless []
end

procedure T()                   #: Term
   suspend 2(="(", E(), =")") | # parenthesized subexpression, or ...
       tab(any(&digits))        # just a value
end

procedure help()
return write(
   "Welcome to 24\n\n",
   "Combine the 4 given digits to make 24 using only + - * / and ( ).\n ",
   "All operations have equal precedence and are evaluated left to right.\n",
   "Combining (concatenating) digits is not allowed.\n",
   "Enter 'help', 'quit', or an expression.\n")
end
```


*Library: Icon Programming Library*
[strings.icn provides deletec and sortc](http://www.cs.arizona.edu/icon/library/src/procs/strings.icn)

### Output

```txt
Welcome to 24

The object of the game is to combine the 4 given digits using only + - * / and ( ).
All operations have equal precedence and are evaluated left to right.
Combining (concatenating) digits is not allowed.
Enter 'help', 'quit', or an expression.

Your four digits are : 8 1 7 2
8*2+(7+1)
Congratulations you win!
Your four digits are : 4 2 7 6
7*6+(4*2)
Your answer was 50. Try again.
Your four digits are : 7 7 8 8
77-88
Invalid expression.
Your four digits are : 9 3 2 3
9+3+2+3+
Malformed expression.
```



## J



```J
require'misc'
deal=: 1 + ? bind 9 9 9 9
rules=: smoutput bind 'see http://en.wikipedia.org/wiki/24_Game'
input=: prompt @ ('enter 24 expression using ', ":, ': '"_)

wellformed=: (' '<;._1@, ":@[) -:&(/:~)  '(+-*%)' -.&;:~ ]
is24=: 24 -: ". ::0:@]

respond=: (;:'no yes') {::~ wellformed * is24

game24=: (respond input)@deal@rules
```


Example use:

    '*<nowiki>game24 *</nowiki>'''
 see <nowiki>http://en.wikipedia.org/wiki/24_Game</nowiki>
 enter 24 expression using 6 5 9 4: **6+5+9+4**
 yes
    '*<nowiki>game24 *</nowiki>'''
 see <nowiki>http://en.wikipedia.org/wiki/24_Game</nowiki>
 enter 24 expression using 3 3 3 3: **3+3+3+3+3+3+3+3**
 no


## Java

*Works with: Java 7*

```java
import java.util.*;

public class Game24 {
    static Random r = new Random();

    public static void main(String[] args) {

        int[] digits = randomDigits();
        Scanner in = new Scanner(System.in);

        System.out.print("Make 24 using these digits: ");
        System.out.println(Arrays.toString(digits));
        System.out.print("> ");

        Stack<Float> s = new Stack<>();
        long total = 0;
        for (char c : in.nextLine().toCharArray()) {
            if ('0' <= c && c <= '9') {
                int d = c - '0';
                total += (1 << (d * 5));
                s.push((float) d);
            } else if ("+/-*".indexOf(c) != -1) {
                s.push(applyOperator(s.pop(), s.pop(), c));
            }
        }
        if (tallyDigits(digits) != total)
            System.out.print("Not the same digits. ");
        else if (Math.abs(24 - s.peek()) < 0.001F)
            System.out.println("Correct!");
        else
            System.out.print("Not correct.");
    }

    static float applyOperator(float a, float b, char c) {
        switch (c) {
            case '+':
                return a + b;
            case '-':
                return b - a;
            case '*':
                return a * b;
            case '/':
                return b / a;
            default:
                return Float.NaN;
        }
    }

    static long tallyDigits(int[] a) {
        long total = 0;
        for (int i = 0; i < 4; i++)
            total += (1 << (a[i] * 5));
        return total;
    }

    static int[] randomDigits() {
        int[] result = new int[4];
        for (int i = 0; i < 4; i++)
            result[i] = r.nextInt(9) + 1;
        return result;
    }
}
```


### Output

```txt
Make 24 using these digits: [1, 2, 4, 8]
> 12*48+*
Correct!
```



## JavaScript



```javascript

function twentyfour(numbers, input) {
    var invalidChars = /[^\d\+\*\/\s-\(\)]/;

    var validNums = function(str) {
        // Create a duplicate of our input numbers, so that
        // both lists will be sorted.
        var mnums = numbers.slice();
        mnums.sort();

        // Sort after mapping to numbers, to make comparisons valid.
        return str.replace(/[^\d\s]/g, " ")
            .trim()
            .split(/\s+/)
            .map(function(n) { return parseInt(n, 10); })
            .sort()
            .every(function(v, i) { return v === mnums[i]; });
    };

    var validEval = function(input) {
        try {
            return eval(input);
        } catch (e) {
            return {error: e.toString()};
        }
    };

    if (input.trim() === "") return "You must enter a value.";
    if (input.match(invalidChars)) return "Invalid chars used, try again. Use only:\n + - * / ( )";
    if (!validNums(input)) return "Wrong numbers used, try again.";
    var calc = validEval(input);
    if (typeof calc !== 'number') return "That is not a valid input; please try again.";
    if (calc !== 24) return "Wrong answer: " + String(calc) + "; please try again.";
    return input + " == 24.  Congratulations!";
};

// I/O below.

while (true) {
    var numbers = [1, 2, 3, 4].map(function() {
        return Math.floor(Math.random() * 8 + 1);
    });

    var input = prompt(
        "Your numbers are:\n" + numbers.join(" ") +
        "\nEnter expression. (use only + - * / and parens).\n", +"'x' to exit.", "");

    if (input === 'x') {
        break;
    }
    alert(twentyfour(numbers, input));
}

```



## Julia

This implementation, because it is based on the Julia parser and evaluator, allows the user to enter arbitrary infix expressions, including parentheses.  (These expressions are checked to ensure that they only include the allowed operations on integer literals.)

```julia
validexpr(ex::Expr) = ex.head == :call && ex.args[1] in [:*,:/,:+,:-] && all(validexpr, ex.args[2:end])
validexpr(ex::Int) = true
validexpr(ex::Any) = false
findnumbers(ex::Number) = Int[ex]
findnumbers(ex::Expr) = vcat(map(findnumbers, ex.args)...)
findnumbers(ex::Any) = Int[]
function twentyfour()
    digits = sort!(rand(1:9, 4))
    while true
        print("enter expression using $digits => ")
        ex = parse(readline())
        try
            validexpr(ex) || error("only *, /, +, - of integers is allowed")
            nums = sort!(findnumbers(ex))
            nums == digits || error("expression $ex used numbers $nums != $digits")
            val = eval(ex)
            val == 24 || error("expression $ex evaluated to $val, not 24")
            println("you won!")
            return
        catch e
            if isa(e, ErrorException)
                println("incorrect: ", e.msg)
            else
                rethrow()
            end
        end
    end
end
```

### Output

```txt

julia> twentyfour()
enter expression using [2,5,8,9] => 5 * 2 - 8 + 9
incorrect: expression (5 * 2 - 8) + 9 evaluated to 11, not 24
enter expression using [2,5,8,9] => 5 * 5 + 2 + 8 - 9
incorrect: expression (5 * 5 + 2 + 8) - 9 used numbers [2,5,5,8,9] != [2,5,8,9]
enter expression using [2,5,8,9] => 8*2*2
incorrect: expression 8 * 2 * 2 used numbers [2,2,8] != [2,5,8,9]
enter expression using [2,5,8,9] => (8 + 9) + (5 + 2)
you won!
```



## Kotlin


```scala
import java.util.Random
import java.util.Scanner
import java.util.Stack

internal object Game24 {
    fun run() {
        val r = Random()
        val digits = IntArray(4).map { r.nextInt(9) + 1 }
        println("Make 24 using these digits: $digits")
        print("> ")

        val s = Stack<Float>()
        var total = 0L
        val cin = Scanner(System.`in`)
        for (c in cin.nextLine()) {
            when (c) {
                in '0'..'9' -> {
                    val d = c - '0'
                    total += (1 shl (d * 5)).toLong()
                    s += d.toFloat()
                }
                else ->
                    if ("+/-*".indexOf(c) != -1) {
                        s += c.applyOperator(s.pop(), s.pop())
                    }
            }
        }

        when {
            tally(digits) != total ->
                print("Not the same digits. ")
            s.peek().compareTo(target) == 0 ->
                println("Correct!")
            else ->
                print("Not correct.")
        }
    }

    private fun Char.applyOperator(a: Float, b: Float) = when (this) {
        '+' -> a + b
        '-' -> b - a
        '*' -> a * b
        '/' -> b / a
        else -> Float.NaN
    }

    private fun tally(a: List<Int>): Long = a.reduce({ t, i -> t + (1 shl (i * 5)) }).toLong()

    private val target = 24
}

fun main(args: Array<String>) = Game24.run()
```



## Lasso

This solution requires web input from the user via a form of the expression.

On submit the expression is checked for valid chars, that the integers are valid and in the original array (which also takes care of non-duplicate integers), and that the integers are not in consecutive positions.

If a valid expression it is evaluated, and the result and success state shown to the user.

```Lasso
[
if(sys_listunboundmethods !>> 'randoms') => {
	define randoms()::array => {
		local(out = array)
		loop(4) => { #out->insert(math_random(9,1)) }
		return #out
	}
}
if(sys_listunboundmethods !>> 'checkvalid') => {
	define checkvalid(str::string, nums::array)::boolean => {
		local(chk = array('*','/','+','-','(',')',' '), chknums = array, lastintpos = -1, poscounter = 0)
		loop(9) => { #chk->insert(loop_count) }
		with s in #str->values do => {
			#poscounter++
			#chk !>> #s && #chk !>> integer(#s) ? return false
			integer(#s) > 0 && #lastintpos + 1 >= #poscounter ? return false
			integer(#s) > 0 ? #chknums->insert(integer(#s))
			integer(#s) > 0 ? #lastintpos = #poscounter
		}
		#chknums->size != 4 ? return false
		#nums->sort
		#chknums->sort
		loop(4) => { #nums->get(loop_count) != #chknums(loop_count) ? return false }
		return true
	}
}
if(sys_listunboundmethods !>> 'executeexpr') => {
	define executeexpr(expr::string)::integer => {
		local(keep = string)
		with i in #expr->values do => {
			if(array('*','/','+','-','(',')') >> #i) => {
				#keep->append(#i)
			else
				integer(#i) > 0 ? #keep->append(decimal(#i))
			}
		}
		return integer(sourcefile('['+#keep+']','24game',true,true)->invoke)
	}
}

local(numbers = array, exprsafe = true, exprcorrect = false, exprresult = 0)
if(web_request->param('nums')->asString->size) => {
	with n in web_request->param('nums')->asString->split(',') do => { #numbers->insert(integer(#n->trim&)) }
}
#numbers->size != 4 ? #numbers = randoms()
if(web_request->param('nums')->asString->size) => {
	#exprsafe = checkvalid(web_request->param('expr')->asString,#numbers)
	if(#exprsafe) => {
		#exprresult = executeexpr(web_request->param('expr')->asString)
		#exprresult == 24 ? #exprcorrect = true
	}
}

]<h1>24 Game</h1>
<p><b>Rules:</b>

Enter an expression that evaluates to 24</p>
<ul>
<li>Only multiplication, division, addition, and subtraction operators/functions are allowed.</li>
<li>Brackets are allowed.</li>
<li>Forming multiple digit numbers from the supplied digits is disallowed. (So an answer of 12+12 when given 1, 2, 2, and 1 is wrong).</li>
<li>The order of the digits when given does not have to be preserved.</li>
</ul>

<h2>Numbers</h2>
<p>[#numbers->join(', ')] (<a href="?">Reload</a>)</p>
[!#exprsafe ? '<p>Please provide a valid expression</p>']
<form><input type="hidden" value="[#numbers->join(',')]" name="nums"><input type="text" name="expr" value="[web_request->param('expr')->asString]"><input type="submit" name="submit" value="submit"></form>
[if(#exprsafe)]
<p>Result: <b>[#exprresult]</b> [#exprcorrect ? 'is CORRECT!' | 'is incorrect']</p>
[/if]
```



## Liberty BASIC


```lb
dim d(4)
dim chk(4)
print "The 24 Game"
print
print "Given four digits and using just the +, -, *, and / operators; and the"
print "possible use of brackets, (), enter an expression that equates to 24."

do
    d$=""
    for i = 1 to 4
        d(i)=int(rnd(1)*9)+1    '1..9
        chk(i)=d(i)
        d$=d$;d(i)  'valid digits, to check with Instr
    next

    print
    print "These are your four digits: ";
    for i = 1 to 4
        print d(i);left$(",",i<>4);
    next
    print

    Print "Enter expression:"
    Input "24 = ";expr$
    'check expr$ for validity

    'check right digits used
    failed = 0
    for i = 1 to len(expr$)
        c$=mid$(expr$,i,1)
        if instr("123456789", c$)<>0 then 'digit
            if instr(d$, c$)=0 then failed = 1: exit for
            if i>1 and instr("123456789", mid$(expr$,i-1,1))<>0 then failed = 2: exit for
            for j =1 to 4
                if chk(j)=val(c$) then chk(j)=0: exit for
            next
        end if
    next
    if failed=1 then
        print "Wrong digit (";c$;")"
        goto [fail]
    end if

    if failed=2 then
        print "Multiple digit numbers is disallowed."
        goto [fail]
    end if

    'check all digits used
    if chk(1)+ chk(2)+ chk(3)+ chk(4)<>0 then
        print "Not all digits used"
        goto [fail]
    end if

    'check valid operations
    failed = 0
    for i = 1 to len(expr$)
        c$=mid$(expr$,i,1)
        if instr("+-*/()"+d$, c$)=0 then failed = 1: exit for
    next
    if failed then
        print "Wrong operation (";c$;")"
        goto [fail]
    end if
    'some errors (like brackets) trapped by error handler
    Err$=""
    res=evalWithErrCheck(expr$)
    if Err$<>"" then
        print "Error in expression"
        goto [fail]
    end if
    if res = 24 then
        print "Correct!"
    else
        print "Wrong! (you got ";res ;")"
    end if
[fail]
    Input "Play again (y/n)? "; ans$
loop while ans$="y"
end

function evalWithErrCheck(expr$)
    on error goto [handler]
    evalWithErrCheck=eval(expr$)
    exit function
[handler]
end function
```



## LiveCode

GUI version

1. Open livecode and create a new mainstack

2. Create 3 fields and 1 button

3. label fields "YourNumbersField", "EvalField" and "AnswerField"

4. label button "StartButton"

5. Add the following to the code of "StartButton"
```livecode
on mouseUp
    put empty into fld "EvalField"
    put empty into fld "AnswerField"
    put random(9) & comma & random(9) & comma & random(9) & comma & random(9) into fld "YourNumbersField"
end mouseUp
```

6. Add the following to the code of field "EvalField"
```livecode

on keyDown k
    local ops, nums, allowedKeys, numsCopy, expr
    put "+,-,/,*,(,)" into ops
    put the text of fld "YourNumbersField" into nums
    put the text of fld "EvalField" into expr
    if matchText(expr & k,"\d\d") then
        answer "You can't enter 2 digits together"
        exit keyDown
    end if
    repeat with n = 1 to the number of chars of expr
        if offset(char n of expr, nums) > 0 then
            delete char offset(char n of expr, nums) of nums
        end if
    end repeat
    put ops & comma & nums into allowedKeys
    if k is among the items of allowedKeys then
        put k after expr
        delete char offset(k, nums) of nums
        replace comma with empty in nums
        try
            put the value of merge("[[expr]]") into fld "AnswerField"
            if the value of fld "AnswerField" is 24 and nums is empty then
                answer "You win!"
            end if
        end try
        pass keyDown
    else
        exit keyDown
    end if
end keyDown
```



## Locomotive Basic



```locobasic
10 CLS:RANDOMIZE TIME
20 PRINT "The 24 Game"
30 PRINT "
### =====
":PRINT
40 PRINT "Enter an arithmetic expression"
50 PRINT "that evaluates to 24,"
60 PRINT "using only the provided digits"
70 PRINT "and +, -, *, /, (, )."
80 PRINT "(Just hit Return for new digits.)"
90 ' create new digits
100 FOR i=1 TO 4:a(i)=INT(RND*9)+1:NEXT
110 PRINT
120 PRINT "The digits are";a(1);a(2);a(3);a(4)
130 PRINT
140 ' user enters solution
150 INPUT "Your solution";s$
160 IF s$="" THEN PRINT "Creating new digits...":GOTO 100
170 GOTO 300
180 ' a little hack to create something like an EVAL function
190 OPENOUT "exp.bas"
200 PRINT #9,"1000 x="s$":return"
210 CLOSEOUT
220 CHAIN MERGE "exp",240
230 ' now evaluate the expression
240 ON ERROR GOTO 530
250 GOSUB 1000
260 IF x=24 THEN PRINT "Well done!":END
270 PRINT "No, this evaluates to"x:PRINT "Please try again."
280 GOTO 150
290 ' check input for correctness
300 FOR i=1 TO LEN(s$)
310 q=ASC(MID$(s$,i,1))
320 IF q=32 OR (q>39 AND q<44) OR q=45 OR (q>46 AND q<58) THEN NEXT
330 IF i-1=LEN(s$) THEN 370
340 PRINT "Bad character in expression:"CHR$(q)
350 PRINT "Try again":GOTO 150
360 ' new numbers in solution?
370 FOR i=1 TO LEN(s$)-1
380 q=ASC(MID$(s$,i,1)):p=ASC(MID$(s$,i+1,1))
390 IF q>47 AND q<58 AND p>47 AND p<58 THEN PRINT "No forming of new numbers, please!":GOTO 150
400 NEXT
410 FOR i=1 TO 9:orig(i)=0:guess(i)=0:NEXT
420 FOR i=1 TO 4:orig(a(i))=orig(a(i))+1:NEXT
430 FOR i=1 TO LEN(s$)
440 v$=MID$(s$,i,1)
450 va=ASC(v$)-48
460 IF va>0 AND va<10 THEN guess(va)=guess(va)+1
470 NEXT
480 FOR i=1 TO 9
490 IF guess(i)<>orig(i) THEN PRINT "Only use all the provided digits!":GOTO 150
500 NEXT
510 GOTO 190
520 ' syntax error, e.g. non-matching parentheses
530 PRINT "Error in expression, please try again."
540 RESUME 150
```


Note: The program needs a writable disk in the active disk drive.


## Logo

*Works with: UCB_Logo 5.5*

```logo
; useful constants
make "false 1=0
make "true  1=1
make "lf char 10
make "sp char 32

; non-digits legal in expression
make "operators (lput sp [+ - * / \( \)])

; display help message
to show_help :digits
  type lf
  print sentence quoted [Using only these digits:] :digits
  print sentence quoted [and these operators:] [* / + -]
  print quoted [\(and parentheses as needed\),]
  print quoted [enter an arithmetic expression
     which evaluates to exactly 24.]
  type lf
  print quoted [Enter \"!\" to get fresh numbers.]
  print quoted [Enter \"q\" to quit.]
  type lf
end

make "digits []
make "done false
until [done] [

  if empty? digits [
    make "digits (map [(random 9) + 1] [1 2 3 4])
  ]

  (type "Solution sp "for sp digits "? sp )
  make "expression readrawline

  ifelse [expression = "?] [

    show_help digits

  ] [ifelse [expression = "q] [

    print "Bye!
    make "done true

  ] [ifelse [expression = "!] [

    make "digits []

  ] [
    make "exprchars ` expression
    make "allowed (sentence digits operators)

    ifelse (member? false (map [[c] member? c allowed] exprchars)) [
      (print quoted [Illegal character in input.])
    ] [
      catch "error [
        make "syntax_error true
        make "testval (run expression)
        make "syntax_error false
      ]
      ifelse syntax_error [
        (print quoted [Invalid expression.])
      ] [
        ifelse (testval = 24) [
          print quoted [You win!]
          make "done true
        ] [
          (print (sentence
            quoted [Incorrect \(got ] testval quoted [instead of 24\).]))
        ]
      ]
    ]
  ]]]
]
bye
```

### Output

```txt

Solution for 3 8 9 5? ?

Using only these digits: 3 8 9 5
and these operators: * / + -
(and parentheses as needed),
enter an arithmetic expression which evaluates to exactly 24.

Enter "!" to get fresh numbers.
Enter "q" to quit.

Solution for 3 8 9 5? !
Solution for 9 2 8 5? 9+2+8+5
You win!

```



## Lua


```lua

local function help()
	print [[
 The 24 Game

 Given any four digits in the range 1 to 9, which may have repetitions,
 Using just the +, -, *, and / operators; and the possible use of
 brackets, (), show how to make an answer of 24.

 An answer of "q" will quit the game.
 An answer of "!" will generate a new set of four digits.

 Note: you cannot form multiple digit numbers from the supplied digits,
 so an answer of 12+12 when given 1, 2, 2, and 1 would not be allowed.

 ]]
end

local function generate(n)
	result = {}
	for i=1,n do
		result[i] = math.random(1,9)
	end
	return result
end

local function check(answer, digits)
	local adig = {}
	local ddig = {}
	local index
	local lastWasDigit = false
	for i=1,9 do adig[i] = 0 ddig[i] = 0 end
	allowed = {['(']=true,[')']=true,[' ']=true,['+']=true,['-']=true,['*']=true,['/']=true,['\t']=true,['1']=true,['2']=true,['3']=true,['4']=true,['5']=true,['6']=true,['7']=true,['8']=true,['9']=true}
	for i=1,string.len(answer) do
		if not allowed[string.sub(answer,i,i)] then
			return false
		end
		index = string.byte(answer,i)-48
		if index > 0 and index < 10 then
			if lastWasDigit then
				return false
			end
			lastWasDigit = true
			adig[index] = adig[index] + 1
		else
			lastWasDigit = false
		end
	end
	for i,digit in next,digits do
		ddig[digit] = ddig[digit]+1
	end
	for i=1,9 do
		if adig[i] ~= ddig[i] then
			return false
		end
	end
	return loadstring('return '..answer)()
end

local function game24()
	help()
	math.randomseed(os.time())
	math.random()
	local digits = generate(4)
	local trial = 0
	local answer = 0
	local ans = false
	io.write 'Your four digits:'
	for i,digit in next,digits do
		io.write (' ' .. digit)
	end
	print()
	while ans ~= 24 do
		trial = trial + 1
		io.write("Expression "..trial..": ")
		answer = io.read()
		if string.lower(answer) == 'q' then
			break
		end
		if answer == '!' then
			digits = generate(4)
			io.write ("New digits:")
			for i,digit in next,digits do
				io.write (' ' .. digit)
			end
			print()
		else
			ans = check(answer,digits)
			if ans == false then
				print ('The input '.. answer ..' was wonky!')
			else
				print (' = '.. ans)
				if ans == 24 then
					print ("Thats right!")
				end
			end
		end
	end
end
game24()
```


Alternately, using the <code>lpeg.re</code> module:


```lua
function twentyfour()
   print [[
 The 24 Game

 Given any four digits in the range 1 to 9, which may have repetitions,
 Using just the +, -, *, and / operators; and the possible use of
 brackets, (), show how to make an answer of 24.

 An answer of "q" will quit the game.
 An answer of "!" will generate a new set of four digits.

 Note: you cannot form multiple digit numbers from the supplied digits,
 so an answer of 12+12 when given 1, 2, 2, and 1 would not be allowed.

 ]]
   expr = re.compile[[   --matches properly formatted infix expressions and returns all numerals as captures
         expr <- (!.) / (<paren> / <number>) (<ws> <oper> <ws> <expr>)?
         number <- {[0-9]}
         ws <- " "*
         oper <- [-+/*]
         paren <- "(" <ws> <expr> <ws> ")"   ]]
   local val_t = {math.random(9), math.random(9), math.random(9), math.random(9)}
   table.sort(val_t)
   print("the digits are " .. table.concat(val_t, ", "))
   local ex = io.read()
   a, b, c, d, e = expr:match(ex)
   if a and b and c and d and not e then --if there is a fifth numeral the player is cheating
      local digs = {a + 0, b + 0, c + 0, d + 0}
      local flag = false -- (terrorism!)
      table.sort(digs)
      for i = 1, 4 do
	   flag = digs[i] ~= val_t[i] and not print"Wrong digits!" or flag
      end
      if not flag and loadstring("return " .. ex)() == 24 then
         print"You win!"
      else
         print"You lose."
      end
   else print"wat" --expression could not be interpreted as arithmetic
   end
end
twentyfour()
```



## Maple

Click [here](http://maplecloud.maplesoft.com/application.jsp?appId=5764927761416192) to try this game online.

```maple
play24 := module()
	export ModuleApply;
	local cheating;
	cheating := proc(input, digits)
		local i, j, stringDigits;
		use StringTools in
			stringDigits := Implode([seq(convert(i, string), i in digits)]);
			for i in digits do
				for j in digits do
					if Search(cat(convert(i, string), j), input) > 0 then
						return true, ": Please don't combine digits to form another number."
					end if;
				end do;
			end do;
			for i in digits do
				if CountCharacterOccurrences(input, convert(i, string)) < CountCharacterOccurrences(stringDigits, convert(i, string)) then
					return true, ": Please use all digits.";
				end if;
			end do;
			for i in digits do
				if CountCharacterOccurrences(input, convert(i, string)) > CountCharacterOccurrences(stringDigits, convert(i, string)) then
					return true, ": Please only use a digit once.";
				end if;
			end do;
			for i in input do
				try
					if type(parse(i), numeric) and not member(parse(i), digits) then
						return true, ": Please only use the digits you were given.";
					end if;
				catch:
				end try;
			end do;
			return false, "";
		end use;
	end proc:

	ModuleApply := proc()
		local replay, digits, err, message, answer;
		randomize():
		replay := "":
		while not replay = "END" do
			if not replay = "YES" then
				digits := [seq(rand(1..9)(), i = 1..4)]:
			end if;
			err := true:
			while err do
				message := "";
				printf("Please make 24 from the digits: %a. Press enter for a new set of numbers or type END to quit\n", digits);
				answer := StringTools[UpperCase](readline());
				if not answer = "" and not answer = "END" then
					try
						if not type(parse(answer), numeric) then
							error;
						elif cheating(answer, digits)[1] then
							message := cheating(answer, digits)[2];
							error;
						end if;
						err := false;
					catch:
						printf("Invalid Input%s\n\n", message);
					end try;
				else
					err := false;
				end if;
			end do:
			if not answer = "" and not answer = "END" then
				if parse(answer) = 24 then
					printf("You win! Do you wish to play another game? (Press enter for a new set of numbers or END to quit.)\n");
					replay := StringTools[UpperCase](readline());
				else
					printf("Your expression evaluated to %a. Try again!\n", parse(answer));
					replay := "YES";
				end if;
			else
				replay := answer;
			end if;

			printf("\n");
		end do:
		printf("GAME OVER\n");
	end proc:
end module:

play24();
```

### Output

```txt

Please make 24 from the digits: [4, 7, 6, 8]. Press enter for a new set of numbers or type END to quit
You win! Do you wish to play another game? (Press enter for a new set of numbers or END to quit.)

Please make 24 from the digits: [3, 7, 4, 7]. Press enter for a new set of numbers or type END to quit
Your expression evaluated to 21. Try again!

Please make 24 from the digits: [3, 7, 4, 7]. Press enter for a new set of numbers or type END to quit

GAME OVER

```



## Mathematica

*Works with: Mathematica 6*

Since Mathematica hasn't historically had good custom I/O support (the command-line allowed *all* operations, not very good for UI-generation), I had to roll some custom GUI (with a text box), which requires Mathematica 6.

Most of the job is already done by Mathematica (the expression conversion); in fact, it is *too* good&mdash;it automatically converts ex. 3/4 to Times[3, Power[4, -1]], which we have to specifically test for so that real powers don't get through.


```Mathematica
isLegal[n_List, x_String] :=
 Quiet[Check[
   With[{h = ToExpression[x, StandardForm, HoldForm]},
    If[Cases[Level[h, {2, \[Infinity]}, Hold, Heads -> True],
        Except[_Integer | Plus | _Plus | Times | _Times | Power |
          Power[_, -1]]] === {} &&
      Sort[Level[h /. Power[q_, -1] -> q, {-1}] /.
         q_Integer -> Abs[q]] === Sort[n], ReleaseHold[h]]], Null]]
Grid[{{Button[
    "new numbers", {a, b, c, d} = Table[RandomInteger[{1, 9}], {4}]],
   InputField[Dynamic[x], String]}, {Dynamic[{a, b, c, d}],
   Dynamic[Switch[isLegal[{a, b, c, d}, x], Null,
     "Sorry, that is invalid.", 24, "Congrats! That's 24!", _,
     "Sorry, that makes " <> ToString[ToExpression@x, InputForm] <>
      ", not 24."]]}}]
```


## MATLAB / ## Octave


```Matlab
  function twentyfour()
  N = 4;
  n = ceil(rand(1,N)*9);
  printf('Generate a equation with the numbers %i, %i, %i, %i and +, -, *, /, () operators ! \n',n);
  s = input(': ','s');
  t = s;
  for k = 1:N,
    [x,t] = strtok(t,'+-*/() \t');
     if length(x)~=1,
       error('invalid sign %s\n',x);
     end;
     y = x-'0';
     if ~(0 < y & y < 10)
       error('invalid sign %s\n',x);
     end;
     z(1,k) = y;
  end;
  if any(sort(z)-sort(n))
    error('numbers do not match.\n');
  end;

  val =  eval(s);
  if val==24,
    fprintf('expression "%s" results in %i.\n',s,val);
  else
    fprintf('expression "%s" does not result in 24 but %i.\n',s,val);
  end;
```



## MiniScript

We use a simple recursive descent parser, with a bit of extra code to make sure that only available digits are used, and all of them are used.

```MiniScript
evalAddSub = function()
    result = evalMultDiv
    while true
        if not tokens then return result
        op = tokens[0]
        if op != "+" and op != "-" then return result
        tokens.pull  // (discard operator)
        rhs = evalMultDiv
        if result == null or rhs == null then return null
        if op == "+" then result = result + rhs
        if op == "-" then result = result - rhs
    end while
end function

evalMultDiv = function()
    result = evalAtom
    while true
        if not tokens then return result
        op = tokens[0]
        if op != "*" and op != "/" then return result
        tokens.pull  // (discard operator)
        rhs = evalAtom
        if result == null or rhs == null then return null
        if op == "*" then result = result * rhs
        if op == "/" then result = result / rhs
    end while
end function

evalAtom = function()
    if tokens[0] == "(" then
        tokens.pull
        result = evalAddSub
        if not tokens or tokens.pull != ")" then
            print "Unbalanced parantheses"
            return null
        end if
        return result
    end if
    num = val(tokens.pull)
    idx = availableDigits.indexOf(num)
    if idx == null then
        print str(num) + " is not available"
        return null
    else
        availableDigits.remove idx
    end if
    return num
end function

choices = []
for i in range(1, 4)
    choices.push ceil(rnd*9)
end for
result = null
while result != 24
    availableDigits = choices[0:]  // (clones the list)
    print "Using only the digits " + availableDigits + ","
    tokens = input("enter an expression that comes to 24: ").replace(" ","").values
    result = evalAddSub
    if availableDigits then
        print "You forgot to use: " + availableDigits
        result = null
    end if
    if result != null then print "That equals " + result + "."
end while
print "Great job!"
```


### Output

```txt
Using only the digits [5, 5, 2, 7],
enter an expression that comes to 24:
(7+5)*2
You forgot to use: [5]
Using only the digits [5, 5, 2, 7],
enter an expression that comes to 24:
5*7-5*2
That equals 25.
Using only the digits [5, 5, 2, 7],
enter an expression that comes to 24:
5+5+7*2
That equals 24.
Great job!
```



## mIRC Scripting Language


```mirc
alias 24 {
  dialog -m 24-Game 24-Game
}

dialog 24-Game {
  title "24-Game"
  size -1 -1 100 70
  option dbu
  text "", 1, 29 7 42 8
  text "Equation", 2, 20 21 21 8
  edit "", 3, 45 20 40 10
  text "Status", 4, 10 34 80 8, center
  button "Calculate", 5, 5 45 40 20
  button "New", 6, 57 47 35 15
}

on *:DIALOG:24-Game:init:*: {
  did -o 24-Game 1 1 Numbers: $rand(1,9) $rand(1,9) $rand(1,9) $rand(1,9)
}

on *:DIALOG:24-Game:sclick:*: {
  if ($did == 5) {
    if ($regex($did(3),/^[ (]*\d *[-+*/][ (]*\d[ ()]*[-+*/][ ()]*\d[ )]*[-+*/] *\d[ )]*$/)) && ($sorttok($regsubex($did(3),/[^\d]+/g,$chr(32)),32) == $sorttok($remove($did(1),Numbers:),32)) {
      did -o 24-Game 4 1 $iif($calc($did(3)) == 24,Correct,Wrong)
    }
    else {
      did -o 24-Game 4 1 Wrong Numbers or Syntax
    }
  }
  elseif ($did == 6) {
    did -o 24-Game 1 1 Numbers: $rand(1,9) $rand(1,9) $rand(1,9) $rand(1,9)
  }
}
```


## Modula-2
*Library: Ulm's Modula-2 Library*

```modula2
MODULE TwentyFour;

FROM	InOut			IMPORT WriteString, WriteLn, Write, ReadString, WriteInt;
FROM	RandomGenerator IMPORT Random;

TYPE	operator_t 		= (add, sub, mul, div);
		expr_t			= RECORD
							operand		: ARRAY[0..3] OF CARDINAL;
					  		operator 	: ARRAY[1..3] OF operator_t;
						  END;(*of RECORD*)
		numbers_t		= SET OF CHAR;

VAR		expr 	: expr_t;
		numbers	: numbers_t;
(*******************************************************************createExpr*)
(*analyse the input string                                                    *)
PROCEDURE createExpr(s: ARRAY OF CHAR);

VAR index, counter	: INTEGER;
	token 			: CHAR;
	temp_expr 		: expr_t;
	operand   		: CARDINAL;
	operator		: operator_t;

	(************************************nextToken*)
	(* returns the next CHAR that isn`t a space    *)
	PROCEDURE nextToken(): CHAR;
	BEGIN
		INC(index);
		WHILE (s[index] = ' ') DO
			INC(index);
		END;(*of WHILE*)
		RETURN(s[index]);
	END nextToken;
	(***********************************set_operand*)
	(* checks if the CHAR o inerhits a valid number*)
	(* and sets 'operand' to its value             *)
	PROCEDURE set_operand(o: CHAR);
	BEGIN
		CASE o OF
			'0'..'9':	IF o IN numbers THEN
							operand := ORD(o)-48;
							numbers := numbers - numbers_t{o};
						ELSE
							WriteString("ERROR: '");
							Write(					o);
							WriteString(				"' isn`t a available number ");
							WriteLn;
							HALT;
						END;(*of IF*)|
			0  : WriteString("ERROR: error in input ");
					WriteLn;
					HALT;
			ELSE
				WriteString("ERROR: '");
				Write(					o);
				WriteString(				"' isn`t a number ");
				WriteLn;
				HALT;
		END;(*of CASE*)
	END set_operand;
	(**********************************set_operator*)
	(* checks if the CHAR o inerhits a valid       *)
	(* operator and sets 'operator' to its value   *)
	PROCEDURE set_operator(o: CHAR);
	BEGIN
		CASE o OF
			'+' : operator := add;|
			'-' : operator := sub;|
			'*' : operator := mul;|
			'/' : operator := div;|
			0  : WriteString("ERROR: error in input ");
					WriteLn;
					HALT;
		ELSE
			WriteString("ERROR: '");
				Write(					o);
				WriteString(				"' isn`t a operator ");
				WriteLn;
				HALT;
		END;(*of CASE*)
	END set_operator;
	(************************************************)
BEGIN
	index := -1;

	token := nextToken();
	set_operand(token);
	expr.operand[0] := operand;

	token := nextToken();
	set_operator(token);
	expr.operator[1] := operator;


	token := nextToken();
	set_operand(token);
	expr.operand[1] := operand;

	token := nextToken();
	set_operator(token);
	expr.operator[2] := operator;

	token := nextToken();
	set_operand(token);
	expr.operand[2] := operand;

	token := nextToken();
	set_operator(token);
	expr.operator[3] := operator;

	token := nextToken();
	set_operand(token);
	expr.operand[3] := operand;
END createExpr;

(*****************************************************************evaluateExpr*)
(* evaluate the expresion that was createt by 'createExpr'                    *)

PROCEDURE evaluateExpr(VAR num: REAL);

VAR	index	: INTEGER;
BEGIN
	WITH expr DO
		num := VAL(REAL,operand[0]);
		FOR index := 1 TO 3 DO
			CASE operator[index] OF
				add : num := num + VAL(REAL,operand[index]);|
				sub : num := num - VAL(REAL,operand[index]);|
				mul : num := num * VAL(REAL,operand[index]);|
				div : num := num / VAL(REAL,operand[index]);
			END;(*of CASE*)
		END;(*of FOR*)
	END;(*of WITH*)
END evaluateExpr;

(**************************************************************generateNumbers*)
(* generates the 4 random numbers ond write them                              *)
PROCEDURE generateNumbers;
VAR	index,ran : INTEGER;
BEGIN
	numbers := numbers_t{};
	ran := Random(0,9);
	FOR index := 1 TO 4 DO
		WHILE (CHR(ran+48) IN numbers )DO
			ran := Random(0,9);
		END;(*of While*)
		Write(CHR(ran+48));
		WriteLn;
		numbers := numbers + numbers_t{CHR(ran+48)}
	END;(*of FOR*)
END generateNumbers;
(****************************************************************Main Programm*)
VAR	str : ARRAY[0..255] OF CHAR;
	sum : REAL;
BEGIN
	WriteString("Welcome to the 24 game in MODULA-2");
	WriteLn;
	WriteString("Here are your numbers:");
	WriteLn;
	generateNumbers;
	WriteString("Enter your equation(This implementation dosn`t support brackets yet): ");
	WriteLn;
	ReadString(str);
	createExpr(str);
	evaluateExpr(sum);
	WriteLn;
	WriteString("Result:");
	WriteLn;
	WriteInt(TRUNC(sum),0);
	WriteLn;
	CASE (TRUNC(sum) - 24) OF
		0 : WriteString("Perfect!");|
		1 : WriteString("Almost perfect.");
		ELSE
			WriteString("You loose!");
	END;(*of CASE*)
	WriteLn;
END TwentyFour.
```



## MUMPS


```mumps
24Game
	k number, operator, bracket
	; generate 4 random numbers each between 1 & 9
	; duplicates allowed!
	s n1=$r(9)+1, n2=$r(9)+1, n3=$r(9)+1, n4=$r(9)+1
	; save a copy of them so that we can keep restarting
	; if the user gets it wrong
	s s1=n1,s2=n2,s3=n3,s4=n4
Question
	s (numcount,opcount,lbrackcount,rbrackcount)=0
	; restart with the random numbers already found
	s n1=s1,n2=s2,n3=s3,n4=s4
	w !,"Enter an arithmetic expression that evaluates to 24 using (",
	n1," ",n2," ",n3," ",n4,"): "
	r !,expr
	q:expr=""
	; validate numbers and operators
	s error=""
	f n=1:1:$l(expr) {
		s char=$e(expr,n)
		if char?1n {
			s number($i(numcount))=char
			w !
			zw char
		}
		elseif char?1(1"*",1"/",1"+",1"-") {
			s operator($i(opcount))=char
		}
		elseif char?1"(" {
			s bracket($i(lbrackcount))=char
		}
		elseif char?1")" {
			s bracket($i(rbrackcount))=char
		}
		else {
			s error="That ain't no character I wanted to see"
			q
		}
	}
	if error'="" w error g Question
	if numcount'=4 {
		w "Does not have 4 numbers, do it again."
		g Question
	}
	s error=""
	f n=1:1:4 {
		if number(n)=n1 {
			s n1="dont use again" continue
		}
		if number(n)=n2 {
			s n2="dont use again" continue
		}
		if number(n)=n3 {
			s n3="dont use again" continue
		}
		if number(n)=n4 {
			s n4="dont use again" continue
		}
		s error="Numbers entered do not match all of the randomly generated numbers."
		q
	}
	if error'="" {
		w error
		g Question
	}
	if opcount'=3 {
		w "Does not have 3 operators."
		g Question
	}
	if lbrackcount'=rbrackcount {
		w "brackets must be in pairs."
		g Question
	}
	x "s x="_expr
	if x'=24 {
		w !,"Answer does not = 24"
		g Question
	}
	w x
	q

```



## Nim

{{trans|D}}

```nim
from random import randomize, random
from strutils import Whitespace
from algorithm import sort
from sequtils import deduplicate
randomize()

template newSeqWith(len: int, init: untyped): untyped =
  var result = newSeq[type(init)](len)
  for i in 0 .. <len:
    result[i] = init
  result

var
  problem = newSeqWith(4, random(1..9))
  stack = newSeq[float]()
  digits = newSeq[int]()

echo "Make 24 with the digits: ", problem

template op(c) =
  let a = stack.pop
  stack.add c(stack.pop, a)

for c in stdin.readLine:
  case c
  of '1'..'9':
    digits.add c.ord - '0'.ord
    stack.add float(c.ord - '0'.ord)
  of '+': op `+`
  of '*': op `*`
  of '-': op `-`
  of '/': op `/`
  of Whitespace: discard
  else: raise ValueError.newException "Wrong char: " & c

sort digits, cmp[int]
sort problem, cmp[int]
if digits.deduplicate != problem.deduplicate:
  raise ValueError.newException "Not using the given digits."
if stack.len != 1:
  raise ValueError.newException "Wrong expression."
echo "Result: ", stack[0]
echo if abs(stack[0] - 24) < 0.001: "Good job!" else: "Try again."
```

Example game:

```txt
Make 24 with the digits: @[8, 1, 3, 1]
8 3 * 1 + 1 -
Result: 24.0
Good job!
```



## Nit


Source: [https://github.com/nitlang/nit/blob/master/examples/rosettacode/24_game.nit the Nit’s official repository]


```nit
redef class Char
	fun is_op: Bool do return "-+/*".has(self)
end

# Get `numbers` and `operands` from string `operation` collect with `gets` in `main` function
# Fill `numbers` and `operands` array with previous extraction
fun exportation(operation: String, numbers: Array[Int], operands: Array[Char]) do
	var previous_char: nullable Char = null
	var number: nullable Int = null
	var negative = false

	for i in operation.length.times do
		var current_char = operation[i]
		var current_int = current_char.to_i

		if (previous_char == null or previous_char.is_op) and current_char == '-' then
			negative = true
			continue
		end

		if current_char.is_digit then
			if number == null then
				number = current_int
			else
				number = number * 10 + current_int
			end
		end

		if negative and (current_char.is_op or i == operation.length - 1) then
			number = number - number * 2
			negative = false
		end

		if (current_char.is_op or i == operation.length - 1) and number != null then
			numbers.add(number)
			number = null
		end

		if not negative and current_char.is_op then
			operands.add(current_char)
		end
		previous_char = current_char
	end
	# Update `numbers` and `operands` array in main function with pointer
end

# Create random numbers between 1 to 9
fun random: Array[Int] do
	return [for i in 4.times do 1 + 9.rand]
end

# Make mathematical operation with `numbers` and `operands` and add the operation result into `random_numbers`
fun calculation(random_numbers, numbers: Array[Int], operands: Array[Char]) do
	var number = 0
	var temp_numbers = numbers.clone

	while temp_numbers.length > 1 do
		var operand = operands.shift
		var a = temp_numbers.shift
		var b = temp_numbers.shift

		if operand == '+' then number = a + b
		if operand == '-' then number = a - b
		if operand == '*' then number = a * b
		if operand == '/' then number = a / b

		temp_numbers.unshift(number)
	end
	if number != 0 then random_numbers.add(number)
end

# Check if used `numbers` exist in the `random_numbers` created
fun numbers_exists(random_numbers, numbers: Array[Int]): Bool do
	for number in numbers do
		if not random_numbers.count(number) >= numbers.count(number) then return false
	end
	return true
end

# Remove `numbers` when they are used
fun remove_numbers(random_numbers, numbers: Array[Int]) do
	for number in numbers do random_numbers.remove(number)
end

# Check if the mathematical `operation` is valid
fun check(operation: String): Bool do
	var previous_char: nullable Char = null
	var next_char: nullable Char = null
	var next_1_char: nullable Char = null

	for i in operation.length.times do
		var current_char = operation[i]

		if i + 1 < operation.length then
			next_char = operation[i + 1]
			if i + 2 < operation.length then
				next_1_char = operation[i + 2]
			else
				next_1_char = null
			end
		else
			next_char = null
		end

		if not current_char.is_op and not current_char.is_digit then return false
		if next_char == null and current_char.is_op then return false

		if previous_char == null  then
			if next_char == null or next_1_char == null then return false
			if current_char == '-' and not next_char.is_digit then return false
			if current_char != '-' and not current_char.is_digit then return false
		else
			if next_char != null then
				if previous_char.is_digit and current_char.is_op and
				not (next_char == '-' and next_1_char != null and
				next_1_char.is_digit or next_char.is_digit) then
					return false
				end
			end
		end
		previous_char = current_char
	end
	return true
end

var random_numbers = new Array[Int]
var operation = ""

random_numbers = random
while not random_numbers.has(24) and random_numbers.length > 1 do
	var numbers = new Array[Int]
	var operands = new Array[Char]

	print "numbers: " + random_numbers.join(", ")
	operation = gets
	if check(operation) then
		exportation(operation, numbers, operands)
		if numbers_exists(random_numbers, numbers) then
			calculation(random_numbers, numbers, operands)
			remove_numbers(random_numbers, numbers)
		else
			print "NUMBERS ERROR!"
		end
	else
		print "STRING ERROR!"
	end
end

if random_numbers.has(24) then print "CONGRATULATIONS" else print "YOU LOSE"
```



## Objeck

{{trans|C++}}


```objeck
use Collection.Generic;
use System.Matrix;

class RPNParser {
  @stk : Stack<IntHolder>;
  @digits : List<IntHolder>;

  function : Main(args : String[]) ~ Nil {
    digits := List->New()<IntHolder>;
    "Make 24 with the digits: "->Print();
    for(i := 0; i < 4; i += 1;) {
      n : Int := Int->Random(1, 9);
        " {$n}"->Print();
        digits->AddBack(n);
    };
    '\n'->Print();

    parser := RPNParser->New();
    if(parser->Parse(System.IO.Console->ReadString(), digits)) {
      result := parser->GetResult();
      if(result = 24) {
        "Good job!"->PrintLine();
      }
      else {
        "{$result}, Try again."->PrintLine();
      };
    }
    else {
      "Invalid sequence"->PrintLine();
    };
  }

  New() {
    @stk := Stack->New()<IntHolder>;
    @digits := List->New()<IntHolder>;
  }

  method : Op(f : \Func->Calc) ~ Nil {
    if(@stk->Size() < 2) { "Improperly written expression"->ErrorLine(); Runtime->Exit(1); };
    b := @stk->Pop();
    a := @stk->Pop();
    @stk->Push(f(a, b));
  }

  method : Parse(c : Char) ~ Nil {
    if(c >= '0' & c <= '9') {
      value : Int := c - '0';
      @stk->Push(value);
      @digits->AddBack(value);
    }
    else if(c = '+') {
      Op(\Func->Calc : (a, b) => a + b);
    }
    else if(c = '-') {
      Op(\Func->Calc : (a, b) => a - b);
    }
    else if(c = '*') {
      Op(\Func->Calc : (a, b) => a * b);
    }
    else if(c = '/') {
      Op(\Func->Calc : (a, b) => { if(b <> 0) { return a / b;  } else { return 0; }; });
    };
  }

  method : GetResult() ~ Int {
    if(@stk->Size() = 1) {
      return @stk->Top();
    };

    return 0;
  }

  method : Parse(s : String, digits : List<IntHolder>) ~ Bool {
    each(i : s) {
      Parse(s->Get(i));
    };

    @digits->Rewind();
    while(@digits->More()) {
      left := @digits->Get()->Get();
      digits->Rewind();
      found := false;
      while(digits->More() & found = false) {
        right := digits->Get()->Get();
        if(left = right) {
          digits->Remove(); found := true;
        }
        else {
          digits->Next();
        };
      };
      @digits->Next();
    };

    return digits->IsEmpty();
  }
}

alias Func {
  Calc : (IntHolder, IntHolder) ~ IntHolder
}
```



## OCaml


Compile with:
 ocamlopt -pp camlp4o g24.ml -o g24.opt


```ocaml
type expression =
  | Const of float
  | Sum  of expression * expression   (* e1 + e2 *)
  | Diff of expression * expression   (* e1 - e2 *)
  | Prod of expression * expression   (* e1 * e2 *)
  | Quot of expression * expression   (* e1 / e2 *)

let rec eval = function
  | Const c -> c
  | Sum (f, g) -> eval f +. eval g
  | Diff(f, g) -> eval f -. eval g
  | Prod(f, g) -> eval f *. eval g
  | Quot(f, g) -> eval f /. eval g

let rec extract acc = function
  | Const c -> (c::acc)
  | Sum (f, g) -> (extract acc f) @ (extract [] g)
  | Diff(f, g) -> (extract acc f) @ (extract [] g)
  | Prod(f, g) -> (extract acc f) @ (extract [] g)
  | Quot(f, g) -> (extract acc f) @ (extract [] g)

open Genlex

let lexer = make_lexer ["("; ")"; "+"; "-"; "*"; "/"]

let rec parse_expr = parser
     [< e1 = parse_mult; e = parse_more_adds e1 >] -> e
 and parse_more_adds e1 = parser
     [< 'Kwd "+"; e2 = parse_mult; e = parse_more_adds (Sum(e1, e2)) >] -> e
   | [< 'Kwd "-"; e2 = parse_mult; e = parse_more_adds (Diff(e1, e2)) >] -> e
   | [< >] -> e1
 and parse_mult = parser
     [< e1 = parse_simple; e = parse_more_mults e1 >] -> e
 and parse_more_mults e1 = parser
     [< 'Kwd "*"; e2 = parse_simple; e = parse_more_mults (Prod(e1, e2)) >] -> e
   | [< 'Kwd "/"; e2 = parse_simple; e = parse_more_mults (Quot(e1, e2)) >] -> e
   | [< >] -> e1
 and parse_simple = parser
   | [< 'Int i >] -> Const(float i)
   | [< 'Float f >] -> Const f
   | [< 'Kwd "("; e = parse_expr; 'Kwd ")" >] -> e


let parse_expression = parser [< e = parse_expr; _ = Stream.empty >] -> e

let read_expression s = parse_expression(lexer(Stream.of_string s))


let () =
  Random.self_init();
  print_endline "
  The 24 Game

  Given any four digits in the range 1 to 9, which may have repetitions,
  Using just the +, -, *, and / operators; and the possible use of
  brackets, (), show how to make an answer of 24.

  An answer of 'q' will quit the game.
  An answer of '!' will generate a new set of four digits.
  Otherwise you are repeatedly asked for an expression until it evaluates to 24

  Note: you cannot form multiple digit numbers from the supplied digits,
  so an answer of 12+12 when given 1, 2, 2, and 1 would not be allowed.\n";

  let sort = List.sort compare in
  let digits = ref [] in
  let digit_set () =
    let ar = Array.init 4 (fun _ -> 1 + Random.int 9) in
    digits := Array.to_list(Array.map float_of_int ar);
    print_string "The four digits: ";
    List.iter (Printf.printf " %g") !digits;
    print_newline();
  in

  digit_set();
  while true do
    print_string "Expression: ";
    let str = read_line() in
    if str = "q" then exit 0;
    if str = "!" then digit_set()
    else begin
      let expr = read_expression str in
      let res = eval expr in
      Printf.printf " = %g\n%!" res;
      if res = 24.
      && (sort !digits) = (sort (extract [] expr))
      then (print_endline "Congratulations!"; digit_set())
      else print_endline "Try again"
    end
  done
```



## Oforth



```Oforth
import: mapping

: game
| l expr w n i |
   4 #[ 9 rand ] Array init ->l

   System.Out "Digits : " << l << " --> RPN Expression for 24 : " << drop
   System.Console accept ->expr

   expr words forEach: w [
      w "+" == ifTrue: [ + continue ]
      w "-" == ifTrue: [ - continue ]
      w "*" == ifTrue: [ * continue ]
      w "/" == ifTrue: [ >float / continue ]

      w >integer dup ->n  ifNull: [ System.Out "Word " << w << " not allowed " << cr break ]
      n l indexOf dup ->i ifNull: [ System.Out "Integer " << n << " is wrong " << cr break ]
      n l put(i, null)
      ]
   #null? l conform? ifFalse: [ "Sorry, all numbers must be used..." . return ]
   24 if=: [ "You won !" ] else: [ "You loose..." ] .
;
```



## ooRexx

While the solution shown within this page at [Rexx version 2](/tasks/#rexx_version2) was created for Classic Rexx it also can be used unchanged by the ooRexx interpreter and so can be considered a solution for the ooRexx language too.

Incompatibilities(*) that were originally in [Rexx version 1](/tasks/#rexx_version1) were meanwhile "fixed", so it also can be used unchanged by the ooRexx interpreter and so can be considered a solution for the ooRexx language too.

(*) Classic Rexx accepts assignment without an expression (x=;), ooRexx does not.


## OpenEdge/Progress

The dynamic query parser is used to evaluate the expression.
<lang Progress (OpenEdge ABL)>DEFINE TEMP-TABLE tt NO-UNDO FIELD ii AS INTEGER.

DEFINE VARIABLE p_deanswer    AS DECIMAL     NO-UNDO.
DEFINE VARIABLE idigits       AS INTEGER     NO-UNDO EXTENT 4.
DEFINE VARIABLE ii            AS INTEGER     NO-UNDO.
DEFINE VARIABLE Digits        AS CHARACTER   NO-UNDO FORMAT "x(7)".
DEFINE VARIABLE Answer        AS CHARACTER   NO-UNDO FORMAT "x(7)".
DEFINE VARIABLE cexpression   AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cmessage      AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cchar         AS CHARACTER   NO-UNDO.

FUNCTION calculate RETURNS LOGICAL (
   i_de AS DECIMAL
):
   p_deanswer = i_de.
END FUNCTION.

/* generate problem */
DO ii = 1 TO 4:
   ASSIGN
      idigits [ii]   =  RANDOM( 1, 9 ).
      Digits         =  Digits + STRING( idigits [ii] ) + " "
      .
END.

/* ui */
DISPLAY Digits.
UPDATE Answer.

/* check valid input */
DO ii = 1 TO 7:
   cchar = SUBSTRING( Answer, ii, 1 ).
   IF cchar > "" THEN DO:
      IF ii MODULO 2 = 1 THEN DO:
         IF LOOKUP( cchar, Digits, " " ) = 0 THEN
            cmessage = cmessage + SUBSTITUTE( "Invalid digit: &1.~r", cchar ).
         ELSE
            ENTRY( LOOKUP( cchar, Digits, " " ), Digits, " " ) = "".
      END.
      ELSE DO:
         IF LOOKUP( cchar, "+,-,/,*" ) = 0 THEN
            cmessage = cmessage + SUBSTITUTE( "&1 is not a valid operator.~r", cchar ).
      END.
   END.
END.
IF TRIM( Digits ) > "" THEN
   cmessage = cmessage + SUBSTITUTE( "You did not use digits: &1":U, TRIM( Digits ) ).

IF cmessage = "" THEN DO:
   /* expressions need spacing */
   DO ii = 1 TO 7:
      cexpression = cexpression + SUBSTRING( Answer, ii, 1 ) + " ".
   END.
   /* use dynamic query to parse expression */
   TEMP-TABLE tt:DEFAULT-BUFFER-HANDLE:FIND-FIRST(
      SUBSTITUTE(
         "WHERE NOT DYNAMIC-FUNCTION( 'calculate', DECIMAL( &1 ) )",
         cexpression
      )
   ) NO-ERROR.
   IF p_deanswer <> 24 THEN
      cmessage = cmessage + SUBSTITUTE( "The expression evaluates to &1.", p_deanswer ).
   ELSE
      cmessage = "Solved!".
END.

MESSAGE cmessage VIEW-AS ALERT-BOX.

```



## PARI/GP

{{untested}}

```parigp
game()={
  my(v=vecsort(vector(4,i,random(8)+1)));
  print("Form 24 using */+-() and: "v);
  while(1,
    my(ans=input);
    if (!valid(s,v), next);
    trap(,
      print("Arithmetic error");
      next
    ,
      if(eval(s)==24, break, print("Bad sum"))
    )
  );
  print("You win!")
};
valid(s,v)={
  my(op=vecsort(Vec("+-*/()")),u=[]);
  s=Vec(s);
  for(i=1,#s,
    if(setsearch(op,s[i]),next);
    trap(,
      print("Invalid character "s[i]);
      return(0)
    ,
      if(setsearch(v,eval(s[i])),
        u=concat(u,eval(s[i]))
      ,
        print(s[i]" not allowed");
        return(0)
      )
    )
  );
  for(i=2,#s,
    if(!setsearch(op,s[i])&!setsearch(op,s[i-1]),
      print("Concatenating digits is not allowed!");
      return(0)
    )
  );
  if(vecsort(u)!=v,
    print("Invalid digits");
    0
  ,
    1
  )
};
```



## Perl


```perl
#!/usr/bin/env perl
use warnings;
use strict;
use feature 'say';

print <<'EOF';
The 24 Game

Given any four digits in the range 1 to 9, which may have repetitions,
Using just the +, -, *, and / operators; and the possible use of
parentheses, (), show how to make an answer of 24.

An answer of "q" or EOF will quit the game.
A blank answer will generate a new set of four digits.
Otherwise you are repeatedly asked for an expression until it evaluates to 24.

Note: you cannot form multiple digit numbers from the supplied digits,
so an answer of 12+12 when given 1, 2, 2, and 1 would not be allowed.
EOF

my $try = 1;
while (1) {
  my @digits = map { 1+int(rand(9)) } 1..4;
  say "\nYour four digits: ", join(" ", @digits);
  print "Expression (try ", $try++, "): ";

  my $entry = <>;
  if (!defined $entry || $entry eq 'q')
    { say "Goodbye.  Sorry you couldn't win."; last; }
  $entry =~ s/\s+//g;  # remove all white space
  next if $entry eq '';

  my $given_digits = join "", sort @digits;
  my $entry_digits = join "", sort grep { /\d/ } split(//, $entry);
  if ($given_digits ne $entry_digits ||  # not correct digits
      $entry =~ /\d\d/ ||                # combined digits
      $entry =~ m|[-+*/]{2}| ||          # combined operators
      $entry =~ tr|-0-9()+*/||c)         # Invalid characters
    { say "That's not valid";  next; }

  my $n = eval $entry;

  if    (!defined $n) { say "Invalid expression"; }
  elsif ($n == 24)    { say "You win!"; last; }
  else                { say "Sorry, your expression is $n, not 24"; }
}
```



## Perl 6


*Works with: Rakudo 2015.12*

```perl6
use MONKEY-SEE-NO-EVAL;

say "Here are your digits: ",
constant @digits = (1..9).roll(4)».Str;

grammar Exp24 {
    token TOP { ^ <exp> $ { fail unless EVAL($/) == 24 } }
    rule exp { <term>+ % <op> }
    rule term { '(' <exp> ')' | <@digits> }
    token op { < + - * / > }
}

while my $exp = prompt "\n24? " {
    if try Exp24.parse: $exp {
        say "You win :)";
        last;
    } else {
        say (
            'Sorry.  Try again.' xx 20,
            'Try harder.' xx 5,
            'Nope.  Not even close.' xx 2,
            'Are you five or something?',
            'Come on, you can do better than that.'
        ).flat.pick
    }
}

```


The <code>MONKEY-SEE-NO-EVAL</code> pragma enables the dangerous <code>EVAL</code> function, which will compile and execute even user input. In this example, the grammar used to parse the input should ensure that only safe expressions are evaluated.


## Phix



```Phix

-- Note this uses simple/strict left association, so for example:
--  1+2*1*8 is ((1+2)*1)*8 not 1+((2*1)*8) [or 1+(2*(1*8))], and
--  7-(2*2)*8 is (7-(2*2))*8 not 7-((2*2)*8)
--  Does not allow unary minus on the first digit.
-- Uses solve24() from the next task, when it can.
--  (you may want to comment out the last 2 lines/uncomment the if 0, in that file)
--
--include 24_game_solve.exw

--with trace
forward function eval(string equation, sequence unused, integer idx=1)
-- (the above definition is entirely optional, but good coding style)

constant errorcodes = {"digit expected",                    -- 1
                       "')' expected",                      -- 2
                       "digit already used",                -- 3
                       "digit not offered",                 -- 4
                       "operand expected"}                  -- 5

function card(integer idx)  -- (for error handling)
    if idx=1 then return "1st" end if
    if idx=2 then return "2nd" end if
    -- (assumes expression is less than 21 characters)
    return sprintf("%dth",idx)
end function

function errorchar(sequence equation, integer idx)
    if idx>length(equation) then return "" end if
    return sprintf("(%s)",equation[idx])
end function

sequence rset = repeat(0,4)

procedure new_rset()
    for i=1 to length(rset) do
        rset[i] = rand(9)
    end for
end procedure

function get_operand(string equation, integer idx, sequence unused)
integer ch, k,
        error = 1 -- "digit expected"
atom res

    if idx<=length(equation) then
        ch = equation[idx]
        if ch='(' then
            {error,res,unused,idx} = eval(equation,unused,idx+1)
            if error=0
            and idx<=length(equation) then
                ch = equation[idx]
                if ch=')' then
                    return {0,res,unused,idx+1}
                end if
            end if
            if error=0 then
                error = 2   -- "')' expected"
            end if
        elsif ch>='0' and ch<='9' then
            res = ch-'0'
            k = find(res,unused)
            if k!=0 then
                unused[k..k] = {}
                return {0,res,unused,idx+1}
            end if
            if find(res,rset) then
                error = 3  -- "digit already used"
            else
                error = 4  -- "digit not offered"
            end if
        end if
    end if
    return {error,0,unused,idx}
end function

function get_operator(string equation, integer idx)
integer ch, error = 5 -- "operand expected"
    if idx<=length(equation) then
        ch = equation[idx]
        if find(ch,"+-/*") then
            return {0,ch,idx+1}
        end if
    end if
    return {error,0,idx}
end function

function eval(string equation, sequence unused, integer idx=1)
atom lhs, rhs
integer ch, error
    {error,lhs,unused,idx} = get_operand(equation,idx,unused)
    if error=0 then
        while 1 do
            {error,ch,idx} = get_operator(equation,idx)
            if error!=0 then exit end if
            {error,rhs,unused,idx} = get_operand(equation,idx,unused)
            if error!=0 then exit end if
            if    ch='+' then       lhs += rhs
            elsif ch='-' then       lhs -= rhs
            elsif ch='/' then       lhs /= rhs
            elsif ch='*' then       lhs *= rhs
            else ?9/0 -- (should not happen)
            end if
            if idx>length(equation) then
                return {0,lhs,unused,idx}
            end if
            ch = equation[idx]
            if ch=')' then
                return {0,lhs,unused,idx}
            end if
        end while
    end if
    return {error,0,unused,idx}
end function

function strip(string equation)
    for i=length(equation) to 1 by -1 do
        if find(equation[i]," \t\r\n") then
            equation[i..i] = ""
        end if
    end for
    return equation
end function

function strip0(atom a) -- (for error handling)
string res = sprintf("%f",a)
integer ch
    for i=length(res) to 2 by -1 do
        ch = res[i]
        if ch='.' then return res[1..i-1] end if
        if ch!='0' then return res[1..i] end if
    end for
    return res
end function

procedure play()
sequence unused
string equation
integer error,idx
atom res

    new_rset()
    printf(1,"Enter an expression which evaluates to exactly 24\n"&
           "Use all of, and only, the digits %d, %d, %d, and %d\n"&
           "You may only use the operators + - * /\n"&
           "Parentheses and spaces are allowed\n",rset)
    while 1 do
        equation = strip(gets(0))
        if upper(equation)="Q" then exit end if
        if equation="?" then
            puts(1,"\n")
            integer r_solve24 = routine_id("solve24") -- see below
            if r_solve24=-1 then -- (someone copied just this code out?)
                puts(1,"no solve24 routine\n")
            else
                call_proc(r_solve24,{rset})
            end if
        else
            {error,res,unused,idx} = eval(equation, rset)
            if error!=0 then
                printf(1,"  %s on the %s character%s\n",{errorcodes[error],card(idx),errorchar(equation,idx)})
            elsif idx<=length(equation) then
                printf(1,"\neval() returned only having processed %d of %d characters\n",{idx,length(equation)})
            elsif length(unused) then
                printf(1," not all the digits were used\n",error)
            elsif res!=24 then
                printf(1,"\nresult is %s, not 24\n",{strip0(res)})
            else
                puts(1," correct!  Press any key to quit\n")
                if getc(0) then end if
                exit
            end if
        end if
        puts(1,"enter Q to give up and quit\n")
    end while
end procedure

    play()

```



## PHP

{{trans|Perl}}

```PHP
#!/usr/bin/env php
The 24 Game

Given any four digits in the range 1 to 9, which may have repetitions,
Using just the +, -, *, and / operators; and the possible use of
brackets, (), show how to make an answer of 24.

An answer of "q" will quit the game.
An answer of "!" will generate a new set of four digits.
Otherwise you are repeatedly asked for an expression until it evaluates to 24

Note: you cannot form multiple digit numbers from the supplied digits,
so an answer of 12+12 when given 1, 2, 2, and 1 would not be allowed.

<?php

while (true) {
    $numbers = make_numbers();

    for ($iteration_num = 1; ; $iteration_num++) {
        echo "Expresion $iteration_num: ";

        $entry = rtrim(fgets(STDIN));

        if ($entry === '!') break;
        if ($entry === 'q') exit;

        $result = play($numbers, $entry);

        if ($result === null) {
            echo "That's not valid\n";
            continue;
        }
        elseif ($result != 24) {
            echo "Sorry, that's $result\n";
            continue;
        }
        else {
            echo "That's right! 24!!\n";
            exit;
        }
    }
}

function make_numbers() {
    $numbers = array();

    echo "Your four digits: ";

    for ($i = 0; $i < 4; $i++) {
        $number = rand(1, 9);
        // The check is needed to avoid E_NOTICE from PHP
        if (!isset($numbers[$number])) {
            $numbers[$number] = 0;
        }
        $numbers[$number]++;
        print "$number ";
    }

    print "\n";

    return $numbers;
}

function play($numbers, $expression) {
    $operator = true;
    for ($i = 0, $length = strlen($expression); $i < $length; $i++) {
        $character = $expression[$i];

        if (in_array($character, array('(', ')', ' ', "\t"))) continue;

        $operator = !$operator;

        if (!$operator) {
            if (!empty($numbers[$character])) {
                $numbers[$character]--;
                continue;
            }
            return;
        }
        elseif (!in_array($character, array('+', '-', '*', '/'))) {
            return;
        }
    }

    foreach ($numbers as $remaining) {
        if ($remaining > 0) {
            return;
        }
    }

    return eval("return $expression;");
}
?>
```



## PicoLisp


```PicoLisp
(de checkExpression (Lst Exe)
   (make
      (when (diff Lst (fish num? Exe))
         (link "Not all numbers used" ) )
      (when (diff (fish num? Exe) Lst)
         (link "Using wrong number(s)") )
      (when (diff (fish sym? Exe) '(+ - * /))
         (link "Using illegal operator(s)") ) ) )

(loop
   (setq Numbers (make (do 4 (link (rand 1 9)))))
   (prinl
      "Please enter a Lisp expression using (, ), +, -, *, / and "
      (glue ", " Numbers) )
   (prin "Or a single dot '.' to stop: ")
   (T (= "." (setq Reply (catch '(NIL) (in NIL (read)))))
      (bye) )
   (cond
      ((str? Reply)
         (prinl "-- Input error: " Reply) )
      ((checkExpression Numbers Reply)
         (prinl "-- Illegal Expression")
         (for S @
            (space 3)
            (prinl S) ) )
      ((str? (setq Result (catch '(NIL) (eval Reply))))
         (prinl "-- Evaluation error: " @) )
      ((= 24 Result)
         (prinl "++ Congratulations! Correct result :-)") )
      (T (prinl "Sorry, this gives " Result)) )
   (prinl) )
```

### Output

```txt
Please enter a Lisp expression using (, ), +, -, *, / and 1, 3, 3, 5
Or a single dot '.' to stop: (* (+ 3 1) (+ 5 1))
++ Congratulations! Correct result :-)

Please enter a Lisp expression using (, ), +, -, *, / and 8, 4, 7, 1
Or a single dot '.' to stop: (* 8 (% 7 3) 9)
-- Illegal Expression
   Not all numbers used
   Using wrong number(s)
   Using illegal operator(s)

Please enter a Lisp expression using (, ), +, -, *, / and 4, 2, 2, 3
Or a single dot '.' to stop: (/ (+ 4 3) (- 2 2))
-- Evaluation error: Div/0

Please enter a Lisp expression using (, ), +, -, *, / and 8, 4, 5, 9
Or a single dot '.' to stop: .
```



## PL/I


```pli

/* Plays the game of 24. */

TWENTYFOUR: procedure options (main);            /* 14 August 2010 */

CTP: procedure (E) returns (character(50) varying);
   declare E character (*) varying;
   declare OUT character (length(E)) varying;
   declare S character (length(E)) varying controlled;
   declare c character (1);
   declare i fixed binary;

/* This procedure converts an arithmetic expression to Reverse Polish Form. */
/* A push-down pop-up stack is used for operators. */
priority: procedure (a) returns (fixed decimal (1));
   declare a character (1);
   declare ops character (10) initial ('#+-*/') varying static;
   declare pri(6) fixed decimal (1) initial (1,2,2,3,3,4) static;
   declare i fixed binary;

   i = index(ops,a);
   return (pri(i));
end priority;

   allocate S; S = '#'; out = '';
   do i = 1 to length(E);
      c = substr(E, i, 1);
      if index('+-*/', c) > 0 then
         do;
            /* Copy any higher priority operators on the stack to the output. */
            do while ( priority(c) <= priority((S)) );
               out = out || S;
               free S;
            end;
            /* Copy the input character to the stack. */
            allocate S; S = c;
         end;

      if index('123456789', c) > 0 then
         out = out || c;
   end;
   do while (allocation(S) > 1);
      out = out || s;
      free S;
   end;
   return (out);
end CTP;

/* Given a push-down pop-up stack, and an expresion in  */
/* Reverse Polish notation, evaluate the expression.    */
EVAL: procedure (E) returns (fixed decimal(15));
   declare E character (*) varying;
   declare S fixed decimal (15) controlled;
   declare (a, b) fixed decimal (15);
   declare c character (1);
   declare p fixed binary;
   declare (empty_stack, invalid_expression) condition;

   on condition (empty_stack) begin;
      put skip list ('Your expression is not valid.');
      stop;
   end;
   on condition (invalid_expression) begin;
      put skip list ('Your expression is not valid.');
      stop;
   end;

   do p = 1 to length(E);
      c = substr(E, p, 1);
      if index('123456789', c) > 0 then
         do; allocate S; S = c; end;
      else
         do;
            if allocation(S) = 0 then signal condition (empty_stack);
            b = S; free S;
            if allocation(S) = 0 then signal condition (empty_stack);
            a = S;
            select (c);
               when ('+') S = a + b;
               when ('-') S = a - b;
               when ('*') S = a * b;
               when ('/') S = a / b;
               when ('^') S = a ** b;
               otherwise signal condition (invalid_expression);
            end;
         end;
   end;
   if allocation(S) ^= 1 then signal condition (invalid_expression);
   return (S);
END eval;

/* Check that the player has used every digit and no others. */
VALIDATE: procedure (E);
   declare E character (*) varying;
   declare E2 character (length(E)), (i, j) fixed binary;
   declare digits(9) character (1) static initial
      ('1', '2', '3', '4', '5', '6', '7', '8', '9');

   E2 = translate(E, '    ', '+-*/' );
   do i = 1 to 4;
      j = index(E2, digits(k(i)));
      if j > 0 then
         substr(E2, j, 1) = ' ';
      else
         do; put skip list ('You must use the digits supplied.'); stop; end;
   end;
   if E2 ^= '' then
      do; put skip list ('You must use every digit supplied, and no others.'); stop; end;
end VALIDATE;

   declare E character (40) varying;
   declare k(4) fixed decimal;
   declare (time, random) builtin;
   declare V fixed decimal (15);

   k = random(TIME);
   k = 9*random() + 1;
   put skip edit ('Here are four integers:', k) (a);
   put skip list ('With these integers, make up an arithmetic expression' ||
      ' that evaluates to 24.');
   put skip list ('You can use any of the operators +, -, *, and /');
   put skip list ('E.g., Given the integers 1, 3, 7, and 6,' ||
      ' the expression 6*3+7-1 evaluates to 24.');

   put skip list ('Please type an arithmetic expression :');
   get edit (E) (L) COPY;

   CALL VALIDATE (E); /* Check that the player has used every digit and no others. */

   E = CTP(E);
   V = EVAL (E);
   if V = 24 then
      put skip list ('Congratulations: the expression evaluates to 24.');
   else
      put skip edit ('The result is ', trim(V), ' which is not correct') (a);

end TWENTYFOUR;

```



## Potion


```Potion
is_num = (s):
  x = s ord(0)
  if (x >= "0"ord && x <= "9"ord): true.
  else: false.
  .

nums = (s):
  res = ()
  0 to (s length, (b):
    c = s(b)
    if (is_num(c)):
      res push(c).
  .)
  res.

try = 1
while (true):
  r = rand string
  digits = (r(0),r(1),r(2),r(3))
  "\nMy next four digits: " print
  digits join(" ") say
  digit_s = digits ins_sort string

  ("Your expression to create 24 (try ", try, "): ") print
  entry = read slice(0,-1)
  expr = entry eval
  parse = nums(entry)
  parse_s = parse clone ins_sort string
  try++
  if (parse length != 4):
    ("Wrong number of digits:", parse) say.
  elsif (parse_s != digit_s):
    ("Wrong digits:", parse) say.
  elsif (expr == 24):
    "You won!" say
    entry print, " => 24" say
    return().
  else:
    (entry, " => ", expr string, " != 24") join("") say.
.
```



## PowerShell

The "isNumeric" function was taken from the "Determine_if_a_string_is_numeric" task.

todo: add a validation that all given digits were used. Right now the validation is that 4 digits should be used in the expression, but not exactly the ones given. (example: if you are given the digits 2, 2, 6, 9 this program accepts the following solution: 6 * **4** * 2 / 2)


```powershell

CLS

Function isNumeric ($x)
{
    $x2 = 0
    $isNum = [System.Int32]::TryParse($x,[ref]$x2)
Return $isNum
}

$NumberArray = @()
While( $NumberArray.Count -lt 4 ){
    $NumberArray += Random -Minimum 1 -Maximum 10
}

Write-Host @"
Welcome to the 24 game!

Here are your numbers: $($NumberArray -join ",").
Use division, multiplication, subtraction and addition to get 24 as a result with these 4 numbers.
"@

Do
{
$Wrong = 0
$EndResult = $null
$TempChar = $null
$TempChar2 = $null
$Count = $null

$AllowableCharacters = $NumberArray + "+-*/()".ToCharArray()
    $Result = Read-Host
        Foreach($Char in $Result.ToCharArray())
        {
            If( $AllowableCharacters -notcontains $Char ){ $Wrong = 1 }
        }

        If($Wrong -eq 1)
        {
            Write-Warning "Wrong input! Please use only the given numbers."
        }
        Foreach($Char in $Result.ToCharArray())
        {
            If((IsNumeric $TempChar) -AND (IsNumeric $Char))
            {
                Write-Warning "Wrong input! Combining two or more numbers together is not allowed!"
            }
            $TempChar = $Char
        }
        Foreach($Char in $Result.ToCharArray())
        {
            If(IsNumeric $Char)
            {
                $Count++
            }
        }
        If($Count -eq 4)
        {
            $EndResult = Invoke-Expression $Result
                If($EndResult -eq 24)
                {
                    Write-Host "`nYou've won the game!"
                }
                Else
                {
                    Write-Host "`n$EndResult is not 24! Too bad."
                }
        }
        Else
        {
            Write-Warning "Wrong input! You did not supply four numbers."
        }
}
While($EndResult -ne 24)

```



## ProDOS

This example uses the math module:

```ProDOS
:a
editvar /modify -random- = <10
printline These are your four digits: -random- -random- -random- -random-
printline Use an algorithm to make the number 24.
editvar /newvar /value=a /userinput=1 /title=Algorithm:
do -a-
if -a- /hasvalue 24 printline Your algorithm worked! & goto :b (
) else printline Your algorithm did not work.
:b
editvar /newvar /value=b /userinput=1 /title=Do you want to play again?
if -b- /hasvalue y goto :a else exitcurrentprogram
```


## Prolog
{{Works with|GNU Prolog}}

```prolog
:- initialization(main).


answer(24).
play :- round, play ; true.

round :-
    prompt(Ns), get_line(Input), Input \= "stop"
  , ( phrase(parse(Ns,[]), Input) -> Result = 'correct'
                                   ; Result = 'wrong'
    ), write(Result), nl, nl
  . % where
    prompt(Ns)  :- length(Ns,4), maplist(random(1,10), Ns)
                 , write('Digits: '), write(Ns), nl
                 .

parse([],[X])     --> { answer(X) }.
parse(Ns,[Y,X|S]) --> "+", { Z is X  +  Y }, parse(Ns,[Z|S]).
parse(Ns,[Y,X|S]) --> "-", { Z is X  -  Y }, parse(Ns,[Z|S]).
parse(Ns,[Y,X|S]) --> "*", { Z is X  *  Y }, parse(Ns,[Z|S]).
parse(Ns,[Y,X|S]) --> "/", { Z is X div Y }, parse(Ns,[Z|S]).
parse(Ns,Stack)   --> " ", parse(Ns,Stack).
parse(Ns,Stack)   --> { select(N,Ns,Ns1), number_codes(N,[Code]) }
                    , [Code], parse(Ns1,[N|Stack])
                    .

get_line(Xs) :- get_code(X)
              , ( X == 10 -> Xs = [] ; Xs = [X|Ys], get_line(Ys) )
              .
main :- randomize, play, halt.
```

Example "session":

```txt
Digits: [9,4,6,9]
46*9-9+
correct

Digits: [7,4,7,8]
8 4 7 7 / - *
correct

Digits: [7,2,8,2]
7282---
wrong

Digits: [2,6,7,1]
4611***
wrong

Digits: [3,6,5,8]
+
wrong

Digits: [2,1,7,7]
stop
```



## PureBasic


```PureBasic
#digitCount = 4
Global Dim digits(#digitCount - 1) ;holds random digits

Procedure showDigits()
  Print(#CRLF$ + "These are your four digits: ")
  Protected i
  For i = 0 To #digitCount - 1
    Print(Str(digits(i)))
    If i < (#digitCount - 1)
      Print(", ")
    Else
      PrintN("")
    EndIf
  Next
  Print("24 = ")
EndProcedure

Procedure playAgain()
  Protected answer.s
  Repeat
    Print("Play again (y/n)? ")
    answer = LCase(Left(Trim(Input()), 1))
    Select answer
      Case "n"
        ProcedureReturn #False
      Case "y"
        ProcedureReturn #True
      Default
        PrintN("")
        Continue
    EndSelect
  ForEver
EndProcedure

Procedure allDigitsUsed()
  Protected i
  For i = 0 To #digitCount - 1
    If digits(i) <> 0
      ProcedureReturn #False
    EndIf
  Next
  ProcedureReturn #True
EndProcedure

Procedure isValidDigit(d)
  For i = 0 To #digitCount - 1
    If digits(i) = d
      digits(i) = 0
      ProcedureReturn #True
    EndIf
  Next
  ProcedureReturn #False
EndProcedure

Procedure doOperation(List op.c(), List operand.f())
  Protected x.f, y.f, op.c
  op = op(): DeleteElement(op())
  If op = '('
    ProcedureReturn #False ;end of sub-expression
  EndIf

  y = operand(): DeleteElement(operand())
  x = operand()
  Select op
    Case '+'
      x + y
    Case '-'
      x - y
    Case '*'
      x * y
    Case '/'
      x / y
  EndSelect
  operand() = x
  ProcedureReturn #True ;operation completed
EndProcedure

;returns error if present and the expression results in *result\f
Procedure.s parseExpression(expr.s, *result.Float)
  NewList op.c()
  NewList operand.f()
  expr = ReplaceString(expr, " ", "") ;remove spaces

  If Len(expr) = 0: *result\f = 0: ProcedureReturn "": EndIf ;no expression, return zero

  Protected *ech.Character = @expr, lastWasDigit, lastWasOper, parenCheck, c.c
  While *ech\c
    c = *ech\c
    Select c
      Case '*', '/', '-', '+'
        If Not lastWasDigit: ProcedureReturn "Improper syntax, need a digit between operators.": EndIf
        If ListSize(op()) And (FindString("*/", Chr(op()), 1) Or (FindString("+-", Chr(op()), 1) And FindString("+-", Chr(c), 1)))
          doOperation(op(), operand())
        EndIf
        AddElement(op()): op() = c
        lastWasOper = #True: lastWasDigit = #False
      Case '('
        If lastWasDigit: ProcedureReturn "Improper syntax, need an operator before left paren.": EndIf
        AddElement(op()): op() = c
        parenCheck + 1: lastWasOper = #False
      Case ')'
        parenCheck - 1: If parenCheck < 0: ProcedureReturn "Improper syntax, missing a left paren.": EndIf
        If Not lastWasDigit: ProcedureReturn "Improper syntax, missing a digit before right paren.": EndIf
        Repeat: Until Not doOperation(op(),operand())
        lastWasDigit = #True
      Case '1' To '9'
        If lastWasDigit: ProcedureReturn "Improper syntax, need an operator between digits.": EndIf
        AddElement(operand()): operand() = c - '0'
        If Not isValidDigit(operand()): ProcedureReturn "'" + Chr(c) + "' is not a valid digit.": EndIf
        lastWasDigit = #True: lastWasOper = #False
      Default
        ProcedureReturn "'" + Chr(c) + "' is not allowed in the expression."
    EndSelect
    *ech + SizeOf(Character)
  Wend

  If parenCheck <> 0 Or lastWasOper: ProcedureReturn "Improper syntax, missing a right paren or digit.": EndIf
  Repeat
    If Not ListSize(op()): Break: EndIf
  Until Not doOperation(op(),operand())
  *result\f = operand()
  ProcedureReturn "" ;no error
EndProcedure

Define success, failure, result.f, error.s, i
If OpenConsole()
  PrintN("The 24 Game" + #CRLF$)
  PrintN("Given four digits and using just the +, -, *, and / operators; and the")
  PrintN("possible use of brackets, (), enter an expression that equates to 24.")
  Repeat
    For i = 0 To #digitCount - 1
      digits(i) = 1 + Random(8)
    Next

    showDigits()
    error = parseExpression(Input(), @result)
    If error = ""
      If Not allDigitsUsed()
        PrintN( "Wrong! (you didn't use all digits)"): failure + 1
      ElseIf result = 24.0
        PrintN("Correct!"): success + 1
      Else
        Print("Wrong! (you got ")
        If result <> Int(result)
          PrintN(StrF(result, 2) + ")")
        Else
          PrintN(Str(result) + ")")
        EndIf
        failure + 1
      EndIf
    Else
      PrintN(error): failure + 1
    EndIf
  Until Not playAgain()

  PrintN("success:" + Str(success) + " failure:" + Str(failure) + " total:" + Str(success + failure))

  Print(#CRLF$ + "Press ENTER to exit"): Input()
  CloseConsole()
EndIf
```

### Output

```txt
The 24 Game

Given four digits and using just the +, -, *, and / operators; and the
possible use of brackets, (), enter an expression that equates to 24.

These are your four digits: 9, 2, 8, 7
24 = 9*8/2-7
Wrong! (you got 29)
Play again (y/n)? y

These are your four digits: 5, 5, 5, 6
24 = 5*5+5-6
Correct!
Play again (y/n)? n
success:1 failure:1 total:2
```



## Python

### Python: Original, with output
Uses eval, the built-in expression evaluator of infix expressions.

```python
'''
 The 24 Game

 Given any four digits in the range 1 to 9, which may have repetitions,
 Using just the +, -, *, and / operators; and the possible use of
 brackets, (), show how to make an answer of 24.

 An answer of "q" will quit the game.
 An answer of "!" will generate a new set of four digits.
 Otherwise you are repeatedly asked for an expression until it evaluates to 24

 Note: you cannot form multiple digit numbers from the supplied digits,
 so an answer of 12+12 when given 1, 2, 2, and 1 would not be allowed.

'''

from __future__ import division, print_function
import random, ast, re
import sys

if sys.version_info[0] < 3: input = raw_input

def choose4():
    'four random digits >0 as characters'
    return [str(random.randint(1,9)) for i in range(4)]

def welcome(digits):
    print (__doc__)
    print ("Your four digits: " + ' '.join(digits))

def check(answer, digits):
    allowed = set('() +-*/\t'+''.join(digits))
    ok = all(ch in allowed for ch in answer) and \
         all(digits.count(dig) == answer.count(dig) for dig in set(digits)) \
         and not re.search('\d\d', answer)
    if ok:
        try:
            ast.parse(answer)
        except:
            ok = False
    return ok

def main():
    digits = choose4()
    welcome(digits)
    trial = 0
    answer = ''
    chk = ans = False
    while not (chk and ans == 24):
        trial +=1
        answer = input("Expression %i: " % trial)
        chk = check(answer, digits)
        if answer.lower() == 'q':
            break
        if answer == '!':
            digits = choose4()
            print ("New digits:", ' '.join(digits))
            continue
        if not chk:
            print ("The input '%s' was wonky!" % answer)
        else:
            ans = eval(answer)
            print (" = ", ans)
            if ans == 24:
                print ("Thats right!")
    print ("Thank you and goodbye")

if __name__ == '__main__': main()
```


### Output

```txt

 The 24 Game

 Given any four digits in the range 1 to 9, which may have repetitions,
 Using just the +, -, *, and / operators; and the possible use of
 brackets, (), show how to make an answer of 24.

 An answer of "q" will quit the game.
 An answer of "!" will generate a new set of four digits.

 Note: you cannot form multiple digit numbers from the supplied digits,
 so an answer of 12+12 when given 1, 2, 2, and 1 would not be allowed.


Your four digits: 3 2 4 6
Expression 1: (3 - 1)*(6*4)
The input '(3 - 1)*(6*4)' was wonky!
Expression 2: (3 - 2) * 6 * 4
 =  24
That's right!
Thank you and goodbye.
```



### Python: Alternative


```python
import random, re
chars = ["(",")","/","+","-","*"]
while True:
    charsandints, ints = [], []
    for x in range(4):
        ints.append(str(random.randrange(1,10)))
    charsandints = chars + ints
    print "Numbers are:", ints
    guess = raw_input("Enter your guess:")
    if guess.lower() == "q":
        break
    elif guess.lower() == "|":
        pass
    else:
        flag = True
        for a in guess:
            if a not in charsandints or guess.count(a) > charsandints.count(a):
                flag = False
        if re.search("\d\d", guess):
            print "You cannot combine digits."
            break
        if flag:
            print "Your result is: ", eval(guess)
            if eval(guess) == 24:
                print "You won"
                break
            else:
                print "You lost"
                break
        else:
            print "You cannot use anthing other than", charsandints
            break
print "Thanks for playing"
```



## R

This makes use of R's metaprogramming (parse, eval, etc.). It uses parse to obtain a parse tree, which is scanned for containing only permitted elements before evaluating.


```r
twenty.four <- function(operators=c("+", "-", "*", "/", "("),
                        selector=function() sample(1:9, 4, replace=TRUE),
                        arguments=selector(),
                        goal=24) {
  newdigits <- function() {
    arguments <<- selector()
    cat("New digits:", paste(arguments, collapse=", "), "\n")
  }
  help <- function() cat("Make", goal,
      "out of the numbers",paste(arguments, collapse=", "),
      "and the operators",paste(operators, collapse=", "), ".",
      "\nEnter 'q' to quit, '!' to select new digits,",
      "or '?' to repeat this message.\n")
  help()
  repeat {
    switch(input <- readline(prompt="> "),
           q={ cat("Goodbye!\n"); break },
           `?`=help(),
           `!`=newdigits(),
           tryCatch({
             expr <- parse(text=input, n=1)[[1]]
             check.call(expr, operators, arguments)
             result <- eval(expr)
             if (isTRUE(all.equal(result, goal))) {
               cat("Correct!\n")
               newdigits()
             } else {
               cat("Evaluated to", result, "( goal", goal, ")\n")
             }
           },error=function(e) cat(e$message, "\n")))
  }
}

check.call <- function(expr, operators, arguments) {
  unexpr <- function(x) {
    if (is.call(x))
      unexpr(as.list(x))
    else if (is.list(x))
      lapply(x,unexpr)
    else x
  }
  leaves <- unlist(unexpr(expr))
  if (any(disallowed <-
          !leaves %in% c(lapply(operators, as.name),
                         as.list(arguments)))) {
    stop("'", paste(sapply(leaves[disallowed], as.character),
                    collapse=", "),
         "' not allowed. ")
  }
  numbers.used <- unlist(leaves[sapply(leaves, mode) == 'numeric'])

  if (! isTRUE(all.equal(sort(numbers.used), sort(arguments))))
   stop("Must use each number once.")
}
```

Example Session

```r>
 twenty.four()

Make 24 out of the numbers 1, 6, 7, 5 and the operators +, -, *, /, ( .
Enter 'q' to quit, '!' to select new digits, or '?' to repeat this message.
> 6*(5-1)
Must use each number once.
> 1 + 6*5 - 7
Correct!
New digits: 7, 2, 9, 3
> (7+9)/2*3
Correct!
New digits: 1, 4, 1, 7
> 4*(7-1)
Must use each number once.
> (7-1)*4*1
Correct!
New digits: 1, 5, 2, 8
> (5-1)^2+8
'^' not allowed.
> !
New digits: 2, 8, 5, 2
> 52-28
'52, 28' not allowed.
> (8-2)*(5-2/2)
Must use each number once.
> (8+2)*2+5
Evaluated to 25 ( goal 24 )
> q
Goodbye!

```



## Racket


The functional interpreter of an expression given in the infix form.
It parses the S-expression representing the user's answer and handles
invalid cases.


```racket

#lang racket

(define (interprete expr numbers)
  ;; the cashe for used numbers
  (define cashe numbers)

  ;; updating the cashe and handling invalid cases
  (define (update-cashe! x)
    (unless (member x numbers) (error "Number is not in the given set:" x))
    (unless (member x cashe)   (error "Number is used more times then it was given:" x))
    (set! cashe (remq x cashe)))

  ;; the parser
  (define parse
    (match-lambda
      ;; parsing arythmetics
      [`(,x ... + ,y ...) (+ (parse x) (parse y))]
      [`(,x ... - ,y ...) (- (parse x) (parse y))]
      [`(,x ... * ,y ...) (* (parse x) (parse y))]
      [`(,x ... / ,y ...) (/ (parse x) (parse y))]
      [`(,x ,op ,y ...)   (error "Unknown operator: " op)]
      ;; opening redundant brackets
      [`(,expr)           (parse expr)]
      ;; parsing numbers
      [(? number? x)      (update-cashe! x) x]
      ;; unknown token
      [x                  (error "Not a number: " x)]))

  ;; parse the expresion
  (define result (parse expr))

  ;; return the result if cashe is empty
  (if (empty? cashe)
      result
      (error "You didn`t use all numbers!")))

```


Testing the interpreter:

```txt

> (interprete '(1 - 2 * 3 + 8) '(1 2 3 8))
3
> (interprete '(1 - 2 * (3 + 8)) '(1 2 3 8))
-21
> (interprete '((1 - 2) * (3 + 8)) '(1 2 3 8))
-11
> (interprete '((1 - 2) * 3 + 8) '(1 2 3 8))
5
> (interprete '((1 - 2) * 3 + 8) '(1 2 3 4))
 Number is not in the given set: 8
> (interprete '((1 - 2) * 3 + 2) '(1 2 3 3))
 Number is used more times then it was given: 2
> (interprete '((1 - 2) ^ 3 + 2) '(1 2 3 2))
 Unknown operator:  ^
> (interprete '((1 - 2) * 3) '(1 2 3 2))
 You didn`t use all numbers!

```


The program which uses the interpreter to play the game:


```racket

;; starting the program
(define (start)
  (displayln "Combine given four numbers using operations + - * / to get 24.")
  (displayln "Input 'q' to quit or your answer like '1 - 3 * (2 + 3)'")
  (new-game))

;; starting a new game
(define (new-game)
  ;; create a new number set
  (define numbers (build-list 4 (λ (_) (+ 1 (random 9)))))
  (apply printf "Your numbers: ~a  ~a  ~a  ~a\n" numbers)
  (new-input numbers))

;; processing a new user input
(define (new-input numbers)
  ;; if an exception is raized while processing, show the exeption message
  ;; and prompt for another input, but do not stop the program.
  (with-handlers ([exn? (λ (exn)
                          (displayln (exn-message exn))
                          (new-input numbers))])
    ;; get the answer
    (define user-expr (read-the-answer))
    ;; interprete it
    (case user-expr
      [(q) (display "Good buy!")]
      [(n) (new-game)]
      [else (define ans (interprete user-expr numbers))
            (case ans
              [(24) (printf "Indeed! ~a = 24\n" user-expr)
                    (new-game)]
              [else (error "Wrong!" user-expr '= ans)])])))

;; reading and preparing the user's answer
;; "1 + 2 * (3 + 4)" --> '(1 + 2 * (3 + 4))
(define (read-the-answer)
  (read (open-input-string (format "(~a)" (read-line)))))

```



## Red



```Red

Red []
print "Evaluation from left to right with no precedence, unless you use parenthesis." print ""
a: "123456789"
guess: ""
valid: ""
sucess: false
random/seed now/time
loop 4 [append valid last random a]
print ["The numbers are: " valid/1 ", " valid/2 ", " valid/3 " and " valid/4]
sort valid
insert valid " "

expr:    [term ["+" | "-"] expr | term]
term:    [primary ["*" | "/"] term | primary]
primary: [some digit | "(" expr ")"]
digit:   charset valid

while [not sucess] [
	guess: ask "Enter your expression: "
	if guess = "q" [halt]
	numbers: copy guess
	sort numbers
	numbers: take/last/part numbers 4
	insert numbers " "
	either (parse guess expr) and (valid = numbers) [
		repeat i length? guess [insert at guess (i * 2) " "]
		result: do guess
		print ["The result of your expression is: " result]
		if (result = 24) [sucess: true]
	][
	print "Something is wrong with the expression, try again."
	]
]
print "You got it right!"


```

Output:

```txt

Evaluation from left to right with no precedence, unless you use parenthesis.

The numbers are:  7 ,  1 ,  9  and  3
Enter your expression: 7+1+9
Something is wrong with the expression, try again.
Enter your expression: 7+1+8+3
Something is wrong with the expression, try again.
Enter your expression: 1+7*(9/3)
The result of your expression is:  24
You got it right!
>>

```



## REXX


### version 1

A large part of this program deals with validating the user input   (and issuing appropriate and meaningful error

messages),   and also that the digits presented to the user, do in fact, have a possible solution.

This REXX version uses an in─line documentation (for help).


```rexx
/*REXX program helps the user find solutions to the game of  24.
                                 start─of─help
╔═════════════════════════════════════════════════════════════════════════════╗
║ Argument is either of these forms:    (blank)                               ║~
║                                        ssss                                 ║~
║                                        ssss,total,limit                     ║~
║                                        ssss-ffff                            ║~
║                                        ssss-ffff,total,limit                ║~
║                                       -ssss                                 ║~
║                                       +ssss                                 ║~
║                                                                             ║~
║ where   SSSS   and/or   FFFF  must be exactly four numerals (digits)        ║~
║ comprised soley of the numerals (digits)   1 ──> 9     (no zeroes).         ║~
║                                                                             ║~
║         SSSS   is the start,   and     FFFF    is the  end   (inclusive).   ║~
║                                                                             ║~
║ If  ssss  has a leading plus (+) sign,  it is used as the digits,  and      ║~
║ the user is prompted to enter a solution  (using those decimal digits).     ║~
║                                                                             ║~
║ If  ssss  has a leading minus (-) sign,  a solution is looked for and       ║~
║ the user is told there is a solution (or not), but no solutions are shown). ║~
║                                                                             ║~
║ If no argument is specified, this program generates four digits  (no zeros) ║~
║ which has at least one solution,  and shows the sorted digits to the user,  ║~
║ requesting that they enter a solution (the digits used may be in any order).║~
║                                                                             ║~
║ If   TOTAL   is entered,  it is the desired answer.   The default is  24.   ║~
║ If   LIMIT   is entered,  it limits the number of solutions shown.          ║~
║                                                                             ║~
║ A solution to be entered can be in the form of:                             ║
║                                                                             ║
║    digit1   operator   digit2   operator   digit3   operator  digit4        ║
║                                                                             ║
║ where    DIGITn     is one of the digits shown  (in any order),   and       ║
║          OPERATOR   can be any one of:     +   -   *   /                    ║
║                                                                             ║
║ Parentheses  ()  may be used in the normal manner for grouping,  as well as ║
║ brackets  []  or  braces  {}.      Blanks can be used anywhere.             ║
║                                                                             ║
║ I.E.:  for the digits   3448   the following could be entered:  3*8 + (4-4) ║
╚═════════════════════════════════════════════════════════════════════════════╝
                                    end─of─help                                         */
numeric digits 12                                /*where rational arithmetic is needed. */
parse arg orig;              uargs= orig         /*get the  guess  from the command line*/
orig= space(orig, 0)                             /*remove all blanks from  ORIG.        */
negatory= left(orig,1)=='-'                      /*=1, suppresses showing.              */
pository= left(orig,1)=='+'                      /*=1, force $24 to use specific number.*/
if pository | negatory  then orig=substr(orig,2) /*now, just use the absolute vaue.     */
parse var orig orig  ','   $  ","  limit         /*get optional total ($)  and/or  limit*/
parse var orig start '-' finish                  /*get start and finish  (maybe).       */
opers= '*' || "/+-"                              /*arithmetic opers; order is important.*/
ops= length(opers)                               /*the number of arithmetic operators.  */
groupsym= space('  ( )   [ ]   { }   « »  ',  0) /*the allowable grouping symbols.      */
indent= left('', 30)                             /*indents the display of solutions.    */
show= 1                                          /*=1,  shows solutions  (a semifore).  */
digs= 123456789                                  /*numerals/digits that can be used.    */
abuttals = 0                                     /*=1,  allows digit abutal:  12+12     */
if $==''      then $= 24                         /*the name of the game:  (24)          */
if limit==''  then limit= 1                      /*=1,  shows only  one  solution.      */
      do j=1  for ops;   o.j=substr(opers, j, 1) /*these are used for fast execution.   */
      end  /*j*/
if \datatype(limit, 'N')   then do;  call ger  limit  "isn't numeric";   exit 13;    end
limit= limit / 1                                 /*normalize the number for limit.      */
if \datatype($, 'N')       then do;  call ger    $    "isn't numeric";   exit 13;    end
$= $ / 1                                         /*normalize the number for total.      */
if start\=='' & \pository  then do;  call ranger start,finish;           exit 1;     end
show= 0                                          /*stop blabbing solutions in SOLVE.    */
        do forever  while  \negatory             /*keep truckin' until a solution.      */
        x.= 0                                    /*way to hold unique expressions.      */
        rrrr= random(1111, 9999)                 /*get a random set of digits.          */
        if pos(0, rrrr)\==0  then iterate        /*but don't the use of zeroes.         */
        if solve(rrrr)\==0  then leave           /*try to solve for these digits.       */
        end   /*forever*/
show= 1                                          /*enable SOLVE to show solutions.      */
if pository  then rrrr=start                     /*use what's specified.                */
rrrr= sortc(rrrr)                                /*sort four elements.                  */
rd.= 0
                do j=1  for 9;    _= substr(rrrr, j, 1);         rd._= #chars(rrrr, _)
                end   /*j*/                      /* [↑]  count for each digit in  RRRR. */
  do guesses=1;                 say;        @prompt= copies('─', 8)    "Using the digits "
  say @prompt rrrr",  enter an expression that equals"   $   '        (or  ?  or  QUIT):'
  pull y;        uargs= y;      y= space(y, 0)   /*obtain the user's response.          */
  if abbrev('QUIT', y, 1)  then exit 0           /*does the user want to quit this game?*/
  helpstart= 0
  if y=='?'  then do j=1  for sourceline();   _= sourceline(j)  /*get a line of program.*/
                  if p(_)=='start─of─help'          then do;  helpstart= 1;  iterate;  end
                  if p(_)=='end─of─help'            then iterate guesses
                  if \helpstart | right(_, 1)=='~'  then iterate
                  say '  ' _
                  end   /*j*/                    /* [↑]  use an in─line way to show help*/
  _v= verify(y, digs || opers || groupsym)       /*any illegal characters?              */
  if _v\==0  then do;   call ger 'invalid character:'  substr(y, _v, 1);   iterate;    end
  if   y=''  then do;   call validate y;   iterate;    end

    do j=1  for length(y)-1  while \abuttals     /*check for two adjacent decimal digits*/
    if datatype( substr(y, j, 1), 'W')   &   datatype( substr(y, j+1, 1), 'W')  then
                                do;  call ger 'invalid use of digit abuttal' substr(y,j,2)
                                     iterate guesses
                                end
    end   /*j*/

  yd= #chars(y, digs)                            /*count of legal digits  123456789     */
  if yd<4  then do;  call ger 'not enough digits entered.'; iterate guesses; end
  if yd>4  then do;  call ger 'too many digits entered.'  ; iterate guesses; end

      do j=1  for length(groupsym)  by 2
      if #chars(y,substr(groupsym,j  ,1))\==,
         #chars(y,substr(groupsym,j+1,1))  then do;      @mis= 'mismatched'
                                                call ger @mis  substr(groupsym, j, 2)
                                                iterate guesses
                                                end
      end   /*j*/

        do k=1  for 2                            /*check for   **    and    //          */
        _= copies( substr( opers, k, 1), 2)      /*only examine the first two operators.*/
        if pos(_, y)\==0  then do;  call ger 'illegal operator:' _;  iterate guesses;  end
        end   /*k*/

    do j=1  for 9;    if rd.j==0  then iterate;     _d= #chars(y, j)
    if _d==rd.j  then iterate
    if _d<rd.j   then call ger  'not enough'   j   "digits, must be"   rd.j
                 else call ger  'too many'     j   "digits, must be"   rd.j
    iterate guesses
    end   /*j*/

  y= translate(y, '()()', "[]{}");       interpret  'ans=('  y   ") / 1"
  if ans==$  then leave guesses;         say right('incorrect, ' y'='ans, 50)
  end   /*guesses*/

say;      say center('┌─────────────────────┐', 79)
          say center('│                     │', 79)
          say center('│  congratulations !  │', 79)
          say center('│                     │', 79)
          say center('└─────────────────────┘', 79);     say
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
#chars: procedure; parse arg x,c; return length(x) -length( space( translate(x, ,c ), 0) )
div:  procedure; parse arg q; if q=0  then q=1e9; return q  /*tests if dividing by zero.*/
ger:  say '***error*** for argument: ' uargs;  say '      ' arg(1);  errCode= 1;  return 0
p:    return word( arg(1), 1)
s:    if arg(1)==1  then return arg(3);           return word( arg(2) 's', 1)
/*──────────────────────────────────────────────────────────────────────────────────────*/
ranger: parse arg ssss,ffff                      /*parse args passed to this sub.       */
        ffff= p(ffff ssss)                       /*create a   FFFF   if necessary.      */
              do g=ssss  to ffff                 /*process possible range of values.    */
              if pos(0, g)\==0  then iterate     /*ignore any   G   with zeroes.        */
              sols= solve(g);  wols= sols
              if sols==0  then wols= 'No'        /*un─geek number of solutions (if any).*/
              if negatory & sols\==0  then wols= 'A'   /*found only the first solution? */
              if sols==1  & limit==1  then wols= 'A'
              say;            say wols   'solution's(sols)    "found for"    g
              if $\==24  then say  'for answers that equal'    $
              end   /*g*/
       return
/*──────────────────────────────────────────────────────────────────────────────────────*/
solve: parse arg qqqq;  finds= 0;   x.=0         /*parse args passed to this function.  */
if \validate(qqqq)  then return -1
parse  value  '( (( )) )'   with   L  LL  RR  R  /*assign some static variables.        */
nq.= 0
           do jq=1  for 4;  _= substr(qqqq,jq,1) /*count the   number   of each digit.  */
           nq._= nq._ + 1
           end   /*jq*/
                    gLO= 1111;  gHI= 9999
if $==24  then do;  gLO= 1118;  gHI= 9993;  end  /*24:  lowest poss.# that has solutions*/

  do gggg=gLO  to gHI;  if pos(0, gggg)\==0  then iterate   /*ignore values with zeroes.*/
  if verify(gggg, qqqq)\==0  then iterate
  if verify(qqqq, gggg)\==0  then iterate
  ng.= 0
         do jg=1  for 4;  _= substr(gggg, jg, 1);       g.jg= _;            ng._= ng._ + 1
         end   /*jg*/                            /* [↑]  count the number of each digit.*/
                          do kg=1  for 9;   if nq.kg\==ng.kg  then iterate gggg
                          end   /*kg*/           /* [↑]  verify number has same # digits*/
    do i    =1  for ops                          /*insert operator after 1st numeral.   */
      do j  =1  for ops                          /*  "        "      "   2nd    "       */
        do k=1  for ops                          /*  "        "      "   3rd    "       */
          do m=0  for 10;       !.=              /*nullify all grouping symbols (parens)*/
            select                               /*used to generate grouped expressions.*/
            when m==1  then do; !.1=L;           !.3=R;                                end
            when m==2  then do; !.1=L;                              !.5=R;             end
            when m==3  then do; !.1=L;           !.3=R;   !.4=L;              !.6=R;   end
            when m==4  then do;          !.2=L;                     !.5=R;             end
            when m==5  then do;          !.2=L;                               !.6=R;   end
            when m==6  then do; !.1=LL;                             !.5=R;    !.6=R;   end
            when m==7  then do;          !.2=LL;                    !.5=R;    !.6=R;   end
            when m==8  then do; !.1=L;   !.2=L;                               !.6=RR;  end
            when m==9  then do;          !.2=L;           !.4=L;              !.6=RR;  end
            otherwise  nop
            end   /*select*/

          e= space(!.1 g.1 o.i      !.2 g.2 !.3 o.j      !.4 g.3 !.5 o.k       g.4 !.6, 0)
          if x.e  then iterate                   /*was the expression already used?     */
          x.e= 1                                 /*mark this expression as being used.  */
          origE= e                               /*keep original version for the display*/
          pd= pos('/(', e)                       /*find pos of     /(      in  E.       */
          if pd\==0  then do                     /*Found?  Might have possible ÷ by zero*/
                          eo= e
                          lr= lastpos(')', e)    /*find last right )   */
                          lm= pos('-', e, pd+1)  /*find a minus sign (-)    after  (    */
                          if lm>pd & lm<lr  then e= changestr('/(',e,"/div(")   /*change*/
                          if eo\==e then if x.e  then iterate /*expression already used?*/
                          x.e= 1                 /*mark this expression as being used.  */
                          end
          interpret 'x=('   e   ") / 1"          /*have REXX do the heavy lifting here. */
          if x\==$  then do                      /*Not correct?   Then try again.       */
                         numeric digits 9;    x= x / 1               /*re─do evaluation.*/
                         numeric digits 12                           /*re─instate digits*/
                         if x\==$  then iterate  /*Not correct?   Then try again.       */
                         end
          finds= finds + 1                       /*bump number of found solutions.      */
          if \show | negatory  then return finds
          _= translate(origE, '][', ")(")                      /*display  [],  not  (). */
          if show  then say indent   'a solution for'  gggg':'  $"="  _ /*show solution.*/
          if limit==1 & finds==limit  then leave gggg                   /*leave big loop*/
          end     /*m*/
        end       /*k*/
      end         /*j*/
    end           /*i*/
  end             /*gggg*/
return finds
/*──────────────────────────────────────────────────────────────────────────────────────*/
sortc: procedure;  arg nnnn;           @.=        /*sorts the digits of   NNNN          */
              do i=1  for length(nnnn); _= substr(nnnn, i, 1);   @._= @._||_;   end  /*i*/
       return @.0 || @.1 || @.2 || @.3 || @.4 || @.5 || @.6 || @.7 || @.8 || @.9
/*──────────────────────────────────────────────────────────────────────────────────────*/
validate: parse arg y;      errCode= 0;         _v= verify(y, digs)
                   select
                   when y==''         then call ger 'no digits entered.'
                   when length(y)<4   then call ger 'not enough digits entered, must be 4'
                   when length(y)>4   then call ger 'too many digits entered, must be 4'
                   when pos(0,y)\==0  then call ger "can't use the digit  0 (zero)"
                   when _v\==0        then call ger 'illegal character:'  substr(y, _v, 1)
                   otherwise               nop
                   end   /*select*/
          return \errCode
```

Some older REXXes don't have a   **changestr**   BIF,   so one is included here   ──►   [CHANGESTR.REX](/tasks/CHANGESTR.REX).

### output|text=  of a sample execution:

```txt

──────── Using the digits  1689,  enter an expression that equals 24         (or  ?  or  QUIT):
8 * {9-6} / 1               ◄■■■■■■■■■■■■■■ this is the user input.

                            ┌─────────────────────┐
                            │                     │
                            │  congratulations !  │
                            │                     │
                            └─────────────────────┘

```



### version 2

The above was created for Classic Rexx and can be used, unchanged, for Regina and other implementations of Classic Rexx.
After recent changes it can also be used, unchanged, for ooRexx.

Therefore I removed the code of the 24 game's Version 2 for Rexx.

As regards TSO (German code page):
I wrote me a little program that just translates \ and | to ^ and ! in the source (except for literals and comments)
and version 1 (Gerard's) of the 24 game works,
if translated that way and otherwise unchanged, on TSO. TSO supports the A=;
For the sake of my 80 column CLIST PDS I had to split the few lines that are longer than that.

**Result:**

```txt

                               ISPF Command Shell
 Enter TSO or Workstation commands below:
  ===> tso h24
Using the digits 5559 , enter an expression that equals 24 (or QUIT):
5+5+5+9

                             +---------------------+
                             :                     :
                             :  congratulations !  :
                             :                     :
                             +---------------------+

```


I just leave the complete version of changestr here although it has nothing to do with 24!

```rexx
changestr: Procedure
/* change needle to newneedle in haystack (as often as specified      */
/* or all of them if count is omitted                                 */
  Parse Arg needle,haystack,newneedle,count
  If count>'' Then Do
    If count=0 Then Do
      Say 'chstr count must be > 0'
      Signal Syntax
      End
    End
  res=""
  changes=0
  px=1
  do Until py=0
    py=pos(needle,haystack,px)
    if py>0 then Do
      res=res||substr(haystack,px,py-px)||newneedle
      px=py+length(needle)
      changes=changes+1
      If count>'' Then
        If changes=count Then Leave
      End
    end
  res=res||substr(haystack,px)
  Return res
```



## Ring


```ring

# Project : 24 game

load "stdlib.ring"
digits = list(4)
check = list(4)
for choice = 1 to 4
     digits[choice] = random(9)
next

see "enter an equation (using all of, and only, the single digits " + nl
for index = 1 to 4
     see digits[index]
     if index != 4
        see " "
    ok
next
see ")"
see " which evaluates to exactly 24. only multiplication (*), division (/)," + nl
see "addition (+) & subtraction (-) operations and parentheses are allowed:" + nl
see "24 = "
give equation
see "equation = " + equation + nl

while true
        for char = 1 to len(equation)
             digit = substr("0123456789", equation[char]) - 1
             if digit >= 0
                for index = 1 to 4
                     if digit = digits[index]
                        if not check[index]
                           check[index] = 1
                           exit
                        ok
                     ok
                next
                if index > 4
                   see "sorry, you used the illegal digit " + digit + nl
                   exit 2
                ok
            ok
        next
        for index = 1 to 4
             if check[index] = 0
                see "sorry, you failed to use the digit " + digits[index] + nl
                exit 2
             ok
        next
        for pair = 11 to 99
             if substr(equation, string(pair))
                see "sorry, you may not use a pair of digits " + pair + nl
             ok
        next
        eval("result = " + equation)
        if result = 24
           see "congratulations, you succeeded in the task!" + nl
           exit
        else
           see "sorry, your equation evaluated to " + result + " rather than 24!" + nl
        ok
end

```

Output:

```txt

enter an equation (using all of, and only, the single digits
3 1 4 5) which evaluates to exactly 24. only multiplication (*), division (/),
addition (+) & subtraction (-) operations and parentheses are allowed:
24 = 4*5+3+1
equation = 4*5+3+1
congratulations, you succeeded in the task!

enter an equation (using all of, and only, the single digits
5 7 2 5) which evaluates to exactly 24. only multiplication (*), division (/),
addition (+) & subtraction (-) operations and parentheses are allowed:
24 = 98+72
equation = 98+72
sorry, you used the illegal digit 9

```



## Ruby


```ruby
class Guess < String
  def self.play
    nums = Array.new(4){rand(1..9)}
    loop do
      result = get(nums).evaluate!
      break if result == 24.0
      puts "Try again! That gives #{result}!"
    end
    puts "You win!"
  end

  def self.get(nums)
    loop do
      print "\nEnter a guess using #{nums}: "
      input = gets.chomp
      return new(input) if validate(input, nums)
    end
  end

  def self.validate(guess, nums)
    name, error =
      {
        invalid_character:  ->(str){ !str.scan(%r{[^\d\s()+*/-]}).empty? },
        wrong_number:       ->(str){ str.scan(/\d/).map(&:to_i).sort != nums.sort },
        multi_digit_number: ->(str){ str.match(/\d\d/) }
      }
        .find {|name, validator| validator[guess] }

    error ? puts("Invalid input of a(n) #{name.to_s.tr('_',' ')}!") : true
  end

  def evaluate!
    as_rat = gsub(/(\d)/, '\1r')        # r : Rational suffix
    eval "(#{as_rat}).to_f"
  rescue SyntaxError
    "[syntax error]"
  end
end

Guess.play
```


## Rust

The solution below converts the infix notation to RPN and then calculates the result.

I am still new to Rust so i am certain it could be written in a shorter way.
So if there is someone better than me please feel free to improve.
*Library: rand*

```rust
use std::io::{self,BufRead};
extern crate rand;
use rand::Rng;

fn op_type(x: char) -> i32{
    match x {
        '-' | '+' => return 1,
        '/' | '*' => return 2,
        '(' | ')' => return -1,
        _   => return 0,
    }
}

fn to_rpn(input: &mut String){

    let mut rpn_string : String = String::new();
    let mut rpn_stack : String = String::new();
    let mut last_token = '#';
    for token in input.chars(){
        if token.is_digit(10) {
            rpn_string.push(token);
        }
        else if op_type(token) == 0 {
            continue;
        }
        else if op_type(token) > op_type(last_token) || token == '(' {
                rpn_stack.push(token);
                last_token=token;
        }
        else {
            while let Some(top) = rpn_stack.pop() {
                if top=='(' {
                    break;
                }
                rpn_string.push(top);
            }
            if token != ')'{
                rpn_stack.push(token);
            }
        }
    }
    while let Some(top) = rpn_stack.pop() {
        rpn_string.push(top);
    }

    println!("you formula results in {}", rpn_string);

    *input=rpn_string;
}

fn calculate(input: &String, list : &mut [u32;4]) -> f32{
    let mut stack : Vec<f32> = Vec::new();
    let mut accumulator : f32 = 0.0;

    for token in input.chars(){
        if token.is_digit(10) {
            let test = token.to_digit(10).unwrap() as u32;
            match list.iter().position(|&x| x == test){
                Some(idx) => list[idx]=10 ,
                _         => println!(" invalid digit: {} ",test),
            }
            stack.push(accumulator);
            accumulator = test as f32;
        }else{
            let a = stack.pop().unwrap();
            accumulator = match token {
                '-' => a-accumulator,
                '+' => a+accumulator,
                '/' => a/accumulator,
                '*' => a*accumulator,
                _ => {accumulator},//NOP
            };
        }
    }
    println!("you formula results in {}",accumulator);
    accumulator
}

fn main() {

    let mut rng = rand::thread_rng();
    let mut list :[u32;4]=[rng.gen::<u32>()%10,rng.gen::<u32>()%10,rng.gen::<u32>()%10,rng.gen::<u32>()%10];

    println!("form 24 with using + - / * {:?}",list);
    //get user input
    let mut input = String::new();
    io::stdin().read_line(&mut input).unwrap();
    //convert to rpn
    to_rpn(&mut input);
    let result = calculate(&input, &mut list);

    if list.iter().any(|&list| list !=10){
        println!("and you used all numbers");
        match result {
            24.0 => println!("you won"),
            _ => println!("but your formulla doesn't result in 24"),
        }
    }else{
        println!("you didn't use all the numbers");
    }

}
```



## Scala

The solution below is much more complex than strictly needed, because it shows off
Scala's Parser library, which enables easy construction of parsers from
EBNF grammars.

Only problems with solution are shown to the user.
<div style='width: full; overflow: scroll'>
```scala
object TwentyFourGame {
  def main(args: Array[String]) {
    import Parser.TwentyFourParser

    println(welcome)

    var parser = new TwentyFourParser(problemsIterator.next)
    println("Your four digits: "+parser+".")

    var finished = false
    var expressionCount = 1
    do {
      val line = Console.readLine("Expression "+expressionCount+": ")
      line match {
        case "!" =>
          parser = new TwentyFourParser(problemsIterator.next)
          println("New digits: "+parser+".")

        case "q" =>
          finished = true

        case _ =>
          parser readExpression line match {
            case Some(24) => println("That's right!"); finished = true
            case Some(n) => println("Sorry, that's "+n+".")
            case None =>
          }
      }
      expressionCount += 1
    } while (!finished)

    println("Thank you and goodbye!")
  }

  val welcome = """|The 24 Game
                   |
                   |Given any four digits in the range 1 to 9, which may have repetitions,
                   |Using just the +, -, *, and / operators; and the possible use of
                   |brackets, (), show how to make an answer of 24.
                   |
                   |An answer of "q" will quit the game.
                   |An answer of "!" will generate a new set of four digits.
                   |Otherwise you are repeatedly asked for an expression until it evaluates to 24
                   |
                   |Note: you cannot form multiple digit numbers from the supplied digits,
                   |so an answer of 12+12 when given 1, 2, 2, and 1 would not be allowed.
                   |""".stripMargin

  val problemsIterator = (
    Iterator
    continually List.fill(4)(scala.util.Random.nextInt(9) + 1 toDouble)
    filter hasSolution
  )

  def hasSolution(l: List[Double]) = permute(l) flatMap computeAllOperations exists (_ == 24)

  def computeAllOperations(l: List[Double]): List[Double] = l match {
    case Nil => Nil
    case x :: Nil => l
    case x :: xs =>
      for {
        y <- computeAllOperations(xs)
        z <- if (y == 0) List(x*y, x+y, x-y) else List(x*y, x/y, x+y, x-y)
      } yield z
  }

  def permute(l: List[Double]): List[List[Double]] = l match {
    case Nil => List(Nil)
    case x :: xs =>
      for {
        ys <- permute(xs)
        position <- 0 to ys.length
        (left, right) = ys splitAt position
      } yield left ::: (x :: right)
  }

  object Parser {
    /*  Arithmetic expression grammar production rules in EBNF form:
     *
     * <expr> --> <term> ( '+' <term> | '-' <term> )*
     * <term> --> <factor> ( '*'  <factor> | '/'  <factor> )*
     * <factor> --> '(' <expr> ')' | <digit>
     * <digit> --> 0 | 1  | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9
     *
     * Semantically, <digit> can only be a digit from the list of remaining digits.
     */

    class TwentyFourParser(digits: List[Double]) extends scala.util.parsing.combinator.RegexParsers {
      require(digits.length == 4 && digits.forall(d => 0 <= d && d <= 9))
      override val toString = digits.map(_.toInt).mkString(", ")

      // Grammar
      def exprConsumingAllDigits = expr ^? (remainingDigits.allDigitsConsumed, digitsRemainingError) // Guarantees all digits consumed
      def expr : Parser[Double] = term ~ rep( "+" ~ term | "-" ~ term) ^^ solveOperationChain
      def term = factor ~ rep( "*" ~ factor | "/" ~ factor) ^^ solveOperationChain
      def factor = "(" ~> expr <~ ")" | digit
      def digit = digitRegex ^? (remainingDigits.consumeDigit, digitNotAllowedError)
      def digitRegex = "\\d".r | digitExpected
      def digitExpected: Parser[String] = ".".r <~ failure(expectedDigitError) // Produces clear error messages

      // Evaluate expressions
      def readExpression(input: String): Option[Double] = {
        remainingDigits = new DigitList(digits) // Initialize list of digits to be consumed
        parseAll(exprConsumingAllDigits, input) match {
          case Success(result, _) => Some(result)
          case NoSuccess(msg, next) =>
            println(ParsingErrorFormatter(msg, next))
            None
        }
      }

      // List of digits to be consumed
      private var remainingDigits: DigitList = _

      // Solve partial results from parsing
      private def solveOperationChain(partialResult: ~[Double,List[~[String,Double]]]): Double = partialResult match {
        case first ~ chain => chain.foldLeft(first)(doOperation)
      }
      private def doOperation(acc: Double, op: ~[String, Double]): Double = op match {
        case "+" ~ operand => acc + operand
        case "-" ~ operand => acc - operand
        case "*" ~ operand => acc * operand
        case "/" ~ operand => acc / operand
        case x => error("Unknown operation "+x+".")
      }

      // Error messages
      private def digitNotAllowedError(d: String) = "Digit "+d+" is not allowed here. Available digits: "+remainingDigits+"."
      private def digitsRemainingError(x: Any) = "Not all digits were consumed. Digits remaining: "+remainingDigits+"."
      private def expectedDigitError = "Unexpected input. Expected a digit from the list: "+remainingDigits+"."
    }

    private object ParsingErrorFormatter {
      def apply[T](msg: String, next: scala.util.parsing.input.Reader[T]) =
        "%s\n%s\n%s\n" format (msg, next.source.toString.trim, " "*(next.offset - 1)+"^")
    }

    private class DigitList(digits: List[Double]) {
      private var remainingDigits = digits
      override def toString = remainingDigits.map(_.toInt).mkString(", ")

      def consumeDigit: PartialFunction[String, Double] = {
        case d if remainingDigits contains d.toDouble =>
          val n = d.toDouble
          remainingDigits = remainingDigits diff List(n)
          n
      }

      def allDigitsConsumed: PartialFunction[Double, Double] = {
        case n if remainingDigits.isEmpty => n
      }
    }
  }
}
```
</div>

### Output

```txt

C:\Workset>scala TwentyFourGame
The 24 Game

Given any four digits in the range 1 to 9, which may have repetitions,
Using just the +, -, *, and / operators; and the possible use of
brackets, (), show how to make an answer of 24.

An answer of "q" will quit the game.
An answer of "!" will generate a new set of four digits.
Otherwise you are repeatedly asked for an expression until it evaluates to 24

Note: you cannot form multiple digit numbers from the supplied digits,
so an answer of 12+12 when given 1, 2, 2, and 1 would not be allowed.

Your four digits: 2, 7, 7, 2.
Expression 1: 2*7+2+7
Sorry, that's 23.0.
Expression 2: 7*7/2-2
Sorry, that's 22.5.
Expression 3: 2*7+(7-2)
Sorry, that's 19.0.
Expression 4: 2*(7+7-2)
That's right!
Thank you and goodbye!

```



## Scheme


*Works with: PLT Scheme 4*

This uses read to read in a scheme expression, and eval to evaluate it, so in that sense it's not ideal (eval is evil etc.) but any expression that is valid should be safe and terminate in a timely manner.


```scheme
#lang scheme
(require srfi/27 srfi/1) ;; random-integer, every

(define (play)
  (let* ([numbers (build-list 4 (lambda (n)
                                  (add1 (random-integer 9))))]
         [valid?  (curryr valid? numbers)])
    (printf startup-message numbers)
    (let loop ([exp (read)])
      (with-handlers ([exn:fail? (lambda (err)
                                   (printf error-message exp (exn-message err))
                                   (loop (read)))])
       (cond [(eq? exp '!) (play)]

             [(or (eq? exp 'q)
                  (eof-object? exp)) (printf quit-message)]

             [(not (valid? exp))
              (printf bad-exp-message exp)
              (loop (read))]

             [(not (= (eval exp) 24))
              (printf bad-result-message exp (eval exp))
              (loop (read))]

             [else (printf winning-message)])))))

(define (valid? exp numbers)
  ;; must contain each number exactly once and only valid symbols
  (define (valid-symbol? sym)
    ;; only +, -, *, and / are valid
    (case sym
      [(+ - * /) #t]
      [else #f]))

  (let* ([ls (flatten exp)]
         [numbers* (filter number? ls)]
         [symbols  (remove number? ls)])
    (and (equal? (sort numbers <)
                 (sort numbers* <))
         (every valid-symbol? symbols))))

(define startup-message "
Write a lisp expression that evaluates to 24
using only (, ), +, -, *, /
and these four numbers: ~a

or '!' to get a new set of numbers
or 'q' to quit")

(define error-message "
Your expression ~a raised an exception:

  \"~a\"

Please try again")

(define bad-exp-message "Sorry, ~a is a bad expression.")
(define bad-result-message "Sorry, ~a evaluates to ~a, not 24.")
(define quit-message "Thanks for playing...")
(define winning-message "You win!")

(provide play)

```


### Output

```txt

> (require "24game.ss")
> (play)

Write a lisp expression that evaluates to 24
using only (, ), +, -, *, /
and these four numbers: (2 7 2 5)

or '!' to get a new set of numbers
or 'q' to quit
!

Write a lisp expression that evaluates to 24
using only (, ), +, -, *, /
and these four numbers: (9 2 7 6)

or '!' to get a new set of numbers
or 'q' to quit
(9 7 6 2)

Your expression (9 7 6 2) raised an exception:

  "procedure application: expected procedure, given: 9; arguments were: 7 6 2"

Please try again
(+ 9 7 6 2)
You win!

```



## Sidef


```ruby
const digits  = (1..9 -> pick(4))
const grammar = Regex(
    '^ (?&exp) \z
      (?(DEFINE)
          (?<exp> ( (?&term) (?&op) (?&term) )+ )
          (?<term> \( (?&exp) \) | [' + digits.join + '])
          (?<op> [\-+*/] )
      )', 'x'
)

say "Here are your digits: #{digits.join(' ')}"

loop {
    var input = read("Expression: ", String)

    var expr = input
    expr -= /\s+/g     # remove all whitespace

    if (input == 'q') {
        say "Goodbye.  Sorry you couldn't win."
        break
    }

    var given_digits = digits.map{.to_s}.sort.join
    var entry_digits = input.scan(/\d/).sort.join

    if ((given_digits != entry_digits) || (expr !~ grammar)) {
        say "That's not valid"
        next
    }

    given(var n = eval(input)) {
        when (24) { say "You win!"; break }
        default   { say "Sorry, your expression is #{n}, not 24" }
    }
}
```

### Output

```txt

Here are your digits: 8 2 3 4
Expression: 8 * (2 - (3 + 4))
Sorry, your expression is -40, not 24
Expression: 8 * (2 - (3 -
That's not valid
Expression: 8 * (2 - (3 - 4))
You win!

```


## Simula


```simula
BEGIN

    CLASS EXPR;
    BEGIN


        REAL PROCEDURE POP;
        BEGIN
            IF STACKPOS > 0 THEN
            BEGIN STACKPOS := STACKPOS - 1; POP := STACK(STACKPOS); END;
        END POP;


        PROCEDURE PUSH(NEWTOP); REAL NEWTOP;
        BEGIN
            STACK(STACKPOS) := NEWTOP;
            STACKPOS := STACKPOS + 1;
        END PUSH;


        REAL PROCEDURE CALC(OPERATOR, ERR); CHARACTER OPERATOR; LABEL ERR;
        BEGIN
            REAL X, Y; X := POP; Y := POP;
            IF      OPERATOR = '+' THEN PUSH(Y + X)
            ELSE IF OPERATOR = '-' THEN PUSH(Y - X)
            ELSE IF OPERATOR = '*' THEN PUSH(Y * X)
            ELSE IF OPERATOR = '/' THEN BEGIN
                                            IF X = 0 THEN
                                            BEGIN
                                                EVALUATEDERR :- "DIV BY ZERO";
                                                GOTO ERR;
                                            END;
                                            PUSH(Y / X);
                                        END
            ELSE
            BEGIN
                EVALUATEDERR :- "UNKNOWN OPERATOR";
                GOTO ERR;
            END
        END CALC;


        PROCEDURE READCHAR(CH); NAME CH; CHARACTER CH;
        BEGIN
            IF T.MORE THEN CH := T.GETCHAR ELSE CH := EOT;
        END READCHAR;


        PROCEDURE SKIPWHITESPACE(CH); NAME CH; CHARACTER CH;
        BEGIN
            WHILE (CH = SPACE) OR (CH = TAB) OR (CH = CR) OR (CH = LF) DO
                READCHAR(CH);
        END SKIPWHITESPACE;


        PROCEDURE BUSYBOX(OP, ERR); INTEGER OP; LABEL ERR;
        BEGIN
            CHARACTER OPERATOR;
            REAL NUMBR;
            BOOLEAN NEGATIVE;

            SKIPWHITESPACE(CH);

            IF OP = EXPRESSION THEN
            BEGIN

                NEGATIVE := FALSE;
                WHILE (CH = '+') OR (CH = '-') DO
                BEGIN
                    IF CH = '-' THEN NEGATIVE :=  NOT NEGATIVE;
                    READCHAR(CH);
                END;

                BUSYBOX(TERM, ERR);

                IF NEGATIVE THEN
                BEGIN
                    NUMBR := POP; PUSH(0 - NUMBR);
                END;

                WHILE (CH = '+') OR (CH = '-') DO
                BEGIN
                    OPERATOR := CH; READCHAR(CH);
                    BUSYBOX(TERM, ERR); CALC(OPERATOR, ERR);
                END;

            END
            ELSE IF OP = TERM THEN
            BEGIN

                BUSYBOX(FACTOR, ERR);
                WHILE (CH = '*') OR (CH = '/') DO
                BEGIN
                    OPERATOR := CH; READCHAR(CH);
                    BUSYBOX(FACTOR, ERR); CALC(OPERATOR, ERR)
                END

            END
            ELSE IF OP = FACTOR THEN
            BEGIN

                IF (CH = '+') OR (CH = '-') THEN
                  BUSYBOX(EXPRESSION, ERR)
                ELSE IF (CH >= '0') AND (CH <= '9') THEN
                  BUSYBOX(NUMBER, ERR)
                ELSE IF CH = '(' THEN
                BEGIN
                    READCHAR(CH);
                    BUSYBOX(EXPRESSION, ERR);
                    IF CH = ')' THEN READCHAR(CH) ELSE GOTO ERR;
                END
                ELSE GOTO ERR;

            END
            ELSE IF OP = NUMBER THEN
            BEGIN

                NUMBR := 0;
                WHILE (CH >= '0') AND (CH <= '9') DO
                BEGIN
                    NUMBR := 10 * NUMBR + RANK(CH) - RANK('0'); READCHAR(CH);
                END;
                IF CH = '.' THEN
                BEGIN
                    REAL FAKTOR;
                    READCHAR(CH);
                    FAKTOR := 10;
                    WHILE (CH >= '0') AND (CH <= '9') DO
                    BEGIN
                        NUMBR := NUMBR + (RANK(CH) - RANK('0')) / FAKTOR;
                        FAKTOR := 10 * FAKTOR;
                        READCHAR(CH);
                    END;
                END;
                PUSH(NUMBR);

            END;

            SKIPWHITESPACE(CH);

        END BUSYBOX;


        BOOLEAN PROCEDURE EVAL(INP); TEXT INP;
        BEGIN
            EVALUATEDERR :- NOTEXT;
            STACKPOS := 0;
            T :- COPY(INP.STRIP);
            READCHAR(CH);
            BUSYBOX(EXPRESSION, ERRORLABEL);
          ! OUTTEXT("T = '");
          ! OUTTEXT(T);
          ! OUTTEXT("'");
          ! OUTTEXT(", T.POS = ");
          ! OUTINT(T.POS, 0);
          ! OUTTEXT(", STACKPOS = ");
          ! OUTINT(STACKPOS, 0);
          ! OUTTEXT(", T.MORE = ");
          ! OUTCHAR(IF T.MORE THEN 'T' ELSE 'F');
          ! OUTTEXT(", CH = ");
          ! OUTCHAR(CH);
          ! OUTIMAGE;
            IF NOT T.MORE AND STACKPOS = 1 AND CH = EOT THEN
            BEGIN
                EVALUATED := POP;
                EVAL := TRUE;
                GOTO NOERRORLABEL;
            END;
    ERRORLABEL:
            EVAL := FALSE;
            IF EVALUATEDERR = NOTEXT THEN
                EVALUATEDERR :- "INVALID EXPRESSION: " & INP;
    NOERRORLABEL:
        END EVAL;


        REAL PROCEDURE RESULT;
            RESULT := EVALUATED;

        TEXT PROCEDURE ERR;
            ERR :- EVALUATEDERR;

        TEXT T;

        INTEGER EXPRESSION;
        INTEGER TERM;
        INTEGER FACTOR;
        INTEGER NUMBER;

        CHARACTER TAB;
        CHARACTER LF;
        CHARACTER CR;
        CHARACTER SPACE;
        CHARACTER EOT;

        CHARACTER CH;
        REAL ARRAY STACK(0:31);
        INTEGER STACKPOS;

        REAL EVALUATED;
        TEXT EVALUATEDERR;

        EXPRESSION := 1;
        TERM := 2;
        FACTOR := 3;
        NUMBER := 4;

        TAB := CHAR(9);
        LF := CHAR(10);
        CR := CHAR(13);
        SPACE := CHAR(32);
        EOT := CHAR(0);

    END EXPR;

    INTEGER ARRAY DIGITS(1:4);
    INTEGER SEED, I;
    REF(EXPR) E;

    E :- NEW EXPR;
    OUTTEXT("ENTER RANDOM SEED: ");
    OUTIMAGE;
    SEED := ININT;
    FOR I := 1 STEP 1 UNTIL 4 DO DIGITS(I) := RANDINT(0, 9, SEED);

L:  BEGIN
        INTEGER ARRAY DIGITSUSED(0:9);
        INTEGER ARRAY DIGITSTAKEN(0:9);
        CHARACTER C, LASTC;
        TEXT INP;

        LASTC := CHAR(0);
        OUTTEXT("MAKE 24 USING THESE DIGITS: ");
        FOR I := 1 STEP 1 UNTIL 4 DO
        BEGIN
            OUTINT(DIGITS(I), 2);
            DIGITSUSED( DIGITS(I) ) := DIGITSUSED( DIGITS(I) ) + 1;
        END;
        OUTIMAGE;
        INIMAGE;
        INP :- COPY(SYSIN.IMAGE.STRIP);
        OUTIMAGE;
        WHILE INP.MORE DO
        BEGIN
            C := INP.GETCHAR;
            IF (C >= '0') AND (C <= '9') THEN
            BEGIN
                INTEGER D;
                IF (LASTC >= '0') AND (LASTC <= '9') THEN
                BEGIN
                    OUTTEXT("NUMBER HAS TOO MANY DIGITS: ");
                    OUTCHAR(LASTC);
                    OUTCHAR(C);
                    OUTIMAGE;
                    GOTO L;
                END;
                D := RANK(C) - RANK('0');
                DIGITSTAKEN(D) := DIGITSTAKEN(D) + 1;
            END
            ELSE IF NOT ((C = '+') OR (C = '-') OR (C = '/') OR (C = '*') OR
                         (C = ' ') OR (C = '(') OR (C = ')')) THEN
            BEGIN
                OUTTEXT("ILLEGAL INPUT CHARACTER: ");
                OUTCHAR(C);
                OUTIMAGE;
                GOTO L;
            END;
            LASTC := C;
        END;
        FOR I := 0 STEP 1 UNTIL 9 DO
        BEGIN
            IF DIGITSUSED(I) <> DIGITSTAKEN(I) THEN
            BEGIN
                OUTTEXT("NOT THE SAME DIGITS.");
                OUTIMAGE;
                GOTO L;
            END;
        END;
        IF E.EVAL(INP) THEN
        BEGIN
            OUTTEXT("RESULT IS ");
            OUTFIX(E.RESULT, 4, 10);
            OUTIMAGE;
            OUTTEXT(IF ABS(E.RESULT - 24) < 0.001
                    THEN "YOU WIN"
                    ELSE "YOU LOOSE");
            OUTIMAGE;
        END
        ELSE
        BEGIN
            OUTTEXT(E.ERR);
            OUTIMAGE;
        END;
    END;

END.

```

### Output

```txt

ENTER RANDOM SEED:

```

{{in}}

```txt

787

```

### Output

```txt

MAKE 24 USING THESE DIGITS:  6 7 3 9

```

{{in}}

```txt

(9 - 7 + 6) * 3

```

### Output

```txt

RESULT IS    24.0000
YOU WIN

```



## Swift


```swift
import Darwin
import Foundation

println("24 Game")
println("Generating 4 digits...")

func randomDigits() -> Int[] {
    var result = Int[]();
    for var i = 0; i < 4; i++ {
        result.append(Int(arc4random_uniform(9)+1))
    }
    return result;
}

// Choose 4 digits
let digits = randomDigits()

print("Make 24 using these digits : ")

for digit in digits {
    print("\(digit) ")
}
println()

// get input from operator
var input = NSString(data:NSFileHandle.fileHandleWithStandardInput().availableData, encoding:NSUTF8StringEncoding)

var enteredDigits = Int[]()

var enteredOperations = Character[]()

let inputString = input as String

// store input in the appropriate table
for character in inputString {
    switch character {
        case "1", "2", "3", "4", "5", "6", "7", "8", "9":
            let digit = String(character)
            enteredDigits.append(digit.toInt()!)
        case "+", "-", "*", "/":
            enteredOperations.append(character)
        case "\n":
            println()
        default:
            println("Invalid expression")
    }
}

// check value of expression provided by the operator
var value = Int()

if enteredDigits.count == 4 && enteredOperations.count == 3 {
    value = enteredDigits[0]
    for (i, operation) in enumerate(enteredOperations) {
        switch operation {
            case "+":
                value = value + enteredDigits[i+1]
            case "-":
                value = value - enteredDigits[i+1]
            case "*":
                value = value * enteredDigits[i+1]
            case "/":
                value = value / enteredDigits[i+1]
            default:
                println("This message should never happen!")
        }
    }
}

if value != 24 {
    println("The value of the provided expression is \(value) instead of 24!")
} else {
    println("Congratulations, you found a solution!")
}

```



## Tcl

{{trans|Python}}
This version also terminates cleanly on end-of-file.

```tcl
# Four random non-zero digits
proc choose4 {} {
    set digits {}
    foreach x {1 2 3 4} {lappend digits [expr {int(1+rand()*9)}]}
    return [lsort $digits]
}

# Print out a welcome message
proc welcome digits {
    puts [string trim "
The 24 Game

Given any four digits in the range 1 to 9, which may have repetitions,
Using just the +, -, *, and / operators; and the possible use of
brackets, (), show how to make an answer of 24.

An answer of \"q\" will quit the game.
An answer of \"!\" will generate a new set of four digits.
Otherwise you are repeatedly asked for an expression until it evaluates to 24

Note: you cannot form multiple digit numbers from the supplied digits,
so an answer of 12+12 when given 1, 2, 2, and 1 would not be allowed.
    "]
    puts "\nYour four digits: $digits"
}

# Check whether we've got a legal answer
proc check {answer digits} {
    if {
	[regexp "\[^-+*/() \t[join $digits {}]\]" $answer]
	|| [regexp {\d\d} $answer]
    } then {
	return false
    }
    set digs [lsort [regexp -inline -all {\d} $answer]]
    if {$digs ne $digits} {
	return false
    }
    expr {![catch {expr $answer}]}
}

# The main game loop
proc main {} {
    fconfigure stdout -buffering none

    set digits [choose4]
    welcome $digits
    set trial 0
    while true {
	puts -nonewline "Expression [incr trial]: "
	gets stdin answer

        # Check for various types of non-answer
	if {[eof stdin] || $answer eq "q" || $answer eq "Q"} {
	    break
	} elseif {$answer eq "!"} {
	    set digits [choose4]
	    puts "New digits: $digits"
	    continue
	} elseif {![check $answer $digits]} {
	    puts "The input '$answer' was wonky!"
            continue
	}

        # Check to see if it is the right answer
	set ans [expr [regsub {\d} $answer {&.0}]]
	puts " = [string trimright $ans .0]"
	if {$ans == 24.0} {
	    puts "That's right!"
            break
	}
    }
    puts "Thank you and goodbye"
}
main
```



## TorqueScript

Includes an equation parser to avoid using eval.
To use, type startTwentyFourGame(); in the console.

```Torque
function startTwentyFourGame()
{
	if($numbers !$= "")
	{
		echo("Ending current 24 game...");
		endTwentyFourGame();
	}

	echo("Welcome to the 24 game!");
	echo("Generating 4 numbers...");
	for(%a = 0; %a < 4; %a++)
		$numbers = setWord($numbers, %a, getRandom(0, 9));

	echo("Numbers generated! Here are your numbers:");
	echo($numbers);
	echo("Use try24Equation( equation ); to try and guess the equation.");

	$TwentyFourGame = 1;
}

function endTwentyFourGame()
{
	if(!$TwentyFourGame)
	{
		echo("No 24 game is active!");
		return false;
	}

	echo("Ending the 24 game.");
	$numbers = "";
	$TwentyFourGame = 0;
}

function try24Equation(%equ)
{
	if(!$TwentyFourGame)
	{
		echo("No 24 game is active!");
		return false;
	}
	%numbers = "0123456789";
	%operators = "+-*x/()";
	%tempchars = $numbers;
	%other = strReplace(%tempchars, " ", "");

	//Check it and make sure it has all the stuff
	%equ = strReplace(%equ, " ", "");
	%length = strLen(%equ);

	for(%a = 0; %a < %Length; %a++)
	{
		%Char = getSubStr(%equ, %a, 1);
		if(%a+1 != %Length)
			%Next = getSubStr(%equ, %a+1, 1);
		else
			%Next = " ";

		if(strPos(%numbers @ %operators, %char) < 0)
		{
			echo("The equation you entered is invalid! Try again.");
			return false;
		}
		if(strPos(%tempchars, %char) < 0 && strPos(%operators, %char) < 0)
		{
			echo("The equation you entered uses a number you were not given! Try again.");
			return false;
		}
		else if(strPos(%numbers, %next) >= 0 && strPos(%numbers, %char) >= 0)
		{
			echo("No numbers above 9 please! Try again.");
			echo(%next SPC %char SPC %a);
			return false;
		}
		else if(strPos(%operators, %char) > 0)
			continue;

		%pos = 2*strPos(%other, %char);
		if(%pos < 0)
			return "ERROROMG";

		//Remove it from the allowed numbers
		%tempchars = removeWord(%tempchars, %pos/2);
		%other = getSubStr(%other, 0, %pos) @ getSubStr(%other, %pos+1, strLen(%other));
    }

    %result = doEquation(%equ);

    if(%result != 24)
    {
        echo("Your equation resulted to" SPC %result @ ", not 24! Try again.");
        return false;
    }

    for(%a = 0; %a < 4; %a++)
        $numbers = setWord($numbers, %a, getRandom(0, 9));

    echo("Great job!" SPC %equ SPC "Does result to 24! Here's another set for you:");
    echo($numbers);
}

//Evaluates an equation without using eval.
function doEquation(%equ)
{   //Validate the input
    %equ = strReplace(%equ, " ", "");%equ = strReplace(%equ, "*", "x");
    %equ = strReplace(%equ, "+", " + ");%equ = strReplace(%equ, "x", " x ");
    %equ = strReplace(%equ, "/", " / ");%equ = strReplace(%equ, "-", " - ");

    //Parenthesis'
    while(strPos(%equ, "(") > -1 && strPos(%equ, ")") > 0)
    {
        %start = strPos(%equ, "(");
        %end = %start;
        %level = 1;
        while(%level != 0 && %end != strLen(%equ))
        {
            %end++;
            if(getsubStr(%equ, %end, 1) $= "(") %level++;
            if(getsubStr(%equ, %end, 1) $= ")") %level--;
        }
        if(%level != 0)
            return "ERROR";
        %inbrackets = getsubStr(%equ, %start+1, %end - strLen(getsubStr(%equ, 0, %start + 1)));
        %leftofbrackets = getsubStr(%equ, 0, %start);
        %rightofbrackets = getsubStr(%equ, %end + 1, strLen(%equ) - %end);
        %equ = %leftofbrackets @ doEquation(%inbrackets) @ %rightofbrackets;
    }

    if(strPos(%equ, "ERROR") >= 0)
        return "ERROR";

    //Multiplication/Division loop
    for(%a = 0; %a < getWordCount(%equ); %a++)
    {
        if(getWord(%equ, %a) $= "x" || getWord(%equ, %a) $= "/" && %a != 0)
        {
            %f = getWord(%equ, %a - 1);
            %l = getWord(%equ, %a + 1);
            %o = getWord(%equ, %a);
            switch$(%o)
            {
                case "x": %a--;
                    %equ = removeWord(removeWord(setWord(%equ, %a+1, %f * %l), %a+2), %a);
                case "/": %a--;
                    %equ = removeWord(removeWord(setWord(%equ, %a+1, %f / %l), %a+2), %a);
            }
        }
    }

    //Addition/Subraction loop
    for(%a = 0; %a < getWordCount(%equ); %a++)
    {
        if(getWord(%equ, %a) $= "+" || getWord(%equ, %a) $= "-" && %a != 0)
        {
            %f = getWord(%equ, %a - 1);
            %l = getWord(%equ, %a + 1);
            %o = getWord(%equ, %a);
            switch$(%o)
            {
                case "+": %a--;
                    %equ = removeWord(removeWord(setWord(%equ, %a+1, %f + %l), %a+2), %a);
                case "-": %a--;
                    %equ = removeWord(removeWord(setWord(%equ, %a+1, %f - %l), %a+2), %a);
            }
        }
    }
    return %equ;
}
```



## TUSCRIPT


```tuscript

$$ MODE TUSCRIPT
BUILD X_TABLE blanks = ":': :"

SECTION game
operators="*'/'+'-'(')",numbers=""

LOOP n=1,4
number=RANDOM_NUMBERS (1,9,1)
numbers=APPEND(numbers,number)
ENDLOOP

SET allowed=APPEND (numbers,operators)
SET allowed=MIXED_SORT (allowed)
SET allowed=REDUCE (allowed)
BUILD S_TABLE ALLOWED =*
DATA '{allowed}'

SET checksum=DIGIT_SORT (numbers)

printnumbers=EXCHANGE (numbers,blanks)
printoperat=EXCHANGE (operators,blanks)

PRINT "Your numbers ", printnumbers
PRINT "Use only these operators ", printoperat
PRINT "Enter an expression that equates to 24"
PRINT "Enter 'l' for new numbers"
PRINT "Your 4 digits: ",printnumbers

DO play
ENDSECTION

SECTION check_expr
 SET pos = VERIFY (expr,allowed)
 IF (pos!=0) THEN
  PRINT "wrong entry on position ",pos
  DO play
  STOP
 ELSE
  SET yourdigits   = STRINGS (expr,":>/:")
  SET yourchecksum = DIGIT_SORT (yourdigits)
   IF (checksum!=yourchecksum) THEN
    PRINT/ERROR "wrong digits"
    DO play
    STOP
   ELSE
    CONTINUE
   ENDIF
 ENDIF
ENDSECTION

SECTION play
LOOP n=1,3
ASK   "Expression {n}": expr=""
IF (expr=="l") THEN
RELEASE S_TABLE allowed
PRINT "Your new numbers"
DO game
ELSEIF (expr!="") THEN
DO check_expr
sum={expr}
 IF (sum!=24) THEN
  PRINT/ERROR expr," not equates 24 but ",sum
  CYCLE
 ELSE
  PRINT "BINGO ", expr," equates ", sum
  STOP
 ENDIF
ELSE
 CYCLE
ENDIF
ENDLOOP
ENDSECTION
DO game

```

### Output
<pre style='height:30ex;overflow:scroll'>
Your numbers 2 8 9 9
Use only these operators * / + - ( )
Enter an expression that equates to 24
Enter 'l' for new numbers
Your 4 digits: 2 8 9 9
Expression 1 >l
Your new numbers
Your numbers 2 4 2 3
Use only these operators * / + - ( )
Enter an expression that equates to 24
Enter 'l' for new numbers
Your 4 digits: 2 4 2 3
Expression 1 >2+4+2+3
@@@@@@@@  2+4+2+3 not equates 24 but 11                                @@@@@@@@
Expression 2 >2+2+2+3
@@@@@@@@  wrong digits                                                 @@@@@@@@
Expression 1 >2+2+a+3
wrong entry on position 5
Expression 1 >(2+4+2)*3
BINGO (2+4+2)*3 equates 24

```


{{omit from|GUISS}}
{{omit from|ML/I}}


## UNIX Shell

Tried to be POSIX. Not sure about corner-cases.

```bash
gen_digits() {
    awk 'BEGIN { srand()
                 for(i = 1; i <= 4; i++) print 1 + int(9 * rand())
         }' | sort
}

same_digits() {
    [ "$(tr -dc 0-9 | sed 's/./&\n/g' | grep . | sort)" = "$*" ]
}

guessed() {
    [ "$(echo "$1" | tr -dc '\n0-9()*/+-' | bc 2>/dev/null)" = 24 ]
}


while :
do
    digits=$(gen_digits)
    echo
    echo Digits: $digits
    read -r expr

    echo " $expr" | same_digits "$digits" || \
        { echo digits should be: $digits; continue; }

    guessed "$expr" && message=correct \
                    || message=wrong

    echo $message
done
```



## VBA


```vb

Sub Rosetta_24game()

Dim Digit(4) As Integer, i As Integer, iDigitCount As Integer
Dim stUserExpression As String
Dim stFailMessage As String, stFailDigits As String
Dim bValidExpression As Boolean, bValidDigits As Boolean, bValidChars As Boolean
Dim vResult As Variant, vTryAgain As Variant, vSameDigits As Variant

' Generate 4 random digits
GenerateNewDigits:
    For i = 1 To 4
        Digit(i) = [randbetween(1,9)]
    Next i

' Get user expression
GetUserExpression:
    bValidExpression = True
    stFailMessage = ""
    stFailDigits = ""
    stUserExpression = InputBox("Enter a mathematical expression which results in 24, using the following digits: " & _
        Digit(1) & ", " & Digit(2) & ", " & Digit(3) & " and " & Digit(4), "Rosetta Code | 24 Game")

' Check each digit is included in user expression
    bValidDigits = True
    stFailDigits = ""
    For i = 1 To 4
        If InStr(stUserExpression, Digit(i)) = 0 Then
            bValidDigits = False
            stFailDigits = stFailDigits & " " & Digit(i)
        End If
    Next i
    If bValidDigits = False Then
        bValidExpression = False
        stFailMessage = "Your expression excluded the following required digits: " & stFailDigits & vbCr & vbCr
    End If

' Check each character of user expression is a valid character type
    bValidDigits = True
    stFailDigits = ""
    For i = 1 To Len(stUserExpression)
        If InStr("0123456789+-*/()", Mid(stUserExpression, i, 1)) = 0 Then
            bValidDigits = False
            stFailDigits = stFailDigits & " " & Mid(stUserExpression, i, 1)
        End If
    Next i
    If bValidDigits = False Then
        bValidExpression = False
        stFailMessage = stFailMessage & "Your expression contained invalid characters:" & stFailDigits & vbCr & vbCr
    End If

' Check no disallowed integers entered
    bValidDigits = True
    stFailDigits = ""
    iDigitCount = 0
    For i = 1 To Len(stUserExpression)
        If Not InStr("0123456789", Mid(stUserExpression, i, 1)) = 0 Then
            iDigitCount = iDigitCount + 1
            If IsError(Application.Match(--(Mid(stUserExpression, i, 1)), Digit, False)) Then
                bValidDigits = False
                stFailDigits = stFailDigits & " " & Mid(stUserExpression, i, 1)
            End If
        End If
    Next i
    If iDigitCount > 4 Then
        bValidExpression = False
        stFailMessage = stFailMessage & "Your expression contained more than 4 digits" & vbCr & vbCr
    End If
        If iDigitCount < 4 Then
        bValidExpression = False
        stFailMessage = stFailMessage & "Your expression contained less than 4 digits" & vbCr & vbCr
    End If
    If bValidDigits = False Then
        bValidExpression = False
        stFailMessage = stFailMessage & "Your expression contained invalid digits:" & stFailDigits & vbCr & vbCr
    End If

' Check no double digit numbers entered
    bValidDigits = True
    stFailDigits = ""
    For i = 11 To 99
        If Not InStr(stUserExpression, i) = 0 Then
            bValidDigits = False
            stFailDigits = stFailDigits & " " & i
        End If
    Next i
    If bValidDigits = False Then
        bValidExpression = False
        stFailMessage = stFailMessage & "Your expression contained invalid numbers:" & stFailDigits & vbCr & vbCr
    End If

' Check result of user expression
    On Error GoTo EvalFail
    vResult = Evaluate(stUserExpression)
    If Not vResult = 24 Then
        bValidExpression = False
        stFailMessage = stFailMessage & "Your expression did not result in 24. It returned: " & vResult
    End If

' Return results
    If bValidExpression = False Then
        vTryAgain = MsgBox(stFailMessage & vbCr & vbCr & "Would you like to try again?", vbCritical + vbRetryCancel, "Rosetta Code | 24 Game | FAILED")
            If vTryAgain = vbRetry Then
                vSameDigits = MsgBox("Do you want to use the same numbers?", vbQuestion + vbYesNo, "Rosetta Code | 24 Game | RETRY")
                If vSameDigits = vbYes Then
                    GoTo GetUserExpression
                Else
                    GoTo GenerateNewDigits
                End If
            End If
    Else
        vTryAgain = MsgBox("You entered: " & stUserExpression & vbCr & vbCr & "which resulted in: " & vResult, _
            vbInformation + vbRetryCancel, "Rosetta Code | 24 Game | SUCCESS")
        If vTryAgain = vbRetry Then
            GoTo GenerateNewDigits
        End If
    End If
    Exit Sub
EvalFail:
    bValidExpression = False
    vResult = Err.Description
    Resume
End Sub


```



## Yabasic

With reverse polish notation

```Yabasic
operadores$ = "*/+-"
espacios$ = "                                                                                "

clear screen
print "24 Game"
print "
### ======
\n"
print "The player is provided with 4 numbers with which to perform operations"
print "of addition (+), subtraction (-), multiplication (*) or division (/) to attempt"
print "to get 24 as result."
print "Use reverse Polish notation (first the operands and then the operators)."
print "For example: instead of 2 + 4, type 2 4 +\n\n"

repeat
	print at(0,9) espacios$, espacios$, espacios$, espacios$, espacios$, espacios$
	print at(0,9);
	serie$ = ordenaCadena$(genSerie$())
	validos$ = serie$+operadores$
	line input "Enter your formula in reverse Polish notation: " entrada$
	entrada$ = quitaEspacios$(entrada$)
	entradaOrd$ = ordenaCadena$(entrada$)
	if (right$(entradaOrd$,4) <> serie$) or (len(entradaOrd$)<>7) then
		print "Error in the entered series"
	else
		resultado = evaluaEntrada(entrada$)
		print "The result is = ",resultado," "
		if resultado = 24 then
			print "Correct!"
		else
			print "Error!"
		end if
	end if
	print "Want to try again? (press N to exit, or another key to continue)"
until(upper$(left$(inkey$(),1)) = "N")

sub genSerie$()
	local i, c$, s$

	print "The numbers to be used are: ";
	i = ran()
	for i = 1 to 4
		c$ = str$(int(ran(9))+1)
		print c$," ";
		s$ = s$ + c$
	next i
	print
	return s$
end sub


sub evaluaEntrada(entr$)
	local d1, d2, c$, n(4), i

	while(entr$<>"")
		c$ = left$(entr$,1)
		entr$ = mid$(entr$,2)
		if instr(serie$,c$) then
			i = i + 1
			n(i) = val(c$)
		elseif instr(operadores$,c$) then
			d2 = n(i)
			n(i) = 0
			i = i - 1
			d1 = n(i)
			n(i) = evaluador(d1, d2, c$)
		else
			print "Invalid sign"
			return
		end if
	wend

	return n(i)

end sub


sub evaluador(d1, d2, op$)
	local t

	switch op$
		case "+": t = d1 + d2 : break
		case "-": t = d1 - d2 : break
		case "*": t = d1 * d2 : break
		case "/": t = d1 / d2 : break
	end switch

	return t
end sub


sub quitaEspacios$(entr$)
	local n, i, s$, t$(1)

	n = token(entr$,t$()," ")

	for i=1 to n
		s$ = s$ + t$(i)
	next i
	return s$
end sub


sub ordenaCadena$(cadena$)
	local testigo, n, fin, c$

	fin = len(cadena$)-1
	repeat
		testigo = false
		for n = 1 to fin
			if mid$(cadena$,n,1) > mid$(cadena$,n+1,1) then
				testigo = true
				c$ = mid$(cadena$,n,1)
				mid$(cadena$,n,1) = mid$(cadena$,n+1,1)
				mid$(cadena$,n+1,1) = c$
			end if
		next n
	until(testigo = false)
	return cadena$
end sub
```


{{trans|ZX Spectrum Basic}}

```Yabasic
do
	clear screen
	n$=""
	for i=1 to 4
		n$=n$+str$(int(ran(9)+1))
	next i
	print at(35,0) "24 GAME\n"
	print "Combine numbers and operators for obtain 24."
	print "Allowed characters: ";
	i$=n$+"+-*/()"
	for i=1 to 10
		print mid$(i$,i,1)," ";
	next i
	print "[nothing to end program]\n"
label accept
	p$=""
	i$=n$+"+-*/()"
	print at(0,4);:input "Enter the formula: " f$
	if f$="" end
	for i=1 to len(f$)
		c$=mid$(f$,i,1)
		switch c$
			case " ": f$=left$(f$,i-1)+right$(f$,len(f$)-i): break
			case "+":case "-":case "*":case "/": p$=p$+"o": break
			case "(":case ")": p$=p$+c$: break
			case mid$(i$,1,1): p$=p$+"n": mid$(i$,1,1)=" ": break
			case mid$(i$,2,1): p$=p$+"n": mid$(i$,2,1)=" ": break
			case mid$(i$,3,1): p$=p$+"n": mid$(i$,3,1)=" ": break
			case mid$(i$,4,1): p$=p$+"n": mid$(i$,4,1)=" ": break
			default: message(" = Invalid argument!  "): break
		end switch
		if f$="" goto accept
	next i
	restore
	for i=1 to 11
		read t$
		if t$=p$ break
	next i
	if t$<>p$ then
		message(" = Bad construction!  ")
		goto accept
	end if
	let r=eval(f$)
	print "\n",f$," = ",r," ";
	if r<>24 then
		message(" = Wrong!             ")
		goto accept
	else
		message(" = Correct!           ")
		pause 5
	end if
loop

DATA "nononon"
DATA "(non)onon"
DATA "nono(non)"
DATA "no(no(non))"
DATA "((non)on)on"
DATA "no(non)on"
DATA "(non)o(non)"
DATA "no((non)on)"
DATA "(nonon)on"
DATA "(no(non))on"
DATA "no(nonon)"

sub message(m$)
	print at(0,7) f$,m$
	beep: beep: beep
	pause 1
	print at(19,4) "                     "
	f$=""
end sub

sub eval(c$)
	static linea

	linea=linea+1
	c$="sub s"+str$(linea+1000000,"#######")+"():return "+c$+":end sub"
	compile c$
	return execute(mid$(c$,5,8))
end sub

```




## zkl


```zkl
while(1){
   digits := [1..4].pump(String,(0).random.fpM("11-",1,9));
   exp := ask("Enter an expression using the digits ",digits,
	      " that evals to 24: ") - " \n";
   expf:=exp.apply(fcn(c){if ("1"<=c<="9") "(%s).toFloat()".fmt(c) else c});
   reg r;
   try { Compiler.Compiler.compileText(expf).__constructor(); r=vm.regX }
   catch { println("bad expression"); continue; }
   else{
      extra := (exp - "+-/*()" - digits);
      if (extra) { println("Extra goodies in expression: ",extra); continue; }
      (digits.split("").sort() != (exp - "+-/*()").split("").sort()) :
         if(_) { println("You can only use the digits ",digits," once each"); continue; };
      if (exp.matches("*[1-9][1-9]*"))
	 { println("no repeated digits"); continue; }
      if (r.closeTo(24,0.001)) "nice!".println();
      else println("That evaled to ",r,", not 24");
   }
}
```

### Output

```txt

Enter an expression using the digits 8833 that evals to 24: (8/(3-(8/3)))
nice!
Enter an expression using the digits 8833 that evals to 24: (8/(3-(8/9)))
Extra goodies in expression: 9
Enter an expression using the digits 8833 that evals to 24: (8/(3-(8/8)))
You can only use the digits 8833 once each
Enter an expression using the digits 7155 that evals to 24: 7+1+5+5
That evaled to 18, not 24
Enter an expression using the digits 8332 that evals to 24: 8*3*(3-2)
nice!

```



## ZX Spectrum Basic


```zxbasic
10 LET n$=""
20 RANDOMIZE
30 FOR i=1 TO 4
40 LET n$=n$+STR$ (INT (RND*9)+1)
50 NEXT i
60 LET i$="": LET f$="": LET p$=""
70 CLS
80 PRINT "24 game"
90 PRINT "Allowed characters:"
100 LET i$=n$+"+-*/()"
110 PRINT AT 4,0;
120 FOR i=1 TO 10
130 PRINT i$(i);" ";
140 NEXT i
150 PRINT "(0 to end)"
160 INPUT "Enter the formula";f$
170 IF f$="0" THEN STOP
180 PRINT AT 6,0;f$;" = ";
190 FOR i=1 TO LEN f$
200 LET c$=f$(i)
210 IF c$=" " THEN LET f$(i)="": GO TO 250
220 IF c$="+" OR c$="-" OR c$="*" OR c$="/" THEN LET p$=p$+"o": GO TO 250
230 IF c$="(" OR c$=")" THEN LET p$=p$+c$: GO TO 250
240 LET p$=p$+"n"
250 NEXT i
260 RESTORE
270 FOR i=1 TO 11
280 READ t$
290 IF t$=p$ THEN LET i=11
300 NEXT i
310 IF t$<>p$ THEN PRINT INVERSE 1;"Bad construction!": BEEP 1,.1: PAUSE 0: GO TO 60
320 FOR i=1 TO LEN f$
330 FOR j=1 TO 10
340 IF (f$(i)=i$(j)) AND f$(i)>"0" AND f$(i)<="9" THEN LET i$(j)=" "
350 NEXT j
360 NEXT i
370 IF i$( TO 4)<>"    " THEN PRINT FLASH 1;"Invalid arguments!": BEEP 1,.01: PAUSE 0: GO TO 60
380 LET r=VAL f$
390 PRINT r;" ";
400 IF r<>24 THEN PRINT FLASH 1;"Wrong!": BEEP 1,1: PAUSE 0: GO TO 60
410 PRINT FLASH 1;"Correct!": PAUSE 0: GO TO 10
420 DATA "nononon"
430 DATA "(non)onon"
440 DATA "nono(non)"
450 DATA "no(no(non))"
460 DATA "((non)on)on"
470 DATA "no(non)on"
480 DATA "(non)o(non)"
485 DATA "no((non)on)"
490 DATA "(nonon)on"
495 DATA "(no(non))on"
500 DATA "no(nonon)"
```

