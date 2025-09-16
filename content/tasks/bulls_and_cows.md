+++
title = "Bulls and cows"
description = ""
date = 2019-10-22T04:53:38Z
aliases = []
[extra]
id = 4126
[taxonomies]
categories = ["Games", "Puzzles", "task"]
tags = []
languages = [
  "ada",
  "algol_68",
  "applescript",
  "applesoft_basic",
  "autohotkey",
  "awk",
  "basic",
  "batch_file",
  "bbc_basic",
  "brat",
  "c",
  "ceylon",
  "clojure",
  "coco",
  "commodore_basic",
  "common_lisp",
  "cpp",
  "crystal",
  "csharp",
  "d",
  "e",
  "easylang",
  "eiffel",
  "elena",
  "elixir",
  "erlang",
  "euphoria",
  "factor",
  "fan",
  "forth",
  "fortran",
  "go",
  "golo",
  "haskell",
  "hy",
  "j",
  "java",
  "javascript",
  "julia",
  "kotlin",
  "lasso",
  "liberty_basic",
  "logo",
  "lua",
  "m2000_interpreter",
  "maple",
  "mathematica",
  "matlab",
  "maxscript",
  "miniscript",
  "mumps",
  "nim",
  "ocaml",
  "oforth",
  "oorexx",
  "oz",
  "pari_gp",
  "pascal",
  "perl",
  "perl_6",
  "phix",
  "php",
  "picolisp",
  "powershell",
  "processing",
  "prolog",
  "purebasic",
  "python",
  "r",
  "racket",
  "red",
  "rexx",
  "ring",
  "ruby",
  "rust",
  "scala",
  "scheme",
  "seed7",
  "sidef",
  "smalltalk",
  "smart_basic",
  "swift",
  "tcl",
  "tuscript",
  "ubasic_4th",
  "unix_shell",
  "vba",
  "vedit_macro_language",
  "visual_basic_.net",
  "zkl",
  "zx_spectrum_basic",
]
+++

## Task

[[wp:Bulls and Cows|Bulls and Cows]]   is an old game played with pencil and paper that was later implemented using computers.


;Task:
Create a four digit random number from the digits   '''1'''   to   '''9''',   without duplication.

The program should:
::::::*   ask for guesses to this number
::::::*   reject guesses that are malformed
::::::*   print the score for the guess


The score is computed as:
# The player wins if the guess is the same as the randomly chosen number, and the program ends.
# A score of one '''bull''' is accumulated for each digit in the guess that equals the corresponding digit in the randomly chosen initial number.
# A score of one '''cow''' is accumulated for each digit in the guess that also appears in the randomly chosen number, but in the wrong position.


;Related tasks:
*   [[Bulls and cows/Player]]
*   [[Guess the number]]
*   [[Guess the number/With Feedback]]
*   [[Mastermind]]





## Ada


```Ada
with Ada.Text_IO;  use Ada.Text_IO;
with Ada.Numerics.Discrete_Random;

procedure Bulls_And_Cows is
   package Random_Natural is new Ada.Numerics.Discrete_Random (Natural);
   Number : String (1..4);
begin
   declare -- Generation of number
      use Random_Natural;
      Digit    : String   := "123456789";
      Size     : Positive := 9;
      Dice     : Generator;
      Position : Natural;
   begin
      Reset (Dice);
      for I in Number'Range loop
         Position := Random (Dice) mod Size + 1;
         Number (I) := Digit (Position);
         Digit (Position..Size - 1) := Digit (Position + 1..Size);
         Size := Size - 1;
      end loop;
   end;
   loop -- Guessing loop
      Put ("Enter four digits:");
      declare
         Guess : String  := Get_Line;
         Bulls : Natural := 0;
         Cows  : Natural := 0;
      begin
         if Guess'Length /= 4 then
            raise Data_Error;
         end if;
         for I in Guess'Range loop
            for J in Number'Range loop
               if Guess (I) not in '1'..'9' or else (I < J and then Guess (I) = Guess (J)) then
                  raise Data_Error;
               end if;
               if Number (I) = Guess (J) then
                  if I = J then
                     Bulls := Bulls + 1;
                  else
                     Cows := Cows + 1;
                  end if;
               end if;
            end loop;
         end loop;
         exit when Bulls = 4;
         Put_Line (Integer'Image (Bulls) & " bulls," & Integer'Image (Cows) & " cows");
      exception
         when Data_Error => Put_Line ("You should enter four different digits 1..9");
      end;
   end loop;
end Bulls_And_Cows;
```



## ALGOL 68

{{trans|Python}}

{{works with|ALGOL 68|Revision 1 - no extensions to language used}}

{{works with|ALGOL 68G|Any - tested with release [http://sourceforge.net/projects/algol68/files/algol68g/algol68g-1.18.0/algol68g-1.18.0-9h.tiny.el5.centos.fc11.i386.rpm/download 1.18.0-9h.tiny]}}

{{works with|ELLA ALGOL 68|Any (with appropriate job cards) - tested with release [http://sourceforge.net/projects/algol68/files/algol68toc/algol68toc-1.8.8d/algol68toc-1.8-8d.fc9.i386.rpm/download 1.8-8d]}}

```algol68
STRING digits = "123456789";

[4]CHAR chosen;
STRING available := digits;
FOR i TO UPB chosen DO
    INT c = ENTIER(random*UPB available)+1;
    chosen[i] := available[c];
    available := available[:c-1]+available[c+1:]
OD;

COMMENT print((chosen, new line)); # Debug # END COMMENT

OP D = (INT d)STRING: whole(d,0); # for formatting an integer #

print (("I have chosen a number from ",D UPB chosen," unique digits from 1 to 9 arranged in a random order.", new line,
"You need to input a ",D UPB chosen," digit, unique digit number as a guess at what I have chosen", new line));

PRIO WITHIN = 5, NOTWITHIN = 5;
OP WITHIN = (CHAR c, []CHAR s)BOOL: char in string(c,LOC INT,s);
OP NOTWITHIN = (CHAR c, []CHAR s)BOOL: NOT ( c WITHIN s );

INT guesses := 0, bulls, cows;
WHILE
    STRING guess;
    guesses +:= 1;
    WHILE
        # get a good guess #
        print((new line,"Next guess [",D guesses,"]: "));
        read((guess, new line));
        IF UPB guess NE UPB chosen THEN
            FALSE
        ELSE
            BOOL ok;
            FOR i TO UPB guess WHILE
                ok := guess[i] WITHIN digits AND guess[i] NOTWITHIN guess[i+1:]
            DO SKIP OD;
            NOT ok
        FI
    DO
        print(("Problem, try again. You need to enter ",D UPB chosen," unique digits from 1 to 9", new line))
    OD;
# WHILE #
    guess NE chosen
DO
    bulls := cows := 0;
    FOR i TO UPB chosen DO
        IF guess[i] = chosen[i] THEN
            bulls +:= 1
        ELIF guess[i] WITHIN chosen THEN
            cows +:= 1
        FI
    OD;
    print(("  ",D bulls," Bulls",new line,"  ",D cows," Cows"))
OD;
print((new line, "Congratulations you guessed correctly in ",D guesses," attempts.",new line))
```

Output:

```txt

I have chosen a number from 4 unique digits from 1 to 9 arranged in a random order.
You need to input a 4 digit, unique digit number as a guess at what I have chosen

Next guess [1]:

```



## AppleScript


GUI implementation; the prompt for a guess includes a list of all past guesses and their scores.


```applescript
on pickNumber()
	set theNumber to ""
	repeat 4 times
		set theDigit to (random number from 1 to 9) as string
		repeat while (offset of theDigit in theNumber) > 0
			set theDigit to (random number from 1 to 9) as string
		end repeat
		set theNumber to theNumber & theDigit
	end repeat
end pickNumber

to bulls of theGuess given key:theKey
	set bullCount to 0
	repeat with theIndex from 1 to 4
		if text theIndex of theGuess = text theIndex of theKey then
			set bullCount to bullCount + 1
		end if
	end repeat
	return bullCount
end bulls

to cows of theGuess given key:theKey, bulls:bullCount
	set cowCount to -bullCount
	repeat with theIndex from 1 to 4
		if (offset of (text theIndex of theKey) in theGuess) > 0 then
			set cowCount to cowCount + 1
		end if
	end repeat

	return cowCount
end cows

to score of theGuess given key:theKey
	set bullCount to bulls of theGuess given key:theKey
	set cowCount to cows of theGuess given key:theKey, bulls:bullCount
	return {bulls:bullCount, cows:cowCount}
end score

on run
	set theNumber to pickNumber()
	set pastGuesses to {}
	repeat
		set theMessage to ""
		repeat with aGuess in pastGuesses
			set {theGuess, theResult} to aGuess
			set theMessage to theMessage & theGuess & ":" & bulls of theResult & "B, " & cows of theResult & "C" & linefeed
		end repeat
		set theMessage to theMessage & linefeed & "Enter guess:"
		set theGuess to text returned of (display dialog theMessage with title "Bulls and Cows" default answer "")
		set theScore to score of theGuess given key:theNumber
		if bulls of theScore is 4 then
			display dialog "Correct!  Found the secret in " & ((length of pastGuesses) + 1) & " guesses!"
			exit repeat
		else
			set end of pastGuesses to {theGuess, theScore}
		end if
	end repeat
end run
```



## AutoHotkey


```autohotkey
length:=4, Code:="" ; settings

While StrLen(Code) < length {
	Random, num, 1, 9
	If !InStr(Code, num)
		Code .= num
}
Gui, Add, Text, w83 vInfo, I'm thinking of a %length%-digit number with no duplicate digits.
Gui, Add, Edit, wp vGuess, Enter a guess...
Gui, Add, Button, wp Default vDefault, Submit
Gui, Add, Edit, ym w130 r8 vHistory ReadOnly
Gui, Show
Return

ButtonSubmit:
	If Default = Restart
		Reload
	Gui, Submit, NoHide
	If (StrLen(Guess) != length)
		GuiControl, , Info, Enter a %length%-digit number.
	Else If Guess is not digit
		GuiControl, , Info, Enter a %length%-digit number.
	Else
	{
		GuiControl, , Info
		GuiControl, , Guess
		If (Guess = Code)
		{
			GuiControl, , Info, Correct!
			GuiControl, , Default, Restart
			Default = Restart
		}
		response := Response(Guess, Code)
		Bulls := SubStr(response, 1, InStr(response,",")-1)
		Cows := SubStr(response, InStr(response,",")+1)
		GuiControl, , History, % History . Guess ": " Bulls " Bulls " Cows " Cows`n"
	}
Return

GuiEscape:
GuiClose:
	ExitApp

Response(Guess,Code) {
	Bulls := 0, Cows := 0
	Loop, % StrLen(Code)
		If (SubStr(Guess, A_Index, 1) = SubStr(Code, A_Index, 1))
			Bulls++
		Else If (InStr(Code, SubStr(Guess, A_Index, 1)))
			Cows++
	Return Bulls "," Cows
}
```


## AWK


```AWK
# Usage: GAWK -f BULLS_AND_COWS.AWK
BEGIN {
    srand()
    secret = ""
    for (i = 1; i <= 4; ) {
        digit = int(9 * rand()) + 1
        if (index(secret, digit) == 0) {
            secret = secret digit
            i++
        }
    }
    print "Welcome to 'Bulls and Cows'!"
    print "I thought of a 4-digit number."
    print "Please enter your guess."
}
iswellformed($0) {
    if (calcscore($0, secret)) {
        exit
    }
}
function iswellformed(number,    i, digit) {
    if (number !~ /[1-9][1-9][1-9][1-9]/) {
        print "Your guess should contain 4 digits, each from 1 to 9. Try again!"
        return 0
    }
    for (i = 1; i <= 3; i++) {
        digit = substr(number, 1, 1)
        number = substr(number, 2)
        if (index(number, digit) != 0) {
            print "Your guess contains a digit twice. Try again!"
            return 0
        }
    }
    return 1
}
function calcscore(guess, secret,    bulls, cows, i, idx) {
    # Bulls = correct digits at the right position
    # Cows = correct digits at the wrong position
    for (i = 1; i <= 4; i++) {
        idx = index(secret, substr(guess, i, 1))
        if (idx == i) {
            bulls++
        } else if (idx > 0) {
            cows++
        }
    }
    printf("Your score for this guess: Bulls = %d, Cows = %d.", bulls, cows)
    if (bulls < 4) {
        printf(" Try again!\n")
    } else {
        printf("\nCongratulations, you win!\n")
    }
    return bulls == 4
}
```

{{out}}

```txt

Welcome to 'Bulls and Cows'!
I thought of a 4-digit number.
Please enter your guess.
1234
Your score for this guess: Bulls = 0, Cows = 1. Try again!
5678
Your score for this guess: Bulls = 1, Cows = 1. Try again!
9651
Your score for this guess: Bulls = 1, Cows = 1. Try again!
5729
Your score for this guess: Bulls = 0, Cows = 3. Try again!
2695
Your score for this guess: Bulls = 2, Cows = 1. Try again!
2697
Your score for this guess: Bulls = 2, Cows = 2. Try again!
7692
Your score for this guess: Bulls = 4, Cows = 0.
Congratulations, you win!

```



## BASIC

{{works with|QBasic}}

```qbasic
DEFINT A-Z

DIM secret AS STRING
DIM guess  AS STRING
DIM c      AS STRING
DIM bulls, cows, guesses, i

RANDOMIZE TIMER
DO WHILE LEN(secret) < 4
    c = CHR$(INT(RND * 10) + 48)
    IF INSTR(secret, c) = 0 THEN secret = secret + c
LOOP

guesses = 0
DO
    INPUT "Guess a 4-digit number with no duplicate digits: "; guess
    guess = LTRIM$(RTRIM$(guess))
    IF LEN(guess) = 0 THEN EXIT DO

    IF LEN(guess) <> 4 OR VAL(guess) = 0 THEN
        PRINT "** You should enter 4 numeric digits!"
        GOTO looper
    END IF

    bulls = 0: cows = 0: guesses = guesses + 1
    FOR i = 1 TO 4
        c = MID$(secret, i, 1)
        IF MID$(guess, i, 1) = c THEN
            bulls = bulls + 1
        ELSEIF INSTR(guess, c) THEN
            cows = cows + 1
        END IF
    NEXT i
    PRINT bulls; " bulls, "; cows; " cows"

    IF guess = secret THEN
        PRINT "You won after "; guesses; " guesses!"
        EXIT DO
    END IF
looper:
LOOP
```


=
## Applesoft BASIC
=

```ApplesoftBasic
100 D$ = "123456789"
110 FOR I = 1 TO 4
120    P = INT(RND(1) * LEN(D$)) + 1
130    N$ = N$ + MID$(D$, P, 1)
140    D$ = MID$(D$, 1, P - 1) + MID$(D$, P + 1, 8)
150 NEXT
160 PRINT "A RANDOM NUMBER HAS BEEN CREATED.
170 PRINT "THE NUMBER HAS FOUR DIGITS FROM 1 TO 9, WITHOUT DUPLICATION."
200 FOR Q = 0 TO 1 STEP 0
210     INPUT "GUESS THE NUMBER: "; G%
220     G$ = STR$(G%)
230     M = LEN(G$) <> 4 OR G% = 0
240     IF NOT M THEN FOR I = 2 TO 4 : M = MID$(G$, I, 1) = "0" : IF NOT M THEN NEXT I
250     IF NOT M THEN FOR I = 1 TO 3 : FOR J = I + 1 TO 4 : M = MID$(G$, I, 1) = MID$(G$, J, 1) : IF NOT M THEN NEXT J, I
260     IF M THEN PRINT "THE GUESS IS MALFORMED." : NEXT Q
270     B = 0
280     C = 0
300     FOR I = 1 TO 4
310         C$ = MID$(N$, I, 1)
320         BULL = MID$(G$, I, 1) = C$
330         COW = 0
340         IF NOT BULL THEN FOR J = 1 TO 4 : COW = MID$(G$, J, 1) = C$ : IF NOT COW THEN NEXT J
350         B = B + BULL
360         C = C + COW
370     NEXT I
380     PRINT B " BULLS, " C " COWS"
390     Q = G$ = N$
400 NEXT Q
```


=
## Commodore BASIC
=

Based on the AppleSoft BASIC version. Modifications as follows:
* Accommodate 80 character BASIC line length (for line 250 especially), created subroutines.
* Booleans in Commodore BASIC evaluate to -1 for TRUE, therefore ABS function added to give desired results.
* Leading space in printed numbers (which is placeholder for negative sign) is included in string conversion in Commodore BASIC, therefore RIGHT$ function added on 220 to trim leading whitespace upon conversion.
* Other formatting (clear screen, etc.) unique to Commodore BASIC.


```FreeBasic
100 D$="123456789"
110 FOR I=1 TO 4
120 P=INT(RND(1)*LEN(D$))+1
130 N$=N$+MID$(D$,P,1)
140 D$=MID$(D$,1,P - 1)+MID$(D$,P+1,8)
150 NEXT
160 PRINT CHR$(147);"A RANDOM NUMBER HAS BEEN CREATED."
170 PRINT "THE NUMBER HAS FOUR DIGITS FROM 1 TO 9, WITHOUT DUPLICATION."
200 FOR Q=0 TO 1 STEP 0
210 INPUT "GUESS THE NUMBER: "; G%
219 REM CONVERT TO STRING AND TRIM LEADING SPACE
220 G$=RIGHT$(STR$(G%),4)
230 M=LEN(G$)<>4 OR G%=0
240 IF NOT M THEN GOSUB 600
250 IF NOT M THEN GOSUB 700
260 IF M THEN PRINT "THE GUESS IS MALFORMED.":NEXT Q
270 B=0
280 C=0
300 FOR I=1 TO 4
310 C$=MID$(N$,I,1)
320 BULL=MID$(G$,I,1)=C$
330 COW=0
340 IF NOT BULL THEN FOR J=1 TO 4:COW=MID$(G$,J,1)=C$:IF NOT COW THEN NEXT J
350 B=B+ABS(BULL)
360 C=C+ABS(COW)
370 NEXT I
380 PRINT "BULLS:";B:PRINT "COWS:";C
390 Q=ABS(G$=N$)
400 NEXT Q
500 PRINT "GOOD JOB!":END
600 FOR I=2 TO 4:M=MID$(G$,I,1)="0"
610 IF NOT M THEN NEXT I
620 RETURN
700 FOR I=1 TO 3
710 FOR J=I+1 TO 4
720 M=MID$(G$,I,1)=MID$(G$,J,1)
730 IF NOT M THEN NEXT J,I
740 RETURN

```



## Batch File


```dos

::
::Bulls and Cows Task from Rosetta Code Wiki
::Batch File Implementation
::
::Directly OPEN the Batch File to play...
::

@echo off
title Bulls and Cows Game
setlocal enabledelayedexpansion

::GENERATING THE CODE TO BE GUESSED BY PLAYER...
:begin
set list=123456789
set cnt=1
set code=
set tries=0
:gen
set /a mod=10-%cnt%
set /a rnd=%random%%%%mod%
set pick=!list:~%rnd%,1!
set code=%code%%pick%
set list=!list:%pick%=!
if %cnt%==4 (
	set c1=%code:~0,1%&set c2=%code:~1,1%&set c3=%code:~2,1%&set c4=%code:~3,1%
	goto :start
)
set /a cnt+=1
goto :gen
::/GENERATING THE CODE TO BE GUESSED BY PLAYER...

::GAME DISPLAY
:start
cls
echo.
echo Bulls and Cows Game
echo Batch File Implementation
echo.
echo NOTE: Please MAXIMIZE this command window.
echo.
echo Gameplay:
echo.
echo I have generated a 4-digit code from digit 1-9 WITHOUT duplication.
echo Your objective is to guess it. If your guess is equal to my code,
echo then you WIN. If not, I will score your guess:
echo.
echo ** A score of one BULL is accumulated for each digit that equals
echo the CORRESPONDING digit in my code.
echo.
echo ** A score of one COW is accumulated for each digit that appears
echo in your guess, but in the WRONG position.
echo.
echo Now, start guessing^^!
echo.
:game
echo.
set /p guess=Your Guess:
::/GAME DISPLAY

::INPUT VALIDATION
if !guess! gtr 9876 (echo Please input a valid guess.&goto :game)
if !guess! lss 1234 (echo Please input a valid guess.&goto :game)
set i1=%guess:~0,1%&set i2=%guess:~1,1%&set i3=%guess:~2,1%&set i4=%guess:~3,1%
set chk=1
:cycle
set /a tmp1=%chk%+1
for /l %%a in (%tmp1%,1,4) do (
	if !i%chk%!==!i%%a! (
		echo Please input a valid guess.&goto :game
	)
)
if %chk%==3 (
	goto :score
)
set /a chk+=1
goto :cycle
::/INPUT VALIDATION

::SCORING
:score
set /a tries+=1
if %guess%==%code% (goto :won)
set cow=0
set bull=0
for /l %%a in (1,1,4) do (
	if !i%%a!==!c%%a! (
		set /a bull+=1
	) else (
		set "entrycow=%%a"
		call :scorecow
	)
)
set guess=
echo BULLS=%bull% COWS=%cow%
goto :game

:scorecow
set nums=1 2 3 4
set put=!nums:%entrycow%=!
for %%b in (%put%) do (
	if !c%%b!==!i%entrycow%! (
		set /a cow+=1
		goto :EOF
	)
)
goto :EOF
::/SCORING

::ALREADY WON!
:won
echo.
echo.
echo After %tries% Tries, YOU CRACKED IT^^! My code is %code%.
echo.
set /p opt=Play again?(Y/N)
if /i "!opt!"=="y" (call :begin)
if /i "!opt!"=="n" (exit/b)
goto :won
::/ALREADY WON!

```



## BBC BASIC


```bbcbasic
      secret$ = ""
      REPEAT
        c$ = CHR$(&30 + RND(9))
        IF INSTR(secret$, c$) = 0 secret$ += c$
      UNTIL LEN(secret$) = 4

      PRINT "Guess a four-digit number with no digit used twice."'
      guesses% = 0
      REPEAT

        REPEAT
          INPUT "Enter your guess: " guess$
          IF LEN(guess$) <> 4 PRINT "Must be a four-digit number"
        UNTIL LEN(guess$) = 4
        guesses% += 1

        IF guess$ = secret$ PRINT "You won after "; guesses% " guesses!" : END

        bulls% = 0
        cows% = 0
        FOR i% = 1 TO 4
          c$ = MID$(secret$, i%, 1)
          IF MID$(guess$, i%, 1) = c$ THEN
            bulls% += 1
          ELSE IF INSTR(guess$, c$) THEN
              cows% += 1
            ENDIF
          ENDIF
        NEXT i%
        PRINT "You got " ;bulls% " bull(s) and " ;cows% " cow(s)."

      UNTIL FALSE

```



## Brat


```brat
secret_length = 4

secret = [1 2 3 4 5 6 7 8 9].shuffle.pop secret_length

score = { guess |
  cows = 0
  bulls = 0

  guess.each_with_index { digit, index |
    true? digit == secret[index]
      { bulls = bulls + 1 }
      { true? secret.include?(digit)
        { cows = cows + 1 }
      }
  }

  [cows: cows, bulls: bulls]
}

won = false
guesses = 1

p "I have chosen a number with four unique digits from 1 through 9. Can you guess it?"

while { not won }
  {
    print "Guess #{guesses}: "
    guess = g.strip.dice.map { d | d.to_i }

    when { guess == secret }  { p "You won in #{guesses} guesses!"; won = true }
      { guess.include?(0) || guess.include?(null) } { p "Your guess should only include digits 1 through 9." }
      { guess.length != secret.length } { p "Your guess was not the correct length. The number has exactly #{secret.length} digits." }
      { guess.unique.length != secret.length } { p "Each digit should only appear once in your guess." }
      { true } {
        result = score guess
        p "Score: #{result[:bulls]} bulls, #{result[:cows]} cows."
        guesses = guesses + 1
      }
  }
```



## C

{{libheader|ncurses}}

```c
#include <stdio.h>
#include <stdarg.h>
#include <stdlib.h>
#include <stdbool.h>
#include <curses.h>
#include <string.h>

#define MAX_NUM_TRIES 72
#define LINE_BEGIN 7
#define LAST_LINE 18

int yp=LINE_BEGIN, xp=0;

char number[5];
char guess[5];

#define MAX_STR 256
void mvaddstrf(int y, int x, const char *fmt, ...)
{
  va_list args;
  char buf[MAX_STR];

  va_start(args, fmt);
  vsprintf(buf, fmt, args);
  move(y, x);
  clrtoeol();
  addstr(buf);
  va_end(args);
}

void ask_for_a_number()
{
  int i=0;
  char symbols[] = "123456789";

  move(5,0); clrtoeol();
  addstr("Enter four digits: ");
  while(i<4) {
    int c = getch();
    if ( (c >= '1') && (c <= '9') && (symbols[c-'1']!=0) ) {
      addch(c);
      symbols[c-'1'] = 0;
      guess[i++] = c;
    }
  }
}

void choose_the_number()
{
  int i=0, j;
  char symbols[] = "123456789";

  while(i<4) {
    j = rand() % 9;
    if ( symbols[j] != 0 ) {
      number[i++] = symbols[j];
      symbols[j] = 0;
    }
  }
}
```


The following function contains the code to check how many bulls and cows there are.


```c
bool take_it_or_not()
{
  int i;
  int cows=0, bulls=0;

  for(i=0; i < 4; i++) {
    if ( number[i] == guess[i] ) {
      bulls++;
    } else if ( strchr(number, guess[i]) != NULL ) {
      cows++;
    }
  }
  move(yp, xp);
  addstr(guess); addch(' ');
  if ( bulls == 4 ) { yp++; return true; }
  if ( (cows==0) && (bulls==0) ) addch('-');
  while( cows-- > 0 ) addstr("O");
  while( bulls-- > 0 ) addstr("X");
  yp++;
  if ( yp > LAST_LINE ) {
    yp = LINE_BEGIN;
    xp += 10;
  }
  return false;
}

bool ask_play_again()
{
  int i;

  while(yp-- >= LINE_BEGIN) {
    move(yp, 0); clrtoeol();
  }
  yp = LINE_BEGIN; xp = 0;

  move(21,0); clrtoeol();
  addstr("Do you want to play again? [y/n]");
  while(true) {
    int a = getch();
    switch(a) {
    case 'y':
    case 'Y':
      return true;
    case 'n':
    case 'N':
      return false;
    }
  }
}


int main()
{
  bool bingo, again;
  int tries = 0;

  initscr(); cbreak(); noecho();
  clear();

  number[4] = guess[4] = 0;

  mvaddstr(0,0, "I choose a number made of 4 digits (from 1 to 9) without repetitions\n"
                "You enter a number of 4 digits, and I say you how many of them are\n"
                "in my secret number but in wrong position (cows or O), and how many\n"
                "are in the right position (bulls or X)");
  do {
    move(20,0); clrtoeol(); move(21, 0); clrtoeol();
    srand(time(NULL));
    choose_the_number();
    do {
      ask_for_a_number();
      bingo = take_it_or_not();
      tries++;
    } while(!bingo && (tries < MAX_NUM_TRIES));
    if ( bingo )
      mvaddstrf(20, 0, "You guessed %s correctly in %d attempts!", number, tries);
    else
      mvaddstrf(20,0, "Sorry, you had only %d tries...; the number was %s",
		MAX_NUM_TRIES, number);
    again = ask_play_again();
    tries = 0;
  } while(again);
  nocbreak(); echo(); endwin();
  return EXIT_SUCCESS;
}
```



## C++


```cpp
#include <iostream>
#include <string>
#include <algorithm>
#include <cstdlib>

bool contains_duplicates(std::string s)
{
  std::sort(s.begin(), s.end());
  return std::adjacent_find(s.begin(), s.end()) != s.end();
}

void game()
{
  typedef std::string::size_type index;

  std::string symbols = "0123456789";
  unsigned int const selection_length = 4;

  std::random_shuffle(symbols.begin(), symbols.end());
  std::string selection = symbols.substr(0, selection_length);
  std::string guess;
  while (std::cout << "Your guess? ", std::getline(std::cin, guess))
  {
    if (guess.length() != selection_length
        || guess.find_first_not_of(symbols) != std::string::npos
        || contains_duplicates(guess))
    {
      std::cout << guess << " is not a valid guess!";
      continue;
    }

    unsigned int bulls = 0;
    unsigned int cows = 0;
    for (index i = 0; i != selection_length; ++i)
    {
      index pos = selection.find(guess[i]);
      if (pos == i)
        ++bulls;
      else if (pos != std::string::npos)
        ++cows;
    }
    std::cout << bulls << " bulls, " << cows << " cows.\n";
    if (bulls == selection_length)
    {
      std::cout << "Congratulations! You have won!\n";
      return;
    }
  }
  std::cerr << "Oops! Something went wrong with input, or you've entered end-of-file!\nExiting ...\n";
  std::exit(EXIT_FAILURE);
}

int main()
{
  std::cout << "Welcome to bulls and cows!\nDo you want to play? ";
  std::string answer;
  while (true)
  {
    while (true)
    {
      if (!std::getline(std::cin, answer))
      {
        std::cout << "I can't get an answer. Exiting.\n";
        return EXIT_FAILURE;
      }
      if (answer == "yes" || answer == "Yes" || answer == "y" || answer == "Y")
        break;
      if (answer == "no" || answer == "No" || answer == "n" || answer == "N")
      {
        std::cout << "Ok. Goodbye.\n";
        return EXIT_SUCCESS;
      }
      std::cout << "Please answer yes or no: ";
    }
    game();
    std::cout << "Another game? ";
  }
}
```


## C#

```c#
using System;

namespace BullsnCows
{
    class Program
    {

        static void Main(string[] args)
        {
            int[] nums = new int[] { 1, 2, 3, 4, 5, 6, 7, 8, 9 };
            KnuthShuffle<int>(ref nums);
            int[] chosenNum = new int[4];
            Array.Copy(nums, chosenNum, 4);

            Console.WriteLine("Your Guess ?");
            while (!game(Console.ReadLine(), chosenNum))
            {
                Console.WriteLine("Your next Guess ?");
            }

            Console.ReadKey();
        }

        public static void KnuthShuffle<T>(ref T[] array)
        {
            System.Random random = new System.Random();
            for (int i = 0; i < array.Length; i++)
            {
                int j = random.Next(array.Length);
                T temp = array[i]; array[i] = array[j]; array[j] = temp;
            }
        }

        public static bool game(string guess, int[] num)
        {
            char[] guessed = guess.ToCharArray();
            int bullsCount = 0, cowsCount = 0;

            if (guessed.Length != 4)
            {
                Console.WriteLine("Not a valid guess.");
                return false;
            }

            for (int i = 0; i < 4; i++)
            {
                int curguess = (int) char.GetNumericValue(guessed[i]);
                if (curguess < 1 || curguess > 9)
                {
                    Console.WriteLine("Digit must be ge greater 0 and lower 10.");
                    return false;
                }
                if (curguess == num[i])
                {
                    bullsCount++;
                }
                else
                {
                    for (int j = 0; j < 4; j++)
                    {
                        if (curguess == num[j])
                            cowsCount++;
                    }
                }
            }

            if (bullsCount == 4)
            {
                Console.WriteLine("Congratulations! You have won!");
                return true;
            }
            else
            {
                Console.WriteLine("Your Score is {0} bulls and {1} cows", bullsCount, cowsCount);
                return false;
            }
        }
    }
}

```



## Ceylon


```ceylon
import ceylon.random {
    DefaultRandom
}

shared void run() {

    value random = DefaultRandom();

    function generateDigits() =>
            random.elements(1..9).distinct.take(4).sequence();

    function validate(String guess) {
        variable value ok = true;
        if (!guess.every((Character element) => element.digit)) {
            print("numbers only, please");
            ok = false;
        }
        if ('0' in guess) {
            print("only 1 to 9, please");
            ok = false;
        }
        if (guess.distinct.shorterThan(guess.size)) {
            print("no duplicates, please");
            ok = false;
        }
        if (guess.size != 4) {
            print("4 digits please");
            ok = false;
        }
        return ok;
    }

    function score({Integer*} target, {Integer*} guess) {
        variable value bulls = 0;
        variable value cows = 0;
        for ([a, b] in zipPairs(target, guess)) {
            if (a == b) {
                bulls++;
            } else if (target.contains(b)) {
                cows++;
            }
        }
        return [bulls, cows];
    }

    while (true) {
        value digits = generateDigits();
        print("I have chosen my four digits, please guess what they are.
               Use only the digits 1 to 9 with no duplicates and enter them with no spaces. eg 1234
               Enter q or Q to quit.");
        while (true) {
            if (exists line = process.readLine()) {
                if (line.uppercased == "Q") {
                    return;
                }
                if (validate(line)) {
                    value guessDigits = line.map((Character element) => Integer.parse(element.string)).narrow<Integer>();
                    value [bulls, cows] = score(digits, guessDigits);
                    if (bulls == 4) {
                        print("You win!");
                        break;
                    }
                    else {
                        print("Bulls: ``bulls``, Cows:  ``cows``");
                    }
                }
            }
        }
    }
}
```



## Clojure


```clojure

(ns bulls-and-cows)

(defn bulls [guess solution]
  (count (filter true? (map = guess solution))))

(defn cows [guess solution]
  (-
   (count (filter (set solution) guess))
   (bulls guess solution)))

(defn valid-input?
  "checks whether the string is a 4 digit number with unique digits"
  [user-input]
  (if (re-seq #"^(?!.*(\d).*\1)\d{4}$" user-input)
    true
    false))

(defn enter-guess []
  "Let the user enter a guess. Verify the input. Repeat until valid.
returns a list of digits enters by the user (# # # #)"
    (println "Enter your guess: ")
    (let [guess (read-line)]
      (if (valid-input? guess)
        (map #(Character/digit % 10) guess)
        (recur))))

(defn bulls-and-cows []
  "generate a random 4 digit number from the list of (1 ... 9): no repeating digits
player tries to guess the number with bull and cows rules gameplay"
  (let [solution ( take 4 (shuffle (range 1 10)))]
    (println "lets play some bulls and cows!")
    (loop [guess (enter-guess)]
      (println (bulls guess solution) " bulls and " (cows guess solution) " cows.")
      (if (not= guess solution)
        (recur (enter-guess))
        (println "You have won!")))))

(bulls-and-cows)

```



## Coco


{{libheader|Underscore.js}}

To handle I/O, we use functions named <code>say</code> (which simply outputs a string) and <code>prompt</code> (which takes a prompt string to display to the user and returns a line of input, without a trailing newline). These require platform-specific implementations. Here's how they can be implemented for the SpiderMonkey shell:


```coco
say = print
prompt = (str) ->
    putstr str
    readline! ? quit!
```


We can now solve the task using <code>say</code> and <code>prompt</code>:


```coco
const SIZE = 4

secret = _.sample ['1' to '9'], SIZE

for ever
    var guess
    for ever
        guess := _.uniq prompt 'Enter a guess: '
        if guess.length === SIZE and not _.difference guess, ['1' to '9'] .length
            break
        say 'Malformed guess; try again.'
    bulls = cows = 0
    for i til SIZE
        if guess[i] === secret[i]
            ++bulls
        else if _.contains secret, guess[i]
            ++cows
    if bulls === SIZE
        break
    say "#bulls bull#{[if bulls !== 1 then 's']}, #cows cow#{[if cows !== 1 then 's']}."

say 'A winner is you!'
```



## Common Lisp


```lisp
(defun get-number ()
  (do ((digits '()))
      ((>= (length digits) 4) digits)
    (pushnew (1+ (random 9)) digits)))

(defun compute-score (guess number)
  (let ((cows  0)
        (bulls 0))
    (map nil (lambda (guess-digit number-digit)
               (cond ((= guess-digit number-digit) (incf bulls))
                     ((member guess-digit number)  (incf cows))))
         guess number)
    (values cows bulls)))

(defun number->guess (number)
  (when (integerp number)
    (do ((digits '()))
        ((zerop number) digits)
      (multiple-value-bind (quotient remainder) (floor number 10)
        (push remainder digits)
        (setf number quotient)))))

(defun valid-guess-p (guess)
  (and (= 4 (length guess))
       (every (lambda (digit) (<= 1 digit 9)) guess)
       (equal guess (remove-duplicates guess))))

(defun play-game (&optional (stream *query-io*))
  (do ((number (get-number))
       (cows   0)
       (bulls  0))
      ((= 4 bulls))
    (format stream "~&Guess a 4-digit number: ")
    (let ((guess (number->guess (read stream))))
      (cond ((not (valid-guess-p guess))
             (format stream "~&Malformed guess."))
            (t
             (setf (values cows bulls) (compute-score guess number))
             (if (= 4 bulls)
               (format stream "~&Correct, you win!")
               (format stream "~&Score: ~a cows, ~a bulls."
                       cows bulls)))))))
```


## Crystal

{{trans|Ruby}}

```Ruby
size = 4
secret = ('1'..'9').to_a.sample(size)
guess = [] of Char

i = 0
loop do
  i += 1
  loop do
    print "Guess #{i}: "
    guess = gets.not_nil!.chomp.chars
    exit if guess.empty?

    break if guess.size == size &&
             guess.all? { |x| ('1'..'9').includes? x } &&
             guess.uniq.size == size

    puts "Problem, try again. You need to enter #{size} unique digits from 1 to 9"
  end

  if guess == secret
    puts "Congratulations you guessed correctly in #{i} attempts"
    break
  end

  bulls = cows = 0
  size.times do |j|
    if guess[j] == secret[j]
      bulls += 1
    elsif secret.includes? guess[j]
      cows += 1
    end
  end

  puts "Bulls: #{bulls}; Cows: #{cows}"
end
```



## D


```d
void main() {
    import std.stdio, std.random, std.string, std.algorithm,
           std.range, std.ascii;

    immutable hidden = "123456789"d.randomCover.take(4).array;
    while (true) {
        "Next guess: ".write;
        const d = readln.strip.array.sort().release;
        if (d.count == 4 && d.all!isDigit && d.uniq.count == 4) {
            immutable bulls = d.zip(hidden).count!q{ a[0] == a[1] },
                      cows = d.count!(g => hidden.canFind(g)) - bulls;
            if (bulls == 4)
                return " You guessed it!".writeln;
            writefln("bulls %d, cows %d", bulls, cows);
        }
        " Bad guess! (4 unique digits, 1-9)".writeln;
    }
}
```

{{out}}

```txt
Next guess: 6548
bulls 0, cows 3
 Bad guess! (4 unique digits, 1-9)
Next guess: 5284
bulls 2, cows 1
 Bad guess! (4 unique digits, 1-9)
Next guess: 4386
bulls 0, cows 2
 Bad guess! (4 unique digits, 1-9)
Next guess: -
 Bad guess! (4 unique digits, 1-9)
Next guess: 5894
bulls 3, cows 0
 Bad guess! (4 unique digits, 1-9)
Next guess: 5874
bulls 3, cows 0
 Bad guess! (4 unique digits, 1-9)
Next guess: 5814
 You guessed it!
```



## E


Note: This example was deliberately written in an abstracted style, separating out the algorithms, game logic, and UI.


```e
def Digit := 1..9
def Number := Tuple[Digit,Digit,Digit,Digit]

/** Choose a random number to be guessed. */
def pick4(entropy) {
    def digits := [1,2,3,4,5,6,7,8,9].diverge()

    # Partial Fisher-Yates shuffle
    for i in 0..!4 {
        def other := entropy.nextInt(digits.size() - i) + i
        def t := digits[other]
        digits[other] := digits[i]
        digits[i] := t
    }
    return digits(0, 4)
}

/** Compute the score of a guess. */
def scoreGuess(actual :Number, guess :Number) {
    var bulls := 0
    var cows := 0
    for i => digit in guess {
        if (digit == actual[i]) {
            bulls += 1
        } else if (actual.indexOf1(digit) != -1) {
            cows += 1
        }
    }
    return [bulls, cows]
}

/** Parse a guess string into a list of digits (Number). */
def parseGuess(guessString, fail) :Number {
    if (guessString.size() != 4) {
        return fail(`I need four digits, not ${guessString.size()} digits.`)
    } else {
        var digits := []
        for c in guessString {
            if (('1'..'9')(c)) {
                digits with= c - '0'
            } else {
                return fail(`I need a digit from 1 to 9, not "$c".`)
            }
        }
        return digits
    }
}

/** The game loop: asking for guesses and reporting scores and win conditions.
    The return value is null or a broken reference if there was a problem. */
def bullsAndCows(askUserForGuess, tellUser, entropy) {
    def actual := pick4(entropy)

    def gameTurn() {
        return when (def guessString := askUserForGuess <- ()) -> {
            escape tellAndContinue {

                def guess := parseGuess(guessString, tellAndContinue)
                def [bulls, cows] := scoreGuess(actual, guess)

                if (bulls == 4) {
                    tellUser <- (`You got it! The number is $actual!`)
                    null
                } else {
                    tellAndContinue(`Your score for $guessString is $bulls bulls and $cows cows.`)
                }

            } catch message {
                # The parser or scorer has something to say, and the game continues afterward
                when (tellUser <- (message)) -> {
                    gameTurn()
                }
            }
        } catch p {
            # Unexpected problem of some sort
            tellUser <- ("Sorry, game crashed.")
            throw(p)
        }
    }

    return gameTurn()
}
```



### REPL user interface


<!-- GeSHI mangles unicode -->

```txt
def replBullsAndCows() {
    when (
        bullsAndCows(fn {
            def [guess, env] := e`def guess; guess`.evalToPair(interp.getTopScope().nestOuter())
            interp.setTopScope(env)
            println("Please type “ bind guess := \"your guess here\" ”.")
            guess
        }, println, entropy)
    ) -> {} catch p {
      println(`$p${p.eStack()}`)
    }
}
```



### Graphical user interface


{{works with|E-on-Java}} (Java Swing)


```e
def guiBullsAndCows() {
    var lastGuess := ""
    def op := <unsafe:javax.swing.makeJOptionPane>
    return bullsAndCows(fn {
      lastGuess := op.showInputDialog(null, "Enter your guess:", lastGuess)
      if (lastGuess == null) {
        # canceled, so just fail to return an answer and let the game logic get GCed
        Ref.promise()[0]
      } else {
        lastGuess
      }
    }, fn msg {
      op.showMessageDialog(null, msg)
    }, entropy)
}
```



## EasyLang

<lang>dig[] = [ 1 2 3 4 5 6 7 8 9 ]
for i range 4
  h = i + random (9 - i)
  swap dig[i] dig[h]
.
# print dig[]
len g[] 4
attempts = 0
repeat
  repeat
    ok = 0
    s$[] = str_split input
    if len s$[] = 4
      ok = 1
      for i range 4
        g[i] = number s$[i]
        if g[i] = 0
          ok = 0
        .
      .
    .
    until ok = 1
  .
  print g[]
  attempts += 1
  bulls = 0
  cows = 0
  for i range 4
    if g[i] = dig[i]
      bulls += 1
    else
      for j range 4
        if dig[j] = g[i]
          cows += 1
        .
      .
    .
  .
  print "bulls:" & bulls & " cows:" & cows
  until bulls = 4
.
print "Well done! " & attempts & " attempts needed."
```



## Eiffel


```Eiffel

class
	BULLS_AND_COWS

create
	execute

feature

	execute
			-- Initiate game.
		do
			io.put_string ("Let's play bulls and cows.%N")
			create answer.make_empty
			play
		end

feature {NONE}

	play
			-- Plays bulls ans cows.
		local
			count, seed: INTEGER
			guess: STRING
		do
			from
			until
				seed > 0
			loop
				io.put_string ("Enter a positive integer.%NYour play will be generated from it.%N")
				io.read_integer
				seed := io.last_integer
			end
			generate_answer (seed)
			io.put_string ("Your game has been created.%N Try to guess the four digit number.%N")
			create guess.make_empty
			from
			until
				guess ~ answer
			loop
				io.put_string ("Guess: ")
				io.read_line
				guess := io.last_string
				if guess.count = 4 and guess.is_natural and not guess.has ('0') then
					io.put_string (score (guess) + "%N")
					count := count + 1
				else
					io.put_string ("Your input does not have the correct format.")
				end
			end
			io.put_string ("Congratulations! You won with " + count.out + " guesses.")
		end

	answer: STRING

	generate_answer (s: INTEGER)
			-- Answer with 4-digits between 1 and 9 stored in 'answer'.
		require
			positive_seed: s > 0
		local
			random: RANDOM
			ran: INTEGER
		do
			create random.set_seed (s)
			from
			until
				answer.count = 4
			loop
				ran := (random.double_item * 10).floor
				if ran > 0 and not answer.has_substring (ran.out) then
					answer.append (ran.out)
				end
				random.forth
			end
		ensure
			answer_not_void: answer /= Void
			correct_length: answer.count = 4
		end

	score (g: STRING): STRING
			-- Score for the guess 'g' depending on 'answer'.
		require
			same_length: answer.count = g.count
		local
			k: INTEGER
			a, ge: STRING
		do
			Result := ""
			a := answer.twin
			ge := g.twin
			across
				1 |..| a.count as c
			loop
				if a [c.item] ~ ge [c.item] then
					Result := Result + "BULL "
					a [c.item] := ' '
					ge [c.item] := ' '
				end
			end
			across
				1 |..| a.count as c
			loop
				if a [c.item] /= ' ' then
					k := ge.index_of (a [c.item], 1)
					if k > 0 then
						Result := Result + "COW "
						ge [k] := ' '
					end
				end
			end
		end

end

```

{{out}}

```txt

Let's play bulls and cows.
Enter a positive integer.
Your play will be generated from it.
2

Guess: 2497
BULL COW

Guess: 2486
BULL COW COW

Guess: 2485
BULL COW COW

Guess: 2483
BULL BULL COW COW

Guess: 2438
BULL COW COW COW

Guess: 2843
BULL BULL BULL BULL
Congratulations! You won with 6 guesses.

```



## Elena

ELENA 4.1 :

```elena
import system'routines;
import extensions;

class GameMaster
{
    object theNumbers;
    object theAttempt;

    constructor()
    {
        // generate secret number
        var randomNumbers := new int[]::(1,2,3,4,5,6,7,8,9).randomize(9);

        theNumbers := randomNumbers.Subarray(0, 4);
        theAttempt := new Integer(1);
    }

    ask()
    {
        var row := console.print("Your Guess #",theAttempt," ?").readLine();

        ^ row.toArray()
    }

    proceed(guess)
    {
        int cows := 0;
        int bulls := 0;

        if (guess.Length != 4)
        {
            bulls := -1
        }
        else
        {
            try
            {
                for (int i := 0, i < 4, i+=1) {
                    var ch := guess[i];
                    var number := ch.toString().toInt();

                    // check range
                    ifnot (number > 0 && number < 10)
                        { InvalidArgumentException.raise() };

                    // check duplicates
                    var duplicate := guess.seekEach:(x => (x == ch)&&(x.equalReference(ch).Inverted));
                    if (nil != duplicate)
                    {
                        InvalidArgumentException.raise()
                    };

                    if (number == theNumbers[i])
                    {
                        bulls += 1
                    }
                    else
                    {
                        if (theNumbers.ifExists(number))
                            { cows += 1 }
                    }
                }
            }
            catch(Exception e)
            {
                bulls := -1
            }
        };

        bulls =>
            -1 { console.printLine:"Not a valid guess."; ^ true }
             4 { console.printLine:"Congratulations! You have won!"; ^ false }
             : {
                 theAttempt.append(1);

                 console.printLine("Your Score is ",bulls," bulls and ",cows," cows");

                 ^ true
             }
    }
}

public program()
{
    var gameMaster := new GameMaster();

    (lazy:gameMaster.proceed(gameMaster.ask())).doWhile();

    console.readChar()
}
```

{{out}}

```txt

Your Guess #1 ?2497
Your Score is 1 bulls and 1 cows
Your Guess #2 ?2486
Your Score is 1 bulls and 2 cows
Your Guess #3 ?2485
Your Score is 1 bulls and 2 cows
Your Guess #4 ?2483
Your Score is 2 bulls and 2 cows
Your Guess #5 ?2438
Your Score is 1 bulls and 3 cows
Your Guess #6 ?2843
Congratulations! You have won!

```



## Elixir

{{works with|Elixir|1.2}}

```elixir
defmodule Bulls_and_cows do
  def play(size \\ 4) do
    secret = Enum.take_random(1..9, size) |> Enum.map(&to_string/1)
    play(size, secret)
  end

  defp play(size, secret) do
    guess = input(size)
    if guess == secret do
      IO.puts "You win!"
    else
      {bulls, cows} = count(guess, secret)
      IO.puts "  Bulls: #{bulls}; Cows: #{cows}"
      play(size, secret)
    end
  end

  defp input(size) do
    guess = IO.gets("Enter your #{size}-digit guess: ") |> String.strip
    cond do
      guess == "" ->
        IO.puts "Give up"
        exit(:normal)
      String.length(guess)==size and String.match?(guess, ~r/^[1-9]+$/) ->
        String.codepoints(guess)
      true -> input(size)
    end
  end

  defp count(guess, secret) do
    Enum.zip(guess, secret) |>
    Enum.reduce({0,0}, fn {g,s},{bulls,cows} ->
      cond do
        g == s      -> {bulls + 1, cows}
        g in secret -> {bulls, cows + 1}
        true        -> {bulls, cows}
      end
    end)
  end
end

Bulls_and_cows.play
```


{{out}}

```txt

Enter your 4-digit guess: 1234
  Bulls: 1; Cows: 1
Enter your 4-digit guess: 5678
  Bulls: 0; Cows: 2
Enter your 4-digit guess: 2345
  Bulls: 0; Cows: 1
Enter your 4-digit guess: 1456
  Bulls: 1; Cows: 0
Enter your 4-digit guess: 1782
You win!

```



## Erlang

Module:

```erlang
-module(bulls_and_cows).
-export([generate_secret/0, score_guess/2, play/0]).

% generate the secret code
generate_secret() -> generate_secret([], 4, lists:seq(1,9)).
generate_secret(Secret, 0, _) -> Secret;
generate_secret(Secret, N, Digits) ->
  Next = lists:nth(random:uniform(length(Digits)), Digits),
  generate_secret(Secret ++ [Next], N - 1, Digits -- [Next]).

% evaluate a guess
score_guess(Secret, Guess)
  when length(Secret) =/= length(Guess) -> throw(badguess);
score_guess(Secret, Guess) ->
  Bulls = count_bulls(Secret,Guess),
  Cows = count_cows(Secret, Guess, Bulls),
  [Bulls, Cows].

% count bulls (exact matches)
count_bulls(Secret, Guess) ->
  length(lists:filter(fun(I) -> lists:nth(I,Secret) == lists:nth(I,Guess) end,
                      lists:seq(1, length(Secret)))).

% count cows (digits present but out of place)
count_cows(Secret, Guess, Bulls) ->
   length(lists:filter(fun(I) -> lists:member(I, Guess) end, Secret)) - Bulls.

% play a game
play() -> play_round(generate_secret()).

play_round(Secret) -> play_round(Secret, read_guess()).

play_round(Secret, Guess) ->
  play_round(Secret, Guess, score_guess(Secret,Guess)).

play_round(_, _, [4,0]) ->
  io:put_chars("Correct!\n");

play_round(Secret, _, Score) ->
  io:put_chars("\tbulls:"), io:write(hd(Score)), io:put_chars(", cows:"),
  io:write(hd(tl(Score))), io:put_chars("\n"), play_round(Secret).

read_guess() ->
  lists:map(fun(D)->D-48 end,
    lists:sublist(io:get_line("Enter your 4-digit guess: "), 4)).
```


Script:

```erlang
#!/usr/bin/escript
% Play Bulls and Cows
main(_) -> random:seed(now()), bulls_and_cows:play().
```


Sample play:
```txt
Enter your 4-digit guess: 8376
	bulls:1, cows:0
Enter your 4-digit guess: 8941
	bulls:1, cows:1
Enter your 4-digit guess: 8529
	bulls:1, cows:1
Enter your 4-digit guess: 4926
	bulls:1, cows:1
Enter your 4-digit guess: 9321
Correct!

```



## Euphoria

{{works with|Euphoria|4.0.3, 4.0.0 RC1 and later}}

```euphoria
include std\text.e
include std\os.e
include std\sequence.e
include std\console.e

sequence bcData = {0,0} --bull,cow score for the player
sequence goalNum = { {0,0,0,0}, {0,0,0,0}, 0} --computer's secret number digits (element 1), marked as bull/cow
--indexes in element 2, integer value of it in element 3
sequence currentGuess = { {0,0,0,0}, {0,0,0,0}, 0} --player's guess, same format as goalNum
sequence removeChars = 0 & " 0\r\t\n" --characters to trim (remove) from user's input. \r, \t are single escaped characters,
--0 is ascii 0x0 and number zero is ascii 48, or 0x30. The rest are wysiwyg
integer tries = 0 --track number of tries to guess the number
sequence bcStrings ={"bull", "cow"} --stores singular and/or plural strings depending on score in bcData

goalNum[1] = rand( {9,9,9,9} ) --rand function works on objects. here it outputs into each sequence element.
goalNum[3] = goalNum[1][1] * 1000 + goalNum[1][2] * 100 + goalNum[1][3] * 10 + goalNum[1][4] --convert digits to an integer
--and store it

procedure getInputAndProcess(integer stage = 1)  --object = 1 sets default value for the parameter if it isn't specified

    goalNum[2][1..4] = 0 --{0,0,0,0} --set these to unscaned (0) since the scanning will start over.
    currentGuess[1][1..4] = 0 --{0,0,0,0} --these too, or they will contain old marks
    currentGuess[2][1..4] = 0
    tries += 1 --equivalent to tries = tries + 1, but faster and shorter to write
    bcData[1..2] = 0 -- {0,0}

    if stage <= 1 then --if this process was run for the first time or with no parameters, then..
        puts(1,"The program has thought of a four digit number using only digits 1 to 9.\nType your guess and press enter.\n")
    end if

    while 1 label "guesscheck" do --labels can be used to specify a jump point from exit or retry, and help readability
        currentGuess[1] = trim(gets(0), removeChars) --get user input, trim unwanted characters from it, store it in currentGuess[1]
        currentGuess[1] = mapping( currentGuess[1], {49,50,51,52,53,54,55,56,57}, {1,2,3,4,5,6,7,8,9} ) --convert ascii codes to
        -- integer digits they represent
        integer tempF = find('0',currentGuess[1])
        if length(currentGuess[1]) != 4 or tempF != 0 then --if the input string is now more than 4 characters/integers,
            --the input won't be used.
            puts(1,"You probably typed too many digits or a 0. Try typing a new 4 digit number with only numbers 1 through 9.\n")
            retry "guesscheck"
            else
                exit "guesscheck"
        end if
    end while
    --convert separate digits to the one integer they represent and store it, like with goalNum[3]
    currentGuess[3] = currentGuess[1][1] * 1000 + currentGuess[1][2] * 100 + currentGuess[1][3] * 10 + currentGuess[1][4]
--convert digits to the integer they represent, to print to a string later

    --check for bulls
    for i = 1 to 4 do
        if goalNum[1][i] = currentGuess[1][i] then
            goalNum[2][i] = 1
            currentGuess[2][i] = 1
            bcData[1] += 1
        end if
    end for

    --check for cows, but not slots marked as bulls or cows already.
    for i = 1 to 4 label "iGuessElem"do --loop through each guessed digit
        for j = 1 to 4 label "jGoalElem" do --but first go through each goal digit, comparing the first guessed digit,
            --and then the other guessed digits 2 through 4

            if currentGuess[2][i] = 1 then --if the guessed digit we're comparing right now has been marked as bull or cow already
                continue "iGuessElem" --skip to the next guess digit without comparing this guess digit to the other goal digits
            end if

            if goalNum[2][j] = 1 then --if the goal digit we're comparing to right now has been marked as a bull or cow already
                continue "jGoalElem" --skip to the next goal digit
            end if

            if currentGuess[1][i] = goalNum[1][j] then --if the guessed digit is the same as the goal one,
                --it won't be a bull, so it's a cow
                bcData[2] += 1 --score one more cow
                goalNum[2][j] = 1 --mark this digit as a found cow in the subsequence that stores 0's or 1's as flags
                continue "iGuessElem" --skip to the next guess digit, so that this digit won't try to check for
                --matches(cow) with other goal digits
            end if

        end for --this guess digit was compared to one goal digit , try comparing this guess digit with the next goal digit
    end for --this guess digit was compared with all goal digits, compare the next guess digit to all the goal digits

    if bcData[1] = 1 then --uses singular noun when there is score of 1, else plural
        bcStrings[1] = "bull"
        else
            bcStrings[1] = "bulls"
    end if

    if bcData[2] = 1 then --the same kind of thing as above block
        bcStrings[2] = "cow"
        else
            bcStrings[2] = "cows"
    end if

    if bcData[1] < 4 then --if less than 4 bulls were found, the player hasn't won, else they have...
        printf(1, "Guess #%d : You guessed %d . You found %d %s, %d %s. Type new guess.\n", {tries, currentGuess[3], bcData[1], bcStrings[1], bcData[2], bcStrings[2]} )
        getInputAndProcess(2)
    else --else they have won and the procedure ends
        printf(1, "The number was %d. You guessed %d in %d tries.\n", {goalNum[3], currentGuess[3], tries} )
        any_key()--wait for keypress before closing console window.
    end if

end procedure
--run the procedure
getInputAndProcess(1)



```

Output :

```txt

The program has thought of a four digit number using only digits 1 to 9.
Type your guess and press enter.
7456
Guess #1 : You guessed 7456 . You found 1 bull, 1 cow. Type new guess.
7116
Guess #2 : You guessed 7116 . You found 1 bull, 0 cows. Type new guess.
7862
Guess #3 : You guessed 7862 . You found 0 bulls, 2 cows. Type new guess.
1826
[...etc]
6586
Guess #10 : You guessed 6586 . You found 3 bulls, 0 cows. Type new guess.
5586
Guess #11 : You guessed 5586 . You found 3 bulls, 0 cows. Type new guess.
2586
Guess #12 : You guessed 2586 . You found 3 bulls, 0 cows. Type new guess.
9586
The number was 9586. You guessed 9586 in 13 tries.
Press Any Key to continue...

```



## Factor


```Factor
USING: accessors assocs combinators fry grouping hashtables kernel
       locals math math.parser math.ranges random sequences strings
       io ascii ;

IN: bullsncows

TUPLE: score bulls cows ;
: <score> ( -- score ) 0 0 score boa ;

TUPLE: cow ;
: <cow> ( -- cow ) cow new ;

TUPLE: bull ;
: <bull> ( -- bull ) bull new ;

: inc-bulls ( score -- score ) dup bulls>> 1 + >>bulls ;
: inc-cows ( score -- score ) dup cows>> 1 + >>cows ;

: random-nums ( -- seq ) 9 [1,b] 4 sample ;

: add-digits ( seq -- n ) 0 [ swap 10 * + ] reduce number>string ;

: new-number ( -- n narr ) random-nums dup add-digits ;

: narr>nhash ( narr -- nhash ) { 1 2 3 4 } swap zip ;

: num>hash ( n -- hash )
    [ 1string string>number ] { } map-as narr>nhash ;

:: cow-or-bull ( n g -- arr )
    {
        { [ n first g at n second = ] [ <bull> ] }
        { [ n second g value? ] [ <cow> ] }
        [ f ]
    } cond ;

: add-to-score ( arr -- score )
   <score> [ bull? [ inc-bulls ] [ inc-cows ] if ] reduce ;

: check-win ( score -- ? ) bulls>> 4 = ;

: sum-score ( n g -- score ? )
    '[ _ cow-or-bull ] map sift add-to-score dup check-win ;

: print-sum ( score -- str )
    dup bulls>> number>string "Bulls: " swap append swap cows>> number>string
    " Cows: " swap 3append "\n" append ;

: (validate-readln) ( str -- ? ) dup length 4 = not swap [ letter? ] all? or ;

: validate-readln ( -- str )
    readln dup (validate-readln)
    [ "Invalid input.\nPlease enter a valid 4 digit number: "
      write flush drop validate-readln ]
    when ;

: win ( -- ) "\nYou've won! Good job. You're so smart." print flush ;

: main-loop ( x -- )
    "Enter a 4 digit number: " write flush validate-readln num>hash swap
    [ sum-score swap print-sum print flush ] keep swap not
    [ main-loop ] [ drop win ] if ;

: main ( -- ) new-number drop narr>nhash main-loop ;
```



## Fan


```Fan
**
** Bulls and cows. A game pre-dating, and similar to, Mastermind.
**
class BullsAndCows
{

  Void main()
  {
    digits := [1,2,3,4,5,6,7,8,9]
    size := 4
    chosen := [,]
    size.times { chosen.add(digits.removeAt(Int.random(0..<digits.size))) }

    echo("I've chosen $size unique digits from 1 to 9 at random.
          Try to guess my number!")
    guesses := 0
    while (true) // game loop
    {
      guesses += 1
      guess := Int[,]
      while (true) // input loop
      {
        // get a good guess
        Sys.out.print("\nNext guess [$guesses]: ")
        Sys.out.flush
        inString := Sys.in.readLine?.trim ?: ""
        inString.each |ch|
        { if (ch >= '1' && ch <= '9' && !guess.contains(ch)) guess.add(ch-'0') }
        if (guess.size == 4)
          break // input loop
        echo("Oops, try again. You need to enter $size unique digits from 1 to 9")
      }
      if (guess.all |v, i->Bool| { return v == chosen[i] })
      {
        echo("\nCongratulations! You guessed correctly in $guesses guesses")
        break // game loop
      }
      bulls := 0
      cows  := 0
      (0 ..< size).each |i|
      {
        if (guess[i] == chosen[i])
          bulls += 1
        else if (chosen.contains(guess[i]))
          cows += 1
      }
      echo("\n  $bulls Bulls\n  $cows Cows")
    }
  }
}
```



## Forth

{{works with|GNU Forth}}

```forth
include random.fs

create hidden 4 allot

: ok? ( str -- ? )
  dup 4 <> if 2drop false exit then
  1 9 lshift 1- -rot
  bounds do
    i c@ '1 -
    dup 0 9 within 0= if 2drop false leave then
    1 swap lshift over and
    dup 0= if nip leave then
    xor
  loop 0<> ;

: init
  begin
    hidden 4 bounds do 9 random '1 + i c! loop
    hidden 4 ok?
  until ;

: check? ( addr -- solved? )
  0
  4 0 do
    over i + c@
    4 0 do
      dup hidden i + c@ = if     swap
        i j = if 8 else 1 then + swap
      then
    loop drop
  loop nip
  8 /mod tuck . ." bulls, " . ." cows"
  4 = ;

: guess: ( "1234" -- )
  bl parse 2dup ok? 0= if 2drop ." Bad guess! (4 unique digits, 1-9)" exit then
  drop check? if cr ." You guessed it!" then ;
```

 init  ok
 guess: 1234 1 bulls, 0 cows ok
 guess: 1567 1 bulls, 1 cows ok
 guess: 1895 2 bulls, 1 cows ok
 guess: 1879 4 bulls, 0 cows
 You guessed it! ok


## Fortran

{{works with|Fortran|90 and later}}

```fortran
module bac
  implicit none

contains

  subroutine Gennum(n)
    integer, intent(out) :: n(4)
    integer :: i, j
    real :: r

    call random_number(r)
    n(1) = int(r * 9.0) + 1
    i = 2

outer: do while (i <= 4)
         call random_number(r)
         n(i) = int(r * 9.0) + 1
inner:   do j = i-1 , 1, -1
           if (n(j) == n(i)) cycle outer
         end do inner
         i = i + 1
       end do outer

  end subroutine Gennum

  subroutine Score(n, guess, b, c)
    character(*), intent(in) :: guess
    integer, intent(in) :: n(0:3)
    integer, intent(out) :: b, c
    integer :: digit, i, j, ind

    b = 0; c = 0
    do i = 1, 4
      read(guess(i:i), "(i1)") digit
      if (digit == n(i-1)) then
        b = b + 1
      else
        do j = i, i+2
          ind = mod(j, 4)
          if (digit == n(ind)) then
            c = c + 1
            exit
          end if
        end do
      end if
    end do

 end subroutine Score

end module bac

program Bulls_and_Cows
   use bac
   implicit none

   integer :: n(4)
   integer :: bulls=0, cows=0, tries=0
   character(4) :: guess

   call random_seed
   call Gennum(n)

   write(*,*) "I have selected a number made up of 4 digits (1-9) without repetitions."
   write(*,*) "You attempt to guess this number."
   write(*,*) "Every digit in your guess that is in the correct position scores 1 Bull"
   write(*,*) "Every digit in your guess that is in an incorrect position scores 1 Cow"
   write(*,*)

   do while (bulls /= 4)
     write(*,*) "Enter a 4 digit number"
     read*, guess
     if (verify(guess, "123456789") /= 0) then
       write(*,*) "That is an invalid entry. Please try again."
       cycle
     end if
     tries = tries + 1
     call Score (n, guess, bulls, cows)
     write(*, "(a, i1, a, i1, a)") "You scored ", bulls, " bulls and ", cows, " cows"
     write(*,*)
   end do

   write(*,"(a,i0,a)") "Congratulations! You correctly guessed the correct number in ", tries, " attempts"

end program Bulls_and_Cows
```



## Go


```go
package main

import (
    "bufio"
    "bytes"
    "fmt"
    "math/rand"
    "os"
    "strings"
    "time"
)

func main() {
    fmt.Println(`Cows and Bulls
Guess four digit number of unique digits in the range 1 to 9.
A correct digit but not in the correct place is a cow.
A correct digit in the correct place is a bull.`)
    // generate pattern
    pat := make([]byte, 4)
    rand.Seed(time.Now().Unix())
    r := rand.Perm(9)
    for i := range pat {
        pat[i] = '1' + byte(r[i])
    }

    // accept and score guesses
    valid := []byte("123456789")
guess:
    for in := bufio.NewReader(os.Stdin); ; {
        fmt.Print("Guess: ")
        guess, err := in.ReadString('\n')
        if err != nil {
            fmt.Println("\nSo, bye.")
            return
        }
        guess = strings.TrimSpace(guess)
        if len(guess) != 4 {
            // malformed:  not four characters
            fmt.Println("Please guess a four digit number.")
            continue
        }
        var cows, bulls int
        for ig, cg := range guess {
            if strings.IndexRune(guess[:ig], cg) >= 0 {
                // malformed:  repeated digit
                fmt.Printf("Repeated digit: %c\n", cg)
                continue guess
            }
            switch bytes.IndexByte(pat, byte(cg)) {
            case -1:
                if bytes.IndexByte(valid, byte(cg)) == -1 {
                    // malformed:  not a digit
                    fmt.Printf("Invalid digit: %c\n", cg)
                    continue guess
                }
            default: // I just think cows should go first
                cows++
            case ig:
                bulls++
            }
        }
        fmt.Printf("Cows: %d, bulls: %d\n", cows, bulls)
        if bulls == 4 {
            fmt.Println("You got it.")
            return
        }
    }
}
```



## Golo


```golo
#!/usr/bin/env golosh
----
This module is the Bulls and Cows game.
----
module Bullsandcows

import gololang.Decorators
import gololang.Functions
import gololang.IO
import java.util

function main = |args| {
  while true {
    let secret = create4RandomNumbers()
    println("Welcome to Bulls And Cows")
    while true {
      println("Please enter a 4 digit number")
      println("(with only the digits 1 to 9 and no repeated digits, for example 2537)")
      let guess = readln("guess: ")
      let result = validateGuess(guess)
      if result: isValid() {
        let bulls, cows = bullsAndOrCows(result: digits(), secret)
        if bulls is 4 {
          println("You win!")
          break
        }
        println("bulls: " + bulls)
        println("cows:  " + cows)
      } else {
        println(result: message())
      }
    }
  }
}

function create4RandomNumbers = {
  let numbers = vector[1, 2, 3, 4, 5, 6, 7, 8, 9]
  Collections.shuffle(numbers)
  let shuffled = numbers: subList(0, 4)
  return shuffled
}

union Result = {
  Valid = { digits }
  Invalid = { message }
}

@checkArguments(isString())
function validateGuess = |guess| {
  var digits = []
  try {
    let number = guess: toInt()
    digits = number: digits()
    if digits: exists(|d| -> d is 0) {
      return Result.Invalid("No zeroes please")
    }
    if digits: size() isnt 4  {
      return Result.Invalid("Four digits please")
    }
    let digitSet = set[ digit foreach digit in digits ]
    if digitSet: size() < digits: size() {
      return Result.Invalid("No duplicate numbers please")
    }
  } catch(e) {
    return Result.Invalid("Numbers only please")
  }
  return Result.Valid(digits)
}

let is4Vector = |arg| -> arg oftype java.util.List.class and arg: size() is 4

@checkArguments(is4Vector, is4Vector)
function bullsAndOrCows = |guess, secret| {
  var bulls = 0
  var cows = 0
  foreach i in [0..4] {
    let guessNum = guess: get(i)
    let secretNum = secret: get(i)
    if guessNum is secretNum {
      bulls = bulls + 1
    } else if secret: exists(|num| -> guessNum is num) {
      cows = cows + 1
    }
  }
  return [bulls, cows]
}

augment java.lang.Integer {

  function digits = |this| {
    var remaining = this
    let digits = vector[]
    while remaining > 0 {
      digits: prepend(remaining % 10)
      remaining = remaining / 10
    }
    return digits
  }
}

```



## Haskell


```haskell
import Data.List (partition, intersect, nub)
import Control.Monad
import System.Random (StdGen, getStdRandom, randomR)
import Text.Printf

numberOfDigits = 4 :: Int

main = bullsAndCows

bullsAndCows :: IO ()
bullsAndCows = do
    digits <- getStdRandom $ pick numberOfDigits ['1' .. '9']
    putStrLn "Guess away!"
    loop digits

  where loop digits = do
            input <- getLine
            if okay input
              then
                  let (bulls, cows) = score digits input in
                  if bulls == numberOfDigits then
                      putStrLn "You win!"
                  else do
                      printf "%d bulls, %d cows.\n" bulls cows
                      loop digits
              else do
                  putStrLn "Malformed guess; try again."
                  loop digits

        okay :: String -> Bool
        okay input =
            length input == numberOfDigits &&
            input == nub input &&
            all legalchar input
          where legalchar c = '1' <= c && c <= '9'

        score :: String -> String -> (Int, Int)
        score secret guess = (length bulls, cows)
          where (bulls, nonbulls) = partition (uncurry (==)) $
                    zip secret guess
                cows = length $ uncurry intersect $ unzip nonbulls

pick :: Int -> [a] -> StdGen -> ([a], StdGen)
{- Randomly selects items from a list without replacement. -}
pick n l g = f n l g (length l - 1) []
  where  f 0 _ g _   ps = (ps, g)
         f n l g max ps =
             f (n - 1) (left ++ right) g' (max - 1) (picked : ps)
          where (i, g') = randomR (0, max) g
                (left, picked : right) = splitAt i l
```



## Hy



```lisp
(import random)

(def +size+ 4)
(def +digits+ "123456789")
(def +secret+ (random.sample +digits+ +size+))

(while True
  (while True
    (setv guess (list (distinct (raw-input "Enter a guess: "))))
    (when (and
        (= (len guess) +size+)
        (all (map (fn [c] (in c +digits+)) guess)))
      (break))
    (print "Malformed guess; try again"))
  (setv bulls 0)
  (setv cows 0)
  (for [i (range +size+)] (cond
    [(= (get guess i) (get +secret+ i)) (setv bulls (inc bulls))]
    [(in (get guess i) +secret+) (setv cows (inc cows))]))
  (when (= bulls +size+)
    (break))
  (print (.format "{} bull{}, {} cows"
    bulls (if (= bulls 1) "" "s")
    cows (if (= cows 1) "" "s"))))

(print "A winner is you!")
```


=={{header|Icon}} and {{header|Unicon}}==

The following works in both Icon and Unicon.


```Unicon
procedure main()
    digits := "123456789"
    every !digits :=: ?digits
    num := digits[1+:4]
    repeat if score(num, getGuess(num)) then break
    write("Good job.")
end

procedure getGuess(num)
    repeat {
        writes("Enter a guess: ")
        guess := read() | stop("Quitter!")
        if *(guess ** '123456789') == *num then return guess
        write("Malformed guess: ",guess,". Try again.")
        }
end

procedure score(num, guess)
    bulls := 0
    cows := *(num ** guess)
    every (num[i := 1 to *num] == guess[i], bulls +:= 1, cows -:= 1)
    write("\t",bulls," bulls and ",cows," cows")
    return (bulls = *num)
end
```



## J


```j
require 'misc'

plural=: conjunction define
 (":m),' ',n,'s'#~1~:m
)

bullcow=:monad define
  number=. 1+4?9
  whilst. -.guess-:number do.
    guess=. 0 "."0 prompt 'Guess my number: '
    if. (4~:#guess)+.(4~:#~.guess)+.0 e.guess e.1+i.9 do.
      if. 0=#guess do.
        smoutput 'Giving up.'
        return.
      end.
      smoutput 'Guesses must be four different non-zero digits'
      continue.
    end.
    bulls=. +/guess=number
    cows=. (+/guess e.number)-bulls
    smoutput bulls plural 'bull',' and ',cows plural 'cow','.'
  end.
  smoutput 'you win'
)
```


For example:


```j
   bullcow''
Guess my number: 1234
0 bulls and 1 cow.
Guess my number: 5678
3 bulls and 0 cows.
Guess my number: 2349
0 bulls and 0 cows.
Guess my number: 1567
0 bulls and 3 cows.
Guess my number: 6178
3 bulls and 0 cows.
Guess my number: 6157
1 bull and 2 cows.
Guess my number: 5178
4 bulls and 0 cows.
you win
```



## Java


```java5
import java.util.InputMismatchException;
import java.util.Random;
import java.util.Scanner;

public class BullsAndCows{
	public static void main(String[] args){
		Random gen= new Random();
		int target= 0;
		while(hasDupes(target= (gen.nextInt(9000) + 1000)));
		String targetStr = target +"";
		boolean guessed = false;
		Scanner input = new Scanner(System.in);
		int guesses = 0;
		do{
			int bulls = 0;
			int cows = 0;
			System.out.print("Guess a 4-digit number with no duplicate digits: ");
			int guess;
			try{
				guess = input.nextInt();
				if(hasDupes(guess) || guess < 1000) continue;
			}catch(InputMismatchException e){
				continue;
			}
			guesses++;
			String guessStr = guess + "";
			for(int i= 0;i < 4;i++){
				if(guessStr.charAt(i) == targetStr.charAt(i)){
					bulls++;
				}else if(targetStr.contains(guessStr.charAt(i)+"")){
					cows++;
				}
			}
			if(bulls == 4){
				guessed = true;
			}else{
				System.out.println(cows+" Cows and "+bulls+" Bulls.");
			}
		}while(!guessed);
		System.out.println("You won after "+guesses+" guesses!");
	}

	public static boolean hasDupes(int num){
		boolean[] digs = new boolean[10];
		while(num > 0){
			if(digs[num%10]) return true;
			digs[num%10] = true;
			num/= 10;
		}
		return false;
	}
}
```

Output:

```txt
Guess a 4-digit number with no duplicate digits: 5834
2 Cows and 0 Bulls.
Guess a 4-digit number with no duplicate digits: 1234
1 Cows and 0 Bulls.
Guess a 4-digit number with no duplicate digits: 4321
1 Cows and 0 Bulls.
Guess a 4-digit number with no duplicate digits: 3421
0 Cows and 1 Bulls.
Guess a 4-digit number with no duplicate digits: 8412
0 Cows and 0 Bulls.
Guess a 4-digit number with no duplicate digits: 3560
1 Cows and 1 Bulls.
Guess a 4-digit number with no duplicate digits: 3650
0 Cows and 2 Bulls.
Guess a 4-digit number with no duplicate digits: 3759
2 Cows and 2 Bulls.
Guess a 4-digit number with no duplicate digits: 3975
2 Cows and 2 Bulls.
Guess a 4-digit number with no duplicate digits: 3957
You won after 10 guesses!
```





## JavaScript


###  Spidermonkey version


```javascript
#!/usr/bin/env js

function main() {
    var len = 4;
    playBullsAndCows(len);
}

function playBullsAndCows(len) {
    var num = pickNum(len);
    // print('The secret number is:\n  ' + num.join('\n  '));
    showInstructions(len);
    var nGuesses = 0;
    while (true) {
        nGuesses++;
        var guess = getGuess(nGuesses, len);
        var census = countBovine(num, guess);
        showScore(census.bulls, census.cows);
        if (census.bulls == len) {
            showFinalResult(nGuesses);
            return;
        }
    }
}

function showScore(nBulls, nCows) {
    print('    Bulls: ' + nBulls + ', cows: ' + nCows);
}

function showFinalResult(guesses) {
    print('You win!!! Guesses needed: ' + guesses);
}

function countBovine(num, guess) {
    var count = {bulls:0, cows:0};
    var g = guess.join('');
    for (var i = 0; i < num.length; i++) {
        var digPresent = g.search(num[i]) != -1;
        if (num[i] == guess[i]) count.bulls++;
        else if (digPresent) count.cows++;
    }
    return count;
}

function getGuess(nGuesses, len) {
    while (true) {
        putstr('Your guess #' + nGuesses + ': ');
        var guess = readline();
        guess = String(parseInt(guess)).split('');
        if (guess.length != len) {
            print('  You must enter a ' + len + ' digit number.');
            continue;
        }
        if (hasDups(guess)) {
            print('  No digits can be duplicated.');
            continue;
        }
        return guess;
    }
}

function hasDups(ary) {
    var t = ary.concat().sort();
    for (var i = 1; i < t.length; i++) {
        if (t[i] == t[i-1]) return true;
    }
    return false;
}

function showInstructions(len) {
    print();
    print('Bulls and Cows Game');
    print('-------------------');
    print('  You must guess the ' + len + ' digit number I am thinking of.');
    print('  The number is composed of the digits 1-9.');
    print('  No digit appears more than once.');
    print('  After each of your guesses, I will tell you:');
    print('    The number of bulls (digits in right place)');
    print('    The number of cows (correct digits, but in the wrong place)');
    print();
}

function pickNum(len) {
    var nums = [1, 2, 3, 4, 5, 6, 7, 8, 9];
    nums.sort(function(){return Math.random() - 0.5});
    return nums.slice(0, len);
}

main();

```


Example game (cheating!):

 Bulls and Cows Game
 -------------------
   You must guess the 4 digit number I am thinking of.
   The number is composed of the digits 1-9.
   No digit appears more than once.
   After each of your guesses, I will tell you:
     The number of bulls (digits in right place)
     The number of cows (correct digits, but in wrong place)

 Your guess #1: 1234
     Bulls: 0, cows: 2
 Your guess #2: 5678
     Bulls: 1, cows: 1
 Your guess #3: 3167
     Bulls: 0, cows: 3
 Your guess #4: 9123
     Bulls: 1, cows: 1
 Your guess #5: 5137
     Bulls: 1, cows: 3
 Your guess #6: 5317
     Bulls: 2, cows: 2
 Your guess #7: 5731
     Bulls: 2, cows: 2
 Your guess #8: 5713
     Bulls: 4, cows: 0
 You win! Guesses needed: 8


## Julia


```julia
function cowsbulls()
	print("Welcome to Cows & Bulls! I've picked a number with unique digits between 1 and 9, go ahead and type your guess.\n
		You get one bull for every right number in the right position.\n
		You get one cow for every right number, but in the wrong position.\n
		Enter 'n' to pick a new number and 'q' to quit.\n>")
	function new_number()
		s = [1:9]
		n = ""
		for i = 9:-1:6
			n *= string(delete!(s,rand(1:i)))
		end
		return n
	end
	answer = new_number()
	while true
		input = chomp(readline(STDIN))
		input == "q" && break
		if input == "n"
			answer = new_number()
			print("\nI've picked a new number, go ahead and guess\n>")
			continue
		end
		!ismatch(r"^[1-9]{4}$",input) && (print("Invalid guess: Please enter a 4-digit number\n>"); continue)
		if input == answer
			print("\nYou're right! Good guessing!\nEnter 'n' for a new number or 'q' to quit\n>")
		else
			bulls = sum(answer.data .== input.data)
			cows = sum([answer[x] != input[x] && contains(input.data,answer[x]) for x = 1:4])
			print("\nNot quite! Your guess is worth:\n$bulls Bulls\n$cows Cows\nPlease guess again\n\n>")
		end
	end
end
```

The following version checks thoroughly that the input of the player is constituted of four distincts digits.

```julia
function bullsandcows ()
    bulls = cows = turns = 0
    result = (s = [] ; while length(unique(s))<4 push!(s,rand('1':'9')) end; unique(s))
    println("A game of bulls and cows!")
    while (bulls != 4)
      print("Your guess? ")
      guess = collect(chomp(readline(STDIN)))
      if ! (length(unique(guess)) == length(guess) == 4 && all(isdigit,guess))
         println("please, enter four distincts digits")
         continue
      end
      bulls = sum(map(==, guess, result))
      cows = length(intersect(guess,result)) - bulls
      println("$bulls bulls and $cows cows!") ; turns += 1
    end
    println("You win! You succeeded in $turns guesses.")
  end
```

{{Out}}

```txt
julia> bullsandcows()
A game of bulls and cows!
Your guess? 1234
0 bulls and 2 cows!
Your guess? 12334
please, enter four distincts digits
Your guess? 5678
1 bulls and 1 cows!
Your guess? 1111
please, enter four distincts digits
Your guess? grr
please, enter four distincts digits
Your guess? ...
please, enter four distincts digits
Your guess?
```



## Kotlin


```scala
// version 1.1.2

import java.util.Random

const val MAX_GUESSES = 20  // say

fun main(args: Array<String>) {
    val r = Random()
    var num: String
    // generate a 4 digit random number from 1234 to 9876 with no zeros or repeated digits
    do {
        num = (1234 + r.nextInt(8643)).toString()
    } while ('0' in num || num.toSet().size < 4)

    println("All guesses should have exactly 4 distinct digits excluding zero.")
    println("Keep guessing until you guess the chosen number (maximum $MAX_GUESSES valid guesses).\n")
    var guesses = 0
    while (true) {
        print("Enter your guess : ")
        val guess = readLine()!!
        if (guess == num) {
            println("You've won with ${++guesses} valid guesses!")
            return
        }
        val n = guess.toIntOrNull()
        if (n == null)
            println("Not a valid number")
        else if ('-' in guess || '+' in guess)
            println("Can't contain a sign")
        else if ('0' in guess)
            println("Can't contain zero")
        else if (guess.length != 4)
            println("Must have exactly 4 digits")
        else if (guess.toSet().size < 4)
            println("All digits must be distinct")
        else {
            var bulls = 0
            var cows  = 0
            for ((i, c) in guess.withIndex()) {
                if (num[i] == c) bulls++
                else if (c in num) cows++
            }
            println("Your score for this guess:  Bulls = $bulls  Cows = $cows")
            guesses++
            if (guesses == MAX_GUESSES)
                println("You've now had $guesses valid guesses, the maximum allowed")
        }
    }
}
```

Sample input/output:
{{out}}

```txt

Enter your guess : 1234
Your score for this guess:  Bulls = 0  Cows = 2
Enter your guess : 1256
Your score for this guess:  Bulls = 0  Cows = 2
Enter your guess : 2178
Your score for this guess:  Bulls = 1  Cows = 0
Enter your guess : 2519
Your score for this guess:  Bulls = 3  Cows = 0
Enter your guess : 2569
Your score for this guess:  Bulls = 3  Cows = 0
Enter your guess : 2579
Your score for this guess:  Bulls = 3  Cows = 0
Enter your guess : 2589
Your score for this guess:  Bulls = 3  Cows = 0
Enter your guess : 2539
Your score for this guess:  Bulls = 3  Cows = 0
Enter your guess : 2549
You've won with 9 valid guesses!

```



## Lasso

This game uses an HTML form to submit the answer. The random number and history are stored in a session using Lasso's built in session management.

```Lasso
[
define randomizer() => {
	local(n = string)
	while(#n->size < 4) => {
		local(r = integer_random(1,9)->asString)
		#n !>> #r ? #n->append(#r)
	}
	return #n
}
define cowbullchecker(n::string,a::string) => {
	integer(#n) == integer(#a) ? return (:true,map('cows'=0,'bulls'=4,'choice'=#a))
	local(cowbull = map('cows'=0,'bulls'=0,'choice'=#a),'checked' = array)
	loop(4) => {
		if(#checked !>> integer(#a->values->get(loop_count))) => {
			#checked->insert(integer(#a->values->get(loop_count)))
			if(integer(#n->values->get(loop_count)) == integer(#a->values->get(loop_count))) => {
				#cowbull->find('bulls') += 1
			else(#n->values >> #a->values->get(loop_count))
				#cowbull->find('cows') += 1
			}
		}
	}
	#cowbull->find('bulls') == 4 ? return (:true,map('cows'=0,'bulls'=4,'choice'=#a))
	return (:true,#cowbull)
}
session_start('user')
session_addvar('user', 'num')
session_addvar('user', 'historic_choices')
// set up rand
var(num)->isNotA(::string) ? var(num = randomizer)
var(historic_choices)->isNotA(::array) ? var(historic_choices = array)
local(success = false)
// check answer
if(web_request->param('a')->asString->size) => {
	local(success,result) = cowbullchecker($num,web_request->param('a')->asString)
	$historic_choices->insert(#result)
}
if(web_request->params->asStaticArray >> 'restart') => {
	$num = randomizer
	$historic_choices = array
}
]
<h1>Bulls and Cows</h1>
<p>Guess the 4-digit number...</p>
<p>Your win if the guess is the same as the randomly chosen number.

- A score of one bull is accumulated for each digit in your guess that equals the corresponding digit in the randomly chosen initial number.

- A score of one cow is accumulated for each digit in your guess that also appears in the randomly chosen number, but in the wrong position.
</p>
[
local(win = false)
if($historic_choices->size) => {
	with c in $historic_choices do => {^
		'<p>'+#c->find('choice')+': Bulls: '+#c->find('bulls')+', Cows: '+#c->find('cows')
		if(#c->find('bulls') == 4) => {^
			' - YOU WIN!'
			#win = true
		^}
		'</p>'
	^}
}
if(not #win) => {^
]
<form action="?" method="post">
	<input name="a" value="[web_request->param('a')->asString]" size="5">
	<input type="submit" name="guess">
	<a href="?restart">Restart</a>
</form>
[else
	'<a href="?restart">Restart</a>'
^}]

```


{{out}}
Game in progress:

```txt
Bulls and Cows

Guess the 4-digit number...

Your win if the guess is the same as the randomly chosen number.
- A score of one bull is accumulated for each digit in your guess that equals the corresponding digit in the randomly chosen initial number.
- A score of one cow is accumulated for each digit in your guess that also appears in the randomly chosen number, but in the wrong position.

4567: Bulls: 0, Cows: 1
4567: Bulls: 0, Cows: 1

(input box) (submit button)   Restart
```


{{out}}
Game in to completion:

```txt
Bulls and Cows

Guess the 4-digit number...

Your win if the guess is the same as the randomly chosen number.
- A score of one bull is accumulated for each digit in your guess that equals the corresponding digit in the randomly chosen initial number.
- A score of one cow is accumulated for each digit in your guess that also appears in the randomly chosen number, but in the wrong position.

1234: Bulls: 0, Cows: 2
1256: Bulls: 0, Cows: 1
1789: Bulls: 0, Cows: 1
1222: Bulls: 0, Cows: 0
3456: Bulls: 1, Cows: 2
3564: Bulls: 0, Cows: 3
3567: Bulls: 0, Cows: 2
8564: Bulls: 0, Cows: 2
3564: Bulls: 0, Cows: 3
4365: Bulls: 0, Cows: 3
5436: Bulls: 2, Cows: 1
5478: Bulls: 2, Cows: 0
5463: Bulls: 3, Cows: 0
5468: Bulls: 2, Cows: 0
5493: Bulls: 4, Cows: 0 - YOU WIN!
Restart
```



## Liberty BASIC


```lb


    do while len( secret$) <4
        c$ =chr$( int( rnd( 1) *9) +49)
        if not( instr( secret$, c$)) then secret$ =secret$ +c$
    loop

    print " Secret number has been guessed....                                                                                 "; secret$

    guesses  =   0

[loop]
    print " Your guess ";
    input " "; guess$

    guesses  = guesses +1

    r$       =score$( guess$, secret$)

    bulls    =val( word$( r$, 1, ","))
    cows     =val( word$( r$, 2, ","))

    print "   Result: "; bulls; " bulls, and "; cows; " cows"
    print

    if guess$ =secret$ then
        print "  You won after "; guesses; " guesses!"
        print "  You guessed it in "; guesses
        print "  Thanks for playing!"
        wait
    end if

    goto [loop]

end '   _____________________________________________________________

function check( i$) '   check =0 if no digit repeats, else =1
    check =0
    for i =1 to 3
        for j =i +1 to 4
            if mid$( i$, i, 1) =mid$( i$, j, 1) then check =1
        next j
    next i
end function

function score$( a$, b$)    '   return as a csv string the number of bulls & cows.
    bulls    = 0:  cows     = 0
    for i = 1 to 4
        c$ = mid$( a$, i, 1)
        if mid$( b$, i, 1) = c$ then
            bulls = bulls + 1
        else
            if instr( b$, c$) <>0 and instr( b$, c$) <>i then
                cows = cows + 1
            end if
        end if
    next i
    score$ =str$( bulls); ","; str$( cows)
end function

[quit]
close #w
end

```



## Logo

{{works with|UCB Logo}}

```logo
to ok? :n
  output (and [number? :n] [4 = count :n] [4 = count remdup :n] [not member? 0 :n])
end

to init
  do.until [make "hidden random 10000] [ok? :hidden]
end

to guess :n
  if not ok? :n [print [Bad guess! (4 unique digits, 1-9)]  stop]
  localmake "bulls 0
  localmake "cows  0
  foreach :n [cond [
    [[? = item # :hidden] make "bulls 1 + :bulls]
    [[member?  ? :hidden] make "cows  1 + :cows ]
  ]]
  (print :bulls "bulls, :cows "cows)
  if :bulls = 4 [print [You guessed it!]]
end
```



## Lua


```Lua
function ShuffleArray(array)
   for i=1,#array-1 do
      local t = math.random(i, #array)
      array[i], array[t] = array[t], array[i]
   end
end

function GenerateNumber()
   local digits = {1,2,3,4,5,6,7,8,9}

   ShuffleArray(digits)

   return digits[1] * 1000 +
          digits[2] *  100 +
          digits[3] *   10 +
          digits[4]
end

function IsMalformed(input)
   local malformed = false

   if #input == 4 then
      local already_used = {}
      for i=1,4 do
         local digit = input:byte(i) - string.byte('0')
         if digit < 1 or digit > 9 or already_used[digit] then
            malformed = true
            break
         end
         already_used[digit] = true
      end
   else
      malformed = true
   end

   return malformed
end

math.randomseed(os.time())
math.randomseed(math.random(2^31-1)) -- since os.time() only returns seconds

print("\nWelcome to Bulls and Cows!")
print("")
print("The object of this game is to guess the random 4-digit number that the")
print("computer has chosen. The number is generated using only the digits 1-9,")
print("with no repeated digits. Each time you enter a guess, you will score one")
print("\"bull\" for each digit in your guess that matches the corresponding digit")
print("in the computer-generated number, and you will score one \"cow\" for each")
print("digit in your guess that appears in the computer-generated number, but is")
print("in the wrong position. Use this information to refine your guesses. When")
print("you guess the correct number, you win.");
print("")

quit = false

repeat
   magic_number = GenerateNumber()
   magic_string = tostring(magic_number) -- Easier to do scoring with a string
   repeat
      io.write("\nEnter your guess (or 'Q' to quit): ")
      user_input = io.read()
      if user_input == 'Q' or user_input == 'q' then
         quit = true
         break
      end

      if not IsMalformed(user_input) then
         if user_input == magic_string then
            print("YOU WIN!!!")
         else
            local bulls, cows = 0, 0
            for i=1,#user_input do
               local find_result = magic_string:find(user_input:sub(i,i))

               if find_result and find_result == i then
                  bulls = bulls + 1
               elseif find_result then
                  cows = cows + 1
               end
            end
            print(string.format("You scored %d bulls, %d cows", bulls, cows))
         end
      else
         print("Malformed input. You must enter a 4-digit number with")
         print("no repeated digits, using only the digits 1-9.")
      end

   until user_input == magic_string

   if not quit then
      io.write("\nPress <Enter> to play again or 'Q' to quit: ")
      user_input = io.read()
      if user_input == 'Q' or user_input == 'q' then
         quit = true
      end
   end

   if quit then
      print("\nGoodbye!")
   end
until quit
```



## M2000 Interpreter


```M2000 Interpreter

Module Game {
      Malformed=lambda (a$)->{
            =true
            if len(a$)<>4 then exit
            n=0 : dummy=val(a$,"int",n)
            if n<>5 or dummy=0 then exit
            for i=1 to 9
                  if len(filter$(a$,str$(i,0)))<3 then break
            next i
            =false
      }
      BullsAndCows$=lambda$ (a$, b$, &ok) ->{
            Def b, c
            for i=1 to 4
            if mid$(a$,i,1)=mid$(b$,i,1) then
                  b++
            else.if instr(b$,mid$(a$,i,1))>0 then
                  c++
            end if
            next i
            ok=b=4
            =format$("bulls {0}, cows {1}", b, c)
      }
      Random$=lambda$ ->{
            def repl$, bank$, c$
            bank$="123456789"
            for i=1 to 4
                  c$=mid$(bank$,random(1,len(bank$)),1)
                  bank$=filter$(bank$, c$)
                  repl$+=c$
            next i
            =repl$
      }
      target$=Random$()
      def boolean win=false, a$
      do
            do
                  Input "Next guess ";a%
                  a$=str$(a%,0)
                  if Malformed(a$) then Print "Malformed input, try again" else exit
            always
            Print BullsAndCows$(a$, target$, &win)
            if win then exit
            Print "Bad guess! (4 unique digits, 1-9)"
      always
      Print "You guess it"
}
Game

```





## Maple


```maple
BC := proc(n) #where n is the number of turns the user wishes to play before they quit
	local target, win, numGuesses, guess, bulls, cows, i, err;
    	target := [0, 0, 0, 0]:
    	randomize(); #This is a command that makes sure that the numbers are truly randomized each time, otherwise your first time will always give the same result.
	while member(0, target) or numelems({op(target)}) < 4 do #loop continues to generate random numbers until you get one with no repeating digits or 0s
		target := [seq(parse(i), i in convert(rand(1234..9876)(), string))]: #a list of random numbers
	end do:

	win := false:
	numGuesses := 0:
	while win = false and numGuesses < n do #loop allows the user to play until they win or until a set amount of turns have passed
		err := true;
		while err do #loop asks for values until user enters a valid number
			printf("Please enter a 4 digit integer with no repeating digits\n");
			try#catches any errors in user input
				guess := [seq(parse(i), i in readline())];
				if hastype(guess, 'Not(numeric)', 'exclude_container') then
					printf("Postive integers only! Please guess again.\n\n");
				elif numelems(guess) <> 4 then
					printf("4 digit numbers only! Please guess again.\n\n");
				elif numelems({op(guess)}) < 4 then
					printf("No repeating digits! Please guess again.\n\n");
				elif member(0, guess) then
					printf("No 0s! Please guess again.\n\n");
				else
					err := false;
				end if;
			catch:
				printf("Invalid input. Please guess again.\n\n");
			end try;
		end do:
		numGuesses := numGuesses + 1;
		printf("Guess %a: %a\n", numGuesses, guess);
		bulls := 0;
		cows := 0;
		for i to 4 do #loop checks for bulls and cows in the user's guess
			if target[i] = guess[i] then
				bulls := bulls + 1;
			elif member(target[i], guess) then
				cows := cows + 1;
			end if;
		end do;
		if bulls = 4 then
			win := true;
			printf("The number was %a.\n", target);
			printf(StringTools[FormatMessage]("You won with %1 %{1|guesses|guess|guesses}.", numGuesses));
		else
			printf(StringTools[FormatMessage]("%1 %{1|Cows|Cow|Cows}, %2 %{2|Bulls|Bull|Bulls}.\n\n", cows, bulls));
		end if;
	end do:
	if win = false and numGuesses >= n then
        printf("You lost! The number was %a.\n", target);
     end if;
	return NULL;
end proc:
```

{{out}}

```txt

Please enter a 4 digit integer with no repeating digits
Guess 1: [1, 2, 3, 4]
1 Cow, 1 Bull.

Please enter a 4 digit integer with no repeating digits
Guess 2: [2, 3, 4, 5]
0 Cows, 1 Bull.

...

```



## Mathematica


```Mathematica

digits=Last@FixedPointList[If[Length@Union@#==4,#,Table[Random[Integer,{1,9}],{4}]]&,{}]
codes=ToCharacterCode[StringJoin[ToString/@digits]];
Module[{r,bulls,cows},
	While[True,
	r=InputString[];
	If[r===$Canceled,Break[],
		With[{userCodes=ToCharacterCode@r},
		If[userCodes===codes,Print[r<>": You got it!"];Break[],
			If[Length@userCodes==Length@codes,
				bulls=Count[userCodes-codes,0];cows=Length@Intersection[codes,userCodes]-bulls;
				Print[r<>": "<>ToString[bulls]<>"bull(s), "<>ToString@cows<>"cow(s)."],
				Print["Guess four digits."]]]]]]]

```

Output:

```txt

{8, 2, 6, 1}
3432: 0 bull(s), 1 cow(s).
Illegal input.
8261: You got it!

```



## MATLAB


```MATLAB
function BullsAndCows
% Plays the game Bulls and Cows as the "game master"

    % Create a secret number
    nDigits = 4;
    lowVal = 1;
    highVal = 9;
    digitList = lowVal:highVal;
    secret = zeros(1, 4);
    for k = 1:nDigits
        idx = randi(length(digitList));
        secret(k) = digitList(idx);
        digitList(idx) = [];
    end

    % Give game information
    fprintf('Welcome to Bulls and Cows!\n')
    fprintf('Try to guess the %d-digit number (no repeated digits).\n', nDigits)
    fprintf('Digits are between %d and %d (inclusive).\n', lowVal, highVal)
    fprintf('Score: 1 Bull per correct digit in correct place.\n')
    fprintf('       1 Cow per correct digit in incorrect place.\n')
    fprintf('The number has been chosen. Now it''s your moooooove!\n')
    gs = input('Guess: ', 's');

    % Loop until user guesses right or quits (no guess)
    nGuesses = 1;
    while gs
        gn = str2double(gs);
        if isnan(gn) || length(gn) > 1  % Not a scalar
            fprintf('Malformed guess. Keep to valid scalars.\n')
            gs = input('Try again: ', 's');
        else
            g = sprintf('%d', gn) - '0';
            if length(g) ~= nDigits || any(g < lowVal) || any(g > highVal) || ...
                    length(unique(g)) ~= nDigits    % Invalid number for game
                fprintf('Malformed guess. Remember:\n')
                fprintf('  %d digits\n', nDigits)
                fprintf('  Between %d and %d inclusive\n', lowVal, highVal)
                fprintf('  No repeated digits\n')
                gs = input('Try again: ', 's');
            else
                score = CountBullsCows(g, secret);
                if score(1) == nDigits
                    fprintf('You win! Bully for you! Only %d guesses.\n', nGuesses)
                    gs = '';
                else
                    fprintf('Score: %d Bulls, %d Cows\n', score)
                    gs = input('Guess: ', 's');
                end
            end
        end
        nGuesses = nGuesses+1;  % Counts malformed guesses
    end
end

function score = CountBullsCows(guess, correct)
% Checks the guessed array of digits against the correct array to find the score
% Assumes arrays of same length and valid numbers
    bulls = guess == correct;
    cows = ismember(guess(~bulls), correct);
    score = [sum(bulls) sum(cows)];
end
```

{{out}}

```txt
Welcome to Bulls and Cows!
Try to guess the 4-digit number (no repeated digits).
Digits are between 1 and 9 (inclusive).
Score: 1 Bull per correct digit in correct place.
       1 Cow per correct digit in incorrect place.
The number has been chosen. Now it's your moooooove!
Guess: 1234
Score: 0 Bulls, 2 Cows
Guess: 2156
Score: 0 Bulls, 1 Cows
Guess: 7819
Score: 0 Bulls, 1 Cows
Guess: 3457
Score: 0 Bulls, 2 Cows
Guess: hello
Malformed guess. Keep to valid scalars.
Try again: 1123
Malformed guess. Remember:
  4 digits
  Between 1 and 9 inclusive
  No repeated digits
Try again: 34567
Malformed guess. Remember:
  4 digits
  Between 1 and 9 inclusive
  No repeated digits
Try again: 4368
You win! Bully for you! Only 8 guesses.
```


## MAXScript


```MAXScript

numCount = 4 -- number of digits to use

digits = #(1, 2, 3, 4, 5, 6, 7, 8, 9)
num = ""
while num.count < numCount and digits.count > 0 do
(
	local r = random 1 digits.count
	append num (digits[r] as string)
	deleteitem digits r
)
digits = undefined
numGuesses = 0
inf = "Rules: \n1. Choose only % unique digits in any combination"+\
		" (0 can't be used).\n2. Only positive integers are allowed."+\
		"\n3. For each digit that is in it's place, you get a bull,\n"+\
		"\tand for each digit that is not in it's place, you get a cow."+\
		"\n4. The game is won when your number matches the number I chose."+\
		"\n\nPress [esc] to quit the game.\n\n"
clearlistener()
format inf num.count
while not keyboard.escpressed do
(
	local userVal = getkbvalue prompt:"\nEnter your number: "
	if (userVal as string) == num do
	(
		format "\nCorrect! The number is %. It took you % moves.\n" num numGuesses
		exit with OK
	)
	local bulls = 0
	local cows = 0
	local badInput = false
	case of
	(
		(classof userVal != integer):
			(
			format "\nThe number must be a positive integer.\n"
			badInput = true
			)
		((userVal as string).count != num.count):
			(
			format "\nThe number must have % digits.\n" num.count
			badInput = true
			)
		((makeuniquearray (for i in 1 to (userVal as string).count \
			collect (userVal as string)[i])).count != (userVal as string).count):
			(
				format "\nThe number can only have unique digits.\n"
				badInput = true
			)
	)
	if not badInput do
	(
		userVal = userVal as string
		i = 1
		while i <= userVal.count do
		(
			for j = 1 to num.count do
			(
				if userVal[i] == num[j] do
				(
					if i == j then bulls += 1 else cows += 1
				)
			)
			i += 1
		)
		numGuesses += 1
		format "\nBulls: % Cows: %\n" bulls cows
	)
)
```

{{out}}
<lang>
OK
Rules:
1. Choose only 4 unique digits in any combination (0 can't be used).
2. Only positive integers are allowed.
3. For each digit that is in it's place, you get a bull,
	and for each digit that is not in it's place, you get a cow.
4. The game is won when your number matches the number I chose.

Press [esc] to quit the game.

OK

Enter your number:  2468
Bulls: 0 Cows: 2

Enter your number:  2448
The number can only have unique digits.

Enter your number:  1357
Bulls: 0 Cows: 1

```



## MiniScript


```MiniScript
secret = range(1,9)
secret.shuffle
secret = secret[:4].join("")

while true
    guess = input("Your guess? ").split("")
    if guess.len != 4 then
        print "Please enter 4 numbers, with no spaces."
        continue
    end if
    bulls = 0
    cows = 0
    for i in guess.indexes
        if secret[i] == guess[i] then
            bulls = bulls + 1
        else if secret.indexOf(guess[i]) != null then
            cows = cows + 1
        end if
    end for
    if bulls == 4 then
        print "You got it!  Great job!"
        break
    end if
    print "You score " + bulls + " bull" + "s"*(bulls!=1) +
    " and " + cows + " cow" + "s"*(cows!=1) + "."
end while
```

{{out}}

```txt
Your guess? 2385
You score 0 bulls and 2 cows.
Your guess? 2323
You score 0 bulls and 2 cows.
Your guess? 2211
You score 2 bulls and 1 cow.
...
Your guess? 8542
You score 3 bulls and 0 cows.
Your guess? 8642
You got it!  Great job!
```



## MUMPS


```MUMPS
BullCow	New bull,cow,guess,guessed,ii,number,pos,x
	Set number="",x=1234567890
	For ii=1:1:4 Do
	. Set pos=$Random($Length(x))+1
	. Set number=number_$Extract(x,pos)
	. Set $Extract(x,pos)=""
	. Quit
	Write !,"The computer has selected a number that consists"
	Write !,"of four different digits."
	Write !!,"As you are guessing the number, ""bulls"" and ""cows"""
	Write !,"will be awarded: a ""bull"" for each digit that is"
	Write !,"placed in the correct position, and a ""cow"" for each"
	Write !,"digit that occurs in the number, but in a different place.",!
	Write !,"For a guess, enter 4 digits."
	Write !,"Any other input is interpreted as ""I give up"".",!
	Set guessed=0 For  Do  Quit:guessed
	. Write !,"Your guess: " Read guess If guess'?4n Set guessed=-1 Quit
	. Set (bull,cow)=0,x=guess
	. For ii=4:-1:1 If $Extract(x,ii)=$Extract(number,ii) Do
	. . Set bull=bull+1,$Extract(x,ii)=""
	. . Quit
	. For ii=1:1:$Length(x) Set:number[$Extract(x,ii) cow=cow+1
	. Write !,"You guessed ",guess,". That earns you "
	. If 'bull,'cow Write "neither bulls nor cows..." Quit
	. If bull Write bull," bull" Write:bull>1 "s"
	. If cow Write:bull " and " Write cow," cow" Write:cow>1 "s"
	. Write "."
	. If bull=4 Set guessed=1 Write !,"That's a perfect score."
	. Quit
	If guessed<0 Write !!,"The number was ",number,".",!
	Quit
Do BullCow

The computer has selected a number that consists
of four different digits.

As you are guessing the number, "bulls" and "cows"
will be awarded: a "bull" for each digit that is
placed in the correct position, and a "cow" for each
digit that occurs in the number, but in a different place.

For a guess, enter 4 digits.
Any other input is interpreted as "I give up".

Your guess: 1234
You guessed 1234. That earns you 1 cow.
Your guess: 5678
You guessed 5678. That earns you 1 cow.
Your guess: 9815
You guessed 9815. That earns you 1 cow.
Your guess: 9824
You guessed 9824. That earns you 2 cows.
Your guess: 9037
You guessed 9037. That earns you 1 bull and 2 cows.
Your guess: 9048
You guessed 2789. That earns you 1 bull and 2 cows.
Your guess: 2079
You guessed 2079. That earns you 1 bull and 3 cows.
Your guess: 2709
You guessed 2709. That earns you 2 bulls and 2 cows.
Your guess: 0729
You guessed 0729. That earns you 4 cows.
Your guess: 2907
You guessed 2907. That earns you 4 bulls.
That's a perfect score.
```



## Nim

{{trans|Python}}

```nim
import random, strutils, rdstdin
randomize()

proc random(a: string): char = a[random(0..a.len)]

const
  digits = "123456789"
  size = 4

var digitsSet: set[char] = {}
for d in digits: digitsSet.incl d

var chosen = newString(size)
for i in 0..chosen.high: chosen[i] = random(digits)

echo """I have chosen a number from $# unique digits from 1 to 9 arranged in a random order.
You need to input a $# digit, unique digit number as a guess at what I have chosen""".format(size, size)

var guesses = 0
while true:
  inc guesses
  var guess = ""
  while true:
    guess = readLineFromStdin("\nNext guess [$#]: ".format(guesses)).strip()
    if guess.len == size and allCharsInSet(guess, digitsSet):
      break
    echo "Problem, try again. You need to enter $# unique digits from 1 to 9".format(size)
  if guess == chosen:
    echo "\nCongratulations you guessed correctly in ",guesses," attempts"
    break
  var bulls, cows = 0
  for i in 0 .. <size:
    if guess[i] == chosen[i]: inc bulls
    if guess[i] in chosen: inc cows
  echo "  $# Bulls\n  $# Cows".format(bulls, cows)
```

Sample output:

```txt
I have chosen a number from 4 unique digits from 1 to 9 arranged in a random order.
You need to input a 4 digit, unique digit number as a guess at what I have chosen
1195

Next guess [1]: 79
Problem, try again. You need to enter 4 unique digits from 1 to 9

Next guess [1]: 7983
  0 Bulls
  1 Cows

Next guess [2]: 1293
  2 Bulls
  2 Cows

Next guess [3]: 1193
  3 Bulls
  3 Cows

Next guess [4]: 1195

Congratulations you guessed correctly in 4 attempts
```



## OCaml


```ocaml
let rec input () =
  let s = read_line () in
  try
    if String.length s <> 4 then raise Exit;
    String.iter (function
      | '1'..'9' -> ()
      | _ -> raise Exit
    ) s;
    let t = [ s.[0]; s.[1]; s.[2]; s.[3] ] in
    let _ = List.fold_left  (* reject entry with duplication *)
              (fun ac b -> if List.mem b ac then raise Exit; (b::ac))
              [] t in
    List.map (fun c -> int_of_string (String.make 1 c)) t
  with Exit ->
    prerr_endline "That is an invalid entry. Please try again.";
    input ()
;;

let print_score g t =
  let bull = ref 0 in
  List.iter2 (fun x y ->
    if x = y then incr bull
  ) g t;
  let cow = ref 0 in
  List.iter (fun x ->
    if List.mem x t then incr cow
  ) g;
  cow := !cow - !bull;
  Printf.printf "%d bulls, %d cows\n%!" !bull !cow
;;

let () =
  Random.self_init ();
  let rec mkgoal acc = function 4 -> acc
  | i ->
      let n = succ(Random.int 9) in
      if List.mem n acc
      then mkgoal acc i
      else mkgoal (n::acc) (succ i)
  in
  let g = mkgoal [] 0 in
  let found = ref false in
  while not !found do
    let t = input () in
    if t = g
    then found := true
    else print_score g t
  done;
  print_endline "Congratulations you guessed correctly";
;;
```




## Oforth



```Oforth
: bullsAndCows
| numbers guess digits bulls cows |

   ListBuffer new ->numbers
   while(numbers size 4 <>) [ 9 rand dup numbers include ifFalse: [ numbers add ] else: [ drop ] ]

   while(true) [
      "Enter a number of 4 different digits between 1 and 9 : " print
      System.Console askln ->digits
      digits asInteger isNull digits size 4 <> or ifTrue: [ "Number of four digits needed" println continue ]
      digits map(#asDigit) ->guess

      guess numbers zipWith(#==) occurrences(true) ->bulls
      bulls 4 == ifTrue: [ "You won !" println return ]

      guess filter(#[numbers include]) size bulls - ->cows
      System.Out "Bulls = " << bulls << ", cows = " << cows << cr
      ] ;
```



## ooRexx

The solution at [[#Version_2|Rexx Version 2]] is a valid ooRexx program.


## Oz


```oz
declare
  proc {Main}
     Solution = {PickNUnique 4 {List.number 1 9 1}}

     proc {Loop}
        Guess = {EnterGuess}
     in
        {System.showInfo
         {Bulls Guess Solution}#" bulls and "#
         {Cows Guess Solution}#" cows"}
        if Guess \= Solution then {Loop} end
     end
  in
     {Loop}
     {System.showInfo "You have won!"}
  end

  fun {Bulls Xs Sol}
     {Length {Filter {List.zip Xs Sol Value.'=='} Id}}
  end

  fun {Cows Xs Sol}
     {Length {Intersection Xs Sol}}
  end

  local
     class TextFile from Open.file Open.text end
     StdIn = {New TextFile init(name:stdin)}
  in
     fun {EnterGuess}
        try
           {System.printInfo "Enter your guess (e.g. \"1234\"): "}
           S = {StdIn getS($)}
        in
           %% verify
           {Length S} = 4
           {All S Char.isDigit} = true
           {FD.distinct S} %% assert there is no duplicate digit
           %% convert from digits to numbers
           {Map S fun {$ D} D-&0 end}
        catch _ then
           {EnterGuess}
        end
     end
  end

  fun {PickNUnique N Xs}
     {FoldL {MakeList N}
      fun {$ Z _}
         {Pick {Diff Xs Z}}|Z
      end
      nil}
  end

  fun {Pick Xs}
     {Nth Xs {OS.rand} mod {Length Xs} + 1}
  end

  fun {Diff Xs Ys}
     {FoldL Ys List.subtract Xs}
  end

  fun {Intersection Xs Ys}
     {Filter Xs fun {$ X} {Member X Ys} end}
  end

  fun {Id X} X end
in
  {Main}
```



## PARI/GP

This simple implementation expects guesses in the form [a,b,c,d].

```parigp
bc()={
  my(u,v,bulls,cows);
  while(#vecsort(v=vector(4,i,random(9)+1),,8)<4,);
  while(bulls<4,
    u=input();
    if(type(u)!="t_VEC"|#u!=4,next);
    bulls=sum(i=1,4,u[i]==v[i]);
    cows=sum(i=1,4,sum(j=1,4,i!=j&v[i]==u[j]));
    print("You have "bulls" bulls and "cows" cows")
  )
};
```



## Pascal


```pascal
Program BullCow;

{$mode objFPC}

uses Math, SysUtils;

type
   TFourDigit = array[1..4] of integer;

Procedure WriteFourDigit(fd: TFourDigit);
{ Write out a TFourDigit with no line break following. }
var
   i: integer;
begin
   for i := 1 to 4 do
   begin
      Write(fd[i]);
   end;
end;

Function WellFormed(Tentative: TFourDigit): Boolean;
{ Does the TFourDigit avoid repeating digits? }
var
   current, check: integer;
begin

   Result := True;

   for current := 1 to 4 do
   begin
      for check := current + 1 to 4 do
      begin
         if Tentative[check] = Tentative[current] then
         begin
            Result := False;
         end;
      end;
   end;

end;

Function MakeNumber(): TFourDigit;
{ Make a random TFourDigit, keeping trying until it is well-formed. }
var
   i: integer;
begin
   for i := 1 to 4 do
   begin
      Result[i] := RandomRange(1, 9);
   end;
   if not WellFormed(Result) then
   begin
      Result := MakeNumber();
   end;
end;

Function StrToFourDigit(s: string): TFourDigit;
{ Convert an (input) string to a TFourDigit. }
var
   i: integer;
begin
   for i := 1 to Length(s) do
   begin
      StrToFourDigit[i] := StrToInt(s[i]);
   end;
end;

Function Wins(Num, Guess: TFourDigit): Boolean;
{ Does the guess win? }
var
   i: integer;
begin
   Result := True;
   for i := 1 to 4 do
   begin
      if Num[i] <> Guess[i] then
      begin
         Result := False;
         Exit;
      end;
   end;
end;

Function GuessScore(Num, Guess: TFourDigit): string;
{ Represent the score of the current guess as a string. }
var
   i, j, bulls, cows: integer;
begin

   bulls := 0;
   cows := 0;

   { Count the cows and bulls. }
   for i := 1 to 4 do
   begin
      for j := 1 to 4 do
      begin
         if  (Num[i] = Guess[j]) then
         begin
            { If the indices are the same, that would be a bull. }
            if (i = j) then
            begin
               bulls := bulls + 1;
            end
            else
            begin
               cows := cows + 1;
            end;
         end;
      end;
   end;

   { Format the result as a sentence. }
   Result := IntToStr(bulls) + ' bulls, ' + IntToStr(cows) + ' cows.';

end;

Function GetGuess(): TFourDigit;
{ Get a well-formed user-supplied TFourDigit guess. }
var
   input: string;
begin

   WriteLn('Enter a guess:');
   ReadLn(input);

   { Must be 4 digits. }
   if Length(input) = 4 then
   begin

      Result := StrToFourDigit(input);

      if not WellFormed(Result) then
      begin
         WriteLn('Four unique digits, please.');
         Result := GetGuess();
      end;

   end
   else
   begin
      WriteLn('Please guess a four-digit number.');
      Result := GetGuess();
   end;

end;

var
   Num, Guess: TFourDigit;
   Turns: integer;
begin

   { Initialize the randymnity. }
   Randomize();

   { Make the secred number. }
   Num := MakeNumber();

   WriteLn('I have a secret number. Guess it!');

   Turns := 0;

   { Guess until the user gets it. }
   While True do
   begin

      Guess := GetGuess();

      { Count each guess as a turn. }
      Turns := Turns + 1;

      { If the user won, tell them and ditch. }
      if Wins(Num, Guess) then
      begin
         WriteLn('You won in ' + IntToStr(Turns) + ' tries.');
         Write('The number was ');
         WriteFourDigit(Num);
         WriteLn('!');
         Exit;
      end
      else { Otherwise, score it and get a new guess. }
      begin
         WriteLn(GuessScore(Num, Guess));
      end;

   end;

end.

```



## Perl


```perl
use Data::Random qw(rand_set);
use List::MoreUtils qw(uniq);

my $size = 4;
my $chosen = join "", rand_set set => ["1".."9"], size => $size;

print "I've chosen a number from $size unique digits from 1 to 9; you need
to input $size unique digits to guess my number\n";

for ( my $guesses = 1; ; $guesses++ ) {
    my $guess;
    while (1) {
        print "\nNext guess [$guesses]: ";
        $guess = <STDIN>;
        chomp $guess;
        checkguess($guess) and last;
        print "$size digits, no repetition, no 0... retry\n";
    }
    if ( $guess eq $chosen ) {
        print "You did it in $guesses attempts!\n";
        last;
    }
    my $bulls = 0;
    my $cows = 0;
    for my $i (0 .. $size-1) {
        if ( substr($guess, $i, 1) eq substr($chosen, $i, 1) ) {
            $bulls++;
        } elsif ( index($chosen, substr($guess, $i, 1)) >= 0 ) {
            $cows++;
        }
    }
    print "$cows cows, $bulls bulls\n";
}

sub checkguess
{
    my $g = shift;
    return uniq(split //, $g) == $size && $g =~ /^[1-9]{$size}$/;
}
```



## Perl 6

{{trans|Python}}

{{works with|Rakudo|2015.12}}


```perl6
my $size = 4;
my @secret = pick $size, '1' .. '9';

for 1..* -> $guesses {
    my @guess;
    loop {
        @guess = (prompt("Guess $guesses: ") // exit).comb;
        last if @guess == $size and
            all(@guess) eq one(@guess) & any('1' .. '9');
        say 'Malformed guess; try again.';
    }
    my ($bulls, $cows) = 0, 0;
    for ^$size {
        when @guess[$_] eq @secret[$_] { ++$bulls; }
        when @guess[$_] eq any @secret { ++$cows; }
    }
    last if $bulls == $size;
    say "$bulls bulls, $cows cows.";
}

say 'A winner is you!';
```



## Phix


```Phix
constant N = 4

function mask(integer ch)
    return power(2,ch-'1')
end function

function score(sequence guess, sequence goal)
integer bits = 0, bulls = 0, cows = 0, b
    for i=1 to N do
        b = goal[i]
        if guess[i]=b then
            bulls += 1
        else
            bits += mask(b)
        end if
    end for
    for i=1 to N do
        b = mask(guess[i])
        if and_bits(bits,b)!=0 then
            cows += 1
            bits -= b
        end if
    end for
    return {bulls, cows}
end function

procedure game()
sequence tgt = shuffle("123456789")[1..N]
integer attempt = 1, bulls = 0, cows
sequence guess
    while bulls<N do
        while 1 do
            printf(1,"Enter a %d digit number using only the digits 1 to 9:",N)
            guess = trim(gets(0))
            if length(guess)=N then exit end if
            if length(guess)=1 and lower(guess)="q" then
                puts(1,"\nthe secret number was:"&tgt)
                {} = wait_key()
                abort(0)
            end if
            printf(1," - length is not %d  (enter q to give up)\n",N)
        end while
        {bulls, cows} = score(guess,tgt)
        printf(1,"   Guess %-2d  (%s)  bulls:%d  cows:%d\n",{attempt,guess,bulls,cows})
        attempt += 1
    end while
    puts(1,"Well done!\n")
end procedure

game()
```

{{out}}

```txt

Enter a 4 digit number using only the digits 1 to 9:5739   Guess 1   (5739)  bulls:0  cows:3
Enter a 4 digit number using only the digits 1 to 9:7193   Guess 2   (7193)  bulls:2  cows:1
Enter a 4 digit number using only the digits 1 to 9:3495   Guess 3   (3495)  bulls:0  cows:2
Enter a 4 digit number using only the digits 1 to 9:7983   Guess 4   (7983)  bulls:3  cows:0
Enter a 4 digit number using only the digits 1 to 9:7963   Guess 5   (7963)  bulls:3  cows:0
Enter a 4 digit number using only the digits 1 to 9:7923   Guess 6   (7923)  bulls:4  cows:0
Well done!

```



## PHP


```php
<?php
$size = 4;

$chosen = implode(array_rand(array_flip(range(1,9)), $size));

echo "I've chosen a number from $size unique digits from 1 to 9; you need
to input $size unique digits to guess my number\n";

for ($guesses = 1; ; $guesses++) {
    while (true) {
        echo "\nNext guess [$guesses]: ";
        $guess = rtrim(fgets(STDIN));
        if (!checkguess($guess))
            echo "$size digits, no repetition, no 0... retry\n";
        else
            break;
    }
    if ($guess == $chosen) {
        echo "You did it in $guesses attempts!\n";
        break;
    } else {
        $bulls = 0;
        $cows = 0;
        foreach (range(0, $size-1) as $i) {
            if ($guess[$i] == $chosen[$i])
                $bulls++;
            else if (strpos($chosen, $guess[$i]) !== FALSE)
                $cows++;
        }
        echo "$cows cows, $bulls bulls\n";
    }
}

function checkguess($g)
{
  global $size;
  return count(array_unique(str_split($g))) == $size &&
    preg_match("/^[1-9]{{$size}}$/", $g);
}
?>
```



## PicoLisp


```lisp
(de ok? (N)
   (let D (mapcar 'format (chop N))
      (and (num? N)
           (not (member 0 D))
           (= 4 (length D))
           (= D (uniq D))
           D )) )

(de init-cows ()
   (until (setq *Hidden (ok? (rand 1234 9876)))) )

(de guess (N)
   (let D (ok? N)
      (if D
         (let Bulls (cnt '= D *Hidden)
            (if (= 4 Bulls)
               " You guessed it!"
               (let Cows (- (cnt '((N) (member N *Hidden)) D) Bulls)
                  (pack Bulls " bulls, " Cows " cows") ) ) )
         " Bad guess! (4 unique digits, 1-9)" ) ) )

```



## PowerShell


```PowerShell

[int]$guesses = $bulls = $cows = 0
[string]$guess = "none"
[string]$digits = ""

while ($digits.Length -lt 4)
{
    $character = [char](49..57 | Get-Random)

    if ($digits.IndexOf($character) -eq -1) {$digits += $character}
}

Write-Host "`nGuess four digits (1-9) using no digit twice.`n" -ForegroundColor Cyan

while ($bulls -lt 4)
{
    do
    {
        $prompt = "Guesses={0:0#}, Last='{1,4}', Bulls={2}, Cows={3}; Enter your guess" -f $guesses, $guess, $bulls, $cows
        $guess = Read-Host $prompt

        if ($guess.Length -ne 4)                     {Write-Host "`nMust be a four-digit number`n" -ForegroundColor Red}
        if ($guess -notmatch "[1-9][1-9][1-9][1-9]") {Write-Host "`nMust be numbers 1-9`n"         -ForegroundColor Red}
    }
    until ($guess.Length -eq 4)

    $guesses += 1
    $bulls = $cows = 0

    for ($i = 0; $i -lt 4; $i++)
    {
        $character = $digits.Substring($i,1)

        if ($guess.Substring($i,1) -eq $character)
        {
            $bulls += 1
        }
        else
        {
            if ($guess.IndexOf($character) -ge 0)
            {
                $cows += 1
            }
        }
    }
}

Write-Host "`nYou won after $($guesses - 1) guesses." -ForegroundColor Cyan

```

{{Out}}

```txt

Guess four digits (1-9) using no digit twice.

Guesses=00, Last='none', Bulls=0, Cows=0; Enter your guess: 1234
Guesses=01, Last='1234', Bulls=0, Cows=3; Enter your guess: 2345
Guesses=02, Last='2345', Bulls=1, Cows=2; Enter your guess: 2346
Guesses=03, Last='2346', Bulls=1, Cows=1; Enter your guess: 2341
Guesses=04, Last='2341', Bulls=1, Cows=2; Enter your guess: 3241
Guesses=05, Last='3241', Bulls=0, Cows=3; Enter your guess: 4321
Guesses=06, Last='4321', Bulls=1, Cows=2; Enter your guess: 5321
Guesses=07, Last='5321', Bulls=2, Cows=2; Enter your guess: 5312

You won after 7 guesses.

```




## Processing

Produces both a console transcript and a GUI interface to the game.
Creates a new game each time the guess is correct; tracks number of games won.

```processing
IntDict score;
StringList choices;
StringList guess;
StringList secret;
int gamesWon = -1;

void setup() {
  choices = new StringList("0", "1", "2", "3", "4", "5", "6", "7", "8", "9");
  newGame();
}

void newGame() {
  gamesWon++;
  choices.shuffle();
  secret = new StringList();
  for (int i=0; i<4; i++) { // selections
    secret.append(choices.get(i));
  }
  newGuess();
  println("\nsecret:", secret, "\n");
}

void newGuess() {
  guess = new StringList();
  score = null;
}

void draw() {
  background(0);
  text("Bulls & Cows " + gamesWon, 5, 20);
  for (int i=0; i<guess.size(); i++) {
    text(guess.get(i), 20*i+10, height/2);
  }
  if (score!=null) {
    text("bulls:" + score.get("bulls") + " cows:" + score.get("cows"), 10, height-20);
  }
}

void keyReleased() {
  if (score!=null && score.get("bulls")==4) newGame();
  if (guess.size()==secret.size()) newGuess();
  if (guess.hasValue(str(key))) newGuess();
  if (key>=48 && key<=57) guess.append(str(key));
  if (guess.size()==secret.size()) {
    score = checkScore(secret, guess);
    println("guess: ", guess, "\n", score, "wins:", gamesWon);
  }
}

IntDict checkScore(StringList secret, StringList guess) {
  IntDict result = new IntDict();
  result.set("bulls", 0);
  result.set("cows", 0);
  for (int i=0; i<guess.size(); i++) {
    if (guess.get(i).equals(secret.get(i))) {
      result.add("bulls", 1);
    } else if (secret.hasValue(guess.get(i))) {
      result.add("cows", 1);
    }
  }
  return result;
}
```




## Prolog

Works with SWI-Prolog 6.1.8 (for predicate '''foldl'''), module lambda, written by '''Ulrich Neumerkel''' found there http://www.complang.tuwien.ac.at/ulrich/Prolog-inedit/lambda.pl and module clpfd written by '''Markus Triska'''.

```Prolog
:- use_module(library(lambda)).
:- use_module(library(clpfd)).

% Parameters of the server

% length of the guess
proposition(4).

% Numbers of digits
% 0 -> 8
digits(8).


bulls_and_cows_server :-
	proposition(LenGuess),
	length(Solution, LenGuess),
	choose(Solution),
	repeat,
	write('Your guess : '),
	read(Guess),
	(   study(Solution, Guess, Bulls, Cows)
	->  format('Bulls : ~w Cows : ~w~n', [Bulls, Cows]),
	    Bulls = LenGuess
	;   digits(Digits), Max is Digits + 1,
	    format('Guess must be of ~w digits between 1 and ~w~n',
		   [LenGuess, Max]),
	    fail).

choose(Solution) :-
	digits(Digits),
	Max is Digits + 1,
	repeat,
	maplist(\X^(X is random(Max) + 1), Solution),
	all_distinct(Solution),
	!.

study(Solution, Guess, Bulls, Cows) :-
	proposition(LenGuess),
	digits(Digits),

	% compute the transformation 1234 => [1,2,3,4]
	atom_chars(Guess, Chars),
	maplist(\X^Y^(atom_number(X, Y)), Chars, Ms),

	% check that the guess is well formed
	length(Ms, LenGuess),
	maplist(\X^(X > 0, X =< Digits+1), Ms),

	% compute the digit in good place
	foldl(\X^Y^V0^V1^((X = Y->V1 is V0+1; V1 = V0)),Solution, Ms, 0, Bulls),

	% compute the digits in bad place
	foldl(\Y1^V2^V3^(foldl(\X2^Z2^Z3^(X2 = Y1 -> Z3 is Z2+1; Z3 = Z2), Ms, 0, TT1),
			 V3 is V2+ TT1),
	      Solution, 0, TT),
	Cows is TT - Bulls.

```


## PureBasic


```PureBasic
Define.s secret, guess, c
Define.i bulls, cows, guesses, i

If OpenConsole()

  While Len(secret) < 4
    c = Chr(Random(8) + 49)
    If FindString(secret, c, 1) = 0
      secret + c
    EndIf
  Wend

  Repeat
    Print("Guess a 4-digit number with no duplicate digits: ")
    guess = Input()
    If Len(guess) = 0
      Break   ;break from loop
    EndIf

    isMalformedGuess = #False
    If Len(guess) <> 4
      ;guess is too short
      isMalformedGuess = #True
    Else
      For i = 1 To 4
        c = Mid(guess, i, 1)
        If Not FindString("123456789", c, 1) Or CountString(guess, c) <> 1
          ;guess contains either non-digits or duplicate digits
          isMalformedGuess = #True
          Break ;break from For/Next loop
        EndIf
      Next
    EndIf

    If isMalformedGuess
      PrintN("** You should enter 4 different numeric digits that are each from 1 to 9!")
      Continue ;continue loop
    EndIf

    bulls = 0: cows = 0: guesses = guesses + 1
    For i = 1 To 4
      c = Mid(secret, i, 1)
      If Mid(guess, i, 1) = c
        bulls + 1
      ElseIf FindString(guess, c, 1)
        cows + 1
      EndIf
    Next

    Print( Str(bulls) + " bull")
    If bulls <> 1
      Print( "s")
    EndIf
    Print( ", " + Str(cows) + " cow")
    If cows <> 1
      PrintN( "s")
    Else
      PrintN("")
    EndIf

    If guess = secret
      PrintN("You won after " + Str(guesses) + " guesses!")
      Break    ;break from loop
    EndIf
  ForEver

  Print(#CRLF$ + #CRLF$ + "Press ENTER to exit")
  Input()
  CloseConsole()
EndIf
```



## Python


```python
'''
 Bulls and cows. A game pre-dating, and similar to, Mastermind.
'''

import random

digits = '123456789'
size = 4
chosen = ''.join(random.sample(digits,size))
#print chosen # Debug
print '''I have chosen a number from %s unique digits from 1 to 9 arranged in a random order.
You need to input a %i digit, unique digit number as a guess at what I have chosen''' % (size, size)
guesses = 0
while True:
    guesses += 1
    while True:
        # get a good guess
        guess = raw_input('\nNext guess [%i]: ' % guesses).strip()
        if len(guess) == size and \
           all(char in digits for char in guess) \
           and len(set(guess)) == size:
            break
        print "Problem, try again. You need to enter %i unique digits from 1 to 9" % size
    if guess == chosen:
        print '\nCongratulations you guessed correctly in',guesses,'attempts'
        break
    bulls = cows = 0
    for i in range(size):
        if guess[i] == chosen[i]:
            bulls += 1
        elif guess[i] in chosen:
            cows += 1
    print '  %i Bulls\n  %i Cows' % (bulls, cows)
```

Sample output:

```txt
I have chosen a number from 4 unique digits from 1 to 9 arranged in a random order.
You need to input a 4 digit, unique digit number as a guess at what I have chosen

Next guess [1]: 79
Problem, try again. You need to enter 4 unique digits from 1 to 9

Next guess [1]: 7983
  2 Bulls
  2 Cows

Next guess [2]: 7938

Congratulations you guessed correctly in 2 attempts
```



## R

{{works with|R|2.8.1}}

```R
target <- sample(1:9,4)
bulls <- 0
cows <- 0
attempts <- 0
while (bulls != 4)
  {
  input <- readline("Guess a 4-digit number with no duplicate digits or 0s: ")
  if (nchar(input) == 4)
    {
    input <- as.integer(strsplit(input,"")[[1]])
    if ((sum(is.na(input)+sum(input==0))>=1) | (length(table(input)) != 4)) {print("Malformed input!")} else {
      bulls <- sum(input == target)
      cows <- sum(input %in% target)-bulls
      cat("\n",bulls," Bull(s) and ",cows, " Cow(s)\n")
      attempts <- attempts + 1
      }
    } else {print("Malformed input!")}
  }
print(paste("You won in",attempts,"attempt(s)!"))
```




## Racket



```racket

#lang racket

; secret : (listof exact-nonnegative-integer?)
(define secret
  (foldr (λ (n result)
           (cons n (map (λ (y) (if (>= y n) (add1 y) y))
                        result)))
         '()
         (map random '(10 9 8 7))))

; (count-bulls/cows guess) -> (values exact-nonnegative-integer?
;                                     exact-nonnegative-integer?)
; guess : (listof exact-nonnegative-integer?)
(define (count-bulls/cows guess)
  (let* ([bulls (map = guess secret)]
         [cow-candidates (filter-map (λ (x y) (if (false? x) y #f))
                                      bulls
                                      secret)]
         [cows (filter (curryr member cow-candidates) guess)])
    (values (length (filter ((curry equal?) #t) bulls))
            (length cows))))

; (valid-guess guess-str) -> (or/c (listof exact-nonnegative-integer?) #f)
; guess-str : string?
(define (valid-guess guess-str)
  (define (char->digit c)
    (- (char->integer c) (char->integer #\0)))
  (if (regexp-match-exact? #px"[0-9]{4}" guess-str)
      (let ([guess (map char->digit (string->list guess-str))])
        (if (andmap (λ (x) (equal? (count ((curry equal?) x) guess) 1))
                    guess)
            guess
            #f))
      #f))

; Game states
(define win #t)
(define game #f)

; (main-loop state step) -> void?
; state : boolean?
; step  : exact-nonnegative-integer?
(define (main-loop state step)
  (if (equal? state win)
      (printf "You won after ~a guesses." step)
      (begin
        (let* ([guess-str (read-line)]
               [guess (valid-guess guess-str)])
          (if (false? guess)
              (begin (displayln "Guess should include exactly four different digits")
                     (main-loop state step))
              (let-values ([(bulls cows) (count-bulls/cows guess)])
                (if (= bulls 4)
                    (main-loop win (add1 step))
                    (begin (printf "Bulls: ~a Cows: ~a\n" bulls cows)
                           (main-loop state (add1 step))))))))))

(main-loop game 0)
```


Output:

```txt

1234
Bulls: 0 Cows: 1
21345
Guess should include exactly four different digits
2134
Bulls: 1 Cows: 0
2314
Bulls: 0 Cows: 1
5167
Bulls: 1 Cows: 1
5189
Bulls: 1 Cows: 2
0189
Bulls: 1 Cows: 2
8179
Bulls: 1 Cows: 3
7198
You won after 8 guesses.

```



## Red


```Red

Red[]
a: "0123456789"
bulls: 0
random/seed now/time
number: copy/part random a 4
while [bulls <> 4] [
	bulls: 0
	cows: 0
	guess: ask "make a guess: "
	repeat i 4 [
		if (pick guess i) = (pick number i) [bulls: bulls + 1]
	]
	cows: (length? intersect guess number) - bulls
	print ["bulls: " bulls " cows: " cows]
]
print "You won!"

```



## REXX

This game is also known as:
:::::::* Pigs and Bulls
:::::::* Bulls and Cleots
:::::::* Mastermind
:::::::* Master Mind

### version 1

This REXX version of Bulls and Cows doesn't allow repeated digits (in the computer-generated number),

nor the use of the zero digit.


To allow a zero digit, change the   <big> '''1''' </big>   on the 2<sup>nd</sup> REXX statement for the   '''random'''   BIF invocation.

To allow repeated digits in the computer-generated number, delete the 3<sup>rd</sup> REXX statement     '''if pos(r, ?)···''',

and also change the prompt message.

The REXX statement that contains the   '''translate'''   statement can be removed if repeated digits aren't allowed.

```rexx
/*REXX program scores the   Bulls & Cows   game with  CBLFs  (Carbon Based Life Forms). */
?=;     do  until length(?)==4;   r=random(1, 9) /*generate a unique four-digit number. */
        if pos(r, ?)\==0  then iterate           /*don't allow a repeated digit/numeral.*/
        ?=? || r                                 /*add random digit by concatenation.   */
        end   /*until length ···*/               /* [↑]  builds a unique four-digit num.*/
$= '──────── [Bulls & Cows]  '
        do  until  bulls==4;      say            /*play until guessed  or enters "QUIT".*/
        say $  'Please enter a 4-digit guess (with no zeroes)   [or Quit]:'
        pull n;  n=space(n, 0);   if abbrev('QUIT', n, 1)  then exit   /*wants to quit ?*/
        q=?;    L=length(n);  bulls=0;   cows=0  /*initialize some REXX variables.      */

             do j=1  for L;   if substr(n, j, 1)\==substr(q, j, 1)  then iterate /*bull?*/
             bulls=bulls +1;  q=overlay(., q, j)    /*bump bull count; disallow for cow.*/
             end   /*j*/                            /* [↑]  bull count────────~─────────*/
                                                                                 /*cow ?*/
             do k=1  for L;   _=substr(n, k, 1);        if pos(_, q)==0  then iterate
             cows=cows + 1;   q=translate(q, , _)   /*bump cow count; allow mult digits.*/
             end   /*k*/                            /* [↑]  cow  count─────────~────────*/
        say;                  @= 'You got'  bulls
        if L\==0 & bulls\==4  then say $  @  'bull's(bulls)   "and"   cows   'cow's(cows).
        end   /*until bulls==4*/
say
                      say "          ┌─────────────────────────────────────────┐"
                      say "          │                                         │"
                      say "          │  Congratulations, you've guessed it !!  │"
                      say "          │                                         │"
                      say "          └─────────────────────────────────────────┘";     say
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
s:  if arg(1)==1  then return '';   return "s"   /*this function handles pluralization. */
```





### Version 2


```rexx

/*REXX program to play the game of  "Bulls & Cows". *******************
* Changes from Version 1:
* ?= -> qq='' (righthandside mandatory and I never use ? as symbol -
*                               although it is available on all Rexxes)
* implemented singular/plural distinction differently
* change getRand to avoid invalid digit rejection
* check user's input for multiple digits
* add feature MM to ease guessing (MM=Mastermind - a similar game)
* add feature ? to see the solution (for the impatient player)
* program runs as is on ooRexx and on TSO (after changing | to !)
* Made source and output more compact
* formatted source 'my way'  2 July 2012 Walter Pachl
**********************************************************************/
ask='<Bulls & Cows game>   Please enter a four-digit guess  (or QUIT):'

b.='bulls'; b.1='bull'
c.='cows';  c.1='cow'
qq=getRand()
mm=0
Do Forever
  If get_guess()==qq Then leave
  Call scorer
  Say "You got" bulls b.bulls "and" cows c.cows"."
  If mm Then
    Say mms
  End   /*forever*/
Say "         *******************************************"
Say "         *                                         *"
Say "         *  Congratulations, you've guessed it !!  *"
Say "         *                                         *"
Say "         *******************************************"
Exit

get_guess:                           /*get a guess from the guesser. */

do forever
  Say ask
  Parse Pull guessi
  guess=translate(guessi)
  bc=verify(guess,987654321)
  Select
    When guess='?'       Then Say qq 'is the correct sequence'
    When guess='QUIT'    Then Exit
    When guess='MM'      Then Do
                              Say 'Mastermind output enabled'
                              mm=1
                              End
    When guess=''        Then Call ser 'no argument specified.'
    When words(guess)>1  Then Call ser 'too many arguments specified.'
    When verify(0,guess)=0 Then Call ser 'illegal digit: 0'
    When bc>0 Then Call ser 'illegal character:' substr(guessi,bc,1)
    When length(guess)<4 Then Call ser 'not enough digits'
    When length(guess)>4 Then Call ser 'too many digits'
    When dups(guess)     Then Call ser '4 DIFFERENT digits, please'
    Otherwise Do
      /********** Say guess ************/
      Return guess
      End
    End
  End

getRand:
digits='123456789'
qq=''
Do i=1 To 4
  d=random(1,length(digits))
  d=substr(digits,d,1)
  qq=qq||d
  digits=space(translate(digits,' ',d),0)
  /************ Say qq digits ************/
  End
Return qq

scorer: g=qq
mms='----'
bulls=0
Do j=1 for 4
  If substr(guess,j,1)=substr(qq,j,1) Then Do
    bulls=bulls+1
    guess=overlay(' ',guess,j)
    mms=overlay('+',mms,j)
    End
  End
cows=0
Do j=1 To 4
  If pos(substr(guess,j,1),qq)>0 Then Do
    cows=cows+1
    mms=overlay('.',mms,j)
    End
  End
Return

dups: Procedure
Parse Arg s
Do i=1 To 3
  If pos(substr(s,i,1),substr(s,i+1))>0 Then
    Return 1
  End
Return 0

ser: Say '*** error ***' arg(1); Return

```



## Ring


```ring

# Project : Bulls and cows

secret = ""
while len(secret) != 4
        c = char(48 + random(9))
        if substr(secret, c) = 0
           secret = secret + c
        ok
end

see "guess a four-digit number with no digit used twice."
guesses = 0
guess = ""
while true
        guess = ""
        while len(guess) != 4
                see "enter your guess: "
                give guess
                if len(guess) != 4
                   see "must be a four-digit number" + nl
                ok
        end
        guesses = guesses + 1
        if guess = secret
           see "you won after " + guesses + " guesses!"
           exit
        ok
        bulls = 0
        cows = 0
        for i = 1 to 4
             c = secret[i]
             if guess[i] = c
                bulls = bulls + 1
             but substr(guess, c) > 0
                 cows = cows + 1
             ok
        next
        see "you got " + bulls + " bull(s) and " + cows + " cow(s)." + nl
end

```



## Ruby

Inspired by Tcl
{{works with|Ruby|1.8.7+}}

```ruby
def generate_word(len)
  [*"1".."9"].shuffle.first(len)        # [*"1".."9"].sample(len)  ver 1.9+
end

def get_guess(len)
  loop do
    print "Enter a guess: "
    guess = gets.strip
    err = case
          when guess.match(/[^1-9]/)             ; "digits only"
          when guess.length != len               ; "exactly #{len} digits"
          when guess.split("").uniq.length != len; "digits must be unique"
          else return guess.split("")
          end
    puts "the word must be #{len} unique digits between 1 and 9 (#{err}). Try again."
  end
end

def score(word, guess)
  bulls = cows = 0
  guess.each_with_index do |num, idx|
    if word[idx] == num
      bulls += 1
    elsif word.include? num
      cows += 1
    end
  end
  [bulls, cows]
end

word_length = 4
puts "I have chosen a number with #{word_length} unique digits from 1 to 9."
word = generate_word(word_length)
count = 0
loop do
  guess = get_guess(word_length)
  count += 1
  break if word == guess
  puts "that guess has %d bulls and %d cows" % score(word, guess)
end
puts "you guessed correctly in #{count} tries."
```


Inspired by Python
{{works with|Ruby|2.0+}}

```ruby
size = 4
secret = [*'1' .. '9'].sample(size)
guess = nil

i=0
loop do
  i+=1
  loop do
    print "Guess #{i}: "
    guess = gets.chomp.chars
    exit if guess.empty?

    break if guess.size == size and
             guess.all? { |x| ('1'..'9').include? x } and
             guess.uniq.size == size

    puts "Problem, try again. You need to enter #{size} unique digits from 1 to 9"
  end

  if guess == secret
    puts "Congratulations you guessed correctly in #{i} attempts"
    break
  end

  bulls = cows = 0
  size.times do |j|
    if guess[j] == secret[j]
      bulls += 1
    elsif secret.include? guess[j]
      cows += 1
    end
  end

  puts "Bulls: #{bulls}; Cows: #{cows}"
end
```



## Rust

{{libheader|rand}}

```rust
use std::io;
use rand::{Rng,thread_rng};

extern crate rand;

const NUMBER_OF_DIGITS: usize = 4;

static DIGITS: [char; 9] = ['1', '2', '3', '4', '5', '6', '7', '8', '9'];

fn generate_digits() -> Vec<char> {
    let mut temp_digits: Vec<_> = (&DIGITS[..]).into();
    thread_rng().shuffle(&mut temp_digits);
    return temp_digits.iter().take(NUMBER_OF_DIGITS).map(|&a| a).collect();
}

fn parse_guess_string(guess: &str) -> Result<Vec<char>, String> {
    let chars: Vec<char> = (&guess).chars().collect();

    if !chars.iter().all(|c| DIGITS.contains(c)) {
        return Err("only digits, please".to_string());
    }

    if chars.len() != NUMBER_OF_DIGITS {
        return Err(format!("you need to guess with {} digits", NUMBER_OF_DIGITS));
    }

    let mut uniques: Vec<char> = chars.clone();
    uniques.dedup();
    if uniques.len() != chars.len() {
        return Err("no duplicates, please".to_string());
    }

    return Ok(chars);
}

fn calculate_score(given_digits: &[char], guessed_digits: &[char]) -> (usize, usize) {
    let mut bulls = 0;
    let mut cows = 0;
    for i in 0..NUMBER_OF_DIGITS {
        let pos: Option<usize> = guessed_digits.iter().position(|&a| -> bool {a == given_digits[i]});
        match pos {
            None              => (),
            Some(p) if p == i => bulls += 1,
            Some(_)           => cows += 1
        }
    }
    return (bulls, cows);
}

fn main() {
    let reader = io::stdin();

    loop {
        let given_digits = generate_digits();
        println!("I have chosen my {} digits. Please guess what they are", NUMBER_OF_DIGITS);

        loop {
            let guess_string: String = {
                let mut buf = String::new();
                reader.read_line(&mut buf).unwrap();
                buf.trim().into()
            };

            let digits_maybe = parse_guess_string(&guess_string);
            match digits_maybe {
                Err(msg) => {
                    println!("{}", msg);
                    continue;
                },
                Ok(guess_digits) => {
                    match calculate_score(&given_digits, &guess_digits) {
                        (NUMBER_OF_DIGITS, _) => {
                            println!("you win!");
                            break;
                        },
                        (bulls, cows) => println!("bulls: {}, cows: {}", bulls, cows)
                    }
                }
            }
        }
    }
}
```



## Scala


```scala
import scala.util.Random

object BullCow {
   def main(args: Array[String]): Unit = {
      val number=chooseNumber
      var guessed=false
      var guesses=0

      while(!guessed){
         Console.print("Guess a 4-digit number with no duplicate digits: ")
         val input=Console.readInt
         val digits=input.toString.map(_.asDigit).toList
         if(input>=1111 && input<=9999 && !hasDups(digits)){
            guesses+=1
            var bulls, cows=0
            for(i <- 0 to 3)
               if(number(i)==digits(i))
                  bulls+=1
               else if(number.contains(digits(i)))
                  cows+=1

            if(bulls==4)
               guessed=true
            else
               println("%d Cows and %d Bulls.".format(cows, bulls))
         }
      }
      println("You won after "+guesses+" guesses!");
   }

   def chooseNumber={
      var digits=List[Int]()
      while(digits.size<4){
         val d=Random.nextInt(9)+1
         if (!digits.contains(d))
            digits=digits:+d
      }
      digits
   }

   def hasDups(input:List[Int])=input.size!=input.distinct.size
}
```



## Scheme

{{works with|any R6RS Scheme}}


```scheme


;generate a random non-repeating list of 4 digits, 1-9 inclusive
(define (get-num)
  (define (gen lst)
    (if (= (length lst) 4) lst
        (let ((digit (+ (random 9) 1)))
          (if (member digit lst) ;make sure the new digit isn't in the
                                 ;list
              (gen lst)
              (gen (cons digit lst))))))
  (string->list (apply string-append (map number->string (gen '())))))

;is g a valid guess (that is, non-repeating, four digits 1-9
;inclusive?)
(define (valid-guess? g)
  (let ((g-num (string->number (apply string g))))
    ;does the same digit appear twice in lst?
    (define (repeats? lst)
      (cond ((null? lst) #f)
            ((member (car lst) (cdr lst)) #t)
            (else (repeats? (cdr lst)))))
    (and g-num
         (> g-num 1233)
         (< g-num 9877)
         (not (repeats? g)))))

;return '(cows bulls) for the given guess
(define (score answer guess)
  ;total cows + bulls
  (define (cows&bulls a g)
    (cond ((null? a) 0)
          ((member (car a) g) (+ 1 (cows&bulls (cdr a) g)))
          (else (cows&bulls (cdr a) g))))
  ;bulls only
  (define (bulls a g)
    (cond ((null? a) 0)
          ((equal? (car a) (car g)) (+ 1 (bulls (cdr a) (cdr g))))
          (else (bulls (cdr a) (cdr g)))))
  (list (- (cows&bulls answer guess) (bulls answer guess)) (bulls answer guess)))

;play the game
(define (bull-cow answer)
  ;get the user's guess as a list
  (define (get-guess)
    (let ((e (read)))
      (if (number? e)
          (string->list (number->string e))
          (string->list (symbol->string e)))))
  (display "Enter a guess: ")
  (let ((guess (get-guess)))
    (if (valid-guess? guess)
        (let ((bulls (cadr (score answer guess)))
              (cows (car (score answer guess))))
          (if (= bulls 4)
              (display "You win!\n")
              (begin
                (display bulls)
                (display " bulls, ")
                (display cows)
                (display " cows.\n")
                (bull-cow answer))))
        (begin
          (display "Invalid guess.\n")
          (bull-cow answer)))))

(bull-cow (get-num))

```



###  Sample game play


```txt

Enter a guess: 1234
0 bulls, 1 cows.
Enter a guess: 2345
1 bulls, 0 cows.
Enter a guess: 2346
1 bulls, 1 cows.
Enter a guess: 2367
0 bulls, 1 cows.
Enter a guess: 2647
1 bulls, 1 cows.
Enter a guess: 2648
2 bulls, 1 cows.
Enter a guess: 2468
1 bulls, 2 cows.
Enter a guess: 1468
1 bulls, 2 cows.
Enter a guess: 2684
0 bulls, 3 cows.
Enter a guess: 6248
3 bulls, 0 cows.
Enter a guess: 6948
You win!

```



## Seed7


```seed7
$ include "seed7_05.s7i";

const proc: main is func
  local
    const integer: size is 4;
    var set of char: digits is {'1' .. '9'};
    var string: chosen is " " mult size;
    var integer: guesses is 0;
    var string: guess is "";
    var integer: pos is 0;
    var integer: bulls is 0;
    var integer: cows is 0;
    var boolean: okay is FALSE;
  begin
    for pos range 1 to 4 do
      chosen @:= [pos] rand(digits);
      excl(digits, chosen[pos]);
    end for;
    writeln("I have chosen a number from " <& size <& " unique digits from 1 to 9 arranged in a random order.");
    writeln("You need to input a " <& size <& " digit, unique digit number as a guess at what I have chosen");
    repeat
      incr(guesses);
      repeat
        write("Next guess [" <& guesses <& "]: ");
        readln(guess);
        okay := length(guess) = size;
        for key pos range guess do
          okay := okay and guess[pos] in {'1' .. '9'} and pos(guess[succ(pos) ..], guess[pos]) = 0;
        end for;
        if not okay then
          writeln("Problem, try again. You need to enter " <& size <& " unique digits from 1 to 9");
        end if;
      until okay;
      if guess <> chosen then
        bulls := 0;
        cows := 0;
        for key pos range chosen do
          if guess[pos] = chosen[pos] then
            incr(bulls);
          elsif pos(chosen, guess[pos]) <> 0 then
            incr(cows);
          end if;
        end for;
        writeln("  " <& bulls <& " Bulls");
        writeln("  " <& cows <& " Cows");
      end if;
    until guess = chosen;
    writeln("Congratulations you guessed correctly in " <& guesses <& " attempts");
  end func;
```


{{out}}

```txt

I have chosen a number from 4 unique digits from 1 to 9 arranged in a random order.
You need to input a 4 digit, unique digit number as a guess at what I have chosen
Next guess [1]: 1234
  1 Bulls
  0 Cows
Next guess [2]: 1567
  1 Bulls
  2 Cows
Next guess [3]: 1856
  3 Bulls
  0 Cows
Next guess [4]: 1956
Congratulations you guessed correctly in 4 attempts

```



## Sidef


```ruby
var size = 4
var num = @(1..9).shuffle.first(size)

for (var guesses = 1; true; guesses++) {

    var bulls = 0
    var cows  = 0

    var input =
        read("Input: ", String).chars           \
                               .uniq            \
                               .grep(/^[1-9]$/) \
                               .map{.to_n}

    if (input.len != size) {
        warn "Invalid input!\n"
        guesses--
        next
    }

    if (input == num) {
        printf("You did it in %d attempts!\n", guesses)
        break
    }

    for i (^num) {
        if (num[i] == input[i]) {
            bulls++
        }
        elsif (num.contains(input[i])) {
            cows++
        }
    }

    "Bulls: %d; Cows: %d\n".printf(bulls, cows)
}
```

{{out}}

```txt

Input: 2953
Bulls: 1; Cows: 1
Input: 9654
Bulls: 1; Cows: 1
Input: 8924
Bulls: 1; Cows: 3
Input: 2894
You did it in 4 attempts!

```



## Smalltalk

{{works with|GNU Smalltalk}}

```smalltalk
Object subclass: BullsCows [
  |number|
  BullsCows class >> new: secretNum [ |i|
    i := self basicNew.
    (self isValid: secretNum)
       ifFalse: [ SystemExceptions.InvalidArgument
                    signalOn: secretNum
                    reason: 'You need 4 unique digits from 1 to 9' ].
    i setNumber: secretNum.
    ^ i
  ]
  BullsCows class >> new [ |b| b := Set new.
     [ b size < 4 ]
       whileTrue: [ b add: ((Random between: 1 and: 9) displayString first) ].
     ^ self new: (b asString)
  ]
  BullsCows class >> isValid: num [
    ^ (num asSet size = 4) & ((num asSet includes: $0) not)
  ]
  setNumber: num [ number := num ]
  check: guess [ |bc| bc := Bag new.
     1 to: 4 do: [ :i |
       (number at: i) = (guess at: i)
         ifTrue: [ bc add: 'bulls' ]
         ifFalse: [
             (number includes: (guess at: i))
               ifTrue: [ bc add: 'cows' ]
         ]
     ].
     ^ bc
  ]
].

'Guess the 4-digits number (digits from 1 to 9, no repetition)' displayNl.

|guessMe d r tries|
[
  tries := 0.
  guessMe := BullsCows new.
  [
    [
      'Write 4 digits: ' display.
      d := stdin nextLine.
      (BullsCows isValid: d)
    ] whileFalse: [
         'Insert 4 digits, no repetition, exclude the digit 0' displayNl
    ].
    r := guessMe check: d.
    tries := tries + 1.
    (r occurrencesOf: 'bulls') = 4
  ] whileFalse: [
    ('%1 cows, %2 bulls' % { r occurrencesOf: 'cows'. r occurrencesOf: 'bulls' })
      displayNl.
  ].
  ('Good, you guessed it in %1 tries!' % { tries }) displayNl.
  'Do you want to play again? [y/n]' display.
  ( (stdin nextLine) = 'y' )
] whileTrue: [ Character nl displayNl ].
```




## Smart BASIC


```smart BASIC

'by rbytes, January 2017
OPTION BASE 1
P("  B U L L S   A N D    C O W S")!l
P("A secret 4-digit number has been created, with")
P("no repeats and no zeros. You must guess the number.")
P("After each guess, you will be shown how many of your")
P("digits are correct and in the matching location (bulls),")
P("and how many are correct but in a different location (cows).")
p("See how many tries it takes you to get the right answer.")
' generate a 4-digit number with no repeats
guesses  =   0
1 WHILE LEN(sec$) <4
  c$ =CHR$( INTEG(RND(1) *9) +49)
  IF INSTR(sec$, c$)=-1 THEN sec$&=c$
ENDWHILE!l
2 PRINT "Your guess: "
INPUT "FOUR DIGITS": guess$!l
IF guess$="" THEN       ' check if entry is null
  P("Please enter something!")!l
  GOTO 2
ENDIF
P(guess$)!l
IF LEN(guess$)<>4 THEN  ' check if entry is exactly 4 characters
  P("Please enter exactly 4 digits.")!l
  GOTO 2
ENDIF
FOR t=1 TO 4            ' check if all characters are digits 1 - 9
  IF INSTR("123456789",MID$(guess$,t,1))=-1 THEN
    P("You have entered at least one illegal character")!l
    GOTO 2
  ENDIF
NEXT t
rep = check(guess$)     ' check if any digits are repeated
IF check.chk THEN
  P("Please enter a number with no repeated digits.")!l
  GOTO 2
ENDIF

guesses+=1
r$=score$(guess$,sec$)
P(r$)!l

IF guess$=sec$ THEN
  P("W I N N E R ! ! !")!l
  IF guesses>1 THEN gs$="guesses!" ELSE gs$="guess!"
  P("You won after "&guesses&" "&gs$)
  P("Thanks for playing!")!l
  PAUSE 2
  P("A G A I N  with a new secret number")!l
  guesses=0
END IF
GOTO 1
END '   _____________________________________________________________

DEF P(p$)              ' function to print a string
  PRINT p$
END DEF

DEF L()                ' function to print an empty line
  PRINT
END DEF

DEF check(i$)          ' check=0 if digit is not repeated, else=1
  chk=0
  FOR i =1 TO 3
    FOR j =i +1 TO 4
      IF MID$( i$, i, 1)=MID$( i$, j, 1) THEN chk =1
    NEXT j
  NEXT i
END DEF

DEF score$(a$,b$)      ' calculate the numbers of bulls & cows.
  bulls=0!cows=0
  FOR i = 1 TO 4
    c$ = MID$( a$, i, 1)
    IF MID$( b$, i, 1)=c$ THEN
      bulls+=1
    ELSE
      IF INSTR( b$, c$) <>-1 AND INSTR( b$, c$) <>i THEN
        cows+=1
      END IF
    END IF
  NEXT i
  r$ ="Bulls: "&STR$( bulls)& ", "& "Cows: " &STR$( cows)
  RETURN r$
END DEF
END

```


## Swift



```swift
import Foundation

func generateRandomNumArray(numDigits: Int = 4) -> [Int] {
  guard numDigits > 0 else {
    return []
  }

  let needed =  min(9, numDigits)
  var nums = Set<Int>()

  repeat {
    nums.insert(.random(in: 1...9))
  } while nums.count != needed

  return Array(nums)
}

func parseGuess(_ guess: String) -> [Int]? {
  guard guess.count == 4 else {
    return nil
  }

  let guessArray = guess.map(String.init).map(Int.init).compactMap({ $0 })

  guard Set(guessArray).count == 4 else {
    return nil
  }

  return guessArray
}

while true {
  let num = generateRandomNumArray()
  var bulls = 0
  var cows = 0

  print("Please enter a 4 digit number with digits between 1-9, no repetitions: ")

  guard let guessStr = readLine(strippingNewline: true), let guess = parseGuess(guessStr) else {
    print("Invalid input")
    continue
  }

  for (guess, actual) in zip(guess, num) {
    if guess == actual {
      bulls += 1
    } else if num.contains(guess) {
      cows += 1
    }
  }

  print("Actual number: \(num.map(String.init).joined())")
  print("Your score: \(bulls) bulls and \(cows) cows\n")
  print("Would you like to play again? (y): ")

  guard readLine(strippingNewline: true)!.lowercased() == "y" else {
    exit(0)
  }
}
```


{{out}}


```txt
Please enter a 4 digit number with digits between 1-9, no repetitions:
8496
Actual number: 6475
Your score: 1 bulls and 1 cows

Would you like to play again? (y):
y
Please enter a 4 digit number with digits between 1-9, no repetitions:
5983
Actual number: 7846
Your score: 0 bulls and 1 cows

```



## Tcl


```tcl
proc main {} {
    fconfigure stdout -buffering none
    set length 4

    puts "I have chosen a number from $length unique digits from 1 to 9 arranged in a random order.
You need to input a $length digit, unique digit number as a guess at what I have chosen
    "

    while true {
        set word [generateWord $length]
        set count 1
        while {[set guess [getGuess $length]] ne $word} {
            printScore $length $word $guess
            incr count
        }
        puts "You guessed correctly in $count tries."
        if {[yn "Play again?"] eq "n"} break
    }
}

proc generateWord {length} {
    set chars 123456789
    for {set i 1} {$i <= $length} {incr i} {
        set idx [expr {int(rand() * [string length $chars])}]
        append word [string index $chars $idx]
        set chars [string replace $chars $idx $idx]
    }
    return $word

    # here's another way to generate word with no duplications
    set word ""
    while {[string length $word] < $length} {
        set char [expr {int(1 + 9*rand())}]
        if {[string first $char $word] == -1} {
            append word $char
        }
    }
}

proc getGuess {length} {
    puts -nonewline "Enter your guess: "
    while true {
        gets stdin guess
        if {[string match [string repeat {[1-9]} $length] $guess]} {
            return $guess
        }
        if {[string tolower [string trim $guess]] eq "quit"} {
            puts Bye
            exit
        }
        puts "The word must be $length digits between 1 and 9 inclusive.  Try again."
    }
}

proc printScore {length word guess} {
    set bulls 0
    set cows 0
    for {set i 0} {$i < $length} {incr i} {
        if {[string index $word $i] eq [string index $guess $i]} {
            incr bulls
            set word [string replace $word $i $i +]
        }
    }
    puts "  $bulls bulls"
    for {set i 0} {$i < $length} {incr i} {
        if {[set j [string first [string index $guess $i] $word]] != -1} {
            incr cows
            set word [string replace $word $j $j -]
        }
    }
    puts "  $cows cows"
}

proc yn {msg} {
    while true {
        puts -nonewline "$msg \[y/n] "
        gets stdin ans
        set char [string tolower [string index [string trim $ans] 0]]
        if {$char eq "y" || $char eq "n"} {
            return $char
        }
    }
}

main
```


## TUSCRIPT


```tuscript

$$ MODE tuscript
SET nr1=RANDOM_NUMBERS (1,9,1)
LOOP
  SET nr2=RANDOM_NUMBERS (1,9,1)
  IF (nr2!=nr1) EXIT
ENDLOOP
LOOP
  SET nr3=RANDOM_NUMBERS (1,9,1)
  IF (nr3!=nr1,nr2) EXIT
ENDLOOP
LOOP
  SET nr4=RANDOM_NUMBERS (1,9,1)
  IF (nr4!=nr1,nr2,nr3) EXIT
ENDLOOP
SET nr=JOIN(nr1,"'",nr2,nr3,nr4), limit=10
LOOP r=1,limit
SET bulls=cows=0
ASK "round {r} insert a number":guessnr=""
SET length=LENGTH(guessnr), checknr=STRINGS (guessnr,":>/:")
 LOOP n=nr,y=checknr
  IF (length!=4) THEN
   PRINT "4-letter digit required"
   EXIT
  ELSEIF (n==y) THEN
   SET bulls=bulls+1
  ELSEIF (nr.ct.":{y}:") THEN
   SET cows=cows+1
  ENDIF
 ENDLOOP
PRINT "bulls=",bulls," cows=",cows
 IF (bulls==4) THEN
  PRINT "BINGO"
  EXIT
 ELSEIF (r==limit) THEN
  PRINT "BETTER NEXT TIME"
  EXIT
 ENDIF
ENDLOOP

```

Output:

```txt

round 1 insert a number >1234
bulls=1 cows=1
round 2 insert a number >5678
bulls=1 cows=1
round 3 insert a number >1298
bulls=2 cows=0
round 4 insert a number >2379
bulls=0 cows=0
round 5 insert a number >1468
bulls=4 cows=0
BINGO

```



## uBasic/4tH

<lang>Local(2)                               ' Let's use no globals

Proc _Initialize                       ' Get our secret number

Do                                     ' Repeat until it's guessed
  Do
    Input "Enter your guess: ";a@      ' Enter your guess
    While FUNC(_Invalid(a@))           ' but make sure it's a valid guess
  Loop

  a@ = FUNC(_Bulls)                    ' Count the number of bulls
  b@ = FUNC(_Cows)                     ' Count the number of cows
                                       ' Now give some feedback
  Print : Print "\tThere were ";a@;" bulls and ";b@;" cows." : Print
  Until a@ = 4                         ' Until the secret is guessed
Loop

Print "You win!"                       ' Yes, you guessed it

End


_Initialize                            ' Make a secret
  Local (1)

  Do
    a@ = 1234 + RND(8643)              ' Get a valid number
    While FUNC(_Invalid(a@))           ' and accept it unless invalid
  Loop

  For a@ = 0 to 3                      ' Now save it at the proper place
    @(a@+4) = @(a@)
  Next
Return


_Invalid Param(1)                      ' Check whether a number is valid
  Local(2)
                                       ' Ok, these can't be right at all
  If (a@ < 1234) + (a@ > 9876) Then Return (1)
                                       ' Now break 'em up in different digits
  For b@ = 3 To 0 Step -1
    @(b@) = a@ % 10                    ' A digit of zero can't be right
    If @(b@) = 0 Then Unloop : Return (1)
    a@ = a@ / 10
  Next

  For b@ = 0 To 2                      ' Now compare all digits
    For c@ = b@ + 1 To 3               ' The others were already compared
      If @(b@) = @(c@) Then Unloop : Unloop : Return (1)
    Next                               ' Wrong, we found similar digits
  Next
Return (0)                             ' All digits are different


_Bulls                                 ' Count the number of valid guesses
  Local (2)

  b@ = 0                               ' Start with zero

  For a@ = 0 to 3                      ' Increment with each valid guess
    If @(a@) = @(a@+4) Then b@ = b@ + 1
  Next
Return (b@)                            ' Return number of valid guesses


_Cows
  Local (3)                            ' Count the number of proper digits

  c@ = 0                               ' Start with zero

  For a@ = 0 To 3                      ' All the players guesses
    For b@ = 4 To 7                    ' All the computers secrets
      If (a@+4) = b@ Then Continue     ' Skip the bulls
      If @(a@) = @(b@) Then c@ = c@ + 1
    Next                               ' Increment with valid digits
  Next
Return (c@)                            ' Return number of valid digits
```



## UNIX Shell

{{works with|bash|3}}


```bash
#!/bin/bash

rand() {
  local min=${1:-0}
  local max=${2:-32767}

  [ ${min} -gt ${max} ] &&
  min=$(( min ^ max )) &&
  max=$(( min ^ max )) &&
  min=$(( min ^ max ))

  echo -n $(( ( $RANDOM % $max ) + $min ))
}

in_arr() {
  local quandry="${1}"
  shift
  local arr=( $@ )
  local i=''

  for i in ${arr[*]}
  do
    [ "${quandry}" == "${i}" ] && return 0 && break
  done

  return 1
}

delete_at() {
  local idx="$(( $1 + 1 ))"
  shift
  local arr=( "sentinel" $@ )

  echo -n "${arr[@]:1:$(( idx - 1 ))} ${arr[@]:$((idx + 1)):$(( ${#arr[@]} - idx - 1))}"
}

delete_first() {
  local meanie="${1}"
  shift
  local arr=( $@ )
  local i=0

  for (( i = 0; i < ${#arr[@]} ; i++ ))
  do
    [ "${arr[${i}]}" == "${meanie}" ] && arr=( $( delete_at ${i} ${arr[*]} ) )
  done

  echo -n "${arr[*]}"
}

to_arr() {
  local string="${1}"
  local arr=()

  while [ "${#string}" -gt 0 ]
  do
    arr=( ${arr[*]} ${string:0:1} )
    string="${string:1}"
  done

  echo -n "${arr[*]}"
}

choose_idx() {
  local arr=( $@ )

  echo -n "$( rand 0 $(( ${#arr[@]} - 1 )) )"
}

locate_bulls() {
  local secret=( $( to_arr "${1}" ) )
  local guess=( $( to_arr "${2}" ) )
  local hits=()
  local i=0

  for (( i=0; i<4; i++ ))
  do
    [ "${secret[${i}]}" -eq "${guess[${i}]}" ] && hits=( ${hits[*]} ${i} )
  done

  echo -n "${hits[*]}"
}

bulls() {
  local secret="${1}"
  local guess="${2}"
  local bulls=( $( locate_bulls "${secret}" "${guess}" ) )

  echo -n "${#bulls[@]}"
}

cows() {
  local secret=( $( to_arr "${1}" ) )
  local guess=( $( to_arr "${2}" ) )
  local bulls=( $( locate_bulls "${1}" "${2}" ) )
  local hits=0
  local i=''

  # Avoid double-counting bulls
  for i in ${bulls[*]}
  do
    secret=( $( delete_at ${i} ${secret[*]} ) )
  done

  # Process the guess against what's left of the secret
  for i in ${guess[*]}
  do
    in_arr "${i}" ${secret[*]} &&
    secret=( $( delete_first "${i}" ${secret[*]} ) ) &&
    (( hits++ ))
  done

  echo -n ${hits}
}

malformed() {
  local guess=( $( to_arr "${1}" ) )
  local i=''

  [ ${#guess[@]} -ne 4 ] &&
  return 0

  for i in ${guess[*]}
  do
    if ! in_arr ${i} 1 2 3 4 5 6 7 8 9
    then
      return 0
      break
    fi
  done

  return 1
}

candidates=( 1 2 3 4 5 6 7 8 9 )
secret=''

while [ "${#secret}" -lt 4 ]
do
  cidx=$( choose_idx ${candidates[*]} )
  secret="${secret}${candidates[${cidx}]}"
  candidates=( $(delete_at ${cidx} ${candidates[*]} ) )
done

while read -p "Enter a four-digit guess:  " guess
do
  malformed "${guess}" && echo "Malformed guess" && continue
  [ "${guess}" == "${secret}" ] && echo "You win!" && exit
  echo "Score: $( bulls "${secret}" "${guess}" ) Bulls, $( cows "${secret}" "${guess}" ) Cows"
done
```



## VBA



```vb

Option Explicit

Sub Main_Bulls_and_cows()
Dim strNumber As String, strInput As String, strMsg As String, strTemp As String
Dim boolEnd As Boolean
Dim lngCpt As Long
Dim i As Byte, bytCow As Byte, bytBull As Byte
Const NUMBER_OF_DIGITS As Byte = 4
Const MAX_LOOPS As Byte = 25 'the max of lines supported by MsgBox

    strNumber = Create_Number(NUMBER_OF_DIGITS)
    Do
        bytBull = 0: bytCow = 0: lngCpt = lngCpt + 1
        If lngCpt > MAX_LOOPS Then strMsg = "Max of loops... Sorry you loose!": Exit Do
        strInput = AskToUser(NUMBER_OF_DIGITS)
        If strInput = "Exit Game" Then strMsg = "User abort": Exit Do
        For i = 1 To Len(strNumber)
            If Mid(strNumber, i, 1) = Mid(strInput, i, 1) Then
                bytBull = bytBull + 1
            ElseIf InStr(strNumber, Mid(strInput, i, 1)) > 0 Then
                bytCow = bytCow + 1
            End If
        Next i
        If bytBull = Len(strNumber) Then
            boolEnd = True: strMsg = "You win in " & lngCpt & " loops!"
        Else
            strTemp = strTemp & vbCrLf & "With : " & strInput & " ,you have : " & bytBull & " bulls," & bytCow & " cows."
            MsgBox strTemp
        End If
    Loop While Not boolEnd
    MsgBox strMsg
End Sub

Function Create_Number(NbDigits As Byte) As String
Dim myColl As New Collection
Dim strTemp As String
Dim bytAlea As Byte

    Randomize
    Do
        bytAlea = Int((Rnd * 9) + 1)
        On Error Resume Next
        myColl.Add CStr(bytAlea), CStr(bytAlea)
        If Err <> 0 Then
            On Error GoTo 0
        Else
            strTemp = strTemp & CStr(bytAlea)
        End If
    Loop While Len(strTemp) < NbDigits
    Create_Number = strTemp
End Function

Function AskToUser(NbDigits As Byte) As String
Dim boolGood As Boolean, strIn As String, i As Byte, NbDiff As Byte

    Do While Not boolGood
        strIn = InputBox("Enter your number (" & NbDigits & " digits)", "Number")
        If StrPtr(strIn) = 0 Then strIn = "Exit Game": Exit Do
        If strIn <> "" Then
            If Len(strIn) = NbDigits Then
                NbDiff = 0
                For i = 1 To Len(strIn)
                    If Len(Replace(strIn, Mid(strIn, i, 1), "")) < NbDigits - 1 Then
                        NbDiff = 1
                        Exit For
                    End If
                Next i
                If NbDiff = 0 Then boolGood = True
            End If
        End If
    Loop
    AskToUser = strIn
End Function

```



## Vedit macro language


```vedit
Buf_Switch(Buf_Free)
#90 = Time_Tick                     // seed for random number generator
#91 = 10                            // random numbers in range 0 to 9
while (EOB_pos < 4) {               // 4 digits needed
    Call("RANDOM")
    BOF Ins_Char(Return_Value + '0')
    Replace("(.)(.*)\1", "\1\2", REGEXP+BEGIN+NOERR)  // remove any duplicate
}

#3 = 0
repeat (99) {
    Get_Input(10, "Guess a 4-digit number with no duplicate digits: ", NOCR)
    if (Reg_Size(10) == 0) { Break }                // empty string = exit
    Num_Eval_Reg(10)                                // check for numeric digits
    if (Chars_Matched != 4) {
        M("You should enter 4 numeric digits\n")
        Continue
    }

    Goto_Pos(4)                                     // count bulls
    Reg_Ins(10, OVERWRITE)
    #1 = Search("(.)...\1", REGEXP+BEGIN+ALL+NOERR)

    RS(10, "[", INSERT)                             // count cows
    RS(10, "]", APPEND)
    #2 = Search_Block(@10, 0, 4, REGEXP+BEGIN+ALL+NOERR) - #1

    #3++
    NT(#1, NOCR) M(" bulls,") NT(#2, NOCR) M(" cows\n")
    if (#1 == 4) {
        M("You won after") NT(#3, NOCR) M(" guesses!\n")
        Break
    }
}
Buf_Quit(OK)
Return

//--------------------------------------------------------------
// Generate random numbers in range 0 <= Return_Value < #91
//  #90 = Seed    (0 to 0x7fffffff)
//  #91 = Scaling (0 to 0x10000)

:RANDOM:
#92 = 0x7fffffff / 48271
#93 = 0x7fffffff % 48271
#90 = (48271 * (#90 % #92) - #93 * (#90 / #92)) & 0x7fffffff
Return ((#90 & 0xffff) * #91 / 0x10000)
```



## Visual Basic .NET



```vbnet
Imports System
Imports System.Text.RegularExpressions

Module Bulls_and_Cows
    Function CreateNumber() As String
        Dim random As New Random()
        Dim sequence As Char() = {"1"c, "2"c, "3"c, "4"c, "5"c, "6"c, "7"c, "8"c, "9"c}

        For i As Integer = 0 To sequence.Length - 1
            Dim j As Integer = random.Next(sequence.Length)
            Dim temp As Char = sequence(i) : sequence(i) = sequence(j) : sequence(j) = temp
        Next

        Return New String(sequence, 0, 4)
    End Function

    Function IsFourDigitNumber(ByVal number As String) As Boolean
        Return Regex.IsMatch(number, "^[1-9]{4}$")
    End Function

    Sub Main()
        Dim chosenNumber As String = CreateNumber()
        Dim attempt As Integer = 0
        Console.WriteLine("Number is chosen")
        Dim gameOver As Boolean = False
        Do
            attempt += 1
            Console.WriteLine("Attempt #{0}. Enter four digit number: ", attempt)
            Dim number As String = Console.ReadLine()
            Do While Not IsFourDigitNumber(number)
                Console.WriteLine("Invalid number: type four characters. Every character must digit be between '1' and '9'.")
                number = Console.ReadLine()
            Loop

            Dim bulls As Integer = 0
            Dim cows As Integer = 0

            For i As Integer = 0 To number.Length - 1
                Dim j As Integer = chosenNumber.IndexOf(number(i))
                If i = j Then
                    bulls += 1
                ElseIf j >= 0 Then
                    cows += 1
                End If
            Next

            If bulls < chosenNumber.Length Then
                Console.WriteLine("The number '{0}' has {1} bulls and {2} cows", _
                    number, bulls, cows)
            Else
                gameOver = True
            End If
        Loop Until gameOver
        Console.WriteLine("The number was guessed in {0} attempts. Congratulations!", attempt)
    End Sub
End Module
```



## zkl

Play one game:

```zkl
d:=Dictionary(); do{ d[(1).random(10)]=True }while(d.len()<4);
abcd:=d.keys.shuffle();
while(1){
   guess:=ask("4 digits: ")-" ,";
   if(guess.len()!=4 or guess.unique().len()!=4) continue;
   bulls:=abcd.zipWith('==,guess).sum(0);
   cows:=guess.split("").enumerate()
      .reduce('wrap(s,[(n,c)]){ s + (d.find(c,False) and abcd[n]!=c) },0);
   if(bulls==4) { println("You got it!"); break; }
   "%d bull%s and %d cow%s".fmt(bulls,s(bulls),cows,s(cows)).println();
}
fcn s(n){ (n!=1) and "s" or "" }
```

{{out}}

```txt

4 digits: 4,5,6,7
1 bull and 0 cows
4 digits: 1234
0 bulls and 2 cows
4 digits: 3528
You got it!

```



## ZX Spectrum Basic

{{trans|QBasic}}

```zxbasic
10 DIM n(10): LET c$=""
20 FOR i=1 TO 4
30 LET d=INT (RND*9+1)
40 IF n(d)=1 THEN GO TO 30
50 LET n(d)=1
60 LET c$=c$+STR$ d
70 NEXT i
80 LET guesses=0
90 INPUT "Guess a 4-digit number (1 to 9) with no duplicate digits: ";guess
100 IF guess=0 THEN STOP
110 IF guess>9999 OR guess<1000 THEN PRINT "Only 4 numeric digits, please": GO TO 90
120 LET bulls=0: LET cows=0: LET guesses=guesses+1: LET g$=STR$ guess
130 FOR i=1 TO 4
140 IF g$(i)=c$(i) THEN LET bulls=bulls+1: GO TO 160
150 IF n(VAL g$(i))=1 THEN LET cows=cows+1
160 NEXT i
170 PRINT bulls;" bulls, ";cows;" cows"
180 IF c$=g$ THEN PRINT "You won after ";guesses;" guesses!": GO TO 10
190 GO TO 90

```

