+++
title = "Guess the number"
description = ""
date = 2019-10-21T16:57:55Z
aliases = []
[extra]
id = 8622
[taxonomies]
categories = []
tags = []
+++

{{task|Games}} 
[[Category: Conditional loops]] 
[[Category:Randomness]]

;Task:
Write a program where the program chooses a number between   '''1'''   and   '''10'''. 

A player is then prompted to enter a guess.   If the player guesses wrong,   then the prompt appears again until the guess is correct. 

When the player has made a successful guess the computer will issue a   "Well guessed!"   message,   and the program exits.

A   [[:Category:Conditional loops|conditional loop]]   may be used to repeat the guessing until the user is correct.


;Related tasks:
*   [[Bulls and cows]]
*   [[Bulls and cows/Player]]
*   [[Guess the number/With Feedback]]
*   [[Mastermind]]





## ABAP


```ABAP
REPORT guess_the_number.

DATA prng TYPE REF TO cl_abap_random_int.

cl_abap_random_int=>create(
  EXPORTING
    seed = cl_abap_random=>seed( )
    min  = 1
    max  = 10
  RECEIVING
    prng = prng ).

DATA(number) = prng->get_next( ).

DATA(field) = VALUE i( ).

cl_demo_input=>add_field( EXPORTING text = |Choice one number between 1 and 10| CHANGING field = field ).
cl_demo_input=>request( ).

WHILE number <> field.
  cl_demo_input=>add_field( EXPORTING text = |You miss, try again| CHANGING field = field ).
  cl_demo_input=>request( ).
ENDWHILE.

cl_demo_output=>display( |Well Done| ).

```



## Ada



```Ada
with Ada.Numerics.Discrete_Random;
with Ada.Text_IO;
procedure Guess_Number is
   subtype Number is Integer range 1 .. 10;
   package Number_IO is new Ada.Text_IO.Integer_IO (Number);
   package Number_RNG is new Ada.Numerics.Discrete_Random (Number);
   Generator  : Number_RNG.Generator;
   My_Number  : Number;
   Your_Guess : Number;
begin
   Number_RNG.Reset (Generator);
   My_Number := Number_RNG.Random (Generator);
   Ada.Text_IO.Put_Line ("Guess my number!");
   loop
      Ada.Text_IO.Put ("Your guess: ");
      Number_IO.Get (Your_Guess);
      exit when Your_Guess = My_Number;
      Ada.Text_IO.Put_Line ("Wrong, try again!");
   end loop;
   Ada.Text_IO.Put_Line ("Well guessed!");
end Guess_Number;
```


## Aime


```aime
file f;
integer n;
text s;

f.stdin;

n = irand(1, 10);
o_text("I'm thinking of a number between 1 and 10.\n");
o_text("Try to guess it!\n");
while (1) {
    f_look(f, "0123456789");
    f_near(f, "0123456789", s);
    if (atoi(s) != n) {
	o_text("That's not my number.\n");
	o_text("Try another guess!\n");
    } else {
	break;
    }
}

o_text("You have won!\n");
```



## ALGOL 68

{{trans|C|Note: This specimen retains the original [[#C|C]] coding style. [http://rosettacode.org/mw/index.php?title=Guess_the_number&action=historysubmit&diff=107856&oldid=107855 diff]}}
{{works with|ALGOL 68|Revision 1 - no extensions to language used.}}
{{works with|ALGOL 68G|Any - tested with release [http://sourceforge.net/projects/algol68/files/algol68g/algol68g-1.18.0/algol68g-1.18.0-9h.tiny.el5.centos.fc11.i386.rpm/download 1.18.0-9h.tiny].}}
{{wont work with|ELLA ALGOL 68|Any (with appropriate job cards) - tested with release [http://sourceforge.net/projects/algol68/files/algol68toc/algol68toc-1.8.8d/algol68toc-1.8-8d.fc9.i386.rpm/download 1.8-8d] - due to extensive use of '''format'''[ted] ''transput''.}}

```algol68
main:
(
    INT n;
    INT g;
    n := ENTIER (random*10+1);
    PROC puts = (STRING string)VOID: putf(standout, ($gl$,string));
    puts("I'm thinking of a number between 1 and 10.");
    puts("Try to guess it! ");
    DO
        readf(($g$, g));
        IF g = n THEN break
        ELSE
          puts("That's not my number. ");
          puts("Try another guess!")
        FI
    OD;
    break: 
    puts("You have won! ")
)
```

Sample output:

```txt

I'm thinking of a number between 1 and 10.
Try to guess it! 
1
That's not my number. 
Try another guess!
2
That's not my number. 
Try another guess!
3
You have won! 

```



## AppleScript



```AppleScript
on run
    -- define the number to be guessed
    set numberToGuess to (random number from 1 to 10)
    -- prepare a variable to store the user's answer
    set guessedNumber to missing value
    -- start a loop (will be exited by using "exit repeat" after a correct guess)
    repeat
        try
            -- ask the user for his/her guess
            set usersChoice to (text returned of (display dialog "Guess the number between 1 and 10 inclusive" default answer "" buttons {"Check"} default button "Check"))
            -- try to convert the given answer to an integer
            set guessedNumber to usersChoice as integer
        on error
            -- something gone wrong, overwrite user's answer with a non-matching value
            set guessedNumber to missing value
        end try
        -- decide if the user's answer was the right one
        if guessedNumber is equal to numberToGuess then
            -- the user guessed the correct number and gets informed
            display dialog "Well guessed! The number was " & numberToGuess buttons {"OK"} default button "OK"
            -- exit the loop (quits this application)
            exit repeat
        end if
    end repeat
end run
```



Or, constraining mutation, and abstracting a little to an '''until(predicate, function, value)''' pattern

```AppleScript
-- GUESS THE NUMBER ----------------------------------------------------------

on run
    -- isMatch :: Int -> Bool
    script isMatch
        on |λ|(x)
            tell x to its guess = its secret
        end |λ|
    end script
    
    -- challenge :: () -> {secret: Int, guess: Int}
    script challenge
        on response()
            set v to (text returned of (display dialog ¬
                "Guess the number in range 1-10" default answer ¬
                "" buttons {"Esc", "Check"} default button ¬
                "Check" cancel button "Esc"))
            
            if isInteger(v) then
                v as integer
            else
                -1
            end if
        end response
        
        on |λ|(rec)
            {secret:(random number from 1 to 10), guess:response() ¬
                of challenge, attempts:(attempts of rec) + 1}
        end |λ|
    end script
    
    
    -- MAIN LOOP -------------------------------------------------------------
    set rec to |until|(isMatch, challenge, {secret:-1, guess:0, attempts:0})
    
    display dialog (((guess of rec) as string) & ":    Well guessed ! " & ¬
        linefeed & linefeed & "Attempts: " & (attempts of rec))
end run


-- GENERIC FUNCTIONS ---------------------------------------------------------

-- isInteger :: a -> Bool
on isInteger(e)
    try
        set n to e as integer
    on error
        return false
    end try
    true
end isInteger

-- Lift 2nd class handler function into 1st class script wrapper 
-- mReturn :: Handler -> Script
on mReturn(f)
    if class of f is script then
        f
    else
        script
            property |λ| : f
        end script
    end if
end mReturn

-- until :: (a -> Bool) -> (a -> a) -> a -> a
on |until|(p, f, x)
    set mp to mReturn(p)
    set v to x
    
    tell mReturn(f)
        repeat until mp's |λ|(v)
            set v to |λ|(v)
        end repeat
    end tell
    return v
end |until|
```



## Arturo



```arturo
n $(random 1 10)
print "Guess the number: "

loop $(toNumber $(strip $(input))) != n {
	print "Wrong! Guess again: "
}
print "Well guessed!"
```


{{out}}


```txt
Guess the number: 
5
Wrong! Guess again: 
2
Wrong! Guess again: 
7
Well guessed!
```



## AutoHotkey


```AutoHotkey
Random, rand, 1, 10  ; This stores a number between 1 and 10 in the var rand using the Mersenne Twister
msgbox I am thinking of a number between 1 and 10.

loop
{
	InputBox, guess, Guess the number, Type in a number from 1 to 10
		If (guess = rand)
		{
			msgbox Well Guessed!
			Break ; exits loop
		}
		Else
			Msgbox Try again.
}

```

=={{Header|AutoIt}}==

```autoit

$irnd = Random(1, 10, 1)
$iinput = -1
While $input <> $irnd
	$iinput = InputBox("Choose a number", "Please chosse a Number between 1 and 10")
WEnd
MsgBox(0, "Success", "Well guessed!")

```


## AWK


```AWK

# syntax: GAWK -f GUESS_THE_NUMBER.AWK
BEGIN {
    srand()
    n = int(rand() * 10) + 1
    print("I am thinking of a number between 1 and 10. Try to guess it.")
    while (1) {
      getline ans
      if (ans !~ /^[0-9]+$/) {
        print("Your input was not a number. Try again.")
        continue
      }
      if (n == ans) {
        print("Well done you.")
        break
      }
      print("Incorrect. Try again.")
    }
    exit(0)
}

```



## BASIC


=
## Applesoft BASIC
=

```ApplesoftBasic
10 N% = RND(1) * 10 + 1
20 PRINT "A NUMBER FROM 1 ";
30 PRINT "TO 10 HAS BEEN ";
40 PRINT "RANDOMLY CHOSEN."
50 FOR Q = 0 TO 1 STEP 0
60     INPUT "ENTER A GUESS. "; G%
70     Q = G% = N%
80 NEXT
90 PRINT "WELL GUESSED!"
```


==={{header|IS-BASIC}}===
<lang IS-BASIC>100 PROGRAM "Guess.bas"
110 RANDOMIZE
120 LET N=RND(10)+1
130 DO
140   INPUT PROMPT "Guess a number that's between 1-10: ":G
150 LOOP UNTIL N=G
160 PRINT "Well guessed!"
```


=
## QBasic
=

```qbasic
supervisor:
GOSUB initialize
GOSUB guessing
GOTO continue

initialize:
RANDOMIZE TIMER
n = 0: r = INT(RND * 100 + 1): g = 0: c$ = ""
RETURN

guessing:
WHILE g <> r
    INPUT "Pick a number between 1 and 100"; g
    IF g = r THEN
        PRINT "You got it!"
        n = n + 1
        PRINT "It took "; n; "tries to pick the right number."
    ELSEIF g < r THEN
        PRINT "Try a larger number."
        n = n + 1
    ELSE
        PRINT "Try a smaller number."
        n = n + 1
    END IF
WEND
RETURN

continue:
WHILE c$ <> "YES" AND c$ <> "NO"
    INPUT "Do you want to continue? (YES/NO)"; c$
    c$ = UCASE$(c$)
    IF c$ = "YES" THEN
        GOTO supervisor
    ELSEIF c$ = "NO" THEN
        STOP
    END IF
WEND
```


=
## ZX Spectrum Basic
=
ZX Spectrum Basic has no [[:Category:Conditional loops|conditional loop]] constructs, so we have to emulate them here using IF and GO TO.

```zxbasic
10 LET n=INT (RND*10)+1
20 INPUT "Guess a number that's between 1 and 10: ",g
30 IF g=n THEN PRINT "That's my number!": STOP
40 PRINT "Guess again!"
50 GO TO 20
```



## BASIC256


```BASIC256
n = int(rand * 10) + 1
print "I am thinking of a number from 1 to 10"
do
   input "Guess it > ",g
   if g <> n then
      print "No luck,  Try again."
   endif
until g = n
print "Yea! You guessed my number."
```



## Batch File

At the line set /a answer=%random%%%(10-1+1)+1, if you want to change the minimum and maximum numbers, change all number ones (not counting the one that is in 10) to your desired chosen number and for the maximum, which is 10, do the same, but for maximum (eg. the minimum could be 123 and the maximum could be 321, etc.).
```dos
@echo off
set /a answer=%random%%%(10-1+1)+1
set /p guess=Pick a number between 1 and 10: 
:loop
if %guess%==%answer% (echo Well guessed!
pause) else (set /p guess=Nope, guess again: 
goto loop)
```



## BBC BASIC


```bbcbasic
      choose% = RND(10)
      REPEAT
        INPUT "Guess a number between 1 and 10: " guess%
        IF guess% = choose% THEN
          PRINT "Well guessed!"
          END
        ELSE
          PRINT "Sorry, try again"
        ENDIF
      UNTIL FALSE
```



## Befunge


{{works with|Fungus|0.28}}
{{works with|CCBI|2.1}}

```Befunge
v     RNG anthouse
>          v  ,,,,,,<
          v?v       ,
         vvvvv      ,
        v?????v     ,
       vvvvvvvvv    ,
      >?????????v   ,
      >vvvvvvvvvv<  ,
      >??????????<  ,
      >vvvvvvvvvv<  ,
      >??????????<  ,
      >vvvvvvvvvv<  ^"Well guessed!"<
      >??????????<        >"oN",,91v  actual game unit
       1234567899         ^_91+"!"  ^
                1          ^-g22<&<>+,v
                +>,,,,,,,,,,,,,,,,^
       >>>>>>>>>v^"guessthenumber!"+19<
       RNG unit > 22p                 ^
```



## Bracmat

The value is generated by the <code>clk</code> function, which returns a (probably) non-integral rational number. The <code>den</code> function retrieves the denominators of this number. The rational number, multiplied by its denominator, becomes an natural number.

```bracmat
( ( GuessTheNumber
  =   mynumber
    .   clk$:?mynumber
      & mod$(!mynumber*den$!mynumber.10)+1:?mynumber
      &   whl
        ' ( put'"Guess my number:"
          & get':~!mynumber:?K
          )
      & out'"Well guessed!"
  )
& GuessTheNumber$
);
```



## Brat


```brat
number = random 10

p "Guess a number between 1 and 10."

until {
  true? ask("Guess: ").to_i == number
    { p "Well guessed!"; true }
    { p "Guess again!" }
}
```



## C



```c>#include <stdlib.h

#include <stdio.h>
#include <time.h>

int main(void)
{
    int n;
    int g;
    char c;

    srand(time(NULL));
    n = 1 + (rand() % 10);

    puts("I'm thinking of a number between 1 and 10.");
    puts("Try to guess it:");

    while (1) {
        if (scanf("%d", &g) != 1) {
		/* ignore one char, in case user gave a non-number */
		scanf("%c", &c);
		continue;
	}

        if (g == n) {
	    puts("Correct!");
	    return 0;
	}
        puts("That's not my number. Try another guess:");
    }
}
```



## C++



```cpp>#include <iostream

#include <cstdlib>
#include <ctime>

int main()
{
    srand(time(0));
    int n = 1 + (rand() % 10);
    int g;
    std::cout << "I'm thinking of a number between 1 and 10.\nTry to guess it! ";
    while(true)
    {
        std::cin >> g;
        if (g == n)
            break;
        else
            std::cout << "That's not my number.\nTry another guess! ";
    }
    std::cout << "You've guessed my number!";
    return 0;
}


```



## C sharp


```csharp
using System;

class GuessTheNumberGame
{
    static void Main()
    {
        bool numberCorrect = false;
        Random randomNumberGenerator = new Random();
        int randomNumber = randomNumberGenerator.Next(1, 10+1);
        
        Console.WriteLine("I'm thinking of a number between 1 and 10.  Can you guess it?");
        do
        {
            Console.Write("Guess: ");
            int userGuess = int.Parse(Console.ReadLine());

            if (userGuess == randomNumber)
            {
                numberCorrect = true;
                Console.WriteLine("Congrats!!  You guessed right!");
            }
            else
                Console.WriteLine("That's not it.  Guess again.");
        } while (!numberCorrect);
    }
};
```



## Clojure


```Clojure

(def target (inc (rand-int 10))

(loop [n 0]
   (println "Guess a number between 1 and 10 until you get it right:")
   (let [guess (read)]
	(if (= guess target)
	    (printf "Correct on the %d guess.\n" n)
	    (do
	     (println "Try again")
	     (recur (inc n))))))

```



## COBOL


```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. Guess-The-Number.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  Random-Num PIC 99.
       01  Guess      PIC 99.

       PROCEDURE DIVISION.
           COMPUTE Random-Num = 1 + (FUNCTION RANDOM * 10)
           DISPLAY "Guess a number between 1 and 10:"

           PERFORM FOREVER
               ACCEPT Guess

               IF Guess = Random-Num
                   DISPLAY "Well guessed!"
                   EXIT PERFORM
               ELSE
                   DISPLAY "That isn't it. Try again."
               END-IF
           END-PERFORM
           
           GOBACK
           .
```



## CoffeeScript


```coffeescript
num = Math.ceil(Math.random() * 10)
guess = prompt "Guess the number. (1-10)"
while parseInt(guess) isnt num
  guess = prompt "YOU LOSE! Guess again. (1-10)"
alert "Well guessed!"
```


{{works_with|node.js}}


```coffeescript

# This shows how to do simple REPL-like I/O in node.js.
readline = require "readline"

do ->
  number = Math.ceil(10 * Math.random())
  interface = readline.createInterface process.stdin, process.stdout

  guess = ->
    interface.question "Guess the number between 1 and 10: ", (answer) ->
      if parseInt(answer) == number
        # These lines allow the program to terminate.
        console.log "GOT IT!"
        interface.close()
        process.stdin.destroy()
      else
        console.log "Sorry, guess again"
        guess()
      
  guess()

```



## Common Lisp


```lisp
(defun guess-the-number ()
  (let ((num-guesses 0)
	(num (1+ (random 9)))
	(guess nil))
    (format t "Try to guess a number from 1 to 10!~%")
    (loop do (format t "Guess? ")
	  (incf num-guesses)
	  (setf guess (read))
	  (cond ((not (numberp guess))
		 (format t "Your guess is  not a number!~%"))
		((not (= guess num))
		 (format t "Your guess was wrong.  Try again.~%")))
	  until (and (numberp guess)
		     (= guess num)))
    (format t "You guessed correctly on the ~:r try!~%" num-guesses)))

```

Output:

```txt
CL-USER> (guess-the-number)
Try to guess a number from 1 to 10!
Guess? a
Your guess is  not a number!
Guess? 1
Your guess was wrong.  Try again.
Guess? 2
Your guess was wrong.  Try again.
Guess? 3
You guessed correctly on the fourth try!
```



## D


```d

void main() {
    immutable num = uniform(1, 10).text;

    do write("What's next guess (1 - 9)? ");
    while (readln.strip != num);

    writeln("Yep, you guessed my ", num);
}
```

{{out}}

```txt
What's next guess (1 - 9)? 1
What's next guess (1 - 9)? 2
What's next guess (1 - 9)? 3
Yep, you guessed my 3!
```


## DCL


```DCL
$ time = f$time()
$ number = f$extract( f$length( time ) - 1, 1, time ) + 1
$ loop:
$  inquire guess "enter a guess (integer 1-10) "
$  if guess .nes. number then $ goto loop
$ write sys$output "Well guessed!"
```

{{out}}

```txt
$ @guess_the_number
enter a guess (integer 1-10) : 5
enter a guess (integer 1-10) : 1
enter a guess (integer 1-10) : 2
enter a guess (integer 1-10) : 3
enter a guess (integer 1-10) : 4
Well guessed!
```


## Dart

{{Trans|Kotlin}}

```dart
import 'dart:math';
import 'dart:io';

main() {
  final n = (1 + new Random().nextInt(10)).toString();
  print("Guess which number I've chosen in the range 1 to 10");
  do { stdout.write(" Your guess : "); } while (n != stdin.readLineSync());
  print("\nWell guessed!");
}
```



## Delphi


```Delphi
program GuessTheNumber;

{$APPTYPE CONSOLE}

uses SysUtils;

var
  theDigit : String ;
  theAnswer : String ;

begin
  Randomize ;
  theDigit := IntToStr(Random(9)+1) ;
  while ( theAnswer <> theDigit ) do Begin
    Writeln('Please enter a digit between 1 and 10' ) ;
    Readln(theAnswer);
  End ;
  Writeln('Congratulations' ) ;
end.

```


=={{header|Déjà Vu}}==

```dejavu
local :number random-range 1 11

while true:
	if = number to-num !prompt "Guess my number: ":
		!print "Congratulations, you've guessed it!"
		return
	else:
		!print "Nope, try again."
```



## EasyLang

<lang>n = random 10 + 1
write "Guess a number between 1 and 10: "
repeat
  g = number input
  until g = n
  print " is wrong"
  write "try again: "
.
print " is correct. Well guessed!"
```



## Eiffel


```Eiffel

class
	APPLICATION

create
	make

feature {NONE} -- Initialization

	make
		local
			number_to_guess: INTEGER
		do
			number_to_guess := (create {RANDOMIZER}).random_integer_in_range (1 |..| 10)
			from
				print ("Please guess the number!%N")
				io.read_integer
			until
				io.last_integer = number_to_guess
			loop
				print ("Please, guess again!%N")
				io.read_integer
			end
			print ("Well guessed!%N")
		end

end

```

The code above is simplified if we create a RANDOMIZER, which simplifies reuse (e.g. the code in RANDOMIZER does not have to be recreated for each need of a random number).

```Eiffel

class
	RANDOMIZER

inherit
	ANY
		redefine
			default_create
		end

feature {NONE} -- Initialization

	default_create
			-- <Precursor>
		local
			time: TIME
		do
			sequence.do_nothing
		end

feature -- Access

	random_integer_in_range (a_range: INTEGER_INTERVAL): INTEGER
		do
			Result := (sequence.double_i_th (1) * a_range.upper).truncated_to_integer + a_range.lower
		end

feature {NONE} -- Implementation

	sequence: RANDOM
		local
			seed: INTEGER_32
			time: TIME
		once
			create time.make_now
			seed := time.hour *
					(60 + time.minute) *
					(60 + time.second) *
					(1000 + time.milli_second)
			create Result.set_seed (seed)
		end

end

```

{{out}}

```txt

Please guess the number!
10

Please, guess again!
9

Please, guess again!
8

Correct!

```


## Elena

ELENA 4.x :

```elena
import extensions;
 
public program()
{
    int randomNumber := randomGenerator.eval(1,10);
    console.printLine("I'm thinking of a number between 1 and 10.  Can you guess it?");
    bool numberCorrect := false;
    until(numberCorrect)
    {
        console.print("Guess: ");
        int userGuess := console.readLine().toInt();
        if (randomNumber == userGuess)
        {
            numberCorrect := true;
            console.printLine("Congrats!!  You guessed right!")
        }
        else
        {
            console.printLine("That's not it.  Guess again.")
        }
    }
}
```

{{out}}

```txt

I'm thinking of a number between 1 and 10.  Can you guess it?
Guess: 2
That's not it.  Guess again.
Guess: 5
That's not it.  Guess again.
Guess: 6
Congrats!!  You guessed right!

```



## Elixir

{{works with|Elixir|1.2}}

```elixir
defmodule GuessingGame do
  def play do
    play(Enum.random(1..10))
  end
  
  defp play(number) do
    guess = Integer.parse(IO.gets "Guess a number (1-10): ")
    case guess do
      {^number, _} ->
        IO.puts "Well guessed!"
      {n, _} when n in 1..10 ->
        IO.puts "That's not it."
        play(number)
      _ ->
        IO.puts "Guess not in valid range."
        play(number)
    end
  end
end
 
GuessingGame.play
```



## Emacs Lisp


```Lisp

(let ((num (1+ (random 10))))
   (princ "Guess the no") 
  (loop 
   (setq guess (read)) 
     (if (eq guess num) 
	 (progn(princ-list "Guess was right! " num) (return)) (print "Wrong, try again.") ) ) )
```




## Erlang


```erlang
% Implemented by Arjun Sunel
-module(guess_the_number).
-export([main/0]).

main() ->
	io:format("Guess my number between 1 and 10 until you get it right:\n"),
	N = random:uniform(10),
	guess(N).

guess(N) ->
	{ok, [K]} = io:fread("Guess number :  ","~d"),
	
	if 
		K=:=N ->
			io:format("Well guessed!!\n");
		true -> 
			guess(N)	
	end.	

```



## ERRE


```ERRE

PROGRAM GUESS_NUMBER

!
! for rosettacode.org
!

BEGIN

  RANDOMIZE(TIMER)
  N=0
  R=INT(RND(1)*100+1)  ! RND function gives a random number from 0 to 1
  G=0
  C$=""

  WHILE G<>R DO
    INPUT("Pick a number between 1 and 100";G)
    IF G=R THEN
        PRINT("You got it!")
        N+=1
        PRINT("It took";N;"tries to pick the right Number.")
    ELSIF G<R THEN
        PRINT("Try a larger number.")
        N+=1
    ELSE
        PRINT("Try a smaller number.")
        N+=1
    END IF
  END WHILE
END PROGRAM

```

Note: Adapted from Qbasic version.


## Euphoria

{{trans|ZX_Spectrum_Basic}}

```Euphoria
include get.e

integer n,g
n = rand(10)

puts(1,"I have thought of a number from 1 to 10.\n")
puts(1,"Try to guess it!\n")

while 1 do
    g = prompt_number("Enter your guess: ",{1,10})
    if n = g then
        exit
    end if
    puts(1,"Your guess was wrong. Try again!\n")
end while

puts(1,"Well done! You guessed it.")
```



## Factor



```factor

USING: io random math math.parser kernel formatting ;
IN: guess-the-number

<PRIVATE

: gen-number ( -- n )
  10 random 1 + ;

: make-guess ( n -- n ? )
  dup readln string>number = ;

: play-game ( n -- n )
  [ make-guess ]
  [ "Guess a number between 1 and 10:" print flush ] do until ;

PRIVATE>

: guess-the-number ( -- )
  gen-number play-game
  "Yes, the number was %d!\n" printf ;

```



## Fantom



```fantom

class Main
{
  public static Void main ()
  {
    Str target := (1..10).random.toStr
    Str guess := ""
    while (guess != target)
    {
      echo ("Enter a guess: ")
      guess = Env.cur.in.readLine
      if (guess.trim == target) // 'trim' to remove any spaces/newline
      {
        echo ("Well guessed!")
        break
      }
      else
        echo ("Failed - try again")
    }
  }
}

```



## Forth


```Forth

\ tested with GForth  0.7.0
: RND    ( -- n) TIME&DATE 2DROP 2DROP DROP 10 MOD ;         \ crude random number
: ASK    ( -- ) CR ." Guess a number between 1 and 10? " ;
: GUESS  ( -- n)  PAD DUP 4 ACCEPT EVALUATE ;
: REPLY  ( n n' -- n)  2DUP <> IF CR ." No, it's not " DUP . THEN ;

: GAME ( -- )
          RND
          BEGIN   ASK GUESS REPLY  OVER =  UNTIL
          CR ." Yes it was " .
          CR ." Good guess!"  ;

```



## Fortran



```fortran
program guess_the_number
 implicit none

 integer                          :: guess
 real                             :: r
 integer                          :: i, clock, count, n
 integer,dimension(:),allocatable :: seed

 real,parameter :: rmax = 10	

!initialize random number generator:
 call random_seed(size=n)
 allocate(seed(n))
 call system_clock(count)
 seed = count
 call random_seed(put=seed)
 deallocate(seed)

!pick a random number between 1 and rmax:
 call random_number(r)          !r between 0.0 and 1.0
 i = int((rmax-1.0)*r + 1.0)    !i between 1 and rmax

!get user guess:
 write(*,'(A)') 'I''m thinking of a number between 1 and 10.'
 do   !loop until guess is correct
	write(*,'(A)',advance='NO') 'Enter Guess: '
	read(*,'(I5)') guess
	if (guess==i) exit
	write(*,*) 'Sorry, try again.'
 end do

 write(*,*) 'You''ve guessed my number!'

end program guess_the_number

```



## FreeBASIC


```freebasic
' FB 1.05.0 Win64

Randomize
Dim n As Integer = Int(Rnd * 10) + 1
Dim guess As Integer
Print "Guess which number I've chosen in the range 1 to 10"
Print
Do
  Input " Your guess : "; guess
  If n = guess Then
    Print "Well guessed!"
    End
  End If
Loop
```


{{out}}
Sample input/output :

```txt

Guess which number I've chosen in the range 1 to 10

 Your guess : ? 3
 Your guess : ? 6
 Your guess : ? 7
 Your guess : ? 9
 Your guess : ? 2
 Your guess : ? 4
Well guessed!

```



## Gambas


```gambas
Public Sub Form_Open()
Dim byGuess, byGos As Byte
Dim byNo As Byte = Rand(1, 10)
Dim sHead As String = "Guess the number"

Repeat
  Inc byGos
  byGuess = InputBox("Guess the number between 1 and 10", sHead)
  sHead = "Sorry, have another go"
Until byGuess = byNo

Message.Info("Well guessed! You took " & Str(byGos) & " gos to guess the number was " & Str(byNo), "OK")
Me.Close

End
```



## GML


```GML
var n, g;
n = irandom_range(1,10);
show_message("I'm thinking of a number from 1 to 10");
g = get_integer("Please enter guess", 1);
while(g != n)
    {
    g = get_integer("I'm sorry "+g+" is not my number, try again. Please enter guess", 1);
    }
show_message("Well guessed!");
```



## Go


```go
package main

import (
    "fmt"
    "math/rand"
    "time"
)

func main() {
    fmt.Print("Guess number from 1 to 10: ")
    rand.Seed(time.Now().Unix())
    n := rand.Intn(10) + 1
    for guess := n; ; fmt.Print("No. Try again: ") {
        switch _, err := fmt.Scan(&guess); {
        case err != nil:
            fmt.Println("\n", err, "\nSo, bye.")
            return
        case guess == n:
            fmt.Println("Well guessed!")
            return
        }
    }
}
```



## Groovy


```groovy

def random = new Random()
def keyboard = new Scanner(System.in)
def number = random.nextInt(10) + 1
println "Guess the number which is between 1 and 10: "
def guess = keyboard.nextInt()
while (number != guess) {
    println "Guess again: "
    guess = keyboard.nextInt()
}
println "Hurray! You guessed correctly!"

```


=={{header|GW-BASIC}}==

```qbasic
10 RANDOMIZE TIMER:N=INT(RND*10+1):G=0
20 PRINT "Guess the number between 1 and 10."
30 WHILE N<>G
40 INPUT "Your guess? ",G
50 WEND
60 PRINT "That's correct!"
```



## Haskell


```haskell

import Control.Monad
import System.Random

-- Repeat the action until the predicate is true.
until_ act pred = act >>= pred >>= flip unless (until_ act pred)

answerIs ans guess 
    | ans == guess = putStrLn "You got it!" >> return True
    | otherwise = putStrLn "Nope. Guess again." >> return False

ask = liftM read getLine

main = do
  ans <- randomRIO (1,10) :: IO Int
  putStrLn "Try to guess my secret number between 1 and 10."
  ask `until_` answerIs ans

```


Simple version:


```haskell

import System.Random

main = randomRIO (1,10) >>= gameloop

gameloop :: Int -> IO ()
gameloop r = do	
	i <- fmap read getLine
	if i == r 
	  then putStrLn "You got it!"
	  else putStrLn "Nope. Guess again." >> gameloop r

```



## HolyC



```holyc
U8 n, *g;

n = 1 + RandU16 % 10;

Print("I'm thinking of a number between 1 and 10.\n");
Print("Try to guess it:\n");

while(1) {
  g = GetStr;

  if (Str2I64(g) == n) {
    Print("Correct!\n");
    break;
  }

  Print("That's not my number. Try another guess:\n");
}
```


=={{header|Icon}} and {{header|Unicon}}==

This solution works in both languages.


```unicon
procedure main()
    n := ?10
    repeat {
        writes("Pick a number from 1 through 10: ")
        if n = numeric(read()) then break
        }
    write("Well guessed!")
end
```



## J


```j
require 'misc'
game=: verb define
  n=: 1 + ?10
  smoutput 'Guess my integer, which is bounded by 1 and 10'
  whilst. -. guess -: n do.
    guess=. {. 0 ". prompt 'Guess: '
    if. 0 -: guess do. 'Giving up.' return. end. 
    smoutput (guess=n){::'no.';'Well guessed!'
  end.
)
```


Example session:

<lang>   game''
Guess my integer, which is bounded by 1 and 10
Guess: 1
no.
Guess: 2
Well guessed!
```



## Java

{{works with|Java|6+}}

```java5
public class Guessing {
    public static void main(String[] args) throws NumberFormatException{
        int n = (int)(Math.random() * 10 + 1);
        System.out.print("Guess the number between 1 and 10: ");
        while(Integer.parseInt(System.console().readLine()) != n){
            System.out.print("Wrong! Guess again: ");
        }
        System.out.println("Well guessed!");
    }
}
```

For pre-Java 6, use a <code>Scanner</code> or <code>BufferedReader</code> for input instead of <code>System.console()</code> (see [[Input loop#Java]]).


## JavaScript


```javascript

function guessNumber() {
  // Get a random integer from 1 to 10 inclusive
  var num = Math.ceil(Math.random() * 10);
  var guess;

  while (guess != num) {
    guess = prompt('Guess the number between 1 and 10 inclusive');
  }
  alert('Congratulations!\nThe number was ' + num);
}

guessNumber();
```


Requires a host environment that supports <code>prompt</code> and <code>alert</code> such as a browser.


## jq

{{works with|jq|1.5}}

jq currently does not have a built-in random number generator, so a suitable PRNG for this task is defined below. Once `rand(n)` has been defined, the task can be accomplished as follows: 

```jq

{prompt: "Please enter your guess:", random: (1+rand(9)) }
| ( (while( .guess != .random; .guess = (input|tonumber) ) | .prompt),
  "Well done!"
```


'''Invocation'''

With the program as given here in a file named program.jq, a seed must be specified on the command line as illustrated on the following line:

    $ jq -nr -f program.jq --arg seed 17

'''PRNG'''

```jq
# LCG::Microsoft generates 15-bit integers using the same formula
# as rand() from the Microsoft C Runtime.
# Input: [ count, state, random ]
def next_rand_Microsoft:
  .[0] as $count
  | ((214013 * .[1]) + 2531011) % 2147483648 # mod 2^31
  | [$count+1 , ., (. / 65536 | floor) ];

def rand_Microsoft(seed):
  [0,seed]
  | next_rand_Microsoft  # the seed is not so random
  | next_rand_Microsoft | .[2]; 

# A random integer in [0 ... (n-1)]:
def rand(n): n * (rand_Microsoft($seed|tonumber) / 32768) | trunc;
```



## Jsish

From Javascript entry.

```javascript
function guessNumber() {
    // Get a random integer from 1 to 10 inclusive
    var num = Math.ceil(Math.random() * 10);
    var guess;
    var tries = 0;
 
    while (guess != num) {
        tries += 1;
        printf('%s', 'Guess the number between 1 and 10 inclusive: ');
        guess = console.input();
    }
    printf('Congratulations!\nThe number was %d   it took %d tries\n', num, tries);
}

if (Interp.conf('unitTest')) {
    // Set a predictable outcome
    Math.srand(0);
    guessNumber();
}

/*
=!INPUTSTART!=
5
9
2
=!INPUTEND!=
*/

/*
=!EXPECTSTART!=
Guess the number between 1 and 10 inclusive: Guess the number between 1 and 10 inclusive: Guess the number between 1 and 10 inclusive: Congratulations!
The number was 2   it took 3 tries
=!EXPECTEND!=
*/
```


{{out}}

```txt
prompt$ jsish -u guessNumber.jsi
[PASS] guessNumber.jsi
```



## Julia

{{works with|Julia|0.6}}


```julia
function guess()
    number = dec(rand(1:10))
    print("Guess my number! ")
    while readline() != number
        print("Nope, try again... ")
    end
    println("Well guessed!")
end

guess()
```


{{out}}

```txt
Guess my number! 1
Nope, try again... 2
Nope, try again... 3
Nope, try again... 4
Nope, try again... 5
Nope, try again... 6
Nope, try again... 7
Nope, try again... 8
Nope, try again... 9
Nope, try again... 10
Well guessed!
```



## Kotlin


```scala
// version 1.0.5-2

fun main(args: Array<String>) {
	val n = (1 + java.util.Random().nextInt(10)).toString()
	println("Guess which number I've chosen in the range 1 to 10\n")
	do { print(" Your guess : ") } while (n != readLine())
	println("\nWell guessed!")
}
```

Sample input/output:
{{out}}

```txt

Guess which number I've chosen in the range 1 to 10

 Your guess : 5
 Your guess : 8

Well guessed!

```



## LabVIEW

{{VI solution|LabVIEW_Guess_the_number.png}}


## Langur


```Langur
writeln "Guess a number from 1 to 10"

val .n = random(10)
for {
    val .guess = read ">> ", RE/^0*(?:[1-9]|10)(?:\.0+)?$/, "bad data\N", 7, "nay"
    if .guess == "nay" {
        writeln "too much bad data"
        break
    }
    if toNumber(.guess) == .n {
        writeln "That's it."
        break
    }
    writeln("not it")
}
```


{{out}}

```txt
Guess a number from 1 to 10
>> 0.1
bad data
>> 001.0
not it
>> 789
bad data
>> 2
not it
>> 3
That's it.
```



## Lasso


### Command Line

The following example works when Lasso is called from a command line

```Lasso
local(
	number	= integer_random(10, 1),
	status	= false,
	guess
)

// prompt for a number
stdout('Guess a number between 1 and 10: ')

while(not #status) => {
	#guess = null

	// the following bits wait until the terminal gives you back a line of input
	while(not #guess or #guess -> size == 0) => {
		#guess = file_stdin -> readSomeBytes(1024, 1000)
	}
	#guess = integer(#guess)

	if(not (range(#guess, 1, 10) == #guess)) => {
		stdout('Input not of correct type or range. Guess a number between 1 and 10: ')
	else(#guess == #number)
		stdout('Well guessed!')
		#status = true
	else
		stdout('You guessed wrong number. Guess a number between 1 and 10: ')
	}

}
```



### Web form

The following example is a web page form

```Lasso
<?LassoScript

local(
	number	= integer(web_request -> param('number') or integer_random(10, 1)),
	status	= false,
	guess	= web_request -> param('guess'),
	_guess	= integer(#guess),
	message	= 'Guess a number between 1 and 10'
)



if(#guess) => {
	if(not (range(#_guess, 1, 10) == #_guess)) => {
		#Message = 'Input not of correct type or range. Guess a number between 1 and 10'
	else(#_guess == #number)
		#Message = 'Well guessed!'
		#status = true
	else
		#Message = 'You guessed wrong number. Guess a number between 1 and 10'
	}
}


?><!DOCTYPE html>
<html lang="en">
	<head>
		<title>Guess the number - Rosetta Code</title>
	</head>
	<body>
		<h3>[#message]</h3>
[if(not #status)]
		<form method="post">
			<label for="guess">Guess:</label><br/ >
			<input type="number" name="guess" />
			<input name="number" type="hidden" value="[#number]" />
			<input name="submit" type="submit" />
		</form>
[/if]
	</body>
</html>
```



## LFE



```lisp

(defmodule guessing-game
  (export (main 0)))

(defun get-player-guess ()
  (let (((tuple 'ok (list guessed)) (: io fread '"Guess number: " '"~d")))
    guessed))

(defun check-guess (answer guessed)
    (cond
      ((== answer guessed)
        (: io format '"Well-guessed!!~n"))
      ((/= answer guessed)
        (check-guess answer (get-player-guess)))))

(defun main ()
  (: io format '"Guess the number I have chosen, between 1 and 10.~n")
  (check-guess
    (: random uniform 10)
    (get-player-guess)))

```


From the LFE REPL (assuming the above code was saved in the file "guessing-game.lfe"):

```lisp

> (slurp '"guessing-game.lfe")
#(ok guessing-game)
> (main)
Guess the number I have chosen, between 1 and 10.
Guess number: 10
Guess number: 5
Well-guessed!!
ok

```



## Liberty BASIC


```lb
number = int(rnd(0) * 10) + 1
input "Guess the number I'm thinking of between 1 and 10. "; guess
while guess <> number
    input "Incorrect! Try again! "; guess
wend
print "Congratulations, well guessed! The number was "; number;"."
```




## LiveCode


```LiveCode
command guessTheNumber
    local tNumber, tguess
    put random(10) into tNumber
    repeat until tguess is tNumber
        ask question "Please enter a number between 1 and 10" titled "Guess the number"
        if it is not empty then
            put it into tguess
            if tguess is tNumber then 
                answer "Well guessed!"
            end if
        else
            exit repeat
        end if
    end repeat
end guessTheNumber
```



## Locomotive Basic



```locobasic
10 RANDOMIZE TIME:num=INT(RND*10+1):guess=0
20 PRINT "Guess the number between 1 and 10."
30 WHILE num<>guess
40 INPUT "Your guess? ", guess
50 WEND
60 PRINT "That's correct!"
```



## LOLCODE

There is no native support for random numbers. This solution uses a simple linear congruential generator to simulate them, with the lamentable restriction that the user must first be prompted for a seed.

```LOLCODE
HAI 1.3

VISIBLE "SEED ME, FEMUR! "!
I HAS A seed, GIMMEH seed

HOW IZ I randomizin
    seed R MOD OF SUM OF 1 AN PRODUKT OF 69069 AN seed AN 10
IF U SAY SO

I IZ randomizin MKAY
I HAS A answer ITZ SUM OF seed AN 1
I HAS A guess

IM IN YR guesser
    VISIBLE "WUTS MY NUMBR? "!
    GIMMEH guess, guess IS NOW A NUMBR

    BOTH SAEM guess AN answer, O RLY?
        YA RLY, VISIBLE "U WIN!", GTFO
    OIC
IM OUTTA YR guesser

KTHXBYE
```



## Lua


```lua
math.randomseed( os.time() )
n = math.random( 1, 10 )

print( "I'm thinking of a number between 1 and 10. Try to guess it: " )

repeat
    x = tonumber( io.read() )

    if x == n then
	print "Well guessed!"
    else
	print "Guess again: "
    end
until x == n
```



## M2000 Interpreter

A copy from QBASIC, write blocks  { } where needed, We use GOSUB and GOTO in a Module.


```M2000 Interpreter

Module QBASIC_Based {
      supervisor:
      GOSUB initialize
      GOSUB guessing
      GOTO continue
       
      initialize:
      \\ Not need to RANDOMIZE TIMER
      \\ we can use Random(1, 100) to get a number from 1 to 100
      n = 0: r = INT(RND * 100 + 1): g = 0: c$ = ""
      RETURN
       
      guessing:
      WHILE g <> r {
                INPUT "Pick a number between 1 and 100:"; g
                IF g = r THEN {
                    PRINT "You got it!"
                    n ++
                    PRINT "It took "; n; " tries to pick the right number."
                } ELSE.IF g < r THEN {
                    PRINT "Try a larger number."
                    n ++
                } ELSE {
                    PRINT "Try a smaller number."
                    n++
                }
      }
      RETURN
       
      continue:
      WHILE c$ <> "YES" AND c$ <> "NO" {
          INPUT "Do you want to continue? (YES/NO)"; c$
          c$ = UCASE$(c$)
          IF c$ = "YES" THEN {
              GOTO supervisor
          } ELSE.IF c$ = "NO" THEN {
              Goto End
          }
      }
      End:     
}
QBASIC_Based

```



## Maple



```Maple
GuessNumber := proc()
    local number;
    randomize():
    printf("Guess a number between 1 and 10 until you get it right:\n:");
    number := rand(1..10)();
    while parse(readline()) <> number do
        printf("Try again!\n:");
    end do:
    printf("Well guessed! The answer was %d.\n", number);
end proc:
```


```Maple
GuessNumber();
```


=={{header|Mathematica}} / {{header|Wolfram Language}}==

```Mathematica
number = RandomInteger[{1, 10}];
While[guess =!= number, guess = Input["Guess my number"]];
Print["Well guessed!"]
```



## MATLAB


```matlab
number = ceil(10*rand(1));
[guess, status] = str2num(input('Guess a number between 1 and 10: ','s'));

while (~status || guess ~= number)
    [guess, status] = str2num(input('Guess again: ','s'));
end
disp('Well guessed!')
```



## MAXScript


```MAXScript

rand = random 1 10
clearListener()
while true do
(
	userval = getKBValue prompt:"Enter an integer between 1 and 10: "
	if userval == rand do (format "\nWell guessed!\n"; exit)
	format "\nChoose another value\n"
)

```



## Mercury

Mercury does have a 'time' module in its standard library, but it offers an abstract type instead of the seconds-since-the-epoch we want to seed the RNG with.  So this is also an example of how easy it is to call out to C.  (It's just as easy to call out to C#, Java, and Erlang.)  Also, rather than parse the input, this solution prepares the random number to match the typed input.  This isn't a weakness of Mercury, just the author's desire to cheat a bit.


```Mercury
:- module guess.
:- interface.
:- import_module io.
:- pred main(io::di, io::uo) is det.
:- implementation.
:- import_module random, string.

main(!IO) :-
        time(Time, !IO),
        random.init(Time, Rand),
        random.random(1, 10, N, Rand, _),
        main(from_int(N) ++ "\n", !IO).

:- pred main(string::in, io::di, io::uo) is det.
main(N, !IO) :-
        io.write_string("Guess the number: ", !IO),
        io.read_line_as_string(Res, !IO),
        (
                Res = ok(S),
                ( if S = N then io.write_string("Well guessed!\n", !IO)
                  else main(N, !IO) )
        ;
                Res = error(E)
        ;
                Res = eof
        ).

:- pred time(int::out, io::di, io::uo) is det.

:- pragma foreign_decl("C", "#include <time.h>").
:- pragma foreign_proc("C", time(Int::out, _IO0::di, _IO1::uo),
                       [will_not_call_mercury, promise_pure],
                       "Int = time(NULL);").
```



## MiniScript


```MiniScript
num = ceil(rnd*10)
while true
    x = val(input("Your guess?"))
    if x == num then
        print "Well guessed!"
        break
    end if
end while
```



## MIPS Assembly


```mips

# WRITTEN: August 26, 2016 (at midnight...)

# This targets MARS implementation and may not work on other implementations
# Specifically, using MARS' random syscall
.data
	take_a_guess: .asciiz "Make a guess:"
	good_job: .asciiz "Well guessed!"

.text
	#retrieve system time as a seed
	li $v0,30
	syscall
	
	#use the high order time stored in $a1 as the seed arg
	move $a1,$a0

	#set the seed
	li $v0,40
	syscall
	
	#generate number 0-9 (random int syscall generates a number where):
	# 0 <= $v0 <= $a1
	li $a1,10
	li $v0,42
	syscall
	
	#increment the randomly generated number and store in $v1
	add $v1,$a0,1
	
loop:	jal print_take_a_guess
	jal read_int
	
	#go back to beginning of loop if user hasn't guessed right,
	#    else, just "fall through" to exit_procedure
	bne $v0,$v1,loop
	
exit_procedure:
	#set syscall to print_string, then set good_job string as arg
	li $v0,4
	la $a0,good_job
	syscall
	
	#exit program
	li $v0,10
	syscall
	
print_take_a_guess:
	li $v0,4
	la $a0,take_a_guess
	syscall
	jr $ra
	
read_int:
	li $v0,5
	syscall
	jr $ra

```



## Nemerle


```Nemerle
using System;
using System.Console;

module Guess
{
    Main() : void
    {
        def rand = Random();
        def x = rand.Next(1, 11);  // returns 1 <= x < 11
        mutable guess = 0;
    
        do
        {
            WriteLine("Guess a nnumber between 1 and 10:");
            guess = Int32.Parse(ReadLine());
        } while (guess != x);
    
        WriteLine("Well guessed!");   
    }
}
```



## NetRexx


```NetRexx
/* NetRexx */

options replace format comments java crossref savelog symbols nobinary

guessThis = (Math.random * 10 + 1) % 1
guess = -1
prompt = [ -
  'Try guessing a number between 1 and 10', -
  'Wrong; try again...' -
  ]
promptIdx = int 0

loop label g_ until guess = guessThis
  say prompt[promptIdx]
  promptIdx = 1
  parse ask guess .
  if guess = guessThis then do
    say 'Well guessed!' guess 'is the correct number.'
    leave g_
    end
  end g_

return

```



## NewLISP


```NewLISP
; guess-number.lsp
; oofoe 2012-01-19
; http://rosettacode.org/wiki/Guess_the_number

(seed (time-of-day)) ; Initialize random number generator from clock.
(setq number (+ 1 (rand 10)))

(println "I'm thinking of a number between 1 and 10. Can you guess it?")
(print   "Type in your guess and hit [enter]: ")
(while (!= number (int (read-line))) (print "Nope! Try again: "))
(println "Well guessed! Congratulations!")

(exit)
```


Sample output:


```txt
I'm thinking of a number between 1 and 10. Can you guess it?
Type in your guess and hit [enter]: 5
Nope! Try again: 3
Nope! Try again: 10
Nope! Try again: 1
Nope! Try again: 4
Well guessed! Congratulations!
```



## Nim


```nim

import strutils, math

randomize()
var chosen = 1 + random(10)
echo "I have thought of a number. Try to guess it!"

var guess = parseInt(readLine(stdin))

while guess != chosen:
  echo "Your guess was wrong. Try again!"
  guess = parseInt(readLine(stdin))

echo "Well guessed!"

```


=={{header|NS-HUBASIC}}==
<lang NS-HUBASIC>10 NUMBER=RND(10)+1
20 INPUT "I'M THINKING OF A NUMBER BETWEEN 1 AND 10. WHAT IS IT? ",GUESS
30 IF GUESS<>NUMBER THEN PRINT "INCORRECT GUESS. TRY AGAIN.": GOTO 20
40 PRINT "CORRECT NUMBER."
```


=={{header|Oberon-2}}==
Works with oo2c Version 2

```oberon2

MODULE GuessTheNumber;
IMPORT 
  RandomNumbers,
  In,
  Out;

  PROCEDURE Do;
  VAR
    n,guess: LONGINT;
  BEGIN
    n := RandomNumbers.RND(10);
    Out.String("Guess a number between 1 and 10: ");Out.Flush();
    LOOP
      In.LongInt(guess);  
      IF  guess = n THEN 
        Out.String("You guessed!!"); Out.Ln; EXIT
      END;
      Out.String(" Sorry, try again: ");Out.Flush()
    END
  END Do;

BEGIN
  Do;
END GuessTheNumber.

```



## Objeck


```objeck

use IO;

bundle Default {
  class GuessNumber {
    function : Main(args : String[]) ~ Nil {
      done := false;
      "Guess the number which is between 1 and 10 or 'q' to quite: "->PrintLine();
      rand_num := (Float->Random() * 10.0)->As(Int) + 1;
      while(done = false) {
        guess := Console->ReadString();
        number := guess->ToInt();
        if(number <> 0) {
          if(number <> rand_num) {
            "Guess again: "->PrintLine();
          }
          else {
            "Hurray! You guessed correctly!"->PrintLine();
            done := true;
          };
        }
        else {
          if(guess->StartsWith("q") | guess->StartsWith("Q")) {
            done := true;
          };
        };
      };
    }
  }
}

```


=={{header|Objective-C}}==


```objc

#import <Foundation/Foundation.h>

int main(int argc, const char * argv[])
{

    @autoreleasepool {
        
        NSLog(@"I'm thinking of a number between 1 - 10. Can you guess what it is?\n");
        
        int rndNumber = arc4random_uniform(10) + 1;
        
        // Debug (Show rndNumber in console)
        //NSLog(@"Random number is %i", rndNumber);
        
        int userInput;
        
        do {
           
            NSLog(@"Input the number below\n");
            scanf("%i", &userInput);
          
            if (userInput > 10) {
              
                NSLog(@"Please enter a number less than 10\n");
            }
            
            if (userInput > 10 || userInput != rndNumber) {
                
                NSLog(@"Your guess %i is incorrect, please try again", userInput);
                
            } else {
                
                NSLog(@"Your guess %i is correct!", userInput);
            }
        
        } while (userInput > 10 || userInput != rndNumber);
    }
    return 0;
}

```



## OCaml


```ocaml
#!/usr/bin/env ocaml

let () =
  Random.self_init();
  let n =
    if Random.bool () then
      let n = 2 + Random.int 8 in
      print_endline "Please guess a number between 1 and 10 excluded";
      (n)
    else
      let n = 1 + Random.int 10 in
      print_endline "Please guess a number between 1 and 10 included";
      (n)
  in
  while read_int () <> n do
    print_endline "The guess was wrong! Please try again!"
  done;
  print_endline "Well guessed!"
```



## Oforth



```Oforth
import: console

: guess
   10 rand doWhile: [ "Guess :" . System.Console askln asInteger over <> ]
   drop "Well guessed!" . ;
```



## Ol


```ol

(import (otus random!))

(define number (+ 1 (rand! 10)))
(let loop ()
   (display "Pick a number from 1 through 10: ")
   (if (eq? (read) number)
      (print "Well guessed!")
      (loop)))

```



## PARI/GP


```parigp
guess()=my(r=random(10)+1);while(input()!=r,); "Well guessed!";
```



## Pascal


```pascal
Program GuessTheNumber(input, output);

var
  number, guess: integer;

begin
  randomize;
  number := random(10) + 1;
  writeln ('I''m thinking of a number between 1 and 10, which you should guess.');
  write   ('Enter your guess: ');
  readln  (guess);
  while guess <> number do
  begin
    writeln ('Sorry, but your guess is wrong. Please try again.');
    write   ('Enter your new guess: ');
    readln  (guess);
  end;
  writeln ('You made an excellent guess. Thank you and have a nice day.');
end.

```



## Perl


```perl
my $number = 1 + int rand 10;
do { print "Guess a number between 1 and 10: " } until <> == $number;
print "You got it!\n";
```



## Perl 6


```perl6
my $number = (1..10).pick;
repeat {} until prompt("Guess a number: ") == $number;
say "Guessed right!";
```



## Phix


```Phix
integer secret = rand(10)
puts(1,"Guess the number between 1 and 10: ")
while 1 do
    if prompt_number("",{1,10})=secret then exit end if
    puts(1,"Your guess was wrong.\nTry again: ")
end while
puts(1,"You got it!\n")
```



## PHP


```php

<?php

session_start();

if(isset($_SESSION['number']))
{
   $number = $_SESSION['number'];
}
else
{
   $_SESSION['number'] = rand(1,10);
}


if(isset($_POST["guess"])){
	if($_POST["guess"]){
	    $guess  = htmlspecialchars($_POST['guess']);
	 
		echo $guess . "<br />";
	    if ($guess != $number)
		{ 
	        echo "Your guess is not correct";
	    }
		elseif($guess == $number)
		{
	        echo "You got the correct number!";
	    }
	} 
}
?>

<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<html xmlns="http://www.w3.org/1999/xhtml">
	<head>
		<meta http-equiv="Content-Type" content="text/html; charset=UTF-8" />
		<title>Guess A Number</title>
	</head>

	<body>
		<form action="<?=$_SERVER['PHP_SELF'] ?>" method="post" name="guess-a-number">
		    <label for="guess">Guess number:</label><br/ >
		    <input type="text" name="guess" />
		    <input name="number" type="hidden" value="<?= $number ?>" />
		    <input name="submit" type="submit" />
		</form>
	</body>
</html>

```



## PicoLisp


```PicoLisp
(de guessTheNumber ()
   (let Number (rand 1 9)
      (loop
         (prin "Guess the number: ")
         (T (= Number (read))
            (prinl "Well guessed!") )
         (prinl "Sorry, this was wrong") ) ) )
```



## PlainTeX

This code should be compiled with etex features in console mode (for example "pdftex <name of the file>"):

```tex
\newlinechar`\^^J
\edef\tagetnumber{\number\numexpr1+\pdfuniformdeviate9}%
\message{^^JI'm thinking of a number between 1 and 10, try to guess it!}%
\newif\ifnotguessed
\notguessedtrue
\loop
	\message{^^J^^JYour try: }\read -1 to \useranswer
	\ifnum\useranswer=\tagetnumber\relax
		\message{You win!^^J}\notguessedfalse
	\else
		\message{No, it's another number, try again...}%
	\fi
	\ifnotguessed
\repeat
\bye
```



## PowerShell

Provides a function that analyzes the provided number by its call. The second script block is important and needed inside the script so the function will be called.

```PowerShell
Function GuessNumber($Guess)
{
    $Number = Get-Random -min 1 -max 11
    Write-Host "What number between 1 and 10 am I thinking of?"
        Do
        {
        Write-Warning "Try again!"
        $Guess = Read-Host "What's the number?"
        }
        While ($Number -ne $Guess)
    Write-Host "Well done! You successfully guessed the number $Guess."
}
```


```powershell
$myNumber = Read-Host "What's the number?"
GuessNumber $myNumber
```



## ProDOS

Uses math module:

```ProDOS
:a
editvar /modify /value=-random-= <10
editvar /newvar /value=-random- /title=a
editvar /newvar /value=b /userinput=1 /title=Guess a number:
if -b- /hasvalue=-a- printline You guessed correctly! else printline Your guess was wrong & goto :a
```



## Prolog

{{works with|SWI-Prolog|6}}


```prolog
main :-
    random_between(1, 10, N),
    repeat,
    prompt1('Guess the number: '),
    read(N),
    writeln('Well guessed!'),
    !.
```


Example:


```prolog
?- main.
Guess the number: 1.
Guess the number: 2.
Guess the number: 3.
Well guessed!
true.
```



## PureBasic


```PureBasic
If OpenConsole()
  Define TheNumber=Random(9)+1
  
  PrintN("I've picked a number from 1 to 10." + #CRLF$)
  Repeat
    Print("Guess the number: ")
  Until TheNumber=Val(Input()) 
  
  PrintN("Well guessed!")
  
  Print(#CRLF$ + #CRLF$ + "Press ENTER to exit"): Input()
  CloseConsole()
EndIf
```



## Python


```python
import random
t,g=random.randint(1,10),0
g=int(input("Guess a number that's between 1 and 10: "))
while t!=g:g=int(input("Guess again! "))
print("That's right!")
```



## R


```R
f <- function() {
    print("Guess a number between 1 and 10 until you get it right.")
    n <- sample(10, 1)
    while (as.numeric(readline()) != n) {
        print("Try again.")
    }
    print("You got it!")
}
```



## Racket


```Racket
#lang racket
(define (guess-number)
  (define number (add1 (random 10)))
  (let loop ()
    (define guess (read))
    (if (equal? guess number)
        (display "Well guessed!\n")
        (loop))))
```



## RapidQ


```vb

RANDOMIZE
number = rnd(10) + 1
Print "I selected a number between 1 and 10, try to find it:" + chr$(10)

while Guess <> Number 
    input "Your guess: "; Guess
wend

print "You guessed right, well done !"
input "Press enter to quit";a$

```



## Rascal


```Rascal
import vis::Render;
import vis::Figure;
import util::Math;

public void Guess(){
	random = arbInt(10);
	entered = "";
	guess = false;
	figure = box(vcat([
			text("Try to guess the number from 0 to 9."),
			textfield("Put your guess here", void(str s){guess = (toInt(s)==random); entered = s; }, fillColor("white")),
			text(str(){return guess ? "Correct answer!" : "This is false, the number is not <entered>.";}),
			button("Start over", void(){random = arbInt(10);}) ]));	 	
	render(figure);
}
```


Output: 

[[File:Guess.png]]


## Red


```red

Red []
#include %environment/console/CLI/input.red
random/seed now
print "I have thought of a number between 1 and 10. Try to guess it."
number: random 10
while [(to integer! input) <> number][
    print "Your guess was wrong. Try again."
]
print "Well guessed!"

```



## Retro


```Retro
: checkGuess ( gn-gf || f )
  over = [ drop 0 ] [ "Sorry, try again!\n" puts -1 ] if ;

: think ( -n )
  random abs 10 mod 1+ ;

: guess ( - )
  "I'm thinking of a number between 1 and 10.\n" puts
  "Try to guess it!\n" puts
  think [ getToken toNumber checkGuess ] while
  "You got it!\n" puts ;

```



## REXX


### version 1

(Note:  most REXXes won't accept that first statement, the shebang/sha-bang/hashbang/pound-bang/hash-exclam/hash-pling.)

```rexx
#!/usr/bin/rexx
/*REXX program to play:    Guess the number  */

number = random(1,10)
say "I have thought of a number. Try to guess it!"

guess=0    /* We don't want a valid guess, before we start */

do while guess \= number
  pull guess
  if guess \= number then
    say "Sorry, the guess was wrong. Try again!"
  /* endif - There is no endif in rexx. */
end

say "Well done! You guessed it!"

```



### version 2


```rexx
/*REXX program interactively plays "guess my number" with a human, the range is 1──►10. */
?=random(1, 10)                                  /*generate a low random integer.       */
say 'Try to guess my number between 1 ──► 10  (inclusive).'             /*the directive.*/

                       do j=1  until  g=?                               /*keep at it ···*/
                       if j\==1  then say 'Try again.'                  /*2nd-ary prompt*/
                       pull g                                           /*obtain a guess*/
                       end   /*j*/
say 'Well guessed!'                              /*stick a fork in it,  we're all done. */
```



## Ring



```ring


### Bert Mariani
### 2018-03-01
### Guess_My_Number

myNumber = random(10)
answer   = 0

See "Guess my number between 1 and 10"+ nl

while answer != myNumber
  See "Your guess: "
  Give answer

  if answer = myNumber
    See "Well done! You guessed it! "+ myNumber +nl
  else
    See "Try again"+ nl
  ok

  if answer = 0
    See "Give up. My number is: "+ myNumber +nl
  ok
end


```



## RPL



```UserRPL

DIR
  INITIALIZE
  << { C G R } PURGE RAND 10 * 1 + IP 'R' STO GUESSING
  >>
  GUESSING
  << "Pick a number between 1 and 10." "" INPUT OBJ-> 'G' STO
     IF
       G R ==
     THEN
       CLLCD "You got it!" 1 DISP 7 FREEZE 0 WAIT CLLCD CONTINUE
     ELSE
       IF
         G R <
       THEN
         CLLCD "Try a larger number." 1 DISP 7 FREEZE 0 WAIT GUESSING
       ELSE
         CLLCD "Try a smaller number." 1 DISP 7 FREEZE 0 WAIT GUESSING
       END
     END
  >>
  CONTINUE
  << "Do you want to continue? (0/1)" "" INPUT OBJ-> 'C' STO
     IF
       C 1 ==
     THEN
       INITIALIZE
     ELSE
       CLEAR
     END
  >>
END
```



## Ruby


```ruby

n = rand(1..10)
puts 'Guess the number: '
puts 'Wrong! Guess again: ' until gets.to_i == n
puts 'Well guessed!'

```



## Run BASIC


```Runbasic
while 1
choose = int(RND(0) * 9) + 1
while guess <> choose
   print "Guess a number between 1 and 10: ";: input guess
   if guess = choose THEN
      print "You guessed!"
     else
      print "Sorry, try again"
   end if
wend
wend
```



## Rust

{{libheader|rand}}

```rust
extern crate rand;

fn main() {
    println!("Type in an integer between 1 and 10 and press enter.");

    let n = rand::random::<u32>() % 10 + 1;
    loop {
        let mut line = String::new();
        std::io::stdin().read_line(&mut line).unwrap();
        let option: Result<u32,_> = line.trim().parse();
        match option {
            Ok(guess) => {
                if guess < 1 || guess > 10 {
                    println!("Guess is out of bounds; try again.");
                } else if guess == n {
                    println!("Well guessed!");
                    break;
                } else {
                    println!("Wrong! Try again.");
                }
            },
            Err(_) => println!("Invalid input; try again.")
        }
    }
}
```



## Scala



```scala

val n = (math.random * 10 + 1).toInt
print("Guess the number: ")
while(readInt != n) print("Wrong! Guess again: ")
println("Well guessed!")

```



## Scheme

{{works with|Chicken Scheme}}
{{works with|Guile}}

```scheme
(define (guess) 
  (define number (random 11))
  (display "Pick a number from 1 through 10.\n> ")
  (do ((guess (read) (read)))
        ((= guess number) (display "Well guessed!\n"))
      (display "Guess again.\n")))
```

{{Out}}

```txt
scheme> (guess)
Pick a number from 1 through 10.
1
Guess again.
2
Guess again.
3
Guess again.
4
Guess again.
5
Well guessed!
```



## Seed7


```seed7
$ include "seed7_05.s7i";

const proc: main is func
  local
    var integer: number is 0;
    var integer: guess is 0;
  begin
    number := rand(1, 10);
    writeln("I'm thinking of a number between 1 and 10.");
    writeln("Try to guess it!");
    readln(guess);
    while guess <> number do
      writeln("That's not my number.");
      writeln("Try another guess!");
      readln(guess);
    end while;
    writeln("You have won!");
  end func;
```



## Self


Gives dialog boxes in GUI, uses stdout/in otherwise.

Well factored:


```self
(| 
parent* = traits clonable.
copy = (resend.copy secretNumber: random integerBetween: 1 And: 10).
secretNumber.

ask             = ((userQuery askString: 'Guess the Number: ') asInteger).
reportSuccess   = (userQuery report: 'You got it!').
reportFailure   = (userQuery report: 'Nope. Guess again.').
sayIntroduction = (userQuery report: 'Try to guess my secret number between 1 and 10.').

hasGuessed = ( [ask = secretNumber] onReturn: [|:r| r ifTrue: [reportSuccess] False: [reportFailure]] ).
run  = (sayIntroduction. [hasGuessed] whileFalse)
|) copy run
```


Simple method:


```self
| n | 
userQuery report: 'Try to guess my secret number between 1 and 10.'.
n: random integerBetween: 1 And: 10.
[(userQuery askString: 'Guess the Number.') asInteger = n] whileFalse: [
    userQuery report: 'Nope. Guess again.'].
userQuery report: 'You got it!'
```



## Sidef


```ruby
var n = pick(1..10)
print 'Guess the number: '
while (n != read(Number).int) {
    print 'Wrong! Guess again: '
}
say 'Well guessed!'
```



## Small Basic


```Small Basic
number=Math.GetRandomNumber(10)
TextWindow.WriteLine("I just thought of a number between 1 and 10. What is it?")
While guess<>number
    guess=TextWindow.ReadNumber()
    TextWindow.WriteLine("Guess again! ")
EndWhile
TextWindow.WriteLine("You win!")
```



## SNUSP

{{works with|SNUSP Bloated}}

(random numbers are between 0 and 9; %assigns a random value between 0 and current cell value)
Unoptimised.
                         /++.#
                         \>.--.++<..+ +\
                         />+++++++.>-.+/
        / \              />++++++>++\
        < +              ?
        < >              \ -<<++++++/   
 $++++\ - +              !
 /++++/   >              \++++++++++\ 
 \+ %!/!\?/>>>,>>>>>>+++\/ !/?\<?\>>/
       /++++++++++++++++/   > -  \\
       \+++++++++\  /    /  - <
       /+++++++++/          \ /
       \+++++++++++\ /++++++++++>>/
        /<<<<<</?\!/ !
               < -   /-<<+++++++\
               < >   ?
               < >   \>+++++++>+/
               < >   >
               < >   .
               < >   \-----.>++\
               - >    /.+++.+++/
               \ /    \!/?\<!/?\    \
        \           /   \-/  \-/
      \          <</?\!>/?\!</?\!<<</
                   \-/  >    \-/
                        + -
                        < >
                        < +
                        \ /             


## Swift


```Swift
import Cocoa

var found = false
let randomNum = Int(arc4random_uniform(10) + 1)

println("Guess a number between 1 and 10\n")
while (!found) {
    var fh = NSFileHandle.fileHandleWithStandardInput()
    
    println("Enter a number: ")
    let data = fh.availableData
    var str = NSString(data: data, encoding: NSUTF8StringEncoding)
    if (str?.integerValue == randomNum) {
        found = true
        println("Well guessed!")
    }
}
```



## Tcl


```tcl
set target [expr {int(rand()*10 + 1)}]
puts "I have thought of a number."
puts "Try to guess it!"
while 1 {
    puts -nonewline "Enter your guess: "
    flush stdout
    gets stdin guess
    if {$guess == $target} {
	break
    }
    puts "Your guess was wrong. Try again!"
}
puts "Well done! You guessed it."
```

Sample output:

```txt

I have thought of a number.
Try to guess it!
Enter your guess: 1
Your guess was wrong. Try again!
Enter your guess: 2
Your guess was wrong. Try again!
Enter your guess: 3
Your guess was wrong. Try again!
Enter your guess: 8
Well done! You guessed it.

```


## TUSCRIPT


```tuscript

$$ MODE TUSCRIPT
PRINT "Find the luckynumber (7 tries)!"
luckynumber=RANDOM_NUMBERS (1,10,1)
COMPILE
LOOP round=1,7
message=CONCAT ("[",round,"] Please insert a number")
ASK $message: n=""
 IF (n!='digits') THEN
   PRINT "wrong insert: ",n," Please insert a digit"
 ELSEIF (n>10.or.n<1) THEN
   PRINT "wrong insert: ",n," Please insert a number between 1-10"
 ELSEIF (n==#luckynumber) THEN
   PRINT "BINGO"
   EXIT
ENDIF
IF (round==7) PRINT/ERROR "You've lost: luckynumber was: ",luckynumber
ENDLOOP
ENDCOMPILE

```

Output:

```txt

Find the luckynumber (7 tries)!
[1] Please insert a number >a
wrong insert: a Please insert a digit
[2] Please insert a number >10
[3] Please insert a number >1
[4] Please insert a number >2
[5] Please insert a number >9
[6] Please insert a number >8
BINGO 

```



## UNIX Shell

{{works with|Bourne Shell}}

```bash
#!/bin/sh
# Guess the number
# This simplified program does not check the input is valid

# Use awk(1) to get a random number. If awk(1) not found, exit now.
number=`awk 'BEGIN{print int(rand()*10+1)}'` || exit

echo 'I have thought of a number. Try to guess it!'
echo 'Enter an integer from 1 to 10.'
until read guess; [ "$guess" -eq "$number" ]
do
  echo 'Sorry, the guess was wrong! Try again!'
done
echo 'Well done! You guessed it.'
```


An older version used <code>while [ "$guess" -ne "$number" ]</code>. With [[pdksh]], input like '+' or '4x' would force the test to fail, end that while loop, and act like a correct guess. With <code>until [ "$guess" -eq "$number" ]</code>, input like '+' or '4x' now continues this until loop, and acts like a wrong guess. With Heirloom's Bourne Shell, '+' acts like '0' (always a wrong guess), and '4x' acts like '4' (perhaps correct).

=
## C Shell
=
{{libheader|jot}}

```csh
#!/bin/csh -f
# Guess the number

# jot(1) a random number. If jot(1) not found, exit now.
@ number = `jot -r 1 1 10` || exit

echo 'I have thought of a number. Try to guess it!'
echo 'Enter an integer from 1 to 10.'
@ guess = "$<"
while ( $guess != $number )
	echo 'Sorry, the guess was wrong! Try again!'
	@ guess = "$<"
end
echo 'Well done! You guessed it.'
```



## Vala


```vala
int main() {
	int x = Random.int_range(1, 10);
	stdout.printf("Make a guess (1-10): ");
	while(int.parse(stdin.read_line()) != x) 
                stdout.printf("Wrong! Try again: ");
	stdout.printf("Got it!\n");
	return 0;
}
```



## Ursa

{{trans|Python}}

```ursa
# Simple number guessing game

decl ursa.util.random random
decl int target guess

set target (int (+ 1 (random.getint 9)))
out "Guess a number between 1 and 10." endl console
while (not (= target guess))
        set guess (in int console)
end while

out "That's right!" endl console
```



## VBA


```VB
Sub GuessTheNumber()
Dim NbComputer As Integer, NbPlayer As Integer
    Randomize Timer
    NbComputer = Int((Rnd * 10) + 1)
    Do
        NbPlayer = Application.InputBox("Choose a number between 1 and 10 : ", "Enter your guess", Type:=1)
    Loop While NbComputer <> NbPlayer
    MsgBox "Well guessed!"
End Sub
```



## VBScript


```VBScript
randomize
MyNum=Int(rnd*10)+1
Do
	x=x+1
	YourGuess=InputBox("Enter A number from 1 to 10")
	If not Isnumeric(YourGuess) then
		msgbox YourGuess &" is not numeric. Try again."
	ElseIf CInt(YourGuess)>10 or CInt(YourGuess)<1 then
		msgbox YourGuess &" is not between 1 and 10. Try Again."
	ElseIf CInt(YourGuess)=CInt(MyNum) then
		MsgBox "Well Guessed!"
		wscript.quit
	ElseIf Cint(YourGuess)<>CInt(Mynum) then
		MsgBox "Nope. Try again."
	end If

	if x > 20 then
		msgbox "I take pity on you"
		wscript.quit
	end if
loop
```



## Visual Basic .NET


```vbnet
Module Guess_the_Number
    Sub Main()
        Dim random As New Random()
        Dim secretNum As Integer = random.Next(10) + 1
        Dim gameOver As Boolean = False
        Console.WriteLine("I am thinking of a number from 1 to 10. Can you guess it?")
        Do
            Dim guessNum As Integer
            Console.Write("Enter your guess: ")

            If Not Integer.TryParse(Console.ReadLine(), guessNum) Then
                Console.WriteLine("You should enter a number from 1 to 10. Try again!")
                Continue Do
            End If

            If guessNum = secretNum Then
                Console.WriteLine("Well guessed!")
                gameOver = True
            Else
                Console.WriteLine("Incorrect. Try again!")
            End If
        Loop Until gameOver
    End Sub
End Module
```

{{Out}}

```txt
I am thinking of a number from 1 to 10. Can you guess it?
Enter your guess: abc
You should enter a number from 1 to 10. Try again!
Enter your guess: 1
Incorrect. Try again!
Enter your guess: 2
Incorrect. Try again!
Enter your guess: 3
Incorrect. Try again!
Enter your guess: 4
Well guessed!
```



## Vlang


```vlang
import rand
import time
import os

fn main() {
    t := time.now()
    s := t.calc_unix()
    rand.seed(s)

    num := rand.next(10) // Random number
    
    // Game loop
    for {
        println('Please guess a number from 1-10 and press <Enter>')
        guess := os.get_line()
        if guess.int() == num {
            println('Well guessed!')
            return
        } else {
            print('Incorrect! ')
        }
    }
}

```



## Wee Basic

Due to how the code works, any key has to be entered to generate the random number.

```Wee Basic
let mnnumber=1
let mxnumber=10
let number=mnnumber
let keycode=0
let guessed=0
print 1 "Press any key so the computer can generate a random number for you to guess."
while keycode=0
let number=number=1
let keycode=key()
if number=mxnumber+1
let number=mnnumber
endif
wend
print 1 "Guess the number. It is between "+mnnumber+"and "+mxnumber
while guessed=0
input guess
if guess=number
print 1 "Well done! You guessed the correct number."
let guessed=1
else
print 1 "You guessed the incorrect number. Please try again."
endif
wend
end
```



## Wortel

{{trans|JavaScript}}

```wortel
@let {
  num 10Wc
  guess 0
  [
    @while != guess num
      :guess !prompt "Guess the number between 1 and 10 inclusive"
    !alert "Congratulations!\nThe number was {num}."
  ]
}
```



## X86 Assembly

{{works with|NASM|Linux}}

```asm
global _start

section .data

    rand dd 0
    guess dd 0
    msg1 db "Guess my number (1-10)", 10
    len1 equ $ - msg1
    msg2 db "Wrong, try again!", 10
    len2 equ $ - msg2
    msg3 db "Well guessed!", 10
    len3 equ $ - msg3

section .text

    _start:
        ; random number using time
        mov eax, 13
        mov ebx, rand
        int 80h
        mov eax, [ebx]
        mov ebx, 10
        xor edx, edx
        div ebx
        inc edx
        mov [rand], edx
        
        ; print msg1
        mov eax, 4
        mov ebx, 1
        mov ecx, msg1
        mov edx, len1
        int 80h
        
    input:
        ; get input
        mov eax, 3
        xor ebx, ebx
        mov ecx, msg1
        mov edx, 1
        int 80h
        mov al, [ecx]
        cmp al, 48
        jl check
        cmp al, 57
        jg check
        ; if number
        sub al, 48
        xchg eax, [guess]
        mov ebx, 10
        mul ebx
        add [guess], eax
        jmp input
        
    check:
        ; else check number
        mov eax, 4
        inc ebx
        mov ecx, [guess]
        cmp ecx, [rand]
        je done
        ; if not equal
        mov ecx, msg2
        mov edx, len2
        mov dword [guess], 0
        int 80h
        jmp input
        
    done:
        ; well guessed
        mov ecx, msg3
        mov edx, len3
        int 80h
        ; exit
        mov eax, 1
        xor    ebx, ebx
        int 80h
```



## XLISP


```lisp
(defun guessing-game ()
    (defun prompt ()
        (display "What is your guess? ")
        (define guess (read))
        (if (= guess n)
            (display "Well guessed!")
            (begin
                (display "No...")
                (newline)
                (prompt))))
    (define n (+ (random 10) 1))
    (display "I have thought of a number between 1 and 10. Try to guess it!")
    (newline)
    (prompt))
```

{{out}}

```txt
[1] (guessing-game)
I have thought of a number between 1 and 10. Try to guess it!
What is your guess? 2
No...
What is your guess? 8
No...
What is your guess? 1
No...
What is your guess? 3
No...
What is your guess? 6
No...
What is your guess? 7
No...
What is your guess? 10
Well guessed!
```



## XPL0


```XPL0
code Ran=1, IntIn=10, Text=12;
int N, G;
[N:= Ran(10)+1;
Text(0, "I'm thinking of a number between 1 and 10.^M^J");
loop    [Text(0, "Can you guess it? ");
        G:= IntIn(0);
        if G=N then quit;
        Text(0, "Nope, that's not it.^M^J");
        ];
Text(0, "Well guessed!^M^J");
]
```



## zkl

Strings are used to avoid dealing with error handling

```zkl
r:=((0).random(10)+1).toString();
while(1){
   n:=ask("Num between 1 & 10: ");
   if(n==r){ println("Well guessed!"); break; } 
   println("Nope")
}
```



{{omit from|GUISS}}
{{omit from|Lilypond}}
