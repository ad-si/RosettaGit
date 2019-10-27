+++
title = "Guess the number/With feedback"
description = ""
date = 2019-10-21T20:33:24Z
aliases = []
[extra]
id = 8634
[taxonomies]
categories = []
tags = []
+++

{{task|Games}}

;Task:
Write a game (computer program) that follows the following rules:
::* The computer chooses a number between given set limits.
::* The player is asked for repeated guesses until the the target number is guessed correctly
::* At each guess, the computer responds with whether the guess is:
:::::* higher than the target,
:::::* equal to the target, 
:::::* less than the target,   or
:::::* the input was inappropriate. 


;Related task:
*   [[Guess the number/With Feedback (Player)]]





## Ada



```Ada
with Ada.Numerics.Discrete_Random;
with Ada.Text_IO;
procedure Guess_Number_Feedback is
   procedure Guess_Number (Lower_Limit : Integer; Upper_Limit : Integer) is
      subtype Number is Integer range Lower_Limit .. Upper_Limit;
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
         if Your_Guess > My_Number then
            Ada.Text_IO.Put_Line ("Wrong, too high!");
         else
            Ada.Text_IO.Put_Line ("Wrong, too low!");
         end if;
      end loop;
      Ada.Text_IO.Put_Line ("Well guessed!");
   end Guess_Number;
   package Int_IO is new Ada.Text_IO.Integer_IO (Integer);
   Lower_Limit : Integer;
   Upper_Limit : Integer;
begin
   loop
      Ada.Text_IO.Put ("Lower Limit: ");
      Int_IO.Get (Lower_Limit);
      Ada.Text_IO.Put ("Upper Limit: ");
      Int_IO.Get (Upper_Limit);
      exit when Lower_Limit < Upper_Limit;
      Ada.Text_IO.Put_Line ("Lower limit must be lower!");
   end loop;
   Guess_Number (Lower_Limit, Upper_Limit);
end Guess_Number_Feedback;
```



## ALGOL 68

{{works with|ALGOL 68G|Any - tested with release 2.8.win32}}

```algol68
# simple guess-the-number game #

main:(

    BOOL valid input := TRUE;

    # register an error handler so we can detect and recover from #
    # invalid input #
    on value error( stand in
                  , ( REF FILE f )BOOL:
                    BEGIN
                        valid input := FALSE;
                        TRUE
                    END
                  );

    # construct a random integer between 1 and 100                #
    INT   number = 1 + ROUND ( random * 99 );

    print( ( "I'm thinking of a number between 1 and 100", newline ) );

    # read the user's guesses until they guess correctly          #
    # we give feedback so they can find the number using interval #
    # halving                                                     #
    WHILE
        print( ( "What do you think it is? " ) );
        INT guess;
        WHILE
            valid input := TRUE;
            read( ( guess, newline ) );
            NOT valid input
        DO
            print( ( "Please guess a number between 1 and 100", newline ) )
        OD;
        number /= guess
    DO
        IF number > guess
        THEN
            # the user's guess was too small #
            print( ( newline, "My number is higher than that", newline ) )
        ELSE
            # the user's guess was too large #
            print( ( newline, "My number is lower than that",  newline ) )
        FI
    OD;
    print( ( "That's correct!", newline ) )
)
```

{{out}}

```txt

I'm thinking of a number between 1 and 100
What do you think it is? f
Please guess a number between 1 and 100
q
Please guess a number between 1 and 100
50

My number is higher than that
What do you think it is? 75

My number is lower than that
What do you think it is? 67

My number is higher than that
What do you think it is? 72

My number is higher than that
What do you think it is? 73
That's correct!

```



## AppleScript



```AppleScript
-- defining the range of the number to be guessed
property minLimit : 1
property maxLimit : 100

on run
	-- define the number to be guessed
	set numberToGuess to (random number from minLimit to maxLimit)
	-- prepare a variable to store the user's answer
	set guessedNumber to missing value
	-- prepare a variable for feedback
	set tip to ""
	-- start a loop (will be exited by using "exit repeat" after a correct guess)
	repeat
		-- ask the user for his/her guess, the variable tip contains text after first guess only
		set usersChoice to (text returned of (display dialog "Guess the number between " & minLimit & " and " & maxLimit & " inclusive" & return & tip default answer "" buttons {"Check"} default button "Check"))
		-- try to convert the given answer to an integer and compare it the number to be guessed
		try
			set guessedNumber to usersChoice as integer
			if guessedNumber is greater than maxLimit or guessedNumber is less than minLimit then
				-- the user guessed a number outside the given range 
				set tip to "(Tipp: Enter a number between " & minLimit & " and " & maxLimit & ")"
			else if guessedNumber is less than numberToGuess then
				-- the user guessed a number less than the correct number 
				set tip to "(Tipp: The number is greater than " & guessedNumber & ")"
			else if guessedNumber is greater than numberToGuess then
				-- the user guessed a number greater than the correct number 
				set tip to "(Tipp: The number is less than " & guessedNumber & ")"
			else if guessedNumber is equal to numberToGuess then
				-- the user guessed the correct number and gets informed
				display dialog "Well guessed! The number was " & numberToGuess buttons {"OK"} default button "OK"
				-- exit the loop (quits this application)
				exit repeat
			end if
		on error
			-- something went wrong, remind the user to enter a numeric value
			set tip to "(Tipp: Enter a number between " & minLimit & " and " & maxLimit & ")"
		end try
	end repeat
end run
```



## AutoHotkey



```AutoHotkey
MinNum = 1
MaxNum = 99999999999

Random, RandNum, %MinNum%, %MaxNum%
Loop
{
 InputBox, Guess, Number Guessing, Please enter a number between %MinNum% and %MaxNum%:,, 350, 130,,,,, %Guess%
 If ErrorLevel
  ExitApp
 If Guess Is Not Integer
 {
  MsgBox, 16, Error, Invalid guess.
  Continue
 }
 If Guess Not Between %MinNum% And %MaxNum%
 {
  MsgBox, 16, Error, Guess must be a number between %MinNum% and %MaxNum%.
  Continue
 }
 If A_Index = 1
  TotalTime = %A_TickCount%
 Tries = %A_Index%
 If Guess = %RandNum%
  Break
 If Guess < %RandNum%
  MsgBox, 64, Incorrect, The number guessed (%Guess%) was too low.
 If Guess > %RandNum%
  MsgBox, 64, Incorrect, The number guessed (%Guess%) was too high.
}
TotalTime := Round((A_TickCount - TotalTime) / 1000,1)
MsgBox, 64, Correct, The number %RandNum% was guessed in %Tries% tries, which took %TotalTime% seconds.
```



## AWK


```AWK

# syntax: GAWK -f GUESS_THE_NUMBER_WITH_FEEDBACK.AWK
BEGIN {
    L = 1
    H = 100
    srand()
    n = int(rand() * (H-L+1)) + 1
    printf("I am thinking of a number between %d and %d. Try to guess it.\n",L,H)
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
      if (n > ans) {
        print("Incorrect, your answer was too low. Try again.")
        continue
      }
      if (n < ans) {
        print("Incorrect, your answer was too high. Try again.")
        continue
      }
    }
    exit(0)
}

```



## BASIC


=
## Applesoft BASIC
=

```ApplesoftBasic
100 L% = 1
110 U% = 10
120 N% = RND(1) * (U% - L% + 1) + L%
130 PRINT "A NUMBER FROM " L%;
140 PRINT " TO " U%;
150 PRINT ", CALLED A TARGET, HAS BEEN RANDOMLY CHOSEN."
160 FOR Q = 0 TO 1 STEP 0
170     INPUT "GUESS THE TARGET NUMBER: "; G%
180     IF G% < L% OR G% > U% THEN PRINT "THE INPUT WAS INAPPROPRIATE."
190     IF G% > N% THEN PRINT "THE GUESS WAS HIGHER THAN THE TARGET."
200     IF G% < N% THEN PRINT "THE GUESS WAS LESS THAN THE TARGET."
210     Q = G% = N%
220 NEXT
230 PRINT "THE GUESS WAS EQUAL TO THE TARGET."
```


=
## BASIC256
=
{{works with|BASIC256 }}

```basic256

Min = 5: Max = 15
chosen = int(rand*(Max-Min+1)) + Min 
print "Guess a whole number between "+Min+" and "+Max
do
   input "Enter your number " ,guess
   if guess < Min OR guess > Max then 
	print "That was an invalid number"
	end
   else
	if guess < chosen then print "Sorry, your number was too low"
        if guess > chosen then  print "Sorry, your number was too high"
        if guess = chosen then print "Well guessed!"
   end if
until guess = chosen

```

Output:(example)

```txt

Guess a whole number between 5 and 15
Enter your number 10
Sorry, your number was too high
Enter your number 7
Sorry, your number was too low
Enter your number 8
Well guessed!

```


==={{header|IS-BASIC}}===
<lang IS-BASIC>100 PROGRAM "Guess.bas"
110 RANDOMIZE 
120 LET UP=10:LET LO=1 ! Limits
130 PRINT "I'm thinking of a number between";LO;"and";UP
140 LET COUNT=0:LET NR=RND(UP-LO+1)+LO
150 DO 
160   LET COUNT=COUNT+1
170   INPUT PROMPT "Guess a number: ":GU
180   SELECT CASE GU
190   CASE IS>NR
200     PRINT "My number is lower that."
210   CASE IS<NR
220     PRINT "My number is higher that."
230   CASE ELSE
240     PRINT "Well guessed! Numner of tips:";COUNT
250   END SELECT 
260 LOOP UNTIL NR=GU
```



## Batch File


```dos
@echo off

:A
set /a number=%random% %% 10 + 1
:B
set /p guess=Choose a number between 1 and 10: 
if %guess equ %number% goto win
if %guess% gtr 10 msg * "Number must be between 1 and 10."
if %guess% leq 0 msg * "Number must be between 1 and 10."
if %guess% gtr %number% echo Higher!
if %guess% lss %number% echo Lower!
goto B
:win
cls
echo You won! The number was %number%
pause>nul
goto A
```



## BBC BASIC


```bbcbasic
      Min% = 1
      Max% = 10
      chosen% = RND(Max%-Min%+1) + Min% - 1
      REPEAT
        PRINT "Guess a number between "; Min% " and " ;Max% ;
        INPUT ": " guess%
        CASE TRUE OF
          WHEN guess% < Min% OR guess% > Max%:
            PRINT "That was an invalid number"
          WHEN guess% < chosen%:
            PRINT "Sorry, your number was too low"
          WHEN guess% > chosen%:
            PRINT "Sorry, your number was too high"
          OTHERWISE:
            PRINT "Well guessed!"
            EXIT REPEAT
        ENDCASE
      UNTIL FALSE
      END
```



## Befunge


The number range is hardcoded at the start of the program (<tt>1:"d"</tt> being 1 to 100), but can easily be changed.


```befunge
1:"d">>048*"neewteb rebmun a sseuG">:#,_$\:.\"d na",,\,,:.55+,\-88+0v
<*2\_$\1+%+48*">",,#v>#+:&#>#5-:!_0`00p0"hgih"00g>_0"wol"00g!>_48vv1?
\1-:^v"d correctly!"<^,,\,+55,". >"$_,#!>#:<"Your guess was too"*<>+>
:#,_@>"ess"                                               >"eug uoY">
```


{{out}}

```txt
Guess a number between 1 and 100
> 50
Your guess was too high.
> 25
Your guess was too low.
> 37
Your guess was too high.
> 31
You guessed correctly!
```



## Brat


```brat
number = random 10

p "Guess a number between 1 and 10."

until {
  guess = ask("Guess: ").to_i

  true? (guess.null? || { guess > 10 || guess < 1 })
    { p "Please guess a number between 1 and 10."}
    { true? guess == number
      { p "Correct!"; true }
      { true? guess < number
        { p "Too low!" }
        { p "Too high!" }
      }
    }
}
```



## C


```c>#include <stdlib.h

#include <stdio.h>
#include <time.h>

#define lower_limit 0
#define upper_limit 100

int main(){
  int number, guess;

  srand( time( 0 ) );
  number = lower_limit + rand() % (upper_limit - lower_limit + 1);

  printf( "Guess the number between %d and %d: ", lower_limit, upper_limit );

  while( scanf( "%d", &guess ) == 1 ){
    if( number == guess ){
      printf( "You guessed correctly!\n" );
      break;
    }
    printf( "Your guess was too %s.\nTry again: ", number < guess ? "high" : "low" );
  }

  return 0;
}
```


Demonstration:

```txt
Guess the number between 0 and 100: 50
Your guess was too low.
Try again: 75
Your guess was too high.
Try again: 63
Your guess was too high.
Try again: 57
Your guess was too low.
Try again: 60
Your guess was too high.
Try again: 58
Your guess was too low.
Try again: 59
You guessed correctly!
```



## C++


```cpp>#include <iostream

#include <cstdlib>
#include <ctime>

int main()
{
    std::srand(std::time(0));
    int lower, upper, guess;
    std::cout << "Enter lower limit: ";
    std::cin >> lower;
    std::cout << "Enter upper limit: ";
    std::cin >> upper;
    int random_number = lower + std::rand() % ((upper + 1) - lower);

    do
    {
        std::cout << "Guess what number I have: ";
        std::cin >> guess;
        if (guess > random_number)
            std::cout << "Your guess is too high\n";
        else if (guess < random_number)
            std::cout << "Your guess is too low\n";
        else
            std::cout << "You got it!\n";
    } while (guess != random_number);

    return 0;
}
```

Output:

```txt
Enter lower limit: 1
Enter upper limit: 100
Guess what number I have: 50
Your guess is too high
Guess what number I have: 40
Your guess is too high
Guess what number I have: 25
Your guess is too high
Guess what number I have: 10
Your guess is too high
Guess what number I have: 2
Your guess is too low
Guess what number I have: 4
Your guess is too high
Guess what number I have: 3
You got it!
```



## C sharp


```csharp
using System;

class Program
{
    static void Main(string[] args)
    {
        const int from = 1;
        const int to = 10;

        int randomNumber = new Random().Next(from, to);
        int guessedNumber;

        Console.Write("The number is between {0} and {1}. ", from, to);
        while (true)
        {
            Console.Write("Make a guess: ");
            if (int.TryParse(Console.ReadLine(), out guessedNumber))
            {
                if (guessedNumber == randomNumber)
                {
                    Console.WriteLine("You guessed the right number!");
                    break;
                }
                else
                {
                    Console.WriteLine("Your guess was too {0}.", (guessedNumber > randomNumber) ? "high" : "low");
                }
            }
            else
            {
                Console.WriteLine("Input was not an integer.");
            }
        }

        Console.WriteLine();
        Console.WriteLine("Press any key to exit.");
        Console.ReadKey();
    }
}

```

Output:

```txt
The number is between 1 and 10. Make a guess: 1
Your guess was too low.
Make a guess: 9
Your guess was too high.
Make a guess: 5
Your guess was too low.
Make a guess: 6
Your guess was too low.
Make a guess: hello, world
Input was not an integer.
Make a guess: 7
You guessed the right number!
 
Press any key to exit.

```


=={{header|Caché ObjectScript}}==
<lang Caché ObjectScript>GUESSNUM
    ; get a random number between 1 and 100
    set target = ($random(100) + 1)    ; $r(100) gives 0-99
    
    ; loop until correct
    set tries = 0
    for {
        write !,"Guess the integer between 1 and 100: "
        read "",guess    ; gets input following write
        
        ; validate input
        if (guess?.N) && (guess > 0) && (guess < 101) {
            ; increment attempts
            set tries = tries + 1
            
            ; evaluate the guess
            set resp = $select(guess < target:"too low.",guess > target:"too high.",1:"correct!")
            
            ; display result, conditionally exit loop
            write !,"Your guess was "_resp
            quit:resp="correct!"        
        }
        else {
            write !,"Please enter an integer between 1 and 100."
        }
    }    ; guess loop
    
    write !!,"You guessed the number in "_tries_" attempts."
    quit
```


{{out}}
```txt
SAMPLES>do ^GUESSNUM^

Guess the integer between 1 and 100: 50
Your guess was too low.
Guess the integer between 1 and 100: 75
Your guess was too high.
Guess the integer between 1 and 100: 60
Your guess was too low.
Guess the integer between 1 and 100: 65
Your guess was too low.
Guess the integer between 1 and 100: 70
Your guess was too low.
Guess the integer between 1 and 100: 73
Your guess was too low.
Guess the integer between 1 and 100: 74
Your guess was correct!

You guessed the number in 7 attempts.
```



## Ceylon


In you module.ceylon file put import ceylon.random "1.3.1";


```ceylon
import ceylon.random {
    DefaultRandom
}

shared void run() {
    value random = DefaultRandom();
    value range = 1..10;
    while(true) {
        value chosen = random.nextElement(range);
        print("I have chosen a number between ``range.first`` and ``range.last``.
               What is your guess?");
        while(true) {
            if(exists line = process.readLine()) {
                value guess = Integer.parse(line.trimmed);

                if(is ParseException guess) {
                    print(guess.message);
                    continue;
                }

                switch(guess <=> chosen)
                case (larger) {
                    print("Too high!");
                }
                case (smaller) {
                    print("Too low!");
                }
                case (equal) {
                    print("You got it!");
                    break;
                }
            } else {
                print("Please enter a number!");
            }
        }
    }
}
```



## Clojure


```clojure
(defn guess-run []
  (let [start 1
	end 100
	target (+ start (rand-int (inc (- end start))))]
    (printf "Guess a number between %d and %d" start end)
    (loop [i 1]
      (printf "Your guess %d:\n" i)
      (let [ans (read)]
	(if (cond
	     (not (number? ans)) (println "Invalid format")
	     (or (< ans start) (> ans end)) (println "Out of range")
	     (< ans target)    (println "too low")
	     (> ans target)    (println "too high")
	     :else             true)
	  (println "Correct")
	  (recur (inc i)))))))
```



## COBOL


```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. Guess-With-Feedback.

       DATA DIVISION.
       LOCAL-STORAGE SECTION.
       01  Seed       PIC 9(8).
       01  Random-Num PIC 99.
       01  Guess      PIC 99.
 
       PROCEDURE DIVISION.
           ACCEPT Seed FROM TIME
           COMPUTE Random-Num =
               FUNCTION REM(FUNCTION RANDOM(Seed) * 1000, 10) + 1

           DISPLAY "Guess a number between 1 and 10:"
 
           PERFORM FOREVER
               ACCEPT Guess
 
               IF Guess > Random-Num
                   DISPLAY "Your guess was too high."
               ELSE IF Guess < Random-Num
                   DISPLAY "Your guess was too low."
               ELSE
                   DISPLAY "Well guessed!"
                   EXIT PERFORM
           END-PERFORM
 
           GOBACK
           .
```



## Common Lisp


```lisp
(defun guess-the-number-feedback (&optional (min 1) (max 100))
  (let ((num-guesses 0)
	(num (+ (random (1+ (- max min))) min))
	(guess nil))
    (format t "Try to guess a number from ~:d to ~:d!~%" min max)
    (loop do (format t "Guess? ")
	  (incf num-guesses)
	  (setf guess (read))
	  (format t "Your guess is ~[not a number.~;too small.~;too large.~;correct!~]~%"
		  (cond ((not (numberp guess)) 0)
			((< guess num) 1)
			((> guess num) 2)
			((= guess num) 3)))
	  until (and (numberp guess)
		     (= guess num)))
    (format t "You got the number correct on the ~:r guess!~%" num-guesses)))

```

Output:

```txt
CL-USER> (guess-the-number-feedback 1 1024)
Try to guess a number from 1 to 1,024!
Guess? 512
Your guess is too small.
Guess? 768
Your guess is too small.
Guess? 896
Your guess is too large.
Guess? 832
Your guess is too small.
Guess? 864
Your guess is too large.
Guess? 848
Your guess is too large.
Guess? 840
Your guess is too large.
Guess? 836
Your guess is correct!
You got the number correct on the eighth guess!

```



## D

{{trans|Python}}

```d
import std.stdio, std.random, std.typecons, std.conv, std.string,
       std.range;

void main() {
    immutable interval = tuple(1, 100);
    writefln("Guess my target number that is between " ~
             "%d and %d (inclusive).\n", interval[]);
    immutable target = uniform!"[]"(interval[]);

    foreach (immutable i; sequence!q{n}) {
        writef("Your guess #%d: ", i + 1);
        immutable txt = stdin.readln.strip;

        Nullable!int answer;
        try {
            answer = txt.to!int;
        } catch (ConvException e) {
            writefln("  I don't understand your input '%s'", txt);
            continue;
        }
        if (answer < interval[0] || answer > interval[1]) {
            writeln("  Out of range!");
            continue;
        }
        if (answer == target) {
            writeln("  Well guessed.");
            break;
        }
        writeln(answer < target ? "  Too low." : "  Too high.");
    }
}
```

Sample game:

```txt
Guess my target number that is between 1 and 100 (inclusive).

Your guess #1: 100
  Too high.
Your guess #2: 5
  Too low.
Your guess #3: 522
  Out of range!
Your guess #4: 50
  Too low.
Your guess #5: 75
  Too low.
Your guess #6: 80
  Too low.
Your guess #7: 90
  Too high.
Your guess #8: 85
  Too high.
Your guess #9: 83
  Well Guessed.
```


## DCL


```DCL
$ rnd = f$extract( 21, 2, f$time() )
$ count = 0
$ loop:
$ inquire guess "guess what number between 0 and 99 inclusive I am thinking of"
$ guess = f$integer( guess )
$ if guess .lt. 0 .or. guess .gt. 99
$ then
$  write sys$output "out of range"
$  goto loop
$ endif
$ count = count + 1
$ if guess .lt. rnd then $ write sys$output "too small"
$ if guess .gt. rnd then $ write sys$output "too large"
$ if guess .ne. rnd then $ goto loop
$ write sys$output "it only took you ", count, " guesses"
```

{{out}}

```txt
$ @guess
guess what number between 0 and 99 inclusive I am thinking of: 50
too small
guess what number between 0 and 99 inclusive I am thinking of: 999
out of range
guess what number between 0 and 99 inclusive I am thinking of: 75
too small
guess what number between 0 and 99 inclusive I am thinking of: 82
too small
guess what number between 0 and 99 inclusive I am thinking of: 91
too small
guess what number between 0 and 99 inclusive I am thinking of: 96
too small
guess what number between 0 and 99 inclusive I am thinking of: 98
too large
guess what number between 0 and 99 inclusive I am thinking of: 97
it only took you 7 guesses
```



## Delphi



```Delphi
program GuessTheNumber;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  Math;


const
  // Set min/max limits
  min: Integer = 1;
  max: Integer = 10;

var
  last,
  val,
  inp: Integer;
  s: string;

  // Initialise new game
  procedure NewGame;
  begin
    // Make sure this number isn't the same as the last one
    repeat
      val := RandomRange(min,max);
    until
      val <> last;

    // Save this number
    last := val;
    Writeln('Guess a number between ', min, ' and ', max, ' [Answer = ', val, ']');
  end;

begin
  // Initialise the random number generator with a random value
  Randomize;

  // Initialise last number
  last := 0;
  
  // Initialise user input
  s := '';

  // Start game
  NewGame;

  // Loop
  repeat
    // User input
    Readln(s);

    // Validate - checxk if input is a number
    if TryStrToInt(s,inp) then
      begin
        // Is it the right number?
        if (inp = val) then
          begin
            // Yes - request a new game
            Writeln('Correct! Another go? Y/N');
            Readln(s);
            if SameText(s,'Y') then
              // Start new game
              NewGame
              else
              Exit;
          end
          else
          // Input too low/high
          if (inp < val) then
            Writeln('Too low! Try again...')
            else
            if (inp > val) then
              Writeln('Too high! Try again...');
      end
      else
      // Input invalid
      if not SameText(s,'bored') then
        Writeln('Invalid input! Try again...');
      
  until
    SameText(s,'bored');


end.

```



## EasyLang

<lang>print "Guess a number between 1 and 100!"
n = random 100 + 1
repeat
  g = number input
  if error = 1
    print "You must enter a number!"
  elif g > n
    print "too high"
  elif g < n
    print "too low"
  .
  until g = n
.
print "correct"
```



## EchoLisp


```lisp

;;(read <default-value> <prompt>) prompts the user with a default value using the browser dialog box.
;; we play sounds to make this look like an arcade game
(lib 'web) ; (play-sound) is defined in web.lib

(define (guess-feed (msg " 🔮 Enter a number in [0...100], -1 to stop.") (n (random 100)) (user 0))
   (set! user (read user msg))
   (play-sound 'ko)
   (unless (eq? n user ) ; user is the last user answer
       (guess-feed 
               (cond ;; adapt prompt according to condition
                    ((not (integer? user)) "❌ Please, enter an integer")
                    (( < user 0) (error "🌵 - It was:" n)) ; exit to top level
                    ((> n user) "Too low ...")
                    ((< n user) "Too high ..."))
              n user))
    (play-sound 'ok )  
    " 🔮 Well played!!  🍒 🍇 🍓")

```



## Ela



```ela
open string datetime random core monad io

guess () = do
  putStrLn "What's the upper bound?"
  ub <- readAny
  main ub
  where main ub
          | ub < 0 = "Bound should be greater than 0."
          | else = do 
              putStrLn $ format "Guess a number from 1 to {0}" ub
              dt <- datetime.now
              guesser (rnd (milliseconds $ dt) 1 ub)
        guesser v = do
          x <- readAny
          if x == v then
              cont ()
            else if x < v then
              do putStrLn "Too small!"
                 guesser v
            else 
              do putStrLn "Too big!"
                 guesser v
        cont () = do
          putStrLn "Correct! Do you wish to continue (Y/N)?"
          ask ()
        ask () = do
          a <- readStr
          if a == "y" || a == "Y" then
              guess ()
            else if a == "n" || a == "N" then
              do putStrLn "Bye!"
            else 
              do putStrLn "Say what?"
                 ask ()

guess () ::: IO
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
        else if (randomNumber < userGuess)
        {
            console.printLine("Your guess was too high")
        }
        else
        {
            console.printLine("Your guess was too low")
        }
    }
}
```

{{out}}

```txt

I'm thinking of a number between 1 and 10.  Can you guess it?
Guess: 5
Your guess was too low
Guess: 8
Your guess was too high
Guess: 6
Congrats!!  You guessed right!

```



## Elixir

{{works with|Elixir|1.2}}

```elixir
defmodule GuessingGame do
  def play(lower, upper) do
    play(lower, upper, Enum.random(lower .. upper))
  end
  defp play(lower, upper, number) do
    guess = Integer.parse(IO.gets "Guess a number (#{lower}-#{upper}): ")
    case guess do
      {^number, _} ->
        IO.puts "Well guessed!"
      {n, _} when n in lower..upper ->
        IO.puts if n > number, do: "Too high.", else: "Too low."
        play(lower, upper, number)
      _ ->
        IO.puts "Guess not in valid range."
        play(lower, upper, number)
    end
  end
end
 
GuessingGame.play(1, 100)
```



## Emacs Lisp


```Lisp

(let ((num (1+ (random 100)))) 
  (princ "Guess the no: ") 
  (loop 
   (setq guess (read)) 
   (cond
    ((not (numberp guess)) (print "Please enter a number"))
   ((eq guess num) 
       (progn (princ-list "Guess was right! " num) 
	      (return))) 
     ((> guess num) 
      (print "Too High!"))
     ((< guess num)
       (print "Too low!"))) ) )
```



## Erlang


```erlang
% Implemented by Arjun Sunel
-module(guess_number).
-export([main/0]).

main() ->
	L = 1 ,	 	% Lower Limit
	U = 100, 	  % Upper Limit
	
	io:fwrite("Guess my number between ~p and ", [L]),
	io:fwrite("and ~p until you get it right.\n", [U]),
	N = random:uniform(100),
	guess(N).

guess(N) ->
	{ok, [K]} = io:fread("Guess the number :  ","~d"),
	if 
		K=:=N ->
			io:format("Well guessed!!\n");
		true -> 
			if 
				K > N ->
					io:format("Your guess is too high.\n");
				true ->
					io:format("Your guess is too low.\n")
			end,			
			guess(N)	
	end.	

```

{{out}}

```txt
1> c(guess_number).    
{ok,guess_number}
2> guess_number:main().
Guess my number between 1 and and 100 until you get it right.
Guess the number :  50
Your guess is too low.
Guess the number :  70
Your guess is too low.
Guess the number :  85
Your guess is too high.
Guess the number :  75
Your guess is too high.
Guess the number :  73
Well guessed!!
ok

```



## Euphoria


```euphoria
include get.e

constant lower_limit = 0, upper_limit = 100

integer number, guess
number = rand(upper_limit-lower_limit+1)+lower_limit

printf(1,"Guess the number between %d and %d: ", lower_limit & upper_limit)
while 1 do
    guess = floor(prompt_number("", lower_limit & upper_limit))
    if number = guess then
        puts(1,"You guessed correctly!\n")
        exit
    elsif number < guess then
        puts(1,"You guessed too high.\nTry again: ")
    else
        puts(1,"You guessed to low.\nTry again: ")
    end if
end while
```


=={{header|F_Sharp|F#}}==

```fsharp

open System

[<EntryPoint>]
let main argv =
    let aim =
        let from = 1
        let upto = 100
        let rnd = System.Random()
        Console.WriteLine("Hi, you have to guess a number between {0} and  {1}",from,upto)
        rnd.Next(from,upto)

    let mutable correct = false
    while not correct do

        let guess =
            Console.WriteLine("Please enter your guess:")
            match System.Int32.TryParse(Console.ReadLine()) with
            | true, number -> Some(number)
            | false,_ -> None

        match guess with 
            | Some(number) ->
                match number with 
                    | number when number > aim ->   Console.WriteLine("You guessed to high!")
                    | number when number < aim ->   Console.WriteLine("You guessed to low!")
                    | _                        ->   Console.WriteLine("You guessed right!")
                                                    correct <- true
            | None -> Console.WriteLine("Error: You didn't entered a parsable number!")
         
    0

```



## Factor


```factor
USING:
    formatting
    fry
    io
    kernel
    math math.parser math.ranges
    prettyprint
    random ;
IN: guessnumber

: game-step ( target -- ? )
    readln string>number
    [
        2dup =
        [ 2drop f "Correct!" ]
        [ < "high" "low" ? "Guess too %s, try again." sprintf t swap ]
        if
    ]
    [ drop t "Invalid guess." ]
    if* print flush ;

: main ( -- )
    99 [1,b]
    [ unparse "Number in range %s, your guess?\n" printf flush ]
    [ random '[ _ game-step ] loop ]
    bi ;
```



## Fantom



```fantom

class Main
{
  public static Void main ()
  {
    Int lowerLimit := 1
    Int higherLimit := 100

    Int target := (lowerLimit..higherLimit).random
    Int guess
    while (guess != target)
    {
      echo ("Enter a guess: ")
      try
      {
        // read in a line of input, and try to interpret as an Int
        guess = Env.cur.in.readLine.trim.toInt
        if (guess == target)
        {
          echo ("Well guessed!")
        }
        else if (guess < target)
        {
          echo ("Failed - your guess is too small")
        }
        else // if (guess > target)
        {
          echo ("Failed - your guess is too large")
        }
      }
      catch (Err e) 
      { 
        echo ("Your guess must be an integer") 
      }
    }
  }
}

```


Sample game:

```txt

Enter a guess: 
50
Failed - your guess is too small
Enter a guess: 
75
Failed - your guess is too large
Enter a guess: 
67
Failed - your guess is too large
Enter a guess: 
60
Failed - your guess is too large
Enter a guess: 
55
Failed - your guess is too large
Enter a guess: 
53
Failed - your guess is too small
Enter a guess: 
54
Well guessed!

```



## Fortran

{{works with|Fortran|95 and later}}

```fortran
program Guess_a_number
  implicit none
  
  integer, parameter :: limit = 100
  integer :: guess, number
  real :: rnum
  
  write(*, "(a, i0, a)") "I have chosen a number between 1 and ", limit, &
                         " and you have to try to guess it." 
  write(*, "(a/)")  "I will score your guess by indicating whether it is higher, lower or the same as that number"
 
  call random_seed
  call random_number(rnum)
  number = rnum * limit + 1
  do
    write(*, "(a)", advance="no") "Enter quess: "
    read*, guess
    if(guess < number) then
      write(*, "(a/)") "That is lower"
    else if(guess > number) then
      write(*, "(a/)") "That is higher"
    else
      write(*, "(a)") "That is correct"
      exit
    end if
  end do
end program
```

Output

```txt
I have chosen a number bewteen 1 and 100 and you have to try to guess it.
I will score your guess by indicating whether it is higher, lower or the same as that number.

Enter guess: 50
That is lower

Enter guess: 75
That is higher

Enter guess: 62
That is higher

Enter guess: 57
That is correct
```



## FreeBASIC


```freebasic
' FB 1.05.0 Win64

Randomize
Dim n As Integer = Int(Rnd * 20) + 1
Dim guess As Integer

Print "Guess which number I've chosen in the range 1 to 20"
Print
Do
  Input " Your guess : "; guess
  If guess > n AndAlso guess <= 20 Then
    Print "Your guess is higher than the chosen number, try again "
  ElseIf guess = n Then
    Print "Correct, well guessed!"
    Exit Do
  ElseIf guess < n AndAlso guess >= 1 Then
    Print "Your guess is lower than the chosen number, try again"
  Else
    Print "Your guess is inappropriate, try again"   
  End If
Loop
End
```


Sample input/output
{{out}}

```txt

 Your guess : ? 14
Your guess is lower than the chosen number, try again
 Your guess : ? 17
Your guess is lower than the chosen number, try again
 Your guess : ? 18
Your guess is lower than the chosen number, try again
 Your guess : ? 19
Correct, well guessed!

```



## Genie


```genie
[indent=4]
/*
   Number guessing with feedback, in Genie
   from https://wiki.gnome.org/Projects/Genie/AdvancedSample

   valac numberGuessing.gs
   ./numberGuessing
*/
class NumberGuessing

    prop min:int
    prop max:int
    
    construct(m:int, n:int)
        self.min = m
        self.max = n
    
    def start()
        try_count:int = 0
        number:int = Random.int_range(min, max)
        
        stdout.printf("Welcome to Number Guessing!\n\n")
        stdout.printf("I have thought up a number between %d and %d\n", min, max)
        stdout.printf("which you have to guess. I will give hints as we go.\n\n")
    
        while true
            stdout.printf("Try #%d, ", ++try_count)
            stdout.printf("please enter a number between %d and %d: ", min, max)
            line:string? = stdin.read_line()
            if line is null
                stdout.printf("bailing...\n")
                break

            input:int64 = 0
            unparsed:string = ""
            converted:bool = int64.try_parse(line, out input, out unparsed)
            if not converted or line is unparsed
                stdout.printf("Sorry, input seems invalid\n")
                continue

            guess:int = (int)input
            if number is guess
                stdout.printf("Congratulations! You win.\n")
                break
            else
                stdout.printf("Try again. The number in mind is %s than %d.\n",
                   number > guess ? "greater" : "less", guess)

init
    var game = new NumberGuessing(1, 100)
    game.start()
```


{{out}}

```txt
prompt$ valac numberGuessing.gs      
prompt$ ./numberGuessing        
Welcome to Number Guessing!

I have thought up a number between 1 and 100
which you have to guess. I will give hints as we go.

Try #1, please enter a number between 1 and 100: 50
Try again. The number in mind is greater than 50.
Try #2, please enter a number between 1 and 100: abc
Sorry, input seems invalid
Try #3, please enter a number between 1 and 100: 75
Try again. The number in mind is greater than 75.
Try #4, please enter a number between 1 and 100: 88
Try again. The number in mind is less than 88.
Try #5, please enter a number between 1 and 100: 81
Try again. The number in mind is greater than 81.
Try #6, please enter a number between 1 and 100: 84
Try again. The number in mind is less than 84.
Try #7, please enter a number between 1 and 100: 82
Try again. The number in mind is greater than 82.
Try #8, please enter a number between 1 and 100: 83
Congratulations! You win.

prompt$ ./numberGuessing
Welcome to Number Guessing!

I have thought up a number between 1 and 100
which you have to guess. I will give hints as we go.

Try #1, please enter a number between 1 and 100: 50
Try again. The number in mind is greater than 50.
Try #2, please enter a number between 1 and 100: bailing...
```



## Go


```go
package main

import (
    "fmt"
    "math/rand"
    "time"
)

const lower, upper = 1, 100

func main() {
    fmt.Printf("Guess integer number from %d to %d: ", lower, upper)
    rand.Seed(time.Now().Unix())
    n := rand.Intn(upper-lower+1) + lower
    for guess := n; ; {
        switch _, err := fmt.Scan(&guess); {
        case err != nil:
            fmt.Println("\n", err, "So, bye.")
            return
        case guess < n:
            fmt.Print("Too low. Try again: ")
        case guess > n:
            fmt.Print("Too high. Try again: ")
        default:
            fmt.Println("Well guessed!")
            return
        }
    }
}
```



## Groovy

Based on the Java implementation

```groovy

def rand = new Random() // java.util.Random
def range = 1..100 // Range (inclusive)
def number = rand.nextInt(range.size()) + range.from // get a random number in the range

println "The number is in ${range.toString()}" // print the range

def guess
while (guess != number) { // keep running until correct guess
    try {
        print 'Guess the number: '
        guess = System.in.newReader().readLine() as int // read the guess in as int
        switch (guess) { // check the guess against number
            case { it < number }: println 'Your guess is too low'; break
            case { it > number }: println 'Your guess is too high'; break
            default:              println 'Your guess is spot on!'; break
        }
    } catch (NumberFormatException ignored) { // catches all input that is not a number
        println 'Please enter a number!'
    }
}

```

Example:
<lang>
The number is in 1..100
Guess the number: ghfvkghj
Please enter a number!
Guess the number: 50
Your guess is too low
Guess the number: 75
Your guess is too low
Guess the number: 83
Your guess is too low
Guess the number: 90
Your guess is too low
Guess the number: 95
Your guess is too high
Guess the number: 92
Your guess is spot on!

```



## Haskell


```haskell

import Control.Monad
import System.Random

-- Repeat the action until the predicate is true.
until_ act pred = act >>= pred >>= flip unless (until_ act pred)

answerIs ans guess =
  case compare ans guess of
    LT -> putStrLn "Too high. Guess again." >> return False
    EQ -> putStrLn "You got it!" >> return True
    GT -> putStrLn "Too low. Guess again." >> return False

-- Repeatedly read until the input *starts* with a number.  (Since
-- we use "reads" we allow garbage to follow it, though.)
ask = do line <- getLine
         case reads line of
           ((num,_):_) -> return num
           otherwise -> putStrLn "Please enter a number." >> ask

main = do
  ans <- randomRIO (1,100) :: IO Int
  putStrLn "Try to guess my secret number between 1 and 100."
  ask `until_` answerIs ans

```



## HolyC


```holyc
U8 n, *g;
U8 min = 1, max = 100;

n = min + RandU16 % max;

Print("Guess the number between %d and %d: ", min, max);

while(1) {
  g = GetStr;

  if (Str2I64(g) == n) {
    Print("You guessed correctly!\n");
    break;
  }

  if (Str2I64(g) < n)
    Print("Your guess was too low.\nTry again: ");
  if (Str2I64(g) > n)
    Print("Your guess was too high.\nTry again: ");
}
```


=={{header|Icon}} and {{header|Unicon}}==


```Icon

procedure main()
    smallest := 5
    highest := 25
    n := smallest-1 + ?(1+highest-smallest)
    repeat {
      writes("Pick a number from ", smallest, " through ", highest, ": ")
      guess := read ()
         
      if n = numeric(guess) 
        then { 
          write ("Well guessed!")
          exit ()
        }
      else if n < numeric(guess)
        then write ("Your guess is too high")
      else if n > numeric(guess)
        then write ("Your guess is too low")
      else write ("Did you enter a number?")
    }
end

```


Output:

```txt

$ ./guess-feedback
Pick a number from 5 through 25: 10
Your guess is too low
Pick a number from 5 through 25: 16
Your guess is too low
Pick a number from 5 through 25: 20
Your guess is too low
Pick a number from 5 through 25: 23
Your guess is too high
Pick a number from 5 through 25: 22
Well guessed!

```



## J


```j
require 'misc'
game=: verb define
  assert. y -: 1 >. <.{.y
  n=: 1 + ?y
  smoutput 'Guess my integer, which is bounded by 1 and ',":y
  whilst. -. x -: n do.
    x=. {. 0 ". prompt 'Guess: '
    if. 0 -: x do. 'Giving up.' return. end. 
    smoutput (*x-n){::'You win.';'Too high.';'Too low.'
  end.
)
```


Note: in computational contexts, J programmers typically avoid loops.  However, in contexts which involve progressive input and output and where event handlers are too powerful (too complicated), loops are probably best practice.

Example use:

<lang>   game 100
Guess my integer, which is bounded by 1 and 100
Guess: 64
Too high.
Guess: 32
Too low.
Guess: 48
Too high.
Guess: 40
Too low.
Guess: 44
You win.
```



## Java


```Java
import java.util.Random;
import java.util.Scanner;
public class Main
{
    public static void main(String[] args)
    {
        Scanner scan = new Scanner(System.in);
        Random random = new Random();
        long from = 1;
        long to = 100;
        int randomNumber = random.nextInt(to - from + 1) + from;
        int guessedNumber = 0;

        System.out.printf("The number is between %d and %d.\n", from, to);

        do
        {
            System.out.print("Guess what the number is: ");
            guessedNumber = scan.nextInt();
            if (guessedNumber > randomNumber)
                System.out.println("Your guess is too high!");
            else if (guessedNumber < randomNumber)
                System.out.println("Your guess is too low!");
            else
                System.out.println("You got it!");
        } while (guessedNumber != randomNumber);
    }
}
```

Demonstration:

```txt
The number is between 1 and 100.
Guess what the number is: 50
Your guess is too high!
Guess what the number is: 25
Your guess is too high!
Guess what the number is: 17
Your guess is too high!
Guess what the number is: 10
Your guess is too high!
Guess what the number is: 5
Your guess is too low!
Guess what the number is: 7
You got it!
```



## JavaScript


```html4strict><p>Pick a number between 1 and 100.</p

<form id="guessNumber">
    <input type="text" name="guess">
    <input type="submit" value="Submit Guess">
</form>
<p id="output"></p>
<script type="text/javascript">
```


```javascript
var number = Math.ceil(Math.random() * 100);
 
function verify() {
    var guess = Number(this.elements.guess.value),
        output = document.getElementById('output');
 
    if (isNaN(guess)) {
        output.innerHTML = 'Enter a number.';
    } else if (number === guess) {
        output.innerHTML = 'You guessed right!';
    } else if (guess > 100) {
        output.innerHTML = 'Your guess is out of the 1 to 100 range.';
    } else if (guess > number) {
        output.innerHTML = 'Your guess is too high.';
    } else if (guess < number) {
        output.innerHTML = 'Your guess is too low.';
    }
    return false;
}

document.getElementById('guessNumber').onsubmit = verify;
```


```html4strict></script></lang



###  Spidermonkey Version 


```javascript
#!/usr/bin/env js

function main() {
    guessTheNumber(1, 100);
}

function guessTheNumber(low, high) {
    var num = randOnRange(low, high);
    var guessCount = 0;
    
    function checkGuess(n) {
        if (n < low || n > high) {
            print('That number is not between ' + low + ' and ' + high + '!');
            return false;
        }

        if (n == num) {
            print('You got it in ' + String(guessCount) + ' tries.');
            return true;
        }

        if (n < num) {
            print('Too low.');
        } else {
            print('Too high.');
        }
        return false;
    }

    print('I have picked a number between ' + low + ' and ' + high + '. Try to guess it!');
    while (true) {
        guessCount++;
        putstr("  Your guess: ");
        var n = parseInt(readline());
        if (checkGuess(n)) break;
    }
}

function randOnRange(low, high) {
    var r = Math.random();
    return Math.floor(r * (high - low + 1)) + low;
}

main();

```


Example session:
 I have picked a number between 1 and 100. Try to guess it!
   Your guess: 50
 Too low.
   Your guess: 75
 Too high.
   Your guess: 62
 Too high.
   Your guess: 56
 Too low.
   Your guess: 58
 Too high.
   Your guess: 57
 You got it in 6 tries.


## Julia

{{works with|Julia|0.6}}


```julia
function guesswithfeedback(n::Integer)
    number = rand(1:n)
    print("I choose a number between 1 and $n\nYour guess? ")
    while (guess = readline()) != dec(number)
        if all(isdigit, guess)
            print("Too ", parse(Int, guess) < number ? "small" : "big")
        else
            print("Enter an integer please")
        end
        print(", new guess? ")
    end
    println("You guessed right!")
end

guesswithfeedback(10)
```


{{out}}

```txt
I choose a number between 1 and 10
Your guess? 11
Too big, new guess? 1
Too small, new guess? 5 
Too small, new guess? 6
Too small, new guess? 7
You guessed right!
```



## Kotlin


```scala
// version 1.1.2

fun main(args: Array<String>) {
    val rand = java.util.Random()
    val n = 1 + rand.nextInt(20)
    var guess :Int
    println("Guess which number I've chosen in the range 1 to 20\n")
    while (true) {
        print(" Your guess : ")
        guess = readLine()!!.toInt()
        when (guess) {
            n              ->  { println("Correct, well guessed!") ; return }
            in n + 1 .. 20 ->    println("Your guess is higher than the chosen number, try again")
            in 1 .. n - 1  ->    println("Your guess is lower than the chosen number, try again")
            else           ->    println("Your guess is inappropriate, try again")
        }
    }
}
```

Sample inout/output
{{out}}

```txt

Guess which number I've chosen in the range 1 to 20

 Your guess : 10
Your guess is higher than the chosen number, try again
 Your guess : 5
Your guess is higher than the chosen number, try again
 Your guess : 3
Your guess is higher than the chosen number, try again
 Your guess : 2
Correct, well guessed!

```



## Lambdatalk


```Scheme

{def game

 {def game.rec                                  // recursive part
  {lambda {:n :l :h}
   {let { {:n :n} {:l :l} {:h :h}               // :n, :l, :h redefined
          {:g {round {/ {+ :l :h} 2}}}}         // :g is the middle
    {if {< :g :n}
     then {br}:g too low!
          {game.rec :n {+ :g 1} :h}             // do it again higher
     else {if {> :g :n}
     then {br}:g too high!
          {game.rec :n :l {- :g 1}}             // do it again lower
     else {br}{b :g Got it!} }} }}}             // bingo!

 {lambda {:n}
  {let { {:n :n}                                // :n redefined
         {:N {floor {* :n {random}}}}}          // compute a random number
   Find {b :N} between 0 and :n {game.rec :N 0 :n} 
}}} 

{game {pow 2 32}}       // 2**32 = 4294967296

```


Sample inout/output
{{out}}

```txt

Find 2037303041 between 0 and 4294967296 
2147483648 too high! 
1073741824 too low! 
1610612736 too low! 
1879048192 too low! 
2013265920 too low! 
2080374784 too high! 
2046820352 too high! 
2030043136 too low! 
2038431744 too high! 
2034237440 too low! 
2036334592 too low! 
2037383168 too high! 
2036858880 too low! 
2037121024 too low! 
2037252096 too low! 
2037317632 too high! 
2037284864 too low! 
2037301248 too low! 
2037309440 too high! 
2037305344 too high! 
2037303296 too high! 
2037302272 too low! 
2037302784 too low! 
2037303040 too low! 
2037303168 too high! 
2037303104 too high! 
2037303072 too high! 
2037303056 too high! 
2037303048 too high! 
2037303044 too high! 
2037303042 too high! 
2037303041 Got it!

```



## Lasso


```Lasso
#!/usr/bin/lasso9

local(
	lower	= integer_random(10, 1),
	higher	= integer_random(100, 20),
	number	= integer_random(#higher, #lower),
	status	= false,
	guess
)

// prompt for a number
stdout('Guess a number: ')

while(not #status) => {
	#guess = null

	// the following bits wait until the terminal gives you back a line of input
	while(not #guess or #guess -> size == 0) => {
		#guess = file_stdin -> readSomeBytes(1024, 1000)
	}
	#guess = integer(#guess)

	if(not (range(#guess, #lower, #higher) == #guess)) => {
		stdout('Input not of correct type or range. Guess a number: ')
	else(#guess > #number)
		stdout('That was to high, try again! ')
	else(#guess < #number)
		stdout('That was to low, try again! ')
	else(#guess == #number)
		stdout('Well guessed!')
		#status = true
	}
}
```


With range value 8 and 73. Correct number 13

Output:

```txt
Guess a number: 70
That was to high, try again! 35
That was to high, try again! 14
That was to high, try again! 7
Input not of correct type or range. Guess a number: 10
That was to low, try again! 12
That was to low, try again! 13
Well guessed!

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
        (if (> answer guessed) (: io format '"Your guess is too low.~n"))
        (if (< answer guessed) (: io format '"Your guess is too high.~n"))
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
Your guess is too high.
Guess number: 1
Your guess is too low.
Guess number: 5
Your guess is too low.
Guess number: 7
Well-guessed!!
ok
>

```




## Liberty BASIC


```lb

[start]
    target = int( rnd( 1) * 100) +1

while 1
    do
        input "Guess a whole number between 1 and 100. To finish, type 'exit' "; b$
        if b$ ="exit" then print "Thank you for playing!": end
        c = val( b$)
        ok =( c =int( c)) and ( c >=1) and ( c <=100)
        if ok =0 then notice "Invalid data. Integers 1 to 100 only."
    loop until ok <>0

    if c =target       then print "      You guessed correctly.": print: goto [start]
    if c <target       then print "   Your guess was too low."
    if c >target       then print "   Your guess was too high."
wend

```

 
 


## LiveCode


```LiveCode
command guessTheNumber lowN highN
    local tNumber, tguess, tmin, tmax
    if lowN is empty or lowN < 1 then
        put 1 into tmin
    else
        put lowN into tmin
    end if
    if highN is empty then 
        put 10 into tmax
    else
        put highN into tmax
    end if
    put random(tmax - tmin + 1) + tmin - 1 into tNumber
    repeat until tguess is tNumber
        ask question "Please enter a number between" && tmin && "and" && tmax titled "Guess the number"
        if it is not empty then
            put it into tguess
            if tguess is tNumber then 
                answer "Well guessed!"
            else if tguess < tNumber then 
                answer "too low"
            else  
                answer "too high"
            end if
        else
            exit repeat
        end if
    end repeat
end guessTheNumber
```

Test

```LiveCode
command testGuessNumber
    guessTheNumber  --defaults to 1-10
    guessTheNumber 9,12
end testGuessNumber
```




## Locomotive Basic



```locobasic
10 CLS:RANDOMIZE TIME
20 PRINT "Please specify lower and upper limits":guess=0
30 INPUT "  (must be positive integers) :", first, last
40 IF first<1 OR last<1 THEN 20
50 num=INT(RND*(last-first+1)+first)
60 WHILE num<>guess
70 INPUT "Your guess? ", guess
80 IF guess<num THEN PRINT "too small!"
90 IF guess>num THEN PRINT "too large!"
100 WEND
110 INPUT "That's correct! Another game (y/n)? ", yn$
120 IF yn$="y" THEN 20

```


Output:

[[File:Guess the number cpc.png]]


## Logo

{{trans|UNIX Shell}}

```logo
to guess [:max 100]
  local "number
  make "number random :max
  local "guesses
  make "guesses 0

  local "guess
  forever [
    (type [Guess my number! \(range 1 -\ ] :max "\):\ )
    make "guess first readlist
    ifelse (or (not numberp :guess) (lessp :guess 1) (greaterp :guess :max)) [
      print sentence [Guess must be a number between 1 and] (word :max ".)
    ] [
      make "guesses (:guesses + 1)
      ifelse lessp :guess :number [
        print [Too low!]
      ] [ifelse equalp :guess :number [
        (print [You got it in] :guesses "guesses!)
        stop
      ] [
        print [Too high!]
      ]]
    ]
  ]
end

```


Sample run:
```txt
? guess
Guess my number! (range 1 - 100): 50
Too low!
Guess my number! (range 1 - 100): 75
Too high!
Guess my number! (range 1 - 100): 67
Too low!
Guess my number! (range 1 - 100): 71
Too high!
Guess my number! (range 1 - 100): 69
You got it in 5 guesses!

```



## Lua



```Lua
math.randomseed(os.time())
me_win=false
my_number=math.random(1,10)
while me_win==false do
	print "Guess my number from 1 to 10:"
	your_number = io.stdin:read'*l'
	if type(tonumber(your_number))=="number" then
		your_number=tonumber(your_number)
		if your_number>10 or your_number<1 then
			print "Your number was not between 1 and 10, try again."
		elseif your_number>my_number then
			print "Your number is greater than mine, try again."
		elseif your_number<my_number then
			print "Your number is smaller than mine, try again."
		elseif your_number==my_number then
			print "That was correct."
			me_win=true
		end
	else
		print "Your input was not a number, try again."
	end
end

```



```txt

Output:

Guess my number from 1 to 10:
4
Your number is smaller than mine, try again.
Guess my number from 1 to 10:
8
Your number is greater than mine, try again.
Guess my number from 1 to 10:
5
That was correct.

```




## M2000 Interpreter

{{trans|BASIC256}}

```M2000 Interpreter

Module GuessNumber {
      Read Min, Max
      chosen = Random(Min, Max)
      print "guess a whole number between ";Min;" and ";Max
      do {
                  \\ we use guess so Input get integer value
                  \\ if we press enter without a number we get error
                  do {
                        \\ if we get error then we change line, checking the cursor position
                        If Pos>0 then Print
                        Try ok {
                              input "Enter your number " , guess%
                        }
                  } until ok
                  Select Case guess%
                  case  min to chosen-1
                        print "Sorry, your number was too low"
                  case chosen+1 to max
                        print "Sorry, your number was too high"
                  case chosen 
                        print "Well guessed!"
                  else case
                        print "That was an invalid number"
                  end select
      } until guess% = chosen
}
GuessNumber 5, 15

```


{{out}}
same as BASIC256


## Maple


```Maple
GuessANumber := proc(low, high)
    local number, input;
    randomize():
    printf( "Guess a number between %d and %d:\n:> ", low, high );
    number := rand(low..high)();
    do
        input := parse(readline());
        if input > number then
            printf("Too high, try again!\n:> ");
        elif input < number then
            printf("Too low, try again!\n:> ");
        else
            printf("Well guessed! The answer was %d.\n", number);
            break;
        end if;
    end do:
end proc:
```


```Maple
GuessANumber(2,5);
```


=={{header|Mathematica}} / {{header|Wolfram Language}}==

```mathematica
guessnumber[min_, max_] := 
 Module[{number = RandomInteger[{min, max}], guess}, 
  While[guess =!= number, 
   guess = Input[
     If[guess > number, "Too high.Guess again.", 
      "Too low.Guess again.", 
      "Guess a number between " <> ToString@min <> " and " <> 
       ToString@max <> "."]]]; 
  CreateDialog[{"Well guessed!", DefaultButton[]}]];
guessnumber[1, 10]
```


=={{header|MATLAB}} / {{header|Octave}}==
{{untested|Octave}}
Tested in MATLAB. Untested in Octave.

```MATLAB
function guess_a_number(low, high)

if nargin < 1 || ~isnumeric(low) || length(low) > 1 || isnan(low)
    low = 1;
end;
if nargin < 2 || ~isnumeric(high) || length(high) > 1 || isnan(high)
    high = low+9;
elseif low > high
    [low, high] = deal(high, low);
end

n = floor(rand(1)*(high-low+1))+low;
gs = input(sprintf('Guess an integer between %i and %i (inclusive): ', low, high), 's');
while gs    % No guess quits the game
    g = str2double(gs);
    if length(g) > 1 || isnan(g) || g < low || g > high
        gs = input('Invalid input, guess again: ', 's');
    elseif g < n
        gs = input('Too low, guess again: ', 's');
    elseif g > n
        gs = input('Too high, guess again: ', 's');
    else
        disp('Good job, you win!')
        gs = '';
    end
end
```


## MAXScript


```MAXScript

Range = [1,100]
randomNumber = (random Range.x Range.y) as integer
clearListener()
while true do
(
	userVal = getKBValue prompt:("Enter a number between "+(range[1] as integer) as string+" and "+(range[2] as integer) as string+": ")
	if userVal == randomNumber do (format "\nWell guessed!\n"; exit with OK)
	case of
	(
		(classOf userVal != classof randomNumber): (format "\nBad number!\n")
		(userVal > Range[2] or userVal < Range[1]): (format "\nNumber out of range\n")
		(userVal > randomNumber): (format "\nToo high!\n")
		(userVal < randomNumber): (format "\nToo low!\n")
	)
)

```


Output:

```MAXSCRIPT

Enter a number between 1 and 100:  5
Too low!
Enter a number between 1 and 100:  9.0
Bad number!
Enter a number between 1 and 100:  145
Number out of range
Enter a number between 1 and 100:  95
Too low!
Enter a number between 1 and 100:  99
Too high!
Enter a number between 1 and 100:  97
Well guessed!
OK

```



## Mirah


```mirah
def getInput:int
  s = System.console.readLine()
  Integer.parseInt(s)
end

number = int(Math.random() * 10 + 1)

puts "Guess the number between 1 and 10"

guessed = false

while !guessed do
  begin
    userNumber = getInput
    if userNumber == number
      guessed = true
      puts "You guessed it."
    elsif userNumber > number  
      puts "Too high."
    else
      puts "Too low."
    end
  rescue NumberFormatException => e
    puts "Please enter an integer."
  end
end
```


=={{header|Modula-2}}==

```modula2
MODULE guessf;

IMPORT  InOut, Random, NumConv, Strings;

VAR     number, guess           : CARDINAL;
        input                   : Strings.String;
        OK, Done                : BOOLEAN;

BEGIN
  number := Random.nr (1000);
  InOut.WriteString ("I have chosen a number below 1000; please try to guess it.");
  InOut.WriteLn;
  REPEAT
    REPEAT
      InOut.WriteString ("Enter your guess : ");        InOut.WriteBf;
      InOut.ReadString (input);
      NumConv.Str2Num (guess, 10, input, OK);
      IF  NOT OK  THEN
        InOut.WriteString (input);
        InOut.WriteString (" is not a valid number...");
        InOut.WriteLn
      END
    UNTIL  OK;
    InOut.WriteString ("Your guess is ");
    IF  number = guess  THEN
      Done := TRUE;
      InOut.WriteString ("spot on!")
    ELSE
      Done := FALSE;
      IF  guess > number  THEN
        InOut.WriteString ("too high.")
      ELSE
        InOut.WriteString ("too low.")
      END
    END;
    InOut.WriteLn
  UNTIL  Done;
  InOut.WriteString ("Thank you for playing; have a nice day!");
  InOut.WriteLn
END guessf.
```


```txt
I have chosen a number below 1000; please try to guess it.
Enter your guess : 500
Your guess is too low.
Enter your guess : 750
Your guess is too high.
Enter your guess : 625
Your guess is too high.
Enter your guess : kwak
kwak is not a valid number...
Enter your guess : 531
Your guess is spot on!
Thank you for playing; have a nice day!
```



## Nemerle


```Nemerle
using System;
using System.Console;

module GuessHints
{
    Main() : void
    {
        def     rand   = Random();
        def     secret = rand.Next(1, 101);
        mutable guess  = 0;
        
        def GetGuess() : int {Int32.Parse(ReadLine())} 
        
        WriteLine("Guess a number between 1 and 100:");
        
        do
        {
            guess = GetGuess();
            match(guess.CompareTo(secret))
            {
                |(-1) => WriteLine("Too low! Guess again:")
                |1    => WriteLine("Too high! Guess again:")
                |0    => WriteLine("Well guessed!")
            }
        } while (guess != secret)
    }
}
```



## NetRexx


```NetRexx
/* NetRexx */
options replace format comments java crossref symbols nobinary

parse arg lo hi .
if lo = '' | lo = '.' then lo = 1
if hi = '' | hi = '.' then hi = 100
if lo > hi then parse (hi lo) lo hi -- make sure lo is < hi
rg = Random()
tries = 0
guessThis = rg.nextInt(hi - lo) + lo

say
say 'Rules:  Guess a number between' lo 'and' hi
say '        Use QUIT or . to stop the game'
say '        Use TELL to get the game to tell you the answer.'
say
say 'Starting...'
say

loop label g_ forever
  say  'Try guessing a number between' lo 'and' hi
  parse ask guess .
  select
    when guess.upper = 'QUIT' | guess = '.' then do
      say 'You asked to leave the game.  Goodbye...'
      leave g_
      end
    when guess.upper = 'TELL' | guess = '.' then do
      say 'The number you were looking for is' guessThis
      end
    when \guess.datatype('w') then do
      say guess 'is not a whole number.  Try again.'
      end
    when guess = guessThis then do
      tries = tries + 1
      say 'Well guessed!' guess 'is the correct number.'
      say 'It took you' tries 'tries.'
      leave g_
      end
    when guess < lo then do
      tries = tries + 1
      say guess 'is below the lower limit of' lo
      end
    when guess > hi then do
      tries = tries + 1
      say guess 'is above the upper limit of' hi
      end
    when guess < guessThis then do
      tries = tries + 1
      say guess 'is too low. Try again.'
      end
    when guess > guessThis then do
      tries = tries + 1
      say guess 'is too high. Try again.'
      end
    otherwise do
      say guess 'is an unexpected value.'
      end
    end
  end g_

return

```



## NewLISP


```NewLISP
; guess-number-feedback.lsp
; oofoe 2012-01-19
; http://rosettacode.org/wiki/Guess_the_number/With_feedback

(seed (time-of-day)) ; Initialize random number generator from clock.
(setq low  -5
      high 62 
      number (+ low (rand (- high low)))
      found nil
)

(print "I'm thinking of a number between " low " and " high ".")

(while (not found)
  (print " What's your guess? ")
  (setq guess (int (read-line) 'bad))
  (print (cond ((= 'bad guess) "That's not a number! Try again!")
               ((or (< guess low) (> guess high))
                (string "My number is between " low
                        " and " high ". Try again!"))
               ((< number guess) "Try a little lower...")
               ((> number guess) "Maybe a bit higher...")
               ((= number guess) (setq found true) "Exactly right!")))
  )

(println "\nWell guessed! Congratulations!")

(exit)
```


Sample output:


```txt
I'm thinking of a number between -5 and 62. What's your guess? No idea.
That's not a number! Try again! What's your guess? 99
My number is between -5 and 62. Try again! What's your guess? -5
Maybe a bit higher... What's your guess? 30
Try a little lower... What's your guess? 15
Try a little lower... What's your guess? 0
Maybe a bit higher... What's your guess? 3
Try a little lower... What's your guess? 1
Exactly right!
Well guessed! Congratulations!


```



## Nim


```nim
import random, rdstdin, strutils

randomize()

let iRange = 1..100

echo "Guess my target number that is between ", iRange.a, " and ", iRange.b, " (inclusive)."
let target = random(iRange)
var answer, i = 0
while answer != target:
  inc i
  let txt = readLineFromStdin("Your guess " & $i & ": ")
  try: answer = parseInt(txt)
  except ValueError:
    echo "  I don't understand your input of '", txt, "'"
    continue
  if answer < iRange.a or answer > iRange.b: echo "  Out of range!"
  elif answer < target: echo "  Too low."
  elif answer > target: echo "  Too high."
  else: echo "  Ye-Haw!!"

echo "Thanks for playing."
```

Output:

```txt
Guess my target number that is between 1 and 100 (inclusive).
Your guess 1: 50
  Too high.
Your guess 2: 300
  Out of range!
Your guess 3: -10
  Out of range!
Your guess 4: foo
  I don't understand your input of 'foo'
Your guess 5: 35
  Too low.
Your guess 6: 42
  Too high.
Your guess 7: 38
  Too low.
Your guess 8: 40
  Too low.
Your guess 9: 41
  Ye-Haw!!
Thanks for playing.
```



## Objeck


```objeck
use IO;

bundle Default {
   class GuessNumber {
      function : Main(args : String[]) ~ Nil {
         Guess();
      }
      function : native : Guess() ~ Nil {
         done := false;
         "Guess the number which is between 1 and 10 or 'n' to quite: "->PrintLine();
         rand_num := (Float->Random() * 10.0)->As(Int) + 1;
         while(done = false) {
            guess := Console->ReadString();
            number := guess->ToInt();
            if(number <> 0) {
               if(number <> rand_num) {
                  Console->Print("Your guess was too ")
                     ->Print(number < rand_num ? "low" : "high")
                     ->Print(".\nGuess again: ");
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



## OCaml


```ocaml
let rec _read_int() =
  try read_int()
  with _ ->
    print_endline "Please give a cardinal numbers.";
    (* TODO: what is the correct word? cipher, digit, figure or numeral? *)
    _read_int() ;;

let () =
  print_endline "Please give a set limits (two integers):";
  let a = _read_int()
  and b = _read_int() in
  let a, b =
    if a < b
    then (a, b)
    else (b, a)
  in
  Random.self_init();
  let target = a + Random.int (b - a) in
  Printf.printf "I have choosen a number between %d and %d\n%!" a b;
  print_endline "Please guess it!";
  let rec loop () =
    let guess = _read_int() in
    if guess = target then
    begin
      print_endline "The guess was equal to the target.\nCongratulation!";
      exit 0
    end;
    if guess < a || guess > b then
      print_endline "The input was inappropriate."
    else if guess > target then
      print_endline "The guess was higher than the target."
    else if guess < target then
      print_endline "The guess was less than the target.";
    loop ()
  in
  loop ()
```


Playing the game:

```txt
$ ocaml inapropriate.ml
Please give a set limits (two integers):
3
7
I have choosen a number between 3 and 7
Please guess it!
6
The guess was higher than the target.
7
The guess was higher than the target.
8
The input was inappropriate.
3
The guess was less than the target.
4
The guess was equal to the target.
Congratulation!
```



```txt

$ ocaml inapropriate.ml
Please give a set limits (two integers):
2
6
I have choosen a number between 2 and 6
Please guess it!
three
Please give a cardinal numbers.

```



## Octave


```Octave
function guess_a_number(low,high)
% Guess a number (with feedback)
% http://rosettacode.org/wiki/Guess_the_number/With_feedback

if nargin<1,
	low=1;
end; 
if nargin<2,
	high=10;
end; 

n = floor(rand(1)*(high-low+1))+low;
[guess,state] = str2num(input(sprintf('Guess a number between %i and %i: ',low,high),'s'));
while (~state || guess~=n)
	if guess < n,
		g = input('to low, guess again: ','s');
		[guess, state] = str2num(g); 
	elseif guess > n,
		g = input('to high, guess again: ','s');
		[guess, state] = str2num(g); 
	end; 
	while ~state	
		g = input('invalid input, guess again: ','s');
		[guess, state] = str2num(g); 
	end
end
disp('Well guessed!')
```



## Oforth



```Oforth
import: console

: guessNumber(a, b)
| n g |
   b a - rand a + 1- ->n
   begin
      "Guess a number between" . a . "and" . b . ":" .
      while(System.Console askln asInteger dup -> g isNull) [ "Not a number " println ]
      g n == ifTrue: [ "You found it !" .cr return ]
      g n <  ifTrue: [ "Less" ] else: [ "Greater" ] . "than the target" .cr
   again ;
```



## Ol


```ol

(import (otus random!))

(define from 0)
(define to 100)

(define number (+ from (rand! (+ from to 1))))

(let loop ()
   (for-each display `("Pick a number from " ,from " through " ,to ": "))
   (let ((guess (read)))
      (cond
         ((not (number? guess))
            (print "Not a number!")
            (loop))
         ((or (< guess from)
              (< to guess))
            (print "Out of range!")
            (loop))
         ((< guess number)
            (print "Too low!")
            (loop))
         ((> guess number)
            (print "Too high!")
            (loop))
         ((= guess number)
            (print "Well guessed!")))))

```



## PARI/GP


```parigp
guess_the_number(N=10)={
	a=random(N);
	print("guess the number between 0 and "N);
	for(x=1,N,
		if(x>1,
			if(b>a,
				print("guess again lower")
			,
				print("guess again higher")
		);
		b=input();
		if(b==a,break())
	);
	print("You guessed it correctly")
};
```



## ooRexx

While the program for REXX works perfectly well with ooRexx, here is a version 
written in an alternate (my) style.
 Select instead of a series of If's
 simple comparison instead of strict
 different indentations.
 entering ? shows the number we are looking for
This program should, of course, also work with all other Rexxes

```ooRexx

/*REXX program that plays the guessing (the number) game. */
 low=1                /*lower range for the guessing game.*/
high=100              /*upper range for the guessing game.*/
try=0                 /*number of valid attempts.         */
r=random(1,100)       /*get a random number (low-->high). */

do forever
  say
  say "guess the number, it's between" low 'and' high '(inclusive)',
      'or enter quit to end the game.'
  say
  pull g
  say
  g=space(g)
  Select
    When g='' then iterate
    When g='QUIT' then exit
    When g='?' then Do
      Say 'The number you are looking for is' r
      Iterate
      End
    When \datatype(g,'W') then do
      call ser g "isn't a valid number"
      iterate
      end
    When g<low then do
      call ser g 'is below the lower limit of' low
      iterate
      end
    When g>high then do
      call ser g 'is above the higher limit of' high
      iterate
      end
    When g=r then do
      try=try+1
      Leave
      End
    Otherwise Do
      try=try+1
      if g>r then what='high'
             else what='low'
      say 'your guess of' g 'is too' what'.'
      end
    end
  end

say
tries='tries'
if try=1 then
  say 'Congratulations!, you guessed the number in 1 try. Did you cheat?'
Else
  say 'Congratulations!, you guessed the number in' try 'tries.'
say
exit

ser: say; say '*** error ! ***'; say arg(1); say; return

```


## Pascal

See [[Guess_the_number#Delphi | Delphi]]


## Perl


```Perl
sub prompt {
	my $prompt = shift;
	while (1) {
		print "\n", $prompt, ": ";
		# type ^D, q, quit, quagmire, etc to quit
		defined($_ = <STDIN>) and !/^\s*q/ or exit;

		return $_ if /^\s*\d+\s*$/s;
		$prompt = "Please give a non-negative integer";
	}
}

my $tgt = int(rand prompt("Hola! Please tell me the upper bound") + 1);
my $tries = 1;

$tries++, print "You guessed too ", ($_ == -1 ? "high" : "low"), ".\n"
	while ($_ = $tgt <=> prompt "Your guess");

print "Correct! You guessed it after $tries tries.\n";
```



## Perl 6



```perl6
my $maxnum = prompt("Hello, please give me an upper boundary: ");
until 0 < $maxnum < Inf {
    say "Oops! The upper boundary should be > 0 and not Inf";
    $maxnum = prompt("Please give me a valid upper boundary: ");
}

my $count = 0;
my $number = (1..$maxnum).pick;

say "I'm thinking of a number from 1 to $maxnum, try to guess it!";
repeat until my $guessed-right {
    given prompt("Your guess: ") {
        when /^[e|q]/ { say 'Goodbye.'; exit; }
        when not 1 <= $_ <= $maxnum {
           say "You really should give me a number from 1 to $maxnum."
        }
        $count++;
        when $number { $guessed-right = True }
        when $number < $_ { say "Sorry, my number is smaller." }
        when $number > $_ { say "Sorry, my number is bigger." }
    }
}
say "Great you guessed right after $count attempts!";
```



```txt
Hello, please give me an upper boundary: 10 
I'm thinking of a number from 1 to 10, try to guess it!
Your guess: 5
Sorry, my number is bigger.
Your guess: 7
Sorry, my number is smaller.
Your guess: 6
Great you guessed right after 3 attempts!
```



## Phix


```Phix
constant lower_limit = 0, upper_limit = 100
integer secret = rand(upper_limit-(lower_limit-1))+lower_limit-1
printf(1,"Guess the number between %d and %d: ", lower_limit & upper_limit)
while 1 do
    integer guess = prompt_number("", lower_limit & upper_limit)
    if guess=secret then exit end if
    printf(1,"Your guess is too %s.\nTry again: ",{iff(guess>secret?"high":"low")})
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


if($_POST["guess"]){
    $guess  = htmlspecialchars($_POST['guess']);

	echo $guess . "<br />";
    if ($guess < $number)
	{ 
        echo "Your guess is too low";
    }
	elseif($guess > $number)
	{
        echo "Your guess is too high";
    }
	elseif($guess == $number)
	{
        echo "You got the correct number!";
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
		    <label for="guess">Guess A Number:</label><br/ >
		    <input type="text" name="guess" />
		    <input name="number" type="hidden" value="<?= $number ?>" />
		    <input name="submit" type="submit" />
		</form>
	</body>
</html>

```



## PicoLisp

{{trans|PureBasic}}

```PicoLisp
(de guessTheNumber ()
   (use (Low High Guess)
      (until
         (and
            (prin "Enter low limit : ")
            (setq Low (read))
            (prin "Enter high limit: ")
            (setq High (read))
            (> High Low) ) )
      (seed (time))
      (let Number (rand Low High)
         (loop
            (prin "Guess what number I have: ")
            (T (= Number (setq Guess (read)))
               (prinl "You got it!") )
            (prinl
               "Your guess is too "
               (if (> Number Guess) "low" "high")
               "." ) ) ) ) )
```

Output:

```txt
: (guessTheNumber)
Enter low limit : 1
Enter high limit: 64
Guess what number I have: 32
Your guess is too high.
Guess what number I have: 16
Your guess is too low.
Guess what number I have: 24
You got it!
```




## PowerShell


```PowerShell

function Get-Guess
{
    [int]$number = 1..100 | Get-Random
    [int]$guess = 0
    [int[]]$guesses = @()

    Write-Host "Guess a number between 1 and 100" -ForegroundColor Cyan

    while ($guess -ne $number)
    {
        try
        {
            [int]$guess = Read-Host -Prompt "Guess"

            if ($guess -lt $number)
            {
                Write-Host "Greater than..."
            }
            elseif ($guess -gt $number)
            {
                Write-Host "Less than..."
            }
            else
            {
                Write-Host "You guessed it"
            }
        }
        catch [Exception]
        {
            Write-Host "Input a number between 1 and 100." -ForegroundColor Yellow
            continue
        }

        $guesses += $guess
    }

    [PSCustomObject]@{
        Number  = $number
        Guesses = $guesses
    }
}

$answer = Get-Guess

Write-Host ("The number was {0} and it took {1} guesses to find it." -f $answer.Number, $answer.Guesses.Count)

```

{{Out}}

```txt

Guess a number between 1 and 100
Guess: 50
Greater than...
Guess: 60
Greater than...
Guess: a
Input a number between 1 and 100.
Guess: 70
Greater than...
Guess: 80
Less than...
Guess: 75
Less than...
Guess: 73
You guessed it
The number was 73 and it took 6 guesses to find it.

```



## PureBasic


```PureBasic
OpenConsole()

Repeat
  ; Ask for limits, with sanity check
  Print("Enter low limit : "): low  =Val(Input())
  Print("Enter high limit: "): High =Val(Input())
Until High>low

TheNumber=Random(High-low)+low  
Debug TheNumber
Repeat
  Print("Guess what number I have: "): Guess=Val(Input())
  If Guess=TheNumber
    PrintN("You got it!"): Break
  ElseIf Guess < TheNumber
    PrintN("Your guess is to low.")
  Else
    PrintN("Your guess is to high.")
  EndIf
ForEver
```



## Prolog


{{works with|SWI-Prolog|6}}


```prolog
main :-
    play_guess_number.


/* Parameteres */

low(1).
high(10).


/* Basic Game Logic */

play_guess_number :-
    low(Low),
    high(High),
    random(Low, High, N),
    tell_range(Low, High),
    repeat,                         % roughly, "repeat ... (until) Guess == N "
        ask_for_guess(Guess),
        give_feedback(N, Guess),
    Guess == N.

/* IO Stuff */

tell_range(Low, High) :-
    format('Guess an integer between ~d and ~d.~n', [Low,High]).

ask_for_guess(Guess) :-
    format('Guess the number: '),
    read(Guess).

give_feedback(N, Guess) :-
    ( \+integer(Guess) -> writeln('Invalid input.')
    ; Guess < N        -> writeln('Your guess is too low.')
    ; Guess > N        -> writeln('Your guess is too high.')
    ; Guess =:= N      -> writeln("Correct!")
    ).
```



Input in the standard Prolog top level is terminated with a `.`: E.g.,


```prolog
?- main.
Guess an integer between 1 and 10.
Guess the number: a.
Invalid input.
Guess the number: 3.
Your guess is too low.
```



## Python


```python
import random

inclusive_range = (1, 100)

print("Guess my target number that is between %i and %i (inclusive).\n"
      % inclusive_range)
target = random.randint(*inclusive_range)
answer, i = None, 0
while answer != target:
    i += 1
    txt = input("Your guess(%i): " % i)
    try:
        answer = int(txt)
    except ValueError:
        print("  I don't understand your input of '%s' ?" % txt)
        continue
    if answer < inclusive_range[0] or answer > inclusive_range[1]:
        print("  Out of range!")
        continue
    if answer == target:
        print("  Ye-Haw!!")
        break
    if answer < target: print("  Too low.")
    if answer > target: print("  Too high.")

print("\nThanks for playing.")
```


'''Sample Game'''

```txt
Guess my target number that is between 1 and 100 (inclusive).

Your guess(1): 50
  Too high.
Your guess(2): 25
  Too low.
Your guess(3): 40
  Too high.
Your guess(4): 30
  Too low.
Your guess(5): 35
  Too high.
Your guess(6): 33
  Too high.
Your guess(7): 32
  Too high.
Your guess(8): 31
  Ye-Haw!!

Thanks for playing.
```


'''Sample trapped Errors'''

```txt
Guess my target number that is between 1 and 100 (inclusive).

Your guess(1): 0
  Out of range!
Your guess(2): 101
  Out of range!
Your guess(3): Howdy
  I don't understand your input of 'Howdy' ?
Your guess(4): 
```



## R


```R
GuessANumber <- function( low, high ) {
  print( sprintf("Guess a number between %d and %d until you get it right", low, high ) );
  X <- low:high;
  number <- sample( X, 1 );
  repeat {
    input <- as.numeric(readline());
    if (input > number) {
      print("Too high, try again"); }
    else if (input < number) {
      print("Too low, try again");}
    else {
      print("Correct!");
      break; }
  }
}
```



## Racket


```Racket
#lang racket

(define (guess-number min max)
  (define target (+ min (random (- max min -1))))
  (printf "I'm thinking of a number between ~a and ~a\n" min max)
  (let loop ([prompt "Your guess"])
    (printf "~a: " prompt)
    (flush-output)
    (define guess (read))
    (define response
      (cond [(not (exact-integer? guess)) "Please enter a valid integer"]
            [(< guess target)             "Too low"]
            [(> guess target)             "Too high"]
            [else #f]))
    (when response (printf "~a\n" response) (loop "Try again")))
  (printf "Well guessed!\n"))

(guess-number 1 100)

```



## Retro


```Retro
: high|low ( gn-g$ )
  over > [ "high" ] [ "low" ] if ;

: checkGuess ( gn-gf || f )
  2over = [ "You guessed correctly!\n" puts 2drop 0 ]
          [ high|low "Sorry, your guess was too %s.\nTry again.\n" puts -1 ] if ;

: think ( -n )
  random abs 100 mod 1+ ;

: guess ( - )
  "I'm thinking of a number between 1 and 100.\n" puts
  "Try to guess it!\n" puts
  think [ getToken toNumber checkGuess ] while
  "You got it!\n" puts ;

```



## REXX

To make the program more engaging, randomized words for the hint are used.

```rexx
/*REXX program plays guess the number  game with a human;  the computer picks the number*/
  low=  1                                        /*the lower range for the guessing game*/
 high=100                                        /* "  upper   "    "   "      "      " */
  try=  0                                        /*the number of valid (guess) attempts.*/
    r=random(1, 100)                             /*get a random number  (low ───► high).*/
 lows= 'too_low  too_small too_little below under underneath   too_puny'
highs= 'too_high too_big   too_much   above over  over_the_top too_huge'
  erm= '***error***'
 @gtn= "guess the number, it's between"
prompt=centre(@gtn   low     'and'     high     '(inclusive)  ───or───  Quit:', 79, "─")
                                                 /* [↓]  BY 0 ─── used to  LEAVE  a loop*/
  do ask=0  by 0;     say;      say prompt;      say;     pull g;     g=space(g);    say
    do validate=0  by 0
       select
       when g==''                 then iterate ask
       when abbrev('QUIT', g, 1)  then exit                              /*what a whoos.*/
       when words(g)\==1          then say erm    'too many numbers entered:'        g
       when \datatype(g, 'N')     then say erm g  "isn't numeric"
       when \datatype(g, 'W')     then say erm g  "isn't a whole number"
       when g<low                 then say erm g  'is below the lower limit of'     low
       when g>high                then say erm g  'is above the higher limit of'   high
       otherwise       leave  /*validate*/
       end   /*select*/
    iterate ask
    end      /*validate*/

  try=try+1
  if g=r  then leave
  if g>r  then what=word(highs, random(1, words(highs) ) )
          else what=word( lows, random(1, words( lows) ) )
  say 'your guess of'     g     "is"      translate(what'.', , "_")
  end        /*ask*/

if try==1 then say 'Gadzooks!!!       You guessed the number right away!'
          else say 'Congratulations!, you guessed the number in '     try     " tries."
                                                 /*stick a fork in it,  we're all done. */
```



## Ring


```ring
fr = 1 t0 = 10
while true
see "Hey There, 

### ==================

I'm thinking of a number between " + fr + " and " + t0 + ", Can you guess it??
Guess :> "
give x
n = nrandom(fr,t0)
if x = n see "

                  Congratulations :D

*****************************************************
     ** Your guess was right You Are Genius :D **
*****************************************************


"
exit
else
see "Oops its not true, you were just few steps" 
if x > n see " up :)" else see " down :)" ok 
see copy(nl,3) 
ok
end

func nRandom s,e
while true
d = random(e)
if d >= s return d ok
end
```



## Ruby

{{trans|Mirah}}

```ruby
number = rand(1..10)

puts "Guess the number between 1 and 10"

loop do
  begin
    user_number = Integer(gets)
    if user_number == number
      puts "You guessed it."
      break
    elsif user_number > number  
      puts "Too high."
    else
      puts "Too low."
    end
  rescue ArgumentError
    puts "Please enter an integer."
  end
end
```



## Rust

{{libheader|rand}}

```rust
use std::io::stdin;
use rand::{Rng, thread_rng};

extern crate rand;

const LOWEST: isize = 1;
const HIGHEST: isize = 100;

fn main() {
    let mut rng = thread_rng();

    loop {
        let number: isize = rng.gen_range(LOWEST, HIGHEST + 1);
        let mut num_guesses = 0;

        println!("I have chosen my number between {} and {}. You know what to do", LOWEST, HIGHEST);

        loop {
            num_guesses += 1;

            let mut line = String::new();
            let res = stdin().read_line(&mut line);
            let input: Option<isize> = res.ok().map_or(None, |_| line.trim().parse().ok());

            match input {
                None => println!("numbers only, please"),
                Some(n) if n == number => {
                    println!("you got it in {} tries!", num_guesses);
                    break;
                }
                Some(n) if n < number => println!("too low!"),
                Some(n) if n > number => println!("too high!"),
                Some(_) => println!("something went wrong")
            }
        }
    }
}
```


```txt
I have chosen my number between 0 and 100. You know what to do
50
too high!
25
too high!
12
too low!
18
too low!
21
too low!
23
you got it in 6 tries!
```



## Scala


```scala
import java.util.Random
import java.util.Scanner

val scan = new Scanner(System.in)
val random = new Random
val (from , to) = (1, 100)
val randomNumber = random.nextInt(to - from + 1) + from
var guessedNumber = 0
printf("The number is between %d and %d.\n", from, to)

do {
  print("Guess what the number is: ")
  guessedNumber = scan.nextInt
  if (guessedNumber > randomNumber) println("Your guess is too high!")
  else if (guessedNumber < randomNumber) println("Your guess is too low!")
  else println("You got it!")
} while (guessedNumber != randomNumber)
```


## Scheme

{{works with|Chicken Scheme}}
{{works with|Guile}}

```scheme
(define maximum 5)
(define minimum -5)
(define number (+ (random (- (+ maximum 1) minimum)) minimum))

(display "Pick a number from ") 
(display minimum)
(display " through ")
(display maximum)
(display ".\n> ")
(do ((guess (read) (read))) ((eq? guess number))
        (if (or (>= guess maximum) (< guess minimum))
                (display "Out of range!\n> ")
                (begin
                        (if (> guess number)
                                (display "Too high!\n> "))
                        (if (< guess number)
                                (display "Too low!\n> ")))))
(display "Correct!\n")
```



## Seed7


```seed7
$ include "seed7_05.s7i";
 
const integer: lower_limit is 0;
const integer: upper_limit is 100;
 
const proc: main is func
  local
    var integer: number is 0;
    var integer: guess is 0;
  begin
    number := rand(lower_limit, upper_limit);
    write("Guess the number between " <& lower_limit <& " and " <& upper_limit <& ": ");
    while succeeds(readln(guess)) and number <> guess do
      write("Your guess was too ");
      if number < guess then
        writeln("high.");
      else
        writeln("low.");
      end if;
      write("Try again: ");
    end while;
    if number = guess then
      writeln("You guessed correctly!");
    else
      writeln("You gave up!");
    end if;
  end func;
```



## Sidef

{{trans|Ruby}}

```ruby
var number = rand(1..10);
say "Guess the number between 1 and 10";

loop {
    given(var n = Sys.scanln("> ").to_i) {
        when (number)     { say "You guessed it."; break }
        case (n < number) { say "Too low" }
        default           { say "Too high" }
    }
}
```



## Small Basic


```Small Basic
number=Math.GetRandomNumber(10)
TextWindow.WriteLine("I just thought of a number between 1 and 10. What is it?")
While guess<>number
  guess=TextWindow.ReadNumber()
  If guess>number Then
    TextWindow.WriteLine("Lower number!")
  EndIf
  If guess<number Then
    TextWindow.WriteLine("Higher number!")
  EndIf
EndWhile
TextWindow.WriteLine("You win!")
```



## Sparkling


```sparkling
printf("Lower bound: ");
let lowerBound = toint(getline());

printf("Upper bound: ");
let upperBound = toint(getline());

assert(upperBound > lowerBound, "upper bound must be greater than lower bound");

seed(time());
let n = floor(random() * (upperBound - lowerBound) + lowerBound);
var guess;

print();

while true {
    printf("Your guess: ");
    guess = toint(getline());
    
    if guess < n {
        print("too low");
    } else if guess > n {
        print("too high");
    } else {
        print("You guessed it!");
        break;
    }
}
```


## Swift


```Swift
import Cocoa

var found = false

let randomNum = Int(arc4random_uniform(100) + 1)

println("Guess a number between 1 and 100\n")

while (!found) {
    var fh = NSFileHandle.fileHandleWithStandardInput()
    
    println("Enter a number: ")
    let data = fh.availableData
    let str = NSString(data: data, encoding: NSUTF8StringEncoding)
    if (str?.integerValue == randomNum) {
        found = true
        println("Well guessed!")
    } else if (str?.integerValue < randomNum) {
        println("Good try but the number is more than that!")
    } else if (str?.integerValue > randomNum) {
        println("Good try but the number is less than that!")
    }
}
```



## Tcl


```tcl
set from 1
set to 10
set target [expr {int(rand()*($to-$from+1) + $from)}]
puts "I have thought of a number from $from to $to."
puts "Try to guess it!"
while 1 {
    puts -nonewline "Enter your guess: "
    flush stdout
    gets stdin guess
    if {![string is int -strict $guess] || $guess < $from || $guess > $to} {
	puts "Your guess should be an integer from $from to $to (inclusive)."
    } elseif {$guess > $target} {
	puts "Your guess was too high. Try again!"
    } elseif {$guess < $target} {
	puts "Your guess was too low. Try again!"
    } else {
	puts "Well done! You guessed it."
	break
    }
}
```

Sample output:

```txt

I have thought of a number from 1 to 10.
Try to guess it!

Enter your guess: 2
Your guess was too low. Try again!
Enter your guess: skfg
Your guess should be an integer from 1 to 10 (inclusive).
Enter your guess: 9
Your guess was too high. Try again!
Enter your guess: 5
Your guess was too low. Try again!
Enter your guess: 7
Your guess was too high. Try again!
Enter your guess: 6
Well done! You guessed it.

```


## TUSCRIPT


```tuscript

$$ MODE TUSCRIPT
PRINT "Find the luckynumber (7 tries)!"
SET luckynumber=RANDOM NUMBERS (1,100,1)
LOOP round=1,7
SET message=CONCAT ("[",round,"] Please insert a number")
ASK $message: n=""
 IF (n!='digits') THEN
   PRINT "wrong insert: ",n," Please insert a digit"
 ELSEIF (n>100.or.n<1) THEN
   PRINT "wrong insert: ",n," Please insert a number between 1-100"
 ELSEIF (n==#luckynumber) THEN
   PRINT "BINGO"
   EXIT
 ELSEIF (n.gt.#luckynumber) THEN
   PRINT "too big"
 ELSEIF (n.lt.#luckynumber) THEN
   PRINT "too small"
ENDIF
IF (round==7) PRINT/ERROR "You've lost: luckynumber was: ",luckynumber
ENDLOOP

```

Output:

```txt

Find the luckynumber (7 tries)!
[1] Please insert a number >51
too small
[2] Please insert a number >76
too small
[3] Please insert a number >89
too big
[4] Please insert a number >80
too small
[5] Please insert a number >84
too small
[6] Please insert a number >86
too small
[7] Please insert a number >88
too big
@@@@@@@@  You've lost: luckynumber was: 87

```


{{omit from|GUISS}}


## UNIX Shell

{{works with|Bourne Again SHell}}
{{works with|Public Domain Korn SHell}}
{{works with|Z SHell}}

```sh
function guess {
  [[ -n $BASH_VERSION ]] && shopt -s extglob
  [[ -n $ZSH_VERSION ]] && set -o KSH_GLOB
  local -i max=${1:-100}
  local -i number=RANDOM%max+1
  local -i guesses=0

  local guess
  while true; do
    echo -n "Guess my number! (range 1 - $max): "
    read guess
    if [[ "$guess" != +([0-9]) ]] || (( guess < 1 || guess > max )); then
      echo "Guess must be a number between 1 and $max."
      continue
    fi
    let guesses+=1
    if (( guess < number )); then
      echo "Too low!"
    elif (( guess == number )); then
      echo "You got it in $guesses guesses!"
      break
    else
      echo "Too high!"
    fi
  done
}
```


Sample run:

```txt

$ guess
Guess my number! (range 1 - 100): 50
Too low!
Guess my number! (range 1 - 100): 75
Too high!
Guess my number! (range 1 - 100): 62
Too low!
Guess my number! (range 1 - 100): 69
Too low!
Guess my number! (range 1 - 100): 72
You got it in 5 guesses!

```



## Ursa

{{trans|Python}}

```ursa
decl int high low
set low 0
set high 100

out "Guess a number between " low " and " high "." endl endl console
decl int target answer i
decl ursa.util.random random
set target (int (+ 1 (+ low (random.getint (int (- high low))))))
while (not (= answer target))
	inc i
	out "Your guess(" i "): " console
	set answer (in int console)
	
	if (or (< answer low) (> answer high))
		out "  Out of range!" endl console
		continue
	end if
	if (= answer target)
		out "  Ye-Haw!!" endl console
		continue
	end if
	if (< answer target)
		out "  Too low." endl console
	end if
	if (> answer target)
		out "  Too high." endl console
	end if
end while

out endl "Thanks for playing." endl console
```



## Vala


```vala

void main(){
	const int from = 1;
	const int to = 100;

	int random = Random.int_range(from, to);	
	int guess = 0;

	while (guess != random){
		stdout.printf("Guess the target number that's between %d and %d.\n", from, to);

		string? num = stdin.read_line ();
		num.canon("0123456789", '!'); // replaces any character in num that's not in "0123456789" with "!"
		
		if ("!" in num)
			stdout.printf("Please enter a number!\n");

		else{		
			guess = int.parse(num);

			if (guess > random && guess <= to)
				stdout.printf("Too high!\n");
			if (guess < random && guess >= from)
				stdout.printf("Too low!\n");
			if (guess == random)
				stdout.printf("You guess it! You win!\n");
			if (guess < from || guess > to)
				stdout.printf("%d Your guess isn't even in the right range!\n", guess);
		}

	}//while
} // main

```


Shorter but no error checking

```vala
int main() {
        int guess, x = Random.int_range(1, 10);
        stdout.printf("Make a guess (1-10): ");
        while((guess = int.parse(stdin.read_line())) != x) {
                stdout.printf("%s! Try again: ", x < guess ? "Lower" : "Higher");
        }
        stdout.printf("Got it!\n");
        return 0;
}
```



## VBA Excel

The Application.InputBox display a message when input is inappropriate.

```vb
Sub GuessTheNumberWithFeedback()
Dim Nbc&, Nbp&, m&, n&, c&

    Randomize Timer
    m = 11
    n = 100
    Nbc = Int((Rnd * (n - m + 1)) + m)
    Do
        c = c + 1
        Nbp = Application.InputBox("Choose a number between " & m & " and " & n & " : ", "Enter your guess", Type:=1)
        Select Case Nbp
            Case Is > Nbc: MsgBox "Higher than target!"
            Case Is < Nbc: MsgBox "Less than target!"
            Case Else: Exit Do
        End Select
    Loop
    MsgBox "Well guessed!" & vbCrLf & "You find : " & Nbc & " in " & c & " guesses!"
End Sub
```



## VBScript


```vb

Dim max,min,secretnum,numtries,usernum
max=100
min=1
numtries=0
Randomize
secretnum = Int((max-min+1)*Rnd+min)

Do While usernum <> secretnum
  usernum = Inputbox("Guess the secret number beween 1-100","Guessing Game")
  If IsEmpty(usernum) Then
    WScript.Quit
  End If
  If IsNumeric(usernum) Then
    numtries = numtries + 1
    usernum = Cint(usernum)
    If usernum < secretnum Then
      Msgbox("The secret number is higher than " + CStr(usernum))
    ElseIf usernum > secretnum Then
      Msgbox("The secret number is lower than " + CStr(usernum))
    Else
      Msgbox("Congratulations, you found the secret number in " + CStr(numtries) + " guesses!")
    End If
  Else
    Msgbox("Please enter a valid number.")
  End If
Loop

```



## XLISP


```lisp
(defun guessing-game (a b)
    ; minimum and maximum, to be supplied by the user
    (defun prompt ()
        (display "What is your guess? ")
        (define guess (read))
        (if (eq guess n) ; EQ, unlike =, won't blow up
                         ; if GUESS isn't a number
            (display "Well guessed!")
            (begin
                (display
                    (cond
                        ((not (integer? guess)) "Come on, that isn't even an integer")
                        ((or (< guess a) (> guess b)) "Now you   k n o w   it won't be that")
                        ((< guess n) "Too low")
                        ((> guess n) "Too high")))
                (display "! Try again...")
                (newline)
                (prompt))))
    (define n (+ (random (- (+ b 1) a)) a))
    (display "I have thought of an integer between ")
    (display a)
    (display " and ")
    (display b)
    (display ". Try to guess it!")
    (newline)
    (prompt))
```

{{out}}

```txt
[1] (guessing-game 19 36)
I have thought of an integer between 19 and 36. Try to guess it!
What is your guess? 2
Now you   k n o w   it won't be that! Try again...
What is your guess? 25
Too high! Try again...
What is your guess? 24.5
Come on, that isn't even an integer! Try again...
What is your guess? n
Come on, that isn't even an integer! Try again...
What is your guess? 19
Too low! Try again...
What is your guess? 22
Too high! Try again...
What is your guess? 21
Too high! Try again...
What is your guess? 20
Well guessed!
```



## XPL0


```XPL0
include c:\cxpl\codes;
int Lo, Hi, C, Guess, Number;

[loop  [Text(0, "Low  limit: ");  Lo:= IntIn(0);
        Text(0, "High limit: ");  Hi:= IntIn(0);
        if Lo < Hi then quit;
        Text(0, "Low limit must be lower!^M^J^G");
       ];
Number:= Ran(Hi-Lo+1)+Lo;
Text(0, "I'm thinking of a number between ");
IntOut(0, Lo);  Text(0, " and ");  IntOut(0, Hi); Text(0, ".^M^J");
repeat  Text(0, "Can you guess the number? ");
        loop    [C:= ChIn(0);
                if C>=^0 & C<=^9 then quit;
                Text(0, "Please enter a number in the given range.^M^J");
                OpenI(0);
                ];
        Backup;  Guess:= IntIn(0);
        Text(0, if Guess = Number then "Correct!"
                else if Guess > Number then "Nope, too high."
                else "You're too low.");
        CrLf(0);
until   Guess = Number;
]
```


Example output:

```txt

Low  limit: 1
High limit: 10
I'm thinking of a number between 1 and 10.
Can you guess the number? maybe
Please enter a number in the given range.
3
You're too low.
Can you guess the number? 6
You're too low.
Can you guess the number? 8
Correct!

```



## zkl


```zkl
r:=(0).random(10)+1;
while(1){
   n:=ask("Num between 1 & 10: ");
   try{n=n.toInt()}catch{ println("Number please"); continue; }
   if(n==r){ println("Well guessed!"); break; }
   println((n<r) and "small" or "big");
}
```

{{out}}

```txt

Num between 1 & 10: foo
Number please
Num between 1 & 10: 5
big
Num between 1 & 10: 3
big
Num between 1 & 10: 2
big
Num between 1 & 10: 1
Well guessed!

```



## ZX Spectrum Basic


```zxbasic
ZX Spectrum Basic has no [[:Category:Conditional loops|conditional loop]] constructs, so we have to emulate them here using IF and GO TO.
1 LET n=INT (RND*10)+1
2 INPUT "Guess a number that is between 1 and 10: ",g: IF g=n THEN PRINT "That's my number!": STOP
3 IF g<n THEN PRINT "That guess is too low!": GO TO 2
4 IF g>n THEN PRINT "That guess is too high!": GO TO 2
```

