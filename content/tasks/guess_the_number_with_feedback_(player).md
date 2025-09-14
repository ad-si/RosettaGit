+++
title = "Guess the number/With feedback (player)"
description = ""
date = 2019-08-22T14:31:24Z
aliases = []
[extra]
id = 8636
[taxonomies]
categories = ["task", "Games"]
tags = []
languages = [
  "ada",
  "algol_68",
  "applescript",
  "autohotkey",
  "batch_file",
  "bbc_basic",
  "c",
  "ceylon",
  "clojure",
  "common_lisp",
  "cpp",
  "csharp",
  "d",
  "elixir",
  "erlang",
  "euphoria",
  "fantom",
  "fortran",
  "freebasic",
  "go",
  "haskell",
  "j",
  "java",
  "javascript",
  "julia",
  "kotlin",
  "lasso",
  "liberty_basic",
  "lua",
  "matlab",
  "maxscript",
  "nim",
  "pari_gp",
  "pascal",
  "perl",
  "perl_6",
  "phix",
  "picolisp",
  "prolog",
  "purebasic",
  "python",
  "racket",
  "related_tasks",
  "rexx",
  "ring",
  "ruby",
  "rust",
  "scala",
  "scheme",
  "seed7",
  "sidef",
  "smalltalk",
  "standard_ml",
  "swift",
  "tcl",
  "unix_shell",
  "vba",
  "xpl0",
  "zkl",
  "zx_spectrum_basic",
]
+++

## Task

:Task:
Write a player for the game that follows the following rules:
:The scorer will choose a number between set limits. The computer player will print a guess of the target number. The computer asks for a score of whether its guess is higher than, lower than, or equal to the target. The computer guesses, and the scorer scores, in turn, until the computer correctly guesses the target number.

The computer should guess intelligently based on the accumulated scores given. One way is to use a [[Binary search]] based algorithm.


## Related tasks

*   [[Guess the number/With Feedback]]
*   [[Bulls and cows/Player]]





## Ada



```Ada
with Ada.Text_IO;
procedure Guess_Number_Player is
   procedure Guess_Number (Lower_Limit : Integer; Upper_Limit : Integer) is
      type Feedback is (Lower, Higher, Correct);
      package Feedback_IO is new Ada.Text_IO.Enumeration_IO (Feedback);
      My_Guess : Integer := Lower_Limit + (Upper_Limit - Lower_Limit) / 2;
      Your_Feedback : Feedback;
   begin
      Ada.Text_IO.Put_Line ("Think of a number!");
      loop
         Ada.Text_IO.Put_Line ("My guess: " & Integer'Image (My_Guess));
         Ada.Text_IO.Put ("Your answer (lower, higher, correct): ");
         Feedback_IO.Get (Your_Feedback);
         exit when Your_Feedback = Correct;
         if Your_Feedback = Lower then
            My_Guess := Lower_Limit + (My_Guess - Lower_Limit) / 2;
         else
            My_Guess := My_Guess + (Upper_Limit - My_Guess) / 2;
         end if;
      end loop;
      Ada.Text_IO.Put_Line ("I guessed well!");
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
end Guess_Number_Player;
```



## ALGOL 68

```algol68
BEGIN
    INT    lower    :=    1;
    INT    upper    :=  100;
    print( ( "Think of a number between ", whole( lower, 0 ), " and ", whole( upper, 0 ), newline ) );
    print( ( "Please enter Y if I guess correctly, L is it is lower, G if it is greater or Q if you've had enough", newline ) );
    WHILE
        INT mid = lower + ( ( upper - lower ) OVER 2 );
        CHAR reply;
        WHILE
            print( ( "Is it ", whole( mid, 0 ), "?    Y/L/G/Q: " ) );
            read( ( reply, newline ) );
            NOT char in string( reply, NIL, "YLGQylgq" )
        DO SKIP OD;
        IF   reply = "Q" OR reply = "q" OR reply = "Y" OR reply = "y"
        THEN FALSE
        ELIF lower >= upper THEN
            print( ( "Based on your answers so far, it must be ", whole( lower, 0 ), newline ) );
            FALSE
        ELSE
            IF   reply = "L" OR reply = "l" THEN upper := mid - 1
            ELIF reply = "G" OR reply = "g" THEN lower := mid + 1
            FI;
            TRUE
        FI
    DO SKIP OD
END
```



## AppleScript



```AppleScript
-- defining the range of the number to be guessed
property minLimit : 1
property maxLimit : 100

on run
	-- ask the user to think of  a number in the given range
	display dialog "Please think of a number between " & minLimit & " and " & maxLimit

	-- prepare a variable for the lowest guessed value
	set lowGuess to minLimit
	-- prepare a variable for the highest guessed value
	set highGuess to maxLimit

	repeat
		-- guess a number inside the logical range
		set computersGuess to (random number from lowGuess to highGuess)
		-- ask the user to check my guess
		set guessResult to button returned of (display dialog "I guess " & computersGuess & "!" & return & "What do you think?" buttons {"Lower", "Correct", "Higher"})
		if guessResult = "Lower" then
			-- the number is less than the guess, switch the upper limit to the guess
			set highGuess to computersGuess
		else if guessResult = "Higher" then
			-- the number is greater than the guess, switch the lower limit to the guess
			set lowGuess to computersGuess
		else if guessResult = "Correct" then
			-- the computer guessed the number, beep and exit
			beep
			exit repeat
		end if
	end repeat
end run
```



## AutoHotkey


Works with the AutoHotkey entry at: [[Guess the number/With feedback]]


```AutoHotkey
MaxGuesses = 50

GetParams(LowerBound,UpperBound)
If Not GuessNum(LowerBound,UpperBound,MaxGuesses)
 MsgBox, 16, Error, Could not guess number within %MaxGuesses% guesses.

GetParams(ByRef LowerBound,ByRef UpperBound)
{
 WinWait, Number Guessing ahk_class #32770
 Sleep, 100
 WinGet, InputID, ID
 ControlGetText, Temp1, Static1, ahk_id %InputID%
 Temp2 := InStr(Temp1,A_Space,False,32)
 LowerBound := SubStr(Temp1,31,Temp2 - 31)
 UpperBound := SubStr(Temp1,Temp2 + 5,-1)
}

GuessNum(LowerBound,UpperBound,MaxGuesses)
{
 Loop, %MaxGuesses%
 {
  Guess := LowerBound + ((UpperBound - LowerBound) // 2)
  Temp1 := SendGuess(Guess)
  ToolTip % Temp1
  If Temp1 = Too Low
   LowerBound = %Guess%
  Else If Temp1 = Too High
   UpperBound = %Guess%
  Else
   Return, 1
 }
}

SendGuess(Guess)
{
 WinGet, InputID, ID, Number Guessing ahk_class #32770
 ControlSetText, Edit1, %Guess%, ahk_id %InputID%
 ControlSend, Button1, {Enter}, ahk_id %InputID%
 Loop
 {
  Sleep, 50
  IfWinExist, Correct ahk_class #32770
   Return
  Else IfWinExist, Incorrect ahk_class #32770
   Break
 }
 ControlGetText, Temp1, Static2
 WinClose
 WinWaitClose
 IfInString, Temp1, low
  Return, "Too Low"
 Else
  Return, "Too High"
}
```



## BBC BASIC


```bbcbasic
      min% = 1
      max% = 100
      PRINT "Think of a number between "; min% " and " ;max%
      PRINT "I will try to guess your number."
      REPEAT
        guess% = (min% + max%) DIV 2
        PRINT "My guess is " ; guess%
        INPUT "Is it higher than, lower than or equal to your number", answer$
        CASE LEFT$(answer$,1) OF
          WHEN "L","l": min% = guess% + 1
          WHEN "H","h": max% = guess% - 1
          WHEN "E","e": EXIT REPEAT
          OTHERWISE: PRINT "Sorry, I didn't understand your answer."
        ENDCASE
      UNTIL FALSE
      PRINT "Goodbye."
      END
```



## Batch File


```dos

@echo off

:: Player is prompted to give a number between %min% and %max%. If the input is out of those limits they are prompted to choose again
:choose
set min=0
set max=100

set /p "number=Choose a number [%min%-%max%]: "
if %number% gtr %max% goto choose
if %number% lss %min% goto choose
set attempts=0

:: Loops the guessing process until completed
:comp
set /a attempts+=1
set /a guess=(%max%-%min%)/2+%min%
choice /c "HLE" /n /m "Guess: %guess% - [H]igher, [L]ower or [E]qual"
if errorlevel 3 goto end
if errorlevel 2 (
  set max=%guess%
  goto comp
)
if errorlevel 1 (
  set min=%guess%
  goto comp
)

:end
echo Guesses: %attempts%
pause>nul

```

```txt

Choose a number [0-100]: 42
Guess: 50 - [H]igher, [L]ower or [E]qual L
Guess: 25 - [H]igher, [L]ower or [E]qual H
Guess: 37 - [H]igher, [L]ower or [E]qual H
Guess: 43 - [H]igher, [L]ower or [E]qual L
Guess: 40 - [H]igher, [L]ower or [E]qual H
Guess: 41 - [H]igher, [L]ower or [E]qual H
Guess: 42 - [H]igher, [L]ower or [E]qual E
Guesses: 7

```



## C


```c
#include <stdio.h>

int main(){
  int bounds[ 2 ] = {1, 100};
  char input[ 2 ] = "  ";
    /* second char is for the newline from hitting [return] */
  int choice = (bounds[ 0 ] + bounds[ 1 ]) / 2;
    /* using a binary search */

  printf( "Choose a number between %d and %d.\n", bounds[ 0 ], bounds[ 1 ] );

  do{
    switch( input[ 0 ] ){
      case 'H':
        bounds[ 1 ] = choice;
        break;
      case 'L':
        bounds[ 0 ] = choice;
        break;
      case 'Y':
        printf( "\nAwwwright\n" );
        return 0;
    }
    choice = (bounds[ 0 ] + bounds[ 1 ]) / 2;

    printf( "Is the number %d? (Y/H/L) ", choice );
  }while( scanf( "%1s", input ) == 1 );

  return 0;
}
```


Demonstration (number is 57):

```txt
Choose a number between 1 and 100.
Is the number 50? (Y/H/L) L
Is the number 75? (Y/H/L) H
Is the number 62? (Y/H/L) H
Is the number 56? (Y/H/L) L
Is the number 59? (Y/H/L) H
Is the number 57? (Y/H/L) Y

Awwwright
```


----

The following is a hacky solution using <tt>bsearch()</tt> and pointers to represent integers. Although the pointers do not point to valid things, <tt>bsearch()</tt> doesn't actually dereference the pointers or care what they point to; it just passes them to the comparator and searches the space of pointers. We can use that to search any space of integers.
```c
#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>

enum {
  LOWER = 0,
  UPPER = 100,
  KEY = LOWER-1 // some value that is not in the valid range
};

char dummy;
// A pointer to represent the integer 0, and the basis of our integer-as-pointer
// representation. We can't use the null pointer because bsearch() returns that
// for not found.
#define ZERO ((void *)&dummy)

int get_value(int x) {
  if (x == KEY)
    return 0;
  printf("My guess is: %d. Is it too high, too low, or correct? (H/L/C) ", x);
  char input[2] = " ";
  scanf("%1s", input);
  switch (tolower(input[0])) {
    case 'l':
      return -1;
    case 'h':
      return 1;
    case 'c':
      return 0;
  }
  fprintf(stderr, "Invalid input\n");
  exit(1);
  return 0;
}

int my_cmp(const void *x, const void *y) {
  return get_value(x - ZERO) - get_value(y - ZERO);
}

int main() {
  printf("Instructions:\n"
	 "Think of integer number from %d (inclusive) to %d (exclusive) and\n"
	 "I will guess it. After each guess, you respond with L, H, or C depending\n"
	 "on if my guess was too low, too high, or correct.\n",
	 LOWER, UPPER);
  void *result = bsearch(ZERO + KEY, ZERO + LOWER, UPPER-LOWER, 1, my_cmp);
  if (result == NULL)
    fprintf(stderr, "That is impossible.\n");
  else
    printf("Your number is %d.\n", (int)(result - ZERO));
  return 0;
}
```



## C++

A clever solution that takes advantage of C++'s built-in binary search function <code>lower_bound()</code>. Instead of searching a slice of a container, we search a range of numbers by implementing a specially-designed custom iterator.
```cpp
#include <iostream>
#include <algorithm>
#include <string>
#include <iterator>

struct GuessNumberIterator : std::iterator<std::random_access_iterator_tag, int> {
  int i;
  GuessNumberIterator() { }
  GuessNumberIterator(int _i) : i(_i) { }
  GuessNumberIterator& operator++() { ++i; return *this; }
  GuessNumberIterator operator++(int) {
    GuessNumberIterator tmp = *this; ++(*this); return tmp; }
  bool operator==(const GuessNumberIterator& y) { return i == y.i; }
  bool operator!=(const GuessNumberIterator& y) { return i != y.i; }
  int operator*() {
    std::cout << "Is your number less than or equal to " << i << "? ";
    std::string s;
    std::cin >> s;
    return (s != "" && (s[0] == 'y' || s[0] == 'Y')) ? 0 : -1;
  }
  GuessNumberIterator& operator--() { --i; return *this; }
  GuessNumberIterator operator--(int) {
    GuessNumberIterator tmp = *this; --(*this); return tmp; }
  GuessNumberIterator& operator+=(int n) { i += n; return *this; }
  GuessNumberIterator& operator-=(int n) { i -= n; return *this; }
  GuessNumberIterator operator+(int n) {
    GuessNumberIterator tmp = *this; return tmp += n; }
  GuessNumberIterator operator-(int n) {
    GuessNumberIterator tmp = *this; return tmp -= n; }
  int operator-(const GuessNumberIterator &y) { return i - y.i; }
  int operator[](int n) { return *(*this + n); }
  bool operator<(const GuessNumberIterator &y) { return i < y.i; }
  bool operator>(const GuessNumberIterator &y) { return i > y.i; }
  bool operator<=(const GuessNumberIterator &y) { return i <= y.i; }
  bool operator>=(const GuessNumberIterator &y) { return i >= y.i; }
};
inline GuessNumberIterator operator+(int n, GuessNumberIterator &i) { return i + n; }

const int lower = 0;
const int upper = 100;

int main() {
  std::cout << "Instructions:\n"
	    << "Think of integer number from " << lower << " (inclusive) to "
	    << upper << " (exclusive) and\n"
	    << "I will guess it. After each guess, I will ask you if it is less than\n"
	    << "or equal to some number, and you will respond with \"yes\" or \"no\".\n";
  int answer = std::lower_bound(GuessNumberIterator(lower), GuessNumberIterator(upper), 0).i;
  std::cout << "Your number is " << answer << ".\n";
  return 0;
}
```



## C#


```c#
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading; //Remember to add this if you want the game to pause in RealisticGuess.Start()

namespace ConsoleApplication1
{
    class RealisticGuess //Simulates a guessing game between two people. Guessing efficiency is not a goal.
    {
        private int max;
        private int min;
        private int guess;

        public void Start()
        {
            Console.Clear();
            string input;

            try
            {
                Console.WriteLine("Please enter the lower boundary");
                input = Console.ReadLine();
                min = Convert.ToInt32(input);
                Console.WriteLine("Please enter the upper boundary");
                input = Console.ReadLine();
                max = Convert.ToInt32(input);
            }
            catch (FormatException)
            {
                Console.WriteLine("The entry you have made is invalid. Please make sure your entry is an integer and try again.");
                Console.ReadKey(true);
                Start();
            }
            Console.WriteLine("Think of a number between {0} and {1}.", min, max);
            Thread.Sleep(2500);
            Console.WriteLine("Ready?");
            Console.WriteLine("Press any key to begin.");
            Console.ReadKey(true);
            Guess(min, max);
        }
        public void Guess(int min, int max)
        {
            int counter = 1;
            string userAnswer;
            bool correct = false;
            Random rand = new Random();

            while (correct == false)
            {
                guess = rand.Next(min, max);
                Console.Clear();
                Console.WriteLine("{0}", guess);
                Console.WriteLine("Is this number correct? {Y/N}");
                userAnswer = Console.ReadLine();
                if (userAnswer != "y" && userAnswer != "Y" && userAnswer != "n" && userAnswer != "N")
                {
                    Console.WriteLine("Your entry is invalid. Please enter either 'Y' or 'N'");
                    Console.WriteLine("Is the number correct? {Y/N}");
                    userAnswer = Console.ReadLine();
                }
                if (userAnswer == "y" || userAnswer == "Y")
                {
                    correct = true;
                }
                if (userAnswer == "n" || userAnswer == "N")
                {
                    counter++;
                    if (max == min)
                    {
                        Console.WriteLine("Error: Range Intersect. Press enter to restart the game.");  //This message should never pop up if the user enters good data.
                        Console.ReadKey(true);                                                          //It handles the game-breaking exception that occurs
                        Guess(1, 101);                                                                  //when the max guess number is the same as the min number.
                    }
                    Console.WriteLine("Is the number you're thinking of lower or higher? {L/H}");
                    userAnswer = Console.ReadLine();
                    if (userAnswer != "l" && userAnswer != "L" && userAnswer != "h" && userAnswer != "H")
                    {
                        Console.WriteLine("Your entry is invalid. Please enter either 'L' or 'H'");
                        Console.WriteLine("Is the number you're thinking of lower or higher? {L/H}");
                        userAnswer = Console.ReadLine();
                    }
                    if (userAnswer == "l" || userAnswer == "L")
                    {
                        max = guess;
                    }
                    if (userAnswer == "h" || userAnswer == "H")
                    {
                        min = guess;
                    }
                }
            }
            if (correct == true)
            {
                EndAndLoop(counter);
            }
        }

        public void EndAndLoop(int iterations)
        {
            string userChoice;
            bool loop = false;
            Console.WriteLine("Game over. It took {0} guesses to find the number.", iterations);
            while (loop == false)
            {
                Console.WriteLine("Would you like to play again? {Y/N}");
                userChoice = Console.ReadLine();
                if (userChoice != "Y" && userChoice != "y" && userChoice != "N" && userChoice != "n")
                {
                    Console.WriteLine("Sorry, your input is invalid. Please answer 'Y' to play again, or 'N' to quit.");
                }
                if (userChoice == "Y" || userChoice == "y")
                {
                    Start();
                }
                if (userChoice == "N" || userChoice == "n")
                {
                    Environment.Exit(1);
                }
            }
        }
    }
    class Program
    {
        static void Main(string[] args)
        {
            Console.Title = "Random Number";
            RealisticGuess game = new RealisticGuess();
            game.Start();
        }
    }
}

```



## Ceylon


```ceylon
shared void run() {
    while(true) {
        variable value low = 1;
        variable value high = 10;
        variable value attempts = 1;
        print("Please choose a number between ``low`` and ``high``.
               Press enter when ready.");
        process.readLine();
        while(true) {
            if(low > high) {
                print("Something is wrong. I give up.");
                break;
            }
            variable value guess = (low + high) / 2;
            print("Is ``guess`` (e)qual, (h)igher or (l)ower to your number?
                   (enter q to quit)");
            value answer = process.readLine()?.trimmed?.lowercased;
            switch(answer)
            case("e") {
                print("I got it in only ``attempts`` ``attempts == 1 then "try" else "tries"``!");
                break;
            }
            case("h") {
                high = guess - 1;
                attempts++;
            }
            case("l") {
                low = guess + 1;
                attempts++;
            }
            case("q") {
                return;
            }
            else {
                print("Please enter an e, h, l or q");
            }
        }
    }
}
```



## Clojure



```Clojure
(require '[clojure.string :as str])

(defn guess-game [low high]
  (printf "Think of a number between %s and %s.\n (use (h)igh (l)ow (c)orrect)\n" low high)
  (loop [guess (/ (inc (- high low)) 2)
         [step & more] (next (iterate #(/ % 2) guess))]
    (printf "I guess %s\n=> " (Math/round (float guess)))
    (flush)
    (case (first (str/lower-case (read)))
      \h (recur (- guess step) more)
      \l (recur (+ guess step) more)
      \c (println "Huzzah!")
      (do (println "Invalid input.")
          (recur guess step)))))
```

Output

```txt
user=> (guess-game 1 100)
Think of a number between 1 and 100.
 (use (h)igh (l)ow (c)orrect)
I guess 50
=> l
I guess 63
=> h
I guess 56
=> l
I guess 59
=> l
I guess 61
=> c
Huzzah!

```



## Common Lisp

An imperative solution using LOOP:

```Lisp

(defun guess-the-number (&optional (max 1000) (min 0))
  (flet ((get-feedback (guess)
           (loop
              initially (format t "I choose ~a.~%" guess)
              for answer = (read)
              if (member answer '(greater lower correct))
              return answer
              else do (write-line "Answer greater, lower, or correct."))))
    (loop
       initially (format t "Think of a number between ~a and ~a.~%" min max)
       for guess  = (floor (+ min max) 2)
       for answer = (get-feedback guess)
       until (eq answer 'correct)
       if (eq answer 'greater) do (setf min guess)
       else do (setf max guess)
       finally (write-line "I got it!"))))

```


A recursive solution (we use LABELS instead of FLET for the local function definitions in this version so that GUESS-LOOP will be able to call itself recursively):

```Lisp

(defun guess-the-number (&optional (max 1000) (min 0))
  (labels ((guess-loop (min max)
             (let ((guess (floor (+ min max) 2)))
               (format t "I choose ~a.~%" guess)
               (case (read)
                 (greater (guess-loop guess max))
                 (lower   (guess-loop min guess))
                 (correct (write-line "I got it!"))
                 (otherwise
                   (write-line "Please answer greater, lower, or correct.")
                   (guess-loop min max))))))
    (format t "Think of a number between ~a and ~a.~%" min max)
    (guess-loop min max)))

```



## D

```d
import std.stdio, std.string;

void main() {
    immutable mnOrig = 1, mxOrig = 10;
    int mn = mnOrig, mx = mxOrig;

    writefln(
    "Think of a number between %d and %d and wait for me to guess it.
    On every guess of mine you should state whether the guess was
    too high, too low, or equal to your number by typing h, l, or =",
            mn, mx);

    LOOP: for (int i = 1; ; i++) {
        immutable guess = (mn + mx) / 2;
        writef("Guess %2d is: %2d. The score for which is (h,l,=): ",
               i, guess);
        immutable string txt = readln().strip().toLower();

        switch (txt) {
            case "h":
                mx = guess - 1;
                break;
            case "l":
                mn = guess + 1;
                break;
            case "=":
                writeln("  Yeehaw!!");
                break LOOP;
            default:
                writefln("  I don't understand your input '%s'.",
                         txt);
                continue LOOP;
        }

        if (mn > mx || mn < mnOrig || mx > mxOrig) {
            writeln("Please check your scoring as" ~
                    " I cannot find the value");
            break;
        }
    }
    writeln("\nThanks for keeping score.");
}
```

```txt
Think of a number between 1 and 10 and wait for me to guess it.
    On every guess of mine you should state whether the guess was
    too high, too low, or equal to your number by typing h, l, or =
Guess  1 is:  5. The score for which is (h,l,=): l
Guess  2 is:  8. The score for which is (h,l,=): h
Guess  3 is:  6. The score for which is (h,l,=): l
Guess  4 is:  7. The score for which is (h,l,=): =
  Yeehaw!!

Thanks for keeping score.
```



## Elixir

```Elixir
defmodule Game do
  def guess(a..b) do
    x = Enum.random(a..b)
    guess(x, a..b, div(a+b, 2))
  end

  defp guess(x, a.._b, guess) when x < guess do
    IO.puts "Is it #{guess}? Too High."
    guess(x, a..guess-1, div(a+guess, 2))
  end
  defp guess(x, _a..b, guess) when x > guess do
    IO.puts "Is it #{guess}? Too Low."
    guess(x, guess+1..b, div(guess+b+1, 2))
  end
  defp guess(x, _, _) do
    IO.puts "Is it #{x}?"
    IO.puts " So the number is: #{x}"
  end
end
Game.guess(1..100)
```


```txt

Is it 50? Too High.
Is it 25? Too High.
Is it 13? Too High.
Is it 7? Too Low.
Is it 10?
 So the number is: 10

```



## Erlang


```erlang
% Implemented by Arjun Sunel
-module(guess_game).
-export([main/0]).

main() ->
	L = 1 ,	 	% Lower Limit
	U = 100, 	  % Upper Limit

	io:fwrite("Player 1 : Guess my number between ~p and ", [L]),
	io:fwrite("and ~p until you get it right.\n", [U]),
	N = random:uniform(100),
	guess(L,U,N).

guess(L,U,N) ->
	K = (L+U) div 2,
	io:format("Player 2 : Number guessed : ~p~n",[K]),
	if
		K=:=N ->
			io:format("Well guessed!! by Player 2\n");
		true ->
			if
				K > N ->
					io:format("Player 1 : Your guess is too high!\n"),
					guess(L,K,N);
				true ->
					io:format("Player 1 : Your guess is too low!\n"),
					guess(K,U,N)
			end
	end.
```

```txt
Player 1 : Guess my number between 1 and and 100 until you get it right.
Player 2 : Number guessed : 50
Player 1 : Your guess is too high!
Player 2 : Number guessed : 25
Player 1 : Your guess is too high!
Player 2 : Number guessed : 13
Player 1 : Your guess is too high!
Player 2 : Number guessed : 7
Player 1 : Your guess is too low!
Player 2 : Number guessed : 10
Well guessed!! by Player 2
ok
```



## Euphoria

```euphoria
include get.e
include wildcard.e

sequence Respons
integer min, max, Guess
min = 0
max = 100

printf(1,"Think of a number between %d and %d.\n",{min,max})
puts(1,"On every guess of mine you should state whether my guess was\n")
puts(1,"too high, too low, or equal to your number by typing 'h', 'l', or '='\n")

while 1 do
    if max < min then
        puts(1,"I think something is strange here...\n")
        exit
    end if
    Guess = floor((max-min)/2+min)
    printf(1,"My guess is %d, is this correct? ", Guess)
    Respons = upper(prompt_string(""))
    if Respons[1] = 'H' then
        max = Guess-1
    elsif Respons[1] = 'L' then
        min = Guess+1
    elsif Respons[1] = '=' then
        puts(1,"I did it!\n")
        exit
    else
        puts(1,"I do not understand that...\n")
    end if
end while
```



## Fantom



```fantom

class Main
{
  public static Void main ()
  {
    Int lowerLimit := 1
    Int higherLimit := 100

    echo ("Think of a number between 1 and 100")
    echo ("Press 'enter' when ready")
    Env.cur.in.readLine

    while (true)
    {
      if (higherLimit < lowerLimit)
      { // check that player is not cheating!
        echo ("Something has gone wrong ... I give up")
        break
      }
      myGuess := (higherLimit + lowerLimit) / 2
      echo ("My guess is $myGuess")
      echo ("Enter 'H' if your number is higher, 'L' if lower, or 'E' if equal")
      switch (Env.cur.in.readLine.trim.upper)
      {
        case "E":
          echo ("I got it correct - thankyou!")
          break // game over
        case "H":
          lowerLimit = myGuess + 1
        case "L":
          higherLimit = myGuess - 1
        default:
          echo ("Pardon? Let's try that again")
      }
    }
  }
}

```


Example:

```txt
Think of a number between 1 and 100
Press 'enter' when ready

My guess is 50
Enter 'H' if your number is higher, 'L' if lower, or 'E' if equal
a
Pardon? Let's try that again
My guess is 50
Enter 'H' if your number is higher, 'L' if lower, or 'E' if equal
l
My guess is 25
Enter 'H' if your number is higher, 'L' if lower, or 'E' if equal
h
My guess is 37
Enter 'H' if your number is higher, 'L' if lower, or 'E' if equal
h
My guess is 43
Enter 'H' if your number is higher, 'L' if lower, or 'E' if equal
h
My guess is 46
Enter 'H' if your number is higher, 'L' if lower, or 'E' if equal
h
My guess is 48
Enter 'H' if your number is higher, 'L' if lower, or 'E' if equal
l
My guess is 47
Enter 'H' if your number is higher, 'L' if lower, or 'E' if equal
e
I got it correct - thankyou!

```



## Fortran

```fortran
program Guess_a_number_Player
  implicit none

  integer, parameter :: limit = 100
  integer :: guess, mx = limit, mn = 1
  real :: rnum
  character(1) :: score

  write(*, "(a, i0, a)") "Think of a number between 1 and ", limit, &
                         " and I will try to guess it."
  write(*, "(a)")  "You score my guess by entering: h if my guess is higher than that number"
  write(*, "(a)")  "                                l if my guess is lower than that number"
  write(*, "(a/)") "                                c if my guess is the same as that number"

  call random_seed
  call random_number(rnum)
  guess = rnum * limit + 1
  do
    write(*, "(a, i0, a,)", advance='no') "My quess is: ", guess, "   Score(h, l or c)?: "
    read*, score
    select case(score)
      case("l", "L")
        mn = guess
        guess = (mx-guess+1) / 2 + mn

      case("h", "H")
        mx = guess
        guess = mx - (guess-mn+1) / 2

      case("c", "C")
        write(*, "(a)") "I solved it!"
        exit

      case default
        write(*, "(a)") "I did not understand that"
    end select
  end do
end program
```

Output

```txt
Think of a number between 1 and 100 and I will try to guess it.
You score my guess by entering: h if my guess is higher than that number
                                l if my guess is lower than that number
                                c if my guess is the same as that number

My guess is: 58   Score(h, l or c)?: h
My guess is: 29   Score(h, l or c)?: l
My guess is: 44   Score(h, l or c)?: l
My guess is: 51   Score(h, l or c)?: l
My guess is: 55   Score(h, l or c)?: l
My guess is: 57   Score(h, l or c)?: c
I solved it!
```



## FreeBASIC


```freebasic
' FB 1.05.0 Win64

Dim hle As String
Dim lowest As Integer = 1
Dim highest As Integer = 20
Dim guess As Integer = 10
Print "Please choose a number between 1 and 20 but don't tell me what it is yet"
Print
Do
  Print "My guess is"; guess
  Do
    Input "Is this higher/lower or equal to your chosen number h/l/e : "; hle
    hle = LCase(hle)
    If hle = "l" AndAlso guess = highest Then
      Print "It can't be more than"; highest; ", try again"
      hle = "i" '' invalid
    ElseIf hle = "h" AndAlso guess = lowest Then
      Print "It can't be less than"; lowest; ", try again"
      hle = "i"
    End If
  Loop Until hle = "h" OrElse hle = "l" OrElse hle = "e"
  If hle = "e" Then
    Print "Good, thanks for playing the gaame with me!"
    Exit Do
  ElseIf hle = "h" Then
    If highest > guess - 1 Then highest = guess - 1
  Else
    If lowest < guess + 1  Then lowest = guess + 1
  End If
  guess = (lowest + highest)\2
Loop
End
```


Sample input/output
```txt

Please choose a number between 1 and 20 but don't tell me what it is yet

My guess is 10
Is this higher/lower or equal to your chosen number h/l/e : ? l
My guess is 15
Is this higher/lower or equal to your chosen number h/l/e : ? h
My guess is 12
Is this higher/lower or equal to your chosen number h/l/e : ? h
My guess is 11
Is this higher/lower or equal to your chosen number h/l/e : ? e
Good, thanks for playing the gaame with me!

```



## Go

Go's binary search function (<code>sort.Search()</code>) is general enough to be able to do this type of task, as mentioned in the documentation for the function itself.[http://golang.org/pkg/sort/#Search]

```go
package main

import (
    "fmt"
    "sort"
)

func main() {
    lower, upper := 0, 100
    fmt.Printf(`Instructions:
Think of integer number from %d (inclusive) to %d (exclusive) and
I will guess it. After each guess, I will ask you if it is less than
or equal to some number, and you will respond with "yes" or "no".
`, lower, upper)
    answer := sort.Search(upper-lower, func (i int) bool {
        fmt.Printf("Is your number less than or equal to %d? ", lower+i)
        s := ""
        fmt.Scanf("%s", &s)
        return s != "" && s[0] == 'y'
    })
    fmt.Printf("Your number is %d.\n", lower+answer)
}
```


Manual solution:

```go
package main

import (
    "bufio"
    "fmt"
    "os"
)

func main() {
    lower, upper := 1, 100
    fmt.Printf(`Instructions:
Think of integer number from %d (inclusive) to %d (inclusive) and I will guess it.
After each guess, you respond with l,h,or c depending on
if my guess was too low, too high, or correct.
Press enter when you are thinking of a number. `, lower, upper)
    in := bufio.NewReader(os.Stdin)
    in.ReadString('\n')
    for {
        guess := (upper+lower)/2
        fmt.Printf("My guess: %d (l/h/c) ", guess)
        s, err := in.ReadString('\n')
        if err != nil {
            fmt.Println("\nSo, bye.")
            return
        }
        switch s {
        case "l\n":
            lower = guess + 1
        case "h\n":
            upper = guess - 1
        case "c\n":
            fmt.Println("I did it. :)")
            return
        default:
            fmt.Println("Please respond by pressing l, h, or c")
        }
    }
}
```




## Haskell


Explicit version with arbitrary range:

```Haskell

main :: IO ()
main = do
    putStrLn "Please enter the range:"
    putStr   "From: "
    from <- getLine
    putStr   "To: "
    to   <- getLine
    case (from, to) of
         (_) | [(from', "")] <- reads from
             , [(to'  , "")] <- reads to
             , from'         < to' -> loop from' to'
         (_)  -> putStrLn "Invalid input." >> main

loop :: Integer -> Integer -> IO ()
loop from to = do
    let guess = (to + from) `div` 2
    putStrLn $ "Is it " ++ show guess ++ "? ((l)ower, (c)orrect, (h)igher)"
    answer <- getLine
    case answer of
        "c" -> putStrLn "Awesome!"
        "l" -> loop from  guess
        "h" -> loop guess to
        (_) -> putStrLn "Invalid answer." >> loop from to

```


Short version with set limits:

```Haskell

main = f 0 100
  where f x y = let g = div (x + y) 2 in
          putStrLn (show g ++ "? (l,h,c)") >>
          getLine >>= \a -> case a of
                              "l" -> f x g
                              "h" -> f g y
                              "c" -> putStrLn "Yay!"

```



=={{header|Icon}} and {{header|Unicon}}==
```Icon

procedure main ()
  lower_limit := 1
  higher_limit := 100

  write ("Think of a number between 1 and 100")
  write ("Press 'enter' when ready")
  read ()

  repeat {
    if (higher_limit < lower_limit)
      then { # check that player is not cheating!
        write ("Something has gone wrong ... I give up")
        exit ()
      }
    my_guess := (higher_limit + lower_limit) / 2
    write ("My guess is ", my_guess)
    write ("Enter 'H' if your number is higher, 'L' if lower, or 'E' if equal")
    reply := map(trim(read ()))
    case (reply) of {
      "e" : {
        write ("I got it correct - thankyou!")
        exit () # game over
      }
      "h" : lower_limit := my_guess + 1
      "l" : higher_limit := my_guess - 1
      default : write ("Pardon? Let's try that again")
    }
  }
end

```


=={{header|IS-BASIC}}==
<lang IS-BASIC>100 PROGRAM "GuessIt.bas"
110 LET N=100
120 TEXT 80
130 PRINT "Choose a number between 1 and;" N:PRINT "I will start guess the number."
140 LET BL=1:LET UL=100:LET NR=0
150 DO
160   LET GUESS=INT((BL+UL)/2):LET NR=NR+1
170   SET #102:INK 3:PRINT :PRINT "My";NR;". guess: ";GUESS:SET #102:INK 1
180   LET ANSWER=QUESTION
190   SELECT CASE ANSWER
200   CASE 1
210     LET UL=GUESS-1
220   CASE 2
230     LET BL=GUESS+1
240   CASE ELSE
250   END SELECT
260   IF BL>UL THEN PRINT "You are cheating!":LET ANSWER=9
270 LOOP UNTIL ANSWER=0 OR ANSWER=9
280 PRINT "So the number is:" GUESS
290 DEF QUESTION
300   PRINT "Your number:  1 - Is lower?;  2 - Is higher?;  0 - Is equal?"
310   DO
320     LET K$=INKEY$
330   LOOP UNTIL K$>="0" AND K$<="3"
340   LET QUESTION=VAL(K$)
350 END DEF
```



## J



```j
require 'misc'
guess=:3 :0
  'lo hi'=.y
  while.lo < hi do.
    smoutput 'guessing a number between ',(":lo),' and ',":hi
    guess=.lo+?hi-lo
    select.{.deb tolower prompt 'is it ',(":guess),'? '
      case.'y'do. smoutput 'Win!' return.
      case.'l'do. lo=.guess+1
      case.'h'do. hi=.guess-1
      case.'q'do. smoutput 'giving up' return.
      case.   do. smouput 'options: yes, low, high, quit'
    end.
  end.
)
```


Example session:

<lang>   guess 1 100
guessing a number between 1 and 100
is it 86? hi
guessing a number between 1 and 85
is it 56? hi
guessing a number between 1 and 55
is it 24? lo
guessing a number between 25 and 55
is it 29? lo
guessing a number between 30 and 55
is it 43? lo
guessing a number between 44 and 55
is it 53? hi
guessing a number between 44 and 52
is it 51? hi
guessing a number between 44 and 50
is it 48? lo
guessing a number between 49 and 50
is it 49? lo
50
```


This could be made more efficient by replacing <code>guess=.lo+?hi-lo</code> with <code>guess=.<.-:lo+hi</code>.  (The above example would have finished on the first guess, but the range 1..100 would never take more than 6 guesses -- the remaining answers would be determined after six guesses.)


## Java

A clever solution that uses the built-in binary search functions with a virtual list.
```java5
import java.util.AbstractList;
import java.util.Collections;
import java.util.Scanner;

public class GuessNumber {
    public static final int LOWER = 0, UPPER = 100;
    public static void main(String[] args) {
	System.out.printf("Instructions:\n" +
			  "Think of integer number from %d (inclusive) to %d (exclusive) and\n" +
			  "I will guess it. After each guess, you respond with L, H, or C depending\n" +
			  "on if my guess was too low, too high, or correct.\n",
			  LOWER, UPPER);
	int result = Collections.binarySearch(new AbstractList<Integer>() {
		private final Scanner in = new Scanner(System.in);
		public int size() { return UPPER - LOWER; }
		public Integer get(int i) {
		    System.out.printf("My guess is: %d. Is it too high, too low, or correct? (H/L/C) ", LOWER+i);
		    String s = in.nextLine();
		    assert s.length() > 0;
		    switch (Character.toLowerCase(s.charAt(0))) {
		    case 'l':
			return -1;
		    case 'h':
			return 1;
		    case 'c':
			return 0;
		    }
		    return -1;
		}
	    }, 0);
	if (result < 0)
	    System.out.println("That is impossible.");
	else
	    System.out.printf("Your number is %d.\n", result);
    }
}
```



## JavaScript


###  Spidermonkey version


A boring solution that does nothing clever or inscrutable.

Well, it does use recursion for the guessing function, but that's OK because the depth is bounded by LOG<sub>2</sub> of the range. That's not true if the user changes his number, but then he gets what he deserves.


```javascript
#!/usr/bin/env js

var DONE = RIGHT = 0, HIGH = 1, LOW = -1;

function main() {
    showInstructions();
    while (guess(1, 100) !== DONE);
}

function guess(low, high) {
    if (low > high) {
        print("I can't guess it. Perhaps you changed your number.");
        return DONE;
    }

    var g = Math.floor((low + high) / 2);
    var result = getResult(g);
    switch (result) {
        case RIGHT:
            return DONE;
        case LOW:
            return guess(g + 1, high);
        case HIGH:
            return guess(low, g - 1);
    }
}

function getResult(g) {
    while(true) {
        putstr('Is it ' + g + '? ');
        var ans = readline().toUpperCase().replace(/^\s+/, '') + ' ';
        switch (ans[0]) {
            case 'R':
                print('I got it! Thanks for the game.');
                return RIGHT;
            case 'L':
                return LOW;
            case 'H':
                return HIGH;
            default:
                print('Please tell me if I am "high", "low" or "right".');
        }
    }
}

function showInstructions() {
    print('Think of a number between 1 and 100 and I will try to guess it.');
    print('After I guess, type "low", "high" or "right", and then press enter.');
    putstr("When you've thought of a number press enter.");
    readline();
}

main();

```


An example session:
 Think of a number between 1 and 100 and I will try to guess it.
 After I guess, type "low", "high" or "right", and then press enter.
 When you've thought of a number press enter.
 Is it 50? high
 Is it 25? high
 Is it 12? low
 Is it 18? high
 Is it 15? high
 Is it 13? right
 I got it! Thanks for the game.

Another example session:
 Think of a number between 1 and 100 and I will try to guess it.
 After I guess, type "low", "high" or "right", and then press enter.
 When you've thought of a number press enter.
 Is it 50? n
 Please tell me if I am "high", "low" or "right".
 Is it 50? h
 Is it 25? h
 Is it 12? h
 Is it 6? h
 Is it 3? h
 Is it 1? h
 I can't guess it. Perhaps you changed your number.


## Julia


This version uses the expression <math>\left\lceil-\log\left(\frac{1}{\mathrm{upper} - \mathrm{lower}}\right) / \log(2)\right\rceil</math> to calculate the maximum number of guesses it will need.


```Julia
print("Enter an upper bound: ")
lower = 0
input = readline()
upper = parse(Int, input)

if upper < 1
    throw(DomainError)
end

attempts = 1
print("Think of a number, ", lower, "--", upper, ", then press ENTER.")
readline()
const maxattempts = round(Int, ceil(-log(1 / (upper - lower)) / log(2)))
println("I will need at most ", maxattempts, " attempts ",
    "(⌈-log(1 / (", upper, " - ", lower, ")) / log(2)⌉ = ",
    maxattempts, ").\n")
previous = -1
guess = -1

while true
    previous = guess
    guess = lower + round(Int, (upper - lower) / 2, RoundNearestTiesUp)

    if guess == previous || attempts > maxattempts
        println("\nThis is impossible; did you forget your number?")
        exit()
    end

    print("I guess ", guess, ".\n[l]ower, [h]igher, or [c]orrect? ")
    input = chomp(readline())

    while input ∉ ["c", "l", "h"]
        print("Please enter one of \"c\", \"l\", or \"h\". ")
        input = chomp(readline())
    end

    if input == "l"
        upper = guess
    elseif input == "h"
        lower = guess
    else
        break
    end

    attempts += 1
end

println("\nI win after ", attempts, attempts == 1 ? " attempt." : " attempts.")
```



## Kotlin

```scala
// version 1.0.5-2

fun main(args: Array<String>) {
    var hle: Char
    var lowest  = 1
    var highest = 20
    var guess   = 10
    println("Please choose a number between 1 and 20 but don't tell me what it is yet\n")

    while (true) {
        println("My guess is $guess")

        do {
            print("Is this higher/lower than or equal to your chosen number h/l/e : ")
            hle = readLine()!!.first().toLowerCase()
            if (hle == 'l' && guess == highest) {
                println("It can't be more than $highest, try again")
                hle = 'i' // signifies invalid
            }
            else if (hle == 'h' && guess == lowest) {
                println("It can't be less than $lowest, try again")
                hle = 'i'
            }
        }
        while (hle !in "hle")

        when (hle) {
            'e' -> { println("Good, thanks for playing the game with me!") ; return }
            'h' ->   if (highest > guess - 1) highest = guess - 1
            'l' ->   if (lowest  < guess + 1) lowest  = guess + 1
        }

        guess = (lowest + highest) / 2
    }
}
```

Sample input/output:
```txt


<Please choose a number between 1 and 20 but don't tell me what it is yet

My guess is 10
Is this higher/lower or equal to your chosen number h/l/e : h
My guess is 5
Is this higher/lower or equal to your chosen number h/l/e : l
My guess is 7
Is this higher/lower or equal to your chosen number h/l/e : l
My guess is 8
Is this higher/lower or equal to your chosen number h/l/e : e
Good, thanks for playing the game with me!
/pre>


## Lasso


```Lasso
#!/usr/bin/lasso9

local(
	mini=0,
	maxi=100,
	status	= false,
	count = 0,
	response,
	guess
)

stdoutnl('Think of a number between ' + #mini + ' and ' + #maxi + '
Each time I guess indicate if I was to high (H), to low (L) or just right (R).')


while(not #status) => {

	if(not(#mini <= #maxi)) => {

		stdout('I think you are trying to cheat me. I will not play anymore.')
		#status = true

	else

		#guess = ((#maxi - #mini) /2 ) + #mini

		stdout('You are thinking on ' + #guess + ' ')
		#response = null

		// the following bits wait until the terminal gives you back a line of input
		while(not #response or #response -> size == 0) => {
			#response = file_stdin -> readSomeBytes(1024, 1000)
		}
		#response -> replace(bytes('\n'), bytes(''))

		match(string(#response)) => {
			case('L')
				#mini = #guess + 1
				#count++
			case('H')
				#maxi = #guess - 1
				#count++
			case('R')
				stdout('Am I smart or smart! I guessed it in ' + #count ' tries!')
				#status = true
			case()
				stdout('Are you having issues reading instructions? ')
		}
	}
}
```


Examples:

```txt
Think of a number between 0 and 100
Each time I guess indicate if I was to high (H), to low (L) or just right (R).
You are thinking on 50 l
You are thinking on 75 l
You are thinking on 88 h
You are thinking on 81 r
Am I smart or smart! I guessed it in 4 tries!

```


```txt
Think of a number between 0 and 100
Each time I guess indicate if I was to high (H), to low (L) or just right (R).
You are thinking on 50 k
Are you having issues reading instructions? You are thinking on 50 tr
Are you having issues reading instructions? You are thinking on 50 l
You are thinking on 75 l
You are thinking on 88 l
You are thinking on 94 l
You are thinking on 97 l
You are thinking on 99 h
You are thinking on 98 r
Am I smart or smart! I guessed it in 6 tries!

```


```txt
You are thinking on 50 h
You are thinking on 24 h
You are thinking on 11 l
You are thinking on 17 l
You are thinking on 20 l
You are thinking on 22 l
You are thinking on 23 l
I think you are trying to cheat me. I will not play anymore.

```



## Liberty BASIC


```lb
mini=0
maxi=100

print "Think of a number between ";mini;" and ";maxi
print "Each time I guess a number you must state whether my"
print "guess was too high, too low, or equal to your number."
print

while response$<>"="
   if not(mini<=maxi) then
		print "Error"
		exit while
	end if
	guess=int((maxi-mini)/2)+mini
	print "My guess is ";guess;". Type L for low, H for high, = for correct."
	input response$
	response$=upper$(response$)
	select case response$
		case "L"
			mini=guess+1
		case "H"
         maxi=guess-1
		case "="
			print guess;" is correct."
			exit while
		case else
			print "Your response was not helpful."
	end select
 wend
print "Thanks for playing."
```



## Lua

<lang>function wait(waittime)--wait function is used so that app will not quit immediately
  local timer = os.time()
  repeat until os.time() == timer + waittime
end


upperBound = 100
lowerBound = 0
print("Think of an integer between 1 to 100.")
print("I will try to guess it.")
while true do
	upper1 = upperBound+1
	upper2 = upperBound-1
	if upperBound == lowerBound or upper1 == lowerBound or upper2 == lowerBound or lowerBound > upperBound then--make sure player is not cheating
		io.write("You're cheating! I'm not playing anymore. Goodbye.")
		wait(3)
		break
	else
		Guess = math.floor((upperBound + lowerBound)/2)--guessing mechanism
		print("My guess is: "..Guess..". Is it too high, too low, or correct? (h/l/c)")
		input = io.read()

	if input == "h" then --higher
		upperBound = Guess
	elseif input == "l" then --lower
		lowerBound = Guess
	elseif input == "c" then --correct
		io.write("So I win? Thanks for playing with me.")
		wait(3)
		break
	else
		print("Invalid input. Please try again. ")
		end
	end
end

```



## MATLAB


```MATLAB
function GuessNumberFeedbackPlayer

    lowVal = input('Lower limit: ');
    highVal = input('Upper limit: ');
    fprintf('Think of your number. Press Enter when ready.\n')
    pause
    nGuesses = 1;
    done = false;
    while ~done
        guess = floor(0.5*(lowVal+highVal));
        score = input(sprintf( ...
            'Is %d too high (H), too low (L), or equal (E)? ', guess), 's');
        if any(strcmpi(score, {'h' 'hi' 'high' 'too high' '+'}))
            highVal = guess-1;
            nGuesses = nGuesses+1;
        elseif any(strcmpi(score, {'l' 'lo' 'low' 'too low' '-'}))
            lowVal = guess+1;
            nGuesses = nGuesses+1;
        elseif any(strcmpi(score, {'e' 'eq' 'equal' 'right' 'correct' '='}))
            fprintf('Yay! I win in %d guesses.\n', nGuesses)
            done = true;
        else
            fprintf('Unclear response. Try again.\n')
        end
        if highVal < lowVal
            fprintf('Incorrect scoring. No further guesses.\n')
            done = true;
        end
    end
end
```

```txt
Lower limit: 0
Upper limit: 50
Think of your number. Press Enter when ready.
Is 25 too high (H), too low (L), or equal (E)? L
Is 38 too high (H), too low (L), or equal (E)? H
Is 31 too high (H), too low (L), or equal (E)? H
Is 28 too high (H), too low (L), or equal (E)? L
Is 29 too high (H), too low (L), or equal (E)? E
Yay! I win in 5 guesses.
```


```txt
Lower limit: 0
Upper limit: 10
Think of your number. Press Enter when ready.
Is 5 too high (H), too low (L), or equal (E)? L
Is 8 too high (H), too low (L), or equal (E)? hello
Unclear response. Try again.
Is 8 too high (H), too low (L), or equal (E)? H
Is 6 too high (H), too low (L), or equal (E)? L
Is 7 too high (H), too low (L), or equal (E)? H
Incorrect scoring. No further guesses.
```


=={{header|Mathematica}} / {{header|Wolfram Language}}==
<lang>guessnumber[min0_, max0_] :=
  DynamicModule[{min = min0, max = max0, guess, correct = False},
   guess[] := Round@Mean@{min, max};
   Dynamic@If[correct, Row@{"Your number is ", guess[], "."},
     Column@{Row@{"I guess ", guess[], "."},
       Row@{Button["too high", max = guess[]],
         Button["too low", min = guess[]],
         Button["correct", correct = True]}}]];
guessnumber[1, 100]
```



## MAXScript


```MAXScript
inclusiveRange = [1,100]
lowRange = inclusiveRange.x
maxRange = inclusiveRange.y
guesses = 1
inf =  "Think of a number between % and % and I will try to guess it.\n" +\
		"Type -1 if the guess is less than your number,\n"+\
		"0 if the guess is correct, " +\
		"or 1 if it's too high.\nPress esc to exit.\n"
clearListener()
format inf (int lowRange) (int maxRange)
while not keyboard.escpressed do
(
	local chosen = ((lowRange + maxRange) / 2) as integer
	if lowRange == maxRange do format "\nHaving fun?"
	format "\nI choose %.\n" chosen
	local theAnswer = getKBValue prompt:"Answer? "
	case theAnswer of
	(
		(-1): (lowRange = chosen; guesses += 1)
		(0): (format "\nYay. I guessed your number after % %.\n" \
				guesses (if guesses == 1 then "try" else "tries")
				exit with OK)
		(1): (maxRange = chosen; guesses += 1)
		default: (format "\nI don't understand your input.")
	)
)
```

```MAXScript

OK
Think of a number between 1 and 100 and I will try to guess it.
Type -1 if the guess is less than your number,
0 if the guess is correct, or 1 if it's too high.
Press esc to exit.
OK

I choose 50.
Answer?  -1
I choose 75.
Answer?  -1
I choose 87.
Answer?  1
I choose 81.
Answer?  -1
I choose 84.
Answer?  -1
I choose 85.
Answer?  0
Yay. I guessed your number after 6 tries.
OK

```


=={{header|Modula-2}}==

```modula2
MODULE raden;

IMPORT  InOut;

VAR     done, ok                : BOOLEAN;
        guess, upp, low         : CARDINAL;
        res                     : CHAR;

BEGIN
  InOut.WriteString ("Choose a number between 0 and 1000.");
  InOut.WriteLn;
  InOut.WriteLn;
  upp := 1000;
  low := 0;
  REPEAT
    ok := FALSE;
    guess := ( ( upp - low ) DIV 2 ) + low;
    InOut.WriteString ("My guess is");  InOut.WriteCard (guess, 4);     InOut.Write (11C);
    InOut.WriteString ("How did I score? 'L' = too low, 'H' = too high, 'Q' = OK : ");
    InOut.WriteBf;
    REPEAT
      InOut.Read (res);
      res := CAP (res)
    UNTIL  (res = 'Q') OR (res = 'L') OR (res = 'H');
    CASE  res  OF
      'L'       : low := guess  |
      'H'       : upp := guess
    ELSE
      ok := TRUE
    END;
  UNTIL ok;
  InOut.WriteString ("So the number is");       InOut.WriteCard (guess, 4);
  InOut.WriteLn;
  InOut.WriteString ("Thanks for letting me play with you.");
  InOut.WriteLn
END raden.
```
Example:
```txt
raden
Choose a number between 0 and 1000.

My guess is 500 How did I score? 'L' = too low, 'H' = too high, 'Q' = OK : l
My guess is 750 How did I score? 'L' = too low, 'H' = too high, 'Q' = OK : h
My guess is 625 How did I score? 'L' = too low, 'H' = too high, 'Q' = OK : h
My guess is 562 How did I score? 'L' = too low, 'H' = too high, 'Q' = OK : l
My guess is 593 How did I score? 'L' = too low, 'H' = too high, 'Q' = OK : l
My guess is 609 How did I score? 'L' = too low, 'H' = too high, 'Q' = OK : h
My guess is 601 How did I score? 'L' = too low, 'H' = too high, 'Q' = OK : h
My guess is 597 How did I score? 'L' = too low, 'H' = too high, 'Q' = OK : l
My guess is 599 How did I score? 'L' = too low, 'H' = too high, 'Q' = OK : l
My guess is 600 How did I score? 'L' = too low, 'H' = too high, 'Q' = OK : q
So the number is 600
Thanks for letting me play with you.
```



## Nim


```nim
import rdstdin, strutils

let oRange = 1..10
var iRange = oRange

echo """Think of a number between $# and $# and wait for me to guess it.
On every guess of mine you should state whether the guess was
too high, too low, or equal to your number by typing h, l, or =""".format(iRange.a, iRange.b)

var i = 0
while true:
  inc i
  let guess = (iRange.a + iRange.b) div 2
  let txt = readLineFromStdin("Guess $# is: $#. The score for which is (h,l,=): "
    .format(i, guess))

  case txt
  of "h": iRange.b = guess - 1
  of "l": iRange.a = guess + 1
  of "=":
    echo "  Ye-Haw!!"
    break
  else: echo "  I don't understand your input of '%s'?".format(txt)

  if iRange.a > iRange.b or iRange.a < oRange.a or iRange.b > oRange.b:
    echo "Please check your scoring as I cannot find the value"
    break

echo "Thanks for keeping score."
```

Output:

```txt
Think of a number between 1 and 10 and wait for me to guess it.
On every guess of mine you should state whether the guess was
too high, too low, or equal to your number by typing h, l, or =
Guess 1 is: 5. The score for which is (h,l,=): l
Guess 2 is: 8. The score for which is (h,l,=): h
Guess 3 is: 6. The score for which is (h,l,=): l
Guess 4 is: 7. The score for which is (h,l,=): =
  Ye-Haw!!
Thanks for keeping score.
```


=={{header|Objective-C}}==
A clever solution that uses the built-in binary search functions with a virtual list.
```objc>#import <Foundation/Foundation.h


@interface GuessNumberFakeArray : NSArray {
  int lower, upper;
}
- (instancetype)initWithLower:(int)l andUpper:(int)u;
@end

@implementation GuessNumberFakeArray
- (instancetype)initWithLower:(int)l andUpper:(int)u {
  if ((self = [super init])) {
    lower = l;
    upper = u;
  }
  return self;
}
- (NSUInteger)count { return upper-lower; }
- (id)objectAtIndex:(NSUInteger)i {
  printf("My guess is: %d. Is it too high, too low, or correct? (H/L/C) ", lower + (int)i);
  char input[2] = " ";
  scanf("%1s", input);
  switch (tolower(input[0])) {
    case 'l':
      return @-1;
    case 'h':
      return @1;
    case 'c':
      return @0;
  }
  return nil;
}
@end

#define LOWER 0
#define UPPER 100

int main(int argc, const char *argv[]) {
  @autoreleasepool {

    printf("Instructions:\n"
           "Think of integer number from %d (inclusive) to %d (exclusive) and\n"
           "I will guess it. After each guess, you respond with L, H, or C depending\n"
           "on if my guess was too low, too high, or correct.\n",
           LOWER, UPPER);
    NSUInteger result = [[[GuessNumberFakeArray alloc] initWithLower:LOWER andUpper:UPPER]
                         indexOfObject:[NSNumber numberWithInt: 0]
                         inSortedRange:NSMakeRange(0, UPPER - LOWER)
                               options:0
                       usingComparator:^(id x, id y){ return [x compare: y]; }];
    if (result == NSNotFound)
      printf("That is impossible.\n");
    else
      printf("Your number is %d.", LOWER + (int)result);

  }
  return 0;
}
```



## PARI/GP



```parigp

guessnumber2(b)={
my(c=0,d=b,a=0);
for(x=1,b,
    for(y=1,b,
        if(a<c||a==c||a==d||a>d,
           a=random(b),
           break()
          )
     );
     print("I guess "a" am I h,l,or e ?");
     g=input();
     if(g==h,
     d=a,
     if(g==l,
        c=a,
        if(g==e,
           break()
        )
     )
  )
 );
}

```



## Pascal


```pascal
Program GuessIt(input, output);

var
  done, ok:        boolean;
  guess, upp, low: integer;
  res:             char;

begin
  writeln ('Choose a number between 0 and 1000.');
  write ('Press Enter and I will start to guess the number.');
  readln;
  upp := 1000;
  low := 0;
  repeat
    ok := false;
    guess := ( ( upp - low ) div 2 ) + low;
    write ('My guess is: ', guess:4);
    write ('. How did i score? ''l'' = too low, ''h'' = too high, ''c'' = correct : ');
    repeat
      readln (res);
      res := lowercase(res);
    until (res = 'c') or (res = 'l') or (res = 'h');
    case  res  of
      'l': low := guess;
      'h': upp := guess;
    else
      ok := true
    end;
  until ok;
  writeln ('So the number is: ', guess:4);
  writeln ('It was nice to play with you.');
end.
```

Output:

```txt
:> ./GuessTheNumberPlayerFeedback
Choose a number between 0 and 1000.
Press Enter and I will start to guess the number.
My guess is:  500. How did i score? 'l' = too low, 'h' = too high, 'c' = correct : h
My guess is:  250. How did i score? 'l' = too low, 'h' = too high, 'c' = correct : h
My guess is:  125. How did i score? 'l' = too low, 'h' = too high, 'c' = correct : l
My guess is:  187. How did i score? 'l' = too low, 'h' = too high, 'c' = correct : h
My guess is:  156. How did i score? 'l' = too low, 'h' = too high, 'c' = correct : h
My guess is:  140. How did i score? 'l' = too low, 'h' = too high, 'c' = correct : h
My guess is:  132. How did i score? 'l' = too low, 'h' = too high, 'c' = correct : l
My guess is:  136. How did i score? 'l' = too low, 'h' = too high, 'c' = correct : l
My guess is:  138. How did i score? 'l' = too low, 'h' = too high, 'c' = correct : h
My guess is:  137. How did i score? 'l' = too low, 'h' = too high, 'c' = correct : c
So the number is:  137
It was nice to play with you.
```



## Perl


```perl
#!/usr/bin/perl

my $min = 1;
my $max = 99;
my $guess = int(rand $max) + $min;
my $tries = 0;

print "=>> Think of a number between $min and $max and I'll guess it!\n
Press <ENTER> when are you ready... ";

<STDIN>;

{
    do {

        $tries++;
        print "\n=>> My guess is: $guess Is your number higher, lower, or equal? (h/l/e)\n> ";

        my $score = <STDIN>;

        if ($max <= $min) {
            print "\nI give up...\n" and last;
        } elsif ($score =~ /^h/i) {
            $min = $guess + 1;
        } elsif ($score =~ /^l/i) {
            $max = $guess;
        } elsif ($score =~ /^e/i) {
            print "\nI knew it! It took me only $tries tries.\n" and last;
        } else {
            print "error: invalid score\n";
        }

        $guess = int(($max + $min) / 2);

    } while(1);
}
```


```txt
=>> Think of a number between 1 and 99 and I'll guess it!

Press <ENTER> when are you ready...

=>> My guess is: 88. Is your number higher, lower or equal? (h/l/e)
> l

=>> My guess is: 44. Is your number higher, lower or equal? (h/l/e)
> l

=>> My guess is: 22. Is your number higher, lower or equal? (h/l/e)
> h

=>> My guess is: 33. Is your number higher, lower or equal? (h/l/e)
> l

=>> My guess is: 28. Is your number higher, lower or equal? (h/l/e)
> e

I knew it! It took me only 5 tries.
```



## Perl 6



```perl6
multi sub MAIN() { MAIN(0, 100) }
multi sub MAIN($min is copy where ($min >= 0), $max is copy where ($max > $min)) {
    say "Think of a number between $min and $max and I'll guess it!";
    while $min <= $max {
        my $guess = (($max + $min)/2).floor;
        given lc prompt "My guess is $guess. Is your number higher, lower or equal? " {
            when /^e/ { say "I knew it!"; exit }
            when /^h/ { $min = $guess + 1      }
            when /^l/ { $max = $guess          }
            default   { say "WHAT!?!?!"        }
        }
    }
    say "How can your number be both higher and lower than $max?!?!?";
}
```


You may execute this program with '<tt>perl6 program</tt>' or with '<tt>perl6 program min max</tt>'. Perl 6 creates a usage for us if we don't give the right parameters. It also parses the parameters for us and provides them via <tt>$min</tt> and <tt>$max</tt>. We use multi-subs to provide two MAIN subroutines so the user is able to choose between min and max parameters and no parameters at all, in which case min is set to 0 and max to 100.


## Phix


```Phix
integer Min=0, Max=100, Guess, Response
printf(1,"Think of a number between %d and %d.\n",{Min,Max})
while 1 do
    Guess = floor((Max+Min)/2)
    printf(1,"My guess is %d, is this too high, too low, or correct? (H/L/C)", Guess)
    Response = lower(wait_key())
    puts(1,"\n")
    if Response='h' then
        Max = Guess-1
    elsif Response='l' then
        Min = Guess+1
    elsif Response='c' then
        puts(1,"I did it!\n")
        exit
    else
        puts(1,"I do not understand that...\n")
    end if
    if Max<Min then
        puts(1,"I think something is strange here...\n")
        exit
    end if
end while
```



## PicoLisp

```PicoLisp
(de guessTheNumber (Min Max)
   (prinl "Think of a number between " Min " and " Max ".")
   (prinl "On every guess of mine you should state whether my guess was")
   (prinl "too high, too low, or equal to your number by typing 'h', 'l', Or '='")
   (use Guess
      (loop
         (NIL (> Max Min)
            (prinl "I think somthing is strange here...") )
         (prin
            "My guess is "
            (setq Guess (+ Min (/ (- Max Min) 2)))
            ",is this correct? " )
         (flush)
         (NIL
            (case (uppc (car (line)))
               ("H" (setq Max Guess))
               ("L" (setq Min Guess))
               ("=" (nil (prinl "I did it!")))
               (T (prinl "I do not understand that...")) ) ) ) ) )
```

Output:

```txt
: (guessTheNumber 1 99)
Think of a number between 1 and 99.
On every guess of mine you should state whether my guess was
too high, too low, or equal to your number by typing 'h', 'l', Or '='
My guess is 50,is this correct? h
My guess is 25,is this correct? h
My guess is 13,is this correct? l
My guess is 19,is this correct? l
My guess is 22,is this correct? =
I did it!
```



## Prolog


```prolog
min(1). max(10).

pick_number(Min, Max) :-
    min(Min), max(Max),
    format('Pick a number between ~d and ~d, and I will guess it...~nReady? (Enter anything when ready):', [Min, Max]),
    read(_).

guess_number(Min, Max) :-
    Guess is (Min + Max) // 2,
    format('I guess ~d...~nAm I correct (c), too low (l), or too high (h)? ', [Guess]),
    repeat,
        read(Score),
        ( Score = l -> NewMin is Guess + 1, guess_number(NewMin, Max)
        ; Score = h -> NewMax is Guess - 1, guess_number(Min, NewMax)
        ; Score = c -> writeln('I am correct!')
        ; writeln('Invalid input'),
          false
        ).

play :-
    pick_number(Min, Max),
    guess_number(Min, Max).
```


Example:


```prolog
?- play.
Pick a number between 1 and 10, and I will guess it...
Ready? (Enter anything when ready):y.
I guess 5...
Am I correct (c), too low (l), or too high (h)? h.
I guess 2...
Am I correct (c), too low (l), or too high (h)? l.
I guess 3...
Am I correct (c), too low (l), or too high (h)? c.
I'm correct!
true
```



## PureBasic


```PureBasic
min=0
max=100

If OpenConsole()
  PrintN("Think of a number between "+Str(min)+" and "+Str(max)+".")
  PrintN("On every guess of mine you should state whether my guess was")
  PrintN("too high, too low, or equal to your number by typing 'h', 'l', Or '='")
  Repeat
    If max<=min
      PrintN("I think somthing is strange here...")
      Break
    EndIf
    Guess=(max-min)/2+min
    Print("My guess is "+Str(Guess)+",is this correct? "): Respons.s=UCase(Input())
    If Respons="H":     max=Guess-1
    ElseIf Respons="L": min=Guess+1
    ElseIf Respons="="
      PrintN("I did it!")
      Break
    Else
      PrintN("I do not understand that...")
    EndIf
  ForEver
EndIf
```



## Python


```python
inclusive_range = mn, mx = (1, 10)

print('''\
Think of a number between %i and %i and wait for me to guess it.
On every guess of mine you should state whether the guess was
too high, too low, or equal to your number by typing h, l, or =
''' % inclusive_range)

i = 0
while True:
    i += 1
    guess = (mn+mx)//2
    txt = input("Guess %2i is: %2i. The score for which is (h,l,=): "
                % (i, guess)).strip().lower()[0]
    if txt not in 'hl=':
        print("  I don't understand your input of '%s' ?" % txt)
        continue
    if txt == 'h':
        mx = guess-1
    if txt == 'l':
        mn = guess+1
    if txt == '=':
        print("  Ye-Haw!!")
        break
    if (mn > mx) or (mn < inclusive_range[0]) or (mx > inclusive_range[1]):
        print("Please check your scoring as I cannot find the value")
        break

print("\nThanks for keeping score.")
```


'''Sample Game-play'''

```txt
Think of a number between 1 and 10 and wait for me to guess it.
On every guess of mine you should state whether the guess was
too high, too low, or equal to your number by typing h, l, or =

Guess  1 is:  5. The score for which is (h,l,=): l
Guess  2 is:  8. The score for which is (h,l,=): l
Guess  3 is:  9. The score for which is (h,l,=): l
Guess  4 is: 10. The score for which is (h,l,=): =
  Ye-Haw!!

Thanks for keeping score.
```



### Obscure solution using bisect module


Hacky solution using a fake list class and the <code>bisect</code> module from the standard library to do the binary search.
```python
import bisect
try: input = raw_input
except: pass

class GuessNumberFakeList(object):
    def __getitem__(self, i):
        s = input("Is your number less than or equal to %d?" % i)
        return 0 if s.lower().startswith('y') else -1

LOWER, UPPER = 0, 100

if __name__ == "__main__":
    print("""Instructions:
Think of integer number from %d (inclusive) to %d (exclusive) and
I will guess it. After each guess, I will ask you if it is less than
or equal to some number, and you will respond with "yes" or "no".
""" % (LOWER, UPPER))
    result = bisect.bisect_left(GuessNumberFakeList(), 0, LOWER, UPPER)
    print("Your number is %d." % result)
```


;Sample output:

```txt
Instructions:
Think of integer number from 0 (inclusive) to 100 (exclusive) and
I will guess it. After each guess, I will ask you if it is less than
or equal to some number, and you will respond with "yes" or "no".


Is your number less than or equal to 50?no

Is your number less than or equal to 75?yes

Is your number less than or equal to 63?no

Is your number less than or equal to 69?no

Is your number less than or equal to 72?yes

Is your number less than or equal to 71?no
Your number is 72.
```



## Racket


```Racket
#lang racket

(define (guess low high)
  (define (input-loop available)
    (define input (car (string->list (symbol->string (read)))))
    (if (member input available)
        input
        (begin
          (printf "Invalid Input\n") (input-loop available))))

  (define (guess-loop low high)
    (define guess (floor (/ (+ low high) 2)))
    (printf "My guess is ~a.\n" guess)
    (define input (input-loop (list #\c #\l #\h)))
    (case input
      ((#\c) (displayln "I knew it!\n"))
      ((#\l) (guess-loop low (sub1 guess)))
      ((#\h) (guess-loop (add1 guess) high))))

  (printf "Think of a number between ~a and ~a.
Use (h)igh, (l)ow and (c)orrect to guide me.\n" low high)
  (guess-loop low high))
```


Another way with loops and mutation

```Racket
#lang racket
(define (guess minimum maximum)
  (printf "Think of a number from ~a to ~a, use (h)igh, (l)ow and (c)orrect." minimum maximum)

  (define input "")

  (do ((guess (round (/ (+ maximum minimum) 2))  (round (/ (+ maximum minimum) 2))))
    ((string=? input "c"))
    (printf "My guess is: ~a\n(h/l/=) > ")
    (when (string=? input "h")
      (begin (display "OK...")
               (set! maximum (sub1 guess))))
        (when (string=? input "l")
          (begin (display "OK...")
                 (set! minimum (add1 guess)))))
  (displayln "I was RIGHT!"))
```



## REXX


### with positive integers

This version only handles positive integers up to the nine decimal digits,   but the default HIGH is one thousand.

```rexx
/*REXX program plays  guess─the─number  (with itself)  with  positive integers.         */
parse arg low high seed .                        /*obtain optional arguments from the CL*/
if  low=='' |  low=="," then  low=    1          /*Not specified?  Then use the default.*/
if high=='' | high=="," then high= 1000          /* "      "         "   "   "     "    */
if datatype(seed, 'W')  then call random ,,seed  /*Useful seed?  Then use a random seed.*/
?= random(low, high)                             /*generate random number from low->high*/
$= "──────── Try to guess my number  (it's between  "        /*part of a prompt message.*/
g=                                                           /*nullify the first guess. */
    do #=1;                        oldg= g       /*save the guess for later comparison. */
    if pos('high', info)\==0  then high= g       /*test if the guess is too  high.      */
    if pos('low' , info)\==0  then low = g       /*  "   "  "    "    "  "   low.       */
    say                                          /*display a blank line before prompt.  */
    say $ low  '  and  '   high  "  inclusive):" /*issue the prompt message to terminal.*/
    say                                          /*display a blank line  after prompt.  */
    g= (low +  (high - low) / 2)   / 1           /*calculate the next guess & normalize.*/
    if g=oldg   then g= g + 1                    /*bump guess by one 'cause we're close.*/
    say 'My guess is'       g                    /*display computer's guess to the term.*/
    if g=?  then leave                           /*this guess is correct; leave & inform*/
    if g>?  then info= right(' Your guess is too high.', 60, "─")
            else info= right(' Your guess is too low.' , 60, "─")
    say info
    end   /*try*/
say                                              /*stick a fork in it,  we're all done. */
say 'Congratulations!   You guessed the secret number in'    #    "tries."
```

'''output''' shown is from playing several games:
<pre style="height:70ex">
──────── Try to guess my number  (it's between   1   and   1000   inclusive):

My guess is 500.5
──────────────────────────────────── Your guess is too high.

──────── Try to guess my number  (it's between   1   and   500.5   inclusive):

My guess is 250.75
───────────────────────────────────── Your guess is too low.

──────── Try to guess my number  (it's between   250.75   and   500.5   inclusive):

My guess is 375.625
──────────────────────────────────── Your guess is too high.

──────── Try to guess my number  (it's between   250.75   and   375.625   inclusive):

My guess is 313.1875
───────────────────────────────────── Your guess is too low.

──────── Try to guess my number  (it's between   313.1875   and   375.625   inclusive):

My guess is 344.40625
──────────────────────────────────── Your guess is too high.

──────── Try to guess my number  (it's between   313.1875   and   344.40625   inclusive):

My guess is 328.796875
───────────────────────────────────── Your guess is too low.

──────── Try to guess my number  (it's between   328.796875   and   344.40625   inclusive):

My guess is 336.601563
──────────────────────────────────── Your guess is too high.

──────── Try to guess my number  (it's between   328.796875   and   336.601563   inclusive):

My guess is 332.699219
──────────────────────────────────── Your guess is too high.

──────── Try to guess my number  (it's between   328.796875   and   332.699219   inclusive):

My guess is 330.748047
──────────────────────────────────── Your guess is too high.

──────── Try to guess my number  (it's between   328.796875   and   330.748047   inclusive):

My guess is 329.772461
───────────────────────────────────── Your guess is too low.

──────── Try to guess my number  (it's between   329.772461   and   330.748047   inclusive):

My guess is 330.260254
──────────────────────────────────── Your guess is too high.

──────── Try to guess my number  (it's between   329.772461   and   330.260254   inclusive):

My guess is 330.016358
──────────────────────────────────── Your guess is too high.

──────── Try to guess my number  (it's between   329.772461   and   330.016358   inclusive):

My guess is 329.89441
───────────────────────────────────── Your guess is too low.

──────── Try to guess my number  (it's between   329.89441   and   330.016358   inclusive):

My guess is 329.955384
───────────────────────────────────── Your guess is too low.

──────── Try to guess my number  (it's between   329.955384   and   330.016358   inclusive):

My guess is 329.985871
───────────────────────────────────── Your guess is too low.

──────── Try to guess my number  (it's between   329.985871   and   330.016358   inclusive):

My guess is 330.001115
──────────────────────────────────── Your guess is too high.

──────── Try to guess my number  (it's between   329.985871   and   330.001115   inclusive):

My guess is 329.993493
───────────────────────────────────── Your guess is too low.

──────── Try to guess my number  (it's between   329.993493   and   330.001115   inclusive):

My guess is 329.997304
───────────────────────────────────── Your guess is too low.

──────── Try to guess my number  (it's between   329.997304   and   330.001115   inclusive):

My guess is 329.99921
───────────────────────────────────── Your guess is too low.

──────── Try to guess my number  (it's between   329.99921   and   330.001115   inclusive):

My guess is 330.000163
──────────────────────────────────── Your guess is too high.

──────── Try to guess my number  (it's between   329.99921   and   330.000163   inclusive):

My guess is 329.999687
───────────────────────────────────── Your guess is too low.

──────── Try to guess my number  (it's between   329.999687   and   330.000163   inclusive):

My guess is 329.999925
───────────────────────────────────── Your guess is too low.

──────── Try to guess my number  (it's between   329.999925   and   330.000163   inclusive):

My guess is 330.000044
──────────────────────────────────── Your guess is too high.

──────── Try to guess my number  (it's between   329.999925   and   330.000044   inclusive):

My guess is 329.999985
───────────────────────────────────── Your guess is too low.

──────── Try to guess my number  (it's between   329.999985   and   330.000044   inclusive):

My guess is 330.000015
──────────────────────────────────── Your guess is too high.

──────── Try to guess my number  (it's between   329.999985   and   330.000015   inclusive):

My guess is 330

Congratulations!   You guessed the secret number in 26 tries.

```



###  with positive numbers

This version handles decimal fractions,   the method used can generate numbers from zero to five fractional digits.

```rexx
/*REXX program plays  guess─the─number  (with itself)  with  positive rational numbers. */
parse arg low high frac seed .                   /*obtain optional arguments from the CL*/
if  low=='' |  low=="," then  low=    1          /*Not specified?  Then use the default.*/
if high=='' | high=="," then high= 1000          /* "      "         "   "   "     "    */
if frac=='' | frac=="," then frac=    1          /* "      "         "   "   "     "    */
if datatype(seed, 'W')  then call random ,,seed  /*Useful seed?  Then use a random seed.*/
fdigs= 10**frac                                  /*compute the number of fractional digs*/
?= random(low, high) + random(0,fdigs) / fdigs   /*Tougher game?  It may have fractions.*/
$= "──────── Try to guess my number  (it's between  "        /*part of a prompt message.*/
g=                                                           /*nullify the first guess. */
    do #=1;                        oldg= g       /*save the guess for later comparison. */
    if pos('high', info)\==0  then high= g       /*test if the guess is too  high.      */
    if pos('low' , info)\==0  then low = g       /*  "   "  "    "    "  "   low.       */
    say                                          /*display a blank line before prompt.  */
    say $ low  '  and  '   high  "  inclusive):" /*issue the prompt message to terminal.*/
    say                                          /*display a blank line  after prompt.  */
    g= (low +  (high - low) / 2)   / 1           /*calculate the next guess & normalize.*/
    if g=oldg   then g= g + 1                    /*bump guess by one 'cause we're close.*/
    say 'My guess is'       g                    /*display computer's guess to the term.*/
    if g=?  then leave                           /*this guess is correct; leave & inform*/
    if g>?  then info= right(' Your guess is too high.', 60, "─")
            else info= right(' Your guess is too low.' , 60, "─")
    say info
    end   /*try*/
say                                              /*stick a fork in it,  we're all done. */
say 'Congratulations!   You guessed the secret number in'    #    "tries.""
```

'''output''' will generally be about ten times longer (that is, has ten times the guesses) as the previous REXX version.
<pre style="height:35ex">
──────── Try to guess my number  (it's between   1   and   1000   inclusive):

My guess is 500.5
──────────────────────────────────── Your guess is too high.

──────── Try to guess my number  (it's between   1   and   500.5   inclusive):

My guess is 250.75
───────────────────────────────────── Your guess is too low.

──────── Try to guess my number  (it's between   250.75   and   500.5   inclusive):

My guess is 375.625
──────────────────────────────────── Your guess is too high.

──────── Try to guess my number  (it's between   250.75   and   375.625   inclusive):

My guess is 313.1875
───────────────────────────────────── Your guess is too low.

──────── Try to guess my number  (it's between   313.1875   and   375.625   inclusive):

My guess is 344.40625
──────────────────────────────────── Your guess is too high.

──────── Try to guess my number  (it's between   313.1875   and   344.40625   inclusive):

My guess is 328.796875
───────────────────────────────────── Your guess is too low.

──────── Try to guess my number  (it's between   328.796875   and   344.40625   inclusive):

My guess is 336.601563
──────────────────────────────────── Your guess is too high.

──────── Try to guess my number  (it's between   328.796875   and   336.601563   inclusive):

My guess is 332.699219
──────────────────────────────────── Your guess is too high.

──────── Try to guess my number  (it's between   328.796875   and   332.699219   inclusive):

My guess is 330.748047
──────────────────────────────────── Your guess is too high.

──────── Try to guess my number  (it's between   328.796875   and   330.748047   inclusive):

My guess is 329.772461
───────────────────────────────────── Your guess is too low.

──────── Try to guess my number  (it's between   329.772461   and   330.748047   inclusive):

My guess is 330.260254
───────────────────────────────────── Your guess is too low.

──────── Try to guess my number  (it's between   330.260254   and   330.748047   inclusive):

My guess is 330.504151
───────────────────────────────────── Your guess is too low.

──────── Try to guess my number  (it's between   330.504151   and   330.748047   inclusive):

My guess is 330.626099
──────────────────────────────────── Your guess is too high.

──────── Try to guess my number  (it's between   330.504151   and   330.626099   inclusive):

My guess is 330.565125
───────────────────────────────────── Your guess is too low.

──────── Try to guess my number  (it's between   330.565125   and   330.626099   inclusive):

My guess is 330.595612
───────────────────────────────────── Your guess is too low.

──────── Try to guess my number  (it's between   330.595612   and   330.626099   inclusive):

My guess is 330.610856
──────────────────────────────────── Your guess is too high.

──────── Try to guess my number  (it's between   330.595612   and   330.610856   inclusive):

My guess is 330.603234
──────────────────────────────────── Your guess is too high.

──────── Try to guess my number  (it's between   330.595612   and   330.603234   inclusive):

My guess is 330.599423
───────────────────────────────────── Your guess is too low.

──────── Try to guess my number  (it's between   330.599423   and   330.603234   inclusive):

My guess is 330.601329
──────────────────────────────────── Your guess is too high.

──────── Try to guess my number  (it's between   330.599423   and   330.601329   inclusive):

My guess is 330.600376
──────────────────────────────────── Your guess is too high.

──────── Try to guess my number  (it's between   330.599423   and   330.600376   inclusive):

My guess is 330.5999
───────────────────────────────────── Your guess is too low.

──────── Try to guess my number  (it's between   330.5999   and   330.600376   inclusive):

My guess is 330.600138
──────────────────────────────────── Your guess is too high.

──────── Try to guess my number  (it's between   330.5999   and   330.600138   inclusive):

My guess is 330.600019
──────────────────────────────────── Your guess is too high.

──────── Try to guess my number  (it's between   330.5999   and   330.600019   inclusive):

My guess is 330.59996
───────────────────────────────────── Your guess is too low.

──────── Try to guess my number  (it's between   330.59996   and   330.600019   inclusive):

My guess is 330.59999
───────────────────────────────────── Your guess is too low.

──────── Try to guess my number  (it's between   330.59999   and   330.600019   inclusive):

My guess is 330.600005
──────────────────────────────────── Your guess is too high.

──────── Try to guess my number  (it's between   330.59999   and   330.600005   inclusive):

My guess is 330.599998
───────────────────────────────────── Your guess is too low.

──────── Try to guess my number  (it's between   330.599998   and   330.600005   inclusive):

My guess is 330.600002
──────────────────────────────────── Your guess is too high.

──────── Try to guess my number  (it's between   330.599998   and   330.600002   inclusive):

My guess is 330.6

Congratulations!   You guessed the secret number in 29 tries.

```



## Ring


```ring

min = 1
max = 100
see "think of a number between " + min + " and " + max + nl
see "i will try to guess your number." + nl
while true
      guess =floor((min + max) / 2)
      see "my guess is " + guess + nl
      see "is it higher than, lower than or equal to your number " give answer
      ans = left(answer,1)
      switch ans
             on "l" min = guess + 1
             on "h" max = guess - 1
             on "e" exit
             other see "sorry, i didn't understand your answer." + nl
      off
end
see "goodbye." + nl

```

Output:

```txt

think of a number between 1 and 100
i will try to guess your number.
my guess is 50
is it higher than, lower than or equal to your number l
my guess is 75
is it higher than, lower than or equal to your number h
my guess is 62
is it higher than, lower than or equal to your number h
my guess is 56
is it higher than, lower than or equal to your number l
my guess is 59
is it higher than, lower than or equal to your number l
my guess is 60
is it higher than, lower than or equal to your number e
goodbye.

```



## Ruby

Computer plays versus itself

```ruby
def play(low, high, turns=1)
  num = (low + high) / 2
  print "guessing #{num}\t"
  case is_it?(num)
  when 1
    puts "too high"
    play(low, num - 1, turns + 1)
  when -1
    puts "too low"
    play(num + 1, high, turns + 1)
  else
    puts "found the number in #{turns} turns."
  end
end

def is_it?(num)
  num <=> $number
end

low, high = 1, 100
$number = rand(low .. high)

puts "guess a number between #{low} and #{high}"
play(low, high)
```


Example

```txt
guess a number between 1 and 100
guessing 50     too high
guessing 25     too low
guessing 37     too high
guessing 31     too high
guessing 28     too low
guessing 29     too low
guessing 30     found the number in 7 turns.
```


Since Ruby 2.0 it can be done actually using a built-in binary search (output as above):

```ruby
r = (1..100)
secret = rand(r)
turns = 0

puts "Guess a number between #{r.min} and #{r.max}"
r.bsearch do |guess|                # bsearch works on ranges
  print "Guessing #{guess} \t"
  turns += 1
  low_high = secret <=> guess       # -1, 0, or 1
  puts ["found the number in #{turns} turns", "too low", "too high"][low_high]
  low_high
end

```



## Rust

```rust
use std::io::stdin;

const MIN: isize = 1;
const MAX: isize = 100;

fn main() {
    loop {
        let mut min = MIN;
        let mut max = MAX;
        let mut num_guesses = 1;
        println!("Please think of a number between {} and {}", min, max);
        loop {
            let guess = (min + max) / 2;
            println!("Is it {}?", guess);
            println!("(type h if my guess is too high, l if too low, e if equal and q to quit)");

            let mut line = String::new();
            stdin().read_line(&mut line).unwrap();
            match Some(line.chars().next().unwrap().to_uppercase().next().unwrap()) {
                Some('H') => {
                    max = guess - 1;
                    num_guesses += 1;
                },
                Some('L')=> {
                    min = guess + 1;
                    num_guesses += 1;
                },
                Some('E') => {
                    if num_guesses == 1 {
                        println!("\n*** That was easy! Got it in one guess! ***\n");
                    } else {
                        println!("\n*** I knew it! Got it in only {} guesses! ***\n", num_guesses);
                    }
                    break;
                },
                Some('Q') => return,
                _ => println!("Sorry, I didn't quite get that. Please try again.")
            }
        }
    }
}
```



## Scala


```Scala
object GuessNumber extends App {
  val n = 1 + scala.util.Random.nextInt(20)

  println("Guess which number I've chosen in the range 1 to 20\n")
  do println("Your guess, please: ")
  while (io.StdIn.readInt() match {
    case `n` => println("Correct, well guessed!"); false
    case guess if (n + 1 to 20).contains(guess) => println("Your guess is higher than the chosen number, try again"); true
    case guess if (1 until n).contains(guess) => println("Your guess is lower than the chosen number, try again"); true
    case _ => println("Your guess is inappropriate, try again"); true
  })

}
```


## Scheme

```scheme
(define minimum 1)   (define maximum 100)

(display "Enter a number from ")(display minimum)
(display " to ")(display maximum)(display ": ")
(define number (read))

(define input "")

(do ((guess (round (/ (+ maximum minimum) 2))  (round (/ (+ maximum minimum) 2))))
                ((string= input "="))
        (display "Is it ")(display guess)(display "?\n(h/l/=) > ")
        (set! input (symbol->string (read)))
        (if (string= input "h") (begin (display "OK...")
                (set! maximum (- guess 1))))
        (if (string= input "l") (begin (display "OK...")
                (set! minimum (+ guess 1)))))
(display "I was RIGHT!\n")
```



## Seed7

The program reads the possible commands (l, h, c, q) as single keypresses from the
[http://seed7.sourceforge.net/libraries/keybd.htm#KEYBOARD KEYBOARD] with
[http://seed7.sourceforge.net/libraries/keybd.htm#getc%28in_console_keybd_file%29 getc].
It is also possible to quit the program with q.
The program recognices also a situation when there is only one possible number left
and the command is not c.


```seed7
$ include "seed7_05.s7i";
$ include "keybd.s7i";

const proc: main is func
  local
    var boolean: okay is FALSE;
    var boolean: quit is FALSE;
    var integer: low is 1;
    var integer: high is 1000;
    var integer: guess is 0;
    var char: command is ' ';
  begin
    writeln("Choose a number between 1 and 1000.");
    write("Press Enter and I will start to guess the number.");
    readln;
    repeat
      guess := low + (high - low) div 2;
      write("My guess is: " <& guess);
      write(". How did I score (l=too low, h=too high, c=correct, q=quit)? ");
      flush(OUT);
      repeat
        command := lower(getc(KEYBOARD));
      until command in {'l', 'h', 'c', 'q'};
      writeln(command);
      case command of
        when {'l'}: low := succ(guess);
        when {'h'}: high := pred(guess);
        when {'c'}: okay := TRUE;
        when {'q'}: quit := TRUE;
      end case;
    until quit or okay or low > high;
    if not quit then
      writeln("So the number is: " <& guess);
      if low > high then
        writeln("Why did you cheat?");
      end if;
    end if;
    writeln("It was nice to play with you.");
  end func;
```


Example

```txt

Choose a number between 1 and 1000.
Press Enter and I will start to guess the number.
My guess is: 500. How did I score (l=too low, h=too high, c=correct, q=quit)? h
My guess is: 250. How did I score (l=too low, h=too high, c=correct, q=quit)? l
My guess is: 375. How did I score (l=too low, h=too high, c=correct, q=quit)? h
My guess is: 312. How did I score (l=too low, h=too high, c=correct, q=quit)? l
My guess is: 343. How did I score (l=too low, h=too high, c=correct, q=quit)? l
My guess is: 359. How did I score (l=too low, h=too high, c=correct, q=quit)? l
My guess is: 367. How did I score (l=too low, h=too high, c=correct, q=quit)? h
My guess is: 363. How did I score (l=too low, h=too high, c=correct, q=quit)? l
My guess is: 365. How did I score (l=too low, h=too high, c=correct, q=quit)? c
So the number is: 365
It was nice to play with you.

```



## Sidef


```ruby
var min = 1
var max = 99
var tries = 0
var guess = pick(min..max)
 
print <<"EOT".chomp
\n=>> Think of a number between #{min} and #{max} and I'll guess it!\n
Press <ENTER> when are you ready...
EOT
 
STDIN.readline
 
loop {
    print <<-EOT.chomp
    \n=>> My guess is: #{guess} Is your number higher, lower, or equal? (h/l/e)
    >#{' '}
    EOT
 
    ++tries
    given (STDIN.readline) {
        case (max <= min) {
            say "\nI give up..."
            break
        }
        when (/^h/i) {
            min = guess+1
        }
        when (/^l/i) {
            max = guess
        }
        when (/^e/i) {
            say "\nI knew it! It took me only #{tries} tries."
            break
        }
        default {
            say "error: invalid score"
            next
        }
    }
 
    guess = ((min+max) // 2)
}
```



## Smalltalk


Create a class like this:


```smalltalk
Object subclass: #NumberGuesser
    instanceVariableNames: ''
    classVariableNames: ''
    poolDictionaries: ''
    category: 'Rosettacode'

```


Enter these methods into it:


```smalltalk
playBetween: low and: high
    Transcript clear.
    number := (low to: high) atRandom.
    self playBetween: low and: high atTurn: 1.

playBetween: low and: high atTurn: turn
    | guess |
    guess := (low + ((high - low) / 2)) asInteger.
    self showGuessing: guess atTurn: turn.
    true caseOf: {
        [number > guess] ->
            [self showNumberBeing: #low. self playBetween: guess and: high atTurn: turn+1 ].
        [number < guess] ->
            [self showNumberBeing: #high. self playBetween: low and: guess atTurn: turn+1 ].
        [true] ->
            [self showNumberFoundAtTurnNr: turn ] }.

showGuessing: guess atTurn: turn
    Transcript show: ('{1}. guessing: {2}' format: {turn asString. guess asString}).

showNumberBeing: comparisonToken
    Transcript show: ' ==> too ', (comparisonToken asString); cr.

showNumberFoundAtTurnNr: turn
    Transcript show: (' ==> found the number in {1} turn(s).' format: {turn asString}); cr.

```


If you'd rather not use <code>caseOf:</code> you can write <code>playBetween:and:atTurn:</code> like this:


```smalltalk

playBetween: low and: high atTurn: turn
    | guess |
    guess := (low + ((high - low) / 2)) asInteger.
    self showGuessing: guess atTurn: turn.
    (guess = number)
        ifTrue: [self showNumberFoundAtTurnNr: turn ]
        ifFalse: [
          (number > guess)
              ifTrue: [self showNumberBeing: #low. self playBetween: guess and: high  atTurn: turn+1 ]
              ifFalse: [self showNumberBeing: #high. self playBetween: low and: guess  atTurn: turn+1]].

```


And evaluate this in a Workspace:


```smalltalk
g := NumberGuesser new.
g playBetween: 1 and: 100.

```


Sample output:


```txt

1. guessing: 50 ==> too low
2. guessing: 75 ==> too low
3. guessing: 87 ==> too high
4. guessing: 81 ==> too low
5. guessing: 84 ==> too low
6. guessing: 85 ==> found the number in 6 turn(s).

```



## Standard ML

A clever solution that uses the built-in binary search functions with a virtual list.
```sml
structure GuessNumberHelper : MONO_ARRAY = struct
  type elem = order
  type array = int * int
  fun length (lo, hi) = hi - lo
  fun sub ((lo, hi), i) =
    let
      val n = lo + i
    in
      print ("My guess is: " ^ Int.toString (lo+i) ^ ". Is it too high, too low, or correct? (H/L/C) ");
      let
        val str = valOf (TextIO.inputLine TextIO.stdIn)
      in
        case Char.toLower (String.sub (str, 0)) of
          #"l" => GREATER
        | #"h" => LESS
        | #"c" => EQUAL
      end
    end

  (* dummy implementations for not-needed functions *)
  type vector = unit
  val maxLen = Array.maxLen
  fun update _ = raise Domain
  fun array _ = raise Domain
  fun fromList _ = raise Domain
  fun tabulate _ = raise Domain
  fun vector _ = raise Domain
  fun copy _ = raise Domain
  fun copyVec _ = raise Domain
  fun appi _ = raise Domain
  fun app _ = raise Domain
  fun modifyi _ = raise Domain
  fun modify _ = raise Domain
  fun foldli _ = raise Domain
  fun foldl _ = raise Domain
  fun foldri _ = raise Domain
  fun foldr _ = raise Domain
  fun findi _ = raise Domain
  fun find _ = raise Domain
  fun exists _ = raise Domain
  fun all _ = raise Domain
  fun collate _ = raise Domain
end

structure GuessNumberBSearch = BSearchFn (GuessNumberHelper)

val lower = 0
val upper = 100;

print ("Instructions:\n" ^
       "Think of integer number from " ^ Int.toString lower ^
       " (inclusive) to " ^ Int.toString upper ^ " (exclusive) and\n" ^
       "I will guess it. After each guess, you respond with L, H, or C depending\n" ^
       "on if my guess was too low, too high, or correct.\n");

case GuessNumberBSearch.bsearch (fn (_, x) => x) ((), (lower, upper)) of
  NONE => print "That is impossible.\n"
| SOME (result, _) => print ("Your number is " ^ Int.toString result ^ ".\n")
```


## Swift

<lang>import Cocoa

var found = false
let fh = NSFileHandle.fileHandleWithStandardInput()
println("Enter an integer between 1 and 100 for me to guess: ")
let data = fh.availableData
var num:Int!
var low = 0.0
var high = 100.0
var lastGuess:Double!


if let numFromData = NSString(data: data, encoding: NSUTF8StringEncoding)?.intValue {
    num = Int(numFromData)
}

func guess() -> Double? {
    if (high - low == 1) {
        println("I can't guess it. I think you cheated.");
        return nil
    }

    return floor((low + high) / 2)
}

while (!found) {
    if let guess = guess() {
        lastGuess = guess

    } else {
        break
    }
    println("My guess is: \(Int(lastGuess))")
    println("How was my guess? Enter \"higher\" if it was higher, \"lower\" if it was lower, and \"correct\" if I got it")
    let data = fh.availableData
    let str = NSString(data: data, encoding: NSUTF8StringEncoding)
    if (str == nil) {
        continue
    }
    if (str! == "correct\n") {
        found = true
        println("I did it!")
    } else if (str! == "higher\n") {
        low = lastGuess
    } else if (str! == "lower\n") {
        high = lastGuess
    }
}
```

```txt

Enter an integer between 1 and 100 for me to guess:
10
My guess is: 50
How was my guess? Enter "higher" if it was higher, "lower" if it was lower, and "correct" if I got it
lower
My guess is: 25
How was my guess? Enter "higher" if it was higher, "lower" if it was lower, and "correct" if I got it
lower
My guess is: 12
How was my guess? Enter "higher" if it was higher, "lower" if it was lower, and "correct" if I got it
lower
My guess is: 6
How was my guess? Enter "higher" if it was higher, "lower" if it was lower, and "correct" if I got it
higher
My guess is: 9
How was my guess? Enter "higher" if it was higher, "lower" if it was lower, and "correct" if I got it
higher
My guess is: 10
How was my guess? Enter "higher" if it was higher, "lower" if it was lower, and "correct" if I got it
correct
I did it!
Program ended with exit code: 0
```



## Tcl


```tcl
set from 1
set to 10
fconfigure stdout -buffering none
while 1 {
    set guess [expr {($from+$to) / 2}]
    puts -nonewline "Guess: $guess\tWas it lower (<) equal (=) or higher (>)? "
    switch -- [gets stdin] {
	< { set from [expr {$guess + 1}] }
	> { set to [expr {$guess - 1}] }
	= {
	    puts "Found it: $guess"
	    break
	}
	default {
	    puts "What sort of a response was that?!"
	}
    }
    if {$to < $from} {
	puts "No answer possible?!"
	break
    }
}
```

Sample run:

```txt

Guess: 5	Was it lower (<) equal (=) or higher (>)? <
Guess: 8	Was it lower (<) equal (=) or higher (>)? >
Guess: 6	Was it lower (<) equal (=) or higher (>)? =
Found it: 6

```



## UNIX Shell

```bash
read -p "Lower bound: " lower
read -p "Upper bound: " upper
moves=0
PS3="> "
while :; do
    ((moves++))
    guess=$(( lower + (upper-lower)/2 ))
    echo "Is it $guess?"
    select ans in "too small" "too big" "got it!"; do
        case $ans in
            "got it!")   break 2 ;;
            "too big")   upper=$(( upper==guess ? upper-1 : guess )); break ;;
            "too small") lower=$(( lower==guess ? lower+1 : guess )); break ;;
        esac
    done
    ((lower>upper)) && echo "you must be cheating!"
done
echo "I guessed it in $moves guesses"
```



## VBA



```vba

Sub GuessNumberPlayer()
Dim iGuess As Integer, iLow As Integer, iHigh As Integer, iCount As Integer
Dim vSolved As Variant
On Error GoTo ErrHandler
MsgBox "Pick a number between 1 and 100. I will guess it!", vbInformation + vbOKOnly, "Rosetta Code | Guess the Number Player"
iCount = 0
iLow = 1
iHigh = 100
Do While Not vSolved = "Y"
    iGuess = Application.WorksheetFunction.RandBetween(iLow, iHigh)
    iCount = iCount + 1
CheckGuess:
    vSolved = InputBox("My guess: " & iGuess & vbCr & vbCr & "Y = Yes, correct guess" & vbCr & _
        "H = your number is higher" & vbCr & "L = your number is lower" & vbCr & "X = exit game", "Rosetta Code | Guess the Number Player | Guess " & iCount)
    Select Case vSolved
        Case "Y", "y":              GoTo CorrectGuess
        Case "X", "x":              Exit Sub
        Case "H", "h":              iLow = iGuess + 1
        Case "L", "l":              iHigh = iGuess - 1
        Case Else:                  GoTo CheckGuess
    End Select
Loop
CorrectGuess:
    MsgBox "I guessed number " & iGuess & " in just " & iCount & " attempts!", vbExclamation + vbOKOnly, "Rosetta Code | Guess the Number Player"
    Exit Sub
ErrHandler:
    MsgBox "Not possible. Were you cheating?!", vbCritical + vbOKOnly, "Rosetta Code | Guess the Number Player | ERROR!"
End Sub

```



## XPL0


```XPL0
include c:\cxpl\codes;  \intrinsic 'code' declarations
int Hi, Lo, Guess;
[Text(0, "Think of a number between 1 and 100 then press a key.");
if ChIn(1) then [];
Lo:= 1;  Hi:= 101;
loop    [Guess:= (Lo+Hi)/2;
        Text(0, "^M^JIs it ");  IntOut(0, Guess);
        Text(0, " (Y = yes, H = too high, L = too low)? ");
        case ChIn(1) of
          ^L,^l: Lo:= Guess;
          ^H,^h: Hi:= Guess;
          ^Y,^y: quit
        other   ChOut(0, 7\bel\);
        ];
Text(0, "^M^JYippee!");
]
```


Example output:

```txt

Think of a number between 1 and 100 then press a key.
Is it 51 (Y = yes, H = too high, L = too low)? H
Is it 26 (Y = yes, H = too high, L = too low)? H
Is it 13 (Y = yes, H = too high, L = too low)? L
Is it 19 (Y = yes, H = too high, L = too low)? H
Is it 16 (Y = yes, H = too high, L = too low)? L
Is it 17 (Y = yes, H = too high, L = too low)? Y
Yippee!

```



## zkl

Your basic binary search.

```zkl
println("Pick a number between 0 and 100 and remember it.");
low,high,g := 0,100, 20;
while(True){
   r:=ask("I guess %d; is that high, low or =? ".fmt(g)).strip().toLower();
   if(r=="="){ println("Yea!"); break; }
   if(r[0]=="h") high=g-1 else low=g+1;
   if(low==high){ println("Yea! the number is ",low); break; }
   if(low>high){ println("I'm confused!"); break; }
   g=(low + high)/2;
}
```

```txt

Pick a number between 0 and 100 and remember it.
I guess 20; is that high, low or =? l
I guess 60; is that high, low or =? h
I guess 40; is that high, low or =? h
I guess 30; is that high, low or =? h
I guess 25; is that high, low or =? l
I guess 27; is that high, low or =? h
Yea! the number is 26

```



## ZX Spectrum Basic

```zxbasic
10 LET min=1: LET max=100
20 PRINT "Think of a number between ";min;" and ";max
30 PRINT "I will try to guess your number."
40 LET guess=INT ((min+max)/2)
50 PRINT "My guess is ";guess
60 INPUT "Is it higuer than, lower than or equal to your number? ";a$
65 LET a$=a$(1)
70 IF a$="L" OR a$="l" THEN LET min=guess+1: GO TO 40
80 IF a$="H" OR a$="h" THEN LET max=guess-1: GO TO 40
90 IF a$="E" OR a$="e" THEN PRINT "Goodbye.": STOP
100 PRINT "Sorry, I didn't understand your answer.": GO TO 60
```


