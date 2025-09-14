+++
title = "Monty Hall problem"
description = ""
date = 2019-07-24T09:03:33Z
aliases = []
[extra]
id = 2972
[taxonomies]
categories = ["task", "Discrete math"]
tags = []
languages = [
  "actionscript",
  "ada",
  "algol_68",
  "apl",
  "autohotkey",
  "awk",
  "basic",
  "bbc_basic",
  "c",
  "chapel",
  "clojure",
  "cobol",
  "coldfusion",
  "common_lisp",
  "cpp",
  "csharp",
  "d",
  "dart",
  "dyalect",
  "eiffel",
  "elixir",
  "emacs_lisp",
  "erlang",
  "euphoria",
  "forth",
  "fortran",
  "freebasic",
  "go",
  "haskell",
  "hicest",
  "if_always_switch",
  "if_we_never_switch",
  "if_we_randomly_switch",
  "io",
  "j",
  "java",
  "javascript",
  "julia",
  "kotlin",
  "lua",
  "lua_torch",
  "m2000_interpreter",
  "mathematica",
  "matlab",
  "maxscript",
  "netrexx",
  "nim",
  "ocaml",
  "pari_gp",
  "pascal",
  "perl",
  "perl_6",
  "phix",
  "php",
  "picolisp",
  "pl_i",
  "powershell",
  "purebasic",
  "python",
  "r",
  "racket",
  "rexx",
  "ring",
  "ruby",
  "run_basic",
  "rust",
  "scala",
  "scheme",
  "scilab",
  "seed7",
  "sidef",
  "sinclair_zx81_basic",
  "spad",
  "stata",
  "swift",
  "tcl",
  "that_he_gives_the_player_a_second_choice_between_the_two_remaining_unopened_doors",
  "that_the_host_opens_a_different_door_revealing_a_goat_not_necessarily_door_3_and",
  "the_host_choice_is_more_complicated",
  "to_go_with_the_exact_parameters_of_the_rosetta_challenge_complicating_matters",
  "transact_sql",
  "unix_shell",
  "ursala",
  "vedit_macro_language",
  "x",
  "xpl0",
  "zkl",
]
+++

[[File:Monte_Hall_problem.jpg|600px||right]]

Suppose you're on a game show and you're given the choice of three doors.

Behind one door is a car; behind the others, goats.

The car and the goats were placed randomly behind the doors before the show.


;Rules of the game:
After you have chosen a door, the door remains closed for the time being.

The game show host, Monty Hall, who knows what is behind the doors, now has to open one of the two remaining doors, and the door he opens must have a goat behind it.

If both remaining doors have goats behind them, he chooses one randomly.

After Monty Hall opens a door with a goat, he will ask you to decide whether you want to stay with your first choice or to switch to the last remaining door.

Imagine that you chose Door 1 and the host opens Door 3, which has a goat.

He then asks you "Do you want to switch to Door Number 2?"


;The question:
Is it to your advantage to change your choice?


;Note:
The player may initially choose any of the three doors (not just Door 1), that the host opens a different door revealing a goat (not necessarily Door 3), and that he gives the player a second choice between the two remaining unopened doors.


## Task

Run random simulations of the [[wp:Monty_Hall_problem|Monty Hall]] game. Show the effects of a strategy of the contestant always keeping his first guess so it can be contrasted with the strategy of the contestant always switching his guess.

Simulate at least a thousand games using three doors for each strategy <u>and show the results</u> in such a way as to make it easy to compare the effects of each strategy.


## References

:* Stefan Krauss, X. T. Wang, "The psychology of the Monty Hall problem: Discovering psychological mechanisms for solving a tenacious brain teaser.", Journal of Experimental Psychology: General, Vol 132(1), Mar 2003, 3-22 [https://doi.org/10.1037/0096-3445.132.1.3 DOI: 10.1037/0096-3445.132.1.3]
:* A YouTube video:   [https://www.youtube.com/watch?v=4Lb-6rxZxx0 Monty Hall Problem - Numberphile].





## ActionScript


```actionscript
package {
	import flash.display.Sprite;

	public class MontyHall extends Sprite
	{
		public function MontyHall()
		{
			var iterations:int = 30000;
			var switchWins:int = 0;
			var stayWins:int = 0;

			for (var i:int = 0; i < iterations; i++)
			{
				var doors:Array = [0, 0, 0];
				doors[Math.floor(Math.random() * 3)] = 1;
				var choice:int = Math.floor(Math.random() * 3);
				var shown:int;

				do
				{
					shown = Math.floor(Math.random() * 3);
				} while (doors[shown] == 1 || shown == choice);

				stayWins += doors[choice];
				switchWins += doors[3 - choice - shown];
			}

			trace("Switching wins " + switchWins + " times. (" + (switchWins / iterations) * 100 + "%)");
			trace("Staying wins " + stayWins + " times. (" + (stayWins / iterations) * 100 + "%)");
		}
	}
}
```

Output:

```txt
Switching wins 18788 times. (62.626666666666665%)
Staying wins 11212 times. (37.37333333333333%)
```



## Ada


```ada
-- Monty Hall Game

with Ada.Text_Io; use Ada.Text_Io;
with Ada.Float_Text_Io; use Ada.Float_Text_Io;
with ada.Numerics.Discrete_Random;

procedure Monty_Stats is
   Num_Iterations : Positive := 100000;
   type Action_Type is (Stay, Switch);
   type Prize_Type is (Goat, Pig, Car);
   type Door_Index is range 1..3;
   package Random_Prize is new Ada.Numerics.Discrete_Random(Door_Index);
   use Random_Prize;
   Seed : Generator;
   Doors : array(Door_Index) of Prize_Type;

   procedure Set_Prizes is
      Prize_Index : Door_Index;
      Booby_Prize : Prize_Type := Goat;
   begin
      Reset(Seed);
      Prize_Index := Random(Seed);
      Doors(Prize_Index) := Car;
      for I in Doors'range loop
         if I /= Prize_Index then
            Doors(I) := Booby_Prize;
            Booby_Prize := Prize_Type'Succ(Booby_Prize);
         end if;
      end loop;
   end Set_Prizes;

   function Play(Action : Action_Type) return Prize_Type is
      Chosen : Door_Index := Random(Seed);
      Monty : Door_Index;
   begin
      Set_Prizes;
      for I in Doors'range loop
         if I /= Chosen and Doors(I) /= Car then
            Monty := I;
         end if;
      end loop;
      if Action = Switch then
         for I in Doors'range loop
            if I /= Monty and I /= Chosen then
               Chosen := I;
               exit;
            end if;
         end loop;
      end if;
      return Doors(Chosen);
   end Play;
   Winners : Natural;
   Pct : Float;
begin
   Winners := 0;
   for I in 1..Num_Iterations loop
      if Play(Stay) = Car then
         Winners := Winners + 1;
      end if;
   end loop;
   Put("Stay : count" & Natural'Image(Winners) & " = ");
   Pct := Float(Winners * 100) / Float(Num_Iterations);
   Put(Item => Pct, Aft => 2, Exp => 0);
   Put_Line("%");
   Winners := 0;
   for I in 1..Num_Iterations loop
      if Play(Switch) = Car then
         Winners := Winners + 1;
      end if;
   end loop;
   Put("Switch : count" & Natural'Image(Winners) & " = ");
   Pct := Float(Winners * 100) / Float(Num_Iterations);
   Put(Item => Pct, Aft => 2, Exp => 0);
   Put_Line("%");

end Monty_Stats;
```

Results

```txt
Stay : count 34308 = 34.31%
Switch : count 65695 = 65.69%
```



## ALGOL 68

```algol68
INT trials=100 000;

PROC brand = (INT n)INT: 1 + ENTIER (n * random);

PROC percent = (REAL x)STRING: fixed(100.0*x/trials,0,2)+"%";

main:
(
  INT prize, choice, show, not shown, new choice;
  INT stay winning:=0, change winning:=0, random winning:=0;
  INT doors = 3;
  [doors-1]INT other door;

  TO trials DO
     # put the prize somewhere #
     prize := brand(doors);
     # let the user choose a door #
     choice := brand(doors);
     # let us take a list of unchoosen doors #
     INT k := LWB other door;
     FOR j TO doors DO
        IF j/=choice THEN other door[k] := j; k+:=1 FI
     OD;
     # Monty opens one... #
     IF choice = prize THEN
     # staying the user will win... Monty opens a random port#
       show := other door[ brand(doors - 1) ];
       not shown := other door[ (show+1) MOD (doors - 1 ) + 1]
     ELSE # no random, Monty can open just one door... #
       IF other door[1] = prize THEN
           show := other door[2];
           not shown := other door[1]
       ELSE
           show := other door[1];
           not shown := other door[2]
       FI
     FI;

     # the user randomly choose one of the two closed doors
        (one is his/her previous choice, the second is the
        one not shown ) #
     other door[1] := choice;
     other door[2] := not shown;
     new choice := other door[ brand(doors - 1) ];
     # now let us count if it takes it or not #
     IF choice = prize THEN stay winning+:=1 FI;
     IF not shown = prize THEN change winning+:=1 FI;
     IF new choice = prize THEN random winning+:=1 FI
  OD;

  print(("Staying: ", percent(stay winning), new line ));
  print(("Changing: ", percent(change winning), new line ));
  print(("New random choice: ", percent(random winning), new line ))
)
```

Sample output:

```txt

Staying: 33.62%
Changing: 66.38%
New random choice: 50.17%

```


## APL


```apl
     ∇ Run runs;doors;i;chosen;cars;goats;swap;stay;ix;prices
[1]   ⍝0: Monthy Hall problem
[2]   ⍝1: http://rosettacode.org/wiki/Monty_Hall_problem
[3]
[4]    (⎕IO ⎕ML)←0 1
[5]    prices←0 0 1               ⍝ 0=Goat, 1=Car
[6]
[7]    ix←⊃,/{3?3}¨⍳runs          ⍝ random indexes of doors (placement of car)
[8]    doors←(runs 3)⍴prices[ix]  ⍝ matrix of doors
[9]    stay←+⌿doors[;?3]          ⍝ chose randomly one door - is it a car?
[10]   swap←runs-stay             ⍝ If not, then the other one is!
[11]
[12]   ⎕←'Swap: ',(2⍕100×(swap÷runs)),'% it''s a car'
[13]   ⎕←'Stay: ',(2⍕100×(stay÷runs)),'% it''s a car'
     ∇
```


```txt

      Run 100000
Swap:  66.54% it's a car
Stay:  33.46% it's a car

```



## AutoHotkey


```ahk
#SingleInstance, Force
Iterations = 1000
Loop, %Iterations%
{
   If Monty_Hall(1)
      Correct_Change++
   Else
      Incorrect_Change++
   If Monty_Hall(2)
      Correct_Random++
   Else
      Incorrect_Random++
   If Monty_Hall(3)
      Correct_Stay++
   Else
      Incorrect_Stay++
}
Percent_Change := round(Correct_Change / Iterations * 100)
Percent_Stay := round(Correct_Stay / Iterations * 100)
Percent_Random := round(Correct_Random / Iterations * 100)

MsgBox,, Monty Hall Problem, These are the results:`r`n`r`nWhen I changed my guess, I got %Correct_Change% of %Iterations% (that's %Incorrect_Change% incorrect). That's %Percent_Change%`% correct.`r`n`r`nWhen I randomly changed my guess, I got %Correct_Random% of %Iterations% (that's %Incorrect_Random% incorrect). That's %Percent_Random%`% correct.`r`n`r`nWhen I stayed with my first guess, I got %Correct_Stay% of %Iterations% (that's %Incorrect_Stay% incorrect). That's %Percent_Stay%`% correct.
ExitApp

Monty_Hall(Mode) ;Mode is 1 for change, 2 for random, or 3 for stay
{
	Random, guess, 1, 3
	Random, actual, 1, 3
	Random, rand, 1, 2

	show := guess = actual ? guess = 3 ? guess - rand : guess = 1 ? guess+rand : guess + 2*rand - 3 : 6 - guess - actual
	Mode := Mode = 2 ? 2*rand - 1: Mode
	Return, Mode = 1 ?  6 - guess - show = actual : guess = actual
}
```

Sample output:

```txt

These are the results:

When I changed my guess, I got 659 of 1000 (that's 341 incorrect). That's 66% correct.

When I randomly changed my guess, I got 505 of 1000 (that's 495 incorrect). That's 51% correct.

When I stayed with my first guess, I got 329 of 1000 (that's 671 incorrect). That's 32% correct.

```



## AWK


```awk
#!/bin/gawk -f

# Monty Hall problem

BEGIN {
	srand()
	doors = 3
	iterations = 10000
	# Behind a door:
	EMPTY = "empty"; PRIZE = "prize"
	# Algorithm used
  KEEP = "keep"; SWITCH="switch"; RAND="random";
  #
}
function monty_hall( choice, algorithm ) {
  # Set up doors
  for ( i=0; i<doors; i++ ) {
		door[i] = EMPTY
	}
  # One door with prize
	door[int(rand()*doors)] = PRIZE

  chosen = door[choice]
  del door[choice]

  #if you didn't choose the prize first time around then
  # that will be the alternative
	alternative = (chosen == PRIZE) ? EMPTY : PRIZE

	if( algorithm == KEEP) {
		return chosen
	}
	if( algorithm == SWITCH) {
		return alternative
	}
	return rand() <0.5 ? chosen : alternative
}

function simulate(algo){
	prizecount = 0
	for(j=0; j< iterations; j++){
		if( monty_hall( int(rand()*doors), algo) == PRIZE) {
			prizecount ++
		}
	}
	printf "  Algorithm %7s: prize count = %i, = %6.2f%%\n", \
		algo, prizecount,prizecount*100/iterations
}

BEGIN {
	print "\nMonty Hall problem simulation:"
	print doors, "doors,", iterations, "iterations.\n"
	simulate(KEEP)
	simulate(SWITCH)
	simulate(RAND)

}
```

Sample output:

```awk
bash$ ./monty_hall.awk

Monty Hall problem simulation:
3 doors, 10000 iterations.

  Algorithm    keep: prize count = 3411, =  34.11%
  Algorithm  switch: prize count = 6655, =  66.55%
  Algorithm  random: prize count = 4991, =  49.91%
bash$
```



## BASIC

```qbasic
RANDOMIZE TIMER
DIM doors(3) '0 is a goat, 1 is a car
CLS
switchWins = 0
stayWins = 0
FOR plays = 0 TO 32767
	winner = INT(RND * 3) + 1
	doors(winner) = 1'put a winner in a random door
	choice = INT(RND * 3) + 1'pick a door, any door
	DO
		shown = INT(RND * 3) + 1
	'don't show the winner or the choice
	LOOP WHILE doors(shown) = 1 OR shown = choice
	stayWins = stayWins + doors(choice) 'if you won by staying, count it
	switchWins = switchWins + doors(3 - choice - shown) 'could have switched to win
	doors(winner) = 0 'clear the doors for the next test
NEXT plays
PRINT "Switching wins"; switchWins; "times."
PRINT "Staying wins"; stayWins; "times."
```

Output:

```txt
Switching wins 21805 times.
Staying wins 10963 times.
```


==={{header|IS-BASIC}}===
<lang IS-BASIC>100 PROGRAM "MontyH.bas"
110 RANDOMIZE
120 LET NUMGAMES=1000
130 LET CHANGING,NOTCHANGING=0
140 FOR I=0 TO NUMGAMES-1
150   LET PRIZEDOOR=RND(3)+1:LET CHOSENDOOR=RND(3)+1
160   IF CHOSENDOOR=PRIZEDOOR THEN
170     LET NOTCHANGING=NOTCHANGING+1
180   ELSE
190     LET CHANGING=CHANGING+1
200   END IF
210 NEXT
220 PRINT "Num of games:";NUMGAMES
230 PRINT "Wins not changing doors:";NOTCHANGING,NOTCHANGING/NUMGAMES*100;"% of total."
240 PRINT "Wins changing doors:",CHANGING,CHANGING/NUMGAMES*100;"% of total."
```


=
## Sinclair ZX81 BASIC
=
Works with 1k of RAM.

This program could certainly be made more efficient. What is really going on, after all, is

```txt
if initial guess = car then
    sticker wins
else
    switcher wins;
```

but I take it that the point is to demonstrate the outcome to people who may <i>not</i> see that that's what is going on. I have therefore written the program in a deliberately naïve style, not assuming anything.

```basic
 10 PRINT "     WINS IF YOU"
 20 PRINT "STICK","SWITCH"
 30 LET STICK=0
 40 LET SWITCH=0
 50 FOR I=1 TO 1000
 60 LET CAR=INT (RND*3)
 70 LET GUESS=INT (RND*3)
 80 LET SHOW=INT (RND*3)
 90 IF SHOW=GUESS OR SHOW=CAR THEN GOTO 80
100 LET NEWGUESS=INT (RND*3)
110 IF NEWGUESS=GUESS OR NEWGUESS=SHOW THEN GOTO 100
120 IF GUESS=CAR THEN LET STICK=STICK+1
130 IF NEWGUESS=CAR THEN LET SWITCH=SWITCH+1
140 PRINT AT 2,0;STICK,SWITCH
150 NEXT I
```

```txt
     WINS IF YOU
STICK           SWITCH
341             659
```



## BBC BASIC


```bbcbasic
      total% = 10000
      FOR trial% = 1 TO total%
        prize_door% = RND(3) : REM. The prize is behind this door
        guess_door% = RND(3) : REM. The contestant guesses this door
        IF prize_door% = guess_door% THEN
          REM. The contestant guessed right, reveal either of the others
          reveal_door% = RND(2)
          IF prize_door% = 1 reveal_door% += 1
          IF prize_door% = 2 AND reveal_door% = 2 reveal_door% = 3
        ELSE
          REM. The contestant guessed wrong, so reveal the non-prize door
          reveal_door% = prize_door% EOR guess_door%
        ENDIF
        stick_door% = guess_door% : REM. The sticker doesn't change his mind
        swap_door% = guess_door% EOR reveal_door% : REM. but the swapper does
        IF stick_door% = prize_door% sticker% += 1
        IF swap_door% = prize_door% swapper% += 1
      NEXT trial%
      PRINT "After a total of ";total%;" trials,"
      PRINT "The 'sticker' won ";sticker%;" times (";INT(sticker%/total%*100);"%)"
      PRINT "The 'swapper' won ";swapper%;" times (";INT(swapper%/total%*100);"%)"
```

Output:

```txt

After a total of 10000 trials,
The 'sticker' won 3379 times (33%)
The 'swapper' won 6621 times (66%)

```



## C



```c
//Evidence of the Monty Hall solution.

#include <stdio.h>
#include <stdlib.h>
#include <time.h>

#define GAMES 3000000

int main(void){
    unsigned i, j, k, choice, winsbyswitch=0, door[3];

    srand(time(NULL));                                                          //initialize random seed.
    for(i=0; i<GAMES; i++){
        door[0] = (!(rand()%2)) ? 1: 0;                                         //give door 1 either a car or a goat randomly.
        if(door[0]) door[1]=door[2]=0;                                          //if 1st door has car, give other doors goats.
        else{ door[1] = (!(rand()%2)) ? 1: 0; door[2] = (!door[1]) ? 1: 0; }    //else, give 2nd door car or goat, give 3rd door what's left.
        choice = rand()%3;                                                      //choose a random door.

        //if the next door has a goat, and the following door has a car, or vice versa, you'd win if you switch.
        if(((!(door[((choice+1)%3)])) && (door[((choice+2)%3)])) || (!(door[((choice+2)%3)]) && (door[((choice+1)%3)]))) winsbyswitch++;
    }
    printf("\nAfter %u games, I won %u by switching.  That is %f%%. ", GAMES, winsbyswitch, (float)winsbyswitch*100.0/(float)i);
}

```


Output of one run:


```txt
After 3000000 games, I won 1999747 by switching.  That is 66.658233%.
```


## C#
```c#
using System;

class Program
{
    static void Main(string[] args)
    {
        int switchWins = 0;
        int stayWins = 0;

        Random gen = new Random();

        for(int plays = 0; plays < 1000000; plays++ )
        {
            int[] doors = {0,0,0};//0 is a goat, 1 is a car

            var winner = gen.Next(3);
            doors[winner] = 1; //put a winner in a random door

	    int choice = gen.Next(3); //pick a door, any door
	    int shown; //the shown door
	    do
            {
	        shown = gen.Next(3);
	    }
            while (doors[shown] == 1 || shown == choice); //don't show the winner or the choice

	    stayWins += doors[choice]; //if you won by staying, count it

            //the switched (last remaining) door is (3 - choice - shown), because 0+1+2=3
            switchWins += doors[3 - choice - shown];
        }

        Console.Out.WriteLine("Staying wins " + stayWins + " times.");
        Console.Out.WriteLine("Switching wins " + switchWins + " times.");
    }
}
```

Sample output:

```txt

Staying wins:    333830
Switching wins:  666170

```



## C++


```cpp
#include <iostream>
#include <cstdlib>
#include <ctime>

int randint(int n)
{
  return (1.0*n*std::rand())/(1.0+RAND_MAX);
}

int other(int doorA, int doorB)
{
  int doorC;
  if (doorA == doorB)
  {
    doorC = randint(2);
    if (doorC >= doorA)
      ++doorC;
  }
  else
  {
    for (doorC = 0; doorC == doorA || doorC == doorB; ++doorC)
    {
      // empty
    }
  }
  return doorC;
}

int check(int games, bool change)
{
  int win_count = 0;
  for (int game = 0; game < games; ++game)
  {
    int const winning_door = randint(3);
    int const original_choice = randint(3);
    int open_door = other(original_choice, winning_door);

    int const selected_door = change?
                                other(open_door, original_choice)
                              : original_choice;

    if (selected_door == winning_door)
      ++win_count;
  }

  return win_count;
}

int main()
{
  std::srand(std::time(0));

  int games = 10000;
  int wins_stay = check(games, false);
  int wins_change = check(games, true);
  std::cout << "staying: " << 100.0*wins_stay/games << "%, changing: " << 100.0*wins_change/games << "%\n";
}
```

Sample output:
 staying: 33.73%, changing: 66.9%



## Chapel


```chapel
use Random;

param doors: int = 3;
config const games: int = 1000;

config const maxTasks = 32;
var numTasks = 1;
while( games / numTasks > 1000000 && numTasks < maxTasks ) do numTasks += 1;
const tasks = 1..#numTasks;
const games_per_task = games / numTasks ;
const remaining_games = games % numTasks ;

var wins_by_stay: [tasks] int;

coforall task in tasks {

  var rand = new  RandomStream();

  for game in 1..#games_per_task {
    var player_door =  (rand.getNext() * 1000): int % doors ;
    var winning_door = (rand.getNext() * 1000): int % doors ;
    if player_door == winning_door then
      wins_by_stay[ task ] += 1;
  }

  if task == tasks.last then {
    for game in 1..#remaining_games {
      var player_door =  (rand.getNext() * 1000): int % doors ;
      var winning_door = (rand.getNext() * 1000): int % doors ;
      if player_door == winning_door then
        wins_by_stay[ task ] += 1;
    }
  }

}

var total_by_stay = + reduce wins_by_stay;

var total_by_switch = games - total_by_stay;
var percent_by_stay = ((total_by_stay: real) / games) * 100;
var percent_by_switch = ((total_by_switch: real) / games) * 100;

writeln( "Wins by staying: ", total_by_stay, " or ", percent_by_stay, "%" );
writeln( "Wins by switching: ", total_by_switch, " or ", percent_by_switch, "%" );
if ( total_by_stay > total_by_switch ){
  writeln( "Staying is the superior method." );
} else if( total_by_stay < total_by_switch ){
  writeln( "Switching is the superior method." );
} else {
  writeln( "Both methods are equal." );
}

```

Sample output:

```txt

Wins by staying: 354 or 35.4%
Wins by switching: 646 or 64.6%
Switching is the superior method.

```




## Clojure


```clojure
(ns monty-hall-problem
  (:use [clojure.contrib.seq :only (shuffle)]))

(defn play-game [staying]
  (let [doors (shuffle [:goat :goat :car])
        choice (rand-int 3)
        [a b] (filter #(not= choice %) (range 3))
        alternative (if (= :goat (nth doors a)) b a)]
    (= :car (nth doors (if staying choice alternative)))))

(defn simulate [staying times]
  (let [wins (reduce (fn [counter _] (if (play-game staying) (inc counter) counter))
                     0
                     (range times))]
    (str "wins " wins " times out of " times)))

```


```clojure
monty-hall-problem> (println "staying:" (simulate true 1000))
staying: wins 337 times out of 1000
nil
monty-hall-problem> (println "switching:" (simulate false 1000))
switching: wins 638 times out of 1000
nil

```



## COBOL

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. monty-hall.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       78  Num-Games               VALUE 1000000.

       *> These are needed so the values are passed to
       *> get-rand-int correctly.
       01  One                     PIC 9 VALUE 1.
       01  Three                   PIC 9 VALUE 3.

       01  doors-area.
           03  doors               PIC 9 OCCURS 3 TIMES.

       01  choice                  PIC 9.
       01  shown                   PIC 9.
       01  winner                  PIC 9.

       01  switch-wins             PIC 9(7).
       01  stay-wins               PIC 9(7).

       01  stay-wins-percent       PIC Z9.99.
       01  switch-wins-percent     PIC Z9.99.

       PROCEDURE DIVISION.
           PERFORM Num-Games TIMES
               MOVE 0 TO doors (winner)

               CALL "get-rand-int" USING CONTENT One, Three,
                   REFERENCE winner
               MOVE 1 TO doors (winner)

               CALL "get-rand-int" USING CONTENT One, Three,
                   REFERENCE choice

               PERFORM WITH TEST AFTER
                       UNTIL NOT(shown = winner OR choice)
                   CALL "get-rand-int" USING CONTENT One, Three,
                       REFERENCE shown
               END-PERFORM

               ADD doors (choice) TO stay-wins
               ADD doors (6 - choice - shown) TO switch-wins
           END-PERFORM

           COMPUTE stay-wins-percent ROUNDED =
               stay-wins / Num-Games * 100
           COMPUTE switch-wins-percent ROUNDED =
               switch-wins / Num-Games * 100

           DISPLAY "Staying wins   " stay-wins " times ("
               stay-wins-percent "%)."
           DISPLAY "Switching wins " switch-wins " times ("
               switch-wins-percent "%)."
           .

       IDENTIFICATION DIVISION.
       PROGRAM-ID. get-rand-int.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  call-flag               PIC X VALUE "Y".
           88  first-call          VALUE "Y", FALSE "N".

       01  num-range               PIC 9.

       LINKAGE SECTION.
       01  min-num                 PIC 9.
       01  max-num                 PIC 9.

       01  ret                     PIC 9.

       PROCEDURE DIVISION USING min-num, max-num, ret.
           *> Seed RANDOM once.
           IF first-call
               MOVE FUNCTION RANDOM(FUNCTION CURRENT-DATE (9:8))
                   TO num-range
               SET first-call TO FALSE
           END-IF

           COMPUTE num-range = max-num - min-num + 1
           COMPUTE ret =
              FUNCTION MOD(FUNCTION RANDOM * 100000, num-range)
              + min-num
           .
       END PROGRAM get-rand-int.

       END PROGRAM monty-hall.
```


```txt

Staying wins   0333396 times (33.34%).
Switching wins 0666604 times (66.66%).

```



## ColdFusion


```cfm><cfscript

    function runmontyhall(num_tests) {
	// number of wins when player switches after original selection
	switch_wins	= 0;
	// number of wins when players "sticks" with original selection
	stick_wins  = 0;
	// run all the tests
	for(i=1;i<=num_tests;i++) {
	    // unconditioned potential for selection of each door
	    doors 	= [0,0,0];
	    // winning door is randomly assigned...
	    winner 	= randrange(1,3);
	    // ...and actualized in the array of real doors
	    doors[winner] = 1;
	    // player chooses one of three doors
	    choice 	= randrange(1,3);
	    do {
		// monty randomly reveals a door...
		shown = randrange(1,3);
	    }
	    // ...but monty only reveals empty doors;
	    // he will not reveal the door that the player has choosen
	    // nor will he reveal the winning door
	    while(shown==choice || doors[shown]==1);
	    // when the door the player originally selected is the winner, the "stick" option gains a point
	    stick_wins  += doors[choice];
	    // to calculate the number of times the player would have won with a "switch", subtract the
	    // "value" of the chosen, "stuck-to" door from 1, the possible number of wins if the player
	    // chose and stuck with the winning door (1), the player would not have won by switching, so
	    // the value is 1-1=0 if the player chose and stuck with a losing door (0), the player would
	    // have won by switching, so the value is 1-0=1
	    switch_wins += 1-doors[choice];
	}
	// finally, simply run the percentages for each outcome
	stick_percentage	= (stick_wins/num_tests)*100;
	switch_percentage	= (switch_wins/num_tests)*100;
	writeoutput('Number of Tests:  ' & num_tests);
	writeoutput('<br />Stick Wins: ' & stick_wins & '  ['& stick_percentage &'%]');
	writeoutput('<br />Switch Wins: ' & switch_wins & '  ['& switch_percentage &'%]');
    }
    runmontyhall(10000);
</cfscript>
```

Output:

```txt

Tests: 10,000  |  Switching wins:  6655 [66.55%]  |  Sticking wins:  3345 [33.45%]

```



## Common Lisp


```lisp
(defun make-round ()
  (let ((array (make-array 3
                           :element-type 'bit
                           :initial-element 0)))
    (setf (bit array (random 3)) 1)
    array))

(defun show-goat (initial-choice array)
  (loop for i = (random 3)
        when (and (/= initial-choice i)
                  (zerop (bit array i)))
          return i))

(defun won? (array i)
  (= 1 (bit array i)))
```


```lisp
CL-USER> (progn (loop repeat #1=(expt 10 6)
                      for round = (make-round)
                      for initial = (random 3)
                      for goat = (show-goat initial round)
                      for choice = (loop for i = (random 3)
                                         when (and (/= i initial)
                                                   (/= i goat))
                                           return i)
                      when (won? round (random 3))
                        sum 1 into result-stay
                      when (won? round choice)
                        sum 1 into result-switch
                      finally (progn (format t "Stay: ~S%~%" (float (/ result-stay
                                                                       #1# 1/100)))
                                     (format t "Switch: ~S%~%" (float (/ result-switch
                                                                         #1# 1/100))))))
Stay: 33.2716%
Switch: 66.6593%
```



```lisp

;Find out how often we win if we always switch
(defun rand-elt (s)
  (elt s (random (length s))))

(defun monty ()
  (let* ((doors '(0 1 2))
	 (prize (random 3));possible values: 0, 1, 2
	 (pick (random 3))
	 (opened (rand-elt (remove pick (remove prize doors))));monty opens a door which is not your pick and not the prize
	 (other (car (remove pick (remove opened doors)))))	 ;you decide to switch to the one other door that is not your pick and not opened
    (= prize other))) ; did you switch to the prize?

(defun monty-trials (n)
  (count t (loop for x from 1 to n collect (monty))))

```



## D


```d
import std.stdio, std.random;

void main() {
  int switchWins, stayWins;

  while (switchWins + stayWins < 100_000) {
    immutable carPos = uniform(0, 3);  // Which door is car behind?
    immutable pickPos = uniform(0, 3);  // Contestant's initial pick.
    int openPos;  // Which door is opened by Monty Hall?

    // Monty can't open the door you picked or the one with the car
    // behind it.
    do {
      openPos = uniform(0, 3);
    } while(openPos == pickPos || openPos == carPos);

    int switchPos;
    // Find position that's not currently picked by contestant and
    // was not opened by Monty already.
    for (; pickPos==switchPos || openPos==switchPos; switchPos++) {}

    if (pickPos == carPos)
      stayWins++;
    else if (switchPos == carPos)
      switchWins++;
    else
      assert(0);  // Can't happen.
  }

  writefln("Switching/Staying wins: %d %d", switchWins, stayWins);
}
```

```txt
Switching/Staying wins: 66609 33391
```



## Dart

The class Game attempts to hide the implementation as much as possible, the play() function does not use any specifics of the implementation.

```dart
int rand(int max) => (Math.random()*max).toInt();

class Game {
  int _prize;
  int _open;
  int _chosen;

  Game() {
    _prize=rand(3);
    _open=null;
    _chosen=null;
  }

  void choose(int door) {
    _chosen=door;
  }

  void reveal() {
    if(_prize==_chosen) {
      int toopen=rand(2);
      if (toopen>=_prize)
        toopen++;
      _open=toopen;
    } else {
      for(int i=0;i<3;i++)
        if(_prize!=i && _chosen!=i) {
          _open=i;
          break;
        }
    }
  }

  void change() {
    for(int i=0;i<3;i++)
      if(_chosen!=i && _open!=i) {
        _chosen=i;
        break;
      }
  }

  bool hasWon() => _prize==_chosen;

  String toString() {
    String res="Prize is behind door $_prize";
    if(_chosen!=null) res+=", player has chosen door $_chosen";
    if(_open!=null) res+=", door $_open is open";
    return res;
  }
}

void play(int count, bool swap) {
  int wins=0;

  for(int i=0;i<count;i++) {
    Game game=new Game();
    game.choose(rand(3));
    game.reveal();
    if(swap)
      game.change();
    if(game.hasWon())
      wins++;
  }
  String withWithout=swap?"with":"without";
  double percent=(wins*100.0)/count;
  print("playing $withWithout switching won $percent%");
}

test() {
  for(int i=0;i<5;i++) {
    Game g=new Game();
    g.choose(i%3);
    g.reveal();
    print(g);
    g.change();
    print(g);
    print("win==${g.hasWon()}");
  }
}

main() {
  play(10000,false);
  play(10000,true);
}
```


```txt
playing without switching won 33.32%
playing with switching won 67.63%
```



## Dyalect


```dyalect
var switchWins = 0
var stayWins = 0

for plays in 0..1000000 {
    var doors = [0 ,0, 0]
    var winner = rnd(3)
    doors[winner] = 1
    var choice = rnd(3)
    var shown = rnd(3)

    while doors[shown] == 1 || shown == choice {
        shown = rnd(3)
    }

    stayWins += doors[choice]
    switchWins += doors[3 - choice - shown]
}

print("Staying wins \(stayWins) times.")
print("Switching wins \(switchWins) times.")
```


```txt
Staying wins 431683 times.
Switching wins 568318 times.
```



## Eiffel


```eiffel

note
	description: "[
					Monty Hall Problem as an Eiffel Solution

					1. Set the stage: Randomly place car and two goats behind doors 1, 2 and 3.
					2. Monty offers choice of doors --> Contestant will choose a random door or always one door.
					2a. Door has Goat - door remains closed
					2b. Door has Car - door remains closed
					3. Monty offers cash --> Contestant takes or refuses cash.
					3a. Takes cash: Contestant is Cash winner and door is revealed. Car Loser if car door revealed.
					3b. Refuses cash: Leads to offer to switch doors.
					4. Monty offers door switch --> Contestant chooses to stay or change.
					5. Door reveal: Contestant refused cash and did or did not door switch. Either way: Reveal!
					6. Winner and Loser based on door reveal of prize.

					Car Winner: Chooses car door
					Cash Winner: Chooses cash over any door
					Goat Loser: Chooses goat door
					Car Loser: Chooses cash over car door or switches from car door to goat door
					]"
	date: "$Date$"
	revision: "$Revision$"

class
	MH_APPLICATION

create
	make

feature {NONE} -- Initialization

	make
			-- Initialize Current.
		do
			play_lets_make_a_deal
		ensure
			played_1000_games: game_count = times_to_play
		end

feature {NONE} -- Implementation: Access

	live_contestant: attached like contestant
			-- Attached version of `contestant'
		do
			if attached contestant as al_contestant then
				Result := al_contestant
			else
				create Result
				check not_attached_contestant: False end
			end
		end

	contestant: detachable TUPLE [first_door_choice, second_door_choice: like door_number_anchor; takes_cash, switches_door: BOOLEAN]
			-- Contestant for Current.

	active_stage_door (a_door: like door_anchor): attached like door_anchor
			-- Attached version of `a_door'.
		do
			if attached a_door as al_door then
				Result := al_door
			else
				create Result
				check not_attached_door: False end
			end
		end

	door_1, door_2, door_3: like door_anchor
			-- Doors with prize names and flags for goat and open (revealed).

feature {NONE} -- Implementation: Status

	game_count, car_win_count, cash_win_count, car_loss_count, goat_loss_count, goat_avoidance_count: like counter_anchor
	switch_count, switch_win_count: like counter_anchor
	no_switch_count, no_switch_win_count: like counter_anchor
			-- Counts of games played, wins and losses based on car, cash or goat.

feature {NONE} -- Implementation: Basic Operations

	prepare_stage
			-- Prepare the stage in terms of what doors have what prizes.
		do
			inspect new_random_of (3)
			when 1 then
				door_1 := door_with_car
				door_2 := door_with_goat
				door_3 := door_with_goat
			when 2 then
				door_1 := door_with_goat
				door_2 := door_with_car
				door_3 := door_with_goat
			when 3 then
				door_1 := door_with_goat
				door_2 := door_with_goat
				door_3 := door_with_car
			end
			active_stage_door (door_1).number := 1
			active_stage_door (door_2).number := 2
			active_stage_door (door_3).number := 3
		ensure
			door_has_prize: not active_stage_door (door_1).is_goat or
							not active_stage_door (door_2).is_goat or
							not active_stage_door (door_3).is_goat
			consistent_door_numbers: active_stage_door (door_1).number = 1 and
										active_stage_door (door_2).number = 2 and
										active_stage_door (door_3).number = 3
		end

	door_number_having_prize: like door_number_anchor
			-- What door number has the car?
		do
			if not active_stage_door (door_1).is_goat then
				Result := 1
			elseif not active_stage_door (door_2).is_goat then
				Result := 2
			elseif not active_stage_door (door_3).is_goat then
				Result := 3
			else
				check prize_not_set: False end
			end
		ensure
			one_to_three: between_1_and_x_inclusive (3, Result)
		end

	door_with_car: attached like door_anchor
			-- Create a door with a car.
		do
			create Result
			Result.name := prize
		ensure
			not_empty: not Result.name.is_empty
			name_is_prize: Result.name.same_string (prize)
		end

	door_with_goat: attached like door_anchor
			-- Create a door with a goat
		do
			create Result
			Result.name := gag_gift
			Result.is_goat := True
		ensure
			not_empty: not Result.name.is_empty
			name_is_prize: Result.name.same_string (gag_gift)
			is_gag_gift: Result.is_goat
		end

	next_contestant: attached like live_contestant
			-- The next contestant on Let's Make a Deal!
		do
			create Result
			Result.first_door_choice := new_random_of (3)
			Result.second_door_choice := choose_another_door (Result.first_door_choice)
			Result.takes_cash := random_true_or_false
			if not Result.takes_cash then
				Result.switches_door := random_true_or_false
			end
		ensure
			choices_one_to_three: Result.first_door_choice <= 3 and Result.second_door_choice <= 3
			switch_door_implies_no_cash_taken: Result.switches_door implies not Result.takes_cash
		end

	choose_another_door (a_first_choice: like door_number_anchor): like door_number_anchor
			-- Make a choice from the remaining doors
		require
			one_to_three: between_1_and_x_inclusive (3, a_first_choice)
		do
			Result := new_random_of (3)
			from until Result /= a_first_choice
			loop
				Result := new_random_of (3)
			end
		ensure
			first_choice_not_second: a_first_choice /= Result
			result_one_to_three: between_1_and_x_inclusive (3, Result)
		end

	play_lets_make_a_deal
			-- Play the game 1000 times
		local
			l_car_win, l_car_loss, l_cash_win, l_goat_loss, l_goat_avoided: BOOLEAN
		do
			from
				game_count := 0
			invariant
				consistent_win_loss_counts: (game_count = (car_win_count + cash_win_count + goat_loss_count))
				consistent_loss_avoidance_counts: (game_count = (car_loss_count + goat_avoidance_count))
			until
				game_count >= times_to_play
			loop
				prepare_stage
				contestant := next_contestant
				l_cash_win := (live_contestant.takes_cash)

				l_car_win := (not l_cash_win and
								(not live_contestant.switches_door and live_contestant.first_door_choice = door_number_having_prize) or
								(live_contestant.switches_door and live_contestant.second_door_choice = door_number_having_prize))

				l_car_loss := (not live_contestant.switches_door and live_contestant.first_door_choice /= door_number_having_prize) or
									(live_contestant.switches_door and live_contestant.second_door_choice /= door_number_having_prize)

				l_goat_loss := (not l_car_win and not l_cash_win)

				l_goat_avoided := (not live_contestant.switches_door and live_contestant.first_door_choice = door_number_having_prize) or
									(live_contestant.switches_door and live_contestant.second_door_choice = door_number_having_prize)

				check consistent_goats: l_goat_loss implies not l_goat_avoided end
				check consistent_car_win: l_car_win implies not l_car_loss and not l_cash_win and not l_goat_loss end
				check consistent_cash_win: l_cash_win implies not l_car_win and not l_goat_loss end
				check consistent_goat_avoidance: l_goat_avoided implies (l_car_win or l_cash_win) and not l_goat_loss end
				check consistent_car_loss: l_car_loss implies l_cash_win or l_goat_loss end

				if l_car_win then car_win_count := car_win_count + 1 end
				if l_cash_win then cash_win_count := cash_win_count + 1 end
				if l_goat_loss then goat_loss_count := goat_loss_count + 1 end
				if l_car_loss then car_loss_count := car_loss_count + 1 end
				if l_goat_avoided then goat_avoidance_count := goat_avoidance_count + 1	end

				if live_contestant.switches_door then
					switch_count := switch_count + 1
					if l_car_win then
						switch_win_count := switch_win_count + 1
					end
				else -- if not live_contestant.takes_cash and not live_contestant.switches_door then
					no_switch_count := no_switch_count + 1
					if l_car_win or l_cash_win then
						no_switch_win_count := no_switch_win_count + 1
					end
				end


				game_count := game_count + 1
			end
			print ("%NCar Wins:%T%T " + car_win_count.out +
					"%NCash Wins:%T%T " + cash_win_count.out +
					"%NGoat Losses:%T%T " + goat_loss_count.out +
					"%N-----------------------------" +
					"%NTotal Win/Loss:%T%T" + (car_win_count + cash_win_count + goat_loss_count).out +
					"%N%N" +
					"%NCar Losses:%T%T " + car_loss_count.out +
					"%NGoats Avoided:%T%T " + goat_avoidance_count.out +
					"%N-----------------------------" +
					"%NTotal Loss/Avoid:%T" + (car_loss_count + goat_avoidance_count).out +
					"%N-----------------------------" +
					"%NStaying Count/Win:%T" + no_switch_count.out + "/" + no_switch_win_count.out + " = " + (no_switch_win_count / no_switch_count * 100).out + " %%" +
					"%NSwitch  Count/Win:%T" + switch_count.out + "/" + switch_win_count.out + " = " + (switch_win_count / switch_count * 100).out + " %%"
					)
		end

feature {NONE} -- Implementation: Random Numbers

	last_random: like random_number_anchor
			-- The last random number chosen.

	random_true_or_false: BOOLEAN
			-- A randome True or False
		do
			Result := new_random_of (2) = 2
		end

	new_random_of (a_number: like random_number_anchor): like door_number_anchor
			-- A random number from 1 to `a_number'.
		do
			Result := (new_random \\ a_number + 1).as_natural_8
		end

	new_random: like random_number_anchor
			-- Random integer
			-- Each call returns another random number.
		do
			random_sequence.forth
			Result := random_sequence.item
			last_random := Result
		ensure
			old_random_not_new: old last_random /= last_random
		end

	random_sequence: RANDOM
			-- Random sequence seeded from clock when called.
		attribute
			create Result.set_seed ((create {TIME}.make_now).milli_second)
		end

feature {NONE} -- Implementation: Constants

	times_to_play: NATURAL_16 = 1000
			-- Times to play the game.

	prize: STRING = "Car"
			-- Name of the prize

	gag_gift: STRING = "Goat"
			-- Name of the gag gift

	door_anchor: detachable TUPLE [number: like door_number_anchor; name: STRING; is_goat, is_open: BOOLEAN]
			-- Type anchor for door tuples.

	door_number_anchor: NATURAL_8
			-- Type anchor for door numbers.

	random_number_anchor: INTEGER
			-- Type anchor for random numbers.

	counter_anchor: NATURAL_16
			-- Type anchor for counters.

feature {NONE} -- Implementation: Contract Support

	between_1_and_x_inclusive (a_number, a_value: like door_number_anchor): BOOLEAN
			-- Is `a_value' between 1 and `a_number'?
		do
			Result := (a_value > 0) and (a_value <= a_number)
		end

end

```

```txt

Car Wins:                177
Cash Wins:               486
Goat Losses:             337
-----------------------------
Total Win/Loss:         1000


Car Losses:              657
Goats Avoided:           343
-----------------------------
Total Loss/Avoid:       1000
-----------------------------
Staying Count/Win:      742/573 = 77.223719676549862 %
Switch  Count/Win:      258/90 = 34.883720930232556 %

```



## Elixir


```elixir
defmodule MontyHall do
  def simulate(n) do
    {stay, switch} = simulate(n, 0, 0)
    :io.format "Staying wins   ~w times (~.3f%)~n", [stay,   100 * stay   / n]
    :io.format "Switching wins ~w times (~.3f%)~n", [switch, 100 * switch / n]
  end

  defp simulate(0, stay, switch), do: {stay, switch}
  defp simulate(n, stay, switch) do
    doors = Enum.shuffle([:goat, :goat, :car])
    guess = :rand.uniform(3) - 1
    [choice] = [0,1,2] -- [guess, shown(doors, guess)]
    if Enum.at(doors, choice) == :car, do: simulate(n-1, stay, switch+1),
                                     else: simulate(n-1, stay+1, switch)
  end

  defp shown(doors, guess) do
    [i, j] = Enum.shuffle([0,1,2] -- [guess])
    if Enum.at(doors, i) == :goat, do: i, else: j
  end
end

MontyHall.simulate(10000)
```


```txt

Staying wins   3397 times (33.970%)
Switching wins 6603 times (66.030%)

```



## Emacs Lisp

```lisp

(defun montyhall (keep)
  (let
      ((prize (random 3))
       (choice (random 3)))
    (if keep (= prize choice)
      (/= prize choice))))


(let ((cnt 0))
  (dotimes (i 10000)
    (and (montyhall t) (setq cnt (1+ cnt))))
  (princ (format "Strategy keep: %.3f %%" (/ cnt 100.0))))

(let ((cnt 0))
  (dotimes (i 10000)
    (and (montyhall nil) (setq cnt (1+ cnt))))
  (princ (format "Strategy switch: %.3f %%" (/ cnt 100.0))))

```


```txt

Strategy keep: 34.410 %
Strategy switch: 66.430 %

```




## Erlang


```erlang
-module(monty_hall).

-export([main/0]).

main() ->
	random:seed(now()),
	{WinStay, WinSwitch} = experiment(100000, 0, 0),
	io:format("Switching wins ~p times.\n", [WinSwitch]),
	io:format("Staying wins ~p times.\n", [WinStay]).

experiment(0, WinStay, WinSwitch) ->
	{WinStay, WinSwitch};
experiment(N, WinStay, WinSwitch) ->
	Doors = setelement(random:uniform(3), {0,0,0}, 1),
	SelectedDoor = random:uniform(3),
	OpenDoor = open_door(Doors, SelectedDoor),
	experiment(
		N - 1,
		WinStay + element(SelectedDoor, Doors),
		WinSwitch + element(6 - (SelectedDoor + OpenDoor), Doors) ).

open_door(Doors,SelectedDoor) ->
	OpenDoor = random:uniform(3),
	case (element(OpenDoor, Doors) =:= 1) or (OpenDoor =:= SelectedDoor) of
		true -> open_door(Doors, SelectedDoor);
		false -> OpenDoor
	end.

```

Sample Output:

```txt
Switching wins 66595 times.
Staying wins 33405 times.
```



## Euphoria


```euphoria
integer switchWins, stayWins
switchWins = 0
stayWins = 0

integer winner, choice, shown

for plays = 1 to 10000 do
    winner = rand(3)
    choice = rand(3)
    while 1 do
        shown = rand(3)
        if shown != winner and shown != choice then
            exit
        end if
    end while
    stayWins += choice = winner
    switchWins += 6-choice-shown = winner
end for
printf(1, "Switching wins %d times\n", switchWins)
printf(1, "Staying wins %d times\n", stayWins)

```

Sample Output:<br />
:Switching wins 6697 times<br />
:Staying wins 3303 times

=={{header|F_Sharp|F#}}==
I don't bother with having Monty "pick" a door, since you only win if you initially pick a loser in the switch strategy and you only win if you initially pick a winner in the stay strategy so there doesn't seem to be much sense in playing around the background having Monty "pick" doors.  Makes it pretty simple to see why it's always good to switch.

```fsharp
open System
let monty nSims =
    let rnd = new Random()
    let SwitchGame() =
        let winner, pick = rnd.Next(0,3), rnd.Next(0,3)
        if winner <> pick then 1 else 0

    let StayGame() =
        let winner, pick = rnd.Next(0,3), rnd.Next(0,3)
        if winner = pick then 1 else 0

    let Wins (f:unit -> int) = seq {for i in [1..nSims] -> f()} |> Seq.sum
    printfn "Stay: %d wins out of %d - Switch: %d wins out of %d" (Wins StayGame) nSims (Wins SwitchGame) nSims
```

Sample Output:

```txt
Stay: 332874 wins out of 1000000 - Switch: 667369 wins out of 1000000
```


I had a very polite suggestion that I simulate Monty's "pick" so I'm putting in a version that does that.  I compare the outcome with my original outcome and, unsurprisingly, show that this is essentially a noop that has no bearing on the output, but I (kind of) get where the request is coming from so here's that version...

```fsharp
let montySlower nSims =
    let rnd = new Random()
    let MontyPick winner pick =
        if pick = winner then
            [0..2] |> Seq.filter (fun i -> i <> pick) |> Seq.nth (rnd.Next(0,2))
        else
            3 - pick - winner
    let SwitchGame() =
        let winner, pick = rnd.Next(0,3), rnd.Next(0,3)
        let monty = MontyPick winner pick
        let pickFinal = 3 - monty - pick

        // Show that Monty's pick has no effect...

        if (winner <> pick) <> (pickFinal = winner) then
            printfn "Monty's selection actually had an effect!"
        if pickFinal = winner then 1 else 0

    let StayGame() =
        let winner, pick = rnd.Next(0,3), rnd.Next(0,3)
        let monty = MontyPick winner pick

        // This one's even more obvious than the above since pickFinal
        // is precisely the same as pick

        let pickFinal = pick
        if (winner = pick) <> (winner = pickFinal) then
            printfn "Monty's selection actually had an effect!"
        if winner = pickFinal then 1 else 0

    let Wins (f:unit -> int) = seq {for i in [1..nSims] -> f()} |> Seq.sum
    printfn "Stay: %d wins out of %d - Switch: %d wins out of %d" (Wins StayGame) nSims (Wins SwitchGame) nSims
```



## Forth



### version 1


```forth
include random.fs

variable stay-wins
variable switch-wins

: trial ( -- )
  3 random 3 random ( prize choice )
  = if 1 stay-wins +!
  else 1 switch-wins +!
  then ;
: trials ( n -- )
  0 stay-wins ! 0 switch-wins !
  dup 0 do trial loop
  cr   stay-wins @ . [char] / emit dup . ." staying wins"
  cr switch-wins @ . [char] / emit     . ." switching wins" ;

1000 trials
```


or in iForth:


```forth
0 value stay-wins
0 value switch-wins

: trial ( -- )
  3 choose 3 choose ( -- prize choice )
  = IF  1 +TO stay-wins exit  ENDIF
  1 +TO switch-wins ;

: trials ( n -- )
  CLEAR stay-wins
  CLEAR switch-wins
  dup 0 ?DO  trial  LOOP
  CR   stay-wins DEC. ." / " dup DEC. ." staying wins,"
  CR switch-wins DEC. ." / "     DEC. ." switching wins." ;
```


With output:


```txt
 FORTH> 100000000 trials
 33336877 / 100000000 staying wins,
 66663123 / 100000000 switching wins. ok
```



### version 2

While Forthers are known (and regarded) for always simplifying the problem, I think version 1 is missing the point here. The optimization can only be done if one already understands the game. For what it's worth, here is a simulation that takes all the turns of the game.

```Forth
require random.fs
here seed !

1000000 constant rounds
variable wins
variable car
variable firstPick
variable revealed
defer applyStrategy

: isCar		( u - f )  car @ = ;
: remaining	( u u - u )  3 swap - swap - ;
: setup		3 random car ! ;
: choose	3 random firstPick ! ;
: otherGoat	( - u )  car @ firstPick @ remaining ;
: randomGoat	( - u )  car @ 1+ 2 random + 3 mod ;
: reveal	firstPick @ isCar IF randomGoat ELSE otherGoat THEN revealed ! ;
: keep		( - u )  firstPick @ ;
: switch	( - u )  firstPick @ revealed @ remaining ;
: open		( u - f )  isCar ;
: play		( - f )  setup choose reveal applyStrategy open ;
: record	( f )  1 and wins +! ;
: run 		0 wins !  rounds 0 ?DO play record LOOP ;
: result	wins @ 0 d>f  rounds 0 d>f  f/  100e f* ;
: .result	result f. '%' emit ;

' keep IS applyStrategy    run  ." Keep door   => " .result cr
' switch IS applyStrategy  run  ." Switch door => " .result cr
bye
```


```txt

Keep door   => 33.2922 %
Switch door => 66.7207 %

```


=={{header|Fōrmulæ}}==

In [http://wiki.formulae.org/Monty_Hall_problem this] page you can see the solution of this task.

Fōrmulæ programs are not textual, visualization/edition of programs is done showing/manipulating structures but not text ([http://wiki.formulae.org/Editing_F%C5%8Drmul%C3%A6_expressions more info]). Moreover, there can be multiple visual representations of the same program. Even though it is possible to have textual representation &mdash;i.e. XML, JSON&mdash; they are intended for transportation effects more than visualization and edition.

The option to show Fōrmulæ programs and their results is showing images. Unfortunately images cannot be uploaded in Rosetta Code.


## Fortran

```fortran
PROGRAM MONTYHALL

  IMPLICIT NONE

  INTEGER, PARAMETER :: trials = 10000
  INTEGER :: i, choice, prize, remaining, show, staycount = 0, switchcount = 0
  LOGICAL :: door(3)
  REAL :: rnum

  CALL RANDOM_SEED
  DO i = 1, trials
     door = .FALSE.
     CALL RANDOM_NUMBER(rnum)
     prize = INT(3*rnum) + 1
     door(prize) = .TRUE.              ! place car behind random door

     CALL RANDOM_NUMBER(rnum)
     choice = INT(3*rnum) + 1          ! choose a door

     DO
        CALL RANDOM_NUMBER(rnum)
        show = INT(3*rnum) + 1
        IF (show /= choice .AND. show /= prize) EXIT       ! Reveal a goat
     END DO

     SELECT CASE(choice+show)          ! Calculate remaining door index
       CASE(3)
          remaining = 3
       CASE(4)
          remaining = 2
       CASE(5)
          remaining = 1
     END SELECT

     IF (door(choice)) THEN           ! You win by staying with your original choice
        staycount = staycount + 1
     ELSE IF (door(remaining)) THEN   ! You win by switching to other door
        switchcount = switchcount + 1
     END IF

  END DO

  WRITE(*, "(A,F6.2,A)") "Chance of winning by not switching is", real(staycount)/trials*100, "%"
  WRITE(*, "(A,F6.2,A)") "Chance of winning by switching is", real(switchcount)/trials*100, "%"

END PROGRAM MONTYHALL
```

Sample Output
 Chance of winning by not switching is 32.82%
 Chance of winning by switching is 67.18%


## FreeBASIC


```freebasic
' version 19-01-2019
' compile with: fbc -s console

Const As Integer max = 1000000
Randomize Timer

Dim As UInteger i, car_door, chosen_door, montys_door, stay, switch

For i = 1 To max
    car_door = Fix(Rnd * 3) + 1
    chosen_door = Fix(Rnd * 3) + 1
    If car_door <> chosen_door Then
        montys_door = 6 - car_door - chosen_door
    Else
        Do
          montys_door = Fix(Rnd * 3) + 1
        Loop Until montys_door <> car_door
    End If
    'Print car_door,chosen_door,montys_door
    ' stay
    If car_door = chosen_door Then stay += 1
    ' switch
    If car_door = 6 - montys_door - chosen_door Then switch +=1
Next

Print Using "If you stick to your choice, you have a ##.## percent" _
                                         + " chance to win"; stay / max * 100
Print Using "If you switched, you have a ##.## percent chance to win"; _
                                                           switch / max * 100

' empty keyboard buffer
While InKey <> "" : Wend
Print : Print "hit any key to end program"
Sleep
End
```

```txt
If you stick to your choice, you have a 33.32 percent chance to win
If you switched, you have a 66.68 percent chance to win
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
	games := 100000
	r := rand.New(rand.NewSource(time.Now().UnixNano()))

	var switcherWins, keeperWins, shown int
	for i := 0; i < games; i++ {
		doors := []int{0, 0, 0}
		doors[r.Intn(3)] = 1 // Set which one has the car
		choice := r.Intn(3) // Choose a door
		for shown = r.Intn(3); shown == choice || doors[shown] == 1; shown = r.Intn(3) {}
		switcherWins += doors[3 - choice - shown]
		keeperWins += doors[choice]
	}
	floatGames := float32(games)
	fmt.Printf("Switcher Wins: %d (%3.2f%%)\n",
		switcherWins, (float32(switcherWins) / floatGames * 100))
	fmt.Printf("Keeper Wins: %d (%3.2f%%)",
		keeperWins, (float32(keeperWins) / floatGames * 100))
}
```

Output:

```txt

Switcher Wins: 66542 (66.54%)
Keeper Wins: 33458 (33.46%)

```



## Haskell


```haskell
import System.Random (StdGen, getStdGen, randomR)

trials :: Int
trials = 10000

data Door = Car | Goat deriving Eq

play :: Bool -> StdGen -> (Door, StdGen)
play switch g = (prize, new_g)
  where (n, new_g) = randomR (0, 2) g
        d1 = [Car, Goat, Goat] !! n
        prize = case switch of
            False -> d1
            True  -> case d1 of
                Car  -> Goat
                Goat -> Car

cars :: Int -> Bool -> StdGen -> (Int, StdGen)
cars n switch g = f n (0, g)
  where f 0 (cs, g) = (cs, g)
        f n (cs, g) = f (n - 1) (cs + result, new_g)
          where result = case prize of Car -> 1; Goat -> 0
                (prize, new_g) = play switch g

main = do
    g <- getStdGen
    let (switch, g2) = cars trials True g
        (stay, _) = cars trials False g2
    putStrLn $ msg "switch" switch
    putStrLn $ msg "stay" stay
  where msg strat n = "The " ++ strat ++ " strategy succeeds " ++
            percent n ++ "% of the time."
        percent n = show $ round $
            100 * (fromIntegral n) / (fromIntegral trials)
```


With a <tt>State</tt> monad, we can avoid having to explicitly pass around the <tt>StdGen</tt> so often. <tt>play</tt> and <tt>cars</tt> can be rewritten as follows:


```haskell
import Control.Monad.State

play :: Bool -> State StdGen Door
play switch = do
    i <- rand
    let d1 = [Car, Goat, Goat] !! i
    return $ case switch of
        False -> d1
        True  -> case d1 of
            Car  -> Goat
            Goat -> Car
  where rand = do
            g <- get
            let (v, new_g) = randomR (0, 2) g
            put new_g
            return v

cars :: Int -> Bool -> StdGen -> (Int, StdGen)
cars n switch g = (numcars, new_g)
  where numcars = length $ filter (== Car) prize_list
        (prize_list, new_g) = runState (replicateM n (play switch)) g
```


Sample output (for either implementation):

```haskell
The switch strategy succeeds 67% of the time.
The stay strategy succeeds 34% of the time.
```



## HicEst


```hicest
REAL :: ndoors=3, doors(ndoors), plays=1E4

DLG(NameEdit = plays, DNum=1, Button='Go')

switchWins = 0
stayWins = 0

DO play = 1, plays
  doors = 0                      ! clear the doors
  winner = 1 + INT(RAN(ndoors))  ! door that has the prize
  doors(winner) = 1
  guess = 1 + INT(RAN(doors))    ! player chooses his door

  IF( guess == winner ) THEN     ! Monty decides which door to open:
      show = 1 + INT(RAN(2))     ! select 1st or 2nd goat-door
      checked = 0
      DO check = 1, ndoors
        checked = checked + (doors(check) == 0)
        IF(checked == show) open = check
      ENDDO
  ELSE
      open = (1+2+3) - winner - guess
  ENDIF
  new_guess_if_switch = (1+2+3) - guess - open

  stayWins = stayWins + doors(guess) ! count if guess was correct
  switchWins = switchWins + doors(new_guess_if_switch)
ENDDO

WRITE(ClipBoard, Name) plays, switchWins, stayWins

END
```


```hicest
! plays=1E3; switchWins=695; stayWins=305;
! plays=1E4; switchWins=6673; stayWins=3327;
! plays=1E5; switchWins=66811; stayWins=33189;
! plays=1E6; switchWins=667167; stayWins=332833;
```


=={{header|Icon}} and {{header|Unicon}}==

```Icon
procedure main(arglist)

rounds := integer(arglist[1]) | 10000
doors := '123'
strategy1 := strategy2 := 0

every 1 to rounds do {
   goats  := doors -- ( car := ?doors )
   guess1 := ?doors
   show   := goats -- guess1
   if guess1 == car then strategy1 +:= 1
   else strategy2 +:= 1
   }

write("Monty Hall simulation for ", rounds, " rounds.")
write("Strategy 1 'Staying' won ", real(strategy1) / rounds )
write("Strategy 2 'Switching' won ", real(strategy2) / rounds )

end
```


Sample Output:
```txt
Monty Hall simulation for 10000 rounds.
Strategy 1 'Staying' won 0.3266
Strategy 2 'Switching' won 0.6734
```



## Io


```Io
keepWins := 0
switchWins := 0
doors := 3
times := 100000
pickDoor := method(excludeA, excludeB,
  door := excludeA
  while(door == excludeA or door == excludeB,
    door = (Random value() * doors) floor
  )
  door
)
times repeat(
  playerChoice := pickDoor()
  carDoor := pickDoor()
  shownDoor := pickDoor(carDoor, playerChoice)
  switchDoor := pickDoor(playerChoice, shownDoor)
  (playerChoice == carDoor) ifTrue(keepWins = keepWins + 1)
  (switchDoor == carDoor) ifTrue(switchWins = switchWins + 1)
)
("Switching to the other door won #{switchWins} times.\n"\
    .. "Keeping the same door won #{keepWins} times.\n"\
    .. "Game played #{times} times with #{doors} doors.") interpolate println

```

Sample output:
```txt
Switching to the other door won 66935 times.
Keeping the same door won 33065 times.
Game played 100000 times with 3 doors.
```



## J


The core of this simulation is picking a random item from a set


```j
pick=: {~ ?@#
```


And, of course, we will be picking one door from three doors


```j>DOORS=:1 2 3</lang


But note that the simulation code should work just as well with more doors.

Anyways the scenario where the contestant's switch or stay strategy makes a difference is where Monty has picked from the doors which are neither the user's door nor the car's door.


```j
scenario=: ((pick@-.,])pick,pick) bind DOORS
```


(Here, I have decided that the result will be a list of three door numbers.  The first number in that list is the number Monty picks, the second number represents the door the user picked, and the third number represents the door where the car is hidden.)

Once we have our simulation test results for the scenario, we need to test if staying would win.  In other words we need to test if the user's first choice matches where the car was hidden:


```j
stayWin=: =/@}.
```


In other words: drop the first element from the list representing our test results -- this leaves us with the user's choice and the door where the car was hidden -- and then insert the verb <code>=</code> between those two values.

We also need to test if switching would win.  In other words, we need to test if the user would pick the car from the doors other than the one Monty picked and the one the user originally picked:


```j
switchWin=: pick@(DOORS -. }:) = {:
```


In other words, start with our list of all doors and then remove the door the monty picked and the door the user picked, and then pick one of the remaining doors at random (the pick at random part is only significant if there were originally more than 3 doors) and see if that matches the door where the car is.

Finally, we need to run the simulation a thousand times and count how many times each strategy wins:


```j
   +/ (stayWin,switchWin)@scenario"0 i.1000
320 680
```


Or, we could bundle this all up as a defined word.  Here, the (optional) left argument "names" the doors and the right argument says how many simulations to run:


```j
simulate=:3 :0
  1 2 3 simulate y
:
  pick=. {~ ?@#
  scenario=. ((pick@-.,])pick,pick) bind x
  stayWin=. =/@}.
  switchWin=. pick@(x -. }:) = {:
  r=.(stayWin,switchWin)@scenario"0 i.y
  labels=. ];.2 'limit stay switch '
  smoutput labels,.":"0 y,+/r
)
```


Example use:


```j
   simulate 1000
limit  1000
stay   304
switch 696
```


Or, with more doors (and assuming this does not require new rules about how Monty behaves or how the player behaves):


```j
   1 2 3 4 simulate 1000
limit  1000
stay   233
switch 388
```



## Java


```java
import java.util.Random;
public class Monty{
	public static void main(String[] args){
		int switchWins = 0;
		int stayWins = 0;
		Random gen = new Random();
		for(int plays = 0;plays < 32768;plays++ ){
			int[] doors = {0,0,0};//0 is a goat, 1 is a car
			doors[gen.nextInt(3)] = 1;//put a winner in a random door
			int choice = gen.nextInt(3); //pick a door, any door
			int shown; //the shown door
			do{
				shown = gen.nextInt(3);
			//don't show the winner or the choice
			}while(doors[shown] == 1 || shown == choice);

			stayWins += doors[choice];//if you won by staying, count it

			//the switched (last remaining) door is (3 - choice - shown), because 0+1+2=3
			switchWins += doors[3 - choice - shown];
		}
		System.out.println("Switching wins " + switchWins + " times.");
		System.out.println("Staying wins " + stayWins + " times.");
	}
}
```

Output:

```txt
Switching wins 21924 times.
Staying wins 10844 times.
```



## JavaScript



### Extensive Solution


This solution can test with n doors, the difference in probability for switching is shown to diminish as the number of doors increases.


```javascript

function montyhall(tests, doors) {
	'use strict';
	tests = tests ? tests : 1000;
	doors = doors ? doors : 3;
	var prizeDoor, chosenDoor, shownDoor, switchDoor, chosenWins = 0, switchWins = 0;

	// randomly pick a door excluding input doors
	function pick(excludeA, excludeB) {
		var door;
		do {
			door = Math.floor(Math.random() * doors);
		} while (door === excludeA || door === excludeB);
		return door;
	}

	// run tests
	for (var i = 0; i < tests; i ++) {

		// pick set of doors
		prizeDoor = pick();
		chosenDoor = pick();
		shownDoor = pick(prizeDoor, chosenDoor);
		switchDoor = pick(chosenDoor, shownDoor);

		// test set for both choices
		if (chosenDoor === prizeDoor) {
			chosenWins ++;
		} else if (switchDoor === prizeDoor) {
			switchWins ++;
		}
	}

	// results
	return {
		stayWins: chosenWins + ' ' + (100 * chosenWins / tests) + '%',
		switchWins: switchWins + ' ' + (100 * switchWins / tests) + '%'
	};
}

```


```javascript

montyhall(1000, 3)
Object {stayWins: "349 34.9%", switchWins: "651 65.1%"}
montyhall(1000, 4)
Object {stayWins: "253 25.3%", switchWins: "384 38.4%"}
montyhall(1000, 5)
Object {stayWins: "202 20.2%", switchWins: "265 26.5%"}

```



### Basic Solution


<!-- http://blog.dreasgrech.com/2011/09/simulating-monty-hall-problem.html -->

```javascript

var totalGames = 10000,
    selectDoor = function () {
	return Math.floor(Math.random() * 3); // Choose a number from 0, 1 and 2.
    },
    games = (function () {
	var i = 0, games = [];

	for (; i < totalGames; ++i) {
	    games.push(selectDoor()); // Pick a door which will hide the prize.
	}

	return games;
    }()),
    play = function (switchDoor) {
	var i = 0, j = games.length, winningDoor, randomGuess, totalTimesWon = 0;

	for (; i < j; ++i) {
	    winningDoor = games[i];
	    randomGuess = selectDoor();
	    if ((randomGuess === winningDoor && !switchDoor) ||
		(randomGuess !== winningDoor && switchDoor))
	    {
		/*
		 * If I initially guessed the winning door and didn't switch,
		 * or if I initially guessed a losing door but then switched,
		 * I've won.
		 *
		 * I lose when I initially guess the winning door and then switch,
                 * or initially guess a losing door and don't switch.
		 */

		totalTimesWon++;
	    }
	}
	return totalTimesWon;
    };

/*
 * Start the simulation
 */

console.log("Playing " + totalGames + " games");
console.log("Wins when not switching door", play(false));
console.log("Wins when switching door", play(true));

```


```javascript

Playing 10000 games
Wins when not switching door 3326
Wins when switching door 6630

```



## Julia

To make things interesting, I decided to generalize the problem to <tt>ncur</tt> doors and <tt>ncar</tt> cars.  To allow the MC to always show a goat behind a door after the contestant chooses, <math>ncur-ncar \ge 2</math>.

I was was of two minds on the type of simulation to provide, so I wrote two different simulators.  The literal simulator mimics the mechanics of the game as literally as possible, shuffling the arrangement of cars behind doors and randomizes all choices.  This avoids any feel of cheating but results in rather complex code.  The clean simulator implements the game more elegantly but it might look like cheating.

'''The Literal Simulation Function'''

```Julia

function play_mh_literal{T<:Integer}(ncur::T=3, ncar::T=1)
    ncar < ncur || throw(DomainError())
    curtains = shuffle(collect(1:ncur))
    cars = curtains[1:ncar]
    goats = curtains[(ncar+1):end]
    pick = rand(1:ncur)
    isstickwin = pick in cars
    deleteat!(curtains, findin(curtains, pick))
    if !isstickwin
        deleteat!(goats, findin(goats, pick))
    end
    if length(goats) > 0 # reveal goat
        deleteat!(curtains, findin(curtains, shuffle(goats)[1]))
    else # no goats, so reveal car
        deleteat!(curtains, rand(1:(ncur-1)))
    end
    pick = shuffle(curtains)[1]
    isswitchwin = pick in cars
    return (isstickwin, isswitchwin)
end

```


'''The Clean Simulation Function'''

```Julia

function play_mh_clean{T<:Integer}(ncur::T=3, ncar::T=1)
    ncar < ncur || throw(DomainError())
    pick = rand(1:ncur)
    isstickwin = pick <= ncar
    pick = rand(1:(ncur-2))
    if isstickwin # remove initially picked car from consideration
        pick += 1
    end
    isswitchwin = pick <= ncar
    return (isstickwin, isswitchwin)
end

```


'''Supporting Functions'''

```Julia

function mh_results{T<:Integer}(ncur::T, ncar::T,
                                nruns::T, play_mh::Function)
    stickwins = 0
    switchwins = 0
    for i in 1:nruns
        (isstickwin, isswitchwin) = play_mh(ncur, ncar)
        if isstickwin
            stickwins += 1
        end
        if isswitchwin
            switchwins += 1
        end
    end
    return (stickwins/nruns, switchwins/nruns)
end

function mh_analytic{T<:Integer}(ncur::T, ncar::T)
    stickodds = ncar/ncur
    switchodds = (ncar - stickodds)/(ncur-2)
    return (stickodds, switchodds)
end

function show_odds{T<:Real}(a::T, b::T)
    @sprintf "   %.1f   %.1f     %.2f" 100.0*a 100*b 1.0*b/a
end

function show_simulation{T<:Integer}(ncur::T, ncar::T, nruns::T)
    println()
    print("Simulating a ", ncur, " door, ", ncar, " car ")
    println("Monty Hall problem with ", nruns, " runs.\n")

    println("   Solution   Stick  Switch  Improvement")

    (a, b) = mh_results(ncur, ncar, nruns, play_mh_literal)
    println(@sprintf("%10s: ", "Literal"), show_odds(a, b))

    (a, b) = mh_results(ncur, ncar, nruns, play_mh_clean)
    println(@sprintf("%10s: ", "Clean"), show_odds(a, b))

    (a, b) = mh_analytic(ncur, ncar)
    println(@sprintf("%10s: ", "Analytic"), show_odds(a, b))
    println()
    return nothing
end

```


'''Main'''

```Julia

for i in 3:5, j in 1:(i-2)
    show_simulation(i, j, 10^5)
end

```


This code shows, for a variety of configurations, the results for 3 solutions:  literal simulation, clean simulation, analytic.  Stick is the percentage of times that the player wins a car by sticking to an initial choice.  Switch is the winning percentage the comes with switching one's selection following the goat reveal.  Improvement is the ratio of switch to stick.
```txt

Simulating a 3 door, 1 car Monty Hall problem with 100000 runs.

   Solution   Stick  Switch  Improvement
   Literal:    33.2   66.8     2.02
     Clean:    33.4   66.6     2.00
  Analytic:    33.3   66.7     2.00


Simulating a 4 door, 1 car Monty Hall problem with 100000 runs.

   Solution   Stick  Switch  Improvement
   Literal:    25.1   37.5     1.49
     Clean:    24.7   37.6     1.52
  Analytic:    25.0   37.5     1.50


Simulating a 4 door, 2 car Monty Hall problem with 100000 runs.

   Solution   Stick  Switch  Improvement
   Literal:    49.7   75.3     1.51
     Clean:    49.9   74.9     1.50
  Analytic:    50.0   75.0     1.50


Simulating a 5 door, 1 car Monty Hall problem with 100000 runs.

   Solution   Stick  Switch  Improvement
   Literal:    20.2   26.5     1.31
     Clean:    20.0   26.8     1.34
  Analytic:    20.0   26.7     1.33


Simulating a 5 door, 2 car Monty Hall problem with 100000 runs.

   Solution   Stick  Switch  Improvement
   Literal:    40.0   53.5     1.34
     Clean:    40.4   53.4     1.32
  Analytic:    40.0   53.3     1.33


Simulating a 5 door, 3 car Monty Hall problem with 100000 runs.

   Solution   Stick  Switch  Improvement
   Literal:    60.3   79.9     1.33
     Clean:    59.9   80.1     1.34
  Analytic:    60.0   80.0     1.33

```


'''Literal versus Clean'''

The clean simulation runs significantly faster and uses less memory.

```txt

julia> @time mh_results(3, 1, 10^5, play_mh_literal)
elapsed time: 0.346965522 seconds (183790752 bytes allocated, 27.56% gc time)
(0.33216,0.66784)

julia> @time mh_results(3, 1, 10^5, play_mh_clean)
elapsed time: 0.046481738 seconds (9600160 bytes allocated)
(0.33241,0.66759)

```



## Kotlin

```scala
// version 1.1.2

import java.util.Random

fun montyHall(games: Int) {
    var switchWins = 0
    var stayWins = 0
    val rnd = Random()
    (1..games).forEach {
        val doors = IntArray(3)        // all zero (goats) by default
        doors[rnd.nextInt(3)] = 1      // put car in a random door
        val choice = rnd.nextInt(3)    // choose a door at random
        var shown: Int
        do {
            shown = rnd.nextInt(3)     // the shown door
        }
        while (doors[shown] == 1 || shown == choice)
        stayWins += doors[choice]
        switchWins += doors[3 - choice - shown]
    }
    println("Simulating $games games:")
    println("Staying   wins $stayWins times")
    println("Switching wins $switchWins times")
}

fun main(args: Array<String>) {
    montyHall(1_000_000)
}
```

Sample output:
```txt

Simulating 1000000 games:
Staying   wins 333670 times
Switching wins 666330 times

```


=={{Header|Liberty BASIC}}==

```lb

'adapted from BASIC solution
DIM doors(3) '0 is a goat, 1 is a car

total = 10000   'set desired number of iterations
switchWins = 0
stayWins = 0

FOR plays = 1 TO total
    winner = INT(RND(1) * 3) + 1
    doors(winner) = 1'put a winner in a random door
    choice = INT(RND(1) * 3) + 1'pick a door, any door
    DO
    shown = INT(RND(1) * 3) + 1
    'don't show the winner or the choice
    LOOP WHILE doors(shown) = 1 OR shown = choice
    if doors(choice) = 1 then
        stayWins = stayWins + 1 'if you won by staying, count it
        else
        switchWins = switchWins + 1'could have switched to win
    end if
    doors(winner) = 0 'clear the doors for the next test
NEXT
PRINT "Result for ";total;" games."
PRINT "Switching wins "; switchWins; " times."
PRINT "Staying wins "; stayWins; " times."

```

Output:

```txt

Result for 10000 games.
Switching wins 6634 times.
Staying wins 3366 times.
```



## Lua


```lua
function playgame(player)
   local car = math.random(3)
   local pchoice = player.choice()
   local function neither(a, b) --slow, but it works
      local el = math.random(3)
      return (el ~= a and el ~= b) and el or neither(a, b)
   end
   local el = neither(car, pchoice)
   if(player.switch) then pchoice = neither(pchoice, el) end
   player.wins = player.wins + (pchoice == car and 1 or 0)
end
for _, v in ipairs{true, false} do
   player = {choice = function() return math.random(3) end,
      wins = 0, switch = v}
   for i = 1, 20000 do playgame(player) end
   print(player.wins)
end
```



## Lua/Torch


```lua
function montyHall(n)
    local car    = torch.LongTensor(n):random(3) -- door with car
    local choice = torch.LongTensor(n):random(3) -- player's choice
    local opens  = torch.LongTensor(n):random(2):csub(1):mul(2):csub(1) -- -1 or +1
    local iscarchoice = choice:eq(car)
    local nocarchoice = 1-iscarchoice
    opens[iscarchoice] = (((opens + choice - 1) % 3):abs() + 1)[iscarchoice]
    opens[nocarchoice] = (6 - car - choice)[nocarchoice]
    local change = torch.LongTensor(n):bernoulli() -- 0: stay, 1: change
    local win = iscarchoice:long():cmul(1-change) + nocarchoice:long():cmul(change)
    return car, choice, opens, change, win
end

function montyStats(n)
    local car, pchoice, opens, change, win = montyHall(n)

    local    change_and_win     =    change [  win:byte()]:sum()/   change :sum()*100
    local no_change_and_win     = (1-change)[  win:byte()]:sum()/(1-change):sum()*100
    local    change_and_win_not =    change [1-win:byte()]:sum()/   change :sum()*100
    local no_change_and_win_not = (1-change)[1-win:byte()]:sum()/(1-change):sum()*100

    print(string.format("         %9s    %9s"        , "no change",           "change"           ))
    print(string.format("win      %8.4f%%    %8.4f%%",  no_change_and_win    , change_and_win    ))
    print(string.format("win not  %8.4f%%    %8.4f%%",  no_change_and_win_not, change_and_win_not))
end

montyStats(1e7)
```


Output for 10 million samples:

```txt

         no change       change
win       33.3008%     66.6487%
win not   66.6992%     33.3513%
```


## M2000 Interpreter


```M2000 Interpreter

Module CheckIt {
      Enum Strat {Stay, Random, Switch}
      total=10000
      Print $("0.00")
      player_win_stay=0
      player_win_switch=0
      player_win_random=0
      For i=1 to total {
            Dim doors(1 to 3)=False
            doors(Random(1,3))=True
            guess=Random(1,3)
            Inventory other
            for k=1 to 3 {
                  If k <> guess Then Append other, k
            }
            If doors(guess) Then {
                  Mont_Hall_show=other(Random(0,1)!)
            } Else {
                If doors(other(0!)) Then {
                   Mont_Hall_show=other(1!)
                } Else Mont_Hall_show=other(0!)
                Delete Other, Mont_Hall_show
            }
            Strategy=Each(Strat)
            While Strategy {
                  Select Case Eval(strategy)
                  Case Random
                        {
                              If Random(1,2)=1 Then {
                                    If doors(guess) Then player_win_Random++
                              } else If doors(other(0!)) Then player_win_Random++
                        }
                  Case Switch
                        If doors(other(0!)) Then player_win_switch++
                  Else
                        If doors(guess) Then player_win_stay++
                  End Select
            }
      }
      Print "Stay: ";player_win_stay/total*100;"%"
      Print "Random: ";player_win_Random/total*100;"%"
      Print "Switch: ";player_win_switch/total*100;"%"
}
CheckIt

```

```txt

Stay: 33.39%
Random: 51.00%
Switch: 66.61%

```



## Mathematica



```Mathematica
 montyHall[nGames_] :=
    Module[{r, winningDoors, firstChoices, nStayWins, nSwitchWins, s},
         r := RandomInteger[{1, 3}, nGames];
         winningDoors = r;
         firstChoices = r;
         nStayWins =  Count[Transpose[{winningDoors, firstChoices}], {d_, d_}];
         nSwitchWins = nGames - nStayWins;

    Grid[{{"Strategy", "Wins", "Win %"}, {"Stay", Row[{nStayWins, "/", nGames}], s=N[100 nStayWins/nGames]},
          {"Switch", Row[{nSwitchWins, "/", nGames}], 100 - s}},  Frame -> All]]
```



;Usage:

```Mathematica
montyHall[100000]
```



[[File:MontyHall.jpg]]


## MATLAB


```MATLAB
function montyHall(numDoors,numSimulations)

    assert(numDoors > 2);

    function num = randInt(n)
        num = floor( n*rand()+1 );
    end

    %The first column will tallie wins, the second losses
    switchedDoors = [0 0];
    stayed = [0 0];


    for i = (1:numSimulations)

        availableDoors = (1:numDoors); %Preallocate the available doors
        winningDoor = randInt(numDoors); %Define the winning door
        playersOriginalChoice = randInt(numDoors); %The player picks his initial choice

        availableDoors(playersOriginalChoice) = []; %Remove the players choice from the available doors

        %Pick the door to open from the available doors
        openDoor = availableDoors(randperm(numel(availableDoors))); %Sort the available doors randomly
        openDoor(openDoor == winningDoor) = []; %Make sure Monty doesn't open the winning door
        openDoor = openDoor(randInt(numel(openDoor))); %Choose a random door to open

        availableDoors(availableDoors==openDoor) = []; %Remove the open door from the available doors
        availableDoors(end+1) = playersOriginalChoice; %Put the player's original choice back into the pool of available doors
        availableDoors = sort(availableDoors);

        playersNewChoice = availableDoors(randInt(numel(availableDoors))); %Pick one of the available doors

        if playersNewChoice == playersOriginalChoice
            switch playersNewChoice == winningDoor
                case true
                    stayed(1) = stayed(1) + 1;
                case false
                    stayed(2) = stayed(2) + 1;
                otherwise
                    error 'ERROR'
            end
        else
            switch playersNewChoice == winningDoor
                case true
                    switchedDoors(1) = switchedDoors(1) + 1;
                case false
                    switchedDoors(2) = switchedDoors(2) + 1;
                otherwise
                    error 'ERROR'
            end
        end
    end

    disp(sprintf('Switch win percentage: %f%%\nStay win percentage: %f%%\n', [switchedDoors(1)/sum(switchedDoors),stayed(1)/sum(stayed)] * 100));

end
```


Output:

```MATLAB>>
 montyHall(3,100000)
Switch win percentage: 66.705972%
Stay win percentage: 33.420062%
```




## MAXScript


```maxscript
fn montyHall choice switch =
(
    doors = #(false, false, false)
    doors[random 1 3] = true
    chosen = doors[choice]
    if switch then chosen = not chosen
    chosen
)

fn iterate iterations switched =
(
    wins = 0
    for i in 1 to iterations do
    (
        if (montyHall (random 1 3) switched) then
        (
            wins += 1
        )
    )
    wins * 100 / iterations as float
)

iterations = 10000
format ("Stay strategy:%\%\n") (iterate iterations false)
format ("Switch strategy:%\%\n") (iterate iterations true)
```

Output:

```maxscript
Stay strategy:33.77%
Switch strategy:66.84%
```



## NetRexx

```netrexx
/* NetRexx ************************************************************
* 30.08.2013 Walter Pachl  translated from Java/REXX/PL/I
**********************************************************************/
options replace format comments java crossref savelog symbols nobinary

doors = create_doors
switchWins = 0
stayWins = 0
shown=0
Loop plays=1 To 1000000
  doors=0
  r=r3()
  doors[r]=1
  choice = r3()
  loop Until shown<>choice & doors[shown]=0
    shown  = r3()
    End
  If doors[choice]=1 Then
    stayWins=stayWins+1
  Else
    switchWins=switchWins+1
  End
Say "Switching wins " switchWins " times."
Say "Staying wins   " stayWins   " times."

method create_doors static returns Rexx
  doors = ''
  doors[0] = 0
  doors[1] = 0
  doors[2] = 0
  return doors

method r3 static
  rand=random()
  return rand.nextInt(3) + 1
```

Output

```txt

Switching wins  667335  times.
Staying wins    332665  times.

```



## Nim

```nim
import math
randomize()

proc shuffle[T](x: var seq[T]) =
  for i in countdown(x.high, 0):
    let j = random(i + 1)
    swap(x[i], x[j])

# 1 represents a car
# 0 represent a goat

var
  stay = 0   # amount won if stay in the same position
  switch = 0 # amount won if you switch

for i in 1..1000:
  var lst = @[1,0,0]  # one car and two goats
  shuffle(lst)        # shuffles the list randomly
  let ran = random(3) # gets a random number for the random guess
  let user = lst[ran] # storing the random guess
  del lst, ran        # deleting the random guess

  var huh = 0
  for i in lst:       # getting a value 0 and deleting it
    if i == 0:
      del lst, huh    # deletes a goat when it finds it
      break
    inc huh

  if user == 1:       # if the original choice is 1 then stay adds 1
    inc stay

  if lst[0] == 1:     # if the switched value is 1 then switch adds 1
    inc switch

echo "Stay = ",stay
echo "Switch = ",switch
```

Output:

```txt
Stay = 337
Switch = 663
```



## OCaml


```ocaml
let trials = 10000

type door = Car | Goat

let play switch =
  let n = Random.int 3 in
  let d1 = [|Car; Goat; Goat|].(n) in
    if not switch then d1
    else match d1 with
       Car  -> Goat
     | Goat -> Car

let cars n switch =
  let total = ref 0 in
  for i = 1 to n do
    let prize = play switch in
    if prize = Car then
      incr total
  done;
  !total

let () =
  let switch = cars trials true
  and stay   = cars trials false in
  let msg strat n =
    Printf.printf "The %s strategy succeeds %f%% of the time.\n"
      strat (100. *. (float n /. float trials)) in
  msg "switch" switch;
  msg "stay" stay
```



## PARI/GP


```parigp
test(trials)={
  my(stay=0,change=0);
  for(i=1,trials,
    my(prize=random(3),initial=random(3),opened);
    while((opened=random(3))==prize | opened==initial,);
    if(prize == initial, stay++, change++)
  );
  print("Wins when staying:  "stay);
  print("Wins when changing: "change);
  [stay, change]
};

test(1e4)
```


Output:

```txt
Wins when staying:  3433
Wins when changing: 6567
%1 = [3433, 6567]
```



## Pascal


```pascal
program MontyHall;

uses
  sysutils;

const
  NumGames = 1000;


{Randomly pick a door(a number between 0 and 2}
function PickDoor(): Integer;
begin
  Exit(Trunc(Random * 3));
end;

var
  i: Integer;
  PrizeDoor: Integer;
  ChosenDoor: Integer;
  WinsChangingDoors: Integer = 0;
  WinsNotChangingDoors: Integer = 0;
begin
  Randomize;
  for i := 0 to NumGames - 1 do
  begin
    //randomly picks the prize door
    PrizeDoor := PickDoor;
    //randomly chooses a door
    ChosenDoor := PickDoor;

    //if the strategy is not changing doors the only way to win is if the chosen
    //door is the one with the prize
    if ChosenDoor = PrizeDoor then
      Inc(WinsNotChangingDoors);

    //if the strategy is changing doors the only way to win is if we choose one
    //of the two doors that hasn't the prize, because when we change we change to the prize door.
    //The opened door doesn't have a prize
    if ChosenDoor <> PrizeDoor then
      Inc(WinsChangingDoors);
  end;

  Writeln('Num of games:' + IntToStr(NumGames));
  Writeln('Wins not changing doors:' + IntToStr(WinsNotChangingDoors) + ', ' +
    FloatToStr((WinsNotChangingDoors / NumGames) * 100) + '% of total.');

  Writeln('Wins changing doors:' + IntToStr(WinsChangingDoors) + ', ' +
    FloatToStr((WinsChangingDoors / NumGames) * 100) + '% of total.');

end.

```


Output:

```txt
Num of games:1000
Wins not changing doors:359, 35,9% of total.
Wins changing doors:641, 64,1% of total.
```



## Perl



```perl
#! /usr/bin/perl
use strict;
my $trials = 10000;

my $stay = 0;
my $switch = 0;

foreach (1 .. $trials)
{
   my $prize = int(rand 3);
    # let monty randomly choose a door where he puts the prize
   my $chosen = int(rand 3);
    # let us randomly choose a door...
   my $show;
   do { $show = int(rand 3) } while $show == $chosen || $show == $prize;
    # ^ monty opens a door which is not the one with the
    # prize, that he knows it is the one the player chosen
   $stay++ if $prize == $chosen;
    # ^ if player chose the correct door, player wins only if he stays
   $switch++ if $prize == 3 - $chosen - $show;
    # ^ if player switches, the door he picks is (3 - $chosen - $show),
    # because 0+1+2=3, and he picks the only remaining door that is
    # neither $chosen nor $show
}

print "Stay win ratio " .  (100.0 * $stay/$trials) . "\n";
print "Switch win ratio " . (100.0 * $switch/$trials) . "\n";
```



## Perl 6

This implementation is parametric over the number of doors. [[wp:Monty_Hall_problem#Increasing_the_number_of_doors|Increasing the number of doors in play makes the superiority of the switch strategy even more obvious]].


```perl6>enum Prize <Car Goat
;
enum Strategy <Stay Switch>;

sub play (Strategy $strategy, Int :$doors = 3) returns Prize {

    # Call the door with a car behind it door 0. Number the
    # remaining doors starting from 1.
    my Prize @doors = flat Car, Goat xx $doors - 1;

    # The player chooses a door.
    my Prize $initial_pick = @doors.splice(@doors.keys.pick,1)[0];

    # Of the n doors remaining, the host chooses n - 1 that have
    # goats behind them and opens them, removing them from play.
    while @doors > 1 {
	@doors.splice($_,1)
	    when Goat
		given @doors.keys.pick;
    }

    # If the player stays, they get their initial pick. Otherwise,
    # they get whatever's behind the remaining door.
    return $strategy === Stay ?? $initial_pick !! @doors[0];
}

constant TRIALS = 10_000;

for 3, 10 -> $doors {
    my atomicint @wins[2];
    say "With $doors doors: ";
    for Stay, 'Staying', Switch, 'Switching' -> $s, $name {
        (^TRIALS).race.map: {
            @wins[$s]⚛++ if play($s, doors => $doors) == Car;
        }
        say "  $name wins ",
            round(100*@wins[$s] / TRIALS),
            '% of the time.'
    }
}
```

```txt
With 3 doors:
  Staying wins 34% of the time.
  Switching wins 66% of the time.
With 10 doors:
  Staying wins 10% of the time.
  Switching wins 90% of the time.
```



## Phix

Modified copy of [[Monty_Hall_problem#Euphoria|Euphoria]]

```Phix
integer swapWins = 0, stayWins = 0, winner, choice, reveal, other
atom t0 = time()

for game=1 to 1_000_000 do
    winner = rand(3)
    choice = rand(3)
    while 1 do
        reveal = rand(3)
        if reveal!=winner and reveal!=choice then exit end if
    end while
    stayWins += (choice==winner)
    other = 6-choice-reveal     -- (as 1+2+3=6, and reveal!=choice)
    swapWins += (other==winner)
end for
printf(1, "Stay: %,d\nSwap: %,d\nTime: %3.2fs\n",{stayWins,swapWins,time()-t0})
```

```txt

Stay: 333,292
Swap: 666,708
Time: 0.16s

```



## PHP


```php
<?php
function montyhall($iterations){
	$switch_win = 0;
	$stay_win = 0;

	foreach (range(1, $iterations) as $i){
		$doors = array(0, 0, 0);
		$doors[array_rand($doors)] = 1;
		$choice = array_rand($doors);
		do {
			$shown = array_rand($doors);
		} while($shown == $choice || $doors[$shown] == 1);

		$stay_win += $doors[$choice];
		$switch_win += $doors[3 - $choice - $shown];
	}

	$stay_percentages = ($stay_win/$iterations)*100;
	$switch_percentages = ($switch_win/$iterations)*100;

	echo "Iterations: {$iterations} - ";
	echo "Stayed wins: {$stay_win} ({$stay_percentages}%) - ";
	echo "Switched wins: {$switch_win} ({$switch_percentages}%)";
}

        montyhall(10000);
?>
```

Output:

```txt
Iterations: 10000 - Stayed wins: 3331 (33.31%) - Switched wins: 6669 (66.69%)
```



## PicoLisp


```PicoLisp
(de montyHall (Keep)
   (let (Prize (rand 1 3)  Choice (rand 1 3))
      (if Keep                    # Keeping the first choice?
         (= Prize Choice)         # Yes: Monty's choice doesn't matter
         (<> Prize Choice) ) ) )  # Else: Win if your first choice was wrong

(prinl
   "Strategy KEEP    -> "
   (let Cnt 0
      (do 10000 (and (montyHall T) (inc 'Cnt)))
      (format Cnt 2) )
   " %" )

(prinl
   "Strategy SWITCH  -> "
   (let Cnt 0
      (do 10000 (and (montyHall NIL) (inc 'Cnt)))
      (format Cnt 2) )
   " %" )
```

Output:

```txt
Strategy KEEP    -> 33.01 %
Strategy SWITCH  -> 67.73 %
```



## PL/I

```pli
*process source attributes xref;
 ziegen: Proc Options(main);
 /* REXX ***************************************************************
 * 30.08.2013 Walter Pachl derived from Java
 **********************************************************************/
 Dcl (switchWins,stayWins) Bin Fixed(31) Init(0);
 Dcl doors(3) Bin Fixed(31);
 Dcl (plays,r,choice) Bin Fixed(31) Init(0);
 Dcl c17 Char(17) Init((datetime()));
 Dcl p9  Pic'(9)9' def(c17) pos(5);
 i=random(p9);
 Do plays=1 To 1000000;
   doors=0;
   r=r3();
   doors(r)=1;
   choice=r3();
   Do Until(shown^=choice & doors(shown)=0);
     shown=r3();
     End;
   If doors(choice)=1 Then
     stayWins+=1;
   Else
     switchWins+=1;
   End;
 Put Edit("Switching wins ",switchWins," times.")(Skip,a,f(6),a);
 Put Edit("Staying wins   ",stayWins  ," times.")(Skip,a,f(6),a);

 r3: Procedure Returns(Bin Fixed(31));
 /*********************************************************************
 * Return a random integer: 1, 2, or 3
 *********************************************************************/
   Dcl r Bin Float(53);
   Dcl res Bin Fixed(31);
   r=random();
   res=(r*3)+1;
   Return(res);
 End;
 End;
```

Output:

```txt

Switching wins 665908 times.
Staying wins   334092 times.

```


=={{header|PostScript|Post Script}}==
Use ghostscript or print this to a postscript printer


```PostScript
%!PS
/Courier             % name the desired font
20 selectfont        % choose the size in points and establish
                     % the font as the current one

% init random number generator
(%Calendar%) currentdevparams /Second get srand

1000000				% iteration count
0 0				% 0 wins on first selection 0 wins on switch
2 index				% get iteration count
{
rand 3 mod			% winning door
rand 3 mod			% first choice
eq {
	1 add
}
{
	exch 1 add exch
} ifelse
} repeat

% compute percentages
2 index div 100 mul exch 2 index div 100 mul


% display result
70 600 moveto
(Switching the door: ) show
80 string cvs show (%) show
70 700 moveto
(Keeping the same: ) show
80 string cvs show (%) show


showpage             % print all on the page
```


Sample output:

```txt

Keeping the same: 33.4163%
Switching the door:  66.5837%

```



## PowerShell


```Powershell
#Declaring variables
$intIterations = 10000
$intKept = 0
$intSwitched = 0

#Creating a function
Function Play-MontyHall()
    {
    #Using a .NET object for randomization
    $objRandom = New-Object -TypeName System.Random

    #Generating the winning door number
    $intWin = $objRandom.Next(1,4)

    #Generating the chosen door
    $intChoice = $objRandom.Next(1,4)

    #Generating the excluded number
    #Because there is no method to exclude a number from a range,
    #I let it re-generate in case it equals the winning number or
    #in case it equals the chosen door.
    $intLose = $objRandom.Next(1,4)
    While (($intLose -EQ $intWin) -OR ($intLose -EQ $intChoice))
        {$intLose = $objRandom.Next(1,4)}

    #Generating the 'other' door
    #Same logic applies as for the chosen door: it cannot be equal
    #to the winning door nor to the chosen door.
    $intSwitch = $objRandom.Next(1,4)
    While (($intSwitch -EQ $intLose) -OR ($intSwitch -EQ $intChoice))
        {$intSwitch = $objRandom.Next(1,4)}

    #Simple counters per win for both categories
    #Because a child scope cannot change variables in the parent
    #scope, the scope of the counters is expanded script-wide.
    If ($intChoice -EQ $intWin)
        {$script:intKept++}
    If ($intSwitch -EQ $intWin)
        {$script:intSwitched++}

    }

#Looping the Monty Hall function for $intIterations times
While ($intIterationCount -LT $intIterations)
    {
    Play-MontyHall
    $intIterationCount++
    }

#Output
Write-Host "Results through $intIterations iterations:"
Write-Host "Keep  : $intKept ($($intKept/$intIterations*100)%)"
Write-Host "Switch: $intSwitched ($($intSwitched/$intIterations*100)%)"
Write-Host ""
```

Output:

```txt
Results through 10000 iterations:
Keep  : 3336 (33.36%)
Switch: 6664 (66.64%)
```



## PureBasic


```PureBasic
Structure wins
  stay.i
  redecide.i
EndStructure

#goat = 0
#car = 1
Procedure MontyHall(*results.wins)
  Dim Doors(2)
  Doors(Random(2)) = #car

  player = Random(2)
  Select Doors(player)
    Case #car
      *results\redecide + #goat
      *results\stay + #car
    Case #goat
      *results\redecide + #car
      *results\stay + #goat
  EndSelect
EndProcedure

OpenConsole()
#Tries = 1000000

Define results.wins

For i = 1 To #Tries
  MontyHall(@results)
Next

PrintN("Trial runs for each option: " + Str(#Tries))
PrintN("Wins when redeciding: " + Str(results\redecide) + " (" + StrD(results\redecide / #Tries * 100, 2) + "% chance)")
PrintN("Wins when sticking:   " + Str(results\stay) + " (" + StrD(results\stay / #Tries * 100, 2) + "% chance)")
Input()
```


Output:
```txt
Trial runs for each option: 1000000
Wins when redeciding: 666459 (66.65% chance)
Wins when sticking:   333541 (33.35% chance)
```



## Python


```python
'''
I could understand the explanation of the Monty Hall problem
but needed some more evidence

References:
  http://www.bbc.co.uk/dna/h2g2/A1054306
  http://en.wikipedia.org/wiki/Monty_Hall_problem especially:
  http://en.wikipedia.org/wiki/Monty_Hall_problem#Increasing_the_number_of_doors
'''
from random import randrange

doors, iterations = 3,100000  # could try 100,1000

def monty_hall(choice, switch=False, doorCount=doors):
  # Set up doors
  door = [False]*doorCount
  # One door with prize
  door[randrange(doorCount)] = True

  chosen = door[choice]

  unpicked = door
  del unpicked[choice]

  # Out of those unpicked, the alternative is either:
  #   the prize door, or
  #   an empty door if the initial choice is actually the prize.
  alternative = True in unpicked

  if switch:
    return alternative
  else:
    return chosen

print "\nMonty Hall problem simulation:"
print doors, "doors,", iterations, "iterations.\n"

print "Not switching allows you to win",
print sum(monty_hall(randrange(3), switch=False)
          for x in range(iterations)),
print "out of", iterations, "times."
print "Switching allows you to win",
print sum(monty_hall(randrange(3), switch=True)
          for x in range(iterations)),
print "out of", iterations, "times.\n"
```

Sample output:

```txt
Monty Hall problem simulation:
3 doors, 100000 iterations.

Not switching allows you to win 33337 out of 100000 times.
Switching allows you to win 66529 out of 100000 times.
```





### Python 3 version:

Another (simpler in my opinion), way to do this is below, also in python 3:

```python
import random
 #1 represents a car
 #0 represent a goat

stay = 0  #amount won if stay in the same position
switch = 0 # amount won if you switch

for i in range(1000):
    lst = [1,0,0]           # one car and two goats
    random.shuffle(lst)     # shuffles the list randomly

    ran = random.randrange(3) # gets a random number for the random guess

    user = lst[ran] #storing the random guess

    del(lst[ran]) # deleting the random guess

    huh = 0
    for i in lst: # getting a value 0 and deleting it
        if i ==0:
            del(lst[huh]) # deletes a goat when it finds it
            break
        huh+=1

    if user ==1: # if the original choice is 1 then stay adds 1
        stay+=1

    if lst[0] == 1: # if the switched value is 1 then switch adds 1
        switch+=1

print("Stay =",stay)
print("Switch = ",switch)
#Done by Sam Witton 09/04/2014
```



## R



```rsplus
# Since R is a vector based language that penalizes for loops, we will avoid
#     for-loops, instead using "apply" statement variants (like "map" in other
#     functional languages).

set.seed(19771025)   # set the seed to set the same results as this code
N <- 10000  # trials
true_answers <- sample(1:3, N, replace=TRUE)

# We can assme that the contestant always choose door 1 without any loss of
#    generality, by equivalence.  That is, we can always relabel the doors
#    to make the user-chosen door into door 1.
# Thus, the host opens door '2' unless door 2 has the prize, in which case
#    the host opens door 3.

host_opens <- 2 + (true_answers == 2)
other_door <- 2 + (true_answers != 2)

## if always switch
summary( other_door == true_answers )
## if we never switch
summary( true_answers == 1)
## if we randomly switch
random_switch <- other_door
random_switch[runif(N) >= .5] <- 1
summary(random_switch == true_answers)



## To go with the exact parameters of the Rosetta challenge, complicating matters....
##  Note that the player may initially choose any of the three doors (not just Door 1),
##     that the host opens a different door revealing a goat (not necessarily Door 3), and
##     that he gives the player a second choice between the two remaining unopened doors.

N <- 10000  #trials
true_answers <- sample(1:3, N, replace=TRUE)
user_choice <- sample(1:3, N, replace=TRUE)
## the host_choice is more complicated
host_chooser <- function(user_prize) {
    # this could be cleaner
    bad_choices <- unique(user_prize)
    # in R, the x[-vector] form implies, choose the indices in x not in vector
    choices <- c(1:3)[-bad_choices]
    # if the first arg to sample is an int, it treats it as the number of choices
    if (length(choices) == 1) {  return(choices)}
    else { return(sample(choices,1))}
}

host_choice <- apply( X=cbind(true_answers,user_choice), FUN=host_chooser,MARGIN=1)
not_door <- function(x){ return( (1:3)[-x]) }  # we could also define this
                                                # directly at the FUN argument following
other_door  <- apply( X = cbind(user_choice,host_choice), FUN=not_door, MARGIN=1)


## if always switch
summary( other_door == true_answers )
## if we never switch
summary( true_answers == user_choice)
## if we randomly switch
random_switch <- user_choice
change <- runif(N) >= .5
random_switch[change] <- other_door[change]
summary(random_switch == true_answers)
```




```txt
Results:

> ## if always switch
> summary( other_door == true_answers )
   Mode   FALSE    TRUE
logical    3298    6702
> ## if we never switch
> summary( true_answers == 1)
   Mode   FALSE    TRUE
logical    6702    3298
> ## if we randomly switch
> summary(random_switch == true_answers)
   Mode   FALSE    TRUE
logical    5028    4972


> ## if always switch
> summary( other_door == true_answers )
   Mode   FALSE    TRUE
logical    3295    6705
> ## if we never switch
> summary( true_answers == user_choice)
   Mode   FALSE    TRUE
logical    6705    3295
> ## if we randomly switch
> summary(random_switch == true_answers)
   Mode   FALSE    TRUE
logical    4986    5014
```



```txt
# As above, but generalized to K number of doors

K = 4     # number of doors
N = 1e4   # number of simulation trials

chooser <- function(x) { i <- (1:K)[-x]; if (length(i)>1) sample(i,1) else i }

p100 <- function(...) { cat("\nNumber of doors:", K,
       "\nSimulation yields % winning probability:",
       " (2nd choice after host reveal)\n");
        print(c(...) * 100, digits=3) }

prize_door <- sample(1:K, N, replace=TRUE)
first_choice <- sample(1:K, N, replace=TRUE)

host_opens <- apply(cbind(prize_door, first_choice), 1, chooser)
second_choice <- apply(cbind(host_opens, first_choice), 1, chooser)

p100("By first choice" = (Pr.first_win <- mean(first_choice == prize_door)),
     "By second choice" = (Pr.second_win <- mean(second_choice == prize_door)),
     "  Change gain" = Pr.second_win / Pr.first_win - 1)

#-------
#
# Sample output:

Number of doors: 4
Simulation yields % winning probability:  (2nd choice after host reveal)
 By first choice By second choice      Change gain
            24.7             36.5             48.0

```



## Racket



```Racket

#lang racket

(define (get-last-door a b) ; assumes a != b
  (vector-ref '#(- 2 1
                 2 - 0
                 1 0 -)
              (+ a (* 3 b))))

(define (run-game strategy)
  (define car-door (random 3))
  (define first-choice (random 3))
  (define revealed-goat
    (if (= car-door first-choice)
      (let ([r (random 2)]) (if (<= car-door r) (add1 r) r)) ; random
      (get-last-door car-door first-choice))) ; reveal goat
  (define final-choice (strategy first-choice revealed-goat))
  (define win? (eq? final-choice car-door))
  ;; (printf "car: ~s\nfirst: ~s\nreveal: ~s\nfinal: ~s\n  => ~s\n\n"
  ;;         car-door first-choice revealed-goat final-choice
  ;;         (if win? 'win 'lose))
  win?)

(define (keep-choice first-choice revealed-goat)
  first-choice)

(define (change-choice first-choice revealed-goat)
  (get-last-door first-choice revealed-goat))

(define (test-strategy strategy)
  (define N 10000000)
  (define wins (for/sum ([i (in-range N)]) (if (run-game strategy) 1 0)))
  (printf "~a: ~a%\n"
          (object-name strategy)
          (exact->inexact (/ wins N 1/100))))

(for-each test-strategy (list keep-choice change-choice))

```


Sample Output:

```txt

keep-choice: 33.33054%
change-choice: 66.67613%

```



## REXX


### version 1

```rexx
/* REXX ***************************************************************
* 30.08.2013 Walter Pachl derived from Java
**********************************************************************/
Call time 'R'
switchWins = 0;
stayWins = 0
Do plays = 1 To 1000000
  doors.=0
  r=r3()
  doors.r=1
  choice = r3()
  Do Until shown<>choice  & doors.shown=0
    shown  = r3()
    End
  If doors.choice=1 Then
    stayWins=stayWins+1
  Else
    switchWins=switchWins+1
  End
Say "Switching wins " switchWins " times."
Say "Staying wins   " stayWins   " times."
Say 'REXX:' time('E') 'seconds'
Call time 'R'
'ziegen'
Say 'PL/I:' time('E') 'seconds'
Say ' '
Call time 'R'
'java ziegen'
Say 'NetRexx:' time('E') 'seconds'
Exit
r3: Return random(2)+1
```

Output for 1000000 samples:

```txt

Switching wins  666442  times.
Staying wins    333558  times.
REXX:   4.321000 seconds

Switching wins 665908 times.
Staying wins   334092 times.
PL/I:   0.328000 seconds

Switching wins  667335  times.
Staying wins    332665  times.
NetRexx: 2.042000 seconds

```


From the Rosetta Code:Village Pump/Run times on examples?

As per Michael Mol about showing timings for program execution times:
::* Discouraging timing comparisons between different languages.
::* Allowing detailed timings, if someone wants to, in the talk pages.
::* But generally - like now, leaving them out.


### version 2

This REXX version allows the number of doors to be specified   (as well as the number of trials).

```rexx
/*REXX program  simulates  a number of trials of the classic  Monty Hall  problem.      */
parse arg # d seed .                             /*obtain the optional args from the CL.*/
if #==''  |  #==","     then #=1000000           /*Not specified?  Then use 1 million.  */
if d==''  |  d==","     then d=      3           /* "      "         "   "  three doors.*/
if datatype(seed, 'W')  then call random ,, seed /*Specified?  Use as a seed for RANDOM.*/
wins.= 0                                         /*wins.0 ≡ stay,    wins.1 ≡ switching.*/
           do  #;                   door.   = 0  /*initialize all doors to a value of 0.*/
           car= random(1, d);       door.car= 1  /*the TV show hides a car randomly.    */
             ?= random(1, d);       _= door.?    /*the contestant picks a random door.  */
           wins._ =  wins._  +  1                /*bump the type of win strategy.       */
           end   /*#*/                           /* [↑]  perform the loop   #   times.  */
                                                 /* [↑]  door values:   0≡goat    1≡car */
say 'switching wins '    format(wins.0 / # * 100, , 1)"%  of the time."
say '  staying wins '    format(wins.1 / # * 100, , 1)"%  of the time." ;    say
say 'performed '  #  " times with "  d ' doors.' /*stick a fork in it,  we're all done. */
```

```txt

switching wins  66.8%  of the time.
  staying wins  33.2%  of the time.

performed  1000000  times with  3  doors.

```

```txt

switching wins  75.0%  of the time.
  staying wins  25.0%  of the time.

performed  1000000  times with  4  doors.

```

```txt

switching wins  80.0%  of the time.
  staying wins  20.0%  of the time.

performed  1000000  times with  5  doors.

```

```txt

switching wins  83.6%  of the time.
  staying wins  16.4%  of the time.

performed  1000000  times with  6  doors.

```



## Ring


```ring

total = 10000
swapper = 0
sticker = 0
revealdoor = 0
for trial = 1 to total
    prizedoor = random(3) + 1
    guessdoor = random(3) + 1
    if prizedoor = guessdoor
       revealdoor = random(2) + 1
       if prizedoor = 1 revealdoor += 1 ok
       if (prizedoor = 2 and revealdoor = 2) revealdoor = 3 ok
    else
       revealdoor = (prizedoor ^ guessdoor)
    ok
    stickdoor = guessdoor
    swapdoor = (guessdoor ^ revealdoor)
    if stickdoor = prizedoor sticker += 1 ok
    if swapdoor = prizedoor swapper += 1 ok
next
see "after a total of " + total + " trials," + nl
see "the 'sticker' won " + sticker + " times (" + floor(sticker/total*100) + "%)" + nl
see "the 'swapper' won " + swapper + " times (" + floor(swapper/total*100) + "%)" + nl

```

Output:

```txt

after a total of 10000 trials,
the 'sticker' won 2461 times (24%)
the 'swapper' won 7539 times (75%)

```



## Ruby



```ruby
n = 10_000                  #number of times to play

stay = switch = 0           #sum of each strategy's wins

n.times do                  #play the game n times

  #the doors reveal 2 goats and a car
  doors = [ :goat, :goat, :car ].shuffle

  #random guess
  guess = rand(3)

  #random door shown, but it is neither the guess nor the car
  begin shown = rand(3) end while shown == guess || doors[shown] == :car

  if doors[guess] == :car
    #staying with the initial guess wins if the initial guess is the car
    stay += 1
  else
    #switching guesses wins if the unshown door is the car
    switch += 1
  end

end

puts "Staying wins %.2f%% of the time."   % (100.0 * stay   / n)
puts "Switching wins %.2f%% of the time." % (100.0 * switch / n)
```

Sample Output:

```txt
Staying wins 33.84% of the time.
Switching wins 66.16% of the time.
```



## Run BASIC


```runbasic
' adapted from BASIC solution

input "Number of tries;";tries  	' gimme the number of iterations
FOR plays	= 1 TO tries
     winner	= INT(RND(1) * 3) + 1
     doors(winner) = 1			'put a winner in a random door
     choice	= INT(RND(1) * 3) + 1	'pick a door please
[DO] shown	= INT(RND(1) * 3) + 1
' ------------------------------------------
'  don't show the winner or the choice
     if doors(shown) = 1 then goto [DO]
     if shown = choice   then goto [DO]
     if doors(choice) 	= 1 then
        stayWins	= stayWins + 1 		' if you won by staying, count it
        else
        switchWins	= switchWins + 1	' could have switched to win
     end if
     doors(winner)	= 0 			'clear the doors for the next test
NEXT
PRINT "    Result for ";tries;" games."
PRINT "Switching wins ";switchWins; " times."
PRINT "  Staying wins ";stayWins; " times."
```



## Rust

```rust
extern crate rand;
use rand::Rng;

#[derive(Clone, Copy, PartialEq)]
enum Prize {Goat , Car}

const GAMES: usize = 3_000_000;
fn main() {
    let mut switch_wins = 0;
    let mut rng = rand::thread_rng();

    for _ in 0..GAMES {
        let mut doors = [Prize::Goat; 3];
        *rng.choose_mut(&mut doors).unwrap() = Prize::Car;

        // You only lose by switching if you pick the car the first time
        if rng.choose(&doors).unwrap() != &Prize::Car {
            switch_wins += 1;
        }
    }
    println!("I played the game {total} times and won {wins} times ({percent}%).",
             total   = GAMES,
             wins    = switch_wins,
             percent = switch_wins as f64 / GAMES as f64 * 100.0
    );
}
```


## Scala


```scala
import scala.util.Random

object MontyHallSimulation {
  def main(args: Array[String]) {
    val samples = if (args.size == 1 && (args(0) matches "\\d+")) args(0).toInt else 1000
    val doors = Set(0, 1, 2)
    var stayStrategyWins = 0
    var switchStrategyWins = 0

    1 to samples foreach { _ =>
      val prizeDoor = Random shuffle doors head;
      val choosenDoor = Random shuffle doors head;
      val hostDoor = Random shuffle (doors - choosenDoor - prizeDoor) head;
      val switchDoor = doors - choosenDoor - hostDoor head;

      (choosenDoor, switchDoor) match {
        case (`prizeDoor`, _) => stayStrategyWins += 1
        case (_, `prizeDoor`) => switchStrategyWins += 1
      }
    }

    def percent(n: Int) = n * 100 / samples

    val report = """|%d simulations were ran.
                    |Staying won %d times (%d %%)
                    |Switching won %d times (%d %%)""".stripMargin

    println(report
            format (samples,
                    stayStrategyWins, percent(stayStrategyWins),
                    switchStrategyWins, percent(switchStrategyWins)))
  }
}
```


Sample:


```txt

1000 simulations were ran.
Staying won 333 times (33 %)
Switching won 667 times (66 %)

```



## Scheme


```scheme
(define (random-from-list list) (list-ref list (random (length list))))
(define (random-permutation list)
  (if (null? list)
      '()
      (let* ((car (random-from-list list))
             (cdr (random-permutation (remove car list))))
        (cons car cdr))))
(define (random-configuration) (random-permutation '(goat goat car)))
(define (random-door) (random-from-list '(0 1 2)))

(define (trial strategy)
  (define (door-with-goat-other-than door strategy)
    (cond ((and (not (= 0 door)) (equal? (list-ref strategy 0) 'goat)) 0)
          ((and (not (= 1 door)) (equal? (list-ref strategy 1) 'goat)) 1)
          ((and (not (= 2 door)) (equal? (list-ref strategy 2) 'goat)) 2)))
  (let* ((configuration (random-configuration))
         (players-first-guess (strategy `(would-you-please-pick-a-door?)))
         (door-to-show-player (door-with-goat-other-than players-first-guess
                                                         configuration))
         (players-final-guess (strategy `(there-is-a-goat-at/would-you-like-to-move?
                                          ,players-first-guess
                                          ,door-to-show-player))))
    (if (equal? (list-ref configuration players-final-guess) 'car)
        'you-win!
        'you-lost)))

(define (stay-strategy message)
  (case (car message)
    ((would-you-please-pick-a-door?) (random-door))
    ((there-is-a-goat-at/would-you-like-to-move?)
     (let ((first-choice (cadr message)))
        first-choice))))

(define (switch-strategy message)
  (case (car message)
    ((would-you-please-pick-a-door?) (random-door))
    ((there-is-a-goat-at/would-you-like-to-move?)
     (let ((first-choice (cadr message))
           (shown-goat (caddr message)))
       (car (remove first-choice (remove shown-goat '(0 1 2))))))))

(define-syntax repeat
  (syntax-rules ()
    ((repeat <n> <body> ...)
     (let loop ((i <n>))
       (if (zero? i)
           '()
           (cons ((lambda () <body> ...))
                 (loop (- i 1))))))))

(define (count element list)
  (if (null? list)
      0
      (if (equal? element (car list))
          (+ 1 (count element (cdr list)))
          (count element (cdr list)))))

(define (prepare-result strategy results)
  `(,strategy won with probability
              ,(exact->inexact (* 100 (/ (count 'you-win! results) (length results)))) %))

(define (compare-strategies times)
  (append
   (prepare-result 'stay-strategy (repeat times (trial stay-strategy)))
   '(and)
   (prepare-result 'switch-strategy (repeat times (trial switch-strategy)))))

;; > (compare-strategies 1000000)
;; (stay-strategy won with probability 33.3638 %
;;  and switch-strategy won with probability 66.716 %)
```



## Scilab

<lang>//  How it works:
//  MontyHall() is a function with argument switch:
//  it will be called 100000 times with switch=%T,
//  and another 100000 times with switch=%F

function win=MontyHall(switch)      //If switch==%T the player will switch
    doors=zeros(1,3)                //All goats
    car=grand(1,1,'uin',1,3)
    a(car)=1                        //Place a car somewher
    pick=grand(1,1,'uin',1,3)       //The player picks...
    if pick==car then               //If the player picks right...
        if switch==%T then          //...and switches he will be wrong
            win=%F
        else                        //...but if he doesn't, he will be right
            win=%T
        end
    else                            //If the player picks a goat...
        if switch==%T then          //...and switches: the other door with the goat shall be
            win=%T                  //   opened: the player will switch to the car and win
        else                        //...but if he doesn't, he will remain by his goat
            win=%F
        end
    end
endfunction

wins_switch=0
wins_stay=0
games=100000
for i=1:games
    if MontyHall(%T)==%T then
        wins_switch=wins_switch+1
    end
    if MontyHall(%F)==%T then
        wins_stay=wins_stay+1
    end
end
disp("Switching, one wins"+ascii(10)+string(wins_switch)+" games out of "+string(games))
disp("Staying, one wins"+ascii(10)+string(wins_stay)+" games out of "+string(games))
```


Output:


```txt

 Switching, one wins
 66649 games out of 100000

 Staying, one wins
 33403 games out of 100000

```



## Seed7


```seed7
$ include "seed7_05.s7i";

const proc: main is func
  local
    var integer: switchWins is 0;
    var integer: stayWins is 0;
    var integer: winner is 0;
    var integer: choice is 0;
    var integer: shown is 0;
    var integer: plays is 0;
  begin
    for plays range 1 to 10000 do
      winner := rand(1, 3);
      choice := rand(1, 3);
      repeat
        shown := rand(1, 3)
      until shown <> winner and shown <> choice;
      stayWins +:= ord(choice = winner);
      switchWins +:= ord(6 - choice - shown = winner);
    end for;
    writeln("Switching wins " <& switchWins <& " times");
    writeln("Staying wins " <& stayWins <& " times");
  end func;
```


Output:

```txt

Switching wins 6654 times
Staying wins 3346 times

```



## Sidef


```ruby
var n = 1000                                  # number of times to play
var switchWins = (var stayWins = 0)           # sum of each strategy's wins
 
n.times {                                     # play the game n times
   var prize = pick(^3)
   var chosen = pick(^3)
 
   var show;
   do {
        show = pick(^3)
   } while (show ~~ [chosen, prize])
 
   given(chosen) {
     when (prize)                 { stayWins   += 1 }
     when ([3 - show - prize])    { switchWins += 1 }
     default                      { die "~ error ~" }
   }
}
 
say ("Staying wins %.2f%% of the time."   % (100.0 * stayWins   / n))
say ("Switching wins %.2f%% of the time." % (100.0 * switchWins / n))
```

```txt

Staying wins 31.20% of the time.
Switching wins 68.80% of the time.

```



## SPAD

```SPAD

montyHall(n) ==
  wd:=[1+random(3) for j in 1..n]
  fc:=[1+random(3) for j in 1..n]
  st:=reduce(_+,[1 for j in 1..n | wd.j=fc.j])
  p:=(st/n)::DoubleFloat
  FORMAT(nil,"stay: ~A, switch: ~A",p,1-p)$Lisp

```

Domain:[http://fricas.github.io/api/Integer.html?highlight=random Integer]

```txt

(1) -> montyHall(1000)
   Compiling function montyHall with type PositiveInteger ->
      SExpression

   (1)  stay: 0.319, switch: 0.681
                                                            Type: SExpression
(2) -> montyHall(10000)

   (2)  stay: 0.3286, switch: 0.6714
                                                            Type: SExpression
(3) -> montyHall(100000)

   (3)  stay: 0.33526, switch: 0.66474
                                                            Type: SExpression

```



## Stata



```stata
clear
set obs 1000000
gen car=runiformint(1,3)
gen choice1=runiformint(1,3)
gen succ1=car==choice1
gen shown=cond(succ1,runiformint(1,2),6-car-choice1)
replace shown=shown+1 if succ1 & (car==1 | car==shown)
gen choice2=6-shown-choice1
gen succ2=car==choice2
tabstat succ1 succ2, s(mean)
```


'''Output'''


```txt
   stats |     succ1     succ2
---------+--------------------
    mean |   .333632   .666368
------------------------------
```



## Swift



```swift
import Foundation

func montyHall(doors: Int = 3, guess: Int, switch: Bool) -> Bool {
  guard doors > 2, guess > 0, guess <= doors else { fatalError() }

  let winningDoor = Int.random(in: 1...doors)

  return winningDoor == guess ? !`switch` : `switch`
}

var switchResults = [Bool]()

for _ in 0..<1_000 {
  let guess = Int.random(in: 1...3)
  let wasRight = montyHall(guess: guess, switch: true)

  switchResults.append(wasRight)
}

let switchWins = switchResults.filter({ $0 }).count

print("Switching would've won \((Double(switchWins) / Double(switchResults.count)) * 100)% of games")
print("Not switching would've won \(((Double(switchResults.count - switchWins)) / Double(switchResults.count)) * 100)% of games")
```


```txt
Switching would've won 66.8% of games
Not switching would've won 33.2% of games
```



## Tcl

A simple way of dealing with this one, based on knowledge of the underlying probabilistic system, is to use code like this:

```tcl
set stay 0; set change 0; set total 10000
for {set i 0} {$i<$total} {incr i} {
    if {int(rand()*3) == int(rand()*3)} {
        incr stay
    } else {
        incr change
    }
}
puts "Estimate: $stay/$total wins for staying strategy"
puts "Estimate: $change/$total wins for changing strategy"
```

But that's not really the point of this challenge; it should add the concealing factors too so that we're simulating not just the solution to the game, but also the game itself. (Note that we are using Tcl's lists here to simulate sets.)

We include a third strategy that is proposed by some people (who haven't thought much about it) for this game: just picking at random between all the doors offered by Monty the second time round.

```tcl
package require Tcl 8.5

# Utility: pick a random item from a list
proc pick list {
    lindex $list [expr {int(rand()*[llength $list])}]
}
# Utility: remove an item from a list if it is there
proc remove {list item} {
    set idx [lsearch -exact $list $item]
    return [lreplace $list $idx $idx]
}

# Codify how Monty will present the new set of doors to choose between
proc MontyHallAction {doors car picked} {
    set unpicked [remove $doors $picked]
    if {$car in $unpicked} {
        # Remove a random unpicked door without the car behind it
        set carless [remove $unpicked $car]
        return [list {*}[remove $carless [pick $carless]] $car]
        # Expressed this way so Monty Hall isn't theoretically
        # restricted to using 3 doors, though that could be written
        # as just: return [list $car]
    } else {
        # Monty has a real choice now...
        return [remove $unpicked [pick $unpicked]]
    }
}

# The different strategies you might choose
proc Strategy:Stay {originalPick otherChoices} {
    return $originalPick
}
proc Strategy:Change {originalPick otherChoices} {
    return [pick $otherChoices]
}
proc Strategy:PickAnew {originalPick otherChoices} {
    return [pick [list $originalPick {*}$otherChoices]]
}

# Codify one round of the game
proc MontyHallGameRound {doors strategy winCounter} {
    upvar 1 $winCounter wins
    set car [pick $doors]
    set picked [pick $doors]
    set newDoors [MontyHallAction $doors $car $picked]
    set picked [$strategy $picked $newDoors]
    # Check for win...
    if {$car eq $picked} {
        incr wins
    }
}

# We're always using three doors
set threeDoors {a b c}
set stay 0; set change 0; set anew 0
set total 10000
# Simulate each of the different strategies
for {set i 0} {$i<$total} {incr i} {
    MontyHallGameRound $threeDoors Strategy:Stay     stay
    MontyHallGameRound $threeDoors Strategy:Change   change
    MontyHallGameRound $threeDoors Strategy:PickAnew anew
}
# Print the results
puts "Estimate: $stay/$total wins for 'staying' strategy"
puts "Estimate: $change/$total wins for 'changing' strategy"
puts "Estimate: $anew/$total wins for 'picking anew' strategy"
```

This might then produce output like
 Estimate: 3340/10000 wins for 'staying' strategy
 Estimate: 6733/10000 wins for 'changing' strategy
 Estimate: 4960/10000 wins for 'picking anew' strategy
Of course, this challenge could also be tackled by putting up a GUI and letting the user be the source of the randomness. But that's moving away from the letter of the challenge and takes a lot of effort anyway...


## Transact SQL

T-SQL for general case:

```Transact SQL

---- BEGIN ------------
create table MONTY_HALL(
  NOE int,
  CAR int,
  ALTERNATIVE int,
  ORIGIN int,
  [KEEP] int,
  [CHANGE] int,
  [RANDOM] int
  )

-- INIT
truncate table MONTY_HALL

declare @N int  , @i int -- No of Experiments and their counter
declare @rooms  int  ,  -- number of rooms
        @origin int,  -- original choice
		@car int , -- room with car
		@alternative int -- alternative room

select @rooms = 3, @N  = 100000 , @i = 0

-- EXPERIMENTS LOOP
while  @i < @N begin
  select @car = FLOOR(rand()*@rooms)+1 , @origin = FLOOR(rand()*@rooms)+1
  select @alternative = FLOOR(rand()*(@rooms-1))+1
  select @alternative = case when @alternative < @origin then @alternative else  @alternative + 1 end
  select @alternative = case when @origin = @car then @alternative else @car end

  insert MONTY_HALL
  select @i,@car,@alternative,@origin,@origin,@alternative,case when rand() < 5e-1 then @origin else @alternative end

  select  @i = @i + 1
end

-- RESULTS
select  avg (case when [KEEP] = CAR then 1e0 else 0e0 end )*1e2 as [% OF WINS FOR KEEP],
        avg (case when [CHANGE] = CAR then 1e0 else 0e0 end )*1e2 as [% OF WINS FOR CHANGE],
        avg (case when [RANDOM] = CAR then 1e0 else 0e0 end )*1e2 as [% OF WINS FOR RANDOM]
from MONTY_HALL
---- END ------------

```


```txt

% OF WINS FOR KEEP     % OF WINS FOR CHANGE   % OF WINS FOR RANDOM
---------------------- ---------------------- ----------------------
33.607                 66.393                 49.938

```



## UNIX Shell

```bash
#!/bin/bash
# Simulates the "monty hall" probability paradox and shows results.
# http://en.wikipedia.org/wiki/Monty_Hall_problem
# (should rewrite this in C for faster calculating of huge number of rounds)
# (Hacked up by Éric Tremblay, 07.dec.2010)

num_rounds=10 #default number of rounds
num_doors=3 # default number of doors
[ "$1" = "" ] || num_rounds=$[$1+0]
[ "$2" = "" ] || num_doors=$[$2+0]

nbase=1 # or 0 if we want to see door numbers zero-based
num_win=0; num_lose=0

echo "Playing $num_rounds times, with $num_doors doors."
[ "$num_doors" -lt 3 ] && {
  echo "Hey, there has to be at least 3 doors!!"
  exit 1
}
echo

function one_round() {
  winning_door=$[$RANDOM % $num_doors ]
  player_picks_door=$[$RANDOM % $num_doors ]

  # Host leaves this door AND the player's first choice closed, opens all others
  # (this WILL loop forever if there is only 1 door)
  host_skips_door=$winning_door
  while [ "$host_skips_door" = "$player_picks_door" ]; do
    #echo -n "(Host looks at door $host_skips_door...) "
    host_skips_door=$[$RANDOM % $num_doors]
  done

  # Output the result of this round
  #echo "Round $[$nbase+current_round]: "
  echo -n "Player chooses #$[$nbase+$player_picks_door]. "
  [ "$num_doors" -ge 10 ] &&
    # listing too many door numbers (10 or more) will just clutter the output
    echo -n "Host opens all except #$[$nbase+$host_skips_door] and #$[$nbase+$player_picks_door]. " \
  || {
    # less than 10 doors, we list them one by one instead of "all except ?? and ??"
    echo -n "Host opens"
    host_opens=0
    while [ "$host_opens" -lt "$num_doors" ]; do
      [ "$host_opens" != "$host_skips_door" ] && [ "$host_opens" != "$player_picks_door" ] && \
      echo -n " #$[$nbase+$host_opens]"
      host_opens=$[$host_opens+1]
    done
    echo -n " "
  }
  echo -n "(prize is behind #$[$nbase+$winning_door]) "
  echo -n "Switch from $[$nbase+$player_picks_door] to $[$nbase+$host_skips_door]: "
  [ "$winning_door" = "$host_skips_door" ] && {
    echo "WIN."
    num_win=$[num_win+1]
  } || {
    echo "LOSE."
    num_lose=$[num_lose+1]
  }
} # end of function one_round

# ok, let's go
current_round=0
while [ "$num_rounds" -gt "$current_round" ]; do
  one_round
  current_round=$[$current_round+1]
done

echo
echo "Wins (switch to remaining door):  $num_win"
echo "Losses (first guess was correct): $num_lose"
exit 0
```

Output of a few runs:

```txt

$ ./monty_hall_problem.sh
Playing 10 times, with 3 doors.

Player chooses #2. Host opens #3 (prize is behind #1) Switch from 2 to 1: WIN.
Player chooses #1. Host opens #3 (prize is behind #2) Switch from 1 to 2: WIN.
Player chooses #2. Host opens #3 (prize is behind #2) Switch from 2 to 1: LOSE.
Player chooses #1. Host opens #2 (prize is behind #1) Switch from 1 to 3: LOSE.
Player chooses #2. Host opens #3 (prize is behind #1) Switch from 2 to 1: WIN.
Player chooses #2. Host opens #1 (prize is behind #2) Switch from 2 to 3: LOSE.
Player chooses #3. Host opens #1 (prize is behind #2) Switch from 3 to 2: WIN.
Player chooses #2. Host opens #1 (prize is behind #3) Switch from 2 to 3: WIN.
Player chooses #1. Host opens #3 (prize is behind #1) Switch from 1 to 2: LOSE.
Player chooses #1. Host opens #2 (prize is behind #3) Switch from 1 to 3: WIN.

Wins (switch to remaining door):  6
Losses (first guess was correct): 4


$ ./monty_hall_problem.sh 5 10
Playing 5 times, with 10 doors.

Player chooses #1. Host opens all except #10 and #1. (prize is behind #10) Switch from 1 to 10: WIN.
Player chooses #7. Host opens all except #8 and #7. (prize is behind #8) Switch from 7 to 8: WIN.
Player chooses #6. Host opens all except #1 and #6. (prize is behind #1) Switch from 6 to 1: WIN.
Player chooses #8. Host opens all except #3 and #8. (prize is behind #8) Switch from 8 to 3: LOSE.
Player chooses #6. Host opens all except #5 and #6. (prize is behind #5) Switch from 6 to 5: WIN.

Wins (switch to remaining door):  4
Losses (first guess was correct): 1


$ ./monty_hall_problem.sh 1000
Playing 1000 times, with 3 doors.

Player chooses #2. Host opens #1 (prize is behind #2) Switch from 2 to 3: LOSE.
Player chooses #3. Host opens #1 (prize is behind #2) Switch from 3 to 2: WIN.
[ ... ]
Player chooses #1. Host opens #3 (prize is behind #2) Switch from 1 to 2: WIN.
Player chooses #3. Host opens #2 (prize is behind #1) Switch from 3 to 1: WIN.

Wins (switch to remaining door):  655
Losses (first guess was correct): 345

```



## Ursala


This is the same algorithm as the Perl solution. Generate two lists
of 10000 uniformly distributed samples from {1,2,3}, count each
match as a win for the staying strategy, and count each non-match as a win
for the switching strategy.


```Ursala
#import std
#import nat
#import flo

rounds = 10000

car_locations   = arc{1,2,3}* iota rounds
initial_choices = arc{1,2,3}* iota rounds

staying_wins   = length (filter ==) zip(car_locations,initial_choices)
switching_wins = length (filter ~=) zip(car_locations,initial_choices)

format = printf/'%0.2f'+ (times\100.+ div+ float~~)\rounds

#show+

main =  ~&plrTS/<'stay:   ','switch: '> format* <staying_wins,switching_wins>
```

Output will vary slightly for each run due to randomness.

```txt

stay:   33.95
switch: 66.05

```



## Vedit macro language

Vedit macro language does not have random number generator, so one is implemented in subroutine RANDOM (the algorithm was taken from ANSI C library).

```vedit
#90 = Time_Tick			// seed for random number generator
#91 = 3				// random numbers in range 0 to 2
#1  = 0				// wins for "always stay" strategy
#2  = 0				// wins for "always switch" strategy
for (#10 = 0; #10 < 10000; #10++) {	// 10,000 iterations
    Call("RANDOM")
    #3 = Return_Value		// #3 = winning door
    Call("RANDOM")
    #4 = Return_Value		// #4 = players choice
    do {
	Call("RANDOM")
	#5 = Return_Value	// #5 = door to open
    } while (#5 == #3 || #5 == #4)
    if (#3 == #4) {		// original choice was correct
	#1++
    }
    if (#3 == 3 - #4 - #5) {	// switched choice was correct
	#2++
    }
}
Ins_Text("Staying wins:   ") Num_Ins(#1)
Ins_Text("Switching wins: ") Num_Ins(#2)
return

//--------------------------------------------------------------
// Generate random numbers in range 0 <= Return_Value < #91
//  #90 = Seed    (0 to 0x7fffffff)
//  #91 = Scaling (0 to 0xffff)

:RANDOM:
#92 = 0x7fffffff / 48271
#93 = 0x7fffffff % 48271
#90 = (48271 * (#90 % #92) - #93 * (#90 / #92)) & 0x7fffffff
return ((#90 & 0xffff) * #91 / 0x10000)
```


Sample output:

```txt

Staying winns:    3354
Switching winns:  6646

```



## X++


```x++
//Evidence of the Monty Hall solution in Dynamics AX (by Wessel du Plooy - HiGH Software).

    int changeWins    = 0;
    int noChangeWins  = 0;
    int attempts;
    int picked;
    int reveal;
    int switchdoor;
    int doors[];

    for (attempts = 0; attempts < 32768; attempts++)
    {
        doors[1] = 0;                                           //0 is a goat, 1 is a car
        doors[2] = 0;
        doors[3] = 0;
        doors[(xGlobal::randomPositiveInt32() mod 3) + 1] = 1;  //put a winner in a random door
        picked = (xGlobal::randomPositiveInt32() mod 3) + 1;    //pick a door, any door

        do
        {
            reveal = (xGlobal::randomPositiveInt32() mod 3) + 1;
        }
        while (doors[reveal] == 1 || reveal == picked);           //don't show the winner or the choice

        if (doors[picked] == 1)
            noChangeWins++;
        else
            changeWins++;
    }

    print strFmt("Switching wins %1 times.", changeWins);
    print strFmt("Staying   wins %1 times.", noChangeWins);
    pause;

```


Output:

```txt
Switching wins 21699 times.
Staying   wins 11069 times.
```



## XPL0


```XPL0
def Games = 10000;              \number of games simulated
int Game, Wins;
include c:\cxpl\codes;

proc Play(Switch);              \Play one game
int Switch;
int Car, Player, Player0, Monty;
[Car:= Ran(3);                  \randomly place car behind a door
Player0:= Ran(3);               \player randomly chooses a door
repeat  Monty:= Ran(3);         \Monty opens door revealing a goat
until   Monty # Car and Monty # Player0;
if Switch then                  \player switches to remaining door
        repeat  Player:= Ran(3);
        until   Player # Player0 and Player # Monty
else    Player:= Player0;       \player sticks with original door
if Player = Car then Wins:= Wins+1;
];

[Format(2,1);
Text(0, "Not switching doors wins car in ");
Wins:= 0;
for Game:= 0 to Games-1 do Play(false);
RlOut(0, float(Wins)/float(Games)*100.0);
Text(0, "% of games.^M^J");

Text(0, "But switching doors wins car in ");
Wins:= 0;
for Game:= 0 to Games-1 do Play(true);
RlOut(0, float(Wins)/float(Games)*100.0);
Text(0, "% of games.^M^J");
]
```


Example output:

```txt

Not switching doors wins car in 33.7% of games.
But switching doors wins car in 66.7% of games.

```



## zkl

```zkl
const games=0d100_000;

reg switcherWins=0, keeperWins=0, shown=0;
do(games){
   doors := L(0,0,0);
   doors[(0).random(3)] = 1; // Set which one has the car
   choice := (0).random(3);  // Choose a door
   while(1){ shown = (0).random(3);
      if (not (shown == choice or doors[shown] == 1)) break; }
   switcherWins += doors[3 - choice - shown];
   keeperWins   += doors[choice];
}

"Switcher Wins: %,d (%3.2f%%)".fmt(
   switcherWins, switcherWins.toFloat() / games * 100).println();
"Keeper Wins: %,d (%3.2f%%)".fmt(
   keeperWins, keeperWins.toFloat() / games * 100).println();
```

```txt

Switcher Wins: 66,730 (66.73%)
Keeper Wins: 33,270 (33.27%)

```

