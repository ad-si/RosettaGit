+++
title = "Nim Game"
date = 2019-08-14T00:27:16Z
aliases = []
[extra]
id = 22264
task = """
  Design a simple Nim game where the human player goes first,
  and the computer always wins.
  The game should enforce the rules.
"""
[taxonomies]
categories = []
tags = ["game", "puzzle"]
languages = [
  "asciidots",
  "bloop",
  "c",
  "c++",
  "crystal",
  "factor",
  "go",
  "javascript",
  "julia",
  "lisp",
  "lua",
  "nim",
  "perl",
  "perl6",
  "phix",
  "python",
  "racket",
  "rexx",
  "ruby",
  "rust",
  "scala",
  "smalltalk",
]
+++

Nim is a simple game where the second player -
if they know the trick - will always win.

The game has only 3 rules:

- start with 12 tokens
- each player takes 1, 2, or 3 tokens in turn
- the player who takes the last token wins.

To win every time, the second player simply takes 4 minus the number
the first player took.
So if the first player takes 1,
the second takes 3 - if the first player takes 2,
the second should take 2 - and if the first player takes 3,
the second player will take 1.


## AsciiDots


```AsciiDots

%$LMRTX
.-$"Nim Dots"-$""-

                                                  /$_"Number must be "\
             T                               /----~------\            |
             *M                          /---+-*-[o]     |            |
          R [-]\                        .>#3-+[>][<]-1#<.|            |
     .-#12>--^ \"stod "$-#_$-" ekat uoY"_$---/ \--*----*-/            |
.>$_"How many dots would you like to take"---#?---/                   |
 \X                                     X---------<".3 dna 1 neewteb"$/
            /-----*L                              |
           [-]--\ R                               |
            |   *-$_"Computer takes "-$_#-$" dots"/
          M-*#4[%]
            \---/

                  /----------------$"computer wins!"-&
              /---~--
              *#0[=]
L-------------*---*>$_#-$" dots remaining."-$""
                   T

```

out:

```txt
Nim Dots

How many dots would you like to take?: 3
You take 3 dots
9 dots remaining.

Computer takes 1 dots
8 dots remaining.

How many dots would you like to take?: 1
You take 1 dots
7 dots remaining.

Computer takes 3 dots
4 dots remaining.

How many dots would you like to take?: 2
You take 2 dots
2 dots remaining.

Computer takes 2 dots
0 dots remaining.

computer wins!
```


## BlooP

Bloop has no input capabilites, so the game is defined as a procedure,
called with 3 numbers (since the game will last only 3 rounds anyhow).
The procedure can be called with more numbers -
extra parameters are ignored in most implementations I have found.
Since there is no easy way to get more inputs,
any incorrect values are converted to correct ones.

```BlooP
DEFINE PROCEDURE ''DIVIDE'' [A,B]:
BLOCK 0: BEGIN
  IF A < B, THEN:
    QUIT BLOCK 0;
  CELL(0) <= 1;
  OUTPUT <= 1;
  LOOP AT MOST A TIMES:
  BLOCK 2: BEGIN
    IF OUTPUT * B = A, THEN:
    QUIT BLOCK 0;
    OUTPUT <= OUTPUT + 1;
    IF OUTPUT * B > A, THEN:
    BLOCK 3: BEGIN
      OUTPUT <= CELL(0);
      QUIT BLOCK 0;
    BLOCK 3: END;
    CELL(0) <= OUTPUT;
  BLOCK 2: END;
BLOCK 0: END.

DEFINE PROCEDURE ''MINUS'' [A,B]:
BLOCK 0: BEGIN
  IF A < B, THEN:
    QUIT BLOCK 0;
  LOOP AT MOST A TIMES:
  BLOCK 1: BEGIN
    IF OUTPUT + B = A, THEN:
      QUIT BLOCK 0;
    OUTPUT <= OUTPUT + 1;
  BLOCK 1: END;
BLOCK 0: END.

DEFINE PROCEDURE ''MODULUS'' [A,B]:
BLOCK 0: BEGIN
  CELL(0) <= DIVIDE[A,B];
  OUTPUT <= MINUS[A,CELL(0) * B];
BLOCK 0: END.

DEFINE PROCEDURE ''PLAYER_TURN'' [TOKENS_LEFT, TAKE]:
BLOCK 0: BEGIN
  CELL(0) <= TAKE;

  IF TAKE > 3, THEN:
  BLOCK 1: BEGIN
    CELL(0) <= MODULUS [TAKE, 3] + 1;
    PRINT ['take must be between 1 and 3. setting take to ', CELL(0), '.'];
  BLOCK 1: END;

  IF TAKE < 1, THEN:
  BLOCK 2: BEGIN
    CELL(0) <= 1;
    PRINT ['take must be between 1 and 3. setting take to 1.'];
  BLOCK 2: END;

  OUTPUT <= MINUS [TOKENS_LEFT, CELL(0)];

  PRINT ['player takes ', CELL(0), ' tokens.'];
  PRINT ['tokens remaining: ', OUTPUT];
  PRINT [''];
BLOCK 0: END.

DEFINE PROCEDURE ''COMPUTER_TURN'' [TOKENS_LEFT]:
BLOCK 0: BEGIN
  CELL(0) <= MODULUS [TOKENS_LEFT, 4];
  OUTPUT <= MINUS [TOKENS_LEFT, CELL(0)];

  PRINT ['computer takes ', CELL(0), ' tokens.'];
  PRINT ['tokens remaining: ', OUTPUT];
  PRINT [''];
BLOCK 0: END.

DEFINE PROCEDURE ''PLAY_GAME'' [FST, SEC, THD]:
BLOCK 0: BEGIN
  CELL(0) <= FST;
  CELL(1) <= SEC;
  CELL(2) <= THD;
  OUTPUT <= 12;

  LOOP 3 TIMES:
  BLOCK 1: BEGIN
    OUTPUT <= PLAYER_TURN [OUTPUT, CELL(0)];
    CELL(0) <= CELL(1);
    CELL(1) <= CELL(2);

    OUTPUT <= COMPUTER_TURN [OUTPUT];
  BLOCK 1: END;

  PRINT ['computer wins!'];
BLOCK 0: END.

PLAY_GAME [1,4,3];
```

Sample game:

```txt
 > PLAYER TAKES 1 TOKENS.
 > TOKENS REMAINING: 11
 >
 > COMPUTER TAKES 3 TOKENS.
 > TOKENS REMAINING: 8
 >
 > TAKE MUST BE BETWEEN 1 AND 3. SETTING TAKE TO 2.
 > PLAYER TAKES 2 TOKENS.
 > TOKENS REMAINING: 6
 >
 > COMPUTER TAKES 2 TOKENS.
 > TOKENS REMAINING: 4
 >
 > PLAYER TAKES 3 TOKENS.
 > TOKENS REMAINING: 1
 >
 > COMPUTER TAKES 1 TOKENS.
 > TOKENS REMAINING: 0
 >
 > COMPUTER WINS!
=> 0
```


## C

```c
#include  <stdio.h>

int playerTurn(int numTokens, int take);
int computerTurn(int numTokens);

int main(void)
{
	printf("Nim Game\n\n");

	int Tokens = 12;

	while(Tokens > 0)
	{
		printf("How many tokens would you like to take?: ");

		int uin;
		scanf("%i", &uin);

		int nextTokens = playerTurn(Tokens, uin);

		if (nextTokens == Tokens)
		{
			continue;
		}

		Tokens = nextTokens;

		Tokens = computerTurn(Tokens);
	}
	printf("Computer wins.");

	return 0;
}

int playerTurn(int numTokens, int take)
{
	if (take < 1 || take > 3)
	{
		printf("\nTake must be between 1 and 3.\n\n");
		return numTokens;
	}
	int remainingTokens = numTokens - take;

	printf("\nPlayer takes %i tokens.\n", take);
	printf("%i tokens remaining.\n\n", remainingTokens);

	return remainingTokens;
}

int computerTurn(int numTokens)
{
	int take = numTokens % 4;
	int remainingTokens = numTokens - take;

	printf("Computer takes %u tokens.\n", take);
	printf("%i tokens remaining.\n\n", remainingTokens);

	return remainingTokens;
}
```

Sample game:

```txt
Nim Game

How many tokens would you like to take?: 4
Take must be between 1 and 3.

How many tokens would you like to take?: 2
Player takes 2 tokens.
10 tokens remaining.

Computer takes 2 tokens.
8 tokens remaining.

How many tokens would you like to take?: 1
Player takes 1 tokens.
7 tokens remaining.

Computer takes 3 tokens.
4 tokens remaining.

How many tokens would you like to take?: 3
Player takes 3 tokens.
1 tokens remaining.

Computer takes 1 tokens.
0 tokens remaining.

Computer wins.
```


## C++

Translation from Go.

```c++
#include <iostream

#include <limits>

using namespace std;

void showTokens(int tokens) {
    cout << "Tokens remaining " << tokens << endl << endl;
}

int main() {
    int tokens = 12;
    while (true) {
        showTokens(tokens);
        cout << "  How many tokens 1, 2 or 3? ";
        int t;
        cin >> t;
        if (cin.fail()) {
            cin.clear();
            cin.ignore(numeric_limits<streamsize>::max(), '\n');
            cout << endl << "Invalid input, try again." << endl << endl;
        } else if (t < 1 || t > 3) {
            cout << endl << "Must be a number between 1 and 3, try again." << endl << endl;
        } else {
            int ct = 4 - t;
            string s  = (ct > 1) ? "s" : "";
            cout << "  Computer takes " << ct << " token" << s << endl << endl;
            tokens -= 4;
        }
        if (tokens == 0) {
            showTokens(0);
            cout << "  Computer wins!" << endl;
            return 0;
        }
    }
}
```

Sample game:

```txt
Tokens remaining 12

  How many tokens 1, 2 or 3? nim

Invalid input, try again.

Tokens remaining 12

  How many tokens 1, 2 or 3? 1
  Computer takes 3 tokens

Tokens remaining 8

  How many tokens 1, 2 or 3? 0

Must be a number between 1 and 3, try again.

Tokens remaining 8

  How many tokens 1, 2 or 3? 2
  Computer takes 2 tokens

Tokens remaining 4

  How many tokens 1, 2 or 3? 3
  Computer takes 1 token

Tokens remaining 0

  Computer wins!
```


## Common Lisp

```lisp
(defun pturn (curTokens)
	(write-string "How many tokens would you like to take?: ")
	(setq ans (read))
	(setq tokensRemaining (- curTokens ans))
	(format t "You take ~D tokens~%" ans)
	(printRemaining tokensRemaining)
	tokensRemaining)

(defun cturn (curTokens)
	(setq take (mod curTokens 4))
	(setq tokensRemaining (- curTokens take))
	(format t "Computer takes ~D tokens~%" take)
	(printRemaining tokensRemaining)
	tokensRemaining)

(defun printRemaining (remaining)
	(format t "~D tokens remaining~%~%" remaining))


(format t "LISP Nim~%~%")
(setq tok 12)
(loop
	(setq tok (pturn tok))
	(setq tok (cturn tok))
	(if (<= tok 0)
		(return)))
(write-string "Computer wins!")
```

Sample game:


```txt
LISP Nim

How many tokens would you like to take?: 2
You take 2 tokens
10 tokens remaining

Computer takes 2 tokens
8 tokens remaining

How many tokens would you like to take?: 1
You take 1 tokens
7 tokens remaining

Computer takes 3 tokens
4 tokens remaining

How many tokens would you like to take?: 3
You take 3 tokens
1 tokens remaining

Computer takes 1 tokens
0 tokens remaining

Computer wins!
```


## Crystal

```crystal
tokens = 12

until tokens <= 0
    puts "There are #{tokens} tokens remaining.\nHow many tokens do you take?"
    until (input = (gets || "").to_i?) && (1..3).includes? input
        puts "Enter an integer between 1 and 3."
    end
    puts "Player takes #{input} tokens.\nComputer takes #{4-input} tokens."
    tokens -= 4
end

puts "Computer wins."
```


## Factor

```factor
USING: interpolate io kernel math math.parser sequences ;
IN: rosetta-code.nim-game

: get-input ( -- n )
    "Number of tokens to take (1, 2, or 3): " write readln
    string>number dup { 1 2 3 } member?
    [ drop "Invalid move." print get-input ] unless ;

: .remaining ( n -- )
    nl [I -~~==[ ${} tokens remaining ]==~~-I] nl nl ;

: .choice ( str n -- )
    dup 1 = "" "s" ? [I ${} took ${} token${}I] nl ;

: (round) ( -- )
    "You" get-input "Computer" 4 pick - [ .choice ] 2bi@ ;

: round ( n -- n-4 )
    dup dup .remaining [ drop (round) 4 - round ] unless-zero ;

: nim-game ( -- ) 12 round drop "Computer wins!" print ;

MAIN: nim-game
```

out:


```txt
-~~==[ 12 tokens remaining ]==~~-

Number of tokens to take (1, 2, or 3): 1
You took 1 token
Computer took 3 tokens

-~~==[ 8 tokens remaining ]==~~-

Number of tokens to take (1, 2, or 3): 3
You took 3 tokens
Computer took 1 token

-~~==[ 4 tokens remaining ]==~~-

Number of tokens to take (1, 2, or 3): 4
Invalid move.
Number of tokens to take (1, 2, or 3): 2
You took 2 tokens
Computer took 2 tokens

-~~==[ 0 tokens remaining ]==~~-

Computer wins!
```


## Go

```go
package main

import (
    "bufio"
    "fmt"
    "os"
    "strconv"
)

func showTokens(tokens int) {
    fmt.Println("Tokens remaining", tokens, "\n")
}

func main() {
    tokens := 12
    scanner := bufio.NewScanner(os.Stdin)
    for {
        showTokens(tokens)
        fmt.Print("  How many tokens 1, 2 or 3? ")
        scanner.Scan()
        if scerr := scanner.Err(); scerr != nil {
            fmt.Println("Error reading standard input:", scerr)
            return
        }
        t, err := strconv.Atoi(scanner.Text())
        if err != nil || t < 1 || t > 3 {
            fmt.Println("\nMust be a number between 1 and 3, try again.\n")
        } else {
            ct := 4 - t
            s := "s"
            if ct == 1 {
                s = ""
            }
            fmt.Print("  Computer takes ", ct, " token", s, "\n\n")
            tokens -= 4
        }
        if tokens == 0 {
            showTokens(0)
            fmt.Println("  Computer wins!")
            return
        }
    }
}
```

Sample game:

```txt
Tokens remaining 12

  How many tokens 1, 2 or 3? 2
  Computer takes 2 tokens

Tokens remaining 8

  How many tokens 1, 2 or 3? 4

Must be a number between 1 and 3, try again.

Tokens remaining 8

  How many tokens 1, 2 or 3? 1
  Computer takes 3 tokens

Tokens remaining 4

  How many tokens 1, 2 or 3? 3
  Computer takes 1 token

Tokens remaining 0

  Computer wins!
```


## Javascript

###  Browser Version

This is the easy but dirty way - with prompt for input,
and `console.log` for output.
The Nim class was structured so that input and output could be customized,
for example to use HTML DOM elements for in and out, instead of the terminal.

```JavaScript
class Nim {
	constructor(tokens, printFun) {
		this.startTokens = tokens;
		this.tokens = tokens;
		this.printFun = printFun;
	}

	playerTurn(take) {
		take = Math.round(take);

		if (take < 1 || take > 3) {
			this.printFun("take must be between 1 and 3.\n")
			return false;
		}
		this.tokens -= take;
		this.printFun("Player takes " + take + " tokens.");
		this.printRemaining()

		if (this.tokens === 0) {
			this.printFun("Player wins!\n");
		}
		return true;
	}

	computerTurn() {
		let take = this.tokens % 4;
		this.tokens -= take;
		this.printFun("Computer takes " + take + " tokens.");
		this.printRemaining();

		if (this.tokens === 0) {
			this.printFun("Computer wins.\n");
		}

	}

	printRemaining() {
		this.printFun(this.tokens + " tokens remaining.\n");
	}
}


let game = new Nim(12, console.log);
while (true) {
	if (game.playerTurn(parseInt(prompt("How many tokens would you like to take?")))){
		game.computerTurn();
	}
	if (game.tokens == 0) {
		break;
	}
}
```

Sample game:

```txt
(prompt) How many tokens would you like to take? 2
Player takes 2 tokens.
10 tokens remaining.

Computer takes 2 tokens.
8 tokens remaining.

(prompt) How many tokens would you like to take? 4
take must be between 1 and 3.

(prompt) How many tokens would you like to take? 0
take must be between 1 and 3.

(prompt) How many tokens would you like to take? 3
Player takes 3 tokens.
5 tokens remaining.

Computer takes 1 tokens.
4 tokens remaining.

(prompt) How many tokens would you like to take? 1
Player takes 1 tokens.
3 tokens remaining.

Computer takes 3 tokens.
0 tokens remaining.

Computer wins.
```


## Julia

Translated from Perl 6.

```julia
function nimgame()
    tcount = 12
    takenum = 0
    while true
        while true
            permitted = collect(1:min(3,tcount))
            println("$tcount tokens remain.\nHow many do you take ($permitted)? ")
            takenum = parse(Int, strip(readline(stdin)))
            if takenum in permitted
                break
            end
        end
        tcount -= 4
        println("Computer takes $(4 - takenum). There are $tcount tokens left.")
        if tcount < 1
            println("Computer wins as expected.")
            break
        end
    end
end

nimgame()
```

out:

```txt
12 tokens remain.
How many do you take ([1, 2, 3])?
3
Computer takes 1. There are 8 tokens left.
8 tokens remain.
How many do you take ([1, 2, 3])?
2
Computer takes 2. There are 4 tokens left.
4 tokens remain.
How many do you take ([1, 2, 3])?
1
Computer takes 3. There are 0 tokens left.
Computer wins as expected.
```


## Kotlin

Translated from Go.

```scala
// Version 1.3.21

fun showTokens(tokens: Int) {
    println("Tokens remaining $tokens\n")
}

fun main() {
    var tokens = 12
    while (true) {
        showTokens(tokens)
        print("  How many tokens 1, 2 or 3? ")
        var t = readLine()!!.toIntOrNull()
        if (t == null || t < 1 || t > 3) {
            println("\nMust be a number between 1 and 3, try again.\n")
        } else {
            var ct = 4 - t
            var s = if (ct > 1) "s" else ""
            println("  Computer takes $ct token$s\n")
            tokens -= 4
        }
        if (tokens == 0) {
            showTokens(0)
            println("  Computer wins!")
            return
        }
    }
}
```

Sample game:

```txt
Tokens remaining 12

  How many tokens 1, 2 or 3? 3
  Computer takes 1 token

Tokens remaining 8

  How many tokens 1, 2 or 3? nim

Must be a number between 1 and 3, try again.

Tokens remaining 8

  How many tokens 1, 2 or 3? 2
  Computer takes 2 tokens

Tokens remaining 4

  How many tokens 1, 2 or 3? 1
  Computer takes 3 tokens

Tokens remaining 0

  Computer wins!
```


## Lua

```Lua
tokens = 12

print("Nim Game\n")
print("Starting with " .. tokens .. " tokens.\n\n")

function printRemaining()
	print(tokens .. " tokens remaining.\n")
end

function playerTurn(take)
	take = math.floor(take)
	if (take < 1 or take > 3) then
		print ("\nTake must be between 1 and 3.\n")
		return false
	end

	tokens = tokens - take

	print ("\nPlayer takes " .. take .. " tokens.")
	printRemaining()
	return true
end

function computerTurn()
	take = tokens % 4
	tokens = tokens - take

	print("Computer takes " .. take .. " tokens.")
	printRemaining()
end

while (tokens > 0) do
	io.write("How many tokens would you like to take?: ")
	if playerTurn(io.read("*n")) then
		computerTurn()
	end
end

print ("Computer wins.")
```

Sample game:

```txt
Nim Game

Starting with 12 tokens.


How many tokens would you like to take?: 3
Player takes 3 tokens.
9 tokens remaining.

Computer takes 1 tokens.
8 tokens remaining.

How many tokens would you like to take?: 4
Take must be between 1 and 3.

How many tokens would you like to take?: 1
Player takes 1 tokens.
7 tokens remaining.

Computer takes 3 tokens.
4 tokens remaining.

How many tokens would you like to take?: 2
Player takes 2 tokens.
2 tokens remaining.

Computer takes 2 tokens.
0 tokens remaining.

Computer wins.
```


## Nim

```nim
import strutils
import terminal

var tokens = 12

styledEcho(styleBright, "Nim in Nim\n")

proc echoTokens() =
  styledEcho(styleBright, "Tokens remaining: ", resetStyle, $tokens, "\n")

proc player() =
  var take = '0'
  styledEcho(styleBright, "- Your turn -")
  echo "How many tokens will you take?"
  while true:
    stdout.styledWrite(styleDim, "Take (1–3): ", resetStyle)
    take = getch()
    stdout.write(take, '\n')
    if take in {'1'..'3'}:
      tokens -= parseInt($take)
      break
    else:
      echo "Please choose a number between 1 and 3."
  echoTokens()

proc computer() =
  styledEcho(styleBright, "- Computer's turn -")
  let take = tokens mod 4
  tokens -= take
  styledEcho("Computer took ", styleBright, $take, " ",
             if take == 1: "token"
             else: "tokens")
  echoTokens()

while tokens > 0:
  player()
  computer()

styledEcho(styleBright, "Computer wins!")
```

out:

```txt
- Your turn -
How many tokens will you take?
Take (1–3): 1
Tokens remaining: 11

- Computer's turn -
Computer took 3 tokens
Tokens remaining: 8

- Your turn -
How many tokens will you take?
Take (1–3): 2
Tokens remaining: 6

- Computer's turn -
Computer took 2 tokens
Tokens remaining: 4

- Your turn -
How many tokens will you take?
Take (1–3): 3
Tokens remaining: 1

- Computer's turn -
Computer took 1 token
Tokens remaining: 0

Computer wins!
```


## Perl

Translated from Perl 6.

```perl
use strict;
use warnings;
use feature 'say';

my $tokens = 12;
say "$tokens tokens remaining.\n";

while (1) {
    print "How many tokens do you want to remove; 1, 2 or 3? : ";
    (my $player = <>) =~ s/\s//g;
    say "Nice try. $tokens tokens remaining.\n" and next
        unless $player =~ /^[123]$/;
    $tokens -= 4;
    say "Computer takes @{[4 - $player]}.\n$tokens tokens remaining.\n";
    say "Computer wins." and last
        if $tokens <= 0;
}
```

out:

```txt
12 tokens remaining.

How many tokens do you want to remove; 1, 2 or 3? : 3
Computer takes 1.
8 tokens remaining.

How many tokens do you want to remove; 1, 2 or 3? : -1
Nice try. 8 tokens remaining.

How many tokens do you want to remove; 1, 2 or 3? : 2
Computer takes 2.
4 tokens remaining.

How many tokens do you want to remove; 1, 2 or 3? : 1
Computer takes 3.
0 tokens remaining.

Computer wins.
```


## Perl 6

Works with Rakudo 2019.03.

```perl6
say my $tokens = 12, " tokens remaining.\n";

loop {
    my $player = trim prompt "How many tokens do you want to remove; 1, 2 or 3? : ";
    say "Nice try. $tokens tokens remaining.\n" and
    next unless $player eq any <1 2 3>;
    $tokens -= 4;
    say "Computer takes {4 - $player}.\n$tokens tokens remaining.\n";
    say "Computer wins." and last if $tokens <= 0;
}
```

Sample game:

```txt
12 tokens remaining.

How many tokens do you want to remove; 1, 2 or 3? : 3
Computer takes 1.
8 tokens remaining.

How many tokens do you want to remove; 1, 2 or 3? : 6
Nice try. 8 tokens remaining.

How many tokens do you want to remove; 1, 2 or 3? : G
Nice try. 8 tokens remaining.

How many tokens do you want to remove; 1, 2 or 3? : 2
Computer takes 2.
4 tokens remaining.

How many tokens do you want to remove; 1, 2 or 3? : 1
Computer takes 3.
0 tokens remaining.

Computer wins.
```


## Phix

Translated from Perl 6.

```Phix
integer tokens = 12, player = 0

while true do
    printf(1,"%2d tokens remaining. ",tokens)
    if tokens=0 then printf(1,"Computer wins.\n") exit end if
    printf(1,"How many tokens do you want to remove; 1, 2, or 3?:")
    while player<1 or player>3 do player=getc(0)-'0' end while
    printf(1,"%d. Computer takes %d.\n",{player,4-player})
    tokens -= 4; player = 0
end while
```

out:

```txt
12 tokens remaining. How many tokens do you want to remove; 1, 2, or 3?:1. Computer takes 3.
 8 tokens remaining. How many tokens do you want to remove; 1, 2, or 3?:2. Computer takes 2.
 4 tokens remaining. How many tokens do you want to remove; 1, 2, or 3?:3. Computer takes 1.
 0 tokens remaining. Computer wins.
```


## Python

Works with Python 3.

```Python
print("Py Nim\n")

def getTokens(curTokens):
	global tokens

	print("How many tokens would you like to take? ", end='')
	take = int(input())

	if (take < 1 or take > 3):
		print("Number must be between 1 and 3.\n")
		getTokens(curTokens)
		return

	tokens = curTokens - take
	print(f'You take {take} tokens.')
	print(f'{tokens} tokens remaining.\n')

def compTurn(curTokens):
	global tokens

	take = curTokens % 4
	tokens = curTokens - take
	print (f'Computer takes {take} tokens.')
	print (f'{tokens} tokens remaining.\n')


tokens = 12
while (tokens > 0):
	getTokens(tokens)
	compTurn(tokens)

print("Computer wins!")
```

out:


```txt
Py Nim

How many tokens would you like to take? 2
You take 2 tokens.
10 tokens remaining.

Computer takes 2 tokens.
8 tokens remaining.

How many tokens would you like to take? 1
You take 1 tokens.
7 tokens remaining.

Computer takes 3 tokens.
4 tokens remaining.

How many tokens would you like to take? 3
You take 3 tokens.
1 tokens remaining.

Computer takes 1 tokens.
0 tokens remaining.

Computer wins!
```


## Racket

```racket
#lang racket

(define (print-remaining tokens-remaining)
  (printf "~a tokens remain.\n" tokens-remaining))

(define (read-tokens)
  (define num (read))
  (cond
    [(and (natural? num) (< num 4)) num]
    [else
     (display "Please enter a number between 1 to 3\n")
     (read-tokens)]))

(define (pturn tokens-remaining)
  (cond
    [(not (zero? tokens-remaining))
        (print-remaining tokens-remaining)
        (display "Your turn. How many tokens? ")
        (define n (read-tokens))
        (cturn (- tokens-remaining n) n)]
    [else (display "Computer wins!")]))


(define (cturn tokens-remaining p-took)
  (cond
    [(not (zero? tokens-remaining))
      (print-remaining tokens-remaining)
      (define c-take (- 4 p-took))
      (printf "Computer takes ~a tokens\n" c-take)
      (pturn (- tokens-remaining c-take))]
  [else (display "You win!")]))

(pturn 12)
```


## REXX

Programming notes:
Extra error checking was done with specific informative error messages.
Also included was a method of quitting the game.
The number of (starting) tokens (the ''pot'')
can be specified on the command line, the default is '''12'''.

```rexx
/*REXX program plays the NIM game with a human opponent; the pot size can be specified. */
pad= copies('─', 8)                              /*eyecatcher literal used in messages. */
parse arg pot _ . 1 __                           /*obtain optional argument from the CL.*/
if pot=='' | pot==","  then pot= 12              /*Not specified?  Then use the default.*/
if _\==''       then do; call ser "Too many arguments entered: "    __;     exit 13;   end
if \isNum(pot)  then do; call ser "argument isn't numeric: "       pot;     exit 13;   end
if \isInt(pot)  then do; call ser "argument isn't an integer: "    pot;     exit 13;   end
if pot<4        then do; call ser "The pot number is too small: "  pot;     exit 13;   end
if pot>100      then do; call ser "The pot number is too large: "  pot;     exit 13;   end
pot= pot/1                                       /*normalize the pot  (number).         */

     do forever;   call show pot
            do  until ok;                   ok=1;               say
            say pad "How many tokens do you want to take away  (1, 2, or 3)    (or QUIT)?"
            parse pull t _ . 1 q 1 __;      upper q;            say
            if abbrev('QUIT',q,1)  then do;  say pad 'Quitting.';         exit 1;      end
            if t=''                then call ser "No arguments entered."
            if _\==''              then call ser "Too many arguments entered: "        __
            if \isNum(t)           then call ser "Argument isn't numeric: "             t
            if \isInt(t)           then call ser "Argument isn't an integer: "          t
            if t<1                 then call ser "Argument can't be less than 1: "      t
            if t>3                 then call ser "Argument can't be greater than 3: "   t
            end   /*while*/
     t= t/1                                      /*Normalize the number:  001   2.  +3  */
     #= 4-t                                      /*calculate the computer's take─away.  */
     say pad "The computer takes "    #    " token"s(#).
     pot= pot - t - #                            /*calculate the number of tokens in pot*/
     if pot==0  then do;   say pad 'No tokens left.'       /*No tokens left in the pot? */
                           say pad "The computer wins!"    /*Display a braggart message.*/
                           exit                            /*exit this computer program.*/
                     end
     end   /*forever*/                           /*keep looping until there's a winner. */
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
isNum: return datatype( arg(1), 'N')             /*verify that the arg is a  number.    */
isInt: return datatype( arg(1), 'W')             /*   "     "   "   "   " an integer.   */
show:  say;  say pad "Tokens remaining: "  arg(1)' ' pad;  say;  return
s:     if arg(1)==1  then return arg(3);                         return word(arg(2) 's',1)
ser:   if ok  then say pad '***error***' arg(1);       ok= 0;    return
```

Sample game:

```txt
──────── Tokens remaining:  12  ────────


──────── How many tokens do you want to take away  (1, 2, or 3)    (or QUIT)?
2                                            ◄■■■■■■■■■■■ user input

──────── The computer takes  2  tokens.

──────── Tokens remaining:  8  ────────


──────── How many tokens do you want to take away  (1, 2, or 3)    (or QUIT)?
3                                            ◄■■■■■■■■■■■ user input

──────── The computer takes  1  token.

──────── Tokens remaining:  4  ────────


──────── How many tokens do you want to take away  (1, 2, or 3)    (or QUIT)?
1                                            ◄■■■■■■■■■■■ user input

──────── The computer takes  3  tokens.
──────── No tokens left.
──────── The computer wins!

```


## Ruby

```ruby
[12, 8, 4].each do |remaining|
  puts "There are #{remaining} dots.\nHow many dots would you like to take? "
  unless (num=gets.to_i).between?(1, 3)
    puts "Please enter one of 1, 2 or 3"
    redo
  end
  puts "You took #{num} dots, leaving #{remaining-num}.\nComputer takes #{4-num}.\n\n"
end

puts "Computer took the last and wins."
```

out:

```txt
There are 12 dots.
How many dots would you like to take?
foo
Please enter one of 1, 2 or 3
There are 12 dots.
How many dots would you like to take?
1
You took 1 dots, leaving 11.
Computer takes 3.

There are 8 dots.
How many dots would you like to take?
3
You took 3 dots, leaving 5.
Computer takes 1.

There are 4 dots.
How many dots would you like to take?
2
You took 2 dots, leaving 2.
Computer takes 2.

Computer took the last and wins.
```


## Rust

```rust
use std::io;

fn main() {
    let mut tokens = 12;
    println!("Nim game");
    println!("Starting with {} tokens.", tokens);
    println!("");

    loop {
        tokens = p_turn(&tokens);
        print_remaining(&tokens);
        tokens = c_turn(&tokens);
        print_remaining(&tokens);

        if tokens == 0 {
            println!("Computer wins!");
            break;
        }
    }
}

fn p_turn(tokens: &i32) -> i32 {
    loop {  //try until we get a good number
        println!("How many tokens would you like to take?");

        let mut take = String::new();
        io::stdin().read_line(&mut take)
            .expect("Sorry, I didn't understand that.");

        let take: i32 = match take.trim().parse() {
            Ok(num) => num,
            Err(_) => {
                println!("Invalid input");
                println!("");
                continue;
            }
        };

        if take > 3 || take < 1 {
            println!("Take must be between 1 and 3.");
            println!("");
            continue;
        }

        return tokens - take;
    }
}

fn c_turn(tokens: &i32) -> i32 {
    let take = tokens % 4;

    println!("Computer takes {} tokens.", take);

    return tokens - take;
}

fn print_remaining(tokens: &i32) {
    println!("{} tokens remaining.", tokens);
    println!("");
}
```

Sample game:

```txt

Nim game
Starting with 12 tokens.

How many tokens would you like to take?
foo
Invalid input

How many tokens would you like to take?
3
9 tokens remaining.

Computer takes 1 tokens.
8 tokens remaining.

How many tokens would you like to take?
5
Take must be between 1 and 3.

How many tokens would you like to take?
2
6 tokens remaining.

Computer takes 2 tokens.
4 tokens remaining.

How many tokens would you like to take?
1
3 tokens remaining.

Computer takes 3 tokens.
0 tokens remaining.

Computer wins!
```


## Smalltalk

Works with GNU Smalltalk.

```smalltalk
Object subclass: Nim [
    | tokens |
    <comment: 'I am a game of nim'>
    Nim class >> new [
        <category: 'instance creation'>
        ^(super new) init: 12
    ]

    init: t [
        <category: 'instance creation'>
        tokens := t.
        ^self
    ]


    pTurn [
        | take |
        <category: 'gameplay'>
        Transcript nextPutAll: 'How many tokens will you take?: '.
        take := (stdin nextLine) asNumber.
        ((take < 1) | (take > 3))
            ifTrue: [Transcript nextPutAll: 'Invalid input';nl;nl. self pTurn]
            ifFalse: [tokens := tokens - take]
    ]

    cTurn [
        | take |
        <category: 'gameplay'>
        take := tokens - (4 * (tokens // 4)).    "tokens % 4"
        Transcript nextPutAll: 'Computer takes '.
        take printOn: Transcript.
        Transcript nextPutAll: ' tokens';nl.
        tokens := tokens - take
    ]

    mainLoop [
        <category: 'main loop'>
        Transcript nextPutAll: 'Nim game';nl.
        Transcript nextPutAll: 'Starting with '.
        tokens printOn: Transcript.
        Transcript nextPutAll: ' tokens';nl;nl.
        1 to: 3 do: [ :n |    "The computer always wins on the 3rd turn"
            self pTurn.
            self printRemaining.
            self cTurn.
            self printRemaining.
            (tokens = 0)
                ifTrue:[Transcript nextPutAll: 'Computer wins!';nl. ^0]
        ]
    ]

    printRemaining [
        <category: 'information'>
        tokens printOn: Transcript.
        Transcript nextPutAll: ' tokens remaining';nl;nl
    ]
]

g := Nim new.
g mainLoop.
```

Sample game:

```txt

Nim game
Starting with 12 tokens

How many tokens will you take?: foo
Invalid input

How many tokens will you take?: 3
9 tokens remaining

Computer takes 1 tokens
8 tokens remaining

How many tokens will you take?: 4
Invalid input

How many tokens will you take?: 2
6 tokens remaining

Computer takes 2 tokens
4 tokens remaining

How many tokens will you take?: 1
3 tokens remaining

Computer takes 3 tokens
0 tokens remaining

Computer wins!
```
