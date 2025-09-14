+++
title = "Spoof game"
description = ""
date = 2019-01-12T06:39:41Z
aliases = []
[extra]
id = 21792
[taxonomies]
categories = ["task"]
tags = []
languages = [
  "c",
  "go",
  "kotlin",
  "phix",
  "rexx",
  "ring",
  "zkl",
]
+++

Create Spoof game. See details: [https://en.wikipedia.org/wiki/Spoof_(game) Spoof game]





## C

```c
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

#define TRUE 1
#define FALSE 0
#define ESC 27
#define TEST TRUE  /* set to 'false' to erase each player's coins */

typedef int bool;

int get_number(const char *prompt, int min, int max, bool show_mm) {
    int n;
    char *line = NULL;
    size_t len = 0;
    ssize_t read;
    fflush(stdin);
    do {
        printf("%s", prompt);
        if (show_mm)
            printf(" from %d to %d : ", min, max);
        else
            printf(" : ");
        read = getline(&line, &len, stdin);
        if (read < 2) continue;
        n = atoi(line);
    }
    while (n < min || n > max);
    printf("\n");
    return n;
}

int compare_int(const void *a, const void* b) {
    int i = *(int *)a;
    int j = *(int *)b;
    return i - j;
}

int main() {
    int i, j, n, players, coins, first, round = 1, rem_size;
    int min, max, guess, index, index2, total;
    int remaining[9], hands[10], guesses[10];
    bool found, eliminated;
    char c;
    players = get_number("Number of players", 2, 9, TRUE);
    coins = get_number("Number of coins per player", 3, 6, TRUE);
    for (i = 0; i < 9; ++i) remaining[i] = i + 1;
    rem_size = players;
    srand(time(NULL));
    first = 1 + rand() % players;
    printf("The number of coins in your hand will be randomly determined for");
    printf("\neach round and displayed to you. However, when you press ENTER");
    printf("\nit will be erased so that the other players, who should look");
    printf("\naway until it's their turn, won't see it. When asked to guess");
    printf("\nthe total, the computer won't allow a 'bum guess'.\n");
    while(TRUE) {
        printf("\nROUND %d:\n", round);
        n = first;
        for (i = 0; i < 10; ++i) {
            hands[i] = 0; guesses[i] = -1;
        }
        do {
            printf("  PLAYER %d:\n", n);
            printf("    Please come to the computer and press ENTER\n");
            hands[n] = rand() % (coins + 1);
            printf("      <There are %d coin(s) in your hand>", hands[n]);
            while (getchar() != '\n');
            if (!TEST) {
                printf("%c[1A", ESC);  // move cursor up one line
                printf("%c[2K", ESC);  // erase line
                printf("\r\n");        // move cursor to beginning of line
            }
            else printf("\n");
            while (TRUE) {
                min = hands[n];
                max = (rem_size - 1) * coins + hands[n];
                guess = get_number("    Guess the total", min, max, FALSE);
                found = FALSE;
                for (i = 1; i < 10; ++i) {
                    if (guess == guesses[i]) {
                        found = TRUE;
                        break;
                    }
                }
                if (!found) {
                    guesses[n] = guess;
                    break;
                }
                printf("    Already guessed by another player, try again\n");
            }
            index = -1;
            for (i = 0; i < rem_size; ++i) {
                if (remaining[i] == n) {
                    index = i;
                    break;
                }
            }
            if (index < rem_size - 1)
                n = remaining[index + 1];
            else
                n = remaining[0];
        }
        while (n != first);
        total = 0;
        for (i = 1; i < 10; ++i) total += hands[i];
        printf("  Total coins held = %d\n", total);
        eliminated = FALSE;
        for (i = 0; i < rem_size; ++i) {
            j = remaining[i];
            if (guesses[j] == total) {
                printf("  PLAYER %d guessed correctly and is eliminated\n", j);
                remaining[i] = 10;
                rem_size--;
                qsort(remaining, players, sizeof(int), compare_int);
                eliminated = TRUE;
                break;
            }
        }
        if (!eliminated)
            printf("  No players guessed correctly in this round\n");
        else if (rem_size == 1) {
            printf("\nPLAYER %d buys the drinks!\n", remaining[0]);
            break;
        }
        index2 = -1;
        for (i = 0; i < rem_size; ++i) {
            if (remaining[i] == first) {
                index2 = i;
                break;
            }
        }
        if (index2 < rem_size - 1)
            first = remaining[index2 + 1];
        else
            first = remaining[0];
        round++;
    }
    return 0;
}
```


Sample game (TEST == true):

```txt

Number of players from 2 to 9 : 2

Number of coins per player from 3 to 6 : 4

The number of coins in your hand will be randomly determined for
each round and displayed to you. However, when you press ENTER
it will be erased so that the other players, who should look
away until it's their turn, won't see it. When asked to guess
the total, the computer won't allow a 'bum guess'.

ROUND 1:
  PLAYER 1:
    Please come to the computer and press ENTER
      <There are 4 coin(s) in your hand>

    Guess the total : 6

  PLAYER 2:
    Please come to the computer and press ENTER
      <There are 0 coin(s) in your hand>

    Guess the total : 3

  Total coins held = 4
  No players guessed correctly in this round

ROUND 2:
  PLAYER 2:
    Please come to the computer and press ENTER
      <There are 4 coin(s) in your hand>

    Guess the total : 5

  PLAYER 1:
    Please come to the computer and press ENTER
      <There are 3 coin(s) in your hand>

    Guess the total : 7

  Total coins held = 7
  PLAYER 1 guessed correctly and is eliminated

PLAYER 2 buys the drinks!

```



## Go

```go
package main

import (
    "bufio"
    "fmt"
    "log"
    "math/rand"
    "os"
    "strconv"
    "time"
)

const (
    esc  = "\033"
    test = true // set to 'false' to erase each player's coins
)

var scanner = bufio.NewScanner(os.Stdin)

func indexOf(s []int, el int) int {
    for i, v := range s {
        if v == el {
            return i
        }
    }
    return -1
}

func getNumber(prompt string, min, max int, showMinMax bool) (int, error) {
    for {
        fmt.Print(prompt)
        if showMinMax {
            fmt.Printf(" from %d to %d : ", min, max)
        } else {
            fmt.Printf(" : ")
        }
        scanner.Scan()
        if scerr := scanner.Err(); scerr != nil {
            return 0, scerr
        }
        input, err := strconv.Atoi(scanner.Text())
        if err == nil && input >= min && input <= max {
            fmt.Println()
            return input, nil
        }
    }
}

func check(err error, text string) {
    if err != nil {
        log.Fatalln(err, text)
    }
}

func main() {
    rand.Seed(time.Now().UnixNano())
    players, err := getNumber("Number of players", 2, 9, true)
    check(err, "when getting players")
    coins, err2 := getNumber("Number of coins per player", 3, 6, true)
    check(err2, "when getting coins")
    remaining := make([]int, players)
    for i := range remaining {
        remaining[i] = i + 1
    }
    first := 1 + rand.Intn(players)
    fmt.Println("The number of coins in your hand will be randomly determined for")
    fmt.Println("each round and displayed to you. However, when you press ENTER")
    fmt.Println("it will be erased so that the other players, who should look")
    fmt.Println("away until it's their turn, won't see it. When asked to guess")
    fmt.Println("the total, the computer won't allow a 'bum guess'.")
    for round := 1; ; round++ {
        fmt.Printf("\nROUND %d:\n\n", round)
        n := first
        hands := make([]int, players+1)
        guesses := make([]int, players+1)
        for i := range guesses {
            guesses[i] = -1
        }
        for {
            fmt.Printf("  PLAYER %d:\n", n)
            fmt.Println("    Please come to the computer and press ENTER")
            hands[n] = rand.Intn(coins + 1)
            fmt.Print("      <There are ", hands[n], " coin(s) in your hand>")
            scanner.Scan()
            check(scanner.Err(), "when pressing ENTER")
            if !test {
                fmt.Print(esc, "[1A") // move cursor up one line
                fmt.Print(esc, "[2K") // erase line
                fmt.Println("\r")     // move cursor to beginning of line
            } else {
                fmt.Println()
            }
            for {
                min := hands[n]
                max := (len(remaining)-1)*coins + hands[n]
                guess, err3 := getNumber("    Guess the total", min, max, false)
                check(err3, "when guessing the total")
                if indexOf(guesses, guess) == -1 {
                    guesses[n] = guess
                    break
                }
                fmt.Println("    Already guessed by another player, try again")
            }
            index := indexOf(remaining, n)
            if index < len(remaining)-1 {
                n = remaining[index+1]
            } else {
                n = remaining[0]
            }
            if n == first {
                break
            }
        }
        total := 0
        for _, hand := range hands {
            total += hand
        }
        fmt.Println("  Total coins held =", total)
        eliminated := false
        for _, v := range remaining {
            if guesses[v] == total {
                fmt.Println("  PLAYER", v, "guessed correctly and is eliminated")
                r := indexOf(remaining, v)
                remaining = append(remaining[:r], remaining[r+1:]...)
                eliminated = true
                break
            }
        }
        if !eliminated {
            fmt.Println("  No players guessed correctly in this round")
        } else if len(remaining) == 1 {
            fmt.Println("\nPLAYER", remaining[0], "buys the drinks!")
            return
        }
        index2 := indexOf(remaining, n)
        if index2 < len(remaining)-1 {
            first = remaining[index2+1]
        } else {
            first = remaining[0]
        }
    }
}
```


Sample game. Note that lines enclosed in angle brackets will be erased after the player presses ENTER if TEST = false.

```txt

Number of players from 2 to 9 : 2

Number of coins per player from 3 to 6 : 4

The number of coins in your hand will be randomly determined for
each round and displayed to you. However, when you press ENTER
it will be erased so that the other players, who should look
away until it's their turn, won't see it. When asked to guess
the total, the computer won't allow a 'bum guess'.

ROUND 1:

  PLAYER 2:
    Please come to the computer and press ENTER
      <There are 0 coin(s) in your hand>

    Guess the total : 3

  PLAYER 1:
    Please come to the computer and press ENTER
      <There are 2 coin(s) in your hand>

    Guess the total : 4

  Total coins held = 2
  No players guessed correctly in this round

ROUND 2:

  PLAYER 1:
    Please come to the computer and press ENTER
      <There are 4 coin(s) in your hand>

    Guess the total : 6

  PLAYER 2:
    Please come to the computer and press ENTER
      <There are 3 coin(s) in your hand>

    Guess the total : 7

  Total coins held = 7
  PLAYER 2 guessed correctly and is eliminated

PLAYER 1 buys the drinks!

```



## Kotlin

This program has a TEST mode. If you set it to true, the number of coins allocated to each player for each round won't be erased after he/she presses ENTER, which allows you to check it is working out the total for the round correctly.

```scala
// Version 1.2.40

import java.util.Random

const val ESC = '\u001B'
const val TEST = true  // set to 'false' to erase each player's coins

fun getNumber(prompt: String, min: Int, max: Int, showMinMax: Boolean) : Int {
    var input: Int?
    do {
        print(prompt)
        if (showMinMax)
            print(" from $min to $max : ")
        else
            print(" : ")
        input = readLine()!!.toIntOrNull()
    }
    while (input == null || input !in min..max)
    println()
    return input
}

fun main(args: Array<String>) {
    val rand = Random()
    val players = getNumber("Number of players", 2, 9, true)
    val coins = getNumber("Number of coins per player", 3, 6, true)
    var remaining = MutableList(players) { it + 1 }
    var first = 1 + rand.nextInt(players)
    var round = 1
    println("The number of coins in your hand will be randomly determined for")
    println("each round and displayed to you. However, when you press ENTER")
    println("it will be erased so that the other players, who should look")
    println("away until it's their turn, won't see it. When asked to guess")
    println("the total, the computer won't allow a 'bum guess'.")
    while(true) {
        println("\nROUND $round:\n")
        var n = first
        val hands = IntArray(players + 1)
        val guesses = IntArray(players + 1) { -1 }
        do {
            println("  PLAYER $n:")
            println("    Please come to the computer and press ENTER")
            hands[n] = rand.nextInt(coins + 1)
            print("      <There are ${hands[n]} coin(s) in your hand>")
            do {} while (readLine() == null)
            if (!TEST) {
                print("${ESC}[1A")  // move cursor up one line
                print("${ESC}[2K")  // erase line
                println("\r")       // move cursor to beginning of line
            }
            else println()
            while (true) {
                val min = hands[n]
                val max = (remaining.size - 1) * coins + hands[n]
                val guess = getNumber("    Guess the total", min, max, false)
                if (guess !in guesses) {
                    guesses[n] = guess
                    break
                }
                println("    Already guessed by another player, try again")
            }
            val index = remaining.indexOf(n)
            n = if (index < remaining.lastIndex) remaining[index + 1]
                else remaining[0]
        }
        while (n != first)
        val total = hands.sum()
        println("  Total coins held = $total")
        var eliminated = false
        for (i in remaining) {
            if (guesses[i] == total) {
                println("  PLAYER $i guessed correctly and is eliminated")
                remaining.remove(i)
                eliminated = true
                break
            }
        }
        if (!eliminated)
            println("  No players guessed correctly in this round")
        else if (remaining.size == 1) {
            println("\nPLAYER ${remaining[0]} buys the drinks!")
            return
        }
        val index2 = remaining.indexOf(n)
        first = if (index2 < remaining.lastIndex) remaining[index2 + 1]
                else remaining[0]
        round++
    }
}
```


Sample game. Note that lines enclosed in angle brackets will be erased after the player presses ENTER if TEST = false.

```txt

Number of players from 2 to 9 : 3

Number of coins per player from 3 to 6 : 3

The number of coins in your hand will be randomly determined for
each round and displayed to you. However, when you press ENTER
it will be erased so that the other players, who should look
away until it's their turn, won't see it. When asked to guess
the total, the computer won't allow a 'bum guess'.

ROUND 1:

  PLAYER 3:
    Please come to the computer and press ENTER
      <There are 2 coin(s) in your hand>

    Guess the total : 6

  PLAYER 1:
    Please come to the computer and press ENTER
      <There are 3 coin(s) in your hand>

    Guess the total : 7

  PLAYER 2:
    Please come to the computer and press ENTER
      <There are 3 coin(s) in your hand>

    Guess the total : 8

  Total coins held = 8
  PLAYER 2 guessed correctly and is eliminated

ROUND 2:

  PLAYER 1:
    Please come to the computer and press ENTER
      <There are 2 coin(s) in your hand>

    Guess the total : 4

  PLAYER 3:
    Please come to the computer and press ENTER
      <There are 0 coin(s) in your hand>

    Guess the total : 3

  Total coins held = 2
  No players guessed correctly in this round

ROUND 3:

  PLAYER 3:
    Please come to the computer and press ENTER
      <There are 3 coin(s) in your hand>

    Guess the total : 4

  PLAYER 1:
    Please come to the computer and press ENTER
      <There are 0 coin(s) in your hand>

    Guess the total : 3

  Total coins held = 3
  PLAYER 1 guessed correctly and is eliminated

PLAYER 3 buys the drinks!
```



## Phix


```Phix
function get_number(string prompt, integer lo, hi, pot, bool show=false)
    if show then prompt &= sprintf(" from %d to %d",{lo,hi}) end if
    printf(1,prompt&" : ")
    integer n = -1
    while n<lo or n>hi do
        n = wait_key()-'0'
        if n='!'-'0' and not show then
            printf(1,"\rYou have %d coins in your hand, max guess is %d\r",{lo,hi})
            sleep(2)
            printf(1,"                                                 \r")
            printf(1,prompt&" : ")
        end if
    end while
    if n=pot then
        printf(1,"**\n")
    else
        printf(1,"%d\n",n)
    end if
    return n
end function

constant help = """
Guess the total number of coins in everybody's hands.
Inputs/guesses between 1 and 9 can be entered using the keys 1 to 9, but
just to make life a little more interesting, guesses of 10 .. 54 must be
entered using :;<=>?@AB ... XYZ[\]^_`abcdef (no return key required).
Use ! to see your hand, obviously only when no-one else is looking.
You may not guess less than the number of coins in your hand, or more
than that plus (players-1)*maxcoins, or any already known wrong guess.

"""

procedure main()
    printf(1,help)
    integer players = get_number("Number of players", 2, 9, -1, show:=true),
            maxcoins = get_number("Max coins per player", 3, 6, -1, show:=true),
            first = rand(players), next = first
    printf(1,"player%d goes first\n",first)
    sequence hands = sq_sub(sq_rand(repeat(maxcoins+1,players)),1),
             remaining = tagset(players),
             game_log = repeat("",players),
             guessed = {}
    for i=1 to players do
        game_log[i] = sprintf("player%d:%s",{i,iff(i<first?" -":"")})
    end for
    integer pot = sum(hands),
            rm1 = (players-1)*maxcoins
    while length(remaining)>1 do
        integer player = remaining[next],
                hp = hands[player],
                guess = get_number(sprintf("player%d guess",player),hp,hp+rm1,pot)
        if find(guess,guessed) then
            printf(1,"Already guessed by another player, try again\n");
        else
            game_log[player] &= sprintf(" %d",guess)
            if guess==pot then
                printf(1,"player%d guessed correctly and is eliminated!\n",player)
                remaining[next..next] = {}
            else
                guessed &= guess
                next += 1
            end if
            if next>length(remaining) then next = 1 end if
        end if
    end while
    printf(1,"hands: %s\n",{sprint(hands)})
    printf(1,"Game log:\n")
    pp(game_log,{pp_Nest,1})
    printf(1,"pot:%d, player%d buys the drinks!\n",{pot,remaining[1]})
    {} = wait_key()
end procedure
main()
```

```txt

Guess the total number of coins in everybody's hands.
Inputs/guesses between 1 and 9 can be entered using the keys 1 to 9, but
just to make life a little more interesting, guesses of 10 .. 54 must be
entered using :;<=>?@AB ... XYZ[\]^_`abcdef (no return key required).
Use ! to see your hand, obviously only when no-one else is looking.
You may not guess less than the number of coins in your hand, or more
than that plus (players-1)*maxcoins, or any already known wrong guess.

Number of players from 2 to 9 : 3
Max coins per player from 3 to 6 : 6
player3 goes first
player3 guess : 18
player1 guess : 17
player2 guess : 13
player3 guess : **
player3 guessed correctly and is eliminated!
player1 guess : 9
player2 guess : 11
player1 guess : **
player1 guessed correctly and is eliminated!
hands: {5,1,6}
Game log:
{"player1: - 17 9 12",
 "player2: - 13 11",
 "player3: 18 12"}
pot:12, player2 buys the drinks!

```



## REXX

A method was used to enable a player to play the "same" game over again.

A little extra code was added to add verbiage for various error conditions.

```rexx
/*REXX program plays the "spoof" game with a human player (does presentation & scoring).*/
parse arg seed .;  if datatype(seed, 'W')  then call random ,,seed   /*use RANDOM seed? */
__= copies('─', 9)                               /*literal used in the game's prompting.*/

  do forever                                     /*$ = computer;   @ = human or CBLF.   */
       do until $pot+3<$g;  $pot = random(0, 3)  /*get a computer number for the pot.   */
                            $g   = random(0, 6)  /* "  "    "        "    "   "  guess. */
       end   /*until*/
  say
  say copies('─', 55);      say __ "The computer has got a pot and a guess."
  @pot= 0
  @pot= prompt(__ 'What is your pot?         (or  QUIT)'    )
  @g=   prompt(__ 'What is your guess?       (or  QUIT)',  .)
  say __  "The computer's  pot  is: "  $pot
  say __  "The computer's guess is: "  $g
  pot= $pot + @pot
  say
         select
         when $g==pot  &  @g==pot  then say __ 'This game is a draw.'
         when $g==pot              then say __ 'This game won by the computer.'
         when @g==pot              then say __ 'This game won by you.    Congratulations!'
         otherwise                      say __ 'This game has no winner.'
         end   /*select*/
  end          /*forever*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
prompt:  do forever;   er= __ '***error*** ';     say arg(1);       parse pull # y . 1 _,?
         upper #;      if abbrev('QUIT', #, 1)  then exit 1       /*user wants to quit? */
         if #==''             then do; say er "no argument entered."        ; iterate; end
         if y\==''            then do; say er "too many arguments:"        _; iterate; end
         if \datatype(#,'N')  then do; say er "argument isn't numeric:"    #; iterate; end
         if \datatype(#,'W')  then do; say er "argument isn't an integer:" #; iterate; end
         if ?==. & @pot+3>=#  then do; say er "illegal input for guess:"   #; iterate; end
         #= # / 1;            return #
         end   /*forever*/
```

```txt

───────────────────────────────────────────────────────
───────── The computer has got a pot and a guess.
───────── What is your pot?         (or  QUIT)
1                                                ◄■■■■■■■■■■■ user input.
───────── What is your guess?       (or  QUIT)
2                                                ◄■■■■■■■■■■■ user input.
───────── The computer's  pot  is:  1
───────── The computer's guess is:  6

───────── This game won by you.    Congratulations!

───────────────────────────────────────────────────────
───────── The computer has got a pot and a guess.
───────── What is your pot?         (or  QUIT)
3                                                ◄■■■■■■■■■■■ user input.
───────── What is your guess?       (or  QUIT)
4                                                ◄■■■■■■■■■■■ user input.
───────── The computer's  pot  is:  2
───────── The computer's guess is:  6

───────── This game has no winner.

───────────────────────────────────────────────────────
───────── The computer has got a pot and a guess.
───────── What is your pot?         (or  QUIT)
4                                                ◄■■■■■■■■■■■ user input.
───────── What is your guess?       (or  QUIT)
2                                                ◄■■■■■■■■■■■ user input.
───────── The computer's  pot  is:  1
───────── The computer's guess is:  5

───────── This game won by the computer.

───────────────────────────────────────────────────────
───────── The computer has got a pot and a guess.
───────── What is your pot?         (or  QUIT)
q                                                ◄■■■■■■■■■■■ user input.

```



## Ring


```ring

# Project : Spoof game

spoof = 0:6
see "How many games you want? "
give games

for n = 1 to games
     while true
             mypot = random(3)
             myguess = random(6)
             if mypot + 3 >= myguess
                loop
             else
                exit
             ok
     end
     see "I have put my pot and guess" + nl
     while true
             see "Your pot? "
             give yourpot
             see "Your guess? "
             give yourguess
             if ascii(yourpot) < 48 or ascii(yourpot) > 54 or ascii(yourguess) < 48 or ascii(yourguess) > 54
                loop
             ok
             if yourpot + 3 >= yourguess
                exit
             else
                see "Bad input! Try again" + nl
                loop
             ok
     end
     see "My put is: " + mypot + nl
     see "My guess is: " + myguess + nl
     pot = mypot + yourpot
     if myguess = pot and yourguess = pot
        see "Draw!" + nl
     elseif myguess = pot
        see "I won!" + nl
     elseif yourguess = pot
        see "You won!" + nl
     else
        see "No winner!" + nl
     ok
next

```

Output:

```txt

How many games you want? 4
I have put my pot and guess
Your pot? 1
Your guess? 3
My put is: 2
My guess is: 6
You won!
I have put my pot and guess
Your pot? 1
Your guess? 4
My put is: 2
My guess is: 5
No winner!
I have put my pot and guess
Your pot? 3
Your guess? 6
My put is: 3
My guess is: 6
Draw!
I have put my pot and guess
Your pot? 2
Your guess? 4
My put is: 0
My guess is: 5
No winner!

```



## zkl

```zkl
do(getNum("How many games you want? ")){
   reg mypot,yourpot,myguess,yourguess;
   do{ mypot,myguess = (0).random(4),(0).random(7);  // 0..3, 0..6
      }while(mypot + 3 <= myguess);
   println("I have put my pot and guess.");
   while(True){
      yourpot  =getNum("Your pot? ");
      yourguess=getNum("Your guess? ");
      if(((0<=yourpot<=6) and (0<=yourguess<=6)) and
         yourpot + 3 >= yourguess) break;
      println("Bad input! Try again");
   }
   pot:=mypot + yourpot;
   println("My pot is: ",mypot, ".  My guess is: ",myguess, ".  Pot is ",pot);
   if(myguess==pot==yourguess) println("Draw!\n");
   else if(myguess==pot)       println("I won!\n");
   else if(yourguess==pot)     println("You won!\n");
   else 		       println("No winner!\n");
}
fcn getNum(msg){
   try{ return(ask(msg).strip().toInt()) }
   catch{ println("ack"); return(self.fcn(msg)) }  // tail recursion
}
```

<pre style="height:35ex">
How many games you want? 4
I have put my pot and guess.
Your pot? 2
Your guess? 3
My pot is: 1.  My guess is: 0.  Pot is 3
You won!

I have put my pot and guess.
Your pot? 1
Your guess? 0
My pot is: 2.  My guess is: 3.  Pot is 3
I won!

I have put my pot and guess.
Your pot? 4
Your guess? 5
My pot is: 1.  My guess is: 1.  Pot is 5
You won!

I have put my pot and guess.
Your pot? 2
Your guess? 1
My pot is: 3.  My guess is: 2.  Pot is 5
No winner!

```

