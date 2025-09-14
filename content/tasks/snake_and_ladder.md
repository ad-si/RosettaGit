+++
title = "Snake And Ladder"
description = ""
date = 2019-05-07T23:39:44Z
aliases = []
[extra]
id = 21205
[taxonomies]
categories = ["task"]
tags = []
languages = [
  "c",
  "cpp",
  "csharp",
  "d",
  "go",
  "j",
  "java",
  "kotlin",
  "perl",
  "perl_6",
  "phix",
  "python",
  "racket",
  "rexx",
  "ruby",
  "visual_basic_dotnet",
]
+++

## Task

Create a [https://en.wikipedia.org/wiki/Snakes_and_Ladders Snakes and Ladders] game where the computer plays against a human player.


This game is also known as   '''Chutes and Ladders'''.       <!--    Alias for "Snakes and Ladders".    !-->

For a board example you can use this image here: [http://paulo-jorente.de/text/snakes_ledders.jpg snakes & ledders].

The way you'll represent the board and the players' tokens is totally up to you: graphics or ASCII graphics or even just text.

Happy coding.





## C

```c
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

bool sixesThrowAgain = true;

void init() {
    srand((unsigned int)time(NULL));
}

int rollDice() {
    return rand() % 6 + 1;
}

int nextSquare(int square) {
    switch (square) {
    case  4:  return 14;
    case  9:  return 31;
    case 17:  return  7;
    case 20:  return 38;
    case 28:  return 84;
    case 40:  return 59;
    case 51:  return 67;
    case 54:  return 34;
    case 62:  return 19;
    case 63:  return 81;
    case 64:  return 60;
    case 71:  return 91;
    case 87:  return 24;
    case 93:  return 73;
    case 95:  return 75;
    case 99:  return 78;
    default: return square;
    }
}

int turn(int player, int square) {
    int roll, next;
    while (true) {
        roll = rollDice();
        printf("Player %d, on square %d, rolls a %d", player, square, roll);
        if (square + roll > 100) {
            printf(" but cannot move.\n");
        } else {
            square += roll;
            printf(" and moves to square %d\n", square);
            if (square == 100) return 100;
            next = nextSquare(square);
            if (square < next) {
                printf("Yay! Landed on a ladder. Climb up to %d.\n", next);
                square = next;
            } else if (next < square) {
                printf("Oops! landed on a snake. Slither down to %d.\n", next);
                square = next;
            }
        }
        if (roll < 6 || !sixesThrowAgain) return square;
        printf("Rolled a 6 so roll again.\n");
    }
}

int main() {
    int players[3] = { 1, 1, 1 };
    int i, ns;

    init();

    while (true) {
        for (i = 0; i < sizeof(players) / sizeof(int); ++i) {
            ns = turn(i + 1, players[i]);
            if (ns == 100) {
                printf("Player %d wins!\n", i + 1);
                goto out;
            }
            players[i] = ns;
            printf("\n");
        }
    }

out:
    return 0;
}
```



## C++

```cpp
#include <iostream>
#include <map>
#include <random>

std::default_random_engine generator;
std::uniform_int_distribution<int> dice(1, 6);
int rollDice() {
    return dice(generator);
}

const bool sixesThrowAgain = true;
const std::map<int, int> snl{
    {4, 14},
    {9, 31},
    {17, 7},
    {20, 38},
    {28, 84},
    {40, 59},
    {51, 67},
    {54, 34},
    {62, 19},
    {63, 81},
    {64, 60},
    {71, 91},
    {87, 24},
    {93, 73},
    {95, 75},
    {99, 78},
};

// taken from https://stackoverflow.com/a/2333816
template <template<class, class, class...> class C, typename K, typename V, typename... Args>
V GetWithDef(const C<K, V, Args...>& m, K const& key, const V & defval) {
    typename C<K, V, Args...>::const_iterator it = m.find(key);
    if (it == m.end())
        return defval;
    return it->second;
}

int turn(int player, int square) {
    while (true) {
        int roll = rollDice();
        printf("Player %d, on square %d, rolls a %d", player, square, roll);
        if (square + roll > 100) {
            printf(" but cannot move.\n");
        } else {
            square += roll;
            printf(" and moves to square %d\n", square);
            if (square == 100) return 100;
            int next = GetWithDef(snl, square, square);
            if (square < next) {
                printf("Yay! Landed on a ladder. Climb up to %d.\n", next);
                square = next;
            } else if (next < square) {
                printf("Oops! landed on a snake. Slither down to %d.\n", next);
                square = next;
            }
        }
        if (roll < 6 || !sixesThrowAgain)return square;
        printf("Rolled a 6 so roll again.\n");
    }
}

int main() {
    // three players starting on square one
    int players[] = { 1, 1, 1 };

    while (true) {
        for (int i = 0; i < sizeof(players) / sizeof(int); ++i) {
            int ns = turn(i + 1, players[i]);
            if (ns == 100) {
                printf("Player %d wins!\n", i + 1);
                goto out;
            }
            players[i] = ns;
            printf("\n");
        }
    }

out:
    return 0;
}
```


## C#
```c#
using System;
using System.Collections.Generic;

namespace SnakeAndLadder {
    class Program {
        private static Dictionary<int, int> snl = new Dictionary<int, int>() {
            {4, 14},
            {9, 31},
            {17, 7},
            {20, 38},
            {28, 84},
            {40, 59},
            {51, 67},
            {54, 34},
            {62, 19},
            {63, 81},
            {64, 60},
            {71, 91},
            {87, 24},
            {93, 73},
            {95, 75},
            {99, 78},
        };
        private static Random rand = new Random();
        private const bool sixesThrowAgain = true;

        static int Turn(int player, int square) {
            while (true) {
                int roll = rand.Next(1, 6);
                Console.Write("Player {0}, on square {0}, rolls a {0}", player, square, roll);
                if (square + roll > 100) {
                    Console.WriteLine(" but cannot move.");
                } else {
                    square += roll;
                    Console.WriteLine(" and moves to square {0}", square);
                    if (square == 100) return 100;
                    int next = square;
                    if (snl.ContainsKey(square)) {
                        next = snl[square];
                    }
                    if (square < next) {
                        Console.WriteLine("Yay! Landed on a ladder. Climb up to {0}.", next);
                        if (next == 100) return 100;
                        square = next;
                    } else if (square > next) {
                        Console.WriteLine("Oops! Landed on a snake. Slither down to {0}.", next);
                    }
                }
                if (roll < 6 || !sixesThrowAgain) return square;
                Console.WriteLine("Rolled a 6 so roll again.");
            }
        }

        static void Main(string[] args) {
            // three players atarting on square one
            int[] players = { 1, 1, 1 };
            while (true) {
                for (int i = 0; i < players.Length; i++) {
                    int ns = Turn(i + 1, players[i]);
                    if (ns == 100) {
                        Console.WriteLine("Player {0} wins!", i + 1);
                        return;
                    }
                    players[i] = ns;
                    Console.WriteLine();
                }
            }
        }
    }
}
```



## D

```D
import std.stdio;

//Board layout, start square to end square
auto snl() {
    // Workaround for not being able to static initialize an AA
    int[int] temp;
    temp[4] = 14;
    temp[9] = 31;
    temp[17] = 7;
    temp[20] = 38;
    temp[28] = 84;
    temp[40] = 59;
    temp[51] = 67;
    temp[54] = 34;
    temp[62] = 19;
    temp[63] = 81;
    temp[64] = 60;
    temp[71] = 91;
    temp[87] = 24;
    temp[93] = 73;
    temp[95] = 75;
    temp[99] = 78;
    return temp;
}

int turn(int player, int square) {
    import std.random;

    auto roll = uniform!"[]"(1, 6);
    write("Player ", player, " on square ", square, " rolls a ", roll);
    if (square + roll > 100) {
        writeln(" but cannot move. Next players turn.");
        return square;
    } else {
        square += roll;
        writeln(" and moves to square ", square);
    }

    auto next = snl().get(square, square);
    if (square < next) {
        writeln("Yay! Landed on a ladder. Climb up to ", next);
    } else if (square > next) {
        writeln("Oops! Landed on a snake. Slither down to ", next);
    }
    return next;
}

void main() {
    // three players starting on square one
    auto players = [1, 1, 1];

    while(true) {
        foreach(i,s; players) {
            auto ns = turn(i+1, s);
            if (ns == 100) {
                writeln("Player ", i+1, " wins!");
                return;
            }
            players[i] = ns;
            writeln;
        }
    }
}
```

```txt
Player 1 on square 1 rolls a 5 and moves to square 6

Player 2 on square 1 rolls a 6 and moves to square 7

Player 3 on square 1 rolls a 5 and moves to square 6

Player 1 on square 6 rolls a 6 and moves to square 12

Player 2 on square 7 rolls a 4 and moves to square 11

Player 3 on square 6 rolls a 3 and moves to square 9
Yay! Landed on a ladder. Climb up to 31

Player 1 on square 12 rolls a 1 and moves to square 13

Player 2 on square 11 rolls a 1 and moves to square 12

Player 3 on square 31 rolls a 1 and moves to square 32

Player 1 on square 13 rolls a 5 and moves to square 18

Player 2 on square 12 rolls a 5 and moves to square 17
Oops! Landed on a snake. Slither down to 7

Player 3 on square 32 rolls a 5 and moves to square 37

Player 1 on square 18 rolls a 3 and moves to square 21

Player 2 on square 7 rolls a 2 and moves to square 9
Yay! Landed on a ladder. Climb up to 31

Player 3 on square 37 rolls a 3 and moves to square 40
Yay! Landed on a ladder. Climb up to 59

Player 1 on square 21 rolls a 3 and moves to square 24

Player 2 on square 31 rolls a 2 and moves to square 33

Player 3 on square 59 rolls a 5 and moves to square 64
Oops! Landed on a snake. Slither down to 60

Player 1 on square 24 rolls a 3 and moves to square 27

Player 2 on square 33 rolls a 5 and moves to square 38

Player 3 on square 60 rolls a 1 and moves to square 61

Player 1 on square 27 rolls a 6 and moves to square 33

Player 2 on square 38 rolls a 3 and moves to square 41

Player 3 on square 61 rolls a 3 and moves to square 64
Oops! Landed on a snake. Slither down to 60

Player 1 on square 33 rolls a 3 and moves to square 36

Player 2 on square 41 rolls a 1 and moves to square 42

Player 3 on square 60 rolls a 4 and moves to square 64
Oops! Landed on a snake. Slither down to 60

Player 1 on square 36 rolls a 2 and moves to square 38

Player 2 on square 42 rolls a 2 and moves to square 44

Player 3 on square 60 rolls a 4 and moves to square 64
Oops! Landed on a snake. Slither down to 60

Player 1 on square 38 rolls a 5 and moves to square 43

Player 2 on square 44 rolls a 6 and moves to square 50

Player 3 on square 60 rolls a 5 and moves to square 65

Player 1 on square 43 rolls a 2 and moves to square 45

Player 2 on square 50 rolls a 3 and moves to square 53

Player 3 on square 65 rolls a 4 and moves to square 69

Player 1 on square 45 rolls a 1 and moves to square 46

Player 2 on square 53 rolls a 4 and moves to square 57

Player 3 on square 69 rolls a 4 and moves to square 73

Player 1 on square 46 rolls a 3 and moves to square 49

Player 2 on square 57 rolls a 6 and moves to square 63
Yay! Landed on a ladder. Climb up to 81

Player 3 on square 73 rolls a 1 and moves to square 74

Player 1 on square 49 rolls a 2 and moves to square 51
Yay! Landed on a ladder. Climb up to 67

Player 2 on square 81 rolls a 4 and moves to square 85

Player 3 on square 74 rolls a 4 and moves to square 78

Player 1 on square 67 rolls a 3 and moves to square 70

Player 2 on square 85 rolls a 4 and moves to square 89

Player 3 on square 78 rolls a 4 and moves to square 82

Player 1 on square 70 rolls a 3 and moves to square 73

Player 2 on square 89 rolls a 3 and moves to square 92

Player 3 on square 82 rolls a 3 and moves to square 85

Player 1 on square 73 rolls a 3 and moves to square 76

Player 2 on square 92 rolls a 2 and moves to square 94

Player 3 on square 85 rolls a 5 and moves to square 90

Player 1 on square 76 rolls a 4 and moves to square 80

Player 2 on square 94 rolls a 6 and moves to square 100
Player 2 wins!
```



## Go

```go
package main

import (
    "fmt"
    "math/rand"
    "time"
)

var snl = map[int]int{
    4: 14, 9: 31, 17: 7, 20: 38, 28: 84, 40: 59, 51: 67, 54: 34,
    62: 19, 63: 81, 64: 60, 71: 91, 87: 24, 93: 73, 95: 75, 99: 78,
}

const sixThrowsAgain = true

func turn(player, square int) int {
    for {
        roll := 1 + rand.Intn(6)
        fmt.Printf("Player %d, on square %d, rolls a %d", player, square, roll)
        if square+roll > 100 {
            fmt.Println(" but cannot move.")
        } else {
            square += roll
            fmt.Printf(" and moves to square %d.\n", square)
            if square == 100 {
                return 100
            }
            next, ok := snl[square]
            if !ok {
                next = square
            }
            if square < next {
                fmt.Printf("Yay! Landed on a ladder. Climb up to %d.\n", next)
                if next == 100 {
                    return 100
                }
                square = next
            } else if square > next {
                fmt.Printf("Oops! Landed on a snake. Slither down to %d.\n", next)
                square = next
            }
        }
        if roll < 6 || !sixThrowsAgain {
            return square
        }
        fmt.Println("Rolled a 6 so roll again.")
    }
}

func main() {
    rand.Seed(time.Now().UnixNano())
    // three players starting on square one
    players := [3]int{1, 1, 1}
    for {
        for i, s := range players {
            ns := turn(i+1, s)
            if ns == 100 {
                fmt.Println("Player", i+1, "wins!")
                return
            }
            players[i] = ns
            fmt.Println()
        }
    }
}
```


Sample game:

```txt

Player 1, on square 1, rolls a 3 and moves to square 4.
Yay! Landed on a ladder. Climb up to 14.

Player 2, on square 1, rolls a 2 and moves to square 3.

Player 3, on square 1, rolls a 3 and moves to square 4.
Yay! Landed on a ladder. Climb up to 14.

Player 1, on square 14, rolls a 5 and moves to square 19.

Player 2, on square 3, rolls a 2 and moves to square 5.

Player 3, on square 14, rolls a 4 and moves to square 18.

Player 1, on square 19, rolls a 5 and moves to square 24.

Player 2, on square 5, rolls a 1 and moves to square 6.

Player 3, on square 18, rolls a 3 and moves to square 21.

Player 1, on square 24, rolls a 1 and moves to square 25.

Player 2, on square 6, rolls a 4 and moves to square 10.

Player 3, on square 21, rolls a 6 and moves to square 27.
Rolled a 6 so roll again.
Player 3, on square 27, rolls a 6 and moves to square 33.
Rolled a 6 so roll again.
Player 3, on square 33, rolls a 4 and moves to square 37.

Player 1, on square 25, rolls a 5 and moves to square 30.

Player 2, on square 10, rolls a 4 and moves to square 14.

Player 3, on square 37, rolls a 5 and moves to square 42.

Player 1, on square 30, rolls a 5 and moves to square 35.

Player 2, on square 14, rolls a 3 and moves to square 17.
Oops! Landed on a snake. Slither down to 7.

Player 3, on square 42, rolls a 5 and moves to square 47.

Player 1, on square 35, rolls a 5 and moves to square 40.
Yay! Landed on a ladder. Climb up to 59.

Player 2, on square 7, rolls a 5 and moves to square 12.

Player 3, on square 47, rolls a 6 and moves to square 53.
Rolled a 6 so roll again.
Player 3, on square 53, rolls a 6 and moves to square 59.
Rolled a 6 so roll again.
Player 3, on square 59, rolls a 3 and moves to square 62.
Oops! Landed on a snake. Slither down to 19.

Player 1, on square 59, rolls a 2 and moves to square 61.

Player 2, on square 12, rolls a 4 and moves to square 16.

Player 3, on square 19, rolls a 6 and moves to square 25.
Rolled a 6 so roll again.
Player 3, on square 25, rolls a 4 and moves to square 29.

Player 1, on square 61, rolls a 2 and moves to square 63.
Yay! Landed on a ladder. Climb up to 81.

Player 2, on square 16, rolls a 5 and moves to square 21.

Player 3, on square 29, rolls a 3 and moves to square 32.

Player 1, on square 81, rolls a 5 and moves to square 86.

Player 2, on square 21, rolls a 3 and moves to square 24.

Player 3, on square 32, rolls a 4 and moves to square 36.

Player 1, on square 86, rolls a 4 and moves to square 90.

Player 2, on square 24, rolls a 6 and moves to square 30.
Rolled a 6 so roll again.
Player 2, on square 30, rolls a 3 and moves to square 33.

Player 3, on square 36, rolls a 1 and moves to square 37.

Player 1, on square 90, rolls a 5 and moves to square 95.
Oops! Landed on a snake. Slither down to 75.

Player 2, on square 33, rolls a 1 and moves to square 34.

Player 3, on square 37, rolls a 3 and moves to square 40.
Yay! Landed on a ladder. Climb up to 59.

Player 1, on square 75, rolls a 5 and moves to square 80.

Player 2, on square 34, rolls a 5 and moves to square 39.

Player 3, on square 59, rolls a 3 and moves to square 62.
Oops! Landed on a snake. Slither down to 19.

Player 1, on square 80, rolls a 4 and moves to square 84.

Player 2, on square 39, rolls a 3 and moves to square 42.

Player 3, on square 19, rolls a 1 and moves to square 20.
Yay! Landed on a ladder. Climb up to 38.

Player 1, on square 84, rolls a 5 and moves to square 89.

Player 2, on square 42, rolls a 2 and moves to square 44.

Player 3, on square 38, rolls a 1 and moves to square 39.

Player 1, on square 89, rolls a 4 and moves to square 93.
Oops! Landed on a snake. Slither down to 73.

Player 2, on square 44, rolls a 4 and moves to square 48.

Player 3, on square 39, rolls a 1 and moves to square 40.
Yay! Landed on a ladder. Climb up to 59.

Player 1, on square 73, rolls a 3 and moves to square 76.

Player 2, on square 48, rolls a 2 and moves to square 50.

Player 3, on square 59, rolls a 1 and moves to square 60.

Player 1, on square 76, rolls a 2 and moves to square 78.

Player 2, on square 50, rolls a 2 and moves to square 52.

Player 3, on square 60, rolls a 4 and moves to square 64.
Oops! Landed on a snake. Slither down to 60.

Player 1, on square 78, rolls a 5 and moves to square 83.

Player 2, on square 52, rolls a 2 and moves to square 54.
Oops! Landed on a snake. Slither down to 34.

Player 3, on square 60, rolls a 4 and moves to square 64.
Oops! Landed on a snake. Slither down to 60.

Player 1, on square 83, rolls a 1 and moves to square 84.

Player 2, on square 34, rolls a 2 and moves to square 36.

Player 3, on square 60, rolls a 3 and moves to square 63.
Yay! Landed on a ladder. Climb up to 81.

Player 1, on square 84, rolls a 1 and moves to square 85.

Player 2, on square 36, rolls a 4 and moves to square 40.
Yay! Landed on a ladder. Climb up to 59.

Player 3, on square 81, rolls a 5 and moves to square 86.

Player 1, on square 85, rolls a 5 and moves to square 90.

Player 2, on square 59, rolls a 4 and moves to square 63.
Yay! Landed on a ladder. Climb up to 81.

Player 3, on square 86, rolls a 3 and moves to square 89.

Player 1, on square 90, rolls a 2 and moves to square 92.

Player 2, on square 81, rolls a 3 and moves to square 84.

Player 3, on square 89, rolls a 2 and moves to square 91.

Player 1, on square 92, rolls a 3 and moves to square 95.
Oops! Landed on a snake. Slither down to 75.

Player 2, on square 84, rolls a 2 and moves to square 86.

Player 3, on square 91, rolls a 6 and moves to square 97.
Rolled a 6 so roll again.
Player 3, on square 97, rolls a 3 and moves to square 100.
Player 3 wins!

```



## J

'''Solution'''

As discussed in other implementations, this game is purely determined by chance and no user input is actually required.

The following game starts with players' counters off the board, an exact roll is not required to win and no additional dice roll results from throwing a six.

```j
require 'format/printf general/misc/prompt'
SnakesLadders=:  _2 ]\ 4 14 9 31 17 7 20 38 28 84 40 59 51 67 54 34 62 19 63 81 64 60 71 91 87 24 93 73 95 75 99 78
'idx val'=:  |: SnakesLadders
Board=: val idx} i. >: 100               NB. representation of the board

rollDice=: 1 + $ ?@$ 6:                  NB. roll dice once for each player
playTurn=: Board {~ 100 <. rollDice + ]  NB. returns list of new posisitions given a list of starting positions

NB. Given the number of players, runs until one player reaches 100
runGame=: [: playTurn^:(100 -.@e. ])^:(<_) $&0

NB. Report player positions after each turn and result
report=: ('Player %d won!' sprintf 100 >:@i.~ {:) , echo

start=: >:@".@prompt&'How many players to play against?' [ echo bind 'You are Player 1!'
playSnakesLadders=: [: report@runGame start
```


'''Example Usage:'''

```j
   playSnakesLadders''
You are Player 1!
How many players to play against?
4
 0  0  0   0  0
 5  5  5   5  6
 7 31 11   8  8
13 35 16  14 10
14 36  7  18 11
38 42 31  21 14
39 46 35  25 15
41 48 37  84  7
42 34 41  90  8
47 39 43  96 31
50 59 48  78 37
56 60 34  81 39
59 61 39  84 41
60 67 43  88 44
60 73 44  94 45
61 79 50 100 49
Player 4 won!
```



## Java


```java
import java.util.Map;
import java.util.Random;

public class Game {
    private static final Map<Integer, Integer> snl = Map.ofEntries(
        Map.entry(4, 14),
        Map.entry(9, 31),
        Map.entry(17, 7),
        Map.entry(20, 38),
        Map.entry(28, 84),
        Map.entry(40, 59),
        Map.entry(51, 67),
        Map.entry(54, 34),
        Map.entry(62, 19),
        Map.entry(63, 81),
        Map.entry(64, 60),
        Map.entry(71, 91),
        Map.entry(87, 24),
        Map.entry(93, 73),
        Map.entry(95, 75),
        Map.entry(99, 78)
    );
    private static final boolean sixesThrowAgain = true;
    private static Random rand = new Random();

    private static int turn(int player, int square) {
        int square2 = square;
        while (true) {
            int roll = rand.nextInt(6) + 1;
            System.out.printf("Player %d, on square %d, rolls a %d", player, square2, roll);
            if (square2 + roll > 100) {
                System.out.println(" but cannot move.");
            } else {
                square2 += roll;
                System.out.printf(" and moves to square %d\n", square2);
                if (square2 == 100) return 100;
                Integer next = snl.getOrDefault(square2, square2);
                if (square2 < next) {
                    System.out.printf("Yay! Landed on a ladder. Climb up to %d.\n", next);
                    if (next == 100) return 100;
                    square2 = next;
                } else if (square2 > next) {
                    System.out.printf("Oops! Landed on a snake. Slither down to %d.\n", next);
                    square2 = next;
                }
            }
            if (roll < 6 || !sixesThrowAgain) return square2;
            System.out.println("Rolled a 6 so roll again.");
        }
    }

    public static void main(String[] args) {
        // three players starting on square one
        int[] players = {1, 1, 1};
        while (true) {
            for (int i = 0; i < players.length; ++i) {
                int ns = turn(i + 1, players[i]);
                if (ns == 100) {
                    System.out.printf("Player %d wins!\n", i + 1);
                    return;
                }
                players[i] = ns;
                System.out.println();
            }
        }
    }
}
```



## Kotlin

This includes an option for the player to automatically roll again when a six is rolled which I believe is a common variation:

```scala
// version 1.2.0

import java.util.Random

val rand = Random()

val snl = mapOf(
     4 to 14,  9 to 31, 17 to  7, 20 to 38, 28 to 84, 40 to 59, 51 to 67, 54 to 34,
    62 to 19, 63 to 81, 64 to 60, 71 to 91, 87 to 24, 93 to 73, 95 to 75, 99 to 78
)

val sixThrowsAgain = true

fun turn(player: Int, square: Int): Int {
    var square2 = square
    while (true) {
        val roll = 1 + rand.nextInt(6)
        print("Player $player, on square $square2, rolls a $roll")
        if (square2 + roll > 100) {
            println(" but cannot move.")
        }
        else {
            square2 += roll
            println(" and moves to square $square2.")
            if (square2 == 100) return 100
            val next = snl.getOrDefault(square2, square2)
            if (square2 < next) {
                println("Yay! Landed on a ladder. Climb up to $next.")
                if (next == 100) return 100
                square2 = next
            }
            else if (square2 > next) {
                println("Oops! Landed on a snake. Slither down to $next.")
                square2 = next
            }
        }
        if (roll < 6 || !sixThrowsAgain) return square2
        println("Rolled a 6 so roll again.")
    }
}

fun main(args: Array<String>) {
    // three players starting on square one
    val players = intArrayOf(1, 1, 1)
    while (true) {
        for ((i, s) in players.withIndex()) {
            val ns = turn(i + 1, s)
            if (ns == 100) {
                println("Player ${i+1} wins!")
                return
            }
            players[i] = ns
            println()
        }
    }
}
```


Sample output:

```txt

Player 1, on square 1, rolls a 1 and moves to square 2.

Player 2, on square 1, rolls a 3 and moves to square 4.
Yay! Landed on a ladder. Climb up to 14.

Player 3, on square 1, rolls a 6 and moves to square 7.
Rolled a 6 so roll again.
Player 3, on square 7, rolls a 3 and moves to square 10.

Player 1, on square 2, rolls a 5 and moves to square 7.

Player 2, on square 14, rolls a 4 and moves to square 18.

Player 3, on square 10, rolls a 5 and moves to square 15.

Player 1, on square 7, rolls a 3 and moves to square 10.

Player 2, on square 18, rolls a 1 and moves to square 19.

Player 3, on square 15, rolls a 1 and moves to square 16.

Player 1, on square 10, rolls a 2 and moves to square 12.

Player 2, on square 19, rolls a 5 and moves to square 24.

Player 3, on square 16, rolls a 2 and moves to square 18.

Player 1, on square 12, rolls a 2 and moves to square 14.

Player 2, on square 24, rolls a 6 and moves to square 30.
Rolled a 6 so roll again.
Player 2, on square 30, rolls a 2 and moves to square 32.

Player 3, on square 18, rolls a 1 and moves to square 19.

Player 1, on square 14, rolls a 6 and moves to square 20.
Yay! Landed on a ladder. Climb up to 38.
Rolled a 6 so roll again.
Player 1, on square 38, rolls a 3 and moves to square 41.

Player 2, on square 32, rolls a 4 and moves to square 36.

Player 3, on square 19, rolls a 6 and moves to square 25.
Rolled a 6 so roll again.
Player 3, on square 25, rolls a 3 and moves to square 28.
Yay! Landed on a ladder. Climb up to 84.

Player 1, on square 41, rolls a 4 and moves to square 45.

Player 2, on square 36, rolls a 6 and moves to square 42.
Rolled a 6 so roll again.
Player 2, on square 42, rolls a 3 and moves to square 45.

Player 3, on square 84, rolls a 1 and moves to square 85.

Player 1, on square 45, rolls a 1 and moves to square 46.

Player 2, on square 45, rolls a 1 and moves to square 46.

Player 3, on square 85, rolls a 5 and moves to square 90.

Player 1, on square 46, rolls a 1 and moves to square 47.

Player 2, on square 46, rolls a 5 and moves to square 51.
Yay! Landed on a ladder. Climb up to 67.

Player 3, on square 90, rolls a 4 and moves to square 94.

Player 1, on square 47, rolls a 4 and moves to square 51.
Yay! Landed on a ladder. Climb up to 67.

Player 2, on square 67, rolls a 1 and moves to square 68.

Player 3, on square 94, rolls a 4 and moves to square 98.

Player 1, on square 67, rolls a 5 and moves to square 72.

Player 2, on square 68, rolls a 3 and moves to square 71.
Yay! Landed on a ladder. Climb up to 91.

Player 3, on square 98, rolls a 3 but cannot move.

Player 1, on square 72, rolls a 1 and moves to square 73.

Player 2, on square 91, rolls a 4 and moves to square 95.
Oops! Landed on a snake. Slither down to 75.

Player 3, on square 98, rolls a 6 but cannot move.
Rolled a 6 so roll again.
Player 3, on square 98, rolls a 6 but cannot move.
Rolled a 6 so roll again.
Player 3, on square 98, rolls a 2 and moves to square 100.
Player 3 wins!

```


=={{header|Modula-2}}==

```modula2
MODULE SnakeAndLadder;
FROM FormatString IMPORT FormatString;
FROM RandomNumbers IMPORT Random;
FROM Terminal IMPORT WriteString,WriteLn,ReadChar;

CONST SIXES_THROW_AGAIN = TRUE;

PROCEDURE NextSquare(sq : CARDINAL) : CARDINAL;
BEGIN
    (* Emulate a map for the ladders and snakes *)
    IF    sq =  4 THEN RETURN 14
    ELSIF sq =  9 THEN RETURN 31
    ELSIF sq = 17 THEN RETURN  7
    ELSIF sq = 20 THEN RETURN 38
    ELSIF sq = 28 THEN RETURN 84
    ELSIF sq = 40 THEN RETURN 59
    ELSIF sq = 51 THEN RETURN 67
    ELSIF sq = 54 THEN RETURN 34
    ELSIF sq = 62 THEN RETURN 19
    ELSIF sq = 63 THEN RETURN 81
    ELSIF sq = 64 THEN RETURN 60
    ELSIF sq = 71 THEN RETURN 91
    ELSIF sq = 87 THEN RETURN 24
    ELSIF sq = 93 THEN RETURN 73
    ELSIF sq = 95 THEN RETURN 75
    ELSIF sq = 99 THEN RETURN 78
    END;

    (* No snakes or ladders encountered *)
    RETURN sq
END NextSquare;

PROCEDURE Turn(player,square : CARDINAL) : CARDINAL;
VAR
    buf : ARRAY[0..63] OF CHAR;
    roll,next : CARDINAL;
BEGIN
    LOOP
        roll := Random(1,6);
        FormatString("Player %c, on square %c, rolls a %c", buf, player, square, roll);
        WriteString(buf);
        IF square + roll > 100 THEN
            WriteString(" but cannot move.");
            WriteLn
        ELSE
            square := square + roll;
            FormatString(" and moves to square %c\n", buf, square);
            WriteString(buf);
            IF square = 100 THEN RETURN 100 END;
            next := NextSquare(square);
            IF square < next THEN
                FormatString("Yay! Landed on a ladder. Climb up to %c.\n", buf, next);
                WriteString(buf);
                IF next = 100 THEN RETURN 100 END;
                square := next
            ELSIF square > next THEN
                FormatString("Oops! Landed on a snake. Slither down to %c.\n", buf, next);
                WriteString(buf);
                IF next = 100 THEN RETURN 100 END;
                square := next
            END
        END;
        IF (roll < 6) OR NOT SIXES_THROW_AGAIN THEN RETURN square END;
        WriteString("Rolled a 6 so roll again.");
        WriteLn
    END;

    RETURN square
END Turn;

(* Main *)
VAR
    buf : ARRAY[0..31] OF CHAR;
    players : ARRAY[0..2] OF CARDINAL;
    i,ns : CARDINAL;
BEGIN
    (* Initialize *)
    FOR i:=0 TO HIGH(players) DO
        players[i] := 1
    END;

    (* Game play *)
    LOOP
        FOR i:=0 TO HIGH(players) DO
            ns := Turn(i+1, players[i]);
            IF ns = 100 THEN
                FormatString("Player %c wins!\n", buf, i+1);
                WriteString(buf);
                EXIT
            END;
            players[i] := ns;
            WriteLn
        END
    END;

    ReadChar
END SnakeAndLadder.
```



## Perl

```perl
# board layout
my %snl =( 4, 14,  9, 31, 17,  7, 20, 38, 28, 84, 40, 59, 51, 67, 54, 34,
          62, 19, 63, 81, 64, 60, 71, 91, 87, 24, 93, 73, 95, 75, 99, 78);

@players = (1, 1, 1, 1); # 4 players, starting on square 1 (last player is human)

while () {
    for $player (0..$#players) {
       $turn_count++;
       turn(\$players[$player], $player + 1, $turn_count);
    }
}

sub turn {
    my($square, $player) = @_;
    if ($player == @players) { print "You are on square $$square. Hit enter to roll the die:"; <> }
    my $roll = 1 + int rand 6;
    my $turn = $$square + $roll;
    print "Player $player on square %2d rolls a $roll", $$square;
    if ($turn > 100) {
       print " but cannot move. Next players turn.\n";
       return
    }
    if ($snl{$turn}) {
        $$square = $snl{$turn};
        if ($turn > $$square) {
           print ". Oops! Landed on a snake. Slither down to $$square.\n"
        } else {
           print ". Yay! Landed on a ladder. Climb up to $$square.\n"
        }
    } else {
        $$square = $turn;
        print " and moves to square $$square\n";
    }
    if ($$square == 100) {print "Player $player wins after $turn_count turns.\n"; exit }
    return
}
```

```txt
Player 1 on square  1 rolls a 3. Yay! Landed on a ladder. Climb up to 14.
Player 2 on square  1 rolls a 1 and moves to square 2
Player 3 on square  1 rolls a 5 and moves to square 6
Player 4 on square  1 rolls a 4 and moves to square 5
Player 1 on square 14 rolls a 6. Yay! Landed on a ladder. Climb up to 38.
Player 2 on square  2 rolls a 2. Yay! Landed on a ladder. Climb up to 14.
Player 3 on square  6 rolls a 4 and moves to square 10
Player 4 on square  5 rolls a 3 and moves to square 8
... 703 turns omitted ...
Player 1 on square 83 rolls a 1 and moves to square 84
Player 2 on square 98 rolls a 2 and moves to square 100
Player 2 wins after 706 turns.
```



## Perl 6

Snakes and ladders is entirely chance based, so human interaction is not really required. This version allows up to one human player against any number of computer players and asks for input... but doesn't really ''need'' or even ''use'' it. I didn't bother to implement a graphical interface.


```perl6
        # board layout
my %snl =  4, 14,  9, 31, 17,  7, 20, 38, 28, 84, 40, 59, 51, 67, 54, 34,
          62, 19, 63, 81, 64, 60, 71, 91, 87, 24, 93, 73, 95, 75, 99, 78;

my @players = 1, 1, 1; # three players, starting on square 1
my $human = 1;         # player 1 is human. set to 0 for all computer players

loop {
    for ^@players -> $player {
        turn(@players[$player], $player + 1);
    }
    say '';
}

sub turn ($square is rw, $player) {
    if $player == $human {
        prompt "You are on square $square. Hit enter to roll the die.";
    }
    my $roll = (1..6).roll;
    my $turn = $square + $roll;
    printf "Player $player on square %2d rolls a $roll", $square;
    if $turn > 100 {
        say " but cannot move. Next players turn.";
        return $square;
    }
    if %snl{$turn} {
        $square = %snl{$turn};
        if $turn > $square {
            say ". Oops! Landed on a snake. Slither down to $square."
        } else {
            say ". Yay! Landed on a ladder. Climb up to $square."
        }
    } else {
        $square = $turn;
        say " and moves to square $square";
    }
    say "Player $player wins!" and exit if $square == 100;
    return $square;
}
```

```txt
You are on square 1. Hit enter to roll the die.
Player 1 on square  1 rolls a 1 and moves to square 2
Player 2 on square  1 rolls a 4 and moves to square 5
Player 3 on square  1 rolls a 1 and moves to square 2

You are on square 2. Hit enter to roll the die.
Player 1 on square  2 rolls a 1 and moves to square 3
Player 2 on square  5 rolls a 2 and moves to square 7
Player 3 on square  2 rolls a 3 and moves to square 5

You are on square 3. Hit enter to roll the die.
Player 1 on square  3 rolls a 4 and moves to square 7
Player 2 on square  7 rolls a 1 and moves to square 8
Player 3 on square  5 rolls a 2 and moves to square 7

You are on square 7. Hit enter to roll the die.
Player 1 on square  7 rolls a 2. Yay! Landed on a ladder. Climb up to 31.
Player 2 on square  8 rolls a 3 and moves to square 11
Player 3 on square  7 rolls a 2. Yay! Landed on a ladder. Climb up to 31.

  ... about 15 turns omitted ...

You are on square 90. Hit enter to roll the die.
Player 1 on square 90 rolls a 3. Oops! Landed on a snake. Slither down to 73.
Player 2 on square 55 rolls a 3 and moves to square 58
Player 3 on square 90 rolls a 4 and moves to square 94

You are on square 73. Hit enter to roll the die.
Player 1 on square 73 rolls a 6 and moves to square 79
Player 2 on square 58 rolls a 5. Yay! Landed on a ladder. Climb up to 81.
Player 3 on square 94 rolls a 3 and moves to square 97

You are on square 79. Hit enter to roll the die.
Player 1 on square 79 rolls a 3 and moves to square 82
Player 2 on square 81 rolls a 6. Oops! Landed on a snake. Slither down to 24.
Player 3 on square 97 rolls a 5 but cannot move. Next players turn.

You are on square 82. Hit enter to roll the die.
Player 1 on square 82 rolls a 5. Oops! Landed on a snake. Slither down to 24.
Player 2 on square 24 rolls a 6 and moves to square 30
Player 3 on square 97 rolls a 4 but cannot move. Next players turn.

You are on square 24. Hit enter to roll the die.
Player 1 on square 24 rolls a 4. Yay! Landed on a ladder. Climb up to 84.
Player 2 on square 30 rolls a 5 and moves to square 35
Player 3 on square 97 rolls a 4 but cannot move. Next players turn.

You are on square 84. Hit enter to roll the die.
Player 1 on square 84 rolls a 1 and moves to square 85
Player 2 on square 35 rolls a 2 and moves to square 37
Player 3 on square 97 rolls a 3 and moves to square 100
Player 3 wins!
```



## Phix

```Phix
constant sixesThrowAgain = true

constant snl = new_dict({{4,14},{9,31},{17,7},{20,38},{28,84},{40,59},{51,67},{54,34},
                        {62,19},{63,81},{64,60},{71,91},{87,24},{93,73},{95,75},{99,78}})

constant msgs = {". Oops! Landed on a snake. Slither down to %d.\n", -- (next<square)
                 " and moves to square %d\n",                        -- (next=square)
                 ". Yay! Landed on a ladder. Climb up to %d.\n"}     -- (next>square)

function Turn(integer player, square)
    while true do
        integer roll = rand(6)
        printf(1,"Player %d, on square %d, rolls a %d", {player, square, roll})
        if square+roll>100 then
            puts(1," but cannot move.\n")
        else
            square += roll
            integer next = getd(square,snl)
            if next=0 then next=square end if
            printf(1,msgs[compare(next,square)+2],next)
            square = next
            if square==100 then exit end if
        end if
        if roll<6 or not sixesThrowAgain then exit end if
        puts(1,"Rolled a 6 so roll again.\n")
    end while
    return square
end function

procedure main()
    sequence players = {1,1,1}  -- three players starting on square one
    while true do
        for i=1 to length(players) do
            players[i] = Turn(i, players[i])
            if players[i]==100 then
                printf(1,"Player %d wins!\n",i)
                return
            end if
        end for
    end while
end procedure
main()
```

```txt

Player 1, on square 1, rolls a 2 and moves to square 3
Player 2, on square 1, rolls a 1 and moves to square 2
Player 3, on square 1, rolls a 3. Yay! Landed on a ladder. Climb up to 14.
...
Player 3, on square 89, rolls a 5 and moves to square 94
Player 1, on square 90, rolls a 3. Oops! Landed on a snake. Slither down to 73.
Player 2, on square 97, rolls a 3 and moves to square 100
Player 2 wins!

```



## Python


```python
import random
import sys

snl = {
    4: 14,
    9: 31,
    17: 7,
    20: 38,
    28: 84,
    40: 59,
    51: 67,
    54: 34,
    62: 19,
    63: 81,
    64: 60,
    71: 91,
    87: 24,
    93: 73,
    95: 75,
    99: 78
}
sixesRollAgain = True

def turn(player, square):
    while True:
        roll = random.randint(1,6)
        sys.stdout.write("Player {0} on square {1}, rolls a {2}".format(player, square, roll))
        if square + roll > 100:
            print " but cannot move."
        else:
            square += roll
            print " and moves to square {0}".format(square)
            if square == 100:
                return 100
            next = snl.get(square, square)
            if square < next:
                print "Yay! landed on a ladder. Climb up to {0}.".format(next)
                if square == 100:
                    return 100
                square = next
            elif square > next:
                print "Oops! Landed on a snake. Slither down to {0}.".format(next)
                square = next
        if roll < 6 or not sixesRollAgain:
            return square
        print "Rolled a 6 so roll again."

def main():
    players = [1, 1, 1]
    while True:
        for i in range(0, 3):
            ns = turn(i+1, players[i])
            if ns == 100:
                print "Player {0} wins!".format(i+1)
                return
            players[i] = ns;
            print

main()
```



## Racket


```racket
#lang racket/base

(define portals (hash  4 14  9 31  17 7  20 38  28 84  40 59  51 67  54 34  62 19  63 81  64 60  71 91  87 24  93 73  95 75  99 78))

(define (find-game-winner n-players reroll-on-six? (win values))
  (let turn ((positions-map (for/hash ((p n-players)) (values p 1))) (player 0))
    (newline)
    (let ((die (add1 (random 6)))
          (position (hash-ref positions-map player)))
      (printf "player ~a at ~a rolls ~a: " (add1 player) position die)
      (let* ((try-position (+ position die))
             (new-position
              (cond [(> try-position 100) (display "player can't move beyond the end.") position]
                    [(= try-position 100) (display "player wins!") try-position]
                    [(hash-ref portals try-position #f)
                     => (λ (slide-to)
                          (printf "~a from ~a to ~a." (if (> slide-to try-position) "LADDER" "SNAKE") try-position slide-to)
                          slide-to)]
                    [else (printf "landed on ~a." try-position) try-position])))
          (if (= new-position 100)
              (win (add1 player))
              (turn (hash-set positions-map player new-position)
                    (if (and (= 6 die) reroll-on-six?)
                        (begin0 player (display " [6] rolls again!"))
                        (modulo (add1 player) n-players))))))))

(module+ main
  (find-game-winner 5 #t (λ (p) (printf "~%~%The winner of the game is player #~a" p))))
```


```txt

player 1 at 1 rolls 2: landed on 3.
player 2 at 1 rolls 3: LADDER from 4 to 14.
player 3 at 1 rolls 6: landed on 7. [6] rolls again!
player 3 at 7 rolls 4: landed on 11.
player 4 at 1 rolls 3: LADDER from 4 to 14.
player 5 at 1 rolls 5: landed on 6.
player 1 at 3 rolls 1: LADDER from 4 to 14.
player 2 at 14 rolls 3: SNAKE from 17 to 7.
player 3 at 11 rolls 4: landed on 15.
...
player 3 at 86 rolls 5: landed on 91.
player 4 at 85 rolls 1: landed on 86.
player 5 at 68 rolls 6: landed on 74. [6] rolls again!
player 5 at 74 rolls 1: landed on 75.
player 1 at 59 rolls 1: landed on 60.
player 2 at 94 rolls 6: player wins!

The winner of the game is player #2
```



## REXX

<!--     for Regina REXX,  use a seed of    8    to produce the game shown below.     !-->

```rexx
/*REXX program plays  "Snakes and Ladders"  game for any number of players (default 3). */
parse arg np seed .                              /*obtain optional arguments from the CL*/
if np=='' | np==","  then np= 3                  /*Not specified?  Then use the default.*/
if datatype(seed, 'W')  then call random ,,seed  /*maybe use a seed for the  RANDOM BIF.*/
pad= left('',7)                                  /*variable value used for indenting SAY*/
                    do k=1  for 100;    @.k= k   /*assign default values for board cells*/
                    end   /*k*/                  /* [↑]  number when landing on a square*/
                                                 /* [↓]  define ladder destinations.    */
@.4=14;  @.9=31;    @.20=38;  @.28=84;  @.40=59;   @.51=67;   @.63=81;   @.71=91;  @.95=75
@.17=7;  @.54=34;   @.62=19;  @.64=60;  @.87=24;   @.93=73;   @.99=78
                                                 /* [↑]  define snake  destinations.    */
player.= 1                                       /*start all players on the 1st square. */
          do games=1  until $==100;           say center(' turn '   games" ",  75,  "─")
              do j=1  for np  until $==100;   $= turn(j, player.j);            player.j= $
              end   /*j*/                        /*process each of the number of players*/
          end       /*games*/                    /*exit both loops when there's a winner*/
say pad  'Player '       j       " wins!"        /*announce the winner; the game is over*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
turn: parse arg P, square;     ?= random(1, 6)   /*simulate a roll of a six─sided die.  */
      @does= pad  'Player '    P    " on square "    right(square, 2)     ' rolls a '    ?
      if square+?>100  then do;  say @does   " but can't move.  Next player's turn."
                                 return square
                            end
                       else do;  square= square + ?                     /*move a player.*/
                                 say @does   " and moves to square"     right(square, 3)
                            end
      next= @.square                                                    /*where moved to*/
      @oops=   pad  pad  'Oops!  Landed on a snake,  slither down to'   right(next, 3)
      @ladder= pad  pad   'Yay!  Landed on a ladder,  climb up to'      right(next, 3)
      if square<next  then                       say right(@ladder, 69)
                      else  if square>next then  say right(@oops  , 69)
      return next
```

```txt

───────────────────────────────── turn  1 ─────────────────────────────────
        Player  1  on square   1  rolls a  5  and moves to square   6
        Player  2  on square   1  rolls a  3  and moves to square   4
                           Yay!  Landed on a ladder,  climb up to  14
        Player  3  on square   1  rolls a  5  and moves to square   6
───────────────────────────────── turn  2 ─────────────────────────────────
        Player  1  on square   6  rolls a  5  and moves to square  11
        Player  2  on square  14  rolls a  4  and moves to square  18
        Player  3  on square   6  rolls a  3  and moves to square   9
                           Yay!  Landed on a ladder,  climb up to  31
───────────────────────────────── turn  3 ─────────────────────────────────
        Player  1  on square  11  rolls a  6  and moves to square  17
                       Oops!  Landed on a snake,  slither down to   7
        Player  2  on square  18  rolls a  3  and moves to square  21
        Player  3  on square  31  rolls a  2  and moves to square  33
───────────────────────────────── turn  4 ─────────────────────────────────
        Player  1  on square   7  rolls a  5  and moves to square  12
        Player  2  on square  21  rolls a  4  and moves to square  25
        Player  3  on square  33  rolls a  3  and moves to square  36
───────────────────────────────── turn  5 ─────────────────────────────────
        Player  1  on square  12  rolls a  4  and moves to square  16
        Player  2  on square  25  rolls a  3  and moves to square  28
                           Yay!  Landed on a ladder,  climb up to  84
        Player  3  on square  36  rolls a  6  and moves to square  42
───────────────────────────────── turn  6 ─────────────────────────────────
        Player  1  on square  16  rolls a  6  and moves to square  22
        Player  2  on square  84  rolls a  1  and moves to square  85
        Player  3  on square  42  rolls a  3  and moves to square  45
───────────────────────────────── turn  7 ─────────────────────────────────
        Player  1  on square  22  rolls a  6  and moves to square  28
                           Yay!  Landed on a ladder,  climb up to  84
        Player  2  on square  85  rolls a  3  and moves to square  88
        Player  3  on square  45  rolls a  4  and moves to square  49
───────────────────────────────── turn  8 ─────────────────────────────────
        Player  1  on square  84  rolls a  4  and moves to square  88
        Player  2  on square  88  rolls a  6  and moves to square  94
        Player  3  on square  49  rolls a  1  and moves to square  50
───────────────────────────────── turn  9 ─────────────────────────────────
        Player  1  on square  88  rolls a  3  and moves to square  91
        Player  2  on square  94  rolls a  4  and moves to square  98
        Player  3  on square  50  rolls a  4  and moves to square  54
                       Oops!  Landed on a snake,  slither down to  34
──────────────────────────────── turn  10 ─────────────────────────────────
        Player  1  on square  91  rolls a  6  and moves to square  97
        Player  2  on square  98  rolls a  2  and moves to square 100
        Player  2  wins!

```



## Ruby


```ruby

NONE = 0; LADDER = 1; SNAKE = 2; STAY = 1; MOVE = 2; WIN = 3
class Cell
    @type; @to; attr_reader :type, :to
    def initialize; @type = NONE; @to = 0; end
    def set( t, o ); @type = t; @to = o; end
end
class Player
    @pos; @name; attr_accessor :pos; attr_reader :name
    def initialize( n ); @pos = 0; @name = n; end
    def play( dice )
        s = dice.roll; return s, STAY if @pos + s > 99
        @pos += s; return s, WIN if @pos == 99
        return s, MOVE
    end
end
class Die
    @sides; def initialize( s = 6 ); @sides = s; end
    def roll; return 1 + rand( @sides ); end
end
def initBoard
    @board = Array.new( 100 ); for i in 0 .. 99; @board[i] = Cell.new(); end
    @board[3].set( LADDER, 13 ); @board[8].set( LADDER, 30 ); @board[19].set( LADDER, 37 );
    @board[27].set( LADDER, 83 );@board[39].set( LADDER, 58 ); @board[50].set( LADDER, 66 );
    @board[62].set( LADDER, 80 ); @board[70].set( LADDER, 90 ); @board[16].set( SNAKE, 6 );
    @board[61].set( SNAKE, 18 ); @board[86].set( SNAKE, 23 ); @board[53].set( SNAKE, 33 );
    @board[63].set( SNAKE, 59 ); @board[92].set( SNAKE, 72 ); @board[94].set( SNAKE, 74 );
    @board[98].set( SNAKE, 77 );
end
def initPlayers
    @players = Array.new( 4 );
    for i in 0 .. @playersCount - 1; @players[i] = Player.new( "player " << i + 49 ); end
end
def play
    initBoard; initPlayers; @die = Die.new
    while true
        for p in 0 .. @playersCount - 1
            puts; puts
            if( 0 == p )
                print "#{@players[p].name}, your turn. Your position is cell #{@players[p].pos + 1}.\n"<<
                "Press [RETURN] to roll the die."
                gets; np = @players[p].play( @die ); print "You rolled a #{np[0]}\n"
                if np[1] == WIN
                    print "You reached position #{@players[p].pos + 1} and win the game!!!!\n"; return
                elsif np[1] == STAY; print "Sorry, you cannot move!\n"
                else print "Your new position is cell #{@players[p].pos + 1}.\n";
                end
            else
                np = @players[p].play( @die ); print "#{@players[p].name} rolled a #{np[0]}.\n"
                if np[1] == WIN
                     print "He reached position #{@players[p].pos + 1} and wins the game!!!!\n"; return
                elsif np[1] == STAY; print "But he cannot move....\n"
                else print "His new position is cell #{@players[p].pos + 1}.\n";
                end
            end
            s = @board[@players[p].pos].type
            next if s == NONE
            @players[p].pos = @board[@players[p].pos].to
            case s
                when SNAKE; print "What a pitty, landed on a snake. "
                when LADDER; print "Lucky move! Landed on a ladder. "
            end
            print "New position is cell #{@players[p].pos + 1}.\n"
        end
    end
end
@playersCount = 4; @board; @players; @die
play

```

```txt

...
player 1, your turn. Your position is cell 97.
Press [RETURN] to roll the die.
You rolled a 5
Sorry, you cannot move!

player 2 rolled a 5.
His new position is cell 49.

player 3 rolled a 4.
But he cannot move....

player 4 rolled a 6.
His new position is cell 91.

player 1, your turn. Your position is cell 97.
Press [RETURN] to roll the die.
You rolled a 3
You reached position 100 and win the game!!!!

```



## Visual Basic .NET

```vbnet
Module Module1

    ReadOnly SNL As New Dictionary(Of Integer, Integer) From {
        {4, 14},
        {9, 31},
        {17, 7},
        {20, 38},
        {28, 84},
        {40, 59},
        {51, 67},
        {54, 34},
        {62, 19},
        {63, 81},
        {64, 60},
        {71, 91},
        {87, 24},
        {93, 73},
        {95, 75},
        {99, 78}
    }
    ReadOnly rand As New Random
    Const sixesThrowAgain = True

    Function Turn(player As Integer, square As Integer) As Integer
        Do
            Dim roll = rand.Next(1, 6)
            Console.Write("Player {0}, on square {1}, rolls a {2}", player, square, roll)
            If square + roll > 100 Then
                Console.WriteLine(" but cannot move.")
            Else
                square += roll
                Console.WriteLine(" and moves to square {0}", square)
                If square = 100 Then
                    Return 100
                End If

                Dim nxt = square
                If SNL.ContainsKey(square) Then
                    nxt = SNL(nxt)
                End If
                If square < nxt Then
                    Console.WriteLine("Yay! Landed on a ladder. Climb up to {0}.", nxt)
                    If nxt = 100 Then
                        Return 100
                    End If
                    square = nxt
                ElseIf square > nxt Then
                    Console.WriteLine("Oops! Landed on a snake. Slither down to {0}.", nxt)
                    square = nxt
                End If
            End If

            If roll < 6 OrElse Not sixesThrowAgain Then
                Return square
            End If
            Console.WriteLine("Rolled a 6 so roll again.")
        Loop
    End Function

    Sub Main()
        Dim players = {1, 1, 1}
        Do
            For i = 1 To players.Length
                Dim ns = Turn(i, players(i - 1))
                If ns = 100 Then
                    Console.WriteLine("Player {0} wins!", i)
                    Return
                End If
                players(i - 1) = ns
                Console.WriteLine()
            Next
        Loop
    End Sub

End Module
```

