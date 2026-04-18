+++
title = "2048"
description = ""
date = 2019-09-14T21:53:07Z
aliases = []
[extra]
id = 17706
task = """
  Implement a 2D sliding block puzzle game
  where blocks with numbers are combined to add their values.
"""
[taxonomies]
categories = ["task"]
languages = [
  "ada",
  "algol_68",
  "autohotkey",
  "batch_file",
  "bbc_basic",
  "c",
  "cpp",
  "csharp",
  "clojure",
  "common_lisp",
  "d",
  "elixir",
  "elm",
  "factor",
  "forth",
  "fortran",
  "go",
  "haskell",
  "j",
  "java",
  "javascript",
  "julia",
  "kotlin",
  "m2000_interpreter",
  "maple",
  "matlab",
  "nim",
  "ocaml",
  "perl_6",
  "phix",
  "php",
  "picolisp",
  "pony",
  "prolog",
  "python",
  "qb64",
  "r",
  "racket",
  "rexx",
  "ring",
  "ruby",
  "rust",
  "scala",
  "seed7",
  "tcl",
  "xpl0",
]
tags = ["puzzle", "game"]
+++


## Task

The rules are that on each turn the player must choose a direction (up, down, left or right) and all tiles move as far as possible in that direction, some more than others.
Two adjacent tiles (in that direction only) with matching numbers combine into one bearing the sum of those numbers.
A move is valid when at least one tile can be moved, if only by combination.
A new tile with the value of 2 is spawned at the end of each turn at a randomly chosen empty square, if there is one.
To win the player must create a tile with the number 2048. The player loses if no valid moves are possible.

The name comes from the popular open-source implementation
of this game mechanic:
<https://gabrielecirulli.github.io/2048/>.

Requirements:

- "Non-greedy" movement.
    The tiles that were created by combining other tiles should not be combined again during the same turn (move).
    That is to say that moving the tile row of
    `[2][2][2][2]`
    to the right should result in
    `......[4][4]`
    and not
    `.........[8]`
- "Move direction priority".
    If more than one variant of combining is possible, move direction shows one that will take effect.
    For example, moving the tile row of
    `...[2][2][2]`
    to the right should result in
    `......[2][4]`
    and not
    `......[4][2]`
- Adding a new tile on a blank space.
    Most of the time new "2" is to be added and occasionally (10% of the time) - "4"
- Check for valid moves.
    The player shouldn't be able to skip their turn by trying a move that doesn't change the board.
- Win condition.
- Lose condition.


## Ada

Works with GNAT


{{ code(src="content/tasks/2048/ada.adb", lang="Ada") }}


Output:

```txt
+----+----+----+----+
|  2 | 16 |  2 |  2 |
+----+----+----+----+
| 64 |    |    |    |
+----+----+----+----+
|  4 |    |    |    |
+----+----+----+----+
|    |    |    |    |
+----+----+----+----+
Score = 184
```



## ALGOL 68



{{ code(src="content/tasks/2048/algol_68.a68", lang="algol68") }}




## AutoHotkey



{{ code(src="content/tasks/2048/autohotkey.ahk", lang="AutoHotkey") }}




## Batch File



{{ code(src="content/tasks/2048/batch_file.bat", lang="dos") }}


Output:

```txt
2048 Game in Batch

+----+----+----+----+
|    |  +2|    |    |
+----+----+----+----+
|   4|    |    |    |
+----+----+----+----+
|   4|    |    |    |
+----+----+----+----+
|  16|   4|    |   2|
+----+----+----+----+

Score: 60

Keys: WASD (Slide Movement), N (New game), P (Exit)
```



## BBC BASIC

Works with BBC BASIC for Windows


{{ code(src="content/tasks/2048/bbc_basic.bas", lang="bbcbasic") }}


Output:

```txt
    _    _    _    _
    _    _    _    _
    _    _    2    _
    _    _    _    _
--------------------
    _    _    _    _
    _    _    _    _
    2    _    _    _
    _    2    _    _
--------------------
    2    2    _    _
    _    _    2    _
    _    _    _    _
    _    _    _    _
--------------------
    4    2    _    _
    2    _    _    _
    _    _    _    _
    _    _    _    _
--------------------
.
.
.
.
    2    8    4    2
    4    2   16    4
   16    4    8   32
    4   32    2    4
--------------------
You lost :-(
```



## C


### Version 1

Supports limited colours through vt100 escape codes. Requires a posix machine for <tt>termios.h</tt> and <tt>unistd.h</tt> headers. Provides simplistic animations when moving and merging blocks.


{{ code(src="content/tasks/2048/c_1.c", lang="c") }}


Output:

```txt

Score: 1100 (+4)
-------------------------
|  64 |  32 |  64 |  32 |
|  32 |  16 |   2 |   8 |
|  16 |   4 |   8 |   4 |
|   4 |   2 |   4 |   2 |
-------------------------
You lose!

```




### Version 2





{{ code(src="content/tasks/2048/c_2.c", lang="c") }}



## C#

Translated from C++.


{{ code(src="content/tasks/2048/csharp.cs", lang="c#") }}


Output:

```txt

Score: 572

+------+------+------+------+
|      | 2    | 16   | 4    |
+------+------+------+------+
|      | 2    | 4    | 64   |
+------+------+------+------+
| 4    | 16   | 32   | 4    |
+------+------+------+------+
| 2    | 4    | 2    | 16   |
+------+------+------+------+


(W) Up (S) Down (A) Left (D) Right

```



## C++



{{ code(src="content/tasks/2048/cpp.cpp", lang="cpp") }}


Output:

```txt

SCORE: 2024

+------+------+------+------+
|    2 |    8 |   32 |  256 |
+------+------+------+------+
|      |      |    4 |   32 |
+------+------+------+------+
|      |      |    2 |    8 |
+------+------+------+------+
|      |      |      |    2 |
+------+------+------+------+

(W)Up (S)Down (A)Left (D)Right

```



## Clojure



{{ code(src="content/tasks/2048/clojure.clj", lang="clojure") }}



Output:

```txt
+----+----+----+----+
|    |   2|    |    |
+----+----+----+----+
|   2|    |    |    |
+----+----+----+----+
|   4|    |    |    |
+----+----+----+----+
|  16|   2|    |    |
+----+----+----+----+
```



## Common Lisp

Depends on Windows msvcrt.dll for `_getch`.
Depends on quicklisp.
Use arrow keys to make moves and press "Q" to quit.
Tested with SBCL.


{{ code(src="content/tasks/2048/common_lisp.lisp", lang="lisp") }}


Output:

```txt
* (2048-lisp::prompt)

   Score: 0
+------+------+------+------+
|      |      |      |      |
+------+------+------+------+
|      |      |      |      |
+------+------+------+------+
|      |      |    2 |      |
+------+------+------+------+
|      |      |      |      |
+------+------+------+------+
```

Some time later...

```txt
   Score: 336
+------+------+------+------+
|      |    4 |   16 |   32 |
+------+------+------+------+
|      |      |    4 |   16 |
+------+------+------+------+
|      |      |   32 |      |
+------+------+------+------+
|      |    2 |      |      |
+------+------+------+------+
```



## D


{{ code(src="content/tasks/2048/d.d", lang="d") }}


The output is the same as the C++ version.


## Elixir

Works with Elixir|1.3


{{ code(src="content/tasks/2048/elixir.exs", lang="elixir") }}



Output:

```txt

+----+----+----+----+
|    |   2|    |    |
+----+----+----+----+
|    |    |    |    |
+----+----+----+----+
|   2|    |    |    |
+----+----+----+----+
|    |    |    |    |
+----+----+----+----+
key in wasd or q: s
.
.
.
+----+----+----+----+
|   2|   4|   2|    |
+----+----+----+----+
|  16|    |    |    |
+----+----+----+----+
|   8|  16|  32|   2|
+----+----+----+----+
|  64| 256| 128|   4|
+----+----+----+----+
key in wasd or q: q

```



## Elm

Works with Elm 0.18.0
Try online <https://ellie-app.com/3ZMMpYsbfcMa1/3>


{{ code(src="content/tasks/2048/elm.elm", lang="Elm") }}




## Factor


Can be loaded and run as a module by copying the code to a file and executing "factor 2048.factor".

For every step prints an ASCII representation of the board on the console. Controlled by feeding the program lines with a single character:

* W - up
* S - down
* A - left
* D - right
* Q - exit the game



{{ code(src="content/tasks/2048/factor.factor", lang="factor") }}




## Forth

Works with gforth|0.7.3
Just like my implementation of [[15 Puzzle Game#Forth|15 Puzzle Game]], this uses Vim's h/j/k/l for movement.



{{ code(src="content/tasks/2048/forth.4th", lang="forth") }}



Output:

```txt
Score: 20136 (+2048)
-------------------------
|     |     |     |     |
|   2 |     |     |     |
|     |   8 |   2 |     |
|2048 |   4 |   8 |   2 |
-------------------------
You win!
```



## Fortran


### The Plan

The primary objective was to achieve the processing ''without'' generating similar code for each of the four different move directions or alternatively for the two lots of related directions - left/right and up/down. The various directions each involve variations on a loop of the form <code>DO L = 1,N</code> and this can easily be generalised as <code>DO L = ''first'',''last'',''increment''</code> with a set of suitable values for each direction. Although Fortran encompasses complex arithmetic so that one could play about with vector arithmetic (notably, multiplying by (0,1) rotates by ninety degrees counterclockwise), alas, this is not provided for integer type variables, and in any case, the (x,y) orientation of Cartesian coordinates is not the same as the (row,column) orientation usual for arrays and character-style output, so to reduce mental strain complex arithmetic is not attempted and screen layout rules. However, an echo remains in that the directions are listed in the (x,y) style counter-clockwise: right, up, left, down.

Further thought shows that a move in a selected direction also involves a direction at right angles. To reduce vague generality, suppose the move direction is "right". All squares in a row are to be shoved rightwards, and this is to be repeated for each row: a series perpendicular to the move direction. Indeed, since rows do not interact in this move each row could be processed in parallel, but an ordinary sequential loop will do. It could run in any order so only two sorts of directions need be handled, but to reduce the mental strain, all four are distinct. Thus, there is a two-level process: the outer loop steps through the collection of rows, and the inner loop deals with the movement in each row. The outer loop is controlled by arrays <code>RC1, RCn, RCi</code> for ''first'', ''last'', ''increment'' to step along the rows (or columns): RC. And for the inner loop perpendicular to that so CR for column (or row) there are arrays <code>CR1, CRn, CRi</code> - all this is intended to be read as <code>DO L = 1,N</code> but with extra verbiage because the loop might be <code>DO L = N,1,-1</code> instead.

Holding firmly to the one-dimensional aspect of the row's processing, the actual processing can be seen to be simple. For instance, step along an array comparing each element to its predecessor, as in A(I) and A(I - 1), or, (avoiding index arithmetic) maintain two indices: CI and PI for current and previous index. Start CI at element one, and run the loop as <code>DO L = 2,N</code> with on each iteration PI taking the value of CI, and CI being calculated afresh. Except that the loop has verbiage: DO L = ''(first'' + ''increment)'',''last'',''increment''.

But in fact, the board is represented as a two dimensional array. Fortran does not offer a special "index" type of variable so that if ''this'' was a two-element entity with the value (1,2), <code>A(this)</code> would be equivalent to <code>A(1,2)</code> One must write out the indices, as in <code>A(this(1),this(2))</code>  On the other hand, F90 introduced array arithmetic and related statements, so one can declare CIJ to be a two-element array, and introduce array arithmetic similar to complex number arithmetic to juggle indices. Further, instead of using simple variables and IF-statements or the like to select amongst the directions, this is done by using array WAY, and its associate YAW to obtain a perpendicular direction. That is, for direction W, WAY(W) selects either (0,1) or (1,0) so that RC * WAY(W) switches the value of RC between the first or second dimension, and YAW is the other way around.

Except that WAY and YAW are ''two'' dimensional arrays (rather than a one-dimensional array of complex number pairs, alas) so that the expression is in fact <code>RC * WAY(W,1:2)</code> and the calculations for ''both'' indices are done  together. Because Fortran uses the "column-major" ordering of elements in storage, successive elements of a multi-dimensional array have the leftmost index varying most rapidly so that the order is WAY(1,1), WAY(2,1), WAY(3,1), WAY(4,1), WAY(1,2), ''etc'' and statements such as DATA or PARAMETER whereby values can be listed employ that ordering. So that the list of values for WAY and YAW can be aligned in the source code with the similar lists for the arrays specifying the loop parameters for each direction, the ordering is WAY(4,2) rather than WAY(2,4) even though this means that the two values for a given direction are not in adjacent storage, unlike the two parts of a complex number.

Arrays WAY and YAW are reminiscent of "truth tables" in Boolean logic, and it is tempting to imagine that YAW = ¬WAY, but alas, a NOT operation applied to an integer variable will flip not just the lowest bit. Trying a .NOT. operation on LOGICAL variables instead will work as desired, except that their integer interpretations may not be as hoped for. Yet another ploy might be based on W being even/odd or odd/even, and similar trickery might be applied to the other arrays of constants, but, enough. The devious juggling of arrays is traditional in Fortran.


### Source

The initial attempt at showing the board relied rather heavily on FORMAT tricks, in particular the use of the <''n''> facility whereby the value of an integer expression can be inserted into a format statement's coding on-the-fly, as in the following.


{{ code(src="content/tasks/2048/fortran_1.f90", lang="Fortran") }}


This sort of thing is not necessarily accepted by all compilers, so instead the next stage was to convert to using complicated WRITE statements. If one regarded the various sizes (the values of NR, NC, W in the source) as truly fixed, literal constants could be used throughout. This would however mean that they would appear without explanation, and if one eventually attempted to recode with different values, mistakes would be likely. Thus below, FORMAT 3 has <code> (<NC>(A1,I<W>),A1)</code> and if the <> scheme were unavailable, you'd have to use <code>(4(A1,I6),A1)</code> instead, not too troublesome a change. Or, the text of the format sequence could be written to a CHARACTER variable, as demonstrated in [[Multiplication_tables#Traditional_approach]]. Yet another approach might be <code>(666(A1,I6))</code> which relies on the addendum <code>A1</code> happening to be the same as the start of the <code>(A1,I6)</code> pair, but there is still the appearance of the literal constant six instead of <W>, and if there were to be any change to be made, it would have to be remembered...


{{ code(src="content/tasks/2048/fortran_2.f90", lang="Fortran") }}




### Output

As usual, the aspect ratio of the display here differs from the "console"-type display on the computer monitor, so the square is rather oblong, and the vertical bars do not join. Rather to my surprise the special characters for the "corner" and crossing glyphs do display correctly. If the console display is copied to a text editor (UltraEdit in my case) they are translated to + signs for the crossing and corners! Further confusion is provided by any attempt to type in the character codes (ALT-218, ''etc.'') as some (but not all) codes are translated by UltraEdit or the keyboard interface into other character codes. All-in-all, it is simpler to employ <code>CHAR(218)</code> in the source as plain text with no fiddling.

Input is a bit annoying, as Fortran doesn't offer an interface to the asynchronous keyboard routines (such as KeyPressed and ReadKey in Turbo Pascal, ''etc.'') and the arrow keys are pre-empted for editing the input being typed, notably the up-arrow key recovers the text of the previous line typed. So, one must press an ordinary key and then signify the completion of your input by pressing the "enter" key. Other keys could be allowed, such as SWAZ or KIJM and the like (or UPEJ for a Dvorak keyboard) for "right", "up", "left" and "down", but you would still have to press the enter key as well.

```txt

To play '2048' with 4 rows and 4 columns.
On each move, choose a direction (Up, Down, Left, Right)
by typing the single letter U, D, L, R, or, a space to quit.
All squares will be pushed as far as possible that way.
Those meeting with the same number will form one square
with the sum of the numbers, and one becomes blank.
After each move, a random blank square becomes 2

An integer to start the 'random' number generator: 12345
┌──────┬──────┬──────┬──────┐
│      │      │      │      │
│      │   2  │      │      │
│      │      │      │      │
├──────┼──────┼──────┼──────┤
│      │      │      │      │
│      │      │      │      │
│      │      │      │      │
├──────┼──────┼──────┼──────┤
│      │      │      │      │
│      │      │      │      │
│      │      │      │      │
├──────┼──────┼──────┼──────┤
│      │      │      │      │
│      │      │   2  │      │
│      │      │      │      │
└──────┴──────┴──────┴──────┘
Move   1, score 4. Moves RULD ... Your move: d
┌──────┬──────┬──────┬──────┐
│      │      │      │      │
│      │      │      │      │
│      │      │      │      │
├──────┼──────┼──────┼──────┤
│      │      │      │      │
│      │      │      │      │
│      │      │      │      │
├──────┼──────┼──────┼──────┤
│      │      │      │      │
│      │      │   2  │      │
│      │      │      │      │
├──────┼──────┼──────┼──────┤
│      │      │      │      │
│      │   2  │   2  │      │
│      │      │      │      │
└──────┴──────┴──────┴──────┘
Move   2, score 6. Moves RULD ... Your move:

```



## Go




{{ code(src="content/tasks/2048/go.go", lang="Go") }}




## Haskell



{{ code(src="content/tasks/2048/haskell.hs", lang="haskell") }}




## J


{{ code(src="content/tasks/2048/j_1.ijs", lang="j") }}


'''Usage'''


{{ code(src="content/tasks/2048/j_2.ijs", lang="j") }}




## Java

[[File:game_2048_java2.png|300px|thumb|right]]
Works with Java|8


{{ code(src="content/tasks/2048/java.java", lang="java") }}





## JavaScript

Uses the P5.js library.


{{ code(src="content/tasks/2048/javascript.js", lang="JavaScript") }}




## Julia

Uses the Gtk toolkit. Includes scoring, a choice of board size and toolbar buttons for Undo and New Game.


{{ code(src="content/tasks/2048/julia.jl", lang="julia") }}




## Kotlin

Stateless with focus on clarity rather than conciseness.



{{ code(src="content/tasks/2048/kotlin.kt", lang="scala") }}



Sample output:

```txt

New Grid:
+----+----+----+----+
|   2|    |    |    |
+----+----+----+----+
|    |    |    |   2|
+----+----+----+----+
|   4|  16|    |    |
+----+----+----+----+
|  16|   4|   2|    |
+----+----+----+----+
Direction?

```


## M2000 Interpreter



{{ code(src="content/tasks/2048/m2000_interpreter.m2000", lang="M2000 Interpreter") }}



Each move copied to clipboard
Output:

```txt

Game 2048 Score 14
[   2][   4][   8][   8]
[    ][   2][   2][   2]
[    ][    ][    ][    ]
[    ][    ][    ][    ]
Game 2048 Score 24
[    ][   2][   4][  16]
[    ][    ][   2][   4]
[    ][    ][    ][    ]
[    ][    ][   2][    ]
Game 2048 Score 26
[    ][    ][    ][    ]
[    ][    ][    ][    ]
[   2][    ][   4][  16]
[    ][   2][   4][   4]
Game 2048 Score 30
[    ][    ][    ][    ]
[    ][    ][    ][    ]
[    ][   2][    ][  16]
[   2][   2][   8][   4]


```



## Maple

This application requires a bunch of different components to properly run when being as close to the mobile game as possible.
These components are: A math container for the grid, an arrow key for each direction, a restart button, a text box to display the game over/highscore/arrow key to start messages, labels for score and highscore, and textboxes for the highscore and score values.
Once these are created, change the names to the ones in the main body of code, and include the proper procedures for the 4 arrows and the restart button.

Next is the main body of code:


{{ code(src="content/tasks/2048/maple.mpl", lang="Maple") }}




## MATLAB




{{ code(src="content/tasks/2048/matlab_1.m", lang="MATLAB") }}



You can start with an empty 4 x 4 board and save the last state of the playing field with:


{{ code(src="content/tasks/2048/matlab_2.m", lang="MATLAB") }}



Or you start from a saved playing field:


{{ code(src="content/tasks/2048/matlab_3.m", lang="MATLAB") }}




## Nim


Works with Nim Compiler|0.19.4



{{ code(src="content/tasks/2048/nim.nim", lang="nim") }}





## OCaml




{{ code(src="content/tasks/2048/ocaml.ml", lang="ocaml") }}



Output:


```txt

$ ocaml game2048.ml 4

    s -> left
    f -> right
    e -> up
    d -> down
    q -> quit

 [   0;    0;    0;    4]
 [   0;    0;    0;    0]
 [   0;    0;    0;    0]
 [   0;    2;    0;    0]
d
 [   0;    0;    0;    0]
 [   2;    0;    0;    0]
 [   0;    0;    0;    0]
 [   0;    2;    0;    4]
d
 [   0;    0;    0;    0]
 [   4;    0;    0;    0]
 [   0;    0;    0;    0]
 [   2;    2;    0;    4]
f
 [   0;    2;    0;    0]
 [   0;    0;    0;    4]
 [   0;    0;    0;    0]
 [   0;    0;    4;    4]
f
 [   0;    0;    0;    2]
 [   0;    2;    0;    4]
 [   0;    0;    0;    0]
 [   0;    0;    0;    8]

```



## Perl 6

Uses termios to set the terminal options, so only compatible with POSIX terminals. This version does not include a specific "win" or "lose" condition. (though it would be trivial to add them.) You can continue to play this even after getting a 2048 tile; and if there is no valid move you can make, you can't do anything but quit.
Works with Rakudo|2018.05


{{ code(src="content/tasks/2048/perl_6.p6", lang="perl6") }}


Sample output:

```txt


	Press direction arrows to move.

	Press q to quit.

	┌──────┬──────┬──────┬──────┐
	│  4   │  2   │      │      │
	├──────┼──────┼──────┼──────┤
	│  16  │  8   │      │      │
	├──────┼──────┼──────┼──────┤
	│  64  │  32  │  16  │      │
	├──────┼──────┼──────┼──────┤
	│ 128  │ 512  │ 128  │  64  │
	└──────┴──────┴──────┴──────┘

	Score: 6392

```



## Phix

Faithful desktop gui reproduction of the above link (<https://gabrielecirulli.github.io/2048/>)
Now I just got figure out how to win...


{{ code(src="content/tasks/2048/phix.exw", lang="Phix") }}




## PHP

Works from PHP5 and upwards in CLI mode.


{{ code(src="content/tasks/2048/php.php", lang="PHP") }}





## PicoLisp



{{ code(src="content/tasks/2048/picolisp.l", lang="PicoLisp") }}




## Pony

Works with ponyc 0.10.0


{{ code(src="content/tasks/2048/pony.pony", lang="pony") }}




## Prolog

Works with swi-prolog, any version.


{{ code(src="content/tasks/2048/prolog.pro", lang="Prolog") }}


Output:

```txt
?- play_2048.

Welcome to the Prolog version of 2048

To play using w,s,a,d keys for movement, q to quit

+-------------------+
¦    ¦    ¦    ¦    ¦
¦----+----+----+----¦
¦    ¦    ¦    ¦  2 ¦
¦----+----+----+----¦
¦    ¦    ¦    ¦    ¦
¦----+----+----+----¦
¦    ¦    ¦    ¦    ¦
+-------------------+
```



## Python



{{ code(src="content/tasks/2048/python.py", lang="python") }}




## QB64



{{ code(src="content/tasks/2048/qb64.bas", lang="QB64") }}




## R

orginal R package : <https://github.com/ThinkRstat/r2048>


{{ code(src="content/tasks/2048/r.r", lang="R") }}



## Racket

Original repo: <https://github.com/danprager/2048>
Play the RacketScript fork online here: <http://rapture.twistedplane.com:8080/#example/2048-game>



{{ code(src="content/tasks/2048/racket.rkt", lang="Racket") }}




## REXX

This REXX version has the features:
::*   allows specification of '''N''',   the size of the grid   (default is '''4''').
::*   allows specification of the winning number   (default is '''2048''')
::*   allows specification for the '''random''' BIF's seed   (no default).
::*   allows abbreviations for the directions   (Up, Down, Left, Right).
::*   allows the player to quit the game at any time.
::*   does error checking/validation for entered directions   (in response to the prompt).
::*   keeps track of the number of legal moves made and the score.
::*   displays the number of moves and the score   (when a blank is entered).
::*   displays an error message if a move doesn't do anything.
::*   displays a message if a winning move was entered.
::*   displays the game board as a grid   (with boxes).


{{ code(src="content/tasks/2048/rexx.rexx", lang="rexx") }}


Output:

```txt
           ╔══════╦══════╦══════╦══════╗
           ║      ║      ║      ║      ║
           ╠══════╬══════╬══════╬══════╣
           ║  2   ║      ║      ║      ║
           ╠══════╬══════╬══════╬══════╣
           ║      ║      ║      ║      ║
           ╠══════╬══════╬══════╬══════╣
           ║      ║      ║      ║      ║
           ╚══════╩══════╩══════╩══════╝

──────── Please enter a direction  (Up, Down, Right, Left)       ───or───    Quit:
right                                         ◄■■■■■■■■■■■■■ user input

           ╔══════╦══════╦══════╦══════╗
           ║      ║      ║      ║      ║
           ╠══════╬══════╬══════╬══════╣
           ║      ║      ║      ║  2   ║
           ╠══════╬══════╬══════╬══════╣
           ║      ║      ║      ║      ║
           ╠══════╬══════╬══════╬══════╣
           ║      ║      ║      ║  2   ║
           ╚══════╩══════╩══════╩══════╝

──────── Please enter a direction  (Up, Down, Right, Left)       ───or───    Quit:
up                                            ◄■■■■■■■■■■■■■ user input

           ╔══════╦══════╦══════╦══════╗
           ║      ║      ║      ║  4   ║
           ╠══════╬══════╬══════╬══════╣
           ║      ║      ║      ║  2   ║
           ╠══════╬══════╬══════╬══════╣
           ║      ║      ║      ║      ║
           ╠══════╬══════╬══════╬══════╣
           ║      ║      ║      ║      ║
           ╚══════╩══════╩══════╩══════╝

──────── Please enter a direction  (Up, Down, Right, Left)       ───or───    Quit:
                                              ◄■■■■■■■■■■■■■ user input (a blank)
──────────────────────────────────────── moves: 2 ──────── score: 4

           ╔══════╦══════╦══════╦══════╗
           ║      ║      ║      ║  4   ║
           ╠══════╬══════╬══════╬══════╣
           ║      ║      ║      ║  2   ║
           ╠══════╬══════╬══════╬══════╣
           ║      ║      ║      ║      ║
           ╠══════╬══════╬══════╬══════╣
           ║      ║      ║      ║      ║
           ╚══════╩══════╩══════╩══════╝

──────── Please enter a direction  (Up, Down, Right, Left)       ───or───    Quit:
left                                          ◄■■■■■■■■■■■■■ user input

           ╔══════╦══════╦══════╦══════╗
           ║  4   ║      ║      ║      ║
           ╠══════╬══════╬══════╬══════╣
           ║  2   ║      ║      ║      ║
           ╠══════╬══════╬══════╬══════╣
           ║      ║      ║      ║      ║
           ╠══════╬══════╬══════╬══════╣
           ║      ║      ║      ║  2   ║
           ╚══════╩══════╩══════╩══════╝

──────── Please enter a direction  (Up, Down, Right, Left)       ───or───    Quit:
l                                             ◄■■■■■■■■■■■■■ user input

           ╔══════╦══════╦══════╦══════╗
           ║  4   ║      ║      ║      ║
           ╠══════╬══════╬══════╬══════╣
           ║  2   ║      ║      ║      ║
           ╠══════╬══════╬══════╬══════╣
           ║      ║      ║      ║      ║
           ╠══════╬══════╬══════╬══════╣
           ║  2   ║  4   ║      ║      ║
           ╚══════╩══════╩══════╩══════╝

──────── Please enter a direction  (Up, Down, Right, Left)       ───or───    Quit:
dow                                           ◄■■■■■■■■■■■■■ user input

           ╔══════╦══════╦══════╦══════╗
           ║      ║      ║      ║  2   ║
           ╠══════╬══════╬══════╬══════╣
           ║      ║      ║      ║      ║
           ╠══════╬══════╬══════╬══════╣
           ║  4   ║      ║      ║      ║
           ╠══════╬══════╬══════╬══════╣
           ║  4   ║  4   ║      ║      ║
           ╚══════╩══════╩══════╩══════╝

──────── Please enter a direction  (Up, Down, Right, Left)       ───or───    Quit:
left                                          ◄■■■■■■■■■■■■■ user input

           ╔══════╦══════╦══════╦══════╗
           ║  2   ║      ║      ║      ║
           ╠══════╬══════╬══════╬══════╣
           ║      ║      ║      ║  2   ║
           ╠══════╬══════╬══════╬══════╣
           ║  4   ║      ║      ║      ║
           ╠══════╬══════╬══════╬══════╣
           ║  8   ║      ║      ║      ║
           ╚══════╩══════╩══════╩══════╝

──────── Please enter a direction  (Up, Down, Right, Left)       ───or───    Quit:
lef                                           ◄■■■■■■■■■■■■■ user input

           ╔══════╦══════╦══════╦══════╗
           ║  2   ║      ║      ║      ║
           ╠══════╬══════╬══════╬══════╣
           ║  2   ║      ║      ║  2   ║
           ╠══════╬══════╬══════╬══════╣
           ║  4   ║      ║      ║      ║
           ╠══════╬══════╬══════╬══════╣
           ║  8   ║      ║      ║      ║
           ╚══════╩══════╩══════╩══════╝

──────── Please enter a direction  (Up, Down, Right, Left)       ───or───    Quit:
d                                             ◄■■■■■■■■■■■■■ user input

           ╔══════╦══════╦══════╦══════╗
           ║      ║      ║      ║      ║
           ╠══════╬══════╬══════╬══════╣
           ║  4   ║      ║      ║      ║
           ╠══════╬══════╬══════╬══════╣
           ║  4   ║      ║      ║  2   ║
           ╠══════╬══════╬══════╬══════╣
           ║  8   ║      ║      ║  2   ║
           ╚══════╩══════╩══════╩══════╝

──────── Please enter a direction  (Up, Down, Right, Left)       ───or───    Quit:
d                                             ◄■■■■■■■■■■■■■ user input

           ╔══════╦══════╦══════╦══════╗
           ║      ║      ║      ║      ║
           ╠══════╬══════╬══════╬══════╣
           ║      ║  2   ║      ║      ║
           ╠══════╬══════╬══════╬══════╣
           ║  8   ║      ║      ║      ║
           ╠══════╬══════╬══════╬══════╣
           ║  8   ║      ║      ║  4   ║
           ╚══════╩══════╩══════╩══════╝

──────── Please enter a direction  (Up, Down, Right, Left)       ───or───    Quit:
                                              ◄■■■■■■■■■■■■■ user input (a blank)
──────────────────────────────────────── moves: 9 ──────── score: 32

           ╔══════╦══════╦══════╦══════╗
           ║      ║      ║      ║      ║
           ╠══════╬══════╬══════╬══════╣
           ║      ║  2   ║      ║      ║
           ╠══════╬══════╬══════╬══════╣
           ║  8   ║      ║      ║      ║
           ╠══════╬══════╬══════╬══════╣
           ║  8   ║      ║      ║  4   ║
           ╚══════╩══════╩══════╩══════╝

──────── Please enter a direction  (Up, Down, Right, Left)       ───or───    Quit:
q                                             ◄■■■■■■■■■■■■■ user input

──────── quitting the game
```



## Ring



{{ code(src="content/tasks/2048/ring.ring", lang="ring") }}




## Ruby

inspired by the Perl6 version


{{ code(src="content/tasks/2048/ruby.rb", lang="ruby") }}




## Rust


### Text mode

A simple implementation in rust. The user has to input an endline since i did not find a way to read a key press


{{ code(src="content/tasks/2048/rust.rs", lang="rust") }}




## Scala



{{ code(src="content/tasks/2048/scala.scala", lang="scala") }}




## Seed7

The Seed7 program below works in a text console.
Commands are [read](http://seed7.sourceforge.net/libraries/keybd.htm#getc(in_console_keybd_file)) from the file
[KEYBOARD](http://seed7.sourceforge.net/libraries/keybd.htm#KEYBOARD), which delivers cursor keys and function keys as single characters (e.g. KEY_LEFT or KEY_F1). Additionally KEYBOARD delivers single key-presses without echo.
All this is done independent from the operating system or terminal/console.
The output of the program is written to [STD_CONSOLE](http://seed7.sourceforge.net/libraries/console.htm#STD_CONSOLE),
which allows [cursor positioning](http://seed7.sourceforge.net/libraries/console.htm#setPos(in_console_file,in_integer,in_integer)),
after it has been [opened](http://seed7.sourceforge.net/libraries/console.htm#open(CONSOLE)).
STD_CONSOLE works also always the same, independent from the operating system or terminal/console.



{{ code(src="content/tasks/2048/seed7.sd7", lang="seed7") }}




## Tcl


### Text mode



{{ code(src="content/tasks/2048/tcl.tcl", lang="tcl") }}




### Tk

See <https://tcl.wiki/39566>.


## XPL0



{{ code(src="content/tasks/2048/xpl0.xpl", lang="XPL0") }}


