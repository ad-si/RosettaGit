+++
title = "Talk:Tic-tac-toe"
description = ""
date = 2018-10-27T05:33:17Z
aliases = []
[extra]
id = 9208
[taxonomies]
categories = []
tags = []
+++

==lang tag and line width==
I notice that the lang tag is setting a severe maximum line width before scrolling. I would suggest it not be done or be set to something large e.g. 160 characters. --[[User:Paddy3118|Paddy3118]] 07:15, 4 February 2011 (UTC)

: It's never been a problem for me; is this in fact due to the box containing the code being constrained in width by the result of the {{tmpl|task}} template? If so, the problem will go away (or at least become purely characterized by the width of the browser window, as is good and proper) once there are some more implementations. –[[User:Dkf|Donal Fellows]] 10:04, 4 February 2011 (UTC)
::Oh. Looking at it from work, things seem to be fixed. Thanks?! --[[User:Paddy3118|Paddy3118]] 10:51, 4 February 2011 (UTC)

== Design issues ==

A few notes on the Tcl solution that are perhaps of more general interest.

I decided to keep the game core separate from the players. Although this makes the code quite a bit longer, it also makes it clearer what the responsibilities are and how much knowledge they can have (they know their letter, they can see the board and the legal moves on it) and so it properly demonstrates that it ''is'' a game. Well, in my opinion anyway. –[[User:Dkf|Donal Fellows]] 09:33, 5 February 2011 (UTC)

... And I like your printout of the board.  I really did do a minimal version of the game for Python. Maybe someone else will do a fancy version in Python for comparison? --[[User:Paddy3118|Paddy3118]] 12:45, 5 February 2011 (UTC)

: I took that approach and added still more whitespace (for the REXX entry).   To make the game board (grid) easier to see (without the reference numbers --- otherwise it gets cluttered/obfuscated pretty fast with any game board larger than nine cells)   --- I removed the   ''numbering''   grid and placed it to the right of the playing board (along with generous whitespace.   --- this causes a reference game board (grid) to be displayed, making it a lot easier on the ole eyeballs to see who has what markers where, and what's available for a move (empty cell), and in specifying where to <strike>move</strike> place a piece/marker.   -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 16:18, 3 October 2017 (UTC)


: An illuminating example   (from a   '''4'''&times;'''4'''   tic─tac─toe game in progress):

```txt

───────── computer places a marker  [O]  at cell number   16

             ║     ║     ║                   │     │     │
             ║     ║     ║                1  │  2  │  3  │  4
             ║     ║     ║                   │     │     │
        ═════╬═════╬═════╬═════         ─────┼─────┼─────┼─────
             ║     ║     ║                   │     │     │
             ║  X  ║     ║  X             5  │  6  │  7  │  8
             ║     ║     ║                   │     │     │
        ═════╬═════╬═════╬═════         ─────┼─────┼─────┼─────
             ║     ║     ║                   │     │     │
             ║  O  ║     ║                9  │ 10  │ 11  │ 12
             ║     ║     ║                   │     │     │
        ═════╬═════╬═════╬═════         ─────┼─────┼─────┼─────
             ║     ║     ║                   │     │     │
             ║     ║     ║  O            13  │ 14  │ 15  │ 16
             ║     ║     ║                   │     │     │

───────── Please enter a cell number to place your next marker [X]     (or Quit):

```


== letting the human win (sometimes) ==
A note on the REXX solution:   tic-tac-toe games,   if played perfectly,   will always end in a draw, making it a   [https://en.wikipedia.org/wiki/Futile_game futile game].

(Except for the case for a   '''2x2'''   grid, a win is guaranteed for the player moving first.)

Since kids may be playing it   (or adults with very short attention spans),   I programmed a "hole" in the logic of the REXX program   so that if a human player plays first (the default),   they may win if a certain pair of moves are made.   -- [[User:Gerard Schildberger|Gerard Schildberger]] 23:49, 10 December 2012 (UTC)

It's not much fun playing a game ya can't win.   -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 15:52, 3 October 2017 (UTC)

=="ancient" computer game==

I'm surprised that   BASIC,   FORTRAN,   and   PL/I   computer programming languages haven't had solutions entered   (at the time of this posting).   Tic-tac-toe was one of the first games written in any language as it was so simple to program and display.   I still have my FORTRAN and PL/I programs laying around from the mid 1960s which played on a   NxN   grid.   Now, if I could only find a card reader ...   -- [[User:Gerard Schildberger|Gerard Schildberger]] 19:10, 23 April 2012 (UTC)

== 3D tic-tac-toe ==

Has anyone thought of writing/entering a   '''3D'''   version of tic-tac-toe for Rosetta Code   (as a separate task, of course)?

I imagine the programs would get a tad obtuse   (hard to follow).   -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 00:58, 17 April 2016 (UTC)



-----



I wrote a PL/I program   (the default grid size was 4x4x4)   and it was (an infamous) favorite with the (IBM) OS/MFT operators (in Boston).   No terminals with screens back then for our shop   ---   that was waaaaaaaay back in   (cough-cough)   <small>1970</small>,   ... just the mainframe console   (an IBM 1052 typewriter).   A lot of time was wasted on the off-shifts and weekends, to be sure.   A lot of operators volunteered to work on weekends, and the   ''powers-that-be''   never caught on   (or just didn't care). -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 00:58, 17 April 2016 (UTC)


The PL/I program had a   '''4D'''   capability,   that is,   after so many moves were done   (this could be specified at game start),   the oldest marker was   ''removed''   from the playing grid.   -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 00:58, 17 April 2016 (UTC)
