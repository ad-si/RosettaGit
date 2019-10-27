+++
title = "Chess player"
description = ""
date = 2019-07-29T14:35:28Z
aliases = []
[extra]
id = 11656
[taxonomies]
categories = []
tags = []
+++

{{difficulty}}
{{draft task|Games}}

In the early times, chess used to be the prime example of artificial intelligence. Nowadays, some chess programs can beat a human master, and simple implementations can be written in a few pages of code.

Write a program which plays chess against a human player. 

No need for graphics -- a textual user interface is sufficient.

Rather than implementing a complete monolithic program, you may wish to tackle one of the simpler sub-tasks:

# [[Chess player/Move generation]]
# [[Chess player/Search and evaluation]]
# [[Chess player/Program options and user interface]]

or use those components as part of a complete program, demonstrating your language's support for modularity.


## Go

There are a number of open source Chess programs written in Go on Github.

Rather than spend a lot of time trying to write my own (likely mediocre) program, I thought I'd simply post a link to [https://github.com/notnil/chess notnil/chess] which explains its various capabilities quite well. However, you need to look at the code itself to see that it can cope with all types of move including castling, ''en passant'' capture and promotion to a piece of the player's choice.


## Phix

Version 0.8.1+ will contain demo\rosetta\chess.exw, a slightly cleaned-up copy of a 20-year old translation of TSCP.

It isn't particularly good (though perhaps a reasonable starting point for something better), at over 1,600 lines it does not really bear any useful comparison to the lisp version, and is simply not worth posting on this site, especially in light of potential copyright issues.


## PicoLisp

{{:Chess player/PicoLisp}}


## Python

==={{libheader|pygame}}===
"Python Chess" is a chess game at the [http://www.pygame.org/project-Python+Chess-1099-.html PyGame-Website] and [http://uniformlyuninformative.wordpress.com/python-chess Homepage].

==={{libheader|VPython}}===
There is a 3D-Chess-Board in the [http://vpython.org/contents/contributed/chessboard.py VPython contributed section].
