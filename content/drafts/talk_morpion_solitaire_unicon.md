+++
title = "Talk:Morpion solitaire/Unicon"
description = ""
date = 2012-02-23T15:10:38Z
aliases = []
[extra]
id = 11432
[taxonomies]
categories = []
tags = []
+++

== Other ideas ==
As I was working with this I had ideas for other approaches or saw that others had shown how things could be done differently. Observation of Pentasol in operation.  Review of some of the references like Dimitri Tishchenko's blog as a couple of examples. Some of these ideas add complexity.  Some improve performance.  Many are overkill for the task but may be useful if the task were more ambitious (I'm not suggesting it should be).
* Dispensing with the expandable grid.  Have a statically expandable grid and pick a size like 30x30.
* It should be possible to perform more localized grid scanning. --[[User:Dgamey|Dgamey]] 12:42, 23 February 2012 (UTC)
* It should be possible to preserve and track available moves as the game progresses. --[[User:Dgamey|Dgamey]] 12:42, 23 February 2012 (UTC)
== Strategies and Heuristics ==
The lack of documented (or easily found) strategies for Human players is a bit frustrating.  Rosin's paper on Nested Mote Carlo with Adaptive Rollout suggests a couple.  --[[User:Dgamey|Dgamey]] 12:42, 23 February 2012 (UTC)
== Other Notations ==
I was wondering if there might be some benefit to using other notations for game recording.  There are some intriguing possibilities, Tishchenko's morpion layer encoding is one.  I developed one that eliminates rotational effects that I thought might be useful in a genetic algorithm.  It should also be possible to eliminate reflection symmetry as well. --[[User:Dgamey|Dgamey]] 12:42, 23 February 2012 (UTC)

###  Turtle Notation 

A different game recording/replay notation inspired by turtle graphics that should have several advantages:

* orientaion-less representation of games reduces duplications (under rotation)
* allows reflection-less representation of games given initial move optimization
* possibly suitable for genes in a GA

How it looks in concept (minus visual separators):

[<move number>][<orientation>]<turtle move><turtle line>[<orientation>]

* The move number is straightforward (and probably not required except as an aid to visual inspection)
* The orientation is only needed at the initial position and only for exact replay.  All other occurrences of orientation are only helpful visually.  From a pure programmatic view all of the orientation and move number information can be discarded.
* The turtle move takes you to the cell to be filled and contains:
** a grid distance (not diagonal)
** a turn, Right, Left, Forward (implied), Backwards
* The Turtle line tells how to mark the line and is similar to turtle moves.  Whether it changes the orientation is probably an arbitrary design choice (i.e. the turtle can be left either at the move or end of the line).  Lines would need to be drawn consistently by direction left, right, etc.
* The groups of turtle moves/lines should be normalized (say longest distance first, infront of, left of, ...)
* The turtle starts at the center of the grid (off point) and the first move will have an extra 1/2 cell in each direction (this can be implicit so integers can be used)
* Reflection can be handled in the very first orientation parameter of the game.  Basically stating which side (front/back) the viewer is looking from
* This needs to take into account that there are only 4 possible initial moves ( inside corner, two types of outside corners, and outside valley)

The sequence of turns and distances may prove to be something on which a strategy/fitness can be evaluated.

That's it.  --[[User:Dgamey|Dgamey]] 13:37, 23 February 2012 (UTC)
