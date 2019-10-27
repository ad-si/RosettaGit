+++
title = "Talk:Morpion solitaire"
description = ""
date = 2014-07-05T19:39:31Z
aliases = []
[extra]
id = 9892
[taxonomies]
categories = []
tags = []
+++

== Rules, References, How it works ==

The wikipedia page doesn't exist - where else can the exact rules be seen? The task says "A move is made by adding one point to the end of a line of 4 and drawing a straight line through the 5 points." But the in the given example [http://www.morpionsolitaire.com/Grid5T177RosinA+.GIF RosinA] the move number 2 is the middle point of a diagonal line (not the end). So obviously diagonal lines are allowed, but what means then "drawing a straight line through the 5 points"? Also, the description says that "lines cannot overlap existing lines" which contradicts this --[[User:Abu|Abu]] 13:48, 7 June 2011 (UTC)
:The WP page does exist, the link was just spelled wrong. The rules are pretty clear there. Looks like it takes practice to get it. --[[User:Mwn3d|Mwn3d]] 13:52, 7 June 2011 (UTC)
:Ah. Yeah, it's fine to add a point anywhere that creates a line of 5. When it says lines cannot overlap, I think a better way of putting it might be that lines can't share more than one point. (They can cross at one point, but can't overlap pieces of the line itself.) [[User:MagiMaster|MagiMaster]] 21:15, 7 June 2011 (UTC)
::Presumably that means that we cannot have a line of more than five. [[User:Markhobley|Markhobley]] 21:05, 18 July 2011 (UTC)

::: That's not what I presummed (with a '''T''' game).  A line of '''xxxxbxxxx''' where '''b''' is a blank --- when the next move is placed at the '''b''', you have a line of nine. -- [[User:Gerard Schildberger|Gerard Schildberger]] 05:56, 16 May 2012 (UTC)

:Can we drop "and drawing a line through them" from the task description (or is that some sort of crossing out or deletion?). In other words do we keep the line of 5 Xs? or do we delete them (by changing them to dots), or do we need change them to Os (representing a crossing out) to prevent them from being reused? [[User:Markhobley|Markhobley]] 21:05, 18 July 2011 (UTC)

:: Well, they can be reused when lines cross. -- [[User:Gerard Schildberger|Gerard Schildberger]] 05:56, 16 May 2012 (UTC)

:Are these legal or not legal?:

 X   X      X   X     XXXXX       X
 X   X      XXXXX      XXXXX      XX
 XXXXX      X   X                XXXXX
 X   X      XXXXX                 XX
 X   X      X   X                 XX
                                   X

(Presumably they are impossible to form and none are legal. Is that right?)

[[User:Markhobley|Markhobley]] 07:44, 19 July 2011 (UTC)

"Drawing a straight line" simply marks the line just completed so it's easier to see what happened.  Each node is allowed to be shared by multiple lines, if the lines are all oriented at different directions.  If two lines are aligned, apparently there are two different rules: the "T" (touching) rule says two colinear lines are allowed to share one end point only; the "D" (disjoint) rule says colinear lines can't share nodes at all. The following:
```txt
AAAACBBBB
```
 is allowed by the T rule, where AAAAC and CBBBB are two legal lines; by D rule, only one of them is legal (whichever came first).  On the other hand, 
```txt
XXXXXOOOOO
```
 are considered two legal lines by both rules.  I'm unsure what happens if you put a piece at the blank in 
```txt
XXX_XXX
```
--whether the whole thing is considered as one oversized line, or only 5 or them would be considered used... Then again, the whole thing is pretty bogus in that it's NP-hard so no "proper" solution exists, it's not much fun to play, and it's not much fun to watch, so there.  --[[User:Ledrug|Ledrug]] 08:40, 19 July 2011 (UTC)

: I found some rules: http://www.morpionsolitaire.com/English/Rules.htm :) [[User:Markhobley|Markhobley]] 11:02, 19 July 2011 (UTC)
:: Sorry for not posting this sooner, but the idea was to get the computer to play it, probably randomly, or by some simple heuristic. Dealing with all the intricacies of the rules is not an insignificant programming challenge. Trying to find the best solution is beyond this task. [[User:MagiMaster|MagiMaster]] 14:11, 20 July 2011 (UTC)

::It is not stated in the rules that I found, but I reckon that you must score at least one line of five for each turn of play. (In other words you cannot place marks just anywhere on the grid, you have make a line of five.) If you cannot make a move that creates a line of five, then the game is over. Additionally, marks can be crossed out from the direction that they make the line of 5 in, but can be used from other directions. So 5 in a row vertically, are crossed out vertically, but the marks can still be used to form horizontal or diagonal lines, which then also become crossed out. (I have been playing a version of this on paper for couple of days.) [[User:Markhobley|Markhobley]] 16:56, 20 July 2011 (UTC)

I was looking at this.  It looks to me that the following are valid:

```txt

YES (touching, 5T):
+++.-
++.+-
+.++-
-+++.

YES (non-touching, 5T&D):
++++.
+++.+
++.++

Where:
+ = any X not part of a line running in the same direction being contemplated
- = an X used in a line running in the same direction
. = an empty position in the grid we can use to make a new line
```

These cases eliminate duplicates (reversed patterns).  Everything else is not valid.  --[[User:Dgamey|Dgamey]] 14:58, 12 January 2012 (UTC)

==Multiple lines==
Suppose a newly marked grid creates more than one line.  How much does this score?  How many new marks are permitted?  Until specified, I'd say the programmer should freely choose and state which rule the code follows.  I choose that only one line scores and the game continues in the simple fashion with only a next turn.  In some cases it seems that a new mark would create a contiguous line of 5 on one of the alternate paths.   --LambertDW 00:45, 24 June 2014 (UTC)

== Solution Sizes / Sub-pages ==
I looked into this a bit more and think that many of the solutions will be on the longish side and should have their own sub-pages (e.g.  [[Morpion_solitaire/C]]) like tasks such as [[Go_Fish]].  --[[User:Dgamey|Dgamey]] 00:51, 23 January 2012 (UTC)
: It would be possible to move the tasks into a subpage and use mediawiki trickery (i.e. template magic) to make the contents appear in the page anyway. I'm not planning to do anything about rearranging yet; while there's only one solution, there's no need to act… –[[User:Dkf|Donal Fellows]] 09:33, 23 January 2012 (UTC)
:: Hi Donal, I know there are some plans/considerations underway to restructure RC with SML but not sure where they are.  I think there may have been something about putting all solutions in their own pages, auto sorting within the task, etc. Not sure where that is. It would be very cool if the page could expand the solutions you select.  In the meantime if solutions for this start showing up then something probably should be done. Any idea if there is a policy/guideline on what size of solution should be sub-paged off? I recall the current C solution is 225 lines. --[[User:Dgamey|Dgamey]] 12:06, 23 January 2012 (UTC)
::: We tried this, and it's something I'd very much love to see become the norm. Ran into a SMW bug involving transclusions that's still not fixed upstream. Had to do with templates. People are absolutely welcome to experiment in getting this kind of thing working again. You won't kill the server. :) --[[User:Short Circuit|Michael Mol]] 14:20, 23 January 2012 (UTC)
::: Also, no explicit policy/guidelines regarding solution sizes. (that I remember) It comes down to what's reasonable for a page, which is different for every page. --[[User:Short Circuit|Michael Mol]] 14:20, 23 January 2012 (UTC)
::::I say if you think it might be too long, just put it on its own page. I usually move them off if they approach 10 KB or so (maybe a little less than that...7 or 8?). You can see how big an edit is on the recent changes feed. Then just make sure you name the solution page well--something like [[Task name/Language]] or [[Task name/Language/Language subheading]]. --[[User:Mwn3d|Mwn3d]] 14:48, 23 January 2012 (UTC)
::::: I'd put it down to how large the overall page is as the threshold to start doing things. There definitely are some pages that ought to have some attention. OTOH, I don't think this page has got to that point. Well, not yet. I vote for YAGNI on elaborate stuff (well, at least until the point when we ''do'' need it) since simple pages have the benefit of being known to work. I don't know exactly what the threshold should be though; the old 32kB point where MW moans at us is probably a bit low, since next to nobody seriously uses old IE any more due to the hardware it was on finally being junked. (IIRC, IE7 and up aren't nearly as brain-damaged…) –[[User:Dkf|Donal Fellows]] 22:19, 23 January 2012 (UTC)
:::::: Further to this, see [[Arithmetic/Rational#Tcl]] for an example of how to do the inclusion once pages start getting split up. –[[User:Dkf|Donal Fellows]] 15:58, 17 February 2012 (UTC)

== Consolidated References ==

* [[wp:Morpion Wikipedia]]
* On August 12, 2011 Chris Rosin achieved [http://www.morpionsolitaire.com/English/RecordsGrids5T.htm 178 moves]. Unfortunately this graphic of the game requires the a bit more effort on the part of the reader to work out any ambiguities.
* [http://www.chrisrosin.com/morpion/index.html previous 177 move record] also by Chris Rosin.  This graphic includes 'stops' that disambiguate lines when there are multiple choices in one direction, its also sans background grid.
* [http://www.chrisrosin.com/rosin-ijcai11.pdf Rosin's paper on NPRA (Nested Rollout Policy Adaption for Monte Carlo search algorithm] this describes some heuristics as well as the search and other good references.  
* [http://www.lamsade.dauphine.fr/~cazenave/ Tristan Cazenave's articles] Earlier search algorthims
* [http://www.ipgp.fr/~sibilla/divers/pmorpion_solitaire01.html Jean-Jacques Sibilla's page and best reported random game of 102 moves (french)].
* New Heuristics for Morpion Solitaire, 2007 by Hyyro and Poranen 
* Morpion Solitare, 2006 by Demaine, Demaine, Langerman, and Langerman gives bounds of 170 <= 5T <= 704
* Le Morpion Solitare, 2003 by Flammenkamp claims 5T <= 324
* [http://paths.sheffield.ac.uk/wikiana/wiki/Morpion_solitaire Sheffield Paths Wiki] which has some good references
* [http://koozdra.wordpress.com/2011/05/21/morpion-dna-encoding/ Koozdra blog] which discusses some approaches to heuristics

I'm still looking for references or discussion about human player strategies.  

consolidated references --[[User:Dgamey|Dgamey]] 13:48, 13 February 2012 (UTC)

== Game Notation ==

While working on this I realized it might be handy to be able to replay games and a game notation would be needed.  There is a defacto standard which is used for downloadable games found on the [http://www.morpionsolitaire.com/English/RecordsGrids5T.htm  Morpionsolitare.com records page]]. These games can be replayed on the [http://pentasol.systemutvecklarna.se/ Pentasol player].  

The advantage of using this system of output is that the Pentasol player will validate the game and provide an image of the result.

The notation looks like this:

```txt

#
#     XXXX
#     X  X
#     X  X
#  XXXR  XXXX
#  X        X
#  X        X
#  XXXX  XXXX
#     X  X
#     X  X
#     XXXX
#
# R = reference point
# List of moves starts with reference point (col,row)  i.e. (12,8) below
#
# Annotations: 
# Lines are
#    (col,row) <direction> <center>
#    direction   -      + 
#       |        up     down
#       -        left   right 
#       \        left   right
#       /        left   right
#    center is the distance -2, -1, ..., +2 from 
#    the move coordinate to the center of the line being drawn
#
(12,8)
(11,14) - +2
(12,4) | +2
(9,12) | -2
(18,7) | +2
(14,11) - +2
...
```


While I was unable to find a detailed description of the format, what I worked out is in the annotated comments above.  Comment lines are preceded by hashes.  The first line references the final position of the north-west valley of the starting cross.  Each line is a set of coordinates of the move followed by a direction and an offset to the center of the line created by the move.  The table in the comment above tells you how to figure out where the start of the line is. All coordinates use (column, row) order. --[[User:Dgamey|Dgamey]] 13:41, 13 February 2012 (UTC)

== Proposed Addition to the task definition ==

* Tasks really need to have some form of comparable output.  Drawings of games are often missing information and difficult to validate.  I'm going to suggest that each task should output the game in the notation that is used by the Pentasol player. It seems to be accepted as a defacto standard for morpion and the images it produces are better than ascii art.  I'd suggest that people could use the output of one of their random games played through pentasol to illustrate their example.
* Alternately, they should produce some kind of output like ASCII art.  Keep in mind that an image using a single character for all moves will be an undecipherable blob.  Adding numbers for the moves will require handling of two digit numbers making the art larger.  And lastly, even if you can see each move in order, there is the problem of deciphering up to 5 possible ambiguous moves at a point.  --[[User:Dgamey|Dgamey]] 13:43, 17 February 2012 (UTC)

== Tips and Tricks ==

* The initial two solutions work with expandable boards.  While nice it's not needed.  You'll never get near the world record grids so a fixed grid of say 30x30 would be more than large enough.  If you're worried about changing it later, you could pick an offset so the cross is always constructed from a fixed reference point say 0,0 or 1,1 depending on your language.  --[[User:Dgamey|Dgamey]] 12:38, 22 February 2012 (UTC)
* Playing morpion smartly or exploring the problem space isn't a trivial task and will require lots of effort, code and complexity.  Playing a random game is fairly straight forward.  The core code for the original two solutions comes in at about 200 lines each. --[[User:Dgamey|Dgamey]] 12:38, 22 February 2012 (UTC)

== Thoughts for Long Games ==

* Maximizing the number of possible moves seems like an obvious game solver strategy.  How about instead maximizing the area?  The initial grid makes me think low density is sufficient to have many possible moves.  --LambertDW 19:39, 5 July 2014 (UTC)
