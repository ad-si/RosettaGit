+++
title = "Talk:Solve a Hidato puzzle"
description = ""
date = 2014-07-04T23:00:05Z
aliases = []
[extra]
id = 11186
[taxonomies]
categories = []
tags = []
+++

==Needed improvements==
I'm really reluctant to admit this as a full task just yet, as it is dependent on a complex applet in an external website. Better would be to put a description (which could involve a link to wikipedia) and specific problem to solve in this page. We can always judge for ourselves whether someone's solved the actual problem or just the specific instance of it. –[[User:Dkf|Donal Fellows]] 13:31, 12 January 2012 (UTC)

Technically I think the complex applet is a complex servlet. From the link you have changed this to following the external links to Hidato Home Page and then Hidato Daily Adventures will
take one to the same place. In spite of what's said below I've just enjoyed a few mins research there.
–[[User:Dkf|Nigel Galloway]]
:: There's a bit of a difference between research when writing a task or because it tweaked your interest. :)  And potentially every contributor for every (400+) languages on RC having to do research to solve the task. :( In my experience, they just ignore the task. :( --[[User:Dgamey|Dgamey]] 20:54, 13 January 2012 (UTC)
: How about two separate task? One for creating the puzzles, one for solving them? --[[User:Short Circuit|Michael Mol]] 17:39, 12 January 2012 (UTC)

: It seems like an interesting problem but there really needs to be more supporting information for this task.   
:* The linked WP article [[wp:Hidato]] is of poor quality and provides very little of use, there isn't even a through discussion of rules there saying how to get to the solution. The solution doesn't seem to be full of consecutive numbers in every row, column, or diagonal; and it certainly isn't clear how the numbers got laid down.  Some lines are consecutive except for one number.  Others aren't consecutive at all. There may be a method but it's far from obvious. 
::* Oh! The numbers wind through the grid from lowest to highest.  Now why didn't it say that clearly instead of just leaving it at 'consecutive numbers'? --[[User:Dgamey|Dgamey]] 20:10, 12 January 2012 (UTC)
:* What's with the Knights Tour extra credit reference (we already have  a task)?  Maybe it's an easy step up but I don't see it and the task already seems complex enough. It's a nice fact that this can used for this, but why make it even optionally part of the task? 
:* I think this is a solution task.  Generating a board should be another task.
: Generally if I can't figure out most of what I need to do a task (at least broadly) from the task page then I'm not very interested in going off and researching it.  Nor am I interested in reverse engineering the entire solution from a language I'm unfamiliar with.  Task descriptions should be be able to paint enough of a picture to draw in contributors without requiring potentially a few hundred contributors to do that level of research or translation.  That's my $.02 --[[User:Dgamey|Dgamey]] 19:57, 12 January 2012 (UTC)
:: well, for starters the example problem on wikipedia could be used as the input to be solved. a solution is there too, and a better description for what the rules are can be made. maybe even a better drawing for the solution that includes a thread that connects all the numbers in sequence. alternatively though maybe a square 3x3 problem is easier, so that just needs to be found. are any other details missing to make the task solvable?--[[User:EMBee|eMBee]] 02:21, 13 January 2012 (UTC)
::: Certainly a set example and result are needed. A better description. The wp diagram or one like it should be fine if accompanied by a better description.  I guess one of my concerns is making sure we have a properly set puzzle with a unique solution.  For a small example, this can be validated via brute force. That raises the question if there is any requirement to find a solution by means other than brute force?  And of course copyright if we have to use another source.  I'd also remove the extra credit as a task; although, noting this as an application with some kind of reference would be interesting background.  I'll leave the task of setting a puzzle and observe that we don't have such a task for [[Sudoku]] yet.  That just about covers it for me.  --[[User:Dgamey|Dgamey]] 04:28, 13 January 2012 (UTC)

: I'm not convinced that the Knight's Tour part adds anything at all to this, as one of the most useful techniques that is applicable to this problem (pruning pathing solution candidates that provably can't reach the goal; that's implied by the use of the Moore Neighborhood) doesn't apply at all. The nice thing seems to be that this is a ''fast'' problem, despite needing fairly complex searching. –[[User:Dkf|Donal Fellows]] 19:15, 15 January 2012 (UTC)
:: Donal, I'm with you on this. While I might be interested in a link to some side reading as background, I can't see doing both.  I don't think it's a simple as just swapping in another adjacency function and we already have a separate Knight's Tour task.  --[[User:Dgamey|Dgamey]] 20:02, 15 January 2012 (UTC)

== Rules ==

The rules are:
* You are given a grid with some numbers placed in it. The other squares in the grid will be blank.
** The grid is not necessarily rectangular.
** The grid may have holes in it.
** The grid is always connected.
** The number “1” is always present, as is another number that is equal to the number of squares in the grid. Other numbers are present so as to force the solution to be unique.
* The aim is to place a natural number in each blank square so that in the sequence of numbered squares from “1” upwards, each square is in the [[wp:Moore neighborhood]] of the squares immediately before and after it in the sequence (except for the first and last squares, of course, which only have one-sided constraints).
** Thus, if the grid was overlaid on a chessboard, a king would be able to make legal moves along the path from first to last square in numerical order.
** A square may only contain one number.
* In a proper Hidato puzzle, the solution is unique. (Only really relevant during construction, but might make solving easier.)
I think that sums them up properly. –[[User:Dkf|Donal Fellows]] 10:41, 13 January 2012 (UTC)

: That's very good. I think it could be a bit less formal:
::The rules are:
::* You are given a grid with cells.  Some cells have numbers while others are blank.
::** The grid is not necessarily rectangular and could have holes (non-cells). 
::** All cells are connected/adjacent (no islands).
::** At the start the high and low (normally numbered from 1) cells are marked.  Other numbers are provided to ensure there is a unique solution to the puzzle.
::** When completed each cell contains a single integer and the grid contains the integers in sequence winding through the grid like a King on a Chess board.  More formally, each square is in the [[wp:Moore neighborhood]] of the squares immediately before and after it in the sequence except for the first and last cells which only have only one connection each.
::The task is to write a program to solve these puzzles.  A sample grid and its solution has been provided.
:How's that? Also,
::* We still need the samples, the ones from Wikipedia should work.
::* I still don't know anything about the hardness of this and if brute force or elegance is preferred/needed.  It seems to me that given the Moore Neighbourhood constraint that puzzles would have to get really quite large before brute force would have any problems (but that's just gut feel).  --[[User:Dgamey|Dgamey]] 17:58, 13 January 2012 (UTC)
::: Thinking about it, finding a unique solution to Knights Tour given similar clues and a unique solution is likely much easier than solving Knights Tours.  An alternate name for this might seem to be a Kings Tour (sadly, not very exciting).

See http://en.wikipedia.org/wiki/Adjacency_list fo a discussion of implementations. The Mathprog example Rule 3 is such a list. As is Rule 3 in the Mathprog Knights Tour example. Only the definition of adjacency has changed. We could add extra credit if it also solves Numbrix which is a von Neuman Neighbourhood.
–[[User:Dkf|Nigel Galloway]]
: Perhaps worthy of its own task.  There are probably lots of interesting research and discussion for Numbrix. --[[User:Dgamey|Dgamey]] 21:05, 13 January 2012 (UTC)

== Tcl counterexample ==

The Tcl code logic is unsound: even if a unique solution exists, it doesn't mean any "leg" of the initial puzzle has a unique partial solution.  Try this:

```tcl
solveHidato "
. 4 .
0 7 0
1 0 0
"
# solution:
# . 4 .
# 3 7 5
# 1 2 6
```
 and the program will fail to find anything. --[[User:Ledrug|Ledrug]] 18:15, 1 May 2012 (UTC)

: We were supposed to create code that could solve ''all'' valid puzzles? –[[User:Dkf|Donal Fellows]] 08:05, 2 May 2012 (UTC)
:: I thought that's what the task asked: "''write a program which solves Hidato puzzles.''" Though I don't know if there is a better general algorighm than exponential (exhaustive search). --[[User:Ledrug|Ledrug]] 19:31, 2 May 2012 (UTC)
: I do have a version that solves even those awkward cases (using a more careful speculative execution model but still based on the solution in the task) but it's rather longer. I'll put it on a sub-page. –[[User:Dkf|Donal Fellows]] 08:24, 2 May 2012 (UTC)
:: Extended version failed on the "awkward" example with a stock Tcl8.6.  Guess it's not recent enough? --[[User:Ledrug|Ledrug]] 00:07, 3 May 2012 (UTC)
::: Yes. It needs a feature ([http://www.tcl.tk/cgi-bin/tct/tip/397.html improved object cloning]) that's not yet made it into a beta release. –[[User:Dkf|Donal Fellows]] 07:50, 3 May 2012 (UTC)

== Any good general algorithm? ==

I added a larger example in the C code, which is meant to make brute force search miserable.  The C code can solve it, but it takes quite long; I expect D to behave similarly.  The Perl/Python/Tcl code should be able to solve it, if only taking forever, since they all use the same exhaustive search method (Tcl is forced into such a situation, but anyway).  The Mathprog code gives up on it on my machine, rather quickly, which can be said to be the bright side. --[[User:Ledrug]]
:You do not indicate how glpk gives up on your machine, I am guessing numerical instability. Evil Case 2 makes the problem much larger. I have added examples of using Mathprog suitable for this large example. Note the model is the same, I just used a basis recommended for large multi indexed problems with a nested structure. In all three cases the solution then takes less than 1 second.--[[User:Nigel Galloway|Nigel Galloway]] 15:38, 4 May 2012 (UTC)

:: The earlier Mathprog code failed with something like "no possible solutions", without more explicit explanations. The current code still fails on the "evil 2" example, after about a gadzillion lines of instability warnings, then this:<lang>Error: unable to factorize the basis matrix (1)
Sorry, basis recovery procedure not implemented yet
glp_intopt: cannot solve LP relaxation
```

:: My glpsol version string is <code>GLPSOL: GLPK LP/MIP Solver, v4.45</code>, which might be why it doesn't have <code>--minisat</code>.  Examples were run without it, not sure if that switch was crucial or not. --[[User:Ledrug|Ledrug]] 19:53, 4 May 2012 (UTC)
:::The no feasible solution error implies that the data file you created contained an error, as there is a feasible solution to a correct data file. The --minisat is needed to avoid the numerical instability with a correct data file.--[[User:Nigel Galloway|Nigel Galloway]] 13:26, 5 May 2012 (UTC)
:::For an explanation of minisat you may wish to see http://en.wikipedia.org/wiki/Tseitin-Transformation briefly summized:
::::The Tseitin Transformation is used to produce a boolean equation in conjunctive normal form (CNF) from a combinatorial logic circuit so that it may be solved by a SAT solver. The naive approach is to write the circuit as an equation, and use De Morgan's law and distribution. However, this can result in an exponential increase in equation size. The Tseitin Transformation outputs an equation whose size has grown linearly relative to the input circuit's.
:::Exponential increase is bad when you increase the problems size, so we avoid it --[[User:Nigel Galloway|Nigel Galloway]] 13:47, 5 May 2012 (UTC)
:::I have added an example in Ruby which shows that no time problem exists if the path length between hints is reasonable. The new C version could check the length of the path it is looking for and not adopt the new strategy if it is reasonably short, hence not slowing down normal puzzles.--[[User:Nigel Galloway|Nigel Galloway]] 13:26, 5 May 2012 (UTC)
:::: Even though I noted about slowdowns at the front of the C code, I'm not really all that concerned about it.  The code uses a flood fill to check connectivity, which is O(n) because there's no backtracking, n being number of cells. For puzzles that are simple (i.e. brute force would have been more on the polynomial side without this check), it adds another polynomial term to run time, which could be relatively big but in absolute terms is not an issue (you can't see, but I'm making violent handwaving gesture here).  The 3x3 example requires 8 tries at filling cells with or without the checks, so it's definitely faster without, but it won't be noticeable.  For exponentially backtracking puzzles, the hope is it will kill off some (most?) long fruitless searches early (without check, the 50x3 example tries to fill values to cells 27962062 times before finding solution; with it, 85). I could do fancier checks, but not before I see a good example that demands it. --[[User:Ledrug|Ledrug]] 20:10, 5 May 2012 (UTC)

:::The following graph shows an example where if the path 1 2 5 6 etc is chosen, everything thinks it has reasonable connectivity, but they are kidding themselves. Graphs like this have efficient general solutions, they occur for instance in garbage collectors and glpk.--[[User:Nigel Galloway|Nigel Galloway]] 13:26, 5 May 2012 (UTC)
[[File:Snake2.PNG|centerHidato problem]]
<br clear=both>
Note that that example is trivial for a human. Does anyone have any clever way to deal with such cases? --[[User:Ledrug|Ledrug]] 00:59, 3 May 2012 (UTC)

: The second "sturdy" D version is able to solve the stress (but it's a little slower than the C version because it uses higher level data structures and no pointers, I'll try to speed it up a bit). I think the first "light" D entry breaks on the stress test, so it should be improved by someone.

:: The first simple D version now seems to work correctly.

I suspect that the reason people can solve that problem easily is that they switch strategies easily. Coming up with a way to characterize how to switch is the hard part. (The Tcl code is much more optimized for the case where it's dealing with path sections that are fairly short and which mostly stretch between end points; this is a useful strategy with the majority of Hidato puzzles published in the press, but is far less useful with these evil edge cases.) –[[User:Dkf|Donal Fellows]] 08:07, 3 May 2012 (UTC)

: Heeey, who you callin' evil?
: I was curious if anyone has a good heuristic method that's reasonably simple and works for most cases, like the one used in knight's tour, but I guess it's a bit asking for much.  It's just that a task with only an exponential brute force general solution is somewhat disappointing -- oh well. --[[User:Ledrug|Ledrug]] 11:35, 3 May 2012 (UTC)
:: “Evil” is actually quite complimentary.
:: The reason this is [[wp:NP-complete|hard]] is that it is really the finding of a [[wp:Hamiltonian path|Hamiltonian path]] (on a graph with a bound on the number of links per node); you just add in a (possibly non-planar) link between start and finish to see that this must be the case. I don't really feel like putting lots of effort into cracking very hard problems like this other than by simple brute force. Moreover, because it ''is'' finding a Hamiltonian path, there must be cases which are not easy for either computers or humans to solve. (Do I know what they are? No, but an easy solution to arbitrary Hidato would have tremendous application to many other problems like cryptography.)
:: (I'd have written about this earlier, but there's some kind of weird proxy stopping me from posting to RC from home.) –[[User:Dkf|Donal Fellows]] 16:33, 6 May 2012 (UTC)
::: I'm not sure Hidato is really equiv to a general Hamiltioian path problem -- is it known to be NP-complete?  Hidato is much more constrained in that it has a unique solution requirement; its nodes can have only up to eight neighbors; and more importantly, spatial proximity between nodes are tightly related to the connectivity between them.  Although I do agree that if a problem isn't mathematically well understood (as in, having a known effective algorithm), it's generally not worth putting too much effort into it. --[[User:Ledrug|Ledrug]] 17:49, 6 May 2012 (UTC)
:::: I'm also not ''sure'' that is HP, but the suspicion remains. I couldn't find anything in WP on requirements on connectivity for graphs doing HP; maybe there is something, but I couldn't find it. (Graph theory isn't my specialty, not at all.) –[[User:Dkf|Donal Fellows]] 08:55, 7 May 2012 (UTC)

== Short C version ==
I have a problem with this line of the short C version, maybe there's a small bug:

```c
while (!isspace(*s)) s++;
```

To show it replace that line with:

```c
while (!isspace(*s)) { printf("%u\n", s); s++; }
```

: Right. I though <code>isspace(0)</code> is true; it's not. --[[User:Ledrug|Ledrug]] 07:44, 3 May 2012 (UTC)

== The Snake in the Grass - A case for an Orphaned Cell Supervisor ==

The reason that Evil Case 2 is easy for people is that (most) are capable of learning.

The 'complex applet (should be servlet)in an external website' displays these puzzles on an interesting background, and finally animates a successful solution. Evil Case 2 should be a snake in the grass with a suitably caught rodent having it final journey as the snake's lunch for the successful animation. It should only be attempted on a VERY wide screen monitor.

[[File:Snake.PNG|center|Hidato problem]]

This snake problem comes down to 24 decisions similar to the one above. Suppose the path 1 3 4 5 is attempted. There is then no way back to 2. If your solution can not detect this until it is attempting to use 73 and the only cell you can reach already contains 74 the algorithm is going to be slow.

Obviously most people will detect this on the first corner and realize that every corner is the same.

For the computer cell 2 must complain when it becomes orphaned and prevent that selection asap.
--[[User:Nigel Galloway|Nigel Galloway]] 15:36, 4 May 2012 (UTC)

== On the application of Warnsdorff to Hidato ==

This task's origional premise was that the Knights Tour and Hidato are the same problem requiring only a change of adjacency list.

Normal Hidato are simpler than the Knight's Tour. But considering that Mathprog can solve the Knight's Tour without being commited to Intensive Care for emergency minisat due to induced numerical instability, we must conclude that it is possible to create Hidato's more difficult than Knight's Tours.

If they are the same problem then that which is truth for Knigh't Tour is truth for Hidato. Therfore it seems appropriate to Warnsorff Hidato.

Evil1 and the task's example indicate this does little harm (if only C programmers trained as doctors!). And now we snake it in 16 hunredths of a second.

Finally can we also do the Knight's Tour?

: Warnsdorf is truth for Knight's Tour, but it's not truth for Hidato, hence they are not the same problem (your logic).  Warnsdorff this:
  1  0 -1 -1 -1  0  0 -1 -1 -1  0  0 -1 -1 -1  0  0 -1 -1 -1  0  0 -1 -1 -1  0  0 -1 -1 -1  0  0 -1 -1 -1  0  0 -1 -1 -1  0  0 -1 -1 -1 82
 -1 -1  0 -1  0 -1 -1  0 -1  0 -1 -1  0 -1  0 -1 -1  0 -1  0 -1 -1  0 -1  0 -1 -1  0 -1  0 -1 -1  0 -1  0 -1 -1  0 -1  0 -1 -1  0 -1  0 -1 
 -1  0 -1  0 -1 -1  0 -1  0 -1 -1  0 -1  0 -1 -1  0 -1  0 -1 -1  0 -1  0 -1 -1  0 -1  0 -1 -1  0 -1  0 -1 -1  0 -1  0 -1 -1  0 -1  0 -1 -1 
  0  0  0 -1 -1  0  0  0 -1 -1  0  0  0 -1 -1  0  0  0 -1 -1  0  0  0 -1 -1  0  0  0 -1 -1  0  0  0 -1 -1  0  0  0 -1 -1  0  0  0 -1 -1 -1
:--[[User:Ledrug|Ledrug]] 17:29, 6 May 2012 (UTC)

== On the algebra of Hidato and Knights's Tour ==
To clarify that the problem is mathmatically well understood lets consider Evil case 1. In the general case for

```txt

  a
b c d
e f g

```

The problem is to find a solution for the following simultaneous equations:

```txt

a1 .. g7 are binary variables.

a = a1*1 + a2*2 + a3*3 + a4*4 + a5*5 + a6*6 + a7*7
...
g = g1*1 + g2*2 + g3*3 + g4*4 + g5*5 + g6*6 + g7*7

a1 + a2 + a3 + a4 + a5 + a6 + a7 = 1
...
g1 + g2 + g3 + g4 + g5 + g6 + g7 = 1

a1 + b1 + c1 + d1 + e1 + f1 + g1 = 1
...
a7 + b7 + c7 + d7 + e7 + f7 + g7 = 1

b2 + c2 + d2 = a1
...
b7 + c7 + d7 = a6

a2 + c2 + e2 + f2 = b1
...
a7 + c7 + e7 + f7 = b6

a2 + b2 + d2 + e2 + f2 + g2 = c1
...
a7 + b7 + d7 + e7 + f7 + g7 = c6

a2 + c2 + f2 + g2 = d1
...
a7 + c7 + f7 + g7 = d6

b2 + c2 + f2 = e1
...
b7 + c7 + f7 = e6

b2 + c2 + d2 + e2 + g2 = f1
...
b7 + c7 + d7 + e7 + g7 = f6

c2 + d2 + f2 = g2
...
c7 + d7 + f7 = g6

```

In the particular case:

```txt

  4
b 7 d
1 f g

```

Which for this simple case can be resolved by inspection to:

```txt

b = b2*2 + b3*3
d = d3*3 + d5*5
f = f2*2 + f6*6
g = g3*3 + g6*6

b3 + d3 = 1
b5 + d5 = 1

b2 + f2 = 1
b6 + d6 + f6 + g6 = 1

b3 + b5 = 1
d3 + d5 = 1
f2 + f6 = 1
g6 = 1

```

Putting g6 into the model and resolving again solves the problem by hand.

It is apparent that the number of equations increases rapidly with problem size. A method exists which makes this increase linear see http://en.wikipedia.org/wiki/Tseitin-Transformation.

--[[User:Nigel Galloway|Nigel Galloway]] ([[User talk:Nigel Galloway|talk]]) 14:12, 30 December 2013 (UTC)

: if a1 = 0, the equation b2 + c2 + d2 = a1 means "Non of a's neighbors is 2" which is not necessarily true. so this solution does not work. --[[User:Ak|Ak]] ([[User talk:Ak|talk]]) 17:10, 21 March 2014 (UTC)

==extra credit suggestion==

How about adding an extra credit for this task:  support Numbrix puzzles as well as Hidato puzzles. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 04:50, 18 December 2013 (UTC)

The REXX programming example only needed a couple of '''if''' statements to solve Numbrix puzzles in addition to Hidato puzzles. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 04:50, 18 December 2013 (UTC)

::Two points. First I think it would be better to add a related task Solve Numbrix Puzzles. I know what Numbrix is but only from external servlets and the press. Explaining it to them what be's satisfaction would further complicate this task. Secondly I had an extra credit for showing that the same logic could solve the Knight's Tour, but them what be made me remove it. The Knight's Tour is exactly the same as Hidato, only the Knight has a different view of which cells are adjacent. Numbrix has only 4 adjacent cells whereas Knight's Tour and Hidato have 8.--[[User:Nigel Galloway|Nigel Galloway]] ([[User talk:Nigel Galloway|talk]]) 13:35, 30 December 2013 (UTC)

::: Numbrix puzzles (in my thinking) are just a simpler Hidato puzzle, in the manner of having less possible moves (i.e., being more restrictive).   But I have no qualms of someone creating a Rosetta Code task just for Numbrix puzzles. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 16:20, 30 December 2013 (UTC)

::: As regarding the Knights Tour:  in fact, I used the same code in the REXX example, I just tweaked/optimized the code a bit for speed. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 16:20, 30 December 2013 (UTC)

== solved it using constraint programming ==

Behold: http://hidoku-solver.appspot.com/
