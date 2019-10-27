+++
title = "Talk:Knight's tour"
description = ""
date = 2019-08-29T13:38:48Z
aliases = []
[extra]
id = 9803
[taxonomies]
categories = []
tags = []
+++

== solutions found fast ==
Wow, this was found fast.  I was still prepping my first couple implementations.:)
--[[User:Markjreed|Markjreed]] 02:09, 30 May 2011 (UTC)

Added my original perl solution and sample output; moved out of draft status.
--[[User:Markjreed|Markjreed]] 02:48, 30 May 2011 (UTC)

== References ==
[[wp:Knight%27s_tour]]

The following had more than I needed to know about the problem [http://www.cs.cmu.edu/~sganzfri/REUPaper.pdf A Simple Algorithm for Knight’s Tours] by Sam Ganzfried. --[[User:Paddy3118|Paddy3118]] 07:22, 29 May 2011 (UTC)
:::::::   (above) broken link.     -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 20:56, 22 July 2019 (UTC)
:::::::   (above) referenced document can be found here: [http://math.oregonstate.edu/~math_reu/proceedings/REU_Proceedings/Proceedings2004/2004Ganzfried.pdf A Simple Algorithm for Knight’s Tours by Sam Ganzfried] 


I discovered this weekend that Warnsdorff sometimes generates incomplete tours.  This is discussed in Granzfried (above) and also in Mordecki.
* [http://www.cmat.edu.uy/~mordecki/articles/warnsdorff.pdf Counting Knight's Tours through the Randomized Warnsdor� Rule, Cancela & Mordecki, 2006] --[[User:Dgamey|Dgamey]] 11:05, 30 May 2011 (UTC)
*  [ftp://reports.stanford.edu/pub/cstr/reports/cs/tr/74/442/CS-TR-74-442.pdf Estimating the efficiency of backtrack programs, Knuth, 1974] --[[User:Dgamey|Dgamey]] 11:36, 30 May 2011 (UTC)
*  [http://faculty.olin.edu/~sadams/DM/ktpaper.pdf Knight's Tours, Hill & Tostado, 2004] --[[User:Dgamey|Dgamey]] 19:56, 30 May 2011 (UTC)

I found some additional references that may be of interest including a genetic algorithm, called Ant Colony.

* http://www.markkeen.com/knight/index.html 
* http://www.cs.nott.ac.uk/~gxk/papers/phai04.pdf  Ant Colony Algorithm 
* http://arxiv.org/abs/arXiv:0803.4321
--[[User:Dgamey|Dgamey]] 10:10, 2 June 2011 (UTC)

== Incomplete Tours and Warnsdorff ==
While implementing the Icon/Unicon version over the weekend, I noticed that the algorithm frequently produced incomplete or dead-ended tours.  A little investigation proved insightful:
*  backtracking unless it can be limited dramatically will be inefficient (Knuth)
*  the order the Knight's moves are presented for selection (beyond accessibility) appears to have an effect (Granzfried)
*  the algorithm can be probabilistically tuned (Mordecki)
*  most papers investigating Warnsdorff indicate that it is imperfect

I haven't measured the hit/miss rate of the Unicon version of the algorithm.  There are some aspects that differ from other implementations posted that may be responsible. It would be interesting to compare completion rates of the various implementations. Possible areas of consideration/cause for variance in efficiency:
*  using ordered sets to resolve tie breakers.  The method (in Unicon) used to generate the 8 possible Knight moves is different and other implementations tend to present them in a given order.

--[[User:Dgamey|Dgamey]] 13:21, 30 May 2011 (UTC)

:I did implement Roths extension, which is to break ties by chosing the point furthest from the centre:
:
```python
def accessibility(board, P, boardsize=boardsize, roths_extension=True):
    access = []
    brd = copy.deepcopy(board)
    for pos in knightmoves(board, P, boardsize=boardsize):
        brd[pos] = -1
        if not roths_extension:
            access.append( ( len(knightmoves(brd, pos, boardsize=boardsize)),
                             pos ) )
        else:
            access.append( ((len(knightmoves(brd, pos, boardsize=boardsize)),
                             -(complex(*pos) - (boardsize/2. + boardsize/2.j)).__abs__()),
                            pos) )
        brd[pos] = 0
    return access
```

:I didn't really have time to measure its effectiveness though. --[[User:Paddy3118|Paddy3118]] 13:27, 30 May 2011 (UTC)

:The Perl solution is deterministic, and produces complete tours for all 64 start squares. So it must be something in the ordering of the move list. --[[User:Markjreed|Markjreed]] 01:54, 31 May 2011 (UTC)


###  The 7x7 problem 

:Try a 7x7 board. --[[User:Paddy3118|Paddy3118]] 03:24, 31 May 2011 (UTC)
:: I will.  At this point there is something wrong with this solution.  I will have to come back to it later (possibly this weekend).  This was implemented from the WP description which talks about sets.  I think the heart of the problem lies in the used of unordered sets everywhere and I will have to walk through some comparative examples side by side.  This wouldn't be the first time I've been burned by a vague WP algorithm description.  I'll have to borrow one of the other solutions and add code to show me the order squares get presented.  --[[User:Dgamey|Dgamey]] 09:48, 31 May 2011 (UTC)
::: Found it, I got a bit too clever pre-populating things.  I'll post an adjustment, probably late on the weekend.  --[[User:Dgamey|Dgamey]] 13:40, 1 June 2011 (UTC)
:::: Fixed it, will re-post code soon. It seems to work 100% on 8x8. --[[User:Dgamey|Dgamey]] 10:35, 2 June 2011 (UTC)
:: Confirmed that I sometimes get incomplete tours on 7x7 with Warnsdorff using the move list order in the Perl code. Leaving the any-size-board generalization out of code on page until I handle that case.  --[[User:Markjreed|Markjreed]] 12:47, 31 May 2011 (UTC)

::: I believe that you've got to filter the output of the Warnsdorff algorithm in case it fails to do a tour. (That is, apply the “if at first you don't succeed” principle.) I don't know what the probability of failure is though. You might want to add the checks for impossibility (see the WP page) to your code though. –[[User:Dkf|Donal Fellows]] 13:50, 31 May 2011 (UTC)

:::: The probability of failure depends on the board size and the tiebreak rule (move consideration order, for a first- or last-wins algorithm); for random move selection, it's about 25% on a 7x7 board.  The order that I picked happens to work 100% of the time for an 8x8 board, but a general solution requires a more complex algorithm.  The Ganzfried paper cited above includes one such, Squirrel's algorithm, which adjusts the ordering of the moves after certain landmarks in the progress of the tour. --[[User:Markjreed|Markjreed]] 02:29, 1 June 2011 (UTC)

::::: It should be 100% rate of failure on a 7x7 board! The issue is that a true tour is one that's a circuit that goes back to the initial position. If there's an odd number of squares, a simple parity argument shows that there cannot be solution (there must be different numbers of white and black squares, so a return to the initial location would require a parity violation when completing the loop). Sorry to be so awkward, but this problem's actually very well defined in this area… –[[User:Dkf|Donal Fellows]] 14:12, 2 June 2011 (UTC)

:::::: The way I read the reference documentation, the tour just had to cover every square. Ending on a square that was a nights-move away from the start point was an (interesting), extra requirement. --[[User:Paddy3118|Paddy3118]] 15:27, 2 June 2011 (UTC)

:::::: Donal, there are open and closed tours.  You are referring to ''closed'' tours.  There are no closed tours on boards of size 2 through 7 inclusive.  --[[User:Dgamey|Dgamey]] 02:43, 3 June 2011 (UTC)

:: I started to measure success/failure on a 7x7 using different tie breakers.  Starting with a triangle of squares that provide a minimum under rotation & reflection it really looks like starting position is the predominant factor.  I haven't yet tried all cases just in case rotation/reflection does affect the results.

:: This shows a summary of results for 5 tries at each starting position:

```txt
Results of tests for N=7 :
               Starting Square |       a1      a2      a3      a4      b2      b3      b4      c3      c4      d4   *All*
                     * Count * |      40      40      40      40      40      40      40      40      40      40     400 
                     * Total * |   80.00%   0.00%  70.00%   0.00%  80.00%   0.00%  62.50%  92.50%   0.00% 100.00%  48.50%
           ExperimentalBreaker |   80.00%   0.00%  70.00%   0.00%  80.00%   0.00%  50.00%  90.00%   0.00% 100.00%  47.00%
               FirstTieBreaker |   80.00%   0.00%  70.00%   0.00%  70.00%   0.00%  70.00% 100.00%   0.00% 100.00%  49.00%
              RandomTieBreaker |   90.00%   0.00%  70.00%   0.00%  80.00%   0.00%  50.00%  90.00%   0.00% 100.00%  48.00%
                RothTieBreaker |   70.00%   0.00%  70.00%   0.00%  90.00%   0.00%  80.00%  90.00%   0.00% 100.00%  50.00%
```


:: The notable thing about the pattern of failure in 7x7 is that tours started every other square fail and this shifts by one every rank.  The symmetries of the squares above hold for all tie breakers and the overall pattern of failure is a cross-hatching.  Like this:

```txt
      a b c d e f g
    +---------------+
  7 | T - T - T - T |  7
  6 | - T - T - T - |  6
  5 | T - T - T - T |  5
  4 | - T - T - T - |  4
  3 | T - T - T - T |  3
  2 | - T - T - T - |  2
  1 | T - T - T - T |  1
    +---------------+
      a b c d e f g
```

::: Where T indicates that Warndsdorf/Roth found a tour and - indicates a failure to find a tour. A quick estimate of the number of paths to be test for an exhaustive search confirmed that would be impossible.  I tried a number of searches to find references to unsolvable knights tours on 7x7 boards and found none.  I find myself wonder if there are any solutions on any of those failed squares.  --[[User:Dgamey|Dgamey]] 10:59, 6 June 2011 (UTC) 
 
::: Running tours for all squares looking at the failed 7x7 start at a2 running 48 moves with a3 empty and all symmetries found no reverse paths either.  --[[User:Dgamey|Dgamey]] 10:56, 3 June 2011 (UTC)

:: Looking at two cases where the start was a1 and a3, the a1 failed and a3 start did not (Random Tie Breaker). Both case went through a1 but the one starting in a3 went through a1 on move 37.  Looking at the ties, there was no obvious choice that would have produced a tour.  The start in a1 would have had to violate the accessibility filter to succeed.  That is a1, b3, c1, a2, b4, ... fails vs. a1, b3, c1, a2, c3, ... succeeds.  In the later case c3 was chosen not from the ties but from the group with the highest accessibility.  I'll have to dig into this a bit more later.  What I did add was a log like this showing the move, minimal accessibility, and moves in that group.  This is for the failing a1:

```txt
Debug log : move#, move : (accessibility) choices
1. a1 : (5)  b3 c2 
2. b3 : (3)  c1 a5 
3. c1 : (2)  a2 
4. a2 : (5)  b4 
5. b4 : (2)  a6
```


:: I'd be interested in seeing how others fare with a 7x7.  --[[User:Dgamey|Dgamey]] 10:28, 2 June 2011 (UTC)

:::J:
```j
   ktourw 7
 0 21 32 41 14 19 16
31 48 23 20 17 40 13
22  1 44 33 42 15 18
45 30 47 24 37 12 39
 2 27 36 43 34  9  6
29 46 25  4  7 38 11
26  3 28 35 10  5  8
```
 --[[User:Rdm|Rdm]] 12:54, 3 June 2011 (UTC)

:::: Thanks.  How are you breaking ties?  Roth?  (Also can you find a solution on 7x7 starting on a2? see above.  --[[User:Dgamey|Dgamey]] 13:23, 3 June 2011 (UTC)
::::: I did not write this code.  It is using the lowest index in the case of a tie.  And this version failed when I tried to get it to find a 7x7 solution starting on a2 (index 1). --[[User:Rdm|Rdm]] 13:55, 3 June 2011 (UTC)

== C++ formatting ==

I edited the C++ to remove some of the changes made. Several are fairly pointless (such as adding const to all the local variables; the compiler is smart enough to figure that out if it even makes a difference), and most were just reformatting it to someone else's formatting preferences (++i vs i++, etc). If there were a sitewide C++ formatting guide (a possibility suggested on the C++ discussion page, but not currently implemented) I (or whoever) can format it to fit that.  [[User:MagiMaster|MagiMaster]] 03:43, 31 May 2011 (UTC)
: (Note: I didn't write the C++ code.) For trivial cases, const on local variables is unnecessary, but I'm not sure it was worth removing. Using const when a variable ''shouldn't'' change is often useful to avoid accidental mutation. (For similar reasons, I tend to write if statements as if(constval '''cmp''' nonconst)). Also, the compiler can't assume a local variable can be treated as const unless it can prove so, which goes out the window if a reference (or pointer-to) the variable is passed non-const to a function outside the current linkable object (the compiler can't know that the called function won't mutate the variable), and may also be very difficult under some local non-trivial operations involving its address or created references.
:
: A perfect compiler which took all logic optimizations to their every possible conclusion could be presumed to be able to treat non-const local variables as const (as long as pointers and references didn't go outside the current linkable object), but for any other compiler it's polite to drop hints.
:
: I do like the rest of the changes, though. --[[User:Short Circuit|Michael Mol]] 16:20, 31 May 2011 (UTC)

:: I wrote the original C++ code, and someone that didn't log in or sign edited it. Anyway, if there's a reason for it, I can put the const's back. [[User:MagiMaster|MagiMaster]] 19:25, 31 May 2011 (UTC)
::: I didn't make the anon edits. I'm just pointing out the distinction between 'unnecessary' and 'pointless'. I also felt a need to step in, because I haven't seen edit wars on RC, and it felt like we might be getting close to such over stylistic issues. So I simply stated my opinion and left it at that; I don't need to be a direct party to an edit war, and such things have traditionally been resolved on RC by talking over the pros and cons. C++ is going to be a tricky language for me to not interfere with, because it's my bread and butter... --[[User:Short Circuit|Michael Mol]] 20:11, 31 May 2011 (UTC)

::::Maybe if people tried not to do both 'stylistic' changes and fix coding errors in the one edit, then you could concentrate on arguing the merits of stylistic changes within their given context. SOmeone may be able to argue that for the given program, its's not gonna make much difference. On the problem of people not having an account before editing, then maybe a note asking them politely to properly join in the the discussion by giving you a name to converse with might be all it takes? (I, have been known to play fast-n-loose with Pythons 80 char lines rule). --[[User:Paddy3118|Paddy3118]] 05:38, 1 June 2011 (UTC)

:::: I wasn't accusing anyone of anything, just stating why I'd done what I'd done. I don't want to see an edit-war either, so if it happens again, my first suggestion would be a community coding style. I know what my preferences are, but when I teach programming, I stress consistency over any one style, so I have no problem with the idea. (I'm just a TA, so don't take that as me trying to lend weight to my arguments.) If not a style guide, my second suggestion would be to not make purely stylistic changes to existing code. [[User:MagiMaster|MagiMaster]] 03:21, 2 June 2011 (UTC)
::::: Sorry if I sounded a bit short; most of my wiki edits are in a few spare minutes here and there, so after a few rounds of quick edits and rewrites, my prose can ultimately sound worse than it started... I'm familiar with the TA role; [[User:Mwn3d|Mwn3d]] is a former TA from RIT, and has been heavily involved with RC for three or four years, now. I think he suggested elsewhere following the model of [[J/HouseStyle]], as far as developing a community style. I'm beginning to see a need for that, I suppose; better to have the debate and discussion over style in one place, than in dozens of places. I suppose the C++ page would be at [[C++/HouseStyle]] --[[User:Short Circuit|Michael Mol]] 15:30, 2 June 2011 (UTC)

==Tiling smaller tours==
[https://gilith.wordpress.com/2013/02/20/large-knights-tours/ This] blog post by Joe Leslie-Hurd has a novel technique of stitching together smaller tours to make larger ones. Enjoy. --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 06:28, 21 July 2015 (UTC)
