+++
title = "Talk:Cut a rectangle"
description = ""
date = 2011-10-14T12:55:20Z
aliases = []
[extra]
id = 10657
[taxonomies]
categories = []
tags = []
+++

== Zig zags ==

Do we just want straight cuts here, or are zig zags allowed?

 **     ******
 ****     ****
 ******     **

The above example is a rectangle cut with a zig zag path. Note that the resultant shapes are not exactly identical, but are mirrored to each other (when rotated).
[[User:Markhobley|Markhobley]] 18:21, 9 October 2011 (UTC)
:I'd say this is not allowed because of the phrase "along the square edges". --[[User:Mwn3d|Mwn3d]] 18:44, 9 October 2011 (UTC)
::Right. The zig zag cuts are along the square edges. Maybe we should state a single "straight cut", or something like that. [[User:Markhobley|Markhobley]] 19:11, 9 October 2011 (UTC)
:::Oh I see what you're talking about now. Those are allowed. No new terminology is necessary. See the middle cut in the bottom row of the example. It actually matches your example perfectly. I thought you meant cutting a square in half diagonally. --[[User:Mwn3d|Mwn3d]] 19:25, 9 October 2011 (UTC)

== J implementation ==

I am writing this because so far J is the only implementation for this task.  (And this task description was supplied by a non-J programmer -- there is no reason to feel that this task was tailored for J).

Anyways, I thought that perhaps if I showed some of my intermediate results, and described them, it might give some of you something concrete to model your thoughts on, and make other implementations easier.

So:


```j
   init 3 4
0 0 0 0
0 0 0 0
0 0 0 1
```


I start with a rectangle of bits, with one bit set.  If you look at the example solutions, you will see that all of them have one corner that is always in one of the two halves.  It does not really matter which corner you pick for this purpose.


```j
   prop init 3 4
0 0 0 0
0 0 0 1
0 0 0 0
```


Once you have a bit rectangle, I develop a concept of "neighboring bits".  Here, I have represented "unset bits which are immediately above set bits".  Of course, we need to apply that concept in all four rectangular directions (above, below, left, right).  And, we need to enumerate all the possibilities.  I decided to use a list of "location numbers" to represent all the possible places where an unset bit neighbors a set bit.  In other words, given a list of integers filling a rectangle of the desired size:


```j
   i.3 4
0 1  2  3
4 5  6  7
8 9 10 11
```


I can list the bit locations which correspond to a neighboring bit:


```j
   poss init 3 4
7 10
```


And then I discard asymmetric neighbors.  In other words: I remove from the list of neighbor locations for a bit matrix which correspond to the maximum location value minus a location value which is already set in that bit matrix.

Finally, I need to represent the rectangles with these bits set.


```j
   step init 3 4
0 0 0 0
0 0 0 1
0 0 0 1

0 0 0 0
0 0 0 0
0 0 1 1
```


And this gets back to why I was using a bit matrix in the first place -- a bit matrix is a relatively compact representation of an arbitrary way of dividing up a rectangle.

Anyways, once I have this mechanism, I can apply it multiple times (and of course I have code in there to deal with treating each bit matrix independently, and keeping a list of only unique matrices after each step):


```j
step step init 3 4
0 0 0 1
0 0 0 1
0 0 0 1

0 0 0 0
0 0 1 1
0 0 0 1

0 0 0 0
0 0 0 1
0 0 1 1

0 0 0 0
0 0 1 0
0 0 1 1

0 0 0 0
0 0 0 0
0 1 1 1
```


So, how many times do I need to apply this mechanism?


```j
   N init 3 4
5
```


Basically, I count how many bits are in my rectangle, divide that by 2, and then subtract 1.  


```j
   $ step step step step step init 3 4
9 3 4
```


Here's a simpler example:


```j
   N init 2 3
2
   step step init 2 3
0 1 1
0 0 1

0 0 1
0 1 1

0 0 0
1 1 1
```


(Note: the bulk of the following discussion relates to an earlier version of this code.  I've left them here for completeness, but they are only indirectly relevant to the current implementation.)

: Two thoughts:
:* Instead of using a two-state matrix (marked/unmarked), how about using a tri-state one (unmarked/side one/side two)? Every time you mark a cell "side one", also mark its diagonal opposite "side two"; while looking for new neighbors, only pick unmarked cells.  This way, when you finish N/2 steps, you are garanteed a symmetric solution, no need to throw out asymmetric combinations.
:* Instead of searching for cells, directly track the division path ("the cut").  The path must go through the center of the grids; it can only go from one grid point to a neighboring one; it can not go through a grid point twice; starting from the center of the grids (a grid point if m and n are both even, middle of an edge if not), extend the path symmetrically, and as soon as the path reaches any edge of the rectangle, a unique solution is found.  This should be much more efficient, with the catch that visualizing the result is a little harder because it doesn't directly tell you which side of the cut a cell belongs to. --[[User:Ledrug|Ledrug]] 03:38, 13 October 2011 (UTC)
::These sound plausible, though they have a few other potential "gotchas".  
::*The tri-state matrix is going to need a larger data structure to represent it, since it will not allow me to use the bit data type (or I could use a pair of bit matrices, but this would be equivalent to testing for symmetrical overlap at every step).  
::*Also, I believe the division path would require significantly more complex logic (since I need to seek the far diagonal and the division path is variable length).  This winds up being equivalent to maze generation where we generate all possible mazes -- it only seems simple because these sample rectangles are small.
::Anyways thank you for the suggestions -- if we are dealing with large rectangles, I believe the win for testing for symmetry at every step would outweigh its additional computational cost.  And I do not have to use a tri-state representation for that -- I just need to test that the new bit's reflected position does not overlap an existing bit.  --[[User:Rdm|Rdm]] 11:27, 13 October 2011 (UTC)
::: Tracking the path can still be done with your bit matrix, albeit its size should be (m+1) x (n+1) now. The advantage is it will never produce duplicate solutions, and due to symmetry, you'll only need to search in half or a quarter (if m = n) of the solution space. --[[User:Ledrug|Ledrug]] 14:41, 13 October 2011 (UTC)
:::: I've implemented your suggestion of asymmetric neighbor pruning at each step.  I am not quite sure how I would implement the division path approach -- the data structures for that all seem more complex than the bit matrix (and neighbor location) approach I currently use. --[[User:Rdm|Rdm]] 03:22, 14 October 2011 (UTC)
:::: P.S. I know that we do not do timings on this site, but ....  --[[User:Rdm|Rdm]] 11:01, 14 October 2011 (UTC)
::::: Er what? I guess you missed the fact that the first C program doesn't take arguments.  You were looking at the time needed to calculate all size combos below 10.  I changed it here to run 4x3 for a million times, and the run took .4 seconds. --[[User:Ledrug|Ledrug]] 11:13, 14 October 2011 (UTC)
:::::: Oh, oops, yes -- that would explain it.  Thanks.  --[[User:Rdm|Rdm]] 12:55, 14 October 2011 (UTC)
