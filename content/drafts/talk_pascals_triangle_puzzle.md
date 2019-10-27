+++
title = "Talk:Pascal's triangle/Puzzle"
description = ""
date = 2011-10-19T19:30:57Z
aliases = []
[extra]
id = 7780
[taxonomies]
categories = []
tags = []
+++

==Problem with Python csp library==
The downloaded library relied on a utils.unique() function which is not part of the utils standard library. I had to edit the source of csp.py to use set() instead. --[[User:Paddy3118|Paddy3118]] 17:18, 19 July 2010 (UTC)

== Is this cheating? ==

Is it ok to work out intermediate equations yourself and input those instead of the pyramid? --[[User:Mwn3d|Mwn3d]] 21:28, 1 December 2010 (UTC)
: See rationalization of [[100 doors]]? --[[User:Short Circuit|Michael Mol]] 21:50, 1 December 2010 (UTC)

== The Go rant ==

The first Go solution lists a blank program after much protesting, saying that this problem is easily solved by hand thus not worth programming for.  It ignored the fact that a human with a pencil and stack of paper is (more than) Turing complete, thus by the same logic nothing is ever worthy of a program.  I think it's really uncalled for. --[[User:Ledrug|Ledrug]] 06:20, 20 July 2011 (UTC)

:It does look like the Go author should have discussed the quality of the task here first rather than on the task page. --[[User:Paddy3118|Paddy3118]] 08:49, 20 July 2011 (UTC)
:: Concur. And it does raise a fair point about the task description. The task should probably more clearly request a solver for arbitrary missing elements of Pascal's triangle (Granted, the simplest solution is to generate Pascal's Triangle for as many rows necessary, and fill in the missing bits. I don't want to say that's too simple to be interesting; for some people, that's an interesting problem to tackle. For others, it's going to be too trivial. ) --[[User:Short Circuit|Michael Mol]] 11:29, 20 July 2011 (UTC)
:::The rant, moved here from the task page:  &mdash;[[User:Sonia|Sonia]] 19:30, 19 October 2011 (UTC)
The following solution is based on several observations on the task:

* The task does not ask for solutions to any generalization of the problem, only this one problem.
* The (twice) linked reference in the task description similarly does not describe any generalizations, but only this one problem.
* The talk page notes that intermediate work need not be coded, and indeed, a number of existing solutions do intermediate work.
* The entire problem is solvable with elementary algebra, thus a decision to actually code any part of the problem is arbitrary.
* Any part coded is not only done frivolously, but represents unnecessary chances for errors.
* Skills needed to develop this solution are prerequisite to any other solution.  No other solution is easier.
* The task does not specify program output.  This program provides the solution to the problem in a form that is clear to anyone wishing to further adapt the program to their needs.

```go
package main

func main() {
    // bottom row given:   [X]    [11]    [Y]    [4]    [Z]

    // given sum relation of bricks,
    // next row up:          [x+11]  [y+11]  [y+4]  [z+4]
    // middle row:             [x+y+22] [2y+15] [y+z+8]

    // given brick=40 and relation y=x+z,
    // middle row:               [40]   [2y+15] [3y-10]

    // continuing sum relation of bricks,
    // next row up:                 [2y+55] [5y+5]
    // top brick:                       [7y+60]

    // given top brick = 151,
    // 7y = 91:     y = 13
    // x + y = 18:   x = 5
    // z = y - x:    z = 8
}
```

