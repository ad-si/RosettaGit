+++
title = "Talk:Subset sum problem"
description = ""
date = 2012-05-29T05:44:55Z
aliases = []
[extra]
id = 11130
[taxonomies]
categories = []
tags = []
+++

I ran into some unanticipated implementation issues the last time I tried to create a task, so I'd like to request feedback on this one before promoting it even though it seems straightforward enough to me.

[[User:Sluggo|Sluggo]] 15:58, 1 January 2012 (UTC)

: 1) Why the words?  2) Caution about trying all subsets being "infeasible" seems superfulous. The problem is NP-complete, so for any algorithm, it's easy enough to construct a data set that will run (practically) forever or exhaust memory. --[[User:Ledrug|Ledrug]] 08:16, 2 January 2012 (UTC)

: Thank you for your comments. My rationale is as follows. 1) Any practical application in which this calculation is needed would probably pertain to something more than just integers (e.g., the Dropbox candidate screening exercise). The words are there to stand in for whatever the integers might represent, and thereby to allow other programming language features than arithmetic to be demonstrated in the solutions. 2) Although this is an NP complete problem, an efficient implementation is possible in practice when the weights are restricted to a manageable range as stipulated (e.g., -1000 to 1000). One only needs a bit vector with an entry for each possible sum, and a linear time traversal of the word list. I wanted to make it clear that an alternative to the brute force solution exists and is appropriate. --[[User:Sluggo|Sluggo]] 01:05, 3 January 2012 (UTC)

==how many solutions?==

I was wondering how many solutions there are for the sample names/weights shown, so I ran my REXX program with a "stop" value of 32,000.  The REXX program found that many results.

Does anyone have a fast program that could the find exact number of results (whose weights add up to zero)? 

Since REXX is an interpretive language, it's not exactly a speed demon for these types of number crunching. -- [[User:Gerard Schildberger|Gerard Schildberger]] 18:03, 3 May 2012 (UTC)

: Apparently there are 349167 combinations of zero sum.  Considering subset sum problem is about deciding whether any combination exists at all, that does seem a little high as far as designing a task is concerned: making solutions a dime a dozen doesn't motivate people to use proper methods a difficult task deserves. --[[User:Ledrug|Ledrug]] 20:11, 3 May 2012 (UTC)

:: Uf-ta.  I was going to put in some optimizations into the REXX program, but with over 1/3 million subsets (for solutions), I'm not going to bother. -- [[User:Gerard Schildberger|Gerard Schildberger]] 22:08, 3 May 2012 (UTC)

:: Well, I decided to go ahead and optimize the REXX program, and it's twice as fast.  After a day of thinking, I again made it twice as fast.  The brute force solution may not be appropriate (says Sluggo), but it's doable. -- [[User:Gerard Schildberger|Gerard Schildberger]] 03:51, 8 May 2012 (UTC)

::: Huh, "doable" is such a subjective thing.  Suppose the thirty one numbers given in the task were instead these:<lang>-61 1 32 373 311 249 311 32 -92 -185 -433 -402 -247 156 125 249 32 -464 -278 218 32 -123 -216 373 -185 -402 156 -402 -61 -31 902
```

::: would it still be doable? --[[User:Ledrug|Ledrug]] 04:42, 8 May 2012 (UTC)

:::: Yes  (apart from the fact that there would be less solutions listed, but that computing time isn't much compared to the summing of the weights for the various combinations).  The REXX solution I coded doesn't care what the weights are, it still adds them together to see if they sum to zero (actually, some particular target in this case which just happens to be zero).  I was thinking about added some optimization --- sorting the weights in ascending order, and then taking advantage of the fact that if the sum gets "too large", the rest of the weights need not be summed, and also eliminating any outlier weights, but I didn't.  As it turns out, their are no outlier weights, but still, the code couldn've been added. -- [[User:Gerard Schildberger|Gerard Schildberger]] 05:13, 8 May 2012 (UTC)

::::: Er no, try the example above, and see how long it takes.  I'm rather curious where you draw the line between "doable" and "not". --[[User:Ledrug|Ledrug]] 05:32, 8 May 2012 (UTC)

:::::: It takes as long as the original set of weights (to complete).  This is how the REXX program works.  Even though takes longer to find the first solution (if any), that doesn't mean that it takes a longer time to complete.  As for my definition of "doable", to me means that the program can complete is a reasonable time, and "reasonable" depends just how badly I want the results (output).  I've been running a program (not related to this) for over five years now because I want the output.  For something this trivial (the summing of weights, albeit it consumes a lot of computer time), I won't bother.  I'm using my fast computer for other things.  And my main computer that I connect to the internet is just too slow (and I hate to say what century I built it). -- [[User:Gerard Schildberger|Gerard Schildberger]] 06:42, 8 May 2012 (UTC)

::::::: ''It takes as long as the original set of weights (to complete)''
::::::: Does it? Rexx works in mysterious ways.  Anyhow, I can't verify that myself, as regina-rexx complains about something like <code>sh: 1: COMBN: not found</code> when trying to run your code.  One thing though, I wouldn't call subset sum problem "something this trivial", but to each his own. --[[User:Ledrug|Ledrug]] 07:34, 8 May 2012 (UTC)

:::::::: The lack of a "combN" subroutine (PROCEDURE statement) is my mistake, I "lost" it when doing multiple cut & pastes.  It's been re-inserted.  Normally, I cut & paste the whole program when updating it, but I was taking shortcuts and only cut & pasted the updated subroutine, but I missed the first statement of the PROCEDURE.  Because of the header comment, it looked normal at first glance.  I'm sorry about the mishap -- too much late, late night programing, or rather, early morning programming.  I hope I didn't offend your sensabilities about what I consider a trival program.  The REXX solution (originally) took only a matter of minutes to write, and coupled with the shortness of it, it was a trivial program.  I didn't mean to imply that the importance of the task was trivial. -- [[User:Gerard Schildberger|Gerard Schildberger]] 16:50, 8 May 2012 (UTC)

:::::::: Come to think of it, did the interpreter just call the shell when it couldn't resolve "combN"? That's seriously naughty. --[[User:Ledrug|Ledrug]] 07:42, 8 May 2012 (UTC)

::::::::: Yes, that's the way REXX works.  Any function/subroutine not in the program, REXX just looks elsewhere.  Normally, it would be another REXX program in a common repository (such as a folder) for such things.  There's nothing in the language to distinguish a subroutine that could be written in REXX, assembler, Java, etc.  This is due to the environment that REXX was developed in, where any program can invoke another program, no matter what language it was written in (where program invocation had a common linkage).  This is a mechanism to force REXX to look outside the program for subroutine execution by inclosing the subroutine name in quotes or apostrophes. As an aside, someone has written an interface for REXX to call (use) any Java subroutines (I'm not sure what Java calls such stuff). It's the writing of the linkage (interface) that's tricky. REXX has the same difficulity as PL/I does, it's variable types are, for a lack of a better word, so non-traditional that PL/I had to resort to something called ''dope vectors'' to describe the variable types. I think the closest thing I could call REXX variables would be ''non-null terminated character strings''. -- [[User:Gerard Schildberger|Gerard Schildberger]] 16:50, 8 May 2012 (UTC)

:: After saying I wasn't going to optimize the REXX program (two versions are now on RC), I kept thinking about the program and eventually made it over a magnitude times faster. -- [[User:Gerard Schildberger|Gerard Schildberger]] 23:59, 20 May 2012 (UTC)

:: After upgrading the 2nd version of the REXX program that sorted the names by their weights (and realizing the order of magnitude improvement), it seemed rediculus to keep the much slower version around, so it was deleted and the 2nd version was enhanced to sort the output which presents the names in alphabetical order, thus preserving the original order of the names.  Much more optimization could be made, but I'm not sure the effort would be worthwhile.  I may come back to this program after my other computer is freed up from its heavy usage. -- [[User:Gerard Schildberger|Gerard Schildberger]] 05:44, 29 May 2012 (UTC)
