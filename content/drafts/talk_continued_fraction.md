+++
title = "Talk:Continued fraction"
description = ""
date = 2016-04-16T05:05:51Z
aliases = []
[extra]
id = 11447
[taxonomies]
categories = []
tags = []
+++

== Task name ==

Should the task name be Generalized continued fractions?  Both Mathworld and Wikipedia note that these is more properly called generalized continued fractions and that the term continued fraction is most often used for simple continued fractions. &mdash;[[User:Sonia|Sonia]] 21:41, 27 February 2012 (UTC)

:In principal Continued Fractions are generalized in the sense that they work for all numbers. In computer terms it is often required to handle Integers, Rationals, Reals, and Complex Numbers seperately. For this task it is sufficent for cf to work with Integers. A solution which switches between Integer and Floating Point will not be able to generate the exact solution that for instance the Python and Haskell solutions do. Having to implement cf for all types of number would make the task more time consuming and detract from the structure of the solution: a CF object; generators for aN and bN; and translation from the exact CF object to a real number.--[[User:Nigel Galloway|Nigel Galloway]] 16:11, 29 February 2012 (UTC)

== Subscript convention ==

Both Mathworld and Wikipedia use a subscript convention different that what is used in the task description.  It might be best to match their convention. &mdash;[[User:Sonia|Sonia]] 21:43, 27 February 2012 (UTC)

== Related tasks ==


### Thiele interpolation

One might be [[Thiele's interpolation formula]]. &mdash;[[User:Sonia|Sonia]] 21:59, 27 February 2012 (UTC)

:When the structure of the solution: a CF object; generators for aN and bN; and translation from the exact CF object to a real number, is established then we can tackle the Thiele task properly. Note that the cf representation for: arcsin is http://www4b.wolframalpha.com/input/?i=continued%20fraction%20arcsin%28x%29&lk=2; arccos is http://www.wolframalpha.com/input/?i=arcos%28z%29+continued+fraction+representations; and arctan is http://www4b.wolframalpha.com/input/?i=continued%20fraction%20arctan%28x%29&lk=2.


### Moebius function


I recently read an interesting paper about nested datasets and continued fractions.  One of the useful thing that can be done in this regard is to turn a continued fraction into a Moebius function:

http://www.sigmod.org/sigmod/record/issues/0506/p47-article-tropashko.pdf

:<math>a_0 + \cfrac{b_1}{a_1 + \cfrac{b_2}{a_2 + \cfrac{b_3}{a_3 + x}}} = \frac{A_3+B_3x}{C_3+D_3x}</math>

The purpose would be to compute <math>A_n</math>, <math>D_n</math>, <math>C_n</math> and <math>D_n</math> for any n.--[[User:Grondilu|Grondilu]] 00:55, 16 October 2012 (UTC)

As I think about it, I realize there is a recursive property:
:<math>M_n(x) = M_{n-1}(\frac{b_n}{a_n+x})</math>
That should make things much easier than doing the whole algebraic development.
--[[User:Grondilu|Grondilu]] 02:00, 16 October 2012 (UTC)
:Thanks for the reference, using a continued fraction as a sort of index into a tree structure is interesting. Do you intend to implement it?--[[User:Nigel Galloway|Nigel Galloway]] 11:52, 23 October 2012 (UTC)
::I tried with one of my projects [http://github.com/grondilu/libbitcoin-perl on github] but I encountered a few technical difficulties so I put it on hold.  I'll go back to it someday.--[[User:Grondilu|Grondilu]] 14:38, 4 November 2012 (UTC)

== creating a continued fraction ==

This would be a good tie-in to another (as yet, unmentioned) Rosetta Code task:  generate a continued fraction. -- [[User:Gerard Schildberger|Gerard Schildberger]] 20:29, 24 March 2012 (UTC)

== expressing as a true fraction == 

By the way, it doesn't take that much to add some code to the subroutine to return the result at a true fraction, i.e.:

cff(0 3 1 1 1 2) 

would return 

8/29  

 (that is, the four characters "8/29".  

cff(9 2 2) 

 would return 47/5


This necessiitated (for the REXX subroutine) in writing a subroutine to handle arithmetic [+ - * ^ ÷ // %] functions 

on/for true fractions (including whole numbers and improper fractions, including negative fractions),

which would also be a good task for Rosetta Code. -- [[User:Gerard Schildberger|Gerard Schildberger]] 21:03, 24 March 2012 (UTC)

== Gold Credit for pi and The Harmonic Series ==
REXX has already claimed all the extra credits for this task, however for those implementations which can accept a fractional a_series a Gold Credit has been found for demonstrating the relationship between pi and The Harmonic Series:

:<math>\pi/2 = 1 + \cfrac{1}{1 + \cfrac{1}{1/2 + \cfrac{1}{1/3+ \ddots}}}</math>

This was published in American Mathematical Monthly, December 2008 by Dr. Tom Picket, an associate Professor in the Physics Faculty of The University of Southern Indiana.--[[User:Nigel Galloway|Nigel Galloway]] 12:49, 12 September 2012 (UTC)

== Arithmetics?? ==


It's quite complicated, but it can be done.   See [http://perl.plover.com/yak/cftalk/ this talk] for instance.  A nice library which would hide all the gears behind it would be great.  I've never seen any.--[[User:Grondilu|Grondilu]] 00:05, 22 November 2012 (UTC)

== Alternative Perl6 ==

Great to see another view. Follows a screenshot of the code in my browser:

[[File:Perl6.PNG|center|Perl6]]

Is the strange symbol after infix and line 3 an error with the upload, or do I need a special font?--[[User:Nigel Galloway|Nigel Galloway]] 12:57, 3 January 2013 (UTC)
:It's supposed to be the composition operator <math>\circ</math>, which I copied from [[First-class functions#Perl 6]].  It should be a utf8 grapheme.  You can replace it with a simple "o".--[[User:Grondilu|Grondilu]] 13:13, 3 January 2013 (UTC)

: A good unicode font will have it. I believe it's part of the set of symbols that were originally in the Symbol font, or maybe one of the *dings fonts. Unicode's full of interesting stuff! (Pity that support for it is still distinctly patchy.) –[[User:Dkf|Donal Fellows]] 15:15, 3 January 2013 (UTC)
