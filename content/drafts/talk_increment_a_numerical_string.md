+++
title = "Talk:Increment a numerical string"
description = ""
date = 2016-04-24T09:48:21Z
aliases = []
[extra]
id = 7454
[taxonomies]
categories = []
tags = []
+++

== Parsing integers with bases ==

Is there an RC article for this? --[[User:Mcandre]]
:Yes: [[Non-decimal radices/Input]]. The IRC channel would be a better place for questions like this (I saw that you had asked there before...I would have answered but my phone didn't have good reception and you left before I could). --[[User:Mwn3d|Mwn3d]] 04:13, 7 August 2011 (UTC)

== C int2str ==

I removed <tt>int2str</tt> in [http://rosettacode.org/mw/index.php?title=Increment_a_numerical_string&diff=83245&oldid=81783 a recent revision] because of "various bugs". For the record the issues I noticed are:

# The integer input has to be greater than 0, otherwise the output is bogus.
# The occasional use of a static char buffer inside this function is unnecessary and makes it non-threadsafe.
# The result is written starting from the end of the buffer rather than the beginning (unlike <tt>sprintf</tt> and <tt>itoa</tt>).
# The given buffer is assumed to be 32 chars.

There's a standard way to convert integers to strings so I don't think it's necessary to reinvent this wheel. A well-tested <tt>itoa</tt> implementation might still be interesting though. [[User:Rasalas|Rasalas]] 02:28, 31 May 2010 (UTC)

==comments on language comparisons==

I don't believe that the RC code page is the right place to ''compare'' language features (or what some languages have that others don't, specifically: that the PL/I '''SIZE''' condition isn't in the REXX language).  [Unless, of course, of course, that's the task's requirement.] It should be noted that the Regina REXX has implemented the '''LOSTDIGITS''' condition which addresses that problem.  It might take years to list all the language features that REXX doesn't have that other languages have, and, most likely, a fruitless endeavor.  Again, we are talking about skinning a cat in multiple and different ways, and I pity the poor feline.

Your investigation of ''the situation'' would most likely be more germane in a study (task) of what happens when an arithmetic operation (such as integer addition) causes an imprecise (or wrong-appearing) result due to insufficient precision (or scale), and may result in ''rounding'' (or ''truncation'' may be the better word choice in this instance), or the increment (or whatever) is so small that the arithmetic operation is ineffective because of lack of magnitude.   In any case, I believe your investigative results are beyond the scope of this task's requirements which is about ''incrementing a numerical string''. 

Perhaps someone will create a RC task to try to determine the efficacy of various language's mathematical operations. 

On point could/would be:
::*   underflow
::*   overflow
::*   rounding
::*   loss-of-digits
::*   changing the number's format
::*   changing the exponent indicator ('''E''' or '''e''' or '''D''' or '''Q''' ...)
::*   form of exponentiation (engineering/scientific/other)
::*   normalization
::*   scaling
::*   leading zeroes
::*   trailing zeroes
::*   leading blanks
::*   trailing blanks
::*   imbedded blanks
::*   superfluous signs
::*   etc.  

Many languages don't have this problem as almost all languages don't store numbers as strings (as REXX does). 

Furthermore, ooRexx examples should be entered in the ooRexx section, not (classic) REXX. -- [[User:Gerard Schildberger|Gerard Schildberger]] 00:41, 12 December 2012 (UTC)
