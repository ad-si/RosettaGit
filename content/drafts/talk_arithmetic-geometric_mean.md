+++
title = "Talk:Arithmetic-geometric mean"
description = ""
date = 2018-01-08T02:59:03Z
aliases = []
[extra]
id = 11378
[taxonomies]
categories = []
tags = []
+++

==Trivial solution==
Asking to compute a single number without specifying the algorithm invites solutions like PRINT .85.  Perhaps we want to specify the algorithm, or specify to write some general code that will compute agm for all values of a an b.  Is there something special about the function applied to these particular numbers?  The C++ example computes many digits of precision.  Is this relevant?  The Mathworld page is full of interesting stuff about the function.  Perhaps we could add a little more interest to the task by taking something from there.  &mdash;[[User:Sonia|Sonia]] 19:58, 6 February 2012 (UTC)
: It's short but not quite trivial. A trivial solution doesn't work with IEEE doubles as it doesn't manage to get the final ULP to converge (see my notes with the Tcl solution). Still, it's a very rapidly converging function and not difficult to write provided you're not looking for more than about 15 decimal places of precision. I have yet to be convinced that it is worth trying for more accuracy than that (especially as that would require using non-builtin arithmetic). –[[User:Dkf|Donal Fellows]] 09:54, 7 February 2012 (UTC)

:1 / agm(1,sqrt(2)) is Gauss's Constant. Any constant named after someone is important. A constant named after Gauss is the most important constant in the world. If we look at (16) in the reference we can see that it is Pi in disguise. The precision with which you can calculate Pi is then limited by the precision to which you can calculate this, if Pi to 10 decimal places is good enough then okay. It is not necessary to calculate this using the algorithm used in the C++ example. Here Gauss' Constant is calculated to 20,000 places 1 digit at a time http://oeis.org/A014549/b014549.txt. --[[User:Nigel Galloway|Nigel Galloway]] 14:15, 7 February 2012 (UTC)

:: That was one of the most amazing paragraph to have ever graced RC, where except the first and the last, every sentence is wrong to some extent, and it was written sincerely (as far as I can tell).  For one thing, who's to rate which constant is more important than others? π is named after Archimedes, e is named after Euler, not to mention stuff like 0 an 1 are so fundamental that they didn't even get the chance to be named after anybody.  And how is Gauss's Constant "π in disguise"?  Since we don't know if π and e are independent, your (16) in the reference isn't really all that helpful in establishing a crucial connection between π and Gauss's Constant, so it's not sensical to say "precision with which you can calculate Pi is then limited by ... this".  It's fine to have a task to calculate the constant, but you don't need to hype it up all over the place. --[[User:Ledrug|Ledrug]] 17:59, 7 February 2012 (UTC)

:::Thank you for your interest, but why so angry? I was asked why 1 and 1/sqrt(2) and why so many digits? The answer is my sincere reason for choosing those numbers and so many digits. Surely π and e are related by Eulers formula (see:http://mathworld.wolfram.com/EulerFormula.html) which he did discover, unlike the constant named in his honour but discoverd by the 8th Laird of Merchistoun, John Napier. I have added http://rosettacode.org/wiki/Pi#C.2B.2B which I hope will help you to understand the raltionship between agm(1,1/sqrt(2)) and Pi, and hence why the accuracy with which this task can be computed limits the accuracy with which Pi can be computed using this method. Incidently it is debatable whether 0 is even a number let alone an important constant, more of a nuiscence really.--[[User:Nigel Galloway|Nigel Galloway]] 12:44, 9 February 2012 (UTC)

::The main problem I have with the description as it is is that it is too vague for the goal of finding the value of a constant. If the task goal is to give that value then it's pretty trivial to just calculate it offline (or look it up) and print it as text straight up. If the task goal is to calculate a general arithmetic-geometric mean then it needs a rename and I would suggest using different numbers. It would be nice to have smaller-precision values so that languages with no arbitrary precision libraries can participate. --[[User:Mwn3d|Mwn3d]] 18:17, 7 February 2012 (UTC)

::: I've rewritten the task description to make the focus be on the <code>agm</code> function, with the application to computing agm(1,2**-0.5) being just an application of it. I've also linked things into Wikipedia, as that's a little more accessible than Mathworld. (I'll rename the task in a moment or two.) I'll not tackle whether that is the ''right'' value to calculate; for me, the interesting thing is computing a value using a (pair of) converging sequence(s). –[[User:Dkf|Donal Fellows]] 10:14, 8 February 2012 (UTC)

Are people missing that the number specified is not Gauss's?  agm(1, 1/sqrt(2)) is about .85 and Gauss's, 1/agm(1, sqrt(2)) is about .83.  Either way, I'll clarify that I think a task to compute a constant should a) specify an algorithm to be used, and b) specify the precision to be obtained. &mdash;[[User:Sonia|Sonia]] 19:07, 7 February 2012 (UTC)
:+1 on a and b. --[[User:Mwn3d|Mwn3d]] 19:11, 7 February 2012 (UTC)
Of possible interest:  [http://oeis.org/A096427] &mdash;[[User:Sonia|Sonia]] 20:48, 9 February 2012 (UTC)

==On Using The Arithmetric Geometric Mean to Calculate Pi==
I have included examples of using the agm to calculate Pi:

: http://rosettacode.org/wiki/Pi#C.2B.2B
: http://rosettacode.org/wiki/Pi#using_agm

Proof and details of the algorithm used are described:

: http://mathdl.maa.org/images/upload_library/22/Ford/Almkvist-Berndt585-608.pdf

The paper begins with an investigation of why the agm is such an efficient algorithm, and proves that it converges quadratically. From this section one should remeber equation (1) which defines c. Read through to Theorem 5 (derived from Theorems 3&4). If you have remembered the definition of c this should remind you of the above implementations.

The paper goes on to prove Theorem 5, though one could argue that the success of the above implementations is proof. It then procedes to show how the agm may be used to calculate length of the perimeter of an elipse without differential calculus, which was the origional intent (perhaps another task), but really we are only interested in Pi.

--[[User:Nigel Galloway|Nigel Galloway]] 14:26, 29 March 2012 (UTC)

== Complex ==

I'll retract my complex number solution.  I don't have a reference for this code and even if the AGM is defined to use a certain root, I'm not sure this code is sufficient.  Description and code copied here:

The referenced Mathworld page mentions that AGM is meaningful on the complex plane as well.
:It certainly has. It has been called The Mind of God (perhaps beyond the scope of this task!). There is a little more to it than adding +0i to the real solution. In Real Maths sqrt 4 is 2(This task assumes this). In fantasy maths sqrt 4 is 2 and -2. Then the next iteration takes the sqrt of a negative number, and maths goes complex. The result in Complex maths is the set of all these arithmetric geometric means.--[[User:Nigel Galloway|Nigel Galloway]] 14:20, 23 April 2012 (UTC)

```go
package main

import (
    "fmt"
    "math"
    "math/cmplx"
)

const ε = 1e-14
```


==rapidity of convergence==

From this Rosetta Code task's prologue:

:: <big><big>Since the limit of <math>a_n-g_n</math> tends (rapidly) to zero with iterations, this is an efficient method.</big></big>


With this in mind, the REXX entry was modified   (and suppressed the normal output being displayed),   and

added a display of the iteration count along with the number of decimal digits being used.

The modified REXX program is:

```rexx
/*REXX program calculates the  AGM  (arithmetic─geometric mean)  of two (real) numbers. */
parse arg a b digs .                             /*obtain optional numbers from the C.L.*/
if digs=='' | digs==","  then digs=100           /*No DIGS specified?  Then use default.*/
numeric digits digs                              /*REXX will use lots of decimal digits.*/
if a==''    | a==","     then a=1                /*No  A  specified?   Then use default.*/
if b==''    | b==","     then b=1 / sqrt(2)      /*No  B  specified?     "   "     "    */
call AGM a,b                                     /*invoke AGM  &  don't show A,B,result.*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
agm:  procedure: parse arg x,y;   if x=y  then return x       /*is it an equality case? */
                                  if y=0  then return 0       /*is value of   Y   zero? */
                                  if x=0  then return y / 2   /* "   "    "   X     "   */
      d=digits();   numeric digits d+5           /*add 5 more digs to ensure convergence*/
      tiny='1e-' || (digits() - 1);              /*construct a pretty tiny REXX number. */
      ox=x + 1
                       do #=1  while ox\=x & abs(ox)>tiny;  ox=x;         oy=y
                                                             x=(ox+oy)/2;    y=sqrt(ox*oy)
                       end   /*#*/
      numeric digits d                           /*restore  numeric digits  to original.*/
                                                 /*this is the only output displayed ►─┐*/
      say 'digits='right(d, 7)",  iterations=" right(#, 3)          /* ◄───────────────┘*/
      return x/1                                 /*normalize    X    to the new digits. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
sqrt: procedure; parse arg x; if x=0  then return 0; d=digits(); m.=9; numeric form; h=d+6
      numeric digits; parse value format(x,2,1,,0) 'E0'  with  g 'E' _ .;  g=g *.5'e'_ % 2
        do j=0  while h>9;      m.j=h;               h=h % 2  + 1;  end /*j*/
        do k=j+5  to 0  by -1;  numeric digits m.k;  g=(g+x/g)*.5;  end /*k*/;    return g
```

{{out|output|text=  (from multiple runs)   with a specified number of digits, and with the various outputs being combined/reduced/edited:}}

```txt

digits=    100,  iterations=   9
digits=    200,  iterations=  10
digits=    400,  iterations=  11
digits=    800,  iterations=  12
digits=   1600,  iterations=  13
digits=   3200,  iterations=  14
digits=   6400,  iterations=  15
digits=  12800,  iterations=  16
digits=  25600,  iterations=  17
digits=  51200,  iterations=  18

```

The CPU time quadrupled (about) everytime the digits were doubled.

==Formulae hidden to most browsers by under-tested cosmetic edits at 21:25, 14 April 2016==

Under-tested cosmetic edits made to the task page at 21:25, 14 April 2016, including the injection of spaces around expressions in &lt;math&gt; tags, have left most of the task description formulae completely invisible to all browsers which display the graphic file version of formulae rather than processing the MathML (this is, in fact, the majority of browsers). The MediaWiki processor does not currently expect such spaces, and generates syntactically ill-formed HTML if they are introduced. Other aspects of these cosmetic edits may further compound the problem. [[User:Hout|Hout]] ([[User talk:Hout|talk]]) 08:48, 21 September 2016 (UTC)

: Visibility of formulae restored 18 Oct 2016 [[User:Hout|Hout]] ([[User talk:Hout|talk]]) 19:03, 18 October 2016 (UTC)
