+++
title = "Talk:Least common multiple"
description = ""
date = 2019-04-24T01:29:20Z
aliases = []
[extra]
id = 9407
[taxonomies]
categories = []
tags = []
+++

== special case for zero ==
When I first wrote the task, I forgot about the special case for zero. I later added this special case to the task, but I left it as a draft task. --[[User:Kernigh|Kernigh]] 00:29, 31 March 2011 (UTC)

==Yet another way, isn't?==
From ''"Yet another way is to use rational arithmetic; if you add 1/m and 1/n and reduce the fraction, then the denominator is the lcm"'' 

I did the following:

```python
>>>
 def lcm(a, b): return (fractions.Fraction(1,a) + fractions.Fraction(1,b)).denominator if a and b else 0

>>> lcm(12, 18)
36
>>> lcm(-6, 14)
21
>>> # Whoops!
```

It is out by any common factor. --[[User:Paddy3118|Paddy3118]] 03:17, 31 March 2011 (UTC)

:I do not know where you found that sentence, but a bigger problem seems to be that all of the page history for this page displays for me as 2011-03-11 when I believe this page has been around for quite some time. --[[User:Rdm|Rdm]] 13:03, 31 March 2011 (UTC)
::That sentence used to be in the description, but it was removed when it was found to be wrong. Also the page is even younger than that (as long as I'm reading your date format correctly) It was only created yesterday. I was amazed too. I thought for sure we had this one, but I couldn't find it. --[[User:Mwn3d|Mwn3d]] 13:31, 31 March 2011 (UTC)

::Hi Rdm, try [http://rosettacode.org/mw/index.php?title=Least_common_multiple&oldid=104117 this direct link]. --[[User:Paddy3118|Paddy3118]] 13:35, 31 March 2011 (UTC)

:Does lcm(x,y) = lcm(-x,y) = lcm(-x,-y)? If so then you could just change the sign on the arguments and use the fractional method. I'm pretty sure it works for positive numbers. The fractional method wouldn't work for fractional arguments, though (ex: lcm(1/3, 1/6) = 1/3 = 1 * 1/3 = 2 * 1/6). For those you would nee to use the "iteration over multiples" method I believe. --[[User:Mwn3d|Mwn3d]] 14:53, 31 March 2011 (UTC)

::I have not been able to find a good online reference on LCM that treats negative arguments consistently (for example, consider the distributive and idempotent properties described at the mathworld reference).  But 1/6 + 1/14 in reduced form is 5/21 where the least common multiple of 6 and 14 is 42. --[[User:Rdm|Rdm]] 15:35, 31 March 2011 (UTC)

== Java solution ==

I suspect the intention of the java code before my edits was
```java
for (i = MultipleOfN/m; ...)
```
 instead of 
```java
for (i = MultipleOfM/m; ...)
```
, where the former makes somewhat more sense.  --[[User:Ledrug|Ledrug]] 16:25, 22 August 2011 (UTC)
:Nope. I wanted the loop variable to start at the same multiple as it ended on before (i.e. if it multiplied n 5 times to get to the value of multipleOfN, then you'd need to divide it by n to get back to 5). The way it is now makes more sense. I was trying to think of that way, but my brain was stuck somehow. --[[User:Mwn3d|Mwn3d]] 16:32, 22 August 2011 (UTC)


==Formula hidden to most browsers by under-tested cosmetic edits at 20:13, 17 April 2016 ==

Under-tested cosmetic edits made to the task page at 20:13, 17 April 2016, including the injection of spaces around expressions in &lt;math&gt; tags, have left the main task description formula completely invisible to all browsers which display the graphic file version of formulae rather than processing the MathML (this is, in fact, the majority of browsers). The MediaWiki processor does not currently expect such spaces, and generates syntactically ill-formed HTML if they are introduced. Other aspects of these cosmetic edits may further compound the problem. [[User:Hout|Hout]] ([[User talk:Hout|talk]]) 21:02, 22 September 2016 (UTC)

: Visibility of task description formula restored [[User:Hout|Hout]] ([[User talk:Hout|talk]]) 23:05, 26 October 2016 (UTC)

== (REXX) versions 1 and 2 compared ==
(─── Moved here from the main page, as I (REXX version 1's author) thought it wasn't suitable being there, and it should
instead belong here on the discussion page. ───)     -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 00:55, 3 October 2017 (UTC)

(Below is from the REXX version 2's author.)

-----

The ''(performance) improvement'' of version 2 is due to the different argument handling at the cost of less ''freedom'' of argument specification (you must use lcm2(a,b,c) instead of the ''possible'' lcm1(a b,c). Consider, however, lcm1(3 -4, 5) which, of course, won't work as possibly intended.

The performance improvement is more significant with ooRexx than with Regina.

 ''Note:'' $ in version 1 was replaced by d in order to adapt it for this test with ooRexx.

```rexx
Parse Version v
Say 'Version='v
Call time 'R'
Do a=0 To 100
  Do b=0 To 100
    Do c=0 To 100
      x1.a.b.c=lcm1(a,b,c)
      End
    End
  End
Say 'version 1' time('E')
Call time 'R'
Do a=0 To 100
  Do b=0 To 100
    Do c=0 To 100
      x2.a.b.c=lcm2(a,b,c)
      End
    End
  End
Say 'version 2' time('E')
cnt.=0
Do a=0 To 100
  Do b=0 To 100
    Do c=0 To 100
      If x1.a.b.c=x2.a.b.c then cnt.0ok=cnt.0ok+1
      End
    End
  End
Say cnt.0ok 'comparisons ok'
Exit
/*----------------------------------LCM subroutine----------------------*/
lcm1: procedure; d=strip(arg(1) arg(2));  do i=3  to arg(); d=d arg(i); end
parse var d x d                        /*obtain the first value in args.*/
x=abs(x)                               /*use the absolute value of  X.  */
          do  while d\==''             /*process the rest of the args.  */
          parse var d ! d;   !=abs(!)  /*pick off the next arg (ABS val)*/
          if !==0  then return 0       /*if zero, then LCM is also zero.*/
          x=x*!/gcd1(x,!)               /*have  GCD do the heavy lifting.*/
          end   /*while*/
return x                               /*return with  LCM  of arguments.*/
/*----------------------------------GCD subroutine----------------------*/
gcd1: procedure; d=strip(arg(1) arg(2));  do j=3  to arg(); d=d arg(j); end
parse var d x d                        /*obtain the first value in args.*/
x=abs(x)                               /*use the absolute value of  X.  */
          do  while d\==''             /*process the rest of the args.  */
          parse var d y d;   y=abs(y)  /*pick off the next arg (ABS val)*/
          if y==0  then iterate        /*if zero, then ignore the value.*/
               do  until y==0;  parse  value   x//y  y   with  y  x;   end
          end   /*while*/
return x                               /*return with  GCD  of arguments.*/

lcm2: procedure
x=abs(arg(1))
do k=2 to arg() While x<>0
  y=abs(arg(k))
  x=x*y/gcd2(x,y)
  end
return x

gcd2: procedure
x=abs(arg(1))
do j=2 to arg()
  y=abs(arg(j))
  If y<>0 Then Do
    do until z==0
      z=x//y
      x=y
      y=z
      end
    end
  end
return x
```

{{out}} 
Output of rexx lcmt and regina lcmt cut and pasted together:

```txt
Version=REXX-ooRexx_4.2.0(MT)_32-bit 6.04 22 Feb 2014
                          Version=REXX-Regina_3.9.0(MT) 5.00 16 Oct 2014
version 1 29.652000       version 1 23.821000
version 2 10.857000       version 2 21.209000
1030301 comparisons ok    1030301 comparisons ok
```



-----


I   «[[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 00:55, 3 October 2017 (UTC)»   generally don't like to compare REXX program examples for speed unless a version has increased
functionality (or options), and there is a sufficient increase in speed to warrant including another
(hopefully more advanced) REXX program.   The exception to this is when a better program
version is much faster than another, usually in the order of a magnitude, or in the case where the
program runs a significant amount of time (elapsed or CPU) ─── for instance, in minutes.

From what I understand of the (or ''a'') Rosetta Code policy is that language speed comparisons of two
different languages (such as ooRexx and REXX) are discouraged.   If not different languages,
then at least they are of two different dialects.   We're not here on Rosetta Code to compare 
how fast one language is to another.

For computing the '''LCM''', unless it is invoked a significant number of times, it doesn't matter
that much if one is 10% slower than another.


One bad thing about measuring two different programming examples   (especially if they were
written by two different authors),   is that when one version changes,   will the other
person then re─run their comparison and re─edit the results of the comparison runs?
  This has been shown to be demonstratively not be the case.

In particular, the 1<sup>st</sup> REXX version (mine) had it's '''LCM''' function updated a while ago with
a faster version (by me), but the comparison evaluation used the older REXX version.   Instead of the
2<sup>nd</sup> version being faster by about 10%, it was now slower by about 65%   (or some
such numbers, see below for the latest comparisons).

I would prefer this whole comparison stuff be struck (by a Rosetta Code administrator) from this
task's discussion page as it solves no worthwhile purpose that is in compliance with Rosetta Code
policy.   Rosetta Code is for showing how to solve problems and be helpful in comparing
functionality of computer programming languages.   These are my opinions, of course, and may
not be based in reality.   Hopefully, someone with more more authority on RC's policies will
step in.   However, this may be used as a teaching moment.


Secondly, running (benchmarking) comparative executions is more science than art, there are a lot
of pitfalls:   making sure to use the identical data, not re─writing code (to make a square
peg to fit into a round hole), one version paying a penalty for initializing/defining variables (a
big deal in interpretive languages), the obtaining of storage, including/loading ("system")
subroutines/functions, garbage collection, pollution of memory (pools), the use of caching by
subsequent execution(s), etc.     (or as it is said often elsewhere, too many to be
listed here).   For the above reasons and others, I've extensively re─written the old benchmark program and
re─run it (for more trials).   The results are included here below   (after the benchmark
REXX program).


The old benchmark program was re─worked considerably, including the removal of comparing results.
  The results should be computed once, and after verification that they produce identical
results, that part of the code can be elided.   Once that removal is done, there is no need to
pollute the variable pool with over a million ('''101<sup>3</sup>''') individual answers   (it
then becomes an exercise on how efficient the generating/retrieval of variables is).   Also,
the   '''do'''   loops were optimized to cut down on their overhead, the use of  
'''for'''   instead of   '''to'''   reduces the overhead of the benchmarking
program, that is, the part of the benchmarking program that is common to the testing of both functions.  
We don't want to have the overhead of the benchmark itself overwhelm what is being measured.  
Added to the benchmark program are options to specify how many trials, and the ('''high''') number
of invocations of the various (well, only two)   '''LCM''' function versions.   Also
added were titles and also more whitespace to make the output easier to read.


Note that if the   '''LCM1'''   function is invoked by an alternative invocation, it is
even faster   (see the   ''is faster''   note/comment within the REXX code of version
1 on line sixteen).   This is one reason why the REXX version 1 has that 
option (functionality) ─── to allow the user to use whatever form is easier or desirable for them, 
not to mention the invocation form that is faster.

Also noteworthy is that the PC used for the execution of the benchmark program is an air─gap
computer   (with four cores running under Microsoft's Windows 7)   and having no contention with
any other programs.


The benchmarking program is:

```rexx
/*REXX program supposedly measures/compares 2 versions of LCM (least common multiplier).*/
parse version v;     say 'version=' v

parse arg high trials .
if   high=='' |   high==","   then   high=101
if trials=='' | trials==","   then trials= 10

       do t=1  for trials
       say
       say center(' trial'  right(t, length(trials) )" ", 40, '~')

       call time 'reset'
                              do     a=0  for high
                                do   b=0  for high
                                  do c=0  for high
                                  answer1=lcm1(a, b, c)       /*LCM1(a b c)   is faster.*/
                                  end   /*c*/
                                end     /*b*/
                              end       /*a*/

       say '       version 1 took'   format( time( 'elapsed'), , 2)   "seconds."

       call time 'reset'
                              do     x=0  for high
                                do   y=0  for high
                                  do z=0  for high
                                  answer2=lcm2(x, y, z)
                                  end   /*z*/
                                end     /*y*/
                              end       /*x*/

       say '       version 2 took'   format( time( 'elapsed'), , 2)   "seconds."
       say
       end  /*times*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
lcm1: procedure; parse arg $,_; $=$ _;          do i=3  to arg();  $=$ arg(i);  end  /*i*/
      parse var $ x $                                 /*obtain the first value in args. */
      x=abs(x)                                        /*use the absolute value of  x.   */
                do  while $\==''                      /*process the remainder of args.  */
                parse var $ ! $;   if !<0  then !=-!  /*pick off the next arg (abs val).*/
                if !==0  then return 0                /*if zero, then lcm is also zero. */
                d=x*!                                 /*calculate part of the lcm here. */
                       do  until !==0;    parse  value   x//!  !     with     !  x
                       end   /*until*/                /* [↑]  this is a short & fast gcd*/
                x=d%x                                 /*divide the pre calculated value.*/
                end   /*while*/                       /* [↑]  process subsequent args.  */
      return x                                        /*return with the lcm of the args.*/
/*──────────────────────────────────────────────────────────────────────────────────────*/
lcm2: procedure
x=abs(arg(1))
do k=2 to arg() while x<>0
  y=abs(arg(k))
  x=x*y/gcd2(x,y)
  end
return x

gcd2: procedure
x=abs(arg(1))
do j=2 to arg()
  y=abs(arg(j))
  if y<>0 then do
    do until z==0
      z=x//y
      x=y
      y=z
      end
    end
  end
return x
```

{{out|output|text=  when using the default input:}}

```txt

version= REXX-Regina_3.9.1(MT) 5.00 5 Apr 2015

═══════════════ trial  1 ═══════════════
       version 1 took 13.35 seconds.
       version 2 took 21.34 seconds.


═══════════════ trial  2 ═══════════════
       version 1 took 13.37 seconds.
       version 2 took 21.31 seconds.


═══════════════ trial  3 ═══════════════
       version 1 took 13.37 seconds.
       version 2 took 21.33 seconds.


═══════════════ trial  4 ═══════════════
       version 1 took 13.35 seconds.
       version 2 took 21.34 seconds.


═══════════════ trial  5 ═══════════════
       version 1 took 13.35 seconds.
       version 2 took 21.31 seconds.


═══════════════ trial  6 ═══════════════
       version 1 took 13.35 seconds.
       version 2 took 21.33 seconds.


═══════════════ trial  7 ═══════════════
       version 1 took 13.37 seconds.
       version 2 took 21.34 seconds.


═══════════════ trial  8 ═══════════════
       version 1 took 13.34 seconds.
       version 2 took 21.34 seconds.


═══════════════ trial  9 ═══════════════
       version 1 took 13.31 seconds.
       version 2 took 21.25 seconds.


═══════════════ trial 10 ═══════════════
       version 1 took 13.32 seconds.
       version 2 took 21.25 seconds.

```


-- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 00:55, 3 October 2017 (UTC)

-----
