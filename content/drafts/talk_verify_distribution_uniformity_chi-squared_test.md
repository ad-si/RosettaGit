+++
title = "Talk:Verify distribution uniformity/Chi-squared test"
description = ""
date = 2018-12-11T03:49:48Z
aliases = []
[extra]
id = 4689
[taxonomies]
categories = []
tags = []
+++

I welcome someone checking to see if my mathematics is correct! Right now, the information on this test on the web is really rather confusing, being mostly written by and for people who assume you have been taught it all manually in undergraduate statistics or are reading off from a printed table. —[[User:Dkf|Donal Fellows]] 12:13, 9 August 2009 (UTC)

:My own statistics are rusty, and I am not familiar enough with the TCL math::statistics library to know how to check your math.  That said, I think I would be helped if you specified which [[wp:Chi-square test|Chi-square test]] you have implemented (Pearson's?). [[User:Rdm|Rdm]] 18:37, 8 September 2009 (UTC)

:: Well my stats aren't good enough to answer that question. I know I'm computing the degree of deviation from a uniform distribution – that's the easy bit – but the bit where it computes what this means is pure guesswork and the material on wikipedia doesn't help. Most of wikipedia's non-elementary mathematics is of terrible quality from the perspective of someone who was never a real math major (logic and advanced CS I know, but stats... no). —[[User:Dkf|Donal Fellows]] 20:56, 8 September 2009 (UTC)

== Is the Wikibooks Python example good enough? ==
Assuming that the licenses are compatable, is this [http://en.wikibooks.org/wiki/Algorithm_Implementation/Pseudorandom_Numbers/Chi-Square_Test#Python wikibooks Python example] good enough? --[[User:Paddy3118|Paddy3118]] 04:45, 31 October 2009 (UTC)

== Incomplete gamma ==

It seems computing the the incomplete  gamma function is non-trivial and the technique of numerical integration used by some of the examples here has limitations.  I tried a data set with 100 categories and [my Go code anyway] failed miserably.  Following links on WP I found the continued fraction expansion and tried it.  It worked beautifully for my 100 category test case, but then failed for the second test case used by many of the examples here, the one with strongly non-uniform distribution.  Digging more on the web I found quite a variety of algorithms, and no consensus on the good way to compute this.  The most sensible techniques I saw incorporated multiple algorithms and selected between them based on different ranges of inputs.  &mdash;[[User:Sonia|Sonia]] 17:33, 7 May 2011 (UTC)

:I am not completely sure what "the incomplete gamma function" is, but here's the J implementation:  

:
```j
4 :0
   (1 H. (1+x) % x&((* ^) * (^ -)~)) y
)
```


:So, for example if x is 1.5 and y is 4.1

:
```j
   (1 H. (1+1.5) % 1.5&((* ^) * (^ -)~)) 4.1
0.848957
```


:The right argument to H. is:

:
```j
   ((1+1.5) % 1.5&((* ^) * (^ -)~)) 4.1
0.229307
```


:In other words, 2.5 divided by 10.9024:

:
```j
   ( 1.5&((* ^) * (^ -)~)) 4.1
10.9024
```


:In other words (1.5 * ^ 4.1) * 4.1 ^ -1.5.  (^y with no left argument is e^y).

:And H. is the primitive documented at http://www.jsoftware.com/help/dictionary/dhcapdot.htm

:I do not know if this helps, though.

:--[[User:Rdm|Rdm]] 22:28, 7 May 2011 (UTC)

== possible error ==

In the Ruby version of gammaInc_Q, near the end there is a few lines



 (n-1).step(0, -1) do |j|
    t = h * j
    sum += f0[t] + hh * df0[t]
 end

The last iteration produces a value of j of 0, which means t is zero but df0[t] is not a number
as it requires the calculation of 0**a2. I just changed the enumerator to (n-1).step(1, -1)|
and it seems to give the correct result.
