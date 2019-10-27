+++
title = "Talk:Ackermann function"
description = ""
date = 2019-03-07T16:33:15Z
aliases = []
[extra]
id = 3237
[taxonomies]
categories = []
tags = []
+++


### how well the C++ performs

I don't really know how well my C++ function performs, as I said im just learning C++. but i though i would add it since there wasn't one.
If something is wrong with it, please email me at ki4hbd(dot)nathan(at)gmail(dot)com and explain if you don't mind. Thanks!


### Arbitrary precision with BC

BC is a binary calculator with [[arbitrary precision]] (similar to [[dc]], but this one use the RPN, bc resembles C in its syntax); the code would be (translated from C):


```txt
#! /usr/bin/bc -q
define ack(m, n) {
   if ( m == 0 ) return (n+1);
   if ( n == 0 ) return (ack(m-1, 1));
   return (ack(m-1, ack(m, n-1)));
}

for(n=0; n<7; n++)
{
  for(m=0; m<4; m++)
  {
     print "A(", m, ",", n, ") = ", ack(m,n), "\n"; 
  }
}
quit
```


Would this be suitable for adding in the article? --[[User:ShinTakezou|ShinTakezou]] 14:05, 9 December 2008 (UTC)

: Why not ? [[User:Rahul|Rahul]] 14:45, 9 December 2008 (UTC)

: You could also write bc examples for other articles in [[:Category:Arbitrary precision]]. --[[User:IanOsgood|IanOsgood]] 14:35, 28 December 2008 (UTC)

== Comment preceding MUMPS code (moved from the page) ==

... when I told Tom Ackermann that this function existed, he
couldn't believe that an actual mathematical function was named
after a relative if his... May he rest in peace!

{I am not signing this since I am not the author, I've just moved this from the page &mdash;[[User:ShinTakezou|ShinTakezou]] 17:07, 23 April 2010 (UTC)}

== ABAP ==

Abap results seem wrong

== ZED version? ==

How do we read the ZED version?  (What are we seeing there?) --[[User:Rdm|Rdm]] 08:04, 3 August 2012 (UTC)

==Java 8 example size==
Could someone explain why the Java 8 version merits inclusion? It seems ridiculously long in comparison with the Java version above it. Does Java 8 ''require'' so much code? -[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 17:38, 5 October 2013 (UTC)

The Java 8 version is blazingly fast. Java 8 is perfectly able to run the "standard" version, but using features new in Java 8, the author has managed to both make it magnitudes faster even though it uses unlimited precision integers. Admittedly, I have a hard time understanding (or even reading) the code, but I'm most eager to learn from it. -[[User:Lenborje|Lenborje]] ([[User talk:Lenborje|talk]]) 16:00, 11 January 2016 (UTC)


==Main formula made invisible to many browsers by cosmetic edit of 00:19, 15 August 2016==

An under-tested cosmetic edit made to the task page at 00:19, 15 August 2016, including the injection of spaces around expressions in &lt;math&gt; tags, left the main formula completely invisible to all browsers which display the graphic file version of formulae rather than processing the MathML (this is, in fact, the majority of browsers). The MediaWiki processor does not currently expect such spaces, and generates syntactically ill-formed HTML if they are introduced. Other aspects of this cosmetic edit may further compound the problem. It's essential to test on the majority kind of browser (formula file displaying) when these cosmetic interventions are attempted [[User:Hout|Hout]] ([[User talk:Hout|talk]]) 19:47, 21 September 2016 (UTC)

: Restored the lost visibility of server-side graphics formulae in the task description [[User:Hout|Hout]] ([[User talk:Hout|talk]]) 14:43, 1 November 2016 (UTC)

==Scala==

I agree, the second solution in Scala was overly complex.  As someone wrote the in "needs review":

: The following part of this "heavily recursive" solution is way too complex and does not display the idiom of Scala. Memoization is possible with a stream (See final lesson of the Coursera course "Functional Programming Principles in Scala") which is much simpler and better displays the power of the language.

There should be a memoized version, but keeping the wall of code wasn't going to get it there, so I ripped it out.  If anyone wants to review it or reuse it, here's [https://rosettacode.org/mw/index.php?title=Ackermann_function&diff=278347&oldid=278346 the diff where it was removed].   --[[User:Ashawley|Ashawley]] ([[User talk:Ashawley|talk]]) 16:33, 7 March 2019 (UTC)
