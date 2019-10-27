+++
title = "Talk:Closest-pair problem"
description = ""
date = 2010-02-06T12:41:11Z
aliases = []
[extra]
id = 4148
[taxonomies]
categories = []
tags = []
+++

Since we only compare distances and the actual smallest distance never needs to be reported, could the calculation be sped up by not using a square root in the distance calculation? I'm not sure how the current examples are calculating the distance, but they may be optimized by calculating the square of the distance (d^2 = (x2-x1)^2+(y2-y1)^2). I don't think there is any use for the actual distance anywhere in the pseudocode, but I may be wrong. --[[User:Mwn3d|Mwn3d]] 19:03, 9 May 2009 (UTC)
:I think the actual distance is meant to be the result of the function. Still, that would let you optimize to reduce the number of square root operations performed. (I do question whether we're doing a “how fast can we go” exercise though; clarity is better here, yes?) — [[User:Dkf|Dkf]] 21:10, 9 May 2009 (UTC)

::I believe it would be a reasonable optimization. It would be enough to compute sqrt on the return value (we need a "non recursive front-end" function anyway). But maybe this is up to implementors. Pseudocode should not think about it. --[[User:ShinTakezou|ShinTakezou]] 09:21, 11 May 2009 (UTC)

== About this task ==

I am working on it; currently I have less time than before. The pseudocode needs to be cleaned up and corrected; the Smalltalk impl works fine in all my tests, but I've done a C and Perl impl too... and they work 1/6 times... meaning that once a while the result between the "brute force" method and "fast" method disagree; at the very beginning I thought the fault was in the C and Perl impls, since Smalltalk worked always... but now I think it is indeed a deeper problem and the error does not exhibit in Smalltalk because of the distribution of the random number (this is just an idea... I am trying to figure out a set of tests to understand it better what's going wrong and why, and why not in Smalltalk... using the same set of points could be enough, it's in my plan). Once C and Perl code will work, I will fix the pseudocode and the Smalltalk impl. Of course, if anyone can fix the pseudocode (it is deduced by the Smalltalk impl indeed!)... or doing a new impl finds it work properly (this would be my C and Perl impl are wrong someway... but I can't see where!)... I would like to read any idea on it. (First I am going to use the same set of points, then I will take paper and pencil and try to reinvent the wheel in the hope I understand where I get the algorithm explanation wrong!) --[[User:ShinTakezou|ShinTakezou]] 09:17, 11 May 2009 (UTC)

'''A note''': should be the pseudocode marked as "problematic" (or whatever)? (References are there even to allow any good guy to make it better; sorry for this, but sadly as said before the Smalltalk impl does not exhibit a wrong behaviour, so I was deceived by it :D) --[[User:ShinTakezou|ShinTakezou]] 09:23, 11 May 2009 (UTC)

It seems weird to only return the distance of the closest pair. Wouldn't want the actual pair of points rather than just the distance between them? That's what the description seems to say, but the examples only return the distance. Are they missing the point? --[[User:Mwn3d|Mwn3d]] 16:15, 11 May 2009 (UTC)

: Implementations (at least Smalltalk and new Perl and C) do so! Just I haven't specified it into the pseudocode, since it would make it less clear (the point is not to keep track of the closest points: it is about the "smaller distance" so to say). Indeed it is not hard to keep track of that information even in the pseudocode, so maybe I can add it after all. But my first effort was to provide a working pseudocode... which points can come after. (Now everything seems fixed, so I am going to add it to the pseudocode too...) --[[User:ShinTakezou|ShinTakezou]] 17:06, 11 May 2009 (UTC)

: I've updated the pseudocode to explicitly ''store'' the closest pair; however, after updating I thought after all it was not so useful: since a distance is always computed between two points, the information could be considered as ''hidden'' inside the computed distance... However now the implementors are suggested to give that information too... --[[User:ShinTakezou|ShinTakezou]] 18:09, 11 May 2009 (UTC)

::I found [http://www.cs.iupui.edu/~xkzou/teaching/CS580/Divide-and-conquer-closestPair.ppt This reference] helped me with my Python example, which seems to work. --[[User:Paddy3118|Paddy3118]] 21:56, 11 May 2009 (UTC)
:::C and Perl and also Smalltalk (updated), based now on the '''new''' pseudocode, '''work''', so I suppose the algorithm now is correct! (What written before referred to the previous pseudocode and first Smalltalk implementation) --[[User:ShinTakezou|ShinTakezou]] 09:09, 12 May 2009 (UTC)

==Comment on Algorithms Given==
[http://paddy3118.blogspot.com/2009/05/critique-of-pseudocode-explanations-of_12.html Critique of pseudocode explanations of the Closest Pair Algorithm] --[[User:Paddy3118|Paddy3118]] 05:39, 12 May 2009 (UTC)
: The new code (id est, as it is now) '''works'''. The code that had problems was the previous one (in your blog you report the most recent version, that, as said, works). Maybe I've chosen a not so communicative pseudolanguage... but I thought we were familiar with such notation like "ceil" or "floor", or the meaning of {elements...} (which resembles a set; here we intend an ordered, indexable, "set"); and it seemed to me intuitive that a P(i)<sub>x</sub> refers to the point i-th, x coordinate, even though it would be better to write (P(i))<sub>x</sub> maybe. I would have used P<sub>i,x</sub> but at the end I preferred the P(index) form in general; I could have written P<sub>x</sub>(index) instead... maybe it is clearer? Using the last reference, I've corrected the last part of the pseudocode (and in fact the while loop is fundamentally the one you can see there). The syntax I am dissatisfied with is the last I've added to explicitly take care of the points' coordinates, e.g. <code>(closest, closestPair)</code>... but it is the faster modification I was able to think to add that without too many efforts.
: Language apart, the algorithm now works (at least, Smalltalk, C and Perl implementations, based on that, works).
: Anyone interested in writing better pseudocode (even in changing the current "language" keeping the same "code") is welcome of course! --[[User:ShinTakezou|ShinTakezou]] 09:04, 12 May 2009 (UTC)
::Hi, You've done sterling work. I was dumb. It just took a different explanation before I could 'get' it. On the Ceiling thing, I totally missed the the fact that the bottom of the brackets was missing and so mis-read it as plain old square brackets using a crap font.
::Their is still one outstanding point though: in the ref. I found, they stated that you had to pre-sort only once, for both X and Y ''before'' you entered the recursive routine or the sorts for Y would make the algorithm n(logn)**2 rather than nlogn. But I'm no expert in deriving O(n) notation. --[[User:Paddy3118|Paddy3118]] 12:27, 12 May 2009 (UTC)
::: Of course you're right: sorting must be done once (since once you sort, e.g. by x, splitting won't mess up the order...) That's also why the refs pass sorted sets as arguments... but while implementing it the first time, I've disregarded the question since I was more concentrated on other details... Argh. I should rewrite it all a lot better :( --[[User:ShinTakezou|ShinTakezou]] 13:03, 12 May 2009 (UTC)
Can someone test this?
Memory access bug in C version. Program crashes in closest_pair_ function.
HEAP CORRUPTION DETECTED:
CRT detected that the application wrote to memory after end of heap buffer.
Sorry, don't know how to format this reply.
I put in checks to test out-of-bound indices in the function closest_pair_, and they fail on these lines:
  xm = P[xP[midx]].x;
    if ( P[yP[i]].x <= xm ) {
Using NP 10000,
xP[midx] returns 6041, when N is 5000.
yP[i] returns 7099, when N is 5000.
: Hm interesting... I'll take a look when I'll have more time. In my tests it did not crash, but it does not mean necessarily that it does not write outside the heap... I must do some deeper check. --[[User:ShinTakezou|ShinTakezou]] 10:16, 13 October 2009 (UTC)
: I've overbloated the code with extra assertions in indexes and everything seemed to go fine. About the part you show, N is just the number of element in yP and xP, so the constrain < N (5000) is for <tt>i</tt>, not for yP[i] or xP[i]; these two are indexes on P, which can go from 0 to 10000-1 in your example so the results 6041 and 7099 are ok. I can't reproduce the error, and assertions I've added seem to say it's ok, but of course I will continue to test it; maybe there are some problem related to the random distribution; you can send me the random numbers your computer generated (archived with zip or gzip please); or try this change to the code:
    xm = (P[xP[midx]].x + P[xP[midx+1]].x) / 2.0;
    if ( P[yP[i]].x < xm ) {
: If the problem disappears, it was the problem... Sincerely I can't see why it should be, maybe the latter is better from certain point of view, but to me it should behave the same... (Indeed, I've not checked even "mathematically" if it is true, but I will do... for now, just intuition at work!) --[[User:ShinTakezou|ShinTakezou]] 20:20, 18 October 2009 (UTC)
Sorry, that didn't work.  I sent my pointset data to your gmail account if you want to look at it.
: Thanks. I've taken a look at it and understood from where the problem comes: not unique points, i.e. some points are the same... the closest distance is 0, and the closest point pair should be just the first giving 0 as distance... the algorithm does not take into account the simple fact that points can be the same. I will check it for C and other codes as soon as I can. (To test it just do a simply <tt>cat points.txt | sort -nu |wc -l</tt>, it says 8639 instead of 10000 on he dataset you've sent to me) --[[User:ShinTakezou|ShinTakezou]] 15:30, 25 October 2009 (UTC)
:: Second thought: it was not so because of how sort -nu works... there are 10k different numbers. But of course the problem is still about the distribution of the numbers... --[[User:ShinTakezou|ShinTakezou]] 16:20, 25 October 2009 (UTC)
