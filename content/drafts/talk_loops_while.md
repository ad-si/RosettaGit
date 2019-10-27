+++
title = "Talk:Loops/While"
description = ""
date = 2018-12-03T19:11:47Z
aliases = []
[extra]
id = 2836
[taxonomies]
categories = []
tags = []
+++

Perhaps it should be specified that the number should be int or the number to count down to should be '1' rather than '0' The reason is that for languages that support the numbers rather than integers, the value only tends towards '0' never reaching it :)

==task requirements==

It seems the most simple tasks are so easily misunderstood.

Print, and [''then''] divide.   I can't see the interpretation of dividing and ''then'' printing. -- [[User:Gerard Schildberger|Gerard Schildberger]] 01:32, 30 March 2013 (UTC)

== REXX ==

I am at a loss!

Why was version 2 removed by Siskus??

We have now versions 1 3 and 4

I thought that thge extraneous version 1 was the bad joke (no loop whatsoever)

I am leaving this battlefield. sorry --[[User:Walterpachl|Walterpachl]] ([[User talk:Walterpachl|talk]]) 20:47, 30 October 2013 (UTC)

-----

If I recall correctly, the (original) version 2 was removed by Sikus (no reason was given by him), and then other versions were changed (probably at the same time) to reflect where I had originally referenced some of the other versions   (I hope that is clear as mud).   It sure was a mess for me to fix it all up, as I entered those REXX versions (some of my very first entries) very early in my Rosetta Code life, and no had back ups of the REXX source, and also none of the REXX section headers, so I had to really dig back in time to get the original code versions and section headers (comments).

Once I got all that fixed up, ''another'' REXX version 1 was entered, whenceforth I flagged the bogus (1st first version)
with multiple pink-flags, least of all, the first 1st version couldn't even execute (run), let alone produce output (but incorrect output was shown), and there were at least two major errors such that the REXX first first version would raise a (fatal) "syntax error" (or rather, "syntax errors").   Other program errors were also mentioned in my pink-flagging.   I don't know where he got the output from, which included part of a REXX comment.   The resultant 1st 1st version looked like it was composed by someone trying to imitate someone's first attempt at the REXX language. 

[The bad bad REXX 1st first version was then deleted by you.]

It sure was a lot of wasted time just to get back to a decent version(s) of the original posting/submission.   I wish I could get all that wasted spent time back.   You may have thought it was a joke, but the wasted time was not.   If it was a joke, I didn't see any humor in it all all, I perceived it as malicious, for it was a lot of time spent in making it right again, not to mention the non-humorous non-joke. 

Also, at the same time, another REXX entry was "flagged" as   <nowiki> {{omit}|REXX}} </nowiki>   with no explanation.   There was an existing REXX entry and it was there for quite a while   [code popularity].   I don't have the time or inclination to back-track all of Sikus' changes to see if he did any other damage, joke or not, incorrect (and flagrantly bad at that) code still has be to corrected. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 21:24, 30 October 2013 (UTC)  

-----

Now, I see Sikus is at it again, this time, removing the 2nd REXX version.   I wish he would just try to improve his own code instead of deleting other's. 

The REXX versions now refer to another REXX version's output which is no longer there. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 21:24, 30 October 2013 (UTC)

:: thanks for the repair. I deleted the added "1st" version which I considered ridiculous.
It was not worth your enumeration of errors that came with it. 

::: And what ... just leave the ridiculous and incorrect REXX program there (and for how long?).   It serves nobody to have such an erroreous program on Rosetta Code that shows no worthwhile (or even correct) "solution". -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 22:11, 30 October 2013 (UTC)

:::: NO. just delete it (that's what I did). It serves nobody... --[[User:Walterpachl|Walterpachl]] ([[User talk:Walterpachl|talk]]) 12:23, 31 October 2013 (UTC)


 sorry for interfering. 

Could Siskus pls comment about his or her intentions here??? --[[User:Walterpachl|Walterpachl]] ([[User talk:Walterpachl|talk]]) 22:03, 30 October 2013 (UTC)

== DC ==

The DC solution looks like a DO-UNTIL loop rather than a WHILE.

```txt

$ dc -e '_4[p2/d0<p]dspx'
-4

```

A WHILE should test first and execute the body only if the test succeeded.

Alternative version (needs review, comment, adopt, ...):

{{works with|GNU Dc}}
{{works with|OpenBSD Dc}}
If the comments are omitted: {{works with|AT&T Dc}}

```txt

$ dc<<end
[ q ] sQ      # makro Q : quit 2 levels (quit Q and calling level)
1024 [ d 0!<Q # compare, quit if true
       p 2 /  # print and divide
       lW x   # tail recursion
     ] d sW x # duplicate store as W execute TOS
end
1024
512
256
128
64
32
16
8
4
2
1

```


--[[User:Yeti|Yeti]] ([[User talk:Yeti|talk]]) 04:42, 26 November 2018 (UTC)
