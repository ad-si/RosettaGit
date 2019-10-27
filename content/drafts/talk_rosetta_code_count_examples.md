+++
title = "Talk:Rosetta Code/Count examples"
description = ""
date = 2019-01-22T02:29:11Z
aliases = []
[extra]
id = 3364
[taxonomies]
categories = []
tags = []
+++

The method of counting header template uses may still give an incorrect count of programming examples, but I think it's as close as we can get without getting unreasonably complex. For instance, we could count the number of start lang tags, but some examples use pre and some more still have one example split into sections with explanations. The method used for this task does not account for iterative and recursive solutions for one task, or splits like on the [[String Length]] page. What we end up with for the total across all tasks is the same as counting the members in each language category and subtracting the language implementations (which seems more complicated). --[[User:Mwn3d|Mwn3d]] 19:30, 9 February 2009 (UTC)

== Count ===(.*)=== ==

We can count anything under "triple equals" but it will duplicate results. Like:


```txt

1. Python
 1.1. A
 1.2. B

```


Will count 3, not 2.

Other solution is count only "triple equals" in articles with them. But some tasks like [[HTTP Request]] have only one multiple solution tasks, That will count only [[Erlang]]. 

A complex solution, will be read all sections, but will take a long time, and will be buggy.

== Traceback for problem with Python version ==


```txt

Traceback (most recent call last):
  File "count.py", line 8, in <module>
    y = urllib.urlopen("http://www.rosettacode.org/w/index.php?title=%s&action=raw" % t)
  File "d:\Python26\lib\urllib.py", line 87, in urlopen
    return opener.open(url)
  File "d:\Python26\lib\urllib.py", line 178, in open
    fullurl = unwrap(toBytes(fullurl))
  File "d:\Python26\lib\urllib.py", line 1028, in toBytes
    " contains non-ASCII characters")
UnicodeError: URL u'http://www.rosettacode.org/w/index.php?title=Catmull\u2013Clark_subdivision_s
urface&action=raw' contains non-ASCII characters

```


== cmlimit may not be over 500 for users ==
Attempts to read more than 500 tasks using the XML query fail with this error.  Everybody seems to have used this method (rather than downloading the HTML page for example) so presumably it's not considered to be incorrect. [[User:RichardRussell|RichardRussell]] 12:29, 17 November 2012 (UTC)
: Nevertheless I have modified the BBC BASIC solution to read the full set of tasks. [[User:RichardRussell|RichardRussell]] 12:14, 21 November 2012 (UTC)

==comparing programming language entries==
I think it would be a good idea to show when a program is executed so that outputs could be compared (somewhat) to other entries, even though it may not be the exact time-frame.     -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 01:26, 21 January 2019 (UTC)

:I don't disagree, but that ship sailed nearly 10 years ago. Some of the examples are wildly inaccurate, (Ring and Sidef are two obvious examples at this point,) but since there is no metric to test against, it is difficult to say whether a particular example is accurate or not. I spent quite a bit of effort making the Perl 6 example as accurate as possible. If you check a example count on my list, and check the example count on the actual page, they match. At least until new task examples get added. --[[User:Thundergnat|Thundergnat]] ([[User talk:Thundergnat|talk]]) 23:07, 21 January 2019 (UTC)

==latest changes making output not viewable on two browsers==
With the latest changes, some of the output isn't viewable on the FireFox and/or Internet Explorer versions that I'm using.     -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 23:10, 21 January 2019 (UTC)

:Sorry about that, entirely my fault. Thanks to [[User:SqrtNegInf|SqrtNegInf]] for fixing it before it dragged on too long. --[[User:Thundergnat|Thundergnat]] ([[User talk:Thundergnat|talk]]) 02:29, 22 January 2019 (UTC)
