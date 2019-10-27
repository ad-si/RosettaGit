+++
title = "Talk:Semordnilap"
description = ""
date = 2018-11-03T14:37:17Z
aliases = []
[extra]
id = 12296
[taxonomies]
categories = []
tags = []
+++

== Ruby Implementation ==

Do we really want literal copies of the dict file on the task page? --[[User:Rdm|Rdm]] 13:58, 10 September 2012 (UTC)

I added a note to the submitters talk page and have chopped most of it from the example for now. --[[User:Paddy3118|Paddy3118]] 17:13, 10 September 2012 (UTC)

What sort of a world is it where if someone says your dict looks big an Irish number chops it? From http://rosettacode.org/wiki/User_talk:Nigel_Galloway#Huge_Ruby_example I see I have two choices:
:A) modify it to read the dict from a file
:B) move it to a sub-page on its own

I choose C. Well B+ really. The + is, it finds a use for history, which I see relied on too often.--[[User:Nigel Galloway|Nigel Galloway]] 11:53, 12 September 2012 (UTC)

:Does this mean you like pages being really big and slow to load (and perhaps enough to crash some phone browsers)? If so, perhaps we could all include a copy of that same dictionary in each implementation on the task page?  As an upper bound (99 bottles of beer has what... 200 implementations?) the task page probably would not get too much bigger than 40 megabytes...

:That said, you also left out choices D), E) and F) (fetch dict from the url every time the code runs, or represent dict as a compressed blog, or do not bother implementing the task and let someone else do it).  And, if we put some thought into it, we could probably come up with a G), H) or I) also.  Maybe H) can be your "look up the big version in history"?  --[[User:Rdm|Rdm]] 12:28, 12 September 2012 (UTC)

:Hi Nigel, as I tried to explain, we try not to have overly large examples, and if you look around, those measures I outlined are used by others when they have large examples to post or others may create sub-pages for them without the snide remarks. 
:There are other examples and tasks that use the dictionary and yours is the first example that chose to stick the whole dictionary in the source code. The program you wrote may work, but that is not the only consideration for its inclusion, we also like to have a main task page that doesn't put undue load on browsers and ~200K of source I thought was too much. (The line length was excessive too). --[[User:Paddy3118|Paddy3118]] 16:47, 12 September 2012 (UTC)

:P.S. If you think the site makes the rules up as it goes along, then in essence you are correct. The users are encouraged and empowered to make RC 'better'. We may struggle at times, but I think Rdm and I made a reasonable request, and made an appropriate change. --[[User:Paddy3118|Paddy3118]] 16:47, 12 September 2012 (UTC)
: Nigel, relying on page history that way slams my server. Seriously. Don't do that. MediaWiki only stores the most recent revision verbatim, and diffs from one revision to the next. In order to show a page fifteen revisions old, the server has to apply fourteen diffs. As for the rest of it, I recommend modifying the task to read from a dictionary file. Most non-Windows systems have a words file handy, and a link can be provided to the remainder. --[[User:Short Circuit|Michael Mol]] 17:10, 12 September 2012 (UTC)

== well formed dictionary ==

Perhaps this may be too general a statement, but I wish the dictionary specified would have capitalized or uppercased words in it (along with some words that have a blank in it, hyphenated words, words that start with an apostrophy [''''til''']) to test the program's metal a wee bit.  Having words that may be capitalized would make the dictionary more realistic, or more to the point, make the ''program'' more realistic.  This would mean having words like '''god''' ''and'' '''God''' in the dictionary (not to mention '''I'''), and thereby causing programs to handle "duplicate" words, and be sensitive to word case.  Also, a true dictionary would have multiple entries for nouns vs. verbs and also homonyms (which would look like two different words), but in a dictionary that only lists unique entries, that might be moot here. Having a list of words, with each section (A,B,C...Z) separated by one or more blank lines would also be common.  Having a well formed dictionary just allows programmers to assume too much (such as the words being in some kind of alphabetic order).  But assuming everything will be in lowercase just makes for lazy programming (and I'm not saying that in a perjorative way).  I don't think programming examples should be written just for the dictionary supplied, and from those assumptions, start taking shortcuts. I wrote the 2nd version of the REXX example to handle all that, with just an extra line of code.  I'd also like world peace, ... -- [[User:Gerard Schildberger|Gerard Schildberger]] 22:25, 17 September 2012 (UTC)

== Dead link ==

The file to be used for input appears to be missing or blank. I attempted to use a file of the same name I found in someone's github repository, but I am getting different output from the other examples and I suspect it may be a different file. Any advice on how to proceed? --[[User:Chunes|Chunes]] ([[User talk:Chunes|talk]]) 13:37, 3 November 2018 (UTC)

:After some poking around the website, I found this working link: http://wiki.puzzlers.org/pub/wordlists/unixdict.txt. I'll give this one a shot. --[[User:Chunes|Chunes]] ([[User talk:Chunes|talk]]) 13:49, 3 November 2018 (UTC)

::The issue was that my code was incorrect. Nevertheless, I've updated the link in the task description to the working one. --[[User:Chunes|Chunes]] ([[User talk:Chunes|talk]]) 14:36, 3 November 2018 (UTC)
