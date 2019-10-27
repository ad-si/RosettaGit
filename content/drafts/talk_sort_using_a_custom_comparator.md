+++
title = "Talk:Sort using a custom comparator"
description = ""
date = 2018-08-20T18:46:57Z
aliases = []
[extra]
id = 1871
[taxonomies]
categories = []
tags = []
+++

This is a cleanup of the old [[Sorting]] page.  I've started it with examples for C, Haskell, Perl, and PHP, and I'll try to get Java and Objective-C in soon.  I don't believe the task (strictly speaking) is possible in a UNIX shell without invoking a more complete language like Perl.

I need people to verify/fix any relevant examples from the other languages in Sorting, and add them here (edited for consistency of course).  --[[User:Bob9000|Bob9000]] 06:23, 30 January 2007 (EST)
:I created a template to help you out: [[Template:split-review]]...Add it to the tasks you create, just under <nowiki>{{task}}</nowiki>.  The text to add is <nowiki>{{split-review}}</nowiki>.

:There will be a central Maintenance depot soon.  Right now, [[:Category:Maintenance]] will have to suffice.  

:Thanks for all your efforts...I've been watching the Recent Changes page, and you're one of the top two or three contributers. --[[User:Short Circuit|Short Circuit]] 11:11, 31 January 2007 (EST)

== Ada example incorrect ==

I do not understand how the Ada example is incorrect. 
It uses a custom comparator.
It performs the two sorts required (descending length and ascending lexical sort).
What is the complaint?

:: I'm not the complainant, but the Ada example does not actually appear to do what the task is asking, namely produce ONE list of strings that are sorted by length AND sorted lexicographically where strings are the same length. The first output result appears to be correct (strings are sorted by length and where strings have the same length they also happen to be sorted lexicographically) but that looks more like chance when I look at the code. Or if "lexicographic" is a default secondary sorting criterion in Ada then this should be spelled out. And it is unclear why there are two output lists. Or at least that what I understand the task to be; maybe I'm kinda misreading this somehow. [[User:Sgeier|Sgeier]] 10:18, 20 September 2007 (MDT)

---------------------

I see the problem. My second sort is case sensitive. Thanks for your patience.
:Hm.  I didn't see when the <nowiki>{{incorrect}}</nowiki> template was added.  Normally, when such templates are added, the reason should be mentioned in the Talk page.  I don't know why that didn't happen in this case.  I'll check the template and see if I can't clarify it a bit. --[[User:Short Circuit|Short Circuit]] 23:37, 18 February 2007 (EST)

---------------------
Unfortunately it is still incorrect. See the C# discussion below for more info, as the C# (used to) fail for the same reason, Thanks. --[[User:Paddy3118|Paddy3118]] 06:43, 20 November 2009 (UTC)

---------------------
I've created a new solution to the task, but it's not based on the old code at all.  Should I add mine to the code that's there now or replace the incorrect code? --[[User:Ezephyr|Ezephyr]] 21:05, 24 April 2010 (PST)

:For the moment, I'm just going to add the code that I wrote to the end of the Ada section.  If no one objects within a couple days... I'll switch the order or something. --[[User:Ezephyr|Ezephyr]] 21:05, 24 April 2010 (PST)
:So it's been a couple five days.  I'm 100% positive that my code is correct and solves the stated problem description, so I'm going to go ahead and remove the tag and the broken code.  --[[User:Ezephyr|Ezephyr]] 23:16, 29 April 2010 (PST)


Output is incorrect. Set is before set, but has to be first set and then Set.  --[[User:GetBreak|GetBreak]] 19:20, 20 August 2018 (EET)

:Um. Just to be clear, you ''did'' read the last line of the task description? The one that states: <blockquote>'''Note:'''   Lexicographic order is case-insensitive.</blockquote> It doesn't say anything about sort stability though, or the ordering of words that compare the same, so Set, set is just as valid as set, Set. The AWK and C entries do look a little suspect though.

:BTW, when you mark an entry incorrect, the correct ''(heh)'' format is: 

    <nowiki>{{incorrect|Language name|Short explanation of what is wrong}}</nowiki>

:If you don't follow that format it becomes difficult to figure out what the problem is and problematic entries won't be reported on the "examples needing attention" page. Thanks. --[[User:Thundergnat|Thundergnat]] ([[User talk:Thundergnat|talk]]) 17:36, 20 August 2018 (UTC)


::: I fixed the AWK version (for GAWK, not sure about TAWK as I don't have access to that) as per your observations re sort stability. --[[User:Tigerofdarkness|Tigerofdarkness]] ([[User talk:Tigerofdarkness|talk]]) 18:46, 20 August 2018 (UTC)

== Some examples remain incorrect ==

I have not examined all the solutions by any means, but both the Ada and Common Lisp examples appear to not fit the specifications. Sgeier's elaboration (2007-09-20) is cogent, but some posted solutions continue to not implement that sorting function.

For the record, I'm not sure I understand the final sentence of the task. ('Use a sorting facility provided by the language/library, combined with your own callback comparison function.') Is the use of callback crucial here?  Does the J solution I posted satisfy it (and how would I know if it does)? Perhaps the task specification could be more language-neutral?

Finally, many of the examples use test data that do not effectively test the requirements.

--[[User:TBH|TBH]] 22:39, 8 February 2008 (MST)
:My previously mentioned uncertainty is largely relieved by the replacement J code posted by [http://www.jsoftware.com/jwiki/DanBron Dan Bron], which clearly uses the primary sorting facility of the language. Whether the customizing aspect counts as a callback should not actually matter. --[[User:TBH|TBH]] 23:06, 16 March 2008 (MDT)
::I studied callbacks enough to identify that the adverb technique Dan used definitely counts as such. --[[User:TBH|TBH]] 20:26, 4 April 2008 (MDT)


==C# Example is incorrect==
Hi Ronw,

The C# example is incorrect as the task asks for one callback function that sorts: "in order of descending length, and in ascending lexicographic order for strings of equal length."
This means that when strings are of equal length, and only then, their order is to be in terms of increasing, case-insensitive, character order.
Your output file would have headings for only ''Unsorted'' and ''Sorted'' where strings 'sorted' and 'sample' should be switched; as should 'to' and 'be'. --[[User:Paddy3118|Paddy3118]] 19:32, 19 November 2009 (UTC)
:Yep, I missed that part. It should be fixed now. --[[User:Ronw|Ronw]] 14:17, 19 November 2009 (PST)
:: Ta! :-)     --[[User:Paddy3118|Paddy3118]] 06:53, 20 November 2009 (UTC)
