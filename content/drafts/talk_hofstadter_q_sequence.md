+++
title = "Talk:Hofstadter Q sequence"
description = ""
date = 2016-11-13T21:55:46Z
aliases = []
[extra]
id = 10713
[taxonomies]
categories = []
tags = []
+++

==found the 100000th term,any news?==
so i found the 100000th term,and ive heard something about the challenge...why no one have solved it?then i tried,just for its lovely chaos,i found it.i used a modified python code for this.i just modified it so that i could list up the numbers...
p.s. also,the file was too big to post it as a plain text,so i posted it in my "repository".anyways,kindly check it for me.if the choices are valid.ill try a second run,if requested

[http://geocities.ws/pluman/qseq/ the website,includes the code and the output]

more stories to come!

edit:i found out,in my 3gb of ram,the computer crashed when computing the 2^27 th term.it was a good ride,hte explorer crashed,waitng for that blue scree,and python saved it..memoryerror

edit:my highest,i think is this:q(26)34289195
--[[User:Laptops|Laptops]] ([[User talk:Laptops|talk]]) 12:56, 3 June 2015 (UTC)

: This is not challenge site, and the task is not meant to be a challenge, especially not the 100,000th term, which many of the code examples on the task can already do.  That number is mentioned somewhere on this talk page because it causes very deep recursions and stack overflow if you use a purely recursive method, but it's nothing special.  --[[User:Ledrug|Ledrug]] ([[User talk:Ledrug|talk]]) 20:36, 3 June 2015 (UTC)

:: Indeed, I just checked, and as of today, only 5 computer programming examples (out of 49)   ''didn't''   execute their programs with (or for) 100,000 terms.   It should be noted that this Rosetta Code task didn't specifically ask for (displaying) the 100,000<sup>th</sup> term, so almost every example didn't show it.   I modified the three REXX programming examples to show the 100,000<sup>th</sup> term;   it was a very minor addition to one of the existing REXX statement. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 22:07, 3 June 2015 (UTC)

:: I just added support to show the 1,000,000<sup>th</sup> term for the three REXX programming examples, it was no big deal (even for the recursive example). -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 22:26, 3 June 2015 (UTC)

==Python non-recursive solution==
Thanks Ledrug. I had a brain freeze on removing the recursion and blundered on with the accommodation of the recursion limit solution. --[[User:Paddy3118|Paddy3118]] 18:34, 23 October 2011 (UTC)
==Recursion limit issue==
As [[User:Ledrug|Ledrug]] pointed out wrt. to my Dart solution with cache, there is a problem when calling the function with a large number first (e.g. q(100000)) since the cache is filled recursively counting from n to 2 if it is empty (happens with the Java solution for example).
This doesn't occur in the calculations necessary to solve the tasks since this way the cache is filled ascending.
Maybe the task description should be changed whether it is required to accommodate this. --[[User:AlexLehm|AlexLehm]] 09:53, 28 October 2011 (UTC)

:I updated the task. The recursive Python solution has a similar issue but with Pythons recursion limit being set at only 1000 by default. The extra credit solution gets around this. 
:I think that solutions should address such issues in an extendable way if at all possible (not hard-coding something that works just for this tasks value of n). --[[User:Paddy3118|Paddy3118]] 14:38, 28 October 2011 (UTC)

:: My view is: since examples all define <code>Q</code> as a function, it should try its best to ensure the return values don't depend on call history, because that's what one tends to expect.  It's best not to (unnecessarily) surprise people. --[[User:Ledrug|Ledrug]] 16:25, 28 October 2011 (UTC)

::: Not all examples define <code> Q </code> as a (recursive) function.  At least two examples don't.  -- [[User:Gerard Schildberger|Gerard Schildberger]] 18:58, 26 May 2012 (UTC)

==<nowiki><math></nowiki> not working?==
The equation in the task description is not being rendered for some reason, even though I don't think it has been edited. Could someone check this? Thanks. --[[User:Paddy3118|Paddy3118]] 06:46, 14 December 2011 (UTC)
: It appears that all <nowiki><math></nowiki> handling on this page is entirely broken (I tested with a very simple version that shouldn't have required an external renderer; “<tt><nowiki><math>1</math></nowiki></tt>” which renders as “<math>1</math>”). Haven't checked on other pages yet (not that I expect it to be working there) but it could be associated with the recent updates. –[[User:Dkf|Donal Fellows]] 09:09, 14 December 2011 (UTC)
:: [http://irclog.perlgeek.de/rosettacode/2011-12-14 IRC log] says that <nowiki><math></nowiki> now works. However, I find that <nowiki><math></nowiki> remains broken until someone edits the page. I fixed this page with [http://rosettacode.org/mw/index.php?title=Hofstadter_Q_sequence&diff=128336&oldid=128261 a minor edit to Ruby]. Any single edit seems to reset the cache, so <nowiki><math></nowiki> works again. --[[User:Kernigh|Kernigh]] 19:00, 14 December 2011 (UTC)


==Formulae hidden to most browsers by under-tested cosmetic edits at 03:46, 20 April 2016 ==

Under-tested cosmetic edits made to the task page at 03:46, 20 April 2016, including the injection of spaces around expressions in &lt;math&gt; tags, have left some or all of the task description formulae completely invisible to all browsers which display the graphic file version of formulae rather than processing the MathML (this is, in fact, the majority of browsers). The MediaWiki processor does not currently expect such spaces, and generates syntactically ill-formed HTML if they are introduced. Other aspects of these cosmetic edits may further compound the problem. [[User:Hout|Hout]] ([[User talk:Hout|talk]]) 19:53, 22 September 2016 (UTC)

: Repaired today [[User:Hout|Hout]] ([[User talk:Hout|talk]]) 21:55, 13 November 2016 (UTC)
