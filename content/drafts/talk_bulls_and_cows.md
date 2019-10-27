+++
title = "Talk:Bulls and cows"
description = ""
date = 2012-07-05T14:39:39Z
aliases = []
[extra]
id = 4219
[taxonomies]
categories = []
tags = []
+++

==Scoring==
The scoring algorithm gets more interesting if duplicate digits are permitted... —[[User:Dkf|Dkf]] 09:54, 20 May 2009 (UTC)
:You might try first checking for Bulls, and 'crossing-out' any found, then checking for cows in the remainder? (But the increased complexity in interpreting the score might make the game worse to play). --[[User:Paddy3118|Paddy3118]] 20:09, 20 May 2009 (UTC)
::My understanding is that Bulls and Cows traditionally disallows repeated digits.  However, the similar commercial board game Mastermind does allow duplicates.  The way scoring works there, you get credit for at most one bull (black peg) or cow (white peg) per digit ''in the solution'' (not in the guess).  So if the code is 1122 and you guess 1112, you get three bulls (for the first two 1's and the 2), but no cow for the third 1, because both 1's in the code are already accounted for. -- [[User:Markjreed|Markjreed]] 03:53, 29 January 2012 (UTC)

::: This is the methodology that the REXX program (version 1) uses. -- [[User:Gerard Schildberger|Gerard Schildberger]] 22:53, 4 July 2012 (UTC)
==Form of target number==
The description says that the digits must be from the set 1 through 9, but [http://en.wikipedia.org/wiki/Bulls_and_cows Wikipedia] - and some of the solutions on this page - allow zeroes.  Also, the description should probably specify whether or not repeated digits are allowed.
-- [[User:Markjreed|Markjreed]] 00:20, 31 January 2012 (UTC)

==malformed numbers==

The task didn't elaborate on what a ''malformed'' guess was, but when I programmed a version (cough-cough, 31 years ago), the user was allowed to guess such things as:
* 12x5
* ..73
* 4 5 7 9
* 7 , , 4
* 42?9

Very inexperience beginners (and most children, see below) begin learning the logic of the game by guessing:
* 1111
* 2222
* 3333
* 5
* 79
until they've found four digits that are in the number, and then narrow it down with their own perculiar logic.

The (original) program was intentionlly written to allow these forms of guesses and reasoning (logic).
It was a goal of the program to not to force any sort of logic or restrictive rules upon the guesser.
If the guesser could think it, it was allowed.

I added program logic to the REXX (version 1) program, if only to comply to what ''malformed'' could mean.

:: (I've since rescinded those restrictions.) -- [[User:Gerard Schildberger|Gerard Schildberger]] 14:16, 5 July 2012 (UTC)

The original program also allowed any number of ''digits'', and also allowed specification of what ''digits'',
so that it could mimic the game ''Master Mind'', a popular game of that time.  This was the default mode.

A comment on the program's pegigree: quite a few of the supervisors (where I worked at way back then) were 
allowed to have a (CRT) terminal at home (for working after hours of course, of course), and as a result, their kids could get on-line and play, which back
then, it was quite a treat to play on a computer, and this was before home computers were common.  Even at that,
home computers were NOT cheap. Of course, it was against the company rules to use a company's mainframe computer for such
non-business thingys, but almost all the mainframe computers much very much idle after-hours and on weekends, of course.
When asked by my 2nd level supervisor, I glady wrote some games on my own time in EXEC2, a language on the VM/CMS
system. And who wanted to throw rocks at one's own bosses? -- [[User:Gerard Schildberger|Gerard Schildberger]] 23:35, 4 July 2012 (UTC)

::: (added parenthesized comments and ''mainframe'' words to clarify what kind of computer(s) I was referring to.) -- [[User:Gerard Schildberger|Gerard Schildberger]] 14:21, 5 July 2012 (UTC)

:Hmm. Thanks for the info on the variants where you introduce a placeholder not-a-number symbol. I guess it cannot match anything and would serve to reduce complexity when someone wants to test what they think they may know about a partial solution?

:If I were playing with more digits then I guess it would allow me to reduce the problem to something I could work out in my head. --[[User:Paddy3118|Paddy3118]] 03:00, 5 July 2012 (UTC)

:: Yes, anything not a digit (in this case) doesn't match anything.  It's very helpful when your mind is a bit cloudy (like enjoying some down-time after an eighteen hour coding tear), or maybe you're dealing with a "bulls and cows" game variant with more than four digits.  Also, kids like to use a NaN guess as they seem to prefer to deal with a smaller subset of logic.  Well, ok, ok, so do I at times. I was playing a few games as I always check out the game after I make a change (or an improvement, ha!), and once, I just couldn't find a solution (I had zigged instead of zagged when coding a change), and the "cows" information was wrong. I had to ignore the cows and try only for the bulls. -- [[User:Gerard Schildberger|Gerard Schildberger]] 03:12, 5 July 2012 (UTC)

::: Does anybody besides me think that was a weird thing to say? -- [[User:Gerard Schildberger|Gerard Schildberger]] 14:16, 5 July 2012 (UTC)

-----

Note: the above section was re-added and slightly re-formated as I had to retrieve it with cut and paste from my E-mail reader via Rosetta Code's notification of changes. I left the original timestamps intact. All the original line-ends (newLines) [if any] have been lost. -- [[User:Gerard Schildberger|Gerard Schildberger]] 14:16, 5 July 2012 (UTC)

==REXX version 2==

In the REXX version 2 program comments (quoting changes from version 1), it's mentioned that a ? (question mark) is not everywhere a valid symbol for REXX.  

For what version(s) of (any/a) REXX interpreter (or compiler) isn't the questin mark a valid symbol? 

The statement of ''rightHandSide is mandatory'' isn't strictly true for any classic REXX interpreter that I use, but it may be true for some of the object-orientated REXXes (ooRexx, NetRexx, and ROO [or ROO!]).  If that statement was true, then a majority of REXXes would need changing, the least of which are the REXXes that executed version 1.

It's implied that the '''S''' subroutine doesn't work for all plurals.  Is there ''any'' way it could fail (as coded) for the REXX program version 1?  If not, I'd like the offending statement retracted.  The '''S''' subroutine was only meant for the REXX program (version 1) as coded; it certainly wasn't meant for a general purpose, one-size-fits-all, non-standard/irregular (English) noun pluralizer (foot, datum, century, radius, ...), --- just for '''bulls''' and '''cows'''.  There are also collective nouns which are a whole 'nother kettle of fish. I see no reason to throw the baby out with the bathwater.  

I gave the program to someone that has access to IBM TSO (and he graciously executed the [illegal] game for me, and he/she WILL remain nameless, risking their job to run a stupid game, no less], and it ran without changing anything, least of all the '''|'''  (or) character.  I'm not sure what (if anything, except for the obvious) got changed "under the covers", so to speak, or if a customized version of the uploader/converter program was used. I'm also not sure if the ''boxing'' (extended ASCII) characters got translated correctly, but the REXX program never-the-less ran. I didn't press for details or if they got four bulls. (We probably should've used a dead drop and avoided the black helicopters that we can't see.) Does the program (the one that was used) that downloaded/uploaded/converted ASCII to EBCDIC working properly?  Is there a problem with ''code pages''?  I see that the REXX version 2 program uses the '''||''' (double '''|''' [or-or] for concatenation), so I can see no reason to change the '''|''' to  '''!''' (an exclamation mark) as mentioned in the comments in version 2.  Did you mean to say that you changed a '''\''' (backslash) to an '''!''' (exclamation poiny)?  I know that the CMS and TSO REXXes support the backslash for negation. [Both also support the ''forward slash'' for negation!] I can understand that possibly your keyboard and/or display won't show it, but what does your TSO (or REXX) complain about that you're getting an error?  What is the actual character in the program (the backslash)? 
What did the backslash character get converted/translated to (the ''hex'' value, not the gylph)? Could your code page have more than one backslash? --- As most IBM 327x terminals support two (each) '''{''' and '''}''' braces, or sometimes more humorously: chickenlips. I suppose it's possible that other characters could be "duplicated".  The (EBCDIC) '''|''' and the long '''│''' and the broken vertical bar '''¦''' bring back angst-like memories, some terminals show them all as a single character.

I normally try to use a liberal amount of whitespace for REXX source readability (elsewhere), but I can appreciate some people's programming style of more compact program code;  most often I try to not introduce blank lines at Rosetta Code because of program length, with REXX being not-so-terse as some languages.  One of REXX's design philosophy was ease of program readability (that is, make it natural to read). I usually draw the line when introducing dead code, however, with the exception of showing an alternate method(s) of coding a subroutine (or say, an alternative method to express a complicated IF or WHEN logic) for instance. Sometimes, for short programs, I normally introduce another version if brevity allows it. -- [[User:Gerard Schildberger|Gerard Schildberger]] 00:02, 4 July 2012 (UTC)

----
The changes of ? and | were necessary for the GERMAN MVS/TSO
Version 2 uses || because that's ok on ooRexx.  
qq='' is there for the sake of ooRexx
The s-removal was to show another way to provide that functionality (if bulls and cows should be changed to fox and geese)
--[[User:Walterpachl|Walterpachl]] 04:28, 4 July 2012 (UTC)

:: The removal of the '''S''' subroutine wasn't being questioned, only the comment on the example page for REXX version 2 saying that the version 1 code didn't work for all plurals. It was asked if it could fail for the program as coded (for bulls and cows). It wasn't stated anywhere that it worked for all plurals. -- [[User:Gerard Schildberger|Gerard Schildberger]] 14:39, 5 July 2012 (UTC)
 
----
Actually the change of ? to qq was NOT necessary. ? IS allowed on TSO. However, I never use(d) it.
--[[User:Walterpachl|Walterpachl]] 05:18, 5 July 2012 (UTC)
