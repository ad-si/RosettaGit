+++
title = "Talk:Letter frequency"
description = ""
date = 2015-08-04T21:24:17Z
aliases = []
[extra]
id = 10548
[taxonomies]
categories = []
tags = []
+++

== Task description ==

More detailed task description is needed.
For example, should we only count ASCII letters A-Z? Case in-sensitive?

:: Or for that matter, what is a letter?  For what language? Most programs seemed to assume the Latin alphabet for English. -- [[User:Gerard Schildberger|Gerard Schildberger]] 01:47, 30 June 2012 (UTC)

Maybe the results can be displayed with whatever method is the most convenient?
I assume opening the file is required (not just handle a file that is already open)?

Since the first solutions were copied from another page and may not be correct solutions, those should be marked somehow. (Was there some specific tag for this purpose?)
At least the Pascal solution seems to have nothing to do with the task.

--[[User:PauliKL|PauliKL]] 13:03, 19 September 2011 (UTC)

:I took it as anything that is un-said should be whatever is convenient to the implementer. 
:* ASCII? Count whatever the file open routine makes most easy.
:* Case sensitivity? Count what you get without applying any uppercase/lowercase filters.
:* Output format? whatever is convenient.
:Open the file in your code? I interpreted this as being a ''requirement''.
:This leaves the guts as being a way to iterate through the characters keeping count. --[[User:Paddy3118|Paddy3118]] 14:58, 19 September 2011 (UTC)

"Letter frequency" is not the same as "letter occurences".  The title hints that more is needed in the task description.  It would seem that some description of output is required as well. --[[User:Demivec|Demivec]] 16:43, 9 November 2011 (UTC)

It seems that many program examples interpreted a ''letter'' as a ''character''.  A (Latin) letter has two forms:  its uppercase and lowercase version.  So if two '''H''' characters and three '''h''' characters were in a file, then there would be five occurrences of the letter '''aitch'''. [''Aitch'' is the English name for the letter '''H''' or '''h'''.]  The task description could've been more clear on that point, so for the REXX version 1, a count was done for each (Latin) letter, AND also for each character, and the counts are provided in separate lists.  This made the loosey-goosey interpretation moot.  Since it wasn't stated what a letter ''is''   (I used the primary definition that it's any of the symbols of an alphabet), it seemed appropriate to provide a both lists:   a list of letters, and a list of all characters (for any language's alphabet). -- [[User:Gerard Schildberger|Gerard Schildberger]] 21:19, 25 July 2012 (UTC)

:The English pronunciation of the letter 'H' is haitch surely! :-)
--[[User:Paddy3118|Paddy3118]] 08:48, 26 July 2012 (UTC)

:: One may pronounce it that way (depending upon which side of the pond you're on), but the spelling of the letter isn't that.   <big><big><big> <font color="orange"> ☻ </font> </big></big></big>   -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 21:13, 4 August 2015 (UTC)

In hindsight, it would've been nice to make a requirement to use the program example as the primary input (but not necessarily the only input); that way, everyone could see what was used for its input.  At least one example used UNIXDICT.TXT, which has no capital letters. Another example only counts capital letters.  Still others showed a list, but excluded most of the counts, so it can't be verified if the uppercase letters were included (or not) with the lowercase letters, or kept as separate counts. -- [[User:Gerard Schildberger|Gerard Schildberger]] 21:19, 25 July 2012 (UTC)

== a few remarks for Rexx ==

A few typos:
carraiage -> carriage
occurances -> occurrences
independant -> independent

: (see next section on typos and misspellings) -- [[User:Gerard Schildberger|Gerard Schildberger]] 22:17, 25 July 2012 (UTC)

and some more substantial observations:
y=d2x(L); if @.up.y==0 then iterate  /*zero count?  Then ignore letter*/
  c=d2c(L)                             /*C is the hex version of of char*/

In such cases I use cnt.0up.y so that a possible variable up never interferes
actually y is the hex version  , drop one ‘of’

:: I don't understand what you are observing, substantial or not.  Thank you for your suggestion that I modify the version 1 code, but one of "them" can't be dropped as REXX version 1 keeps track of ''letters'' as well as ''characters'', and both of "them" are needed for their respective (count) lists. -- [[User:Gerard Schildberger|Gerard Schildberger]] 22:01, 25 July 2012 (UTC) 

@.=0                                   /*wouldn't it be neat to use Θ ? */
why not use cnt. ?

: Because it's my style of coding.  I use '''@.''' for important stemmed arrays, and it makes it easier to find in the code where that stemmed array is referenced. Using such constructs as '''c.c''' is very confusing to a novice reader of REXX.  There's two variables being used, '''c''' and '''c''', one is the stemmed array name, the other the stemmed array index. But --- defending one's programming style will just start a religious war, so there is no sense in pointing faults in another's coding style. -- [[User:Gerard Schildberger|Gerard Schildberger]] 22:01, 25 July 2012 (UTC)

In the discussion I read:
“Case sensitivity? Count what you get without applying any uppercase/lowercase filters”

:: No filters were used in REXX version 1, ''letters'' and  ''characters'' were counted correctly without case sensitivity, and a count was provided for each. -- [[User:Gerard Schildberger|Gerard Schildberger]] 22:01, 25 July 2012 (UTC)

upper c -> c=translate(c) would help for other Rexxes (in particular ooRexx)

:: REXX version 1 was coded for classic REXX (not the object-oriented version of REXX, ooRexx), and the use of the '''upper''' statement is more intuitive when being read by people who don't know REXX that well, the '''upper''' bif explains itself.  It also has uses that '''translate''' doesn't have (or doesn't support), but that discussion should be done elsewhere. -- [[User:Gerard Schildberger|Gerard Schildberger]] 22:01, 25 July 2012 (UTC)

--[[User:Walterpachl|Walterpachl]] 09:21, 25 July 2012 (UTC)

==typos and misspellings==

Rather than point out typos and/or misspellings (and hoping that the original author notices the critique and corrects), I believe it is quite acceptable and more than that, expedient to just correct the typo or misspelling as long as it's a comment (in a program) or withing a "talk" page --- if the error is an obvious one.  If there's a doubt, don't change it.  It's harder to tell if there's an error when the wrong word is used (was it intentional?).  If I'd bothered to complain about everybody's bad spelling, typos, or wrong word use, I'd never get any real work done.  The few I did correct, I make sure it's the only thing I did on that update, so other people (especially the original poster) can see what was changed.  And even then, I did the Rosetta Code update with trepidation and consternation. -- [[User:Gerard Schildberger|Gerard Schildberger]] 22:24, 25 July 2012 (UTC)

Changing program code or (input) data is much more probematic.  General rule of thumb: don't.

Some programmers use misspelled words like '''kount''' (instead of count) for variable names intentionaly for whatever reasons.  I also use misspelled words like  Ka-razy  (for crazy) at times in the comment portions; sometimes these attempts at humor may be hard to discern.  There is a fine line between humor and ... not humor. -- [[User:Gerard Schildberger|Gerard Schildberger]] 22:17, 25 July 2012 (UTC)

:Humour is in the eye. (Anyone's eye, not just the beholder). --[[User:Paddy3118|Paddy3118]] 08:52, 26 July 2012 (UTC)

::invited to do so, I corrected one corrected misspelling (occurences -> occurrences)

:::However my observation that 
:::/*C is the hex version of of char*/
:::should read
:::/*Y is the hex version of char   */
:::was not considered and I dare not change it
:::When I did the TSO version I realized that the hex stuff isn't needed at all, is it?  --[[User:Walterpachl|Walterpachl]] 10:18, 26 July 2012 (UTC)

:::: I can't speak for your code, just the code that I entered, and that hex stuff was needed to present a list of letter counts ''as well as'' the character counts.

:::: Also, when changing other people's code or comments, please use a summary stating that (or better yet, whenever you make ''any'' change). This makes it easier for the original poster to see what the changes where via the friendly Rosetta Code notification system.  I know when I first started making changes and entering examples on Rosetta Code, I didn't know about the (edit) summaries.  Now, I have the box checked:

* My preferences
* Editing
* [√] Prompt me when entering a blank edit summary 

:::: Of course, this only nags, er, ''prompts'' you when you don't enter an edit summary, but it helps. -- [[User:Gerard Schildberger|Gerard Schildberger]] 12:20, 26 July 2012 (UTC)
::::: Thanks for the hint. that'll keep me from forgetting. --[[User:Walterpachl|Walterpachl]] 14:03, 26 July 2012 (UTC)

== Whitespace and Assembly code ==

After the Whitespace code, there is a block of Assembly code. Is that part of the Whitespace implementation, or is there a heading missing?
Further, the "Output" section after the Assembly block only contain three numeric values, which doesn't seem to be right. --[[User:PauliKL|PauliKL]] ([[User talk:PauliKL|talk]]) 12:16, 2 August 2013 (UTC)
