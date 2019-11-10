+++
title = "Talk:Hello world/Text"
description = ""
date = 2016-05-20T23:51:36Z
aliases = []
[extra]
id = 3123
[taxonomies]
categories = []
tags = []
+++

==incomplete programs==
Some examples lack some parts of the code. e.g. the Java example is not a complete program.
: It's not uncommon for RC tasks to exclude code that represents the program surrounding the code.  This helps make it obvious what code is specific to the task at hand.  If you need to see what the surrounding code looks like, see [[Empty Program]]. --[[User:Short Circuit|Short Circuit]] 02:31, 21 November 2008 (UTC)

:: Sometimes, it's necessary to show the complete program.  For instance, say a task is to set a variable to '''2.5''' --- showing the statement '''J = 2.5''' by itself in most languages would probably be wrong as (most?) languages would like/need/require some sort of declaration of the ''type'' of variable it is (and/or other attribute thingys), and that's the sticky wicket part.  Just showing the assignment of the variable would do a disservice. I like to see what various languages need for a prologue.-- [[User:Gerard Schildberger|Gerard Schildberger]] 18:14, 11 August 2012 (UTC)

== Hello World ==

I don't care to be cliché (Hence why I originally wrote the text as "Goodbye World"), but in light of [http://rosettacode.org/mw/index.php?title=Rosetta_Code:Village_Pump/Request_a_programming_task&diff=72147&oldid=prev confusion like this] (and I've seen others mention it elsewhere), perhaps the task should be renamed to [[Hello World - text]], and the relevant output strings adjusted? Similar would need to be said for the GUI task. Any objections? --[[User:Short Circuit|Michael Mol]] 03:39, 20 January 2010 (UTC)

: As for the name of the task, most people would know what a "Hello World!" program is, no matter what the actual output was shown, it's supposed to show how to program a ''very'' simple display of text, with a minimum of program/language preamble. -- [[User:Gerard Schildberger|Gerard Schildberger]] 18:28, 11 August 2012 (UTC)

: However, the relevant output text string should be strictly adhered to as that's the task requirements.  As in a real world example, the name of the game ... er, task, is to do what the requirement(s) specifiy, not what the name of the task is.  If the task is to display a specific mixed uppercase/lowercase text with punctuation (and in this case, in direct contradiction to the task's name), then that's what's to be displayed/shown.  We all know to shutdown a Microsoft Windows system, you have to click on '''Start''' first. --- And having wallpaper on a desktop, ... there're way too many examples of oxymoronic procedures/methods/names/requirements/etc.  Many of the Rosetta Code program examples got this very simple task wrong, either by the wrong words, case, and/or punctuation.  A few hit the trifecta. -- [[User:Gerard Schildberger|Gerard Schildberger]] 18:28, 11 August 2012 (UTC)

: Should tasks be marked as '''incorrect''' if they don't meet the task's requirements?  Simplistic as the task is, more than a handful of the examples managed to get it wrong.  Or, should somebody just correct them?  It's easy enough to do, as most of corrections would just be changing a literal. -- [[User:Gerard Schildberger|Gerard Schildberger]] 21:52, 23 October 2012 (UTC)

:: Both sound fine.  But I would avoid "fixing" implementations for languages you are not familiar with (since some languages implement special characters in strings and assuming a special character would be treated literally would be bad -- granted, the chances of any of the characters here being special is low, but there's no point fixing something if you cannot know if your fix is correct).  --[[User:Rdm|Rdm]] 19:31, 24 October 2012 (UTC)

::: Understood.  The point of verifying if an example will pass the "what-is-a-legal/special-character" was foremost on my mind. I know a few languages treat an exclamation point ['''!'''] as special.  I hope flagging the various program examples won't be construed as picayune;  the task is almost a definition of simplicity itself.   -- [[User:Gerard Schildberger|Gerard Schildberger]] 19:21, 1 November 2012 (UTC)

:::: Yes, it's difficult to not be silly when the task itself is somewhat silly.  That said, note that some of your "incorrect" markers might not be completely accurate.  Perhaps we should ask for example output?  For example, I believe that $ is by convention a string terminator in 8086 assembly (like ascii nul character in C)What .

::::: Yes, I would like to see the output of the '''Chef''' example, for one.  I would also like to know if any languages '''don't''' support lowercase, or for that matter, "special" characters like the exclamation point ['''!''']. -- [[User:Gerard Schildberger|Gerard Schildberger]] 21:03, 1 November 2012 (UTC)

::::: I don't feel that his task is silly, brief and simplistic as it is.  In any case, if anyone has questions about any possible incorrect flags that I did, please raise them here.  I based my flagging ('''incorrect output''') if the first word is '''Hello''' instead of '''Goodbye''', and/or no comma (followed by a space), '''world''' instead of '''World''', and/or no trailing exclamation point ['''!'''].  Also, the case of the words must be (only) the first letter capitalized.  In other words, it must be ''exactly'':  '''Goodbye, World!''' ------- Whatever the error(s), they'll be simple to fix. -- [[User:Gerard Schildberger|Gerard Schildberger]] 21:00, 1 November 2012 (UTC)

::::: What if they're not simple to fix, or even possible?  I don't think a language should be excluded from this page - the canonical simplest task ever for any programming language - just because they don't have the ability to output the exact string chosen by the creator of the task.  This should be the one task on RosettaCode that has an entry for almost every language on the site; only those that completely lack text output should be excluded.  But Applesoft BASIC doesn't have lowercase, HQ9+ can output "Hello world" but not "Goodbye, World!" ... [[User:Markjreed|Markjreed]] 05:26, 30 January 2013 (UTC)

:::::: I was re-reading several definitions of computer languages (also, programming languages), and it appears that HQ9+ doesn't meet the requirements, but I certainly won't be the one to exclude it.   To me, it's more of an ''application'' than a language:  enter ''xxx'' parameter, and out pops a specific string(s). -- [[User:Gerard Schildberger|Gerard Schildberger]] 21:49, 16 March 2013 (UTC)

::::: It is probably just easiest to change the text description to read "a greeting such as Goodbye World!", rather than "the string Goodbye World!". This will not have any impact on the existing provided solutions, and solves the problem of having incorrect flags. [[User:Markhobley|Markhobley]] 13:49, 24 February 2013 (UTC)

:::::: Even discussing the (text) greeting is misquoted. -- [[User:Gerard Schildberger|Gerard Schildberger]] 21:49, 16 March 2013 (UTC)

:::::: The easiest thing would be to remove all incorrect tags (everywhere).   That way, no changes (corrections) would have to be made.   Incorrect programs?   Just change the task requirements.   Can't produce a list of primes?   Just use Fibonacci numbers instead.   But seriously, if a language can't meet the requirements, I see no reason why it can't just say that (and why), and use a best-effort example (with notation).   If examples start using any (salutation) string they wish, it doesn't make for good comparisons.   How would most people know that lowercase letters aren't allowed in a language, or that some specific punctuation characters are not supported?   Using non-Latin characters would also be problematic.  Having a common set of punctuation helps also. -- [[User:Gerard Schildberger|Gerard Schildberger]] 21:49, 16 March 2013 (UTC)

: I just noticed that in [http://www.rosettacode.org/mw/index.php?title=Hello_world%2FText&type=revision&diff=211993&oldid=211728 September of last year] someone took it upon themselves to make "Hello world!" the required output string for this task, and attempted to update all of the existing solutions to match. However, since this was mostly done as a find-and-replace, there are a number of languages that have not been updated (Befunge, Brainfuck, Chef, etc.), and a number of others are now broken because the updated text uses mixed case which their language doesn't support (e.g. ALGOL 60 and Applesoft BASIC). There are also several places where comments, classes or variable names still refer to the original "Goodbye World" text, and now no longer make any sense.

: Since this occurred some time ago, and there have been several new additions to the page since the task description changed, simply reverting the original examples to what they were before is not really a solution. Either way you still end up with a page that is a messed up mix of both "Hello world" and "Goodbye World". And updating the new entries doesn't look particularly easy either, so that's not something I would want to attempt myself. For now I'm just noting the problem, but leaving things as they are. Hopefully someone more senior than me can suggest what should be done to fix this (if anything). --[[User:J4 james|j4_james]] ([[User talk:J4 james|talk]]) 22:55, 20 May 2016 (UTC)

== Audible Hello World? ==

I'd like to see a "Hello World/Audible" or "Hello World/Spoken" task here. The only task representing text-to-speech (which is common in accessibility aids and a large field) is "Using a speech engne to highlight words" - a draft task with one implementation (mine). That task would benefit from having a simpler text-to-speech task. I think it would be nice to have another Hello World as well. I know of 4 languages off the top of my head which I can write examples in. Thoughts? --[[User:Crazyfirex|Crazyfirex]] 03:06, 18 September 2011 (UTC)

: There is an audible version: http://rosettacode.org/wiki/Speech_synthesis [[User:Markhobley|Markhobley]] 10:03, 18 September 2011 (UTC)

== Chef ==

not really a programming language.
Ain't 111 cups oil a bit much?
and mustard and oil are added twice?
--[[User:Walterpachl|Walterpachl]] 11:41, 26 January 2013 (UTC)


Oops - indeed a "programming laguage" surprise surprise --[[User:Walterpachl|Walterpachl]] 12:45, 26 January 2013 (UTC)

==Is there a page with the REAL Hello, World! programs?==

why?
