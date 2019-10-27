+++
title = "Talk:A+B"
description = ""
date = 2017-01-30T00:08:08Z
aliases = []
[extra]
id = 6813
[taxonomies]
categories = []
tags = []
+++

==task elegance==
This task is pretty elegant; I suspect it could handily replace both [[User Input]] and [[User Output]]. --[[User:Short Circuit|Michael Mol]] 16:49, 8 April 2010 (UTC)


==Formatting==
This tasks doesn't follow the normal task formatting conventions, where the Task description does not contain explicit headings so that all the task description appears above the contents; and all languages appear with equal "double-equals" heading.

I could reformat it if you like?

P.S. Fabula?

--[[User:Paddy3118|Paddy3118]] 18:48, 8 April 2010 (UTC)

: I've done ''some'' reordering/reformatting. The header/problem material still needs work, but the solutions should now be relatively OK. –[[User:Dkf|Donal Fellows]] 19:56, 8 April 2010 (UTC)

:: Ta! --[[User:Paddy3118|Paddy3118]] 21:46, 8 April 2010 (UTC)



### Input Specification

It is not quite clear whether the input is solely two integers separated by a single space, whether there's a new line or not, whether there can be more stuff in the input, whether there may be leading spaces and things like that. Some solutions displayed are resilient to such variations, others will fail. Some even seem to depend input being separated by a newline for each number.

In other words, it would be nice if the input could be more strictly specified. -[[User:dcsobral/Daniel Sobral]] 21:27, 8 April 2010 (UTC)

: If you look at the solutions contributed in the first version of the page, you'll see code that only makes any sense if the input is two integers on a single line separated by whitespace. There might or might not be leading and trailing whitespace on that line. Or at least that's what the Pascal and C solutions wanted at that point. –[[User:Dkf|Donal Fellows]] 23:27, 8 April 2010 (UTC)

:: Task specification says that the input contains two numbers separated by space. However, many implementations seem to prompt the values one by one. For example Delphi, DMS, DWScript and Elena, just to name few. I think these implementations are incorrect. However, the task specification does not say if the input may contain leading and trailing spaces. --[[User:PauliKL|PauliKL]] 16:02, 25 September 2011 (UTC)
:::I don't think we really need to worry about that. That's not really the point of the task. I say let it go ad let people input the numbers in whatever way is natural to the language. --[[User:Mwn3d|Mwn3d]] 16:21, 25 September 2011 (UTC)
:::: I don't know: what is the point of the task, actually? --[[User:Ledrug|Ledrug]] 16:45, 25 September 2011 (UTC)

::If it is competition input then those tasks are likely fails if they ''require'' a newline to be typed after the first number, before the second can be recognised, as I would have thought that, as the competition input states, only a space is given. 
::What ''is'' wrong, is the task doesn't state how the input is terminated! --[[User:Paddy3118|Paddy3118]] 03:21, 26 September 2011 (UTC)

::: New line is not really required after two numbers, a Ctrl-D signaling end of input could do, too (^D itself is not read as part of input in any way).  This task asks two input numbers in the same line, then adding them; both are pretty trival, but the latter is more so.  I would have guessed the input part is more the point of the task. --[[User:Ledrug|Ledrug]] 03:53, 26 September 2011 (UTC)

:: It's not clear to me what the interpretation of "input stream" would be for a system that doesn't have streams or I/O redirection.  Specifically, any computer running without an operating system, such as most microcontrollers.  By the way, do I have to edit in the user info, date, and so on as I have done, or is there a better way to add a comment than just clicking "edit"? --[[User:Gatmo|Gatmo]] 03:13 21 January 2014 (PDT)


===Input specification ("constraints")===
The task's text says that A and B are (must be? should be?) between -1000 and 1000; no a single example checks this. Is it a mandatory requirement we should add the check for, or it can be ignored? &mdash;[[User:ShinTakezou|ShinTakezou]] 06:30, 13 May 2010 (UTC)

: Usually in such tasks (at least for the ACM ICPC) all input follows the initial specification so no checking is required. Usually the boundaries provided by the task description provide insight into how the problem can be solved; for example, if the problem size is very small it can well be an NP-complete problem you need to brute-force, for medium-sized problems an ''n''<sup>2</sup> algorithm might still be possible. If input numbers easily exceed 10<sup>9</sup> or so you shouldn't iterate through them and find a more clever solution. After all, your program has to solve a task in a certain time (usually between 1 and 5 seconds).
: So, in short, given where the problem comes from I consider checking the input superfluous. There are many more things not checked as well. The Ruby example will dutifully add as many whitespace-separated numbers as there are in the input; not just two. —[[User:Hypftier|Johannes Rössel]] 10:22, 13 May 2010 (UTC)
:: as does one of my Batch File solutions --[[User:Axtens|Axtens]] 14:37, 13 May 2010 (UTC)

: The task specifies that we need the sum of A and B and that they are between -1000 and 1000.  It does NOT specify that we should not provide the sum when they are out of this range.  I believe the intent was that outside of this range the behavior of the program is unspecified.  --[[User:Rdm|Rdm]] 10:30, 13 May 2010 (UTC)


==Fabula?==
What is the meaning of [http://oxforddictionaries.com/noresults?dictionaryVersion=region-uk&isWritersAndEditors=true&noresults=true&page=1&pageSize=20&q=Fabula+&searchUri=All&sort=alpha&type=dictionarysearch fabula]? --[[User:Paddy3118|Paddy3118]] 03:09, 26 September 2011 (UTC)
:I see I asked this a year ago without reply. Time to delete? --[[User:Paddy3118|Paddy3118]] 03:11, 26 September 2011 (UTC)
::Google hit #1 for me: [[wp:Fabula and syuzhet]]. Looks like it's just a fancy word. No real reason to keep it or delete it besides personal preference. If you want to change it or delete it I don't think we'll lose anything. --[[User:Mwn3d|Mwn3d]] 03:51, 26 September 2011 (UTC)
:::[http://en.wiktionary.org/wiki/fabula ''Fabula''] is a Latin word for "story". Wikipedia's article "fabula and syuzhet" is not relevant. --[[User:Kernigh|Kernigh]] 17:47, 26 September 2011 (UTC)
::::Thanks Kernigh for the reference. I would have thought that the Oxford English Dictionary would have an entry for Latin words in 'common' usage, so I don't feel the need to put back the mention of it in the task. --[[User:Paddy3118|Paddy3118]] 18:09, 26 September 2011 (UTC)


== LabVIEW ==
While LabVIEW can add numbers, I'm pretty sure that it cannot read stdin (or if it can, it's well beyond my knowledge). Should I add LabVIEW code, mark it as <nowiki>{{omit from|LabVIEW|No Stdin}}</nowiki>, or just leave it for someone else? --[[User:Crazyfirex|Crazyfirex]] 03:20, 16 January 2012 (UTC)


==REXX==
Siskus:   The REXX version 4 you flagged as ''incorrect'' is indeed correct, and it works as described and has been tested.

It accepted two numbers and summed them.   The fact that it ''can'' accept more then two numbers is beneficial.  

Please refain from flagging correct programs without checking them for accuracy or task compliance. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 16:05, 1 November 2013 (UTC)
