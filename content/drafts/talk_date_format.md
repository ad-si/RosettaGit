+++
title = "Talk:Date format"
description = ""
date = 2018-07-06T17:59:21Z
aliases = []
[extra]
id = 2211
[taxonomies]
categories = []
tags = []
+++

==Input==
I'm not exactly sure what is supposed to go on here. How is the date given? How would you like it formatted? I can give you POSIX time pretty easily in most languages...but I don't think you want that. --[[User:mwn3d|mwn3d]] 23:54, 10 November, 2007 (EST)
: The Forth and Perl examples, as written, print the current date and time.  Seems like a good choice. --[[User:Short Circuit|Short Circuit]] 15:02, 10 February 2008 (MST)
:: The page has been tagged as "This task has been clarified". But it has not been clarified. It still does not say how the date is given. I agree that current date would be a good choice, that should be added to the task description. --[[User:PauliKL|PauliKL]] 09:39, 3 October 2008 (UTC)
::: If any output format is to be chosen then that should be one of ISO 8601, I suppose. --[[User:Dmitry-kazakov|Dmitry-kazakov]] 12:54, 3 October 2008 (UTC)
::::ISO sounds like a great idea. If we want to make things the same across all languages, why not use a standard, right? Also, I think the current date should be used. It's the easiest to specify. --[[User:Mwn3d|Mwn3d]] 14:19, 3 October 2008 (UTC)

==Leading zeros==
While I'm sure that there should be no leading zeros in the format where words occur, I suspect that the intention is for Febrary 1st, 2008 to be formatted "2008-02-01" not "2008-2-1".  Does the specification need to be altered to reflect this? --[[User:TBH|TBH]] 22:17, 8 February 2008 (MST)
:ISO 8601 requires leading zeros, but the task is still not clarified. Personally I think that it mangles two separate issues:
:* splitting a time stamp into fields, like month number, day of week etc in presence of a given time zone
:* formatted output of the fields
:Does it really matter if the output should be 01:02:2008, 2008/01/02, 1/II/2008 etc? --[[User:Dmitry-kazakov|Dmitry-kazakov]] 12:54, 3 October 2008 (UTC)
::Of course the format of date matters. What use would there be for a date if people do not understand what it means? (For example, which number is the month and which one is the day.) This is actually a real problem in the real world. If it is not possible to use the local date format, at least you should format the date correctly so that people will know which format you are using. There are three commonly used formats:
::*American: mm/dd/yyyy
::*European: dd.mm.yyyy
::*ISO: yyyy-mm-dd

::::::::: There is (maybe not-so-common) format (I think SAS uses it a lot):   '''mmMonthyyyy'''   (no leading zeros)   as in (today):   '''5Jul2018'''   (using the 3-char English month name).   I don't know if this particular format has a name.   --[[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 03:03, 6 July 2018 (UTC)

:::::::::: I have to say, that is the ''weirdest'' format I ever heard of. I would have expected it to have a day in there somewhere instead of a month twice. ;) BTW, if you aren't specifying leading zeros for days or months you typically use one place-holder; so July 5, 2018 in the dmmmyyyy format would be 5Jul2018 --[[User:Thundergnat|Thundergnat]] ([[User talk:Thundergnat|talk]]) 11:52, 6 July 2018 (UTC) 

::::::::::: Thanks for catching that error.    (I had to re-read what I wrote before I saw my typo).   I meant to say:   '''dMmmyyyy'''   where   '''Mmm'''   is the capitalized three letter month name (in English).   I suppose the   '''dd'''   could be used, but a leading zero for the day-of-month would be superfluous, but it would make all dates more consistant when numerous dates are displayed.   Also note that not all languages have a unique three-letter abbreviation for the name of the month.   I suppose that not being English centric has its drawbacks.   -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 17:55, 6 July 2018 (UTC) 

::Note that each format uses different separator. You should always use correct separator for each format so that people will know which format is used.
::Here in Rosetta Code, the main thing is that all the implementations implement the same task. Therefore all the implementations should use the same format.
::--[[User:PauliKL|PauliKL]] 12:00, 5 October 2008 (UTC)
::: Separators does not identify the format, unless it is a standard requirement (like the case of ISO). I commonly use /, both in writing the date "european"-way and year/month/day (can't call this the ISO-way if ISO requires -) --[[User:ShinTakezou|ShinTakezou]] 18:11, 13 May 2009 (UTC)
:::: Separators do not reliably identify the format ''just because some people do it wrong!'' That is why it is important to tell people to do it right. --[[User:PauliKL|PauliKL]] 15:05, 15 May 2009 (UTC)
:::::: "American" and "European" do not identify a standard but an "habit", so there can't exist people doing wrong or right: it's just common use; and standards matter only when they matter, i.e. in the context requires them. I won't comply to any standard or "common habit" when writing, unless it's an official document requiring compliance to a specific format. --[[User:ShinTakezou|ShinTakezou]] 17:20, 16 May 2009 (UTC)
::::: "Be strict in what you generate, liberal in what you accept." (From unknown) --[[User:Short Circuit|Short Circuit]] 22:17, 15 May 2009 (UTC)
:::::: [http://en.wikipedia.org/wiki/Robustness_Principle Postel's Law]. The further discussion there is worth reading. --[[User:Kevin Reid|Kevin Reid]] 19:00, 16 May 2009 (UTC)
