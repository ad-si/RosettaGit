+++
title = "Talk:Roman numerals/Encode"
description = ""
date = 2016-07-20T18:41:49Z
aliases = []
[extra]
id = 2790
[taxonomies]
categories = []
tags = []
+++

== enforcing a requirement ==
The Java example does not enforce the requirement that the parameter for the conversion function must be a positive integer. Enforcement of such a requirement should result in an error condition, such as an exception, rather than simply no output when a value less than 1 is passed to the function.--[[User:Waldorf|Waldorf]] 21:14, 31 March 2008 (MDT)
:Fixed. --[[User:Mwn3d|Mwn3d]] 21:22, 31 March 2008 (MDT)

: No, the requirement said to take a positive integer ..., it did NOT say that it HAD to be a positive integer.  My REXX version also allows a non-positive number. I (and I assume others) have removed the error checking (such as negative numbers), non-whole numbers, and the like).  I often remove a lot of error checking from my REXX code that I post as it would just clutter up the example with many, many error checks.



== 400 ==

I'm pretty sure CD is 400. If you look at [[wp:Roman_numerals#Modern_Roman_numerals]] it shows it. --[[User:Mwn3d|Mwn3d]] 21:20, 31 March 2008 (MDT)

== Spec? ==

Roman numerals are by no means standardized. You should reference the particular brand of encoding you wish to support. --[[User:IanOsgood|IanOsgood]] 08:53, 1 April 2008 (MDT)
:What are our choices? --[[User:Mwn3d|Mwn3d]] 09:09, 1 April 2008 (MDT)
:: Project Euler had a similar [http://projecteuler.net/index.php?section=problems&id=89 task], where they addressed possible rules in a [http://projecteuler.net/index.php?section=faq&ref=roman_numerals FAQ]. Wikipedia has also a section on special [http://en.wikipedia.org/wiki/Roman_numerals#XCIX_vs._IC rules]. I guess the safe way is the way all examples so far do it. But maybe specifying it isn't important (I don't know which similarities/dissimilarities between different languages this task should highlight). --[[User:Dirkt|Dirkt]] 15:38, 1 April 2008 (MDT)

-----

Some options are: 

* user of lowercase Roman numerals (letters)
* use of (Roman) fractions [based on twelfths]
* expression of large numbers
* support for   ''zero''   (or ''nothing'')
* Attic style 
* old style
* modern style 

Attic style is a Greek style (also known as acrophonic or Herodianic numerals).  Attic numerals were in use by the Greeks around the 7th century BCE before they converted to their later Greek numerals, but you can clearly see the influence they had on Roman numerals.  

Modern style is where   '''IV'''   is always used instead of   '''IIII''',   but almost all clock-face makers (of clocks and wrist watches that use Roman numerals) today use the old style.

Also where   '''u'''   is used in addition to (or instead of)  '''v''',     and   '''j'''   is used in in place of (any) trailing   '''i'''. 

The manner of expressing large numbers could be considered an option.   The REXX program that I included uses parentheses and ''deep'' parentheses for large numbers.

There are also post-modern (my word, I don't know the real word for this style) where many other (Latin) letters are used for   '''200''',   '''400''',   and other numbers. -- [[User:Gerard Schildberger|Gerard Schildberger]] 07:47, 14 July 2012 (UTC)

== Perl problem ==

The use of the works_with template is interacting badly with the semantic wiki markup, but I'm not sure how to unpick the mess so I'll just flag this up for now. –[[User:Dkf|Donal Fellows]] 08:46, 30 July 2010 (UTC)
: Yeah, I've been wanting to poke the SMW folks in #semantic-mediawiki for thoughts (you can see other weird issues in [[Special:Properties]], but I haven't had time. --[[User:Short Circuit|Michael Mol]] 12:15, 30 July 2010 (UTC)

== Parsing roman numerals ==

Is there a page for that? I couldn't find one...
: I'm not aware of one. It seems a perfectly appropriate task idea. Why don't you create it? (I'd suggest you create an account first) --[[User:Short Circuit|Michael Mol]] 20:03, 9 May 2011 (UTC)
::If you do create it I would suggest putting it at [[Roman numberals/Decode]] or something like that. Also check for task creation hints at [[Rosetta Code:Add a Task]]. --[[User:Mwn3d|Mwn3d]] 20:27, 9 May 2011 (UTC)
::: I've done this (minus the typo): [[Roman numerals/Decode]] --[[User:Mikachu|Mikachu]] 20:48, 9 May 2011 (UTC)
::::Heh...good catch. Anyway it looks good. I'll move this task to Roman numerals/Encode so it's consistent. --[[User:Mwn3d|Mwn3d]] 21:11, 9 May 2011 (UTC)

== Additional Roman numerals: Ⅰ, Ⅴ, Ⅹ, Ⅼ, Ⅽ, Ⅾ, Ⅿ, ⅰ, ⅴ, ⅹ, ⅼ, ⅽ, ⅾ, ⅿ, ↀ, ↁ, ↂ, Ↄ ==

It might be worth permitting full range of Roman numerals eg:
* Ⅰ, Ⅴ, Ⅹ, Ⅼ, Ⅽ, Ⅾ, Ⅿ, ⅰ, ⅴ, ⅹ, ⅼ, ⅽ, ⅾ, ⅿ, ↀ, ↁ, ↂ, Ↄ
c.f. [http://en.wiktionary.org/wiki/ↀ wiktionary]

[[User:NevilleDNZ|NevilleDNZ]] 23:16, 30 November 2011 (UTC)

: Is it just me or can anyone else see those characters?  All I see (under Microsoft Internet Explorer and FireFox Aurora) are "empty squares". <small>''[[User:Gerard Schildberger|Gerard Schildberger]]''</small>
::I see the characters in FF 12. --[[User:Mwn3d|Mwn3d]] 02:57, 26 April 2012 (UTC)
:: It depends on the fonts you've got installed; if they don't have the character glyph, you get that box (on Windows; different platforms have different substitutions). Not much we can do about that. –[[User:Dkf|Donal Fellows]] 10:46, 26 April 2012 (UTC)
:I think they're permitted. Worth it? I dunno. Judging by the "modern Roman numerals" stuff I don't think people really use them anymore. If you want to then go for it. --[[User:Mwn3d|Mwn3d]] 02:57, 26 April 2012 (UTC)

== UNIX Shell problem (fixed) ==
There is an error in how the function for encoding Arabic to Roman numbers is currently written for the UNIX Shell.
In fact, the number "9" is misencoded in "VIV" instead of "IX", as also clear from the published example: 1999 = MCMXCVIV .
This error is easily fixable by inserting the "9" in the values array: change the line
    local values=( 1000 900 500 400 100 90 50 40 10 5 4 1 )
into
    local values=( 1000 900 500 400 100 90 50 40 10 9 5 4 1 )
