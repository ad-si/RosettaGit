+++
title = "Talk:Generate lower case ASCII alphabet"
description = ""
date = 2016-04-08T06:21:04Z
aliases = []
[extra]
id = 18314
[taxonomies]
categories = []
tags = []
+++

== Description needs work ==

(I know, it's out of draft and has a bunch of solutions already.)

It starts okay, but then I don't know what "For this basic task use a reliable style of coding" means.  Is it a dig at some style used in some other tasks?  I don't think it belongs.  "a style fit for a very large program"  Why?  This is not a large program.  Anyway, what does this mean?  Should I add a program version number?  Use map/reduce?  Outsource tech support?  "use strong typing if available"  Few languages allow you to pick strong or weak.  This extra requirement for a very few languages seems strange.

"It's bug prone to enumerate all the lowercase chars manually in the code."  I disagree with this pretty strongly.  If the intent is to disallow such things as strings and literal lists of the 26 letters, that's fine, but it could be a little more clear.  "During code review it's not immediate to spot the bug in a Tcl line like this contained in a page of code:"  You don't even have to be able to program or know what ASCII is to spot that error, just know your (English) alphabet.  An off by one error or a < instead of <= on the other hand can elude the best of us at times.  I would drop the editorializing and just clarify  that literals of the full alphabet are disallowed.  &mdash;[[User:Sonia|Sonia]] ([[User talk:Sonia|talk]]) 17:18, 26 November 2014 (UTC)

== ASCII alphabet vs. EBCDIC vs. Latin (English) alphabet ==
I assume you meant a lowercase Latin (or English) alphabet.   Other than the internal coding of the character (or rather, the lowercase letters),   the glyphs (depending on the code page) aren't any different then an   '''EBCDIC'''   version.   Generating the same lowercase letter sequence in '''EBCDIC''' doesn't   ''define''   it as a lowercase '''EBCDIC''' sequence. 

After saying that, a Latin (or English) alphabet may however, display differently, depending upon what code page is being used.   -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 23:04, 7 April 2016 (UTC)

: I don't think this task has anything to do with the visual appearance of the letters. If you are on a system which only supports EBCDIC for IO then I think for this task you would need an internal representation of ASCII as indices into an array of EBCDIC characters. --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 03:49, 8 April 2016 (UTC)

:: Er, no, I don't.   It's not the visual appearance of the   ''letter'',   it's what a particular 8-bit character is presented (shown) as.   The hexadecimal value of   '''<big>4A</big>'''   is shown as ASCII   '''<big>J</big>''',   or as EBCDIC   '''<big>¢</big>'''.   So in that narrow sense, the glyph shown is important.   That was one reason why I created the Rosetta Code task of   ''Idiomatically determine all the lowercase and uppercase letters''.   The point is that the program doesn't have to know what the ASCII code is for a (particular) lowercase letter.   That's the idiomatic part of the task.   The same point can be made for this task.   It's this point in mind that I thought the author was making when he said to   ''use a reliable style of coding''.   Keep in mind, the REXX program that I entered works on both ASCII and EBCDIC (and makes no assumption about the whether or not the lowercase letters are contiguous), and there isn't a need to translate anything, no matter which system is being run on.   -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 04:41, 8 April 2016 (UTC)

::: Isn't [[Idiomatically_determine_all_the_lowercase_and_uppercase_letters]] a different task from this one? --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 06:02, 8 April 2016 (UTC)  

:::: Yes, it is.   That task stress the   ''idiomatically''   part of the task   (instead of just listing all the characters from   '''a''' ──► '''z'''   (inclusive).   The other task also asks for the uppercase versions of the letters.   It should be noted, however, that the other task is still a draft task at this time.   -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 06:18, 8 April 2016 (UTC)

As to the     ''use a reliable style of coding''     phrase, I interpreted that to mean to code the program in such a way to not assume that the lowercase letters may not be contiguous on other systems, and that the programer should test if a particular character is indeed, a lowercase character.   -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 23:04, 7 April 2016 (UTC)
