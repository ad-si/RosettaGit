+++
title = "Talk:Roman numerals/Decode"
description = ""
date = 2016-04-23T09:42:36Z
aliases = []
[extra]
id = 9630
[taxonomies]
categories = []
tags = []
+++

Hello

My code http://rosettacode.org/wiki/Roman_numerals/Encode#Prolog works in both ways Roman => Arabic and Arabic => Roman. It can be published in this page. [[user:Trap D]] 13/05/2011 18:25

==Roman numeral numbers==

I feel that any legal Roman numeral number (such as   '''IIII'''   should be converted correctly and without error. The Romans started using   '''IV'''   (and others)   after they realized the practicability of shortening their numbers, especially those having   '''8'''s   in them;   easily justified when chiseling those numbers in stone or scribing them in wet clay.

Also, numbers such as   '''IIXX'''   should also be converted correctly, as they do appear on old structures and tombstones.   Even though modern rules say such a construct may be invalid, the number still has an equivalent decimal number.   -- [[User:Gerard Schildberger|Gerard Schildberger]] 03:53, 25 April 2012 (UTC)

Furthermore, the Latin word for   '''eighteen'''   is   '''duodeviginti'''   which literally means   '''two-from-twenty''',   or in Roman numbers;   '''IIXX'''.   -- [[User:Gerard Schildberger|Gerard Schildberger]] 21:34, 9 July 2012 (UTC)

==asking for help==

Does anyone know how to fix a problem I'm having with the overbar template which, for some reason, decides to italicize the text after the overbarred characters  "for twenty-thousand"? 

The offending code is in the REXX program, version 4. People should never give a loaded gun to kids. Pardon me if this isn't the right place to ask this type of question. I couldn't find any other usage of overbars in Rossetta Code. 

I can "fix" the overbar/italicizing problem by putting the "for twenty-thousand" on a new line (via the html "br").

Also, I would like to express how to display an HTML tag without HTML "using" it (as in the previous line).  I assume that there's some type of escape character(s). -- [[User:Gerard Schildberger|Gerard Schildberger]] 03:38, 25 April 2012 (UTC)

It was a stray newline in the [[Template:Overline]] which I just now fixed. If you want to talk about HTML then you can escape it in the usual fashion, &amp;amp;br>, or you can use <nowiki><no</nowiki><nowiki>wiki>
</no</nowiki>wiki>. HTML inside of &lt;lang> blocks is also not interpreted. —[[User:Kevin Reid|Kevin Reid]] 19:37, 25 April 2012 (UTC)

: Thanks for the fix and the hints (albeit a wee bit late for the gratitude). -- [[User:Gerard Schildberger|Gerard Schildberger]] 07:23, 11 July 2012 (UTC) 

==Roman numeral, returning an integer==

This task asks to take a Roman numeral (as its argument)   and return a numeric decimal integer.   This assumes that the Roman numeral is an integer.  

What if the Roman numeral contains (or is) a fraction?  

I presume then, that Roman numerals to be checked won't have fractions.    The Romans had a base '''12''' fractional system.    One-twelfth (fraction)   is an   ''ounce''   which we still use in (for weight)   pounds and ounces, where the troy pound contains 12 ounces.   An '''ounce''' is also used as a unit of time.   -- [[User:Gerard Schildberger|Gerard Schildberger]] 07:23, 11 July 2012 (UTC)

:We aren't doing fractions here. In general "Roman numerals" refers to the the integers. --[[User:Mwn3d|Mwn3d]] 19:50, 18 July 2012 (UTC)
