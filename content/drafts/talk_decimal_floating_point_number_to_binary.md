+++
title = "Talk:Decimal floating point number to binary"
description = ""
date = 2019-07-26T14:46:04Z
aliases = []
[extra]
id = 17167
[taxonomies]
categories = []
tags = []
+++

==What is a "decimal floating point number"?==
I don't understand the task.  Most floating-point numbers are in no way "decimal", but rather stored with a binary mantissa and exponent.  The example given, 23.34375, is a decimal number, but it is not necessarily a floating-point number, despite the fact that many languages would store it as floating-point (such as C or Perl 5).  Other languages will store it as a rational representation with explicit numerator and denominator (Perl 6 does this, unless you explicitly use E notation).  Still other languages might choose to store it in a fixed-point representation with an implied denominator.  And some languages will simply keep it as an ASCII string (vintage Tcl) or as a BCD string (REXX I think) and do arithmetic on the string.  So I suspect this task should either drop the words "floating point" or should make it clear what is supposed to be "floating" here.  Thanks.  --[[User:TimToady|TimToady]] ([[User talk:TimToady|talk]]) 20:19, 4 February 2014 (UTC)

: REXX stores all values (including the aforementioned numbers) as a (character) '''string''', not a BCD. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 22:07, 4 February 2014 (UTC)
: I would hope that   ''decimal floating point number''   wouldn't be re-defined as a decimal floating point number being stored in binary. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 22:07, 4 February 2014 (UTC)

:: Technically, that decimal string is just the serialisation in Tcl. It's also a good-old IEEE binary double. People are encouraged to think in terms of the serialised forms, sure, but it's terribly slow to do everything that way. (We know because Tcl used to work that way, long ago.)
:: I ''believe'' the task is asking for a printed form that is in base two, i.e., that uses only the digits <tt>0</tt> and <tt>1</tt> (and appropriate non-digit characters). Thus, <tt>0.75</tt><sub>dec</sub> is the same as <tt>0.11</tt><sub>bin</sub>. ?[[User:Dkf|Donal Fellows]] ([[User talk:Dkf|talk]]) 00:35, 5 February 2014 (UTC)

::: To me this task is not correctly named. I understand "floating point number" defined by IEEE and what, e.g., PL/I produces when one codes
::: 
```pli
Dcl f Dec Fixed(15); f=1/3; Put skip list(f,unspecx(f)); 
```

::: 
```txt
3.33333333333333E+0000 ABAAAAAAAAAA0A40
```

::: The subject of this task are decimal and binary numbers with fractional digits. --[[User:Walterpachl|Walterpachl]] ([[User talk:Walterpachl|talk]]) 10:45, 8 February 2014 (UTC)

:::: I agree --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 13:46, 8 February 2014 (UTC)
:::: I would suggest a subject "Converting fractions among bases" or some such, and then we could add other radices to the task.  --[[User:TimToady|TimToady]] ([[User talk:TimToady|talk]]) 17:21, 8 February 2014 (UTC)

::::: I also agree (converting to other bases besides binary).   It's not that difficult to convert to any displayable base once one gets the methodology right.   Indeed, the (original) REXX version 1 solution could convert to ''any'' base. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 21:14, 8 February 2014 (UTC)

==range of numbers==
No mention was made for the range of numbers (in digits or as characters).

I.E.:   how large can/should they be? -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 22:07, 4 February 2014 (UTC)

Also, I would assume that negative numbers would be allowed. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 22:07, 4 February 2014 (UTC)

== input ==

If every one use the same input like pi the it would be easier to compare/check. Is it OK ?--[[User:Adam majewski|Adam majewski]] ([[User talk:Adam majewski|talk]]) 20:39, 25 July 2019 (UTC)
:Is there something wrong with the examples given (23.34375 => 10111.01011, 1011.11101 => 11.90625)?  Note that PI is of course always going to be an approximation and therefore likely to be inconsistent even between 32/64 bit versions of the same language. One thing I spotted worth checking is that it does not go into an infinite loop for eg 1/3. --[[User:Petelomax|Pete Lomax]] ([[User talk:Petelomax|talk]]) 13:54, 26 July 2019 (UTC)
:: the examples are ok, but maybe a set of examples, the same for all languages, would be better when one wants to compare results. --[[User:Adam majewski|Adam majewski]] ([[User talk:Adam majewski|talk]]) 14:46, 26 July 2019 (UTC)
