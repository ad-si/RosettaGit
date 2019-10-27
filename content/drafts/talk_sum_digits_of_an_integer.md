+++
title = "Talk:Sum digits of an integer"
description = ""
date = 2015-01-30T14:16:19Z
aliases = []
[extra]
id = 12055
[taxonomies]
categories = []
tags = []
+++

== Output base ==

It looks like the base of the output is supposed to be 10. Does that matter at all? --[[User:Mwn3d|Mwn3d]] 12:59, 19 July 2012 (UTC)
:I agree, for me reading the output as 29 or 0x1d would be the same. For the input the BASE parameter defines how the function interprets the number. Within the computer 0xfe or 254 would both have the same binary representation. I hope that SumDigits(0xfe, 10) would return 11 and that SumDigits(254, 16) would return 29.--[[User:Nigel Galloway|Nigel Galloway]] 11:43, 20 July 2012 (UTC)

::I don't understand. "SumDigits(0xfe, 10) would return 11 and that SumDigits(254, 16) would return 29." if sumDigits(0xfe, 16) -> 29<sub>10</sub>, then how would sumDigits(256, 16) -> 29<sub>10</sub>? Shouldn't it return 13<sub>10</sub>? --[[User:La Longue Carabine|La Longue Carabine]] 16:19, 23 July 2012 (UTC)

:::254 in base 16 has the digits with decimal values 15 and 14.  Their sum when expressed in decimal notation is 29.  In base 16, using hexadecimal digits, this sum might be represented as 1d = f + e.   But we should get a different value for the sum of the digits of 256 -- in base 16, the digits have decimal values 1, 0 and 0, so their sum should be 1.  That said, the base used to express the result need not have anything to do with the base used to perform the calculation.  --[[User:Rdm|Rdm]] 16:54, 23 July 2012 (UTC)

:::: I've rewritten “0xfe” to “<code>fe</code><sub><code>16</code></sub>” in the task description; the issue with it before was that “0xfe” is just an encoding of the number (in a way that happens to be a numeric constant in many languages) and not the number itself. In particular, “x” isn't a hex digit. –[[User:Dkf|Donal Fellows]] 08:55, 31 July 2012 (UTC)


== Input base ==

Does the input base matter at all?  No matter what the base is, the digit 1 has value 1, 2 means 2, ... a means 10, ... z means 35.   Unless we're expected to throw an error if the input contains a digit that is not valid for the given base, I don't see the point of specifying one? --[[User:Markjreed|Markjreed]] ([[User talk:Markjreed|talk]]) 02:10, 30 January 2015 (UTC)
:10 in binary does not equal 10 in hexadecimal, so unless the input is a single digit, yes, it does need to be specified. -- [[User:Eriksiers|Erik Siers]] ([[User talk:Eriksiers|talk]]) 03:38, 30 January 2015 (UTC)

::But the sum of the '''binary''' ''digits' 10 expressed as a decimal is the sum of ''digits'' 10 in ''any'' valid base when expressed as a decimal, i.e. a one plus a zero equals one! --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 06:59, 30 January 2015 (UTC)
:The input BASE parameter defines how the function interprets the number. Within the computer 0xfe or 254 would both have the same binary representation. I hope that SumDigits(0xfe, 10) would return 11 and that SumDigits(254, 16) would return 29. In the example above in base 35 z would sum to z (35 in base 10), z in base 10 (35) would sum to 8.--[[User:Nigel Galloway|Nigel Galloway]] ([[User talk:Nigel Galloway|talk]]) 14:16, 30 January 2015 (UTC)
