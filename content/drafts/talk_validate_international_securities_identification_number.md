+++
title = "Talk:Validate International Securities Identification Number"
description = ""
date = 2016-08-07T13:32:44Z
aliases = []
[extra]
id = 18797
[taxonomies]
categories = []
tags = []
+++

== Dup? ==

Is this a duplicate of [[Luhn_test_of_credit_card_numbers]]?

If not, what is the task? --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 07:22, 26 February 2015 (UTC)

Thanks Rdm. I should have spotted the connection with Luhn. The difference is that ISINs can contain alphabetic characters, which must be translated to digits before a Luhn checksum is calculated. CUSIP, the North American stock codes, are shorter but otherwise use the same algorithm as ISINs. I couldn't see a Rosetta page for them.

There is another page on Rosetta for [[SEDOLs]], which use the same letter-to-number technique, but thereafter use a different checksum algorithm, not Luhn. 

So I think there is a point in having an ISIN page. What do you think?

--[[User:TheWombat|TheWombat]] ([[User talk:TheWombat|talk]]) 00:05, 27 February 2015 (UTC)

: Any of these sound like they would work. Another possibility might be to mark pages with something like Category: Checksums (very broad) or Category: Digit Checksums, or something like that.

: That said, note that in my previous reading I did not pick up that letters were being included in the checksum, using a "base 36" sort of mechanism.

: Anyways, I do not have any real strong opinions on this topic. I guess just proceed how you like and see if anyone else weighs in? --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 09:29, 27 February 2015 (UTC)
== Adding an example ==
I suggest adding an ISIN ending in 0, as it may show a bug in the implementation. For instance, I had the (bad) idea to replace x==(-y)%10 with 10-x==y%10 in the Python implementation (where x and y are expressions in the Python code, and x is the last digit). This transformation is of course wrong, but it fails only when x=0. I have found several valid ISIN ending in 0, here is one: FR0000988040 (from HSBC I think).

[[User:Arbautjc|Arbautjc]] ([[User talk:Arbautjc|talk]]) 10:55, 7 August 2016 (UTC)

: I just did a complete rewrite of the task description anyway, and used this opportunity to add your suggested test-case. --[[User:Smls|Smls]] ([[User talk:Smls|talk]]) 12:30, 7 August 2016 (UTC)
::Thank you! I'll edit the Python implementation. [[User:Arbautjc|Arbautjc]] ([[User talk:Arbautjc|talk]]) 13:32, 7 August 2016 (UTC)
