+++
title = "Talk:UTF-8 encode and decode"
description = ""
date = 2017-03-12T08:55:37Z
aliases = []
[extra]
id = 21333
[taxonomies]
categories = []
tags = []
+++

Is the task more about converting between a Unicode codepoint (an integer) and the bytes? or converting between a displayed character and bytes? In the solutions for some languages, they basically start with a 1-character string (because that's the natural way to represent a character in that language), and convert that string to and from the bytes, and also separately get the codepoint integer out from the string to print to the output. Is that acceptable for this task, or is it necessary to actually convert between the integer and the bytes? --[[User:Spoon!|Spoon!]] ([[User talk:Spoon!|talk]]) 21:57, 8 March 2017 (UTC)
:: I actually envisioned that the task should demonstrate how to actually implement the basic UTF-8 encoding and decoding algorithm in each language, which does mean explicitely converting back and forth between the code point and the 1-4 octets, and NOT to just use some standard library trick to get a quick solution. Otherwise I would have just referenced the one-liner library solution myself and be done with it. [[User:Avi|Avi]] ([[User talk:Avi|talk]]) 08:55, 12 March 2017 (UTC)
