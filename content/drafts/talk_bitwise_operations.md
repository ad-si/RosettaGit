+++
title = "Talk:Bitwise operations"
description = ""
date = 2018-12-17T12:57:25Z
aliases = []
[extra]
id = 2860
[taxonomies]
categories = []
tags = []
+++

Shifts have now been added to the task. Please check your languages to make sure they have shifting properly implemented. --[[User:Mwn3d|Mwn3d]] 17:11, 19 May 2008 (MDT)

== assumption of how integers are stored ==

Since the task is more-or-less bit orientated, the assumption is that integers are stored in binary form (that is, bits).  

This isn't the case for all languages, or for that matter, all computers. 

There exist decimal computers (<strike>albet</strike> albeit, no
longer being built, but a few are still running). I don't mean computers that HANDLE decimal numbers, but RUN in
decimal.

The REXX language stores all it's variables in character form.  REXX does allow the programmer to "get to" the
(actual) binary value of the characters (which may be a representation of an integer, or some other number), but the
binary bits would be different on an ASCII machine vs. an EBCDIC machine. -- [[User:Gerard Schildberger|Gerard Schildberger]] 23:35, 22 March 2012 (UTC)
:I do not see any such assumption.  I do see a request "If any operation is not available in your language, note it."  That said, these operations can be performed in most languages regardless of how numbers are stored:
:There will typically be some largest value for n where all values starting from 0 and less than 2^n can be represented in the language.  On a machine that stores numbers in twos complement binary, n will typically be 31 on a 32 bit machine and 63 on a 64 bit machine, but as long as some kind of integer numbers are representable n will probably be non-zero.  And note that this does not depend on how the numbers are stored.
:There is a one-to-one relationship between any integer in the range 0..((2^n)-1) and a list of true/false values of length n.  If the language is capable of representing such lists, it should not be hard to build functions to convert a number between these two forms.
:And, once you have achieved a binary representation. the rest of the problem is trivial.  The language does not have to do this for you for it to be possible.  --[[User:Rdm|Rdm]] 15:18, 23 March 2012 (UTC)

:: Since the task is BITwise operations, then yes, the assumption ia that the binary representation for integers is meaningful.  In fact, the binary representation of a value of 75 in REXX (language) is '011011100110101'b or '3735'x (in hex) [on an ASCII machine]. One can do bitwise operations on that, but it would be more or less meaningless. Note that this is regardless of how a MACHINE stores (some) numbers, twos complement binary or whatnot.  The task is to see how a LANGUAGE does bitwise, not the machine that it's running on.  So it really does depending on how the numbers are stored.  Even though my computer is a "32-bit" machine, the limitation of the REXX language's repesentation is not 2^31 or 2^32 (-1), but a limitation that (most) people relate to: "1" followed by so many zeroes; or mathematically: 10^p where p has to be AT LEAST 999,999,999 --- which is significantly larger than any binary number can be stored, and for the most uses, enough for most needs --- but then there are those Mersenne prime people who will be pushing even that limit ... It should also be noted that the REXX language stores numbers in this manner no matter what the underlying hardware supports (or doesn't support). In fact, REXX does arithmetic much in the same manner that humans do (say, in the way one muliplies 11.2 and -17.9 on paper), and as a consequence, there's no rounding in handling decimal fractions. But that's another matter that COBOL enthusiasts (and PL/I) would love (think PICtures). I didn't mean to delve into how arithmetic is performed. -- [[User:Gerard Schildberger|Gerard Schildberger]] 18:33, 23 March 2012 (UTC)
::: I do not think it's fair to say that '011011100110101'b represents the integer 75.  It represents a the character sequence '7','5'.  If indirect symbolism were legit here, we'd have huge problems, because anything can represent anything if you allow enough indirection stages.  --[[User:Rdm|Rdm]] 18:45, 23 March 2012 (UTC)

:::: It's not about fairness.  It IS the way the REXX language represents (stores) integers (and every kind/type of number).  Now you get it. 75 is stored as the character sequence '75' !!  There is no indirect symbolism here, that's just the way the REXX language stores EVERYTHING.  No binary integers, no binary floating point numbers, no boolean switches/flags/bits, ... just characters.  There is no indirection stages.  Just the storing of characters. If you think that's indirect symbolism, then, yes, that's what I've been saying (above), bitwise operations on integers that are stored as characters doesn't make that much sense. But the REXX language does allow it --- but most uses of bitwise operations (on integers) aren't usually performed for arithmetic reasons as such). -- [[User:Gerard Schildberger|Gerard Schildberger]] 19:11, 23 March 2012 (UTC)

"If any operation is not available in your language, note it.", 

Such languages should also write about how they differ from the assumptions/try as best they can? --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 12:57, 17 December 2018 (UTC)
