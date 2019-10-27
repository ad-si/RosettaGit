+++
title = "Talk:Compound data type"
description = ""
date = 2010-02-06T12:49:26Z
aliases = []
[extra]
id = 1973
[taxonomies]
categories = []
tags = []
+++

This is a completely different task now.

I'm not saying that it is inappropriate or anything, just that I think it would vhave been better to create a new task rather than completely change the definition of a task that already has a bunch of code examples in it. The original task ("Create a structure Point(x,y) is appropriate for any language that is capable of collecting various items into a single variable. The modified version excludes all high-level languages (I'd say it is pretty much the definition of a HLL that you do not have to think about things like "how is this going to look like in memory"). Quite frankly I doubt the utility of a task that boils down to "access your machines memory in a certain particular bit-for-bit way"; but that is what this has become. [[User:Sgeier|Sgeier]] 18:35, 25 February 2007 (EST)

: Moreover, the description of the second task is plain wrong.
:* There's no guarantee that char has 8 bits (it is only guaranteed that it has ''at least'' 8 bits), and while all popular computer architectures use 8-bit bytes, there are AFAIK still machines sold where this is not the case (some IBM mainframes IIRC).
:* Also, there's no guarantee that short is 16 bit and long is 32 bit. Indeed, IIRC on some 64-bit architectures long is 64 bits. I think that's because until the 1999 standard (C99), C did not officially support the long long int type.
:* While int indeed is intended to be the "natural" size, which could reasonably be interpreted as "register size", in practice this is not universally true either. On 64 bit platforms, int is quite often still 32 bit, in order to maintain compatibility with (technically broken) code which assumes 32-bit int.
:* There's even less a guarantee on the floating point types. Well, if you are on an IEEE floating point compatible platform, then the sizes of float and double are virtually guaranteed by the IEEE floating point standard (which is separate from the C standard). However, the size of long double isn't guaranteed, and indeed, with a ''very'' popular OS/compiler combination (Visual C++ on 32-bit Windows), [http://support.microsoft.com/default.aspx?scid=kb;en-us;129209  long double is exactly the same size as double]. Of course not every platform has IEEE compliant floating point anyway, thus your floats, doubles and long doubles might be any size.
:* The char[80] also is not an ANSI string. It's a character array, using whatever character encoding is used on the machine. On Windows this is typically "ANSI" (more exactly, CP-1252, which is a slight variation of ISO-8859-1), on Linux, it's typically ISO-8859-1, and on EBCDIC systems (yes, they still exist), it's typically EBCDIC (who would have guessed).
: At least the comment on the wchar_t array is correct: It's indeed not sure that it is UTF16; on Linux e.g. it's typically UCS4 aka UTF32. Of course the standard doesn't guarantee that it's even Unicode; I don't know if there's any implementation which doesn't use Unicode here, though.
: BTW, if you want guaranteed integer sizes, then you can use C99's int8_t etc.; AFAIK there's no equivalent for floating point. --[[User:Ce|Ce]] 09:06, 26 February 2007 (EST)
::I reverted the task to its most recent version with the previous description.  It pains me to see all that work thrown away, but the task became too specific and, as you folks pointed out, inaccurate.  I'm going to block the IP of the offending user until I can get in touch with him. --[[User:Short Circuit|Short Circuit]] 10:46, 26 February 2007 (EST)

== New page name ==

Unfortunately the new name is as ambiguous as the old. My first thought when I read "complex data type" is "a type which models complex numbers." --[[User:Ce|Ce]] 19:26, 26 February 2007 (EST)
:I realized later it should be "Compound Data Type" ... This page is going to leave a trail through the namespace a mile wide... --[[User:Short Circuit|Short Circuit]] 19:31, 26 February 2007 (EST)
