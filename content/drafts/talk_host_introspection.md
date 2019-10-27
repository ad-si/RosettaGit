+++
title = "Talk:Host introspection"
description = ""
date = 2019-07-24T08:48:21Z
aliases = []
[extra]
id = 3084
[taxonomies]
categories = []
tags = []
+++

== Ada ==
The Ada example provided is technically not correct. The `Bit_Order` attribute does not correspond to ''byte'' endianness, it is a representational attribute used to specify the order of ''bits'' in the layout of a machine scalar in a record representation clause. While this is known as ''bit'' endianness' It has no impact on the byte ordering within a machine scalar. This is described in section '13.5.3 Bit Ordering' of the Ada Reference Manual. The language is a bit ambiguous, but this seems to refer explicitly to bit ordering.

== C ==
The C example is not correct: While it is true that on current popular platforms a pointer is as large as a word, this is not universally true. For example in x86 16 bit code using the large memory model a pointer has two words (one for the segment, one for the offset). I'd not be surprised if there are other, more current platforms (especially embedded ones) where pointer size and word size don't agree either.

Before 64 bit platforms appeared, the size of an int was a good indicator of the word size, because it was intended to be the fastest integer type, and that's typically the word size. Thus for 16 bit code, int was 16 bit, and for 32 bit code, int was 32 bit. However, when the transition to 64 bits came, compatbility with code making hard assumptions about the size of int was considered more important, thus int remained at 32 bits.

Maybe using sizeof(size_t) would be a better test: Even though on 16 bit x86, pointers can be 32 bits, object sizes always fit into 16 bits (because on 16 bit systems a segment only is that large). Of course it's not guaranteed either, but it's at least the best bet you can make.

Also, multiplying with 8 isn't quite right either: While today a byte is commonly 8 bits, this is not guaranteed. I'm not sure if today there are systems sold where a byte has not 8 bits (again, embedded systems might be prime candidates to look at, as they might well lack support for sub-word addressing, making a byte as large as a word), however in limits.h there exists a macro CHAR_BIT which holds the number of bits in a byte, so there's no need to make any possibly wrong assumptions.

I'm going to change the C example according to the explanations above. --[[User:Ce|Ce]] 17:35, 13 October 2008 (UTC)

== Dupe? ==

Is this too similar to [[Introspection]]? Should they be combined? --[[User:Mwn3d|Mwn3d]] 18:14, 13 October 2008 (UTC)

:I'm fine merging them, but they're inspecting different things.  [[Introspection]] asks a programming language to inspect ''itself'', and [[Host Introspection]] asks a programming language to inspect its ''host''.  This is pretty different from a language asking questions about itself (maybe a better name for [[Introspection]] is [[Reflection]]?).
:Some (many) languages abstract away the host, so that programs can run on many different types of host, without requiring host-specific tests or changes.  If your language is in this category, this task asks whether it still gives you access to host-specific information (i.e. the abstraction means you don't ''have'' to care about the host, but introspection allows you to care if you ''want'' to).

== Is Java a program ==
I thought everything in Java had to be a class? Is the two lines of java truly a compilable program? (I'm asking 'cos I don't know). --[[User:Paddy3118|Paddy3118]] 23:37, 13 October 2008 (UTC)
:No it's not a compilable program. I just thought it'd be silly to set up a whole class and main method for those two little lines. I'm pretty sure there are a couple of Java examples like that scattered about. --[[User:Mwn3d|Mwn3d]] 00:20, 14 October 2008 (UTC)

== Perl Program ==

It looks like Perl uses 4-bytes as int size, even on 64-bit systems (yes, I've 64-bit version of Perl). The examples aren't valid...

 C:\Users\Konrad\Desktop>perl
 use Config;
 print "int size: $Config{intsize}, byte order: $Config{byteorder}\n";
 ^D
 int size: 4, byte order: 12345678

[[User:GlitchMr|GlitchMr]] 18:55, 24 October 2011 (UTC)

: I decided to change the program from $Config{intsize} to $Config{uvsize}, so that the byte order has the same size. ([http://perldoc.perl.org/Config.html perldoc Config] says that $Config{byteorder} is for UV.)

: "Word size" has multiple meanings. For an amd64 (x64) processor, some programmers think that a ''word'' is 16 bits, a ''double word'' is 32 bits, and a ''quad word'' is 64 bits. Other programmers think that a ''word'' is 32 bits, like an ''int''. Yet other programmers think that a ''word'' is 64 bits, like a pointer. So $Config{intsize} and $Config{uvsize} are both correct even if they give different sizes. --[[User:Kernigh|Kernigh]] 03:38, 25 October 2011 (UTC)
