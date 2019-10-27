+++
title = "Talk:String Byte Length"
description = ""
date = 2007-05-14T22:58:46Z
aliases = []
[extra]
id = 2038
[taxonomies]
categories = []
tags = []
+++

The C and C++ examples of finding the length of a string of wide characters (wchar_t) were just plain wrong, so I deleted them. Regardless of string length, the examples would always show 16 because it was computing against the length of a ''pointer'' to a wchar_t, rather than the length of the string. --[[User:139.85.252.186|139.85.252.186]] 17:00, 23 April 2007 (EDT)
: I've undone the delete of the C++ example. While you were right about the C example (except it will show the product of a pointer size and a wchar_t size, which is not necessarily 16, but depends on the CPU and OS), the C++ example does not even contain a pointer (except hidden somewhere in the internals of C++), and std::wstring::length() returns the number of characters, not the size of a pointer (which would be quite pointless anyway). --[[User:Ce|Ce]] 17:40, 23 April 2007 (EDT)


The task should clarify what "byte length" mean.  For C, with its
traditional representation of strings and notion of mulitbyte
encodings byte length makes perfect sense.  But for other
string representations that is not clear: do we mean amount
of storage taken by string?  However, C string length is
smaller than storage use (because of null terminator) and
other languages frequently add extra data to strings: tags,
count giving current length, count giving capacity.  Also,
there is separate task devoted to size of variables, so
I would prefer avoid question of storage size here.

One can talk about "payload size": storage ocupated by characters
itself, ommiting control information.  But if language uses
sophisticated reprezentation of string it may be quite difficult
to separate payload form control information.

IMHO the most sensible formulation (and having most practical
applications!) is to determine byte length of the printed
representation of the string in some external byte-oriented
encoding (say UTF-8).  If one uses such interpretation then
many of current solutions will be incorrect.
