+++
title = "Talk:Read entire file"
description = ""
date = 2016-11-13T13:10:08Z
aliases = []
[extra]
id = 13081
[taxonomies]
categories = []
tags = []
+++

== Encoding selection ==

The task description mentions "encoding selection". What has encoding got to do with reading entire file?
If you read a file into memory, you just read the file. Encoding is something to be considered when you are manipulating the data.
I assume the "encoding" refers to text encoding. However, the file may be a picture file, or binary data or whatever.
--[[User:PauliKL|PauliKL]] 16:09, 6 March 2013 (UTC)
: The primary objective is to read a file into a string, not necessarily into memory. Some languages are encoding-aware and behave accordingly. Ruby, for instance:
:
```Ruby
['ASCII', 'UTF-8'].map { |e| File.open('foo', encoding: e).read.size }
# => [3, 1]
```
[[User:Isopsephile|Isopsephile]] 16:43, 6 March 2013 (UTC)

: It depends on whether you're reading the file as a sequence of bytes or as a sequence of characters. The encoding is only required when converting from bytes to characters (or ''vice versa'', of course). Failure to properly distinguish between the two concepts has been the cause of a huge amount of pain over the past decade or two, pain which we're only gradually emerging from as an industry. Thank goodness for Unicode and UTF-8. –[[User:Dkf|Donal Fellows]] 22:43, 7 March 2013 (UTC)

== Fortran ==

There was some stuff pretending that it's impossible to read the file into memory, and that the only way is some convoluted loop, with goto (!). Sorry, that's wrong as of the current Fortran standard. I replaced this garbage with an example allocating a character string exactly the right size, and reading the file in stream access.

It's of course not the ''only'' way: it's possible to read file blocks, but here it is asked to read an entire file.
Also, production code would control errors.

I also showed another example using Intel Fortran, that makes use of the Windows API to create a memory map of the file.

[[User:Arbautjc|Arbautjc]] ([[User talk:Arbautjc|talk]]) 20:00, 11 November 2016 (UTC)

:The initial example was for UNFORMATTED and notionally would read the file in one go, but the problem was that there was no idea how large the file would be so there was no way to make STUFF the right size to receive its content because the INQUIRE statement has no "file size" specifier. This was explained. Except that lo! In F2003 there ''is'' just such a facility, though not in F90/95 or earlier, except for non-standard installations. Amusingly, a google search immediately tossed up reports of malfunction via one system or another, and questions as to whether the units are bytes or what, but no matter. In the absence of a size discovery, and the absence of some convoluted system interface facility (that is not a part of the language) to the same effect, what then? [[User:Dinosaur|Dinosaur]] ([[User talk:Dinosaur|talk]]) 10:52, 13 November 2016 (UTC)
::Convoluted intreface facility? With ISO_C_BINDING and a bunch of modules, Intel Fortran and Absoft Pro Fortran make access to Windows system functions almost trivial (but you can already see that, since I added a solution with IFORT - I don't think that this 20 line program is "convoluted"). The size given by INQUIRE is not well defined? Not quite so, sir, have a look at FILE_STORAGE_SIZE in the ISO_FORTRAN_ENV module, and then section 9.10.2.30 if the Fortran 2008 standard. I don't think it's a good advice to give to RC readers that Fortran is stuck at its 1978 status. Fortran has evolved, and is still evolving. Compilers do implement [http://fortranwiki.org/fortran/show/Fortran+2003+status Fortran 2003] quite well by now, and some give full [http://fortranwiki.org/fortran/show/Fortran+2008+status Fortran 2008] conformance. Besides, the only official ISO standard is the last. And Fortran 2015 is underway. Wake up! You are repeatedly giving bad advice and a completely wrong view of what Fortran is today. I am sorry to have to tell you this, but people willing to learn a language deserve better. Feel free to update you knowledge: http://www.j3-fortran.org/doc/year/10/10-007r1.pdf
::FYI, Rosetta Code is about ''[[Rosetta Code|"[...] solutions to the same task in as many different languages as possible, to demonstrate how languages are similar and different, and to aid a person with a grounding in one approach to a problem in learning another."]]''. It's not about technology that was already outdated 40 years ago.
::[[User:Arbautjc|Arbautjc]] ([[User talk:Arbautjc|talk]]) 11:36, 13 November 2016 (UTC)
