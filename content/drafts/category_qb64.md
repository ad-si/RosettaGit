+++
title = "Category:QB64"
description = ""
date = 2017-12-25T09:13:57Z
aliases = []
[extra]
id = 21684
[taxonomies]
categories = []
tags = []
+++

{{implementation|BASIC}}{{IDE}}{{Compiler}}

'''QB64''' is a self-hosting [[BASIC]] compiler for [[Windows]], [[Linux]] and [[Mac OS|Mac OS X]], designed to be compatible with [[QuickBASIC|QBasic and QuickBASIC]]. QB64 is a [[C++]] emitter, which is integrated with a C++ compiler to provide compilation via C++ code and [[GCC]] optimization.

QB64 implements most QBasic statements, and can run many QBasic programs, including [[Microsoft]]'s "Gorillas" and "Nibbles" QBasic games. Furthermore, QB64 has been designed to contain an [[IDE]] resembling the QBASIC IDE. QB64 also extends the QBasic programming language to include 64-bit data types, as well as better sound and graphics support.

QB64's syntax is designed to be completely backwards compatible with QuickBASIC. Line numbers are not required, and statements are terminated by newlines or by colons (:).

An example of the "Hello, World" program is:
PRINT "Hello, World!"

QB64's extended commands begin with an underscore in order to avoid conflicts with any names that may be used in a QuickBASIC program. QB64 extends the QuickBASIC language in several ways. It adds the new data types including _BIT, _BYTE, _INTEGER64 and _FLOAT as well as unsigned data types. The new data types have suffixes just like the traditional BASIC data types. QB64 also includes an audio library which allows playing most common audio formats including MP3, Ogg Vorbis, and WAV files as well as libraries allowing users to use higher resolution graphics than the 640×480 offered by QuickBASIC, use different fonts, and plot images in BMP, PNG, and JPEG format. It also allows the use of 32-bit colors as opposed to the limited 256 (or 16, depending) colors originally offered. The programmer also does not have to specify which programming libraries to include since QB64 does it automatically. The programmer has the option to include a library of their own through the $INCLUDE command just as QuickBASIC did.

==See also==
* [[wp:QB64|QB64 on Wikipedia]]
* [http://www.qb64.net/ QB64 homepage]
* [http://www.thejoyfulprogrammer.com/qb64/forum/index.php?rndtime=1512833625226929130/ QB64 Partner Site QB64.TheJoyfulProgrammer.com]
* [https://www.qb64.org/ QB64 Partner Site QB64.org]
