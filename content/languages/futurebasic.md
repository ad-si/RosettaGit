+++
title = "FutureBasic"
description = ""
date = 2016-08-22T22:46:45Z
aliases = []
[extra]
id = 21052
[taxonomies]
categories = []
tags = []
+++
FutureBasic began life as Zbasic, a commercial variant of [BASIC](https://rosettacode.org/wiki/BASIC) for the early Macintoshes, but has grown far beyond that into a mature freeware IDE that, through its FBtoC translator, can be used to compile C and Objective-C [object-oriented](https://rosettacode.org/wiki/object-oriented) code using the clang compiler included with an Xcode installation. It is excellent as a educational tool and for fast prototyping -- especially in Objective-C (Cocoa) by those who prefer programmatic code over the overhead of Xcode. Among its enthusiasts are commercial developers, engineers, professors, doctors, musicians, writers and a host of amateurs who program with FB for the sheer joy of it.

## FutureBasic Home Page & Download
http://4toc.com/fb/index.htm

Here is where you can download your freeware copy of the FutureBasic IDE for Macintosh OS X 10.4 and newer, along with detailed installation instructions (FB requires installation of Xcode and QuickDraw headers); programming examples; and other information.

On 1 January 2008, Staz Software announced FB as freeware. Downloads and their executables are freeware, but source code and rights of distribution are reserved to the respective authors (the FBtoC team and Staz Software). The IDE is continuously being improved.

FB 5.x builds Mac OS X Carbon applications (32-bit only) and command-line tools (32- and 64-bit).

The FB 5 IDE consists of a syntax-aware editor, and a translator (FBtoC) that converts FB code into C code. The translation is then compiled with the system compiler gcc, or in the most recent versions, clang.

Here is a sample program:

```txt

include "ConsoleWindow"

local fn PrintSomething( str as Str255 )
 print str
end fn

fn PrintSomething( "Hello, World!" )

```


## FutureBasic List
http://freegroups.net/groups/futurebasic/

The FutureBasic mailing list is a free service to the FB programming community, courtesy of associate.com. The list is available by online, and/or by email subscription to anyone interested FB programming on the Macintosh. List members include raw beginners through published commercial software authors. The FB development team and some long-time enthusiasts are knowledgeable and friendly and are very quick to respond to questions posted on the list. In addition, demonstration program code is frequently posted here.

## Wikipedia
https://en.wikipedia.org/wiki/FutureBASIC

Discusses the history of FutureBasic and its predecessor, ZBasic, from the early days of the Macintosh when it was a commercial product, until its morph into today's robust front end to the clang compiler. Information on this page can be outdated, to a better source of the most current information about FB can be found at the web sites above.

## Why FutureBasic?
Considering the contempt some programmers have for the BASIC language -- "BASIC ruins programmers" -- it's almost a shame FB has the word "Basic" in its official name. Not only can FB handle BASIC source code, but since it is a front end to clang, it can translate C, Apple's Core Foundation, Objective-C (Cocoa), HTML, XML, SOAP, UNIX Shell, Open GL, etc. This makes it an excellent tool for prototyping -- especially for programmatic Objective-C when the overhead of Xcode is not needed.

According to Wikipedia, FutureBasic began life at the dawn of Apple's Macintosh in the mid-1980s as ZBasic, an implementation of '''BASIC''' -- the ''Beginner's All-purpose Symbolic Instruction Code'' -- which had been around since the language was invented by John G. Kemeny and Thomas E. Kurtz at Dartmouth College during 1963 and 1964.

ZBasic acquired a devoted following of developers who praised its ease of use and the tight, fast code produced by the compiler (a legendary labor involving extensive use of hand-built 68K assembly language code and the brainchild of Andrew Gariepy).

In 1992, Zedcor Inc., the company of the Gariepy brothers Andy, Mike, Peter and friends based in Tucson, Arizona presented announced their reworked compiler called FutureBASIC.

In 1995 Staz Software, led by Chris Stasny based in Diamondhead, Miss., acquired the rights to market FutureBASIC. Stasny started this business with an upgraded version, namely FBII, and with his own development, the Program Generator (PG PRO), a CASE tool.

When Apple transitioned the Mac from 68k to PowerPC, the FB editor was rewritten by Stasny and was coupled with an adaptation of the compiler by Andy Gariepy. The result of their efforts, a dramatically enhanced IDE called FB^3 (FB-cubed) was released in September 1999.

Major update releases introduced a full-featured Appearance Compliant runtime written by the late New Zealander Robert Purves renown for his brilliant programming. Once completely carbonized to run natively on the Mac OS X, the FutureBASIC Integrated Development Environment (FB IDE) was called FB4 and released in July 2004.

In August 2005, Staz Software was devastated by Hurricane Katrina just at the time Apple was transitioning from Motorola PPC microprocessors to Intel chips. FB development slowed  almost to a standstill. On January 1, 2008, Staz Software announced that FB would henceforth be freeware and FB4 with FBtoC 1.0 was made available.

Since that time, an independent team of volunteer developers initially lead by Purves continued to improve FBtoC, which took code produced by the FB Editor and translated it to C for processing by gcc, and more recently the more robust clang.

On Sunday, June 3, 2012, members of the FB List Serve were notified that Robert Purves had died after a long bout with cancer. The news came as a surprise to many FB developers who were unaware of Purves' illness. While coping with cancer, he continued as an active member of the FB community, improving FB, answering questions, solving problems, and posting exquisitely terse code often salted with pithy remarks from his wonderfully dry humor. He never mentioned his health problems and never complained. A tribute to Purves can be found at the bottom of the FB Home Page

Today, a team of skilled developers who worked on the FB editor, who were also tutored at Purves' knee on his pet FB project, the FBtoC translator, continue his work keeping the Macintosh's oldest compiler viable for a new generation of coders.
