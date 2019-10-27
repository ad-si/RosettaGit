+++
title = "Plan 9"
description = ""
date = 2008-06-21T19:29:08Z
aliases = []
[extra]
id = 2487
[taxonomies]
categories = []
tags = []
+++

[[Category:Operating Systems]]'''Plan 9''' is an operating system from AT&T's [[Bell Labs]]. It was created by the same group which developed [[UNIX]]. Their intent was to design a new operating system that incorporated new developments since UNIX's creation, such as networking and [[GUI|graphical user interfaces]].

==Programming==
The kernel is written primarily in [[C]], with a minimum of assembly for greater portability. The user-level applications are mostly in C or the [[rc]] shell.

Plan 9 is designed in such a way that the "everything is a file" metaphor extends very well. Network connections can be accessed through /net/tcp, for example, and mouse events are written in plain text to /dev/mouse. Every [[process]] has a separate namespace.

Available languages include:
*C (Plan 9 dialect)
*rc
*[[sh]] ([[POSIX]] emulation)
*[[Perl]]
*[[Python]]
*[[Haskell]]
*[[Assembly]] (all platforms share the same syntax; less machine-specific)

==GUI==
The creators of Plan 9 took the opportunity to build graphics capabilities into the system from the ground up, unlike UNIX. The most frequently-used "window manager" for Plan 9 is called rio, as seen in [http://csplan9.rit.edu/users/john/rosettacode.png this image].

==Compatibility==
Plan 9 is not POSIX-compliant, although it shares similarities with POSIX systems. The architects decided to create a new operating system without backwards-compatibility baggage, which allowed them greater freedom in implementing whatever they thought was important. There is, however, a POSIX emulation layer which allows compilation of some POSIX programs.

==External Links==
*[http://en.wikipedia.org/wiki/Plan_9_from_Bell_Labs Wikipedia: Plan 9 from Bell Labs]
* [http://plan9.bell-labs.com/plan9/ Plan 9 homepage]
