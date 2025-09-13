+++
title = "Locomotive Basic"
description = ""
date = 2017-01-19T02:50:34Z
aliases = []
[extra]
id = 8410
[taxonomies]
categories = []
tags = []
+++
'''Locomotive BASIC''' is a variant of BASIC that is built into the ROM of the [Amstrad CPC](https://en.wikipedia.org/wiki/Amstrad_CPC) series of Z80-based home computers introduced in 1984. In the 1980s, CPCs were a popular, slightly more expensive alternative to the Commodore C64/C128 and were not just suited for games but also office work because of their high resolution displays and [CP/M](https://en.wikipedia.org/wiki/CP/M) support. They came with a built-in cassette deck (CPC 464) or 3" disk drive (CPC 664 and
6128) as storage devices and a "green screen" or color monitor which also housed the power supply. Sales were particularly strong in the UK, Germany, France, and Spain. Today, Locomotive BASIC can be used via CPC emulators such as [http://www.winape.net/ WinAPE] or [http://sourceforge.net/projects/javacpc/ JavaCPC]—which is perfectly legal because Amstrad has given their permission for distributing CPC ROM images with emulators.

## Features
Locomotive BASIC was comparatively advanced for its time (with e.g. software interrupts and comprehensive graphics and audio commands) and is very cleanly implemented, just like the rest of the Amstrad CPC ROM. No doubt this was partly because the CPC debuted relatively late during the era of 8-bit machines, so Locomotive Software had more of an opportunity to consider the good and bad software design decisions in existing home computers. System calls are made with <tt>CALL</tt> via dedicated jump blocks in RAM, so they are unaffected by changes to the ROM between CPC models. This makes software very compatible between the original three CPC models and ''mostly'' compatible for the later, considerably less popular "Plus" models that came out in 1990. Together with AMSDOS, the '''Ams'''trad '''D'''isk '''O'''perating '''S'''ystem, Locomotive BASIC is also used for disk and tape operations. (Some commands however, e.g. <tt>disckit3</tt> for formatting disks, are only available under CP/M.)

## Z80 Extensions
As on many other 8-bit machines of the era, BASIC programs are often extended with [Z80](https://rosettacode.org/wiki/Z80_Assembly) machine code which is <tt>READ</tt> from <tt>DATA</tt> statements, <tt>[POKE](https://en.wikipedia.org/wiki/PEEK_and_POKE)</tt>-d to RAM, and then <tt>CALL</tt>-ed—an approach that was especially popular with type-in games in CPC magazines. The CPC also has a more convenient form of BASIC extensions, RSX<ref>http://www.cpcwiki.eu/index.php/RSX</ref> commands ('''r'''esident '''s'''ystem e'''x'''tensions). They are easily recognized by being prefixed with a pipe character and have the advantage of being freely relocatable in memory. Some RSX commands, such as "|ren" to rename files, are part of the AMSDOS ROM, but RSX routines can also reside in RAM, e.g. to provide new graphics primitives or other new capabilities to BASIC.

## Memory management
Memory on the CPC 464 and 664 is subdivided into four 16 kB blocks (0 to 3), with block 3 at &c000 normally reserved for the screen. The CPC 6128 features a second 64 kB bank (blocks 4 to 7) which can be accessed from BASIC with [bank switching](https://en.wikipedia.org/wiki/Bank_switching) in block 1 (&4000 and &7fff), e.g.


```locobasic
out &7fff,&x11000100
```


would make block 4 accessible in the address space of block 1.<ref>http://k1.dyndns.org/Vintage/Schneider%20CPC/Das%20Schneider%20CPC%20Systembuch/z87.htm</ref> This way, the entire 128 kB of memory can be used by BASIC. Alternatively, RSX commands for bank switching and copying between banks are included on the system discs and discussed in chapter 8 of the CPC user manual.<ref>http://www.cpcwiki.eu/index.php/User_Manual</ref> Even on the 64 kB models, it is possible to do [double buffering](https://en.wikipedia.org/wiki/Multiple_buffering) in BASIC by reserving another RAM block for the screen, drawing into the hidden screen, and then setting the CRTC screen address to the currently hidden screen with the BASIC [port I/O](https://en.wikipedia.org/wiki/I/o_port) command <tt>OUT</tt>.

## Language versions and user manual
Version 1.0 of Locomotive BASIC shipped with the CPC 464, later models included Locomotive Basic v1.1 which brought some important improvements. Despite the fact that all later BASIC versions call themselves v1.1, ROM headers show that e.g. the Amstrad Plus version is actually v1.40.<ref>http://www.grimware.org/doku.php/documentations/software/locomotive.basic/start#basic.1.1</ref> Amstrad CPC emulators usually default to a CPC 6128 with a color monitor and BASIC 1.1.

The extensive CPC user manual features a finely written introduction to BASIC that does not feel as rushed or dumbed-down as in some other manuals of the time. Obviously Amstrad expected many customers would want to program their CPCs themselves, not just use them with off-the-shelf software. There are even some pretty decent BASIC type-in games in appendix 3, such as clones of the arcade classics ''[Breakout](https://en.wikipedia.org/wiki/Breakout_(video_game))'' and ''[Pong](https://en.wikipedia.org/wiki/Pong)''.

## Getting started with Locomotive BASIC
Either use a browser-based CPC emulator (http://www.cpcbox.com/) or download a binary for your platform. A list of emulators is available at http://cpcwiki.eu/index.php/Emulators. JavaCPC (http://sourceforge.net/projects/javacpc/) or WinAPE (http://www.winape.net/) are particularly recommended. An advantage of native emulators is that they tend to have copy-and-paste functionality, so you can edit programs in external editors.

In the emulator, type


```locobasic
10 print "Goodbye, World!"
run
```


to run your first Locomotive BASIC program.

## References
<references />

## See Also
* Locomotive BASIC documentation:
** [Locomotive BASIC](https://en.wikipedia.org/wiki/Locomotive_BASIC) at Wikipedia
** [http://www.grimware.org/doku.php/documentations/software/locomotive.basic/ Locomotive BASIC command reference with code examples] (based on the official documentation)
** [http://www.cpcwiki.eu/index.php/User_Manual Scanned CPC user manuals]
** [http://www.cpcwiki.eu/index.php/Locomotive_BASIC Locomotive BASIC] at the [http://www.cpcwiki.eu/ Amstrad CPC Wiki]
* CPC emulators:
** [http://sourceforge.net/projects/javacpc/ JavaCPC emulator]
** [http://cpcbox.com/ Online CPC emulator in JavaScript] (best performance in Firefox, but also works in Chrome)
** [http://www.cpcwiki.eu/index.php/Emulators List of CPC emulators]
