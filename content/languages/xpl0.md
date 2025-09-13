+++
title = "XPL0"
description = ""
date = 2012-05-14T00:26:24Z
aliases = []
[extra]
id = 11648
[taxonomies]
categories = []
tags = []
+++
'''XPL0''' is essentially a cross between Pascal and C. It looks somewhat like Pascal but works more like C. It was originally created in 1976
by Peter J. R. Boyle, who designed it to run on a 6502 microprocessor as
an alternative to BASIC, which was the dominant language for personal
computers at the time. XPL0 is based on PL/0, an example compiler in the
book ''Algorithms + Data Structures = Programs'' by Niklaus Wirth. The first
XPL0 compiler was written in ALGOL, which was then used to create a compiler
written in XPL0's syntax.

XPL0 has been implemented on more than a dozen processors, but it's
currently maintained for IBM-type PCs. Programs run under DOS and under versions of Windows that can still run DOS apps. Free, open-source versions of the
compilers (interpreted, assembly-code compiled, and optimizing) are
available from the official website: xpl0.org [http://www.xpl0.org/]. The 32-bit version of the compiler, XPLPX, was used for all these Rosetta Code tasks.

Here's how the traditional Hello World program is coded:


```txt

      code Text=12;
      Text(0, "Hello World!")

```


Text is a built-in routine, called an ''intrinsic'', that outputs a string
of characters. The zero (0) tells where to send the string. In this case
it is sent to the display screen; but it could just as easily be sent to
a printer, a file, or out a serial port by using a different number.

All names must be declared before they can be used. The command word
''code'' associates the name Text to the built-in routine number 12, which
outputs strings. There are about 80 of these built-in routines that
provide capabilities such as input and output, graphics, and trig
functions.
