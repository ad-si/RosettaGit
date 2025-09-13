+++
title = "Z80 Assembly"
description = ""
date = 2011-11-26T18:46:43Z
aliases = []
[extra]
id = 5411
[taxonomies]
categories = []
tags = ["assembly"]
+++

Z80 Assembly is an assembly language for the Zilog Z80 processor,
which was introduced in 1976 and used in 1980s home computers
such as the [Sinclair ZX Spectrum](https://en.wikipedia.org/wiki/ZX_Spectrum)
and the [Amstrad CPC](https://en.wikipedia.org/wiki/Amstrad_CPC) series.
In the late 1990s,
some TI graphing calculators (e.g. the TI-83 series) were also Z80-based.

The Z80 is binary compatible with the earlier Intel 8080,
but the assembly code is different because instead of having
several different commands for loading and storing data,
there is only one,<tt>LD</tt>, on the Z80.
Therefore, on the assembly level, Z80 code is actually closer to 8086 code
when <tt>MOV</tt> is changed to <tt>LD</tt>
and the different register structure is taken into account.
<ref>http://en.wikipedia.org/wiki/Z80#Z80_assembly_language</ref>

Today, the Z80 is still widely used in embedded systems
and consumer electronics.
Several cross assemblers exist for the Z80,
e.g. [z80asm](http://savannah.nongnu.org/projects/z80asm/)
Also, some emulators of e.g. the Amstrad CPC feature a built-in assembler.


## See also

- ["Programming the Z80"](http://www.z80.info/zaks.html)
    by Rodney Zaks, 1981 3rd ed. (PDF)—downloadable with permission of the author
- [Z80 instruction set and opcodes](http://www.grimware.org/doku.php/documentations/devices/z80)
- [Z80 product specification and data sheet](http://www.produktinfo.conrad.com/datenblaetter/175000-199999/181862-da-01-en-Z_80_A_CPU.pdf)
- [Z80 information site](http://www.z80.info/)
