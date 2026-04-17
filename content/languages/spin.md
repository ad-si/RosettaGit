+++
title = "Spin"
description = ""
date = 2019-03-04T04:46:37Z
aliases = []
[extra]
id = 21128
[taxonomies]
categories = []
tags = []
+++
Spin is the primary high level language for programming the Parallax P8X32A microcontroller.

Typically a Spin compiler runs on a PC and produces Spin bytecode for the bytecode interpreter each P8X32A chip contains.

Atypical:

[FastSpin](http://forums.parallax.com/discussion/164101/fastspin-an-lmm-compiler-for-spin) can compile Spin code to C, C++, LMM-PASM and COG-PASM (the native machine code of the P8X32A) and even generate C or C++ code for e.g. GCC on other systems than the Parallax P8X32A microcontroller.

[Sphinxcompiler](http://www.sphinxcompiler.com/) and [Spinc](http://forums.parallax.com/discussion/123795/spinix/p1) are executed on operating systems running on the P8X32A.

## Implementations
* [The Propeller Tool](https://www.parallax.com/downloads/propeller-tool-software-windows) - Parallax's original Spin IDE.
* [Brad's Spin Tool](http://www.fnarfbargle.com/)
** BST - IDE (monolithic binary including compiler)
** BSTC - stand alone commandline compiler
* [HomeSpun](http://forums.parallax.com/discussion/106401/homespun-spin-compiler-0-31-now-open-source)
* [Sphinxcompiler](http://www.sphinxcompiler.com/)
* Spinc (included in [Spinix](http://forums.parallax.com/discussion/123795/spinix/p1))
* [OpenSpin](https://github.com/parallaxinc/OpenSpin)
* [FastSpin/FlexSpin](http://forums.parallax.com/discussion/164101/fastspin-an-lmm-compiler-for-spin)

## Links
* <https://www.parallax.com/sites/default/files/downloads/P8X32A-Web-PropellerManual-v1.2.pdf>
* <https://lamestation.atlassian.net/wiki/display/SPIN>
* <https://github.com/rosco-pc/propeller-wiki>

----
