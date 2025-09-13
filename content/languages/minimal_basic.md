+++
title = "Minimal BASIC"
description = ""
date = 2019-08-03T10:53:00Z
aliases = []
[extra]
id = 22445
[taxonomies]
categories = []
tags = []
+++

'''Minimal BASIC''' is a very small subset of the [BASIC](https://rosettacode.org/wiki/BASIC) language. It is specified in ECMA Standard 55, ANSI X3.60-1978 and others. It has a rigorous test suite (initially NBSIR 77-1420 from 1978) to ensure that vendors supplying computer systems to the US Federal government met compatibility guidelines.

The language is very limited:

* line numbers are required
* no string manipulation commands
* all variables must be defined/modified using the LET keyword
* the only loop structure is FOR … TO … STEP …, with loop parameters fixed at entry
* all programs must end with an END statement.

Implementations:

* Microsoft BASIC 5.0 for 8080 (BASIC-80, MBASIC for CP/M - 1981) claims compliance in its manual.
* [https://jorgicor.niobe.org/bas55/ bas55]
* [https://buraphakit.sourceforge.io/BASIC.shtml John's ECMA-55 Minimal BASIC Compiler]

Compare with [Full BASIC](https://rosettacode.org/wiki/Full_BASIC).
