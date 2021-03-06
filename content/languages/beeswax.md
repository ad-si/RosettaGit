+++
title = "Beeswax"
description = ""
date = 2015-12-22T23:59:50Z
aliases = []
[extra]
id = 19853
[taxonomies]
categories = []
tags = []
+++

{{language|beeswax}}

'''beeswax''' is a stack-based 2 dimensional esoteric programming language developed by [[user:Albedo]] (Manuel Lohmann). Beeswax draws inspiration from bees moving around on a honeycomb, and is partly inspired by languages like [http://esolangs.org/wiki/Cardinal Cardinal] etc.
The instruction pointers (bees) move around on a 2D hexagonal grid (the honeycomb). beeswax programs can manipulate their own source code, change the program size, and can read and write files.
beeswax programs are stored in a rectangular grid with a 6-neigborhood. β depicts a bee/instruction pointer.

 21
 3β0
  45

analogous to

    2 — 1
   / \ / \
  3 — β — 0
   \ / \ /
    4 — 5

Bees can perform all kinds of arithmetic operations, jumps, code modifications etc. Each bee carries a fixed-size stack of length 3 and can store or retrieve data by interacting either with the global stack (of arbitrary size) or the honeycomb/source code itself.

Data on the global stack can be written to files on disk, and global stack contents can be written to files on disk.

All data in the local and global stacks are unsigned 64-bit integers, output as characters uses UTF8 encoding.


== More information ==

* [[eso:Beeswax|Esoteric languages wiki entry]]

== Reference implementation, example programs and complete language specification ==

* [https://github.com/m-lohmann/BeeswaxEsolang.jl GitHub repository containing reference implementation in Julia, example programs and language specification]


[[Category:Esoteric_Languages]]
