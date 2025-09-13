+++
title = "SequenceL"
description = ""
date = 2015-12-15T03:45:58Z
aliases = []
[extra]
id = 19813
[taxonomies]
categories = []
tags = []
+++
SequenceL is a general purpose functional programming language, whose primary design objectives are performance on multicore hardware, ease of programming, and code clarity/readability. Its primary advantage is that it can be used to write straightforward code that automatically takes full advantage of all the processing power available, without the developer having to concern themselves with identifying parallelisms, avoiding race conditions, and the like.

Programs written in SequenceL can be compiled to multithreaded code that runs in parallel with no explicit indications from the programmer of how or what to parallelize. (Current versions of the SequenceL compiler generate C++ and OpenCL code, though other languages may also be supported.) An OS-specific runtime manages the threads safely, automatically providing parallel performance according to the cores available.

## Citations
* [Wikipedia:SequenceL](https://en.wikipedia.org/wiki/SequenceL)
