+++
title = "Oz"
description = ""
date = 2019-02-13T19:06:54Z
aliases = []
[extra]
id = 2769
[taxonomies]
categories = []
tags = []
+++
The [Mozart](https://rosettacode.org/wiki/Mozart/Oz) Programming System is the primary implementation of Oz. It is released with an [open source](https://rosettacode.org/wiki/open_source) license by the Mozart Consortium. Mozart has been ported to different flavors of [Unix](https://rosettacode.org/wiki/Unix), FreeBSD, [Linux](https://rosettacode.org/wiki/Linux), [Microsoft](https://rosettacode.org/wiki/Microsoft) [Windows](https://rosettacode.org/wiki/Windows), and [Mac OS X](https://rosettacode.org/wiki/Mac_OS_X).[[2](https://rosettacode.org/wiki/#Citation)]

## How to execute the examples on Rosetta Code
All examples that start with <code>declare</code> can be used directly in the Emacs-based IDE, without a separate compilation step. Just copy the source code to the <code>Oz</code> buffer and select the menu item "Oz&rarr;Feed Buffer".

Some examples are functor definitions and must be compiled. The compiler is invoked with a command such as: <code>ozc -c filename.oz</code>, and then executed with the command, <code>ozengine filename.ozf</code>. This [https://stackoverflow.com/a/29207029/371304 Stack Overflow answer] shows an example of the boilerplate to transform code written for the Emacs IDE to code that can run directly on the Mozart VM.

## Citation
#[https://mozart.github.io/mozart-v1/doc-1.4.0/tutorial/index.html Tutorial of Oz]
#[Wikipedia:Oz (programming language)](https://en.wikipedia.org/wiki/Oz_%28programming_language%29)
