+++
title = "TXR"
description = ""
date = 2016-10-06T05:16:59Z
aliases = []
[extra]
id = 10531
[taxonomies]
categories = []
tags = []
+++
TXR is a new language implemented in [C](https://rosettacode.org/wiki/C), running on POSIX platforms such as [Linux](https://rosettacode.org/wiki/Linux), [Mac OS X](https://rosettacode.org/wiki/Mac_OS_X) and [Solaris](https://rosettacode.org/wiki/Solaris) as well as on [Microsoft Windows](https://rosettacode.org/wiki/Microsoft_Windows). It is a dynamic, high level language originally intended for "data munging" tasks in Unix-like environments, particularly tasks requiring accurate, robust text scraping from loosely structured documents.

The Rosetta Code TXR solutions can be viewed in color, and all on one page with a convenient navigation pane [http://www.nongnu.org/txr/rosetta-solutions.html here].

TXR started as a language for "reversing here-documents": evaluating a template of text containing variables, plus useful pattern matching directives, against some body of text and binding pieces of the text which matches variables. The variable bindings were output in POSIX shell variable assignment syntax, allowing for shell code like

<code>eval $(txr <txr-program> <args> ...)</code>

TXR was internally based, from the beginning, on a data model based on Lisp and eventually exposed a Lisp dialect that came to be known as TXR Lisp. TXR Lisp at first complemented the pattern extraction language, extending its power, but eventually became distinct. Programs can be written in TXR Lisp with no traces of the TXR pattern language, or vice versa.

TXR Lisp is an original dialect that contains many innovative features, which orchestrate together to express neat, compact solutions to everyday data processing problems. Programmers familiar with Common Lisp will be comfortable with TXR Lisp, and there is much to like for those who use Scheme, Racket or Clojure.  TXR Lisp incorporates ideas from contemporary scripting languages also; a key motivation in many of its developments is the promotion of succinctness, which is something that often isn't associated with languages in the Lisp family.
