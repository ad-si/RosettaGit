+++
title = "Emacs Lisp"
description = ""
date = 2012-07-12T22:46:47Z
aliases = []
[extra]
id = 4016
[taxonomies]
categories = []
tags = []
+++

Emacs Lisp (often abbreviated to elisp) is the [Lisp](https://rosettacode.org/wiki/Lisp) implementation used by the [Emacs](https://rosettacode.org/wiki/Emacs) text editor. It doesn't provide all features of [Common Lisp](https://rosettacode.org/wiki/Common_Lisp) (e.g. its optional arguments always default to nil), but it contains special types which are specific for the use in the editor, like buffers (which is where you actually edit the text), windows, events etc.

[Lisp](https://rosettacode.org/wiki/Lisp) expressions can be executed directly from within Emacs editor buffers, or loaded from files which optionally can be compiled into bytecode.

Emacs configuration files actually are Emacs Lisp source files.

Emacs Lisp can be used as a general programming language with the emacs "-batch" or "--script" command line arguments. [http://www.emacswiki.org/emacs/BatchMode]
