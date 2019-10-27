+++
title = "Odd word problem/TrivialCharacterStreamSupportForJ"
description = ""
date = 2012-08-07T13:08:41Z
aliases = []
[extra]
id = 10818
[taxonomies]
categories = []
tags = []
+++

[[../#J|J]] lacks a character stream implementation, so this is a minimal sketch of "get a character" and "put a character" code.

''One issue, here, is that all of J's primitives are designed for dealing with sequences.''

''Also, the language itself is designed to amortize interpreter overhead when the primitives deal directly with sequences rather than factoring so that primitives deal with [for example] a single character at a time.  In other words, the language is radically more efficient on algorithms which treat sequences regularly than it is on algorithms which micro-manage data handling.  This is in some ways analogous to the relationship between context dependent and context free languages, except that it's in the language's semantics rather than in the language's syntax.'''

''So it does not make much sense to implement some specialized interface for working with one character at a time.''

Note that this implementation allows only one input stream and one output stream per locale (a J locale is roughly equivalent to a "namespace", or a "class" or an "object" or perhaps even a "stack frame (a call/cc context)" in another language)..


```j
begin_instream=: 3 :0  NB.
  instream=: y
    last=: _1
)
 
  getch=: 3 :0
    last=: last+1
    last{instream
  )
 
 
clear_outstream=: 3 :0
  outstream=: ''
)
 
  outch=: 3 :0
    outstream=: outstream, y
  )
```

