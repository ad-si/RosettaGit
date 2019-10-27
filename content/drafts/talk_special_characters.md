+++
title = "Talk:Special characters"
description = ""
date = 2011-12-08T18:39:10Z
aliases = []
[extra]
id = 4710
[taxonomies]
categories = []
tags = []
+++

== Clarification? ==

It is not really clear what is asked of by this task, especially the "special characters" part. Most of the examples currently list the character escape sequences used in character and string literals in the languages, fulfilling the "escape sequences" part. But some of the examples also go beyond and list all the symbols used in the language and how they are used in the syntax and stuff; do we really want that? That part seems unrelated to the escape sequences part. --[[Special:Contributions/76.173.203.32|76.173.203.32]] 09:25, 11 August 2009 (UTC)

: The task says: List the special characters and escape sequences. Likely we can define special characters as those that can't be used in symbols (like variables or functions names) and have a "special" built-in meaning in the language. Escape sequences are those used in string literals '''and''' to use special characters like normal symbol, when this can be done (like in TeX). --[[User:ShinTakezou|ShinTakezou]] 11:20, 11 August 2009 (UTC)

== The Perl answer ==
I'm not sure about the Perl-Part for several reasons:
* it is very long, mainly because it list many (not all) operators the language has.
* The description of operators lack the meaning of some operators in list context (which would make the list even longer)
* There is nothing said about the sigils ($%&*), which are really special characters in that language.
* are m, s, y, tr, qr, qw, q and qq special charakters? i think, they are, more than the other operators.
* same goes for // and ??;
* Heredoc begin?
* the all global variables? like %( which leads to more than just confusion to most Perl-Programmers, and is used mostly for obfuscation.
Same note as above, What are special charakters? is ~ a special charackter, because it's an undocumented operator? [[User:Patrick|Patrick]] 18:39, 8 December 2011 (UTC)
