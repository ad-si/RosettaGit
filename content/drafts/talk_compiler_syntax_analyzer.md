+++
title = "Talk:Compiler/syntax analyzer"
description = ""
date = 2019-04-22T18:20:20Z
aliases = []
[extra]
id = 21172
[taxonomies]
categories = []
tags = []
+++

==Unary operator precedence==
In the grmmmar, the definition of primary is:

```txt

    primary             =   Identifier
                          | Integer
                          | '(' expr ')'
                          | ('+' | '-' | '!') expr
                          ;

```

which makes the unary operators the lowest precedence? Should that be:

```txt

    primary             =   Identifier
                          | Integer
                          | '(' expr ')'
                          | ('+' | '-' | '!') primary
                          ;

```

which would be more usual?
If I read it correctly, the C reference implementation does treat unary operators as highest precedence.
--[[User:Tigerofdarkness|Tigerofdarkness]] ([[User talk:Tigerofdarkness|talk]]) 08:46, 23 October 2016 (UTC)

You are correct.  Thanks for catching this! I have modified the grammar. --[[User:Ed Davis|Ed Davis]] ([[User talk:Ed Davis|talk]]) 11:03, 23 October 2016 (UTC)

==prt_list production incorrect==
I think the prt_list production should be:

```txt

prt_list            =   (String | expr) { ',' (String | expr) } ;

```

because | is lower precedence than concatenation.

Yep, I agree.  It has been corrected.  Thanks for the catch! --[[User:Ed Davis|Ed Davis]] ([[User talk:Ed Davis|talk]]) 18:19, 22 April 2019 (UTC)
