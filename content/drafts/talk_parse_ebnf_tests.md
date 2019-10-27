+++
title = "Talk:Parse EBNF/Tests"
description = ""
date = 2011-09-12T00:41:41Z
aliases = []
[extra]
id = 10486
[taxonomies]
categories = []
tags = []
+++

Why the example provided at the bottom of the page

{ foo = bar . }

is not a valid EBNF grammar, according to the accepted definition?

: I guess because it never defines a production for ''bar''. --[[User:Kernigh|Kernigh]] 01:51, 9 September 2011 (UTC)

:: Then we have a contradiction - grammar { foo = bar . } is correct if definition on the linked page is used (and at least one implementation verifies that), together with restrictions on identifier and literal allowed characters, but is not correct, if we assume that all identifiers should be defined.

:: I'd lift the requirement that all identifiers should be defined, and stick to the provided definitions. Then this example grammar should be in the "correct" section.
