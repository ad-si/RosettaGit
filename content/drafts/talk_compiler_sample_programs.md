+++
title = "Talk:Compiler/Sample programs"
description = ""
date = 2016-11-04T10:27:10Z
aliases = []
[extra]
id = 21191
[taxonomies]
categories = []
tags = []
+++

==99 Bottles sample==
In the 99 bottles of beer sample, the parser output contains a Greater node, I suspect that should be a Greaterequal node to match the >= in the source?


Also, for strict compatibility with the RC 99 Bottles of Beer task :), the assignment:
::bottles = bottles - 1;
should appear before the final:
::print(bottles, " bottles of beer on the wall\n\n");
which would also mean the >= in the source should be > afterall...
--[[User:Tigerofdarkness|Tigerofdarkness]] ([[User talk:Tigerofdarkness|talk]]) 09:12, 4 November 2016 (UTC)

===Great catch!===
This pointed out a bug in the C version.  I've updated the sample program, and will be updating the C Syntax Analyzer.  And I anxiously await your Algol W solutions to the other compiler related tasks!
--[[User:Ed Davis|Ed Davis]] ([[User talk:Ed Davis|talk]]) 10:26, 4 November 2016 (UTC)
