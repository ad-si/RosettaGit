+++
title = "Talk:Go 1"
description = ""
date = 2014-09-03T02:40:37Z
aliases = []
[extra]
id = 11607
[taxonomies]
categories = []
tags = []
+++

Is this to be considered an implementation or a revision of the language itself? If the former, mark with {{tmpl|implementation}} (of Go) please. :-) –[[User:Dkf|Donal Fellows]] 07:53, 29 March 2012 (UTC)
:Done.  Of course it's both a version and a revision, but I guess it's most significantly an implementation.  (Other groups have worked on alternative implementations.) &mdash;[[User:Sonia|Sonia]] 19:14, 29 March 2012 (UTC)
:This might be similar to Java and Java 1.5. 1.5 added new syntax and new capabilities, but it was backward compatible with code from older versions of Java. It may also be like [[Perl]] and [[Perl 6]] where the new version is so different that it should be considered its own language. I think it really depends on what the general language community thinks. --[[User:Mwn3d|Mwn3d]] 19:17, 29 March 2012 (UTC)

:I don't think this should page should exist. [http://golang.org/doc/faq#Implementation Go implementations] include the original <tt>gc</tt> one and the <tt>[http://golang.org/doc/install/gccgo gccgo]</tt> one and those pages should be created. All Go 1.x versions will be backwards compatible with Go 1.0. Pre-1.0 Go code is not used in practice and is only of historical interest IMO. Specific Go examples could note when they're using a "new" language feature but personally I'd only do this if it's something specific to the task at hand (e.g. the task requires something not availble in an earlier language version). This would likely match how other languages do it (e.g. C examples tend to only mention C89, C99, etc when specifically relevant and any given example may silently require the latest, and C89, C99, etc are not listed as [[:Category:C Implementations]]). &mdash;[[User:Dchapes|Dchapes]] ([[User talk:Dchapes|talk]]) 18:38, 1 September 2014 (UTC)

:: I'll second all that. &mdash;[[User:Sonia|Sonia]] ([[User talk:Sonia|talk]]) 02:40, 3 September 2014 (UTC)
