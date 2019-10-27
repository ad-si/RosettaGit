+++
title = "Template:Update"
description = ""
date = 2016-08-07T12:54:13Z
aliases = []
[extra]
id = 13483
[taxonomies]
categories = []
tags = []
+++

{{alertbox|#ffc8c8|2=<div style="text-align:center">This example needs '''updating''' due to a modification in the task. Please examine and fix the code if needed, then remove this message.</div>{{#if:{{{2|}}}|
<blockquote style="margin:0;font-size:90%">'''''Details:''''' {{{2|}}}</blockquote>}} }}<includeonly>{{#if:{{{1|}}}|[[Category:{{{1}}} examples needing attention]]{{#set:example requires attention={{{1}}} }} }}[[Category:Examples needing attention]]</includeonly><noinclude>
Insert this message above a programming example using the text <tt><nowiki>{{update|Lang}}</nowiki></tt> or <tt><nowiki>{{update|Lang|Explanation of the problem.}}</nowiki></tt>, where ''Lang'' is the programming language used. 

This will put the page in [[:Category:Examples needing attention]] and [[:Category:Lang examples needing attention]], and apply [[Property:example requires attention]].

{{template}}{{att temp}}</noinclude>
