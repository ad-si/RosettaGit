+++
title = "Template:Incorrect"
description = ""
date = 2016-08-08T15:16:08Z
aliases = []
[extra]
id = 1898
[taxonomies]
categories = []
tags = []
+++

{{alertbox|#ffc8c8|2=<div style="text-align:center">This example is '''incorrect'''. {{#if:{{{2|}}}||It does not accomplish the given task.}} Please fix the code and remove this message.</div>{{#if:{{{2|}}}|
<blockquote style="margin:0;font-size:90%">'''''Details:''''' {{{2|}}}</blockquote>}} }}<includeonly>{{#if:{{{1|}}}|[[Category:{{{1}}} examples needing attention]]{{#set:example requires attention={{{1}}} }} }}[[Category:Examples needing attention]]</includeonly><noinclude>

Insert this message above a programming example using the text <tt><nowiki>{{incorrect|lang}}</nowiki></tt> or <tt><nowiki>{{incorrect|lang|Explanation of the problem.}}</nowiki></tt>, where ''lang'' is the programming language used.  This will put the page in [[:Category:Examples needing attention]] and [[:Category:Lang examples needing attention]], and apply [[Property:example requires attention]].

{{template}}{{att temp}}</noinclude>
