+++
title = "Template:Output?/Beta"
description = ""
date = 2011-08-16T21:41:32Z
aliases = []
[extra]
id = 10322
[taxonomies]
categories = []
tags = []
+++

{{alertbox|lightgray|This task requires an examples output to be not only generated, but also displayed '''on this page''' (or a page linked to from here). {{#if:{{{2|}}}|{{{2}}}|}} 
Please copy and paste the output under the example and remove this message. 
''<small>Note: Output which is very long may be shortened or placed on its own subpage of this task (e.g. [[{{PAGENAME}}/{{{1}}}/Output]]). See {{tmpl|Output?}} for more information.</small>''}}<includeonly>{{#if:{{{1|}}}|[[Category:{{{1}}} examples needing attention]]}}[[Category:Examples needing attention]]</includeonly><noinclude>

This template is not necessarily an indication that the programming example is incorrect. Tasks which use words like "print", "show", and "display" are usually asking for textual output. It is good practice to show the output of a program whenever there is output. Sometimes it's required by the task as easy proof that the example works (that way people don't necessarily have to set up their system to run the example to see that it's correct).

Insert this message above a programming example using the text <tt><nowiki>{{output?|lang}}</nowiki></tt>, where ''lang'' is the programming language used. This will put the page in [[:Category:Examples needing attention]] and [[:Category:Lang examples needing attention]]. There is also an optional second argument which you can use to give additional reasons for using this template. The text in the second argument will appear in the box after the first sentence.</noinclude>
