+++
title = "Template:Output?"
description = ""
date = 2014-03-22T08:54:58Z
aliases = []
[extra]
id = 9568
[taxonomies]
categories = []
tags = []
+++

{{alertbox|#ffc8c8|This example does not show the output mentioned in the task description '''on this page''' (or a page linked to from here). {{#if:{{{2|}}}|{{{2}}}|}} Please ensure that it meets all task requirements and remove this message. 
<small>Note that phrases in task descriptions such as ''"print and display"'' and ''"print and show"'' for example, indicate that (reasonable length) output be a part of a language's solution.</small>}}
<includeonly>
{{#if:{{{1|}}}|[[Category:{{{1}}} examples needing attention]]{{#set:Example requires attention={{{1}}} }} }}
[[Category:Examples needing attention]]
</includeonly>
<noinclude>Insert this message above a programming example using the text <tt><nowiki>{{output?|lang|reason}}</nowiki></tt>, where ''lang'' is the programming language used and ''reason'' an optional description of the problem. This will put the page in [[:Category:Examples needing attention]] and [[:Category:Lang examples needing attention]], and sets [[Property:Example requires attention]].
{{template}}
{{att temp}}
</noinclude>
