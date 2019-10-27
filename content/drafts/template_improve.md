+++
title = "Template:Improve"
description = ""
date = 2014-03-19T21:04:56Z
aliases = []
[extra]
id = 4734
[taxonomies]
categories = []
tags = []
+++

{{alertbox|lightgray|{{#if:{{{2|}}}|<div style="text-align:center">This example is '''in need of improvement:'''</div>
<blockquote>{{{2}}}</blockquote>|<div style="text-align:center">This example is '''in need of improvement'''.</div>}} }}<includeonly>{{#if:{{{1|}}}|[[Category:{{{1}}} examples needing attention]]{{#set:example requires attention={{{1}}} }} }}[[Category:Examples needing attention]]</includeonly><noinclude>

Use this message box template when an example '''meets the task requirements''', but could be improved in some way. If the example does not meet the task requirements, use [[Template:incorrect]] instead.

Insert this message above a programming example using the text  <code><nowiki>{{improve|lang|Explanation of the problem.}}</nowiki></code>, where ''lang'' is the programming language used.  This will put the page in [[:Category:Examples needing attention]] and [[:Category:Lang examples needing attention]], and apply [[Property:example requires attention]].
{{template}}{{att temp}}</noinclude>
