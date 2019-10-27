+++
title = "Template:Todo"
description = ""
date = 2016-07-31T11:16:14Z
aliases = []
[extra]
id = 21038
[taxonomies]
categories = []
tags = []
+++

{{alertbox|#f9e5b4|2=<div style="text-align:center">'''TODO:'''</div>
<blockquote style="margin:0;font-size:90%">{{{2|???}}}</blockquote>}}<includeonly>{{#if:{{{1|}}}|[[Category:{{{1}}} examples needing attention]]{{#set:example requires attention={{{1}}} }} }}[[Category:Examples needing attention]]</includeonly><noinclude>

Use this message box template to add a to-do item to an example or section, to be considered by future editors.

To suggest necessary improvements for an example, use [[Template:improve]] instead.

If an example does not meet the task requirements, use [[Template:incorrect]] instead.

Insert this message using the text  <code><nowiki>{{todo|lang|Explanation of the to-do item.}}</nowiki></code>, where ''lang'' is the programming language used.  This will put the page in [[:Category:Examples needing attention]] and [[:Category:Lang examples needing attention]], and apply [[Property:example requires attention]].
{{template}}{{att temp}}</noinclude>
