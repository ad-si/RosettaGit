+++
title = "Template:Untested"
description = ""
date = 2014-03-22T08:55:10Z
aliases = []
[extra]
id = 5011
[taxonomies]
categories = []
tags = []
+++

{{alertbox|#ffffd8|This example is '''untested'''. Please check that it's correct, debug it as necessary, and remove this message.}}
<includeonly>
{{#if:{{{1|}}}|[[Category:{{{1}}} examples needing attention]]{{#set:Example requires attention={{{1}}} }} }}
[[Category:Examples needing attention]]
</includeonly>
<noinclude>Insert this message above a programming example using the text <tt><nowiki>{{untested|lang}}</nowiki></tt>, where ''lang'' is the programming language used. This will put the page in [[:Category:Examples needing attention]] and [[:Category:Lang examples needing attention]], and sets [[Property:Example requires attention]].
{{template}}
{{att temp}}
</noinclude>
