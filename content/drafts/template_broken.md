+++
title = "Template:Broken"
description = ""
date = 2014-03-22T08:32:12Z
aliases = []
[extra]
id = 1897
[taxonomies]
categories = []
tags = []
+++

{{alertbox|#ffc8c8| This example is '''broken'''.  It fails to compile or causes a runtime error.  Please fix the code and remove this message.}}
<includeonly>
{{#if:{{{1|}}}|[[Category:{{{1}}} examples needing attention]]{{#set:Example requires attention={{{1}}} }} }}
[[Category:Examples needing attention]]
</includeonly>
<noinclude>Insert this message above a programming example using the text <tt><nowiki>{{broken|lang}}</nowiki></tt>, where ''lang'' is the programming language used.  This will put the page in [[:Category:Examples needing attention]] and [[:Category:Lang examples needing attention]] and sets [[Property:Example requires attention]].
{{template}}
[[Category:Example attention templates]]
</noinclude>
