+++
title = "Template:Novice example"
description = ""
date = 2014-03-22T08:43:10Z
aliases = []
[extra]
id = 5251
[taxonomies]
categories = []
tags = []
+++

{{alertbox|#ffffd8"|This example was written by a novice in '''{{{1}}}'''. If you are familiar with {{{1}}}, please review and edit this example and remove this message. If the example does not work and you cannot fix it, replace this message with <tt><nowiki>{{</nowiki>[[Template:incorrect|incorrect]]<nowiki>|</nowiki>{{{1}}}<nowiki>|</nowiki>''description of problem as you see it''<nowiki>}}</nowiki></tt>. If the code is correct but unidiomatic and you cannot fix it, replace this message with <tt><nowiki>{{</nowiki>[[Template:improve|improve]]<nowiki>|</nowiki>{{{1}}}<nowiki>|</nowiki>''description of how it should be improved''<nowiki>}}</nowiki></tt>.}}
<includeonly>
{{#if:{{{1|}}}|[[Category:{{{1}}} examples needing attention]]{{#set:Example requires attention={{{1}}} }} }}
[[Category:Examples needing attention]]
</includeonly>
<noinclude>Insert this message above a programming example using the text <tt><nowiki>{{novice example|</nowiki>''lang''<nowiki>}}</nowiki></tt>, where ''lang'' is the programming language used. This will put the page in [[:Category:Examples needing attention]] and Category:''Lang'' examples needing attention, and sets [[Property:Example requires attention]].
{{template}}
{{att temp}}
</noinclude>
