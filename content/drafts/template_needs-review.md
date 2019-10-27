+++
title = "Template:Needs-review"
description = ""
date = 2014-03-21T21:20:00Z
aliases = []
[extra]
id = 1900
[taxonomies]
categories = []
tags = []
+++

{{alertbox|#ffffd8|This example '''may be incorrect'''{{#if:{{{2|}}}|. 
<div class="messagebox-inset" style="border: 1px solid #aaa; background: white; color: black; font-size: 89%; padding: 2px;">{{{2}}}</div><!--  messagebox-usertext is a class I made up; if it becomes real then move the hardwired styling here into a stylesheet.  -->
| due to a recent change in the task requirements or a lack of testing.}} Please verify it and remove this message. If the example does not match the requirements or does not work, replace this message with [[Template:incorrect]] or fix the code yourself.}}<includeonly>{{#if:{{{1|}}}|[[Category:{{{1}}} examples needing attention]]{{#set:Example requires attention={{{1}}} }} }}[[Category:Examples needing attention]]</includeonly><noinclude>

Insert this message above a programming example using the text <tt><nowiki>{{needs-review|lang}}</nowiki></tt>, where ''lang'' is the programming language used. This will put the page in [[:Category:Examples needing attention]] and [[:Category:Lang examples needing attention]] and sets [[Property:Example requires attention]].
</noinclude><noinclude>{{template}}{{att temp}}</noinclude>
