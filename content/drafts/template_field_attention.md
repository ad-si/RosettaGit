+++
title = "Template:Field attention"
description = ""
date = 2014-03-21T21:05:00Z
aliases = []
[extra]
id = 4742
[taxonomies]
categories = []
tags = []
+++

{{alertbox|#ffffd8|This example needs review by someone who knows about: [[:Category:Field knowledge needed/{{{2}}}|'''{{{2}}}''']] {{#if:{{{3|}}}|

<div class="messagebox-inset" style="border: 1px solid #aaa; background: white; color: black; font-size: 89%; padding: 2px;">{{{3}}}</div><!--  messagebox-usertext is a class I made up; if it becomes real then move the hardwired styling here into a stylesheet.  -->
}}
If you know {{{2}}}, please review this example and, as necessary, improve it or describe what should be done for someone who knows the language.<includeonly>{{#if:{{{1|}}}|[[Category:{{{1}}} examples needing attention]]{{#set:Example requires attention={{{1}}} }} }}[[Category:Examples needing attention]][[Category:Field knowledge needed/{{{2}}}]][[Category:Field knowledge needed/{{{2}}}/{{{1}}}]]</includeonly>}}<noinclude>

Insert this message above a programming example using the text  <code><nowiki>{{field attention|</nowiki>''lang''<nowiki>|</nowiki>''field''<nowiki>|Explanation of the problem.}}</nowiki></code>, where ''lang'' is the category name of the language and ''field'' is a short, consistent name for the area of expertise needed. This will put the page in [[:Category:Examples needing attention]], Category:''Lang'' examples needing attention, Category:Field knowledge needed/''field'', and Category:Field knowledge needed/''field''/''lang''. Additionally [[Property:Example requires attention]] is set.

{{template}}{{att temp}}</noinclude>
