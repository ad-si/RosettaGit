+++
title = "Template:Incomplete"
description = ""
date = 2014-03-22T08:16:07Z
aliases = []
[extra]
id = 1976
[taxonomies]
categories = []
tags = []
+++

{{alertbox|#ffc8c8|This example is '''incomplete'''. {{#if:{{{2|}}}|{{{2}}}|}} Please ensure that it meets all task requirements and remove this message.}}<includeonly>{{#if:{{{1|}}}|[[Category:{{{1}}} examples needing attention]]{{#set:Example requires attention={{{1}}} }} }}[[Category:Examples needing attention]]</includeonly><noinclude>

Insert this message above a programming example using the text <tt><nowiki>{{incomplete|lang|reason}}</nowiki></tt>, where ''lang'' is the programming language used and ''reason'' an optional description of what is missing. This will put the page in [[:Category:Examples needing attention]] and [[:Category:Lang examples needing attention]] and sets [[Property:Example requires attention]].

{{template}}{{att temp}}</noinclude>
