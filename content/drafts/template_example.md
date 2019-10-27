+++
title = "Template:Example"
description = ""
date = 2012-02-02T21:42:01Z
aliases = []
[extra]
id = 8040
[taxonomies]
categories = []
tags = []
+++

{{#if: {{{langpage|}}} | <span id="{{{language}}}">[[:Category:{{{language}}}|{{{langpage}}}]]</span> | [[:Category:{{{language}}}|{{{language}}}]] }}{{#declare:example of=task|implemented in language=language}}<noinclude><br/>
Sets up an example page
{|
!param name
!meaning
|-
|task
|The name of the task this is an example of
|-
|language
|The name of the language this example is implemented in
|-
|langpage
|(optional) The pagename of the language, if different (for example: C_sharp for C#)
|}

For example, when adding an example of the [[Hough transform]] implemented in C++, you would put this at the top of your example page:
<nowiki>=={{example|task=Hough transform|language=C++}}==</nowiki>

If you are adding an example in a language that needs a special page title, such as C#, you would put this at the top of your example page:
<nowiki>=={{example|task=Hough transform|language=C#|langpage=C_sharp}}==</nowiki>

Unfortunately, the header markup needs to be in the page that uses the template, rather than the template itself. Otherwise, you won't see edit links.

{{template}}</noinclude>
