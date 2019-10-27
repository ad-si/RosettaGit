+++
title = "Help talk:Adding a new programming example"
description = ""
date = 2008-12-05T13:08:15Z
aliases = []
[extra]
id = 1963
[taxonomies]
categories = []
tags = []
+++


### Syntax Highlighting 

I tried adding syntax coloring to Ada programs. The results were very disappointing. Some of the reserved words were highlighted. Some were not. All indentation was lost. Am I required to put the highlighting tags around each line individually, or should I be able to apply syntax highlighting to a whole block of text? -- Waldorf
:Yeah, I'm not particularly impressed with the highlighter.  It seems to give inconsistent results...It definitely needs work.  You can put the <nowikie><highightSyntax></nowiki> tags around blocks of text. --[[User:Short Circuit|Short Circuit]] 07:13, 21 February 2007 (EST)
:
:The syntax highlighter needs a ''lot'' of work. I'm going to have to contact the author of the MediaWiki extension and the PHP beautifier package, to see if either of them know what can be done. --[[User:Short Circuit|Short Circuit]] 08:12, 21 February 2007 (EST)
:Still needing, and it should be made clearer how to insert highlighted code. Here you say to use the &lt;code lang="LANG"&gt; syntax, but often the &lt;LANG&gt; syntax is used... and when a language is not supportend, odd things happen (fixed here and there by enclosing the code with the pre tag) &mdash; [[User:ShinTakezou|ShinTakezou]] 13:08, 5 December 2008 (UTC)
