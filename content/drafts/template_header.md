+++
title = "Template:Header"
description = ""
date = 2016-12-18T17:46:25Z
aliases = []
[extra]
id = 2133
[taxonomies]
categories = []
tags = []
+++

<includeonly>{{#switch: {{lc: {{{1|}}} }}
| c sharp
| c# = [[:Category:C sharp|C#]] [[Category:C sharp]] {{#set:Implemented in language=C sharp}}
| f sharp
| f_sharp
| f# = [[:Category:F Sharp|F#]] [[Category:F Sharp]] {{#set:Implemented in language=F Sharp}}
| javascript = [[:Category:JavaScript|JavaScript]] [[Category:JavaScript]] {{#set:Implemented in language=JavaScript}}
| tex
| plaintex = [[:Category:PlainTeX|{{PlainTeX}}]] [[Category:PlainTeX]] {{#set:Implemented in language=PlainTeX}}
| latex = [[:Category:LaTeX|{{LaTeX}}]] [[Category:LaTeX]] {{#set:Implemented in language=LaTeX}}
| #default = [[:Category:{{{1}}}|{{{1}}}]] [[Category:{{{1}}}]] {{#set:Implemented in language={{{1}}} }}
}}</includeonly><noinclude>Usage: <code><nowiki>
## language name
</nowiki></code>

This template is intended to be used for the header of a programming language solution. The language's category is added to the task page and the Semantic MediaWiki property ''Implemented in language'' is set.

For the following languages, any of the listed spellings is ok:
* '''C#''': ''C#'', ''C sharp'' or ''C Sharp''
* '''F#''': ''F#'', ''F sharp'', ''F Sharp'', ''F_sharp'' or ''F_Sharp''
* '''JavaScript''': ''JavaScript'' or ''Javascript''
* '''{{PlainTeX}}''': ''PlainTeX'' or ''plaintex'' or ''TeX'' or ''tex''
* '''{{LaTeX}}''': ''LaTeX'' or ''latex''
{{template}}
[[Category:Example description templates]]</noinclude>
