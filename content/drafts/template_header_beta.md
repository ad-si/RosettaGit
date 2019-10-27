+++
title = "Template:Header beta"
description = ""
date = 2011-07-22T23:37:30Z
aliases = []
[extra]
id = 10011
[taxonomies]
categories = []
tags = []
+++

{{#if: {{{2|}}} | <span id="{{{1}}}">[[:Category:{{{1}}}|{{{2}}}]]</span>|{{#switch: {{lc: {{{1|}}} }}|c sharp=<span id="C sharp">[[:Category:C sharp|C#]]</span>|f sharp=<span id="F sharp">[[:Category:F sharp|F#]]</span>|[[:Category:{{{1}}}|{{{1}}}]]}} }}[[Category:{{{1}}}]]{{#set:implemented in language={{{1}}} }}<noinclude>

This is intended to be used for the header of a programming language solution. Used within the header, it adds a category with the same name. It can be invoked two ways:

<code><nowiki>
 
## Your Language Here

</nowiki></code>

expands to:
<code><nowiki>
==[[Your Language Here]] [[Category:Your Language Here]]==
</nowiki></code>

To catch difficult wiki names like C#:

<code><nowiki>
 =={{header|Your Language Here|Display Name}}==
</nowiki></code>

expands to:
<code><nowiki>
==[[Your Language Here|Display Name]] [[Category:Your Language Here]]==
</nowiki></code>
</noinclude>
