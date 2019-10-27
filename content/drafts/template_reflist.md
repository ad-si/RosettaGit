+++
title = "Template:Reflist"
description = ""
date = 2010-04-25T16:42:35Z
aliases = []
[extra]
id = 7114
[taxonomies]
categories = []
tags = []
+++

<noinclude>Taken from the reflist template from [http://en.wikipedia.org/wiki/Template:Reflist Wikipeida]</noinclude>
<div class="references-small {{#if: {{{colwidth|}}} | references-column-width | {{#iferror: {{#ifexpr: {{{1|1}}}>1 | references-column-count references-column-count-{{{1}}} }} }} }}" {{#if: {{{colwidth|}}}| style="-moz-column-width:{{{colwidth}}}; column-width:{{{colwidth}}};" | {{#if: {{{1|}}}| style="-moz-column-count:{{{1}}}; column-count:{{{1}}};" }} }}>
{{#tag:references|{{{refs|}}}|group={{{group|}}}}}</div><noinclude>
{{documentation}}</noinclude>
