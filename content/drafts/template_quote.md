+++
title = "Template:Quote"
description = ""
date = 2016-08-20T15:00:18Z
aliases = []
[extra]
id = 21066
[taxonomies]
categories = []
tags = []
+++

<includeonly><div style="margin: 1px 0 0.7em 0.2em; padding:0.5em 1em; border-left:solid 0.4em #dddddd; background:#f9f9f9">{{{1}}}</div>{{#if:{{{2|}}}|
<div style="margin:-0.7em 0.5em 0.7em 2em; font-style:italic; font-size:90%; text-align:right">&mdash; {{{2|}}}</div>
|}}</includeonly><noinclude>

This template can be used to make a quote stand out a little.

;Usage:

: <code><nowiki>{{quote|text of the quote}}</nowiki></code>

: <code><nowiki>{{quote|text of the quote|source of the quote}}</nowiki></code>

; Example:


```txt

{{quote
 | What is the smallest positive integer whose square ends in the digits 269,696?
 | Babbage, letter to Lord Bowden, 1837; see Hollingdale and Tootill, <i>Electronic Computers</i>, second edition, 1970, p. 125.
}}
```


...renders as:
{{quote|What is the smallest positive integer whose square ends in the digits 269,696?|Babbage, letter to Lord Bowden, 1837; see Hollingdale and Tootill, <i>Electronic Computers</i>, second edition, 1970, p. 125.}}

{{template}}</noinclude>
