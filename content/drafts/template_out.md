+++
title = "Template:Out"
description = ""
date = 2016-08-27T10:34:16Z
aliases = []
[extra]
id = 11399
[taxonomies]
categories = []
tags = []
+++

<includeonly><div>
;{{#if: {{{1|}}} | {{{1}}} | Output}}{{#if: {{{note|}}} | <i style="font-weight:normal> ({{{note}}})</i> |}}{{#if: {{{case|}}}{{{input|}}} | <span style="font-weight:normal>  &mdash;  {{{case|for input <code>{{{input|}}}</code>}}}</span> |}}{{#if: {{{text|}}} | <nowiki/> <span style="font-weight:normal>{{{text}}}</span> | <nowiki>:</nowiki>}}</div></includeonly><noinclude>

This template is used to introduce the output of a programming example.


### Usage


Write <code><nowiki>{{out}}</nowiki></code> just above the <code><nowiki>
```txt
...
```
</nowiki></code> block that lists a code example's output.

It also accepts these optional parameters:

{| class="wikitable"
|-
! Parameter
! Purpose
! Example
! Renders as
|-
| 
| 
| <code><nowiki>{{out}}</nowiki></code>
| {{out}}
|-
| <code>1</code>
| Text to show instead of the word "Output".
| <code><nowiki>{{out|Return value}}</nowiki></code>
| <div style="white-space:nowrap">{{out|Return value}}</div>
|-
| <code>case</code>
| The test-case/condition which produces this output.
| <code><nowiki>{{out|case=test case 3}}</nowiki></code>
| <div style="white-space:nowrap">{{out|case=test case 3}}</div>
|-
| <code>input</code>
| The (single-line) input which produces this output.
| <code><nowiki>{{out|input=Hello world}}</nowiki></code>
| <div style="white-space:nowrap">{{out|input=Hello world}}</div>
|-
| <code>note</code>
| A remark to show in parentheses.
| <code><nowiki>{{out|note=elided}}</nowiki></code>
| <div style="white-space:nowrap">{{out|note=elided}}</div>
|-
| <code>text</code>
| Free-form text to follow instead of a colon.
| <code><nowiki>{{out|text=...}}</nowiki></code>
| <div style="white-space:nowrap">{{out|text=...}}</div>
|}

You can combine multiple parameters, except that <tt>case</tt> and <tt>input</tt> are mutually exclusive.

{{template}}</noinclude>
