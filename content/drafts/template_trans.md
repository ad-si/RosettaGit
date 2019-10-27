+++
title = "Template:Trans"
description = ""
date = 2017-11-24T07:51:09Z
aliases = []
[extra]
id = 2740
[taxonomies]
categories = []
tags = []
+++

<includeonly><div class="examplemeta translation">'''Translation of''': {{#if:{{{version|}}}|[http://{{SERVERNAME}}/mw/index.php?oldid={{{version}}}#{{{1}}} {{{1}}} (old version)]|{{#if:{{{subpage|}}}|[[../{{{1}}}|{{{1}}}]]|[[#{{{1}}}|{{{1}}}]]}}}}{{#if:{{{2|}}}| &ndash; {{{2}}} }}</div></includeonly><noinclude>

This template marks a programming example as a translation of another one on the same task page.


### Purpose


Sometimes examples are translated directly from other examples by multi-lingual users. This is a template to help people see where examples came from when they were translated directly from other languages. Of course the idea of RC is that all of the examples on a page do exactly the same thing in the same way, but this is not always possible. Examples that differ in this way may still be able to be translated back and forth. Some examples need to use fundamentally different methods to do things where certain things aren't available in a language (GUIs, data structures, etc.) or languages aren't similar in a way that relates to the task. These examples probably cannot be translated between each other.


### Usage


; First argument:
: The other programming language, spelled exactly as its heading is spelled on the same task page. This link will take them to that example.
; Second argument ''(optional):
: Summary of changes, or other remarks about the translation.
; Named arguments ''(optional):
:{|
|-
| <code>subpage</code> &mdash;  || Set to 1 if the examples for this task are on subpages (i.e. pages named ''Taskname/Langname''), so that the link can be adjusted accordingly.
|-
| <code>version</code> &mdash;  || Set to a version number if you want to link to an old version page
|}

{| class="wikitable"
|-
! Example
! Renders as
|-
| <tt><nowiki>{{trans|Python}}</nowiki></tt>
| {{trans|Python}}
|-
| <tt><nowiki>{{trans|Python|without the error handling}}</nowiki></tt>
| {{trans|Python|without the error handling}}
|-
| <tt><nowiki>{{trans|Python|subpage=1}}</nowiki></tt>
| {{trans|Python}}
|-
| <tt><nowiki>{{trans|Haskell|version=256063}}</nowiki></tt>
| {{trans|Haskell|version=256063}}
|}

{{template}}</noinclude>
