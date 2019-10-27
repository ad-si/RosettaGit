+++
title = "Template:Uses from"
description = ""
date = 2010-11-20T17:11:03Z
aliases = []
[extra]
id = 8706
[taxonomies]
categories = []
tags = []
+++

<div class="examplemeta libheader"><noinclude>'''Uses:''' (tool name) <small> ('''Component[s]:''' Component1, Component2, ComponentN )</small></noinclude><includeonly>'''Uses:''' [[SMW::off]]<!--

-->[[uses::{{{1}}}/{{{2}}}|{{{2}}}]]<!--
-->{{#set:Uses {{{1}}}={{{1}}}/{{{2}}}}}<!--

-->[[Category:{{{2}}}]]<!--

There must be a component N=1, for there to be a component N>1. If there's no component N=1, then there's no reason to put any of the component list artifacts in.
-->{{#if: {{{component1|}}}|<!--

--> <small> (<!--

Is there more than one component?
-->{{#if: {{{component2}}}|<!--
Plural
-->'''Components:''' <!--
-->|<!--
Singular
-->'''Component:''' <!--
End plural/singluar #if.
-->}}<!--

-->{{#foreach: component$n$|<!--

-->[[SMW::on]]<!--

-->[[Uses {{{1}}}::{{{1}}}/{{{2}}}/{{{component$n$}}}|{{{component$n$}}}]], <!--

Ending #foreach
-->}}<!--

Ending #if componentN
-->}}<!--

Only apply </small> if we have components.
-->{{#if: {{{component1|}}}|<!--
--> )</small> <!--
-->}}<!--

Ending examplemeta div.
--></includeonly></div><!--

--><noinclude>

This template is intended to handle the case where a {{{1}}} has many components within it (examples of this include CPAN, RubyGems and Tcllib). It has two required parameters: <tt><nowiki>{{</nowiki>uses from|</tt>''{{{1}}}''<tt>|</tt>''component''<tt><nowiki>}}</nowiki></tt>. The ''{{{1}}}'' is the name of the overall {{{1}}} of components (which should also be the name of a category here on RC) and the ''component'' is the name of the component within it (which can be almost any string).

==Discussion==
This template is derived from {{tmpl|tcllib}}, and seeks to have a similar aim, except more generically, and as a potential eventual replacement for {{tmpl|libheader}} and {{tmpl|works with}}. (And {{tmpl|tcllib}}, if it achieves sufficient functionality.)


### What it does

* Associates the page with the {{{1}}}:
** Semantically, as <tt><nowiki>[[Uses {{{1}}}::{{{2}}}]</nowiki></tt>
** As part of <tt>[[:Category:{{{2}}}]]</tt> (though this may eventually change)
** By linking to <tt>[[{{{1}}}/{{{2}}}]]</tt>.
* Associates the page with each component specified
** Semantically, as <tt><nowiki>[[Uses {{{1}}}::{{{2}}}/{{{componentN}}}]]</nowiki></tt> (Placing the component as a subpage of the {{{1}}}
** By linking to <tt><nowiki>[[{{{1}}}/{{{2}}}/{{{componentN}}}]]</nowiki></tt>.

===What it needs to do (eventually)===
* Associate {{{1}}} version information with the page.
* Associate component version information with the page.


### What it should do

* Avoid using MediaWiki categories as possible.

The richer the relationships we can define, the more powerful the search options we'll have available.

{{template}}[[Category:Example description templates]]</noinclude>
