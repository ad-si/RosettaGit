+++
title = "Rosetta Code:Village Pump/Adding transclusion plugin"
description = ""
date = 2015-12-05T06:03:51Z
aliases = []
[extra]
id = 19597
[taxonomies]
categories = []
tags = []
+++

{{Vptopic
|topic=Adding transclusion plugin
|summary=Suggestion to add an existing mediawiki plugin, Transclusion, to address other village pump topics
}}
==Relevant vp-topics==
# [[Rosetta Code:Village Pump/SMW Examples by language and concept]]
# [[Rosetta Code:Village Pump/Language template enhancements]]
# [[Rosetta Code:Village Pump/Download Perl Code]]
==MediaWiki support and documentation==
[[mw:Transclusion|Transclusion]] is the inclusion of content by reference. MediaWiki has a [[mw:Extension:Labeled_Section_Transclusion|plugin]] for it, which would be ready for use in RosettaCode thanks to consistent use of headers per-programming language, through the [[mw:Extension:Labeled_Section_Transclusion#Transcluding_sections_by_headings|header-based transclusion]] command it adds to wiki markup.

The plugin is not currently installed, according to the [[Special:Version]] page.

--[[User:Mcint|Mcint]] ([[User talk:Mcint|talk]]) 05:51, 5 December 2015 (UTC)

=Transclusion scratch space=
It says the function can be called as follows:
 {{#lsth:pagename|sectionX}}

Current guess is that using the <nowiki>{{header|...}}</nowiki> macro in a section name is preventing the title from being recognized to do the transclusion
: The extension "Labeled Section Transclusion" isn't available on Rosetta Code. Thus <code>lsth</code> isn't recognized as a parser hook and the whole <code>{{#...}}</code> part is interpreted as normal text. --[[User:AndiPersti|Andreas Perstinger]] ([[User talk:AndiPersti|talk]]) 09:25, 25 September 2015 (UTC)

==Attempted Usage==
I would like to call:
 <nowiki>
## OCaml
</nowiki>
from the [[Pythagorean triples]] page. Also not working:
 <nowiki>{{#lsth:Pythagorean triples|OCaml}}</nowiki>
{{#lsth:Pythagorean triples|OCaml}}
nor:
 <nowiki>{{#lsth:Pythagorean triples|{{header|OCaml}}}}</nowiki>
{{#lsth:Pythagorean triples|{{header|OCaml}}}}
