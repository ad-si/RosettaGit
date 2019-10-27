+++
title = "Template:Language"
description = ""
date = 2011-09-29T20:50:06Z
aliases = []
[extra]
id = 2319
[taxonomies]
categories = []
tags = []
+++

{{infobox_begin}}{{Language/Icon}}'''{{PAGENAME}}'''<br/> This '''programming language''' may be used to instruct a computer to perform a task.
{{#if:{{{site|}}}|{{{!}}style="text-align: left; font-size: 75%; background: transparent;"
{{!}}-
![{{{site}}} Official website]
{{!}}}
}}
{|style="text-align: left; font-size: 75%; background: transparent;"
{{#if: {{{exec|}}}|{{!}}-
![[:Category:Execution method|Execution method]]:
{{!}}{{#switch: {{lc:{{{exec|}}}}}|machine = Compiled (machine code) [[Category:Execution method/Compiled/Machine code]]|interpreted = Interpreted [[Category:Execution method/Interpreted]]|bytecode = Compiled (bytecode) [[Category:Execution method/Compiled/Bytecode]]|both = Interpreted or compiled [[Category:Execution method/Interpreted]][[Category:Execution method/Compiled]]}}
{{!}}-
}}
{{#if: {{{gc|}}}|![[:Category:Garbage collection|Garbage collected]]:
{{!}}{{ucfirst:{{{gc|}}}}}[[Category:Garbage collection/{{ucfirst:{{{gc}}}}}]]
{{!}}-
}}
{{#if: {{{parampass|}}}|![[:Category:Parameter passing|Parameter passing methods]]:
{{!}}{{#ifeq: {{{parampass}}}|both| By reference, By value[[Category:Parameter passing/By reference]][[Category:Parameter passing/By value]]|
 By {{lc:{{{parampass}}}}}[[Category:Parameter passing/By {{lc:{{{parampass}}}}}]]}}
{{!}}-
}}
{{#if: {{{safety|}}}|![[:Category:Typing|Type safety]]:
{{!}}{{#ifeq: {{{safety}}}|both|Safe, Unsafe[[Category:Typing/Safe]][[Category:Typing/Unsafe]]|{{ucfirst:{{{safety}}}}}
[[Category:Typing/{{ucfirst:{{{safety}}}}}]]}}
{{!}}-
}}
{{#if: {{{strength|}}}|![[:Category:Typing|Type strength]]:
{{!}}{{ucfirst:{{{strength}}}}}
[[Category:Typing/{{ucfirst:{{{strength}}}}}]]
{{!}}-
}}
{{#if: {{{compat|}}}|![[:Category:Typing/Compatibility|Type compatibility]]:
{{!}}{{#ifeq: {{{compat}}}|both|Nominative, Structural[[Category:Typing/Compatibility/Nominative]][[Category:Typing/Compatibility/Structural]]|{{ucfirst:{{{compat}}}}}
[[Category:Typing/Compatibility/{{ucfirst:{{{compat}}}}}]]}}
{{!}}-
}}
{{#if: {{{express|}}}|![[:Category:Typing/Expression|Type expression]]:
{{!}}{{#ifeq: {{{express}}}|both|Implicit, Explicit[[Category:Typing/Expression/Implicit]][[Category:Typing/Expression/Explicit]]|{{ucfirst:{{{express}}}}}
[[Category:Typing/Expression/{{ucfirst:{{{express}}}}}]]}}
{{!}}-
}}
{{#if: {{{checking|}}}|![[:Category:Typing/Checking|Type checking]]:
{{!}}{{#ifeq: {{{checking}}}|both|Dynamic, Static[[Category:Typing/Checking/Dynamic]][[Category:Typing/Checking/Static]]|
{{ucfirst:{{{checking|}}}}}[[Category:Typing/Checking/{{ucfirst:{{{checking}}}}}]]}}
{{!}}-
}}
{{#if: {{{untyped|}}}|!{{#ifeq: {{{untyped}}}|yes|Typing:
{{!}}Untyped[[Category:Typing/Untyped]]}}
{{!}}-
}}
{{#if: {{{tags|}}}|![[Help:Syntax_Highlighting|Lang tag(s)]]:
{{!}}{{{tags}}}
{{!}}-
}}
{{#ifeq: {{#expr: {{{LCT|0}}} or {{{bnf|0}}} or {{#ifeq: {{{hopl|yes}}} | yes | 1 | 0}} }}|0||!See Also:}}
|{{#ifeq: {{{hopl|yes}}}|yes|{{*}}{{#if: {{{hopl id|}}}|{{HOPL|id={{{hopl id}}} }}|{{HOPL}} }}|}}
{{#if: {{{bnf|}}}|{{*}} [{{{bnf}}} BNF Grammar for {{PAGENAME}}]}}
{{#ifeq:{{{LCT|}}}|yes|{{*}} [[Language Comparison Table#{{PAGENAME}}|{{PAGENAME}} compared to other languages]]}}
|}
Listed below are all of the tasks on Rosetta Code which have been solved using {{PAGENAME}}.{{infobox_end}}{{Impl needed}}<includeonly>[[Category:Programming Languages|{{uc:{{PAGENAME}}}}]]{{#set:is language=true}}</includeonly><noinclude>This box is primarily used in category pages (click "What links here" in the navbar on the left), not all of which will have much information in them. If a language category page has too little content, but too many articles, the infobox will overlap the article list.

Usage:
{|class="wikitable" style="text-align: center;"
!Param name
!values
!meaning
|-
|exec
|"machine", "interpreted", "bytecode", or "both"
|execution method
|-
|site
|a full URL
|official language website
|-
|gc
|"yes" or "no"
|garbage collection
|-
|parampass
|"value", "reference", or "both"
|parameter passing mode(s)
|-
|safety
|"safe", "unsafe", or "both"
|type safety
|-
|strength
|"strong" or "weak"
|type strength
|-
|compat
|"nominative", "structural", "both", or "duck"
|type compatibility
|-
|express
|"implicit", "explicit", or "both"
|type expression
|-
|checking
|"static", "dynamic", or "both"
|type checking
|-
|untyped
|"yes" or undefined
|"yes" if the language has no type system
|-
|tags
|a list of lang tag arguments
|this language's corresponding lang tag argument
|-
|hopl
|"yes" or undefined are the same, any other value is essentially "no"
|whether there should be a link to the HOPL from this language page. If defined and non-"yes" there will be no link. If undefined there will be a link.
|-
|hopl id
|id number like "1558"
|id number from HOPL. If defined, link to HOPL bypasses search page.
|-
|LCT
|"yes" or "no"
|whether this language has an entry in the [[Language Comparison Table]]. '''Only use "yes" if you have added it to the LCT or plan to add it shortly.'''
|-
|bnf
|(URL)
|Where the BNF grammar for this language is located or may be found. This should remain undefined if there is no BNF.
|}
{{template}}</noinclude>
