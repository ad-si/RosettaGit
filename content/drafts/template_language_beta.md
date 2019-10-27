+++
title = "Template:Language beta"
description = ""
date = 2013-03-30T16:17:18Z
aliases = []
[extra]
id = 7017
[taxonomies]
categories = []
tags = []
+++

<includeonly>{{language/Start}}
{{#if:{{{site|test}}}|{{{!}}style="text-align: left; font-size: 75%; background: transparent;"
{{!}}-
![{{{site|http://example.com}}} Official website]
{{!}}}
}}
{|style="text-align: left; font-size: 90%"
{{#if: {{{exec|test}}}|{{language/Property/Generic|[[:Category:Execution method|Execution method]]:|{{#switch: {{lc:{{{exec|machine}}}}}|machine = Compiled (machine code) [[Category:Execution method/Compiled/Machine code]]|interpreted = Interpreted [[Category:Execution method/Interpreted]]|bytecode = Compiled (bytecode) [[Category:Execution method/Compiled/Bytecode]]|both = Interpreted or compiled [[Category:Execution method/Interpreted]][[Category:Execution method/Compiled]]}}}}
}}
{{#if: {{{gc|test}}}|{{language/Property/Generic|[[:Category:Garbage collection|Garbage collected]]:|{{ucfirst:{{{gc|yes}}}}}[[Category:Garbage collection/{{ucfirst:{{{gc|yes}}}}}]]}}
}}
{{#if: {{{parampass|test}}}|{{language/Property/Generic|[[:Category:Parameter passing|Parameter passing methods]]:|
{{#ifeq: {{{parampass|both}}}|both|'''By reference, By value'''[[Category:Parameter passing/By reference]][[Category:Parameter passing/By value]]|
 '''By {{lc:{{{parampass}}}}}'''[[Category:Parameter passing/By {{lc:{{{parampass}}}}}]]}}}}
}}
{{#if: {{{safety|test}}}|{{language/Property/Generic|[[:Category:Typing|Type safety]]:|
{{#ifeq: {{{safety|both}}}|both|'''Safe, Unsafe'''[[Category:Typing/Safe]][[Category:Typing/Unsafe]]|'''{{ucfirst:{{{safety}}}}}'''
[[Category:Typing/{{ucfirst:{{{safety}}}}}]]}}}}
}}
{{#if: {{{strength|test}}}|{{language/Property/Generic|[[:Category:Typing|Type strength]]:|
'''{{ucfirst:{{{strength|strong}}}}}'''[[Category:Typing/{{ucfirst:{{{strength|strong}}}}}]]}}
}}
{{#if: {{{compat|test}}}|{{language/Property/Generic|[[:Category:Typing/Compatibility|Type compatibility]]:|
{{#ifeq: {{{compat|both}}}|both|'''Nominative, Structural'''[[Category:Typing/Compatibility/Nominative]][[Category:Typing/Compatibility/Structural]]|'''{{ucfirst:{{{compat}}}}}'''
[[Category:Typing/Compatibility/{{ucfirst:{{{compat}}}}}]]}}}}
}}
{{#if: {{{express|test}}}|{{language/Property/Generic|[[:Category:Typing/Expression|Type expression]]:|
{{#ifeq: {{{express|both}}}|both|'''Implicit, Explicit'''[[Category:Typing/Expression/Implicit]][[Category:Typing/Expression/Explicit]]|{{ucfirst:{{{express}}}}}
[[Category:Typing/Expression/{{ucfirst:{{{express}}}}}]]}}}}
}}
{{#if: {{{checking|test}}}|{{language/Property/Generic|[[:Category:Typing/Checking|Type checking]]:|
{{#ifeq: {{{checking|both}}}|both|'''Dynamic, Static'''[[Category:Typing/Checking/Dynamic]][[Category:Typing/Checking/Static]]|
{{ucfirst:{{{checking|}}}}}[[Category:Typing/Checking/{{ucfirst:{{{checking}}}}}]]}}}}
}}
{{#if: {{{untyped|}}}|{{language/Property/Generic|{{#ifeq: {{{untyped|yes}}}|yes|Typing:}}|
{{#ifeq: {{{untyped|yes}}}|yes|'''Untyped'''[[Category:Typing/Untyped]]}}}}
}}
{{#if: {{{tags|test}}}|{{language/Property/Generic|[[Help:Syntax_Highlighting|Lang tag(s)]]:|
'''{{{tags|test1, test2}}}}}'''
}}
{{#ifeq: {{#expr: {{{LCT|0}}} or {{{bnf|0}}} or {{#ifeq: {{{hopl|yes}}} | yes | 1 | 0}} }}|0||{{language/Property/Generic|See Also:|
{{#ifeq: {{{hopl|yes}}}|yes|*'''{{HOPL}}'''|}}
{{#if: {{{bnf|test}}}|* '''[{{{bnf|http://example.com}}} BNF Grammar for {{PAGENAME}}]'''}}
{{#ifeq:{{{LCT|yes}}}|yes|* '''[[Language Comparison Table#{{PAGENAME}}|{{PAGENAME}} compared to other languages]]''}}}}}}
|}
Listed below are all of the tasks on Rosetta Code which have been solved using {{PAGENAME}}.{{sharethis}}
{{language/End}}
[[Category:Programming Languages|{{uc:{{PAGENAME}}}}]]</includeonly><noinclude>This box is primarily used in category pages (click "What links here" in the navbar on the left), not all of which will have much information in them. If a language category page has too little content, but too many articles, the infobox will overlap the article list.

Usage:
{|style="text-align: center;"
!Param name
!values
!meaning
|-
|name
|any string
|the name of the language
|-
|exec
|"machine", "interpreted", or "bytecode"
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
|"implicit" or "explicit"
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
|LCT
|"yes" or "no"
|whether this language has an entry in the [[Language Comparison Table]]
|-
|bnf
|(URL)
|Where the BNF grammar for this language is located or may be found
|}
{{template}}[[Category:Possible Redundant Templates]]</noinclude>
