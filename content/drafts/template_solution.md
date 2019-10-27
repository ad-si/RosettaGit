+++
title = "Template:Solution"
description = ""
date = 2010-10-22T19:02:53Z
aliases = []
[extra]
id = 8549
[taxonomies]
categories = []
tags = []
+++

<includeonly>
<!-- Property this -->
== {{{lang}}} ==
<!-- Property this -->
{{#if: {{{translation|}}}
 | '''Translation of [[#{{{translation|}}} |{{{translation|}}} Solution]].'''
 |
}}

<!-- Property this -->
{{#if: {{{libraries|}}}
 |
'''Libraries:'''
{{#arraymap:{{{libraries}}}|,|x|[[Libraries/x | x]]|,}}

 |'''This solution has no libraries specified.'''
}}

<!-- Property this -->
{{#if: {{{code|}}}
 |
'''Code:'''
{{{code}}}
 |{{requires-code}}
}}

<!-- Property this -->
{{#if: {{{example|}}}
 |
'''Example usage:'''
{{{example}}}
 |{{requires-example}}
}}

<!-- Property this -->
{{#if: {{{output|}}}
 |
'''Output:'''
{{{output}}}
 |{{requires-output}}
}}
[[Is solution of::{{{task}}}|]][[Page has default form::Solution| ]]
<!-- categories,properties here-->
</includeonly>
