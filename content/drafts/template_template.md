+++
title = "Template:Template"
description = ""
date = 2010-08-28T13:41:35Z
aliases = []
[extra]
id = 2213
[taxonomies]
categories = []
tags = []
+++

<includeonly>
----
</includeonly>
{{alertbox|lightgreen|This is a template.  There are many others.  See [[:Category:RCTemplates]] for a complete list of templates.}}[[Category:RCTemplates|{{PAGENAME}}]]{{#if: {{{1|}}} |[[Category:RCTemplates/{{{1}}}]]}}<noinclude>
--The above gets included in any page (preferably templates) that uses ''this'' template.

Mind you: The proper way to use ''this'' template is such:

<nowiki>&lt;noinclude&gt;{{template}}&lt;/noinclude&gt;</nowiki>

If you neglect the "noinclude" tags, you'll include the body of this template in any page that uses the template you're editing.

If you're reading this on Rosetta Code, you're probably a programmer in some language that supports function calls.  Think of this template like a function call from within a function call, and the return value of the nested call gets included with the return value of the parent function.  The "noinclude" tags prevent the nested call's output from affecting the parent function's return value.
</noinclude>
