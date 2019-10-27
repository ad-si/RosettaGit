+++
title = "Template:Task heading"
description = ""
date = 2016-08-23T10:19:42Z
aliases = []
[extra]
id = 21040
[taxonomies]
categories = []
tags = []
+++

<includeonly><div id="{{#replace:{{{1|Task}}}| |_}}" style="margin:20px 0 10px 0; line-height:1.1; font-size:16.5px; font-weight:bold">{{{1|Task}}}{{#ifeq:{{{1|Task}}}|Task|<span class="mw-editsection"><span class="mw-editsection-bracket">[</span>[{{fullurl:{{FULLPAGENAME}}|action=edit&amp;section=0&amp;summary=%2F*%20{{#replace:{{{1|Task}}}| |_}}%20*%2F%20}} edit]<span class="mw-editsection-bracket">]</span></span>}}</div></includeonly><noinclude>

This template can be used to show a heading (which won't affect the table of contents) above the task description on a [[:Category:Programming Tasks|task page]].

Multiple headings can also be used to break longer task descriptions up into logical subsections (see example below).


### Advantages


In the past, some Rosettacode editors have used the definition header syntax (e.g. <code>;Task:</code>) to to create such headings.

Using this template instead, has the following advantages:

* It is styled more appropriately for a heading.
*: <small>''(i.e. larger font, and more spacing.)''</small>

* It has an edit link.
*: <small>''(Only for the main "Task" heading, because due to technical limitations, it can't give you a specific subsection of the task to edit - it gives you the whole task description, or more precisely, everything above the first real heading.)''</small>

* It has a link anchor.
*: <small>''(E.g. to link to the "See also" section, you can write <tt><nowiki>[[#See_also]]</nowiki></tt>. Note that spaces have to be replaced by underscores.)''</small>


### Usage


The first and only argument is the title of the heading to show.

If it is not specified, it defaults to the word "Task".

{| class=wikitable
|-
! Usage example
! Renders as
|-
| style="vertical-align:top; padding:1em" |
<pre style="border:none; margin:0; padding:0">
{{task heading}}

...

{{task heading|Test cases}}

...

{{task heading|See also}}

...

```

| style="vertical-align:top; padding:0 1em" |
{{task heading}}
...
{{task heading|Test cases}}
...
{{task heading|See also}}
...
|}

{{template}}</noinclude>
