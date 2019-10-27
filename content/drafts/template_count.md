+++
title = "Template:Count"
description = ""
date = 2014-03-22T19:53:36Z
aliases = []
[extra]
id = 17436
[taxonomies]
categories = []
tags = []
+++

<includeonly>
{{#arraydefine:temp|{{{2}}} }}
|[[{{{1}}}]]||{{#arraysize:temp}}
|-
</includeonly>

<noinclude>This template creates a row of a ranking table. The first parameter should be a page (as a string) and the second a comma-separated list. The number of elements of the list will be printed in the second column.

A full table still needs a [[Template:2-column table header|header]] and a [[Template:Table footer|footer]].</noinclude>
