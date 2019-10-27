+++
title = "Template:Examples needing attention/Body"
description = ""
date = 2014-03-22T18:21:14Z
aliases = []
[extra]
id = 17414
[taxonomies]
categories = []
tags = []
+++

<includeonly>
{{#arraymap:{{{2}}}
|,
|@@
|{{!}}[[{{#replace:@@|Category:|}}]]{{!}}{{!}}[[{{{1}}}#{{#replace:@@|Category:|}}{{!}}{{{1}}}]]
{{!}}-
|\n}}
</includeonly>

<noinclude>This template is used for creating the body of the table on [[User:AndiPersti/Examples needing attention]]. It expects as first parameter a task page and as second parameter a list of language category pages (both as strings). For each language a row in the table is created containing a link to the language page in the first column and a direct link to the corresponding section on the task page in the second column.

Used in combination with [[Template:Examples needing attention/Header]] and [[Template:Examples needing attention/Footer]].</noinclude>
