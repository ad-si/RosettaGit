+++
title = "Template:Unimpl header"
description = ""
date = 2009-10-18T18:32:13Z
aliases = []
[extra]
id = 3550
[taxonomies]
categories = []
tags = []
+++

<includeonly>[[Category:Unimplemented tasks by language|{{{1}}}]]</includeonly>{{#ifeq: {{{1}}} | C |<div class="messagebox">Due to a bug in mod_rewrite, the link for tasks not implemented in C++ may redirect here. To go there, try [http://rosettacode.org/w/index.php?title=Reports:Tasks_not_implemented_in_C%2B%2B this link] instead.</div>|}}

'''The code that drives these pages is under active development, and formatting issues may be present.  However, the result set itself should be accurate to within fifteen minutes of the last viewing.'''

These are currently not implemented in [[:Category:{{{1}}}|{{{1}}}]]. Please implement if you can. If any tasks are not possible or too complex in {{{1}}}, they will not be on this list. To mark a task as such, add <nowiki>{{omit from|</nowiki>{{{1}}}<nowiki>}}</nowiki> to that task.

<div style="clear: both; column-count: 3; -webkit-column-count:3; -moz-column-count:3"> <!-- it would be better to have this inside one page balanced. Perhaps use a universal template inside each of the "Tasks not implemented in ___" pages which contains the reference to the "unimp body ___" page? -->
