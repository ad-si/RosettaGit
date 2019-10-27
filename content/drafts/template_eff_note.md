+++
title = "Template:Eff note"
description = ""
date = 2010-07-05T00:04:48Z
aliases = []
[extra]
id = 4799
[taxonomies]
categories = []
tags = []
+++

<div class="messagebox" style="text-align:center"><small>Generally, this task should be accomplished in {{{1}}} using <code>{{{2}}}</code>. Here we take an approach that's more comparable with the other examples on this page.</small></div><noinclude>

This is meant to replace (possibly pretentious-sounding) notes on examples that highlight the use of more efficient built-in functions to accomplish tasks that ask for a particular algorithm to be used. The language is given as the first argument to this template, and a function name or short code snippet is given as the second argument.

Example use:

For the merge sort task in Java, someone may add a note reading "But none of this is necessary because Collections.sort(List) will use the merge sort when the list is long enough that it is efficient to use it." This note may be replaced with <nowiki>{{eff note|Java|Collections.sort(List)}}</nowiki>.

Try not to put too much text in this box if you want to add to it. It's really just for a little note.

{{template}}</noinclude>
