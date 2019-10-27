+++
title = "Talk:FASTA format"
description = ""
date = 2016-09-14T00:57:49Z
aliases = []
[extra]
id = 13253
[taxonomies]
categories = []
tags = []
+++

==FASTA file specifications==

I didn't know how much of the FASTA file specifications were to be implemented (supported), so for the REXX example (version 2), I took what I could glean from the WIKI article and supported:

* blank lines
* lines that start with a semicolon [<code>;</code>]
* lines that end with an asterisk [<code>*</code>]
* data lines that contain blanks, tabs, and other whitespace



No attempt was made to format lines that contain   <code>␋</code>   (vertical tabs, U+0000B) which can be used to specify multiple lines in one record.
-- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 20:17, 5 April 2013 (UTC)

== "Quality"? ==

I am dubious of the "quality of implementation" claim on the original page.  Some laptops purchased last year had 32GB memory and many had 16GB. --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 21:01, 20 April 2013 (UTC)
