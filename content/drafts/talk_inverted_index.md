+++
title = "Talk:Inverted index"
description = ""
date = 2010-04-29T14:04:26Z
aliases = []
[extra]
id = 7091
[taxonomies]
categories = []
tags = []
+++

==Which Kind of Inverted Index?==

Were we supposed to do an '''inverted file index''' or a '''full inverted index'''? I've implemented both in the Tcl solution even though that does make the code quite a bit more complex (and in production code, I'd use [[SQLite]] with its FTS module as that lets you do all this and more). –[[User:Dkf|Donal Fellows]] 09:56, 21 April 2010 (UTC)

: Nice job.  whatever you did can be a "bonus" task I guess... 
: I am not sure I  understand.  What is a "full inverted index"?
:: It's defined on the Wikipedia page you linked to. :-) Basically, it's keeping not just the file name/index, but also the index into the file of the word being indexed. What it means is that you can construct a metric over the words in the file so that you can say "this word is next to that one" in a relatively efficient fashion. –[[User:Dkf|Donal Fellows]] 08:37, 22 April 2010 (UTC)
::: Since the specification did not say anything about supporting phrase searching, I imagine that that should be a different task. --[[User:Rdm|Rdm]] 14:04, 29 April 2010 (UTC)

: Re “production code / fts module”, I just wanted to take away the mystery of search a little. Will try to do [http://en.wikipedia.org/wiki/Disjoint-set_data_structure union-find] soon. --[[User:Tinku99|Tinku99]] 19:27, 21 April 2010 (UTC)
:: Good. Could do with more of these sorts of tasks. –[[User:Dkf|Donal Fellows]] 08:37, 22 April 2010 (UTC)
