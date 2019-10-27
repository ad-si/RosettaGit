+++
title = "Talk:Go Fish/Go"
description = ""
date = 2015-09-12T05:21:34Z
aliases = []
[extra]
id = 18774
[taxonomies]
categories = []
tags = []
+++

== Not idiomatic ==

This is not an idiomatic or good example of Go code.

See [https://golang.org/doc/effective_go.html Effective Go] and [https://golang.org/wiki/CodeReviewComments the Go Authors code review comments page] to start.

For example:
* Identifier names should not have underscores and no leading underscores. Names like <code>kFoo</code> and <code>tBar</code> are also non-idiomatic.
* Function/method comments are usually in <code>//</code> rather than <code>/* */</code>. They shouldn't be wrapped so short and they should be written for reasonable <tt>godoc</tt> parsing (e.g. start with a summary sentence that starts with the name of the function/method/type).
* Receiver names should '''not''' be <code>self</code>, <code>this</code>, or <code>me</code>
* …
The overall structure and quality of this example is also sub-par.

&mdash;[[User:dchapes|dchapes]] ([[User talk:dchapes|talk]] | [[Special:Contributions/dchapes|contribs]]) 18:45, 22 February 2015 (UTC)

It's been refactored on two separate occasions since this was posted. How do you feel about it now?

&mdash;[[User:Joshua.Snider|Joshua.Snider]] ([[User talk:Joshua.Snider|talk]]) 05:21, 12 September 2015 (UTC)
