+++
title = "Talk:SHA-256"
description = ""
date = 2015-11-14T23:10:27Z
aliases = []
[extra]
id = 13354
[taxonomies]
categories = []
tags = []
+++

== [[Perl 6]] implementation takes all input in memory ==
The current implementation takes all data at once, which is not a good idea if the data is large.
Ideally the function should be able to take a possibly lazy list of buffers.
All suggestions welcome.
--[[User:Grondilu|Grondilu]] ([[User talk:Grondilu|talk]]) 20:46, 23 April 2013 (UTC)

== Spec ==

This SHA-256 task really needs the spec included, the way the SHA-1 included the necessary reference material for that task. --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 23:10, 14 November 2015 (UTC)
