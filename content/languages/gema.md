+++
title = "Gema"
description = ""
date = 2010-05-18T13:32:10Z
aliases = []
[extra]
id = 6791
[taxonomies]
categories = []
tags = []
+++

{{language|gema
|site=http://gema.sourceforge.net/new/index.shtml
}}


gema is a general purpose text processing utility based on the concept of pattern matching. In general, it reads an input file and copies it to an output file, while performing certain transformations to the data as specified by a set of patterns defined by the user. It can be used to do the sorts of things that are done by Unix utilities such as cpp, grep, [[sed]], [[AWK]], or strings. It can be used as a macro processor, but it is much more general than cpp or [[m4]] because it does not impose any particular syntax for what a macro call looks like. Unlike utilities like [[sed]] or [[AWK]], gema can deal with patterns that span multiple lines and with nested constructs. It is also distinguished by being able to use multiple sets of rules to be used in different contexts.

gema has been extended with a binding to the scripting language [[Lua]], named GeL, that provides the ability of executing piece of [[Lua]] code in gema actions. It also may be used to add gema powerful matching capabilities to any Lua-enabled software.
