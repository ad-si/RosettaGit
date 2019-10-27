+++
title = "Talk:Longest common prefix"
description = ""
date = 2019-08-20T22:02:54Z
aliases = []
[extra]
id = 18894
[taxonomies]
categories = []
tags = []
+++

== explaining the maths equations ==
Please explain the maths equation for an audience of programmers rather than mathematicians, thanks. --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 15:00, 19 March 2015 (UTC)

== No strings case ==

As I understand it, the example says that given 0 strings, it should return the empty string, i.e. <code>lcp() = ""</code>. Is this necessary? I would argue the answer is undefined in this case, as ''any string'' is vacuously a "common prefix" of 0 strings, and therefore the "longest common prefix" is the longest string, which doesn't exist. Also, if you consider that the longest common prefix of multiple strings can be defined recursively as the longest common prefix of (the first string, the longest common prefix of the rest of the strings), it won't work if the longest common prefix of no strings is the empty string.  --[[User:Spoon!|Spoon!]] ([[User talk:Spoon!|talk]]) 01:32, 21 March 2015 (UTC)

: This definitely requires a special case handling.

: However, any implementation for the empty case requires a special case handling:

: If the result were the length of the prefix, it might be plausible to return a representation of "infinity" for this case. However, the contents of that infinite length prefix would be - as you suggest - undefined.

: Therefore: it's not reasonable to suppose that this prefix should be useful for the recursive case. --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 02:57, 21 March 2015 (UTC)

==Python: Use of an error==
I wish that os.path.commonprefix would be fixed rather than documenting that it is broken and hope to prompt a fix [https://groups.google.com/forum/#!topic/python-ideas/GmExT6lcNfs here] on python-ideas. --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 05:20, 21 March 2015 (UTC)

== Empty string ==

What should be the result of lcp("abc","","abd") ?
or, in your language, 
lcp("throne",\varepsilon,"dungeon") = ??

:That result should be the empty string. --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 21:53, 24 March 2015 (UTC)

::Could you pls add this case to the task description!?! --[[User:Walterpachl|Walterpachl]] ([[User talk:Walterpachl|talk]]) 07:15, 25 March 2015 (UTC)

:::You could do that yourself. Just change the task description and add [[Template:Update]] to the existing implementations. (Note that this isn't my task - I was only answering based on the definition of the task. It should be pretty clear that the longest common prefix for a set of strings which includes the empty string has to be the empty string, because the empty string is the only valid prefix for the empty string...) --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 07:38, 25 March 2015 (UTC)

::::I'll leave that to the task's author. Thanks for the clarification --[[User:Walterpachl|Walterpachl]] ([[User talk:Walterpachl|talk]]) 08:24, 25 March 2015 (UTC)
