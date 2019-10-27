+++
title = "Talk:Empty string"
description = ""
date = 2011-07-08T02:42:00Z
aliases = []
[extra]
id = 10024
[taxonomies]
categories = []
tags = []
+++

Can we have an example of a language which has special syntax for empty strings, to justify the first claim in the task description? —[[User:Kevin Reid|Kevin Reid]] 21:51, 4 July 2011 (UTC)
: The Perl 6 and Python examples both mentioned how empty strings are treated as boolean false, while nonempty ones are true (well in Perl if nonempty string looks like number "0" it's again treated as false, but that's more exception than rule). --[[User:Ledrug|Ledrug]] 23:30, 4 July 2011 (UTC)
::Those are ''interpretations'' or ''semantics'' of empty strings, not ''syntax for'' empty strings. —[[User:Kevin Reid|Kevin Reid]] 00:16, 5 July 2011 (UTC)

: There are lots of examples of languages with special semantics around null strings.  Syntax, I'm not so sure.  SNOBOL was one, variables are NULL strings by default and they get reinterpreted depending on the operation. Add a null string and it treats it as a zero. Icon/Unicon have special syntax for a null type but that's not the same as a null string.   What's expected where a language doesn't have special syntax? --[[User:Dgamey|Dgamey]] 03:26, 5 July 2011 (UTC)

:: Just demonstrate how to achieve the task would be achieved within the language. [[User:Markhobley|Markhobley]] 08:32, 5 July 2011 (UTC)

:Perhaps ruby's nil.to_s would count as special syntax?  --[[User:Rdm|Rdm]] 16:49, 5 July 2011 (UTC)

No one has presented an example of syntax for creating an empty string, so unless there are any objections I will remove that part of the task description soon. —[[User:Kevin Reid|Kevin Reid]] 17:57, 6 July 2011 (UTC)

:Ok hang on a bit with that: ruby has already been mentioned and I may be able to provide some more examples. [[User:Markhobley|Markhobley]] 18:31, 6 July 2011 (UTC)

::nil.to_s is not ''syntax''; it's a method of nil! I will wait. —[[User:Kevin Reid|Kevin Reid]] 18:42, 6 July 2011 (UTC)
:::Certainly. But <code>nil.to_s</code> has syntax which is not the standard syntax for representing an arbitrary string.  But perhaps your objection is that the syntax involved is not dedicated to the purpose of representing empty strings?  It's true that the method invocation syntax behind <code>nil.to_s</code> is very general and that <code>'arbitrary string'.to_s</code> can be used to represent an arbitrary string. --[[User:Rdm|Rdm]] 19:58, 6 July 2011 (UTC)
::::I think Kevin Reid's objection is valid, as none of the examples here so far provided a  ''syntax'' dedicated to dealing with an empty string; <code>nil.to_s</code> in a sense is still showing the ''semantics'' about equivenlance between nil and empty string.  It probably wouldn't do any harm to leave the "syntax" wording alone, but strictly speaking it shouldn't be there. --[[User:Ledrug|Ledrug]] 01:28, 8 July 2011 (UTC)
:::::How about http://www.snobol4.org/docs/burks/tutorial/ch4.htm?  Here, an empty replacement field is treated as an empty string.  (Note that in another context in snobol, blank is a concatenation operator.)  Note also that the same issue arises in shell scripting: in various contexts the absence of an argument is treated the same as the presence of an empty string.  In other words, the following are equivalent:
:::::
```bash
$ FOO='5'; FOO=''; echo $FOO
$ FOO='5'; FOO=; echo $FOO
```
  --[[User:Rdm|Rdm]] 01:40, 8 July 2011 (UTC)
::::::Heh the SNOBOL example is fair enough.  As for shells, the multitude of quoting rules, whitespaces and such are so all over the place that I'd rather we not discuss it (but if you want to feed a program an empty argument instead of no argument, you do say <code>foo ""</code>, not <code>foo</code> &mdash; but hey, I'm not discussing it.) :) --[[User:Ledrug|Ledrug]] 02:42, 8 July 2011 (UTC)
::The possible minor rewording of the task description will not affect the task itself. Is there any reason that we can not promote this to task? [[User:Markhobley|Markhobley]] 16:42, 7 July 2011 (UTC)
