+++
title = "Talk:Optional parameters"
description = ""
date = 2012-05-13T14:58:52Z
aliases = []
[extra]
id = 4237
[taxonomies]
categories = []
tags = []
+++

==Table?==
So the input table in your example is:

```txt

  "a"    "b"    "c"
  ""     "q"    "z"
  "zap"  "zip"  "Zot"

```

And sorting is ''of'' rows and ''by'' column.
Actually, now I have written it out - it makes sense. --[[User:Paddy3118|Paddy3118]] 06:58, 24 May 2009 (UTC)

===Don't use this table===
I don't recommend copying my sample data -- it's ugly and also suffers from being written in a hurry -- in particular, the length is too correlated with the first letter. --[[User:Kevin Reid|Kevin Reid]]

== Isn't too much if the aim is to show optional parameters? ==

I mean, if the aim if to show how languages allow (if allow) (named) optional parameters, then isn't the task too much complex? Even without implementing the sort if not built-in, wouldn't it be simpler just to ask for sorting of an array with the optional parameter ordering and reverse? It would show anyway the "optional parameter" part without the fuss of coping with multidimensional arrays? --[[User:ShinTakezou|ShinTakezou]] 09:10, 24 May 2009 (UTC)

: Since you're not required to provide the actual sort, you don't actually need to deal with the data structure at all — just provide the parameter declaration. The reason the parameter set is so complex is to discourage the scenario of “oh, this situation is so simple it doesn't need optional parameters”. --[[User:Kevin Reid|Kevin Reid]] 11:12, 24 May 2009 (UTC)

:: Yep, Fortran code I am going to add does a little bit more... could it be all reduced to the "interface" (in the Fortran sense) declaration and an explanation of the "present" intrinsic? (To discourage the scenario, it would be enough to me to focus the task on optional argumentes... they can't be disregarded if the task is about them!) --[[User:ShinTakezou|ShinTakezou]] 12:46, 24 May 2009 (UTC)

: I don't understand the penalization of languages that don't have a SORT built-in.  The sort part is trivial.  What I thought was the point of the task was to show how to use/utilize/specify optional parameters.  The REXX language handles these types of problems with ease, even though it has a pretty small set of built-ins.  The handling of optional/named/multiple/omitted/null/positional/etc. parameters is where REXX excels.  (By the way, omitted and null parameters aren't the same in REXX.)  What is the purpose in excluding languages that don't have a SORT? Do you think the (source code) example(s) would become too large?  Would it be OK to add an example, but don't include the SORT code? -- [[User:Gerard Schildberger|Gerard Schildberger]] 23:35, 12 May 2012 (UTC)

::I read it as just omit the implementation ''details'' of a sort and just call simpler sort(s) allowing the example to concentrate on the optional parameters to the enclosing routine? --[[User:Paddy3118|Paddy3118]] 07:56, 13 May 2012 (UTC)

::: I didn't read it that way unfortunately.  I read it (erronously) that "the implementation" was the implementation of the example, not the sort.  By bad.  It would be nice if that sentence would read: ",just omit the sort implementation (with a comment)."  --- but it's not my dog.  -- [[User:Gerard Schildberger|Gerard Schildberger]] 14:58, 13 May 2012 (UTC)

== Absence of arguments ==

I just realized that my design for this task does not exercise the language's facility for distinguishing ''absence'' of optional arguments (as opposed to having a simple default value which someone could also supply explicitly). Is this worthwhile? Any suggestions on how to incorporate it? --[[User:Kevin Reid|Kevin Reid]] 11:12, 24 May 2009 (UTC)

:I was thinking of adding the following task:
::'''Optional Arguments'''

::Create a function that takes a number as its first argument followed by that number of extra arguments. The function should check the number of arguments given and return -1 if their are not enough, 0 if their are the required amount, and 1 if their are too many extra arguments given.

:This is the kind of thing that C's sprintf does. --[[User:Paddy3118|Paddy3118]] 13:18, 24 May 2009 (UTC)

:: It's "simply" vararg, maybe there exists already a task showing that in particular. But not so simple is to cope with optional parameters. I've thought using something like "tags", as did the "new" (!) AmigaOS 2.x API (e.g. take a look at [http://www.liquido2.com/tutorial/close_window.html this code] to understand what I mean, or my "article" [http://www.capo-nord.org/soci/xmav/articolo.php?id=20080413 here] <nowiki>[</nowiki>which contains 3 strangeness I note now! well, consider it C-like pseudolanguage, not real C<nowiki>]</nowiki>)... but this is "''cheating''", it's not a facility of the language --[[User:ShinTakezou|ShinTakezou]] 13:59, 24 May 2009 (UTC)

::I agree that's more in the scope of [[varargs]]. I'm talking about specific (that is, not just a collection of all following arguments) optional parameters having not-otherwise-reproducible behavior when omitted. --[[User:Kevin Reid|Kevin Reid]] 14:01, 24 May 2009 (UTC)

:::Some languages strongly distinguish, some don't. The fact that some do though means that it is worth describing on this site. On the other hand, the proposed name is far too close to this task's, so it's better to split this one so that it focusses on the optionality, and have [[Named Arguments|another one]] handle the naming. —[[User:Dkf|Donal Fellows]] 08:06, 29 June 2009 (UTC)

::::I'm not sure exactly what you propose splitting this task to; could you clarify?
I am concerned about creating too fine-grained distinctions: RC tasks should compare the approaches of different languages, and we should therefore avoid preferring tasks which are "Demonstrate this feature of your language if it has it". This task I specifically designed to be neutral about whether the language supports named parameters. --[[User:Kevin Reid|Kevin Reid]] 11:13, 29 June 2009 (UTC)
