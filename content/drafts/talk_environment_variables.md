+++
title = "Talk:Environment variables"
description = ""
date = 2013-03-06T22:02:24Z
aliases = []
[extra]
id = 10903
[taxonomies]
categories = []
tags = []
+++

==PARI/GP and environment variables not being captured==
Hi CRGreathouse, You seem to be ignoring various attempts to explain to you the needs of the task. You have removed flags to your example by first user 192.139.122.42, and then by me where we have tried to point out to you what the task, (and the consensus of the other language examples), is about.

At the moment your example is in error. If you would like to discuss the PARI/GP example and why it doesn't do what is needed then this talk page is the place, but would you leave the notice of its problem alone until it is clearly in accordance with the task.

Please, if others have comments, (either way), then do chip in. --[[User:Paddy3118|Paddy3118]] 07:11, 21 November 2011 (UTC)

: You're making a requirement that isn't part of the task. You have a history of making up these requirements and changing task descriptions until they fit your understanding of them. I do rather wish you'd stop. [[User:CRGreathouse|CRGreathouse]] 08:13, 21 November 2011 (UTC)

::Take a look at most of the other examples and see if you do as they do. We do have to have some consistency of interpretation of the task goal to aid comparison; and yet tasks are written by different people, with different skills in writing descriptions.  If everyone were to try and interpret the description differently then the resulting page would not aid language comparison. With so many examples here, and no demonstrable 'extra' gained by the PARI/GP interpretation of the task goals, there is little reason to keep it, hence the comments. --[[User:Paddy3118|Paddy3118]] 08:29, 21 November 2011 (UTC)
::: a very brief look at the pari documentation gives the impression that there is no way to access environment variables directly. executing a system command seems to be the only way, however besides <code>system()</code> there is a function <code>extern()</code> which feeds the result back. i believe the following would satisfy the task:

```PARI/GP
print(extern("echo $HOME"))
```

::: though i am not sure if the syntax is correct.--[[User:EMBee|eMBee]] 12:07, 21 November 2011 (UTC)

::::Thanks EMBee. Looks good to me. CRGreathouse, if your amenable and could do a further checks that this is valid PARI/GP then this could form the basis of a better solution? --[[User:Paddy3118|Paddy3118]] 13:19, 21 November 2011 (UTC)

::::: I still fundamentally disagree on this issue. No gain?  It shows readers how to do the task in GP; isn't that the whole idea of the site?
::::: But I'm happy to improve the code, so I'll add a version as eMBee suggests.
::::: [[User:CRGreathouse|CRGreathouse]] 22:55, 21 November 2011 (UTC)

== Get or display ==

Many of the examples seem just to display an environment variable. That is not very useful. It would be better to show how to get an environment variable into an internal variable so that it can be processed further in the program code. --[[User:PauliKL|PauliKL]] 09:18, 6 March 2013 (UTC)

:Could you state some examples please? Thanks. --[[User:Paddy3118|Paddy3118]] 12:07, 6 March 2013 (UTC)

::If I understand them correctly, at least the following examples do not specify any variable where the data is read: C, C++, Clojure, Delphi/Pascal, E, Eiffel, Euphoria, Factor, Icon, Java, Joy, Lua, Mathematica, MATLAB / Octave, Perl, etc.
::Maybe some languages do not use variables. For example Forth stores the data in stack I guess. Anyway, the purpose of reading an environment variable is that you do something with it, for example check if it has a specific value. --[[User:PauliKL|PauliKL]] 16:40, 6 March 2013 (UTC)

:::I guess it is a simple task and their interpretations fit. I see your point too and maybe the task should have been enhanced from the outset, but it is too late for a change now.

::::Getting the environment variable into an internal variable is still within the scope of the existing task description. Individual solutions can be enhanced to encompass utilization via an internal variable where needed, without the need for a mandate.  [[User:Markhobley|Markhobley]] 21:11, 6 March 2013 (UTC)
:::::A task change for assigning an environment variable to a simple program variable would break too many examples. --[[User:Paddy3118|Paddy3118]] 22:02, 6 March 2013 (UTC)

:::I queried your question as I thought you might be referring to "shelling out" when running on Unix toouse the shell to print an environment variable. --[[User:Paddy3118|Paddy3118]] 20:46, 6 March 2013 (UTC)
