+++
title = "Talk:Assertions"
description = ""
date = 2010-03-24T09:49:03Z
aliases = []
[extra]
id = 4928
[taxonomies]
categories = []
tags = []
+++

I propose adding to the task that examples shall state whether there is a compile- or run-time option to enable/disable assertions (as in C or Java) or not (as in Common Lisp or E). --[[User:Kevin Reid|Kevin Reid]] 03:07, 26 October 2009 (UTC)
: Yes. A more general related issue is whether the assertion is a part of the program logic or not. Consider assertions raising exceptions. Let the program handle these exceptions and do something different than terminating or breaking into the debugger. This is a behavior, which will be lost when the assertion gets disabled. For example, a program may assert that the file end is not reached and exit the file reading loop when the assertion fails. Such programs are considered "wrong". But failed assertions '''have''' to behave in some way. Once they do (e.g. propagate an exception) that behavior can/must be exploited making the program broken, almost automatically. --[[User:Dmitry-kazakov|Dmitry-kazakov]] 08:38, 26 October 2009 (UTC)
:: If you've got a managed monitoring environment, the only way that the program might see the assertion failure might be as a very long time to execute some piece of code. (There are many different ways to handle an assertion failure trap; throwing a normal exception is only one of them.) —[[User:Dkf|Donal Fellows]] 09:30, 26 October 2009 (UTC)
::: The program may not see an assertion failure in itself. Another program (e.g. the debugger, watch dog monitor, OS supervisor etc) can. The problem with assertions in many implementations is that they execute code with side effects on the program context. It is important for this task to specify whether the language implementation of assertions is consistent in not violating the program semantics, e.g. in having no side effects whenever assertions enabled or disabled. --[[User:Dmitry-kazakov|Dmitry-kazakov]] 19:49, 26 October 2009 (UTC)

==Boolean expression?==
The task description fails to mention that assertions test some expression that is expected to be true for normal program flow to continue. This aspect is also mentioned [[wp:Assertion (computing)|here]] on wp. Shouldn't the task description be modified? --[[User:Paddy3118|Paddy3118]] 09:49, 24 March 2010 (UTC)
