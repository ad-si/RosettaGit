+++
title = "Talk:Arithmetic evaluation"
description = ""
date = 2019-07-01T04:03:26Z
aliases = []
[extra]
id = 2415
[taxonomies]
categories = []
tags = []
+++

== assume clean input or do error checking? ==
Should we assume clean input or do we need to add in error checking? --[[User:Mwn3d|Mwn3d]] 14:17, 11 December 2007 (MST)
:Left unspecified (a solution can do either) [[User:Epsilon]]

== TCL and [expr] ==

As is is written, this task is impossible in TCL since TCL has no concept of a "number" -- all variables are strings of bytes at the bottom and the only way to add a number to another one is to expressly declare some string to be interpreted as ... an expression. I.e. if variable "A" contains the string "3" then TCL has no idea that this might be a number. However [expr $A] would be a number (returned as a string again) and [expr $A+3] would be the string "6". Thus no matter how complicated my program might get, at some point I would <i>have to</i> invoke something like [expr $var1 $symbol $var2] (which would take the string formed by concatenating the contents of $var1, $symbol and $var2 and interpret them as an expression and return the result). This "bottom invocation", however would be prohibited by the task rules (and if I allow [expr] then I might as well just call [expr $original_string] and be done with the whole exercise in one line). Suggestions anybody? [[User:Sgeier|Sgeier]] 20:01, 22 January 2008 (MST)
:I have rephrased the task in an attempt to clarify that the prohibition is against relatively direct evaluation of the input. Evaluation of the results of parsing is expected, not prohibited.  --[[User:TBH|TBH]] 09:37, 23 January 2008 (MST)

==Bigger than a task?==
Is this task on par with [[RCBF]]? Is it closer to a project than a task? --[[User:Mwn3d|Mwn3d]] 08:55, 24 January 2008 (MST)
:: To be honest, I'd put it in with "riddles" more than anything. It's a pretty standars CS101 task and the generic answer says that you use two stacks, one for the numbers and one for the operators. Thus this really boils down to "implement a stack". Of course that doesn't mean that someone might not have a particularly clever solution that would teach a lot about the way some language works. (Just because I find a task uninteresting doesn't mean someone else does the same). The reason I would put this in "puzzles" is that all languages I actually use have an arithmetic evaluator built-in. Thus "assign a value to an array" is an operation I do actually perform in my code, but "parse arithmetic strings" is not.[[User:Sgeier|Sgeier]] 14:46, 24 January 2008 (MST)
:::That makes sense. It's at least ''different'' from a regular task. A prefix notation "small programming language" interpreter (with just addition and multiplication and strings) was actually a project for my CS3 class (the CS classes are 200-level at my school). We did it using a tree rather than a stack, but that was for prefix notation. --[[User:Mwn3d|Mwn3d]] 14:51, 24 January 2008 (MST)
:The per-language examples are pretty darned big.  Does anybody else see a need for cutting the larger examples off into supplement pages? --[[User:Short Circuit|Short Circuit]] 18:23, 22 February 2008 (MST)



== Use of libraries? ==

This task would be a perfect fit for the boost.spirit library. Is using such a library appropriate for this task? (Note that unlike parser generators like yacc, boost.spirit is a pure library solution, i.e. you don't use an external tool to generate C++ code, but you directly feed your source code to the C++ compiler). --[[User:Ce|Ce]] 10:39, 22 February 2008 (MST)
:If it's the right tool for the job, that's perfectly fine with me. --[[User:Short Circuit|Short Circuit]] 18:20, 22 February 2008 (MST)

== Why AST is required? ==
Why AST generation is put into the requirement? Clearly arithmetic expression interpretation with predefined types does not need AST. In fact, it is just wasting resources to use AST for that. So if AST is essential, then the task should rather be named like, parsing infix expression in order to generate AST. I.e. the output must be the AST, not the expression value. --[[User:Dmitry-kazakov|Dmitry-kazakov]] 19:39, 3 September 2008 (UTC)

==Autohotkey example being solely a link offsite==
It is not ideal. Should it be allowed?

If left to a vote I would vote that either someone acquired the permissions to post the code here under a compatible license or it was left out. --[[User:Paddy3118|Paddy3118]] 17:54, 22 April 2010 (UTC)
: I see that with some J examples, too. I don't like it, myself; it makes it more difficult to see the code in context. I'd rather not see them removed, though. Perhaps a usage of ENA, something along the lines of "this example is offsite, and should be hosted on wiki"? --[[User:Short Circuit|Michael Mol]] 02:15, 23 April 2010 (UTC)

:: What is ENA? Does it flag something on the languages pages like incorrect does? Could you implement this on this example please? --[[User:Paddy3118|Paddy3118]] 05:54, 23 April 2010 (UTC)
::: Example Needs Attention; a catch-all name for [[:Category:Example attention templates|these]]. I was thinking of a catch-all template for a catch-all case, though. If nobody else does, I can put it together this evening. --[[User:Short Circuit|Michael Mol]] 14:47, 23 April 2010 (UTC)
: sorry about the off site link initially.  I had tried to solve this task a while ago without knowing any theory and failed.  Well, I went off and studied antlr for a couple of weeks.  Fixed !! unless some one finds a bug... [[User:Tinku99|Tinku99]] 05:47, 29 May 2010 (UTC)

== Ruby Code and permissions ==
Hi, I noted that the author given for the Ruby code and the name of the person submitting the code didn't agree and there seems to be no comment on licensing/permissions? --[[User:Paddy3118|Paddy3118]] 21:45, 8 September 2010 (UTC)

==unary operators==

This task's requirements states
::::''The four symbols + - * / must be supported as binary operators with conventional precedence rules.''
but doesn't mention explicitly that the   '''+'''   and   '''-'''   be supported as unary operators.


I.E.:     -3+27

  and    5678/(-9*14)


Can we assume that unary operators should be supported? -- [[User:Gerard Schildberger|Gerard Schildberger]] 04:05, 24 December 2012 (UTC)

== FBSL Maths Evaluation ==
FBSL has a full range of string functions to create decent parsers similar to what other languages are doing here. But it would be extremely inefficient, speed-wise, to follow these lines in an interpretative environment having a ready-made ExecLine() at hand. AST's are part of FBSL's intrinsic multilevel parser. Can I consider the task fulfilled as it is? TheWatcher 20:36, 11 May 2013 (UTC)

:Hi, Most of the interpreted languages have eval/exec too, but the second point of the task description specifically stops their straight-forward use:
::''The AST must be used in evaluation, also, so the input may not be directly evaluated (e.g. by calling eval or a similar language feature.)''
:If you have access to your underlying AST then you can do something similar to the Python second entry. --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 02:36, 12 May 2013 (UTC)

::Hi, Then I guess I'll have to remove this entry until better times... :) TheWatcher 06:19, 12 May 2013 (UTC)
::Again, BBC BASIC here does build an AST with extra parentheses denoting tree nodes. Then it uses the AST as a string input for its intrinsic EVAL() to get the expression result. Is that an acceptable scenario? TheWatcher 06:36, 12 May 2013 (UTC)

:::Sounds fine. --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 07:44, 12 May 2013 (UTC)
