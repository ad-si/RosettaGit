+++
title = "Talk:Named parameters"
description = ""
date = 2015-08-11T08:58:01Z
aliases = []
[extra]
id = 4452
[taxonomies]
categories = []
tags = []
+++

==Duplication?==
You don't seem to have adequately expressed the difference between this task and Varargs/Optional parameters. If you take away the TCL example, what is left?
Is the task to show arguments being given in a call in a different order to that of the definition? Are default values necessary?
Do we need to overhaul all three tasks and work out what aspects of function arguments we need to show/how many tasks to do it? --[[User:Paddy3118|Paddy3118]] 05:58, 29 June 2009 (UTC)
:There is some duplication, yes, but it seems that the [[Optional parameters]] task was written by someone who wasn't aware of the possibility of optional positional parameters, which some languages support, so leaving that task still ambiguously defined. I'm trying to decompose features in this area so that these assumptions are teased out, and the naming of parameters is actually independent of supporting variable numbers of them (witness [[Objective C]]). Another justification for having them separate is that being able to set parameters by name leads to a different, far more literate programming style to setting them purely by position.
:Also note that the Tcl example in the [[Optional parameters]] task is wrong; there's another mechanism (optional positional parameters) that should be demonstrated there instead. I'll have to correct that. Bother… —[[User:Dkf|Donal Fellows]] 07:56, 29 June 2009 (UTC)
::When I wrote [[Optional parameters]] it was my intent that positional optional parameters were included in it; the text even says so. It was designed to be accomplished in many different ways (as opposed to, say, this task, which specifies the particular concept of named parameters).
I am concerned that the idea you mention of "decomposition" of features leads to many tasks which demonstrate single language features/properties, and entirely leave out languages which simply do not have those features -- and the whole point of RC is ''comparative'' programming, so this is undesirable; we should aim to have tasks which many languages can implement, ''using their own particular facilities''. --[[User:Kevin Reid|Kevin Reid]] 13:39, 4 July 2009 (UTC)
:::So Kevin, would a task of "Function definition/Function call semantics and examples" be too wide a task? You might have noticed that after my initial question above, I thought it might be best to write something that might cover all such tasks and linked the Python entries of the other two to this one.  --[[User:Paddy3118|Paddy3118]] 13:55, 4 July 2009 (UTC)
::::Such a task is a different kind of thing, "explain your language" rather than "implement this specification". I agree that such tasks should exist (e.g. I wrote [[Eval]] and [[Variables]]), but they should be considered separately from "implement this specification" tasks like [[Optional parameters]]. [[Named Arguments]] I would say falls into a third category, along with e.g. [[List Comprehension]]: "given that your language has this feature, tell us about your particular variation".
In the course of writing this comment, I have changed my position: that third category is in fact valuable, and named arguments are a significant enough feature that they should have a task which focuses on them (e.g. comparing Common Lisp vs. Python vs. Objective-C would be enlightening as to the variations possible).
I still think that there are too-narrow tasks: for example the basic loop tasks in [[:Category:Iteration]] are such because they assume a particular family of iteration facilities: once you've been that specific, there is no room for interesting variation between languages, just syntax. --[[User:Kevin Reid|Kevin Reid]] 15:12, 4 July 2009 (UTC)

==Autohotkey work-around limitations==
The limitations of the Autohotkey work-around should be stated. Immediately on seeing the entry I had to go to the other site to find buried in comments that limitations were stated there but not here on RC. --[[User:Paddy3118|Paddy3118]] 22:31, 11 July 2009 (UTC)

: I agree that they should be stated. After all, the whole point of the site is to allow people who know some languages to learn about others, and the limitations in one language's solution of a task is an important aspect. —[[User:Dkf|Donal Fellows]] 11:55, 12 July 2009 (UTC)

==REXX examples==
Do the REXX examples actually complete the task? Is it idiomatic REXX? do the docs for "normal" function calls in the language mention any of those solutions? Might it be better to just omit REXX? --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 07:26, 2 July 2014 (UTC)

: Yes, the REXX examples actually complete the task, although ''named parameters'' aren't normally used in REXX as it's a somewhat bulky method to do in the manner specified.   Most often, because of the manner the (original) host (operating system) passes arguments (parameters) to a REXX program, lends itself to specifying options (parameters) followed by a value (with no intervening equal sign, and the order of the arguments isn't important. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 23:59, 2 July 2014 (UTC)

: Yes, it is idiomatic REXX.   (That begs a question, must every solution have to idiomatic?)   REXX is a minimalistic language and many techniques aren't necessarily documented as solutions.   Being so minimalistic, there are often numerous ways to skin a cat (as the various language versions attest to).   Named parameters is a specific method of passing parameters in REXX (but hardly used as the manner of this Rosetta Code task).   Of what I've observed of the many REXX publications (from different vendors/authors), it isn't in the nature to mention specific types of solutions in function calling (invocation).   However, there are some excellent publications on various techniques in using the REXX language to address all manners of problems. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 23:59, 2 July 2014 (UTC)

: No, the REXX documentation(s) for function calls don't give solutions;   the various docs (that I've seen) for REXX show the syntactical format of functions (the built-in ones) and their various arguments, what the functions do, which arguments are optional (and can be omitted), and if any of the arguments have a default value.   Note that some BIF functions in REXX perform different things based on the number of arguments, or most often, specific options;   this is in its philosophy of a minimalistic language.   It isn't the nature of the REXX manuals that I've read to give solutions in how to use any particular function (BIF or user written), but to explain the language (syntax) of the (BIF) function and its arguments.   Also, I don't know what "normal" function calls are in this context, unless you mean BIFs instead of user written functions.   I wouldn't know what an abnormal function call would look like.   There are only two ways to call a function, explicitly via a '''call''' verb, or as a function.   Well, there is a third way, to perform/invoke/execute the function as if it was a "system/user command", and have the host (operating system) invoke the function/program as if it was a regular program (that returns a value, like a function).   I hope we won't be tripping up on the definition of routine versus a function, at least as far a REXX is concerned). -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 23:59, 2 July 2014 (UTC)

: I don't see why it would be better to (just) omit REXX, especially as there are two solutions posted.   I thought the whole point of Rosetta Code is to show how other languages solve a particular task, even if the solutions may not be understood, idiomatic, or look pretty. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 23:59, 2 July 2014 (UTC)

::Hi Gerard. Thanks for taking the time to give me your answers. I asked because I was unsure if most REXX users seeing the solutions would consider them "too far from the mainstream" to consider using. Your explanation of the kind of documentation, and what programmers might routinely have to consider when reading/writing REXX have put put me straight on that. --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 07:31, 3 July 2014 (UTC)

== Algol68 ==

The example does not compile on Algol 68 Genie 2.8:

```txt

a68g Named_parameters.a68 
28    print pet(())
                1  
a68g: error: 1: this vacuum cannot have row elements (use a REF [] UNION (OWNER, OPTBREED, OPTSPECIES, 
OPTNAME) generator) (detected in collateral-clause starting at "(" in this line).

```

But it works if the offending line is removed.
I cannot decide if this is a feature of algol 68 or a bug of the compiler.

: Curious.  It looks like a bug in the compiler to me, not least because other changes I made to the source to see what may be going on also gave unexpected results.  I'll contact Marcel.

: BTW, it doesn't "work" if the line is removed because although it compiles and runs, it doesn't print the defaults.
: --[[User:Brnikat|Brnikat]] ([[User talk:Brnikat|talk]]) 08:58, 11 August 2015 (UTC)
