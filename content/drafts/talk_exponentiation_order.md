+++
title = "Talk:Exponentiation order"
description = ""
date = 2014-03-24T09:05:06Z
aliases = []
[extra]
id = 17400
[taxonomies]
categories = []
tags = []
+++

== Functions ==

So it has to be an operator? A "pow" function is not allowed? --[[User:Mwn3d|Mwn3d]] ([[User talk:Mwn3d|talk]]) 19:00, 18 March 2014 (UTC)

: No, and then no.   Er, I mean, it doesn't ''have'' to be, and yes, '''POW''' is allowed if there isn't an operator for exponentiation.   I have no qualms about including the '''POW''' function in any case.   But a function has no misinterpretation of what order the exponentiation is in, as in   '''pow(x,y)'''.   Of course, you could write   '''pow(x, pow(y,z))'''   (or something similar, depending on you or your language interpret the operator) to show this task's intention, but there is no ambiguity in function calls.   Whereas,   '''5**3**2'''   is swimming with vagueness. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 19:09, 18 March 2014 (UTC)

:: Why? Somehow people don't find "a - b - c" swimming in vagueness.  And in the languages I know, if there's an pow operator, 5**3**2 is always interpreted as 5**(3**2), just like in maths. --[[User:Ledrug|Ledrug]] ([[User talk:Ledrug|talk]]) 03:31, 19 March 2014 (UTC)

::: I also don't believe that   a-b-c   is vague, but then, subtraction isn't what is being discussed.   The ''why'' is simple to explain:   because not all computer languages treat multiple (or chained) exponentiation the same.   5**3**2   is not always interpreted as   5**(3**2).   If that expression would be treated universally the same, there wouldn't be a need for this Rosetta Code task.   I'm not going to pontificate whether or not which manner of evaluation is correct; the main thing I'm interested is how various computer languages evaluate the expression, and what value is produced.   It's not a matter of doing it "the right way", the way that a computer language does it, is the way it is.   To quote my niece, Jackie, it is what it is. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 03:56, 19 March 2014 (UTC)

::::"If that expression would be treated universally the same, there wouldn't be a need for this Rosetta Code task." Can you give an example of a place where it is not right associative? Because I precisely have trouble figuring out why there's need for this Rosetta Code task. --[[User:Spoon!|Spoon!]] ([[User talk:Spoon!|talk]]) 09:06, 19 March 2014 (UTC)
:::::The existing examples show a difference. The first Perl 6 example shows that 5**3**2 = 5**(3**2) and the REXX example shows that 5**3**2 = (5**3)**2. --[[User:Mwn3d|Mwn3d]] ([[User talk:Mwn3d|talk]]) 13:05, 19 March 2014 (UTC)

::::::Likewise, a - b - c is also "swimming in vagueness" if you might or might not be programming in an APL derivative, where pretty much everything is right associative, if I recall correctly.  --[[User:TimToady|TimToady]] ([[User talk:TimToady|talk]]) 21:16, 19 March 2014 (UTC)

::::::: Which is one reason to show examples.   Without examples of different computer languages, we'd have to resort to researching each computer language to find this information, and in the case of REXX (and probably other language docs), the documentation doesn't mention specifically the result of   a**b**c.   As soon as other REXX "derivatives" (in the sense that they were developed later), APL (+ derivatives), and other computer languages, more differences will show up. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 23:29, 23 March 2014 (UTC)

::::::::If the docs don't say anything about the order of evaluation, it's implementation-dependent, isn't it? Shouldn't you then mention for the REXX entry which implementation produced the output? --[[User:AndiPersti|Andreas Perstinger]] ([[User talk:AndiPersti|talk]]) 08:16, 24 March 2014 (UTC)

::::::::: No, it just means the docs aren't complete.   That's what documentation for language standards are meant for, to eliminate those differences between implementations of compilers (or interpreters).   Most differences have to do with the numerous file systems on various operating systems and their differing architecture.   For the CLASSIC REXX language, for what I've observed, there is much more testing to ensure that the various interpreters are consistent, whether or not the behavior is documented.   It's a bit easier, I think, because the REXX language is very minimalistic.   That is to say that differences don't creep in during the creation of another REXX interpreter.   As for which REXX implementation was used, I executed most of the REXXes that I have installed: PC/REXX, Personal REXX, all versions of Regina, R4, and ROO.   I have others, but they are somewhat of a pain in the neckhole to execute for numerous reasons.   But there are more REXX that I don't have, not the least of which are the object-oriented REXXes.   If there are any Classic REXXes that produce a different result, I'm sure other people will note the difference. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 09:05, 24 March 2014 (UTC)  

:When home computers where new and there seemed to be so many implementations of BASIC, associativity was all over the place. I found out from this task that Pythons naive reduce example doesn't give the 'proper' associativity for example. --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 07:09, 24 March 2014 (UTC)

::IMHO the Python <code>reduce</code> function has nothing to do with that task. It will always apply the given function on elements of the sequence from left to right, regardless of the function.
::And I don't think that task makes sense for languages without an exponentiation operator. --[[User:AndiPersti|Andreas Perstinger]] ([[User talk:AndiPersti|talk]]) 08:23, 24 March 2014 (UTC)

::: The Rosetta Code task is still in ''draft status'', so it could be changed if you think it's warranted.   I didn't think that there were that many computer languages that didn't have an exponential operator, so I didn't want to exclude them on that basis. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 09:05, 24 March 2014 (UTC)

== Task necessary? ==

The task [[Operator precedence]] which lists all the precedence and associativity rules for a language exists already. Since the order of "chained" exponentiations is simply determined by the associativity of the exponentiation operator I don't think this task adds any new information. --[[User:AndiPersti|Andreas Perstinger]] ([[User talk:AndiPersti|talk]]) 22:49, 23 March 2014 (UTC)
: I understand the most computer languages have associativity rules, but not all rules are easy to find (if at all) for all languages (see above for such an example).   One reason for tasks in Rosetta Code is to find the differences between computer languages in solving (or executing) different tasks.   There shouldn't have to be a need to do research in a computer language's documentation to find out the differences.   Not all associative rules (for multiple computer languages) are obvious or that easy to find. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 23:38, 23 March 2014 (UTC)
::But what about subtraction, division, modulo operation? Do you think there should be similar tasks for them too? If no, why not?
::My point is that all this information should be on a single page/task and not scattered on several subtasks. --[[User:AndiPersti|Andreas Perstinger]] ([[User talk:AndiPersti|talk]]) 08:09, 24 March 2014 (UTC)

::: I don't believe there is a need to see   '''b-c-d'''   (and other operators) and how it differs (if at all) among different computer languages.   However, if someone thinks there is a need, and/or they create a Rosetta Code task, they won't have an argument from me.   I'll probably never question the need for the creation of any Rosetta Code task, even if I think it's too trivial.   I won't play the Rosetta Code cop who says this or that task doesn't have merit.   ''Exponentiation order'' is the one thing that isn't 100% consistent (among different computer languages), nor is it even covered/discussed in almost all of the (older) language manuals I'm familiar with.   The fact that there are different interpretations of what   '''5**3**2'''   means the Rosetta Code was worth entering. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 08:35, 24 March 2014 (UTC)
