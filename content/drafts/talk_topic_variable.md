+++
title = "Talk:Topic variable"
description = ""
date = 2014-04-11T19:18:11Z
aliases = []
[extra]
id = 12861
[taxonomies]
categories = []
tags = []
+++

Computing the cosine and rounding may be a relatively difficult task in some languages (esp. if the language does not have built in floating point handling), as opposed to setting the topic variable to a simple constant. If the task is just to demonstrate the use of the topic variable, then maybe simplify the task and remove the trigonometry and rounding requirements. I suggest leaving the task description simply as "Demonstrate the utilization and behaviour of the topic variable within the language",

[[User:Markhobley|Markhobley]] 13:01, 27 January 2013 (UTC)

: Related issues are "what is a topic variable?" and "what is a shortcut?" --[[User:Rdm|Rdm]] 14:11, 27 January 2013 (UTC)

:If a language does not have standard trigonometric functions, custom versions can be provided, for instance with a Taylor series  approximation or something.  Same for rounding.    For comparison purposes, I didn't want to be too liberal on how to show how topic variables can be used.  Thus the three standard functions.  It's a method that has been chosen for other tasks on RC, irrc.--[[User:Grondilu|Grondilu]] 15:37, 27 January 2013 (UTC)

:Might it be changed to x, 2*x and x*x for example? --[[User:Paddy3118|Paddy3118]] 16:14, 27 January 2013 (UTC)
::One algeabraic expression is OK but several of them are redondant.  Also I wanted at least one function call.  But I guess I could use sqrt instead of a trigonometric function.--[[User:Grondilu|Grondilu]] 23:00, 27 January 2013 (UTC)

:There is also useful information that could be included such as how the topic variable behaves under different levels of scope that might be usefully to be shown here. This comes under utilization and behaviour. It does not come under trigonometric functions. The trigometry, could be made optional, eg: optionally show how to calculate a cosine, or maybe rename task Topic variable/Trigonometry, to show that the task is really about the topic variable in a trigonometric context, rather than generalized. [[User:Markhobley|Markhobley]] 17:26, 27 January 2013 (UTC)
::The task is not about the topic variable in trigonometric context.  I just wanted an example of method or function call.--[[User:Grondilu|Grondilu]] 23:12, 27 January 2013 (UTC)

== Better definition for "topic variable" ==

Can someone give a better definition for "topic variable" than the current task description? Is a special variable which contains the result of the last expression (especially when used interactively/in a REPL) considered a topic variable? What if that variable is only available inside the REPL and not when a program is run as script/compiled? --[[User:AndiPersti|Andreas Perstinger]] ([[User talk:AndiPersti|talk]]) 11:09, 11 April 2014 (UTC)

:This question inspired the Pyuthon entry, and I see you have followed :-)
:I guess if someone thinks it's wrong, they'll flag it. --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 14:20, 11 April 2014 (UTC)

::I've read the [http://perldoc.perl.org/perlvar.html#General-Variables Perl] and [http://doc.perl6.org/language/variables#%24_ Perl 6] documentation about <code>$_</code> (the languages which started this task) and thus I would say at least Erlang, PARI/GP, Python, Standard ML and Unix Shell don't show topic variables (i.e. should be flagged as incorrect/omitted). AIUI <code>$_</code> is used as the default value for functions/language constructs if no argument is provided. But the mentioned languages show variables/consructs that store the most recent result.
::For example compare this Python session

```txt
>>> 1 + 2
3
>>> _
3
```

::or this Standard ML (SML/NJ) session

```txt
- 1 + 2;
val it = 3 : int
- it;
val it = 3 : int
```

::with this Perl 6 (Rakudo) session

```txt
> 1 + 2
3
> $_
Any()
```

::or this Ruby (irb) session

```txt
irb(main):001:0> 1 + 2
=> 3
irb(main):002:0> $_
=> nil
```

::There is obviously a difference between both types of special variables. That's why I would like to have a better definition in the task description. --[[User:AndiPersti|Andreas Perstinger]] ([[User talk:AndiPersti|talk]]) 19:18, 11 April 2014 (UTC)
