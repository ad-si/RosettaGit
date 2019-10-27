+++
title = "Talk:Averages/Simple moving average"
description = ""
date = 2015-10-27T12:48:34Z
aliases = []
[extra]
id = 4412
[taxonomies]
categories = []
tags = []
+++

==Numerical soundness==

The implementations which use or permit floats and keep a running sum which is added to and subtracted from, if given nontrivial numbers, will eventually have the sum drift away from the actual value because floating-point arithmetic is nonassociative ((((a + b) + c) + d) - a is not necessarily equal to (((a - a) + b) + c) + d = (b + c) + d); should this be considered incorrect, or warned about? --[[User:Kevin Reid|Kevin Reid]] 14:51, 20 June 2009 (UTC)

:Hmm, and wouldn't it potentially loose less precision if you sorted on absolute values and summed from the smallest absolute value up? We could hope that people read the talk page before using code snippets? --[[User:Paddy3118|Paddy3118]] 16:07, 20 June 2009 (UTC)

:: I suppose in a "moving average" you can't sort operands, unless you store them, and doing so has not so good implications. I suppose also that investigating the limits of IEEE floating point is not the focus of the task. Users run code on RC at their own risk! --[[User:ShinTakezou|ShinTakezou]] 15:27, 21 June 2009 (UTC)
::: Rosetta Code is not a code snippet repository; Code snippets shouldn't be placed here under the expectation that someone will use them ''verbatim''.  Pointing out the implications of floating-point error (or any other error caused  by underlying tools) as it relates to the task or problem area is well within the ''educational'' nature of the site, and is a reasonable thing to do when notice to avoid harming people who use the code without sufficiently understanding what it does. --[[User:Short Circuit|Short Circuit]] 19:36, 21 June 2009 (UTC)
:::: Of course, it's worth noting. (And by "you can't sort operands..." I meant "you can't sort all the operands, unless you store them all" of course...:D) --[[User:ShinTakezou|ShinTakezou]] 23:01, 22 June 2009 (UTC)
::::: If you're only maintaining the last <math>n</math> values, sorting isn't such a big issue and the size of the error is likely to remain small (a few ULP in the result if all values are positive, not a problem if they are all of the same sort of magnitude). However, it does require keeping those <math>n</math> values around and recalculating the sum each time rather than adding and subtracting from a running total. Not very onerous for <math>n=5</math>. In fact, it's only really a problem for people who are being too clever by half… –[[User:Dkf|Donal Fellows]] 06:35, 6 February 2010 (UTC)

==Autohotkey takes average of all previous==
-Ut should be a sum of the last N items, not all previous items. --[[User:Paddy3118|Paddy3118]] 04:02, 22 June 2009 (UTC)

==Lua problem==
The lua entry has a problem. A moving average is of the last N items if N is 5 for example then it is an averaage of the ''last'' (no more than) 5 items. When a sixth item comes along you must drop the first and average the last five, i.e. the second to the sixth items. I was going to say check the wp link given, but it seems to be obfuscated by its terminology - oh well. --[[User:Paddy3118|Paddy3118]] 06:40, 5 February 2010 (UTC)
:Fixed, my good man. Task description could be clearer, I guess.
Thanks for looking in on this problem. Looking at the update, the routine seems to have a fixed period of 10. The idea is to '''call a routine, that would produce your routine''', in effect. I.e. I call a routine and say I want it to produce an SMA with period 10 and it returns a routine that will compute a simple moving average with period 10; I call a routine and give it a period of 22, and it produces a routine that I can use independently to generate a simple moving average of period 22. It is the action of I() the initialiser in the updated task description. I explained the action of the initialiser without writing pseudo-code as the initialiser would be different for an OO, class based implementation, and a procedural implementation for example. --[[User:Paddy3118|Paddy3118]] 03:50, 7 February 2010 (UTC)

==PL/I and problem description==
:''"Then the description of the problem is incorrect. The description says that it can be implemented with an initializer, but there is no requirement that it must be. Naturally, the period P is fixed. That is a requirement in order to be an SMA of the last P items."''
(The comment above was moved from the page).

I think a reading of the full task description, and maybe a glance at other solutions should give an idea of what is expected but I will look again at the task description to see what can be done to aid you. --[[User:Paddy3118|Paddy3118]] 04:01, 26 March 2010 (UTC)

:Hi, hopefully the changes to the task description have clarified things. --[[User:Paddy3118|Paddy3118]] 04:11, 26 March 2010 (UTC)

:: Please add the framework (Proc Options(main)) to show how this could be used.--[[User:Walterpachl|Walterpachl]] ([[User talk:Walterpachl|talk]]) 09:54, 30 January 2014 (UTC)

== J Alternate Implementation ==

Here is a streaming variant of the J implementation, with a description:


```J
lex =:  conjunction define(dyad define)
    n__x=.1|.!.y n__x
    (+/%#)(#~1-128!:5)n__x
  )
  a=.cocreate''
  n__a=.m#_.
  a&v
)
```


The "inner" (most indented) definition is a verb which takes two arguments:  A namespace (<code>x</code>) and a number (<code>y</code>).  In the inner definition  <code>n__x=.1|.!.y n__x</code> shifts the number <code>y</code> into the list named <code>n</code> in the <code>x</code> namespace.  Then, <code>(+/%#)(#~1-128!:5)n__x</code> removes all NaN values from the list and finds the average of the values that remain.

The "outer" definition here is a conjunction which takes two arguments:  A number (<code>m</code>) and the verb (<code>v</code>) defined above.  In this outer definition, <code>a=.cocreate<nowiki>''</nowiki></code> defines a new, empty namespace <code>a</code>.  Then, <code>n__a=.m#_.</code> populates the name <code>n</code> in that namespace with <code>m</code> NaN values.  Finally, we curry the verb <code>v</code> with this namespace and return the derived verb.

--[[User:Rdm|Rdm]] 18:04, 7 June 2010 (UTC)

==Some confusion over interpretation==
The text states "create a ... that takes a period and returns a routine that ..." which seems to me to require the generation of the source code of a function that will perform the simple moving average. As distinct from devising a function that calculates a moving average. In a system with a pre-processor as a part of the language (as with pl/i) the preprocessor procedure would be given a period P (such as 3) and would generate the pl/i source for a function sma3(v), and on another invocation such as 5 would generate the source for sma5(v) and so on. Pl/i also allows alternate entry points to functions/procedures, so these could be supplied to enable the initialisation of a given SMA''n'' routine, so that each different size could be employed separately and restarted at will. I am supposing that each routine would be invoked with successive values to average, as in <code>for i:=1:20 do sma3(sqrt(i));</code> or similar. If alternate entries are unavailable, then each SMA''n'' could be used on only one sequence during the life of a prog., unless some global variable can be adjusted to cause a re-initialisation.

Alternatively, and especially for compiler-based systems, a compound routine is intended, with multiple parameters that enable the specification of a period P and the initialisation of such a summation's internal storage, followed by the presentation of a datum for a particular period P (presumably, two parameters: the chosen P and the value to be averaged) with potentially many different averages (each with a distinct P) going on together as further data are provided...

Alternatively, the plan would be to devise a routine SMA that on its first invocation is with P; the routine allocates internal memory to save values between the second and successive calls that will follow so as to provide the moving average of order P. Such a routine could not be used on two independent streams of data, and the initial P cannot be changed.

Alternatively, provide the auxiliary storage as a second parameter, with different such parameters for each summation (different values of P, and multiple summations for the same P). But this means that the routine itself contains no state information internally.

So, I'm confused... [[User:Dinosaur|Dinosaur]] ([[User talk:Dinosaur|talk]]) 07:52, 27 October 2015 (UTC)

: I think any of your proposed interpretations should be acceptable, given the current task description. --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 12:48, 27 October 2015 (UTC)
