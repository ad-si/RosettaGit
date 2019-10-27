+++
title = "Talk:Truth table"
description = ""
date = 2013-12-05T21:17:58Z
aliases = []
[extra]
id = 10745
[taxonomies]
categories = []
tags = []
+++

==Inspiration==
Inspired by a mention of truth tables [[User:Mwn3d#Projects|here]] (Thanks Mike), which reminded me of an old [http://paddy3118.blogspot.com/2010/03/expressions-to-truth-tables.html blog post] of mine. --[[User:Paddy3118|Paddy3118]] 07:24, 31 October 2011 (UTC)
:Always glad to inspire :). --[[User:Mwn3d|Mwn3d]] 13:16, 31 October 2011 (UTC)

==Which operators?==
Just to get ahead of a possible question, which operators should this program support? And, or, and not are pretty much locks, but do we need implication operators? Xor? --[[User:Mwn3d|Mwn3d]] 13:16, 31 October 2011 (UTC)

:And, or, not as a minimum; but if the rest are just more of the same then they could be left out for brevity. --[[User:Paddy3118|Paddy3118]] 14:06, 31 October 2011 (UTC)

: From a website I found, the ^ boolean operator is an <tt> AND </tt> and almost all examples here seem to use it as as the <math> C </math> <tt> XOR </tt> operator. This same website didn't even mention (or use) the  &  [<tt>AND</tt>] operator. The list that I used is in the REXX example. I'll change it if the consenus say that's incorrect, since there seems to be a very heavy <math> C </math> influence in Rosetta Code.  I think it may be better to have consistency in the coding and examples. -- [[User:Gerard Schildberger|Gerard Schildberger]] 20:16, 28 April 2012 (UTC)

:: In C, <code>^</code> is the [[bitwise operations|bitwise XOR]] operator. --[[User:Kernigh|Kernigh]] 20:40, 28 April 2012 (UTC)

:: I've changed the <tt> REXX </tt> coding example to comply with <math> C </math>
:::They probably used ^ for and to go along with the <math>\and</math> and <math>\or</math> operators from discrete math. I think it's fine to use whatever standard you like as long as you note it. --[[User:Mwn3d|Mwn3d]] 00:07, 29 April 2012 (UTC)

::: I was wondering why the website I visited used a lowercase <tt>v</tt> --- it was modeled after the <math>\or</math> character (disjunction). Conjunction is the <math>\and</math> charcter. -- [[User:Gerard Schildberger|Gerard Schildberger]] 07:01, 30 April 2012 (UTC)

:: As for which boolean operators to support, the task description is (to me) pretty clear: boolean equation(s) are all or any boolean expression(s), whether or not your language of choice only supports two or three (or a few more). I've included all sixteen boolean operator names in the REXX language example; I hope the boolean operations are named correctly, there are many variants).  Also added is support for the TRUE and FALSE boolean values, as well as boolean conditionals (comparisons). -- [[User:Gerard Schildberger|Gerard Schildberger]] 06:24, 30 April 2012 (UTC)

:: If anyone thinks the REXX example is too long, I could delete some of the boxed comments and put them here instead. But I thought the boolean operator names needed listing. -- [[User:Gerard Schildberger|Gerard Schildberger]] 06:35, 30 April 2012 (UTC)

== Incorrect example ==

Why is the Déjà Vu example incorrect? AFAIK, it does exactly the same thing as the D and Go examples. --[[User:Gvx|Gvx]] ([[User talk:Gvx|talk]]) 14:35, 21 November 2013 (UTC)

:Hi Gvx, I took a look and it seems that all three are incorrect for the same reason. You could look at the history of the page and find what the first few examples do, but in this case I think the task description does state that it wants the input of an expression to be parsed and evaluated. Some languages have that built-in; for others there is the hint of the mention of reverse-polish or infix evaluators. 
:If marked incorrect (or left out completely), it signals that a correct examples is needed. --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 08:00, 22 November 2013 (UTC)
:[[Parsing/RPN calculator algorithm|This]] might help. --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 08:06, 22 November 2013 (UTC)
::I agree with what Fwend said below. It seems to me that this is actually two tasks: Parsing a boolean expression and Printing a truth table. There are several other tasks on RS that are closely related, like [[Queue/Definition]] and [[Queue/Usage]] &mdash; and I think it makes more sense to split this one than the queue task. I'll rewrite the Déjà Vu example if we can't convince you. :) --[[User:Gvx|Gvx]] ([[User talk:Gvx|talk]]) 21:17, 5 December 2013 (UTC)

==Incorrect title?==
We could also mark the title of this task as incorrect, because what on earth has building a (not very robust) parser have to do with truth tables.[[User:Fwend|Fwend]] ([[User talk:Fwend|talk]]) 14:55, 22 November 2013 (UTC)
:Most of the work might be in creating the parser if your language does not come with a suitable one. Other languages make that easy. The task title seems accurate to me? --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 05:40, 23 November 2013 (UTC)
