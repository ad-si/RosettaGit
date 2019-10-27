+++
title = "Talk:Boolean values"
description = ""
date = 2013-02-20T19:54:53Z
aliases = []
[extra]
id = 4492
[taxonomies]
categories = []
tags = []
+++

Python twice?
:It's already been noted. a merge is in the works. --[[User:Mwn3d|Mwn3d]] 20:20, 10 July 2009 (UTC)
Sorry about the wait. All done. --[[User:Paddy3118|Paddy3118]] 21:12, 10 July 2009 (UTC)

== Rename page? ==

This page only deals with the two-valued boolean algebra.

This leaves out some important history and the full scope of what the word "Boolean" means.  This is a distressingly common practice.

See, for example:

http://sumon3get.hubpages.com/hub/Basic-Concept-And-History-Of-Boolean-Algebra

http://mathworld.wolfram.com/BooleanAlgebra.html

Remedies might include:

1. A new page title that somehow incorporates the phrase "two valued".

2. A new Rosetta code task which treats some other subset of boolean values.  For example, here's a table the boolean operation which corresponds to "logical and" on the integers 0 through 3:


```J
   *. table i. 4
┌──┬───────┐
│*.│0 1 2 3│
├──┼───────┤
│0 │0 0 0 0│
│1 │0 1 2 3│
│2 │0 2 2 6│
│3 │0 3 6 3│
└──┴───────┘
```


(this operation is "least common multiple",).

And here's the boolean operation which corresponds to "logical or" (greatest common divisor) on these integers:


```J
   +. table i. 4
┌──┬───────┐
│+.│0 1 2 3│
├──┼───────┤
│0 │0 1 2 3│
│1 │1 1 1 1│
│2 │2 1 2 1│
│3 │3 1 1 3│
└──┴───────┘
```


Also, I cannot find any treatment of "logical not" in older treatments of boolean algebra.  It has been added to newer works, but in that context it seems to be an arbitrary operation (it might be 1-x, or it might be any of a wide variety of other operations).
--[[User:Rdm|Rdm]] 13:40, 30 April 2012 (UTC)
: Using the Mathworld page you cited, which LCM as "and", and GCD as "or":
:* Eqs. 1-8 are sastified;
:* For eqs. 9-12, we have LCM(a, Ø) = Ø and GCD(a, Ø) = a for all a; only viable candidate for Ø is Ø = 0.  Similarly, LCM(a, I) = a and GCD(a, I) = I means I = 1.
:* for (13), LCM(a, ¬a) = Ø = 0, given any finite non-zero a, ¬a has to be 0;  but this leads to a problem in (14): now GCD(a, ¬a) = GCD(a, 0) = a, not I.
: It would seem plain GCD and LCM don't make good Boolean operators. You'd have to map numbers to sets to have a viable definition. --[[User:Ledrug|Ledrug]] 22:54, 30 April 2012 (UTC)

:: GCD and LCM work just fine as boolean operators if the set you are dealing with is non-negative integers.  Using the set of integers can also work with a minor clarification about the sign of least common multiple (for example: the result of lcm has the same sign as the product of the two numbers).

:: But it was Shannon that placed the emphasis on restricting focus in boolean algebra to the two-valued case.  Boolean algebra on integers was established well before this.  And, nowadays, because so many people have been working with computer languages which use "boolean" as a synonym for "two valued boolean", we are losing track of what boolean algebra is.  The reference works (such as the ones I cited) focus almost exclusively on the "logic value" case, but shy away from the issue of what distinguishes "logic" from "boolean algebra".  --[[User:Rdm|Rdm]] 13:06, 1 May 2012 (UTC)

::: Eh I didn't say anything about two-valued system.  Go through literatures and you'll see that LCM and GCD can make a Boolean algebra on ''some'' bounded sets like {1, 2, 3, 6} (all divisors of 6); on the set of all positive integers (or non-negative integers, or all integers) it doesn't work because you can't have a consistent complement/NOT operator, as I've shown above.  You can forgo the NOT operator and universal bounds, and call the rest "Rdm Algebra", or "Ledrug Algebra" if you don't like it, but it's not Boolean: Boole intended to formalize thought process and decision making, thus lacking a negation operation makes it singly useless in this regard.

:::: The complement only exists in two-valued boolean algebras.  It is not a characteristic of all boolean algebras.  https://en.wikipedia.org/wiki/Boolean_ring#Notations --[[User:Rdm|Rdm]] 19:36, 1 May 2012 (UTC)

::::: The complement is not required for Boolean lattice or Boolean ring, but it is required for Boolean ''Algebra'', see the same page you linked. It's besides the point anyway, the two value system is so well established that it's hard not to think of it when "boolean" is mentioned. --[[User:Ledrug|Ledrug]] 19:55, 1 May 2012 (UTC)

:::::: Quoting that page: ''There are at least four different and incompatible systems of notation for Boolean rings and algebras.  ... The old terminology was to use ... "Boolean algebra" to mean a Boolean ring with an identity.''  --[[User:Rdm|Rdm]] 20:03, 1 May 2012 (UTC)
::::::: Yes, and read on: ''Also note that, when a Boolean ring has an identity, then a complement operation becomes definable on it'' --[[User:Ledrug|Ledrug]] 20:06, 1 May 2012 (UTC)
:::::::: Except: that statement is provably false.  Consider the ring I have been using as an example:  GCD has the identity value 0.  LCM has the identity value 1.  But no complement can exist except in the two-valued case.  --[[User:Rdm|Rdm]] 20:21, 1 May 2012 (UTC)
::::::::: Er, no: 0 is a lousy identity for GCD. What's GCD(0, 0)? --[[User:Ledrug|Ledrug]] 20:34, 1 May 2012 (UTC)
:::::::::: GCD maps to Logical OR, and LCM maps to Logical AND when 0 maps to false and 1 maps to true.  GCD(0, 0) is AND(false, false).   Note that we are defining LCM and GCD to satisfy the constraints of Boolean Algebra (in the sense of a Boolean Ring with Identity), and this drives the definition in the cases which would otherwise be undefined.  --[[User:Rdm|Rdm]] 20:47, 1 May 2012 (UTC)
::::::::::: 1 doesn't work as indentity for GCD either: GCD(1, x) = 1, not x. 

:::::::::::: Yes. Ouch.  My last edit corrected a non error.  I was in a hurry and not paying attention.  Rewinding (and reverting part of my last edit).  GCD(0, 0) must be 0, by definition based on the rules of Boolean Algebra.  The point here is that we define GCD to satisfy the rules of the algebra.  --[[User:Rdm|Rdm]] 21:19, 1 May 2012 (UTC)

::::::::::: Anyway, this is quickly growing into a pissing contest, and I should stop here.  Let me just reiterate my position on this task: I think it's fine to keep the title as is, but it's reasonable to give a brief mention of the broader sense of the word "Boolean" if you feel like; if you want to make a new task about general Boolean algebra, make sure your example is waterproof. /zip my mouth on this subject now.--[[User:Ledrug|Ledrug]] 21:08, 1 May 2012 (UTC)

:::::::::::: Ok, I will think about creating a page for that task.  Thanks.  --[[User:Rdm|Rdm]] 21:19, 1 May 2012 (UTC)

::: As to Shannon "dumbing it down" (I guess you'd rather put it this way) to two values, again, since it's about decisions, making the results always "yes" or "no" is at least practical.

:::: Huh? --[[User:Rdm|Rdm]] 19:36, 1 May 2012 (UTC)

::: More to the point of the task, though, is that I think the task is fine as is; within the context of computers, it's not wrong to think "Boolean values" means "true or false".  If you want to introduce another task on Boolean algebra in the more formal mathematical sense, that's fine too, just make sure your introductory example is correct. --[[User:Ledrug|Ledrug]] 17:45, 1 May 2012 (UTC)

:::: My point is that I think we should acknowledge that the distinction exists. --[[User:Rdm|Rdm]] 19:36, 1 May 2012 (UTC)

: I think it'd be fine to rename this page to [[Binary values]]. It's clearly not specific to Boolean logic or algebra, and those concepts could warrant their own task (or set of tasks). Of course, [[Binary values]] would need to be disambiguated from [[Binary represenatation]], but I think that would be fine as well. --[[User:Short Circuit|Michael Mol]] 19:15, 1 May 2012 (UTC)
::I think the name is fine the way it is. When people are programming or learning about programming and they think "boolean" they are usually thinking about "true" and "false" or whatever their language calls them (unless they are actually working on these Boolean algebra concepts). It might be worth noting on this page that there is an expanded mathematical concept like this, but the title of this page is exactly what the programs on it represent: the computer concept of Boolean types. --[[User:Mwn3d|Mwn3d]] 19:49, 1 May 2012 (UTC)
:::The issue here is that some computer languages have deep ties to mathematics.  Mathematica, Haskell, and Coq are examples.  So if we have a computer term which looks like a Mathematical term, and we have computer languages designed to deal with mathematics, and if we are illustrating the concept on this site without even mentioning the distinction, we are fostering confusion about terminology.
:::We do not have to rename the page -- that was just a suggestion -- but I think we do need to mention that we are using the redefined version of the term.
:::Note also that we do have tasks here which focus on mathematical issues.  --[[User:Rdm|Rdm]] 19:54, 1 May 2012 (UTC)

:::: I reread the title and task description and took a look at the Wolfram article and the wp article and I think it would be difficult for someone to confuse the meaning of this page and what is required to solve it. If a mathematician is used to many valued logics then the fact that only two values are mentioned in the description should be enough to set them straight (although it would be odd that the mathematician not know that the two valued set is the default in most computer languages). I would go along with those that want to keep this description but who are not averse to seeing a task in other-valued Boolean logic. (Being obscure, this other task may need careful explanation). --[[User:Paddy3118|Paddy3118]] 12:11, 7 May 2012 (UTC)

::::: This is not about "many valued logics".  If you want to deal with "many valued logics" you might be using a container which holds multiple logical values.  Or, you might be using "fuzzy logic" where logical values are represented using a fractional value in the range [0..1] instead of a single bit.

::::: The confusion I am concerned with is the linguistic pressure to treat "Boolean" as equivalent to "Logical".  That's a valid shorthand in the context of the two valued boolean algebra.  But it loses track of what makes boolean algebra "boolean" in the first case.

::::: And this confusion happens a lot.  Every time someone says "Integers are not Booleans" they are expressing this confusion.  And that statement is distressingly common even among people with PhDs with a focus on type theory.  --[[User:Rdm|Rdm]] 13:25, 7 May 2012 (UTC)
