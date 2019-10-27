+++
title = "Talk:Vector products"
description = ""
date = 2019-07-05T22:36:51Z
aliases = []
[extra]
id = 9426
[taxonomies]
categories = []
tags = []
+++

== what is meat by ''function''? ==
What specifically is meant by 'function'? Would 'routine' or 'relinkable code' suffice? I recall that there was some difficulty in another task where there was a hang-up on another task about how languages could implement code asking for the implementation and demonstration of utility code. --[[User:Short Circuit|Michael Mol]] 15:48, 9 April 2011 (UTC)

:Erm, "''A named ecapsulation of code with parameters, that can be called with arguments and may return a value''"? I've expanded the task description to encompass the main names used: function, method and subroutine. --[[User:Paddy3118|Paddy3118]] 16:58, 9 April 2011 (UTC)

:'Function' is a sufficient term. 'Routine' would work, but is not better than 'function'. 'Relinkable code' might be a library, not a function. A function can be a [[Ruby]] method (or block), a [[dc]] string, or a [[Factor]] word (or quotation). --[[User:Kernigh|Kernigh]] 00:25, 10 April 2011 (UTC)

::Hi Kernigh, I too thought function would be OK but I have expanded in the past too. I am after something with a name, so lambda functions or unnamed blocks would '''not''' be best. --[[User:Paddy3118|Paddy3118]] 06:32, 10 April 2011 (UTC)

: FWIW, when someone asks for a function I'm usually happy to give a procedure in any Tcl solution I write. (Occasionally, I'll make it into a full function if the problem is particularly mathematical – there's a small technical distinction to Tcl programmers – but the difference is really slight.) The key distinction is that it is named, reasonably encapsulated, takes arguments and produces a result (or results). Most programming (and all practical ones) are happy with doing something that matches this. ([[Brainf***]], [[SNUSP]] and [[Piet]]… well would you write a big app in them? Really?)
: WRT “relinkable code”, that makes me think of “library”, which is really the next major level of organization up. I'd be surprised at someone using a shared library like this; it would be so slow, and as fragile as the overlay mechanism that used to exist in old DOS programs back when “640kB was enough for anybody” (except it wasn't). Let's not encourage anyone to go there; it was ''very'' painful. –[[User:Dkf|Donal Fellows]] 22:19, 10 April 2011 (UTC)

== "create" ... ==

IDL has builtins for most of these, or at the most would come to something like <tt>total(a*b)</tt>. Would it be acceptable to just show how this would be done there or do you really want me to "create a function" that merely returns the result of a one-liner?[[User:Sgeier|Sgeier]] 04:56, 8 June 2011 (UTC)

== Rigid requirements ==

Task requirement 3 and 4 should be dropped.  Dot and cross products are fundamental, while the triples are not, they are just combinations of the former two. --[[User:Ledrug|Ledrug]] 03:42, 2 August 2011 (UTC)
:True, they are not fundamental; but they are common. --[[User:Paddy3118|Paddy3118]] 05:42, 2 August 2011 (UTC)
:: I meant those don't have to be implemented as functions.  If using operator overloading, they are naturally done by dot and cross, defining their own functions is counterintuitive.  I have no problem as to requirement 7 and 8 without the "their function" part. --[[User:Ledrug|Ledrug]] 06:02, 2 August 2011 (UTC)
:::Makes sense. I edited the task description to make the function optional. Thanks. --[[User:Paddy3118|Paddy3118]] 07:00, 2 August 2011 (UTC)
