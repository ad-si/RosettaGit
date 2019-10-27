+++
title = "Talk:Numeric error propagation"
description = ""
date = 2016-08-19T21:22:42Z
aliases = []
[extra]
id = 10186
[taxonomies]
categories = []
tags = []
+++

==Task updates==
Unfortunately I probably won't be able to track comments on this draft task for a couple of days as I will be making the most of a weekend with my beautiful partner without our kids. For what it's worth, I had hoped to make this task rather like [[Quaternion type]]. (Maybe I should change the name to "Error propagation type")? --[[User:Paddy3118|Paddy3118]] 02:59, 30 July 2011 (UTC)
==What about correlations?==
Even if error of a and b are fully independent, stuff like (a+b) and (a-b) would be (anti)correlated.  What to do with those? Ignore? --[[User:Ledrug|Ledrug]] 03:00, 30 July 2011 (UTC)
:I purposefully skipped the stats and just extracted some equations; hopefully enough to make an interesting task. I think it might be best to leave such extensions to the individual language implementors and have them make notes in their language entry? 
:I don't want to further expand the task description with talk of correlations unless their omission really offends those in the know. (University was a few decades ago for me and I haven't made much use of the subject since). --[[User:Paddy3118|Paddy3118]] 03:13, 30 July 2011 (UTC)
::As long as the task doesn't require operations between correlated errors it doesn't make a difference.  Otherwise each uncertainty would have to keep track of the error sources, which will become complicated real fast.  --[[User:Ledrug|Ledrug]] 03:22, 30 July 2011 (UTC)

== Not sure how to do this ==

Conceptually speaking: the new type is a number plus a function of a random variable.

But consider, for example: a*a.  Here, both factors depend on the same random variable.  We could express this particular example as a^2, because the same random variable is being used on both sides of the equation, but in the general case?

I am not seeing how to make this consistent, except by pushing this burden of analysis onto the programmer.  But that approach seems at odds with implementing this as a type.  --[[User:Rdm|Rdm]] 15:55, 31 July 2011 (UTC)

:They should be treated as independent for the purposes of this task, but see the Python solution where I have added a check for an object being multiplied by itself, which means having the same id() in Python (implemented as a check on the memory location occupied by an object in C-Python). --[[User:Paddy3118|Paddy3118]] 18:19, 31 July 2011 (UTC)
:: Don't do that.  This is the same question as the one I asked above about correlations.  For a variable, you either track all its error sources for correctness, or track none for simplicity.  Checking just one source is good for neither.  Consider:<lang>a.value = 10
a.error = 1.0
b := a * 2
c := a - b
```
How do you deal with the error at assignment of c and d? Checking memory location isn't enough.  If a variable has full error source information, a and b would have the same source of error but with different magnitude, so c.error == a.error, but a and b won't have the same mem location so you probably would think c.error = a.error * sqrt(5).  Checking identifier equality is not much more correct than tracking nothing, but more confusing and less consistent. --[[User:Ledrug|Ledrug]] 20:20, 31 July 2011 (UTC)
::: Ahh. Gotcha. I'll scrub that. Thanks. --[[User:Paddy3118|Paddy3118]] 01:12, 1 August 2011 (UTC)

==Equation writing==
I had to write some equations for this task but I always didn't like the way the TeX rendering always generated a PNG image that was too high (e.g. <math>\sqrt{2}</math>) to fit with the rest of the text on a line.

I don't think RC has the wikipedia math template for rendering: <nowiki>{{math|{{radical|2}}}}</nowiki>, so I can't tell what that would be like. Instead I used plain old HTML special characters and the Windows Character Map tool for this task. It is not very good at the micro-formatting so I have to also use more brackets than is usual to remove ambiguity, as well as favouring leaving in a square over using the &radic; character in one equation. 

P.S. I used [http://meta.wikimedia.org/wiki/Help:Displaying_a_formula#TeX_vs_HTML this] to help me. --[[User:Paddy3118|Paddy3118]] 07:47, 1 August 2011 (UTC)
:To each his own, of course, but TeX rendering has one crucial advantage: it's reliable and unambiguous, and can handle arbitrarily complex equations.  Wiki has 3 ways of displaying math: TeX png image, MathML, or for very simple stuff, normal text with sub-/superscripts.  I set minimum font size in firefox, so sometimes plain subscripts look as big as normal text. MathML depends on user having math font installed.  Currently only TeX can be relied upon to deliver equations accurately. If you want to reduce TeX math display size, there are hacks (be careful tho): <math>\scriptstyle\sqrt 2</math> --[[User:Ledrug|Ledrug]] 20:49, 1 August 2011 (UTC)

::I would have stayed with TeX if I could get it to not break up the line so much. I guess it works best on a line by itself. I know that TeX has great support and I will be using it more as there are problems with the HTML I used above that make it awkward for more complex equations.
::I guess I am new to TeX and could do with a GUI-based tool. (I wonder, Is TeX the language used by Open Office)? --[[User:Paddy3118|Paddy3118]] 05:29, 2 August 2011 (UTC)
:::Open Office equation editor can take TeX-like input, but I don't think it's using TeX engine internally.  TeX, being an (almost?) Turing-complete language itself, is not easy to embed. I don't really know for sure, though.  --[[User:Ledrug|Ledrug]] 06:07, 2 August 2011 (UTC)

==Negative uncertainties==
The original formula for exponentiation a^c allowed for negative uncertainties if a < 0. This does not seem to be meaningful, so I assume this always means the absolute value. I have changed the formula.

: Multiplication by a negative constant didn't seem right either so I fixed that.  Thanks. --[[User:Paddy3118|Paddy3118]] 12:44, 17 August 2011 (UTC)
:: It's not pertinent to current task, but a negative uncertainty does have meaning if correlation is considered.  If <math>a\equiv a_0\pm\sigma</math>, you can say <math>-a=-a_0\pm(-\sigma)</math>, and the sum <math>a + (-a) = 0\pm 0</math> (note how the <math>\sigma</math>s just add, not RMS). --[[User:Ledrug|Ledrug]] 12:58, 17 August 2011 (UTC)

==Do multiplication and exponentiation propagate differently?==
Do I misunderstand the formulae, or do multiplication and exponentiation indeed result in different error propagations?
If I look at the square of the uncertainties for a*a vs. a^2:
   f = a*a
      σ[f]^2 =
         f^2 * ((σ[a] / a)^2 + (σ[a] / a)^2) =
         (a*a)^2 * 2 * (σ[a] / a)^2 =
         a*a*a*a * 2 * σ[a]^2 / a^2

   f = a^2
      σ[f]^2 =
         (f * c * σ[a] / a)^2 =
         f^2 * c^2 * σ[a]^2 / a^2 =
         a*a*a*a * 2 * 2 * σ[a]^2 / a^2
Take a = 100 ± 2. For a*a I get an error of 282.84, while for a^2 I get 400.00. --[[User:Abu|Abu]] 10:01, 18 August 2011 (UTC)

:Yes, but this is at least in part a bug in the implementation of multiplicative uncertainty.  See also: [[Talk:Numeric_error_propagation#What_about_correlations.3F|What about correlations?]] and [[Talk:Numeric_error_propagation#Not_sure_how_to_do_this|Not sure how to do this]].  Since this question is so easy to ask, I am going to explicitly address it in the task description.  --[[User:Rdm|Rdm]] 10:38, 18 August 2011 (UTC)
:Indeed it's a correlation issue again.  Suppose <math>a = a_0 + \epsilon_a</math>, where <math>\epsilon_a</math> is a random variable with mean 0 and stddev <math>\sigma</math>; similiarly <math>b = a_0 + \epsilon_b</math>, then <math>a\times b = a_0^2 + (\epsilon_a + \epsilon_b) a_0 + O(\epsilon^2)</math>, whose standard deviation is the new error. If <math>\epsilon_a</math> and <math>\epsilon_b</math> are independent but with same stddev, then the <math>\epsilon</math>s are added by RMS and error is <math>\sqrt{2}a_0\sigma</math> (= 282); if they are the same variable (correlation = 1) such as in the case of  <math>a^2</math>, <math>\epsilon</math>s are directly added and error is <math>2a_0\sigma</math> (400).  As a side note, the <math>O(\epsilon^2)</math> will introduce a bias to the mean of the result, which is normally small and ignored.
:It's more work to keep track what's correlated with what, so for this task you can always assume the error terms are independent.  If errors are small compared to mean, this is often acceptible. --[[User:Ledrug|Ledrug]] 00:11, 19 August 2011 (UTC)

== Wrong values used in demonstration ==

I have just fixed the Java entry, which had been tagged as incorrect 2 months ago.  The problem was, that the demonstration code swapped the values for x2 and y1.  Unfortunately this gives the same distance (111.80) and only a slightly different error value, i. e. 2.94 instead of 2.48.
Several other languages have also swapped x2 and y1 in their demonstration with respect to the values given in the task details. As of today the implementations/demonstrations of the following languages still suffer from this: C++, D, <s>Java,</s> Kotlin, Scala. --[[User:PKai|pKai]] ([[User talk:PKai|talk]]) 21:08, 19 August 2016 (UTC)<br/>
Fixed Kotlin, too. But I have not installed the other languages, and I won't do any fixes, if I can't test it. --[[User:PKai|pKai]] ([[User talk:PKai|talk]]) 21:21, 19 August 2016 (UTC)
