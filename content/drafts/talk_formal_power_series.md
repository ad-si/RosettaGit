+++
title = "Talk:Formal power series"
description = ""
date = 2010-02-06T13:25:34Z
aliases = []
[extra]
id = 2797
[taxonomies]
categories = []
tags = []
+++

== Is it possible for non-functional language? ==
As alway, I misunderstood the task, the sin & cos is not defined by Integral of each other, but by explicit series definition.

I just wondered if non-functional language can accomplish the task. Is my examples totally go the wrong way, or 1 step away from goal? -- [[User:Badmadevil|badmadevil]] 02:49, 6 April 2008 (MDT)
: You beat to me flagging it as incorrect :-) And the key is lazyness, which in principle should also work in a non-functional language. As D doesn't seem to support it directly, you'll have to emulate it: Each coefficient should be a class that either already has a concrete value (an infinite precision rational, if available), or contains a call to a generator that can calculate the value. The first time the value is required, the generator is called, the next time, the cached value is used ("call-by-need"). That will also get rid of the inefficiencies in your implementation when do e.g. multiply several series (which causes the same coefficients to be calculated many times). And you need an infinite list of coefficients, which can be done with the same trick for a cons-cell. Yes, it's much nicer if the language does this naturally, which is the point of this task :-) --[[User:Dirkt|Dirkt]] 03:09, 6 April 2008 (MDT)
::I found it can be done lastly. I struck at how to define the two object at the same time, which is not need. Thanks -- [[User:Badmadevil|badmadevil]] 03:30, 6 April 2008 (MDT)
::: I've seen you added an extra indirection, but I don't see how you handle "call-by-need", i.e. when you replace that indirection with the real value. So does it? But maybe my D guessing skills are not sufficient. --[[User:Dirkt|Dirkt]] 04:14, 6 April 2008 (MDT)
::::Is ''indirection'' mean the ''interface'' which called like a class? I think it is one of the D ways to do ''closure''. Actually I am still wondering how it work. -- [[User:Badmadevil|badmadevil]] 04:32, 6 April 2008 (MDT)
::::: It's the extra member ''term'' of type ''UT''. I don't understand D enough to say why you need an ''interface'', or if you need one at all. I only understand that the usual way to implement lazyness is to use such an extra variable. And yes, it's related to closures -- the call to a generator mentioned above can also be a closure. As I said, I don't understand D well enough, but it still doesn't look like I would expect it. In particular, you still seem to have the efficiency problem (try multiplying three series, and insert some debugging code that shows you when a coefficient gets calculated. You should see that the same coefficient gets calculated multiple times, which means a '''lot''' of times at higher numbered coefficients. --[[User:Dirkt|Dirkt]] 05:36, 6 April 2008 (MDT)
::::::The ''term'' is an interface, which as previously state, may think as a class object. In solving the efficiency problem, I think the interface can be expanded to a proper class object, which has at least 2 member function. One is the coefficient generator, other is cache function to access an cache array storage. The cache function will query the generator if the coefficient is not inside the cache array, and of course, after return from the generator, the cache array will be updated. It is a trade off of time and space. I will try to implement this structure later if not too complicated. -- [[User:Badmadevil|badmadevil]] 06:35, 6 April 2008 (MDT)

BTW, I only mentioned the rationals as a "nice to have" feature (because then you can immediately see if the coefficients for sine and cosine are correct). If you need an extra module that implements the rationals from scratch, and if you don't have arbitrary precision anyway, I personally would prefer ''double''s. The task leaves this detail unspecified on purpose. --[[User:Dirkt|Dirkt]] 05:36, 6 April 2008 (MDT)
:Yes, it is overkill to implement from scratch, it is strict forward though.-- [[User:Badmadevil|badmadevil]] 06:35, 6 April 2008 (MDT)

==Multiplication and division==
Could someone post examples of correct multiplication and division of power series, and perhaps even an explanation of the correct algorithm? The Haskell code is invalid, and I just wrote a translation of the D/Java code, which gives wrong results: multiplying 2 by 1 yields 3 + 3x + 3x^2 + ... --[[User:Kevin Reid|Kevin Reid]] 00:04, 17 February 2009 (UTC)

: I hope multiplication of the Ada solution is correct. As for division, it is impossible to implement because the result can infinite (example: 1-''x'') or non-existent (example: ''x''). --[[User:Dmitry-kazakov|Dmitry-kazakov]] 15:14, 10 March 2009 (UTC)

:: Division introduces two problems.  One problem has to do with remainders (as in the 1/(1-x) or 1/x cases suggested by [[User:Dmitry-kazakov|Dmitry-kazakov]]), and the task specification did not specify how they should be treated.  A related but deeper problem is that the quotient is not knowable unless every element can be inspected (or treated in some symbolic fashion).  In other words, the Kth element of the result depends on the Jth element of the divisor and the (J+K)th element of the numerator for arbitrary K and J.  In the general case, significant examples of both J and J+K can be infinite for any finite K.  This problem becomes tractable when we deal with finite sequences (which, in essence, is what lazy evaluation gives us, though without the tractability).  --[[User:Rdm|Rdm]] 19:32, 22 January 2010 (UTC)

It is like normal multiplication of polynomials, but done on (potentially) infinite elements (we shall truncate the series anyway at some point...) It is enough to do the right grouping according to the ''power'', so say you have (a<sub>0</sub> + a<sub>1</sub>x + ...) and (b<sub>0</sub> + b<sub>1</sub>x + ...) then you do what you would normally do: a<sub>0</sub> with all the b<sub>i</sub>x<sup>i</sup> and so on; but of course you must group equal powers. You could write it shortly as

<math>p_n = \sum_{i=0}^n a_ib_{n-i}x^n</math>

About division, it is possible: it should be enough that at least one ''coordinate'' is not zero. Of course, if it makes sense or not it still depends on the value where we shall compute the ''function''. The procedure (at least, done by hand...) is the same as for the multiplication. Then you compare the powers and find the right coefficients. On the fly I am not able to write it in a general form that is useful in order to write a computer algorithm, anyway it should be a start point (''a'' are the coeffs of the dividend, ''b'' of the divisor and ''c'' of the quotient):

<math>a_0+a_1x+a_2x^2+\dots = c_0b_0 + (c_0b_1+b_0c_1)x + (c_0b_2+c_1b_1+c_2b_0)x^2+\dots</math>

This is simply the multiplication, hence you obtain

<math>c_0=\frac{b_0}{a_0}</math>

<math>c_1=\frac{a_1-c_0b_1}{b_0}</math>

where you can substitute c<sub>0</sub> from the previous expression.
Apparently this gives inconsistent results when a coefficient is zero; but this is just because of this is not the right generalized formulation. You can try yourself with something like

<math>\frac{\cos x}{\sin x}=\sum_{i=0}^\infty c_ix^i</math>

expanding cos/sin as you know. Then you will see that having zero coeffs is not really a problem; doing it by hand is easier than thinking about a general algo that should work for every ''situation'' &mdash;I'm thinking about but not this wine-poisoned-evening (likely someone else more talented has already done it, but there's no fun without selfmade (re)discovering). --[[User:ShinTakezou|ShinTakezou]] 22:45, 10 March 2009 (UTC)

:No, it does not work. As I said above, divide 1 by ''x'':
<blockquote>
<math>\frac{1}{x}</math>
</blockquote>
:It just does not have Taylor series in ''x''<sub>0</sub>=0. As for the method you refer to. It is based on solving a linear equations system. The system for 1/''x'' does not have a solution (''a''<sub>0</sub>=1, ''b''<sub>0</sub>=0). That's it, it could not be otherwise. In other cases, like also mentioned above
<blockquote>
<math>\frac{1}{1-x}=\sum_{n=0}^\infty x^n</math> (sum of a geometric progression |x|<1)
</blockquote>
:the solution is infinite. And we ignored the convergence issue, so far. --[[User:Dmitry-kazakov|Dmitry-kazakov]] 09:36, 11 March 2009 (UTC)
:: There are several real problems. One is that we can't expand in 0 if the function (or any of the derivative) diverges in 0. Instead of sin, use the 1/x function itself to find its coefficients "directly" the same way you use for sin... Another problem is that we expand in powers of x where the exponent is positive, so it exists no expansion for x<sup>-1</sup>. We should drop Taylor series and use Laurent series (always with care I suppose).  If we can "generate" the Taylor series in 0 for r(x), being r(x) = f(x)/g(x), then we can divide the expansion of f(x) (its formal power series) by the one for g(x). There are still problems anyway; e.g. if it exists N so that f<sub>i</sub> = 0 for i>N, and similar for g<sub>i</sub> but with M, and N<M, what happens? (Similar to 1/x case I suppose), at a glance this situation gives problem too. But this still does not say that division is '''always''' impossible. It is "sometimes"! --[[User:ShinTakezou|ShinTakezou]] 19:15, 11 March 2009 (UTC)

:::Yep, the task name is misleading. When it says "formal series" that gives an impression of some generalized framework for dealing with more or less '''any''' series, Fourier, Chebyshev, not just Taylor ones. Laurent series is yet another story. It goes in direction of approximations by rational polynomials, Padé etc. So what is the task about? The example of a definition of cos-sin has almost nothing to do with either series or approximations and how they are dealt with in real world. (I addressed this issue in a subthread.) --[[User:Dmitry-kazakov|Dmitry-kazakov]] 09:44, 12 March 2009 (UTC)

:::: I am trying to understand better, and it's why I still haven't added code: to me one thing is to "handle" a formal power series (big but still finite array for a computer language?), another thing is to ''generate'' a formal power series. The first can be accomplished without caring too much, we need only to put constraints over some coefficient for operations like / (maybe, not sure, enough the constant is not zero for the division? ... anyway still problems, since we indeed manipulate "finite" series, can arise, but maybe the task asks to disregard these details). I admit I've not taken a deep look to posted codes yet. --[[User:ShinTakezou|ShinTakezou]] 13:37, 13 March 2009 (UTC)

==About solving equations==
The task seems incomplete without defining cos as:

<math>\cos x = \frac{-d^2}{dx^2} \cos x</math>

assuming that d/d''x'' denotes the differential operator. The task uses the integral operator instead and sin''x'' as a temporal. Shouldn't it work with '''any''' operator defined defined in the task, in any combination of? --[[User:Dmitry-kazakov|Dmitry-kazakov]] 10:32, 17 February 2009 (UTC)
:But also <math>\sin x = \frac{-d^2}{dx^2} \sin x</math>; and in fact any linear combination of sin and cos will satisfy that differential equation you gave. The basic reason why we don't use differentiation is that it loses information, so equations based on differentiation will not have unique solutions. --[[Special:Contributions/76.167.241.45|76.167.241.45]] 06:00, 18 February 2009 (UTC)

::No, the actual problem is that the solutions proposed in the task (and lazy expressions in particular) do not solve what the task is supposed to require. It only happens so that formal integration of 1 plus an infinite self recursion occasionally gives Taylor series of cos:

:::<math>1</math>

:::<math>1 - \int\int 1dx = 1 - x/2</math>

:::<math>1 - \int\int (1 - \int\int 1dy) dx = 1 - x^2/2 + x^4/2*3*4</math>

:::<math>1 - \int\int (1 - \int\int (1 - \int\int 1dz)dy)dx = 1 - x^2/2 + x^4/2*3*4 - x^6/2*3*4*5*6</math>

:::. . .


::But it is a wrong solution in other cases, which are mathematically equivalent. Strange if that would be otherwise. Because mathematically it is about solving equations (integral, differential, basically any).
::I would suggest to remove laziness as irrelevant to Taylor series, as well as self-recursive and also incomputable task. Instead of this I would take some finite series and add, integrate them etc. --[[User:Dmitry-kazakov|Dmitry-kazakov]] 10:38, 18 February 2009 (UTC)

:::Can you give any examples of equations with these integrals where it would be "wrong"? I don't see anything unusual with what you've shown. Maybe you are not appreciating the power of recursive definitions. When you try to break it down into steps it might seem weird but the process is correct. --[[Special:Contributions/76.167.241.45|76.167.241.45]] 11:03, 18 February 2009 (UTC)

::::These integrals are OK, the implementations of functions using Taylor series are not, '''provided''' they should serve the purpose of solving functional equations like <math>F (x, f_1, f_2, ...) = 0</math>, where x is a variable, <math>f_i</math> are Taylor series and F is a combination of functional operators from the task (+, - etc). This is what <math>cos x + \int\int cos x=0</math> makes me suggest. But that does not looks to me like a use case of the Taylor series representation. If it were a case, then for example: <math>cos^2 x + sin^2 x=1</math> should work as well. Will it?

:::::So the article originally had kinda poor math. <math>\int \ldots dx</math> is not what we want because it defines an infinite family of functions, all differing by a constant. I changed the article to use what we really meant, which was the definite integral <math>\int_0^x \ldots dx</math>. <math>\cos x = -\int_0^x \int_0^x \cos x\, dx\, dx</math> is not true, but <math>\cos x = 1 - \int_0^x \int_0^x \cos x\, dx\, dx</math> (which is equivalent to the definition in the article) is.
:::::The article doesn't claim that we can use the method to solve things of the form <math>F (x, f_1, f_2, ...) = 0</math>; I don't know where you got that from. It should say more clearly that the power series represent the Taylor series of functions. And it should explain that, since the integration operation can yield the first term in the power series (the constant term) before evaluating its argument, we can define power series recursively in terms of integrals of themselves, and be able to obtain the entire power series, and that is what the example task tests. --[[Special:Contributions/76.167.241.45|76.167.241.45]] 19:43, 18 February 2009 (UTC)

::::::It is not sloppy. <math>\int f</math> is an [http://en.wikipedia.org/wiki/Indefinite_integral indefinite integral] of ''f''. <math>\int\int f</math> is defined up to a linear function, of course. So <math>cos = -\int\int cos</math> is perfectly OK.
::::::My problem is that I don't see what integration of a constant has to do with formal manipulations of Taylor series. Well, constant is a kind of Taylor series. But that looks rather thin to me. It would be better to have a test task with more substance.
::::::Anyway, we seem to agree that this is not about functional equations, but merely about repetitive integration of a constant. The net effect is that laziness and recursion can be thrown over the board and replaced by a plain loop. --[[User:Dmitry-kazakov|Dmitry-kazakov]] 22:21, 18 February 2009 (UTC)

== Task and goal ==

As far as I can understand it, the task (from the bold '''task''' to the end of paragraph, before "As an example") could be accomplished by implementing a (numerical) method for derivative to compute Taylor coefficients... if the given function is ''derivable'' (he*l I'm not sure this is the right translation) &infty; times in 0; and then implementing operations like + - * / over ''finite'' (only theoretically infinite) vectors (which are nothing but coordinates in a space with basis 1,x,x<sup>2</sup>...)...

Until here, everything's fine, we can leave functional programming or lazy evaluation apart: we can just ''register'' a function (e.g. returning an opaque ''handler'' in langs like C) and expand it when needed (e.g. for computation... but the result of course "exists" only as coefficients), truncating the series to N (where N must be choosen smartly, but even roughly, it won't change too much for the task part); or, we can expand it and store the coefficients, truncating the expansion at the ''convenient'' number N... this approach is better for a lot of languages, and the user likely can't see the difference (if N is choosen well).

But this is about the task part. The example makes it less clear: is it asking just to demonstrate that the integral we implemented works fine? I.e. we find the coeffs for sin, then the coeffs for cos, then we compute integral of cos (using the new type) &mdash;or the derivative of sin...&mdash; and we show that the coeffs are the same of sin...? (then in the example we did not need to use a definite integral for this)

At the end, the goal can be easily missed even accomplishing the task part: how langs handle "new numeric types" is already shown in other tasks (at least one, [[Rational Arithmetic]]); and delayed/lazy evaluation, which is not usable (''directly'') by every language, is also not strictly needed.

Summarizing: is the task accomplished even if the goals are (partially) missed? --[[User:ShinTakezou|ShinTakezou]] 17:30, 9 March 2009 (UTC)

== Java and generics ==

It was my understanding that generics had been added to Java recently, yet the Java example indicates that they're not available.  A generic implementation would be far better than using the "swap out the type for the one you want" implementation it currently uses. --[[User:Short Circuit|Short Circuit]] 17:31, 17 May 2009 (UTC)
: Alas, <tt>java.lang.Number</tt> is not a very useful base-class in this regard, nor are its standard subclasses any better. In particular, there's no built in methods for performing arithmetic; those operations are only defined on the atomic numeric types, which can't participate in the generic type system. This means that the example would have to build its own type framework, and suddenly that's looking like real work and not elegant examples. (FWIW, the reason why the atomic types don't participate in the generics system is that they are treated specially by the JVM spec; addition on integers is completely different to addition on doubles. This would have forced recompilation for handling generic atomics, which was rejected as being silly and expensive. By contrast, swapping one object type for another is pretty straight-forward.) —[[User:Dkf|Dkf]] 21:40, 22 May 2009 (UTC)
