+++
title = "Talk:Exponentiation operator"
description = ""
date = 2013-11-06T13:59:46Z
aliases = []
[extra]
id = 2603
[taxonomies]
categories = []
tags = []
+++

== Fractional Exponents ==
This is for integer powers only right? No fractional exponents? --[[User:Mwn3d|Mwn3d]] 05:58, 19 February 2008 (MST)

I believe that the REAL * REAL fraction can probably be done faster using some numerical equation.  Eg simply 
 OP ** = ( REAL base, exponent )REAL: exp(log(base)*exponent); ~ # ... #
This above would work for fractional powers.  For integral exponents the routine I provided is "sometimes" faster 
then alternatives, esp where the exponent is s power of two.

BTW: I wrote these routine as a hint to [[User:Short Circuit]], and as a replacement to the <code>mpz_class pow2(mpz_class exp)</code>
routine he contributed in [[Lucas-Lehmer_test#C++]] (Which is probably painfully slow).

[[User:NevilleDNZ|NevilleDNZ]] 06:11, 19 February 2008 (MST)
:OK, but for this task, all that is required is int<sup>int</sup> and real<sup>int</sup> right? --[[User:Mwn3d|Mwn3d]] 06:19, 19 February 2008 (MST)

Yes. [[User:NevilleDNZ|NevilleDNZ]] 06:22, 19 February 2008 (MST)

It should be specified also the return type; even for ''int''<sup>''int''</sup> it makes sense to return a real value, since if the exponent is negative, it is what we get mathemtically. Should we return an integer instead? (This means: if the exp is less than 0, the result is 0)
--[[User:ShinTakezou|ShinTakezou]] 01:07, 8 December 2008 (UTC)

== AWK ==

The awk solution is pretty weak: It does not handle fractional exponents. It looks like there is a log function for awk, so I reckon it is possible to create a fully working solution, but has been many years since I last looked at how to do this, so I need to do some revision on mathematics. I am working on a solution, but if someone already has a working algorithm then please paste it here as pseudocode, and I will try and translate it into awk. [[User:Markhobley|Markhobley]] 11:44, 7 September 2011 (UTC)

: <math>a^b = e^{b \times \log a}</math>

: For example, [[Real constants and functions#bc]] uses <code>e(l(2) * -3.4)</code> to calculate 2 to the power of -3.4. This formula requires a >= 0, unless you can use [[Arithmetic/Complex|complex numbers]]. --[[User:Kernigh|Kernigh]] 18:00, 7 September 2011 (UTC)

::The exponent operator is causing confusion. I am under the impression that some versions of awk do not have this. Looking at various documentation on the internet, the caret operator is not on the list of operators. However, some documents state that POSIX lists the caret, but that doesn't mean anything because that could be a retrofit. Did System 5 awk supported the caret as an exponent operator? I only have it listed as a regular expression anchor. I was hoping to come up with a formula that does not use the exponent operator altogether. [[User:Markhobley|Markhobley]] 22:34, 7 September 2011 (UTC)
	
:::[http://heirloom.sourceforge.net/man/oawk.1.html oawk] is missing the ^ operator. I believe that the ^ operator and the function keyword appeared in [[nawk]] around 1985 to 1988, but I am not sure. --[[User:Kernigh|Kernigh]] 23:34, 6 March 2012 (UTC)

==diatomic operator==

The REXX language allows the exponential operator to be specified as

* **
* *   *

(that is, with an optional whitespace between the two asterisks.

I once read that one of these was diatomic, but I can't find a
reference anymore, and I know not of which is which. 

Of course, it's possible that both are diatomic.

Also, REXX allows such (multiple) statements as   (but not recommended, of course):

```rexx

 tot = x *,       /*raise  X  to the ...   */  
         * 10     /*  ... tenth power.     */

```

-- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 19:02, 7 August 2013 (UTC)

== 0**0 ==

Rexx 0**0 gives 1, so does ipow (the internal routine)

And so does NetRexx.

But should it not raise error as PL/I does?--[[User:Walterpachl|Walterpachl]] ([[User talk:Walterpachl|talk]]) 09:53, 6 November 2013 (UTC)

:It depends upon that which you are doing with it:
::a**b=c is the inverse function of log(c base b) = a - there is no such thing as log base 0 and no number has a log of zero in any base.
::Number Theory dealing with polynomials (which are themselves numbers) any number ** 0 is 1 by definition.
::Calculus treats it as undefined.

--[[User:Nigel Galloway|Nigel Galloway]] ([[User talk:Nigel Galloway|talk]]) 13:59, 6 November 2013 (UTC)
