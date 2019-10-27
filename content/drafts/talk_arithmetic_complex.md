+++
title = "Talk:Arithmetic/Complex"
description = ""
date = 2016-11-16T13:08:19Z
aliases = []
[extra]
id = 2692
[taxonomies]
categories = []
tags = []
+++

Ada and C++ both still need negation. I forgot to put it in the Java example before but I added I it later. Sorry for the confusion. --[[User:Mwn3d|Mwn3d]] 10:13, 9 March 2008 (MDT)
:The Ada entry has been amended to include negation. --[[User:Waldorf|Waldorf]] 14:40, 9 March 2008 (MDT)

==C99 example incorrect?==
I believe the C example is incorrect. The current written example relies upon operator overloading which is not supported by C99.--[[User:Waldorf|Waldorf]] 09:10, 11 March 2008 (MDT)
:I did some looking; there are complex built-in types in the C99 standard, with defined implicit casts up and down to the other numeric primitives. --[[User:IanOsgood|IanOsgood]] 10:36, 11 March 2008 (MDT)
::How could an implicit cast from a complex type to a floating point or integer type result in a valid value? Simple integers and floats cannot represent both the real and imaginary parts of the complex number.--[[User:Waldorf|Waldorf]] 17:36, 11 March 2008 (MDT)
:::The code example only uses conversions from real types to complex types; but complex types can be converted to real types by discarding the imaginary part. --[[User:Spoon!|Spoon!]] 18:21, 11 March 2008 (MDT)
::::The code example includes addition, multiplication, and negation using the language-defined operators. In those instances no conversion is possible without loosing information (the imaginary part of the complex number). Without operator overloading no correct result can be obtained. Any implicit conversion will result in loss of the imaginary part of the complex number. The C example must therefore be incorrect.--[[User:Waldorf|Waldorf]] 07:18, 13 March 2008 (MDT)
:::::The only ways I can think of to convert a complex number to a purely real number without losing information is to find the length of the number in the complex plane or convert it to polar form. You can't reconstruct the original number from the length, so that shouldn't be done for this conversion. The polar form still requires two parts (magnitude and angle), so it's not really a primitive (at least in C). I don't know why anyone would want to convert a complex number to a real number when you can just get the "real part." The C example is not incorrect as the task is written because the task says nothing about casting to anything. A complex number is a complex number and that's all it will ever be. Any casts to purely real types ''should'' have information loss. (int)(2.5 + 3.2i) should be 2. (Complex)(3*5) should be 15 + 0i. That's how anyone could expect it to work. --[[User:Mwn3d|Mwn3d]] 08:49, 13 March 2008 (MDT)
::::::I agree with what you say. The task requires addition and multiplication of two complex numbers, and inversion and negation of a single complex number. The C solution uses C arithmetic operators. C99 does not provide operator overloading. This means that the arithmetic operators can only be used if implicit conversion to a primitive type occurs. Either the example is wrong because it improperly uses arithmetic operators for complex arithmetic, or it is wrong because implicit conversions result in real arithmetic, not complex arithmetic. The truth can be easily demonstrated by printing the results of the operations in C.--[[User:Waldorf|Waldorf]] 09:30, 13 March 2008 (MDT)
:::::::OK...looking into it you're right. I offer two solutions. 1: I added an example which defines a struct and the desired operations, 2: rework the complex.h solution to use creal and cimag functions (from [http://en.wikipedia.org/wiki/Complex.h complex.h]) as part of other functions to do the math. --[[User:Mwn3d|Mwn3d]] 10:01, 13 March 2008 (MDT)
:::::Waldorf, I don't know what you're going on about. When I compile this with '''gcc -std=c99''' I get correct results (once I corrected the constants). The implicit conversions only happen when there are type mismatches, but all the operations are between complex types. For 1.0/a, there is an ''up'' conversion to 1+0i before dividing. The standard operators are all defined correctly for complex types, and other library functions like '''conj()''' for the conjugate are defined for complex-specific operations. --[[User:IanOsgood|IanOsgood]] 10:47, 13 March 2008 (MDT)
:::::As for operator overloading, perhaps you are confusing this with operator overloading for ''user defined'' types. This is trivially true for C because you cannot define functions that overload the symbolic infix operators (as opposed to the operator+() overloading in C++). However operator overloading for ''built-in'' types is well supported in C. C99 happens to add complex numbers as built-in types.  --[[User:IanOsgood|IanOsgood]] 11:10, 13 March 2008 (MDT)
:::::To avoid this kind of argument in the future, I am changing the task requirements to require printing the results. --[[User:IanOsgood|IanOsgood]] 11:10, 13 March 2008 (MDT)

== Complex conjugate ==

''See [[Talk:Complex conjugate]].''

== ooRexx ==

say c1/c1 seems to be incorrect!?! --[[User:Walterpachl|Walterpachl]] ([[User talk:Walterpachl|talk]]) 19:47, 12 May 2014 (UTC)
:Yea, I get 0.44+0.08i so flagged it as incorrect. --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 20:30, 12 May 2014 (UTC)
::I've corrected the inv method which was incorrect --[[User:Walterpachl|Walterpachl]] ([[User talk:Walterpachl|talk]]) 19:22, 14 May 2014 (UTC)


==Formulae made invisible by under-tested cosmetic edits at 01:25, 25 August 2016 ==

Under-tested cosmetic edits made to the task page at 01:25, 25 August 2016, including the injection of spaces around expressions in &lt;math&gt; tags, have left some or all of the task description formulae completely invisible to all browsers which display the graphic file version of formulae rather than processing the MathML (this is, in fact, the majority of browsers). The MediaWiki processor does not currently expect such spaces, and generates syntactically ill-formed HTML if they are introduced. Other aspects of these cosmetic edits may further compound the problem. [[User:Hout|Hout]] ([[User talk:Hout|talk]]) 10:36, 24 September 2016 (UTC)

: Now repaired [[User:Hout|Hout]] ([[User talk:Hout|talk]]) 13:08, 16 November 2016 (UTC)
