+++
title = "Talk:Random numbers"
description = ""
date = 2017-01-25T23:26:35Z
aliases = []
[extra]
id = 2025
[taxonomies]
categories = []
tags = []
+++

== Isn't this page misnamed? ==
Isn't this page misnamed?  These are all pseudo-random numbers, I think.

: Nope. These are APIs into a provider for random numbers. What this provider is and how it works depends on the language, the implementation and the OS. Some may or may not produce pseudo-random numbers; some others may not. 

== Formatting trouble ==

Here's SAS code, which I can't figure out how to format for this wiki right now:

data test;
do i = 1 to 1000;
  x = rand("NORMAL",1,.5);
  output;
  end;
run;



sorry for editing without logging in - that was me all along...[[User:Sgeier|Sgeier]] 00:57, 7 April 2007 (EDT)

== Normal Distribution ==

The Java and IDL examples do not appear to create normally distributed numbers, just uniformly distributed numbers over a given range (1 to 1.5 in the Java case).

The numbers generated will also not have the correct mean and standard deviation.  The expected value of the Java example, for instance, will be 1.25 instead of 1.  The standard deviation will be .144 instead of 0.5.

For an example of an algorithm, check out http://en.wikipedia.org/wiki/Normal_distribution#Generating_values_for_normal_random_variables

--[[User:Nachtrabe|Nachtrabe]] 13:53, 31 May 2007 (EDT)

: I've fixed the Java example. --[[User:Ce|Ce]] 14:25, 31 May 2007 (EDT)

: The IDL example uses randomn(), which yields normally distributed numbers (mean 0, std-dev 1). Uniformly distributed numbers would be randomu()

I've modified the MAXScript example using formula others have used, but the results are not correct. Specifically, I'm getting a mean of around 1.62 and a std-dev of 0.33. I believe this is because "sqrt (-2*log a) * cos (2*pi*b)" isn't returning values [0.0..1.0]. Removing the "1.0 + 0.5 *" terms gives a mean of 1.25 and std-dev 0.66. Anyone have any ideas where I'm going wrong? [[User:Drea|Drea]] 01:23, 21 September 2007 (MDT)

: The normal distribution does not limit values to a hard range. They are instead centered on 0 with a standard deviation of 1. One possibility: make sure your log is the natural logarithm and not base 10. --[[User:IanOsgood|IanOsgood]] 09:03, 21 September 2007 (MDT)

:: The base of the logarithm could indeed explain the standard deviation; using base 10 instead of base 2 would add a factor of sqrt(1/ln(10)), which indeed is just about 0.66. The second thing to check is the cosine: If MAXScript takes the argument of the cosine in degrees instead of radians, then in the formula the value 2*pi must be replaced by 360.
:: Additional note: Usually floating random number generators give numbers from the interval [0,1) (i.e. 0 included, 1 excluded). However this algorithm needs random numbers from (0,1] (0 excluded, 1 included). It doesn't really matter for b, but for a the value 0 would be fatal (the logarithm isn't defined at value 0). Thus you should check what the random number generator does, and in case it's indeed using the interval [0,1), use <tt>1.0 - random 0.0 1.0</tt> instead. --[[User:Ce|Ce]] 12:26, 21 September 2007 (MDT)
:::Thanks guys, it turned out to be cos expecting degrees rather than radians and I've fixed it now.
:::The documentation of the random generator claims the return values will be inclusive of the arguments, but I tested 1000000000 randoms and got 50 hits on <tt>rand == 1.0</tt> and no hits on <tt>rand == 0.0</tt>. I know the vagaries of floating point comparisons, but as that test took ~20 mins to run, I think it's good enough for this purpose ;o) [[User:Drea|Drea]] 14:19, 21 September 2007 (MDT)

== using a pi approximation ==

I noticed one programming example using   22/7   for the value of pi.   Is this close enough for government work?   Could/should it be flagged   ''in need of improvement''? -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 18:45, 28 October 2014 (UTC)

== Are we allowed to use a built-in normally distributed RNG? ==

TI-BASIC has a command that generates normally distributed random numbers: randNorm(
Are we allowed to use it, or do we have to convert from uniformly distributed?
