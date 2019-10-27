+++
title = "Talk:Pathological floating point problems"
description = ""
date = 2018-03-28T15:49:22Z
aliases = []
[extra]
id = 21310
[taxonomies]
categories = []
tags = []
+++

== Mention of the IBM 1620 in the FORTRAN entry==
The   '''IBM 1620'''   (a decimal computer) can support integer arithmetic up to the size of the machine;   it came in twenty, forty, or sixty thousand decimal digits   (a ''digit'' consisted of six bits:

  a check bit     (nowadays, this would be called a parity bit)
  a flag bit      (for negative numbers, indirect addressing, end of field) 
  4 numeric bits  (a decimal digit, or one-half of a character)

The   '''IBM 1620 model I'''   had tables for addition and multiply   (stored in low memory),   so it shouldn't be overlaid if one expects to perform integer arithmetic.   However, modifying these tables enabled arithmetic in other bases (up to base ten). 

My first exposure to a computer was during a tour of the computer room (1965), the teacher entered (typed) a machine-language program into the computer memory (via the typewriter), and the computer program raised   999   to the   999<sup>th</sup>   power,   and it typed   2,997   decimal digits.


There were no control cards for any model of the '''IBM 1620'''. 


The   '''IBM 1620 model II'''   removed the need to store those tables in low memory.


For the (optional) floating point feature, the maximum mantissa size was 99 decimal digits, and the exponent was fixed at two decimal digits.

-- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 18:00, 16 February 2017 (UTC)

<b>
-----
</b>

:After some head-scratching I found https://www.mirrorservice.org/sites/www.bitsavers.org/pdf/ibm/1620/ which had a link to https://www.mirrorservice.org/sites/www.bitsavers.org/pdf/ibm/1620/C26-5876-2_FORTRAN_II_Mar65.pdf wherein after reading to page 24 (damn .pdf is image-only: no searchable text) I found the heading '''Source Program Control Card''' which is to precede the Fortran source, in the form <code>*ffkks</code> where ''ff'' is the size of the floating-point mantissa (2 to 28), ''kk'' the size of integers (2 to 10) and ''s'' is the memory size of 2, 4, 6 for 20,000 to 60,000 digits if it is to be not that of the current computer.

:I don't recall ever using this facility, as my major program at Auckland University for the 1620 was to write a simulator for the AMI computer (simulated on the IBM 1130) that had been used for teaching a first year course in applied mathematics in AMI machine code, leading to the calculation of functions and Chebyshev adjustment of those calculations. I have always wondered how the 1620 handled such functions (sine, cosine, etc) given different precisions, as there was no sign of different decks of cards for the subroutine library for different precisions, and the use of Chebyshev polynomials to "economise" the precision of a particular calculation via a power series depended very much on what precision was being employed and pursued. Perhaps, just slog through additional terms until they become small enough rather than a pre-determined sufficient number of terms. [[User:Dinosaur|Dinosaur]] ([[User talk:Dinosaur|talk]]) 03:31, 30 April 2017 (UTC)

== Explanation of task 1 ==

Task 1 is a nonlinear recurrence equation. However, it's easily solved by the change of variable v(n)=w(n)/w(n-1), and it leads to the linear equation w(n)=aw(n-1)+bw(n-2)+cw(n-3), with a=111, b=-1130 and c=3000. To solve this, one has to compute the roots of the polynomial x^3-ax^2-bx-c, which are 5, 6 and 100. Hence the general solution of the linear equation is

w(n)=c1 5^n + c2 6^n + c3 100^n.

Then v(n)=w(n)/w(n-1) tends to 100 if c3<>0, to 6 if c3=0 and c2<>0 and to 5 if c2=c3=0.

One has then to find these 3 parameters, using the initial conditions, and possibly more terms. This leads to a linear system of equations:

w(1)-v(1)w(0)=0
w(2)-v(2)w(1)=0
w(3)-v(3)w(2)=0

Where v(1) and v(2) are known (here 2 and -4 respectively) and v(3) can be computed from the initial relation v(3)=a+b/v(1)+c(v(1)v(2)). And the unknowns are of course c1,c2,c3.
However, this system is underdetermined (it has rank 2). If the system had full rank, the unique solution would be c1=c2=c3=0, which is impossible, as the quotient w(1)/w(0) for instance would then be undefined.

Since the system has rank 2, one can find the expression of say c2 and c3, given c1 as a parameter. Interestingly, the value of c3 is a fraction whose numerator is c1*(30-v1(11-v2)). And c1 can't be zero (it also appears as a factor in the numerator of c2, hence if c1=0, w(n)=0 for all n). Hence, c3=0 if and only if v1(11-v2)=30. Which is the case here with v1=2 and v2=-4, hence the limit can't be 100.

We can further find that c2=c3=0 if and only if v(1)=v(2)=5.

Therefore, the limit has to be 6.

However, the limits 5 and 6 are unstable: any value that is even slightly inaccurate in the computation of furthr terms will lead to c3<>0 hence limit 100.
Hence, even computations in multiprecision with bounded accuracy will, sooner or later, yield a sequence converging to 100. However, it's possible in bounded (but large) precision to find an accurate value of a fixed term of this sequence, and the precision to use depends on the term one wants.

[[User:Eoraptor|Eoraptor]] ([[User talk:Eoraptor|talk]]) 15:48, 28 March 2018 (UTC)
