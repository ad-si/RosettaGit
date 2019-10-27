+++
title = "Talk:Nth root"
description = ""
date = 2016-10-04T06:38:24Z
aliases = []
[extra]
id = 4637
[taxonomies]
categories = []
tags = []
+++

== More effient implementation by computing the diffence first ==
Most implementations use x(k+1) = (x(k)*(n-1) + A/x(k)^n) / n, d = x(k+1)-x(k) instead of d = (A/x(k)^n - x(k) / n, x(k+1) = x(k) + d; This leads to inefficient algorithm and should be corrected (see implementation for for AWK, C and Octave). --[[User:Aschloegl]]

: There's no reason to believe that. At least in the C example, the bottleneck is likely the <code>pow_()</code> function, so calculating diff or x_k shouldn't make much of a difference.  In fact, the C example compiled with <code>gcc -Ofast -msse</code> on my machine, looping over <code>root(x, 15)</code> a million times actually became marginally slower after your edit. I'll not revert the edit as the difference is small. You also forgot to change the return value. --[[User:Ledrug|Ledrug]] 12:57, 10 November 2012 (UTC)

==Comparison to Non-integer Exponentiation==
Okay, I get that this task calls for implementing a particular algorithm (convergence by successive approximation).  However, it seems like it would be appropriate to describe (in comments perhaps) whether the language supports a more direct method of computing an nth root (such as raising a number to a fractional power: x ** (1/n) for the nth root of x).

For those languages which support it (non-integer exponentiation; or via any library function) I'd also recommend that the task specify that the code should compare the results of this algorithm versus the one obtained in the language's most natural native method.  [[User:JimD|JimD]] 22:03, 3 August 2009 (UTC)
:Exponentiation operators should be discussed in [[Math constants and functions]]. Comparisons could be added optionally. --[[User:Mwn3d|Mwn3d]] 22:14, 3 August 2009 (UTC)
::[[Math constants and functions]] does not ask for an Nth root function, though that is related to exponentation. --[[User:Rdm|Rdm]] 15:16, 18 May 2010 (UTC)

==C edit==
I see the recent edits on C example as having multiple problems: 1) math function pow() was specifically avoided because it makes no sense to do nth root if we could just pow() it; 2) prec parameter is of wrong type; 3) when comparing for convergence, at least take the absolute value; and the comparison should be made against a relative value; 4) requiring 4 parameters is counterintuive.  I'll let it stand for now in case the new editor will come and fix it, or it will be reverted. --[[User:Ledrug|Ledrug]] 22:40, 4 September 2011 (UTC)

==Maclaurin Series==
Nth root can be calculated using the maclaurin series of exp and ln like this


<math>x^{\frac{1}{n}} = e^{\frac{1}{n} \log{(x)}}</math>


<math>\ln{(\frac{1+x}{1-x})} = \sum^{\infty}_{n=0}2 \frac{x^{2 n+1}}{2 n+1}</math>


<math>\log{x} = \ln{\frac{x-1}{1+x}}</math>


<math>e^x = \sum^{\infty}_{n=0}\frac{x^n}{n!}</math>


since n is always an integer >= 0, a simple pow function can be used for the calculations.

example:

```lisp
(defun powint (a b)
  (if (= b 0)
      1
    (* a (powint a (- b 1)))))
```



Ofcourse it's not possible to calculate up to an infinity, but the value can be calculated til it is good enough.

Using a library that implements a type that includes nominator and a denominator (like GMP's mpt_q), it is possible to calculate

the Nth root using only integers.

Maybe not the best solution but it could be something like this

```lisp
(defun factorial (n)
  (if (= n 0)
      1
    (* n (factorial (- n 1)))))

(defun powint (a b)
  (if (= b 0)
      1
    (* a (powint a (- b 1)))))

(defun logS(x n) (if (= n 0) (* 2 x) (+ (* 2 (/ (powint x (+ (* 2 n) 1)) (+ (* 2 n) 1))) (logS x (- n 1)) )  ))

(defun loge(x n) (logS (/ (- x 1) (+ 1 x)) n))

(defun expon(x n) (if (= n 0) 1 (+ (/ (powint x n) (factorial n)) (expon x (- n 1)))))

(defun pown(x a n) (expon (* a (loge x n)) n))
```



example output:
 [3]> (pown 2 1/3 3)
 45765070755968273/36327499129868625

 [4]> (* (pown 2 1/3 20) 1.0)
 1.2599211


Though, the higher value you want to calculate the Nth root of. The more iterations of the sums is needed to make an accurate computation.

--[[User:Spekkio|Spekkio]] 14:17, 29 November 2011 (UTC)

==task expansion==

I see no reason why this task can't be expanded to handle non-negative, non-positive numbers. ... Er, that is, cases of zero for the  X.  

This special case (while being excluded by the task's requirement), should be handled by all examples, I should think, as would any trivia solutions. -- [[User:Gerard Schildberger|Gerard Schildberger]] 23:38, 18 August 2012 (UTC)


==Formula hidden to most browsers by under-tested cosmetic edits at 20:07, 20 June 2016 ==

Under-tested cosmetic edits made to the task page at 20:07, 20 June 2016, including the injection of spaces around expressions in &lt;math&gt; tags, have left formula content completely invisible to all browsers which display the graphic file version of formulae rather than processing the MathML (this is, in fact, the majority of browsers). The MediaWiki processor does not currently expect such spaces, and generates syntactically ill-formed HTML if they are introduced. Other aspects of these cosmetic edits may further compound the problem. [[User:Hout|Hout]] ([[User talk:Hout|talk]]) 21:28, 22 September 2016 (UTC)

: Visibility now restored [[User:Hout|Hout]] ([[User talk:Hout|talk]]) 06:38, 4 October 2016 (UTC)
