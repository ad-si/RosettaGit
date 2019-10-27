+++
title = "Talk:Multiple regression"
description = ""
date = 2018-03-14T14:02:24Z
aliases = []
[extra]
id = 4457
[taxonomies]
categories = []
tags = []
+++

== Clarification needed ==
This task needs more clarification, like a link to a suitable wikipedia page. —[[User:Dkf|Donal Fellows]] 17:04, 29 June 2009 (UTC)

This task requires merging with [[Polynomial Fitting]] already representing least squares approximation example in the basis <span style="font-family: serif">{1, ''x'', ''x''<sup>2</sup>}</span>. Linear regression is just same in the basis <span style="font-family: serif">{1, ''x''}</span>. --[[User:Dmitry-kazakov|Dmitry-kazakov]] 18:31, 29 June 2009 (UTC)

OK, there's now implementations in two languages. It's not clear to me how this is different from the polynomial fitting task either, but I'm a completionist (for [[Tcl]]) and not a statistician... —[[User:Dkf|Donal Fellows]] 10:34, 9 July 2009 (UTC)

An explanation from the Lapack documentation may be helpful. [http://www.netlib.org/lapack/lug/node27.html]
The idea is that you want to model a set of empirical data

<math>\{(x_1,F(x_1))\dots (x_m,F(x_m))\}</math> 

by fitting it to a function of the form

<math> \hat{F}(x) = \sum_{i=1}^n\beta_i f_i(x)</math>

where you've already chosen the <math>f_i</math> functions
and only the <math>\beta</math>'s need to be determined.
The number of data points <math>m</math> generally will
exceed the number of functions in the model, <math>n</math>,
and the functions <math>f_i</math> can be anything, not just <math>x^i</math>
as in the case of polynomial curve fitting.

I don't believe the Ruby and Tcl solutions on the page solve the general case of this problem
because they assume a polynomial model. I propose that the task be clarified
to stipulate that the inputs are two tables or matrices of numbers, one containing all values of
<math>f_j(x_i)</math> and the other containing all values of <math>F(x_i)</math>
with <math>i</math> ranging from 1 to <math>m</math> and <math>j</math> ranging from
1 to <math>n</math>, and <math>m>n</math>. --[[User:Sluggo|Sluggo]] 12:52, 9 August 2009 (UTC)

:The thing that worried me was that there wasn't any code in there to determine whether the degree of the polynomial selected was justified. I had to hard-code the depth of polynomial to try in the example code. I ''think'' the fitting engine itself doesn't care; you can present any sampled function you want for fitting. (I suppose many of the other more-advanced statistics tasks have this same problem; they require an initial “and magic happens here” to have happened before you can make use of them.) —[[User:Dkf|Donal Fellows]] 13:47, 9 August 2009 (UTC)

:You can always fit any empirical data to a polynomial. This task is about fitting it to functions of a more general form (i.e., a different basis). For example, by selecting the functions <math>f_i</math> as sinusoids with appropriate frequencies, it should be possible to obtain Fourier coefficients (albeit less efficiently than with a dedicated FFT solver). I don't see how the code in the given solutions could be used to do that. --[[User:Sluggo|Sluggo]] 16:57, 9 August 2009 (UTC)
:: Well, the only code in the Tcl example that assumes a polynomial is in the example part and not the core solution part; that's using polynomials of up to degree 2 because that's what the page that provided the data used for the example suggested, not out of some kind of endorsement of polynomials. —[[User:Dkf|Donal Fellows]] 20:51, 9 August 2009 (UTC)
::Fair enough. Thank you for the explanation. --[[User:Sluggo|Sluggo]] 22:44, 9 August 2009 (UTC)

== Misleading Note in Intro ==

The note in the introduction is incorrect. So I deleted it. This task is a multiple ''linear'' regression problem; the use of OLS indicates that we are dealing with a ''linear model''. This is very different from a polynomial fitting problem which, by definition, is generally non-linear. At best, the multiple regression task is ''multi-linear'' and it is most certainly a subset of polynomial fitting problems. The note would be correct if we were talking about a multi-variate polynomial fitting task (which would actually make an excellent task). --[[User:Treefall|Treefall]] 22:36, 20 August 2010 (UTC)

== Python example is not correct??? ==

The Python example solves a different problem, namely fitting a quadratic polynomial in one variable to a set of points in the plane. What is asked for is a way of fitting a _linear_ poynomial in _several_ variables to a set of points in some dimension. I think np.linalg.lstsq() , a function from numpy , is what is needed.
: The method with the matrix operations was basically correct, but it was hard to see with it using random data.  I substituted the Wikipedia example data so it was more clear that the method works.  I also added a np.linalg.lstsq() version, which I understand is preferred.  &mdash;[[User:Sonia|Sonia]] ([[User talk:Sonia|talk]]) 21:37, 3 April 2015 (UTC)

==Task description has too many equations and not enough guidance==
Could someone at least try and mitigate the need to at least think you know what multiple regression is, before you can make sense of the page?

The task doesn't ask for an example computation, and the wikipedia page on OLS is impenetrable to the good programmer/lesser mathematician. --[[User:Paddy3118|Paddy3118]] 07:13, 2 April 2011 (UTC)

==Equations invisible on OS X Chrome and Safari==

The main page of this task is one of a number of Rosetta task pages which simply show a set of framed blank panels (not even a fallback image) in lieu of equations, when viewed on OS X with Safari or Chrome (FWIW the MathML on this Talk page is displaying without problem in the same browsers).
Is it possible that the markup on some of these pages is making assumptions that are now out of date, or are possibly less platform/browser independent than was hoped at the time of editing ?  [[User:Hout|Hout]] ([[User talk:Hout|talk]]) 12:46, 15 September 2016 (UTC)
: Experimenting with edit preview, I found that visibilty of the equations could be restored (and removed again) by removing (or re-introducing) the single white space between the opening &lt;math&gt; tag and the first printing character of the Math ML code. For the moment I have committed that edit, and can now see the formulae on the OS X (Sierra) browsers. Has that temporary edit suppressed visibility on any other platforms ? If not, I suggest a check for other instances of a math tag followed by an immediate white space character, on other Rosetta Code Task pages.  [[User:Hout|Hout]] ([[User talk:Hout|talk]]) 13:12, 15 September 2016 (UTC)

:: Task description now restored to visibility, though excessive font size disparities introduced by doubled &lt;big&gt; tags still need to be be removed [[User:Hout|Hout]] ([[User talk:Hout|talk]]) 19:42, 28 September 2016 (UTC)
::: Font size disparities now normalized by [[User:Tikkanz|Tikkanz]] 2016-09-28 [[User:Hout|Hout]] ([[User talk:Hout|talk]]) 20:34, 28 September 2016 (UTC)

:: Some formula elements in the preamble to the Perl 6 contribution remain invisible to most browsers. Two instance of the Greek letter gamma (γ) and the whole of the formula  I = h 0 ∧ h 1 ∧ h 2 which follows the word 'Noting'.  The server-side graphic used by Chrome, IE/Edge, Safari etc is not displaying. (Only a minority of browsers (using MathML + local fonts, as in the case of Firefox) are displaying these formula elements) [[User:Hout|Hout]] ([[User talk:Hout|talk]]) 19:42, 28 September 2016 (UTC)
