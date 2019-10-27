+++
title = "Talk:QR decomposition"
description = ""
date = 2015-04-30T20:11:47Z
aliases = []
[extra]
id = 9940
[taxonomies]
categories = []
tags = []
+++

==usage?==

What does "and the usage for linear least squares problems on the example from Polynomial_regression" mean, specifically, as a task requirement? --[[User:Rdm|Rdm]] 16:02, 17 June 2011 (UTC)
:It means that there already is a existing task on RC ([[Polynomial regression]]) which requires linear least squares, and since LLS is one use case for QR, that task can be used here as an example. The Go and R solutions of the Polynomial regression already used QR instead of the normal equations approach. --[[User:Avi|Avi]] 20:52, 17 June 2011 (UTC)
:According to the wikipedia page on least squares, QR reduction is supposed to be more numerically stable than faster approaches.  But we can solve the Polynomial regression example exactly, so I am not sure that that example is a good one for QR reduction least squares fitting.  --[[User:Rdm|Rdm]] 16:34, 17 June 2011 (UTC)
::The advantage is that it is already existing, has many solutions and can be used as a comparison. --[[User:Avi|Avi]] 20:52, 17 June 2011 (UTC)

== C example ==

The code does do the job, but is quite ugly, and overly long.  Much improvement is needed, and all the matrix_delete() calls would make you appreciate garbage collection that much more. --[[User:Ledrug|Ledrug]] 01:26, 29 June 2011
: Is the D version (that is a port of the Common Lisp version) any better?
I believe the code has a problem. Under certain circumstances, division by zero will occur in the line
   vdiv(e, vnorm(e, m->m), e, m->m);
The reason seems to be that the loop which creates the Householder matrices loops over the number of rows in the input matrix m (m->m), and attempts to extract a column with the same index as the row index from the matrix z (which has the same dimensions as m). This is fine if m->m <= m->n. For purposes of solving over-determined equation systems, however, we will have m->m > m->n. Consequently, we will try to extract non-existing columns from the matrix z, and what they contain will be indeterminate. Should it happen that all column elements are zero, the division by zero referred to above will take place. This of course renders the whole calculation useless. --[[User:Ernstegon|Ernstegon]] 21:22, 17 November 2012 (UTC)
:: You are right, the code was using the wrong subscript of the matrices.  Goes to show that ugly code is more prone to bugs.  I hope it's fixed now. --[[User:Ledrug|Ledrug]] 07:33, 19 November 2012 (UTC)
:::Thanks for looking at the problem so quickly. However, I still have doubts. Is the result really a correct QR decomposition? Is not the matrix R supposed to be upper triangular, i.e. all elements below the diagonal should be zero? That is not the case with the results produced with the amended code. When I use it for solving an over-determined system of linear equations, I get results that differ by quite a bit from Matlab - more than could ever be explained by numerical issues. I tried the following simple system in Matlab:

:::A = [1.0 -2.0; 1.0 -1.0; 1.0 0.0; 1.0 2.0; 1.0 3.0]

:::B = [-4.0 -1.0 3.0 6.0 13.0]'

:::X = A\B

:::The result is X = [2.162790698 3.093023256]

:::When I try the amended code in a C test program, using the same input data, I get X = [2.156692 3.108271]. Close, but no cigar.

:::The code as it was before your update produced X = [2.162790 3.093023] - in those cases where it did not crash. --[[User:Ernstegon|Ernstegon]] 16:14, 19 November 2012 (UTC)
::::After a closer look, it seems the two loops which you modified should be:

::::
```c
for (k = 0; k < m->n; ++k)
```


::::...not k < m->n - 1

::::With that modification, the code produces correct results in my test program. --[[User:Ernstegon|Ernstegon]] 17:20, 19 November 2012 (UTC)
:::::Eh, ok, maybe it's fixed now. --[[User:Ledrug|Ledrug]] 17:51, 19 November 2012 (UTC)

Am I missing something?  I am having trouble compiling the code as is.  Is it assuming a certain version/standard of C?  --[[User:Cantorg|Cantorg]] 09:51, 18 February 2015 (UTC)

== Common Lisp example ==

What is eye in make-householder? In array-range m and n seem useless.

:Thanks for pointing this out. (eye n) returns a nxn identity matrix. I overlooked that. (identity) is an already taken Common Lisp function, so I couldnt use that. The terminology is following matlab/octave's naming. Your second point is also correct, and these two local bindings can be removed. Thanks. --[[User:Avi|Avi]] 16:21, 12 July 2011 (UTC)

==Description flow==
In response to [http://irclog.perlgeek.de/rosettacode/2011-07-03#i_4051779], I find the task description difficult to follow, as its narrative form interleaves mathematical formulae and expressions with English text. I tried splitting it up as I did in [[Gauss-Legendre_Quadrature]], but I was unable to. If someone could organize the task description to visually separate the formulas from the written text, that'd be great. --[[User:Short Circuit|Michael Mol]] 15:34, 4 July 2011 (UTC)

:The narative interleaves formulas and text since this is how most papers are written. The formulas are not meant to be a pseudocode in mathematical notation which can then be reimplemented line by line, I have given a non-optimized Common Lisp version which follows the steps and should be easy to follow and can be used as a reference. --[[User:Avi|Avi]] 16:25, 12 July 2011 (UTC)

==Pseudocode request==
As it stands, I think this task description has nothing near good enough to inform a '''programmer''' how to solve it.It relies too much on correct interpretation of the mathematical equations. O.K. this talk page then goes on to state that the Lisp example should be followed, but the lisp example is poorly commented.

How about better comments on what each Lisp function needs, does, and returns. In programming terms. Then add it to the task description as the pseudocode to follow? --[[User:Paddy3118|Paddy3118]] 14:08, 23 July 2011 (UTC)
:Why not read the [https://en.wikipedia.org/wiki/QR_decomposition Wikipedia page about QR]? Then if it is not enough, why not open a book on numerical analysis, for example Golub & Van Loan? If a book is asking too much, there are many good online courses about matrix decompositions, [http://www.giyf.com/ Google is your friend]. I'm not claiming that writing a good QR code is easy: it needs some thoughts about geometry, basic matrix operations, and some good ideas about storage (in order to write the Q and R factor in a single n x n matrix plus a vector, as is done in linpack or lapack) -- for example, the Ada solution is not very usable as is, it's just showing how things work with Householder matrix H, in an inefficient way. But programming is not about translating someone else's code, and sometimes it requires some "homework" to understand how to write it yourself. [[User:Arbautjc|Arbautjc]] ([[User talk:Arbautjc|talk]]) 19:55, 6 October 2013 (UTC)

==Draft status==
Also in response to [http://irclog.perlgeek.de/rosettacode/2011-07-03#i_4051779], the simplest way to get a task from draft to non-draft status is to leave a note here asking if there are any further issues that people would like resolved. So I'll ask it; are there any issues or points of confusion in this task that people would like to see resolved? --[[User:Short Circuit|Michael Mol]] 15:34, 4 July 2011 (UTC)
: Please fix the CommonLisp version.
:: I did now. Thanks for your hints. Are there any other objections? --[[User:Avi|Avi]] 17:02, 12 July 2011 (UTC)
::: What is this line of make-householder doing? (beta (/ 2 (mmul (mtp v) v))))
:::: It creates a local variable <math>\beta</math> (which is often used in articles about the QR decomposition) with the value <math>\frac{2}{v^T v}</math>, and is used to create the Householder matrix <math>H = I - \beta \, v v^T</math>. I also could have written the first formula directly instead of that. --[[User:Avi|Avi]] 18:18, 13 July 2011 (UTC)

::::
```lisp
(m- (eye m)
    (.* (/ 2 (mmul (mtp v) v)))
        (mmul v (mtp v))))
```

:::: <math>\beta</math> is a scalar value (since <math>v^T v</math> is a doct product), not a matrix, so making it a array will cause a type error. --[[User:Avi|Avi]] 16:07, 14 July 2011 (UTC)
::::: What you have reverted breaks the code to me. Maybe I am using a different Common Lisp compiler? http://ideone.com/UUsVG

:::::: I have to agree with you once again. In my development version, I used a slightly different matrix multiplication routine (mmul), that turns a 1x1 array, like in the case of <math>v^Tv</math>, to a scalar. The [[Matrix multiplication#Common Lisp|mmul on RC]] does not do that, so your fix was correct, and I will revert my change. Thanks for your effort. --[[User:Avi|Avi]] 09:35, 16 July 2011 (UTC)

: Are there any reasons to keep this draft? The task appears to be clear enough (mathematical, but that's just its nature) and there are multiple correct implementations. –[[User:Dkf|Donal Fellows]] 13:07, 23 September 2011 (UTC)
:: My only complaint is that it's very difficult to grok, with the way the task description is laid out. But that's something someone can come along and clean up. I agree; there's no substantive reason to keep this as a draft. --[[User:Short Circuit|Michael Mol]] 13:37, 23 September 2011 (UTC)



### Mathematica and Matlab

The Mathematica and Matlab examples simply show to use the builtin functions for QR-decomposition.
Is this enough? Or should they implement the algorithm given in the description?
I think they should show the algorithm implemented not just the function called!

== Questionnable intent ==

The task is misleading at best. Some answers successfully compute Householder projections,
but, like the task description, they fail to understand that a QR decomposition is never
computed this way, as this would be too much time- and space- consumming.

Only the vector ''u'' of array ''I - s uu''' is ever stored, together with ''s'' or something equivalent to ''s''.
And both ''Q'' and ''R'' are stored in ''A'' in the process, with only a supplementary vector.

LINPACK and LAPACK are of course no exception, though they handle the question in a slightly
different way [http://stackoverflow.com/questions/3031215/mystified-by-qr-q-what-is-an-orthonormal-matrix-in-compact-form].

And when solving a system given an already computed QR in packed form, there are also ways to do
it in a clever way, not computing the ''Q'' and ''R'' matrices effectively.

Most answers only show bad ways to do the job, and they are just showing programming language
in (bad) action.

While I understand that Rosetta Code is not the place for state-of-the-art algorithms, it should be at least
mentionned that they are indeed very poor in this case. And anyway, a reasonnable QR algorithm is not more difficult
to implement, it just requires some work.

[[User:Arbautjc|Arbautjc]] ([[User talk:Arbautjc|talk]]) 11:05, 30 April 2015 (UTC)
