+++
title = "Talk:Deconvolution/2D+"
description = ""
date = 2019-02-03T15:37:24Z
aliases = []
[extra]
id = 6111
[taxonomies]
categories = []
tags = []
+++

== higher dimensional deconvolution ==
I got interested in higher dimensional deconvolution when contacted by someone about using it as a way of looking for trading indicators in financial time series. I set this task as an example of something that I'm speculating can be done well with functional and array processing languages, but only with difficulty otherwise, which I hope someone will weigh in to confirm or refute. In case anyone wants to know, consistent [[http://drop.io/yatxacy test data]] were generated partly by this higher dimensional convolution function,

```Ursala
conv = +^|(~&x+,*DrlDSNiCK9xxSNiCK9K7iFS+ *)=>times+ **+ *K7|\x+ iota; * ! plus:-0
```

invoked as <code>(conv d)(h,f)</code> with dimension <code>d &gt; 0</code> and conforming <code>h</code> and <code>f</code>. (This function essentially subsumes the [[Image convolution]] task as a special case with <code>d = 2</code> and <code>|h| = 3</code>.) I suggest a development methodology based on warming up with [[Deconvolution/1D]], then hand coding the solutions for the next few dimensions, and then looking for the pattern.

--[[User:Sluggo|Sluggo]] 03:24, 23 February 2010 (UTC)


I am new here, but I think the definition of convolution is wrong and should be
:<math>G(n_0, \dots, n_{d-1})=\sum_{m_0=-\infty}^{\infty}\dots\sum_{m_{d-1}=-\infty}^{\infty}F(m_0, \dots, m_{d-1})H(n_0-m_0, \dots, n_{d-1}-m_{d-1})</math>

in accordance with [[http://en.wikipedia.org/wiki/Convolution#Definition|Wiki convolution]]

--[[User:R.E. Boss|R.E. Boss]] 08:45, 3 march 2010

My bad. It's fixed now.

--[[User:Sluggo|Sluggo]] 09:21, 3 March 2010 (UTC)

== Help ==
How do you help someone who knows how to program, but not how to de/convolve? I would like to attempt a solution but, not knowing Ursala, the example code given is of no help, and the maths only helps those that know rather than being a means to communicate to those outside the field. In [[Averages/Pythagorean means]] I was careful to add textual descriptions of the maths to help those people who know how to program, but whose eyes might start to glaze over when confronted with the equations. In fact, the second part of the task: "Show that the ..." was initially below the block of equations, and several implementers missed it completely in their haste to gloss over the equations.

Your task description is even more reliant on equations, and might be helped by much more non-math description - such as a scripting-like pseudo-code. --[[User:Paddy3118|Paddy3118]] 07:51, 23 February 2010 (UTC)

== Informal description ==

It can't hurt to try. Here is an informal description of an algorithm for it, with
preference given to clarity over brevity. I'll record it here rather than
in the task specification, because this isn't the only way to solve it
or the best.


###  one dimension 


First, the <code>band</code> function takes a pair of lists <math>(g,f)</math> representing the
spaces you want to deconvolve. The length of <math>g</math> is <math>|g|</math> and the length
of <math>f</math> is <math>|f|</math>, and <math>|g| \geq |f|</math>. The <code>band</code> function
returns a matrix with <math>|g|</math> rows and <math>|h|</math> columns, where the length of the
answer, <math>|h|</math>, is given by
<math>|g| - |f| + 1</math>. Assuming all indices are numbered from 0, the entry in the
<math>i</math>-th row and the <math>j</math>-th column of this matrix is <math>f_{i+j}</math> if
<math>i \geq j</math> and
<math>i+j < |f|</math>, but 0 otherwise. How you write the code to build it is up
to you. It doesn't have to be done with list combinators if the
language is better at handling arrays, but when it's done, the matrix
should look like the coefficient matrix for the system of equations
shown in the [[Deconvolution/1D]] task specification.

For the one dimensional case, the rest is easy. Plug this matrix and
the vector <math>g</math> into a linear equation solver if the language has one, or
the function from the [[Reduced row echelon form]] task otherwise, and
you're done. The only minor complication is that you have to delete
some of the equations if the solver can't cope with having more
equations than unknowns. (In practice, it's better for noise reduction
if it can.) You can delete all but the first <math>|h|/2</math> from the beginning
and last <math>|h|/2</math> from the end (or one extra from somewhere if <math>|h|</math> is an
odd number). If the solver precludes over determined systems
and the language also doesn't let you delete rows from matrices, you might
have to write the code in the <code>band</code> function to build it that way in
the first place.


###  two dimensions 


For the two dimensional case, each member of <math>f</math> and <math>g</math> is itself a
vector rather than a scalar. Proceed using the same algorithm as in
the <code>band</code> function above to construct a matrix <math>A</math> of vectors following
the same pattern, with which each <math>i,j</math>-th element of <math>A</math> being either a
vector from <math>f</math>, or a zero vector of conforming length.

I'm not saying it can't be done, but I'm not sure how pruning the matrix generalizes to higher dimensions, so the rest of this description assumes the solver copes with over determined systems. Such a solver is readily available in the [[http://www.netlib.org/lapack Lapack]] library, which is callable from many languages.

Next, pair up each row of the matrix-of-vectors <math>A</math> with the corresponding member of
<math>g</math>. Since <math>A</math> is a matrix of vectors, each row of it can be considered a matrix
of scalars on its own. For each <math>i</math> ranging from 0 to <math>|g|-1</math>, the pair <math>(g_i,A_i)</math> of the <math>i</math>-th member of <math>g</math> and the <math>i</math>-th row of <math>A</math> will consist of a vector <math>g_i</math> and a matrix <math>A_{i}</math>. Pair up a copy of <math>g_i</math> with each row of <math>A_{i}</math> and apply the 1 dimensional <code>band</code> function to each member of the list of all such pairs. This step will yield a list of matrices (of scalars) each with <math>|g_i|</math> rows. Flatten this list of matrices into a single matrix with <math>|g_i|</math> rows by concatenating them row-wise (i.e., horizontally). Having done so for all <math>(g_i,A_i)</math> pairs, flatten the resulting list of <math>|g|</math> matrices into one single matrix having <math>|g|*|g_0|</math> rows by concatenating them column-wise (i.e., vertically). This matrix should then be fed to the linear equation solver along with a flattened copy of <math>g</math>.

The result returned by the linear equation solver will be a list of
scalars, which must be unflattened into a list of vectors in the two
dimensional case. Do so using whatever algorithm you prefer, provided
that the length of each vector in the result is <math>|g_0| - |f_0| + 1</math>. (If
the length of the result returned by the linear equation solver is not
a multiple of this number, you've done it wrong.)


###  three dimensions 


For three dimensions, <math>f</math> and <math>g</math> will be lists of matrices. Begin as
above, but in this case populating the zero entries of <math>A</math> with
conforming zero matrices rather than vectors or scalars. When you get
to the point of operating on each <math>(g_i,A_i)</math> pair, it turns out not to
be a vector and a matrix of scalars as in the two dimensional case,
but a list of vectors and matrix of vectors. At this point, pair up a
copy of <math>g_i</math> with each row of <math>A_{i}</math>, and treat each of the pairs as an
instance of the two dimensional problem. Construct the input matrix to
the linear equation solver corresponding to each of these subproblems as
explained above, but don't solve it. Instead, flatten them into a
single matrix by concatenating them horizontally. Repeat for each <math>i</math>,
flatten the results vertically as explained above, and feed resulting
data to the solver. (The top level <math>g</math> will have to be flattened twice
for the solver for the three dimensional case.) Take the solution and unflatten it first into
sublists of the innermost dimension <math>|g_{00}| - |f_{00}| + 1</math>, and then
unflatten the intermediate result further into sublists of length <math>|g_0| - |f_0| + 1</math>.


###  general 


A four dimensional problem instance is to three as three is to two,
and finishes with three unflattenings. Higher dimensions are
analogous. Whether the language has suitable abstraction mechanisms
to express a general form for this algorithm is part of what the task
is intended to establish.

-- [[User:Sluggo|Sluggo]] 02:08, 24 February 2010 (UTC)
