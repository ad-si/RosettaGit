+++
title = "Talk:LU decomposition"
description = ""
date = 2018-11-02T20:56:13Z
aliases = []
[extra]
id = 12100
[taxonomies]
categories = []
tags = []
+++

== Python example contains error ==
The Python example has a divide by zero error for the matrix

b = [[1, 1, 1, 1], [1, 1, -1, -1], [1, -1, 0, 0], [0, 0, 1, -1]]

although a LUP decomposition exists:
[http://www.wolframalpha.com/input/?i=lu+decomposition+{{1%2C+1%2C+1%2C+1}%2C+{1%2C+1%2C+-1%2C+-1}%2C+{1%2C+-1%2C+0%2C+0}%2C+{0%2C+0%2C+1%2C+-1}}]

The permutation matrix has to be updated at each step, but that will make the code a lot more complicated.

== Example 2 pivot matrix seems to be wrong ==
The pivot matrix in example 2 should also swap the last two rows of the current resulting pivoted matrix: A'(3, 3) = 2 while there is a 7 right beneath it. Here is the result of the multiplication of the two matrices on  [http://www.wolframalpha.com/input/?i=matrix+multiplication+calculator&f1={{1%2C0%2C0%2C0}%2C{0%2C0%2C1%2C0}%2C{0%2C1%2C0%2C0}%2C{0%2C0%2C0%2C1}}&f=MatricesOperations.theMatrix1_{{1%2C0%2C0%2C0}%2C{0%2C0%2C1%2C0}%2C{0%2C1%2C0%2C0}%2C{0%2C0%2C0%2C1}}&f2={{11%2C9%2C24%2C2}%2C{1%2C5%2C2%2C6}%2C{3%2C17%2C18%2C1}%2C{2%2C5%2C7%2C1}}&f=MatricesOperations.theMatrix2_{{11%2C9%2C24%2C2}%2C{1%2C5%2C2%2C6}%2C{3%2C17%2C18%2C1}%2C{2%2C5%2C7%2C1}}&a=*FVarOpt.1-_**-.***MatricesOperations.theMatrix3---.*-- wolfram alpha]

The pivot matrix I propose is {{1,0,0,0},{0,0,1,0},{0,0,0,1},{0,1,0,0}}.

I did not change the article because it seemed very strange that I would be the first to see this and I therefore wonder if I'm not wrong.

== Re: Example 2 pivot matrix seems to be wrong == 

I noticed this too. In the implementations it appears that the max pivot values are taken from the original A matrix without swapping. When the max pivot is being found for row 3 of A the choices presented are row 3 [3, 7, 18, 1] and row 4 [2, 5, 7, 1], since row 3 contains the largest pivot value the third row of the current permutation matrix is swapped with itself, which contains [0, 1, 0, 0] from the initial swap with row 2. It's strange, but a lot of the algorithm implementations seem to follow that trend and multiplying by the permutation matrix instead of swapping inline with the permutation matrix.

I think the permutation matrix you proposed makes more sense as well.

== Matlab code is wrong ==

Please lock at the following example
A=[1 2 -1 0;2 4 -2 -1; -3 -5 6 1; -1 2 8 -2]
the code returns nonsense for U, namely

U =

         0         0         0   -1.0000
    0.5000    1.0000   -0.5000         0
   -0.1429         0    1.0000    0.2857
    0.1563         0         0    1.0000

I presume the reason is the permutation matrix. I will try to correct it.
