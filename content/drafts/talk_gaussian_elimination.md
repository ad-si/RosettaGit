+++
title = "Talk:Gaussian elimination"
description = ""
date = 2017-10-26T16:22:39Z
aliases = []
[extra]
id = 11392
[taxonomies]
categories = []
tags = []
+++

== Task relationship? ==

What is the relationship of this task to [[LU decomposition]]? –[[User:Dkf|Donal Fellows]] 09:01, 12 February 2012 (UTC)
:Basically, Gaussian elimination is the same as LU decomposition followed by backsubstitution. To solve the system AX=B with LU decomposition, you multiply on the left by P: PAX=PB, hence LUX=PB. Then you solve first the lower triangular system LZ=PB, which gives a vector Z (or matrix if B is a matrix, which happens if you want to solve several systems at the same time with the same matrix A). Then you solve the upper triangular system UX=Z, and you are done.
:The difference lies in the order the operations are done: in Gaussian elimination, the lower triangular matrix L is not stored, since the computation of Z (that is, the solution of the lower triangular system LZ=PB) is done once and for all in the first step. Likewise, the permutation matrix P is not needed because the rows of B are swapped at the same time the rows of A are swapped. The backsubstitution step is identical in Gaussian elimination and LU decomposition.
:The Stata section shows both methods.
:[[User:Eoraptor|Eoraptor]] ([[User talk:Eoraptor|talk]]) 16:22, 26 October 2017 (UTC)
