+++
title = "Talk:Deconvolution/1D"
description = ""
date = 2010-02-28T11:56:42Z
aliases = []
[extra]
id = 6109
[taxonomies]
categories = []
tags = []
+++

==Pseudo-code please==
Could you give some pseudo code that solves the problem please. Going from the presented maths to a way to solve it is not straight forward. --[[User:Paddy3118|Paddy3118]] 19:58, 22 February 2010 (UTC)
: Actually it's not too bad if you use the reduced row echelon form task solution; you just build a matrix in an obvious way from the math as presented, run it through the RREF solver, and pick the answer out of the top of the final column. The (brand new) Tcl solution to this task ought to help (lord knows, the Ursala solution doesn't...) –[[User:Dkf|Donal Fellows]] 10:13, 23 February 2010 (UTC)

:: Ta Donal, that was just the start I needed! --[[User:Paddy3118|Paddy3118]] 00:19, 24 February 2010 (UTC)

:: And thanks Sluggo for [[Talk:Deconvolution/2D+#one dimension|this explanation]]. --[[User:Paddy3118|Paddy3118]] 06:16, 24 February 2010 (UTC)

==Method is mandatory ?==

I wonder... since the problem is easily solved by FFT, why bother with a linear system ? Solution will be much slower, and may introduce much rounding errors depending on method use for solving the system. I chose the FFT method for the R code. If it's really needed I'll write a "linear system" later... Should not be too difficult with matrix capabilities of R. [[User:Toucan|Toucan]] 16:18, 27 February 2010 (UTC)

: The method isn't mandatory (and in fact two different methods are suggested) so I think that showing off how to do it with FFT would be fine, so long as you also explain what it is doing (i.e., act as a guide so that other languages can also adopt the solution). If you could also do the same for the [[Deconvolution/2D+|higher-order case]], that'd be even cooler. –[[User:Dkf|Donal Fellows]] 11:56, 28 February 2010 (UTC)
