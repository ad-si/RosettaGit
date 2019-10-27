+++
title = "Talk:Polynomial regression"
description = ""
date = 2016-12-18T18:13:28Z
aliases = []
[extra]
id = 2877
[taxonomies]
categories = []
tags = []
+++

In which norm the approximation has to be done? Euclidean? C-norm? The basis seem to be x<sup>n</sup>? --[[User:Dmitry-kazakov|Dmitry-kazakov]] 14:10, 4 June 2008 (MDT)

Any norm could be used. The task is not about mathematics. This task is intended as a subtask. The produced polynoms are used as labels for simple charts, therefore any norm will suffice. You may create a new task if a more formal task statement is desirable.
The basis is 1, x, x<sup>2</sup>, x<sup>3</sup>, ..., x<sup>n</sup>. `n' is supposed to be known. -- [[User:Geka Sua|Geka Sua]] 08:04, 7 June 2008 (MDT)


==About fortran==
It is quickly coded, and while trying to remember things. It works anyway, but I suppose there's a better way. the method I've followed is straightforwardly from [http://mathworld.wolfram.com/LeastSquaresFittingPolynomial.html Wolfram.com] --[[User:ShinTakezou|ShinTakezou]] 23:58, 18 December 2008 (UTC)

----

Good work with the Fortran routine. However, the scaling of the independent variable is missing without which the fitting will fail in many cases. For this purpose one needs the ''mean'' and ''standard deviation'' of the independent variable (vx in the code). Now scale vx to vxcopy and do everything using vxcopy. The code should be changed as follows. I am using this modified version for big applications in my work. 
 
    allocate(ipiv(n))
    allocate(work(lwork))
    allocate(XT(n, size(vx)))
    allocate(X(size(vx), n))
    allocate(XTX(n, n))
 
    vxcopy = vx                          !=== make a copy of the independent variable
    mu = stdmean(size(vxcopy), vxcopy)   !=== call the function stdmean and get mu(1) - mean, mu(2) - standard deviation
    vxcopy = (vxcopy - mu(1)) / mu(2)    !=== scale the coordinates 

    ! prepare the matrix
    do i = 0, d
       do j = 1, size(vx)
          X(j, i+1) = vxcopy(j)**i        !=== do the rest with vxcopy
       end do
    end do

--[[User:Raghu|Raghu]] 12:31, 12 July 2011 (UTC)

== Fortran matmul use ==

I am re working the Fortran example to use in a Delphi project.

The matrix multiply at the end of the example looks to be in the incorrect order.

I believe it should be.


		polyfit = matmul( XTX, matmul(XT, vy))



I would appreciate if somebody with the Fortran language available could confirm.

== FORTRAN DGETRx ==

Where can I read what
call DGETRF(n, n, XTX, lda, ipiv, info)
call DGETRI(n, XTX, lda, ipiv, work, lwork, info)
do? ..[[User:Walterpachl|Walterpachl]] ([[User talk:Walterpachl|talk]]) 18:13, 18 December 2016 (UTC)
