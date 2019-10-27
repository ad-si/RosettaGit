+++
title = "Talk:Multi-dimensional array"
description = ""
date = 2016-10-27T16:30:02Z
aliases = []
[extra]
id = 18494
[taxonomies]
categories = []
tags = []
+++

==limits of indices/dimensions==

What is the maximum number of dimensions?
:Generic, four and more.

Perhaps a mention of the range of indices?
: 2,3,4,5 or 5,4,3,2 items in each dimension  for this example.

Are negative indices allowed?
:If you have to build it then in general do enough for the task and no more (unless that little extra makes other things simpler).

Do the indices have to be numeric?
:Integer.

Can a range be specified?   
:::::::: declare   array(1900:2050)  
or somesuch syntax.
:If you have it, flaunt it! but don't create it for the task.

What are the limits for the index (number)?
::::::::  x = array(777888999)
:Enough to do the example would suffice.

-- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 07:12, 10 January 2015 (UTC)

:My answers are interspersed with your questions above. The general idea is that if you have to create multidimensional array handling for your language then you should create the minimum to accomplish the task.

:If there is an idiomatic method of dealing with multi-dimensional arrays in the language then that should be used over any new creation; and if there is in-built support for multi-dimensional arrays then use that over all. 

:The idea is really to show how good language programmers would agree to implement the task given prior community history and language features rather than inventing something new, but if this is new to the language community then ... --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 08:47, 10 January 2015 (UTC)

== Column-major order in Fortran ==
I removed something a bit silly in the page. It should probably be discussed here before attempting to give a personal opinion.

First, it's silly to compare Fortran to Algol and wonder why Fortran does not follow the Algol convention: Fortran ''predates'' Algol by several years[1]. It's also silly to mention that MATMUL follow this "convention". It has nothing to do with that. MATMUL multiplies matrices, period. Matrices are stored in column-major order, but they could be stored in any way (diagonal order, or any fancy order), that would not change MATMUL's "interpretation": a matrix is a matrix.

There was apparently a misconception in the comment: one must not confuse array ''storage'' order with array indexing. Maybe the use of some programming languages has given the author of this comment the habit to ''think'' that arrays are naturally stored in row-major order, but this is indeed a convention. Some languages follow another convention. Some libraries use still other storage mechanisms to save place (for sparse arrays, triangular arrays, band arrays...). Still, array indexing will probably always be done like in mathematics, that is, A(i,j) is element at row i and column j, whatever the storage peculiarities may be.

May I remind the reader that this site is about comparing programming languages, not about personal opinions on computer history.

[1] In the very first [http://www.eah-jena.de/~kleine/history/languages/FortranAutomaticCodingSystemForTheIBM704.pdf Fortran manual], dated october 1956, one can see the array storage was already column-major. See page 11. However, one can also read that array were always stored backwards: yet another convention, surprise surprise.

[[User:Arbautjc|Arbautjc]] ([[User talk:Arbautjc|talk]]) 07:15, 25 October 2016 (UTC)

:Well, anyone unfamiliar with Fortran's array ordering who is intent on matrix arithmetic is going to be puzzled. Taking two-dimensional arrays A and B as obviously equivalent to matrices A and B, one can prepare a test prog. that has READ A, and READ B (or equivalently, uses a DATA statement to initialise the arrays), and prepare the layout
  1, 2,
  3, 4
:And indeed, such an input as that will appear as expected with output via WRITE A, and WRITE B. But when the result of MATMUL(A,B) is printed there will be confusion. Yes, a matrix is a matrix, and also, an array is an array. And an array can be regarded as a matrix or a matrix an array. But if you don't know the difference between the mathematician's "row major" order for a matrix and Fortran's "column major" for an array, you won't get far.
:That arrays were stored backwards in memory is a feature of the old style and relevant to assembler programming on say the IBM1130, but this wasn't done on the B6700 for example. Thus, "storage order" doesn't really apply to reverse memory order. Perhaps it would be better to omit "storage order" in favour of emphasis on the consecutive order of input and output.  I was thinking of equivalencing a two-dimensional array to a single-dimensional array. For systems using memory backwards, both arrays would be in memory backwards but one would still regard the single-dimensional array as having a natural sequence 1, 2, 3, ... etc. I have never seen an explanation for this reverse-memory order. Perhaps with index bound violations and COMMON storage starting at high memory a large index would attack lower memory, most probably the user's code area rather than assault the system area in low memory addresses (the skeleton supervisor on the IBM1130 for example) via negative indices which presumably would be less likely. But this is guesswork.

:Similarly with arrays of matrices. Suppose you had an array A(3,2) which you regarded as a matrix, and then you wanted a collection of such matrices: AHORDE(3,2,100) would give you a hundred such while AHORDE(100,3,2) would give you confusion. But explaining this and schemes for passing a part of such a 3-D array to a subroutine expecting a 2-D array I thought too tricky. On the other hand, not explaining that matrices are normally considered as "row major" in mathematics, ''and that Fortran's MATMUL uses that interpretation'' despite its arrays being the reverse leaves a gap. I have never seen an explanation for this Fortran choice, which it seems to me has only introduced trouble. [[User:Dinosaur|Dinosaur]] ([[User talk:Dinosaur|talk]]) 08:36, 25 October 2016 (UTC)

:: It's probably good, for this site, to move discussions of internals on task pages to task pages that are about exposing those internals (emphasis on "task pages" because the corresponding talk pages sometimes wind up like this one). That said, I do understand that there's something of a tradition in talking about such details (and, also, that these traditions all too often get historical issues wrong). --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 09:02, 25 October 2016 (UTC)
<br/>
::''not explaining that matrices are normally considered as "row major" in mathematics'' But they are not! There is no such thing as '''storage''' in mathematics. When you write a matrix by hand, elements are written in a square. There is no mathematical reason prefer rows to columns: rows for equations? But then columns for basis vectors. And if you actually believe that has the slightest impact on Matmul, then I fear you don't understand at all the difference between storage and the mathematical definition of a matrix product. The latter is perfectly defined and universally accepted (it's also a matter of convention, but it's well established). This may come from an abuse of row-major languages: maybe this leads you to think it's natural and everything else is not. This is plain wrong.<br/>
::Actually, mathematically, an n-dimensional array is nothing but an application from E_1 x E_2 ... x E_n to some field K, where E_i = {1 ... m_i}. There is no idea of storage or order whatsoever. Imagine you write the elements in a spiral, on a sphere or whatever, it does not change, and it has nothing to do with, this application. Writing a matrix as a square is a convenience, not a fundamental part of the mathematical object. And as already said, it does not induce any idea of storage anyway. ''You'' decide to read elements row by row. It's ''your'' convention, not a mathematical convention.<br/>
::Regarding puzzled users: anyone doing numerical analysis should be aware that array storage depends on language and/or mathematical library. Even when programming in C for numerical linear algebra, it's likely you will make use of a vendor optimized LAPACK library. Also, Fortran is not the only one to use column-major order. Matlab, VBA and R do, for instance. Of course it comes from Fortran roots, but nevertheless, they do. It's the right place in RC to tell the difference, not to express your personal opinion that you don't understand why it's so. After all, one could also ask, why later languages did not follow the Fortran convention?
::As to actual reasons to prefer column-major or row-major? I don't know the original reason. There may be actually none, in fact. Since it's mostly equivalent (regarding implementation of numerical algorithms), it's perfectly acceptable to choose one or the other. And to see why it's equivalent, I suggest having a look at ''Matrix Computations'' (Golub & Val Loan). They give a good discussion on row-wise and column-wise algorithms.
::[[User:Arbautjc|Arbautjc]] ([[User talk:Arbautjc|talk]]) 11:11, 25 October 2016 (UTC)
::: Most statements using "normal" and "mathematics" in the same sentence are superficially plausible, but actually wrong (unless we are talking about something related to normal vectors, for example).
:::Anyways, the task asked for some description about contiguous storage issues which is probably what led to this tangent.
:::Puns intended. --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 12:35, 25 October 2016 (UTC)
::::Agreed. I changed the task to explicitly ask about this storage order: it is not specific to Fortran, and whatever the convention may be, it has to be known in order to use the language correctly, for cross-language calls, memory cache optimization and possibly for disk storage if the memory is mapped directly to a file.
::::Anyway, what was written was wrong, and I don't think it's a good thing to have wrong comments on a site supposed to be useful for students, in a task supposed to be tricky to newcomers. It's still far too lengthy to my state, by the way. Looks like an old-timer enjoys remembering the good old times of uppercase programs limited to 72 columns. But all of this is obsolete and unlikely to be of any use to a new Fortran programmer, or to a C programmer trying to call a Fortran library. I like computer history a lot, but when I come on RC to actually learn something (and I do !), I am looking for accurate technical details, not personal opinions about history. [[User:Arbautjc|Arbautjc]] ([[User talk:Arbautjc|talk]]) 12:55, 25 October 2016 (UTC)

:I agree that I should not have mumbled that mathematics uses "row major" since in the Platonic space the issue of storage doesn't arise [from the realisation that the READ/WRITE problem noted wouldn't arise were the "row-major" style used; by back-formation came the mental burp that mathematics is "row-major". Apologies to Plato ''et seq''.], but I was trying to explain the difference between the expectations engendered by mathematical notation and layout, and the "formula translated" with an eye to the many who have been disconcerted by the results of WRITE(...) A after a READ(...) A followed by some calculations. And indeed there are computations that may be expressed row-wise or column-wise and there may be mathematical reasons to prefer one over the other or to regard both as equivalent. But when staring at the realisation of one in Fortran, where when the mathematics calls for (i,j) and (j,i) one may find (j,i) and (i,j) perhaps with DO J...; DO I... when DO I...; DO J... were expected, but that after some inspection seems to be consistent and indeed correctly implementing the algorithm, so, why? Aha, someone has discovered "thrashing" and been driven to taking steps... [[User:Dinosaur|Dinosaur]] ([[User talk:Dinosaur|talk]]) 01:19, 26 October 2016 (UTC)
::I was probably too unkind, sorry for this. Patience is not my primary quality. Anyway, let me show you why all this discussion is really pointless, on an example: gaussian pivoting, to solve the equation AX=B, where A is (n,n), X is (n,p) and B is (n,p). A first attempt could be this (I removed row swapping to shorten the code, as it won't change the idea):
::
```fortran
do i = 1, n - 1
    do j = i + 1, n
        s = a(j, i) / a(i, i)

        do k = i + 1, n
            a(j, k) = a(j, k) - s * a(i, k)
        end do
        
        do k = 1, p
            b(j, k) = b(j, k) - s * b(i, k)
        end do
    end do
end do

do i = n, 1, -1
    do j = i + 1, n
        s = a(i, j)
        do k = 1, p
            b(i, k) = b(i, k) - s * b(j, k)
        end do
    end do
    
    do k = 1, p
        b(i, k) = b(i, k) / a(i, i)
    end do
end do
```

::It works, but as you can see, all inner loops run on rows: it would be good in C or another "row-order language". It's very easy to write the same thing with column-order inner loops:
::
```fortran
do i = 1, n - 1
    do k = i + 1, n
        s = a(i, k) / a(i, i)
        a(i, k) = s
        do j = i + 1, n
            a(j, k) = a(j, k) - s * a(j, i)
        end do
    end do
end do

do k = 1, p
    do i = 1, n
        s = b(i, k) / a(i, i)
        b(i, k) = s
        do j = i + 1, n
            b(j, k) = b(j, k) - s * a(j, i)
        end do
    end do

    do i = n, 1, -1
        s = b(i, k)
        do j = i - 1, 1, -1
            b(j, k) = b(j, k) - s * a(j, i)
        end do
    end do
end do
```

::There is absolutely nothing awkward in this alternate code. The number of loops is identical. However, with n=p=1000, to invert the matrix A (with B=eye(1000)), it's 10 times faster. Not 10 percent, but 10 times. So, basically, you need to do this correctly. The problem would be identical in C: one of these algorithms should be 10 times faster, but it will be the first. The problem does not lie in introducing risk or complication in the program: they are equally complicated. There is no confusion either. You don't have to actually know how the matrix is stored to understand the algorithm. You have to know this, however, to understand why one is much slower than the other.<br/>
::I would like to add: when writing programs, I almost never enter a matrix with READ/WRITE, but they are built from data that is read from a file (or passed by a DLL call from a higher level language such as Excel/VBA or R).
::There is no source of confusion: in A(i,j), i is the row index as one would expect, and j the column index. You will be confused only if you think the storage dictates the interpretation. But it does not. You don't have to worry about the storage except in those situations:
::*Mixing languages that do not use the same storage order (it happens if I call Fortran code from SAS/IML for instance).
::*Optimization: cache misses cost '''much'''. Always manage to get your inner loops to work on contiguous data, whenever it's possible.
::*Disk storage, although usually it's not a problem: either you store quick and dirty memory dumps, and you don't care how it's stored, or you store data in a widespread format for interchange, and either you would use a library, or you will have to take care of many details beyond storage order, anyway.<br/>
::[[User:Arbautjc|Arbautjc]] ([[User talk:Arbautjc|talk]]) 16:44, 26 October 2016 (UTC)

:::I quite agree that when one is writing A(i,j) and all the variations in the formulae of interest, the issue of column/row ordering is of no importance to the expressions in the code and the link between the formulae translated and the mathematics. As for the input and output of matrices (and arrays generally) I suspect that the processes you mention involve explicit statement of indices and the nesting of loops, possibly hidden in service routines or routines that construct the desired matrices from other data, again with explicit indexing. It is only if one is tempted by the simplicity of READ(...) A or DATA A/.../ that difficulty will arise in Fortran, exacerbated by WRITE(...) A indeed showing the order as input. Such as an attempt at debugging by inserting a simple statement at various stages while attempting to understand the workings of the algorithm, especially when odd results are  appearing.
:::By "disc storage" I was meaning the equivalent of what is now called virtual memory, though one could indeed be messing with data on disc. With regard to thrashing, one can distinguish between cpu time and elapsed time and I'm going to suggest that the ten-times-longer time is in terms of elapsed time. The row/column processing in cpu time will be much the same (except for on-chip memory complications), it is just that there are these damnable delays between bursts of cpu time as thumbs are twiddled in impatience. But for batch jobs, where the charge is for cpu time and you have to wait until tomorrow for a ten-minute run to be returned... In a general situation, there can be many storage hierarchies. Aside from cpu registers, The Elliot 503 had main memory, slower "backing memory" and disc. The Burroughs 6700 had main memory, disc memory, and ... magnetic tape (though I never heard of this being triggered), the IBMpc et al have on-chip memory (and of various sorts), main memory, and disc. Timing test variations can be tiresome, with a restart required.
:::Changing the order of loop nesting can be risky depending on the algorithm. I'm thinking of the LU decomposition for example. For this, one might prefer swapping A(i,j) for A(j,i) to swapping the order of loops but it is a decade since I got my version to work so a lot of refreshment would be needed before I could be sure of the issues. I did demonstrate that the FOR ALL statement was indeed incorrect in one usage (as in <code>A(I,J) = A(I,J) - SUM(A(I,1:I - 1)*A(1:I - 1,J))	!Note the reach-back changes.</code>) thus confirming that the FOR ALL statement did manifest its special interpretation. 
:::In another example, an attempt at modelling sub-surface electrical conductivity via measurement of electric field polarisation along the surface, the code as supplied was the usual stodge 
```Fortran
      DO 120 I=2,LP
      DO 120 J=2,LP
      C=F(I,J+1)*C1(I,J)+F(I,J-1)*C3(I,J)+F(I+1,J)*C4(I,J)+F(I-1,J)*C2(I,J)
      P=G(I,J+1)*C1(I,J)+G(I,J-1)*C3(I,J)+G(I+1,J)*C4(I,J)+G(I-1,J)*C2(I,J)
      TEMPF=(C*A(I,J)-B(I,J)*P)/(A(I,J)**2+B(I,J)**2)
      TEMPG=(A(I,J)*P+C*B(I,J))/(A(I,J)**2+B(I,J)**2)
      RESIDF=ABS(TEMPF-F(I,J))
      RESIDG=ABS(TEMPG-G(I,J))
      IF (RESIDF.GT.BIGF) BIGF=RESIDF
      IF (RESIDG.GT.BIGG) BIGG=RESIDG
      F(I,J)=TEMPF
120   G(I,J)=TEMPG
```
 (with two lines extending beyond col. 72, but I've appended the continuation as I've never actually liked the limit despite me being born on the sixth of December) - swapping (i,j) for (j,i) would be asking for mistypes, especially in following through for all subsequent usages as in 
```Fortran
      DO 110 J=2,LP
      AETAL=SQRT(REGION(I,J-1))
      AETAR=SQRT(REGION(I,J))
      AETA=(AETAL+AETAR)/2.0
      ROOTI=CMPLX(1./SQRT(2.),1./SQRT(2.))
      E0=CMPLX(F(I,J),G(I,J))
      E1=CMPLX(F(I+1,J),G(I+1,J))
      DERIVZ=(AETA*ROOTI*(2.0*E1-E0*(CEXP(AETA*K(I)*ROOTI)+CEXP(-AETA*K(
     1I)*ROOTI))))/(CEXP(AETA*K(I)*ROOTI)-CEXP(-AETA*K(I)*ROOTI))
      DPHASE(J)=ATAN2(G(I,J),F(I,J))
      DPHAHY(J)=ATAN2(REAL(DERIVZ),-AIMAG(DERIVZ))
      DPHAHZ(J)=0.0
      IF (((G(I,J+1)-G(I,J-1)).NE.0.0).OR.((F(I,J+1)-F(I,J-1)).NE.0.0))
     1DPHAHZ(J)=ATAN2(F(I,J-1)-F(I,J+1),G(I,J+1)-G(I,J-1))
      AME(J)=SQRT(F(I,J)**2+G(I,J)**2)
      AMHY(J)=(1./OMEGA)*CABS(DERIVZ)
      AMHZ(J)=(1./OMEGA)*(SQRT(((F(I,J+1)-F(I,J-1))/(H(J)+H(J-1)))**2+((
     1G(I,J+1)-G(I,J-1))/(H(J)+H(J-1)))**2))
  110 APPRES(J)=(2.0/FREQ)*((AME(J)/AMHY(J))**2)
```

:::Which is why I mentioned risks. Swapping the loop nesting of the first lot would be better, except that this was an oriented problem. Although after attaining convergence the result would be the same, the calculation was stopped well short of convergence (due to cost! 1·2 seconds/loop cpu, 400 iterations not enough) and so the orientation counted. Indeed, after my fiddlings reduced the loop time to about 0·7 secs and incidentally removed a left-to-right bias in the results, I was asked to reinstate the left-to-right bias "So as to maintain compatibility with previous results." - and this by a doctorate student. Ah well.
:::One of my ploys was to waste some space in array storage, as in 
```Fortran
       DIMENSION A(41,41),B(41,41),C1(41,41),C2(41),C3(41,41),C4(41)
C ABOVE ARRAYS NEED ONLY 40,BUT MADE 41 TO MATCH F,G.(SPEED IN ACCESS).
       COMMON F(41,41),G(41,41),CC(41,41),R(40,40),H(40),D(40),SK,FRQ,NN
```

:::Some array elements were for "between" other array elements as with fence lengths and fence poles. The idea was to encourage the compiler to recognise equivalent indexing. I hadn't extended this to array R though when I abandoned further effort. My next ploy would have been to consider equivalencing the two-dimensional arrays to one-dimensional, to take advantage of such details as R(I - 1,J) being 40 elements away from R(I,J), though my earlier changes had reduced the frequency of such offset accesses so that delicate testing would be required for assessing any speed changes. Oddly, I had found that the optimising compiler produced code that ran slower than unoptimised.
:::So, I've been wondering about mentioning that n-dimensional arrays can be equivalenced to m-dimensional arrays... [[User:Dinosaur|Dinosaur]] ([[User talk:Dinosaur|talk]]) 11:47, 27 October 2016 (UTC)
::::Regarding elapsed time: it's not relevant. Use SYSTEM_CLOCK or CPU_TIME, and you will get a ratio of 10 (and even 20 with Intel Fortran, the other one was GNU Fortran). The problem is, using a row-wise algorithm in Fortran, there are many cache misses, thus the CPU waits for data. Of course it's not "pure CPU time", but such a measure would be totally useless: when working with large data, the memory pattern of your program has a big impact, it's not just a matter of CPU cycles (you can use "cache touch" machine instructions to load before you compute, but it will always be much faster with contiguous data). By the way, the code I wrote above is almost exactly an LU decomposition (the second one was copy-pasted from two subroutines doing the LU decomposition for the first, and solving the two triangulat systems for the second). Gaussian elimination is more or less the same thing as an LU decomposition.
::::Of course, permuting loops invloves changes to the body of the loops. This is part of algorithm design, not implementation.
::::
::::Regarding FORALL, the interpretation is relatively simple, as with other array statements and functions introduced by Fortran 90: never rely on side effects, hence evaluate all right-hand side values before any assignment. This would involve allocating temporary storage and copying in a rigid interpretation of this rule. In many situations, however, the compiler is able to do better than that. FORALL is indeed useful only when the result does not depend on the evaluation order. Anyway, given the ability of modern compilers, I don't regard FORALL as very usueful, but see also [http://stackoverflow.com/questions/8602596/fortran-forall-restrictions here] and [http://stackoverflow.com/questions/4122099/do-fortran-95-constructs-such-as-where-forall-and-spread-generally-result-in-fa here].
::::[[User:Arbautjc|Arbautjc]] ([[User talk:Arbautjc|talk]]) 16:13, 27 October 2016 (UTC)
