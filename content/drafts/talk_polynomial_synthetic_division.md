+++
title = "Talk:Polynomial synthetic division"
description = ""
date = 2015-07-08T19:44:54Z
aliases = []
[extra]
id = 19228
[taxonomies]
categories = []
tags = []
+++

== Dup? ==

I think this task is a duplicate of [[Polynomial long division]]

That said, when I use an implementation from that page to divide 1, -12, 0, -42 by 1, -3 I get a result of 2.44444, -4.66667, -14 and a remainder of -1.44444 (or quite similar numbers expressed as fractions with 9 in the denominator if I ask for exact results). This is because that implementation places the constant on the left hand side and powers increase to the right. If I reverse the representation of the polynomials I get an answer similar to that of the current python example here.

But that just illustrates that the task is also underspecified (since the task description says nothing about this issue - though it's true that if I follow the seventh link in the current version of the proposed task page I do find a representation where the constant coefficient is rightmost and powers increase to the left).

Anyways, I think maybe the best approach here would be to merge useful content onto the other page and create a #redirect link so that either page title will get to the content. Does that seem reasonable? --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 23:16, 5 June 2015 (UTC)

: Task itself shouldn't need to specify how a polynomial is represented, since growing power in either direction could make sense, so we could just let the code providing the interface choose. I personally would prefer zero-th term in the front because it would make infinite power series possible, but that clearly is not the intended goal here.  The real issue is, long division and synthetic division may look different with pen and paper, there doesn't seem to be a meaningful difference in terms of math or computer implementation: one subtracts, the other inverts and adds, that's about it.  I agree that this looks like a dup. --[[User:Ledrug|Ledrug]] ([[User talk:Ledrug|talk]]) 03:12, 6 June 2015 (UTC)

:: Ok, so, since it seems to be a dup, what's the path forward? Also, if we are going to be order agnostic, shouldn't that mean that the implementation must specify what it has chosen? (Ok, that will not matter here, if the page is going to be replaced with a #redirect, but should that point be made on the other task?) --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 11:36, 6 June 2015 (UTC)

::: I am the author of the first draft with Python code. I do not agree this is a dup: the computational complexity is totally different between the two (O(n*m) here while the other is recursive until you go down to the quotient, thus roughly O(n*m*q) where n and m are the length of both polynomials and q the recursion depth), and the approach is quite different (we only work on the coefficients instead of accounting for the degrees in a recursive fashion). This can be seen as an extension of the Horner's scheme (which is commonly used to evaluate polynomials). To my knowledge, synthetic division is the fastest algorithm ever to divide polynomials, I think this reason alone is sufficient to promote a page dedicated to it. Of course, the result is the same as Polynomial long division (and that's the goal), but this algorithm is a lot more clever and of course they bear some ressemblance, just like different sorting algorithms, but the time complexity isn't the same at all. What may confuse you is that on the page for Polynomial long division, some snippets are actually implementing synthetic division instead of polynomial long division, so I guess it would be more logical to move these snippets here instead of the other way around. --[[User:Lrq3000|Lrq3000]] ([[User talk:Lrq3000|talk]]) 06:38, 14 June 2015 (UTC)

::: About the ordering of coefficients, I have added a note about that. Indeed, I reused the exact same example input from the Polynomial long division page, but reversing the order. I think this ordering is a better fit to easily implement Synthetic Division. The loops could be reversed inside the function but I guess this would just make things more confusing, but that's possible if you really want to follow the same convention. --[[User:Lrq3000|Lrq3000]] ([[User talk:Lrq3000|talk]]) 06:49, 14 June 2015 (UTC)

:::: Uh....

:::: So, ok, the other task includes some psuedo-code which will solve the task requirements. But I know that I entirely ignored that pseudo-code when I was implementing that task. And, if nothing else, my implementation was not recursive (except in the sense that we can think of numbers as representing recursive processes).

:::: And, in general, I try to ignore statements about the details of how things should be done and instead try to focus on what happens to the data, when implementing tasks here (or, anywhere). Any other approach doesn't really make sense to me. The differences between languages often enough are more significant than such algorithmic details. (For example, when you get to thinking about what optimizing compilers can do, or when you get into entirely different modes of notation.)

:::: So, if that's the only difference, I'd be inclined to say maybe just bring this entire page (along with the implementation) over to the other task page, and update the task description to be more clear. --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 14:30, 14 June 2015 (UTC)

::::: This is NOT an implementation detail, it's a difference in the algorithm that drastically reduce the number of calculations required to find the solution. The [https://en.wikipedia.org/wiki/Computational_complexity_theory computational complexity] is different, so that asymptotically (ie, when using big polynomials), you will never reach the same performance as Synthetic Division if you use Long Division, no matter how you implement it or the language you use. Avoiding the recursive loop of Long Division is possible, I also did it, but it's a simple loop flattening trick, it doesn't change the computational cost (you will do the same number of calculations and iterations as if you did it recursively). This difference between the two algos is similar to the difference that separates Merge Sort and Quick Sort, and they both have a separate page: the algorithms are different and thus they incur a different computational cost. Of course, the goal is the same, just like those sorting algorithm, so maybe both Polynomial Long Division and Polynomial Synthetic Division should be put under the same subcategory Polynomial Division. --[[User:Lrq3000|Lrq3000]] ([[User talk:Lrq3000|talk]]) 17:28, 3 July 2015 (UTC)


:::::: That would be correct if the other page required a specific algorithm.

:::::: But instead, it merely suggests a [bad] algorithm. --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 19:51, 3 July 2015 (UTC)

:::::: I don't understand, what do you mean by "the other page" and "bad algorithm"? --[[User:Lrq3000|Lrq3000]] ([[User talk:Lrq3000|talk]]) 19:44, 8 July 2015 (UTC)
