+++
title = "Talk:Test integerness"
description = ""
date = 2016-12-03T21:54:54Z
aliases = []
[extra]
id = 17720
[taxonomies]
categories = []
tags = []
+++

==Some thoughts==
# We also have: [[Determine if a string is numeric]]
# What does integerness mean? would ''"Have no imaginary part(s) and nothing* after the decimal point"'' do?
:: (At least for ints, reals, complex, and [[Quaternion type]]s; not sure about infinities and whatnot).
:: *Note, .999... === 1 though.
--[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 16:54, 18 June 2014 (UTC)

:Thanks for clarifying the task. --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 12:11, 21 June 2014 (UTC)

::An integer is an element of '''[[wp:Integer|Z]]'''.  Complex numbers, quaternions etc do no qualify as integers even if their real components are integers.  There would be some sense in considering them so, but it's not the case mostly for historical reasons I guess.
::To make the task clearer, I'll add a link to the Wikipedia article.  Hope that helps.--[[User:Grondilu|Grondilu]] ([[User talk:Grondilu|talk]]) 13:50, 21 June 2014 (UTC)
:::REXX considers 1.00 and 1e27 to be integers as long as Numeric Digits is large enough. ok? --[[User:Walterpachl|Walterpachl]] ([[User talk:Walterpachl|talk]]) 19:12, 21 June 2014 (UTC)
::::I'm not sure I understand the problem.  1.00 and 1e27 *are* integers.  They may be stored as floating point numbers, but they are integers.--[[User:Grondilu|Grondilu]] ([[User talk:Grondilu|talk]]) 01:11, 22 June 2014 (UTC)

On second thought, the test makes sense with complex numbers.  Basically a complex number is an integer if its real part is integer and its imaginary part is nul.--[[User:Grondilu|Grondilu]] ([[User talk:Grondilu|talk]]) 08:04, 22 June 2014 (UTC)

: (As per above) I'm assuming that   4+0i   is an integer, even though the   '''0i'''   isn't "nul";   the imaginary part is equal to zero, but it's not equal to a "nul" (depending on one's definition of the equality of zero and "nul" in the previous sentence). -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 15:31, 25 June 2014 (UTC)
::I wonder what you mean by «'''0i''' isn't nul».  It is very much nul to me, or equal to zero, which means the same imho.  Unless you're talking about nul as "undefined" or something, but that's clearly not what we're talking about here.--[[User:Grondilu|Grondilu]] ([[User talk:Grondilu|talk]]) 19:46, 25 June 2014 (UTC)

::: What I meant that zero (0)   [or 0i]   and "nul" aren't the same thing, they aren't equal.   And I wasn't talking about nul as having a value as undefined or somesuch.   Also, a nul character ('00x') and a null value are two different animals.     [ ] is not equal to [0]. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 20:23, 25 June 2014 (UTC)

:::: "The question is," said Alice, "whether you can make words mean so many different things."    ────   ''Through the Looking-Glass'' by Lewis Carroll (Charles Lutwidge Dodgson).

::::Well, we were talking about numbers, not characters or anything.  So in that context ''nul'' does mean zero imho.--[[User:Grondilu|Grondilu]] ([[User talk:Grondilu|talk]]) 11:19, 26 June 2014 (UTC)

:: Imaginary numbers are taught as existing on an orthogonal axis to the reals intersecting the reals at 0j. It ''seems'' OK to thing of am imaginary number with zero imaginary part and zero after the decimal point as equivalent to an integer for this task.
:: --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 16:11, 25 June 2014 (UTC)

: Mathematicians define a complex number (an element of the field '''C''') as an ordered pair (''re'', ''im'') where ''re'' and ''im'' are elements of the field '''R''', the real numbers.  An ordered pair of elements is quite distinct from a single element. 

: If you wish to test whether a complex number is an integer you also need to be careful to state what you mean by that term.  A complex integer, more usually termed a ''Gaussian integer'' or an element of the ring '''Z'''[''i''], is an ordered pair of elements (''re'', ''im'') of the ring '''Z''', the ring of integers.  The sub-ring which has ''im'' = 0 is isomorphic to '''Z'''.  My ''guess'' is that the latter set is what is meant when the task is extended to treat complex numbers but this should be made explicit.
: --[[User:Brnikat|Brnikat]] ([[User talk:Brnikat|talk]]) 21:15, 5 August 2015 (UTC)


==Number set symbols not visible to most browsers==

Browsers which display a graphic file (rather than locally processed MathML and local fonts) for the content of &lt;math&gt; tags are not displaying number set symbols on the task page, because the MediaWiki processor is choking on unexpected input and generating syntactically ill-formed HTML tags (in which a semi-colon is missing between the height and vertical-align attributes).

May be worth further investigation and testing of the expectations of the MediaWiki processor here. [[User:Hout|Hout]] ([[User talk:Hout|talk]]) 19:25, 21 September 2016 (UTC)

:No obvious permutations of '''&lt;math&gt;''' tag expressions achieve display of number set symbols on these MediaWiki pages in the majority of browsers at the moment (Chrome, IE/Edge, Safari, Opera etc, i.e. the browsers which display the font-independent server-side graphic)  (Subject to installation of necessary fonts, it may be possible in Firefox, which uses local font-dependent processing of MathML). 

:Future changes in the MediaWiki HTML generator may enable display of number set symbols in most browsers, but in the meanwhile, I suggest that we adopt the approach used in some of the discussion above, and replace the blank gaps in the task description with bold capitals like '''Z''', '''Q''', '''R''', '''C'''. [[User:Hout|Hout]] ([[User talk:Hout|talk]]) 12:52, 2 December 2016 (UTC)

:: This change has now been made (using symbol bold caps for number set symbols – this makes them visible in the majority of browsers. They were previously hidden to Chrome, IE/Edge, Safari, Opera etc)
