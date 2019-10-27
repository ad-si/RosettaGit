+++
title = "Talk:Check Machin-like formulas"
description = ""
date = 2016-11-21T15:58:08Z
aliases = []
[extra]
id = 12560
[taxonomies]
categories = []
tags = []
+++

==Aesthetics==
The original equations without the math formatting have a lot going for them. What do you think? --[[User:Paddy3118|Paddy3118]] 21:08, 8 November 2012 (UTC)
: The text format can be used for data input (as in the Python example) and the formatted fractions are a bit crowded (small line spacing).
: There are some odd text sizes now, the last identity [tan(-a)=-tan(a)] is in a smaller fontwith Firefox 16.0.2, anyone else see this? 
: If we can't improve the current layout I would prefer the original format. [[User:TobyK|TobyK]] 22:22, 8 November 2012 (UTC)
:: That's why I was careful to ''not''  use the fully stacked form; it forces the mediawiki <nowiki><math></nowiki> environment to switch from inline mode (converting to appropriate HTML) to display mode (pushing through LaTeX to make an image). With inline mode, it would have been reasonable to expect a cut-n-paste-into-parser approach to work; display mode makes that impossible (well, not without OCRing things first, which is another few orders of magnitude of complexity!) –[[User:Dkf|Donal Fellows]] 11:42, 9 November 2012 (UTC)
::: Well, not really.  Whether math shows as image or text (or MathML) depends on user wiki pref settings.  The only way to garantee copy/paste is keep it in text format, and if you want to use math environment, might as well let wiki engine format it nicer since you won't know if it comes out as an image (or MathML) anyway. --[[User:Ledrug|Ledrug]] 18:22, 9 November 2012 (UTC)

==OCaml: Very long line==
Could the OCaml be changed to indicate equality with one as a short true/false indicator rather than printing out the very long line? Thanks. --[[User:Paddy3118|Paddy3118]] 21:24, 8 November 2012 (UTC)
::  OK, I have changed the Ocaml program so the output is shorter [[User:TobyK|TobyK]] 22:07, 8 November 2012 (UTC)
:::(... And thanks for the task Toby. I enjoyed it :-)
--[[User:Paddy3118|Paddy3118]] 06:05, 9 November 2012 (UTC)

==Non-draft task==
I've changed this to a non-draft task since there are now several different language solutions and there seem to be no problems with the problem definition. Comment if you think there are any outstanding issues.  [[User:TobyK|TobyK]] 19:02, 9 December 2012 (UTC)

==Floating-point calculations==
I don't believe the J or Perl 6 entries are using exact arithmetic, unless they are using non-obvious symbolic language features. I will flag these as incorrect unless someone can explain how they are exact. [[User:TobyK|TobyK]] 13:16, 20 December 2012 (UTC)
: In J, exact arithmetic is termed "extended precision" (or when specifically describing non-integral values, we sometimes say "rational").  And, as noted in the J solution "''the function '''<tt>x:</tt> coerces values to extended precision'''; thereafter J will maintain extended precision throughout its calculations, as long as it can.''".  
:For example, dividing one by seven with <tt>1%7</tt> by default produces the float point approximation <tt>0.142857</tt>, but coercing extended precision with <tt>x: 1%7</tt> produces the exact ratio <tt>1r7</tt> (which is J's notation for rational numbers).  J will carry exact values as far as it can through its computations; there are some built-in functions which have not been designed to produce exact results, and will revert to floating point values.  Users have no control over these functions, and if they want a work-alike that produces exact results, they must write it themselves.
:--[[User:DanBron|DanBron]] 20:04, 20 December 2012 (UTC)


:: Normally [http://en.wikipedia.org/wiki/Extended_precision extended precision] means floating-point with greater precision, however I will accept your explanation. The J code is very terse, it would be useful to readers if you or someone else can add a brief description to the solution of how it implements the algorithm.
::: Thanks, I hadn't realized J's terminology was non-standard.  I'll change the note on the solution to say "exact arithmetic" rather than "extended precision numbers".  

:::Regarding the style of the solution: J's compact notation is one of, if not the most, salient feature of the language.  So, when I post solutions to RC, I try to emphasize it.  

:::Because J's orthography is symbolic rather than textual, as most other languages are, anyone who wants to read it in effect has to learn a whole new language.  Knowing Java and trying to read J is somewhat analogous to knowing English and trying to read Japanese.  So, given that that's a high bar to hurdle, I try to give my audience some impetus to make the leap.

:::I'm hoping that repeatedly seeing J solutions which are short, sharp, and crisp, especially on tasks where other languages need to expend paragraphs or even pages of code, will provide that impetus.  To be clear: I don't play code golf with my solutions (you really don't want to see me do that in J), but I do try to keep the code clean and uncluttered, with minimal narrative (e.g. breaking out trivial sub-functions, overlyLongVariableNames, patronizing comments, etc).  The alternative is to write longer, more tutorial-style solutions, which given J's alien nature, would amount to a language primer rather than an algorithmic overview.  

:::To put it another way: if learning J is analogous to learning Japanese, then teaching J is analogous to teaching Japanese, and marking-up the solutions is analogous to trying to elucidate the beauty of a Japanese haiku to someone who is only familiar with English ballads.

:::Of course, we don't have to choose one extreme or the other.  If I think a solution merits deeper consideration, sometimes I (or my J-brethren) will write up a longer exposition on the task's Talk page, and link to it from the solution, e.g. [[Talk:Zig_Zag#reading_the_J_examples|Zig Zag]] (which explanation helped other languages improve their solutions).  But explanations of that depth take some effort, so I don't it for many solutions.

:::--[[User:DanBron|DanBron]] 23:34, 21 December 2012 (UTC)

:::: Ah, that Zig-Zag explanation! For me it's an RC classic. One of the better bits of RC and its community that I remember fondly. Thanks again DanBron. --[[User:Paddy3118|Paddy3118]] 05:10, 22 December 2012 (UTC)
::::: Yes, that was a fun one.  Good times, good times.  Happy holidays Paddy3118, and to all my other RosettaComrades!  --[[User:DanBron|DanBron]] 18:23, 22 December 2012 (UTC)

== Task is wrong ==

Proving that ''tan(x)=1'' is by no means a proof that ''x=pi/4''.

Thus, the sentence

''Verify the following Machin-like formulas are correct by calculating the value of tan(right hand side) for each equation using exact arithmetic and showing they equal 1:''

is just painfully wrong: this is '''not''' a verification that the equations are '''correct''', only that the RHS evaluates to ''pi/4 + k pi'', for some unknown integer ''k''.

[[User:Arbautjc|Arbautjc]] ([[User talk:Arbautjc|talk]]) 10:39, 30 April 2015 (UTC)


:Agreed that technically you also need to show that |RHS| < pi*3/4 to complete a formal proof, but since |arctan(x)| < |x| I would say that |RHS| < 2 is obvious by inspection of the denominators. I may reword the description to mention this [[User:TobyK|TobyK]] ([[User talk:TobyK|talk]])
::Yes, it's easy to amend :) [[User:Arbautjc|Arbautjc]] ([[User talk:Arbautjc|talk]]) 20:14, 1 May 2015 (UTC)

==Formulae hidden to most browsers by under-tested cosmetic edits on 24 July 2016 ==

Under-tested cosmetic edits made to the task page at and around 19:37, 24 July 2016, including the injection of spaces around expressions in &lt;math&gt; tags, have left some or all of the task description formulae completely invisible to all browsers which display the graphic file version of formulae rather than processing the MathML (this is, in fact, the majority of browsers). The MediaWiki processor does not currently expect such spaces, and generates syntactically ill-formed HTML if they are introduced. Other aspects of these cosmetic edits may further compound the problem. [[User:Hout|Hout]] ([[User talk:Hout|talk]]) 09:59, 22 September 2016 (UTC)

: Visibility of task descrption formulae restored by [[User:TobyK|TobyK]] Nov 7 2016

: (A couple of formulae remain coincidentally invisible in the preamble to the GAP contribution) [[User:Hout|Hout]] ([[User talk:Hout|talk]]) 15:21, 7 November 2016 (UTC)
::There is no space around the invisible formula in the GAP section. Also, it prints correctly on Wikipedia: [https://en.wikipedia.org/wiki/User:Kiwipidae/Test test]. I don't intend to circumvent a botched LaTeX implementation. [[User:Arbautjc|Arbautjc]] ([[User talk:Arbautjc|talk]]) 16:39, 7 November 2016 (UTC)
::: Helpful to readers, perhaps, to add a note that you have only managed to make those two formulae '''(k \mathrm{atan}(x)''' and '''\frac{5\pi}4)''' visible in Firefox ? (And perhaps give plain text equivalents ? Very understandable that you don't want to pay the price of someone else's Latex limitations, so I'm sure you won't want your readers to pay it either :-) [[User:Hout|Hout]] ([[User talk:Hout|talk]]) 17:05, 7 November 2016 (UTC)
::::Actually, I'm already paying it, since I am using Opera now. You are right, I can change the formula to text. [[User:Arbautjc|Arbautjc]] ([[User talk:Arbautjc|talk]]) 19:11, 7 November 2016 (UTC)
:::::: PS I don't know what your view is of the parallel issue in the Python contribution to https://rosettacode.org/wiki/Faulhaber%27s_formula [[User:Hout|Hout]] ([[User talk:Hout|talk]]) 15:57, 21 November 2016 (UTC)
::::: :-) Many thanks – one more page completely clear of that 'exheight' glitch now [[User:Hout|Hout]] ([[User talk:Hout|talk]]) 19:26, 7 November 2016 (UTC)
