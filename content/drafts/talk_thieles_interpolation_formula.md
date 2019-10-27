+++
title = "Talk:Thiele's interpolation formula"
description = ""
date = 2018-06-05T23:08:52Z
aliases = []
[extra]
id = 8377
[taxonomies]
categories = []
tags = []
+++

== How many entries should the trig table have? ==
How many entries should the trig table have? --[[User:Short Circuit|Michael Mol]] 14:46, 2 October 2010 (UTC)
: I make it 16, with ''x'' varying by steps of 0.05 from 0 to 1.55. (Now, if only I could make my version of this ''work'', but that algol68 code is deeply gnarly; does it really have different base indices in different dimensions?!?) –[[User:Dkf|Donal Fellows]] 16:01, 2 October 2010 (UTC)

I picked 32 rows to the table.  Basically "'''from''' 0 '''by''' 0.05 '''to''' 1.55 ..." as 0.05 seems to give the full single precision answer (on an i686 CPU at least). Ideally this size would be calculated from the desired precision, but I don't have a formula for this precision calculation. [[User:NevilleDNZ|NevilleDNZ]] 21:31, 2 October 2010 (UTC)
: The task description modification is nice, but would it be terribly problematic to just provide the  table? The task seems to have more stages than it really needs. A TSV table would be pretty useful for the purpose, IMHO. --[[User:Short Circuit|Michael Mol]] 22:35, 2 October 2010 (UTC)

The table of (''x, sin x, cos x, tan x'') records is generated prior to any interpolation being done...  Then the interpolation is used to create &mdash; user defined &mdash; ''inv sin, inv cos and inv tan'' functions.  The three simple &pi; calculations &mdash; 6 &times; sin<sup>-1</sup> &frac12;, 3 &times; cos<sup>-1</sup> &frac12; and 4 &times; tan<sup>-1</sup> 1 &mdash; simply present/test/prove the interpolation is implemented correctly.  A subsequent table of (''y, inv sin y, inv cos y, inv tan y'') records &mdash; while a bonus &mdash; is not strictly required. [[User:NevilleDNZ|NevilleDNZ]] 23:25, 2 October 2010 (UTC)
: Understood, but since the task is about interpolation, I figured that generating the initial reference table might be a spurious requirement. --[[User:Short Circuit|Michael Mol]] 23:43, 2 October 2010 (UTC)
I confess that I was trying to think of something "interesting" (yet simple) to interpolate, and recalled using interpolation to calculate inverse trig functions from high school log/trig tables[http://www.eton.co.nz/shop/Mathematics+Text+And+Homework+Books/Age+17+Alternative+Textbooks/Eton+Statistical+And+Math+Tables+%28Fourth+Edition%29.html]. Hence this unit test. What would be nice would be some "historic" interpolation, e.g. [[wp:Discovery of Neptune#Discovery Observation: September 24th 1846|Discovery of Neptune]], that would be another task in itself. The three &pi; calculations from a generic trig table is practical. [[User:NevilleDNZ|NevilleDNZ]] 00:18, 3 October 2010 (UTC)

== re:base indices in different dimensions ==

Yes.  Here is a quote from the 1968 Congress: [http://www.cs.ru.nl/~kees/home/papers/psi96.pdf C.H.A. Koster (1993). "The Making of Algol 68" (PDF)].

 The IFIP 1968 Congress took place that August in Edinburgh, just a few hours drive
 away from North Berwick. Van Wijngaarden’s invited lecture on Algol 68 was to me
 the high point of the conference, and not only to me. The auditorium was packed,
 people were standing on all sides, even in the corridors and outside, in front of the
 hall. Van Wijngaarden appeared in the centre, smiling radiantly. “Let me sell you
 a language”, he started, and proceeded to outline the ideas behind the language. He
 showed some examples. “Can you deﬁne triangular arrays?” someone (Tony Hoare?)
 interrupted. “Not just triangular, but even elliptical” replied Aad, and showed how.
 He carried the listeners with him, from scepsis to enthusiasm. There was a prolonged
 applause.
 
 Vehemently discussing, people streamed out of the hall. A small man pushed
 through the throng, straight at me. “Conkratulations, your Master hass done it”
 said Niklaus Wirth in his inimitable Swiss-German English.

Basically, Algol 68 is agnostic about where an array starts, although the default starting point for both an array and a '''do''' ~ '''od''' loop is <u>one</u>.  Hence - for convenience - in the [[wp:User:Hair_Commodore|Hair Commodore]]'s ALGOL 68 code specimen s/he pushes some array base indices to 1 using the [@1] construct.

[[User:NevilleDNZ|NevilleDNZ]] 21:31, 2 October 2010 (UTC)

==Better example code?==
I find the Algol hard to follow, and am having difficulty finding the algorithm in another language on the 'net. Does anyone have an example of in aonther language/pseudocode? It would be good if we could get  better description of the task than the wikipedia entry I. --[[User:Paddy3118|Paddy3118]] 04:24, 3 October 2010 (UTC)

I've just tried to do a little more and the description given by the formula isn't long enough to clearly show the form of the continued fraction. I think the ellipsis needs further elaboration.  --[[User:Paddy3118|Paddy3118]] 05:29, 3 October 2010 (UTC)

== Tcl example wrong ==

I know that there's something distinctly wrong with the Tcl version of the code as the output values are wrong, but I just cannot see it. Too much looking at code, so I've dumped it in here for now as I'm pretty sure that the ''approach'' is right. It's just the details which I think are off. If anyone spots the issue, please let me know! –[[User:Dkf|Donal Fellows]] 07:15, 3 October 2010 (UTC)

:Damn! And there I was waiting on your example to help work out how to create the Python one. :-)
--[[User:Paddy3118|Paddy3118]] 11:21, 3 October 2010 (UTC)
::I just added a C specimen... enjoy... [[User:NevilleDNZ|NevilleDNZ]] 12:00, 3 October 2010 (UTC)

: Corrected it. The D version is much easier to work with; it doesn't play indexing games so it's easy to translate into code that always uses zero-based indexing. –[[User:Dkf|Donal Fellows]] 14:48, 3 October 2010 (UTC)

== Suggest a related task ==

This task would probably be much easier to solve without resorting to translation if there were a task explicitly demonstrating finite sequences. [[wp:reciprocal difference]] would be helpful. I'd suggest using the same f(x) as used here. (sin, tan, cos) --[[User:Short Circuit|Michael Mol]] 12:45, 3 October 2010 (UTC)

== Perl 6 Version is flawed ==

It works, but isn't 100% faithful to the formula. (See the comment about "form" vs. the real form of the continued fraction.) [[User:Smosher|Smosher]] 19:51, 6 October 2010 (UTC)
:Does that explain why the cos result is way off? --[[User:Ledrug|Ledrug]] 23:12, 27 July 2011 (UTC)

==Some formulae now invisible on standard OS X browsers==
May need tidying up to achieve formula visibility on the OS X platform. Problems introduced at 18:05, 31 July 2016, which made several formula elements invisible on the majority of browsers (those which display the server side graphic for formulae, rather than locally processing MathML where requisite fonts are available) may include recent introduction of redundant white space flanking of LateX expressions inside &lt;math&gt; tags [[User:Hout|Hout]] ([[User talk:Hout|talk]]) 13:33, 16 September 2016 (UTC)

: Now repaired [[User:Hout|Hout]] ([[User talk:Hout|talk]]) 19:22, 28 September 2016 (UTC)
