+++
title = "Talk:Egyptian fractions"
description = ""
date = 2019-03-27T15:36:42Z
aliases = []
[extra]
id = 17477
[taxonomies]
categories = []
tags = []
+++

== Still incorrect as is ==

I added incorrect tags on both REXX and Perl 6 solutions earlier, then RC went offline before I could post in this talk page.  Although now the incorrect tags are removed and description changed to allow an integer in front of the expansion, it's still a cop-out.  There ''should not'' be an integer term, because any rational number, whether larger than 1 or not, can be represented as a proper sum of unit fractions.  In fact, it's dubious to even allow 1/1 as a term in the expansion.

The current solutions can be defended as "they match the task description", but it's better to do things right, instead of fudging the task requirement to suit existing examples. --[[User:Ledrug|Ledrug]] ([[User talk:Ledrug|talk]]) 17:43, 3 April 2014 (UTC)

: Any whole number can be expressed as a sum of 1/1 fractions, or even a vary large number of unit fractions, but that would be become voluminous with even moderate integers.   The whole number part of an improper fraction (as noted in the task) are to be expressed as integers, and leave the proper fraction part to be expressed as Egyptian fractions.   Egyptian fractions don't use unity as a denominator in any case, and also, Egyptian fractions must have unique denominators, so that rules out multiple 1/1 unit fractions.   The inclusion of the newest part of the task wasn't meant to be a cop-out, but a practical solution to address improper fractions when expressing them in conjunction with Egyptian fractions.   I read very long ago that this is the manner in which the Egyptians handled improper (or vulgar) fractions, an obvious and necessary preliminary first step.   Whether or not that is thought of as a work-around, it wasn't made up just to suit existing examples, as the programming solutions were in keeping with the task's requirements.   Almost all descriptions of Egyptian fractions today (from what I've viewed online) seem to assume that Egyptian fractions deal with proper fractions, and indeed, every example illustrates that assumption.   Whether or not that's true, I haven't seen an example showing an Egyptian fraction example for an improper fraction.   Furthermore, in all of the online calculators that convert a fraction (in particular, an improper fraction) to an Egyptian fraction, the whole part (integer) of the fraction is just shown as a simple integer followed by the Egyptian fractions, each separated by a plus sign.   One reason I chose to have the whole (integer) part expressed in some manner to distinguish the whole part is that I envisioned somebody rebelling against expression the unit fractions as 1/n;   some people use different notations:   for instance,   8/11   as   [2, 5, 37, 4070]   -- most of the time without the blanks after the commas.   In such a case, the (whole) integer part would need a special notation to make it clearly distinguishable from the other integers. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 18:47, 3 April 2014 (UTC)

:: You are using an "Egyption expansion" that's not an Egyption expansion at all because it's more convenient, which is what "cop-out" is.  If nobody talks about using Egyption expansion on numbers greater than 1, then you can simply exclude it in this task; if you still want to see the expansion of larger fractions, then ''do it right''.  Let me say it again: any (positive) rational number can be expressed as the sum of a finite set of distinct unit fractions, so you don't need to choose a half solution.  I don't care if the integer part is written in a different style or what not, since it's not there to begin with.  And making up something to "prevent someone from rebelling" simply sounds narrow-minded. --[[User:Ledrug|Ledrug]] ([[User talk:Ledrug|talk]]) 19:05, 3 April 2014 (UTC)

::: I disagree with your definition of what a cop-out is.   A choice of practicality and/or expediency doesn't make it a cop-out; there's isn't any need to use pejorative words.   Whether or not anybody talks about Egyptian fractions for improper fractions (or not) doesn't change the fact that the use of same is part of this Rosetta Code task.   It's there.   If you want to solve the improper fraction your way, please feel free to do so.   it'll be very interesting to see your solution for the 3<sup>rd</sup> fraction, the vulgar one.   You can repeat that phrase about ''any rational number ···'' as much as you want, I'm not disagreeing with you.   You're beating a dead horse.   I don't understand your comment about ''it's (the integer part) not there to begin with''.   An improper fraction ''has'' an integer part, it's just expressed as part of the improper fraction, I just chose to have the integer part split off from the fraction part of the number before converting it to an Egyptian fraction.   As for making something up to prevent ···, I didn't.   That's not what I did, and that's not what I intended.   Nothing is preventing any rebellion.   Programmers are still free to express/display the unit fractions in any matter they want.   So far, only unit fractions with a 1 (unity) over a solidus ('''/''') are being used (to date) in the programming examples' outputs.   I included the new part of the task to answer/rebuttal your flagging of examples as incorrect.   I also would like the ceasing of name-calling and incorrect characterizations of what I have done or have responded to; it's not professional nor civil.   Whether it be narrow-minded, lame, or other ill-chosen words, it's just not polite and sets a wrong tone for conversations/discussions on Rosetta Code, whether it be in talk pages or elsewhere.   These conversations will be around for a long time. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 21:37, 3 April 2014 (UTC)

== Request for task clarification ==
Two points:

# Better to state what an improper fraction is and that the integer part of any improper part be first isolated and shown to the left, surrounded be square brackets - giving an example; any other method permitted but needing explanation in the language example.
# "''for all 1- and 2-digit integers, find and show an Egyptian fraction that has: the largest number of terms; the largest denominator''"
:What does this mean? I cannot work it out from this description.

Thanks. --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 20:48, 4 April 2014 (UTC)

-----

I didn't think I had to define an improper fraction, as it is such a common term, plus there was a link to the Wikipedia entry defining the terms, but I'll enter it a few hours to give people a chance to respond to the descriptions/definitions below.   I didn't want to clutter up the task description or the task requirements defining basic arithmetic concepts.   Heaven knows what I entered will be "tidied" up so much so that it becomes almost ugly, but then one's man beauty is another's eyesore.   I was thinking of the below:

All proper fractions are of the form   '''a/b'''   where   '''a'''   and   '''b'''   are positive integers, such that   '''a < b'''.
::::: (or, ···   such that   ''' b > a '''). 
--- I don't know which looks "nicer" or the easiest to eyeball.



As for the 1- and 2-digit integers thingy requirement verbiage, I couldn't think of a concise way to express it, but what I was thinking was to supposed to be transferred by mental telepathy, but I guess that didn't work.

For (the above), I meant, all proper fractions (that was implied) for one and two digit (positive) integers, such as:

1/2, 1/3, 1/4, 1/5, 1/6, 1/7, 1/8, 1/9/ 1,10, 1/11, 1/12,   ···   1/97, 1/98, 1/99,   and

 2/3, 2/4, 2/5, 2/6,   ···   2/97, 2/98, 2/99,   and
:  ···    and
96/99, 97/99, and 98/99.

After a few days to mull it over, here's what I came up with, I may append (or replace) the (below) as part of the requirement's verbage: 


For the requirement:

"for all 1- and 2-digit integers, find and show an Egyptian fraction that has: the largest number of terms; the largest denominator" 


 How about: 

for all proper fractions,   '''a/b'''   where   '''a'''   and   '''b'''   are positive one-or two-digit (decimal) integers, find and show an Egyptian fraction that has: 
::* the largest number of terms,   
::* the largest denominator.

-- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 22:12, 4 April 2014 (UTC)

-----

==Several formulae here now invisible to many browsers==

Various formulae on this page have become invisible to many browsers (all those - the majority - which display the graphic file rather than processing the MathML directly), as a result of 'tidying and spacing' edits at 02:50, 16 August 2016. One of the issues is an attempt at cosmetic introduction of redundant white space around Latex expressions inside &lt;math&gt; tags. This white space is not currently expected by the MediaWiki processor, and leads to the generation of syntactically ill-formed HTML. [[User:Hout|Hout]] ([[User talk:Hout|talk]]) 12:57, 19 September 2016 (UTC)

: Visibility of formulae to Chrome, IE/Edge, Safari etc restored by reverting task description to state before under-tested cosmetic edits of 02:50, 16 August 2016.  The editor may wish to restore some of these cosmetic edits, testing their real effects in Google, Microsoft and Apple's browsers, but the the most practical immediate solution is simply to return to the state before visibility of formulae was lost. [[User:Hout|Hout]] ([[User talk:Hout|talk]]) 00:29, 17 October 2016 (UTC)

== Rename and rewrite task as "Greedy algorithm for Egyptian fractions" ==

This task is (and should be) about
[[wp:Greedy_algorithm_for_Egyptian_fractions|the greedy Egyptian expansion]]
and not about
[[wp:Egyptian_fraction|Egyptian fractions]].

Above, [[User:Gerard Schildberger|Gerard Schildberger]] wrote:
: > "Whether or not that's true, I haven't seen an example showing an Egyptian fraction example for an improper fraction."
[[wp:Egyptian_fraction#Motivating_applications|Wikipedia]] has an example, namely: 13/12 = 1/2 + 1/3 + 1/4.

Furthermore, that Wikipedia page covers several calculation methods and shows a "much better expansion" (their words) for 5/121 = 1/33 + 1/121 + 1/363.

The task currently says:
:* show '''''the''''' Egyptian fractions for: <math> \tfrac{43}{48} </math> and <math> \tfrac{5}{121} </math> and <math> \tfrac{2014}{59} </math>
but as shown above there is not a unique Egyptian fraction for a given positive rational number.

I think this task should be renamed and rewritten to be only about the narrower [[wp:Greedy_algorithm_for_Egyptian_fractions|Greedy algorithm for Egyptian fractions]],
and only for proper fractions.
A more general task about Egyptian fractions (if desired) should probably encourage giving the "better" expansions by using other/multiple methods.

The task also currently says:
:* '''for all''' proper fractions, <math>\tfrac{a}{b}</math> where <math>a</math> and <math>b</math> are positive one-or two-digit (decimal) integers, '''find and show an''' Egyptian fraction that has:
::* the largest number of terms,
::* the largest denominator.

Since a given number can have multiple valid Egyptian fraction expansions, as written, the task seems to imply you want output for each proper fraction the results of a search through all possible expansions looking for large number of terms and large denominators.
Existing solutions (mostly) instead calculate the greedy Egyptian expansion for each proper fraction looking for the largest number of terms and largest denominator.
This should be what the rewritten task actually asks for.

&mdash;[[User:dchapes|dchapes]] ([[User talk:dchapes|talk]] | [[Special:Contributions/dchapes|contribs]]) 21:56, 7 June 2018 (UTC)

==A problem with the current Python result ?==

The task requirement (bullet 2) begins:
 ''for all '''proper''' fractions'' ... 

but the candidate result offered for the ''largest number of terms'' by the Python script appears to be an '''improper''' fraction (97/53).

Perhaps an oversight ? [[User:Hout|Hout]] ([[User talk:Hout|talk]]) 15:36, 27 March 2019 (UTC)
