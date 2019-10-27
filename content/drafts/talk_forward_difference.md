+++
title = "Talk:Forward difference"
description = ""
date = 2008-09-26T13:39:15Z
aliases = []
[extra]
id = 2462
[taxonomies]
categories = []
tags = []
+++

== forward difference of one number ==

What's the forward difference of one element? --[[User:Mwn3d|Mwn3d]] 07:17, 11 January 2008 (MST)
:With regard to pure mathematics I suppose it is undefined. It is natural, however, to extend it by definition so that the forward difference of a list of one element is the empty list. That's how the J code resolves it, too. --[[User:TBH|TBH]] 07:31, 11 January 2008 (MST)
::I guess the FD of an empty list should just be another empty list then? These corner cases always get you. --[[User:Mwn3d|Mwn3d]] 07:39, 11 January 2008 (MST)
:::Yes, that's the way to handle receiving an empty list. (A similar "corner case" has been discussed [[Talk:Maximum_subarray|here]].) 
One of the things I appreciate about [[J]] is how many of these exception-situations have been resolved within the language itself. In this instance both the single-element and empty-list possibilities require no specific code. --[[User:TBH|TBH]] 09:45, 11 January 2008 (MST)
:The formula given '''Bn = An+1 - An''' only applies to lists of two or more elements. It could be argued that it is more correct to signal an error condition when given a list of less than two elements to work on.--[[User:Paddy3118|Paddy3118]] 04:42, 27 August 2008 (UTC)
::There are differences of opinion as to how best to handle input that does not lie within the domain of a function. My inclination is to (1) rely entirely on the error-reporting facilities of the programming environment, when possible, and (2) use a separate function to assure conformance of the input with the function, when necessary. In the case of forward difference we should also consider whether there is specific value to avoiding the definitional extensions that allow these functions to succeed when provided single-valued or empty lists. In the absence of a larger problem context I cannot see any benefit from producing error conditions instead of empty lists. --[[User:TBH|TBH]] 18:50, 27 August 2008 (UTC)
:::Any 'extension' behaviour should be carefully noted if you rely on it which means you should know what this behaviour is, and that you rely on it. In this specific case I think it would be incorrect to produce a forward difference from an input list of one value. It is more correct to signal an error (with a meaningful explanation). 
:::If your function was guaranteed to be called in such a way that you would never be called on to produce a forward difference of less than two values then no further checks need be done, but I think its wrong to argue that the forward difference of one value is an empty list just because a particular implementation would generate that - unless you extend ''your'' meaning of forward difference explicitly to handle this. --[[User:Paddy3118|Paddy3118]] 21:53, 27 August 2008 (UTC)
::::Except for those Rosetta Code tasks that specifically involve error-trapping, code solutions should assure that correct results are produced when the program is provided "good" input. Your claim is that input other than a numeric list of at least length two does not count as good. When the input is not good it is irrelevant what the code produces. Error reporting such as you have suggested has nothing to do with the task at hand. --[[User:TBH|TBH]] 18:21, 28 August 2008 (UTC)
:::::Which makes this whole section irrelevant! :-) --[[User:Paddy3118|Paddy3118]] 20:02, 28 August 2008 (UTC)
:I believe that it is completely mathematically consistent for the forward difference of a list of one element to be the empty list. For one thing, you can see that the forward difference of a list is usually a list one shorter; so the forward difference of a list of length 1 should be a list of length 0; i.e. the empty list. Also, a reasonable definition of forward difference is (the list without the first element) - (the list without the last element), where the "-"  is an element-by-element subtraction of the sublists. In the case of the list of one element, both of those sublists are empty, and the result is the empty list. Now, I agree that the forward difference of an empty list should be an error, because it fails both of my above arguments (what is a list of length -1?). --[[User:Spoon!|Spoon!]] 19:09, 28 August 2008 (UTC)
::One shorter? Check! But your reasonable definition is an ''alternative'' definition whose output differs in this case under discussion. It would be better just to state what a forward difference of a list with one member should be - and so exclude one of the definitions, or, state that good input excludes this case. (Or delete the whole section from the talk page and pretend that all is well :-)  --[[User:Paddy3118|Paddy3118]] 20:02, 28 August 2008 (UTC)

It is very nice to see the [[Python]] entry. Studying it has improved my grasp of that language. --[[User:TBH|TBH]] 09:45, 11 January 2008 (MST)

== J: verb vs adverb ==

With regard to the replacement of '''<tt>((}. - }:) ^:)</tt>''' by '''<tt>(2&(-/\))</tt>''':
I agree that a solution in the form of a verb is somewhat better than a solution that is an adverb, although the benefits strike me as subtle and minor.
I think the original solution is worth retaining as an example because of its form. It is not only a different algorithm, it is another strong example of how tacit form allows one to "code the concept."  These two solutions complement one another, so both should be included.
Contrary to the comment upon replacement, the new program is not shorter than the first solution. This is demonstrated by the following J interaction:
    #;:'(}. - }:) ^:'
 6
    #;:'2&(-/\)'
 7
--[[User:TBH|TBH]] 21:13, 20 August 2008 (UTC)
 
:Yep, both solutions could be retained.  I'll put the orig back in a moment.  To address your specific comments:

:I actually think that <tt>2 -/\ y</tt> is a clearer expression of the concept of forward differences.  That is, I think of a forward difference as "inserting a <tt>-</tt> between each pair of numbers", which is exactly what <tt>2 -/\ y</tt> tells J to do; it took me a moment to realize subtracting the curtail from the behead is equivalent.

:Of course, this is entirely subjective: it depends on the way you think about the problem.  So you may think of "forward differences" exactly as "subtracting the behead from the curtail".  In fact, I think some of the other languages calculate their results exactly that way, so the original solution may serve better as a "direct translation".

:Regarding verbal vs adverbal: I don't think the differences are minor (though they may be subtle).  The primary argument for a verb solution runs like this:  we have two noun inputs and want to produce a noun output, which is the very definition of a dyadic verb.  All the arguments are supplied at "run time" not "definition time", and no new verbs are produced, so the adverb does not benefit us.  Yet it costs us plenty: adverbs are hard to use like verbs.  For example, how would you write the equivalent of <tt>+/@(2&(-/\))</tt> (the sum of the forward differences) without rewriting your the adverb as a verb?  You'd have to go through convolutions like <tt>((}.-}:)^:)(+/@)</tt>.

:A secondary (but still important) difference is generality:  Verbal solutions can be easily extended with rank, but it's difficult to slice-and-dice arguments to adverbs.  For example, how would you recreate this result using the adverb solution?
    (3 2 3) 2&(-/\)"_1 ?. 3 5 $ 10
  16 _20  0
  16 _14 11
 _17  22  0

:Or even this (however contrived)?
    (-:@#`],:<@#`+:) 2&(-/\)"1 ] 16 7 82 4    NB.  Apply different _verbs_ (i.e. a multidim array of _verbs_ is an argument)
  84 _153   0 0
   0    0   0 0
   0    0   0 0
   0    0   0 0
 
  32   14 164 8
  18 _150 156 0
 168 _306   0 0
 474    0   0 0

:You're right that the adverb is a concise expression of the concept, which emphasizes J's strengths. I find the verb solution another compelling example of that:  <tt>2&(-/\)</tt> is how you would code the simple forward difference (i.e. as a monad).  The fact that it works as a dyad to produce the <tt>N</tt><sup>th</sup> forward difference (without a single change) highlights J's value as a notation.  Put another way, I find it pleasant that monad is the dyad with an implicit left argument of <tt>1</tt>, which is the "normal use case".

:Finally, With regard to length, my metric was <tt>#</tt> rather than <tt>#@;:</tt>, as in :
    #'(}.-}:)^:'
 9
    #'2&(-/\)'
 7

:I chose <tt>#</tt> because the comment on the solution specifically highlighted succinctness, and total length is the metric of succictness for the solution's target audience (i.e. developers unfamiliar with J, who we're trying to impress).

[[User:DanBron|DanBron]] 23:01, 26 August 2008 (UTC)
::Thank you for elaborating on why it is beneficial to code this as a verb, rather than as an adverb. It is always nice to get explanation for a correction.
::As for which algorithm is a more "natural" or elegant expression, it seems like one of those tough calls. I'm sure my bias was merely a function of what I happened to learn first. Over time I may well come to prefer the version that relies on Infix. I do now concede that is is significantly better because it produces a verb. --[[User:TBH|TBH]] 19:07, 27 August 2008 (UTC)
