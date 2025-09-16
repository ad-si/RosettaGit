+++
title = "Talk:Fibonacci sequence"
description = ""
date = 2017-02-02T19:55:24Z
aliases = []
[extra]
id = 2804
[taxonomies]
categories = []
tags = []
+++

==recursion too slow?==
The task states 

<tt>Solutions can be iterative or recursive (though recursive solutions are generally considered too slow and are mostly used as an exercise in recursion).</tt>

while the parenthetical comment is true, I'm wondering what to make of it. Is there any application of FIbonacci numbers where "computation time" is actually an issue? Do we care about computation time at all? If we do, I'd use neither an iterative nor a recursive approach but an analytic solution through phi (I added that to IDL, I see from a glance that at least the D solution has one of these as well). 

: Just for this <tt> RC </tt> task, speed shouldn't be an issue, I would think.  As an aside, it would irk me to no end if I included an example that's slower than molassas in January. But many programmers who reuse (their) code and/or "borrow" code from others, and if there is a need to generate large amounts of Fibonacci numbers (or often), or test to see if some number ''is'' a Fibonacci number, then having a very fast version would be beneficial for those processes. I have one for my  '''isFib'''  function, for instance. -- [[User:Gerard Schildberger|Gerard Schildberger]] 22:51, 29 May 2012 (UTC)

I guess my question is: if we care about speed, why demand that the solution be iterative or recursive? Or, more generally, if performance is a concern, why demand any particular approach at all -- since the "best" approach for any one task is bound to be different for different languages. (Something with decent tail recursion might have recursive solutions faster than iterative ones and what-have-you). 

And yet other way 'round: does performance matter here on RC? Almost all code samples I've ever contributed would have to be changed if I cared about speed of execution -- I've been trying to optimize for clarity of exposition instead. Is there some kind of guideline / a practical consensus / an ongoing discussion about this somewhere?[[User:Sgeier|Sgeier]] 14:16, 7 April 2008 (MDT)

: I strongly doubt that the analytic version is actually faster than fast solutions using integer arithmetic (manipulating expression trees should be more expensive than manipulating integers). However, a fast solution wouldn't use the recursion formula directly, but use the fact that

```txt

    /1 1\ n    /F<sub>n+1</sub> F<sub>n</sub>  \
   (     )  = (                                 )
    \1 0/      \F<sub>n</sub>   F<sub>n-1</sub>/

```

:together with a fast exponentiation algorithm to calculate F<sub>n</sub> in O(log n) time (in comparison, the naive iterative solution has O(n) time).
: According to the general question if clarity or performance should be more important: I'd say clarity ''always'' wins over "programming tricks" performance (e.g. in C and C++ always use n/2 instead of n>>1 if your goal is to calculate the half of an integer), however ''algorithmic'' performance should be considered, if it doesn't conflict with the goals of the task (e.g. if the goal of a task is to explicitly show how to write a recursive function, replacing it with a faster iterative version obviously isn't a good idea). However, clarity is important, so the algorithmic performance should be weighted against it (also, the complexity of an algorithm shouldn't get too high IMHO; if you want to present complicated algorithms, there are other sites like literateprograms for that). Of course ultimately it's always the task which sets the rules. --[[User:Ce|Ce]] 15:21, 7 April 2008 (MDT)

== negative n errors ==

The task description contains: "Support for negative n errors is optional."
Negative n isn't an error; while conventionally only the definition for positive n is given, the Fibonaccy sequence is actually defined for arbitrary n. You can rewrite the recursion formula F<sub>n+1</sub>=F<sub>n</sub>+F<sub>n-1</sub> as F<sub>n-1</sub>=F<sub>n+1</sub>-F<sub>n</sub> and thus extend the recursive definition to negative values. This results in
:<math>F<sub>0</sub>=0</math>
:<math>F<sub>-n</sub>=(-1)<sup>n+1</sup>F<sub>n</sub></math>
If the word "errors" is removed from the description, it becomes reasonable. --[[User:Ce|Ce]] 14:40, 7 April 2008 (MDT)

: I've now fixed that. --[[User:Ce|Ce]] 08:35, 14 October 2008 (UTC)

== Clarified? ==

In the task page there is the note ''"This task has been clarified. Its programming examples are in need of review..."''. I am not sure what exactly has been clarified, but at least the value of fibonacci(0) seems to be wrong in many language examples.

Should the language examples that have been fixed be marked somehow so that we will know when all the examples have been corrected? (Or perhaps mark all examples and then remove the mark when it has been checked.)
--[[User:PauliKL|PauliKL]] 15:14, 14 April 2009 (UTC)
:I just tried to fix what I could figure out based on that definition. Some of the iterative examples may still be partially wrong, but I think I got a lot of the recursive examples. --[[User:Mwn3d|Mwn3d]] 18:17, 14 April 2009 (UTC)

== Rules abuse! ==

Take a look at the third example under BASIC, Iterative. I stretched the meaning of ''generate'' and just predetermined all the numbers that can be handled by my chosen data type (LONG; 32-bit signed, without support for F<sub>n</sub> where n < 0) and dumped them into an array. Bam! Almost instant results! :-) -- [[User:Eriksiers|Eriksiers]] 20:48, 7 August 2009 (UTC)
:I don't think this (or other types of memoized solutions) violates the rules. It's a neat idea if your language uses a finite integer type. --[[User:Mwn3d|Mwn3d]] 20:50, 7 August 2009 (UTC)
::I was thinking more along the lines of the IOCCC award, "Worst Abuse of the Rules" -- the one given to the guy who turned in the empty source file and called it the world's smallest [[quine]].
::Anyway... what I posted can be applied to pretty much any data type, given time, space, and interest... although I wouldn't want to use it for, say, the limits of an 80-bit float. -- [[User:Eriksiers|Eriksiers]] 20:56, 7 August 2009 (UTC)
:::Recursive? No! Iterative? No! It is best described as table lookup. (But I admire your cheek :-)   --[[User:Paddy3118|Paddy3118]] 02:30, 8 August 2009 (UTC)
::::Table lookup! I ''knew'' there was a better place to put this than "iterative". Gonna change it. :-) -- [[User:Eriksiers|Eriksiers]] 19:59, 9 August 2009 (UTC)

== Alternative ==
<!-- Sorry for the "changing" of someone's comment, but I couldn't read the original formula, so I though an enlarged version would be beneficial to everyone. --- Gerard Schildberger. -->
Fibonacci sequence can also be calculated using this formula.
          <big><big><big><math> Fib[n] = \frac{(1+\sqrt{5})^n - (1-\sqrt{5})^n}{2^n \sqrt{5}} </math></big></big></big>
Size of the floating-point type (float, double, long double etc..) will limit how high n can be calulated.

--[[User:Spekkio|Spekkio]] 11:27, 25 November 2011 (UTC)

:Thanks. Prompted by the above, I read [http://en.wikipedia.org/wiki/Fibonacci_chain#Closed-form_expression this].

==Optional credits==

I would've like to see an optional credit solution for allowing the specification of the starting (two) numbers.

This would've allowed the examples to also generate the Lucas numbers. 


{| style="text-align: left; width: 20%;" border="4" cellpadding="2" cellspacing="2"
|+ Fibonacci sequences 
|- style="background-color: rgb(255, 204, 255);"
! series name !! initial starting numbers
|-
| Fibonacci || 0, 1
|-
| Lucas     || 2, 1
|}



 Another possibility would allow the specifiction of how many (previous) values to be summed.

{| style="text-align: left; width: 25%;" border="4" cellpadding="2" cellspacing="2"
|+ Fibonacci sequences [sum of N numbers]
|- style="background-color: rgb(255, 204, 255);"
! series name !! number of values to add !! OEIS entries
|-
| Lucas        || 2  || A000032, A000204
|-  
| Fibonacci    || 2  || A000045
|-
| tribonacci   || 3  || A000073, A000213
|-
| tetranacci   || 4  || A000078, A000288
|-
| pentanacci   || 5  || A001591, A000322
|-
| hexanacci    || 6  || A001592, A000383
|-
| heptanacci   || 7  || A122189, A060455
|-
| octanacci    || 8  || A079262, A123536
|-
| nonanacci    || 9  || A127193
|-
| decanacci    || 10 || A127194
|-
| undecanacci  || 11 || A127624
|-
| dodecanacci  || 12 || A207539
|-
| 13th-order   || 13 || A163551
|}


There may be more named Fibonacci numbers in this series of series. -- [[User:Gerard Schildberger|Gerard Schildberger]] 20:15, 24 May 2012 (UTC)


Note that there seems to be two definitions of the above series, the main difference is in how the initial numbers are specified. -- [[User:Gerard Schildberger|Gerard Schildberger]] 18:15, 25 May 2012 (UTC)
 
:There seems to be enough in your proposal for a new task maybe called "Fibonacci-like sequences generator" in which the starting numbers/numbers of past numbers to sum to form the next, could be set.
:It would also be good to maybe confirm some of the Lucas/Fibonacci identities mentioned in the Lucas wp article. --[[User:Paddy3118|Paddy3118]] 20:33, 24 May 2012 (UTC)

==[http://mathworld.wolfram.com/Fibonaccin-StepNumber.html Fibonacci n-Step Numbers]==
The name is from MathWorld. I doodled the following:

```python
>>>
 def fiblike(start):
	addnum = len(start)
	def fibber(n):
		try:
			return fibber.memo[n]
		except:
			ans = sum(fibber(i) for i in range(n-addnum, n))
			fibber.memo.append(ans)
			return ans
	fibber.memo = start[:]
	return fibber

>>> f = fiblike([1,1])
>>> [f(i) for i in range(10)]
[1, 1, 2, 3, 5, 8, 13, 21, 34, 55]
>>> l = fiblike([2,1])
>>> [l(i) for i in range(10)]
[2, 1, 3, 4, 7, 11, 18, 29, 47, 76]
>>> f3= fiblike([1,1,2])
>>> [f3(i) for i in range(10)]
[1, 1, 2, 4, 7, 13, 24, 44, 81, 149]
>>> f4 = fiblike([1,1,2,4])
>>> [f4(i) for i in range(10)]
[1, 1, 2, 4, 8, 15, 29, 56, 108, 208]
>>> f5 = fiblike([1,1,2,4,8])
>>> [f5(i) for i in range(10)]
[1, 1, 2, 4, 8, 16, 31, 61, 120, 236]
>>> f6 = fiblike([1,1,2,4,8,16])
>>> [f6(i) for i in range(10)]
[1, 1, 2, 4, 8, 16, 32, 63, 125, 248]
>>> f7 = fiblike([1,1,2,4,8,16,32])
>>> [f7(i) for i in range(10)]
[1, 1, 2, 4, 8, 16, 32, 64, 127, 253]
>>> 
```


I will try and write a task, although I have flu so it could be interesting! --[[User:Paddy3118|Paddy3118]] 21:12, 24 May 2012 (UTC)

:Gerard, I hope you like [[Fibonacci n-step number sequences]] and thanks for the inspiration! --[[User:Paddy3118|Paddy3118]] 21:59, 24 May 2012 (UTC)

:: Hell's-Bells, ahhh likes all kinds!!   I have a handy-dandy, slicer-dicer, one-size-fits-all Swiss Army knife calculator (written in REXX), and among many other things, it has around a thousand different sequences, not the least of which are the Fibonacci-type sequences and other ilk.    And don't even get me started on the numerous prime/prime-ish sequences.   It's got more of 'em then ya can shake a stick at.   -- [[User:Gerard Schildberger|Gerard Schildberger]] 22:11, 24 May 2012 (UTC)

::: As an aside, to quote someone long ago:   the one language all programmers know is profanity.   -- [[User:Gerard Schildberger|Gerard Schildberger]] 22:11, 24 May 2012 (UTC)

==trade mark==

Slightly off-topic, by what the hey!!  
What is the proper syntax for adding a trade-mark symbol after (say) ''Wolfram MathWorld''?  
Is it:

* MathWorld &#8482;
 --or--
* MathWorld&#8482;

Does anyone think it would be a good idea to do a global change for the spelling of Mathworld --> MathWorld, and another change to add the trademark symbol (presumably by a superuser) ? -- [[User:Gerard Schildberger|Gerard Schildberger]] 20:32, 25 May 2012 (UTC)

: Have they asked us to do that?  The rules for trademarks are imposed by the trademark owner.  Note also that they allow the use of their trademark in domain names (without capitalization and without any special trademark symbols).  --[[User:Rdm|Rdm]] 20:38, 25 May 2012 (UTC)

:: No, nobody asked.  I understand domain names would be hard to add a trade-mark symbol, but RC uses/references/quotes that trademarked site a lot, and I thought it would be a show of "good faith" on RC's part to honor that site with at least a TM symbol. -- [[User:Gerard Schildberger|Gerard Schildberger]] 20:42, 25 May 2012 (UTC)

==ECMAScript 2015 (ES6) recursive implementations exploration==
I wrote this little exploration of various recursive takes on Fib generation a while ago: http://pastebin.com/ExTuYkAE Among other things it takes time consumption and multiple uses into account. Might look a bit foreign to many as it uses the new ES6 fat arrow syntax for pretty much everything.
If someone wants to incorporate it into the JavaScript section, be my guest!

== Ocaml matrix example remark ==
I notice someone edited the Ocaml matrix example to say 'actually O(n*n)'. I can't identify who that was now, but unless someone can explain/justify it I will remove it, as it seems to be incorrect. [[User:TobyK|TobyK]] ([[User talk:TobyK|talk]])

==Two formulae in SuperCollider Recursive contribution invisible to most browsers==

The two formulae in the preface to the Recursive section of the SuperCollider examples are invisible to most browsers. Perhaps they were edited and tested in FireFox ? The majority of browsers, including Chrome, IE/Edge, Safari etc, use the part of the HTML code which displays the server-side graphic file for each formula. Only a minority, which happens to include FireFox, locally process the MathML expression, and only then if requisite fonts are installed. As the MediaWiki processor generates separate code to support each approach, visibility of formulae in Firefox doesn't guarantee successful code compilation and visibility to the majority of browsers.

Redundant space inside &lt;math&gt; tags can be one source of this problem. Perhaps the author can experiment and check the result in in Chrome, IE/Edge or Safari ? [[User:Hout|Hout]] ([[User talk:Hout|talk]]) 11:38, 15 October 2016 (UTC)


:OK; I'll take a look into it [[User:Telephon|Telephon]]. 
:I can reproduce it, yes. I had just tried and it worked on Firefox. Is there an alternative that would work on other browsers?--[[User:Telephon|Telephon]] ([[User talk:Telephon|talk]]) 14:12, 15 October 2016 (UTC)

:: Good question - for the set membership 'drawn from', we can use the HTML entity (ampersand '''isin''' semicolon), though it comes out smallish on its own 
```txt
<big>&isin;</big>
```
 In isolation, neither 
```txt
<math>n</math>
```
 nor 
```txt
<math>\mathbb{N}</math>
```
 choke the preprocessor, but it does seem to have difficulty with both 
```txt
<math>\mathbb{N_0}</math>
```
 and 
```txt
<math>\mathbb{Z}</math>
```


:: Would you be happy to fall back to English descriptions of the sets ? It's possible that these limitations will eventually be overcome in MediaWiki or the Math extension. [[User:Hout|Hout]] ([[User talk:Hout|talk]]) 14:59, 15 October 2016 (UTC)


Yes, a description in English is really easy here. Thank you for looking into it in detail! --[[User:Telephon|Telephon]] ([[User talk:Telephon|talk]]) 15:34, 15 October 2016 (UTC)

==Possibly misleading use of 'iterative' in Haskell subsection ?==

Hi [[User:WillNess|WillNess]]  , I notice that the ('''fibonacci by folding''') Haskell example has just been moved under the heading 'Iterative'. I wonder if that doesn't risk confusing a little, or even possibly misleading ?  

Folds are implemented recursively (either directly or indirectly) in the Prelude, and are generally understood as 'recursion schemes' in the sense of Meijer et al. (See, for example, http://blog.sumtypeofway.com/an-introduction-to-recursion-schemes/ and the much-read paper which it references http://maartenfokkinga.github.io/utwente/mmf91m.pdf).

I also notice that other examples which have ended up in the 'Iteration' section might risk compounding a reader's confusion – they are either implemented by direct and immediate recursion on helper functions like '''go''' and '''next''', or are expressed in terms of '''zipWith''', '''scanl''' etc, which are also implemented as recursive functions.

Perhaps 'iteration' is not quite the clearest or best-fitting term to use here ?
	[[User:Hout|Hout]] ([[User talk:Hout|talk]]) 19:31, 2 February 2017 (UTC)

FWIW, I would argue that if we need to subdivide, then the main distinction here is '''memoising vs not memoising'''. If we feel a need to subdivide further, and perhaps capture something like the category now labelled "Iterative", then perhaps what we really mean here is closer to ''direct and indirect recursion'' ? [[User:Hout|Hout]] ([[User talk:Hout|talk]]) 19:44, 2 February 2017 (UTC)
