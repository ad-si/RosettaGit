+++
title = "Talk:Sieve of Eratosthenes"
description = ""
date = 2016-12-12T12:24:40Z
aliases = []
[extra]
id = 2377
[taxonomies]
categories = []
tags = []
+++

==Needs tidying up of examples that don't fit the task description==

I just looked at Racket and Python, but it seems that these languages have examples that ignore the task description in one or more of their optimisations. It could be present in other languages examples. 

I would suggest the nonconforming examples be moved to this talk page or discarded as each language needs at least one clear implementation of the full task description. 
 Several non-conforming examples is excessive I think. --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 05:30, 18 November 2014 (UTC)

==Optimizations==
My Forth is very rusty, but it looks like your Forth version of the [[Sieve_of_Eratosthenes]] lacks both important features of the sieve: The outer loop should stop at the sqrt of the total limit, and the inner loop should start with the square of the just detected prime (any other multiples must already have been crossed out). Without those, the sieve won't be faster than to trial division for each number. Some other implementations seem to have the same problem. (And some only sieve odd numbers, while others don't, making overall comparison between languages more difficult). Maybe all this should also be clarified in the task description? [[User:Dirkt|Dirkt]] 02:48, 2 December 2007 (MST)
:I"m ambivalent about these optimizations. The half dozen or so sieve implementations I based this on did not contain these optimizations (including those on similar comparison sites, such as the [http://shootout.alioth.debian.org/gp4/benchmark.php?test=nsieve Computer Language Shootout]). Also when I measured this a while ago, the speedup of these optimizations was minimal. For small ranges, the other optimization to assume 2 is prime (i.e. only examine odd numbers) had a greater effect, both in time and space.  (Out of curiosity, did Eratosthenes himself document these optimizations, or were they found later?)
:As to trial division, you are assuming division and square root are cheap. The simple implementation only uses addition (multiplication may not be implemented on a given processor).
:I think keeping the tasks simple should be a goal of Rosetta Code. Keeping things simple encourages folks to donate their time in writing task solutions.
:I agree that the examples should all use the same algorithm. The algorithm we decide upon should be described in the task description. --[[User:IanOsgood|IanOsgood]] 07:53, 2 December 2007 (MST)
:: The difference only starts to kick in for large numbers. It's a similar story with the purely functional Haskell version I mentioned - nobody really tried this for large numbers, so nobody discovered it was flawed for a long time. And yes, only examining odd numbers (or using wheels other than this simple one that is only based on the prime 2) do indeed speed up things quite a lot, even for large limits.
:: By "trial division", I didn't mean actual cost, but asymptotic cost. The 'trick' behind the Sieve of Eratosthenes is to avoid checking whether a prime p divides some remaining candidate k if it is already known that k is composite. Trial division, in contrast, performs this check for every p and every k. And because the sieve only does a fraction of those checks, it's much faster.
: The above is wrong!  Real difference between trial division and sieve is that in trial division most divisons is "useless": when dividing n by k we just learn that k is not a divisor of n.  Sieve only looks at pairs n,k such that n divides k.  Even most naive Sieve of Eratosthenes has complexity n*log(n), while using trial division and stopping at sqare root we are close to n*sqrt(n).  Stopping sieve at sqrt(n) saves half of the work, so it is really comparable to looking only at odd numbers.  Starting inner loop at p^2 has negligable effect.
:: The ''actual'' cost of sum versus multiplication is recovered quickly by the better asymptotic behavior (I can do the math for you, if you want me to).
:: I agree that things should be kept simple (and therefore I would prefer to simply test for all numbers, not only odd, or not those with remainder 1 or 5 (mod 6), etc.), but these two things (which are really two sides of the same idea - if you start crossing at p*p for each prime p, than of course you can stop crossing once p*p is over the limit) are really easy to implement and do affect asymptotic cost, so I'd like to see them included. [[User:Dirkt|Dirkt]] 09:24, 2 December 2007 (MST)
:FYI, if you apply the outer loop optimization (loop from 2 to sqrt n), then you must also print the primes in a second loop from 2 to n. Many of the examples are now incorrect as they stand. --[[User:IanOsgood|IanOsgood]] 10:15, 2 December 2007 (MST)
:: Yes, if one insists that the primes must actually be printed. But the task only says "find all the primes", and keeping the sieve around is actually the less space intensive way to store them (as I found out the hard way some time ago).
:: Anyway, I think that's not the main point :-) And the examples are first of all *different*. If you think that from the point of view of comparing languages, an inefficient Sieve of Eratosthenes is better, then the task should clearly say so, and all examples should be adapted. However, as I said, I think that those two optimasations are simple enough, and worth including. [[User:Dirkt|Dirkt]] 01:38, 3 December 2007 (MST)
:::If I might chime in...
:::I'm not really familiar with the Sieve of Eratosthenes, but perhaps a page could be taken from [[FizzBuzz]] and [[100 Doors]]?  Language sections in those pages are subsected to reflect optimized vs unoptimized approaches.  For the purposes of easy comparison, perhaps such a subsection would be appropriate here?--[[User:Short Circuit|Short Circuit]] 12:23, 3 December 2007 (MST)
::I personally think the 100 doors "optimized" solutions are useless, adding nothing compared to the random number task, and cluttering up the table of contents. In fact, I suggested the Sieve of Eratosthenes task because I didn't like 100 doors in the first place!
::I don't really have anything against the optimized bounds for the sieve (Wikipedia mentions it, and I have a Forth program ready to go). Just make sure the algorithm is documented in the task description. --[[User:IanOsgood|IanOsgood]] 16:56, 3 December 2007 (MST)

In the task description: "you shouldn't optimize by using pre-computed wheels, i.e. '''assume''' you need only to cross out odd numbers (wheel based on 2), numbers equal to 1 or 5 modulo 6 (wheel based on 2 and 3)"

This might be clearer if it read "don't assume". The word "shouldn't" is supposed to apply distributively: you shouldn't optimize ... you shouldn't assume. But it's actually open to being interpreted as the opposite of the intended meaning, which is: Don't assume that the only numbers you need to sieve are those supplied by a pre-computed wheel. Adding the word "don't" would eliminate the ambiguity. Should I change it? --[[User:Snoman|Snoman]] 01:26, 16 July 2010 (UTC)
: I went ahead and made this change. Please feel free to undo. --[[User:Snoman|Snoman]] 18:26, 16 July 2010 (UTC)

Also, I noticed that the Ruby function was misspelled. Fixed. :-) --[[User:Snoman|Snoman]] 03:34, 16 July 2010 (UTC)

== Python ==

This task can also be handled by the [http://tnt.math.se.tmu.ac.jp/nzmath/ NZMATH] modules prime and arith1 to enable its generator_eratosthenes function.--[[User:Billymac00|Billymac00]] 02:45, 3 January 2011 (UTC)
: You can add a code example that shows the use of the module, as long as there's already a code example that satisfies the task as written. --[[User:Short Circuit|Michael Mol]] 04:13, 3 January 2011 (UTC)

== The not-a-sieve method ==
The first code sample under Perl is indeed not a sieve method.  Is there a proper procedure to remove it--or can I just remove it? (and who in the world uses a <i>one space</i> indent?) --[[User:Ledrug|Ledrug]] 09:13, 12 June 2011 (UTC)
:Sometimes we might leave an example that deviates from the task especially if it says how it deviates from the task. If someone did a proper Perl sieve, then that would definitely be the time to consider removing this one. As it stands, its good to ask/inform the original author? --[[User:Paddy3118|Paddy3118]] 12:07, 12 June 2011 (UTC)
:: The wrong code was added by an IP with it being his only contrib, a long time ago.  The note was added by someone else, the code was edited by someone else (for stylistic reasons, and it's still bad looking), and there are three other, correct implementations in the same section, by someone else.  This really screams "delete" IMO. --[[User:Ledrug|Ledrug]] 00:27, 13 June 2011 (UTC)
::: I have replaced that code with a sieve implementation. --[[User:Rdm|Rdm]] 14:35, 13 June 2011 (UTC)

Question zkl: It has been marked as incorrect. Could you (the marker) explain why this is? The code implements the animated algorithm illustrated in the Wikipedia article very closely.  -- resolved, thank you.

== unix shell versions ==
[[User:Kernigh]] has added a note that the shell version use trial division (I don't quite understand how they actually works, it's difficult to trace), I have added on version that is actually using the sieve operation at the end. --[[User:AlexLehm|AlexLehm]] 23:09, 28 October 2011 (UTC)

== NetRexx Performance ?? ==
Better performing program requested

:Got a dramatically better version from Kermit Kiser (thanks) and shall post it later 
:when I have understood the changes.
--[[User:Walterpachl|Walterpachl]] 04:58, 24 July 2012 (UTC)

Added the improved version.
:For high=200000 0.6 instead of 6.8 seconds.
:--[[User:Walterpachl|Walterpachl]] 16:22, 24 July 2012 (UTC)
=== Please re-check with more memory ===
It looks like it has spent most of the time in garbage collection before failing, explaining this performance.
Please rerun with -Xms256M -Xmx512M at least.
[[User:Rvjansen|rvjansen]] 10:27, 23 July 2012 (UTC)
:I shall try this in the evening. Thanks --
:: but couldn't
:by the way for high=500000 PL/I needs 5 milliseconds on TSO!
:for high=600000 it bombs with the storage I have

[[User:Walterpachl|Walterpachl]] 10:40, 23 July 2012 (UTC)
== Java ==
Please replace the horribly inefficient published Java code with the following:


```java5
import java.util.ArrayList;
import java.util.List;

public class Eratosthenes {
    
    public List<Integer> sieve(Integer n) {
        
        List<Integer> primes = new ArrayList<Integer>(n);
        
        boolean[] isComposite = new boolean[n + 1];
        
        for(int i = 2; i <= n; i++) {
            
            if(!isComposite[i]) {
                
                primes.add(i);
                
                for(int j = i * i; j >= 0 && j <= n; j += i) {
                    isComposite[j] = true;
                }
            }
        }
        
        return primes;
    }
    
}
```


The problem here is the concept of removing values. Even the "optimisation" of filling nums with odd values is no optimisation at all. Trying to sieve 40004 for the posted algorithm takes around 3.292s ("optimised") while the algorithm above takes 0.006s and the bitset sieve clocks at 0.008s. Thanks --[[User:Xelamitchell|xelamitchell]] 19:50, 7 October 2013 (UTC)

... Except that the above Java code isn't a ''Sieve of Eratosthenes''.   A ''SoE'' algorithm doesn't '''TEST''' for primality, it ''just'' removes all composites (and sometimes unity);   what's left is a (somehow marked/indicated) list/array of primes  [up to the (original) highest element in the list/array of integers]. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 22:50, 7 October 2013 (UTC)

:Hmm, I'm not sure. If by primality check you mean the line "if(!isComposite[i])" then it is doing the same work of the posted code's "nums.remove()" (finding the next valid prime to begin marking, it is not a method merely a reference to the array) only much more efficiently. Also my code is truer to the algorithm (in terms of algorithmic description, pseudocode, end representation (having a marked list of values and a separate list of collected primes by the end of the run) and performance) as described in [http://en.wikipedia.org/wiki/Sieve_of_eratosthenes Wikipedia - Sieve of Eratosthenes] than the posted code. I don't know where you got the requirement that composites must be removed, the original algorithm merely states they must be '''marked iteratively'''. --[[User:Xelamitchell|xelamitchell]] 10:22, 8 October 2013 (UTC)

:: My bad.   I should've said something like:   ... it "just" removes/marks/indicates all composites ...     And then, the primes are those integers ''not'' marked (or the integers that are left remaining). -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 20:12, 10 October 2013 (UTC)

:::So what exactly do you mean by "primality check" in this case? If you check most of the sieve codes for other languages they are very close to my code (check the array of values to see if the current value is a prime (by the fact that it is marked true), if it is a prime mark all it's multiples as composites iteratively). --[[User:Xelamitchell|xelamitchell]] 10:18, 11 October 2013 (UTC)

:I think the <tt>BitSet</tt> approach at the bottom of the Java example does something like this. I bet the timings would be similar. --[[User:Mwn3d|Mwn3d]] ([[User talk:Mwn3d|talk]]) 18:33, 11 March 2014 (UTC)

== Python translated to Java ==

The Java incremental "infinite" generator as translated from the Python version uses the short 32-bit int type and thus suffers from an early overflow problem limiting the useful sieve range.  This is because in Java, short fixed range 32-bit two's complement int's have a range from -2^31 or -2,147,483,648 to 2^31 - 1 or 2,147,483,647 and by default don't have an error exception on overflow, so "roll over" from the highest maximum positive value to negative numbers when that value is exceeded.  Java's default integer literals are these short 32-bit integers unless suffixes are used along with type designations to change to the "Long/long" types or to the "BigInteger" (arbitrary precision) type.  This is unlike Python 2, which automatically promotes the type to the arbitrary precision "long" integer type when the value exceeds the "sys.maxint" value.  For the Incremental Sieve of Eratosthenes using 32-bit signed integers as described above, this Java overflow starts for the next prime above the floor of the square root of 2 ^ 31 - 1 or the prime value 46349, for which the square is 2148229801, and due to roll over will be recorded as -2146737495.  This negative number then becomes the highest priority (lowest value) in the Priority Queue, at least until some other square value produces a number even closer to the negative minimum value.  No positive prime candidate will ever match the top of the queue, meaning that all subsequent positive candidate numbers will be flagged as primes, the prime multiple culling sequences will never be advanced, and the algorithm is incorrect from that point on.

The development of the incremental sieve as per [http://www.cs.hmc.edu/~oneill/papers/Sieve-JFP.pdf Melissa O'Neill's orginal paper] in Haskell from which most of these algorithms are derived would have had the same problem except that Haskell defaults to the infinite precision Integer type rather than to Int32, which would have masked the problem.  O'Neill went on to do further work using postponed adding of base primes to data structures as done for the improved algorithms.  Doing this greatly increases the sieve range for a given number precision without overflow and possible removing the requirement for the less efficient infinite precision Integer type while greatly reducing the memory footprint and improving the run efficiency.

With the fix to a higher precision type, the Priority Queue code is adequate for sieving trivial ranges of primes up to a few million as for solving Euler problems but for higher ranges of a billion or higher, the use of a hash table based HashMap is somewhat better because it does not have the additional log n computational complexity overhead for sequence re-insertions, but even that is 10's of times slower than using an "infinite" generator/iterator using page segmentation as per the JavaScript example due to the constant overhead of the hashing computations as compared to direct array access.

This bug highlights a problem with the task as described since even if there is a output shown, showing the primes up to 100 doesn't show that the algorithm has been tested over various subsets of the full range or any consideration of the limitations of the algorithm has been made.

:Python 2.x int's automatically extend to longs, and so may not show the problem you state above. (Sorry that it has taken me so long to look into this):
:
```python
Python 2.7.5 (default, May 15 2013, 22:43:36) [MSC v.1500 32 bit (Intel)] on win32
Type "copyright", "credits" or "license()" for more information.
>>> x = 2147483647
>>> x
2147483647
>>> type(x)
<type 'int'>
>>> x = x + 1
>>> type(x)
<type 'long'>
>>> x
2147483648L
>>> 
```

:--[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 09:06, 24 June 2014 (UTC)
::Yes, I looked into it further as well and the problem does not apply to Python but only to the Java implementation in its current form using 32-bit int's.  I have corrected the comment above to match these findings, leaving the comment as related to the Java code.--[[User:GordonBGood|GordonBGood]] ([[User talk:GordonBGood|talk]]) 11:07, 24 June 2014 (UTC)

== Classic REXX timings ==

It might be a better idea to time the Classic REXX programs with a classic REXX   (if it serves a useful purpose). 

'''ooRexx''' programs should be entered under the '''ooRexx''' language entry (and their timings, if any). 

As I understand the Rosetta Code philosophy (theology? [sic]), it isn't considered kosher to compare timings for different languages, that's not the purpose of this forum.   Comparing two different versions of (and in) the same language is (maybe) supposedly OK, but maybe the absolute times should be expressed as percentages instead (if at all).   Another possibility is to just move the relative speed merits of the pertinent programming language entries to this talk/discussion page (with caveats).   It doesn't make sense to me to execute a (classic) REXX program using ooRexx (for timing purposes) under the (Classic) REXX language entry   (even though it can't execute as-is anyway). -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 23:06, 24 July 2014 (UTC)

All the Classic REXX programs that I programmed and entered were designed/written to be run with a Classic REXX interpreter, not ooRexx.   I very rarely post timings for the REXX programs I enter, except to sometimes say that   ''a''   version is   xxx%   times faster (or just faster) than a previous entry that I wrote/entered.   If it's slower (and a CPU hog), I almost never enter it unless it demonstrates a different or novel technique, or adds more function.   Sometimes, I add a very heavily commentized version. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 23:06, 24 July 2014 (UTC)

I would not be adverse to the idea of removing the whole timing business.   As improvements (or any changes,for that matter) are made, the timings become obsolete (or at least, outdated). -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 23:06, 24 July 2014 (UTC)

-----

== Trial division sieves should be moved from here ==

As the title says. The context for this is the discussion at [[User talk:Elibarzilay|Eli Barzilay's talk page]].

Here's my response to that discussion.

It is most definitely a sieve, just not the sieve of Eratosthenes as it is understood today, and at least as early as 1772 -- time of publication of the article by Horsley, referenced in the Wikipedia article, in which he much trashes Nicomachus (yes, trashes, it is beyond criticism), incidentally, for being unable to convey clearly the essence of Eratosthenes's method. 

'''It indeed is a ''trial division'' sieve - a sieve where the removal is done by trying out''' (for each candidate number, in separation, one after the other) '''the divisions''' by primes, one prime after another, as opposed to the "true" sieve of Eratosthenes finding out the composites as primes' multiples, and getting the primes for free, in the gaps between the multiples, as "what's left" after all the numbers are let ''through "the sieve"'' (when holes are made under the composites).

I.e. in the proper SoE '''the sieve is ''constructed'' (for a range of numbers as a whole) from primes, by ''counting'' (i.e. by repeated additions); no number needs be attempted to be divided by any other number. And that's the difference between the two.'''

A defining characteristic of an algorithm is its theoretical time complexity. The sieve of Eratosthenes' complexity is N log (log N) ~ n log n log (log n), for n primes below N; the (''optimal'') trial division sieve's - N^1.5/(log N)^2 ~ n^1.5/(log n)^0.5, as derived in M. O'Neill's paper. Granted, that paper is short on clear-cut explanations, but it has the needed math derivations nonetheless. 

The real question we're left with here, is whether to open up a new page for "trial division sieves", or to add - both Scala and Racket, and any other entry's there might be - to the [[Primality by trial division]] page. Which I did, incidentally, for the Haskell entry, long time ago, where we can find this unoptimized simple famous code both in its original, and optimized formulation. 

'''''I propose all such entries are, indeed, moved to [[Primality by trial division]]''''', for now; and if the special page gets created for them later, they can be further moved there. Right now, having trial division sieves under the heading of "sieve of Eratosthenes", on the page which explicitly forbids anything other than the proper algorithm, seems to be a no-go proposition. -- [[User:WillNess|WillNess]] ([[User talk:WillNess|talk]]) 19:59, 10 September 2014 (UTC)

:+1 --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 20:18, 10 September 2014 (UTC)

:+2   (for the moving part, but to where?) --- although many of the entries are a combination of a Sieve of Eratosthenes ''optimized'' with trial division (mostly with just low primes, however), and it would probably be a good idea to have a Rosetta Code task just for those moved from   '''Sieve of Eratosthenes'''   to   '''Sieve of Eratosthenes with wheel optimization''',   or some such title.   It would make the decision to choose which candidates to move a simple one.   Almost all of those that would (possibly) be moved to primality by division probably wouldn't fit the requirements for primality by division as they are hybrids.   I'd hate to see all those optimizations and commentaries be put into the wrong pigeonhole. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 20:43, 10 September 2014 (UTC)

::... should be moved to [[Primality by trial division]]. See [[Primality by trial division#Sieve_by_trial_division|the Haskell entry]]. The "optimal" in my phrase "optimized formulation" refers to the postponement of opening up new filters until a prime's square. '''This not about the wheel optimization'''. The wheel optimization is allowed on ''this'' page, it is clearly stated in the preamble. But it would have to be the wheel optimization applied to a sieve of Eratosthenes, not a trial division sieve. -- [[User:WillNess|WillNess]] ([[User talk:WillNess|talk]]) 21:04, 10 September 2014 (UTC)

::perhaps you misunderstood. (?) In Haskell, it is the difference between


```haskell
primes = sieveTD [2..] where  -- suboptimal trial division sieve
   sieveTD (p:xs) = p : sieveTD [x | x <- xs, mod x p /= 0]
```

::and

```haskell
primes = sieveEr [2..] where  -- suboptimal sieve of Eratosthenes
   sieveEr (p:xs) = p : sieveEr (diff xs [p, p+p, ...])
```

:: The first version and its equivalents in other languages, is the subject matter here. -- [[User:WillNess|WillNess]] ([[User talk:WillNess|talk]]) 21:11, 10 September 2014 (UTC)

::BTW wheels can be generated, without using any trial divisions, too (e.g. [http://www.haskell.org/haskellwiki/Prime_numbers#Wheeled_list_representation this]). -- [[User:WillNess|WillNess]] ([[User talk:WillNess|talk]]) 21:18, 10 September 2014 (UTC)

: I also think they should be moved.  This topic is, I believe just about moving "sieve by trial division" implementations off of this page.  We have multiple possible tasks, not all of which need their own page:
: * [[Primality by trial division]]: given n, return boolean indicating whether n is prime
: * [[AKS test for primes]]: primality by extremely inefficient trial division
: * Sieve by trial division: find primes up to n using trial division (you may optimize vs. just looping over the previous task).  These should not be on the SoE task, but whether they're just pushed into the [[Primality by trial division]] task or made as a separate task is a question.  I believe it should be a separate task:  (1) it doesn't clutter the very simple primality task, (2) it gives us a good place to put them and explain the differences, and (3) it's an interesting task for all languages, albeit not the most practical.
: * [[Sieve of Eratosthenes]]: monolithic sieve up to n
: * Sieve of Eratosthenes with wheel optimization: a proper SoE but skipping multiples of 2, 2+3, 2+3+5, etc.  There are lots of examples of this on the current page.  I rather like the way this is currently set up, with a simple sieve as requirement, and additional optimizations as optional.  On the other hand, making it a separate task would remove clutter.
: * [[Extensible_prime_generator]]: automatically adjust size.  The current task doesn't indicate how this is done, and a cursory glance shows examples using trial division, wheel trial division, M-R tests, full SoE reseives, expand by segment SoE, segment SoE iterators, segment SoA iterators, etc.
: I'd like to see a segmented sieve task (generate primes between ''a'' and ''b'' without generating all primes to ''b''), but that's a completely different topic.  [[User:Danaj|Danaj]] ([[User talk:Danaj|talk]]) 08:51, 11 September 2014 (UTC)

::Just to correct one thing: "generate primes between ''a'' and ''b'' without generating all primes to ''b''" (to ''a'', you mean?)  is '''not''' a segmented sieve. Segmented sieve proceeds by segments, one after another, storing the found primes at a side storage. With just one fixed segment, I've seen it called an "offset" sieve. It is, also, a subtask of "produce all primes above ''a'' indefinitely". If we split the bounded sieves (such that work up to a limit) and the unbounded ones, the two would naturally go each into its respective parent. But even bounded sieve may validly be implemented as segmented a.o.t. monolithic sieve, so perhaps there's no place for this split (and it ''is'' a separate split, monolithic vs segmented). Having them all in one place makes for one compelling story.

::Whatever is decided though, moving the trial division sieves to the trial division page, ''first'', should be a priority. I think. -- [[User:WillNess|WillNess]] ([[User talk:WillNess|talk]]) 10:07, 11 September 2014 (UTC)

::: I agree with you on the last point.  Re the digression:  on to ''a'' vs. ''b'': the naive way to generate primes from ''a'' to ''b'' is to generate to ''b'' and then ignore all the ones earlier than ''a''.  Hence to ''b''.  The main point being that we don't make an array for all values from ''0'' to ''b'', but just ''a'' to ''b'', plus intermediate storage (to ''sqrt(b)'' for a full sieve).  I suppose the right way to do the task is to let the implementers show multiple ways to do it.  You are correct on the distinction between segment and segmented.  As my text is written the ''a'' to ''b'' range could be done as a single block, hence it is a segment or offset and is a different concept from a segmented sieve.  Certainly a bounded range can be done as a segmented sieve and for large ranges they are much faster.  Easily allowing non-zero starting points is a bonus.  [[User:Danaj|Danaj]] ([[User talk:Danaj|talk]]) 11:17, 11 September 2014 (UTC)

:::: Ah, ISWYM, now. Yes, to ''b''. Just wanted to reiterate: "the distinction between ''offset segment sieve'' and ''segmented sieve''. :) -- [[User:WillNess|WillNess]] ([[User talk:WillNess|talk]]) 11:51, 11 September 2014 (UTC)

::I've now read the Scala entry closely, and I find it is mischaracterized in the discussion I referred to at the very top of this subsection. It only contains one very short (three lines) code of trial-division--based sieve, which is used as a backdrop for further discussions of the following variants, all of which are proper sieve of Eratosthenes. -- The Racket section OTOH has just 2 short proper SoEs, and three variants of trial-division sieve. -- [[User:WillNess|WillNess]] ([[User talk:WillNess|talk]]) 11:56, 11 September 2014 (UTC)

:::So I've moved the three Racket sieves to [[Primality by trial division#Infinite list of primes]]. Will try to detach the Scala TD-sieve and move it too, later. -- [[User:WillNess|WillNess]] ([[User talk:WillNess|talk]]) 16:21, 11 September 2014 (UTC)
:::There. [[Primality_by_trial_division#Odds-Only_.22infinite.22_primes_generator_using_Streams_and_Co-Inductive_Streams|Moved Scala as well]]. -- [[User:WillNess|WillNess]] ([[User talk:WillNess|talk]]) 16:31, 11 September 2014 (UTC)

::::WillNess: First of all, I haven't read the Scala entries in depth, mostly looked at the decriptions -- and there is still one left behind that starts with "As per the "unfaithful sieve" article linked above", and another that talks about being much better than the unfaithful version which is no longer there.  So at least these descriptions should be fixed.
::::Second, IMO moving those things into some subsections of "Primality by trial division" is a huge mistake, since it has way more in common with the SoE than the latter.  Like I said in my talk page when this was discussed, I think that if these things are moved, then they should all move consistently (which looks like you're trying to do) and they should move to their own page.  Making these things be such subsections demotes them to look like side-comments, which is a disservice to something that is somewhat popular in programming courses as a demonstration of infinite list implementations, (I know of at least three other place in addition to my own course), and like you say, it is *a* sieve. (One of these places is https://www.youtube.com/watch?v=hB05UFqOtFA which has a nice explanation of how it works, and makes it pretty obvious that it is a sieve.) --[[User:Elibarzilay|Elibarzilay]] ([[User talk:Elibarzilay|talk]]) 18:19, 11 September 2014 (UTC)

:::::re "own page": I was ambivalent but you've convinced me, at least. I think I'll start that page if nobody else will beat me to it. :) 
:::::about Scala entry: thanks, will take another look (anyone can, that's a wiki, right?). That article also contained the proper sieve list-based code by Richard Bird, in the Epilogue, and that was the reference that you saw, I think. I tried leaving a link there to the newly moved entry though. 
:::::And thanks for the link! Very interesting.  -- [[User:WillNess|WillNess]] ([[User talk:WillNess|talk]]) 07:14, 12 September 2014 (UTC)

::::::Well, I think that the main difficulty in a new page is explaining what it is, but it sounds like you have that part clear.  Probably also good to make it close enough to the SoE page, maybe have references from each one to the other which can also simplify the explanation.  Moving the entries should be easy then, since they're not too linked to the current text around them (perhaps except for Haskell if you've done that a while ago).  BTW, I've added SoE versions now, with the same three versions -- it's just that their additional verbosity makes the equivalence between the three versions harder to see.  They're also roughly as fast as the other version, mostly because I'm too lazy to find out if there's some magic possible that's supposed to make it much faster.  (In any case, they're both fast enough to make finding the 19th prime number very fast, and Racket's Lazy language is really not something that can be considered "fast" -- so I still have no idea what that paper mumbled about.)  --[[User:Elibarzilay|Elibarzilay]] ([[User talk:Elibarzilay|talk]]) 08:22, 12 September 2014 (UTC)

::::::: The "magic" trick is in postponement - check [[Primality by trial division#Postponed sieve by trial division|the Haskell entry]] to see what I mean (also [http://stackoverflow.com/questions/6802112/why-is-this-scala-prime-generation-so-slow-memory-intensive/14821313#14821313 explained here], among other places). I've also wrote quite a lot of verbiage on the haskellwiki prime numbers page about that. The author just missed that small step, and went all the way to do the proper implementation (which still had no postponement, in the article, but the priority queue kept the issue hidden, for a while), saw the enormous speed difference, and wrote about it (I guess). But there really is no giant leap, because there is an intermediate step that the author glossed over, so there are two big steps instead of one giant jump. Still impressive, but not that ''flabbergasting''. And unfortunately, her explanations were lacking in clarity. The basics of it is, coming back full circle, the distinction between TD-sieve and "genuine" ;) SoE: the first tests each number by ''all primes'' below it (or its square root), potentially, whereas the other generates for each prime its multiples starting from it (or its square) - and thus generates each composite only from ''its actual prime factors''. The first ever corecursive algorithm. Exciting stuff. :)

:::::::: The author didn't say as much, but she did say that the Turner's sieve is equivalent to the trial division which uses all the primes below the test number. Hence the complexity of ''O(n^2)'' to produce ''n'' primes. Then she actually derived the complexity of producing primes with filtering by trial division up to the ''sqrt'' of the test number (which is achieved with the postponement technique). Which is ''O(n^2/(log n)^0.5)''. She didn't show any code in the form of ''sieve'' that would achieve that, but instead the one with ''filter'', AFAICR. --[[User:WillNess|WillNess]] ([[User talk:WillNess|talk]]) 15:03, 12 September 2014 (UTC)

::::::::: Well, the postponement trick is nice, but I don't see how it can apply to the SoE code.  As a quick experiment, I implemented it for the plain (non-E) sieve, and indeed it made it much faster.  I then implemented the Bird SoE algorithm and it's faster than the version I had before -- but the plain sieve + postponement trick is about twice faster than my Bird implementation.  (I'll put both of the new things on the two pages, if you care to look at them.)
::::::::: Also, looking at your SO answer, I don't see the conceptual difference between what you refer to as the Bird algorithm and the one at the end of the answer: the two minor differences that I see are (a) it works only on non-even numbers; (b) you use the union' thing to always spit out the first element which is done in Bird's code by pulling out the first p*p out of the union code (in fact, I first didn't do this pulling-out, and resolved the resulting infinite loop by making my merge do the same thing that your union' does). --[[User:Elibarzilay|Elibarzilay]] ([[User talk:Elibarzilay|talk]]) 19:32, 13 September 2014 (UTC)

:::::::::: Elibarzilay: (sorry, missed your remark before): the big conceptual difference is that it's using the [http://en.wikipedia.org/wiki/Fold_%28higher-order_function%29#Linear_vs._tree-like_folds tree-like folding] function [http://en.wikipedia.org/wiki/Fold_(higher-order_function)#Tree-like_folds ''foldi''] instead of the linear folding of ''foldr'', for the logarithmic complexity advantage. M.O'Neill derives Bird sieve's complexity at above n^1.5 (in ''n'' primes produced) but the tree folding code runs empirically at the same orders of growth (~ n^1.2..1.25) as her PQ-based code, which is probably ''n (log n)^2 log(log n)''. Earliest such code that I know of is [https://www.haskell.org/pipermail/haskell-cafe/2007-July/029077.html Jul 16 2007 post by Dave Bayer]. -- [[User:WillNess|WillNess]] ([[User talk:WillNess|talk]]) 21:46, 10 November 2014 (UTC)

::::::: I'm having second thoughts about the new page now (bear with me). See, the Greek "koskinon" is a noun; it is "sieve" in English, but "sieve" in English is also a verb, a synonym of "filter". I think, ''that'' was the cause of much confusion about this issue. With the Trial Division, it is very easy and natural to use it to produce sequences of primes, unbound as well as bound, by means of filtering. And Turner's code is only "sieve" in as much as it is "filter", it doesn't build "a sieve". And how will we distinguish between the TD "sieve" and TD "filter"? The first does it in separate steps, filtering separately by each prime, and the other does that by testing against a list of primes, by a short-circuiting logic? That's not a ''basic'' distinction, [[Primality by trial division#Segmented Generate and Test|that's an optimization]]!  What will we do, allow the former but forbid the latter, on the new page??

::::::: So I'm not sure about this. What do you think? I proposed to amend the description of TD page to include generating sequences; we may mention the word "sieve" there, and of course discuss the similarity and distinction etc.... (???) -- [[User:WillNess|WillNess]] ([[User talk:WillNess|talk]]) 10:03, 12 September 2014 (UTC)

(unindent) So, how about if we '''create the new page''' titled '''"Sequence of primes by Trial Division"''' which would include all kinds of ways to do that, including "sieves", bounded and not, monolithic and not, from 2 or not; and also by filtering.''' Please state your opinion.''' -- [[User:WillNess|WillNess]] ([[User talk:WillNess|talk]]) 13:32, 13 September 2014 (UTC)

:* I was being bold and went ahead and created a draft task page as discussed above: [[Sequence of primes by Trial Division]]. I don't want to be ''too'' bold, so someone else please remove the draft status, if you agree. -- [[User:WillNess|WillNess]] ([[User talk:WillNess|talk]]) 14:06, 13 September 2014 (UTC)

:: I don't consider myself active enough to remove the draft thing too... but of course I think that it's the right thing.  --[[User:Elibarzilay|Elibarzilay]] ([[User talk:Elibarzilay|talk]]) 19:32, 13 September 2014 (UTC)

::: WillNess: Sorry, but continuing down here...  What I did now is: (1) Removed the sequence versions from the PbTD page since they're on the new page; (2) I first edited your version on the SoE page, but then realized that my attempt at doing the same to the Bird code is shorter and faster, so I dropped both in *instead* of your code -- I don't like removing other people's code, but I decided on doing that because it's better on speed, clarity, and being more idiomatic (feel free to tweak it however you like, specifically, the timings might be off); (3) On the new page, I've added a version that is optimized with the postponement trick.
::: FWIW, I'm getting results that still don't go in-line with the conclusion that a proper SoE should be faster: the "unfaithful" plain sieve with the postponement trick <1> is about twice faster than my Bird implementation with the same trick <3>, and that is a tiny bit faster than my Bird implementation without the trick <2>, while your version of the proper SoE with the trick is about three times slower than <1>.  Based on the paper and what you said, I'd expect <1> and and <2> to be roughly the same, and <3>/<4> to be much faster.  --[[User:Elibarzilay|Elibarzilay]] ([[User talk:Elibarzilay|talk]]) 20:16, 13 September 2014 (UTC)

::::Elibarzilay: good to have a discussion with you. :) About Bird (your <2>) (and it's Richard Bird, not William Byrd): O'Neill actually derives the complexity for it too, and proves it's slightly worse than the optimal TD (your <1>). The trick to making it yet faster (which isn't in the paper) is to fold the composites ''in a tree'', substituting [https://en.wikipedia.org/wiki/Fold_(higher-order_function)#Tree-like_folds "foldi" for "foldr"]. I have a Scheme entry with SICP styled streams, that does that (and of course [[Sieve_of_Eratosthenes#List-based_tree-merging_incremental_sieve|the Haskell entry]] itself). The original version at [http://www.haskell.org/haskellwiki/Prime_numbers_miscellaneous#Implicit_Heap haskellwiki]. 
::::And of course what we should be comparing are [http://en.wikipedia.org/wiki/Analysis_of_algorithms#Empirical_orders_of_growth empirical orders of growth], not constant factors. So if one is constantly slower than the other by the same factor ''at several size points'', it's fine. TD with postponement (i.e. the optimal TD, your <1>) runs pretty well for lower ranges (10k-20k primes) in Haskell too. It's because its complexity is O(n^1.5/(log n)^0.5) and at low ranges ''log'' is approximated by higher power law coefficient, i.e. its influence can be sizeable (I've commonly seen n^1.3 for low ranges of OTD, going steadily to n^1.45..49 for higher ranges, as it should).
::::About your edits: there's no need to add postponement to Bird's code; it achieves the same effect on its own, because it folds to the right. Postponement achieves the same effect laboriously, and should still be worse computationally [http://www.haskell.org/haskellwiki/Prime_numbers#Linear_merging because it folds from the left]. My version should probably be restored (or maybe you have a tweak on it) because it is a stepping stone from the simple lazy version ''to'' the Bird's version. (It's strange that your "postponed Bird" is slightly faster than the straight version of it.) -- [[User:WillNess|WillNess]] ([[User talk:WillNess|talk]]) 09:17, 14 September 2014 (UTC)

::::Elibarzilay: so I've switched back my postponed version for your postponed Bird's, and placed it ''before'' Bird's. You're welcome to tweak it with your ''when-bigger'', or I'll do it when I have more time (will have to check if it's faster or not). It's more-or-less equivalent to Haskell's <code>span</code>-using versions; but with Haskell's GHC, manually inlining the ''span'' as I did here, was more efficient.  -- [[User:WillNess|WillNess]] ([[User talk:WillNess|talk]]) 10:09, 14 September 2014 (UTC)

::::: WillNess: I've fixed some of the indentation problems, and also removed the <code>#lang lazy</code> from partial sources (they're misleading, since the <code>#lang</code> should be used at the top of a module and determines its language, it's not just a typographical annotation of the text...).  As for the speed, factors are obviously not interesting in theory, and high factors are interesting in practice only.  But I didn't measure them or tried to analyze the runtime, I just tried a few semi-long runs since I'm too lazy to invest more time in this...  You're welcome to do more detailed analysis of course (and to mention it: something that I didn't do since I didn't put in the effort...).  Oh, and btw, my <code>when-bigger</code> trick is to avoid calling another function that will call itself recursively to collect two values, since this kind of thing would be disastrously slow in Lazy Racket (I should know, I implemented it...).  My solution was a haskell-span-like function that takes a callback for the tail, which is enough here, and should generally be fast enough and clear.  --[[User:Elibarzilay|Elibarzilay]] ([[User talk:Elibarzilay|talk]]) 05:45, 15 September 2014 (UTC)

::::::yes, I know (both about the implementation :) and the meaning of the function). I even wanted to rename it <code>span</code>. -- [[User:WillNess|WillNess]] ([[User talk:WillNess|talk]]) 10:59, 15 September 2014 (UTC)

== Recent edits to Haskell and my reversal of them ==

why I made [http://rosettacode.org/mw/index.php?title=Sieve_of_Eratosthenes&diff=201761&oldid=201693 this edit]:

Much careful thought went into gradually presenting the codes. No function was missing, it was defined in a sub-section above its use. No need to define same "minus" function over and over. In Haskell, short variable names are idiomatic; long ones with tortured pronunciation aren't. "gaps" is perfectly idiomatic and is clear upon examining the code. Your redefinition of "minus" is not an improvement, as it uses two calls "<" and ">" instead of one "compare". You confused between "left" and "right" leaning structure, and it is anyway explained in the following subsection. Similarly, "gap" appears in the following subsection, as an improvement, no need to improve the previous function as it only serves as an illustration anyway. The spacing in the simple unbounded version is carefully chosen to follow closely the spacing of the original Miranda code by D. Turner. etc. etc. etc. 

I kept several of the remarks you've added. Thanks. -- [[User:WillNess|WillNess]] ([[User talk:WillNess|talk]]) 14:06, 10 April 2015 (UTC)

: also, "naive" version is ''not'' based on Euler's definition. The primes in "primesEQ" are not separate, but the same - this is by design; for the reader to see the difference when separate supply is used later, in the more efficient versions. Maybe we ought to add types as you did in all the versions, not as it is now in the most efficient tree-merging version only. Will think it over...  -- [[User:WillNess|WillNess]] ([[User talk:WillNess|talk]]) 14:16, 10 April 2015 (UTC)

: also, writing "primesEulerQ ()" (with the ()) is not guaranteed to prevent memoization. A compiler could "optimize" even this code, recognizing it as "common subexpression", causing it to be memoized. Only "_Y" guarantees this (well, so far, as I could see, for various versions of GHC). [[User:WillNess|WillNess]] ([[User talk:WillNess|talk]]) 14:29, 10 April 2015 (UTC)

:: I recognized this as your code, Will, and the main reason I made changes was so that a noobie reader could just copy and paste the code segment into a '.hs' file or REPL and try it out without without needing to patch in a "where" or definition of a function from a previous entry in the progression of codes or look up an import; for instance, I don't think one can use "compare" without importing "Data.Ord" (or at least on my version - Windows/HP 2014.2.0), which is why I changed the codes to not use "compare".  Yes, I know this appears from the outside as two operations rather than one, but there are at least two operations inside "compare" and we aren't trying for the ultimate in efficiency here anyway.  Anyway, just showing the "import Data.Ord" would work.  It seems to me that RosettaCode is most often used by people new to a language to get a flavour of how it is used, and who may want to just paste in a code segment to try it and learn about it.  If you see that this could be valuable to them, then make those changes yourself.  And no problem with your reverting of my changes, I was just trying to help beginning Haskell programmers as I was in the not-too-distant past.  --[[User:GordonBGood|GordonBGood]] ([[User talk:GordonBGood|talk]]) 01:54, 11 April 2015 (UTC)

::: I just tried to make a clear presentation (you know, one feels invested and all that stuff...). Yes, that's a valuable point about the ability to run the code as-is.  But OTOH to copy-paste some code from a near-by section shouldn't be much of a bother, hopefully. "compare" works inside GHCi right away, BTW, w/ 2014.2.0.0. on Win7. [[User:WillNess|WillNess]] ([[User talk:WillNess|talk]]) 14:44, 11 April 2015 (UTC)

::: but I kept most of your remarks and incorporated your clarifications (about Y etc.) so hopefully overall the quality had improved, thanks to your intervention. :) [[User:WillNess|WillNess]] ([[User talk:WillNess|talk]]) 14:50, 11 April 2015 (UTC)

:::: Strange, I just tried compare again without the "import Data.Ord" and it works, but I distinctly remember getting compiler errors saying "perhaps you meant 'compare' from Data.Ord" - never mind that point, my eyes are getting bad and perhaps I mistyped it and couldn't see the difference...

:::: I do think that you should add type signatures to all of the top level functions for two reasons:  1) its recommended Haskell style, and 2) if there are no signatures, integer numerics default to "Integer" which can be considerably slower than "Int".

:::: I see your point on using the combinator to avoid holding the lists in memory '''for the actual sequence of calculations''', however making each a function as in "primes :: () -> [Int]" rather than "primes :: [Int]" does two things:  1) for noobie people testing timing and such, it means that on every call to the '''function''' the whole calculation sequence starts from the beginning rather than using that portion of the memoized stream already computed, and 2) having the outer binding means that the result stream can not be garbage collected as the stream is consumed by say "head $ drop 1000000 primes" where I am suggesting "head $ drop 1000000 $ primes ()" will not hold onto the results stream and will have a very low memory residency; one does have to be careful when making recursive references (not using "fix" or combinators) as a reference to a function does generate a whole new stream - when that is not desired, internal simple bindings must be used to avoid the generation of new streams when they are not desired.  --[[User:GordonBGood|GordonBGood]] ([[User talk:GordonBGood|talk]]) 00:10, 12 April 2015 (UTC)

:::::  but those versions are inefficient anyway. Plus, the proper way to test is to run a compiled, standalone executable, where even <code>primes :: [Int]</code> is garbage collected and runs in near constant memory; while if playing at GHCi prompt, depending on a version, I've seen even <code>primes :: () -> [Int]</code>  memoized. But here I thought the aim is to showcase a language not even to (practicing) newbies (there are plenty more other resources for those), but to people even totally unfamiliar with it, just to glance over (but the snippets are still runnable of course). So for me, clarity was the utmost goal. [[User:WillNess|WillNess]] ([[User talk:WillNess|talk]]) 12:06, 12 April 2015 (UTC)

==Problem found in first Erlang example.==
A note. This code bugs out on 

```txt

3> erato:primes_upto(100).
** exception error: no true branch found when evaluating an if expression
     in function  lists:seq/3 (lists.erl, line 249)
     in call from erato:find_prime/3 (erato.erl, line 18)
     in call from lists:foldl/3 (lists.erl, line 1248)
     in call from erato:primes_upto/1 (erato.erl, line 10)

```


Problem found by user  Poetaster . I just moved the report here. --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 06:53, 19 September 2015 (UTC)

== R code error ==

In the R code, the inner loop does not start at the square of the prime just found. The code tries to change the index of the loop using last.prime. 


   for(i in last.prime:floor(sqrt(n)))
   {
      primes[seq.int(2L*last.prime, n, last.prime)] <- FALSE
      last.prime <- last.prime + min(which(primes[(last.prime+1):n]))
   }
   which(primes)
}
 
However from R in the help function - The ‘seq’ in a ‘for’ loop is evaluated at the start of the loop;changing it subsequently does not affect the loop. If ‘seq’ has
length zero the body of the loop is skipped. Otherwise the variable ‘var’ is assigned in turn the value of each element of‘seq’. You can assign to ‘var’ within the body of the loop, but this will not affect the next iteration. When the loop terminates,
‘var’ remains as a variable containing its latest value.
If you wish to change the loop you can use a while or repeat if written below.

sieveOfEratosthenes<-function(n){
  list<-2:n        #list of numbers 2:n
  A<-rep(TRUE,(n)) # Vector of true - if true at end prime
  i<-2            #index at 2 first
    repeat{
      if(A[i]==TRUE)
          {
        A[seq(i**2,n,i)]=FALSE
              }
      if(i>sqrt(n))
          {
        break
        }else{
        i<-i+1
          }
    }
  return(list[A[-1]]) #remove 1
}
:Corrected. Some oddities in your own program: once you have computed i^2, no need to compute sqrt(n), just compare i^2 and n. Also, no need for 'list', use the 'which' function. You may replace 'break' with 'return' and 'repeat' with a 'for' loop since you are incrementing the index 'i' at the end of each loop. Last, but not least, your function fails for all n < 9, probably due to a nasty bug you didn't investigate: 'seq(a, b)' is not empty when a > b. [[User:Arbautjc|Arbautjc]] ([[User talk:Arbautjc|talk]]) 12:16, 12 December 2016 (UTC)
