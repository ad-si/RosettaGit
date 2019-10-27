+++
title = "Talk:Multiplicative order"
description = ""
date = 2007-12-11T08:01:49Z
aliases = []
[extra]
id = 2404
[taxonomies]
categories = []
tags = []
+++


###  Clarification 


There's more than one way to determine the multiplicative order. While my J is next to non-existant, I can tell you're not using one of the naive ones :-) As the point of Rosetta is to compare equal algorithms in different languages (and not different algorithms in different languages), please state in the task which algorithm you'd like to be used. 
Also, IMHO the tasks should be chosen to allow a meaningful comparison of languages, and I am not sure if this is a good task for this purpose, because it's more centered on the algorithm than on the possible different ways different languages work. [[User:Dirkt|Dirkt]] 03:57, 8 December 2007 (MST)
: The J solution implements the algorithm described in Bach & Shallit, <i>Algorithmic Number Theory I</i>, exercise 5.8, page 115. 
:: If you implement a specific algorithm, it would be nice to describe the algorithm for those who don't happen to have this book lying around.
: Judging by the other programming tasks, the submissions in the various languages don't always implement the same algorithm, but use algorithms that are "natural" to the language.  That too is a meaningful comparison. [[User:Roger Hui|Roger Hui]] 08:28, 8 December 2007 (MST)
:: But they should implement the same algorithm as far as mathematical principles, say, are concerned. (And if they don't, it's bad, and it should be changed.) Using different mathematical principles to arrive at the same result has nothing to do with what's "natural" for one language. And that's exactly my criticism here: I don't see how this task brings out features that are "natural" to a language. Instead it highlights features of a different algorithmic approach. It's of course tempting to show off with a clever "deep" algorithm, but it misses somehow the point.
:: And J has enough interesting features that require a different "natural" approach than in other languages that it would be really worthwhile to pick tasks which highlight this. And if you'd add a bit of explanation to really point this out in the implementation, for those not familiar with J, all the better.
:: One thing this Wiki is really good for is to try to get up to speed in an unfamiliar language, by comparing the approaches with those in a familiar language. This doesn't work if all similarity is suppressed, because the algorithms are essentially incomparable.[[User:Dirkt|Dirkt]] 13:46, 8 December 2007 (MST)

I'll see what I can do about an algorithm description.  I'd prefer to upload a jpeg image of the pages 
in question as my LaTex skills are probably not up to reproducing the formulae.
Implementation of the Bach-Shallit algorithm is facilitated by having
<ul>
<li>primes and factoring</li>
<li>extended precision arithmetic</li>
<li><tt>a^b modulo c</tt></li>
<li>vector operations</li>
</ul>
It would be interesting to see how the algorithm would be implemented in a language 
that lacks one or more of these features.
[[User:Roger Hui|Roger Hui]] 18:06, 8 December 2007 (MST)
: There's no need to upload any image - it just takes a few sentences to describe the algorithm. And this algorithm has NOTHING, absolutely '''NOTHING''' that makes it in any way "natural" to J - I could write a C version as easily as the Haskell version, even for large numbers. Just a matter of using the right libraries (for example ''gmp'', in the C case). Sorry to shout, but I am really angry I head to spend several hours learning enough J to figure out the algorithm, for what you could have done in five minutes. And I '''hate''' wasting my time. And I hate even more playing silly hide-and-seek games just because people want to show off "their" language.
: J, like APL, has lots of interesting features. The maybe two most prominent are IMHO that you can use HOFs to combine existing functions (in J terminology, ''adverbs'' and ''conjunctions''), and that the nearly only data structure are arbitrary-rank tensors, so you have to organize the program around that. I'd love to see simple examples that illustrate this in comparison to other languages. But IMHO implementing each of the zillion algorithms of moderate difficulty from number theory or other maths fields is just a waste of time, because it tells you '''NOTHING''' about the language whatsoever. Again, sorry for shouting, but I am still quite angry. [[User:Dirkt|Dirkt]] 07:40, 9 December 2007 (MST)

I accept your apologies for being angry and for shouting.
Nothing happened here that merited it.

You asked for a description of the algorithm.  
I provided a reference to a well-known text.  
Then you asked the description in the text.  
I promised to provide a reproduction of the text, 
but before I was able to do so (and within less than 12 hours) 
you spent a few hours learning enough J to derive an English 
description from the J program.  That is a testament to your skill, to how well the J program is written, and to J itself.
: I didn't want to criticize J, but I just cannot let this stand. The skill required wasn't very high, once I figured out the general approach of the J program, it was more or less obvious to proceed (even without the textbook). I can't comment on how well the program is written, but the main obstacle I had to face is that if you're not used to J, it is '''really''' hard to figure out what is going on. Every sequence of punctuation symbols has half a dozen different meanings, and you have to internalize most of that before you can make sense of the code. In many places, I just guessed the most obvious way to proceed, and then checked laboriously if the code matched my idea. And I still haven't completely figured out the second-last line of ''mopk''. So I wouldn't call this a "testament to J", at least as far as readability goes. And a simple English description like "For every factor p^k of m, factor &Phi;(p^k) in turn, then find for each factor q^e of it the smallest d such that a^(q^(d-e)*&Phi(p^k)) = 1" would have been enough. You could have written down this sentence in five minutes, but for some reason you don't seem to like to explain your own programs. Why is that? If you want to get to people to learn J, they '''need''' explanations.

Fine.  But then you complained and shouted that you
wasted a few hours.  Well, how you spend or waste your
time is your choice.
: The point is that I had to spend hours for what you've could have done in minutes, and for a task that really should be a natural thing: To document what a program does, on a high level.

The main reason I did not simply describe what the J
program did (as you did) is that the solutions in the
other languages should not be influenced by the J solution.
: I am sorry, but this is rubbish. This place is called '''Rosetta'''. The Rosetta-stone contained three texts with '''identical''' contents in different languages. That enabled researchers later on to learn an to this point unknown language when they already knew one of others. If instead each of the texts had been different versions, each exploiting the rhetorical depth of natural languages to the extreme, the researchers would have been quite lost, and nobody would know about the name "Rosetta" today.
: Here, we can do the same for programming languages. So it is imperative that all task solutions implement the '''same''' algorithm (in the way most natural for each language), otherwise no comparison is possible.
: This site is '''not''' about finding clever solutions to a problem, keeping this solution as mystical as possible, and then gloating about how inferior the other solutions are. It's not about posing problems that need clever algorithms to solve them, either, unless you clearly say in the task which algorithm you expect to use, for exactly the same reason: It makes no sense to compare different algorithms in different languages. It makes a lot sense to compare the same algorithm in different languages, however.

(Unless their authors choose to be so influenced.)
I have now typed in the description from Bach & Shallit
and will replace your description with theirs.
: BTW, this exercise only covers the inner loop (probably because the authors thought the generalization is obvious). I reverted to my original description, including the J comments, and added your excerpt at the end. I also tried to change some of the formulas to TeX-markup, but this Wiki doesn't seem to understand math-tags.

I was not playing hide-and-seek games.  If I were, 
I would not have provided a reference, nor provided 
a solution in a notation as clear as J,
: Sorry to contradict, but just because the notation of J is clear to you, it doesn't mean that it is clear to the rest of the world. Actually, in my example, it wasn't, and that's why I was '''asking''' for a description, '''twice'''. And instead of just writing down a rough description, in English, you were talking of uploading a copy of a textbook. Which, as it turns out, only documents part of the algorithm, includes lots of information that is irrelevant to understanding the algorithm, and wouldn't have helped me a lot understand the J program in the first place. I mean, there's really some sort of mismatch here, isn't there?

nor written the best J program that I could.

I disagree with your comment that "implementing
each of the zillion algorithms ... from number theory
or other math fields is just a waste of time".
: Don't take that out of context. Implementing such algorithms by itself is interesting, but it's a waste of time to do it here, especially if you don't document the algorithm in the first place, because it won't tell you anything about features specific to a language. Which is what this site is about.

The presence of the "Prime number" and "Sieve of Eratosthenes" 
programming tasks too argue against your position.
: Actually, both of them needed clarification for a precise description of the algorithm required, just because they were numerical problems. And the algorithms chosen are simple enough so that the implementations do expose interesting language features (the Haskell example is one of the better ones to demonstrate imperative programming in Haskell, say).

Examples that compare and contrast J against other
languages can be found by following the links in
[[J|the J page]] (17 articles so far). [[User:Roger Hui|Roger Hui]] 23:26, 9 December 2007 (MST)
: I know. What I wanted to say is that I think it would be good to find '''simple''' examples that '''bring out''' the features of J. Finding good examples for this is really an art. And comments to explain what this particular implementation does differently than the others don't hurt, either.


A small counterexample to the assertion that number theory
algorithm tell you nothing about the language whatsoever:
I now know that what in J can be done using<tt> *./ </tt>
you can do with<tt> foldl1' lcm </tt> in Haskell. [[User:Roger Hui|Roger Hui]] 23:34, 9 December 2007 (MST)
: Yes, but it would have been much more appropriate to expose this in isolation, on a separate page for just this subject, instead of as part of a more complicated algorithm. Haskell uses often lists instead of arrays, so there are two folds, ''foldl'' from the left and ''foldr'' from the right. These versions also need a "neutral" element (if the list is empty), while the ''foldl1'' and ''foldr1'' variants assume that the list is not empty. Finally, there are the strict variants ''foldl' '' and ''foldl1' '', which are more efficient in some situations. And you cannot learn those things by looking at a more complicated example where one member of this family shows up without any explanation of context, and without explanation why just this version is used there. You can learn that by looking at a simpler example, where the more limited scope of this example allows such explanations.
: BTW, the ''\'' adverb in J orresponds to the family of Haskell ''scan'' functions in a similar way. And those sort of comparisons are really interesting and the reason for this site, but complicated numerical algorithms doesn't make it easy to see.
: And if I head implemented a different algorithm (say, the naive one

 multOrder a m = (+1) $ length $ takeWhile (/= 1) $ iterate (\x -> a*x `mod` m) $ a

: ), you wouldn't have been able to learn anything at all by comparison, unless you can guess already what the Haskell code means. It's exactly '''because''' I used the '''same''' algorithm that you can learn something from it. [[User:Dirkt|Dirkt]] 05:50, 10 December 2007 (MST)

Whatever the merits or flaws of the Bach & Shallit description, it is all that the J program had to 
go on when it was written.  As things now stand in this page, the specifications are now different:  
the original specs by Bach & Shallit, and the specs you have written.  Also, the J and Haskell programs
are commented to different standards.

With disparate languages it can be (and is) difficult to devise simple tasks that bring into focus
the distinct features of the languages.  What you need are both:  simple tasks that hopefully bring
out a single orthogonal feature, and more complicated tasks such as multiplicative order that
tells you what are the areas where it may be fruitful to explore simpler tasks. [[User:Roger Hui|Roger Hui]] 01:01, 11 December 2007 (MST)


###  Java solution 


There is currently a mismatched left paren in the line:
 for(;x.modPow(retVal, y) != BigInteger.ONE;retVal = retVal.add(BigInteger.ONE);

[[User:Roger Hui|Roger Hui]] 21:27, 7 December 2007 (MST)

:You're allowed to fix it too if you know how. No one will be angry. --[[User:Mwn3d|Mwn3d]] 09:43, 8 December 2007 (MST)
