+++
title = "Talk:Kaprekar numbers"
description = ""
date = 2012-10-27T12:10:47Z
aliases = []
[extra]
id = 9893
[taxonomies]
categories = []
tags = []
+++

==We've been linked to from a prestigious source==
[http://webcache.googleusercontent.com/search?q=cache:6-8YE2tEQKwJ:oeis.org/A006886+site:https://oeis.org/A006886+%22Rosetta%2BCode%22&cd=1&hl=en&ct=clnk&gl=uk The On-Line Encyclopedia of Integer Sequences] has a link to this RC page! (The link is to the google cache - for the highlight) --[[User:Paddy3118|Paddy3118]] 22:24, 29 August 2011 (UTC)

And on this entry [https://oeis.org/A194218 A194218] we get mentioned a second time as a place to compare programs! This is just brilliant to me as I have admired OEIS for what it collates for some time. --[[User:Paddy3118|Paddy3118]] 06:11, 30 August 2011 (UTC)
: Sweet. --[[User:Short Circuit|Michael Mol]] 13:15, 30 August 2011 (UTC)

==Java count missing==
Just add (and show), the count of how many there are <1million and you will have completed the stretch goal! --[[User:Paddy3118|Paddy3118]] 14:07, 7 June 2011 (UTC)
:I had actually added it as a test because I saw the other examples do it, but for some reason I didn't read it as a requirement so I took it out. Mornings are hard. --[[User:Mwn3d|Mwn3d]] 14:47, 7 June 2011 (UTC)
:: :-)
--[[User:Paddy3118|Paddy3118]] 17:29, 7 June 2011 (UTC)

== Why the complexity? ==

The wikipedia page says "Let X be a non-negative integer. X is a Kaprekar number for base b if there exist non-negative integers n, A, and positive number B satisfying ...".  In other words A can be zero.  Why do we have to have a bunch of text claiming that A cannot be zero but A can be an empty string and that it is meaningful to add an empty string to a number?  (Can't we just take advantage of the fact that leading zero digits do not change the value of a number?)  --[[User:Rdm|Rdm]] 20:56, 7 June 2011 (UTC)
:To me the text "However a conceptual single split at the very end or before the first digit that produces one empty string does have the empty string counted" means that A can be zero. Unless I'm confused about what A and B are. --[[User:Mwn3d|Mwn3d]] 22:30, 7 June 2011 (UTC)

Consider 100*100 = 10000 which could be split as 100 + 00. Now the 00 is ''dis''allowed.
Compare that with 1*1 = 1 which ''is'' a K-number. For this to be a k-number then it must be expressed as either 1 plus no digists to the right or no digits to the left + 1. Either way no digits is treated as a positive integer of value zero but any string of one or more noughts is expressly forbidden.

Having 1 in the series seemed to add that complexity that needed explaining in my mind. --[[User:Paddy3118|Paddy3118]] 23:14, 7 June 2011 (UTC)
: It has to do with the way that the splitting is done mathematically, i.e., <math>\mathit{N} = A + B</math> and <math>N^2 = A\times 10^x + B</math> where <math>10^x > B > 0</math>; in the case of <math>\mathrm{1}</math>, it becomes possible to use <math>\mathit{A} = 0</math>. This only works for that value; nothing larger is equal to itself when squared and zero is inadmissible by the constraint on <math>\mathit{B}</math>. Trust a mathematician to fluff the operation of splitting a number-string and introduce an unwanted special case! –[[User:Dkf|Donal Fellows]] 23:48, 7 June 2011 (UTC)

::I was reading the expressions, but the implication just fazed me. I had a mental block about the 1 case, even when it was explained more explicitely. Gosh, I'm not getting senile? er, I'll change that ? to ! --[[User:Paddy3118|Paddy3118]] 09:05, 8 June 2011 (UTC)


###  Which sounds better? 


What should we say, "split into parts" or "split once into whole number components made of groups of neighbouring digits from within the number"?

We are splitting into the number into numeric values the belong to the set of "whole numbers" aren't we? Or is there a misunderstanding here?

"split into parts" sounds too vague to me.

--[[User:Markhobley|Markhobley]] 16:06, 8 June 2011 (UTC)
:If I had to change it from what we have now I'd say "split into two parts". The way it is now seems fine to me especially with the examples in the description. Since the number is specified as an integer, I don't think we need to add "whole number" noise. --[[User:Mwn3d|Mwn3d]] 16:16, 8 June 2011 (UTC)

::Right. The "whole numbers" verbage avoids the issue of explaining that chains of zeros do not fulfil the criteria, (although we would probably still need to keep that as a note to remind the task implementers.) --[[User:Markhobley|Markhobley]] 16:22, 8 June 2011 (UTC)
:::"Whole numbers", to me, includes 0 so I don't think that works that way. Is "whole numbers" an official mathematical set like "natural numbers"? Maybe say "split in to two positive parts"? --[[User:Mwn3d|Mwn3d]] 16:33, 8 June 2011 (UTC)

::::There isn't a consensus on the definition of a whole number, natural numbers, or counting number (especially in grammer/grade/highschool/secondary school texts). See the links for "Mathworld" http://mathworld.wolfram.com/WholeNumber.html, http://mathworld.wolfram.com/NaturalNumber.html, http://mathworld.wolfram.com/CountingNumber.html, all those seem to prefer defintions based on integers (positive integers, nonnegative integers, negative intergers, ∙∙∙ --[[User:Gerard Schildberger|Gerard Schildberger]] 19:26, 21 March 2012 (UTC)

:::Generally zero is not considered to be a whole number, but there are people who would disagree. We could use the term "integers greater than or equal to one" --[[User:Markhobley|Markhobley]] 19:40, 8 June 2011 (UTC)

:::FWIW zero is positive, but not a whole number, so "positive parts" would be no clearer IMHO, I would also call them numerical components, rather than parts, because 12 can be split into parts of sizes 8 and 4, but 1 and 2 are its numerical components. --[[User:Markhobley|Markhobley]] 19:45, 8 June 2011 (UTC)
::::OK 0 cannot be positive because then no one would need to call anything "non-negative" ([http://www.positiveintegers.org/] [http://mathworld.wolfram.com/PositiveInteger.html] [http://simple.wikipedia.org/wiki/Positive_number]). The phrase "string representation" earlier in the first sentence avoids the "split" definition you're thinking of. And once again, the examples following the language reinforce the wording. --[[User:Mwn3d|Mwn3d]] 19:58, 8 June 2011 (UTC)

==Promoting to full task status==
As the resolution of any of the discussions above in unlikely to change the fundamental task  goals, and we have several correct examples.--[[User:Paddy3118|Paddy3118]] 20:07, 8 June 2011 (UTC)

: I say yes. Go! --[[User:Markhobley|Markhobley]] 20:11, 8 June 2011 (UTC)

== C++ sample code ==

I'm tempted to change all the "long" type to "long long" in the C++ sample code, because it would fail on 32 bit system as is. --[[User:Ledrug|Ledrug]] 17:08, 14 June 2011 (UTC)
:That change seems fine since the output goes up pretty high anyway. Does that just mean they used a 64-bit machine? --[[User:Mwn3d|Mwn3d]] 17:17, 14 June 2011 (UTC)
::What 'long' means depends on the compiler, but it's typically defined to be the longest native integer type, while 'long long' is (I think) defined to be 64-bit.  Someone correct me if I'm wrong. I'll change the code anyhow, because it overflows with 32 bit long for sure. --[[User:Ledrug|Ledrug]] 17:25, 14 June 2011 (UTC)
::: On every C and C++ compiler I've used on a 64-bit OS (so, GNU on Linux and MSVC/MSVC++ on Windows), 'long' is 32-bits. 'long long' is 64-bits. I don't know about, e.g. SPARC64, though. That's why you get things like uint32_t from stdint.h or DWORD from Windows.h. --[[User:Short Circuit|Michael Mol]] 18:05, 14 June 2011 (UTC)
:::: It's always sort of confusing to me anyway.  For reference, see http://www.unix.org/whitepapers/64bit.html, which also has a table for integer types. It's true that if you worry about the exact length, use exactly defined types instead of the compiler native ones. --[[User:Ledrug|Ledrug]] 18:28, 14 June 2011 (UTC)
::::: If 53 bits is enough (and I think it is, here), you might also consider using double. --[[User:Rdm|Rdm]] 20:01, 14 June 2011 (UTC)
:::::: Heh, tried double, seg faulted.  I'm not very motivated to see what's happening--probably that atol()--so I'll just leave it as is. --[[User:Ledrug|Ledrug]] 21:44, 14 June 2011 (UTC)

What's up with the people modifying the C++ code, previously (before long to long long) it returned wrong numbers, now it simply never returns on 32 bit system with g++ 4.5.  Also maybe we should promote testing sample code with full warnings on: g++ gives signedness mismatch warning on line 20. --[[User:Ledrug|Ledrug]] 23:02, 15 June 2011 (UTC)

Would someone modify the code to work for a happy medium and then state on the task page that the C++ code is fragile w.r.t 32/64 bit systems and where the fragility lies? It sounds like it is an issue that should be taken into account when comparing that particular C++ code with other languages.

Come to think of it, maybe other similar languages might state why they don't have an issue? Or maybe there is a separate task here: "Do something that is otherwise straight-forward but needs slightly different code, or will not work on both 32 and 64 bit X86 systems and/or up-to-date, widely used, but different versions of compilers/interpreters"? --[[User:Paddy3118|Paddy3118]] 05:06, 16 June 2011 (UTC)

I really don't think it's a language or compiler problem, though.  And this task is already pretty straightforward: you take a number and look at its digits, that's it, the language certainly can handle that. --[[User:Ledrug|Ledrug]] 08:54, 16 June 2011 (UTC)

== Indentation... ==

Can we please don't adjust someone else's code indentation just because it's different from yours... --[[User:Ledrug|Ledrug]] 20:21, 14 June 2011 (UTC)
: I appreciate that it can be annoying when people touch ''your'' code, but it's a wiki. You're going to have to get used to it. My code is changed regularly. Sometimes for the better. Sometimes it's simply changed but not really improved, in my opinion. Unless you want to get involved in edit wars all the time, you're going to have to relax about it. In your case, I honestly thought that the huge indent was accidental.  Otherwise I would have left it alone. [[User:Fwend|Fwend]] 21:23, 14 June 2011 (UTC)
:: K&R 8 space tab is typical BSD style, I'm not sure why you'd think it's accidental.  And I didn't mind someone changine my <i>code</i>, but changing purely because of the <i>code indentation</i> is prone to start an edit war, please don't get too used to it (and I didn't revert your edit, did I?). --[[User:Ledrug|Ledrug]] 21:32, 14 June 2011 (UTC)
::You might try leaving the original indentation if it is consistent and "reasonable"; and adjusting to the existing style when making small edits, (helps with the diffs). If you make a large change then probably relax the rules about changing styles? --[[User:Paddy3118|Paddy3118]] 04:21, 15 June 2011 (UTC)

All that being said, there's a balance to be struck here between demonstrating that an idiosyncratic style is acceptable vs demonstrating "normal" code for the culture in question.  And in this case, it's true that Perl culture is quite accepting of idiosyncracies, but also that the culture as a whole tends toward 4-space indent, not 8.  In fact, 4-space indent is specifically recommended in the Camel book.  So a good case can be made for either, and people should not get too hung up about either idiosyncratic tabbing or having that idiosyncratic tabbing normalized to something closer to the norm.  Certainly for the Perl 6 examples I reserve the right to be a bit pickier about setting the cultural norm, since many new people will be learning Perl 6 by studying these examples, whereas Perl 5 already has large collections of existing code outside of RC.  [[User:TimToady|TimToady]] 06:03, 16 June 2011 (UTC)--
: Oh dear, didn't expect this to get the attention of Larry.  Let me clarify: it's not because someone changed <i>my</i> indentation; rather since people tend to have very strong opinions about what a tab size is, changing <i>anybody</i>'s code just to make the indenting fit one's own taste may start flame wars (well, sort of like what we have now), and people should refrain from doing that IMO.  --[[User:Ledrug|Ledrug]] 07:07, 16 June 2011 (UTC)
:: This isn't really a flamewar, just a back-and-forth defense and clarification of different positions. It's actually pretty normal and healthy for RC. It's when the participants stop arguing in good faith (which I don't see anyone here having done) that it gets bad. --[[User:Short Circuit|Michael Mol]] 15:53, 16 June 2011 (UTC)
==Old D version==
(Regarding the changes to the second D version, that Fwend has reverted: the current second D version returns 0/1 but the function has to return a bool, and D is not C. The C-style code was faster, and it was shorter too. Pre-conditions are a good habit in D, that makes the code safer and less wild-west-style as C code. This C-derived code is also able to accept a bigger range of input n values. And the code has a main too, so it's not just a nonworking fragment.) --Unsigned, added by [[Special:Contributions/95.235.203.130|95.235.203.130]]
: An answer to a question: it's not n*n that causes the first overflow, it's tens*=10. I think assert(n<=3_162_277_660UL) is correct.
::Ah ok, I think I asked that question about overflow.  Truth be told, I was only guessing, since I don't even know what a "ulong" is in D. --[[User:Ledrug|Ledrug]] 17:10, 16 June 2011 (UTC)
: I think my code should stand, because it shows a different approach to solving the problem, which makes the task page more interesting. [[User:Fwend|Fwend]] 20:33, 16 June 2011 (UTC)
::Multiple approaches are allowed (and encouraged). If the examples do things differently, put them both up and notate them. --[[User:Mwn3d|Mwn3d]] 20:41, 16 June 2011 (UTC)
: The C version is basically the same as the second and third D versions, slight difference is that it breaks from testing if the first half of nn is already larger than n, so only needs to do half the math for most cases.  The goofy looking comparison itself is just coincidence after looking at the quotient/remainder equation. --[[User:Ledrug|Ledrug]] 21:30, 16 June 2011 (UTC)
:: O, hey that gives me an idea: if (b > n) break; :) [[User:Fwend|Fwend]] 22:32, 16 June 2011 (UTC)

== Extra extra credit? ==

I just had an idea, maybe make an optional task for code to allow bases other than 10--would that be fun? --[[User:Ledrug|Ledrug]] 22:55, 16 June 2011 (UTC)
:Sounds good. Go ahead and add it to the extra credit requirements. I think a reasonable amount of languages should be able to do it easily. --[[User:Mwn3d|Mwn3d]] 00:02, 17 June 2011 (UTC)
::Added.  Now thinking of it, it may be a little hard for most examples using string based methods. Oh well. --[[User:Ledrug|Ledrug]] 00:46, 17 June 2011 (UTC)
:::I think I already have it for Java. Here's my full base 17 list (I have 24):

```txt
1	1	1	0 + 1
16	g	f1	f + 1
64	3d	e2g	e + 2g
225	d4	a52g	a5 + 2g
288	gg	gf01	gf + 01
1536	556	1b43b2	1b4 + 3b2
3377	bbb	8093b2	809 + 3b2
4912	ggg	ggf001	ggf + 001
7425	18bd	24e166g	24e + 166g
9280	1f1f	39b1b94	39b + 1b94
16705	36db	b992c42	b99 + 2c42
20736	43cd	10de32fg	10de + 32fg
30016	61eb	23593f92	2359 + 3f92
36801	785d	351e433g	351e + 433g
37440	7a96	37144382	3714 + 4382
46081	967b	52g94382	52g9 + 4382
46720	98b4	5575433g	5575 + 433g
53505	af26	6ga43f92	6ga4 + 3f92
62785	cd44	9a5532fg	9a55 + 32fg
66816	da36	aeg42c42	aeg4 + 2c42
74241	f1f2	d75f1b94	d75f + 1b94
76096	f854	e1f5166g	e1f5 + 166g
83520	gggg	gggf0001	gggf + 0001
266224	33334	a2c52a07g	a2c5 + 2a07g
```

:::--[[User:Mwn3d|Mwn3d]] 01:23, 17 June 2011 (UTC)
::::Great, that looks identical to mine (except I cheated and didn't split 1, because it's not really the same as the rest). --[[User:Ledrug|Ledrug]] 01:42, 17 June 2011 (UTC)
:::::Note that many details of that line format were listed as "for example".  Now that we have examples, perhaps that sentence should be removed as superfluous?  Or does someone want to promote them to extra credit work?  --[[User:Rdm|Rdm]] 11:15, 27 June 2011 (UTC)

==Incomplete output==
The task specifically asks that output be shown for n<10000 so when shortening output to fit the page, please ensure that these are still all shown, thanks. --[[User:Paddy3118|Paddy3118]] 07:28, 22 June 2011 (UTC)

== In base B <> 10 ==

Just an idea : why bother printing in base B ? Being Kaprekar in base B is unrelated to the printing base. Then, when implementing with a loop doing divisions/multiplications by 10, just replace 10 by B to get Kaprekar numbers in base B.
[[User:Toucan|Toucan]] 17:18, 26 June 2011 (UTC)
:I don't quite understand that. Can you show an example? --[[User:Mwn3d|Mwn3d]] 17:29, 26 June 2011 (UTC)
::Let's take an example from the implementation in C:
::74241 is a Kaprekar number in base 17. But, written in base 17, it's f1f2, or if you prefer f1f2_17 = 74241_10, with obvious notation.
::The fact that it's a Kaprekar number will be better showed when printing in base 17 (numbers with _17 appended), and you will write
::   f1f2_17^2 = d75f1b94_ 17 and d75f_17 + 1b94_17 = f1f2_17
::However, the ''number'' is still f1f2_17 = 74241_10 = 12201_16 = ... You can print this number in any base you wish, it's the same number.
::As Ledrug says, the only reason to print f1f2 is showing how N^2 may be cut to prove it's a Kaprekar number. But it's not really related to 
::Kaprekar numbers, it's a "printing in base B" subtask.
::[[User:Toucan|Toucan]] 11:09, 27 June 2011 (UTC)
:True. I added the non-base10 extra requirement, and the printing part is there to clearly show if a solution decoupled the numerical aspect from a number's string representation.  The base 17 was specifically chosen because languages often can handle base conversion up to 16 natively, so 17 may require a hand-rolled string conversion, which can also be interesting while not difficult. Plus, if you print it out in base 17, it's much easier for a human reader to see the correctness of the solution.  --[[User:Ledrug|Ledrug]] 21:25, 26 June 2011 (UTC)

== Just for fun ==

For fun I tried reversing the digits in the squared number (e.g. using "5203" as the "squared number" for 55 instead of "3025") to see if there would be any pattern in the new results. There were some overlaps for numbers that were all repeating digits. I didn't see anything notable. I got 17 rakerpak (kaprekar backwards....get it?) each for base 10 and 17. The code isn't notable either (just add a bit to one line to reverse the string representation of the squared number). I thought it would be kinda neat to think about. --[[User:Mwn3d|Mwn3d]] 15:16, 8 August 2011 (UTC)

----
I tried this for ooRexx
 Had to change # to n
 But get a syntax error for j=100000 since s=1E+10
 It works with Numeric Digits 14
--[[User:Walterpachl|Walterpachl]] 14:53, 29 June 2012 (UTC)
