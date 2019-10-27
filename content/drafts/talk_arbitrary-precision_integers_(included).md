+++
title = "Talk:Arbitrary-precision integers (included)"
description = ""
date = 2017-09-10T18:42:08Z
aliases = []
[extra]
id = 6037
[taxonomies]
categories = []
tags = []
+++

==Use of external libraries==
I ''strongly'' recommend allowing the use of external libraries in the case where the language doesn't have inherent bignum support. Otherwise, you would need a more generalized task, and I don't see a point to the example code duplication that would require. --[[User:Short Circuit|Michael Mol]] 08:33, 13 February 2010 (UTC)

:Hi Michael, I am trying to distinguish between the use of a library that is associated with the language versus one that is generic. It is a grey area because, for example, CPAN does ''wrap'' [http://www.gnu.org/software/gmp/manual/html_node/Language-Bindings.html GNU MP] which would make it fine as you would assume that the average Perl user would have no problem installing the CPAN module. But what of ''some other language with a generic interface to C/C++ that needs a generic download of the GNU MP source, compiled, then loaded with a generic inter-language call capability?'' I am trying to ''exclude this latter case'' as we already have an entry for calling other languages and I would like this task to not degenerate into showing how half the languages can link to GNU MP.
:Another grey area might be languages where the ''norm'' is to use libraries from a another language that it is very close to. All the .Net languages might use a standard Bignum implemented in C#; or JVM languages use the standard Java Bignum. In these cases, where the languages regularly and normally share 'standard' libraries, then I think they should go ahead and use it. --[[User:Paddy3118|Paddy3118]] 09:14, 13 February 2010 (UTC)
:I guess I'm trying to make the '... (included)' tasks show more of what comes with very little effort, and is known to work, with the language itself. --[[User:Paddy3118|Paddy3118]] 09:23, 13 February 2010 (UTC)
::So, you're looking for what people can do within the language or with a very common add-on library? (In some languages, it's normal and expected to be using extra libraries for things, and so the core language itself has very little functionality.) –[[User:Dkf|Donal Fellows]] 11:49, 13 February 2010 (UTC)
:: Just because the mechanism behind a task is already shown somewhere else doesn't mean we shouldn't allow the usage of that mechanism to solve a different problem. If we were to generalize that as a rule, most of the tasks on Rosetta Code would be defunct because we have tasks for looping and flow control.  By their very nature, people tend to go with least-effort solutions, and for any given language, the least-effort solution to an expert will be idiomatic for that language. If a language has good support for arbitrary precision, an expert in that language is likely to use the native support. If the language has poor support, the expert in that language will prefer to pull in an outside library. --[[User:Short Circuit|Michael Mol]] 16:38, 13 February 2010 (UTC)
::: Can we get some more chimes in? I haven't forgotten about this; I've just been too busy to come back to it. I really don't care for requiring the use of builtins for a sophisticated problem, as I think a savvy developer in a language would favor builtins over imported functionality, but I'd like to know what other people on here think. (Do we have a name for contributors around here?) --[[User:Short Circuit|Michael Mol]] 14:53, 25 February 2010 (UTC)

I was rather hoping people would demonstrate how to write an arbitrary-precision / bignum library, in their preferred language, from scratch. I've been scribbling ideas on the train / bus to and from work. Um, err, yes ... in VBScript. Of course, it wouldn't necessarily be very efficient in that language, but, once working, it might be better in VB6 ... --[[User:Axtens|Axtens]] 13:31, 13 February 2010 (UTC)
:Hi Axtens, You could write a task to, say, compute the same number using only, say, 16 bit integers, but I think a full-blown Bignum implementation would become too large to compare implementations? I did the easy one ;-)
:--[[User:Paddy3118|Paddy3118]] 16:21, 13 February 2010 (UTC)
: You might find [[Long multiplication]] interesting. Also, I'm almost certain I created a long division task, but I don't see it anywhere. A better way to ask for what you're looking for might be to ask for implementations of particular algorithms. I don't know the names of many relevant algorithms off-hand, though; perhaps some of the CS majors could chime in. --[[User:Short Circuit|Michael Mol]] 16:28, 13 February 2010 (UTC)
: Bignum engines are fairly large. I've just checked the Tcl sources, and our [http://math.libtomcrypt.com/ core bignum engine] is 4600 lines of [[C]] for just basic mathematical operations (division takes a lot it seems). OK, that includes comments but even so, it's all a bit much for RC. Given the general difficulty of writing a bignum implementation that doesn't suck, every language that provides built-in bignums will be using one of the existing libraries for it. –[[User:Dkf|Donal Fellows]] 08:15, 14 February 2010 (UTC)
: Laszlo's [http://www.autohotkey.com/forum/viewtopic.php?t=21650 bignum library] in a combination of 450 lines of ahk and 330 lines of c...  [[User:Tinku99|Tinku99]] 15:49, 4 June 2010 (UTC)tinku99

::If Laszlo would try and get his/her code accepted as <u>the</u> Bignum library for AHK and put in the normal place for AHK's endorsed libraries mentioned with the distribution, then you would be doing the AHK community a favour as well as merely extending Rosetta Code. More power to Laszlo! --[[User:Paddy3118|Paddy3118]] 20:02, 4 June 2010 (UTC)
::: My impression from watching AHK progress on RC is that the AHK forums ''are'' the go-to place for AHK libraries and modules. An unusual approach, to be sure, but that's been my impression. I haven't done specific research on it. --[[User:Short Circuit|Michael Mol]] 20:20, 5 June 2010 (UTC)
::: Yeah, there isn't an official library.  The forum and the wiki are it. Laszlo is the goto person for math related things in autohotkey. I have added a link to it on the [http://www.autohotkey.com/wiki/index.php?title=Script_Listing#Math_.26_Calculators wiki] as well.
::I would suggest then that if Laszlo's library is available at a standard place, then the AHK example could just refer to the library and show its use in solving the problem rather than quote the library on RC, as, for AHK at least, that would then make Laszlo's bignum library a "standard library"?  --[[User:Paddy3118|Paddy3118]] 06:06, 6 June 2010 (UTC)

Late answer... I agree that external libraries should be allowed, and it make little sense to require ''a single, overwhelming, library of varied modules that is endorsed by its home site''. In some cases, the language does not have a single home page, simply because it's an international standard. In Fortran, there are at least 3 well known multiprecision libraries (by [https://maths-people.anu.edu.au/~brent/pub/pub043.html Brent], [http://crd-legacy.lbl.gov/~dhbailey/mpdist/ Bailey] and [http://myweb.lmu.edu/dmsmith/fmlib.html Smith]). Should we dismiss all of them because it is required to have only one ''overwhelming'' library? As Michael Mol commented above several years ago, that means we would need another more general task, and duplicate code. The task looks currently too rigid to me. For the record, I have added a solution using Smith's FM, and I may add solutions with the other two later.

[[User:Arbautjc|Arbautjc]] ([[User talk:Arbautjc|talk]]) 23:58, 11 November 2016 (UTC)

:Ah well. It seems to me that the point of the task is to demonstrate the ''in-built'' facilities for arbitrary-precision integers, with however allowance for the existence of some library extension so routinely available at every installation that everyone would already be using it for such tasks. If the required number size had not been so large, the IBM1620 would have managed, but perhaps modernists with ibmpc-monocuture aren't interested. Evidently, any language can be extended to present such facilities, but the specification explicitly rejects developing such a library. If in Fortran's case there are three such libraries, and (as it appears from the above) different verbiage is required to employ each for the same calculation, we would find ourselves comparing such libraries rather than the in-built features of different languages. [[User:Dinosaur|Dinosaur]] ([[User talk:Dinosaur|talk]]) 10:29, 13 November 2016 (UTC)
::True. But then remove also the C and Ada entries, and probably others as well. Oh, wait, the point of this comment was to show that it would be necessary then to add a task to allow external libraries, since many languages don't have builtin multiprecision. It was already stated by Michael Mol in 2010 above. Now, what's most useful? A long talk about the IBM 1620, a computer from 1959 that has nothing to do with modern Fortran, or a way to do multiprecision computations in Fortran today? I am interested in history, but I am definitely interested first in practical way to do things right now: programming is not primarily about history. But, if really asking to slightly change the task (so that some entries become conformant, and not only Fortran), is too much, then I'll copy-cut to create a new task to allow this. I don't think it's particularly wise though. [[User:Arbautjc|Arbautjc]] ([[User talk:Arbautjc|talk]]) 11:41, 13 November 2016 (UTC)

:::I note the remarks in the Algol68 example about strictly speaking, and so therefore, those entries that involve external features or explicit routines for performing multi-precision should indeed be ejected as not conforming to the task's statement, strictly. Otherwise, we face mere variations of something like <include library>...<eval(5^4^3^2)> with specious differences not elucidating the language facilities. Entries for the likes of Alore and bc amount to that but without the "include" because the necessary facilities are built-in to the language. Only some libraries amount to being a part of the language, as with C's stdio.h. I'm interested in the historical changes in a language as features are introduced or removed and generality is attained or implemented with limits - so no INTEGER*2000 for example, even though in principle this could be allowed, somewhat as in Algol68 - and was amused by the thought that the IBM1620 could perhaps have succeeded, and the IBM1403 also? These represent a possible development path in this matter that was not taken and anyone interested in language comparison would be interested in such decisions. As for what's more useful, someone seeking to solve such a problem "right now" via perusal of RC to select a language for immediate use would want to know which languages had built-in facilities or not. And reading the task description, would expect to see just that, and if for example bc was available, success would be moments away. But with libraries required there would be a delay, unless the library was overwhelmingly available so as to seem a part of the language and even more delay for supplied routines. Thus I am speaking against the inclusion of libraries or specially-written routines, as per the task specification for this specific task.
:::As for organising another task with or without this or that, I leave that to others as already, many flowers bloom and I wander without guide. There does not seem to be a facility for grouping related tasks, or multiple parts of a larger task, or even cross-referencing tasks. Just a flat list of tasks ordered alphabetically and with not always helpful choices of the first word. A tree-structure? But the A.B.etc, B.X.etc ordering won't work when it is not clear which should be the first-level keywords. This is a matter for the design of RC's presentation, which I know little about, and I see a lot of scope for disagreement. [[User:Dinosaur|Dinosaur]] ([[User talk:Dinosaur|talk]]) 07:09, 14 November 2016 (UTC)
::::Just for fun: even languages with builtin multiprecision may have additional libraries, for good reasons. For instance, Python has big integers, but the gmpy2 library is linked to GMP, which is way faster than the standard Python integers (plus it also adds multiprecision floats, which are not in the base Python distribution). It makes very little sense to only allow what is included in the bare language: who programs in C++ without BOOST? Who programs in Fortran without LAPACK? Who programs in Lisp without ASDF or even QuickLisp and a bunch of libraries? Who programs, in any language, a GUI without a GUI library? (rarely included, and even when it is, it's never the only one) It's a basic fact of programming that one should not reinvent the wheol but reuse, hence it would be perfectly correct to link to an external library. That's how it ''should'' be done in real life. But, bitter truth, this is not the present task. I'll write another one, maybe. This is really silly. Feel free to quibble about what Fortran could have been sixty years ago, it's almost as useless as the rest of the task! [[User:Arbautjc|Arbautjc]] ([[User talk:Arbautjc|talk]]) 18:07, 14 November 2016 (UTC)

== Criteria for Non-Draft? ==

What are the criteria for making this page a full task? There are already a number of implementations and the task itself is largely clear too. –[[User:Dkf|Donal Fellows]] 11:51, 13 February 2010 (UTC)
:I'm pretty sure there are no criteria. It's just a consensus. This one seems to still have a little discussion going on about how we want it to work so maybe we should wait to make it a full task. --[[User:Mwn3d|Mwn3d]] 16:45, 13 February 2010 (UTC)
::Whoops. I've changed it already due to the number of examples. Please put it back in draft if you think it needs it though. --[[User:Paddy3118|Paddy3118]] 16:58, 13 February 2010 (UTC)

==“Precision”==

Is the use of “precision” appropriate? To me, “precision” suggests ''fractional'' digits (to the right of the decimal[-or-binary] point). —[[User:Kevin Reid|Kevin Reid]] 12:55, 13 February 2010 (UTC)
:Hi Kevin, I've heard it used colloquially as "the number of digits of precision of an integer", and also shortened to just "the number of digits". I guess, because we are dealing with integers which are usually thought of as without an exponent then the "range" of an integer might be used instead, but the [[wp:Arbitrary-precision arithmetic|wp article]] uses precision when speaking of integers:
::''"In computer science, arbitrary-precision arithmetic is a technique whereby calculations  are performed on numbers whose digits of precision are limited only by the available memory of the host system. This contrasts with the faster fixed-precision arithmetic found in most ALU hardware, which typically offers between 6 and 16 decimal digits. It is also called bignum arithmetic, and sometimes even "infinite-precision arithmetic" (which is a misnomer, since the number of digits is both finite and bounded in practice)."''
: --[[User:Paddy3118|Paddy3118]] 16:54, 13 February 2010 (UTC)

==Ursala took a day?==
On my four year old laptop, the Python version took around a minute to run. Maybe the Ursala version should not have used BCD if another representation is available? --[[User:Paddy3118|Paddy3118]] 19:08, 8 April 2010 (UTC)

== Time constriction? ==
----
Copied from [[User talk:Short Circuit]]:

Sorry, to bother you again. I have written some code for the task "Arbitrary-precision_integers_(included)" in the programming language AutoHotkey. Now my problem here is the following: the code runs on my PC for '''26 hours''' and then spits out the '''correct''' result. I have used an implementation of BCD multiplication by AHK forum user Laszlo (permission has been granted to publish his code) together with my own implementation of "long powers", that held tight when tested with Euler Challenges. I am as sure as I can possibly be, that this code is correct (forum discussion starts with the second post from the top here: [http://www.autohotkey.com/forum/viewtopic.php?t=44657&postdays=0&postorder=asc&start=225 AHK forum], if you want to have a look). Now I think that pride thing you mentioned yesterday has got a grip on me too, so here is my question: Is there a time constriction at all for submissions to RosettaCode? I assure you that this is not some untested, half-baked code that gets thrown at RosettaCode, but it can be improved. [[User:Wolf|Wolf]] 12.May 2010 14:32h (GMT+2)
: No time constriction. --[[User:Short Circuit|Michael Mol]] 13:42, 12 May 2010 (UTC)

:But isn't a condition of that task that it is only solvable by languages that come with their own in-built implementation of arbitrary precision integers? If you are having to code it then it should not be included. --[[User:Paddy3118|Paddy3118]] 14:09, 12 May 2010 (UTC)
: You're correct. And this is a discussion better suited for that task's talk page. --[[User:Short Circuit|Michael Mol]] 14:22, 12 May 2010 (UTC)


== Does the Clojure entry count the digits? ==
--[[User:Paddy3118|Paddy3118]] 06:13, 11 June 2010 (UTC)

==Move timings to the talk pages==
I'm not sure that timings should be included in answers except maybe in general terms, as [http://shootout.alioth.debian.org/ The Computer Language Benchmarks Game] does it in a more rigourous way and yet still points out [http://shootout.alioth.debian.org/dont-jump-to-conclusions.php issues]. --[[User:Paddy3118|Paddy3118]] 00:34, 25 February 2011 (UTC)

: I am not wanting to see twenty timings for twenty different machines using C with GMP. So I removed my timing from that section, because another user posted the first timing. I kept my 5 other timings (bc, C with OpenSSL, dc, Perl, Ruby), but I removed most of my commentary. Instead I make one list, here on the talk page. --[[User:Kernigh|Kernigh]] 03:26, 27 February 2011 (UTC)

::Thanks Kernigh. I know I prefer it this way rather than having to out-do TCLBG. What do others think? --[[User:Paddy3118|Paddy3118]] 04:05, 27 February 2011 (UTC)


###  Timings with AMD Phenom II X2 560 

# C with GMP: 0m0.04s
# Ruby 1.9.2: 0m0.52s
# C with OpenSSL: 0m1.30s
# Ruby 1.8.6: 0m8.59s
# [[OpenBSD bc]] or [[OpenBSD dc|dc]]: 0m24.81s
# Perl 5.10.1: 1m4.28s

--[[User:Kernigh|Kernigh]] 03:26, 27 February 2011 (UTC)

== Missing output ==

[[User:Paddy3118|Paddy3118]] added <nowiki>{{incorrect|Golfscript|Output not shown.}}</nowiki> to GolfScript and several other languages. I ran the GolfScript program, and it output the correct digits, so I removed <nowiki>{{incorrect|Golfscript|Output not shown.}}</nowiki> from GolfScript without changing the code.

I did not add the output to the page. The output would be redundant, because the other solutions make the exact same digits. Several other solutions have no output on the page. The task is not specific about output. Some solutions output the first 20 digits. Some other solutions never output the first 20 digits, but do check them. It is enough if the user who runs the program knows that the first 20 digits are correct.

There should not be tag like <nowiki>{{incorrect|Golfscript|Output not shown.}}</nowiki> unless the code is missing a print command. --[[User:Kernigh|Kernigh]] 01:40, 9 May 2011 (UTC)


:Hi Kernigh, the task is written that the output is to be shown. People mis-interpret the task and write ''some'' entries without output. That does not mean that all output should not be shown from then on. I try and keep entries on track by reminding people of the need for output to be shown. 
:I do note that it is very rare for people to misunderstand a task and insist on doing ''more'' rather than less. By arguments such as yours a task could get steadily worse over time as people leave out one or more aspects over time and others look to implement the bare minimum.
:In this case, could you not remove the need for output to be shown until someone actually shows the output from the programs? Thanks. --[[User:Paddy3118|Paddy3118]] 05:26, 9 May 2011 (UTC)

:Strange? You added output for Scala, but just removed the templates for Factor and GolfScript. --[[User:Paddy3118|Paddy3118]] 05:30, 9 May 2011 (UTC)

We are confusing two different ideas:

# Show the output to the user running the program.
# Post the output to this wiki page.

Users can post the output to this wiki page, but that is no task requirement. A language that shows the output, but does not post to this wiki page, is not wrong. For example, the [[Loops/Foreach]] task says to "print each element", but few languages post the output to that wiki page. --[[User:Kernigh|Kernigh]] 22:17, 15 May 2011 (UTC)

: Seems fair to me. To be exact, so long as the right output is produced, it's not required here. Especially if there is model output that can be pointed at. –[[User:Dkf|Donal Fellows]] 00:03, 16 May 2011 (UTC)


: Unfotunately, that interpretation is wrong. The only reason for:
:# Duplication/emphasis of print-like keywords e.g. "print and show" or "generate and show" in this case it is "find and show".
:# Attaching an <nowiki>{{output?|language|reason>}}</nowiki> box on examples that don't.
:# Having the task author repeatedly state in the task talk page that output should appear.
:# Having edits summaries for revisions mentioning the lack of output.
:# The majority of other examples having output shown.
: ... Is to guide fellow contributors to the conclusion that the giving of output is a necessary part of the task description.

: Note also that in case of personal doubt, you are less likely to need to revise your submission if you include the output. --[[User:Paddy3118|Paddy3118]] 05:15, 16 May 2011 (UTC)


==Undertested cosmetic edits at 05:58, 24 July 2016 hid formula from most browsers==

Cosmetic edits made to the task page at 05:58, 24 July 2016 , including the injection of spaces around expressions in &lt;math&gt; tags, left the main expression completely invisible to all browsers which display the graphic file version of formulae rather than processing the MathML (this is, in fact, the majority of browsers). The MediaWiki processor does not currently expect such spaces, and generates syntactically ill-formed HTML if they are introduced. Other aspects of these cosmetic edits may further compound the problem. [[User:Hout|Hout]] ([[User talk:Hout|talk]]) 08:48, 21 September 2016 (UTC)
: Repaired [[User:Hout|Hout]] ([[User talk:Hout|talk]]) 00:29, 3 November 2016 (UTC)

==failure of ooRexx using Classic REXX example==
(Moved here from the epilogue (after the '''output''') of the Classic REXX example.)   -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 18:31, 10 September 2017 (UTC)


```txt

ooRexx gives me 

      5 *-* n=5** (4** (3** 2))  /*calc. multiple exponentations. */
 Error 26 running Z:\huge.rex line 5:  Invalid whole number
 Error 26.8:  Operand to the right of the power operator (**) must be a whole number; found "2.6214E+5" 
Other Rexx-es don't??

```


No answer yet. numeric digits 6 at the start fixes the problem for ooRexx --[[User:Walterpachl|Walterpachl]] ([[User talk:Walterpachl|talk]]) 18:47, 5 November 2013 (UTC)

: Yes, Regina and R4 are both very lenient (liberal?) in their usage of exponents.
: Also, the (internal) subroutines of these REXXes may use a   '''numeric digits 9''', 
: thereby bypassing the use/limit of '''numeric digits 5'''   (as in the older/replaced 2<sup>nd</sup> version).   -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 18:31, 10 September 2017 (UTC)
