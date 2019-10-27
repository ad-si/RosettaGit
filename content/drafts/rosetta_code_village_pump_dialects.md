+++
title = "Rosetta Code:Village Pump/Dialects"
description = ""
date = 2012-08-15T22:08:13Z
aliases = []
[extra]
id = 11313
[taxonomies]
categories = []
tags = []
+++

{{Vptopic
|topic=Dialects
|summary=What are, or should be, the Rosetta Code conventions for dialects of programming languages - implementations on specific platforms that may have minor differences of syntax and functionality from the official distributions?
}}
I would rather have different dialects not count as language of their own. Otherwise we have many main languages, which are similar in many ways, and many more items to show the code for. The navigation gets cluttered up and language lists would need to be structured in a hierarchy. This is already a problem with ZX80 Basic and C64 Basic and maybe more variants. 

I propose
* have an entry page for the dialect which redirects to the main language
* allow different, dialect specific implementations of the same problem on the page of the "main" language.

[[User:Codecop|Codecop]] 13:33, 28 January 2012 (UTC)
: My general position has been to leave the distinction to the language community. To a C++ programmer, all BASIC implementations might look alike, but to people with production code in QuickBasic, that's a nonsensical position. --[[User:Short Circuit|Michael Mol]] 14:44, 28 January 2012 (UTC)
:: I tend to agree, it will be very specific to the language and community.  Guidance and suggestions are probably welcome and this would be a good anchor page, but I would not want to weigh in on any of the languages mentioned above.  --[[User:Dgamey|Dgamey]] 14:51, 28 January 2012 (UTC)

::: One man's dialect is another's version.  To a beginner reading these pages, it gets confusing enough to have to know ''a priori'' what the differences/defaults/nuances are and what can or can't be used in their version/release/sub-release/dialect/whatever.

: I'm going to read into the proposal that navigation is really the issue and not pure numbers.   If we worry about pure numbers, we have about 600 tasks and 450 languages which results in a pretty big matrix.  Not that we will ever see that as other than a sparse-ish matrix.  When that happens we will need to solve that problem.  Until then I have to fall back onto YAGNI. I suggest we put some practical examples and suggestions/ideas here.  Then when a community starts to have a problem they have a place to come for ideas. --[[User:Dgamey|Dgamey]] 15:03, 28 January 2012 (UTC)

:::: YAGNI:  "You ain't gonna need it" --- or --- "You aren't gonna need it". [[wp:You ain't gonna need it|{Wikipedia link: YAGNI}]] -- [[User:Gerard Schildberger|Gerard Schildberger]] 05:58, 10 July 2012 (UTC)

==Examples==

###  Icon and Unicon 

Hi, I did a lot of the work on [[Icon]] and [[Unicon]] which are closely related.  Much of what I went through is discussed on the talk pages for [[Category_talk:Unicon]]. What you are proposing isn't far from what I did.  Here's a summary, you can also see how it looks (there's over 450 examples on RC):
* Each distinct variant gets it's own page/category.  If there are minor variants, it may make sense to put them in a section of the major variant's page.
* In tasks, I have one header with both major variant names in a single header line. Both Icon/Unicon were used because of recognition and to prevent orphaned headings (This may become more problematic as there is also a [[Jcon]] and [[ObjectIcon]]).  Orphaned headings and duplicate efforts is a big problem if you have more than a few contributors.  I found that having a combined header that sorts early worked best.
:: At one point I had thought of disambiguation pages as a possible part of the solution but I concluded I didn't really need them and they didn't add much in this case.  That may not be true with a large number of true variants and also like named but unrelated languages (think Basic and VisualBasic). 
* Under the task heading there is a structure that allows different solutions where there are differences.  However, where the differences are very minor I usually rely upon description text.
I tried a couple of kicks at it and found this approach practical (not necessarily perfect).  It may or may not work with others. Hope this helps.  

--[[User:Dgamey|Dgamey]] 14:42, 28 January 2012 (UTC)


###  UNIX Shell 

Today:
* '''[[:Category:UNIX Shell]]''' counts as one language and contains all Bourne-compatible dialects, including [[Almquist Shell]], [[Bash]], [[Bourne Shell]], [[Korn Shell]] and [[Z Shell]].
* ''[[:Category:Bash]]'' and ''[[:Category:Korn Shell]]'' redirect to UNIX Shell.
* ''[[:Category:Zsh]]'' (Z Shell) is not empty. When it becomes empty, it can redirect to UNIX Shell.
* '''[[:Category:C Shell]]''' and '''[[:Category:es]]''' (Extensible Shell) count as two more languages, because these shell dialects are too different. For task pages, these languages appear as subsections of UNIX Shell.
** So [[Loops/Infinite#UNIX Shell]] lists C Shell and es under UNIX Shell.
* [[Sieve of Eratosthenes#Bash]] is a dummy section that links to [[Sieve of Eratosthenes#UNIX Shell]]. This stops editors who want to write a Sieve of Eratosthenes in Bash.

--[[User:Kernigh|Kernigh]] 16:12, 28 January 2012 (UTC)

=== REXX, ooRexx, and others ===

Nowadays there are many implementations of Rexx.

As 'my' two Rexxes are currently TSO-Rexx (still Classic Rexx to me)
and ooRexx I shall enumerate the incompatibilities between these two.

Rexx was created some 30 years ago for VM/CMS and was later ported
to MVS/TSO. Its current grandchild for Windows and other PC-platforms
is ooRexx.

Some 'features' of the original Rexx were removed '''in this grandchild (ooRexx)'''
:- Use of @#$¢ as or in symbols
:- a= as a short form or a=""
:- Multi-line strings (extending a string over line boundaries)
:- Bifs: externals, find, index, justify, linesize
:- the Upper instruction
:- /= and /== as alternatives to \= or \== (see characters below)

Stream I/O is only optional on TSO (not on 'mine')
(I have to use EXECIO)

Other features were introduced '''with the ANSII standard''' created
by a committee comprising Rexx implementers from IBM (notably the
original author, Mike Cowlishaw) and from elsewhere:

:- Date and Time conversion
:- Parse Caseless
:- Address With (not available in ooRexx)
:- Bifs: changestr, countstr, qualify

A feature introduced with ooRexx (and Regina unless you opt out, see below) 

:- -- as line comment (instead of /* ... */)

Avoiding all of the language elements mentioned above makes a program
portable across all Rexxes (including the two I am using and therefore interested in).
Using additional features introduced with other implementations
(notably directives in ooRexx) will also prohibit program porting.

Characters:
:Originally ¬ was defined as not-character and /= was accepted for not equal.
:This was later changed to the backslash on PCs and ^ on the host.

:On my German TSO I cannot use | and \ due to code page diff:erences.
:My Rexx sources on TSO use therefore ! and ^, respectively.
:(I avoid the ^ by using lt-gt instead of ^=)

::For a Brief History of Rexx see
:: http://www.speleotrove.com/rexxhist/rexxhistory.html
::For a rather complete list of Rexx implementations see
:: http://www.speleotrove.com/rexxhist/rexxplat.html
::(both courtesy Mike Cowlishaw)
 --[[User:Walterpachl|Walterpachl]] 20:23, 8 July 2012 (UTC)

          Regina 3.4 introduced the      single-line comment  feature, 
          Regina 3.5 wasn't configurable concerning single-line comments, 
                         and it wasn't until
          Regina 3.6 that allowed the  noSingle_line_comments  
                         environmental variable. --- Gerard Schildberger

===REXX and o-o REXXes===

(This section was being edited at exactly the same time as the previous section.)

Classic REXX and the object-oriented REXXes have a similiar problem (with dialects, language and concept extensions, re-definitions, incompatibilities, dropped features, added features, etc) in that ooRexx, NetRexx, ROO! (and others) are somewhat of a continuation of classic REXXes (at least, those features that are still approved), with object-oriented features added, and the object-oriented REXXes aren't fully compatiable the classic REXX language.  A list of incompatabilities between classic REXX and the object-oriented REXXes is apparently being developed, as some of the differences are quite subtle.   

Up to very recently, people have been entering NetRexx and ooRexx solutions under '''NetRexx''' and '''ooREXX''' instead of '''REXX'''.  Some of the REXX examples have been modified (and modified again) so that they can apparently execute under a particular ooRexx (but not necessarily NetRexx or others).  Not everyone who enters (and tests) classic REXX has an ooRexx or NetRexx interpreter available to verify if the REXX program(s) will work on the various o-o REXXes.  Putting them all under the REXX language umbrella would necessitate someone going back and entering disclaimers to virtually all of the classic REXX entries, stating that it works for classic REXX, but not necessarily ooRexx and/or NetRexx and/or ROO (since there may be a lack of test platforms), so there may not be a statement which o-o REXXes it works or not works on.  This would make the REXX entries pretty bulky and make perusing unwieldly and harder to follow visually.  I don't feel that moving all the ooREXX and NetRexx examples back to classic REXX would serve any useful purpose, nor entering ooRexx examples in the classic REXX section (which just clutters up entries for classic REXX), and since ooRexx has it's own section, it would behoove everyone to keep ooRexx and NetRexx examples in the ooRexx and NetRexx language section, respectively.  Entering object-orientated REXX language programs in non object-orientated (classic) REXX language section would be like entering C# and other o-o C's under the '''C''' language.  Each lanuage has their own syntax.  I certainly don't know where the line is drawn between dialects and specific implementation differences.  I know you can change Fortran programs enough so they run under REXX, but I can't see the use of that portability.

* 387 (classic) REXX examples (with one cross-posting)
* 120 NetRexx examples  (with no classic REXX postings)
* 120 ooRexx examples   (with no classic REXX postings)

Since the above mentioned ''cross-entering'' has now occurred (with no apparent corrective possibility) and that this is continuing to happen, I would hope there would be a consensus before the pollution gets to far to be corrected easily. As an aside, I have no desire to change program syntax or variables for portability to a language that is of no interest to me, I'm trying to write the classic REXX code to ensure that it works for all classic REXX interpreters (that I can test) and I'm not interested in object-oriented languages.  For classic REXX programs, it's like using a hammer to kill a fly.  That is why there exists two other o-o REXX language sections (NetRexx and ooRexx). I haven't seen any effort in making ooRexx compatible to class REXX (and there shouldn't be, of course; that portability wouldn't be useful, I should think). -- [[User:Gerard Schildberger|Gerard Schildberger]] 21:01, 8 July 2012 (UTC)

My suggestion was to have Classic, or let's say plain Rexx programs under Rexx
Object oriented Rexx programs under ooRexx (I hav eno idea how Roo fits with tah
and NetRexx, to me is an entirely different story.
I remeber having seen entries in ooRexx that said something like: the Rexx program works also under ooRexx
Would you want to have either that sentence for all Rexx programs that work unchanged and
the changed version (changing $# and a= as necessary) in ooRexx?
I shall proceed in that direction.
--[[User:Walterpachl|Walterpachl]] 21:13, 8 July 2012 (UTC)

-----

Classic REXX is not object-orientated REXX.  To put them under an o-o umbrella would be a disservice to both languages.  A classic REXX entered under the ooRexx language would (to me) imply that it would work using ooRexx, and I have no way of testing/verifying that.  My main interest is in classic REXX and have no need for object-oriented features.  Again: hammer, fly.  I test all of my classic REXX program examples (unless noted otherwise) under three classic REXXes: Personal/REXX, Regina (sometimes more than one version when there is a perculiar release-based feature), and R4.  [Sometimes I say PC/REXX when I meant to say Personal REXX --- they have the same pedigree.]

I don't know what you mean by ''plain'' REXX; the point under discussion is classic REXX vs. the o-o (object-orientated) REXXes. 

And yes, some ooRexx examples have that wording: " ... the entry under REXX works with oo-REXX ..." (or NetRexx).  There's nothing wrong with that, and it shows for that particular classic REXX program, it solves the task using that language.  As for asking what I want, I have no interest in the o-o REXXes. I don't care if you want to enter a comment under ooRexx that it works with the program entry under REXX or not (but I think that would be a good idea).  I you want to change a copy of a classic REXX program and then move that changed classic REXX program to the ooRexx language section, I think that would also be a great idea and an endeaver worth the time and effort. -- [[User:Gerard Schildberger|Gerard Schildberger]] 22:16, 8 July 2012 (UTC)  

----
This is one of the cross postings that you deny to exist. (24 game)

: Walter, I don't deny any exist.  I emphatically stated that they DO exist ("... has now occurred ..."), and in any case, and I didn't specifically mention any particular REXX entry.  I do wish you wouldn't accuse me of denying things to exist when I stated the opposite.  Also, I have just noted that the program example that I referred to has just been moved to ooRexx. -- [[User:Gerard Schildberger|Gerard Schildberger]] 22:16, 8 July 2012 (UTC)

  * 120 ooRexx examples   (with no classic REXX postings) this is your line which I may have misunderstood.
  this I consider some existing cross-postings.
  As to the one you addressed:
  I moved the result, not the program (as to your request and against my conviction: why not have results together??)
  And again: ooRexx implements Classic Rexx (with the few exceptions listed on top of this topic)
  --[[User:Walterpachl|Walterpachl]] 05:32, 9 July 2012 (UTC)

It was my start with the topic and you changed your program (thanks)
 "While the solution shown within this page at Rexx version 2 was created for Classic Rexx 
 it also can be used unchanged by the ooRexx interpreter and so can be considered a solution 
 for the ooRexx language too.
 
 Incompatibilities(*) that were originally in Rexx version 1 were meanwhile "fixed",  
 so it also can be used unchanged by the ooRexx interpreter and so can be considered
 a solution for the ooRexx language too.
 
 (*) Classic Rexx accepts assignment without an expression (x=;), ooRexx does not."
--[[User:Walterpachl|Walterpachl]] 21:27, 8 July 2012 (UTC)
--[[User:Walterpachl|Walterpachl]] 21:24, 8 July 2012 (UTC)
----

While it is not that hard to craft Rexx source that runs with ooRexx, it is much harder and often impossible using NetRexx - there a universal source would be a rare sight. Some in the NetRexx community regard it as a different language altogether - I am not necessarily in agreement with that. When Object Oriented sourcecode is used, the program is not compatible to Classic Rexx.
The previous I consider facts. These facts bring me to the opinion that most NetRexx should be in its own language category, and only procedural code can be shared by ooRexx and Classic Rexx. Conversely, when someone crafts an ooRexx sample, it should show off the oo facilities and class libraries and not be gratuitously incompatible to Classic Rexx. As an addition, a Classic Rexx sample that can be compatible with ooRexx should not use the corner cases in which it isn't.
[[User:Rvjansen|rvjansen]] 07:41, 9 July 2012 (UTC)
----
Actually we want to get to an agreement where to place Rexx code in this wiki.

Here are my thoughts:

Category ooRexx should contain only programs using oo-Features! (i.e. directives)

There could either be a general statement on top of ooRexx:
All programs under category REXX can be run with ooRexx
either unchanged (e.g., 24 game,...)
or when modified according to these descriptions. (e.g., Catalan numbers uses # in cat#)

OR
for every applicable task there may be an entry in ooRexx with either one of these sentences:
(1) The program under category Rexx can be run unchanged with ooRexx
(2) The program under category Rexx can be run with ooRexx
    when modified according to these descriptions (e.g., Catalan numbers change cat# to cat_nr)

OR the modified program could be put into category ooRexx
   (I wouldn't like this duplication in the wrong place)

OR the modified program could be put as version 2 in category REXX
   if the author of the current version does not want to change it  
   for the sake of compatibility/portability (e.g., identity matrix)

Please voice your opinion!

--[[User:Walterpachl|Walterpachl]] 09:04, 9 July 2012 (UTC)

----

I don't quite understand why the overlaying of reasons why an author may or may not want to modify/change a program entry (or even want to) to comply to another's language specification should be at issue.  The reasons shouldn't be assummed, nor should it be stated for the sake of (say, compatibility or portability). There may be other reasons, lack of time would be a major concern, for one.  It's easy to volunteer another person's time, after all. The classic REXX program examples that were entered were for classic REXX (and don't pretend to be for any other kind), and to enforce another language specifications (or more importantly, restrictions, if you will) isn't warrented nor fair.  No other Rosetta Code (language) programming examples have had this demand made of them (that I know of, I obviously haven't looked at the thousands and thousands of programming examples). Rosetta Code is supposed to be a place (I hope) to ''solve tasks according to the task description, using any language you may know'' (slightly re-worded for tense).  I hope that the spirit of entering solutions using what languages that one knows is still encouraged.  I've been doing that, consumming more time that I ever want to confess.  I think 380+ classic REXX examples are enough to stand as a (et al) collective classic REXX effort such that they don't have to be changed just to conform to another version of a REXX langage, object-orientated or not.  Nowhere is the demand of portability or compatibility asked or implied by the philosopy of Rosetta Code.  I may be wrong, I don't know what is/was in the mind of the founder when he started this site.  If ooRexx is that important, then people would've probably entered ooRexx examples long ago.  Many of the examples I've entered were ripped out of much bigger classic REXX programs, none of those were designed with even a thought of object-oriented REXXes (or it's restrictions), nor was it even a concern to make it portable to ooRexx (or any other o-o REXX).  Running those programs under ooRexx wasn't even feasible nor practical (nor even possible).  I don't have an ooRexx to ensure (verify) that the classic REXX examples even run (and produce the correct or same results), nor do I intend to install a version for that purpose. I have no interest in object-oriented programming and also have no wish to ensure/verify that classic REXX programs conform to ooRexx standards/restrictions, nor do I wish to ensure that they run under ooREXX. Object-oriented languages treat everything as objects (as I was told; I don't know that for a fact, nor do I want to take the time and/or effort to learn another language as classic REXX works fine for the programs as coded).  Classic REXX doesn't have objects.  I hear that ooRexx claims to run (most?) classic REXX programs.  I have no interest in proving or disproving that claim, I have no agenda concerning ooRexx. There are more differences than the ones mentioned above, and I'm not an expert in ooRexx (nor care to be), but one long thread (and I mean ''long'') on '''comp.lang.rexx''' had to do with the fact that everything in ooRexx is an object, and therein lies subtle differences in the way classic REXX treats stemmed variables versus how ooRexx does (I believe one main issue was how a ''stem'' is thought of, another issue was stem ''tails'').  (Mention was also made about breaking REXX ANSI standards, and that's another topic.) This may seem like a minor detail, but this single case (stems and/or tails) provoked a long and comprehensive and vocal discussion.  But being a "newsgroup", it lacked the formality and consise discussion that it deserved.  It can't be treated lightly, or more importantly, can't be ignored. I don't have an agenda to promote ooRexx and/or to find/explain/justify/clarify the differences, or why they are present.  I'm not sure if there are other languages that have this problem where an object-oriented language is incompatable with the parent (that there be a "few" differences/restrictions/incompatibilites or not). I get the sense that the word ''few'' is being used to give the impression that it's being used to minimize the number of differences -- that's just the way I read it. If not, then there's no reason to qualify the number of differences. Mention was made that ooRexx is a grandchild of classic REXX.  I don't see it as that, it's a seperate language (my opinion of course), but it did evolve from a shared basic syntax (as did NetRexx, AREXX, BREXX, PC/REXX, KEXX and probably others), but that's a matter of (everyone's/anyone's) opinion as there is no concise definition of what constitutes a dialect, nor is there a definition of when another language becomes different enough to be called a separate language.  That one can change the original language source code so that it runs under the object-oriented version isn't germain.  Almost any language can be changed if portability is a main concern (it's just a matter of degree) to run in another.  I've done that with BASIC, PL/I, and FORTRAN (to REXX), not with complete success, but it wasn't a useful or pratical endeavor.  That's not the way programmers, er ... program.  I try to make the classic REXX examples portable to the other classic REXXes that I have access to (and this was years ago), and I do verify the programs to ensure correct and consistant output (with the classic REXXes being used).  The classic REXXes all share a very similar definition (of the language), with a hardly a notable omission of a few functions.  Nobody has ever asked a '''C''' programmer to make their programs compatible to '''C++''' or '''C#''', or any other language such as Fortran or COBOL.  They (object-oriented versions) are different languages, even though the basic syntax is quite similar.

One thing about a dialect (as used in Wikipedia) is that it says its a ''relatively small variation or extension of the language''.  I can understand when a version of an interpreter/compiler adds functions, functions with more features/options, and maybe even statements.  But ooRexx is a major change/enhacement (I think) in capability and syntax.  That ooRexx can accept almost all classic REXX syntax isn't the issue, it has to do with the underlining concepts. In the talk section, it was stated that the '''C++''' (object-oriented) isn't a dialect of '''C'''.  Now, I'm not claiming that either statement is definitive, it probably just boils down to how many people believe one side of the issue or the other. Judging by the lack of a concise definition, I can only assume this matter is mainly a matter of interpretation as the separation of languages from dialects may end up being a case of degree. -- [[User:Gerard Schildberger|Gerard Schildberger]] 14:07, 9 July 2012 (UTC)
----
Much of the above digresses from my question how to handle Rexx programs that can run on classis AND ooRexx
without duplicating code or creating variants where not necessary.
I looked at (and tested) the alphabetically first 20 Rexx solutions and found 15 of them to be compatible.
The five that are not (and the incompatibilities:
 24 game/Solve                  L=
 Align columns                  v2: t.=; @.= _=
 Anagrams                       @.
 Apply a callback to an array   a.=;   b.=
 Array concatenation            p=  f=
So the major hangup here is the use of assignment without righthandside 
and second comes the use of symbols (symbol characters that ooRexx does not support.
Fixing these would be easy and allow for stating that "all Rexx programs shown here can run under ooRexx". A statement I would love to make.
ooRexx IS a grandchild of original Rexx because it was raised in the IBM family.
Now it has found a new home in RexxLA and is open source, free, stable and top quality.
Whether there are objects inside I don't care. I personally don't use those on the ooutside. Too old for that.
--[[User:Walterpachl|Walterpachl]] 02:34, 10 July 2012 (UTC)

-----

Walter, I wish you would refrain from stating what I don't love to do, or worse yet, assuming the reasons why I don't conform to your philosophy (based on your statements). 

The major hangups (as you see them) are yours, not mine. My reasons of why I don't dance to your tune are my own.

I will ''never'' make as assertion that any of the classic REXX example programs (that I entered on Rosetta Code or elsewhere) will work on ooRexx (or whatever REXX, not just the object-oriented REXXes) if I haven't tested them first on those languages.  Some, I'm sure, would work on AREXX or BREXX or OS/2 REXXes or CMS REXX or TSO REXX ..., but I don't have access to those (at least, not directly, and I certainly don't want to ask others to execute my versions on their computer, or worse yet, their employer's computer).  I don't have an interest on what does or what doesn't run on object-oriented REXXes; I have no interest in those languages, and I don't have the time nor inclination to install them for whatever reason. -- [[User:Gerard Schildberger|Gerard Schildberger]] 20:37, 10 July 2012 (UTC)

Just because ooRexx and classic REXX were developed in the IBM family, doesn't make one a grandchild of the other.  QBE was developed by IBM.  So were a lot of other languages ... -- [[User:Gerard Schildberger|Gerard Schildberger]] 20:37, 10 July 2012 (UTC)

: The next 20 tasks
:: 12 ok,  5 use @ or $, 2 use a=, 
:: and one (Brownian tree) uses @ and a= and scrsize() which is not available in ooRexx. 
--[[User:Walterpachl|Walterpachl]] 19:59, 10 July 2012 (UTC)

----- 

Yes, the REXX code from the classic REXX example in the ''Brownian tree'' task is:

```rexx
if height==0 | width==0 then _=scrsize()  /*not all REXXes have SCRSIZE.*/
```

which clearly states what you've stated. -- [[User:Gerard Schildberger|Gerard Schildberger]] 20:37, 10 July 2012 (UTC)
-----
An interesting case is under Rexx Roman numerals/Decode
: where Version 1 (good for all Rexxes)
:: was turned into 2 other versions that are functionally superior 
but no longer good for ooRexx (due to the use of # and @
--[[User:Walterpachl|Walterpachl]] 08:30, 10 July 2012 (UTC)
-----

Er, no.  The example version 2 for REXX was a straight copy of version 1's functionality (but with the superfluous statements and dead code removed), and as such, mimicked the inability to handle the Roman numberal '''IIXX''' (and others of this type). This means that version 2 has the same functionality as version 1.  I had added a comment to that effect (about the inability concerning version 1 and its cousin, version 2), but it was deleted by someone else.  

This also implies that the NetRexx version has the same problem, as the REXX version (as stated) was taken/copied/transcribed from NetRexx), but I can't verify if the NetRexx version as I don't have access to the NetRexx language.  Most likely, other examples have the same inability.

REXX version 1 ''may'' be good for all REXXes (which I'm not claiming, as I don't have access to all REXXes to test that claim, and I would be very surprised if anyone still does have access to all REXXes), but version 1 and version 2 still lack the ability to handle REXX numerals of the aforementioned type.  I don't know if that makes it good or not.  As I mentioned elsewhere, the Roman word for '''18''' is '''duodeviginti''' which translated literally means '''two-from-twenty''', and there exists a photograph(s) of '''IIXX''' chiseled on an archway.  I also seen '''XIIX''', which I admit, looks very symetrical. That numeral may not be of the form that was approved of in the years after when the format/form of Roman numerals were codified more restrictively.  Even the use of '''IIII''' wasn't approved of, but yet, wrist watches and clocks that use Roman numerals all have that form (I haven't looked at ''all'' watches or clocks, by the way, just a mere half-dozen or so). 

Proof (tongue-in-cheek):
* 3 is prime, 
* 5 is prime, 
* 7 is prime,
* 9 isn't prime (a square),
* 11 is prime, 
* 13 is prime,
* 15 isn't prime (ends in 5),
* 17 is prime,
* 19 is prime,
* ...
* all odd numbers are prime.

I'm not saying that those "unapproved" forms of Roman numerals should be used, but they ''have'' been used, and I think they should be converted correctely (to Arabic numerals).

Also, iterating again, I entered the classic REXX code to solve the task, using the language that I know best, and I never claimed it would work on an object-oriented version (such as ooRexx or NetRexx). Even if it would work elsewhere, I don't have the means or inclination (nor the time) to test if a classic REXX program will work on (any or all) object-oriented versions of REXX, or even other platforms, for that matter. -- [[User:Gerard Schildberger|Gerard Schildberger]] 20:37, 10 July 2012 (UTC)

-----
Again a long story not addressing the points that I made:
You changed the program to the worse (for ooRexx!) by changing a reasonable name to a
characted not supported by ooRexx.
I had the time and the inclination to test (some of) your programs with ooRexx
and have shown the results.
SCRSIZE: of course you said that. My point: All 40 programs work on ooRexx unchanged or 
with minimal changes @->a #->n $->d except for this ONE.
It's your choice to insist on using @ and # and $ forever.
And I end the analysis now.
--[[User:Walterpachl|Walterpachl]] 06:49, 11 July 2012 (UTC)
