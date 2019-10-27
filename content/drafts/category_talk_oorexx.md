+++
title = "Category talk:OoRexx"
description = ""
date = 2017-06-02T23:43:34Z
aliases = []
[extra]
id = 11827
[taxonomies]
categories = []
tags = []
+++

__TOC__

==ooRexx fully compatible with classic REXX==  

Could the phrase 


''Since ooRexx is fully upward compatible with REXX, every Rexx program shown here can be also run, unchanged, using ooRexx''


be removed or corrected?  I know the next paragraph says otherwise, but why have an incorrect statement at all, even if corrected later in the same section?  My area is classic REXX and I feel like a brown shoe at a tuxedo party here.  

ooRexx isn't fully compatible with classic REXX, there are differences, even if only a few (I may be wrong on the number of differences).  I don't want to enumerate them (at least the ones that I know of), for once done so, people will think that is the extent of the differences.  I'm not an expert in the subtleties of ooRexx (differences).  If the offending statement gets removed or corrected, feel free to delete this section (in the TALK section) in its entirety. -- [[User:Gerard Schildberger|Gerard Schildberger]] 20:24, 21 June 2012 (UTC) 

----
I adapted the ooRexx section to the best of my (current) knowledge
[[User:Walterpachl|Walterpachl]] 17:36, 22 June 2012 (UTC)
----
The compatibility is vast and the differences are few.  

If all the different interpreters were exactly the same then they wouldn't be different interpreters.  

Perhaps the sentence should read ''It is possible to write Classic rexx code that won't run unchanged on ooRexx''.    I have yet to see a case that could not be easily re-written without changing the meaning of the code in a way that will run on both regina and ooRexx. The wording that Gerard appears to be pressing for, would suggest that code from any classic Rexx interpreter would run on any other classic Rexx interpreter, but not on ooRexx; and that seems to me to serve the truth even less than saying ''fully compatible''.
--[[User:Sahananda|Sahananda]] 05:43, 26 June 2012 (UTC)



-----


There're different REXX interpreters, and most likely, for one reason (the main reason?) at least, is that the (at one time) REXX interpreter wasn't available for other platforms, and there was an apparent and obvious need (or desire) to have a REXX interpreter on DOS.  I believe that's why the first non-IBM version (PC/REXX?) was written --- to make REXX available for the PC -- but it was bundled with KEDIT and that made the reasons a little murky in retrospect, part of KEDIT's (an XEDIT wannabe) flexibility and useability was using REXX as it's macro language. -- [[User:Gerard Schildberger|Gerard Schildberger]] 19:10, 26 June 2012 (UTC)

The two main objectives of (classic Rexx) Regina are to "become 100% compliant with the ANSI Standard, and be available on as many platforms as possible".  I'm sure that there are other (secondary) reasons as well for the various REXX interpreters. -- [[User:Gerard Schildberger|Gerard Schildberger]] 19:10, 26 June 2012 (UTC)

As for my wording (suggested or otherwise): no, I never said, implied, or suggesting that code from any classic REXX interpreter would run on any other classic REXX interpreter.  If it would, I wouldn't have so many Regina bugs entered in Source Forge. -- [[User:Gerard Schildberger|Gerard Schildberger]] 19:10, 26 June 2012 (UTC)

I should not be accused of not serving the truth for something that I did NOT say.  (This is not true, no?)  This is a strawman argument.  I resolutely stand by the words that I did NOT say and will defend my right to not say those words that I won't say. -- [[User:Gerard Schildberger|Gerard Schildberger]] 19:10, 26 June 2012 (UTC) 

I could say, ''"It is possible to write Classic REXX code that won't run unchanged on Fortran"''.  Also, ''"it is possible to write Classic REXX code that will run unchanged on Fortran"''.  But I was only contesting the statement that ''Since ooRexx is fully upward compatible with REXX, every REXX program shown here can also run, unchanged, using ooRexx''.  That italicized statement isn't true.  I asked to have that statement removed or corrected (and it was corrected). I also didn't suggest any sort of wording to be used for the manner of correction. -- [[User:Gerard Schildberger|Gerard Schildberger]] 19:10, 26 June 2012 (UTC)

Also, it was suggested that some classic REXX programs be changed to run under ooRexx.  Could sauce for the gander be sauce for the goose?  That is, could ooRexx programs be changed to run under classic REXX?  [I'm not suggesting that this be done, I'm asking the more-or-less rhetorical question.]  I'm not an ooRexx expert (I can spell it easily enough), but do simple tasks require the use of object-only features? I would imagine some tasks could use those features to make the code more concise or easier.  I didn't intend for my dog to be dragged into this discussion (on ooRexx), I was entering classic REXX programs only.  And then the ''24 game'' was thrown in front and center into a roiling controversy. If it weren't for the changes and programming style made to the classic REXX program that I entered, I wouldn't be here in an ooRexx discussion trying to defend statements that I didn't make concerning classic REXX vs. ooRexx. -- [[User:Gerard Schildberger|Gerard Schildberger]] 19:10, 26 June 2012 (UTC)

----
Apologies if I misquoted you -  the only quote I can see that I attributed to you was ''fully compatible'' which I see even now in the 2nd para of this section and the section header.  I admit that you did not press for this wording, merely included it in your argument.  It was careless of me to refer to you as ''pressing for it'', and reading your post again in the light of your more recent post, your original post seems more reasonable.  My mistake. 

I'm not sure I follow all your arguments Gerard.

As I see it, ooRexx is a classic rexx interpreter, that also has object oriented abilities.  I would be surprised if there are any tasks that would run on the other classic rexx interpreters that could not be made to run on the ooRexx interpreter using only the classic features with few changes.  

It seems true - though it comes as a genuine surprise to me that many of the rexx examples here are written in such a way that they will not run on ooRexx.

: I can only speak for the REXX code that was entered by me, the programs were written and tested in three classic REXX interpreters and I don't assume they'll run anywhere else, I would test CMS and TSO versions if I could.   I may be reading into your statement more than was intended, but it was not written in such a way that they won't run, that is, no other intention was intended to have them also runnable in other (object-oriented) REXX interpreters (as far as ooRexx).  I have no interest in writing code that may or may not work for ooRexx, only classic REXX.  Some run correctly under ROO (at least, it produces the same results which could be happenstance), but I don't pretend to know all the nuances and/or subtleties of object oriented REXX, and saying that would (or does) run would be a tacit way to say that it works correctly as intended under ooRexx.    

I don't have the numbers, but I believe there are many people who run ooRexx, without taking advantage of the oo facilities.  The point I was trying to make is that the differences you find when you use it like this are like the differences between one classic rexx interpreter and another. 

: How so?  Which differences would those be (excluding bugs, of course).  Of course, some classic REXX interpreters are much older than others and don't have some of the newer BIFs or their extensions, but those differences weren't made that way, there was an evolution while keeping capability to the older versions of REXX.  REXX programs that I wrote in 1982 will will run unchanged in the CMS and TSO REXXes of 2012. 

:: What I had in mind here particularly was permitted character sets.  I am not an expert in different interpreters - I have used VM/SP on CMS, the Rexx interpreter that came with PCDos7, Kexx, Personal rexx, and for the last decade or more ooRexx (or Object rexx as it was back then) with a little Kexx thrown in.  I don't remember it all, but I remember that the not symbol on sp6 was ¬, I don't now remember what was not on PC Dos7. On personal rexx it was ~ which is the message operator on ooRexx.  On SP6 you had to start your code with a comment /* */ so the correct interpreter was invoked.  In Kexx comments are lines starting with a * - that is a standard, but not a Rexx one.  

::: Yes, KEXX was a subset of REXX, a few other notable restrictions is that every clause had to be complete within one line, and that included comments.  KEXX also accepted the normal REXX comments, but again, they had to be complete within one line (no multi-line comments).  There were many other restrictions, most of which aren't probably germane to this discussion.

I would say that the main difference between ooRexx as a classic rexx interpreter and the other classic rexx interpreters is that ooRexx syntax checks the whole code before running any of it thus you cannot have syntactically invalid code in a clause even if it is never run.  
Other differences can generally be avoided, the use of -- for + is problematic in ooRexx, but you could write code that would function on any interpreter by leaving a space between the operators without changing the meaning or function of your code.  Certain characters such as @ were valid in early interperters, but cannot be used in ooRexx, although it does honour the ¬ sign.  

: I'm not sure, but the way the REXX interpreter parses the source isn't defined in the language specifications.  The reason that the older REXX executed code as it read the file (line by line) was a matter of speed, as I recall.  Hard disks were a lot slower than today, of course. I believe ''R4'' parses the whole of the program before it begins execution, and almost all REXX program normally invoke a BIF at some point, which necessitates the need for the interpreter to parse the rest of the source anyway (looking for an internal version, which at the invocation time, it can't tell if the subroutine/function is internal or external, excluding those subroutines enclosed in quotes).  Also, you presume that the use of a double negative was a conscious choice, it may be (macro/program) generated statement, or a user-entered REXX clause for instance, among other reasons.  I hate to think about writing any code (REXX or otherwise) that could become invalid in another five or ten years time.  Unforeseen "requirements" changing the validity of valid REXX statements. I have yet to see the advantage of introducing the double-minus comment to classic REXX, especially in the light that it disables (breaks or invalidates) a legitimate REXX statement.  One could use the same argument (say) for using a '''//''' comment, everyone could break up the two '''/''' in their programs (albeit, this particular example has a lot more uses than a double negative, but that's only a popularity contest).  Implementing a new way to code comments via least astonishment, in my opinion, isn't a sound way to extend a language. But then, nobody asked for my opinion when the vote came up. -- [[User:Gerard Schildberger|Gerard Schildberger]] 00:23, 27 June 2012 (UTC)  

:: I also as far as I remember was not at the right place at the right time for that decision either, and I take your point, it probably wasn't an entirely smart move, but taking the -- as a given now, removing it would break many programs, whereas, if there were actual programs that were broken by this (I have never heard of any actual cases), then they could have been repaired by the simple expedient I mentioned, however generated.

Of course Regina has several features such as address-with that I believe are unique to it just as personal Rexx did (I loved the RxWindow package).  

I don't know which of the rexx examples are yours, but many of them would run on ooRexx if they didn't use variable names like @.  However, if the task demanded single character unreadable variable names you could still use ! or ?.

: I use '''@''' and '''$''' more or less consistency (to mean certain things), as well as '''!''' and '''?'''.  I do note that ''ROO'' supports all those characters just fine without precluding them for use as variables.  [''ROO'' is an object-oriented REXX interpreter.] 

:: I have met roo, though I find it as baffling as some people seem to find the oo side of ooRexx.  Again, I'm not an expert on the developments that rexx has gone through, and I could be wrong about this, but I understand that the decision to disallow those particular characters pre-dates Object Rexx, and I assume that there was some reason behind the decision.

::: I don't know where (or when) that happened (the disallowing).  What classic REXX doesn't allow those national characters? -- [[User:Gerard Schildberger|Gerard Schildberger]] 20:14, 27 June 2012 (UTC) 

Perhaps you might explain to me why you would object to the rexx code examples here being written in such a way that they would be portable between what are probably the two most popular rexx interpreters.  I would see that as an improvement that would serve visitors to this site and cannot understand why you would object.  If you do object, then it might be as well to know which of the rexx examples are yours.

: I never thought about "signing" my REXX examples, if that would help someone's understanding of where it would run, I'll do it.  It would take me a while to go through the 380+ REXX examples.  As I stated elsewhere, I wrote all my code to work on Classic REXX and don't assume it runs anywhere else, nor do I test it for a REXX interpreter that I don't have installed. I don't write the classic REXX code and keep thinking if this will or will not run on ooRexx. I concern myself to make sure it runs as intended for the Rosetta Code REXX entry for classic REXX. If writing code for maximum portability is good, then writing ooRexx code that doesn't need object-oriented features would also be good, this would lead to more ooRexx code being portable to classic REXX.  I hope that one can see the obvious absurdity of that fallacious statement. Of course, you could always look in the history of who entered the code (for the example program).  I have noticed that very very very few people sign their code.  It seems rather egocentric to do so. -- [[User:Gerard Schildberger|Gerard Schildberger]] 00:23, 27 June 2012 (UTC)

:: I think this is the nub of the problem, not wishing to repeat myself, but to many people ooRexx is just another classic Rexx interpreter.  For them, I assume that portability is important.  Anders and Mark have done a good job, and there is a fantastic product there.  Were it not for an accident of history I might have been a Regina user too.  There are things you can do in Regina that cannot be done with ooRexx (address-with springs to mind, some of the options).  ooRexx also has it's strengths, and if someone wants to meet the oo paradigm it is a jolly good place to start.  Of course I see that it is absurd to value compatibility so highly that you deny yourself the power of the language.  I just didn't see that choosing particular single character variable names should rank that highly as a priority.  However, I did write that before you said that these variable names have a consistent meaning. 

::: The very definition of a classic REXX interpreter excludes an object-oriented REXX.  Some people may think that ooRexx is a classic interpreter, after all, some people believe that ooRexx will run any classic REXX code without change.  It'll be interesting to see the list (matrix) of incompatibles (or differences, if you prefer) between the various classic REXX interpreters and ooRexx. I would hope that ''roo'' would be included in that matrix.  ROO (as far as I can tell) has fewer restrictions in that regard. -- [[User:Gerard Schildberger|Gerard Schildberger]] 20:14, 27 June 2012 (UTC)

::: Also, keep in mind, some of these classic REXX programs were written before there was an ooRexx, and the choice of variable names was made for (and under) classic REXX, others were simply inherited from older code that couldn't/shouldn't be changed.  Some of the decision making was colored by PC/REXX's restrictions, and at the time, that was the only game in town as far as coding REXX on a PC in a Windows world (or MS' DOS).  The PC/REXX's restrictions that were applicable (and still are): no single REXX line could be longer than 250 characters (a limit that my tested and true ''one-liner'' subroutines share), and a limit of a 40k memory pool (called an ISA) of literals and symbols.  There is also a limit on the length of a REXX clause. Now 40k isn't that much when you start writing a serious REXX program.  I had often wished that limit was raised or eliminated entirely.  (I still wish for world peace also, but there ya have it). I still use PC/REXX in syntax checking (without actual execution).  I wonder why the the '''S''' option (Scan) for the '''TRACE''' instruction was dropped from non-IBM REXX interpreters, it was a very useful option for debugging.  Oh well, water under the bridge. I regularly use PC/REXX's "optimize" option for that purpose, in addition to creating an "optimized" version of the program, it checks for syntax errors at the same time (without executing the program, of course). It should be noted that the word ''optimized'' would better be replaced with ''tokenized'', but ... that's the word used, and it does optimize the REXX program's execution. One nice thing is that ''R4'' will execute the optimized REXX program correctly, but Regina doesn't (execute it at all). It has to do with Regina's ignoring the end-of-file character. -- [[User:Gerard Schildberger|Gerard Schildberger]] 20:14, 27 June 2012 (UTC)
 
:: I certainly don't want to be an irritant to you, and I am incredibly busy at the moment, but I could offer to take a bit at a time the classic Rexx samples here and see what it would take to make them run on ooRexx.  I could then pass them back to you and if I hadn't broken them for regina etc., then you could choose to post them or not.  I quite understand if you are not interested.  --[[User:Sahananda|Sahananda]] 12:24, 27 June 2012 (UTC)

::: If you're going to change them for ooRexx, why not also test them to see if they still work with the three class REXX interpreters that they were intended to work for (and also already tested)?  I really don't want to spend that amount of time re-testing other people's code.  I'm pretty sure your time is as valuable to you as my time is to me. I'm not sure if you appreciate the amount of time that will be spent in just visiting each of the classic REXX programs, updating (changing) them, added summaries, and testing for (at least) four REXX environments, and that's assuming that you have the aforementioned classic REXX interpreters.  Some (ok, ok, a few) of the classic REXX programs were also tested under CMS and/or TSO (by obliging dear friends)  [--- if it works on one, it'll most likely work on the other, as it's the same REXX interpreter] --- and I certainly don't want to put them through that again (running of non-business work on the company's dime). This avenue of testing is water over the dam ... most of them have retired. I started to go through the REXX examples, trying to removed the extra blanks lines that my earlier REXX examples had at the beginning and end of the REXX programs, and even that was taking a lot of my time. So far, I got through the '''A'''s and '''B'''s.  -- [[User:Gerard Schildberger|Gerard Schildberger]] 20:14, 27 June 2012 (UTC)  

::: One option would be to include a 2nd version, with a comment or note that it was changed to run under ooRexx and only tested under ooRexx.  I suppose that just placing that version in the ooRexx language section would be simpler. By the way (and this is no small thing), it was this multiple testing of my classic REXX example programs that led to me finding numerous errors in the Regina and R4 classic REXX interpreters.  I never assume that even writing simple code will correctly work on all REXX interpreters until I actually execute them. Ya never can tell when something will rise up and <strike>bit</strike> bite ya in the hinder. -- [[User:Gerard Schildberger|Gerard Schildberger]] 20:14, 27 June 2012 (UTC)        

thanks. --[[User:Sahananda|Sahananda]] 22:48, 26 June 2012 (UTC)

== Plagiarism ==
In that the code presented herein is covered by the [http://www.example.com GNU Free Documentation Licence 1.2], I'm not convinced that anything developed or reimplemented from an example in another language can be or should be identified as &quot;plagiarized&quot;, particularly if the appropriate acknowledgements are in place.  (Rosetta Code even provides the means witihin the editor to indicate that a program is a direct translation of some other work via the <nowiki>{{trans|XXXX}}</nowiki> template. <nowiki>[</nowiki>[[Template:Trans|see Template:Trans]]<nowiki>]</nowiki>)  Plagiarism can be a very emotive word and a serious charge to level at someone, particularly when related to software development; we should avoid describing work presented here as such.

Remember that by contributing programs to Rosetta Code  you are &quot;promising us that you wrote this yourself, or copied it from a public domain or similar free resource&quot; [sic.] --[[User:Alansam|Alansam]]

Pls advise at a better (acceptable) wording!
I did to the best of my knowledge.
--[[User:Walterpachl|Walterpachl]] 08:30, 9 June 2012 (UTC)

:Hi Walterpachl; You should say that the second was an adaptation of the first example that works on the IBM ...

:In general, the two paragraphs should be changed so that there is no need for the correction mentioned at the beginning of the second paragraph, just remove the wrong info on compatibility between Rexx and ooRexx. --[[User:Paddy3118|Paddy3118]] 08:54, 9 June 2012 (UTC)

thanks. changed.
--[[User:Walterpachl|Walterpachl]]

: The first paragraph still states that ''Since ooRexx is fully compatible with REXX, every ...'' is incorrect.  ooRexx is '''not''' fully compatible. -- [[User:Gerard Schildberger|Gerard Schildberger]] 17:03, 10 June 2012 (UTC)

: It was mentioned that the REXX example for the '''24 game''' was adapted (changed?) to run on IBM's Rexx (presumbably TSO's version).  What exactly was changed to do this? What code in the original example is incompatible with IBM's version? If this can be enumerated, than I could go back and change all the other REXX examples to make them all IBM REXX compliant. -- [[User:Gerard Schildberger|Gerard Schildberger]] 17:03, 10 June 2012 (UTC)

: I cannot use backslash and vertical bar on my (German) TSO
That's why I avoid the backslash (using <> instead of \=) and have still to exchange | and !
when up/downloading code.
The only other thing I changed was L.= to L.='' and the like
This not for the host's sake but for ooRexx'
The host accepts it:
6 *-* a=                 
7 *-* Say '>'a'<'        
  >>>   "><"
--[[User:Walterpachl|Walterpachl]] 08:30, 11 June 2012 (UTC)

:: (Removed my incorrect comments about CMS and TSO recognition of the ¬ (not) symbol) -- [[User:Gerard Schildberger|Gerard Schildberger]] 22:51, 19 June 2012 (UTC)

:: I should also mention that Regina REXX doesn't honor the not (⌐) symbol, but PC/REXX, R4, and ROO do.   I would like to know if NetRexx, ooRexx, CRX, AREXX, BREXX, and others support that character.  Perhaps if someone could chime in who know those interpreters. -- [[User:Gerard Schildberger|Gerard Schildberger]] 08:52, 11 June 2012 (UTC)

----
I wrote me a little program that just translates \ and | to ^ and ! in the source (except for literals and comments) and version 1
(Gerard's) of the 24 game works, if translated that way and otherwise unchanged, on TSO. TSO supports the A=; 
--[[User:Walterpachl|Walterpachl]] 21:16, 12 June 2012 (UTC)

: Are you saying that TSO doesn't support the logical or symbol (|) and that it also doesn't support the \ (backslash) symbol for negation? -- [[User:Gerard Schildberger|Gerard Schildberger]] 22:51, 19 June 2012 (UTC)

: I believe what Walter is saying is that his German keyboard and/or code page don't support those characters in the same way as they don't support other characters commonly found on a US keyboard (like the $ character).  As I remember this is the reason that !, ? and _ were introduced as symbols and $, @ and # were deprecated. [Ref. ''The REXX Language : A Practical Approach to Programming, <small>Second Edition</small>'' by M. F. Cowlishaw, 1990.  <small>Page 21, Footnote 14.</small>]  --[[User:Alansam|Alansam]] 02:00, 27 June 2012 (UTC)

:: I always remembered all those six characters as being in the earliest release of REXX, along with the '''¢''' (cent) sign, all seven of them are still supported on the CMS and TSO version of classic REXX, as well as ''R4'' and ''ROO''.  You may say some of those characters were deprecated, but they're all supported still on the IBM mainframe REXXes, as well as the REXX compiler. It should be noted that current IBM (REXX) publications note that not all keyboards and/or code pages support the '''!''' (exclamation point) symbol, but that doesn't stop (IBM) REXX from supporting that character (or others) for use in REXX programs. -- [[User:Gerard Schildberger|Gerard Schildberger]] 02:44, 27 June 2012 (UTC)

:: '''I'm''' not saying the characters were [[wp:Deprecation|deprecated]], the author of the language in his last publication (see the reference above) is saying it.  There are many features in many programming environments that are deprecated; it doesn't mean that they don't/won't work it just means that you should think carefully before using them because as the environment evolves they '''may''' stop working.  Such is the case with these characters in (for example) ooRexx.  --[[User:Alansam|Alansam]] 20:02, 27 June 2012 (UTC)

== Coloring / Highlighting Rexx code ==

I asked for adding GeShi highlighting for Rexx (and ooRexx) code.
This is now in place.
Thanks are in order to Jon Wolfers who created the php some years ago.
Also many thanks to Benny Baumann who took care of adding rexx.php to GeShi and link rosetta to it!
Please state whether you like it, hate it, want it removed, want it changed.
Always trying and sometimes achieving something
--[[User:Walterpachl|Walterpachl]]

: You may want to put a pointer in the category for REXX as well as this poll involves that language, more so than ooREXX as there are many more REXX entries than ooREXX. -- [[User:Gerard Schildberger|Gerard Schildberger]] 16:44, 9 June 2012 (UTC) 

: I vote NO, strongly and emphatically, for the use of the PHP for REXX.   For REXX code, particularly with code that contains a significant amount of comments, I find the italicization almost dreadful to read.  When I read (other's) REXX code, my eyes drift to the comments (especially if they are robust and plentiful) to get an overview of the program (which is especially important when I don't have a nary clue what that program is doing), and the italics really really make that difficult.   I can't be the only older programmer that fading eyesight is becoming a problem.  As for the highlighting, it too is distracting, and well, detracts from the readability and flow of the code.   On the other hand, highlighting is nice for "strange" languages where I don't know what are keywords and what are variables, etc.   When a language is more natural (to read), highlighting just gets in the way.  I (try to) read a lot of other code, especially tasks that are very hard to understand, and some tasks (and many golf solutions/examples) are downright obtuse, so I spend a lot of my purusing time trying to read other languages solutions and I find that the ones with minimal highlighting much easier to scan/read.  However, for the golf cases, for some strange reason, the comments are of the null variety, but that's another story.  [We could start a whole new thread on the dearth of comments, and if ever there was a place for a huge amount of comments, Rosetta Code would be the place.]  To me, it almost looks like someone got a new box of crayons and went ka-razy, thinking that the more colors, the better it looks.  Nothing could be further from the truth.  It would be nice if the hypertext preprocessor could be disabled by the user (trying) to read the source text.  I don't mind it so much for my own code, as I just ignore the PHP version on Rosetta Code and edit a copy of the source code locally, which really shouldn't have to be the case, as the PHP is ''supposed'' to make reading the code ''easier'', but instead it gets in the way.  Walter Pachl (above) had an idea to possible use &lt;lang crexx&gt; for the coloring, and &lt;lang rexx&gt; for "normal" viewing.  I beg your indulgence concerning the length of this entry as I'm trying to explain my rather intense feelings about this PHP. -- [[User:Gerard Schildberger|Gerard Schildberger]] 16:44, 9 June 2012 (UTC)
:: The purpose of the syntax highlighting is to make it easier for someone to read the code. In particular, one of the key purposes of Rosetta Code is to present languages to people who don't know them by means of idiomatic examples, and that is enormously easier for people when code is highlighted according to a “standard” scheme. The key common parts are that comments are one format, strings are a second (where supported by the language) and keywords (and keyword-like things) are a third. Like that, the structure of the code itself is apparent. Unlike with a larger project, here it should be possible to go ''fairly'' light on the comments; very long or very complex stuff is outside RC's scope (well, for reasonable definitions of “long” and “complex”; even that varies by language).
:: That said, GeSHi works by marking up the ''rendered'' code with HTML <code><nowiki><span></nowiki></code> elements that are labelled with the semantic class that has been identified for that part; you can override what they look like (e.g., turning off all foreground coloring and font changes for REXX) by supplying your own CSS overrides. Now, I admit I've never experimented with doing that sort of thing on MediaWiki, but I know others have on Wikipedia so it must be possible (and Google will be able to find out the details). That will give you what you want (unhighlighted code) without stopping others from getting what they want. (You'll want to apply an important override to all spans that are children of a <nowiki>
```txt
</nowiki> that is of class “rexx”.) –[[User:Dkf|Donal Fellows]] 23:40, 9 June 2012 (UTC)

::: I think ''going fairly light on the comments'' is going against the grain (philosophy) of what Rosetta Code is about.  Comments help explain the code (obvious to experts) but very helpful for most people whose expertise isn't the language they're reading.  I've been adding comments left and right as I go back frequently and try to better explain hows and whys of the (REXX) code;  I can't believe that anyone would discourage comments (even verbose ones), heavy or otherwise, even those that might seem too obvious  (if I understand what is being suggested).  Hell's bells, I wouldn't mind a ten page dissertation on the '''J''' examples (probably all of them).  Boy oh boy, that is some terse coding. -- [[User:Gerard Schildberger|Gerard Schildberger]] 07:37, 10 June 2012 (UTC) 
:::: Yes and no. :-) Write the code so that it is so obvious that it doesn't need so many comments, e.g., through careful choice of variable names or possibly refactoring into suitable procedures (with informative names). Keep comments for stuff that is harder to capture that way. Sometimes it can also be helpful to just add more blank lines and split the code over more lines so as to give it room to speak for itself. (I can't tell you exactly how to produce readable informative code, as so much of it is about æsthetics, but comments are definitely not the only tool in the arsenal for that fight…) –[[User:Dkf|Donal Fellows]] 08:01, 10 June 2012 (UTC)
 
: I vote YES, also strongly and emphatically, for syntax highlighting.  I disagree with almost every point Gerard makes; I've always found that highlighting keywords makes it easier to follow the logic flow particularly when strange indentation styles have been adopted.  I'd also like [[NetRexx]] included in the deployment if possible.  (It's syntax is so [[REXX|Rexx]]-like I can't see it being much of a stretch.)  P.S. There may be a better way, but inserting &quot;angle brackets&quot; (&lt; and &gt;) in the text can be done with the HTML tags &quot;&amp;lt;&quot; and &quot;&amp;gt;&quot;. --[[User:Alansam|Alansam]] 19:50, 9 June 2012 (UTC)

: I also vote YES, although I find the italicisation a bit weird.  It seems to be the mandatory style for comments on GeSHi.

: I vote No.  While I much prefer the clarity and universality of Walter's versions of Rexx task code, I must side with Gerard on GeSHi.  Code highlighting is a valuable tool for debugging but I find it distracting (especially color) when trying to read Rexx.  I have no problem with highlighting when the viewer can control what is being highlighted, but please don't inflict your choices on me.  This is a language-dependent determination; {many {languages {especially {C} derivatives}} {benefit {from the metadata}} provided. :-}  For Rexx, "code in black, comments in gray" is as far as I'd go.
: The best reason to eschew syntax highlighting is that like Rexx token casing and clause indentation, it will be a constant source of contention. --[[User:Aviatrexx|Aviatrexx]]




So the opinions are arriving. Thanks
I think you can see the black/white undistorted code when pressing Edit. 
 
A possible solution to make everyone more or less happy would be to:
* remove the rexx php from rosetta (it was worth a try)
* add a crexx.php (essentially the current rexx.php - c stands for colored)
* and an oorexx.php which takes care of the new keywords such as class, method etc. (they are curently also in rexx.php).

The best solution would, of course, be to have a switch to toggle between highlighted and plain display.
But it's far beyond my reach whether this is feasible and even further to implement it.
--[[User:Walterpachl|Walterpachl]] 07:14, 10 June 2012 (UTC)

::: I've done this via some code   (Shared CSS/JavaScript)   that was written/given to me, so I have a toggle switch that gives me the ability to turn off syntax highlighting when I want.   I think that switch should be made available to everyone without the need for everyone to install the script to enable the toggle.   If anybody wants to view it, I could display it within a html PRE window.   -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 23:59, 25 March 2016 (UTC)

: Categorically NO to “crexx” (well, unless someone produces a REXX variant that actually has that name after case-normalization). The parameter to <nowiki><lang></nowiki> is semantic, not just some flag to the highlighter; it's actually used for some of the off-line tools associated with RC. However, having a separate oorexx version ''is OK'' exactly because that is naming a language variation; it would cause different semantic markup to be applied (which might happen to include some extra syntax classes, but that's a side-effect).
: It's also possible that tuning down the amount of highlighting would be a good idea; an example might be to not mark up numbers and parentheses. –[[User:Dkf|Donal Fellows]] 07:52, 10 June 2012 (UTC)

:: Point taken. Someone said that the main obstacle (Italization of comments) cannot be removed. Is that true? Anyone any hope for the toggle? --[[User:Walterpachl|Walterpachl]] 10:25, 10 June 2012 (UTC)

::: If you read the generated HTML, you'll see that all you've got is a <nowiki>
```txt
</nowiki> (with a class attached) which contains text marked up with <nowiki><span></nowiki> tags, with classes. Changing what things look like is just a matter of changing the CSS style. Changing things dynamically is just a matter of changing the CSS dynamically. Modern browsers can do that (jQuery does so to great effect) so it is Just A Simple Matter Of Javascript Programming. Personally, I hate coding in JS (and also hate futzing around with CSS) so I won't work on togglable styles, but there's certainly sufficient in the way of hooks to do it, and I believe that there's the ability to hook running of your own custom JS when you view pages (but I don't know the details, and you've got equal access to search engines).
::: In short, you've got the tools available. Get coding! :-) –[[User:Dkf|Donal Fellows]] 15:11, 10 June 2012 (UTC)
----
:Thanks for the hint! Unfortunately not my area of expertise but maybe someone's
..[[User:Walterpachl|Walterpachl]] 15:28, 10 June 2012 (UTC)
----

I vote YES, as syntax highlighting is an incredible enabler for comprehension of code. That is also the reason IMHO that practically every modern depiction of code has syntax highlighting. Or the other way around: if no syntax highlighting is available, it is very hard for interested readers who are not acquainted in detail with the syntax of a language to comprehend what is demonstrated in a short time. As we are talking about a Rexx language, which from its inception was intentionally "human centric", all the Rexx (and ooRexx) examples should be as short/simple as possible, using meaningful names for variables, such that the snippets become self-documentary. (Concurring empathically with what user Dkf above has stated and explained so well.) 

Ad comments being shown in italic: this is fine with me as they remain easily legible, except the italics signal (in all languages) that the text is comment, making it easy to find the comment in longer programs. 

Remember, syntax-highlighting is aimed at easying comprehension of code for anyone, but especially for newcomers, unacquainted but interested readers. Not applying it to ease comprehension, makes it unnecessary hard on the reader who (mistakingly) may attribute the language itself for being difficult to understand! The latter is also true for "clever programming" tricks in a language, which only could be appreciated by the ones in the know, but makes it impossible for newcomers to understand/comprehend the examples, hence making Rexx/ooRexx appear to be very difficult! 

So please leave the "rexx" syntax highlighting in place and maybe create a GeShi variation for "oorexx", adding keywords and subkeywords introduced by it. (Also this may then be applied to the Sourceforge Wiki ooRexx samples to ease rading and comprehension.)
--[[User:Rony|Rony]] 11:08, 10 June 2012 (UTC)


----

First I should say that the anonymous entry above was mine - it was not my intention that it should be anonymous, but that is how it appeared.  This time of use, the editor appears with a useful toolbar and link to editing help.

I 'created' the PHP for GeSHi rexx colouring several years ago when I had some spare time, mainly with the intention that colouring should appear on the Sourceforge ooRexx wiki.  It is the first and only PHP I have ever 'written' and I achieved it by plugging constants into a template.  I submitted it to the GeSHi project, but for some reason it never got used.  

I never saw how it rendered anything until yesterday.

I did not specify in that PHP what colours or styles should be applied to what elements of the language, only how to identify various elements.  It is a long time ago now, and I cannot remember what is possible with GeSHi.

It never occurred to me that there were people in the world for whom syntax highlighting was anything other than a boon, although I do find some renderings over-fussy and as I said above, I find the use of italics as weird.

The decisions made here will apply to other Wiki based websites that use GeSHi.

So, whilst I am sorry to inconvenience Gerard, I must state a strong preference for syntax-colouring. This time, let's hope it is not anonymous.
--[[User:Sahananda|Sahananda]] 16:09, 10 June 2012 (UTC)

----

It occurred to me that one litmus test of highlighting (or not highlighting) --- and that nobody brought up --- is what people use on their own computers.   

I use KEDIT exclusively when editing REXX files.  And I heartly endorse KEDIT  coloring.   It's very very good.   The best best part is, '''it can be turned off!'''.   I never use it.    

It has one other good feature, it doesn't change the font, so NO italics!   If you do like lighlighting, you can ''choose'' what colors are to be used for highlighting (if ya want it) for what thingys.  

But I would drop KEDIT today if it forced it on me.    Well, ok, I wouldn't drop it, I would just use an older version.  At least, I have a say on what is productive for me.   Fortunately, the author of KEDIT didn't force his idea of productivity or ease of use on me.  Kudos.  

I did also notice that nobody was clammering for highlighting for REXX, NetREXX, or ooRexx ''before'' it was enabled for REXX.   

I also noted that the PL/I language which reads very much like REXX (one of REXX's parent) doesn't have highlighting, and nobody is clammering for its highlighting.   If there were many more comments (heck, most programs seem to have no comments) in other example programs, there'd probably be a push to change the font from italics to, er, a plain font.  

Somebody said (above) that we've got the tools available. Get coding!.   Yeah, easier said than done.   I really don't want to learn JS so I can read REXX code easier.  I really don't like to have to learn a language to modify the behaviour of a syntax colorer.

If somebody has the expertise and can inform those that want it how to disable highlighting (and especially those italics!), there'll be a check coming in the mail to ya! 

Be thankful that section and header comments are left alone!  They may be next. -- [[User:Gerard Schildberger|Gerard Schildberger]] 08:36, 11 June 2012 (UTC)

----
With all respect to Gerard's work, I vote unambiguously YES for syntax colouring. LPEX, the live parsing editor (one of the first), is from Mike Cowlishaw, the maker of Rexx, and inspired by his work on the Oxford dictionary; there is objective evidence of the advantages of the use of colour. I use Emacs modes for NetRexx and ooRexx that closely follow the LPEX standards, including italicised comments.

: As I understood LEXX (changed, see :: comment below), it was designed for use on the IBM 3279 terminal (which supported color, and most 3279s also supported graphics), but it also supported multiple fonts.  It was this multiple font that made it possible to show (among other characters) the schwa and other upside-down characters, bold and/or italic characters, digraph characters, accented characters, and the like.  If anybody ever scrolled though a good/huge (printed) dictionary, there was a large use of such symbols, and it made sense to show the those same symbols (glyphs) on a terminal screen for the people maintaining that dictionary.  It was the only IBM terminal (that I knew of) at that time which could represent those special characters. [There must have been other special terminals that could support those functions.] Color was there on the 3279 terminal, so it was taken advantage of.  That it used color doesn't make it objective evidence of the advantages of color.  That is displayed what the printed dictionary contained was of paramount importance.  If color was that important, the dictionary could've been printed in color.  I remember a company that I worked for which got a slough (ok, a mere truckload of 64) of IBM (color) 3279 terminals (free!  --- did I mention free?) with a highlighting editor, and almost all the programmers didn't use the highlighting --- (most said it was too distracting from editing the source code they already knew).  I believe the test study was limited to just programmers.  But the color was nice in XEDIT as it allowed users to specify what colors to use for various parts of the screen (of a file being edited) such as the current line, tab line, excluded line(s), ID line, command line, prefix area, the data area (the source code or whatever was being edited), the MSGline(s) [which normally displayed the error or warning messages], and other such areas.  XEDIT didn't force the coloring on the user, it was a choice.  And having a truckload of free terminals was good business for the company, and a glowing report was giving to IBM how great color was, and then we got the terminals for keeps.  This was no small amount of coin here.  A most thoroughly unbiased opinion if I ever heard one.  If somebody give me a state-of-the-art $2200 terminal today (for free!)  --- that what was the price of a terminal in ''those'' days if you bought it, I'd probably be tempted to say that italized comments are the cat's pajamas, the bee's knees (--- and then wouldn't use it).  But fortunately, I already have three of those, so I can complain about the italized uglyness. I wish the highlighting would highlight '''syntax''', not keywords, literals, comments, etc. I also wish for world peace, and a bigger, er ..., terminal. -- [[User:Gerard Schildberger|Gerard Schildberger]] 20:39, 24 June 2012 (UTC)  

: I fondly remember the 3279. It was LEXX, not LPEX, that was designed for those. LPEX was a PC product that had its inspiration from LEXX. By the time, free 3279 terminals could not influence the decision to like it.--[[User:rvjansen|Rene Jansen]] 15:21, 25 June 2012

:: Thanks for the correction.  I changed my reference from LPEX to LEXX. I know nothing of LPEX, I only reported on my exposure to LEXX in the early 1980s. What (or how) are the decendants of LEXX & LPEX doing today?  I just looked at examples of LPEX and it appears to be a bit involved and requires some investment to learn. -- [[User:Gerard Schildberger|Gerard Schildberger]] 17:08, 25 June 2012 (UTC)

:::I have been told that a Java version of LPEX made its way into the Visual Age products, and a descendant of that is now to be found in Eclipse as the Eclipse editor. As I was not involved in any of this, it would be nice to find out for sure. I am using Emacs since losing X2 with the demise of OS/2 - although I have it back running on eComstation. To be honest - I never switch on syntax coloring in ISPF, which is the only environment where I find it to be distracting; it was also introduced around the time that I stopped doing mainframe work exclusively, so I never got used to it. The 3270 fonts and colours have some peculiar connotations, like green for edit and cyan for browse - this also made me to wage wars on pc departments that forced emulators on us with the wrong shade of cyan. But for general Unix/Wintel/Mac work, there is no question that the colouring helps. I even like Bill Finlason's Eclipse editor for NetRexx - he has got the right colouring scheme out of the box. [[User:Rvjansen|rvjansen]]

As mentioned, while editing code, there is no colouring on Rosetta, so there cannot be distraction from that. Editing code for examples should be in each author's personal environment anyway. Syntax colouring increases the status of a language - it is a fact that the wider known/used languages have colouring, the lesser known/used have not. This argument extends to PL/I which influenced Rexx and also deserves the same treatment - it seems unjustly devaluated by being shown in plain black-on-white.

On a typograhical note: if italics are bothering, and sometimes they do bother me also, most of the time the quality of the font is bad - some just use transformations of the roman glyphs and not separate italic forms, as they should. In particular, older windows versions are terrible. Changing the default font might help here. I find having the comments in italics, and preferably sea-green, isolates them in the right way from code and does not force me to re-read the comments all the time.
--[[User:rvjansen|Rene Jansen]]
----
I vote YES for syntax colouring as I've never seen anything else that makes code easier readable and in consequence more comprehensible especially to others. At this level I would describe syntax colouring as a no-brainer. 
I understand that someone feels no need to use syntax highlighting for himself, but it should be clear that he'll do a favour to others by providing it. --[[User:DMacho|D.Macho]] 11:29, 11 June 2012 (UTC)

: Yes, provide it as an option (or even as the default), but don't force the user to use it or provide no means to toggle it; the example shown below not withstanding.   

I'm not voting, but if color highlight is really undesirable, here's a compromise: go to your RC preference page, select "Appearance", then "custom javascript", edit and add the following:
```javascript

(function(){
	function get_code_pres() {
		var pres = document.getElementsByTagName('pre');
		var codes = [];
		for(var i = 0; i < pres.length; i++)
			if(pres[i].className.match(/\bhighlighted_source\b/))
				codes.push(pres[i]);
		return codes;
	}

	function toggle_highlight(pre) {
		if(pre._alt_html == null) {
			pre._alt_html = pre.innerHTML;
			var spans = pre.getElementsByTagName('span');
			for (var i = 0; i < spans.length; i++)
				spans[i].className = '';
		} else {
			var tmp=pre.innerHTML;
			pre.innerHTML=pre._alt_html;
			pre._alt_html=tmp;
		}
	}
		
	function add_toggle_links() {
		var codes = get_code_pres();
		for(var i = 0; i < codes.length;i++) {
			var a=document.createElement('a');
			a.textContent='Toggle syntax highlighting';
			a.style.cursor = 'pointer';

			(function(e) {
				a.addEventListener('click', function(){ toggle_highlight(e); }, false);
				e.parentNode.insertBefore(a, e);
			})(codes[i]);

			// uncomment next line to have blocks unhighlighted as soon as page loads
			// toggle_highlight(codes[i]);
		}
	}

	add_toggle_links();
})();
```
 which will <s>format your hard drive</s> add a link before each code block to toggle highlights.  I only tested it on Firefox, though it should work with other browsers.  If you want to used it and find it lacking, let me know. --[[User:Ledrug|Ledrug]] 16:08, 12 June 2012 (UTC)

::: Also, see my (below) version of the above custom javascript.   -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 23:38, 2 June 2017 (UTC)

Thanks.   When I uncommented that one line, it did exactly what I wanted, but still allowed me to use highlighting when I want it.

It receives a nag message when using I.E., however, but I use I.E. sparingly [I use FireFox Aurora almost exclusively].  

I would quote the script error, but I don't know nuttin' about nuttin' in Javascript, and it may just be something in my environment. 

I have since found that when browsing other webpages using I.E. (not just Rosetta Code), I'm receiving the same error and it's becoming a pita.  [Error: object doesn't support this property method; Code: 0; URL: http://www.imbd.com/]. Usually, two such errors are presented, and then I.E. proceeds to display the page ok.


I wish this code (as is) would be present without having to install in all my preferences, which would give the viewer a choice (and presumably, the default would be highlighting).  As someone else said, I don't like to have highlighting forced on my viewing of source code, having the toggle allows me a choice. -- [[User:Gerard Schildberger|Gerard Schildberger]] 20:31, 12 June 2012 (UTC)

: One thing I noticed, it sure has a number of chicken lips. --- Or is it chickenlips? -- [[User:Gerard Schildberger|Gerard Schildberger]] 20:35, 12 June 2012 (UTC)
:: (small fix in the code above--shouldn't have caused malfunction, though) I don't know much about IE, and frankly I don't want to care. As to "installing in all your preferences", putting it in the shared custom javascript alone should be enough--or so I thought, I'm no MediaWiki expert.  And, "chickenlips"?  That's a midwestern euphemism for somthing? --[[User:Ledrug|Ledrug]] 03:00, 13 June 2012 (UTC)

::: Oh, I'm sorry, I thought everybody knew what that is.  Chickenlips are the (curly) braces:  ''' { } ''' --- a somewhat pejorative and amusing term, I believe.   
----
I also vote YES for syntax coloring. As I'm new to Rexx it's fairly difficult for me to read the code without syntax highlights. On the other side I guess everybody agrees that Rexx is not the most widespread and most popular language around, so there are potentially more developers out there who are actually very interested in Rexx and who would need the highlighting as much as I do. --[[User:RolandR|RolandR]] 08:10, 13 June 2012 (UTC)
i also vote YES for syntax coloring; it's easier to read. --[[User:Glehm|Glehm]] 09:11, 15 June 2012 (UTC)

==custom JavaScript==
This is my version of the (above) JavaScript, the main difference is an inclusion of a solid separator (with a slightly different text)   between computer programming languages   (to aid in scrolling),   and it defaults to    ''no''   colorization.

I also tried to indent stuff to help me better understand the code logic and flow.

```Javascript
(function(){
  	    function get_code_pres() {
		                      var pres = document.getElementsByTagName('pre');
		                      var codes = [];
		                      for(var i=0;i<pres.length;i++)
                                             if(pres[i].className.match(/\bhighlighted_source\b/)) codes.push(pres[i]);
		                      return codes;
	                             }

 	    function toggle_highlight(pre) {
		                            if(pre._alt_html == null) {
		                	                               pre._alt_html = pre.innerHTML;
		                 	                               var spans = pre.getElementsByTagName('span');
		                 	                               for(var i=0;i<spans.length;i++) spans[i].className = '';
		                                                      }
                                                                 else {var z=pre.innerHTML;
			                                               pre.innerHTML=pre._alt_html;
			                                               pre._alt_html=z;
           }
	                                   }

 	    function add_toggle_links() {
                                                         // the bar below has  66  'db'x  characters.
                                         var xdb = '████████████████████████████████████████████████████████████████'; 
                                         var bar = xdb.concat(' «toggle highlighting»');
		                         var codes = get_code_pres();
		                         for(var i=0;i<codes.length;i++) {
			                                                  var a=document.createElement('a');
		                                                          a.textContent  = bar;
 		                                                          a.style.cursor = 'pointer';
 			                 (function(e) {a.addEventListener('click', function(){ toggle_highlight(e); }, false);
			                 e.parentNode.insertBefore(a, e);
		    	                                                 } ) (codes[i]);
 	         	                                                        toggle_highlight(codes[i]);
		                                                         }
	                                }

 	    add_toggle_links();
})();
```


I wish the   ''choice''    of colorization would be made available to all users without each user to have to enter a JavaScript program.   The above JavaScript version assumes the choice of '''off''',   but that's my choice.   -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 23:38, 2 June 2017 (UTC)

Also, with large Rosetta Code tasks  (that is, which large programs and/or a large number of programming entries),   it's faster   ("loading"/display the page).   [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 23:43, 2 June 2017 (UTC)



-----


