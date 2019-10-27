+++
title = "Talk:CRC-32"
description = ""
date = 2014-10-03T18:01:28Z
aliases = []
[extra]
id = 10967
[taxonomies]
categories = []
tags = []
+++

==Task goal==
Hi, What do you want the task to be about? --[[User:Paddy3118|Paddy3118]] 17:45, 29 November 2011 (UTC)
: The questions that come to my mind...Which polynominal? Is this in pursuit of a particular protocol's check? (i.e. ethernet frame checksums, ZIP file stream checksums, etc). "CRC" isn't any more specific than, say, "draw a shape".--[[User:Short Circuit|Michael Mol]] 18:02, 29 November 2011 (UTC)
:: I would like a task that calculates the ubiquitous CRC-32 as in RFC 1952 for GZIP. (The RFC contains an algorithm in C code, and refers to section 8.1.1.6.2 "32-bit frame check sequence" from [http://www.itu.int/rec/T-REC-V.42-200203-I/en ITU-T Rec. V.42].) However, I am not author of this draft task. --[[User:Kernigh|Kernigh]] 21:13, 29 November 2011 (UTC)
::: Given that there are language (and even CPU) accelerators for different common cases of CRC32, I'd say go ahead and create a separate task for each form of interest. Probably use a naming scheme of "CRC32/$name", where $name would be the common name for the CRC32 variant. [[CRC32/CRC32c]], for example. I realize this would lead to a small explosion in tasks which would be mundane and uninteresting after the second or so variation, but that leaves low-hanging fruit for whoever comes along later. --[[User:Short Circuit|Michael Mol]] 15:27, 30 November 2011 (UTC)
::Isn't it possible to write a program that calculates an N-bit CRC of a string with polynomial P ? --[[User:Spekkio|Spekkio]] 21:14, 29 November 2011 (UTC)

Hmm, ok I see. There are many types of algoritms that can be called a CRC. I think the most common CRC algoritm would be the CRC-32 used by IEEE 802.3, also mentioned by [[User:Kernigh|Kernigh]]. But it would also be interesting to make CRC-8,CRC-16 etc... --[[User:Spekkio|Spekkio]] 07:47, 30 November 2011 (UTC)
: Maybe (I don't know), and perhaps its of theoretical interest, but who wants to use a generic algorithm? CRC is used specifically because it's a fast, low-cost way to generate a reasonable checksum. --[[User:Short Circuit|Michael Mol]] 15:27, 30 November 2011 (UTC)
:: I'm not really sure what you mean? Why use a different kind of CRC algoritm that is not a standard? I have seen different kinds of CRC algoritms, like in Modbus, though Modbus is old.--[[User:Spekkio|Spekkio]] 16:09, 30 November 2011 (UTC)
::: I didn't mean to suggest that you'd want to use a CEC algorithm that wasn't either de facto or de jure standard. --[[User:Short Circuit|Michael Mol]] 18:15, 30 November 2011 (UTC)
::Why use a standard CRC algoritm, I think to reduce confusion :) --[[User:Spekkio|Spekkio]] 16:09, 30 November 2011 (UTC)
::: Certainly, but there are many meaningful, standard CRC algorithms. I noted above that it might be reasonable to implement several of them as different tasks. --[[User:Short Circuit|Michael Mol]] 18:15, 30 November 2011 (UTC)
::Or do you mean why make a function that can calculate N bit CRC for poly P? Maybe it would be interesting to have such a function if an MCU  is communicating with several devices that uses different polynomials and different bit length CRC, maybe it could save space, but it would be slower --[[User:Spekkio|Spekkio]] 16:09, 30 November 2011 (UTC)
::: A generic function, while theoretically interesting (and <s>probably</s> certainly worthwhile as a generic, illustrative case) is not something you'd see in a practical setting. Practical settings tend to call for a great deal of speed, and thus rely on optimized or specially-provided forms. As a consequence, languages like PHP provide a [http://www.php.net/manual/en/function.crc32.php crc32 function], and Intel is even including a CPU instruction for accelerating CRC32 with their Nehalem architecture. These aren't likely to be able to be illustrated with a good generic function, but they're going to be the best way forward for some CRC32s. That's why I noted different tasks above. --[[User:Short Circuit|Michael Mol]] 18:15, 30 November 2011 (UTC)
:::: That would have been the basic argument behind the splitting of the  [[MD5]] and [[MD5/Implementation]] tasks. The other was that for the complexity of MD5 it seemed those calling a library function got an easy pass.  Although in this case CRC32 is less complex so the two tasks could live in one.  The other argument for implementation is also practical, in low use situations a generic works just fine - especially given that there may not be a library version available. --[[User:Dgamey|Dgamey]] 13:44, 7 December 2011 (UTC)
::::: Now there's an old thread! As far as CRC32 vs MD5 sums go, CRC32 adds a complication...there are several different variations of CRC32, whereas there is only ''one'' md5sum. Spekkio's generic function would be akin to [[MD5/Implementation]] in that it's almost necessarily a pure-language implementation of the algorithm. As an aside...anyone have any idea which polynominal PHP's crc32 function uses? It relies on [http://svn.php.net/viewvc/php/php-src/trunk/ext/hash/php_hash_crc32_tables.h?view=markup these tables], but there's no documentation on where the tables came from. --[[User:Short Circuit|Michael Mol]] 15:04, 7 December 2011 (UTC)
:::::: No idea.  It bears no obvious relationship with the cited WP articles.  I thought he the intended algorithm was specified by <math>x^{32} + x^{26} + x^{23} + x^{22} + x^{16} + x^{12} + x^{11} + x^{10} + x^8 + x^7 + x^5 + x^4 + x^2 + x + 1</math> --[[User:Dgamey|Dgamey]] 03:50, 8 December 2011 (UTC)
:::::: Maybe related to [http://www.w3.org/TR/PNG/#D-CRCAppendix]
::::::: I don't know about the one named crc32_table, but the one named crc32b_table is the polynomial used in zip files. I verified this by comparing with the tables in the info-zip source. The values in php are endian-swapped compared to the info-zip table, but that could just be due to operating in the opposite direction. --[[User:Coderjoe|Coderjoe]] 08:31, 8 December 2011 (UTC)
:::::::: oops. I just realized I was looking at the big-endian table in info-zip. after checking the little endian table, it matches the php table named crc32b_table. --[[User:Coderjoe|Coderjoe]] 08:35, 8 December 2011 (UTC)


### Exact task requirement

We still don't have an answer to the question "What do we need to do to fulfill this task?". How about stating:
: ''"Implement the CRC commonly known as ... whose equation is ... . Use it to generate a checksum for the following bytes ..."''
--[[User:Paddy3118|Paddy3118]] 05:39, 1 December 2011 (UTC)
:: Why is it important to define a string of bytes to use, it's not in the [[MD5]] task description for example ? --[[User:Spekkio|Spekkio]] 09:59, 2 December 2011 (UTC)

:::All the language examples have to work with the same input leading to greater uniformity of purpose and hopefully comparability in resultant code. As soon as one example gives a checksum, other examples can compare their checksums. --[[User:Paddy3118|Paddy3118]] 10:55, 2 December 2011 (UTC)
::::Yes I understand, but what is meant was it is not in the task description of MD5 for example, or is it just implied? --[[User:Spekkio|Spekkio]] 13:25, 2 December 2011 (UTC)
::::: I can't answer that.  But it is in the [[MD5/Implementation]] which is closer in principle to this task.  [[MD5]] frequently just calls libraries that have been validated elsewhere. --[[User:Dgamey|Dgamey]] 13:18, 7 December 2011 (UTC)


### Use a library?

Is using a built-in implementation acceptable? Or a library? –[[User:Dkf|Donal Fellows]] 15:17, 6 December 2011 (UTC)
:Given some of the above discussion (for example: "A generic function, while theoretically interesting...") I assumed that using a built-in was desirable.  --[[User:Rdm|Rdm]] 15:38, 6 December 2011 (UTC)
:: I don't think this is clear.  See above discussion comparing with [[MD5/Implementation]] and  [[MD5]] tasks. --[[User:Dgamey|Dgamey]] 03:50, 8 December 2011 (UTC)

::: To me, when he said "implement the Cryclic <tt> ... </tt> Algorithms are described <tt> ... </tt>" meant to write a program to implement it, as described in the Wiki doc. Just calling a function so show what it returns isn't interesting, just mundane. -- [[User:Gerard Schildberger|Gerard Schildberger]] 00:55, 31 March 2012 (UTC) 

:: I don't think using a built in function is very interesting --[[User:Spekkio|Spekkio]] 12:15, 8 December 2011 (UTC)
::: Nor do I. Also it's trivial. --[[User:Dgamey|Dgamey]] 12:32, 8 December 2011 (UTC)
:::: So? Some people round here seem to think that doing everything with a hair shirt on is fine, but in reality it is the people who use the built-in functions and well-known libraries that will really prosper. In particular, they are working ''idiomatically'' with their language. If there is anything that people should be doing when posting solutions here, it is ensuring that those solutions are — to the greatest extent practicable — idiomatic and easy to read. After all, that is how to best encourage the use of the language and to compare and contrast. Moreover, some languages really do slam-dunk their way through tasks that others find exceptionally challenging (and for many reasons). ''That'' is interesting, bit manipulations… not so much. –[[User:Dkf|Donal Fellows]] 15:04, 8 December 2011 (UTC)
::::: Mostly concur with Donal, except that I do think seeing bit-twiddling solutions useful as well; it's best to use a language's idiomatic solution, but it's almost as important, in my opinion, to have an understanding of how it ''works.'' (Pragmatic vs theoretical arguments are abound, but it's often pragmatic to have an understanding of the theory) I'll retreat on my original recommendation of having polynomial-specific pages, though, and just suggest that different polynomials beyond the most common be optional entries; if a language has a shortcut for CRC32 of a particular polynomial, then that shortcut should be included as an additional solution. --[[User:Short Circuit|Michael Mol]] 15:28, 8 December 2011 (UTC)
:::::: (responding to [[User:Dkf|Donal Fellows]]) -- I agree, you should use built-in functions. But they do not always exist, for example when using the HI-TECH C-compiler or Microchips C compilers. And if I would use a built in function, it would produce unessecary large code. It would be better if you had a good understanding of the function and make your own. We're programming in different enviroments. --[[User:Spekkio|Spekkio]] 19:14, 8 December 2011 (UTC)
::::::: And if I started writing my own from scratch it would produce unreasonably large (and slow) code. Delegating to a well-known library (which might or might not use special system instructions) is good. Most programmers — especially most on this site — don't use those compilers you mentioned; having a basic implementation is acceptable as a possible method, but it should not be the ''required'' method for something as well known as CRC32 which has many library implementations already. –[[User:Dkf|Donal Fellows]] 08:43, 9 December 2011 (UTC)
:::::::: I do not agree, I rewrite the CRC function every time. Sometimes it just needs to compute a CRC8 for 2 bytes. I feel this is not a good place for electronic engineers :) --[[User:Spekkio|Spekkio]] 08:55, 9 December 2011 (UTC)
:::::: (responding to [[User:Short Circuit|Michael Mol]]) -- Or if the language has a shortcut for CRC polynomials in general... (The implementation I posted uses a library routine which currently supports running any 33 bit polynomial, where the leading bit is set, against a sequence of character literals.) --[[User:Rdm|Rdm]] 19:16, 8 December 2011 (UTC)
:::: (responding to Donal) Don't take my opinion that calling a library routine isn't useful. I wasn't griping. I was just commenting that the task is less interesting.  Again, my real point was that the task originally wasn't all that clear on that (and a number of other points).  Hence my comments on library versus implementation tasks.  If the task(s) are clear then these debates become far simpler and people get less caught up.  In my view BOTH types of tasks are useful.  One to show how to get the specific job done.  The other shows how to do it in detail and by extension how to do something like it that isn't in a library somewhere. In my case there was no crc32 for my language that I known of so I needed to write one. The folks caught in this position had a small challenge digging through the references to find the right spec.  --[[User:Dgamey|Dgamey]] 13:41, 9 December 2011 (UTC)

== Error correction ==
I work alot with different microcontrollers and communication with differnt devices, and everytime I get in contact with a CRC it takes some time to get it working. Sometimes it's CRC-8 or CRC-15 or something else, I've never really got a good grip of CRC. And I have never programmed a function that corrects an error using CRC, from what I understand this is possible? --[[User:Spekkio|Spekkio]] 14:48, 8 December 2011 (UTC)
: No. CRC is for error ''detection'', not error correction. You'd be more interested in [[wp:Hamming code|Hamming code]]. We have [[Hamming numbers]], but I don't think they're related. (If they are, it's not clear how, at least not on RC.) --[[User:Short Circuit|Michael Mol]] 15:18, 8 December 2011 (UTC)
:: I suggest something based on [[wp:Low-density parity-check code|Gallager code]]s would be appropriately beefy, as those are used in very high performance applications like transmissions from satellites. If that doesn't satisfy you, Spekkio, I don't know what will. :-) Different task though. –[[User:Dkf|Donal Fellows]] 17:27, 11 December 2011 (UTC)
::: Thanks, that is very interesting :D --[[User:Spekkio|Spekkio]] 10:02, 12 December 2011 (UTC)
:::: Hamming numbers are the number of inverted bits in a row in the message, plus one in the checksum code, that can go wrong before it can fail to detect the error. So if you had a hamming distance of 6, as in the CRC-15 used by CANbus, then you would have up to five bits in a row that could be wrong, along with one wrong in the checksum itsself. So CANCRC15 has a hammin distance of 6. This means its very good at getting rid of electrical interference on a line: i.e. a burst error of 5 bits or less will be detected. Look up papers by Cooperman for a math description, and lots of deifferent polynomials for CRCs. [[User:Robin48gx|Robin48gx]] ([[User talk:Robin48gx|talk]]) 13:28, 23 September 2014 (UTC)

== CRC library ported to several languages ==

I modified the CRC library from Lammert Bies (http://www.lammertbies.nl) to add many 8,16,24,32,40 and 64-bit CRC algorithms.
I translated them in several computer languages. I put them at SourceForge:
http://sourceforge.net/projects/crc/

== REXX ==

added Numeric Digits 10 to avoid this error (on ooRexx):

```txt

    17 *-*   say 'hex CRC-32 checksum =' c2x(checksum) left('',15),    "dec CRC-32 checksum =" c2d(checksum)     /*show CRC-32 in hex & dec*/
     6 *-* call show a_string
Error 93 running H:\crc32a.rex line 17:  Incorrect call to method
Error 93.936:  C2D result is not a valid whole number with NUMERIC DIGITS 9

```

---[[User:Walterpachl|Walterpachl]] ([[User talk:Walterpachl|talk]]) 19:55, 17 August 2013 (UTC)
: Question: Is this an 'extension' introduced by ooRexx?

```txt

cd2: Returns the decimal value of the binary representation of string. If the result cannot be expressed as a
whole number, an error results. That is, the result must not have more digits than the current setting of
NUMERIC DIGITS

```


:: Is the above a cut 'n paste from a document, or is the '''cd2''' a mere typo? -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 19:02, 18 August 2013 (UTC) 

I would think that Rexx on VM or TSO would fail as well but have no longer access to try. Anyone else?? --[[User:Walterpachl|Walterpachl]] ([[User talk:Walterpachl|talk]]) 05:09, 18 August 2013 (UTC)
:: Could you please run this test on your Rexx(es)? 

```rexx
/* REXX ***************************************************************
* 18.08.2013 Walter Pachl Test c2d behavior
**********************************************************************/
Parse Version v
Say v
cnt.=0
c=d2c(999999999)
id='001'
If c2x(c)='3B9AC9FF' Then
  Call cnt 'ok'
d=c2d(c)
Signal on Syntax
id='S001'
d1=d2c(d+1)
cnt.0err=cnt.0err+1
BS001:
Signal on Syntax
id='002'
rec=c2d('ffffffff'x)
B002:
id='003'
Numeric Digits 10
say c2d('ffffffff'x)
B003:
id='004'
Numeric Digits 20
say c2d('ffffffffffff'x)
B004:
Say cnt.0ok 'Tests ok'
Say cnt.0err 'Tests failed'
Exit

cnt:
  Parse Arg x
  If x='ok' Then
    cnt.0ok=cnt.0ok+1
  Else Do
    cnt.0err=cnt.0err+1
    Say 'Test' id 'failed'
    End
  Return

syntax:
If left(id,1)='S' Then
  Call cnt 'ok'
Else
  Call cnt id
Signal value('B'id)
```

--[[User:Walterpachl|Walterpachl]] ([[User talk:Walterpachl|talk]]) 05:53, 18 August 2013 (UTC)

:: Please note that the   '''signal value(...'''   isn't a valid construct from some older REXXes,   so PC/REXX, Personal REXX, and KEXX can't be used for the above REXX program. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 19:10, 18 August 2013 (UTC)

-----

I don't quite understand the reasoning/logic behind the test program:
: if the program gets a SYNTAX error ''anywhere'', it counts as OK.
:: not anymore --[[User:Walterpachl|Walterpachl]] ([[User talk:Walterpachl|talk]]) 19:27, 18 August 2013 (UTC)
: if the program successfully does the two '''D2C'''s, it counts as an error in one case, but not the other.
: if the program successfully does the 2nd '''D2C''', it is counted as an error.
:: that's because it should not do it successfully --[[User:Walterpachl|Walterpachl]] ([[User talk:Walterpachl|talk]]) 19:27, 18 August 2013 (UTC)
: no check is made to verify that the value of '''D''' matches the value of the 1st argument (999999999).
:: the result is compared with the value that should be there --[[User:Walterpachl|Walterpachl]] ([[User talk:Walterpachl|talk]]) 19:27, 18 August 2013 (UTC)
A     '''parse version x; say x'''     was added (in my version) after the 1st statement.

A     '''say 'digits=' digits()'''     would be a nice addition.


But in any case   (all were executed under a Windows/XP DOS window (cmd.exe):


'''output''' from Regina   (including 3.3, 3.4, 3.5, 3.6, 3.7):

```txt

REXX-Regina_3.7(MT) 5.00 14 Oct 2012
1 Tests ok
1 Tests failed

```

'''output''' from Personal REXX:

```txt

REXX/Personal 4.00 21 Mar 1992
2 Tests ok
0 Tests failed

```

'''output''' from R4:

```txt

REXX-r4 4.00 27 Jan 2013
2 Tests ok
0 Tests failed

```

'''output''' from ROO:

```txt

REXX-roo 4.00 28 Jan 2007 
Error 40 : Incorrect call to routine (OFF)
Information: D2C argument 1 is invalid. Whole number is required.
Argument value: 1.00000000E+9

Error occurred in statement# 9
Statement source: d1= d2c(d+1)
Statement context: D:\crud0.rex, procedure: badwalter

```

Differentially, different REXXes which are different are behaving differently (or indifferently?). -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 08:26, 18 August 2013 (UTC)

-----
:: Thanks for testing. The 'test case' was not for any program. (I tested the Rexx compiler for VM and then TSO and used such a framework for all programs. Anyway, you see that not only ooRexx does follow the specs (Regina doesn't!?!) - the "obnoxious" statement is therefore not just for ooRexx. right? If you use primarily Regina you won't notice this "problem" right?? --[[User:Walterpachl|Walterpachl]] ([[User talk:Walterpachl|talk]]) 08:54, 18 August 2013 (UTC)

::: The word I used was ''innoxious'', not ''obnoxious''.   Also, I believe it's considered bad form to change people's quotes (I don't mind the correcting of an obvious misspelling, but not the content) --- especially on the discussion (talk) page.   The reason I had the various flavors of Regina listed was that some Regina versions were considering numbers as integers that may or may not be considered integers,   i.e.:   -.1e7   and   12345678912345   for instance.   I don't know where Regina REXX currently stands on this.   There was some discussion of this topic in the newsgroup '''comp.lang.rexx''' a few months (or maybe years) back; it had to do with what a user expected (REXX's principle of least astonishment) versus what the REXX standard stated   (in both examples, both are clearly integers, but maybe not so much according to '''numeric digits'''.)   I used the word ''integers'' instead of ''whole numbers'' 'cause some people where saying (in the newsgroup) that negative numbers weren't whole numbers, and some also stated that zero isn't a whole number.   Sheesh!   ''Natural numbers'' and ''counting numbers'' were also thrown around rather loosely.   It was a lively and animated discussion as I recall.   Walter, if you're going to add your comments interspersed within my signed comments, please sign your inserted statements.   It appears that I'm contradicting (my bullet points) with myself and it's not clear at all who is stating what.   Also, now that the REXX program has changed drastically, my (REXX) output no longer matches anything posted, so the results are meaningless.   That's what programming versions are for (version 1, my output; version 2, ...). -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 18:52, 18 August 2013 (UTC)

:::: sorry about *noxious (I did not know either word in my restricted English. I tried to respond with a better version. --[[User:Walterpachl|Walterpachl]] ([[User talk:Walterpachl|talk]]) 19:27, 18 August 2013 (UTC)

::: So Regina would not have passed my test suite. Time to ask for Regina's POV (which I will do when I am back home) --[[User:Walterpachl|Walterpachl]] ([[User talk:Walterpachl|talk]]) 09:09, 18 August 2013 (UTC)
:::: I forgot that I've entered a ticket about this in sourceforge on 12 July 2013. Alas no response there yet. Fixing may, however, break existing programs :-( --[[User:Walterpachl|Walterpachl]] ([[User talk:Walterpachl|talk]]) 18:08, 18 August 2013 (UTC)
::::: No wonder I "forgot". It was not me :-) 

```txt

bugs:#413] c2d should fail if result > numeric digits
Status: open
Created: Fri Jul 12, 2013 11:44 AM UTC by Julian Levens
Last Updated: Fri Jul 12, 2013 11:44 AM UTC
Owner: nobody

```

--[[User:Walterpachl|Walterpachl]] ([[User talk:Walterpachl|talk]]) 18:13, 18 August 2013 (UTC)

----- 

Perhaps this discussion would be better served where there're more (varied) REXX experts (and REXX authors) hanging around --- [such as the REXX newsgroup, '''comp.lang.rexx''']. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 19:00, 18 August 2013 (UTC)

:: the ticket raised by someone else should fit your request. Why can't you accept that Regina is wrong here (according to the standard?  According to your tests it's the only Rexx that fails to obey the spec! BTW: I entered a doc ticket for ooRexx now because there is a wrong sentence in the C2D description. --[[User:Walterpachl|Walterpachl]] ([[User talk:Walterpachl|talk]]) 19:27, 18 August 2013 (UTC)

::: Correction:   Regina (all five versions) was ''one'' of REXXes of the four that I tested;   two of the tested REXX implementations behaved differently, a small sampling. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 20:10, 18 August 2013 (UTC)

::: If you're addressing those statements/questions to me:   I don't think that Regina is wrong nor do I think that Regina is right.   I don't feel comfortable explaining my philosophy on why or why not I accept (the correctness) of certain REXX implementations --- especially on Rosetta Code, not many REXX authors (coders/writers of REXX interpreters) visit here, as far as I can discern.   In the case of Regina REXX, it has its own location for reporting (possible) bugs.   I know a lot of REXX authors read (subscribe to) the '''comp.lang.rexx''' newsgroup. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 20:10, 18 August 2013 (UTC)

:::: Only ONE Rexx (Regina) did not reject that c2d invocation. Can you please update this line

```rexx

numeric digits 10                      /*only needed for ooRexx.        */

```

to something like 

```rexx

numeric digits 10                      /*not needed for Regina.         */

```

"In the case of Regina REXX, it has its own location for reporting (possible) bugs."
It has been reported there (on sourceforge)! For other Rexxes there is nothing to report (except that Roo, I think it was, does not behave as the others. --[[User:Walterpachl|Walterpachl]] ([[User talk:Walterpachl|talk]]) 20:21, 18 August 2013 (UTC)

:: I changed the statement, but I didn't include the comment as Regina REXX may fix/change its behaviour, so the comment may be mute, incorrect, or misleading in the future. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 20:40, 18 August 2013 (UTC)

Thanks for the change. Matter closed with these final words from Regina:

```txt

[bugs:#413] c2d should fail if result > numeric digits
Status: closed-duplicate
Created: Fri Jul 12, 2013 11:44 AM UTC by Julian Levens
Last Updated: Mon Aug 19, 2013 12:53 AM UTC
Owner: Mark Hessling
Duplicate of [#208]. Due to potential serious breakage in existing Rexx programs,
the current default behaviour of Regina will remain;
 ie x2d('3b9aca00') will not raise an error.
However with Regina's OPTIONS STRICT_ANSI this error IS trapped.
Sent from sourceforge.net because you indicated interest in
https://sourceforge.net/p/regina-rexx/bugs/413/

```

Again learned something. --[[User:Walterpachl|Walterpachl]] ([[User talk:Walterpachl|talk]]) 05:18, 19 August 2013 (UTC)

== Size of example string ==
strlen("The quick brown fox jumps over the lazy dog") is 43.

Due to modern processors having specific hardware engines used to calculate CRC's that are 32 bit wide
at a time, such as the cortex M3 arm series, it would be better to have a test string that
is wholly divisible by 4. The current test string has 43 characters. adding a full stop to this
would give a strlen() of 44.

Also CRC-16 engines take 16 bit chunks at a time, and would not be compatible with the example string chosen.

strlen("The quick brown fox jumps over the lazy dog.") is 44.
 cprog out: crc32 of test string <The quick brown fox jumps over the lazy dog.>44 is 1368401385 519025E9
This agrees with feeding 11 32 bit words into the arm cortex CRC engine with it
set to CRC32 and seeded with ~0.

[[User:Robin48gx|Robin48gx]] ([[User talk:Robin48gx|talk]]) 10:47, 23 September 2014 (UTC)

:*The CRC-32 references given are defined on arbitrary sized strings, not limited to multiples of four.
:*Therefore, an implementation that only supported a multiple of four would not be a correct implementation of this task.
:*Performance issues are typically not the point of Rosetta Code examples. If an implementation is optimized for four byte multiples it's just an optimization irrelevant to the task itself.
:*The example input should therefore purposely ''not'' be a multiple of four.
::&mdash;[[User:dchapes|dchapes]] ([[User talk:dchapes|talk]] | [[Special:Contributions/dchapes|contribs]]) 13:50, 23 September 2014 (UTC)

::: Yes I take your points, but 32 and 64 bit words are the norm now, as is increasing integration of
::: common tasks (such as CRCs, multiple ADC readings, zero crossing for motor control). Making this string
::: a little longer makes it compatible with blinding fast hardware circuitry. I think its just keeping up with the times.
::: CRC-32 is often used to validate a block of data, not just transmission lines.:wq --[[User:Robin48gx|Robin48gx]] ([[User talk:Robin48gx|talk]])

::::Mind you, they are still shipping shed loads of 16 and 8 bit processors as they are smaller, cheaper and more power efficient ;-)
--[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 14:49, 3 October 2014 (UTC)

::::Robin48gx, what you say is completely irrelevant for all the reasons I gave. &mdash;[[User:dchapes|dchapes]] ([[User talk:dchapes|talk]] | [[Special:Contributions/dchapes|contribs]]) 18:01, 3 October 2014 (UTC)
