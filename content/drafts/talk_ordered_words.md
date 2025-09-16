+++
title = "Talk:Ordered words"
description = ""
date = 2019-06-07T19:18:06Z
aliases = []
[extra]
id = 8700
[taxonomies]
categories = []
tags = []
+++

==Lexicographical order==
Note: This task should probably be modified to be aware (and tolerant) of [[wp:Lexicographical order|lexicographical order]]. --[[User:Short Circuit|Michael Mol]] 22:15, 9 November 2010 (UTC)
: I think it's probably not important on that particular dictionary; it's all lower case and well-behaved. –[[User:Dkf|Donal Fellows]] 22:37, 9 November 2010 (UTC)

: Yes, I thought I had programmed (REXX) for an unlexicographical ordered dictionary, but it erronously gave me the correct result (because the list of words was in order).  Yes, that does sound strange.  The program has been fixed. -- [[User:Gerard Schildberger|Gerard Schildberger]] 06:28, 14 July 2012 (UTC)

::Indeed the task description should state either
::any lexicographical ordered dictionary such as ... (instead of 'this dictionary')
:::tasks should not be geared to one input, should they?
::or (better yet??) drop the requirement for it to be ordered.
Being a sloppy reader I used an unordered input for my test with ooRexx
and detected/reported the bug (which wasn't one according to the current task description)
Anyway: I think the program was improved and works now also for an unordered dictionary
and (slightly modified) for ooRexx.
--[[User:Walterpachl|Walterpachl]] 08:05, 14 July 2012 (UTC)

==longest word length==
By "longest word length", do you mean "equal to the length of the longest ordered word"? --[[User:Short Circuit|Michael Mol]] 22:16, 9 November 2010 (UTC)
:Yes. I find the ordered words, find the maximum length of any ordered word, then find all ordered words of that maximum length. --[[User:Paddy3118|Paddy3118]] 13:09, 10 November 2010 (UTC)
:: For contrast, the Tcl code does it in a single pass. (It happened to be more natural to express it that way.) The result is the same though; the words in the result list are such that they are all of the same length, all ordered words, and there is no other ordered word (in the originating dictionary) such that its length is greater than the length of any result word. (Also, every ordered word of that length in the originating dictionary is present in the result.) I can't be bothered to write that mathematically. :-) –[[User:Dkf|Donal Fellows]] 16:24, 10 November 2010 (UTC)

==Knotty problem==
I noticed three of the examples written by [[User talk:Ulrie]], C++, Perl and Perl 6; don't have the word knotty mentioned. I don't know if this is due to a faulty copy of the dictionary or a faulty algorithm. (Knotty is still in the [http://www.puzzlers.org/pub/wordlists/unixdict.txt dictionary]). I will give it a day then I think I should mark them incorrect? --[[User:Paddy3118|Paddy3118]] 12:05, 27 November 2010 (UTC)
:Or could their algorithms not cope with the double-t in knotty? (But note, The task description does state that the word 'abbey' with a double-b ''is'' to be considered ordered. --[[User:Paddy3118|Paddy3118]] 16:58, 27 November 2010 (UTC)
:: Is there a public-domain (or, GFDL1.2-compatible, at least) wordlist I could host and have dictionary-dependent tasks draw from? URL preferred; Places I'm looking have weird or tricky copyright restricitons. --[[User:Short Circuit|Michael Mol]] 18:18, 27 November 2010 (UTC)
::: [http://wordlist.sourceforge.net/scowl-readme This]? --[[User:Paddy3118|Paddy3118]] 06:02, 28 November 2010 (UTC)
:In the Perl code it was a faulty algorithm that hardwired the number of expected results!?!  I fixed the Perl and Perl 6, but someone else can fix the C++. --[[User:TimToady|TimToady]] 18:32, 27 November 2010 (UTC)
:::Thanks TimToady. --[[User:Paddy3118|Paddy3118]] 05:42, 28 November 2010 (UTC)

==Output==

Generally, tasks which are about writing programs (not describing language features) make requirements of the ''program''’s output, not the form of the example itself. If it is required the output of the program to be included in the example,
* then please clarify the task wording.
* why is it required?
A few output examples are useful in checking one's own work when adding implementations, but it seems silly to insist that every single program come with its output. I would like to understand the motivation here.
—[[User:Kevin Reid|Kevin Reid]] 18:36, 1 February 2011 (UTC)

:Hi Kevin, on other occasions, having output available has helped find and debug problems where output has been given and it was easier to spot something missing from the output and then look more closely at the implementation. I, and others do try and not insist on output if output will always be large. An easy case for insisting on output is something like [[Conway's Game of Life]] were output (of some sort) has been given. Come to think of it, it was [[Wireworld]] where odd output lead to fixing of an example.
:In other cases it aids consistency. If output is needed from one, to allow others to complete the task, then asking for output from all is consistent. The requst for output in this task should be straight-forward to do.

:Different tasks get patrolled and "policed" to different degrees, but I'm sure their is no underlying malice to a request to add output. --[[User:Paddy3118|Paddy3118]] 19:03, 1 February 2011 (UTC)


### E and Factor

Someone marked the E and Factor examples as incomplete/incorrect because they print no output. Yet both examples seem to have print statements (<tt>println(" ".rjoin(best.snapshot()))</tt> or <tt>filter-longest-words [ print ] each</tt>. Did someone add the print statements and forget to remove the incomplete/incorrect marks? --[[User:Kernigh|Kernigh]] 20:45, 9 March 2011 (UTC)

:If you compare those to others you should see that the output of those programs are not given, even though the task asks for it. One might argue that running the programs would give the display, but a quick reading of other examples should show that the output is required in addition to the program source. --[[User:Paddy3118|Paddy3118]] 21:06, 9 March 2011 (UTC)

Not sure about E, but I installed Factor and confirmed that the Factor example really outputs the answer. The task asks for the program to "display" the answer, but the task does not require anyone to post the answer on this page. --[[User:Kernigh|Kernigh]] 23:18, 9 March 2011 (UTC)
: Hi Kernigh, you seem to be ignoring both comments and the other examples - most of which give their output on the task page. (not this talk page), ... ...How should the task page be altered to ensure most people put the results on the page? Most of the current results comply; the task states that it should be displayed and displaying it elsewhere would not be of much help. --[[User:Paddy3118|Paddy3118]] 00:25, 10 March 2011 (UTC)

I am guessing that the program would display it on standard output, or in a GUI window, or something like that. That would allow the person who runs the program to see the answer. Or am I misunderstanding? --[[User:Kernigh|Kernigh]] 01:09, 10 March 2011 (UTC)
:We want it shown on this page. Just copy the output and paste it in a pre block under the example. --[[User:Mwn3d|Mwn3d]] 01:34, 10 March 2011 (UTC)

:I don't think that the 'on this page' comment is needed in the task description; most people would imply that. --[[User:Paddy3118|Paddy3118]] 17:49, 10 March 2011 (UTC)

Now that we fixed the task description, I agree that the output must be on the page. As of now, every example on the page has output on the page, except for E. Can someone run the E program and post the output? --[[User:Kernigh|Kernigh]] 02:24, 11 March 2011 (UTC)

== A bug (which was not really a bug) in Rexx solution ==
I suggest to change the last two lines of the Rexx program to

```rexx

say words(i.m) 'words found (of length' m")"; say     /*show & tell time*/
  do n=1 for words(i.m);  say word(i.m,n);  end       /*list the words. */

```

I added abcdefgh to the unixdict.txt and got 0 words of length 8.
And please add '' to i.= for compatibility.

--[[User:Walterpachl|Walterpachl]] 18:26, 13 July 2012 (UTC)

: Well, yes, you used the wrong index.  If I changed the code to what you want, the REXX program will no longer work correctly nor solve the task at hand. -- [[User:Gerard Schildberger|Gerard Schildberger]] 21:18, 13 July 2012 (UTC)
: And also, these comments shouldn't be placed under the '''E and Factor''' section. -- [[User:Gerard Schildberger|Gerard Schildberger]] 21:18, 13 July 2012 (UTC)
: Also, note that this program entry was entered for classic REXX, and is not meant for other object-oriented languages. -- [[User:Gerard Schildberger|Gerard Schildberger]] 21:18, 13 July 2012 (UTC)



Please try your program with this test file:

```txt

abcdef
abcde
abcd

```

Expected result: 1 word of length 5.

: My expectation is: 1 words of length 6.   [I didn't think it would be necessary to add a pluralizer to this program, but in hindsight, I should have.] -- [[User:Gerard Schildberger|Gerard Schildberger]] 06:42, 14 July 2012 (UTC)

Suggested code change under wrong section. sorry.
: "and is not meant for other object-oriented languages"
:: How can a program be meant to be...
:: When corrected as shown above it runs perfectly well with ooRexx
--[[User:Walterpachl|Walterpachl]] 05:34, 14 July 2012 (UTC)

::: I do not have an ooREXX (other than ROO!) available to check whether it runs under an object-oriented language (nor do I have an interest of doing so as I have often repeated), so I can't answer your query regarding ooRexx.  -- [[User:Gerard Schildberger|Gerard Schildberger]] 06:42, 14 July 2012 (UTC)

Looking at the program again I noticed two typos
EBCDICI should be EBCDIC
and then should be than (2 places)
I dare not change your program.

: The misspelling wasn't in the program, but the REXX language entry section header.   -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 23:06, 13 August 2016 (UTC)
: I had programmed the REXX example to expect a lexicographical ordered word list.  I corrected the error. -- [[User:Gerard Schildberger|Gerard Schildberger]] 06:42, 14 July 2012 (UTC)

:Thank you
:You don't have to have ooRexx. You COULD write i.='' instead of i.= to avoid the incompatibility
:(and the use of $#@ in symbols - which aren't used in this program)
:: and you didn't correct the 'thens' (is less then -> is less than) :-(
--[[User:Walterpachl|Walterpachl]] 07:49, 14 July 2012 (UTC)

Yes, I corrected ''then'' to ''than'' several updates ago.  You may have to do a '''RELOAD''' or '''REFRESH''' to see the updated version. -- [[User:Gerard Schildberger|Gerard Schildberger]] 07:57, 14 July 2012 (UTC)

:I think you have missed this one:
::In ASCII, A is less then a, while in EBCDIC, it's the other way around.
--[[User:Walterpachl|Walterpachl]] 08:15, 14 July 2012 (UTC)

::: Hell's bells, I must've read that line a half dozen times, and I still missed it!  It's been corrected, finally. -- [[User:Gerard Schildberger|Gerard Schildberger]] 08:21, 14 July 2012 (UTC)

And I'd appreciate i.='' instead of i= (as you adapted some other programs)

:Second thoughts about the task description: your mentioning of uppercase versus lowercase and
:letters of the English alphabet. Things get tricky with German Umlaute (äöüÄÖÜß) not to speak of Scandinavian alphabets
: of which I know nothing except that they are different.

One could ask for a list of characters showing the desired sort order (ol)
and turn a>=b into pos(a,ol)>=pos(b,ol).

--[[User:Walterpachl|Walterpachl]] 08:43, 14 July 2012 (UTC)
--[[User:Walterpachl|Walterpachl]] 08:43, 14 July 2012 (UTC)

:: The REXX language was always English-centric (well, Latin letter centric, as least).  The UPPER statement, function, and option was just designed for the Latin alphabet, and when porting REXX to be used with other alphabets becomes problematic and a subject worthy of a full discussion. [Note that some REXX support other languages for error messages. The names of the weekdays and months and various time suffixes are still in English, however, as well as the ''options'' of all functions.] The order of sorting characters is in itself, a field of study.  The order in which various hardware sorts characters (or put in order) is also of interest.  Some sort packages and other computer software allow for specifying (for instance) how to sort/order numeric digits: ASCII has them below letters, BCDIC and EBCDIC has them above. Also, ASCII has the uppercase Latin letters lower in a list, lowercase letters are higher. BCDIC and EBCDIC is the other way around. The German essett character ('''ß''') has two problems, it has no uppercase equivalent [other then SS], and in the German alphabet, the '''ß''' is normally listed after '''z'''. [This is due to the way it's pronounced.]  Using the '''translate''' and/or '''upper''' instruction/option/function to uppercase (verb) non-Latin letters quickly degenerates into one heck of a mess; how would a computer language know ''a prioi'' how (or what) to ''uppercase'' (or to ''lowercase'')?  It's a question that I'm not equipped to address. -- [[User:Gerard Schildberger|Gerard Schildberger]] 17:51, 14 July 2012 (UTC)

:::actually I know all of that. Whereas the Rexx language is English-centric one can still process German input
albeit not as simply as English. For example to uppercase a string.

```rexx

uppercase: Procedure
Parse Arg s
a2z='abcdefghijklmnopqrstuvwxyz'
r=translate(s,translate(a2z)'ÄÖÜ',a2z'äöü')
r=changestr('ß',r,'SS')
Return r

```

I know that lowercase ain't that easy!
By the way, did the Romans have lowercase letters (you mention that in Roman Number decoding)?
--[[User:Walterpachl|Walterpachl]] 19:01, 14 July 2012 (UTC)

: Yes, the Romans had lowercase letters. A papyri written in Latin from Herculaneum dating before 79 CE was found using lowercase letters '''a''', '''b''', '''d''', '''h''', '''p''', and '''r''' (as per Wikipedia).  At one point, when using lowercase Roman numerals, "they" started using a lowercase '''j''' instead of an '''i''' so the reader could more easily distinguish the end of the Roman numeral.  Think what would ''ci'', ''cii'', ''vi'' (Latin ablative singular of ''vis''), ''di'' (Latin irregular masculine plural of deus [diety]), ''lii'', ''mi'' (as in the 7-note muscial scale, from the 1st verse of the Latin hymm Ut queant laxis), ''xi'' (name of a Greek letter) ...  would look like in a Roman text.  Boy, this paragraph is a hodge-podge. It needs a kitchen sink thrown in. Maybe a tune-a-fish. Perhaps some (Roman) scholar could add a few words here --- and I don't mean any long-dead scholars. -- [[User:Gerard Schildberger|Gerard Schildberger]] 20:09, 14 July 2012 (UTC)


: By the way, the above REXX example could be re-written without the need for a PROCEDURE, a Latin alphabet literal string, or temporary variables (along with some appropriate comments explaining the three steps performed):

```rexx
uppercase: return translate(changestr("ß",translate(arg(1),'ÄÖÜ',"äöü"),'SS'))
```

Of course, if one wanted to break up the complex instruction, then a local (temporary) variable would be needed, along with a PROCEDURE statement.  But the Latin alphabet literal isn't needed. I wonder how those German characters would translate on various codepages. -- [[User:Gerard Schildberger|Gerard Schildberger]] 20:09, 14 July 2012 (UTC)

:::thanks for the Roman explanation
:::not only could, but can as you did. But there's nothing wrong with Procedure.
:::It makes it readable without comments, I think-
:::other code pages: AWFUL (even on my DOS Window or command prompt.
--[[User:Walterpachl|Walterpachl]] 03:58, 15 July 2012 (UTC)

thanks for the line. I'd settle for

```rexx

uppercase: Procedure
Parse Arg a
  a=translate(arg(1),'ÄÖÜ',"äöü")     /* translate lowercase umlaute */
  a=changestr("ß",a,'SS')             /* replace ß with SS           */
  return translate(a)                 /* translate lowercase letters */

```

 --[[User:Walterpachl|Walterpachl]] 19:26, 15 July 2012 (UTC)

The above REXX program could be shortened to:

```rexx
uppercase: Procedure
  a=translate(arg(1),'ÄÖÜ',"äöü")     /* translate lowercase umlaute */
  a=changestr("ß",a,'SS')             /* replace ß with SS           */
  return translate(a)                 /* translate lowercase letters */
```

(which removes a line of dead code.)


As for using '''Procedure''', it does come with some overhead.

I ran a benchmark with the original '''uppercase''' subroutine, and the second version, along with the above version and the one-liner version.

The original version was faster than version two (but only slightly), and the one-liner was about four times faster.  An in-line version was twice as fast as the one-liner.

Now, in this day and age of fast computers, some people don't care about speed that much.  I have two REXX applications, one that processes over 708,000 records (actually, words in an English word list which needs to be uppercased while doing a search), and that amount of invocations addes up.  Another application reads over 58 million records, and one can see that inefficient subroutines can really slow up the works.  I'd like to think that Rosetta Code is a place to show well-written routines that are applicable to any size/amount of use; one can never know where people will use such a routine (or the scale of use).  A REXX ''procedure'' has to build an environment which has its own NUMERIC DIGITS, FORM, and FUZZ, its own timing (elapsed and resetted timers), local REXX variables (RC, SIGL, RESULT), and whatnot. In the above case, one local variable ('''a''') also has to be DROPed.  There is always something to be paid (as far as overhead).  Once the '''uppercase''' becomes a one-liner, then it becomes available to be used as an in-line algorithm, bypassing the overhead of calling a subroutine (whether a procedure or not).  If anyone wants to see the benchmark REXX program, I can post it here. -- [[User:Gerard Schildberger|Gerard Schildberger]] 02:04, 16 July 2012 (UTC)

Then how about

```rexx

uppercase:
 return translate(changestr("ß",translate(arg(1),'ÄÖÜ',"äöü"),'SS'))
/**********************************************************************
  a=translate(arg(1),'ÄÖÜ',"äöü")     /* translate lowercase umlaute */
  a=changestr("ß",a,'SS')             /* replace ß with SS           */
  return translate(a)                 /* translate lowercase letters */
**********************************************************************/

```

As for me, I like to program for people (readingI with a limited line length.
:now I see what you mean by your recurring elimination of dead code
:which is actually avoiding empty lines created by the formatter and
:makes copy/paste harder for me.

:: No, I was referring to dead '''code''', not whitespace.  The code that I was referring to was the '''Parse Arg a''' REXX statement, which I see is no longer present in the above example, but it is back in the benchmark program. -- [[User:Gerard Schildberger|Gerard Schildberger]] 20:53, 16 July 2012 (UTC)

:: By the way, the above boxed REXX example for the '''uppercase''' subroutine/function is the best yet, a straight one-liner, albeit that it's really a two-liner. -- [[User:Gerard Schildberger|Gerard Schildberger]] 20:53, 16 July 2012 (UTC)

And finally, can you provide your benchmark results for the strict comparison case?
::--[[User:Walterpachl|Walterpachl]] 06:11, 16 July 2012 (UTC)

Interesting: for ooRexx it's a mere 30%

```rexx

Call time 'R'
Do i=1 to 10000000
x=uppercase('Wölter')
End
Say 'oneliner: ' i time('E')
Call time 'R'
Do i=1 to 10000000
x=uppercase2('Wölter')
End
Say 'Procedure:' I time('E')
Exit
uppercase:
 return translate(changestr("ß",translate(arg(1),'ÄÖÜ',"äöü"),'SS'))
uppercase2: Procedure
Parse Arg a
  a=translate(arg(1),'ÄÖÜ',"äöü")     /* translate lowercase umlaute */
  a=changestr("ß",a,'SS')             /* replace ß with SS           */
  return translate(a)                 /* translate lowercase letters */

```

 oneliner:  10000001 11.731000
 Procedure: 10000001 16.029000
:please post your benchmark
--[[User:Walterpachl|Walterpachl]] 06:45, 16 July 2012 (UTC)

-----

I can't speak for ooRexx as I don't have a copy to test it.

I don't like to use words like ''mere'' which preloads a judgement.
30% of an 20 hour run is an extra   <sup>'''1</sup>/<sub>4'''</sub>   day (this would be in regards to that 82 million record "database").

I took your program ''as is'' and ran it on my isolated computer (no internet connection, no active anti-virus protection programs running, etc, it's a 3.20 GHz box and is running all four processors with five 100%-CPU-bound unrelated programs on below-normal priority), and the results are:

```txt

 oneliner:  10000001 13.088000
 Procedure: 10000001 60.223000

```

Then, just to show what the REXX overhead is for processing a "normal" '''do''' loop, I replaced the

```rexx
do i=1 to 10000000
```

with

```rexx
i=10000000
do i
```

and the results are:

```txt

 oneliner:  10000001 13.412000
 Procedure: 10000001 60.061000

```

Considering that the bulk of the execution time is spent in the subroutines, it's noteworthy; the difference is the way REXX handles incrementing a '''do''' loop index (and testing for termination of same).


I then made the program compliant by adding a
<lang>/*REXX*/
```

statement to the front of the program.


Code was then added for:

* user-specifiable (number of) times to repeat the loop
* added "greasers" (have REXX allocate stuff so SUBs don't have to)
* force REXX interpreter to read entire file (mostly for older REXXes)
* a repetition of both invocations to eliminate snowplowing
* use of FOR instead of TO in DO loops for faster execution
* disallowing the caching effect for "small" loops
* made invocations unique by using unique passed arguments
* eliminating piggy-backing by not using the same variables
* show the version of the REXX interpreter


Here's the code that was used for the 3rd benchmark:

```rexx
/*REXX*/   parse version _;   say 'version:' _;   say
arg times .
if times=='' then times=10 * 1000000  /*ten million times, yikes !  */
call time 'R'                         /*just grease the wheels a bit*/
call uppercase; call uppercase2       /*force some REXXes to get 'em*/
x=0; y=0; j=0; k=0                    /*have REXX allocate variables*/

  do 2
         call time 'R'    /*────────────────────reset the REXX timer*/
           do j=1 for times
           x=uppercase(j 'w÷lter')
           end
         say 'oneliner: ' j time('e')

         call time 'R'    /*────────────────────reset the REXX timer*/
           do k=1 for times
           y=uppercase2(k 'w÷lter')
           end
         say 'procedure:' k time('e')
  end

exit

/*──────────────────────────────────UPPERCASE subroutine─────────────*/
uppercase:
 return translate(changestr("ß",translate(arg(1),'ÄÖÜ',"äöü"),'SS'))

/*──────────────────────────────────UPPERCASE2 subroutine────────────*/
uppercase2: procedure
parse arg a     /*<-------------------------------------- dead code. */
  a=translate(arg(1),'ÄÖÜ',"äöü")     /* translate lowercase umlaute */
  a=changestr("ß",a,'SS')             /* replace ß with SS           */
  return translate(a)                 /* translate lowercase letters */
```

and the results were:

```txt

version: REXX-Regina_3.6(MT) 5.00 31 Dec 2011

 oneliner:  10000001 15.605000
 Procedure: 10000001 63.023000
 oneliner:  10000001 15.420000
 Procedure: 10000001 63.106000

```


More work should be done on the benchmark REXX program(s), but there's only so much time in a day... -- [[User:Gerard Schildberger|Gerard Schildberger]] 20:53, 16 July 2012 (UTC)

 ... and it's not worth doing it
 ooRexx results:

```txt

version: REXX-ooRexx_4.0.1(MT) 6.03 2 May 2010
oneliner:  10000001 16.271000
procedure: 10000001 18.751000
oneliner:  10000001 15.881000
procedure: 10000001 18.767000

```

At to ''mere'': I'd assume that the program does some other stuff in them 20 hours, so the
20 % cost shown here would amount to how many minutes?
--[[User:Walterpachl|Walterpachl]] 05:28, 17 July 2012 (UTC)

-----

It's a moot point, as I '''cannot''' use ooRexx.  So, the question is, is the ''procedure'' version worth the four times the execution time in the REXX that I have to use?

The answer to your question is: no, anything more than twenty hours is too long, the run takes long enough as it is.

ooRexx consumes too much virtual storage (which is just one of my concerns), and the big classic REXX program is always bumping into the 2G limit (this is for Regina REXX).  What I mean is that the program frequently exhausts virtual memory and the run (solution) has to be managed in another way, essentially breaking up the many runs into more multiple runs, which is a major pita.  What I remember from 15 years ago, (IBM's o-o REXX for a big program consummed too much CPU for the this type of program (long running, lots of I/O, very big stemmed arrays) that I use.  "It" is two main programs, 3825 + 330 REXX statements, plus it makes use of other classic REXX programs. I have no desire to install ooRexx and then spend many hours reworking a bunch of classic REXX programs to work with ooRexx.

Another thing to compare would be a REXX program that runs under (say) Regina REXX, and compare it to running under ooRexx (on the same hardward and operating system, of course).  It would be an interesting comparison.  Since ooRexx was originally (I think) written (coded) by IBM, I assume it has pretty high standards.  I really don't know if IBM wrote the code or had it written elsewhere. -- [[User:Gerard Schildberger|Gerard Schildberger]] 05:58, 17 July 2012 (UTC)

-----

It 'classic' IBM Rexx and ooRexx was written by IBM (people) and the key person(s) are still here (in RexxLA).
High standards? Yes, I managed the testing of the Rexx compiler(s) on VM and then TSO
(one of my best lifetime projects).
--[[User:Walterpachl|Walterpachl]] 06:25, 17 July 2012 (UTC)

==REXX benchmarks==

These are the results for REXX exact vs. regular comparisons as per Walter's request.


I no longer have the original ''regular compare'' vs. ''exact compare'' REXX bench-marking programs,

but I took the (above) existing code and ripped its guts out (er, disemboweled it), and made a

simple benchmark test out of it.

I soon discovered that the two versions of the '''if''' statement was being dwarfed by the

overhead of the '''do''' loop, so I unrolled the '''if''' statements.

Just for grins, I reversed the order of the compares on every other compare, and I was

somewhat surprised that more CPU time was consumed.

I left that modification in the benchmark program.

I ran the REXX benchmark against the three classic REXX interpreters that I have

installed on my two computers, plus an o-o REXX interpreter:

::*   R4
::*   ROO
::*   Regina
::*   Personal REXX


```rexx
/*REXX*/   parse version _;   say 'version:' _;   say
arg times .
if times=='' then times=1000000       /*default is one-million times*/
call time 'R'                         /*just grease the wheels a bit*/
j=0; k=0; x=0; y=0                    /*have REXX allocate variables*/

  do 3
         call time 'R'    /*────────────────────reset the REXX timer*/
           do j=1 for times
           if _=j  then x=j
           if j=_  then x=j
           if _=j  then x=j
           if j=_  then x=j
           if _=j  then x=j
           if j=_  then x=j
           if _=j  then x=j
           if j=_  then x=j
           if _=j  then x=j
           if j=_  then x=j
           if _=j  then x=j
           if j=_  then x=j
           end
         say ' reg compare:' times "times" right(format(time('e'),,2),15)

         call time 'R'    /*────────────────────reset the REXX timer*/
           do k=1 for times
           if _==k then y=k
           if k==_ then y=k
           if _==k then y=k
           if k==_ then y=k
           if _==k then y=k
           if k==_ then y=k
           if _==k then y=k
           if k==_ then y=k
           if _==k then y=k
           if k==_ then y=k
           if _==k then y=k
           if k==_ then y=k
           end
         say 'Xact compare:' times "times" right(format(time('e'),,2),15)
         say
  end
```

Using the benchmark program (shown above), for both computers (one is running

Windows/XP pro, the other is running Windows 7), the results are:

```txt

* R4 ---------------  27% slower using regular comparisons
* ROO --------------  39% slower using regular comparisons
* Regina ----------- 150% slower using regular comparisons
* Personal REXX ---- 250% slower using regular comparisons

```


(In all of the above runs (about a half-dozen runs on each computer), I used the

lowest percentage found.)

```rexx
/*REXX*/   parse version _;   say 'version:' _;   say;   _=word(_,2)
arg times .
if times=='' then times=1000000       /*default is one-million times*/
call time 'R'                         /*just grease the wheels a bit*/
j=0; k=0; x=0; y=0; p=0; q=0          /*have REXX allocate variables*/

  do 3
         call time 'R'    /*────────────────────reset the REXX timer*/
           do j=1 for times
           p=_||j
           if p=j  then x=j
           if j=p  then x=j
           end
         say ' reg compare:' times "times" right(format(time('e'),,2),15)

         call time 'R'    /*────────────────────reset the REXX timer*/
           do k=1 for times
           q=_||j
           if q==k then y=k
           if k==q then y=k
           end
         say 'Xact compare:' times "times" right(format(time('e'),,2),15)
         say
  end
```

Using the benchmark program (shown above) and using the same methodology, the results are:

```txt

* R4 --------------- 115% slower using regular comparisons (115% --> 119%)
* ROO --------------  39% slower using regular comparisons  (39% -->  40%)
* Regina -----------  40% slower using regular comparisons  (40% -->  41%)
* Personal REXX ----  35% slower using regular comparisons  (35% -->  41%)

```


(In all of the above runs (again, about a half-dozen runs), I used the lowest percentage found, but I
included the ranges as well.)


Please note that these benchmark tests were of the "quick and dirty" type, and I didn't have the

time to spend on it as I would have wished. I spent (probably) way too much time on this simple case.

This is to say, your mileage may vary.  I hope others will execute these two REXX benchmark programs

for other REXXes (or the same ones on other operating systems, other hardware).

I tried to include the usual methodologies to minimize background noise and overhead interference.

As with most benchmarks, I often feel that I'm leading a horse to water ...

 -- [[User:Gerard Schildberger|Gerard Schildberger]] 23:32, 16 July 2012 (UTC)


P.S.:   I benchmarked the programs on an air-gap computer.   -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 07:02, 15 August 2018 (UTC)

-----

ooRexx Results for the above 2 programs thar surprised me (a little):

```txt

version: REXX-ooRexx_4.0.1(MT) 6.03 2 May 2010
 reg compare: 1000000 times            1.33
Xact compare: 1000000 times            1.11

version: REXX-ooRexx_4.0.1(MT) 6.03 2 May 2010
 reg compare: 1000000 times            1.09
Xact compare: 1000000 times            0.53

```

--[[User:Walterpachl|Walterpachl]] 05:16, 17 July 2012 (UTC)

==Ruby Golfing?==
It looks as if the last Ruby example is just a "Code golf" solution and is not idiomatic Ruby. If so then it probably shouldn't be on RC.

What do you think?

--[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 09:08, 14 August 2015 (UTC)

It's pretty idiomatic (would add a couple more spaces and use full words instead of chars for variables), but I agree that it ventures closer to that realm.  So, I deleted it.

I was comparing my solution to the "short local version" of the python code.  I would argue that my one line of ruby code (which I pulled) is more idiomatic than the python code for the "short local version".  The python code fails to use variable names (uses single char variable names), and it leaves a dangling filehandle in the way the file is opened (should use a with..as context manager).  Also, given that python is at version 3.4, the use of 2.X print statement syntax is outdated (should be a print function).  In other words, I think an argument could be made that my one liner is more idiomatic ruby than the python "short local version".  Perhaps that one should be updated in a similar fashion? [I don't really care, I'm just bringing it up for the sake of consistency].

--[[User:Jtprince|Jtprince]] ([[User talk:Jtprince|talk]]) 15:52, 14 August 2015 (UTC)

==No Dictionary?==
At this moment, the URL for the dictionary (http://www.puzzlers.org/pub/wordlists/unixdict.txt) is returning a 401. Is any action required to remedy this?

--[[User:Balrog|Balrog]] ([[User talk:Balrog|talk]]) 20:20, 14 August 2018 (UTC)

: Yes, it would be nice to have a (stable) version of the   '''unixdict.txt'''   stored somewhere on Rosetta Code,   that way,   any new computer programming examples would be consistent with those entered before the latest updates or changes that might have been made to the original (dictionary) file.   Plus it would eliminate the possibility of any 401 and 404 errors,   and the possibility of added cookies from the original host site mentioned above.   -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 22:50, 14 August 2018 (UTC)

::I started to look into it, but have to stop, myself. I got as far as [http://www.puzzlers.org/word-lists this page] which states that '''some''' of the lists may be open-source. If someone finds that we could site the list here then we might then try and get a copy re-hosted; but it would have to be the exact same page or saved as a latin_1 encoded text file. (We would not want a copy to cause problems with existing code).

:::I successfully accessed the wordlist using the "Wayback Machine" with this URL ==> https://web.archive.org/web/20180611003215/http://www.puzzlers.org/pub/wordlists/unixdict.txt
:::I'll edit that URL into the page. If that's a mistake feel free to back out my change.
:::--[[User:Balrog|Balrog]] ([[User talk:Balrog|talk]]) 19:16, 7 June 2019 (UTC)
