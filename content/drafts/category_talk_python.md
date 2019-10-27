+++
title = "Category talk:Python"
description = ""
date = 2019-03-04T14:39:47Z
aliases = []
[extra]
id = 2332
[taxonomies]
categories = []
tags = []
+++

== New Python 2.6 and 3.0 ==
The upcoming releases of Python 2.6 and Python3 suggest that we should adjust many of the Python code snippets to exemplify the best programming practices that are available in the standard libraries.  (We can, of course, leave the most backwards compatible examples as is and merely add the comparison to the simpler code that's available with newer releases).

Some examples that come to mind:
* use ''with'', where appropriate, in place of more explicit ''try''...''finally'' handling; particular to show the built-in support for file closing ad threading lock releases
** show an example of using ''contextlib'' to create object classes the automatically perform their own lock releases and/or transactional commit or rollback when called in a ''with'' structure.
* review all uses of threading for cases that would benefit from the use of the new ''multiprocessing'' to overcome the limitations of the GIL (global interpreter lock) on SMP/NUMA scaling.
** In particular look for cases where ''Pool'' and ''Manager'' objects from ''multiprocessing'' would simplify code examples.
** Look tasks related to distributed processing for examples that readily implemented using ''multiprocessing'' ''Manager'' objects and other 2.6 and Python 3 feaatures.
* Look for examples in which Python's traditional ''%'' string formatting operator can be simplified, clarified or otherwise improved using the new ''format()'' methods on string (''str'') (and Unicode and similar) objects.
** Show and example of how custom classes can implement their own ''__format__()'' special method such that they can be targets of formatting string interpolation directives.
* Consider adding notes to any topics that focus on the use of the traditional '''''print''''' statement that this will be removed from Python3 ... it's slated to become a function (and thus required an argument list as per the normal Python function call syntax).  This will probably become one of the most confusing issues for future adopters of Python3 (as it becomes the default) when they're trying to study and adapt from 15 years of Python 2.x examples.
* Similar concerns might be raised about the changes from ''raw_input()'' to simply ''input()'' and the need, in Python3, to call ''eval(input())'' to get the same semantics as the traditional ''input()'' built-in function (in relatively rare cases were the use of the old semantics really was warranted).  However most use of the old ''input()'' function was unwarranted.
** Look for cases where the ''ast.literal_eval()'' method would be a more secure or otherwise better alternative to ''eval().''

The gist of all my suggestions is that we want the examples on RosettaCode to be exemplary --- to consistently show the best practices for each language thta we cover.  These changes to Python, mostly preparatory to the release of Python3 (a.k.a. Python3000 or Python3k) represent significant advances in the standard libraries ... which should simplify and improve the Python entries for a number of the tasks covered on this site. [[User:JimD|JimD]] 22:50, 8 September 2008 (UTC)


Thanks to the [http://code.google.com/p/google-highly-open-participation-psf/ Google Highly Open Participation contest] for high-school students, Python has passed Perl as the language with the most solutions.  Well done! --[[User:IanOsgood|IanOsgood]] 18:33, 28 November 2007 (MST)
:Heh.  I was browing programming.reddit.com, and saw a request for small Python projects.  I sent the guy an email, but didn't find out until today what it was about.  You can see part of the email [http://code.google.com/p/google-highly-open-participation-psf/wiki/ProjectSuggestionNotes here]. --[[User:Short Circuit|Short Circuit]] 20:35, 4 December 2007 (MST)

Take a look at some of the interesting python code here, it could probably be integrated: [http://pleac.sourceforge.net/pleac_python/fileaccess.html Programming Language Examples Alike Cookbook]
[[User:StaticShock|StaticShock]]

==Bytecode?==
Python's interpreted? I thought it was compiled to a bytecode... —[[User:Dkf|Donal Fellows]] 23:17, 30 July 2009 (UTC)
:You're right. I thought I knew. --[[User:Mwn3d|Mwn3d]] 23:24, 30 July 2009 (UTC)
::Does mean that we need to find an instance of a current language that's pure interpreted. —[[User:Dkf|Donal Fellows]] 00:55, 31 July 2009 (UTC)
:::Ruby? --[[User:Mwn3d|Mwn3d]] 01:09, 31 July 2009 (UTC)
:Python is both. Interpreted and bytecode. Just like C (interpreted (see [[wp:CINT]]) and compiled (machine code)).

:Like most things in life, it is very hard to find a clean distinction when you look closer. Python is interpreted and has byte code; (as is Perl). Java is compiled and has byte code (as is C#). Trying to categorize Lisp and Forth will force you to look deeper into what distinctions you want to make when you bin a language as ''either'' compiled ''or'' interpreted. (Does a [[wp:Monotreme|mammal lay eggs]])? --[[User:Paddy3118|Paddy3118]] 04:40, 31 July 2009 (UTC)

:: I was asking specifically so that the language could be categorized. It's ''primarily'' bytecoded, and it doesn't have the poor performance of a purely interpreted language. (FWIW, I believe Tcl's the same in this regard; mainly bytecoded with the potential to go interpreted in some awkward highly dynamic cases.) And mammals are defined by having mammary glands. —[[User:Dkf|Donal Fellows]] 07:49, 31 July 2009 (UTC)

:::Hi Donal, Python is ''both'' interpreted ''and'' byte coded.It doesn't have to be either-or. Whilst the internal byte code is normally stored as a file for any modules used, this is an optimisation. Python will work if they are not present.  If, (but only if), your categorization of Python as being byte-coded precludes it from also being interpreted then the categorization scheme is faulty. 
:::   P.S. The classifiers had to drop the rule that mammals gave birth to live young when made aware of animals like the Duck-Billed Platypus --[[User:Paddy3118|Paddy3118]] 09:19, 31 July 2009 (UTC)

:::P.P.S: See [[wp:Interpreter (computing)|this]] description of an interpreter. Especially points two and three which state (an interpreter) "translates source code into some efficient intermediate representation (code) and immediately executes this" (or) "explicitly executes stored precompiled code[1] made by a compiler which is part of the interpreter system". --[[User:Paddy3118|Paddy3118]] 09:49, 31 July 2009 (UTC)

:::: I told you, Tcl's in exactly the same boat except that it is less common to save the bytecodes from one execution to another (our compiler's fast enough that we don't bother normally). The interpreter is mainly used for cases where it is known that the code cannot be executed again (i.e., certain kinds of callbacks) and for handling legacy code. The primary point of mine is that the main way that people experience both languages is as bytecoded systems. —[[User:Dkf|Donal Fellows]] 10:15, 31 July 2009 (UTC)

::::: Hi Donal, things are similar in Python, (and Perl and Ruby), but in the Python community, Python is known as an interpreter even though the interpreter generates byte codes.  It would be a mistake to call Python a compiled language without much more explanation about what is meant by the term and contrasting it with what is the extended meaning of an interpreter. If in TCL your community refers to TCL as being a compiler then both communities could well be right when they explain their meanings.

Beginners in Python are told to experiment in the interactive shell. Newcomers to TCL are often in the command line shell of some Electronic Design Automation tool where they both experience the interactive environment of a typical interpreter. I guess decisions on compiler  or interpreter are also coloured by other distinctions such as dynamic vs static typing. --[[User:Paddy3118|Paddy3118]] 11:22, 31 July 2009 (UTC)

== Time to use Python 3.x by default ==
I think that because of the small size, and very little legacy dependencies of most Python examples for RC tasks; that it is time to '''always give Python 3 answers in new entries'''. I carefully haven't said anything about existing entries or about showing support for Python 2.x. I personally think that new entries might have Python 2.x translations for some time, but what I would like to see is Python 3.x examples become the norm, only omitted when an external 3.x compliant library cannot be found. --[[User:Paddy3118|Paddy3118]] 07:19, 23 February 2010 (UTC)
:I'd like to see 2.x support remain for a few years at least. There is a lot of legacy Python code around. For example, Mac OS X was using 2.3 for a long time, and their latest release is still only shipping with 2.6. --[[User:IanOsgood|IanOsgood]] 17:04, 23 February 2010 (UTC)
::Hi Ian, What do you think of always having Python 3 though? I think it is the future - if we make it so, and Rosetta Code has hardly any of the reasons to stick with 2.x '''as a default'''. --[[User:Paddy3118|Paddy3118]] 19:53, 23 February 2010 (UTC)
:Going through and adding 3.x answers to where there are only 2.x answers would seem to be a worthy task. Keep the old style ones for a while though; some readers might be learning so they can work with old code. You never really know… –[[User:Dkf|Donal Fellows]] 23:27, 23 February 2010 (UTC)
::That is more work than what I was proposing, which was to have Python 3 solutions for '''new''' tasks. --[[User:Paddy3118|Paddy3118]] 00:22, 24 February 2010 (UTC)

Time has passed, and I find that making the same code work for 2.X ''and'' 3.X to be not too hard. When pushed, I might favour just a 3.X solution. If anyone finds the deviations from good Python style that are used to make an example work on both 3.X and 2.X to be a bad thing, then please discuss. --[[User:Paddy3118|Paddy3118]] 05:37, 15 August 2010 (UTC)

==Motto magic ?==

Not much turns on this –  I don't want to press it – but I do wonder whether the eager explanation (in line three) that a motto makes it easy to create bug-free programs doesn't sound a little quaint ?

I'm sure we have all seen buggy Python code (on RC as much as elsewhere in the wild, as it happens), so perhaps, for reassurance of the reader, we could at least reference a few of the experiments which have established confidence in (our ?) view that that memorisation or incantation of mottos does have a statistically detectable impact on bug-rates ? 

Or should such papers prove elusive, perhaps we could cite any experimental confirmation of our hope that bug-rates in Python code might in fact be significantly lower than in comparable corpora written in other languages ? [[User:Hout|Hout]] ([[User talk:Hout|talk]]) 13:09, 4 March 2019 (UTC)
: Anything useful in this, for example ? [http://web.cs.ucdavis.edu/~filkov/papers/lang_github.pdf](A Large Scale Study of Programming Languages and Code Quality in Github) [[User:Hout|Hout]] ([[User talk:Hout|talk]]) 13:15, 4 March 2019 (UTC)
:: A quick glance at Table 6 suggests that adoption of Python may, if anything, be associated with slightly '''increased''' rates of defects :-( [[User:Hout|Hout]] ([[User talk:Hout|talk]]) 14:34, 4 March 2019 (UTC)
:::( Though on the bright side, the Python effect doesn't seem quite as bad as that of C++ or PHP ) [[User:Hout|Hout]] ([[User talk:Hout|talk]]) 14:39, 4 March 2019 (UTC)
