+++
title = "Talk:Long multiplication"
description = ""
date = 2016-07-20T22:14:37Z
aliases = []
[extra]
id = 3983
[taxonomies]
categories = []
tags = []
+++

==BigNum==

Wouldn't it be appropriate to replace this task with [http://en.wikipedia.org/wiki/Bignum arbitrary-precision arithmetic] implementation? I.e. long to long +, -, *, /, and long to short +, -, *, /. The divisions are to be implemented in a complete form (result + remainder).

Hi [[User_talk:Dmitry-kazakov]].  Yes that makes sense.  
For the moment I'll switch this task to be a member of [[:Category:Arbitrary precision]].
IIRC the Newton Raphson Kontorovich method can be used to speed up the division.  It
looks interesting....

Re: long/short implementations... I spotted the Ada Rational Arithmetic 
sample code with "'''long''' + '''short'''" and considered replicating.
But by the time if counted all the combinations I ran out of 
fingers and toes. E.g. one would have to combine the  '''short...''',
'''short''' '''short''', '''short''', '''long''', '''long''' '''long''',
'''long...''' for '''int''', '''real''', '''compl''' for the operators
"+", "-", "/", "*", "%", "%*", "**", etc ... ALSO: &times;, &divide;, ...
'''abs''', '''over''', '''mod''' '''up''', '''bin''' etc ... together 
with "+:=", "-:=" etc ... 

Result: No more toes... I think C++/Ada templates can manage this kind of
complexity.

The [[ALGOL 68]] standard had a shorthand/template ≮L≯ for this. e.g.
 '''op''' ≮÷*, %*, ÷×, %×, '''mod'''≯  = (L '''int''' a, i) L '''int''': ...
But this for the compiler writer, and not available to programmers.

[[User:NevilleDNZ|NevilleDNZ]] 10:56, 26 February 2009 (UTC)

: Hm, there's already good code out there, with ''good'' license (GPLv2 or later) too... e.g. [[:Category:Bc|Bc]] is an arbitrary precision calculator ([http://directory.fsf.org/project/bc/ Bc]) ... does it make sense copy-pasting or this behaviour would just waste RC's aims and space? (I believe RC is not about ''originality'', so it would be ok such a copypasted code, if licenses permit it?) Or should someone at least semplify the code or extract relevant parts in order to show more clearly the ''techinics'' of the calculation? --[[User:ShinTakezou|ShinTakezou]] 14:07, 26 February 2009 (UTC)
: I created this task for two reasons.  First, I wanted to see what a programmatic implementation of the ''algorithm'' behind long multiplication might look like.  Second, there are better arbitrary-precision multiplication algorithms out there, but before one looks at those, it helps to understand the simpler, less-efficient ones first.  Creating this task is a stepping stone for implementing those better algorithms.  Libraries implementing general-purpose arbitrary-precision math tasks are great, but this one was specifically about the algorithm in question.  I'll update the task description to be a little more clear on the "this isn't something you should do in production code" point. --[[User:Short Circuit|Short Circuit]] 15:34, 26 February 2009 (UTC)
:: It is clear. But going further (implementing a whole algebra ...), I am not sure it would exemplify more. Anyway, does it make sense to show the GMP usage too, or the code should be eliminated? --[[User:ShinTakezou|ShinTakezou]] 14:32, 27 February 2009 (UTC)
::: {{IMO|Short Circuit}}, the code ought to be kept, but it's in the wrong task.  Perhaps we need more general arbitrary precision tasks, in addition to showing the individual algorithms.  Or maybe the concept of a "task" needs to be further subdivided between "implement this algorithm" and "achieve this end."  On the "implement this algorithm" pages, care should probably be taken to point out that there could be more idiomatic or best-practice methods of achieving the same goal, and include a link to corresponding "achieve this end" page.--[[User:Short Circuit|Short Circuit]] 19:45, 27 February 2009 (UTC)

== Wrong algorithms ==

Some of the examples seem to call an outside algorithm:

* ALGOL 68 (1st example), calls operator * of LONG LONG INT
* BBC BASIC (1st example), calls FNMAPM_Multiply
* Bracmat, calls operator *
* C++, calls operator * of cln::cl_I
* C#, calls Multiply of System.Numerics.BigInteger
* D (1st example), calls operator * of BigInt from std.bigint
* F#, calls operator *
* Groovy, calls operator *
* Icon and Unicon, calls operator *
* Java, calls multiply of java.math.BigInteger
* Liberty BASIC, calls operator *
* PHP, calls bcmul
* PicoLisp, calls operator *
* Prolog, calls operator *
* PureBasic, calls TimesDecimal of decimal.pbi
* Python (1st example), calls operator *
* R (1st example), calls mul.bigz of gmp
* <del>REXX, calls operator *</del>
* Scheme, calls operator *
* Seed7, calls operator *
* Slate, calls operator *
* Smalltalk, calls operator *

For these examples, I would like some assurance that the outside algorithm really is long multiplication, and not some other algorithm (such as Karatsuba or Toom multiplication). Examples that use the wrong algorithm are incorrect. Further, I would like to clarify the task, to prohibit the use of outside algorithms, unless those algorithms do long multiplication. I think that [[Arbitrary-precision integers (included)]] is a better place to call outside algorithms. --[[User:Kernigh|Kernigh]] 00:36, 20 March 2012 (UTC)

: What kind of assurance would be acceptable?   Since I wrote my personal assurance of REXXes algorithm, it was ''still'' flagged as incorrect.   I've been using REXX for over 30 years and it was always thus.   I've seen the original (assembler) source.   REXX does explicit long multiplication (as well as addition, subtraction, and division). -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 06:55, 13 May 2013 (UTC)  

At the moment, I believe that <math>2^{64}</math> and <math>2^{128}</math> are so small that most outside algorithms would use long multiplication. (In [[MRI]] Ruby, the threshold for Karatsuba multiplication seems to be around <math>2^{2240}</math> or <math>2^{4480}</math>.) --[[User:Kernigh|Kernigh]] 03:03, 20 March 2012 (UTC)

: REXX can handle (at least up to) 2^2,111,222,333 using exact multiplication.   I'm not vouching for how long it takes. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 06:55, 13 May 2013 (UTC)

:: I Marked many of these as wrong, since the least that should be done is uniform treatment of all bignum implementations as wrong.  If it's fine to use bignums (regardless of range or speed), then it should be very clear, and the wrong comments should be removed from all of the implementations, IMO. --[[User:Elibarzilay|Elibarzilay]] ([[User talk:Elibarzilay|talk]]) 07:09, 13 May 2013 (UTC)

::: At one point, this site had a guideline that tasks should specify the goals of the implementation, and maybe some tests, but that we should not specify how the computations are performed. Otherwise, in the general case, we do not have a way of comparing languages. We have drifted away from that, I think, because it's so popular to specify the details of how things get done. I think we are running into that tension, here. --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 10:59, 13 May 2013 (UTC)
:::: I can see the dilemma,  here are some more various shades of this issue:
::::* Solutions using Built in or standard distribution routines - maybe good to demonstrate best practice.
::::* Solutions with an implementation (algorithm) example  - maybe good to demonstrate algorithm methodology in a specific language.
::::* Other libraries or implementation specific extensions e.g.
::::** Solutions using GPL/LGPL/BSD libraries - good to demonstrate availability, portability and longevity
::::** Solutions using Proprietary libraries - good to demonstrate available support for hire.
::::All of these points are important to different coders in different scenarios.  What would be nice is to have a standard template indicating how which solution was used, eg instead of simply <nowiki>{{header|ALGOL 68}}</nowiki>, we could have <nowiki>{{header|ALGOL 68|using=library:GSL}} or {{header|ALGOL 68|solution=builtin}} or {{header|ALGOL 68|solution=algorithm}}</nowiki>
:::: Alternatively we could split tasks, eg [[Long_multiplication/algorithm]], [[Long_multiplication/builtin]], [[Long_multiplication/Proprietary Library]] 
:::: Or a check box table (for each of the above) could be included at the start of each languages section as a visual aid to seeing which solution is most complete.
::::(As an aside, [[ALGOL 68]] implemented the first two solutions, builtin and a complete algorithm.)
::::So far, I implement the most interesting solution,  which is not always the shortest solution.  <i>Sometimes</i> I implement both as the generalised code versus the one-off code when such a comparison is useful/interesting/informative.
:::: [[User:NevilleDNZ|NevilleDNZ]] ([[User talk:NevilleDNZ|talk]])
::::: Perhaps the task should be defined in two parts - one to represent the intermediate results, the other to go from the intermediate results to the final results?  --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 15:18, 13 May 2013 (UTC)
::: I do not really see a point in asking for the code in languages where arbitrary precision is already builtin. For one, it should not lead newcomers of a language to do it that way. And second, as the builtin operators are usually highly tuned to the task, which a naive piece of code usually is not. Also, I found other languages, where it seemed perfectly ok to write a comment like "is native in language" (sidef) or even a call to BigInt arithmetic (D), so I think that should be also ok for languages like scheme, self, smalltalk etc. And b.t.w. what about languages which support an int256 type (eg native limited. but higher precision integer type). Are those allowed to use it or not? As I understood rosetta, it should give programmers a feel of how the language is used, not how its builtin operators are implemented. --[[User:Cg|Cg]] ([[User talk:Cg|talk]])
:::: Are you saying that newcomers to a language should not use the builtins? (Also, please sign your comments - you inserted your comment in front of my signature which might have been misleading.) --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 22:19, 26 October 2015 (UTC)

== PLI Fails for me ==

With
 

*process source xref attributes;

 m: Proc Options(Main);;

 Dcl a(2) Dec fixed(1) Init(1,2);

 Dcl b(2) Dec fixed(1) Init(1,2);

 Dcl c(5) Dec fixed(1) Init((5)0);

 Call multiply(a,b,c);

I get FIXEDOVERFLOW

Can you please add a valid main program? --[[User:Walterpachl|Walterpachl]] ([[User talk:Walterpachl|talk]]) 21:52, 13 May 2013 (UTC)

==negative numbers==

Most of the programming examples don't appear to handle   ''negative''   numbers.   -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 20:25, 25 July 2015 (UTC)

==integers or numbers?==
It doesn't specifically mention that only integers are to be used   (although it did mention that   ''one possible approach to arbitrary-precision integer algebra'').   -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 20:25, 25 July 2015 (UTC)
