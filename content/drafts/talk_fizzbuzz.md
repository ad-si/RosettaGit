+++
title = "Talk:FizzBuzz"
description = ""
date = 2015-09-22T21:18:17Z
aliases = []
[extra]
id = 2186
[taxonomies]
categories = []
tags = []
+++

==FizzBuzzJazz==
I wasn't satisfied with the approach most of the languages were taking, so I added a second approach to PHP, that uses the concatenation operator.  I ''almost'' added a third code example as an extension of the puzzle, to demonstrate the approach.

I wanted to add a third term, "Jazz", to appear on multiples of 7.  So 7 would have been "Jazz", 21/42/84 would have been "FizzJazz", 35/70 would have been BuzzJazz, and 105 (if the loop were to get that far) would have been FizzBuzzJazz.  The if-else ladder approach would have needed three additional if statements to accommodate, while the concatenation approach would have needed only one.

This is probably a good time to point out that FizzBuzz is classified as a puzzle; As the header template suggests, "Multiple approaches are not discouraged."  So if you've got a better way to solve the problem for a given language, let's see it! --[[User:Short Circuit|Short Circuit]] 13:24, 31 October 2007 (MDT)

== Puzzle talk ==

Why was this taken out of the puzzle category? As far as I can tell....it is a puzzle. Besides that, if it's not a puzzle, then what is it? --[[User:Mwn3d|Mwn3d]] 14:03, 11 December 2007 (MST)

:I removed it, since it's not a puzzle since no thinking is involved in solving it. All that it is, is writing the specification in the syntax of a language.
::The thought is in how to do best do it in a given language.  When the task first went up, most of the exaples used an if-elseif-else approach, which couldn't really be expanded efficiently. (See the FizzBuzzJazz comment above.)  Also, compare the text of [[Template:Task]] and [[Template:Puzzle]].  Their goals differ slightly.  While a normal task benefits more from syntactical and functional uniformity, a puzzle benefits from drawing out how different approaches affect efficiency. (Also...Sign your posts with <nowiki>--~~~~</nowiki>.) --[[User:Short Circuit|Short Circuit]] 21:25, 11 December 2007 (MST)
:::I think the whole idea behind FizzBuzz is that it misleads people to think it is a puzzle, while in reality it is not. The straightforward solution with if-elseif-else is the best and easiest, but most people start to think they should do something "clever" to combine the fizz and buzz parts. Trying to do that, it is easy to make a mistake. That is why FizzBuzz has been used in testing job applicants.
:::--[[User:PauliKL|PauliKL]] 13:28, 20 September 2008 (UTC)
::::I didn't take it as a puzzle. How can you after reading through most of the other responses? But I did take it as (1) something to solve that would demonstrate how VBScript does it compared to other languages, and (2) a testbed for other ways of doing it (assuming, of course, that TIMTOWTDI) --[[User:Axtens|Axtens]] 02:43, 17 February 2010 (UTC)
::::Isn't it fair to say that a trick question is analogous to a puzzle?  The other side of this argument is that FizzBuzz should absolutely be over-thought for any of several reasons, the least of which being that a solution using if-elseif-else is flawed, as it is missing a test case. --[[User:Bendingoutward|Bendingoutward]] 07:28, 25 July 2010 (UTC)

== Broken up ==

This page needs to be broken up. Preferably not by language, but perhaps by idomatic approach. (if-else ladder, concatenation, ternary, etc.) Rationale: The syntax highlighting for this much code takes a *long* time to process. --[[User:Short Circuit|Michael Mol]] 19:55, 15 February 2012 (UTC)

: The fundamental problem is that  ''lots'' of languages have tackled this problem (129 at the last count). It's a problem of success (good to have!) but it's still a real problem. Breaking up by approach may well not help very much, especially as that will be taken by many as a challenge to implement ''both'' methods (and we'd have to work out for ourselves how many of the languages actually solve the problem, which can be anything from quite easy or very hard; I don't want to even try to work out what the Whitespace solution is doing). –[[User:Dkf|Donal Fellows]] 21:09, 15 February 2012 (UTC)
:: I was thinking more as subpages, not separate tasks. --[[User:Short Circuit|Michael Mol]] 21:21, 15 February 2012 (UTC)
:::We would still need to figure out the approaches of the unlabelled examples to see which subpage to put them on. Or we could just leave them here? Seems messy. --[[User:Mwn3d|Mwn3d]] 21:39, 15 February 2012 (UTC)
::::If the approach of an example isn't clear, leaving it at [[FizzBuzz]] is fine. No need to be perfect. --[[User:Short Circuit|Michael Mol]] 21:43, 15 February 2012 (UTC)
:::::I suggest one subpage for examples where the number of lines is a multiple of three... &mdash;[[User:Sonia|Sonia]] 22:07, 15 February 2012 (UTC)
::::::One for where the number of lines is a multiple of Fizz, and another for where it is a multiple of Buzz! All other solutions to be marked as wrong… –[[User:Dkf|Donal Fellows]] 22:27, 15 February 2012 (UTC)

::::::: Number of Lines?   ... Including blank lines?   Including comment lines?   ... Or number of computer language program statements?   Code golf, here we come! -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 04:48, 20 November 2014 (UTC)

Why the sudden break up of the page by language type? --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 13:02, 22 September 2015 (UTC)
:It is easier to compare programs "from the same family" that way.
:Especially when they are spread out, like all the different BASIC-variants. --[[User:Hajo|Hajo]] ([[User talk:Hajo|talk]]) 21:11, 22 September 2015 (UTC)

== Missing from Solutions by Programming Task Category ==

FizzBuzz doesn't show up on the [[Category:Solutions by Programming Task]] page.
-- 2014-01-06T02:59:23‎ Infogulch
: No problem, both FizzBuzz and General FizzBuzz are showing up -- [[User:Hajo|Hajo]] ([[User talk:Hajo|talk]]) 21:03, 22 September 2015 (UTC)

==Too many versions for some languages==
Some are code golf/ or otherwise obscure algorithms. If a language has more than two versions it should be reviewed and only the one(s) showing the language in best light should be kept. --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 20:35, 19 November 2014 (UTC)
:I disagree - I think, having several different algorithms for a task is a good thing. 
:And that is easier to show on simpler tasks, with fairly short code, e.g. FizzBuzz.
:See [[Sieve_of_Eratosthenes]] or [[Fibonacci sequence]] for tasks with lots of different approaches.
:If pages get too long, make subpages, see [[Go Fish]]. But [[99 Bottles of Beer]] would need that more...
:Also, see [[:Category:Collection Members]] --[[User:Hajo|Hajo]] ([[User talk:Hajo|talk]]) 21:59, 19 November 2014 (UTC)

:: I also disagree --- ''only keeping the one(s) showing the language in best light should be kept''   --- is very subjective, who is able to say that other versions are kinda OK, but MY version is the bestus and shows off the language better than the others   (possibly implying that the quality of other versions aren't up to scratch).   But putting the joking aside, each version (most likely) demonstrates how to do a very simple task in different ways, I prefer simplicity (for understanding) and easy expandability (such as possibly adding a   '''Jazz'''   option for multiples of 7   and/or others --- for instance).   Just because the requirements are so simplistic, it doesn't mean that we have to prune the examples down to a couple (or whatever number).   Besides, it's not like they are consuming a vast amount of space.   I think each example shows how to solve the task using different approaches (or algorithms).   Once we start pruning computer programming examples, where does it stop? --- And who does the pruning?   Who gets to decide that one version isn't that much different than another?   Certainly, a few comments in the code (or prologue) would help immensely for some of the "trickier" and/or obtuse algorithms (or heaven help us all, obfuscated code) --- but then I'm not a huge fan of code golf (albeit, sometimes it's hard to distinguish between   ''shortness of code''   and;   ''concise code'') --- although it seems that there are a lot of examples that seem to preach that idea (as demonstrated by the number of programming examples in the various Rosetta Code tasks).   Of course, it's possible that my observations are somewhat jaded a bit. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 00:33, 20 November 2014 (UTC) 

:::To Hajo and <strike>Gerald</strike> Gerard: Ouch and ouch respectively. I think we all should collectively bare in mind the need to show the language off clearly and without too much bloat. If you are putting up another variant then the reason for adding it should either be obvious or discussed and made plain. Is the imposition of extra constraints a good reason? In this case  don't think so. --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 05:56, 20 November 2014 (UTC)


### Here are the five versions as they were:


====Version 1 - regular if / else====
with linebreaks after each "FizzBuzz":  <!-- http://ideone.com/UrHdvd -->

```AWK
# usage: awk  -v n=38  -f FizzBuzz.awk
#
BEGIN {
   if(!n) n=100
   print "# FizzBuzz:"
   for (ii=1; ii<=n; ii++)
       if (ii % 15 == 0)
           {print "FizzBuzz"}
       else if (ii % 3 == 0)
           {printf "Fizz "}
       else if (ii % 5 == 0)
           {printf "Buzz "}
       else
           {printf "%3d ", ii}
    print "\n# Done."
}
```

{{out}}

```txt

# FizzBuzz:
  1   2 Fizz   4 Buzz Fizz   7   8 Fizz Buzz  11 Fizz  13  14 FizzBuzz
 16  17 Fizz  19 Buzz Fizz  22  23 Fizz Buzz  26 Fizz  28  29 FizzBuzz
 31  32 Fizz  34 Buzz Fizz  37  38 Fizz Buzz  41 Fizz  43  44 FizzBuzz
 46  47 Fizz  49 Buzz Fizz  52  53 Fizz Buzz  56 Fizz  58  59 FizzBuzz
 61  62 Fizz  64 Buzz Fizz  67  68 Fizz Buzz  71 Fizz  73  74 FizzBuzz
 76  77 Fizz  79 Buzz Fizz  82  83 Fizz Buzz  86 Fizz  88  89 FizzBuzz
 91  92 Fizz  94 Buzz Fizz  97  98 Fizz Buzz 
# Done.

```


====Version 2 - bash with echo====
using echo to generate the numbers: <!-- http://ideone.com/0VMIuO -->

```AWK
echo {1..100} | awk '
BEGIN {RS=" "} 
$1 % 15 == 0 {print "FizzBuzz"; next}
$1 %  5 == 0 {printf "Buzz ";   next}
$1 %  3 == 0 {printf "Fizz ";   next}
{printf "%3d ",$1}
'
```


====Version 3 - one-liner with seq====
using bash with seq to generate the input: <!-- http:  -->

```AWK
seq 100 | awk '$0=NR%15?NR%5?NR%3?$0:"Fizz":"Buzz":"FizzBuzz"'
```


====Version 4 - no divisions====
awk, using no division & no modulo: <!-- http://ideone.com/uHmYUr -->

```AWK
# usage: awk -v n=38  -f fizzbuzzNoDiv.awk
#
# FizzBuzz using no division & no modulo - operations:
BEGIN {
    if(!n) n=100
    print "# FizzBuzz:"
    while (c1<n) {
	  c1++; c3++; c5++; cF++; x=sprintf("%3d ",c1)
	  if(c3>= 3) { c3=0; x="Fizz " }
	  if(c5>= 5) { c5=0; x="Buzz " }
	  if(cF>=15) { cF=0; x="FizzBuzz\n" }
	  printf(x)
    }
    print "\n# Done."
}
```

Same output as version 1.

====Version 5 - no divisions, repeating pattern====
another solution with no division & no modulo: <!-- http://ideone.com/jF9Ddd -->

```AWK
# usage: awk -v n=42  -f fizzbuzzRepeatPattern.awk
#
func p(n,t) {
    if(0+t==0) {printf("%3d ",n); return}
    printf fb[t]
}
BEGIN {
    if(!n) n=100
    print "# FizzBuzz:"

    F="003053003503006"
    split("1,2, Fizz,4, Buzz, FizzBuzz\n,", fb, ",")

    while (i<n) {
	  i++; fmt++;  
	  p(i, substr(F,fmt,1) )
	  if(fmt>length(F)) { fmt=1; }
    }
    print "\n# Done."
}
```

Same output as version 1.


### Critique of AWK versions

I decided that there were possibly too many versions - 5 - for such a straight-forward task. 

* Version 1 seemed clear in both its chosen algorithm and its execution.
* Version 2 seemed the the next clearest. Use of a bash shell and echo for some of the work is all standard AWK usage and the AWK program is well layed out.
* Version 3 is not as nice code, less maintainable, but it ''is'' in the grand tradition of AWK one liners.
* Version 4: Well using no division and modulo are just artificial restrictions that don't make the code look as good as version 1, and just makes me ask why? Those restrictions don't give a solution that emphasises some other part of AWK particularly - not enough for a mention by its author anyway.
* Version 5 reads like a me-to take on 4 with its stated use of the same extra restrictions and that really obscure <code>F="003053003503006"</code> line.

There is a task. THere is a language. The idea is to show off the language to its best. We do have a talk page, and people have delved into a task in more "depth" on talk pages before, but I thought that people were not thinking enough about showing AWK at its best. There was no comments on the different language features used in an example, just artificial limits applied to the task to produce different code.

Now some languages mention that they support different programming paradigms and so it could be useful to show, for example, both an OO and functional version of a Python program. This does ''not'' apply to versions 4 and 5

Sometimes a different example can use a different part of the language, such as testing for primes using a regexp rather than a non-regexp solution , or  solution that allows for tail-call optimization versus an obvious algorithm that does not.That too would be highlighting a feature of the language, but even then judgement might be needed if the resultant code is too messy compared to the non tail-call optimized version/algorithm.

If you are adding the fourth or more variant for a language then you should think harder about your reasons for trying to include it with an eye on what extra it reveals about the language and/or its contribution to the task as a whole. 

Now the above is just my opinion, and we ''could'' just have an edit war on version 4 and 5, but I would prefer to get your reasons for keeping them as, as you have stated, other tasks may have similar numbers of examples for similar reasons. I am trying to get a good task page with good, idiomatic code for each language. If you think my idea is duff then please explain your reasoning too (Not just Hajo, every RC user willing to express an opinion). Hopefully I can learn from how my ideas are shot down too (ouch).
:-)
 --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 23:26, 19 November 2014 (UTC)
: I moved the awk-stuff to [[FizzBuzz/AWK]] and added some comments.
: v2 and v3 are not mine, and v1 just got the printf - output-tweak from me.
: Different algorithm can be expressed just as well with "idiomatic code".
: Not understanding how it works is not the same as obfuscation.
: E.g. I don't understand how the [[Yin and yang]] rendering works (yet)...
: OTOH, lots of programs could use better explanations (or any). --[[User:Hajo|Hajo]] ([[User talk:Hajo|talk]]) 01:47, 20 November 2014 (UTC)

== Enterprise FizzBuzz ==

There's no real way of including something like this here, but perhaps this is worth mentioning:

https://github.com/EnterpriseQualityCoding/FizzBuzzEnterpriseEdition

It's... well, see for yourself... --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 01:29, 16 April 2015 (UTC)
