+++
title = "Talk:100 doors"
description = ""
date = 2018-12-29T14:20:04Z
aliases = []
[extra]
id = 2152
[taxonomies]
categories = []
tags = []
+++

== An observation ==
An observation: You're actually making 101 passes.  100 mutative, and one for reading the final state.  I'm wondering if the wording of the task should be changed, as no way of reporting the final state within the first 100 passes immediately comes to mind. --[[User:Short Circuit|Short Circuit]] 00:14, 7 October 2007 (MDT)
:Actually, ''Vedit macro language'' example does not need any passes to display the results. The results are visible in the edit buffer as soon as the macro has finished opening and closing the doors. --[[User:PauliKL|PauliKL]] 14:59, 29 April 2009 (UTC)
----
Oddly enough it seems that the only doors left open after all the passes are complete are those which are perfect squares of integers: 1, 4, 9, 16, 25, 36, 49, 64, 81, and 100
Trivially trying the same code for 1000 doors and 1000 mutative passes seems to suggest that this is true for larger numbers (though it's far from proven).  I should, undoubtely, do a proper analysis to see if I can prove that it generalizes and explain why.[[User:JimD|JimD]] 16:03, 11 October 2007 (MDT)
:The number of times a door is visited is the same as the number of factors of the door's index. Open doors have been visited an odd number of times and only perfect squares have an odd number of factors. This [http://olimu.com/Notes/Monkeys&Doors.htm] explains it.[[User:Drea|Drea]] 16:20, 11 October 2007 (MDT)
::Actually, square numbers have an even number of factors, the odd count comes from the first iteration that opens all doors. --[[User:AlexLehm|AlexLehm]] 22:16, 19 October 2011 (UTC)
::: Integer factors of 5 -> [1, 5], Integer factors of 4 -> [1, 2, 4]  (The integer square root gives the list an odd length - other factors are always paired with their matching quotients) [[User:Hout|Hout]] ([[User talk:Hout|talk]]) 07:59, 8 September 2016 (UTC)
----
Some of the "solutions" given are incorrect.  This problem is also a great example of the "Fence Post" condition often missed by programmers.  Look over the solutions and ask yourself if the 100th door is shown to be open or closed.
----
== Optimized Examples ==
Somone just added a Python example which exploits the observation (noted in the preceding discussion here) that perfect squares are the only "open" doors after following this algorithm.  That's fair enough, I guess.  However, it suggests that similarly optimized implementations should be shown for all languages in which they are relevant (to offer a fair comparison). [[User:JimD|JimD]] 16:02, 15 October 2007 (MDT)
:That was me, and you're absolutely right. I was just about to post a MAXScript version. I'll have to leave the Perl and Ada versions to someone else though. [[User:Drea|Drea]] 16:16, 15 October 2007 (MDT)
:Unless we think that this kind of optimisation is beyond the scope of Rosetta Code? I'm not sure, but I thought I'd throw it out there. [[User:Drea|Drea]] 16:23, 15 October 2007 (MDT)
::Or is the problem itself beyond the scope of Rosetta Code? What does the 100 Doors task show off about a langauge that is not covered in one of the other tasks? --[[User:IanOsgood|IanOsgood]] 12:04, 16 October 2007 (MDT)
:::Strangely enough, the unoptimised MAXScript and Python examples demonstrate increasing the loop index by other than one. I haven't seen that in other tasks for languages where (<start>, <stop>, <step>) isn't always explicit in loop constructs. [[User:Drea|Drea]] 15:11, 16 October 2007 (MDT)
:::And looking back at the loop structures task, only a couple of language examples show that it's possible. For example, the C snippet only has "for (i=0; i<10; ++i)" (it also had one round and one curly bracket, but that's fixed now). [[User:Drea|Drea]] 15:27, 16 October 2007 (MDT)
::::Good point. This seems to be an argument not to have the optimized examples. Another classic task for loop increment >1 is the Sieve of Eratosthenes. --[[User:IanOsgood|IanOsgood]] 15:55, 16 October 2007 (MDT)
:::As long as the code examples serve to illustrate factors that relate or separate languages, they're within the scope of Rosetta Code. (At least, as how I originally envisioned it.  But I would like to see RC expand some by including more encyclopedic, documentary and historical information.)  However, for clarity's sake, I described the optimized algorithm in the task description, and added additional organization to the code examples. --[[User:Short Circuit|Short Circuit]] 21:25, 16 October 2007 (MDT)
::I would not call that "optimized" example. It is entirely different algorithm. In fact, it just displays the known results. --[[User:PauliKL|PauliKL]] 14:11, 9 April 2009 (UTC)
:I think all optimized samples should be removed and forbidden. This task essentially boils down to finding the powers of 2 for numbers 1 to 10. See ultra-optimized coffeescript example.  -- [[Special:Contributions/201.21.40.201]] 2011-05-27 06:43:49
::That I think is more a comment on the character of the problem than a comment on the character of the implementations.  In my opinion, mathematical analysis is a perfectly good programming tool, as is community involvement.  Rosetta code could do with fewer "problems" where people feel they should force a particular algorithm.  In this case, we might have to live with that, but if there is no general problem that allows for a range of solutions then posting the answer is the essence of optimization.  (Though I would also agree that optimization is usually a bad idea in practice except when you cannot tolerate the resource requirements otherwise.)  Anyways, I could live with "forbidding the optimized versions" but only if the task also got a warning message that this is not the sort of task that we want on Rosetta code.  --[[User:Rdm|Rdm]] 12:24, 27 May 2011 (UTC)
::: Specialization is useful for teasing out specific differences between languages, but generalization obviously offers more flexibility in choices and demonstrations of clever solutions. If a particular class of solutions must be forbidden, I'd prefer to see the task forked to allow that class to be demonstrated. (Otherwise, a task is very likely to get stuck in a particular idiomatic mindset) --[[User:Short Circuit|Michael Mol]] 17:16, 27 May 2011 (UTC)
I would prefer if all solutions had at least the unoptimized version, though the optimized is fine with me as well (as of this writing, Erlang is missing a unoptimized version for example) --[[User:AlexLehm|AlexLehm]] 22:19, 19 October 2011 (UTC)
::: The so-called "optimized" version solves a _very different_ problem: finding the code with lowest Kolomogorov Complexity that prints the output of the "nonoptimized" version. It's basically the same as printing the solution directly from a string. (And here, ladies and gentlemen, is my optimized version of proving Fermat's Last Theorem, and it fits in the margin, too: 
```txt
"print 'TRUE'"
```
). IMHO, for the optimized version to be acceptable, the language should be a theorem prover that spits out the optimized version from a properly encoded problem statement and also provides the proof that this is indeed equivalent to the nonoptimized version. Maybe the problem should be changed to ''"start with a random open/closed door state and then proceed as follows..."'' [[User:Bear-Shaped Lampshade|Bear-Shaped Lampshade]] ([[User talk:Bear-Shaped Lampshade|talk]]) 14:19, 29 December 2018 (UTC) 
:Optimized solutions are essentially bypassing (my opinioin) the task requirements in that they don't perform the task's description (which I interpreted as implying how to solve the task or at least, implying the method; namely: visit every door and toggle the door).  Otherwise, why don't we just change the task's name to ''display the non-negative squares up to'' '''N'''? -- [[User:Gerard Schildberger|Gerard Schildberger]] 19:49, 23 June 2012 (UTC)

:: Perhaps it was inevitable that this task would split into two, given the character of its solution. Understandable to have decided, at some point, that 'optimisations' were OK, but maybe that can still be reversed, and perhaps a second simpler task created, to harvest the various routes which have been shown to generating a simple series of powers. Personally, when I added an 'unoptimised' solution, I felt some kind of pressure of precedent to provide a counterpart to the demonstrations of simple series in other languages. Can't speak for others, but I would be very happy to have my own 'optimised' snippets moved or simply binned :-) [[User:Hout|Hout]] ([[User talk:Hout|talk]]) 16:57, 21 October 2015 (UTC)

== Self-contradictory task description ==

The expression <tt>"... starting with the first door every time...."</tt> is in direct contradiction with statements like <tt>"...visit every 2nd door (door #2, #4, #6, ...)..."</tt>. On the second round am I supposed to start with #1 or with #2?
:I fixed the task description. It should start with door 1 on pass 1, door 2 on pass 2, etc. --[[User:Mwn3d|Mwn3d]] 22:07, 11 July 2008 (UTC)
:I'm fine with the update, but if you start with the first door every time and toggle every Nth door, you'll get the same result, which is how I interpreted it. --[[User:Cybernicus|Cybernicus]] Aug 22, 2014 15:12EST

== Sub-headings ==

Do we need the sub-headings for unoptimized and optimized implementations?
They just make the Contents list very long and more difficult to find a language.
I think it would be better just to use normal text with bold.
--[[User:PauliKL|PauliKL]] 11:22, 8 April 2009 (UTC)

==FlogScript==
On this discussions page we can post things in esoteric programming and various other things, so here it is in FlogScript (optimized):
 ),.{.*}%&(;
(this is only a function, not the entire program) FlogScript is program language for making code-golf. This kind of stuff doesn't really go with the article but I think it goes OK on discussion page. --[[Special:Contributions/24.207.48.53|24.207.48.53]] 16:51, 5 August 2009 (UTC)
:If it's a programming language, make the whole program and put it on the real page. We have [[:Category:Esoteric Languages|esoteric languages]] on this site and they have programming examples just like any other language. --[[User:Mwn3d|Mwn3d]] 16:56, 5 August 2009 (UTC)
::Only if someone provides some proper documentation so that we could, say, have writing an interpreter/compiler for it as an RC challenge. Otherwise it might as well be a secret handshake; the examples from the esolangs wiki show it's short, but do not illuminate. —[[User:Dkf|Donal Fellows]] 21:46, 5 August 2009 (UTC)

== overgeneralization ==

The main page currently says: "Since people can only keep seven things (plus or minus two) in their minds at a time, this quickly becomes an intellectual burden."  But there is some significant evidence that this issue is related to the native language of the speaker.  I do not know keywords to search on to dig out references, but I am remembering that typical chinese speakers can hold ten independent concepts where english speakers can hold six.  --[[User:Rdm|Rdm]] 15:58, 3 December 2010 (UTC)
::FWIW and with a lag of 5 years, the 7+/-1 trope goes back to the 1950s and is very much out of date. The particular issue with Mandarin was simply that that the Mandarin integer morphemes are phonetically simpler, and impose a lighter load on the 'acoustic loop' of internal rehearsal, which does seem to have a rather limited capacity. The word "seven" is just phonetically more expensive (3 consonants, two syllables) than Mandarin's monosyllabic first tone 'qi', for example.  Mandarin number words simply pack a bit more densely than English in constrained acoustic space. (Cantonese, which has more final consonants in its number morphemes, is likely to be somewhere in between).

== Output consistency ==

It would be good to unify what output the examples should produce. Currently some give simple list of open door numbers, some list only open door numbers, one per line in a full sentence and some give a line for each door saying whether it's open or closed. Plus some add other decorations and stuff. Considering the output formatting is actually significant portion of many of the samples, the difference makes the examples somewhat difficult to compare.

:Did anyone notice that almost all examples didn't provide any output (as per the task's two questions)? If that is acceptable, then we all could save a ton of Rosetta Code space by not including the output in most of the (other) examples and say, the '''output''' is the same as the '''yyy''' example.  Or ... take my word for it, with tongue in cheek.] -- [[User:Gerard Schildberger|Gerard Schildberger]] 19:45, 23 June 2012 (UTC)

I often like to look at how different languages (or programmers) generated their much varied output(s):
* the language construct (instruction used)
* formatting
* ''style'' of formatting (in general)
* columnar vs list 
* vertical vs. horizonal columns (if used) 
* commatized or not (not a word, but every programmer will know what it means)
* use of (or not) headers and/or separators
* imbedded spaces to make columns align
* right vs. left number alignment
* use of whitespace to make perusing the output easier
* indentation
* other visual fidelity techniques
* proper use of plurals, no:  '''1 results found.'''
* no weasel "words", no: '''1 result(s) found.'''
* proper use of verb tense, no: '''1 results were found.'''
* yada, yada, yada.  

Hell's bells, this whole rambling paragraph would be a good Rosetta Code task, if not all encompassing. Well, ok, ok, ''serveral'' Rosetta Code tasks.


I find the manner in which programmers treat their program's output a significant insight in how they treat problems (tasks).  

A lot treat it (here, in Rosetta Code) as a one-off, but such is the nature of Rosetta Code's tasks.  

Yes, it would add more statements to the source code, but as in cooking, presentation (display) is important, as any good restaurant knows. 


I wonder if the word ''weasel'' is known by all nationalities to mean (among other things) to use a technique to escape a tricky or difficult situation, a means to evade a hard task. I just found that Australia (and of course, Antarctica) don't have weasels, but some opinions may differ. -- [[User:Gerard Schildberger|Gerard Schildberger]] 20:19, 23 June 2012 (UTC)

:Hi Gerard, the output thing is difficult. In a lot of my tasks I asked up-front for the output to be displayed and then flagged entries as incomplete/incorrect if output was missing. It can lead to lively discussions on the need for such output. The [[Pascals triangle]] task has an issue about whether the point of the triangle should be indented to be in the middle leading to something like:
::<code>
   #
  # #
 # # #</code>

:Or is this kind of output formatting ''enough'':
::<code>
 #
 # #
 # # #</code>

:There are some tasks, such as [[Yin and yang]] which are very visual and the task is written in such a way that you cannot complete the task without showing output.
:Some contributers miss a stated requirement for output, some complain about the need for output; I tend to like a task written to require some small output to be shown, and I have in the past revisited an example to improve its output. 
:Some competition sites rigidly define I/O formats as entries are automatically checked but their is no such need on RC and I prefer to allow some flexibility in the output formatting (unless the [[Range extraction|task]] is about rigidly formatting output of course) --[[User:Paddy3118|Paddy3118]] 22:48, 23 June 2012 (UTC)

:: Yeah, I read the ''lively'' (if not downright heated) discussion on the Pascal's triangle thing).  This wouldn't fall under the consistency "rule" as much as being correct (or just ''better'', ... I hate to use the phrase ''more correct''), which in almost most cases, is highly controversial and the may be the cause of many fistfights if the contenders are in close proximity.  This is one place where indentation is almost universally used elsewhere, I've never seen the presentation of Pascal's triangle so abused before.  (Ok, a right triangle is a triangle, but still I view it as a crime ..., well, a misdemeanor then). For you guys on the other side of the pond, ''misdemeanour''.  You'd thunk that a little effort would've gone a long way here. Showing Pascal's triangle in the usual matter should've been a requirement (too late for that, gosh darn!), but I can understand why some programmers took the easy way. I hope ''gosh darn'' isn't too strong for this forum, eh? -- [[User:Gerard Schildberger|Gerard Schildberger]] 23:02, 23 June 2012 (UTC)

:: I view the triangle issue in the same vein as output (say) a list of primes or fibonacci numbers, you ''expect'' programmers to list them in ascending order, it's so normal of an expectation that nobody has to state the obvious ... until somebody doesn't. --- Whatcha mean, I can't output the numbers spelled out in English (or Spanish, or Mayan glyphs, or base 19 ...) ? -- [[User:Gerard Schildberger|Gerard Schildberger]] 23:14, 23 June 2012 (UTC)

::: Getting programmers to agree is harder than herding cats. (And quite rightly so).
:-)     --[[User:Paddy3118|Paddy3118]] 05:02, 24 June 2012 (UTC)

== 6502 example ==

I just edited in missing initializations and fixed an off-by one termination test.  I also commented some assumptions and a few 6502 quirks.  This example has no output code -- the referenced emulator displays the memory directly. If I've counted the pixels(!) correctly, the output is numerically correct.  It probably was anyway, but if so, only because of unstated initial conditions in the emulator.  I don't know if I need to do anything to log the user (Gary Tippery) and time (11-Feb-2014 03:14 PDT).

== the optimized version ==

Well, the optimized version for the C, and C++ codes are just this for loop

```txt

for(var i=1;i<=10;i++){
  print("Door %d is open",i*i);
}

```

But in the examples it goes to 100, and not to 10, that means it's calculating 1000 doors, and not 100 doors? [[User:Rainb|Rainb]] ([[User talk:Rainb|talk]]) 07:03, 21 July 2014 (UTC)
