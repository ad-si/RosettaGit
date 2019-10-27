+++
title = "Talk:Lucky and even lucky numbers"
description = ""
date = 2014-03-14T15:42:12Z
aliases = []
[extra]
id = 17345
[taxonomies]
categories = []
tags = []
+++

== Wrong example of even luckies? ==

'''take a list of all the positive even integers'''
: 2, 4, 6, 8, 10, ...
'''remove every 2nd number
: 2, 6, 10, ...
'''take the 2nd number   (which is   4 )'''
: Not 6?

(Also, why was this talk page deleted?) --[[User:Ledrug|Ledrug]] ([[User talk:Ledrug|talk]]) 09:09, 9 March 2014 (UTC)

:I had a question that wasn't relevant once I understood the task more so deleted it almost immediately. --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 11:50, 9 March 2014 (UTC)

:The sequence itself is correct, according OEIS, so the problem is in the description of the task.  I believe the 2nd line in the second section is a copy/paste difficulty stemming from how the first sequence is defined.  If instead of talking about deleting every other one, I suspect both sequences should be leave out the 2nd "skip every other one", and revise the first rule to be in terms of the odd numbers, the second sequence in terms of the even numbers.  So we'd simply have:
<blockquote>
''Lucky numbers'' are positive integers that are formed by:

:* take a list of all the positive odd integers
:* take the 2<sup>nd</sup> number   (which is   ''' 3 ''')
:* remove every 3<sup>rd</sup> number
:* take the 3<sup>rd</sup> number   (which is   ''' 7 ''')
:* remove every 7<sup>th</sup> number
:* take the 4<sup>th</sup> number   (which is   ''' 9 ''')
:* remove every 9<sup>th</sup> number
:* take the 5<sup>th</sup> number   (which is   ''' 13 ''')
:* remove every 13<sup>th</sup> number
:* take the 6<sup>th</sup> number ··· 


'''definition of even lucky numbers'''

''Even lucky numbers'' are positive even integers that are formed by:
:* take a list of all the positive even integers
:* take the 2<sup>nd</sup> number   (which is   ''' 4 ''')
:* remove every 4<sup>th</sup> number
:* take the 3<sup>rd</sup> number   (which is   ''' 6 ''')
:* remove every 6<sup>th</sup> number
:* take the 4<sup>th</sup> number   (which is   ''' 10 ''')
:* remove every 10<sup>th</sup> number
:* take the 5<sup>th</sup> number   (which is   ''' 12 ''')
:* remove every 12<sup>th</sup> number
:* take the 6<sup>th</sup> number ··· 
</blockquote>
:Parallelizing these descriptions shows how they are essentially the same algorithm (as shown by the parameterization of the algorithm in the Perl 6 solution). --[[User:TimToady|TimToady]] ([[User talk:TimToady|talk]]) 01:56, 10 March 2014 (UTC)

:'''Or there is this explanation''', (very similar):

<blockquote>
Note that in the following explanation list indices are assumed to start at one.

''Lucky numbers'' are positive integers that are formed by:

:* Form a list of all the positive odd integers
:* Return the the 1<sup>st</sup> number from the list   (which is   ''' 1 ''')
:* (Loop begins here)
:* Note then return the 2<sup>nd</sup> number from the list   (which is   ''' 3 ''')
:* Discard every 3<sup>rd</sup>, (as noted), number from the list to form the new list.
:* (Expanding the loop a few more times...)
:* Note then return the 3<sup>rd</sup> number from the list   (which is   ''' 7 ''')
:* Discard every 7<sup>th</sup>, (as noted), number from the list to form the new list.
:* (In short ...)
:* Take the 4<sup>th</sup>. which is   ''' 9 '''. Remove every 9<sup>th</sup>.
:* Take the 5<sup>th</sup>. which is   ''' 13 '''. Remove every 13<sup>th</sup>.
:* ...
:* Take the n<sup>th</sup>. which is   ''' m '''. Remove every m<sup>th</sup>. increment n
:* ...


'''Definition of even lucky numbers'''

This follows the same rules as the definition of lucky numbers above ''except for the very first step'' which becomes:
:* Form a list of all the positive even integers

The change in initial condition of the list generates a new sequence starting 2, 4, 6, 10, ... as the same algorithmic steps are followed. 

--[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 07:47, 10 March 2014 (UTC)
</blockquote>

== What does "Mixed case should be supported" mean? ==

This phrase can be read in diametrically opposite ways.  I took it to mean that it doesn't matter what case you type the argument, which means the prior rule about spelling would only care if you had the wrong letters, not the wrong case.  By this interpretation the Perl 6 entry is not incorrect in its argument handling, and the example is merely demonstrating case insensitivity.  If this is not the intended meaning of "mixed case", it needs to be clarified in terms of case sensitivity vs insensitivity.  Perhaps my intepretation is colored by my absolute loathing of "camel case" words, which I think have no place in a task such as this, if indeed they are being considered mandatory.  It's quite challenging enough to meet the numeric challenges of this task without all the extra API folderol, in my estimation. --[[User:TimToady|TimToady]] ([[User talk:TimToady|talk]]) 01:09, 10 March 2014 (UTC)

:(Also, if the quibble is that Perl 6 entry isn't following the first three rules, it is.  It is, in fact, testing for those conditions with the signature matching.  If the intent is to mandate some kind of particular error message in <i>response</i> to those errors, it should be made clearer just how anally the original implementation is to be copied.)  --[[User:TimToady|TimToady]] ([[User talk:TimToady|talk]]) 01:24, 10 March 2014 (UTC)
:(Or if the requirement is for a placeholder comma, it's certainly not necessary for the Perl 6 solution, though I could certainly make it throw one away easily enough.  The point of Rosettacode is to show idiomatic usages, not force every language to show how it supports the limitations of other languages...)  --[[User:TimToady|TimToady]] ([[User talk:TimToady|talk]]) 01:42, 10 March 2014 (UTC)

:: Support of "mixed case" means to support the alphabetic options in any case, lower, upper, or mixed case (when the option was entered as the command line).  Another phrase would be case insensitivity of a keyword (or option). The API requirement is meant to deal with writing a simple program that handles processing of lucky or even lucky numbers (either specific numbers, a range of numbers, or all numbers within a range) without changing/rewriting a program (either statements or values).  This should not be seen as a limitation of any one language, but to support a user's invocation of a program to solve a general request (from the command line (versus via a program request). This is one method of supporting a general usage via the command line (CL) as opposed to writing a program that deals with just a fixed set of numbers to be generated. As for the comma placeholder, the comma is being used to indicate an omitted argument (or options), and is one common method to indicate such. It was not meant to be used just because of any language limitation, but just a method to indicate an omitted argument. The command line interface is one of the more common interfaces, and a free form format is the one that I choose to be used, it shouldn't be seen as particularly difficult or pedantic.  -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 07:09, 10 March 2014 (UTC)

:Protip: Please be more specific when you mark something incorrect, if you don't want to read all this verbiage of me wondering what you meant.  <tt>:-)</tt> --[[User:TimToady|TimToady]] ([[User talk:TimToady|talk]]) 02:36, 10 March 2014 (UTC)

::The quibble was that yesterday I thought itwas wrong; today I can find nothing wrong with your perl entry. I screwed up. (Again). I am going to have to come up with a new scheme for more thoroughly checking before marking thingsas incorrect. I might remember the track record of contributors and adjust my level of restraint based on that in the future for example. My apologies. --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 06:53, 10 March 2014 (UTC)

== Should this task be split? ==
The more I think about it, the more this seems like two completely distinct tasks unnaturally wedded.  We have on the one hand
the relatively pretty matter of producing the integer sequences named in the title of the task, and on the other hand, we have
the task of implementing a relatively ugly (from a Unix perspective) command-line API.  The two tasks seem to have almost nothing to do with each other, and the latter task in not something the title of the page would lead one to expect.  It feels like an add-on to me.  I'd recommend that we have a page for the pure math, and a different page for the API style, justifiable perhaps as a demo of how to fit into a culture that doesn't use switches for optional arguments.  --[[User:TimToady|TimToady]] ([[User talk:TimToady|talk]]) 02:30, 10 March 2014 (UTC)

I know how you feel. In fact I wrote the Python entry as a neat function for the generation of the series, and a sepearate section handling the argument requirements. --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 06:56, 10 March 2014 (UTC)

: When I created this task, I didn't mean that the command line interface should be so difficult, it was just a method of invoking a program in different manners to specify which series was desired with a straight-forward method of either one number, or multiple numbers, or a range of numbers (with a common method of indicating omitted arguments). I thought I had chosen two integer sequences that were closely related to each other that could be generated by the same subroutine (or function). It wasn't an add-on, but the interface was the primary requirement and impetus of the creation of this task. The method of argument specification was written in such a way to facilitate invocation (from the command line) to generate a specific result (or results). I never thought that there would be a culture clash about using switches versus named options (or optional arguments). -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 07:35, 10 March 2014 (UTC)

:: If the interface is "the primary requirement and impetus" of the task, name the task after it instead of the sequence, although we'd then have a commandline parsing task with an unrelated integer sequence tacked on -- maybe not much better.  I think TimToady's idea of splitting the task is the way to go, whether the commandline processing is difficult or not (it is not, just uninteresting.) --[[User:Ledrug|Ledrug]] ([[User talk:Ledrug|talk]]) 08:28, 12 March 2014 (UTC)

::: I thought the command line processing is quite useful from a utilitarian point of view (albeit simple); it may be uninteresting, but a most common one when one is executing a program which takes its parameter(s) from the C.L.   If this task would be split up, the command line interface would be so trivial and not worth the effort of creating a task for it, it would almost be pointless.   And there would still be a need to make it an interesting and purposeful task, to specify some numbers/options/parameters such that some kind of program would parse them (the arguments) and then make use of those arguments/options for some (easy to understand) purpose, and with clarity and with not much clutter (that is, easy to peruse and understand).   Abstracting the process would make the whole process (I think) obtuse.   Practically all of the programs I enter take options from the command line, it makes it easier to test and execute with different options or ranges of numbers.   This makes the program much more versatile and useful (without the need to modify the program to accept different defaults).   Taking out the C.L. interface (which by most accounts, is quite simple) would make the task ... uninteresting.   If I find a Rosetta Code task uninteresting (or worse, boring), I vote with my feet. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 09:23, 12 March 2014 (UTC)
