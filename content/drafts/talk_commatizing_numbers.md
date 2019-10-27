+++
title = "Talk:Commatizing numbers"
description = ""
date = 2014-06-09T07:47:55Z
aliases = []
[extra]
id = 17506
[taxonomies]
categories = []
tags = []
+++

== several numbers in the input ==

shouldn't 2000 become 2,000 ? --[[User:Walterpachl|Walterpachl]] ([[User talk:Walterpachl|talk]]) 05:07, 11 April 2014 (UTC)

: If you're referring to the 4<sup>th</sup> string to be commatized ''===US$0017440 millions'''..., then no.   The 1<sup>st</sup> numberic string is '''0017440''', not '''2000''' (which is the 2<sup>nd</sup> numeric string). -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 05:26, 11 April 2014 (UTC)

== Ambiguous task spec. Oddly specific for some things while it glosses over others completely. ==

It seems to me that this task is really 3 subtasks that have been mashed together. That isn't necessarily a problem, but there is no separation of subtasks in the task description. It is all or nothing. As I interpret it, the first, and ostensibly the primary task, is to add appropriate separators to numbers to display them as numeric strings. That one I have no problem with. It is a fine, useful and commonly performed operation. 

: Well, that's close, the task is to add separators to the numeric part (just the first number) of a larger string while ignoring/preserving the window dressing. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 06:00, 9 June 2014 (UTC)

The second subtask, as I read it, is to algorithmically pick out numeric strings from mixed alphanumeric strings. Again, a fine, useful operation, though a little more touchy-feely and open to interpretation.

: Yes, another part of the task is to identify the numeric part of a string of characters, find where the number starts and stops, and add separators where/if appropriate; none of the locating/identification of the number (numeric part of the string) should be left to interpretation, hence the numerous rules. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 06:00, 9 June 2014 (UTC)

The third subtask, and the one I have the real problem with, is to algorithmically determine WHAT a particular numeric string represents so the "correct" arbitrary, inconsistent, seemingly pulled-out-of-the-air set of rules can be applied. I would be happier if the spec followed [[wp:Decimal_mark|wikipedias guidelines for digit grouping]] (which incidentally doesn't mention scientific notation at all) but that's neither here nor there. Task writers are free to impose arbitrary restrictions.

: No, that was never a part of the task, nor was it inferred; the ''purpose'' of the number was never to be determined what it (the number) represents.   Any restrictions specified were not arbitrary and was the main reason of mentioning the specific rules.   None of the rules are contradictory, they all are consistent with the purpose of the task; this task was never intended ''just'' for commatizing a group of digits. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 06:00, 9 June 2014 (UTC). 

My issue is that the task implies and assumes that there is some general algorithm that can be determined, but language is too messy and inconsistent to do that. Numeric strings VERY commonly are identifiers, not numbers: zip codes, phone numbers, credit card numbers, dates (it could be argued that 2014 is numeric, but really it just identifies a certain period of time to make it easier to refer to it). How about numbers in binary? hexadecimal? base 36?

Perhaps if someone could demonstrate how the current implementations "correctly" commatize the following strings:

    "The number 8675309 had to be taken out of service in every area code after Tommy Tutones song 'Jenny' was released"
    "Including in Beverly Hills 90210"
    "Use the credit card number 6011123456789876"
    "1946 was a good year for some; 6/9/1946 was a good day at most." 
    "hex 1234e56 = decimal 1091030"
    "bogus<sub>(36)</sub> = 1001010110101011001010100<sub>(2)</sub>." 
    "123456789 is often used as an 'unused' demonstration social security number."

If the answer is "Well you need to know what the numeric string represents ahead of time and only use when appropriate.", then don't pretend that there is some general algorithm that can be applied. Knowing ahead of time is not an algorithm.--[[User:Thundergnat|Thundergnat]] ([[User talk:Thundergnat|talk]]) 15:54, 19 April 2014 (UTC)

: I share your frustration with this task. Like you, I do not think that this task represents any sort of "general algorithm", and I'd much rather see efforts directed at geometric or mechanical modelling, or manipulation of audio data, or something interesting like that. That said, I would not characterize it as "ambiguous" so much as "completely arbitrary". And perhaps it would indeed be better to create one or more of the three modular tasks you've implied, and add links to them from here? That said, I guess I also need to acknowledge that knowing ahead of time is actually kind of important. --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 16:07, 19 April 2014 (UTC)

: Well it's a wiki. How about we give the original author some time to respond as well as thinking of what a re-write could look like ? --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 18:00, 19 April 2014 (UTC)

:: The task isn't to determine ''if'' a number (within a string) is to be commatized (or not) whether or not the number ''in context'' is to be commatized, but find a number (as series of digits or some characters that can be seen as a number) in a string and add commas (or some other characters based on various supplied options or parameters).   Adding examples of numbers that aren't commatizable based on knowledge that telephone numbers, years (as in dates), addresses, or numbers that may or may not be in base ten aren't part of the task, and other such inappropriate numbers, shouldn't be commatized.   The scope in those cases would be daunting if not next to impossible and would be almost impossible, even if "only" some of the common rules for English would be used --- and I wouldn't want anybody to even try to list the rules for such an endeavor.   There are just too many exceptions where a set of digits wouldn't be considered a number that should be commatized.   So, the examples used in the task could've been:   blah blah blah blah blah blab5679979yadda yadda yadda.   but that would've been ··· well, boring beyond belief.   The task's numeric strings supplied had extra context added ''only'' to illustrate some examples in which they could be used (for commatizing), and that extra context wasn't inferred to be determining ''if'' they (the numbers) should be used in making a determination if the numbers should be commatized or not.   This task isn't about examining the words (in whatever human language shown) and interpret them and make a determination what the number means and if it is appropriate to commatize the number or not.   This isn't an AI problem, but simply a mechanism to add commas (what whatever) to a series of (numeric) digits, irrespective of their context. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 18:25, 21 April 2014 (UTC)

:: If there is something specific concerning what is "glossed over", I would be appreciative if they would be stated or expressed.   Addressing those specifics is what makes a better and clearer defined Rosetta Code task (rather than leave the concerns unsaid and guessed about).   I suspect the hardest part of this task is the locating/indentifying of the start and end of a numeric string (including numbers in scientific notation of any kind or persuasion ... or not) within a larger string (that is, a number with embellishments or window dressing). -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 06:00, 9 June 2014 (UTC)

== Layout - non breakinging space ==

FYI A non breaking space NBSP has total no meaning if it is surrounded by normal (breaking) spaces. More then one space makes the layout ugly and is not an common practice. There are more elegant ways to do so. Think how it will look in an other environment, browser etc.
Cheers --[[User:Siskus|Siskus]] ([[User talk:Siskus|talk]]) 06:56, 9 June 2014 (UTC)

: NBSP has a meaning: (one use is) it introduces a true blank that is non-compressible (multiple blanks or superfluous blanks are normally condensed to a single blank).   A double blank at sentence end and before/after certain clauses and around highlighted words aren't ugly and serve to make the text more readable and/or identifiable --- the whole purpose is to make the sentence or text clearer.   Whether or not there are more elegant ways to do so, it ''is'' the method that I use.   It doesn't matter what method is used, as long as the end product (the text) has the number of blanks that I intended to be viewed.   It is a common practice in composing text to have a double blank after a the end of a sentence, but it isn't universally followed.   Some editors use a full blank + ½ blank.   I don't know of another way to specify double blanks at sentence end (on Rosetta Code), but I know it isn't accomplished by removing the nbsp HTML keyword.   I use the nbsp keyword to make the text easy to read and understand, that's the whole purpose of composing sentences and text. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 07:26, 9 June 2014 (UTC)

: There is a pretty good Wikipedia entry on   [http://en.wikipedia.org/wiki/Sentence_spacing Sentence spacing]   which does a pretty good explanation on the pros and cons of the number of blanks at end-of-sentence.   More and more digital sources (Wiki for one) are apparently leaning to a one-blank sentence end, but as the Wiki article states, the few recent direct studies produced inconclusive results.   My belief is that double blanks at end-of-sentence helps readability and understanding (especially when used with highlighted text), and in no way makes it ugly.   I'm in favor of anything that makes it easier to understand or read.   A double-blank sentence end is a cheap thing to implement. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 07:46, 9 June 2014 (UTC)

== Table of contents ==

Where is the table of contents I think the author has screwed up something. Like a program you must be very exact in HTML or wikitext. I already found a </sup[[)]].
Cheers --[[User:Siskus|Siskus]] ([[User talk:Siskus|talk]]) 07:03, 9 June 2014 (UTC)

: Sorry, but you can't just say that I screwed up the table of contents.   The table of contents (TOC) isn't built until some predefined number of entries (==headers==) are created (I believe the threshold is four entries).   There is a way to force a table-of-contents, however. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 07:08, 9 June 2014 (UTC)
