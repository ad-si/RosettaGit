+++
title = "Talk:Population count"
description = ""
date = 2014-03-24T14:12:35Z
aliases = []
[extra]
id = 17398
[taxonomies]
categories = []
tags = []
+++

== task requirements change ==

When I entered this Rosetta Code task (for three different types of integer sequences), it never even occurred to me that the output wouldn't be labeled or titled.

So, I changed the task's requirements to included proper labeling so as to identify which numbers are being shown. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]])

:To my mind, it is not really necessary that things be labeled when they are unambiguously identified by position.  Otherwise we should all be using named parameters rather than positional parameters.  In this case you asked for three things in order, and three separate lines of output seems quite unambiguous already.  I can count to three. Or are newlines some kind of second-class citizen when it comes to whitespace?  Do we label the rows in our matrices?  Where do we draw the line on labeling everything?  What makes a label "proper"? Do we feel compelled to put a comment on every line of code?  It's easy for propriety to start looking like an obsession.  Specifying exact output formats for tasks where it doesn't much matter is also a form of improper, to my mind.  Not saying never, but you obviously make the tradeoff at a different knob setting than I would. Sorry I'm a grouch this morning.  Sorry I can't seem to talk in longer sentences this morning. <tt>:)</tt> --[[User:TimToady|TimToady]] ([[User talk:TimToady|talk]]) 16:34, 18 March 2014 (UTC)

:: Not all programming examples shown the sequences in order of them being listed in the requirements.   I thought the displaying of the output would be in order of the requirements and this being universally understood, but no.   The only way to tell what's what would be to examine the program and make the connection.   It would be almost arbitrary to flag an example's output as incorrect if there were three (correct) lines of output (but in the "wrong" order, and that is subjective), so I added the requirement so that other people could tell which output was for what sequence.   At least one entry didn't display the numbers in order of the requests, and I could see their point, the order wasn't explicitly requested, just implied, something not worth flagging as ''incorrect'', in my opinion.   ''Proper labeling'' is very simple to me: if it identifies what the output is, that's good enough (which is much better than no label).   I didn't want to cause a ruckus and extract ire out of anyone over such a minor request.   It isn't the format that is in question, but ''which'' output belongs to what sequence.   Labeling the outputs isn't unreasonable, especially in this case where each of the three distinct (popCount, evil, and odious) sequences is very similar looking.   I had misgivings of creating a Rosetta Code task that asked for three different sequences, but all three were similar enough to include into one task.   Knowing that the three outputs could easily be confused, I had assumed that people would label their output;   I was mistaken, so the identification requirement was added. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 17:18, 18 March 2014 (UTC) 

I thought I'd seen a way of highlighting a draft task (the ''requirements'' in this case) in some manner.   If somebody could inform me how to do that (or change it directly), I would be obliged.   As this is a draft task, my attempt should suffice in the interim. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 14:16, 18 March 2014 (UTC)

I finally found it (a way to highlight a draft task that may new review by authors/programmers of the examples entered.   It is:   <nowiki>{{clarified-review}}</nowiki>       -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 14:12, 24 March 2014 (UTC)

: Having written a few tasks in the past, I advise that you shouldn't get too prescriptive as to the nature of the output unless the producing of that output in that exact form is the ''sole'' reason for the task; it isn't here, since there's also the calculation of the Hamming weight itself. “Printing a list of numbers” isn't really very significant for most languages these days, after all. (Should we change the name of the task to reflect the wikipedia name for this sort of thing? “Population count” is more likely to be confused with something else.) –[[User:Dkf|Donal Fellows]] ([[User talk:Dkf|talk]]) 21:37, 19 March 2014 (UTC)

:: The form (list the numbers on one line as opposed to a vertical list) was asked for to more easily see the commonality of the three sets of outputs, thus, using a common format.   Since it is trivial for most computer languages to show each set on a single line, I didn't think it was unreasonable to have that as a requirement.   It wasn't the sole reason, but simply a matter of visual convenience.   In the task, ''population count'' was described in the first sentence (and I'll make that even clearer forthwith), and should clearly identify the reference.   ''Hamming weight'' has other uses, and for the binary case, it would be then equivalent to a ''popcount''. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 23:13, 19 March 2014 (UTC)
