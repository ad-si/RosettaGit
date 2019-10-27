+++
title = "Talk:Reverse the gender of a string"
description = ""
date = 2017-10-23T15:09:46Z
aliases = []
[extra]
id = 12444
[taxonomies]
categories = []
tags = []
+++

==Political Correctness==

This task will be fraught with much angst.  Many gender-specific words are now considered derogatory if not vulgar. 

Many of those words are no longer used or even condoned in polite society.   

The task will require a translator in context, something way above my ability.

The male/female names of various animals is problematic as there's a ''lot'' of overlap.  More so, with human names (which I elected not to enter nor support).

I was toying with the idea of flagging some of the XXXman/XXXwoman/XXXmen/XXXwomen words to make the list smaller.

Perhaps, to save space, a common list of words could be constructed and programmers could just read that file. -- [[User:Gerard Schildberger|Gerard Schildberger]] 08:18, 24 October 2012 (UTC)

: While the exact list of words would be a problem were we to be talking about doing a definitive translation, if solutions just make it so that the list of words is relatively easy to change (i.e., so that it is clear that the code isn't taking a special position on what the true mapping is) then there should be no special problem; it's just a string processing task that applies many simple transformations to some input text. Anyone who really takes issue with it can be invited to come up with the definitive mapping and samples to demonstrate with (which will keep them busy for a while). –[[User:Dkf|Donal Fellows]] 12:32, 17 March 2013 (UTC)

: And yes, I'm not converting names in my code. They'd be easy to add but why bother? You still wouldn't get all cases right. (The conversion of “<tt>his</tt>” is ambiguous, and would need a grammar checker to fix. Like I'm going to bother with that!) –[[User:Dkf|Donal Fellows]] 12:36, 17 March 2013 (UTC)

==Linguistically impossible task==
To a linguist, this task is completely laughable on many levels.  First of all, you can't reverse the gender of a string because strings don't have gender.  But even allowing that the string represents a text with gender-based references, you can at best do a partial effort at each linguistic level: the morphological level, the lexical level, the semantic level, and the pragmatic level.  And physiologically speaking, at some point you're going to be trying to decide whether certain sexual characteristics can be considered "opposite", which is unknowable without deep understanding of the text, since to calculate an opposite you have to know in which sense something is being taken, and only change one aspect of it.  Plus all the other problems.  But the lexical problems are the real killer; sometimes the opposite of "a cock" is simply "a hen".  And be careful how you cock that gun...  --[[User:TimToady|TimToady]] 23:56, 24 October 2012 (UTC)

: Do not worry. I'm sure that the text can stand quite a bit of womanhandling. (We can apply a particular set of transformations easily enough, but care would need to be taken to both respect word boundaries and to avoid input texts with the problems in your little sample. Semantic transformation is probably a Hard-AI challenge; heck, it's a big problem for people too, as I know from talking to people who do translation of novels.) –[[User:Dkf|Donal Fellows]] 09:19, 25 October 2012 (UTC)

::The fun detail is that the grammar of human languages is non-computable. So, that's that. I recall a girlfriend talking about a computing assignment which was to write a SNOBOL prog. that would replace all naughty words in a text by ... (or other marker); the SNOBOL source was of course densely packed with expletives and there was much comparison of lists.
::More seriously, how about a task to convert a word from singular to plural (and vice-versa?) - I am sick and tired of seeing *"1 errors" and the like, nor is "6 error(s)" much of an improvement. Aside from "errors", I have encountered a need for "days", and  "mismatches", but not yet "ladies". [[User:Dinosaur|Dinosaur]] ([[User talk:Dinosaur|talk]]) 11:25, 17 May 2016 (UTC)

:::Five years on, this issue still stands and this task is still a draft.  Might it be time to decide that a string:replace() with a gigantic list of value pairs isn't a very good fit for an illustrative RC task?  [[User:FreeTom]] 2017-10-23
