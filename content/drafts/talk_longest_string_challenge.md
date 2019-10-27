+++
title = "Talk:Longest string challenge"
description = ""
date = 2011-09-29T14:18:22Z
aliases = []
[extra]
id = 10310
[taxonomies]
categories = []
tags = []
+++

== Generalization ==
This is an interesting programming challenge.  An active discussion is needed to strike a balance between generalization and defeating the task. 
* I would expect to see a number of languages mark this as omit.  
* Would this only be possible for languages with implicit iteration?
* Would it be fair to relax the lists to lists of lists or supplementary lists in cases of languages like C that represent strings as arrays?
* Etc.
How about a discussion here of how this would apply to a few languages (say in the top 20 or so)?


###  Suitability and Applicability 

There needs to be an anchor for the emerging discussion on the suitability of this task for RC as the comments are under several headings.

When I borrowed this problem as a task, I did so not knowing if the task has general applicability. I did state this at the top of the talk page and I did want a discussion.  I think the discussion so far has some value.  I'm not sure about the ultimate outcome. --[[User:Dgamey|Dgamey]] 19:12, 14 August 2011 (UTC)

: It might be appropriate to put some kind of a warning see this discussion on the task page to read the discussion.  Is there a template for that? --[[User:Dgamey|Dgamey]] 19:12, 14 August 2011 (UTC)
: What options are there if the task doesn't survive?  It evolves into something else possibly removing the restrictions?  Is there a dead tasks template?  Other? --[[User:Dgamey|Dgamey]] 19:12, 14 August 2011 (UTC)

A number of observations have been made in the restrictions section.  I knew that they could be controversial.  I also knew there would need to be a discussion around them and they would require elaboration. That's happening on the talk page.  It's not clear to me yet that how this will play out. --[[User:Dgamey|Dgamey]] 19:36, 14 August 2011 (UTC)

One of the things that I like to see on this site is how to do things in other languages that you are unfamiliar with or less familiar with.  My C is very rusty and I thought it would probably get an omit for this task.  But the solution mildly surprised me and fits what I believe is the intent. Because of that it also means I'll remember that aspect of C now.  I'm willing to guess that other people on this site may have a similar sentiment.  --[[User:Dgamey|Dgamey]] 19:36, 14 August 2011 (UTC)

Whatever does happen, I'd like to see this discussion preserved.  Even if in the worst case it's only something to point people at to show why this kind of thing didn't work.  If it just gets deleted, then someone a month or a year from know could do something similar. --[[User:Dgamey|Dgamey]] 19:36, 14 August 2011 (UTC)

I'm really not convinced that this needs to be deleted, but I haven't had time to keep up with discussion. I had a response far earlier today (quoted below), but it got blocked by an edit conflict. --[[User:Short Circuit|Michael Mol]] 01:59, 15 August 2011 (UTC)
:RC is occasionally more than a straightforward chrestomathy with clear directions. Sometimes, the directions require hard thinking about the nature of a language and the various possible indirect ways to satisfy requirements or the spirit of the task. Beyond chrestomathy, RC is about illumination of the nature of problems and the tools that solve them. To that end, it's sometimes the case that the less direct the solution (by some necessity), the more illustrative it is of the shape of a language. This task also got me thinking about [http://irclog.perlgeek.de/rosettacode/2011-08-13#i_4272285 how one might distinguish between kinds of tasks]. --[[User:Short Circuit|Michael Mol]] 01:59, 15 August 2011 (UTC)
:: I'm happy that the task has generated a lot of discussion and thinking.  It seems to be a bit more of a can of worms than I'd hoped for and I really hope it can be salvaged in a fair and reasonable way. I expect that the task description will need a concise summary of the intent so people don't go too literal on it.   --[[User:Dgamey|Dgamey]] 03:20, 15 August 2011 (UTC)


###  Revised task description candidate 


: Given the level of consideration and debate, I don't want to just replace the task description without showing my hand first.  :)

Background

This problem and challenge is inspired by one that used to be given as a challenge to students learning Icon. It was intended to be tried in Icon and another language the student was familiar with.  The basic problem is quite simple the challenge and fun part came through the introduction of restrictions. Experience has shown that the original restrictions required some adjustment to bring out the intent of the challenge and make it suitable for Rosetta Code.  

The original programming challenge and some solutions can be found at [https://tapestry.tucson.az.us/twiki/bin/view/Main/LongestStringsPuzzle Unicon Programming TWiki / Longest Strings Puzzle].  (See notes on talk page if you have trouble with the site).

Basic problem statement:

:Write a program that reads lines from standard input and, upon end of file, writes the longest line to standard output.
:If there are ties for the longest line, the program writes out all the lines that tie.
:If there is no input, the program should produce no output. 

Original list of restrictions: 

:1. No comparison operators may be used.
:2. No arithmetic operations, such as addition and subtraction, may be used.
:3. The only datatypes you may use are integer and string. In particular, you may not use lists. 

An additional restriction became apparent in the discussion.
:4. Do not re-read the input file.  Avoid using files as a replacement for lists.

Because of the variety of languages on Rosetta and the wide variety of concepts used in them there needs to be a bit of clarification and guidance here to get to the spirit of the challenge and the intent of the restrictions.

:The basic problem can be solved very conventionally and that's boring and pedestrian.   The original intent here wasn't to unduly frustrate people with interpreting the restrictions, it was to get people to think outside of their particular box and have a bit of fun doing it. 

:The guiding principle here should be that when using the language of your choice, try to solve this creatively showing off some of your language capabilities. If you need to bend the restrictions a bit, explain why and try to follow the intent.  If you absolutely can't get around one, say why in your description.

:Now having said that, the restrictions require some elaboration.

::* In general, the restrictions are meant to avoid the explicit use of these features.
::* "No comparison operators may be used" - At some level there must be some test that allows the solution to get at the length and determine if one string is longer.  Comparison operators, in particular any less/greater comparison should be avoided. Various approaches allow for detecting the end of a string. Some of these involve implicitly using equal/not-equal; however, explicitly using equal/not-equal should be acceptable.
::* "No arithmetic operations" - Again, at some level something may have to advance through the string.  Often there are ways a language can do this implicitly advance a cursor or pointer without explicitly using a +, - , ++, --, add, subtract, etc.
::* The datatype restrictions are amongst the most difficult to reinterpret.  In the language of the original challenge strings are atomic datatypes and structured datatypes like lists are quite distinct and have many different operations that apply to them.  This becomes a bit fuzzier with languages with a different programming paradigm.  The intent would be to avoid using an easy structure to accumulate the longest strings and spit them out. There will be some natural reinterpretation here. 
::: To make this a bit more concrete, here are a couple of specific examples:
:::: In C, a string is an array of chars, so using a couple of arrays as strings is in the spirit while using a second array in a non-string like fashion would violate the intent. 
:::: In APL or J, arrays are the core of the language so ruling them out is unfair.  Meeting the spirit will come down to how they are used.
::: Please keep in mind these are just examples and you may hit new territory finding a solution. There will be other cases like these. Explain your reasoning. You may want to open a discussion on the talk page as well.
::* The added "No rereading" restriction is for practical reasons, re-reading stdin should be broken.  I haven't outright banned the use of other files but I've discouraged them as it is basically another form of a list.  Somewhere there may be a language that just sings when doing file manipulation and where that makes sense; however, for most there should be a way to accomplish without resorting to an externality.

:At the end of the day for the implementer this should be a bit of fun.  As an implementer you represent the expertise in your language, the reader may have no knowledge of your language.  For the reader it should give them insight into how people think outside the box in other languages.  Comments, especially for non-obvious (to the reader) bits will be extremely helpful.  While the implementations may be a bit artificial in the context of this task, the general techniques may be useful elsewhere.
    
Task    
    
Implement a solution to the basic problem that adheres to the spirit of the restrictions.  
Describe how you circumvented or got around these 'restrictions' and met the 'spirit' of the challenge.  Your supporting description may need to describe any challenges to interpreting the restrictions and how you interpretation.  You should state any assumptions, warnings, or other relevant points. 

This task is likely to encourage multiple different types of solutions.  They should be substantially different approaches. 

Given the input: (etc. as is current).
--[[User:Dgamey|Dgamey]] 13:30, 15 August 2011 (UTC)


### = Comments / Feedback =
 

: How does this work for people?  --[[User:Dgamey|Dgamey]] 13:35, 15 August 2011 (UTC)

::This looks plausible, to me.  But I am somewhat leery of a task specification that refers to individual languages.  It might not be completely avoidable, but I think I would be more comfortable if the task could be specified without Icon (and APL, C and J) being mentioned directly.  I think that that kind of thing is fine on the task page, and of course that kind of thing is fine within the part of the page dedicated to the language, but I think that having a language reference in the task itself should raise some kind of minor warning flag if it's anything other than a historical reference.
::: Let's see:
:::: I can move the reference to the Unicon TWiki up to the top to keep all the history together.
:::: The remaining references to Icon are historical/contextual such as explaining the nature of the restrictions.  It could be replaced by the language used in the original challenge but it's just awkward.  We could state that further uses are historical/contextual.
:::: In "This becomes a bit fuzzier with languages like APL, C, or J" the languages were meant as an example.  We can change this to something nebulous like "languages with a different paradigm" or we can make it clearer that these are just examples.
::::: I have changed the above line as suggested.  --[[User:Dgamey|Dgamey]] 11:51, 16 August 2011 (UTC)
:::: The specific examples for C and J could be sanitized or referred to a section here on this page. Or it could be made very clear they are just examples.
::::: I will make it clear these are just examples and add some additional generalizing text. --[[User:Dgamey|Dgamey]] 11:51, 16 August 2011 (UTC)
:::: BTW, If anyone else has some suggested rewording or improvements, I'd welcome it. --[[User:Dgamey|Dgamey]] 18:07, 15 August 2011 (UTC)
:::: Also, I added a bit about comments being helpful in the last paragraph. --[[User:Dgamey|Dgamey]] 12:09, 16 August 2011 (UTC)
::That said, I am wondering if this kind of page should belong to some special kind of category.  Perhaps <nowiki>[[Category:Gimmick]]</nowiki>?  And, if so, perhaps "Category:Gimmick" entries (or whatever you want to call them) need to follow different rules from normal tasks?  --[[User:Rdm|Rdm]] 17:00, 15 August 2011 (UTC)
::: Possibly, but Gimmick is a bit harsh. --[[User:Dgamey|Dgamey]] 18:07, 15 August 2011 (UTC)
:::: If there can be defined a good description for "this kind of task", {{tmpl|Puzzle}}, or some subclassification from that, would seem to be appropriate. --[[User:Short Circuit|Michael Mol]] 19:43, 15 August 2011 (UTC)

:: To be honest, I think this is a very poor task as currently structured. It goes out of its way to make things difficult with those restrictions (e.g., causing problems in languages that unify lists and arrays) and all without the purpose of achieving something that maps to a real life goal. By contrast, being able to handle very large numbers of lines (e.g., 10 million) where they may be long and where there may be many maxima, that actually means something vaguely useful. (Except that hardly never is line length of real relevance; I've never seen it for real.) If you insist on having the restrictions, make them Extra Credit points only. –[[User:Dkf|Donal Fellows]] 00:00, 16 August 2011 (UTC)

::: I will respectfully disagree with some of your assertions.  If every task on the site were purely utilitarian, we could cut out a fair percentage of tasks.  Many also don't address real life goals.  Anyone is free to write another task to handle millions of lines.  Part of being a draft task is that things need working out.  This task started with a straight copy and we are seeing if it can be adapted and generalized. Right now it's a bit of an experiment.  I'm surprised that I've seen little in terms of positive suggestions about how to evolve this.  Simply making it optional defeats the whole point.  --[[User:Dgamey|Dgamey]] 02:38, 16 August 2011 (UTC)

:I quite like the rewrite. I wouldn't want to go so far as to add something like <nowiki>{{puzzle}}</nowiki> until we've all had a chance to see how things go with this task; and I think the name puzzle might be so easily miss-used on game tasks we have had in the past, that did not need the distinction. --[[User:Paddy3118|Paddy3118]] 05:57, 16 August 2011 (UTC)
:: Thanks Paddy. I think it needs a wee bit more work.  I'm not sure if I should add the change to the main page so people don't miss it or give it a couple of days for feedback here.  I also think Puzzle is the wrong name for that reason.  It could be a stretch, a challenge, or we could add the word programming to it. --[[User:Dgamey|Dgamey]] 11:51, 16 August 2011 (UTC)


### Ready to leave Draft?

The task and talk page seems to have been stable for a while.  I'm thinking it's about time to remove draft status.  If I don't see more activity, I'm thinking that this should be done at the end of September.  --[[User:Dgamey|Dgamey]] 03:22, 9 September 2011 (UTC)


==Similar to [[Averages/Mode]]==

The problem statement itself (without the restrictions) is very similar to [[Averages/Mode]], except that you do not need to compute the counts of elements, instead taking the length of the string serves as its "count". --[[Special:Contributions/208.80.119.69|208.80.119.69]] 00:43, 13 August 2011 (UTC)
:Except that in mode you can use comparison operators and arithmetic operators. You're not allowed to do either here. Besides that, I still don't think it's really that similar. --[[User:Mwn3d|Mwn3d]] 00:59, 13 August 2011 (UTC)

== Can't load https link ==
:Firefox won't load the reference link due to invalid SSL cert. --[[User:Ledrug|Ledrug]] 00:08, 13 August 2011 (UTC)
:: In firefox you can *temporarily* accept the cert.  The site is scheduled for upgrade and I can ask if there is a way that http can be allowed for anonymous users.  --[[User:Dgamey|Dgamey]] 11:55, 13 August 2011 (UTC)
:: The TWiki Twisty (show/hide) appears to break in newer firefox and IE.  If you have something like NoScript and don't trust the scripts on the site it will show all of the hidden material below the headings.  --[[User:Dgamey|Dgamey]] 11:55, 13 August 2011 (UTC)

==Rereading==

I think the task description should ''explicitly'' forbid (or permit, if that was the intent) reading the input twice. —[[User:Kevin Reid|Kevin Reid]] 03:08, 13 August 2011 (UTC)
: An interesting point.  Since I was not the originator of the challenge, I can't answer.  My take is if it isn't explicitly forbidden and it's not a cheat on one of the restrictions it should be allowed without stating it explicitly.  As there are a lot of very creative people out there the list of techniques could get quite large. --[[User:Dgamey|Dgamey]] 12:20, 13 August 2011 (UTC)
:: Well, my preference is that given that the task is to read from "standard input", and standard input may not be a seekable file, this is prohibited implicitly (and so should also be prohibited explicitly to avoid permitting semi-broken solutions). —[[User:Kevin Reid|Kevin Reid]] 14:43, 13 August 2011 (UTC)
::: Now you have me curious, you must have a rereading solution in mind.  If the description were to read from standard input or a file, that would allow rereading.  I'd probably want to see a comment to the effect that it would break on stdin.  Since this is more about alternate/creative ways around the restrictions, I think it would be fine.  --[[User:Dgamey|Dgamey]] 07:09, 14 August 2011 (UTC)
:::: If you can read the file twice then there's no need to keep an accumulator.  Read once to find longest length, read again, along the way print anything that long.  --[[User:Ledrug|Ledrug]] 07:47, 14 August 2011 (UTC)
::::: Ah, obvious (now that I'm up and had coffee).  As worded the task also doesn't necessarily prohibit the use of a temporary file either. In Icon a file is a datatype and could be argued as a cheat.  Not sure about other languages. --[[User:Dgamey|Dgamey]] 15:13, 14 August 2011 (UTC)
:::::: The more I think about rereading and see some of the discussion on it, I think it should be forbidden.  --[[User:Dgamey|Dgamey]] 04:18, 15 August 2011 (UTC)

==Restrictions==

What is the purpose of the restrictions? This is a chrestomathy. We would get a better language comparison, if we removed the restrictions. [[User:Markhobley|Markhobley]] 09:24, 13 August 2011 (UTC)
: The basic task is very simple, the point of the restrictions is try and get people to think a bit out of the box and make it a bit more challenging. --[[User:Dgamey|Dgamey]] 12:00, 13 August 2011 (UTC)
::I'm not sure whether that is the objective of rosettacode, because this is a chrestomathy. If the task has a good implementation that can be achieved through the use of operators, then we should be able to use them here. This could possibly be a challenge for a programming challenge site, but I don't know of one off hand. Alternatively, we could possibly make the restrictions optional. [[User:Markhobley|Markhobley]] 14:55, 13 August 2011 (UTC)
::: The restrictions were really part of the original intent and not an optional add on.  That is the intent is to see ways of dealing with things a bit out of the ordinary.  I would argue that there are other puzzles and challenges on the site and that limitations are not counter to the intent of the site.  There is also nothing preventing a [[Longest string]] task either.  --[[User:Dgamey|Dgamey]] 18:57, 13 August 2011 (UTC)
:::: These restrictions are self contradictory, unless perhaps viewed through the blinkers of some particular language specifications.  For example, a string is a list of characters but some languages might obscure this issue.  For example, anything which conditionally chooses between two options involves some sort of comparison but a restriction on "comparison operators" might make sense in the context of a specific language spec.  And so on...  So I think this is a bad problem for this site.  It assumes too much about the language used to implement the problem and since it's doing this in the restrictions it is essentially restricting the languages used to implement the task, which I believe is a no-no here.  --[[User:Rdm|Rdm]] 17:24, 14 August 2011 (UTC)

::::: I too am wary about the general applicability of this task. --[[User:Paddy3118|Paddy3118]] 18:29, 14 August 2011 (UTC)

:::::: I started a section on general suitability.  I understand you being wary but still think the discussion may be useful in several ways.  Understanding how tasks get defined clarified for example.  It's not a science by any stretch.  --[[User:Dgamey|Dgamey]] 19:26, 14 August 2011 (UTC)
:::::: The intent of the restrictions was to make it a bit more interesting, not just to exclude things.  I've already said the restrictions need some more explanation to find the right balance.  --[[User:Dgamey|Dgamey]] 19:26, 14 August 2011 (UTC)
::::::: Maybe think again about my suggestion of making the restrictions optional. Possibly something like. "Optionally try to produce an implementation that does not utilize foo or bar." (Where foo and bar are the facilities that you want to restrict). [[User:Markhobley|Markhobley]] 19:44, 14 August 2011 (UTC)
::::::: Remember also that it is the interest of the reader that is of primary concern, rather than the interest of the implementer. If you want to challenge the implementer, then maybe another site would be more suitable for this. Restricting things makes the provided code less optimal and less practical. [[User:Markhobley|Markhobley]] 19:51, 14 August 2011 (UTC)

::::::::: I'm actually thinking of both reader and implementer.  If done properly, the reader should learn something more than just looking at straight translations.   --[[User:Dgamey|Dgamey]] 20:02, 14 August 2011 (UTC)

:::::::: I'm not there yet as I want to hear more.  I am beginning to think that the intent isn't being stated in a positive way.  Restrictions are by definition negative.  What was the original intent here?  Really, it was to get people to think outside of their particular box.  This problem can be solved very conventionally and that's boring and pedestrian. The point perhaps is in your language of choice how would you solve this creatively showing off some of your language capabilities? This also lets people have a little bit of fun with it. The restrictions could be more guidance/example in this case.  --[[User:Dgamey|Dgamey]] 19:56, 14 August 2011 (UTC)

::::::::: If that is is the point, then rather than placing restrictions, just make that point in the task description, and let the implementer make the decision as to how this is best achieved. Maybe say "Demonstrate this creatively showing off some of your language capabilities." [[User:Markhobley|Markhobley]] 07:46, 9 September 2011 (UTC)
:::::::::: Borrowed from this idea in the last revision.  I think there is a good balance between rigid restrictions and anarchy :) --[[User:Dgamey|Dgamey]] 14:17, 29 September 2011 (UTC)

::::::::: (I find I am enjoying following the discussion and ultimately hope we ''can'' get the 'nuance' right to make this a good task). --[[User:Paddy3118|Paddy3118]] 09:23, 15 August 2011 (UTC)

== Intent of Restrictions==

Beyond thinking outside the box, the restrictions need to be clear and will probably get reworded.
: Because the original problem came from an Icon site and talked only peripherally about implementing in other languages, I expected that the task description would require clarification.  Also, my intent was not so much to use the restrictions as a way of excluding languages; hence the discussions here are really about clarifying the tasks description to allow solutions without eliminating the challenge aspect. --[[User:Dgamey|Dgamey]] 06:10, 14 August 2011 (UTC)
:: I think based on discussions re: 'explict' operations the intent is coming out.  I'd like to leave it for a few days and see if the discussion stabilizes before updating the task description with specifics. I'll temporarily add a note to refer to the discussion re: intent.  --[[User:Dgamey|Dgamey]] 06:15, 14 August 2011 (UTC)
::: Given the interesting discussions, I think what will happen is that the clarification will include the current restrictions as example and something more will be needed for the core. --[[User:Dgamey|Dgamey]] 04:11, 15 August 2011 (UTC)

All of the restrictions apply to any called routines such as libraries.  I suppose if there was a native built-in library function that returned the longest strings that would be allowable; but, library routines shouldn't be used to cheat on the restrictions.  --[[User:Dgamey|Dgamey]] 12:35, 13 August 2011 (UTC)


###  No comparisons 

: "No comparison operators may be used."
::  This would also cover built-in comparison functions like lt(a,b).  Instead of "operators" it should probably read "operators/functions" or "operations" possibly with "built-in" in front.  Now if you can write a comparison function that doesn't use these things, that would work. --[[User:Dgamey|Dgamey]] 12:35, 13 August 2011 (UTC)
::: (1) Do equality operators count as comparison operators? In particular, do "pointer-equality" operators which do not even do data-structure-specific examinations count? (2) Do pattern-matching (regular expressions, structural patterns, etc.) operators count? —[[User:Kevin Reid|Kevin Reid]] 14:44, 13 August 2011 (UTC)
:::: Based on the discussions below about the C cmp() function, I think the main intent was to avoid explicit less/greater than comparisons and comparisons of the contents of the data (i.e. two strings). Beyond end of string detection, I'm not sure how equality tests would help.  So I think the answer to (1) is no.  The question in (2) is a bit more complicated, the Icon solution (and others on the original TWiki) use the built in string scanning/pattern matching to consume length so this kind of thing was not intended to count as a comparison.  A full-blown regular expression might allow other ways to solve this than by consuming length.  Provided it avoided explicit comparison and math it should probably be allowed.  It would have to be a built-in regexp as a regexp procedure in the source language would likely violate one or more of the restrictions.  But also, because of it's portability it really isn't showing off the characteristics of the language even if reg exp is built in and is a bit of a cheat in that sense.  If it were the only way to do it, then yes, but if there were other ways to do it I think the intent of the task is to see those other ways.  I could see some people submitting a regexp and non regexp version to show the variety.  --[[User:Dgamey|Dgamey]] 06:00, 14 August 2011 (UTC)


###  No arithmetic operations 

: "No arithmetic operations, such as addition and subtraction, may be used."
:: I think this is clear. --[[User:Dgamey|Dgamey]] 12:35, 13 August 2011 (UTC)
::: Based on discussions below this is really about explicit math.  It wasn't intended to exclude functions that implicitly advance a pointer/cursor.  --[[User:Dgamey|Dgamey]] 06:00, 14 August 2011 (UTC)
:::: In my brain, <tt>i++</tt> sits smack dab in the middle between the two. It is expressly an arithmetic operation (in adds 1 to i, thus forbidden) but is almost never used that way. Instead it is usually used in the way that you allow up there as something that implicitly advances a pointer (<tt>r = s[i++]</tt> or some such). Is that allowed or restricted?
::::: A number of languages have this as a short hand for <tt>i = i + 1</tt>, so I think it doesn't pass.  There are a couple of solutions that use operations like substrings and detect failures/errors/etc. One of these might work for you?  --[[User:Dgamey|Dgamey]] 23:26, 15 August 2011 (UTC)


###  Use only integers and strings 

: "The only datatypes you may are use are integer and string. In particular, you may not use lists."
:: To avoid semantic arguments, "lists" means lists/arrays/vectors in the broader sense.  However, clearly in the case of C, where strings are arrays of characters, this needs to be relaxed a bit.  Using arrays for other than strings would be a cheat here. --[[User:Dgamey|Dgamey]] 12:35, 13 August 2011 (UTC)

::: These restrictions are utterly nonsensical in J.  Consider for example, in the C implementation <code>p = &p[1]</code>.  Is that an arithmetic operator?  In essence, it's computing <code>p= p+1</code>.  Anyways, in J, the restrictions make no sense except by reinterpreting the meaning of the language in much the way that <code>p = &p[1]</code> may be reinterpreted as <code>p= p+1</code>.  --[[User:Rdm|Rdm]] 17:36, 14 August 2011 (UTC)

:::: Since I don't know J, I really can't say and I can't really understand how they are nonsensical. I have to take your word on that.  But I'm curious and would like to understand more.  Beyond that I think the applicability of the task should be discussed in one place.  It's starting to fragment. --[[User:Dgamey|Dgamey]] 19:02, 14 August 2011 (UTC)

::::: Ok, here is my attempt at helping you to understand more:
::::: Part 1:  J has only one data type: array.  So any part of a task having to do with data types has to be interpreted liberally.  Meanwhile, a list is a kind of array.  Strings are are a kind of list.  So what does the restriction against lists mean?  Is it a prohibition on arrays?  Is the point to eliminate J's core design concepts?
::::: Part 2: what is a "comparison operator"?  (I have no clue what is being restricted here.)
::::: Part 3: what is a data type?  (See part 1). --[[User:Rdm|Rdm]] 20:16, 14 August 2011 (UTC)
:::::: Ok, lets start with my intent was not to rule out J.  You also might want to look at the (currently) last paragraph under Restrictions (search for "boring and pedestrian") and see that I'm thinking that the task needs to be specified more positively.  Toward that end, I'd appreciate your input.  I also appreciate that J (and APL and others) are sufficiently different in approach that there are creative approaches that would be different from other languages and also that you may very well be correct that it doesn't make much sense.  The pedestrian solution is to loop through the input building a list/array of the longest strings testing length using gt/lt.  J rarely does things in such a pedestrian and serial manner.
:::::: Re Pt1 and 3.  I thought J had (data)types inside arrays like numbers and literals.  I also thought J had operators for dealing with literals that were not generalized to arbitrary dimensioned arrays.
:::::: Re Pt2.  Stepping back this seems to be focusing on avoiding direct comparison of length as in the pedestrian solution. Most of the posted solutions are looking at some secondary way of detecting end of string. 
:::::: Does any of that help?  As a start? --[[User:Dgamey|Dgamey]] 21:45, 14 August 2011 (UTC)
::::::: Ok, yes, J has data types inside of its arrays, they are mostly invisible, but they can be grouped into:  numeric, literal (which means that it's an array of characters), and boxed (which means that it's an array of references to arrays).  So, hypothetically speaking, we could claim that "no list" really means, in the context of J, no boxed arrays" -- this means we can have arrays of characters with multiple dimensions even though in another language a person might think of this as "lists of strings".  That might leave use with a solution like <code>(];._2 ([ #~ -:"1) [:|."1|.;._2)@(1!:1)</code> but would this be legal?  It's using the -: verb, which tests for an exact [http://jsoftware.com/help/dictionary/d122.htm match].  But is that a comparison operator?  If the point is to not be testing on length, then this might or might not be legal, depending on what that concept really means. 
::::::: But, ok, you probably do not know enough J to read that sentence, so here's a description:  Using the end of line marker, form the characters into a two dimensional array (padding short lines with spaces).  This is the left argument to the inner expression.  The right argument is formed using the same technique, except that we reverse each line in the process of forming them into an array and then reverse each row again after it has been padded.  The inner set of parenthesis contains an expression which will keep the rows which are identical in both copies.
::::::: That said, it's probably also worth noting that most J programming involves "thinking outside the box" if "the box" means "traditional approaches used in traditional languages".
::::::: Also, for reference, here is a "use length directly" approach: <code>(#~ (=>./)@:(#@>))@(<;._2)@(1!:1)</code> and note that it does not use any "comparison operators" except = and that it's not doing any "arithmetic".  It is, however, finding the maximum length and using that to select the relevant rows.
::::::: --[[User:Rdm|Rdm]] 01:08, 15 August 2011 (UTC)
:::::::: Oh, I knew that this (adapting the task) was going to hurt :)  And I get that J thinking tends to be outside the box, which is why I'd be very disappointed if we could not find a way to fit this task to J.  It's really a question of how to respecify it.
:::::::: From what I've seen in Icon on the TWiki and in some of the submissions here, just avoiding comparing lengths directly via lt/gt pretty much satisfies.  The Icon solutions, C, python, are using techniques that detect end of string indirectly.  They don't so much find the maximum length as creep up on it - detecting that one more character exists the two posted Icon solutions do this through string scanning move consumes characters in the current string and by testing for the existence of the i-th character of a string.  The avoidance of lists was just to find another way of accumulating the results instead of the easy reset the list of strings each time we see a longer list.  Solutions involve using recursion or just a long string as an accumulator work well.
:::::::: I'm barely following your explanations (not your fault at all).  Do you mean no boxed strings (does that even make sense)? As for the first explanation, if we have 'ab' and 'abc' you will be matching 'ab ' & 'abc' vs. ' ba' & 'abc'.  Not directly using length.  The C example (cmp) is another example of indirect pointer incrementing and using equality testing for the end of string.  I think the C example passes the intent.  --[[User:Dgamey|Dgamey]] 03:59, 15 August 2011 (UTC)
:::::::::: Yes, if we forbid "boxed" data, we forbid boxed strings.  But if we change the restriction to "do not represent the length of any line as a number" then that (very J specific) detail might be irrelevant.  That said, my first example would check 'ab' and 'abc' using [using a javascript-ish notation]: <nowiki>match('ab ',' ab')</nowiki> and match('abc','abc').  --[[User:Rdm|Rdm]] 10:51, 15 August 2011 (UTC)
::::::::: I think, the solution you suggest treads a fine line - which I think is ok because of the approach.  The same structures could be used to read the entire file into memory and performing two passes - which I would think is on the wrong side of line.  There's been a suggestion elsewhere that rereading should be prohibited (and I tend to agree).  So some of this (where the line should be) is going to be subjective. For example, the recursive solution uses recursion effectively as a way of rereading the strings. Yet it feels like it meets the intent.  Ultimately, this task may encourage multiple solutions.  --[[User:Dgamey|Dgamey]] 04:27, 15 August 2011 (UTC)
:::::::::: Ultimately, you might say that J is all about "doing multiple passes".  Reading the file, in J takes a reference to a string representing the file name and returns a string representing the file contents.  At that point, it's a done deal:  The moment you do two operations on that string you are doing "multiple passes".  I can think of no meaningful way to prohibit 'multiple passes' and still allow a J solution.  That said, there are good (efficiency) reasons for doing it this way, for most tasks.  But that can get into a long discussion of computer memory architecture, and efficiency and is not one I want to start in a paragraph indented by <code>::::::::::</code>, but the short form is that cpu caches are optimized for serial processing.  And, yes, this can create an issue when you get into multi-gigabyte files.  One approach, in that case, would be taking the solution that worked on shorter files and recasting that as working on "blocks".  This typically means you do something special about the line which gets broken between blocks and for combining results from different blocks.  Thus you would expect a solution about 3 times the complexity of what you would need for a single file.  (But when you are working with data that big, this approach can be much more efficient than "line at a time" approaches.  But a more important issue would be that most files nowadays should not be multi-gigabyte files.  That said said, there's also a language implementation issue, here, that gets into how memory mapped files get handled.) --[[User:Rdm|Rdm]] 10:51, 15 August 2011 (UTC)
::::::::::: Ok, but the salient difference is you're not doing a cheap trick with the second pass.  :)--[[User:Dgamey|Dgamey]] 21:45, 15 August 2011 (UTC)

== Not pointless ==
I had to chuckle at the comment on the C submission as I think it proves the point quite well. Clearly, I need to document the point better :)
: While I think about how to word it, let me explain.  This site allows people to compare implementations in different languages.  Restrictions force people to find other ways. To  choose the road less traveled.  Now this may require a technique that would be useful in some other problem but perhaps overkill in the simple unrestricted case.  This can show people different techniques.  The C contribution's cmp procedure is a case in point (I think).  My C is very rusty (entirely seized up) and it doesn't look like a cheat to me.  But I only have an inkling of how it may work.  I'd have to crack my old copy of K&R and scratch my head a bit for it to be clear. A fluent C programmer would 't need to, but on the other hand they wouldn't be on this site to read code.  Where I failed in the task description is to require people to provide some description of how they got around the restriction so that people coming from other languages will be able to understand.  I'll add this to the task description. --[[User:Dgamey|Dgamey]] 13:01, 13 August 2011 (UTC)
:: Well, the <code>cmp</code> function doesn't contain arithmetic operators is not the same as the code uses no arithmetics.  <code>&p[1]</code> is taking address of the next element of pointer <code>p</code>, i.e. <code>p + 1</code>.  And <code>if (p)</code> is the same as <code>if (p != 0)</code>, so one may argue if that contains comparison operator: it certain contains comparison ''operation''.  If these are allowed, because pointers can be converted to integers, there really isn't any restriction on arithmetics now: you can increment p by &p[1] and decrement it by &p[-1], you can compare it to zero, then you can do addition and subtraction; with those you can do mulplication and division; if you work hard enough you can probably get a whole math library in there. --[[User:Ledrug|Ledrug]] 19:57, 13 August 2011 (UTC)
::: Thanks for the explanation, my rusty old C memory had it about half figured out.  Now while there is admittedly an end of string comparison and pointer advancement happening, I think this passes the spirit of the task.  The implicit end of string test is a != comparison for a very specific purpose and the addressing is implicitly moving through a string.  Looking at the solutions posted to the Unicon TWiki, automatically advancing a character was fine but if someone used s[p+1] it was considered in violation of the intent. --[[User:Dgamey|Dgamey]] 05:33, 14 August 2011 (UTC)
::: Also, Icon is implicitly advancing a cursor (&pos) inside the string scanning/matching which strikes me as equivalent to what is happening inside of cmp.  --[[User:Dgamey|Dgamey]] 06:04, 14 August 2011 (UTC)

==Icon uses boolean datatypes==
'move(1)' must return something coerced to a boolean for the if statement/expression to work on its return value. In fact, their are more conditionals, so things are getting coerced to booleans there too. I think the problem statement could prove tough to tie down ;-) 
 --[[User:Paddy3118|Paddy3118]]

Yet more. From the Icon comments:
:''move(1)  - succeeds/fails ...''
So move is a conditional itself. Which is not allowed. So the Icon example does not meet the conditions of the task statement!?

The language of the task is very loose and seems to be set up to dazzle students unwilling to argue a point when learning a new language. --[[User:Paddy3118|Paddy3118]] 19:32, 13 August 2011 (UTC)
:: Icon success/failure is more like exception than data, in that they implicitly propagate outwards.  In any event, some boolean stuff has to be there, because eventually one has to answer "is this longer than before".  I think the goal is to avoid ''explicitly'' storing anything into a variable other than ints and strings, or ''explicitly'' doing math.  It's gimmicky for sure, but it could be fun. --[[User:Ledrug|Ledrug]] 20:09, 13 August 2011 (UTC)
::: I'm not sure about ''explicitly'' storing, but the spirit of the task *is* to avoid ''explicit'' math and comparisons.  In particular, avoiding explicit less/greater comparisons.  --[[User:Dgamey|Dgamey]] 05:19, 14 August 2011 (UTC)
:: Icon has no boolean type.  Expressions succeed (returning a value) or fails and do not.  It's a form of short circuit evaluation.  For example, it is not possible in Icon to run out of bounds of a list, string, etc. as doing so fails.  'move(1)' returns the next character in the string being scanned or fails if there is none.  --[[User:Dgamey|Dgamey]] 05:19, 14 August 2011 (UTC)
::: Similarly, s[n] succeeds returning the n-th element of s (string or list) or fails  --[[User:Dgamey|Dgamey]] 06:55, 14 August 2011 (UTC)

==Request for Python explanation==
Hi Ledrug,

Could you give me a brief explanation as to why your later Python example of:

```python
import fileinput

# return len(a) - len(b) if positive, 0 otherwise
def longer(a, b):
    while len(a) and len(b):
        a, b = a[1:], b[1:]
    return len(a)

longest, lines = '', ''
for x in fileinput.input():
    if longer(x, longest):
        lines, longest = x, x
    elif not longer(longest, x):
        lines += x

print(lines, end='')
```


Replaces:

```python
import fileinput
import operator as op
 
maxlen, maxlines = 0, ''
for line in fileinput.input():
    ll = len(line)
    if op.gt(ll, maxlen):
        maxlen, maxlines = ll, line
    elif op.eq(ll, maxlen):
        maxlines = op.concat(maxlines, line)
print(maxlines, end='')
```

It would help me understand the task a little better. Thanks. --[[User:Paddy3118|Paddy3118]] 06:39, 14 August 2011 (UTC)
: Under "No comparisons" above, Dgamey specifically listed "no comparison functions like lt(a, b)", so that pretty much ruled out <code>op.gt</code>.  <code>op.concat</code> would have been fine, but <code>lines += x</code> is simpler.  The <code>longer</code> function uses only substring and boolean operations (the <code>and</code> can be turned into nested <code>if</code> blocks if booleans ops are not allowed), so I think it does not breach the requirement of the task. --[[User:Ledrug|Ledrug]] 06:51, 14 August 2011 (UTC)
::Ta. --[[User:Paddy3118|Paddy3118]] 09:31, 14 August 2011 (UTC)

== bounds checking in C ==

The last revision to the page claims:  "Silently wrong result is no better than garbage output or crashing."

I believe this claim is false, since crashing can lead to machine compromises in contexts where the data comes from elsewhere.  --[[User:Rdm|Rdm]] 02:51, 15 August 2011 (UTC)
:Segfault isn't all that different from <code>kill -9</code>, or even calling <code>exit()</code> for that matter: the process is gone, along with all its memory pages and file handles, leaving not much to be compromised.  A crashed program of course can leave behind some inconsistent state around such as half written files, but that's not a problem here.  The C code can overrun buffers even if we use <code>fgets</code> (and the fgets length should be 1 less anyway), so the last fix didn't really fix anything, only adding a possibility of wrong result besides crashing.  If there is a chance for the program to fail and we are not going to completely prevent it, I'd rather have it fail more obviously. --[[User:Ledrug|Ledrug]] 03:13, 15 August 2011 (UTC)
::Segfault is not the only possible outcome from buffer overflow.  Also, it's my understanding that the length argument to fgets is the buffer size -- if it's 65536 then a maximum of 65535 characters will be read as the final character to be placed in the buffer must be null.  That said, if there were some other way to crash the program, I would like to understand it, and I would also like for that issue to be fixed.  --[[User:Rdm|Rdm]] 17:07, 15 August 2011 (UTC)
:::The fgets needs to be 1 less because of the strcat of a newline right after it.  Even with fgets, the accumulator buffer <code>buf</code> can still be overrun when we add lines to it (suppose the input has 2000 lines each 1000 chars long, for example).  If you want to be safe, well, 
```c>#include <stdio.h

#include <string.h>
#include <assert.h>

int cmp(const char *p, const char *q)
{
	while (*p && *q) {
		p = &p[1];
		q = &q[1];
	}
	return *p;
}

int inc(int x) { return (int)&((char *)x)[1]; }
int dec(int x) { return (int)&((char *)x)[-1]; }
int gt(int x, int y)
{
	while (y && x) y = dec(y), x = dec(x);
	return x;
}

int add(int x, int y)
{
	while(y) x = inc(x), y = dec(y);
	return x;
}

/* strlen(a) + 1 */
int length(char *a)
{
	char *x = 0;
	while (*a) a = &a[1], x = &x[1];
	return (int)x;
}

#define LINE_MAX 10
#define ACCU_MAX 30
int main()
{
	char line[LINE_MAX];
	char buf[ACCU_MAX] = {0};
	char *last = buf;
	char *next = buf;
 
	while (fgets(line, LINE_MAX, stdin)) {
		/* check that fgets didn't truncate line, or result will be wrong */
		assert(gt(dec(LINE_MAX), length(line)));

		if (cmp(last, line)) continue;
		if (cmp(line, last)) next = buf;
		last = next;

		assert(!gt(add(length(buf), length(line)), ACCU_MAX));

		strcpy(next, line);
		while (*next) next = &next[1];
	}
 
	printf("%s", buf);
	return 0;
}
```
 I haven't carefully checked all the comparisons in the above, but it's on the right track at least. --[[User:Ledrug|Ledrug]] 00:17, 17 August 2011 (UTC)
:::: Ok, I see what you are saying.  I have updated the C implementation to avoid buffer overflow problems.  Note that this will not prevent crashes nor erroneous results -- it's only correct if the lines are short enough and if few enough of them are long.  --[[User:Rdm|Rdm]] 01:13, 17 August 2011 (UTC)
::::: Er I'd say <code>size <<= 1</code> qualifies as arithmetic operator.  You could use the <code>add()</code> function above, but it gets really ugly looking, which is why I didn't bother to begin with. --[[User:Ledrug|Ledrug]] 01:41, 17 August 2011 (UTC)
:::::: It could be replaced by a function which returns the next available size (implemented as a large switch statement).  --[[User:Rdm|Rdm]] 04:03, 17 August 2011 (UTC)
::::::: Heh.  I said somewhere on this page before, this task is gimmicky, and I wrote some gimmicky code as a proof of concept -- for fun.  Then people came around and started to pile common sense stuff on it: <code>const</code> pointers, <code>fgets</code>, <code>realloc</code>, etc.  The problem is, this is not a common sense task.  We know that the buffer can be overrun, but what are the consequences? That someone will put the code on a webserver with root priviledge and accept unsanitized data?  Or will it be run as a system daemon?  I highly doubt it.  I'd be fine with a giant, red, flashing warning label about buffer overrun, but making it correct by burying the (somewhat) interesting part with two hundred lines of safety code is going out of hand.  I would suggest just slap on a warning label and leave it at that.  The task requires a handicapped program, and we have a handicapped program, it's fitting. --[[User:Ledrug|Ledrug]] 05:20, 17 August 2011 (UTC)

:::::::: Now you 've pointed it out, it does verge on the absurd :-)
 There is also much more talk than task entries. --[[User:Paddy3118|Paddy3118]] 05:59, 17 August 2011 (UTC)
::::::::: I think I'll post the revised task descriptions with a small tweak about warnings.  The intent was for demonstration code with some documentation.  It would be fine to point out that the code isn't secure in the description.  Now we've got all kinds of secondary functions.  I'd rather see comments on the line p = &p[1] and the while loop showing the reader with little C what's happening.  
::::::::: A separate task or family of tasks showing various safe vs. unsafe practices might be worth considering.  --[[User:Dgamey|Dgamey]] 11:39, 17 August 2011 (UTC)

::In other words, this program will not block for input:  
```c>#include <stdio.h

main() {
        char buf[9];
        fgets(buf, 1, stdin);
}
```
 --[[User:Rdm|Rdm]] 17:10, 15 August 2011 (UTC)
: A buffer overflow is a security violation. Input, that overflows the buffer, might overwrite the return address and hijack control of the program. A correct program would check bounds and report an error (or realloc() a longer buffer). Does anyone know how to check bounds without any comparison operators? --[[User:Kernigh|Kernigh]] 21:34, 15 August 2011 (UTC)
::Use functions with bounds-checking built-in, such as strncpy. (Or one of Microsoft's _s extensions). --[[User:Short Circuit|Michael Mol]] 23:24, 15 August 2011 (UTC)
::Alternately, XOR your return value, and use C's "0 is false, nonzero is true" behavior in conditional expression evaluation. --[[User:Short Circuit|Michael Mol]] 23:24, 15 August 2011 (UTC)

=== bounds checking in C, take 2 ===
:I do not think that C code with buffer overflows should ever be considered to be simpler than C code that guards against them.  It might be faster, but any apparent simplicity is deceptive since the possibility of buffer overflows pushes complexity out onto the user.  That said, gets() here requires strcat(), because gets() drops the end of line character.  Meanwhile fgets() does not trim off that character.  And bounds checking could be implemented using memset() and then using the implementation's cmp() between a reference and an appropriately choosen spot near the end of buffer.  So if we are will to pay the minor increase in complexity to use gets instead of fgets, I am not sure why we are not willing to pay a few extra lines to get an implementation without buffer overflow. --[[User:Rdm|Rdm]] 16:14, 17 August 2011 (UTC)
:: 1) I added a second version without buffer issues.  2) With fgets, you still need to check if the line is too long and was truncated; if it is, we are pretty much screwed and need to be able to read in the rest of the line and append it to the line buffer, which is a lot of work because of the "no arithmetics" clause.  The second method does handle arbitrarily long input, but see how much more complicated it is.
:: It's not that I particularly love gets or buffer overruns, and "faster" is totally not a concern for this task, the key issue is how easy it is to read the code.  The original C code was really just demonstrating how to compare the lengths of two strings without directly measuring them, and I deliberately did not pretend it was safe code (I've ''never'' used gets before); declaring const and fgets, to me, feel like false promise of safety.
:: That said, if you can check bounds and truncation correctly without making it overly complicated, feel free: I can't think of a way to do it without integer math, like in the second solution. --[[User:Ledrug|Ledrug]] 18:27, 17 August 2011 (UTC)
::: Given that the current version does not always work, I do not see anything wrong with a replacement version which does not always work.  That said, I think I prefer "exit with an error code" over "crash" for behavior in adverse conditions.   But, ok, I'll try for a bounds checking version.  --[[User:Rdm|Rdm]] 18:32, 17 August 2011 (UTC)
:::: The code I posted above on this talk page does that: it aborts if buffer would be overrun or line is too long.  The second code I posted on task page only aborts on realloc failure and should function correctly if that doesn't happen.  ''None'' of the changes made by various people to the original code could garantee a correct result or a timely abort, which is why I don't feel it was worth the trouble. --[[User:Ledrug|Ledrug]] 18:45, 17 August 2011 (UTC)
::::: What do you think of the version I added? --[[User:Rdm|Rdm]] 21:02, 17 August 2011 (UTC)
:::::: <code>if (!longer(bufend, line))</code> isn't really checking for accumulator overflow, is it?  Also, if you use <code>memset</code> to mark unused buffer region, they should to be called every time the accumulator is reset (i.e. found a new longest line), or it would be possible that there's a null byte in the tail region of <code>buf</code> before the reset, but accumulated strings haven't reached there yet, and next <code>if (!longer(bufend, line))</code> can give a false positive on overflow.  (I think) --[[User:Ledrug|Ledrug]] 21:24, 17 August 2011 (UTC)
:::::EDIT yes that does happen.  Modified the code to this: (LINE_MAX is your 1000000, your buffer would have been <code>#define ACCU_MAX (11*LINE_MAX + 1)</code>, but the point is the same:
```c
#define LINE_MAX 6
#define ACCU_MAX (4*LINE_MAX+1)
int main() {
	char line[LINE_MAX];
	char buf[ACCU_MAX];
	char *linend= &line[LINE_MAX - 1];
	char *bufend= &buf[ACCU_MAX - LINE_MAX - 1];
	char *last = buf;
	char *next = buf;

	memset(line, 1, LINE_MAX);
	memset(buf, 1, ACCU_MAX);
	buf[0]= buf[ACCU_MAX - 1]= 0;
	while (fgets(line, LINE_MAX, stdin)) {
		if (!*linend) exit(1);
		if (longer(last, line)) continue;
		if (!longer(bufend, line)) exit(1);
		if (longer(line, last)) next = buf;
		last = next;
		strcpy(next, line);
		while (*next) next = &next[1];
	}
```
 and it aborts on the input <code>echo "aa\nbb\ncc\ndd\nee\nff\naaa" | ./a.out</code> but works if you remove any of the doubles. --[[User:Ledrug|Ledrug]] 21:28, 17 August 2011 (UTC)
:::::: I do not consider "false positives" to be a bug.  That's a (very minor) efficiency issue, but the point was to avoid buffer overflows, not to squeeze every last ounce out of the buffer.  So I have one extra byte in the line buffer, and an extra line buffer sized region in the big buffer.  If you prefer, imagine that those particular bytes were part of other variables. --[[User:Rdm|Rdm]] 21:35, 17 August 2011 (UTC)
:::::: Eh.  If it's garanteed to fail under some situations where it has the resource to succeed, it's not an efficiency issue, it's a correctness issue.  It's not even hard to fix:
```c
	while (fgets(line, LINE_MAX, stdin)) {
		if (!*linend) exit(1);
		if (longer(last, line)) continue;
		if (longer(line, last)) {
			memset(buf, 1, ACCU_MAX);
			buf[0]= buf[ACCU_MAX - 1] = 0;
			next = buf;
		}
		if (!longer(bufend, line)) exit(1);
		last = next;
		strcpy(next, line);
		while (*next) next = &next[1];
	}
```
 --[[User:Ledrug|Ledrug]] 21:45, 17 August 2011 (UTC)
::::::: Bytes from <code>bufend</code> on were not "resources" they were "protection".  So no, I do not agree that it's a correctness issue.  If that's a correctness issue, you might as well argue that a program is incorrect because some memory on the heap has not been allocated. Your ACCU_MAX is the moral equivalent of my <code>bufend</code>.  I could agree to calling this an efficiency issue, but not a correctness issue.  --[[User:Rdm|Rdm]] 22:23, 17 August 2011 (UTC)
:::: (deindent) Ok I guess that's what's confusing about the code.  You could check available buffer at a more natural place: <code>next+1</code>.  That's where you'd copy the line string to to begin with:
```c
	char line[LINE_MAX] = {0};
	char buf[ACCU_MAX] = {0};
	char *last = buf;
	char *next = buf;

	while (fgets(line, LINE_MAX, stdin)) {
		if (line[LINE_MAX - 2]) exit(1);

		if (longer(last, line)) continue;
		if (longer(line, last)) {
			memset(buf, 1, ACCU_MAX);
			buf[0] = buf[ACCU_MAX - 1] = 0;
			next = buf;
		}
		if (*line && longer(&line[1], &next[1])) exit(1);
		strcpy(next, line);

		for (last = next; *next; next = &next[1]);
	}
```
 (I didn't define the LINE_MAX and ACCU_MAX just to be different, it's easier to change the values for testing.  They ''are'' related to your <code>bufend</code>).  There's still one issue: <code>echo -n "a\naa\naaa"|./a.out</code> fails, which has nothing to do with the modifications I made. --[[User:Ledrug|Ledrug]] 23:10, 17 August 2011 (UTC)
::::: The treatment of the <code>echo -n "a\naa\naaa"</code> case depends on the definition of a "line".  Since "line" was not defined in the task, I am comfortable saying that that represents an ill-formed file and thus cannot represent a correctness issue.  (If I cared about that case, I would exit(1) for any line that did not end with a newline character.) --[[User:Rdm|Rdm]] 23:29, 17 August 2011 (UTC)

== Boring solution v. restrictions ==
I'm not sure if this will help or not but here goes:

```Icon
procedure main()    # boring and pedestrian solution
maxlen := 0                    # set max string length
L := []
while line := read() do        
    if maxlen <:= *line then L := [line]    # <:= lt assignment blatantly fails comparison restriction
    else if maxlen = *line then put(L,line) # L is a list accumulator and blatantly fails list restriction
every write(!L)                              
end
```

== Conforms to the specs? ==
Hoping to confirm that the AHK solution didn't break any rules
: Looks like it meets the spirit just fine.  --[[User:Dgamey|Dgamey]] 14:18, 29 September 2011 (UTC)
