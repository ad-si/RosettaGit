+++
title = "Talk:Man or boy test"
description = ""
date = 2017-03-14T11:53:17Z
aliases = []
[extra]
id = 2427
[taxonomies]
categories = []
tags = []
+++

= MoB =

'''Please sign your posts with <nowiki>--~~~~</nowiki>, as it will make the conversation much easier to follow.''' --[[User:Short Circuit|Short Circuit]] 22:35, 28 March 2008 (MDT)

All posts are signed at the last entry at the same "indentation level". The alternative is to sign after each remark to some point, which to me seems a bit excessive. Yes, discussion on Wikipages sucks :-( And long discussions suck even more. Any alternative? --[[User:Dirkt|Dirkt]] 05:32, 29 March 2008 (MDT)

== Clarify! ==

As written, this task accomplishes absolutely nothing whatsoever. No functionality is mentioned or referenced. There's a pointer here on the discussion page to a gif file of a scan of an old article that does not specify any kind of algorithm whatsoever and no criteria for determining whether any piece of code is "valid" for this task. 

: I didn't write the task, but if you look at Knuth's example, it should be obvious that the criterion is "are the updates to ''k'' inside ''B'' correct"? Or in other words, does ''B'' access the "correct" frame when accessing ''k''? You can also determine validity from the output given in the example: If it's -67, then it's correct.

The Knuth article touts a piece of code for testing something about the implementation of ADA compilers and thus any piece of code that does not compile as valid ADA <i>fails</i> the test as referenced.

: Didn't read the article, but originally this was about ALGOL, not ADA. Anyway, the programming language shouldn't matter.

The task description needs to be clarified at least to the point where the following question can be answered: 
The BASIC one-liner: <tt>10 PRINT "-67"</tt> will output what appears to be "the correct result". Does that make it an entry for this task (I highly suspect it doesn't) and <i>why not?</i> -- [[User:Sgeier|Sgeier]] 17:05, 26 March 2008 (MDT)

: It doesn't make it an entry for this task, because your BASIC program doesn't attempt to deal with access to variables in different stack frames. And it doesn't follow the outline of calling nested subroutines ''A'' and ''B'', either. 

: If you still think the task is unclear with this additional information, could you try to come up with some way of phrasing it that you would understand? As the task was clear to me, I'm probably the wrong person to do that :-) -- [[User:Dirkt|Dirkt]] 08:11, 27 March 2008 (MDT)
::Don't you think that asking the person who doesn't understand the task to explain it is a bit silly? 
::: No. Because those people who had problems with understanding the task have a feeling where the problems are, and what information they need (once they understand what is meant).
::::I and Sgeier are not offering a task description now. We are offering task description descriptions. '''You''', because you know what to do for this task, should give a task description.
:: Right now what we have for the "directions" for this task is an explanation of why it was done, a silly quote, and nothing else. The description says nothing about nested subroutines or stack frames. It needs ''steps or characteristics'' at the ''top'' of the page. Don't link to another page to explain the task. Don't hope that people can decode the task from other languages. 
::: See what I mean? ''You'' know very well what is needed. Much better then me :-)

:: Especially, don't trust task description to someone who doesn't understand the task (like me and Sgeier in this case). --[[User:Mwn3d|Mwn3d]] 08:54, 27 March 2008 (MDT)
::: Which part of it is still unclear? --[[User:Dirkt|Dirkt]] 15:28, 27 March 2008 (MDT)
::::There are no commands in the task description. There are no restrictions. The BASIC program proposed before accomplishes the task. Also thie BASIC program does:
::::
```txt


```

::::because THERE IS NO TASK!! This article is more of an encyclopedia article (and a bad one at that) with arbitrary code on it. Maybe the title of this section should be "give actual directions" instead of "clarify!". --[[User:Mwn3d|Mwn3d]] 18:08, 27 March 2008 (MDT)

Let's please try to get a bit more constructive. You don't have to yell at me, I didn't come up with this task :-) If you're just looking for an explicitly worded direction, would "do the same thing the Algol60 program does" be enough? If not, what extra instructions do you require? --[[User:Dirkt|Dirkt]] 02:01, 28 March 2008 (MDT)

BASIC does not have "recursion and non-local references" and so the task cannot be coded in BASIC.  Kind of like trying to represent π (pi) as a Fraction. The ADA version works fine for me.  [[User:60.52.12.85|60.52.12.85]] 05:04, 28 March 2008 (MDT)
:You seem to forget what I've said already. "Don't hope that people can decode the task from other languages."
:: Ah, so the problem is that you cannot understand what the Algol60 program does?

: What needs to be in a task description are ''words'' '''explaining''' what to do. What characteristics are in a man or boy test program? How are they arranged? 
:: The characteristics are that it does the same as the Algol60 program, and especially that it handles the updates to ''k'' in the same way. Yes, that's a stupid task, but I didn't invent it :-)

: I should be able to code it in a language that I know (and a language where it's possible to code it) without any other examples on the page. I just don't understand how you can think that and explanation of what the man or boy test is for is an adequate task description. I shout because of absurdity. --[[User:Mwn3d|Mwn3d]] 06:30, 28 March 2008 (MDT)
:: I don't think it is an adequate task description. My problem is that I have to understand why *you* don't understand it. Which is difficult, unless you tell me (or unless you understand it yourself, because then you can just say "ah, that's where I had trouble"). Because I just look at the Algol60 program and think "ok, let's do that in a different language". And the task is made in such a way that there isn't really much else to it -- it doesn't really have some "core problem" on it's own, that could be described in words. But that's not my fault. :-) --[[User:Dirkt|Dirkt]] 07:37, 28 March 2008 (MDT)
:::First, stop giving me that little smirk. 
:::: I'm using the smileys because I hoped they would calm the situation. For some reason you're yelling at me, you think it's my lone responsibility to fix everything, and you're very angry at me. I don't understand why.
::: Second, I don't understand it because there's nothing to understand. I have been trying to help. 
:::: And I've also been trying to help, but I just get yelled at...
::: I can give you direct quotes of me trying to help:
:::*"[The task] needs steps or characteristics at the top of the page"
:::*"Don't hope that people can decode the task from other languages"
:::*"There are no commands in the task description. There are no restrictions."
:::*"What needs to be in a task description are words explaining what to do. What characteristics are in a man or boy test program? How are they arranged?"
:::If these things can't be done then it's a different story. 
:::: No, these things can't be done. The task really boils down to "imitate the Algol60 program, all of which is does is to create a complicated tree of activation records, and then updates values inside them." If you don't like this, then you have to delete the task, but that's how it is.
::: You said there is no "core problem," which would have been a helpful statement when Sgeier first asked for a clarified description. If that is true, then I think it should be at least noted.
:::: If I had known that this would be helpful for him, I had mentioned it first thing. But I cannot read minds, and that's why we're having this discussion. You must tell me what you find difficult to understand in the task. Because I DON'T KNOW. And a general idea how a task should be structured doesn't help, because the task as it is just isn't structured this way.
::: I don't think it is, though. 
:::: If you know better than me what this task is all about, then why don't you change the task description?
::: You also mentioned nested subroutines and variables from different stack frames. If these are required for the task then they should be listed as a task description. I asked before "What characteristics are in a man or boy test program?" Make a list of things that are required and post it. --[[User:Mwn3d|Mwn3d]] 08:03, 28 March 2008 (MDT)
:::: They are not "required" in a strict sense. This is the way the Algol60 program works, and in another language, you have to emulate them with whatever is closest. Of all the languages, I think only Smalltalk handles the activation records (i.e., "stack" frames) in a similar way; and closures work of course, too. --[[User:Dirkt|Dirkt]] 10:14, 28 March 2008 (MDT)

No one here will start the list, so I'm trying on my own. From looking at a few posts I found in various places (wikipedia not included), I think at least these things are required for the test:
*variables in different stack frames
*nested subroutines
*mutable references
*state monads (an explanation of monads is [[monads|still wanted]] on this site)
*recursive function calls
*call-by-ref and call-by-value functions
Are any of these wrong and are there any more requirements?
: None of these is "required". I don't think such a list makes a lot of sense. Since you insist that I take responsibility, I've updated the task description. It's probably still not satisfactory for you, but unless we can finally find out where the problem in understanding the task is, that's the best I can do. --[[User:Dirkt|Dirkt]] 10:14, 28 March 2008 (MDT)
::Thank you. It's a step in the right direction. At least now there are directions. I still don't understand what to do, but that's because I don't understand ALGOL xx and I don't understand the idea of "activation records." --[[User:Mwn3d|Mwn3d]] 10:37, 28 March 2008 (MDT)

###  Copyright issues 

The fact that this page sprung into being with many language examples at once with code wrapped in <source> blocks makes me worry that the code was transwikied from somewhere like Literate Programs, which would be a copyright violation. Are you certain that you have permission to do this? (Hmm. I was going to point you at a Copyrights page, but I can't seem to find one.) --[[User:IanOsgood|IanOsgood]] 10:35, 18 December 2007 (MST)
:[[:Rosetta Code:Copyrights]]? --[[User:Mwn3d|Mwn3d]] 11:13, 18 December 2007 (MST)
: It is copied from Wikipedia (as is indicated in the initial edit summary). It is probably ok to copy from Wikipedia, as it is also GFDL, but there should probably be a note somewhere on the page indicating where it comes from. Also, I am not sure if it is a point in just copy the content of another wiki, even if it is legal. Maybe this page should only contain new implementations, along with a link to the Wikipedia article. [[User:Ahy1|Ahy1]] 11:35, 18 December 2007 (MST)
Hi...  I transplanted the page from wikipedia.  Basically it is a matter of time before the wikipedia article get the other bulk of the code samples pruned.  Wikipedia typically requires that content is notable, and wikipedia isn't typically a repository for code sample. Also... most the the examples are "original research" having been created by the contributor.

But having said that I would hate to loose the examples.  Hence the transplant.

Of the code snippets, the two that were originated from further afield are the ALGOL 60 (being the absolute original created by Donald Knuth), and ALGOL 68 version that was derived from Charles H. Lindsey's version that appeared in ALGOL Bulletin.   Being so small, are they copyrightable?  

ps Sorry for the broken links.  Does RosettaCode want pages for IT pioneers etc etc

[[User:NevilleDNZ|NevilleDNZ]] 17:31, 18 December 2007 (MST)
:::I've updated [[Rosetta Code:Copyrights]].  Any suggestions on where to link to it to make it more easily found? --[[User:Short Circuit|Short Circuit]] 18:41, 18 December 2007 (MST)

Add a link in [[Rosetta_Code:About]], and maybe include a link at the bottom of each page with  Privacy policy/About Rosetta Code/Disclaimers maybe.

Re: copyright of the Man_or_boy algorithm and derivatives, I attributed the sources for the Non-GPL versions.  As far as the Donald Knuth and Charles H. Lindsey versions... I know there is a rule on fair usage, but I am not sure how far this goes.  The algorithm was published in the ALGOL bulletin Number 17 - July 1964 [http://archive.computerhistory.org/resources/text/algol/algol_bulletin/A17/P24.HTM].  I was very brief.  The Charles H. Lindsey - AB52 December 1988 versions [http://archive.computerhistory.org/resources/text/algol/algol_bulletin/A52/P43.HTM] embedded the Algol 68 version in a much larger document. 

Maybe the ALGOL Bulletin itself has a more exacting/generous license.  I am thinking that I will go back the wikipedia and get their thoughts.  And if necessary delete both extracts in both wikis.  Any thoughts?

[[User:NevilleDNZ|NevilleDNZ]] 20:20, 18 December 2007 (MST)


== Helping to Understand the problem! ==

Knuth's MoB test is hard to get right.  Of course it was designed that way. :)

The description in the task is accurate but leaves a bit to be desired.  If you haven't seen this problem before, or don't know ALGOL60, or can't just port something close to your language then it's going to give you trouble. Some modern safer languages may have some difficult with parts of this task.  

Having done some ALGOL programming in the depths of antiquity on a safer version of ALGOL, there are a few things that you need to understand.
* call by value isn't the default you must ask nicely
* procedure call syntax doesn't require () unless you have arguments, hence you can not tell if x is a variable or a function call from looking
* returned values are just assigned to variables representing the function and are subject to scoping conventions
* running with scissors didn't get you any warnings from your Mom

The Wikipedia article sums it up fairly well but doesn't really show you where you can get hurt.

If you succeed at this task, you'll have a much better understanding of the semantics of your language. Always a good thing.  Not to mention it's a bit of fun mental excercise.

The thing here is that the process for doing mob, for really understanding it in a different environment, involves a series of enlightenment moments not unlike being hit upside the head with a cricket bat.  There are a couple of common ways to fail at this (1) k going negative and recursing to death (2) working correctly for k=0..4 and then messing up. The truly evil "man-compiler" part of mob is in x4 + x5.  As B gets rolled through the x's everything is safe up to and including the penultimate B+x5.  The trick is that in the ultimate form when you get to  B+B is that k will be one less entering the second B!

I'd truly love the back story on this program.  My suspicion is that Knuth hit a bug his compiler and distilled the problem down to MoB or something close to it.  From the Knuth's submission to Algol report 17:

: 'I have written the following simple routine, which may separate the "man-compilers" from the "boy-compilers"'; and
: 'This uses nothing known to be tricky or ambiguous.'
: 'My question is: What should the answer be?  Unfortunately, I don't have access to a man compiler myself, ...'

I find Knuth's comments both understated and mischievously misleading. :)

[[User:Dgamey|Dgamey]] 03:25, 6 May 2010 (UTC)-

==Haskell version==
: Perhaps skip the Control.Monad import by using an applicative alternative to liftM2 ?
:: liftM2 (+) x4 x5   is equivalent to   pure (+) <*> x4 <*> x5   
: [[User:Hout|Hout]] ([[User talk:Hout|talk]]) 00:08, 14 March 2017 (UTC)
:: Tho in fact I notice that the applicative reformulation seems to run a little slower … [[User:Hout|Hout]] ([[User talk:Hout|talk]]) 00:17, 14 March 2017 (UTC)
::: What about:
:::: <code>(+) <$> x4 <*> x5</code>
::: Not sure if that's more readable than the <code>liftM2</code> version, but arguably more idiomatic nowadays. &mdash;''[[User:Ruud Koot|Ruud]]'' 08:42, 14 March 2017 (UTC)
:::: Good thought, and a little faster too. Perhaps your first instinct is right though ? liftM2 is not unreadable, and still seems to be the fastest of the three. [[User:Hout|Hout]] ([[User talk:Hout|talk]]) 11:53, 14 March 2017 (UTC)
