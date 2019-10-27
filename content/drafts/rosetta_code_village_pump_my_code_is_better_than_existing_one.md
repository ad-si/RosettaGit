+++
title = "Rosetta Code:Village Pump/My code is better than existing one"
description = ""
date = 2018-11-04T16:58:35Z
aliases = []
[extra]
id = 22056
[taxonomies]
categories = []
tags = []
+++

{{Vptopic
|topic=My code is better than existing one
|summary=I wrote a code for one of the problems that in my opinion is more concise and easier to read. What should I do? Should I replace previous version by mine? Or the fastest gun wins? I couldn't find the rules on this matter. -- [[User:Georgy|Georgy]] ([[User talk:Georgy|talk]]) 05:58, 4 November 2018‎ (UTC)
}}

It depends. If your version uses substantially different algorithms to accomplish the same goal, it may be worth adding as a second, alternate version. If it is essentially the same process, just clearer, my inclination would be to replace. Note that Rosetta Code is mostly geared toward demonstrating how your language works to people who may no be familiar with it, so extremely dense code to the point of obfuscation isn't really the goal. Some languages can really help it though... (lookin' at you, J) Conciseness bringing clarity is a win though.

Since you are a new user, my gut reaction would be to say: add as an alternate version and watch the reaction from the community.

BTW, It is a good idea to sign your comments, I retroactively added your signature above. Just put <nowiki> --~~~~ </nowiki> at the end of your edit and the wiki will transform that into a signature. --[[User:Thundergnat|Thundergnat]] ([[User talk:Thundergnat|talk]])



### One way.

(quick edit conflict resolution as Thundergnats answer above preceeded mine).

One could move the discussion to the talk page by leaving a short note to that effect on the page. In the section of the talk page you could put your code and why you tink it should replace what is shown. Is it more idiomatic? More readable? ...

RC isn't really about the fastest. There are really no good ways to judge speed, and speed is not usually mentioned in the task. One would expect answers to work in a "reasonable" time, and people have been known to note solutions they found quite slow; for example, a few languages might have implementations of "big" integers that make some calculations slow and have stated that their language took tens of minutes to perform  task. (Especially if they got the feling that it was orders of magnitude longer than other languages).

If you have an innovative approach to solving the task as stated, then there can be two examplesfor a language. If you think that an existing example is poor/un-idiomatic then best to follow my first suggestion, giving a wider section of your languages community a say, or quote better adherence to the languages published quidelines.

 [[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 11:38, 4 November 2018 (UTC)


Thanks for explanation! Unfortunately, I did a poor decision of not waiting for replies and blatantly replaced the code here: [http://www.rosettacode.org/wiki/Abbreviations,_automatic#Functional http://www.rosettacode.org/wiki/Abbreviations,_automatic#Functional].  <br />
Should I make a roll-back and start a discussion now?--[[User:Georgy|Georgy]] ([[User talk:Georgy|talk]]) 12:00, 4 November 2018 (UTC)

: At least start a discussion pointing-out why. [[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 12:30, 4 November 2018 (UTC)



### The Rosetta goal


No problem with the initial deletion – it's easily reverted :-)

The [[Rosetta_Code|idea of Rosetta Code]] is "to demonstrate how languages are similar and different, and to aid a person with a grounding in one approach to a problem in learning another". 

There are no "rules on this matter" and no "winning" – the value lies in the Rosetta insight provided, and that's what you need to use as a guide to what you contribute,  and its relationship to anything already there. The Rosetta Stone itself has '''three''' versions of the same message, and the similarities between each all contributed to the new insights that scholars were able to gain from it.

In this particular case, we have two functional approaches, and their similarities and differences are instructive. Mine is a composition of pre-existing generic (curried) primitives, and aims to optimise for reliability and speed of composition. Currying enables more flexible composition. Re-use of primitives is good economy, and allows, over time for the development of well tested and reliably inter-operable units. You will be able to explain what yours optimises for. They complement each other.

As for 'better' etc – as you know, the quality of code is a function of its relationship to a particular pragmatic and organisational context. There is no such thing as 'better' in isolation, only 'better '''for''' XYZ', and we always need to specify the 'XYZ'. One draft might be highly optimised for time or space compression, but less well optimised for refactoring. Another might be scrupulously adherent to a particular house-style, but not actually work, or be poorly adapted in some other way to its context.

Equally, 'more readable' is a function of a particular audience. Each of the 3 versions on the Rosetta Stone was less readable to some audiences, and more to others. Therein lies the interest and the value. 

I've restored the deletion, and added a brief gloss to the 'composition of curried primitives' version – perhaps you would like to amend/expand the explanatory gloss on your version ? [[User:Hout|Hout]] ([[User talk:Hout|talk]]) 13:22, 4 November 2018 (UTC)
