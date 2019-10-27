+++
title = "Talk:Guess the number"
description = ""
date = 2019-10-14T14:12:18Z
aliases = []
[extra]
id = 8623
[taxonomies]
categories = []
tags = []
+++

== most pleasing message sequence ==

I think you really get the most pleasing message sequence when using techniques from [[Loops/N plus one half]], except that instead of a fixed limit, you have a dynamic one. –[[User:Dkf|Donal Fellows]] 21:15, 28 October 2010 (UTC)

== dupe? (sort of) ==

I think this task is already covered in [[Bulls and Cows]], but with more functionality. Could this be a dupe? --[[User:Mwn3d|Mwn3d]] 21:26, 28 October 2010 (UTC)

:They might have some similarity, but no more than say [[wp:Go (game)|Go]] and [[wp:Gomoku|Gomoku]]. This game might well be an exercise for people without the ability to play the more difficult B&C. It might also serve as a useful programming problem for those new to programming. --[[User:Paddy3118|Paddy3118]] 16:17, 29 October 2010 (UTC)

== How about a modification to turn it into more of a game? ==
The computer could give the bounds, select a target, ask for a guess, but then tell .... I see a new task coming on: [[Guess the number/With Feedback]]  :-)
 --[[User:Paddy3118|Paddy3118]] 06:02, 29 October 2010 (UTC)

:I am a great believer in the KISS principle. I think more complicated examples should be kept separate. The idea of rosetta code (according to the front page) is to present solutions to the same task in as many different languages as possible, to demonstrate how languages are similar and different, and to aid a person with a grounding in one approach to a problem in learning another.

:I think the examples should be short and simple just for the purposes of illustrating specific differences. It might be an idea to have a "see also" section that links to larger more complex examples of course. I think the idea of creating a separate task is a good idea. For this reason, I think that "Bulls and Cows", "Guess the Number" and "Guess the number (with feedback)" should be considered to be three separate tasks.

:These are my own opinions of course. :)
:[[User:Markhobley|Markhobley]] 19:59, 29 October 2010 (UTC)

== Language ==

Suggest removing the explicit invokation of 'conditional loop', as some languages' syntax don't use that syntax. (I'm thinking of logic languages and, I think, functional languages.) A more precise description might be, "The program randomly chooses a number [1-10]. The program then keeps asking the user to guess what that number is until the user inputs that number as their guess." --[[User:Short Circuit|Michael Mol]] 14:32, 29 October 2010 (UTC)

:Well, I think we could keep the wording, but just state in the examples that the language does not use conditional loops and state how these are emulated. so at least we have a task for conditional loops.
:Failing that we could split the task. For example:
::Guess the number (using conditional loops) and Guess the number (using the blah blah blah method), etc.
:[[User:Markhobley|Markhobley]] 14:40, 29 October 2010 (UTC)

::I don't think that split would be good. It would be better to allow more freedom in this task and have the example writers specify what constructs they go with. This isn't an algorithm-centric task, so we should focus on the functionality (which I still think is too similar to [[Bulls and Cows]]--see above). --[[User:Mwn3d|Mwn3d]] 16:02, 29 October 2010 (UTC)

:::Again, the purpose of rosette code is to demonstrate how languages are similar and different. There may be several approaches to a task. I think if you have several approaches on one task you illustrate the differences in the ways that the task is approached, rather than the differences in the languages. If the approaches are different, the readers may be thinking "Hmmm, he used a different technique. I wonder if the other technique works too...". It would probably be better to state the technique being illustrated, and just place notes against the languages where that technique cannot be used. Again IMHO.
:::[[User:Markhobley|Markhobley]] 20:07, 29 October 2010 (UTC)
::::It's just as good to give implementers freedom so that readers can see differences in how languages approach problems. And besides, if you want to see differences in how languages handle a particular approach, people can add multiple approaches to this page where applicable and natural for the language (see: [[100 doors]], [[99 Bottles of Beer]], [[Pascal's triangle]], probably others). It depends on if you want to show idioms or simple syntax. --[[User:Mwn3d|Mwn3d]] 21:51, 29 October 2010 (UTC)
:I re-worded the task to make the use of a conditional loop advisory rather than mandated; to cater for any language whose idiomatic solution might be different. If too many hate it then it can just as easily be reverted, but I'm with Michael on this so far, I don't think it should be a demonstrator for conditional loops - I'd prefer it as a simple game with a hint at implementation. --[[User:Paddy3118|Paddy3118]] 21:23, 29 October 2010 (UTC)
