+++
title = "Talk:One of n lines in a file"
description = ""
date = 2011-12-11T18:20:06Z
aliases = []
[extra]
id = 10468
[taxonomies]
categories = []
tags = []
+++

==Inspiration==
I read [http://stackoverflow.com/questions/7267100/how-to-select-one-of-n-objects-at-random-without-knowing-n-at-first this] and thought of RC. --[[User:Paddy3118|Paddy3118]] 22:03, 7 September 2011 (UTC)

== implicit constraints ==

I believe that this task implicitly assumes that a single line will always be small enough to be kept in memory?  If so, I think that this assumption should be explicitly stated.  (It sounds silly, but it can turn into a real issue.) --[[User:Rdm|Rdm]] 20:19, 14 September 2011 (UTC)
: Choosing a random item from a sequence of unknown length has its uses other than reading files.  Actually since the task doesn't ask anyone to read a file anyway, shouldn't it be named something else like, er, "Choosing a random item from a sequence of unknown length"? --[[User:Ledrug|Ledrug]] 20:36, 14 September 2011 (UTC)
:: Ok... well... I implemented it so that if the file is under 10 MB, the entire sequence is handled at once.  In most practical situations this means that the length is not unknown.  If this were a serious exercise I would increase the buffer size to 100MB.  --[[User:Rdm|Rdm]] 11:10, 15 September 2011 (UTC)

I don't know whether it's worth noting or not, but there will be issues relating to precision in implementations' real, non-integer numeric types and with precision in its random number generator. --[[User:Short Circuit|Michael Mol]] 21:18, 14 September 2011 (UTC)

:The central idea is to show the algorithm and to show that it gives approximately the right frequency of choice of each line for an example. Yes, if you were to use it in earnest then you would need to measure the limits of the environment you put it in. If you never have files of more than X lines then you can work out what numerical precisions you need. Put a limit on the maximum length of a line of a file then you can begin to think of memory constraints. Thing is, that is something that anyone using RC code  in production should be doing anyway. I would not want to add a comment about line lengths to the task for that reason.

:My original reference talked about choosing lines from a file, hence the title. The algorithm may be of use elsewhere (thanks for your suggestion Ledrug), but unless this algorithm has another name by which it is more commonly known, then I would like to leave the name as it is, rather than thinking up a new name that may only be of used on RC (rather than RC and StackOverflow :)

 --[[User:Paddy3118|Paddy3118]] 21:43, 14 September 2011 (UTC)
: IEEE double can handle 52 bits of integers without precision loss, which I guess should be big enough for realistic uses. --[[User:Ledrug|Ledrug]] 22:03, 14 September 2011 (UTC)

==Simulation==
Please note that the actual task just asks for the results of a simulation of the algorithm. You can use mock files ''or not''. Although the explanation of the algorithm mentions files, the task doesn't ''require'' file handling. Thanks. --[[User:Paddy3118|Paddy3118]] 21:54, 14 September 2011 (UTC)

== Rename? ==

Actually since the task doesn't ask anyone to read a file anyway, shouldn't it be named something else like, er, "Choosing a random item from a sequence of unknown length"? --Ledrug 20:36, 14 September 2011 (UTC) 
:I agree. None of the examples seem to use files at all. I'd say it needs a name (and probably a description) that doesn't mention files. Maybe the description should just mention files....less? --[[User:Mwn3d|Mwn3d]] 00:45, 29 September 2011 (UTC)


:From an answer I gave above:
::''My original reference talked about choosing lines from a file, hence the title. The algorithm may be of use elsewhere (thanks for your suggestion Ledrug), but unless this algorithm has another name by which it is more commonly known, then I would like to leave the name as it is, rather than thinking up a new name that may only be used on RC (rather than RC and StackOverflow :-)''
:--[[User:Paddy3118|Paddy3118]] 04:21, 29 September 2011 (UTC)

:: This is a special case of Knuth's Algorithm S (running backwards though).  Note that I'm not suggesting we should rename this task "Alghrithm S", it's not quite the same thing.  The reverse Algorithm S is for selecting n items out of N total where N may be unknown, and I'll describe it out of idle interest:
::# Select first n items;
::# For the m-th item where m > n, have a random chance of n/m of keeping it.  If failing this chance, go to next item.  If not, have it randomly (1/n) replace one of the previously selected n items.
::# Repeat until all N items exhausted.
::It's easy to see that if n = 1, it reduces to current task. --[[User:Ledrug|Ledrug]] 18:54, 20 October 2011 (UTC)

:::Hi Ledrug, that's brilliant! Why not make a task of maybe:
::::  ''Create a function "s_of_n_creator" that given n, returns a function "s_of_n" that when called with successive items <strike>from a list</strike> returns an equi-weighted random sample of n elements from its arguments since creation using Knuths Algorithm S. Test your function by printing and showing the frequency of occurrences of the selected digits from 10000 repetitions of using the s_of_n_creator(3) and for each s_of_n using the last set of 3 elements when it is given the integers 0..9 inclusive, in order.''

::::: Huh? I don't quite understand that description, but it seems like an entirely different task. Since there are already several implementations, I don't think it is good idea to rename the task and change the task description. Further, I don't really see the purpose of "function that returns a function". In any case, that requirement means that only a small fraction of languages can be used to implement the task. That is another reason to not change this task. If you want such a task, make it a new task. --[[User:PauliKL|PauliKL]] 14:05, 21 October 2011 (UTC)

:::::: Hi PauliKL, The idea is to create an entirely new task not a re-naming of this one. I'll flesh out the description a little, and choose the name wisely. 
:::::: Check out [[Accumulator factory]] which is another task that required a function returning a function and has over fifty entries, so I expect there are a considerable number of languages that could implement what I have in mind. --[[User:Paddy3118|Paddy3118]] 16:59, 21 October 2011 (UTC)
:::Or I could do this if you like this weekend? --[[User:Paddy3118|Paddy3118]] 06:37, 21 October 2011 (UTC)
:::: Sure, you do that.  The exact reference is ''The Art of Computer Programming'', Vol 2, 3.4.2 p.142.  I can't seem to find any ref online, though.  Also, don't name it "m of N lines in a file"-- --[[User:Ledrug|Ledrug]] 11:34, 21 October 2011 (UTC)

::::: Behold [[Knuth's algorithm S]]! --[[User:Paddy3118|Paddy3118]] 19:21, 21 October 2011 (UTC)
== Algorithm to pick Random line ==
I just posted a solution for PureBasic.  As I compared the sample output for other solutions, all current solutions (except PureBasic's) appear to be using an algorithm for randomly choosing a line from a file that is different than the one specified in the task description.

According to the task description ''it says that after each line is read a random float value is checked against the fraction 1/n where n is the line just read, to see if the process should stop, or repeat''.  This would mean the probability for each subsequent line is the combination of the probabilities of the lines that preceded it.  The simplist case, the first line in the file, should have a 1/2 chance of being used (kept).  All of the solutions (except PureBasic's) show more or less an equal distribution between the 10 lines of the simulated file.  That would imply all those solutions are incorrect. --[[User:Demivec|Demivec]] 17:27, 11 December 2011 (UTC)

: Where did you get the "the process should stop" part? --[[User:Ledrug|Ledrug]] 17:44, 11 December 2011 (UTC)
:: You raise a very good point.  It seems I was incorrect by adding that particular assumption. Taking that mis-step into account, the distribution of line choices does follow  a normal distribution when the process is repeat for each line of the file. Thanks for helping me see the light ;). --[[User:Demivec|Demivec]] 18:04, 11 December 2011 (UTC)

:::Hi Demivec, there shouldn't be a [[wp:Normal distribution|normal distribution]]. The counts of how many times a particular line is chosen should be roughly the same (as shown by the other examples). --[[User:Paddy3118|Paddy3118]] 18:16, 11 December 2011 (UTC)

::: It's supposed to give uniform distribution ("normal distribution" means something entirely different) among all lines.  Think it this way: when you read in the n-th line, it has 1/n chance of becoming the selected one, overriding the previous selection.  Meanwhile, lines from 1 to n-1 each has equal chance of remaining selected, that is, 1/n.  If you are familiar with mathematical induction, it's pretty easy to prove.  If you are not, try the simple cases of n=2 and n=3 by hand, and you'll see. --[[User:Ledrug|Ledrug]] 18:11, 11 December 2011 (UTC)

:::: Yes, "uniform distribution" is what I should have said.  It was easy enough to prove it to myself that all was in order once you pointed out my inadvertent blunder.  Thanks again.--[[User:Demivec|Demivec]] 18:20, 11 December 2011 (UTC)
