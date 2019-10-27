+++
title = "Talk:Evolutionary algorithm"
description = ""
date = 2015-03-03T06:30:29Z
aliases = []
[extra]
id = 4876
[taxonomies]
categories = []
tags = []
+++

==A Priori Assumptions?==
Looking at this project, I wonder how it demonstrates evolution (in the classic goo-to-you-via-the-zoo sense.) "Methinks it is like a weasel" is like getting in a time machine and going back to the beginning with the express purpose of directing an undirected process to produce a weasel. At the beginning there exists no information for weasels. Starting with no information, we have to come up with the information to generate a weasel (if indeed the process ever generates one) through natural selection (a process that reduces populations) and random mutation (a process that can only work if there's something there already to mutate). 

Apologies if I'm not making myself clear here, I'm just struggling with the concept as a whole: here we are at the end of the process saying what constitutes fitness and then using that as the means for directing a process which is undirected to come up with the fit thing that was unknown (unknowable?) at the outset. Okay, it doesn't stop us writing software which compares implementations across languages. Maybe I should just be satisfied with that. That and hang on to this weird belief that spontaneous generation is demonstrably impossible but had to have happened at least once. [[User:Axtens|Axtens]] 06:29, 16 June 2011 (UTC)

:If you are getting lost, I would focus on these issues:  

:# This is a search algorithm
:# The algorithm favors results which maximize (or minimize, depending on how you implement it) its "fitness function"
:# Computer implementations are radically simpler than anything that goes on in real life

:In other words:

:On the one hand, I would not over project the simplicity of a toy program like this onto real life and expect too much out of it.

:On the other hand, I would also not over project the complexities of real life onto a program when trying to read or write working code.  --[[User:Rdm|Rdm]] 12:00, 16 June 2011 (UTC)

::Thanks, Rdm, that makes it a whole lot clearer. [[User:Axtens|Axtens]] 15:07, 16 June 2011 (UTC)

:Dawkins explains the difficulty of this model and clarifies it's purpose (full quote from Wikipedia below). The purpose of the algorithm is to model cumulative selection. He goes on to state that evolution has no "long-distance target" and that is "absurd notion" - giving the example of humans being the final target. As explained by Rdm, this is a search algorithm. That means it gets to its target with absolute certainty, every time. Only the path to the target varies. So it's not trying to properly model evolutionary process but it aims to show how significant change is possible incrementally (cumulatively) not by "blind chance" in a single iteration.
:"Although the monkey/Shakespeare model is useful for explaining the distinction between single-step selection and cumulative selection, it is misleading in important ways. One of these is that, in each generation of selective 'breeding', the mutant 'progeny' phrases were judged according to the criterion of resemblance to a distant ideal target, the phrase METHINKS IT IS LIKE A WEASEL. Life isn't like that. Evolution has no long-term goal. There is no long-distance target, no final perfection to serve as a criterion for selection, although human vanity cherishes the absurd notion that our species is the final goal of evolution. In real life, the criterion for selection is always short-term, either simple survival or, more generally, reproductive success."  - http://en.wikipedia.org/wiki/Weasel_program - --[[User:Davidj|Davidj]] 18:15, 1 October 2011 (UTC)
::Actually, whether the target exists, and whether it is reliably reached, depends in part on both the search space and the fitness function.  Anyways, I would not overgeneralize too much, based on this particular example.  --[[User:Rdm|Rdm]] 19:31, 1 October 2011 (UTC)

==Adherence to similarly named variables/functions?==
When I added the text:
: <small>Note: to aid comparison, try and ensure the variables and functions mentioned in the task description appear in solutions</small>
I was not sure if I might be "stepping on implementers toes" too much, so if your languages normal idioms would be broken by the above, then please discuss, although caSe changes/lngth changes should just be done. (I would expect it to be broken by languages created solely to be obtuse such as brainf*!ck, and calculator programs might have limits on name sizes). --[[User:Paddy3118|Paddy3118]] 03:40, 7 October 2009 (UTC)

== Controversy on Wikipedia ==

WP mentions that this is a controversial algorithm (with all the noise apparently coming from people who don't grok what it is actually showing, naturally). However, there are only really two controversial points:
# This is (somewhat) directed evolution, though this mostly just affects the fitness function.
# There is an end-point to the evolution (only really needed to make it into an algorithm rather than an ongoing process).
These issues can be technically resolved by making only the fitness function know what the target is and having the algorithm only terminate once perfect fitness is achieved (i.e., when no possible mutation will improve things). At that point, the ''scientific'' issues are moot, and the non-scientific ones dispute whether the algorithm should even exist, which is out of scope for this site anyway. As a plus side, it also makes the presentation of the algorithm more aesthetically pleasing as it removes the assumption that there is only one possible maxima (informal testing using a fitness function that uses the max of individual fitness functions as in this task, except with different target strings, indicates that this works quite well). —[[User:Dkf|Donal Fellows]] 10:12, 9 October 2009 (UTC)

:That controversy does not currently seem to be there, and apparently it has been replaced by a critique of the limitations of these kinds of algorithms.  Also, detecting that an absolute maxima has been reached may be prohibitively expensive.  --[[User:Rdm|Rdm]] 19:38, 2 September 2010 (UTC)

== Tacit J solution ==
I was wanting to add a purely tacit (arguments not explicitly referenced) J solution. I came up with the following two options and am seeking opinions on which one is "better".

'''VERSION ONE'''

Uses a user-defined adverb and conjunction that help keep the rest of the code tidy.

```j
CHARSET=: 'ABCDEFGHIJKLMNOPQRSTUVWXYZ '
NPROG=:   100                            NB. number of progeny (C)
MRATE=:   0.05                           NB. mutation rate

create  =: (?@$&$ { ])&CHARSET           NB. creates random list from charset of same shape as y
fitness =: +/@:~:"1
copy    =: # ,:
mutate  =: &(>: $ ?@$ 0:)(`(,: create))} NB. adverb
select  =: ] {~ (i. <./)@:fitness        NB. select fittest member of population

nextgen =: select ] , [: MRATE mutate NPROG copy ]
while   =: conjunction def '(] , (u {:))^:(v {:)^:_ ,:'

evolve=: nextgen while (0 < fitness) create
```


'''VERSION TWO'''

Only uses verbs (functions) which are can be easier to understand/parse, especially to start with.


```j
CHARSET=: 'ABCDEFGHIJKLMNOPQRSTUVWXYZ '
NPROG=:   100                           NB. number of progeny (C)

create  =: (?@$&$ { ])&CHARSET          NB. get random list from charset of same shape as y
fitness =: +/@:~:"1
copy    =: # ,:
mrate   =: %@:#                         NB. mutation rate
mutate  =: (0&{:: >: $@(1&{::) ?@$ 0:)`((,: create)@(1&{::))}@;
select  =: ] {~ (i. <./)@:fitness       NB. select fittest member of population

nextgen    =: ] , [ select {:@] , mrate@[ mutate NPROG copy {:@]
notperfect =: (0 < fitness) {:

evolve=: nextgen ^: notperfect ^:_ ,:@create
```

--[[User:Tikkanz|Tikkanz]] 00:53, 4 November 2009 (UTC)

: Put them both in, while explaining the ways in which each is better than the other (perhaps with a little more explanatory text than above). It's a shame that there isn't highlighting for [[J]] yet (especially the comments) as that would aid reading. –[[User:Dkf|Donal Fellows]] 08:59, 5 November 2009 (UTC)

== Problem in previous Python implementation ==
In the first Python example:

```Python
def mutaterate():
    'Less mutation the closer the fit of the parent'
    #This formula does not work as intended because it performs int division
    return 1-((perfectfitness - fitness(parent)) / perfectfitness * (1 - minmutaterate))
```

 For ex-  ((20 - 10)/20*(1-0.09)) will evaluate to 0
 Thus it gets stuck in an infinite loop waiting for mutation to occur.
 Edited the program to avoid this by casting perfectfitness as float.
--lookog --14:22, 26 February 2010 (UTC)

I should have stated that the original was created on Python 3.x. Thanks for making it work on 2.X too. --[[User:Paddy3118|Paddy3118]] 23:10, 26 February 2010 (UTC)

== Are the Python and C++ solution cheating? ==

The Python solution makes the mutation rate depend on the distance to the target. This sounds to me like cheating, because the target (and therefore the distance to it) should ideally be unknown. Note that I don't see a principal problem with modifying the mutation rate; the problem is using information about the distance to the target in determining it. The mutation rate could well itself evolve, but the knowledge about the target should only be used for selection, not for mutation. --[[User:Ce|Ce]] 09:00, 1 September 2010 (UTC)

'''Python'''
    def mutaterate():
        'Less mutation the closer the fit of the parent'
        return 1-((perfectfitness - fitness(parent)) / perfectfitness * (1 - minmutaterate))

'''C++'''
    double const mutation_rate = 0.02 + (0.9*fitness)/initial_fitness;

: Looks like it's within the letter of the task, but as to the spirit, I don't know. Intuition tells me that knowing the true distance to optimal will help avoid problems of local minima, and some (but certainly not all!) problems that evolutionary algorithms are applied to have true distance (or a close approximation of such) available. This may be a good case for splitting the task and specifying a goal-agnostic algorithm. --[[User:Short Circuit|Michael Mol]] 16:19, 1 September 2010 (UTC)
:: in the pike example i used to start the rate at the length of the string, decreasing the value gradually, but i found that whatever rate i started with, the result was the same. on the other hand using a rate larger than 1 character per mutation (even only 2) takes longer to find the result. so now the code uses a rate of changing one character per mutation. also as is noted below about scalability, a high rate is never useful. i now believe that one change per mutation is the optimal approach, because if more than one character is changed then for any new character that is found there is another character lost. --[[User:EMBee|eMBee]] 08:57, 11 October 2011 (UTC)

: In a real problem, you've got a high-dimensional space that you're searching and the fitness function is only poorly known (the profusion of species is clear demonstration that there are many local minima in the problem space that is biology). However, the only effect of varying the mutation rate with fitness, given that we have a reasonable metric, is that it results in faster convergence with smaller populations at each step. It doesn't change the fact that you're ''still'' having to do the evolution towards a solution through random variation and selection, which is the whole point. –[[User:Dkf|Donal Fellows]] 08:26, 2 September 2010 (UTC)
:: +1. (But then I would :-) --[[User:Paddy3118|Paddy3118]] 21:36, 1 October 2011 (UTC)
: Varying the mutation rate is not necessarily cheating but it is deviating from Richard Dawkins' purpose of demonstrating "random variation combined with non-random cumulative selection". The Weasel model uses a mutator and a selector. The mutator is intended to be random while the selector is non-random. If you add a non-random process to the mutator it breaks down the whole purpose of Dawkins' model. I don't understand why it's necessary to vary the mutation rate in the model. Is there biological evidence that nature reduces mutations when we near the ideal target? Dawkins states the notion of the ideal target is "absurd". It's important to stick with the purpose of the model and not change the essence of the model to simple converge more quickly. It's not a competition about who has the most rapidly converging model. --[[User:Davidj|Davidj]] 18:02, 1 October 2011 (UTC)
: Out of curiosity, could the author of the C++ solution explain the constants 0.02 and 0.9 used to calculate the mutation_rate. Thank you.  (double const mutation_rate = 0.02 + (0.9*fitness)/initial_fitness;) --[[User:Davidj|Davidj]] 18:02, 1 October 2011 (UTC)

In answer to the first questioner of this section, the target is clearly given and is a static value. This is a major departure from what happens naturally. This is a task to show evolution, as in the gradual development of an answer towards a goal and shouldn't be taken as the answer to evolution theory sceptics. There is no intended cheating in the Python solution, it just "is what it is" and was written to follow the task goals.

I wrote the task and the Python solution without being being an expert in evolutionary algorithms. It may be that what the knowledgable call an evolutionary algorithm has necessary aspects that are not part of this task description - if so, then I apologise; but I have tried to make the Python solution fit the task description as given, and did do some research into the subject at the time. --[[User:Paddy3118|Paddy3118]] 21:34, 1 October 2011 (UTC)

: That's the thing, biological evolution is not "a gradual development of an answer towards a goal".  It's a process of spontaneous mutation coupled with a selection mechanism, where a mutation can go any direction, but some mutations have less of a chance to survive.  This is a purely statistical process: the fitness of offsprings of previous generation are distributed around the parent (inheritance + random small mutations), but survival chance of some of them are higher than others, so only the more optimal ones will remain when it's time to create next generation, at which time the average fitness of the current generation is ''likely'' higher than the parent generation.  As a ''side effect'', the average fitness of each generation exhibits a tendency towards a local maximum even though mutations are not directed.
: Once the species approaches a local maximum, random mutations still happen at the same pace, but deviating too far from the optimal configuation will lower survival chance, so the average fitness will stay there from then on.  Individual specimen ''do not'' consciously regulate the mutation rate: there's no need.  There's no goal in mind, it just so happens that as a collective, a species will eventually reach some average state that looks like an locally optimal solution and stay there (we call those that didn't make it "extinct").
: As a side note, I've been careful in using the word "locally".  Evidently most species did not exhibit a tendency to evolve into something that's capable of inventing nuclear bombs and wielding world wars, which is clearly a globally optimal configuation.  This is not relevant to current subject, however. --[[User:Ledrug|Ledrug]] 02:11, 2 October 2011 (UTC)

== "Official" Algorithm ==
Is there an "official" algorithm or program? Has Dawkins published the algorithm he has used? Just looking at the C & the C++, there are non trivial differences. 
Also just want to flag that I'm making two minor changes to the C program. I'm removing the hardcoded "27" (number of characters A..Z) from a couple of formulas and introducing a constant called POSSIBILITIES that is calculated from the number of characters that are possible. Also doing a cast to INT in two places because of a type compile error. --[[User:Davidj|Davidj]] 18:54, 1 October 2011 (UTC)
:# I don't know any evidence that Dawkins has anything to do with this task;
- this algorithm was described by Dawkins in his book "The Blind Watchmaker" - http://en.wikipedia.org/wiki/Weasel_program . The Wiki page states that "Dawkins did not provide the source code for his program, a "Weasel" style algorithm could run as follows...". I think that this question has been answered. A little strange that the original source code was not provided but I'm sure he had his reasons. Should I now remove this section? --[[User:Davidj|Davidj]] 19:29, 3 October 2011 (UTC)

:# I removed most of the string length references in the code. --[[User:Ledrug|Ledrug]] 01:33, 2 October 2011 (UTC)
:Thank Ledrug, the code looks great. I've had fun running it with larger target phrases & other variations. Thanks. --[[User:Davidj|Davidj]] 19:29, 3 October 2011 (UTC)

== How to make the algorithm scalable for larger target phrases? ==
I've run the Weasel simulation for some larger target phrases. I was curious to see how the target phrase length affects the number of iterations. I'm guessing that to model a real life organism would require thousands of characters. It seems that as you increase the target phrase the number of iterations required goes up dramatically (possibly exponentially). With a phrase of 600 characters, after 10 million iterations the program hadn't got very far.
I am ignorant of biology so I don't know how a natural example would work. What variables etc should be changed to make this more realistic? Can the phrase be broken down into smaller chunks and then solved and still be an evolutionary algorithm? I realize that I shouldn't over-complicate a simple model. Ideas? --[[User:Davidj|Davidj]] 02:18, 11 October 2011 (UTC)
: Based on previous talks, I'm assuming you are experimeting with the C code.  A few things:
:* I made a rookie mistake in the <code>CHOICE</code> macro.  It should be <code>sizeof(tbl) - 1</code>.
:* Mutate rate of 1/15 is too high for long strings.  When offsprings mutate too much, the effectiveness of inheriting from a fit parent is suppressed.  Think it this way: if <code>MUTATE</code> is 1, i.e. every letter always mutates, you are effectively generating a completely random string every time, and fitness of parent string is ignored.  In that case, for a 600 letter string, you have a chance of 1 in 27<sup>600</sup> to match regardless of current parent, and given a 32 bit random number generator, chances are it's never going to happen.  Try set <code>MUTATE</code> to, say, 1000.
:* This task is more of a demonstration than a simulation of evolution.  It demonstrates how inheritance helps reduce generations needed to improve a species' average fitness to a dramatic level, but don't take it at the face value and think evolution works exactly like it in nature. --[[User:Ledrug|Ledrug]] 11:37, 11 October 2011 (UTC)
Thanks Ledrug, that's really helpful.
:* I'll set <code>MUTATE</code> to 1000 and also try <code>COPIES</code> to 100 (currently it's 30).
:* Also, anyone please let me know if someone has already done this. It will save my poor laptop a few billion processing cycles. :)  --[[User:Davidj|Davidj]] 12:45, 11 October 2011 (UTC)
:: i am running the pike version right now on a 5600 char string.after 300000 iterations it found 89% if only one character is changed per mutation. if it changes 10 characters per mutation then it only finds 48% at that point. so a low mutation rate is clearly more effective. as for scalability i am guessing the algorithm is linear: O(n).
::to speed it up some parallelization may be helpful. by mutating only one character at a time we know exactly which change caused a successful mutation. we thus can run multiple mutations at the same time and then combine the results.--[[User:EMBee|eMBee]] 13:45, 11 October 2011 (UTC)
::: It's not linear.  With your model, suppose the string length is <math>L</math>, alphabet size is <math>n</math> (27 in this case), at any given stage where there are still <math>x</math> unmatch characters, a random mutation will improve the fitness iff: a) you picked an unmatched char, chance is <math>x/L</math>; b) it mutates into a matched char, chance is <math>1/n</math>.  So the chance a random mutation is an improvement is <math>{x \over n L}</math>, which is to say, with <math>x</math> unmatched chars, you'd expect an improvement and reduce <math>x</math> by 1 every <math>nL/x</math> mutations.  Suppose you start from a string with zero matches (<math>x = L</math>), and work all the way to a full match (<math>x = 0</math>), total expected mutations should be <math>\sum_{x=1}^L nL/x = nL(1/1 + 1/2 + 1/3\cdots+1/L) \approx nL\ln(L)</math>.  Actually the sum doesn't go all the way up to <math>L</math>, because your initial random string should match about <math>1/n</math> of the target already, but it's a relatively small difference if <math>L</math> is large. --[[User:Ledrug|Ledrug]] 15:51, 11 October 2011 (UTC)
:::: thanks. i was just guessing. my point mainly was, that it's not exponential or something like that. if i am using the right function in pike then it is something like this: 
```Pike
float L=1.0; while(L+=1)
{ 
    write("%f\n", (27*L*Math.logn(Math.e, L))/L ); 
}
```
 which produces a very slowly rising value for <math>nL\ln(L)/L</math>.--[[User:EMBee|eMBee]] 16:40, 11 October 2011 (UTC)
::::on a slight tangent, how would that formula look like if i don't pick 1 char but <math>y</math> chars? there is a chance that some chars are unmatched while others are matched, thus reducing the overall chance of getting an improvement.--[[User:EMBee|eMBee]] 16:52, 11 October 2011 (UTC)
:::: Firstly, I'm not sure why you are calculating <math>nL\ln(L)/L</math>.
::::: because i want to measure how the algorithm scales as <math>L</math> increases. <math>nL\ln(L)</math> only gives me the total number of mutations needed to find the solution. of course this number will get larger and larger as <math>L</math> gets larger. but if i calculate <math>nL\ln(L)/L</math> i get the average number of mutations needed to improve the result by one character. now if <math>nL\ln(L)/L</math> were constant then, that would mean that <math>nL\ln(L)</math> increases with <math>O(n)</math> (or rather <math>O(L)</math>) but that is not the case, and <math>nL\ln(L)/L</math> just makes this easy to see. it's a visual aid to avoid having to plot <math>nL\ln(L)</math> in order to see that it is a curve going up. --[[User:EMBee|eMBee]] 02:50, 12 October 2011 (UTC)
:::: Secondly, formulating an expression for a general case of <math>y</math> is huuuugely complicated.  The case <math>y=2</math> is doable, though.  Suppose you always pick two letters to mutate; there are two cases where a mutation is an improvement:
::::# Both letters are unmatched (chance <math>{x\over L}{x-1\over L}</math>), and both are mutated into the matching char (chance <math>{1\over n}{1\over n}</math>). Overall chance: <math>x(x-1)\over L^2n^2</math>.
::::# First choice chooses an unmatched char and mutates into matched, chance <math>{x\over L}{1 \over n}</math>; second choice chooses an unmatched char and mutates into an unmatched char (chance <math>{x-1\over L}{n-1\over n}</math>), '''or''' choose a matched char and "mutates" into the same char (chance <math>{L-x\over L}{1\over n}</math>).  Overall chance: <math>2{x\over n^2L^2}\Big((x-1)(n-1)+L-x\Big)</math> (the 2 in the front is from the permutation of 2 choices).
:::: Adding them up, we get the chance at step <math>x</math> as: <math>x(x-1)(2n-1)+2x(L-x)\over n^2L^2</math>.  Summing the inverse of that should give you the answer of needed total mutations, unfortunately it's non-trivial to do.  However, we can examine its tendency: when <math>x</math> is large (many unmatched chars), the expression is roughly <math>2 x^2\over nL^2</math>; recall that the single char mutation had <math>x\over nL</math>, which means double char mutation is initially faster than single char mutation, until <math>x\approx L/2</math>, i.e. half of the letters matched (this shouldn't be surprising).
:::: After that, when <math>x</math> becomes very small, the expression is <math>\approx {2x\over n^2L}</math>, about <math>2/n</math> of the single char mutation speed, meaning it requires about 13 (n=27)times as many mutations when close to perfect match.  Since this is the slowest part of the evolution already, mutating two chars is overall probably about an order of magnitude slower. --[[User:Ledrug|Ledrug]] 18:34, 11 October 2011 (UTC)
::::: wow, thank you. i tried to figure it out, but my math is not good enough to even tell if i just missed something or if it is really complicated.
:::: You guys are amazing. You're right - I changed the MUTATE rate and the COPIES to 100 and even with 1000 characters I always get to the target in less than 10,000 iterations (sometimes as low as 5000). 
:::: I need a bit of time to get my head around the maths. :)
:::: I added the following because I was getting the same random sequence every time. Not sure if there's a more efficient way to do that...   <code>srand ( time(NULL) );</code> and included <time.h>
:::: Thank you --[[User:Davidj|Davidj]] 18:45, 11 October 2011 (UTC)

== Genetic Algorithm Okay? ==

I've included a Genetic Algorithm implemented in MATLAB. I am unaware if this was okay or not. Genetic Algorithms are considered a subset of evolutionary programming and are very similar to Evolutionary Algorithms, but have a few differences in the way they go about evolving the population. I made sure to include some of the differences between Genetic Algorithms and straight Evolutionary Algorithms, but i believe the code i posted would be a great resource for anyone who wants to implement a vectorized GA in MATLAB (and i have tried to comment out the code well enough that people will understand it).

If you guys disagree that the Genetic Algorithm should be included then i may go ahead and make a new task for it. It just didn't seem quite different enough to warrant its own task. Let me know what you guys think!
--[[User:Gwilli|Gwilli]] ([[User talk:Gwilli|talk]]) 06:30, 3 March 2015 (UTC)
