+++
title = "Talk:Primes - allocate descendants to their ancestors"
description = ""
date = 2015-05-18T17:02:33Z
aliases = []
[extra]
id = 18794
[taxonomies]
categories = []
tags = []
+++

==Task comments==
Hi, on reading the task I thought that:

# The language examples show no output.
# If they did show output, would it be too long?
# The task shows time comparisons which is difficult to make comparable.
# The task shows time comparisons between different languages - we normally write tasks in which solutions only need authors to use one language.
# The task mentions comparing algorithms but does not explain the different algorithms in pseudocode or a textual description.
--[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 17:16, 24 February 2015 (UTC)
:Hi,
# 'The list layout' showed a partial output for parent [46]. I've changed it to the full results for parent [46].
# Yes, the output is too big to be posted (5.079 KB), that's why, I give the total of all descendants.
# I did put some time comparisons, because I was so surprised, from a few seconds in C to almost 10 minutes required by the "Visual Basic .NET with collection". This is my first 'Visual Basic .NET' program, I probably did something wrong, because the program runs in 20 seconds, now. I deleted the time comparisons.
# I didn't know one author/one language. Where can I find the rules ?
# I already put a textual description : "This solution could be compared to the solution that would use the decomposition into primes for all the numbers between 5 and 3^33".
--[[User:Old man|Old man]] ([[User talk:Old man|talk]]) 09:00, 25 February 2015 (UTC)

== Task is ambiguous ==

I would like to see some ambiguities resolved, here:

(1) Timing. Timing is meaningless because of potential differences between machines. (Imagine I use elastic map reduce here, for example.) See also [[Rosetta_Code:Village_Pump/Run_times_on_examples%3F]]

(2) Inadequate illustration: So, ok, an ancestor is the sum of the factors of a prime. But the example only considers a case where there are two factors. What are the ancestors of 72? When we are summing ancestors, if a number is an ancestor of two other numbers do we count it once or twice? And so on...

(3) Inadequate definition: What is a descendant? Clearly there are some bounds on descendants from the problem writeup stating that 46 has 557 descendants, but what are they?

Hypothetically, this part of the "C implementation" might define the bounds of the problem:


```C
#define MAXPRIME 99                                             // greatest prime factor
#define MAXPARENT 99                                    // greatest parent number
#define NBRPRIMES 30                                    // max number of prime factors
#define NBRANCESTORS 10                                 // max number of parent's ancestors
```


But there might be additional constraints implemented in the algorithm and/or data structures. This kind of thing really belongs in the task description.

Hypothetically, I might try inspecting some of the code samples, to see how they behave. But, for example, the C implementation does not compile. Among its problems is that it tries to do <code>malloc(sizeof(Children))</code> when what has been defined is struct Children.

(I am rather reluctant to attempt to debug someone else's buggy code to attempt to reverse engineer the bounds of the problem when I could more easily ask for those bounds.)

So... would it be possible to rephrase the task to address these issues? --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 02:53, 26 February 2015 (UTC)


:Hi, too.
(1) Timing. I already addressed this question, yesterday. See my answer to Paddy3118, point 3, above.

(2) I posted "... is to add the decomposition into prime factors of a number to get its 'ancestors'".

What difference does it make if there are more than 2 prime factors ?

I also gave the example of 3^33 = 5.559.060.566.555.523 (parent 99).

That is, 3^33 --> 3+3+3+3+3+3+3+3+3+3+3+3+3+3+3+3+3+3+3+3+3+3+3+3+3+3+3+3+3+3+3+3+3 = 99


The ancestor lists has to be provided for a delimited set of ancestors (from 5 to 99), see the task description.

And, yes, some ancestors may have common ancestors.


(3) If 25 is the Parent of 46, you can deduct that 46 is a descendant (child) of 25.

And I posted : "... use the decomposition into primes for all the numbers between 5 and 3^33".


The 557 descendants of the ancestor 46 are posted, since yesterday, see my answer to Paddy3118, point 1, above.


MAXPARENT is clearly stated in the task description (ancestors from 5 to 99).

MAXPRIME is also clearly stated in the task description "You only have to consider the prime factors < 100".

This information could also have been deducted from MAXPARENT.

And, yes, 99 is not a prime factor. MAXPRIME is given as an upper bound.

MAXPRIME is used to get a list of all the prime factors which are lower than (or equal to) MAXPRIME.


NBRPRIMES and NBRANCESTORS are only used to delimit the size of 2 tables. They are not relevant to solve the problem.


malloc(sizeof(Children))

Thanks, for this information, it's a typo, my compiler didn't even warn me on that. I changed the C code.

malloc(sizeof(struct Children))


I posted one solution, there are maybe hundreds.

Hope it helps!

--[[User:Old man|Old man]] ([[User talk:Old man|talk]]) 10:39, 26 February 2015 (UTC)

This does help. And it looks like you are counting the number of distinct numbers as opposed to how many times a number occurs.

That said, there's something very quirky about "for a delimited set of ancestors (from 5 to 99)" - you are clearly considering ancestors smaller than 5. All of your examples include such ancestors. By reading your code, I can see what you are doing - but that doesn't make for a good task description (and leads to all sorts of unreasonable hypothesis about what you might be doing, such as those I was considering when I asked my initial set of questions). 

I would highly recommend that you drop the lower limit on ancestors to 2 (because 2 is a relevant ancestor, given the results you are getting) and fix your code so it doesn't crash when you do that. --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 14:38, 26 February 2015 (UTC)

Read the task description, again. All the necessary information are stated in the description.
Some information are clearly stated, some others are somehow hidden (i.e. a prime factor is not its own ancestor nor its own descendant).
Also, the title clearly states "allocate descendants to their ancestors", that is, each descendant is counted for 1.
46 is a descendant of 25, but 46 is also the ancestor of 129. Is this a reason to exclude 46 from the descendants of 25 ? Of course not.

Where ? Give me an example, because there is no ancestor smaller than 5.
5 has no ancestor and 1, 2, 3 and 4 do not have any ancestor nor descendant.

I think, you are out of context, I posted a task, but, I do not impose a solution.

2 is not a relevant ancestor, because 2 is not a ancestor at all. But, if you want to consider 2, 3 and 4 as ancestors, you are free to post an alternate task.

--[[User:Old man|Old man]] ([[User talk:Old man|talk]]) 08:30, 2 March 2015 (UTC)

: I think that the hidden information should not be hidden. Perhaps it is sufficient for the hidden information to be stated on the discussion page, but this hidden information is not information that I get from reading the task description in its current form. I will grant that at first blush your definitions seem like they might be obvious. But this "obviousness" does not hold up under a close examination.

: For example, I feel it's not clear what you mean by 'the title clearly states "allocate descendants to their ancestors", that is, each descendant is counted for 1" -- for one what?

: Still, after studying the output from your C program, I see that each descendant is counted once for each ancestor that it is a descendant of. For example, 20 is a descendant of 8 and it is also a descendant of 9, so it gets counted twice when both 8 and 9 are considered as parents. So if I set MAXPRIME and MAXPARENT to 11, in your C program, it's rather clear that 20 would be counted for 2 (as opposed to being counted as 20 in the total, for example).

: Of course, I understand that can not be what you meant. But without considerable effort spent on your code, and some time in this discussion, I would not know what you had meant. It's worth spending some time addressing the ambiguities of interpretation which necessarily arise from the use of the english language (or any language which humans have developed for speaking with each other - but in this case we are using english). There are good biological reasons for these ambiguities, and it's relatively simple to address them.

: Another difficulty is that in your implementation, for example, 8 is an ancestor of 26, but 26 is not a descendant of 8. Why is the reason for this not stated in the task description? 

: Quoting the generated Ancestors.txt file:

: <lang>[8] LEVEL: 2
ANCESTORS: 5,6
DESCENDANTS: 3
15,16,18
```


: and

: <lang>[26] LEVEL: 4
ANCESTORS: 5,6,8,15
DESCENDANTS: 60
69,133,169,190,228,238,286,340,408,459,484,728,819,975,1040,1155,1170,1232,1248,1375,1386,1404,1650,1715,1760,1980,2058,2112,2376,2450,2673,2940,3136,3500,3528,3969,4200,4480,4725,5000,5040,5376,5625,5670,6000,6048,6400,6750,6804,7200,7680,8100,8192,8640,9216,9720,10368,10935,11664,13122
```


: Do you see why I feel the task is ambiguous? --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 15:59, 2 March 2015 (UTC)


- Hidden does not mean non existent. I see, you don't like small enigmas. Do I have to define what a prime factor is ? And what a decomposition in prime factor is ? I don't think so. But, I agree with you, it requires a little effort of reflection.

- The program counts 2 things, the ancestors and the descendants and I count each descendant for ... 1 descendant. Is it not obviousness ?

- I disagree with you, 20 = (2*2*5) is not a descendant of 8, it's a descendant of 9 (2+2+5).

9 = (3*3) is a descendant of 6 (3+3).

6 = (2*3) is a descendant of 5 (2+3).

I don't know what version of the C program you ran, but it is not the one I posted.
And no, 20 is counted for 1 descendant. Did you notice that I do not ask for the level of the descendants ? (Another hidden information).

- Don't worry, your english is not that bad. But, if you feel more comfortable, we can continue in french.

A considerable effort, really ? I don't believe you.
The main logic is concentrated in the GetChildren function (11 lines of code) where I do the sum and the product of the prime factors.
For the other functions, you can find similar functions on this site. The 'sort', with the tree method, already existed in the early sixties, far before the existence of this site.

- Again, you should have noticed that I do not ask for the level of the descendants, because it defaults to 1, the direct descendants.
If you want to list all the children, the grand-children, the gran-grand-children, etc... you can try.
You can go down up to the limit of your imagination or at least up to the limit of you processor (2^64 - 1).
Even if you exclude the number 4, all the prime factors and all the multiples of the prime factors greater than 97, that's going to be a very huge number of descendants.

- No, I don't see why you feel the task is ambiguous. The only ambiguity I can see, here, is your interpretation of the task description, because where you see ambiguities, I see simply truisms.
It just requires a little common sense and common sense is biological.

- I do not have too much time, to post on this site, I give support on a forum, very promising.
Nevertheless, I've decided to extend the delimited set of ancestors (from 1 to 99).
I think it's a good practice to let the program detect if the numbers have or have not, ancestors and/or descendants.
The source codes have been modified accordingly.
 
Are you happy with that extension ?

--[[User:Old man|Old man]] ([[User talk:Old man|talk]]) 10:27, 3 March 2015 (UTC)

: Yeah, when I said that 20 was a descendant of 8, I was wrong. I don't remember what I was thinking there. (But that hypothesis was obviously wrong...)

: That said, I expected that if 5 were an ancestor of 8, that 8 would be a descendant of 5. And this seems to not be the case, and I do not see where in your task description you make this issue clear.

: You have made it clear in your code, and you've spent considerable time explaining to me in the comments here how I am wrong. And I appreciate that. But what I'm really looking for is an adequate task description. --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 17:09, 4 March 2015 (UTC)


- No problem, everybody can do mistakes. I don't blame you, since you agree with me that 1 number can have 0 or 1 direct ancestor.

- It is specified. I gave the greatest number 3<sup>33</sup>. 3<sup>33</sup> is a direct descendant of 99.
But I'm good prince, I concede it, for more clarity and for newbies. I replace "all descendants" by "direct descendants".

The code didn't change that much since the original post. I'm surpise that you suddenly see it clearer.
But, ok, it's a good start.

- Considerable time ? No. I spend a few minutes between 2 supports, only, when I have some spare time.
I think you surely spend more time, trying to find flaws, than what I spend to teach you.
It is very relaxing to teach others.

--[[User:Old man|Old man]] ([[User talk:Old man|talk]]) 18:48, 5 March 2015 (UTC)

: Actually I was trying to find the definitions of your terms - I'd rather work forward from the definitions than reverse engineer your code. What you see as me trying to find flaws was, from my point of view, an attempt to piece together a set of definitions which would satisfy you and be consistent with my understanding of the words. --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 19:09, 5 March 2015 (UTC)


Could you be more specific ?

--[[User:Old man|Old man]] ([[User talk:Old man|talk]]) 11:33, 6 March 2015 (UTC)

See [[Primes_-_allocate_descendants_to_their_ancestors#Definition_of_terms]] for my current understanding. --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 19:11, 6 March 2015 (UTC)

==Parent-Child relationship==
Hi again "Old man". I thought I would note something that confuses me when reading your task descrition: The parent-child relationship seems the other way around to me?

When you state:

```txt
46 = 2*23 --> 2+23 = 25, is the parent of 46.
```

I see 25 being derived ''from'' 46. Normally I think of the child being derived from the parent but you have the derived-from relationship going the other way?

--[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 17:37, 4 March 2015 (UTC)

P.S. I do note that after pondering #thedress, the answer might well be "blue and black"


:Hi,

:I see what you mean, but with such reasoning, humanity has absolutely no future.

:Personally, I'd prefer to discuss normality. I posted, 46 has 557 children and you'd like, 46 has 557 parents.
:I can say one thing : What an orgy!
:Why not ? LOL. Excellent! LOL.
:Thanks for this little touch of humor. This was missing.

:I didn't see the signature button, before today.
:I copied and pasted the signature from post to post, changing date and time.
:But, my signature still does not appear "blue and black", with the button.

--[[User:Old man|Old man]] ([[User talk:Old man|talk]]) 19:03, 5 March 2015 (UTC)

: To make your signature appear blue and black, you'll probably want to click through where it's red, and then save something on the page where you land. --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 19:14, 6 March 2015 (UTC)
