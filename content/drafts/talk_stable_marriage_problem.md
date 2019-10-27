+++
title = "Talk:Stable marriage problem"
description = ""
date = 2016-07-24T16:55:29Z
aliases = []
[extra]
id = 7896
[taxonomies]
categories = []
tags = []
+++

==Strategy?==
I guess that if this were a marriage game then the best strategy for a guy would be to ask quickly and ask often. For a girl (who is not allowed to ask remember)? It seems girls can have no nothing better than they are given. Bummer! --[[User:Paddy3118|Paddy3118]] 09:45, 6 August 2010 (UTC)

Yet the girl is the one that is allowed to change her mind over and over agiain, not that bad I would say. Kind of like in real life, :D --[[User:Jofur|&lt;Jofur&gt;]] 10:12, 6 August 2010 (UTC)
:: Offtopic banter can be enjoyable in talk pages and IRC, but remember it's a public place, and be wary of escalation. :) --[[User:Short Circuit|Michael Mol]] 14:21, 6 August 2010 (UTC)

: You might like to find space to duck :-)
I guess girls who ditch the rules and ask the guys win big time. --[[User:Paddy3118|Paddy3118]] 12:35, 6 August 2010 (UTC)

::True that a girl who ask her first choice would have him, but the total sum may suffer since he may be much happier somewhere else. This is shown with the stability test, no pair where both want each other more than their partner, e.g. even if Ben wants Ada more than his wife Ellen, Ellen should not have the same values or the sort has failed. 

::This bring up old memories from those classes in national economy, where one thing was how to calculate the maximum sum of ‘usefulness’ that any resource could do in a society. And that is neither “all to one”, or “equal to all”. Resources where according o the maximization thesis to be spread so that their total sum is maximized, e.g. alls individual needs for different resources valued against their specific needs and available resources… I think I just connected it into the Knapsack series… --[[User:Jofur|&lt;Jofur&gt;]] 12:59, 6 August 2010 (UTC)

==Test==
Just found [http://www.spoj.pl/problems/STABLEMP/ this site] which has two testcases and expected results. I modified the Python solution:

```python

if True:
    ## Sphere online judge dataset from: http://www.spoj.pl/problems/STABLEMP/
    ## (But they print couples in reversed order)
    galprefers = dict( (x.strip().split()[0], x.strip().split()[1:])
                       for x in ''' 1 3 4 2 1 6 7 5
                                    2 6 4 2 3 5 1 7
                                    3 6 3 5 7 2 4 1
                                    4 1 6 3 2 4 7 5
                                    5 1 6 5 3 4 7 2
                                    6 1 7 3 4 5 6 2
                                    7 5 6 2 4 3 7 1'''.split('\n') )
    guyprefers =  dict( (x.strip().split()[0], x.strip().split()[1:])
                       for x in ''' 1 4 5 3 7 2 6 1
                                    2 5 6 4 7 3 2 1
                                    3 1 6 5 4 3 7 2
                                    4 3 5 6 7 2 4 1
                                    5 1 7 6 4 3 5 2
                                    6 6 3 7 5 2 4 1
                                    7 1 7 4 2 6 5 3'''.split('\n') )
    guys = sorted(guyprefers.keys())
    gals = sorted(galprefers.keys())


```

Which then went on to provide the correct result. (I was relying on the check() function before. --[[User:Paddy3118|Paddy3118]] 14:04, 6 August 2010 (UTC)


==Stability Check==
The stability check seems to be giving at least two entries some problems in what to report. --[[User:Paddy3118|Paddy3118]] 16:02, 18 August 2010 (UTC)

: Can you be more specific about what the problem is? For the PicoLisp solution you wrote "checkCouples routines output is not clear". What is not clear, the wording? Or the logic? --[[User:Abu|Abu]] 12:02, 20 August 2010 (UTC)

::Hi Abu, What is not clear is that the wording given in the check message is equivalent to the stability criterion which is that 
:::"No male would rather be with ''another'' female ''where that other female would also prefer that male over her current partner''" 
::You need the logic to search for the above but would only need to state that 
:::"X, and Y would rather be together than with their current respective partners"
::Where X and Y are not of the same sex and are the found names violating stability. The PicoLisp error message says "X likes Y1 better than Y2", which may well be the case, but does not point out how it violates the the stability criterion. Looking at the Python example, the check there could be better too, as it assumes that you know that the third name (dan) is the firsts partner. I'll fix it later. --[[User:Paddy3118|Paddy3118]] 14:34, 20 August 2010 (UTC)
:::Hi Paddy, OK, I see. Half of the message is missing (though the code itself states it fully in the 'and' expression and the comments). Thanks! --[[User:Abu|Abu]] 12:34, 23 August 2010 (UTC)


### J Stability Check

Again, it seems hard to work out that the stability criteria is violated using the results of the current J stability check. --[[User:Paddy3118|Paddy3118]] 06:55, 3 September 2010 (UTC)

:I do not understand this comment.  Do you need another example to show what the stability check looks like when the result is stable?  (There would be no output for that case).  Do you need the result formatted in a different fashion?  Do you need additional information (if so what additional information do you need)?  For now, I am removing the comment on the main page because as near as I can tell the J implementation complies with the task description as is.  --[[User:Rdm|Rdm]] 12:37, 3 September 2010 (UTC)

::Ok, never mind -- I had left a reference to what is now an undefined value for the stability check.  I got rid of that now.  --[[User:Rdm|Rdm]] 12:42, 3 September 2010 (UTC)

== Title ==

Since this specifies the Gale-Shapley algorithm, shouldn't that be the name of the task? Similar to how [[Lucas-Lehmer test]] finds Mersienne primes? --[[User:Short Circuit|Michael Mol]] 16:30, 23 August 2010 (UTC)
: Ah, your thinking logically. That's your problem! :-)
: Stable marriage problem is more often quoted than the algorithm used to solve it, although it looks like that algorithm is the only one used to solve the problem (apart from exhaustive search). See [http://googlefight.com/index.php?lang=en_GB&word1=%22Gale+Shapley+algorithm%22&word2=%22Stable+marriage+problem%22 this]. --[[User:Paddy3118|Paddy3118]] 21:58, 23 August 2010 (UTC)

:: Created [[Gale-Shapley algorithm]] as a redirect to this task. –[[User:Dkf|Donal Fellows]] 14:34, 26 August 2010 (UTC)
::: An excellent solution I think! --[[User:Paddy3118|Paddy3118]] 15:06, 26 August 2010 (UTC)

== Incorrect statements of incorrectness ==

Currently the history for the main page looks like this:

 (cur) (prev)  2010-09-08T14:48:58 Rdm (Talk | contribs) (45,168 bytes) (J: remove incorrect statement of incorrectness, again -- am adding to discussion page) (undo)
 (cur) (prev)  2010-09-08T05:11:21 Paddy3118 (Talk | contribs) (45,259 bytes) (→<nowiki>{{header|J}}</nowiki>: Marked as incorrect as failures of the stability criterion are not clearly shown. (See talk).) (undo)

And, currently, the revision history of this page looks like this:

 (cur) (prev)  2010-09-03T12:42:44 Rdm (Talk | contribs) (6,507 bytes) (→J Stability Check) (undo)
 (cur) (prev)  2010-09-03T12:37:04 Rdm (Talk | contribs) (6,330 bytes) (→J Stability Check) (undo)
 (cur) (prev)  2010-09-03T06:55:30 Paddy3118 (Talk | contribs) (5,812 bytes) (→J Stability Check: Can't work out that stability is violated with the results as given.) (undo)

In other words, a week ago Paddy marked the J solution as incorrect.  I asked him what was incorrect about it, and then noticed a problem in the example usage, and fixed that.  Now, a week later, he re-asserts that the solution is incorrect but with no statements about what is missing.  So, I have removed his statement that it is incorrect and I am a little upset about this.

If we look at the main page's treatment of the stability criteria, we see in the task description:

:# ...
:# Perturb this set of engagements to form an unstable set of engagements then check this new set for stability.

And we see in the J implementation:

:Stability check:

:
```j
   1 2 A."_1 matchMake '' NB. perturbed matches
┌───┬────┬───┬───┬───┬────┬───┬────┬───┬───┐
│abe│bob │col│dan│ed │fred│gav│hal │jon│ian│
├───┼────┼───┼───┼───┼────┼───┼────┼───┼───┤
│ivy│cath│dee│fay│jan│bea │gay│hope│eve│abi│
└───┴────┴───┴───┴───┴────┴───┴────┴───┴───┘
   checkStable 1 2 A."_1 matchMake ''
Better matches:
┌───┬────┐
│jon│fay │
├───┼────┤
│jon│gay │
├───┼────┤
│jon│abi │
├───┼────┤
│ian│hope│
└───┴────┘
|assertion failure: assert
|       assert-.bad
```


Now the task description defines a stable relationship as:

:A stable set of engagements for marriage is one where no man prefers a women over the one he is engaged to, where that other woman also prefers that man over the one she is engaged to. I.e. with consulting marriages, there would be no reason for the engagements between the people to change.

So I would think that identifying a relationship where both parties prefer each other over their previous pairings would qualify as showing that the set was not stable.

And, for example, in the above example, <code>jon</code> and <code>fay</code> are listed as a pair where they both prefer each other over their previous pairs.  And if we look at the perturbed data that was used, jon was paired with eve and fay was paired with dan.  And if we look at the preference data we see:
   jon: abi, '''fay''', jan, gay, '''eve''', bea, dee, cath, ivy, hope
   fay: bob, abe, ed, ian, '''jon''', '''dan''', fred, gav, col, hal

So, clearly, the jon+fay paring by itself should be sufficient proof that the perturbed set of relationships was not stable.

So, in my opinion, Paddy has no basis for saying that the J solution is incorrect.

That said, if he wants to change the task description, adding new requirements for the stability check, I would be happy to satisfy those requirements.  But I am not going to agree to fix some imaginary problem without a detailed explanation of what that problem is.

--[[User:Rdm|Rdm]] 15:01, 8 September 2010 (UTC)

:Hi Rdm, First my apologies. I did not mean to upset, and I though the arguments for the other cases of where I thought the stability check was unclear might suffice. I should add that I too went and added to my original Python example to make it clear.
:An issue I had with other checks was that they didn't show ''why'' stability was violated without having to compute the stability criterion in your head, i.e. pairing A and B was better ''because'' A likes B better than present partner C and B likes A better than present partner D. I thought that just showing a pairing wasn't enough. again, it is just an opinion, and I am sure the site can come to a consensus, especially if others chip in. Does the task description need expansion? Or should we accept that a check routine that prints a pairing that could be shown to violate stability is enough? --[[User:Paddy3118|Paddy3118]] 15:38, 8 September 2010 (UTC)

::Thanks for giving your perspective.  This does indeed help me.
::Personally, I am happy with showing pairings where both partners would prefer each other over their previously asserted partners.  That said, if you want additional information to be reported, I would be happy to provide it -- I just want that to be a part of the task.  --[[User:Rdm|Rdm]] 18:09, 8 September 2010 (UTC)

I have changed the example perturbation to be the same as the one used in other examples to facilitate easier comparison. I've also edited the introductory text for the list of better matches to explicitly state why they are better. Hopefully that will satisfy everyone. I note that apart from the J solution, currently only the PicoLisp solution enumerates all the better matches that would cause a set of engagements to be unstable. The J and PicoLisp solutions agree. --[[User:Tikkanz|Tikkanz]] 21:44, 8 September 2010 (UTC)

== Problem with the Tcl example? ==

The test output for the Tcl example finishes with:
:Swapping two fiances to introduce an error
:  abi is now engaged to fred
:  ...

:... and abi likes fred better than their current partner

Huh?
--[[User:PhilThornley|PhilThornley]] 16:48, 12 September 2010 (UTC)

: Hi Phil, the full text for the TCL check is:

```txt
Swapping two fiances to introduce an error
  abi is now engaged to fred
  bea is now engaged to jon

fred likes bea better than abi and abi likes fred better than their current partner
Engagement stability check FAILED
```

:Which is a much better statement of violation of stability than for the SPARK example. Could you possibly read the sections above on writing a checker that prints output that doesn't need too much mental gymnastics to work out how the stability ccriterion is violated? Thanks. --[[User:Paddy3118|Paddy3118]] 17:10, 12 September 2010 (UTC)

::Hi Paddy - but if "abi is now engaged to fred" then "their current partner" for abi is fred, so:
::"abi likes fred better than their current partner" == abi likes fred better than fred

::I think that the output from the D code is correct, what's your opinion?

::--[[User:PhilThornley|PhilThornley]] 19:13, 12 September 2010 (UTC)

::: Hey, you ''are'' right! There is a problem with the TCL examples checking routine! Sorry to doubt. --[[User:Paddy3118|Paddy3118]] 21:05, 12 September 2010 (UTC)
: The error message was the same as the one originally produced by the Python version, but hadn't been updated when that other code was changed. –[[User:Dkf|Donal Fellows]] 08:58, 16 September 2010 (UTC)

== Nobel Prize ==

The 2012 Nobel Prize in Economics was awarded to Roth and Shapley for work that began with this algorithm.  It seems the algorithm was the start of a new field called "market design" that goes far beyond marriage design, and that even the original Gale/Shapley algorithm has more general applications.  &mdash;[[User:Sonia|Sonia]] 21:22, 16 October 2012 (UTC)

: Thanks for this nugget. --[[User:Paddy3118|Paddy3118]] 15:54, 17 October 2012 (UTC)

== OOP? ==

After looking at some of the solutions here, I noticed that there are several that used a language with OOP capabilities, but did not come up with an object-oriented solution. I'm not saying that's good or bad necessarily; but it would be interesting to see more solutions that go beyond translations of straight procedural logic. --[[User:MikeLorenz|Mike Lorenz]] 05:06, 3 November 2012 (UTC)

== PHP ==

If you want you can add my PHP port of the Python version from: http://www.leaseweblabs.com/2013/04/stable-marriage-problem-in-php/

--[[User:Maurits|Maurits]] ([[User talk:Maurits|talk]]) 15:01, 24 April 2013 (UTC)
