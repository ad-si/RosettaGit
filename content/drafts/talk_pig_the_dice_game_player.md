+++
title = "Talk:Pig the dice game/Player"
description = ""
date = 2012-11-09T18:50:50Z
aliases = []
[extra]
id = 12299
[taxonomies]
categories = []
tags = []
+++

==Very draft task==
Unlike most of the tasks I start, I have started this one without having a Python solution to hand and so I am unsure of how large a task this is, and may have left important things out of the task description that may lead to non-compatible future edits .

Thanks Tim T. for the many corrections to the hopeless grammar I initially had in [[Pig the dice game]]; I hope my English has improved. --[[User:Paddy3118|Paddy3118]] 05:57, 14 September 2012 (UTC)

==Strategy==
I've put together an estimator for use in deciding whether to reroll or not.  Here's pseudo-code for people that cannot read J:

:consider each roll, for non-1 rolls, each roll's base value is the minimum of itself and 100-(sum of dice currently rolled).  For a roll of 1, the value is -(uncommitted rolls).  Average these to get the estimated value of the current possibilities.  [Or, of course, if you just want to know if the result is positive, you do not need to divide by 6.]  --[[User:Rdm|Rdm]] 19:17, 14 September 2012 (UTC)

::That said, using philosophy behind "you don't have to be faster than the bear", a strategy which takes into account your opponent's score could be superior -- sometimes playing more conservatively because we know that our opponent doesn't have a good chance of winning.  That said, I have not completely convinced myself that this would ever be a good idea -- intuitively, it seems like it could be, but I have not had the time to think it through, yet.  --[[User:Rdm|Rdm]] 19:22, 14 September 2012 (UTC)

I was thinking of implementing a simple always roll n times then hold. I could then do stats on randomly varying n for each player and see if there are any patterns for the winner. But as yet I have no prog at all so its just an idea... --[[User:Paddy3118|Paddy3118]] 03:09, 15 September 2012 (UTC)

The winning strategy needs to take into account the complete game state, namely the current player's total score, his current holding score, and the other player's score.  The function to calculate the player's best strategy and winning chance given a game state is (psuedo code):
<lang>function P(total, this_round, op):
// args: my total, my rolled so far, and opponent's score; it's my turn
	if total + this_round >= 100 then: return 1

	// I yield; my winning chance is whatever opponent doesn't win his
	chance_yield = 1 - P(op, 0, total + this_round)

	// chance to win after rolling a 1
	chance_roll = (1 - P(op, 0, total)) / 6

	// plus chance to win for rolling 2--6
	for roll from 2 to 6 do:
		chance_roll += P(total, this_round + roll, op) / 6

	return max(chance_roll, chance_yield) // choose better prospect
```

Note that this is a recursive relation, and the P function can't be evaluated just like that due to infinite recursions.  There are ways around it by doing matrix iterations or some such, assuming a single function for probability makes sense -- depending on the game type, it's possible that there's no optimal strategy, in which case either the probability matrix won't converge, or there could be multiple stable solutions, etc. I don't think that happens for this game, but calculating the matrix can still be quite daunting. --[[User:Ledrug|Ledrug]] 06:01, 15 September 2012 (UTC)

:Hi Ledrug, Sounds good but I don't want to add a particular strategy to the task description so that people can at least try something that they find easy to code. --[[User:Paddy3118|Paddy3118]] 11:28, 15 September 2012 (UTC)

: It's probably not fair to call the strategy you are defining the "winning strategy" since there's an element of luck in this game.  It's perhaps the "best strategy", but even that's not completely clear:

: We can only assign a probability to our opponents moves if we know how our opponent's strategy. Let's say that we know our opponent's strategy, and that it's this

:<lang>if current total is 0, always roll
if current total is 1 reroll only if the prior total does not exceed 98
if current total is 2 reroll only if the prior total does not exceed 97
if current total is 3 reroll only if the prior total does not exceed 96
if current total is 4 reroll only if the prior total does not exceed 95
if current total is 5 reroll only if the prior total does not exceed 93
if current total is 6 reroll only if the prior total does not exceed 92
if current total is 7 reroll only if the prior total does not exceed 91
if current total is 8 reroll only if the prior total does not exceed 90
if current total is 9 reroll only if the prior total does not exceed 89
if current total is 10 reroll only if the prior total does not exceed 87
if current total is 11 reroll only if the prior total does not exceed 86
if current total is 12 reroll only if the prior total does not exceed 85
if current total is 13 reroll only if the prior total does not exceed 84
if current total is 14 reroll only if the prior total does not exceed 82
if current total is 15 reroll only if the prior total does not exceed 81
if current total is 16 reroll only if the prior total does not exceed 80
if current total is 17 reroll only if the prior total does not exceed 78
if current total is 18 reroll only if the prior total does not exceed 77
if current total is 19 reroll only if the prior total does not exceed 75
Do not reroll if current total is 20 or higher
```


We can compute our opponent's probability of winning with this information if we also know our own strategy.

So let's say that we adopt the same strategy -- now we can compute our opponent's probability of winning, and our own. This allows us to build a table describing probabilities for all possible game states.

Now let's introduce the possibility that for only one move we will calculate a different strategy (and we do this for each game state). This allows us to use your pseudo-code, except that our recursive calls to P are no longer recursive -- they're just table lookups. Now we can build a table based on the static strategy and a table based on the [almost trivial] "dynamic" strategy. If the tables are identical, we are done.

And I think that this game is one where paying attention to our opponent's current point total can improve our odds.  For example, if we have 94 prior points, and we rolled a 5, we have a potential score of 99 points.  If our opponent also has 99 points then we have at least a 5/6 chance of losing if we stop now, and at least 5/6 chance of winning if we roll again, while the above static strategy says we should stop and wait until next turn.  This is an unlikely game state, but it's a valid one.

So, anyways, we can assign even odds to our opponent's chance of picking either strategy, and we can pick the best odds for ourselves from the known strategies, and we can grant ourselves the right to make one choice differently again, at each game state, and compute a new table of odds, then iterate until we find where the sequence of tables converge to. For this game the sequences at each game state should have asymptotes, and finding them would give us an answer without requiring infinite computation.

So, anyways, that result would be the best strategy when faced with certain kinds of opponents. But our strategy might become unstable if we try to model our opponent having knowledge of what we will pick.

Assuming this instability will exist: We can dampen that instability by adding randomness to our choices: we can start with the "best deterministic strategy" determined above, and grant our opponent complete knowledge that we will use that strategy and give them the right to make one choice differently (at any game state) based on that knowledge, and let the opponent iterate until they find a new best strategy. Then we set up a new strategy where we pick (with even odds) from either our first strategy or this new strategy that our opponent would have picked, and let our opponent pick a new best strategy again, iterating... Here, we converge not to a single deterministic Roll/Hold choice for each game state but to a distribution function for each game state (or odds of picking "Roll" for example, in each game state).

But, here, we are still carrying an assumption about our opponent -- for example, that our opponent would be "like us".

To do better than this, we would have to engage in "discovering how our opponent is different from us". Here, we might start with the opponent using a "like us" strategy and then look for evidence that the opponent is using some other strategy, and then attempt to build a model of that strategy and play against our models of our opponent instead of just against an opponent who is "just like us". It's not at all clear, though, that there's any justification for doing that for this game. (And, of course, this approach is also intractable in the general case (where it's not trivial and irrelevant) because the number of implementations open to our opponent is infinite.)  --[[User:Rdm|Rdm]] 12:02, 15 September 2012 (UTC)

: By "winning strategy" I meant the choice of roll or hold that maximizes the chance of winning at a given board state, that's what the P() function is.  Instead of thinking it as function, consider P as a matrix with board state as its subscripts, and the elements need to satisfy the relations given in the code.  An optimal strategy exists (for both sides) if there is a unique set of P values that satisfy all the interrelations; if no such solution exists, or there are multiple sets of solutions, then this game has no global optimal strategy.  But, if a unique solution does exist, then there's no need to consider what strategy the opponent would choose: you just do what gives you best winning chance. If your opponent is smart enough, he'd do the same, otherwise it's all the worse for him.
: I believe this particular game has a unique solution, and I do have ''a'' solution of the P matrix, although proving it's unique is not easy.  The holding choice pattern is pasted [http://pastebin.com/SWhU4mL7 here], where each row starts with two numbers, your current score and current holding score; the horizontal axis is the opponent's current score.  Wherever there's a dot, it means you're better off hold; otherwise you should roll.  For example, on line "0 21: ", there are 11 dots at the begining of the line, which means: if your total score is 0, and have already rolled 21 points this turn, then your best choice is hold if opponent has 0-10 points, but better keep rolling if he has 11 or more. The graph has many odd features, but generally makes sense. --[[User:Ledrug|Ledrug]] 13:13, 15 September 2012 (UTC)

:: That's an interesting set of data.  I would particularly like to understand the "concave" parts of the holding pattern -- especially for right-facing concavities (such as the rightmost part of 36 22: through 36 25:).  Those features seem counter-intuitive to me.  Thanks.  --[[User:Rdm|Rdm]] 23:39, 15 September 2012 (UTC)

::: Well, simple flukes there.  In that region the winning chances are about .2 ~ .3, but the differences between hold and roll are very small (<1e-3), which could simply be because of loss of floating point precision. It isn't very meaningful. --[[User:Ledrug|Ledrug]] 00:33, 16 September 2012 (UTC)

:::: There are other concave regions (both vertically facing and right facing).  Are they all "doesn't matter much what you choose" issues? --[[User:Rdm|Rdm]] 12:49, 16 September 2012 (UTC)
::::: I don't know, probably. I can't paste the graph with numbers in it, the file would be too big for pastebin.  Instead the code generating numbers is in [[User:Ledrug/bits]] at the top, you can play with it to see the numbers (warning: huge output on stdout). --[[User:Ledrug|Ledrug]] 21:31, 16 September 2012 (UTC)

I should at least include a strategy of random play in my solution so that I can gauge any other strategy against it.--[[User:Paddy3118|Paddy3118]] 11:28, 15 September 2012 (UTC)


### Tournament of Pigs

I decided to have various strategies fight it out and see who has the highest winning chance.  Contestants:

A: always roll

R: randomly roll. Roll if holding score is 0, otherwise has 1/3 of chance to hold.

H: hold at 20.  Roll if opponent >= 80; roll if self score >= 78; roll if holding score <= 20; hold otherwise.

O: the optimal holding pattern discussed above.

Each strat also has an "anti": AntiX is the holding pattern that maximizes winning when playing agains X.  Note that AntiO ''is'' O.  The following table is the resulting winning chances when one plays agains another in row major, that is, the top right corner means "when playing agains O, strat A has 0.121 chance of winning if move first, 0.112 if move second".  The third number is just the sum of those two numbers; if it's gerater than 1, the strat has better overall odds when playing that opponent.

```txt

                A                AntiA                 R                 AntiR                 H                AntiH                 O
  A    0.503/0.497 1.000   0.122/0.113 0.235   0.168/0.160 0.329   0.139/0.130 0.268   0.124/0.115 0.238   0.121/0.112 0.234   0.121/0.112 0.233
AntiA  0.887/0.878 1.765   0.529/0.471 1.000   0.806/0.768 1.575   0.577/0.520 1.097   0.570/0.510 1.081   0.530/0.471 1.001   0.524/0.464 0.988
  R    0.840/0.832 1.671   0.232/0.194 0.425   0.528/0.472 1.000   0.191/0.153 0.343   0.206/0.168 0.374   0.215/0.175 0.390   0.221/0.181 0.402
AntiR  0.870/0.861 1.732   0.480/0.423 0.903   0.847/0.809 1.657   0.527/0.473 1.000   0.509/0.452 0.962   0.473/0.415 0.889   0.475/0.416 0.891
  H    0.885/0.876 1.761   0.490/0.430 0.919   0.832/0.794 1.626   0.548/0.491 1.038   0.529/0.471 1.000   0.483/0.421 0.904   0.489/0.428 0.917
AntiH  0.888/0.879 1.766   0.529/0.469 0.999   0.825/0.785 1.610   0.585/0.526 1.111   0.579/0.517 1.096   0.530/0.470 1.000   0.527/0.466 0.993
  O    0.888/0.879 1.767   0.536/0.476 1.012   0.819/0.779 1.598   0.584/0.525 1.109   0.572/0.511 1.083   0.534/0.473 1.007   0.530/0.470 1.000

```

One funny thing is, AntiA is not the best at beating A; I blame loss of floating point precision for this, though I'm not completely sure.

===Someone else's strategy===
I found a [http://cs.gettysburg.edu/projects/pig/piggame.html site] and a paper "Optimal Play of the Dice Game Pig" by Todd W. Neller and Clifton G.M. Presser. I glossed over the pretty graphs which may however be something like what Ledrug is computing for his optimal strategy. I was after simple strategies and picked out their mention of 20 as being the accumulated points in a round where the odds of throwing a one are balanced by accumulated point.

They also go on to describe why roll till 20 fails when getting nearer to the end of a game where it is advantageous to 'sprint for the win'. I ignored their full optimised strategy and just coded a 'region of desperation'. If any player is within 20 of finishing then this player should keep on rolling until it either wins or is bust as another player is likely to win on its next go. --[[User:Paddy3118|Paddy3118]] 05:55, 17 September 2012 (UTC)
: Holding at 20 is obviously not universal: if your opponent has 98 or 99 points, you have more than 50% chance of losing if you hold at any point before 100.  If you want a simple rule, this is closer to truth: if your opponent has more than 80ish, roll no matter what; if you have more than 78 points, roll no matter what; otherwise hold if your holding score is more than 20ish.  That should be a pretty good rough approximation.
: The fact is, at any give point in the game, the probabilities of roll vs hold never differ all that much: the winning chance of rolling is never lower than 80% that of holding.  If you want a ''really'' simple rule, "just keep rolling" is the simplest.  But as a game, a good strategy has a lot do with human perception of the outcome rather than boring mathematics.  Take for an example, the absurd situation where both players are at 0, but you somehow with a streak of terrific luck and had rolled 99 so far. What to do at this point? If you hold, you have 98.8% chance winning; if you roll, it's 91.2%.  You can hardly say the odds are against you if you choose to roll, but you'd be kicking yourself really hard under the table if you rolled a 1.  Really, what one may consider a "good enough" strategy sometimes has nothing to do with facts at all. --[[User:Ledrug|Ledrug]] 07:26, 17 September 2012 (UTC)

== Table of Contents (List of implementations) ==

I added the Go example, but there seems to be no 'Table of Contents' under the task description. Forgive me, but I'm a complete noob, and don't know how to add it. [[User:La Longue Carabine|La Longue Carabine]] 18:43, 9 November 2012 (UTC)
:Hi, I forced it to appear, but normally it appears when there are four or more examples.--[[User:Paddy3118|Paddy3118]] 18:46, 9 November 2012 (UTC)

::Thanks. I guess I should have searched around a little more, finally found that out by looking at the Wikipedia Cheat Sheet. [[User:La Longue Carabine|La Longue Carabine]] 18:50, 9 November 2012 (UTC)
