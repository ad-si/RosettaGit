+++
title = "Blackjack strategy"
description = ""
date = 2014-11-12T11:09:38Z
aliases = []
[extra]
id = 14761
[taxonomies]
categories = ["task"]
tags = []
+++

{{draft task}} [[Category:Games]]
The objective of this task is to recreate and explore the following [https://web.archive.org/web/20130625120019/http://www.blackjacktactics.com/blackjack/strategy/charts/single-deck/ strategy charts] for the game of [[wp:Blackjack|blackjack]] (which is known by many other names as well).

Assume that my casino:
* uses a single deck,
* does not allow Double after split,
* pays out 3 to 2 for Blackjack, and
* uses [https://web.archive.org/web/20140822052840/http://www.blackjacktactics.com/blackjack/how-to-play// these rules].

Begin by assuming the player's dealt hand contains no aces and is not a pair. 
Create functions which given the players dealt cards and the dealers exposed card returns the number of wins and losses for all possible continuations when the player either sticks or hits. 
Gather the results together, set a threshold at which you consider it wise to Double the bet and reconstruct the Hard Totals Table enhanced with precise probabilities.

Enhance your analysis by considering the case when the player's hand contains an Ace. Again by considering all continuations recreate the Soft Totals Table again enhanced with precise probabilities.

Finally complete your analysis by considering the case when the player's hand contains a pair. 
Again by considering all continuations recreate the Pair Splitting Table again enhanced with precise probabilities.

You should now create a function which randomly deals hands. 
Assuming I play 50 hands at a visit and visit everyday for a year, 
applying the strategy defined by the tables you have created, 
answer the following questions:
* How many days can I expect to win/lose?
* What can I expect to be my biggest win?
* What can I expect to be my biggest loss?
* What can I expect to win/lose over the year?
