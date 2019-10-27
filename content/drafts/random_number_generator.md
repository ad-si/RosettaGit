+++
title = "Random number generator"
description = ""
date = 2011-08-09T15:19:28Z
aliases = []
[extra]
id = 10001
[taxonomies]
categories = []
tags = []
+++

A '''random number generator''' picks numbers at chance from a distribution.

The most common type is a ''uniform generator'', where each number has an equal chance. If a uniform generator yields decimal digits from 0 to 9, then each digit has a 10% chance. The other type is a [[unbias a random generator|''weighted'' or ''biased'' generator]], where the chances are not equal. A biased generator that yields 0 with 70% chance, yields 1 with 30% chance, would probably yield more zeros than ones.

''True random numbers'' are impossible to predict. Many programs use ''pseudorandom numbers'', which are not as good. A pseudorandom generator uses a formula and a state to calculate a sequence of numbers. Anyone who knows the formula and the state can predict those numbers.

Pseudorandom numbers can also have statistical problems. True random numbers are [[wp:independence (probability theory)|independent]]. A pseudorandom sequence may contain patterns where the values of some numbers change the probabilities of other numbers.

See also [[Pick_random_element]].

== Tasks ==
* [[Random number generator (included)]]
* [[Random number generator (device)]]
* Everything in [[:Category:Randomness]]

== Links ==
* Wikipedia's article about [[wp:Random number generation|Random number generation]]
* [http://csrc.nist.gov/groups/ST/toolkit/rng/index.html NIST's statistical tests for random numbers]

[[Category:Encyclopedia]]
