+++
title = "Talk:Factors of an integer"
description = ""
date = 2018-10-03T17:33:38Z
aliases = []
[extra]
id = 4738
[taxonomies]
categories = []
tags = []
+++

==Should this be "Basic language learning"?==
Should this be "Basic language learning"? I don't even know.
[[User:Oligomous|Oligomous]] 12:10, 15 August 2009 (UTC)

This is a bit too similar to [[Prime decomposition]] for my taste.  --[[User:IanOsgood|IanOsgood]] 15:48, 15 August 2009 (UTC)

The Clojure, <strike>Python</strike>, <strike>Ruby</strike> and <strike>Tcl</strike> examples are all wrong; they omit the number itself which is always a factor (just as 1 is). Oops! --[[User:Dkf|Donal Fellows]] <small>(not logging in on this computer; don't own it...)</small>

:No they are not wrong as such, the task description is very woolly. Do we mean integer factors? What about negative factors? Do we include zero?
:The task specification needs to be tightened up, so following what was given would most likely fit what was envisaged, but I should have flaagged for clarification. --[[User:Paddy3118|Paddy3118]] 03:19, 16 August 2009 (UTC)
:: If 1 is deemed to be a factor of 42, then so must 42 be; [[wp:Divisor|Wikipedia]] agrees with me. I've never heard of anyone defining factors for anything other than positive integers and in terms of positive integers; if I remember my schooling, it's usually a lead-in to talking about prime numbers. The wikipedia article also talks about "proper divisors", which is what the examples here were originally delivering, but that's probably not the best way to fix this task. --[[User:Dkf|Donal Fellows]]

==task description==

The first sentence should be "Find the factors of a positive integer.", as the second sentence goes on to define what the factors are of a positive integer, not a number. -- [[User:Gerard Schildberger|Gerard Schildberger]] 04:57, 30 April 2012 (UTC)


The last preamble sentence in the Rosetta Code task: 
::*   ''Note that even prime numbers will have will have at least two factors; ‘1’ and themselves.''
would read better as: 
::*   ''Note that''    all   ''prime numbers will have''   exactly two   ''factors:   unity and the prime number itself''.


When I first read the above (original) sentence, I thought:   "why single out the even prime (as there is only one of those, namely   <big>2</big>),   ··· and exclude the odd primes?   -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 23:23, 14 June 2015 (UTC)



Also, worthy of note is:   All   '''semiprimes'''   (also known as '''biprimes''' or '''2-almost primes''')   have exactly three factors:   unity and two (not necessarily distinct) primes.   -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 23:23, 14 June 2015 (UTC)

: I made the wording more verbose to avoid the issue with the word "even". Of course, this leaves open issues such as factors of non-integers, and verbosity sometimes makes problems worse. But maybe it's good enough for now? --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 15:12, 15 June 2015 (UTC)

==Python relative performance==
Hi Ledrug, care to join us over on [[Talk:Proper_divisors#Python:_comparisons]]... --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 08:45, 20 December 2014 (UTC)

== fix bug in k ( kx.com's dialect of APL ) ==

The code 
```K
   f:{d:&~x!'!1+_sqrt x;?d,_ x%|d}
```

was not working with current KDB version .. "~x!'" gives a type error
"q" is a literate wrapper for "k" so I provided that to make it clearer what the code is doing

```K
q) f:{i:{y where x=y*x div y}[x ; 1+ til floor sqrt x]; distinct i,x div reverse i}
```


== Formatting very wide Haskell lines ==

Very nice final list comprehension example there. Thanks !

Perhaps Johan Tibell style (https://github.com/tibbe/haskell-style-guide) or hindent (http://chrisdone.com/posts/hindent-5) to bring the 97-character within the 80 character Rosetta guidelines, and for ease of reading  ? (PS maybe that Data.List import is not needed with current default builds of GHC ? Those functions all seem to be Prelude default)


```haskell
factors_o :: Integral a => a -> [a]
factors_o n =
  ds ++
  [ r
  | (d, 0) <- [divMod n r] 
  , r <-
     r :
     [ d
     | d > r ] ] ++
  reverse (map (n `div`) ds)
  where
    r = floor (sqrt (fromIntegral n))
    ds =
      [ i
      | i <- [1 .. r - 1] 
      , mod n i == 0 ]
```


: A rare 97 character line does not seem like a problem, to me. --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 16:29, 29 December 2016 (UTC)
