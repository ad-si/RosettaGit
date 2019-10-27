+++
title = "Talk:Array length"
description = ""
date = 2015-10-23T12:42:48Z
aliases = []
[extra]
id = 19630
[taxonomies]
categories = []
tags = []
+++

'''JavaScript'''

I like the direction of rdm's suggestion about efficiency and the cost of folds (depending of course, on implementation - the Haskell prelude 'last' is a fold). ES6, which has put some work into tail-recursion optimisations, may be a bit better than ES5 as a vehicle for functional styles of code composition.


```txt
function last(list, defaultValue) {
   return list.length ?list[list.length-1] :defaultValue;
}
```


Taking it one step further, I guess one could also argue that, depending on implementation, two calls to the length function, though probably unlikely to involve two scans of the whole array, might also look a bit profligate.

Perhaps just cache it ?:

```txt
function last(list, defaultValue) {
    var lng = list.length,
        return lng ? list[lng-1] : defaultValue;
}
```


Though once we announce the start of the pre-optimisation season, we are probably also inviting someone to pop up and point out, quite correctly, that JS implementations of ternary expressions tend to be less efficient than their implementations of if then else statements :-) [[User:Hout|Hout]] ([[User talk:Hout|talk]]) 16:22, 8 October 2015 (UTC)

: Yep. And on a related note Haskell's fold optimizations (where non-used elements aren't fetched at runtime) do have a compile time cost.

: And it's also possible that different JS implementations will optimize ternary expression handling. But mostly I don't care about optimizations which yield less than a factor-of-2 improvement - with exceptions to the rule, of course, being in the context of resource-critical bottlenecks. Reason being that quite often there's factor of 1000 (or better) optimizations available if you instead take some time to digest the relevant issues. (And the reason for that is that efficiency is not a universal good, but something which relates to a specific effort.) --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 17:21, 8 October 2015 (UTC)

::Absolutely – which is why it seems a pity to be starting up more of these simple documentation-lookup entries –  missing the chance to actually formulate a problem and task (external to the inner concerns of particular models of computation). A task focus does does yield much more insight and relevance (as well as better coverage across languages).[[User:Hout|Hout]] ([[User talk:Hout|talk]]) 17:38, 8 October 2015 (UTC)

'''Was this documentation-snippet pseudo-task foisted on us by a spamming scam ?'''

:I notice from the history that this documentation-snippet pseudo-task arises from one of these '''BugMeNot''' (login-credential evasion service) logins which have subsequently tried to repeat the trick, perhaps to establish false credentials from which spamming exercises can be launched.

:Clearly somebody has spotted that editorial protection of the central Rosetta Code principle of '''task focus''' has been a bit slack :-) 

:Failure to distinguish between genuine tasks and facile documentation-lookup entries has become a bit of an Achilles heel. It's understandable – facile pseudo-tasks are very quickly 'solved', generating the specious impression that this kind of entry is "popular" – when in fact we have no idea how much reader demand there actually is for them – they may, in fact, be egregiously unpopular – and they certainly violate the principle of task focus, scoring exceedingly poorly in the 3 Rosetta axes of '''1.''' relevance to as many languages as possible '''2.''' as much insight as possible '''3'''. as much value to learners as possible.

: A serious blind spot, and a source not just of poor quality, but also of vulnerability to spamming exploits, it turns out.

: At the very least, if there were signs of interest in this area, it should have been immediately reformulated towards less superficiality and at least some vague semblance of a genuine Rosetta task. Even something as simple as ''Find the last element of an ordered sequence'' would have generated richer and more useful material.

:: I must say that i highly value these simple tasks. I mentioned my use case on [[Talk:Array search]]. I don't think this discussion should be repeated all over the wiki, so i'm not gonna repeat it here. --[[User:Bugmenot2|Bugmenot2]] ([[User talk:Bugmenot2|talk]]) 12:42, 23 October 2015 (UTC)
