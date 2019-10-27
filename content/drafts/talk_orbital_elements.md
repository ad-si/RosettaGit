+++
title = "Talk:Orbital elements"
description = ""
date = 2018-12-05T19:23:10Z
aliases = []
[extra]
id = 22049
[taxonomies]
categories = []
tags = []
+++

== seemingly incorrect results ? ==
Python results seem to be incorrect ??
--Walter Pachl 17:37, 29 October 2018 (UTC)

:The task needs concrete examples to make this sort of issue clear. --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 17:43, 29 October 2018 (UTC)

::Pls look at the one and only input used by all other samples. I found this when I had to see that my results were wrong --Walter Pachl 17:48, 29 October 2018 (UTC)

:::Should I believe that they are correct? I have seen incorrect work replicated in other tasks here (where people trust that the first posted result is correct). I have not worked through the problem statement myself. --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 18:01, 29 October 2018 (UTC)

::::Well, most (all) others prodce identical results. --Walter Pachl 18:25, 29 October 2018 (UTC)

Could you pls show 'your' values of

```txt

 c = math.cos(trueAnomaly)
 s = math.sin(trueAnomaly)
 fact = math.sqrt(2.0 / r - 1.0 / semimajorAxis)

```

? this could show where we diverge --Walter Pachl 20:30, 29 October 2018 (UTC)


###  might be a task definition issue 


It's difficult for me to say for sure (because I am a novice in this area, and because r in <big> r = L/(1 + e cos(angle)) </big> is not defined), but it's currently [http://spiff.rit.edu/classes/phys440/lectures/ellipse/ellipse.html looking] to me like the angle there should be the ''eccentric anomaly'' instead of the ''true anomaly''. --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 19:22, 5 December 2018 (UTC)

== REXX (version 2) results match Perl 6's ==
I took the orbital elements from the '''Perl 6''' example/entry and added them to the '''REXX''' (version 2) example/entry and the results agree,   so we now have multiple concurrences of two different data examples.     -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 21:51, 29 October 2018 (UTC)

:No surprise: same for ooRexx --Walter Pachl 07:59, 30 October 2018 (UTC)

== Probably time to clean up this task? ==

As it currently stands, this task is a glaring example of how *not* to define a task.

For example, we have a constraint equation involving <big>r</big>, but nothing about what that r represents.

For example, we've a physics system, but very little about what units we're using.

For example, the problem description involves euler angles (longitude of the ascending node, the inclination and the argument of periapsis) but does not say which of the various euler angle conventions we're using (one resource suggests the Z1 X2 Z3 convention).

For example, when researching orbital elements, it becomes apparent that there's a variety of different kinds of orbital elements for different kinds of orbital bodies. It looks like this particular set is intended to represent the motion of an orbiting planet (which is not entirely unreasonable), and we're dealing with different representations of an elliptical orbit. But ... this should be obvious from the description rather than something which falls out of researching notation conventions.

Anyways .. basically, the implementer is being asked to research these ambiguities (with answers which can vary depending on how that research is conducted). It's almost like this was somebody trying to use rosettacode to fish for answers for a class assignment (though of course, other motivations are entirely possible).

Still... we have the seed of a good task here, so it would be good to nail down the details. But that might mean a major task description rewrite.

That said, I've not implemented this myself, I'm putting a little of my free time into an implementation (and I'm currently brushing up on Kepler's work to see if that would get me to where I need to be -- to implement something which fits the task description, to adequately judge the accuracy of the existing implementations, and to possibly come up with other example test cases and verify whether I'm doing them right).

Anyone with deep familiarity with orbital mechanics, feel free to speak up and fill in the missing pieces?  I might get the right answers myself, but I'm not there yet. --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 17:23, 28 November 2018 (UTC)
