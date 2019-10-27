+++
title = "Talk:N-body problem"
description = ""
date = 2016-11-16T14:53:58Z
aliases = []
[extra]
id = 17524
[taxonomies]
categories = []
tags = []
+++

== Cool Story, Bro! ==

What we supposed to be doing here? What's the task itself? –[[User:Dkf|Donal Fellows]] ([[User talk:Dkf|talk]]) 14:43, 20 April 2014 (UTC)
: I've added a clear task here; simulate a 3 body gravitation system for 20 time-steps. It might not be perfect, but it's a ''definite'' thing for programmers to program, rather than just wallowing in mathematics. –[[User:Dkf|Donal Fellows]] ([[User talk:Dkf|talk]]) 07:14, 26 May 2014 (UTC)

In how many dimensions should we solve this?  All masses at a point with no initial motion?  Linear?  Planar?  Three dimensional?  --LambertDW 01:53, 8 June 2014 (UTC)

== Caution ==

: "Beware that because of the inherently chaotic nature of the n-body problem, two different but correct implementations may diverge after a sufficiently long sequence of <math>\delta T</math> steps." -- [http://www.csee.umbc.edu/~motteler/teaching/courses/parallel_prog/96b/nbody/nbody1.html] --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 00:33, 16 April 2014 (UTC)

== tcl implementation ==

I animated [projections of the data from] the tcl implementation, and I noticed that the second and third bodies practically bounce near the end. You can see this by comparing the values in the last two rows with the previous row. What's odd is that the first body reacts similarly at roughly the same point in space, but two time increments earlier than the other two.

Anyways, my concern is that delta-T is probably way too big if things are bouncing so sharply.

But this raises another, more important issue: how can we be sure our results are correct (or even "close to plausible") for a problem like this? --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 07:48, 26 May 2014 (UTC)

== Let's agree on a specific problem to solve ==

While the code we write may be general purpose, let's come up with some initial positions and velocities and masses of objects to solve. 

Since this is just for a gravitational interaction simulation, collisions should not be worried about, right?

I propose we leave the method of numerical integration up to the user as long as their error is within a given amount.

== a simple case with an explicit solution ==
I think two separate problems are being discussed:  an abstraction in which point masses interact gravitationally and non-relativistically, and a more concrete problem considering actual interactions between bodies in earth's solar system.  I recommend that this task be split into two: 'n-body problem' which is the abstract one, and 'orbital mechanics' which is the concrete one.  My remaining comments are with regard to the abstract one. 

I recommend that the requirement is a solution to the n-body problem for an arbitrary number of bodies.  The verification case should be the simplest case for which there is an explicit solution.  The J solution is a candidate, a radially symmetrical system of three bodies orbiting a central point at constant velocity.  Assuming that the agreed verification case is some sort of system with stable orbits, the time span should be at least one revolution of the system.  Additional cases might be presented, at the discretion of the programmer.
