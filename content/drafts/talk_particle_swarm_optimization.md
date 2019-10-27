+++
title = "Talk:Particle Swarm Optimization"
description = ""
date = 2015-08-13T19:33:26Z
aliases = []
[extra]
id = 19449
[taxonomies]
categories = []
tags = []
+++

This task currently needs more detail - probably a specific example (or a few specific examples).  In other words, probably some data that needs to be used, and an unambiguous description of the desired result.

Using jargon by itself is too vague since you can wind up with different implementations solving different problems, which makes it difficult for users new to a language to learn from the examples here.

Sometimes we also need details about the algorithm, or even pseudocode, but of course between mathematical equivalence and differences in the underlying languages, that should only be a suggestion.

See also [[Rosetta_Code:Add_a_Task]]. Especially the middle sections: Task Focus, Basic Information, Example Code.

And, thanks for the attention! We need good problems here. --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 10:51, 31 July 2015 (UTC)

: Ok, this is getting a bit better, but in its current state, we will not get comparable implementations for different languages. Specifically, the task suggests using "the 2d Rosenbrock function" - but that's really a family of functions as <code>a</code> and <code>b</code> are constants (at least in the wikipedia description of this function). Worse, the current only implementation doesn't use any thing from that family but instead uses the McCormick function.

: That by itself needn't be an issue, but the current example implementation does not provide a clean distinction between the particle swarm implementation and the function they are working against.

: So... ok, maybe any function from wikipedia's [[wp:Test_functions_for_optimization|Test functions for optimization]] should be considered valid, here. But how do we then judge an implementation for correctness?

: Judging a particle swarm implementation for correctness seems like a serious problem. Basically you start with a set of random numbers and then have them perform a not-quite-random walk within the space defined by some arbitrary function. And, on top of that, "particle swarm optimization" itself is not a single algoritm but a bunch of related algorithms (for example, depending on how the concept of "best known position" is implemented - what's the scope of "best known" for example).

: In other words, currently the task suggests that each implementation should be different. And, since we're working with random numbers, we should expect different results from two runs of the same program. But that won't give us comparable implementations.

: So maybe the task should specify three test functions to be used, and one set of initial "random numbers" to be used with each test function. And I think the task should also specify what "best known position" should be, and what the terminating condition should be, and what the swarm's velocity parameters should be (what contribution to velocity from previous velocity, what contribution to velocity from the random term based on the distance from the particle's initial position, what contribution to velocity, what contribution to velocity from the random term based on the distance from the particle's best known position).

: With those nailed down, I think that implementations and results should be comparable. --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 19:02, 1 August 2015 (UTC)

The Wikipedia article describes the basic algorithm fairly specifically and also mentions the existence of variants (which the task description fails to mention).  Note that the current examples now both conform to the basic algorithm, and also exercise it with the same objective function and search bounds, and have comparable results.  Should the task description suggest, or require, an algorithm version, objective function, and parameters?  It appears that all other issues have been addressed.  --[[User:JimTheriot|JimTheriot]] ([[User talk:JimTheriot|talk]]) 23:30, 3 August 2015 (UTC)

: I think it should specify those thing. Ideally, two different cases. --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 14:19, 4 August 2015 (UTC)
This is a draft task "for reasons that should be found in its talk page" - for clarity, can someone summarize the remaining obstacles? --[[User:JimTheriot|JimTheriot]] ([[User talk:JimTheriot|talk]]) 18:57, 13 August 2015 (UTC)

: For the generic draft issues, there's the [[Rosetta_Code:Add_a_Task#Draft_vs_non-draft|draft vs non-draft]] part of the "Add a Task" page. Also, I think it has been typical practice to leave a task in draft status until it has been implemented in four different languages. (Though there have been exceptions to that approach also - especially when people have been unaware of the draft issues.) --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 19:33, 13 August 2015 (UTC)
