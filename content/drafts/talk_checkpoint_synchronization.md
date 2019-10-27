+++
title = "Talk:Checkpoint synchronization"
description = ""
date = 2011-02-19T06:09:29Z
aliases = []
[extra]
id = 7915
[taxonomies]
categories = []
tags = []
+++

I'm trying to understand the problem so I can adapt it into my language. Do I correctly understand that there is no point at which the tasks communicate with each other ''during'' the checkpoint? The ''only'' property produced by this concurrency mechanism is that no worker shall begin work iteration #n before everyone has finished #(n-1)? —[[User:Kevin Reid|Kevin Reid]] 22:59, 10 August 2010 (UTC)
: That's what it looks like to me. There are scenarios where that's the normal model (most notably GPGPU programming; the checkpoint is needed for hardware instruction resync.). In languages where data structure syncronization is implicit, I don't think a communication phase is necessary. Otherwise, (if I were writing in C++, for example), the communication phase might get used for work item distribution and condition checks. --[[User:Short Circuit|Michael Mol]] 00:37, 11 August 2010 (UTC)
::Right, the intent was to show the problem in its distilled form. It is comparable with plain events where there is no data exchange too, just a raw signals. event + data = message, it is already a different object and problem. But I agree with Kevin that another task with data exchange might be interesting too. I think it could be a bit more concrete problem. We could take some known parallel algorithm like block matrix multiplication or FFT. --[[User:Dmitry-kazakov|Dmitry-kazakov]] 06:51, 11 August 2010 (UTC)
::: FFT would be very interesting, because it's a very popular signal processing transform, and there are huge number of ways to improve performance via platform instruction sets, acceleration libraries and parallelization. It's very likely it will lead to multiple examples per language, but as long as there's at least one naive example per language (for logic clarity), I think that's probably a good thing.
::: That said, I'd be careful about appropriating FFT specifically to demonstrate "[[Checkpoint synchronization]] with a communication phase"...for something like that, a more appropriate task might be something like map/reduce with an otherwise meaningless function call on each data block. (I wanted to say "generic function call", but that might force a distinction between languages that have (or can fake) first-class functions, and languages that don't/can't.)  I like the FFT idea, but I don't care to have distinct tasks for a FFT map/reduce-like task, and FFT in general. Ditto for matrix block multiplication, for the same reasons; it's not helpful for site organization to demonstrate the same thing (such as a transform or algorithm) in multiple places. --[[User:Short Circuit|Michael Mol]] 10:46, 11 August 2010 (UTC)
: The principle is that there is a checkpoint group (which a thread may ask to join and leave) and on synchronization, nobody completes the checkpoint synch operation until all members of the group complete it. It's use might be if you've got a bunch of threads doing some kind of physical simulation, with a checkpoint barrier at the end of each simulation time step. –[[User:Dkf|Donal Fellows]] 13:53, 11 August 2010 (UTC)

Is 'checkpoint synchronization' the best name for this task? On wikipedia, there's an article ( http://en.wikipedia.org/wiki/Barrier_(computer_science)#Threads_synchronization_primitive ) which seems to fit the description of this task.

== Ruby code does wrong task? ==

I added a Ruby example. Like the Perl code, the Ruby code uses blocking IO on sockets to synchronize the workers. I am not sure if my Ruby code does the correct task. When the workers finish a mechanism, they do not immediately start to build the next mechanism. Instead, all workers wait until the main thread orders the next mechanism. While they wait, the main thread can add or remove workers. Thus, workers can only join or leave the workshop ''between mechanisms'', while all workers are idle. --[[User:Kernigh|Kernigh]] 06:09, 19 February 2011 (UTC)
