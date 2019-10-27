+++
title = "Process"
description = ""
date = 2009-08-21T19:34:03Z
aliases = []
[extra]
id = 2570
[taxonomies]
categories = []
tags = []
+++

[[Category:Encyclopedia]]A '''process''' is an instance of a computer program. This is not to be confused with a [[thread]], which is a section of code which runs concurrently with other sections of code, or a program, which is a list of instructions. Threads are technically part of a process. For instance, in a [[Java]] [[GUI]] program, many GUI actions are handled on a separate thread from the rest of the program, but it all still runs in a single process.

Both processes and threads are examples of [[task]]s, an entity participating in resource sharing, and in particular, in sharing the central processor unit. I.e. both are subjects of scheduling. The main difference between a process and thread is that threads use a trusted model of resource sharing, while processes share the resources in a way to prevent mutual disturbance. For this reason, in a modern [[OS]] the resources allocated to a process are usually collected. The resources allocated to a thread are typically unprotected and collected only together with the process owning the thread.
