+++
title = "Talk:Program termination"
description = ""
date = 2010-02-06T14:11:47Z
aliases = []
[extra]
id = 2962
[taxonomies]
categories = []
tags = []
+++

The task needs a clarification in respect to the semantics of the termination, namely whether a cleanup is performed.

Consider a program that creates objects with some finalization defined. Shall they be finalized upon termination? As an example take an object encapsulating a database connection. Object's finalization commits the changes in the database. If finalization is not done, then changes will be lost. Is it the semantics of termination? Note, that even upon an exception propagation, finalization is still made. Anyway, if the answer were yes, it has to stop at once, leaving '''everything''' as is, then that would be inconsistent with the behavior of most implementations. Because OS and the language run-time still execute some epilogue, collecting resources, closing files etc. So what is the semantics? -- [[User:Dmitry-kazakov]]

: Since any given system isn't necessarily capable of a specific behavior in this area, I would suggest not requiring anything, but having the task ask for an explanation of the finalization behavior of the termination operations shown. If someone else likes this idea I'll do it. --[[User:Kevin Reid|Kevin Reid]] 12:00, 3 August 2008 (UTC)

::Sounds reasonable to me. --[[User:Dmitry-kazakov|Dmitry-kazakov]] 12:14, 3 August 2008 (UTC)
