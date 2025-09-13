+++
title = "Loglan82"
description = ""
date = 2016-02-13T06:14:17Z
aliases = []
[extra]
id = 20039
[taxonomies]
categories = []
tags = []
+++


'''Loglan'82''' is a programming language conceived for object and distributed programming.

It has many features that make Loglan'82 a tool surpassing other programming languages:
* It enjoys a unique safe and efficient system of managing objects, see [Safe deallocation](https://rosettacode.org/wiki/Safe_deallocation),
* It offers modules of classes ('''class'''), moreover it allows to declare modules of cooperating objects i.e. '''coroutines''' and modules of threads ('''process'''). One can create not only objects of classes, but also objects of coroutines with their ''fibres'' and objects of processes, aka agents with their ''threads''.
* Virtual machines of Loglan may connect (through internet) in a virtual, multiprocesorr computer.This allows to distribute a computation of a program between processors in a controlled way.
* Objects of processes (agents) can be alocated on different nodes of a network of virtual machines or on the same machine. It means that Loglan uses one model for concurrent as well as for distributed computations. (Less learning).
* Loglan'82 offers an original, fully object protocol of communication and synchronization, so called ''alien call'' of methods of an agent.
* Each object of a process, i.e. an agent may create its own system of coroutine objects and manage it,

[https://sourceforge.net/projects/loglan82/ Loglan82 on Sourceforge]
