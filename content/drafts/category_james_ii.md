+++
title = "Category:JAMES II"
description = ""
date = 2010-07-13T14:09:35Z
aliases = []
[extra]
id = 7746
[taxonomies]
categories = []
tags = []
+++

{{library}}{{implementation|JAMES II/Rule-based Cellular Automata}}
JAMES II ('''Ja'''va-based '''M'''ultipurpose '''E'''nvironment for '''S'''imulation '''II''') is a generic modeling and simulations framework written in the [[:Category:Java|Java]] programming language. It is currently still under development at the University of Rostock, although an early alpha version with some formalisms and simulators exists for download and review<ref>[http://wwwmosi.informatik.uni-rostock.de/mosi/plonesoftwarecenter.2006-03-21.4262636143/pscproject.2006-03-21.6122697558/releases Current software releases]</ref>.

It is not tied to a particular modeling formalism or simulation algorithm, allowing those to be easily exchanged and extended via plug-ins. JAMES II consists of four main parts:
* the graphical user interface
* the experiment layer (supports diverse experiment types, e.g. optimization experiments)
* the modeling layer (supports diverse modeling formalisms)
* the execution layer (supports the simulation of various formalisms; there may be more than one simulation algorithm per formalism)

While the specifics of a formalism and a simulator aren't rigidly specified, there exists the distinction between a ''model'', i.e. a specific set of rules for a given formalism and ''parameters'', which are used to parametrize a certain model. Parameters can include a starting configuration among other things but this very much depends on the formalism and the model.

Currently the publicly available version of JAMES II includes the core framework itself including the GUI, a selection of pseudo-random number generators, random distributions as well as formalisms and simulators for (among others) [[:wp:Cellular automaton|cellular automata]], [[:wp:DEVS|DEVS]], [[:wp:Pi calculus|Pi calculus]] and Stochastic Pi.

== References ==
<references/>

== External links ==
* [http://jamesii.org Main homepage of the framework]

{{stub}}
