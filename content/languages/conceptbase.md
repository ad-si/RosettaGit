+++
title = "ConceptBase"
description = ""
date = 2013-09-18T12:16:41Z
aliases = []
[extra]
id = 8145
[taxonomies]
categories = []
tags = []
+++
[http://conceptbase.sourceforge.net/ ConceptBase]

ConceptBase implements Datalog-neg, i.e. Datalog with negation. The computational model mimics Prolog's SLDNF but
employs so-called tabling for all derived predicates. Tabling is a technique to maintain the extension of finite
predicates. 

As such Datalog-neg is not a full-fledged programming language. It always terminates, so it cannot compute certain
functions. ConceptBase extends Datalog in the following ways:

# Arithmetic on integer and floating point numbers: integer arithmetic is already sufficient to make the resulting language undecidable
# Recursive function definitions: this allows to define functions like the Ackermann function
# Active rules: other than Datalog rules, active rules change the state of the database (or memory)

The result is a Turing-complete language, which is rooted in Datalog but which no longer belongs to the Datalog family.
