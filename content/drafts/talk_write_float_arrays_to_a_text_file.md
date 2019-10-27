+++
title = "Talk:Write float arrays to a text file"
description = ""
date = 2016-10-17T20:48:30Z
aliases = []
[extra]
id = 2437
[taxonomies]
categories = []
tags = []
+++

== Precision vs decimal places ==
The majority of the code samples seem to assume precision means number of digits after the decimal point. The task description isn't super clear about this, but it becomes clear when you look at the example output in the task description. Should "incorrect" tags be added to most of the examples?

== Redundant? ==
Is this already covered in [[File I/O]]? It doesn't seem to add anything extra. --[[User:Mwn3d|Mwn3d]] 23:10, 23 December 2007 (MST)
: Agreed.  Unless serialization deserves its own category, this task is somewhat superfluous.  Even if serialization did get its own category, there are better ways to tackle the subject thoroughly. (And, admittedly, I wouldn't mind seeing the subject tackled, with cross-platform file compatibility being just one interesting aspect.) --[[User:Short Circuit|Short Circuit]] 01:29, 24 December 2007 (MST)
# This task was created due to the '''Modularization''' section in [[Help:Adding a new programming task]]. [[Write float arrays to a text file]] is a subtask of [[Measure relative performance of sorting algorithms implementations]]. [[Measure relative performance of sorting algorithms implementations]] depends on ''writedat()'', therefore I could move it there if the subtask ever be perished.
# In addition, a blue print of this subtask is an output of numbers with a given number of significant digits, and an output in a format suitable for external plotting program. It is interesting, how it could be accomplished in languages without ''printf''-like expressions.
# Concerning redundancy of this task, It is an eternal dichotomy between orthogonal (minimum overlap) and human (rich, superfluous) intefaces in programming, for example, compare interfaces of Python's list object with the Ruby's Array. The former has less then ten non-special methods, at the same time the latter has much more then that. In general, It's a matter of taste or religion which way to choose. Wiki may benefit from both approaches simultaneously. [[User:Geka Sua|Geka Sua]] 13:03, 24 December 2007 (MST)

== J code March 13th ==

# The J code posted on March 13 does not limit the precision of the formatted numbers as required by the specifications. (I also wish to note that I don't see how the example values provided for this task test the requirement for precision-limitation of the 'x' array. Do they?) --[[User:TBH|TBH]] 11:50, 13 March 2008 (MDT)
# Yes it does.  That's the purpose of the <code>0 j.</code> bit.  EG:
    ": 1.2345678
 1.23457
    0j2 ": 1.2345678
 1.23
