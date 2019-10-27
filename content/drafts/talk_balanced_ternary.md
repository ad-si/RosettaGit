+++
title = "Talk:Balanced ternary"
description = ""
date = 2014-09-16T15:22:37Z
aliases = []
[extra]
id = 10749
[taxonomies]
categories = []
tags = []
+++

=== re: draft task, because NevilleDNZ liked it so much ===
Many ThanX ;-) I confess that I might be a [http://xkcd.com/356/ Nerd] at heart and as I find it hard resist solving an especially annoying puzzle. 

But now I have a hammer, where is the nail? An actual "real world" ''test case'' would be really sweet!  Ironically I have yet to think of one.  Ideas are invited...

e.g. from Wikipedia's [[wp:Balanced ternary|Balanced ternary]] page: ''For example, a classical "2-pan" [[wp:Weighing scale#Balance|balance]], with one weight for each power of 3, can weigh relatively heavy objects accurately with a small number of weights, by moving weights between the two pans and the table. For example, with weights for each power of 3 through 81, a 60-gram object (60<sub>10</sub> = 1<u>1</u>1<u>1</u>0) will be balanced perfectly with an 81 gram weight in the other pan, the 27 gram weight in its own pan, the 9 gram weight in the other pan, the 3 gram weight in its own pan, and the 1 gram weight set aside. This is an optimal solution in terms of the number of weights needed to weigh any object''.

[[User:NevilleDNZ|NevilleDNZ]] 04:44, 1 November 2011 (UTC)


###  Test case discussion 

'''Floating point test case:''' With balanced ternaries ''a'' from string "+-0++0+.+-0++0+", ''b'' from native integer -436.436, ''c'' "+-++-.+-++-":
* write out ''a'', ''b'' and ''c'' in decimal notation;
* calculate ''a'' × (''b'' − ''c''), write out the result in both ternary and decimal notations.

[[User:NevilleDNZ|NevilleDNZ]] 04:52, 1 November 2011 (UTC)
: Eh that's asking for a lot of trouble.  -436.436 decimal doesn't actually termintate in base 3. --[[User:Ledrug|Ledrug]] 04:59, 1 November 2011 (UTC)

I suspect that "+-0++0+.+-0++0+" and "+-++-.+-++-"  in decimal don't terminate either. /* In for a penny, in for a £... ;-) */ Maybe the precision could be user defined, and set at about 81 digits after the point for the test case? [[User:NevilleDNZ|NevilleDNZ]] 05:15, 1 November 2011 (UTC)

: A floating point involves too many things.  Seemingly simple stuff like "0.1" has no exact finite representation in either base 2 (IEEE) or base 3, thus the meaning of "436.436" in a task requirement is dubious to begin with: Rdm ''will'' come along and bury me with questions, so I'd rather not do that.
: We could dodge this issue by limiting digit length after decimal point, which is essentially treating them as rationals, then we'd have different problems: either we don't cancel out common factors in numerator and denominator, which is obviously unsatisfactory; or we do cancel them out, which requires division and modulo, which would be difficult and long-winded.  I don't want to make the task require more effort than necessary.  In any event, if the task proves to be so popular that people flock to it like hot cakes (I doubt it), one could always make another task to extend it to floating point numbers. --[[User:Ledrug|Ledrug]] 16:42, 1 November 2011 (UTC)
::Floating point numbers are essentially a fixed width approximation of rational numbers.  So why would a person want arbitrary precision floating point?  An answer might be to represent irrational function results, but you have to limit the precision of those results or you run out of memory before you even get started.  The simplest approach for representing irrational results would probably to use a fixed width representation for irrational numbers.  But if your problem domain demands something else, then understanding that problem domain can really help in building a reasonably implementation. --[[User:Rdm|Rdm]] 12:31, 2 November 2011 (UTC)

Ironically this algorithm is (basically) the same as the binary equivalent (and which also is yet to be added into RosettaCode).  Hence (for 2, 4, 8 & 16 bytes precision) it is generally just ''built-in'' on everybody's computer.

'''Moreover''' the basic addition and multiplication algorithm is taught to every grade school student on the planet... It is awesome to realise what complex algorithms the mind of an [http://education.more4kids.info/46/homeschooling-math-concepts-and-skills-by-age/ 8-11] year old can be taught to process with a the aid of a good teacher.

However - back to the point - I am not opposed to splitting it into 2 tasks, eg ''whole'' and ''floating point'' balance ternary.  Certainly ''whole balance ternary'' is a whole lot easier on the old belfry, and ''floating point'' would then be a corollary task.  And it would be sad not to see the more general ''floating point'' solution.

I do see a parallel with why hardware floating point took so long in arriving into a our typical PC.

[[User:NevilleDNZ|NevilleDNZ]] 20:45, 1 November 2011 (UTC)

== bop notation for balanced ternery ==

An interesting set of visual properties devolve from the use of p, o and b as -1, 0 and +1 respectively. In particular, any number can be negated by rotation about the horizontal axis. The negative of the number bop (eight, of course, as nine minus one) is pob. [[User:Snezzy|Snezzy]] ([[User talk:Snezzy|talk]]) 15:22, 16 September 2014 (UTC)
