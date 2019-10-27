+++
title = "Talk:Paraffins"
description = ""
date = 2019-01-12T22:57:57Z
aliases = []
[extra]
id = 10977
[taxonomies]
categories = []
tags = []
+++

== Algorithm? ==

would it be possible to describe an algorithm for the solution in a few paragraphs?

and explain why there is only 1 paraffin for 4 or less carbon atoms? and why there are 2 for 5 and a few more, so that those of us who don't know organic chemistry can get some understanding of how the results are created?--[[User:EMBee|eMBee]] 16:53, 30 November 2011 (UTC)
:Even just some general rules about how the atoms are allowed to be arranged would help. I know that carbon atoms can have 4 bonds (usually...I remember a Christmas carol from my high school chemistry class called "Rudolph the 5-bond Carbon"). It also looks like for this class of molecules that cycles aren't allowed? --[[User:Mwn3d|Mwn3d]] 17:24, 30 November 2011 (UTC)
:4 to 6 carbon configuations: (if any "c" doesn't have 4 bonds already, imagine it's connected to invisable hydrogens).  The basic algorithm would be some kind of recursive tree generation, probably with memoization for large numbers.<lang>4:
c-c-c    c-c-c-c
  |
  c

5:
c-c-c-c-c   c-c-c-c     c
              |         |
              c       c-c-c
                        |
                        c

6:
c-c-c-c-c-c   c-c-c-c-c   c-c-c-c-c   c-c-c-c       c
                |             |         | |         |
                c             c         c c       c-c-c-c
                                                    |
                                                    c
```
--[[User:Ledrug|Ledrug]] 21:02, 30 November 2011 (UTC)

::Conceptually speaking, we would be looking for graph equivalence here.  Although the graph can be represented as a tree, "equivalence" allows any node in the tree to be the root, and the branches from any node can be in any order.  --[[User:Rdm|Rdm]] 20:59, 5 December 2011 (UTC)
:::I think deciding to go by graph equivalence answers the isomer question.  Stereo isomers have equivalent graphs and so would not be counted.  This is what A000602 does.  It also answers the question of physically impossible isomers that start at C16.  We would count these because we can produce a graph for them, even though they cannot physically exist. &mdash;[[User:Sonia|Sonia]] 21:31, 5 December 2011 (UTC)
:::: Wait, what do you mean "cannot physically exist?" --[[User:Ledrug|Ledrug]] 01:27, 6 December 2011 (UTC)
::::: http://www-jmg.ch.cam.ac.uk/data/isomercount/
::::: Seems that when the molecules get highly branched, there's not enough room for all the parts to squeeze in close enough to bond.  I don't do this kind of work myself, but these guys seem to have put some thought into it. &mdash;[[User:Sonia|Sonia]] 02:20, 6 December 2011 (UTC)
:::::: Ok. Well they were not describing it (either 16 or 17) as impossible to form, it's not a space issue. What happens is that due to the structure and consequently how electrons should distribute to obtain lowest energy state, if you put such a molecule under room temperature, thermal energy alone will be enough to break one of the carbon bonds and split the molecule into two.  They can still form, but you can't hold on to a beaker of such material for any meaningful amount of time without refrigeration (they didn't give numbers, so it's hard to tell if that C17 would be stable at absolute zero; C16 would.) --[[User:Ledrug|Ledrug]] 02:50, 6 December 2011 (UTC)

=== stereo-isomers ===
Someone (IP:79.54.58.148) has reverted part of my edit because they incorrectly think paraffins cannot have stereo-isomers. In fact they can, for example C(H)(CH<sub>3</sub>)(C<sub>2</sub>H<sub>5</sub>)(C<sub>3</sub>H<sub>7</sub>) (better known as 3-methylhexane: CH<sub>3</sub>CH<sub>2</sub>CH(CH<sub>3</sub>)CH<sub>2</sub>CH<sub>2</sub>CH<sub>3</sub>) is [http://www.wwnorton.com/college/chemistry/orgo3/ch4/2_methylhexane.htm chiral] and so exists in stereo-isomeric form. If you disagree please discuss it here before I change the page back. [[User:TobyK|TobyK]] 00:09, 1 December 2011 (UTC)
: <s>I think Anon is mostly right: single carbon-carbon bond can (almost) freely rotate around its axis, so stereoisomers can be rotated at each "joint" to transform from one into the other.  The only conceivable case where this can't be done is when the carbon tree branches in a very complicated way that different branches spatially interlock each other, so the rotations are restricted--though I don't know if that can really happen, and I'd imagine you'll need at least 20ish carbons in the molecule for that.</s> No wait, that was wrong. Another case is if a carbon atom is bonded to 4 different structures, then there will be a chirality difference that can't be fixed by rotations. --[[User:Ledrug|Ledrug]] 01:41, 1 December 2011 (UTC)
