+++
title = "Talk:Langton's ant"
description = ""
date = 2012-03-11T23:14:53Z
aliases = []
[extra]
id = 10742
[taxonomies]
categories = []
tags = []
+++

==draft task==
please review the task and remove the draft category if appropriate --[[User:AlexLehm|AlexLehm]] 14:11, 30 October 2011 (UTC)

:I made a few changes based on what I think you were saying.  If you do not like them, please add your own clarifications?  --[[User:Rdm|Rdm]] 10:50, 2 November 2011 (UTC)

== Middle? ==

It's a 100x100 field, there is no middle cell.  I suggest odd-numbered sides (99x99 or 101x101) to remove this ambiguity.

[[User:CRGreathouse|CRGreathouse]] 01:56, 21 November 2011 (UTC)
: It says "near the center". Just pick something close by. It doesn't matter too much since the area is much larger than what it takes to get to the repetitive part. --[[User:Mwn3d|Mwn3d]] 01:59, 21 November 2011 (UTC)

==Chirality?==
Perhaps this is just a nit, but a large minority of the solutions appear to be turning right on black and left on white, resulting in a mirror image of the specced result.  You can tell by looking at which direction the "pinwheel" goes... --[[User:TimToady|TimToady]] 22:51, 9 March 2012 (UTC)

: The task never specifies the initial color of the field. Can one produce a mirror image by swapping the initial color. --[[User:Kernigh|Kernigh]] 01:20, 10 March 2012 (UTC)
:: Er, yes it does: white is specified for the initial ground color in the first sentence.  Upon reflection (no pun intended, except maybe retroactively), there are several other ways that the image can come out mirrored.  In the case of Ada, we see that the color is tested after inversion rather than before inversion, so the ant turns the wrong way.  In other cases, it appears that the x/y coordinates are assumed to be on a standard mathematical graph, but then then one of the axes is flipped upon output by traversing one of the dimensions in the opposite order.  (The Perl 6 solution assumes the whole graph is printed on its side, but that's merely a rotation around the origin, not a mirror flip.) It's also possible some of the examples are merely screwing up what is true and false. --[[User:TimToady|TimToady]] 01:34, 10 March 2012 (UTC)

::: I'd argue that a rotation is still correct, but that the Ada solution is ''technically'' wrong (because the implementation uses the wrong information to act). Mind you, mere renaming of the variables should not be enough to invalidate a solution, so solutions which are of the wrong handedness because of exchanging x/y are also correct (except for the display code, which is not the focus of the task). –[[User:Dkf|Donal Fellows]] 07:08, 10 March 2012 (UTC)

::::Well, it could be argued that right and left have no meaning without an observer outside the plane, which makes the display code rather more important. In which case you should counter-argue that the "wrong" chirality is merely observing the graph from underneath. But I'm not trying to be a troublemaker.  It's just that I was trained in chemistry, where the wrong stereoisomer can have results like Thalidamide.  Or if you're lucky, you just end up up with the difference between spearmint flavor and caraway flavor, which are stereoisomers.  So maybe I'm just overly sensitive on the subject.  Chemists don't deal in abstractions, so they aren't generally taught to say "That wasn't the focus of my task."  <tt>:-)</tt>

: Nit or not, updated Perl 5 code to correct the chirality.  Output now matches Wikipedia image.  Also made similar mod to Perl 6 and exchanged x/y in output code so they have the more traditional interpretation while maintaining correct chirality.  Thanks, Larry.  --[[User:Markjreed|Markjreed]] 23:14, 11 March 2012 (UTC)
