+++
title = "Talk:Horizontal sundial calculations"
description = ""
date = 2016-12-15T10:36:51Z
aliases = []
[extra]
id = 7590
[taxonomies]
categories = []
tags = []
+++

Hi,
How about adding the actual calculation being done to the Task description, or pointing it out if it is in the wikipedia article? Thanks. --[[User:Paddy3118|Paddy3118]] 07:05, 24 June 2010 (UTC)



It surprises me that every submission (except BBC BASIC) outputs an obviously incorrect value for 6pm (18:00). --Richard Russell 14:47, 16 May 2011 (UTC)

:This point of view seems a bit provocative, perhaps even trollish?  I see one entry that has labeled "11 am" as "1 am" but most of the others use an "offset from noon" notation, which has some educational merit.]  --[[User:Rdm|Rdm]] 17:29, 16 May 2011 (UTC)
::I think he's talking about the hour line angle values. BBC BASIC shows "-95.775" while most other langauges show "84.225". I can't quite tell if these are equivalent (I need a picture), but their absolute values add up to 180 which makes me think they might just be the same angle from different directions. --[[User:Mwn3d|Mwn3d]] 17:50, 16 May 2011 (UTC)

::: Oh, yes, the principal value issue.  Hmm... --[[User:Rdm|Rdm]] 18:12, 16 May 2011 (UTC)

::: Arguably, the existing implementations satisfy the task requirement: enough information is given for a human to produce an accurate sundial.  However, we are computer programmers and we know that that a computer consuming these results would do the wrong thing.  Probably the task itself should be more specific about what is required.  --[[User:Rdm|Rdm]] 18:49, 16 May 2011 (UTC)
::::I see you added a fix for J. What is the math behind this fix? In general, how do you avoid this problem? --[[User:Mwn3d|Mwn3d]] 18:51, 16 May 2011 (UTC)

:::::In essence, I use [[wp:Atan2|atan2]] with arguments  X= cos (sun hour angle), Y= sin (sun hour angle)* sin (latitude).  --[[User:Rdm|Rdm]] 19:15, 16 May 2011 (UTC)


### Equation of Time

Because the earth's orbit is not circular, the length of a day (solar noon to noon) varies and the differences accumulate and then diminish over the cycle of an orbit, calculable via the Equation of Time. The difference is up to twenty minutes each way. Thus, a sundial will tell the correct time only at the equinoxes (when the sun is directly above the equator) and superior sundials have a graph showing the necessary offset against the month of the year. I mention this only because of "if such a sundial is to tell the ''correct time''" and of course because I'm a nitpicker.

An interesting variant is the digital sundial, described in a Scientific American article. This is a stand with a mostly opaque block through which holes are drilled (or, clear paths exist) in a cunning pattern such that within the shadow of the block appears sunlight shining through a clear aperture so as to read say 12:00 on the table top. A minute later (the sun having moved) the reading is 12:01, and so on. Obviously, the block has to be correctly oriented and the pattern of opacity depends on the angle to the sun. I have contemplated a calculation that would distribute black splotches across a page to be printed on clear plastic, many such pages stacked with careful alignment. In other words, start with a black block, then project a clear 12:00 through its centre in the orientation of the sun at 12:00, and so forth for the other times and their orientations. Hummm... [[User:Dinosaur|Dinosaur]] ([[User talk:Dinosaur|talk]]) 10:36, 15 December 2016 (UTC)
