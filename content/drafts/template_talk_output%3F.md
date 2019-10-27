+++
title = "Template talk:Output?"
description = ""
date = 2011-08-17T00:15:29Z
aliases = []
[extra]
id = 10320
[taxonomies]
categories = []
tags = []
+++

== Make this look less severe ==

We recently had [http://rosettacode.org/mw/index.php?title=Arbitrary-precision_integers_(included)&curid=6036&diff=117617&oldid=117591&rcid=118896 some confusion] over the meaning of this template. I think it partly comes from the fact that it's a gigantic red box. It makes it look like it's saying the code is incorrect (even though we have {{tmpl|incorrect}} for that). Maybe we could change the color of the box to gray? The wording may need to be reworked too. Maybe it should say something like "This code example shows the desired output when run, but the output is not shown '''on this page'''. Please run the code, copy the output, and paste it with the example."? --[[User:Mwn3d|Mwn3d]] 15:52, 16 August 2011 (UTC)
:Hi, I see what you are getting at and changing to a less severe colour seems good, but even when I try and [http://rosettacode.org/wiki/User_talk:Arm#Output explain] what is being requested I usually end up with a lot of text. I find it hard to be both explicit and short on this particular task. (But others might do better). :-)
 --[[User:Paddy3118|Paddy3118]] 16:05, 16 August 2011 (UTC)
::I tried out some new stuff in {{tmpl|Output?/Beta}}. See if it's OK. There may be a problem with using "lightgray" in some browsers. I can't remember if that color name was supported everywhere. --[[User:Mwn3d|Mwn3d]] 17:10, 16 August 2011 (UTC)

:::I also use the template when the code to generate missing output ''might'' also be missing, so your current alternate text needs a tweak. --[[User:Paddy3118|Paddy3118]] 17:32, 16 August 2011 (UTC)
::::I tried again. It's getting kind of bulky again. I wish I could think of a more concise way to put it. --[[User:Mwn3d|Mwn3d]] 17:47, 16 August 2011 (UTC)
:::::I just gave it a go too. Feel free to revert the change if you don't like it. --[[User:Arm|Arm]] 15:58 16/08/2011 (GMT-3)

== Don't do this ==

Please, please, don't make showing output normative. I have noticed a trend towards this on RC, and I dislike it. In particular, the "beta" version of this template says "It is good practice to show the output of a program whenever there is output.". I disagree that this is good practice.

In particular, I consider it useless clutter to show output per-program on tasks ''where the output is specified so precisely as to be identical across all correct programs'' — the output need only be shown once, in the task description.

In other cases, I wish that it be left up to the discretion of the task, and there not be a RC-wide policy of including output. —[[User:Kevin Reid|Kevin Reid]] 21:17, 16 August 2011 (UTC)

:I don't believe it should be the norm for RC either, but I do think it has advantages in catching wrong implementations ''especially'' where the task goal is to give short, precise output such as in [[Range extraction]] amongst others, where, with the best will in the world, it can be difficult to see that last t to be crossed in ones implementation. --[[User:Paddy3118|Paddy3118]] 21:34, 16 August 2011 (UTC)
::I concur with Kevin that it should be left up to the task. If a language example isn't up to snuff, it can be left to someone with the opportunity to run the code to detect this. Otherwise, there's separate maintenance between code and output, and that ''will'' get desynchronized. An additional reason to not require output comes from a more technical problem...sufficiently large pages don't get emitted by the MediaWiki engine because PHP hits a memory limit. That's why MW warns when editing a page which has become very large. Similarly, large pages exacerbate an issue I've overhead numerous complaints about elsewhere; when someone is looking for something in particular, they don't want to see anything but the two or three languages they're interested in. --[[User:Short Circuit|Michael Mol]] 22:54, 16 August 2011 (UTC)
:I really like seeing output in the tasks. I don't think there are many tasks that define a character-by-character output (the string processing tasks, yes, but I think there are more mathematical ones that don't define it so precisely), so you get subtleties in output. There was at least one task where showing the output caused some confusion with the language and resulted in a language change (it was how it displayed complex numbers). When the output is defined down to the character, I understand not showing it, but otherwise it's nice to see it to verify the example and to see how the language displays things (like those weird languages that use underscores for negative signs). If there are technical reasons for not showing it, maybe there could be a technical solution (how large does that page have to be?). --[[User:Mwn3d|Mwn3d]] 00:15, 17 August 2011 (UTC)
