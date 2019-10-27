+++
title = "Talk:RCRPG"
description = ""
date = 2019-06-17T16:57:31Z
aliases = []
[extra]
id = 22380
[taxonomies]
categories = []
tags = []
+++

OK, I'm going to say it.  This is a dumb game, in so many ways.  The dungeon is replete with sledges, but you have to take the one in the starting room, never have any reason to drop it, and so all the rest are completely pointless.  The "equip" command must always be used as the second command (right after "take sledge") and is pointless thereafter; the unequip command is never needed at all.  The starting and ending positions are known and you can go anywhere, with no hazards, so there is no reason to explore except to acquire ladders.  Going anywhere pointlessly involves two steps (attack <dir> followed by <dir>) in some cases, offering no challenge except to see how well you're paying attention (though you could just ''always'' use both commands and it would work fine).  Solving the game is trivial: go north and east so you are at 1,1,0, and then work your way upwards, merely detouring north (or any other direction) as far as necessary to get a ladder, and then coming back to 1,1.  After a few minutes of such pointless activity, you will be at the prize room, whereupon the game doesn't even bother to exit.

I get that it's supposed to be a tiny demo, not a real game, but surely we could come up with something just as tiny that would nonetheless have at least a ''little'' fun to it. --[[User:JoeStrout|JoeStrout]] ([[User talk:JoeStrout|talk]]) 13:58, 17 June 2019 (UTC)

:Well, those are minimal guidelines. The task is mostly to demo how to do a rough game framework with command handling and basic inventory management. If you want to make something more elaborate, feel free. --[[User:Thundergnat|Thundergnat]] ([[User talk:Thundergnat|talk]]) 16:20, 17 June 2019 (UTC)

:Far harder to write than play, no argument there. In the phix one, I made the sledge break on use 1/10 times. (We once had a polish lad working for us digging out a garden break the pickaxe and then hide it 'cos he thought we'd shout at him. We just laughed, you could not have broken that thing without putting quite a bit of effort into it, and went off to buy him a new handle.) I also invented a secret way to get yourself killed, let you alias say "attack north" to "smack bitch", figured out a way to ensure a sledge was reachable, but does not have to be in the first room, and made it print "You win! Game over." and quit when you reach the goal. --[[User:Petelomax|Pete Lomax]] ([[User talk:Petelomax|talk]]) 16:41, 17 June 2019 (UTC)

:Fair points.  I'll take the constraints in the task and see what I can do with it. --[[User:JoeStrout|JoeStrout]] ([[User talk:JoeStrout|talk]]) 16:57, 17 June 2019 (UTC)
