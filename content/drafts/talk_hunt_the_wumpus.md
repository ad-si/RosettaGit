+++
title = "Talk:Hunt The Wumpus"
description = ""
date = 2018-06-10T20:40:36Z
aliases = []
[extra]
id = 19113
[taxonomies]
categories = []
tags = []
+++

== Details ==

These rules actually conflict with the original wumpus rules, and leave some issues unspecified. Would it be acceptable to implement something closer to the original? --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 07:41, 8 May 2015 (UTC)

I'd like to modify my Javascript source to something more similar to the original, if you point me to the differences you noticed. Also, what is not specified? I'd love to edit the description. Last thing: The only conscious conflict I've made, just for a matter of semplicity, is the ability to only shoot to nearby rooms.--[[User:Pistacchio|Pistacchio]] ([[User talk:Pistacchio|talk]]) 10:16, 8 May 2015 (UTC)

Ok, sure... well, I started coding up a J simplification, and so I extracted the original intro text (without the press return to continue stuff), and it looked like this:


```J
INTRO=:0 :0
 WELCOME TO 'HUNT THE WUMPUS'
  THE WUMPUS LIVES IN A CAVE OF 20 ROOMS. EACH ROOM
HAS 3 TUNNELS LEADING TO OTHER ROOMS. (LOOK AT A
DODECAHEDRON TO SEE HOW THIS WORKS-IF YOU DON'T KNOW
WHAT A DODECAHEDRON IS, ASK SOMEONE)

     HAZARDS:
 BOTTOMLESS PITS - TWO ROOMS HAVE BOTTOMLESS PITS IN THEM
     IF YOU GO THERE, YOU FALL INTO THE PIT (& LOSE!)
 SUPER BATS - TWO OTHER ROOMS HAVE SUPER BATS. IF YOU
     GO THERE, A BAT GRABS YOU AND TAKES YOU TO SOME OTHER
     ROOM AT RANDOM. (WHICH MAY BE TROUBLESOME)

     WUMPUS:
 THE WUMPUS IS NOT BOTHERED BY HAZARDS (HE HAS SUCKER
 FEET AND IS TOO BIG FOR A BAT TO LIFT).  USUALLY
 HE IS ASLEEP.  TWO THINGS WAKE HIM UP: YOU SHOOTING AN
ARROW OR YOU ENTERING HIS ROOM.
     IF THE WUMPUS WAKES HE MOVES (P=.75) ONE ROOM
 OR STAYS STILL (P=.25).  AFTER THAT, IF HE IS WHERE YOU
 ARE, HE EATS YOU UP AND YOU LOSE!

     YOU:
 EACH TURN YOU MAY MOVE OR SHOOT A CROOKED ARROW
   MOVING:  YOU CAN MOVE ONE ROOM (THRU ONE TUNNEL)
   ARROWS:  YOU HAVE 5 ARROWS.  YOU LOSE WHEN YOU RUN OUT
   EACH ARROW CAN GO FROM 1 TO 5 ROOMS. YOU AIM BY TELLING
   THE COMPUTER THE ROOM#S YOU WANT THE ARROW TO GO TO.
   IF THE ARROW CAN'T GO THAT WAY (IF NO TUNNEL) IT MOVES
   AT RANDOM TO THE NEXT ROOM.
     IF THE ARROW HITS THE WUMPUS, YOU WIN.
     IF THE ARROW HITS YOU, YOU LOSE.

    WARNINGS:
     WHEN YOU ARE ONE ROOM AWAY FROM A WUMPUS OR HAZARD,
     THE COMPUTER SAYS:
 WUMPUS:  'I SMELL A WUMPUS'
 BAT   :  'BATS NEARBY'
 PIT   :  'I FEEL A DRAFT'

)
```


So... different (more concise!) messages, bats are non-lethal, and the wumpus sometimes moves.

Also, looking at the implementation, this was unspecified: initially none of the player nor the hazards are co-located. --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 12:14, 8 May 2015 (UTC)

:Yeah, I really don't see how this task got out of draft status. At the time that happened, there were actually more submissions following the rules of the original game than there were using the task spec. Right now it's about half-half, but several of the submissions that chose to follow the task spec have also made up their own rules, so very few exactly match the spec. I think it would make a whole lot more sense to just link to the original source and description that was [http://www.atariarchives.org/bcc1/showpage.php?page=247 published in Creative Computing magazine]. Or at least put the task back into draft status until there is more of a consensus on what the rules should be. --[[User:J4 james|j4_james]] ([[User talk:J4 james|talk]]) 20:37, 10 June 2018 (UTC)
