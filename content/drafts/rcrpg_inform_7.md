+++
title = "RCRPG/Inform 7"
description = ""
date = 2010-10-30T20:10:51Z
aliases = []
[extra]
id = 8639
[taxonomies]
categories = []
tags = []
+++

{{collection|RCRPG}}
This [[Inform 7]] version of [[:Category:RCRPG|RCRPG]] simulates an XYZ space of rooms using a fixed pool of in-game objects.

Player-defined aliases are not supported, although some common abbreviations are built in. Equipped items are marked with a flag instead of being removed from inventory. The sledge has a chance of breaking each time the player breaks through a wall (but only after a replacement has been discovered somewhere).

== Language Idioms ==
(Add to this list if you see anything of interest not mentioned.)

This version demonstrates the following language idioms:
* Title and section headings
* Tables
* Objects and kinds
* Object pooling
* Indexed text ([[:Category:String manipulation|strings]])
* Action rules
* User input via actions ("Understand...")
* Activity rules ("printing the name")
* Dynamic dispatch
* Text substitutions ("To say...")
* Adjectives ("Definition: ...")
* Relations (wielding)
* Chronology tests ("for the first time")
* Pathfinding ("number of moves")
* Customized action patterns ("sledging with")

== Code ==
{{works with|Z-machine|8}}
{{works with|Glulx virtual machine}}

```inform7
"RCRPG"
 
The story headline is "An implementation of http://rosettacode.org/wiki/RCRPG".
 
Chapter - Rooms
 
Section - Prize objects
 
[Inform objects aren't created dynamically, but we can fake it by predefining
pools of identical objects. These will start out off-stage, and we'll move them
to newly discovered rooms as needed.]
 
A prize is a kind of thing.
 
A sledge is a kind of prize. There are 20 sledges.
A gold coin is a kind of prize. There are 70 gold coins.
A ladder is a kind of prize. There are 10 ladders.
 
Section - Rooms in XYZ space
 
A room has a number called X position.
A room has a number called Y position.
A room has a number called Z position.
 
To say room coordinates of (R - room):
	say "room ([X position of R],[Y position of R],[Z position of R])".
 
Section - Dynamically naming rooms
 
A room has indexed text called the given name.
The given name of a room is usually "The Unnamed Room".
 
Definition: a room is undubbed if the given name of it is "The Unnamed Room".
 
The printed name of a room is "[given name]".
 
Section - Listing exits
 
The description of a room is "You are in [room coordinates of the item
described], [the item described]. [room exit list]".
 
Definition: a direction (called D) is accessible rather than inaccessible if
the room D from the location is not nothing.
 
To say room exit list:
	let L be the list of accessible directions;
	if the number of entries in L is:
		-- 0: say "There are no exits from this room. Perhaps you need
				to make one?[run paragraph on]";
		-- 1: say "There is an exit [entry 1 in L].[run paragraph on]";
		-- otherwise: say "There are exits [L].[run paragraph on]";
 
Rule for printing the name of a direction (called D) while looking:
	say exit direction of D.
 
To say exit direction of (D - up): say "in the ceiling".
To say exit direction of (D - down): say "in the floor".
To say exit direction of (D - a direction): say "to the [printed name of D]".
 
Section - The recycled room pool
 
[The room pool is similar to the prize pools, but "off-stage" is meaningless
for rooms, so we define a new either/or property.]
 
A room can be allocated or unallocated. A room is usually unallocated.
There are 100 unallocated rooms.
 
[When we run out of unallocated rooms, we'll reclaim rooms that have already
been discovered. We prefer to reclaim rooms that haven't been named, and we'll
never reclaim the current room, a neighboring room, or the two special rooms.]
 
Definition: a room is reclaimable:
	if it is the location, no;
	if it is Start Room or it is Prize Room, no;
	if it is adjacent, no;
	yes.
 
Definition: a prize is reclaimable if it is in a reclaimable room.
 
To decide which room is a recycled room:
	if a room (called R) is unallocated, decide on R;
	if there is a reclaimable undubbed room (called R), decide on R;
	decide on a random reclaimable room.
 
[After a room is recycled for use, we need to clear out the traces of its
previous use and fill it with a prize.]
 
To initialize (R - room):
	now R is allocated;
	now the given name of R is "The Unnamed Room";
	now everything in R is off-stage;
	repeat with D running through directions:
		let R2 be the room D from R;
		if R2 is not nothing:
			change the D exit of R to nothing;
			change the (opposite of D) exit of R2 to nothing;
	let P be a random off-stage prize;
	if P is nothing, let P be a random reclaimable prize;
	if P is not nothing, move P to R.
 
To decide which room is a room allocated for position (X - number) /
(Y - number) / (Z - number):
	repeat with AR running through allocated rooms:
		if the X position of AR is X and
			the Y position of AR is Y and
			the Z position of AR is Z, decide on AR;
	let R be a recycled room;
	initialize R;
	now the X position of R is X;
	now the Y position of R is Y;
	now the Z position of R is Z;
	decide on R.
 
Section - Predefined rooms
 
The player is in Start Room.
 
Start Room is an allocated room with given name "Start Room".
In Start room is a sledge.
 
Prize Room is an allocated room with given name "Prize Room", X position 1,
Y position 1, and Z position 5. In Prize Room are 8 gold coins.
 
Section - Traveling in XYZ space
 
[Table syntax can be used to define new objects, or in this case to set the
properties of previously defined ones.]
 
Some directions are defined by the Table of Directional Offsets.
 
Table of Directional Offsets
direction	X offset	Y offset	Z offset
up		0		0		1
down		0		0		-1
north		0		1		0
east		1		0		0
south		0		-1		0
west		-1		0		0
 
[Inform also knows about these, but we won't use them for digging...]
[northeast	1		1		0
southeast	1		-1		0
southwest	-1		-1		0
northwest	-1		1		0]
 
Before going up when the location does not contain a ladder and the player
carries a ladder (called L):
	say "(first dropping [the L])";
	silently try dropping L.
 
Instead of going up when the location does not contain a ladder, say
"There needs to be a ladder in the room before you can climb."
 
Report going up for the first time: say "You climb up the ladder."
 
Report going down to a room that does not contain a ladder for the first time:
	say "You jump down, but you'll need to find a ladder before you can get
		back up."
 
[Redirect CLIMB LADDER to GO UP.]
Instead of climbing a ladder, try going up.
 
[Redirect CLIMB UP/DOWN to GO UP/DOWN.]
Before climbing a direction (called D), try going D instead.
 
Instead of jumping, say "You jump on the spot, but you can't quite reach
the ceiling."
 
Section - Equipment
 
A thing can be equipped or unequipped. A thing is usually unequipped.
 
[This lets the player refer to equipped objects in commands like "drop
unequipped sledge", and also groups them apart in inventory listings.]
Understand the equipped property as referring to a thing.
 
After printing the name of something equipped while taking inventory:
	say " (equipped)".
 
Wielding relates a person (called P) to a thing (called T) when P carries T
and T is equipped. The verb to wield (he wields, they wield, he is wielding,
it is wielded) implies the wielding relation.

Every turn:
	now everything not carried by a player is unequipped.
 
Chapter - Actions
 
Section - Naming
 
Naming is an action applying to one topic. Understand "name [text]" as naming.
 
Carry out naming:
	now the given name of the location is the topic understood in
		title case.
 
Report naming:
	say "OK, [room coordinates of the location] is now [given name of the
		location]."
 
Section - Sledging
 
Sledging is an action applying to one visible thing. Understand "attack
[direction]" or "sledge [direction]" as sledging. Understand the command "a"
as "attack".
 
The sledging action has an object called the active sledge (matched as "with").
 
Setting action variables for sledging:
	if the player wields a sledge (called S), now the active sledge is S.

Before sledging with nothing when the player is carrying a sledge (called S):
	say "(first equipping [the S])";
	silently try equipping S;
	if the player wields S, now the active sledge is S.
 
Check sledging with nothing:
	instead say "You won't accomplish anything without a sledge equipped.";
 
Check sledging an accessible direction:
	instead say "You swing your sledge wildly."
 
Definition: a direction (called D) is bashable rather than unbashable:
	if the X offset of D is not 0, yes;
	if the Y offset of D is not 0, yes;
	if the Z offset of D is not 0, yes;
	no.
 
Check sledging an unbashable direction:
	instead say "You can only open exits to the [list of bashable
		directions]."
 
Carry out sledging a direction (called D):
	let X be the X position of the location plus the X offset of D;
	let Y be the Y position of the location plus the Y offset of D;
	let Z be the Z position of the location plus the Z offset of D;
	let R be a room allocated for position X / Y / Z;
	change the D exit of the location to R;
	change the (opposite of D) exit of R to the location.

Definition: a thing is accessible if the location of it is a connected room.
Definition: a room is connected if the number of moves from it to the location
is not -1.

Carry out sledging when a random chance of 1 in 4 succeeds and an unequipped
sledge is accessible:
	remove the active sledge from play.
 
Report sledging a direction (called D):
	let destination be the room D of the location;
	say "You bash until the surface crumbles, leading a hole [exit
		direction of D] you can crawl through";
	if the active sledge is off-stage, say ". Your sledge shatters
		from the impact";
	say "."
 
Section - Equipping
 
Equipping is an action applying to one carried thing. Understand "equip
[something]" or "wield [something]" as equipping.
 
Check equipping something which is equipped:
	instead say "[The noun] is already equipped."
 
Carry out equipping:
	now the noun is equipped.
 
Report equipping:
	say "You equip [the noun][first time]. Put it to good use[only]."
 
Section - Unequipping
 
Unequipping is an action applying to one carried thing. Understand "unequip"
or "unequip [something]" as unequipping.
 
Rule for supplying a missing noun when unequipping:
	if the player is wielding something (called the equipped item), now
		the noun is the equipped item.
 
Check unequipping when the player is not wielding the noun:
	instead say "[The noun] is not equipped."
 
Carry out unequipping:
	now the noun is unequipped.
 
Report unequipping:
	say "You unequip [the noun]."
```

