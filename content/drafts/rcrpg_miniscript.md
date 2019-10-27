+++
title = "RCRPG/MiniScript"
description = ""
date = 2019-06-17T23:58:40Z
aliases = []
[extra]
id = 22382
[taxonomies]
categories = []
tags = []
+++

This [[MiniScript]] version of [[RCRPG]] attempts to go beyond the requirements of the task to make something a little more game-like.  In particular, the goal location is randomized, but clues are hidden in the dungeon to guide your way.  In addition, pointless busywork (like having to explicitly bash open a passage before moving) have been mostly eliminated.  Also, some "color" comments are randomly added to the rooms, but both these comments and the room contents are persistent, even between levels.  This strengthens the illusion of moving around a realistic virtual space.

== Objective ==
The objective of the game is to find your way to the mystical Room of MacGuffin, whose location is unknown.  Watch for messages scratched on the walls by previous travelers, which may provide clues on your quest.

==Commands==

Direction commands: '''north''', '''south''', '''east''', '''west''', '''up''', '''down'''. Enter these (or their one-letter abbreviations) to move in the given direction, bashing open a passage if necessary.  Note that such bashing requires a sledgehammer; one has been provided to you.  In order to go up, there must be a ladder in the room.

Take items from the room with '''take''' or '''get'''.  You may specify an item name (''take ladder'') to take one of that item; a plural name (''take ladders'') to take all available; or ''take all'' to take everything from the room.

Drop items into the room with '''drop'''.  Specify the item to drop in the same way as for ''take''.

View the items you're carrying with '''inventory''' (or '''inv''' or '''i''').  Review the room description, including its contents, with '''look'''.

To get in-game help, enter '''help''' (optionally followed by the command you want to know more about).

==Code==


```MiniScript
pos = [0,0,0]
goal = [floor(rnd*10), floor(rnd*10), floor(3+rnd*5)]

dir = {}			// key: direction name; value: [dx, dy, dz]
dirAbbrevs = {}		// key: direction abbrevation; value: full name
dir.up = [0,0,1]
dir.down = [0,0,-1]
dir.north = [0,1,0]
dir.south = [0,-1,0]
dir.east = [1,0,0]
dir.west = [-1,0,0]
for k in dir.indexes
	dirAbbrevs[k[0]] = k
end for
inverseDir = {"up":"down", "down":"up", "east":"west", "west":"east", "north":"south", "south":"north"}

descNum = function(count, noun)
	if count == 1 then return "a " + noun
	return str(count) + " " + noun + "s"
end function

descList = function(lst)
	if lst.len == 0 then return ""
	if lst.len == 1 then return lst[0]
	if lst.len == 2 then return lst[0] + " and " + lst[1]
	return lst[:-1].join(", ") + ", and " + lst[-1]
end function

pickAny = function(options)
	lst = options.split(";")
	return lst[rnd * lst.len]
end function

Contents = {}
Contents.ladders = 0
Contents.gold = 0
Contents.hammers = 0  // (note: a "sledge" is a sled or sleigh, not a sledgehammer) 
Contents.desc = function(prefix, postfix=".")
	s = []
	if self.ladders > 0 then s.push descNum(self.ladders, "ladder")
	if self.hammers > 0 then s.push descNum(self.hammers, "sledgehammer")
	if self.gold > 0 then s.push descNum(self.gold, "gold coin")
	if not s then return prefix + " nothing" + postfix
	return prefix + " " + descList(s) + postfix
end function
Contents.initRandom = function()
	self.ladders = (rnd < 0.3)
	self.gold = ceil(rnd * 3) * (rnd < 0.1)
	self.hammers = (rnd < 0.02)
end function
Contents.propName = function(obj)
	if obj == "ladder" or obj == "ladders" then return "ladders"
	if obj == "gold" or obj == "coin" or obj == "coins" then return "gold"
	if obj[:6] == "hammer" or obj[:6] == "sledge" then return "hammers"
	return ""
end function
Contents.hasAny = function(obj)
	pname = Contents.propName(obj)
	if pname == "" then return false
	return self[pname] > 0
end function
Contents.withdraw = function(obj)
	result = new Contents
	if obj == "all" then
		result.ladders = self.ladders
		self.ladders = 0
		result.hammers = self.hammers
		self.hammers = 0
		result.gold = self.gold
		self.gold = 0
	else
		pname = Contents.propName(obj)
		if self[pname] < 1 then return null
		if obj[-1] == "s" then count = self[pname] else count = 1
		self[pname] = self[pname] - count
		result[pname] = count
	end if
	return result
end function
Contents.deposit = function(c)
	self.ladders = self.ladders + c.ladders
	self.hammers = self.hammers + c.hammers
	self.gold = self.gold + c.gold
end function
		
inventory = new Contents
inventory.hammers = 1

Room = {}
Room.exits = {}
Room.color = ""
Room.init = function(pos)
	self.contents = new Contents
	self.contents.initRandom
	if pos == goal then
		self.color = "YOU FOUND IT!  This is the mystical Room of MacGuffin!"
	else if rnd < 0.5 then
		// Give a hint about where the goal is.
		opt = floor(rnd * 3)
		if opt == 0 then
			if goal[2] == pos[2] then
				hint = "The MacGuffin lies on this level."
			else if goal[2] > pos[2] then
				hint = "The MacGuffin rests above."
			else
				hint = "The MacGuffin lies below."
			end if
		else if opt == 1 then
			if goal[0] > pos[0] then
				hint = "The MacGuffin lies to the east."
			else if goal[0] < pos[0] then
				hint = "The MacGuffin lies to the west."
			else
				hint = "The MacGuffin lies... <undecipherable>"
			end if
		else
			if goal[1] > pos[1] then
				hint = "The MacGuffin lies to the north."
			else if goal[1] < pos[1] then
				hint = "The MacGuffin lies to the south."
			else
				hint = "The MacGuffin lies... <undecipherable>"
			end if
		end if
		self.color = "Scratched on the wall is a message: " + hint
	else if rnd < 0.5 then
		// Give some random color comment.
		color = []
		opt = floor(rnd * 3)
		if opt == 1 then
			color.push "You detect " + pickAny("a faint;an odd;a musty;a rotten;an unpleasant")
			color.push pickAny("smell;odor;scent;stench") + " here."
		else if opt == 2 then
			color.push "You can hear a" + pickAny(" faint; quiet; soft; strange;n eerie")
			color.push pickAny("dripping;scratching;scrabbling;whistling;moaning")
			color.push pickAny("sound;noise") + " here."
		else
			color.push "The " + pickAny("walls here are;floor here is;ceiling of this room is")
			color.push pickAny("smeared with;discolored by;marred by;covered with")
			color.push pickAny("dried blood;cobwebs;scratches;gouges;scorch marks;soot;mineral deposits;bits of fur") + "."
		end if
		self.color = color.join
	end if
	self.exits = {}
end function

rooms = {}  // key: STRING FORM of position; value: Room
getRoom = function(pos=null)
	if pos == null then pos = globals.pos
	key = str(pos)
	if not rooms.hasIndex(key) then
		rooms[key] = new Room
		rooms[key].init pos
	end if
	return rooms[key]
end function

// Commands:
commands = {}
help = {}

commands.drop = function(obj)
	items = inventory.withdraw(obj)
	if items == null then
		print "You don't have any " + obj + "."
	else
		getRoom.contents.deposit items
		print items.desc("You drop")
	end if
end function
help.drop = "Drops an item from your inventory into the room.  Specify object name or ""all""."

commands.go = function(d)
	oldRoom = getRoom
	if dirAbbrevs.hasIndex(d) then d = dirAbbrevs[d]
	if not dir then
		print "Which direction?"
	else if not dir.hasIndex(d) then
		print "That's not a direction I recognize."
	else if d == "up" and oldRoom.contents.ladders == 0 then
		print "There is no ladder in this room to go up."
	else
		if not oldRoom.exits.hasIndex(d) then
			if inventory.hammers < 1 then
				print "There is no exit that way, and you don't have a sledgehammer."
				return
			end if
			wall = "wall"
			if d == "up" then wall = "ceiling"
			if d == "down" then wall = "floor"
			print "You bash the " + wall + " until you make a passage big enough to crawl through."
			oldRoom.exits.push d
		end if
		delta = dir[d]
		pos[0] = pos[0] + delta[0]
		pos[1] = pos[1] + delta[1]
		pos[2] = pos[2] + delta[2]
		newRoom = getRoom
		newRoom.exits.push inverseDir[d]
		verb = "crawl"
		if d == "up" then verb = "climb"
		if d == "down" then
			if newRoom.contents.ladders > 0 then verb = "climb" else verb = "drop"
		end if
		print "You " + verb + " " + d + "."
		commands.look
		if pos == goal then
			print "You have recovered the MacGuffin and " + descNum(inventory.gold, "gold coin") + ".  You win!"
			globals.gameOver = true
		end if
	end if
end function
help.go = "Moves in the given direction, bashing open a passage if necessary."

commands.help = function(arg)
	if aliases.hasIndex(arg) then arg = aliases[arg]
	if help.hasIndex(arg) then
		print arg + ": " + help[arg]
	else
		print "Available commands: " + descList(help.indexes.sort)
	end if
end function
help.help = "Prints the help.  Obviously."

commands.inventory = function(arg)
	print inventory.desc("You have")
end function
help.inventory = "Lists the items you are carrying."

commands.look = function(arg)
	print "You are at " + pos + "."
	room = getRoom
	if room.color != "" then print room.color
	print room.contents.desc("You see", " here.")
	exits = room.exits.indexes
	if exits.len == 0 then
		print "There are no exits."
	else if room.exits.len == 1 then
		print "There is a passage " + exits[0] + "."
	else
		print "There are passages " + descList(exits) + "."
	end if		
end function
help.look = "Prints a description of the room and its contents."

commands.quit = function(arg)
	print "Quitter!"
	globals.gameOver = true
end function
help.quit = "Quits the game."

commands.take = function(obj)
	roomStuff = getRoom.contents
	items = roomStuff.withdraw(obj)
	if items == null then
		print "You don't see any " + obj + " here."
	else
		inventory.deposit items
		print items.desc("You take")
	end if
end function
help.take = "Picks up an item in the room; specify item name, or ""all""."

// Command aliases:
aliases = {"i":"inventory", "inv":"inventory", "l":"look", "get":"take"}

// Main game loop
gameOver = false
commands.look
while not gameOver
	cmd = input(">").split(" ", 2)
	if cmd.len == 1 then cmd.push null
	verb = cmd[0]
	if aliases.hasIndex(verb) then verb = aliases[verb]
	if commands.hasIndex(verb) then
		f = commands[verb]
		f cmd[1]
	else if dirAbbrevs.hasIndex(verb) or dir.hasIndex(verb) then
		commands.go verb
	else
		print "Invalid command.  For help, enter: help"
	end if
end while
```


{{out}}

```txt
You are at [0, 0, 0].
You see a ladder and 2 gold coins here.
There are no exits.
>up
You bash the ceiling until you make a passage big enough to crawl through.
You climb up.
You are at [0, 0, 1].
You see nothing here.
There is a passage down.
>n
You bash the wall until you make a passage big enough to crawl through.
You crawl north.
You are at [0, 1, 1].
The floor here is covered with bits of fur.
You see 3 gold coins here.
There is a passage south.
>get all
You take 3 gold coins.
...
You are at [3, 5, 6].
Scratched on the wall is a message: The MacGuffin lies... <undecipherable>
You see nothing here.
There are passages north and down.
>drop ladder
You drop a ladder.
>up
You bash the ceiling until you make a passage big enough to crawl through.
You climb up.
You are at [3, 5, 7].
YOU FOUND IT!  This is the mystical Room of MacGuffin!
You see a ladder here.
There is a passage down.
You have recovered the MacGuffin and 5 gold coins.  You win!
```

