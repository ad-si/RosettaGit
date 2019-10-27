+++
title = "RCRPG/D"
description = ""
date = 2015-01-05T15:56:44Z
aliases = []
[extra]
id = 12918
[taxonomies]
categories = []
tags = []
+++

{{collection|RCRPG}}


[[D]] version of [[:Category:RCRPG|RCRPG]], with very basic input validation.

==Code==
{{trans|Python}}

```d
import std.stdio, std.typecons, std.random, std.string, std.conv,
       std.array, std.range, std.algorithm, std.traits, std.typetuple;

alias P3 = Tuple!(int, "x", int, "y", int, "z");

immutable P3[string] directions;
string[][string] aliases;
string[P3] roomNames;

static this() {
    directions = ["north" : P3( 0,-1, 0),
                  "east"  : P3( 1, 0, 0),
                  "south" : P3( 0, 1, 0),
                  "west"  : P3(-1, 0, 0),
                  "up"    : P3( 0, 0, 1),
                  "down"  : P3( 0, 0,-1)];

    aliases = ["north" : ["move","north"],
               "south" : ["move","south"],
               "east"  : ["move","east"],
               "west"  : ["move","west"],
               "up"    : ["move","up"],
               "down"  : ["move","down"]];

    roomNames = [P3(0, 0, 0): "the starting room",
                 P3(1, 1, 5): "the prize room"];
}

class Room {
    string[] items;
    bool[string] passages;

    this(in string[] items = null) {
        this.passages = directions.byKey.zip(repeat(false))
                        .assocArray;
        this.items = items.dup;
    }

    string describe(in P3 location) const {
        auto result = "You are at ";
        if (location in roomNames)
            result ~= roomNames[location];
        else
            result ~= format("%d,%d,%d", location.tupleof);

        if (!this.items.empty)
            result ~= "\nOn the ground you can see: " ~
                      this.items.join(", ");
        result ~= "\nExits are: ";
        string[] exits = this.passages.byKey
                         .filter!(k => passages[k])
                         .array;
        if (exits.empty) exits = ["None"];
        result ~= exits.map!capitalize.join(", ");
        return result;
    }

    string[] take(in string target) {
        string[] results;
        if (target == "all") {
            results = this.items.dup;
            this.items.length = 0;
            writeln("You now have everything in the room.");
        } else {
            immutable idx = this.items.countUntil(target);
            if (idx != -1) {
                this.items = this.items.remove(idx);
                results = [target];
                writeln("Taken ", target, ".");
            } else {
                writeln("Item not found.");
            }
        }
        return results;
    }
}

P3 get_new_coord(P3 oldCoord, string direction) pure nothrow {
    return P3(oldCoord[0] + directions[direction][0],
              oldCoord[1] + directions[direction][1],
              oldCoord[2] + directions[direction][2]);
}

string opposite_dir(string direction) pure nothrow {
    switch (direction) {
        case "north" : return "south";
        case "south" : return "north";
        case "west"  : return "east";
        case "east"  : return "west";
        case "up"    : return "down";
        case "down"  : return "up";
        default:
            throw new Error("No direction found: " ~ direction);
    }
}

string[] make_random_items() {
    return [[], ["sledge"], ["ladder"], ["gold"]][uniform(0, $)];
}

class World {
    P3 currentPos = P3(0,0,0);
    Room[P3] rooms;
    string[] inv;
    string equipped;

    this() {
        this.rooms = [P3(0,0,0): new Room(["sledge"])];
    }

    string look() {
        return this.rooms[this.currentPos].describe(this.currentPos);
    }

    void move(in string direction) {
        if (direction == "up" &&
            !canFind(this.rooms[this.currentPos].items, "ladder"))
            return writeln("You'll need a ladder in " ~
                           "this room to go up.");
        if (direction !in directions)
            return writeln("That's not a direction.");
        if (this.rooms[this.currentPos].passages[direction])
            currentPos = get_new_coord(this.currentPos, direction);
        else
            writeln("Can't go that way.");
    }

    void newalias(in string newAlias, in string[] command...) {
        if (command.length == 2) {
            if (command[0] in aliases) { // avoid recursive aliasing
                writeln("You cannot alias an alias: " ~ command[0]);
            } else {
                aliases[newAlias] = command.dup;
                writeln("Alias created.");
            }
        } else
            writeln("Wrong number of arguments for alias.");
    }

    void inventory() {
        if (this.inv.empty)
            writeln("You aren't carrying anything.");
        else
            writeln("Carrying: ", this.inv.join(", "));
        if (!this.equipped.empty)
            writeln("Holding: ", this.equipped);
    }

    void take(in string target) {
        this.inv ~= this.rooms[this.currentPos].take(target);
    }

    void drop(in string target) {
        if (target == "all") {
            this.rooms[this.currentPos].items ~= this.inv;
            this.inv.length = 0;
            writeln("Everything dropped.");
        } else {
            immutable idx = this.inv.countUntil(target);
            if (idx != -1) {
                this.inv = this.inv.remove(idx);
                this.rooms[this.currentPos].items ~= target;
                writeln("Dropped ", target , ".");
            } else {
                writeln("Could not find item in inventory.");
            }
        }
    }

    void equip(in string itemName) {
        immutable idx = this.inv.countUntil(itemName);
        if (idx != -1) {
            if (!this.equipped.empty)
                this.unequip;
            this.inv = this.inv.remove(idx);
            this.equipped = itemName;
            writeln("Equipped ", itemName, ".");
        } else
            writeln("You aren't carrying that.");
    }

    void unequip() {
        if (this.equipped.empty)
            writeln("You aren't equipped with anything.");
        else {
            this.inv ~= this.equipped;
            writeln("Unequipped ", this.equipped, ".");
            this.equipped = null;
        }
    }

    void name(in string[] newRoomNameTokens...) {
        roomNames[this.currentPos] = newRoomNameTokens.join(' ');
    }

    void dig(in string direction) {
        if (this.equipped != "sledge")
            return writeln("You don't have a digging tool equipped.");
        if (direction !in directions)
            return writeln("That's not a direction.");
        if (!this.rooms[this.currentPos].passages[direction]) {
            this.rooms[this.currentPos].passages[direction] = true;
            auto joinRoomPos = get_new_coord(this.currentPos,
                                             direction);
            if (joinRoomPos !in this.rooms)
                this.rooms[joinRoomPos]= new Room(make_random_items);
            this.rooms[joinRoomPos]
                .passages[opposite_dir(direction)] = true;
            writeln("You've dug a tunnel.");
        } else
            writeln("Already a tunnel that way.");
    }
}

void processArguments(in string[] a, World w) {
    foreach (f; __traits(derivedMembers, World)) {
        static if (isCallable!(mixin("World." ~ f))) {
            enum len = ParameterTypeTuple!(mixin("World." ~ f)).length;
            enum var = variadicFunctionStyle!(mixin("World." ~ f));
            static if (len == 0)
                mixin("if (a[0] == f) w." ~ f ~ "();");
            else static if (var && len == 1)
                mixin("if(a[0] == f) w." ~ f ~ "(a[1 .. $]);");
            else static if (var && len == 2)
                mixin("if(a[0] == f) w." ~ f ~ "(a[1], a[2 .. $]);");
            else static if (len == 1)
                mixin("if (a[0] == f) w." ~ f ~ "(a[1]);");
        }
    }
}

void main() {
    auto world = new World;
    writeln("Welcome to the dungeon!\nGrab the sledge && make your" ~
            " way to room 1,1,5 for a non-existent prize!");
    while (true) {
        writeln("\n", world.look);
        write("> ");
        auto tokens = readln.strip.toLower.split;
        if (tokens[0] == "quit")
            break;
        if (tokens[0] == "alias")
            tokens[0] = "newalias";
        while (!tokens.empty) {
            if (tokens[0] in aliases &&
                tokens[0] != aliases[tokens[0]][0]) {
                tokens = aliases[tokens[0]] ~ tokens[1 .. $];
                continue;
            }
            try {
                processArguments(tokens, world);
            } catch (Throwable e) {
                writeln("Invalid input.");
            }

            break;
        }
    }

    writeln("Thanks for playing!");
}
```

