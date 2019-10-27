+++
title = "RCRPG/Python"
description = ""
date = 2013-02-12T00:52:17Z
aliases = []
[extra]
id = 7424
[taxonomies]
categories = []
tags = []
+++

{{collection|RCRPG}}
This [[Python]] 3.1 version of [[:Category:RCRPG|RCRPG]] uses the standard text interface.

All the commands listed on the blog post are implemented, as well as move and unequip.

==Code==

```python
directions = {'north':(0,-1,0),
              'east':(1,0,0),
              'south':(0,1,0),
              'west':(-1,0,0),
              'up':(0,0,1),
              'down':(0,0,-1)}

aliases = {'north': ['move','north'],
           'south': ['move','south'],
           'east': ['move','east'],
           'west': ['move','west'],
           'up': ['move','up'],
           'down': ['move','down']}

roomNames = {(0,0,0):'the starting room',
             (1,1,5):'the prize room'}

class Room:
    def __init__(self, items=[]):
        self.passages = dict.fromkeys(directions.keys(), False)
        self.items = items[:]

    def describe(self, location):
        result = 'You are at '
        if location in roomNames:
            result += roomNames[location]
        else:
            posStr = ",".join(str(v) for v in location)
            result += posStr
        
        if self.items:
            result += '\nOn the ground you can see: '+', '.join(self.items)
        result += '\nExits are: '
        exits = [k for k,v in self.passages.items() if v] or ['None']
        result += ', '.join(map(str.capitalize, exits))
        return result

    def take(self, target):
        results = []
        if target == 'all':
            results = self.items[:]
            del self.items[:]
            print('You now have everything in the room.')
        elif target in self.items:
            self.items.remove(target)
            results = [target]
            print('Taken.')
        else:
            print('Item not found.')
        return results

def get_new_coord(oldCoord, direction):
    return (oldCoord[0]+directions[direction][0],
            oldCoord[1]+directions[direction][1],
            oldCoord[2]+directions[direction][2])

def opposite_dir(direction):
    if direction == 'north':
        return 'south'
    elif direction == 'south':
        return 'north';
    elif direction == 'west':
        return 'east';
    elif direction == 'east':
        return 'west';
    elif direction == 'up':
        return 'down';
    elif direction == 'down':
        return 'up';
    else:
        raise Exception('No direction found: '+direction)

def make_random_items():
    from random import randrange
    rand = randrange(4)
    if rand == 0:
        return []
    elif rand == 1:
        return ['sledge']
    elif rand == 2:
        return ['ladder']
    else:
        return ['gold']

class World:
    def __init__(self):
        self.currentPos = 0,0,0
        self.rooms = {(0,0,0): Room(['sledge'])}
        self.inv = []
        self.equipped = ''

    def look(self):
        return self.rooms[self.currentPos].describe(self.currentPos)

    def move(self, direction):
        if direction == 'up' and \
           'ladder' not in self.rooms[self.currentPos].items:
            print("You'll need a ladder in this room to go up.")
            return
        if direction not in directions:
            print("That's not a direction.")
            return
        if self.rooms[self.currentPos].passages[direction]:
            self.currentPos = get_new_coord(self.currentPos, direction)
        else:
            print("Can't go that way.")

    def alias(self, newAlias, *command):
        aliases[newAlias] = list(command)
        print('Alias created.')

    def inventory(self):
        if self.inv:
            print('Carrying: '+', '.join(self.inv))
        else:
            print("You aren't carrying anything.")
        if self.equipped:
            print('Holding: '+self.equipped)

    def take(self, target):
        self.inv += self.rooms[self.currentPos].take(target)

    def drop(self, target):
        if target == 'all':
            self.rooms[self.currentPos].items.extend(self.inv)
            del self.inv[:]
            print('Everything dropped.')
        elif target in self.inv:
            self.inv.remove(target)
            self.rooms[self.currentPos].items.append(target)
            print('Dropped.')
        else:
            print('Could not find item in inventory.')

    def equip(self, itemName):
        if itemName in self.inv:
            if self.equipped:
                self.unequip()
            self.inv.remove(itemName)
            self.equipped = itemName
            print('Equipped '+itemName+'.')
        else:
            print("You aren't carrying that.")

    def unequip(self):
        if not self.equipped:
            print("You aren't equipped with anything.")
        else:
            self.inv.append(self.equipped)
            print('Unequipped '+self.equipped+'.')
            self.equipped = ''

    def name(self, *newRoomNameTokens):
        roomNames[self.currentPos] = ' '.join(newRoomNameTokens)

    def dig(self, direction):
        if self.equipped != 'sledge':
            print("You don't have a digging tool equipped.")
            return
        if direction not in directions:
            print("That's not a direction.")
            return
        if not self.rooms[self.currentPos].passages[direction]:
            self.rooms[self.currentPos].passages[direction] = True
            joinRoomPos = get_new_coord(self.currentPos, direction)
            if joinRoomPos not in self.rooms:
                self.rooms[joinRoomPos] = Room(make_random_items())
            self.rooms[joinRoomPos].passages[opposite_dir(direction)] = True
            print("You've dug a tunnel.")
        else:
            print("Already a tunnel that way.")
        

world = World()
print("Welcome to the dungeon!\nGrab the sledge and make your way to room 1,1,5 for a non-existent prize!")
while True:
    print('\n'+world.look())
    tokens = input("> ").strip().lower().split()
    if tokens[0] == 'quit':
        break
    while tokens:
        if tokens[0] in aliases and tokens[0] != aliases[tokens[0]][0]:
            tokens = aliases[tokens[0]] + tokens[1:]
            continue
        
        try:
            getattr(world, tokens[0])(*tokens[1:])
        except AttributeError as ex:
            print('Command not found.')
        except TypeError as ex:
            print('Wrong number of args.')
            
        break
print("Thanks for playing!")

```

