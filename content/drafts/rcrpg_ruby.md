+++
title = "RCRPG/Ruby"
description = ""
date = 2010-04-28T04:47:45Z
aliases = []
[extra]
id = 7154
[taxonomies]
categories = []
tags = []
+++

{{collection|RCRPG}}
This [[Ruby]] version of [[RCRPG]] implements a text interface.

==Language Idioms==

This program illustrates some of the interesting aspects of Ruby:
* object-oriented programming
* open classes
* regular expression matching
* built-in data structures: hashes and arrays
* higher-order functions like map and select, using blocks

== Objective ==
The objective of the game is to find your way to the exit (which is located at 1, 1, 5).

==Commands==

Direction commands:
```txt
north, south, east, west, up, down
```
Type these in order to go in a certain direction. There must be a hole in the wall in that direction. All directions are aliased by their first letter (ie. you can go south by just typing s). In order to go up, you must have a ladder in your current room. In order to go down, the room below you must have a ladder in it.

Attack:
```txt
attack &lt;direction&gt;
```
Break a hole in the wall in the specified direction. You must have a sledge equipped to do this.

Take/Drop:
```txt
take &lt;item&gt;
drop &lt;item&gt;
```
Pick up an item in the room, or drop an item from your inventory.

View Inventory:
```txt
inventory
```


Equip an item:
```txt
equip &lt;item&gt;
```


Create an alias:
```txt
alias &lt;old command&gt; &lt;new command&gt;
```
Make it so that whenever you type <i>new command</i> it means the same thing as <i>old command</i>.

==Code==

```ruby
require 'matrix'

class Vector
  # add a pretty method to Vector for serialization
  def pretty
    @elements.join(",")
  end
end

class Room
  attr_accessor :name, :items, :links

  def initialize name, items, *links
    @name, @items, @links = name, items, links
  end
end

Directions = {
  "north" => Vector[ 0,  1,  0],
  "south" => Vector[ 0, -1,  0],
  "east"  => Vector[ 1,  0,  0],
  "west"  => Vector[-1,  0,  0],
  "up"    => Vector[ 0,  0,  1],
  "down"  => Vector[ 0,  0, -1]
}

Items = [:sledge, :ladder, :gold]

$aliases = {
  "n" => "north",
  "s" => "south",
  "e" => "east",
  "w" => "west",
  "u" => "up",
  "d" => "down",
  "i" => "inventory",
  "inv" => "inventory",
  "a" => "attack",
  "q" => "quit"
}

$player = {
  :inventory => {
    "ladder" => 0,
    "sledge" => 0,
    "gold" => 0
  },
  :equipped => nil,
  :position => Vector[0, 0, 0]
}

$rooms = {
  "0,0,0" => Room.new("start", [:sledge])
}

# creates a new room in the world
def create_room where, from
  random_items = Items.select { rand > 0.75 }

  $rooms[where] = Room.new(where, random_items, from)
  $rooms[from].links << where
end

# returns the room that the player is currently in
def current_room
  $rooms[$player[:position].pretty]
end

# action - displays the characters inventory
def inventory
  puts "You have:"
  non_empty = $player[:inventory].select { |obj, q| q > 0 }

  if non_empty.empty?
    puts "- nothing."
  else
    non_empty.each do |item, quantity|
      if quantity > 1
        puts "- #{item} x #{quantity}"
      else
        puts "- #{item}"
      end
    end
  end
  
  if $player[:equipped]
    puts "Equipped: #{$player[:equipped]}"
  end
end

# action - move in a certain direction
def move dir
  if !Directions.key?(dir)
    puts "Unknown direction '#{dir}'."
    return
  end
  new_pos = $player[:position] + Directions[dir]

  target_room  = $rooms[new_pos.pretty]

  if !current_room.links.include?(new_pos.pretty)
    puts "You can't go that way."
  elsif dir == "up" and !current_room.items.include?(:ladder)
    puts "You can't go up without a ladder in the room."
  elsif dir == "down" and !target_room.items.include?(:ladder)
    puts "You can't go down that way, there is no ladder in that room."
  else
    $player[:position] = new_pos
    if new_pos.pretty == "1,1,5"
      puts "You win!"
      exit
    end
  end
end

# action - pick up an item off the floor
def take what
  if current_room.items.include?(what.to_sym)
    $player[:inventory][what] += 1
    current_room.items.delete what.to_sym
    puts "Picked up the #{what}."
  else
    puts "There is no #{what} in this room."
  end
end

# action - equip something
def equip what
  what = what.to_s.downcase
  if !%w[sledge].include?(what)
    puts "You can't equip a #{what}."
  elsif $player[:inventory].key?(what) and $player[:inventory][what] > 0
    puts "You have equipped the #{what}."
    $player[:equipped] = what
  else
    puts "You don't have any #{what}s."
  end
end

# action - attack in a certain direction
def attack dir
  if !Directions.key?(dir)
    puts "Unknown direction '#{dir}'."
    return
  end
  target_pos = ($player[:position] + Directions[dir]).pretty

  if $player[:equipped] != "sledge"
    puts "You must equip a sledge to attack."
  elsif current_room.links.include?(target_pos)
    puts "There is already a pathway in that direction."
  else
    puts "You create a path #{dir}."

    if $rooms.key?(target_pos)
      $rooms[target_pos].links << $player[:position].pretty
      current_room.links << target_pos
    else
      create_room target_pos, $player[:position].pretty
    end

    # see if sledge breaks
    if rand < 0.1
      puts "Your sledge has broken."
      $player[:inventory]["sledge"] -= 1
      $player[:equipped] = nil
    end
  end
end

# action - drop an item
def drop what
  what = what.to_s.downcase
  if !%w[sledge ladder gold].include?(what)
    puts "You can't drop a #{what}."
  elsif $player[:inventory].key?(what) and $player[:inventory][what] > 0
    puts "You have dropped a #{what}."
    $player[:inventory][what] -= 1
    current_room.items << what.to_sym
  else
    puts "You don't have any #{what}s."
  end
end

# this is the main routine for processing a command typed into the game
def process_cmd cmd
  case cmd
  when "exit", "quit"
    exit
  when /^(#{Directions.keys.join "|"})$/
    move $1
  when /^attack (#{Directions.keys.join "|"})$/
    attack $1
  when /^drop ([\w\s]+)$/
    drop $1
  when /^take ([\w\s]+)$/
    take $1
  when "inventory"
    inventory
  when /^equip ([\w\s]+)$/
    equip $1
  when /^alias (\w+) (\w+)$/
    $aliases[$2] = $1
  else
    if $aliases.key? cmd[/^\w+/]
      process_cmd cmd.sub($~.to_s, $aliases[$~.to_s])
    else
      puts "Invalid command: '#{cmd}'"
    end
  end
end

# displays info about the room we are in
def display_info
  puts "You are at (#{$player[:position].pretty})."

  room = $rooms[$player[:position].pretty]

  if room.items.size > 0
    puts "You see: #{room.items.map(&:to_s).join(", ")}."
  end

  if room.links.size > 0
    puts "There are openings " + room.links.map { |link|
      link = Vector[*link.split(",").map(&:to_i)]
      Directions.index(link - $player[:position])
    }.join(", ") + "."
  end
end

# the main game loop
while true
  display_info
  print "> "
  process_cmd gets.chop.downcase
end
```

