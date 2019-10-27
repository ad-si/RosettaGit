+++
title = "RCRPG/Go"
description = ""
date = 2014-02-18T22:08:38Z
aliases = []
[extra]
id = 7439
[taxonomies]
categories = []
tags = []
+++

{{collection|RCRPG}}

```go
package main

import (
	"fmt"
	"io"
	"math/rand"
	"os"
	"sort"
	"strings"
)

const (
	VERSION_MAJOR = 1
	VERSION_MINOR = 0
)

type Direction int

const (
	NORTH = Direction(iota)
	SOUTH
	EAST
	WEST
	UP
	DOWN
	NUM_DIRECTIONS = iota
)

func (d Direction) Opposite() Direction {
	return d ^ 1
}

func (d Direction) String() string {
	switch d {
	case NORTH:
		return "north"
	case SOUTH:
		return "south"
	case EAST:
		return "east"
	case WEST:
		return "west"
	case UP:
		return "up"
	case DOWN:
		return "down"
	}
	panic("Invalid direction!")
}

type Item int

const (
	NIL_ITEM = Item(iota)
	GOLD
	LADDER
	SLEDGE
	NUM_ITEMS = iota
)

func (item Item) String() string {
	switch item {
	case GOLD:
		return "Gold coin"
	case LADDER:
		return "Ladder"
	case SLEDGE:
		return "Sledge hammer"
	}
	panic("Invalid item!")
}

type Room struct {
	X, Y, Z  int
	Name     string
	Passages [6]*Room
	Items    map[Item]int
}

// Creates a new room
func GenerateRoom(X, Y, Z int) *Room {
	room := new(Room)
	room.X, room.Y, room.Z = X, Y, Z
	room.Name = ""
	room.Items = make(map[Item]int)
	switch item := Item(rand.Intn(4)); item {
	case GOLD, LADDER, SLEDGE:
		room.Items[item]++
	}
	return room
}

// Connects this room to another
func (room *Room) Connect(dir Direction, other *Room) {
	room.Passages[dir] = other
	other.Passages[dir.Opposite()] = room
}

// The room name
func (room *Room) String() string {
	if len(room.Name) == 0 {
		return fmt.Sprintf("Room %d,%d,%d", room.X, room.Y, room.Z)
	}
	return room.Name
}

// Counts the number of items are laying in this room
func (room *Room) NumItems() int {
	total := 0
	for _, count := range room.Items {
		total += count
	}
	return total
}

// Counts the number of exits out of this room
func (room *Room) NumExits() int {
	total := 0
	for _, other := range room.Passages {
		if other != nil {
			total++
		}
	}
	return total
}

// Prints a description of the current room
func (room *Room) Describe() {
	fmt.Printf("You are at %s\n", room)
	if room.NumItems() > 0 {
		fmt.Print("On the ground you can see: ")
		first := true
		for item := NIL_ITEM; item < NUM_ITEMS; item++ {
			count := room.Items[item]
			if count <= 0 {
				continue
			}
			if !first {
				fmt.Print(", ")
			}
			first = false
			if count == 1 {
				fmt.Printf("a %s", item)
			} else {
				fmt.Printf("%d %ss", count, item)
			}
		}
		fmt.Println()
	}
	if room.NumExits() == 0 {
		fmt.Print("There are no exits.\n")
	} else {
		fmt.Print("Exits are:\n")
		for dir := Direction(0); dir < NUM_DIRECTIONS; dir++ {
			other := room.Passages[dir]
			if other != nil {
				fmt.Printf("  %s: %s\n", dir, other)
			}
		}
	}
}

type Game struct {
	// Three level mapping of the rooms
	Rooms map[int]map[int]map[int]*Room
	// Number of items of each type
	Inventory map[Item]int
	// Currently equipped item
	Equipped Item
	// Current room
	Room *Room
	// All commands
	Commands map[string]func(args []string)
	// All aliases
	Aliases map[string][]string
	// Help texts
	HelpText map[string]string
}

// Counts the number of items you have in your inventory
func (g *Game) NumItems() int {
	total := 0
	for _, count := range g.Inventory {
		total += count
	}
	return total
}

// Creates or returns the room at specified coordinates.
func (g *Game) GetRoomAt(X, Y, Z int) *Room {
	if g.Rooms == nil {
		g.Rooms = make(map[int]map[int]map[int]*Room)
	}
	if g.Rooms[X] == nil {
		g.Rooms[X] = make(map[int]map[int]*Room)
	}
	if g.Rooms[X][Y] == nil {
		g.Rooms[X][Y] = make(map[int]*Room)
	}
	room := g.Rooms[X][Y][Z]
	if room == nil {
		room = GenerateRoom(X, Y, Z)
		g.Rooms[X][Y][Z] = room
	}
	return room
}

// north, south, east, west, up, down: Moves around
func (g *Game) Move(args []string) {
	if len(args) != 1 {
		fmt.Print("Invalid number of arguments\n")
		return
	}
	var dir Direction
	switch strings.ToLower(args[0]) {
	default:
		fmt.Printf("Invalid direction: %q\n", args[0])
		return
	case "north":
		dir = NORTH
	case "south":
		dir = SOUTH
	case "east":
		dir = EAST
	case "west":
		dir = WEST
	case "up":
		dir = UP
	case "down":
		dir = DOWN
	}
	other := g.Room.Passages[dir]
	if other == nil {
		fmt.Printf("You can't move %s. There is a wall in the way.\n", dir)
	} else if dir == UP && g.Room.Items[LADDER] == 0 {
		fmt.Printf("You can't move %s. There are no %s.\n", dir, LADDER)
	} else {
		g.Room = other
		other.Describe()
	}
}

// attack: Breaks a wall.
func (g *Game) Sledge(args []string) {
	if len(args) != 1 {
		fmt.Print("Invalid number of arguments\n")
		return
	}
	if g.Equipped != SLEDGE {
		fmt.Printf("You need a %s equipped to break the wall.\n", SLEDGE)
		return
	}
	X, Y, Z := g.Room.X, g.Room.Y, g.Room.Z
	var dir Direction
	switch strings.ToLower(args[0]) {
	default:
		fmt.Printf("Invalid direction: %q\n", args[0])
		return
	case "north":
		dir = NORTH
		Y--
	case "south":
		dir = SOUTH
		Y++
	case "east":
		dir = EAST
		X++
	case "west":
		dir = WEST
		X--
	case "up":
		dir = UP
		Z++
	case "down":
		dir = DOWN
		Z--
	}
	other := g.Room.Passages[dir]
	if other != nil {
		fmt.Print("There is already a passage there.\n")
		return
	}
	other = g.GetRoomAt(X, Y, Z)
	g.Room.Connect(dir, other)
	fmt.Printf("Made a passage %s to %s\n", dir, other)
}

// drop: Puts items down
func (g *Game) DropItem(args []string) {
	if len(args) != 1 {
		fmt.Print("Invalid number of arguments\n")
		return
	}
	var item Item
	switch strings.ToLower(args[0]) {
	default:
		fmt.Printf("You don't have any %q.\n", args[0])
		return
	case "all":
		if g.NumItems() == 0 {
			fmt.Print("You don't have any items.\n")
			return
		}
		fmt.Print("You drop: ")
		first := true
		for item := NIL_ITEM; item < NUM_ITEMS; item++ {
			count := g.Inventory[item]
			if count <= 0 {
				continue
			}
			if !first {
				fmt.Print(", ")
			}
			first = false
			if count == 1 {
				fmt.Printf("a %s", item)
			} else {
				fmt.Printf("%d %ss", count, item)
			}
			g.Inventory[item] = 0
			g.Room.Items[item] += count
		}
		fmt.Println()
		return
	case "gold":
		item = GOLD
	case "ladder":
		item = LADDER
	case "sledge":
		item = SLEDGE
	}
	total := g.Inventory[item]
	if total == 0 {
		fmt.Printf("You don't have any %ss.\n", item)
		return
	}
	g.Inventory[item]--
	g.Room.Items[item]++
	fmt.Printf("You drop a %s.\n", item)
}

// take: Picks up items
func (g *Game) TakeItem(args []string) {
	if len(args) != 1 {
		fmt.Print("Invalid number of arguments\n")
		return
	}
	var item Item
	switch strings.ToLower(args[0]) {
	default:
		fmt.Printf("You don't see any %q.\n", args[0])
		return
	case "all":
		if g.Room.NumItems() == 0 {
			fmt.Print("You don't see any items.\n")
			return
		}
		fmt.Print("You take: ")
		first := true
		for item := NIL_ITEM; item < NUM_ITEMS; item++ {
			count := g.Room.Items[item]
			if count <= 0 {
				continue
			}
			if !first {
				fmt.Print(", ")
			}
			first = false
			if count == 1 {
				fmt.Printf("a %s", item)
			} else {
				fmt.Printf("%d %ss", count, item)
			}
			g.Room.Items[item] = 0
			g.Inventory[item] += count
		}
		fmt.Println()
		return
	case "gold":
		item = GOLD
	case "ladder":
		item = LADDER
	case "sledge":
		item = SLEDGE
	}
	total := g.Room.Items[item]
	if total == 0 {
		fmt.Printf("You don't see any %ss.\n", item)
		return
	}
	g.Room.Items[item]--
	g.Inventory[item]++
	fmt.Printf("You take a %s.\n", item)
}

// inventory: Shows what you are carrying
func (g *Game) ShowInventory(args []string) {
	if len(args) != 0 {
		fmt.Print("Invalid number of arguments\n")
		return
	}
	if g.Equipped != NIL_ITEM {
		fmt.Printf("You have a %s equipped.\n", g.Equipped)
	}
	if g.NumItems() == 0 {
		fmt.Print("You are not carrying anything.\n")
		return
	}
	fmt.Print("You are carrying: ")
	first := true
	for item := NIL_ITEM; item < NUM_ITEMS; item++ {
		count := g.Inventory[item]
		if count <= 0 {
			continue
		}
		if !first {
			fmt.Print(", ")
		}
		first = false
		if count == 1 {
			fmt.Printf("a %s", item)
		} else {
			fmt.Printf("%d %ss", count, item)
		}
	}
	fmt.Println()
}

// name: Renames the current room
func (g *Game) RenameRoom(args []string) {
	if len(args) != 1 {
		fmt.Print("Invalid number of arguments\n")
		return
	}
	g.Room.Name = args[0]
	fmt.Printf("This room is now called %q\n", args[0])
	g.Room.Describe()
}

// equip: Equips an item
func (g *Game) EquipItem(args []string) {
	if len(args) != 1 {
		fmt.Print("Invalid number of arguments\n")
		return
	}
	var item Item
	switch strings.ToLower(args[0]) {
	default:
		fmt.Printf("You don't have any %q.\n", args[0])
		return
	case "gold":
		item = GOLD
	case "ladder":
		item = LADDER
	case "sledge":
		item = SLEDGE
	}
	count := g.Inventory[item]
	if count <= 0 {
		fmt.Printf("You don't have any %ss.\n", item)
		return
	} else if item == g.Equipped {
		fmt.Printf("You already have a %s equipped.\n", item)
		return
	}
	if g.Equipped != NIL_ITEM {
		fmt.Printf("You unequip the %s. ", g.Equipped)
		g.Inventory[g.Equipped]++
	}
	fmt.Printf("You equip a %s.\n", item)
	g.Inventory[item]--
	g.Equipped = item
}

// alias: Creats an alias
func (g *Game) CreateAlias(args []string) {
	if len(args) < 1 {
		fmt.Print("Invalid number of arguments\n")
		return
	}
	name := strings.ToLower(args[0])
	switch name {
	case "north", "south", "east", "west", "up", "down", "attack",
		"drop", "take", "inventory", "name", "equip", "alias":
		fmt.Printf("Can't overwrite the %q command.\n", name)
		return
	}
	if len(args) == 1 {
		g.Commands[name] = nil
		fmt.Printf("Removed the %q alias.\n", name)
		return
	}
	g.Aliases[name] = args[1:]
	fmt.Printf("Created the %q alias.\n", name)
}

// help: Shows information about commands
func (g *Game) Help(args []string) {
	if len(args) == 0 {
		fmt.Print("Commands:\n")
		for _, cmd := range g.GetCommands() {
			fmt.Printf("  %s\n", cmd)
		}
	} else if len(args) == 1 {
		name := args[0]
		arr, ok := g.Aliases[name]
		if ok {
			fmt.Printf("Alias for: %q\n", strings.Join(arr, " "))
			return
		}
		_, ok = g.Commands[name]
		if ok {
			fmt.Printf("Command: %s\n", name)
			descr, ok := g.HelpText[name]
			if ok {
				fmt.Printf("  %s\n", descr)
			}
			return
		}
		fmt.Printf("Unknown command: %q\n", name)
	} else if len(args) > 1 {
		fmt.Print("Invalid number of arguments\n")
	}
}

// look: Looks around
func (g *Game) Look(args []string) {
	if len(args) != 0 {
		fmt.Print("Invalid number of arguments\n")
		return
	}
	g.Room.Describe()
}

// quit: Ends the game. The End. Final.
func (g *Game) Quit(args []string) {
	if len(args) != 0 {
		fmt.Print("Invalid number of arguments\n")
		return
	}
	os.Exit(0)
}

// Resolves aliases and dispatches to the correct handler
func (g *Game) Dispatch(args []string) {
	seen := make(map[string]bool)
	name := args[0]
	for {
		seen[name] = true
		prefix, ok := g.Aliases[name]
		if !ok {
			break
		}
		args = append(prefix, args[1:]...)
		name = args[0]
		if seen[name] {
			fmt.Print("Recursive alias.\n")
			return
		}
	}
	cb := g.Commands[name]
	if cb == nil {
		fmt.Printf("Invalid command: %q\n", name)
		return
	}
	cb(args[1:])
	return
}

// Set everything up
func (g *Game) Initialize() *Game {
	startRoom := g.GetRoomAt(0, 0, 0)
	startRoom.Name = "Start room"
	startRoom.Items[SLEDGE] = 1
	goalRoom := g.GetRoomAt(1, 1, 5)
	goalRoom.Name = "Prize room"
	g.Room = startRoom
	g.Inventory = make(map[Item]int)
	g.Commands = map[string]func(args []string){
		"move":      func(args []string) { g.Move(args) },
		"attack":    func(args []string) { g.Sledge(args) },
		"drop":      func(args []string) { g.DropItem(args) },
		"take":      func(args []string) { g.TakeItem(args) },
		"inventory": func(args []string) { g.ShowInventory(args) },
		"name":      func(args []string) { g.RenameRoom(args) },
		"equip":     func(args []string) { g.EquipItem(args) },
		"alias":     func(args []string) { g.CreateAlias(args) },
		"quit":      func(args []string) { g.Quit(args) },
		"look":      func(args []string) { g.Look(args) },
		"help":      func(args []string) { g.Help(args) },
	}
	g.Aliases = map[string][]string{
		"north": []string{"move", "north"},
		"south": []string{"move", "south"},
		"east":  []string{"move", "east"},
		"west":  []string{"move", "west"},
		"up":    []string{"move", "up"},
		"down":  []string{"move", "down"},
		"n":     []string{"north"},
		"s":     []string{"south"},
		"e":     []string{"east"},
		"w":     []string{"west"},
		"u":     []string{"up"},
		"d":     []string{"down"},
		"i":     []string{"inventory"},
		"inv":   []string{"inventory"},
		"a":     []string{"attack"},
	}
	g.HelpText = map[string]string{
		"move":      "Moves you through passages.",
		"attack":    "Attacks a wall with the equipped Sledge hammer.",
		"drop":      "Drops items.",
		"take":      "Picks up items.",
		"inventory": "Shows your inventory.",
		"name":      "Rename a room.",
		"equip":     "Equip an item.",
		"alias":     "Creates an alias.",
		"quit":      "Exits the game.",
		"look":      "Describes the room.",
		"help":      "Help about commands.",
	}
	return g
}

// Read a single line of input
func readLine(in io.Reader) (string, bool) {
	var line []string
	buf := []byte{0}
	_, err := in.Read(buf)
	for err == nil && buf[0] != '\n' && buf[0] != '\r' {
		line = append(line, string(buf))
		_, err = in.Read(buf)
	}
	if buf[0] == '\r' {
		in.Read(buf)
	}
	return strings.Join(line, ""), err == nil
}

// Returns a sorted array of command and alias names
func (g *Game) GetCommands() []string {
	var commandNames []string
	for name := range g.Commands {
		commandNames = append(commandNames, name)
	}
	for name := range g.Aliases {
		commandNames = append(commandNames, name)
	}
	sort.Strings(commandNames)
	return commandNames
}

// Main game loop
func (g *Game) Run() {
	fmt.Printf("Welcome to RC Minimalist RPG, Go version %d.%d.\n",
		VERSION_MAJOR, VERSION_MINOR)
	fmt.Print("You start in room 0,0,0. Your goal is at 1,1,5. Good luck!\n")
	fmt.Printf("Commands: %s\n", strings.Join(g.GetCommands(), ", "))
	g.Room.Describe()
	fmt.Print("\n> ")
	line, ok := readLine(os.Stdin)
	for ok && len(line) == 0 {
		fmt.Print("\n> ")
		line, ok = readLine(os.Stdin)
	}
	for ok {
		tokens := strings.Split(line, " ")
		g.Dispatch(tokens)
		fmt.Print("\n> ")
		line, ok = readLine(os.Stdin)
		for ok && len(line) == 0 {
			fmt.Print("\n> ")
			line, ok = readLine(os.Stdin)
		}
	}
}

func main() {
	g := new(Game).Initialize()
	g.Run()
}
```

