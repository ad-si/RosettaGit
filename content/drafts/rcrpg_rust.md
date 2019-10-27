+++
title = "RCRPG/Rust"
description = ""
date = 2019-09-06T12:54:37Z
aliases = []
[extra]
id = 22516
[taxonomies]
categories = []
tags = []
+++

{{collection|RCRPG}}
[[Rust]] version of [[:Category:RCRPG|RCRPG]].

==Code==

```rust

//! Implementation of the simple text-based game [RCRPG](https://web.archive.org/web/20080212201605/http://shortcircuit.us/muddy-kinda-like-a-mud-but-single-player/)
//! in Rust

use rand::prelude::*;
use std::borrow::BorrowMut;
use std::collections::{HashMap, HashSet};
use std::fmt::{Debug, Display};
use std::iter::FromIterator;
use std::ops::Add;
use std::{fmt, io};

/// Maps each Locations to a direction
const DIRECTION_MAPPING: [(Location, Direction); 6] = [
    (Location(0, -1, 0), Direction::North),
    (Location(0, 1, 0), Direction::South),
    (Location(-1, 0, 0), Direction::West),
    (Location(1, 0, 0), Direction::East),
    (Location(0, 0, 1), Direction::Down),
    (Location(0, 0, -1), Direction::Up),
];

/// Objects possessed by the player
type Inventory = HashSet<Object>;
/// Maps the (possibly user-defined) aliases to their actual action, so that for instance a player
/// can input either `n` or `north` to go North, and can also define new aliases
type CommandAliases = Vec<(HashSet<String>, Command)>;

/// 3D coordinates of objects in the dungeon
#[derive(Hash, Eq, PartialEq, Copy, Clone)]
struct Location(i32, i32, i32);

impl Add for Location {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        Location(self.0 + rhs.0, self.1 + rhs.1, self.2 + rhs.2)
    }
}

impl Debug for Location {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "({}, {}, {})", self.0, self.1, self.2)
    }
}

/// Objects that can be found in the dungon rooms
#[derive(Eq, PartialEq, Hash, Copy, Clone, Debug)]
enum Object {
    Ladder,
    Sledge,
    Gold,
}

impl Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        match *self {
            Object::Ladder => write!(f, "a ladder"),
            Object::Sledge => write!(f, "a sledge"),
            Object::Gold => write!(f, "some gold"),
        }
    }
}

impl Object {
    /// Tries to parse a string to an object, like `"gold"` to `Object::Gold`
    fn from_string(s: &str) -> Option<Object> {
        match s {
            "ladder" => Some(Object::Ladder),
            "sledge" => Some(Object::Sledge),
            "gold" => Some(Object::Gold),
            _ => None,
        }
    }
}

/// Player information
struct Player {
    /// Room where the player currently is
    location: Location,
    /// The objects carried by the player
    inventory: Inventory,
    /// The object wieled by the player, if any
    equipped: Option<Object>,
}

/// Information about each room of the dungeon
struct Room {
    /// Fixed description for special rooms (like the first one or the prize room)
    description: Option<String>,
    /// Objects currently in the room
    objects: Inventory,
}

impl Room {
    fn new() -> Self {
        Room {
            description: None,
            objects: HashSet::new(),
        }
    }

    /// Sets the room description
    fn with_description(mut self, description: &str) -> Self {
        self.description = Some(description.to_string());
        self
    }

    /// Sets the objects in the room
    fn with_objects(mut self, objects: Vec<Object>) -> Self {
        self.objects.extend(objects);
        self
    }

    /// Adds some randoms objects to the room
    fn with_random_objects(mut self, rng: &mut ThreadRng) -> Self {
        let objects: Vec<_> = vec![
            if rng.gen::<f32>() < 0.33 {
                Some(Object::Sledge)
            } else {
                None
            },
            if rng.gen::<f32>() < 0.33 {
                Some(Object::Ladder)
            } else {
                None
            },
            if rng.gen::<f32>() < 0.33 {
                Some(Object::Gold)
            } else {
                None
            },
        ]
        .iter()
        .filter_map(|o| *o)
        .collect();

        self.objects.extend(objects);
        self
    }
}

/// Cardinat directions
#[derive(Copy, Clone, Eq, PartialEq)]
enum Direction {
    North,
    South,
    West,
    East,
    Down,
    Up,
}

impl Display for Direction {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        match *self {
            Direction::North => write!(f, "north"),
            Direction::South => write!(f, "south"),
            Direction::West => write!(f, "west"),
            Direction::East => write!(f, "east"),
            Direction::Down => write!(f, "down"),
            Direction::Up => write!(f, "up"),
        }
    }
}

impl Direction {
    /// Tries to parse a string to a direction, like `"north"` to `Direction::North`
    fn from_string(s: &str) -> Option<Direction> {
        match s {
            "north" => Some(Direction::North),
            "south" => Some(Direction::South),
            "west" => Some(Direction::West),
            "east" => Some(Direction::East),
            "down" => Some(Direction::Down),
            "up" => Some(Direction::Up),
            _ => None,
        }
    }

    /// Returns the normalized 3D point of the location, for instance `Direction::North` is
    /// `(0, -1, 0)` where `(x, y, z)`
    fn to_location(self) -> Location {
        DIRECTION_MAPPING.iter().find(|d| d.1 == self).unwrap().0
    }
}

/// Collection of rooms
struct Dungeon {
    /// The rooms that make up the dungeon
    rooms: HashMap<Location, Room>,
}

impl Dungeon {
    fn new() -> Self {
        Dungeon {
            rooms: HashMap::from_iter(vec![
                (
                    Location(0, 0, 0),
                    Room::new()
                        .with_description("The room where it all started...")
                        .with_objects(vec![Object::Ladder, Object::Sledge]),
                ),
                (
                    Location(1, 1, 5),
                    Room::new().with_description("You found it! Lots of gold!"),
                ),
            ]),
        }
    }

    /// Given a room location, returns the list of `Direction`s that lead to other rooms
    fn exits_for_room(&self, location: Location) -> Vec<Direction> {
        DIRECTION_MAPPING
            .iter()
            .filter_map(|d| {
                let location_to_test = location + d.0;

                if self.rooms.contains_key(&location_to_test) {
                    return Some(d.1);
                }
                None
            })
            .collect()
    }
}

/// Collection of all the available commands to interact to the dungeon world
#[derive(Debug, Copy, Clone)]
enum Command {
    North,
    South,
    West,
    East,
    Down,
    Up,
    Help,
    Dig,
    Look,
    Inventory,
    Take,
    Drop,
    Equip,
    Unequip,
    Alias,
}

/// Returns the list of all the default command aliases
fn default_aliases() -> CommandAliases {
    vec![
        (
            vec!["n".to_string(), "north".to_string()]
                .into_iter()
                .collect(),
            Command::North,
        ),
        (
            vec!["s".to_string(), "south".to_string()]
                .into_iter()
                .collect(),
            Command::South,
        ),
        (
            vec!["w".to_string(), "west".to_string()]
                .into_iter()
                .collect(),
            Command::West,
        ),
        (
            vec!["e".to_string(), "east".to_string()]
                .into_iter()
                .collect(),
            Command::East,
        ),
        (
            vec!["d".to_string(), "down".to_string()]
                .into_iter()
                .collect(),
            Command::Down,
        ),
        (
            vec!["u".to_string(), "up".to_string()]
                .into_iter()
                .collect(),
            Command::Up,
        ),
        (
            vec!["help".to_string()].into_iter().collect(),
            Command::Help,
        ),
        (vec!["dig".to_string()].into_iter().collect(), Command::Dig),
        (
            vec!["l".to_string(), "look".to_string()]
                .into_iter()
                .collect(),
            Command::Look,
        ),
        (
            vec!["i".to_string(), "inventory".to_string()]
                .into_iter()
                .collect(),
            Command::Inventory,
        ),
        (
            vec!["take".to_string()].into_iter().collect(),
            Command::Take,
        ),
        (
            vec!["drop".to_string()].into_iter().collect(),
            Command::Drop,
        ),
        (
            vec!["equip".to_string()].into_iter().collect(),
            Command::Equip,
        ),
        (
            vec!["unequip".to_string()].into_iter().collect(),
            Command::Unequip,
        ),
        (
            vec!["alias".to_string()].into_iter().collect(),
            Command::Alias,
        ),
    ]
}

/// Tries to parse a string to a command also taking into account the aliases
fn find_command(command: &str, aliases: &[(HashSet<String>, Command)]) -> Option<Command> {
    let command = command.to_lowercase();

    aliases.iter().find(|a| a.0.contains(&command)).map(|a| a.1)
}

/// Prints the help string
fn help() {
    println!(
        "You need a sledge to dig rooms and ladders to go upwards.
Valid commands are: directions (north, south...), dig, take, drop, equip, inventory and look.
Additionally you can tag rooms with the 'name' command and alias commands with 'alias'.
Have fun!"
    )
}

/// Defines a new alias for a command
fn alias(command_aliases: &mut CommandAliases, args: &[&str]) {
    if args.len() < 2 {
        println!("To assign an alias: alias CMQ NEW_ALIAS");
    } else {
        let command = args[0].to_lowercase();
        let new_alias = args[1].to_lowercase();

        let mut found = false;
        for ca in command_aliases {
            if ca.0.contains(&command) {
                ca.0.insert(new_alias.clone());
                found = true;
            }
        }

        if found {
            println!("You can use \"{}\" in lieu of \"{}\"", new_alias, command);
        } else {
            println!("The commands \"{}\" does not exist", command);
        }
    }
}

/// Describes the current rooom
fn look(player: &Player, dungeon: &Dungeon) {
    let room = &dungeon.rooms[&player.location];

    if let Some(description) = &room.description {
        print!("{}", description);
    } else {
        print!("Room at {:?}.", player.location);
    }

    if !room.objects.is_empty() {
        print!(
            " On the floor you can see: {}.",
            room.objects
                .iter()
                .map(|o| o.to_string())
                .collect::<Vec<String>>()
                .join(", ")
        );
    }

    let room_exits = dungeon.exits_for_room(player.location);
    match room_exits.len() {
        0 => println!(" There are no exits in this room."),
        1 => println!(" There is one exit: {}.", room_exits[0].to_string()),
        _ => println!(
            " Exits: {}.",
            room_exits
                .iter()
                .map(|o| o.to_string())
                .collect::<Vec<String>>()
                .join(", ")
        ),
    }
}

/// Grabs an object lying on the floor of a room and puts it into the player's inventory
fn take(player: &mut Player, dungeon: &mut Dungeon, args: &[&str]) {
    if args.is_empty() {
        println!("To take something: take OBJECT|all")
    } else if dungeon.rooms[&player.location].objects.is_empty() {
        println!("There is nothing to take here")
    } else if args[0] == "all" {
        let room_objects = dungeon
            .rooms
            .get_mut(&player.location)
            .expect("The player is in a room that should not exist!")
            .objects
            .borrow_mut();

        player.inventory.extend(room_objects.iter());
        room_objects.clear();

        println!("All items taken");
    } else if let Some(object) = Object::from_string(args[0]) {
        let room_objects = dungeon
            .rooms
            .get_mut(&player.location)
            .expect("The player is in a room that should not exist!")
            .objects
            .borrow_mut();

        if room_objects.contains(&object) {
            player.inventory.insert(object);
            room_objects.remove(&object);
            println!("Taken");
        }
    } else {
        println!("You can't see anything like that here")
    }
}

/// Removes an object from the player's inventory and leaves it lying on the current room's floor
fn drop(player: &mut Player, dungeon: &mut Dungeon, args: &[&str]) {
    if args.is_empty() {
        println!("To drop something: drop OBJECT|all")
    } else if player.inventory.is_empty() {
        println!("You are not carrying anything")
    } else if args[0] == "all" {
        let room_objects = dungeon
            .rooms
            .get_mut(&player.location)
            .expect("The player is in a room that should not exist!")
            .objects
            .borrow_mut();

        room_objects.extend(player.inventory.iter());
        player.inventory.clear();

        println!("All items dropped");
    } else if let Some(object) = Object::from_string(args[0]) {
        let room_objects = dungeon
            .rooms
            .get_mut(&player.location)
            .expect("The player is in a room that should not exist!")
            .objects
            .borrow_mut();

        if player.inventory.contains(&object) {
            player.inventory.remove(&object);
            room_objects.insert(object);
            println!("Dropped");
        }
    } else {
        println!("You don't have anything like that")
    }
}

/// Prints the list of object currently carries by the player
fn inventory(player: &Player) {
    if player.inventory.is_empty() {
        println!("You are not carrying anything")
    } else {
        println!(
            "You are carrying: {}",
            player
                .inventory
                .iter()
                .map(|o| o.to_string())
                .collect::<Vec<String>>()
                .join(", ")
        );
    }
}

/// Digs a tunnel to a new room connected to the current one
#[allow(clippy::map_entry)]
fn dig(player: &Player, dungeon: &mut Dungeon, rng: &mut ThreadRng, args: &[&str]) {
    if args.is_empty() {
        println!("To dig a tunnel: dig DIRECTION");
    } else if let Some(direction) = Direction::from_string(args[0]) {
        if let Some(equipped) = player.equipped {
            if equipped == Object::Sledge {
                let target_location = player.location + direction.to_location();

                if dungeon.rooms.contains_key(&target_location) {
                    println!("There is already an exit, there!");
                }

                dungeon.rooms.entry(target_location).or_insert_with(|| {
                    println!("There is now an exit {}ward", direction);

                    Room::new().with_random_objects(rng)
                });
            } else {
                println!("You cannot dig with {}", equipped);
            }
        } else {
            println!("With your bare hands?");
        }
    } else {
        println!("That is not a direction I recognize");
    }
}

/// Moves the player to an adjacent room
fn goto(player: &mut Player, dungeon: &Dungeon, direction: Direction) {
    if direction == Direction::North
        && !dungeon.rooms[&player.location]
            .objects
            .contains(&Object::Ladder)
    {
        println!("You can't go upwards without a ladder!");
    } else {
        let target_location = player.location + direction.to_location();
        if !dungeon.rooms.contains_key(&target_location) {
            println!("There's no exit in that direction!");
        } else {
            player.location = target_location;
            look(player, dungeon);
        }
    }
}

/// Equips an object
fn equip(player: &mut Player, args: &[&str]) {
    if args.is_empty() {
        println!("To equip something: equip OBJECT");
    } else if let Some(object) = Object::from_string(args[0]) {
        if player.inventory.contains(&object) {
            player.equipped = Some(object);
            println!("Item equipped");
        } else {
            println!("You don't have such object");
        }
    } else {
        println!("You don't have such object");
    }
}

/// Unequips an object
fn unequip(player: &mut Player) {
    if player.equipped.is_some() {
        player.equipped = None;
        println!("Unequipped");
    } else {
        println!("You are already not using anything");
    }
}

/// Main game loop
fn main() {
    let mut command_aliases = default_aliases();
    let mut dungeon = Dungeon::new();
    let mut player = Player {
        location: Location(0, 0, 0),
        inventory: HashSet::from_iter(vec![Object::Sledge]),
        equipped: None,
    };
    let mut rng = rand::thread_rng();

    // init
    println!("Grab the sledge and make your way to room 1,1,5 for a non-existant prize!\n");
    help();

    loop {
        let mut input = String::new();
        io::stdin()
            .read_line(&mut input)
            .expect("Cannot read from stdin");
        let input: &str = &input.trim().to_lowercase();

        let splitted = input.split_whitespace().collect::<Vec<&str>>();

        if !splitted.is_empty() {
            match find_command(splitted[0], &command_aliases) {
                Some(Command::Help) => help(),
                Some(Command::Alias) => alias(&mut command_aliases, &splitted[1..]),
                Some(Command::Look) => look(&player, &dungeon),
                Some(Command::Take) => take(&mut player, &mut dungeon, &splitted[1..]),
                Some(Command::Drop) => drop(&mut player, &mut dungeon, &splitted[1..]),
                Some(Command::Inventory) => inventory(&player),
                Some(Command::Dig) => dig(&player, &mut dungeon, &mut rng, &splitted[1..]),
                Some(Command::Equip) => equip(&mut player, &splitted[1..]),
                Some(Command::Unequip) => unequip(&mut player),
                Some(Command::North) => goto(&mut player, &dungeon, Direction::North),
                Some(Command::South) => goto(&mut player, &dungeon, Direction::South),
                Some(Command::West) => goto(&mut player, &dungeon, Direction::West),
                Some(Command::East) => goto(&mut player, &dungeon, Direction::East),
                Some(Command::Down) => goto(&mut player, &dungeon, Direction::Down),
                Some(Command::Up) => goto(&mut player, &dungeon, Direction::Up),
                _ => println!("I don't know what you mean."),
            }
        }
    }
}
```

