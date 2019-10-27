+++
title = "RCRPG/C++11"
description = ""
date = 2015-05-07T08:39:53Z
aliases = []
[extra]
id = 17451
[taxonomies]
categories = []
tags = []
+++

{{collection|RCRPG}}
[[C++11]] version of [[:Category:RCRPG|RCRPG]]. The code can also be checked out and contributed to on [https://github.com/pistacchio/rosettacode.clojure.rcrpg github] .

==Code==

```cpp

//
//  main.cpp
//  RCRPGCpp
//
//  Created by pistacchio on 19/03/14.
//
//

#include <algorithm>
#include <iostream>
#include <map>
#include <set>
#include <string>
#include <random>
#include <regex>
#include <vector>

// Initialize the randomization engine
std::default_random_engine rnd_eng(std::random_device{}());

//////////////////
// String utils //
//////////////////

// Concats string in a nice readable format adding "," and "and"
std::string descriptive_join(const std::vector<std::string>& items) {
    std::string desc;
    std::vector<std::string> itms {items.begin(), items.end()};
    int items_size {(int)items.size()};
    
    if (items_size > 1) {
        for (int i {0}; i < items_size - 1; i++) {
            itms.insert(itms.begin() + i * 2 + 1, i == items_size -2 ? " and " : ", ");
        }
    }
    
    std::for_each(itms.begin(), itms.end(), [&](std::string s) {
        desc += s;
    });
    
    return desc;
}

//////////
// Item //
//////////

enum Item {LADDER, SLEDGE, GOLD};
static const std::map<Item, std::string> Item_strings {
    {LADDER, "a ladder"},
    {SLEDGE, "a sledge"},
    {GOLD,   "some gold"}
};
static const std::map<std::string, Item> String_items {
    {"ladder",   LADDER},
    {"sledge", SLEDGE},
    {"gold",   GOLD}
};

/////////////
// Aliases //
/////////////

std::map<std::string, std::string> Aliases {
    {"i", "inventory"},
    {"n", "north"},
    {"e", "east"},
    {"w", "west"},
    {"s", "south"},
    {"get", "take"},
    {"u", "up"},
    {"l", "look"},
    {"d", "down"}
};
std::string unalias(std::string term) {
    if (Aliases.find(term) == Aliases.end()) {
        return term;
    } else {
        return Aliases.find(term)->second;
    }
}
void list_aliases() {
    std::cout << "Current aliases are:" << std::endl;
    for_each(Aliases.begin(), Aliases.end(), [](std::pair<std::string, std::string> alias){
        std::cout << alias.first << " -> " << alias.second << std::endl;
    });
}
void add_alias(std::string alias, std::string command) {
    if (Aliases.find(alias) == Aliases.end()) {
        Aliases[alias] = command;
    }

}

//////////////
// Location //
//////////////

struct Location {
    int x;
    int y;
    int z;
    
    Location (int xval, int yval, int zval): x(xval), y(yval), z(zval) {}

    Location& operator+= (Location const& other) {
        *this = *this + other;
        return *this;
    }
    Location operator+ (const Location& other) {
        return Location(this->x + other.x, this->y + other.y, this->z + other.z);
    }
};
bool operator< (const Location& left,  const Location& right) {
    return std::to_string(left.x) + '-' + std::to_string(left.y) + '-' + std::to_string(left.z) <
    std::to_string(right.x) + '-' + std::to_string(right.y) + '-' + std::to_string(right.z);
}

///////////////
// Direction //
///////////////

static const std::map<std::string, Location> Direction {
    {"north", Location( 0, -1,  0)},
    {"south", Location( 0,  1,  0)},
    {"east",  Location(-1,  0,  0)},
    {"west",  Location( 1,  0,  0)},
    {"up",    Location( 0,  0, -1)},
    {"down",  Location( 0,  0,  1)}
};

//////////////////////////////
// Interface Item Container //
//////////////////////////////

// Inherited by Player and Room, both need a way to contain items
class IItemContainer {
public:
    bool has_item(const Item item) const {
        return items.find(item) != items.end();
    }
    bool add_item(Item item) {
        if (items.find(item) == items.end()) {
            items.insert(item);
            return true;
        } else {
            return false;
        }
    }
    std::set<Item> add_all_items(std::set<Item> new_items) {
        items.insert(new_items.begin(), new_items.end());
        return items;
    }
    bool remove_item(Item item) {
        if (items.find(item) == items.end()) {
            return false;
        } else {
            items.erase(item);
            return true;
        }
    }
    std::set<Item> remove_all_items() {
        std::set<Item> removed_items {items.begin(), items.end()};
        items = std::set<Item> {};
        return removed_items;
    }
protected:
    std::set<Item> items;
};


//////////
// Room //
//////////

class Room : public IItemContainer {
public:
    Room () = default;
    Room (std::string description): description(description) {
        std::uniform_int_distribution<int> gen(0, 2);

        if (gen(rnd_eng) == 0) {
            items.insert(LADDER);
        }
        if (gen(rnd_eng) == 0) {
            items.insert(SLEDGE);
        }
        if (gen(rnd_eng) == 0) {
            items.insert(GOLD);
        }
    }
    const std::string describe (std::vector<std::string> adjacent_directions) const {
        std::string items_on_floor;
        std::string exits;
        
        if (items.size() > 0) {
            items_on_floor = "\nOn the floor you can see " + describe_items();
        }
        
        if (adjacent_directions.size() == 1) {
            exits = "There is one exit: " + adjacent_directions.front();
        } else if (adjacent_directions.size() > 1) {
            exits = "\nYou can see the following directions: ";

            exits += descriptive_join(adjacent_directions) + ".";
        }
        
        return description + items_on_floor + exits + "\n";
    }
private:
    std::string    description;
    std::string describe_items () const {
        std::vector<std::string> itms;
        std::for_each(items.begin(), items.end(), [&](Item i) {
            itms.push_back(Item_strings.find(i)->second);
        });
        
        return descriptive_join(itms) + ".";
    }
};


///////////
// World //
///////////

class World {
public:
    World () {
        rooms[Location(0, 0, 0)] = Room{"The room where it all started..."};
        rooms[Location(1, 1, 5)] = Room{"You found it! Lots of gold!"};
    }
    bool room_exists (const Location room) const {
        return rooms.find(room) != rooms.end();
    }
    Room& room_at (const Location location) {
        return rooms[location];
    }
    const void dig (Location location) {
        rooms[location] = Room{};
    }
    const std::vector<std::string> adjacent_directions (Location location) {
        std::vector<std::string> result {};

        std::for_each(Direction.begin(), Direction.end(), [&](std::pair<std::string, Location> direction) {
            if (room_exists(location + direction.second)) {
                result.push_back(direction.first);
            }
        });
        
        return result;
    }
private:
    std::map<Location, Room> rooms;
};

////////////
// Player //
////////////

class Player : public IItemContainer {
public:
    Player () : current_location(0, 0, 0) {
        items.insert(SLEDGE);
    };
    const Location goto_direction (const Location direction) {
        current_location += direction;
        return current_location;
    }
    Location get_current_location () const {
        return current_location;
    }
    std::string inventory () {
        if (items.size() == 0) {
            return "You are not carrying anything";
        } else {
            std::vector<std::string> itemsv;
            std::for_each(items.begin(), items.end(), [&](Item i) {
                itemsv.push_back(Item_strings.find(i)->second);
            });
            return "You are carrying: " + descriptive_join(itemsv);
        }
    }
    bool equip(Item item) {
        if (has_item(item)) {
            equipping = true;
            equipped = item;
            return true;
        } else {
            return false;
        }
    }
    bool unequip(Item item) {
        if (equipped == item) {
            equipping = false;
            return true;
        } else {
            return false;
        }
    }
    bool is_equipping(Item item) {
        return equipping == true && equipped == item;
    }
private:
    Location current_location;
    bool equipping {false};
    Item equipped;
};


//////////
// Game //
//////////

class Game {
public:
    void execute(const std::string command) {
        std::vector<std::string> cmd;

        std::for_each(std::sregex_token_iterator {command.begin(), command.end(), std::regex {"\\s+"}, -1},
                      std::sregex_token_iterator {},
                      [&](std::ssub_match sm) {
            cmd.push_back(sm.str());
        });
        
        std::string action {unalias(cmd.front())};
        
        // HELP
        if (action == "help") {
            print_help();
        
        // MOVE (Up, Down, North etc)
        } else if (Direction.find(action) != Direction.end()) {
            Location direction {Direction.find(action)->second};
            Location location {direction + player.get_current_location()};
            
            if (world.room_exists(location)) {
                Room& room {world.room_at(location)};
                if (action == "up" && !room.has_item(LADDER)) {
                    std::cout << "You can't go upwards without a ladder!" << std::endl;
                } else {
                    Location current_location = player.goto_direction(direction);
                    std::cout << world.room_at(current_location).describe(world.adjacent_directions(current_location));
                }
            } else {
                std::cout << "There's no exit in that direction!" << std::endl;
            }
            
        // LOOK and describe the room
        } else if (action == "look") {
            std::cout << world.room_at(player.get_current_location()).describe(world.adjacent_directions(player.get_current_location()));
        
        // DIG towards a direction
        } else if (action == "dig") {
            if (cmd.size() != 2) {
                std::cout << "Where do you want to dig?" << std::endl;
            } else if (!player.is_equipping(SLEDGE)) {
                std::cout << "With your bare hands?!" << std::endl;
            } else {
                if (Direction.find(cmd[1]) == Direction.end()) {
                    std::cout << "That is not a direction I recognize" << std::endl;
                } else {
                    Location new_loc = player.get_current_location() + Direction.find(cmd[1])->second;
                    if (world.room_exists(new_loc)) {
                        std::cout << "There is already an exit, there!" << std::endl;
                    } else {
                        world.dig(player.get_current_location() + Direction.find(cmd[1])->second);
                        std::cout << "There is now a new exit " << cmd[1] << "ward" << std::endl;
                    }
                }
            }
        
        // DROP an item or all in the inventory
        } else if (action == "drop") {
            if (cmd.size() == 1) {
                std::cout << "Drop what?" << std::endl;
            } else {
                if (cmd[1] == "all" ) {
                    Room& room {world.room_at(player.get_current_location())};
                    room.add_all_items(player.remove_all_items());
                    std::cout << "All items dropped;" << std::endl;
                } else if (String_items.find(cmd[1]) == String_items.end()) {
                    std::cout << "\"" << cmd[1] <<"\" is not something I recognize" << std::endl;
                } else {
                    Item item {String_items.find(cmd[1])->second};
                    if (player.remove_item(item)) {
                        Room& room {world.room_at(player.get_current_location())};
                        room.add_item(item);
                        std::cout << "Item dropped" << std::endl;
                    } else {
                        std::cout << "You don't have any" << cmd[1] << std::endl;
                    }
                }
            }
        
        // TAKE an item from the room or all of them
        } else if (action == "take" ) {
            if (cmd.size() == 1) {
                std::cout << "Take what?" << std::endl;
            } else {
                if (cmd[1] == "all") {
                    Room& room {world.room_at(player.get_current_location())};
                    player.add_all_items(room.remove_all_items());
                    std::cout << "All items taken;" << std::endl;
                } else if (String_items.find(cmd[1]) == String_items.end()) {
                      std::cout << "\"" << cmd[1] <<"\" is not something I recognize" << std::endl;
                } else {
                    Item item {String_items.find(cmd[1])->second};
                    Room& room {world.room_at(player.get_current_location())};
                    if (room.remove_item(item)) {
                        player.add_item(item);
                        std::cout << "Item taken" << std::endl;
                    } else {
                        std::cout << "There is no such item in the room" << std::endl;
                    }
                }
            }
        
        // INVENTORY listing
        } else if (action == "inventory") {
            std::cout << player.inventory() << std::endl;
        
        // EQUIP an item currently in the inventory
        } else if (action == "equip") {
            if (cmd.size() == 1) {
                std::cout << "What do you want to equip?" << std::endl;
            } else {
                if (String_items.find(cmd[1]) == String_items.end()) {
                    std::cout << "That is not an item I recognize" << std::endl;
                } else {
                    if (player.equip(String_items.find(cmd[1])->second)) {
                        std::cout << "Item equipped!" << std::endl;
                    } else {
                        std::cout << "You don't have it" << std::endl;
                    }
                }
            }
        
        // UNEQUIP an item currently equipped
        } else if (action == "unequip") {
            if (cmd.size() == 1) {
                std::cout << "What do you want to unequip?" << std::endl;
            } else {
                if (String_items.find(cmd[1]) == String_items.end()) {
                    std::cout << "That is not an item I recognize" << std::endl;
                } else {
                    if (player.equip(String_items.find(cmd[1])->second)) {
                        std::cout << "Item unequipped!" << std::endl;
                    } else {
                        std::cout << "You don't have it" << std::endl;
                    }
                }
            }
        
        // ALIAS a command, or list the aliases
        } else if (action == "alias") {
            if (cmd.size() == 1) {
                list_aliases();
            } else if (cmd.size() == 3) {
                add_alias(cmd[1], cmd[2]);
                std::cout << "Alias added" << std::endl;
            } else {
                std::cout << "The correct use is: alias <ALIAS> <COMMAND>" << std::endl;
            }
        
        // WHAT?! A command that is not undertood by this simplistic parser
        } else {
            std::cout << "Hm?! What do you mean?" << std::endl;
        }
    }
    void print_help() {
        std::cout << "You need a sledge to dig rooms and ladders to go upwards." << std::endl
            << "Valid commands are: directions (north, south...), dig, take, drop, equip, inventory and look." << std::endl
            << "Additionally you can tag rooms with the 'name' command and alias commands with 'alias'." << std::endl
            << "Have fun!" << std::endl;
    }
private:
    World  world;
    Player player;
};


////////////////
// Main cycle //
////////////////

int main()
{
    Game game;
    std::string input_line;
    
    std::cout << "Welcome to the dungeon!" << std::endl
        << "Grab the sledge and make your way to room 1,1,5 for a non-existant prize!" << std::endl << std::endl;
    game.print_help();
    
    while (input_line != "exit") {
        std::getline(std::cin, input_line);
        
        game.execute(input_line);
    }
    
    std::cout << "See you next time!" << std::endl;
    
    return 0;
}

```

