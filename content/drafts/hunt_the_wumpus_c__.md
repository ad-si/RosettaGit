+++
title = "Hunt The Wumpus/C++"
description = ""
date = 2018-10-22T15:15:36Z
aliases = []
[extra]
id = 19157
[taxonomies]
categories = []
tags = []
+++

#include <iostream>
#include <random>
#include <string>
#include <sstream>

#include "wumpus.h"

namespace wumpus {

    Dungeon::Dungeon()
    {
        // create room numbers
        std::array<Room_number,20> random_room_numbers;

        for (size_t i = 0; i < rooms.size(); ++i) {
            random_room_numbers[i] = i + 1;
        }

        //generate random numbers to use to put room numbers random
        std::random_device rd;
        std::mt19937 g(rd());
        std::shuffle(random_room_numbers.begin(), random_room_numbers.end(),g);

        // add room numbers randomly
        for (size_t i = 0; i < rooms.size(), i < random_room_numbers.size(); ++i) {
            rooms[i].room_number = random_room_numbers[i];
        }

        std::size_t i{ 0 };
        rooms[i++].has_player = true;
        rooms[i++].has_wumpus = true;

        for (auto pits{ count_of_pits }; pits; --pits) {
            rooms[i++].has_pit = true;
        }

        for (auto bats{ count_of_bats }; bats; --bats) {
            rooms[i++].has_bat = true;
        }

        std::shuffle(rooms.begin(), rooms.end(), g);
    }

    void Dungeon::indicate_hazards()
    {
        bool is_first_bat = true;
        bool is_first_pit = true;

        // find the player
        auto player_room{ std::find_if(rooms.begin(), rooms.end(), [](const Room &r) { return r.has_player; }) };

        for (auto& x : player_room->neighbors) {
            if (x->has_wumpus) { 
                std::cout << "I smell the wumpus\n";
            }
            if (is_first_pit && x->has_pit) {
                is_first_pit = false;
                std::cout << "I feel a breeze\n";
            }
            if (is_first_bat && x->has_bat) {
                is_first_bat = false;
                std::cout << "I hear a bat\n";
            }
        }

        std::cout   << "You are in room " << player_room->room_number << "\n"
                    << "You have "<<arrows<< " arrow(s) left\n"
                    << "Tunnels lead to rooms " 
                    << player_room->neighbors[0]->room_number << ", "
                    << player_room->neighbors[1]->room_number << " and "
                    << player_room->neighbors[2]->room_number << "\n"
                    << "what do you want to do? (M)ove or (S)hoot?\n";
    }

    bool Dungeon::shoot_arrow(std::vector<int> target_rooms)
        //trys to shoot in the supplied tar rooms an arrow
        //if the wumpus is hit returns true to indicate victory
        //moves the wumpus on fail
    {
        --arrows;

        // find the player
        auto player_room{ std::find_if(rooms.begin(), rooms.end(), [](const Room &r) { return r.has_player; }) };

        for (const auto& target : target_rooms){

            bool room_reached = false;

            for (const auto& neigbour : player_room->neighbors) {

                if (neigbour->room_number == target) {
                    room_reached = true;

                    if (rooms[neigbour->room_number - 1].has_wumpus) {
                        std::cout << "!!!!!!YOU WON!!!!!!: You killed the Wumpus in room " << rooms[neigbour->room_number - 1].room_number << "\n";
                        return true;
                    }
                    break;
                }
            }
            if (!room_reached) {    
                std::cout << "Room " << target << " could not be reached from arrow\n";
                return false;
            }           
        }
        if (arrows == 0) {
            std::cout << "You lost: You ran out of arrows";
            return true;
        }
        return false;
    }

    bool Dungeon::move_wumpus() 
    {
        auto direction = get_random(0, 3);
        if (direction == 3) {               // 25% chance that wumpus won't move
            return false;
        }

        // find the wumpus
        auto wumpus_room{ std::find_if(rooms.begin(), rooms.end(), [](const Room &r) { return r.has_wumpus; }) };

        // move him
        wumpus_room->has_wumpus = false;
        auto new_room = wumpus_room->neighbors[direction];
        new_room->has_wumpus = true;

        if (new_room->has_player) {
            std::cout << "You lost: Wumpus enters your room and eats you\n";
            return true;
        }
        return false;
    }

    bool Dungeon::move_player(Room_number target_room_number)
        //trys to move player to the selected room
        //if deadly hazard like pit or wumpus is found return game over = true;
        //if bat is found choose new random room free from hazards to put the player
    {

        // find the player
        auto player_room{ std::find_if(rooms.begin(), rooms.end(), [](const Room &r) { return r.has_player; }) };

        for (auto& x : player_room->neighbors) {

            if (x->room_number == target_room_number) {
                if (x->has_wumpus) {
                    std::cout << "You lost: You got eaten by the Wumpus\n";
                    return true;
                }
                else if (x->has_pit) {
                    std::cout << "You lost: You fell in a bottomless pit\n";
                    return true;
                }
                else if (x->has_bat) {
                    std::cout << "Gigantic bat appeared!!!\n";
                    std::cout << "You got dragged to a new room\n";

                    //Only put player in empty room
                    Room* bat_destionation_room = nullptr;
                    do{
                        bat_destionation_room = &rooms[get_random(0, rooms.size() - 1)];
                    } while (bat_destionation_room->has_wumpus || bat_destionation_room->has_pit || bat_destionation_room->has_bat || bat_destionation_room->has_player);

                    player_room->has_player = false;
                    bat_destionation_room->has_player = true;
                    return false;
                }
                else {
                    player_room->has_player = false;
                    auto target_room = &rooms[target_room_number];
                    target_room->has_player = true;
                    return false;
                }
            }
        }
        std::cerr << "Dungeon::move_player: Unknown target room entered";
        return false;
    }

    Room_number Dungeon::select_room_to_move()
    {
        for (;;) {

            std::cout << "To where??\n";

            Room_number target = 0;
            std::cin >> target;

            if (std::cin.fail()) {
                std::cin.clear();
                std::cin.ignore(999, '\n');
                continue;
            }

            auto neighbor = get_neighbour_rooms();

            if (target == neighbor[0] || target == neighbor[1] || target == neighbor[2])
                return target;
        }
    }

    std::array<Room_number, 3> Dungeon::get_neighbour_rooms() const
    {
        // find the player
        auto player_room{ std::find_if(rooms.begin(), rooms.end(), [](const Room &r) { return r.has_player; }) };

        return std::array<Room_number, 3>{
            player_room->neighbors[0]->room_number,
            player_room->neighbors[1]->room_number,
            player_room->neighbors[2]->room_number
        };
    }

    void Dungeon::show_state_of_dungeon()
    {
        auto print_rooms = rooms;

        std::sort(print_rooms.begin(), print_rooms.end(), [](const Room &a, const Room &b) { return b.room_number > a.room_number; });

        for (const auto&room : print_rooms) {
            std::cout << "Room " << room.room_number << " connects to: ";

            for (const auto&neighbor : room.neighbors) {
                if (neighbor != nullptr) {
                    std::cout << neighbor->room_number << " ";
                }
                else {
                    std::cout << "np" << " ";
                }
            }

            std::cout << " ";
            if (room.has_wumpus) {
                std::cout << "wumpus:" << room.has_wumpus << " ";
            }
            if (room.has_pit) {
                std::cout << "pit:" << room.has_pit << " ";
            }
            if (room.has_bat) {
                std::cout << "bat:" << room.has_bat << " ";
            }
            if (room.has_player) {
                std::cout << "player:" << room.has_player << " ";
            }
            std::cout << "\n";
        }
    }

    //-------------------------------------------------------------
    //Helper functions
    //-------------------------------------------------------------

    int get_random(int min, int max)
    {
        static std::random_device rd;
        static std::mt19937 mt(rd());
        std::uniform_int_distribution<int> distribution(min, max);
        return distribution(mt);
    }

    void hunt_the_wumpus()
    {
        instructions();

        for (;;)        // restart game
        {
            Dungeon dungeon;

            dungeon.show_state_of_dungeon();

            for (;;) {      // current room handle

                dungeon.indicate_hazards();

                std::string in;
                std::cin >> in;
                if (std::cin.fail()) {
                    std::cin.clear();
                    std::cin.ignore(999, '\n');
                    continue;
                }

                bool game_over = false;

                if (in == "m" || in == "M" || in == "Move" || in == "move") {
                    game_over = dungeon.move_player(dungeon.select_room_to_move());
                }
                else if (in == "s" || in == "S" || in == "Shoot" || in == "shoot") {

                    game_over = dungeon.shoot_arrow(select_rooms_to_shoot());

                    if (game_over == true) { 
                        break; 
                    }
                    game_over = dungeon.move_wumpus();
                }
                else if (in == "cheat") {       // secret menue to show dungeon state 
                    dungeon.show_state_of_dungeon();
                }
                if (game_over == true) {
                    break;
                }
            }

            std::cout << "Press any key to start a new game or (q)uit to end game\n";
            std::string in;
            std::cin >> in;

            if (in == "q" || in == "Q" || in == "Quit" || in == "quit")
                break;
        }
    }

    void instructions()
    {
        std::cout <<R"(Welcome to "Hunt the Wumpus"!
The wumpus lives in a cave of rooms.Each room has 3 tunnels leading to
other rooms. (Look at a dodecahedron to see how this works - if you don't know
what a dodecahedron is, ask someone).

Hazards
Bottomless pits - two rooms have bottomless pits in them. If you go there, you
fall into the pit(and lose!)
Super bats - two other rooms have super bats.If you go there, a bat grabs you
and takes you to some other room at random. (Which may be troublesome).

Wumpus
The wumpus is not bothered by hazards(he has sucker feet and is too big for a
bat to lift).Usually he is asleep.Two things wake him up : you shooting an
arrow or you entering his room."

If the wumpus wakes he moves(p = .75) one room or stays still(p = .25).After
that, if he is where you are, he eats you up and you lose!"

Each turn you may move or shoot a crooked arrow.
Moving: you can move one room(thru one tunnel).
Arrows : you have 5 arrows.You lose when you run out.Each arrow can go from 1
to 3 rooms.You aim by telling the computer the rooms you want the arrow to go
to.If the arrow can't go that way (if no tunnel) it moves at random to the
next room.If the arrow hits the wumpus, you win.If the arrow hits you, you lose.

Warnings
When you are one room away from a wumpus or hazard, the computer says :

Wumpus: "I smell the wumpus"
Bat : "I hear a bat"
Pit : "I feel a breeze"


"Press any key to start")";

        char c;
        std::cin.get(c);
    }

    std::vector<Room_number> select_rooms_to_shoot()
    {
        for(;;){
            std::cout << "Enter the rooms you want to shoot the arrow (e.g. 2-3-12, e.g. 4-5, e.g. 2)\n";

            std::string input;
            std::cin >> input;

            std::istringstream ist{ input };

            std::vector<int> target_rooms;

            bool bad_input = false;

            while (!ist.eof()) {

                int room_number;
                ist >> room_number;

                if (ist.fail()) {
                    bad_input = true;
                    break;
                }

                target_rooms.push_back(room_number);

                if (target_rooms.size() == 3 || ist.eof())
                    break;

                char seperator;
                ist >> seperator;

                if (ist.fail()) {
                    bad_input = true;
                    break;
                }
                if ((seperator != '-')   || (target_rooms.size() > 3)) {
                    bad_input = true; 
                    break; 
                }
            }

            if (bad_input) {
                continue;
            }
            else {
                return target_rooms;
            }
        }
    }
}
