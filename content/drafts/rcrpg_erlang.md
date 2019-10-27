+++
title = "RCRPG/Erlang"
description = ""
date = 2013-12-16T20:52:14Z
aliases = []
[extra]
id = 16917
[taxonomies]
categories = []
tags = []
+++

{{collection|RCRPG}}
My play tester insisted that the underground rooms did not exceed 8. Not interesting enough for more rooms, he said. But it is possible to make more rooms, with adjustable amount of ladders on each floor. Then explore the rooms.

No play output included by others so I will not do that either.

```Erlang

-module( rcrpg ).

-export( [explore/1, rooms/4, task/0] ).

explore( Rooms ) ->
    Start = {1, 1, 1},
    io:fwrite( "You want to find the gold. You are at ~p ({x,y,z}).~n", [Start] ),
    io:fwrite( "X increases north. Y increases east. Z increases down.~n" ),
    io:fwrite( "Walk in the direction you want to go. Use sledge to open a passage to the next room.~n" ),
    io:fwrite( "You can only climb if the room has a ladder (not in your inventory). Use sledge to open a passage to the next room.~n" ),
    usage(),
    loop( command(), Start, ["sledge"], Rooms ).

rooms( X_max, Y_max, Z_max, Ladders_per_level ) ->
    Empty_rooms = [{{X, Y, Z}, []} || X <- lists:seq(1, X_max), Y <- lists:seq(1, Y_max), Z <- lists:seq(1, Z_max)],
    Ladders = [{random:uniform(X_max), random:uniform(Y_max), Z} || Z <- lists:seq(1, Z_max), _Times <- lists:seq(1, Ladders_per_level)],
    Ladder_rooms = lists:foldl( fun (Position, Acc) -> [{Position, ["ladder"]} | proplists:delete(Position, Acc)] end, Empty_rooms, Ladders ),
    %% Gold last so no ladder replaces it. Yes, we might lose one ladder.
    Gold = {random:uniform(X_max), random:uniform(Y_max), random:uniform(Z_max)},
    [{Gold, ["gold"]} | proplists:delete(Gold, Ladder_rooms)].

task() -> explore( rooms(2, 2, 2, 1) ).



command() -> command( string:tokens(command_line(), " ") ).

command( ["climb", Argument | _T] ) when Argument =/= "up", Argument =/= "down" ->
	usage( "climb" ),
	command();
command( ["climb", Argument | _T] ) -> ["climb", Argument];
command( ["help"] ) ->
	usage(),
	command();
command( ["help", Command] ) ->
        usage( Command ),
        command();
command( ["inventory" | _T] ) -> ["inventory"];
command( ["look" | _T] ) -> ["look"];
command( ["pack", Argument | _T] ) -> ["pack", Argument];
command( ["unpack", Argument | _T] ) -> ["unpack", Argument];
command( ["smash", Direction | _T] ) when Direction =/= "north", Direction =/= "east", Direction =/= "west", Direction =/= "south", Direction =/= "up", Direction =/= "down" ->
        usage( "smash" ),
        command();
command( ["smash", Direction | _T] ) -> ["smash", Direction];
command( ["walk", Argument | _T] ) when Argument =/= "north", Argument =/= "east", Argument =/= "west", Argument =/= "south" ->
	usage( "walk" ),
	command();
command( ["walk", Argument | _T] ) -> ["walk", Argument];
command( ["whereami" | _T] ) -> ["whereami"];
command( ["quit" | _T] ) -> ["quit"];
command( Command ) ->
	io:fwrite( "~s: unknown command (or missing argument).~n", [Command] ),
	usage(),
	command().

command_line() -> string:strip( io:get_line("Enter command (and argument): "), right, 10 ).

commands() -> ["climb", "help", "inventory", "look", "pack", "unpack", "smash", "walk", "whereami", "quit"].

is_climb_ok( Direction, Position, Rooms ) ->
	Room_items = room_items( Position, Rooms ),
	lists:member( "ladder", Room_items )
	andalso lists:member( "hole" ++ Direction, Room_items ).

is_direction_ok( "south", {1, _Y, _Z}, _Rooms ) -> false;
is_direction_ok( "west", {_X, 1, _Z}, _Rooms ) -> false;
is_direction_ok( "up", {_X, _Y, 1}, _Rooms ) -> false;
is_direction_ok( "north", {Px, _Py, _Pz}, Rooms ) ->
	X_max = lists:max( [X || {{X, _Y, _Z}, _} <- Rooms] ),
	Px < X_max;
is_direction_ok( "east", {_Px, Py, _Pz}, Rooms ) ->
	Y_max = lists:max( [Y || {{_X, Y, _Z}, _} <- Rooms] ),
	Py < Y_max;
is_direction_ok( "down", {_Px, _Py, Pz}, Rooms ) ->
	Z_max = lists:max( [Z || {{_X, _Y, Z}, _} <- Rooms] ),
	Pz < Z_max.

is_smash_ok( Direction, Position, Rooms ) ->
	is_smash_ok_sledge( lists:member("sledge", room_items(Position, Rooms)) )
        andalso is_direction_ok( Direction, Position, Rooms ).

is_smash_ok_sledge( true ) -> true;
is_smash_ok_sledge( false ) ->
	io:fwrite( "You do not have the sledge available.~n" ),
	false.

is_walk_ok( Direction, Position, Rooms ) -> lists:member( "hole" ++ Direction, room_items(Position, Rooms) ).

loop( ["climb", Direction], Position, Inventory, Rooms ) ->
    loop_climb( is_climb_ok(Direction, Position, Rooms), Direction, Position, Inventory, Rooms );
loop( ["inventory"], Position, Inventory, Rooms ) ->
    io:fwrite( "Inventory: ~p.~n", [Inventory] ),
    loop( command(), Position, Inventory, Rooms );
loop( ["look"], Position, Inventory, Rooms ) ->
    io:fwrite( "You see: ~p.~n", [proplists:get_value(Position, Rooms)] ),
    loop( command(), Position, Inventory, Rooms );
loop( ["smash", Direction], Position, Inventory, Rooms ) ->
    loop_smash( is_smash_ok(Direction, Position, Rooms), Direction, Position, Inventory, Rooms );
loop( ["whereami"], Position, Inventory, Rooms ) ->
    io:fwrite( "You are here: ~p.~n", [Position] ),
    loop( command(), Position, Inventory, Rooms );
loop( ["pack", Item], Position, Inventory, Rooms ) ->
    loop_pack( lists:member(Item, room_items(Position, Rooms)), Item, Position, Inventory, Rooms );
loop( ["unpack", Item], Position, Inventory, Rooms ) ->
    loop_unpack( lists:member(Item, Inventory), Item, Position, Inventory, Rooms );
loop( ["quit"], _Position, _Inventory, _Rooms ) -> quit;
loop( ["walk", Direction], Position, Inventory, Rooms ) ->
    loop_walk( is_walk_ok(Direction, Position, Rooms), Direction, Position, Inventory, Rooms );
loop( Position, Inventory, Command, Rooms ) ->
    io:fwrite( "Unimplemented command ~p. Position ~p Inventory ~p.~n", [Command, Position, Inventory] ),
    loop_done( lists:member(gold, Inventory), Position, Inventory, Rooms ).

loop_climb( true, Direction, Position, Inventory, Rooms ) ->
	io:fwrite( "You climb ~p.~n", [Direction] ),
	loop( command(), position_update(Direction, Position), Inventory, rooms_update_ladder(Direction, Position, Rooms) );
loop_climb( false, Direction, Position, Inventory, Rooms ) ->
       io:fwrite( "You can not climb ~p.~n", [Direction] ),
       loop( command(), Position, Inventory, Rooms ).

loop_done( true, _Position, _Inventory, _Rooms ) -> success;
loop_done( false, Position, Inventory, Rooms ) -> loop( command(), Position, Inventory, Rooms ).

loop_pack( true, Item, Position, Old_inventory, Rooms ) ->
	io:fwrite( "You pack ~p.~n", [Item] ),
	Inventory = [Item | Old_inventory],
	loop_done( lists:member("gold", Inventory), Position, Inventory, rooms_remove_item(Item, Position, Rooms) );
loop_pack( false, Item, Position, Inventory, Rooms ) ->
	io:fwrite( "Item ~p not in room.~n", [Item] ),
	loop( command(), Position, Inventory, Rooms ).

loop_unpack( true, Item, Position, Old_inventory, Rooms ) ->
        io:fwrite( "Item ~p unpacked.~n", [Item] ),
        Inventory = lists:delete( Item, Old_inventory ),
	loop_done( lists:member("gold", Inventory), Position, Inventory, rooms_add_item(Item, Position, Rooms) );
loop_unpack( false, Item, Position, Inventory, Rooms ) ->
        io:fwrite( "Item ~p not in inventory.~n", [Item] ),
        loop( command(), Position, Inventory, Rooms ).

loop_smash( true, Direction, Position, Inventory, Rooms ) ->
        io:fwrite( "You smash hole ~p.~n", [Direction] ),
	loop( command(), Position, Inventory, rooms_add_hole(Direction, Position, Rooms) );
loop_smash( false, Direction, Position, Inventory, Rooms ) ->
	io:fwrite( "You can not smash hole ~p.~n", [Direction] ),
	loop( command(), Position, Inventory, Rooms ).

loop_walk( true, Direction, Position, Inventory, Rooms ) ->
        io:fwrite( "You walk ~p.~n", [Direction] ),
        loop( command(), position_update(Direction, Position), Inventory, Rooms );
loop_walk( false, Direction, Position, Inventory, Rooms ) ->
	io:fwrite( "You can not walk ~p.~n", [Direction] ),
        loop( command(), Position, Inventory, Rooms ).

opposite( "north" ) -> "south";
opposite( "south" ) -> "north";
opposite( "east" ) -> "west";
opposite( "west" ) -> "east";
opposite( "up" ) -> "down";
opposite( "down" ) -> "up".

position_update( "north", {X, Y, Z} ) -> {X + 1, Y, Z};
position_update( "south", {X, Y, Z} ) -> {X - 1, Y, Z};
position_update( "east", {X, Y, Z} ) -> {X, Y + 1, Z};
position_update( "west", {X, Y, Z} ) -> {X, Y - 1, Z};
position_update( "up", {X, Y, Z} ) -> {X, Y, Z - 1};
position_update( "down", {X, Y, Z} ) -> {X, Y, Z + 1}.

room_items( Position, Rooms ) -> proplists:get_value( Position, Rooms ).

rooms_add_hole( Direction, Position, Old_rooms ) ->
	Hole = "hole" ++ Direction,
	Opposite_hole = "hole" ++ opposite( Direction ),
	Rooms = rooms_add_item( Hole, Position, Old_rooms ),
	rooms_add_item(	Opposite_hole, position_update(Direction, Position), Rooms ).

rooms_add_item( Item, Position, Rooms ) ->
        Room_items = room_items(Position, Rooms),
        rooms_update( [Item | Room_items], Position, Rooms ).

rooms_remove_item( Item, Position, Rooms ) ->
        Room_items = room_items(Position, Rooms),
        rooms_update( lists:delete(Item, Room_items), Position, Rooms ).

rooms_update( Items, Position, Rooms ) -> [{Position, Items} | proplists:delete(Position, Rooms)].

rooms_update_ladder( Direction, Position, Old_rooms ) ->
	Rooms = rooms_remove_item( "ladder", Position, Old_rooms ),
	rooms_add_item( "ladder", position_update(Direction, Position), Rooms ).

usage() ->
	io:fwrite( "Available commands: ~p~n", [commands()] ),
	io:fwrite( "help command, will give help for a command~n" ).

usage( "climb" ) -> [io:fwrite( "climb ~s. Will move you ~s one level if the ~s has been smashed by a sledge and ladder is in the room (not in the inventory).~n", [X, X, Y] ) || {X, Y} <- [{"down", "floor"}, {"up", "ceiling"}]];
usage( "help" ) -> io:fwrite( "help command. Will give help for a command.~n" );
usage( "inventory" ) -> io:fwrite( "inventory. See what you pack with you.~n" );
usage( "look" ) -> io:fwrite( "look. See what is in the room.~n" );
usage( "pack" ) -> io:fwrite( "pack item. Will add item to inventory, removing it from the room.~n" );
usage( "unpack" ) -> io:fwrite( "unpack item. Will remove item from inventory, adding it to the room.~n" );
usage( "smash" ) -> [io:fwrite("smash ~s. Will smash a hole ~s with the sledge.~n", [X, X]) || X <- ["north", "east", "west", "south", "down", "up"]],
    io:fwrite( "You can not smash outside of the building.~n" ),
    io:fwrite( "You can only smash if sledge is in the room, no longer in the inventory.~n" );
usage( "walk" ) -> [io:fwrite( "walk ~s, will move you to the ~s if that wall has been smashed a sledge.~n", [X, X] ) || X <- ["north", "east", "west", "south"]];
usage( "whereami" ) -> io:fwrite( "whereami, will give you the co-ordinates {x,y,z} of where you are.~n" );
usage( "quit" ) -> io:fwrite( "quit, will quit (save not available).~n" );
usage( Command ) ->
    io:fwrite( "~p: unknown command.~n", [Command] ),
    usage().

```

