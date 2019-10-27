+++
title = "RCRPG/Oz"
description = ""
date = 2010-05-20T19:20:28Z
aliases = []
[extra]
id = 7382
[taxonomies]
categories = []
tags = []
+++

{{collection|RCRPG}}
This [[Oz]] version of [[:Category:RCRPG|RCRPG]] implements a text interface.

It is a purely functional implementation, i.e. the state of the world is passed as an argument to functions. If a function modifies the world, a new version is returned.

It implements the same commands as the other implementations (no "alias", "name" or "help" commands, though).

==Code==


```Oz
declare
  %% Basic data structures

  fun {CreateWorld Goal}
     world(goal:Goal
           position:[0 0 0]
           equipped:empty
           inventory:nil
           status:nil
           %% initially we only have one room
           %% coords: X      Y      Z
           rooms:unit(0:unit(0:unit(0:{CreateRoom sledge})))
          )
  end
  
  fun {CreateRoom InitialItem}
     room(items:[InitialItem]
          north:wall
          south:wall
          east:wall
          west:wall
          up:wall
          down:wall)
  end
  
  %% COMMANDS (and shortcuts)
  fun {CreateCommandTable}
     Commands =
     unit(north: {Curry1of2 Go north}  n:Commands.north
          south: {Curry1of2 Go south}  s:Commands.south
          east:  {Curry1of2 Go east}   e:Commands.east
          west:  {Curry1of2 Go west}   w:Commands.west
          up:    {Curry1of2 Go up}     u:Commands.up
          down:  {Curry1of2 Go down}   d:Commands.down
          attack:Attack                a:Commands.attack
          drop:  Drop
          take:  Take
          inventory:Inventory         i:Commands.inventory
          inv:   Commands.inventory
          equip: Equip
         )
  in
     Commands
  end
  CommandTable = {Value.byNeed CreateCommandTable}
  
  %% The game loop.
  proc {Loop World0}
     {PrintStatus World0}
     World = {SetStatus World0 nil}
  in
     if World.position == World.goal then
        {System.showInfo "You are now in the goal room. You have won the game!"}
     else
        {System.printInfo ">"}
        %% Read and parse user input
        Tokens = {Map {String.tokens {ReadLine} & } String.toAtom}
     in
        if Tokens == nil then %% repeat room description when user just presses enter
           {Loop World}
        else
           ComName|Args = Tokens
        in
           if {Not {HasFeature CommandTable ComName}} then
              {Loop {SetStatus World "Unknown command."}}
           else
              ComFunction = CommandTable.ComName
           in
              %% check for number of args (+2: "World" in and out)
              if {Procedure.arity ComFunction} \= 2 + {Length Args} then
                 {Loop {SetStatus World "Wrong number of arguments."}}
              else
                 WorldAfterCommand
              in
                 %% dynamically call ComFunction
                 %% (statically unknown number of arguments)
                 {Procedure.apply ComFunction World|{Append Args [WorldAfterCommand]}}
                 {Loop WorldAfterCommand}
              end
           end
        end
     end
  end

  %% IMPLEMENTATION OF COMMANDS
  %% All commands take the "world" as their first argument.
  %% They might take additional arguments (from the user).
  %% All commands return the new world.

  %% Try to go one step into the specified direction.
  %% Direction: atom like 'north'
  %% (Go is not a real command; it only becomes one after currying.)
  fun {Go Direction World}
     CR = {GetCurrentRoom World}
  in
     if CR.Direction == open then
        if Direction == up andthen {Not {Member ladder CR.items}} then
           {SetStatus World "There is no ladder in this room."}
        else
           {AdjoinAt World position {MovePos World.position Direction}}
        end
     else
        {SetStatus World "There is a wall in that direction."}
     end
  end

  fun {Attack World Direction}
     if {Not {IsDirection Direction}} then
        {SetStatus World "Unknown direction"}
     elseif World.equipped == sledge then
        case {GetCurrentRoom World}.Direction
        of wall then
           {SetStatus {ConnectCurrentRoomTo World Direction}
            "I made a hole in the wall."}
        [] open then
           {SetStatus World "There is already a connection in that direction."}
        end
     else
        {SetStatus World "Can't attack with this item."}
     end
  end

  %% Item: name of item as atom
  fun {Drop World Item}
     if Item == all then {FoldL World.inventory Drop World}
     else
        if {Member Item World.inventory} then
           CR = {GetCurrentRoom World}
           %% add to room
           NewRoom = {AdjoinAt CR items Item|CR.items}
           World2 = {SetRoom World World.position NewRoom}
           %% remove from inventory
           RemainingItems = {Remove World2.inventory Item}
           World3 = {AdjoinAt World3 inventory RemainingItems}
           %% possibly update "equipped"
           World4 = if {Not {Member World3.equipped RemainingItems}} then
                       {AdjoinAt World3 equipped empty}
                    else
                       World3
                    end
        in
           {Inventory World4}
        else
           {SetStatus World "Not carrying such an item."}
        end
     end
  end

  %% Item: item name as an atom
  fun {Take World Item}
     CR = {GetCurrentRoom World}
  in
     if Item == all then {FoldL CR.items Take World}
     else
        if {Member Item CR.items} then
           %% remove from room
           NewRoom = {AdjoinAt CR items {Remove CR.items Item}}
           World2 = {SetRoom World World.position NewRoom}
           %% add to inventory
           World3 = {AdjoinAt World2 inventory Item|World2.inventory}
        in
           {Inventory World3}
        else
           {SetStatus World "There is no such item in this room."}
        end
     end
  end

  fun {Equip World Item}
     if {Member Item World.inventory} then
        {SetStatus {AdjoinAt World equipped Item}
         "Equipped with "#Item}
     else
        {SetStatus World "No such item in inventory."}
     end
  end

  %% Shows the inventory as the status.
  fun {Inventory World}
     {SetStatus World "inventory: "#{ListToString World.inventory}}
  end

  
  %% Operations on basic data structures (the "world" and rooms)

  proc {PrintStatus World}
     if World.status == nil then %% describe room if no status
        [X Y Z] = World.position
     in
        {System.showInfo
         "You are in room ("#X#", "#Y#", "#Z#").\n"#
         "The following items are in this room: "#
         {ListToString {GetCurrentRoom World}.items}#"."}
     else
        {System.showInfo World.status}
     end
  end

  fun {SetStatus World Status}
     {AdjoinAt World status Status}
  end

  fun {NewRoom}
     {CreateRoom {RandomlySelect [sledge gold ladder]}}
  end

  %% Returns the room at the given position.
  %% Might modify the world (if the room does not yet exist) and returns the new world
  %% in 'NewWorld'.
  fun {GetRoom World Position ?NewWorld}
     {CondGet World rooms|Position NewRoom ?NewWorld}
  end
  
  fun {GetCurrentRoom World}
     [X Y Z] = World.position
  in
     World.rooms.X.Y.Z
  end

  %% Replaces a room. Returns the new world.
  fun {SetRoom World Pos NewRoom}
     {Set World rooms|Pos NewRoom}
  end

  local
     %% Maps directions to movement deltas.
     DirPos = unit(east: [~1 0 0] west: [1 0 0]
                   north:[0 ~1 0] south:[0 1 0]
                   up:   [0 0 ~1] down: [0 0 1])
  in
     %% Returns the position that results from going one step
     %% into the specified direction.
     fun {MovePos Pos Dir}
        {List.zip Pos DirPos.Dir Number.'+'}
     end
  end

  local
     OppositeDir = unit(east:west west:east
                        north:south south:north
                        up:down down:up)
  in
     fun {IsDirection X} {HasFeature OppositeDir X} end

     %% Connects the current room to a neighbouring room.
     %% Returns the new world.
     fun {ConnectCurrentRoomTo World Direction}
        World2 = {OpenRoom World World.position Direction}
        OtherPos = {MovePos World2.position Direction}
     in
        {OpenRoom World2 OtherPos OppositeDir.Direction}
     end
  end
  
  fun {OpenRoom World RoomPosition Direction}
     World2
     Room = {GetRoom World RoomPosition ?World2}
  in
     {SetRoom World2 RoomPosition {AdjoinAt Room Direction open}}
  end
  
  
  %% general helpers

  %% Creates a one-argument function from a two-argument function.
  fun {Curry1of2 Fun Arg1}
     fun {$ Arg2}
        Return
     in
        {Procedure.apply Fun [Arg1 Arg2 Return]}
        Return
     end
  end

  %% Removes the first occurance of Y from the list Xs.
  fun {Remove Xs Y}
     case Xs of !Y|Yr then Yr
     [] X|Xr then X|{Remove Xr Y}
     [] nil then nil
     end
  end

  %% Returns a randomly picked element of Xs.
  fun {RandomlySelect Xs}
     Idx = {OS.rand} * {Length Xs} div {OS.randLimits _} + 1
  in
     {Nth Xs Idx}
  end

  %% Reads a line from stdin.
  local
     StdIn = {New class $ from Open.file Open.text end init(name:stdin)}
  in
     fun {ReadLine}
        {StdIn getS($)}
     end
  end

  fun {ListToString Xs}
     {Value.toVirtualString Xs 1000 1000}
  end
  
  
  %% Support for using nested records as multidimensional immutable arrays

  Nothing = {NewName}

  %% Returns a value from an array where Fs is a list of array indices.
  %% If the specified entry does not exist, it is created by calling Otherwise
  %% and the new array is returned in NewArr.
  %% Example: Val = {CondGet a(1:b(3:42)) [1 3] _ _} == 42
  fun {CondGet Arr Fs Otherwise ?NewArr}
     case Fs of F|Fr then
        Arr2 = if Arr == Nothing then unit else Arr end
        NewArrF
        Res = {CondGet {CondSelect Arr2 F Nothing} Fr Otherwise ?NewArrF}
     in
        NewArr = {AdjoinAt Arr2 F NewArrF}
        Res
     [] nil then
        NewArr = if Arr == Nothing then {Otherwise} else Arr end
        NewArr
     end
  end

  %% Sets a (new or existing) entry in Arr to V.
  %% Returns the new array.
  fun {Set Arr Fs V}
     case Fs of F|Fr then
        Arr2 = if Arr == Nothing then unit else Arr end
     in
        {AdjoinAt Arr2 F {Set {CondSelect Arr2 F Nothing} Fr V}}
     [] nil then
        V
     end
  end
in
  %% Start game
  {Loop {CreateWorld [1 1 5]}}
```

