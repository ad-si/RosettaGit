+++
title = "RCRPG/PureBasic"
description = ""
date = 2014-04-02T22:23:39Z
aliases = []
[extra]
id = 9275
[taxonomies]
categories = []
tags = []
+++

{{collection|RCRPG}}
This version is a more or less direct translation of the version done in Perl (See [[RCRPG/Perl]]) with some additional functionality added. Commands include attack, drop, take, equip, alias, name, inventory, and directions.  The 'name' command can use a double-quoted string if spaces wanted to be included in the name (i.e. 'name "this room").

An additional command 'ask' was implemented for providing hints in finding the randomly placed Prize Room.  The program also allows the stacking of commands if they are separated by a space (i.e. 'take all equip sledge a north n' will perform four actions: take all, equip sledge, [a]ttack north, [n]orth).

A simple outlay of the play strategy is: Take and equip a sledge. Get some gold to ask for hints (from the program) and move a ladder into a room to go up if necessary and attack walls to make exits.

```PureBasic
;Here is a description of all of the commands:
;
;north
;south
;east
;west
;up
;down
;
;Move in the direction specified. The player won’t be able to move if there isn’t
;an exit in that direction.
;
;attack (direction)
;
;Attack in the direction specified. (Equip the sledge first.)
;
;drop {all|(item name)}
;
;Drop the item specified. Or drop everything the player is carrying.
;
;take {all|(item name)}
;
;Take the item specified. Or take everything in the room.
;
;inventory
;
;Display everything the player is carrying.
;
;name (name)
;
;The player can rename the room with this command.
;
;equip (item name)
;
;Equip the item in question.
;
;alias (existing command name) (new command name)
;
;Give an existsing command an alias. Starting aliases are n,s,e,w,u,d. i and inv
;are the same as inventory, a is the same as attack.
;
;At the start of the game, only two rooms exist, the Start and the Prize room.
;All other rooms the player creates by moving around.  The Prize room is randomly
;located each game.  If you need a hint just 'ask'.
;
;To move out of the start room, the player needs to break a hole in a wall,
;ceiling or floor. The sledge is needed to do this.
;
;There are three kinds of items in the different rooms: sledges, gold and
;ladders. A sledge is needed to gain access to rooms the player doesn’t have
;access to. A ladder is needed to move between levels.
;
;Every room, except the prize room, may randomly have gold, a sledge and/or a
;ladder. The player will have to leave the ladder behind when they go up a level,
;so they may have to hunt around a bit for a new ladder.
;
;When the player finds a ladder, they can either move up a level from that room,
;or they can take the ladder and drop it in a room that’s more convenient.
;
;The player can make their way to the Prize Room but there's nothing
;useful there; just gold.
;----------
Structure coord3D
  x.i
  y.i
  z.i
EndStructure

Structure room
  location.coord3D
  name.s
  description.s
  List items.i()
  exits.i ;bitmapped
EndStructure 

Structure player
  *curRoomPtr.room
  location.coord3D
  List inventory.i()
  equipped.i
EndStructure

Enumeration ;items
  #itm_any = -1
  #itm_nothing = 0
  #itm_sledge
  #itm_ladder
  #itm_gold
  #itm_count
EndEnumeration

Enumeration ;directions
  #dir_Nowhere
  #dir_North
  #dir_South
  #dir_East
  #dir_West
  #dir_Up
  #dir_Down
  #dir_count
EndEnumeration

Enumeration ;console colors
  #con_Black
  #con_Blue
  #con_Green
  #con_Cyan
  #con_Red
  #con_Magenta
  #con_Brown
  #con_Light_Grey
  #con_Dark_Grey
  #con_Bright_Blue
  #con_Bright_Green
  #con_Bright_Cyan
  #con_Bright_Red
  #con_Bright_Magenta
  #con_Yellow
  #con_White
EndEnumeration
#ConsoleWidth = 79 ;character width of console

Global NewMap *locations.room() ;"xx,yy,zz" hash of locations and their associated room data
Global NewList rooms.room() ;room data
Prototype.s command_prt(cmd.s) ;each command acts on an input string and returns the unused portion
Global NewMap *commands() ;hash of commands and their functions, all use the same prototype
Global quitNow = #False
Global isConsoleAvailable = #False
Global player.player
Global prizeRoom.coord3D
Global hintCount
Global foundPrizeRoom = #False

;
### ====================

DataSection
  dirData:
  Data.i #dir_Nowhere: Data.s "nowhere", "nowhere", "to nowhere": Data.i  0,  0,  0
  Data.i #dir_North  : Data.s "north", "south", "to the north"  : Data.i  0,  1,  0
  Data.i #dir_South  : Data.s "south", "north", "to the south"  : Data.i  0, -1,  0
  Data.i #dir_East   : Data.s "east", "west", "to the east"     : Data.i  1,  0,  0
  Data.i #dir_West   : Data.s "west", "east", "to the west"     : Data.i -1,  0,  0
  Data.i #dir_Up     : Data.s "up", "down", "in the ceiling"    : Data.i  0,  0,  1
  Data.i #dir_Down   : Data.s "down", "up", "in the floor"      : Data.i  0,  0, -1
  
  roomDescription:
  Data.s "featureless room", "cold dark room", "very stinky room", "small but comfortable room"
  Data.s "room with an incredible echo! Yahollaydoo!", "room resembling a window-less jail"
  Data.s "green room", "black room", "blue room", "brown sad room", "room which breathes pain"
  Data.s "mysterious room", "yellow room", "room with a think layer of dust"
EndDataSection
#count_roomDescriptions = 14 ;count of room Descriptions

Structure dirText
  name.s
  reverseName.s
  exitDesc.s
EndStructure

Global Dim dirText.dirText(#dir_count - 1) ;direction text for each direction index
Global NewMap dirIndex.i(#dir_count - 1) ;index for each name
Global NewMap directions.coord3D() ;coordinate offsets
Define i, x
For i = 0 To #dir_count - 1
  Read.i x
  Read.s dirText(x)\name
  Read.s dirText(x)\reverseName
  Read.s dirText(x)\exitDesc
  
  dirIndex(dirText(x)\name) = x
  
  Read.i directions(dirText(x)\name)\x
  Read.i directions(dirText(x)\name)\y
  Read.i directions(dirText(x)\name)\z
Next 

;
### ======================

Global Dim itemName.s(#itm_count - 1) ;item names for each item index
itemName(#itm_nothing) = "nothing"
itemName(#itm_sledge) = "sledge"
itemName(#itm_ladder) = "ladder"
itemName(#itm_gold) = "gold"

;
### ======================

Declare setLocation(*location.coord3D, x, y, z)

*locations("0,0,0") = AddElement(rooms())
rooms()\name = "The Start"
setLocation(rooms()\location, 0, 0, 0)
rooms()\description = "simple room"
AddElement(rooms()\items())
rooms()\items() = #itm_sledge

setLocation(prizeRoom, Random(6) - 3, Random(6) - 3, Random(5) + 2)
*locations(Str(prizeRoom\x) + "," + Str(prizeRoom\y) + "," + Str(prizeRoom\z)) = AddElement(rooms())
rooms()\name = "The Prize Room"
rooms()\location = prizeRoom
rooms()\description = "very bright and shiny metallic room"
For i = 1 To 9
  AddElement(rooms()\items())
  rooms()\items() = #itm_gold
Next 

Procedure setLocation(*location.coord3D, x, y, z)
  *location\x = x
  *location\y = y
  *location\z = z
EndProcedure

Procedure.s shift(cmd.s, count = 1, sep.s = " ")
  While count > 0
    Protected x = FindString(cmd, sep, 1)
    If x
      cmd = Right(cmd, Len(cmd) - x)
    Else
      cmd = ""
    EndIf
    count - 1
  Wend   
  ProcedureReturn Trim(cmd)
EndProcedure

Procedure display(text.s, textColor = #con_Bright_Cyan, backColor = #con_Black)
  Static lastPrintPos = 0
  If isConsoleAvailable
    Protected lineCount, curLine = 1, lineText.s, maxWidth, wrapPos, outputText.s
    ConsoleColor(textColor, backColor)
    
    lineCount = CountString(text, #CR$)
    Repeat
      lineText = StringField(text, curLine, #CR$)
      While (lastPrintPos + Len(lineText)) >  #ConsoleWidth
        maxWidth = #ConsoleWidth - lastPrintPos
        wrapPos = maxWidth - FindString(ReverseString(Left(lineText, maxWidth)), " ", 1) ;wrap at space
        outputText = Left(lineText, wrapPos)
        If wrapPos <> maxWidth
          ;find last of a group of spaces
          Repeat
            wrapPos + 1
          Until Mid(lineText, wrapPos + 1, 1) <> " "
        ElseIf maxWidth <> #ConsoleWidth And FindString(lineText, " ", 1) <= #ConsoleWidth
          wrapPos = 0
          outputText = ""
        EndIf 
        
        PrintN(outputText)
        lineText = Mid(lineText, wrapPos + 1)
        lastPrintPos = 0
      Wend 
      
      If lineCount > 0
        PrintN(lineText)
        lastPrintPos = 0
      Else
        Print(lineText)
        lastPrintPos + Len(lineText)
      EndIf 
      lineCount - 1: curLine + 1
    Until lineCount < 0
  EndIf 
  ProcedureReturn lastPrintPos
EndProcedure

Procedure displayN(text.s, textColor = #con_Bright_Cyan, backColor = #con_Black)
  ProcedureReturn display(text + #CR$, textColor, backColor)
EndProcedure

Procedure decodeExits(Array dir(1), exits)
  Protected i, count
  Dim dir(#dir_count - 1)
  
  For i = 1 To #dir_count
    If exits & 1
      dir(count) = i
      count + 1
    EndIf
    exits >> 1
  Next 
  If count = 0
    dir(count) = #dir_Nowhere
    count = 1
  EndIf 
  Redim dir(count - 1)
EndProcedure

Procedure parseItem(item.s)
  Protected i, type = #itm_nothing
  For i = 0 To #itm_count - 1
    If itemName(i) = item
      type = i
      Break
    EndIf
  Next
  ProcedureReturn type
EndProcedure

Procedure greet(*room.room)
  display(#CR$ + "You are in room (" + Str(player\location\x) + ", " +  Str(player\location\y) + ", " + Str(player\location\z) + "), ", #con_White)
  If player\location\x = prizeRoom\x And player\location\y = prizeRoom\y And player\location\z = prizeRoom\z
    display(*room\name, #con_Yellow, #con_Bright_Blue)
    If Not foundPrizeRoom
      display(".  It took you " + Str(hintCount) + " hints to find this room.  I bet your arms are tired!  After you've rested a bit you can start patching up all the holes..", #con_White)
      foundPrizeRoom = #True
    EndIf 
  Else
    display(*room\name, #con_Black, #con_White)
  EndIf 
  displayN(".  It is a " + *room\description + ".", #con_White)
 
  Dim dir_present(0)
  decodeExits(dir_present(), *room\exits)
  Protected i, count = ArraySize(dir_present()) + 1
  
  Select count
    Case 1
      If dir_present(0) = #dir_Nowhere
        displayN("There are no exits from this room. Perhaps you need to make one?", #con_Yellow)
      Else
        displayN("There is an exit " + dirText(dir_present(0))\exitDesc, #con_Yellow)
      EndIf 
    Default 
      display("There are exits ", #con_Yellow)
      For i = 0 To count - 3
        display(dirText(dir_present(i))\exitDesc + ", ", #con_Yellow)
      Next 
      displayN(dirText(dir_present(count - 2))\exitDesc + " and " + dirText(dir_present(count - 1))\exitDesc + ".", #con_Yellow)
  EndSelect
  
  count = ListSize(*room\items())
  If count > 0
    display("There is a ", #con_Yellow)
    FirstElement(*room\items())
    Select count
      Case 1
        display(itemName(*room\items()), #con_Yellow)
      Default 
        For i = 1 To count - 2
          display(itemName(*room\items()) + ", ", #con_Yellow)
          NextElement(*room\items())
        Next
        display(itemName(*room\items()) + " and a ", #con_Yellow)
        NextElement(*room\items())
        display(itemName(*room\items()), #con_Yellow)
    EndSelect
    displayN(" here.", #con_Yellow)
  Else
    displayN("There is nothing useful here you can take.", #con_Yellow)
  EndIf 
EndProcedure

Procedure mainloop()
  player\location\x = 0: player\location\y = 0: player\location\z = 0
  player\curRoomPtr = *locations(Str(player\location\x) + "," + Str(player\location\y) + "," + Str(player\location\z))
  displayN("Welcome to RCRPG, the PureBasic Edition.", #con_Yellow, #con_Bright_Blue)
  displayN("The aim is to find 'The Prize Room', good luck!", #con_Yellow)
  
  Repeat
    greet(player\curRoomPtr)
    displayN(#CR$ + "Try 'help' for help", #con_Bright_Green)
    display(#CR$ + ">")
    Protected cmd.s = Trim(Input())
    While cmd <> "" And quitNow = #False
      Protected command.s = StringField(cmd, 1, " ")
    
      If FindMapElement(*commands(), command)
        Protected function.command_prt = *commands()
        cmd = function(shift(cmd))
      Else
        displayN("'" + StringField(cmd, 1, " ") + "' didn't make any sense.  Try 'help'")
        cmd = ""
      EndIf 
    Wend 
  Until quitNow
EndProcedure

;Return pointer to room at the given coordinates, create it if it doesn't already exist.
Procedure roomcheck(x, y, z)
  Protected roomAddr.s = Str(x) + "," + Str(y) + "," + Str(z)
  If FindMapElement(*locations(), roomAddr)
    ProcedureReturn *locations()
  EndIf 
  
  *locations(roomAddr) = AddElement(rooms())
  With rooms()  
    \location\x = x: \location\y = y: \location\z = z
    \name = "Nameless"
    Protected i, newRoomDescript = Random(#count_roomDescriptions - 1) 
    Restore roomDescription
    For i = 0 To newRoomDescript
      Read.s \description
    Next 
    ;add items
    For i = 1 To Random(2)
      AddElement(\items())
      \items() = Random(#itm_count - 2) + 1
    Next 
  EndWith
  ProcedureReturn *locations()
EndProcedure

Procedure isLadderPresent(List items.i())
  ForEach items()
    If items() = #itm_ladder
      ProcedureReturn #True ;ladder present
    EndIf
  Next
  ProcedureReturn #False ;no ladder present
EndProcedure

;check if there is an exit from currentroom in the given direction (vector)
;and move to it if possible
Procedure move(dir.s)
  If player\curRoomPtr\exits & (1 << (dirIndex(dir) - 1))
    Protected curRoom.coord3D = player\curRoomPtr\location
    Protected VECTOR.coord3D = directions(dir)
    Protected *toRoom.room = roomcheck(curRoom\x + VECTOR\x, curRoom\y + VECTOR\y, curRoom\z + VECTOR\z)
    If dir = "up"
      ;check for ladder in room
      If Not isLadderPresent(player\curRoomPtr\items())
        displayN("There needs to be a ladder in the room before you can climb up.")
        ProcedureReturn
      EndIf 
    ElseIf dir = "down"
      ;check for ladder in lower room
      If Not isLadderPresent(*toRoom\items())
        displayN("There needs to be a ladder in that room before you can climb down.")
        ProcedureReturn
      EndIf 
    EndIf 
    player\curRoomPtr = *toRoom
    player\location = player\curRoomPtr\location
  Else
    displayN("Your way is blocked.")
  EndIf
EndProcedure

Procedure.s cmd_north(cmd.s)
  move("north")
  ProcedureReturn cmd.s
EndProcedure

Procedure.s cmd_south(cmd.s)
  move("south")
  ProcedureReturn cmd.s
EndProcedure

Procedure.s cmd_east(cmd.s)
  move("east")
  ProcedureReturn cmd.s
EndProcedure

Procedure.s cmd_west(cmd.s)
  move("west")
  ProcedureReturn cmd.s
EndProcedure

Procedure.s cmd_up(cmd.s )
  move("up")
  ProcedureReturn cmd.s
EndProcedure

Procedure.s cmd_down(cmd.s)
  move("down")
  ProcedureReturn cmd.s
EndProcedure

;connect two existing rooms via a related exit
Procedure link(*fromRoom.room, direction.s)
  ;calculate coord of toRoom
  Protected toPos.coord3D = directions(direction)
  toPos\x + *fromRoom\location\x:  toPos\y + *fromRoom\location\y :  toPos\z + *fromRoom\location\z 
  
  ;check if toRoom exists, if not create it (roomcheck)
  Protected *toRoom.room = roomcheck(toPos\x, toPos\y, toPos\z)
  
  ;place exit in fromRoom and corresponding exit direction in toRoom
  *fromRoom\exits | (1 << (dirIndex(direction) - 1))
  *toRoom\exits | (1 << (dirIndex(dirText(dirIndex(direction))\reverseName) - 1))
EndProcedure

Procedure.s cmd_attack(cmd.s)
  Protected direction.s = StringField(cmd, 1, " ")

  If direction = "help"
		displayN("Usage: attack (direction)", #con_Bright_Green)
    ProcedureReturn shift(cmd)
  EndIf 

	If direction <> ""
    If Not FindMapElement(directions(), direction)
      display("I don't now that direction.  Try: ")
      Protected output.s
 			ForEach directions()
        output + " " + MapKey(directions()) + ","
      Next
      displayN(Trim(output, ","))
      ProcedureReturn ""
    EndIf 
	Else
		displayN("Invalid syntax.  Try 'attack (direction)'")
    ProcedureReturn ""
  EndIf 
 
	If player\equipped = parseItem("sledge") 
    If dirIndex(direction) = #dir_Nowhere
      displayN("You destroyed a portion of the " + direction + " and the result is nothing.")
    ElseIf player\curRoomPtr\exits & (1 << (dirIndex(direction) - 1))
			displayN("You swing your sledge wildly.")
		Else
			displayN("You bash until the surface crumbles, leaving a hole you can crawl through.")
      link(player\curRoomPtr, direction)
		EndIf
	Else
		displayN("You accomplish nothing.")
	EndIf
  ProcedureReturn shift(cmd)
EndProcedure

Procedure.s cmd_name(cmd.s)
  Protected sep.s = " ", shiftCount = 1
  
  If Left(cmd, 1) = #DQUOTE$
    sep = #DQUOTE$  : shiftCount = 2
  EndIf 
  
  Protected newName.s = StringField(cmd, shiftCount, sep)
  
 	If newName = "help" Or newName = ""
    displayN("Usage: name (New name of room)", #con_Bright_Green)
    ProcedureReturn ""
  EndIf 
  
  Protected *room.room = player\curRoomPtr
  *room\name = newName 
  displayN("Room is now named '" + *room\name + "'.")
  ProcedureReturn shift(cmd, shiftCount, sep)
EndProcedure

Procedure.s cmd_equip(cmd.s)
  Protected toEquip.s = StringField(cmd, 1, " ")
  
	If toEquip = "help" Or toEquip = ""
    displayN("Usage: equip (itemname)", #con_Bright_Green)
    ProcedureReturn shift(cmd)
  EndIf 
  
  ;Find it
  Protected type = parseItem(toEquip)
  If player\equipped = type And type <> #itm_nothing
    displayN("You are already equipped with your " + toEquip + ".")
  ElseIf type = #itm_nothing
    player\equipped = type
    displayN("You are now equipped with " + toEquip + ".")
  Else
    Protected found
    ;Equip it.
    ForEach player\inventory()
      If player\inventory() = type
        found = 1
        player\equipped = type
        Break
      EndIf 
    Next 
    
    If found
      displayN("You equipped your " + toEquip + ".  Put it to good use.")
    Else 
      displayN("You don't have one of those.  Try 'i' to see what you have.")
    EndIf
  EndIf
  
  ProcedureReturn shift(cmd)
EndProcedure

Procedure move_items(List fromRef.i(), List toRef.i(), cmd.s)
  Protected item.s = StringField(cmd, 1, " ")
  Protected moveall, moved, type
  
  If item = "all"
    moveall = #True
    type = #itm_any
  Else 
    type = parseItem(item)
  EndIf 

  LastElement(toRef())
  ForEach fromRef()
    If type = #itm_any  Or fromRef() = type
      AddElement(toRef())
      toRef() = fromRef()
      DeleteElement(fromRef())
      moved + 1
      If Not moveall: Break: EndIf
    EndIf 
  Next 
    
  ProcedureReturn moved
EndProcedure

Procedure.s cmd_take(cmd.s)
  Protected item.s  = StringField(cmd, 1, " ")
  
 	If item = "help" Or Len(cmd) = 0
    displayN("Usage: take {all|(itemname)}", #con_Bright_Green)
    ProcedureReturn ""
  EndIf 

  Protected *room.room = player\curRoomPtr
  Protected moved = move_items(*room\items(), player\inventory(), cmd)
  Select moved
    Case 0
      displayN("There aren't any of those here.")
    Case 1
      displayN("You took 1 item.")
    Default 
      displayN("You took " + Str(moved) + " items.")
  EndSelect 
  ProcedureReturn shift(cmd)
EndProcedure

Procedure.s cmd_drop (cmd.s)
  Protected item.s = StringField(cmd, 1, " ")

	If item = "help" Or item = ""
    displayN("Usage: drop {all|(itemname)}", #con_Bright_Green)
    ProcedureReturn ""
  EndIf 
 
  Protected *room.room = player\curRoomPtr
  Protected moved = move_items(player\inventory(), *room\items(), cmd)
	Select moved
    Case 0
      displayN("You don't have any of those.")
    Case 1
      displayN("You dropped 1 item.")
    Default
      displayN("You dropped " + Str(moved) + " items.")
  EndSelect 
  
  ;Check to see if we dropped our equipped item.
  If moved And player\equipped
    Protected type = parseItem(item)
    Protected droppedEquipped = #True
    If type <> #itm_nothing
      ForEach player\inventory()
        If player\inventory() = player\equipped
          droppedEquipped = #False
          Break
        EndIf 
      Next 
    EndIf
    If droppedEquipped
      displayN("You dropped the item you were using.")
      cmd_equip("nothing")
    EndIf 
  EndIf 
  ProcedureReturn shift(cmd)
EndProcedure

Procedure.s cmd_inventory(cmd.s)
  Protected help.s = StringField(cmd, 1, " ")

	If help = "help"
    displayN("Usage: inventory", #con_Bright_Green)
  	ProcedureReturn ""
  EndIf 
 
  Protected count = ListSize(player\inventory())
	If count
    FirstElement(player\inventory())
    display("You have a ")
    Select count
      Case 1
        display(itemName(player\inventory()))
      Default
        Protected i
        For i = 1 To count - 2
          display(itemName(player\inventory()) + ", ")
          NextElement(player\inventory())
        Next
        display(itemName(player\inventory()) + " and a ")
        NextElement(player\inventory())
        display(itemName(player\inventory()))
    EndSelect
    displayN(".")
  Else
    displayN("You're not carrying anything.  Ask again later.")
  EndIf 
  ProcedureReturn shift(cmd)
EndProcedure

Procedure Max(x, y)
  If x < y
    ProcedureReturn y
  EndIf
  ProcedureReturn x
EndProcedure

Procedure.s cmd_hint(cmd.s)
  If StringField(cmd, 1, " ")
    displayN("Usage: hint", #con_Bright_Green)
    ProcedureReturn ""
  EndIf 
  
  ;check for gold
  Protected hasGold = #False 
  ForEach player\inventory()
    If player\inventory() = #itm_gold
      hasGold = #True
      Break
    EndIf 
  Next 
    
  ;if gold is not present then exit
  If Not hasGold
    displayN("You can't get a hint, you need gold.")
    ProcedureReturn cmd
  EndIf 
  
  display("Will you pay a gold for a hint (y/n)?")
  Protected Answer.s = Input()
  displayN("")
  If LCase(Left(Answer, 1)) <> "y"
    displayN("You must be saving your money for retirement, good luck!")
    ProcedureReturn cmd
  EndIf 
  DeleteElement(player\inventory())
  hintCount + 1
  
  ;calculate vector to prizeroom
  Protected VECTOR.coord3D, maxVector.coord3D, max
  VECTOR\x = prizeRoom\x - player\location\x
  VECTOR\y = prizeRoom\y - player\location\y
  VECTOR\z = prizeRoom\z - player\location\z
  If Abs(VECTOR\y) >= Max(Abs(VECTOR\x), Abs(VECTOR\z)): max = 1: EndIf
  If Abs(VECTOR\z) >= Max(Abs(VECTOR\x), Abs(VECTOR\y)): max = 2: EndIf
  
  Protected *ptrVec.Integer = @VECTOR + max * SizeOf(Integer)
  Protected *ptrMaxVec.Integer = @maxVector + max * SizeOf(Integer)
  If *ptrVec\i < 0
    *ptrMaxVec\i = -1
  ElseIf *ptrVec\i > 0
    *ptrMaxVec\i = 1
  EndIf 
  
  ForEach directions()
    With directions()
      If \x = maxVector\x And \y = maxVector\y And \z = maxVector\z
        Protected dir.s = MapKey(directions())
        Break
      EndIf
    EndWith
  Next 
  display("You place the gold into a small slot on the wall.  A message appears on the wall in bright letters that says '")
  display("Go " + dir, #con_Bright_Blue, #con_Yellow)
  displayN("'.  After a few moments it fades from view.")
   
  ProcedureReturn cmd.s
EndProcedure

Procedure.s cmd_quit(cmd.s)
  Protected help.s = StringField(cmd, 1, " ")
  If help = "help"
    displayN("Usage: quit", #con_Bright_Green)
    ProcedureReturn ""
  EndIf
  
  displayN("Goodbye!")
  quitNow = #True
  ProcedureReturn ""
EndProcedure

Procedure.s cmd_alias(cmd.s)
  Protected existing.s = StringField(cmd, 1, " ")
  Protected newCommand.s = StringField(cmd, 2, " ")
  
  If existing = "help" Or existing = "" Or newCommand = ""
    displayN("Usage: alias (existing command name) (additional name for command)", #con_Bright_Green)
    ProcedureReturn ""
  EndIf
  
	If FindMapElement(*commands(), newCommand)
    displayN( "Can't redefine an existing command!")
    ProcedureReturn ""
  EndIf 
  
  If Not FindMapElement(*commands(), existing)
    displayN("The command '" + existing + "' is not currently defined")
  Else 
    Protected function =  *commands()
  EndIf
  *commands(newCommand) = function
  displayN("alias '" + newCommand + "' defined.")
  ProcedureReturn shift(cmd, 2)
EndProcedure

Procedure.s cmd_help(cmd.s)
  Protected command.s = StringField(cmd, 1, " ")
  If command = "help"
    displayN("Usage: help (command name) ... But you apparently discovered that.", #con_Bright_Green)
    ProcedureReturn ""
  ElseIf FindMapElement(*commands(), command)
    Protected function.command_prt = *commands()
    function("help " + command)
  Else
    displayN("Valid commands are:" + #CR$, #con_Bright_Green)
    ResetMap(*commands())
    Protected i, count = MapSize(*commands())
    For i = count - 1 To 1 Step - 1
      NextMapElement(*commands())
      display(MapKey(*commands()), #con_Bright_Green)
      If i <> 1
        display(", ", #con_Bright_Green)
      EndIf 
    Next 
    displayN(".", #con_Bright_Green)
  EndIf 
  ProcedureReturn ""
EndProcedure

*commands("up") = @cmd_up()
*commands("down") = @cmd_down()
*commands("north") = @cmd_north()
*commands("south") = @cmd_south()
*commands("east") = @cmd_east()
*commands("west") = @cmd_west()
*commands("attack") = @cmd_attack()
*commands("name") = @cmd_name()
*commands("take") = @cmd_take()
*commands("drop") = @cmd_drop()
*commands("equip") = @cmd_equip()
*commands("inventory") = @cmd_inventory()
*commands("ask") = @cmd_hint()
*commands("quit") = @cmd_quit()
*commands("alias") = @cmd_alias()
*commands("help") = @cmd_help()

cmd_alias("north n")
cmd_alias("south s")
cmd_alias("east e")
cmd_alias("west w")
cmd_alias("up u")
cmd_alias("down d")
cmd_alias("attack a")
cmd_alias("inventory i")
cmd_alias("inventory inv")
cmd_alias("quit q")

isConsoleAvailable = OpenConsole()
If isConsoleAvailable
  mainloop()

  display(#CR$ + #CR$ + "Press ENTER to exit"): Input()
  CloseConsole()
EndIf 
```

