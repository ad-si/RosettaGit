+++
title = "RCRPG/Haskell"
description = ""
date = 2010-11-01T19:41:17Z
aliases = []
[extra]
id = 8659
[taxonomies]
categories = []
tags = []
+++

{{collection|RCRPG}}
This Haskell version of [[:Category:RCRPG|RCRPG]] is based on the Python implementation, and is undoubtedly ugly. Someone should probably clean it up.

==Code==

```Haskell

import Char (toLower)
import qualified Data.Map as Map
import qualified Data.List as List
import qualified System.Random as Random
import Data.Maybe (fromJust, isJust, isNothing)

strToLower :: String -> String
strToLower = map toLower

join :: [String] -> String
join [] = ""
join [x] = x
join (x:xs) = x ++ ", " ++ join xs

maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just a) = [a]

addTripple :: (Int,Int,Int) -> (Int,Int,Int) -> (Int,Int,Int)
addTripple (x1,x2,x3) (y1,y2,y3) = (x1+y1,x2+y2,x3+y3)

directions = Map.fromList [
				("north", (0,-1,0)),
				("east",  (1,0,0) ),
				("south", (0,1,0) ),
				("west",  (-1,0,0)),
				("up",    (0,0,1) ),
				("down",  (0,0,-1))]
				
directionOpposites = Map.fromList [
				("north", "south"),
				("east",  "west" ),
				("south", "north"),
				("west",  "east" ),
				("up",    "down" ),
				("down",  "up"   )]

data World = World {
	aliases :: AliasMap,
	roomNames :: RoomNameMap,
	rooms :: RoomMap,
	currentPos :: (Int,Int,Int),
	inventory :: [String],
	equipped :: String,
	itemGen :: ItemGenerator
	}
newWorld x = World {aliases = newAliasMap, roomNames = newRoomNameMap, rooms = newRooms, currentPos = (0,0,0), inventory = [], equipped = "", itemGen = x}
instance Show World where
	show world = let
		thisRoom = fromJust $ Map.lookup (currentPos world) (rooms world); --Potential error point
		locationName = if (isNothing $ Map.lookup (currentPos world) (roomNames world))
			then show (currentPos world)
			else fromJust $ Map.lookup (currentPos world) (roomNames world);
		itemsText = if (null $ roomItems thisRoom) then "" else "\nOn the ground you can see: "++join (roomItems thisRoom);
		exits = map (fst) $ filter (snd) (Map.toList $ passages thisRoom);
		exitsText = if (null exits) then "None" else join exits;
		in "\nYou are at " ++ locationName ++ itemsText ++ "\nExits are: " ++ exitsText

data Room = Room { passages :: PassageMap, roomItems :: [String] } deriving Show
newRoom x = Room {passages = Map.fromList(zip (Map.keys directions) (take 6 $ repeat False)), roomItems = x}

data ItemGenerator = ItemGenerator Random.StdGen
instance Show ItemGenerator where show gen = "ItemGen"
generateItems :: ItemGenerator -> ([String], ItemGenerator)
generateItems (ItemGenerator rand) = let results = Random.randomR (0,3) rand in (itemList !! (fst results), ItemGenerator (snd results))
itemList = [[],["sledge"],["ladder"],["gold"]]

type PassageMap = Map.Map String Bool

type RoomMap = Map.Map (Int,Int,Int) Room
newRooms = Map.fromList [((0,0,0), newRoom ["sledge"])]

type AliasMap = Map.Map String [String]
newAliasMap = Map.fromList [
	("north", ["move","north"]),
	("south", ["move","south"]),
	("east",  ["move","east"]),
	("west",  ["move","west"]),
	("up",    ["move","up"]),
	("down",  ["move","down"])
	]
	
type RoomNameMap = Map.Map (Int,Int,Int) String
newRoomNameMap = Map.fromList [((0,0,0),"the starting room"),((1,1,5),"the prize room")]

replaceAliases :: [String] -> World -> [String]
replaceAliases [] world = []
replaceAliases (t:ts) world = let results = Map.lookup t (aliases world)
	in if (isJust results && fromJust results !! 0 /= t)
		then replaceAliases (fromJust results ++ ts) world
		else (t:ts)

changeWorld :: [String] -> World -> (String, World)
changeWorld (t:ts) world = let results = Map.lookup t commandMap 
	in if (isJust results)
		then fromJust results ts world
		else ("Command not found.",world)
	
commandMap :: Map.Map String ([String] -> World -> (String, World))
commandMap = Map.fromList [("move",actMove),("alias", actAlias),("inventory", actInventory),("take", actTake),
	("drop", actDrop),("equip", actEquip),("unequip", actUnequip),("name", actName),("dig", actDig)]
	
actAlias :: [String] -> World -> (String, World)
actAlias (t:ts) world = ("Alias created.", world {aliases = (Map.insert t ts (aliases world))} )
	
actInventory :: [String] -> World -> (String, World)
actInventory tokens world = let
	carry = if (null $ inventory world)
		then "You aren't carrying anything."
		else "Carrying: " ++ join (inventory world);
	hold = if (null $ equipped world)
		then ""
		else "\nHolding: " ++ equipped world
	in (carry ++ hold, world)
	
actTake :: [String] -> World -> (String, World)
actTake tokens world = let
	oldRoom = fromJust $ Map.lookup (currentPos world) (rooms world); --Potential error point
	grabbed = if (tokens == ["all"])
		then roomItems oldRoom
		else maybeToList $ List.find (==(unwords tokens)) (roomItems oldRoom);
	changedInventory = (inventory world) ++ grabbed;
	changedRoom = Room (passages oldRoom) ((roomItems oldRoom) List.\\ grabbed);
	changedRooms = Map.insert (currentPos world) changedRoom (rooms world);
	changedWorld = if (null grabbed) then world else world{rooms = changedRooms, inventory = changedInventory};
	message = if (null grabbed)
		then if (tokens == ["all"])
			then "There is nothing in the room to take."
			else "Item not found."
		else if (tokens == ["all"])
			then "You now have everything in the room."
			else "Taken.";
	in (message, changedWorld)

actDrop :: [String] -> World -> (String, World)
actDrop tokens world = let
	oldRoom = fromJust $ Map.lookup (currentPos world) (rooms world); --Potential error point
	dropped = if (tokens == ["all"])
		then inventory world
		else maybeToList $ List.find (==(unwords tokens)) (inventory world);
	changedInventory = (inventory world) List.\\ dropped;
	changedRoom = Room (passages oldRoom) ((roomItems oldRoom) ++ dropped);
	changedRooms = Map.insert (currentPos world) changedRoom (rooms world);
	changedWorld = if (null dropped) then world else world{rooms = changedRooms, inventory = changedInventory};
	message = if (tokens == ["all"])
		then "Everything dropped."
		else if (null dropped)
			then "Could not find item in inventory."
			else "Dropped."
	in (message, changedWorld)

actEquip :: [String] -> World -> (String, World)
actEquip tokens world = let
	actEquipped = List.find (==(unwords tokens)) (inventory world);
	actUnequipped = if (isJust actEquipped && equipped world /= "") then [equipped world] else [];
	changedEquipped = fromJust actEquipped --Potential error point
	changedInventory = ((inventory world) List.\\ [(unwords tokens)]) ++ actUnequipped;
	changedWorld = if (isNothing actEquipped) then world else world{inventory = changedInventory, equipped = changedEquipped};
	message = if (isJust actEquipped)
		then "Equipped "++ fromJust actEquipped ++"."
		else "You aren't carrying that."
	in (message, changedWorld)

actUnequip :: [String] -> World -> (String, World)
actUnequip tokens world = let
	actUnequipped = if (equipped world /= "") then [equipped world] else [];
	changedInventory = (inventory world) ++ actUnequipped;
	changedWorld = if (null actUnequipped) then world else world{inventory = changedInventory, equipped = ""};
	message = if (null actUnequipped)
		then "You aren't equipped with anything."
		else "Unequipped "++ head actUnequipped ++"."
	in (message, changedWorld)

actName :: [String] -> World -> (String, World)
actName tokens world = ( "Room renamed", world{roomNames = (Map.insert (currentPos world) (unwords tokens) (roomNames world))} )

actDig :: [String] -> World -> (String, World)
actDig tokens world = let
	direction = unwords tokens;
	isValidDirection = direction `Map.member` directions;
	thisRoom = fromJust $ Map.lookup (currentPos world) (rooms world); --Potential error point
	potentialExceptions = [
		((equipped world /= "sledge"), "You don't have a digging tool equipped."),
		((not isValidDirection), "That's not a direction."),
		((isValidDirection && fromJust (Map.lookup direction (passages thisRoom))), "Already a tunnel that way.")];
	exception = List.find (fst) potentialExceptions;
	in if (isJust exception)
		then ((snd $ fromJust exception), world)
		else ("You've dug a tunnel.", worldDig world direction)

worldDig :: World -> String -> World
worldDig world direction = let
	newRandom = generateItems $ itemGen world;
	randomItems = fst newRandom;
	newItemGen = snd newRandom;
	oppositeDirection = fromJust $ Map.lookup direction directionOpposites; --Possible error point
	destinationPos = (currentPos world) `addTripple` (fromJust (Map.lookup direction directions));
	currentRoom = fromJust $ Map.lookup (currentPos world) (rooms world);
	destinationLookup = Map.lookup destinationPos (rooms world);
	destinationRoom = if (isNothing destinationLookup)
		then newRoom randomItems
		else fromJust destinationLookup;
	newCurrentRoom = Room (Map.insert direction True (passages currentRoom)) (roomItems currentRoom);
	newDestinationRoom = Room (Map.insert oppositeDirection True (passages destinationRoom)) (roomItems destinationRoom);
	changedRooms = Map.insert (currentPos world) newCurrentRoom (Map.insert destinationPos newDestinationRoom (rooms world))
	in world{rooms = changedRooms, itemGen = newItemGen}
	
actMove :: [String] -> World -> (String, World)
actMove tokens world = let
	direction = unwords tokens;
	isValidDirection = direction `Map.member` directions;
	thisRoom = fromJust $ Map.lookup (currentPos world) (rooms world); --Potential error point
	potentialExceptions = [
		((not isValidDirection), "That's not a direction."),
		((isValidDirection && not (fromJust (Map.lookup direction (passages thisRoom)))), "Can't go that way."),
		((direction == "up" && isNothing (List.find (=="ladder") (roomItems thisRoom))), "You'll need a ladder in this room to go up.")];
	exception = List.find (fst) potentialExceptions;
	destinationPos = (currentPos world) `addTripple` (fromJust (Map.lookup direction directions));
	in if (isJust exception)
		then ((snd $ fromJust exception), world)
		else ("You go "++direction++".", world{currentPos = destinationPos})

takeInput :: World -> IO ()
takeInput world = do
	print world
	input <- getLine
	let tokens = replaceAliases (words $ strToLower input) world
	if (length tokens == 0)
		then takeInput world
		else
			if (tokens !! 0 == "quit")
				then return ()
				else do
					let update = changeWorld tokens world  -- Returns a tupple of the form (String, World)
					putStrLn $ fst update                  -- Report the results of the update
					takeInput (snd update)                 -- Continue game

main = do
	putStrLn "Welcome to the dungeon!\nGrab the sledge and make your way to room 1,1,5 for a non-existant prize!"
	rand <- Random.newStdGen
	takeInput $ newWorld (ItemGenerator rand)
	putStrLn "Thanks for playing!"
	junk <- getLine
	return ()

```

