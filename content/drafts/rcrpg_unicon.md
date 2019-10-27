+++
title = "RCRPG/Unicon"
description = ""
date = 2014-02-18T22:08:27Z
aliases = []
[extra]
id = 9765
[taxonomies]
categories = []
tags = []
+++

{{collection|RCRPG}}
The following program implements the RCRPG game in Unicon.  

It was fun and fairly fast to write.  After some initial simple bugs, I had to deal with relatively few oversights.  The most major were forgetting to back link rooms and overwriting the prize room after tunneling into it.

This version was developed from the blog description and some skimming of other solutions for messaging and consistency in handling under specified features.  I believe it's a bit chattier than some of the other versions.

There are a couple embellishments.  
* Pre-loading simple aliases for ease of use. 
* A magic word for when all hope is lost. Every good adventure like game needs a magic word.
* If you name a room but forget to give a name, a random one will be chosen.
* Things like a low probability of breaking a sledge and taking multiple hits to break through were probably stolen from other variants.

There are quirks, probably some undiscovered ones at that.  Most notably the alias command only works on the command verb.  The facility could be extended but would need some safeties to prevent you from doing things like redefining north. 

Programming style is slightly lazy.  Locals aren't declared, most procedures fail rather than return a value even when they do the write thing.  Only a few globals are used.

The implementation uses a number of minor Unicon extensions over Icon.  The main extension(s) used is probably the extended delete function to remove list elements. It may reference records by subscripts but I think that is removed.

The main program follows:

```Unicon
record roominfo(x,y,z,name,items,n,s,e,w,u,d)    # about rooms
record urinfo(here,carrying,using)               # about your character

global Start, Finish, Directions, UR, Aliases, Command

invocable all                                    # needed for command verbs

procedure main()                                 #:  main procedure for RCRPG
   Start  := AddRoom(0,0,0,"the starting room","sledge")
   Finish := AddRoom(1,1,5,"the prize room","gold")
   Directions := table()
   every x := "north"|"east"|"south"|"west"|"up"|"down" & Directions[x|x[1]] := x
   UR := urinfo(Start,[])
   write("Welcome to RCRPG (Icon/Unicon version)!\n\n_
         Your goal is make your way to room 1,1,5.\n_
         Hint: You'll need the sledge hammer.\n")
   Help()
   (\Alias_Setup)()                                           # optional
   repeat Describe() & DoCommand()
end
```


The following implement the navigation procedures:

```Unicon
procedure AddRoom(x,y,z,name,item[])             #: Create room w/item(s)
   if *item = 0 then push(item, \?[&null,"sledge","ladder","gold"]) # randomize
   return roominfo(x,y,z,\name|"",item)   
end   

procedure Attack()                               #:  Attack
   whereto := Directions[Command[2]]
   if /NextRoom(whereto) then 
      if \UR.using == "sledge" then  {
         if ?0 > .1 then {                    # 90% chance of success
            x := UR.here.x + ((whereto=="east",1) | (whereto=="west",-1) | 0)
            y := UR.here.y + ((whereto=="north",1) | (whereto=="south",-1) | 0)
            z := UR.here.z + ((whereto=="up",1) | (whereto=="down",-1) | 0)    
            
            r := if x = Finish.x & y = Finish.y & z = Finish.z then Finish
            else AddRoom(x,y,z)
            
            NextRoom(whereto) := r
            case whereto of { "up":r.d; "down":r.u; 
               "north":r.s; "east":r.w; "south":r.n; "west":r.e } := UR.here  
            write("You created a tunnel to another room.")
            }
         else write("You're almost through, try again.")
         if ?0 < .1 then {                    # 10% chance of breakage
            UR.using := &null
            put(UR.here.items,"broken hammer")
            write("You broke your sledge hammer and left it on the ground.")
            write(if \Abracadabra then "There is a hint of magic in the air."
                  else "Hope you found another.")
            }
         }
      else write("You have no digging equipment.")
   else if not NextRoom(whereto) then write("I don't know that direction.")
   else write("There is already an opening in that direction.")
end  

procedure Go()                                   #:  Go in a direction
   if room := NextRoom(whereto := \Directions[Command[2]]) then {
      HandleLadder(whereto)    
      (UR.here := \room) | write("There is no opening going ",whereto) 
      }      
   else write("I don't know that direction.")      
end   

procedure NextRoom(whereto)                      #: return next room or fail
   return case Directions[whereto] of {
      "up"    : UR.here.u
      "down"  : UR.here.d
      "north" : UR.here.n
      "east"  : UR.here.e
      "south" : UR.here.s
      "west"  : UR.here.w    
      }  
end
```


The following are the command processing and non-navigational commands:

```Unicon
procedure Alias()                                #: make a command alias
   /Aliases := table()
   if Aliases[Command[2]] := Command[3] then 
      write(Command[2]," is now an alias for ",Command[3])
   else write("What did you want me to alias?")
end   

procedure Describe()                             #: Describe where U R
static lastroom
   if lastroom ~===:= UR.here then Look()        # don't repeat description
   if UR.here === Finish then stop("Congratulations! You won.\nCome again.")
return                                           # needed for main loop   
end  

procedure Drop()                                 #: drop item
   room := UR.here
   if item := Command[2] then 
      if item == "all" then {
         while put(room.items, get(UR.carrying))
         put(room.items,\UR.using) & UR.using := &null
         write("Everything you had is on the floor.")
         }
      else if item == \UR.using then {
         UR.using := &null   
         put(room.items,item)
         write("You dropped the ",item)
         }
      else if item == !UR.carrying then {
         delete(UR.carrying,index(UR.carrying,item))
         put(room.items,item)
         write("You dropped the ",item)
         }      
      else write("You don't have any ",item)
   else write("What do you want me to drop?")
end   

procedure DoCommand(command)                     #: Do the command
static verbs
initial {
   verbs := table()
   every p := ![Alias,Attack,Drop,Equip,Go,Help,Inventory,Look,Name,Take,Abracadabra] do
       verbs[map(image(\p))[*"procedure X":0]] := p
   }

   Command := []
   ( writes("> "), trim(pretrim(read()))) ? until pos(0) do {
      put(Command,tab(upto(' \t')|0))
      tab(many(' \t'))
      }

   repeat {
      if \Directions[\Command[1]] then push(Command,"go")
      if p := \verbs[\Command[1]] then {
         p()                                                   # call command
         return
         }
      else if Command[1] := \(\Aliases)[Command[1]] then next  # aliased, retry 
      else if Command[1] == "quit" then stop("Quitting.")
      else break write("I;m sorry I don't know that command.")
      }
end   

procedure Equip()                                #:  Equip
   if item := Command[2] then 
      if item == (\UR.using | !UR.carrying) then {
         put(UR.carrying, \UR.using)                            # save equip
         delete(UR.carrying,index(UR.carrying,UR.using:=item)) # move to using  
         write("You are now equipped with a ",UR.using)         
         }
      else write("You aren't carrying a ",item)
   else write("Equip with what?")
end   
        
procedure HandleLadder(whereto)                         #:  Handle ladder
   if whereto == "up" then {                            # only for up
      if (item := "ladder") == \UR.using then 
         put(UR.here.items,item) & UR.using := &null
      else if item == !UR.carrying then 
         put(UR.here.items,item) & delete(UR.carrying,index(UR.carrying,item))
      if item == !UR.here.items then return
      else write("You can't go up without a ladder in the room.")
      }
end   

procedure Help()                                 #: give help
write("Commands:\n_
(go) north, south, east, west, up, down - \n_
\tmoves you in the direction specified if there is an exit\n_
\ta ladder is required to go up or down\n_
look - take a look around\n_
inventory - Show everything you're carrying\n_
equip (item name) - Equip the item in question\n_
attack (direction) - attack in the direction specified\n_
drop (item name)|all - Drop the item specified or everything you'e carrying\n_
take (item name)|all - Take the item specified or everything in the room\n_
name (name) - Rename the room to whatever you want to call it\n_
alias (existing command) (new name) -\n_
\tCreate an alias for an existing command.\n_
\tYou can't redefine an existing command.\n_
quit - useful if you broke your only sledge hammer\n")
end   #

procedure Inventory()                            #: show inventory
   if /UR.using & (*UR.carrying = 0) then 
      write("You haven't got anything with you.")
   else { 
      write("You have ",ShowList((*UR.carrying>0,UR.carrying))," in your pack.")
      write("You are holding a ",\UR.using," in your hand.")
      }           
end   

procedure Look()                                 #: Look around and describe    
   r := UR.here
   write(sprintf("You are in the %s(%d,%d,%d) room.",r.name,r.x,r.y,r.z))
   if *r.items>0 then write("On the ground you can see " || ShowList(r.items))
   every insert(dl := set(), (\NextRoom(d := !Directions), d)) 
   if *dl>0 then write("There are openings in the directions ", ShowList(dl))
   else write("There are no openings.")
end   

procedure Name()                                 #:  Name the current room
   if UR.here.name := \Command[2] then
      write("The room is now called ",UR.here.name)
   else write("You didn't provide a name.  I think I shall name it ",
               UR.here.name := ?["Fred","Ringo","George","John","Paul","Ozzy",
                                 "Hamlet","Kermit","Spot","Minka", "Archie"])
end

procedure Take()                                 #: take item
   room := UR.here
   if item := Command[2] then 
      if item == "all" then {
         while put(UR.carrying,get(room.items))
         write("You have everything in the room greedy!")
         }
      else if delete(room.items,index(room.items,item)) then {
         put(UR.carrying,item)
         write("You now have the ",item)
         }
      else write("I see no ",item)
   else write("What do you want me to take?")
end 
```


The following are miscellaneous supprt procedures:

```Unicon
procedure ShowList(X,c)                          #: return c separated elements
   /c := ", "
   every (s := "") ||:= !X || c
   return s[1:-*c]
end   

procedure index(L, x)		                      #: generate indices for x
   every if x === L[i := 1 to *L] then suspend i
end
```


The following are minor embellishments that are silently linked.  If they are left out, the remaining code should be able to continue on unaffected.

```Unicon
procedure Abracadabra()                          #: magic word
static uses
initial uses := 3
  write("That is very old magic, I'm not sure it still works ...")
  if 0 < (uses -:= 1) then {
     write("... Poof! A sledge appears in a cloud of purple smoke")
     put(UR.here.items,"sledge")
     }
   else if uses = 0 then write("... No. Nothing happens.")
   else if uses = -1 then write("... You now have a fuchsai tail!!")
   else write("... Thankfully nothing else happens.")
end

procedure Alias_Setup()                          #: ease of use aliases
   /Aliases := table()
   every a := key(Directions) | !["inventory","attack","take","look"] do
      Aliases[a[1]] := a
   every Aliases[!["hocus","pocus","shazzam","walla","xyzzy"]] := "abracadabra"
end
```

