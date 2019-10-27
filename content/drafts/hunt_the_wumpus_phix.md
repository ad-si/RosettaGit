+++
title = "Hunt The Wumpus/Phix"
description = ""
date = 2015-08-07T20:47:30Z
aliases = []
[extra]
id = 19482
[taxonomies]
categories = []
tags = []
+++

{{collection|Hunt_The_Wumpus}}
[[Phix]] version of [[:Category:Hunt_The_Wumpus|Hunt The Wumpus]] .

==Code==

```Phix

--Hunt the Wumpus
--the classic 70's game
--converted from the BASIC http://www.atariarchives.org/morebasicgames/showpage.php?page=179

--/*
include std/console.e
include std/text.e
include std/regex.e as re
include std/get.e
--*/
--/**/include get.e

--trying to stick closer to the spirit of the BASIC Wumpus!
constant YOU = 1, WUMPUS = 2, PITS1 = 3, PITS2 = 4, BATS1 = 5, BATS2 = 6, ARROWS = 7

---------------------------------------------------------------------------------------------
procedure instructions()
---------------------------------------------------------------------------------------------
    clear_screen()

    puts(1, "Welcome to Hunt the Wumpus\n")
    puts(1, "The wumpus lives in a cave of 20 rooms. Each room has 3 tunnels\n")
    puts(1, "leading to other rooms. (Look at a dodecahedron to see how this\n")
    puts(1, "works - if you don't know what a dodecahedron is, ask someone).\n")
    puts(1, "\n")
    puts(1, "Hazards\n")
    puts(1, "Bottomless pits - 2 rooms have bottomless pits in them. If you\n")
    puts(1, "  there you fall in (and lose). \n")
    puts(1, "Superbats - 2 other rooms have superbats. If you go there, a \n")
    puts(1, "  superbat grabs you and takes you to some other random room \n")
    puts(1, "  (which could be..... troublesome!\n")
    puts(1, "\n")
    puts(1, "The wumpus is not bothered by such hazards (he has sucker feet,\n")
    puts(1, "and is too big to lift). Usually he is asleep. Two things wake him\n")
    puts(1, "up : your entering his room, and your shooting an arrow.\n")
    puts(1, "If the wumpus wakes up, he moves one room (P=0.75) or stays\n")
    puts(1, "put (P=0.25). After that, if he is in the same room as you\n")
    puts(1, "he eats you up (and you lose), or he goes back to sleep!\n")
    puts(1, "\n")

--DEV
--/**/  puts(1,"Press any key..........")
--/**/  if wait_key() then end if
--/**/  puts(1,"\n")
--/*
    any_key("Press any key..........")
--*/

    puts(1, "Each turn, you may move, or shoot a crooked arrow.\n")
    puts(1, "  Moving - you can move one room (through a tunnel)\n")
    puts(1, "  Shooting - you have 5 arrows - you lose when you run out.\n")
    puts(1, "  Each arrow can go through 1 to 5 rooms, you aim by telling\n")
    puts(1, "  the computer the rooms you want it to go through. If the arrow\n")
    puts(1, "  can't go that way (no tunnel, it moves at random to an adjacent \n")
    puts(1, "  room. If the arrow hits the wumpus, you win, if it hits you,\n")
    puts(1, "  you lose.\n")
    puts(1, "\n")
    puts(1, "When you are 1 room away from a hazard, the computer will say\n")
    puts(1, "  Wumpus   - I smell a wumpus\n")
    puts(1, "  Bats     - Bats nearby\n")
    puts(1, "  Pit      - I feel a draft\n")
    puts(1, "\n")
    puts(1, "Good luck!\n")

    if wait_key() then end if

end procedure

---------------------------------------------------------------------------------------------
function initialise_caves()
    ---------------------------------------------------------------------------------------------
    -- Remember to swap row and column from the BASIC source in future references
    return {{2,5,8}, {1, 3, 10}, {2, 4, 12}, {3, 5, 14}, {1, 4, 6},
            {5, 7, 15}, {6, 8, 17}, {1, 7, 9}, {8, 10, 18}, {2, 9, 11},
            {10, 12, 19}, {3, 11, 13}, {12, 14, 20}, {4, 13, 15}, {6, 14, 16},
            {15, 17, 20}, {7, 16, 18}, {18, 9, 17}, {19, 11, 18}, {20, 13, 16, 19}}
end function

---------------------------------------------------------------------------------------------
function ask(sequence prompt, sequence filter)
---------------------------------------------------------------------------------------------
/*
print prompt and wait for an answer
filter is a regular expression to filter answer
*/
--/**/ -- Phix: filter is either "[ms]" or "[yn]", do it without regex...
--/*
re:regex valid_answer=re:new(filter,CASELESS)
--*/
sequence answer

    puts(1,prompt)
--/**/ filter = filter[2..-2]
    while 1 do
        answer = gets(0)
        answer = answer[1..$-1]
--/**/  answer = lower(answer)
--/**/  if length(answer)=1 and find(answer[1],filter) then
--/*
        if sequence(re:find(valid_answer,answer)) then
--*/
            exit
        else
            puts(1,"\ninvalid answer, try again\n"&prompt)
        end if
    end while
    return answer
end function

---------------------------------------------------------------------------------------------
function mkStatusList()
---------------------------------------------------------------------------------------------
sequence status = {0,0,0,0,0,0,5} --setting [7] to arrow count
    sequence list = {1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20}
    integer r
    for i = 1 to 6 do
        r = rand(20) -- select a number between i and 20 inclusive
        status[i] = list[r]  -- copy the value to status
        list[r] = list[i]    -- removing list[r] from the list
    end for
    return status
end function

---------------------------------------------------------------------------------------------
procedure show_where_you_are(sequence status, sequence caves)
    ---------------------------------------------------------------------------------------------
    printf(1, "You are in room %d\n", {status[YOU]})

    printf(1, "There are tunnels leading to rooms ", {})

    for i = 1 to length(caves[status[YOU]]) do
        printf(1, "%d ", {caves[status[YOU]][i]})
    end for

    puts(1, "\n")

end procedure

---------------------------------------------------------------------------------------------
procedure detect_hazards(sequence status, sequence caves)
---------------------------------------------------------------------------------------------
sequence hzd = {}

    if find(status[BATS1],caves[status[YOU]]) or find(status[BATS2],caves[status[YOU]]) then
        hzd = append(hzd, "Bats nearby!")
    end if

    if find(status[PITS1],caves[status[YOU]]) or find(status[PITS2],caves[status[YOU]]) then
        hzd = append(hzd, "I feel a draft!")
    end if

    if find(status[WUMPUS],caves[status[YOU]]) then
        hzd = append(hzd, "I smell a wumpus!")
    end if

    if length(hzd) = 0 then
        return
    end if

    for i = 1 to length(hzd) do
        puts(1, hzd[i] & "\n")
    end for

end procedure

---------------------------------------------------------------------------------------------
function move_hunter(sequence status, sequence caves)
---------------------------------------------------------------------------------------------
object dest_room
sequence s

    s = prompt_string("\nMove to which room? ")
    dest_room = value(s)
    dest_room = dest_room[2]

    if find(dest_room, caves[status[YOU]] ) = 0 then
        puts(1, "You cannot get to that room from here!\n\n")
        return status
    end if

    puts(1, "\n")
    status[YOU] = dest_room
    return status
end function

---------------------------------------------------------------------------------------------
function detect_entered_hazards(sequence status, sequence caves)
    ---------------------------------------------------------------------------------------------
    --bats
    if status[YOU] = status[PITS1] or status[YOU] = status[PITS2] then
        puts(1, "AAAAARRGGHHHHH! Fell in a pit!\n")
        status[YOU] = -1
        return status
    end if

    if status[YOU] = status[BATS1] or status[YOU] = status[BATS2] then
        puts(1, "Bats grabbed you - off to another room!\n")
        while 1 do
            status[YOU] = rand(20)
            --don't want the hunter in a room with the wumpus, bats or pits!
            if status[YOU] = status[BATS1] or status[YOU] = status[BATS1] or
            status[YOU] = status[PITS1] or status[YOU] = status[PITS1] or
            status[YOU] = status[WUMPUS] then
                continue
            else
                exit
            end if
        end while
        return status
    end if

    if status[YOU] = status[WUMPUS] then
        puts(1, "Munch munch, gobble gobble. Wumpus ate you!\n")
        status[YOU] = -1
    end if

    return status
end function

---------------------------------------------------------------------------------------------
function shoot_arrow(sequence status, sequence caves)
--fires an arrow through a number of rooms
--if you miss the wumpus, it wakes up, and moves to another room
--if you hit it, the wumpus is dead.
---------------------------------------------------------------------------------------------
object nr, ar, current_arrow_room
integer nwr

    status[ARROWS] -= 1
    current_arrow_room = status[YOU]

    puts(1, "\n")

    while 1 do
        nr = prompt_string("No. of rooms : ")
        nr = value(nr)
        nr = nr[2]
        if nr >= 1 and nr <= 5 then exit end if
        puts(1, "Between 1 and 5 rooms please.\n")
    end while

    for i = 1 to nr do
        while 1 do
            --? caves[current_arrow_room]  --uncomment this for an easier game
            ar = prompt_string("Which room : ")
            ar = value(ar)
            ar = ar[2]

            if find(ar, caves[current_arrow_room]) = 0 then
                puts(1, "Arrows aren't that crooked - try another room!\n")
                continue
            end if
            exit
        end while

        current_arrow_room = ar

        --check for pesence of wumpus
        if current_arrow_room = status[WUMPUS] then
            status[WUMPUS] = -1
            return status
        end if

        --check if you've shot back into your room
        if current_arrow_room = status[YOU] then
            puts(1, "Ouch, you shot yourself - luckily in the head, no damage!")
            exit
        end if
    end for

    --didn't get the Wumpus then, but woke it up
    nwr = rand(3)
    status[WUMPUS] = caves[status[WUMPUS]][nwr]
    if status[WUMPUS] = status[YOU] then
        puts(1, "Munch munch, gobble gobble. Wumpus ate you!\n")
        status[YOU] = -1
        return status
    end if

    if status[ARROWS] = 0 then
        status[YOU] = -1
        puts(1, "You ran out of arrows!\n")
    end if

    return status
end function

---------------------------------------------------------------------------------------------
procedure start_game(sequence status, sequence caves)
---------------------------------------------------------------------------------------------
sequence response

    while 1 do
        show_where_you_are(status, caves)
        detect_hazards(status, caves)
        response = ask("Move or shoot (m/s)", "[ms]")

        if response[1] = 'm' then
            status = move_hunter(status, caves)
            status = detect_entered_hazards(status, caves)
        else
            status = shoot_arrow(status, caves)
        end if

        if status[YOU] = -1 then
            puts(1, "You're dead!\n")
            return
        end if

        if status[WUMPUS] = -1 then
            puts(1, "You heartless fiend - you killed a defenceless Wumpus!\n")
            return
        end if

    end while

end procedure
---------------------------------------------------------------------------------------------
procedure main()
---------------------------------------------------------------------------------------------
sequence inp
sequence caves
sequence status

    inp = upper(prompt_string("Do you want instructions? (y/n)"))
    if inp[1] = 'Y' then
        instructions()
    end if

    while 1 do
        caves = initialise_caves()
        status = mkStatusList()
        start_game(status, caves)
        inp = upper(ask("Play again?", "[yn]"))
        if inp[1] = 'N' then exit end if
    end while
end procedure

main()


```

