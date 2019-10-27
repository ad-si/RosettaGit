+++
title = "RCRPG/Phix"
description = ""
date = 2019-01-20T18:53:34Z
aliases = []
[extra]
id = 22146
[taxonomies]
categories = []
tags = []
+++

{{collection|RCRPG}}

Most of the complexity is contained in the get_command() routine. I went
with a bespoke auto-select-by-initial-letter affair rather than forcing 
the full commands to be (exactly) spelt out very time. You get used to it,
and can explore all available command options using the arrow keys.

I tried putting commands in a dictionary but sequences proved much easier.
In contrast the rooms were always much better off in a dictionary, not least
because indexing {0,0,0}/extendible in all directions could be problematic.

Notable features are the test for cyclic references in aliases (which can
be individual words or any-length series of other complete commands), the
ability to completely remove all the original/builtin commands/words, and
showing a warning when first letters of commands/items/directions clash.

Always having a sledge in the start room was just far too dull for words,
and I felt it was my sworn duty to invent some way to get yourself killed!


```Phix
-- demo\rosetta\RCRPG.exw
constant start_text = """
Welcome to RCRPG!
Press ? for help

"""
-- (nb: the leading space of help_text[1] is intentional)
constant help_text = """ 
Welcome to RCRPG!

### ===========


Command input is via single characters, eg n -> north, tg-> take gold.
You do however need to hit return to confirm your choice.
The left and right arrow keys cycle through permitted options.
Use the backspace key to erase any accidentally selected commands.
The up and down arror keys cycle through command history.

Available directions are north, south, east, east, west, up, and down.
Take or drop the items sledge, ladder, and gold, which appear randomly.
Make a tunnel by attacking a direction.
You need to equip a sledge before you can attack.
You need a ladder to go up, but you cannot take it between floors.
The inventory command lists the items you are carrying.

You can alias a single word or an entire series of commands - type 
a(lias)<return> and follow the prompts (rather than all on one line).
More help can be found by entering a(lias)<return> then ?<return>

Make your way to {1,1,5} for the glorious prize of ending this torture!
There is also a way to kill yourself instead - can you figure out how?

"""

constant alias_help = """
        Example of using the alias command
        
### ============================

Command:alias
Enter alias name(? for help):shiney_shiney
Alias:gold
Command:inventory
You are carrying one piece of gold.
Command:alias
Enter alias name(? for help):gold
Delete existing command?(Y/N):Y
Alias:
Command:inventory
You are carrying one piece of shiney_shiney.
Command:

You can delete an alias or even an original command by setting it as 
an alias, confirm the delete, then hit return to abandon adding the 
replacement. A check is made to prevent the deletion of the last of
each kind, since that would make the game completely unplayable.

As shown the item gold is no more, you now use/see shiney_shiney, 
that is everywhere else except for this help text. Deleting an alias
or original also expunges it from the command history.

The above aliased an item; if you alias a complete command, you get 
reprompted with "Alias:", allowing one alias to daisy-chain several.

You can even alias the alias command itself, eg as "rename", but I 
have not bothered to test that very thoroughly.

While you can alias "smack bitch" to "take gold", obviously you would 
instead need "smack" to "take" and "bitch" to "gold" as two separate
aliases to allow "smack ladder" and "drop bitch".

Note that commands and aliases are automatically selected by their
first letter, and there should be no conflicts between shared first 
letters of any (aliased) actions and directions/items, eg "dig down" 
might prevent "drop" from being selected, apart from via left/right 
arrow keys, eg a 'd' changes "dig" -> "dig down" rather than "drop".
A warning is displayed if the use of "alias" breaks this rule (and
if the warning stops after a delete, you have solved the conflict).

Aliases can invoke other aliases. Circular references are detected
and playback is prohibited until any such issues are resolved (by
deleting or overiding the offending aliases).

The currently defined commands and aliases are:
"""

enum SINGLE_WORD_COMMAND, DIRECTION, NEEDS_DIRECTION, ITEM, NEEDS_ITEM, ALIAS
-- (nb: NEEDS_DIRECTION==DIRECTION+1, NEEDS_ITEM==ITEM+1)

enum N,W,U,D,E,S,INVENTORY,ATTACK,TAKE,DROP,MAKEALIAS,EQUIP,SLEDGE,LADDER,GOLD,ALL,QUIT
-- (nb: N/W/U/D/E/S (tee hee) must be 1..6, N<->S etc via 7-d)
-- (nb: SLEDGE/LADDER/GOLD together in that order)
-- (nb: This enum is fixed, whereas eg "north" can be aliased away}

-- order must match enum:
constant moves = {{+1, 0, 0},   -- N
                  { 0,-1, 0},   -- W
                  { 0, 0,+1},   -- U
                  { 0, 0,-1},   -- D
                  { 0,+1, 0},   -- E
                  {-1, 0, 0}}   -- S

-- order not important:
sequence cmds = {{"north",DIRECTION,N},
                 {"west", DIRECTION,W},
                 {"up",   DIRECTION,U},
                 {"down", DIRECTION,D},
                 {"east", DIRECTION,E},
                 {"south",DIRECTION,S},
                 {"inventory",SINGLE_WORD_COMMAND,INVENTORY},
--               {"dig",NEEDS_DIRECTION,ATTACK}, -- no (prevents drop)
                 {"attack",NEEDS_DIRECTION,ATTACK},
                 {"take", NEEDS_ITEM,TAKE},
                 {"drop", NEEDS_ITEM,DROP},
                 {"alias",SINGLE_WORD_COMMAND,MAKEALIAS},
                 {"equip",NEEDS_ITEM,EQUIP},
                 {"sledge",ITEM,SLEDGE},
--               {"pickaxe",ITEM,SLEDGE},
                 {"ladder",ITEM,LADDER},
                 {"gold",ITEM,GOLD},
                 {"all",ITEM,ALL},
                 {"quit",SINGLE_WORD_COMMAND,QUIT},
                 {"n",ALIAS,{{"attack","north"},{"north"}}}}, -- (eg)
         {commands,kinds,data} = columnize(cmds)

procedure clear_prompt(string prompt, sequence words)
    string s = prompt&join(words)
    puts(1,"\r"&repeat(' ',length(s))&"\r")
end procedure

function next_cmd(integer k, shift)
    k += shift
    if k=0 then
        k = length(cmds)
    elsif k>length(cmds) then
        k = 1
    end if
    return k
end function

function begins_with(integer ch, kind, bool isalias)
    if kind=ALIAS then ?9/0 end if -- sanity check
    sequence items = {}
    for i=1 to length(commands) do
        if ((kind=0 and (kinds[i]!=ITEM or isalias)) or
             kinds[i]=kind)
        and commands[i][1]=ch then
            items = append(items,commands[i])
        end if
    end for
    return items
end function

sequence history = {}

function get_command(string prompt="")
--
-- Selects by ititial letter, eg tg -> "take gold" (<return> confirms)
--
-- Returns eg {"take","gold"}, or {} to quit (already confirmed)
--
-- Could probably be improved by allowing full entry of eg "take gold" 
-- to ignore entered characters that match auto-fills from the t and g.
--
    string s = ""
    sequence words = {}, items
    integer last, this
    bool show = false, bExtend, isalias = (prompt="Alias:")
    integer k, shift, hdx = 0
    puts(1,prompt)
    while 1 do
        integer ch = lower(wait_key())
        if ch='\r' then
            if isalias then exit end if
            if length(words) then
                if find(last,{NEEDS_DIRECTION,NEEDS_ITEM}) then
                    clear_prompt(prompt,words)
                    string what = iff(last=NEEDS_ITEM?"item":"direction")
                    printf(1,"missing %s\n",{what})
                    show = true
                else
                    exit
                end if
            else
                clear_prompt(prompt,words)
                puts(1,"Quit?(Y/N):")
                ch = upper(wait_key())
                puts(1,ch)
                if ch='Y' then exit end if
                puts(1,"\r            \r")
                show = true
            end if
        elsif ch=#1B
           or ch='\b' then
            if ch=#1B then -- escape
                if length(words)=0 then exit end if
                -- (else treat as \b)
            end if
            if length(words)>0 then
                clear_prompt(prompt,words)
                words = words[1..$-1]
                show = true
            end if
        elsif ch='?' then
            clear_prompt(prompt,words)
            puts(1,help_text)
            show = true
        elsif ch='!' then
            ?9/0
        elsif ch>=' ' and ch<='~' then
            items = {}
            if length(words)
            and find(last,{NEEDS_DIRECTION,NEEDS_ITEM}) then
                items = begins_with(ch,last-1,false)
                bExtend = true
            end if
            if length(items)=0 then
                items = begins_with(ch,iff(length(words)?last:0),isalias)
                if (length(items)=0 and length(words)=1)
                or items=words then
                    items = begins_with(ch,0,isalias)
                    bExtend = false
                else
                    bExtend = length(words)=0
                end if
            end if
            if length(items) then
                k = iff(length(words)=0?0:find(words[$],items))
                if k=0 or k=length(items) then
                    k = 1
                else
                    k += 1
                end if
                string key = items[k]
                if bExtend then
                    if length(words) then puts(1," ") end if
                    words = append(words,key)
                    puts(1,key)
                else
                    clear_prompt(prompt,words)
                    words[$] = key
                    show = true
                end if
                last = kinds[find(key,commands)]
                if last=ALIAS then last = 0 end if
            end if
        elsif ch=331        -- leftarrow
           or ch=333 then   -- rightarrow
            shift = iff(ch=331?-1:+1)
            clear_prompt(prompt,words)
            if length(words) then
                k = find(words[$],commands)
                if k=0 then ?9/0 end if
            else
                k = iff(ch=331?length(cmds)+1:0)
            end if
            if length(words)=2 then
                while true do
                    k = next_cmd(k,shift)
                    if kinds[k]=last then
                        words[$] = commands[k]
                        exit
                    end if
                end while
            else
                while true do
                    k = next_cmd(k,shift)
                    last = kinds[k]
                    if last=ALIAS then last = 0 end if
                    if last!=ITEM then
                        words = {commands[k]}
                        exit
                    end if
                end while
            end if
            show = true
        elsif ch=328        -- uparrow
           or ch=336 then   -- downarrow
            clear_prompt(prompt,words)
            if length(history)=0 then
                puts(1,"no history\n")
            else
                shift = iff(ch=328?-1:+1)
                hdx += shift
                if hdx<=0 then hdx = length(history)
                elsif hdx>length(history) then hdx = 1 end if
                words = history[hdx]
            end if
            show = true
        end if
        if show then
            show = false
            s = prompt&join(words)
            puts(1,s)
            if length(words) then
                last = kinds[find(words[$],commands)]
                if last=ALIAS then last = 0 end if
            end if
        end if
    end while
    printf(1,"\n")
    if length(words) then
        k = find(words,history)
        if k then
            history[k..k] = {}
        end if
        history = append(history,words)
    end if
    return words
end function 

bool aliases_banned = false

function check_circular(string name, integer k, sequence seen={})
--
-- eg 1) alias "yy" to something benign, such as inventory.
--    2) alias "zz" to "yy" (quite possibly indirectly)
--    3) re-alias "yy" to "zz", oh dear, infinite loop...
--
-- solution: ban the running of aliases until such resolved.
--
    if kinds[k]!=ALIAS then return false end if
    sequence kk = data[k]
    for i=1 to length(kk) do
        string cmd = kk[i][1]
        k = find(cmd,commands)
        if not find(k,seen) then
            seen &= k
            if cmd=name or k=0 or check_circular(name,k,seen) then
                return true
            end if
        end if
    end for
    return false
end function

procedure check_conflicts()
--
-- warn if eg "dig down" is going to conflict with "drop"
-- (potentially preventing selection of "drop" via 'd')
-- unlike check_circular, this does not ban anything.
--
    string needs = "", ids = ""
    sequence needn = {}
    integer ch, k, ki
    for i=1 to length(kinds) do
        ch = commands[i][1]
        ki = kinds[i]
        if find(ki,{NEEDS_DIRECTION,NEEDS_ITEM}) then
            k = find(ch,needs)
            if k=0 then
                needs &= ch
                needn &= 1
            else
                needn[k] += 1
            end if
        elsif find(ki,{DIRECTION,ITEM}) then
            k = find(ch,ids)
            if k=0 then
                ids &= ch
            end if
        end if
    end for
    for i=1 to length(ids) do
        ch = ids[i]
        k = find(ch,needs)
        if k and needn[k]>1 then
            printf(1,"warning: command/direction/item conflict over '%c'\n",ch)
        end if
    end for
end procedure
check_conflicts()

procedure alias()
--
-- single word aliases are added to "cmds" as per the originals.
-- whole command [set]s are added to "cmds" as type ALIAS.
--
-- note this is open to all kinds of abuse I had no time to test for,
-- eg you can alias the alias command itself as rename, which is fine,
-- but you could also specify rename as attack north/alias/take gold, 
-- which makes no proper sense, and would re-prompt at every mid-run.
--
    string name = lower(prompt_string("Enter alias name(? for help):"))
    if name="?" then
        puts(1,alias_help)
        sequence sets = {{"DIRECTION",DIRECTION},
                         {"INVENTORY",INVENTORY},
                         {"MAKEALIAS",MAKEALIAS},
                         {"ATTACK",ATTACK},
                         {"TAKE",TAKE},
                         {"DROP",DROP},
                         {"EQUIP",EQUIP},
                         {"SLEDGE",SLEDGE},
                         {"LADDER",LADDER},
                         {"GOLD",GOLD},
                         {"ALL",ALL},
                         {"QUIT",QUIT}}
        for i=1 to length(sets) do
            {string kindstr, integer kind} = sets[i]
            sequence set = {}
            for k=1 to length(cmds) do
                if iff(kind=DIRECTION?kinds[k]:data[k])=kind then
                    set = append(set,commands[k])
                end if
            end for
            printf(1,"%s: %s\n",{kindstr,join(set,",")})
        end for
        for i=1 to length(kinds) do
            if kinds[i]=ALIAS then
                sequence di = data[i]
                for j=1 to length(di) do di[j] = join(di[j]," ") end for
                printf(1,"%s : %s\n",{commands[i],join(di,", ")})
            end if
        end for
        printf(1,"\n")
    elsif name!="" then
        integer k = find(name,commands)
        if k then
            -- note: no check is made as to whether a command is still 
            --       being referenced, just "not the last of its kind".
            if kinds[k]!=ALIAS then
                integer k2 = 0
                for i=1 to length(kinds) do
                    if i!=k 
                    and kinds[i]=kinds[k]
                    and data[i]=data[k] then
                        k2 = i
                        exit
                    end if
                end for
                if k2=0 then
                    -- user needs to copy(/alias), then delete...
                    printf(1,"last of kind, cannot delete\n")
                    return
                end if
            end if
            printf(1,"Delete existing command?(Y/N):")
            integer ch = upper(wait_key())
            printf(1,"%c\n",ch)
            if ch!='Y' then return end if
            cmds[k..k] = {}
            commands[k..k] = {}
            kinds[k..k] = {}
            data[k..k] = {}
            for i=length(history) to 1 by -1 do
                if find(name,history[i]) then
                    history[i..i] = {}
                end if
            end for
        end if
        sequence set = {}
        while true do
            object one = get_command("Alias:")
            if one={} then exit end if
            k = find(one[1],commands)
            integer kk = kinds[k]
            if length(one)=1
            and kk!=ALIAS then
                -- single word
                if find(kk,{NEEDS_DIRECTION,NEEDS_ITEM,ITEM}) then
                    if length(set) then
                        puts(1,"invalid - whole commands only, in a set\n")
                    else
                        cmds = append(cmds,{name,kk,data[k]})
                        commands = append(commands,name)
                        kinds = append(kinds,kk)
                        data = append(data,data[k])
                        check_conflicts()
                        exit
                    end if
                else
                    set = append(set,one)
                end if
            else
                -- whole command(s)
                set = append(set,one)
            end if
        end while
        if length(set) then
            sequence kind = {name,ALIAS,set}
            cmds = append(cmds,kind)
            commands = append(commands,name)
            kinds = append(kinds,ALIAS)
            data = append(data,set)
        end if
        aliases_banned = false
        for k=1 to length(cmds) do
            if check_circular(commands[k],k) then
                puts(1,"invalid - circular or undefined reference\n")
                aliases_banned = true
                exit
            end if
        end for
    end if
end procedure

constant rooms = new_dict()
sequence location = {0,0,0}

constant room_descriptions = {"cold dark room",
                              "very stinky room",
                              "small but comfortable room",
                              "room with an incredible echo! Yahollaydoo!",
                              "grim room resembling a window-less jail",
                              "green room",
                              "black room",
                              "blue room",
                              "brown sad room",
                              "room which breathes pain"}

bool game_over = false,
     equipped = false   -- (possibly a bit too simple)

integer others = 1  -- ensure a SLEDGE is available somewhere,
                    -- set to -1 when that's all done & dusted

procedure make_room(sequence location, integer hole=0)
    integer desc = rand(length(room_descriptions))
    sequence items = {},    -- (nb no quantities, as yet...)
             exits = repeat(false,S)
    if hole then exits[hole] = true end if
    for d=N to S do
        -- No up until we've placed a sledge, otherwise would
        --    need to ensure ladder was present on each floor.
        if others=-1 or d!=U then
            sequence next_door = sq_add(location,moves[d])
            integer node = getd_index(next_door,rooms)
            if node=NULL then
                if rand(5)=1 then
                    exits[d] = true
                    if others!=-1 then others += 1 end if
                end if
            else
                sequence {?,?,nexits} = getd_by_index(node,rooms)
                exits[d] = nexits[7-d]
            end if
        end if
    end for
    others -= 1
    for item=SLEDGE to GOLD do -- (LADDER in the middle)
        if rand(3)=1 
        or others=0             -- ensure sledge if last room
        or (exits[U] and item=LADDER) then -- no death traps
            items &= item
            if others=0 then others = -1 end if
        end if
    end for
    sequence room = {desc,items,exits}
    setd(location,room,rooms)
end procedure
if getd_index(location,rooms)=NULL then make_room(location) end if

sequence carryable = {SLEDGE,LADDER,GOLD},
         carrying = repeat(0,length(carryable))

procedure inventory()
    string totes = ""
    if sum(carrying)=0 and not game_over then
        totes = "nothing"
    else
        for i=1 to length(carrying) do
            integer ci = carryable[i], cq = carrying[i]
            if cq!=0 then
                if length(totes) then totes &= ", " end if
                string name = commands[find(ci,data)]
                if cq=1 then
                    totes &= iff(ci=GOLD?"one piece of ":"a ")&name
                else
                    totes &= sprintf("%d ",cq)&iff(ci=GOLD?"pieces of "&name:name&"s")
                end if
            end if
        end for
        if game_over then
            if length(totes) then totes &= ", and " end if
            totes &= "a printed A4 certificate of achievement as given to all winners"
        end if
    end if
    printf(1,"You are carrying %s.\n",{totes})
end procedure

procedure show_room()
    {integer desc, sequence items, sequence exits} = getd(location,rooms)
    printf(1, "You are in a %s %v\n",{room_descriptions[desc],location})
    if length(items) then
        for i=1 to length(items) do
            items[i] = commands[find(items[i],data)]
        end for
        if length(items)=1 and items!={"gold"} then
            items = "a "&items[1]
        else
            items = join(items,", ")
        end if
        printf(1,"You see %s.\n",{items})
    end if
    if find(true,exits)!=0 then
        for i=length(exits) to 1 by -1 do
            if exits[i] then
                exits[i] = commands[find(i,data)]
            else
                exits[i..i] = {}
            end if
        end for
        string isare = iff(length(exits)=1?"is an exit":"are exits")
        printf(1,"There %s %s.\n",{isare,join(exits,", ")})
    end if
    printf(1,"\n")
    if location={1,1,5} then
        game_over = true
        printf(1,"You win! Game over.\n\n")
        inventory()
    end if
end procedure

procedure move(integer direction)
    {integer desc, sequence items, sequence exits} = getd(location,rooms)
    if not exits[direction] then
        printf(1,"There is no passage that way.\n")
    elsif direction=U
      and not find(LADDER,items) then
        printf(1,"There is no ladder in the room.\n")
    else
        location = sq_add(location,moves[direction])
        if getd_index(location,rooms)=NULL then
            make_room(location,7-direction)
        end if
        {desc, items, exits} = getd(location,rooms)
        if exits[D] then
            sequence below = sq_add(location,moves[D])
            integer node = getd_index(below,rooms)
            if node!=NULL then
                {desc, items, exits} = getd_by_index(node,rooms)
                if find(LADDER,items)=0 then
                    printf(1,"You fall down a hole in the floor and die.\n")
                    game_over = true
                    return
                end if
            end if
        end if
        show_room()
    end if
end procedure

procedure dig(integer direction)
    {integer desc, sequence items, sequence exits} = getd(location,rooms)
    if not equipped then
        printf(1,"You claw at the rock with your bare hands until your fingers bleed.\n")
    elsif rnd()<0.1 then
        -- (if it is the last one then game over...)
        printf(1,"Your sledge has broken.\n")
        equipped = false
        carrying[find(SLEDGE,carryable)] -= 1
    elsif exits[direction] then
        printf(1,"There is already a passage that way, you make it a little bigger.\n")
    else
        sequence new_location = sq_add(location,moves[direction])
        if getd_index(new_location,rooms)=NULL then
            make_room(new_location)
        end if
        exits[direction] = true
        sequence die = {desc,items,exits} -- (save before overwriting)
        {desc,items,exits} = getd(new_location,rooms)
        integer hole = 7-direction
        if exits[hole] then ?9/0 end if
        if direction=D
        and not find(LADDER,items) then
            printf(1,"You dig a tiny hole no bigger than your fist.\n")
            printf(1,"Seeing no ladder, you decide it is too risky to continue.\n")
        else
            exits[hole] = true
            setd(location,die,rooms)
            setd(new_location,{desc,items,exits},rooms)
            string way = commands[find(direction,data)]
            printf(1,"You dig a passage %s.\n",{way})
        end if
    end if
end procedure

procedure take(integer item)
    {integer desc, sequence items, sequence exits} = getd(location,rooms)
    if item=ALL then
        for i=1 to length(items) do take(items[i]) end for
    else
        string name = commands[find(item,data)]
        integer k = find(item,items)
        if k=0 then
            printf(1,"There isn't any %s here.\n",{name})
        else
            items[k..k] = {}
            setd(location,{desc,items,exits},rooms)
            k = find(item,carryable)
            carrying[k] += 1
            printf(1,"You pick up the %s.\n",{name})
        end if
    end if
end procedure

procedure drop(integer item, integer qty=1)
-- note: dropping (all) 8 pieces of gold results in one on the floor...
    if item=ALL then
        if sum(carrying)=0 then
            printf(1,"You aren't carrying anything.\n")
        else
            for i=1 to length(carrying) do
                qty = carrying[i]
                drop(carryable[i],qty)
            end for
        end if
    elsif qty then
        string name = commands[find(item,data)]
        integer k = find(item,carryable)
        if carrying[k]=0 then
            printf(1,"You aren't carrying any %s.\n",{name})
        else
            carrying[k] -= qty
            {integer desc, sequence items, sequence exits} = getd(location,rooms)
            if not find(item,items) then
                items &= item
                setd(location,{desc,items,exits},rooms)
            end if
            printf(1,"You drop the %s.\n",{name})
            if item=SLEDGE then equipped = false end if
        end if
    end if
end procedure

procedure equip(integer item)
    string name = commands[find(item,data)]
    integer k = find(item,carryable)
    if carrying[k]=0 then
        printf(1,"You aren't carrying %s %s.\n",{iff(item=GOLD?"any":"a"),name})
    elsif item!=SLEDGE then
        printf(1,"You can't equip %s%s.\n",{iff(item=GOLD?"":"a "),name})
    elsif equipped then
        printf(1,"already equipped.\n")
    else
        equipped = true
        printf(1,"You equip the %s.\n",{name})
    end if
end procedure

procedure process_command(sequence c)
--
-- tip: replace the inner switches with tables of routine_ids,
--      that is should they start to get a little too unwieldy.
--
    integer k = find(c[1],commands)
    switch kinds[k] do
        case ALIAS: -- (the nested variety)
            if aliases_banned then
                printf(1,"Sorry, aliases banned until circular references resolved.\n")
            else
                for i=1 to length(data[k]) do
                    printf(1,"Running:%s\n",{join(data[k][i])})
                    process_command(data[k][i])
                    if game_over then exit end if
                end for
            end if
        case DIRECTION:
            move(data[k])
        case NEEDS_DIRECTION, NEEDS_ITEM:
            integer dk = data[find(c[2],commands)]
            switch data[k] do
                case ATTACK: dig(dk)
                case TAKE:   take(dk)
                case DROP:   drop(dk)
                case EQUIP:  equip(dk)
                default: ?9/0
            end switch
        case SINGLE_WORD_COMMAND:
            switch data[k] do
                case INVENTORY: inventory()
                case MAKEALIAS: alias()
                case QUIT: game_over = true
                default: ?9/0
            end switch
        default: ?9/0
    end switch
end procedure

puts(1,start_text)
show_room()
while not game_over do
    sequence c = get_command("Command:")
    if c={} then exit end if
    process_command(c)
end while
```

