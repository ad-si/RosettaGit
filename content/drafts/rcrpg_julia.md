+++
title = "RCRPG/Julia"
description = ""
date = 2019-02-21T05:32:14Z
aliases = []
[extra]
id = 22178
[taxonomies]
categories = []
tags = []
+++

This [[Julia]] version of [[RCRPG]] contains two versions of the game code: one version with a text interface and a second one with a GUI interface.

==Language Idioms==

This program illustrates some of the interesting aspects of Julia:
* multiple function dispatch used to select function behavior by data argument type
* both dynamic and optionally strongly typed data types
* built-in data structures including named tuples, vectors, matrices, and Dicts (hashes)
* functional programming with map and other functions using anonymous functions on arrays

<br />
In addition, the GUI version code illustrates the following additional aspects of Julia:
* The use of Channels to create a communication between game code and display code in a way that could allow the game to run as a client-server version with little change
* The use of @async to allow co-routine based multitasking with Channels to communicate between co-routines

== Objective ==
The objective of the game is to find your way to the treasure room, which is located in the upper left of level 5 
(against the dungeon level 5 corner at (1, 1, 5)), and then to exit the game by ascending above level 1.

==Commands==

Direction commands:
```txt
north, south, east, west, up, down
```
Type the first letter of these commands 
in order to go in a certain direction (n, s, e, w, u, d). 

To exit a room, you must use the sledge to create a hole through the room wall, ceiling, or floor. 
Make a hole by e(q)uipping a sledge (equip with q), and (a)ttacking the obstacle (attack with a). 

To go down, there must be a pit in the floor where you are standing. Create a pit by (a)ttacking (d)own.

In order to go up, there must be a skylight in the ceiling where you are standing and you must have a ladder equipped.
Create a skylight with the sledge (attack up). 

A sledge can be used indefinitely without losing it, but you lose your equipped ladder when you go up using that ladder.

Attack (dig with the sledgehammer) with the letter a. You must have a sledge equipped (equip with letter q) to do this.

Take gold and other items from room floor by standing over the item and then (t)aking with letter t. 

Remove items with letter r for (r)emove. Removing equipped items will move to inventory, 
removing inventory drops items on the dungeon floor.

Rooms for each level remain only while you are on that level. Levels generate new rooms each time you re-enter them.

View inventory: Letter i.

Current room in(f)ormation: Letter f.

Aliasing: Letter l. Restrictions are that the keys are case insensitive and cannot be extended keys.

Help: Letter h.

==Code==

```julia
using Crayons

struct Point
    x::Int
    y::Int
    z::Int
end

const obstacles = (wall = '\u2592', permwall = '\u2593', water = '~')

const defaults = (baseluck = 0.5, levelrooms = 8, itemrarity = 0.2, maxroomdim = 20,
    wallheight = 3, leveldim = 100, prizeroomlevel = 5, fastdig = true, map = 1,
    showlevel = true)

abstract type Item end

struct Room
    origin::Point
    xsize::Int
    ysize::Int
end

struct Level
    map::Int
    grid::Matrix{Char}
    zbase::Int
    floorlevel::Int
    height::Int
    rooms::Vector{Room}
    content::Dict{Point, Vector{Item}}
end

struct Location
    p::Point
    room::Union{Room, Nothing}
    level::Level
end

mutable struct Agent <: Item
    location::Location
    inventory::Vector{Item}
    wearing::Vector{Item}
    wielding::Vector{Item}
    Agent(loc) = new(loc, Item[Gold(0)], Item[], Item[])
end

mutable struct Gold <: Item
    value::Int
    disp::Char
    Gold(v::Int) = new(v, 'g')
end

struct Sledge <: Item
    weight::Int
    disp::Char
    Sledge() = new(10, 's')
end

struct Ladder <: Item
    weight::Int
    disp::Char
    Ladder() = new(7, 'l')
end

struct Pit <: Item
    disp::Char
    Pit() = new('_')
end

struct Skylight <: Item
    disp::Char
    Skylight() = new('^')
end

const charcrayons = Dict{Char,Crayon}('p' => crayon"white", ' ' => crayon"black", 'g' => crayon"yellow",
                                      's' => crayon"blue", 'l' => crayon"green", '_' => crayon"red", 
                                      '^' => crayon"light_cyan", '~' => crayon"blue",
                                      '\u2592' => crayon"light_gray", '\u2593' => crayon"dark_gray")

onpoint(p, level) = haskey(level.content, p) ? level.content[p] : Item[]
hastype(t::Type, arr) = begin for i in arr if typeof(i) == t return true end end; false end
hastype(t, p, level) = hastype(t, onpoint(p, level))
x1y1x2y2(room) = [room.origin.x, room.origin.y, room.origin.x + room.xsize, room.origin.y + room.ysize]
inroom(x, y, rm) = begin x1, y1, x2, y2 = x1y1x2y2(rm); x1 <= x <= x2 && y1 <= y <= y2 end
inroom(p::Point, rm) = inroom(p.x, p.y, rm)
isinventory(t::Type, player) = hastype(t, player.inventory)
iswielding(t::Type, player) = hastype(t, player.wielding)
iswearing(t::Type, player) = hastype(t, player.wearing)
isequip(i::Ladder) = true
isequip(i::Sledge) = true
isequip(i::Item) = false

function overlaps(testroom::Room, baseroom::Room, level::Level)
    x1, y1, x2, y2 = x1y1x2y2(testroom)
    xx1, yy1, xx2, yy2 = x1y1x2y2(baseroom)
    x2 < xx1 || xx2 < x1 || y2 < yy1 || yy2 < y1 ? false : true
end

overlaps(notyet::Room, level::Level) = (for r in level.rooms if overlaps(notyet, r, level) return true end end; false)
randpoint(level) = begin xmax, ymax = size(level.grid); Point(rand(1:xmax), rand(1:ymax), level.zbase) end
addatpoint(i, p, lev) = (if !haskey(lev.content, p) lev.content[p] = Vector{Item}() end; push!(lev.content[p], i))

function randorigin(level, xd=defaults.maxroomdim, yd=defaults.maxroomdim)
    xmax, ymax = size(level.grid)
    Point(rand(1:xmax-xd), rand(1:ymax-yd), level.zbase)
end

function notyetaroom(level::Level, origin::Point, minx=6, maxx=50, miny=6, maxy=25)
    levsize = size(level.grid)
    if origin.x < 3 || origin.y < 3
        origin = Point(3, 3, origin.z)
    end
    xmax, ymax = min(maxx, levsize[1] - origin.x), min(maxy, levsize[2] - origin.y)
    xsize = rand(minx:xmax)
    ysize = rand(miny:ymax)
    Room(origin, xsize, ysize)
end

function emptypoints(room, level)
    vpt = Vector{Point}()
    x1, y1, x2, y2 = x1y1x2y2(room)
    for x in x1+1:x2-1, y in y1+1:y2-1
        if level.grid[x, y] == ' '
            push!(vpt, Point(x, y, level.zbase))
        end
    end
    vpt
end
randemptypoint(room, level) = (pts = emptypoints(room, level); isempty(pts) ? room.origin .+ 2 : rand(pts))

function addtoroom(item, room, level)
    p = randemptypoint(room, level)
    addatpoint(item, p, level)
    level.grid[p.x, p.y] = item.disp
end

function fillroom(room, level, uppossible=true, downpossible=true)
    grid = level.grid
    x1, y1, x2, y2 = x1y1x2y2(room)
    grid[x1:x2, y1:y2] .= ' '
    grid[x1:x2, y1] .= obstacles.wall
    grid[x1:x2, y2] .= obstacles.wall
    grid[x1, y1:y2] .= obstacles.wall
    grid[x2, y1:y2] .= obstacles.wall
    for _ in 1:Int(floor(defaults.baseluck * 8))
        if rand() < defaults.baseluck
            val = Int(floor(100.0 * rand() * defaults.baseluck))
            addtoroom(Gold(val), room, level)
        end
    end
    if rand() * defaults.baseluck * 10 > 1.0 && uppossible
        addtoroom(Ladder(), room, level)
    end
    if rand() * defaults.baseluck * 15 > 1.0 && downpossible
        addtoroom(Pit(), room, level)
    end
    if rand() * defaults.baseluck * 5 > 1.0
        addtoroom(Sledge(), room, level)
    end
    room
end

function makeprizeroom(level)
    prizeroom = Room(Point(2, 2, level.zbase), defaults.maxroomdim, defaults.maxroomdim)
    x1, y1, x2, y2 = x1y1x2y2(prizeroom)
    level.grid[x1:x2, y1:y2] .= ' '
    level.grid[x1:x2, y1] .= obstacles.permwall
    level.grid[x1:x2, y2] .= obstacles.wall
    level.grid[x1, y1:y2] .= obstacles.permwall
    level.grid[x2, y1:y2] .= obstacles.wall
    for _ in 1:Int(floor(defaults.baseluck * 40))
        val = Int(floor(5000.0 * rand() * defaults.baseluck))
        addtoroom(Gold(val), prizeroom, level)
    end
    prizeroom
end

function makelevel(floorlevel, gridsize=defaults.leveldim, height=defaults.wallheight, map=defaults.map)
    grid = fill(obstacles.wall, gridsize, gridsize)
    grid[:, 1] .= obstacles.permwall
    grid[:, end] .= obstacles.permwall
    grid[1, :] .= obstacles.permwall
    grid[end, :] .= obstacles.permwall
    rooms = Vector{Room}()
    level = Level(map, grid, floorlevel * height, floorlevel, height, rooms, Dict())
    if floorlevel == defaults.prizeroomlevel
        push!(rooms, makeprizeroom(level))
    end
    trycount = 0
    while trycount < defaults.levelrooms * 5 && length(rooms) < defaults.levelrooms
        rm = notyetaroom(level, randorigin(level))
        if !overlaps(rm, level)
            fillroom(rm, level)
            push!(rooms, rm)
        end
        trycount += 1
    end
    level
end

function displaytunnel(player)
    p = player.location.p
    visgrid = view(player.location.level.grid, p.x-1:p.x+1, p.y-1:p.y+1)
    for y in 1:3
        for x in 1:3
            ch = (x == 2 == y) ? 'p' : visgrid[x, y]
            print(charcrayons[ch], ch)
        end
        println()
    end
    println()
end

function updateroom(player)
    r = nothing
    for room in player.location.level.rooms
        if inroom(player.location.p, room)
            r = room
            break
        end
    end
    if r != player.location.room
        player.location = Location(player.location.p, r, player.location.level)
    end
end

function displayroom(player, lit=true)
    println()
    if !lit
        return
    end
    x1, y1, x2, y2 = x1y1x2y2(player.location.room)
    playerx, playery = player.location.p.x, player.location.p.y
    for y in y1:y2
        for x in x1:x2
            ch = (x == playerx && y == playery) ? 'p' : player.location.level.grid[x, y]
            print(charcrayons[ch], ch)
        end
        println()
    end
    println()
end

function displaylevel(player, showunvisited=true)
    mat = player.location.level.grid
    p = player.location.p
    for y in 1:size(mat)[2]
        println()
        for x in 1:size(mat)[1]
            print((x == p.x && y == p.y) ? 'p' : mat[x, y])
        end
    end
    println
end

function queryprompt(query, choices, choicetxt="")
    carr = map(x -> uppercase(strip(string(x))), collect(choices))
    while true
        print(query, " ", choicetxt == "" ? carr : choicetxt, ": ")
        choice = uppercase(strip(readline(stdin)))
        if choice in carr
            return choice
        end
        println()
    end
end

help(player=nothing) = 
    println("n north, s south, w west, e east, d down, u up, i inventory, o roominfo, t take, r drop, q equip, a attack/dig")

function playerdisplayinventory(player)
    println("Inventory: $(player.inventory)\n")
    println("Wielding: $(player.wielding)\n")    
end

function playerroominfo(player)
    if player.location.room == nothing
        println("You are between rooms on level $(player.location.level).")
        return
    end
    if player.location.level.floorlevel == defaults.prizeroomlevel
        println("The prize room is on this level!")
    end
    println("You are on level $(player.location.level.floorlevel). You look around the current room.")
    hasgold, hasladder, haspit, hassledge, hasdiggable = false, false, false, false, false
    saytype(t::Gold) = if !hasgold println("There is gold here."); hasgold = true end
    saytype(t::Ladder) = println("There is a ladder here.")
    saytype(t::Pit) = println("There is a pit here.")
    saytype(t::Sledge) = println("There is a sledgehammer here.")
    for (p, v) in player.location.level.content
        if inroom(p, player.location.room)
            for i in v
                saytype(i)
            end
        end
    end
end

function playerequip(player)
    for (i, item) in enumerate(player.inventory)
        if isequip(item)
            yn = queryprompt("Equip $item? (y)es, (n)o, (q)uit choice", ["Y", "N", "Q"])
            if yn == "Y"
                if iswielding(typeof(item), player)
                    println("You are already wielding a $(typeof(item)).")
                else
                    push!(player.wielding, item)
                    deleteat!(player.inventory, i)
                end
            elseif yn == "Q"
                break
            end
        end
    end
end

function playerremove(player)
    for (i, item) in enumerate(player.wielding)
        if iswielding(typeof(item), player)
            yn = queryprompt("Unequip $item? (y)es, (n)o, (q)uit choice", ["Y", "N", "Q"])
            if yn == "Y"
                push!(player.inventory, item)
                deleteat!(player.wielding, i)
            elseif yn == "Q"
                break
            end
        end
    end
    for (i, item) in enumerate(player.inventory)
        if isinventory(item)
            yn = queryprompt("Drop $item? (y)es, (n)o, (q)uit choice", ["Y", "N", "Q"])
            if yn == "Y"
                if !haskey(player.location.level.content, player.location.p)
                    player.location.level.content[player.location.p] = Vector{Item}()
                end
                push!(player.location.level.content[player.location.p], item)
                deleteat!(player.inventory, i)
            elseif yn == "Q"
                break
            end
        end
    end
end

function playertake(player)
    function addgold(gitem)
        if findfirst(x -> typeof(x) == Gold, player.inventory) == nothing
            push!(player.inventory, Gold(0))
        end
        player.inventory[findall(x -> typeof(x) == Gold,
            player.inventory)[1]].value += gitem.value
    end
    canpickup(i::Gold) = true
    canpickup(i::Sledge) = true
    canpickup(i::Ladder) = true
    canpickup(i::Item) = (println("Cannot pick that up."); false)
    takeit(g::Gold) = begin addgold(g); println("You add $(g.value) in gold.") end
    takeit(s::Sledge) = begin push!(player.inventory, s); println("You have a sledge.") end
    takeit(s::Ladder) = begin push!(player.inventory, s); println("You have a ladder.") end

    d = player.location.level.content
    if player.location.room != nothing && haskey(d, player.location.p)
        for _ in 1:length(d[player.location.p])
            item = pop!(d[player.location.p])
            if canpickup(item)
                takeit(item)
            else
                pushfirst!(d[player.location.p], item)
            end
        end
        if isempty(d[player.location.p])
            delete!(d, player.location.p)
            player.location.level.grid[player.location.p.x, player.location.p.y] = ' '
        else
            player.location.level.grid[player.location.p.x, player.location.p.y] =
                d[player.location.p][1].disp
        end
    end
end

function exitmap(player)
    println("You exit the game with inventory $(player.inventory), wielding $(player.wielding).")
    exit()
end

function newlevel(player, levelsdown, startup=false)
    oldlevel = player.location.level.floorlevel
    newlevel = oldlevel + levelsdown
    if newlevel < 1
        exitmap(player)
    else
        level = makelevel(newlevel)
        room = rand(level.rooms)
        point = randemptypoint(room, level)
        player.location = Location(point, room, level)
        playerroominfo(player)
    end
    if defaults.showlevel
        displaylevel(player)
    end
    player
end

function playerup(player)
    if haskey(player.location.level.content, player.location.p) &&
        hastype(Skylight, player.location.p, player.location.level) &&
        iswielding(Ladder, player)
        deleteat!(player.wielding, findfirst(i -> typeof(i) == Ladder, player.wielding))
        println("You have to leave your ladder behind.")
        newlevel(player, -1)
    else
        println("You need to wield a ladder while at a skylight to go upwards.")
    end
end

function playerdown(player)
    if haskey(player.location.level.content, player.location.p) &&
        hastype(Pit, player.location.p, player.location.level)
        newlevel(player, 1)
    else
        println("You can only move downwards at a Pit in the floor.")
    end
end

function playerdig(player)
    movedirs = Dict("N" => [0, -1], "W" => [-1, 0], "S" => [0, 1], "E" => [1, 0])
    if !iswielding(Sledge, player)
        println("You need to be wielding a Sledge.")
        return
    end
    direc = queryprompt("Direction?", ["N", "S", "E", "W", "U", "D"])
    if direc == "U"
        if rand() < defaults.baseluck
            addatpoint(Skylight(), player.location.p, player.location.level)
            player.location.level.grid[player.location.p.x, player.location.p.y] = '^'
            println("There is now a hole in the ceiling above you.")
        end
    elseif direc == "D"
        if rand() < defaults.baseluck
            addatpoint(Pit(), player.location.p, player.location.level)
            player.location.level.grid[player.location.p.x, player.location.p.y] = '_'
            println("There is now a pit in the floor at your feet.")
        end
    else
        goalp = Point(player.location.p.x + movedirs[direc][1], player.location.p.y +
            movedirs[direc][2], player.location.p.z)
        if player.location.level.grid[goalp.x, goalp.y] == obstacles.wall
            if rand() < defaults.baseluck
                if defaults.fastdig
                    depth = 0
                    for i in 1:div(defaults.leveldim, 10)
                        p = Point(player.location.p.x + movedirs[direc][1] * i,
                            player.location.p.y + movedirs[direc][2] * i, player.location.p.z)
                        if player.location.level.grid[p.x, p.y] == obstacles.wall
                           player.location.level.grid[p.x, p.y] = ' '
                           depth += 1
                        end
                    end
                    if depth > 3
                        println("Boom! Your sledgehammer's attack on this wall is amazing.")
                    end
                else
                    player.location.level.grid[goalp.x, goalp.y] = ' '
                end
            end
        elseif player.location.level.grid[goalp.x, goalp.y] == obstacles.permwall
            println("This kind of wall cannot be removed.")
        end
    end
end

function playermove(player, dx, dy)
    goalp = Point(player.location.p.x + dx, player.location.p.y + dy, player.location.p.z)
    if player.location.level.grid[goalp.x, goalp.y] in obstacles
        println("Something is in the way.")
    else
        player.location = Location(goalp, player.location.room, player.location.level)
    end
end
playerN(player) = playermove(player, 0, -1)
playerW(player) = playermove(player, -1, 0)
playerS(player) = playermove(player, 0, 1)
playerE(player) = playermove(player, 1, 0)

const usercommands = Dict("N" => playerN, "S" => playerS, "E" => playerE, "W" => playerW,
                          "U" => playerup, "D" => playerdown, "I" => playerdisplayinventory,
                          "O" => playerroominfo, "T" => playertake, "R" => playerremove,
                          "Q" => playerequip, "A" => playerdig, "?" => help)

function runuser(player)
    updateroom(player)
    if player.location.room == nothing
        displaytunnel(player)
    else
        displayroom(player)
    end
    choice = queryprompt("Choose move (nsewudiotrqa?)",
        ["N", "S", "E", "W", "U", "D", "I", "O", "T", "R", "Q", "A", "?"])
    usercommands[choice](player)
end

function newplayer(newlevel=1)
    level = makelevel(newlevel)
    room = rand(level.rooms)
    point = randemptypoint(room, level)
    player = Agent(Location(point, room, level))
    playerroominfo(player)
    if defaults.showlevel
        displaylevel(player)
    end
    player
end

function rungame()
    player = newplayer()
    while true
        runuser(player)
    end
end

rungame()

```


==GUI Code==

```julia
using Gtk.ShortNames, Colors, Cairo, Graphics

#
### =========== GUI CODE ================
#

const fontpointsize = 10
const mapwidth = 1000
const mapheight = 500
const logheight = 100
const leveldim = div(mapwidth, fontpointsize)
const windowmaxx = div(mapheight, fontpointsize)
const windowmaxy = leveldim
const basebuffer = fill('=', windowmaxx, windowmaxy)

win = Window("Dungeon Game", mapwidth, mapheight + logheight) |> (Frame() |> (vbox = Box(:v)))
set_gtk_property!(vbox, :expand, true)
can = Canvas(div(mapwidth, 3), mapheight)
push!(vbox, can)
textdisplay = ScrolledWindow()
logtxt = TextBuffer()
logtxt.text[String] = "===  ** DUNGEON GAME ** ===\n"
tview = TextView(logtxt)
push!(textdisplay, tview)
push!(vbox, textdisplay)

mainchoices = ["N", "S", "E", "W", "Up", "Down", "Inven", "inFo", "Take", "Remove", "eQuip", "Attack", "aLiases", "Help"]
aliases = Dict{String, String}()
directionchoices = ["N", "S", "E", "W", "U", "D"]
yesnochoices = ["Yes", "No"]
yesnoquitchoices = ["Yes", "No", "Quit"]
aliases = Dict{String, String}()

struct DisplayUpdate
    x::Int # location in basebuffer of data
    y::Int # location in basebuffer of data
    playerx::Int
    playery::Int
    data::Matrix{Char}
end

inputchan = Channel{String}(1000)
kbinput(w, event) = (push!(inputchan, string(Char(event.keyval))); 1)
inputhid = signal_connect(kbinput, win, "key-press-event")

dchan = Channel{DisplayUpdate}(100)
lchan = Channel{String}(20)
mchan = Channel{String}(20)
const playerchar = ['p']
const itemcolors = Dict{Char, Colorant}('p' => colorant"white", ' ' => colorant"black", 'g' => colorant"gold",
                                      's' => colorant"skyblue", 'l' => colorant"green", '_' => colorant"red",
                                      '^' => colorant"blue", '~' => colorant"navy",
                                      '\u2592' => colorant"ivory", '\u2593' => colorant"silver")

@guarded draw(can) do widget
    ctx = getgc(can)
    select_font_face(ctx, "Courier", Cairo.FONT_SLANT_NORMAL, Cairo.FONT_WEIGHT_BOLD)
    set_font_size(ctx, fontpointsize)
    workcolor = colorant"black"
    set_source_rgb(ctx, 0.2, 0.2, 0.2)
    rectangle(ctx, 0, 0, mapwidth, mapheight)
    fill(ctx)
    color = colorant"white"
    set_source(ctx, color)
    linelen = size(basebuffer)[2]
    workbuf = Char[]
    for i in 1:size(basebuffer)[1]
        move_to(ctx, 0, i * fontpointsize)
        lastcharprinted = '\x01'
        for j in 1:linelen
            ch = basebuffer[i, j]
            if j == 1
                lastcharprinted = ch
            elseif ch != lastcharprinted
                show_text(ctx, String(workbuf))
                empty!(workbuf)
            end
            if haskey(itemcolors, ch) && itemcolors[ch] != color
                color = itemcolors[ch]
                set_source(ctx, color)
            end
            push!(workbuf, ch)
            if j == linelen
                show_text(ctx, String(workbuf))
                empty!(workbuf)
            end
        end
    end
end

function makemenu(strings, chan)
    menu = Box(:h)
    for s in strings
        c = String("" * s[findfirst(isuppercase, s)])
        but = Button(s)
        onclick(b) = push!(chan, c)
        signal_connect(onclick, but, "clicked")
        push!(menu, but)
    end
    menu
end

swapforlastinbox(menu, box) = begin oldmenu = box[3]; delete!(box, box[3]); push!(box, menu); Gtk.showall(win); oldmenu end

function logwindow(s1="", s2="", s3="", s4="")
    set_gtk_property!(logtxt, :text, String(string(s1) * string(s2) * string(s3) * string(s4) *
        get_gtk_property(logtxt, :text, String)))
    Gtk.showall(win)
end

logln(s1="", s2="", s3="", s4="") = logwindow(s1, s2, string(s3) * string(s4), "\n")

mainmenu = makemenu(mainchoices, inputchan)
directionmenu = makemenu(directionchoices, inputchan)
yesnomenu = makemenu(yesnochoices, inputchan)
yesnoquitmenu = makemenu(yesnoquitchoices, inputchan)
push!(vbox, mainmenu)
Gtk.showall(win)

#
### =========== INPUT INTERFACE BETWEEN GUI AND GAME PLAY VIA CHANNELS ====================
#

function aliasing(player)
    aliaschan = Channel{String}(4)
    aliaskeyhit(w, event) = push!(aliaschan, string(Char(event.keyval)))
    signal_handler_block(win, inputhid)
    hid = signal_connect(aliaskeyhit, win, "key-press-event")
    logln("Hit the CURRENTLY DEFAULT key for function:")
    oldch = take!(aliaschan)
    logln("Hit the TO BE AN ALIAS key for the last key:")
    newch = take!(aliaschan)
    aliases[newch] = oldch
    signal_handler_disconnect(win, hid)
    signal_handler_unblock(win, inputhid)
end

function queryprompt(menu, options, txt="")
    if txt != ""
        push!(lchan, txt)
    end
    oldmenu = swapforlastinbox(menu, vbox)
    while true
        choice = uppercase(take!(inputchan))
        if haskey(aliases, choice)
            choice = uppercase(aliases[choice])
        end
        if choice in options
            swapforlastinbox(oldmenu, vbox)
            return choice
        end
    end
end

function updatedisplay()
    while isopen(dchan)
        upd = take!(dchan)
        dx, dy = size(upd.data)
        for x in 1:dx
            for y in 1:dy
                basebuffer[upd.x + x - 1, upd.y + y - 1] =
                    (y == upd.playery && x == upd.playerx) ? playerchar[1] : upd.data[x, y]
            end
        end
        draw(can)
    end
end

function updatemenu()
    while isopen(mchan)
        m = take!(mchan)
        if haskey(menus, m)
            swapforlastinbox(menu, vbox)
        end
    end
end

function updatelog()
    while isopen(lchan)
        line = take!(lchan)
        logln(line)
    end
end

function startchannelconsumers()
    @async(updatedisplay())
    @async(updatemenu())
    @async(updatelog())
end

#
### ================== GAME PLAY CODE ==================================
#

struct Point
    x::Int
    y::Int
    z::Int
end

const obstacles = (wall = '\u2592', permwall = '\u2593', water = '~')

const defaults = (baseluck = 0.5, levelrooms = div(leveldim, 20), maxroomdim = div(leveldim, 5),
    wallheight = 3, prizeroomlevel = 5, fastdig = true, map = 1, showlevel = true)

abstract type Item end

struct Room
    origin::Point
    xsize::Int
    ysize::Int
end

struct Level
    map::Int
    grid::Matrix{Char}
    zbase::Int
    floorlevel::Int
    height::Int
    rooms::Vector{Room}
    content::Dict{Point, Vector{Item}}
end

struct Location
    p::Point
    room::Union{Room, Nothing}
    level::Level
end

mutable struct Agent <: Item
    location::Location
    inventory::Vector{Item}
    wearing::Vector{Item}
    wielding::Vector{Item}
    Agent(loc) = new(loc, Item[Gold(0)], Item[], Item[])
end

mutable struct Gold <: Item
    value::Int
    disp::Char
    Gold(v::Int) = new(v, 'g')
end

struct Sledge <: Item
    weight::Int
    disp::Char
    Sledge() = new(10, 's')
end

struct Ladder <: Item
    weight::Int
    disp::Char
    Ladder() = new(7, 'l')
end

struct Pit <: Item
    disp::Char
    Pit() = new('_')
end

struct Skylight <: Item
    disp::Char
    Skylight() = new('^')
end

displayroom(player, lit=true) = if lit push!(dchan, DisplayUpdate(1, 1, player.location.p.x,
                                player.location.p.y, player.location.level.grid)) end

onpoint(p, level) = haskey(level.content, p) ? level.content[p] : Item[]
hastype(t::Type, arr) = begin for i in arr if typeof(i) == t return true end end; false end
hastype(t, p, level) = hastype(t, onpoint(p, level))
x1y1x2y2(room) = [room.origin.x, room.origin.y, room.origin.x + room.xsize, room.origin.y + room.ysize]
inroom(x, y, rm) = begin x1, y1, x2, y2 = x1y1x2y2(rm); x1 <= x <= x2 && y1 <= y <= y2 end
inroom(p::Point, rm) = inroom(p.x, p.y, rm)
isinventory(t::Type, player) = hastype(t, player.inventory)
iswielding(t::Type, player) = hastype(t, player.wielding)
iswearing(t::Type, player) = hastype(t, player.wearing)
isequip(i::Ladder) = true
isequip(i::Sledge) = true
isequip(i::Item) = false

function overlaps(testroom::Room, baseroom::Room, level::Level)
    x1, y1, x2, y2 = x1y1x2y2(testroom)
    xx1, yy1, xx2, yy2 = x1y1x2y2(baseroom)
    x2 < xx1 || xx2 < x1 || y2 < yy1 || yy2 < y1 ? false : true
end

overlaps(notyet::Room, level::Level) = (for r in level.rooms if overlaps(notyet, r, level) return true end end; false)
randpoint(level) = begin xmax, ymax = size(level.grid); Point(rand(1:xmax), rand(1:ymax), level.zbase) end
addatpoint(i, p, lev) = (if !haskey(lev.content, p) lev.content[p] = Vector{Item}() end; push!(lev.content[p], i))

function randorigin(level, xd=defaults.maxroomdim, yd=defaults.maxroomdim)
    xmax, ymax = size(level.grid)
    Point(rand(1:xmax-xd), rand(1:ymax-yd), level.zbase)
end

function notyetaroom(level::Level, origin::Point, minx=6, maxx=50, miny=6, maxy=25)
    levsize = size(level.grid)
    if origin.x < 3 || origin.y < 3
        origin = Point(3, 3, origin.z)
    end
    xmax, ymax = min(maxx, levsize[1] - origin.x), min(maxy, levsize[2] - origin.y)
    xsize = rand(minx:xmax)
    ysize = rand(miny:ymax)
    Room(origin, xsize, ysize)
end

function emptypoints(room, level)
    vpt = Vector{Point}()
    x1, y1, x2, y2 = x1y1x2y2(room)
    for x in x1+1:x2-1, y in y1+1:y2-1
        if level.grid[x, y] == ' '
            push!(vpt, Point(x, y, level.zbase))
        end
    end
    vpt
end
randemptypoint(room, level) = (pts = emptypoints(room, level); isempty(pts) ? room.origin .+ 2 : rand(pts))

function addtoroom(item, room, level)
    p = randemptypoint(room, level)
    addatpoint(item, p, level)
    level.grid[p.x, p.y] = item.disp
end

function fillroom(room, level, uppossible=true, downpossible=true)
    grid = level.grid
    x1, y1, x2, y2 = x1y1x2y2(room)
    grid[x1:x2, y1:y2] .= ' '
    grid[x1:x2, y1] .= obstacles.wall
    grid[x1:x2, y2] .= obstacles.wall
    grid[x1, y1:y2] .= obstacles.wall
    grid[x2, y1:y2] .= obstacles.wall
    for _ in 1:Int(floor(defaults.baseluck * 8))
        if rand() < defaults.baseluck
            val = Int(floor(100.0 * rand() * defaults.baseluck))
            addtoroom(Gold(val), room, level)
        end
    end
    if rand() * defaults.baseluck * 10 > 1.0 && uppossible
        addtoroom(Ladder(), room, level)
    end
    if rand() * defaults.baseluck * 15 > 1.0 && downpossible
        addtoroom(Pit(), room, level)
    end
    if rand() * defaults.baseluck * 5 > 1.0
        addtoroom(Sledge(), room, level)
    end
    room
end

function permwalls(level)
    level.grid[1:end, 1] .= obstacles.permwall
    level.grid[1:end, end] .= obstacles.permwall
    level.grid[1, 1:end] .= obstacles.permwall
    level.grid[end, 1:end] .= obstacles.permwall
end

function makeprizeroom(level)
    prizeroom = Room(Point(2, 2, level.zbase), defaults.maxroomdim, defaults.maxroomdim)
    x1, y1, x2, y2 = x1y1x2y2(prizeroom)
    level.grid[x1:x2, y1:y2] .= ' '
    level.grid[x1:x2, y1] .= obstacles.permwall
    level.grid[x1:x2, y2] .= obstacles.wall
    level.grid[x1, y1:y2] .= obstacles.permwall
    level.grid[x2, y1:y2] .= obstacles.wall
    for _ in 1:Int(floor(defaults.baseluck * 40))
        val = Int(floor(5000.0 * rand() * defaults.baseluck))
        addtoroom(Gold(val), prizeroom, level)
    end
    prizeroom
end

function makelevel(floorlevel, height=defaults.wallheight, map=defaults.map)
    grid = fill(obstacles.wall, windowmaxx, windowmaxy)
    rooms = Vector{Room}()
    level = Level(map, grid, floorlevel * height, floorlevel, height, rooms, Dict())
    if floorlevel == defaults.prizeroomlevel
        push!(rooms, makeprizeroom(level))
    end
    trycount = 0
    while trycount < defaults.levelrooms * 5 && length(rooms) < defaults.levelrooms
        rm = notyetaroom(level, randorigin(level))
        if !overlaps(rm, level)
            fillroom(rm, level)
            push!(rooms, rm)
        end
        trycount += 1
    end
    permwalls(level)
    level
end

function updateroom(player)
    r = nothing
    for room in player.location.level.rooms
        if inroom(player.location.p, room)
            r = room
            break
        end
    end
    if r != player.location.room
        player.location = Location(player.location.p, r, player.location.level)
    end
end

help(player=nothing) = push!(lchan,
    "n north, s south, w west, e east, d down, u up, i inventory, f roominfo, t take, r drop, q equip, a attack/dig, l aliases")

playerdisplayinventory(player) = push!(lchan, "Inventory: $(player.inventory)\nWielding: $(player.wielding)\n")

function playerroominfo(player)
    txt = ""
    if player.location.room == nothing
        txt *= "You are between rooms on level $(player.location.level)."
    else
        if player.location.level.floorlevel == defaults.prizeroomlevel
            txt *= "The prize room is on this level!\n"
        end
        txt *= "You are on level $(player.location.level.floorlevel). You look around the current room.\n"
        hasgold, hasladder, haspit, hassledge, hasdiggable = false, false, false, false, false
        saytype(t::Gold) = if !hasgold txt *= "There is gold here. "; hasgold = true end
        saytype(t::Ladder) = begin txt *= "There is a ladder here. " end
        saytype(t::Pit) = begin txt *= "There is a pit here. " end
        saytype(t::Sledge) = begin txt *= "There is a sledgehammer here. " end
        for (p, v) in player.location.level.content
            if inroom(p, player.location.room)
                for i in v
                    saytype(i)
                end
            end
        end
        txt *= "\n"
    end
    push!(lchan, txt)
end

function playerequip(player)
    for (i, item) in enumerate(player.inventory)
        if isequip(item)
            yn = queryprompt(yesnoquitmenu, ["Y", "N", "Q"], "Wield $item? (Q to quit menu)")
            if yn == "Y"
                if iswielding(typeof(item), player)
                    push!(lchan, "You are already wielding a $(typeof(item)).")
                else
                    push!(player.wielding, item)
                    deleteat!(player.inventory, i)
                end
            elseif yn == "Q"
                break
            end
        end
    end
end

function playerremove(player)
    for (i, item) in enumerate(player.wielding)
        mainmenu = swapforlastinbox(yesnoquitmenu, vbox)
        yn = queryprompt(yesnoquitmenu, ["Y", "N", "Q"], "Remove $item? (Q to quit menu)")
        if yn == "Y"
            push!(player.inventory, item)
            deleteat!(player.wielding, i)
        elseif yn == "Q"
            break
        end
    end
    for (i, item) in enumerate(player.inventory)
        yn = queryprompt(yesnoquitmenu, ["Y", "N", "Q"], "Remove $item? (Q to quit menu)")
        if yn == "Y"
            if !haskey(player.location.level.content, player.location.p)
                player.location.level.content[player.location.p] = Vector{Item}()
            end
            push!(player.location.level.content[player.location.p], item)
            deleteat!(player.inventory, i)
        elseif yn == "Q"
            break
        end
    end
end

function playertake(player)
    function addgold(gitem)
        if findfirst(x -> typeof(x) == Gold, player.inventory) == nothing
            push!(player.inventory, Gold(0))
        end
        player.inventory[findall(x -> typeof(x) == Gold,
            player.inventory)[1]].value += gitem.value
    end
    canpickup(i::Gold) = true
    canpickup(i::Sledge) = true
    canpickup(i::Ladder) = true
    canpickup(i::Item) = (push!(lchan, "Cannot pick that up."); false)
    takeit(g::Gold) = begin addgold(g); push!(lchan, "You add $(g.value) in gold.") end
    takeit(s::Sledge) = begin push!(player.inventory, s); push!(lchan, "You have a sledge.") end
    takeit(s::Ladder) = begin push!(player.inventory, s); push!(lchan, "You have a ladder.") end

    d = player.location.level.content
    if player.location.room != nothing && haskey(d, player.location.p)
        for _ in 1:length(d[player.location.p])
            item = pop!(d[player.location.p])
            if canpickup(item)
                takeit(item)
            else
                pushfirst!(d[player.location.p], item)
            end
        end
        if isempty(d[player.location.p])
            delete!(d, player.location.p)
            player.location.level.grid[player.location.p.x, player.location.p.y] = ' '
        else
            player.location.level.grid[player.location.p.x, player.location.p.y] =
                d[player.location.p][1].disp
        end
    end
end

function exitmap(player)
    s = "You exit the game with inventory $(player.inventory), wielding $(player.wielding)."
    push!(lchan, s)
    println(s)
    exit()
end

function newlevel(player, levelsdown, startup=false)
    oldlevel = player.location.level.floorlevel
    newlevel = oldlevel + levelsdown
    if newlevel < 1
        exitmap(player)
    else
        level = makelevel(newlevel)
        room = rand(level.rooms)
        point = randemptypoint(room, level)
        player.location = Location(point, room, level)
        playerroominfo(player)
    end
    player
end

function playerup(player)
    if haskey(player.location.level.content, player.location.p) &&
        hastype(Skylight, player.location.p, player.location.level) &&
        iswielding(Ladder, player)
        deleteat!(player.wielding, findfirst(i -> typeof(i) == Ladder, player.wielding))
        push!(lchan, "You have to leave your ladder behind.")
        newlevel(player, -1)
    else
        push!(lchan, "You need to wield a ladder while at a skylight to go upwards.")
    end
end

function playerdown(player)
    if haskey(player.location.level.content, player.location.p) &&
        hastype(Pit, player.location.p, player.location.level)
        newlevel(player, 1)
    else
        push!(lchan, "You can only move downwards at a Pit in the floor.")
    end
end

function playerdig(player)
    movedirs = Dict("N" => [-1, 0], "W" => [0, -1], "S" => [1, 0], "E" => [0, 1])
    if !iswielding(Sledge, player)
        push!(lchan, "You need to be wielding a Sledge.")
        return
    end
    direc = queryprompt(directionmenu, ["N", "S", "E", "W", "U", "D"], "Direction?")
    if direc == "U"
        if rand() < defaults.baseluck
            addatpoint(Skylight(), player.location.p, player.location.level)
            player.location.level.grid[player.location.p.x, player.location.p.y] = '^'
            push!(lchan, "There is now a hole in the ceiling above you.")
        end
    elseif direc == "D"
        if rand() < defaults.baseluck
            addatpoint(Pit(), player.location.p, player.location.level)
            player.location.level.grid[player.location.p.x, player.location.p.y] = '_'
            push!(lchan, "There is now a pit in the floor at your feet.")
        end
    else
        goalp = Point(player.location.p.x + movedirs[direc][1], player.location.p.y +
            movedirs[direc][2], player.location.p.z)
        if player.location.level.grid[goalp.x, goalp.y] == obstacles.wall
            if rand() < defaults.baseluck
                if defaults.fastdig
                    depth = 0
                    for i in 1:div(leveldim, 10)
                        p = Point(player.location.p.x + movedirs[direc][1] * i,
                            player.location.p.y + movedirs[direc][2] * i, player.location.p.z)
                        if player.location.level.grid[p.x, p.y] == obstacles.wall
                           player.location.level.grid[p.x, p.y] = ' '
                           depth += 1
                        end
                    end
                    if depth > 3
                        push!(lchan, "Boom! Your sledgehammer's attack on this wall is amazing.")
                    end
                else
                    player.location.level.grid[goalp.x, goalp.y] = ' '
                end
            end
        elseif player.location.level.grid[goalp.x, goalp.y] == obstacles.permwall
            push!(lchan, "This kind of wall cannot be removed.")
        end
    end
end

function playermove(player, dx, dy)
    goalp = Point(player.location.p.x + dx, player.location.p.y + dy, player.location.p.z)
    if player.location.level.grid[goalp.x, goalp.y] in obstacles
        push!(lchan, "Something is in the way.")
    else
        player.location = Location(goalp, player.location.room, player.location.level)
    end
end
playerN(player) = playermove(player, -1, 0)
playerW(player) = playermove(player, 0, -1)
playerS(player) = playermove(player, 1, 0)
playerE(player) = playermove(player, 0, 1)

function makeplayer(newlevel=1)
    level = makelevel(newlevel)
    room = rand(level.rooms)
    point = randemptypoint(room, level)
    player = Agent(Location(point, room, level))
    playerroominfo(player)
    player
end

const usercommands = Dict("N" => playerN, "S" => playerS, "E" => playerE, "W" => playerW,
                          "U" => playerup, "D" => playerdown, "I" => playerdisplayinventory,
                          "F" => playerroominfo, "T" => playertake, "R" => playerremove,
                          "Q" => playerequip, "A" => playerdig, "L" => aliasing, "H" => help)

function rungame()
    startchannelconsumers()
    player = makeplayer()
    while true
        updateroom(player)
        displayroom(player)
        choice = queryprompt(mainmenu, keys(usercommands))
        usercommands[choice](player)
    end
end

rungame()

```

