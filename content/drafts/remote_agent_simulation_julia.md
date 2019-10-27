+++
title = "Remote agent/Simulation/Julia"
description = ""
date = 2019-07-12T18:41:04Z
aliases = []
[extra]
id = 22422
[taxonomies]
categories = []
tags = []
+++

This implementation uses the Gtk GUI library. There are two files: a module and a file to implement the GUI server's Gtk game view.
<br />
File: RemoteGameServerClient.jl

```julia
module RemoteGameServerClient

using Sockets, Distributed, Colors

export Forward, TurnRight, TurnLeft, Get, Drop, GameOver, Stop, SectorRed,
 SectorGreen, SectorYellow, SectorBlue, BallRed, BallGreen, BallYellow,
    BallBlue, Bump, SectorFull, AgentFull, NoSectorBall, NoAgentBall

export North, East, South, West, Red, Green, Yellow, Blue, nocolor, Direction

export Pt, configs, asyncserversocketIO, asyncclientsocketIO, makegrid, hasball,
    iswall, inside, Sector, Agent, Grid, directions, sectorcolors, colorsectors,
    colorballs, adjacent, responses, compasspoints

const North = [-1, 0]
const East = [0, 1]
const South = [1, 0]
const West = [0, -1]
const Direction = Vector{Int}
const compasspoints = [North, East, South, West]

const Forward = '^'
const TurnRight = '>'
const TurnLeft = '<'
const Get = '@'
const Drop = '!'
const GameOver = '+'
const Stop = '.'
const SectorRed = 'R'
const SectorGreen = 'G'
const SectorYellow = 'Y'
const SectorBlue = 'B'
const BallRed = 'r'
const BallGreen = 'g'
const BallYellow = 'y'
const BallBlue = 'b'
const Bump = '|'
const SectorFull = 'S'
const AgentFull = 'A'
const NoSectorBall = 's'
const NoAgentBall = 'a'

const Red, Green, Blue, Yellow, nocolor = colorant"red", colorant"green", 
    colorant"blue", colorant"yellow", colorant"transparent"
const sectorcolors = [Red, Green, Blue, Yellow]
const colorsectors = Dict{Colorant,Char}(Red => SectorRed, Green => SectorGreen,
    Blue => SectorBlue, Yellow => SectorYellow)
const colorballs = Dict{Colorant,Char}(Red => BallRed, Green => BallGreen,
    Blue => BallBlue, Yellow => BallYellow)

const configs = Dict{String,Any}("dimensions" => (8, 12), "ballchance" => 0.67,
    "wallchance" => 0.10, "walltile" => '▒', "emptytile" => ' ',
    "unknowntile" => '?', "ip" => ip"127.0.0.1","portnumber" => 5021,
    "wallcolor" => colorant"darkgray", "unknowncolor" => colorant"gold")

mutable struct Sector
    ch::Char
    clr::Colorant
    ball::Colorant
end

struct Pt 
    x::Int
    y::Int
end
Pt(v::Vector{Int}) = new(v[1], v[2])

mutable struct Agent
    location::Pt
    direction::Direction
    ball::Colorant
end

mutable struct Grid
    mat::Matrix{Sector}
    agent::Agent
    turncount::Int64
end

hasball(obj) = obj.ball in sectorcolors
inside(m::Matrix{Sector}, x::Int, y::Int) = (0 < x <= size(m)[1]) && (0 < y <= size(m)[2])
inside(m::Matrix{Sector}, p::Pt) = inside(m, p.y, p.y)
adjacent(x, y) = [[x - 1, y], [x, y + 1], [x + 1, y], [x, y - 1]]
adjacent(p::Pt) = [Pt(a[1], a[2]) for a in adjacent(p.x, p.y)]
iswall(s::Sector) = s.ch == configs["walltile"]
iswall(m::Matrix{Sector}, x, y) = inside(m, x, y) && iswall(m[x, y])
iswall(m, p::Pt) = iswall(m, p.x, p.y)

allballsmatch(grid) = all(x -> iswall(x) || !hasball(x) || x.clr == x.ball, grid.mat)

function makegrid(height, width)
    wallcolor = configs["wallcolor"]
    m = Matrix{Sector}(undef, height, width)
    probinteriorwall = configs["wallchance"]
    while true
        for i in 1:height, j in 1:width
            m[i, j] = ((i == 1) || (j == 1) || (i == height) || (j == width)) ?
                Sector(configs["walltile"], wallcolor, nocolor) :
            m[i, j] = probinteriorwall > rand() ?
                Sector(Char(configs["walltile"]), wallcolor, nocolor) :
                configs["ballchance"] > rand() ?
                    Sector(Char(configs["emptytile"]), rand(sectorcolors, 2)...) :
                    Sector(Char(configs["emptytile"]), rand(sectorcolors), nocolor)
        end
        for i in 2:height-1, j in 2:width-1 # once walls up, open any isolated sectors
            arr = adjacent(Pt(i, j))
            if !iswall(m, i, j) && all(p -> iswall(m, p), arr)
                m[i, j] = Sector(Char(configs["emptytile"]), rand(sectorcolors, 2)...)
            end
        end
        if all(colr -> sum(s -> s.clr == colr, m) >= sum(s -> s.ball == colr, m), sectorcolors)
            break # redo if more balls than sectors for a color
        end
    end
    rows, cols = collect(2:height - 1), collect(2:width-1)
    while true
        i, j = rand(rows), rand(cols)
        if hasball(m[i, j]) # agent goes on empty sector at start
            m[i, j] = Sector(configs["emptytile"], m[i, j].clr, nocolor)
            return Grid(m, Agent(Pt(i, j), North, nocolor), 0)
        end
    end
end

function forward!(grid)
    reply = Char[]
    px, py = grid.agent.location.x, grid.agent.location.y
    newx, newy = px + grid.agent.direction[1], py + grid.agent.direction[2]
    if iswall(grid.mat, newx, newy)
        push!(reply, Bump)
    else
        grid.agent.location = Pt(newx, newy)
        sect = grid.mat[newx, newy]
        push!(reply, colorsectors[sect.clr])
        if hasball(sect)
            push!(reply, colorballs[sect.ball])
        end
        grid.turncount += 1
    end
    push!(reply, Stop)
    reply
end

const rightdelta = Dict(North => East, East => South, South => West, West => North)
const leftdelta = Dict(p[2] => p[1] for p in rightdelta)
rightturn!(grid) = begin grid.agent.direction = rightdelta[grid.agent.direction]; [Stop] end
leftturn!(grid) = begin grid.agent.direction = leftdelta[grid.agent.direction]; [Stop] end

function getball!(grid)
    reply = Char[]
    grid.turncount += 1
    if hasball(grid.agent)
        push!(reply, AgentFull)
    else
        p = grid.agent.location
        sect = grid.mat[p.x, p.y]
        if hasball(sect)
            grid.agent.ball = sect.ball
            sect.ball = nocolor
        else
            push!(reply, NoSectorBall)
        end
    end
    push!(reply, Stop)
    reply
end

function dropball!(grid)
    reply = Char[]
    grid.turncount += 1
    if !hasball(grid.agent)
        push!(reply, NoAgentBall)
    else
        p = grid.agent.location
        sect = grid.mat[p.x, p.y]
        if !hasball(sect)
            sect.ball = grid.agent.ball
            grid.agent.ball = nocolor
            if allballsmatch(grid)
                push!(reply, GameOver)
            end
        else
            push!(reply, SectorFull)
        end
    end
    push!(reply, Stop)
    reply
end

responses = Dict{Char,Function}(Forward => forward!, TurnRight => rightturn!,
    TurnLeft => leftturn!, Get => getball!, Drop => dropball!)

function asyncserversocketIO(inchan, outchan, debug=false)
    try
        socket = TCPSocket()
        server = listen(configs["ip"], configs["portnumber"])
        debug && println("Listening at ", configs["ip"], " ", configs["portnumber"])
        socket = accept(server)
        debug && println("Connected at ", configs["ip"], " ", configs["portnumber"])
        while isopen(socket) && isopen(inchan) && isopen(outchan)
            ch = read(socket, Char)
            put!(inchan, ch) 
            debug && println("socket sent to inchan: $ch")
            yield()
            while (ch = take!(outchan)) != '.'
                write(socket, ch)
                flush(socket)
                debug && println("outsocket: $ch")
            end
            write(socket, ch)
            flush(socket)
            debug && println("outsocket: $ch")
        end
    catch y
        @warn("Socket IO: caught exception $y")
    end
end

function asyncclientsocketIO(inchan, outchan, debug=false)
    try
        debug && println("Trying connection at ", configs["ip"], " ", configs["portnumber"])
        socket = connect(configs["ip"], configs["portnumber"])
        debug && println("Connected at ", configs["ip"], " ", configs["portnumber"])
        while isopen(socket) && isopen(inchan) && isopen(outchan)
            if !isready(outchan)
                sleep(0.05)
                yield()
            else
                while isready(outchan)
                    ch = take!(outchan)
                    debug && println("outsocket: $ch")
                    write(socket, ch)
                    flush(socket)
                end
                while (ch = read(socket, Char)) != '.'
                    put!(inchan, ch)
                    debug && println("inchan: $ch")
                end
                put!(inchan, ch)
                debug && println("inchan: $ch")             
            end
        end
    catch y
        println("Socket IO: caught exception $y")
    end
end

end # module RemoteGameServerClient


# modulino
if occursin(Base.PROGRAM_FILE, @__FILE__)

using .RemoteGameServerClient

function evalcommand(grid, inchan, outchan, debug=false)
    debug && println("Starting command evaluation service.")
    debug && println("Configuration settings are: $(RemoteGameServerClient.configs).")
    while isopen(inchan) && isopen(outchan)
        ch = take!(inchan)
        debug && println("responding to $ch as $(channelcodes[ch])")
        response = responses[channelcodes[ch]](grid)
        debug && println(" and $response")
        debug && println("Got code: $ch.  Response is: $response")
        for code in response
            ch = streamcodes[code]
            put!(outchan, ch)
        end
        if GameOver in response
            close(inchan)
            close(outchan)
            break
        end
    end
end

function newgame()
    serverin, serverout = Channel{Char}(100), Channel{Char}(100)
    @async asyncserversocketIO(serverin, serverout)
    grid = makegrid(10, 16)
    evalcommand(grid, serverin, serverout, true)
end

newgame()

end # modulino

```

File: gtk_remoteserversimulation.jl

```julia
using Distributed, Sockets, Gtk, Colors, Graphics, Cairo

include("RemoteGameServerClient.jl")

using .RemoteGameServerClient

function matchballsgameserver()
    # The player is not to know what direction they are facing, so as to not
    # give them more information about the map at start. However, we ideally would
    # want the map as learned by agent to be aligned with the map known to server.
    # To do so, we start agent facin north, but in a random location with the map
    # height and width randomly exchanged, so knowing facing north gives no net
    # information about the shape and contents of the grid.
    height, width = configs["dimensions"]
    if rand() > 0.5
        height, width = width, height
    end
    fontsize = configs["dimensions"][1] * 2
    win = GtkWindow("Match Color Balls Game      ", 600, 600) |> (GtkFrame() |> (box = GtkBox(:v)))
    can = GtkCanvas()
    set_gtk_property!(can, :expand, true)
    push!(box, can)
    grid = makegrid(height, width)

    @guarded Gtk.draw(can) do widget
        ctx = Gtk.getgc(can)
        select_font_face(ctx, "Courier", Cairo.FONT_SLANT_NORMAL, Cairo.FONT_WEIGHT_BOLD)
        set_font_size(ctx, fontsize)
        set_source_rgb(ctx, 0.2, 0.2, 0.2)
        l = fontsize * 2.5
        for i in 1:size(grid.mat)[1], j in 1:size(grid.mat)[2]
            fill(ctx)
            set_source(ctx, grid.mat[i, j].clr)
            rectangle(ctx, (i - 1) * l, (j - 1) * l, l, l)
            fill(ctx)
            if hasball(grid.mat[i, j])
                set_source(ctx, grid.mat[i, j].ball)
                circle(ctx, (i - 1) * l + fontsize * 1.25, (j - 1) * l + fontsize * 1.25, fontsize)
                fill(ctx)                
                set_source(ctx, colorant"gray")
                arc(ctx, (i - 1) * l + fontsize * 1.25, (j - 1) * l + fontsize * 1.25, fontsize, 0, 2*pi)
                stroke(ctx)                
            end
            if Pt(i, j) == grid.agent.location
                move_to(ctx, (i - 1) * l + fontsize * 1.25, (j - 1) * l + fontsize * 0.2)
                set_source(ctx, colorant"silver")
                line_to(ctx, i * l, j * l)
                line_to(ctx, (i - 1) * l, j * l)
                line_to(ctx, (i - 1) * l + fontsize * 1.25, (j - 1) * l + fontsize * 0.2)
                stroke(ctx) 
                set_source(ctx, grid.agent.ball == nocolor ? colorant"black" : grid.agent.ball)
                circle(ctx, (i - 1) * l + fontsize * 1.25, (j - 1) * l + fontsize * 2, fontsize / 4)
                fill(ctx)
                set_source(ctx, colorant"silver")
                arc(ctx, (i - 1) * l + fontsize * 1.25, (j - 1) * l + fontsize * 2, fontsize / 4, 0, 2*pi)
                stroke(ctx)                
            end
        end
    end

    function evalcommand(grid, inchan, outchan, debug=false)
        debug && println("Starting command evaluation service.")
        debug && println("Configuration settings are: $(RemoteGameServerClient.configs).")
        while isopen(inchan) && isopen(outchan)
            ch = take!(inchan)
            debug && println("responding to $ch input from client")
            response = responses[ch](grid)
            debug && println(" and response is then $response")
            draw(can)
            show(can)
            show(win)
            debug && println("screen update done")
            for ch in response
                put!(outchan, ch)
            end
            if GameOver in response
                for ch in [GameOver, GameOver, GameOver, GameOver, GameOver, Stop]
                    put!(outchan, ch)
                end
                sleep(10)
                close(inchan)
                close(outchan)
                break
            end
        end
    end
    
    serverin, serverout = Channel{Char}(100), Channel{Char}(100)
    @async asyncserversocketIO(serverin, serverout, false)
    @async evalcommand(grid, serverin, serverout, false)
    
    condition = Condition()
    endit(w) = notify(condition)
    signal_connect(endit, win, :destroy)
    showall(win)
    wait(condition)
end

matchballsgameserver()

```

