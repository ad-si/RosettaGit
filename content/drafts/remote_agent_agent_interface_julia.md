+++
title = "Remote agent/Agent interface/Julia"
description = ""
date = 2019-07-09T11:00:38Z
aliases = []
[extra]
id = 22423
[taxonomies]
categories = []
tags = []
+++

In order to do the agent task in two parts, the implementation below is crippled: it leaves out the ball handling code. The simulated agent interacts with the server to explore the game board, mapping the board visibly on the Gtk game app. The third part of the simulation task implements the logic for actually winning the game.

```julia
using Distributed, Gtk, Colors, Cairo

include("RemoteGameServerClient.jl")

using .RemoteGameServerClient

const dirorder = Dict(North => 1, East => 2, South => 3, West => 4)
const orderdir = Dict(1 => North, 2 => East, 3 => South, 4 => West)
const rightturns = [[North, East], [East, South], [South, West], [West, North]]
const leftturns = [[East, North], [South, East], [West, South], [North, West]]
const turnpi = [[East, West], [South, North], [West, East], [North, South]]

function turn(grid, from::Direction, to::Direction)
    grid.agent.direction = to
    [from, to] in rightturns ? [TurnRight] :
    [from, to] in leftturns ? [TurnLeft] :
    [from, to] in turnpi ? [TurnRight, TurnRight] :
    Char[]
end

function chooseforward(grid)
    ag = grid.agent
    nearby = [grid.mat[a[1], a[2]].ch for a in adjacent(ag.location.x, ag.location.y)]
    allunknown = [orderdir[i] for i in 1:length(nearby) if nearby[i] == configs["unknowntile"]]
    allempty = [orderdir[i] for i in 1:length(nearby) if nearby[i] == configs["emptytile"]]
    if nearby[dirorder[ag.direction]] == configs["unknowntile"]
        return [Forward]
    elseif length(allunknown) > 0
        return vcat(turn(grid, ag.direction, rand(allunknown)), [Forward])
    elseif length(allempty) > 0
        return vcat(turn(grid, ag.direction, rand(allempty)), [Forward])
    else
        throw("Cannot find a way out from location ", ag.location)
    end
end

function makeunknowngrid(height, width)
    m = Matrix{Sector}(undef, height, width)
    for i in 1:height, j in 1:width
        m[i, j] = ((i == 1) || (j == 1) || (i == height) || (j == width)) ?
            Sector(configs["walltile"], configs["wallcolor"], nocolor) :
            Sector(configs["unknowntile"], configs["unknowncolor"], nocolor)
    end
    Grid(m, Agent(Pt(height ÷ 2, width ÷ 2), North, nocolor), 0)
end

const scolordict = Dict(p[2] => p[1] for p in colorsectors)
const ballcolordict = Dict(lowercase(p[2]) => p[1] for p in colorsectors)
nullexec(c, g, s, l) = ()
warnexec(c, g, s, l) = @warn("Server told client command was in error because $c")
ballexec(c, g, s, l) = begin s.ball = ballcolordict[c] end

function bumpexec(c, g, s, l)
    if s.ch == configs["emptytile"]
        throw("Bump for a sector at $newlocation we marked empty ($newsector)")
    end
    s.ch, s.clr, s.ball = configs["walltile"], configs["wallcolor"], nocolor
end

function intosectorexec(c, g, s, l)
    if s.ch == configs["walltile"]
        throw("The sector at $l was marked as a wall, now is marked empty")
    end
    g.agent.location = l
    s.ch = configs["emptytile"]
    s.clr = scolordict[c]
end

const charexec = Dict{Char, Function}(GameOver => nullexec,
    Bump => bumpexec, SectorFull => warnexec, AgentFull => warnexec,
    NoSectorBall => warnexec, NoAgentBall => warnexec, 
    SectorRed => intosectorexec, SectorGreen => intosectorexec, 
    SectorYellow => intosectorexec, SectorBlue => intosectorexec, 
    BallRed => ballexec, BallGreen => ballexec, 
    BallYellow => ballexec, BallBlue => ballexec)

function processreplies(chars, grid)
    newx = grid.agent.location.x + grid.agent.direction[1]
    newy = grid.agent.location.y + grid.agent.direction[2]
    newsector, newlocation = grid.mat[newx, newy], Pt(newx, newy)
    for cmd in chars
        if cmd == Stop
            break
        end
        charexec[cmd](cmd, grid, newsector, newlocation)
    end
end

function matchballsgameclient()
    # The agent starts facing north, but in a random location on map, with the map
    # height and width randomly excahnged. Therefore, starting facing north gives
    # no useful information about the map contents, since its shape is not known.
    height = maximum(configs["dimensions"]) * 2 # big enough for any
    width, fontsize = height, configs["dimensions"][1] * 2
    win = GtkWindow("Match Color Balls Game Client Running     ", 600, 600) |>
        (GtkFrame() |> (box = GtkBox(:v)))
    can = GtkCanvas()
    set_gtk_property!(can, :expand, true)
    push!(box, can)
    grid = makeunknowngrid(height, width)

    @guarded Gtk.draw(can) do widget
        ctx = Gtk.getgc(can)
        select_font_face(ctx, "Courier", Cairo.FONT_SLANT_NORMAL, Cairo.FONT_WEIGHT_BOLD)
        set_source_rgb(ctx, 0.2, 0.2, 0.2)
        l = fontsize * 2.5
        for i in 1:size(grid.mat)[1], j in 1:size(grid.mat)[2]
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
                set_source(ctx, grid.agent.ball)
                circle(ctx, (i - 1) * l + fontsize * 1.25, (j - 1) * l + fontsize * 2, fontsize / 4)
                fill(ctx)
                set_source(ctx, colorant"silver")
                arc(ctx, (i - 1) * l + fontsize * 1.25, (j - 1) * l + fontsize * 2, fontsize / 4, 0, 2*pi)
                stroke(ctx)
            end
            if grid.mat[i, j].ch == configs["unknowntile"]
                move_to(ctx, (i - 1) * l + fontsize * 0.8 , (j - 1) * l + fontsize * 2)
                set_font_size(ctx, fontsize * 1.5)
                set_source(ctx, colorant"silver")
                show_text(ctx, "?")
            end
        end
    end

    function clientsendcommand(grid, inchan, outchan, debug=false)
        debug && println("Starting client game play.")
        debug && println("Configuration settings are: $(RemoteGameServerClient.configs).")

        while isopen(inchan) && isopen(outchan)
            draw(can)
            show(can)
            show(win)
            debug && println("Currently agent is facing ", grid.agent.direction)
            cmds = chooseforward(grid)
            for ch in cmds
                draw(can)
                show(can)
                show(win)
                debug && println("Sending command as char $ch")
                put!(outchan, ch)
                ch, reply = '\0', Char[]
                while (ch = take!(inchan)) != '.'
                    push!(reply, ch)
                end
                push!(reply, ch) # '.'
                debug && println("clientsendcommand: Reply from server is $reply")
                processreplies(reply, grid)
            end
        end
    end

    grid = makeunknowngrid(height, width)
    serverin, serverout = Channel{Char}(100), Channel{Char}(100)
    @async asyncclientsocketIO(serverin, serverout, false)
    @async clientsendcommand(grid, serverin, serverout, true)

    condition = Condition()
    endit(w) = notify(condition)
    signal_connect(endit, win, :destroy)
    showall(win)
    wait(condition)
end

matchballsgameclient()
<lang>
