+++
title = "Remote agent/Agent logic/Julia"
description = ""
date = 2019-07-12T18:43:56Z
aliases = []
[extra]
id = 22425
[taxonomies]
categories = []
tags = []
+++

The agent uses a random walk algorithm, to which is added a preference for unknown tiles, to explore the board, and the following: 
<br />
When the agent does not have a ball, the agent will tend to move toward a sector on a straight line path from agent (if such a sector has a mismatch between sector and ball colors), to get the next ball for agent. 
<br />
When the agent holds a ball, it will have a preference for straight line paths leading to an empty sector the color of the agent's ball in order to drop the ball.
<br />
If the agent carries the same ball for over 25 moves, it will drop that ball and look for a differently colored ball.

```julia
using Distributed, Gtk, Colors, Cairo

include("RemoteGameServerClient.jl")

using .RemoteGameServerClient

const dirorder = Dict(North => 1, East => 2, South => 3, West => 4)
const orderdir = Dict(1 => North, 2 => East, 3 => South, 4 => West)
const rightturns = [[North, East], [East, South], [South, West], [West, North]]
const leftturns = [[East, North], [South, East], [West, South], [North, West]]
const turnpi = [[East, West], [South, North], [West, East], [North, South]]
const dlabl = Dict(North => "north", East => "east", South => "south", West => "west")
const param = Dict("won" => false, "carriedsame" => 0, "bannedball" => nocolor)

function ballhandler(grid)
    sector = grid.mat[grid.agent.location.x, grid.agent.location.y]
    agent = grid.agent
    if rand() > 0.8
        param["bannedball"] = nocolor # banned color is one that was hard to drop last time
    end
    if sector.ch == configs["emptytile"] && sector.clr != sector.ball
        if (sector.ball == nocolor && agent.ball == sector.clr) ||
            (sector.ball == nocolor && agent.ball != nocolor && param["carriedsame"] > 25)
            sector.ball = agent.ball
            agent.ball = nocolor
            param["carriedsame"] = 0 # was able to drop this color
            param["bannedball"] = (sector.clr != sector.ball) ? sector.ball : nocolor
            return [Drop]
        elseif sector.ball != nocolor && agent.ball == nocolor && 
                    sector.ball != param["bannedball"]
            agent.ball = sector.ball
            sector.ball = nocolor
            return [Get]
        end
    end
    Char[]
end

function turn(grid, from::Direction, to::Direction)
    grid.agent.direction = to
    [from, to] in rightturns ? [TurnRight] :
    [from, to] in leftturns ? [TurnLeft] :
    [from, to] in turnpi ? [TurnRight, TurnRight] :
    Char[]
end

forhaswrongball(sector, agent) = sector.ball == nocolor && sector.clr == agent.ball
forhasnoball(sector, agent) = sector.ball != nocolor && sector.clr != sector.ball

function sectortoward(grid, ax, ay, dir, f)
    x, y = ax + dir[1], ay + dir[2]
    while grid.mat[x, y].ch == configs["emptytile"]
        if f(grid.mat[x, y], grid.agent)
            return true
        end
        x, y = x + dir[1], y + dir[2]
    end
    return false
end

function chooseforward(grid)
    ret = ballhandler(grid)
    ag = grid.agent
    nearby = [grid.mat[a[1], a[2]].ch for a in adjacent(ag.location.x, ag.location.y)]
    allunknown = [orderdir[i] for i in 1:length(nearby) if nearby[i] == configs["unknowntile"]]
    allempty = [orderdir[i] for i in 1:length(nearby) if nearby[i] == configs["emptytile"]]
    grid.turncount += 1
    if ag.ball != nocolor
        param["carriedsame"] += 1
    end
    if nearby[dirorder[ag.direction]] == configs["unknowntile"]
        return vcat(ret, [Forward])
    elseif length(allunknown) > 0
        return vcat(ret, turn(grid, ag.direction, rand(allunknown)), [Forward])
    elseif length(allempty) > 0
        x, y = ag.location.x, ag.location.y
        f = (ag.ball == nocolor) ? forhasnoball : forhaswrongball
        for dir in allempty
            if sectortoward(grid, x, y, dir, f)
                return vcat(ret, turn(grid, ag.direction, dir), [Forward])
            end
        end
        return vcat(ret, turn(grid, ag.direction, rand(allempty)), [Forward])
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
wonexec(c, g, s, l) = param["won"] = true

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

const charexec = Dict{Char, Function}(GameOver => wonexec,
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
                set_source(ctx, (grid.agent.ball == nocolor) ? colorant"black" : grid.agent.ball)
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
            debug && println("Currently agent is facing ", grid.agent.direction, " holding ", grid.agent.ball)
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
                if param["won"] == true
                    println("Game over, agent won in ", grid.turncount, " moves.")
                    warn_dialog("You have WON the game!\nTotal moves: $(grid.turncount)", win)
                    close(inchan)
                    close(outchan)
                end
            end
        end
    end

    grid = makeunknowngrid(height, width)
    serverin, serverout = Channel{Char}(100), Channel{Char}(100)
    @async asyncclientsocketIO(serverin, serverout, false)
    @async clientsendcommand(grid, serverin, serverout, false)

    condition = Condition()
    endit(w) = notify(condition)
    signal_connect(endit, win, :destroy)
    showall(win)
    wait(condition)
end

matchballsgameclient()

```

