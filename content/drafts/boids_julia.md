+++
title = "Boids/Julia"
description = ""
date = 2019-06-08T09:27:59Z
aliases = []
[extra]
id = 22358
[taxonomies]
categories = []
tags = []
+++


```julia
using Gtk.ShortNames, Colors, Cairo, Graphics

const fontpointsize = 10
const mapwidth = 1000
const mapheight = 500
const windowmaxx = div(mapwidth, Int(round(fontpointsize * 0.92)))
const windowmaxy = div(mapheight, fontpointsize)
const basebuffer = fill(' ', windowmaxy, windowmaxx)

win = Window("Boids", mapwidth, mapheight) |> (can = Canvas())
set_gtk_property!(can, :expand, true)

@guarded Gtk.draw(can) do widget
    ctx = Gtk.getgc(can)
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


@enum Directions NW N NE E SE S SW W Here

const defaultdirection = E

boidmoves = Dict{Directions, Vector{Int}}(Here => [0, 0], NW => [-1, -1], N => [0, -1], NE => [1, -1],
    E => [1, 0], SE => [1, 1], S => [0, 1], SW => [-1, 1], W => [-1, 0])

struct Point
    x::Int
    y::Int
end

mutable struct Obstacle
    occupied::Vector{Point}
end

mutable struct Walls
    occupied::Vector{Point}
end

mutable struct Environment
    width::Int
    height::Int
    walls::Walls
    obstacles::Vector{Obstacle}
    buffer::Matrix{Char}
end

ebuf(e, p::Point) = e.buffer[p.x, p.y]

mutable struct Boid
    pos::Point
    flock::Vector{Boid}
end

aschar(b::Boid) = 'o'
aschar(o::Obstacle) = '*'
aschar(w::Walls) = '\u2593'

function buildwalls(environ)
    for i in 1:environ.height
        push!(environ.walls.occupied, Point(1, i), Point(environ.width, i))
    end
    for i in 1:environ.width
        push!(environ.walls.occupied, Point(i, 1), Point(i, environ.height))
    end
    for p in environ.walls.occupied
        if 0 < p.x <= environ.width && 0 < p.y <= environ.height
            environ.buffer[p.y, p.x] = aschar(environ.walls)
        end
    end
end

inellipse(dx, dy, a, b) = (dx / a)^2 + (dy / b)^2 < 1.0
inellipseat(p, x, y, a, b) = inellipse(x - p.x, y - p.y, a, b)

function buildobstacles(environ, n=5)
    widthoptions = collect(3:max(5, div(environ.height, 10)))
    heightoptions = collect(7:max(10, div(environ.height, 2)))
    for i in 1:n
        obst = Obstacle(Point[])
        push!(environ.obstacles, obst)
        w, h = rand(widthoptions), rand(heightoptions)
        w, h = (w > h) ? (h, w) : (w, h)
        center = Point(Int(round(environ.width * rand())), Int(round(environ.height * rand())))
        for y in center.y-h:center.y+h, x in center.x-w:center.x+w
            if inellipseat(center, x, y, w, h) && 1 < x < environ.width && 1 < y < environ.height
                push!(obst.occupied, Point(x, y))
                environ.buffer[y, x] = aschar(obst)
            end
        end
    end
end

function buildenvironment()
    environ = Environment(windowmaxx, windowmaxy, Walls(Point[]), Obstacle[], basebuffer)
    buildwalls(environ)
    buildobstacles(environ)
    environ
end

function addflock(allflocks, numboids, environ)
    f = Vector{Boid}()
    center = Point(12, div(environ.height, 2))
    varpick = collect(-10:10)
    while length(f) < numboids
        while true
            newboid = Boid(Point(center.x + rand(varpick), center.y + rand(varpick)), f)
            if all(x -> x.pos != newboid.pos, f) && environ.buffer[newboid.pos.y, newboid.pos.x] == ' '
                push!(f, newboid)
                break
            end
        end
    end
    push!(allflocks, f)
end

function availablemoves(boid, environ)
    avail = Vector{Directions}()
    for (direc, v) in boidmoves
        if environ.buffer[boid.pos.y + v[2], boid.pos.x + v[1]] == ' '
            push!(avail, direc)
        end
    end
    avail
end

function center(flock)
    xs, ys, n = 0, 0, length(flock)
    for b in flock
        xs += b.pos.x
        ys += b.pos.y
    end
    Point(Int(round(xs / n)), Int(round(ys / n)))
end

isobstacle(x, y, environ) = (c = environ.buffer[y, x]; c != '*' && c != '\u2593')
nextobstaclex(pos, e) = (x = pos.x + 1; while e.buffer[pos.y, x] == ' ' x += 1 end; x)

nearobs(b, e, delt=8) = b.pos.x - nextobstaclex(b.pos, e) < delt
atobs(b, e) = e.buffer[b.pos.y, b.pos.x + 1] != ' '

function nearbyopen(b, e, opendist = e.width)
    dist, y = findmax([nextobstaclex(Point(b.pos.x, y), e) for y in 1:e.height])
    return y
end

function move(boid, d::Directions)
    m = boidmoves[d]
    boid.pos = Point(boid.pos.x + m[1], boid.pos.y + m[2])
end

showboid(b, environ) = begin x, y = b.pos.x, b.pos.y; environ.buffer[y, x] = aschar(b) end
hideboid(b, environ) = begin x, y = b.pos.x, b.pos.y; environ.buffer[y, x] = ' ' end
showmove(b, e) = begin hideboid(b, e); move(b, e); showboid(b, e) end

function move(boid, environ::Environment)
    possmoves = availablemoves(boid, environ)
    fcenter = center(boid.flock)
    wantsouth, wantnorth = false, false
    if atobs(boid, environ)
        wanty = nearbyopen(boid, environ, 8)
        d = wanty > boid.pos.y && S in possmoves ? S : N in possmoves ? N : rand(possmoves)
        move(boid, d)
        return
    end
    if rand() > 0.5 && nearobs(boid, environ)
        wanty = nearbyopen(boid, environ, 8)
        d = wanty > boid.pos.y && SE in possmoves ? SE : NE in possmoves ? NE : E
        move(boid, d)
        return
    end
    if rand() > 0.5 && fcenter.x < boid.pos.x - 2 && W in possmoves
        move(boid, W)
        return
    end
    if fcenter.y > boid.pos.y + 1
        wantsouth = true
    elseif fcenter.y < boid.pos.y - 1
        wantnorth = true
    end
    if wantsouth
        if SE in possmoves
            move(boid, SE)
        elseif S in possmoves
            move(boid, S)
        end
    elseif wantnorth
        if NE in possmoves
            move(boid, NE)
        elseif N in possmoves
            move(boid, N)
        end
    elseif E in possmoves
        move(boid, E)
    end
end

const itemcolors = Dict{Char, Colorant}('o' => colorant"white", ' ' => colorant"black", '*' => colorant"gold",
                                       '\u2593' => colorant"silver")

environ = buildenvironment()
const allflocks = Vector{Vector{Boid}}()
addflock(allflocks, 5, environ)

draw(can)
show(can)

while true 
    sleep(0.5)
    for flock in allflocks, boid in flock
        showmove(boid, environ)
    end
    draw(can)
end

```

