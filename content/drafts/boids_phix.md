+++
title = "Boids/Phix"
description = ""
date = 2017-09-20T03:30:07Z
aliases = []
[extra]
id = 21410
[taxonomies]
categories = []
tags = []
+++

Only uses about 2.5% cpu with 60 boids at 25 FPS. Split into two source files, for no particular reason.
{{libheader|pGUI}}

```Phix
--
-- demo\pGUI\boids3d.exw
--
-- Originally by by Matt Lewis
-- Ported from arwen to pGUI, Pete Lomax June 2017
--
include pGUI.e
include boids3d.e

constant TITLE = "Boids 3D on pGUI"

Ihandle canvas, dialog,
        restart, speed_label, speed_txt, dist_label, dist_txt, 
        radius_label, radius_txt, boids_label, boids_txt, shadow_check
cdCanvas cddbuffer, cdcanvas

integer ox,oy,oz
procedure set_observer()
    {ox,oy,oz} = {floor(X_MAX/2),floor(Y_MAX/2),Z_MAX*2}
end procedure

sequence colors = {}

sequence verts = {}

integer draw_shadows = 1

integer dx = 0, dy = 0, dw = 0, dh = 0

function project_point(sequence pt)
atom d
atom px,py,pz

    {px,py,pz} = pt
    if pz=oz then
        d = 0.0001
    else
        d =  1-pz/(pz-oz)
    end if

    px = floor(ox+(ox-px)/d)
    py = dh - floor(oy+(oy-py)/d)

    return {px,py}
end function

procedure draw_lines(sequence coords)
--
-- Draw zero or more lines.
--  The lines are drawn between the sets of coordinates in coords.
--  This sequence can contain Colors, Lines, or Points:
--      A Color is a single atom that is a 24-bit color value. Subsequent lines use this color.
--      A Line is a 4-element sequence {X1,Y1,X2,Y2} that specifies the X,Y position of a lines
--          starting point and the X,Y position of its end point. The line is drawn from X1,Y1
--          to X2,Y2.
--      A Point is a 2-element sequence {X,Y} that gives the X,Y position of the end-point
--          of a line. The line is drawn to this position from the last end-point supplied.
--          There must have been a preceding Line (or Point), else x1 unassigned error.
--
-- If no color parameters are supplied, the current pen color for the control is used.
--
-- Example:
--
--      -- draw a shape in TheWindow
--      draw_lines({White,{40,0,0,80},{80,80},{40,0},
--                  Blue,{40,5,0,85},{80,85},{40,5}})
--
--  draws white lines {40,0}..{0,80}, {0,80}..{80,80}, {80,80}..{40,0}
--    and blue lines {40,5}..{0,85}, {0,85}..{80,85}, {80,85}..{40,5}.
--
atom x1, y1, x2, y2
object ci

    for i = 1 to length(coords) do
        ci = coords[i]
        if atom(ci) then
            cdCanvasSetForeground(cddbuffer, ci)
        elsif length(ci) = 4 then
            {x1,y1,x2,y2} = ci
            cdCanvasLine(cddbuffer,x1,y1,x2,y2)
            {x1,y1} = {x2,y2}
        elsif length(ci) = 2 then
            {x2,y2} = ci
            cdCanvasLine(cddbuffer,x1,y1,x2,y2)
            {x1,y1} = {x2,y2}
        else
            ?9/0
        end if
    end for
end procedure

procedure draw_polygon(sequence points)
    integer {{x1,y1},{x2,y2},{x3,y3}} = points
    cdCanvasBegin(cddbuffer,CD_FILL)
    cdCanvasVertex(cddbuffer,x1,y1)
    cdCanvasVertex(cddbuffer,x2,y2)
    cdCanvasVertex(cddbuffer,x3,y3)
    cdCanvasEnd(cddbuffer)
end procedure

function redraw_cb(Ihandle /*ih*/, integer /*posx*/, integer /*posy*/)
integer N, x, y, z
atom x1, y1, z1,
     x2, y2, z2
    {dw,dh} = IupGetIntInt(canvas, "DRAWSIZE")
    {dx,dy} = {floor(dw/4)+1,floor(dh/4)+1}
    {X_MAX,Y_MAX,Z_MAX} = {dw,dh,floor((dw+dh)/2)}
    set_observer()
    cdCanvasActivate(cddbuffer)
    cdCanvasClear(cddbuffer)
    cdCanvasSetForeground(cddbuffer, #909090)

    {x1,y1,z1} = {X_MIN,  0.0,Z_MIN}
    {x2,y2,z2} = {X_MAX,Y_MAX,Z_MAX}

    -- draw the verticals on the sides and horizontals on the floor and ceiling
    N = floor((z2-z1)/100)
    if N<3 then N = 3 end if
    for i=1 to N-1 do
        z = z1+floor((i/N)*(z2-z1))
        draw_lines({#808080,
                    project_point({x2, y2, z}) & project_point({x2, y1, z}),
                    project_point({x1, y1, z}),
                    project_point({x1, y2, z}),
                    #FFFFFF,
                    project_point({x2, y2, z})})
    end for

    -- draw the horizontals on the back and the not-quite horizontals on the sides
    N = floor((y2-y1)/100)
    if N<3 then N = 3 end if
    for i=1 to N-1 do
        y = y1+floor((i/N)*(y2-y1))
        draw_lines({#808080,
                    project_point({x1,y,z1}) & project_point({x1,y,z2}),
                    project_point({x2,y,z2}),
                    project_point({x2,y,z1})})
    end for

    -- draw the verticals on the back and the not-quite-verticals on floor/ceiling
    N = floor((x2-x1)/100)
    if N<3 then N = 3 end if
    for i=1 to N-1 do
        x = x1+floor((i/N)*(x2-x1))
        draw_lines({#808080,
                    project_point({x,y2,z2}) & project_point({x,y1,z2}),
                    project_point({x,y1,z1}),
                    #FFFFFF,
                    project_point({x,y2,z1}) & project_point({x,y2,z2})})
    end for

    -- draw the minimal room outline
    draw_lines({CD_BLACK,
                project_point({x1, y1, z2}) & project_point({x2, y1, z2}),
                project_point({x2, y2, z2}),
                project_point({x1, y2, z2}),
                project_point({x1, y1, z2}),
                project_point({x1, y1, z1}) & project_point({x1, y1, z2}),
                project_point({x2, y1, z1}) & project_point({x2, y1, z2}),
                project_point({x2, y2, z1}) & project_point({x2, y2, z2}),
                project_point({x1, y2, z1}) & project_point({x1, y2, z2})})

    if draw_shadows then
        cdCanvasSetForeground(cddbuffer, #A0A0A0)
        for i=1 to BOIDS do
            draw_polygon(verts[i][6..8])
        end for
    end if

    -- draw boids as polygons, starting with farthest from the POV
    for i=BOIDS to 1 by -1 do
        sequence v = verts[i]
        cdCanvasSetForeground(cddbuffer, colors[v[5]])
        draw_polygon(v[2..4])
    end for

    cdCanvasFlush(cddbuffer)
    return IUP_DEFAULT
end function

constant null_u = {0,0}
function get_ortho(sequence v)
sequence y, yhat, z, u

    u = v[1..2]
    if equal(u, null_u) then
        return {1,1,0}
    end if
    y = {1,1}
    yhat = sq_mul(sq_div(dot(u,y),dot(u,u)),u)
    z = sq_sub(y,yhat)
    if equal(z, null_u) then
        return {1,1,0}
    end if
    return z & 0
end function

function timer_cb(Ihandle /*ih*/)
sequence in,boid, pt, v, v1, v2, v3, sv

    move_boids()

    in = boidsnp1

    if length(colors)=0 then
        colors = repeat(0, BOIDS)
        for i=1 to BOIDS do
            colors[i] = rand(#FFFFFF)
        end for
    end if

    -- convert boid location and velocities into triangles in 3D 
    -- and project them onto the screen
    verts = repeat("", BOIDS)
    for i=1 to BOIDS do

        boid = in[i]

        pt = boid[B_X..B_Z]
        v = boid[B_XV..B_ZV]

        if equal(v,{0,0,0}) then
            v = {1,0,1}
        end if

        -- the 'nose' of the boid
        v1 = sq_add(pt,make_length(v, 20))

        -- make the base of the triangle
        sv = make_length(get_ortho(v), 4)
        v2 = sq_add(pt,sv)
        v3 = sq_sub(pt,sv)

        verts[i] = {distance3(pt, {ox,oy,oz}), v1, v2, v3, i, v1, v2, v3}

        -- project the 3D points onto a 2D viewing surface
        for j=2 to 4 do
            verts[i][j] = project_point(verts[i][j])
        end for
        if draw_shadows then
            for j=6 to 8 do
                pt = verts[i][j]
                pt[2] = 0.0
                verts[i][j] = project_point(pt)
            end for
        end if

    end for

    -- sort by distance, so nearer boids clip those that are more distant   
    verts = sort(verts)

    IupUpdate(canvas)
    return IUP_IGNORE
end function

function add_boids(integer num)
sequence boid = repeat(0, B_ELEMENTS)
    if num>BOIDS then
        for i=BOIDS+1 to num do
            boid[B_X] = rand(X_MAX)
            boid[B_Y] = rand(Y_MAX)
            boid[B_Z] = rand(Z_MAX)

            boid[B_XV] = floor(V_MAX/2)-rand(V_MAX)
            boid[B_YV] = floor(V_MAX/2)-rand(V_MAX)
            boid[B_ZV] = floor(V_MAX/2)-rand(V_MAX)
            boidsnp1 = append(boidsnp1, boid)
            colors &= rand(#FFFFFF)
        end for
        boidsn = boidsnp1
    end if
--  BOIDS = num
    return num
end function

function valuechanged_cb(Ihandle ih)
    integer i = IupGetInt(ih, "VALUE")
    switch ih do
        case speed_txt:  V_MAX  = max(i,1)
        case dist_txt:   DIST   = max(i,10)
        case radius_txt: N_DIST = max(i,1)
        case boids_txt:  BOIDS  = add_boids(i)
    end switch
    return IUP_DEFAULT
end function

function shadow_cb(Ihandle /*ih*/, integer state)
    draw_shadows = state
    return IUP_DEFAULT
end function

function restart_cb(Ihandle /*ih*/)
    setup()
    return IUP_DEFAULT
end function

function map_cb(Ihandle ih)
    cdcanvas = cdCreateCanvas(CD_IUP, ih)
    cddbuffer = cdCreateCanvas(CD_DBUFFER, cdcanvas)
    cdCanvasSetBackground(cddbuffer, CD_GREY)
    return IUP_DEFAULT
end function

function esc_close(Ihandle /*ih*/, atom c)
    if c=K_ESC then return IUP_CLOSE end if
    return IUP_CONTINUE
end function

procedure main()
Ihandle hbox

    IupOpen()

    canvas = IupCanvas("RASTERSIZE=625x690")
    IupSetCallback(canvas, "MAP_CB", Icallback("map_cb"))
    IupSetCallback(canvas, "ACTION", Icallback("redraw_cb"))

    speed_label = IupLabel("Max Speed","PADDING=0x4")
    speed_txt = IupText("VALUECHANGED_CB", Icallback("valuechanged_cb"), 
                        "SPIN=Yes, SPINMIN=1, RASTERSIZE=48x")
    IupSetInt(speed_txt,"VALUE",V_MAX)

    dist_label = IupLabel("Separation","PADDING=0x4")
    dist_txt = IupText("VALUECHANGED_CB", Icallback("valuechanged_cb"), 
                             "SPIN=Yes, SPINMIN=10, SPINMAX=600, RASTERSIZE=48x")
    IupSetInt(dist_txt,"VALUE",DIST)

    radius_label = IupLabel("Neighbor Radius","PADDING=0x4")
    radius_txt = IupText("VALUECHANGED_CB", Icallback("valuechanged_cb"), 
                         "SPIN=Yes, SPINMIN=1, SPINMAX=200, RASTERSIZE=48x")
    IupSetInt(radius_txt,"VALUE",N_DIST)

    boids_label = IupLabel("Boids","PADDING=0x4")
    boids_txt = IupText("VALUECHANGED_CB", Icallback("valuechanged_cb"), 
                        "SPIN=Yes, SPINMIN=1, SPINMAX=200, RASTERSIZE=48x")
    IupSetInt(boids_txt,"VALUE",BOIDS)

    shadow_check = IupToggle("Draw Shadows","ACTION",Icallback("shadow_cb"),"VALUE=ON")
    restart = IupButton("Restart","ACTION",Icallback("restart_cb"),"GAP=0x14")

    hbox = IupHbox({IupVbox({speed_label,speed_txt},"NORMALIZESIZE=HORIZONTAL"),
                    IupVbox({dist_label,dist_txt},"NORMALIZESIZE=HORIZONTAL"),
                    IupVbox({radius_label,radius_txt},"NORMALIZESIZE=HORIZONTAL"),
                    IupVbox({boids_label,boids_txt},"NORMALIZESIZE=HORIZONTAL"),
                    IupVbox({shadow_check,restart},"NORMALIZESIZE=HORIZONTAL")})
    IupDestroy(IupNormalizer({speed_label,shadow_check},"NORMALIZE=VERTICAL"))
    dialog = IupDialog(IupVbox({canvas,hbox}, "MARGIN=5x5, GAP=5"),"MINSIZE=455x170")
    IupSetAttribute(dialog,"TITLE",TITLE);
    IupSetCallback(dialog, "K_ANY", Icallback("esc_close"));

    Ihandle hTimer = IupTimer(Icallback("timer_cb"), 40)
    setup()
    set_observer()
    {} = timer_cb(hTimer)
    IupShow(dialog)
    IupSetAttribute(canvas, "RASTERSIZE", NULL)

    IupMainLoop()
    IupClose()
end procedure
main()
```



```Phix
--
-- demo\pGUI\boids3d.e
-- 
### =============

--
global integer BOIDS = 60
--             OBSTACLES = 3
global atom N_DIST = 75.0,
            DIST = 30.0,
            V_MIN = 4.0,
            V_MAX = 10.0,
            X_MIN = 0.0,
            Y_MIN = 0.0,
            Z_MIN = 0.0,
            X_MAX = 600.0,
            Y_MAX = 600.0,
            Z_MAX = 600.0

constant DIST_FACTOR = 1.0/100.0

global enum B_X, B_Y, B_Z, B_XV, B_YV, B_ZV, B_ELEMENTS = B_ZV

global sequence boidsn, boidsnp1
--, obstacles

function magnitude3(sequence v)
-- return the scalar magnitude of a 3D vector
    return sqrt(v[1]*v[1]+v[2]*v[2]+v[3]*v[3])
end function

global function make_length(sequence v, atom l)
-- change the scalar magnitude of a 3D vector
    return sq_mul(v,l/magnitude3(v))
end function

global function dot(sequence u, sequence v)
-- return the dot product of 2 2D vectors
    return u[1]*v[1]+u[2]*v[2]
end function

global function distance3(sequence v1, sequence v2)
-- return the distance between two 3D vectors
    return magnitude3(sq_sub(v1[1..3],v2[1..3]))
end function

function boids_dist(integer b1, integer b2)
-- return the distance between two boids, identified by their position in the boidsn sequence
    return distance3(boidsn[b1], boidsn[b2])
end function

sequence n_id, n_dist

function neighbors(integer bid, atom distance)
-- return a list of boids within the N_DIST radius of a specified boid
atom dist
integer ix
sequence n, nid, ndist

    if bid=1 then
        n_id = repeat("", BOIDS)
        n_dist = n_id
    end if

    nid = n_id[bid]
    ndist = n_dist[bid]
    n = repeat({}, BOIDS)
    ix = 0
    for i=1 to length(n_id[bid]) do
        ix += 1
        n[ix] =  {nid[i], ndist[i]}
    end for

    for i=bid+1 to BOIDS do
        dist = boids_dist(bid, i)
        if dist<=distance then
            ix += 1
            n[ix] = {i,dist}
            n_id[i] &= bid
            n_dist[i] &= dist
        end if
    end for

    return n[1..ix]
end function

procedure maintain_distance(integer bid, sequence n)
-- alter a boids velocity to try to stay at least DIST away from other boids
atom dx, dy, dz
sequence this, other

    dx = 0.0
    dy = 0.0
    dz = 0.0

    this = boidsn[bid]

    for i=1 to length(n) do
        if n[i][2]<DIST then

            other = boidsn[n[i][1]]

            dx -= (other[B_X]-this[B_X])*2
            dy -= (other[B_Y]-this[B_Y])*2
            dz -= (other[B_Z]-this[B_Z])*2

        end if

    end for

    dx *= DIST_FACTOR
    dy *= DIST_FACTOR
    dz *= DIST_FACTOR

    boidsnp1[bid][B_XV] += dx
    boidsnp1[bid][B_YV] += dy
    boidsnp1[bid][B_ZV] += dz
end procedure

procedure avoid_walls(integer bid)
-- avoid the boundaries of MAX and MIN for each dimension (X, Y, Z)
sequence this
atom dx, dy, dz, t
    dx = 0.0
    dy = 0.0
    dz = 0.0
    this = boidsn[bid]
    t = this[B_X]
    if t<DIST+X_MIN then
        dx += 1
    elsif t>X_MAX-DIST then
        dx -= 1
    end if

    t = this[B_Y]
    if t<DIST+Y_MIN then
        dy += 1
    elsif t>Y_MAX-DIST then
        dy -= 1
    end if

    t = this[B_Z]
    if t<DIST+Z_MIN then
        dz += 1
    elsif t>Z_MAX-DIST then
        dz -= 1
    end if

    boidsnp1[bid][B_XV] += dx
    boidsnp1[bid][B_YV] += dy
    boidsnp1[bid][B_ZV] += dz
end procedure

procedure match_velocity(integer bid, sequence n)
-- try to match the velocity of a boid to its neighbors
atom dx, dy, dz
sequence this, other

    if length(n) then
        dx = 0.0
        dy = 0.0
        dz = 0.0
        this = boidsn[bid]

        for i=1 to length(n) do
            other = boidsn[n[i][1]]

            dx += other[B_XV]
            dy += other[B_YV]
            dz += other[B_ZV]
        end for

        dx /= length(n)
        dy /= length(n)
        dz /= length(n)

        dx -= this[B_XV]
        dy -= this[B_YV]
        dz -= this[B_ZV]

        dx *= DIST_FACTOR
        dy *= DIST_FACTOR
        dz *= DIST_FACTOR

        boidsnp1[bid][B_XV] += dx
        boidsnp1[bid][B_YV] += dy
        boidsnp1[bid][B_ZV] += dz
    end if
end procedure

procedure move_to_center(integer bid, sequence n)
-- try to move a boid toward the center of its neighbors
atom x, y, z
sequence other
    if length(n) then
        x = 0.0
        y = 0.0
        z = 0.0
        for i=1 to length(n) do
            other = boidsn[n[i][1]]
            x += other[B_X]
            y += other[B_Y]
            z += other[B_Z]
        end for

        -- compute the center
        x /= length(n)
        y /= length(n)
        z /= length(n)

        -- figure out the direction...
        other = boidsn[bid]
        x -= other[B_X]
        y -= other[B_Y]
        z -= other[B_Z]

        x *= DIST_FACTOR
        y *= DIST_FACTOR
        z *= DIST_FACTOR

        boidsnp1[bid][B_XV] += x
        boidsnp1[bid][B_YV] += y
        boidsnp1[bid][B_ZV] += z
    end if
end procedure

procedure constrain(integer bid)
-- don't let them go too fast or too slow
    atom mag = magnitude3(boidsnp1[bid][B_XV..B_ZV])
    if mag>V_MAX then
        boidsnp1[bid][B_XV..B_ZV] = sq_div(boidsnp1[bid][B_XV..B_ZV],mag/V_MAX)
    elsif mag<V_MIN then
        if mag then
            boidsnp1[bid][B_XV..B_ZV] = sq_mul(boidsnp1[bid][B_XV..B_ZV],V_MIN/mag)
        else
            boidsnp1[bid][B_XV] = V_MIN*rand(100)/100
            boidsnp1[bid][B_YV] = V_MIN*rand(100)/100
            boidsnp1[bid][B_ZV] = V_MIN*rand(100)/100
        end if
    end if
end procedure

procedure move(integer bid)
    boidsnp1[bid][B_X..B_Z] = sq_add(boidsnp1[bid][B_X..B_Z],boidsnp1[bid][B_XV..B_ZV])
end procedure

global procedure setup()
atom mag
    boidsn = repeat(repeat(0.0, B_ELEMENTS), BOIDS)
    boidsnp1 = boidsn

    -- place them randomly
    for boid=1 to BOIDS do
        boidsnp1[boid][B_X] = rand(X_MAX)
        boidsnp1[boid][B_Y] = rand(Y_MAX)
        boidsnp1[boid][B_Z] = rand(Z_MAX)

        boidsnp1[boid][B_XV] = V_MAX-rand(2*V_MAX)
        boidsnp1[boid][B_YV] = V_MAX-rand(2*V_MAX)
        boidsnp1[boid][B_ZV] = V_MAX-rand(2*V_MAX)

        mag = magnitude3(boidsnp1[boid])/V_MAX

        if mag>1.0 then
            boidsnp1[boid][B_XV..B_ZV] = sq_div(boidsnp1[boid][B_XV..B_ZV],mag)
        end if
    end for

--  obstacles = repeat({},OBSTACLES)
--  for o=1 to OBSTACLES do
--      obstacles[o] = {rand(X_MAX), rand(Y_MAX), rand(Z_MAX), 30}
--  end for

    boidsn = boidsnp1

end procedure

global procedure move_boids()
sequence n
    for boid=1 to BOIDS do
        n = neighbors(boid, N_DIST)
        maintain_distance(boid, n)
        match_velocity(boid, n)
        move_to_center(boid, n)
        avoid_walls(boid)
        constrain(boid)
        move(boid)
    end for
    boidsn = boidsnp1
end procedure
```

