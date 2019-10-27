+++
title = "Morpion solitaire/Phix"
description = ""
date = 2019-07-19T09:34:24Z
aliases = []
[extra]
id = 22432
[taxonomies]
categories = []
tags = []
+++


## Phix

{{libheader|pGUI}}
I focused on a half-decent gui and playing back the 178-record.

Gruntwork of searching 10^23 possible moves left as an exercise...

```Phix
-- demo\rosetta\Morpion_solitaire.exw
--
--  Download http://www.morpionsolitaire.com/Grid5T178Rosin.txt and
--  save it to the current directory, if you want this to replay it.
--
constant p178file = "Grid5T178Rosin.txt"

--
--  One point worth clarifying, suppose you have:
--
--      ?
--      **
--      * *
--      *  *
--      *   *
--
--  Then placing a tile at 1,1/'?' makes //**either**// a | or a \,
--  but *not* both. In fact, your next tile could then go at either
--  0,0 (if you made a |) or 1,0 (if you made a \), making the other
--  one, and leaving one tile on row 5 unused (however both tiles on
--  rows 2-4 become part of a 5-set). (Aside: the indexes just used
--  are quite unlike the extending/double-spaced ones used below.)
--

constant help_text = """
The game of Morpion Solitaire.

The aim is to make as many lines of 5 tiles as you can.
Lines may cross and share endpoints, but not overlap.
Valid places where a new tile may be placed are shown in orange.

The worst case game is 20 lines, the world record is 178.
The play-178 button is disabled if it cannot open Grid5T178Rosin.txt 
in the current directory. Use +/- to speed up/slow down the playback.
"""

include pGUI.e

Ihandle dlg, canvas, hframe, history, play178, timer

sequence board
--
-- board is {string}, with odd col&row as tiles, either even as spaces/lines, eg
--
--       123456789012345
--     {`...............`, 1
--      `...............`, 2
--      `..O-O-O.....O..`, 3
--      `..|\|X|..../...`, 4
--      `..O-O-O-O-O....`, 5
--      `..|.|\|\./.....`, 6
--      `..O-O-O-O-O....`, 7
--      `..|.|.|X.\.....`, 8
--      `..O-O-O-O-O....`, 9
--      `..|.|/|..\.\...`, 10
--      `..O-O-O-O-O.O..`, 11
--      `...............`, 12
--      `?..............`} 13
--
-- in other words if odd(col) and odd(row) then [r,c] must be one of ".O", else
-- [r,c] must be one of ".-|\/X", for every single element/char of board[][].
-- (It turned out pretty easy to map that to a fairly nice gui, plus of course
--  the above proved far easier to debug than (say) a bunch of bit-settings!
--  Likewise for debugging '.' are somewhat easier to count/follow than ' '. )
--
-- There is a double-space border so that tiles can be placed (ie clicked on),
-- at which point the board is automatically extended with a new double edge.
-- For instance, playing a tile at the spot marked '?' means we must insert
-- two spaces at the start of every line, add two new blank lines on the end,
-- and run through the entire history/playback adding {2,0} to everything. At
-- {1,1}, obviously, you have to add {2,2} to everything (and nowhere else).

sequence valid_moves = {}
-- saved in redraw_cb(), for click testing in button_cb():
integer r = 0, r2 = 0
-- delay between moves in playback mode
atom pause = 1

sequence played = {},
         playback = {}

function redraw_cb(Ihandle ih, integer /*posx*/, integer /*posy*/)
    integer {cw,ch} = IupGetIntInt(ih, "DRAWSIZE"),
            bw = length(board[1]),
            bh = length(board)
    
    cdCanvas cddbuffer = IupGetAttributePtr(ih,"DBUFFER")
    IupGLMakeCurrent(ih)
    cdCanvasActivate(cddbuffer)
    cdCanvasClear(cddbuffer)
    integer mx = min(floor(cw/((bw+1)/2)),floor(ch/((bh+1)/2)))
    r = floor(mx/2) -- save for button_cb()
    mx = r*2    -- (prevent drift)
    r2 = r*r
    integer t = r+floor(r/3),
            cy = ch-r, cx = r,
            hr = floor(r/2)
    -- draw grid
    cdCanvasSetForeground(cddbuffer,CD_LIGHT_GREY)
    while cx<cw or cy>0 do
        cdCanvasLine(cddbuffer,cx,1,cx,ch)
        cdCanvasLine(cddbuffer,1,cy,cw,cy)
        cx += mx
        cy -= mx
    end while
    -- draw lines
    cdCanvasSetForeground(cddbuffer,CD_BLACK)
    cy = ch-mx
    for y=2 to bh do
        cx = r
        integer step = 1+and_bits(y,1)
        for x=2 to bw by step do 
            integer c = board[y][x]
            if c!='.' then
                if c='-' then
                    cdCanvasLine(cddbuffer,cx,cy,cx+mx,cy)
                elsif c='|' then
                    cdCanvasLine(cddbuffer,cx+r,cy-r,cx+r,cy+r)
                elsif c='\\' then
                    cdCanvasLine(cddbuffer,cx,cy+r,cx+mx,cy-r)
                elsif c='X' then
                    cdCanvasLine(cddbuffer,cx,cy+r,cx+mx,cy-r)
                    cdCanvasLine(cddbuffer,cx+mx,cy+r,cx,cy-r)
                elsif c='/' then
                    cdCanvasLine(cddbuffer,cx,cy-r,cx+mx,cy+r)
                end if
            end if
            cx += step*r
        end for
        cy -= r
    end for
    -- draw tiles
    cy = ch-r
    for y=1 to bh by 2 do
        cx = r
        for x=1 to bw by 2 do 
            if board[y][x]='O' then
                cdCanvasSetForeground(cddbuffer,CD_GREY)
                cdCanvasSector(cddbuffer, cx, cy, t, t, 0, 360)
                cdCanvasSetForeground(cddbuffer,CD_DARK_GREY)
                cdCanvasCircle(cddbuffer, cx, cy, t)
            end if
            cx += mx
        end for
        cy -= mx
    end for
    -- draw valid moves
    cdCanvasSetForeground(cddbuffer,CD_ORANGE)
    for i=1 to length(valid_moves) do
        integer {x,y} = valid_moves[i]
        if i>1 and {x,y}=valid_moves[i-1][1..2] then
            cdCanvasSetForeground(cddbuffer,CD_DARK_RED)
        else    
            cdCanvasSetForeground(cddbuffer,CD_ORANGE)
        end if
        cx = x*r
        cy = ch-y*r
        cdCanvasLine(cddbuffer,cx-hr,cy,cx+hr,cy)
        cdCanvasLine(cddbuffer,cx,cy+hr,cx,cy-hr)
    end for     
    cdCanvasFlush(cddbuffer)
    return IUP_DEFAULT
end function

function map_cb(Ihandle ih)
    IupGLMakeCurrent(ih)
    atom res = IupGetDouble(NULL, "SCREENDPI")/25.4
    cdCanvas cddbuffer = cdCreateCanvas(CD_GL, "10x10 %g", {res})
    IupSetAttributePtr(ih,"DBUFFER",cddbuffer)
    cdCanvasSetBackground(cddbuffer, CD_PARCHMENT)
    return IUP_DEFAULT
end function

function canvas_resize_cb(Ihandle canvas)
    cdCanvas cddbuffer = IupGetAttributePtr(canvas,"DBUFFER")
    integer {canvas_width, canvas_height} = IupGetIntInt(canvas, "DRAWSIZE")
    atom res = IupGetDouble(NULL, "SCREENDPI")/25.4
    cdCanvasSetAttribute(cddbuffer, "SIZE", "%dx%d %g", {canvas_width, canvas_height, res})
    return IUP_DEFAULT
end function

constant directions = {{-1,-1,'\\','/'},
                       {-1, 0,'|','.'},
                       {+1,-1,'/','\\'},
                       { 0,-1,'-','.'}}

function scan_d(integer y, x, dx, dy, bh, bw, xc)
    integer count = 0
    for i=1 to 4 do
        x += dx
        y += dy
        if x=0 or y=0 or x>bw or y>bh then exit end if
        integer link = board[y,x]
        if link!='.' and link!=xc then exit end if
        x += dx
        y += dy
        integer tile = board[y,x]
        if tile!='O' then exit end if
        count += 1      
    end for
    return count
end function

procedure find_valid_moves()
    integer bh = length(board),
            bw = length(board[1])
    valid_moves = {}
    for y=1 to bh by 2 do
        for x=1 to bw by 2 do
            if board[y][x]='.' then
                for d=1 to length(directions) do
                    -- (obviously) this is what we're looking for:
                    -- OOOO.    -2
                    -- OOO.O    -1
                    -- OO.OO     0
                    -- O.OOO    +1
                    -- .OOOO    +2
                    -- with lc as count left of dot, and rc right,
                    -- the (only) "dirty trick" below is "2-lc".
                    integer {dy, dx, nc, xc} = directions[d]
                    integer lc = scan_d(y,x,+dx,+dy,bh,bw,xc),
                            rc = scan_d(y,x,-dx,-dy,bh,bw,xc)
                    while lc+rc>=4 do
                        if lc=-1 then ?9/0 end if   -- sanity check
                        sequence move = {x,y,nc&"",2-lc}
                        if not find(move,valid_moves) then
                            valid_moves = append(valid_moves,move)
                        end if
                        lc -= 1
                    end while
                end for
            end if
        end for
    end for
    valid_moves = sort(valid_moves) -- (entirely optional, helps debug)
end procedure

procedure redraw_all()
    find_valid_moves()
    IupUpdate(canvas)
end procedure

procedure adjust_moves(integer dx, dy)
    for i=1 to length(playback) do
        playback[i][1] += dx
        playback[i][2] += dy
    end for
    for i=1 to length(played) do
        played[i][1] += dx
        played[i][2] += dy
    end for
end procedure

procedure make_move(integer i)
    sequence vmi = valid_moves[i]
    integer {x, y, {c}, d} = vmi
    IupSetStrAttribute(history,"APPENDITEM","%v",{vmi})
    played = append(played,vmi)
    board[y][x] = 'O'
    integer {dy, dx, l, nl} = directions[find(c,vslice(directions,3))],
            {ul,dr} = {d*2-3,d*2+3}
            -- "" are 1..7, -1..5, -3..3, -5..1, -7..-1
            --  for d= +2     +1      0     -1     -2,
            --      ie +2: .-O-O-O-O
            --         +1: O-.-O-O-O
            --          0: O-O-.-O-O
            --         -1: O-O-O-.-O
            --         -2: O-O-O-O-.
    for j=ul to dr by 2 do -- make lines/links
        integer ly = y-j*dy,
                lx = x-j*dx,
                nc = board[ly][lx]
        if nc='.' then
            nc = l
        elsif nc=nl then
            nc = 'X'
        else
            ?9/0
        end if
        board[ly][lx] = nc
    end for
    -- then extend board if rqd (maintain a double-space border)
    if x=1 then
        -- extend left
        for i=1 to length(board) do
            board[i] = ".."&board[i]
        end for
        adjust_moves(2,0)
    elsif x=length(board[1]) then
        -- extend right
        for i=1 to length(board) do
            board[i] &= ".."
        end for
    end if
    -- (copy the undamaged lines from the other end...)
    if y=1 then
        -- extend up
        board = board[$-1..$]&board
        adjust_moves(0,2)
    elsif y=length(board) then
        -- extend down
        board &= board[1..2]
    end if
    redraw_all()
end procedure

function button_cb(Ihandle /*canvas*/, integer button, pressed, x, y, atom /*pStatus*/)
    if button=IUP_BUTTON1 and not pressed then      -- (left button released)
        sequence possible = {}
        for i=1 to length(valid_moves) do
            integer {cx,cy} = sq_sub(sq_mul(valid_moves[i][1..2],r),{x,y})
            if (cx*cx+cy*cy)<=r2 then
                possible &= i
            end if
        end for
        if length(possible)>1 then
            -- This needs some kind of popup... (IupPopup, IupMenu, IUP_MOUSEPOS...)
            -- with menu entries such as:
            --   | +2
            --   - -2
            --   / +1
            --   \ 0
            ?"ambiguous... (tbc)"
        end if
        if length(possible)=1 then
            integer i = possible[1]
            make_move(i)
        end if
    end if
    return IUP_CONTINUE
end function

procedure set_hframe_title()
    string title = "History"
    if IupGetInt(play178,"ACTIVE") and IupGetInt(play178,"RUNNING") then
        string e = elapsed(pause)
        e = e[1..find(',',e)-1]
        e = e[1..match(" and ",e)-1]
        title = sprintf("Playing world record (%s/move)",{e})
    end if
    IupSetStrAttribute(hframe,"TITLE",title)
end procedure

procedure fill_square(integer x1, x2, y1, y2, ch)
    for x=x1 to x2 by 2 do
        for y=y1 to y2 by 2 do
            board[y][x] = ch
        end for
    end for
end procedure

procedure new_game()
    board = repeat(repeat('.',23),23)
    -- solid-fill a big '+'...
    fill_square( 3,21, 9,15,'O')
    fill_square( 9,15, 3,21,'O')
    -- then vacate inner '+'
    fill_square( 5,19,11,13,'.')
    fill_square(11,13, 5,19,'.')
    played = {}
    IupSetAttribute(history,"REMOVEITEM","ALL")
    IupSetInt(play178,"RUNNING",false)
    find_valid_moves()
    set_hframe_title()
end procedure

function new_game_cb(Ihandle /*ih*/)
    new_game()
    redraw_all()
    return IUP_DEFAULT
end function

function exit_cb(Ihandle /*ih*/)
    return IUP_CLOSE
end function

function help_cb(Ihandln /*ih*/)
    IupMessage("Morpion Solitaire",help_text)
    return IUP_DEFAULT
end function

function play178_cb(Ihandln /*ih*/)
    sequence text = get_text(p178file,GT_LF_STRIPPED),
             res = {}, r
    integer dx, dy
    bool first = true
    for i=1 to length(text) do
        string ti = text[i]
        if ti[1]!='#' then
            r = scanf(ti,iff(first?"(%d,%d)","(%d,%d) %c %d"))
            if length(r)!=1 then
                IupMessage("Error","Error processing line %d (%s) [%v]",{i,ti,r})
                return IUP_DEFAULT
            end if
            r = r[1]
            r[1..2] = sq_mul(r[1..2],2)
            if first then
                {dy,dx} = sq_sub(9,r)
                first = false
            else
                r[1] += dy
                r[2] += dx
                res = append(res,r)
            end if
        end if
    end for
    new_game()
    playback = res
    IupSetInt(play178,"RUNNING",true)
    set_hframe_title()
    IupSetInt(timer,"RUN",true)
    return IUP_DEFAULT
end function

function timer_cb(Ihandle /*ih*/)
    if length(playback)=0 then
        IupSetInt(timer,"RUN",false)
    else
        sequence move = playback[1]
        integer p = find(move,valid_moves)
        if p=0 then ?9/0 end if
        playback = playback[2..$]
        make_move(p)
    end if
    return IUP_DEFAULT
end function

function key_cb(Ihandle /*dlg*/, atom c)
    if c=K_ESC then return IUP_CLOSE
    elsif c=K_F1 then return help_cb(NULL)
    elsif c='?' then ?valid_moves
    elsif find(c,"+-") then
        --(Initially 1s/move: you cannot actually stop it, 
        -- but 20+ makes it wait 6 days between moves,
        -- and obviously 20- makes it finish in 0.0001s)
        if c='+' and pause<250000000 then
            pause *= 2
        elsif c='-' and pause>0.01 then
            pause /= 2
        end if
        set_hframe_title()
        IupSetInt(timer,"TIME",round(pause*1000))
        if IupGetInt(timer,"RUN") then
            -- restart needed to apply new TIME
            IupSetInt(timer,"RUN",false)
            IupSetInt(timer,"RUN",true)
        end if
    end if
    return IUP_CONTINUE
end function

procedure main()
    IupOpen()
 
    canvas = IupGLCanvas("RASTERSIZE=200x200")
    history = IupList("VISIBLELINES=10, EXPAND=YES")
    hframe = IupFrame(history,"TITLE=History, PADDING=5x4")
    play178 = IupButton("Play 178",Icallback("play178_cb"),"PADDING=5x4")
    IupSetInt(play178,"RUNNING",false)
    Ihandle newgame = IupButton("New Game",Icallback("new_game_cb"),"PADDING=5x4"),
            help = IupButton("Help (F1)",Icallback("help_cb"),"PADDING=5x4"),
            quit = IupButton("E&xit",Icallback("exit_cb"),"PADDING=5x4"),
            buttons = IupHbox({newgame,IupFill(),help,IupFill(),play178,IupFill(),quit}),
            full = IupHbox({canvas,IupVbox({hframe,buttons})})
    IupSetInt(play178,"ACTIVE",file_exists(p178file))
    IupSetCallbacks({canvas}, {"ACTION", Icallback("redraw_cb"),
                               "MAP_CB", Icallback("map_cb"),
                               "RESIZE_CB", Icallback("canvas_resize_cb"),
                               "BUTTON_CB", Icallback("button_cb")})
    dlg = IupDialog(IupHbox({full},"MARGIN=3x3"),"TITLE=\"Morpion Solitaire\"")
    IupSetCallback(dlg, "K_ANY", Icallback("key_cb"))
    new_game()
    IupShowXY(dlg,IUP_CENTER,IUP_CENTER)
    IupSetAttribute(dlg, "RASTERSIZE", NULL)
    IupSetStrAttribute(dlg, "MINSIZE", IupGetAttribute(dlg,"RASTERSIZE"))
    timer = IupTimer(Icallback("timer_cb"), 1000, active:=false)
    IupMainLoop()
    IupClose()
end procedure
 
main()
```

