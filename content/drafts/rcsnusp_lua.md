+++
title = "RCSNUSP/Lua"
description = ""
date = 2017-05-19T08:49:00Z
aliases = []
[extra]
id = 21377
[taxonomies]
categories = []
tags = []
+++


## Lua


```lua

-- directions
UP, DOWN, LEFT, RIGHT = 1, 2, 4, 8
-- snusp 
snIPC, snIPL, snMPT, snDIR, snMEMORY, snSTACK, snCODE = 1, 1, 1, RIGHT, {0}, {}, {}

-- stackframe
Stackframe = {}
function Stackframe:new()
    local sf = { IPC, IPL, DIR = 0, 0, 0 }
    self.__index = self
    return setmetatable( sf, self )
end

function memStep( a )
    snMPT = snMPT + a
    if snMPT < 0 then return false end
    if snMPT > #snMEMORY then 
        table.insert( snMEMORY, 0 )
    end
    return true
end
function changeDir( d )
    if d == RIGHT then
        if snDIR == RIGHT then snDIR = UP
        elseif snDIR == LEFT then snDIR = DOWN
        elseif snDIR == DOWN then snDIR = LEFT
        else snDIR = RIGHT end
    elseif d == LEFT then
        if snDIR == RIGHT then snDIR = DOWN
        elseif snDIR == LEFT then snDIR = UP
        elseif snDIR == DOWN then snDIR = RIGHT
        else snDIR = LEFT end
    end
end
function step()
    if snDIR == RIGHT then snIPC = snIPC + 1
    elseif snDIR == LEFT then snIPC = snIPC - 1
    elseif snDIR == DOWN then snIPL = snIPL + 1
    elseif snDIR == UP then snIPL = snIPL - 1
    end
    if snIPL > #snCODE or snIPL < 1 then return false end
    if snIPC > #snCODE[snIPL] or snIPC < 1 then return false end
    return true
end
function pushFrame()
    local sf = Stackframe:new(); sf.IPC = snIPC; 
    if snDIR == RIGHT then sf.IPC = sf.IPC + 1
    elseif snDIR == LEFT then sf.IPC = sf.IPC - 1
    end
    sf.IPL = snIPL; 
    if snDIR == DOWN then sf.IPL = sf.IPL + 1
    elseif snDIR == UP then sf.IPL = sf.IPL - 1
    end
    sf.DIR = snDIR
    table.insert( snSTACK, 1, sf ) 
end
function popFrame()
    if #snSTACK < 1 then return false end
    local sf = table.remove( snSTACK, 1 )
    snIPC = sf.IPC; snIPL = sf.IPL; snDIR = sf.DIR
    sf = nil
    return true
end
function exec( c )
    local res = true
    if c == "<" then res = memStep( -1 )
    elseif c == ">" then res = memStep(  1 )
    elseif c == "+" then snMEMORY[snMPT] = snMEMORY[snMPT] + 1
    elseif c == "-" then snMEMORY[snMPT] = snMEMORY[snMPT] - 1
    elseif c == "." then io.write( string.char( snMEMORY[snMPT] ) )
    elseif c == "," then snMEMORY[snMPT] = string.byte( io.read() )
    elseif c == "!" then res = step()
    elseif c == "?" and snMEMORY[snMPT] == 0 then res = step()
    elseif c == "@" then pushFrame()
    elseif c == "#" then res = popFrame()
    elseif c == "/" then changeDir( RIGHT )
    elseif c == "\\" then  changeDir( LEFT )
    end
    return res
end
function run( filename )
    local i, lc = assert( io.open( filename, "rb" ) ), 1
    for ln in io.lines( filename ) do
        for c = 1, #ln do
            if ln:sub( c, c ) == "$" then 
                snIPL = lc; snIPC = c
            end
        end
        snCODE[#snCODE + 1] = ln
        lc = lc + 1
      end
    repeat
        if not exec( snCODE[snIPL]:sub( snIPC, snIPC ) ) then break end
    until step() == false
end
-- [[ entry point ( argument = snusp program file name ) ]]--
if arg[1] ~= nil then run( arg[1] ) end

```

