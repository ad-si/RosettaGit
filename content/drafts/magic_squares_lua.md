+++
title = "Magic squares/Lua"
description = ""
date = 2017-05-31T07:46:25Z
aliases = []
[extra]
id = 21403
[taxonomies]
categories = []
tags = []
+++


## Lua


```lua

function makeArray( s )
    local q = {}
    for j = 1, s do
        table.insert( q, {} )
        for i = 1, s do
            table.insert( q[j], 0 )
        end
    end
    return q
end

-- [[ odd magic square ]] --
function buildOMS( s )
    if s % 2 == 0 then s = s + 1 end
    local q, p = makeArray( s ), 1
    local i, j, ti, tj = 1 + math.floor( s / 2 ), 1
    while( p <= s * s ) do
        q[i][j] = p
        ti = i + 1; if ti > s then ti = 1 end
        tj = j - 1; if tj < 1 then tj = s end
        if q[ti][tj] ~= 0 then
            ti = i; tj = j + 1
        end
        i = ti; j = tj; p = p + 1;
    end
    return q, s
end

-- [[ singly even magic square ]] --
function buildSEMS( s )
    if s % 2 == 1 then s = s + 1 end
    while( s % 4 == 0 ) do s = s + 2 end

    local q, z, o = makeArray( s ), math.floor( s / 2 )
    local b, c, d, a = z * z; c = 2 * b; d = 3 * b

    o = buildOMS( z )
    for j = 1, z do
        for i = 1, z do
            a = o[i][j]
            q[i][j] = a
            q[i + z][j + z] = a + b
            q[i + z][j] = a + c
            q[i][j + z] = a + d
        end
    end

    local lc = math.floor( z / 2 )
    local rc, t = lc - 1 
    for j = 1, z do
        for i = 1, s do  
            if i <= lc or i > s - rc or ( i == lc + 1 and j == lc + 1 ) then
                if not( i == 1 and j == lc+ 1 ) then
                    t = q[i][j]
                    q[i][j] = q[i][j + z]
                    q[i][j + z] = t
                end
            end
        end
    end
    return q, s
end

-- [[ doubly even magic square ]] --
function buildDEMS( s )
    while( s % 4 > 0 ) do s = s + 1 end
    local q = makeArray( s )
    local temp, n, tot, sx, sy = {{1,0,0,1}, {0,1,1,0}, {0,1,1,0}, {1,0,0,1}}, 
          0, s * s
    for j = 1, s do
        for i = 1, s do
            sx = i % 4; if sx < 1 then sx = 4 end
            sy = j % 4; if sy < 1 then sy = 4 end
            if temp[sy][sx] == 1 then q[i][j] = n + 1
            else q[i][j] = tot - n end
            n = n + 1
        end
    end
    return q, s
end

function myFormat( s, l )
    for i = 1, l - #s do
        s = "0" .. s
    end
    return s .. " "
end

LOG_10 = 2.302585092994
function display( q, s )
    io.write( string.format( " - %d x %d\n", s, s ) )
    local k = 1 + math.floor( math.log( s * s ) / LOG_10 )
    for j = 1, s do
        for i = 1, s do
            io.write( myFormat( string.format( "%d", q[i][j] ), k ) )
        end
        print()
    end
    io.write( string.format( "Magic sum: %d\n", s * ( ( s * s ) + 1 ) / 2 ) )
end

--[[ entry point ]]--
io.write( "\nOdd Magic Square" )
display( buildOMS( 9 ) )

io.write( "\nSingly Even Magic Square" )
display( buildSEMS( 6 ) )

io.write( "\nDoubly Even Magic Square" )
display( buildDEMS( 8 ) )


```

{{out}}
```txt

Odd Magic Square - 9 x 9
47 58 69 80 01 12 23 34 45
57 68 79 09 11 22 33 44 46
67 78 08 10 21 32 43 54 56
77 07 18 20 31 42 53 55 66
06 17 19 30 41 52 63 65 76
16 27 29 40 51 62 64 75 05
26 28 39 50 61 72 74 04 15
36 38 49 60 71 73 03 14 25
37 48 59 70 81 02 13 24 35
Magic sum: 369

Singly Even Magic Square - 6 x 6
35 01 06 26 19 24
03 32 07 21 23 25
31 09 02 22 27 20
08 28 33 17 10 15
30 05 34 12 14 16
04 36 29 13 18 11
Magic sum: 111

Doubly Even Magic Square - 8 x 8
01 63 62 04 05 59 58 08
56 10 11 53 52 14 15 49
48 18 19 45 44 22 23 41
25 39 38 28 29 35 34 32
33 31 30 36 37 27 26 40
24 42 43 21 20 46 47 17
16 50 51 13 12 54 55 09
57 07 06 60 61 03 02 64
Magic sum: 260

```

