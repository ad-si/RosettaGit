+++
title = "Robots/Phix"
description = ""
date = 2019-04-24T19:24:15Z
aliases = []
[extra]
id = 22068
[taxonomies]
categories = []
tags = []
+++


## Phix

{{trans|C++}}

```Phix
constant W = 62, H = 42, INC = 10

integer robotsCount, aliveRobots, score, X, Y, alive
sequence board = repeat(repeat(' ',W),H)
 
procedure clearBoard()
    board = repeat(repeat('#',W),H)
    for y=2 to H-1 do
        for x=2 to W-1 do
            board[y,x] = ' '
        end for
    end for
end procedure
 
procedure printScore()
    position(H,1); bk_color(GREEN); text_color(BRIGHT_GREEN);
    printf(1,"      SCORE: %d      ", score)
end procedure
 
procedure createBoard()
    aliveRobots = robotsCount
    for x=1 to robotsCount do
        while true do
            integer a = rand(W),
                    b = rand(H)
            if board[b,a]=' ' then
                board[b,a]='+'
                exit
            end if
        end while
    end for
end procedure
 
procedure displayBoard()
    position(1,1)
    for y=1 to H do
        for x=1 to W do
            integer t = board[y,x],
                    k = find(t," #+A*@")
            bk_color(BLACK)
            text_color({WHITE,BRIGHT_BLUE,YELLOW,BRIGHT_RED,BRIGHT_RED,BRIGHT_GREEN}[k])
            puts(1,t)
        end for
        puts(1,"\n")
    end for
    printScore()
end procedure
 
procedure checkCollision(integer x, y)
    if X==x and Y==y then
        alive = false
        board[y,x] = 'A'
    else
        integer c = board[y,x]
        if find(c,"*+~") then
            board[y,x] = '*'
            c = 1+(c!='*')
            aliveRobots -= c
            score += c
        end if
    end if
end procedure
 
procedure moveRobots()
    for y=1 to H do
        for x=1 to W do
            if board[y,x]='+' then
                integer tx = x+compare(X,x), 
                        ty = y+compare(Y,y)
                if tx!=x or ty!=y then
                    board[y,x] = ' '
                    if board[ty,tx]=' ' then
                        board[ty,tx] = '~'
                    else
                        checkCollision(tx, ty)
                    end if
                end if
            end if
        end for
    end for
    for y=1 to H do
        for x=1 to W do
            if board[y,x]=='~' then board[y,x] = '+' end if
        end for
    end for
end procedure
 
procedure execute(integer x, y)
    board[Y,X] = ' '; X += x; Y += y;
    board[Y,X] = '@'; moveRobots();
end procedure
 
procedure teleport()
    board[Y,X] = ' '
    X = rand(W-2) + 1;
    Y = rand(H-2) + 1;
    if find(board[Y,X],"*+~") then
        alive = false
        board[Y,X] = 'A'
    else
        board[Y,X] = '@'
    end if
    moveRobots()
end procedure
 
procedure waitForEnd()
    while aliveRobots and alive do
        moveRobots()
        displayBoard()
        sleep(0.5)
    end while
end procedure
 
procedure getInput()
    while true do
        integer k = upper(wait_key())
        if    k='Q' and X > 2   and Y > 2   then execute(-1,-1) exit
        elsif k='W' and             Y > 2   then execute( 0,-1) exit
        elsif k='E' and X < W-1 and Y > 2   then execute( 1,-1) exit
        elsif k='A' and X > 2               then execute(-1, 0) exit
        elsif k='D' and X < W-1             then execute( 1, 0) exit
        elsif k='Y' and X > 2   and Y < H-1 then execute(-1, 1) exit
        elsif k='X'             and Y < H-1 then execute( 0, 1) exit
        elsif k='C' and X < W-1 and Y < H-1 then execute( 1, 1) exit
        elsif k='T'                         then teleport()     exit
        elsif k='Z'                         then waitForEnd()   exit
        elsif k='!'                         then alive = false  exit
        end if
    end while
    printScore()
end procedure
 
procedure play()
    clear_screen()
    while true do
        cursor(NO_CURSOR)
        robotsCount = 10; score = 0; alive = true; 
        clearBoard(); X = rand(W-2); Y = rand(H-2);
        board[Y,X] = '@'; createBoard();
        while alive do
            displayBoard(); getInput(); 
            if aliveRobots=0 then
                robotsCount += INC; clearBoard(); 
                board[Y,X] = '@'; createBoard(); 
            end if
        end while
        displayBoard()
        position(25, 1); bk_color(BLACK); text_color(WHITE)
        position( 8,10); puts(1,"+----------------------------------------+")
        position( 9,10); puts(1,"|               GAME OVER                |")
        position(10,10); puts(1,"|            PLAY AGAIN(Y/N)?            |")
        position(11,10); puts(1,"+----------------------------------------+")
        position(10,39); cursor(BLOCK_CURSOR);
        if upper(wait_key())!='Y' then exit end if
    end while
    clear_screen()
end procedure

play()
```

