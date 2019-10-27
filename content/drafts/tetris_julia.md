+++
title = "Tetris/Julia"
description = ""
date = 2018-11-08T02:56:10Z
aliases = []
[extra]
id = 22040
[taxonomies]
categories = []
tags = []
+++


## Julia

This is a fairly complete, configurable pure Julia implementation of Tetris, played in the standard Julia terminal. No external packages are used.

Detailed notes are in the long comment below the Julia script, to be attached to the code. At least its first few lines are needed, as they are shown as info, upon the request of the user.

{{works with|Julia|1.0}}


```julia
struct B4                         # Tetromino made of 4 Blocks
  bg::Int                         # ANSI code of tetromino color (bground of " ")
  d::Array{Int,2}                 # d[b,1:2] = row,col/2 offset of block b
                                  # constructor: set centered offsets of blocks
  B4(bg,A::Array{Int,2}) = new(bg,A.-sum.(extrema(A,dims=1)).>>1)
end
                                  # in loops ITERATE over the blocks in B4
Base.iterate(B::B4,i=1) = i<5 ? (B.d[i,:],i+1) : nothing
                                  # PRINT/CLEAR tetromino on terminal
drw(B,r,c)=(for(i,j)=B print("\e[$(B.bg)m\e[$(r+i);$(c+2j)H  ") end)
clr(B,r,c)=(for(i,j)=B print("\e[0m\e[$(r+i);$(c+2j)H  ") end)
                                  # HIT other B4s or border?
hit(B,r,c)=(for(i,j)=B try BD[r+i,c>>1+j]>0 catch;true end&& return true end;false)
                                  
function mov(B,r,c,x,y)           # MOVE tetrominos left/right/down, if no hit
  hit(B,r+x,c+y) || (clr(B,r,c); drw(B,r+=x,c+=y))
  TMv || (global tmr=Timer(tm)); return (r,c)
end            

function rot(B,r,c,rt=0)          # ROTATE tetrominos left/right, if no hit
  TMv || (global tmr=Timer(tm))
  A = B4(B.bg, rt>0 ? [B.d[:,2] (3 .-B.d[:,1])] : [(3 .-B.d[:,2]) B.d[:,1]])
  for j = c.+(0,+2,-2)            # try shifted positions
    hit(A,r,j) || (clr(B,r,c); drw(A,r,j); return (A,j))
  end; return (B,c)               # cannot rotate: all 3 positions hit
end

function mrk(B,r,c)               # RECORD place,CLEAR full-lines,DROP-above,score
  global lines,score,level;  n,l = 0,0
  for(i,j)=B BD[r+i,c>>1+j]=B.bg end  # record stuck B4
  for i in r.+sort(unique(B.d[:,1]))  # empty full rows in board; drop all above
    if all(BD[i,:].>0) n += 1; l = i  # l = lowest line changed
      for j=i-1:-1:1 BD[j+1,:]=BD[j,:] end
      print("\e[0m\e[$i;2H$s20"); sleep(0.3)
  end end                         # update changed display from board data ->
  for i=1:l,j=1:10 print("\e[$(BD[i,j])m\e[$i;$(2j)H  ") end
  score+=(level+1)*(0,100,300,500,800)[n+1]
  lines+=n; level=isqrt(lines>>3) # update lines, score, level
end

function cnf(p)                   # CONFIRMATION dialog. p = 1-line prompt
  print("$p - Enter confirms, other keys ignore")
  t = take!(chnl); println("\e[2K"); t==[0xd]
end
                                  # SHOW basic usage- and status info
inf()=print("\e[0m\e[26H\e[2K i: Info, l: Locked drop=$LDr, t: Timed move=$TMv")

                                  # SETUP nonblocking, non-echoed keyboard INPUT
ccall(:jl_tty_set_mode,Cint,(Ptr{Cvoid},Cint),stdin.handle,1)==0 || 
  throw("Terminal cannot enter raw mode.") # raw terminal mode to catch keystrokes
const chnl = Channel{Array{UInt8,1}}(0)    # unbuffered channel for key codes
@async while true put!(chnl,readavailable(stdin)) end  # task, catching keystrokes

I = B4(106,[0 0; 0 1; 0 2; 0 3])  # define the 7 tetrominos
T = B4(105,[0 1; 1 0; 1 1; 1 2]); O = B4(103,[0 0; 0 1; 1 0; 1 1])
S = B4(102,[0 1; 0 2; 1 0; 1 1]); Z = B4(101,[0 0; 0 1; 1 1; 1 2])
J = B4(104,[0 0; 1 0; 1 1; 1 2]); L = B4(107,[0 2; 1 0; 1 1; 1 2])
LDr,TMv,Ifo = falses(3)

begin @label RESTART              # @label - @goto: require begin..end
  global lines,score,level,s20,eq = 0,0,0," "^20,"="^6
  BD = fill(0,22,10)              # empty BOARD. Screen_Col = 2*Board_Col

  print("\e[0m\e[2J\e[?25l\e[1H") # Set default colors/clear screen/hide cursor
  print("▐$(s20)▌\n"^22*"▝$("▀"^20)▘\n\n$s20\n$s20") # screen BORDER, Cols=2:2:20

  X0 = (I,T,O,S,Z,J,L)[rand(1:7)]
  while true                      # random B4, timed drop, act on keystrokes
    global lines,score,level,tm,tmr,LDr,TMv,Ifo
    global X0,X,r,c = (I,T,O,S,Z,J,L)[rand(1:7)],X0,2,10
    clr(X,24,10); drw(X0,24,10); inf()       # show next piece, information
    print("\n Level =\t$level\n Lines filled =\t$lines\n Score =\t$score")
    tm=(.8-level*.007)^level; tmr=Timer(0.5) # 0.5s initial delay
    hit(X,r,c) && (while true cnf("\e[31H Game Over: RESTART")&&@goto RESTART end)
    while isready(chnl) take!(chnl) end      # flush queued keystrokes, max 1
    while true
      global X,r,c,tm,tmr
      if !isopen(tmr)             # time to drop tetromino by a line
        hit(X,r+1,c) && (mrk(X,r,c); break)
        clr(X,r,c); drw(X,r+=1,c); tmr=Timer(tm)
      end
      if isready(chnl)            # if there is a queued keystroke
        global X,r,c,score
        ch = take!(chnl)          # take keys
        if     ch==[0x1b,0x5b,0x41] X,c=rot(X,r,c)              # UP
        elseif ch==[0x1b,0x5b,0x42] r,c=mov(X,r,c,1, 0);score+=1# DOWN
        elseif ch==[0x1b,0x5b,0x43] r,c=mov(X,r,c,0, 2)         # RIGHT
        elseif ch==[0x1b,0x5b,0x44] r,c=mov(X,r,c,0,-2)         # LEFT
        elseif ch==[0x61]           X,c=rot(X,r,c)              # a
        elseif ch==[0x64]           X,c=rot(X,r,c,1)            # d
        elseif ch==[0x20] r0=r; clr(X,r,c); tmr=Timer(tm*!LDr)  # SPACE
          while !hit(X,r+=1,c) end; drw(X,r-=1,c); score+=2r-2r0; continue
        elseif ch==[0x1b,0x5b,0x31,0x7e]                        # HOME
          cnf("\e[0m\e[31H RESTART") && @goto RESTART
        elseif ch==[0x1b,0x5b,0x34,0x7e]                        # END
          cnf("\e[0m\e[31H EXIT") && return
        elseif ch==[0x6c] LDr=!LDr; inf()                       # l: Locked drop
        elseif ch==[0x74] TMv=!TMv; inf()                       # t: Timed move
        elseif ch==[0x69] if (Ifo=!Ifo) open(@__FILE__) do f    # i: long-Info
            readuntil(f,eq); println("\e[0m\e[32H$(readuntil(f,eq))") end
          else print("\e[0m\e[32H\e[J") # erase txt
      end end end
      sleep(0.01)                 # not to take all CPU time
end end end
```


The following long comment block has to be appended to program code:


```txt
#======       Julia-TETRIS by Laszlo Hars, Version 1.2 10/31/2018 
         (Set Terminal window to 43+ rows, don't scroll)
                          -- Key assignments --
  ↑: rotate +90° counterclockwise   i: Info about the program on/off
  ↓: soft drop (one line)           l: Locked hard drop on/off
  ←: shift left                     t: restart Timer after move on/off
  →: shift right                    SPACE: hard drop until hit
  a: rotate +90° counterclockwise   HOME: restart game
  d: rotate -90° clockwise          END:  exit
======
The game is played in the Julia standard Unicode character based Terminal
    Controlled by ANSI escape sequences (Windows conhost, ConEmu...)
    Set font to e.g. Fantasque Sans Mono
      Monospace, height:width = 2:1, line drawing chars

Playfield rows*cols: 10x20 active, +2 extra rows on top
Tetrominos appear centered in row 2 (and 1)

Rotate is around the center of the tetromino. It is not unique
    thus the rotated tetromino is also shifted left, then right.
    The first of the 3 positions, which does not hit anything is taken.
    
Naive gravity is used
    Full rows (lines) get cleared
    Rows above it move down, but floating blocks can remain

Information is displayed under the board
    PREVIEW next tetromino
    Game LEVEL
    Number of LINES cleared
    SCORE
    Keys for short Info, Locked-drop-, and Timed-move status

Game LEVEL: 0,1...= [sqrt(LINES/8)]
    Increase after 8,32,72,128,200... lines cleared

SCORE: Cleared_Line values: *(LEVEL+1)
    Single = 100
    Double = 300
    Triple = 500
    Tetris = 800

Bonus points: (not increasing with level)
    Soft drop = 1 point per line
    Hard drop = 2 points per line

Time_delay per drop one line, decreasing with LEVEL
    (0.8-LEVEL*0.007)^LEVEL (seconds)

Timing values
    Initial delay for new tetromino = 0.5s
    Cleared line is shown for 0.3s

Tetrominos are drawn by blocks of 2 space characters with color background
    Background color is set by ANSI codes, e.g print("\e[101m  ")
   bg-color_code  shape
Black   100 
                     ████
Red     101   Z        ████
                  ████
Green   102   S ████
                       ████
Yellow  103   O        ████
                ██
Blue    104   J ██████
                       ██
Magenta 105   T      ██████

Cyan    106   I ████████
                         ██
White   107   L      ██████

Julia specifics
    Terminal is switched to RAW mode to catch keystrokes.
    Non-blocking keyboard input is by asynchronous (maybe blocked) 2nd TASK.
    Keystrokes are transferred to the main program through a CHANNEL.
    STRUCTs (B4) describe Tetrominos, with color code and coordinates of blocks.
      At construction the coordinates get centralized.
    Assigning Struct to a variable (X) only creates a REFERENCE, not copied.
    Memory used by orphan instance is cleared by internal GARBAGE COLLECTION.
    Out of range indices are caught with TRY..CATCH, to keep tetrominos in board.
    A TIMER controls the dropping delay of tetrominos.
    ITERATE is defined for tetrominos, looping over the coordinates of blocks.
    In loops when variables get assigned before other use, Julia 1.0 assumes
      local variables, therefore many GLOBAL variables are declared and used
    For simplicity Int variables are used; in 64-bit OS versions: 8 Bytes.
      In most places 1 Byte Int8 (or Int16, Int32) would work. Take care
        when changing: Int8(x)+1->Int64, Int8(x)+0x1->UInt8. Use +Int8(1).
      Only "lines" and "score" need to be UInt32 or longer.
    LABEL - GOTO (for restart) require the main loop enclosed in Begin..End.
    Several instructions are written in single lines of at most 84 chars,
      to keep the program well under 100 non-comment/non-blank lines!
    #.. Comments denote tasks, explain important points.
    In strings some UNICODE characters appear. They can be replaced with
      \u{hex_digits} to make the program all ASCII (for old editors).
======#
```

