+++
title = "Langton's ant"
description = ""
date = 2019-08-17T15:35:48Z
aliases = []
[extra]
id = 10740
[taxonomies]
categories = []
tags = []
+++

{{task|Cellular automata}}

[[wp:Langton's ant|Langton's ant]] is a cellular automaton that models an [https://en.wikipedia.org/wiki/Ant ant] sitting on a plane of cells, all of which are white initially, the ant facing in one of four directions. 

Each cell can either be black or white. 

The ant moves according to the color of the cell it is currently sitting in, with the following rules:
::#   If the cell is black, it changes to white and the ant turns left;
::#   If the cell is white, it changes to black and the ant turns right;
::#   The ant then moves forward to the next cell, and repeat from step 1.


This rather simple ruleset leads to an initially chaotic movement pattern, and after about 10000 steps, a cycle appears where the ant moves steadily away from the starting location in a diagonal corridor about 10 cells wide.  
Conceptually the ant can then walk infinitely far away.


;Task:
Start the ant near the center of a 100<small>x</small>100 field of cells, which is about big enough to contain the initial chaotic part of the movement.
  
Follow the movement rules for the ant, terminate when it moves out of the region, and show the cell colors it leaves behind.


The problem has received some analysis; for more details, please take a look at the Wikipedia article   (a link is below)..


;See also:
*   Wikipedia:   [https://en.wikipedia.org/wiki/Langton%27s_ant Langton's ant].


;Related task:
*   Rosetta Code:   [[Conway's Game of Life]].





## Ada


```Ada
with Ada.Text_IO;

procedure Langtons_Ant is

   Size: constant Positive := 100; -- change this to extend the playground

   subtype Step is Integer range -1 .. +1;

   procedure Right(N, W: in out Step) is
      Tmp: Step := W;
   begin
      W := - N;
      N := Tmp;
   end Right;

   procedure Left(N, W: in out Step) is
   begin
      for I in 1 .. 3 loop
         Right(N, W);
      end loop;
   end Left;

   Color_Character: array(Boolean) of Character :=
     (False => ' ', True => '#');

   Is_Black: array (1 .. Size, 1 .. Size) of Boolean :=
     (others => (others => False)); -- initially, the world is white;

   Ant_X, Ant_Y: Natural := Size/2; -- Position of Ant;
   Ant_North: Step := 1; Ant_West: Step := 0; -- initially, Ant looks northward

   Iteration: Positive := 1;

begin
   loop -- iterate the loop until an exception is raised
      if Is_Black(Ant_X, Ant_Y) then
         Left(Ant_North, Ant_West);
      else
         Right(Ant_North, Ant_West);
      end if;
      Is_Black(Ant_X, Ant_Y) := not Is_Black(Ant_X, Ant_Y);
      Ant_X := Ant_X - Ant_North; -- this may raise an exception
      Ant_Y := Ant_Y - Ant_West;  -- this may raise an exception
      Iteration := Iteration + 1;
    end loop;

exception
   when Constraint_Error => -- Ant has left its playground ... now output
      for X in 1 .. Size loop
         for Y in 1 .. Size loop
            Ada.Text_IO.Put(Color_Character(Is_Black(X, Y)));
         end loop;
         Ada.Text_IO.New_Line;
      end loop;
      Ada.Text_IO.Put_Line("# Iteration:" & Integer'Image(Iteration));
end Langtons_Ant;

```

Ouptut (to save space, I have removed the all-blank lines):
<pre style="height:30ex;overflow:scroll">                                         ##  ############  ##                                       
                                        #  ####          #  ##                                      
                                       ###   ##            ## #                                     
                                       # #  #         #  #    #                                     
                                   ##  ## # #         ###       #                                   
                                ### #  #   #     #     ## ##  ###                                   
                                 # #  ###  ## #### ##   # #  # ##  ##                               
                                 # ### ##  # ##  ### # #     ###   ###                              
                               #     #   ##### # #  ####  #   ### # # #                             
                              ### ##   # ####  ## ## ###### # ### #   #                             
                              # ### # ## # # ## ## ## #   ##### ### ##                              
                                  # #   # ## ###   #   # #  ####    # ##                            
                               #  #         ## ##   #  ##     ## #     ##                           
                              ###   # # ## ###  #  ##     #   ### ##  ## #                          
                             #  ###  ##   ## ##   ###  #    #  ## ####   #                          
                            ###   #   # #  # # #### ##  # ## ###  #     #                           
                           #  ###  # ##    #  # ###  #      ### ## #  #  ##                         
                          ###   #     # ## # ##  ##  ##### ####  #### ##   #                        
                         #  ###  # # #  # ### # # ##      ##   # # #    #   #                       
                        ###   #  ## ###  ## #   ##       #### ####   #      #                       
                       #  ###  # #  #   ##  ########### #  ####  #    #    #                        
                      ###   #  ##      # ####  ##  #########  #  ##    #  ##                        
                     #  ###  # #   ##  # ##   ## ## ### ###   #  # ##  #### #                       
                    ###   #  ##   #  # ###### ## # ## # #    ### ###   ##   #                       
                   #  ###  # #   #     ##### # #####     # #  ## #    ##   #                        
                  ###   #  ##    #     # ## ##### ##  # #   #  #  ## #  #  #                        
                 #  ###  # #     #    #   #### #  ##### ##   ##########   ##                        
                ###   #  ##      # ##   ##   #  #   ####  #   ## #### ##                            
               #  ###  # #        ##### #  ##   ## #   #    # #  #  #  # #                          
              ###   #  ##          ##  ## # # #    ## ## # # ##  #  ##  ##                          
             #  ###  # #                 #  #    # ######## # # ##  #### #                          
            ###   #  ##                  #  #   #       ## ##   #  #  ## #                          
           #  ###  # #                    #  #  #      #  ##  ##   ## ####                          
          ###   #  ##                      ##   #       ##  ##    #   # ###                         
         #  ###  # #                            # ##  ####    #### ### ####                         
        ###   #  ##                              ##  ####    ##  # ## # #  #                        
       #  ###  # #                                ##    ##    ## ### ## #####                       
      ###   #  ##                                                # ## #  ####                       
     #  ###  # #                                                     ## ## ##                       
    ###   #  ##                                                      ##                             
   #  ###  # #                                                     # ##  #### #                     
  ###   #  ##                                                     #  # ###  ###                     
 #  ###  # #                                                      # ## #  #  #                      
###   #  ##                                                        ##      ##                       
   ##  # #                                                          ##                              
##  #  ##                                                                                           
 # # # #                                                                                            
#### ##                                                                                             
# ## #                                                                                              
 ####                                                                                               
  ##                                                                                                
# Iteration: 11656

```



## Aime

[[File:ant_phpoFTAAk.png|100px|Output png]]

```aime
void
ant(integer x, y, d, list map)
{
    while (-1 < x && x < 100 && -1 < y && y < 100) {
        integer e, p, w;
        data b;

        b = map[y];
        w = b[x >> 3];
        p = 1 << (7 - (x & 7));
        b[x >> 3] = w ^ p;

        d += w & p ? 1 : 3;

        e = d & 1;
        set(e, $e + ((d & 2) - 1) * (2 * e - 1));
    }
}

integer
main(void)
{
    file f;
    list l;

    call_n(100, lb_p_data, l, data().run(13, 0));
    ant(50, 50, 2, l);

    f.create("ant.pbm", 00644).text("P4\n100 100\n");
    l.ucall(f_data, 1, f);

    0;
}
```



## ALGOL 68


```algol68
BEGIN
    # size of board for Langton's ant #
    INT max board = 100;
    [ 1 : max board, 1 : max board ]CHAR board;
    # start with the board all white #
    CHAR white = " ", black = "#";
    FOR r TO 1 UPB board DO FOR c TO 2 UPB board DO board[ r, c ] := white OD OD;
    # possible ant directions #
    INT head left = 0, head up = 1, head right = 2, head down = 3;
    # returns the new direction if we turn left from curr direction #
    OP LEFT = ( INT curr direction )INT:
       IF   curr direction = head left  THEN head down
       ELIF curr direction = head down  THEN head right
       ELIF curr direction = head right THEN head up
       ELSE                                  head left
       FI ; # LEFT #
    # returns the new direction if we turn right from curr direction #
    OP RIGHT = ( INT curr direction )INT:
       IF   curr direction = head left  THEN head up
       ELIF curr direction = head up    THEN head right
       ELIF curr direction = head right THEN head down
       ELSE                                  head left
       FI ; # RIGHT #
    # move the ant until it leaves the board #
    INT ant row := max board OVER 2;
    INT ant col := max board OVER 2;
    INT ant direction := head up;
    INT max row := 1;
    INT max col := 1;
    INT min row := max board;
    INT min col := max board;
    INT moves := 0;
    WHILE ant row >= 1 LWB board AND ant row <= 1 UPB board
      AND ant col >= 2 LWB board AND ant col <= 2 UPB board
    DO
        moves +:= 1;
        IF ant row > max row THEN max row := ant row FI;
        IF ant col > max col THEN max col := ant col FI;
        IF ant row < min row THEN min row := ant row FI;
        IF ant col < min col THEN min col := ant col FI;
        IF board[ ant row, ant col ] = white THEN
            # ant turns right on a white square #
            ant direction := RIGHT ant direction;
            board[ ant row, ant col ] := black
        ELSE
            # ant turns left on a black square #
            ant direction :=  LEFT ant direction;
            board[ ant row, ant col ] := white
        FI;
        # move the ant #
        IF     ant direction = head up    THEN ant row -:= 1
        ELIF   ant direction = head down  THEN ant row +:= 1
        ELIF   ant direction = head left  THEN ant col -:= 1
        ELSE # ant direction = head right #    ant col +:= 1
        FI
    OD;
    # show resultant position #
    print( ( "After ", whole( moves, 0 ), " moves."
           , " Showing rows ", whole( min row,0 ), " to ", whole( max row, 0 )
           , " columns ", whole( min col,0 ), " to ", whole( max col, 0 )
           , newline
           )
         );
    FOR r FROM min row TO max row DO
        print( ( board[ r, min col : max col ], newline ) )
    OD
END
```

{{out}}

```txt

After 11655 moves. Showing rows 28 to 78 columns 1 to 79
                                         ##  ############  ##                  
                                        #  ####          #  ##                 
                                       ###   ##            ## #                
                                       # #  #         #  #    #                
                                   ##  ## # #         ###       #              
                                ### #  #   #     #     ## ##  ###              
                                 # #  ###  ## #### ##   # #  # ##  ##          
                                 # ### ##  # ##  ### # #     ###   ###         
                               #     #   ##### # #  ####  #   ### # # #        
                              ### ##   # ####  ## ## ###### # ### #   #        
                              # ### # ## # # ## ## ## #   ##### ### ##         
                                  # #   # ## ###   #   # #  ####    # ##       
                               #  #         ## ##   #  ##     ## #     ##      
                              ###   # # ## ###  #  ##     #   ### ##  ## #     
                             #  ###  ##   ## ##   ###  #    #  ## ####   #     
                            ###   #   # #  # # #### ##  # ## ###  #     #      
                           #  ###  # ##    #  # ###  #      ### ## #  #  ##    
                          ###   #     # ## # ##  ##  ##### ####  #### ##   #   
                         #  ###  # # #  # ### # # ##      ##   # # #    #   #  
                        ###   #  ## ###  ## #   ##       #### ####   #      #  
                       #  ###  # #  #   ##  ########### #  ####  #    #    #   
                      ###   #  ##      # ####  ##  #########  #  ##    #  ##   
                     #  ###  # #   ##  # ##   ## ## ### ###   #  # ##  #### #  
                    ###   #  ##   #  # ###### ## # ## # #    ### ###   ##   #  
                   #  ###  # #   #     ##### # #####     # #  ## #    ##   #   
                  ###   #  ##    #     # ## ##### ##  # #   #  #  ## #  #  #   
                 #  ###  # #     #    #   #### #  ##### ##   ##########   ##   
                ###   #  ##      # ##   ##   #  #   ####  #   ## #### ##       
               #  ###  # #        ##### #  ##   ## #   #    # #  #  #  # #     
              ###   #  ##          ##  ## # # #    ## ## # # ##  #  ##  ##     
             #  ###  # #                 #  #    # ######## # # ##  #### #     
            ###   #  ##                  #  #   #       ## ##   #  #  ## #     
           #  ###  # #                    #  #  #      #  ##  ##   ## ####     
          ###   #  ##                      ##   #       ##  ##    #   # ###    
         #  ###  # #                            # ##  ####    #### ### ####    
        ###   #  ##                              ##  ####    ##  # ## # #  #   
       #  ###  # #                                ##    ##    ## ### ## #####  
      ###   #  ##                                                # ## #  ####  
     #  ###  # #                                                     ## ## ##  
    ###   #  ##                                                      ##        
   #  ###  # #                                                     # ##  #### #
  ###   #  ##                                                     #  # ###  ###
 #  ###  # #                                                      # ## #  #  # 
###   #  ##                                                        ##      ##  
   ##  # #                                                          ##         
##  #  ##                                                                      
 # # # #                                                                       
#### ##                                                                        
# ## #                                                                         
 ####                                                                          
  ##                                                                           

```



## AutoHotkey

ahk forum: [http://ahkscript.org/boards/viewtopic.php?f=17&t=1363 discussion]
{{works with|AutoHotkey 1.1}} (Fixed by just me)

```AutoHotkey
#NoEnv
SetBatchLines, -1
; Directions
Directions := {0: "North", 1: "East", 2: "South", 3: "West"}
; Initialize the plane (set all cells to white)
White := 0xFFFFFF
Plane := []
PW := PH := 100
loop, % PH {
    I := A_Index
    loop, % PW
        Plane[I, A_Index] := White
}
; Let it run
DI := D := 0 ; initial direction
X := Y := 50 ; initial coordinates
while (X > 0) && (X <= PW) && (Y > 0) && (Y <= PH) {
    D := (D + ((Plane[X, Y] ^= White) ? 1 : 3)) & 3
    if (D & 1)
        X += -(D = 3) + (D = 1)
    else
        Y += -(D = 0) + (D = 2)
}
; Show the result
HBM := CreateDIB(Plane, PW, PH, 400, 400, 0)
Gui, Margin, 0, 0
Gui, Add, Text, x0 y0 w20 h440 Center 0x200, W
Gui, Add, Text, x20 y0 w400 h20 Center 0x200, N
Gui, Add, Picture, x20 y20 w400 h400 0x4E hwndHPIC ; SS_REALSIZECONTROL = 0x40 | SS_BITMAP = 0xE
DllCall("User32.dll\SendMessage", "Ptr", HPIC, "UInt", 0x172, "Ptr", 0, "Ptr", HBM) ; STM_SETIMAGE = 0x172
Gui, Add, Text, xp+5 yp h20 0x200 BackgroundTrans, % "Initial direction: " . Directions[DI]
Gui, Add, Text, x20 y420 w400 h20 Center 0x200, S
Gui, Add, Text, x420 y0 w20 h440 Center 0x200, E
Gui, Show, , Langton's ant (%PW%x%PH%)
Return

GuiClose:
ExitApp

CreateDIB(PixelArray, PAW, PAH, BMW := 0, BMH := 0, Gradient := 1) { ; SKAN, 01-Apr-2014 / array version by just me
    SLL := (PAW * 3) + (PAW & 1)
    VarSetCapacity(BMBITS, SLL * PAH, 0)
    P := &BMBITS
    loop, % PAH {
        R := A_Index
        loop, % PAW
            P := Numput(PixelArray[R, A_Index], P + 0, "UInt") - 1
        P += (PAW & 1)
    }
    HBM := DllCall("Gdi32.dll\CreateBitmap", "Int", PAW, "Int", PAH, "UInt", 1, "UInt", 24, "Ptr", 0, "UPtr")
    HBM := DllCall("User32.dll\CopyImage", "Ptr", HBM, "UInt", 0, "Int", 0, "Int", 0, "UInt", 0x2008, "UPtr")
    DllCall( "Gdi32.dll\SetBitmapBits", "Ptr", HBM, "UInt", SLL * PAH, "Ptr", &BMBITS)
    if (!Gradient)
        HBM := DllCall("User32.dll\CopyImage", "Ptr", HBM, "UInt", 0, "Int", 0, "Int", 0, "Int", 8, "UPtr")
    return DllCall("User32.dll\CopyImage", "Ptr", HBM, "UInt", 0, "Int", BMW, "Int", BMH, "UInt", 0x200C, "UPtr")
} ; http://ahkscript.org/boards/viewtopic.php?f=6&t=3203
```



## AutoIt


```AutoIt

Global $iCountMax = 100000
Global $aFields[100][100][2]
Global $iDelayStep = 10  ; stop between steps in msec

Global $aDirection[4][4] = [ _ ; [ direction 0-3 ][ left change x, y, right change x, y ]
[-1,  0, +1,  0], _   ; == direction 0
[ 0, -1,  0, +1], _   ; == direction 1
[+1,  0, -1,  0], _   ; == direction 2
[ 0, +1,  0, -1]]     ; == direction 3

Global $hGui = GUICreate("Langton's ant", 100*8, 100*8)
GUISetBkColor(0xFFFFFF)

For $i = 0 To 99
	For $j = 0 To 99
		$aFields[$i][$j][0] = GUICtrlCreateLabel('', $j*8, $i*8)
		GUICtrlSetColor(-1, 0xFF0000)
		$aFields[$i][$j][1] = 0
	Next
Next

GUISetState()

GUICtrlSetData($aFields[49][49][0], '#')

Do
	Sleep($iDelayStep)
Until Not _SetAnt()

Do
Until GUIGetMsg() = -3


Func _SetAnt()
	Local Static $iRowLast = 49, $iColLast = 49, $iCount = 0
	Local Static $aCol[2] = [0xFFFFFF,0x000000], $iDirection = 0
	Local $iRow, $iCol, $fRight = False
	If $iCount = $iCountMax Then Return 0

	; == get current color
	Local $iLastColor = $aFields[$iRowLast][$iColLast][1]

	; == go to left/right
	If $iLastColor = 0 Then $fRight = True

	; == set the ant to the next field
	Local $indexX = 0, $indexY = 1
	If $fRight Then
		$indexX = 2
		$indexY = 3
	EndIf
	$iRow = $iRowLast + ($aDirection[$iDirection][$indexX])
	$iCol = $iColLast + ($aDirection[$iDirection][$indexY])
	If $iRow < 0 Or $iRow > 99 Or $iCol < 0 Or $iCol > 99 Then Return 0
	GUICtrlSetData($aFields[$iRowLast][$iColLast][0], '')
	GUICtrlSetData($aFields[$iRow][$iCol][0], '#')

	; == direction for next step
	If $fRight Then
		$iDirection += 1
		If $iDirection = 4 Then $iDirection = 0
	Else
		$iDirection -= 1
		If $iDirection = -1 Then $iDirection = 3
	EndIf

	; == change the color of the current field
	GUICtrlSetBkColor($aFields[$iRowLast][$iColLast][0], $aCol[(Not $iLastColor)*1])
	$aFields[$iRowLast][$iColLast][1] = (Not $iLastColor)*1

	$iRowLast = $iRow
	$iColLast = $iCol
	$iCount += 1
	WinSetTitle($hGui, '', "Langton's ant      [ step: " & StringFormat('%06d', $iCount) & " ]")
	Return 1
EndFunc  ;==>_SetAnt

```

[http://www.imgbox.de/users/BugFix/langtons_ant.png To see the GUI output, click here.]
--[[User:BugFix|BugFix]] ([[User talk:BugFix|talk]]) 14:48, 16 November 2013 (UTC)


## AWK


```awk

# usage: awk  -v debug=0  -f langton.awk

# Simulates the cellular automaton "Langton's ant",
# see http://en.wikipedia.org/wiki/Langton%27s_ant

function turnRight() {
	dir++
	if( dir>4 ) { dir=1 }
}
function turnLeft() {
	dir--
	if( dir<1 ) { dir=4 }
}
function move() {
	if (dir==1) { y--; z="^" }
	if (dir==3) { y++; z="v" }

	if (dir==2) { x++; z=">" }
	if (dir==4) { x--; z="<" }
}

function ant() {
	if( debug )  AntStat() 				##

	if( grid[x,y]==0 ) { turnLeft() } else { turnRight() }
	if( grid[x,y]==0 ) { color=1    } else { color=0 }

	if( debug )  print( "# action", color, dir, z )	##

	grid[x,y] = color
	move()
}

###

function AntStat() {
	printf( "Move# %d : Ant @ x=%d y=%d dir=%d %s  color=%d\n",
		 moveNr, x,y, dir,z, grid[x,y] )
}
function dumpGrid() {
	AntStat()

	printf( "Grid:" )
	for(xx=1; xx<=limit/10; xx++) {
		printf( "....+....%s", xx )
	}
	printf "\n"

	cSum=0
	for(yy=1; yy <= limit; yy++) {
		printf( "%4d:",yy )
		for(xx=1; xx <= limit; xx++) {
			c = grid[xx,yy]
			if( c ) cSum++
	c1++
	c2+=grid[xx,yy]
			if( (xx==x)&&(yy==y) ) 	{ c=z } 	# Ant
			printf( c )
		}
		printf( "\n" )
	}
	printf( "Cells: %d  'black' cells: %d  Moves: %d\n\n", limit*limit, cSum, moveNr )
}

BEGIN { 
	  print( "Langton's ant\n" ) 

	  limit  = 72
	  for(x=1; x <= limit; x++) {
		for(y=1; y <= limit; y++) {
			grid[x,y] = 0
		}
	  }

	  moveNr =   0
	  x      =  36
	  y      =  28
	  dir    =   1	# 1=up/north 2=right/east 3=down/south 4=left/west
	  z      = "!"

	  while( moveNr < 11200 ) { 
		moveNr++
 		ant() 
		if(x<0 || x>limit) break
		if(y<0 || y>limit) break

		# Snapshots:
		if (moveNr==163 || moveNr==1297 || moveNr==10095 ) dumpGrid()
		if (y<=5 ) break
	  }
	  dumpGrid()
}
END	{ print("END.") }

```



## BBC BASIC


```BBC BASIC

      REM Implementation of Langton's ant for Rosetta Code
      fieldsize%=100
      REM Being pedantic, this will actually result in a field of 101 square,
      REM since arrays start at 0, and my implementation allows them to use it
      DIM field&(fieldsize%,fieldsize%)   : REM variables with an & suffix are byte variables
      x%=fieldsize%/2
      y%=fieldsize%/2
      d%=0
      REPEAT
        IF field&(x%,y%)=0 THEN field&(x%,y%)=1:d%-=1 ELSE field&(x%,y%)=0:d%+=1
        GCOL 15*field&(x%,y%)
        PLOT 69,x%*2,y%*2     :REM for historical reasons there are two "plot points" per pixel
        d%=(d%+4) MOD 4       :REM ensure direction is always between 0 and 3
        CASE d% OF
          WHEN 0:y%+=1
          WHEN 1:x%+=1
          WHEN 2:y%-=1
          WHEN 3:x%-=1
        ENDCASE
      UNTIL x%>fieldsize% OR x%<0 OR y%>fieldsize% OR y%<0
      END

```



## bc

The output function <code>o</code> prints the resulting image (as a [[wp:Netpbm_format|PBM image]]) to <code>stdout</code>. One can either store it into a file or pipe it through an image viewer (e.g. <code>bc langton.bc | display</code>).


```bc
define o() {
    auto i, j
    
    "P1 "
    w
    h
    for (j = 0; j < h; j++) {
        for (i = 0; i < w; i++) {
            a[j * w + i]
        }
    }
}
   
define l(w, h, x, y) {
    auto a[], d, i, x[], y[]

    /* d represents one of the four possible directions:
     *             0
     *             ⇑
     *           3⇐ ⇒1
     *             ⇓
     *             2
     * The arrays x[] and y[] contain the changes to the x and y direction for 
     * each value of d.
     */
    x[1] = 1
    x[3] = -1
    y[0] = -1
    y[2] = 1

    while (1) {
        i = y * w + x
        if (a[i] == 0) d += 1   /* turn right if white */
        if (a[i] == 1) d -= 1   /* turn left if black */
        if (d < 0) d = 3
        if (d > 3) d = 0
        x += x[d]
        y += y[d]
        a[i] = 1 - a[i]         /* toggle cell colour */
        if (x < 0) break
        if (x == w) break
        if (y < 0) break
        if (y == h) break
    }

    o()
}

l(100, 100, 50, 50)
quit
```



## Befunge



```befunge
"22222 -"*>>>1-:0\:"P"%\v>\7%1g48*-/2%3*48*+,1+:20g`!v1g01+55p03:_$$$>@
!"$(0@`vp00_^#!:p+7/"P"<<^g+7/*5"p"\%"P"/7::+g03*"d":_$,1+>:40g`!^1g03<
_::10g\v>00g+4%:00p::3\`\1-*50g+50p:2\-\0`*+::0\`\"c"`+50g:0\`\"c"`++#^
-*84g1<v^+1*2g09pg08g07-*g06-1*2p09:%2/g06:gp08:+7/*5"p"\p07:%"P"/7:p06
0p+:7%^>>-:0`!*+10p::20g\-:0`*+20p:"d"*50g::30g\-:0`!*+30p::40g\-:0`*+4
```


{{out}}

<pre style="font-size: 4px">                                          ##  ############  ##
                                         #  ####          #  ##
                                        ###   ##            ## #
                                        # #  #         #  #    #
                                    ##  ## # #         ###       #
                                 ### #  #   #     #     ## ##  ###
                                  # #  ###  ## #### ##   # #  # ##  ##
                                  # ### ##  # ##  ### # #     ###   ###
                                #     #   ##### # #  ####  #   ### # # #
                               ### ##   # ####  ## ## ###### # ### #   #
                               # ### # ## # # ## ## ## #   ##### ### ##
                                   # #   # ## ###   #   # #  ####    # ##
                                #  #         ## ##   #  ##     ## #     ##
                               ###   # # ## ###  #  ##     #   ### ##  ## #
                              #  ###  ##   ## ##   ###  #    #  ## ####   #
                             ###   #   # #  # # #### ##  # ## ###  #     #
                            #  ###  # ##    #  # ###  #      ### ## #  #  ##
                           ###   #     # ## # ##  ##  ##### ####  #### ##   #
                          #  ###  # # #  # ### # # ##      ##   # # #    #   #
                         ###   #  ## ###  ## #   ##       #### ####   #      #
                        #  ###  # #  #   ##  ########### #  ####  #    #    #
                       ###   #  ##      # ####  ##  #########  #  ##    #  ##
                      #  ###  # #   ##  # ##   ## ## ### ###   #  # ##  #### #
                     ###   #  ##   #  # ###### ## # ## # #    ### ###   ##   #
                    #  ###  # #   #     ##### # #####     # #  ## #    ##   #
                   ###   #  ##    #     # ## ##### ##  # #   #  #  ## #  #  #
                  #  ###  # #     #    #   #### #  ##### ##   ##########   ##
                 ###   #  ##      # ##   ##   #  #   ####  #   ## #### ##
                #  ###  # #        ##### #  ##   ## #   #    # #  #  #  # #
               ###   #  ##          ##  ## # # #    ## ## # # ##  #  ##  ##
              #  ###  # #                 #  #    # ######## # # ##  #### #
             ###   #  ##                  #  #   #       ## ##   #  #  ## #
            #  ###  # #                    #  #  #      #  ##  ##   ## ####
           ###   #  ##                      ##   #       ##  ##    #   # ###
          #  ###  # #                            # ##  ####    #### ### ####
         ###   #  ##                              ##  ####    ##  # ## # #  #
        #  ###  # #                                ##    ##    ## ### ## #####
       ###   #  ##                                                # ## #  ####
      #  ###  # #                                                     ## ## ##
     ###   #  ##                                                      ##
    #  ###  # #                                                     # ##  #### #
   ###   #  ##                                                     #  # ###  ###
  #  ###  # #                                                      # ## #  #  #
 ###   #  ##                                                        ##      ##
#  ###  # #                                                          ##
 ### #  ##
# # # # #
 #### ##
 # ## #
  ####
   ##
```



## C

Requires ANSI terminal.

```c>#include <stdio.h

#include <stdlib.h>
#include <string.h>
#include <unistd.h>

int w = 0, h = 0;
unsigned char *pix;

void refresh(int x, int y)
{
	int i, j, k;
	printf("\033[H");
	for (i = k = 0; i < h; putchar('\n'), i++)
		for (j = 0; j < w; j++, k++)
			putchar(pix[k] ? '#' : ' ');
}

void walk()
{
	int dx = 0, dy = 1, i, k;
	int x = w / 2, y = h / 2;

	pix = calloc(1, w * h);
	printf("\033[H\033[J");

	while (1) {
		i = (y * w + x);
		if (pix[i]) k = dx, dx = -dy, dy = k;
		else	    k = dy, dy = -dx, dx = k;

		pix[i] = !pix[i];
		printf("\033[%d;%dH%c", y + 1, x + 1, pix[i] ? '#' : ' ');

		x += dx, y += dy;

		k = 0;
		if (x < 0) {
			memmove(pix + 1, pix, w * h - 1);
			for (i = 0; i < w * h; i += w) pix[i] = 0;
			x++, k = 1;
		}
		else if (x >= w) {
			memmove(pix, pix + 1, w * h - 1);
			for (i = w-1; i < w * h; i += w) pix[i] = 0;
			x--, k = 1;
		}

		if (y >= h) {
			memmove(pix, pix + w, w * (h - 1));
			memset(pix + w * (h - 1), 0, w);
			y--, k = 1;
		}
		else if (y < 0) {
			memmove(pix + w, pix, w * (h - 1));
			memset(pix, 0, w);
			y++, k = 1;
		}
		if (k) refresh(x, y);
		printf("\033[%d;%dH\033[31m@\033[m", y + 1, x + 1);

		fflush(stdout);
		usleep(10000);
	}
}

int main(int c, char **v)
{
	if (c > 1) w = atoi(v[1]);
	if (c > 2) h = atoi(v[2]);
	if (w < 40) w = 40;
	if (h < 25) h = 25;

	walk();
	return 0;
}
```



## C++

[[File:langtonsAnt_cpp.png|300px]]

If you want to see it running infinitely, set the const bool INFINIT_RUN = true

```cpp

#include <windows.h>
#include <string>

//--------------------------------------------------------------------------------------------------
using namespace std;

//--------------------------------------------------------------------------------------------------
const int BMP_SIZE = 600, CELL_SIZE = 4, GRID_SIZE = BMP_SIZE / CELL_SIZE;
const bool INFINIT_RUN = false;

enum cellState { WHITE, BLACK, ANT };
enum facing { NOR, EAS, SOU, WES };
enum state { RUNNING, RESTING };

//--------------------------------------------------------------------------------------------------
class myBitmap
{
public:
    myBitmap() : pen( NULL ) {}
    ~myBitmap()
    {
	DeleteObject( pen );
	DeleteDC( hdc );
	DeleteObject( bmp );
    }

    bool create( int w, int h )
    {
	BITMAPINFO	bi;
	ZeroMemory( &bi, sizeof( bi ) );

	bi.bmiHeader.biSize	   = sizeof( bi.bmiHeader );
	bi.bmiHeader.biBitCount	   = sizeof( DWORD ) * 8;
	bi.bmiHeader.biCompression = BI_RGB;
	bi.bmiHeader.biPlanes	   = 1;
	bi.bmiHeader.biWidth	   =  w;
	bi.bmiHeader.biHeight	   = -h;

	HDC dc = GetDC( GetConsoleWindow() );
	bmp = CreateDIBSection( dc, &bi, DIB_RGB_COLORS, &pBits, NULL, 0 );
	if( !bmp ) return false;

	hdc = CreateCompatibleDC( dc );
	SelectObject( hdc, bmp );
	ReleaseDC( GetConsoleWindow(), dc ); 

	width = w; height = h;

	return true;
    }

    void clear()
    {
	ZeroMemory( pBits, width * height * sizeof( DWORD ) );
    }

    void setPenColor( DWORD clr )
    {
	if( pen ) DeleteObject( pen );
	pen = CreatePen( PS_SOLID, 1, clr );
	SelectObject( hdc, pen );
    }

    void saveBitmap( string path )
    {
	BITMAPFILEHEADER fileheader;
	BITMAPINFO	 infoheader;
	BITMAP		 bitmap;
	DWORD		 wb;

	GetObject( bmp, sizeof( bitmap ), &bitmap );

	DWORD* dwpBits = new DWORD[bitmap.bmWidth * bitmap.bmHeight];
	ZeroMemory( dwpBits, bitmap.bmWidth * bitmap.bmHeight * sizeof( DWORD ) );
	ZeroMemory( &infoheader, sizeof( BITMAPINFO ) );
	ZeroMemory( &fileheader, sizeof( BITMAPFILEHEADER ) );

	infoheader.bmiHeader.biBitCount = sizeof( DWORD ) * 8;
	infoheader.bmiHeader.biCompression = BI_RGB;
	infoheader.bmiHeader.biPlanes = 1;
	infoheader.bmiHeader.biSize = sizeof( infoheader.bmiHeader );
	infoheader.bmiHeader.biHeight = bitmap.bmHeight;
	infoheader.bmiHeader.biWidth = bitmap.bmWidth;
	infoheader.bmiHeader.biSizeImage = bitmap.bmWidth * bitmap.bmHeight * sizeof( DWORD );

	fileheader.bfType    = 0x4D42;
	fileheader.bfOffBits = sizeof( infoheader.bmiHeader ) + sizeof( BITMAPFILEHEADER );
	fileheader.bfSize    = fileheader.bfOffBits + infoheader.bmiHeader.biSizeImage;

	GetDIBits( hdc, bmp, 0, height, ( LPVOID )dwpBits, &infoheader, DIB_RGB_COLORS );

	HANDLE file = CreateFile( path.c_str(), GENERIC_WRITE, 0, NULL, CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, NULL );
	WriteFile( file, &fileheader, sizeof( BITMAPFILEHEADER ), &wb, NULL );
	WriteFile( file, &infoheader.bmiHeader, sizeof( infoheader.bmiHeader ), &wb, NULL );
	WriteFile( file, dwpBits, bitmap.bmWidth * bitmap.bmHeight * 4, &wb, NULL );
	CloseHandle( file );

	delete [] dwpBits;
    }

    HDC getDC() const     { return hdc; }
    int getWidth() const  { return width; }
    int getHeight() const { return height; }

private:
    HBITMAP bmp;
    HDC	    hdc;
    HPEN    pen;
    void   *pBits;
    int	    width, height;
};
//--------------------------------------------------------------------------------------------------
class Ant
{
public:
    Ant() 
    {
	_bmp.create( BMP_SIZE, BMP_SIZE );
	ZeroMemory( _grid, sizeof( _grid ) );
	RED_BRUSH = CreateSolidBrush( 255 );
	_antState = RUNNING;
    }

    ~Ant()
    {
	DeleteObject( RED_BRUSH );
    }

    void setPosition( int x, int y )
    {
	_sx = x; _sy = y;
	_facing = WES;
    }

    void mainLoop()
    {
	switch( _antState )
	{
	    case RUNNING:
	        simulate();
		// fall thru
	    case RESTING:
		display();
	}
    }

    void setHWND( HWND hwnd ) { _hwnd = hwnd; }

private:
    void simulate()
    {
	switch( _grid[_sx][_sy] )
	{
	    case BLACK:
		_grid[_sx][_sy] = WHITE;
		if( --_facing < NOR ) _facing = WES;
	    break;
	    case WHITE:
		_grid[_sx][_sy] = BLACK;
		if( ++_facing > WES ) _facing = NOR;
	}
	switch( _facing )
	{
	    case NOR: 
		if( --_sy < 0 )
		{
		    if( INFINIT_RUN ) _sy = GRID_SIZE - 1;
		    else _antState = RESTING;
		}
	    break;
	    case EAS:
		if( ++_sx >= GRID_SIZE )
		{
		    if( INFINIT_RUN ) _sx = 0;
		    else _antState = RESTING;
		}
	    break;
	    case SOU:
		if( ++_sy >= GRID_SIZE )
		{
		    if( INFINIT_RUN ) _sy = 0;
		    else _antState = RESTING;
		}
	    break;
	    case WES:
	        if( --_sx < 0 )
		{
		    if( INFINIT_RUN ) _sx = GRID_SIZE - 1;
		    else _antState = RESTING;
		}
	}
    }

    void display()
    {
        _bmp.clear();
		
        HBRUSH br; RECT rc;
        int xx, yy; HDC dc = _bmp.getDC();

        for( int y = 0; y < GRID_SIZE; y++ )
	    for( int x = 0; x < GRID_SIZE; x++ )
	    {
	        switch( _grid[x][y] )
	        {
		    case BLACK: br = static_cast<HBRUSH>( GetStockObject( BLACK_BRUSH ) ); break;
		    case WHITE: br = static_cast<HBRUSH>( GetStockObject( WHITE_BRUSH ) );
	        }
	        if( x == _sx && y == _sy ) br = RED_BRUSH;

	        xx = x * CELL_SIZE; yy = y * CELL_SIZE;
	        SetRect( &rc, xx, yy, xx + CELL_SIZE, yy + CELL_SIZE );
	        FillRect( dc, &rc, br );
	    }

        HDC wdc = GetDC( _hwnd );
        BitBlt( wdc, 0, 0, BMP_SIZE, BMP_SIZE, dc, 0, 0, SRCCOPY );
        ReleaseDC( _hwnd, wdc );
    }

    myBitmap _bmp;
    HWND     _hwnd;
    HBRUSH   RED_BRUSH;
    BYTE     _grid[GRID_SIZE][GRID_SIZE];
    int      _sx, _sy, _facing;
    state    _antState;
};
//--------------------------------------------------------------------------------------------------
class wnd
{
public:
    int wnd::Run( HINSTANCE hInst )
    {
	_hInst = hInst;
	_hwnd = InitAll();

	_ant.setHWND( _hwnd );
	_ant.setPosition( GRID_SIZE / 2, GRID_SIZE / 2 );

	ShowWindow( _hwnd, SW_SHOW );
	UpdateWindow( _hwnd );

	MSG msg;
	ZeroMemory( &msg, sizeof( msg ) );
	while( msg.message != WM_QUIT )
	{
	    if( PeekMessage( &msg, NULL, 0, 0, PM_REMOVE ) != 0 )
	    {
		TranslateMessage( &msg );
		DispatchMessage( &msg );
	    }
	    else
	    {
		_ant.mainLoop();
	    }
	}
	return UnregisterClass( "_LANGTONS_ANT_", _hInst );
    }
private:
    static int WINAPI wnd::WndProc( HWND hWnd, UINT msg, WPARAM wParam, LPARAM lParam )
    {
	switch( msg )
	{
	    case WM_DESTROY: PostQuitMessage( 0 ); break;
	    default:
		return DefWindowProc( hWnd, msg, wParam, lParam );
	}
	return 0;
    }

    HWND InitAll()
    {
	WNDCLASSEX wcex;
	ZeroMemory( &wcex, sizeof( wcex ) );
	wcex.cbSize	       = sizeof( WNDCLASSEX );
	wcex.style	       = CS_HREDRAW | CS_VREDRAW;
	wcex.lpfnWndProc   = ( WNDPROC )WndProc;
	wcex.hInstance     = _hInst;
	wcex.hCursor       = LoadCursor( NULL, IDC_ARROW );
	wcex.hbrBackground = ( HBRUSH )( COLOR_WINDOW + 1 );
	wcex.lpszClassName = "_LANGTONS_ANT_";

	RegisterClassEx( &wcex );

	return CreateWindow( "_LANGTONS_ANT_", ".: Langton's Ant -- PJorente :.", WS_SYSMENU, CW_USEDEFAULT, 0, BMP_SIZE, BMP_SIZE, NULL, NULL, _hInst, NULL );
    }

    HINSTANCE _hInst;
    HWND      _hwnd;
    Ant       _ant;
};
//--------------------------------------------------------------------------------------------------
int APIENTRY _tWinMain( HINSTANCE hInstance, HINSTANCE hPrevInstance, LPTSTR lpCmdLine, int nCmdShow )
{
    wnd myWnd;
    return myWnd.Run( hInstance );
}
//--------------------------------------------------------------------------------------------------

```


=={{header|C sharp|C#}}==

```csharp
using System;

namespace LangtonAnt
{
    public struct Point
    {
        public int X;
        public int Y;

        public Point(int x, int y)
        {
            X = x;
            Y = y;
        }
    }

    enum Direction
    {
        North, East, West, South
    }

    public class Langton
    {
        public readonly bool [,] IsBlack;
        private Point _origin;
        private Point _antPosition = new Point(0, 0);
        public bool OutOfBounds { get; set;}

        // I don't see any mention of what direction the ant is supposed to start out in
        private Direction _antDirection = Direction.East;

        private readonly Direction[] _leftTurn = new[] { Direction.West, Direction.North, Direction.South, Direction.East };
        private readonly Direction[] _rightTurn = new[] { Direction.East, Direction.South, Direction.North, Direction.West };
        private readonly int[] _xInc = new[] { 0, 1,-1, 0};
        private readonly int[] _yInc = new[] {-1, 0, 0, 1};

        public Langton(int width, int height, Point origin)
        {
            _origin = origin;
            IsBlack = new bool[width, height];
            OutOfBounds = false;
        }

        public Langton(int width, int height) : this(width, height, new Point(width / 2, height / 2)) {}

        private void MoveAnt()
        {
            _antPosition.X += _xInc[(int)_antDirection];
            _antPosition.Y += _yInc[(int)_antDirection];
        }

        public Point Step()
        {
            if (OutOfBounds)
            {
                throw new InvalidOperationException("Trying to step after ant is out of bounds");
            }
            Point ptCur = new Point(_antPosition.X + _origin.X, _antPosition.Y + _origin.Y);
            bool leftTurn = IsBlack[ptCur.X, ptCur.Y];
            int iDirection = (int) _antDirection;
            _antDirection = leftTurn ? _leftTurn[iDirection] : _rightTurn[iDirection];
            IsBlack[ptCur.X, ptCur.Y] = !IsBlack[ptCur.X, ptCur.Y];
            MoveAnt();
            ptCur = new Point(_antPosition.X + _origin.X, _antPosition.Y + _origin.Y);
            OutOfBounds = 
                ptCur.X < 0 ||
                ptCur.X >= IsBlack.GetUpperBound(0) ||
                ptCur.Y < 0 ||
                ptCur.Y >= IsBlack.GetUpperBound(1);
            return _antPosition;
        }
    }
    class Program
    {
        static void Main()
        {
            Langton ant = new Langton(100, 100);

            while (!ant.OutOfBounds) ant.Step();

            for (int iRow = 0; iRow < 100; iRow++)
            {
                for (int iCol = 0; iCol < 100; iCol++)
                {
                    Console.Write(ant.IsBlack[iCol, iRow] ? "#" : " ");
                }
                Console.WriteLine();
            }

            Console.ReadKey();
        }
    }
}

```

Output:

```txt

<Blank lines eliminated for efficiency>                          # #                                                                       
                        ## # #                                                                      
                       # ### ##                                                                     
                      #### ### #                                                                    
                      ##### #  ##                                                                   
                       #   ## ## #                                                                  
                        ###   #  ##                                                                 
                         #   ## ## #                                                                
                          ###   #  ##                                                               
                           #   ## ## #                                                              
                            ###   #  ##                                                             
                             #   ## ## #                                                            
                              ###   #  ##                                                           
                               #   ## ## #                                                          
                                ###   #  ##                                                         
                                 #   ## ## #                                                        
                                  ###   #  ##                                                       
                                   #   ## ## #                                                      
                                    ###   #  ##                                                     
                                     #   ## ## #                                                    
                                      ###   #  ##                                                   
                                       #   ## ## #                                                  
                                        ###   #  ##                                                 
                                         #   ## ## #                                                
                                          ###   #  ##                                               
                                           #   ## ## #                                              
                                            ###   #  ##                                             
                                             #   ## ## #                                            
                                              ###   #  ##                                           
                                               #   ## ## #                                          
                                                ###   #  ##                                         
                                                 #   ## ## #  ##                                    
                                                  ###   #  ##  ##                                   
                                                   #   ## ##  ##   #                                
                                             ####   ###   #   #  ###                                
                                            #    #   #   ## ####   #                                
                                           ###    #   # #      # ## #                               
                                           ###    # ##     # ##  # ##                               
                                            #    #   ## # #     ##                                  
                                            # #      # #####  #   #                                 
                                           #   #####          ## ######                             
                                           ###  ##  # ## # # #   ## # ##                            
                                         ##  # ####### #   #  ###    ## #                           
                                        #  #  ###### ##   #  # ##   #   #                           
                                       #    # # ## #  ###### #######   #                            
                                       # #### ## # ####    ##  ## # ## #                            
                                        #    ####   #  # ###### ##    ###                           
                                           #   # ## # ### #  ##  ##   ###                           
                                              #######    #  ## ## #     #                           
                                      ####  ## ##  #### ## ## ##  #     #                           
                                     #    # #   ### ## ###    # ####    #                           
                                    ###       ### # # #####    # #      #                           
                                    # #   ### #### ## #   ## ### ##     #                           
                                          ## ##  ####    #### # # #     #                           
                                     #    #  ##   ###  ###     ###      #                           
                                     ##   ## ### ####  #      ###   ##  #                           
                                     ## # ####     #   #  # ## ### ##   #                           
                                    #### ##   ## ####  # #  #  #  ###   #                           
                                    # ## ###  # # ## # #     # #     # #                            
                                        # #  #    ## ##  # #  ### ##                                
                                        ## #    #  ##### #    #    #  # #                           
                                       # ## #  #    ## ## #  ###      ###                           
                                     # #   #  #  #  #  ###   ##  ##    #                            
                                    ### # ##### ###### ### ####### # ##                             
                                    # # #    #####   ##  ##### #####                                
                                      #  ##   #      #  # ##  ### ###                               
                                   ####   ##### #########   # #                                     
                              ##    #  #     ### # #   # ###  ###                                   
                             #  #  #### ##   ### ##   ### ##     ##                                 
                            ###    # ## # #####   #    #  #  ## ###                                 
                            # ##### # #   ##  ##     #    #   #  #                                  
                                ###### ####  ## #   #  ##  # # ##                                   
                              ##      # ### ##  ####   #   ###                                      
                               #  # #####  #   # ##   #  #  #                                       
                               ## ### #######     #     # ##                                        
                              # #  ## ##      #   ##    #                                           
                             #  # ####        ###  ##  #                                            
                             # ## ###            ##  ##                                             
                              ##                                                                    
                               ##                                                                   

```



## Chapel


```chapel

config const gridHeight: int = 100;
config const gridWidth: int = 100;

class PBMWriter {
  var imgDomain: domain(2);
  var imgData: [imgDomain] int;
  
  proc PBMWriter( height: int, width: int ){
    imgDomain = { 1..#height, 1..#width };
  }
  
  proc this( i : int, j : int) ref : int{
    return this.imgData[ i, j ];
  }

  proc writeImage( fileName: string ){
    var file = open(fileName, iomode.cw);
    var writingChannel = file.writer();
    writingChannel.write("P1\n", imgDomain.dim(1).size, " " ,imgDomain.dim(2).size,"\n");
    
    for px in imgData {
      writingChannel.write( px, " " );
    }
    
    writingChannel.write( "\n" );
    writingChannel.flush();
    writingChannel.close();
  }
  
}

enum Color { white, black };

inline proc nextDirection( position: 2*int, turnLeft: bool ): 2*int {
  return ( (if turnLeft then 1 else -1 ) * position[2], (if turnLeft then -1 else 1 ) * position[1] );
}

proc <( left: 2*int, right: 2*int ){
  return left[1] < right[1] && left[2] < right[2];
}

proc <=( left: 2*int, right: 2*int ){
  return left[1] <= right[1] && left[2] <= right[2];
}

proc main{
  const gridDomain: domain(2) = {1..#gridHeight, 1..#gridWidth};
  var grid: [gridDomain] Color;
  
  var antPos = ( gridHeight / 2, gridWidth / 2 );
  var antDir = (1,0); // start up;
  
  while (0,0) < antPos && antPos <= (gridHeight, gridWidth ) {
    var currColor = grid[ antPos ];
    grid[antPos] = if currColor == Color.white then Color.black else Color.white ;
    
    antDir = nextDirection( antDir, currColor == Color.black );
    antPos = antPos + antDir;
  }
  
  var image = new PBMWriter( height = gridHeight, width = gridWidth );
  
  for (i, j) in gridDomain {
    image[i,j] = if grid[gridHeight-j+1,gridHeight-i+1] == Color.black then 0 else 1;
  }
  
  image.writeImage( "output.png" );
}

```



## Clojure

In keeping with the spirit of Clojure, this program eschews mutable state entirely. Instead, all computation occurs within a single recursive loop whose "variables" are "adjusted" at each iteration, a natural fit for this particular execution model.

```Clojure
(let [bounds (set (range 100))
      xs [1 0 -1 0] ys [0 -1 0 1]]
  (loop [dir 0 x 50 y 50
         grid {[x y] false}]
    (if (and (bounds x) (bounds y))
      (let [cur (not (grid [x y]))
            dir (mod (+ dir (if cur -1 1)) 4)]
        (recur dir (+ x (xs dir)) (+ y (ys dir))
               (merge grid {[x y] cur})))
      (doseq [col (range 100)]
        (println
          (apply str
                 (map #(if (grid [% col]) \# \.)
                      (range 100))))))))
```



## COBOL

The following program displays the simulation in the console, and a very small font size (~4pt) will be needed to fit it into the window.
{{works with|OpenCOBOL}}

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. langtons-ant.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       78  Grid-Size               VALUE 100.
       01  grid-area.
           03  grid-x              OCCURS Grid-Size TIMES.
               05  grid-y          OCCURS Grid-Size TIMES.
                   07  cell-colour PIC X VALUE "W".
                       88  black   VALUE "B".
                       88  white   VALUE "W".

       01  ant-x                   PIC 999.
       01  ant-y                   PIC 999.

       01  ant-direction           PIC 9.
           88  upward              VALUE 0.
           88  rightward           VALUE 1.
           88  downward            VALUE 2.
           88  leftward            VALUE 3.

       78  Pause-Time-Ns           VALUE 10000000.

       01  display-y               PIC 999.

       78  Black-Background        VALUE 0.
       78  White-Background        VALUE 7.

       01  i                       PIC 999.
       01  j                       PIC 999.

       01  pause                   PIC X.

       PROCEDURE DIVISION.
       main-line.
           DIVIDE Grid-Size BY 2 GIVING ant-x, ant-y

           PERFORM display-initial-grid
           PERFORM UNTIL (ant-x = Grid-Size OR 0)
                   OR (ant-y = Grid-Size OR 0)
               PERFORM step-simulation
               CALL "CBL_OC_NANOSLEEP" USING Pause-Time-Ns
           END-PERFORM

           DISPLAY "Press enter to quit." AT LINE 1 COLUMN 1
           ACCEPT pause

           GOBACK
           .
       step-simulation.
           IF black (ant-x, ant-y)
               SET white (ant-x, ant-y) TO TRUE
               PERFORM display-ant-cell
               COMPUTE ant-direction =
                   FUNCTION MOD(ant-direction + 1, 4)
           ELSE
               SET black (ant-x, ant-y) TO TRUE
               PERFORM display-ant-cell
               COMPUTE ant-direction =
                   FUNCTION MOD(ant-direction - 1, 4)
           END-IF

           EVALUATE TRUE
               WHEN upward
                   ADD 1 TO ant-y
               WHEN rightward
                   ADD 1 TO ant-x
               WHEN downward
                   SUBTRACT 1 FROM ant-y
               WHEN leftward
                   SUBTRACT 1 FROM ant-x
           END-EVALUATE
           .
       display-ant-cell.
               SUBTRACT ant-y FROM Grid-Size GIVING display-y
               IF black (ant-x, ant-y)
                   DISPLAY SPACE AT LINE display-y COLUMN ant-x
                       WITH BACKGROUND-COLOR Black-Background
               ELSE
                   DISPLAY SPACE AT LINE display-y COLUMN ant-x
                      WITH BACKGROUND-COLOR White-Background
               END-IF
               .
       display-initial-grid.
           PERFORM VARYING i FROM 1 BY 1 UNTIL i > Grid-Size
                   AFTER j FROM 1 BY 1 UNTIL j > Grid-Size
               DISPLAY SPACE AT LINE i COLUMN j
                   WITH BACKGROUND-COLOR White-Background
           END-PERFORM
           .
```



## CoffeeScript


```coffeescript

class Ant
  constructor: (@world) ->
    @location = [0, 0]
    @direction = 'E'
    
  move: =>
    [x, y] = @location
    if @world.is_set x, y
      @world.unset x, y
      @direction = Directions.left @direction
    else
      @world.set x, y
      @direction = Directions.right @direction
    @location = Directions.forward(x, y, @direction)

# Model a theoretically infinite 2D world with a hash, allowing squares
# to be black or white (independent of any ants.)
class BlackWhiteWorld
  constructor: ->
    @bits = {}
    
  set: (x, y) ->
    @bits["#{x},#{y}"] = true
    
  unset: (x, y) ->
    delete @bits["#{x},#{y}"]
    
  is_set: (x, y) ->
    @bits["#{x},#{y}"]

  draw: ->
    # Most of this code just involves finding the extent of the world.
    # Always include the origin, even if it's not set.
    @min_x = @max_x = @min_y = @max_y = 0
    for key of @bits
      [xx, yy] = (coord for coord in key.split ',')
      x = parseInt xx
      y = parseInt yy
      @min_x = x if x < @min_x
      @max_x = x if x > @max_x
      @min_y = y if y < @min_y
      @max_y = y if y > @max_y
    console.log "top left: #{@min_x}, #{@max_y}, bottom right: #{@max_x}, #{@min_y}"
    for y in [@max_y..@min_y] by -1
      s = ''
      for x in [@min_x..@max_x]
        if @bits["#{x},#{y}"]
          s += '#'
        else
          s += '_'
      console.log s

# Simple code for directions, independent of ants.
Directions =
  left: (dir) ->
    return 'W' if dir == 'N'
    return 'S' if dir == 'W'
    return 'E' if dir == 'S'
    'N'
  
  right: (dir) ->
    return 'E' if dir == 'N'
    return 'S' if dir == 'E'
    return 'W' if dir == 'S'
    'N'
    
  forward: (x, y, dir) ->
    return [x, y+1] if dir == 'N'
    return [x, y-1] if dir == 'S'
    return [x+1, y] if dir == 'E'
    return [x-1, y] if dir == 'W'


world = new BlackWhiteWorld()
ant = new Ant(world)
for i in [1..11500]
  ant.move()
console.log "Ant is at #{ant.location}, direction #{ant.direction}"
world.draw()

```


output

<lang>
> coffee langstons_ant.coffee 
Ant is at -24,46, direction W
top left: -25, 47, bottom right: 22, -29
_##__##_________________________________________
##_#####________________________________________
#____##_#_______________________________________
____#_#_##______________________________________
_####_###_#_____________________________________
_#####_#__##____________________________________
__#___##_##_#___________________________________
___###___#__##__________________________________
____#___##_##_#_________________________________
_____###___#__##________________________________
______#___##_##_#_______________________________
_______###___#__##______________________________
________#___##_##_#_____________________________
_________###___#__##____________________________
__________#___##_##_#___________________________
___________###___#__##__________________________
____________#___##_##_#_________________________
_____________###___#__##________________________
______________#___##_##_#_______________________
_______________###___#__##______________________
________________#___##_##_#_____________________
_________________###___#__##____________________
__________________#___##_##_#___________________
___________________###___#__##__________________
____________________#___##_##_#_________________
_____________________###___#__##________________
______________________#___##_##_#_______________
_______________________###___#__##______________
________________________#___##_##_#__##_________
_________________________###___#__##__##________
__________________________#___##_##__##___#_____
____________________####___###___#___#__###_____
___________________#____#___#___##_####___#_____
__________________###____#___#_#______#_##_#____
__________________###____#_##_____#_##__#_##____
___________________#____#___##_#_#_____##_______
___________________#_#______#_#####__#___#______
__________________#___#####__________##_######__
__________________###__##__#_##_#_#_#___##_#_##_
________________##__#_#######_#___#__###____##_#
_______________#__#__######_##___#__#_##___#___#
______________#____#_#_##_#__######_#######___#_
______________#_####_##_#_####____##__##_#_##_#_
_______________#____####___#__#_######_##____###
__________________#___#_##_#_###_#__##__##___###
_____________________#######____#__##_##_#_____#
_____________####__##_##__####_##_##_##__#_____#
____________#____#_#___###_##_###____#_####____#
___________###_______###_#_#_#####____#_#______#
___________#_#___###_####_##_#___##_###_##_____#
_________________##_##__####____####_#_#_#_____#
____________#____#__##___###__###_____###______#
____________##___##_###_####__#______###___##__#
____________##_#_####_____#___#__#_##_###_##___#
___________####_##___##_####__#_#__#__#__###___#
___________#_##_###__#_#_##_#_#_____#_#_____#_#_
_______________#_#__#____##_##__#_#__###_##_____
_______________##_#____#__#####_#____#____#__#_#
______________#_##_#__#____##_##_#__###______###
____________#_#___#__#__#__#__###___##__##____#_
___________###_#_#####_######_###_#######_#_##__
___________#_#_#____#####___##__#####_#####_____
_____________#__##___#______#__#_##__###_###____
__________####___#####_#########___#_#__________
_____##____#__#_____###_#_#___#_###__###________
____#__#__####_##___###_##___###_##_____##______
___###____#_##_#_#####___#____#__#__##_###______
___#_#####_#_#___##__##_____#____#___#__#_______
_______######_####__##_#___#__##__#_#_##________
_____##______#_###_##__####___#___###___________
______#__#_#####__#___#_##___#__#__#____________
______##_###_#######_____#_____#_##_____________
_____#_#__##_##______#___##____#________________
____#__#_####________###__##__#_________________
____#_##_###____________##__##__________________
_____##_________________________________________
______##________________________________________

```




## Common Lisp


```lisp
(defmacro toggle (gv) `(setf  ,gv (not ,gv)))

(defun langtons-ant (width height start-x start-y start-dir) 
  (let ( (grid (make-array (list width height)))
         (x start-x)
         (y start-y)
         (dir start-dir) )
    (loop while (and (< -1 x width) (< -1 y height)) do
      (if (toggle (aref grid x y))
        (setq dir (mod (1+ dir) 4))
        (setq dir (mod (1- dir) 4)))
      (case dir
        (0 (decf y))
        (1 (incf x))
        (2 (incf y))
        (3 (decf x)))
    )
    grid
  )
)

(defun show-grid (grid) 
  (destructuring-bind (width height) (array-dimensions grid) 
    (dotimes (y height)
      (dotimes (x width)
        (princ (if (aref grid x y) "#" ".")))
      (princ #\Newline))
  )
)

(setf *random-state* (make-random-state t))
(show-grid (langtons-ant 100 100 (+ 45 (random 10)) (+ 45 (random 10)) (random 4)))
```



## D


### Textual Version


```d
void main() @safe {
    import std.stdio, std.traits;

    enum width = 75, height = 52;
    enum maxSteps = 12_000;
    enum Direction { up, right, down, left }
    enum Color : char { white = '.', black = '#' }
    uint x = width / 2, y = height / 2;
    Color[width][height] M;
    auto dir = Direction.up;

    with (Color)
        for (int i = 0; i < maxSteps && x < width && y < height; i++) {
            immutable turn = M[y][x] == black;
            dir = [EnumMembers!Direction][(dir + (turn ? 1 : -1)) & 3];
            M[y][x] = (M[y][x] == black) ? white : black;
            final switch(dir) with (Direction) {
                case up:    y--; break;
                case right: x--; break;
                case down:  y++; break;
                case left:  x++; break;
            }
        }

    writefln("%(%-(%c%)\n%)", M);
}
```

{{out}}

```txt
...........................................................................
...........................................................................
...........................................................................
...........................................................................
.............................##..############..##..........................
............................#..####..........#..##.........................
...........................###...##............##.#........................
...........................#.#..#.........#..#....#........................
.......................##..##.#.#.........###.......#......................
....................###.#..#...#.....#.....##.##..###......................
.....................#.#..###..##.####.##...#.#..#.##..##..................
.....................#.###.##..#.##..###.#.#.....###...###.................
...................#.....#...#####.#.#..####..#...###.#.#.#................
..................###.##...#.####..##.##.######.#.###.#...#................
..................#.###.#.##.#.#.##.##.##.#...#####.###.##.................
......................#.#...#.##.###...#...#.#..####....#.##...............
...................#..#.........##.##...#..##.....##.#.....##..............
..................###...#.#.##.###..#..##.....#...###.##..##.#.............
.................#..###..##...##.##...###..#....#..##.####...#.............
................###...#...#.#..#.#.####.##..#.##.###..#.....#..............
...............#..###..#.##....#..#.###..#......###.##.#..#..##............
..............###...#.....#.##.#.##..##..#####.####..####.##...#...........
.............#..###..#.#.#..#.###.#.#.##......##...#.#.#....#...#..........
............###...#..##.###..##.#...##.......####.####...#......#..........
...........#..###..#.#..#...##..###########.#..####..#....#....#...........
..........###...#..##......#.####..##..#########..#..##....#..##...........
.........#..###..#.#...##..#.##...##.##.###.###...#..#.##..####.#..........
........###...#..##...#..#.######.##.#.##.#.#....###.###...##...#..........
.......#..###..#.#...#.....#####.#.#####.....#.#..##.#....##...#...........
......###...#..##....#.....#.##.#####.##..#.#...#..#..##.#..#..#...........
.....#..###..#.#.....#....#...####.#..#####.##...##########...##...........
....###...#..##......#.##...##...#..#...####..#...##.####.##...............
...#..###..#.#........#####.#..##...##.#...#....#.#..#..#..#.#.............
..###...#..##..........##..##.#.#.#....##.##.#.#.##..#..##..##.............
.#..###..#.#.................#..#....#.########.#.#.##..####.#.............
###...#..##..................#..#...#.......##.##...#..#..##.#.............
...##..#.#....................#..#..#......#..##..##...##.####.............
##..#..##......................##...#.......##..##....#...#.###............
.#.#.#.#............................#.##..####....####.###.####............
####.##..............................##..####....##..#.##.#.#..#...........
#.##.#................................##....##....##.###.##.#####..........
.####................................................#.##.#..####..........
..##.....................................................##.##.##..........
.........................................................##................
.......................................................#.##..####.#........
......................................................#..#.###..###........
......................................................#.##.#..#..#.........
.......................................................##......##..........
........................................................##.................
...........................................................................
...........................................................................
...........................................................................
```



### Image Version

This similar version requires the module from the Grayscale Image Task to generate and save a PGM image.

```d
import std.stdio, std.algorithm, std.traits, grayscale_image;

void main() {
    enum width = 100, height = 100;
    enum nSteps = 12_000;
    enum Direction { up, right, down, left }
    auto M = new Image!Gray(width, height);
    M.clear(Gray.white);
    uint x = width / 2, y = height / 2;
    auto dir = Direction.up;

    for (int i = 0; i < nSteps && x < width && y < height; i++) {
        immutable turn = M[x, y] == Gray.black;
        dir = [EnumMembers!Direction][(dir + (turn ? 1 : -1)) & 3];
        M[x, y] = (M[x, y] == Gray.black) ? Gray.white : Gray.black;
        final switch(dir) with (Direction) {
            case up:    y--; break;
            case right: x--; break;
            case down:  y++; break;
            case left:  x++; break;
        }
    }

    M.savePGM("langton_ant.pgm");
}
```



## Dyalect



```dyalect
const xInc = [0, 1, -1, 0]
const yInc = [-1, 0, 0, 1]
const north = 0
const east = 1
const west = 2
const south = 3

const leftTurns  = [ west, north, south, east ]
const rightTurns = [ east, south, north, west ]

func move(ant) {
    ant.position.x += xInc[ant.direction]
    ant.position.y += yInc[ant.direction]
}

func Array.step(ant) {
    var ptCur = (x: ant.position.x + ant.origin.x, y: ant.position.y + ant.origin.y)
    var leftTurn = this[ptCur.x][ptCur.y]
    ant.direction =
        if leftTurn  {
            leftTurns[ant.direction] 
        } else {
            rightTurns[ant.direction]
        }
    this[ptCur.x][ptCur.y] = !this[ptCur.x][ptCur.y]
    move(ant)
    ptCur = (x: ant.position.x + ant.origin.x, y: ant.position.y + ant.origin.y)
    ant.outOfBounds = 
        ptCur.x < 0 ||
        ptCur.x >= ant.width ||
        ptCur.y < 0 ||
        ptCur.y >= ant.height
    ant.position
}

func newAnt(width, height) {
    (
        position: (x: 0, y: 0),
        origin: (x: width / 2, y: height / 2),
        outOfBounds: false,
        isBlack: [],
        direction: east,
        width: width,
        height: height
    )
}

func run() {
    const w = 100
    const h = 100
    const blacks = Array.empty(w, () => Array.empty(h, false))
    const ant = newAnt(w, h)

    while !ant.outOfBounds {
        blacks.step(ant)
    }

    var iRow = 0;

    while iRow < w {
        var iCol = 0;
        var ln = ""
        while iCol < h {
            ln += if blacks[iCol][iRow] {
                "#"
            } else {
                " "
            }
            iCol += 1
        }
        print(ln)
        iRow += 1
    }
}

run()
```


{{out}}

Empty lines are omitted.


```txt
                          # #
                        ## # #
                       # ### ##
                      #### ### #
                      ##### #  ##
                       #   ## ## #
                        ###   #  ##
                         #   ## ## #
                          ###   #  ##
                           #   ## ## #
                            ###   #  ##
                             #   ## ## #
                              ###   #  ##
                               #   ## ## #
                                ###   #  ##
                                 #   ## ## #
                                  ###   #  ##
                                   #   ## ## #
                                    ###   #  ##
                                     #   ## ## #
                                      ###   #  ##
                                       #   ## ## #
                                        ###   #  ##
                                         #   ## ## #
                                          ###   #  ##
                                           #   ## ## #
                                            ###   #  ##
                                             #   ## ## #
                                              ###   #  ##
                                               #   ## ## #
                                                ###   #  ##
                                                 #   ## ## #  ##
                                                  ###   #  ##  ##
                                                   #   ## ##  ##   #
                                             ####   ###   #   #  ###
                                            #    #   #   ## ####   #
                                           ###    #   # #      # ## #
                                           ###    # ##     # ##  # ##
                                            #    #   ## # #     ##
                                            # #      # #####  #   #
                                           #   #####          ## ######
                                           ###  ##  # ## # # #   ## # ##
                                         ##  # ####### #   #  ###    ## #
                                        #  #  ###### ##   #  # ##   #   #
                                       #    # # ## #  ###### #######   #
                                       # #### ## # ####    ##  ## # ## #
                                        #    ####   #  # ###### ##    ###
                                           #   # ## # ### #  ##  ##   ###
                                              #######    #  ## ## #     #
                                      ####  ## ##  #### ## ## ##  #     #
                                     #    # #   ### ## ###    # ####    #
                                    ###       ### # # #####    # #      #
                                    # #   ### #### ## #   ## ### ##     #
                                          ## ##  ####    #### # # #     #
                                     #    #  ##   ###  ###     ###      #
                                     ##   ## ### ####  #      ###   ##  #
                                     ## # ####     #   #  # ## ### ##   #
                                    #### ##   ## ####  # #  #  #  ###   #
                                    # ## ###  # # ## # #     # #     # #
                                        # #  #    ## ##  # #  ### ##
                                        ## #    #  ##### #    #    #  # #
                                       # ## #  #    ## ## #  ###      ###
                                     # #   #  #  #  #  ###   ##  ##    #
                                    ### # ##### ###### ### ####### # ##
                                    # # #    #####   ##  ##### #####
                                      #  ##   #      #  # ##  ### ###
                                   ####   ##### #########   # #
                              ##    #  #     ### # #   # ###  ###
                             #  #  #### ##   ### ##   ### ##     ##
                            ###    # ## # #####   #    #  #  ## ###
                            # ##### # #   ##  ##     #    #   #  #
                                ###### ####  ## #   #  ##  # # ##
                              ##      # ### ##  ####   #   ###
                               #  # #####  #   # ##   #  #  #
                               ## ### #######     #     # ##
                              # #  ## ##      #   ##    #
                             #  # ####        ###  ##  #
                             # ## ###            ##  ##
                              ##
                               ##
```



## EasyLang


[https://easylang.online/apps/run.html?code=len%20f%5B%5D%20100%20%2A%20100%0Afunc%20show%20.%20.%0Afor%20y%20range%20100%0Afor%20x%20range%20100%0Aif%20f%5By%20%2A%20100%20%2B%20x%5D%20%3D%201%0Amove%20x%20y%0Arect%201%201%0A.%0A.%0A.%0A.%0Afunc%20run%20x%20y%20dir%20.%20.%0Adx%5B%5D%20%3D%20%5B%200%201%200%20-1%20%5D%0Ady%5B%5D%20%3D%20%5B%20-1%200%201%200%20%5D%0Awhile%20x%20%3E%3D%200%20and%20x%20%3C%20100%20and%20y%20%3E%3D%200%20and%20y%20%3C%20100%0Av%20%3D%20f%5By%20%2A%20100%20%2B%20x%5D%0Af%5By%20%2A%20100%20%2B%20x%5D%20%3D%201%20-%20v%0Adir%20%3D%20%28dir%20%2B%201%20%2B%202%20%2A%20v%29%20mod%204%0Ax%20%2B%3D%20dx%5Bdir%5D%0Ay%20%2B%3D%20dy%5Bdir%5D%0A.%0A.%0Acall%20run%2070%2040%200%0Acall%20show Run it]

<lang>len f[] 100 * 100
func show . .
  for y range 100
    for x range 100
      if f[y * 100 + x] = 1
        move x y
        rect 1 1
      .
    .
  .
.
func run x y dir . .
  dx[] = [ 0 1 0 -1 ]
  dy[] = [ -1 0 1 0 ]
  while x >= 0 and x < 100 and y >= 0 and y < 100
    v = f[y * 100 + x]
    f[y * 100 + x] = 1 - v
    dir = (dir + 1 + 2 * v) mod 4
    x += dx[dir]
    y += dy[dir]
  .
.
call run 70 40 0
call show
```



## EchoLisp

We implement multi-colored ants, as depicted in the article. An ant is described using L(eft)R(ight) patterns. LR is the basic black and white ant, other are  RRLLLRRL or  RRLLLRLLLRRR. See results for s [http://www.echolalie.org/echolisp/images/ant-1.png black-and-white] or [http://www.echolalie.org/echolisp/images/ant-2.png colored] ants.

```scheme

(lib 'plot)
(lib 'types)

(define (move iter x dir constant: plane turns cmax  width  xmax (cidx 0))
	(while (> iter 0)
	;; get color index of current square
	(set! cidx (vector-ref plane x)) 

	;; turn	
	(if (vector-ref turns cidx)  
		(set! dir (if (= dir 3) 0 (1+ dir))) ;; right is #t
		(set! dir (if (= dir 0) 3 (1- dir)))) 

	;; rotate colors
	(set! cidx (if (= cidx cmax) 0 (1+ cidx)))
	(vector-set! plane x cidx)

	;; move
	;; x = v + h*width for a pixel at (h,v)
	(set! x 
		(cond
			((= dir 0) (1+ x))
			((= dir 1) (+ x width))
			((= dir 2) (1- x))
			((= dir 3) (- x width)))) 
		
	(when (or (< x 0) (>= x xmax)) (set! iter -666)) ;; out of bounds
	(set! iter (1- iter)))
	iter)
	
;; a color table of 16 colors
(define colors 
   (list 0 (rgb 1 1 1) (rgb 1 0 0) (rgb 0 1 0) (rgb 0 0 1) (rgb 1 1 0) (rgb 1 0 1) (rgb 0 1 1)))
(define colors (list->vector (append colors colors)))

;; transform color index into rgb color, using colors table.
(define (colorize plane xmax)
	(for ((x xmax)) 
		(vector-set! plane x (vector-ref colors (vector-ref plane x))))
	(vector->pixels plane)
	xmax )
	
;; ant's patterns
(define turns #(#t #t #f #f #f #t #f #f #f #t #t #t))   ;; RRLLLRLLLRRR
;;(define turns #(#t #t #f #f #f #t #t #f)) ; RRLLLRRL
;;(define turns #(#t #f)) ; RL : basic ant

(define  (ant (iter 100000))
	(plot-clear)
	(define width (first (pixels-dim))) ;; plane dimensions
	(define height (rest (pixels-dim)))
	(define plane (pixels->uint32-vector))
	(define x (+ (quotient width 2) (* width (quotient height 2)))) ;; middle of plane
	(define xmax (* width height))
	
	(move iter  x 0 plane turns (1- (vector-length turns)) width xmax)
	(colorize plane xmax))

(ant) ;; run

```



## Ela


A straightforward implementation (assumes that we start with ant looking forward):


```ela
open list core generic
 
type Field = Field a
type Color = White | Black
type Direction = Lft | Fwd | Rgt | Bwd
field s = Field [[White \\ _ <- [1..s]] \\ _ <- [1..s]]
 
isBlack Black = true
isBlack _ = false
 
newfield xc yc (Field xs) = Field (newfield' 0 xs)
  where newfield' _ [] = []
        newfield' n (x::xs) 
          | n == yc = row 0 x :: xs
          | else   = x :: newfield' (n+1) xs
          where row _ [] = []
                row n (x::xs) 
                  | n == xc = toggle x :: xs
                  | else    = x :: row (n+1) xs
                  where toggle White = Black
                        toggle Black = White
 
showPath (Field xs) = toString <| show' "" xs
  where show' sb [] = sb +> ""
        show' sb (x::xs) = show' (showRow sb x +> "\r\n") xs
          where showRow sb [] = sb +> ""
                showRow sb (x::xs) = showRow (sb +> s) xs
                  where s | isBlack x = "#"
                          | else = "_"
 
move s xc yc = move' (Fwd,xc,yc) (field s)
  where move' (pos,xc,yc)@coor fld 
          | xc >= s || yc >= s || xc < 0 || yc < 0 = fld
          | else = fld |> newfield xc yc |> move' (matrix (dir fld) coor)
          where dir (Field xs) 
                  | `isBlack` (xs:yc):xc = Lft
                  | else = Rgt
                matrix Lft (pos,x,y) = go (left pos,x,y)
                matrix Rgt (pos,x,y) = go (right pos,x,y)
                go (Lft,x,y) = (Lft,x - 1,y)
                go (Rgt,x,y) = (Rgt,x+1,y)
                go (Fwd,x,y) = (Fwd,x,y - 1)
                go (Bwd,x,y) = (Bwd,x,y+1)
                right Lft = Fwd
                right Fwd = Rgt
                right Rgt = Bwd
                right Bwd = Lft
                left Lft = Bwd
                left Bwd = Rgt
                left Rgt = Fwd
                left Fwd = Lft
```


This implementation is pure (doesn't produce side effects).

Testing:


```ela
showPath <| move 100 50 50
```


Output (empty lines are skipped to save space):


```txt
__________________________________________##__############__##______________________________________
_________________________________________#__####__________#__##_____________________________________
________________________________________###___##____________##_#____________________________________
________________________________________#_#__#_________#__#____#____________________________________
____________________________________##__##_#_#_________###_______#__________________________________
_________________________________###_#__#___#_____#_____##_##__###__________________________________
__________________________________#_#__###__##_####_##___#_#__#_##__##______________________________
__________________________________#_###_##__#_##__###_#_#_____###___###_____________________________
________________________________#_____#___#####_#_#__####__#___###_#_#_#____________________________
_______________________________###_##___#_####__##_##_######_#_###_#___#____________________________
_______________________________#_###_#_##_#_#_##_##_##_#___#####_###_##_____________________________
___________________________________#_#___#_##_###___#___#_#__####____#_##___________________________
________________________________#__#_________##_##___#__##_____##_#_____##__________________________
_______________________________###___#_#_##_###__#__##_____#___###_##__##_#_________________________
______________________________#__###__##___##_##___###__#____#__##_####___#_________________________
_____________________________###___#___#_#__#_#_####_##__#_##_###__#_____#__________________________
____________________________#__###__#_##____#__#_###__#______###_##_#__#__##________________________
___________________________###___#_____#_##_#_##__##__#####_####__####_##___#_______________________
__________________________#__###__#_#_#__#_###_#_#_##______##___#_#_#____#___#______________________
_________________________###___#__##_###__##_#___##_______####_####___#______#______________________
________________________#__###__#_#__#___##__###########_#__####__#____#____#_______________________
_______________________###___#__##______#_####__##__#########__#__##____#__##_______________________
______________________#__###__#_#___##__#_##___##_##_###_###___#__#_##__####_#______________________
_____________________###___#__##___#__#_######_##_#_##_#_#____###_###___##___#______________________
____________________#__###__#_#___#_____#####_#_#####_____#_#__##_#____##___#_______________________
___________________###___#__##____#_____#_##_#####_##__#_#___#__#__##_#__#__#_______________________
__________________#__###__#_#_____#____#___####_#__#####_##___##########___##_______________________
_________________###___#__##______#_##___##___#__#___####__#___##_####_##___________________________
________________#__###__#_#________#####_#__##___##_#___#____#_#__#__#__#_#_________________________
_______________###___#__##__________##__##_#_#_#____##_##_#_#_##__#__##__##_________________________
______________#__###__#_#_________________#__#____#_########_#_#_##__####_#_________________________
_____________###___#__##__________________#__#___#_______##_##___#__#__##_#_________________________
____________#__###__#_#____________________#__#__#______#__##__##___##_####_________________________
___________###___#__##______________________##___#_______##__##____#___#_###________________________
__________#__###__#_#____________________________#_##__####____####_###_####________________________
_________###___#__##______________________________##__####____##__#_##_#_#__#_______________________
________#__###__#_#________________________________##____##____##_###_##_#####______________________
_______###___#__##________________________________________________#_##_#__####______________________
______#__###__#_#_____________________________________________________##_##_##______________________
_____###___#__##______________________________________________________##____________________________
____#__###__#_#_____________________________________________________#_##__####_#____________________
___###___#__##_____________________________________________________#__#_###__###____________________
__#__###__#_#______________________________________________________#_##_#__#__#_____________________
_###___#__##________________________________________________________##______##______________________
#__###__#_#__________________________________________________________##_____________________________
_###_#__##__________________________________________________________________________________________
#_#_#_#_#___________________________________________________________________________________________
_####_##____________________________________________________________________________________________
_#_##_#_____________________________________________________________________________________________
__####______________________________________________________________________________________________
___##_______________________________________________________________________________________________

```



## Elixir

{{works with|Elixir|1.1+}}
{{trans|Ruby}}

```elixir
defmodule Langtons do
  def ant(sizex, sizey) do
    {px, py} = {div(sizex,2), div(sizey,2)}     # start position
    move(MapSet.new, sizex, sizey, px, py, {1,0}, 0)
  end
  
  defp move(plane, sx, sy, px, py, _, step) when px<0 or sx<px or py<0 or sy<py, do:
    print(plane, sx, sy, px, py, step)
  defp move(plane, sx, sy, px, py, dir, step) do
    {plane2, {dx,dy}} = if {px,py} in plane,
                          do:   {MapSet.delete(plane, {px,py}), turn_right(dir)},
                          else: {MapSet.put(plane, {px,py}), turn_left(dir)}
    move(plane2, sx, sy, px+dx, py+dy, {dx,dy}, step+1)
  end
  
  defp turn_right({dx, dy}), do: {dy, -dx}
  defp turn_left({dx, dy}), do: {-dy, dx}
  
  defp print(plane, sx, sy, px, py, step) do
    IO.puts "out of bounds after #{step} moves: (#{px}, #{py})"
    Enum.each(0..sy, fn j ->
      IO.puts Enum.map(0..sx, fn i -> if {i,j} in plane, do: "#", else: "." end)
    end)
  end
end

Langtons.ant(100, 100)
```


{{out}}
<pre style="height:40ex;overflow:scroll">
out of bounds after 11669 moves: (26, -1)
..........................#.#........................................................................
........................##.#.#.......................................................................
.......................#.###.##......................................................................
......................####.###.#.....................................................................
......................#####.#..##....................................................................
.......................#...##.##.#...................................................................
........................###...#..##..................................................................
.........................#...##.##.#.................................................................
..........................###...#..##................................................................
...........................#...##.##.#...............................................................
............................###...#..##..............................................................
.............................#...##.##.#.............................................................
..............................###...#..##............................................................
...............................#...##.##.#...........................................................
................................###...#..##..........................................................
.................................#...##.##.#.........................................................
..................................###...#..##........................................................
...................................#...##.##.#.......................................................
....................................###...#..##......................................................
.....................................#...##.##.#.....................................................
......................................###...#..##....................................................
.......................................#...##.##.#...................................................
........................................###...#..##..................................................
.........................................#...##.##.#.................................................
..........................................###...#..##................................................
...........................................#...##.##.#...............................................
............................................###...#..##..............................................
.............................................#...##.##.#.............................................
..............................................###...#..##............................................
...............................................#...##.##.#...........................................
................................................###...#..##..........................................
.................................................#...##.##.#..##.....................................
..................................................###...#..##..##....................................
...................................................#...##.##..##...#.................................
.............................................####...###...#...#..###.................................
............................................#....#...#...##.####...#.................................
...........................................###....#...#.#......#.##.#................................
...........................................###....#.##.....#.##..#.##................................
............................................#....#...##.#.#.....##...................................
............................................#.#......#.#####..#...#..................................
...........................................#...#####..........##.######..............................
...........................................###..##..#.##.#.#.#...##.#.##.............................
.........................................##..#.#######.#...#..###....##.#............................
........................................#..#..######.##...#..#.##...#...#............................
.......................................#....#.#.##.#..######.#######...#.............................
.......................................#.####.##.#.####....##..##.#.##.#.............................
........................................#....####...#..#.######.##....###............................
...........................................#...#.##.#.###.#..##..##...###............................
..............................................#######....#..##.##.#.....#............................
......................................####..##.##..####.##.##.##..#.....#............................
.....................................#....#.#...###.##.###....#.####....#............................
....................................###.......###.#.#.#####....#.#......#............................
....................................#.#...###.####.##.#...##.###.##.....#............................
..........................................##.##..####....####.#.#.#.....#............................
.....................................#....#..##...###..###.....###......#............................
.....................................##...##.###.####..#......###...##..#............................
.....................................##.#.####.....#...#..#.##.###.##...#............................
....................................####.##...##.####..#.#..#..#..###...#............................
....................................#.##.###..#.#.##.#.#.....#.#.....#.#.............................
........................................#.#..#....##.##..#.#..###.##.................................
........................................##.#....#..#####.#....#....#..#.#............................
.......................................#.##.#..#....##.##.#..###......###............................
.....................................#.#...#..#..#..#..###...##..##....#.............................
....................................###.#.#####.######.###.#######.#.##..............................
....................................#.#.#....#####...##..#####.#####.................................
......................................#..##...#......#..#.##..###.###................................
...................................####...#####.#########...#.#......................................
..............................##....#..#.....###.#.#...#.###..###....................................
.............................#..#..####.##...###.##...###.##.....##..................................
............................###....#.##.#.#####...#....#..#..##.###..................................
............................#.#####.#.#...##..##.....#....#...#..#...................................
................................######.####..##.#...#..##..#.#.##....................................
..............................##......#.###.##..####...#...###.......................................
...............................#..#.#####..#...#.##...#..#..#........................................
...............................##.###.#######.....#.....#.##.........................................
..............................#.#..##.##......#...##....#............................................
.............................#..#.####........###..##..#.............................................
.............................#.##.###............##..##..............................................
..............................##.....................................................................
...............................##....................................................................
.....................................................................................................
.....................................................................................................
.....................................................................................................
.....................................................................................................
.....................................................................................................
.....................................................................................................
.....................................................................................................
.....................................................................................................
.....................................................................................................
.....................................................................................................
.....................................................................................................
.....................................................................................................
.....................................................................................................
.....................................................................................................
.....................................................................................................
.....................................................................................................
.....................................................................................................
.....................................................................................................
.....................................................................................................
.....................................................................................................
.....................................................................................................

```


## Elm


```elm
import Maybe as M
import Matrix 
import Time exposing (Time, every, second)
import List exposing (..)
import String exposing (join)
import Html exposing (div, h1, text)
import Html.App exposing (program)
import Svg 
import Svg.Attributes exposing (version, viewBox, cx, cy, r, x, y, x1, y1, x2, y2, fill,style, width, height, preserveAspectRatio)

w = 700
h = 700
dt = 0.0001

type Direction = North | West | South | East

type alias Model =
  { rows : Int
  , cols : Int
  , boxes : Matrix.Matrix Bool
  , location : Matrix.Location
  , direction : Direction
  }

initModel : Int -> Int -> Model
initModel cols rows = 
     { rows = rows
     , cols = cols 
     , boxes = Matrix.matrix rows cols (\location -> False)
     , location = (rows//2,cols//2)
     , direction = North
     }

view model =
  let
    borderLineStyle = style "stroke:black;stroke-width:0.3"

    x1Min = x1 <| toString 0
    y1Min = y1 <| toString 0
    x1Max = x1 <| toString model.cols
    y1Max = y1 <| toString model.rows
    x2Min = x2 <| toString 0
    y2Min = y2 <| toString 0
    x2Max = x2 <| toString model.cols
    y2Max = y2 <| toString model.rows

    borders = [ Svg.line [ x1Min, y1Min, x2Max, y2Min, borderLineStyle ] []
              , Svg.line [ x1Max, y1Min, x2Max, y2Max, borderLineStyle ] []
              , Svg.line [ x1Max, y1Max, x2Min, y2Max, borderLineStyle ] []
              , Svg.line [ x1Min, y1Max, x2Min, y2Min, borderLineStyle ] []
              ]

    circleInBox (row,col) color = 
      Svg.circle [ r "0.25"
      , fill (color)
      , cx (toString (toFloat col + 0.5))
      , cy (toString (toFloat row + 0.5))
      ] [] 

    showUnvisited location box =
       if box then [circleInBox location "black" ]
              else []

    unvisited = model.boxes 
                  |> Matrix.mapWithLocation showUnvisited 
                  |> Matrix.flatten 
                  |> concat

    maze = [ Svg.g [] <| borders ++ unvisited ] 

  in
      div 
          [] 
          [ h1 [] [text "Langton's Ant"]
          , Svg.svg 
              [ version "1.1"
              , width (toString w)
              , height (toString h)
              , viewBox (join " " 
                           [ 0          |> toString
                           , 0          |> toString
                           , model.cols |> toString
                           , model.rows |> toString ])
              ] 
              maze
          ]

updateModel : Model -> Model
updateModel model = 
      let current = model.location
          inBox =    snd current >= 0 && snd current < model.cols
                  && fst current >= 0 && fst current < model.rows
      in if not inBox then
           model
         else
           let currentValue = Matrix.get current model.boxes |> M.withDefault False

               dir = case (model.direction, currentValue) of
                       (North, True) -> East
                       (East, True) -> South
                       (South, True) -> West
                       (West, True) -> North
 
                       (North, False) -> West
                       (East, False) -> North
                       (South, False) -> East
                       (West, False) -> South
 
               next = case dir of
                        North -> (fst current+1, snd current)
                        South -> (fst current-1, snd current)
                        East -> (fst current, snd current+1)
                        West -> (fst current, snd current-1)
 
               boxes = Matrix.set current (not currentValue) model.boxes 
 
           in {model | boxes=boxes, location=next, direction=dir}

type Msg = Tick Time 

subscriptions model = every (dt * second) Tick

main =
  let 
    update msg model = (updateModel model, Cmd.none)
    init = (initModel 100 100 , Cmd.none)
  in program 
       { init = init
       , view = view
       , update = update
       , subscriptions = subscriptions
       }
```


Link to live demo: https://dc25.github.io/langtonsAntElm/


## Erlang

Over-engineered sine I have summer vacation. Ex: Display function only display lines with black cells.

```Erlang

-module( langtons_ant ).

-export( [task/0] ).

-record( neighbour, {north, south, east, west} ).
-record( state, {colour=white, controller, max_x, max_y, neighbour, position} ).

task() ->
       Controller = erlang:self(),
       Max_x = Max_y = 100,
       Pid_positions = plane_create( Controller, Max_x, Max_y ),
       Pids = [X || {X, _} <- Pid_positions],
       [X ! {pid_positions, Pid_positions} || X <- Pids],
       {Pid, _Position} = lists:keyfind( {Max_x div 2, Max_y div 2}, 2, Pid_positions ),
       Pid ! {ant_start, north, Controller},
       receive
       {ant_arrives, _Pid} -> ok
       end,
       display( Controller, Max_x, Max_y, Pids ),
       [X ! {stop, Controller} || X <- Pids].



display( Controller, Max_x, Max_y, Pids ) ->
        Positions_colours = display_positions_colours( Pids, Controller ),
        All_lines = [display_line( Max_x, Positions_colours, Y ) || Y <- lists:seq(Max_y, 1, -1)],
        Lines_with_black = [X || X <- All_lines, lists:member(black, X)],
        [io:fwrite( "~s~n", [[display_on_screen(X) || X <- Lines]] ) || Lines <- Lines_with_black].

display_line( Max_x, Positions_colours, Y ) -> [proplists:get_value({X,Y}, Positions_colours, white) || X <- lists:seq(1, Max_x)].

display_on_screen( white ) -> $_;
display_on_screen( black ) -> $#.

display_positions_colours( Pids, Controller ) ->
        [X ! {position_colour, Controller} || X <- Pids],
        [display_positions_colours_receive() || _X <- Pids].

display_positions_colours_receive( ) ->
        receive
        {position_colour, Position, Colour} -> {Position, Colour}
        end.

loop( State ) ->
    receive
    {pid_positions, Pid_positions} ->
        {_My_position, Neighbour} = lists:foldl( fun loop_neighbour/2, {State#state.position, #neighbour{}}, Pid_positions ),
        erlang:garbage_collect(), % Shrink process after using large Pid_positions. For memory starved systems.
        loop( State#state{neighbour=Neighbour} );
    {ant_start, Direction, Controller} when Controller =:= State#state.controller ->
                {Pid, New_state} = loop_ant_departs( Direction, State ),
                Pid ! {ant_arrives, erlang:self()},
                loop( New_state );
    {ant_arrives, From} ->
                {Direction, New_state} = loop_ant_arrives( From, State ),
                {To, Newest_state} = loop_ant_departs( Direction, New_state ),
                To ! {ant_arrives, erlang:self()},
                loop( Newest_state );
    {position_colour, Controller} when Controller =:= State#state.controller ->
                Controller ! {position_colour, State#state.position, State#state.colour},
                loop( State );
    {stop, Controller} when Controller =:= State#state.controller -> ok
    end.

loop_ant_arrives( Pid, State ) ->
        Neighbour = State#state.neighbour,
        From = loop_ant_arrives_direction( Pid, Neighbour ),
        {loop_ant_arrives_new_direction(From, State), State}.

loop_ant_arrives_direction( Pid, #neighbour{north=Pid} ) -> north;
loop_ant_arrives_direction( Pid, #neighbour{south=Pid} ) -> south;
loop_ant_arrives_direction( Pid, #neighbour{east=Pid} ) -> east;
loop_ant_arrives_direction( Pid, #neighbour{west=Pid} ) -> west.

loop_ant_arrives_new_direction( north, #state{colour=white} ) -> west;
loop_ant_arrives_new_direction( north, #state{colour=black} ) -> east;
loop_ant_arrives_new_direction( south, #state{colour=white} ) -> east;
loop_ant_arrives_new_direction( south, #state{colour=black} ) -> west;
loop_ant_arrives_new_direction( east, #state{colour=white} ) -> north;
loop_ant_arrives_new_direction( east, #state{colour=black} ) -> south;
loop_ant_arrives_new_direction( west, #state{colour=white} ) -> south;
loop_ant_arrives_new_direction( west, #state{colour=black} ) -> north.

loop_ant_departs( north, #state{position={_X,Y}, max_y=Y}=State ) ->
        {State#state.controller, State};
loop_ant_departs( south, #state{position={_X,1}}=State ) ->
        {State#state.controller, State};
loop_ant_departs( east, #state{position={X,_Y}, max_x=X}=State ) ->
        {State#state.controller, State};
loop_ant_departs( west, #state{position={1,_Y}}=State ) ->
        {State#state.controller, State};
loop_ant_departs( Direction, State ) ->
        Neighbour = State#state.neighbour,
        Pid = loop_ant_departs_pid( Direction, Neighbour ),
        {Pid, State#state{colour=other_colour(State)}}.

loop_ant_departs_pid( north, #neighbour{north=Pid} ) -> Pid;
loop_ant_departs_pid( south, #neighbour{south=Pid} ) -> Pid;
loop_ant_departs_pid( east, #neighbour{east=Pid} ) -> Pid;
loop_ant_departs_pid( west, #neighbour{west=Pid} ) -> Pid.

loop_neighbour( {Pid, {X, Y}}, {{X, My_y}, Neighbour} ) when Y =:= My_y + 1 -> {{X, My_y}, Neighbour#neighbour{north=Pid}};
loop_neighbour( {Pid, {X, Y}}, {{X, My_y}, Neighbour} ) when Y =:= My_y - 1 -> {{X, My_y}, Neighbour#neighbour{south=Pid}};
loop_neighbour( {Pid, {X, Y}}, {{My_x, Y}, Neighbour} ) when X =:= My_x + 1 -> {{My_x, Y}, Neighbour#neighbour{east=Pid}};
loop_neighbour( {Pid, {X, Y}}, {{My_x, Y}, Neighbour} ) when X =:= My_x - 1 -> {{My_x, Y}, Neighbour#neighbour{west=Pid}};
loop_neighbour( _Pid_position, Acc ) -> Acc.

other_colour( #state{colour=white} ) -> black;
other_colour( #state{colour=black} ) -> white.

plane_create( Controller, Max_x, Max_y ) -> [{plane_create_cell(Controller, Max_x, Max_y, {X, Y}), {X,Y}} || X <- lists:seq(1, Max_x), Y<- lists:seq(1, Max_y)].
plane_create_cell( Controller, Max_x, Max_y, Position ) -> erlang:spawn_link( fun() -> loop( #state{controller=Controller, max_x=Max_x, max_y=Max_y, position=Position} ) end ).

```


{{out}}
<pre style="height:30ex;overflow:scroll">
___________________________________________________________________##_______________________________
____________________________________________________________________##______________________________
_____________________________________________##__##____________###_##_#_____________________________
____________________________________________#__##__###________####_#__#_____________________________
___________________________________________#____##___#______##_##__#_#______________________________
________________________________________##_#_____#_____#######_###_##_______________________________
_______________________________________#__#__#___##_#___#__#####_#__#_______________________________
______________________________________###___#___####__##_###_#______##______________________________
___________________________________##_#_#__##__#___#_##__####_######________________________________
__________________________________#__#___#____#_____##__##___#_#_#####_#____________________________
_________________________________###_##__#__#____#___#####_#_##_#____###____________________________
_________________________________##_____##_###___##_###___##_####__#__#_____________________________
___________________________________###__###_#___#_#_###_____#__#____##______________________________
_____________________________________#_#___#########_#####___####___________________________________
_______________________________###_###__##_#__#______#___##__#______________________________________
________________________________#####_#####__##___#####____#_#_#____________________________________
_____________________________##_#_#######_###_######_#####_#_###____________________________________
____________________________#____##__##___###__#__#__#__#___#_#_____________________________________
___________________________###______###__#_##_##____#__#_##_#_______________________________________
___________________________#_#__#____#____#_#####__#____#_##________________________________________
________________________________##_###__#_#__##_##____#__#_#________________________________________
____________________________#_#_____#_#_____#_#_##_#_#__###_##_#____________________________________
___________________________#___###__#__#__#_#__####_##___##_####____________________________________
___________________________#___##_###_##_#__#___#_____####_#_##_____________________________________
___________________________#__##___###______#__####_###_##___##_____________________________________
___________________________#______###_____###__###___##__#____#_____________________________________
___________________________#_____#_#_#_####____####__##_##__________________________________________
___________________________#_____##_###_##___#_##_####_###___#_#____________________________________
___________________________#______#_#____#####_#_#_###_______###____________________________________
___________________________#____####_#____###_##_###___#_#____#_____________________________________
___________________________#_____#__##_##_##_####__##_##__####______________________________________
___________________________#_____#_##_##__#____#######______________________________________________
___________________________###___##__##__#_###_#_##_#___#___________________________________________
___________________________###____##_######_#__#___####____#________________________________________
____________________________#_##_#_##__##____####_#_##_####_#_______________________________________
____________________________#___#######_######__#_##_#_#____#_______________________________________
___________________________#___#___##_#__#___##_######__#__#________________________________________
___________________________#_##____###__#___#_#######_#__##_________________________________________
____________________________##_#_##___#_#_#_##_#__##__###___________________________________________
_____________________________######_##__________#####___#___________________________________________
_________________________________#___#__#####_#______#_#____________________________________________
__________________________________##_____#_#_##___#____#____________________________________________
_______________________________##_#__##_#_____##_#____###___________________________________________
_______________________________#_##_#______#_#___#____###___________________________________________
________________________________#___####_##___#___#____#____________________________________________
________________________________###__#___#___###___####_____________________________________________
________________________________#___##__##_##___#___________________________________________________
___________________________________##__##__#___###__________________________________________________
____________________________________##__#_##_##___#_________________________________________________
_________________________________________##__#___###________________________________________________
__________________________________________#_##_##___#_______________________________________________
___________________________________________##__#___###______________________________________________
____________________________________________#_##_##___#_____________________________________________
_____________________________________________##__#___###____________________________________________
______________________________________________#_##_##___#___________________________________________
_______________________________________________##__#___###__________________________________________
________________________________________________#_##_##___#_________________________________________
_________________________________________________##__#___###________________________________________
__________________________________________________#_##_##___#_______________________________________
___________________________________________________##__#___###______________________________________
____________________________________________________#_##_##___#_____________________________________
_____________________________________________________##__#___###____________________________________
______________________________________________________#_##_##___#___________________________________
_______________________________________________________##__#___###__________________________________
________________________________________________________#_##_##___#_________________________________
_________________________________________________________##__#___###________________________________
__________________________________________________________#_##_##___#_______________________________
___________________________________________________________##__#___###______________________________
____________________________________________________________#_##_##___#_____________________________
_____________________________________________________________##__#___###____________________________
______________________________________________________________#_##_##___#___________________________
_______________________________________________________________##__#___###__________________________
________________________________________________________________#_##_##___#_________________________
_________________________________________________________________##__#___###________________________
__________________________________________________________________#_##_##___#_______________________
___________________________________________________________________##__#_#####______________________
____________________________________________________________________#_#___####______________________
_____________________________________________________________________##_###_#_______________________
______________________________________________________________________#___##________________________

```



## Euphoria

{{works with|Euphoria|4.0.3, 4.0.0 RC1 and later}}

```euphoria
include std\console.e
include std\graphics.e

sequence grid = repeat(repeat(1,100),100) --fill 100 by 100 grid with white (1)
sequence antData = {48, 53, 360} --ant x coordinate, y coordinate, facing angle
integer iterations = 0

--while ant isn't out of bounds of the 100 by 100 area..
while antData[1] > 0 and antData[1] < 100 and antData[2] > 0 and antData[2] < 100 do
    switch grid[antData[1]][antData[2]] do
        case 1 then--cell is already white
            grid[antData[1]][antData[2]] = 0 --cell turns black, ant turns right
            antData[3] += 90
            break
        case 0 then--cell is already black
            grid[antData[1]][antData[2]] = 1 --cell turns white, ant turns left
            antData[3] -= 90
            break
    end switch
    --wrap ant directions if > 360 or < 90 (by 90)
    switch antData[3] do
        case 450 then
            antData[3] = 90
            break
        case 0 then
            antData[3] = 360
            break
    end switch  
    --move ant based on its new facing, one square
    --first north, then south, east, west
    switch antData[3] do
        case 360 then
            antData[2] -= 1
            break
        case 180 then
            antData[2] += 1
            break
        case 90 then
            antData[1] += 1
            break
        case 270 then
            antData[1] -= 1
            break
    end switch
iterations += 1
end while

wrap(0) --don't wrap text output, the grid wouldnt display as a square

for y=1 to 100 do
    printf(1,"\n")
    for x=1 to 100 do
        switch grid[x][y] do--each grid block , based on color
            case 0 then
                printf(1,".")
                break
            case 1 then
                printf(1,"#")
                break
        end switch
    end for
end for     

printf(1,"\n%d Iterations\n",iterations)
any_key()--wait for keypress, put default message 'press any key..'
```
[[File:LangtonsAnt_Euphoria_SDL.png|right|thumb|SDL output]]
Code needed to run SDL example with Mark Akita's SDL_gfx_Test1.exw (as template) included with his SDL_gfx package from rapideuphoria.com's archive -
In initialization section :
```euphoria

sequence grid = repeat(repeat(1,100),100) --fill 100 by 100 grid with white (1)
sequence antData = {48, 53, 360} --x coordinate, y coordinate, facing angle
```

In main() , after keystate=SDL_GetKeyState(NULL) , you can adapt the program above to draw the ant's step each frame.
Use dummy=pixelColor(surface,x+20,y+12,#000000FF) (for example) to replace the text output.
Just before the close of the while loop, use dummy=pixelColor(surface,antData[1]+20,antData[2]+12,#FF0000FF) for the ant
and SDL_UpdateRect(surface,0,0,0,0) to display the graphic.


## Fantom



```fantom

class World
{
  Int height
  Int width
  Bool[] state

  new make (Int height, Int width)
  {
    this.height = height
    this.width = width
    state = List(Bool#, height * width)
    (height*width).times { state.add (false) }
  }

  Bool inWorld (Int x, Int y)
  {
    x >= 0 && x < width && y >= 0 && y < height
  }

  Void show ()
  {
    height.times |h|
    {
      width.times |w|
      {
        Env.cur.out.writeChar (state[w*width+h] ? '#' : '.')
      }
      Env.cur.out.writeChar ('\n')
    }
  }

  Void flip (Int x, Int y)
  {
    state[x*width + y] = !state[x*width + y]
  }

  Bool stateOf (Int x, Int y)
  {
    state[x*width + y]
  }
}

enum class Direction 
{ 
  up (0, -1), 
  down (0, 1), 
  left (-1, 0), 
  right (1, 0)

  private new make (Int deltaX, Int deltaY)
  {
    this.deltaX = deltaX
    this.deltaY = deltaY
  }

  Direction rotateLeft ()
  {
    if (this == up) return left
    if (this == down) return right
    if (this == left) return down
    // if (this == right) 
    return up
  }

  Direction rotateRight ()
  {
    if (this == up) return right
    if (this == down) return left
    if (this == left) return up
    // if (this == right) 
    return down
  }

  const Int deltaX
  const Int deltaY
}

class Ant
{
  World world
  Int currX
  Int currY
  Direction direction

  new make (World world, Int x, Int y)
  {
    this.world = world
    currX = x
    currY = y
    direction = Direction.up
  }

  Bool inWorld ()
  {
    world.inWorld (currX, currY)
  }

  // the ant movement rules
  Void move ()
  {
    if (world.stateOf (currX, currY))
    {
      direction = direction.rotateLeft
    }
    else
    {
      direction = direction.rotateRight
    }
    world.flip (currX, currY)
    currX += direction.deltaX
    currY += direction.deltaY
  }
}

class Main
{
  Void main ()
  {
    world := World (100, 100)
    ant := Ant (world, 50, 50)
    numIterations := 0
    while (ant.inWorld)
    {
      ant.move
      numIterations += 1
    }
    world.show
    echo ("Finished in $numIterations iterations")
  }
}

```


Output (snipping the blank lines):

```txt

..........................................##..############..##......................................
.........................................#..####..........#..##.....................................
........................................###...##............##.#....................................
........................................#.#..#.........#..#....#....................................
....................................##..##.#.#.........###.......#..................................
.................................###.#..#...#.....#.....##.##..###..................................
..................................#.#..###..##.####.##...#.#..#.##..##..............................
..................................#.###.##..#.##..###.#.#.....###...###.............................
................................#.....#...#####.#.#..####..#...###.#.#.#............................
...............................###.##...#.####..##.##.######.#.###.#...#............................
...............................#.###.#.##.#.#.##.##.##.#...#####.###.##.............................
...................................#.#...#.##.###...#...#.#..####....#.##...........................
................................#..#.........##.##...#..##.....##.#.....##..........................
...............................###...#.#.##.###..#..##.....#...###.##..##.#.........................
..............................#..###..##...##.##...###..#....#..##.####...#.........................
.............................###...#...#.#..#.#.####.##..#.##.###..#.....#..........................
............................#..###..#.##....#..#.###..#......###.##.#..#..##........................
...........................###...#.....#.##.#.##..##..#####.####..####.##...#.......................
..........................#..###..#.#.#..#.###.#.#.##......##...#.#.#....#...#......................
.........................###...#..##.###..##.#...##.......####.####...#......#......................
........................#..###..#.#..#...##..###########.#..####..#....#....#.......................
.......................###...#..##......#.####..##..#########..#..##....#..##.......................
......................#..###..#.#...##..#.##...##.##.###.###...#..#.##..####.#......................
.....................###...#..##...#..#.######.##.#.##.#.#....###.###...##...#......................
....................#..###..#.#...#.....#####.#.#####.....#.#..##.#....##...#.......................
...................###...#..##....#.....#.##.#####.##..#.#...#..#..##.#..#..#.......................
..................#..###..#.#.....#....#...####.#..#####.##...##########...##.......................
.................###...#..##......#.##...##...#..#...####..#...##.####.##...........................
................#..###..#.#........#####.#..##...##.#...#....#.#..#..#..#.#.........................
...............###...#..##..........##..##.#.#.#....##.##.#.#.##..#..##..##.........................
..............#..###..#.#.................#..#....#.########.#.#.##..####.#.........................
.............###...#..##..................#..#...#.......##.##...#..#..##.#.........................
............#..###..#.#....................#..#..#......#..##..##...##.####.........................
...........###...#..##......................##...#.......##..##....#...#.###........................
..........#..###..#.#............................#.##..####....####.###.####........................
.........###...#..##..............................##..####....##..#.##.#.#..#.......................
........#..###..#.#................................##....##....##.###.##.#####......................
.......###...#..##................................................#.##.#..####......................
......#..###..#.#.....................................................##.##.##......................
.....###...#..##......................................................##............................
....#..###..#.#.....................................................#.##..####.#....................
...###...#..##.....................................................#..#.###..###....................
..#..###..#.#......................................................#.##.#..#..#.....................
.###...#..##........................................................##......##......................
#..###..#.#..........................................................##.............................
.###.#..##..........................................................................................
#.#.#.#.#...........................................................................................
.####.##............................................................................................
.#.##.#.............................................................................................
..####..............................................................................................
...##...............................................................................................
Finished in 11669 iterations

```



## Forth

{{works with|GNU Forth|0.7.0}}
All array manipulations were taken from Rosetta Code examples.

```forth

 1 0 0 0                                                          \ pushes orientation of the ant to the stack. 

 100 CONSTANT border                                              \ lenght of the side of the grid 
 border border * constant size                                    \ size of the grid

 variable antpos                                                  \ for storing position of the ant
 size 2 / border 2 / +  antpos !                                  \ positions ant in the middle of the grid

 create Grid size cells allot
 here constant GridEnd                                            \ creates an array to hold the grid

 : turn.left 
	>r rot r> SWAP ;                                          \ rotates ant anti-clockwise

 : turn.right 
	turn.left turn.left turn.left ;                           \ rotates ant clockwise

 : stop.ant 
	antpos @ DUP 0<  SWAP size > + ;                          \ checks if ant not out of bounds

 : call.pos 
	Grid antpos @ cells + @ ;                                 \ pushes ant position to the stack

 : grid.add 
	Grid antpos @ cells + @ -1 + Grid antpos @ cells + !   ;  \ pushes -1 to the current position of the ant on the grid
	
 : swap.pos 
	call.pos dup * Grid antpos @ cells + ! ;                  \ multiplies current grid cell by itself to turn -1 into 1

 : swap.col 
	grid.add swap.pos ;                                       \ changes current grid cell color

 : go.ant                                                         \ moves ant one step in the direction taken from the stack
	2over 2over                                               \ copies stack for testing
	1 = IF antpos @ border + antpos ! 2DROP DROP ELSE         \ if true moves ant one cell up, drops unused numbers from stack
	1 = IF antpos @ 1 + antpos ! 2DROP ELSE                   \ same, but moves to the right
	1 = IF antpos @ border - antpos ! DROP ELSE               \ here to the left
	1 = IF antpos @ 1 - antpos ! ELSE                         \ and down

	THEN THEN THEN THEN  ;                                
	
 : step.ant                                                       \ preforms one full step.
	 call.pos 1 = IF turn.left swap.col ELSE
	 turn.right swap.col 
	 
	 THEN go.ant  ;
	 
 : run.ant                                                        \ runs the ant until it leaves the grid                                                                                                  
	BEGIN
	step.ant 
	stop.ant UNTIL ;
	
 : square.draw                                                     \ draws an "*" if grid cell is one or " " if zero
	1 = IF 42 EMIT ELSE 32 EMIT THEN ;
		
	
 : draw.grid                                                       \ draws grid on screen
	PAGE                                                       \ clear sreen 
	size 0 DO I
	I border MOD 0= IF  CR  THEN                               \ breaks the grid into lines
	Grid I cells + @ square.draw DROP
	
	LOOP ; 
	
 : langton.ant run.ant draw.grid ;                                 \ launches the ant, outputs the result


```
	
{{out}} 
<pre style="height:60ex;overflow:scroll">

                                                                    **
                                                                     **
                                              **  **            *** ** *
                                             *  **  ***        **** *  *
                                            *    **   *      ** **  * *
                                         ** *     *     ******* *** **
                                        *  *  *   ** *   *  ***** *  *
                                       ***   *   ****  ** *** *      **
                                    ** * *  **  *   * **  **** ******
                                   *  *   *    *     **  **   * * ***** *
                                  *** **  *  *    *   ***** * ** *    ***
                                  **     ** ***   ** ***   ** ****  *  *
                                    ***  *** *   * * ***     *  *    **
                                      * *   ********* *****   ****
                                *** ***  ** *  *      *   **  *
                                 ***** *****  **   *****    * * *
                              ** * ******* *** ****** ***** * ***
                             *    **  **   ***  *  *  *  *   * *
                            ***      ***  * ** **    *  * ** *
                            * *  *    *    * *****  *    * **
                                 ** ***  * *  ** **    *  * *
                             * *     * *     * * ** * *  *** ** *
                            *   ***  *  *  * *  **** **   ** ****
                            *   ** *** ** *  *   *     **** * **
                            *  **   ***      *  **** *** **   **
                            *      ***     ***  ***   **  *    *
                            *     * * * ****    ****  ** **
                            *     ** *** **   * ** **** ***   * *
                            *      * *    ***** * * ***       ***
                            *    **** *    *** ** ***   * *    *
                            *     *  ** ** ** ****  ** **  ****
                            *     * ** **  *    *******
                            ***   **  **  * *** * ** *   *
                            ***    ** ****** *  *   ****    *
                             * ** * **  **    **** * ** **** *
                             *   ******* ******  * ** * *    *
                            *   *   ** *  *   ** ******  *  *
                            * **    ***  *   * ******* *  **
                             ** * **   * * * ** *  **  ***
                              ****** **          *****   *
                                  *   *  ***** *      * *
                                   **     * * **   *    *
                                ** *  ** *     ** *    ***
                                * ** *      * *   *    ***
                                 *   **** **   *   *    *
                                 ***  *   *   ***   ****
                                 *   **  ** **   *
                                    **  **  *   ***
                                     **  * ** **   *
                                          **  *   ***
                                           * ** **   *
                                            **  *   ***
                                             * ** **   *
                                              **  *   ***
                                               * ** **   *
                                                **  *   ***
                                                 * ** **   *
                                                  **  *   ***
                                                   * ** **   *
                                                    **  *   ***
                                                     * ** **   *
                                                      **  *   ***
                                                       * ** **   *
                                                        **  *   ***
                                                         * ** **   *
                                                          **  *   ***
                                                           * ** **   *
                                                            **  *   ***
                                                             * ** **   *
                                                              **  *   ***
                                                               * ** **   *
                                                                **  *   ***
                                                                 * ** **   *
                                                                  **  *   ***
                                                                   * ** **   *
                                                                    **  * *****
                                                                     * *   ****
                                                                      ** *** *
                                                                       * * **                        ok

```



## Fortran

{{works with|Fortran|90 and later}}

```fortran
program Langtons_Ant
  implicit none

  integer, parameter :: csize = 100
  integer :: direction = 0, maxsteps = 20000
  integer :: i, x, y  
  logical :: cells(csize,csize) = .true.
  logical :: cflag
  
  x = csize / 2;   y = x
  
  do i = 1, maxsteps
    cflag = cells(x,y)
    if(cflag) then
      direction = direction + 1
      if(direction == 4) direction = direction - 4
    else
      direction = direction - 1
      if(direction == -1) direction = direction + 4
    end if
  
    cells(x,y) = .not. cells(x,y)

    select case(direction)
      case(0)
        y = y - 1
      case(1)
        x = x + 1
      case(2)
        y = y + 1
      case(3)
        x = x - 1
    end select

    if(x < 1 .or. x > csize .or. y < 1 .or. y > csize) exit
  end do
  
  do y = 1, csize
    do x = 1, csize
      if(cells(x,y)) then
        write(*, "(a)", advance="no") "."
      else
        write(*, "(a)", advance="no") "#"
      end if
    end do
    write(*,*)
  end do
end program
```

{{out}} (Cropped to save space)
<pre style="height:60ex;overflow:scroll">
...................................................................................
...................................................................................
.........................................##..############..##......................
........................................#..####..........#..##.....................
.......................................###...##............##.#....................
.......................................#.#..#.........#..#....#....................
...................................##..##.#.#.........###.......#..................
................................###.#..#...#.....#.....##.##..###..................
.................................#.#..###..##.####.##...#.#..#.##..##..............
.................................#.###.##..#.##..###.#.#.....###...###.............
...............................#.....#...#####.#.#..####..#...###.#.#.#............
..............................###.##...#.####..##.##.######.#.###.#...#............
..............................#.###.#.##.#.#.##.##.##.#...#####.###.##.............
..................................#.#...#.##.###...#...#.#..####....#.##...........
...............................#..#.........##.##...#..##.....##.#.....##..........
..............................###...#.#.##.###..#..##.....#...###.##..##.#.........
.............................#..###..##...##.##...###..#....#..##.####...#.........
............................###...#...#.#..#.#.####.##..#.##.###..#.....#..........
...........................#..###..#.##....#..#.###..#......###.##.#..#..##........
..........................###...#.....#.##.#.##..##..#####.####..####.##...#.......
.........................#..###..#.#.#..#.###.#.#.##......##...#.#.#....#...#......
........................###...#..##.###..##.#...##.......####.####...#......#......
.......................#..###..#.#..#...##..###########.#..####..#....#....#.......
......................###...#..##......#.####..##..#########..#..##....#..##.......
.....................#..###..#.#...##..#.##...##.##.###.###...#..#.##..####.#......
....................###...#..##...#..#.######.##.#.##.#.#....###.###...##...#......
...................#..###..#.#...#.....#####.#.#####.....#.#..##.#....##...#.......
..................###...#..##....#.....#.##.#####.##..#.#...#..#..##.#..#..#.......
.................#..###..#.#.....#....#...####.#..#####.##...##########...##.......
................###...#..##......#.##...##...#..#...####..#...##.####.##...........
...............#..###..#.#........#####.#..##...##.#...#....#.#..#..#..#.#.........
..............###...#..##..........##..##.#.#.#....##.##.#.#.##..#..##..##.........
.............#..###..#.#.................#..#....#.########.#.#.##..####.#.........
............###...#..##..................#..#...#.......##.##...#..#..##.#.........
...........#..###..#.#....................#..#..#......#..##..##...##.####.........
..........###...#..##......................##...#.......##..##....#...#.###........
.........#..###..#.#............................#.##..####....####.###.####........
........###...#..##..............................##..####....##..#.##.#.#..#.......
.......#..###..#.#................................##....##....##.###.##.#####......
......###...#..##................................................#.##.#..####......
.....#..###..#.#.....................................................##.##.##......
....###...#..##......................................................##............
...#..###..#.#.....................................................#.##..####.#....
..###...#..##.....................................................#..#.###..###....
.#..###..#.#......................................................#.##.#..#..#.....
###...#..##........................................................##......##......
...##..#.#..........................................................##.............
##..#..##..........................................................................
.#.#.#.#...........................................................................
####.##............................................................................
#.##.#.............................................................................
.####..............................................................................
..##...............................................................................
...................................................................................
...................................................................................
```


===But, if one remembers complex numbers===

```Fortran

      PROGRAM LANGTONSANT
C   Langton's ant wanders across an initially all-white board, stepping one cell at a go.
C   If the current cell is white, it becomes black and the ant turns right.
C   If the current cell is black, it becomes white and the ant turns left.
C   The ant advances one cell in its latest direction, and reconsiders.
      INTEGER ENUFF
      PARAMETER (ENUFF = 100)		!Said to be so.
      CHARACTER*1 CELL(ENUFF,ENUFF)	!The work area.
      COMPLEX WAY,PLACE		!A direction and a position.
      INTEGER X,Y,XN,Y1		!Integer versions.
      INTEGER STEP		!A counter.
      CELL = ""				!Clear for action.
      PLACE = CMPLX(ENUFF/2,ENUFF/2)	!Start at the middle.
      WAY = (1,0)		!Initial direction is +x.
Commence wandering.
      DO STEP = 1,20000	!Enough to be going on with.
        X = REAL(PLACE)		!Change languages.
        Y = AIMAG(PLACE)	!Could mess about with EQUIVALENCE...
        IF (X.LE.0 .OR. X.GT.ENUFF	!Are we still
     1  .OR.Y.LE.0 .OR. Y.GT.ENUFF) THEN!Within bounds?
          WRITE (6,1) STEP - 1,X,Y		!No! Offer details.
    1     FORMAT ("Step ",I0," to (",I0,",",I0,") is out of bounds!")
          EXIT					!And wander no further.
        END IF				!But, if we're within bounds,
        IF (CELL(X,Y).NE."#") THEN	!Consider our position.
          CELL(X,Y) = "#"		!A blank cell becomes black. Ish.
          WAY = WAY*(0,-1)		!Turn right.
         ELSE				!Otherwise,
          CELL(X,Y) = "+"		!A black cell becomes white. Ish.
          WAY = WAY*(0,+1)		!Turn left.
        END IF			!So much for changing direction.
        PLACE = PLACE + WAY	!Advance one step.
      END DO		!On to the next step.
Consider the bounds...
      DO Y1 = 1,ENUFF		!Work up from the bottom.
        IF (ANY(CELL(:,Y1).NE." ")) EXIT	!The last line with a splot.
      END DO			!Subsequent lines would be blank.
      DO XN = ENUFF,1,-1	!Work back from the right hand side.
        IF (ANY(CELL(XN,:).NE." ")) EXIT	!The last column with a splot.
      END DO			!Subsequent columns would be blank.
Cast forth the splotches.
      DO Y = ENUFF,Y1,-1	!The topmost y-coordinate first!
        WRITE (6,"(666A1)") CELL(1:XN,Y)	!Roll a line's worth.
      END DO			!On to the next line.
Completed.
      END

```


Output is the same, except for orientation. Here I have stuck to (x,y) Cartesian orientation rather than lines (y) increasing downwards. Just for fun, + signs mark cells that have been trampled and then cleaned. But not to pure white... Notice that some interior cells have never been trampled.
{{out}} 
<pre style="height:60ex;overflow:scroll">
Step 11669 to (26,101) is out of bounds!
                         #+#
                       ## #+#
                      #+###+##
                     ####+###+#
                     #####+#++##
                      #+++##+##+#
                       ###+++#++##
                        #+++##+##+#
                         ###+++#++##
                          #+++##+##+#
                           ###+++#++##
                            #+++##+##+#
                             ###+++#++##
                              #+++##+##+#
                               ###+++#++##
                                #+++##+##+#
                                 ###+++#++##
                                  #+++##+##+#
                                   ###+++#++##
                                    #+++##+##+#
                                     ###+++#++##
                                      #+++##+##+#
                                       ###+++#++##
                                        #+++##+##+#
                                         ###+++#++##
                                          #+++##+##+#
                                           ###+++#++##
                                            #+++##+##+#
                                             ###+++#++##
                                              #+++##+##+#
                                               ###+++#++##
                                                #+++##+##+#  ##
                                                 ###+++#++## +##
                                                  #+++##+##++##+++#
                                            ####   ###+++#+++#++###
                                           #++++#   #+++##+####+++#
                                          ###++++#   #+#++++++#+##+#
                                          ###++++# ##  +++#+##++#+##
                                           #++++#  +## #+#+++++##++
                                           #+#++++++#+#####++#+++#+
                                          #+++#####++++++++++##+######
                                          ###++##++#+##+#+#+#+++##+#+##
                                        ## +#+#######+#+++#++###++++##+#
                                       #++#++######+##+++#++#+## ++#+++#
                                      #++++#+#+##+#++######+#######+++#
                                      #+####+##+#+####++++##++##+#+##+#
                                       #++++####+++#++#+######+##++++###
                                          #+++#+##+#+###+#++##++##+++###
                                          +++#######++++#++##+##+#+++++#
                                     #### +##+##++####+##+##+##++#+++++#
                                    #++++#+#+++###+##+###++++#+####++++#
                                   ###+++++++###+#+#+#####++++#+#++++++#
                                   #+#+++###+####+##+#+++##+###+##+++++#
                                     ++++##+##++####++++####+#+#+#+++++#
                                    #++++#++##+++###++###+++++###++++++#
                                    ##+++##+###+####++#++++++###+++##++#
                                    ##+#+####+++++#+++#++#+##+###+##+++#
                                   ####+##+++##+####++#+#++#++#++###+++#
                                   #+##+###++#+#+##+#+#+++++#+#+++++#+#
                                     ++#+#++#++++##+##++#+#++###+##+++
                                     ++##+#++++#++#####+#++++#++++#++#+#
                                     +#+##+#++#++++##+##+#++###++++++###
                                    #+#+++#++#++#++#++###+++##++##++++#
                                   ###+#+#####+######+###+#######+#+##
                                   #+#+#++++#####+++##++#####+#####+
                                     #++##+++#++++++#++#+##++###+###
                                  ####+++#####+#########+++#+#+++
                             ##   +#++#+++++###+#+#+++#+###++###+
                            #++#  ####+##+++###+##+++###+##+++++##
                           ###++++#+##+#+#####+++#++++#++#++##+###
                           #+#####+#+#+++##++##++++ #++++#+  #++#
                             ++######+####++## #+++#+ ##++#+# ##
                             ##++++++#+###+##++####++ #+++###
                              #++#+#####++#+++#+##+++#+ #++#
                              ##+###+#######+++++#+++++# ##
                             #+#++##+##++++++#+++##++++#
                            #++#+####        ###++##++#
                            #+##+###            ##  ##
                             ##+
                              ##

```



## FreeBASIC


```freebasic
' version 16-10-2016
' compile with: fbc -s gui

' a cell size of 4 x 4 pixels is used
' In FreeBASIC the 0,0 is the top left corner

ScreenRes 400,400,8 ' give a 100 by 100 field
Dim As UByte Ptr p = ScreenPtr
If p = 0 Then End ' p does not point to screen

Palette 0,       0,   0,   0      ' index 0 = black
Palette 255,   255, 255, 255      ' index 225 = white

Line (0, 0) - (799, 799), 255, bf   ' draw box and fill it with white color

Dim As Integer count, offset, x = 199, y = 199
Dim As UByte col   ' = color
' direction, 0 = up, 1 = right, 2 = down, 3 = left
Dim As UByte d     ' d = 0, looking up

Do
  offset = x + y * 400
  col = p[offset]

  If col = 0 Then
    d = (d -1) And 3
  Else
    d = (d +1) And 3
  EndIf

  col = col Xor 255 ' flip the color

  ScreenLock        ' don't update screen while we are drawing

  ' draw a 4*4 block and paint it with palette color [0 | 255]
  Line (x, y) - (x +3, y -3), col, bf

  ScreenUnLock    ' allow screen update's

  'Sleep 100       ' slow the program down if needed

  ' true = 0, false = -1
  If (d And 1) = 1 Then
    x = x + (d = 1) * 4 - (d = 3) * 4
  Else
    y = y - (d = 0) * 4 + (d = 2) * 4
  End If

  count += 1
  ' update step count window title bar
  WindowTitle "Langton's ant step: " + Str(count)

  ' has user clicked on close window "X" then end program
  If InKey = Chr(255) + "k" Then End

Loop Until x < 1 Or x > 398 Or y < 1 Or y > 398

' display total count in window title bar
WindowTitle "Langton's ant has left the field in " + Str(count) + " steps"

' empty keyboard buffer
While InKey <> "" : Wend
'Print : Print "hit any key to end program"
Sleep
End
```



## Gambas


```gambas
'This code will create a GUI Form to display the result

hGridView As GridView                                               'The display is on a GridView
iCol As Integer = 38                                                'Column start position
iRow As Integer = 30                                                'Row start position

Public Sub Form_show()

SetUpForm                                                           'Run the SetUpForm routine
Go                                                                  'Run the Go routine

End

Public Sub Go()                                                     'This is what does the work
Dim siDir As Short = 3                                              'Stores the Direction of the ant 0 = North, 1 = East, 2 = South ,3 = West
Dim siCount As Short                                                'Counter

Repeat                                                              'Repeat loop
  Inc siCount                                                       'Increase siCount
  If hGridView[iRow, iCol].background = -1 Then                     'If the Background of the cell is white then..(Right turn)
    hGridView[iRow, iCol].background = 0                            'Make the Background black
    siDir = Direction(siDir, True)                                  'Get the direction to turn
    If siDir = 0 Then Dec iRow                                      'Decrease Row if facing North
    If siDir = 1 Then Inc iCol                                      'Increase Column if facing East
    If siDir = 2 Then Inc iRow                                      'Increase Row if facing South
    If siDir = 3 Then Dec iCol                                      'Decrease Column if facing West
  End If 
'Wait                                                                'This will allow you to see the Grid being populated. Rem it out for an instant result
  If hGridView[iRow, iCol].background = 0 Then                      'If the Background of the cell is black then.. Left Turn
    hGridView[iRow, iCol].background = -1                           'Make the Background white
    siDir = Direction(siDir, False)                                 'Get the direction to turn
    If siDir = 0 Then Dec iRow                                      'Decrease Row if facing North
    If siDir = 1 Then Inc iCol                                      'Increase Column if facing East
    If siDir = 2 Then Inc iRow                                      'Increase Row if facing South
    If siDir = 3 Then Dec iCol                                      'Decrease Column if facing West
  End If 
Until siCount = 9660                                                'Loop 9660 times

End

Public Sub Direction(siDirection As Short, bWay As Boolean) As Byte 'To workout which way to go

If bWay Then                                                        'If turning Right then
  Inc siDirection                                                   'Increase siDirection e.g. 0 = North to 1 = East 
Else                                                                'Else if turning Left
  Dec siDirection                                                   'Decrease siDirection e.g. 2 = South to 1 = East 
End If

If siDirection < 0 Then siDirection = 3                             'To address 0 - 1 = -1
If siDirection > 3 Then siDirection = 0                             'To address 3 + 1 = 4

Return siDirection                                                  'Return the correct direction

End

Public Sub SetUpForm()                                              'Set up the Form and Create the Gridview

With Me                                                             'Change the Properties of the Form
  .Height = 1012                                                    'Set the Form Height
  .Width = 1012                                                     'Set the Form Width
  .Arrangement = Arrange.Vertical                                   'Set the Form Arrangement
  .Padding = 5                                                      'Set the Form Padding (Border)
  .title = "Langton's ant"                                          'Set the Form Title
End With

hGridView = New GridView(Me)                                        'Create a GridView
With hGridView                                                      'Change the Properties of the GridView
  .Columns.count = 100                                              'Create 100 Columns
  .Rows.count = 100                                                 'Create 100 Rows
  .Columns.Width = 10                                               'Set the Column Width
  .Rows.Height = 10                                                 'Set the Column Height
  .expand = True                                                    'Set the Gridview to Expand to fill the Form
  .background = -1                                                  'Set the Gridview background to White
End With

End
```

'''[http://www.cogier.com/gambas/Langton's%20ant.png Click here for an image of the result]'''


## GFA Basic


To make it easier to see the output on small Atari screens, the output is written to a text file.


```basic

'
' Langton's ant
'
' World is a global boolean array, 100x100 in size
width%=100
height%=100
DIM world!(width%,height%)
ARRAYFILL world!(),FALSE
' Time in world
time%=0
' Ant is represented by a global three-element array
' holding: x, y, direction [0=north,1=west,2=south,3=east]
DIM ant%(3)
'
@setup_ant
@run_ant
@display_world
'
' Displays the world to file "langton.out": . for false, # for true
'
PROCEDURE display_world
  LOCAL i%,j%
  OPEN "o",#1,"langton.out"
  PRINT #1,"Time in world: ";time%;" ticks"
  FOR i%=0 TO width%-1
    FOR j%=0 TO height%-1
      IF world!(i%,j%)
        PRINT #1,"#";
      ELSE
        PRINT #1,".";
      ENDIF
    NEXT j%
    PRINT #1,""
  NEXT i%
  CLOSE #1
RETURN
'
' Set up the ant to start at (50,50) facing north
'
PROCEDURE setup_ant
  ant%(0)=50
  ant%(1)=50
  ant%(2)=0
RETURN
'
' check if ant position is within world's bounds
'
FUNCTION ant_in_world
  RETURN ant%(0)>=0 AND ant%(0)<width% AND ant%(1)>=0 AND ant%(1)<height%
ENDFUNC
'
' Turn ant direction to left
'
PROCEDURE ant_turn_left
  ant%(2)=(ant%(2)+1) MOD 4
RETURN
'
' Turn ant direction to right
'
PROCEDURE ant_turn_right
  ant%(2)=(ant%(2)+3) MOD 4
RETURN
'
' Ant takes a step forward in current direction
'
PROCEDURE ant_step_forward
  SELECT ant%(2)
  CASE 0
    ant%(0)=ant%(0)+1
  CASE 1
    ant%(1)=ant%(1)+1
  CASE 2
    ant%(0)=ant%(0)-1
  CASE 3
    ant%(1)=ant%(1)-1
  ENDSELECT
RETURN
'
' Run the ant until it falls out of the world
'
PROCEDURE run_ant
  WHILE @ant_in_world
    time%=time%+1
    IF world!(ant%(0),ant%(1)) ! true for white
      world!(ant%(0),ant%(1))=FALSE
      @ant_turn_left
    ELSE ! false for black
      world!(ant%(0),ant%(1))=TRUE
      @ant_turn_right
    ENDIF
    @ant_step_forward
  WEND
RETURN

```



## Go

[[file:GoAnt.png|right|thumb|Output png]]

```go
package main

import (
    "fmt"
    "image"
    "image/color"
    "image/draw"
    "image/png"
    "os"
)

const (
    up = iota
    rt
    dn
    lt
)

func main() {
    bounds := image.Rect(0, 0, 100, 100)
    im := image.NewGray(bounds)
    gBlack := color.Gray{0}
    gWhite := color.Gray{255}
    draw.Draw(im, bounds, image.NewUniform(gWhite), image.ZP, draw.Src)
    pos := image.Point{50, 50}
    dir := up
    for pos.In(bounds) {
        switch im.At(pos.X, pos.Y).(color.Gray).Y {
        case gBlack.Y:
            im.SetGray(pos.X, pos.Y, gWhite)
            dir--
        case gWhite.Y:
            im.SetGray(pos.X, pos.Y, gBlack)
            dir++
        }
        if dir&1 == 1 {
            pos.X += 1 - dir&2
        } else {
            pos.Y -= 1 - dir&2
        }
    }
    f, err := os.Create("ant.png")
    if err != nil {
        fmt.Println(err)
        return
    }
    if err = png.Encode(f, im); err != nil {
        fmt.Println(err)
    }
    if err = f.Close(); err != nil {
        fmt.Println(err)
    }
}
```



## Haskell

The set of black cells is represented as a set of points. Complementary set is regarded as white cells.

Necessary import:


```Haskell
import Data.Set (member,insert,delete,Set)
```


In order to express the ant's algorithm literally we define two operators:


```Haskell
-- functional sequence 
(>>>) = flip (.)

-- functional choice
p ?>> (f, g) = \x -> if p x then f x else g x
```


Finally define the datatype representing the state of ant and ant's universe

```Haskell
data State = State { antPosition :: Point
                   , antDirection :: Point
                   , getCells :: Set Point }

type Point = (Float, Float)
```


Now we are ready to express the main part of the algorithm

```Haskell
step :: State -> State
step = isBlack ?>> (setWhite >>> turnRight,
                    setBlack >>> turnLeft) >>> move
  where
    isBlack   (State p _     m) = member p m
    setBlack  (State p d     m) = State p d (insert p m)
    setWhite  (State p d     m) = State p d (delete p m)
    turnRight (State p (x,y) m) = State p (y,-x) m
    turnLeft  (State p (x,y) m) = State p (-y,x) m
    move (State (x,y) (dx,dy) m) = State (x+dx, y+dy) (dx, dy) m
```


That's it.

Here is the solution of the task:

```Haskell
task :: State -> State
task = iterate step 
   >>> dropWhile ((< 50) . distance . antPosition) 
   >>> getCells . head
  where distance (x,y) = max (abs x) (abs y)
```


For given initial configuration it returns the set of black cells at the end of iterations.

We can display it graphically using Gloss library

```haskell
import Graphics.Gloss

main = display w white (draw (task initial))
  where
    w = InWindow "Langton's Ant" (400,400) (0,0)
    initial = State (0,0) (1,0) mempty
    draw = foldMap drawCell
    drawCell (x,y) = Translate (10*x) (10*y) $ rectangleSolid 10 10
```


Or animate the ant's trajectory

```haskell
main = simulate w white 500 initial draw (\_ _ -> step)
  where
    w = InWindow "Langton's Ant" (400,400) (0,0)
    initial = State (0,0) (1,0) mempty
    draw (State p _ s) = pictures [foldMap drawCell s, color red $ drawCell p]
    drawCell (x,y) = Translate (10*x) (10*y) $ rectangleSolid 10 10
```


=={{header|Icon}} and {{header|Unicon}}==
[[file:LangtonsAnt_unicon_100x100_11655.gif|right|thumb]]

```Icon
link graphics,printf

procedure main(A) 
   e := ( 0 < integer(\A[1])) | 100   # 100 or whole number from command line
   LangtonsAnt(e)
end

record antrec(x,y,nesw)

procedure LangtonsAnt(e)
   size  := sprintf("size=%d,%d",e,e)
   label := sprintf("Langton's Ant %dx%d [%d]",e,e,0)
   &window := open(label,"g","bg=white",size) | 
               stop("Unable to open window") 

   ant := antrec(e/2,e/2,?4%4)
   board := list(e)
   every !board := list(e,"w")
   
   k := 0
   repeat {
      k +:= 1
      WAttrib("fg=red")
      DrawPoint(ant.x,ant.y)
      
      cell := board[ant.x,ant.y]
      if cell == "w" then {                        # white cell
         WAttrib("fg=black")
         ant.nesw := (ant.nesw + 1) % 4            # . turn right
         }
      else {                                       # black cell
         WAttrib( "fg=white")   
         ant.nesw := (ant.nesw + 3) % 4            # . turn left = 3 x right
         }
      board[ant.x,ant.y] := map(cell,"wb","bw")    # flip colour         
      DrawPoint(ant.x,ant.y)
      
      case ant.nesw of {                           # go
         0: ant.y -:= 1                            # . north
         1: ant.x +:= 1                            # . east
         2: ant.y +:= 1                            # . south
         3: ant.x -:= 1                            # . west
         }
         
      if 0 < ant.x <= e & 0 < ant.y <= e then next
      else break      
      }
   printf("Langton's Ant exited the field after %d rounds.\n",k)
   label := sprintf("label=Langton's Ant %dx%d [%d]",e,e,k)
   WAttrib(label)
   WDone()
end
```


{{libheader|Icon Programming Library}}  
[http://www.cs.arizona.edu/icon/library/src/procs/printf.icn printf.icn provides formatting] 
[http://www.cs.arizona.edu/icon/library/src/procs/graphics.icn graphics.icn provides graphics support (WDone)]


## J



```j
dirs=: 0 1,1 0,0 _1,:_1 0
langton=:3 :0
  loc=. <.-:$cells=. (_2{.y,y)$dir=. 0
  while. *./(0<:loc), loc<$cells do.
    color=. (<loc) { cells
    cells=. (-.color) (<loc)} cells
    dir=. 4 | dir +  _1 ^ color
    loc=. loc + dir { dirs
  end.
  ' #' {~ cells
)
```


<pre style="font-size: 2px">   langton 100 100
                          # #                                                                       
                        ## # #                                                                      
                       # ### ##                                                                     
                      #### ### #                                                                    
                      ##### #  ##                                                                   
                       #   ## ## #                                                                  
                        ###   #  ##                                                                 
                         #   ## ## #                                                                
                          ###   #  ##                                                               
                           #   ## ## #                                                              
                            ###   #  ##                                                             
                             #   ## ## #                                                            
                              ###   #  ##                                                           
                               #   ## ## #                                                          
                                ###   #  ##                                                         
                                 #   ## ## #                                                        
                                  ###   #  ##                                                       
                                   #   ## ## #                                                      
                                    ###   #  ##                                                     
                                     #   ## ## #                                                    
                                      ###   #  ##                                                   
                                       #   ## ## #                                                  
                                        ###   #  ##                                                 
                                         #   ## ## #                                                
                                          ###   #  ##                                               
                                           #   ## ## #                                              
                                            ###   #  ##                                             
                                             #   ## ## #                                            
                                              ###   #  ##                                           
                                               #   ## ## #                                          
                                                ###   #  ##                                         
                                                 #   ## ## #  ##                                    
                                                  ###   #  ##  ##                                   
                                                   #   ## ##  ##   #                                
                                             ####   ###   #   #  ###                                
                                            #    #   #   ## ####   #                                
                                           ###    #   # #      # ## #                               
                                           ###    # ##     # ##  # ##                               
                                            #    #   ## # #     ##                                  
                                            # #      # #####  #   #                                 
                                           #   #####          ## ######                             
                                           ###  ##  # ## # # #   ## # ##                            
                                         ##  # ####### #   #  ###    ## #                           
                                        #  #  ###### ##   #  # ##   #   #                           
                                       #    # # ## #  ###### #######   #                            
                                       # #### ## # ####    ##  ## # ## #                            
                                        #    ####   #  # ###### ##    ###                           
                                           #   # ## # ### #  ##  ##   ###                           
                                              #######    #  ## ## #     #                           
                                      ####  ## ##  #### ## ## ##  #     #                           
                                     #    # #   ### ## ###    # ####    #                           
                                    ###       ### # # #####    # #      #                           
                                    # #   ### #### ## #   ## ### ##     #                           
                                          ## ##  ####    #### # # #     #                           
                                     #    #  ##   ###  ###     ###      #                           
                                     ##   ## ### ####  #      ###   ##  #                           
                                     ## # ####     #   #  # ## ### ##   #                           
                                    #### ##   ## ####  # #  #  #  ###   #                           
                                    # ## ###  # # ## # #     # #     # #                            
                                        # #  #    ## ##  # #  ### ##                                
                                        ## #    #  ##### #    #    #  # #                           
                                       # ## #  #    ## ## #  ###      ###                           
                                     # #   #  #  #  #  ###   ##  ##    #                            
                                    ### # ##### ###### ### ####### # ##                             
                                    # # #    #####   ##  ##### #####                                
                                      #  ##   #      #  # ##  ### ###                               
                                   ####   ##### #########   # #                                     
                              ##    #  #     ### # #   # ###  ###                                   
                             #  #  #### ##   ### ##   ### ##     ##                                 
                            ###    # ## # #####   #    #  #  ## ###                                 
                            # ##### # #   ##  ##     #    #   #  #                                  
                                ###### ####  ## #   #  ##  # # ##                                   
                              ##      # ### ##  ####   #   ###                                      
                               #  # #####  #   # ##   #  #  #                                       
                               ## ### #######     #     # ##                                        
                              # #  ## ##      #   ##    #                                           
                             #  # ####        ###  ##  #                                            
                             # ## ###            ##  ##                                             
                              ##                                                                    
                               ##                                                                   
                                                                                                    
                                                                                                    
                                                                                                    
                                                                                                    
                                                                                                    
                                                                                                    
                                                                                                    
                                                                                                    
                                                                                                    
                                                                                                    
                                                                                                    
                                                                                                    
                                                                                                    
                                                                                                    
                                                                                                    
                                                                                                    
                                                                                                    
                                                                                                    
                                                                                                    
                                                                                                   
```


## Java

This implementation allows for sizes other than 100x100, marks the starting position with a green box (sometimes hard to see at smaller zoom levels and the box is smaller than the "pixels" so it doesn't cover up the color of the "pixel" it's in), and includes a "zoom factor" (<code>ZOOM</code>) in case the individual "pixels" are hard to see on your monitor.

```java
import java.awt.Color;
import java.awt.Graphics;

import javax.swing.JFrame;
import javax.swing.JPanel;

public class Langton extends JFrame{
	private JPanel planePanel;
	private static final int ZOOM = 4;
	
	public Langton(final boolean[][] plane){
		planePanel = new JPanel(){
			@Override
			public void paint(Graphics g) {
				for(int y = 0; y < plane.length;y++){
					for(int x = 0; x < plane[0].length;x++){
						g.setColor(plane[y][x] ? Color.BLACK : Color.WHITE);
						g.fillRect(x * ZOOM, y * ZOOM, ZOOM, ZOOM);
					}
				}
				//mark the starting point
				g.setColor(Color.GREEN);
				g.fillRect(plane[0].length / 2 * ZOOM,
				           plane.length / 2 * ZOOM, ZOOM/2, ZOOM/2);
			}
		};
		planePanel.setSize(plane[0].length - 1, plane.length - 1);
		add(planePanel);
		setSize(ZOOM * plane[0].length, ZOOM * plane.length + 30);
		setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		setVisible(true);
	}
	
	public static void main(String[] args){
		new Langton(runAnt(100, 100));
	}

	private static boolean[][] runAnt(int height, int width){
		boolean[][] plane = new boolean[height][width];
		int antX = width/2, antY = height/2;//start in the middle-ish
		int xChange = 0, yChange = -1; //start moving up
		while(antX < width && antY < height && antX >= 0 && antY >= 0){
			if(plane[antY][antX]){
				//turn left
				if(xChange == 0){ //if moving up or down
					xChange = yChange;
					yChange = 0;
				}else{ //if moving left or right
					yChange = -xChange;
					xChange = 0;
				}
			}else{
				//turn right
				if(xChange == 0){ //if moving up or down
					xChange = -yChange;
					yChange = 0;
				}else{ //if moving left or right
					yChange = xChange;
					xChange = 0;
				}
			}
			plane[antY][antX] = !plane[antY][antX];
			antX += xChange;
			antY += yChange;
		}
		return plane;
	}
}
```

Output (click for a larger view):

[[Image:Langton Java.png|250px]]


## JavaScript


Utilises the HTML5 canvas element to procedurally generate the image... I wanted to see the progress of the grid state as it was generated, so this implementation produces a incrementally changing image until an 'ant' hits a cell outside of the coordinate system. It can also accept multiple ants, this adds minimal complexity with only the addition of an 'ants' array which is iterated in each step, no additional conditions are necessary to simulate multiple ants, they coexist quite well... good ants ! 1st argument is an array of ant objects, 2nd argument is an object property list of options to change grid size, pixel size and interval (animation speed).


```JavaScript

// create global canvas
var canvas = document.createElement('canvas');
canvas.id = 'globalCanvas';
document.body.appendChild(canvas);

function langtonant(antx, optx) {
	'use strict';
	var x, y, i;

	// extend default opts
	var opts = {
		gridsize: 100,
		pixlsize: 4,
		interval: 4
	};
	for (i in optx) {
		opts[i] = optx[i];
	}

	// extend default ants
	var ants = [{
		x: 50,
		y: 50,
		d: 0
	}];
	for (i in antx) {
		ants[i] = antx[i];
	}

	// initialise grid
	var grid = [];
	for (x = 0; x < opts.gridsize; x ++) {
		grid[x] = [];
		for (y = 0; y < opts.gridsize; y ++) {
			grid[x][y] = true;
		}
	}

	// initialise directions
	var dirs = [
		{x: 1, y: 0},
		{x: 0, y: -1},
		{x: -1, y: 0},
		{x: 0, y: 1}
	];

	// initialise canvas
	var canv = document.getElementById('globalCanvas');
	var cont = canv.getContext('2d');
	canv.width = opts.gridsize * opts.pixlsize;
	canv.height = opts.gridsize * opts.pixlsize;

	// initialise pixels
	var pixlblac = cont.createImageData(opts.pixlsize, opts.pixlsize);
	for (i = 0; i < (opts.pixlsize * opts.pixlsize * 4); i += 4) {
		pixlblac.data[i + 3] = 255;
	}
	var pixlwhit = cont.createImageData(opts.pixlsize, opts.pixlsize);
	for (i = 0; i < (opts.pixlsize * opts.pixlsize * 4); i += 4) {
		pixlwhit.data[i + 3] = 0;
	}

	// run simulation
	function simulate() {
		var sane = true;

		// iterate over ants
		for (i = 0; i < ants.length; i ++) {
			var n = ants[i];

			// invert, draw, turn
			if (grid[n.x][n.y]) {
				grid[n.x][n.y] = false;
				cont.putImageData(pixlblac, n.x * opts.pixlsize, n.y * opts.pixlsize);
				n.d --;
			} else {
				grid[n.x][n.y] = true;
				cont.putImageData(pixlwhit, n.x * opts.pixlsize, n.y * opts.pixlsize);
				n.d ++;
			}

			// modulus wraparound
			n.d += dirs.length;
			n.d %= dirs.length;

			// position + direction
			n.x += dirs[n.d].x;
			n.y += dirs[n.d].y;

			// sanity check
			sane = (n.x < 0 || n.x > opts.gridsize || n.y < 0 || n.y > opts.gridsize) ? false : sane;
		}

		// loop with interval
		if (sane) {
			setTimeout(simulate, opts.interval);
		}
	}

	simulate();
}

```


Usage: default ants, custom opts


```JavaScript

langtonant({}, {
	gridsize: 100,
	pixlsize: 4,
	interval: 4
});

```


{{out}}

[http://jsbin.com/ocuwal/1/edit Live Version]

[[Image:Langtons Ant - JavaScript 1.png]]

Usage: custom ants, default opts


```JavaScript

langtonant([
	{
		x: (100 / 2) + 7,
		y: (100 / 2) + 7,
		d: 1
	}, {
		x: (100 / 2) + 7,
		y: (100 / 2) - 7,
		d: 2
	}, {
		x: (100 / 2) - 7,
		y: (100 / 2) - 7,
		d: 3
	}, {
		x: (100 / 2) - 7,
		y: (100 / 2) + 7,
		d: 0
	}
]);

```


{{out}}

[http://jsbin.com/ahumuz/1/edit Live Version]

[[Image:Langtons Ant - JavaScript 2.png]]

'''More functional approach to Javascript.'''

Requires lodash. Wants a canvas with id = "c"


```javascript

///////////////////
// LODASH IMPORT //
///////////////////

// import all lodash functions to the main namespace, but isNaN not to cause conflicts
_.each(_.keys(_), k => window[k === 'isNaN' ? '_isNaN' : k] = _[k]);

const
WORLD_WIDTH  = 100,
WORLD_HEIGHT = 100,
PIXEL_SIZE   = 4,
DIRTY_COLOR  = '#000',
VIRGIN_COLOR = '#fff',
RUNS         = 10000,
SPEED        = 50,

//            up right down left
DIRECTIONS = [0, 1,    2,    3],

displayWorld = (world) => each(world, (row, rowidx) => {
  each(row, (cell, cellidx) => {
    canvas.fillStyle = cell === 1 ? DIRTY_COLOR : VIRGIN_COLOR;
    canvas.fillRect(rowidx * PIXEL_SIZE, cellidx * PIXEL_SIZE, PIXEL_SIZE, PIXEL_SIZE);
  });
}),

moveAnt = (world, ant) => {
  world[ant.x][ant.y] = world[ant.x][ant.y] === 1 ? 0 : 1;
  ant.dir             = DIRECTIONS[(4 + ant.dir + (world[ant.x][ant.y] === 0 ? 1 : -1)) % 4];
  switch (ant.dir) {
    case DIRECTIONS[0]:
      ant.y -= 1;
      break;
    case DIRECTIONS[1]:
      ant.x -= 1;
      break;
    case DIRECTIONS[2]:
      ant.y += 1;
      break;
    case DIRECTIONS[3]:
      ant.x += 1;
      break;
  }

  return [world, ant];
},

updateWorld = (world, ant, runs) => {
  [world, ant] = moveAnt(world, ant);
  displayWorld(world);

  if (runs > 0) setTimeout(partial(updateWorld, world, ant, --runs), SPEED);
},

canvas = document.getElementById('c').getContext('2d');

let
world = map(range(WORLD_HEIGHT), i => map(range(WORLD_WIDTH), partial(identity, 0))),
ant   = {
  x:   WORLD_WIDTH  / 2,
  y:   WORLD_HEIGHT / 2,
  dir: DIRECTIONS[0]
};

canvas.canvas.width  = WORLD_WIDTH  * PIXEL_SIZE;
canvas.canvas.height = WORLD_HEIGHT * PIXEL_SIZE;

updateWorld(world, ant, RUNS);

```



## jq

In the following, the grid is boolean, and white is represented by true. 

```jq

def matrix(m; n; init):  
  if m == 0 then [range(0;n)] | map(init)
  elif m > 0 then [range(0;m)][ range(0;m) ] = matrix(0;n;init) 
  else error("matrix\(m);_;_) invalid")
  end;

def printout:
  . as $grid
  | ($grid|length) as $height
  | ($grid[0]|length) as $width
  | reduce range(0;$height) as $i ("\u001BH";
    . + reduce range(0;$width) as $j ("\n";
         . + if $grid[$i][$j] then " " else "#" end ) );


def langtons_ant(grid_size):

  def flip(ant):
    # Flip the color of the current square
    .[ant[0]][ant[1]] = (.[ant[0]][ant[1]] | not) 
  ;

  # input/output: the ant's state: [x, y, direction]
  # where direction is one of (0,1,2,3)
  def move(grid):
    # If the cell is black, it changes to white and the ant turns left;
    # If the cell is white, it changes to black and the ant turns right;
    (if grid[.[0]][.[1]] then 1 else 3 end) as $turn
    | .[2] = ((.[2] + $turn) % 4)
    | if   .[2] == 0 then .[0] += 1
      elif .[2] == 1 then .[1] += 1
      elif .[2] == 2 then .[0] += -1
      else                .[1] += -1
      end
  ;

  # state: [ant, grid]
  def iterate:
    .[0] as $ant | .[1] as $grid
    # exit if the ant is outside the grid
    | if $ant[0] < 1 or $ant[0] > grid_size 
      or $ant[1] < 1 or $ant[1] > grid_size
      then [ $ant, $grid ]
      else
        ($grid | flip($ant)) as $grid
        | ($ant | move($grid)) as $ant
        | [$ant, $grid] | iterate
      end
  ;

  ((grid_size/2) | floor | [ ., ., 0]) as $ant
  | matrix(grid_size; grid_size; true) as $grid 
  | [$ant, $grid] | iterate
  | .[1]
  | printout
;
 
langtons_ant(100)
```

{{Out}}
The output is the same as for [[#Rexx|Rexx]] below.


## Julia

{{works with|Julia|1.0}}

```Julia

function ant(width, height)
    y, x = fld(height, 2), fld(width, 2)
    M = falses(height, width)

    dir = im
    for i in 0:100000
        x in 1:width && y in 1:height || break
        dir *= M[y, x] ? im : -im
        M[y, x] = !M[y, x]
        x, y = reim(x + im * y + dir)
    end

    for row in 1:size(M,1)
        println(mapreduce(x -> x ? 'x' : '.', *, M[row,:]))
    end
end

ant(100, 100)

```



## Kotlin

{{trans|D}}

```scala
// version 1.2.0

enum class Direction { UP, RIGHT, DOWN, LEFT }

const val WHITE = 0
const val BLACK = 1

fun main(args: Array<String>) {
    val width = 75
    val height = 52
    val maxSteps = 12_000
    var x = width / 2
    var y = height / 2
    val m = Array(height) { IntArray(width) }
    var dir = Direction.UP
    var i = 0
    while (i < maxSteps && x in 0 until width && y in 0 until height) {
        val turn = m[y][x] == BLACK
        val index = (dir.ordinal + if (turn) 1 else -1) and 3
        dir = Direction.values()[index]
        m[y][x] = if (m[y][x] == BLACK) WHITE else BLACK
        when (dir) {
             Direction.UP    -> y--
             Direction.RIGHT -> x--
             Direction.DOWN  -> y++
             Direction.LEFT  -> x++
        }
        i++
    }
    for (j in 0 until height) {
        for (k in 0 until width) print(if(m[j][k] == WHITE) '.' else '#')
        println()
    }
}
```


{{out}}

```txt

Same as D entry (textual version)

```



## Liberty BASIC

Native graphics.
[[Image:langtonsant.png]]

```lb
dim arena(100,100)
black=0
white=not(black)
for i = 1 to 100
  for j = 1 to 100
    arena(i,j)=white
  next
next
'north=1 east=2 south=3 west=4

nomainwin
graphicbox #1.g, 0, 0, 100, 100
open "Langton's Ant" for window as #1
#1 "trapclose Quit"
#1.g "down"

antX=50:antY=50
nsew=1    'ant initially points north

while (antX>0) and (antX<100) and (antY>0) and (antY<100)
    if arena(antX,antY) then
      nsew=nsew-1
      if nsew<1 then nsew=4
    else
      nsew=nsew+1
      if nsew>4 then nsew=1
    end if

    select case nsew
      case 1: antY=antY-1
      case 2: antX=antX+1
      case 3: antY=antY+1
      case 4: antX=antX-1
      end select

    arena(antX,antY)=not(arena(antX,antY))
    #1.g "color ";GetColor$(antX,antY)
    #1.g "set ";antX;" ";antY
wend

#1.g "flush"
wait

function GetColor$(x,y)
    if arena(x,y) then
        GetColor$="white"
    else
        GetColor$="black"
    end if
    end function

sub Quit handle$
    close #handle$
    end
    end sub
 
```


Text version.

```lb

'move up=1 right=2 down=3 left=4
' ---------------------------------
dim plane(100,100)
x  = 50: y = 50
mx = 100

while (x>0) and (x<100) and (y>0) and (y<100)
if plane(x,y) then
   nxt = nxt - 1
   if nxt < 1 then nxt = 4
  else
   nxt = nxt + 1
   if nxt > 4 then nxt = 1
end if

x          = x + (nxt = 2) - (nxt = 4)
y          = y + (nxt = 3) - (nxt = 1)
plane(x,y) = (plane(x,y) <> 1)
mx         = min(x,mx)
wend

for x = mx to 100
  for y = 1 to 100
   print chr$((plane(x,y)*3) + 32);
  next y
  print x
next x
 
 
```



## Locomotive Basic



```locobasic
10 mode 1:defint a-z:deg
20 ink 1,0:ink 0,26
30 x=50:y=50:ang=270
40 dim play(100,100)
50 graphics pen 3:move 220,100:drawr 200,0:drawr 0,200:drawr -200,0:drawr 0,-200
60 ' move ant
70 if play(x,y) then ang=ang-90 else ang=ang+90
80 play(x,y)=1-play(x,y)
90 plot 220+2*x,100+2*y,play(x,y)
100 ang=ang mod 360
110 x=x+sin(ang)
120 y=y+cos(ang)
130 if x<1 or x>100 or y<1 or y>100 then end
140 goto 70
```


Output:

[[File:Langtons Ant Locomotive BASIC.png]]


## Logo


```logo
make "size 100
make "white 1
make "black 2
make "sum sum :white :black
make "chars [. #]
make "origin quotient :size 2
make "grid mdarray (list :size :size) 
make "directions [ [1 0] [0 1] [-1 0] [0 -1] ]

repeat size [
  local "y
  make "y repcount
  repeat size [
    mdsetitem (list repcount :y) :grid :white
  ]
]
make "x quotient :size 2
make "y quotient :size 2
make "direction sum 1 random count :directions

while [(and (:x > 0) (:x <= :size) (:y > 0) (:y <= :size))] [
  local "color
  make "color mditem (list :x :y) :grid
  local "delta
  ifelse [equal? :color :white] [
     make "delta 1
  ] [
     make "delta -1
  ]
  make "direction sum 1 (modulo (:direction + :delta - 1) count :directions)
  make "dir (item :direction :directions)
  mdsetitem (list :x :y) :grid (sum :sum minus :color)
  make "x sum :x first :dir
  make "y sum :y last :dir
]

repeat size [
  local "y 
  local "blank
  make "y repcount
  make "blank "true
  repeat size [if ( (mditem (list repcount :y) :grid) = :black ) [make "blank "false]]

  if [not :blank] [
    repeat size [
      type item (mditem (list repcount :y) :grid) :chars
    ]
    print []
  ]
]
bye
```


{{Output}}

```txt
...............................................................................................##...
..............................................................................................####..
.............................................................................................#.##.#.
............................................................................................##.####.
...........................................................................................#.#.#.#.#
..........................................................................................##..#.###.
.............................##..........................................................#.#..###..#
......................##......##........................................................##..#...###.
.....................#..#..#.##.#......................................................#.#..###..#..
....................###..###.#..#.....................................................##..#...###...
....................#.####..##.#.....................................................#.#..###..#....
............................##......................................................##..#...###.....
......................##.##.##.....................................................#.#..###..#......
......................####..#.##.#................................................##..#...###.......
......................#####.##.###.##....##....##................................#.#..###..#........
.......................#..#.#.##.#..##....####..##..............................##..#...###.........
........................####.###.####....####..##.#............................#.#..###..#..........
........................###.#...#....##..##.......#...##......................##..#...###...........
.........................####.##...##..##..#......#..#..#....................#.#..###..#............
.........................#.##..#..#...##.##.......#...#..#..................##..#...###.............
.........................#.####..##.#.#.########.#....#..#.................#.#..###..#..............
.........................##..##..#..##.#.#.##.##....#.#.#.##..##..........##..#...###...............
.........................#.#..#..#..#.#....#...#.##...##..#.#####........#.#..###..#................
...........................##.####.##...#..####...#..#...##...##.#......##..#...###.................
.......................##...##########...##.#####..#.####...#....#.....#.#..###..#..................
.......................#..#..#.##..#..#...#.#..##.#####.##.#.....#....##..#...###...................
.......................#...##....#.##..#.#.....#####.#.#####.....#...#.#..###..#....................
......................#...##...###.###....#.#.##.#.##.######.#..#...##..#...###.....................
......................#.####..##.#..#...###.###.##.##...##.#..##...#.#..###..#......................
.......................##..#....##..#..#########..##..####.#......##..#...###.......................
.......................#....#....#..####..#.###########..##...#..#.#..###..#........................
......................#......#...####.####.......##...#.##..###.##..#...###.........................
......................#...#....#.#.#...##......##.#.#.###.#..#.#.#..###..#..........................
.......................#...##.####..####.#####..##..##.#.##.#.....#...###...........................
........................##..#..#.##.###......#..###.#..#....##.#..###..#............................
..........................#.....#..###.##.#..##.####.#.#..#.#...#...###.............................
.........................#...####.##..#....#..###...##.##...##..###..#..............................
.........................#.##..##.###...#.....##..#..###.##.#.#...###...............................
..........................##.....#.##.....##..#...##.##.........#..#................................
...........................##.#....####..#.#...#...###.##.#...#.#...................................
.............................##.###.#####...#.##.##.##.#.#.##.#.###.#...............................
............................#...#.###.#.######.##.##..####.#...##.###...............................
............................#.#.#.###...#..####..#.#.#####...#.....#................................
.............................###...###.....#.#.###..##.#..##.###.#..................................
..............................##..##.#..#.#...##.####.##..###..#.#..................................
..................................###..##.##.....#.....#...#..#.###.................................
..................................#.......###.........#.#.##..##....................................
....................................#....#..#.........#..#.#........................................
....................................#.##............##...###........................................
.....................................##..#..........####..#.........................................
......................................##..############..##..........................................

```



## LOLCODE



```LOLCODE
HAI 1.3

I HAS A plane ITZ A BUKKIT
IM IN YR init UPPIN YR i TIL BOTH SAEM i AN 10000
    plane HAS A SRS i ITZ FAIL
IM OUTTA YR init

I HAS A x ITZ 50, I HAS A y ITZ 50
I HAS A dir ITZ 0, I HAS A pos, I HAS A cell

BTW, WE PURRTIND WE HAS A 2D STRUKSHUR FUR EZ AKSESS
IM IN YR walker
    pos R SUM OF PRODUKT OF y AN 100 AN x
    cell R NOT plane'Z SRS pos
    plane'Z SRS pos R cell
    dir R MOD OF SUM OF dir AN SUM OF 5 AN PRODUKT OF cell AN 2 AN 4

    dir, WTF?
    OMG 0, x R  SUM OF x AN 1, GTFO
    OMG 1, y R DIFF OF y AN 1, GTFO
    OMG 2, x R DIFF OF x AN 1, GTFO
    OMG 3, y R  SUM OF y AN 1, GTFO
    OIC

    BTW, CHEKIN TEH ANTZ BOUNDZ
    WON OF BOTH SAEM x AN -1 AN BOTH SAEM x AN 100, O RLY?, YA RLY, GTFO, OIC
    WON OF BOTH SAEM y AN -1 AN BOTH SAEM y AN 100, O RLY?, YA RLY, GTFO, OIC
IM OUTTA YR walker

IM IN YR printer UPPIN YR cell TIL BOTH SAEM cell AN 10000
    plane'Z SRS cell, O RLY?
        YA RLY, VISIBLE "#"!
        NO WAI, VISIBLE "."!
    OIC

    NOT MOD OF SUM OF cell AN 1 AN 100, O RLY?, YA RLY, VISIBLE "", OIC
IM OUTTA YR printer BTW, UR OUTTA CYAN

KTHXBYE
```



## Lua

For this example, the lua Socket and Curses modules and a terminal with enough lines are needed.

```LUA
local socket = require 'socket' -- needed for socket.sleep
local curses = require 'curses' -- used for graphics

local naptime = 0.02 -- seconds
local world_x, world_y = 100, 100

local world = (function (x, y)
	local wrl = {}
	for i = 1, y do
		wrl[i] = {}
		for j = 1, x do
			wrl[i][j] = 0
		end
	end
	return wrl
end)(world_x, world_y)

-- directions: 0 up, clockwise
local ant = { 
	x = math.floor(world_x / 2),
	y = math.floor(world_y / 2),
	dir = 0,
	step = function(self)
		if self.dir == 0 then self.y = self.y - 1
		elseif self.dir == 1 then self.x = self.x + 1
		elseif self.dir == 2 then self.y = self.y + 1
		else self.x = self.x - 1
		end
	end
}

world.step = function (self, ant)
	if self[ant.y][ant.x] == 0 then	-- white
		-- change cell color
		self[ant.y][ant.x] = 1
		-- change dir
		ant.dir = (ant.dir + 1) % 4
		ant:step()
		-- boundary conditions
		if ant.x < 1 then ant.x = world_x
		elseif ant.x > world_x then ant.x = 1
		end
		if ant.y < 1 then ant.y = world_y
		elseif ant.y > world_y then ant.y = 1
		end
	else
		-- change cell color
		self[ant.y][ant.x] = 0
		-- change dir
		ant.dir = (ant.dir - 1) % 4
		ant:step()
		-- boundary conditions
		if ant.x < 1 then ant.x = world_x
		elseif ant.x > world_x then ant.x = 1
		end
		if ant.y < 1 then ant.y = world_y
		elseif ant.y > world_y then ant.y = 1
		end
	end
end

world.draw = function (self, ant)
	for i = 1, #self do
		for j = 1, #self[i] do
			if i == ant.y and j == ant.x then
				win:attron(curses.color_pair(3))
				win:mvaddch(i,j,"A")
				--win:attroff(curses.color_pair(3))
			elseif self[i][j] == 0 then 
				win:attron(curses.color_pair(1))
				win:mvaddch(i,j," ")
				--win:attroff(curses.color_pair(1))
			elseif self[i][j] == 1 then
				win:attron(curses.color_pair(2))
				win:mvaddch(i,j," ")
				--win:attroff(curses.color_pair(2))
			else error("self[" .. i .. "][" .. j .. "] is " .. self[i][j] .. "!")
			end
		end
	end
end

local it = 1
curses.initscr()
curses.start_color()
curses.echo(false)
curses.init_pair(1, curses.COLOR_WHITE, curses.COLOR_WHITE)
curses.init_pair(2, curses.COLOR_BLACK, curses.COLOR_BLACK)
curses.init_pair(3, curses.COLOR_RED, curses.COLOR_WHITE)
curses.init_pair(4, curses.COLOR_WHITE, curses.COLOR_BLACK)
win = curses.newwin(world_y + 1, world_x, 0, 0)
win:clear()
repeat
	world:draw(ant)
	win:move(world_y, 0)
	win:clrtoeol()
	win:attron(curses.color_pair(4))
	win:addstr("Iteration: " .. it .. ", nap = " .. naptime*1000 .. "ms")
	win:refresh()
	world:step(ant)
	it = it + 1
	--local c = stdscr:getch()
	--if c == '+' then naptime = naptime - (naptime / 10)
	--elseif c == '-' then naptime = naptime + (naptime / 10)
	--end
	socket.sleep(naptime)
until false

```


## M2000 Interpreter


```M2000 Interpreter

Module Ant {
	Form 120,102
	N=100
	Enum CellColor {black=0,white=#FFFFFF}
	Enum Direction{North=90, West=180, South=270, East=0}
	Function Rotate(cd as Direction, clockwise=true) {
			cd=(cd+if(clockwise->270,90)) mod 360
			=cd  ' return a Direction Enum type
	}
	
	dim rect(1 to N, 1 to N)=white
	cx=N div 2
	cy=N div 2
	cd=North
	rect(cx,cy)=black
	endmove=False
	while not endmove
		movecell()
	
	end while
	Disp()
	
	sub movecell()
		select case rect(cx,cy)
		case black
			cd=Rotate(cd, false) : rect(cx, cy)=white
		case white
			cd=Rotate(cd) : rect(cx, cy)=black
		end select
		select case cd
		case North
			cy--
		case West
			cx--
		case South
			cy++
		case East
			cx++
		end select
		endmove= cx<1 or cx>N or cy<1 or cy>N
	end sub
	sub disp()
		Local Doc$, i, j
		Document Doc$
		for i=1 to N:for j=1 to N
			Doc$=if$(rect(j,i)=White->"_","#")
		next
		Doc$={
		}
		next
		cls
		Print #-2,Doc$
		clipboard Doc$
	end sub
}
Ant

```


{{out}}
<pre style="height:30ex;overflow:scroll">
____________________________________________________________________________________________________
__________________________________________________________________##________________________________
___________________________________________________________________##_______________________________
____________________________________________##__##____________###_##_#______________________________
___________________________________________#__##__###________####_#__#______________________________
__________________________________________#____##___#______##_##__#_#_______________________________
_______________________________________##_#_____#_____#######_###_##________________________________
______________________________________#__#__#___##_#___#__#####_#__#________________________________
_____________________________________###___#___####__##_###_#______##_______________________________
__________________________________##_#_#__##__#___#_##__####_######_________________________________
_________________________________#__#___#____#_____##__##___#_#_#####_#_____________________________
________________________________###_##__#__#____#___#####_#_##_#____###_____________________________
________________________________##_____##_###___##_###___##_####__#__#______________________________
__________________________________###__###_#___#_#_###_____#__#____##_______________________________
____________________________________#_#___#########_#####___####____________________________________
______________________________###_###__##_#__#______#___##__#_______________________________________
_______________________________#####_#####__##___#####____#_#_#_____________________________________
____________________________##_#_#######_###_######_#####_#_###_____________________________________
___________________________#____##__##___###__#__#__#__#___#_#______________________________________
__________________________###______###__#_##_##____#__#_##_#________________________________________
__________________________#_#__#____#____#_#####__#____#_##_________________________________________
_______________________________##_###__#_#__##_##____#__#_#_________________________________________
___________________________#_#_____#_#_____#_#_##_#_#__###_##_#_____________________________________
__________________________#___###__#__#__#_#__####_##___##_####_____________________________________
__________________________#___##_###_##_#__#___#_____####_#_##______________________________________
__________________________#__##___###______#__####_###_##___##______________________________________
__________________________#______###_____###__###___##__#____#______________________________________
__________________________#_____#_#_#_####____####__##_##___________________________________________
__________________________#_____##_###_##___#_##_####_###___#_#_____________________________________
__________________________#______#_#____#####_#_#_###_______###_____________________________________
__________________________#____####_#____###_##_###___#_#____#______________________________________
__________________________#_____#__##_##_##_####__##_##__####_______________________________________
__________________________#_____#_##_##__#____#######_______________________________________________
__________________________###___##__##__#_###_#_##_#___#____________________________________________
__________________________###____##_######_#__#___####____#_________________________________________
___________________________#_##_#_##__##____####_#_##_####_#________________________________________
___________________________#___#######_######__#_##_#_#____#________________________________________
__________________________#___#___##_#__#___##_######__#__#_________________________________________
__________________________#_##____###__#___#_#######_#__##__________________________________________
___________________________##_#_##___#_#_#_##_#__##__###____________________________________________
____________________________######_##__________#####___#____________________________________________
________________________________#___#__#####_#______#_#_____________________________________________
_________________________________##_____#_#_##___#____#_____________________________________________
______________________________##_#__##_#_____##_#____###____________________________________________
______________________________#_##_#______#_#___#____###____________________________________________
_______________________________#___####_##___#___#____#_____________________________________________
_______________________________###__#___#___###___####______________________________________________
_______________________________#___##__##_##___#____________________________________________________
__________________________________##__##__#___###___________________________________________________
___________________________________##__#_##_##___#__________________________________________________
________________________________________##__#___###_________________________________________________
_________________________________________#_##_##___#________________________________________________
__________________________________________##__#___###_______________________________________________
___________________________________________#_##_##___#______________________________________________
____________________________________________##__#___###_____________________________________________
_____________________________________________#_##_##___#____________________________________________
______________________________________________##__#___###___________________________________________
_______________________________________________#_##_##___#__________________________________________
________________________________________________##__#___###_________________________________________
_________________________________________________#_##_##___#________________________________________
__________________________________________________##__#___###_______________________________________
___________________________________________________#_##_##___#______________________________________
____________________________________________________##__#___###_____________________________________
_____________________________________________________#_##_##___#____________________________________
______________________________________________________##__#___###___________________________________
_______________________________________________________#_##_##___#__________________________________
________________________________________________________##__#___###_________________________________
_________________________________________________________#_##_##___#________________________________
__________________________________________________________##__#___###_______________________________
___________________________________________________________#_##_##___#______________________________
____________________________________________________________##__#___###_____________________________
_____________________________________________________________#_##_##___#____________________________
______________________________________________________________##__#___###___________________________
_______________________________________________________________#_##_##___#__________________________
________________________________________________________________##__#___###_________________________
_________________________________________________________________#_##_##___#________________________
__________________________________________________________________##__#_#####_______________________
___________________________________________________________________#_###_####_______________________
____________________________________________________________________##_###_#________________________
_____________________________________________________________________#_#_##_________________________
______________________________________________________________________#_#___________________________

</pre >

=={{header|MATLAB}} / {{header|Octave}}==

```MATLAB
function u = langton_ant(n)
	if nargin<1, n=100; end;
	A = sparse(n,n);	% white
	P = [n/2;n/2];	% Positon
	D = 3;	         % index of direction 0-3 
	T = [1,0,-1,0;0,1,0,-1];	% 4 directions
	k = 0;
	while (1)
		k = k+1;	
		a = A(P(1),P(2));
		A(P(1),P(2)) = ~a;
		if ( a )
			D = mod(D+1,4);
		else
			D = mod(D-1,4);
		end;
		P = P+T(:,D+1);
		
		if (~mod(k,100)),spy(A);pause(.1);end;  %display after every 100 interations
	end; 
end
```



## Octave


```OCTAVE
clear
E=100           % Size of lattice.
N=11200         % Number of iterations.
z(1:1:E^2)=-1;  % Init lattice rotations (-1=right, 1=left)
k(1:1:E^2)=0;
k(1)=(E^2+E)/2; % Init the Ant @ 50,50
for t=1:1:N;
k(t+1)=mod(k(t)+real(round((0.5*(E+1)*exp(i*pi/4*(trace(diag(z))-E^2)))-(0.5*(E-1)*exp(-i*pi/4*(trace(diag(z))-E^2)))))+imag(round((0.5*(E+1)*exp(i*pi/4*(trace(diag(z))-E^2)))-(0.5*(E-1)*exp(-i*pi/4*(trace(diag(z))-E^2))))),E^2);
z(k(t+1)+1)=real(exp(2*i*pi/4*(1+z(k(t+1)+1))));
endfor;
imagesc(reshape(z,E,E))    % Draw the Lattice

```



## Mathematica

[[File:LangtonsAnt.png|right|thumb|Output]]


```mathematica
direction = 1;
data = SparseArray[{{50, 50} -> -1}, {100, 100}, 1];
NestWhile[
  {Re@#, Im@#} &@(direction *= (data[[Sequence @@ #]] *= -1) I) + # &,
  {50, 50}, 1 <= Min@# <= Max@# <= 100 &];
Image@data
```



## Nim

{{trans|Python}}

```nim
import strutils, sequtils

type
  Direction = enum up, right, down, left
  Color = enum white, black

const
  width = 75
  height = 52
  maxSteps = 12_000

var
  m: array[height, array[width, Color]]
  dir = up
  x = width div 2
  y = height div 2

var i = 0
while i < maxSteps and x in 0 .. < width and y in 0 .. < height:
  let turn = m[y][x] == black
  m[y][x] = if m[y][x] == black: white else: black

  dir = Direction((4 + int(dir) + (if turn: 1 else: -1)) mod 4)
  case dir
  of up:    dec y
  of right: dec x
  of down:  inc y
  of left:  inc x

  inc i

for row in m:
  echo map(row, proc(x: Color): string =
    if x == white: "." else: "#").join("")
```



## OCaml



```ocaml
open Graphics

type dir = North | East | South | West

let turn_left = function
  | North -> West
  | East  -> North
  | South -> East
  | West  -> South

let turn_right = function
  | North -> East
  | East  -> South
  | South -> West
  | West  -> North

let move (x, y) = function
  | North -> x, y + 1
  | East  -> x + 1, y
  | South -> x, y - 1
  | West  -> x - 1, y

let () =
  open_graph "";
  let rec loop (x, y as pos) dir =
    let color = point_color x y in
    set_color (if color = white then black else white);
    plot x y;
    let dir = (if color = white then turn_right else turn_left) dir in
    if not(key_pressed()) then loop (move pos dir) dir
  in
  loop (size_x()/2, size_y()/2) North
```


Run with:
 $ ocaml graphics.cma langton.ml


## PARI/GP

[[File:Langton-pari.png|right|thumb]]

```parigp
langton()={
  my(M=matrix(100,100),x=50,y=50,d=0);
  while(x && y && x<=100 && y<=100,
    d=(d+if(M[x,y],1,-1))%4;
    M[x,y]=!M[x,y];
    if(d%2,x+=d-2,y+=d-1);
  );
  M
};
show(M)={
  my(d=sum(i=1,#M[,1],sum(j=1,#M,M[i,j])),u=vector(d),v=u,t);
  for(i=1,#M[,1],for(j=1,#M,if(M[i,j],v[t++]=i;u[t]=j)));
  plothraw(u,v)
};
show(langton())
```



## Pascal

Pascal does not offer complex number arithmetic, so adjusting directions via multiplication of ±i is out. Similarly, it does not offer array manipulation statements, so <code>Cell:=White;</code> must be achieved via explicit for-loops with explicitly stated indices and bounds, and the adjustment of the (x,y) position by (dx,dy) can't be done by array addition. Otherwise, matters are straightforward, so instead this version tries to animate the ant on the screen. Alas, the maximum screen size is 80 characters by 50 lines, except that output to the last line causes a screen scroll so that only 49 lines are available. Alas, this cell array is too small and the bounds are exceeded in step 5,156 - before the ant starts its migration.

The animation shows the arrival at a cell with a yellow arrow pointing in the arrival direction. The cell state is investigated to decide the new direction (which is shown as a green arrow), the current cell's state is flipped, and the move to the new cell position is made. To show these events, the programme waits for a keystroke but if the S key is pressed, full speed results. Each stepwise ant move thus requires two keystrokes (one for each of the two directions being shown) however a quirk of Pascal's processing of keyboard symbols has certain keystrokes represented via ''two'' values from ReadKey, so pressing the arrow keys for example provides a doubled advance.

{{works with| Free Pascal}}
{{works with|Turbo Pascal}} Except, the green arrow on step 4 does not appear!


```Pascal

{$B- Early and safe resolution of  If x <> 0 and 1/x...}
Program LangtonsAnt; Uses CRT;
{Perpetrated by R.N.McLean (whom God preserve), Victoria University, December MMXV.}
 Var AsItWas: record mode: word; ta: word; end;
 Var LastLine,LastCol: byte;

 Procedure Swap(var a,b: integer);	{Oh for a compiler-recognised statement.}
  var t: integer;			{Such as A=:=B;}
   Begin
    t:=a; a:=b; b:=t;
   End;

 var Stepwise: boolean;
 Var Cell: Array[1..80,1..50] of byte;	{The screen is of limited size, alas.}
 Var x,y,Step: integer;		{In the absence of complex numbers,}
 Var dx,dy: integer;		{And also of array action statements.}

 Procedure Croak(Gasp: string);	{Exit message...}
  Begin
   GoToXY(1,12); TextColor(Yellow);	{Reserve line twelve.}
   WriteLn(Gasp,' on step ',Step,' to (',x,',',y,')');
   HALT;
  End;

 Procedure Harken;		{Waits for a keystroke.}
  var ch: char;			{The character. Should really be 16-bit.}
  Begin
   ch:=ReadKey;			{Fancy keys evoke double characters. I don't care.}
   if (ch = 'S') or (ch = 's') then Stepwise:=not Stepwise	{Quick, slow, quick, quick, slow...}
    else if ch = #27 then Croak('ESC!');	{Or perhaps, enough already!}
  End;				{Fancy keys will give a twostep.}
 Procedure Waitabit;		{Slows the action.}
  Begin
   if Stepwise or KeyPressed then Harken;	{Perhaps a change while on the run.}
  End;	{of Waitabit.}

 Procedure Turn(way:integer);	{(dx,dy)*(0,w) = (-w*dy,+w*dx)}
  Begin
   Swap(dx,dy);			{In the absence of complex arithmetic,}
   dx:=-way*dx; dy:=way*dy;	{Do this in two stages.}
  End;

 const Arrow: array[-1..+1,-1..+1] of integer	{Only four entries are of interest.}
  = ((1,27,3),(25,5,24),(7,26,9));		{For the four arrow symbols.}
 Procedure ShowDirection(Enter,How: byte);	{Show one.}
  Begin
   GoToXY(x,LastLine - y + 1);	{(x,y) position, in Cartesian style.}
   TextBackground(Enter);	{The value in Cell[x,y] may have been changed.}
   TextColor(How);
   Writeln(chr(Arrow[dx,dy]));	{Not an ASCII control character, but an arrow symbol.}
   Waitabit;			{Having gone to all this trouble.}
  End;
 Procedure ShowState;		{Special usage for line two of the screen.}
  Begin
   GoToXY(1,2); TextBackground(LightGray); TextColor(Black);
   Write(Step:5,' (',x:2,',',y:2,') ');
   TextColor(Yellow);		{Yellow indicates the direction in mind.}
   Write(chr(Arrow[dx,dy]));	{On *arrival* at a position.}
  End;

 Var i,j: integer;		{Steppers. No whole-array assault as in Cell:=LightGray;}
 var Enter: byte;		{Needed to remember the cell state on arrival.}
 BEGIN
  AsItWas.mode:=LastMode;	{Grr. I might want to save the display content too!}
  AsItWas.ta:=TextAttr;		{Not just its colour and style.}
  TextMode(C80+Font8x8);	{Crazed gibberish gives less unsquare character cells, and 80x50 of them.}
  LastLine:=Hi(WindMax);	{ + 1 omitted, as a write to the last line scrolls the screen up one...}
  LastCol:=Lo(WindMax) + 1;	{Counting starts at zero, even though GoToXY starts with one.}
  x:=LastCol div 2;		{Start somewhere middleish.}
  y:=LastLine div 2;		{Consider (x,y) as being (0,0) for axes.}
  dx:=+1; dy:=0;		{Initial direction.}
  TextBackground(LightGray);	{"White" is not valid for background colour.}
  TextColor(Black);		{This will show up on a light background.}
  ClrScr;			{Here we go.}

  WriteLn('Langton''s Ant, on x = 1:',LastCol,', y = 1:',LastLine);
  ShowState;					{Where we start.}
  WriteLn; TextColor(Black);
  WriteLn('Press a key for each step.');	{Some encouragement.}
  WriteLn('"S" to pause each step or not.');
  WriteLn('ESC to quit.');

  for i:=1 to LastLine do begin GoToXY(x,i); Write('|'); end;			{Draw a y-axis.}
  for i:=1 to LastCol do begin GoToXY(i,LastLine - y + 1); Write('-'); end;	{And x.}
  gotoxy(1,6);	{Can't silence the cursor!}

  for i:=1 to LastCol do	{Prepare the cells.}
   for j:=1 to LastLine do	{One by one.}
    Cell[i,j]:=LightGray;	{Cell:=LightGray. Sigh.}

  Stepwise:=true;		{The action is of interest.}
  for Step:=1 to 12000 do	{Here we go.}
   if (x <= 0) or (x > LastCol) or (y <= 0) or (y > LastCol) then Croak('Out of bounds')
    else				{We're in a cell.}
     begin				{So, inspect it.}
      if Stepwise or (Step mod 10 = 0) then ShowState	{On arrival.}
       else if KeyPressed then Harken;			{If we're not pausing, check for a key poke.}
      Enter:=cell[x,y];					{This is what awaits the feet.}
      if Stepwise then ShowDirection(Enter,Yellow);	{Current direction, about to be changed.}
      case cell[x,y] of					{So, what to do?}
   LightGray: begin Cell[x,y]:=Black;     Turn(-1); end;{White. Make black and turn right.}
       Black: begin Cell[x,y]:=LightGray; Turn(+1); end;{Black. Make white and turn left.}
      end;						{Having decided,}
      if Stepwise then ShowDirection(Enter,Green);	{Show the direction about to be stepped.}
      GoToXY(x,LastLine - y + 1);	{Screen location (column,line) for (x,y)}
      TextBackground(Cell[x,y]);	{Change the state I'm about to leave.}
      Write(' ');			{Foreground colour irrelevant for spaces.}
      x:=x + dx; y:=y + dy;		{Make the step!}
     end;			{On to consider our new position.}

  Croak('Finished');		{That was fun.}

 END.

```



## Perl


```perl
#!/usr/bin/perl
use strict;
# Perl 5 implementation of Langton's Ant

# Using screen coordinates - 0,0 in upper-left, +X right, +Y down -
# these directions (right, up, left, down) are counterclockwise
# so advance through the array to turn left, retreat to turn right
my @dirs = ( [1,0], [0,-1], [-1,0], [0,1] );
my $size = 100;

# we treat any false as white and true as black, so undef is fine for initial all-white grid
my @plane;
for (0..$size-1) { $plane[$_] = [] };

# start out in approximate middle
my ($x, $y) = ($size/2, $size/2);

# pointing in a random direction
my $dir = int rand @dirs;

my $move;
for ($move = 0; $x >= 0 && $x < $size && $y >= 0 && $y < $size; $move++) {
  # toggle cell's value (white->black or black->white)
  if ($plane[$x][$y] = 1 - ($plane[$x][$y] ||= 0)) {
        # if it's now true (black), then it was white, so turn right 
        $dir = ($dir - 1) % @dirs;
  } else {
        # otherwise it was black, so turn left
        $dir = ($dir + 1) % @dirs;
  }
  $x += $dirs[$dir][0];
  $y += $dirs[$dir][1];
}

print "Out of bounds after $move moves at ($x, $y)\n";
for (my $y=0; $y<$size; ++$y) {
  for (my $x=0; $x<$size; ++$x) {
    print $plane[$x][$y] ? '#' : '.';
  }
  print "\n";
}
```



## Perl 6

{{trans|Perl}}
In this version we use 4-bits-per-char graphics to shrink the output to a quarter the area of ASCII graphics.

```perl6
constant @vecs = [1,0,1], [0,-1,1], [-1,0,1], [0,1,1];
constant @blocky = ' ▘▝▀▖▌▞▛▗▚▐▜▄▙▟█'.comb;
constant $size = 100;
enum Square <White Black>;
my @plane = [White xx $size] xx $size;
my ($x, $y) = $size/2, $size/2;
my $dir = @vecs.keys.pick;
my $moves = 0;
loop {
    given @plane[$x][$y] {
        when :!defined { last }
        when White { $dir--; $_ = Black; }
        when Black { $dir++; $_ = White; }
    }
    ($x,$y,$moves) »+=« @vecs[$dir %= @vecs];
}
say "Out of bounds after $moves moves at ($x, $y)";
for 0,2,4 ... $size - 2 -> $y {
    say join '', gather for 0,2,4 ... $size - 2 -> $x {
        take @blocky[ 1 * @plane[$x][$y]
                    + 2 * @plane[$x][$y+1]
                    + 4 * @plane[$x+1][$y]
                    + 8 * @plane[$x+1][$y+1] ];
    }
}
```

{{out}}

```txt
Out of bounds after 11669 moves at (-1, 26)
            ▄▚▚                                   
           ▟▟▜▟▚                                  
           ▜▀▚▌▟▚                                 
            ▜▘▗▌▟▚                                
             ▜▘▗▌▟▚                               
              ▜▘▗▌▟▚                              
               ▜▘▗▌▟▚                             
                ▜▘▗▌▟▚                            
                 ▜▘▗▌▟▚                           
                  ▜▘▗▌▟▚                          
                   ▜▘▗▌▟▚                         
                    ▜▘▗▌▟▚                        
                     ▜▘▗▌▟▚                       
                      ▜▘▗▌▟▚                      
                       ▜▘▗▌▟▚                     
                        ▜▘▗▌▟▚ ▄                  
                         ▜▘▗▌▟▘▟▘▗                
                      ▞▀▚ ▜▘▗▌▄▙▝▜                
                     ▐█  ▌▄▘▘▗▗▞▐▚▌               
                      ▌▖▝ ▐▚▙▙ ▖▀▖                
                     ▐▄▝█▀▖▄▗▗▗▀▐▛▛▙              
                    ▞▚▝▟██▜▞ ▞▗▜▌ ▞▘▌             
                   ▐▗▄▌▙▜▐▄▛▀▜▞▜▛▛▄▐              
                    ▘▗▝▜▚▖▌▟▞▛▜▌▜▖ █▌             
                   ▄▄ ▄▜▛▜▙▖▟▗▛▟▘▌  ▌             
                  ▟▖ ▘▘▄▛▌▛▟█▖ ▚▜▀  ▌             
                  ▘▘ █▚▛▜▟▌▘▗█▞▛▞▌  ▌             
                  ▐▖ ▙▐▙▗█▌▐▀  ▟▛ ▄ ▌             
                  ▟▙▚▛▀▄▗▟▖▐▗▘▛▐▀▟▌ ▌             
                  ▘▀▞▛▗▘▘█▐▞▗▗▝▟▖▄▝▝              
                   ▗▜▞▖▗▘▝█▜▞▖▗▙ ▝ ▙▌             
                  ▟▞▖▟▄▌▟▄▙▐█▗▟▙▟▚▗▞              
                  ▘▌▚▖▝▛▀ ▐▘▞█▀▟▛█▖               
               ▄ ▝▛▚ ▀▜▙▜▜▀▜▚▄▘▙▖                 
              ▟▖▘▐▜▌▛▄▟▛▝▌ ▜▘▛▗▖▟▌                
              ▘▀█▙▙▚▄▛▗▛▖ ▞▗▖▚▗▚▞                 
               ▜ ▖▄▙▛▚▀▗▜▛ ▞▗▝▛                   
               ▞▌▜▌█▀▀▘▖ ▙  ▌▀                    
              ▐▗▌█▛    ▀▚▞▚▞                      
               ▜▖                                 
                                                  

```



## Phix


```Phix
sequence grid = repeat(repeat(' ',100),100)
integer aX = 50, aY = 50,
        gXY, angle = 1                      -- ' '/'#';  0,1,2,3 = NESW
constant dX = {0,-1,0,1}                    -- (dY = reverse(dX))
 
while aX>=1 and aX<=100
  and aY>=1 and aY<=100 do
    gXY = grid[aX][aY]
    grid[aX][aY] = 67-gXY           -- ' '<=>'#', aka 32<->35
    angle = mod(angle+2*gXY+3,4)    -- +/-1, ie 0,1,2,3 -> 1,2,3,0 or 3,0,1,2
    aX += dX[angle+1]
    aY += dX[4-angle]
end while
 
puts(1,join(grid,"\n"))
```

{{out}}
<pre style="font-size: 2px">
                                         ##  ############  ##
                                        #  ####          #  ##
                                       ###   ##            ## #
                                       # #  #         #  #    #
                                   ##  ## # #         ###       #
                                ### #  #   #     #     ## ##  ###
                                 # #  ###  ## #### ##   # #  # ##  ##
                                 # ### ##  # ##  ### # #     ###   ###
                               #     #   ##### # #  ####  #   ### # # #
                              ### ##   # ####  ## ## ###### # ### #   #
                              # ### # ## # # ## ## ## #   ##### ### ##
                                  # #   # ## ###   #   # #  ####    # ##
                               #  #         ## ##   #  ##     ## #     ##
                              ###   # # ## ###  #  ##     #   ### ##  ## #
                             #  ###  ##   ## ##   ###  #    #  ## ####   #
                            ###   #   # #  # # #### ##  # ## ###  #     #
                           #  ###  # ##    #  # ###  #      ### ## #  #  ##
                          ###   #     # ## # ##  ##  ##### ####  #### ##   #
                         #  ###  # # #  # ### # # ##      ##   # # #    #   #
                        ###   #  ## ###  ## #   ##       #### ####   #      #
                       #  ###  # #  #   ##  ########### #  ####  #    #    #
                      ###   #  ##      # ####  ##  #########  #  ##    #  ##
                     #  ###  # #   ##  # ##   ## ## ### ###   #  # ##  #### #
                    ###   #  ##   #  # ###### ## # ## # #    ### ###   ##   #
                   #  ###  # #   #     ##### # #####     # #  ## #    ##   #
                  ###   #  ##    #     # ## ##### ##  # #   #  #  ## #  #  #
                 #  ###  # #     #    #   #### #  ##### ##   ##########   ##
                ###   #  ##      # ##   ##   #  #   ####  #   ## #### ##
               #  ###  # #        ##### #  ##   ## #   #    # #  #  #  # #
              ###   #  ##          ##  ## # # #    ## ## # # ##  #  ##  ##
             #  ###  # #                 #  #    # ######## # # ##  #### #
            ###   #  ##                  #  #   #       ## ##   #  #  ## #
           #  ###  # #                    #  #  #      #  ##  ##   ## ####
          ###   #  ##                      ##   #       ##  ##    #   # ###
         #  ###  # #                            # ##  ####    #### ### ####
        ###   #  ##                              ##  ####    ##  # ## # #  #
       #  ###  # #                                ##    ##    ## ### ## #####
      ###   #  ##                                                # ## #  ####
     #  ###  # #                                                     ## ## ##
    ###   #  ##                                                      ##
   #  ###  # #                                                     # ##  #### #
  ###   #  ##                                                     #  # ###  ###
 #  ###  # #                                                      # ## #  #  #
###   #  ##                                                        ##      ##
   ##  # #                                                          ##
##  #  ##
 # # # #
#### ##
# ## #
 ####
  ##

```



## PHP

[[Image:langtons_ant_php.png|right|100px]]


''This is an implementation of Langton`s Ant in PHP''

''(The TEXT TO IMAGE - part is obviously not necessary.''

''Additionally the x and y startpositions could be set''

''to the halves of width and height.)''




```php

// INIT AND DEFINITION
define('dest_name', 'output.png'); // destination image
define('width', 100);
define('height', 100);

$x = 50;
$y = 70;
$dir = 0; // 0-up, 1-left, 2-down, 3-right
$field = array();
$step_count = 0; 

// LANGTON´S ANT PROCEDURE
while(0 <= $x && $x <= width && 0 <= $y && $y <= height){
	if(isset($field[$x][$y])){
		unset($field[$x][$y]);
		$dir = ($dir + 3) % 4;
	}else{
		$field[$x][$y] = true;
		$dir = ($dir + 1) % 4;
	}
	switch($dir){
		case 0: $y++; break;
		case 1: $x--; break;
		case 2: $y--; break;
		case 3: $x++; break;
	}
	$step_count++;
}
// ARRAY TO IMAGE
$img = imagecreatetruecolor(width, height);
$white = imagecolorallocate($img, 255, 255, 255);
for($x = 0; $x < width; $x++){	
	for($y = 0; $y < height; $y++){
		if(isset($field[$x][$y])){
			imagesetpixel($img, $x, $y, $white);
		}
	}
}
// TEXT TO IMAGE
$color = array();
$color[0] = imagecolorallocate($img, 255, 0, 0);
$color[1] = imagecolorallocate($img, 0, 255, 0);
$color[2] = imagecolorallocate($img, 0, 0, 255);
$print_array = array(
	0 => 'Langton`s Ant', 1=>'PHP Version', 2=>'Steps: ' . $step_count
);
foreach($print_array as $key => $line){
	imagestring($img, 3, 3, 3 + $key*11, $line, $color[$key]);
}
// SAVE IMAGE
imagepng($img, dest_name);

```



## PicoLisp

[[File:Picolisp_ant.gif|right|thumb]]
This code pipes a PBM into ImageMagick's "display" to show the result:

```PicoLisp
(de ant (Width Height X Y)
   (let (Field (make (do Height (link (need Width)))) Dir 0)
      (until (or (le0 X) (le0 Y) (> X Width) (> Y Height))
         (let Cell (nth Field X Y)
            (setq Dir (% (+ (if (car Cell) 1 3) Dir) 4))
            (set Cell (not (car Cell))) 
            (case Dir
               (0 (inc 'X))
               (1 (inc 'Y))
               (2 (dec 'X))
               (3 (dec 'Y)) ) ) )
      (prinl "P1")
      (prinl Width " " Height)
      (for Row Field
         (prinl (mapcar '[(X) (if X 1 0)] Row)) ) ) )
(out '(display -) (ant 100 100 50 50))
(bye)

```



## PowerShell

{{works with|PowerShell|2}}
To simplify the steps within the loop, -1 and 1 are used to represent the binary state of the spaces in the grid. As neither state is now a default value, to simplify setting the starting states, an array of arrays is used instead of a two dimensional array.

```PowerShell

$Size = 100

$G = @()
1..$Size | ForEach { $G += ,( @( 1 ) * $Size ) }

$x = $y = $Size / 2
 
#  Direction of next move
$Dx = 1
$Dy = 0
 
#  While we are still on the grid...
While ( $x -ge 0 -and $y -ge 0 -and $x -lt $Size -and $y -lt $Size )
    {
    #  Change direction
    $Dx, $Dy = ( $Dy * $G[$x][$y] ), -( $Dx * $G[$x][$y] )
 
    #  Change state of current square
    $G[$x][$y] = -$G[$x][$y]
 
    #  Move forward
    $x += $Dx
    $y += $Dy
    }
 
#  Convert to strings for output
ForEach ( $Row in $G ) { ( $Row | ForEach { ( ' ', '', '#')[$_+1] } ) -join '' }

```

{{out}}
Default PowerShell console colors reverse the colors from black on white to white on dark blue. Most blank lines not included below.
<pre style="height:50ex;overflow:scroll">
####################################################################################################
################################################################################################  ##
###############################################################################################    #
############################################################################################## #  # 
#############################################################################################  #    
############################################################################################ # # # #
###########################################################################################  ## ##  
##############################  ########################################################## # ##  ###
#######################  ######  ########################################################  ## ###   
###################### ## ## #  # ###################################################### # ##   ## #
#####################   ##   # ## #####################################################  ## ###   ##
##################### #    ##  # ##################################################### # ##   ## ###
#############################  ######################################################  ## ###   ####
#######################  #  #  ##################################################### # ##   ## #####
#######################    ## #  # ################################################  ## ###   ######
#######################     #  #   #  ####  ####  ################################ # ##   ## #######
######################## ## # #  # ##  ####    ##  ##############################  ## ###   ########
#########################    #   #    ####    ##  # ############################ # ##   ## #########
#########################   # ### ####  ##  ####### ###  ######################  ## ###   ##########
##########################    #  ###  ##  ## ###### ## ## #################### # ##   ## ###########
########################## #  ## ## ###  #  ####### ### ## ##################  ## ###   ############
########################## #    ##  # # #        # #### ## ################# # ##   ## #############
##########################  ##  ## ##  # # #  #  #### # # #  ##  ##########  ## ###   ##############
########################## # ## ## ## # #### ### #  ###  ## #     ######## # ##   ## ###############
############################  #    #  ### ##    ### ## ###  ###  # ######  ## ###   ################
########################  ###          ###  #     ## #    ### #### ##### # ##   ## #################
######################## ## ## #  ## ## ### # ##  #     #  # ##### ####  ## ###   ##################
######################## ###  #### #  ## # #####     # #     ##### ### # ##   ## ###################
####################### ###  ###   #   #### # #  # #  #      # ## ###  ## ###   ####################
####################### #    ##  # ## ###   #   #  #  ###  # ##  ### # ##   ## #####################
########################  ## ####  ## ##         ##  ##    # ######  ## ###   ######################
######################## #### #### ##    ## #           ##  ### ## # ##   ## #######################
####################### ###### ###    #    #######  ### #  ##   #  ## ###   ########################
####################### ### #### # # ###  ######  # # #   # ## # # ##   ## #########################
######################## ###  #    ##    #     ##  ##  # #  # ##### ###   ##########################
#########################  ## ## #  #   ###### ##   # ## ####  # ##   ## ###########################
########################### ##### ##   #  # ##  #    # # ## # ### ###   ############################
########################## ###    #  ## #### ##   ###  #  ###  ##   ## #############################
########################## #  ##  #   ### #####  ## ##   #  # # ###   ##############################
###########################  ##### #  #####  ## ###  #  ######### ## ###############################
############################  # ####    ## # ### ###   #  # ### # ##################################
##############################  #   #     ### #  #  #  # # #  # #   # ##############################
############################# ### #   # #      #  #  ##    # ###  #   ##############################
############################# # # #   ### ##    ## # #     ### ##### ###############################
##############################   ###   ##### # #   ##  # ##  #   # #################################
###############################  ##  # ## # ###  #    #  ##   ## # #################################
###################################   ##  #  ##### ##### ### ## #   ################################
################################### #######   ######### # #  ##  ###################################
##################################### #### ## ######### ## # #######################################
##################################### #  ############  ###   #######################################
######################################  ## ##########    ## ########################################
#######################################  ##            ##  #########################################
####################################################################################################

```



## Processing

Processing implementation, this uses two notable features of Processing, first of all, the animation is calculated with the draw() loop, second the drawing on the screen is also used to represent the actual state.

```processing
/*
 * we use the following conventions:
 * directions 0: up, 1: left, 2: down: 3: right
 *
 * pixel white: true, black: false
 *
 * turn right: true, left: false
 *
 */

// number of iteration steps per frame
// set this to 1 to see a slow animation of each
// step or to 10 or 100 for a faster animation

final int STEP=100;

int x;
int y;
int direction;

void setup() {
  // 100x100 is large enough to show the
  // corridor after about 10000 cycles
  size(100, 100, P2D);

  background(#ffffff);

  x=width/2;
  y=height/2;

  direction=0;
}

int count=0;

void draw() {
  for(int i=0;i<STEP;i++) {
    count++;
    boolean pix=get(x,y)!=-1; //white =-1
    setBool(x,y,pix);
  
    turn(pix);
    move();
  
    if(x<0||y<0||x>=width||y>=height) {
      println("finished");
      noLoop();
      break;
    }
  }
  if(count%1000==0) {
    println("iteration "+count);
  }
}

void move() {
  switch(direction) {
    case 0:
      y--;
      break;
    case 1:
      x--;
      break;
    case 2:
      y++;
      break;
    case 3:
      x++;
      break;
  }
}

void turn(boolean rightleft) {
  direction+=rightleft?1:-1;
  if(direction==-1) direction=3;
  if(direction==4) direction=0;
}

void setBool(int x, int y, boolean white) {
  set(x,y,white?#ffffff:#000000);
}
```



## Prolog

This sort of problem, when stated in Prolog, reads a bit like a story book.  Our main goal (go) succeeds if we can move north from the middle of the 100x100 matrix, and update_win- which outputs the black/1 blocks.  The move/3 and direction/3 goals are really quite self explanatory, mirroring the instructions for the task.
{{works with|SWI Prolog|6.2.6 by Jan Wielemaker, University of Amsterdam}}
[[File:ant.jpg|thumb|right|Sample output]]

```prolog
%_______________________________________________________________
% Langtons ant.
:-dynamic
	black/1.

plot_point(Row, Col) :-   % Output a 5x5 black box at R,C
	new(C, box(5,5)), X is Col * 5 - 2, Y is Row * 5 - 2,
	send(C, colour, colour(black)), send(C, fill_pattern, colour(blue)),
	send(C, center(point(X,Y))), send(@win, display, C).
update_win :-  % Make a 500x500 window, find all the black points and plot them
	new(@win, window('Langtons Ant')),
	send(@win, size, size(500,500)), send(@win, open),
	black(Row/Col),plot_point(Row,Col),fail.
update_win.

direction(Row, Col, left) :- black(Row/Col), !, retract(black(Row/Col)).
direction(Row, Col, right):- not(black(Row/Col)), !, assert(black(Row/Col)).

move(_, Row,Col) :- (Row < 0; Col < 0; Row > 99; Col > 99), !.
move(north,Row,Col) :-
	(direction(Row,Col,left), C is Col - 1, !, move(west, Row, C));
	(direction(Row,Col,right), C is Col + 1, !, move(east, Row, C)).
move(south,Row,Col) :-
	(direction(Row,Col,right), C is Col - 1, !, move(west, Row, C));
	(direction(Row,Col,left), C is Col + 1, !, move(east, Row, C)).
move(east,Row,Col) :-
	(direction(Row,Col,right), R is Row + 1, !, move(south, R, Col));
	(direction(Row,Col,left), R is Row - 1, !, move(north, R, Col)).
move(west,Row,Col) :-
	(direction(Row,Col,left), R is Row + 1, !, move(south, R, Col));
	(direction(Row,Col,right), R is Row - 1, !, move(north, R, Col)).

go :-   retractall(black(_)), move(north,49,49), update_win.
```



## PureBasic

[[File:PureBasic_Langtons_ant.png|thumb|Sample display of PureBasic solution]]

```purebasic
#White = $FFFFFF
#Black = 0
#planeHeight = 100
#planeWidth = 100
#canvasID = 0
#windowID = 0
OpenWindow(#windowID, 0, 0, 150, 150, "Langton's ant", #PB_Window_SystemMenu | #PB_Window_ScreenCentered)
CanvasGadget(#canvasID, 25, 25, #planeWidth, #planeHeight)
StartDrawing(CanvasOutput(#canvasID))
  Box(0, 0, #planeWidth, #planeHeight, #White)
StopDrawing()

Define event, quit, ant.POINT, antDirection, antSteps

ant\x = #planeHeight / 2
ant\y = #planeWidth / 2
Repeat
  Repeat
    event = WindowEvent()
    If event = #PB_Event_CloseWindow
      quit = 1
      event = 0
    EndIf 
  Until event = 0

  StartDrawing(CanvasOutput(#canvasID))
    Select Point(ant\x, ant\y)
      Case #Black
        Plot(ant\x, ant\y, #White)
        antDirection = (antDirection + 1) % 4 ;turn left
      Case #White
        Plot(ant\x, ant\y, #Black)
        antDirection = (antDirection - 1 + 4) % 4 ;turn right
    EndSelect
  StopDrawing()

  Select antDirection
    Case 0 ;up
      ant\y - 1
    Case 1 ;left
      ant\x - 1
    Case 2 ;down
      ant\y + 1
    Case 3 ;right
      ant\x + 1
  EndSelect
  antSteps + 1
  
  If ant\x < 0 Or ant\x >= #planeWidth Or ant\y < 0 Or ant\y >= #planeHeight
    MessageRequester("Langton's ant status", "Out of bounds after " + Str(antSteps) + " steps.")
    quit = 1
  EndIf    
  
  Delay(10) ;control animation speed and avoid hogging CPU
Until quit = 1
```

Sample output:

```txt
Out of bounds after 11669 steps.
```



## Python


```python

"""Langton's ant implementation."""
from enum import Enum, IntEnum


class Dir(IntEnum):
    """Possible directions."""

    UP = 0
    RIGHT = 1
    DOWN = 2
    LEFT = 3


class Color(Enum):
    """Possible colors."""

    WHITE = " "
    BLACK = "#"


def invert_color(grid, x, y):
    """Invert the color of grid at x, y coordinate."""
    if grid[y][x] == Color.BLACK:
        grid[y][x] = Color.WHITE
    else:
        grid[y][x] = Color.BLACK


def next_direction(grid, x, y, direction):
    """Compute next direction according to current position and direction."""
    if grid[y][x] == Color.BLACK:
        turn_right = False
    else:
        turn_right = True
    direction_index = direction.value
    if turn_right:
        direction_index = (direction_index + 1) % 4
    else:
        direction_index = (direction_index - 1) % 4
    directions = [Dir.UP, Dir.RIGHT, Dir.DOWN, Dir.LEFT]
    direction = directions[direction_index]
    return direction


def next_position(x, y, direction):
    """Compute next position according to direction."""
    if direction == Dir.UP:
        y -= 1
    elif direction == Dir.RIGHT:
        x -= 1
    elif direction == Dir.DOWN:
        y += 1
    elif direction == Dir.LEFT:
        x += 1
    return x, y


def print_grid(grid):
    """Display grid."""
    print(80 * "#")
    print("\n".join("".join(v.value for v in row) for row in grid))


def ant(width, height, max_nb_steps):
    """Langton's ant."""
    grid = [[Color.WHITE] * width for _ in range(height)]
    x = width // 2
    y = height // 2
    direction = Dir.UP

    i = 0
    while i < max_nb_steps and 0 <= x < width and 0 <= y < height:
        invert_color(grid, x, y)
        direction = next_direction(grid, x, y, direction)
        x, y = next_position(x, y, direction)
        print_grid(grid)
        i += 1


if __name__ == "__main__":
    ant(width=75, height=52, max_nb_steps=12000)

```

The output is similar to the basic D version.


## R


```R

langton.ant = function(n = 100) {
	map = matrix(data = 0, nrow = n, ncol = n)
	p = floor(c(n/2, n/2))
	d = sample(1:4, 1)
	i = 1
	while(p[1] > 0 & p[1] <= n & p[2] > 0 & p[2] <= n) {
		if(map[p[1], p[2]] == 1) {
			map[p[1], p[2]] = 0
			p = p + switch(d, c(0, 1), c(-1, 0), c(0, -1), c(1, 0))
			d = ifelse(d == 4, 1, d + 1)
		} else {
			map[p[1], p[2]] = 1
			p = p + switch(d, c(0, -1), c(1, 0), c(0, 1), c(-1, 0))
			d = ifelse(d == 1, 4, d - 1)
		}
	}
	return(map)
}

image(langton.ant(), xaxt = "n", yaxt = "n", bty = "n")

```



## Racket

[[File:Racket_Langtons_ant.png|thumb|Sample display of Racket solution]]

This Racket program attempts to avoid mutation.


```racket
#lang racket

;; contracts allow us to describe expected behaviour of funcitons
(define direction/c (or/c 'u 'r 'l 'd))
(define turn/c (-> direction/c direction/c))
(define grid/c (hash/c integer? (hash/c integer? boolean?)))
(define-struct/contract ant ([d direction/c] [x integer?] [y integer?]))

(define/contract (turn-right dir) turn/c
  (case dir ((u) 'r) ((d) 'l) ((r) 'd) ((l) 'u)))

(define/contract (turn-left dir) turn/c
  (case dir ((u) 'l) ((d) 'r) ((r) 'u) ((l) 'd)))

(define/contract (move d x y)
  (-> direction/c integer? integer? (list/c direction/c integer? integer?))
  (list
   d
   (+ x (case d ((l) -1) ((r) 1) (else 0)))
   (+ y (case d ((u) -1) ((d) 1) (else 0)))))


(define/contract (move-ant d a) (-> direction/c ant? ant?)
  (apply make-ant (move d (ant-x a) (ant-y a))))

(define/contract (langton a grid) (-> ant? grid/c grid/c)
  (let ((ax (ant-x a)) (ay (ant-y a)))
    (if (and (<= 1 ax 100) (<= 1 ay 100))
        (let* ((grid-row (hash-ref grid ay hash))
               (cell-black? (hash-ref grid-row ax #f)))
          (langton
           (move-ant ((if cell-black? turn-left turn-right) (ant-d a)) a)
           (hash-set grid ay (hash-set grid-row ax (not cell-black?)))))
        grid)))

(define/contract (show-grid/text grid) (-> grid/c void?)
  (for* ; for* allows us to refer to y in rw
      ((y (in-range 1 101))
       (rw (in-value (hash-ref grid y #f)))
       #:when rw        ; if there is no row, the ant never visisted it
       #:when (newline) ; when can be used simply for its side effect
       (x (in-range 1 101)))
    (case (hash-ref rw x #\?)
      ((#\?) (display #\space)) ; distingush between "ant-visited white" vs. pure white
      ((#f)  (display #\:))     ; little anty footprints left
      ((#t)  (display #\#)))))


(show-grid/text (langton (make-ant 'u 50 50) (hash)))

(require 2htdp/image)
(define/contract (show-grid/png grid) (-> grid/c image?)
  (for*/fold
      ((scn (empty-scene 408 408)))
       ((y (in-range 1 101))
       (rw (in-value (hash-ref grid y #f)))
       #:when rw        ; if there is no row, the ant never visisted it
       (x (in-range 1 101)))
    (case (hash-ref rw x #\?)
      ((#\?) scn) ; distingush between "ant-visited white" vs. pure white
      ((#f)  (place-image (circle 2 "outline" "gray") (* x 4) (* y 4) scn))     ; little anty footprints left
      ((#t)  (place-image (circle 2 "solid" "black")  (* x 4) (* y 4) scn)))))
(show-grid/png (langton (make-ant 'u 50 50) (hash)))

```

Output (text):
<pre style="height:60ex;overflow:scroll">

                                         ##  ############  ##                                       
                                        #::####::::::::::# :##                                      
                                       ###:::##::::::::::::##:#                                     
                                       #:#::#:::::::::#::#::::#                                     
                                   ##  ##:#:#:::::::::###:::::::#                                   
                                ###:#::#:::#:::::#:::::##:##::###                                   
                                :#:#::###::##:####:##:::#:#::#:##  ##                               
                                :#:###:##: #:##::###:#:#:::::###:::###                              
                               #:::::#:::#####:#:#::####::#:::###:#:#:#                             
                              ###:##:::#:####::##:##:######:#:###:#:::#                             
                              #:###:#:##:#:#:##:##:##:#:::#####:###:##                              
                                ::#:#:::#:##:###:::#:::#:#::####::::# ##                            
                               #::#:::::::::##:##:::#::##:::::##:#::: :##                           
                              ###:::#:#:##:###::#::##:::::#:::###:##::##:#                          
                             #::###::##:::##:##:::###::#::::#::##:####:::#                          
                            ###:::#:::#:#::#:#:####:##::#:##:###::#:::::#                           
                           #::###::#:##::::#::#:###::#::::::###:##:#::#: ##                         
                          ###:::#:::  #:##:#:##::##::#####:####::####:##:::#                        
                         #::###::#:# #::#:###:#:#:##::::::##:::#:#:#::  #:::#                       
                        ###:::#::## ###::##:#:::##:::::::####:####:::#::::::#                       
                       #::###::#:#  #:::##::###########:#::####::#::: #::::#                        
                      ###:::#::##     :#:####::##::#########::#::##::::#::##                        
                     #::###::#:#   ## :#:##:::##:##:###:###:::#::#:##::####:#                       
                    ###:::#::##   #::#:######:##:#:##:#:#::::###:###:::##:::#                       
                   #::###::#:#   #:::::#####:#:#####:::::#:#::##:#::::##:::#                        
                  ###:::#::##    #:::::#:##:#####:##::#:#:::#::#::##:# :#::#                        
                 #::###::#:#     #::::#:::####:#::#####:##:::##########:::##                        
                ###:::#::##      #:##:::##:::#::#:::####::#:::##:####:##:::                         
               #::###::#:#        #####:#::##:::##:#:::#::::#:#::#::#::#:#:                         
              ###:::#::##          ##  ## #:#:#::::##:##:#:#:##::#::##::##:                         
             #::###::#:#                 #::#:   #:########:#:#:##::####:#:                         
            ###:::#::##                  #::#:  #:::::::##:##:::#::#::##:#:                         
           #::###::#:#                    #::#  #::::::#::##::##:::##:####:                         
          ###:::#::##                      ##   #:::::::##::##::::#:::#:###                         
         #::###::#:#                            #:##::####::::####:###:####                         
        ###:::#::##                              ##: ####:   ##: #:##:#:#::#                        
       #::###::#:#                                ##    ##    ## ###:##:#####                       
      ###:::#::##                                                #:##:#::####                       
     #::###::#:#                                                    :##:##:##                       
    ###:::#::##                                                     :##::::::                       
   #::###::#:#                                                     #:##::####:#                     
  ###:::#::##                                                     #::#:###::###                     
 #::###::#:#                                                      #:##:#  #::#                      
###:::#::##                                                        ##:     ##                       
:::##::#:#                                                          ##                              
##::#::##                                                                                           
 #:#:#:#                                                                                            
####:##                                                                                             
#:##:#                                                                                              
 ####                                                                                               
  ##                                                                                                

```



## REXX

This REXX program automatically justifies (or ''crops'') the '''left''', '''right''', '''top''' and '''bottom''' of the ant's walk field on 

the screen to display the maximum area of the ant's path (walk).

Or in other words, this REXX program only shows the pertinent part of the ant's walk─field.

```rexx
/*REXX program implements Langton's ant walk and displays the ant's path (finite field).*/
parse arg dir char seed .                        /*obtain optional arguments from the CL*/
if datatype(seed, 'W')   then call random ,,seed /*Integer? Then use it as a RANDOM SEED*/
if  dir=='' |  dir==","  then dir=random(1, 4)   /*ant is facing a random direction,    */
if char=='' | char==","  then char= '#'          /*binary colors:   0≡white,  1≡black.  */
parse value scrSize() with sd          sw .      /*obtain the terminal's depth and width*/
                           sd=sd -6;   sw=sw -1  /*adjust for terminal's useable area.  */
   xHome=1000000;       yHome=1000000            /*initially in the middle of nowhere.  */
 x=xHome;             y=yHome                    /*start ant's walk in middle of nowhere*/
$.=1;     $.0=4 ;   $.2=2;    $.3=3;   $.4=4     /* 1≡north   2≡east   3≡south   4≡west.*/
minX=x;   minY=y;   maxX=x;   maxY=y             /*initialize the min/max values for X,Y*/
@.=0                                             /*the universe  (walk field)  is white.*/
     do #=1  until (maxX-minY>sw)|(maxY-minY>sd) /*is the path out─of─bounds for screen?*/
     black=@.x.y;                  @.x.y= \@.x.y /*invert (flip)  ant's cell color code.*/
     if black  then dir=dir - 1                  /*if cell color was black,  turn  left.*/
               else dir=dir + 1                  /* "   "    "    "  white,  turn right.*/
     dir=$.dir                                   /*$ array handles/adjusts under & over.*/
               select                            /*ant walks the direction it's facing. */
               when dir==1  then y= y + 1        /*is ant walking north?  Then go up.   */
               when dir==2  then x= x + 1        /* "  "     "     east?    "  "  right.*/
               when dir==3  then y= y - 1        /* "  "     "    south?    "  "  down. */
               when dir==4  then x= x - 1        /* "  "     "     west?    "  "  left. */
               end   /*select*/                  /*the  DIRection  is always normalized.*/
     minX=min(minX, x);     maxX=max(maxX, x)    /*find the minimum and maximum of  X.  */
     minY=min(minY, y);     maxY=max(maxY, y)    /*  "   "     "     "     "     "  Y.  */
     end   /*steps*/                             /* [↑]  ant walks  hither and thither. */
                                                 /*finished walking, it's out─of─bounds.*/
say center(" Langton's ant walked "    #     ' steps ', sw, "─")
@.xHome.yHome='█'                                /*show the ant's initial starting point*/
@.x.y=        '∙'                                /*show where the ant went out─of─bounds*/
                                                 /* [↓]  show Langton's ant's trail.    */
     do    y=maxY  to minY  by -1;  _=           /*display a single  row  of cells.     */
        do x=minX  to maxX;         _=_ || @.x.y /*build a cell row for the display.    */
        end   /*x*/                              /* [↓]  strip trailing blanks from line*/
     _=strip( translate(_, char, 10),  'T')      /*color the cells:   black  or  white. */
     if _\==''  then say _                       /*display line (strip trailing blanks).*/
     end      /*y*/                              /*stick a fork in it,  we're all done. */
```

Programing note:   the 23<sup>rd</sup> REXX line:

```rexx
               when dir==4  then x= x - 1        /* "  "     "     west?    "  "  left. */
```

could've been coded as:

```rexx
               otherwise         x= x - 1        /* "  "     "     west?    "  "  left. */
```

The terminal's screen size used was   '''80'''<small>x</small>'''160'''.

The ant's walk starts at the   █   glyph   (in the middle of the "fist")   and   ends at   <big><b>'''∙'''</b></big>   (at the very top of the output)   where it goes out─of─bounds.

{{out|output|text=  when using the default inputs:}}

(Shown at <sup>'''1'''</sup>/<sub>'''2'''</sub> size.)
<b>
<pre style="font-size:50%">
                                                                                                     ∙
                                                                                                     # #
                                                                                                   ## # #
                                                                                                  ###   #
                                                                                                 # ##  #
                                                                                                ## ### #
                                                                                               # # #  ###
                                                                                              ##  # #####
                                                                                             # #  ###  #
                                                                                            ##  #   ###
                                                                                           # #  ###  #
                                                                                          ##  #   ###
                                                                                         # #  ###  #
                                                                                        ##  #   ###
                                                                                       # #  ###  #
                                                                                      ##  #   ###
                                                                                     # #  ###  #
                                                                                    ##  #   ###
                                                                                   # #  ###  #
                                                                                  ##  #   ###
                                                                                 # #  ###  #
                                                                                ##  #   ###
                                                                               # #  ###  #
                                                                              ##  #   ###
                                                                             # #  ###  #
                                                                            ##  #   ###
                                                                           # #  ###  #
                                                                          ##  #   ###
                                                                         # #  ###  #
                                                                        ##  #   ###
                                                                       # #  ###  #
                                                                      ##  #   ###
         ##                                                          # #  ###  #
  ##      ##                                                        ##  #   ###
 #  #  # ## #                                                      # #  ###  #
###  ### #  #                                                     ##  #   ###
# ####  ## #                                                     # #  ###  #
        ##                                                      ##  #   ###
  ## ## ##                                                     # #  ###  #
  ####  # ## #                                                ##  #   ###
  ##### ## ### ##    ##    ##                                # #  ###  #
   #  # # ## #  ##    ####  ##                              ##  #   ###
    #### ### ####    ####  ## #                            # #  ###  #
    ### #   #    ##  ##       #   ##                      ##  #   ###
     #### ##   ##  ##  #      #  #  #                    # #  ###  #
     # ##  #  #   ## ##       #   #  #                  ##  #   ###
     # ####  ## # # ######## #    #  #                 # #  ###  #
     ##  ##  #  ## # # ## ##    # # # ##  ##          ##  #   ###
     # #  #  #  # #    #   # ##   ##  # #####        # #  ###  #
       ## #### ##   #  ####   #  #   ##   ## #      ##  #   ###
   ##   ##########   ## #####  # ####   #    #     # #  ###  #
   #  #  # ##  #  #   # #  ## ##### ## #     #    ##  #   ###
   #   ##    # ##  # #     ##### # #####     #   # #  ###  #
  #   ##   ### ###    # # ## # ## ###### #  #   ##  #   ###
  # ####  ## #  #   ### ### #█ ##   ## #  ##   # #  ###  #
   ##  #    ##  #  #########  ##  #### #      ##  #   ###
   #    #    #  ####  # ###########  ##   #  # #  ###  #
  #      #   #### ####       ##   # ##  ### ##  #   ###
  #   #    # # #   ##      ## # # ### #  # # #  ###  #
   #   ## ####  #### #####  ##  ## # ## #     #   ###
    ##  #  # ## ###      #  ### #  #    ## #  ###  #
      #     #  ### ## #  ## #### # #  # #   #   ###
     #   #### ##  #    #  ###   ## ##   ##  ###  #
     # ##  ## ###   #     ##  #  ### ## # #   ###
      ##     # ##     ##  #   ## ##         #  #
       ## #    ####  # #   #   ### ## #   # #
         ## ### #####   # ## ## ## # # ## # ### #
        #   # ### # ###### ## ##  #### #   ## ###
        # # # ###   #  ####  # # #####   #     #
         ###   ###     # # ###  ## #  ## ### #
          ##  ## #  # #   ## #### ##  ###  # #
              ###  ## ##     #     #   #  # ###
              #       ###         # # ##  ##
                #    #  #         #  # #
                # ##            ##   ###
                 ##  #          ####  #
                  ##  ############  ##

```

</b>


## Ring


```ring

load "guilib.ring"
load "stdlib.ring"

new qapp 
        {
        win1 = new qwidget() {
                   setwindowtitle("drawing using qpainter")
                   setgeometry(100,100,500,500)
                   label1 = new qlabel(win1) {
                             setgeometry(10,10,400,400)
                             settext("")
                  }
                  new qpushbutton(win1) {
                         setgeometry(200,400,100,30)
                         settext("draw")
                         setclickevent("draw()")
                  }
                  show()
         }
         exec()
         }

func draw
        p1 = new qpicture()
             color = new qcolor() {
             setrgb(0,0,255,255)
        }
        pen = new qpen() {
                  setcolor(color)
                  setwidth(1)
        }
        new qpainter() {
               begin(p1)
               setpen(pen)

        fieldsize=100
        field = newlist(fieldsize,fieldsize)   
        x=fieldsize/2
        y=fieldsize/2  
        d=0
        while x<=fieldsize and x>=0 and y<=fieldsize and y>=0
                if field[x][y]=0  field[x][y]=1 d-=1 else field[x][y]=0 d+=1 ok
                drawpoint(x*2, y*2)     
                d=(d+4) % 4 
                switch d 
                        on 0 y+=1
                        on 1 x+=1
                        on 2 y-=1
                        on 3 x-=1
                off
        end 

        endpaint()
        }
        label1 { setpicture(p1) show() }

```


Output:

[[File:CalmoSoftLangtons.jpg]]


## Ruby


```ruby
class Ant
  
  class OutOfBoundsException < StandardError; end
  
  class Plane
    def initialize(x, y)
      @size_x, @size_y = x, y
      @cells = Array.new(y) {Array.new(x, :white)}
    end
    
    def white?(px, py)
      @cells[py][px] == :white
    end
    
    def toggle_colour(px, py)
      @cells[py][px] = (white?(px, py) ? :black : :white)
    end
    
    def check_bounds(px, py)
      unless (0 <= px and px < @size_x) and (0 <= py and py < @size_y)
        raise OutOfBoundsException, "(#@size_x, #@size_y)"
      end
    end
    
    def to_s
      @cells.collect {|row|
        row.collect {|cell| cell == :white ? "." : "#"}.join + "\n"
      }.join
    end
  end
  
  dir_move = [[:north, [0,-1]], [:east, [1,0]], [:south, [0,1]], [:west, [-1,0]]]
  Move = Hash[dir_move]
  directions = dir_move.map{|dir, move| dir}       # [:north, :east, :south, :west]
  Right = Hash[ directions.zip(directions.rotate).to_a ]
  Left  = Right.invert
  
  def initialize(size_x, size_y, pos_x=size_x/2, pos_y=size_y/2)
    @plane = Plane.new(size_x, size_y)
    @pos_x, @pos_y = pos_x, pos_y
    @direction = :south
    @plane.check_bounds(@pos_x, @pos_y)
  end
  
  def run
    moves = 0
    loop do
      begin
        moves += 1
        move
      rescue OutOfBoundsException
        break
      end
    end
    moves
  end
  
  def move
    @plane.toggle_colour(@pos_x, @pos_y)
    advance
    if @plane.white?(@pos_x, @pos_y)
      @direction = Right[@direction]
    else
      @direction = Left[@direction]
    end
  end
  
  def advance
    dx, dy = Move[@direction]
    @pos_x += dx
    @pos_y += dy
    @plane.check_bounds(@pos_x, @pos_y)
  end
  
  def position
    "(#@pos_x, #@pos_y)"
  end
  
  def to_s
    @plane.to_s
  end
end

#
# the simulation
#
ant = Ant.new(100, 100)
moves = ant.run
puts "out of bounds after #{moves} moves: #{ant.position}"
puts ant
```


{{out}}
<pre style="height: 40ex; overflow: scroll">out of bounds after 11669 moves: (26, -1)
..........................#.#.......................................................................
........................##.#.#......................................................................
.......................#.###.##.....................................................................
......................####.###.#....................................................................
......................#####.#..##...................................................................
.......................#...##.##.#..................................................................
........................###...#..##.................................................................
.........................#...##.##.#................................................................
..........................###...#..##...............................................................
...........................#...##.##.#..............................................................
............................###...#..##.............................................................
.............................#...##.##.#............................................................
..............................###...#..##...........................................................
...............................#...##.##.#..........................................................
................................###...#..##.........................................................
.................................#...##.##.#........................................................
..................................###...#..##.......................................................
...................................#...##.##.#......................................................
....................................###...#..##.....................................................
.....................................#...##.##.#....................................................
......................................###...#..##...................................................
.......................................#...##.##.#..................................................
........................................###...#..##.................................................
.........................................#...##.##.#................................................
..........................................###...#..##...............................................
...........................................#...##.##.#..............................................
............................................###...#..##.............................................
.............................................#...##.##.#............................................
..............................................###...#..##...........................................
...............................................#...##.##.#..........................................
................................................###...#..##.........................................
.................................................#...##.##.#..##....................................
..................................................###...#..##..##...................................
...................................................#...##.##..##...#................................
.............................................####...###...#...#..###................................
............................................#....#...#...##.####...#................................
...........................................###....#...#.#......#.##.#...............................
...........................................###....#.##.....#.##..#.##...............................
............................................#....#...##.#.#.....##..................................
............................................#.#......#.#####..#...#.................................
...........................................#...#####..........##.######.............................
...........................................###..##..#.##.#.#.#...##.#.##............................
.........................................##..#.#######.#...#..###....##.#...........................
........................................#..#..######.##...#..#.##...#...#...........................
.......................................#....#.#.##.#..######.#######...#............................
.......................................#.####.##.#.####....##..##.#.##.#............................
........................................#....####...#..#.######.##....###...........................
...........................................#...#.##.#.###.#..##..##...###...........................
..............................................#######....#..##.##.#.....#...........................
......................................####..##.##..####.##.##.##..#.....#...........................
.....................................#....#.#...###.##.###....#.####....#...........................
....................................###.......###.#.#.#####....#.#......#...........................
....................................#.#...###.####.##.#...##.###.##.....#...........................
..........................................##.##..####....####.#.#.#.....#...........................
.....................................#....#..##...###..###.....###......#...........................
.....................................##...##.###.####..#......###...##..#...........................
.....................................##.#.####.....#...#..#.##.###.##...#...........................
....................................####.##...##.####..#.#..#..#..###...#...........................
....................................#.##.###..#.#.##.#.#.....#.#.....#.#............................
........................................#.#..#....##.##..#.#..###.##................................
........................................##.#....#..#####.#....#....#..#.#...........................
.......................................#.##.#..#....##.##.#..###......###...........................
.....................................#.#...#..#..#..#..###...##..##....#............................
....................................###.#.#####.######.###.#######.#.##.............................
....................................#.#.#....#####...##..#####.#####................................
......................................#..##...#......#..#.##..###.###...............................
...................................####...#####.#########...#.#.....................................
..............................##....#..#.....###.#.#...#.###..###...................................
.............................#..#..####.##...###.##...###.##.....##.................................
............................###....#.##.#.#####...#....#..#..##.###.................................
............................#.#####.#.#...##..##.....#....#...#..#..................................
................................######.####..##.#...#..##..#.#.##...................................
..............................##......#.###.##..####...#...###......................................
...............................#..#.#####..#...#.##...#..#..#.......................................
...............................##.###.#######.....#.....#.##........................................
..............................#.#..##.##......#...##....#...........................................
.............................#..#.####........###..##..#............................................
.............................#.##.###............##..##.............................................
..............................##....................................................................
...............................##...................................................................
....................................................................................................
....................................................................................................
....................................................................................................
....................................................................................................
....................................................................................................
....................................................................................................
....................................................................................................
....................................................................................................
....................................................................................................
....................................................................................................
....................................................................................................
....................................................................................................
....................................................................................................
....................................................................................................
....................................................................................................
....................................................................................................
....................................................................................................
....................................................................................................
....................................................................................................
....................................................................................................
```

'''Simple Version:'''

```ruby
class Ant
  MOVE = [[1,0], [0,1], [-1,0], [0,-1]]   # [0]:east, [1]:south, [2]:west, [3]:north
  
  def initialize(size_x, size_y, pos_x=size_x/2, pos_y=size_y/2)
    @plane = Array.new(size_y) {Array.new(size_x, true)}  # true -> white, false -> black
    @sx, @sy = size_x, size_y
    @px, @py = pos_x, pos_y       # start position
    @direction = 0                # south
    @moves = 0
    move  while (0 <= @px and @px < @sx) and (0 <= @py and @py < @sy)
  end
  
  def move
    @moves += 1
    @direction = (@plane[@py][@px] ? @direction+1 : @direction-1) % 4
    @plane[@py][@px] = !@plane[@py][@px]
    @px += MOVE[@direction][0]
    @py += MOVE[@direction][1]
  end
  
  def to_s
    ["out of bounds after #{@moves} moves: (#@px, #@py)"] +
      (0...@sy).map {|y| (0...@sx).map {|x| @plane[y][x] ? "." : "#"}.join}
  end
end

puts Ant.new(100, 100).to_s
```

;Output is the same above.


## Run BASIC



```Runbasic
dim plane(100,100)
x  = 50: y = 50: minY = 100

while (x>0) and (x<100) and (y>0) and (y<100)
 if plane(x,y) then
   nxt = nxt - 1
   if nxt < 1 then nxt = 4
  else
   nxt = nxt + 1
   if nxt > 4 then nxt = 1
 end if

 x          = x + (nxt = 2) - (nxt = 4)
 y          = y + (nxt = 3) - (nxt = 1)
 plane(x,y) = (plane(x,y) <> 1)
 minY       = min(y,minY)          ' find lowest and 
 maxY       = max(y,maxY)          '  highest y to prevent printing blank lines
wend

graphic #g, 100,100
for y = minY to maxY
  for x = 1 to 100
   print chr$((plane(x,y)*3) + 32);
     if plane(x,y) = 1 then  #g "color green ; set "; x; " "; y else #g "color blue ; set "; x; " "; y
  next x
  print y
next y
render #g 
#g "flush""
```


Ouptut (Produces both character and graphic):[[File:Langtons_ant_run_basic.png‎|right|graphic]]
<pre style="height: 40ex; overflow: scroll">                                                                                                    20
                                                                   ##                               21
                                                                    ##                              22
                                             ##  ##            ### ## #                             23
                                            #  ##  ###        #### #  #                             24
                                           #    ##   #      ## ##  # #                              25
                                        ## #     #     ####### ### ##                               26
                                       #  #  #   ## #   #  ##### #  #                               27
                                      ###   #   ####  ## ### #      ##                              28
                                   ## # #  ##  #   # ##  #### ######                                29
                                  #  #   #    #     ##  ##   # # ##### #                            30
                                 ### ##  #  #    #   ##### # ## #    ###                            31
                                 ##     ## ###   ## ###   ## ####  #  #                             32
                                   ###  ### #   # # ###     #  #    ##                              33
                                     # #   ######### #####   ####                                   34
                               ### ###  ## #  #      #   ##  #                                      35
                                ##### #####  ##   #####    # # #                                    36
                             ## # ####### ### ###### ##### # ###                                    37
                            #    ##  ##   ###  #  #  #  #   # #                                     38
                           ###      ###  # ## ##    #  # ## #                                       39
                           # #  #    #    # #####  #    # ##                                        40
                                ## ###  # #  ## ##    #  # #                                        41
                            # #     # #     # # ## # #  ### ## #                                    42
                           #   ###  #  #  # #  #### ##   ## ####                                    43
                           #   ## ### ## #  #   #     #### # ##                                     44
                           #  ##   ###      #  #### ### ##   ##                                     45
                           #      ###     ###  ###   ##  #    #                                     46
                           #     # # # ####    ####  ## ##                                          47
                           #     ## ### ##   # ## #### ###   # #                                    48
                           #      # #    ##### # # ###       ###                                    49
                           #    #### #    ### ## ###   # #    #                                     50
                           #     #  ## ## ## ####  ## ##  ####                                      51
                           #     # ## ##  #    #######                                              52
                           ###   ##  ##  # ### # ## #   #                                           53
                           ###    ## ###### #  #   ####    #                                        54
                            # ## # ##  ##    #### # ## #### #                                       55
                            #   ####### ######  # ## # #    #                                       56
                           #   #   ## #  #   ## ######  #  #                                        57
                           # ##    ###  #   # ####### #  ##                                         58
                            ## # ##   # # # ## #  ##  ###                                           59
                             ###### ##          #####   #                                           60
                                 #   #  ##### #      # #                                            61
                                  ##     # # ##   #    #                                            62
                               ## #  ## #     ## #    ###                                           63
                               # ## #      # #   #    ###                                           64
                                #   #### ##   #   #    #                                            65
                                ###  #   #   ###   ####                                             66
                                #   ##  ## ##   #                                                   67
                                   ##  ##  #   ###                                                  68
                                    ##  # ## ##   #                                                 69
                                         ##  #   ###                                                70
                                          # ## ##   #                                               71
                                           ##  #   ###                                              72
                                            # ## ##   #                                             73
                                             ##  #   ###                                            74
                                              # ## ##   #                                           75
                                               ##  #   ###                                          76
                                                # ## ##   #                                         77
                                                 ##  #   ###                                        78
                                                  # ## ##   #                                       79
                                                   ##  #   ###                                      80
                                                    # ## ##   #                                     81
                                                     ##  #   ###                                    82
                                                      # ## ##   #                                   83
                                                       ##  #   ###                                  84
                                                        # ## ##   #                                 85
                                                         ##  #   ###                                86
                                                          # ## ##   #                               87
                                                           ##  #   ###                              88
                                                            # ## ##   #                             89
                                                             ##  #   ###                            90
                                                              # ## ##   #                           91
                                                               ##  #   ###                          92
                                                                # ## ##   #                         93
                                                                 ##  #   ###                        94
                                                                  # ## ##   #                       95
                                                                   ##  # #####                      96
                                                                    # #   ####                      97
                                                                     ## ### #                       98
                                                                      # # ##                        99
                                                                                                    100

```



## Rust


```rust
struct Ant {
    x: usize,
    y: usize,
    dir: Direction
}

#[derive(Clone,Copy)]
enum Direction {
    North,
    East,
    South,
    West
}

use Direction::*;

impl Ant {
    fn mv(&mut self, vec: &mut Vec<Vec<u8>>) {
        let pointer = &mut vec[self.y][self.x];
        //change direction
        match *pointer {
            0 => self.dir = self.dir.right(),
            1 => self.dir = self.dir.left(),
            _ => panic!("Unexpected colour in grid")
        }
        //flip colour
        //if it's 1 it's black
        //if it's 0 it's white
        *pointer ^= 1;

        //move direction
        match self.dir {
            North => self.y -= 1,
            South => self.y += 1,
            East => self.x += 1,
            West => self.x -= 1,
        }

    }
}

impl Direction {
    fn right(self) -> Direction {
        match self {
            North => East,
            East => South,
            South => West,
            West => North,
        }
    }

    fn left(self) -> Direction {
        //3 rights equal a left
        self.right().right().right()
    }
}

fn main(){
    //create a 100x100 grid using vectors
    let mut grid: Vec<Vec<u8>> = vec![vec![0; 100]; 100];
    let mut ant = Ant {
        x: 50, y: 50, dir: Direction::North
    };

    while ant.x < 100 && ant.y < 100 {
        ant.mv(&mut grid);
    }
    for each in grid.iter() {
        //construct string
        //using iterator methods to quickly convert the vector
        //to a string
        let string = each.iter()
                         .map(|&x| if x == 0 { " " } else { "#" })
                         .fold(String::new(), |x, y| x+y);
        println!("{}", string);
    }
}
```



## Scala


```scala
class Langton(matrix:Array[Array[Char]], ant:Ant) {
  import Langton._
  val rows=matrix.size
  val cols=matrix(0).size
	
  def isValid = 0 <= ant.row && ant.row < cols && 0 <= ant.col  && ant.col < rows
  def isBlack=matrix(ant.row)(ant.col)==BLACK
  def changeColor(c:Char)={matrix(ant.row)(ant.col)=c; matrix}
	
  def evolve():Langton={
    val (newCol, newAnt)=if(isBlack) (WHITE, ant.turnLeft) else (BLACK, ant.turnRight)
    new Langton(changeColor(newCol), newAnt.move)
  }
  override def toString()=matrix map (_.mkString("")) mkString "\n"	
}

case class Ant(row:Int, col:Int, d:Int=0) {
  def turnLeft=Ant(row,col,(d-1)&3)
  def turnRight=Ant(row,col,(d+1)&3)
  def move=d match {
    case 0 => Ant(row-1,col,d)	// north
    case 1 => Ant(row,col+1,d)	// east
    case 2 => Ant(row+1,col,d)	// south
    case 3 => Ant(row,col-1,d)	// west
  }
}

object Langton {
  val BLACK='#'
  val WHITE='.'
  def apply(x:Int=100, y:Int=100)=new Langton(Array.fill(y, x)(WHITE), Ant(x>>>1, y>>>1, 0))
	
  def main(args: Array[String]): Unit = {
    var l=Langton(100,100)
    var moves=0
    while (l.isValid) {
      moves += 1
      l=l.evolve
    }
    println("Out of bounds after "+moves+" moves")
    println(l)
  }
}
```

Output:
<pre style="height: 40ex; overflow: scroll">Out of bounds after 11669 moves
....................................................................................................
....................................................................................................
....................................................................................................
....................................................................................................
....................................................................................................
....................................................................................................
....................................................................................................
....................................................................................................
....................................................................................................
....................................................................................................
....................................................................................................
....................................................................................................
....................................................................................................
....................................................................................................
....................................................................................................
....................................................................................................
....................................................................................................
....................................................................................................
....................................................................................................
....................................................................................................
....................................................................................................
....................................................................................................
....................................................................................................
....................................................................................................
....................................................................................................
....................................................................................................
....................................................................................................
....................................................................................................
..........................................##..############..##......................................
.........................................#..####..........#..##.....................................
........................................###...##............##.#....................................
........................................#.#..#.........#..#....#....................................
....................................##..##.#.#.........###.......#..................................
.................................###.#..#...#.....#.....##.##..###..................................
..................................#.#..###..##.####.##...#.#..#.##..##..............................
..................................#.###.##..#.##..###.#.#.....###...###.............................
................................#.....#...#####.#.#..####..#...###.#.#.#............................
...............................###.##...#.####..##.##.######.#.###.#...#............................
...............................#.###.#.##.#.#.##.##.##.#...#####.###.##.............................
...................................#.#...#.##.###...#...#.#..####....#.##...........................
................................#..#.........##.##...#..##.....##.#.....##..........................
...............................###...#.#.##.###..#..##.....#...###.##..##.#.........................
..............................#..###..##...##.##...###..#....#..##.####...#.........................
.............................###...#...#.#..#.#.####.##..#.##.###..#.....#..........................
............................#..###..#.##....#..#.###..#......###.##.#..#..##........................
...........................###...#.....#.##.#.##..##..#####.####..####.##...#.......................
..........................#..###..#.#.#..#.###.#.#.##......##...#.#.#....#...#......................
.........................###...#..##.###..##.#...##.......####.####...#......#......................
........................#..###..#.#..#...##..###########.#..####..#....#....#.......................
.......................###...#..##......#.####..##..#########..#..##....#..##.......................
......................#..###..#.#...##..#.##...##.##.###.###...#..#.##..####.#......................
.....................###...#..##...#..#.######.##.#.##.#.#....###.###...##...#......................
....................#..###..#.#...#.....#####.#.#####.....#.#..##.#....##...#.......................
...................###...#..##....#.....#.##.#####.##..#.#...#..#..##.#..#..#.......................
..................#..###..#.#.....#....#...####.#..#####.##...##########...##.......................
.................###...#..##......#.##...##...#..#...####..#...##.####.##...........................
................#..###..#.#........#####.#..##...##.#...#....#.#..#..#..#.#.........................
...............###...#..##..........##..##.#.#.#....##.##.#.#.##..#..##..##.........................
..............#..###..#.#.................#..#....#.########.#.#.##..####.#.........................
.............###...#..##..................#..#...#.......##.##...#..#..##.#.........................
............#..###..#.#....................#..#..#......#..##..##...##.####.........................
...........###...#..##......................##...#.......##..##....#...#.###........................
..........#..###..#.#............................#.##..####....####.###.####........................
.........###...#..##..............................##..####....##..#.##.#.#..#.......................
........#..###..#.#................................##....##....##.###.##.#####......................
.......###...#..##................................................#.##.#..####......................
......#..###..#.#.....................................................##.##.##......................
.....###...#..##......................................................##............................
....#..###..#.#.....................................................#.##..####.#....................
...###...#..##.....................................................#..#.###..###....................
..#..###..#.#......................................................#.##.#..#..#.....................
.###...#..##........................................................##......##......................
#..###..#.#..........................................................##.............................
.###.#..##..........................................................................................
#.#.#.#.#...........................................................................................
.####.##............................................................................................
.#.##.#.............................................................................................
..####..............................................................................................
...##...............................................................................................
....................................................................................................
....................................................................................................
....................................................................................................
....................................................................................................
....................................................................................................
....................................................................................................
....................................................................................................
....................................................................................................
....................................................................................................
....................................................................................................
....................................................................................................
....................................................................................................
....................................................................................................
....................................................................................................
....................................................................................................
....................................................................................................
....................................................................................................
....................................................................................................
....................................................................................................
....................................................................................................
....................................................................................................
```



## Seed7


```seed7
$ include "seed7_05.s7i";

const type: direction is new enum UP, RIGHT, DOWN, LEFT end enum;
 
const proc: main is func
  local
    const integer: width is 75;
    const integer: height is 52;
    var array array boolean: m is height times width times FALSE;
    var direction: dir is UP;
    var integer: x is width div 2;
    var integer: y is height div 2;
  begin
    while x in {1 .. width} and y in {1 .. height} do
      dir := direction conv ((ord(dir) + 2 * ord(m[y][x]) - 1) mod 4);
      m[y][x] := not m[y][x];
      case dir of
        when {UP}:    decr(y);
        when {RIGHT}: decr(x);
        when {DOWN}:  incr(y);
        when {LEFT}:  incr(x);
      end case;
    end while;
    for key x range m do
      for y range 1 to width do
        write(".#"[succ(ord(m[x][y]))]);
      end for;
      writeln;
    end for;
  end func;
```


{{out}}

```txt

...........................................................................
...........................................................................
...........................................................................
............................##..############..##...........................
...........................#..####..........#..##..........................
..........................###...##............##.#.........................
..........................#.#..#.........#..#....#.........................
......................##..##.#.#.........###.......#.......................
...................###.#..#...#.....#.....##.##..###.......................
....................#.#..###..##.####.##...#.#..#.##..##...................
....................#.###.##..#.##..###.#.#.....###...###..................
..................#.....#...#####.#.#..####..#...###.#.#.#.................
.................###.##...#.####..##.##.######.#.###.#...#.................
.................#.###.#.##.#.#.##.##.##.#...#####.###.##..................
.....................#.#...#.##.###...#...#.#..####....#.##................
..................#..#.........##.##...#..##.....##.#.....##...............
.................###...#.#.##.###..#..##.....#...###.##..##.#..............
................#..###..##...##.##...###..#....#..##.####...#..............
...............###...#...#.#..#.#.####.##..#.##.###..#.....#...............
..............#..###..#.##....#..#.###..#......###.##.#..#..##.............
.............###...#.....#.##.#.##..##..#####.####..####.##...#............
............#..###..#.#.#..#.###.#.#.##......##...#.#.#....#...#...........
...........###...#..##.###..##.#...##.......####.####...#......#...........
..........#..###..#.#..#...##..###########.#..####..#....#....#............
.........###...#..##......#.####..##..#########..#..##....#..##............
........#..###..#.#...##..#.##...##.##.###.###...#..#.##..####.#...........
.......###...#..##...#..#.######.##.#.##.#.#....###.###...##...#...........
......#..###..#.#...#.....#####.#.#####.....#.#..##.#....##...#............
.....###...#..##....#.....#.##.#####.##..#.#...#..#..##.#..#..#............
....#..###..#.#.....#....#...####.#..#####.##...##########...##............
...###...#..##......#.##...##...#..#...####..#...##.####.##................
..#..###..#.#........#####.#..##...##.#...#....#.#..#..#..#.#..............
.###...#..##..........##..##.#.#.#....##.##.#.#.##..#..##..##..............
#..###..#.#.................#..#....#.########.#.#.##..####.#..............
.###.#..##..................#..#...#.......##.##...#..#..##.#..............
#.#.#.#.#....................#..#..#......#..##..##...##.####..............
.####.##......................##...#.......##..##....#...#.###.............
.#.##.#............................#.##..####....####.###.####.............
..####..............................##..####....##..#.##.#.#..#............
...##................................##....##....##.###.##.#####...........
....................................................#.##.#..####...........
........................................................##.##.##...........
........................................................##.................
......................................................#.##..####.#.........
.....................................................#..#.###..###.........
.....................................................#.##.#..#..#..........
......................................................##......##...........
.......................................................##..................
...........................................................................
...........................................................................
...........................................................................
...........................................................................

```



## Scilab

{{works with|Scilab|5.4.1 or above}}
<lang>grid_size=100;                              //side length of the square grid
ant_pos=round([grid_size/2 grid_size/2]);   //ant's initial position at center of grid
head_direction='W';                         //ant's initial direction can be either
                                            //'N' north, 'S' south, 'E' east, or 'W' west

grid=~zeros(grid_size,grid_size)    //blank grid
col=[];                             //cell color handler
next_step=%T;                       //step flag
i=0;                                //step counter

while next_step
    
    col=grid(ant_pos(1),ant_pos(2));    //get cell color
    
    if col then                         //if white cell
        grid(ant_pos(1),ant_pos(2))=~grid(ant_pos(1),ant_pos(2));  //switch color
        if head_direction=='N' then     //if head to N
            head_direction='E';         //turn right to E
            ant_pos(2)=ant_pos(2)+1;    //step forward
            
        elseif head_direction=='E' then //if head to E
            head_direction='S';         //turn right to S
            ant_pos(1)=ant_pos(1)+1;    //step forward
            
        elseif head_direction=='S' then //if head to S
            head_direction='W';         //turn right to W
            ant_pos(2)=ant_pos(2)-1;    //step forward
            
        elseif head_direction=='W' then //if head to W
            head_direction='N';         //turn right to N
            ant_pos(1)=ant_pos(1)-1;    //step forward
        end
    else                                //if black cell
        grid(ant_pos(1),ant_pos(2))=~grid(ant_pos(1),ant_pos(2));  //switch color
        if head_direction=='N' then     //if head to N
            head_direction='W';         //turn left to E
            ant_pos(2)=ant_pos(2)-1;    //step foward
            
        elseif head_direction=='W' then //if head to W
           head_direction='S';          //turn left to S
            ant_pos(1)=ant_pos(1)+1;    //step forward
            
        elseif head_direction=='S' then //if head to S
            head_direction='E';         //turn left to E
            ant_pos(2)=ant_pos(2)+1;    //step forward
            
        elseif head_direction=='E' then //if head to E
            head_direction='N';         //turn left to N
            ant_pos(1)=ant_pos(1)-1;    //step forward
        end
    end
    
    i=i+1;
    
    if ant_pos(1)<1 | ant_pos(1)>100 | ant_pos(2)<0 | ant_pos(2)>100 then   //check ant's position
        disp("Out of bounds after "+string(i)+" steps");
        next_step=~next_step;                                               //break loop if out of bounds
    end
end

ascii_grid=string(zeros(grid));     //create grid of chars to display
                                    //on the console
for a=1:length(grid)
    if grid(a) then
        ascii_grid(a)=" ";          //blank space if cell is white
    else
        ascii_grid(a)="#";          //# if cell is black
    end
end

disp(ascii_grid);
```

{{out}}
<pre style="font-size: 10px">
 Out of bounds after 11669 steps   
 
!                                                                                                                                                                                                                                                                                                            !
!                                                                                                                                                                                                                                                                                                            !
!                                                                                                                                                                                                                                                                                                            !
!                                                                                                                                                                                                                                                                                                            !
!                                                                                                                                                                                                                                                                                                            !
!                                                                                                                                                                                                                                                                                                            !
!                                                                                                                                                                                                                                                                                                            !
!                                                                                                                                                                                                                                                                                                            !
!                                                                                                                                                                                                                                                                                                            !
!                                                                                                                                                                                                                                                                                                            !
!                                                                                                                                                                                                                                                                                                            !
!                                                                                                                                                                                                                                                                                                            !
!                                                                                                                                                                                                                                                                                                            !
!                                                                                                                                                                                                                                                                                                            !
!                                                                                                                                                                                                                                                                                                            !
!                                                                                                                                                                                                                                                                                                            !
!                                                                                                                                                                                                                                                                                                            !
!                                                                                                                                                                                                                                                                                                            !
!                                                                                                                                                                                                                                                                                                            !
!                                                                                                                                                                                                                                                                                                            !
!                                                                                                                                                                                                                                                                                                            !
!                                                                                                                                                                                                                                                                                                            !
!                                                                                                                                                                                                                                                                                                            !
!                                                                                                                                                                                                                                                                                                            !
!                                                                                                                                                                                                                                                                                                            !
!                                                                                                                                                                                                                                                                                                            !
!                                                                                                                                                                                                                                                                                                            !
!                                                                                                                                                                                                                                                                                                            !
!                                                                                                                                                                                                                                                                                                            !
!                                                                                                                                                                                                                                                                                                            !
!                                                                                                                                                                                                                                                                                                            !
!                                                                                                                                                                                                                                                                                                            !
!                                                                                                                                                                                                                                                                                                            !
!                                                                                                                                                                                                                                                                                                            !
!                                                                                                                                                                                                                                                                                                            !
!                                                                                                                                                                                                                                                                                                            !
!                                                                                                                                                                                                                                                                                                            !
!                                                                                                                                                                                                                                                                                                            !
!                                                                                                                                                                                                                                                                                                            !
!                                                                                                                                                                                                                                                                                                            !
!                                                                                                                                                                                                         #  #                                                                                               !
!                                                                                                                                                                                                                                                                                                            !
!                                                                                                                                                                                                            #  #                                                                                            !
!                                                                                                                                                                                                                                                                                                            !
!                                                                                                                                       #  #        #  #                                      #  #  #     #  #     #                                                                                         !
!                                                                                                                                                                                                                                                                                                            !
!                                                                                                                                    #        #  #        #  #  #                          #  #  #  #     #        #                                                                                         !
!                                                                                                                                                                                                                                                                                                            !
!                                                                                                                                 #              #  #           #                    #  #     #  #        #     #                                                                                            !
!                                                                                                                                                                                                                                                                                                            !
!                                                                                                                        #  #     #                 #                 #  #  #  #  #  #  #     #  #  #     #  #                                                                                               !
!                                                                                                                                                                                                                                                                                                            !
!                                                                                                                     #        #        #           #  #     #           #        #  #  #  #  #     #        #                                                                                               !
!                                                                                                                                                                                                                                                                                                            !
!                                                                                                                  #  #  #           #           #  #  #  #        #  #     #  #  #     #                    #  #                                                                                            !
!                                                                                                                                                                                                                                                                                                            !
!                                                                                                         #  #     #     #        #  #        #           #     #  #        #  #  #  #     #  #  #  #  #  #                                                                                                  !
!                                                                                                                                                                                                                                                                                                            !
!                                                                                                      #        #           #              #                 #  #        #  #           #     #     #  #  #  #  #     #                                                                                      !
!                                                                                                                                                                                                                                                                                                            !
!                                                                                                   #  #  #     #  #        #        #              #           #  #  #  #  #     #     #  #     #              #  #  #                                                                                      !
!                                                                                                                                                                                                                                                                                                            !
!                                                                                                   #  #                 #  #     #  #  #           #  #     #  #  #           #  #     #  #  #  #        #        #                                                                                         !
!                                                                                                                                                                                                                                                                                                            !
!                                                                                                         #  #  #        #  #  #     #           #     #     #  #  #                 #        #              #  #                                                                                            !
!                                                                                                                                                                                                                                                                                                            !
!                                                                                                               #     #           #  #  #  #  #  #  #  #  #     #  #  #  #  #           #  #  #  #                                                                                                           !
!                                                                                                                                                                                                                                                                                                            !
!                                                                                             #  #  #     #  #  #        #  #     #        #                    #           #  #        #                                                                                                                    !
!                                                                                                                                                                                                                                                                                                            !
!                                                                                                #  #  #  #  #     #  #  #  #  #        #  #           #  #  #  #  #              #     #     #                                                                                                              !
!                                                                                                                                                                                                                                                                                                            !
!                                                                                       #  #     #     #  #  #  #  #  #  #     #  #  #     #  #  #  #  #  #     #  #  #  #  #     #     #  #  #                                                                                                              !
!                                                                                                                                                                                                                                                                                                            !
!                                                                                    #              #  #        #  #           #  #  #        #        #        #        #           #     #                                                                                                                 !
!                                                                                                                                                                                                                                                                                                            !
!                                                                                 #  #  #                    #  #  #        #     #  #     #  #              #        #     #  #     #                                                                                                                       !
!                                                                                                                                                                                                                                                                                                            !
!                                                                                 #     #        #              #              #     #  #  #  #  #        #              #     #  #                                                                                                                          !
!                                                                                                                                                                                                                                                                                                            !
!                                                                                                #  #     #  #  #        #     #        #  #     #  #              #        #     #                                                                                                                          !
!                                                                                                                                                                                                                                                                                                            !
!                                                                                    #     #                 #     #                 #     #     #  #     #     #        #  #  #     #  #     #                                                                                                              !
!                                                                                                                                                                                                                                                                                                            !
!                                                                                 #           #  #  #        #        #        #     #        #  #  #  #     #  #           #  #     #  #  #  #                                                                                                              !
!                                                                                                                                                                                                                                                                                                            !
!                                                                                 #           #  #     #  #  #     #  #     #        #           #                 #  #  #  #     #     #  #                                                                                                                 !
!                                                                                                                                                                                                                                                                                                            !
!                                                                                 #        #  #           #  #  #                    #        #  #  #  #     #  #  #     #  #           #  #                                                                                                                 !
!                                                                                                                                                                                                                                                                                                            !
!                                                                                 #                    #  #  #                 #  #  #        #  #  #           #  #        #              #                                                                                                                 !
!                                                                                                                                                                                                                                                                                                            !
!                                                                                 #                 #     #     #     #  #  #  #              #  #  #  #        #  #     #  #                                                                                                                                !
!                                                                                                                                                                                                                                                                                                            !
!                                                                                 #                 #  #     #  #  #     #  #           #     #  #     #  #  #  #     #  #  #           #     #                                                                                                              !
!                                                                                                                                                                                                                                                                                                            !
!                                                                                 #                    #     #              #  #  #  #  #     #     #     #  #  #                       #  #  #                                                                                                              !
!                                                                                                                                                                                                                                                                                                            !
!                                                                                 #              #  #  #  #     #              #  #  #     #  #     #  #  #           #     #              #                                                                                                                 !
!                                                                                                                                                                                                                                                                                                            !
!                                                                                 #                 #        #  #     #  #     #  #     #  #  #  #        #  #     #  #        #  #  #  #                                                                                                                    !
!                                                                                                                                                                                                                                                                                                            !
!                                                                                 #                 #     #  #     #  #        #              #  #  #  #  #  #  #                                                                                                                                            !
!                                                                                                                                                                                                                                                                                                            !
!                                                                                 #  #  #           #  #        #  #        #     #  #  #     #     #  #     #           #                                                                                                                                   !
!                                                                                                                                                                                                                                                                                                            !
!                                                                                 #  #  #              #  #     #  #  #  #  #  #     #        #           #  #  #  #              #                                                                                                                          !
!                                                                                                                                                                                                                                                                                                            !
!                                                                                    #     #  #     #     #  #        #  #              #  #  #  #     #     #  #     #  #  #  #     #                                                                                                                       !
!                                                                                                                                                                                                                                                                                                            !
!                                                                                    #           #  #  #  #  #  #  #     #  #  #  #  #  #        #     #  #     #     #              #                                                                                                                       !
!                                                                                                                                                                                                                                                                                                            !
!                                                                                 #           #           #  #     #        #           #  #     #  #  #  #  #  #        #        #                                                                                                                          !
!                                                                                                                                                                                                                                                                                                            !
!                                                                                 #     #  #              #  #  #        #           #     #  #  #  #  #  #  #     #        #  #                                                                                                                             !
!                                                                                                                                                                                                                                                                                                            !
!                                                                                    #  #     #     #  #           #     #     #     #  #     #        #  #        #  #  #                                                                                                                                   !
!                                                                                                                                                                                                                                                                                                            !
!                                                                                       #  #  #  #  #  #     #  #                                #  #  #  #  #           #                                                                                                                                   !
!                                                                                                                                                                                                                                                                                                            !
!                                                                                                   #           #        #  #  #  #  #     #                    #     #                                                                                                                                      !
!                                                                                                                                                                                                                                                                                                            !
!                                                                                                      #  #                 #     #     #  #           #              #                                                                                                                                      !
!                                                                                                                                                                                                                                                                                                            !
!                                                                                             #  #     #        #  #     #                 #  #     #              #  #  #                                                                                                                                   !
!                                                                                                                                                                                                                                                                                                            !
!                                                                                             #     #  #     #                    #     #           #              #  #  #                                                                                                                                   !
!                                                                                                                                                                                                                                                                                                            !
!                                                                                                #           #  #  #  #     #  #           #           #              #                                                                                                                                      !
!                                                                                                                                                                                                                                                                                                            !
!                                                                                                #  #  #        #           #           #  #  #           #  #  #  #                                                                                                                                         !
!                                                                                                                                                                                                                                                                                                            !
!                                                                                                #           #  #        #  #     #  #           #                                                                                                                                                           !
!                                                                                                                                                                                                                                                                                                            !
!                                                                                                         #  #        #  #        #           #  #  #                                                                                                                                                        !
!                                                                                                                                                                                                                                                                                                            !
!                                                                                                            #  #        #     #  #     #  #           #                                                                                                                                                     !
!                                                                                                                                                                                                                                                                                                            !
!                                                                                                                           #  #        #           #  #  #                                                                                                                                                  !
!                                                                                                                                                                                                                                                                                                            !
!                                                                                                                              #     #  #     #  #           #                                                                                                                                               !
!                                                                                                                                                                                                                                                                                                            !
!                                                                                                                                 #  #        #           #  #  #                                                                                                                                            !
!                                                                                                                                                                                                                                                                                                            !
!                                                                                                                                    #     #  #     #  #           #                                                                                                                                         !
!                                                                                                                                                                                                                                                                                                            !
!                                                                                                                                       #  #        #           #  #  #                                                                                                                                      !
!                                                                                                                                                                                                                                                                                                            !
!                                                                                                                                          #     #  #     #  #           #                                                                                                                                   !
!                                                                                                                                                                                                                                                                                                            !
!                                                                                                                                             #  #        #           #  #  #                                                                                                                                !
!                                                                                                                                                                                                                                                                                                            !
!                                                                                                                                                #     #  #     #  #           #                                                                                                                             !
!                                                                                                                                                                                                                                                                                                            !
!                                                                                                                                                   #  #        #           #  #  #                                                                                                                          !
!                                                                                                                                                                                                                                                                                                            !
!                                                                                                                                                      #     #  #     #  #           #                                                                                                                       !
!                                                                                                                                                                                                                                                                                                            !
!                                                                                                                                                         #  #        #           #  #  #                                                                                                                    !
!                                                                                                                                                                                                                                                                                                            !
!                                                                                                                                                            #     #  #     #  #           #                                                                                                                 !
!                                                                                                                                                                                                                                                                                                            !
!                                                                                                                                                               #  #        #           #  #  #                                                                                                              !
!                                                                                                                                                                                                                                                                                                            !
!                                                                                                                                                                  #     #  #     #  #           #                                                                                                           !
!                                                                                                                                                                                                                                                                                                            !
!                                                                                                                                                                     #  #        #           #  #  #                                                                                                        !
!                                                                                                                                                                                                                                                                                                            !
!                                                                                                                                                                        #     #  #     #  #           #                                                                                                     !
!                                                                                                                                                                                                                                                                                                            !
!                                                                                                                                                                           #  #        #           #  #  #                                                                                                  !
!                                                                                                                                                                                                                                                                                                            !
!                                                                                                                                                                              #     #  #     #  #           #                                                                                               !
!                                                                                                                                                                                                                                                                                                            !
!                                                                                                                                                                                 #  #        #           #  #  #                                                                                            !
!                                                                                                                                                                                                                                                                                                            !
!                                                                                                                                                                                    #     #  #     #  #           #                                                                                         !
!                                                                                                                                                                                                                                                                                                            !
!                                                                                                                                                                                       #  #        #           #  #  #                                                                                      !
!                                                                                                                                                                                                                                                                                                            !
!                                                                                                                                                                                          #     #  #     #  #           #                                                                                   !
!                                                                                                                                                                                                                                                                                                            !
!                                                                                                                                                                                             #  #        #           #  #  #                                                                                !
!                                                                                                                                                                                                                                                                                                            !
!                                                                                                                                                                                                #     #  #     #  #           #                                                                             !
!                                                                                                                                                                                                                                                                                                            !
!                                                                                                                                                                                                   #  #        #           #  #  #                                                                          !
!                                                                                                                                                                                                                                                                                                            !
!                                                                                                                                                                                                      #     #  #     #  #           #                                                                       !
!                                                                                                                                                                                                                                                                                                            !
!                                                                                                                                                                                                         #  #        #     #  #  #  #  #                                                                    !
!                                                                                                                                                                                                                                                                                                            !
!                                                                                                                                                                                                            #     #  #  #     #  #  #  #                                                                    !
!                                                                                                                                                                                                                                                                                                            !
!                                                                                                                                                                                                               #  #     #  #  #     #                                                                       !
!                                                                                                                                                                                                                                                                                                            !
!                                                                                                                                                                                                                  #     #     #  #                                                                          !
!                                                                                                                                                                                                                                                                                                            !
!                                                                                                                                                                                                                     #     #                                                                                !
```



## Sidef

{{trans|Perl 6}}

```ruby
define dirs = [[1,0], [0,-1], [-1,0], [0,1]]
define size = 100

enum |White, Black|
var plane = size.of { size.of (White) }

var (x, y) = ([size >> 1] * 2)...
var dir = dirs.len.irand

var moves = 0
loop {
    (x >= 0) && (y >= 0) && (x < size) && (y < size) || break

    given (plane[x][y]) {
        when (White) { dir--; plane[x][y] = Black }
        when (Black) { dir++; plane[x][y] = White }
    }

    ++moves
    [[\x, \y], dirs[dir %= dirs.len]].zip {|a,b| *a += b }
}

say "Out of bounds after #{moves} moves at (#{x}, #{y})"
plane.map{.map {|square| square == Black ? '#' : '.' }}.each{.join.say}
```



## Swift

{{trans|C#}}

```Swift
import Foundation

let WIDTH = 100
let HEIGHT = 100

struct Point {
    var x:Int
    var y:Int
}

enum Direction: Int {
    case North = 0, East, West, South
}

class Langton {
    let leftTurn = [Direction.West, Direction.North, Direction.South, Direction.East]
    let rightTurn = [Direction.East, Direction.South, Direction.North, Direction.West]
    let xInc = [0, 1,-1, 0]
    let yInc = [-1, 0, 0, 1]
    var isBlack:[[Bool]]
    var origin:Point
    var antPosition = Point(x:0, y:0)
    var outOfBounds = false
    var antDirection = Direction.East
    
    init(width:Int, height:Int) {
        self.origin = Point(x:width / 2, y:height / 2)
        self.isBlack = Array(count: width, repeatedValue: Array(count: height, repeatedValue: false))
    }
    
    func moveAnt() {
        self.antPosition.x += xInc[self.antDirection.rawValue]
        self.antPosition.y += yInc[self.antDirection.rawValue]
    }
    
    func step() -> Point {
        if self.outOfBounds {
            println("Ant tried to move while out of bounds.")
            exit(0)
        }
        
        var ptCur = Point(x:self.antPosition.x + self.origin.x, y:self.antPosition.y + self.origin.y)
        let black = self.isBlack[ptCur.x][ptCur.y]
        let direction = self.antDirection.rawValue
        
        self.antDirection = (black ? self.leftTurn : self.rightTurn)[direction]

        self.isBlack[ptCur.x][ptCur.y] = !self.isBlack[ptCur.x][ptCur.y]
        
        self.moveAnt()
        ptCur = Point(x:self.antPosition.x + self.origin.x, y:self.antPosition.y + self.origin.y)
        self.outOfBounds =
            ptCur.x < 0 ||
            ptCur.x >= self.isBlack.count ||
            ptCur.y < 0 ||
            ptCur.y >= self.isBlack[0].count
        
        return self.antPosition
    }
}


let ant = Langton(width: WIDTH, height: HEIGHT)
while !ant.outOfBounds {
    ant.step()
}

for row in 0 ..< WIDTH {
    for col in 0 ..< HEIGHT {
        print(ant.isBlack[col][row] ? "#" : " ")
    }
    println()
}
```

{{out}}
Blank lines omitted

```txt
                          # #                                                                       
                        ## # #                                                                      
                       # ### ##                                                                     
                      #### ### #                                                                    
                      ##### #  ##                                                                   
                       #   ## ## #                                                                  
                        ###   #  ##                                                                 
                         #   ## ## #                                                                
                          ###   #  ##                                                               
                           #   ## ## #                                                              
                            ###   #  ##                                                             
                             #   ## ## #                                                            
                              ###   #  ##                                                           
                               #   ## ## #                                                          
                                ###   #  ##                                                         
                                 #   ## ## #                                                        
                                  ###   #  ##                                                       
                                   #   ## ## #                                                      
                                    ###   #  ##                                                     
                                     #   ## ## #                                                    
                                      ###   #  ##                                                   
                                       #   ## ## #                                                  
                                        ###   #  ##                                                 
                                         #   ## ## #                                                
                                          ###   #  ##                                               
                                           #   ## ## #                                              
                                            ###   #  ##                                             
                                             #   ## ## #                                            
                                              ###   #  ##                                           
                                               #   ## ## #                                          
                                                ###   #  ##                                         
                                                 #   ## ## #  ##                                    
                                                  ###   #  ##  ##                                   
                                                   #   ## ##  ##   #                                
                                             ####   ###   #   #  ###                                
                                            #    #   #   ## ####   #                                
                                           ###    #   # #      # ## #                               
                                           ###    # ##     # ##  # ##                               
                                            #    #   ## # #     ##                                  
                                            # #      # #####  #   #                                 
                                           #   #####          ## ######                             
                                           ###  ##  # ## # # #   ## # ##                            
                                         ##  # ####### #   #  ###    ## #                           
                                        #  #  ###### ##   #  # ##   #   #                           
                                       #    # # ## #  ###### #######   #                            
                                       # #### ## # ####    ##  ## # ## #                            
                                        #    ####   #  # ###### ##    ###                           
                                           #   # ## # ### #  ##  ##   ###                           
                                              #######    #  ## ## #     #                           
                                      ####  ## ##  #### ## ## ##  #     #                           
                                     #    # #   ### ## ###    # ####    #                           
                                    ###       ### # # #####    # #      #                           
                                    # #   ### #### ## #   ## ### ##     #                           
                                          ## ##  ####    #### # # #     #                           
                                     #    #  ##   ###  ###     ###      #                           
                                     ##   ## ### ####  #      ###   ##  #                           
                                     ## # ####     #   #  # ## ### ##   #                           
                                    #### ##   ## ####  # #  #  #  ###   #                           
                                    # ## ###  # # ## # #     # #     # #                            
                                        # #  #    ## ##  # #  ### ##                                
                                        ## #    #  ##### #    #    #  # #                           
                                       # ## #  #    ## ## #  ###      ###                           
                                     # #   #  #  #  #  ###   ##  ##    #                            
                                    ### # ##### ###### ### ####### # ##                             
                                    # # #    #####   ##  ##### #####                                
                                      #  ##   #      #  # ##  ### ###                               
                                   ####   ##### #########   # #                                     
                              ##    #  #     ### # #   # ###  ###                                   
                             #  #  #### ##   ### ##   ### ##     ##                                 
                            ###    # ## # #####   #    #  #  ## ###                                 
                            # ##### # #   ##  ##     #    #   #  #                                  
                                ###### ####  ## #   #  ##  # # ##                                   
                              ##      # ### ##  ####   #   ###                                      
                               #  # #####  #   # ##   #  #  #                                       
                               ## ### #######     #     # ##                                        
                              # #  ## ##      #   ##    #                                           
                             #  # ####        ###  ##  #                                            
                             # ## ###            ##  ##                                             
                              ##                                                                    
                               ##                                                                   

```



## Tcl

{{libheader|Tk}}
[[File:LangtonAnt_Tcl.gif|thumb|Output of Tcl solution of Langton's ant task]]

```tcl
package require Tk

proc step {workarea} {
    global x y dir
    if {[lindex [$workarea get $x $y] 0]} {
	$workarea put black -to $x $y
	if {[incr dir] > 3} {set dir 0}
    } else {
	$workarea put white -to $x $y
	if {[incr dir -1] < 0} {set dir 3}
    }
    switch $dir {
	0 {incr x}
	1 {incr y}
	2 {incr x -1}
	3 {incr y -1}
    }
    expr {$x < 0 || $x >= [image width $workarea] || $y < 0 || $y >= [image height $workarea]}
}

image create photo antgrid -width 100 -height 100
pack [label .l -image antgrid]
antgrid put white -to 0 0 99 99
set x [set y 50]
set dir 0

while 1 {
    update
    if {[step antgrid]} break
}

# Produce output in file
antgrid write ant.gif -format gif
```


=={{header|TI-83 BASIC}}==
The variable N counts the generation number.
<lang TI-83b>PROGRAM:LANT
:ClrDraw
:0→N
:47→A
:31→B
:90→Θ
:Repeat getKey
:If pxl-Test(B,A)
:Then
:Θ+90→Θ
:Else
:Θ-90→Θ
:End
:Pxl-Change(B,A)
:A+cos(Θ°)→A
:B+sin(Θ°)→B
:N+1→N
:End

```



## VBA



```vb

Option Explicit

Sub Ant()
Dim TablDatas(1 To 200, 1 To 256) As String, sDir As String, sFile As String, Str As String
Dim ColA As Integer, LigA As Long, ColF As Integer, LigF As Long, i As Long, j As Integer, Num As Long
Dim Top As Boolean, Left As Boolean, Bottom As Boolean, Right As Boolean
    
    'init variables
    Top = True
    LigF = 80
    ColF = 50
    For i = 1 To 200
        For j = 1 To 256
            TablDatas(i, j) = " "
        Next
    Next
    'directory
    sDir = "C:\Users\yourname\Desktop\"
    'name txt file
    sFile = "Langton_Ant.txt"
    
    'start
    For i = 1 To 15000
        LigA = LigF
        ColA = ColF
        If LigA = 1 Or ColA = 1 Or ColA = 256 Or LigA = 200 Then GoTo Fin
        If TablDatas(LigA, ColA) = " " Then
            TablDatas(LigA, ColA) = "#"
            Select Case True
                Case Top: Top = False: Left = True: LigF = LigA: ColF = ColA - 1
                Case Left: Left = False: Bottom = True: LigF = LigA + 1: ColF = ColA
                Case Bottom: Bottom = False: Right = True: LigF = LigA: ColF = ColA + 1
                Case Right: Right = False: Top = True: LigF = LigA - 1: ColF = ColA
            End Select
        Else
            TablDatas(LigA, ColA) = " "
            Select Case True
                Case Top: Top = False: Right = True: LigF = LigA: ColF = ColA + 1
                Case Left: Left = False: Top = True: LigF = LigA - 1: ColF = ColA
                Case Bottom: Bottom = False: Left = True: LigF = LigA: ColF = ColA - 1
                Case Right: Right = False: Bottom = True: LigF = LigA + 1: ColF = ColA
            End Select
        End If
    Next i
    'result in txt file
    Num = FreeFile
    Open sDir & sFile For Output As #Num
    For i = 1 To UBound(TablDatas, 1)
        Str = vbNullString
        For j = 1 To UBound(TablDatas, 2)
            Str = Str & TablDatas(i, j)
        Next j
        Print #1, Str
    Next i
    Close #Num
    Exit Sub
Fin:
MsgBox "Stop ! The ant is over limits."
End Sub

```

{{out}}

```txt
                                                                                                                                                                                                                                                                
                                      ##  ############  ##                                                                                                                                                                                                      
                                     ##  #          ####  #                                                                                                                                                                                                     
                                    # ##            ##   ###                                                                                                                                                                                                    
                                    #    #  #         #  # #                                                                                                                                                                                                    
                                  #       ###         # # ##  ##                                                                                                                                                                                                
                                  ###  ## ##     #     #   #  # ###                                                                                                                                                                                             
                              ##  ## #  # #   ## #### ##  ###  # #                                                                                                                                                                                              
                             ###   ###     # # ###  ## #  ## ### #                                                                                                                                                                                              
                            # # # ###   #  ####  # # #####   #     #                                                                                                                                                                                            
                            #   # ### # ###### ## ##  #### #   ## ###                                                                                                                                                                                           
                             ## ### #####   # ## ## ## # # ## # ### #                                                                                                                                                                                           
                           ## #    ####  # #   #   ### ## #   # #                                                                                                                                                                                               
                          ##     # ##     ##  #   ## ##         #  #                                                                                                                                                                                            
                         # ##  ## ###   #     ##  #  ### ## # #   ###                                                                                                                                                                                           
                         #   #### ##  #    #  ###   ## ##   ##  ###  #                                                                                                                                                                                          
                          #     #  ### ## #  ## #### # #  # #   #   ###                                                                                                                                                                                         
                        ##  #  # ## ###      #  ### #  #    ## #  ###  #                                                                                                                                                                                        
                       #   ## ####  #### #####  ##  ## # ## #     #   ###                                                                                                                                                                                       
                      #   #    # # #   ##      ## # # ### #  # # #  ###  #                                                                                                                                                                                      
                      #      #   #### ####       ##   # ##  ### ##  #   ###                                                                                                                                                                                     
                       #    #    #  ####  # ###########  ##   #  # #  ###  #                                                                                                                                                                                    
                       ##  #    ##  #  #########  ##  #### #      ##  #   ###                                                                                                                                                                                   
                      # ####  ## #  #   ### ### ## ##   ## #  ##   # #  ###  #                                                                                                                                                                                  
                      #   ##   ### ###    # # ## # ## ###### #  #   ##  #   ###                                                                                                                                                                                 
                       #   ##    # ##  # #     ##### # #####     #   # #  ###  #                                                                                                                                                                                
                       #  #  # ##  #  #   # #  ## ##### ## #     #    ##  #   ###                                                                                                                                                                               
                       ##   ##########   ## #####  # ####   #    #     # #  ###  #                                                                                                                                                                              
                           ## #### ##   #  ####   #  #   ##   ## #      ##  #   ###                                                                                                                                                                             
                         # #  #  #  # #    #   # ##   ##  # #####        # #  ###  #                                                                                                                                                                            
                         ##  ##  #  ## # # ## ##    # # # ##  ##          ##  #   ###                                                                                                                                                                           
                         # ####  ## # # ######## #    #  #                 # #  ###  #                                                                                                                                                                          
                         # ##  #  #   ## ##       #   #  #                  ##  #   ###                                                                                                                                                                         
                         #### ##   ##  ##  #      #  #  #                    # #  ###  #                                                                                                                                                                        
                        ### #   #    ##  ##       #   ##                      ##  #   ###                                                                                                                                                                       
                        #### ### ####    ####  ## #                            # #  ###  #                                                                                                                                                                      
                       #  # # ## #  ##    ####  ##                              ##  #   ###                                                                                                                                                                     
                      ##### ## ### ##    ##    ##                                # #  ###  #                                                                                                                                                                    
                      ####  # ## #                                                ##  #   ###                                                                                                                                                                   
                      ## ## ##                                                     # #  ###  #                                                                                                                                                                  
                            ##                                                      ##  #   ###                                                                                                                                                                 
                    # ####  ## #                                                     # #  ###  #                                                                                                                                                                
                    ###  ### #  #                                                     ##  #   ###                                                                                                                                                               
                     #  #  # ## #                                                      # #  ###  #                                                                                                                                                              
                      ##      ##                                                        ##  #   ###                                                                                                                                                             
                             ##                                                          # #  ###  #                                                                                                                                                            
                                                                                          ##  #   ###                                                                                                                                                           
                                                                                           # #  ###  #                                                                                                                                                          
                                                                                            ##  #   ###                                                                                                                                                         
                                                                                             # #  ###  #                                                                                                                                                        
                                                                                              ##  #   ###                                                                                                                                                       
```



## Vim Script


```vim
" return character under cursor
function! CurrChar() 
   return matchstr(getline('.'), '\%' . col('.') . 'c.')
endfunction

" draw all-white grid (arguments are characters to use for white and black)
function! LangtonClear(white, black)
  let l:bufname = 'langtons.ant'
  if bufexists(l:bufname)
    let l:winnum = bufwinnr(l:bufname)
    if l:winnum == -1
      execute 'sbuffer ' . bufnr(l:bufname)
    else
      execute l:winnum . 'wincmd w'
    endif
  else
    execute 'new ' . l:bufname
  end
  execute '1,$ delete _'
  call append(0, repeat(a:white,100))
  execute 'normal! 1Gyy99p'
  goto 5100
  let b:directions = [ 'k', 'l', 'j', 'h' ]
  let b:direction = 0
  let b:white = a:white
  let b:black = a:black 
endfunction

" move the ant one step
function! LangtonStep()
  let l:ch = CurrChar()
  if l:ch == b:white
    let l:ch = b:black
    let b:direction = (b:direction  + 1) % 4
  elseif l:ch == b:black
    let l:ch = b:white
    let b:direction = (b:direction  + 3) % 4
  endif
  execute 'normal! r'.l:ch.b:directions[b:direction]
endfunction

" run until we hit the edge
" optional arguments specify white and black characters;
" default . and @, respectively.
function! RunLangton(...)
  let l:white='.'
  let l:black='@'
  if a:0 > 0 
    let l:white=a:1
    if a:0 > 1 
      let l:black=a:2
    endif
  endif
  call LangtonClear(l:white, l:black)
  while 1
    let l:before = getpos('.')
    call LangtonStep()
    let l:after = getpos('.')
    if l:before == l:after
      break
    endif
  endwhile
endfunction
```



## Whitespace



```Whitespace
   	  			   	  		
 
    		  	  
		    	
	  	 
   		
		    	
	  	 
   			  	  
		    	
	  	 
    	
		 
    
   	
	  	 
     
		  
     
 
		  	
		  
   	  			 			 	 

   	
 
    		  	  
	 		
		 	 
 
    		  	  
	 	 
		 	 
   		  	  
 	  	
 	  	
	 			  	
	  	 
   		  	  
 	  	
 	  	
	 	 	  	
	  	 
 
	 	  	
			   	
	      	 
	 		 	  	 
 	  	
		    	 
	  
   	 	
	   	      	  
	 		 
	 	  	
   	  			   	    
	   				   
 
 	

   	 
 

 
 	  	
   		
 
 			   	     
	   	
     	
	    
    		  	  
	 		
	  	 	

   	  
 
    	  			   	    
	  	
		 		
 





   	 	
   	 	 
	
  
 
 	  
```


Following is the pseudo-Assembly from which the above was generated.


```asm
; For easier access, the direction vector is stored at the end of the heap.
push 10003 dup push  100 store
push 1 sub dup push   -1 store
push 1 sub dup push -100 store
push 1 sub dup push    1 store

0:  ; Initialize the grid.
    push 1 sub dup push 0 store
    dup push 0 swap sub jn 0
    push 5050 ; Start the ant at the center.

1:  ; Make sure the ant's in bounds.
    dup push 100 mod jn 2
    dup push 100 div jn 2
    push 100 copy 1 copy 1 mod sub jz 2
    push 100 copy 1 copy 1 div sub jz 2

    swap copy 1 load      ; Get current cell state.
    push 1 add push 2 mod ; Invert it.
    copy 2 copy 1  store  ; Then store it back.
    push 2 mul push 5 add add push 4 mod ; Determine new direction.
    swap copy 1 push 10000 add load add  ; Update position accordingly.
    jump 1

2:  ; Initialize a counter and flow into the printer.
    pop dup sub

3:  ; Iterate over the cells.
    dup load push 32 add ochr ; Print ' ' for off, '!' for on.
    push 1 add dup    ; Increment the counter.
    push 100 mod jz 5 ; Branch at the end of a row.
    4:
        dup push 10000 sub jn 3 ; Go again unless counter is 10000.
        pop exit ; All done, exit clean.

5:  ; Print a newline and jump back to the counter check.
    push 10 ochr jump 4
```




## XPL0

[[File:AntXPL0.gif|right]]

```XPL0
include c:\cxpl\codes;          \intrinsic 'code' declarations
int X, Y, Dir;
[SetVid($13);                   \set 320x200 graphic video mode
X:= 50;  Y:= 50;  Dir:= 0;      \start in middle facing east
repeat  if ReadPix(X,Y) then    \(black and white are reversed)
                [Dir:= Dir-1;\left\   Point(X,Y, 0\black\)]
        else    [Dir:= Dir+1;\right\  Point(X,Y,$F\white\)];
        case Dir & 3 of
          0:    X:= X+1;        \east
          1:    Y:= Y+1;        \south
          2:    X:= X-1;        \west
          3:    Y:= Y-1         \north
        other   [];
until   X<0 ! X>=100 ! Y<0 ! Y>=100;
X:= ChIn(1);                    \wait for keystroke
SetVid(3);                      \restore normal text mode
]
```



## zkl

[[File:AntXPL0.gif|right]]
{{trans|XPL0}}
Uses the PPM class from http://rosettacode.org/wiki/Bitmap/Bresenham%27s_line_algorithm#zkl

```zkl
white:=0xff|ff|ff; black:=0;
w:=h:=100; bitmap:=PPM(w,h,white);
x:=w/2; y:=h/2; dir:=0;	// start in middle facing east
do{
   if(bitmap[x,y]){ dir-=1; bitmap[x,y]=black; } // white-->black, turn left
      else        { dir+=1; bitmap[x,y]=white; } // black-->white, turn right
   switch(dir.bitAnd(3)){  // dir is always <0
      case(0){ x+=1; }	// east
      case(1){ y-=1; }	// south
      case(2){ x-=1; }	// west
      case(3){ y+=1; }  // north
   }
}while((0<=x<w) and (0<=y<h));

bitmap.write(File("foo.ppm","wb"));
```

{{out}}
Same as XPL0 (and using their image).
