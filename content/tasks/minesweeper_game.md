+++
title = "Minesweeper game"
description = ""
date = 2019-09-08T21:55:26Z
aliases = []
[extra]
id = 7718
[taxonomies]
categories = ["task", "games"]
tags = []
+++

## Task

There is an n by m grid that has a random number (between 10% to 20% of the total number of tiles, though older implementations may use 20%..60% instead) of randomly placed mines that need to be found.

Positions in the grid are modified by entering their coordinates where the first coordinate is horizontal in the grid and the second vertical. The top left of the grid is position 1,1; the bottom right is at n,m.

* The total number of mines to be found is shown at the beginning of the game.
* Each mine occupies a single grid point, and its position is initially unknown to the player
* The grid is shown as a rectangle of characters between moves.
* You are initially shown all grids as obscured, by a single dot '.'
* You may mark what you think is the position of a mine which will show as a '?'
* You can mark what you think is free space by entering its coordinates.
:*  If the point is free space then it is cleared, as are any adjacent points that are also free space- this is repeated recursively for subsequent adjacent free points unless that point is marked as a mine or is a mine.
::*   Points marked as a mine show as a '?'.
::*   Other free points show as an integer count of the number of adjacent true mines in its immediate neighborhood, or as a single space ' ' if the free point is not adjacent to any true mines.
* Of course you lose if you try to clear space that has a hidden mine.
* You win when you have correctly identified all mines.

The Task is to '''create a program that allows you to play minesweeper on a 6 by 4 grid, and that assumes all user input is formatted correctly''' and so checking inputs for correct form may be omitted.
You may also omit all GUI parts of the task and work using text input and output.

Note: Changes may be made to the method of clearing mines to more closely follow a particular implementation of the game so long as such differences and the implementation that they more accurately follow are described.


C.F: [[wp:Minesweeper (computer game)]]


## Ada



```Ada
with Ada.Numerics.Discrete_Random;
with Ada.Text_IO;

procedure Minesweeper is
   package IO renames Ada.Text_IO;
   package Nat_IO is new IO.Integer_IO (Natural);
   package Nat_RNG is new Ada.Numerics.Discrete_Random (Natural);

   type Stuff is (Empty, Mine);
   type Field is record
      Contents : Stuff   := Empty;
      Opened   : Boolean := False;
      Marked   : Boolean := False;
   end record;
   type Grid is array (Positive range <>, Positive range <>) of Field;

   -- counts how many mines are in the surrounding fields
   function Mines_Nearby (Item : Grid; X, Y : Positive) return Natural is
      Result : Natural := 0;
   begin
      -- left of X:Y
      if X > Item'First (1) then
         -- above X-1:Y
         if Y > Item'First (2) then
            if Item (X - 1, Y - 1).Contents = Mine then
               Result := Result + 1;
            end if;
         end if;
         -- X-1:Y
         if Item (X - 1, Y).Contents = Mine then
            Result := Result + 1;
         end if;
         -- below X-1:Y
         if Y < Item'Last (2) then
            if Item (X - 1, Y + 1).Contents = Mine then
               Result := Result + 1;
            end if;
         end if;
      end if;
      -- above of X:Y
      if Y > Item'First (2) then
         if Item (X, Y - 1).Contents = Mine then
            Result := Result + 1;
         end if;
      end if;
      -- below of X:Y
      if Y < Item'Last (2) then
         if Item (X, Y + 1).Contents = Mine then
            Result := Result + 1;
         end if;
      end if;
      -- right of X:Y
      if X < Item'Last (1) then
         -- above X+1:Y
         if Y > Item'First (2) then
            if Item (X + 1, Y - 1).Contents = Mine then
               Result := Result + 1;
            end if;
         end if;
         -- X+1:Y
         if Item (X + 1, Y).Contents = Mine then
            Result := Result + 1;
         end if;
         -- below X+1:Y
         if Y < Item'Last (2) then
            if Item (X + 1, Y + 1).Contents = Mine then
               Result := Result + 1;
            end if;
         end if;
      end if;
      return Result;
   end Mines_Nearby;

   -- outputs the grid
   procedure Put (Item : Grid) is
      Mines : Natural := 0;
   begin
      IO.Put ("   ");
      for X in Item'Range (1) loop
         Nat_IO.Put (Item => X, Width => 3);
      end loop;
      IO.New_Line;
      IO.Put ("   +");
      for X in Item'Range (1) loop
         IO.Put ("---");
      end loop;
      IO.Put ('+');
      IO.New_Line;
      for Y in Item'Range (2) loop
         Nat_IO.Put (Item => Y, Width => 3);
         IO.Put ('|');
         for X in Item'Range (1) loop
            if Item (X, Y).Opened then
               if Item (X, Y).Contents = Empty then
                  if Item (X, Y).Marked then
                     IO.Put (" - ");
                  else
                     Mines := Mines_Nearby (Item, X, Y);
                     if Mines > 0 then
                        Nat_IO.Put (Item => Mines, Width => 2);
                        IO.Put (' ');
                     else
                        IO.Put ("   ");
                     end if;
                  end if;
               else
                  if Item (X, Y).Marked then
                     IO.Put (" + ");
                  else
                     IO.Put (" X ");
                  end if;
               end if;
            elsif Item (X, Y).Marked then
               IO.Put (" ? ");
            else
               IO.Put (" . ");
            end if;
         end loop;
         IO.Put ('|');
         IO.New_Line;
      end loop;
      IO.Put ("   +");
      for X in Item'Range (1) loop
         IO.Put ("---");
      end loop;
      IO.Put ('+');
      IO.New_Line;
   end Put;

   -- marks a field as possible bomb
   procedure Mark (Item : in out Grid; X, Y : in Positive) is
   begin
      if Item (X, Y).Opened then
         IO.Put_Line ("Field already open!");
      else
         Item (X, Y).Marked := not Item (X, Y).Marked;
      end if;
   end Mark;

   -- clears a field and it's neighbours, if they don't have mines
   procedure Clear
     (Item   : in out Grid;
      X, Y   : in Positive;
      Killed : out Boolean)
   is
      -- clears the neighbours, if they don't have mines
      procedure Clear_Neighbours (The_X, The_Y : Positive) is
      begin
         -- mark current field opened
         Item (The_X, The_Y).Opened := True;
         -- only proceed if neighbours don't have mines
         if Mines_Nearby (Item, The_X, The_Y) = 0 then
            -- left of X:Y
            if The_X > Item'First (1) then
               -- above X-1:Y
               if The_Y > Item'First (2) then
                  if not Item (The_X - 1, The_Y - 1).Opened and
                     not Item (The_X - 1, The_Y - 1).Marked
                  then
                     Clear_Neighbours (The_X - 1, The_Y - 1);
                  end if;
               end if;
               -- X-1:Y
               if not Item (The_X - 1, The_Y).Opened and
                  not Item (The_X - 1, The_Y).Marked
               then
                  Clear_Neighbours (The_X - 1, The_Y);
               end if;
               -- below X-1:Y
               if The_Y < Item'Last (2) then
                  if not Item (The_X - 1, The_Y + 1).Opened and
                     not Item (The_X - 1, The_Y + 1).Marked
                  then
                     Clear_Neighbours (The_X - 1, The_Y + 1);
                  end if;
               end if;
            end if;
            -- above X:Y
            if The_Y > Item'First (2) then
               if not Item (The_X, The_Y - 1).Opened and
                  not Item (The_X, The_Y - 1).Marked
               then
                  Clear_Neighbours (The_X, The_Y - 1);
               end if;
            end if;
            -- below X:Y
            if The_Y < Item'Last (2) then
               if not Item (The_X, The_Y + 1).Opened and
                  not Item (The_X, The_Y + 1).Marked
               then
                  Clear_Neighbours (The_X, The_Y + 1);
               end if;
            end if;
            -- right of X:Y
            if The_X < Item'Last (1) then
               -- above X+1:Y
               if The_Y > Item'First (2) then
                  if not Item (The_X + 1, The_Y - 1).Opened and
                     not Item (The_X + 1, The_Y - 1).Marked
                  then
                     Clear_Neighbours (The_X + 1, The_Y - 1);
                  end if;
               end if;
               -- X+1:Y
               if not Item (The_X + 1, The_Y).Opened and
                  not Item (The_X + 1, The_Y).Marked
               then
                  Clear_Neighbours (The_X + 1, The_Y);
               end if;
               -- below X+1:Y
               if The_Y < Item'Last (2) then
                  if not Item (The_X + 1, The_Y + 1).Opened and
                     not Item (The_X + 1, The_Y + 1).Marked
                  then
                     Clear_Neighbours (The_X + 1, The_Y + 1);
                  end if;
               end if;
            end if;
         end if;
      end Clear_Neighbours;
   begin
      Killed := False;
      -- only clear closed and unmarked fields
      if Item (X, Y).Opened then
         IO.Put_Line ("Field already open!");
      elsif Item (X, Y).Marked then
         IO.Put_Line ("Field already marked!");
      else
         Killed := Item (X, Y).Contents = Mine;
         -- game over if killed, no need to clear
         if not Killed then
            Clear_Neighbours (X, Y);
         end if;
      end if;
   end Clear;

   -- marks all fields as open
   procedure Open_All (Item : in out Grid) is
   begin
      for X in Item'Range (1) loop
         for Y in Item'Range (2) loop
            Item (X, Y).Opened := True;
         end loop;
      end loop;
   end Open_All;

   -- counts the number of marks
   function Count_Marks (Item : Grid) return Natural is
      Result : Natural := 0;
   begin
      for X in Item'Range (1) loop
         for Y in Item'Range (2) loop
            if Item (X, Y).Marked then
               Result := Result + 1;
            end if;
         end loop;
      end loop;
      return Result;
   end Count_Marks;

   -- read and validate user input
   procedure Get_Coordinates
     (Max_X, Max_Y : Positive;
      X, Y         : out Positive;
      Valid        : out Boolean)
   is
   begin
      Valid := False;
      IO.Put ("X: ");
      Nat_IO.Get (X);
      IO.Put ("Y: ");
      Nat_IO.Get (Y);
      Valid := X > 0 and X <= Max_X and Y > 0 and Y <= Max_Y;
   exception
      when Constraint_Error =>
         Valid := False;
   end Get_Coordinates;

   -- randomly place bombs
   procedure Set_Bombs (Item : in out Grid; Max_X, Max_Y, Count : Positive) is
      Generator : Nat_RNG.Generator;
      X, Y      : Positive;
   begin
      Nat_RNG.Reset (Generator);
      for I in 1 .. Count loop
         Placement : loop
            X := Nat_RNG.Random (Generator) mod Max_X + 1;
            Y := Nat_RNG.Random (Generator) mod Max_Y + 1;
            -- redo placement if X:Y already full
            if Item (X, Y).Contents = Empty then
               Item (X, Y).Contents := Mine;
               exit Placement;
            end if;
         end loop Placement;
      end loop;
   end Set_Bombs;

   Width, Height : Positive;

begin
   -- can be dynamically set
   Width  := 6;
   Height := 4;
   declare
      The_Grid : Grid (1 .. Width, 1 .. Height);
      -- 20% bombs
      Bomb_Count         : Positive := Width * Height * 20 / 100;
      Finished           : Boolean  := False;
      Action             : Character;
      Chosen_X, Chosen_Y : Positive;
      Valid_Entry        : Boolean;
   begin
      IO.Put ("Nr. Bombs: ");
      Nat_IO.Put (Item => Bomb_Count, Width => 0);
      IO.New_Line;
      Set_Bombs
        (Item  => The_Grid,
         Max_X => Width,
         Max_Y => Height,
         Count => Bomb_Count);
      while not Finished and Count_Marks (The_Grid) /= Bomb_Count loop
         Put (The_Grid);
         IO.Put ("Input (c/m/r): ");
         IO.Get (Action);
         case Action is
            when 'c' | 'C' =>
               Get_Coordinates
                 (Max_X => Width,
                  Max_Y => Height,
                  X     => Chosen_X,
                  Y     => Chosen_Y,
                  Valid => Valid_Entry);
               if Valid_Entry then
                  Clear
                    (Item   => The_Grid,
                     X      => Chosen_X,
                     Y      => Chosen_Y,
                     Killed => Finished);
                  if Finished then
                     IO.Put_Line ("You stepped on a mine!");
                  end if;
               else
                  IO.Put_Line ("Invalid input, retry!");
               end if;
            when 'm' | 'M' =>
               Get_Coordinates
                 (Max_X => Width,
                  Max_Y => Height,
                  X     => Chosen_X,
                  Y     => Chosen_Y,
                  Valid => Valid_Entry);
               if Valid_Entry then
                  Mark (Item => The_Grid, X => Chosen_X, Y => Chosen_Y);
               else
                  IO.Put_Line ("Invalid input, retry!");
               end if;
            when 'r' | 'R' =>
               Finished := True;
            when others =>
               IO.Put_Line ("Invalid input, retry!");
         end case;
      end loop;
      Open_All (The_Grid);
      IO.Put_Line
        ("Solution: (+ = correctly marked, - = incorrectly marked)");
      Put (The_Grid);
   end;
end Minesweeper;
```



## AutoHotkey

Gui clone w/o graphic files but with cheats.

```autohotkey
; Minesweeper.ahk - v1.0.6
; (c) Dec 28, 2008 by derRaphael
; Licensed under the Terms of EUPL 1.0
; Modded by Sobriquet and re-licenced as CeCILL v2
; Modded (again) by Sobriquet and re-licenced as GPL v1.3

#NoEnv
#NoTrayIcon
SetBatchLines,-1
#SingleInstance,Off
/*
[InGameSettings]
*/
Level	=Beginner
Width	=9
Height	=9
MineMax	=10
Marks	=1
Color	=1
Sound	=0
BestTimeBeginner	=999 seconds	Anonymous
BestTimeIntermediate=999 seconds	Anonymous
BestTimeExpert		=999 seconds	Anonymous
; above settings are accessed as variables AND modified by IniWrite - be careful
BlockSize	=16
Title		=Minesweeper
MineCount	=0
mColor		=Blue,Green,Red,Purple,Navy,Olive,Maroon,Teal
GameOver	=0
TimePassed	=0

; Add mines randomly
While (MineCount<MineMax)									; loop as long as neccessary
{
	Random,x,1,%Width%										; get random horizontal position
	Random,y,1,%Height%										; get random vertical position
	If (T_x%x%y%y%!="M")									; only if not already a mine
		T_x%x%y%y%:="M"									; assign as mine
		,MineCount++										; keep count
}

Gui +OwnDialogs
;Menu,Tray,Icon,C:\WINDOWS\system32\winmine.exe,1
Menu,GameMenu,Add,&New	F2,NewGame
Menu,GameMenu,Add
Menu,GameMenu,Add,&Beginner,LevelMenu
Menu,GameMenu,Add,&Intermediate,LevelMenu
Menu,GameMenu,Add,&Expert,LevelMenu
Menu,GameMenu,Add,&Custom...,CustomMenu
Menu,GameMenu,Add
Menu,GameMenu,Add,&Marks (?),ToggleMenu
Menu,GameMenu,Add,Co&lor,ToggleMenu
Menu,GameMenu,Add,&Sound,ToggleMenu
Menu,GameMenu,Add
Menu,GameMenu,Add,Best &Times...,BestTimesMenu
Menu,GameMenu,Add
Menu,GameMenu,Add,E&xit,GuiClose
Menu,GameMenu,Check,% "&" level . (level="Custom" ? "..." : "")
If (Marks)
	Menu,GameMenu,Check,&Marks (?)
If (Color)
	Menu,GameMenu,Check,Co&lor
If (Sound)
	Menu,GameMenu,Check,&Sound

Menu,HelpMenu,Add,&Contents	F1,HelpMenu
Menu,HelpMenu,Add,&Search for Help on...,HelpMenu
Menu,HelpMenu,Add,Using &Help,HelpMenu
Menu,HelpMenu,Add
Menu,HelpMenu,Add,&About Minesweeper...,AboutMenu

Menu,MainMenu,Add,&Game,:GameMenu
Menu,MainMenu,Add,&Help,:HelpMenu
Gui,Menu,MainMenu

Gui,Font,s22,WingDings
Gui,Add,Button,% "h31 w31 y13 gNewGame vNewGame x" Width*BlockSize/2-4,K	; J=Smiley / K=Line / L=Frowny / m=Circle
Gui,Font,s16 Bold,Arial
Gui,Add,Text,% "x13 y14 w45 Border Center vMineCount c" (color ? "Red" : "White"),% SubStr("00" MineCount,-2)	; Remaining mine count
Gui,Add,Text,% "x" Width*BlockSize-34 " y14 w45 Border Center vTimer c" (color ? "Red" : "White"),000			; Timer

; Buttons
Gui,Font,s11,WingDings
Loop,% Height													; iterate vertically
{
	y:=A_Index												; remember line
	Loop,% Width												; set covers
	{
		x:=A_Index
		Gui,Add,Button,% "vB_x" x "y" y	" w" BlockSize 			; mark variable / width
						. " h" BlockSize " " (x = 1				; height / if 1st block
							? "x" 12 " y" y*BlockSize+39		; fix vertical offset
							: "yp x+0")							; otherwise stay inline /w prev. box
	}
}

; Show Gui
Gui,Show,% "w" ((Width>9 ? Width : 9))*BlockSize+24 " h" Height*BlockSize+68,%Title%
OnMessage(0x53,"WM_HELP")										; invoke Help handling for tooltips
Return ;--------------------------------------------------------; End of auto-execute section
#IfWinActive Minesweeper ahk_class AutoHotkeyGUI 				; variable %title% not supported

StartGame(x,y) {
	Global
	If (TimePassed)
		Return
	If (T_x%x%y%y%="M")											; we'll save you this time
	{
		T_x%x%y%y%:=""										; remove this mine
		Loop,% Height
		{
			y:=A_Index
			Loop,% Width
			{
				x:=A_Index
				If ( T_x%x%y%y%!="M")							; add it to first available non-mine square
					T_x%x%y%y%:="M"
					,break:=1
			IfEqual,break,1,Break
			}
		IfEqual,break,1,Break
		}
	}
	Loop,% Height												; iterate over height
	{
		y:=A_Index
		Loop,% Width											; iterate over width
		{
			x:=A_Index
			Gui,Font
			If (T_x%x%y%y%="M") {								; check for mine
				Gui,Font,s11,WingDings
				hColor:="Black"								; set color for text
			} Else {
				Loop,3 {										; loop over neighbors: three columns vertically
					ty:=y-2+A_Index							; calculate top offset
					Loop,3 {									; and three horizontally
						tx:=x-2+A_Index						; calculate left offset
						If (T_x%tx%y%ty%="M")					; no mine and inbound
							T_x%x%y%y%++						; update hint txt
					}
				}
				Gui,Font,s11 Bold,Courier New Bold
				If (Color)
					Loop,Parse,mColor,`,						; find color
						If (T_x%x%y%y% = A_Index) {				; hinttxt to color
							hColor:=A_LoopField				; set color for text
							Break
						}
				Else hColor:="Black"
			}
			Gui,Add,Text,% "c" hColor " Border +Center vT_x" x "y" y	; set color / variable
							. " w" BlockSize " h" BlockSize " "	(x = 1	; width / height / if 1st block
								? "x" 12 " y" y*BlockSize+39			; fix vertical position
								: "yp x+0")								; otherwise align to previous box
							,% T_x%x%y%y%								; M is WingDings for mine
			GuiControl,Hide,T_x%x%y%y%
		}
	}
	GuiControl,,Timer,% "00" . TimePassed:=1
	SetTimer,UpdateTimer,1000										; start timer
	If (Sound)
		SoundPlay,C:\WINDOWS\media\chord.wav
}

GuiSize:
	If (TimePassed)
		SetTimer,UpdateTimer,On 									; for cheat below
Return

UpdateTimer:														; timer
	GuiControl,,Timer,% (++TimePassed < 999) ? SubStr("00" TimePassed,-2) : 999
	If (Sound)
		SoundPlay,C:\WINDOWS\media\chord.wav
Return

Check(x,y){
	Global
	If (GameOver || B_x%x%y%y% != ""						; the game is over / prevent click on flagged squares and already-opened squares
		|| x<1 || x>Width || y<1 || y>Height)				; do not check neighbor on illegal squares
		Return
	If (T_x%x%y%y%="") {									; empty field?
		CheckNeighbor(x,y)									; uncover it and any neighbours
	} Else If (T_x%x%y%y% != "M") {							; no Mine, ok?
		GuiControl,Hide,B_x%x%y%y%							; hide covering button
		GuiControl,Show,T_x%x%y%y%
		B_x%x%y%y%:=1										; remember we been here
		UpdateChecks()
	} Else {												; ewww ... it's a mine!
		SetTimer,UpdateTimer,Off							; kill timer
		GuiControl,,T_x%x%y%y%,N							; set to Jolly Roger=N to mark death location
		GuiControl,,NewGame,L								; set Smiley Btn to Frown=L
		RevealAll()
		If (Sound)											; do we sound?
			If (FileExist("C:\Program Files\Microsoft Office\Office12\MEDIA\Explode.wav"))
				SoundPlay,C:\Program Files\Microsoft Office\Office12\MEDIA\Explode.wav
			Else
				SoundPlay,C:\WINDOWS\Media\notify.wav
	}
}

CheckNeighbor(x,y) {								; This function checks neighbours of the clicked
	Global											; field and uncovers empty fields plus adjacent hint fields
	If (GameOver || B_x%x%y%y%!="")
		Return
	GuiControl,Hide,B_x%x%y%y%						; uncover it
	GuiControl,Show,T_x%x%y%y%
	B_x%x%y%y%:=1									; remember it
	UpdateChecks()
	If (T_x%x%y%y%!="")								; is neighbour an nonchecked 0 value field?
		Return
	If (y-1>=1)										; check upper neighbour
		CheckNeighbor(x,y-1)
	If (y+1<=Height)								; check lower neighbour
		CheckNeighbor(x,y+1)
	If (x-1>=1)										; check left neighbour
		CheckNeighbor(x-1,y)
	If (x+1<=Width)									; check right neighbour
		CheckNeighbor(x+1,y)
	If (x-1>=1 && y-1>=1)							; check corner neighbour
		CheckNeighbor(x-1,y-1)
	If (x-1>=1 && y+1<=Height)						; check corner neighbour
		CheckNeighbor(x-1,y+1)
	If (x+1<=Width && y-1>=1)						; check corner neighbour
		CheckNeighbor(x+1,y-1)
	If (x+1<=Width && y+1<=Height)					; check corner neighbour
		CheckNeighbor(x+1,y+1)
}

UpdateChecks() {
	Global
	MineCount:=MineMax, Die1:=Die2:=0
	Loop,% Height
	{
		y:=A_Index
		Loop,% Width
		{
			x:=A_Index
			If (B_x%x%y%y%="O")
				MineCount--
			If (T_x%x%y%y%="M" && B_x%x%y%y%!="O") || (T_x%x%y%y%!="M" && B_x%x%y%y%="O")
				Die1++
			If (B_x%x%y%y%="" && T_x%x%y%y%!="M")
				Die2++
		}
	}
	GuiControl,,MineCount,% SubStr("00" MineCount,-2)
	If (Die1 && Die2)
		Return	; only get past here if flags+untouched squares match mines perfectly
	SetTimer,UpdateTimer,Off							; game won - kill timer
	GuiControl,,NewGame,J								; set Smiley Btn to Smile=J
	RevealAll()
	If (Sound)											; play sound
		SoundPlay,C:\WINDOWS\Media\tada.wav
	If (level!="Custom" && TimePassed < SubStr(BestTime%level%,1,InStr(BestTime%level%," ")-1) )
	{
		InputBox,name,Congratulations!,You have the fastest time`nfor level %level%.`nPlease enter your name.`n,,,,,,,,%A_UserName%
		If (name && !ErrorLevel)
		{
			BestTime%level%:=%TimePassed% seconds	%name%
			IniWrite,BestTime%level%,%A_ScriptFullPath%,InGameSettings,BestTime%level%
		}
	}
}

RevealAll(){
	Global
	Loop,% Height										; uncover all
	{
		y:=A_Index
		Loop,% Width
		{
			x:=A_Index
			If(T_x%x%y%y%="M")
			{
				GuiControl,Hide,B_x%x%y%y%
				GuiControl,Show,T_x%x%y%y%
			}
		}
	}
	GameOver:=True									; remember the game's over to block clicks
}

~F2::NewGame()
NewGame(){
NewGame:
	Reload
	ExitApp
}

LevelMenu:
	If (A_ThisMenuItem="&Beginner")
		level:="Beginner", Width:=9, Height:=9, MineMax:=10
	Else If (A_ThisMenuItem="&Intermediate")
		level:="Intermediate", Width:=16, Height:=16, MineMax:=40
	Else If (A_ThisMenuItem="&Expert")
		level:="Expert", Width:=30, Height:=16, MineMax:=99
	IniWrite,%level%,%A_ScriptFullPath%,InGameSettings,level
	IniWrite,%Width%,%A_ScriptFullPath%,InGameSettings,Width
	IniWrite,%Height%,%A_ScriptFullPath%,InGameSettings,Height
	IniWrite,%MineMax%,%A_ScriptFullPath%,InGameSettings,MineMax
	IniWrite,%Marks%,%A_ScriptFullPath%,InGameSettings,Marks
	IniWrite,%Sound%,%A_ScriptFullPath%,InGameSettings,Sound
	IniWrite,%Color%,%A_ScriptFullPath%,InGameSettings,Color
	NewGame()
Return

CustomMenu:												; label for setting window
	Gui,2:+Owner1										; make Gui#2 owned by gameWindow
	Gui,2:Default										; set to default
	Gui,1:+Disabled										; disable gameWindow
	Gui,-MinimizeBox +E0x00000400
	Gui,Add,Text,x15 y36 w38 h16,&Height:
	Gui,Add,Edit,Number x60 y33 w38 h20 vHeight HwndHwndHeight,%Height%	; use inGame settings
	Gui,Add,Text,x15 y60 w38 h16,&Width:
	Gui,Add,Edit,Number x60 y57 w38 h20 vWidth HwndHwndWidth,%Width%
	Gui,Add,Text,x15 y85 w38 h16,&Mines:
	Gui,Add,Edit,Number x60 y81 w38 h20 vMineMax HwndHwndMines,%MineMax%
	Gui,Add,Button,x120 y33 w60 h26 Default HwndHwndOK,OK
	Gui,Add,Button,x120 y75 w60 h26 g2GuiClose HwndHwndCancel,Cancel		; jump directly to 2GuiClose label
	Gui,Show,w195 h138,Custom Field
Return

2ButtonOK:													; label for OK button in settings window
	Gui,2:Submit,NoHide
	level:="Custom"
	MineMax:=MineMax<10 ? 10 : MineMax>((Width-1)*(Height-1)) ? ((Width-1)*(Height-1)) : MineMax
	Width:=Width<9 ? 9 : Width>30 ? 30 : Width
	Height:=Height<9 ? 9 : Height>24 ? 24 : Height
	GoSub,LevelMenu
Return

2GuiClose:
2GuiEscape:
	Gui,1:-Disabled								; reset GUI status and destroy setting window
	Gui,1:Default
	Gui,2:Destroy
Return

ToggleMenu:
	Toggle:=RegExReplace(RegExReplace(A_ThisMenuItem,"\s.*"),"&")
	temp:=%Toggle%
	%Toggle%:=!%Toggle%
	Menu,GameMenu,ToggleCheck,%A_ThisMenuItem%
	IniWrite,% %Toggle%,%A_ScriptFullPath%,InGameSettings,%Toggle%
Return

BestTimesMenu:
	Gui,3:+Owner1
	Gui,3:Default
	Gui,1:+Disabled
	Gui,-MinimizeBox +E0x00000400
	Gui,Add,Text,x15 y22 w250 vBestTimeBeginner HwndHwndBTB,Beginner:	%BestTimeBeginner%
	Gui,Add,Text,x15 y38 w250 vBestTimeIntermediate HwndHwndBTI,Intermediate:	%BestTimeIntermediate%
	Gui,Add,Text,x15 y54 w250 vBestTimeExpert HwndHwndBTE,Expert:		%BestTimeExpert%
	Gui,Add,Button,x38 y89 w75 h20 HwndHwndRS,&Reset Scores
	Gui,Add,Button,x173 y89 w45 h20 Default HwndHwndOK,OK
	Gui,Show,w255 h122,Fastest Mine Sweepers
Return

3ButtonResetScores:
	BestTimeBeginner	=999 seconds	Anonymous
	BestTimeIntermediate=999 seconds	Anonymous
	BestTimeExpert		=999 seconds	Anonymous
	GuiControl,,BestTimeBeginner,Beginner:	%BestTimeBeginner%`nIntermediate:	%BestTimeIntermediate%`nExpert:		%BestTimeExpert%
	GuiControl,,BestTimeIntermediate,Intermediate:	%BestTimeIntermediate%`nExpert:		%BestTimeExpert%
	GuiControl,,BestTimeExpert,Expert:		%BestTimeExpert%
	IniWrite,%BestTimeBeginner%,%A_ScriptFullPath%,InGameSettings,BestTimeBeginner
	IniWrite,%BestTimeIntermediate%,%A_ScriptFullPath%,InGameSettings,BestTimeIntermediate
	IniWrite,%BestTimeExpert%,%A_ScriptFullPath%,InGameSettings,BestTimeExpert
3GuiEscape:	; fall through:
3GuiClose:
3ButtonOK:
	Gui,1:-Disabled									; reset GUI status and destroy setting window
	Gui,1:Default
	Gui,3:Destroy
Return

^q::
GuiClose:
	ExitApp

HelpMenu:
	;Run C:\WINDOWS\Help\winmine.chm
Return

AboutMenu:
	Msgbox,64,About Minesweeper,AutoHotkey (r) Minesweeper`nVersion 1.0.6`nCopyright (c) 2014`nby derRaphael and Sobriquet
Return

~LButton::
~!LButton::
~^LButton::
~#LButton::
	Tooltip
	If (GetKeyState("RButton","P"))
		Return
	If A_OSVersion not in WIN_2003,WIN_XP,WIN_2000,WIN_NT4,WIN_95,WIN_98,WIN_ME
		If(A_PriorHotkey="~*LButton Up" && A_TimeSincePriorHotkey<100)
			GoTo,*MButton Up	; invoke MButton handling for autofill on left double-click in vista+
	If (GameOver)
		Return
	MouseGetPos,,y
	If (y>47)
		GuiControl,,NewGame,m
Return

~*LButton Up::
	If (GetKeyState("RButton","P"))
		GoTo,*MButton Up
	If (GameOver)
		Return
	GuiControl,,NewGame,K
	control:=GetControlFromClassNN(), NumX:=NumY:=0
	RegExMatch(control,"Si)T_x(?P<X>\d+)y(?P<Y>\d+)",Num)		; get Position
	StartGame(NumX,NumY)										; start game if neccessary
	Check(NumX,NumY)
Return

+LButton::
*MButton::
	Tooltip
	If (GameOver)
		Return
	MouseGetPos,,y
	If (y>47)
		GuiControl,,NewGame,m
Return

+LButton Up::
*MButton Up::
	If (GameOver)
		Return
	GuiControl,,NewGame,K
	StartGame(NumX,NumY)											; start game if neccessary
	If (GetKeyState("Esc","P"))
	{
		SetTimer,UpdateTimer,Off
		Return
	}
	control:=GetControlFromClassNN(), NumX:=NumY:=0
	RegExMatch(control,"Si)T_x(?P<X>\d+)y(?P<y>\d+)",Num)
	If ( !NumX || !NumY || B_x%NumX%y%NumY%!=1)
		Return
	temp:=0
	Loop,3 {														; loop over neighbors: three columns vertically
		ty:=NumY-2+A_Index										; calculate top offset
		Loop,3 {													; and three horizontally
			tx:=NumX-2+A_Index									; calculate left offset
			If (B_x%tx%y%ty% = "O")									; count number of marked mines around position
				temp++
		}
	}
	If (temp=T_x%NumX%y%NumY%)
		Loop,3 {													; loop over neighbors: three columns vertically
			ty:=NumY-2+A_Index									; calculate top offset
			Loop,3 {												; and three horizontally
				tx:=NumX-2+A_Index								; calculate left offset
				Check(tx,ty)										; simulate user clicking on each surrounding square
			}
		}
Return

~*RButton::
	If (GameOver || GetKeyState("LButton","P"))
		Return
	control:=GetControlFromClassNN(), NumX:=NumY:=0
	RegExMatch(control,"Si)T_x(?P<X>\d+)y(?P<y>\d+)",Num)			; grab 'em
	If ( !NumX || !NumY || B_x%NumX%y%NumY%=1)
		Return
	StartGame(NumX,NumY)											; start counter if neccessary
	B_x%NumX%y%NumY%:=(B_x%NumX%y%NumY%="" ? "O" : (B_x%NumX%y%NumY%="O" && Marks=1 ? "I" : ""))
	GuiControl,,B_x%NumX%y%NumY%,% B_x%NumX%y%NumY%
	UpdateChecks()
Return

~*RButton Up::
	If (GetKeyState("LButton","P"))
		GoTo,*MButton Up
Return

GetControlFromClassNN(){
	Global width
	MouseGetPos,,,,control
	If (SubStr(control,1,6)="Button")
		NumX:=mod(SubStr(control,7)-2,width)+1,NumY:=(SubStr(control,7)-2)//width+1
	Else If (SubStr(control,1,6)="Static")
		NumX:=mod(SubStr(control,7)-3,width)+1,NumY:=(SubStr(control,7)-3)//width+1
	Return "T_x" NumX "y" NumY
}

WM_HELP(_,_lphi)
{
	Global
	hItemHandle:=NumGet(_lphi+0,12)
	If (hItemHandle=HwndBTB || hItemHandle=HwndBTI || hItemHandle=HwndBTE)
		ToolTip Displays a player's best game time`, and the player's name`, for`neach level of play: Beginner`, Intermediate`, and Expert.
	Else If (hItemHandle=HwndRS)
		ToolTip Click to clear the current high scores.
	Else If (hItemHandle=HwndOK)
		ToolTip Closes the dialog box and saves any changes you`nhave made.
	Else If (hItemHandle=HwndCancel)
		ToolTip Closes the dialog box without saving any changes you have`nmade.
	Else If (hItemHandle=HwndHeight)
		ToolTip Specifies the number of vertical squares on the`nplaying field.
	Else If (hItemHandle=HwndWidth)
		ToolTip Specifies the number of horizontal squares on the`nplaying field.
	Else If (hItemHandle=HwndMines)
		ToolTip Specifies the number of mines to be placed on the`nplaying field.
}

:*?B0:xyzzy:: ; cheat
	KeyWait,Shift,D T1									; 1 sec grace period to press shift
	If (ErrorLevel)
		Return
	While,GetKeyState("Shift","P")
	{
		control:=GetControlFromClassNN()
		If (%control% = "M")
			SplashImage,,B X0 Y0 W1 H1 CW000000			; black pixel
		Else
			SplashImage,,B X0 Y0 W1 H1 CWFFFFFF			; white pixel
		Sleep 100
	}
	SplashImage,Off
Return

/*
 Version History

### ===========

Dec 29, 2008
1.0.0	Initial Release
1.0.1	BugFix
		- Game Restart Issues mentioned by Frankie
		- Guess count fixup I
		- Startbehaviour of game (time didnt start when 1st click was RMB Guess)
Dec 30, 2008
1.0.2	BugFix
		- Field Size Control vs Max MineCount / mentioned by °digit° / IsNull
1.0.3	BugFix
		- Guess count fixup II mentioned by °digit°
		- Corrected count when 'guessed' field uncovered
Dec 31, 2008
1.0.4	BugFix
		- Fix of Min Field & MineSettings
Mar 7, 2014
1.0.5	AddFeatures
		- Make appearance more like original
		- Make first click always safe
		- Re-license as CeCILL v2
Mar 8, 2014
1.0.6	AddFeatures
		- Add cheat code
		- Add middle-button shortcut
		- Re-license as GPL 1.3
*/
```



## BASIC256


###  Mouse version

[[File:Minesweeper game BASIC-256 won.png|thumb|right|Game won]]
[[File:Minesweeper game BASIC-256 lost.png|thumb|right|Game lost (green mines are those that were flagged before explosion)]]

```basic256
N = 6 : M = 5 : H = 25 : P = 0.2

fastgraphics
graphsize N*H,(M+1)*H
font "Arial",H/2+1,75
dim f(N,M) # 1 open, 2 mine, 4 expected mine
dim s(N,M) # count of mines in a neighborhood

trian1 = {1,1,H-1,1,H-1,H-1} : trian2 = {1,1,1,H-1,H-1,H-1}
mine = {2,2, H/2,H/2-2, H-2,2, H/2+2,H/2, H-2,H-2, H/2,H/2+2, 2,H-2, H/2-2,H/2}
flag = {H/2-1,3, H/2+1,3, H-4,H/5, H/2+1,H*2/5, H/2+1,H*0.9-2, H*0.8,H-2, H*0.2,H-2, H/2-1,H*0.9-2}

mines = int(N*M*P) : k = mines : act = 0
while k>0
	i = int(rand*N) : j = int(rand*M)
	if not f[i,j] then
		f[i,j] = 2 : k = k - 1		# set mine
		s[i,j] = s[i,j] + 1 : gosub adj	# count it

	end if
end while
togo =  M*N-mines : over = 0 : act = 1

gosub redraw
while not over
	clickclear
	while not clickb
		pause 0.01
	end while
	i = int(clickx/H) : j = int(clicky/H)
	if i<N and j<M then
		if clickb=1 then
			if not (f[i,j]&4) then ai = i : aj = j : gosub opencell
			if not s[i,j] then gosub adj
		else
			if not (f[i,j]&1) then
				if f[i,j]&4 then mines = mines+1
				if not (f[i,j]&4) then mines = mines-1
				f[i,j] = (f[i,j]&~4)|(~f[i,j]&4)
			end if
		end if
		if not (togo or mines) then over = 1
		gosub redraw
	end if
end while
imgsave "Minesweeper_game_BASIC-256.png", "PNG"
end

redraw:
	for i = 0 to N-1
		for j = 0 to M-1
			if over=-1 and f[i,j]&2 then f[i,j] = f[i,j]|1
			gosub drawcell
		next j
	next i
	# Counter
	color (32,32,32) : rect 0,M*H,N*H,H
	color white : x = 5 : y = M*H+H*0.05
	if not over then text x,y,"Mines: " + mines
	if over=1 then text x,y,"You won!"
	if over=-1 then text x,y,"You lost"
	refresh
	return

drawcell:
	color darkgrey
	rect i*H,j*H,H,H
	if f[i,j]&1=0 then	# closed
		color black : stamp i*H,j*H,trian1
		color white : stamp i*H,j*H,trian2
		color grey : rect i*H+2,j*H+2,H-4,H-4
		if f[i,j]&4 then color blue : stamp i*H,j*H,flag
	else
		color 192,192,192 : rect i*H+1,j*H+1,H-2,H-2
		# Draw
		if f[i,j]&2 then	# mine
			if not (f[i,j]&4) then color red
			if f[i,j]&4 then color darkgreen
			circle i*H+H/2,j*H+H/2,H/5 : stamp i*H,j*H,mine
		else
			if s[i,j] then color (32,32,32) : text i*H+H/3,j*H+1,s[i,j]
		end if
	end if
	return

adj:
	aj = j-1
	if j and i then ai = i-1 : gosub adjact
	if j then ai = i : gosub adjact
	if j and i<N-1 then ai = i+1 : gosub adjact
	aj = j
	if i then ai = i-1 : gosub adjact
	if i<N-1 then ai = i+1 : gosub adjact
	aj = j+1
	if j<M-1 and i then ai = i-1 : gosub adjact
	if j<M-1 then ai = i : gosub adjact
	if j<M-1 and i<N-1 then ai = i+1 : gosub adjact
	return

adjact:
	if not act then s[ai,aj] = s[ai,aj]+1 : return
	if act then gosub opencell : return

opencell:
	if not (f[ai,aj]&1) then
		f[ai,aj] = f[ai,aj]|1
		togo = togo-1
	end if
	if f[ai,aj]&2 then over = -1
	return
```



## C

dwlmines uses curses rather than battleship coordinate input, giving the game a gui feel driven with the keyboard rather than the mouse, played in a character cell terminal rather than a window filled with glorious new icons.  On ubuntu linux distribution this command might be that I used to install curses:
$ sudo apt-get install libncurses-dev

```c
#if 0

  Unix build:

  make CPPFLAGS=-DNDEBUG LDLIBS=-lcurses mines

  dwlmines, by David Lambert;  sometime in the twentieth Century.  The
  program is meant to run  in a terminal window compatible with curses
  if unix  is defined  to cpp,  or to an  ANSI terminal  when compiled
  without  unix macro  defined.   I suppose  I  have built  this on  a
  windows 98 computer using gcc running in a cmd window.  The original
  probably came from a VAX running  VMS with a vt100 sort of terminal.
  Today I  have xterm and gcc available  so I will claim  only that it
  works with this combination.

  As  this program can  automatically play  all the  trivially counted
  safe  squares.  Action  is quick  leaving the  player with  only the
  thoughtful action.  Whereas  's' steps on the spot  with the cursor,
  capital 'S' (Stomp) invokes autoplay.

  The cursor motion keys are as in the vi editor; hjkl move the cursor.

  'd' displays the number of unclaimed bombs and cells.

  'f' flags a cell.

  The  numbers  on the  field  indicate the  number  of  bombs in  the
  unclaimed neighboring  cells.  This is more useful  than showing the
  values you  expect.  You  may find unflagging  a cell adjacent  to a
  number will help you understand this.

  There  is extra  code  here.  The  multidimensional array  allocator
  allocarray is much better than  those of Numerical Recipes in C.  If
  you subtracted  the offset  1 to make  the arrays FORTRAN  like then
  allocarray could substitute for those of NR in C.
#endif

#include <stdarg.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>

#ifndef NDEBUG
# define DEBUG_CODE(A) A
#else
# define DEBUG_CODE(A)
#endif


#include <time.h>

#define DIM(A) (sizeof((A))/sizeof(*(A)))
#define MAX(A,B) ((A)<(B)?(B):(A))
#define BIND(A,L,H) ((L)<(A)?(A)<(H)?(A):(H):(L))

#define SET_BIT(A,B) ((A)|=1<<(B))
#define CLR_BIT(A,B) ((A)&=~(1<<(B)))
#define TGL_BIT(A,B) ((A)^=1<<(B))
#define INQ_BIT(A,B) ((A)&(1<<(B)))
#define FOREVER for(;;)
#define MODINC(i,mod) ((i)+1<(mod)?(i)+1:0)
#define MODDEC(i,mod) (((i)<=0?(mod):(i))-1)

void error(int status,const char *message) {
  fprintf(stderr, "%s\n", message);
  exit(status);
}

void*dwlcalloc(int n,size_t bytes) {
  void*rv = (void*)calloc(n,bytes);
  if (NULL == rv)
    error(1,"memory allocation failure");
  DEBUG_CODE(fprintf(stderr,"allocated address %p\n",rv);)
  return rv;
}

void*allocarray(int rank,size_t*shape,size_t itemSize) {
  /*
     Allocates arbitrary dimensional arrays (and inits all pointers)
     with only 1 call to malloc.  David W. Lambert, written before 1990.
     This is wonderful because one only need call free once to deallocate
     the space.  Special routines for each size array are not need for
     allocation of for deallocation.  Also calls to malloc might be expensive
     because they might have to place operating system requests.  One call
     seems optimal.
  */
  size_t size,i,j,dataSpace,pointerSpace,pointers,nextLevelIncrement;
  char*memory,*pc,*nextpc;
  if (rank < 2) {
    if (rank < 0)
      error(1,"invalid negative rank argument passed to allocarray");
    size = rank < 1 ? 1 : *shape;
    return dwlcalloc(size,itemSize);
  }
  pointerSpace = 0, dataSpace = 1;
  for (i = 0; i < rank-1; ++i)
    pointerSpace += (dataSpace *= shape[i]);
  pointerSpace *= sizeof(char*);
  dataSpace *= shape[i]*itemSize;
  memory = pc = dwlcalloc(1,pointerSpace+dataSpace);
  pointers = 1;
  for (i = 0; i < rank-2; ) {
    nextpc = pc + (pointers *= shape[i])*sizeof(char*);
    nextLevelIncrement = shape[++i]*sizeof(char*);
    for (j = 0; j < pointers; ++j)
      *((char**)pc) = nextpc, pc+=sizeof(char*), nextpc += nextLevelIncrement;
  }
  nextpc = pc + (pointers *= shape[i])*sizeof(char*);
  nextLevelIncrement = shape[++i]*itemSize;
  for (j = 0; j < pointers; ++j)
    *((char**)pc) = nextpc, pc+=sizeof(char*), nextpc += nextLevelIncrement;
  return memory;
}

#define PRINT(element) \
  if (NULL == print_elt) \
    printf("%10.3e",*(double*)(element)); \
  else \
    (*print_elt)(element)

/* matprint prints an array in APL\360 style */
/* with a NULL element printing function matprint assumes an array of double */
void matprint(void*a,int rank,size_t*shape,size_t size,void(*print_elt)()) {
  union {
    unsigned **ppu;
    unsigned *pu;
    unsigned u;
  } b;
  int i;
  if (rank <= 0 || NULL == shape)
    PRINT(a);
  else if (1 < rank) {
    for (i = 0; i < shape[0]; ++i)
      matprint(((void**)a)[i], rank-1,shape+1,size,print_elt);
    putchar('\n');
    for (i = 0, b.pu = a; i < shape[0]; ++i, b.u += size) {
      PRINT(b.pu);
      putchar(' ');
    }
  }
}

#ifdef __unix__
# include <curses.h>
# include <unistd.h>
# define SRANDOM srandom
# define RANDOM random
#else
# include <windows.h>
  void addch(int c) { putchar(c); }
  void addstr(const char*s) { fputs(s,stdout); }
# define ANSI putchar(27),putchar('[')
  void initscr(void) { printf("%d\n",AllocConsole()); }
  void cbreak(void) { ; }
  void noecho(void) { ; }
  void nonl(void) { ; }
  int move(int r,int c) { ANSI; return printf("%d;%dH",r+1,c+1); }
  int mvaddch(int r,int c,int ch) { move(r,c); addch(ch); }
  void refresh(void) { ; }
# define WA_STANDOUT 32
  int attr_on(int a,void*p) { ANSI; return printf("%dm",a); }
  int attr_off(int a,void*p) { attr_on(0,NULL); }
# include <stdarg.h>
  void printw(const char*fmt,...) {
    va_list args;
    va_start(args,fmt);
    vprintf(fmt,args);
    va_end(args);
  }
  void clrtoeol(void) {
    ANSI;addstr("0J");
  }
# define SRANDOM srand
# define RANDOM rand
#endif

#ifndef EXIT_SUCCESS
# define EXIT_SUCCESS 1		/* just a guess */
#endif


#if 0
  cell status
    UNKN --- contains virgin earth (initial state)
    MINE --- has a mine
    FLAG --- was flagged
#endif

enum {UNKN,MINE,FLAG};	/* bit numbers */

#define DETECT(CELL,PROPERTY) (!!INQ_BIT(CELL,PROPERTY))

DEBUG_CODE(                                                                \
  void pchr(void*a) {		/* odd comment removed */                  \
    putchar('A'+*(char*a));	/* should print the nth char of alphabet */\
  }                             /* where 'A' is 0 */                       \
)

char**bd;			/* the board */
size_t shape[2];
#define RWS (shape[0])
#define CLS (shape[1])

void populate(int x,int y,int pct) { /* finished size in col, row, % mines */
  int i,j,c;
  x = BIND(x,4,200), y = BIND(y,4,400); /* confine input as piecewise linear */
  shape[0] = x+2, shape[1] = y+2; bd = (char**)allocarray(2,shape,sizeof(char));
  memset(*bd,1<<UNKN,shape[0]*shape[1]*sizeof(char)); /* all unknown */
  for (i = 0; i < shape[0]; ++i)		      /* border is safe */
    bd[i][0] = bd[i][shape[1]-1] = 0;
  for (i = 0; i < shape[1]; ++i)
    bd[0][i] = bd[shape[0]-1][i] = 0;
  {
    time_t seed; /* now I would choose /dev/random */
    printf("seed is %u\n",(unsigned)seed);
    time(&seed), SRANDOM((unsigned)seed);
  }
  c = BIND(pct,1,99)*x*y/100;	/* number of mines to set */
  while(c) {
    i = RANDOM(), j = 1+i%y, i = 1+(i>>16)%x;
    if (! DETECT(bd[i][j],MINE)) /* 1 mine per site */
      --c, SET_BIT(bd[i][j],MINE);
  }
  DEBUG_CODE(matprint(bd,2,shape,sizeof(int),pchr);)
    RWS = x+1, CLS = y+1;	/* shape now stores the upper bounds */
}

struct {
  int i,j;
} neighbor[] = {
  {-1,-1}, {-1, 0}, {-1, 1},
  { 0,-1}, /*home*/ { 0, 1},
  { 1,-1}, { 1, 0}, { 1, 1}
};
/* NEIGHBOR seems to map 0..8 to local 2D positions */
#define NEIGHBOR(I,J,K) (bd[(I)+neighbor[K].i][(J)+neighbor[K].j])

int cnx(int i,int j,char w) {	/* count neighbors with property w */
  int k,c = 0;
  for (k = 0; k < DIM(neighbor); ++k)
    c += DETECT(NEIGHBOR(i,j,k),w);
  return c;
}

int row,col;
#define ME bd[row+1][col+1]

int step(void) {
  if (DETECT(ME,FLAG)) return 1; /* flags offer protection */
  if (DETECT(ME,MINE)) return 0; /* lose */
  CLR_BIT(ME,UNKN);
  return 1;
}

int autoplay(void) {
  int i,j,k,change,m;
  if (!step()) return 0;
  do				/* while changing */
    for (change = 0, i = 1; i < RWS; ++i)
      for (j = 1; j < CLS; ++j)
	if (!DETECT(bd[i][j],UNKN)) { /* consider nghbrs of safe cells */
	  m = cnx(i,j,MINE);
	  if (cnx(i,j,FLAG) == m) { /* mines appear flagged */
	    for (k = 0; k < DIM(neighbor); ++k)
	      if (DETECT(NEIGHBOR(i,j,k),UNKN)&&!DETECT(NEIGHBOR(i,j,k),FLAG)) {
		if (DETECT(NEIGHBOR(i,j,k),MINE)) { /* OOPS! */
		  row = i+neighbor[k].i-1, col = j+neighbor[k].j-1;
		  return 0;
		}
		change = 1, CLR_BIT(NEIGHBOR(i,j,k),UNKN);
	      }
	  } else if (cnx(i,j,UNKN) == m)
	    for (k = 0; k < DIM(neighbor); ++k)
	      if (DETECT(NEIGHBOR(i,j,k),UNKN))
		change = 1, SET_BIT(NEIGHBOR(i,j,k),FLAG);
	}
  while (change);
  return 1;
}

void takedisplay(void) { initscr(), cbreak(), noecho(), nonl(); }

void help(void) {
  move(RWS,1),clrtoeol(), printw("move:hjkl flag:Ff step:Ss other:qd?");
}

void draw(void) {
  int i,j,w;
  const char*s1 = " 12345678";
  move(1,1);
  for (i = 1; i < RWS; ++i, addstr("\n "))
    for (j = 1; j < CLS; ++j, addch(' ')) {
      w = bd[i][j];
      if (!DETECT(w,UNKN)) {
	w = cnx(i,j,MINE)-cnx(i,j,FLAG);
	if (w < 0) attr_on(WA_STANDOUT,NULL), w = -w;
	addch(s1[w]);
	attr_off(WA_STANDOUT,NULL);
      }
      else if (DETECT(w,FLAG)) addch('F');
      else addch('*');
    }
  move(row+1,2*col+1);
  refresh();
}

void show(int win) {
  int i,j,w;
  const char*s1 = " 12345678";
  move(1,1);
  for (i = 1; i < RWS; ++i, addstr("\n "))
    for (j = 1; j < CLS; ++j, addch(' ')) {
      w = bd[i][j];
      if (!DETECT(w,UNKN)) {
	w = cnx(i,j,MINE)-cnx(i,j,FLAG);
	if (w < 0) attr_on(WA_STANDOUT,NULL), w = -w;
	addch(s1[w]);
	attr_off(WA_STANDOUT,NULL);
      }
      else if (DETECT(w,FLAG))
	if (DETECT(w,MINE)) addch('F');
	else attr_on(WA_STANDOUT,NULL), addch('F'),attr_off(WA_STANDOUT,NULL);
      else if (DETECT(w,MINE)) addch('M');
      else addch('*');
    }
  mvaddch(row+1,2*col,'('), mvaddch(row+1,2*(col+1),')');
  move(RWS,0);
  refresh();
}

#define HINTBIT(W) s3[DETECT(bd[r][c],(W))]
#define NEIGCNT(W) s4[cnx(r,c,(W))]

const char*s3="01", *s4="012345678";

void dbg(int r, int c) {
  int i,j,unkns=0,mines=0,flags=0,pct;
  char o[6];
  static int hint;
  for (i = 1; i < RWS; ++i)
    for (j = 1; j < CLS; ++j)
      unkns += DETECT(bd[i][j],UNKN),
	mines += DETECT(bd[i][j],MINE),
	flags += DETECT(bd[i][j],FLAG);
  move(RWS,1), clrtoeol();
  pct = 0.5+100.0*(mines-flags)/MAX(1,unkns-flags);
  if (++hint<4)
    o[0] = HINTBIT(UNKN), o[1] = HINTBIT(MINE), o[2] = HINTBIT(FLAG),
      o[3] = HINTBIT(UNKN), o[4] = NEIGCNT(MINE), o[5] = NEIGCNT(FLAG);
  else
    memset(o,'?',sizeof(o));
  printw("(%c%c%c) u=%c, m=%c, f=%c,  %d/%d (%d%%) remain.",
	 o[0],o[1],o[2],o[3],o[4],o[5],mines-flags,unkns-flags,pct);
}
#undef NEIGCNT
#undef HINTBIT

void toggleflag(void) {
  if (DETECT(ME,UNKN))
    TGL_BIT(ME,FLAG);
}
int sureflag(void) {
  toggleflag();
  return autoplay();
}
int play(int*win) {
  int c = getch(), d = tolower(c);
  if ('q' == d) return 0;
  else if ('?' == c) help();
  else if ('h' == d) col = MODDEC(col,CLS-1);
  else if ('l' == d) col = MODINC(col,CLS-1);
  else if ('k' == d) row = MODDEC(row,RWS-1);
  else if ('j' == d) row = MODINC(row,RWS-1);
  else if ('f' == c) toggleflag();
  else if ('s' == c) return *win = step();
  else if ('S' == c) return *win = autoplay();
  else if ('F' == c) return *win = sureflag();
  else if ('d' == d) dbg(row+1,col+1);
  return 1;
}

int convert(const char*name,const char*s) {
  if (strlen(s) == strspn(s,"0123456789"))
    return atoi(s);
  fprintf(stderr,"    use:  %s [rows [columns [percentBombs]]]\n",name);
  fprintf(stderr,"default:  %s 20 30 25\n",name);
  exit(EXIT_SUCCESS);
}

void parse_command_line(int ac,char*av[],int*a,int*b,int*c) {
  switch (ac) {
  default:
  case 4: *c = convert(*av,av[3]);
  case 3: *b = convert(*av,av[2]);
  case 2: *a = convert(*av,av[1]);
  case 1: ;
  }
}

int main(int ac,char*av[],char*env[]) {
  int win = 1, rows = 20, cols = 30, prct = 25;
  parse_command_line(ac,av,&rows,&cols,&prct);
  populate(rows,cols,prct);
  takedisplay();
  while(draw(), play(&win));
  show(win);
  free(bd);
# ifdef __unix__
  {
    const char*s = "/bin/stty";
    execl(s,s,"sane",(const char*)NULL);
  }
# endif
  return 0;
}
```



### Mouse version

Using ncurses and mouse input.  Compiled with <code>gcc -lncurses -Wall -std=c99</code>.  Run as <code>a.out [height] [width]</code>; your terminal needs to support mouse input, and at least 2*width + 2 columns wide.  Left button clears a cell, right button toggles mine mark, middle button on a cleared cell clears all neighboring cells (or blow up if there are unmarked mines).  When mine count drops to zero, click "claim victory" to win the game or blow up.

```C>#include <ncurses.h

#include <locale.h>
#include <stdlib.h>

int width = 0, height = 0;
int mine_ratio = 10, n_mines;
int reveal = 0;

WINDOW *win, *wrap;

enum {
	M_NONE = 0,
	M_CLEARED = 1 << 0,
	M_MARKED  = 1 << 1,
	M_MINED   = 1 << 2,
	M_BOMBED  = 1 << 3,
};
typedef struct { unsigned short flag, cnt; } mine_t;

#define for_i for (int i = 0; i < height; i++)
#define for_j for (int j = 0; j < width; j++)
void init_mines(void * ptr)
{
	mine_t (*m)[width] = ptr;
	for_i for_j
		if (rand() % mine_ratio)
			m[i][j].flag = M_NONE;
		else {
			m[i][j].flag = M_MINED;
			n_mines ++;
		}

	for_i for_j {
		m[i][j].cnt = 0;
		for (int x = j - 1; x <= j + 1; x++) {
			if (x < 0 || x > width) continue;
			for (int y = i - 1; y <= i + 1; y++) {
				if (y < 0 || y >= width) continue;
				m[i][j].cnt += 1 && (m[y][x].flag & M_MINED);
			}
		}
	}
}

int mine_clear(void *ptr, int x, int y, int mass_clear)
{
	mine_t (*m)[width] = ptr;
	unsigned short flag;
	if (x < 0 || x >= width || y < 0 || y >= height)
		return 1;
	flag = m[y][x].flag;

	if (((flag & M_CLEARED) && 1) != mass_clear) return 1;

	if ((flag & M_MINED) && !(flag & M_MARKED)) {
		m[y][x].flag |= M_BOMBED;
		reveal = 1;
		return 0;
	}

	if (!(flag & M_MARKED))
		flag = (m[y][x].flag |= M_CLEARED);

	if (m[y][x].cnt && !mass_clear) return 1;
	if (flag & M_MARKED) return 1;

	for (int i = y - 1; i <= y + 1; i++)
		for (int j = x - 1; j <= x + 1; j++)
			if (!mine_clear(ptr, j, i, 0)) return 0;
	return 1;
}

void mine_mark(void *ptr, int x, int y)
{
	mine_t (*m)[width] = ptr;
	if (m[y][x].flag & M_CLEARED) return;
	if (m[y][x].flag & M_MARKED)
		n_mines ++;
	else
		n_mines --;
	m[y][x].flag ^= M_MARKED;
}

int check_wining(void *ptr)
{
	mine_t (*m)[width] = ptr;
	int good = 1;
	for_i for_j {
		int f = m[i][j].flag;
		if ((f & M_MINED) && !(f & M_MARKED)) {
			m[i][j].flag = M_BOMBED;
			good = 0;
		}
	}
	mvwprintw(wrap, height + 1, 0, good ? "All clear!    " : "BOOM!           ");
	reveal = 1;
	return good;
}

void repaint(void *ptr)
{
	mine_t (*m)[width] = ptr, *p;
	box(win, 0, 0);
	for_i for_j {
		char c;
		p = &m[i][j];
		int f = p->flag;
		if (reveal)
			c = (f & M_BOMBED) ? 'X' : (f & M_MINED) ? 'o' : ' ';
		else if (p->flag & M_BOMBED)
			c = 'X';
		else if (p->flag & M_MARKED)
			c = '?';
		else if (p->flag & M_CLEARED)
			c = p->cnt ? p->cnt + '0' : ' ';
		else
			c = '.';
		mvwprintw(win, i + 1, 2 * j + 1, " %c", c);
	}
	if (reveal);
	else if (n_mines)
		mvwprintw(wrap, height + 1, 0, "Mines:%6d   ", n_mines);
	else
		mvwprintw(wrap, height + 1, 0, "Claim victory?    ");
	wrefresh(wrap);
	wrefresh(win);
}

int main(int c, char **v)
{
	MEVENT evt;

	printf("%d\n", c);

	if (c >= 3) {
		height = atoi(v[1]);
		width = atoi(v[2]);
	}
	if (height < 3) height = 15;
	if (width < 3) width = 30;

	initscr();
	int mines[height][width];
	init_mines(mines);

	win = newwin(height + 2, 2 * width + 2, 0, 0);
	wrap = newwin(height + 3, 2 * width + 2, 1, 0);

	keypad(wrap, 1);
	mousemask(BUTTON1_CLICKED | BUTTON2_CLICKED | BUTTON3_CLICKED, 0);

	while (1) {
		int ch;
		repaint(mines);
		if ((ch = wgetch(wrap)) != KEY_MOUSE) {
			if (ch != 'r') break;
			reveal = !reveal;
			continue;
		}

		if (getmouse(&evt) != OK) continue;

		if ((evt.bstate & BUTTON1_CLICKED)) {
			if (evt.y == height + 2 && !n_mines) {
				check_wining(mines);
				break;
			}
			if (!mine_clear(mines, (evt.x - 1) / 2, evt.y - 1, 0))
				break;
		}
		else if ((evt.bstate & BUTTON2_CLICKED)) {
			if (!mine_clear(mines, (evt.x - 1) / 2, evt.y - 1, 1))
				break;
		}
		else if ((evt.bstate & BUTTON3_CLICKED))
			mine_mark(mines, (evt.x - 1)/2, evt.y - 1);
	}
	repaint(mines);

	mousemask(0, 0);
	keypad(wrap, 0);
	endwin();
	return 0;
}
```



## C++

This solution implements the required task and one more command: unknown. It is represented by a '?' that's why the flag in this solution is represented as a '!'

```cpp

#include <iostream>
#include <string>
#include <windows.h>
using namespace std;
typedef unsigned char byte;

enum fieldValues : byte { OPEN, CLOSED = 10, MINE, UNKNOWN, FLAG, ERR };

class fieldData
{
public:
    fieldData() : value( CLOSED ), open( false ) {}
    byte value;
    bool open, mine;
};

class game
{
public:
    ~game()
    { if( field ) delete [] field; }

    game( int x, int y )
    {
        go = false; wid = x; hei = y;
	field = new fieldData[x * y];
	memset( field, 0, x * y * sizeof( fieldData ) );
	oMines = ( ( 22 - rand() % 11 ) * x * y ) / 100;
	mMines = 0;
	int mx, my, m = 0;
	for( ; m < oMines; m++ )
	{
	    do
	    { mx = rand() % wid; my = rand() % hei; }
	    while( field[mx + wid * my].mine );
	    field[mx + wid * my].mine = true;
	}
	graphs[0] = ' '; graphs[1] = '.'; graphs[2] = '*';
	graphs[3] = '?'; graphs[4] = '!'; graphs[5] = 'X';
    }

    void gameLoop()
    {
	string c, r, a;
	int col, row;
	while( !go )
	{
	    drawBoard();
	    cout << "Enter column, row and an action( c r a ):\nActions: o => open, f => flag, ? => unknown\n";
	    cin >> c >> r >> a;
	    if( c[0] > 'Z' ) c[0] -= 32; if( a[0] > 'Z' ) a[0] -= 32;
	    col = c[0] - 65; row = r[0] - 49;
	    makeMove( col, row, a );
	}
    }

private:
    void makeMove( int x, int y, string a )
    {
	fieldData* fd = &field[wid * y + x];
	if( fd->open && fd->value < CLOSED )
	{
	    cout << "This cell is already open!";
	    Sleep( 3000 ); return;
	}
	if( a[0] == 'O' ) openCell( x, y );
	else if( a[0] == 'F' )
	{
	    fd->open = true;
	    fd->value = FLAG;
	    mMines++;
	    checkWin();
	}
	else
	{
	    fd->open = true;
	    fd->value = UNKNOWN;
	}
    }

    bool openCell( int x, int y )
    {
	if( !isInside( x, y ) ) return false;
	if( field[x + y * wid].mine ) boom();
	else
	{
	    if( field[x + y * wid].value == FLAG )
	    {
		field[x + y * wid].value = CLOSED;
		field[x + y * wid].open = false;
		mMines--;
	    }
	    recOpen( x, y );
	    checkWin();
	}
	return true;
    }

    void drawBoard()
    {
	system( "cls" );
	cout << "Marked mines: " << mMines << " from " << oMines << "\n\n";
	for( int x = 0; x < wid; x++ )
	    cout << "  " << ( char )( 65 + x ) << " ";
	cout << "\n"; int yy;
	for( int y = 0; y < hei; y++ )
	{
	    yy = y * wid;
	    for( int x = 0; x < wid; x++ )
		cout << "+---";

	    cout << "+\n"; fieldData* fd;
	    for( int x = 0; x < wid; x++ )
	    {
		fd = &field[x + yy]; cout<< "| ";
		if( !fd->open ) cout << ( char )graphs[1] << " ";
		else
		{
		    if( fd->value > 9 )
			cout << ( char )graphs[fd->value - 9] << " ";
		    else
		    {
			if( fd->value < 1 ) cout << "  ";
			    else cout << ( char )(fd->value + 48 ) << " ";
		    }
		}
	    }
	    cout << "| " << y + 1 << "\n";
	}
	for( int x = 0; x < wid; x++ )
	    cout << "+---";

	cout << "+\n\n";
    }

    void checkWin()
    {
	int z = wid * hei - oMines, yy;
	fieldData* fd;
	for( int y = 0; y < hei; y++ )
	{
	    yy = wid * y;
	    for( int x = 0; x < wid; x++ )
	    {
		fd = &field[x + yy];
		if( fd->open && fd->value != FLAG ) z--;
	    }
	}
	if( !z ) lastMsg( "Congratulations, you won the game!");
    }

    void boom()
    {
	int yy; fieldData* fd;
	for( int y = 0; y < hei; y++ )
	{
	    yy = wid * y;
	    for( int x = 0; x < wid; x++ )
	    {
		fd = &field[x + yy];
		if( fd->value == FLAG )
		{
		    fd->open = true;
		    fd->value = fd->mine ? MINE : ERR;
		}
		else if( fd->mine )
		{
		    fd->open = true;
		    fd->value = MINE;
		}
	    }
	}
	lastMsg( "B O O O M M M M M !" );
    }

    void lastMsg( string s )
    {
	go = true; drawBoard();
	cout << s << "\n\n";
    }

    bool isInside( int x, int y ) { return ( x > -1 && y > -1 && x < wid && y < hei ); }

    void recOpen( int x, int y )
    {
	if( !isInside( x, y ) || field[x + y * wid].open ) return;
	int bc = getMineCount( x, y );
	field[x + y * wid].open = true;
	field[x + y * wid].value = bc;
	if( bc ) return;

	for( int yy = -1; yy < 2; yy++ )
	    for( int xx = -1; xx < 2; xx++ )
	    {
		if( xx == 0 && yy == 0 ) continue;
		recOpen( x + xx, y + yy );
	    }
    }

    int getMineCount( int x, int y )
    {
	int m = 0;
	for( int yy = -1; yy < 2; yy++ )
	    for( int xx = -1; xx < 2; xx++ )
	    {
		if( xx == 0 && yy == 0 ) continue;
		if( isInside( x + xx, y + yy ) && field[x + xx + ( y + yy ) * wid].mine ) m++;
	    }

	return m;
    }

    int wid, hei, mMines, oMines;
    fieldData* field; bool go;
    int graphs[6];
};

int main( int argc, char* argv[] )
{
    srand( GetTickCount() );
    game g( 4, 6 ); g.gameLoop();
    return system( "pause" );
}

```

```txt

Marked mines: 0 from 5
  A   B   C   D
+---+---+---+---+
|   |   | 1 | . | 1
+---+---+---+---+
|   |   | 1 | * | 2
+---+---+---+---+
|   |   | 1 | . | 3
+---+---+---+---+
| 1 | 2 | 2 | . | 4
+---+---+---+---+
| . | * | * | . | 5
+---+---+---+---+
| . | . | * | * | 6
+---+---+---+---+

B O O O M M M M M !


Marked mines: 1 from 7
  A   B   C   D   E   F   G   H
+---+---+---+---+---+---+---+---+
|   |   |   |   | 1 | . | . | . | 1
+---+---+---+---+---+---+---+---+
| 1 | 1 |   | 1 | 2 | . | . | . | 2
+---+---+---+---+---+---+---+---+
| ! | 1 |   | 1 | . | . | . | . | 3
+---+---+---+---+---+---+---+---+
| 1 | 1 |   | 1 | 2 | . | 2 | 1 | 4
+---+---+---+---+---+---+---+---+
|   |   |   |   | 1 | . | 1 |   | 5
+---+---+---+---+---+---+---+---+
| 1 | 1 |   |   | 1 | 1 | 1 |   | 6
+---+---+---+---+---+---+---+---+
| . | 1 | 1 | 1 | 1 |   |   |   | 7
+---+---+---+---+---+---+---+---+
| . | . | . | . | 1 |   |   |   | 8
+---+---+---+---+---+---+---+---+

Enter column, row and an action( c r a ):
Actions: o => open, f => flag, ? => unknown

```

## C#
The following solution implements the required task providing GUI (Windows Forms).


```c#
using System;
using System.Drawing;
using System.Windows.Forms;

class MineFieldModel
{
    public int RemainingMinesCount{
        get{
            var count = 0;
            ForEachCell((i,j)=>{
                if (Mines[i,j] && !Marked[i,j])
                    count++;
            });
            return count;
        }
    }

    public bool[,] Mines{get; private set;}
    public bool[,] Opened{get;private set;}
    public bool[,] Marked{get; private set;}
    public int[,] Values{get;private set; }
    public int Width{ get{return Mines.GetLength(1);} }
    public int Height{ get{return Mines.GetLength(0);} }

    public MineFieldModel(bool[,] mines)
    {
        this.Mines = mines;
        this.Opened = new bool[Height, Width]; // filled with 'false' by default
        this.Marked = new bool[Height, Width];
        this.Values = CalculateValues();
    }

    private int[,] CalculateValues()
    {
        int[,] values = new int[Height, Width];
        ForEachCell((i,j) =>{
            var value = 0;
            ForEachNeighbor(i,j, (i1,j1)=>{
                if (Mines[i1,j1])
                    value++;
            });
            values[i,j] = value;
        });
        return values;
    }

    // Helper method for iterating over cells
    public void ForEachCell(Action<int,int> action)
    {
        for (var i = 0; i < Height; i++)
        for (var j = 0; j < Width; j++)
            action(i,j);
    }

    // Helper method for iterating over cells' neighbors
    public void ForEachNeighbor(int i, int j, Action<int,int> action)
    {
        for (var i1 = i-1; i1 <= i+1; i1++)
        for (var j1 = j-1; j1 <= j+1; j1++)
            if (InBounds(j1, i1) && !(i1==i && j1 ==j))
                action(i1, j1);
    }

    private bool InBounds(int x, int y)
    {
        return y >= 0 && y < Height && x >=0 && x < Width;
    }

    public event Action Exploded = delegate{};
    public event Action Win = delegate{};
    public event Action Updated = delegate{};

    public void OpenCell(int i, int j){
        if(!Opened[i,j]){
            if (Mines[i,j])
                Exploded();
            else{
                OpenCellsStartingFrom(i,j);
                Updated();
                CheckForVictory();
            }
        }
    }

    void OpenCellsStartingFrom(int i, int j)
    {
            Opened[i,j] = true;
            ForEachNeighbor(i,j, (i1,j1)=>{
                if (!Mines[i1,j1] && !Opened[i1,j1] && !Marked[i1,j1])
                    OpenCellsStartingFrom(i1, j1);
            });
    }

    void CheckForVictory(){
        int notMarked = 0;
        int wrongMarked = 0;
        ForEachCell((i,j)=>{
            if (Mines[i,j] && !Marked[i,j])
                notMarked++;
            if (!Mines[i,j] && Marked[i,j])
                wrongMarked++;
        });
        if (notMarked == 0 && wrongMarked == 0)
            Win();
    }

    public void Mark(int i, int j){
        if (!Opened[i,j])
            Marked[i,j] = true;
            Updated();
            CheckForVictory();
    }
}

class MineFieldView: UserControl{
    public const int CellSize = 40;

    MineFieldModel _model;
    public MineFieldModel Model{
        get{ return _model; }
        set
        {
            _model = value;
            this.Size = new Size(_model.Width * CellSize+1, _model.Height * CellSize+2);
        }
    }

    public MineFieldView(){
        //Enable double-buffering to eliminate flicker
        this.SetStyle(ControlStyles.AllPaintingInWmPaint | ControlStyles.UserPaint | ControlStyles.DoubleBuffer,true);
        this.Font = new Font(FontFamily.GenericSansSerif, 14, FontStyle.Bold);

        this.MouseUp += (o,e)=>{
            Point cellCoords = GetCell(e.Location);
            if (Model != null)
            {
                if (e.Button == MouseButtons.Left)
                    Model.OpenCell(cellCoords.Y, cellCoords.X);
                else if (e.Button == MouseButtons.Right)
                    Model.Mark(cellCoords.Y, cellCoords.X);
            }
        };
    }

    Point GetCell(Point coords)
    {
        var rgn = ClientRectangle;
        var x = (coords.X - rgn.X)/CellSize;
        var y = (coords.Y - rgn.Y)/CellSize;
        return new Point(x,y);
    }

    static readonly Brush MarkBrush = new SolidBrush(Color.Blue);
    static readonly Brush ValueBrush = new SolidBrush(Color.Black);
    static readonly Brush UnexploredBrush = new SolidBrush(SystemColors.Control);
    static readonly Brush OpenBrush = new SolidBrush(SystemColors.ControlDark);


    protected override void OnPaint(PaintEventArgs e)
    {
        base.OnPaint(e);
        var g = e.Graphics;
        if (Model != null)
        {
            Model.ForEachCell((i,j)=>
            {
                var bounds = new Rectangle(j * CellSize, i * CellSize, CellSize, CellSize);
                if (Model.Opened[i,j])
                {
                    g.FillRectangle(OpenBrush, bounds);
                    if (Model.Values[i,j] > 0)
                    {
                        DrawStringInCenter(g, Model.Values[i,j].ToString(), ValueBrush, bounds);
                    }
                }
                else
                {
                    g.FillRectangle(UnexploredBrush, bounds);
                    if (Model.Marked[i,j])
                    {
                        DrawStringInCenter(g, "?", MarkBrush, bounds);
                    }
                    var outlineOffset = 1;
                    var outline = new Rectangle(bounds.X+outlineOffset, bounds.Y+outlineOffset, bounds.Width-2*outlineOffset, bounds.Height-2*outlineOffset);
                    g.DrawRectangle(Pens.Gray, outline);
                }
                g.DrawRectangle(Pens.Black, bounds);
            });
        }

    }

    static readonly StringFormat FormatCenter = new StringFormat
                            {
                                LineAlignment = StringAlignment.Center,
                                Alignment=StringAlignment.Center
                            };

    void DrawStringInCenter(Graphics g, string s, Brush brush, Rectangle bounds)
    {
        PointF center = new PointF(bounds.X + bounds.Width/2, bounds.Y + bounds.Height/2);
        g.DrawString(s, this.Font, brush, center, FormatCenter);
    }

}

class MineSweepForm: Form
{

    MineFieldModel CreateField(int width, int height)
{
        var field = new bool[height, width];
        int mineCount = (int)(0.2 * height * width);
        var rnd = new Random();
        while(mineCount > 0)
        {
            var x = rnd.Next(width);
            var y = rnd.Next(height);
            if (!field[y,x])
            {
                field[y,x] = true;
                mineCount--;
            }
        }
        return new MineFieldModel(field);
    }

    public MineSweepForm()
    {
        var model = CreateField(6, 4);
        var counter = new Label{ };
        counter.Text = model.RemainingMinesCount.ToString();
        var view = new MineFieldView
                        {
                            Model = model, BorderStyle = BorderStyle.FixedSingle,
                        };
        var stackPanel = new FlowLayoutPanel
                        {
                            Dock = DockStyle.Fill,
                            FlowDirection = FlowDirection.TopDown,
                            Controls = {counter, view}
                        };
        this.Controls.Add(stackPanel);
        model.Updated += delegate{
            view.Invalidate();
            counter.Text = model.RemainingMinesCount.ToString();
        };
        model.Exploded += delegate {
            MessageBox.Show("FAIL!");
            Close();
        };
        model.Win += delegate {
            MessageBox.Show("WIN!");
            view.Enabled = false;
        };

    }
}

class Program
{
    static void Main()
    {
        Application.Run(new MineSweepForm());
    }
}
```



## Ceylon

Be sure to import ceylon.random in your module.ceylon file.

```ceylon
import ceylon.random {

	DefaultRandom
}

class Cell() {

	shared variable Boolean covered = true;
	shared variable Boolean flagged = false;
	shared variable Boolean mined = false;
	shared variable Integer adjacentMines = 0;

	string =>
			if (covered && !flagged)
			then "."
			else if (covered && flagged)
			then "?"
			else if (!covered && mined)
			then "X"
			else if (!covered && adjacentMines > 0)
			then adjacentMines.string
			else " ";
}

"The main function of the module. Run this one."
shared void run() {

	value random = DefaultRandom();
	value chanceOfBomb = 0.2;

	value width = 6;
	value height = 4;

	value grid = Array { for (j in 1..height) Array { for (i in 1..width) Cell() } };

	function getCell(Integer x, Integer y) => grid[y]?.get(x);

	void initializeGrid() {

		for (row in grid) {
			for (cell in row) {
				cell.covered = true;
				cell.flagged = false;
				cell.mined = random.nextFloat() < chanceOfBomb;
			}
		}

		function countAdjacentMines(Integer x, Integer y) => count {
			for (j in y - 1 .. y + 1)
			for (i in x - 1 .. x + 1)
			if (exists cell = getCell(i, j))
			cell.mined
		};

		for (j->row in grid.indexed) {
			for (i->cell in row.indexed) {
				cell.adjacentMines = countAdjacentMines(i, j);
			}
		}
	}

	void displayGrid() {
		print("  " + "".join(1..width));
		print("  " + "-".repeat(width));
		for (j->row in grid.indexed) {
			print("``j + 1``|``"".join(row)``|``j + 1``");
		}
		print("  " + "-".repeat(width));
		print("  " + "".join(1..width));
	}

	Boolean won() =>
		expand(grid).every((cell) => (cell.flagged && cell.mined) || (!cell.flagged && !cell.mined));

	void uncoverNeighbours(Integer x, Integer y) {
		for (j in y - 1 .. y + 1) {
			for (i in x - 1 .. x + 1) {
				if (exists cell = getCell(i, j), cell.covered, !cell.flagged, !cell.mined) {
					cell.covered = false;
					if (cell.adjacentMines == 0) {
						uncoverNeighbours(i, j);
					}
				}
			}
		}
	}

	while (true) {
		print("Welcome to minesweeper!
		       -----------------------");
		initializeGrid();
		while (true) {
			displayGrid();
			print("
			       The number of mines to find is ``count(expand(grid)*.mined)``.
			       What would you like to do?
			       [1] reveal a free space (or blow yourself up)
			       [2] mark (or unmark) a mine");
			assert (exists instruction = process.readLine());
			print("Please enter the coordinates. eg 2 4");
			assert (exists line2 = process.readLine());
			value coords = line2.split().map(Integer.parse).narrow<Integer>().sequence();
			if (exists x = coords[0], exists y = coords[1], exists cell = getCell(x - 1, y - 1)) {
				switch (instruction)
				case ("1") {
					if (cell.mined) {
						print("
### ===========

						       === You lose! ===

### ===========
");
						expand(grid).each((cell) => cell.covered = false);
						displayGrid();
						break;
					}
					else if (cell.covered) {
						cell.covered = false;
						uncoverNeighbours(x - 1, y - 1);
					}
				}
				case ("2") {
					if (cell.covered) {
						cell.flagged = !cell.flagged;
					}
				}
				else { print("bad choice"); }
				if (won()) {
					print("****************
					       *** You win! ***
					       ****************");
					break;
				}
			}
		}
	}
}
```



## Clojure


```Clojure
(defn take-random [n coll]
  (->> (repeatedly #(rand-nth coll))
       distinct
       (take n ,)))

(defn postwalk-fs
  "Depth first post-order traversal of form, apply successive fs at each level.
  (f1 (map f2 [..]))"
  [[f & fs] form]
  (f
   (if (and (seq fs) (coll? form))
     (into (empty form) (map (partial postwalk-fs fs) form))
     form)))

(defn neighbors [x y n m pred]
  (for [dx (range (Math/max 0 (dec x)) (Math/min n (+ 2 x)))
	dy (range (Math/max 0 (dec y)) (Math/min m (+ 2 y)))
	:when (pred dx dy)]
    [dx dy]))

(defn new-game [n m density]
  (let [mines (set (take-random (Math/floor (* n m density)) (range (* n m))))]
    (->> (for [y (range m)
	       x (range n)
	       :let [neighbor-mines (count (neighbors x y n m #(mines (+ %1 (* %2 n)))))]]
	   (#(if (mines (+ (* y n) x)) (assoc % :mine true) %) {:value neighbor-mines}))
	 (partition n ,)
	 (postwalk-fs [vec vec] ,))))

(defn display [board]
  (postwalk-fs [identity println #(condp % nil
				    :marked \?
				    :opened (:value %)
				    \.)] board))

(defn boom [{board :board}]
  (postwalk-fs [identity println #(if (:mine %) \* (:value %))] board)
  true)

(defn open* [board [[x y] & rest]]
  (if-let [value (get-in board [y x :value])] ; if nil? value -> nil? x -> nil? queue
    (recur
     (assoc-in board [y x :opened] true)
     (if (pos? value)
       rest
       (concat rest
	       (neighbors x y (count (first board)) (count board)
			  #(not (get-in board [%2 %1 :opened]))))))
    board))

(defn open [board x y]
  (let [x (dec x), y (dec y)]
    (condp (get-in board [y x]) nil
      :mine {:boom true :board board}
      :opened board
      (open* board [[x y]]))))

(defn mark [board x y]
  (let [x (dec x), y (dec y)]
    (assoc-in board [y x :marked] (not (get-in board [y x :marked])))))

(defn done? [board]
  (if (:boom board)
    (boom board)
    (do (display board)
	(->> (flatten board)
	     (remove :mine ,)
	     (every? :opened ,)))))

(defn play [n m density]
  (let [board (new-game n m density)]
    (println [:mines (count (filter :mine (flatten board)))])
    (loop [board board]
      (when-not (done? board)
	(print ">")
	(let [[cmd & xy] (.split #" " (read-line))
	      [x y] (map #(Integer. %) xy)]
	  (recur ((if (= cmd "mark") mark open) board x y)))))))
```



## Common Lisp


```lisp
(defclass minefield ()
  ((mines :initform (make-hash-table :test #'equal))
   (width :initarg :width)
   (height :initarg :height)
   (grid :initarg :grid)))

(defun make-minefield (width height num-mines)
  (let ((minefield (make-instance 'minefield
                                  :width width
                                  :height height
                                  :grid (make-array
                                          (list width height)
                                          :initial-element #\.)))
        (mine-count 0))
    (with-slots (grid mines) minefield
      (loop while (< mine-count num-mines)
            do (let ((coords (list (random width) (random height))))
                 (unless (gethash coords mines)
                   (setf (gethash coords mines) T)
                   (incf mine-count))))
      minefield)))

(defun print-field (minefield)
  (with-slots (width height grid) minefield
    (dotimes (y height)
      (dotimes (x width)
        (princ (aref grid x y)))
      (format t "~%"))))

(defun mine-list (minefield)
  (loop for key being the hash-keys of (slot-value minefield 'mines) collect key))

(defun count-nearby-mines (minefield coords)
  (length (remove-if-not
            (lambda (mine-coord)
              (and
                (> 2 (abs (- (car coords) (car mine-coord))))
                (> 2 (abs (- (cadr coords) (cadr mine-coord))))))
            (mine-list minefield))))

(defun clear (minefield coords)
  (with-slots (mines grid) minefield
    (if (gethash coords mines)
      (progn
        (format t "MINE! You lose.~%")
        (dolist (mine-coords (mine-list minefield))
          (setf (aref grid (car mine-coords) (cadr mine-coords)) #\x))
        (setf (aref grid (car coords) (cadr coords)) #\X)
        nil)
      (setf (aref grid (car coords) (cadr coords))
            (elt " 123456789"(count-nearby-mines minefield coords))))))

(defun mark (minefield coords)
  (with-slots (mines grid) minefield
    (setf (aref grid (car coords) (cadr coords)) #\?)))

(defun win-p (minefield)
  (with-slots (width height grid mines) minefield
    (let ((num-uncleared 0))
      (dotimes (y height)
        (dotimes (x width)
          (let ((square (aref grid x y)))
            (when (member square '(#\. #\?) :test #'char=)
              (incf num-uncleared)))))
      (= num-uncleared (hash-table-count mines)))))

(defun play-game ()
  (let ((minefield (make-minefield 6 4 5)))
    (format t "Greetings player, there are ~a mines.~%"
            (hash-table-count (slot-value minefield 'mines)))
    (loop
      (print-field minefield)
      (format t "Enter your command, examples: \"clear 0 1\" \"mark 1 2\" \"quit\".~%")
      (princ "> ")
      (let ((user-command (read-from-string (format nil "(~a)" (read-line)))))
        (format t "Your command: ~a~%" user-command)
        (case (car user-command)
          (quit (return-from play-game nil))
          (clear (unless (clear minefield (cdr user-command))
                   (print-field minefield)
                   (return-from play-game nil)))
          (mark (mark minefield (cdr user-command))))
        (when (win-p minefield)
          (format t "Congratulations, you've won!")
          (return-from play-game T))))))

(play-game)
```



## D

[[Minesweeper_game/D|Minesweeper in D]]


## EasyLang

[https://easylang.online/apps/minesweeper.html Run it]
<lang>len cell[] 56
len cnt[] 56
len flag[] 56
#
subr initvars
  state = 0
  ticks = 0
  indx = -1
.
func getind r c . ind .
  ind = -1
  if r >= 0 and r <= 6 and c >= 0 and c <= 7
    ind = r * 8 + c
  .
.
func draw_cell ind h . .
  r = ind / 8
  c = ind mod 8
  x# = c * 12 + 2.5
  y# = r * 12 + 14.5
  move x# y#
  rect 11 11
  if h > 0
    # count
    move x# + 3 y# + 2
    color 000
    text h
  elif h = -3
    # flag
    x# += 4
    color 000
    linewidth 0.8
    move x# y# + 3
    line x# y# + 8
    color 600
    linewidth 2
    move x# + 0.5 y# + 4
    line x# + 2 y# + 4
  elif h <> 0
    # mine
    color 333
    if h = -2
      color 800
    .
    move x# + 5 y# + 6
    circle 3
    line x# + 8 y# + 2
  .
.
func open ind . .
  if ind <> -1 and cell[ind] = 0
    cell[ind] = 2
    flag[ind] = 0
    color 686
    call draw_cell ind cnt[ind]
    if cnt[ind] = 0
      r0 = ind / 8
      c0 = ind mod 8
      for r = r0 - 1 to r0 + 1
        for c = c0 - 1 to c0 + 1
          if r <> r0 or c <> c0
            call getind r c ind
            call open ind
          .
        .
      .
    .
  .
.
func show_mines m . .
  for ind range 56
    if cell[ind] = 1
      color 686
      if m = -1
        color 353
      .
      call draw_cell ind m
    .
  .
.
func outp col s$ . .
  move 2.5 2
  color col
  rect 59 11
  color 000
  move 5 4.5
  text s$
.
func upd_info . .
  nm = 0
  nc = 0
  for i range 56
    nm += flag[i]
    if cell[i] < 2
      nc += 1
    .
  .
  if nc = 8
    call outp 484 "Well done"
    call show_mines -1
    state = 1
  else
    call outp 464 8 - nm & " mines left"
  .
.
func test ind . .
  if cell[ind] < 2 and flag[ind] = 0
    if cell[ind] = 1
      call show_mines -1
      color 686
      call draw_cell ind -2
      call outp 844 "B O O M !"
      state = 1
    else
      call open ind
      call upd_info
    .
  .
.
func start . .
  color 676
  move 0 0
  rect 100 100
  color 353
  for ind range 56
    cnt[ind] = 0
    cell[ind] = 0
    flag[ind] = 0
    call draw_cell ind 0
  .
  n = 8
  while n > 0
    c = random 8
    r = random 7
    ind = r * 8 + c
    if cell[ind] = 0
      n -= 1
      cell[ind] = 1
      for rx = r - 1 to r + 1
        for cx = c - 1 to c + 1
          call getind rx cx ind
          if ind > -1
            cnt[ind] += 1
          .
        .
      .
    .
  .
  call initvars
  call outp 464 ""
  textsize 4
  move 5 3
  text "Minesweeper - 8 mines"
  move 5 7.8
  text "Long-press for flagging"
  textsize 6
  timer 0
.
on mouse_down
  if state = 0
    call getind floor ((mouse_y - 14) / 12) floor ((mouse_x - 2) / 12) indx
    ticks0 = ticks
  elif state = 3
    call start
  .
.
on mouse_up
  if state = 0 and indx <> -1
    call test indx
  .
  indx = -1
.
on timer
  if state = 1
    state = 2
    timer 1
  elif state = 2
    state = 3
  elif ticks > 3000
    call outp 844 "B O O M !"
    call show_mines -2
    state = 2
    timer 1
  else
    if indx > -1 and ticks = ticks0 + 5
      if cell[indx] < 2
        color 353
        flag[indx] = 1 - flag[indx]
        opt = 0
        if flag[indx] = 1
          opt = -3
        .
        call draw_cell indx opt
        call upd_info
      .
      indx = -1
    .
    if ticks mod 10 = 0
      move 64.5 2
      color 464
      if ticks >= 2500
        color 844
      .
      rect 33 11
      color 000
      move 66 4.5
      text "Time:" & 300 - ticks / 10
    .
    ticks += 1
    timer 0.1
  .
.
call start
```



## Go

... though altered somewhat.

```go
package main

import (
    "bufio"
    "fmt"
    "math"
    "math/rand"
    "os"
    "strconv"
    "strings"
    "time"
)

type cell struct {
    isMine  bool
    display byte // display character for cell
}

const lMargin = 4

var (
    grid        [][]cell
    mineCount   int
    minesMarked int
    isGameOver  bool
)

var scanner = bufio.NewScanner(os.Stdin)

func makeGrid(n, m int) {
    if n <= 0 || m <= 0 {
        panic("Grid dimensions must be positive.")
    }
    grid = make([][]cell, n)
    for i := 0; i < n; i++ {
        grid[i] = make([]cell, m)
        for j := 0; j < m; j++ {
            grid[i][j].display = '.'
        }
    }
    min := int(math.Round(float64(n*m) * 0.1)) // 10% of tiles
    max := int(math.Round(float64(n*m) * 0.2)) // 20% of tiles
    mineCount = min + rand.Intn(max-min+1)
    rm := mineCount
    for rm > 0 {
        x, y := rand.Intn(n), rand.Intn(m)
        if !grid[x][y].isMine {
            rm--
            grid[x][y].isMine = true
        }
    }
    minesMarked = 0
    isGameOver = false
}

func displayGrid(isEndOfGame bool) {
    if !isEndOfGame {
        fmt.Println("Grid has", mineCount, "mine(s),", minesMarked, "mine(s) marked.")
    }
    margin := strings.Repeat(" ", lMargin)
    fmt.Print(margin, " ")
    for i := 1; i <= len(grid); i++ {
        fmt.Print(i)
    }
    fmt.Println()
    fmt.Println(margin, strings.Repeat("-", len(grid)))
    for y := 0; y < len(grid[0]); y++ {
        fmt.Printf("%*d:", lMargin, y+1)
        for x := 0; x < len(grid); x++ {
            fmt.Printf("%c", grid[x][y].display)
        }
        fmt.Println()
    }
}

func endGame(msg string) {
    isGameOver = true
    fmt.Println(msg)
    ans := ""
    for ans != "y" && ans != "n" {
        fmt.Print("Another game (y/n)? : ")
        scanner.Scan()
        ans = strings.ToLower(scanner.Text())
    }
    if scanner.Err() != nil || ans == "n" {
        return
    }
    makeGrid(6, 4)
    displayGrid(false)
}

func resign() {
    found := 0
    for y := 0; y < len(grid[0]); y++ {
        for x := 0; x < len(grid); x++ {
            if grid[x][y].isMine {
                if grid[x][y].display == '?' {
                    grid[x][y].display = 'Y'
                    found++
                } else if grid[x][y].display != 'x' {
                    grid[x][y].display = 'N'
                }
            }
        }
    }
    displayGrid(true)
    msg := fmt.Sprint("You found ", found, " out of ", mineCount, " mine(s).")
    endGame(msg)
}

func usage() {
    fmt.Println("h or ? - this help,")
    fmt.Println("c x y  - clear cell (x,y),")
    fmt.Println("m x y  - marks (toggles) cell (x,y),")
    fmt.Println("n      - start a new game,")
    fmt.Println("q      - quit/resign the game,")
    fmt.Println("where x is the (horizontal) column number and y is the (vertical) row number.\n")
}

func markCell(x, y int) {
    if grid[x][y].display == '?' {
        minesMarked--
        grid[x][y].display = '.'
    } else if grid[x][y].display == '.' {
        minesMarked++
        grid[x][y].display = '?'
    }
}

func countAdjMines(x, y int) int {
    count := 0
    for j := y - 1; j <= y+1; j++ {
        if j >= 0 && j < len(grid[0]) {
            for i := x - 1; i <= x+1; i++ {
                if i >= 0 && i < len(grid) {
                    if grid[i][j].isMine {
                        count++
                    }
                }
            }
        }
    }
    return count
}

func clearCell(x, y int) bool {
    if x >= 0 && x < len(grid) && y >= 0 && y < len(grid[0]) {
        if grid[x][y].display == '.' {
            if !grid[x][y].isMine {
                count := countAdjMines(x, y)
                if count > 0 {
                    grid[x][y].display = string(48 + count)[0]
                } else {
                    grid[x][y].display = ' '
                    clearCell(x+1, y)
                    clearCell(x+1, y+1)
                    clearCell(x, y+1)
                    clearCell(x-1, y+1)
                    clearCell(x-1, y)
                    clearCell(x-1, y-1)
                    clearCell(x, y-1)
                    clearCell(x+1, y-1)
                }
            } else {
                grid[x][y].display = 'x'
                fmt.Println("Kaboom! You lost!")
                return false
            }
        }
    }
    return true
}

func testForWin() bool {
    isCleared := false
    if minesMarked == mineCount {
        isCleared = true
        for x := 0; x < len(grid); x++ {
            for y := 0; y < len(grid[0]); y++ {
                if grid[x][y].display == '.' {
                    isCleared = false
                }
            }
        }
    }
    if isCleared {
        fmt.Println("You won!")
    }
    return isCleared
}

func splitAction(action string) (int, int, bool) {
    fields := strings.Fields(action)
    if len(fields) != 3 {
        return 0, 0, false
    }
    x, err := strconv.Atoi(fields[1])
    if err != nil || x < 1 || x > len(grid) {
        return 0, 0, false
    }
    y, err := strconv.Atoi(fields[2])
    if err != nil || y < 1 || y > len(grid[0]) {
        return 0, 0, false
    }
    return x, y, true
}

func main() {
    rand.Seed(time.Now().UnixNano())
    usage()
    makeGrid(6, 4)
    displayGrid(false)
    for !isGameOver {
        fmt.Print("\n>")
        scanner.Scan()
        action := strings.ToLower(scanner.Text())
        if scanner.Err() != nil || len(action) == 0 {
            continue
        }
        switch action[0] {
        case 'h', '?':
            usage()
        case 'n':
            makeGrid(6, 4)
            displayGrid(false)
        case 'c':
            x, y, ok := splitAction(action)
            if !ok {
                continue
            }
            if clearCell(x-1, y-1) {
                displayGrid(false)
                if testForWin() {
                    resign()
                }
            } else {
                resign()
            }
        case 'm':
            x, y, ok := splitAction(action)
            if !ok {
                continue
            }
            markCell(x-1, y-1)
            displayGrid(false)
            if testForWin() {
                resign()
            }
        case 'q':
            resign()
        }
    }
}
```


Sample session:

```txt

h or ? - this help,
c x y  - clear cell (x,y),
m x y  - marks (toggles) cell (x,y),
n      - start a new game,
q      - quit/resign the game,
where x is the (horizontal) column number and y is the (vertical) row number.

Grid has 2 mine(s), 0 mine(s) marked.
     123456
     ------
   1:......
   2:......
   3:......
   4:......

>c 1 1
Grid has 2 mine(s), 0 mine(s) marked.
     123456
     ------
   1:
   2:    11
   3: 1111.
   4: 1....

>m 6 3
Grid has 2 mine(s), 1 mine(s) marked.
     123456
     ------
   1:
   2:    11
   3: 1111?
   4: 1....

>c 6 4
Grid has 2 mine(s), 1 mine(s) marked.
     123456
     ------
   1:
   2:    11
   3: 1111?
   4: 1...1

>c 5 4
Grid has 2 mine(s), 1 mine(s) marked.
     123456
     ------
   1:
   2:    11
   3: 1111?
   4: 1..11

>c 4 4
Grid has 2 mine(s), 1 mine(s) marked.
     123456
     ------
   1:
   2:    11
   3: 1111?
   4: 1.111

>m 3 4
Grid has 2 mine(s), 2 mine(s) marked.
     123456
     ------
   1:
   2:    11
   3: 1111?
   4: 1?111
You won!
     123456
     ------
   1:
   2:    11
   3: 1111Y
   4: 1Y111
You found 2 out of 2 mine(s).
Another game (y/n)? : y
Grid has 3 mine(s), 0 mine(s) marked.
     123456
     ------
   1:......
   2:......
   3:......
   4:......

>c 1 1
Grid has 3 mine(s), 0 mine(s) marked.
     123456
     ------
   1:1.....
   2:......
   3:......
   4:......

>c 1 3
Kaboom! You lost!
     123456
     ------
   1:1.....
   2:N.....
   3:x.....
   4:.N....
You found 0 out of 3 mine(s).
Another game (y/n)? : n

```


=={{header|Icon}} and {{header|Unicon}}==
The following solution implements the required task and additionally error checking, and several additional commands.

```Icon
global DEFARGS,MF

record minefield(mask,grid,rows,cols,mines,density,marked)

$define _DEFAULTS  [6,  4, .2, .6]  # task defaults
#$define _DEFAULTS  [6, 7, .05, .1]   # defaults for debugging
$define _INDENT    6
$define _MINE      "Y"
$define _TRUEMINE  "Y"
$define _FALSEMINE "N"
$define _MASK      "."
$define _MARK      "?"
$define _TOGGLE1   ".?"
$define _TOGGLE2   "?."

procedure main(arglist)                           #: play the game
static trace
initial trace := -1

DEFARGS := _DEFAULTS
if *arglist = 0 then arglist := DEFARGS

newgame!arglist
while c := trim(read()) do {
   c ? {  tab(many(' '))
          case move(1) of {
                                                 # required commands
            "c": clear() & showgrid()            #   c  clear 1 sq and show
            "m": mark()                          #   m flag/unflag a mine
            "p": showgrid()                      #   p show the mine field
            "r": endgame("Resigning.")           #   r resign this game
                                                 # optional commands
            "n": newgame!arglist                 #   n new game grid
            "k": clearunmarked() & showgrid()    #   k clears adjecent unmarked cells if #flags = count
            "x": clearallunmarked()              #   x clears every unflagged cell at once win/loose fast
            "q": stop("Quitting")                #   q quit
            "t": &trace :=: trace                #   t toggle tracing for debugging
            default: usage()
            }}
   testforwin(g)
   }
end

procedure newgame(r,c,l,h)                       #: start a new game
local i,j,t

MF := minefield()

MF.rows := 0 < integer(\r) | DEFARGS[1]
MF.cols := 0 < integer(\c) | DEFARGS[2]

every !(MF.mask := list(MF.rows)) := list(MF.cols,_MASK)  # set mask
every !(MF.grid := list(MF.rows)) := list(MF.cols,0)      # default count

l := 1 > (0 < real(\l))  | DEFARGS[3]
h := 1 > (0 < real(\h))  | DEFARGS[4]
if l > h then l :=: h

until MF.density := l <= ( h >= ?0 )                      # random density between l:h
MF.mines := integer(MF.rows * MF.cols * MF.density)       # mines needed
MF.marked := 0

write("Creating ",r,"x",c," mine field with ",MF.mines," (",MF.density * 100,"%).")
every 1 to MF.mines do until \MF.grid[r := ?MF.rows, c := ?MF.cols] := &null                # set mines
every \MF.grid[i := 1 to MF.rows,j:= 1 to MF.cols] +:= (/MF.grid[i-1 to i+1,j-1 to j+1], 1) # set counts

showgrid()
return
end

procedure usage()                                         #: show usage
return write(
"h or ? - this help\n",
"n      - start a new game\n",
"c i j  - clears x,y and displays the grid\n",
"m i j  - marks (toggles) x,y\n",
"p      - displays the grid\n",
"k i j  - clears adjecent unmarked cells if #marks = count\n",
"x      - clears ALL unmarked flags at once\n",
"r      - resign the game\n",
"q      - quit the game\n",
"where i is the (vertical) row number and j is the (horizontal) column number." )
end

procedure getn(n)                                         #: command parsing
  tab(many(' '))
  if n := n >= ( 0 < integer(tab(many(&digits)))) then return n
  else write("Invalid or out of bounds grid square.")
end

procedure showgrid()                                      #: show grid
local r,c,x

   write(right("",_INDENT),"   ",repl("----+----|",MF.cols / 10 + 1)[1+:MF.cols])
   every r := 1 to *MF.mask do {
      writes(right(r,_INDENT)," : ")
      every c := 1 to *MF.mask[r] do
         writes( \MF.mask[r,c] | map(\MF.grid[r,c],"0"," ") | _MINE)
      write()
      }
   write(MF.marked," marked mines and ",MF.mines - MF.marked," mines left to be marked.")
end

procedure mark()                                          #: mark/toggle squares
local i,j

   if \MF.mask[i := getn(MF.rows), j :=getn(MF.cols)] := map(MF.mask[i,j],_TOGGLE1,_TOGGLE2) then {
      case MF.mask[i,j] of {
         _MASK : MF.marked -:= 1
         _MARK : MF.marked +:= 1
         }
      }
end

procedure clear()                                         #: clear a square
local i,j

   if ( i := getn(MF.rows) ) & ( j :=getn(MF.cols) ) then
      if /MF.mask[i,j] then
         write(i," ",j," was already clear")
      else if /MF.grid[i,j] then endgame("KABOOM! You lost.")
         else return revealclearing(i,j)
end

procedure revealclearing(i,j)                             #: reaveal any clearing

   if \MF.mask[i,j] := &null then {
      if MF.grid[i,j] = 0 then
         every revealclearing(i-1 to i+1,j-1 to j+1)
      return
      }
end

procedure clearunmarked()                                 #: clears adjecent unmarked cells if #flags = count
local i,j,k,m,n

   if ( i := getn(MF.rows) ) & ( j :=getn(MF.cols) ) then
      if /MF.mask[i,j] & ( k := 0 < MF.grid[i,j] ) then {
         every (\MF.mask[i-1 to i+1,j-1 to j+1] == _MARK) & ( k -:= 1)
         if k = 0 then {
            every (m := i-1 to i+1) & ( n := j-1 to j+1) do
               if \MF.mask[m,n] == _MASK then MF.mask[m,n] := &null
            revealclearing(i,j)
            return
            }
         else
            write("Marked squares must match adjacent mine count.")
            }
      else write("Must be adjecent to one or more marks to clear surrounding squares.")
end

procedure clearallunmarked()                             #: fast win or loose
local i,j,k

   every (i := 1 to MF.rows) & (j := 1 to MF.cols) do {
      if \MF.mask[i,j] == _MASK then {
         MF.mask[i,j] := &null
         if /MF.grid[i,j] then k := 1
         }
      }
   if \k then endgame("Kaboom - you loose.")
end

procedure testforwin()                                  #: win when rows*cols-#_MARK-#_MASK are clear and no Kaboom
local t,x

   t :=  MF.rows * MF.cols - MF.mines
   every x := !!MF.mask do if /x then t -:= 1
   if t = 0 then endgame("You won!")
end

procedure endgame(tag)                                  #: end the game
local i,j,m

   every !(m := list(MF.rows)) := list(MF.cols)         # new mask
   every (i := 1 to MF.rows) & (j := 1 to MF.cols) do
      if \MF.mask[i,j] == _MARK then
         m[i,j] := if /MF.grid[i,j] then _TRUEMINE else _FALSEMINE
   MF.mask := m
   write(tag) & showgrid()
end
```


Sample output:

```txt
Creating 6x4 mine field with 9 (40.9903518170081%).
         ----
     1 : ....
     2 : ....
     3 : ....
     4 : ....
     5 : ....
     6 : ....
0 marked mines and 9 mines left to be marked.
h
h or ? - this help
n      - start a new game
c i j  - clears x,y and displays the grid
m i j  - marks (toggles) x,y
p      - displays the grid
k i j  - clears adjecent unmarked cells if #marks = count
x      - clears ALL unmarked flags at once
r      - resign the game
q      - quit the game
where i is the (vertical) row number and j is the (horizontal) column number.
c 1 1
         ----
     1 :  1..
     2 : 12..
     3 : ....
     4 : ....
     5 : ....
     6 : ....
0 marked mines and 9 mines left to be marked.
```



## J

'''Solution'''

```j
NB. minefield.ijs script
NB.
### ===================================================

NB. Game engine

NB.require 'guid'
NB.([ 9!:1) _2 (3!:4) , guids 1            NB. randomly set initial random seed

coclass 'mineswpeng'

newMinefield=: 3 : 0
  if. 0=#y do. y=. 9 9 end.
  Marked=: Cleared=: y$0
  NMines=: <. */(0.01*10+?20),y            NB. 10..20% of tiles are mines
  mines=. (i. e. NMines ? */) y            NB. place mines
  Map=: (9*mines) >. y{. (1,:3 3) +/@,;.3 (-1+y){.mines
)

markTiles=: 3 : 0
  Marked=: (<"1 <:y) (-.@{)`[`]} Marked    NB. toggle marked state of cell(s)
)

clearTiles=: clearcell@:<:                 NB. decrement coords - J arrays are 0-based

clearcell=: verb define
  if. #y do.
    free=. (#~ (Cleared < 0 = Map) {~ <"1) y
    Cleared=: 1 (<"1 y)} Cleared           NB. set cell(s) as cleared
    if. #free do.
      clearcell (#~ Cleared -.@{~ <"1) ~. (<:$Map) (<."1) 0 >. getNbrs free
    end.
  end.
)

getNbrs=: [: ,/^:(3=#@$) +"1/&(<: 3 3#: i.9)

eval=: verb define
  if. 9 e. Cleared #&, Map do.            NB. cleared mine(s)?
    1; 'KABOOM!!'
  elseif. *./ 9 = (-.Cleared) #&, Map do. NB. all cleared except mines?
    1; 'Minefield cleared.'
  elseif. do.                             NB. else...
    0; (": +/, Marked>Cleared),' of ',(":NMines),' mines marked.'
  end.                                    NB. result: isEnd; message
)

showField=: 4 : 0
  idx=. y{ (2 <. Marked + +:Cleared) ,: 2
  |: idx} (11&{ , 12&{ ,: Map&{) x  NB. transpose result - J arrays are row,column
)

NB.
### ===================================================

NB. User interface

Minesweeper_z_=: conew&'mineswp'

coclass 'mineswp'
coinsert 'mineswpeng'        NB. insert game engine locale in copath

Tiles=: ' 12345678**.?'

create=: verb define
  smoutput Instructions
  startgame y
)
destroy=: codestroy
quit=: destroy

startgame=: update@newMinefield
clear=: update@clearTiles
mark=: update@markTiles

update=: 3 : 0
  'isend msg'=. eval ''
  smoutput msg
  smoutput < Tiles showField isend
  if. isend do.
    msg=. ('K'={.msg) {:: 'won';'lost'
    smoutput 'You ',msg,'! Try again?'
    destroy ''
  end.
  empty''
)

Instructions=: 0 : 0

###  MineSweeper

Object:
   Uncover (clear) all the tiles that are not mines.

How to play:
 - the left, top tile is: 1 1
 - clear an uncleared tile (.) using the command:
      clear__fld <column index> <row index>
 - mark and uncleared tile (?) as a suspected mine using the command:
      mark__fld <column index> <row index>
 - if you uncover a number, that is the number of mines adjacent
   to the tile
 - if you uncover a mine (*) the game ends (you lose)
 - if you uncover all tiles that are not mines the game ends (you win).
 - quit a game before winning or losing using the command:
      quit__fld ''
 - start a new game using the command:
      fld=: MineSweeper <num columns> <num rows>
)
```


'''Example Usage'''

```j
   load 'minefield.ijs'
   fld=: Minesweeper 6 4

###  MineSweeper

Object:
   Uncover (clear) all the tiles that are not mines.

How to play:
 - the left, top tile is: 1 1
 - clear an uncleared tile (.) using the command:
      clear__fld <column index> <row index>
 - mark and uncleared tile (?) as a suspected mine using the command:
      mark__fld <column index> <row index>
 - if you uncover a number, that is the number of mines adjacent
   to the tile
 - if you uncover a mine (*) the game ends (you lose)
 - if you uncover all tiles that are not mines the game ends (you win).
 - quit a game before winning or losing using the command:
      quit__fld ''
 - start a new game using the command:
      fld=: MineSweeper <num columns> <num rows>

0 of 5 mines marked.
┌──────┐
│......│
│......│
│......│
│......│
└──────┘
   clear__fld 1 1
0 of 5 mines marked.
┌──────┐
│2.....│
│......│
│......│
│......│
└──────┘
   clear__fld 6 4
0 of 5 mines marked.
┌──────┐
│2..2  │
│...2  │
│..31  │
│..1   │
└──────┘
   mark__fld 3 1 ,: 3 2               NB. mark and clear both accept lists of coordinates
2 of 5 mines marked.
┌──────┐
│2.?2  │
│..?2  │
│..31  │
│..1   │
└──────┘
   clear__fld 1 2 , 1 3 ,: 1 4
2 of 5 mines marked.
┌──────┐
│2.?2  │
│3.?2  │
│2.31  │
│1.1   │
└──────┘
   clear__fld 2 1
KABOOM!!
┌──────┐
│2**2  │
│3**2  │
│2*31  │
│111   │
└──────┘
You lost! Try again?
   fld=: Minesweeper 20 10            NB. create bigger minefield
...                                   NB. instructions elided
0 of 50 mines marked.
┌────────────────────┐
│....................│
│....................│
│....................│
│....................│
│....................│
│....................│
│....................│
│....................│
│....................│
│....................│
└────────────────────┘
   clear__fld >: 4$.$. 9 >  Map__fld  NB. Autosolve ;-)
Minefield cleared.
┌────────────────────┐
│1***2**2**2*2*2*1111│
│123222222221212222*1│
│11          1223*421│
│*3321   11223**5**1 │
│3***211 1*2**6**432 │
│2*433*222223**423*2 │
│1111*4*4*2 13*2 3*41│
│11112*3**321211 2**1│
│3*3112334*3*211 1332│
│***1 1*12*312*1  1*1│
└────────────────────┘
You won! Try again?
```



## Java



```java


This is a version of minesweeper with a gui. The code might not be optimal, but at least its not hard to understand.

//--------------------------------- START of Main.java ---------------------------------

/*
 * Main.java
 *
 * Created by Potrik
 * Last modified: 07.22.13
 */

public class Main
{
	public static void main (String[] args)
	{
		int x = 10;		//Width of the board
		int y = 10;		//Height of the board
		int d = 25;		//The difficulty of the game, the percentage of mines in the board. The number of mines per board is random, but this number is the probability that a cell will become
					//a mine.

		new Minesweeper(x, y, d);
	}
}

//--------------------------------- END of Main.java ---------------------------------

//--------------------------------- START of Cell.java ---------------------------------

/*
 * Cell.java
 *
 * Created by Potrik
 * Last modified: 07.22.13
 */

public class Cell
{
	private boolean isMine, isFlagged, isObscured;
	private int number;

	public Cell()
	{
		isMine = false;
		isFlagged = false;
		isObscured = true;
		number = 0;
	}

	public void setMine()
	{
		isMine = true;
	}

	public void flag()
	{
		isFlagged = true;
	}

	public void unflag()
	{
		isFlagged = false;
	}

	public void reveal()
	{
		isObscured = false;
	}

	public void setNumber(int i)
	{
		number = i;
	}

	public boolean isMine()
	{
		return isMine;
	}

	public boolean isFlagged()
	{
		return isFlagged;
	}

	public boolean isObscured()
	{
		return isObscured;
	}

	public int getNumber()
	{
		return number;
	}
}

//--------------------------------- END of Cell.java ---------------------------------

//--------------------------------- START of Board.java ---------------------------------

/*
 * Board.java
 *
 * Created by Potrik
 * Last modified: 07.22.13
 */

import java.awt.Color;
import java.awt.Dimension;
import java.awt.Graphics;
import javax.swing.JPanel;

public class Board extends JPanel
{
	private static final long serialVersionUID = 1L;
	private Minesweeper mine;
	private Cell[][] cells;

	public Board(Minesweeper m)
	{
		mine = m;
		cells = mine.getCells();

		addMouseListener(new Actions(mine));

		setPreferredSize(new Dimension(mine.getx() * 20, mine.gety() * 20));
	}

	public void paintComponent(Graphics g)
	{
		cells = mine.getCells();

		for (int i = 0; i < mine.getx(); i++)
		{
			for (int j = 0; j < mine.gety(); j++)
			{
				Cell current = cells[i][j];

				if (current.isFlagged())
				{
					if (current.isMine() && mine.isFinished())
					{
						g.setColor(Color.ORANGE);
						g.fillRect(i * 20, j * 20, i * 20 + 20, j * 20 + 20);
						g.setColor(Color.BLACK);

						g.drawLine(i * 20, j * 20, i * 20 + 20, j * 20 + 20);
						g.drawLine(i * 20, j * 20 + 20, i * 20 + 20, j * 20);
					}
					else if (mine.isFinished())
					{
						g.setColor(Color.GREEN);
						g.fillRect(i * 20, j * 20, i * 20 + 20, j * 20 + 20);
						g.setColor(Color.BLACK);
					}
					else
					{
						g.setColor(Color.YELLOW);
						g.fillRect(i * 20, j * 20, i * 20 + 20, j * 20 + 20);
						g.setColor(Color.BLACK);
					}
				}
				else if (current.isObscured())
				{
					g.setColor(Color.GRAY);
					g.fillRect(i * 20, j * 20, i * 20 + 20, j * 20 + 20);
					g.setColor(Color.BLACK);
				}
				else if (current.isMine())
				{
					g.setColor(Color.RED);
					g.fillRect(i * 20, j * 20, i * 20 + 20, j * 20 + 20);
					g.setColor(Color.BLACK);
					g.drawLine(i * 20, j * 20, i * 20 + 20, j * 20 + 20);
					g.drawLine(i * 20, j * 20 + 20, i * 20 + 20, j * 20);
				}
				else
				{
					g.setColor(Color.LIGHT_GRAY);
					g.fillRect(i * 20, j * 20, i * 20 + 20, j * 20 + 20);
					g.setColor(Color.BLACK);
				}
				if (!current.isObscured())
				{
					if (current.getNumber() == 1)
					{
						g.drawLine(i * 20 + 13, j * 20 + 5, i * 20 + 13, j * 20 + 9);	//3
						g.drawLine(i * 20 + 13, j * 20 + 11, i * 20 + 13, j * 20 + 15);	//6
					}
					else if (current.getNumber() == 2)
					{
						g.drawLine(i * 20 + 8, j * 20 + 4, i * 20 + 12, j * 20 + 4);	//2
						g.drawLine(i * 20 + 13, j * 20 + 5, i * 20 + 13, j * 20 + 9);	//3
						g.drawLine(i * 20 + 8, j * 20 + 10, i * 20 + 12, j * 20 + 10);	//4
						g.drawLine(i * 20 + 7, j * 20 + 11, i * 20 + 7, j * 20 + 15);	//5
						g.drawLine(i * 20 + 8, j * 20 + 16, i * 20 + 12, j * 20 + 16);	//7
					}
					else if (current.getNumber() == 3)
					{
						g.drawLine(i * 20 + 8, j * 20 + 4, i * 20 + 12, j * 20 + 4);	//2
						g.drawLine(i * 20 + 13, j * 20 + 5, i * 20 + 13, j * 20 + 9);	//3
						g.drawLine(i * 20 + 8, j * 20 + 10, i * 20 + 12, j * 20 + 10);	//4
						g.drawLine(i * 20 + 13, j * 20 + 11, i * 20 + 13, j * 20 + 15);	//6
						g.drawLine(i * 20 + 8, j * 20 + 16, i * 20 + 12, j * 20 + 16);	//7
					}
					else if (current.getNumber() == 4)
					{
						g.drawLine(i * 20 + 7, j * 20 + 5, i * 20 + 7, j * 20 + 9);		//1
						g.drawLine(i * 20 + 13, j * 20 + 5, i * 20 + 13, j * 20 + 9);	//3
						g.drawLine(i * 20 + 8, j * 20 + 10, i * 20 + 12, j * 20 + 10);	//4
						g.drawLine(i * 20 + 13, j * 20 + 11, i * 20 + 13, j * 20 + 15);	//6
					}
					else if (current.getNumber() == 5)
					{
						g.drawLine(i * 20 + 7, j * 20 + 5, i * 20 + 7, j * 20 + 9);		//1
						g.drawLine(i * 20 + 8, j * 20 + 4, i * 20 + 12, j * 20 + 4);	//2
						g.drawLine(i * 20 + 8, j * 20 + 10, i * 20 + 12, j * 20 + 10);	//4
						g.drawLine(i * 20 + 13, j * 20 + 11, i * 20 + 13, j * 20 + 15);	//6
						g.drawLine(i * 20 + 8, j * 20 + 16, i * 20 + 12, j * 20 + 16);	//7
					}
					else if (current.getNumber() == 6)
					{
						g.drawLine(i * 20 + 7, j * 20 + 5, i * 20 + 7, j * 20 + 9);		//1
						g.drawLine(i * 20 + 8, j * 20 + 4, i * 20 + 12, j * 20 + 4);	//2
						g.drawLine(i * 20 + 8, j * 20 + 10, i * 20 + 12, j * 20 + 10);	//4
						g.drawLine(i * 20 + 7, j * 20 + 11, i * 20 + 7, j * 20 + 15);	//5
						g.drawLine(i * 20 + 13, j * 20 + 11, i * 20 + 13, j * 20 + 15);	//6
						g.drawLine(i * 20 + 8, j * 20 + 16, i * 20 + 12, j * 20 + 16);	//7
					}
					else if (current.getNumber() == 7)
					{
						g.drawLine(i * 20 + 8, j * 20 + 4, i * 20 + 12, j * 20 + 4);	//2
						g.drawLine(i * 20 + 13, j * 20 + 5, i * 20 + 13, j * 20 + 9);	//3
						g.drawLine(i * 20 + 13, j * 20 + 11, i * 20 + 13, j * 20 + 15);	//6
					}
					else if (current.getNumber() == 8)
					{
						g.drawLine(i * 20 + 7, j * 20 + 5, i * 20 + 7, j * 20 + 9);		//1
						g.drawLine(i * 20 + 8, j * 20 + 4, i * 20 + 12, j * 20 + 4);	//2
						g.drawLine(i * 20 + 13, j * 20 + 5, i * 20 + 13, j * 20 + 9);	//3
						g.drawLine(i * 20 + 8, j * 20 + 10, i * 20 + 12, j * 20 + 10);	//4
						g.drawLine(i * 20 + 7, j * 20 + 11, i * 20 + 7, j * 20 + 15);	//5
						g.drawLine(i * 20 + 13, j * 20 + 11, i * 20 + 13, j * 20 + 15);	//6
						g.drawLine(i * 20 + 8, j * 20 + 16, i * 20 + 12, j * 20 + 16);	//7
					}
				}
				g.setColor(Color.BLACK);
				g.drawRect(i * 20, j * 20, i * 20 + 20, j * 20 + 20);
			}
		}
	}
}

//--------------------------------- END of Board.java ---------------------------------

//--------------------------------- START of Actions.java ---------------------------------

/*
 * Board.java
 *
 * Created by Potrik
 * Last modified: 07.22.13
 */

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;

public class Actions implements ActionListener, MouseListener
{
	private Minesweeper mine;

	public Actions(Minesweeper m)
	{
		mine = m;
	}

	public void actionPerformed(ActionEvent e)
	{
		mine.reset();

		mine.refresh();
	}

	public void mouseClicked(MouseEvent e)
	{
		if (e.getButton() == 1)
		{
			int x = e.getX() / 20;
			int y = e.getY() / 20;

			mine.select(x, y);
		}

		if (e.getButton() == 3)
		{
			int x = e.getX() / 20;
			int y = e.getY() / 20;

			mine.mark(x, y);
		}

		mine.refresh();
	}

	public void mouseEntered(MouseEvent e)
	{

	}

	public void mouseExited(MouseEvent e)
	{

	}

	public void mousePressed(MouseEvent e)
	{

	}

	public void mouseReleased(MouseEvent e)
	{

	}

}

//--------------------------------- END of Actions.java ---------------------------------

//--------------------------------- START of Minesweeper.java ---------------------------------

/*
 * Minesweeper.java
 *
 * Created by Potrik
 * Last modified: 07.22.13
 */

import java.awt.BorderLayout;
import java.util.Random;
import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JOptionPane;

public class Minesweeper extends JFrame
{
	private static final long serialVersionUID = 1L;
	private int width, height;
	private Cell[][] cells;
	private int difficulty;
	private Board board;
	private JButton reset;
	private boolean finished;

	public Minesweeper(int x, int y, int d)
	{
		width = x;
		height = y;
		difficulty = d;
		cells = new Cell[width][height];

		reset();

		board = new Board(this);
		reset = new JButton("Reset");

		add(board, BorderLayout.CENTER);
		add(reset, BorderLayout.SOUTH);

		reset.addActionListener(new Actions(this));

		setTitle("Minesweeper");
		setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		setResizable(false);
		pack();
		setVisible(true);
	}

	public int getx()
	{
		return width;
	}

	public int gety()
	{
		return height;
	}

	public Cell[][] getCells()
	{
		return cells;
	}

	public void reset()
	{
		Random random = new Random();
		finished = false;

		for (int i = 0; i < width; i++)
		{
			for (int j = 0; j < height; j++)
			{
				Cell c = new Cell();
				cells[i][j] = c;
				int r = random.nextInt(100);

				if (r < difficulty)
				{
					cells[i][j].setMine();
				}
			}
		}
		setNumbers();
	}

	private void setNumbers()
	{
		for (int i = 0; i < width; i++)
		{
			for (int j = 0; j < height; j++)
			{
				int count = 0;

				if (i > 0 &&	j > 0 && cells[i - 1]	[j - 1]	.isMine()) count++;
				if (j > 0 && cells[i][j - 1].isMine()) count++;
				if (i < width - 1 && j > 0 && cells[i + 1][j - 1].isMine()) count++;

				if (i > 0 && cells[i - 1][j].isMine()) count++;
				if (i < width - 1 && cells[i + 1][j].isMine()) count++;

				if (i > 0 && j < height - 1 && cells[i - 1][j + 1].isMine()) count++;
				if (j < height - 1	&& cells[i] [j + 1].isMine()) count++;
				if (i < width - 1 && j < height - 1 && cells[i + 1][j + 1].isMine()) count++;

				cells[i][j].setNumber(count);

				if (cells[i][j].isMine())
				{
					cells[i][j].setNumber(-1);
				}

				if (cells[i][j].getNumber() == 0)
				{
					cells[i][j].reveal();
				}
			}
		}

		for (int i = 0; i < width; i++)
		{
			for (int j = 0; j < height; j++)
			{
				if (i > 0 &&	j > 0 && cells[i - 1][j - 1].getNumber() == 0) cells[i][j].reveal();
				if (j > 0 && cells[i][j - 1].getNumber() == 0) cells[i][j].reveal();
				if (i < width - 1 && j > 0 && cells[i + 1][j - 1].getNumber() == 0) cells[i][j].reveal();

				if (i > 0 && cells[i - 1][j].getNumber() == 0) cells[i][j].reveal();
				if (i < width - 1 && cells[i + 1]	[j]		.getNumber() == 0) cells[i][j].reveal();

				if (i > 0 && j < height - 1 && cells[i - 1][j + 1].getNumber() == 0) cells[i][j].reveal();
				if (j < height - 1 && cells[i][j + 1].getNumber() == 0) cells[i][j].reveal();
				if (i < width - 1 && j < height - 1 && cells[i + 1][j + 1]	.getNumber() == 0) cells[i][j].reveal();
			}
		}
	}

	public void refresh()
	{
		board.repaint();
	}

	public void select(int x, int y)
	{
		if (cells[x][y].isFlagged()) return;
		cells[x][y].reveal();
		resetMarks();
		refresh();

		if (cells[x][y].isMine())
		{
			loose();
		}
		else if (won())
		{
			win();
		}
	}

	private void loose()
	{
		finished = true;
		for (int i = 0; i < width; i++)
		{
			for (int j = 0; j < height; j++)
			{
				if (!cells[i][j].isObscured()) cells[i][j].unflag();
				cells[i][j].reveal();
			}
		}
		refresh();
		JOptionPane.showMessageDialog(null, "BOOOOM!");
		reset();
	}

	private void win()
	{
		finished = true;
		for (int i = 0; i < width; i++)
		{
			for (int j = 0; j < height; j++)
			{
				cells[i][j].reveal();
				if (!cells[i][j].isMine()) cells[i][j].unflag();
			}
		}

		refresh();
		JOptionPane.showMessageDialog(null, "Congratulations! You won!");
		reset();
	}

	private boolean won()
	{
		for (int i = 0; i < width; i++)
		{
			for (int j = 0; j < height; j++)
			{
				if (cells[i][j].isObscured() && !cells[i][j].isMine())
				{
					return false;
				}
			}
		}

		return true;
	}

	public void mark(int x, int y)
	{
		if (cells[x][y].isFlagged()) cells[x][y].unflag();
		else if (cells[x][y].isObscured()) cells[x][y].flag();

		resetMarks();
	}

	private void resetMarks()
	{
		for (int i = 0; i < width; i++)
		{
			for (int j = 0; j < height; j++)
			{
				if (!cells[i][j].isObscured()) cells[i][j].unflag();
			}
		}
	}

	public boolean isFinished()
	{
		return finished;
	}
}

//--------------------------------- END of Minesweeper.java ---------------------------------



```



## Julia

Save the following code to a file (for example "minesweeper.jl") and include it in the Julia REPL to play the game.

```Julia

# Minesweeper:

mutable struct Field
    size::Tuple{Int, Int}
    numbers::Array{Int, 2}
    possible_mines::Array{Bool, 2}
    actual_mines::Array{Bool, 2}
    visible::Array{Bool, 2}
end

function Field(x, y)
    size = (x, y)
    actual_mines = convert(Array{Bool, 2}, rand(x, y) .< 0.15)
    possible_mines = zeros(Bool, x, y)
    numbers = zeros(Int, x, y)
    visible = zeros(Bool, x, y)
    for i = 1:x
        for j = 1:y
            n = 0
            for di = -1:1
                for dj = -1:1
                    n += (0 < di+i <= x && 0 < dj+j <= y) ?
                         (actual_mines[di+i, dj+j] ? 1 : 0) : 0
                end
            end
            numbers[i, j] = n
        end
    end
    return Field(size, numbers, possible_mines, actual_mines, visible)
end

function printfield(f::Field; showall = false)
    spaces = Int(floor(log(10, f.size[2])))

    str = " "^(4+spaces)
    for i in 1:f.size[1]
        str *= string(" ", i, " ")
    end
    str *= "\n" * " "^(4+spaces) * "___"^f.size[1] * "\n"
    for j = 1:f.size[2]
        str *= " " * string(j) * " "^(floor(log(10, j)) > 0 ? 1 : spaces+1) * "|"
        for i = 1:f.size[1]
            if showall
                str *= f.actual_mines[i, j] ? " * " : "   "
            else
                if f.visible[i, j]
                    str *= " " * string(f.numbers[i, j] > 0 ? f.numbers[i, j] : " ") * " "
                elseif f.possible_mines[i, j]
                    str *= " ? "
                else
                    str *= " . "
                end
            end
        end
        str *= "\r\n"
    end
    println("Found " * string(length(f.possible_mines[f.possible_mines.==true])) *
            " of " * string(length(f.actual_mines[f.actual_mines.==true])) * " mines.\n")
    print(str)
end

function parse_input(str::String)
    input = split(chomp(str), " ")
    mode = input[1]
    println(str)
    coords =  length(input) > 1 ? (parse(Int,input[2]), parse(Int,input[3])) : (0, 0)
    return mode, coords
end

function eval_input(f::Field, str::String)
    mode, coords = parse_input(str)
    (coords[1] > f.size[1] || coords[2] > f.size[2]) && return true
    if mode == "o"
        reveal(f, coords...) || return false
    elseif mode == "m" && f.visible[coords...] == false
        f.possible_mines[coords...] = !f.possible_mines[coords...]
    elseif mode == "close"
        error("You closed the game.")
    end
    return true
end

function reveal(f::Field, x::Int, y::Int)
    (x > f.size[1] || y > f.size[2]) && return true # check for index out of bounds
    f.actual_mines[x, y] && return false # check for mines
    f.visible[x, y] = true
    if f.numbers[x, y] == 0
        for di = -1:1
            for dj = -1:1
                if (0 < di+x <= f.size[1] && 0 < dj+y <= f.size[2]) &&
                        f.actual_mines[x+di, y+dj] == false &&
                        f.visible[x+di, y+dj] == false
                    reveal(f, x+di, y+dj)
                end
            end
        end
    end
    return true
end

function play()
    print("\nWelcome to Minesweeper\n\nEnter the gridsize x  y:\n")
    s = split(readline(), " ")
    f = Field(parse.(Int,s)...)
    won = false
    while true
        printfield(f)
        print("\nWhat do you do? (\"o x y\" to reveal a field; \"m x y\" to toggle a mine; close)\n")
        eval_input(f, readline()) || break
        print("_"^80 * "\n")
        (f.actual_mines == f.possible_mines ||
              f.visible == .!f.actual_mines) && (won = true; break)
    end
    println(won ? "You won the game!" : "You lost the game:\n")
    printfield(f, showall = true)
end

play()

```

typical game:

```Julia

julia> include("minesweeper.jl")

Welcome to Minesweeper

Enter the gridsize x y:
6 4
Found 0 of 5 mines.

     1  2  3  4  5  6
    __________________
 1 | .  .  .  .  .  .
 2 | .  .  .  .  .  .
 3 | .  .  .  .  .  .
 4 | .  .  .  .  .  .

What do you do? ("o x y" to reveal a field; "m x y" to toggle a mine; close)
o 1 1
________________________________________________________________________________

Found 0 of 5 mines.

     1  2  3  4  5  6
    __________________
 1 | 1  .  .  .  .  .
 2 | .  .  .  .  .  .
 3 | .  .  .  .  .  .
 4 | .  .  .  .  .  .

What do you do? ("o x y" to reveal a field; "m x y" to toggle a mine; close)
o 1 2
________________________________________________________________________________

Found 0 of 5 mines.

     1  2  3  4  5  6
    __________________
 1 | 1  .  .  .  .  .
 2 | 2  .  .  .  .  .
 3 | .  .  .  .  .  .
 4 | .  .  .  .  .  .

What do you do? ("o x y" to reveal a field; "m x y" to toggle a mine; close)
o 2 1
________________________________________________________________________________

Found 0 of 5 mines.

     1  2  3  4  5  6
    __________________
 1 | 1  2  .  .  .  .
 2 | 2  .  .  .  .  .
 3 | .  .  .  .  .  .
 4 | .  .  .  .  .  .

What do you do? ("o x y" to reveal a field; "m x y" to toggle a mine; close)
o 6 4
________________________________________________________________________________

Found 0 of 5 mines.

     1  2  3  4  5  6
    __________________
 1 | 1  2  .  .  .  .
 2 | 2  .  2  1  1  1
 3 | .  .  2
 4 | .  .  1

What do you do? ("o x y" to reveal a field; "m x y" to toggle a mine; close)
m 2 2
________________________________________________________________________________

Found 1 of 5 mines.

     1  2  3  4  5  6
    __________________
 1 | 1  2  .  .  .  .
 2 | 2  ?  2  1  1  1
 3 | .  .  2
 4 | .  .  1

What do you do? ("o x y" to reveal a field; "m x y" to toggle a mine; close)
close
ERROR: You closed the game.


```



## Locomotive Basic



```locobasic
10 mode 1:randomize time
20 defint a-z
30 boardx=6:boardy=4
40 dim a(boardx,boardy)
50 dim c(boardx+2,boardy+2)
60 dim c2(boardx+2,boardy+2)
70 nmines=int((rnd/3+.1)*boardx*boardy)
80 for i=1 to nmines
90 ' place random mines
100 xp=int(rnd*(boardx-1)+1)
110 yp=int(rnd*(boardy-1)+1)
120 if a(xp,yp) then 100
130 a(xp,yp)=64
140 for xx=xp to xp+2
150 for yy=yp to yp+2
160 c(xx,yy)=c(xx,yy)+1
170 next yy
180 next xx
190 next i
200 gosub 350
210 x=1:y=1
220 gosub 600
230 ' wait for key press
240 k$=lower$(inkey$)
250 if k$="" then 240
260 if k$="q" and y>1 then gosub 660:y=y-1:gosub 600
270 if k$="a" and y<boardy then gosub 660:y=y+1:gosub 600
280 if k$="o" and x>1 then gosub 660:x=x-1:gosub 600
290 if k$="p" and x<boardx then gosub 660:x=x+1:gosub 600
300 if k$="m" then a(x,y)=a(x,y) xor 128:gosub 600:gosub 1070
310 if k$=" " then a(x,y)=a(x,y) or 512:gosub 700:sx=x:sy=y:gosub 450:x=sx:y=sy:gosub 600
320 goto 240
330 end
340 ' print board
350 mode 1
360 gosub 450
370 locate 1,12
380 print "Move on the board with the Q,A,O,P keys"
390 print "Press Space to clear"
400 print "Press M to mark as a potential mine"
410 print
420 print "There are"nmines"mines."
430 return
440 ' update board
450 for y=1 to boardy
460 for x=1 to boardx
470 locate 2*x,y
480 gosub 530
490 next
500 next
510 return
520 ' print tile
530 if a(x,y) and 128 then print "?":return
540 if c(x+1,y+1)=0 then d$=" " else d$=chr$(c(x+1,y+1)+48)
550 if a(x,y) and 256 then print d$:return
560 'if a(x,y) and 64 then print "M":return
570 print "."
580 return
590 ' turn on tile
600 locate 2*x,y
610 pen 0:paper 1
620 gosub 530
630 pen 1:paper 0
640 return
650 ' turn off tile
660 locate 2*x,y
670 gosub 530
680 return
690 ' clear tile
700 if a(x,y) and 64 then locate 15,20:print "*** BOOM! ***":end
710 locate 1,25:print "-WAIT-";
720 for x2=1 to boardx
730 for y2=1 to boardy
740 c2(x2+1,y2+1)=a(x2,y2)
750 next
760 next
770 ' iterate clearing
780 cl=0
790 for x2=1 to boardx
800 for y2=1 to boardy
810 if c2(x2+1,y2+1) and 512 then gosub 940:cl=cl+1
820 next y2
830 next x2
840 if cl then 780
850 for x2=1 to boardx
860 for y2=1 to boardy
870 vv=c2(x2+1,y2+1)
880 if vv>1000 then a(x2,y2)=vv xor 1024
890 next y2
900 next x2
910 locate 1,25:print "      ";
920 return
930 ' find neighbors
940 c2(x2+1,y2+1)=(c2(x2+1,y2+1) xor 512) or 256 or 1024
950 for ii=0 to 2
960 for jj=0 to 2
970 if ii=0 and jj=0 then 1030
980 if c2(x2+ii,y2+jj) and 64 then 1030
990 if c2(x2+ii,y2+jj) and 128 then 1030
1000 if c2(x2+ii,y2+jj) and 1024 then 1030
1010 c2(x2+ii,y2+jj)=c2(x2+ii,y2+jj) or 512
1020 ' next tile
1030 next jj
1040 next ii
1050 return
1060 ' update discovered mine count
1070 mm=0
1080 for x2=1 to boardx
1090 for y2=1 to boardy
1100 if (a(x2,y2) and 128)>0 and (a(x2,y2) and 64)>0 then mm=mm+1
1110 next
1120 next
1130 if mm=nmines then locate 5,22:print "Congratulations, you've won!":end
1140 return
```



## M2000 Interpreter

Playing in M2000 Console. We can change Form 80,32 and rows=8 and columns=14 for greater board

Cheat mode enabled with c command. If we use ? column row and place it where the mines are (an all of them) then game end and player win. Without cheat mode command ? can be used to block clearing.

By default when first play and hit mine, the mine change position. We can alter this putting a rem before swap_first%=used%

We can place anything as command and we get response for errors. Also we can pass multiple commands.

Algorithm not used recursion, but a feature of M2000, a stack of values which is visible from all statements in a module (and subroutines too). So when we need to clear to adjacent cells we just pass command to stack. Also there are two kind of clearing, type o and type n. The difference is that the second one allow searching if there is no adjacent cell with a mine. The second one send commands as type o (as a choice, you can alter this with a type n).



```M2000 Interpreter

Module Minesweeper {
      Font "Arial Black"
      Bold 0
      Form 60,40
      Refresh 1000
      Def com$, com_label$
      Def x, b_row, b_col, where, top_where
      Def rows=4, columns=6, swap_first%
      Def boolean skiptest, end_game, cheat
      Dim Board$(0 to rows+1, 0 to columns+1)="? "
      Def mines%, i%, j%, used%, acc%, n%, m%
      mines%=max.data(random(int(columns*rows*.1),int(columns*rows*.2)-1), 1)
      For i%=1 to rows:For j%=1 to columns
            Board$(i%,j%)=". "
      Next j%:Next i%
      used%=mines%
      While used%
            used%--
            Do
                  i%=random(1,rows)
                  j%=random(1, columns)
            Until right$(Board$(i%,j%),1)=" "
            Board$(i%,j%)=".*"
      End While
      used%=rows*columns-mines%
      \\ remove rem so to never loose from first open
      Rem :
      swap_first%=used%
      \\ when mines%=0 or used%=0 then player win
      Report {Minesweeper - rosettacode task
            Commands:
            -  ? 1 2  flag/unflag 1 2
            -  1 2   open 1 2
            -  q  to quit
            You can pass multiple commands in a line, but q erase all before execute
      }
      top_where=Row
      While not End_Game {GameLoop()}
      End
      Sub PrintBoard()
      Cls, top_where
      Print
      Print "   X  ";
      For j%=1 to columns {
            Print format$("{0::-3}  ", j%);
      }
      Print
      For i%=1 to rows {
            Print format$(" {0::-3}  ", i%);
            For j%=1 to columns {
                  Print "  ";Left$(Board$(i%,j%),1);"  ";
                  \\ rem above and unrem follow line to display mines
                  Rem: Print "  ";Board$(i%,j%)+" ";
            }
            Print
      }
      End Sub
      Sub PrintMines()
      Cls, top_where
      Print
      Print "   X  ";
      For j%=1 to columns {
            Print format$("{0::-3}  ", j%);
      }
      Print
      For i%=1 to rows {
            Print format$(" {0::-3}  ", i%);
            For j%=1 to columns {
                  Print "  ";Right$(Board$(i%,j%),1);"  ";
            }
            Print
      }
      End Sub
      Sub GameLoop()
            Local com$, loopagain as boolean
            PrintBoard()
            InputCommand()
            do
                  loopagain=true
                  while not empty
                              \\ process game command
                        Read com$
                        if com$="q " Then
                              Print "Quit" : end_game=True : exit
                        Else.if com$="o " Then
                              OpenCell()
                        Else.if com$="n " Then
                              OpenCell2()
                        Else.if com$="? " Then
                              SwapCell()
                        Else.if com$="c " Then
                              Exit Sub
                        End if
                  End While
                  If mines%=0 or used%=0 then
                        PrintBoard(): Print "Player Win": end_game=True: Exit Sub
                  End if
                  If mines%=-1 then
                        if swap_first%=used% then
                              mines%=rows*columns-used%
                              Local n%, m%
                              While mines%
                                    Let n%=random(1,rows), m%=random(1, columns)
                                    If Board$(n%, m%)=". " then  Board$(n%, m%)=".*" : mines%=0
                              End While
                              Board$(i%, j%)=". "
                              mines%=rows*columns-used%
                              swap_first%=-100
                              Push i%, j%, "o "
                              loopagain=false
                        else
                              PrintMines(): Print "Player Loose": end_game=True : Exit Sub
                        end if
                  End If
            Until loopagain
            Flush
            Refresh if(End_Game->10,1000)
      End Sub
      Sub InputCommand()
            where=row
            While com$=""
                  cls, where
                  Print "x x | ? x x | q >";
                  Refresh 10
                  Try {
                        Input "", com$
                  }
            End While
            x=1
            Flush
            While com$<>""
                  com_label$=""
                  ParseCommand()
                  if len(com_label$)<>2 then
                        com$="" : Print com_label$ : Flush
                        Refresh 10
                        push key$ : drop
                  else
                        Data com_label$, b_col, b_row
                  End if
            End While
            Refresh 1000
      End Sub
      Sub ParseCommand()
            com_label$="o "
            skiptest=true
            ReadColumn()
            if len(com_label$)<>2 then
                  com$=""
            Else.if x=-1 then
                  com_label$=lcase$(Left$(com$,1))+" "
                  com$=mid$(com$, 2)
                  x=1
                  if len(com_label$)<>2 then
                        com_label$="no command found"
                  else.if com_label$="? " then
                        ReadColumn()
                        if x>-1 then ReadRow()
                  else.if com_label$="c " then
                        cheat=true
                  else.if com_label$="q " then
                        flush
                        com$=""
                  else
                        com_label$="Use q or ? for commands"
                        com$=""
                  End if
            else
                  ReadRow()
                  if x>-1 then com_label$="o "
            End if
      End Sub
      Sub ReadRow()
            com$=mid$(com$,x)
            b_row=val(com$, "??", x)
            if x=-1 then
                  com_label$="Need a row"
            else.if b_row<1 or b_row>rows then
                  com_label$="Need a row from 1 to "+str$(rows)
                  x=-1
            else
                  com$=mid$(com$,x+1)
                  x=1
            End if
      End Sub
      Sub ReadColumn()
            com$=mid$(com$,x)
            b_col=val(com$, "??", x)
            if x=-1 then
                  if not skiptest then com_label$="Need a column"
            else.if b_col<1 or b_col>columns then
                  com_label$="Need a column from 1 to"+str$(columns)
            else
                  com$=mid$(com$,x+1)
                  x=1
            End if
            skiptest=false
      End Sub
      Sub SwapCell()
            Read j%, i%
            If left$(Board$(i%,j%),1)="?" then
                  Board$(i%,j%) ="."+Right$(Board$(i%,j%),1)
                  If cheat Then if Right$(Board$(i%,j%),1)="*" then mines%++
            Else.If left$(Board$(i%,j%),1)="." then
                  Board$(i%,j%) ="?"+Right$(Board$(i%,j%),1)
                  If cheat Then if Right$(Board$(i%,j%),1)="*" then mines%--
            End if
      End Sub
      Sub OpenCell()
            Read j%, i%
            If left$(Board$(i%,j%),1)="." then {
                  if Right$(Board$(i%,j%),1)="*" then mines%=-1 : flush : exit
                  acc%=0
                  used%--
                  Local n%, m%
                  For n%=i%-1 to i%+1 {
                        For m%=j%-1 to j%+1 {
                                If Right$(Board$(n%,m%),1)="*" then acc%++
                        }
                  }
                  For n%=i%-1 to i%+1 {
                        For m%=j%-1 to j%+1 {
                             if not (n%=i% and m%=j%) then
                                   if not Right$(Board$(n%,m%),1)="*" then
                                         If left$(Board$(n%,m%),1)="." then
                                              Push n%, m%, "n "  ' reverse to stack
                                              Rem : Print stack.size : Refresh
                                         End If
                                   End If
                             End If
                        }
                  }
                  Board$(i%,j%)=if$(acc%=0->"  ",str$(acc%, "# "))
            }
      End Sub
      Sub OpenCell2()
            Read J%, i%
            If left$(Board$(i%,j%),1)="." then {
                  if Right$(Board$(i%,j%),1)="*" then exit
                  acc%=0
                  used%--
                  For n%=i%-1 to i%+1 {
                        For m%=j%-1 to j%+1 {
                                If Right$(Board$(n%,m%),1)="*" then acc%++
                        }
                  }
                  \\ if cell has no mines around then we check all
                  if acc%=0 then
                        Local n%, m%
                        For n%=i%-1 to i%+1
                              For m%=j%-1 to j%+1
                                   if not (n%=i% and m%=j%) then
                                         if not Right$(Board$(n%,m%),1)="*" then
                                               If left$(Board$(n%,m%),1)="." then
                                                    Push n%, m%, "o "  ' reverse to stack
                                                    Rem : Print stack.size : Refresh
                                               End If
                                         End If
                                   End If
                              Next m%
                        Next n%
                  End If
                  Board$(i%,j%)=if$(acc%=0->"  ",str$(acc%, "# "))
            }
      End Sub
}
Minesweeper

```



## Mathematica


### Mouse version


```mathematica
DynamicModule[{m = 6, n = 4, minecount, grid, win, reset, clear,
  checkwin},
 reset[] :=
  Module[{minesdata, adjacentmines},
   minecount = RandomInteger[Round[{.1, .2} m*n]];
   minesdata =
    Normal@SparseArray[# -> 1 & /@
       RandomSample[Tuples[Range /@ {m, n}], minecount], {m, n}];
   adjacentmines =
    ArrayPad[
     Total[RotateLeft[
         ArrayPad[minesdata,
          1], #] & /@ {{-1, -1}, {0, -1}, {1, -1}, {-1, 0}, {1,
         0}, {-1, 1}, {0, 1}, {1, 1}}], -1];
   grid = Array[{If[minesdata[[#1, #2]] == 1, "*",
        adjacentmines[[#1, #2]]], ".", 1} &, {m, n}]; win = ""];
 clear[i_, j_] :=
  If[grid[[i, j, 1]] == "*", win = "You lost.";
    grid = grid /. {{n_Integer, "?", _} :> {n, "x", 0}, {"*",
         ".", _} :> {"*", "*", 0}},
    grid[[i, j]] = {grid[[i, j, 1]], grid[[i, j, 1]], 0};
    If[grid[[i, j, 2]] == 0, grid[[i, j, 2]] = "";
     clear[i + #[[1]],
        j + #[[2]]] & /@ {{-1, -1}, {0, -1}, {1, -1}, {-1, 0}, {1,
        0}, {-1, 1}, {0, 1}, {1, 1}}]] /;
   1 <= i <= m && 1 <= j <= n && grid[[i, j, 2]] == ".";
 checkwin[] :=
  If[FreeQ[grid, {_Integer, "?", _} | {_, "*", _} | {_Integer,
      ".", _}], win = "You win.";
   grid = grid /. {{"*", ".", _} :> {"*", "?", 1}}];
 reset[];
 Panel@Column@{Row@{Dynamic@minecount, "\t",
      Button["Reset", reset[]]},
    Grid[Table[
      With[{i = i, j = j},
       EventHandler[
        Button[Dynamic[grid[[i, j, 2]]], Null, ImageSize -> {17, 17},
         Appearance ->
          Dynamic[If[grid[[i, j, 3]] == 0, "Pressed",
            "DialogBox"]]], {{"MouseClicked", 1} :>
          If[win == "", clear[i, j]; checkwin[]], {"MouseClicked",
           2} :> If[win == "",
           Switch[grid[[i, j, 2]], ".", grid[[i, j, 2]] = "?";
            minecount--, "?", grid[[i, j, 2]] = "."; minecount++];
           checkwin[]]}]], {i, 1, m}, {j, 1, n}], Spacings -> {0, 0}],
     Dynamic[win]}]
```



## MATLAB


===Built-In Graphical Version===


```MATLAB>xpbombs</lang



### Original Graphical Version


Graphical version with mouse controls designed to imitate Microsoft Minesweeper as much as possible.

Changes to task:
*Number of mines is shown at the beginning and counts down as the user marks mine flags.
*Mine flags are indicated by the character 'M', unknown flags are indicated by '?'.
*Adjacent mine-free space is cleared, excluding squares marked as mines, but including squares marked as unknown.
*Game is won, not by marking all mines with flags, but by clearing all the non-mine squares.
Items that could use improvement:
*Mines are seeded prior to first click, so it is possible to lose on first move.
*Loading of field and general speed slows at higher field dimensions. Timer also seems to temporarily lose a second on some moves.
*Code uses <code>gca</code> and <code>gcf</code> frequently, which could possibly cause problems if user is working with multiple figure windows at once.


```MATLAB
function Minesweeper

    % Game parameters (should be modified by user)
    nRows = 6;
    nCols = 4;
    percentMines = 0.15;

    % Create minefield
    nMines = ceil(percentMines*nRows*nCols);
    field = makeGrid(nRows, nCols, nMines);

    % Create timer for updating in axes title
    stopwatch = timer('TimerFcn', {@updateTime, field}, ...
        'ExecutionMode', 'fixedRate', 'Period', 1, 'StartDelay', 1, ...
        'TasksToExecute', 999);

    % Specify callbacks
    set(gcf, 'CloseRequestFcn', {@cleanUp, stopwatch})
    set(gca, 'ButtonDownFcn', {@onClick, field, stopwatch})


end

function field = makeGrid(nRows, nCols, nMines)
    % Create minefield with unit squares
    % Use quadrant IV to make indexing semi-consistent
    % i.e. square in ith row, jth column has lower-right corner at (j, -i)
    figure
    set(gcf, 'Color', [1 1 1])
    axis([0 nCols -nRows 0])
    axis square
    axis manual
    hold on
    set(gca, 'GridLineStyle', '-')
    grid on
    set(gca, 'XTick', 0:nCols)
    set(gca, 'YTick', -nRows:0)
    set(gca, 'XTickLabel', [])
    set(gca, 'YTickLabel', [])
    set(gca, 'Color', [0.6 0.6 0.6])
    setTitle(nMines, ': )', 0)
    xlabel('left-click to dig, right-click to mark')

    % Set up field structure
    % text will contain the handles to the labels in each square
    %   One character per square (or other functions will break)
    %   . Nothing noted
    %   M Marked as mine
    %   ? Marked as unknown
    %   1-8 Digits indicate how many mines this square touches
    %   * Mine (game over)
    %   (blank) Square has been (dug) and is not mined nor touching any mines
    % squares will contain handles to the colored "fill" objects
    %   fill objects will be deleted once square is "dug"
    %   Use ishandle() to determine if square is not yet dug
    % mines will contain a logical array indicating positions of mines
    %   true Mine
    %   false No mine
    % Later versions of MATLAB use gobjects() to preallocate text and squares
    field = struct('text', zeros(nRows, nCols), ...
        'squares', zeros(nRows, nCols), ...
        'mines', false(nRows, nCols));

    % Create individual square color and label objects
    for r = 1:nRows
        for c = 1:nCols
            field.squares(r, c) = ...
                fill([c-1 c-1 c c c-1], [-r+1 -r -r -r+1 -r+1], [0.9 0.9 0.9]);
            set(field.squares(r, c), 'HitTest', 'off')
            field.text(r, c) = text(c-0.5, -r+0.5, '.');
            set(field.text(r, c), 'FontSize', 12, 'FontWeight', 'bold', ...
                'HorizontalAlignment', 'center', 'HitTest', 'off');
        end
    end

    % Place mines randomly without repeats
    k = 0;
    while k < nMines
        idx = randi(nRows*nCols);
        if ~field.mines(idx)
            field.mines(idx) = true;
            k = k+1;
        end
    end
end

function onClick(obj, event, field, stopwatch)
    if strcmp(stopwatch.Running, 'off')
        start(stopwatch)
    end

    pt = get(obj, 'CurrentPoint');
    r = ceil(-pt(1, 2));
    c = ceil(pt(1, 1));

    if r > 0 && c > 0 && r <= size(field.squares, 1) && ... % Not yet been "dug"
            c <= size(field.squares, 2) && ishandle(field.squares(r, c))
        buttons = {'normal' 'alt' 'extend'};
        btn = find(strcmp(get(get(obj, 'Parent'), 'SelectionType'), buttons));
        labels = '.M?'; % Unmarked, mine flag, unknown flag
        currLabel = get(field.text(r, c), 'String');

        if btn == 1     % Left click
            if currLabel ~= labels(2);  % Don't dig if flagged as mine
                if field.mines(r, c)    % Mine there -> you lose
                    gameLost(field, stopwatch, r, c)
                else                    % No mine -> free to dig
                    minesLeft = countMineFlags(field);
                    digSquare(field, r, c)
                    if all(all(ishandle(field.squares) == field.mines))
                        gameWon(field, stopwatch)
                    else
                        faceTimer = timer('StartDelay', 0.5, ...
                            'StartFcn', {@setTitleOnTimer, minesLeft, ...
                            ': o', stopwatch.TasksExecuted}, ...
                            'TimerFcn', {@setTitleOnTimer, minesLeft, ...
                            ': )', stopwatch.TasksExecuted}, ...
                            'StopFcn', @deleteTimer);
                        start(faceTimer)
                    end
                end
            end

        elseif btn == 2 % Right-click
            % Rotate through labels list to the next one
            switch find(currLabel == labels)
                case 1
                    newLabel = labels(2);
                case 2
                    newLabel = labels(3);
                case 3
                    newLabel = labels(1);
            end
            set(field.text(r, c), 'String', newLabel);
            setTitle(countMineFlags(field), ': )', stopwatch.TasksExecuted)

        elseif btn == 3 % Middle-click
            % Mark/unmark unknown flag
            if currLabel == labels(1)
                set(field.text(r, c), 'String', labels(3));
            elseif currLabel == labels(3)
                set(field.text(r, c), 'String', labels(1));
            end
        end
    end
end

function updateTime(obj, event, field)
    setTitle(countMineFlags(field), ': )', obj.TasksExecuted)
end

function deleteTimer(obj, event)
    delete(obj)
end

function setTitleOnTimer(obj, event, mines, face, time)
    setTitle(mines, face, time)
end

function setTitle(mines, face, time)
    title(sprintf('%03d Mines        %s        Timer %03d', mines, face, time))
end

function minesLeft = countMineFlags(field)
% Determine how many mines are unmarked (negative means too many mines marked)
    minesLeft = sum(field.mines(:));
    for k = 1:numel(field.text)
        if get(field.text(k), 'String') % Not an empty string
            minesLeft = minesLeft-(get(field.text(k), 'String') == 'M');
        end
    end
end

function digSquare(field, r, c)
% If square is touching one or more mines then indicate the number
% Otherwise indicate no mines and dig surrounding squares recursively
% Assumes current square is clear of mines
    delete(field.squares(r, c))
    [nRows, nCols] = size(field.mines);
    surrR = [r-1 r r+1 r-1 r+1 r-1 r r+1];
    surrC = [c-1 c-1 c-1 c c c+1 c+1 c+1];
    toDelete = surrR < 1 | surrR > nRows | surrC < 1 | surrC > nCols;
    surrR(toDelete) = [];
    surrC(toDelete) = [];
    nearMines = sum(field.mines(sub2ind([nRows nCols], surrR, surrC)));
    label = sprintf('%d', nearMines);
    textColor = [0 0 0 ; 0 0 1 ; 0 1 0 ; 1 0 0 ; 0.5 0.1 0.9 ; ...
        0.6 0 0 ; 0.2 0.5 0.3 ; 0.2 0.2 0.1 ; 0 0 0];
    if ~nearMines
        label = '';
        for k = 1:length(surrR)
            if ~field.mines(surrR(k), surrC(k)) && ...
                    ishandle(field.squares(surrR(k), surrC(k))) && ...
                    ~strcmp(get(field.text(k), 'String'), 'M')
                digSquare(field, surrR(k), surrC(k))
            end
        end
    end
    set(field.text(r, c), 'String', label, 'Color', textColor(nearMines+1, :))
end

function gameLost(field, stopwatch, r, c)
    stop(stopwatch)
    setTitle(countMineFlags(field), 'X (', stopwatch.TasksExecuted)
    set(field.squares(r, c), 'FaceColor', [1 0 0])
    for k = 1:numel(field.text)
        if field.mines(k) && any(get(field.text(k), 'String') == '.?')
            set(field.text(k), 'String', '*', 'FontSize', 20)
        elseif ~field.mines(k) && ishandle(field.squares(k)) && ...
                get(field.text(k), 'String') == 'M'
            set(field.text(k), 'String', 'X', 'Color', [1 0 0])
        end
    end
    set(gca, 'HitTest', 'off')
    queryPlayAgain('Game over')
end

function gameWon(field, stopwatch)
% Flag any leftover mines and indicate win
    stop(stopwatch)
    set(field.text(ishandle(field.squares)), 'String', 'M')
    setTitle(0, 'B )', stopwatch.TasksExecuted)
    set(gca, 'HitTest', 'off')
    queryPlayAgain('Minefield cleared!')
end

function queryPlayAgain(msg)
% Ask player if they want to play again
% Reset game by closing and reopening figure
    choice = questdlg(sprintf('%s\nWould you like to play again?', msg), ...
        '', 'Yes', 'No', 'No');
    if strcmp(choice, 'Yes')
        close
        Minesweeper
    end
end

function cleanUp(obj, event, stopwatch)
% Stop and close down all necessary processes
    stop(stopwatch)
    delete(stopwatch)
    delete(obj)
end
```



## OCaml



```ocaml
exception Lost
exception Won

let put_mines g m n mines_number =
  let rec aux i =
    if i < mines_number then
    begin
      let x = Random.int n
      and y = Random.int m in
      if g.(y).(x)
      then aux i
      else begin
        g.(y).(x) <- true;
        aux (succ i)
      end
    end
  in
  aux 0

let print_abscissas n =
  print_string "\n      "; for x = 1 to n do print_int (x mod 10) done;
  print_string "\n      "; for x = 1 to n do print_char '|' done;
  print_newline()

let print_display d n =
  print_abscissas n;
  Array.iteri (fun y line ->
    Printf.printf " %2d - " (y+1);  (* print ordinates *)
    Array.iter print_char line;
    print_newline()
  ) d;
  print_newline()

let reveal d g n =
  print_abscissas n;
  Array.iteri (fun y line ->
    Printf.printf " %2d - " (y+1);  (* print ordinates *)
    Array.iteri (fun x c ->
      print_char (
        match c, g.(y).(x) with
        | '0'..'9', _ -> c
        | '.', true -> 'x'
        | '?', true -> 'X'
        | '?', false -> 'N'
        | '.', false -> '.'
        | _ -> c)
    ) line;
    print_newline()
  ) d;
  print_newline()

let toggle_mark d x y =
  match d.(y).(x) with
  | '.' -> d.(y).(x) <- '?'
  | '?' -> d.(y).(x) <- '.'
  | _ -> ()

let rec feedback g d x y =
  if d.(y).(x) = '.' then
  begin
    let n = ref 0 in  (* the number of mines around *)
    for i = (pred y) to (succ y) do
      for j = (pred x) to (succ x) do
        try if g.(i).(j) then incr n
        with _ -> ()
      done;
    done;
    match !n with
    | 0 ->
        (* recursive feedback when no mines are around *)
        d.(y).(x) <- ' ';
        for j = (pred y) to (succ y) do
          for i = (pred x) to (succ x) do
            try feedback g d i j
            with _ -> ()
          done
        done
    | _ ->
        d.(y).(x) <- (string_of_int !n).[0]
  end

let clear_cell g d x y =
  if g.(y).(x)
  then (d.(y).(x) <- '!'; raise Lost)
  else feedback g d x y

let rec user_input g d =
  try
    let s = read_line() in
    match Scanf.sscanf s "%c %d %d" (fun c x y -> c,x,y) with
    | 'm', x, y -> toggle_mark d (x-1) (y-1)
    | 'c', x, y -> clear_cell g d (x-1) (y-1)
    | _ -> raise Exit
  with Exit | Scanf.Scan_failure _
  | Invalid_argument "index out of bounds" ->
      print_string "# wrong input, try again\n> ";
      user_input g d

let check_won g d =
  let won = ref true in
  Array.iteri (fun y line ->
    Array.iteri (fun x c ->
      match c, g.(y).(x) with
      | '.', _ -> won := false
      | '?', false -> won := false
      | _ -> ()
    ) line
  ) d;
  if !won then raise Won

let minesweeper n m percent =
  let round x = int_of_float (floor (x +. 0.5)) in
  let mines_number = round ((float (n * m)) *. percent) in
  (* the ground containing the mines *)
  let g = Array.make_matrix m n false in
  put_mines g m n mines_number;
  Printf.printf "# You have to find %d mines\n" mines_number;
  (* what's displayed to the user *)
  let d = Array.make_matrix m n '.' in
  try
    while true do
      print_display d n;
      print_string "> ";
      user_input g d;
      check_won g d;
    done
  with
  | Lost ->
      print_endline "# You lost!";
      reveal d g n
  | Won ->
      print_endline "# You won!";
      reveal d g n

let () =
  Random.self_init();
  let ios, fos = int_of_string, float_of_string in
  let n, m, percent =
    try ios Sys.argv.(1), ios Sys.argv.(2), fos Sys.argv.(3)
    with _ ->
      try ios Sys.argv.(1), ios Sys.argv.(2), 0.2
      with _ -> (6, 4, 0.2)
  in
  minesweeper n m percent;
;;
```


Sample output:

 % ocaml minesweeper.ml
 # You have to find 5 mines

       123456
       ||||||
   1 - ......
   2 - ......
   3 - ......
   4 - ......

 > c 1 1

       123456
       ||||||
   1 -   2...
   2 - 113...
   3 - ......
   4 - ......

 > m 1 3

       123456
       ||||||
   1 -   2...
   2 - 113...
   3 - ?.....
   4 - ......

 > c 2 3
 # You lost!

       123456
       ||||||
   1 -   2xx.
   2 - 113xx.
   3 - N!....
   4 - ......


## Perl


```perl
#!/usr/bin/perl
use warnings;
use strict;

{   package Local::Field;

    use constant {
        REAL  => 0,
        SHOW  => 1,
        COUNT => 2,
    };

    sub new {
        my ($class, $width, $height, $percent) = @_;
        my $field;
        for my $x (1 .. $width) {
            for my $y (1 .. $height) {
                $field->[$x - 1][$y - 1][REAL] = ' ';
                $field->[$x - 1][$y - 1][SHOW] = '.';
            }
        }
        for (1 .. $percent / 100 * $width * $height) {
            my ($x, $y) = map int rand $_, $width, $height;
            redo if 'm' eq $field->[$x][$y][REAL];
            $field->[$x][$y][REAL] = 'm';
            for my $i ($x - 1 .. $x + 1) {
                for my $j ($y - 1 .. $y + 1) {
                    $field->[$i][$j][COUNT]++
                        if $i >= 0 and $j >= 0
                        and $i <= $#$field and $j <= $#{ $field->[0] };
                }
            }
        }
        bless $field, $class;
    }


    sub show {
        my ($self) = @_;
        print "\n  ";
        printf '%2d ', $_ + 1 for 0 .. $#$self;
        print "\n";

        for my $row (0 .. $#{ $self->[0] }) {
            printf '%2d ', 1 + $row;
            for my $column (0 .. $#$self) {
                print $self->[$column][$row][SHOW], '  ';
            }
            print "\n";
        }
    }


    sub mark {
        my ($self, $x, $y) = @_;
        $_-- for $x, $y;

        if ('.' eq $self->[$x][$y][SHOW]) {
            $self->[$x][$y][SHOW] = '?';

        } elsif ('?' eq $self->[$x][$y][SHOW]) {
            $self->[$x][$y][SHOW] = '.';
        }
    }


    sub end {
        my $self = shift;
        for my $y (0 .. $#{ $self->[0] }) {
            for my $x (0 .. $#$self) {
                $self->[$x][$y][SHOW] = '!' if '.' eq $self->[$x][$y][SHOW]
                    and 'm' eq $self->[$x][$y][REAL];
                $self->[$x][$y][SHOW] = 'x' if '?' eq $self->[$x][$y][SHOW]
                    and 'm' ne $self->[$x][$y][REAL];
            }
        }
        $self->show;
        exit;
    }

    sub _declassify {
        my ($self, $x, $y) = @_;
        return if '.' ne $self->[$x][$y][SHOW];
        if (' ' eq $self->[$x][$y][REAL] and '.' eq $self->[$x][$y][SHOW]) {
            $self->[$x][$y][SHOW] = $self->[$x][$y][COUNT] || ' ';
        }
        return if ' ' ne $self->[$x][$y][SHOW];

        for my $i ($x - 1 .. $x + 1) {
            next if $i < 0 or $i > $#$self;
            for my $j ($y - 1 .. $y + 1) {
                next if $j < 0 or $j > $#{ $self->[0] };
                no warnings 'recursion';
                $self->_declassify($i, $j);
            }
        }
    }


    sub clear {
        my ($self, $x, $y) = @_;
        $_-- for $x, $y;
        return unless '.' eq $self->[$x][$y][SHOW];

        print "You lost.\n" and $self->end if 'm' eq $self->[$x][$y][REAL];

        $self->_declassify($x, $y);
    }


    sub remain {
        my $self = shift;
        my $unclear = 0;
        for my $column (@$self) {
            for my $cell (@$column) {
                $unclear++ if '.' eq $cell->[SHOW];
            }
        }
        return $unclear;
    }

}

sub help {
    print << '__HELP__';
Commands:
h     ... help
q     ... quit
m X Y ... mark/unmark X Y
c X Y ... clear X Y
__HELP__
}


my ($width, $height, $percent) = @ARGV;
$width   ||= 6;
$height  ||= 4;
$percent ||= 15;

my $field = 'Local::Field'->new($width, $height, $percent);

my $help = 1;
while (1) {
    $field->show;
    help() if $help;
    $help = 0;
    my $remain = $field->remain;
    last if 0 == $remain;
    print "Cells remaining: $remain.\n";
    my $command = <STDIN>;
    exit if $command =~ /^q/i;

    if ($command =~ /^m.*?([0-9]+).*?([0-9]+)/i) {
        $field->mark($1, $2);

    } elsif ($command =~ /^c.*?([0-9]+).*?([0-9]+)/i) {
        $field->clear($1, $2);

    } elsif ($command =~ /^h/i) {
        $help = 1;

    } else {
        print "Huh?\n";
    }
}
print "You won!\n";

```



## Perl 6

```perl6
enum Tile-Type <Empty Mine>;

class Tile {
    has Tile-Type $.type;
    has $.face is rw;
    method Str { with $!face { ~$!face } else { '.' } }
}

class Field {
    has @.grid;
    has Int $.width;
    has Int $.height;
    has Int $.mine-spots;
    has Int $.empty-spots;

    method new (Int $height, Int $width, Rat $mines-ratio=0.1) {

        my $mine-spots = round $width*$height*$mines-ratio;
        my $empty-spots = $width*$height - $mine-spots;

        my @grid;
        for ^$height X ^$width -> ($y, $x) {
            @grid[$y][$x] = Tile.new(type => Empty);
        }
        for (^$height).pick($mine-spots) Z (^$width).pick($mine-spots) -> ($y, $x) {
            @grid[$y][$x] = Tile.new( type => Mine);
        }
        self.bless(:$height, :$width, :@grid, :$mine-spots, :$empty-spots);
    }

    method open( $y, $x) {
        return if @!grid[$y][$x].face.defined;

        self.end-game("KaBoom") if @!grid[$y][$x].type ~~ Mine;

        my @neighbors = gather do
	    take [$y+.[0],$x+.[1]]
		if 0 <= $y + .[0] < $!height && 0 <= $x + .[1] < $!width
		 for [-1,-1],[+0,-1],[+1,-1],
		     [-1,+0],        [+1,+0],
		     [-1,+1],[+0,+1],[+1,+1];

        my $mines = +@neighbors.grep: { @!grid[.[0]][.[1]].type ~~ Mine };

        $!empty-spots--;
        @!grid[$y][$x].face = $mines || ' ';

        if $mines == 0 {
            self.open(.[0], .[1]) for @neighbors;
        }
        self.end-game("You won") if $!empty-spots == 0;
    }

    method end-game(Str $msg ) {
        for ^$!height X ^$!width -> ($y, $x) {
            @!grid[$y][$x].face = '*' if @!grid[$y][$x].type ~~ Mine
        }
        die $msg;
    }

    method mark ( $y, $x) {
        if !@!grid[$y][$x].face.defined {
            @!grid[$y][$x].face = "⚐";
            $!mine-spots-- if @!grid[$y][$x].type ~~ Mine;
        }
	elsif !@!grid[$y][$x].face eq "⚐" {
            undefine @!grid[$y][$x].face;
            $!mine-spots++ if @!grid[$y][$x].type ~~ Mine;
        }
        self.end-game("You won") if $!mine-spots == 0;
    }

    constant @digs = |('a'..'z') xx *;

    method Str {
        [~] flat '  ', @digs[^$!width], "\n",
	         ' ┌', '─' xx $!width, "┐\n",
	    join '', do for ^$!height -> $y {
	      $y, '│', @!grid[$y][*],   "│\n"; },
	         ' └', '─' xx $!width, '┘';
	}

    method valid ($y, $x) {
        0 <= $y < $!height && 0 <= $x < $!width
    }
}

sub a2n($a) { $a.ord > 64 ?? $a.ord % 32 - 1 !! +$a }

my $f = Field.new(6,10);

loop {
    say ~$f;
    my @w = prompt("[open loc|mark loc|loc]: ").words;
    last unless @w;
    unshift @w, 'open' if @w < 2;
    my ($x,$y) = $0, $1 if @w[1] ~~ /(<alpha>)(<digit>)|$1=<digit>$0=<alpha>/;
    $x = a2n($x);
    given @w[0] {
	when !$f.valid($y,$x) { say "invalid coordinates" }
	when /^o/ { $f.open($y,$x)   }
	when /^m/ { $f.mark($y,$x)   }
	default     { say "invalid cmd" }
    }
    CATCH {
	say "$_: end of game.";
	last;
    }
}

say ~$f;
```



## Phix

Simple text-based console version

```Phix
constant help = """

Minesweeper

### =====


Enter eg A1 to open a cell, FA1 to flag a cell, Q to quit.

The board is initially displayed with 10-20% of cells mined, as follows:

. - an unknown cell (unopened and not flagged)
_ - an empty cell, with no surrounding mines
1..8 - an empty cell, with some nearby mines
? - a cell you have flagged as a mine
    (a flag can only be cleared by opening)

On completion:
X - the mine you detonated (if any)
* - a mine which was not flagged
+ - a mine which was correctly flagged
"""

string board = """

 123456
A......
B......
C......
D......
"""

sequence data = repeat(repeat(0,6),4)
-- 0: empty, no nearby mines
-- 1-8: empty, with surrounding mines
-- 9: a mine

-- data[row][col] maps to board[row*8+4+col], lets quickly verify that:
if find('.',board)!=13 then ?9/0 end if
if rfind('.',board)!=42 then ?9/0 end if
-- (above may trigger if copy/paste/dowload etc messed up whitespace)

constant prompt = "\nEnter eg A1 to open a cell, FA1 to flag a cell, Q to quit, or ? for help:"

integer mines = round((6*4)*0.10+rand(6*4)*0.10),  -- 10-20%
        cleared = 0,
        flagged = 0,
        flag = false,
        row = 0,
        col

procedure plant_mines()
    for i=1 to mines do
        while 1 do
            row = rand(4)
            col = rand(6)
            if data[row][col]!=9 then
                data[row][col] = 9
                for rx=row-1 to row+1 do
                    if rx>=1 and rx<=4 then
                        for cx=col-1 to col+1 do
                            if cx>=1 and cx<=6 then
                                if data[rx][cx]!=9 then
                                    data[rx][cx] += 1
                                end if
                            end if
                        end for
                    end if
                end for
                exit
            end if
        end while
    end for
    printf(1,"%d mines planted\n",mines)
    row = 0
end procedure

procedure clear_cell(integer row, col, drc)
    board[row*8+4+col] = iff(drc?drc+'0':' ')
    cleared += 1
    if drc=0 then
        for rx=row-1 to row+1 do
            if rx>=1 and rx<=4 then
                for cx=col-1 to col+1 do
                    if cx>=1 and cx<=6 then
                        drc = data[rx][cx]
                        if drc!=9
                        and board[rx*8+4+cx]='.' then
                            clear_cell(rx,cx,drc)
                        end if
                    end if
                end for
            end if
        end for
    end if
end procedure

function make_move()
    integer brc = row*8+4+col
    if flag then
        if board[brc]='.' then
            board[brc] = '?'
            flagged += 1
        end if
    else
        integer drc = data[row][col]
        if drc=9 then
            board[brc] = 'X'
            puts(1,"\n\n***BOOM!***")
            return true
        end if
        if find(board[brc],".?") then
            clear_cell(row,col,drc)
        end if
    end if
    row = 0
    flag = false
    -- nb: flagged and cleared may be wrong following incorrect input.
    if flagged=mines
    or cleared=6*4-mines then
        puts(1,"\n\n***You Win!***")
        return true
    end if
    return false    -- no "BOOM" yet!
end function

procedure disclose()
    for row=1 to 4 do
        for col=1 to 6 do
            if data[row][col]=9 then
                integer bdx = row*8+4+col,
                        bch = board[bdx]
                if bch='.' then
                    bch = '*'
                elsif bch='?' then
                    bch = '+'
                elsif bch!='X' then
                    ?9/0
                end if
                board[bdx] = bch
            end if
        end for
    end for
end procedure

plant_mines()
while 1 do
    if not flag and row=0 then
        puts(1,board)
        puts(1,prompt)
    end if
    integer ch = upper(getc(0))
    puts(1,ch)
    if ch='Q' then exit end if
    if ch='?' then
        puts(1,help)
    elsif ch='F' then
        flag = true
    elsif ch>='A'
      and ch<='D' then
        row = ch-'@'
    elsif ch>='1'
      and ch<='6' then
        col = ch-'0'
        if make_move() then exit end if
    else
        printf(1,"\n\nunrecognised:%c\n\n",ch)
        flag = false
        row = 0
    end if
end while
disclose()
puts(1,board&"game over\n\n")
{} = wait_key()
```

```txt

5 mines planted


 123456
A......
B......
C......
D......

Enter eg A1 to open a cell, FA1 to flag a cell, Q to quit, or ? for help:A1

 123456
A  2...
B  2...
C123...
D......

Enter eg A1 to open a cell, FA1 to flag a cell, Q to quit, or ? for help:Q

 123456
A  2*.*
B  2*..
C123...
D.**...
game over

```



## PHP

This example generates webpage. It also uses JavaScript to have right-click events. Use it as CGI script.

```PHP
<?php
define('MINEGRID_WIDTH',  6);
define('MINEGRID_HEIGHT', 4);

define('MINESWEEPER_NOT_EXPLORED', -1);
define('MINESWEEPER_MINE',         -2);
define('MINESWEEPER_FLAGGED',      -3);
define('MINESWEEPER_FLAGGED_MINE', -4);
define('ACTIVATED_MINE',           -5);

function check_field($field) {
    if ($field === MINESWEEPER_MINE || $field === MINESWEEPER_FLAGGED_MINE) {
        return true;
    }
    else {
        return false;
    }
}

function explore_field($field) {
    if (!isset($_SESSION['minesweeper'][$field])
     || !in_array($_SESSION['minesweeper'][$field],
                  array(MINESWEEPER_NOT_EXPLORED, MINESWEEPER_FLAGGED))) {
        return;
    }

    $mines = 0;

    // Make reference to that long name
    $fields  = &$_SESSION['minesweeper'];

    // @ operator helps avoiding isset()... (it removes E_NOTICEs)

    // left side options
    if ($field % MINEGRID_WIDTH !== 1) {
        $mines += check_field(@$fields[$field - MINEGRID_WIDTH - 1]);
        $mines += check_field(@$fields[$field - 1]);
        $mines += check_field(@$fields[$field + MINEGRID_WIDTH - 1]);
    }

    // bottom and top
    $mines += check_field(@$fields[$field - MINEGRID_WIDTH]);
    $mines += check_field(@$fields[$field + MINEGRID_WIDTH]);

    // right side options
    if ($field % MINEGRID_WIDTH !== 0) {
        $mines += check_field(@$fields[$field - MINEGRID_WIDTH + 1]);
        $mines += check_field(@$fields[$field + 1]);
        $mines += check_field(@$fields[$field + MINEGRID_WIDTH + 1]);
    }

    $fields[$field] = $mines;

    if ($mines === 0) {
        if ($field % MINEGRID_WIDTH !== 1) {
            explore_field($field - MINEGRID_WIDTH - 1);
            explore_field($field - 1);
            explore_field($field + MINEGRID_WIDTH - 1);
        }

        explore_field($field - MINEGRID_WIDTH);
        explore_field($field + MINEGRID_WIDTH);

        if ($field % MINEGRID_WIDTH !== 0) {
            explore_field($field - MINEGRID_WIDTH + 1);
            explore_field($field + 1);
            explore_field($field + MINEGRID_WIDTH + 1);
        }
    }
}

session_start(); // will start session storage

if (!isset($_SESSION['minesweeper'])) {
    // Fill grid with not explored tiles
    $_SESSION['minesweeper'] = array_fill(1,
                                         MINEGRID_WIDTH * MINEGRID_HEIGHT,
                                         MINESWEEPER_NOT_EXPLORED);

    // Generate random number of mines between 0.1 and 0.2
    $number_of_mines = (int) mt_rand(0.1 * MINEGRID_WIDTH * MINEGRID_HEIGHT,
                                     0.2 * MINEGRID_WIDTH * MINEGRID_HEIGHT);

    // generate mines randomly
    $random_keys = array_rand($_SESSION['minesweeper'], $number_of_mines);

    foreach ($random_keys as $key) {
        $_SESSION['minesweeper'][$key] = MINESWEEPER_MINE;
    }

    // to make calculations shorter use SESSION variable to store the result
    $_SESSION['numberofmines'] = $number_of_mines;
}

if (isset($_GET['explore'])) {
    if(isset($_SESSION['minesweeper'][$_GET['explore']])) {
        switch ($_SESSION['minesweeper'][$_GET['explore']]) {
            case MINESWEEPER_NOT_EXPLORED:
                explore_field($_GET['explore']);
                break;
            case MINESWEEPER_MINE:
                $lost = 1;
                $_SESSION['minesweeper'][$_GET['explore']] = ACTIVATED_MINE;
                break;
            default:
                // The tile was discovered already. Ignore it.
                break;
        }
    }
    else {
        die('Tile doesn\'t exist.');
    }
}
elseif (isset($_GET['flag'])) {
    if(isset($_SESSION['minesweeper'][$_GET['flag']])) {
        $tile = &$_SESSION['minesweeper'][$_GET['flag']];
        switch ($tile) {
            case MINESWEEPER_NOT_EXPLORED:
                $tile = MINESWEEPER_FLAGGED;
                break;
            case MINESWEEPER_MINE:
                $tile = MINESWEEPER_FLAGGED_MINE;
                break;
            case MINESWEEPER_FLAGGED:
                $tile = MINESWEEPER_NOT_EXPLORED;
                break;
            case MINESWEEPER_FLAGGED_MINE:
                $tile = MINESWEEPER_MINE;
                break;
            default:
                // This tile shouldn't be flagged. Ignore it.
                break;
        }
    }
    else {
        die('Tile doesn\'t exist.');
    }
}

// Check if the player won...
if (!in_array(MINESWEEPER_NOT_EXPLORED, $_SESSION['minesweeper'])
 && !in_array(MINESWEEPER_FLAGGED,      $_SESSION['minesweeper'])) {
    $won = true;
}
?>
<!DOCTYPE html>
<title>Minesweeper</title>
<style>
table {
    border-collapse: collapse;
}

td, a {
    text-align:      center;
    width:           1em;
    height:          1em;
}

a {
    display:         block;
    color:           black;
    text-decoration: none;
    font-size:       2em;
}
</style>
<script>
function flag(number, e) {
    if (e.which === 2 || e.which === 3) {
        location = '?flag=' + number;
        return false;
    }
}
</script>
<?php
    echo "<p>This field contains $_SESSION[numberofmines] mines.";
?>
<table border="1">
<?php
// array_shift() affects array, so we need a copy
$mine_copy = $_SESSION['minesweeper'];

for ($x = 1; $x <= MINEGRID_HEIGHT; $x++) {
    echo '<tr>';
    for ($y = 1; $y <= MINEGRID_WIDTH; $y++) {
        echo '<td>';

        $number = array_shift($mine_copy);
        switch ($number) {
            case MINESWEEPER_FLAGGED:
            case MINESWEEPER_FLAGGED_MINE:
                if (!empty($lost) || !empty($won)) {
                    if ($number === MINESWEEPER_FLAGGED_MINE) {
                        echo '<a>*</a>';
                    }
                    else {
                        echo '<a>.</a>';
                    }
                }
                else {
                    echo '<a href=# onmousedown="return flag(',
                         ($x - 1) * MINEGRID_WIDTH + $y,
                         ',event)" oncontextmenu="return false">?</a>';
                }
                break;
            case ACTIVATED_MINE:
                echo '<a>:(</a>';
                break;
            case MINESWEEPER_MINE:
            case MINESWEEPER_NOT_EXPLORED:
                // oncontextmenu causes the menu to disappear in
                // Firefox, IE and Chrome

                // In case of Opera, modifying location causes menu
                // to not appear.

                if (!empty($lost)) {
                    if ($number === MINESWEEPER_MINE) {
                        echo '<a>*</a>';
                    }
                    else {
                        echo '<a>.</a>';
                    }
                }
                elseif (!empty($won)) {
                    echo '<a>*</a>';
                }
                else {
                    echo '<a href="?explore=',
                         ($x - 1) * MINEGRID_WIDTH + $y,
                         '" onmousedown="return flag(',
                         ($x - 1) * MINEGRID_WIDTH + $y,
                         ',event)" oncontextmenu="return false">.</a>';
                }
                break;
            case 0:
                echo '<a></a>';
                break;
            default:
                echo '<a>', $number, '</a>';
                break;
        }
    }
}
?>
</table>
<?php
if (!empty($lost)) {
    unset($_SESSION['minesweeper']);
    echo '<p>You lost :(. <a href="?">Reboot?</a>';
}
elseif (!empty($won)) {
    unset($_SESSION['minesweeper']);
    echo '<p>Congratulations. You won :).';
}
```



## PicoLisp


```PicoLisp
# NIL    Hidden: Empty field
# T      Hidden: Mine
# 0-8    Marked: Empty field
# ?      Marked: Mine

(de minesweeper (DX DY Density)
   (default Density 20)
   (setq *Field (make (do DY (link (need DX)))))
   (use (X Y)
      (do (prinl "Number of mines: " (*/ DX DY Density 100))
         (while
            (get *Field
               (setq Y (rand 1 DY))
               (setq X (rand 1 DX)) ) )
         (set (nth *Field Y X) T) ) )
   (showMines) )

(de showMines ()
   (for L *Field
      (for F L
         (prin (if (flg? F) "." F)) )
      (prinl) ) )

(de *NeighborX -1  0 +1 -1  +1 -1  0 +1)
(de *NeighborY -1 -1 -1  0   0 +1 +1 +1)

(de c (X Y)
   (if (=T (get *Field Y X))
      "KLABOOM!! You hit a mine."
      (let Visit NIL
         (recur (X Y)
            (when
               (=0
                  (set (nth *Field Y X)
                     (cnt
                        '((DX DY)
                           (=T (get *Field (+ Y DY) (+ X DX))) )
                        *NeighborX
                        *NeighborY ) ) )
               (mapc
                  '((DX DY)
                     (and
                        (get *Field (inc 'DY Y))
                        (nth @ (inc 'DX X))
                        (not (member (cons DX DY) Visit))
                        (push 'Visit (cons DX DY))
                        (recurse DX DY) ) )
                  *NeighborX
                  *NeighborY ) ) ) )
      (showMines) ) )

(de m (X Y)
   (set (nth *Field Y X) '?)
   (showMines)
   (unless (fish =T *Field)
      "Congratulations! You won!!" ) )
```

Output:

```txt
: (minesweeper 6 4)
Number of mines: 5
......
......
......
......
-> NIL

: (c 6 4)
......
...122
...100
...100
-> NIL

# ... omitted ...

: (c 1 4)
.201..
.20122
121100
01.100
-> NIL

# ... omitted ...

: (m 1 1)
?201..
.20122
121100
01.100
-> NIL

# ... omitted ...

: (m 3 4)
?201??
?20122
121100
01?100
-> "Congratulations! You won!!"
```



## Prolog

Works with SWI-Prolog


```Prolog
:- use_module(library(clpfd)).

% Play - run the minesweeper game with a specified width and height
play(W,H) :-
	format('
Welcome to prolog minesweeper!
: o X Y  exposes a cell of the grid
: m X Y  marks bombs

Any else to quit.
'),

	make_grid(W, H, Grid),
	!,
	play(Grid),
	!.

play(Grid) :- % win condition is true
	map_grid(won, Grid),
	map_grid(print_cell, Grid),
	writeln('you won!').

play(Grid) :- % lose condition is true
	\+ map_grid(still_playing, Grid),
	map_grid(print_cell, Grid),
	writeln('you hit a bomb!').

play(Grid) :- % stil playing
	map_grid(print_cell, Grid),
	parse_input(Op, X, Y),
	do_op(Op, p(X,Y), Grid, Grid2),
	!,
	play(Grid2).

/* Create a new Grid
 *
 * The grid is created initially as a flat list, and after everything
 * has been populated is converted into a 2 dimensional array.
 */
make_grid(W, H, grid(W,H,MappedCells)) :-
	% create a flat list
	Len is W * H,
	length(Cells, Len),

	% create a list of bombs that is 20% of the grid list
	NBombs is W * H / 5,
	floor(NBombs, NBint),
	format('There are ~w bombs on the grid~n', NBint),
	length(AllBombs, NBint),
	maplist(=('?'), AllBombs),
	% add the bombs to the start of the grid list
	map_bombs_to_cells(Cells, AllBombs, NewC),

	% randomise and convert to a 2D array
	random_permutation(NewC, RCells),
	convert_col(RCells, W, H, CreatedCells),

	% populate the hidden part of the grid with number of bombs next to each cell
	map_grid(adj_bomb(grid(W,H,CreatedCells)), grid(W,H,CreatedCells), grid(W,H,MappedCells)).

% puts the bombs at the start of the flat list before shuffling.
map_bombs_to_cells(C, [], C).
map_bombs_to_cells([_|Ct], [B|Bt], [B|R]) :- map_bombs_to_cells(Ct, Bt, R).

convert_row(T, 0, [], T).
convert_row([H|T], W, [H|R], Rem) :-
	dif(W, 0), succ(W1, W),
	convert_row(T, W1, R, Rem).

convert_col([], _, 0, []).
convert_col(C, W, H, [Row|MoreCells]) :- dif(H, 0), succ(H1, H),
	convert_row(C, W, Row, Rem),
	convert_col(Rem, W, H1, MoreCells).

% determine the number of bombs next to a cell (use mapgrid)
adj_bomb(_, _, _, C, cell('.',C)) :- C =@= '?'.
adj_bomb(Grid, p(X,Y), D, Cell, cell('.',NBombs)) :-
	dif(Cell, '?'),
	findall(p(Ax,Ay), (
		adj(p(X,Y), D, p(Ax,Ay)),
		indomain(Ax), indomain(Ay),
		grid_val_xy(Grid, p(Ax,Ay), Val),
		Val =@= '?'
	), Bombs),
	length(Bombs, NBombs).

% Print the grid (use mapgrid)
print_cell(p(X,_), dim(X,_), cell(C,A), cell(C,A)) :- format("~w~n", C).
print_cell(p(X,_), dim(W,_), cell(C,A), cell(C,A)) :- dif(X,W), format("~w ", C).

% determine if we have lost yet or not (use mapgrid).
still_playing(_,_,cell(A,_),_) :- A \= '*'.

% determine if we have won yet or not (use mapgrid).
won(_,_,cell(N,N),_) :- integer(N).
won(_,_,cell('?','?'),_).

% Operate on all cells in a grid, this is a meta predicate that is
% applied several times throughout the code
map_grid(Goal, G) :- map_grid(Goal, G, G).

map_grid(Goal, grid(W,H,Cells), grid(W,H,OutCells)) :-
	map_grid_col(Cells, 1, dim(W,H), Goal, OutCells).

map_grid_col([], _, _, _, []).
map_grid_col([H|T], Y, D, Goal, [NRow|NCol]) :-
	map_grid_row(H, p(1, Y), D, Goal, NRow),
	succ(Y, Y1),
	map_grid_col(T, Y1, D, Goal, NCol).

map_grid_row([], _, _, _, []).
map_grid_row([H|T], p(X, Y), D, Goal, [Cell|R]) :-
	call(Goal, p(X, Y), D, H, Cell),
	succ(X, X1),
	map_grid_row(T, p(X1, Y), D, Goal, R).

% Get a value from the grid by X Y
grid_val_xy(grid(_,_,Cells), p(X,Y), Val) :-
	nth1(Y, Cells, Row),
	nth1(X, Row, Val).

% Set a value on the grid by X Y
grid_set_xy(grid(W,H,Cells), p(X,Y), Val, grid(W,H,NewCells)) :- grid_set_col(Cells, X, Y, Val, NewCells).
grid_set_col([H|T], X, 1, Val, [Row|T]) :- grid_set_row(H, X, Val, Row).
grid_set_col([H|T], X, Y, Val, [H|New]) :- dif(Y, 0), succ(Y1, Y), grid_set_col(T, X, Y1, Val, New).
grid_set_row([_|T], 1, Val, [Val|T]).
grid_set_row([H|T], X, Val, [H|New]) :- dif(X, 0), succ(X1, X), grid_set_row(T, X1, Val, New).

% All coordinates adjacent to an x,y position
adj(p(X,Y), dim(W,H), p(Ax,Ay)) :-

	dif(p(X,Y),p(Ax,Ay)),

	% adjacent X
	Ax in 1..W,
	Xmin #= X-1, Xmax #= X+1,
	Ax in Xmin..Xmax,

	% adjacent Y
	Ay in 1..H,
	Ymin #= Y-1, Ymax #= Y+1,
	Ay in Ymin..Ymax.

% get user operation from input
parse_input(Op, X, Y) :-
	read_line_to_codes(user_input, In),
	maplist(char_code, InChars, In),
	phrase(mine_op(Op, X, Y), InChars, []).

mine_op(mark, X, Y) --> [m], [' '], coords(X, Y).
mine_op(open, X, Y) --> [o], [' '], coords(X, Y).
coords(Xi, Yi) --> number_(X), { number_chars(Xi, X) }, [' '], number_(Y), { number_chars(Yi, Y) }.

number_([D|T]) --> digit(D), number_(T).
number_([D]) --> digit(D).
digit(D) --> [D], { char_type(D, digit) }.


% Do mark operation
do_op(mark, P, G, Ng) :-
	grid_val_xy(G, P, cell(_,A)),
	grid_set_xy(G, P, cell('?',A), Ng).

% Do open operation, opening a bomb
do_op(open, P, G, Ng) :-
	grid_val_xy(G, P, cell(_,'?')),
	grid_set_xy(G, P, cell('*','?'), Ng).

% Do open operation, not a bomb
do_op(open, P, G, Ng) :-
	grid_val_xy(G, P, cell(_,A)),
	dif(A, '?'),
	grid_set_xy(G, P, cell(A,A), Ng1),
	expose_grid(P, Ng1, Ng).

% expose the grid by checking all the adjacent cells and operating
% appropriately
expose_grid(p(X,Y), grid(W,H,Cells), Ng2) :-
	findall(p(Ax,Ay), (
		    adj(p(X,Y), dim(W,H), p(Ax,Ay)),
		    indomain(Ax), indomain(Ay)
		), Coords),
	expose_grid_(Coords, grid(W,H,Cells), Ng2).

expose_grid_([], G, G).

expose_grid_([H|T], G, Ng) :-  % this cell has already been exposed, continue
	grid_val_xy(G, H, cell(A,B)),
	member(A, [B,'?']),
	expose_grid_(T, G, Ng).

expose_grid_([H|T], G, Ng) :- % ignore bombs
	grid_val_xy(G, H, cell(_,'?')),
	expose_grid_(T, G, Ng).

expose_grid_([H|T], G, Ng) :- % is an integer, expose and continue
	grid_val_xy(G, H, cell(_,N)),
	integer(N),
	N #> 0,
	grid_set_xy(G, H, cell(N,N), Ng1),
	expose_grid_(T, Ng1, Ng).

expose_grid_([H|T], G, Ng) :- % is a space, recurse
	grid_val_xy(G, H, cell('.',0)),
	grid_set_xy(G, H, cell(0,0), Ng1),
	expose_grid(H, Ng1, Ng2),
	expose_grid_(T, Ng2, Ng).
```



```txt

Welcome to prolog minesweeper!
: o X Y  exposes a cell of the grid
: m X Y  marks bombs

Any else to quit.
There are 4 bombs on the grid
. . . . . .
. . . . . .
. . . . . .
. . . . . .
|:
```




## PureBasic


```PureBasic
Structure cell
  isMine.i
  display.c ;character to displays for cell, one of these {'.', '?', ' ', #)
EndStructure

Global Dim grid.cell(0,0)
Global mineCount, minesMarked, isGameOver

Procedure makeGrid(n, m)
  Protected rm, x, y
  Dim grid.cell(n - 1, m - 1)
  mineCount = n * m * (Random(4) + 2) / 10
  If mineCount < 0: mineCount = 1: EndIf

  For x = 0 To n - 1
    For y = 0 To m - 1
      grid(x, y)\display = '.'
    Next
  Next

  rm = mineCount
  While rm
    x = Random(n - 1)
    y = Random(m - 1)
    If Not grid(x, y)\isMine
      rm - 1: grid(x, y)\isMine = #True
    EndIf
  Wend
  minesMarked = 0
  isGameOver = #False
EndProcedure

Procedure displayGrid(isEndOfGame = #False)
  #lMargin = 4
  Protected x, y, display.s
  If Not isEndOfGame
    PrintN("Grid has " + Str(mineCount) + " mines, " + Str(minesMarked) + " mines marked.")
  EndIf
  PrintN(Space(#lMargin + 1) + ReplaceString(Space(ArraySize(grid(), 1) + 1), " ", "-"))
  For y = 0 To ArraySize(grid(), 2)
    Print(RSet(Str(y + 1), #lMargin, " ") + ":")
    For x = 0 To ArraySize(grid(), 1)
      Print(Chr(grid(x,y)\display))
    Next
    PrintN("")
  Next
EndProcedure

Procedure endGame(msg.s)
  Protected ans.s
  isGameOver = #True
  PrintN(msg): Print("Another game (y/n)?"): ans = Input()
  If LCase(Left(Trim(ans),1)) = "y"
    makeGrid(6, 4)
  EndIf
EndProcedure

Procedure resign()
  Protected x, y, found
  For y = 0 To ArraySize(grid(), 2)
    For x = 0 To ArraySize(grid(), 1)
      With grid(x,y)
        If \isMine
          If \display = '?'
            \display = 'Y'
            found + 1
          ElseIf \display <> 'x'
            \display = 'N'
          EndIf
        EndIf
      EndWith
    Next
  Next
  displayGrid(#True)
  endGame("You found " + Str(found) + " out of " + Str(mineCount) + " mines.")
EndProcedure

Procedure usage()
  PrintN("h or ? - this help,")
  PrintN("c x y  - clear cell (x,y),")
  PrintN("m x y  - marks (toggles) cell (x,y),")
  PrintN("n      - start a new game,")
  PrintN("q      - quit/resign the game,")
  PrintN("where x is the (horizontal) column number and y is the (vertical) row number." + #CRLF$)
EndProcedure

Procedure markCell(x, y)
  If grid(x, y)\display = '?'
    minesMarked - 1: grid(x, y)\display = '.'
  ElseIf grid(x, y)\display = '.'
    minesMarked + 1: grid(x, y)\display = '?'
  EndIf
EndProcedure

Procedure countAdjMines(x, y)
  Protected count, i, j
  For j = y - 1 To y + 1
    If j >= 0 And j <= ArraySize(grid(), 2)
      For i = x - 1 To x + 1
        If i >= 0 And i <= ArraySize(grid(), 1)
          If grid(i, j)\isMine
            count + 1
          EndIf
        EndIf
      Next
    EndIf
  Next

  ProcedureReturn count
EndProcedure

Procedure clearCell(x, y)
  Protected count
  If x >= 0 And x <= ArraySize(grid(), 1) And y >= 0 And y <= ArraySize(grid(), 2)
    If grid(x, y)\display = '.'
      If Not grid(x,y)\isMine
        count = countAdjMines(x, y)
        If count
          grid(x, y)\display = Asc(Str(count))
        Else
          grid(x, y)\display = ' '
          clearCell(x + 1, y)
          clearCell(x + 1, y + 1)
          clearCell(x    , y + 1)
          clearCell(x - 1, y + 1)
          clearCell(x - 1, y)
          clearCell(x - 1, y - 1)
          clearCell(x    , y - 1)
          clearCell(x + 1, y - 1)
        EndIf
      Else
        grid(x, y)\display = 'x'
        PrintN("Kaboom!  You lost!")
        resign()
      EndIf
    EndIf
  EndIf
EndProcedure

Procedure testforwin()
  Protected x, y, isCleared
  If minesMarked = mineCount
    isCleared = #True
    For x = 0 To ArraySize(grid(), 1)
      For y = 0 To ArraySize(grid(), 2)
        If grid(x, y)\display = '.': isCleared = #False: EndIf
      Next
    Next
  EndIf
  If isCleared: endGame("You won!"): EndIf
EndProcedure

If OpenConsole()
  Define action.s
  usage()
  makeGrid(6, 4): displayGrid()
  Repeat
    PrintN(""): Print(">"):  action = Input()
    Select Asc(LCase(Left(action, 1)))
      Case 'h', '?'
        usage()
      Case 'n'
        makeGrid(6, 4): displayGrid()
      Case 'c'
        clearCell(Val(StringField(action, 2, " ")) - 1, Val(StringField(action, 3, " ")) - 1)
        If Not isGameOver: displayGrid(): Else: testforwin(): EndIf
      Case 'm'
        markCell(Val(StringField(action, 2, " ")) - 1, Val(StringField(action, 3, " ")) - 1)
        displayGrid()
        testforwin()
      Case 'q'
        resign()
    EndSelect
  Until isGameOver

  Print(#CRLF$ + #CRLF$ + "Press ENTER to exit"): Input()
  CloseConsole()
EndIf
```

Sample output:

```txt
h or ? - this help,
c x y  - clear cell (x,y),
m x y  - marks (toggles) cell (x,y),
n      - start a new game,
q      - quit/resign the game,
where x is the (horizontal) column number and y is the (vertical) row number.

Grid has 4 mines, 0 mines marked.
     ------
   1:......
   2:......
   3:......
   4:......

>c 1 1
Grid has 4 mines, 0 mines marked.
     ------
   1:  1...
   2:  1...
   3: 12...
   4: 1....

>m 3 4
Grid has 4 mines, 1 mines marked.
     ------
   1:  1...
   2:  1...
   3: 12...
   4: 1?...

>c 4 3
Grid has 4 mines, 1 mines marked.
     ------
   1:  1...
   2:  1...
   3: 123..
   4: 1?...

>c 4 2
Kaboom!  You lost!
     ------
   1:  1...
   2:  1x..
   3: 123NN
   4: 1Y...
You found 1 out of 4 mines.
Another game (y/n)?n
```



## Python


```python

'''
Minesweeper game.

    There is an n by m grid that has a random number of between 20% to 60%
    of randomly hidden mines that need to be found.

    Positions in the grid are modified by entering their coordinates
    where the first coordinate is horizontal in the grid and the second
    vertical. The top left of the grid is position 1,1; the bottom right is
    at n,m.

    * The total number of mines to be found is shown at the beginning of the
    game.
    * Each mine occupies a single grid point, and its position is initially
    unknown to the player
    * The grid is shown as a rectangle of characters between moves.
    * You are initially shown all grids as obscured, by a single dot '.'
    * You may mark what you think is the position of a mine which will show
    as a '?'
    * You can mark what you think is free space by entering its coordinates.
    :*  If the point is free space then it is cleared, as are any adjacent
    points that are also free space- this is repeated recursively for
    subsequent adjacent free points unless that point is marked as a mine
    or is a mine.
    ::*   Points marked as a mine show as a '?'.
    ::*   Other free points show as an integer count of the number of adjacent
    true mines in its immediate neighbourhood, or as a single space ' ' if the
    free point is not adjacent to any true mines.
    * Of course you loose if you try to clear space that starts on a mine.
    * You win when you have correctly identified all mines.


    When prompted you may:
        Toggle where you think a mine is at position x, y:
          m <x> <y>
        Clear the grid starting at position x, y (and print the result):
          c <x> <y>
        Print the grid so far:
          p
        Resign
          r
    Resigning will first show the grid with an 'N' for unfound true mines, a
    'Y' for found true mines and a '?' for where you marked clear space as a
    mine

'''


gridsize  = (6, 4)
minerange = (0.2, 0.6)


try:
    raw_input
except:
    raw_input = input

import random
from itertools import product
from pprint import pprint as pp


def gridandmines(gridsize=gridsize, minerange=minerange):
    xgrid, ygrid = gridsize
    minmines, maxmines = minerange
    minecount = xgrid * ygrid
    minecount = random.randint(int(minecount*minmines), int(minecount*maxmines))
    grid = set(product(range(xgrid), range(ygrid)))
    mines = set(random.sample(grid, minecount))
    show = {xy:'.' for xy in grid}
    return grid, mines, show

def printgrid(show, gridsize=gridsize):
    xgrid, ygrid = gridsize
    grid = '\n'.join(''.join(show[(x,y)] for x in range(xgrid))
                     for y in range(ygrid))
    print( grid )

def resign(showgrid, mines, markedmines):
    for m in mines:
        showgrid[m] = 'Y' if m in markedmines else 'N'

def clear(x,y, showgrid, grid, mines, markedmines):
    if showgrid[(x, y)] == '.':
        xychar = str(sum(1
                         for xx in (x-1, x, x+1)
                         for yy in (y-1, y, y+1)
                         if (xx, yy) in mines ))
        if xychar == '0': xychar = '.'
        showgrid[(x,y)] = xychar
        for xx in (x-1, x, x+1):
            for yy in (y-1, y, y+1):
                xxyy = (xx, yy)
                if ( xxyy != (x, y)
                     and xxyy in grid
                     and xxyy not in mines | markedmines ):
                    clear(xx, yy, showgrid, grid, mines, markedmines)

if __name__ == '__main__':
    grid, mines, showgrid = gridandmines()
    markedmines = set([])
    print( __doc__ )
    print( '\nThere are %i true mines of fixed position in the grid\n' % len(mines) )
    printgrid(showgrid)
    while markedmines != mines:
        inp = raw_input('m x y/c x y/p/r: ').strip().split()
        if inp:
            if inp[0] == 'm':
                x, y = [int(i)-1 for i in inp[1:3]]
                if (x,y) in markedmines:
                    markedmines.remove((x,y))
                    showgrid[(x,y)] = '.'
                else:
                    markedmines.add((x,y))
                    showgrid[(x,y)] = '?'
            elif inp[0] == 'p':
                printgrid(showgrid)
            elif inp[0] == 'c':
                x, y = [int(i)-1 for i in inp[1:3]]
                if (x,y) in mines | markedmines:
                    print( '\nKLABOOM!! You hit a mine.\n' )
                    resign(showgrid, mines, markedmines)
                    printgrid(showgrid)
                    break
                clear(x,y, showgrid, grid, mines, markedmines)
                printgrid(showgrid)
            elif inp[0] == 'r':
                print( '\nResigning!\n' )
                resign(showgrid, mines, markedmines)
                printgrid(showgrid)
                break

    print( '\nYou got %i and missed %i of the %i mines'
           % (len(mines.intersection(markedmines)),
              len(markedmines.difference(mines)),
              len(mines)) )
```


'''Sample output'''

```txt

Minesweeper game.

    There is an n by m grid that has a random number of between 20% to 60%
    of randomly hidden mines that need to be found.

    Positions in the grid are modified by entering their coordinates
    where the first coordinate is horizontal in the grid and the second
    vertical. The top left of the grid is position 1,1; the bottom right is
    at n,m.

    * The total number of mines to be found is shown at the beginning of the
    game.
    * Each mine occupies a single grid point, and its position is initially
    unknown to the player
    * The grid is shown as a rectangle of characters between moves.
    * You are initially shown all grids as obscured, by a single dot '.'
    * You may mark what you think is the position of a mine which will show
    as a '?'
    * You can mark what you think is free space by entering its coordinates.
    :*  If the point is free space then it is cleared, as are any adjacent
    points that are also free space- this is repeated recursively for
    subsequent adjacent free points unless that point is marked as a mine
    or is a mine.
    ::*   Points marked as a mine show as a '?'.
    ::*   Other free points show as an integer count of the number of adjacent
    true mines in its immediate neighbourhood, or as a single space ' ' if the
    free point is not adjacent to any true mines.
    * Of course you loose if you try to clear space that starts on a mine.
    * You win when you have correctly identified all mines.


    When prompted you may:
        Toggle where you think a mine is at position x, y:
          m <x> <y>
        Clear the grid starting at position x, y (and print the result):
          c <x> <y>
        Print the grid so far:
          p
        Resign
          r
    Resigning will first show the grid with an 'N' for unfound true mines, a
    'Y' for found true mines and a '?' for where you marked clear space as a
    mine



There are 13 true mines of fixed position in the grid

......
......
......
......
m x y/c x y/p/r: c 3 2
.2....
345...
..5...
.4....
m x y/c x y/p/r: m 1 1
m x y/c x y/p/r: m 1 4
m x y/c x y/p/r: p
?2....
345...
..5...
?4....
m x y/c x y/p/r: r

Resigning!

Y2NN.N
345N.N
NN5NN.
Y4NN..

You got 2 and missed 0 of the 13 mines
```



## Racket


```Racket

#lang racket
(require math/array)
;board uses arrays directly, but maintaining an abstraction is nice
(define (board-ref b row col) (array-ref b (vector row col)))
(define (board-rows b) (vector-ref (array-shape b) 0))
(define (board-cols b) (vector-ref (array-shape b) 1))
(define (on-board? b row col)
  (and (<= 0 row (sub1 (board-rows b)))
       (<= 0 col (sub1 (board-cols b)))))
(define (board->lists b) (array->list* b))
;run on adjacent board positions
(define-syntax (for-adj stx)
  (syntax-case stx ()
    [(_ b (r row) (c col) diag? body ...)
     (with-syntax ([is (if (syntax->datum #'diag?) #''(0 0 1 1 1 -1 -1 -1) #''(0 0 1 -1))]
                   [js (if (syntax->datum #'diag?) #''(1 -1 0 -1 1 0 -1 1) #''(1 -1 0 0))])
       #'(for ([i is] [j js])
           (let ([r (+ row i)]
                 [c (+ col j)])
             (when (on-board? b r c)
               body ...))))]))
;mark is either hidden, assume-mine, or clear
;n is int equal to # adj mines or -1 for mine
(struct pos ([mark #:mutable] n) #:transparent)
(define (mine? p) (= (pos-n p) -1))
(define (mine-count b) (apply + (array->list (array-map (λ (p) (if (mine? p) 1 0)) b))))
;hidden0? is needed because only spaces with no mines in them and no mines adjacent
;to them are cleared recursively
(define (hidden0? p)
  (and (symbol=? (pos-mark p) 'hidden)
       (zero? (pos-n p))))
(define (show-pos p)
  (match-let ([(pos m n) p])
    (case m
      [(hidden) "."]
      [(assume-mine) "?"]
      [(clear) (if (zero? n) " " (number->string n))]
      [else (error "illegal mark" m)])))
;put "|" around positions
(define (show-board b)
  (for ([row (board->lists b)])
    (displayln (format "|~a|" (string-join (map show-pos row) "|")))))

;winning = every position is either cleared or a hidden mine
(define (win? b)
  (for*/and ([r (range 0 (board-rows b))]
             [c (range 0 (board-cols b))])
    (let ([p (board-ref b r c)])
      (or (symbol=? (pos-mark p) 'clear)
          (mine? p)))))

(define (init-board rows cols)
  (let ([chance (+ (/ (random) 10) 0.1)]
        ;empty board
        [b (array->mutable-array (build-array (vector rows cols)
                                              (λ (x) (pos 'hidden 0))))])
    ;loop whole board
    (for* ([row (range 0 rows)]
           [col (range 0 cols)])
      (when (< (random) chance)
        ;put a mine
        (array-set! b (vector row col) (pos 'hidden -1))
        ;increment adjacent mine counts unless that adjacent position is a mine
        (for-adj b (r row) (c col) #t
                 (let ([p (board-ref b r c)])
                   (unless (mine? p)
                     (array-set! b (vector r c) (pos 'hidden (add1 (pos-n p)))))))))
    b))

;only clear position if it's not a mine
;only continue recursing when it's a hidden0?
(define (try-clear! p)
  (cond [(mine? p) #f]
        [(hidden0? p) (set-pos-mark! p 'clear) #t]
        [else (set-pos-mark! p 'clear) #f]))

;the following player move functions return boolean where #f = lose, #t = still going
;assuming can never directly lose ((void) == #t from the set!)
;make sure to not allow overwriting an already cleared position
(define (toggle-assume! b row col)
  (let ([p (board-ref b row col)])
    (set-pos-mark! p (case (pos-mark p)
                       [(assume-mine) 'hidden]
                       [(hidden) 'assume-mine]
                       [(clear) 'clear]
                       [else (error "invalid mark" (pos-mark p))]))))

;clearing loses when the chosen position is a mine
;void = #t as far as if works, so no need to return #t
(define (clear! b row col)
  (let ([p (board-ref b row col)])
    (and (not (mine? p))
         ;not a mine, so recursively check adjacents, and maintain list of visited positions
         ;to avoid infinite loops
         (let ([seen '()])
           ;clear the chosen position first, only continuing if it's a 0
           (when (try-clear! p)
             (let clear-adj ([row row] [col col])
               (for-adj b (r row) (c col) #f
                        ;make sure its not seen
                        (when (and (not (member (list r c) seen))
                                   (try-clear! (board-ref b r c)))
                          ;it was cleared, so loop after saving this position as being seen
                          (set! seen (cons (list r c) seen))
                          (clear-adj r c)))))))))

(define assume-string "a")
(define clear-string "c")
;validates input...returns either #f for an error or the move to execute
(define (parse-and-create-move! b s)
  (match (string-split s)
    [(list type row col)
     (let ([row (string->number row)]
           [col (string->number col)])
       (and (number? row)
            (number? col)
            (let ([row (sub1 row)]
                  [col (sub1 col)])
              (and (on-board? b row col)
                   (or (and (string=? type assume-string) (λ () (toggle-assume! b row col)))
                       (and (string=? type clear-string) (λ () (clear! b row col))))))))]
    [else #f]))
(define (run)
  (displayln (string-append "--- Enter one of:\n"
                            (format "--- \"~a <row> <col>\" to clear at (row,col), or~n" clear-string)
                            (format (string-append "--- \"~a <row> <col>\" to flag a possible mine "
                                                   "(or clear a flag) at (row,col).~n")
                                    assume-string)))
  (let ([b (init-board 4 6)])
    (displayln (format "There are ~a mines.~n" (mine-count b)))
    (let run ()
      (show-board b)
      (display "enter move: ")
      ;parse either failed or gave the procedure to execute
      (let ([proc? (parse-and-create-move! b (read-line))])
        ;was the parse successful?
        (if proc?
            ;then run it
            (if (proc?)
                ;didn't lose, so either we won or we're not done
                (if (win? b) (displayln "CLEAR!") (run))
                (displayln "BOOM!"))
            ;parse failed
            (run))))))

```



## Ruby


```Ruby
puts <<EOS
    Minesweeper game.

    There is an n by m grid that has a random number of between 20% to 60%
    of randomly hidden mines that need to be found.

    Positions in the grid are modified by entering their coordinates
    where the first coordinate is horizontal in the grid and the second
    vertical. The top left of the grid is position 1,1; the bottom right is
    at n,m.

    * The total number of mines to be found is shown at the beginning of the
    game.
    * Each mine occupies a single grid point, and its position is initially
    unknown to the player
    * The grid is shown as a rectangle of characters between moves.
    * You are initially shown all grids as obscured, by a single dot '.'
    * You may mark what you think is the position of a mine which will show
    as a '?'
    * You can mark what you think is free space by entering its coordinates.
    :*  If the point is free space then it is cleared, as are any adjacent
    points that are also free space- this is repeated recursively for
    subsequent adjacent free points unless that point is marked as a mine
    or is a mine.
    ::*   Points marked as a mine show as a '?'.
    ::*   Other free points show as an integer count of the number of adjacent
    true mines in its immediate neighbourhood, or as a single space ' ' if the
    free point is not adjacent to any true mines.
    * Of course you loose if you try to clear space that starts on a mine.
    * You win when you have correctly identified all mines.


    When prompted you may:
        Toggle where you think a mine is at position x, y:
          m <x> <y>
        Clear the grid starting at position x, y (and print the result):
          c <x> <y>
        Print the grid so far:
          p
        Quit
          q
    Resigning will first show the grid with an 'N' for unfound true mines, a
    'Y' for found true mines and a '?' for where you marked clear space as a
    mine
EOS

WIDTH, HEIGHT = 6, 4
PCT = 0.15
NUM_MINES = (WIDTH * HEIGHT * PCT).round

def create_mines sx, sy
  arr = Array.new(WIDTH) { Array.new(HEIGHT, false) }
  NUM_MINES.times do
    x, y = rand(WIDTH), rand(HEIGHT)
    # place it if it isn't at (sx, sy) and we haven't already placed a mine
    redo if arr[x][y] or (x == sx and y == sy)
    arr[x][y] = true
  end
  arr
end

def num_marks
  $screen.inject(0) { |sum, row| sum + row.count("?") }
end

def show_grid revealed = false
  if revealed
    puts $mines.transpose.map { |row| row.map { |cell| cell ? "*" : " " }.join(" ") }
  else
    puts "Grid has #{NUM_MINES} mines, #{num_marks} marked."
    puts $screen.transpose.map{ |row| row.join(" ") }
  end
end

SURROUND = [-1,0,1].product([-1,0,1]) - [[0,0]]     # surround 8
def surrounding x, y
  # apply the passed block to each spot around (x, y)
  SURROUND.each do |dx, dy|
    # don't check if we're out of bounds, or at (0,0)
    yield(x+dx, y+dy) if (0...WIDTH).cover?(x+dx) and (0...HEIGHT).cover?(y+dy)
  end
end

def clear_space x, y
  return unless $screen[x][y] == "."
  # check nearby spaces
  count = 0
  surrounding(x, y) { |px, py| count += 1 if $mines[px][py] }
  if count == 0
    $screen[x][y] = " "
    surrounding(x, y) { |px, py| clear_space px, py }
  else
    $screen[x][y] = count.to_s
  end
end

def victory?
  return false if $mines.nil?  # first one, don't need to check
  return false if num_marks != NUM_MINES
  mines_left = NUM_MINES
  WIDTH.times do |x|
    HEIGHT.times do |y|
      mines_left -= 1 if $mines[x][y] and $screen[x][y] == "?"
    end
  end

  mines_left == 0
end

def check_input x, y
  x, y = x.to_i - 1, y.to_i - 1
  [x, y] if (0...WIDTH).cover?(x) and (0...HEIGHT).cover?(y)
end

$mines = nil
$screen = Array.new(WIDTH) { Array.new(HEIGHT, ".") }

puts "Welcome to Minesweeper!"
show_grid

loop do
  print "> "
  action = gets.chomp.downcase

  case action
  when "quit", "exit", "x", "q"
    puts "Bye!"
    break
  when /^m (\d+) (\d+)$/
    # mark this cell
    x, y = check_input $1, $2
    next unless x
    if $screen[x][y] == "."
      # mark it
      $screen[x][y] = "?"
      if victory?
        show_grid
        puts "You win!"
        break
      end
    elsif $screen[x][y] == "?"
      # unmark it
      $screen[x][y] = "."
    end
    show_grid
  when /^c (\d+) (\d+)$/
    x, y = check_input $1, $2
    next unless x
    $mines ||= create_mines(x, y)
    if $mines[x][y]
      puts "You hit a mine!"
      show_grid true
      break
    else
      clear_space x, y
      show_grid
      if victory?
        puts "You win!"
        break
      end
    end
  when "p"
    show_grid
  end
end
```



## Rust


```Rust
extern crate rand;

use std::io;
use std::io::Write;

fn main() {

    use minesweeper::{MineSweeper, GameStatus};

    let mut width = 6;
    let mut height = 4;
    let mut mine_perctg = 10;
    let mut game = MineSweeper::new(width, height, mine_perctg);

    loop {
        let mut command = String::new();

        println!(
            "\n\
             M I N E S W E E P E R\n\
             \n\
             Commands: \n\
             line col            - reveal line,col \n\
             m line col          - mark   line,col \n\
             q                   - quit\n\
             n                   - new game\n\
             n width height perc - new game size and mine percentage\n"
        );

        game.print();
        print!("> ");
        io::stdout().flush().unwrap();
        while let Ok(_) = io::stdin().read_line(&mut command) {
            let mut command_ok = false;
            {
                let values: Vec<&str> = command.trim().split(' ').collect();
                if values.len() == 1 {
                    if values[0] == "q" {
                        println!("Goodbye");
                        return;
                    } else if values[0] == "n" {
                        println!("New game");
                        game = MineSweeper::new(width, height, mine_perctg);
                        command_ok = true;
                    }
                } else if values.len() == 2 {
                    if let (Ok(x), Ok(y)) = (
                        values[0].parse::<usize>(),
                        values[1].parse::<usize>(),
                    )
                    {
                        game.play(x - 1, y - 1);

                        match game.game_status {
                            GameStatus::Won => println!("You won!"),
                            GameStatus::Lost => println!("You lost!"),
                            _ => (),
                        }
                        command_ok = true;
                    }
                } else if values.len() == 3 {
                    if values[0] == "m" {
                        if let (Ok(x), Ok(y)) = (
                            values[1].parse::<usize>(),
                            values[2].parse::<usize>(),
                        )
                        {
                            game.mark(x - 1, y - 1);
                            command_ok = true;
                        }
                    }
                } else if values.len() == 4 {
                    if values[0] == "n" {
                        if let (Ok(new_width), Ok(new_height), Ok(new_mines_perctg)) =
                            (
                                values[1].parse::<usize>(),
                                values[2].parse::<usize>(),
                                values[3].parse::<usize>(),
                            )
                        {
                            width = new_width;
                            height = new_height;
                            mine_perctg = new_mines_perctg;
                            game = MineSweeper::new(width, height, mine_perctg);
                            command_ok = true;
                        }
                    }
                }
            }

            if command_ok {
                game.print();
            } else {
                println!("Invalid command");
            }

            print!("> ");
            io::stdout().flush().unwrap();
            command.clear();
        }
    }
}

pub mod minesweeper {

    pub struct MineSweeper {
        cell: [[Cell; 100]; 100],
        pub game_status: GameStatus,
        mines: usize,
        width: usize,
        height: usize,
        revealed_count: usize,
    }

    #[derive(Copy, Clone)]
    struct Cell {
        content: CellContent,
        mark: Mark,
        revealed: bool,
    }

    #[derive(Copy, Clone)]
    enum CellContent {
        Empty,
        Mine,
        MineNeighbour { count: u8 },
    }

    #[derive(Copy, Clone)]
    enum Mark {
        None,
        Mine,
    }

    pub enum GameStatus {
        InGame,
        Won,
        Lost,
    }

    extern crate rand;

    use std::cmp::max;
    use std::cmp::min;
    use self::rand::Rng;
    use self::CellContent::*;
    use self::GameStatus::*;

    impl MineSweeper {
        pub fn new(width: usize, height: usize, percentage_of_mines: usize) -> MineSweeper {
            let mut game = MineSweeper {
                cell: [[Cell {
                    content: Empty,
                    mark: Mark::None,
                    revealed: false,
                }; 100]; 100],
                game_status: InGame,
                mines: (width * height * percentage_of_mines) / 100,
                width: width,
                height: height,
                revealed_count: 0,
            };
            game.put_mines();
            game.calc_neighbours();
            game
        }

        pub fn play(&mut self, x: usize, y: usize) {
            match self.game_status {
                InGame => {
                    if !self.cell[x][y].revealed {
                        match self.cell[x][y].content {
                            Mine => {
                                self.cell[x][y].revealed = true;
                                self.revealed_count += 1;
                                self.game_status = Lost;
                            }
                            Empty => {
                                self.flood_fill_reveal(x, y);
                                if self.revealed_count + self.mines == self.width * self.height {
                                    self.game_status = Won;
                                }
                            }
                            MineNeighbour { .. } => {
                                self.cell[x][y].revealed = true;
                                self.revealed_count += 1;
                                if self.revealed_count + self.mines == self.width * self.height {
                                    self.game_status = Won;
                                }
                            }
                        }
                    }
                }
                _ => println!("Game has ended"),
            }
        }

        pub fn mark(&mut self, x: usize, y: usize) {
            self.cell[x][y].mark = match self.cell[x][y].mark {
                Mark::None => Mark::Mine,
                Mark::Mine => Mark::None,
            }
        }

        pub fn print(&self) {
            print!("┌");
            for _ in 0..self.width {
                print!("─");
            }
            println!("┐");
            for y in 0..self.height {
                print!("│");
                for x in 0..self.width {
                    self.cell[x][y].print();
                }
                println!("│");
            }
            print!("└");
            for _ in 0..self.width {
                print!("─");
            }
            println!("┘");
        }

        fn put_mines(&mut self) {
            let mut rng = rand::thread_rng();
            for _ in 0..self.mines {
                while let (x, y, true) = (
                    rng.gen::<usize>() % self.width,
                    rng.gen::<usize>() % self.height,
                    true,
                )
                {
                    match self.cell[x][y].content {
                        Mine => continue,
                        _ => {
                            self.cell[x][y].content = Mine;
                            break;
                        }
                    }
                }
            }
        }

        fn calc_neighbours(&mut self) {
            for x in 0..self.width {
                for y in 0..self.height {
                    if !self.cell[x][y].is_bomb() {
                        let mut adjacent_bombs = 0;

                        for i in max(x as isize - 1, 0) as usize..min(x + 2, self.width) {
                            for j in max(y as isize - 1, 0) as usize..min(y + 2, self.height) {
                                adjacent_bombs += if self.cell[i][j].is_bomb() { 1 } else { 0 };
                            }
                        }

                        if adjacent_bombs == 0 {
                            self.cell[x][y].content = Empty;
                        } else {
                            self.cell[x][y].content = MineNeighbour { count: adjacent_bombs };
                        }
                    }
                }
            }
        }

        fn flood_fill_reveal(&mut self, x: usize, y: usize) {
            let mut stack = Vec::<(usize, usize)>::new();
            stack.push((x, y));

            while let Some((i, j)) = stack.pop() {
                if self.cell[i][j].revealed {
                    continue;
                }
                self.cell[i][j].revealed = true;
                self.revealed_count += 1;
                if let Empty = self.cell[i][j].content {
                    for m in max(i as isize - 1, 0) as usize..min(i + 2, self.width) {
                        for n in max(j as isize - 1, 0) as usize..min(j + 2, self.height) {
                            if !self.cell[m][n].is_bomb() && !self.cell[m][n].revealed {
                                stack.push((m, n));
                            }
                        }
                    }
                }
            }
        }
    }

    impl Cell {
        pub fn print(&self) {
            print!(
                "{}",
                if self.revealed {
                    match self.content {
                        Empty => ' ',
                        Mine => '*',
                        MineNeighbour { count } => char::from(count + b'0'),
                    }
                } else {
                    match self.mark {
                        Mark::Mine => '?',
                        Mark::None => '.',
                    }
                }
            );
        }

        pub fn is_bomb(&self) -> bool {
            match self.content {
                Mine => true,
                _ => false,
            }
        }
    }
}
```


'''Sample output'''


```txt

M I N E S W E E P E R

Commands:
line col            - reveal line,col
m line col          - mark   line,col
q                   - quit
n                   - new game
n width height perc - new game size and mine percentage

┌──────┐
│......│
│......│
│......│
│......│
└──────┘
> 5 3
┌──────┐
│.1    │
│.211  │
│...1  │
│...1  │
└──────┘
> 1 4
┌──────┐
│.1    │
│1211  │
│ 1.1  │
│ 1.1  │
└──────┘
> 3 4
You won!
┌──────┐
│.1    │
│1211  │
│ 1.1  │
│ 111  │
└──────┘
> q
Goodbye

```



## Tcl


```tcl
package require Tcl 8.5
fconfigure stdout -buffering none

# Set up the grid and fill it with some mines
proc makeGrid {n m} {
    global grid mine
    unset -nocomplain grid mine
    set grid(size) [list $n $m]
    set grid(shown) 0
    set grid(marked) 0
    for {set j 1} {$j <= $m} {incr j} {
	for {set i 1} {$i <= $n} {incr i} {
	    set grid($i,$j) .
	}
    }
    set squares [expr {$m * $n}]
    set mine(count) [expr {int((rand()*0.4+0.2) * $squares)}]
    for {set count 0} {$count < $mine(count)} {incr count} {
	while 1 {
	    set i [expr {1+int(rand()*$n)}]
	    set j [expr {1+int(rand()*$m)}]
	    if {![info exist mine($i,$j)]} {
		set mine($i,$j) x
		break
	    }
	}
    }
    return $mine(count)
}

# Print out the grid
proc displayGrid {} {
    global grid
    lassign $grid(size) n m
    for {set j 1} {$j <= $m} {incr j} {
	set row "\t"
	for {set i 1} {$i <= $n} {incr i} {
	    append row $grid($i,$j)
	}
	puts $row
    }
}

# Toggle the possible-mine flag on a cell
proc markCell {x y} {
    global grid
    if {![info exist grid($x,$y)]} return
    if {$grid($x,$y) eq "."} {
	set grid($x,$y) "?"
	incr grid(marked)
    } elseif {$grid($x,$y) eq "?"} {
	set grid($x,$y) "."
	incr grid(marked) -1
    }
}

# Helper procedure that iterates over the 9 squares centered on a location
proc foreachAround {x y xv yv script} {
    global grid
    upvar 1 $xv i $yv j
    foreach i [list [expr {$x-1}] $x [expr {$x+1}]] {
	foreach j [list [expr {$y-1}] $y [expr {$y+1}]] {
	    if {[info exist grid($i,$j)]} {uplevel 1 $script}
	}
    }
}

# Reveal a cell; returns if it was a mine
proc clearCell {x y} {
    global grid mine
    if {![info exist grid($x,$y)] || $grid($x,$y) ne "."} {
	return 0; # Do nothing...
    }
    if {[info exist mine($x,$y)]} {
	set grid($x,$y) "!"
	revealGrid
	return 1; # Lose...
    }
    set surround 0
    foreachAround $x $y i j {incr surround [info exist mine($i,$j)]}
    incr grid(shown)
    if {$surround == 0} {
	set grid($x,$y) " "
	foreachAround $x $y i j {clearCell $i $j}
    } else {
	set grid($x,$y) $surround
    }
    return 0
}

# Check the winning condition
proc won? {} {
    global grid mine
    lassign $grid(size) n m
    expr {$grid(shown) + $mine(count) == $n * $m}
}

# Update the grid to show mine locations (marked or otherwise)
proc revealGrid {} {
    global grid mine
    lassign $grid(size) n m
    for {set j 1} {$j <= $m} {incr j} {
	for {set i 1} {$i <= $n} {incr i} {
	    if {![info exist mine($i,$j)]} continue
	    if {$grid($i,$j) eq "."} {
		set grid($i,$j) "x"
	    } elseif {$grid($i,$j) eq "?"} {
		set grid($i,$j) "X"
	    }
	}
    }
}

# The main game loop
proc play {n m} {
    set m [makeGrid $n $m]
    puts "There are $m true mines of fixed position in the grid\n"
    displayGrid
    while 1 {
	puts -nonewline "m x y/c x y/p/r: "
	if {[gets stdin line] < 0} break; # check for eof too!
	switch -nocase -regexp [string trim $line] {
	    {^m\s+\d+\s+\d+$} {
		markCell [lindex $line 1] [lindex $line 2]
	    }
	    {^c\s+\d+\s+\d+$} {
		if {[clearCell [lindex $line 1] [lindex $line 2]]} {
		    puts "KABOOM!"
		    displayGrid
		    break
		} elseif {[won?]} {
		    puts "You win!"
		    displayGrid
		    break
		}
	    }
	    {^p$} {
		displayGrid
	    }
	    {^r$} {
		puts "Resigning..."
		revealGrid
		displayGrid
		break
	    }
	}
    }
}

play 6 4
```

Sample output:

```txt

There are 6 true mines of fixed position in the grid

	......
	......
	......
	......
m x y/c x y/p/r: c 1 1
m x y/c x y/p/r: c 2 2
m x y/c x y/p/r: c 3 3
m x y/c x y/p/r: c 4 4
m x y/c x y/p/r: p
	1.....
	.1....
	..1...
	...2..
m x y/c x y/p/r: c 2 3
m x y/c x y/p/r: p
	1.....
	112...
	  1...
	  12..
m x y/c x y/p/r: m 2 1
m x y/c x y/p/r: m 4 3
m x y/c x y/p/r: p
	1?....
	112...
	  1?..
	  12..
m x y/c x y/p/r: c 3 1
m x y/c x y/p/r: c 4 1
m x y/c x y/p/r: c 4 2
m x y/c x y/p/r: c 5 4
m x y/c x y/p/r: m 5 3
m x y/c x y/p/r: c 6 1
m x y/c x y/p/r: p
	1?11.2
	1123..
	  1??.
	  123.
m x y/c x y/p/r: c 6 3
KABOOM!
	1X11.2
	1123xx
	  1XX!
	  123.

```




## VBA

VBA for Excel

```txt
In your Vbaproject, insert :
- 1 Module
- 1 Class Module (Name it cMinesweeper)
```


### Code Module :


```vb

Option Explicit

Public vTime As Single
Public PlaysCount As Long

Sub Main_MineSweeper()
Dim Userf As New cMinesweeper
'Arguments :
    'First arg is level : 0 = easy, 1 = middle, 2 = difficult
    'Second arg is Cheat Mode : True if you want to cheat...
    Userf.Show 0, True
End Sub

```



### Code Class Module


```txt
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
Need to reference the following object libraries (From the Tools menu, choose References)
    Microsoft Forms 2.0 Object Library
    Microsoft Visual Basic For Applications Extensibility 5.3
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
```



```vb

Option Explicit

Public myForm As Object
Public Fram As MSForms.Frame
Public Dico As Object
Public DicoParent As Object
Public TypeObjet As String
Public Mine As Boolean
Public boolFind As Boolean
Private strName As String
Private cNeighbours() As cMinesweeper
Public WithEvents myButton As MSForms.CommandButton
Private Const WIDTH_BUTT As Byte = 18
Private Const MIN_OF_LINES As Byte = 7
Private Const MAX_OF_LINES As Byte = 30 - MIN_OF_LINES
Private Const MIN_COL As Byte = 7
Private Const MAX_COL As Byte = 40 - MIN_COL
Private Const POURCENT_SIMPLE As Byte = 10
Private Const POURCENT_MEDIUM As Byte = 2 * POURCENT_SIMPLE
Private Const POURCENT_HARD As Byte = 3 * POURCENT_SIMPLE
Private Const COLOR_MINE As Long = &H188B0
Private Const COLOR_BOUTON As Long = &H8000000F
Private Const COLOR_MINE_POSSIBLE As Long = &H80FF&
Private Const COLOR_MINE_PROB As Long = &H8080FF

Property Get Neighbours() As cMinesweeper()
   Neighbours = cNeighbours
End Property

Property Let Neighbours(ByRef nouvNeighbours() As cMinesweeper)
   cNeighbours = nouvNeighbours
End Property

Private Sub Class_Initialize()
    Set Dico = CreateObject("Scripting.dictionary")
End Sub

Public Sub Show(ByRef Difficult As Long, Optional CheatMode As Boolean = False)
    On Error GoTo ErrorParametresMacros
    With ThisWorkbook.VBProject: End With

    Dim Lin As Long, Col As Long, NbLines As Long, NbColumns As Long
    Dim NbMines As Long, MineAdress() As String, CptMine As Long
    Randomize Timer
    NbLines = Int(MAX_OF_LINES * Rnd) + MIN_OF_LINES
    NbColumns = Int(MAX_COL * Rnd) + MIN_COL
    Select Case Difficult
        Case 0: Difficult = POURCENT_SIMPLE
        Case 1: Difficult = POURCENT_MEDIUM
        Case 2: Difficult = POURCENT_HARD
    End Select
    PlaysCount = 0
    NbMines = (NbLines * NbColumns) * Difficult \ 100
    ReDim MineAdress(NbMines)
    For CptMine = 1 To NbMines
        MineAdress(CptMine) = Int(NbColumns * Rnd) + 1 & "-" & Int(NbLines * Rnd) + 1
    Next
    Call Create_Usf("Minesweeper", (NbColumns * WIDTH_BUTT) + 5, (NbLines * WIDTH_BUTT) + 22)
    Call New_Frame("Fram1", "", NbColumns * WIDTH_BUTT, NbLines * WIDTH_BUTT)
    For Lin = 1 To NbLines
        For Col = 1 To NbColumns
            Call Dico("Fram1").New_Button(Col & "-" & Lin, "", WIDTH_BUTT * (Col - 1), WIDTH_BUTT * (Lin - 1), IsIn(Col & "-" & Lin, MineAdress), CheatMode)
            Set Dico("Fram1").Dico(Col & "-" & Lin).DicoParent = Dico("Fram1").Dico
        Next Col
    Next Lin
    MsgBox "Start With " & NbMines & " mines." & vbCrLf & "Good luck !"
    myForm.Show
    Exit Sub
ErrorParametresMacros:
    MsgBox "Programmatic Access to Visual Basic Project is not trusted. See it in Macro's security!"
End Sub

Private Sub Create_Usf(strTitle As String, dblWidth As Double, dblHeight As Double)
    TypeObjet = "UserForm"
    Set myForm = ThisWorkbook.VBProject.VBComponents.Add(3)
    strName = myForm.Name
    VBA.UserForms.Add (strName)
    Set myForm = UserForms(UserForms.Count - 1)
    With myForm
        .Caption = strTitle
        .Width = dblWidth
        .Height = dblHeight
    End With
End Sub

Public Sub New_Frame(myStringName As String, strTitle As String, dblWidth As Double, dblHeight As Double)
    If Dico.Exists(myStringName) = True Then Exit Sub
    Dim myClass As New cMinesweeper
    Select Case TypeObjet
        Case "UserForm": Set myClass.Fram = myForm.Controls.Add("forms.frame.1")
        Case "Frame": Set myClass.Fram = Fram.Controls.Add("forms.frm.1")
    End Select
    myClass.TypeObjet = "Frame"
    Set myClass.myForm = myForm
    With myClass.Fram
        .Name = myStringName
        .Caption = strTitle
        .Move 0, 0, dblWidth, dblHeight
    End With
    Dico.Add myStringName, myClass
    Set myClass = Nothing
End Sub

Public Sub New_Button(myStringName As String, strTitle As String, dblLeft As Double, dblTop As Double, boolMine As Boolean, Optional CheatMode As Boolean)
    If Dico.Exists(myStringName) = True Then Exit Sub
    Dim myClass As New cMinesweeper
    Select Case TypeObjet
        Case "UserForm": Set myClass.myButton = myForm.Controls.Add("forms.CommandButton.1")
        Case "Frame": Set myClass.myButton = Fram.Controls.Add("forms.CommandButton.1")
    End Select
    Set myClass.myForm = myForm
    myClass.Mine = boolMine
    With myClass.myButton
        .Name = myStringName
        .Caption = strTitle
        .Move dblLeft, dblTop, WIDTH_BUTT, WIDTH_BUTT
        If CheatMode Then
            If boolMine Then .BackColor = COLOR_MINE Else .BackColor = COLOR_BOUTON
        Else
            .BackColor = COLOR_BOUTON
        End If
    End With
    Dico.Add myStringName, myClass
    Set myClass = Nothing
End Sub

Private Function IsIn(strAddress As String, Tb) As Boolean
    Dim i As Long
    For i = 0 To UBound(Tb)
        If Tb(i) = strAddress Then IsIn = True: Exit Function
    Next i
End Function

Private Sub myButton_MouseDown(ByVal Button As Integer, ByVal Shift As Integer, ByVal x As Single, ByVal y As Single)
    If Button = XlMouseButton.xlSecondaryButton Then
        Select Case myButton.Caption
            Case "": myButton.Caption = "!": myButton.BackColor = COLOR_MINE_PROB
            Case "!": myButton.Caption = "?": myButton.BackColor = COLOR_MINE_POSSIBLE
            Case "?": myButton.Caption = "": myButton.BackColor = COLOR_BOUTON
            Case Else:
        End Select
    ElseIf Button = XlMouseButton.xlPrimaryButton Then
        PlaysCount = PlaysCount + 1
        If PlaysCount = 1 Then vTime = Timer
        If DicoParent.Item(myButton.Name).Mine Then
            Call Show_Mines
            MsgBox "Game over!"
            myForm.Hide
        Else
            myButton.BackColor = COLOR_BOUTON
            Dim myClass As cMinesweeper
            Set myClass = DicoParent.Item(myButton.Name)
            Call Demine(myClass)
        End If
    End If
    If IsVictoriousGame Then
        Call Show_Mines
        MsgBox "You win." & vbCrLf & "in : " & PlaysCount & " clicks, and in : " & Timer - vTime & " seconds."
        Erase Neighbours
        myForm.Hide
    End If
End Sub

Private Sub Show_Mines()
Dim cle As Variant
    For Each cle In DicoParent.Keys
        If DicoParent.Item(cle).Mine Then DicoParent.Item(cle).myButton.BackColor = COLOR_MINE
    Next
End Sub

Private Sub Demine(Cl As cMinesweeper)
Dim NbMines As Integer
    NbMines = Count_Of_Mines(Cl.myButton.Name)
    If NbMines > 0 Then
        Cl.myButton.Caption = NbMines
        Cl.boolFind = True
        Cl.myButton.BackColor = COLOR_BOUTON
    Else
        If Cl.boolFind = False Then
            Cl.boolFind = True
            Cl.myButton.Visible = False
            What_Neighbours Cl
            Dim Tb() As cMinesweeper, i As Integer
            Tb = Cl.Neighbours
            For i = 0 To UBound(Tb)
                Demine Tb(i)
            Next
        End If
    End If
End Sub

Private Function Count_Of_Mines(Bout As String) As Integer
Dim i As Integer, j As Integer, Col As Integer, Lin As Integer
Dim myClass As cMinesweeper
    For i = -1 To 1
        For j = -1 To 1
            Col = CInt(Split(Bout, "-")(0)) + i
            Lin = CInt(Split(Bout, "-")(1)) + j
            If DicoParent.Exists(Col & "-" & Lin) Then
                Set myClass = DicoParent.Item(Col & "-" & Lin)
                If myClass.Mine Then Count_Of_Mines = Count_Of_Mines + 1
            End If
        Next j
    Next i
End Function

Private Sub What_Neighbours(Cl As cMinesweeper)
Dim i As Integer, j As Integer, Col As Integer, Lin As Integer
Dim myClass As cMinesweeper, ListNeighbours() As cMinesweeper, cpt As Byte
    For i = -1 To 1
        For j = -1 To 1
            Col = CInt(Split(Cl.myButton.Name, "-")(0)) + i
            Lin = CInt(Split(Cl.myButton.Name, "-")(1)) + j
            If DicoParent.Exists(Col & "-" & Lin) And Cl.myButton.Name <> Col & "-" & Lin Then
                Set myClass = DicoParent.Item(Col & "-" & Lin)
                ReDim Preserve ListNeighbours(cpt)
                Set ListNeighbours(cpt) = myClass
                cpt = cpt + 1
            End If
        Next j
    Next i
    Cl.Neighbours = ListNeighbours
End Sub

Private Function IsVictoriousGame() As Boolean
Dim cle As Variant
    For Each cle In DicoParent.Keys
        If DicoParent.Item(cle).boolFind = False And DicoParent.Item(cle).Mine = False Then IsVictoriousGame = False: Exit Function
    Next
    IsVictoriousGame = True
End Function

Private Sub Class_Terminate()
    Dim VBComp
    Set Dico = Nothing
    Set DicoParent = Nothing
    If strName <> "" Then
        Set VBComp = ThisWorkbook.VBProject.VBComponents(strName)
        ThisWorkbook.VBProject.VBComponents.Remove VBComp
    End If
End Sub

```

