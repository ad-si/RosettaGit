+++
title = "Pascal's triangle/Puzzle"
description = ""
date = 2018-09-25T14:10:03Z
aliases = []
[extra]
id = 2770
[taxonomies]
categories = ["task"]
tags = []
languages = [
  "ada",
  "algol_68",
  "autohotkey",
  "bbc_basic",
  "c",
  "clojure",
  "csharp",
  "curry",
  "d",
  "factor",
  "go",
  "haskell",
  "j",
  "julia",
  "kotlin",
  "mathematica",
  "nim",
  "oz",
  "pari_gp",
  "perl",
  "perl_6",
  "phix",
  "picolisp",
  "prolog",
  "purebasic",
  "python",
  "racket",
  "rexx",
  "ruby",
  "scala",
  "sidef",
  "systemverilog",
  "tcl",
  "zkl",
]
+++

This puzzle involves a [http://xunor.free.fr/en/riddles/auto/pyramidnb.php Pascals Triangle], also known as a [http://xunor.free.fr/en/riddles/auto/pyramidnb.php Pyramid of Numbers].

```txt

           [ 151]
          [  ][  ]
        [40][  ][  ]
      [  ][  ][  ][  ]
    [ X][11][ Y][ 4][ Z]

```

Each brick of the pyramid is the sum of the two bricks situated below it.

Of the three missing numbers at the base of the pyramid,
the middle one is the sum of the other two (that is, Y = X + Z).


## Task

Write a program to find a solution to this puzzle.





## Ada

The solution makes an upward run symbolically, though excluding Z. After that two blocks (1,1) and (3,1) being known yield a 2x2 linear system, from which X and Y are determined. Finally each block is revisited and printed.

```ada
with Ada.Text_IO; use Ada.Text_IO;

procedure Pyramid_of_Numbers is

   B_X, B_Y, B_Z : Integer := 0; -- Unknown variables

   type Block_Value is record
      Known   : Integer := 0;
      X, Y, Z : Integer := 0;
   end record;
   X : constant Block_Value := (0, 1, 0, 0);
   Y : constant Block_Value := (0, 0, 1, 0);
   Z : constant Block_Value := (0, 0, 0, 1);
   procedure Add (L : in out Block_Value; R : Block_Value) is
   begin -- Symbolically adds one block to another
      L.Known := L.Known + R.Known;
      L.X := L.X + R.X - R.Z; -- Z is excluded as n(Y - X - Z) = 0
      L.Y := L.Y + R.Y + R.Z;
   end Add;
   procedure Add (L : in out Block_Value; R : Integer) is
   begin -- Symbolically adds a value to the block
      L.Known := L.Known + R;
   end Add;

   function Image (N : Block_Value) return String is
   begin -- The block value, when X,Y,Z are known
      return Integer'Image (N.Known + N.X * B_X + N.Y * B_Y + N.Z * B_Z);
   end Image;

   procedure Solve_2x2 (A11, A12, B1, A21, A22, B2 : Integer) is
   begin -- Don't care about things, supposing an integer solution exists
      if A22 = 0 then
         B_X := B2 / A21;
         B_Y := (B1 - A11*B_X) / A12;
      else
         B_X := (B1*A22 - B2*A12) / (A11*A22 - A21*A12);
         B_Y := (B1 - A11*B_X) / A12;
      end if;
      B_Z := B_Y - B_X;
   end Solve_2x2;

   B : array (1..5, 1..5) of Block_Value; -- The lower triangle contains blocks

begin
   -- The bottom blocks
   Add (B(5,1),X); Add (B(5,2),11); Add (B(5,3),Y); Add (B(5,4),4); Add (B(5,5),Z);

   -- Upward run
   for Row in reverse 1..4 loop
      for Column in 1..Row loop
         Add (B (Row, Column), B (Row + 1, Column));
         Add (B (Row, Column), B (Row + 1, Column + 1));
      end loop;
   end loop;

   -- Now have known blocks 40=(3,1), 151=(1,1) and Y=X+Z to determine X,Y,Z
   Solve_2x2
   (  B(1,1).X, B(1,1).Y, 151 - B(1,1).Known,
      B(3,1).X, B(3,1).Y,  40 - B(3,1).Known
   );

   -- Print the results
   for Row in 1..5 loop
      New_Line;
      for Column in 1..Row loop
         Put (Image (B(Row,Column)));
      end loop;
   end loop;
end Pyramid_of_Numbers;
```

```txt


 151
 81 70
 40 41 29
 16 24 17 12
 5 11 13 4 8

```


## ALGOL 68

<!-- {{does not work with|ELLA ALGOL 68|Any (with appropriate job cards AND formatted transput statements removed) - tested with release 1.8.8d.fc9.i386 - ELLA has no FORMATted transput, and not  }} -->

```algol68
MODE
  FIELD = REAL,
  VEC = [0]REAL,
  MAT = [0,0]REAL;
MODE BRICK = UNION(INT, CHAR);

FLEX[][]BRICK puzzle = (
           ( 151),
         ( " ", " "),
       (  40, " ", " "),
     ( " ", " ", " ", " "),
   ( "x",  11, "y",  4, "z")
);

PROC mat col = (INT row, col)INT: row*(row-1)OVER 2 + col;
INT col x = mat col(5,1),
    col y = mat col(5,3),
    col z = mat col(5,5);

OP INIT = (REF VEC vec)VOID: FOR elem FROM LWB vec TO UPB vec DO vec[elem]:=0 OD;
OP INIT = (REF MAT mat)VOID: FOR row FROM LWB mat TO UPB mat DO INIT mat[row,] OD;

OP / = (MAT a, MAT b)MAT:( # matrix division #
  [LWB b:UPB b]INT p ;
  INT sign;
  [,]FIELD lu = lu decomp(b, p, sign);
  [LWB a:UPB a, 1 LWB a:2 UPB a]FIELD out;
  FOR col FROM 2 LWB a TO 2 UPB a DO out[,col] := lu solve(b, lu, p, a[,col]) OD;
  out
);

OP / = (VEC a, MAT b)VEC: ( # vector division #
  [LWB a:UPB a,1]FIELD transpose a;
  transpose a[,1]:=a;
  (transpose a/b)[,LWB a]
);

INT upb mat = mat col(UPB puzzle, UPB puzzle);
[upb mat, upb mat] REAL mat; INIT mat;
[upb mat] REAL vec; INIT vec;

INT mat row := LWB mat;
INT known row := UPB mat - UPB puzzle + 1;

# build the simultaneous equation to solve #
FOR row FROM LWB puzzle TO UPB puzzle DO
  FOR col FROM LWB puzzle[row] TO UPB puzzle[row] DO
    IF row < UPB puzzle THEN
      mat[mat row, mat col(row, col)] := 1;
      mat[mat row, mat col(row+1, col)] := -1;
      mat[mat row, mat col(row+1, col+1)] := -1;
      mat row +:= 1
    FI;
    CASE puzzle[row][col] IN
      (INT value):(
        mat[known row, mat col(row, col)] := 1;
        vec[known row] := value;
        known row +:= 1
      ),
      (CHAR variable):SKIP
    ESAC
  OD
OD;

# finally add x - y + z = 0 #
mat[known row, col x] := 1;
mat[known row, col y] := -1;
mat[known row, col z] := 1;

FORMAT real repr = $g(-5,2)$;

CO # print details of the simultaneous equation being solved #
FORMAT
  vec repr = $"("n(2 UPB mat-1)(f(real repr)", ")f(real repr)")"$,
  mat repr = $"("n(1 UPB mat-1)(f(vec repr)", "lx)f(vec repr)")"$;

printf(($"Vec: "l$,vec repr, vec, $l$));
printf(($"Mat: "l$,mat repr, mat, $l$));
END CO

# finally actually solve the equation #
VEC solution vec = vec/mat;

# and wrap up by printing the solution #
FLEX[UPB puzzle]FLEX[0]REAL solution;
FOR row FROM LWB puzzle TO UPB puzzle DO
  solution[row] := LOC[row]REAL;
  FOR col FROM LWB puzzle[row] TO UPB puzzle[row] DO
    solution[row][col] := solution vec[mat col(row, col)]
  OD;
  printf(($n(UPB puzzle-row)(4x)$, $x"("f(real repr)")"$, solution[row], $l$))
OD;

FOR var FROM 1 BY 2 TO 5 DO
  printf(($5x$,$g$,puzzle[UPB puzzle][var],"=", real repr, solution[UPB puzzle][var]))
OD
```

```txt

                 (151.0)
             (81.00) (70.00)
         (40.00) (41.00) (29.00)
     (16.00) (24.00) (17.00) (12.00)
 ( 5.00) (11.00) (13.00) ( 4.00) ( 8.00)
     x= 5.00     y=13.00     z= 8.00

```



## AutoHotkey

The main part is this:

```autohotkey
N1 := 11, N2 := 4, N3 := 40, N4 := 151
Z := (2*N4 - 7*N3 - 8*N2 + 6*N1) / 7
X := (N3 - 2*N1 - Z) / 2
MsgBox,, Pascal's Triangle, %X%`n%Z%
```

Message box shows:

```txt
5.000000
8.000000
```

The fun part is to create a GUI for entering different values for N1, N2, N3 and N4.

The GUI shows all values in the solved state.

```autohotkey
;---------------------------------------------------------------------------
; Pascal's triangle.ahk
; by wolf_II
;---------------------------------------------------------------------------
; http://rosettacode.org/wiki/Pascal's_triangle/Puzzle
;---------------------------------------------------------------------------



;---------------------------------------------------------------------------
AutoExecute: ; auto-execute section of the script
;---------------------------------------------------------------------------
    #SingleInstance, Force          ; only one instance allowed
    #NoEnv                          ; don't check empty variables
    ;-----------------------------------------------------------------------
    AppName := "Pascal's triangle"
    N1 := 11, N2 := 4, N3 := 40, N4 := 151

    ; monitor MouseMove events
    OnMessage(0x0200, "WM_MOUSEMOVE")

    ; GUI
    Gosub, GuiCreate
    Gui, Show,, %AppName%

Return



;---------------------------------------------------------------------------
GuiCreate: ; create the GUI
;---------------------------------------------------------------------------
    Gui, -MinimizeBox
    Gui, Margin, 8, 8

    ; 15 edit controls
    Loop, 5
        Loop, % Row := A_Index {
            xx := 208 + (A_Index - 5) * 50 - (Row - 5) * 25
            yy := 8 + (Row - 1) * 22
            vv := Row "_" A_Index
            Gui, Add, Edit, x%xx% y%yy% w50 v%vv% Center ReadOnly -TabStop
        }
    GuiControl, -WantReturn, Edit11
    GuiControl, -WantReturn, Edit15

    ; buttons (2 hidden)
    Gui, Add, Button, x8 w78, &Restart
    Gui, Add, Button, x+8 wp, &Solve
    Gui, Add, Button, x+8 wp, &Check
    Gui, Add, Button, x8 wp, Cle&ar
    Gui, Add, Button, xp wp Hidden, &Cancel
    Gui, Add, Button, x+8 wp, &New
    Gui, Add, Button, xp wp Hidden, &Apply
    Gui, Add, Button, x+8 wp, E&xit

    ; status bar
    Gui, Add, StatusBar

    ; blue font
    Gui, Font, bold cBlue
    GuiControl, Font, Edit11
    GuiControl, Font, Edit15
    ; falling through

;---------------------------------------------------------------------------
ButtonRestart: ; restart retaining the blue clues
;---------------------------------------------------------------------------
    Controls(True) ; enable controls
    Loop, 15
        If A_Index Not In 1,4,11,12,14,15
            GuiControl,, Edit%A_Index% ; clear
    GuiControl,, Edit1, %N4%
    GuiControl,, Edit4, %N3%
    GuiControl,, Edit12, %N1%
    GuiControl,, Edit14, %N2%
    GuiControl,, Edit11, %X%
    GuiControl,, Edit15, %Z%
    GreenFont:
    Gui, Font, bold cGreen
    GuiControl, Font, Edit1
    GuiControl, Font, Edit4
    GuiControl, Font, Edit12
    GuiControl, Font, Edit14

Return



;---------------------------------------------------------------------------
ButtonSolve: ; calculate solution
;---------------------------------------------------------------------------
    ; N1 := 11    N2 := 4    N3 := 40    N4 := 151
    ;-----------------------------------------------------------------------
    ; Y = X + Z
    ; 40  = (11+X) + (11+Y)
    ; A   = (11+Y) + (Y+4)
    ; B   =  (4+Y) + (4+Z)
    ; 151 = (40+A) + (A+B)
    ;-----------------------------------------------------------------------
    Gosub, GreenFont
    GuiControl,, Edit15, % Z := Round( (2*N4 - 7*N3 - 8*N2 + 6*N1) / 7 )
    GuiControl,, Edit11, % X := Round( (N3 - 2*N1 - Z) / 2 )
    ; falling through

;---------------------------------------------------------------------------
ButtonCheck: ; check the [entry|solution] for errors
;---------------------------------------------------------------------------
    Controls(False) ; disable controls
    Gui, Submit, NoHide
    X := 5_1, Z := 5_5
    Loop, 5
        Loop, % Row := A_Index
            If (%Row%_%A_Index% = "")
                %Row%_%A_Index% := 0
    GuiControl,, Edit13, % 5_3 := 5_1 + 5_5
    GuiControl,, Edit10, % 4_4 := 5_4 + 5_5
    GuiControl,, Edit9,  % 4_3 := 5_3 + 5_4
    GuiControl,, Edit8,  % 4_2 := 5_2 + 5_3
    GuiControl,, Edit7,  % 4_1 := 5_1 + 5_2
    GuiControl,, Edit6,  % 3_3 := 4_4 + 4_3
    GuiControl,, Edit5,  % 3_2 := 4_3 + 4_2
    GuiControl,, Edit4,  % 3_1 := 4_2 + 4_1
    GuiControl,, Edit3,  % 2_2 := 3_3 + 3_2
    GuiControl,, Edit2,  % 2_1 := 3_2 + 3_1
    GuiControl,, Edit1,  % 1_1 := 2_2 + 2_1
    Gui, Font, bold cRed
    If Not 3_1 = N3
        GuiControl, Font, Edit4
    If Not 1_1 = N4
        GuiControl, Font, Edit1

Return



;---------------------------------------------------------------------------
ButtonClear: ; restart without the blue clues
;---------------------------------------------------------------------------
    X := Z := ""
    Gosub, ButtonRestart

Return



;---------------------------------------------------------------------------
ButtonNew: ; enter new numbers for the puzzle
;---------------------------------------------------------------------------
    Gosub, GreenFont
    Loop, 15
        If A_Index Not In 1,4,12,14
            GuiControl,, Edit%A_Index% ; clear
    Controls(False) ; disable controls
    NewContr(True)  ; enable controls for new numbers

Return



;---------------------------------------------------------------------------
ButtonApply: ; remember the new numbers
;---------------------------------------------------------------------------
    Gui, Submit, NoHide
    N1 := 5_2, N2 := 5_4, N3 := 3_1, N4 := 1_1
    NewContr(False) ; disable controls for new numbers
    Controls(True)  ; enable controls

Return



;---------------------------------------------------------------------------
ButtonCancel: ; restore the old numbers
;---------------------------------------------------------------------------
    GuiControl,, Edit1, %N4%
    GuiControl,, Edit4, %N3%
    GuiControl,, Edit12, %N1%
    GuiControl,, Edit14, %N2%
    NewContr(False) ; disable controls for new numbers
    Controls(True)  ; enable controls

Return



;---------------------------------------------------------------------------
GuiClose:
;---------------------------------------------------------------------------
GuiEscape:
;---------------------------------------------------------------------------
ButtonExit:
;---------------------------------------------------------------------------
    ; common action
    ExitApp

Return



;---------------------------------------------------------------------------
Controls(Bool) { ; [dis|re-en]able some controls
;---------------------------------------------------------------------------
    Enable  := Bool ? "+" : "-"
    Disable := Bool ? "-" : "+"

    GuiControl, %Disable%ReadOnly, Edit11
    GuiControl, %Disable%ReadOnly, Edit15
    GuiControl, %Enable%TabStop, Edit11
    GuiControl, %Enable%TabStop, Edit15

    GuiControl, %Disable%Default, &Restart
    GuiControl, %Enable%Default, &Check
    GuiControl, %Disable%Disabled, &Check
    GuiControl, %Enable%Disabled, &Restart
}



;---------------------------------------------------------------------------
NewContr(Bool) { ; [dis|re-en]able control for new numbers
;---------------------------------------------------------------------------
    Enable  := Bool ? "+" : "-"
    Disable := Bool ? "-" : "+"

    GuiControl, %Disable%ReadOnly, Edit1
    GuiControl, %Disable%ReadOnly, Edit4
    GuiControl, %Disable%ReadOnly, Edit12
    GuiControl, %Disable%ReadOnly, Edit14

    GuiControl, %Enable%TabStop, Edit1
    GuiControl, %Enable%TabStop, Edit4
    GuiControl, %Enable%TabStop, Edit12
    GuiControl, %Enable%TabStop, Edit14

    GuiControl, %Enable%Hidden, Button1
    GuiControl, %Enable%Hidden, Button2
    GuiControl, %Enable%Hidden, Button3
    GuiControl, %Enable%Hidden, Button4
    GuiControl, %Disable%Hidden, Button5
    GuiControl, %Enable%Hidden, Button6
    GuiControl, %Disable%Hidden, Button7
    GuiControl, %Enable%Hidden, Button8

}



;---------------------------------------------------------------------------
WM_MOUSEMOVE() { ; monitor MouseMove events
;---------------------------------------------------------------------------
    ; display quick help in StatusBar
    ;-----------------------------------------------------------------------
    global AppName
    CurrControl := A_GuiControl
    IfEqual True,, MsgBox ; dummy

    ; mouse is over buttons
    Else If (CurrControl = "&Restart")
        SB_SetText("restart retaining the blue clues")
    Else If (CurrControl = "&Solve")
        SB_SetText("calculate solution")
    Else If (CurrControl = "&Check")
        SB_SetText("check if the entries are correct")
    Else If (CurrControl = "Cle&ar")
        SB_SetText("restart without the blue clues")
    Else If (CurrControl = "&New")
        SB_SetText("enter new numbers for the puzzle")
    Else If (CurrControl = "E&xit")
        SB_SetText("exit " AppName)

    ; delete status bar text
    Else SB_SetText("")
}
```



## BBC BASIC

```bbcbasic
      INSTALL @lib$ + "ARRAYLIB"

      REM Describe the puzzle as a set of simultaneous equations:
      REM  a + b = 151
      REM  a - c = 40
      REM  -b + c + d = 0
      REM  e + f = 40
      REM  -c + f + g = 0
      REM  -d + g + h = 0
      REM  e - x = 11
      REM  f - y = 11
      REM  g - y = 4
      REM  h - z = 4
      REM  x - y + z = 0
      REM So we have 11 equations in 11 unknowns.

      REM We can represent these equations as a matrix and a vector:
      DIM matrix(10,10), vector(10)
      matrix() = \ a, b, c, d, e, f, g, h, x, y, z
      \            1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, \
      \            1, 0,-1, 0, 0, 0, 0, 0, 0, 0, 0, \
      \            0,-1, 1, 1, 0, 0, 0, 0, 0, 0, 0, \
      \            0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, \
      \            0, 0,-1, 0, 0, 1, 1, 0, 0, 0, 0, \
      \            0, 0, 0,-1, 0, 0, 1, 1, 0, 0, 0, \
      \            0, 0, 0, 0, 1, 0, 0, 0,-1, 0, 0, \
      \            0, 0, 0, 0, 0, 1, 0, 0, 0,-1, 0, \
      \            0, 0, 0, 0, 0, 0, 1, 0, 0,-1, 0, \
      \            0, 0, 0, 0, 0, 0, 0, 1, 0, 0,-1, \
      \            0, 0, 0, 0, 0, 0, 0, 0, 1,-1, 1
      vector() = 151, 40, 0, 40, 0, 0, 11, 11, 4, 4, 0

      REM Now solve the simultaneous equations:
      PROC_invert(matrix())
      vector() = matrix().vector()

      PRINT "X = " ; vector(8)
      PRINT "Y = " ; vector(9)
      PRINT "Z = " ; vector(10)
```

```txt

X = 5
Y = 13
Z = 8

```



## C#


```c sharp

using System;

namespace Pyramid_of_Numbers
{
        class Program
	{
		public static void Main(string[] args)
		{
			// Set console properties
			Console.Title = " Pyramid of Numbers  /  Pascal's triangle Puzzle";
			Console.SetBufferSize(80,1000);
			Console.SetWindowSize(80,60);
			Console.ForegroundColor = ConsoleColor.Green;


			// Main Program Loop
			ConsoleKeyInfo k = new ConsoleKeyInfo('Y', ConsoleKey.Y,true,true,true);
			while (k.Key == ConsoleKey.Y)
			{
				Console.Clear();

				Console.WriteLine("----------------------------------------------");
				Console.WriteLine(" Pyramid of Numbers / Pascal's triangle Puzzle");
				Console.WriteLine("----------------------------------------------");
				Console.WriteLine();



				//
				// Declare new Pyramid array
				//
				int r = 5;// Number of rows
				int [,] Pyramid = new int[r,r];

				// Set initial Pyramid values
				for (int i = 0; i < r; i++)
				{
					for(int j = 0; j < r; j++)
					{
						Pyramid[i,j] = 0;
					}
				}
				// Show info on created array
				Console.WriteLine(" Pyramid has " + r + " rows");
				Console.WriteLine("--------------------------------------------");

				// Enter Pyramid values
				for(int i = 0; i <= r-1; i++)
				{
					Console.WriteLine(" Enter " + (i+1).ToString() + ". row values:");
					Console.WriteLine("--------------------------------------------");

					for(int j = 0; j < i+1; j++)
					{
						Console.Write(" " + (j+1).ToString() + ". value = ");
						int v = int.Parse(Console.ReadLine());

						Pyramid[i,j] = v;
					}
					Console.WriteLine("--------------------------------------------");
				}

				//
				// Show initial Pyramid values
				//
				Console.WriteLine();
				Console.WriteLine(" Initial Pyramid Values ");
				Console.WriteLine();

				// Show Pyramid values
				for(int i = 0; i <= r-1; i++)
				{
					for(int j = 0; j < i+1; j++)
					{
						Console.Write("{0,4}",Pyramid[i,j]);
					}
					Console.WriteLine();
				}
				Console.WriteLine("--------------------------------------------");

				// Find solution
				Solve_Pyramid(Pyramid);

				Console.WriteLine();
				Console.Write(" Start new calculation <Y/N>  . . . ");
				k = Console.ReadKey(true);
			}
		}

                //
                // Solve Function
                //
		public static void Solve_Pyramid(int [,] Pyramid)
		{
			int r = 5; // Number of rows

			// Calculate Y
			int a = Pyramid[r-1,1];
			int b = Pyramid[r-1,3];
			int c = Pyramid[0,0];

			int y =  (c - (4*a) - (4*b))/7;
			Pyramid[r-1,2] = y;


			// Create copy of Pyramid
			int [,] Pyramid_Copy = new int[r,r];
			Array.Copy(Pyramid,Pyramid_Copy,r*r);

			int n = 0; // solution counter
			for(int x = 0; x < y + 1; x++)
			{
				for(int z = 0; z < y + 1; z++)
				{
					if( (x+z) == y)
					{
						Pyramid[r-1,0]   = x;
						Pyramid[r-1,r-1] = z;

						// Recalculate Pyramid values
						for(int i = r-1; i > 0; i--)
						{
							for(int j = 0; j < i; j++)
							{
								Pyramid[i-1,j] = Pyramid[i,j]+Pyramid[i,j+1];
							}
						}


						// Compare Pyramid values
						bool solved = true;
						for(int i = 0; i < r-1; i++)
						{
							for(int j = 0; j < i+1; j++)
							{
								if(Pyramid_Copy[i,j]>0)
								{
									if(Pyramid[i,j] != Pyramid_Copy[i,j])
									{
										solved = false;
										i = r;
										break;
									}
								}
							}
						}

						if(solved)
						{
							n++;
							Console.WriteLine();
							Console.WriteLine(" Solved Pyramid Values no." + n);
							Console.WriteLine();

							// Show Pyramid values
							for(int i = 0; i <= r-1; i++)
							{
								for(int j = 0; j < i+1; j++)
								{
									Console.Write("{0,4}",Pyramid[i,j]);
								}
								Console.WriteLine();
							}
							Console.WriteLine();
							Console.WriteLine(" X = " + Pyramid[r-1,0] + "   " +
							                  " Y = " + Pyramid[r-1,2] + "   " +
							                  " Z = " + Pyramid[r-1,4]);
							Console.WriteLine();
							Console.WriteLine("--------------------------------------------");
						}

						Array.Copy(Pyramid_Copy,Pyramid,r*r);
					}
				}
			}

			if(n == 0)
			{
				Console.WriteLine();
				Console.WriteLine(" Pyramid has no solution ");
				Console.WriteLine();
			}
		}

	}
}

```

```txt


----------------------------------------------
 Pyramid of Numbers / Pascal's triangle Puzzle
----------------------------------------------

 Pyramid has 5 rows
--------------------------------------------
 Enter 1. row values:
--------------------------------------------
 1. value = 151
--------------------------------------------
 Enter 2. row values:
--------------------------------------------
 1. value = 0
 2. value = 0
--------------------------------------------
 Enter 3. row values:
--------------------------------------------
 1. value = 40
 2. value = 0
 3. value = 0
--------------------------------------------
 Enter 4. row values:
--------------------------------------------
 1. value = 0
 2. value = 0
 3. value = 0
 4. value = 0
--------------------------------------------
 Enter 5. row values:
--------------------------------------------
 1. value = 0
 2. value = 11
 3. value = 0
 4. value = 4
 5. value = 0
--------------------------------------------

 Initial Pyramid Values

 151
   0   0
  40   0   0
   0   0   0   0
   0  11   0   4   0
--------------------------------------------

 Solved Pyramid Values no.1

 151
  81  70
  40  41  29
  16  24  17  12
   5  11  13   4   8

 X = 5    Y = 13    Z = 8

--------------------------------------------

 Start new calculation <Y/N>  . . .



```



## C

This solution is based upon algebraic necessities, namely that a solution exists when (top - 4(a+b))/7 is integral. It also highlights the type difference between floating point numbers and integers in C.


```c

/* Pascal's pyramid solver
 *
 *               [top]
 *            [   ] [   ]
 *         [mid] [   ] [   ]
 *      [   ] [   ] [   ] [   ]
 *   [ x ] [ a ] [ y ] [ b ] [ z ]
 *             x + z = y
 *
 * This solution makes use of a little bit of mathematical observation,
 * such as the fact that top = 4(a+b) + 7(x+z) and mid = 2x + 2a + z.
 */

#include <stdio.h>
#include <math.h>

void pascal(int a, int b, int mid, int top, int* x, int* y, int* z)
{
    double ytemp = (top - 4 * (a + b)) / 7.;
    if(fmod(ytemp, 1.) >= 0.0001)
    {
        x = 0;
        return;
    }
    *y = ytemp;
    *x = mid - 2 * a - *y;
    *z = *y - *x;
}
int main()
{
    int a = 11, b = 4, mid = 40, top = 151;
    int x, y, z;
    pascal(a, b, mid, top, &x, &y, &z);
    if(x != 0)
        printf("x: %d, y: %d, z: %d\n", x, y, z);
    else printf("No solution\n");

    return 0;
}

```

```txt

x: 5, y: 13, z: 8

```





### Field equation solver

Treating relations between cells as if they were differential equations, and apply negative feedback to each cell at every iteration step.  This is how field equations with boundary conditions are solved numerically.  It is, of course, not the optimal solution for this particular task.

```c
#include <stdio.h>
#include <stdlib.h>

void show(int *x) {
	int i, j;

	for (i = 0; i < 5; i++)
		for (j = 0; j <= i; j++)
			printf("%4d%c", *(x++), j < i ? ' ' : '\n');
}

inline int sign(int i)
{
	return i < 0 ? -1 : i > 0;
}

int iter(int *v, int *diff) {
	int sum, i, j, e = 0;

#	define E(x, row, col) x[(row) * ((row) + 1) / 2 + (col)]
	/* enforce boundary conditions */
	E(v, 0, 0) = 151;
	E(v, 2, 0) = 40;
	E(v, 4, 1) = 11;
	E(v, 4, 3) = 4;

	/* calculate difference from equilibrium */
	for (i = 1; i < 5; i++) {
		for (j = 0; j <= i; j++) {
			E(diff, i, j) = 0;
			if (j < i)
				E(diff, i, j) += E(v, i - 1, j) -
						 E(v, i, j + 1) -
						 E(v, i, j);
			if (j)
				E(diff, i, j) += E(v, i - 1, j - 1) -
						 E(v, i, j - 1) -
						 E(v, i, j);
		}
	}

	for (i = 0; i < 4; i++)
		for (j = 0; j < i; j++)
			E(diff, i, j) += E(v, i + 1, j) +
					 E(v, i + 1, j + 1) -
					 E(v, i, j);

	E(diff, 4, 2) += E(v, 4, 0) + E(v, 4, 4) - E(v, 4, 2);
#	undef E

	/* Do feedback, check if we are done. */
	for (i = sum = 0; i < 15; i++) {
		sum += !!sign(e = diff[i]);

		/* 1/5-ish feedback strength on average.  These numbers are highly
		   magical, depending on nodes' connectivities. */
		if (e >= 4 || e <= -4) 		v[i] += e/5;
		else if (rand() < RAND_MAX/4)	v[i] += sign(e);
	}
	return sum;
}

int main() {
	int v[15] = { 0 }, diff[15] = { 0 }, i, s;

	for (i = s = 1; s; i++) {
		s = iter(v, diff);
		printf("pass %d: %d\n", i, s);
	}
	show(v);

	return 0;
}
```

```txt
pass 1: 12
pass 2: 12
pass 3: 14
pass 4: 14
...
pass 113: 4
pass 114: 7
pass 115: 0
 151
  81   70
  40   41   29
  16   24   17   12
   5   11   13    4    8
```



## Clojure


X and Z are the independent variables, so first work bottom up and determine the value of each cell in the form (n0 + n1*X + n2*Z).
We'll use a vector [n0 n1 n2] to represent each cell.

```clojure
(def bottom [ [0 1 0], [11 0 0], [0 1 1], [4 0 0], [0 0 1] ])

(defn plus  [v1 v2] (vec (map + v1 v2)))
(defn minus [v1 v2] (vec (map - v1 v2)))
(defn scale [n v]   (vec (map #(* n %) v )))

(defn above [row] (map #(apply plus %) (partition 2 1 row)))

(def rows (reverse (take 5 (iterate above bottom))))
```

We know the integer value of cells c00 and c20 ( base-0 row then column numbers), so by subtracting these values we get two equations of the form 0=n0+n1*X+n2*Z.

```clojure
(def c00 (get-in rows [0 0]))
(def c20 (get-in rows [2 0]))

(def eqn0 (minus c00 [151 0 0]))
(def eqn1 (minus c20 [ 40 0 0]))
```

In this case, there are only two variables, so solving the system of linear equations is simple.

```clojure
(defn solve [m]
  (assert (<= 1 m 2))
  (let [n  (- 3 m)
        v0 (scale (eqn1 n) eqn0)
        v1 (scale (eqn0 n) eqn1)
        vd (minus v0 v1)]
    (assert (zero? (vd n)))
    (/ (- (vd 0)) (vd m))))

(let [x (solve 1), z (solve 2), y (+ x z)]
  (println "x =" x ", y =" y ", z =" z))
```

If you want to solve the whole pyramid, just add a call ''(show-pyramid x z)'' to the previous ''let'' form:

```clojure
(defn dot [v1 v2] (reduce + (map * v1 v2)))

(defn show-pyramid [x z]
  (doseq [row rows]
    (println (map #(dot [1 x z] %) row)))
```



## Curry

```curry
import CLPFD
import Constraint (allC, andC)
import Findall (findall)
import List (init, last)


solve :: [[Int]] -> Success
solve body@([n]:rest) =
    domain (concat body) 1 n
  & andC (zipWith atop body rest)
  & labeling [] (concat body)
  where
    xs `atop` ys = andC $ zipWith3 tri xs (init ys) (tail ys)

tri :: Int -> Int -> Int -> Success
tri x y z = x =# y +# z

test (x,y,z) | tri y x z =
    [ [151]
    , [ _,  _]
    , [40,  _, _]
    , [ _,  _, _, _]
    , [ x, 11, y, 4, z]
    ]
main = findall $ solve . test
```

```txt
Execution time: 0 msec. / elapsed: 0 msec.
[(5,13,8)]
```



## D

```d
import std.stdio, std.algorithm;

void iterate(bool doPrint=true)(double[] v, double[] diff) @safe {
    static ref T E(T)(T[] x, in size_t row, in size_t col)
    pure nothrow @safe @nogc {
        return x[row * (row + 1) / 2 + col];
    }

    double tot = 0.0;
    do {
        // Enforce boundary conditions.
        E(v, 0, 0) = 151;
        E(v, 2, 0) = 40;
        E(v, 4, 1) = 11;
        E(v, 4, 3) = 4;

        // Calculate difference from equilibrium.
        foreach (immutable i; 1 .. 5) {
            foreach (immutable j; 0 .. i + 1) {
                E(diff, i, j) = 0;
                if (j < i)
                    E(diff, i, j) += E(v, i - 1, j) - E(v, i, j + 1) - E(v, i, j);
                if (j)
                    E(diff, i, j) += E(v, i - 1, j - 1) - E(v, i, j - 1) - E(v, i, j);
            }
        }

        foreach (immutable i; 1 .. 4)
            foreach (immutable j; 0 .. i)
                E(diff, i, j) += E(v, i + 1, j) + E(v, i + 1, j + 1) - E(v, i, j);

        E(diff, 4, 2) += E(v, 4, 0) + E(v, 4, 4) - E(v, 4, 2);

        // Do feedback, check if we are close enough.
        // 4: scale down the feedback to avoid oscillations.
        v[] += diff[] / 4;
        tot = diff.map!q{ a ^^ 2 }.sum;

        static if (doPrint)
            writeln("dev: ", tot);

        // tot(dx^2) < 0.1 means each cell is no more than 0.5 away
        // from equilibrium. It takes about 50 iterations. After
        // 700 iterations tot is < 1e-25, but that's overkill.
    } while (tot >= 0.1);
}

void main() {
    static void show(in double[] x) nothrow @nogc {
        int idx;
        foreach (immutable i; 0 .. 5)
            foreach (immutable j; 0 .. i+1) {
                printf("%4d%c", cast(int)(0.5 + x[idx]), j < i ? ' ' : '\n');
                idx++;
            }
    }

    double[15] v = 0.0, diff = 0.0;
    iterate(v, diff);
    show(v);
}
```

```txt
dev: 73410
dev: 17968.7
dev: 6388.46
dev: 2883.34
dev: 1446.59
dev: 892.753
dev: 564.678
[... several more iterations...]
dev: 0.136504
dev: 0.125866
dev: 0.116055
dev: 0.107006
dev: 0.0986599
 151
  81   70
  40   41   29
  16   24   17   12
   5   11   13    4    8
```


=={{header|F_Sharp|F#}}==
<p>In a script, using the [http://numerics.mathdotnet.com/ Math.NET Numerics] library</p>

```fsharp

#load"Packages\MathNet.Numerics.FSharp\MathNet.Numerics.fsx"

open MathNet.Numerics.LinearAlgebra

let A = matrix [
                    [ 1.; 1.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0. ]
                    [ -1.;  0.; 1.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0. ]
                    [ 0.; -1.; 1.; 1.; 0.; 0.; 0.; 0.; 0.; 0.; 0. ]
                    [ 0.; 0.; 0.; 0.; 1.; 1.; 0.; 0.; 0.; 0.; 0. ]
                    [ 0.; 0.; -1.; 0.; 0.; 1.; 1.; 0.; 0.; 0.; 0. ]
                    [ 0.; 0.; 0.; -1.; 0.; 0.; 1.; 1.; 0.; 0.; 0. ]
                    [ 0.; 0.; 0.; 0.; -1.; 0.; 0.; 0.; 1.; 0.; 0. ]
                    [ 0.; 0.; 0.; 0.; 0.; -1.; 0.; 0.; 0.; 1.; 0. ]
                    [ 0.; 0.; 0.; 0.; 0.; 0.; -1.; 0.; 0.; 1.; 0. ]
                    [ 0.; 0.; 0.; 0.; 0.; 0.; 0.; -1.; 0.; 0.; 1. ]
                    [ 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 1.; -1.; 1. ]
                ]

let b = vector [151.; -40.; 0.; 40.; 0.; 0.; -11.; -11.; -4.; -4.; 0.]

let x = A.Solve(b)

printfn "x = %f, Y = %f, Z = %f" x.[8] x.[9] x.[10]
```

```txt
x = 5.000000, Y = 13.000000, Z = 8.000000
```



## Factor


```factor
USING: arrays backtrack combinators.extras fry grouping.extras
interpolate io kernel math math.ranges sequences ;
IN: rosetta-code.pascals-triangle-puzzle

: base ( ?x ?z -- seq ) 2dup + swap '[ _ 11 _ 4 _ ] >array ;

: up ( seq -- seq' ) [ [ + ] 2clump-map ] twice ;

: find-solution ( -- x z )
    10 [1,b] dup [ amb-lazy ] bi@ 2dup base
    up dup first 40 = must-be-true
    up first 151 = must-be-true ;

find-solution [I X = ${1}, Z = ${}I] nl
```

```txt

X = 5, Z = 8

```



## Go

This solution follows the way the problem might be solved with pencil and paper.  It shows a possible data representation of the problem, uses the computer to do some arithmetic, and displays intermediate and final results.

```go
package main

import "fmt"

// representation of an expression in x, y, and z
type expr struct {
    x, y, z float64 // coefficients
    c       float64 // constant term
}

// add two expressions
func addExpr(a, b expr) expr {
    return expr{a.x + b.x, a.y + b.y, a.z + b.z, a.c + b.c}
}

// subtract two expressions
func subExpr(a, b expr) expr {
    return expr{a.x - b.x, a.y - b.y, a.z - b.z, a.c - b.c}
}

// multiply expression by a constant
func mulExpr(a expr, c float64) expr {
    return expr{a.x * c, a.y * c, a.z * c, a.c * c}
}

// given a row of expressions, produce the next row up, by the given
// sum relation between blocks
func addRow(l []expr) []expr {
    if len(l) == 0 {
        panic("wrong")
    }
    r := make([]expr, len(l)-1)
    for i := range r {
        r[i] = addExpr(l[i], l[i+1])
    }
    return r
}

// given expression b in a variable, and expression a,
// take b == 0 and substitute to remove that variable from a.
func substX(a, b expr) expr {
    if b.x == 0 {
        panic("wrong")
    }
    return subExpr(a, mulExpr(b, a.x/b.x))
}

func substY(a, b expr) expr {
    if b.y == 0 {
        panic("wrong")
    }
    return subExpr(a, mulExpr(b, a.y/b.y))
}

func substZ(a, b expr) expr {
    if b.z == 0 {
        panic("wrong")
    }
    return subExpr(a, mulExpr(b, a.z/b.z))
}

// given an expression in a single variable, return value of that variable
func solveX(a expr) float64 {
    if a.x == 0 || a.y != 0 || a.z != 0 {
        panic("wrong")
    }
    return -a.c / a.x
}

func solveY(a expr) float64 {
    if a.x != 0 || a.y == 0 || a.z != 0 {
        panic("wrong")
    }
    return -a.c / a.y
}

func solveZ(a expr) float64 {
    if a.x != 0 || a.y != 0 || a.z == 0 {
        panic("wrong")
    }
    return -a.c / a.z
}

func main() {
    // representation of given information for bottom row
    r5 := []expr{{x: 1}, {c: 11}, {y: 1}, {c: 4}, {z: 1}}
    fmt.Println("bottom row:", r5)

    // given definition of brick sum relation
    r4 := addRow(r5)
    fmt.Println("next row up:", r4)
    r3 := addRow(r4)
    fmt.Println("middle row:", r3)

    // given relation y = x + z
    xyz := subExpr(expr{y: 1}, expr{x: 1, z: 1})
    fmt.Println("xyz relation:", xyz)
    // remove z from third cell using xyz relation
    r3[2] = substZ(r3[2], xyz)
    fmt.Println("middle row after substituting for z:", r3)

    // given cell = 40,
    b := expr{c: 40}
    // this gives an xy relation
    xy := subExpr(r3[0], b)
    fmt.Println("xy relation:", xy)
    // substitute 40 for cell
    r3[0] = b

    // remove x from third cell using xy relation
    r3[2] = substX(r3[2], xy)
    fmt.Println("middle row after substituting for x:", r3)

    // continue applying brick sum relation to get top cell
    r2 := addRow(r3)
    fmt.Println("next row up:", r2)
    r1 := addRow(r2)
    fmt.Println("top row:", r1)

    // given top cell = 151, we have an equation in y
    y := subExpr(r1[0], expr{c: 151})
    fmt.Println("y relation:", y)
    // using xy relation, we get an equation in x
    x := substY(xy, y)
    fmt.Println("x relation:", x)
    // using xyz relation, we get an equation in z
    z := substX(substY(xyz, y), x)
    fmt.Println("z relation:", z)

    // show final answers
    fmt.Println("x =", solveX(x))
    fmt.Println("y =", solveY(y))
    fmt.Println("z =", solveZ(z))
}
```

```txt

bottom row: [{1 0 0 0} {0 0 0 11} {0 1 0 0} {0 0 0 4} {0 0 1 0}]
next row up: [{1 0 0 11} {0 1 0 11} {0 1 0 4} {0 0 1 4}]
middle row: [{1 1 0 22} {0 2 0 15} {0 1 1 8}]
xyz relation: {-1 1 -1 0}
middle row after substituting for z: [{1 1 0 22} {0 2 0 15} {-1 2 0 8}]
xy relation: {1 1 0 -18}
middle row after substituting for x: [{0 0 0 40} {0 2 0 15} {0 3 0 -10}]
next row up: [{0 2 0 55} {0 5 0 5}]
top row: [{0 7 0 60}]
y relation: {0 7 0 -91}
x relation: {1 0 0 -5}
z relation: {0 0 -1 8}
x = 5
y = 13
z = 8

```



## Haskell

I assume the task is to solve any such puzzle, i.e. given some data


```haskell
puzzle = [["151"],["",""],["40","",""],["","","",""],["X","11","Y","4","Z"]]
```


one should calculate all possible values that fit. That just means solving a linear system of equations. We use the first three variables as placeholders for ''X'', ''Y'' and ''Z''. Then we can produce the matrix of equations:


```haskell
triangle n = n * (n+1) `div` 2

coeff xys x = maybe 0 id $ lookup x xys

row n cs = [coeff cs k | k <- [1..n]]

eqXYZ n = [(0, 1:(-1):1:replicate n 0)]

eqPyramid n h = do
  a <- [1..h-1]
  x <- [triangle (a-1) + 1 .. triangle a]
  let y = x+a
  return $ (0, 0:0:0:row n [(x,-1),(y,1),(y+1,1)])

eqConst n fields = do
  (k,s) <- zip [1..] fields
  guard $ not $ null s
  return $ case s of
    "X" - (0, 1:0:0:row n [(k,-1)])
    "Y" - (0, 0:1:0:row n [(k,-1)])
    "Z" - (0, 0:0:1:row n [(k,-1)])
    _   - (fromInteger $ read s, 0:0:0:row n [(k,1)])

equations :: [[String]] - ([Rational], [[Rational]])
equations puzzle = unzip eqs where
  fields = concat puzzle
  eqs = eqXYZ n ++ eqPyramid n h ++ eqConst n fields
  h = length puzzle
  n = length fields
```


To solve the system, any linear algebra library will do (e.g [http://hackage.haskell.org/cgi-bin/hackage-scripts/package/hmatrix-0.2.0.0 hmatrix]). For this example, we assume there are functions ''decompose'' for LR-decomposition, ''kernel'' to solve the homogenous system and ''solve'' to find a special solution for an imhomogenous system. Then


```haskell
normalize :: [Rational] - [Integer]
normalize xs = [numerator (x * v) | x <- xs] where
  v = fromInteger $ foldr1 lcm $ map denominator $ xs

run puzzle = map (normalize . drop 3) $ answer where
  (a, m) = equations puzzle
  lr = decompose 0 m
  answer = case solve 0 lr a of
    Nothing - []
    Just x  - x : kernel lr
```


will output one special solution and modifications that lead to more solutions, as in


```haskell
*Main run puzzle
[[151,81,70,40,41,29,16,24,17,12,5,11,13,4,8]]
*Main run [[""],["2",""],["X","Y","Z"]]
[[3,2,1,1,1,0],[3,0,3,-1,1,2]]
```


so for the second puzzle, not only X=1 Y=1 Z=0 is a solution,
but also X=1-1=0, Y=1+1=2 Z=0+2=2 etc.

Note that the program doesn't attempt to verify that the puzzle is in correct form.


## J


Fixed points in the pyramid are 40 and 151, which I use to check a resulting pyramid for selection:


```j
chk=:40 151&-:@(2 4{{."1)
```


verb for the base of the pyramid:


```j
base=: [,11,+,4,]
```


the height of the pyramid:


```j
ord=:5
```


=> 'chk', 'base' and 'ord' are the knowledge rules abstracted from the problem definition.

The J-sentence that solves the puzzle is:


```j
    |."2(#~chk"2) 2(+/\)^:(<ord)"1 base/"1>,{ ;~i:28
```


```txt
 151  0  0  0 0
  81 70  0  0 0
  40 41 29  0 0
  16 24 17 12 0
   5 11 13  4 8
```


Get rid of zeros:


```j
,.(1+i.5)<@{."0 1{.|."2(#~chk"2) 2(+/\)^:(<ord)"1 base/"1>,{ ;~i:28
```

or

```j
,.(<@{."0 1~1+i.@#){.|."2(#~chk"2) 2(+/\)^:(<ord)"1 base/"1>,{ ;~i:28
```



```txt
 +-----------+
 |151        |
 +-----------+
 |81 70      |
 +-----------+
 |40 41 29   |
 +-----------+
 |16 24 17 12|
 +-----------+
 |5 11 13 4 8|
 +-----------+
```



## Julia

```julia
function pascal(a::Integer, b::Integer, mid::Integer, top::Integer)
    yd = round((top - 4 * (a + b)) / 7)
    !isinteger(yd) && return 0, 0, 0
    y  = Int(yd)
    x  = mid - 2a - y
    return x, y, y - x
end

x, y, z = pascal(11, 4, 40, 151)
if !iszero(x)
    println("Solution: x = $x, y = $y, z = $z.")
else
    println("There is no solution.")
end
```


```txt
Solution: x = 5, y = 13, z = 8.
```



## Kotlin

```scala
// version 1.1.3

data class Solution(val x: Int, val y: Int, val z: Int)

fun Double.isIntegral(tolerance: Double = 0.0) =
    (this - Math.floor(this)) <= tolerance || (Math.ceil(this) - this) <= tolerance

fun pascal(a: Int, b: Int, mid: Int, top: Int): Solution {
    val yd = (top - 4 * (a + b)) / 7.0
    if (!yd.isIntegral(0.0001)) return Solution(0, 0, 0)
    val y = yd.toInt()
    val x = mid - 2 * a - y
    return Solution(x, y, y - x)
}

fun main(args: Array<String>) {
    val (x, y, z) = pascal(11, 4, 40, 151)
    if (x != 0)
        println("Solution is: x = $x, y = $y, z = $z")
    else
        println("There is no solutuon")
}
```


```txt

Solution is: x = 5, y = 13, z = 8

```



## Mathematica

We assign a variable to each block starting on top with a, then on the second row b,c et cetera. k,m, and o are replaced by X, Y, and Z. We can write the following equations:

```Mathematica
b+c==a
d+e==b
e+f==c
g+h==d
h+i==e
i+j==f
l+X==g
l+Y==h
n+Y==i
n+Z==j
X+Z==Y
```

And we have the knowns

```Mathematica
a->151
d->40
l->11
n->4
```

Giving us 10 equations with 10 unknowns; i.e. solvable. So we can do so by:

```Mathematica
eqs={a==b+c,d+e==b,e+f==c,g+h==d,h+i==e,i+j==f,l+X==g,l+Y==h,n+Y==i,n+Z==j,Y==X+Z};
knowns={a->151,d->40,l->11,n->4};
Solve[eqs/.knowns,{b,c,e,f,g,h,i,j,X,Y,Z}]
```

gives back:

```Mathematica
```

In pyramid form that would be:

```Mathematica
				151
			81		70
		40		41		29
	16		24		17		12
5		11		13		4		8
```


An alternative solution in Mathematica 10, constructing the triangle:

```Mathematica
triangle[n_, m_] :=  Nest[MovingMap[Total, #, 1] &, {x, 11, y, 4, z}, n - 1][[m]]
Solve[{triangle[3, 1] == 40, triangle[5, 1] == 151, y == x + z}, {x, y, z}]
```

Three equations and three unknowns, which gives back:

```Mathematica
```



## Nim

Translation of Ada solution:

```nim
import math, strutils

var B_X, B_Y, B_Z : int = 0

type
   Block_Value = object
      Known   : int
      X, Y, Z : int

let
   X: Block_Value = Block_Value(Known:0, X:1, Y:0, Z:0)
   Y: Block_Value = Block_Value(Known:0, X:0, Y:1, Z:0)
   Z: Block_Value = Block_Value(Known:0, X:0, Y:0, Z:1)

proc Add (L : var Block_Value, R : Block_Value) =
   # Symbolically adds one block to another
   L.Known = L.Known + R.Known
   L.X = L.X + R.X - R.Z    # Z is excluded as n(Y - X - Z) = 0
   L.Y = L.Y + R.Y + R.Z

proc Add (L: var Block_Value, R: int) =
   # Symbolically adds a value to the block
   L.Known = L.Known + R

proc Image (N : Block_Value): string =
   # The block value, when X,Y,Z are known
   result = $(N.Known + N.X * B_X + N.Y * B_Y + N.Z * B_Z)

proc Solve_2x2 (A11: int, A12:int, B1:int, A21:int, A22:int, B2: int) =
   # Don't care about things, supposing an integer solution exists
   if A22 == 0:
      B_X = toInt(B2 / A21)
      B_Y = toInt((B1 - (A11*B_X)) / A12)
   else:
      B_X = toInt((B1*A22 - B2*A12) / (A11*A22 - A21*A12))
      B_Y = toInt((B1 - A11*B_X) / A12)
   B_Z = B_Y - B_X

var B : array [1..5, array[1..5, Block_Value]]   # The lower triangle contains blocks

# The bottom blocks
Add(B[5][1],X)
Add(B[5][2],11)
Add(B[5][3],Y)
Add(B[5][4],4)
Add(B[5][5],Z)

# Upward run
for Row in countdown(4,1):
   for Column in 1 .. Row:
      Add (B[Row][Column], B[Row + 1][Column])
      Add (B[Row][Column], B[Row + 1][Column + 1])

# Now have known blocks 40=[3][1], 151=[1][1] and Y=X+Z to determine X,Y,Z
Solve_2x2( B[1][1].X,
           B[1][1].Y,
           151 - B[1][1].Known,
           B[3][1].X,
           B[3][1].Y,
           40 - B[3][1].Known)

#Print the results
for Row in 1..5:
   writeln(stdout,"")
   for Column in 1..Row:
      write(stdout, Image(B[Row][Column]), " ")
```

```txt
151
81 70
40 41 29
16 24 17 12
5 11 13 4 8
```



## Oz


```oz
%% to compile : ozc -x <file.oz>
functor

import
  System Application FD Search
define

  proc{Quest Root Rules}

    proc{Limit Rc Ls}
      case Ls of nil then skip
      [] X|Xs then
        {Limit Rc Xs}
        case X of N#V then
          Rc.N =: V
        [] N1#N2#N3 then
          Rc.N1 =: Rc.N2 + Rc.N3
        end
      end
    end

    proc {Pyramid R}
      {FD.tuple solution 15 0#FD.sup R}  %% non-negative integers domain
  %%          01      , pyramid format
  %%        02  03
  %%      04  05  06
  %%    07  08  09  10
  %%  11  12  13  14  15
      R.1 =: R.2 + R.3     %% constraints of Pyramid of numbers
      R.2 =: R.4 + R.5
      R.3 =: R.5 + R.6
      R.4 =: R.7 + R.8
      R.5 =: R.8 + R.9
      R.6 =: R.9 + R.10
      R.7 =: R.11 + R.12
      R.8 =: R.12 + R.13
      R.9 =: R.13 + R.14
      R.10 =: R.14 + R.15

      {Limit R Rules}      %% additional constraints

      {FD.distribute ff R}
    end
  in
    {Search.base.one Pyramid Root} %% search for solution
  end

  local
    Root R
  in
    {Quest Root [1#151 4#40 12#11 14#4 13#11#15]} %% supply additional constraint rules
    if {Length Root} >= 1 then
      R = Root.1
      {For 1 15 1
        proc{$ I}
          if {Member I [1 3 6 10]} then
            {System.printInfo R.I#'\n'}
          else
            {System.printInfo R.I#' '}
          end
        end
      }
    else
      {System.showInfo 'No solution found.'}
    end
  end

  {Application.exit 0}
end
```


## PARI/GP

[ 6y+x+z+4a[2]+4a[4]= 7y +4a[2]+4a[4]]
[3y+x+37 ][3y+z+23]
[40=x+y+22][ 2y+15][ y+z+8 ]
[ x+11 ][y+11 ][y+4 ][z+4 ]
[ X][11][ Y][ 4][ Z]

this helped me...

```parigp

Pascals_triangle_puzzle(topvalue=151,leftsidevalue=40,bottomvalue1=11,bottomvalue2=4) = {
y=(topvalue-(4*(bottomvalue1+bottomvalue2)))/7;
x=leftsidevalue-(y+2*bottomvalue1);
z=y-x;
print(x","y","z); }

```


I'm thinking of one to solve all puzzles regardless of size and positions. but the objective was to solve this puzzle.

## Perl


```perl
# set up triangle
my $rows = 5;
my @tri = map { [ map { {x=>0,z=>0,v=>0,rhs=>undef} } 1..$_ ] } 1..$rows;
$tri[0][0]{rhs} = 151;
$tri[2][0]{rhs} = 40;
$tri[4][0]{x} = 1;
$tri[4][1]{v} = 11;
$tri[4][2]{x} = 1;
$tri[4][2]{z} = 1;
$tri[4][3]{v} = 4;
$tri[4][4]{z} = 1;

# aggregate from bottom to top
for my $row ( reverse 0..@tri-2 ) {
    for my $col ( 0..@{$tri[$row]}-1 ){
        $tri[$row][$col]{$_} = $tri[$row+1][$col]{$_}+$tri[$row+1][$col+1]{$_} for 'x','z','v';
    }
}
# find equations
my @eqn;
for my $row ( @tri ) {
    for my $col ( @$row ){
        push @eqn, [ $$col{x}, $$col{z}, $$col{rhs}-$$col{v} ] if defined $$col{rhs};
    }
}
# print equations
print "Equations:\n";
print "  x +   z = y\n";
printf "%d x + %d z = %d\n", @$_ for @eqn;
# solve
my $f = $eqn[0][1] / $eqn[1][1];
$eqn[0][$_] -=  $f * $eqn[1][$_] for 0..2;
$f = $eqn[1][0] / $eqn[0][0];
$eqn[1][$_] -=  $f * $eqn[0][$_] for 0..2;
# print solution
print "Solution:\n";
my $x = $eqn[0][2]/$eqn[0][0];
my $z = $eqn[1][2]/$eqn[1][1];
my $y = $x+$z;
printf "x=%d, y=%d, z=%d\n", $x, $y, $z;

```

```txt
Equations:
  x +   z = y
7 x + 7 z = 91
2 x + 1 z = 18
Solution:
x=5, y=13, z=8

```


## Perl 6

```perl6
# set up triangle
my $rows = 5;
my @tri = (1..$rows).map: { [ { x => 0, z => 0, v => 0, rhs => Nil } xx $_ ] }
@tri[0][0]<rhs> = 151;
@tri[2][0]<rhs> = 40;
@tri[4][0]<x> = 1;
@tri[4][1]<v> = 11;
@tri[4][2]<x> = 1;
@tri[4][2]<z> = 1;
@tri[4][3]<v> = 4;
@tri[4][4]<z> = 1;

# aggregate from bottom to top
for @tri - 2 ... 0 -> $row {
    for 0 ..^ @tri[$row] -> $col {
        @tri[$row][$col]{$_} = @tri[$row+1][$col]{$_} + @tri[$row+1][$col+1]{$_} for 'x','z','v';
    }
}

# find equations
my @eqn = gather for @tri -> $row {
    for @$row -> $cell {
        take [ $cell<x>, $cell<z>, $cell<rhs> - $cell<v> ] if defined $cell<rhs>;
    }
}

# print equations
say "Equations:";
say "  x +   z = y";
for @eqn -> [$x,$z,$y] { say "$x x + $z z = $y" }

# solve
my $f = @eqn[0][1] / @eqn[1][1];
@eqn[0][$_] -=  $f * @eqn[1][$_] for 0..2;
$f = @eqn[1][0] / @eqn[0][0];
@eqn[1][$_] -=  $f * @eqn[0][$_] for 0..2;

# print solution
say "Solution:";
my $x = @eqn[0][2] / @eqn[0][0];
my $z = @eqn[1][2] / @eqn[1][1];
my $y = $x + $z;
say "x=$x, y=$y, z=$z";
```

```txt
Equations:
  x +   z = y
7 x + 7 z = 91
2 x + 1 z = 18
Solution:
x=5, y=13, z=8
```



## Phix

I approached this with a view to solving general pyramid puzzles, not just the one given.

```Phix
--This little ditty converts the pyramid to rules quite nicely, however I will concede
--that solving those two rules (18=2x+z and 73=5x+6z) and specifically converting them
--into xrule(35=7x) and zrule(56=7z) is somewhat amateurish - suggestions welcome.

sequence pyramid = {
          {151},
         {"",""},
       {40,"",""},
     {"","","",""},
   {"x",11,"y",4,"z"}}

sequence rules = {}

-- each cell in the pyramid is either an integer final value or an equation.
-- initially the equations are strings, we substitute all with triplets of
-- the form {k,x,z} ie k+l*x+m*z, and known values < last row become rules.

for r=5 to 1 by -1 do
    for c=1 to length(pyramid[r]) do
        object prc = pyramid[r][c], equ
        if    prc="x" then  prc = {0,1,0}     -- ie one x
        elsif prc="y" then  prc = {0,1,1}     -- ie one x plus one z
        elsif prc="z" then  prc = {0,0,1}     -- ie            one z
        else
            if prc="" or r<=4 then
                -- examples: x+11 is {0,1,0}+{11,0,0} -> {11,1,0},
                --           11+y is {11,0,0}+{0,1,1} -> {11,1,1},
                --       40=""+"" is {40,0,0}={22,2,1} ==> {18,2,1}
                equ = sq_add(pyramid[r+1][c],pyramid[r+1][c+1])
            end if
            if prc="" then  prc = equ
            else            prc = {prc,0,0}
                            if r<=4 then
                                equ[1] = prc[1]-equ[1]
                                rules = append(rules,equ)
                            end if
            end if
        end if
        pyramid[r][c] = prc
    end for
end for

ppOpt({pp_Nest,1,pp_StrFmt,1})
?"equations"
pp(pyramid)
?"rules"
pp(rules)
puts(1,"=====\n")

if length(rules)!=2 then ?9/0 end if    -- more work needed!?

-- admittedly this bit is rather amateurish, and maybe problem-specific:
sequence xrule = sq_sub(sq_mul(rules[1],rules[2][3]),sq_mul(rules[2],rules[1][3])),
         zrule = sq_sub(sq_mul(rules[2],rules[1][2]),sq_mul(rules[1],rules[2][2]))

?{"xrule",xrule}
?{"zrule",zrule}

integer x = xrule[1]/xrule[2],
        z = zrule[1]/zrule[3],
        y = x+z

printf(1,"x = %d, y=%d, z=%d\n",{x,y,z})

-- finally evaluate all the equations and print it.
for r=1 to length(pyramid) do
    for c=1 to length(pyramid[r]) do
        integer {k, l, m} = pyramid[r][c]
        pyramid[r][c] = k+l*x+m*z
    end for
end for

pp(pyramid)
```

```txt

"equations"
{{{151,0,0}},
 {{55,2,2}, {23,3,4}},
 {{40,0,0}, {15,2,2}, {8,1,2}},
 {{11,1,0}, {11,1,1}, {4,1,1}, {4,0,1}},
 {{0,1,0}, {11,0,0}, {0,1,1}, {4,0,0}, {0,0,1}}}
"rules"
{{18,2,1},
 {73,5,6}}
=====
{"xrule",{35,7,0}}
{"zrule",{56,0,7}}
x = 5, y=13, z=8
{{151},
 {81,70},
 {40,41,29},
 {16,24,17,12},
 {5,11,13,4,8}}

```

Interestingly, this appears to match Python in that 40 is propagated up the tree, whereas Perl and Go appear to propagate 22+2x+z up, not that I can think of any case where that would make a difference.


## PicoLisp


```PicoLisp
(be number (@N @Max)
   (^ @C (box 0))
   (repeat)
   (or
      ((^ @ (>= (val (-> @C)) (-> @Max))) T (fail))
      ((^ @N (inc (-> @C)))) ) )

(be + (@A @B @Sum)
   (^ @ (-> @A))
   (^ @ (-> @B))
   (^ @Sum (+ (-> @A) (-> @B))) )

(be + (@A @B @Sum)
   (^ @ (-> @A))
   (^ @ (-> @Sum))
   (^ @B (- (-> @Sum) (-> @A)))
   T
   (^ @ (ge0 (-> @B))) )

(be + (@A @B @Sum)
   (number @A @Sum)
   (^ @B (- (-> @Sum) (-> @A))) )

#{
         151
        A   B
      40  C   D
     E  F  G    H
   X  11  Y   4   Z
}#

(be puzzle (@X @Y @Z)
   (+ @A @B 151)
   (+ 40 @C @A)
   (+ @C @D @B)
   (+ @E @F 40)
   (+ @F @G @C)
   (+ @G @H @D)
   (+ @X 11 @E)
   (+ 11 @Y @F)
   (+ @Y 4 @G)
   (+ 4 @Z @H)
   (+ @X @Z @Y)
   T )
```

```txt
: (? (puzzle @X @Y @Z))
 @X=5 @Y=13 @Z=8
```



## Prolog


```prolog
:- use_module(library(clpfd)).

puzzle(Ts, X, Y, Z) :-
    Ts =   [ [151],
            [_, _],
          [40, _, _],
         [_, _, _, _],
       [X, 11, Y, 4, Z]],
    Y #= X + Z, triangle(Ts), append(Ts, Vs), Vs ins 0..sup, label(Vs).

triangle([T|Ts]) :- ( Ts = [N|_] -> triangle_(T, N), triangle(Ts) ; true ).

triangle_([], _).
triangle_([T|Ts], [A,B|Rest]) :- T #= A + B, triangle_(Ts, [B|Rest]).

% ?- puzzle(_,X,Y,Z).
% X = 5,
% Y = 13,
% Z = 8 ;
```



## PureBasic

Brute force solution.

```PureBasic
; Known;
; A.
;         [ 151]
;        [a ][b ]
;      [40][c ][d ]
;    [e ][f ][g ][h ]
;  [ X][11][ Y][ 4][ Z]
;
; B.
;  Y = X + Z

Procedure.i SolveForZ(x)
  Protected a,b,c,d,e,f,g,h,z
  For z=0 To 20
    e=x+11: f=11+(x+z): g=(x+z)+4: h=4+z
    If e+f=40
      c=f+g : d=g+h: a=40+c: b=c+d
      If a+b=151
        ProcedureReturn z
      EndIf
    EndIf
  Next z
  ProcedureReturn -1
EndProcedure

Define x=-1, z=0, title$="Pascal's triangle/Puzzle in PureBasic"
Repeat
  x+1
  z=SolveForZ(x)
Until z>=0
MessageRequester(title$,"X="+Str(x)+#CRLF$+"Y="+Str(x+z)+#CRLF$+"Z="+Str(z))
```



## Python

```python
# Pyramid solver
#            [151]
#         [   ] [   ]
#      [ 40] [   ] [   ]
#   [   ] [   ] [   ] [   ]
#[ X ] [ 11] [ Y ] [ 4 ] [ Z ]
#  X -Y + Z = 0

def combine( snl, snr ):

	cl = {}
	if isinstance(snl, int):
		cl['1'] = snl
	elif isinstance(snl, string):
		cl[snl] = 1
	else:
		cl.update( snl)

	if isinstance(snr, int):
		n = cl.get('1', 0)
		cl['1'] = n + snr
	elif isinstance(snr, string):
		n = cl.get(snr, 0)
		cl[snr] = n + 1
	else:
		for k,v in snr.items():
			n = cl.get(k, 0)
			cl[k] = n+v
	return cl


def constrain(nsum, vn ):
	nn = {}
	nn.update(vn)
	n = nn.get('1', 0)
	nn['1'] = n - nsum
	return nn

def makeMatrix( constraints ):
	vmap = set()
	for c in constraints:
		vmap.update( c.keys())
	vmap.remove('1')
	nvars = len(vmap)
	vmap = sorted(vmap)		# sort here so output is in sorted order
	mtx = []
	for c in constraints:
		row = []
		for vv in vmap:
			row.append(float(c.get(vv, 0)))
		row.append(-float(c.get('1',0)))
		mtx.append(row)

	if len(constraints) == nvars:
		print 'System appears solvable'
	elif len(constraints) < nvars:
		print 'System is not solvable - needs more constraints.'
	return mtx, vmap


def SolvePyramid( vl, cnstr ):

	vl.reverse()
	constraints = [cnstr]
	lvls = len(vl)
	for lvln in range(1,lvls):
		lvd = vl[lvln]
		for k in range(lvls - lvln):
			sn = lvd[k]
			ll = vl[lvln-1]
			vn = combine(ll[k], ll[k+1])
			if sn is None:
				lvd[k] = vn
			else:
				constraints.append(constrain( sn, vn ))

	print 'Constraint Equations:'
	for cstr in constraints:
		fset = ('%d*%s'%(v,k) for k,v in cstr.items() )
		print ' + '.join(fset), ' = 0'

	mtx,vmap = makeMatrix(constraints)

	MtxSolve(mtx)

	d = len(vmap)
	for j in range(d):
		print vmap[j],'=', mtx[j][d]


def MtxSolve(mtx):
	# Simple Matrix solver...

	mDim = len(mtx)			# dimension---
	for j in range(mDim):
		rw0= mtx[j]
		f = 1.0/rw0[j]
		for k in range(j, mDim+1):
			rw0[k] *= f

		for l in range(1+j,mDim):
			rwl = mtx[l]
			f = -rwl[j]
			for k in range(j, mDim+1):
				rwl[k] += f * rw0[k]

	# backsolve part ---
	for j1 in range(1,mDim):
		j = mDim - j1
		rw0= mtx[j]
		for l in range(0, j):
			rwl = mtx[l]
			f = -rwl[j]
			rwl[j]    += f * rw0[j]
			rwl[mDim] += f * rw0[mDim]

	return mtx


p = [ [151], [None,None], [40,None,None], [None,None,None,None], ['X', 11, 'Y', 4, 'Z'] ]
addlConstraint = { 'X':1, 'Y':-1, 'Z':1, '1':0 }
SolvePyramid( p, addlConstraint)
```

```txt
Constraint Equations:
-1*Y + 1*X + 0*1 + 1*Z  = 0
-18*1 + 1*X + 1*Y  = 0
-73*1 + 5*Y + 1*Z  = 0
System appears solvable
X = 5.0
Y = 13.0
Z = 8.0

```

The Pyramid solver is not restricted to solving for 3 variables, or just this particular pyramid.

Alternative solution using the csp module (based on code by Gustavo Niemeyerby):
http://www.fantascienza.net/leonardo/so/csp.zip


```python
from csp import Problem

p = Problem()
pvars = "R2 R3 R5 R6 R7 R8 R9 R10 X Y Z".split()
# 0-151 is the possible finite range of the variables
p.addvars(pvars, xrange(152))
p.addrule("R7 == X + 11")
p.addrule("R8 == Y + 11")
p.addrule("R9 == Y + 4")
p.addrule("R10 == Z + 4")
p.addrule("R7 + R8 == 40")
p.addrule("R5 == R8 + R9")
p.addrule("R6 == R9 + R10")
p.addrule("R2 == 40 + R5")
p.addrule("R3 == R5 + R6")
p.addrule("R2 + R3 == 151")
p.addrule("Y == X + Z")
for sol in p.xsolutions():
    print [sol[k] for k in "XYZ"]
```


```txt
[5, 13, 8]
```



## Racket

(Based on the clojure version)

Only X and Z are independent variables.
We'll use a struct (cell v x z) to represent each cell,
where the value is (v + x*X + z*Z).

```Racket

#lang racket/base
(require racket/list)

(struct cell (v x z) #:transparent)

(define (cell-add cx cy)
  (cell (+ (cell-v cx) (cell-v cy))
        (+ (cell-x cx) (cell-x cy))
        (+ (cell-z cx) (cell-z cy))))

(define (cell-sub cx cy)
  (cell (- (cell-v cx) (cell-v cy))
        (- (cell-x cx) (cell-x cy))
        (- (cell-z cx) (cell-z cy))))

```


We first work bottom up and determine the value of each cell, starting from the bottom row.

```Racket

(define (row-above row) (map cell-add (drop row 1) (drop-right row 1)))

(define row0 (list (cell 0 1 0) (cell 11 0 0) (cell 0 1 1) (cell 4 0 0) (cell 0 0 1)))
(define row1 (row-above row0))
(define row2 (row-above row1))
(define row3 (row-above row2))
(define row4 (row-above row3))

```


We know the value of two additional cells, so by subtracting these values we get two equations of the form 0=v+x*X+z*Z. In the usual notation we get x*X+z*Z=-v, so v has the wrong sign.


```Racket

(define eqn40 (cell-sub (car row4) (cell 151 0 0)))
(define eqn20 (cell-sub (car row2) (cell 40 0 0)))

```


To solve the 2 equation system, we will use the Cramer's rule.

```Racket

(define (det2 eqnx eqny get-one get-oth)
  (- (* (get-one eqnx) (get-oth eqny)) (* (get-one eqny) (get-oth eqnx))))

(define (cramer2 eqnx eqny get-val get-unk get-oth)
  (/ (det2 eqnx eqny get-val get-oth)
     (det2 eqnx eqny get-unk get-oth)))

```


To get the correct values of X, Y and Z we must change their signs.

```Racket

(define x (- (cramer2 eqn20 eqn40 cell-v cell-x cell-z)))
(define z (- (cramer2 eqn20 eqn40 cell-v cell-z cell-x)))

(displayln (list "X" x))
(displayln (list "Y" (+ x z)))
(displayln (list "Z" z))

```


```txt

(X 5)
(Y 13)
(Z 8)

```



## REXX


```rexx
/*REXX program solves a   (Pascal's)   "Pyramid of Numbers"   puzzle given four values. */
                              /*╔══════════════════════════════════════════════════╗
                                ║                                     answer       ║
                                ║                     mid            /             ║
                                ║                        \          /              ║
                                ║                         \       151              ║
                                ║                          \   ααα  ααα            ║
                                ║                           40   ααα   ααα         ║
                                ║                         ααα  ααα  ααα  ααα       ║
                                ║                       x    11    y    4    z     ║
                                ║                           /            \         ║
                                ║                          /              \        ║
                                ║                         /                \       ║
                                ║ Find:  x  y  z         b                  d      ║
                                ╚══════════════════════════════════════════════════╝*/
   do #=2;  _=sourceLine(#)                      /* [↓]  this DO loop shows (above) box.*/
   if pos('#',_)\==0  then leave                 /*only display  up to  the above line. */
   say sourceLine(#)                             /*display one line of the above box.   */
   end   /*#*/                                   /* [↑]  this is one cheap way for doc. */
parse arg  b  d  mid  answer .                   /*obtain optional variables from the CL*/
if     b=='' |      b==","  then      b=  11     /*Not specified?  Then use the default.*/
if     d=='' |      d==","  then      d=   4     /* "      "         "   "   "     "    */
if    mid='' |    mid==","  then    mid=  40     /* "      "         "   "   "     "    */
if answer='' | answer==","  then answer= 151     /* "      "         "   "   "     "    */
   pad= left('', 15)                             /*used for inserting spaces in output. */
   big= answer - 4*b - 4*d                       /*calculate big   number less constants*/
middle= mid - 2*b                                /*    "    middle    "     "      "    */
say
   do x  =-big  to big
     do y=-big  to big
     if x+y\==middle   then iterate              /*40 = x+2B+Y   ──or──   40-2*11 = x+y */
         do z=-big  to big
         if z  \==  y - x   then iterate         /*z has to equal y-x  (y=x+z)    */
         if x+y*6+z == big  then say pad  'x = '   x   pad   "y = "   y   pad   'z = '   z
         end   /*z*/
     end       /*y*/
   end         /*x*/                             /*stick a fork in it,  we're all done. */
```

```txt

                              /*╔══════════════════════════════════════════════════╗
                                ║                                     answer       ║
                                ║                     mid            /             ║
                                ║                        \          /              ║
                                ║                         \       151              ║
                                ║                          \   ααα  ααα            ║
                                ║                           40   ααα   ααα         ║
                                ║                         ααα  ααα  ααα  ααα       ║
                                ║                       x    11    y    4    z     ║
                                ║                           /            \         ║
                                ║                          /              \        ║
                                ║                         /                \       ║
                                ║ Find:  x  y  z         b                  d      ║
                                ╚══════════════════════════════════════════════════╝*/

                x =  5                 y =  13                 z =  8

```



## Ruby

uses [[Reduced row echelon form#Ruby]]

```ruby
require 'rref'

pyramid = [
           [ 151],
          [nil,nil],
        [40,nil,nil],
      [nil,nil,nil,nil],
    ["x", 11,"y", 4,"z"]
]
pyramid.each{|row| p row}

equations = [[1,-1,1,0]]   # y = x + z

def parse_equation(str)
  eqn = [0] * 4
  lhs, rhs = str.split("=")
  eqn[3] = rhs.to_i
  for term in lhs.split("+")
    case term
    when "x" then eqn[0] += 1
    when "y" then eqn[1] += 1
    when "z" then eqn[2] += 1
    else          eqn[3] -= term.to_i
    end
  end
  eqn
end

-2.downto(-5) do |row|
  pyramid[row].each_index do |col|
    val = pyramid[row][col]
    sum = "%s+%s" % [pyramid[row+1][col], pyramid[row+1][col+1]]
    if val.nil?
      pyramid[row][col] = sum
    else
      equations << parse_equation(sum + "=#{val}")
    end
  end
end

reduced = convert_to(reduced_row_echelon_form(equations), :to_i)

for eqn in reduced
  if eqn[0] + eqn[1] + eqn[2] != 1
    fail "no unique solution! #{equations.inspect} ==> #{reduced.inspect}"
  elsif eqn[0] == 1 then x = eqn[3]
  elsif eqn[1] == 1 then y = eqn[3]
  elsif eqn[2] == 1 then z = eqn[3]
  end
end

puts
puts "x == #{x}"
puts "y == #{y}"
puts "z == #{z}"

answer = []
for row in pyramid
  answer << row.collect {|cell| eval cell.to_s}
end
puts
answer.each{|row| p row}
```


```txt

[151]
[nil, nil]
[40, nil, nil]
[nil, nil, nil, nil]
["x", 11, "y", 4, "z"]

x == 5
y == 13
z == 8

[151]
[81, 70]
[40, 41, 29]
[16, 24, 17, 12]
[5, 11, 13, 4, 8]

```



## Scala


```Scala
object PascalTriangle extends App {

  val (x, y, z) = pascal(11, 4, 40, 151)

  def pascal(a: Int, b: Int, mid: Int, top: Int): (Int, Int, Int) = {
    val y = (top - 4 * (a + b)) / 7
    val x = mid - 2 * a - y
    (x, y, y - x)
  }

  println(if (x != 0) s"Solution is: x = $x, y = $y, z = $z" else "There is no solution.")
}
```

{{Out}}See it in running in your browser by [https://scalafiddle.io/sf/UJF14fw/0 (JavaScript)]
or by [https://scastie.scala-lang.org/l0AlwpSdR7i801Fq8CWHEQ Scastie (JVM)].

## Sidef

```ruby
# set up triangle
var rows = 5
var tri = rows.of {|i| (i+1).of { Hash(x => 0, z => 0, v => 0, rhs => nil) } }
tri[0][0]{:rhs} = 151
tri[2][0]{:rhs} = 40
tri[4][0]{:x} = 1
tri[4][1]{:v} = 11
tri[4][2]{:x} = 1
tri[4][2]{:z} = 1
tri[4][3]{:v} = 4
tri[4][4]{:z} = 1
 
# aggregate from bottom to top
for row in (tri.len ^.. 1) {
    for col in (^tri[row-1]) {
        [:x, :z, :v].each { |key|
            tri[row-1][col]{key} = (tri[row][col]{key} + tri[row][col+1]{key})
        }
    }
}
 
# find equations
var eqn = gather {
    for r in tri {
        for c in r {
            take([c{:x}, c{:z}, c{:rhs} - c{:v}]) if defined(c{:rhs})
        }
    }
}
 
# print equations
say "Equations:"
say " x +  z = y"
for x,z,y in eqn { say "#{x}x + #{z}z = #{y}" }
 
# solve
var f = (eqn[0][1] / eqn[1][1])
{|i| eqn[0][i] -= (f * eqn[1][i]) } << ^3
f = (eqn[1][0] / eqn[0][0])
{|i| eqn[1][i] -= (f * eqn[0][i]) } << ^3
 
# print solution
say "Solution:"
var x = (eqn[0][2] / eqn[0][0])
var z = (eqn[1][2] / eqn[1][1])
var y = (x + z)
say "x=#{x}, y=#{y}, z=#{z}"
```

```txt

Equations:
 x +  z = y
7x + 7z = 91
2x + 1z = 18
Solution:
x=5, y=13, z=8

```



## SystemVerilog

We can view this as a problem of generating a set of random numbers that satisfy the constraints. Because there is only one solution, the result isn't very random...

```SystemVerilog
program main;

  class Triangle;
    rand bit [7:0] a,b,c,d,e,f,g,h,X,Y,Z;

    function new();
      randomize;
      $display("     [%0d]",                151);
      $display("    [%0d][%0d]",            a, b);
      $display("   [%0d][%0d][%0d]",        40,c,d);
      $display("  [%0d][%0d][%0d][%0d]",    e,f,g,h);
      $display(" [%0d][%0d][%0d][%0d][%0d]",X,11,Y,4,Z);
    endfunction

    constraint structure {
       151 == a + b;

         a == 40 + c;
         b == c + d;

        40 == e + f;
         c == f + g;
         d == g + h;

         e == X + 11;
         f == 11 + Y;
         g == Y + 4;
         h == 4 + Z;
    };

    constraint extra {
         Y == X + Z;
    };

  endclass

  Triangle answer = new;
endprogram
```


```txt
     [151]
    [81][70]
   [40][41][29]
  [16][24][17][12]
 [5][11][13][4][8]

```



## Tcl

using code from [[Reduced row echelon form#Tcl]]

```tcl
package require Tcl 8.5
namespace path ::tcl::mathop

set pyramid {
    {151.0 "" "" "" ""}
    {"" "" "" "" ""}
    {40.0 "" "" "" ""}
    {"" "" "" "" ""}
    {x 11.0 y 4.0 z}
}

set equations {{1 -1 1 0}}

proc simplify {terms val} {
    set vars {0 0 0}
    set x 0
    set y 1
    set z 2
    foreach term $terms {
        switch -exact -- $term {
            x - y - z {
                lset vars [set $term] [+ 1 [lindex $vars [set $term]]]
            }
            default {
                set val [- $val $term]
            }
        }
    }
    return [concat $vars $val]
}

for {set row [+ [llength $pyramid] -2]} {$row >= 0} {incr row -1} {
    for {set cell 0} {$cell <= $row} {incr cell } {
	set sum [concat [lindex $pyramid [+ 1 $row] $cell] [lindex $pyramid [+ 1 $row] [+ 1 $cell]]]
	if {[set val [lindex $pyramid $row $cell]] ne ""} {
            lappend equations [simplify $sum $val]
	} else {
            lset pyramid $row $cell  $sum
        }
    }
}

set solution [toRREF $equations]
foreach row $solution {
    lassign $row a b c d
    if {$a + $b + $c > 1} {
        error "problem does not have a unique solution"
    }
    if {$a} {set x $d}
    if {$b} {set y $d}
    if {$c} {set z $d}
}
puts "x=$x"
puts "y=$y"
puts "z=$z"

foreach row $pyramid {
    set newrow {}
    foreach cell $row {
        if {$cell eq ""} {
            lappend newrow ""
        } else {
            lappend newrow [expr [join [string map [list x $x y $y z $z] $cell] +]]
        }
    }
    lappend solved $newrow
}
print_matrix $solved
```


```txt
x=5.0
y=13.0
z=8.0
151.0
 81.0 70.0
 40.0 41.0 29.0
 16.0 24.0 17.0 12.0
  5.0 11.0 13.0  4.0 8.0
```



## zkl

```zkl
# Pyramid solver
#            [151]
#         [   ] [   ]
#      [ 40] [   ] [   ]
#   [   ] [   ] [   ] [   ]
#[ X ] [ 11] [ Y ] [ 4 ] [ Z ]
# Known: X - Y + Z = 0

p:=T( L(151), L(Void,Void), L(40,Void,Void), L(Void,Void,Void,Void),
      L("X", 11, "Y", 4, "Z") );
addlConstraint:=Dictionary( "X",1, "Y",-1, "Z",1, "1",0 );
solvePyramid(p, addlConstraint);
```


```zkl
fcn solvePyramid([List]vl,[Dictionary]cnstr){  //ListOfLists,Hash-->zip
   vl=vl.reverse();
   constraints:=L(cnstr);
   lvls:=vl.len();
   foreach lvln in ([1..lvls-1]){
      lvd:=vl[lvln];
      foreach k in (lvls-lvln){
         sn:=lvd[k];
	 ll:=vl[lvln-1];
	 vn:=combine(ll[k], ll[k+1]);
	 if(Void==sn) lvd[k]=vn;
	 else constraints.append(constrainK(sn,vn));
      }
   }
   println("Constraint Equations:");
   constraints.pump(Console.println,fcn(hash){
      hash.pump(List,fcn([(k,v)]){"%d*%s".fmt(v,k)}).concat(" + ") + " = 0"
   });

   mtx,vmap:=makeMatrix(constraints);
   mtxSolve(mtx);

   d:=vmap.len();
   foreach j in (d){ println(vmap[j]," = ", mtx[j][d]); }
}

fcn [mixin=Dictionary] constrainK([Int]nsum,[Dictionary]vn){ //-->new hash of old hash, sum K
   nn:=vn.copy(); nn["1"]=nn.find("1",0) - nsum;
   return(nn.makeReadOnly());
}

fcn combine(snl,snr){ //Int|String|Hash *2 --> new Hash
   cl:=Dictionary();
   if(snl.isInstanceOf(Int))         cl["1"]=snl;
   else if(snl.isInstanceOf(String)) cl[snl]=1;
   else				     cl     =snl.copy();

   if(snr.isInstanceOf(Int))         cl["1"]=cl.find("1",0) + snr;
   else if(snr.isInstanceOf(String)) cl[snr]=cl.find(snr,0) + 1;
   else{ foreach k,v in (snr){ 	     cl[k]  =cl.find(k,0)   + v; } }
   return(cl.makeReadOnly())
}

    //-->(listMatrix(row(X,Y,Z,c),row...),List("X","Y","Z"))
fcn makeMatrix([Dictionary]constraints){
   vmap:=Dictionary();// create a sorted list of the variable names in constraints
   foreach c in (constraints){ vmap.extend(c) }  // no duplicate names
   vmap.del("1"); vmap=vmap.keys.sort();  # sort here so output is in sorted order

   mtx:=constraints.pump(List,'wrap(c){ // create list of [writeable] rows
      vmap.pump(List, c.find.fp1(0),"toFloat").copy()
      .append(-c.find("1",0).toFloat())
   }).copy();

   nvars:=vmap.len();
   if(constraints.len()==nvars) println("System appears solvable");
   else if(constraints.len()<nvars)
      println("System is not solvable - needs more constraints.");
   return(mtx,vmap);
}

fcn mtxSolve([List]mtx){ //munge mtx	# Simple Matrix solver...
   mDim:=mtx.len();			# num rows
   foreach j in (mDim){
      rw0:=mtx[j];
      f:=1.0/rw0[j];
      foreach k in ([j..mDim]){ rw0[k]=rw0[k]*f }
      foreach l in ([j+1..mDim-1]){
         rwl:=mtx[l]; f:=-rwl[j];
	 foreach k in ([j..mDim]){ rwl[k]+=f*rw0[k] }
      }
   }
   # backsolve part ---
   foreach j1 in ([1..mDim-1]){
      j:=mDim - j1; rw0:=mtx[j];
      foreach l in (j){
	 rwl:=mtx[l]; f:=-rwl[j];
	 rwl[j]   +=f*rw0[j];
	 rwl[mDim]+=f*rw0[mDim];
      }
   }
   return(mtx);
}
```

```txt

Constraint Equations:
0*1 + 1*X + -1*Y + 1*Z = 0
-18*1 + 1*X + 1*Y = 0
-73*1 + 5*Y + 1*Z = 0
System appears solvable
X = 5
Y = 13
Z = 8

```


