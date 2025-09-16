+++
title = "Maze generation"
description = ""
date = 2019-10-21T21:41:16Z
aliases = []
[extra]
id = 8976
[taxonomies]
categories = ["task", "Games"]
tags = []
languages = [
  "ada",
  "aime",
  "autohotkey",
  "awk",
  "basic",
  "batch_file",
  "bbc_basic",
  "befunge",
  "c",
  "clojure",
  "commodore_basic",
  "common_lisp",
  "cpp",
  "csharp",
  "d",
  "easylang",
  "egl",
  "elixir",
  "elm",
  "emacs_lisp",
  "erlang",
  "freebasic",
  "go",
  "haskell",
  "huginn",
  "j",
  "java",
  "javascript",
  "julia",
  "kotlin",
  "lua",
  "m2000_interpreter",
  "mathematica",
  "nim",
  "node_js",
  "ocaml",
  "ol",
  "perl",
  "perl_6",
  "phix",
  "php",
  "picolisp",
  "pl_i",
  "prolog",
  "purebasic",
  "python",
  "racket",
  "rascal",
  "related_tasks",
  "rexx",
  "ruby",
  "rust",
  "scala",
  "sidef",
  "supercollider",
  "swift",
  "tcl",
  "txr",
  "xpl0",
  "zkl",
]
+++

[[File:a maze.png|300px||right|a maze]]



## Task

Generate and show a maze, using the simple [[wp:Maze_generation_algorithm#Depth-first_search|Depth-first search]] algorithm.

<!-- BEGIN TEXT FROM WIKIPEDIA -->
#Start at a random cell.
#Mark the current cell as visited, and get a list of its neighbors. For each neighbor, starting with a randomly selected neighbor:
#:If that neighbor hasn't been visited, remove the wall between this cell and that neighbor, and then recurse with that neighbor as the current cell.
<!-- END TEXT FROM WIKIPEDIA -->




## Related tasks

* [[Maze solving]].





## Ada

mazes.ads:

```Ada
generic
   Height : Positive;
   Width : Positive;
package Mazes is

   type Maze_Grid is private;

   procedure Initialize (Maze : in out Maze_Grid);

   procedure Put (Item : Maze_Grid);

private

   type Directions is (North, South, West, East);

   type Cell_Walls is array (Directions) of Boolean;
   type Cells is record
      Walls   : Cell_Walls := (others => True);
      Visited : Boolean    := False;
   end record;

   subtype Height_Type is Positive range 1 .. Height;
   subtype Width_Type is Positive range 1 .. Width;

   type Maze_Grid is array (Height_Type, Width_Type) of Cells;

end Mazes;
```

mazes.adb:

```Ada
with Ada.Numerics.Discrete_Random;
with Ada.Text_IO;

package body Mazes is
   package RNG is new Ada.Numerics.Discrete_Random (Positive);
   package Random_Direction is new Ada.Numerics.Discrete_Random (Directions);

   Generator     : RNG.Generator;
   Dir_Generator : Random_Direction.Generator;

   function "-" (Dir : Directions) return Directions;

   procedure Depth_First_Algorithm
     (Maze   : in out Maze_Grid;
      Row    : Height_Type;
      Column : Width_Type);

   function Has_Unvisited_Neighbours
     (Maze   : Maze_Grid;
      Row    : Height_Type;
      Column : Width_Type)
      return   Boolean;

   procedure Move
     (Row        : in out Height_Type;
      Column     : in out Width_Type;
      Direction  : Directions;
      Valid_Move : out Boolean);

   function "-" (Dir : Directions) return Directions is
   begin
      case Dir is
         when North =>
            return South;
         when South =>
            return North;
         when East =>
            return West;
         when West =>
            return East;
      end case;
   end "-";

   procedure Depth_First_Algorithm
     (Maze   : in out Maze_Grid;
      Row    : Height_Type;
      Column : Width_Type)
   is
      Next_Row        : Height_Type;
      Next_Column     : Width_Type;
      Next_Direction  : Directions;
      Valid_Direction : Boolean;
   begin
      -- mark as visited
      Maze (Row, Column).Visited := True;
      -- continue as long as there are unvisited neighbours left
      while Has_Unvisited_Neighbours (Maze, Row, Column) loop
         -- use random direction
         Next_Direction := Random_Direction.Random (Dir_Generator);
         Next_Row       := Row;
         Next_Column    := Column;
         Move (Next_Row, Next_Column, Next_Direction, Valid_Direction);
         if Valid_Direction then
            -- connect the two cells
            if not Maze (Next_Row, Next_Column).Visited then
               Maze (Row, Column).Walls (Next_Direction)              :=
                 False;
               Maze (Next_Row, Next_Column).Walls (-Next_Direction)   :=
                 False;
               Depth_First_Algorithm (Maze, Next_Row, Next_Column);
            end if;
         end if;
      end loop;
   end Depth_First_Algorithm;

   function Has_Unvisited_Neighbours
     (Maze   : Maze_Grid;
      Row    : Height_Type;
      Column : Width_Type)
      return   Boolean
   is
      Neighbour_Row    : Height_Type;
      Neighbour_Column : Width_Type;
      Is_Valid         : Boolean;
   begin
      for Dir in Directions loop
         Neighbour_Row    := Row;
         Neighbour_Column := Column;
         Move
           (Row        => Neighbour_Row,
            Column     => Neighbour_Column,
            Direction  => Dir,
            Valid_Move => Is_Valid);
         if Is_Valid
           and then not Maze (Neighbour_Row, Neighbour_Column).Visited
         then
            return True;
         end if;
      end loop;
      return False;
   end Has_Unvisited_Neighbours;

   procedure Initialize (Maze : in out Maze_Grid) is
      Row, Column : Positive;
   begin
      -- initialize random generators
      RNG.Reset (Generator);
      Random_Direction.Reset (Dir_Generator);
      -- choose starting cell
      Row    := RNG.Random (Generator) mod Height + 1;
      Column := RNG.Random (Generator) mod Width + 1;
      Ada.Text_IO.Put_Line
        ("Starting generation at " &
         Positive'Image (Row) &
         " x" &
         Positive'Image (Column));
      Depth_First_Algorithm (Maze, Row, Column);
   end Initialize;

   procedure Move
     (Row        : in out Height_Type;
      Column     : in out Width_Type;
      Direction  : Directions;
      Valid_Move : out Boolean)
   is
   begin
      Valid_Move := False;
      case Direction is
         when North =>
            if Row > Height_Type'First then
               Valid_Move := True;
               Row        := Row - 1;
            end if;
         when East =>
            if Column < Width_Type'Last then
               Valid_Move := True;
               Column     := Column + 1;
            end if;
         when West =>
            if Column > Width_Type'First then
               Valid_Move := True;
               Column     := Column - 1;
            end if;
         when South =>
            if Row < Height_Type'Last then
               Valid_Move := True;
               Row        := Row + 1;
            end if;
      end case;
   end Move;

   procedure Put (Item : Maze_Grid) is
   begin
      for Row in Item'Range (1) loop
         if Row = Item'First (1) then
            for Col in Item'Range (2) loop
               if Col = Item'First (2) then
                  Ada.Text_IO.Put ('+');
               end if;
               if Item (Row, Col).Walls (North) then
                  Ada.Text_IO.Put ("---");
               else
                  Ada.Text_IO.Put ("   ");
               end if;
               Ada.Text_IO.Put ('+');
            end loop;
            Ada.Text_IO.New_Line;
         end if;
         for Col in Item'Range (2) loop
            if Col = Item'First (2) then
               if Item (Row, Col).Walls (West) then
                  Ada.Text_IO.Put ('|');
               else
                  Ada.Text_IO.Put (' ');
               end if;
            elsif Item (Row, Col).Walls (West)
              and then Item (Row, Col - 1).Walls (East)
            then
               Ada.Text_IO.Put ('|');
            elsif Item (Row, Col).Walls (West)
              or else Item (Row, Col - 1).Walls (East)
            then
               Ada.Text_IO.Put ('>');
            else
               Ada.Text_IO.Put (' ');
            end if;
            if Item (Row, Col).Visited then
               Ada.Text_IO.Put ("   ");
            else
               Ada.Text_IO.Put ("???");
            end if;
            if Col = Item'Last (2) then
               if Item (Row, Col).Walls (East) then
                  Ada.Text_IO.Put ('|');
               else
                  Ada.Text_IO.Put (' ');
               end if;
            end if;
         end loop;
         Ada.Text_IO.New_Line;
         for Col in Item'Range (2) loop
            --for Col in Item'Range (2) loop
            if Col = Item'First (2) then
               Ada.Text_IO.Put ('+');
            end if;
            if Item (Row, Col).Walls (South) then
               Ada.Text_IO.Put ("---");
            else
               Ada.Text_IO.Put ("   ");
            end if;
            Ada.Text_IO.Put ('+');
            --end loop;
         end loop;
         Ada.Text_IO.New_Line;
      end loop;
   end Put;
end Mazes;
```

Example main.adb:

```Ada
with Mazes;
procedure Main is
   package Small_Mazes is new Mazes (Height => 8, Width => 11);
   My_Maze : Small_Mazes.Maze_Grid;
begin
   Small_Mazes.Initialize (My_Maze);
   Small_Mazes.Put (My_Maze);
end Main;
```

```txt
Starting generation at  3 x 7
+---+---+---+---+---+---+---+---+---+---+---+
|   |               |   |                   |
+   +   +   +---+   +   +   +---+---+---+   +
|       |       |       |   |       |       |
+   +---+---+   +---+---+   +   +   +   +---+
|           |           |   |   |   |   |   |
+   +---+---+---+---+   +---+   +   +   +   +
|   |           |       |       |       |   |
+   +   +---+   +   +   +   +---+---+---+   +
|   |   |           |   |       |           |
+   +   +---+---+---+---+---+   +---+   +   +
|   |   |                   |           |   |
+---+   +   +---+---+---+   +---+---+---+   +
|       |   |           |                   |
+   +---+   +---+---+   +---+---+---+---+---+
|                                           |
+---+---+---+---+---+---+---+---+---+---+---+
```



## Aime


```aime
grid_maze(data b, integer N)
{
    data d;

    N.times(bb_cast, d, "+---");
    bb_cast(d, "+\n");

    N.times(bb_cast, d, "| * ");
    bb_cast(d, "|\n");

    N.times(bb_copy, b, d);

    b_size(d, N * 4 + 2);

    bb_copy(b, d);
}

void
walk_cell(data b, integer N, line_size, x, y, list x_offsets, y_offsets)
{
    integer i, p, q, r;

    b_replace(b, y + x, ' ');

    r = drand(3);

    i = 0;
    while (i < 4) {
        p = x + x_offsets[q = (r + i) & 3];
        q = y + y_offsets[q];

        if (-1 < p && p < line_size
            && -1 < q && q < line_size * (N * 2 + 1)) {
            if (b[q + p] == '*') {
                walk_cell(b, N, line_size, p, q, x_offsets, y_offsets);
                b[(q + y) / 2 + (p + x) / 2] = ' ';
                if (p == x) {
                    b[(q + y) / 2 + p - 1] = ' ';
                    b[(q + y) / 2 + p + 1] = ' ';
                }
            }
        }

        i += 1;
    }
}

walk_maze(data b, integer N)
{
    integer line_size, x, y;
    list x_offsets, y_offsets;

    line_size = N * 4 + 1 + 1;

    l_bill(x_offsets, 0, 4, 0, -4, 0);
    l_bill(y_offsets, 0, 0, line_size * 2, 0, line_size * -2);

    x = drand(N - 1) * 4 + 2;
    y = line_size * (drand(N - 1) * 2 + 1);

    walk_cell(b, N, line_size, x, y, x_offsets, y_offsets);
}

main(void)
{
    data b;

    grid_maze(b, 10);
    walk_maze(b, 10);

    o_(b);

    0;
}
```

```txt
+---+---+---+---+---+---+---+---+---+---+
|                       |               |
+   +---+---+---+---+   +   +---+---+   +
|   |       |       |   |   |           |
+   +   +   +   +   +   +   +   +---+   +
|   |   |       |       |   |   |   |   |
+   +---+---+   +---+---+---+   +   +   +
|           |               |   |   |   |
+---+---+---+---+---+---+   +   +   +   +
|                       |   |   |       |
+   +---+---+---+---+   +   +   +---+---+
|   |               |   |   |           |
+   +---+---+---+   +   +   +---+   +   +
|       |       |       |       |   |   |
+   +   +   +   +   +---+---+   +---+   +
|   |       |   |           |       |   |
+---+---+---+   +---+---+---+---+   +   +
|               |       |       |       |
+   +---+---+---+   +   +   +   +---+   +
|                   |       |           |
+---+---+---+---+---+---+---+---+---+---+

```



## AutoHotkey

For a challenge, this maze generation is entirely string based. That is to say, all operations including the wall removal and retrieval of cell states are done on the output string.

```AHK
; Initially build the board
Width	:= 11
Height	:= 8
Loop % height*2+1
{
	Outer := A_Index
	Loop % Width
		maze .= Outer & 1 ? "+-" : "|0"
	maze .= (Outer & 1 ? "+" : "|") "`n"
}
StringTrimRight, maze, maze, 1 ; removes trailing newline
Clipboard := Walk(maze)

Walk(S, x=0, y=0){
	If !x{	; --Start at a random cell...
		StringReplace, junk, S, `n,,UseErrorLevel ; Calculate rows
		Random, y, 1, ErrorLevel//2
		Random, x, 1, InStr(S, "`n")//2-1         ; Calculate height
	}

	; --Obtain a list of its neighbors...
	neighbors := x "," y+1 "`n" x "," y-1 "`n" x+1 "," y "`n" x-1 "," y
	; --Randomize the list...
	Sort neighbors, random

	; --Then for each neighbor...
	Loop Parse, neighbors, `n
	{
		pC := InStr(A_LoopField, ","), x2 := SubStr(A_LoopField, 1, pC-1), y2 := SubStr(A_LoopField, pC+1)
		; If it has not been visited...
		If GetChar(S, 2*x2, 2*y2) = "0"{
			; Mark it as visited...
			S := ChangeChar(s, 2*x2, 2*y2, " ")
			; Remove the wall between this cell and the neighbor...
			S := ChangeChar(S, x+x2, y+y2, " ")
			; Then recurse with the neighbor as the current cell
			S := Walk(S, x2, y2)
		}
	}
	return S
}

; Change a character in a string using x and y coordinates
ChangeChar(s, x, y, c){
	Loop Parse, s, `n
	{
		If (A_Index = Y)
			Loop Parse, A_LoopField
				If (A_Index = x)
					out .= c
				Else	out .= A_LoopField
		Else out .= A_LoopField
		out .= "`n"
	}
	StringTrimRight, out, out, 1
	return out
}

; retrieve a character in a string using x and y coordinates
GetChar(s, x, y, n=1){
	x*=n, y*=n
	Loop Parse, s, `n
		If (A_Index = Y)
			return SubStr(A_LoopField, x, 1)
}
```

```txt
+-+-+-+-+-+-+-+-+-+-+-+
|         |     |     |
+-+ +-+-+ +-+ + + +-+-+
|   |         | |     |
+ +-+ +-+ +-+-+ +-+ + +
| |     | |   |   | | |
+ + +-+-+ + + +-+ +-+ +
| |   |   | |     |   |
+ +-+ + +-+-+-+ +-+ + +
| |   |       |     | |
+ +-+-+-+-+-+ +-+-+-+ +
| |   |       |   |   |
+ + + + +-+-+-+ + + +-+
|   |   |   |   | |   |
+-+-+-+-+ +-+ + +-+-+ +
|             |       |
+-+-+-+-+-+-+-+-+-+-+-+
```


### Alternative Version

http://rosettacode.org/wiki/Maze_solving#AutoHotkey

Generator and solver combined.


## AWK



```awk
#!/usr/bin/awk -f

# Remember: AWK is 1-based, for better or worse.

BEGIN {
    # The maze dimensions.
    width = 20;  # Global
    height = 20; # Global
    resetMaze();

    # Some constants.
    top = 1;
    bottom = 2;
    left = 3;
    right = 4;

    # Randomize the PRNG.
    randomize();

    # Visit all the cells starting at a random point.
    visitCell(getRandX(), getRandY());

    # Show the result.
    printMaze();
}

# Wander through the maze removing walls as we go.
function visitCell(x, y,    dirList, dir, nx, ny, ndir, pi) {
    setVisited(x, y);   # This cell has been visited.

    # Visit neighbors in a random order.
    dirList = getRandDirList();
    for (dir = 1; dir <= 4; dir++) {
        # Get coordinates of a random neighbor (next in random direction list).
        ndir = substr(dirList, dir, 1);
        nx = getNextX(x, ndir);
        ny = getNextY(y, ndir);

        # Visit an unvisited neighbor, removing the separating walls.
        if (wasVisited(nx, ny) == 0) {
            rmWall(x, y, ndir);
            rmWall(nx, ny, getOppositeDir(ndir));
            visitCell(nx, ny)
        }
    }
}

# Display the text-mode maze.
function printMaze(    x, y, r, w) {
    for (y = 1; y <= height; y++) {
        for (pass = 1; pass <= 2; pass++) { # Go over each row twice: top, middle
            for (x = 1; x <= width; x++) {
                if (pass == 1) { # top
                    printf("+");
                    printf(hasWall(x, y, top) == 1 ? "---" : "   ");
                    if (x == width) printf("+");
                }
                else if (pass == 2) { # left, right
                    printf(hasWall(x, y, left) == 1 ? "|" : " ");
                    printf("   ");
                    if (x == width) printf(hasWall(x, y, right) == 1 ? "|" : " ");
                }
            }
            print;
        }
    }
    for (x = 1; x <= width; x++) printf("+---"); # bottom row
    print("+"); # bottom right corner
}

# Given a direction, get its opposite.
function getOppositeDir(d) {
    if (d == top) return bottom;
    if (d == bottom) return top;
    if (d == left) return right;
    if (d == right) return left;
}

# Build a list (string) of the four directions in random order.
function getRandDirList(    dirList, randDir, nx, ny, idx) {
    dirList = "";
    while (length(dirList) < 4) {
        randDir = getRandDir();
        if (!index(dirList, randDir)) {
            dirList = dirList randDir;
        }
    }
    return dirList;
}

# Get x coordinate of the neighbor in a given a direction.
function getNextX(x, dir) {
    if (dir == left) x = x - 1;
    if (dir == right) x = x + 1;
    if (!isGoodXY(x, 1)) return -1; # Off the edge.
    return x;
}

# Get y coordinate of the neighbor in a given a direction.
function getNextY(y, dir) {
    if (dir == top) y = y - 1;
    if (dir == bottom) y = y + 1;
    if (!isGoodXY(1, y)) return -1; # Off the edge.
    return y;
}

# Mark a cell as visited.
function setVisited(x, y,    cell) {
    cell = getCell(x, y);
    if (cell == -1) return;
    cell = substr(cell, 1, 4) "1"; # walls plus visited
    setCell(x, y, cell);
}

# Get the visited state of a cell.
function wasVisited(x, y,    cell) {
    cell = getCell(x, y);
    if (cell == -1) return 1; # Off edges already visited.
    return substr(getCell(x,y), 5, 1);
}

# Remove a cell's wall in a given direction.
function rmWall(x, y, d,    i, oldCell, newCell) {
    oldCell = getCell(x, y);
    if (oldCell == -1) return;
    newCell = "";
    for (i = 1; i <= 4; i++) { # Ugly as concat of two substrings and a constant?.
        newCell = newCell (i == d ? "0" : substr(oldCell, i, 1));
    }
    newCell = newCell wasVisited(x, y);
    setCell(x, y, newCell);
}

# Determine if a cell has a wall in a given direction.
function hasWall(x, y, d,    cell) {
    cell = getCell(x, y);
    if (cell == -1) return 1; # Cells off edge always have all walls.
    return substr(getCell(x, y), d, 1);
}

# Plunk a cell into the maze.
function setCell(x, y, cell,    idx) {
    if (!isGoodXY(x, y)) return;
    maze[x, y] = cell
}

# Get a cell from the maze.
function getCell(x, y,    idx) {
    if (!isGoodXY(x, y)) return -1; # Bad cell marker.
    return maze[x, y];
}

# Are the given coordinates in the maze?
function isGoodXY(x, y) {
    if (x < 1 || x > width) return 0;
    if (y < 1 || y > height) return 0;
    return 1;
}

# Build the empty maze.
function resetMaze(    x, y) {
    delete maze;
    for (y = 1; y <= height; y++) {
        for (x = 1; x <= width; x++) {
            maze[x, y] = "11110"; # walls (up, down, left, right) and visited state.
        }
    }
}

# Random things properly scaled.

function getRandX() {
    return 1 + int(rand() * width);
}

function getRandY() {
    return 1 +int(rand() * height);
}

function getRandDir() {
    return 1 + int(rand() * 4);
}

function randomize() {
    "echo $RANDOM" | getline t;
    srand(t);
}

```


Example output:

```txt

+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
|                       |                   |                       |           |
+---+   +---+   +---+---+   +---+   +---+---+   +---+   +---+---+   +   +---+   +
|       |   |   |           |   |           |       |   |   |       |       |   |
+   +---+   +   +   +---+---+   +---+---+   +   +---+   +   +   +---+---+---+   +
|       |       |   |                   |       |       |       |               |
+   +   +   +---+   +---+   +   +---+   +---+---+   +---+---+   +---+   +---+   +
|   |   |   |   |       |   |   |       |       |   |       |           |       |
+---+   +   +   +---+   +---+   +   +---+---+   +   +   +   +---+---+---+   +---+
|       |       |       |       |               |       |   |       |       |   |
+   +   +---+---+   +---+   +---+---+---+---+   +---+---+   +---+   +   +---+   +
|   |   |       |   |           |           |   |       |       |   |   |       |
+   +---+   +   +   +---+---+   +---+   +   +   +   +   +---+   +   +   +   +   +
|   |       |       |       |       |   |   |   |   |       |   |   |       |   |
+   +   +---+---+---+   +   +---+   +   +   +   +---+---+   +   +   +---+---+   +
|   |   |               |           |   |   |               |   |               |
+   +   +---+---+---+   +---+---+---+   +   +---+---+---+   +   +---+---+   +---+
|       |               |   |           |           |       |   |       |       |
+   +---+   +---+---+---+   +   +---+---+---+---+   +   +---+   +   +   +---+   +
|   |       |           |   |   |       |       |   |   |   |       |   |       |
+   +   +   +   +---+   +   +   +   +   +   +   +   +   +   +---+---+   +   +---+
|       |   |       |   |           |   |   |   |   |   |           |   |       |
+   +---+---+---+   +   +---+---+---+   +   +   +   +   +   +---+   +   +---+   +
|   |               |           |   |       |   |           |   |   |       |   |
+---+   +---+---+---+---+---+   +   +---+   +---+---+---+---+   +   +---+   +   +
|   |   |       |           |   |       |   |           |       |       |   |   |
+   +   +   +---+   +---+   +   +---+   +   +   +---+   +---+   +---+   +   +   +
|   |   |           |       |       |       |   |           |           |   |   |
+   +   +   +---+---+   +---+---+   +   +---+   +---+---+   +---+---+   +   +---+
|   |   |   |   |       |           |       |       |   |           |   |       |
+   +   +   +   +   +---+   +---+---+---+---+---+   +   +---+---+   +   +---+   +
|       |   |   |           |                       |               |       |   |
+---+---+   +   +---+---+---+---+   +   +---+---+---+   +---+---+---+---+   +   +
|       |       |               |   |       |       |           |           |   |
+   +   +---+   +---+---+   +   +   +---+   +   +   +---+---+   +---+---+---+   +
|   |       |       |       |   |       |   |   |   |       |           |       |
+   +   +---+---+   +   +---+   +   +---+   +---+   +   +   +---+---+   +   +---+
|   |           |   |   |       |   |       |       |   |           |   |       |
+   +---+   +---+   +   +   +---+---+   +---+   +---+   +---+---+   +   +---+   +
|       |               |               |                       |               |
+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

```



## Batch File

```dos
:amaze Rows Cols [wall char]
:: A stack-less, iterative, depth-first maze generator in native WinNT batch.
:: Rows and Cols must each be >1 and Rows*Cols cannot exceed 2096.
:: Default wall character is #, [wall char] is used if provided.

@ECHO OFF
SETLOCAL EnableDelayedExpansion

:: check for valid input, else GOTO :help
IF /I "%~2" EQU "" GOTO :amaze_help
FOR /F "tokens=* delims=0123456789" %%A IN ("%~1%~2") DO IF "%%~A" NEQ "" GOTO :amaze_help
SET /A "rows=%~1, cols=%~2, mTmp=rows*cols"
IF !rows! LSS 2    GOTO :amaze_help
IF !cols! LSS 2    GOTO :amaze_help
IF !mTmp! GTR 2096 GOTO :amaze_help

:: set map characters and use 1st character of %3 for wall, if defined
SET "wall=#"
SET "hall= "
SET "crumb=."
IF "%~3" NEQ "" SET "wall=%~3"
SET "wall=!wall:~0,1!"

:: assign width, height, cursor position, loop count, and offsets for NSEW
SET /A "cnt=0, wide=cols*2-1, high=rows*2-1, size=wide*high, N=wide*-2, S=wide*2, E=2, W=-2"

:: different random entrance points
:: ...on top
:: SET /A "start=(!RANDOM! %% cols)*2"
:: ...on bottom
:: SET /A "start=size-(!RANDOM! %% cols)*2-1"
:: ...on top or bottom
:: SET /A ch=cols*2, ch=!RANDOM! %% ch
:: IF !ch! GEQ !cols! ( SET /A "start=size-(ch-cols)*2-1"
:: ) ELSE SET /A start=ch*2
:: random entrance inside maze
SET /A "start=(!RANDOM! %% cols*2)+(!RANDOM! %% rows*2)*wide"
SET /A "curPos=start, cTmp=curPos+1, loops=cols*rows*2+1"

:: fill the maze with 8186 wall characters, clip to size, and open 1st cell
SET "mz=!wall!"
FOR /L %%A IN (1,1,6) DO SET mz=!mz!!mz!!mz!!mz!
SET bdr=!mz:~-%wide%!
SET mz=!mz:~3!!mz:~3!
SET mz=!mz:~-%size%!
SET mz=!mz:~0,%curPos%!!hall!!mz:~%cTmp%!

:: iterate #cells*2+1 steps of random depth-first search
FOR /L %%@ IN (1,1,%loops%) DO (
	SET "rand=" & SET "crmPos="
	REM set values for NSEW cell and wall positions
	SET /A "rCnt=rTmp=0, cTmp=curPos+1, np=curPos+N, sp=curPos+S, ep=curPos+E, wp=curPos+W, wChk=curPos/wide*wide, eChk=wChk+wide, nw=curPos-wide, sw=curPos+wide, ew=curPos+1, ww=curPos-1"
	REM examine adjacent cells, build direction list, and find last crumb position
	FOR /F "tokens=1-8" %%A IN ("!np! !sp! !ep! !wp! !nw! !sw! !ew! !ww!") DO (
		IF !np! GEQ 0 IF "!mz:~%%A,1!" EQU "!wall!" ( SET /A rCnt+=1 & SET "rand=n !rand!"
		) ELSE IF "!mz:~%%E,1!" EQU "!crumb!" SET /A crmPos=np, cw=nw
		IF !sp! LEQ !size! IF "!mz:~%%B,1!" EQU "!wall!" ( SET /A rCnt+=1 & SET "rand=s !rand!"
		) ELSE IF "!mz:~%%F,1!" EQU "!crumb!" SET /A crmPos=sp, cw=sw
		IF !ep! LEQ !eChk! IF "!mz:~%%C,1!" EQU "!wall!" ( SET /A rCnt+=1 & SET "rand=e !rand!"
		) ELSE IF "!mz:~%%G,1!" EQU "!crumb!" SET /A crmPos=ep, cw=ew
		IF !wp! GEQ !wChk! IF "!mz:~%%D,1!" EQU "!wall!" ( SET /A rCnt+=1 & SET "rand=w !rand!"
		) ELSE IF "!mz:~%%H,1!" EQU "!crumb!" SET /A crmPos=wp, cw=ww
	)
	IF DEFINED rand ( REM adjacent unvisited cell is available
		SET /A rCnt=!RANDOM! %% rCnt
		FOR %%A IN (!rand!) DO ( REM pick random cell + wall
			IF !rTmp! EQU !rCnt! SET /A "curPos=!%%Ap!, cTmp=curPos+1, mw=!%%Aw!, mTmp=mw+1"
			SET /A rTmp+=1
		)
		REM write the 2 new characters into the maze
		FOR /F "tokens=1-4" %%A IN ("!mw! !mTmp! !curPos! !cTmp!") DO (
			SET "mz=!mz:~0,%%A!!crumb!!mz:~%%B!"
			SET "mz=!mz:~0,%%C!!hall!!mz:~%%D!"
		)
	) ELSE IF DEFINED crmPos ( REM follow the crumbs backward
		SET /A mTmp=cw+1
		REM erase the crumb character and set new cursor position
		FOR /F "tokens=1-2" %%A IN ("!cw! !mTmp!") DO SET "mz=!mz:~0,%%A!!hall!!mz:~%%B!"
		SET "curPos=!crmPos!"
	)
)
SET /A open=cols/2*2, mTmp=open+1
ECHO !wall!!bdr:~0,%open%!!hall!!bdr:~%mTmp%!!wall!
FOR /L %%A IN (0,!wide!,!size!) DO IF %%A LSS !size! ECHO !wall!!mz:~%%A,%wide%!!wall!
ECHO !wall!!bdr:~0,%open%!!hall!!bdr:~%mTmp%!!wall!
ENDLOCAL
EXIT /B 0

:amaze_help
ECHO Usage:   %~0 Rows Cols [wall char]
ECHO          Rows^>1, Cols^>1, and Rows*Cols^<=2096
ECHO Example: %~0 11 39 @
ENDLOCAL
EXIT /B 0

```


Example output:

```txt

@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
@                   @     @   @         @     @     @       @   @         @   @
@ @@@@@@@@@@@ @@@@@@@ @ @ @ @ @ @@@@@ @ @ @ @ @@@ @ @@@ @@@ @ @@@ @ @@@@@ @@@ @
@ @       @   @       @ @ @ @   @     @ @ @ @ @   @     @     @   @ @         @
@ @ @@@@@@@ @@@ @@@@@@@ @ @ @@@@@ @@@@@ @@@ @ @ @@@@@@@@@@@@@@@ @@@ @@@@@@@@@@@
@ @ @     @ @   @   @   @ @ @   @   @       @   @   @         @ @ @     @     @
@ @ @ @@@ @ @ @@@@@ @ @ @@@ @ @ @@@ @@@@@@@ @@@@@ @ @ @@@@@@@ @ @ @@@@@ @ @@@ @
@   @   @ @ @     @ @ @ @   @ @   @   @   @ @     @ @ @ @     @   @   @     @ @
@@@ @@@ @ @ @@@@@ @ @ @ @ @@@ @ @ @@@ @ @ @@@ @@@@@ @ @ @ @@@ @@@ @ @@@@@@@@@ @
@     @ @ @     @   @ @ @   @ @ @   @   @       @   @   @ @   @   @       @   @
@ @@@@@ @ @@@ @ @@@ @ @@@@@ @ @ @@@@@@@@@@@@@@@@@ @@@@@ @ @@@@@ @@@ @@@@@ @ @@@
@   @   @ @   @   @ @ @     @ @ @   @       @   @   @   @ @     @ @ @   @ @   @
@@@@@ @@@ @ @ @@@@@ @ @ @@@@@ @ @ @ @ @@@@@ @ @ @@@ @ @@@ @ @@@@@ @ @ @ @ @@@@@
@     @     @ @     @ @     @ @   @     @   @ @     @ @ @   @       @ @ @   @ @
@ @@@@@@@@@@@ @ @@@@@ @@@@@ @ @@@@@@@ @@@ @ @ @@@@@ @ @ @@@@@@@@@ @@@ @@@ @ @ @
@ @         @ @     @     @ @   @   @ @   @ @     @ @ @         @ @   @   @ @ @
@ @ @@@@@@@ @@@@@@@ @@@ @@@ @@@ @ @ @@@ @@@ @@@@@ @@@ @@@@@@@ @ @ @ @ @ @@@ @ @
@ @     @ @       @ @   @   @   @ @ @   @   @ @   @   @     @ @ @   @ @ @ @ @ @
@ @@@@@ @ @@@ @@@@@ @ @ @ @@@@@@@ @ @ @@@ @@@ @ @@@ @@@ @@@ @ @ @@@@@@@ @ @ @ @
@       @   @       @ @ @   @   @ @   @ @ @   @ @   @   @   @ @ @       @ @   @
@ @@@@@@@ @ @@@@@@@@@ @@@@@ @ @ @ @@@@@ @ @ @ @ @ @@@ @@@ @@@ @ @ @@@@@@@ @@@ @
@         @         @         @   @         @ @       @       @   @           @
@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

```



## BASIC

This implementation was written using QB64. It should also be compatible with Qbasic, as it uses no QB64-exclusive features.

```BASIC
OPTION BASE 0
RANDOMIZE TIMER

REM must be even
width% = 40
height% = 20

REM make array and fill
DIM maze$(width%, height%)
FOR x% = 0 TO width%
    FOR y% = 0 TO height%
        maze$(x%, y%) = "#"
    NEXT y%
NEXT x%

REM initial start location
currentx% = INT(RND * (width% - 1))
currenty% = INT(RND * (height% - 1))
REM value must be odd
IF currentx% MOD 2 = 0 THEN currentx% = currentx% + 1
IF currenty% MOD 2 = 0 THEN currenty% = currenty% + 1
maze$(currentx%, currenty%) = " "

REM generate maze
done% = 0
DO WHILE done% = 0
    FOR i% = 0 TO 99
        oldx% = currentx%
        oldy% = currenty%

        REM move in random direction
        SELECT CASE INT(RND * 4)
            CASE 0
                IF currentx% + 2 < width% THEN currentx% = currentx% + 2
            CASE 1
                IF currenty% + 2 < height% THEN currenty% = currenty% + 2
            CASE 2
                IF currentx% - 2 > 0 THEN currentx% = currentx% - 2
            CASE 3
                IF currenty% - 2 > 0 THEN currenty% = currenty% - 2
        END SELECT

        REM if cell is unvisited then connect it
        IF maze$(currentx%, currenty%) = "#" THEN
            maze$(currentx%, currenty%) = " "
            maze$(INT((currentx% + oldx%) / 2), ((currenty% + oldy%) / 2)) = " "
        END IF
    NEXT i%

    REM check if all cells are visited
    done% = 1
    FOR x% = 1 TO width% - 1 STEP 2
        FOR y% = 1 TO height% - 1 STEP 2
            IF maze$(x%, y%) = "#" THEN done% = 0
        NEXT y%
    NEXT x%
LOOP

REM draw maze
FOR y% = 0 TO height%
    FOR x% = 0 TO width%
        PRINT maze$(x%, y%);
    NEXT x%
    PRINT
NEXT y%

REM wait
DO: LOOP WHILE INKEY$ = ""
```


This used a slightly modified version that outputs to a text file. (You can't copy from a QB64 window.)

```txt
#########################################
# #   #     #     #   #   #       # #   #
# ### # # # # ##### # # ### # # # # # ###
# #   # # #   #   # #     # # # #     # #
# # # ####### # ####### ####### ##### # #
#   #             # #     #     #   #   #
# ##### ### ### # # ### # # ##### ### ###
#     # # # #   #   # # #     # # #     #
### ##### # ##### ### ### # ### # # #####
#       # #   #     #     # # #   # #   #
# # # ### # # ##### ### # # # # ##### ###
# # # #     # #         # #     # #     #
# ### # # ######### ### ####### # ##### #
# #   # # #   # #     #   # # # #   # # #
# ### ####### # ### # ##### # # ### # # #
#   #         # #   # # #     #       # #
##### # ### ### ### ### # # # # # # # # #
#     # #   #     #   #   # #   # # #   #
# # # # ### # ### ### ### # ### ### ### #
# # # #   #   #   #   #   #   #   #   # #
#########################################
```



## BBC BASIC

```bbcbasic
      MazeWidth% = 11
      MazeHeight% = 9
      MazeCell% = 50

      VDU 23,22,MazeWidth%*MazeCell%/2+3;MazeHeight%*MazeCell%/2+3;8,16,16,128
      VDU 23,23,3;0;0;0; : REM Line thickness
      PROCgeneratemaze(Maze&(), MazeWidth%, MazeHeight%, MazeCell%)
      END

      DEF PROCgeneratemaze(RETURN m&(), w%, h%, s%)
      LOCAL x%, y%
      DIM m&(w%, h%)
      FOR y% = 0 TO h%
        LINE 0,y%*s%,w%*s%,y%*s%
      NEXT
      FOR x% = 0 TO w%
        LINE x%*s%,0,x%*s%,h%*s%
      NEXT
      GCOL 15
      PROCcell(m&(), RND(w%)-1, y% = RND(h%)-1, w%, h%, s%)
      ENDPROC

      DEF PROCcell(m&(), x%, y%, w%, h%, s%)
      LOCAL i%, p%, q%, r%
      m&(x%,y%) OR= &40 : REM Mark visited
      r% = RND(4)
      FOR i% = r% TO r%+3
        CASE i% MOD 4 OF
          WHEN 0: p% = x%-1 : q% = y%
          WHEN 1: p% = x%+1 : q% = y%
          WHEN 2: p% = x% : q% = y%-1
          WHEN 3: p% = x% : q% = y%+1
        ENDCASE
        IF p% >= 0 IF p% < w% IF q% >= 0 IF q% < h% IF m&(p%,q%) < &40 THEN
          IF p% > x% m&(p%,q%) OR= 1 : LINE p%*s%,y%*s%+4,p%*s%,(y%+1)*s%-4
          IF q% > y% m&(p%,q%) OR= 2 : LINE x%*s%+4,q%*s%,(x%+1)*s%-4,q%*s%
          IF x% > p% m&(x%,y%) OR= 1 : LINE x%*s%,y%*s%+4,x%*s%,(y%+1)*s%-4
          IF y% > q% m&(x%,y%) OR= 2 : LINE x%*s%+4,y%*s%,(x%+1)*s%-4,y%*s%
          PROCcell(m&(), p%, q%, w%, h%, s%)
        ENDIF
      NEXT
      ENDPROC
```

'''Sample output:'''


[[File:maze_bbc.gif]]


## Befunge

Dimensions are specified by the first two values pushed onto the stack - currently 20 (45*) by 16 (28*). Note, however, that the upper limit in a standard Befunge-93 implementation will be around 38 by 40 (1520 cells) due to the constrained page size.

Also note that this requires an interpreter with working read-write memory support, which is suprisingly rare in online implementations. Padding the code page with extra blank lines or spaces can sometimes help. Using smaller dimensions might also be preferable, especially on slower implementations.


```befunge
45*28*10p00p020p030p006p0>20g30g00g*+::"P"%\"P"/6+gv>$\1v@v1::\+g02+*g00+g03-\<
0_ 1!%4+1\-\0!::\-\2%2:p<pv0<< v0p+6/"P"\%"P":\+4%4<^<v-<$>+2%\1-*20g+\1+4%::v^
#| +2%\1-*30g+\1\40g1-:v0+v2?1#<v>+:00g%!55+*>:#0>#,_^>:!|>\#%"P"v#:*+*g00g0<>1
02!:++`\0\`-1g01:\+`\< !46v3<^$$<^1,g2+1%2/2,g1+1<v%g00:\<*g01,<>:30p\:20p:v^3g
0#$g#<1#<-#<`#<\#<0#<^#_^/>#1+#4<>"P"%\"P"/6+g:2%^!>,1-:#v_$55+^|$$ "JH" $$>#<0
::"P"%\"P"/6+g40p\40g+\:#^"P"%#\<^ ::$_,#!0#:<*"|"<^," _"<:g000 <> /6+g4/2%+#^_

```


```txt
 _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
|_  |  _ _  |_     _ _|  _ _   _ _  |_  |
| |_ _|  _|_  |_|_  |  _| |  _|_  |_  | |
|   |_   _  | |   |_ _   _| |  _ _ _|_  |
| |_  |_|  _|_ _|_  |_ _|  _|_  |    _| |
| |_ _ _  |   |  _ _|   |  _  | | | |  _|
|  _ _  |_| | | |  _ _| |_|  _|  _|_ _| |
| | |  _|  _|_ _|_  |_ _  | | | |  _ _  |
|_  |_ _ _|  _ _  |_ _  |_ _| | |_  | | |
| |_ _ _ _ _|   |_ _ _ _| |   |_  | | | |
|_    |  _ _ _|_ _ _ _ _  | |_  |_ _| | |
|  _|_ _|_ _   _    |  _ _|_  |_   _ _| |
| |  _ _ _  | |  _|_|_ _ _  |_  |_ _  | |
|  _|  _  | | |_ _ _ _ _   _ _| | |   | |
|_  |_  |  _|_|   |  _  |_|  _ _ _| | | |
| | | | |_|  _ _| | |  _|  _|   |  _|_| |
|_ _ _|_ _ _ _ _|_ _|_ _ _ _ _|_|_ _ _ _|
```



## C

Generation/solver in one. Requires UTF8 locale and unicode capable console.  If your console font line-drawing chars are single width, define DOUBLE_SPACE to 0.

```c
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <locale.h>

#define DOUBLE_SPACE 1

#if DOUBLE_SPACE
#	define SPC "　"
#else
#	define SPC " "
#endif

wchar_t glyph[] = L""SPC"│││─┘┐┤─└┌├─┴┬┼"SPC"┆┆┆┄╯╮ ┄╰╭ ┄";

typedef unsigned char byte;
enum { N = 1, S = 2, W = 4, E = 8, V = 16 };

byte **cell;
int w, h, avail;
#define each(i, x, y) for (i = x; i <= y; i++)

int irand(int n)
{
	int r, rmax = n * (RAND_MAX / n);
	while ((r = rand()) >= rmax);
	return r / (RAND_MAX/n);
}

void show()
{
	int i, j, c;
	each(i, 0, 2 * h) {
		each(j, 0, 2 * w) {
			c = cell[i][j];
			if (c > V) printf("\033[31m");
			printf("%lc", glyph[c]);
			if (c > V) printf("\033[m");
		}
		putchar('\n');
	}
}

inline int max(int a, int b) { return a >= b ? a : b; }
inline int min(int a, int b) { return b >= a ? a : b; }

static int dirs[4][2] = {{-2, 0}, {0, 2}, {2, 0}, {0, -2}};
void walk(int x, int y)
{
	int i, t, x1, y1, d[4] = { 0, 1, 2, 3 };

	cell[y][x] |= V;
	avail--;

	for (x1 = 3; x1; x1--)
		if (x1 != (y1 = irand(x1 + 1)))
			i = d[x1], d[x1] = d[y1], d[y1] = i;

	for (i = 0; avail && i < 4; i++) {
		x1 = x + dirs[ d[i] ][0], y1 = y + dirs[ d[i] ][1];

		if (cell[y1][x1] & V) continue;

		/* break walls */
		if (x1 == x) {
			t = (y + y1) / 2;
			cell[t][x+1] &= ~W, cell[t][x] &= ~(E|W), cell[t][x-1] &= ~E;
		} else if (y1 == y) {
			t = (x + x1)/2;
			cell[y-1][t] &= ~S, cell[y][t] &= ~(N|S), cell[y+1][t] &= ~N;
		}
		walk(x1, y1);
	}
}

int solve(int x, int y, int tox, int toy)
{
	int i, t, x1, y1;

	cell[y][x] |= V;
	if (x == tox && y == toy) return 1;

	each(i, 0, 3) {
		x1 = x + dirs[i][0], y1 = y + dirs[i][1];
		if (cell[y1][x1]) continue;

		/* mark path */
		if (x1 == x) {
			t = (y + y1)/2;
			if (cell[t][x] || !solve(x1, y1, tox, toy)) continue;

			cell[t-1][x] |= S, cell[t][x] |= V|N|S, cell[t+1][x] |= N;
		} else if (y1 == y) {
			t = (x + x1)/2;
			if (cell[y][t] || !solve(x1, y1, tox, toy)) continue;

			cell[y][t-1] |= E, cell[y][t] |= V|E|W, cell[y][t+1] |= W;
		}
		return 1;
	}

	/* backtrack */
	cell[y][x] &= ~V;
	return 0;
}

void make_maze()
{
	int i, j;
	int h2 = 2 * h + 2, w2 = 2 * w + 2;
	byte **p;

	p = calloc(sizeof(byte*) * (h2 + 2) + w2 * h2 + 1, 1);

	p[1] = (byte*)(p + h2 + 2) + 1;
	each(i, 2, h2) p[i] = p[i-1] + w2;
	p[0] = p[h2];
	cell = &p[1];

	each(i, -1, 2 * h + 1) cell[i][-1] = cell[i][w2 - 1] = V;
	each(j, 0, 2 * w) cell[-1][j] = cell[h2 - 1][j] = V;
	each(i, 0, h) each(j, 0, 2 * w) cell[2*i][j] |= E|W;
	each(i, 0, 2 * h) each(j, 0, w) cell[i][2*j] |= N|S;
	each(j, 0, 2 * w) cell[0][j] &= ~N, cell[2*h][j] &= ~S;
	each(i, 0, 2 * h) cell[i][0] &= ~W, cell[i][2*w] &= ~E;

	avail = w * h;
	walk(irand(2) * 2 + 1, irand(h) * 2 + 1);

	/* reset visited marker (it's also used by path finder) */
	each(i, 0, 2 * h) each(j, 0, 2 * w) cell[i][j] &= ~V;
	solve(1, 1, 2 * w - 1, 2 * h - 1);

	show();
}

int main(int c, char **v)
{
	setlocale(LC_ALL, "");
	if (c < 2 || (w = atoi(v[1])) <= 0) w = 16;
	if (c < 3 || (h = atoi(v[2])) <= 0) h = 8;

	make_maze();

	return 0;
}
```

```txt
┌───┬─────┬─────────┬───────┬───┐
│┄┄╮│╭┄┄┄╮│　　╭┄┄┄┄┄╮│　　╭┄┄┄╮│╭┄╮│
│　│┆│┆──┐┆│　│┆──┬─┐┆└──┆┌─┐┆│┆│┆│
│　│┆│╰┄╮│┆│　│╰┄╮│　│╰┄┄┄╯│　│╰┄╯│┆│
│　│┆└──┆│┆└─┼──┆│　└─────┤　└─┬─┘┆│
│　│╰┄┄┄╯│╰┄╮│╭┄╯│　　　　　　　│　　　│╭┄╯│
│　└─────┴─┐┆│┆┌─┴───┐　│　│　│　│┆──┤
│　　　　　　　　　│┆│┆│╭┄┄┄╮│　│　　　│　│╰┄╮│
│　──────┐　│┆│┆│┆──┐┆└─┤　┌─┘　└─┐┆│
│　　　　　　　│　│┆│╰┄╯　　│╰┄╮│　│　　　　　│┆│
│　┌─────┘　│┆├─────┴─┐┆│　│　──┬─┘┆│
│　│　　　　　　　│┆│╭┄┄┄┄┄╮│┆│　│　　　│╭┄╯│
├─┤　──┬─┬─┘┆│┆┌─┬──┆│┆└─┴─┐　│┆┌─┤
│　│　　　│　│╭┄╯│┆│　│╭┄╯│╰┄┄┄╮│　│┆│　│
│　└──　│　│┆──┘┆│　│┆──┴────┆│　│┆│　│
│　　　　　│　　╰┄┄┄╯│　　╰┄┄┄┄┄┄┄╯│　　╰┄┄│
└─────┴───────┴───────────┴─────┘
```


## C++

[[File:maze_cpp.png|300px]]

```cpp

#include <windows.h>
#include <iostream>
#include <string>

//--------------------------------------------------------------------------------------------------
using namespace std;

//--------------------------------------------------------------------------------------------------
const int BMP_SIZE = 512, CELL_SIZE = 8;

//--------------------------------------------------------------------------------------------------
enum directions { NONE, NOR = 1, EAS = 2, SOU = 4, WES = 8 };

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
    void    *pBits;
    int	    width, height;
};
//--------------------------------------------------------------------------------------------------
class mazeGenerator
{
public:
    mazeGenerator()
    {
	_world = 0;
	_bmp.create( BMP_SIZE, BMP_SIZE );
	_bmp.setPenColor( RGB( 0, 255, 0 ) );
    }

    ~mazeGenerator() { killArray(); }

    void create( int side )
    {
	_s = side;
	generate();
	display();
    }

private:
    void generate()
    {
	killArray();
	_world = new BYTE[_s * _s];
	ZeroMemory( _world, _s * _s );
	_ptX = rand() % _s; _ptY = rand() % _s;
	carve();
    }

    void carve()
    {
	while( true )
	{
	    int d = getDirection();
	    if( d < NOR ) return;

	    switch( d )
	    {
	        case NOR:
	            _world[_ptX + _s * _ptY] |= NOR; _ptY--;
		    _world[_ptX + _s * _ptY] = SOU | SOU << 4;
		break;
	        case EAS:
		    _world[_ptX + _s * _ptY] |= EAS; _ptX++;
		    _world[_ptX + _s * _ptY] = WES | WES << 4;
		break;
		case SOU:
		    _world[_ptX + _s * _ptY] |= SOU; _ptY++;
		    _world[_ptX + _s * _ptY] = NOR | NOR << 4;
		break;
		case WES:
		    _world[_ptX + _s * _ptY] |= WES; _ptX--;
		    _world[_ptX + _s * _ptY] = EAS | EAS << 4;
	    }
	}
    }

    void display()
    {
	_bmp.clear();
	HDC dc = _bmp.getDC();
	for( int y = 0; y < _s; y++ )
	{
	    int yy = y * _s;
	    for( int x = 0; x < _s; x++ )
	    {
		BYTE b = _world[x + yy];
		int nx = x * CELL_SIZE,
		    ny = y * CELL_SIZE;

		if( !( b & NOR ) )
		{
		    MoveToEx( dc, nx, ny, NULL );
		    LineTo( dc, nx + CELL_SIZE + 1, ny );
		}
		if( !( b & EAS ) )
		{
		    MoveToEx( dc, nx + CELL_SIZE, ny, NULL );
		    LineTo( dc, nx + CELL_SIZE, ny + CELL_SIZE + 1 );
		}
		if( !( b & SOU ) )
		{
		    MoveToEx( dc, nx, ny + CELL_SIZE, NULL );
		    LineTo( dc, nx + CELL_SIZE + 1, ny + CELL_SIZE );
		}
		if( !( b & WES ) )
		{
		    MoveToEx( dc, nx, ny, NULL );
		    LineTo( dc, nx, ny + CELL_SIZE + 1 );
		}
	    }
	}

	//_bmp.saveBitmap( "f:\\rc\\maze.bmp" );
	BitBlt( GetDC( GetConsoleWindow() ), 10, 60, BMP_SIZE, BMP_SIZE, _bmp.getDC(), 0, 0, SRCCOPY );
    }

    int getDirection()
    {
	int d = 1 << rand() % 4;
	while( true )
	{
	    for( int x = 0; x < 4; x++ )
	    {
		if( testDir( d ) ) return d;
		d <<= 1;
		if( d > 8 ) d = 1;
	    }
	    d = ( _world[_ptX + _s * _ptY] & 0xf0 ) >> 4;
	    if( !d ) return -1;
	    switch( d )
	    {
		case NOR: _ptY--; break;
		case EAS: _ptX++; break;
		case SOU: _ptY++; break;
		case WES: _ptX--; break;
	    }
            d = 1 << rand() % 4;
	}
    }

    bool testDir( int d )
    {
	switch( d )
	{
	    case NOR: return ( _ptY - 1 > -1 && !_world[_ptX + _s * ( _ptY - 1 )] );
	    case EAS: return ( _ptX + 1 < _s && !_world[_ptX + 1 + _s * _ptY] );
	    case SOU: return ( _ptY + 1 < _s && !_world[_ptX + _s * ( _ptY + 1 )] );
	    case WES: return ( _ptX - 1 > -1 && !_world[_ptX - 1 + _s * _ptY] );
	}
	return false;
    }

    void killArray() { if( _world ) delete [] _world; }

    BYTE*    _world;
    int      _s, _ptX, _ptY;
    myBitmap _bmp;
};
//--------------------------------------------------------------------------------------------------
int main( int argc, char* argv[] )
{
    ShowWindow( GetConsoleWindow(), SW_MAXIMIZE );
    srand( GetTickCount() );

    mazeGenerator mg;
    int s;
    while( true )
    {
	cout << "Enter the maze size, an odd number bigger than 2 ( 0 to QUIT ): "; cin >> s;
	if( !s ) return 0;
	if( !( s & 1 ) ) s++;
	if( s >= 3 ) mg.create( s );
	cout << endl;
	system( "pause" );
	system( "cls" );
    }
    return 0;
}
//--------------------------------------------------------------------------------------------------

```


## C#


```c#
using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Text;
using System.Drawing;

namespace MazeGeneration
{
    public static class Extensions
    {
        public static IEnumerable<T> Shuffle<T>(this IEnumerable<T> source, Random rng)
        {
            var e = source.ToArray();
            for (var i = e.Length - 1; i >= 0; i--)
            {
                var swapIndex = rng.Next(i + 1);
                yield return e[swapIndex];
                e[swapIndex] = e[i];
            }
        }

        public static CellState OppositeWall(this CellState orig)
        {
            return (CellState)(((int) orig >> 2) | ((int) orig << 2)) & CellState.Initial;
        }

        public static bool HasFlag(this CellState cs,CellState flag)
        {
            return ((int)cs & (int)flag) != 0;
        }
    }

    [Flags]
    public enum CellState
    {
        Top = 1,
        Right = 2,
        Bottom = 4,
        Left = 8,
        Visited = 128,
        Initial = Top | Right | Bottom | Left,
    }

    public struct RemoveWallAction
    {
        public Point Neighbour;
        public CellState Wall;
    }

    public class Maze
    {
        private readonly CellState[,] _cells;
        private readonly int _width;
        private readonly int _height;
        private readonly Random _rng;

        public Maze(int width, int height)
        {
            _width = width;
            _height = height;
            _cells = new CellState[width, height];
            for(var x=0; x<width; x++)
                for(var y=0; y<height; y++)
                    _cells[x, y] = CellState.Initial;
            _rng = new Random();
            VisitCell(_rng.Next(width), _rng.Next(height));
        }

        public CellState this[int x, int y]
        {
            get { return _cells[x,y]; }
            set { _cells[x,y] = value; }
        }

        public IEnumerable<RemoveWallAction> GetNeighbours(Point p)
        {
            if (p.X > 0) yield return new RemoveWallAction {Neighbour = new Point(p.X - 1, p.Y), Wall = CellState.Left};
            if (p.Y > 0) yield return new RemoveWallAction {Neighbour = new Point(p.X, p.Y - 1), Wall = CellState.Top};
            if (p.X < _width-1) yield return new RemoveWallAction {Neighbour = new Point(p.X + 1, p.Y), Wall = CellState.Right};
            if (p.Y < _height-1) yield return new RemoveWallAction {Neighbour = new Point(p.X, p.Y + 1), Wall = CellState.Bottom};
        }

        public void VisitCell(int x, int y)
        {
            this[x,y] |= CellState.Visited;
            foreach (var p in GetNeighbours(new Point(x, y)).Shuffle(_rng).Where(z => !(this[z.Neighbour.X, z.Neighbour.Y].HasFlag(CellState.Visited))))
            {
                this[x, y] -= p.Wall;
                this[p.Neighbour.X, p.Neighbour.Y] -= p.Wall.OppositeWall();
                VisitCell(p.Neighbour.X, p.Neighbour.Y);
            }
        }

        public void Display()
        {
            var firstLine = string.Empty;
            for (var y = 0; y < _height; y++)
            {
                var sbTop = new StringBuilder();
                var sbMid = new StringBuilder();
                for (var x = 0; x < _width; x++)
                {
                    sbTop.Append(this[x, y].HasFlag(CellState.Top) ? "+--" : "+  ");
                    sbMid.Append(this[x, y].HasFlag(CellState.Left) ? "|  " : "   ");
                }
                if (firstLine == string.Empty)
                    firstLine = sbTop.ToString();
                Debug.WriteLine(sbTop + "+");
                Debug.WriteLine(sbMid + "|");
                Debug.WriteLine(sbMid + "|");
            }
            Debug.WriteLine(firstLine);
        }
    }

    class Program
    {
        static void Main(string[] args)
        {
            var maze = new Maze(20, 20);
            maze.Display();
        }
    }
}

```

Sample output:

```txt

+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
|        |     |     |        |        |     |        |     |
|        |     |     |        |        |     |        |     |
+  +--+  +  +  +  +  +--+  +  +--+  +  +  +  +  +--+  +  +--+
|  |     |  |     |        |     |  |     |  |  |     |     |
|  |     |  |     |        |     |  |     |  |  |     |     |
+  +--+  +  +--+--+--+--+--+--+  +  +--+--+  +  +  +--+--+  +
|     |     |           |        |     |     |  |        |  |
|     |     |           |        |     |     |  |        |  |
+  +  +--+--+  +  +--+--+  +--+--+--+  +  +--+  +--+--+  +  +
|  |  |        |     |     |        |  |        |     |     |
|  |  |        |     |     |        |  |        |     |     |
+  +  +--+--+--+  +  +  +--+  +--+  +  +--+  +--+  +  +--+--+
|  |  |        |  |     |     |  |  |     |  |     |        |
|  |  |        |  |     |     |  |  |     |  |     |        |
+  +  +  +--+  +--+--+  +  +  +  +  +--+  +  +  +--+--+--+  +
|  |  |  |  |        |  |  |  |           |  |  |     |     |
|  |  |  |  |        |  |  |  |           |  |  |     |     |
+  +  +  +  +--+--+  +  +--+  +--+--+--+--+  +  +  +  +  +  +
|  |     |  |        |     |        |        |  |  |  |  |  |
|  |     |  |        |     |        |        |  |  |  |  |  |
+  +--+--+  +  +--+--+--+  +--+--+  +  +--+--+  +--+  +  +--+
|           |     |     |     |     |  |              |     |
|           |     |     |     |     |  |              |     |
+--+--+--+  +--+  +  +--+--+  +  +--+  +  +--+--+--+--+--+  +
|              |  |           |  |     |  |     |           |
|              |  |           |  |     |  |     |           |
+  +--+--+--+--+  +--+--+  +--+  +--+--+  +  +--+  +--+--+--+
|  |     |     |        |     |        |  |     |  |  |     |
|  |     |     |        |     |        |  |     |  |  |     |
+  +--+  +  +  +--+--+  +--+  +--+--+  +  +--+  +--+  +  +  +
|     |  |  |        |     |        |  |     |     |     |  |
|     |  |  |        |     |        |  |     |     |     |  |
+--+  +  +  +--+--+  +--+  +--+--+  +  +--+  +--+  +  +--+  +
|     |  |        |     |     |     |     |     |        |  |
|     |  |        |     |     |     |     |     |        |  |
+  +--+  +--+--+  +--+  +--+  +--+  +--+  +--+  +  +--+--+  +
|     |        |  |        |     |        |  |  |  |        |
|     |        |  |        |     |        |  |  |  |        |
+--+  +  +--+  +  +--+--+  +--+  +--+--+  +  +  +--+  +--+--+
|  |  |     |  |     |     |     |  |        |     |        |
|  |  |     |  |     |     |     |  |        |     |        |
+  +  +  +  +  +--+  +--+  +  +--+  +  +--+--+--+  +--+--+  +
|     |  |  |  |  |     |     |     |  |  |     |        |  |
|     |  |  |  |  |     |     |     |  |  |     |        |  |
+  +--+--+  +  +  +--+  +--+--+--+  +  +  +  +  +--+--+  +  +
|           |     |     |           |  |  |  |        |  |  |
|           |     |     |           |  |  |  |        |  |  |
+--+--+--+--+--+  +  +--+--+  +--+--+  +  +  +--+--+  +  +  +
|     |        |  |     |     |           |        |     |  |
|     |        |  |     |     |           |        |     |  |
+  +  +  +  +  +  +--+  +  +--+  +--+--+--+--+--+  +--+--+  +
|  |     |  |  |  |     |        |  |     |     |        |  |
|  |     |  |  |  |     |        |  |     |     |        |  |
+  +--+--+  +--+  +  +--+--+--+  +  +  +  +  +  +--+--+  +  +
|     |  |     |  |           |     |  |  |  |        |     |
|     |  |     |  |           |     |  |  |  |        |     |
+--+  +  +--+  +  +--+--+--+  +--+--+  +  +  +  +--+--+--+  +
|           |              |           |     |              |
|           |              |           |     |              |
+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--

```




## Clojure


```clojure
(ns maze.core
  (:require [clojure.set :refer [intersection
                                 select]]
            [clojure.string :as str]))

;; Misc functions
(defn neighborhood
  ([] (neighborhood [0 0]))
  ([coord] (neighborhood coord 1))
  ([[y x] r]
   (let [y-- (- y r) y++ (+ y r)
         x-- (- x r) x++ (+ x r)]
     #{[y++ x] [y-- x] [y x--] [y x++]})))

(defn cell-empty? [maze coords]
  (= :empty (get-in maze coords)))

(defn wall? [maze coords]
  (= :wall (get-in maze coords)))

(defn filter-maze
  ([pred maze coords]
   (select (partial pred maze) (set coords)))
  ([pred maze]
   (filter-maze
     pred
     maze
     (for [y (range (count maze))
           x (range (count (nth maze y)))]
       [y x]))))

(defn create-empty-maze [width height]
  (let [width (inc (* 2 width))
        height (inc (* 2 height))]
    (vec (take height
               (interleave
                 (repeat (vec (take width (repeat :wall))))
                 (repeat (vec (take width (cycle [:wall :empty])))))))))

(defn next-step [possible-steps]
  (rand-nth (vec possible-steps)))

;;Algo
(defn create-random-maze [width height]
  (loop [maze (create-empty-maze width height)
         stack []
         nonvisited (filter-maze cell-empty? maze)
         visited #{}
         coords (next-step nonvisited)]
    (if (empty? nonvisited)
      maze
      (let [nonvisited-neighbors (intersection (neighborhood coords 2) nonvisited)]
        (cond
          (seq nonvisited-neighbors)
          (let [next-coords (next-step nonvisited-neighbors)
                wall-coords (map #(+ %1 (/ (- %2 %1) 2)) coords next-coords)]
            (recur (assoc-in maze wall-coords :empty)
                   (conj stack coords)
                   (disj nonvisited next-coords)
                   (conj visited next-coords)
                   next-coords))

          (seq stack)
          (recur maze (pop stack) nonvisited visited (last stack)))))))

;;Conversion to string
(def cell-code->str
  ["  " "  " "  " "  " "· " "╵ " "╴ " "┘ "
   "  " "  " "  " "  " "╶─" "└─" "──" "┴─"
   "  " "  " "  " "  " "╷ " "│ " "┐ " "┤ "
   "  " "  " "  " "  " "┌─" "├─" "┬─" "┼─"])

(defn cell-code [maze coord]
  (transduce
    (comp
      (map (partial wall? maze))
      (keep-indexed (fn [idx el] (when el idx)))
      (map (partial bit-shift-left 1)))
    (completing bit-or)
    0
    (sort (cons coord (neighborhood coord)))))

(defn cell->str [maze coord]
  (get cell-code->str (cell-code maze coord)))

(defn maze->str [maze]
  (->> (for [y (range (count maze))]
         (for [x (range (count (nth maze y)))]
           (cell->str maze [y x])))
       (map str/join)
       (str/join \newline)))

;;Task
(println (maze->str (create-random-maze 10 10)))
```


```txt
┌───────────┬───────────────┬───────┬───┐
│           │               │       │   │
├───╴   ╷   ╵   ┌───────────┤   ╷   │   │
│       │       │           │   │   │   │
│   ╷   └───┐   │   ╶───┐   ╵   │   │   │
│   │       │   │       │       │   │   │
│   └───┐   └───┴───╴   ├───────┤   │   │
│       │               │       │   │   │
│   ╷   └───────────────┼───╴   │   ╵   │
│   │                   │       │       │
├───┴───┐   ┌───────┐   ╵   ╷   ├───╴   │
│       │   │       │       │   │       │
│   ╷   ╵   │   ╷   ╵   ┌───┴───┘   ┌───┤
│   │       │   │       │           │   │
│   └───────┴───┴───────┤   ╶───────┤   │
│                       │           │   │
│   ╶───────┬───────┐   └───┬───╴   │   │
│           │       │       │       │   │
├───────╴   ╵   ╷   │   ╶───┘   ╶───┘   │
│               │   │                   │
└───────────────┴───┴───────────────────┘
```



## Commodore BASIC

Written in Commodore BASIC V2 and tested on Commodore 64 and Commodore 128 hardware. (It will also run on the unexpanded Commodore VIC-20 if you reduce the maze size to 8x8.) Due to stack size limitations in the operating systems, this solution eschews recursive subroutine calls. Recursion is accomplished by conditional branching within the maze build routine and the use of an array-based stack for data elements.


```BASIC
100 MS=10:REM MAZE SIZE
110 DIM S(MS+1,MS+1):REM SOUTH WALLS
120 DIM W(MS+1,MS+1):REM WEST WALLS
130 DIM V(MS+1,MS+1):REM VISITED CELLS
140 PRINT "INITIALIZING..."
150 GOSUB 260:REM INITIALIZE MAZE
160 PRINT "BUILDING..."
170 DIM PC(MS*MS+1):DIM PR(MS*MS+1):REM STACK
180 REM PICK RANDOM STARTING CELL
190 X = RND(-TI)
200 C=(INT(RND(1)*MS)+1)
210 R=(INT(RND(1)*MS)+1)
220 GOSUB 400:REM BUILD MAZE
230 GOSUB 540:REM DRAW MAZE
240 END
250 REM -----INITIALIZE MAZE-----
260 REM SET WALLS ON AND VISITED CELLS OFF
270 T=MS+1
280 FOR C=0 TO T:FOR R=0 TO T:
290 S(C,R)=1:W(C,R)=1:V(C,R)=0
300 NEXT R:NEXT C
310 REM SET BORDER CELLS TO VISITED
320 FOR C=0 TO T
330 V(C,0)=1:V(C,T)=1
340 NEXT C
350 FOR R=0 TO T
360 V(0,R)=1:V(T,R)=1
370 NEXT R
380 RETURN
390 REM -----BUILD MAZE-----
400 U=U+1:PC(U)=C:PR(U)=R:REM PUSH
410 V(C,R)=1
420 IF V(C,R+1)=1 AND V(C+1,R)=1 AND V(C,R-1)=1 AND V(C-1,R)=1 THEN GOTO 500
430 Z=INT(RND(1)*4)
440 IF Z=0 AND V(C,R+1)=0 THEN S(C,R)=0:R=R+1:GOTO 400
450 IF Z=1 AND V(C+1,R)=0 THEN W(C+1,R)=0:C=C+1:GOTO 400
460 IF Z=2 AND V(C,R-1)=0 THEN S(C,R-1)=0:R=R-1:GOTO 400
470 IF Z=3 AND V(C-1,R)=0 THEN W(C,R)=0:C=C-1:GOTO 400
480 GOTO 430
500 C=PC(U):R=PR(U):U=U-1:REM POP
510 IF U > 0 THEN GOTO 420
520 RETURN
530 REM -----DRAW MAZE-----
540 REM OPEN 4,4:CMD 4:REM SEND OUTPUT TO PRINTER
550 PRINT "+--+--+--+--+--+--+--+--+--+--+"
560 FOR R = 1 TO MS
570 FOR C = 1 TO MS+1
580 IF W(C,R)=0 THEN PRINT "   ";
590 IF W(C,R)=1 THEN PRINT ":  ";
600 NEXT C
610 PRINT
620 FOR C = 1 TO MS
630 IF S(C,R)=0 THEN PRINT "+  ";
640 IF S(C,R)=1 THEN PRINT "+--";
650 NEXT C
660 PRINT "+"
670 NEXT R
680 REM PRINT#4:CLOSE 4:REM CLOSE PRINTER DEVICE
690 RETURN
```

```txt
+--+--+--+--+--+--+--+--+--+--+
:     :        :              :
+  +  +  +  +--+  +--+--+--+  +
:  :  :  :        :     :     :
+  +  +  +--+  +--+  +  +  +--+
:  :     :  :  :     :  :     :
+  +--+--+  +  +  +--+  +--+  +
:     :     :  :  :  :        :
+--+  +  +--+--+  +  +--+--+  +
:  :  :  :        :  :        :
+  +  +  +  +--+--+  +  +--+--+
:     :  :        :  :  :     :
+  +--+  +--+--+  +  +  +  +  +
:     :  :        :     :  :  :
+  +--+  +  +--+--+  +--+--+  +
:  :     :  :     :        :  :
+  +  +--+  +  +--+--+--+  +  +
:  :     :  :  :        :  :  :
+  +--+  +  +  +  +  +--+  +  +
:     :     :     :           :
+--+--+--+--+--+--+--+--+--+--+
```



## Common Lisp


The remove-wall function has been written so as to be as close as possible to the specification.  The walls are made from a single unicode character, specified by the block keyword, e. g. (maze 20 6 :block #\X).  The BOX_DRAWINGS_LIGHT_DIAGONAL_CROSS character is used by default.

```lisp
(defun shuffle (list)                        ;; Z not uniform
  (sort list '> :key (lambda(x) (random 1.0))))

(defun neighbors (x y maze)
  (remove-if-not
   (lambda (x-y) (and (< -1 (first x-y) (array-dimension maze 0))
                 (< -1 (second x-y) (array-dimension maze 1))))
   `((,x ,(+ y 2)) (,(- x 2) ,y) (,x ,(- y 2)) (,(+ x 2) ,y))))

(defun remove-wall (maze x y &optional visited)
  (labels ((walk (maze x y)
             (push (list x y) visited)
             (loop for (u v) in (shuffle (neighbors x y maze))
                unless (member (list u v) visited :test 'equal)
                do (setf (aref maze u v) #\space
                         (aref maze (/ (+ x u) 2) (/ (+ y v) 2)) #\space)
                   (walk maze u v))))
    (setf (aref maze x y) #\space)
    (walk maze x y)))

(defun draw-maze (width height &key (block #\BOX_DRAWINGS_LIGHT_DIAGONAL_CROSS))
  (let ((maze (make-array (list (1+ (* 2 height)) (1+ (* 2 width)))
                          :element-type 'character :initial-element block)))
    (remove-wall maze (1+ (* 2 (random height))) (1+ (* 2 (random width))))
    (loop for i below (array-dimension maze 0)
          do (fresh-line)
             (loop for j below (array-dimension maze 1)
                   do (princ (aref maze i j))))))

(draw-maze 20 6)
```

```txt

╳╳╳╳╳╳╳╳╳╳╳╳╳╳╳╳╳╳╳╳╳╳╳╳╳╳╳╳╳╳╳╳╳╳╳╳╳╳╳╳╳
╳         ╳       ╳     ╳         ╳     ╳
╳ ╳╳╳╳╳╳╳ ╳ ╳╳╳ ╳ ╳╳╳ ╳ ╳╳╳╳╳ ╳╳╳ ╳ ╳╳╳╳╳
╳ ╳     ╳   ╳ ╳ ╳     ╳       ╳ ╳ ╳     ╳
╳ ╳╳╳ ╳ ╳╳╳╳╳ ╳ ╳╳╳ ╳╳╳╳╳╳╳╳╳╳╳ ╳ ╳ ╳╳╳ ╳
╳   ╳ ╳ ╳     ╳ ╳   ╳     ╳     ╳ ╳   ╳ ╳
╳╳╳ ╳ ╳ ╳╳╳ ╳ ╳ ╳╳╳ ╳╳╳╳╳ ╳ ╳╳╳ ╳ ╳╳╳ ╳ ╳
╳ ╳ ╳ ╳     ╳ ╳   ╳   ╳   ╳   ╳ ╳   ╳ ╳ ╳
╳ ╳ ╳ ╳╳╳╳╳╳╳ ╳╳╳ ╳╳╳ ╳ ╳╳╳╳╳ ╳╳╳╳╳ ╳╳╳ ╳
╳   ╳   ╳ ╳   ╳ ╳   ╳ ╳     ╳     ╳   ╳ ╳
╳ ╳╳╳╳╳ ╳ ╳ ╳╳╳ ╳╳╳ ╳╳╳╳╳╳╳ ╳ ╳ ╳╳╳╳╳ ╳ ╳
╳       ╳         ╳         ╳ ╳         ╳
╳╳╳╳╳╳╳╳╳╳╳╳╳╳╳╳╳╳╳╳╳╳╳╳╳╳╳╳╳╳╳╳╳╳╳╳╳╳╳╳╳
```


Another solution using unicode line drawing chars.  Assumes they are single width on console.  Code pretty horribly unreadable.

```lisp
(setf *random-state* (make-random-state t))

(defun 2d-array (w h)
  (make-array (list h w) :initial-element 0))

(defmacro or-and (v a b c)
  `(if (or ,a (and ,b (= 1 ,c))) 0 ,v))

(defun make-maze (w h)
  (let ((vis (2d-array w h))
	(ver (2d-array w h))
	(hor (2d-array w h)))
    (labels
      ((walk (y x)
	     (setf (aref vis y x) 1)
	     (loop
	       (let (x2 y2)
		 (loop for (dx dy) in '((-1 0) (1 0) (0 -1) (0 1))
		       with cnt = 0 do
		       (let ((xx (+ x dx))
			     (yy (+ y dy)))
			 (if (and (array-in-bounds-p vis yy xx)
				  (zerop (aref vis yy xx))
				  (zerop (random (incf cnt))))
			   (setf x2 xx y2 yy))))
		 (if (not x2) (return-from walk))
		 (if (= x x2)
		   (setf (aref hor (min y y2) x) 1)
		   (setf (aref ver y (min x x2)) 1))
		 (walk y2 x2))))

      (show ()
	     (let ((g " │││─┘┐┤─└┌├─┴┬┼"))
	       (loop for i from 0 to h do
		     (loop for j from 0 to w do
			   (format t "~c~a"
			     (char g
			       (+ (or-and 1 (= i 0) (> j 0) (aref ver (1- i) (1- j)))
				  (or-and 2 (= i h) (> j 0) (aref ver i      (1- j)))
				  (or-and 4 (= j 0) (> i 0) (aref hor (1- i) (1- j)))
				  (or-and 8 (= j w) (> i 0) (aref hor (1- i) j    ))))
			     (if (and (< j w)
				      (or (= i 0)
					  (= 0 (aref hor (1- i) j))))
			       "───" "   ")))
		     (terpri)
		     (when (< i h)
		       (loop for j from 0 below w do
			     (format t (if (or (= j 0)
					       (= 0 (aref ver i (1- j))))
					 "│   " "    ")))
		       (format t "│~%"))))))

      (walk (random h) (random w))
      (show))))

(make-maze 20 20)
```

```txt
┼───┴───┼───┴───┴───┼───┴───┴───┼
│       │           │           │
┼────   │   │   │   │   ┌───┐   ├
│       │   │   │   │   │   │   │
┤   ┌───┘   │   │   │   │   │   ├
│   │       │   │       │   │   │
┤   │   ┌───┘   ├───────┤   │   ├
│   │   │       │       │       │
┤   │   │   ────┤   │   │   ────┼
│       │       │   │   │       │
┤   ────┼───┐   │   │   └───┐   ├
│       │   │       │       │   │
┼───┐   │   └───────┼───┐   └───┼
│   │               │   │       │
┤   └────────────   │   └───┐   ├
│                           │   │
┼───┬───┬───┬───┬───┬───┬───┼───┼
```



## D


```d
void main() @safe {
    import std.stdio, std.algorithm, std.range, std.random;

    enum uint w = 14, h = 10;
    auto vis = new bool[][](h, w),
         hor = iota(h + 1).map!(_ => ["+---"].replicate(w)).array,
         ver = h.iota.map!(_ => ["|   "].replicate(w) ~ "|").array;

    void walk(in uint x, in uint y) /*nothrow*/ @safe /*@nogc*/ {
        vis[y][x] = true;
        //foreach (immutable p; [[x-1,y], [x,y+1], [x+1,y], [x,y-1]].randomCover) {
        foreach (const p; [[x-1, y], [x, y+1], [x+1, y], [x, y-1]].randomCover) {
            if (p[0] >= w || p[1] >= h || vis[p[1]][p[0]]) continue;
            if (p[0] == x) hor[max(y, p[1])][x] = "+   ";
            if (p[1] == y) ver[y][max(x, p[0])] = "    ";
            walk(p[0], p[1]);
        }
    }
    walk(uniform(0, w), uniform(0, h));
    foreach (const a, const b; hor.zip(ver ~ []))
        join(a ~ "+\n" ~ b).writeln;
}
```

```txt
+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
|   |               |           |                       |
+   +   +---+---+   +   +---+   +   +---+---+---+   +   +
|               |           |   |       |       |   |   |
+---+---+---+---+---+---+---+   +---+   +---+   +   +---+
|                   |       |       |           |       |
+   +---+---+---+   +   +   +---+   +---+---+   +---+---+
|       |       |   |   |               |   |       |   |
+---+   +   +   +   +   +---+---+---+   +   +---+   +   +
|       |   |   |       |       |           |       |   |
+   +---+   +   +---+---+   +   +---+---+---+   +---+   +
|           |   |           |               |           |
+   +---+---+   +   +---+---+---+---+---+   +---+---+   +
|       |   |       |   |               |           |   |
+---+   +   +---+---+   +   +---+   +   +   +---+---+   +
|   |               |       |       |   |   |           |
+   +---+---+---+   +   +---+   +---+   +   +   +---+---+
|   |               |   |       |   |   |       |       |
+   +   +---+---+---+---+   +---+   +   +---+---+   +   +
|                           |                       |   |
+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
```


## EasyLang


[https://easylang.online/apps/run.html?code=size%3D20%0A%23%20%0Asz%3D2%2Asize%2B1%0A%23%20we%20only%20have%20one-dimensional%20arrays%0Alen%20f%5B%5D%20sz%2Asz%0A%23%20%0Afunc%20make_maze..%0A%23%20the%20maze%20is%20created%20by%20random%20walking%20around%0Afor%20i%20range%20len%20f%5B%5D%0Af%5Bi%5D%3D1%0A.%0Ax%3D1%2B2%2Arandom%20size%0Ay%3D1%2B2%2Arandom%20size%0Af%5Bx%2By%2Asz%5D%3D0%0Avisited%3D1%0Awhile%20visited%20%3C%20size%2Asize%0Aoldx%3Dx%0Aoldy%3Dy%0Adir%3Drandom%204%0Aif%20dir%3D0%20and%20x%2B2%20%3C%20sz%0Ax%2B%3D2%0Aelif%20dir%3D1%20and%20y%2B2%20%3C%20sz%0Ay%2B%3D2%0Aelif%20dir%3D2%20and%20x%20%3E%202%0Ax-%3D2%0Aelif%20dir%3D3%20and%20y%20%3E%202%0Ay-%3D2%0A.%0Aif%20f%5By%2Asz%2Bx%5D%3D1%0Af%5By%2Asz%2Bx%5D%3D0%0Af%5B%28y%2Boldy%29/2%2Asz%2B%28x%2Boldx%29/2%5D%3D0%0Avisited%2B%3D1%0A.%0A.%0Af%5B%28sz-1%29%2Asz%2Bsz-2%5D%3D0%0A.%0Afunc%20show_maze..%0Ac2%23%3D%28100-24/size%29/size/2%0Ac10%23%3Dc2%23/5%0Alinewidth%202%2Ac10%23%0Acolor%20997%0Amove%200%200%0Arect%20100%20100%0Acolor%20543%0Afor%20r%20range%20sz%0Afor%20c%20range%20sz%0Aif%20f%5Br%2Asz%2Bc%5D%3D1%0Aif%20r%20mod%202%3D0%0Aif%20c%20mod%202%3D1%0Amove%20c10%23%2B%28c-1%29%2Ac2%23%20c10%23%2Br%2Ac2%23%0Aline%20c10%23%2B%28c%2B1%29%2Ac2%23%20c10%23%2Br%2Ac2%23%0A.%0Aelse%0Amove%20c10%23%2Bc%2Ac2%23%20c10%23%2B%28r-1%29%2Ac2%23%0Aline%20c10%23%2Bc%2Ac2%23%20c10%23%2B%28r%2B1%29%2Ac2%23%0A.%0A.%0A.%0A.%0A.%0Acall%20make_maze%0Acall%20show_maze Run it]


```easyprog.online
size = 20
#
sz = 2 * size + 1
# we only have one-dimensional arrays
len f[] sz * sz
#
func make_maze . .
  # the maze is created by random walking around
  for i range len f[]
    f[i] = 1
  .
  x = 1 + 2 * random size
  y = 1 + 2 * random size
  f[x + y * sz] = 0
  visited = 1
  while visited < size * size
    oldx = x
    oldy = y
    dir = random 4
    if dir = 0 and x + 2 < sz
      x += 2
    elif dir = 1 and y + 2 < sz
      y += 2
    elif dir = 2 and x > 2
      x -= 2
    elif dir = 3 and y > 2
      y -= 2
    .
    if f[y * sz + x] = 1
      f[y * sz + x] = 0
      f[(y + oldy) / 2 * sz + (x + oldx) / 2] = 0
      visited += 1
    .
  .
  f[(sz - 1) * sz + sz - 2] = 0
.
func show_maze . .
  c2# = (100 - 24 / size) / size / 2
  c10# = c2# / 5
  linewidth 2 * c10#
  color 997
  move 0 0
  rect 100 100
  color 543
  for r range sz
    for c range sz
      if f[r * sz + c] = 1
        if r mod 2 = 0
          if c mod 2 = 1
            move c10# + (c - 1) * c2# c10# + r * c2#
            line c10# + (c + 1) * c2# c10# + r * c2#
          .
        else
          move c10# + c * c2# c10# + (r - 1) * c2#
          line c10# + c * c2# c10# + (r + 1) * c2#
        .
      .
    .
  .
.
call make_maze
call show_maze
```



## EGL


```EGL
program MazeGen

    // First and last columns/rows are "dead" cells. Makes generating
    // a maze with border walls much easier. Therefore, a visible
    // 20x20 maze has a maze size of 22.
    mazeSize int = 22;

    south boolean[][];
    west boolean[][];
    visited boolean[][];

    function main()
        initMaze();
        generateMaze();
        drawMaze();
    end

    private function initMaze()

        visited = createBooleanArray(mazeSize, mazeSize, false);

        // Initialize border cells as already visited
        for(col int from 1 to mazeSize)
            visited[col][1] = true;
            visited[col][mazeSize] = true;
        end
        for(row int from 1 to mazeSize)
            visited[1][row] = true;
            visited[mazeSize][row] = true;
        end

        // Initialize all walls as present
        south = createBooleanArray(mazeSize, mazeSize, true);
        west = createBooleanArray(mazeSize, mazeSize, true);

    end

    private function createBooleanArray(col int in, row int in, initialState boolean in) returns(boolean[][])

        newArray boolean[][] = new boolean[0][0];

        for(i int from 1 to col)
            innerArray boolean[] = new boolean[0];
            for(j int from 1 to row)
                innerArray.appendElement(initialState);
            end
            newArray.appendElement(innerArray);
        end

        return(newArray);

    end

    private function createIntegerArray(col int in, row int in, initialValue int in) returns(int[][])

        newArray int[][] = new int[0][0];

        for(i int from 1 to col)
            innerArray int[] = new int[0];
            for(j int from 1 to row)
                innerArray.appendElement(initialValue);
            end
            newArray.appendElement(innerArray);
        end

        return(newArray);

    end

    private function generate(col int in, row int in)

        // Mark cell as visited
        visited[col][row] = true;

        // Keep going as long as there is an unvisited neighbor
        while(!visited[col][row + 1] || !visited[col + 1][row] ||
                !visited[col][row - 1] || !visited[col - 1][row])

            while(true)
                r float = MathLib.random(); // Choose a random direction

                case
                    when(r < 0.25 && !visited[col][row + 1]) // Go south
                        south[col][row] = false; // South wall down
                        generate(col, row + 1);
                        exit while;
                    when(r >= 0.25 && r < 0.50 && !visited[col + 1][row]) // Go east
                        west[col + 1][row] = false; // West wall of neighbor to the east down
                        generate(col + 1, row);
                        exit while;
                    when(r >= 0.5 && r < 0.75 && !visited[col][row - 1]) // Go north
                        south[col][row - 1] = false; // South wall of neighbor to the north down
                        generate(col, row - 1);
                        exit while;
                    when(r >= 0.75 && r < 1.00 && !visited[col - 1][row]) // Go west
                        west[col][row] = false; // West wall down
                        generate(col - 1, row);
                        exit while;
                end
            end
        end

    end

    private function generateMaze()

        // Pick random start position (within the visible maze space)
        randomStartCol int = MathLib.floor((MathLib.random() *(mazeSize - 2)) + 2);
        randomStartRow int = MathLib.floor((MathLib.random() *(mazeSize - 2)) + 2);

        generate(randomStartCol, randomStartRow);

    end

    private function drawMaze()

        line string;

        // Iterate over wall arrays (skipping dead border cells as required).
        // Construct a line at a time and output to console.
        for(row int from 1 to mazeSize - 1)

            if(row > 1)
                line = "";
                for(col int from 2 to mazeSize)
                    if(west[col][row])
                        line ::= "|   ";
                    else
                        line ::= "    ";
                    end
                end
                Syslib.writeStdout(line);
            end

            line = "";
            for(col int from 2 to mazeSize - 1)
                if(south[col][row])
                    line ::= "+---";
                else
                    line ::= "+   ";
                end
            end
            line ::= "+";
            SysLib.writeStdout(line);

        end

    end

end
```

```txt

+---+---+---+---+---+---+---+---+---+---+
|   |                   |           |   |
+   +   +---+---+---+   +---+   +   +   +
|   |       |   |   |       |   |       |
+   +---+   +   +   +   +   +   +---+   +
|       |       |   |   |   |   |       |
+   +   +---+   +   +---+   +   +   +---+
|   |       |   |   |       |   |       |
+   +---+---+   +   +   +---+   +---+---+
|   |           |   |   |       |       |
+   +   +---+---+   +   +   +   +   +   +
|   |   |   |       |   |   |       |   |
+   +   +   +   +---+   +   +---+---+   +
|       |   |           |   |       |   |
+   +---+   +---+---+---+   +   +   +   +
|   |                   |   |   |       |
+   +---+   +---+   +   +---+   +---+   +
|       |   |       |           |   |   |
+---+   +---+   +---+---+---+---+   +   +
|               |                       |
+---+---+---+---+---+---+---+---+---+---+

```



## Elixir

```elixir
defmodule Maze do
  def generate(w, h) do
    maze = (for i <- 1..w, j <- 1..h, into: Map.new, do: {{:vis, i, j}, true})
           |> walk(:rand.uniform(w), :rand.uniform(h))
    print(maze, w, h)
    maze
  end

  defp walk(map, x, y) do
    Enum.shuffle( [[x-1,y], [x,y+1], [x+1,y], [x,y-1]] )
    |> Enum.reduce(Map.put(map, {:vis, x, y}, false), fn [i,j],acc ->
      if acc[{:vis, i, j}] do
        {k, v} = if i == x, do: {{:hor, x, max(y, j)}, "+   "},
                          else: {{:ver, max(x, i), y}, "    "}
        walk(Map.put(acc, k, v), i, j)
      else
        acc
      end
    end)
  end

  defp print(map, w, h) do
    Enum.each(1..h, fn j ->
      IO.puts Enum.map_join(1..w, fn i -> Map.get(map, {:hor, i, j}, "+---") end) <> "+"
      IO.puts Enum.map_join(1..w, fn i -> Map.get(map, {:ver, i, j}, "|   ") end) <> "|"
    end)
    IO.puts String.duplicate("+---", w) <> "+"
  end
end

Maze.generate(20, 10)
```


```txt

+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
|       |               |                           |                   |       |
+   +---+   +   +---+   +---+   +---+---+---+---+   +   +   +---+---+   +   +---+
|   |       |       |           |                   |   |   |       |   |       |
+   +   +---+---+   +---+---+---+---+   +---+---+   +   +   +   +   +   +   +   +
|   |   |       |           |       |   |       |   |   |   |   |   |   |   |   |
+   +   +   +   +---+---+   +   +   +---+   +   +   +---+   +   +---+   +   +   +
|   |   |   |           |       |           |   |           |           |   |   |
+   +   +   +   +---+---+---+---+---+---+---+   +---+---+---+   +---+---+---+   +
|   |   |   |                   |           |       |       |   |           |   |
+   +   +---+---+---+---+   +   +   +   +   +---+   +   +   +   +   +---+   +   +
|   |                   |   |   |   |   |       |   |   |           |   |       |
+   +---+---+---+---+   +   +   +   +   +---+   +   +   +---+---+---+   +---+   +
|                   |   |   |       |   |       |   |   |       |       |       |
+   +---+---+---+---+   +   +---+---+   +---+---+   +   +   +   +   +---+   +---+
|           |           |       |       |           |   |   |   |       |   |   |
+   +---+   +   +---+---+---+---+   +---+   +---+---+---+   +   +---+   +   +   +
|   |   |   |       |               |       |               |       |   |   |   |
+   +   +   +---+   +   +---+---+   +   +---+---+---+---+---+---+   +   +   +   +
|       |               |           |                               |           |
+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

```



## Elm


```elm
import Maybe as M
import Result as R
import Matrix
import Mouse
import Random exposing (Seed)
import Matrix.Random
import Time exposing (Time, every, second)
import Set exposing (Set, fromList)
import List exposing (..)
import String exposing (join)
import Html exposing (Html, br, input, h1, h2, text, div, button)
import Html.Events as HE
import Html.Attributes as HA
import Html.App exposing (program)
import Json.Decode  as JD
import Svg
import Svg.Attributes exposing (version, viewBox, cx, cy, r, x, y, x1, y1, x2, y2, fill,points, style, width, height, preserveAspectRatio)

minSide = 10
maxSide = 40
w = 700
h = 700
dt = 0.001

type alias Direction = Int
down = 0
right = 1

type alias Door = (Matrix.Location, Direction)

type State = Initial | Generating | Generated | Solved

type alias Model =
  { rows : Int
  , cols : Int
  , animate : Bool
  , boxes : Matrix.Matrix Bool
  , doors : Set Door
  , current : List Matrix.Location
  , state : State
  , seedStarter : Int
  , seed : Seed
  }

initdoors : Int -> Int -> Set Door
initdoors rows cols =
  let
    pairs la lb = List.concatMap (\at -> List.map ((,) at) lb) la
    downs = pairs (pairs [0..rows-2] [0..cols-1]) [down]
    rights = pairs (pairs [0..rows-1] [0..cols-2]) [right]
  in downs ++ rights |> fromList

initModel : Int -> Int -> Bool -> State -> Int -> Model
initModel rows cols animate state starter =
  let rowGenerator = Random.int 0 (rows-1)
      colGenerator = Random.int 0 (cols-1)
      locationGenerator = Random.pair rowGenerator colGenerator
      (c, s)= Random.step locationGenerator (Random.initialSeed starter)
  in { rows = rows
     , cols = cols
     , animate = animate
     , boxes = Matrix.matrix rows cols (\location -> state == Generating && location == c)
     , doors = initdoors rows cols
     , current = if state == Generating then [c] else []
     , state = state
     , seedStarter = starter -- updated every Tick until maze generated.
     , seed = s
     }

view model =
  let
    borderLineStyle = style "stroke:green;stroke-width:0.3"
    wallLineStyle = style "stroke:green;stroke-width:0.1"

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

    doorToLine door =
      let (deltaX1, deltaY1) = if (snd door == right) then (1,0) else (0,1)
          (row, column) = fst door
      in Svg.line [ x1 <| toString (column + deltaX1)
                  , y1 <| toString (row    + deltaY1)
                  , x2 <| toString (column + 1)
                  , y2 <| toString (row    + 1)
                  , wallLineStyle ] []

    doors = (List.map doorToLine <| Set.toList model.doors )

    circleInBox (row,col) color =
      Svg.circle [ r "0.25"
      , fill (color)
      , cx (toString (toFloat col + 0.5))
      , cy (toString (toFloat row + 0.5))
      ] []

    showUnvisited location box =
       if box then [] else [ circleInBox location "yellow" ]

    unvisited = model.boxes
                  |> Matrix.mapWithLocation showUnvisited
                  |> Matrix.flatten
                  |> concat

    current =
      case head model.current of
          Nothing -> []
          Just c -> [circleInBox c "black"]

    maze =
      if model.animate || model.state /= Generating
      then [ Svg.g [] <| doors ++ borders ++ unvisited ++ current ]
      else [ Svg.g [] <| borders ]
  in
    div
      []
      [ h2 [centerTitle] [text "Maze Generator"]
      , div
          [floatLeft]
          (  slider "rows" minSide maxSide model.rows SetRows
          ++ [ br [] [] ]

          ++ slider "cols" minSide maxSide model.cols SetCols
          ++ [ br [] [] ]

          ++ checkbox "Animate" model.animate SetAnimate
          ++ [ br [] [] ]

          ++ [ button
                 [ HE.onClick Generate ]
                 [ text "Generate"]
             ] )
      , div
          [floatLeft]
          [ Svg.svg
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
      ]

checkbox label checked msg =
  [ input
      [ HA.type' "checkbox"
      , HA.checked checked
      , HE.on "change" (JD.map msg HE.targetChecked)
      ]
      []
    , text label
  ]

slider name min max current msg =
  [ input
    [ HA.value (if current >= min then current |> toString else "")
    , HE.on "input" (JD.map msg HE.targetValue )
    , HA.type' "range"
    , HA.min <| toString min
    , HA.max <| toString max
    ]
    []
  , text <| name ++ "=" ++ (current |> toString)
  ]

floatLeft = HA.style [ ("float", "left") ]
centerTitle = HA.style [ ( "text-align", "center") ]

unvisitedNeighbors : Model -> Matrix.Location -> List Matrix.Location
unvisitedNeighbors model (row,col) =
  [(row, col-1), (row-1, col), (row, col+1), (row+1, col)]
    |> List.filter (\l -> fst l >= 0 && snd l >= 0 && fst l < model.rows && snd l < model.cols)
    |> List.filter (\l -> (Matrix.get l model.boxes) |> M.withDefault False |> not)

updateModel' : Model -> Int -> Model
updateModel' model t =
  case head model.current of
    Nothing -> {model | state = Generated, seedStarter = t }
    Just prev ->
      let neighbors = unvisitedNeighbors model prev
      in if (length neighbors) > 0 then
           let (neighborIndex, seed) = Random.step (Random.int 0 (length neighbors-1)) model.seed
               next = head (drop neighborIndex neighbors) |> M.withDefault (0,0)
               boxes = Matrix.set next True model.boxes
               dir = if fst prev == fst next then right else down
               doorCell = if (  (dir == down)   && (fst prev < fst next))
                             || (dir == right ) && (snd prev < snd next) then prev else next
               doors = Set.remove (doorCell, dir) model.doors
           in {model | boxes=boxes, doors=doors, current=next :: model.current, seed=seed, seedStarter = t}
         else
           let tailCurrent = tail model.current |> M.withDefault []
           in updateModel' {model | current = tailCurrent} t

updateModel : Msg -> Model -> Model
updateModel msg model =
  let stringToCellCount s =
    let v' = String.toInt s |> R.withDefault minSide
    in if v' < minSide then minSide else v'
  in case msg of
       Tick tf ->
         let t = truncate tf
         in
           if (model.state == Generating) then updateModel' model t
           else { model | seedStarter = t }

       Generate ->
         initModel model.rows model.cols model.animate Generating model.seedStarter

       SetRows countString ->
         initModel (stringToCellCount countString) model.cols model.animate Initial model.seedStarter

       SetCols countString ->
         initModel model.rows (stringToCellCount countString) model.animate Initial model.seedStarter

       SetAnimate b ->
         { model | animate = b }

       NoOp -> model

type Msg = NoOp | Tick Time | Generate | SetRows String | SetCols String | SetAnimate Bool

subscriptions model = every (dt * second) Tick

main =
  let
    update msg model = (updateModel msg model, Cmd.none)
    init = (initModel 21 36 False Initial 0, Cmd.none)
  in program
       { init = init
       , view = view
       , update = update
       , subscriptions = subscriptions
       }
```


Link to live demo: http://dc25.github.io/mazeGenerationElm/


## Erlang

Erlang is single assignment. To get mutability I use processes. The code is over-enginered for this task, but the extra is used for [[Maze_solving]]. Also, Erlang starts counting at 1, not 0, so the co-ordinate of the lower left corner is 1,1.


### Using multiple processes


```Erlang

-module( maze ).

-export( [cell_accessible_neighbours/1, cell_content/1, cell_content_set/2, cell_pid/3, cell_position/1, display/1, generation/2, stop/1, task/0] ).

-record( maze, {dict, max_x, max_y, start} ).
-record( state, {content=" ", controller, is_dug=false, max_x, max_y, neighbours=[], position, walls=[north, south, east, west], walk_done} ).

cell_accessible_neighbours( Pid ) -> read( Pid, accessible_neighbours ).

cell_content( Pid ) -> read( Pid, content ).

cell_content_set( Pid, Content ) -> Pid ! {content, Content, erlang:self()}.

cell_pid( X, Y, Maze ) -> dict:fetch( {X, Y}, Maze#maze.dict ).

cell_position( Pid ) -> read( Pid, position ).

display( #maze{dict=Dict, max_x=Max_x, max_y=Max_y} ) ->
	Position_pids = dict:to_list( Dict ),
	display( Max_x, Max_y, reads(Position_pids, content), reads(Position_pids, walls) ).

generation( Max_x, Max_y ) ->
       Controller = erlang:self(),
       Position_pids = cells_create( Controller, Max_x, Max_y ),
       Pids = [Y || {_X, Y} <- Position_pids],
       [X ! {position_pids, Position_pids} || X <- Pids],
       {Position, Pid} = lists:nth( random:uniform(Max_x * Max_y), Position_pids ),
       Pid ! {dig, Controller},
       receive
       {dig_done} -> ok
       end,
       #maze{dict=dict:from_list(Position_pids), max_x=Max_x, max_y=Max_y, start=Position}.

stop( #maze{dict=Dict} ) ->
      Controller = erlang:self(),
      Pids = [Y || {_X, Y} <- dict:to_list(Dict)],
      [X ! {stop, Controller} || X <- Pids],
      ok.

task() ->
       Maze = generation( 16, 8 ),
       io:fwrite( "Starting at ~p~n", [Maze#maze.start] ),
       display( Maze ),
       stop( Maze ).



cells_create( Controller, Max_x, Max_y ) -> [{{X, Y}, cell_create(Controller, Max_x, Max_y, {X, Y})} || X <- lists:seq(1, Max_x), Y<- lists:seq(1, Max_y)].

cell_create( Controller, Max_x, Max_y, {X, Y} ) -> erlang:spawn_link( fun() -> random:seed( X*1000, Y*1000, (X+Y)*1000 ), loop( #state{controller=Controller, max_x=Max_x, max_y=Max_y, position={X, Y}} ) end ).

display( Max_x, Max_y, Position_contents, Position_walls ) ->
        All_rows = [display_row( Max_x, Y, Position_contents, Position_walls ) || Y <- lists:seq(Max_y, 1, -1)],
        [io:fwrite("~s+~n~s|~n", [North, West]) || {North, West} <- All_rows],
	io:fwrite("~s+~n", [lists:flatten(lists:duplicate(Max_x, display_row_north(true)))] ).

display_row( Max_x, Y, Position_contents, Position_walls ) ->
	North_wests = [display_row_walls(proplists:get_value({X,Y}, Position_contents), proplists:get_value({X,Y}, Position_walls)) || X <- lists:seq(1, Max_x)],
	North = lists:append( [North || {North, _West} <- North_wests] ),
	West = lists:append( [West || {_X, West} <- North_wests] ),
	{North, West}.

display_row_walls( Content, Walls ) -> {display_row_north( lists:member(north, Walls) ), display_row_west( lists:member(west, Walls), Content )}.

display_row_north( true ) -> "+---";
display_row_north( false ) -> "+   ".

display_row_west( true, Content ) -> "| " ++ Content ++ " ";
display_row_west( false, Content ) -> "  " ++ Content ++ " ".

loop( State ) ->
    receive
    {accessible_neighbours, Pid} ->
    	Pid ! {accessible_neighbours, loop_accessible_neighbours( State#state.neighbours, State#state.walls ), erlang:self()},
        loop( State );
    {content, Pid} ->
    	Pid ! {content, State#state.content, erlang:self()},
        loop( State );
    {content, Content, _Pid} ->
        loop( State#state{content=Content} );
    {dig, Pid} ->
	    Not_dug_neighbours = loop_not_dug( State#state.neighbours ),
	    New_walls = loop_dig( Not_dug_neighbours, lists:delete( loop_wall_from_pid(Pid, State#state.neighbours), State#state.walls), Pid ),
	    loop( State#state{is_dug=true, walls=New_walls, walk_done=Pid} );
    {dig_done} ->
	    Not_dug_neighbours = loop_not_dug( State#state.neighbours ),
	    New_walls = loop_dig( Not_dug_neighbours, State#state.walls, State#state.walk_done ),
	    loop( State#state{walls=New_walls} );
    {is_dug, Pid} ->
    	    Pid ! {is_dug, State#state.is_dug, erlang:self()},
	    loop( State );
    {position, Pid} ->
    	Pid ! {position, State#state.position, erlang:self()},
        loop( State );
    {position_pids, Position_pids} ->
        {_My_position, Neighbours} = lists:foldl( fun loop_neighbours/2, {State#state.position, []}, Position_pids ),
        erlang:garbage_collect(), % Shrink process after using large Pid_positions. For memory starved systems.
        loop( State#state{neighbours=Neighbours} );
    {stop, Controller} when Controller =:= State#state.controller ->
    	   ok;
    {walls, Pid} ->
    	    Pid ! {walls, State#state.walls, erlang:self()},
	    loop( State )
    end.

loop_accessible_neighbours( Neighbours, Walls ) -> [Pid || {Direction, Pid} <- Neighbours, not lists:member(Direction, Walls)].

loop_dig( [], Walls, Pid ) ->
	Pid ! {dig_done},
	Walls;
loop_dig( Not_dug_neighbours, Walls, _Pid ) ->
        {Dig_pid, Dig_direction} = lists:nth( random:uniform(erlang:length(Not_dug_neighbours)), Not_dug_neighbours ),
        Dig_pid ! {dig, erlang:self()},
	lists:delete( Dig_direction, Walls ).

loop_neighbours( {{X, Y}, Pid}, {{X, My_y}, Acc} ) when Y =:= My_y + 1 -> {{X, My_y}, [{north, Pid} | Acc]};
loop_neighbours( {{X, Y}, Pid}, {{X, My_y}, Acc} ) when Y =:= My_y - 1 -> {{X, My_y}, [{south, Pid} | Acc]};
loop_neighbours( {{X, Y}, Pid}, {{My_x, Y}, Acc} ) when X =:= My_x + 1 -> {{My_x, Y}, [{east, Pid} | Acc]};
loop_neighbours( {{X, Y}, Pid}, {{My_x, Y}, Acc} ) when X =:= My_x - 1 -> {{My_x, Y}, [{west, Pid} | Acc]};
loop_neighbours( _Position_pid, Acc ) -> Acc.

loop_not_dug( Neighbours ) ->
	My_pid = erlang:self(),
	[Pid ! {is_dug, My_pid} || {_Direction, Pid} <- Neighbours],
	[{Pid, Direction} || {Direction, Pid} <- Neighbours, not read_receive(Pid, is_dug)].

loop_wall_from_pid( Pid, Neighbours ) -> loop_wall_from_pid_result( lists:keyfind(Pid, 2, Neighbours) ).
loop_wall_from_pid_result( {Direction, _Pid} ) -> Direction;
loop_wall_from_pid_result( false ) -> controller.

read( Pid, Key ) ->
	Pid ! {Key, erlang:self()},
	read_receive( Pid, Key ).

read_receive( Pid, Key ) ->
        receive
        {Key, Value, Pid} -> Value
        end.

reads( Position_pids, Key ) ->
    My_pid = erlang:self(),
    [Pid ! {Key, My_pid} || {_Position, Pid} <- Position_pids],
    [{Position, read_receive(Pid, Key)} || {Position, Pid} <- Position_pids].

```

```txt

5> maze:task().
Starting at {10,5}
+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
|                   |       |           |   |                   |
+---+   +---+---+   +   +   +   +---+   +   +   +   +---+---+   +
|       |       |       |   |       |   |       |           |   |
+   +---+   +   +---+   +---+---+   +   +---+---+---+---+   +   +
|   |       |       |   |           |           |           |   |
+   +---+   +---+---+   +   +---+---+---+---+   +   +---+---+   +
|       |   |           |   |           |       |   |   |       |
+   +   +   +   +---+---+   +   +---+---+   +---+   +   +   +---+
|   |   |       |           |           |   |       |       |   |
+---+   +---+---+   +---+---+---+---+   +   +   +---+   +---+   +
|       |       |           |       |   |   |   |       |       |
+   +   +   +   +---+---+   +   +---+   +   +   +---+   +   +   +
|   |   |   |           |   |       |       |       |       |   |
+   +---+   +   +---+---+   +---+   +---+---+---+   +---+---+   +
|           |                                       |           |
+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

```



### Using 2 digraphs

Uses 2 digraph "objects": a) the 'matrix', a fully connected digraph of MxN vertices and b) the 'maze', an unconnected digraph, also MxN, that is populated while walking.

Employs a faux Visitor pattern to populate the maze while traversing the matrix in depth-first order.

Vertices: 0 .. MxN - 1

Rows: 0 .. M - 1

Cols: 0 .. N - 1

Usage: start with generate_default/0. Use generate_MxN() to test other maze sizes.

```Erlang

-module(maze).
-record(maze, {g, m, n}).
-export([generate_default/0, generate_MxN/2]).

make_maze(M, N) ->
    Maze = #maze{g = digraph:new(), m = M, n = N},
    lists:foreach(fun(X) -> digraph:add_vertex(Maze#maze.g, X) end, lists:seq(0, M * N - 1)),
    Maze.

row_at(V, Maze) -> trunc(V / Maze#maze.n).
col_at(V, Maze) -> V - row_at(V, Maze) * Maze#maze.n.
vertex_at(Row, Col, Maze) -> Cell_Exists = cell_exists(Row, Col, Maze), if Cell_Exists -> Row * Maze#maze.n + Col; true -> -1 end.
cell_exists(Row, Col, Maze) -> (Row >= 0) and (Row < Maze#maze.m) and (Col >= 0) and (Col < Maze#maze.n).

adjacent_cells(V, Maze) -> % ordered: left, up, right, down
    adjacent_cell(cell_left, V, Maze)++adjacent_cell(cell_up, V, Maze)++adjacent_cell(cell_right, V, Maze)++adjacent_cell(cell_down, V, Maze).

adjacent_cell(cell_left, V, Maze) -> case (col_at(V, Maze) == 0) of true -> []; _Else -> [V - 1] end;
adjacent_cell(cell_up, V, Maze) -> case (row_at(V, Maze) == 0) of true -> []; _Else -> [V - Maze#maze.n] end;
adjacent_cell(cell_right, V, Maze) -> case (col_at(V, Maze) == Maze#maze.n - 1) of true -> []; _Else -> [V + 1] end;
adjacent_cell(cell_down, V, Maze) -> case (row_at(V, Maze) == Maze#maze.m - 1) of true -> []; _Else -> [V + Maze#maze.n] end.

connect_all(V, Maze) ->
    lists:foreach(fun(X) -> digraph:add_edge(Maze#maze.g, V, X) end, adjacent_cells(V, Maze)).

make_maze(M, N, all_connected) ->
    Maze = make_maze(M, N),
    lists:foreach(fun(X) -> connect_all(X, Maze) end, lists:seq(0, M * N - 1)),
    Maze.

maze_parts(Maze) ->
    SPR = Maze#maze.n + 1,      % slots per row is #columns + 1
    NPR = (Maze#maze.m * 2) + 1,    % # part rows is #(rows * 2) + 1
    [make_part(Maze, trunc(Index/SPR), Index - trunc(Index/SPR) * SPR) || Index <- lists:seq(0, (SPR * NPR) - 1)].

draw_part(Part) ->
    case Part of
        {pwall, pclosed} -> io:format("+---");
        {pwall, popen} -> io:format("+   ");
        {pwall, pend} -> io:format("+~n");
        {phall, pclosed} -> io:format("|   ");
        {phall, popen} -> io:format("    ");
        {phall, pend} -> io:format("|~n")
    end.

has_neighbour(Maze, Row, Col, Direction) ->
    V = vertex_at(Row, Col, Maze),
    if
        V >= 0 ->
            Adjacent = adjacent_cell(Direction, V, Maze),
            if
                length(Adjacent) > 0 ->
                    Neighbours = digraph:out_neighbours(Maze#maze.g, lists:nth(1, Adjacent)),
                    lists:member(V, Neighbours);
                true -> false
            end;
        true -> false
    end.

make_part(Maze, DoubledRow, Col) ->
    if
        trunc(DoubledRow/2) * 2 == DoubledRow -> % --- (even row) making a wall above the cell
            make_part(Maze, trunc(DoubledRow/2), Col, cell_up, pwall);
        true -> % ---otherwise (odd row) making a hall through the cell
            make_part(Maze, trunc(DoubledRow/2), Col, cell_left, phall)
    end.

make_part(Maze, _, Col, _, Part_Type) when Col == Maze#maze.n -> {Part_Type, pend};
make_part(Maze, Row, Col, Direction, Part_Type) ->
    Has_Neighbour = has_neighbour(Maze, Row, Col, Direction),
    if
        Has_Neighbour -> {Part_Type, popen};
        true -> {Part_Type, pclosed}
    end.

shuffle([], Acc) -> Acc;
shuffle(List, Acc) ->
    Elem = lists:nth(random:uniform(length(List)), List),
    shuffle(lists:delete(Elem, List), Acc++[Elem]).

processDepthFirst(Maze) ->
    if
        Maze#maze.m * Maze#maze.n == 0 -> [{pwall, pend}];
        true ->
            Visited = array:new([{size, Maze#maze.m * Maze#maze.n},{fixed,true},{default,false}]),
            {_, Path} = processDepthFirst(Maze, -1, random:uniform(Maze#maze.m * Maze#maze.n) - 1, {Visited, []}),
            Path
    end.

processDepthFirst(Maze, Vfrom, V, VandP) ->
    {Visited, Path} = VandP,
    Was_Visited = array:get(V, Visited),
    if
        not Was_Visited ->
            Walker = fun(X, Acc) -> processDepthFirst(Maze, V, X, Acc) end,
            Random_Neighbours = shuffle(digraph:out_neighbours(Maze#maze.g, V), []),
            lists:foldl(Walker, {array:set(V, true, Visited), Path++[{Vfrom, V}]}, Random_Neighbours);
        true -> VandP
    end.

open_wall(_, {-1, _}) -> ok;
open_wall(Maze, {V, V2}) ->
    case (V2 > V) of true -> digraph:add_edge(Maze#maze.g, V, V2); _Else -> digraph:add_edge(Maze#maze.g, V2, V) end.

generate_MxN(M, N) ->
    Maze = make_maze(M, N),
    Matrix = make_maze(M, N, all_connected),
    Trail = processDepthFirst(Matrix),
    lists:foreach(fun(X) -> open_wall(Maze, X) end, Trail),
    Parts = maze_parts(Maze),
    lists:foreach(fun(X) -> draw_part(X) end, Parts).

generate_default() ->
    generate_MxN(9, 9).

```

```txt

8> maze:generate_default().
+---+---+---+---+---+---+---+---+---+
|       |                           |
+   +   +---+   +---+---+---+---+   +
|   |       |           |       |   |
+   +---+   +---+---+   +   +   +   +
|   |       |   |       |   |       |
+   +   +   +   +   +---+---+---+   +
|   |   |   |   |       |       |   |
+   +   +---+   +   +   +   +   +---+
|   |           |   |       |       |
+   +---+---+---+---+---+---+---+   +
|   |                           |   |
+   +   +---+---+   +---+---+   +   +
|   |           |   |           |   |
+   +---+---+---+   +---+   +---+   +
|   |           |       |       |   |
+   +   +---+   +---+   +---+   +   +
|       |               |           |
+---+---+---+---+---+---+---+---+---+
ok
9>
```


## Emacs Lisp

file: maze.el

```lisp

(require 'cl-lib)

(cl-defstruct maze rows cols data)

(defmacro maze-pt (w r c)
  `(+ (* (mod ,r (maze-rows ,w)) (maze-cols ,w))
      (mod ,c (maze-cols ,w))))

(defmacro maze-ref (w r c)
  `(aref (maze-data ,w) (maze-pt ,w ,r ,c)))

(defun new-maze (rows cols)
  (setq rows (1+ rows)
        cols (1+ cols))
  (let ((m (make-maze :rows rows :cols cols :data (make-vector (* rows cols) nil))))

    (dotimes (r rows)
      (dotimes (c cols)
        (setf (maze-ref m r c) (copy-sequence '(wall ceiling)))))

    (dotimes (r rows)
      (maze-set m r (1- cols) 'visited))

    (dotimes (c cols)
      (maze-set m (1- rows) c 'visited))

    (maze-unset m 0 0 'ceiling) ;; Maze Entrance
    (maze-unset m (1- rows) (- cols 2) 'ceiling) ;; Maze Exit

    m))

(defun maze-is-set (maze r c v)
  (member v (maze-ref maze r c)))

(defun maze-set (maze r c v)
  (let ((cell (maze-ref maze r c)))
    (when (not (member v cell))
      (setf (maze-ref maze r c) (cons v cell)))))

(defun maze-unset (maze r c v)
  (setf (maze-ref maze r c) (delete v (maze-ref maze r c))))

(defun print-maze (maze &optional marks)
  (dotimes (r (1- (maze-rows maze)))

    (dotimes (c (1- (maze-cols maze)))
      (princ (if (maze-is-set maze r c 'ceiling) "+---" "+   ")))
    (princ "+")
    (terpri)

    (dotimes (c (1- (maze-cols maze)))
      (princ (if (maze-is-set maze r c 'wall) "|" " "))
      (princ (if (member (cons r c) marks) " * " "   ")))
    (princ "|")
    (terpri))

  (dotimes (c (1- (maze-cols maze)))
    (princ (if (maze-is-set maze (1- (maze-rows maze)) c 'ceiling) "+---" "+   ")))
  (princ "+")
  (terpri))

(defun shuffle (lst)
  (sort lst (lambda (a b) (= 1 (random 2)))))

(defun to-visit (maze row col)
  (let (unvisited)
    (dolist (p '((0 . +1) (0 . -1) (+1 . 0) (-1 . 0)))
      (let ((r (+ row (car p)))
            (c (+ col (cdr p))))
      (unless (maze-is-set maze r c 'visited)
        (push (cons r c) unvisited))))
    unvisited))

(defun make-passage (maze r1 c1 r2 c2)
  (if (= r1 r2)
      (if (< c1 c2)
          (maze-unset maze r2 c2 'wall) ; right
        (maze-unset maze r1 c1 'wall))  ; left
    (if (< r1 r2)
        (maze-unset maze r2 c2 'ceiling)   ; up
      (maze-unset maze r1 c1 'ceiling))))  ; down

(defun dig-maze (maze row col)
  (let (backup
        (run 0))
    (maze-set maze row col 'visited)
    (push (cons row col) backup)
    (while backup
      (setq run (1+ run))
      (when (> run (/ (+ row col) 3))
        (setq run 0)
        (setq backup (shuffle backup)))
      (setq row (caar backup)
            col (cdar backup))
      (let ((p (shuffle (to-visit maze row col))))
        (if p
            (let ((r (caar p))
                  (c (cdar p)))
              (make-passage maze row col r c)
              (maze-set maze r c 'visited)
              (push (cons r c) backup))
          (pop backup)
          (setq backup (shuffle backup))
          (setq run 0))))))

(defun generate (rows cols)
  (let* ((m (new-maze rows cols)))
    (dig-maze m (random rows) (random cols))
    (print-maze m)))

(defun parse-ceilings (line)
  (let (rtn
        (i 1))
    (while (< i (length line))
      (push (eq ?- (elt line i)) rtn)
      (setq i (+ i 4)))
    (nreverse rtn)))

(defun parse-walls (line)
  (let (rtn
        (i 0))
    (while (< i (length line))
      (push (eq ?| (elt line i)) rtn)
      (setq i (+ i 4)))
    (nreverse rtn)))

(defun parse-maze (file-name)
  (let ((rtn)
        (lines (with-temp-buffer
                 (insert-file-contents-literally file-name)
                 (split-string (buffer-string) "\n" t))))
    (while lines
      (push (parse-ceilings (pop lines)) rtn)
      (push (parse-walls (pop lines)) rtn))
    (nreverse rtn)))

(defun read-maze (file-name)
  (let* ((raw (parse-maze file-name))
         (rows (1- (/ (length raw) 2)))
         (cols (length (car raw)))
         (maze (new-maze rows cols)))
    (dotimes (r rows)
      (let ((ceilings (pop raw)))
        (dotimes (c cols)
          (unless (pop ceilings)
            (maze-unset maze r c 'ceiling))))
      (let ((walls (pop raw)))
        (dotimes (c cols)
          (unless (pop walls)
            (maze-unset maze r c 'wall)))))
    maze))

(defun find-exits (maze row col)
  (let (exits)
    (dolist (p '((0 . +1) (0 . -1) (-1 . 0) (+1 . 0)))
      (let ((r (+ row (car p)))
            (c (+ col (cdr p))))
        (unless
            (cond
             ((equal p '(0 . +1)) (maze-is-set maze r   c   'wall))
             ((equal p '(0 . -1)) (maze-is-set maze row col 'wall))
             ((equal p '(+1 . 0)) (maze-is-set maze r   c   'ceiling))
             ((equal p '(-1 . 0)) (maze-is-set maze row col 'ceiling)))
          (push (cons r c) exits))))
    exits))

(defun drop-visited (maze points)
  (let (not-visited)
    (while points
      (unless (maze-is-set maze (caar points) (cdar points) 'visited)
        (push (car points) not-visited))
      (pop points))
    not-visited))

(defun solve-maze (maze)
  (let (solution
        (exit (cons (- (maze-rows maze) 2) (- (maze-cols maze) 2)))
        (pt (cons 0 0)))
    (while (not (equal pt exit))
      (maze-set maze (car pt) (cdr pt) 'visited)
      (let ((exits (drop-visited maze (find-exits maze (car pt) (cdr pt)))))
        (if (null exits)
            (setq pt (pop solution))
          (push pt solution)
          (setq pt (pop exits)))))
    (push pt solution)))

(defun solve (file-name)
  (let* ((maze (read-maze file-name))
         (solution (solve-maze maze)))
    (print-maze maze solution)))

(provide 'maze)

```

file: maze-generate

```lisp

#!/usr/bin/env emacs -script
;; -*- lexical-binding: t -*-
;;> Simple maze generator.
;;> Example: ./maze-generate 20 20

(add-to-list 'load-path (file-name-directory load-file-name))
(require 'cl-lib)
(require 'maze)

(generate (string-to-number (elt command-line-args-left 0))
          (string-to-number (elt command-line-args-left 1)))

```


<pre style="height:35ex;overflow:scroll;">
+   +---+---+---+---+---+---+---+---+---+
|           |   |                   |   |
+---+---+   +   +---+---+   +---+---+   +
|   |       |   |       |   |       |   |
+   +   +   +   +---+   +   +   +---+   +
|       |               |           |   |
+---+---+---+---+---+   +---+---+   +   +
|   |       |   |   |       |   |   |   |
+   +---+   +   +   +---+   +   +   +   +
|   |   |   |   |               |       |
+   +   +   +   +---+   +   +   +---+   +
|   |   |   |           |   |           |
+   +   +   +---+---+---+   +---+---+   +
|   |   |               |   |   |       |
+   +   +---+---+   +   +   +   +   +   +
|       |   |       |       |       |   |
+   +   +   +---+---+---+---+---+   +   +
|   |       |       |               |   |
+   +---+---+   +   +   +---+---+---+   +
|               |       |               |
+---+---+---+---+---+---+---+---+---+   +

```

=={{header|F_Sharp|F#}}==
Using mutable state in the form of 2D arrays:

```fsharp
let rnd : int -> int =
  let gen = new System.Random()
  fun max -> gen.Next(max)

// randomly choose an element of a list
let choose (xs:_ list) = xs.[rnd xs.Length]

type Maze(width, height) =
  // (x,y) -> have we been here before?
  let visited = Array2D.create width height false
  // (x,y) -> is there a wall between (x,y) and (x+1,y)?
  let horizWalls = Array2D.create width height true
  // (x,y) -> is there a wall between (x,y) and (x,y+1)?
  let vertWalls = Array2D.create width height  true

  let isLegalPoint (x,y) =
    x >= 0 && x < width && y >= 0 && y < height

  let neighbours (x,y) =
    [(x-1,y);(x+1,y);(x,y-1);(x,y+1)] |> List.filter isLegalPoint

  let removeWallBetween (x1,y1) (x2,y2) =
    if x1 <> x2 then
      horizWalls.[min x1 x2, y1] <- false
    else
      vertWalls.[x1, min y1 y2] <- false

  let rec visit (x,y as p) =
    let rec loop ns =
      let (nx,ny) as n = choose ns
      if not visited.[nx,ny] then
        removeWallBetween p n
        visit n
      match List.filter ((<>) n) ns with
      | [] -> ()
      | others -> loop others

    visited.[x,y] <- true
    loop (neighbours p)

  do visit (rnd width, rnd height)

  member x.Print() =
    ("+" + (String.replicate width "-+")) ::
    [for y in 0..(height-1) do
       yield "\n|"
       for x in 0..(width-1) do
         yield if horizWalls.[x,y] then " |" else "  "
       yield "\n+"
       for x in 0..(width-1) do
         yield if vertWalls.[x,y] then "-+" else " +"
    ]
    |> String.concat ""
    |> printfn "%s"

let m = new Maze(10,10)
m.Print()
```

```txt
+-+-+-+-+-+-+-+-+-+-+
|         |     |   |
+ +-+-+-+-+ +-+ + + +
|       |   |   | | |
+ +-+-+ + +-+-+ +-+ +
|     | |     |     |
+-+ +-+ +-+-+ +-+-+ +
|   |   |     |     |
+ +-+ +-+ +-+-+-+ +-+
| | |   |       |   |
+ + +-+ +-+ +-+ +-+ +
| |   | | |   |   | |
+ + +-+ + +-+-+-+ + +
|   |   |         | |
+-+ + +-+-+-+-+-+-+ +
|   |     |       | |
+ +-+-+ +-+ +-+-+ + +
| |   |   |     |   |
+ +-+ +-+ +-+-+ +-+-+
|       |           |
+-+-+-+-+-+-+-+-+-+-+
```


=={{header|Fōrmulæ}}==

In [https://wiki.formulae.org/Maze_generation this] page you can see the solution of this task.

Fōrmulæ programs are not textual, visualization/edition of programs is done showing/manipulating structures but not text ([http://wiki.formulae.org/Editing_F%C5%8Drmul%C3%A6_expressions more info]). Moreover, there can be multiple visual representations of the same program. Even though it is possible to have textual representation &mdash;i.e. XML, JSON&mdash; they are intended for transportation effects more than visualization and edition.

The option to show Fōrmulæ programs and their results is showing images. Unfortunately images cannot be uploaded in Rosetta Code.


## FreeBASIC


```freebasic
' version 04-12-2016
' compile with: fbc -s console
' when generating a big maze it's possible to run out of stack space
' increase stack with the -t xxxx (xxxx is the amount you want in Kbytes)

ReDim Shared As String d() ' directions
ReDim Shared As ULong c()  ' cell's

Sub cell(x As ULong, y As ULong, s As ULong)

    Dim As ULong x1, y1, di_n
    c(x,y) = 1 ' mark as visited

    Do
        Dim As String di = d(x, y)
        Dim As Long l = Len(di) -1
        If l < 0 Then Exit Sub ' no directions left then exit
        di_n = di[l] ' get direction
        If l = 0 Then
            d(x,y) = ""
        Else
            d(x,y) = Left(di,l)
        End If

        Select Case di_n ' 0,0 is upper left corner
            Case Asc("N")
                x1 = x    : y1 = y -1
            Case Asc("E")
                x1 = x +1 : y1 = y
            Case Asc("S")
                x1 = x    : y1 = y +1
            Case Asc("W")
                x1 = x -1 : y1 = y
        End Select

        If c(x1,y1) <> 0 Then Continue Do

        Select Case di_n ' 0,0 is upper left corner
            Case Asc("N")
                Line (x * s +1 , y * s) - ((x +1) * s -1, y * s),0
            Case Asc("E")
                Line (x1 * s, y * s +1) - (x1 * s, (y +1) * s -1),0
            Case Asc("S")
                Line (x * s +1, y1 * s) - ((x +1) * s -1, y1 * s),0
            Case Asc("W")
                Line (x * s , y * s +1) - (x * s, (y +1) * s -1),0
        End Select

        cell(x1, y1, s)
    Loop

End Sub

Sub gen_maze(w As ULong, h As ULong, s As ULong)

    ReDim d(w, h)
    ReDim c(w, h)
    Dim As ULong x, y, r, i
    Dim As String di

    d(0, 0) = "SE"       ' cornes
    d(0, h -1) ="NE"
    d(w -1, 0) ="SW"
    d(w -1, h -1) ="NW"

    For x = 1 To w -2  ' sides
        d(x,0) = "EWS"
        d(x,h -1) = "NEW"
    Next

    For y = 1 To h -2
        d(0, y) = "NSE"
        d(w -1, y) ="NSW"
    Next

    For x = 0 To w -1     ' shuffle directions
        For y = 0 To h -1
            di = d(x,y)
            If di = "" Then di = "NEWS"
            i = Len(di)
            Do
                r = Fix(Rnd * i)
                i = i - 1
                Swap di[r], di[i]
            Loop Until i = 0
            d(x,y) = di
        Next
    Next

    ScreenRes w * s +1, h * s +1, 8
    ' draw the grid
    For x = 0 To w
        Line (x * s, 0) - (x * s, h * s), 2 ' green color
    Next

    For y = 0 To h
        Line(0, y * s) - (w* s, y * s),2
    Next
    ' choice the start cell
    x = Fix(Rnd * w)
    y = Fix(Rnd * h)

    cell(x, y, s)

End Sub

' ------=< MAIN >=------

Randomize Timer

Dim As ULong t

Do
    ' gen_maxe(width, height, cell size)
    gen_maze(30, 30, 20)
    WindowTitle " S to save, N for next maze, other key to stop"
    Do
    Var key = Inkey
    key = UCase(key)
    If key = "S" Then
        t = t +1
        BSave("maze" + Str(t) + ".bmp"), 0
        key = ""
    End If
    If key = "N" Then Continue Do, Do
    If key <> "" Then Exit Do, Do
    Loop
Loop

End
```



## Go


```go
package main

import (
    "bytes"
    "fmt"
    "math/rand"
    "time"
)

type maze struct {
    c  []byte   // cell contents
    h  []byte   // horizontal walls above cells
    v  []byte   // vertical walls to the left of cells
    c2 [][]byte // cells by row
    h2 [][]byte // horizontal walls by row (ignore first row)
    v2 [][]byte // vertical walls by row (ignore first of each column)
}

func newMaze(rows, cols int) *maze {
    c := make([]byte, rows*cols)              // all cells
    h := bytes.Repeat([]byte{'-'}, rows*cols) // all horizontal walls
    v := bytes.Repeat([]byte{'|'}, rows*cols) // all vertical walls
    c2 := make([][]byte, rows)                // cells by row
    h2 := make([][]byte, rows)                // horizontal walls by row
    v2 := make([][]byte, rows)                // vertical walls by row
    for i := range h2 {
        c2[i] = c[i*cols : (i+1)*cols]
        h2[i] = h[i*cols : (i+1)*cols]
        v2[i] = v[i*cols : (i+1)*cols]
    }
    return &maze{c, h, v, c2, h2, v2}
}

func (m *maze) String() string {
    hWall := []byte("+---")
    hOpen := []byte("+   ")
    vWall := []byte("|   ")
    vOpen := []byte("    ")
    rightCorner := []byte("+\n")
    rightWall := []byte("|\n")
    var b []byte
    // for all rows
    for r, hw := range m.h2 {
        // draw h walls
        for _, h := range hw {
            if h == '-' || r == 0 {
                b = append(b, hWall...)
            } else {
                b = append(b, hOpen...)
            }
        }
        b = append(b, rightCorner...)
        // draw v walls
        for c, vw := range m.v2[r] {
            if vw == '|' || c == 0 {
                b = append(b, vWall...)
            } else {
                b = append(b, vOpen...)
            }
            // draw cell contents
            if m.c2[r][c] != 0 {
                b[len(b)-2] = m.c2[r][c]
            }
        }
        b = append(b, rightWall...)
    }
    // draw bottom edge of maze
    for _ = range m.h2[0] {
        b = append(b, hWall...)
    }
    b = append(b, rightCorner...)
    return string(b)
}

func (m *maze) gen() {
    m.g2(rand.Intn(len(m.c2)), rand.Intn(len(m.c2[0])))
}

const (
    up = iota
    dn
    rt
    lf
)

func (m *maze) g2(r, c int) {
    m.c2[r][c] = ' '
    for _, dir := range rand.Perm(4) {
        switch dir {
        case up:
            if r > 0 && m.c2[r-1][c] == 0 {
                m.h2[r][c] = 0
                m.g2(r-1, c)
            }
        case lf:
            if c > 0 && m.c2[r][c-1] == 0 {
                m.v2[r][c] = 0
                m.g2(r, c-1)
            }
        case dn:
            if r < len(m.c2)-1 && m.c2[r+1][c] == 0 {
                m.h2[r+1][c] = 0
                m.g2(r+1, c)
            }
        case rt:
            if c < len(m.c2[0])-1 && m.c2[r][c+1] == 0 {
                m.v2[r][c+1] = 0
                m.g2(r, c+1)
            }
        }
    }
}

func main() {
    rand.Seed(time.Now().UnixNano())
    m := newMaze(4, 6)
    m.gen()
    fmt.Print(m)
}
```

```txt

+---+---+---+---+---+---+
|   |           |       |
+   +   +   +---+   +---+
|   |   |           |   |
+   +   +---+---+---+   +
|   |   |               |
+   +   +   +---+---+   +
|           |           |
+---+---+---+---+---+---+

```



## Haskell


```haskell
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

import Data.Array.ST
       (STArray, freeze, newArray, readArray, writeArray)
import Data.STRef (STRef, newSTRef, readSTRef, writeSTRef)
import System.Random (Random(..), getStdGen, StdGen)
import Control.Monad (forM_, unless)
import Control.Monad.ST (ST, stToIO)
import Data.Array (Array, (!), bounds)
import Data.Bool (bool)

rand
  :: Random a
  => (a, a) -> STRef s StdGen -> ST s a
rand range gen = do
  (a, g) <- randomR range <$> readSTRef gen
  gen `writeSTRef` g
  return a

data Maze = Maze
  { rightWalls, belowWalls :: Array (Int, Int) Bool
  }

maze :: Int -> Int -> StdGen -> ST s Maze
maze width height gen = do
  visited <- mazeArray False
  rWalls <- mazeArray True
  bWalls <- mazeArray True
  gen <- newSTRef gen
  (,) <$> rand (0, maxX) gen <*> rand (0, maxY) gen >>=
    visit gen visited rWalls bWalls
  Maze <$> freeze rWalls <*> freeze bWalls
  where
    visit gen visited rWalls bWalls here = do
      writeArray visited here True
      let ns = neighbors here
      i <- rand (0, length ns - 1) gen
      forM_ (ns !! i : take i ns ++ drop (i + 1) ns) $
        \there -> do
          seen <- readArray visited there
          unless seen $
            do removeWall here there
               visit gen visited rWalls bWalls there
      where
        removeWall (x1, y1) (x2, y2) =
          writeArray (bool rWalls bWalls (x1 == x2)) (min x1 x2, min y1 y2) False
    neighbors (x, y) =
      bool [(x - 1, y)] [] (0 == x) ++
      bool [(x + 1, y)] [] (maxX == x) ++
      bool [(x, y - 1)] [] (0 == y) ++ bool [(x, y + 1)] [] (maxY == y)
    maxX = width - 1
    maxY = height - 1
    mazeArray =
      newArray ((0, 0), (maxX, maxY)) :: Bool -> ST s (STArray s (Int, Int) Bool)

printMaze :: Maze -> IO ()
printMaze (Maze rWalls bWalls) = do
  putStrLn $ '+' : concat (replicate (maxX + 1) "---+")
  forM_ [0 .. maxY] $
    \y -> do
      putStr "|"
      forM_ [0 .. maxX] $
        \x -> do
          putStr "   "
          putStr $ bool " " "|" (rWalls ! (x, y))
      putStrLn ""
      forM_ [0 .. maxX] $
        \x -> do
          putStr "+"
          putStr $ bool "   " "---" (bWalls ! (x, y))
      putStrLn "+"
  where
    maxX = fst (snd $ bounds rWalls)
    maxY = snd (snd $ bounds rWalls)

main :: IO ()
main = getStdGen >>= stToIO . maze 11 8 >>= printMaze
```

  +---+---+---+---+---+---+---+---+---+---+---+
  |               |                           |
  +   +---+---+---+   +---+---+---+---+---+   +
  |               |           |   |       |   |
  +   +---+---+   +---+---+   +   +   +   +   +
  |   |   |       |           |       |   |   |
  +   +   +   +---+---+---+---+   +---+   +   +
  |       |   |                   |   |       |
  +---+---+   +   +---+---+---+---+   +---+---+
  |       |   |   |                       |   |
  +   +   +   +   +---+---+---+   +---+   +   +
  |   |       |   |               |       |   |
  +   +---+---+   +   +---+---+---+   +---+   +
  |               |       |           |       |
  +   +---+---+---+---+   +   +---+---+   +   +
  |                       |               |   |
  +---+---+---+---+---+---+---+---+---+---+---+


## Huginn


```huginn
import Algorithms as algo;
import Mathematics as math;
import Terminal as term;

class Maze {
	_rows = none;
	_cols = none;
	_data = none;
	constructor( rows_, cols_ ) {
		_rows = ( rows_ / 2 ) * 2 - 1;
		_cols = ( cols_ / 2 ) * 2 - 1;
		_data = [].resize( _rows + 2, [].resize( _cols + 2, false ) );
		x = 0;
		y = 0;
		path = [];
		rng = math.Randomizer( math.Randomizer.DISTRIBUTION.DISCRETE, 0, integer( $2 ^ $63 - $1 ) );
		for ( _ : algo.range( _rows * _cols / 3 ) ) {
			_data[y + 1][x + 1] = true;
			while ( true ) {
				n = neighbours( y, x );
				ns = size( n );
				if ( ns == 0 ) {
					if ( size( path ) == 0 ) {
						break;
					}
					y, x = path[-1];
					path.pop();
					continue;
				}
				oy, ox = ( y, x );
				y, x = n[rng.next() % ns];
				_data[(y + oy) / 2 + 1][(x + ox) / 2 + 1] = true;
				path.push( ( y, x ) );
				break;
			}
		}
		_data[0][1] = true;
		_data[-1][-2] = true;
	}
	neighbours( y_, x_ ) {
		n = [];
		if ( ( x_ > 1 ) && ! _data[y_ + 1][x_ - 1] ) {
			n.push( ( y_, x_ - 2 ) );
		}
		if ( ( y_ > 1 ) && ! _data[y_ - 1][x_ + 1] ) {
			n.push( ( y_ - 2, x_ ) );
		}
		if ( ( x_ < ( _cols - 2 ) ) && ! _data[y_ + 1][x_ + 3] ) {
			n.push( ( y_, x_ + 2 ) );
		}
		if ( ( y_ < ( _rows - 2 ) ) && ! _data[y_ + 3][x_ + 1] ) {
			n.push( ( y_ + 2, x_ ) );
		}
		return ( n );
	}
	to_string() {
		s = "";
		for ( r : _data ) {
			s += ∑( algo.map( r, @( b ) { b ? " " : "#"; } ) );
			s += "\n";
		}
		return ( s );
	}
}

main() {
	rows = term.lines() - 2;
	cols = term.columns() - 1;
	maze = Maze( rows, cols );
	print( "{}".format( maze ) );
}
```


=={{header|Icon}} and {{header|Unicon}}==
[[File:Mazegen-unicon-20x30-1321112170.gif|thumb|right|20x30 with two random openings]]
[[File:Mazegen-unicon-20x30-1321060467.gif|thumb|right|20x30 with opposite openings]]

```Icon
link printf

procedure main(A)                               # generate rows x col maze
   /mh := \A[1] | 12                            # or take defaults 12 x 16
   /mw := \A[2] | 16
   mz := DisplayMaze(GenerateMaze(mh,mw))
   WriteImage(mz.filename)                      # save file
   WAttrib(mz.window,"canvas=normal")           # show maze in hidden window
   until Event() == &lpress                     # wait for left mouse press
   close(mz.window)
end

$define FINISH 64 # exit
$define START  32 # entrance
$define PATH  128
$define SEEN   16 # bread crumbs for generator
$define NORTH   8 # sides ...
$define EAST    4
$define SOUTH   2
$define WEST    1
$define EMPTY   0 # like new

procedure GenerateMaze(r,c)                     #: Depth First Maze Generation
static maze,h,w,rd
   if /maze then {                              # BEGING - No maze yet
      /h := integer(1 < r) | runerr(r,205)      # valid size 2x2 or better
      /w := integer(1 < c) | runerr(r,205)
      every !(maze := list(h)) := list(w,EMPTY) # shinny new empty maze
      start  := [?h,?w,?4-1,START]              # random [r,c] start & finish
      finish := [?h,?w,(start[3]+2)%4,FINISH]   # w/ opposite side exponent
      every x := start | finish do {
         case x[3] := 2 ^ x[3] of {             # get side from exponent and
            NORTH : x[1] := 1                   # project r,c to selected edge
            EAST  : x[2] := w
            SOUTH : x[1] := h
            WEST  : x[2] := 1
            }
         maze[x[1],x[2]] +:= x[3] + x[4]        # transcribe s/f to maze
         }
      rd := [NORTH, EAST, SOUTH, WEST]          # initial list of directions
      GenerateMaze(start[1],start[2])           # recurse through maze
      return 1(.maze,maze := &null)             # return maze, reset for next
   }
   else {         # ----------------------- recursed to clear insize of maze
      if iand(maze[r,c],SEEN) = 0 then {        # in bounds and not SEEN yet?
         maze[r,c] +:= SEEN                     # Mark current cell as visited
         every !rd :=: ?rd                      # randomize list of directions
         every d := !rd do
            case d of {                         # try all, succeed & clear wall
               NORTH :  maze[r,c] +:= ( GenerateMaze(r-1,c), NORTH)
               EAST  :  maze[r,c] +:= ( GenerateMaze(r,c+1),  EAST)
               SOUTH :  maze[r,c] +:= ( GenerateMaze(r+1,c), SOUTH)
               WEST  :  maze[r,c] +:= ( GenerateMaze(r,c-1),  WEST)
               }
         return                                 # signal success to caller
         }
   }
end

$define CELL   20                                   # cell size in pixels
$define BORDER 30                                   # border size in pixels

record mazeinfo(window,maze,filename)               # keepers

procedure DisplayMaze(maze)                         #: show it off
if CELL < 8 then runerr(205,CELL)                   # too small

wh := (ch := (mh := *maze  ) * CELL) + 2 * BORDER   # win, cell, maze height
ww := (cw := (mw := *maze[1]) * CELL) + 2 * BORDER  # win, cell, maze width

wparms := [ sprintf("Maze %dx%d",*maze,*maze[1]),   # window parameters
            "g","bg=white","canvas=hidden",
            sprintf("size=%d,%d",ww,wh),
            sprintf("dx=%d",BORDER),
            sprintf("dy=%d",BORDER)]

&window := open!wparms | stop("Unable to open Window")

Fg("black")                                         # Draw full grid
every DrawLine(x := 0 to cw by CELL,0,x,ch+1)       # . verticals
every DrawLine(0,y := 0 to ch by CELL,cw+1,y)       # . horizontals

Fg("white")                                         # Set to erase lines
every y := CELL*((r := 1 to mh)-1) & x := CELL*((c := 1 to mw)-1) do {
   WAttrib("dx="||x+BORDER,"dy="||y+BORDER)         # position @ cell r,c
   if iand(maze[r,c],NORTH) > 0 then DrawLine(2,0,CELL-1,0)
   if iand(maze[r,c],EAST)  > 0 then DrawLine(CELL,2,CELL,CELL-1)
   if iand(maze[r,c],SOUTH) > 0 then DrawLine(2,CELL,CELL-1,CELL)
   if iand(maze[r,c],WEST)  > 0 then DrawLine(0,2,0,CELL-1)
   }

return mazeinfo(&window,maze,sprintf("maze-%dx%d-%d.gif",r,c,&now))
end
```

Note: The underlying maze structure (matrix) is uni-directional from the start
[http://www.cs.arizona.edu/icon/library/src/procs/printf.icn printf.icn provides formatting]


## J

This algorithm allows almost no parallelism.  So, while it might be "simple", generating very large mazes this way will not be necessarily efficient to implement on future (highly parallel) systems.  That said, perhaps mazes with millions of cells are not very likely to be needed to be generated quickly.

But without any relevant grid library:

```j
maze=:4 :0
  assert.0<:n=.<:x*y
  horiz=. 0$~x,y-1
  verti=. 0$~(x-1),y
  path=.,:here=. ?x,y
  unvisited=.0 (<here+1)} 0,0,~|:0,0,~1$~y,x
  while.n do.
    neighbors=. here+"1 (,-)=0 1
    neighbors=. neighbors #~ (<"1 neighbors+1) {unvisited
    if.#neighbors do.
      n=.n-1
      next=. ({~ ?@#) neighbors
      unvisited=.0 (<next+1)} unvisited
      if.{.next=here
      do. horiz=.1 (<-:here+next-0 1)} horiz
      else. verti=. 1 (<-:here+next-1 0)} verti end.
      path=.path,here=.next
    else.
      here=.{:path
      path=.}:path
    end.
  end.
  horiz;verti
)

display=:3 :0
  size=. >.&$&>/y
  text=. (}:1 3$~2*1+{:size)#"1":size$<' '
  'hdoor vdoor'=. 2 4&*&.>&.> (#&,{@;&i./@$)&.> y
  ' ' (a:-.~0 1;0 2; 0 3;(2 1-~$text);(1 4&+&.> hdoor),,vdoor+&.>"0/2 1;2 2;2 3)} text
)
```

The result of <code>maze</code> is a pair of arrays: one for open "doors" in the horizontal direction and the other for open "doors" in the vertical direction.  The entry and exit doors are not represented by <code>maze</code> -- they are implicitly defined and are implemented in <code>display</code>. (The sequences of coordinates in <code>display</code> are the relative coordinates for the doors. For example, <code>2 1;2 2;2 3</code> are where we put spaces for each vertical door. The variable <code>text</code> is an ascii representation of the maze grid before the doors are placed.)

```j
   display 8 maze 11
+   +---+---+---+---+---+---+---+---+---+---+
|       |           |                   |   |
+   +   +   +   +---+   +   +---+---+   +   +
|   |       |   |       |           |   |   |
+   +---+---+   +   +---+---+---+   +   +   +
|   |           |   |           |   |       |
+---+   +---+   +   +   +---+   +   +---+---+
|       |       |   |       |   |           |
+   +   +---+---+   +---+   +   +---+---+   +
|   |   |       |   |   |   |           |   |
+   +---+   +   +   +   +   +---+---+   +   +
|           |           |           |       |
+   +---+---+---+---+---+---+---+   +---+   +
|   |   |   |       |       |       |   |   |
+   +   +   +   +   +   +   +   +---+   +   +
|       |       |       |       |
+---+---+---+---+---+---+---+---+---+---+---+
```



## Java

```java5
package org.rosettacode;

import java.util.Collections;
import java.util.Arrays;

/*
 * recursive backtracking algorithm
 * shamelessly borrowed from the ruby at
 * http://weblog.jamisbuck.org/2010/12/27/maze-generation-recursive-backtracking
 */
public class MazeGenerator {
	private final int x;
	private final int y;
	private final int[][] maze;

	public MazeGenerator(int x, int y) {
		this.x = x;
		this.y = y;
		maze = new int[this.x][this.y];
		generateMaze(0, 0);
	}

	public void display() {
		for (int i = 0; i < y; i++) {
			// draw the north edge
			for (int j = 0; j < x; j++) {
				System.out.print((maze[j][i] & 1) == 0 ? "+---" : "+   ");
			}
			System.out.println("+");
			// draw the west edge
			for (int j = 0; j < x; j++) {
				System.out.print((maze[j][i] & 8) == 0 ? "|   " : "    ");
			}
			System.out.println("|");
		}
		// draw the bottom line
		for (int j = 0; j < x; j++) {
			System.out.print("+---");
		}
		System.out.println("+");
	}

	private void generateMaze(int cx, int cy) {
		DIR[] dirs = DIR.values();
		Collections.shuffle(Arrays.asList(dirs));
		for (DIR dir : dirs) {
			int nx = cx + dir.dx;
			int ny = cy + dir.dy;
			if (between(nx, x) && between(ny, y)
					&& (maze[nx][ny] == 0)) {
				maze[cx][cy] |= dir.bit;
				maze[nx][ny] |= dir.opposite.bit;
				generateMaze(nx, ny);
			}
		}
	}

	private static boolean between(int v, int upper) {
		return (v >= 0) && (v < upper);
	}

	private enum DIR {
		N(1, 0, -1), S(2, 0, 1), E(4, 1, 0), W(8, -1, 0);
		private final int bit;
		private final int dx;
		private final int dy;
		private DIR opposite;

		// use the static initializer to resolve forward references
		static {
			N.opposite = S;
			S.opposite = N;
			E.opposite = W;
			W.opposite = E;
		}

		private DIR(int bit, int dx, int dy) {
			this.bit = bit;
			this.dx = dx;
			this.dy = dy;
		}
	};

	public static void main(String[] args) {
		int x = args.length >= 1 ? (Integer.parseInt(args[0])) : 8;
		int y = args.length == 2 ? (Integer.parseInt(args[1])) : 8;
		MazeGenerator maze = new MazeGenerator(x, y);
		maze.display();
	}

}
```

```txt
+---+---+---+---+---+---+---+---+---+---+
|   |                           |       |
+   +---+---+   +---+---+   +   +   +---+
|           |   |   |       |   |       |
+---+---+   +   +   +   +---+   +---+   +
|           |       |   |   |       |   |
+   +---+---+   +---+   +   +---+   +   +
|   |       |   |       |           |   |
+   +   +   +---+   +---+---+---+   +   +
|   |   |       |               |       |
+   +   +---+   +   +---+---+   +---+---+
|   |       |   |   |           |       |
+   +---+   +   +---+   +---+---+   +   +
|       |   |       |               |   |
+---+   +   +---+   +   +---+---+---+   +
|   |   |       |   |       |           |
+   +   +---+   +   +---+---+   +---+   +
|   |       |   |           |   |   |   |
+   +---+   +   +---+---+   +   +   +   +
|               |               |       |
+---+---+---+---+---+---+---+---+---+---+
```



## JavaScript

```javascript
function maze(x,y) {
	var n=x*y-1;
	if (n<0) {alert("illegal maze dimensions");return;}
	var horiz =[]; for (var j= 0; j<x+1; j++) horiz[j]= [],
	    verti =[]; for (var j= 0; j<x+1; j++) verti[j]= [],
	    here = [Math.floor(Math.random()*x), Math.floor(Math.random()*y)],
	    path = [here],
	    unvisited = [];
	for (var j = 0; j<x+2; j++) {
		unvisited[j] = [];
		for (var k= 0; k<y+1; k++)
			unvisited[j].push(j>0 && j<x+1 && k>0 && (j != here[0]+1 || k != here[1]+1));
	}
	while (0<n) {
		var potential = [[here[0]+1, here[1]], [here[0],here[1]+1],
		    [here[0]-1, here[1]], [here[0],here[1]-1]];
		var neighbors = [];
		for (var j = 0; j < 4; j++)
			if (unvisited[potential[j][0]+1][potential[j][1]+1])
				neighbors.push(potential[j]);
		if (neighbors.length) {
			n = n-1;
			next= neighbors[Math.floor(Math.random()*neighbors.length)];
			unvisited[next[0]+1][next[1]+1]= false;
			if (next[0] == here[0])
				horiz[next[0]][(next[1]+here[1]-1)/2]= true;
			else
				verti[(next[0]+here[0]-1)/2][next[1]]= true;
			path.push(here = next);
		} else
			here = path.pop();
	}
	return {x: x, y: y, horiz: horiz, verti: verti};
}

function display(m) {
	var text= [];
	for (var j= 0; j<m.x*2+1; j++) {
		var line= [];
		if (0 == j%2)
			for (var k=0; k<m.y*4+1; k++)
				if (0 == k%4)
					line[k]= '+';
				else
					if (j>0 && m.verti[j/2-1][Math.floor(k/4)])
						line[k]= ' ';
					else
						line[k]= '-';
		else
			for (var k=0; k<m.y*4+1; k++)
				if (0 == k%4)
					if (k>0 && m.horiz[(j-1)/2][k/4-1])
						line[k]= ' ';
					else
						line[k]= '|';
				else
					line[k]= ' ';
		if (0 == j) line[1]= line[2]= line[3]= ' ';
		if (m.x*2-1 == j) line[4*m.y]= ' ';
		text.push(line.join('')+'\r\n');
	}
	return text.join('');
}
```

Variable meanings in function <code>maze</code>:
# <code>x</code>,<code>y</code> — dimensions of maze
# <code>n</code> — number of openings to be generated
# <code>horiz</code> — two dimensional array of locations of horizontal openings (true means wall is open)
# <code>verti</code> — two dimensional array of locations of vertical openings (true means wall is open)
# <code>here</code> — current location under consideration
# <code>path</code> — history (stack) of locations that might need to be revisited
# <code>unvisited</code> — two dimensional array of locations that have not been visited, padded to avoid need for boundary tests (true means location needs to be visited)
# <code>potential</code> — locations adjacent to <code>here</code>
# <code>neighbors</code> — unvisited locations adjacent to <code>here</code>
Variable meanings in function <code>display</code>:
# <code>m</code> — maze to be drawn
# <code>text</code> — lines of text representing maze
# <code>line</code> — characters of current line
Note that this implementation relies on javascript arrays being treatable as infinite in size with false (null) values springing into existence as needed, to support referenced array locations.  (This significantly reduces the bulk of the necessary initialization code.)

```html
<html><head><title></title></head><body>
<pre id="out">
```
</body></html>
<script type="text/javascript">
/* ABOVE CODE GOES HERE */
document.getElementById('out').innerHTML= display(maze(8,11));
</script>
```

produced output:

```txt
+   +---+---+---+---+---+---+---+---+---+---+
|                   |                   |   |
+---+---+   +   +---+   +   +---+---+   +   +
|       |   |   |       |   |           |   |
+   +   +   +---+   +---+   +---+---+   +   +
|   |   |               |           |   |   |
+   +---+   +---+---+---+---+---+   +   +   +
|       |   |               |       |       |
+---+   +---+   +---+---+   +   +---+---+   +
|   |   |       |               |       |   |
+   +   +   +---+---+---+---+---+   +   +   +
|       |                   |       |   |   |
+   +---+---+   +---+---+   +   +---+---+   +
|   |       |   |           |       |       |
+   +   +   +---+   +---+---+   +   +   +---+
|       |           |           |
+---+---+---+---+---+---+---+---+---+---+---+
```

For an animated presentation of the progress of this maze creation process, you can use <code>display</code> in each iteration of the main loop.  You would also need to take steps to make sure you could see each intermediate result.

For example, change replace the line <code>while (0<n) {</code> with:

```javascript
	function step() {
		if (0<n) {
```

And replace the closing brace for this while loop with:

```javascript
			document.getElementById('out').innerHTML= display({x: x, y: y, horiz: horiz, verti: verti, here: here});
			setTimeout(step, 100);
		}
	}
	step();
```

To better see the progress, you might want a marker in place, showing the position being considered.  To do that, replace the line which reads <code>if (0 == k%4) {</code> with

```javascript
				if (m.here && m.here[0]*2+1 == j && m.here[1]*4+2 == k)
					line[k]= '#'
				else if (0 == k%4) {
```

Note however that this leaves the final '#' in place on maze completion, and that the function <code>maze</code> no longer returns a result which represents a generated maze.

Note also that this display suggests an optimization.  You can replace the line reading <code>path.push(here= next);</code> with:

```javascript
			here= next;
			if (1 < neighbors.length)
				path.push(here);
```

And this does indeed save a negligible bit of processing, but the maze algorithm will still be forced to backtrack through a number of locations which have no unvisited neighbors.

### HTML Table

Using HTML, CSS and table cells for maze.

```html
<html><head><title>Maze maker</title>

<style type="text/css">
table { border-collapse: collapse }
td { width: 1em; height: 1em; border: 1px solid }
td.s { border-bottom: none }
td.n { border-top: none }
td.w { border-left: none }
td.e { border-right: none }
td.v { background: skyblue}
</style>
<script type="application/javascript">
Node.prototype.add = function(tag, cnt, txt) {
	for (var i = 0; i < cnt; i++)
		this.appendChild(ce(tag, txt));
}
Node.prototype.ins = function(tag) {
	this.insertBefore(ce(tag), this.firstChild)
}
Node.prototype.kid = function(i) { return this.childNodes[i] }
Node.prototype.cls = function(t) { this.className += ' ' + t }

NodeList.prototype.map = function(g) {
	for (var i = 0; i < this.length; i++) g(this[i]);
}

function ce(tag, txt) {
	var x = document.createElement(tag);
	if (txt !== undefined) x.innerHTML = txt;
	return x
}

function gid(e) { return document.getElementById(e) }
function irand(x) { return Math.floor(Math.random() * x) }

function make_maze() {
	var w = parseInt(gid('rows').value || 8, 10);
	var h = parseInt(gid('cols').value || 8, 10);
	var tbl = gid('maze');
	tbl.innerHTML = '';
	tbl.add('tr', h);
	tbl.childNodes.map(function(x) {
			x.add('th', 1);
			x.add('td', w, '*');
			x.add('th', 1)});
	tbl.ins('tr');
	tbl.add('tr', 1);
	tbl.firstChild.add('th', w + 2);
	tbl.lastChild.add('th', w + 2);
	for (var i = 1; i <= h; i++) {
		for (var j = 1; j <= w; j++) {
			tbl.kid(i).kid(j).neighbors = [
				tbl.kid(i + 1).kid(j),
				tbl.kid(i).kid(j + 1),
				tbl.kid(i).kid(j - 1),
				tbl.kid(i - 1).kid(j)
			];
		}
	}
	walk(tbl.kid(irand(h) + 1).kid(irand(w) + 1));
	gid('solve').style.display='inline';
}

function shuffle(x) {
	for (var i = 3; i > 0; i--) {
		j = irand(i + 1);
		if (j == i) continue;
		var t = x[j]; x[j] = x[i]; x[i] = t;
	}
	return x;
}

var dirs = ['s', 'e', 'w', 'n'];
function walk(c) {
	c.innerHTML = ' ';
	var idx = shuffle([0, 1, 2, 3]);
	for (var j = 0; j < 4; j++) {
		var i = idx[j];
		var x = c.neighbors[i];
		if (x.textContent != '*') continue;
		c.cls(dirs[i]), x.cls(dirs[3 - i]);
		walk(x);
	}
}

function solve(c, t) {
	if (c === undefined) {
		c = gid('maze').kid(1).kid(1);
		c.cls('v');
	}
	if (t === undefined)
		t = gid('maze')	.lastChild.previousSibling
				.lastChild.previousSibling;

	if (c === t) return 1;
	c.vis = 1;
	for (var i = 0; i < 4; i++) {
		var x = c.neighbors[i];
		if (x.tagName.toLowerCase() == 'th') continue;
		if (x.vis || !c.className.match(dirs[i]) || !solve(x, t))
			continue;

		x.cls('v');
		return 1;
	}
	c.vis = null;
	return 0;
}

</script></head>
<body><form><fieldset>
<label>rows </label><input id='rows' size="3"/>
<label>colums </label><input id='cols' size="3"/>
<a href="javascript:make_maze()">Generate</a>
<a id='solve' style='display:none' href='javascript:solve(); void(0)'>Solve</a>
</fieldset></form><table id='maze'/></body></html>
```



## Node.js

This difers of the basic Javascript in that in NodeJS we take advantage of the asynchronous behaviour. This code was modified from the plain Javascript section to make it '''Asynchronous''' and able to run under ''strict mode''.


```javascript

'use strict';
/*
 * Imported from http://rosettacode.org/wiki/Maze_generation#JavaScript
 * Added asynchronous behaviour to the maze generation.
 *
 * Port by sigmasoldier
 */

/**
 * Generates the maze asynchronously.
 * @param {Number} x Width of the maze.
 * @param {Number} y Height of the maze.
 * @returns {Promise} finished when resolved.
 */
function maze(x,y) {
	return new Promise((resolve, reject) => {
		let n=x*y-1;
		if (n<0) {
			reject(new Error(`illegal maze dimensions (${x} x ${y} < 1)`));
		} else {
			let horiz =[]; for (let j= 0; j<x+1; j++) horiz[j]= [];
			let verti =[]; for (let j= 0; j<x+1; j++) verti[j]= [];
			let here = [Math.floor(Math.random()*x), Math.floor(Math.random()*y)];
			let path = [here];
			let unvisited = [];
			for (let j = 0; j<x+2; j++) {
				unvisited[j] = [];
				for (let k= 0; k<y+1; k++)
					unvisited[j].push(j>0 && j<x+1 && k>0 && (j != here[0]+1 || k != here[1]+1));
			}
			while (0<n) {
				let potential = [[here[0]+1, here[1]], [here[0],here[1]+1],
						[here[0]-1, here[1]], [here[0],here[1]-1]];
				let neighbors = [];
				for (let j = 0; j < 4; j++)
					if (unvisited[potential[j][0]+1][potential[j][1]+1])
						neighbors.push(potential[j]);
				if (neighbors.length) {
					n = n-1;
					let next= neighbors[Math.floor(Math.random()*neighbors.length)];
					unvisited[next[0]+1][next[1]+1]= false;
					if (next[0] == here[0])
						horiz[next[0]][(next[1]+here[1]-1)/2]= true;
					else
						verti[(next[0]+here[0]-1)/2][next[1]]= true;
					path.push(here = next);
				} else
					here = path.pop();
			}
			resolve({x: x, y: y, horiz: horiz, verti: verti});
		}
	});
}

/**
 * A mere way of generating text.
 * The text (Since it can be large) is generated in a non-blocking way.
 * @param {Object} m Maze object.
 * @param {Stream} writeTo Optinally, include here a function to write to.
 * @returns {Promise} finished when the text is generated.
 */
function display(m, writeTo) {
	return new Promise((resolve, reject) => {
		let text = [];
		for (let j= 0; j<m.x*2+1; j++) {
			let line = [];
			if (0 == j%2)
				for (let k=0; k<m.y*4+1; k++)
					if (0 == k%4)
						line[k] = '+';
					else
						if (j>0 && m.verti[j/2-1][Math.floor(k/4)])
							line[k] = ' ';
						else
							line[k] = '-';
			else
				for (let k=0; k<m.y*4+1; k++)
					if (0 == k%4)
						if (k>0 && m.horiz[(j-1)/2][k/4-1])
							line[k] = ' ';
						else
							line[k] = '|';
					else
						line[k] = ' ';
			if (0 == j) line[1] = line[2] = line[3] = ' ';
			if (m.x*2-1 == j) line[4*m.y]= ' ';
			text.push(line.join('')+'\r\n');
		}
		const OUTPUT = text.join('');
		if (typeof writeTo === 'function')
			writeTo(OUTPUT);
		resolve(OUTPUT);
	});
}

module.exports = {
  maze: maze,
  display: display
}

```


Image that you have a <code>main.js</code> file, to run then invoke in your shell <code>node main.js</code>
Here is a basic example of what your main file should contain:


```javascript

'use strict';

const maze = require('./maze.js');
const X = 20,
  Y = 20;

console.log(`Generating a maze of ${X} x ${Y}...`);
const origin = new Date().getTime();

maze.maze(X, Y).then((m) => {
  const time = new Date().getTime() - origin;
  console.log(`Done in ${time <= 1000 ? time+'ms' : Math.round(time/1000)+'s'}!`);
  maze.display(m, console.log); //Here you can pass a given stream (ie: stream) and it's write function;
  //An example could be: maze.display(m, stream.write);
}, (err) => console.error(err));


```


Sample Output:

```txt

$ node main.js
Generating a maze of 10 x 10...
Done in 3ms!
+   +---+---+---+---+---+---+---+---+---+
|                       |               |
+   +---+---+   +---+   +   +---+   +   +
|   |   |       |       |       |   |   |
+   +   +   +---+   +---+---+   +   +   +
|   |   |   |   |           |   |   |   |
+   +   +   +   +---+---+   +   +   +---+
|       |   |           |   |   |       |
+---+   +   +   +---+---+   +---+---+   +
|       |   |       |       |       |   |
+---+---+   +---+   +   +---+   +   +   +
|           |   |   |   |       |   |   |
+   +---+---+   +   +   +   +---+   +   +
|               |       |   |           |
+---+---+---+---+   +---+   +---+---+   +
|           |       |               |   |
+   +---+   +   +---+---+---+---+   +   +
|       |   |   |                   |   |
+---+   +   +   +---+   +---+---+---+   +
|       |               |
+---+---+---+---+---+---+---+---+---+---+

```



## Julia

'''Generating functions'''

```julia
check(bound::Vector) = cell -> all([1, 1] .≤ cell .≤ bound)
neighbors(cell::Vector, bound::Vector, step::Int=2) =
    filter(check(bound), map(dir -> cell + step * dir, [[0, 1], [-1, 0], [0, -1], [1, 0]]))

function walk(maze::Matrix, nxtcell::Vector, visited::Vector=[])
    push!(visited, nxtcell)
    for neigh in shuffle(neighbors(nxtcell, size(maze)))
        if neigh ∉ visited
            maze[round.(Int, (nxtcell + neigh) / 2)...] = 0
            walk(maze, neigh, visited)
        end
    end
    maze
end
function maze(w::Int, h::Int)
    maze = collect(i % 2 | j % 2 for i in 1:2w+1, j in 1:2h+1)
    firstcell = 2 * [rand(1:w), rand(1:h)]
    return walk(maze, firstcell)
end
```


'''Printing functions'''

```julia
pprint(matrix) = for i = 1:size(matrix, 1) println(join(matrix[i, :])) end
function printmaze(maze)
    walls = split("╹ ╸ ┛ ╺ ┗ ━ ┻ ╻ ┃ ┓ ┫ ┏ ┣ ┳ ╋")
    h, w = size(maze)
    f = cell -> 2 ^ ((3cell[1] + cell[2] + 3) / 2)
    wall(i, j) = if maze[i,j] == 0 " " else
        walls[Int(sum(f, filter(x -> maze[x...] != 0, neighbors([i, j], [h, w], 1)) .- [[i, j]]))]
    end
    mazewalls = collect(wall(i, j) for i in 1:2:h, j in 1:w)
    pprint(mazewalls)
end

printmaze(maze(10, 10))

```


```txt
┏━━━━━┳━━━━━┳━━━━━━━┓
┃ ╻ ╻ ┃ ╺━┓ ╹ ┏━━━┓ ┃
┣━┛ ┃ ┗━┓ ┗━━━┛ ╻ ┃ ┃
┃ ╺━┻━┓ ┃ ╺━┳━━━┛ ┃ ┃
┃ ╺━┓ ┃ ┗━━━┛ ┏━━━┛ ┃
┣━━━┛ ┣━━━━━┳━┛ ┏━━━┫
┃ ┏━━━┛ ┏━╸ ┃ ╺━┛ ╻ ┃
┣━┛ ┏━━━┛ ╻ ┣━━━━━┛ ┃
┃ ┏━┛ ┏━━━┻━┛ ┏━━━┳━┫
┃ ┃ ╺━┛ ╺━━━━━┛ ╻ ╹ ┃
┗━┻━━━━━━━━━━━━━┻━━━┛
```



## Kotlin

```scala
import java.util.*

class MazeGenerator(val x: Int, val y: Int) {
    private val maze = Array(x) { IntArray(y) }

    fun generate(cx: Int, cy: Int) {
        Direction.values().shuffle().forEach {
            val nx = cx + it.dx
            val ny = cy + it.dy
            if (between(nx, x) && between(ny, y) && maze[nx][ny] == 0) {
                maze[cx][cy] = maze[cx][cy] or it.bit
                maze[nx][ny] = maze[nx][ny] or it.opposite!!.bit
                generate(nx, ny)
            }
        }
    }

    fun display() {
        for (i in 0..y - 1) {
            // draw the north edge
            for (j in 0..x - 1)
                print(if (maze[j][i] and 1 == 0) "+---" else "+   ")
            println('+')

            // draw the west edge
            for (j in 0..x - 1)
                print(if (maze[j][i] and 8 == 0) "|   " else "    ")
            println('|')
        }

        // draw the bottom line
        for (j in 0..x - 1) print("+---")
        println('+')
    }

    inline private fun <reified T> Array<T>.shuffle(): Array<T> {
        val list = toMutableList()
        Collections.shuffle(list)
        return list.toTypedArray()
    }

    private enum class Direction(val bit: Int, val dx: Int, val dy: Int) {
        N(1, 0, -1), S(2, 0, 1), E(4, 1, 0),W(8, -1, 0);

        var opposite: Direction? = null

        companion object {
            init {
                N.opposite = S
                S.opposite = N
                E.opposite = W
                W.opposite = E
            }
        }
    }

    private fun between(v: Int, upper: Int) = v >= 0 && v < upper
}

fun main(args: Array<String>) {
    val x = if (args.size >= 1) args[0].toInt() else 8
    val y = if (args.size == 2) args[1].toInt() else 8
    with(MazeGenerator(x, y)) {
        generate(0, 0)
        display()
    }
}
```



## Lua

```Lua

math.randomseed( os.time() )

-- Fisher-Yates shuffle from http://santos.nfshost.com/shuffling.html
function shuffle(t)
  for i = 1, #t - 1 do
    local r = math.random(i, #t)
    t[i], t[r] = t[r], t[i]
  end
end

-- builds a width-by-height grid of trues
function initialize_grid(w, h)
  local a = {}
  for i = 1, h do
    table.insert(a, {})
    for j = 1, w do
      table.insert(a[i], true)
    end
  end
  return a
end

-- average of a and b
function avg(a, b)
  return (a + b) / 2
end


dirs = {
  {x = 0, y = -2}, -- north
  {x = 2, y = 0}, -- east
  {x = -2, y = 0}, -- west
  {x = 0, y = 2}, -- south
}

function make_maze(w, h)
  w = w or 16
  h = h or 8

  local map = initialize_grid(w*2+1, h*2+1)

  function walk(x, y)
    map[y][x] = false

    local d = { 1, 2, 3, 4 }
    shuffle(d)
    for i, dirnum in ipairs(d) do
      local xx = x + dirs[dirnum].x
      local yy = y + dirs[dirnum].y
      if map[yy] and map[yy][xx] then
        map[avg(y, yy)][avg(x, xx)] = false
        walk(xx, yy)
      end
    end
  end

  walk(math.random(1, w)*2, math.random(1, h)*2)

  local s = {}
  for i = 1, h*2+1 do
    for j = 1, w*2+1 do
      if map[i][j] then
        table.insert(s, '#')
      else
        table.insert(s, ' ')
      end
    end
    table.insert(s, '\n')
  end
  return table.concat(s)
end

print(make_maze())

```

```txt
#################################
# #     # #         #         # #
# # ### # # ### ##### # ##### # #
# # # # #     #       # #   #   #
# # # # ########### ### # # #####
#   # #     #     # # #   #     #
# ### ##### # ### # # ####### # #
# #     # # # # # #       # # # #
# # ### # # # # # ######### ### #
# #   #   # #   #   #       #   #
# ### ### # ### ### # ####### # #
# #   # # #     # #   #   #   # #
# # ### # ####### ##### # # ### #
# #   # #       #   #   # # # # #
# ### # ####### # # # ### # # # #
#     #           #   #     #   #
#################################
```




## M2000 Interpreter


### Random Generation

For Next is not the same as basic. In M2000 always a loop perform once. Step converted to absolute value if start<>end. To go down we have to place start>end. If start=end then the value after the loop is equal to start+step and here step used as is (with no conversion to absolute value).

We can use the for loop as in basic using a software switch: see Help Switch from console.
Here we have positive steps so we can translate nicely from Basic without the use of the switch.

Also there is another switch if we want a Dim a(10) to have 11 items, from 0 to 10. But here we set lower and upper index (we may use negative numbers too, when we use number to number for each dimension)
Variables with % in name as last character are like integers, but inside can be any numeric type, so width%=40 is internal a double, width$=40@ is internal a decimal. When we assign a number then this number rounded to integer, where w%=1.5 is 2 and w%=-1.5 is -2. If w%=1 then the statement w%/=2 not changed the w% (internal go to 0.5 so rounded to 1).

We can use integers, say a long, so a statement Long a=1 make a=1 and a/=2 set a to 0.

INT((currentx% + oldx%) / 2) return a double, because has 2 as double so we get (integer+integer)/double or integer/double or double. Int(0.5) return.


```M2000 Interpreter

Module Maze {
      width% = 40
      height% = 20
      \\ we can use DIM maze$(0 to width%,0 to  height%)="#"
      \\ so we can delete the two For loops
      DIM maze$(0 to width%,0 to  height%)
      FOR x% = 0 TO width%
          FOR y% = 0 TO height%
              maze$(x%, y%) = "#"
          NEXT y%
      NEXT x%

      currentx% = INT(RND * (width% - 1))
      currenty% = INT(RND * (height% - 1))

      IF currentx% MOD 2 = 0 THEN currentx%++
      IF currenty% MOD 2 = 0 THEN currenty%++
      maze$(currentx%, currenty%) = " "

      done% = 0
      WHILE done% = 0 {
          FOR i% = 0 TO 99
              oldx% = currentx%
              oldy% = currenty%
              SELECT CASE INT(RND * 4)
                  CASE 0
                      IF currentx% + 2 < width% THEN currentx%+=2
                  CASE 1
                      IF currenty% + 2 < height% THEN currenty%+=2
                  CASE 2
                      IF currentx% - 2 > 0 THEN currentx%-=2
                  CASE 3
                      IF currenty% - 2 > 0 THEN currenty%-=2
              END SELECT
              IF maze$(currentx%, currenty%) = "#"  Then {
                  maze$(currentx%, currenty%) = " "
                  maze$(INT((currentx% + oldx%) / 2), ((currenty% + oldy%) / 2)) = " "
             }
          NEXT i%
          done% = 1
          FOR x% = 1 TO width% - 1 STEP 2
              FOR y% = 1 TO height% - 1 STEP 2
                  IF maze$(x%, y%) = "#" THEN done% = 0
              NEXT y%
          NEXT x%
      }


      FOR y% = 0 TO height%
          FOR x% = 0 TO width%
              PRINT maze$(x%, y%);
          NEXT x%
          PRINT
      NEXT y%
}
Maze

```


===Depth-first search===
We use here the stack of values (it is heap based), which used for calls. Each module get the parent stack. A function has always a fresh stack. Using statement Stack New { } we use a new stack for that block, and at the exit the old stack return as current stack. Identifier [] is the stack as a stack object and work as command and do a swap of a fresh stack with the current one, so Array([]) leave current stack empty and return an array with all elements of stack.
!arraypointer in a module place a copy of items to current stack, so when we call NewForChoose(!entry) we pass to stack by value all elements of entry (has always two).
Subs have same scope with module, so we use local statement for local variables. Module's variables are local by default. A inner module has own scope, and can't see parent modules variables. Modules call other modules with same stack of values. Functions in expressions have own stack of variables. Modules ans subs can return values to stack of values.
A Push 10 write 10 to top of stack. A Read X read the top of stack to X. A Data 10 write to the end of stack, so a stack can be used as FIFO also.
A lambda function also has own scope, so we have to include w and h as closures, which are mutable copies of h and w.



```M2000 Interpreter

Module Maze2 {
      \\ depth-first search
      Profiler
      Form 80,50
      let w=60, h=40
      Double
      \\center proportional text double size
      Report 2, Format$("Maze {0}x{1}",w,h)
      Normal
      Refresh
      Set Fast !
      Dim maze$(1 to w+1, 1 to h+1)="#"
      Include=Lambda w,h (a,b) ->a>=1 and a<=w and b>=1 and b<=h
      Flush ' empty stack
      if random(1,2)=1 then {
            entry=(if(random(1,2)=1->2, w),Random(1, h/2)*2)
      } else {
            entry=(random(1,w/2)*2,If(Random(1,2)=1->2,h))
      }
      maze$(entry#val(0), entry#val(1))=" "
      forchoose=(,)
      Push Entry
      do {
            do {
                  NewForChoose(!entry)
                  status=len(forchoose)
                  if status>0 then {
                        status--
                        forchoose=forchoose#val(random(0,status))
                        if status>0 then Push forchoose
                        OpenDoor(!Entry, !forchoose)
                       Rem : ShowMaze()
                  } else exit
                  entry=forchoose
            } Always
            if empty then exit
            Read entry
      } Always
      ShowMaze()
      Print timecount/1000
      Sub NewForChoose(x,y)
            Local x1=x-2, x2=x+2, y1=y-2, y2=y+2, arr=(,)
            Stack New {
                  if include(x1,y) then if Maze$(x1,y)<>" " Then push (x1, y)
                  if include(x2,y) then if Maze$(x2,y)<>" " Then push (x2, y)
                  if include(x,y1) then if Maze$(x,y1)<>" " Then push (x, y1)
                  if include(x,y2) then if Maze$(x,y2)<>" " Then push (x, y2)
                  forchoose= Array([])
            }
      End Sub
      Sub OpenDoor(x1,y1, x2,y2)
            Local i
            if x1=x2 then {
                  y1+=y2<=>y1
                  for i=y1 to y2 {maze$(x1, i)=" " }
            }  Else {
                  x1+=x2<=>x1
                  for i=x1 to x2 {maze$(i, y1)=" "}
            }
      End Sub
      Sub ShowMaze()
            Refresh 5000
            cls ,4  ' split screen - preserve lines form 0 to 3
            Local i, j
            For j=1 to h+1 { Print @(10) : for i=1 to w+1 {Print maze$(i,j);}:Print}
            Print
            Refresh 100
      End Sub
}
Maze2

```



## Mathematica


```mathematica
MazeGraphics[m_, n_] :=
 Block[{$RecursionLimit = Infinity,
   unvisited = Tuples[Range /@ {m, n}], maze},
  maze = Graphics[{Line[{{#, # - {0, 1}}, {#, # - {1, 0}}}] & /@
      unvisited,
     Line[{{0, n}, {0, 0}, {m, 0}}]}]; {unvisited =
      DeleteCases[unvisited, #];
     Do[If[MemberQ[unvisited, neighbor],
       maze = DeleteCases[
         maze, {#,
           neighbor - {1, 1}} | {neighbor, # - {1, 1}}, {5}]; #0@
        neighbor], {neighbor,
       RandomSample@{# + {0, 1}, # - {0, 1}, # + {1, 0}, # - {1,
           0}}}]} &@RandomChoice@unvisited; maze];
maze = MazeGraphics[21, 13]
```

[[File:MathematicaMazeGraphics.png]]

### Graph

Here I generate a maze as a graph. Vertices of the graph are cells and edges of the graph are removed walls. This version is mush faster and is convenient to solve.

```mathematica
MazeGraph[m_, n_] :=
 Block[{$RecursionLimit = Infinity, grid = GridGraph[{m, n}],
   unvisitedQ}, unvisitedQ[_] := True;
  Graph[Range[m n], Reap[{unvisitedQ[#] = False;
        Do[
         If[unvisitedQ[neighbor],
          Sow[# <-> neighbor]; #0@neighbor], {neighbor,
          RandomSample@AdjacencyList[grid, #]}]} &@
      RandomChoice@VertexList@grid][[2, 1]],
   GraphLayout -> {"GridEmbedding", "Dimension" -> {m, n}}]];
maze = MazeGraph[13, 21]
```

[[File:MathematicaMazeGraph.png]]

=={{header|MATLAB}} / {{header|Octave}}==

```Matlab
function M = makeMaze(n)
    showProgress = false;

    colormap([1,1,1;1,1,1;0,0,0]);
    set(gcf,'color','w');

    NoWALL      = 0;
    WALL        = 2;
    NotVISITED  = -1;
    VISITED     = -2;

    m = 2*n+3;
    M = NotVISITED(ones(m));
    offsets = [-1, m, 1, -m];

    M([1 2:2:end end],:) = WALL;
    M(:,[1 2:2:end end]) = WALL;

    currentCell = sub2ind(size(M),3,3);
    M(currentCell) = VISITED;

    S = currentCell;

    while (~isempty(S))
        moves = currentCell + 2*offsets;
        unvistedNeigbors = find(M(moves)==NotVISITED);

        if (~isempty(unvistedNeigbors))
            next = unvistedNeigbors(randi(length(unvistedNeigbors),1));
            M(currentCell + offsets(next)) = NoWALL;

            newCell = currentCell + 2*offsets(next);
            if (any(M(newCell+2*offsets)==NotVISITED))
                S = [S newCell];
            end

            currentCell = newCell;
            M(currentCell) = VISITED;
        else
            currentCell = S(1);
            S = S(2:end);
        end

        if (showProgress)
            image(M-VISITED);
            axis equal off;
            drawnow;
            pause(.01);
        end
    end

    image(M-VISITED);
    axis equal off;
```



## Nim

```nim
import math, sequtils, strutils
randomize()

template newSeqWith(len: int, init: expr): expr =
  var result {.gensym.} = newSeq[type(init)](len)
  for i in 0 .. <len:
    result[i] = init
  result

iterator randomCover[T](xs: openarray[T]): T =
  var js = toSeq 0..xs.high
  for i in countdown(js.high, 0):
    let j = random(i + 1)
    swap(js[i], js[j])
  for j in js:
    yield xs[j]

const
  w = 14
  h = 10

var
  vis = newSeqWith(h, newSeq[bool](w))
  hor = newSeqWith(h+1, newSeqWith(w, "+---"))
  ver = newSeqWith(h, newSeqWith(w, "|   ") & "|")

proc walk(x, y) =
  vis[y][x] = true
  for p in [[x-1,y], [x,y+1], [x+1,y], [x,y-1]].randomCover:
    if p[0] notin 0 .. <w or p[1] notin 0 .. <h or vis[p[1]][p[0]]: continue
    if p[0] == x: hor[max(y, p[1])][x] = "+   "
    if p[1] == y: ver[y][max(x, p[0])] = "    "
    walk p[0], p[1]

walk random(0..w), random(0..h)
for a,b in zip(hor, ver & @[""]).items:
  echo join(a & "+\n" & b)
```

Example output:

```txt
+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
|           |       |           |       |       |       |
+   +   +   +   +   +   +---+   +   +   +   +   +   +   +
|   |   |   |   |       |       |   |       |       |   |
+   +   +   +---+   +---+   +---+   +---+---+---+---+   +
|   |   |       |   |   |                       |       |
+---+   +---+   +   +   +---+---+---+---+---+   +   +---+
|       |   |   |           |   |       |       |       |
+---+---+   +   +---+---+   +   +   +   +---+---+   +   +
|       |       |       |   |   |   |           |   |   |
+   +---+   +---+   +---+   +   +   +---+---+   +---+   +
|           |   |       |   |   |       |   |       |   |
+   +---+---+   +   +   +   +   +---+   +   +---+   +   +
|   |           |   |   |           |   |       |   |   |
+   +   +---+   +   +   +   +---+---+   +---+   +   +   +
|   |       |       |       |           |       |   |   |
+   +---+---+---+---+---+---+   +---+---+   +---+   +   +
|   |       |       |           |       |           |   |
+   +   +   +   +   +   +---+---+   +   +   +---+---+   +
|       |       |       |           |                   |
+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
```



## OCaml


```ocaml
let seen = Hashtbl.create 7
let mark t = Hashtbl.add seen t true
let marked t = Hashtbl.mem seen t

let walls = Hashtbl.create 7
let ord a b = if a <= b then (a,b) else (b,a)
let join a b = Hashtbl.add walls (ord a b) true
let joined a b = Hashtbl.mem walls (ord a b)

let () =
  let nx = int_of_string Sys.argv.(1) in
  let ny = int_of_string Sys.argv.(2) in

  let shuffle lst =
     let nl = List.map (fun c -> (Random.bits (), c)) lst in
     List.map snd (List.sort compare nl) in

  let get_neighbours (x,y) =
    let lim n k = (0 <= k) && (k < n) in
    let bounds (x,y) = lim nx x && lim ny y in
    List.filter bounds [(x-1,y);(x+1,y);(x,y-1);(x,y+1)] in

  let rec visit cell =
    mark cell;
    let check k =
      if not (marked k) then (join cell k; visit k) in
    List.iter check (shuffle (get_neighbours cell)) in

  let print_maze () =
    begin
    for i = 1 to nx do print_string "+---";done; print_endline "+";
    let line n j k l s t u =
      for i = 0 to n do print_string (if joined (i,j) (i+k,j+l) then s else t) done;
      print_endline u in
    for j = 0 to ny-2 do
      print_string "|   ";
      line (nx-2) j 1 0 "    " "|   " "|";
      line (nx-1) j 0 1 "+   " "+---" "+";
    done;
    print_string "|   ";
    line (nx-2) (ny-1) 1 0 "    " "|   " "|";
    for i = 1 to nx do print_string "+---";done; print_endline "+";
   end in

  Random.self_init();
  visit (Random.int nx, Random.int ny);
  print_maze ();
```

Output from 'ocaml gen_maze.ml 10 10':
```txt
+---+---+---+---+---+---+---+---+---+---+
|           |                   |       |
+   +---+   +---+   +   +---+   +---+   +
|       |       |   |       |           |
+   +   +---+   +---+---+   +---+---+   +
|   |   |       |           |           |
+   +   +---+   +   +---+   +---+---+---+
|   |       |   |   |       |           |
+---+---+   +   +   +---+---+   +---+   +
|           |   |               |       |
+   +---+---+   +---+---+---+---+   +---+
|   |               |           |       |
+   +---+---+---+   +   +---+   +---+   +
|               |       |   |       |   |
+---+---+---+   +---+---+   +---+   +   +
|           |   |       |       |       |
+   +---+   +---+   +   +   +   +---+   +
|       |       |   |       |       |   |
+---+   +   +---+   +---+---+---+   +   +
|       |                       |       |
+---+---+---+---+---+---+---+---+---+---+

```



## Ol


```scheme

; maze generation
(import (otus random!))
(define WIDTH 30)
(define HEIGHT 8)

(define maze
   (map (lambda (?)
         (repeat #b01111 WIDTH)) ; 0 - unvisited, 1111 - all walls exists
      (iota HEIGHT)))
(define (at x y)
   (list-ref (list-ref maze y) x))

(define (unvisited? x y)
   (if (and (< -1 x WIDTH) (< -1 y HEIGHT))
      (zero? (band (at x y) #b10000))))
(define neighbors '((-1 . 0) (0 . -1) (+1 . 0) (0 . +1)))
(define walls     '( #b10111  #b11011  #b11101  #b11110))
(define antiwalls '( #b11101  #b11110  #b10111  #b11011))

(let loop ((x (rand! WIDTH)) (y (rand! HEIGHT)))
   (list-set! (list-ref maze y) x (bor (at x y) #b10000))
   (let try ()
      (if (or
            (unvisited? (- x 1) y) ; left
            (unvisited? x (- y 1)) ; top
            (unvisited? (+ x 1) y) ; right
            (unvisited? x (+ y 1))) ; bottom
         (let*((p (rand! 4))
               (neighbor (list-ref neighbors p)))
            (let ((nx (+ x (car neighbor)))
                  (ny (+ y (cdr neighbor))))
            (if (unvisited? nx ny)
               (let ((ncell (at nx ny)))
                  (list-set! (list-ref maze y) x (band (at x y) (list-ref walls p)))
                  (list-set! (list-ref maze ny) nx (band ncell (list-ref antiwalls p)))
                  (loop nx ny)))
            (try))))))

```


```scheme

; maze printing:
(display "+")
(for-each (lambda (?) (display "--+")) (iota WIDTH))
(print)
(for-each (lambda (l)
            ; left wall (always)
            (display "|")
            ; draw right wall
            (for-each (lambda (x)
                        (display "  ")
                        (display (if (zero? (band x #b10)) " " "|")))
               l)
            (print)
            (display "+")
            ; draw bottom wall
            (for-each (lambda (x)
                        (display (if (zero? (band x #b01)) "  " "--"))
                        (display "+"))
               l)
            (print))
   maze)
(print)

```

```txt
+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
|        |        |     |           |                 |           |     |        |        |
+  +  +  +  +--+  +--+  +  +--+  +--+  +--+--+  +--+  +--+  +--+  +  +--+  +--+  +  +--+  +
|  |  |  |     |     |  |  |     |           |  |  |     |     |     |     |     |  |     |
+--+  +  +--+  +--+  +  +  +  +  +  +--+--+--+  +  +--+  +--+  +  +--+  +--+  +--+  +  +  +
|     |        |  |  |     |  |  |  |           |     |     |  |  |     |  |     |  |  |  |
+  +--+--+--+--+  +  +--+--+  +--+  +  +--+--+--+  +  +--+  +  +--+  +--+  +--+  +  +  +  +
|     |        |              |     |  |           |     |  |  |     |        |     |  |  |
+--+  +  +--+  +--+--+--+--+  +  +--+  +--+--+  +  +--+--+  +  +  +--+--+  +  +--+--+  +  +
|     |  |  |  |     |  |     |  |  |        |  |     |     |  |  |     |  |  |        |  |
+  +--+  +  +  +  +  +  +  +--+  +  +--+--+  +  +--+  +  +--+  +  +  +  +  +--+  +--+--+--+
|     |  |  |     |  |     |     |     |     |     |           |     |  |  |  |  |        |
+--+  +  +  +--+--+  +--+--+  +--+  +--+  +--+--+  +--+--+--+--+--+--+  +  +  +  +  +--+  +
|     |           |  |     |  |        |     |           |           |  |  |  |     |     |
+  +--+--+--+--+  +  +  +  +  +--+--+  +--+  +--+--+--+--+  +--+--+  +  +  +  +--+--+  +  +
|                 |     |                 |                       |        |           |  |
+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
```



## Perl


```perl
use List::Util 'max';

my ($w, $h) = @ARGV;
$w ||= 26;
$h ||= 127;
my $avail = $w * $h;

# cell is padded by sentinel col and row, so I don't check array bounds
my @cell = (map([(('1') x $w), 0], 1 .. $h), [('') x ($w + 1)]);
my @ver = map([("|  ") x $w], 1 .. $h);
my @hor = map([("+--") x $w], 0 .. $h);

sub walk {
	my ($x, $y) = @_;
	$cell[$y][$x] = '';
	$avail-- or return;	# no more bottles, er, cells

	my @d = ([-1, 0], [0, 1], [1, 0], [0, -1]);
	while (@d) {
		my $i = splice @d, int(rand @d), 1;
		my ($x1, $y1) = ($x + $i->[0], $y + $i->[1]);

		$cell[$y1][$x1] or next;

		if ($x == $x1) { $hor[ max($y1, $y) ][$x] = '+  ' }
		if ($y == $y1) { $ver[$y][ max($x1, $x) ] = '   ' }
		walk($x1, $y1);
	}
}

walk(int rand $w, int rand $h);	# generate

for (0 .. $h) {			# display
	print @{$hor[$_]}, "+\n";
	print @{$ver[$_]}, "|\n" if $_ < $h;
}
```

Run as <code>maze.pl [width] [height]</code> or use default dimensions.
```txt
+--+--+--+--+
|           |
+--+--+--+--+
```



## Perl 6

Supply a width and height and optionally the x,y grid coords for the starting cell. If no starting cell is supplied, a random one will be selected automatically. 0,0 is the top left corner.

```perl6
constant mapping = :OPEN(' '),
		      :N< ╵ >,
		      :E< ╶ >,
		     :NE< └ >,
		      :S< ╷ >,
		     :NS< │ >,
		     :ES< ┌ >,
		    :NES< ├ >,
		      :W< ╴ >,
		     :NW< ┘ >,
		     :EW< ─ >,
		    :NEW< ┴ >,
		     :SW< ┐ >,
		    :NSW< ┤ >,
		    :ESW< ┬ >,
		   :NESW< ┼ >,
		   :TODO< x >,
	          :TRIED< · >;

enum Sym (mapping.map: *.key);
my @ch = mapping.map: *.value;

enum Direction <DeadEnd Up Right Down Left>;

sub gen_maze ( $X,
               $Y,
               $start_x = (^$X).pick * 2 + 1,
               $start_y = (^$Y).pick * 2 + 1 )
{
    my @maze;
    push @maze, $[ flat ES, -N, (ESW, EW) xx $X - 1, SW ];
    push @maze, $[ flat (NS, TODO) xx $X, NS ];
    for 1 ..^ $Y {
	push @maze, $[ flat NES, EW, (NESW, EW) xx $X - 1, NSW ];
	push @maze, $[ flat (NS, TODO) xx $X, NS ];
    }
    push @maze, $[ flat NE, (EW, NEW) xx $X - 1, -NS, NW ];
    @maze[$start_y][$start_x] = OPEN;

    my @stack;
    my $current = [$start_x, $start_y];
    loop {
        if my $dir = pick_direction( $current ) {
            @stack.push: $current;
            $current = move( $dir, $current );
        }
        else {
            last unless @stack;
            $current = @stack.pop;
        }
    }
    return @maze;

    sub pick_direction([$x,$y]) {
	my @neighbors =
	    (Up    if @maze[$y - 2][$x]),
	    (Down  if @maze[$y + 2][$x]),
	    (Left  if @maze[$y][$x - 2]),
	    (Right if @maze[$y][$x + 2]);
	@neighbors.pick or DeadEnd;
    }

    sub move ($dir, @cur) {
	my ($x,$y) = @cur;
	given $dir {
	    when Up    { @maze[--$y][$x] = OPEN; @maze[$y][$x-1] -= E; @maze[$y--][$x+1] -= W; }
	    when Down  { @maze[++$y][$x] = OPEN; @maze[$y][$x-1] -= E; @maze[$y++][$x+1] -= W; }
	    when Left  { @maze[$y][--$x] = OPEN; @maze[$y-1][$x] -= S; @maze[$y+1][$x--] -= N; }
	    when Right { @maze[$y][++$x] = OPEN; @maze[$y-1][$x] -= S; @maze[$y+1][$x++] -= N; }
	}
	@maze[$y][$x] = 0;
	[$x,$y];
    }
}

sub display (@maze) {
    for @maze -> @y {
	for @y.rotor(2) -> ($w, $c) {
	    print @ch[abs $w];
	    if $c >= 0 { print @ch[$c] x 3 }
	    else { print ' ', @ch[abs $c], ' ' }
	}
	say @ch[@y[*-1]];
    }
}

display gen_maze( 29, 19 );
```

<small>
```txt
┌ ╵ ────────────────────────────┬───────────────────────────────────────────┬───────────┬───────────────────────────┐
│                               │                                           │           │                           │
│   ╶───────────┬───────────┐   │   ┌───────────────────────╴   ┌───────┐   ├───╴   ╷   │   ┌───────────┬───┬───╴   │
│               │           │   │   │                           │       │   │       │   │   │           │   │       │
│   ┌───────┐   ╵   ┌───┐   ├───┘   │   ┌───────────┬───────────┤   ╶───┤   │   ╶───┴───┤   │   ┌───┐   │   │   ╶───┤
│   │       │       │   │   │       │   │           │           │       │   │           │   │   │   │   │   │       │
│   └───╴   └───────┤   │   ╵   ┌───┘   │   ╷   ╶───┤   ┌───┐   │   ╷   │   ├───────╴   │   ╵   │   │   │   └───┐   │
│                   │   │       │       │   │       │   │   │   │   │   │   │           │       │   │   │       │   │
├───────┬───────┐   │   ├───────┤   ╶───┤   └───┐   ╵   │   │   ╵   │   ╵   │   ┌───┐   └───┬───┘   │   │   ╷   │   │
│       │       │   │   │       │       │       │       │   │       │       │   │   │       │       │   │   │   │   │
│   ╶───┤   ╷   ╵   │   ╵   ╷   └───┐   ├───┐   ├───────┤   └───────┴───────┘   │   └───┐   └───╴   │   ╵   ├───┘   │
│       │   │       │       │       │   │   │   │       │                       │       │           │       │       │
├───╴   │   ├───────┴───┐   ├───╴   │   │   │   ╵   ╷   └───┐   ╶───┬───────┬───┘   ┌───┴───────╴   │   ┌───┘   ╷   │
│       │   │           │   │       │   │   │       │       │       │       │       │               │   │       │   │
│   ╷   │   │   ╶───┐   └───┤   ╷   │   │   └───────┴───┐   └───╴   │   ╷   ╵   ╷   ╵   ╶───────┬───┤   │   ┌───┴───┤
│   │   │   │       │       │   │   │   │               │           │   │       │               │   │   │   │       │
│   │   │   │   ╷   ├───╴   ╵   │   │   │   ┌───────┐   └───────────┤   └───┬───┴───────────┐   ╵   │   │   ╵   ╷   │
│   │   │   │   │   │           │   │   │   │       │               │       │               │       │   │       │   │
│   │   │   ├───┘   │   ┌───────┴───┘   │   ╵   ┌───┴───────╴   ╷   ├───┐   │   ╶───────┐   └───┐   │   └───┬───┘   │
│   │   │   │       │   │               │       │               │   │   │   │           │       │   │       │       │
│   └───┘   │   ┌───┘   │   ┌───┬───────┼───╴   │   ╶───┬───────┤   ╵   │   └───┬───╴   ├───┐   │   └───┐   │   ╷   │
│           │   │       │   │   │       │       │       │       │       │       │       │   │   │       │   │   │   │
│   ┌───────┘   ├───────┤   │   ╵   ╷   │   ┌───┴───┐   │   ╶───┘   ┌───┴───╴   │   ┌───┘   │   │   ┌───┘   │   │   │
│   │           │       │   │       │   │   │       │   │           │           │   │       │   │   │       │   │   │
│   └───╴   ╷   │   ╷   │   └───┐   │   ╵   │   ╷   ╵   ├───────┐   │   ╶───────┤   └───┐   │   └───┤   ┌───┘   │   │
│           │   │   │   │       │   │       │   │       │       │   │           │       │   │       │   │       │   │
├───────────┤   │   │   └───╴   │   └───────┤   └───────┘   ╷   └───┴───┬───╴   ├───╴   │   └───┐   │   │   ╶───┴───┤
│           │   │   │           │           │               │           │       │       │       │   │   │           │
│   ┌───╴   │   │   ├───╴   ┌───┴───────┬───┴───┐   ┌───────┴───────┐   ╵   ┌───┤   ╶───┤   ╷   │   ╵   └───┬───╴   │
│   │       │   │   │       │           │       │   │               │       │   │       │   │   │           │       │
│   │   ╶───┘   │   │   ╶───┤   ╶───┐   ╵   ╷   │   └───╴   ┌───╴   └───────┘   ├───╴   │   ├───┴───────┬───┘   ╷   │
│   │           │   │       │       │       │   │           │                   │       │   │           │       │   │
│   ├───────┬───┘   ├───────┴───┐   ├───────┤   └───────────┤   ┌───────────┐   │   ┌───┘   │   ╶───┐   ╵   ┌───┘   │
│   │       │       │           │   │       │               │   │           │   │   │       │       │       │       │
│   └───╴   │   ╷   │   ╶───┐   ╵   │   ╷   └───────────┐   │   │   ┌───┐   └───┘   │   ╶───┴───┐   └───────┴───────┤
│           │   │   │       │       │   │               │   │   │   │   │           │           │                   │
├───────╴   │   └───┴───╴   └───────┴───┴───────────╴   │   └───┘   ╵   └───────────┴───╴   ╷   └───────────────┐   │
│           │                                           │                                   │                   │   │
└───────────┴───────────────────────────────────────────┴───────────────────────────────────┴───────────────────┴ │ ┘
```
</small>


## Phix

Adapted a couple of techniques from the excellent D submission

(however this holds the grid as an array of complete lines)

```Phix
--
-- grid is eg for w=3,h=2: {"+---+---+---+",    -- ("wall")
--                          "| ? | ? | ? |",    -- ("cell")
--                          "+---+---+---+",    -- ("wall")
--                          "| ? | ? | ? |",    -- ("cell")
--                          "+---+---+---+",    -- ("wall")
--                          ""} -- (for a trailing \n)
--
-- note those ?(x,y) are at [y*2][x*4-1], and we navigate
-- using y=2..2*h (+/-2), x=3..w*4-1 (+/-4) directly.
--
constant w = 11, h = 8

sequence wall = join(repeat("+",w+1),"---")&"\n",
         cell = join(repeat("|",w+1)," ? ")&"\n",
         grid = split(join(repeat(wall,h+1),cell),'\n')

procedure amaze(integer x, integer y)
    grid[y][x] = ' '                        -- mark cell visited
    sequence p = shuffle({{x-4,y},{x,y+2},{x+4,y},{x,y-2}})
    for i=1 to length(p) do
        integer {nx,ny} = p[i]
        if nx>1 and nx<w*4 and ny>1 and ny<=2*h and grid[ny][nx]='?' then
            integer mx = (x+nx)/2
            grid[(y+ny)/2][mx-1..mx+1] = ' ' -- knock down wall
            amaze(nx,ny)
        end if
    end for
end procedure

function heads()
    return rand(2)=1 -- 50:50 true(1)/false(0)
end function

integer {x,y} = {(rand(w)*4)-1,rand(h)*2}
amaze(x,y)
-- mark start pos
grid[y][x] = '*'
-- add a random door (heads=rhs/lhs, tails=top/btm)
if heads() then
    grid[rand(h)*2][heads()*w*4-1] = ' '
else
    x = rand(w)*4-1
    grid[heads()*h*2+1][x-1..x+1] = ' '
end if
puts(1,join(grid,'\n'))
```

```txt

+---+---+---+---+---+---+---+---+---+---+---+
|   |                                   |   |
+   +   +---+---+---+---+---+---+   +   +   +
|   |       |   | *             |   |       |
+   +---+   +   +---+---+---+   +   +---+---+
|   |       |               |   |           |
+   +   +---+   +---+   +---+   +---+---+   +
|   |       |   |   |       |   |           |
+   +---+   +   +   +---+   +   +   +---+---+
|   |       |       |       |       |       |
+   +   +---+---+---+   +---+---+---+---+   +
|   |               |           |           |
+   +---+---+---+   +---+   +   +   +---+   +
|       |       |           |       |       |
+   +---+   +   +---+---+---+---+---+   +---+
            |                               |
+---+---+---+---+---+---+---+---+---+---+---+

```




## PHP

Code inspired by the D and Python solutions (with the implementation of backtracking, or sometimes it wouldn't work). Could have been done procedurally or fully OO (with cells as class too). A debug flag has been provided to allow following the inner workings. Works on PHP > 5.6.

```php
<?php
class Maze
{
    protected $width;
    protected $height;
    protected $grid;
    protected $path;
    protected $horWalls;
    protected $vertWalls;
    protected $dirs;
    protected $isDebug;

    public function __construct($x, $y, $debug = false)
    {
        $this->width = $x;
        $this->height = $y;
        $this->path = [];
        $this->dirs = [ [0, -1], [0, 1], [-1, 0], [1, 0]]; // array of coordinates of N,S,W,E
        $this->horWalls = []; // list of removed horizontal walls (---+)
        $this->vertWalls = [];// list of removed vertical walls (|)
        $this->isDebug = $debug; // debug flag

        // generate the maze:
        $this->generate();
    }

    protected function generate()
    {
        $this->initMaze(); // init the stack and an unvisited grid
        // start from a random cell and then proceed recursively
        $this->walk(mt_rand(0, $this->width-1), mt_rand(0, $this->height-1));
    }

    /**
    * Actually prints the Maze, on stdOut. Put in a separate method to allow extensibility
    * For simplicity sake doors are positioned on the north wall and east wall
    */
    public function printOut()
    {
        $this->log("Horizontal walls: %s", json_encode($this->horWalls));
        $this->log("Vertical walls: %s", json_encode($this->vertWalls));

        $northDoor = mt_rand(0,$this->width-1);
        $eastDoor = mt_rand(0, $this->height-1);

        $str = '+';
        for ($i=0;$i<$this->width;$i++) {
            $str .= ($northDoor == $i) ? '   +' : '---+';
        }
        $str .= PHP_EOL;
        for ($i=0; $i<$this->height; $i++) {

            for ($j=0; $j<$this->width; $j++) {
                $str .= (!empty($this->vertWalls[$j][$i]) ? $this->vertWalls[$j][$i] : '|   ');
            }
            $str .= ($i == $eastDoor ? '  ' : '|').PHP_EOL.'+';
            for ($j=0; $j<$this->width; $j++) {
                $str .= (!empty($this->horWalls[$j][$i]) ? $this->horWalls[$j][$i] : '---+');
            }
            $str .= PHP_EOL;
        }
        echo $str;
    }

    /**
    * Logs to stdOut if debug flag is enabled
    */
    protected function log(...$params)
    {
        if ($this->isDebug) {
            echo vsprintf(array_shift($params), $params).PHP_EOL;
        }
    }

    private function walk($x, $y)
    {
        $this->log('Entering cell %d,%d', $x, $y);
        // mark current cell as visited
        $this->grid[$x][$y] = true;
        // add cell to path
        $this->path[] = [$x, $y];
        // get list of all neighbors
        $neighbors = $this->getNeighbors($x, $y);
        $this->log("Valid neighbors: %s", json_encode($neighbors));

        if(empty($neighbors)) {
            // Dead end, we need now to backtrack, if there's still any cell left to be visited
            $this->log("Start backtracking along path: %s", json_encode($this->path));
            array_pop($this->path);
            if (!empty($this->path)) {
                $next = array_pop($this->path);
                return $this->walk($next[0], $next[1]);
            }
        } else {
            // randomize neighbors, as per request
            shuffle($neighbors);

            foreach ($neighbors as $n) {
                $nextX = $n[0];
                $nextY = $n[1];
                if ($nextX == $x) {
                    $wallY = max($nextY, $y);
                    $this->log("New cell is on the same column (%d,%d), removing %d, (%d-1) horizontal wall", $nextX, $nextY, $x, $wallY);
                    $this->horWalls[$x][min($nextY, $y)] = "   +";
                }
                if ($nextY == $y) {
                    $wallX = max($nextX, $x);
                    $this->log("New cell is on the same row (%d,%d), removing %d,%d vertical wall", $nextX, $nextY, $wallX, $y);
                    $this->vertWalls[$wallX][$y] = "    ";
                }
                return $this->walk($nextX, $nextY);
            }
        }
    }

    /**
    * Initialize an empty grid of $width * $height dimensions
    */
    private function initMaze()
    {
        for ($i=0;$i<$this->width;$i++) {
            for ($j = 0;$j<$this->height;$j++) {
                $this->grid[$i][$j] = false;
            }
        }
    }

    /**
    * @param int $x
    * @param int $y
    * @return array
    */
    private function getNeighbors($x, $y)
    {
        $neighbors = [];
        foreach ($this->dirs as $dir) {
            $nextX = $dir[0] + $x;
            $nextY = $dir[1] + $y;
            if (($nextX >= 0 && $nextX < $this->width && $nextY >= 0 && $nextY < $this->height) && !$this->grid[$nextX][$nextY]) {
                $neighbors[] = [$nextX, $nextY];
            }
        }
        return $neighbors;
    }
}

$maze = new Maze(10,10);
$maze->printOut();
```

```txt

+---+   +---+---+---+---+---+---+---+---+
|                                       |
+---+---+---+---+---+---+---+   +---+   +
|               |   |           |       |
+   +---+---+   +   +   +---+---+   +---+
|   |       |       |   |       |   |   |
+   +   +   +---+---+   +---+   +   +   +
|   |   |       |       |       |       |
+   +---+   +   +---+   +   +---+---+   +
|           |   |       |   |       |   |
+---+---+---+   +   +---+   +   +   +   +
|           |       |           |       |
+   +---+---+---+---+   +---+---+---+---+
|       |               |
+---+   +   +---+---+---+---+---+   +   +
|       |   |           |           |   |
+   +---+   +   +   +   +   +---+---+   +
|   |       |   |   |   |   |   |       |
+   +   +---+---+   +   +   +   +   +---+
|                   |       |           |
+---+---+---+---+---+---+---+---+---+---+

```



## PicoLisp

This solution uses 'grid' from "lib/simul.l" to generate the two-dimensional structure.

```PicoLisp
(load "@lib/simul.l")

(de maze (DX DY)
   (let Maze (grid DX DY)
      (let Fld (get Maze (rand 1 DX) (rand 1 DY))
         (recur (Fld)
            (for Dir (shuffle '((west . east) (east . west) (south . north) (north . south)))
               (with ((car Dir) Fld)
                  (unless (or (: west) (: east) (: south) (: north))
                     (put Fld (car Dir) This)
                     (put This (cdr Dir) Fld)
                     (recurse This) ) ) ) ) )
      (for (X . Col) Maze
         (for (Y . This) Col
            (set This
               (cons
                  (cons
                     (: west)
                     (or
                        (: east)
                        (and (= Y 1) (= X DX)) ) )
                  (cons
                     (: south)
                     (or
                        (: north)
                        (and (= X 1) (= Y DY)) ) ) ) ) ) )
      Maze ) )

(de display (Maze)
   (disp Maze 0 '((This) "   ")) )
```

```txt
: (display (maze 11 8))
   +   +---+---+---+---+---+---+---+---+---+---+
 8 |           |       |                       |
   +   +   +   +   +   +   +---+   +---+---+   +
 7 |   |   |       |   |   |       |       |   |
   +---+   +---+---+   +   +   +---+   +   +   +
 6 |   |       |       |   |           |   |   |
   +   +---+   +---+   +---+---+---+   +   +---+
 5 |       |       |               |   |       |
   +---+   +---+   +---+---+---+   +---+---+   +
 4 |   |       |       |       |   |           |
   +   +---+   +---+   +---+   +   +   +---+   +
 3 |       |       |   |       |   |       |   |
   +   +---+---+   +   +   +   +   +---+   +   +
 2 |       |       |   |   |   |   |       |   |
   +   +   +   +---+   +   +---+   +   +---+   +
 1 |   |               |               |
   +---+---+---+---+---+---+---+---+---+---+---+
     a   b   c   d   e   f   g   h   i   j   k
```



## PL/I

```pli
*process source attributes xref or(!);
 mgg: Proc Options(main);
 /* REXX ***************************************************************
 * 04.09.2013 Walter Pachl translated from REXX version 3
 **********************************************************************/
 Dcl (MIN,MOD,RANDOM,REPEAT,SUBSTR) Builtin;
 Dcl SYSIN    STREAM INPUT;
 Dcl print Print;
 Dcl imax Bin Fixed(31) init(10);
 Dcl jmax Bin Fixed(31) init(15);
 Dcl seed Bin Fixed(31) init(4711);
 Get File(sysin) Data(imax,jmax,seed);
 Dcl ii   Bin Fixed(31);
 Dcl jj   Bin Fixed(31);
 Dcl id   Bin Fixed(31);
 Dcl jd   Bin Fixed(31);
 id=2*imax+1;                        /* vertical dimension of a.i.j   */
 jd=2*jmax+1;                        /* horizontal dimension of a.i.j */
 Dcl c Char(2000) Var;
 c=repeat('123456789'!!'abcdefghijklmnopqrstuvwxyz'!!
                       'ABCDEFGHIJKLMNOPQRSTUVWXYZ',20);
 Dcl x Bin Float(53);
 x=random(seed);
 Dcl ps Bin Fixed(31) Init(1);         /* first position             */
 Dcl na Bin Fixed(31) Init(1);         /* number of points used      */
 Dcl si Bin Fixed(31);                 /* loop to compute paths      */
 Begin;
 Dcl a(id,jd) Bin Fixed(15);
 Dcl p(imax,jmax) Char(1);
 Dcl 1 pl(imax*jmax),
      2 ic Bin Fixed(15),
      2 jc Bin Fixed(15);
 Dcl 1 np(imax*jmax),
      2 ic Bin Fixed(15),
      2 jc Bin Fixed(15);
 Dcl 1 pos(imax*jmax),
      2 ic Bin Fixed(15),
      2 jc Bin Fixed(15);
 Dcl npl Bin Fixed(31) Init(0);
 a=1;                                   /* mark all borders present   */
 p='.';                                 /* Initialize all grid points */
 ii=rnd(imax);                          /* find a start position      */
 jj=rnd(jmax);
 Do si=1 To 1000;                       /* Do Forever - see Leave     */
   Call path(ii,jj);              /* compute a path starting at ii/jj */
   If na=imax*jmax Then                 /* all points used            */
     Leave;                             /* we are done                */
   Call select_next(ii,jj);             /* get a new start from a path*/
   End;
 Call show;
 Return;

 path: Procedure(ii,jj);
 /**********************************************************************
 * compute a path starting from point (ii,jj)
 **********************************************************************/
 Dcl ii   Bin Fixed(31);
 Dcl jj   Bin Fixed(31);
 Dcl nb   Bin Fixed(31);
 Dcl ch   Bin Fixed(31);
 Dcl pp   Bin Fixed(31);
   p(ii,jj)='1';
   pos.ic(ps)=ii;
   pos.jc(ps)=jj;
   Do pp=1 to 50;               /* compute a path of maximum length 50*/
     nb=neighbors(ii,jj);               /* number of free neighbors   */
     Select;
       When(nb=1)                       /* just one                   */
         Call advance((1),ii,jj);       /* go for it                  */
       When(nb>0) Do;                   /* more Than 1                */
         ch=rnd(nb);                    /* choose one possibility     */
         Call advance(ch,ii,jj);        /* and go for that            */
         End;
       Otherwise                        /* none available             */
         Leave;
       End;
     End;
   End;

 neighbors: Procedure(i,j) Returns(Bin Fixed(31));
 /**********************************************************************
 * count the number of free neighbors of point (i,j)
 **********************************************************************/
 Dcl i    Bin Fixed(31);
 Dcl j    Bin Fixed(31);
 Dcl in   Bin Fixed(31);
 Dcl jn   Bin Fixed(31);
 Dcl nb   Bin Fixed(31) Init(0);
   in=i-1; If in>0     Then Call check(in,j,nb);
   in=i+1; If in<=imax Then Call check(in,j,nb);
   jn=j-1; If jn>0     Then Call check(i,jn,nb);
   jn=j+1; If jn<=jmax Then Call check(i,jn,nb);
   Return(nb);
 End;

 check: Procedure(i,j,n);
 /**********************************************************************
 * check if point (i,j) is free and note it as possible successor
 **********************************************************************/
 Dcl i    Bin Fixed(31);
 Dcl j    Bin Fixed(31);
 Dcl n    Bin Fixed(31);
   If p(i,j)='.' Then Do;               /* point is free              */
     n+=1;                              /* number of free neighbors   */
     np.ic(n)=i;                        /* note it as possible choice */
     np.jc(n)=j;
     End;
 End;

 advance: Procedure(ch,ii,jj);
 /**********************************************************************
 * move to the next point of the current path
 **********************************************************************/
 Dcl ch   Bin Fixed(31);
 Dcl ii   Bin Fixed(31);
 Dcl jj   Bin Fixed(31);
 Dcl ai   Bin Fixed(31);
 Dcl aj   Bin Fixed(31);
 Dcl pii  Bin Fixed(31) Init((ii));
 Dcl pjj  Bin Fixed(31) Init((jj));
 Dcl z    Bin Fixed(31);
   ii=np.ic(ch);
   jj=np.jc(ch);
   ps+=1;                               /* position number            */
   pos.ic(ps)=ii;                       /* note its coordinates       */
   pos.jc(ps)=jj;
   p(ii,jj)=substr(c,ps,1);             /* mark the point as used     */
   ai=pii+ii;                           /* vertical border position   */
   aj=pjj+jj;                           /* horizontal border position */
   a(ai,aj)=0;                          /* tear the border down       */
   na+=1;                               /* number of used positions   */
   z=npl+1;                             /* add the point to the list  */
   pl.ic(z)=ii;                         /* of follow-up start pos.    */
   pl.jc(z)=jj;
   npl=z;
   End;

 show: Procedure;
 /*********************************************************************
 * Show the resulting maze
 *********************************************************************/
 Dcl i Bin Fixed(31);
 Dcl j Bin Fixed(31);
 Dcl ol Char(300) Var;
   Put File(print) Edit('mgg',imax,jmax,seed)(Skip,a,3(f(4)));
   Put File(print) Skip Data(na);
   Do i=1 To id;
     ol='';
     Do j=1 To jd;
       If mod(i,2)=1 Then Do;            /* odd lines                 */
         If a(i,j)=1 Then Do;            /* border to be drawn        */
           If mod(j,2)=0 Then
             ol=ol!!'---';               /* draw the border           */
           Else
             ol=ol!!'+';
           End;
         Else Do;                        /* border was torn down      */
           If mod(j,2)=0 Then
             ol=ol!!'   ';               /* blanks instead of border  */
           Else
             ol=ol!!'+';
           End;
         End;
       Else Do;                          /* even line                 */
         If a(i,j)=1 Then Do;
           If mod(j,2)=0 Then            /* even column               */
             ol=ol!!'   ';               /* moving space              */
           Else                          /* odd column                */
             ol=ol!!'!';                 /* draw the border           */
           End;
         Else                            /* border was torn down      */
           ol=ol!!' ';                   /* blank instead of border   */
         End;
       End;
     Select;
       When(i=6) substr(ol,11,1)='A';
       When(i=8) substr(ol, 3,1)='B';
       Otherwise;
       End;
     Put File(print) Edit(ol,i)(Skip,a,f(3));
     End;
   End;

 select_next: Procedure(is,js);
 /**********************************************************************
 * look for a point to start the nnext path
 **********************************************************************/
 Dcl is Bin Fixed(31);
 Dcl js Bin Fixed(31);
 Dcl n  Bin Fixed(31);
 Dcl nb Bin Fixed(31);
 Dcl s  Bin Fixed(31);
   Do Until(nb>0);                       /* loop until one is found   */
     n=npl;                              /* number of points recorded */
     s=rnd(n);                           /* pick a random index       */
     is=pl.ic(s);                        /* its coordinates           */
     js=pl.jc(s);
     nb=neighbors(is,js);                /* count free neighbors      */
     If nb=0 Then Do;                    /* if there is none          */
       pl.ic(s)=pl.ic(n);                /* remove this point         */
       pl.jc(s)=pl.jc(n);
       npl-=1;
       End;
     End;
 End;

 rnd: Proc(n) Returns(Bin Fixed(31));
 /*********************************************************************
 * return a pseudo-random integer between 1 and n
 *********************************************************************/
 dcl (r,n) Bin Fixed(31);
 r=min(random()*n+1,n);
 Return(r);
 End;

 End;
 End;
```

Output:

```txt

�mgg   5  15  43
NA=            75;
+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+  1
!       !                                       !       !   !  2
+   +   +   +---+---+---+   +---+---+   +---+   +   +   +   +  3
!   !       !           !           !       !       !       !  4
+   +---+---+   +---+---+---+---+   +---+   +---+---+---+---+  5
!         A !           !           !   !       !           !  6
+---+---+   +---+---+   +   +---+---+   +---+   +   +---+---+  7
! B     !       !       !                   !       !       !  8
+   +   +---+   +   +   +---+---+---+---+---+---+   +   +   +  9
!   !               !                           !       !   ! 10
+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+ 11

```



## Prolog

Works with SWI-Prolog and XPCE.

```Prolog
:- dynamic cell/2.

maze(Lig,Col) :-
	retractall(cell(_,_)),

	new(D, window('Maze')),

	% creation of the grid
	forall(between(0,Lig, I),
	       (XL is  50, YL is I * 30 + 50,
		XR is Col * 30 + 50,
		new(L, line(XL, YL, XR, YL)),
		send(D, display, L))),

	forall(between(0,Col, I),
	       (XT is  50 + I * 30, YT is 50,
		YB is Lig * 30 + 50,
		new(L, line(XT, YT, XT, YB)),
		send(D, display, L))),

	SX is Col * 30 + 100,
	SY is Lig * 30 + 100,
	send(D, size, new(_, size(SX, SY))),

	% choosing a first cell
	L0 is random(Lig),
	C0 is random(Col),
	assert(cell(L0, C0)),
	\+search(D, Lig, Col, L0, C0),
	send(D, open).

search(D, Lig, Col, L, C) :-
	Dir is random(4),
	nextcell(Dir, Lig, Col, L, C, L1, C1),
	assert(cell(L1,C1)),
	assert(cur(L1,C1)),
	erase_line(D, L, C, L1, C1),
	search(D, Lig, Col, L1, C1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
erase_line(D, L, C, L, C1) :-
	(   C < C1 -> C2 = C1; C2 = C),
	XT is C2  * 30 + 50,
	YT is L * 30 + 51, YR is (L+1) * 30 + 50,
	new(Line, line(XT, YT, XT, YR)),
	send(Line, colour, white),
	send(D, display, Line).

erase_line(D, L, C, L1, C) :-
	XT is  51 + C * 30, XR is 50 + (C + 1) * 30,
	(   L < L1 -> L2 is L1; L2 is L),
	YT is L2 * 30 + 50,
	new(Line, line(XT, YT, XR, YT)),
	send(Line, colour, white),
	send(D, display, Line).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
nextcell(Dir, Lig, Col, L, C, L1, C1) :-
	next(Dir, Lig, Col, L, C, L1, C1);
	(   Dir1 is (Dir+3) mod 4,
	    next(Dir1, Lig, Col, L, C, L1, C1));
	(   Dir2 is (Dir+1) mod 4,
	    next(Dir2, Lig, Col, L, C, L1, C1));
	(   Dir3 is (Dir+2) mod 4,
	    next(Dir3, Lig, Col, L, C, L1, C1)).

% 0 => northward
next(0, _Lig, _Col, L, C, L1, C) :-
	L > 0,
	L1 is L - 1,
	\+cell(L1, C).

% 1 => rightward
next(1, _Lig, Col, L, C, L, C1) :-
	C < Col - 1,
	C1 is C + 1,
	\+cell(L, C1).

% 2 => southward
next(2, Lig, _Col, L, C, L1, C) :-
	L < Lig - 1,
	L1 is L + 1,
	\+cell(L1, C).

% 3 => leftward
next(2, _Lig, _Col, L, C, L, C1) :-
	C > 0,
	C1 is C - 1,
	\+cell(L, C1).


```

[[File:Prolog-Maze.jpg]]


## PureBasic


```PureBasic
Enumeration
  ;indexes for types of offsets from maze coordinates (x,y)
  #visited ;used to index visited(x,y) in a given direction from current maze cell
  #maze    ;used to index maze() in a given direction from current maze cell
  #wall    ;used to index walls in maze() in a given direction from current maze cell
  #numOffsets = #wall
  ;direction indexes
  #dir_ID = 0 ;identity value, produces no changes
  #firstDir
  #dir_N = #firstDir
  #dir_E
  #dir_S
  #dir_W
  #numDirs = #dir_W
EndEnumeration

DataSection
  ;maze(x,y) offsets for visited, maze, & walls for each direction
  Data.i 1, 1,  0,  0, 0, 0 ;ID
  Data.i 1, 0,  0, -1, 0, 0 ;N
  Data.i 2, 1,  1,  0, 1, 0 ;E
  Data.i 1, 2,  0,  1, 0, 1 ;S
  Data.i 0, 1, -1,  0, 0, 0 ;W
  Data.i %00, %01, %10, %01, %10 ;wall values for ID, N, E, S, W
EndDataSection

#cellDWidth = 4

Structure mazeOutput
  vWall.s
  hWall.s
EndStructure


;setup reference values indexed by type and direction from current map cell
Global Dim offset.POINT(#numOffsets, #numDirs)
Define i, j
For i = 0 To #numDirs
  For j = 0 To #numOffsets
    Read.i offset(j, i)\x: Read.i offset(j, i)\y
  Next
Next

Global Dim wallvalue(#numDirs)
For i = 0 To #numDirs: Read.i wallvalue(i): Next


Procedure makeDisplayMazeRow(Array mazeRow.mazeOutput(1), Array maze(2), y)
  ;modify mazeRow() to produce output of 2 strings showing the vertical walls above and horizontal walls across a given maze row
  Protected x, vWall.s, hWall.s
  Protected mazeWidth = ArraySize(maze(), 1), mazeHeight = ArraySize(maze(), 2)

  vWall = "": hWall = ""
  For x = 0 To mazeWidth
    If maze(x, y) & wallvalue(#dir_N): vWall + "+   ": Else: vWall + "+---": EndIf
    If maze(x, y) & wallvalue(#dir_W): hWall + "    ": Else: hWall + "|   ": EndIf
  Next
  mazeRow(0)\vWall = Left(vWall, mazeWidth * #cellDWidth + 1)
  If y <> mazeHeight: mazeRow(0)\hWall = Left(hWall, mazeWidth * #cellDWidth + 1): Else: mazeRow(0)\hWall = "": EndIf
EndProcedure

Procedure displayMaze(Array maze(2))
  Protected x, y, vWall.s, hWall.s, mazeHeight = ArraySize(maze(), 2)
  Protected Dim mazeRow.mazeOutput(0)

  For y = 0 To mazeHeight
    makeDisplayMazeRow(mazeRow(), maze(), y)
    PrintN(mazeRow(0)\vWall): PrintN(mazeRow(0)\hWall)
  Next
EndProcedure

Procedure generateMaze(Array maze(2), mazeWidth, mazeHeight)
  Dim maze(mazeWidth, mazeHeight) ;Each cell specifies walls present above and to the left of it,
                                  ;array includes an extra row and column for the right and bottom walls
  Dim visited(mazeWidth + 1, mazeHeight + 1) ;Each cell represents a cell of the maze, an extra line of cells are included
                                             ;as padding around the representative cells for easy border detection

  Protected i
  ;mark outside border as already visited (off limits)
  For i = 0 To mazeWidth
    visited(i + offset(#visited, #dir_N)\x, 0 + offset(#visited, #dir_N)\y) = #True
    visited(i + offset(#visited, #dir_S)\x, mazeHeight - 1 + offset(#visited, #dir_S)\y) = #True
  Next
  For i = 0 To mazeHeight
    visited(0 + offset(#visited, #dir_W)\x, i + offset(#visited, #dir_W)\y) = #True
    visited(mazeWidth - 1 + offset(#visited, #dir_E)\x, i + offset(#visited, #dir_E)\y) = #True
  Next

  ;generate maze
  Protected x = Random(mazeWidth - 1), y = Random (mazeHeight - 1), cellCount, nextCell
  visited(x + offset(#visited, #dir_ID)\x, y + offset(#visited, #dir_ID)\y) = #True
  PrintN("Maze of size " + Str(mazeWidth) + " x " + Str(mazeHeight) + ", generation started at " + Str(x) + " x " + Str(y))

  NewList stack.POINT()
  Dim unvisited(#numDirs - #firstDir)
  Repeat
    cellCount = 0
    For i = #firstDir To #numDirs
      If Not visited(x + offset(#visited, i)\x, y + offset(#visited, i)\y)
        unvisited(cellCount) = i: cellCount + 1
      EndIf
    Next

    If cellCount
      nextCell = unvisited(Random(cellCount - 1))
      visited(x + offset(#visited, nextCell)\x, y + offset(#visited, nextCell)\y) = #True
      maze(x + offset(#wall, nextCell)\x, y + offset(#wall, nextCell)\y) | wallvalue(nextCell)

      If cellCount > 1
        AddElement(stack())
        stack()\x = x: stack()\y = y
      EndIf
      x + offset(#maze, nextCell)\x: y + offset(#maze, nextCell)\y
    ElseIf ListSize(stack()) > 0
      x = stack()\x: y = stack()\y
      DeleteElement(stack())
    Else
      Break  ;end maze generation
    EndIf
  ForEver

  ; ;mark random entry and exit point
  ; x = Random(mazeWidth - 1): y = Random(mazeHeight - 1)
  ; maze(x, 0) | wallvalue(#dir_N): maze(mazeWidth, y) | wallvalue(#dir_E)
  ProcedureReturn
EndProcedure

If OpenConsole()
  Dim maze(0, 0)
  Define mazeWidth = Random(5) + 7: mazeHeight = Random(5) + 3
  generateMaze(maze(), mazeWidth, mazeHeight)
  displayMaze(maze())

  Print(#CRLF$ + #CRLF$ + "Press ENTER to exit"): Input()
  CloseConsole()
EndIf
```

The maze is represented by an array of cells where each cell indicates the walls present above (#dir_N) and to its left (#dir_W).  Maze generation is done with a additional array marking the visited cells.  Neither an entry nor an exit are created, these were not part of the task.  A simple means of doing so is included but has been commented out.

```txt
Maze of size 11 x 8, generation started at 9 x 3
+---+---+---+---+---+---+---+---+---+---+---+
|   |           |           |               |
+   +   +---+   +   +---+   +   +---+   +   +
|   |       |       |       |   |       |   |
+   +---+   +---+---+   +---+   +   +---+---+
|   |       |       |           |           |
+   +   +---+---+   +---+---+---+---+---+   +
|       |           |       |       |       |
+   +---+   +---+   +   +---+   +   +---+---+
|           |   |   |           |           |
+---+---+---+   +   +   +---+---+   +---+   +
|           |       |           |       |   |
+   +---+---+   +---+   +---+---+   +   +---+
|       |       |       |       |   |       |
+   +   +   +---+---+---+   +   +---+---+   +
|   |                       |               |
+---+---+---+---+---+---+---+---+---+---+---+
```



## Python


```python
from random import shuffle, randrange

def make_maze(w = 16, h = 8):
    vis = [[0] * w + [1] for _ in range(h)] + [[1] * (w + 1)]
    ver = [["|  "] * w + ['|'] for _ in range(h)] + [[]]
    hor = [["+--"] * w + ['+'] for _ in range(h + 1)]

    def walk(x, y):
        vis[y][x] = 1

        d = [(x - 1, y), (x, y + 1), (x + 1, y), (x, y - 1)]
        shuffle(d)
        for (xx, yy) in d:
            if vis[yy][xx]: continue
            if xx == x: hor[max(y, yy)][x] = "+  "
            if yy == y: ver[y][max(x, xx)] = "   "
            walk(xx, yy)

    walk(randrange(w), randrange(h))

    s = ""
    for (a, b) in zip(hor, ver):
        s += ''.join(a + ['\n'] + b + ['\n'])
    return s

if __name__ == '__main__':
    print(make_maze())
```

```txt
+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
|        |     |     |                    |     |
+  +  +  +  +  +  +  +  +--+--+--+--+--+  +--+  +
|  |  |     |  |  |     |     |        |        |
+--+  +--+--+  +  +--+--+--+  +  +--+  +--+--+  +
|     |     |  |  |  |        |     |  |        |
+  +--+  +--+  +  +  +  +  +  +--+  +  +  +--+--+
|  |           |  |     |  |     |  |     |     |
+  +--+  +--+--+  +  +--+  +--+--+  +--+--+  +  +
|     |  |        |     |           |        |  |
+--+  +  +  +--+--+--+  +--+--+--+--+--+--+--+  +
|     |  |  |        |        |           |     |
+  +--+--+  +--+--+  +--+--+  +--+  +--+  +  +  +
|        |        |        |        |     |  |  |
+  +--+  +--+--+--+  +  +--+--+--+--+  +--+  +  +
|     |              |                       |  |
+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
```



## Racket


Maze generator

```racket

#lang racket

;; the structure representing a maze of size NxM
(struct maze (N M tbl))

;; managing cell properties
(define (connections tbl c) (dict-ref tbl c '()))

(define (connect! tbl c n)
  (dict-set! tbl c (cons n (connections tbl c)))
  (dict-set! tbl n (cons c (connections tbl n))))

(define (connected? tbl a b) (member a (connections tbl b)))

;; Returns a maze of a given size
;; build-maze :: Index Index -> Maze
(define (build-maze N M)
  (define tbl (make-hash))
  (define (visited? tbl c) (dict-has-key? tbl c))
  (define (neigbours c)
    (filter
     (match-lambda [(list i j) (and (<= 0 i (- N 1)) (<= 0 j (- M 1)))])
     (for/list ([d '((0 1) (0 -1) (-1 0) (1 0))]) (map + c d))))
  ; generate the maze
  (let move-to-cell ([c (list (random N) (random M))])
    (for ([n (shuffle (neigbours c))] #:unless (visited? tbl n))
      (connect! tbl c n)
      (move-to-cell n)))
  ; return the result
  (maze N M tbl))

```


Printing out the maze


```racket

;; Shows a maze
(define (show-maze m)
  (match-define (maze N M tbl) m)
  (for ([i N]) (display "+---"))
  (displayln "+")
  (for ([j M])
    (display "|")
    (for ([i (- N 1)])
      (if (connected? tbl (list i j) (list (+ 1 i) j))
          (display "    ")
          (display "   |")))
    (display "   |")
    (newline)
    (for ([i N])
      (if (connected? tbl (list i j) (list i (+ j 1)))
          (display "+   ")
          (display "+---")))
    (displayln "+"))
  (newline))

```


Example:

```txt

-> (define m (build-maze 10 7))
-> (show-maze m)
+---+---+---+---+---+---+---+---+---+---+
|       |       |       |               |
+   +---+---+   +   +   +---+   +---+   +
|           |   |   |       |       |   |
+   +   +---+   +   +---+   +---+---+   +
|   |       |       |   |           |   |
+   +---+   +---+---+   +---+---+   +   +
|   |   |               |       |       |
+   +   +---+---+---+   +   +   +---+   +
|       |       |       |   |   |       |
+---+   +   +---+   +---+   +   +   +---+
|   |       |       |       |       |   |
+   +---+   +   +---+   +---+---+---+   +
|           |                           |
+---+---+---+---+---+---+---+---+---+---+

```



## Rascal

```rascal
import IO;
import util::Math;
import List;

public void make_maze(int w, int h){
	vis = [[0 | _ <- [1..w]] | _ <- [1..h]];
	ver = [["|  "| _ <- [1..w]] + ["|"] | _ <- [1..h]] + [[]];
	hor = [["+--"| _ <- [1..w]] + ["+"] | _ <- [1..h + 1]];

	void walk(int x, int y){
		vis[y][x] = 1;

		d = [<x - 1, y>, <x, y + 1>, <x + 1, y>, <x, y - 1>];
		while (d != []){
			<<xx, yy>, d> = takeOneFrom(d);
			if (xx < 0 || yy < 0 || xx >= w || yy >= w) continue;
			if (vis[yy][xx] == 1) continue;
			if (xx == x) hor[max([y, yy])][x] = "+  ";
			if (yy == y) ver[y][max([x, xx])] = "   ";
			walk(xx, yy);
			}
 	}

	walk(arbInt(w), arbInt(h));
	for (<a, b> <- zip(hor, ver)){
		println(("" | it + "<z>" | z <- a));
		println(("" | it + "<z>" | z <- b));
	}
}
```



```txt
rascal>make_maze(10,10)
+--+--+--+--+--+--+--+--+--+--+
|     |        |              |
+  +--+  +--+  +--+--+--+  +  +
|  |     |  |           |  |  |
+  +  +--+  +--+--+--+  +  +  +
|  |     |     |        |  |  |
+  +--+--+  +  +  +--+--+--+  +
|           |  |  |           |
+  +--+--+--+  +  +  +--+--+--+
|        |     |  |  |        |
+  +--+  +--+--+  +  +  +--+  +
|  |     |        |        |  |
+  +  +--+  +--+--+  +--+--+  +
|  |  |     |     |  |  |     |
+--+  +  +--+  +--+  +  +  +  +
|     |     |           |  |  |
+  +  +--+  +--+--+--+--+  +  +
|  |  |     |     |     |  |  |
+  +--+  +--+  +  +  +  +  +  +
|              |     |     |  |
+--+--+--+--+--+--+--+--+--+--+

ok

```



## REXX


### prettified maze version

In order to preserve the aspect ratio (for most display terminals), several   '''changestr'''   invocations and

some other instructions were added to increase the horizontal dimension (cell size).

```rexx
/*REXX program generates and displays a  rectangular  solvable maze  (of any size).     */
parse arg rows cols seed .                       /*allow user to specify the maze size. */
if rows='' | rows==','  then rows= 19            /*No rows given?  Then use the default.*/
if cols='' | cols==','  then cols= 19            /* " cols   "  ?    "   "   "     "    */
if datatype(seed, 'W')  then call random ,,seed  /*use a random  seed for repeatability.*/
ht=0;                        @.=0                /*HT= # rows in grid;  @.: default cell*/
call makeRow  '┌'copies('~┬', cols - 1)'~┐'      /*construct the top edge of the maze.  */
                                                 /* [↓]  construct the maze's grid.     */
      do    r=1  for rows;   _=;     __=;      hp= "|";              hj= '├'
         do c=1  for cols;   _= _ || hp'1';    __= __ || hj"~";      hj= '┼';      hp= "│"
         end   /*c*/
                        call makeRow  _'│'       /*construct the right edge of the cells*/
      if r\==rows  then call makeRow __'┤'       /*    "      "    "     "   "  "  maze.*/
      end      /*r*/                             /* [↑]  construct the maze's grid.     */

call makeRow  '└'copies("~┴",  cols - 1)'~┘'     /*construct the bottom edge of the maze*/
r!= random(1, rows) *2;     c!= random(1, cols) *2;      @.r!.c!= 0   /*choose 1st cell.*/
                                                 /* [↓]  traipse through the maze.      */
  do forever;    n= hood(r!, c!);    if n==0  then if \fCell()  then leave  /*¬freecell?*/
  call ?;        @._r._c= 0                      /*get the (next) maze direction to go. */
  ro= r!;        co= c!;     r!= _r;    c!= _c   /*save original maze cell coordinates. */
  ?.zr= ?.zr % 2;            ?.zc= ?.zc % 2      /*get the maze row and cell directions.*/
  rw= ro + ?.zr;             cw= co + ?.zc       /*calculate the next row and column.   */
  @.rw.cw= .                                     /*mark the maze cell as being visited. */
  end   /*forever*/
                                                 /* [↓]  display maze to the terminal.  */
         do     r=1  for ht;            _=
             do c=1  for cols*2 + 1;    _= _ || @.r.c;    end  /*c*/
         if \(r//2)  then _= translate(_, '\', .)                   /*trans to backslash*/
         @.r= _                                                     /*save the row in @.*/
         end   /*r*/

      do #=1  for ht;           _= @.#           /*display the maze to the terminal.    */
      call makeNice                              /*prettify cell corners and dead─ends. */
      _=  changestr( 1 ,  _   , 111     )        /*──────these four ────────────────────*/
      _=  changestr( 0 ,  _   , 000     )        /*───────── statements are ────────────*/
      _=  changestr( . ,  _   , "   "   )        /*────────────── used for preserving ──*/
      _=  changestr('~',  _   , "───"   )        /*────────────────── the aspect ratio. */
      say translate( _ , '─│' , "═|\10" )        /*make it presentable for the screen.  */
      end   /*#*/
      end   /*#*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
@:       parse arg _r,_c;     return  @._r._c    /*a fast way to reference a maze cell. */
makeRow: parse arg z; ht= ht+1;  do c=1  for length(z); @.ht.c=substr(z,c,1);  end; return
hood:    parse arg rh,ch;     return  @(rh+2,ch)  + @(rh-2,ch)  + @(rh,ch-2)  + @(rh,ch+2)
/*──────────────────────────────────────────────────────────────────────────────────────*/
?:         do forever;  ?.= 0;   ?= random(1, 4);     if ?==1  then ?.zc= -2     /*north*/
                                                      if ?==2  then ?.zr=  2     /* east*/
                                                      if ?==3  then ?.zc=  2     /*south*/
                                                      if ?==4  then ?.zr= -2     /* west*/
           _r= r! + ?.zr;       _c= c! + ?.zc;        if @._r._c == 1    then return
           end   /*forever*/
/*──────────────────────────────────────────────────────────────────────────────────────*/
fCell:     do     r=1  for rows;                rr= r + r
               do c=1  for cols;                         cc= c + c
               if hood(rr,cc)==1  then do;  r!= rr;  c!= cc;   @.r!.c!= 0;  return 1;  end
               end   /*c*/                       /* [↑]  r! and c!  are used by invoker.*/
           end       /*r*/;       return 0
/*──────────────────────────────────────────────────────────────────────────────────────*/
makeNice: width= length(_);     old= # - 1;     new= # + 1;     old_= @.old;   new_= @.new
          if left(_, 2)=='├.'  then _= translate(_, "|", '├')
          if right(_,2)=='.┤'  then _= translate(_, "|", '┤')

             do  k=1  for  width  while #==1;         z= substr(_, k, 1) /*maze top row.*/
             if z\=='┬'                  then iterate
             if substr(new_, k, 1)=='\'  then _= overlay("═", _, k)
             end   /*k*/

             do  k=1  for  width  while #==ht;        z= substr(_, k, 1) /*maze bot row.*/
             if z\=='┴'                  then iterate
             if substr(old_, k, 1)=='\'  then _= overlay("═", _, k)
             end   /*k*/

             do  k=3  to  width-2  by 2  while #//2;  z= substr(_, k, 1) /*maze mid rows*/
             if z\=='┼'   then iterate
             le= substr(_   , k-1, 1)
             ri= substr(_   , k+1, 1)
             up= substr(old_, k  , 1)
             dw= substr(new_, k  , 1)
                    select
                    when le== .  & ri== .  & up=='│' & dw=="│"  then _= overlay('|', _, k)
                    when le=='~' & ri=="~" & up=='\' & dw=="\"  then _= overlay('═', _, k)
                    when le=='~' & ri=="~" & up=='\' & dw=="│"  then _= overlay('┬', _, k)
                    when le=='~' & ri=="~" & up=='│' & dw=="\"  then _= overlay('┴', _, k)
                    when le=='~' & ri== .  & up=='\' & dw=="\"  then _= overlay('═', _, k)
                    when le== .  & ri=="~" & up=='\' & dw=="\"  then _= overlay('═', _, k)
                    when le== .  & ri== .  & up=='│' & dw=="\"  then _= overlay('|', _, k)
                    when le== .  & ri== .  & up=='\' & dw=="│"  then _= overlay('|', _, k)
                    when le== .  & ri=="~" & up=='\' & dw=="│"  then _= overlay('┌', _, k)
                    when le== .  & ri=="~" & up=='│' & dw=="\"  then _= overlay('└', _, k)
                    when le=='~' & ri== .  & up=='\' & dw=="│"  then _= overlay('┐', _, k)
                    when le=='~' & ri== .  & up=='│' & dw=="\"  then _= overlay('┘', _, k)
                    when le=='~' & ri== .  & up=='│' & dw=="│"  then _= overlay('┤', _, k)
                    when le== .  & ri=="~" & up=='│' & dw=="│"  then _= overlay('├', _, k)
                    otherwise   nop
                    end   /*select*/
             end          /*k*/;                   return
```

Some older REXXes don't have a   '''changestr'''   BIF, so one is included here   ──►   [[CHANGESTR.REX]].

```txt

┌───────────┬───────────────┬───────────────────────┬───────────────────┬───┐
│           │               │                       │                   │   │
│   ────┐   │   │   │   │   │   ┌───────┐   ────────┤   ────┐   ────┐   │   │
│       │   │   │   │   │       │       │           │       │       │       │
│   │   └───┘   │   │   │   ────┤   │   └───┐   │   └────   ├────   │   ┌───┤
│   │           │   │   │       │   │       │   │           │       │   │   │
│   │   ┌───────┤   │   └───────┘   ├────   │   └───┐   ┌───┘   │   │   │   │
│   │   │       │   │               │       │       │   │       │   │       │
│   │   └────   │   ├───────┐   │   │   ┌───┴───┐   ├───┘   ┌───┘   │   ────┤
│   │           │   │       │   │   │   │       │   │       │       │       │
│   ├───────┐   │   │   │   │   │   │   │   │   └───┤   ────┤   ────┴───┐   │
│   │       │   │       │   │   │   │   │   │       │       │           │   │
│   │   │   │   ├────   │   │   ├───┘   │   └───┐   └───┐   └───┐   │   └───┤
│       │   │   │       │   │   │       │       │       │       │   │       │
├───────┘   │   │   ────┤   │   │   │   └───┐   └───────┴────   ├───┴───┐   │
│           │   │       │   │   │   │       │                   │       │   │
├───────────┴───┴────   │   │   │   └───────┴───────────────────┤   │   │   │
│                       │   │   │                               │   │   │   │
│   ┌───────┬───────┬───┘   │   ├───────────────────────┬────   │   │   │   │
│   │       │       │       │   │                       │       │   │       │
│   │   │   │   │   │   ────┼───┘   ┌───────────────┐   │   ┌───┘   ├───────┤
│       │   │   │   │       │       │               │       │       │       │
│   ────┤   │   │   │   │   │   ┌───┘   │   ────┐   │   ────┤   ┌───┴────   │
│       │   │   │   │   │   │   │       │       │   │       │   │           │
├────   │   └───┘   ├───┘   │   │   ┌───┴───────┤   │   │   │   │   │   ────┤
│       │           │       │       │           │   │   │   │       │       │
│   ────┴───┬───────┘   ┌───┴───────┘   ┌────   │   └───┤   └───┬───┴───┐   │
│           │           │               │       │       │       │       │   │
│   ┌────   │   ────────┘   ┌───────────┼───────┴────   │   │   └────   │   │
│   │       │               │           │               │   │           │   │
│   │   ┌───┴───┬───────────┤   ┌───┐   │   ┌───────┬───┤   │   ────────┤   │
│   │   │       │           │   │   │       │       │   │   │           │   │
│   │   │   │   │   ────────┤   │   │   ┌───┘   │   │   │   ├───────┬───┘   │
│   │       │   │           │       │   │       │   │       │       │       │
│   ├───────┤   └────────   ├───────┤   │   │   │   │   ┌───┘   │   │   ────┤
│   │       │               │       │   │   │   │   │   │       │   │       │
│   │   │   │   ────┐   ┌───┘   │   └───┘   │   │   ├───┘   │   │   └────   │
│       │   │       │   │       │           │   │   │       │   │           │
├───┬───┘   ├────   ├───┘   │   └───────┬───┘   │   │   ────┴───┴───┐   ────┤
│   │       │       │       │           │       │   │               │       │
│   │   │   │   ┌───┤   ┌───┴───┬───┐   │   │   │   └───┬───────┐   │   │   │
│       │   │   │   │   │       │   │   │   │   │       │       │   │   │   │
│   ┌───┴───┘   │   │   └───┐   │   │   │   ├───┴───┐   │   │   │   ├───┘   │
│   │           │   │       │   │       │   │       │       │   │   │       │
│   │   ────────┘   │   │   │   │   ────┤   │   │   └───────┤   │   │   ────┤
│   │               │   │       │       │       │           │       │       │
└───┴───────────────┴───┴───────┴───────┴───────┴───────────┴───────┴───────┘

```



### simpler version of above

The above REXX version had a quite of bit of code to "dress up" the maze presentation,   so a slimmed-down version

was included here for easier reading and understanding of the program's logic.

```rexx
/*REXX program  generates and displays  a  rectangular  solvable maze  (of any size).   */
parse arg rows cols seed .                       /*allow user to specify the maze size. */
if rows='' | rows==","  then rows= 19            /*No rows given?  Then use the default.*/
if cols='' | cols==","  then cols= 19            /* " cols   "  ?    "   "   "     "    */
if datatype(seed, 'W')  then call random ,,seed  /*use a random  seed for repeatability.*/
ht=0;                        @.=0                /*HT= # rows in grid;  @.: default cell*/
call makeRow '┌'copies("─┬", cols-1)'─┐'         /*construct the  top edge  of the maze.*/

      do    r=1  for rows;   _=;     __=;      hp= "|";              hj= '├'
         do c=1  for cols;   _= _ || hp'1';    __= __ || hj"─";      hj= '┼';      hp= "│"
         end   /*c*/
                        call makeRow  _'│'       /*construct the right edge of the cells*/
      if r\==rows  then call makeRow __'┤'       /*    "      "    "     "   "  "  maze.*/
      end      /*r*/                             /* [↑]  construct the maze's grid.     */

call makeRow '└'copies("─┴", cols-1)'─┘'         /*construct the bottom edge of the maze*/
r!= random(1, rows)*2;    c!= random(1, cols)*2;     @.r!.c!= 0   /*choose the 1st cell.*/
                                                 /* [↓]  traipse through the maze.      */
  do forever;        n= hood(r!, c!)             /*number of free maze cells.           */
  if n==0  then if \fCell()  then leave          /*if no free maze cells left, then done*/
  call ?;            @._r._c= 0                  /*get the (next) maze direction to go. */
  ro= r!;            co= c!;    r!= _r;   c!= _c /*save the original cell coordinates.  */
  ?.zr= ?.zr % 2;    ?.zc= ?.zc % 2              /*get the maze row and cell directions.*/
  rw= ro + ?.zr;     cw= co + ?.zc               /*calculate the next maze row and col. */
  @.rw.cw=.                                      /*mark the maze cell as being visited. */
  end   /*forever*/
                                                 /* [↓]  display maze to the terminal.  */
         do     r=1  for ht;           _=
             do c=1  for cols*2 + 1;   _= _ || @.r.c;   end  /*c*/
         if \(r//2)  then _= translate(_, '\',.) /*translate a  period  to a  backslash.*/
         _=  changestr( 1 ,  _  , 111   )        /*──────these four ────────────────────*/
         _=  changestr( 0 ,  _  , 000   )        /*───────── statements are ────────────*/
         _=  changestr( . ,  _  , "   " )        /*────────────── used for preserving ──*/
         _=  changestr('─',  _  , "───" )        /*────────────────── the aspect ratio. */
         say translate( _ , '│' , "|\10")        /*make it presentable for the screen.  */
         end   /*r*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
@:       parse arg _r,_c;     return  @._r._c    /*a fast way to reference a maze cell. */
makeRow: parse arg z; ht= ht+1;  do c=1  for length(z); @.ht.c=substr(z,c,1);  end; return
hood:    parse arg rh,ch;     return  @(rh+2,ch)  + @(rh-2,ch)  + @(rh,ch-2)  + @(rh,ch+2)
/*──────────────────────────────────────────────────────────────────────────────────────*/
?:         do forever;  ?.= 0;   ?= random(1, 4);     if ?==1  then ?.zc= -2     /*north*/
                                                      if ?==2  then ?.zr=  2     /* east*/
                                                      if ?==3  then ?.zc=  2     /*south*/
                                                      if ?==4  then ?.zr= -2     /* west*/
           _r= r! + ?.zr;       _c= c! + ?.zc;        if @._r._c == 1    then return
           end   /*forever*/
/*──────────────────────────────────────────────────────────────────────────────────────*/
fCell:     do     r=1  for rows;                rr= r + r
               do c=1  for cols;                         cc= c + c
               if hood(rr,cc)==1  then do;  r!= rr;  c!= cc;   @.r!.c!= 0;  return 1;  end
               end   /*c*/                       /* [↑]  r! and c!  are used by invoker.*/
           end       /*r*/;       return 0
```

```txt

┌───┬───┬───┬───┬───┬───┬───┬───┬───┬───┐
│                                   │   │
├   ┼   ┼───┼───┼───┼   ┼───┼───┼   ┼   ┤
│   │   │               │       │       │
├   ┼   ┼───┼───┼───┼───┼   ┼   ┼───┼   ┤
│   │                   │   │       │   │
├   ┼───┼───┼   ┼───┼   ┼   ┼───┼   ┼───┤
│           │   │   │   │   │   │       │
├   ┼───┼   ┼   ┼   ┼   ┼   ┼   ┼───┼   ┤
│   │       │   │   │       │       │   │
├   ┼───┼───┼   ┼   ┼   ┼───┼───┼   ┼   ┤
│           │       │       │   │       │
├───┼───┼   ┼───┼───┼   ┼   ┼   ┼───┼───┤
│       │           │   │   │           │
├   ┼   ┼───┼───┼   ┼───┼   ┼   ┼───┼   ┤
│   │               │       │   │       │
├   ┼───┼───┼───┼───┼   ┼───┼───┼   ┼   ┤
│                   │   │       │   │   │
├   ┼───┼───┼───┼   ┼───┼   ┼   ┼   ┼   ┤
│               │           │       │   │
└───┴───┴───┴───┴───┴───┴───┴───┴───┴───┘

```



### version 3


```rexx
/* REXX ***************************************************************
* 04.09.2013 Walter Pachl
**********************************************************************/
Parse Arg imax jmax seed
If imax='' Then imax=10
If jmax='' Then jmax=15
If seed='' Then seed=4711
c='123456789'||,
  'abcdefghijklmnopqrstuvwxyz'||,
  translate('abcdefghijklmnopqrstuvwxyz')
c=copies(c,10)
call random 1,10,seed
id=2*imax+1                         /* vertical dimension of a.i.j   */
jd=2*jmax+1                         /* horizontal dimension of a.i.j */
a.=1                                   /* mark all borders present   */
p.='.'                                 /* Initialize all grid points */
pl.=0                                  /* path list                  */
ii=random(1,imax)                      /* find a start position      */
jj=random(1,jmax)
p=1                                    /* first position             */
na=1                                   /* number of points used      */
Do si=1 To 1000                        /* Do Forever - see Leave     */
  /* Say 'loop' si na                     show progress              */
  Call path ii,jj                /* compute a path starting at ii/jj */
  If na=imax*jmax Then                 /* all points used            */
    Leave                              /* we are done                */
  Parse Value select_next() With ii jj /* get a new start from a path*/
  End

/***************
Do i=1 To imax
  ol=''
  Do j=1 To jmax
    ol=ol||p.i.j
    End
    Say ol
  End
Say ' '
***************/
Call show
/***********************
Do pi=1 To imax*jmax
  Say right(pi,3) pos.pi
  End
***********************/
Exit

path: Procedure Expose p. np. p pl. c a. na imax jmax id jd pos.
/**********************************************************************
* compute a path starting from point (ii,jj)
**********************************************************************/
  Parse Arg ii,jj
  p.ii.jj='1'
  pos.p=ii jj
  Do pp=1 to 50                /* compute a path of maximum length 50*/
    neighbors=neighbors(ii,jj)         /* number of free neighbors   */
    Select
      When neighbors=1 Then            /* just one                   */
        Call advance 1,ii,jj           /* go for it                  */
      When neighbors>0 Then Do         /* more Than 1                */
        ch=random(1,neighbors)         /* choose one possibility     */
        Call advance ch,ii,jj          /* and go for that            */
        End
      Otherwise                        /* none available             */
        Leave
      End
    End
  Return

neighbors: Procedure Expose p. np.  imax jmax neighbors pl.
/**********************************************************************
* count the number of free neighbors of point (i,j)
**********************************************************************/
  Parse Arg i,j
  neighbors=0
  in=i-1; If in>0     Then Call check in,j
  in=i+1; If in<=imax Then Call check in,j
  jn=j-1; If jn>0     Then Call check i,jn
  jn=j+1; If jn<=jmax Then Call check i,jn
  Return neighbors

check: Procedure Expose p. imax jmax np. neighbors pl.
/**********************************************************************
* check if point (i,j) is free and note it as possible successor
**********************************************************************/
  Parse Arg i,j
  If p.i.j='.' Then Do                 /* point is free              */
    neighbors=neighbors+1              /* number of free neighbors   */
    np.neighbors=i j                   /* note it as possible choice */
    End
  Return

advance: Procedure Expose p pos. np. p. c ii jj a. na pl. pos.
/**********************************************************************
* move to the next point of the current path
**********************************************************************/
  Parse Arg ch,pii,pjj
  Parse Var np.ch ii jj
  p=p+1                                /* position number            */
  pos.p=ii jj                          /* note its coordinates       */
  p.ii.jj=substr(c,p,1)                /* mark the point as used     */
  ai=pii+ii                            /* vertical border position   */
  aj=pjj+jj                            /* horizontal border position */
  a.ai.aj=0                            /* tear the border down       */
  na=na+1                              /* number of used positions   */
  z=pl.0+1                             /* add the point to the list  */
  pl.z=ii jj                           /* of follow-up start pos.    */
  pl.0=z
  Return

show: Procedure Expose id jd a.  na
/*********************************************************************
* Show the resulting maze
*********************************************************************/
  say 'mgg 6 18 4711'
  say 'show na='na
  Do i=1 To id
    ol=''
    Do j=1 To jd
      If i//2=1 Then Do                /* odd lines                 */
        If a.i.j=1 Then Do             /* border to be drawn        */
          If j//2=0 Then
            ol=ol||'---'               /* draw the border           */
          Else
            ol=ol'+'
          End
        Else Do                        /* border was torn down      */
          If j//2=0 Then
            ol=ol||'   '               /* blanks instead of border  */
          Else
            ol=ol||'+'
          End
        End
      Else Do                          /* even line                 */
        If a.i.j=1 Then Do
          If j//2=0 Then               /* even column               */
            ol=ol||'   '               /* moving space              */
          Else                         /* odd column                */
            ol=ol||'|'                 /* draw the border           */
          End
        Else                           /* border was torn down      */
          ol=ol||' '                   /* blank instead of border   */
        End
      End
    Select
      When i=6 Then ol=overlay('A',ol,11)
      When i=8 Then ol=overlay('B',ol, 3)
      Otherwise Nop
      End
    Say ol format(i,2)
    End
  Return

select_next: Procedure Expose p. pl. imax jmax
/*********************************************************************
* look for a point to start the nnext path
*********************************************************************/
  Do Until neighbors>0                 /* loop until one is found   */
    n=pl.0                             /* number of points recorded */
    s=random(1,n)                      /* pick a random index       */
    Parse Var pl.s is js               /* its coordinates           */
    neighbors=neighbors(is,js)         /* count free neighbors      */
    If neighbors=0 Then Do             /* if there is none          */
      pl.s=pl.n                        /* remove this point         */
      pl.0=pl.0-1
      End
    End
  Return is js                         /* return the new start point*/
```

Output:

```txt

rexx mgg 6 18 4711
show na=108
+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+  1
|           |   |       |           |               |           |       |  2
+   +---+   +   +   +   +   +---+   +   +---+---+   +   +---+   +   +---+  3
|       |   |       |           |   |   |       |       |   |   |       |  4
+---+---+   +   +---+---+---+   +   +   +   +   +---+---+   +   +---+   +  5
|       | A                 |   |   |       |   |           |           |  6
+   +   +---+---+---+---+   +---+   +---+---+   +   +---+   +   +---+---+  7
| B |   |       |       |       |       |       |   |       |   |       |  8
+   +   +---+   +   +   +---+   +   +   +   +---+   +---+   +   +   +   +  9
|   |   |       |   |       |   |   |   |       |       |   |       |   | 10
+   +   +   +---+---+---+   +   +   +   +---+---+---+   +   +   +---+   + 11
|   |                           |   |                   |       |       | 12
+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+ 13

```



## Ruby


```ruby
class Maze
  DIRECTIONS = [ [1, 0], [-1, 0], [0, 1], [0, -1] ]

  def initialize(width, height)
    @width   = width
    @height  = height
    @start_x = rand(width)
    @start_y = 0
    @end_x   = rand(width)
    @end_y   = height - 1

    # Which walls do exist? Default to "true". Both arrays are
    # one element bigger than they need to be. For example, the
    # @vertical_walls[x][y] is true if there is a wall between
    # (x,y) and (x+1,y). The additional entry makes printing easier.
    @vertical_walls   = Array.new(width) { Array.new(height, true) }
    @horizontal_walls = Array.new(width) { Array.new(height, true) }
    # Path for the solved maze.
    @path             = Array.new(width) { Array.new(height) }

    # "Hack" to print the exit.
    @horizontal_walls[@end_x][@end_y] = false

    # Generate the maze.
    generate
  end

  # Print a nice ASCII maze.
  def print
    # Special handling: print the top line.
    puts @width.times.inject("+") {|str, x| str << (x == @start_x ? "   +" : "---+")}

    # For each cell, print the right and bottom wall, if it exists.
    @height.times do |y|
      line = @width.times.inject("|") do |str, x|
        str << (@path[x][y] ? " * " : "   ") << (@vertical_walls[x][y] ? "|" : " ")
      end
      puts line

      puts @width.times.inject("+") {|str, x| str << (@horizontal_walls[x][y] ? "---+" : "   +")}
    end
  end

  private

  # Reset the VISITED state of all cells.
  def reset_visiting_state
    @visited = Array.new(@width) { Array.new(@height) }
  end

  # Is the given coordinate valid and the cell not yet visited?
  def move_valid?(x, y)
    (0...@width).cover?(x) && (0...@height).cover?(y) && !@visited[x][y]
  end

  # Generate the maze.
  def generate
    reset_visiting_state
    generate_visit_cell(@start_x, @start_y)
  end

  # Depth-first maze generation.
  def generate_visit_cell(x, y)
    # Mark cell as visited.
    @visited[x][y] = true

    # Randomly get coordinates of surrounding cells (may be outside
    # of the maze range, will be sorted out later).
    coordinates = DIRECTIONS.shuffle.map { |dx, dy| [x + dx, y + dy] }

    for new_x, new_y in coordinates
      next unless move_valid?(new_x, new_y)

      # Recurse if it was possible to connect the current and
      # the cell (this recursion is the "depth-first" part).
      connect_cells(x, y, new_x, new_y)
      generate_visit_cell(new_x, new_y)
    end
  end

  # Try to connect two cells. Returns whether it was valid to do so.
  def connect_cells(x1, y1, x2, y2)
    if x1 == x2
      # Cells must be above each other, remove a horizontal wall.
      @horizontal_walls[x1][ [y1, y2].min ] = false
    else
      # Cells must be next to each other, remove a vertical wall.
      @vertical_walls[ [x1, x2].min ][y1] = false
    end
  end
end

# Demonstration:
maze = Maze.new 20, 10
maze.print
```


```txt

+---+---+---+---+---+---+---+---+---+---+---+---+---+   +---+---+---+---+---+---+
|                   |   |                       |   |       |           |       |
+   +---+   +---+   +   +   +---+---+---+   +   +   +---+   +   +---+   +---+   +
|   |   |   |           |   |       |       |   |       |       |   |   |       |
+   +   +   +---+---+   +   +   +---+   +---+   +---+   +---+---+   +   +   +---+
|   |   |           |   |       |       |       |           |   |       |       |
+   +   +---+---+   +---+---+   +   +---+---+   +   +---+   +   +   +---+---+   +
|   |           |           |   |           |       |       |   |               |
+   +---+---+   +---+---+   +   +---+---+   +---+---+   +---+   +---+---+---+   +
|               |       |   |       |   |   |                       |       |   |
+---+---+   +---+   +   +   +---+   +   +   +---+---+---+---+   +---+   +   +   +
|   |       |       |           |       |           |           |       |       |
+   +   +---+   +---+---+---+---+---+   +---+---+   +   +---+---+   +---+---+---+
|   |       |                       |   |       |   |       |   |   |           |
+   +---+   +---+---+---+---+---+   +   +---+   +   +---+   +   +   +---+---+   +
|       |           |   |           |           |       |       |   |           |
+   +   +---+---+   +   +   +---+---+---+---+   +---+   +---+---+   +   +---+---+
|   |       |       |   |       |               |       |           |           |
+   +   +---+   +---+   +---+   +   +---+---+---+   +---+   +---+---+---+---+   +
|   |                       |                   |                               |
+---+---+---+---+---+---+---+---+---+---+   +---+---+---+---+---+---+---+---+---+

```



## Rust

Uses the [https://crates.io/crates/rand rand] library

```rust
use rand::{thread_rng, Rng, rngs::ThreadRng};

const WIDTH: usize = 16;
const HEIGHT: usize = 16;

#[derive(Clone, Copy)]
struct Cell {
    col: usize,
    row: usize,
}

impl Cell {
    fn from(col: usize, row: usize) -> Cell {
        Cell {col, row}
    }
}

struct Maze {
    cells: [[bool; HEIGHT]; WIDTH],         //cell visited/non visited
    walls_h: [[bool; WIDTH]; HEIGHT + 1],   //horizontal walls existing/removed
    walls_v: [[bool; WIDTH + 1]; HEIGHT],   //vertical walls existing/removed
    thread_rng: ThreadRng,                  //Random numbers generator
}

impl Maze {

    ///Inits the maze, with all the cells unvisited and all the walls active
    fn new() -> Maze {
        Maze {
            cells: [[true; HEIGHT]; WIDTH],
            walls_h: [[true; WIDTH]; HEIGHT + 1],
            walls_v: [[true; WIDTH + 1]; HEIGHT],
            thread_rng: thread_rng(),
        }
    }

    ///Randomly chooses the starting cell
    fn first(&mut self) -> Cell {
        Cell::from(self.thread_rng.gen_range(0, WIDTH), self.thread_rng.gen_range(0, HEIGHT))
    }

    ///Opens the enter and exit doors
    fn open_doors(&mut self) {
        let from_top: bool = self.thread_rng.gen();
        let limit = if from_top { WIDTH } else { HEIGHT };
        let door = self.thread_rng.gen_range(0, limit);
        let exit = self.thread_rng.gen_range(0, limit);
        if from_top {
            self.walls_h[0][door] = false;
            self.walls_h[HEIGHT][exit] = false;
        } else {
            self.walls_v[door][0] = false;
            self.walls_v[exit][WIDTH] = false;
        }
    }

    ///Removes a wall between the two Cell arguments
    fn remove_wall(&mut self, cell1: &Cell, cell2: &Cell) {
        if cell1.row == cell2.row {
            self.walls_v[cell1.row][if cell1.col > cell2.col { cell1.col } else { cell2.col }] = false;
        } else {
            self.walls_h[if cell1.row > cell2.row { cell1.row } else { cell2.row }][cell1.col] = false;
        };
    }

    ///Returns a random non-visited neighbor of the Cell passed as argument
    fn neighbor(&mut self, cell: &Cell) -> Option<Cell> {
        self.cells[cell.col][cell.row] = false;
        let mut neighbors = Vec::new();
        if cell.col > 0 && self.cells[cell.col - 1][cell.row] { neighbors.push(Cell::from(cell.col - 1, cell.row)); }
        if cell.row > 0 && self.cells[cell.col][cell.row - 1] { neighbors.push(Cell::from(cell.col, cell.row - 1)); }
        if cell.col < WIDTH - 1 && self.cells[cell.col + 1][cell.row] { neighbors.push(Cell::from(cell.col + 1, cell.row)); }
        if cell.row < HEIGHT - 1 && self.cells[cell.col][cell.row + 1] { neighbors.push(Cell::from(cell.col, cell.row + 1)); }
        if neighbors.is_empty() {
            None
        } else {
            let next = neighbors.get(self.thread_rng.gen_range(0, neighbors.len())).unwrap();
            self.remove_wall(cell, next);
            Some(*next)
        }
    }

    ///Builds the maze (runs the Depth-first search algorithm)
    fn build(&mut self) {
        let mut cell_stack: Vec<Cell> = Vec::new();
        let mut next = self.first();
        loop {
            while let Some(cell) = self.neighbor(&next) {
                cell_stack.push(cell);
                next = cell;
            }
            match cell_stack.pop() {
                Some(cell) => next = cell,
                None => break,
            }
        }
    }

    ///Displays a wall
    fn paint_wall(h_wall: bool, active: bool) {
        if h_wall {
            print!("{}", if active { "+---" } else { "+   " });
        } else {
            print!("{}", if active { "|   " } else { "    " });
        }
    }

    ///Displays a final wall for a row
    fn paint_close_wall(h_wall: bool) {
        if h_wall { println!("+") } else { println!() }
    }

    ///Displays a whole row of walls
    fn paint_row(&self, h_walls: bool, index: usize) {
        let iter = if h_walls { self.walls_h[index].iter() } else { self.walls_v[index].iter() };
        for &wall in iter {
            Maze::paint_wall(h_walls, wall);
        }
        Maze::paint_close_wall(h_walls);
    }

    ///Paints the maze
    fn paint(&self) {
        for i in 0 .. HEIGHT {
            self.paint_row(true, i);
            self.paint_row(false, i);
        }
        self.paint_row(true, HEIGHT);
    }
}

fn main() {
    let mut maze = Maze::new();
    maze.build();
    maze.open_doors();
    maze.paint();
}
```


```txt
+---+---+---+---+---+---+---+   +---+---+---+---+---+---+---+---+
|           |           |                       |               |
+   +---+---+   +   +   +---+   +---+---+---+   +---+---+---+   +
|           |   |   |   |       |       |   |           |       |
+   +---+   +   +   +   +   +---+   +   +   +---+---+   +   +   +
|       |   |   |   |       |       |               |       |   |
+---+   +   +   +   +---+---+   +   +---+---+   +---+---+---+   +
|       |       |   |   |       |   |   |       |               |
+   +---+---+---+   +   +   +---+   +   +   +---+   +---+---+---+
|               |   |       |   |   |           |   |           |
+---+---+   +---+   +   +---+   +   +---+---+   +   +   +   +   +
|           |       |   |       |           |   |   |   |   |   |
+   +---+---+   +---+   +   +---+---+---+   +---+   +   +   +---+
|   |       |   |       |               |       |   |   |       |
+   +   +   +   +   +---+   +---+   +   +---+   +   +---+---+   +
|   |   |   |   |       |       |   |       |   |   |       |   |
+   +   +---+   +---+   +---+---+   +---+   +   +   +   +   +   +
|   |   |       |   |   |           |       |   |   |   |   |   |
+   +   +   +---+   +   +   +   +---+---+---+   +   +   +   +   +
|   |   |   |       |   |   |   |           |   |       |       |
+   +   +   +   +   +   +   +---+   +---+   +   +---+---+---+   +
|       |   |   |       |   |       |           |   |           |
+---+   +   +   +---+---+   +   +---+---+---+   +   +   +---+---+
|       |   |           |   |               |   |   |   |       |
+   +---+   +---+---+   +   +---+---+---+   +   +   +   +   +   +
|       |       |   |           |   |       |   |   |   |   |   |
+---+---+   +   +   +---+---+   +   +   +---+   +   +   +---+   +
|       |   |   |           |   |   |   |           |       |   |
+---+   +---+   +---+   +   +   +   +   +---+---+---+---+   +   +
|   |       |       |   |   |       |                   |       |
+   +---+   +---+   +   +---+---+   +---+---+---+---+   +---+   +
|                   |                               |           |
+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+   +
```



## Scala


```scala
import scala.util.Random

object MazeTypes {
  case class Direction(val dx: Int, val dy: Int)

  case class Loc(val x: Int, val y: Int) {
    def +(that: Direction): Loc = Loc(x + that.dx, y + that.dy)
  }

  case class Door(val from: Loc, to: Loc)

  val North = Direction(0,-1)
  val South = Direction(0,1)
  val West = Direction(-1,0)
  val East = Direction(1,0)
  val directions = Set(North, South, West, East)
}

object MazeBuilder {
  import MazeTypes._

  def shuffle[T](set: Set[T]): List[T] = Random.shuffle(set.toList)

  def buildImpl(current: Loc, grid: Grid): Grid = {
    var newgrid = grid.markVisited(current)
    val nbors = shuffle(grid.neighbors(current))
    nbors.foreach { n =>
      if (!newgrid.isVisited(n)) {
        newgrid = buildImpl(n, newgrid.markVisited(current).addDoor(Door(current, n)))
      }
    }
    newgrid
  }

  def build(width: Int, height: Int): Grid = {
    val exit = Loc(width-1, height-1)
    buildImpl(exit, new Grid(width, height, Set(), Set()))
  }
}

class Grid(val width: Int, val height: Int, val doors: Set[Door], val visited: Set[Loc]) {

  def addDoor(door: Door): Grid =
    new Grid(width, height, doors + door, visited)

  def markVisited(loc: Loc): Grid =
    new Grid(width, height, doors, visited + loc)

  def isVisited(loc: Loc): Boolean =
    visited.contains(loc)

  def neighbors(current: Loc): Set[Loc] =
    directions.map(current + _).filter(inBounds(_)) -- visited

  def printGrid(): List[String] = {
    (0 to height).toList.flatMap(y => printRow(y))
  }

  private def inBounds(loc: Loc): Boolean =
    loc.x >= 0 && loc.x < width && loc.y >= 0 && loc.y < height

  private def printRow(y: Int): List[String] = {
    val row = (0 until width).toList.map(x => printCell(Loc(x, y)))
    val rightSide = if (y == height-1) " " else "|"
    val newRow = row :+ List("+", rightSide)
    List.transpose(newRow).map(_.mkString)
  }

  private val entrance = Loc(0,0)

  private def printCell(loc: Loc): List[String] = {
    if (loc.y == height)
      List("+--")
    else List(
      if (openNorth(loc)) "+  " else "+--",
      if (openWest(loc) || loc == entrance) "   " else "|  "
    )
  }

  def openNorth(loc: Loc): Boolean = openInDirection(loc, North)

  def openWest(loc: Loc): Boolean = openInDirection(loc, West)

  private def openInDirection(loc: Loc, dir: Direction): Boolean =
    doors.contains(Door(loc, loc + dir)) || doors.contains(Door(loc + dir, loc))
}
```

```txt

+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
                     |     |                 |
+--+--+  +  +--+--+  +  +  +--+--+--+  +--+  +
|     |  |     |  |  |  |           |  |     |
+  +  +--+--+  +  +  +  +--+--+--+  +--+  +  +
|  |           |        |        |     |  |  |
+  +--+--+--+--+  +--+--+--+  +  +--+  +  +  +
|  |     |        |        |  |     |  |  |  |
+  +  +  +--+--+--+  +--+  +  +--+  +  +  +  +
|     |  |           |  |  |  |  |  |     |  |
+  +--+  +  +--+--+--+  +  +  +  +  +--+--+  +
|  |  |  |     |        |  |     |        |  |
+  +  +  +--+  +--+--+  +  +--+--+--+  +--+  +
|  |  |     |           |           |        |
+  +  +  +--+--+--+--+  +--+--+--+  +--+--+  +
|     |              |  |     |  |        |  |
+--+  +--+--+  +--+--+  +  +  +  +--+--+  +--+
|  |        |        |  |  |     |     |     |
+  +--+--+  +--+--+  +  +  +--+  +  +  +--+  +
|        |        |  |     |     |  |     |  |
+  +--+  +--+--+  +  +--+--+  +--+  +--+  +  +
|     |     |     |        |        |  |  |  |
+--+  +--+  +  +--+  +--+  +--+--+--+  +  +  +
|  |     |     |     |  |  |           |  |  |
+  +--+  +--+--+  +--+  +  +  +--+  +--+  +  +
|  |        |     |           |     |     |  |
+  +  +--+--+  +  +--+--+--+--+--+--+  +--+  +
|  |  |        |  |                    |  |  |
+  +  +  +--+--+--+  +--+--+--+--+--+--+  +  +
|     |                                   |
+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+

```


## Sidef

```ruby
var(w:5, h:5) = ARGV.map{.to_i}...
var avail = (w * h)

# cell is padded by sentinel col and row, so I don't check array bounds
var cell = (1..h -> map {([true] * w) + [false]} + [[false] * w+1])
var ver  = (1..h -> map {["|  "] * w })
var hor  = (0..h -> map {["+--"] * w })

func walk(x, y) {
    cell[y][x] = false;
    avail-- > 0 || return;  # no more bottles, er, cells

    var d = [[-1, 0], [0, 1], [1, 0], [0, -1]]
    while (!d.is_empty) {
        var i = d.pop_rand
        var (x1, y1) = (x + i[0], y + i[1])

        cell[y1][x1] || next

        if (x == x1) { hor[[y1, y].max][x] = '+  ' }
        if (y == y1) { ver[y][[x1, x].max] = '   ' }
        walk(x1, y1)
    }
}

walk(w.rand.int, h.rand.int)   # generate

for i in (0 .. h) {
    say (hor[i].join('') + '+')
    if (i < h) {
        say (ver[i].join('') + '|')
    }
}
```

```txt

+--+--+--+--+--+
|           |  |
+--+  +--+  +  +
|     |        |
+  +--+--+--+--+
|     |        |
+--+  +  +--+  +
|     |  |     |
+  +--+--+  +--+
|              |
+--+--+--+--+--+

```



## SuperCollider


```SuperCollider

// some useful functions
(
~grid = { 0 ! 60 } ! 60;

~at = { |coord|
	var col = ~grid.at(coord[0]);
	if(col.notNil) { col.at(coord[1]) }
};
~put = { |coord, value|
	var col = ~grid.at(coord[0]);
	if(col.notNil) { col.put(coord[1], value) }
};

~coord = ~grid.shape.rand;
~next = { |p|
	var possible = [p] + [[0, 1], [1, 0], [-1, 0], [0, -1]];
	possible = possible.select { |x|
		var c = ~at.(x);
		c.notNil and: { c == 0 }
	};
	possible.choose
};
~walkN = { |p, scale|
	var next = ~next.(p);
	if(next.notNil) {
		~put.(next, 1);
		Pen.lineTo(~topoint.(next, scale));
		~walkN.(next, scale);
		~walkN.(next, scale);
		Pen.moveTo(~topoint.(p, scale));
	};
};

~topoint = { |c, scale| (c + [1, 1] * scale).asPoint };

)

// do the drawing
(
var b, w;

b = Rect(100, 100, 700, 700);
w = Window("so-a-mazing", b);
w.view.background_(Color.black);

w.drawFunc = {
	var p = ~grid.shape.rand;
	var scale = b.width / ~grid.size * 0.98;
	Pen.moveTo(~topoint.(p, scale));
	~walkN.(p, scale);
	Pen.width = scale / 4;
	Pen.color = Color.white;
	Pen.stroke;
};
w.front.refresh;
)

```



## Swift

```Swift
import Foundation

extension Array {
    mutating func shuffle() {
        guard count > 1 else { return }

        for i in 0..<self.count - 1 {
            let j = Int(arc4random_uniform(UInt32(count - i))) + i
            guard i != j else { continue }
            swap(&self[i], &self[j])
        }
    }
}

enum Direction:Int {
    case north = 1
    case south = 2
    case east = 4
    case west = 8

    static var allDirections:[Direction] {
        return [Direction.north, Direction.south, Direction.east, Direction.west]
    }

    var opposite:Direction {
        switch self {
        case .north:
            return .south
        case .south:
            return .north
        case .east:
            return .west
        case .west:
            return .east
        }
    }

    var diff:(Int, Int) {
        switch self {
        case .north:
            return (0, -1)
        case .south:
            return (0, 1)
        case .east:
            return (1, 0)
        case .west:
            return (-1, 0)
        }
    }

    var char:String {
        switch self {
        case .north:
            return "N"
        case .south:
            return "S"
        case .east:
            return "E"
        case .west:
            return "W"
        }
    }

}

class MazeGenerator {
    let x:Int
    let y:Int
    var maze:[[Int]]

    init(_ x:Int, _ y:Int) {
        self.x  = x
        self.y = y
        let column = [Int](repeating: 0, count: y)
        self.maze = [[Int]](repeating: column, count: x)
        generateMaze(0, 0)
    }

    private func generateMaze(_ cx:Int, _ cy:Int) {
        var directions = Direction.allDirections
        directions.shuffle()
        for direction in directions {
            let (dx, dy) = direction.diff
            let nx = cx + dx
            let ny = cy + dy
            if inBounds(nx, ny) && maze[nx][ny] == 0 {
                maze[cx][cy] |= direction.rawValue
                maze[nx][ny] |= direction.opposite.rawValue
                generateMaze(nx, ny)
            }
        }
    }

    private func inBounds(_ testX:Int, _ testY:Int) -> Bool {
        return inBounds(value:testX, upper:self.x) && inBounds(value:testY, upper:self.y)
    }

    private func inBounds(value:Int, upper:Int) -> Bool {
        return (value >= 0) && (value < upper)
    }

    func display() {
        let cellWidth = 3
        for j in 0..<y {
            // Draw top edge
            var topEdge = ""
            for i in 0..<x {
                topEdge += "+"
                topEdge += String(repeating: (maze[i][j] & Direction.north.rawValue) == 0 ? "-" : " ", count: cellWidth)
            }
            topEdge += "+"
            print(topEdge)

            // Draw left edge
            var leftEdge = ""
            for i in 0..<x {
                leftEdge += (maze[i][j] & Direction.west.rawValue) == 0 ? "|" : " "
                leftEdge += String(repeating: " ", count: cellWidth)
            }
            leftEdge += "|"
            print(leftEdge)
        }

        // Draw bottom edge
        var bottomEdge = ""
        for _ in 0..<x {
            bottomEdge += "+"
            bottomEdge += String(repeating: "-", count: cellWidth)
        }
        bottomEdge += "+"
        print(bottomEdge)
    }

    func displayInts() {
        for j in 0..<y {
            var line = ""
            for i in 0..<x {
                line += String(maze[i][j]) + "\t"
            }
            print(line)
        }
    }

    func displayDirections() {
        for j in 0..<y {
            var line = ""
            for i in 0..<x {
                line += getDirectionsAsString(maze[i][j]) + "\t"
            }
            print(line)
        }
    }

    private func getDirectionsAsString(_ value:Int) -> String {
        var line = ""
        for direction in Direction.allDirections {
            if (value & direction.rawValue) != 0 {
                line += direction.char
            }
        }
        return line
    }
}


let x = 20
let y = 10
let maze = MazeGenerator(x, y)
maze.display()
```

```txt

+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
|           |                                   |           |               |   |
+---+---+   +---+   +   +---+---+---+---+---+   +---+   +   +---+---+   +   +   +
|       |       |   |   |                   |           |               |       |
+   +---+---+   +   +   +   +---+   +---+   +---+---+---+---+---+---+---+---+   +
|       |       |   |   |   |   |       |               |                   |   |
+---+   +   +---+   +   +   +   +---+   +---+---+---+   +   +---+---+   +   +   +
|       |   |       |   |       |   |   |       |       |           |   |   |   |
+   +---+   +   +---+   +---+   +   +   +   +   +---+---+---+   +---+   +---+   +
|   |       |       |   |           |       |       |           |       |       |
+   +   +---+---+   +   +---+---+   +---+---+---+   +   +---+---+   +   +   +---+
|       |   |       |       |               |       |   |   |       |   |       |
+   +---+   +   +---+---+   +---+---+---+   +   +   +   +   +   +---+   +---+   +
|   |   |               |           |       |   |   |   |   |   |           |   |
+   +   +   +---+---+---+   +---+   +   +---+   +   +   +   +   +---+---+---+   +
|   |       |           |   |       |       |   |   |   |   |   |               |
+   +---+---+   +---+   +   +   +---+---+   +   +---+   +   +   +   +---+---+---+
|           |   |       |   |   |           |       |   |   |   |           |   |
+---+---+   +   +   +---+---+   +   +---+---+---+   +   +   +   +---+---+   +   +
|               |               |               |           |                   |
+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
```



## Tcl

```tcl
package require TclOO; # Or Tcl 8.6

# Helper to pick a random number
proc rand n {expr {int(rand() * $n)}}
# Helper to pick a random element of a list
proc pick list {lindex $list [rand [llength $list]]}
# Helper _function_ to index into a list of lists
proc tcl::mathfunc::idx {v x y} {lindex $v $x $y}

oo::class create maze {
    variable x y horiz verti content
    constructor {width height} {
	set y $width
	set x $height

	set n [expr {$x * $y - 1}]
	if {$n < 0} {error "illegal maze dimensions"}
	set horiz [set verti [lrepeat $x [lrepeat $y 0]]]
	# This matrix holds the output for the Maze Solving task; not used for generation
	set content [lrepeat $x [lrepeat $y " "]]
	set unvisited [lrepeat [expr {$x+2}] [lrepeat [expr {$y+2}] 0]]
	# Helper to write into a list of lists (with offsets)
	proc unvisited= {x y value} {
	    upvar 1 unvisited u
	    lset u [expr {$x+1}] [expr {$y+1}] $value
	}

	lappend stack [set here [list [rand $x] [rand $y]]]
	for {set j 0} {$j < $x} {incr j} {
	    for {set k 0} {$k < $y} {incr k} {
		unvisited= $j $k [expr {$here ne [list $j $k]}]
	    }
	}

	while {0 < $n} {
	    lassign $here hx hy
	    set neighbours {}
	    foreach {dx dy} {1 0  0 1  -1 0	 0 -1} {
		if {idx($unvisited, $hx+$dx+1, $hy+$dy+1)} {
		    lappend neighbours [list [expr {$hx+$dx}] [expr {$hy+$dy}]]
		}
	    }
	    if {[llength $neighbours]} {
		lassign [set here [pick $neighbours]] nx ny
		unvisited= $nx $ny 0
		if {$nx == $hx} {
		    lset horiz $nx [expr {min($ny, $hy)}] 1
		} else {
		    lset verti [expr {min($nx, $hx)}] $ny 1
		}
		lappend stack $here
		incr n -1
	    } else {
		set here [lindex $stack end]
		set stack [lrange $stack 0 end-1]
	    }
	}

	rename unvisited= {}
    }

    # Maze displayer; takes a maze dictionary, returns a string
    method view {} {
	set text {}
	for {set j 0} {$j < $x*2+1} {incr j} {
	    set line {}
	    for {set k 0} {$k < $y*4+1} {incr k} {
		if {$j%2 && $k%4==2} {
		    # At the centre of the cell, put the "content" of the cell
		    append line [expr {idx($content, $j/2, $k/4)}]
		} elseif {$j%2 && ($k%4 || $k && idx($horiz, $j/2, $k/4-1))} {
		    append line " "
		} elseif {$j%2} {
		    append line "|"
		} elseif {0 == $k%4} {
		    append line "+"
		} elseif {$j && idx($verti, $j/2-1, $k/4)} {
		    append line " "
		} else {
		    append line "-"
		}
	    }
	    if {!$j} {
		lappend text [string replace $line 1 3 "   "]
	    } elseif {$x*2-1 == $j} {
		lappend text [string replace $line end end " "]
	    } else {
		lappend text $line
	    }
	}
	return [join $text \n]
    }
}

# Demonstration
maze create m 11 8
puts [m view]
```

```txt

+   +---+---+---+---+---+---+---+---+---+---+
|                   |               |       |
+---+---+   +---+---+   +   +---+   +---+   +
|           |           |   |       |       |
+   +   +---+   +---+---+   +---+   +   +   +
|   |   |               |       |   |   |   |
+   +---+   +---+---+---+   +   +   +   +   +
|       |   |           |   |   |       |   |
+   +   +   +   +---+---+   +   +---+---+   +
|   |       |       |       |   |   |       |
+---+---+---+---+   +   +---+   +   +   +---+
|               |   |   |   |   |   |       |
+   +---+---+   +   +   +   +   +   +---+   +
|       |   |       |       |   |       |   |
+---+   +   +---+---+---+---+   +   +---+   +
|                               |
+---+---+---+---+---+---+---+---+---+---+---+

```



## TXR


===Simple, Depth-First===

Legend: cu = current location; vi = boolean hash of visited locations; pa = hash giving a list neighboring cells to which there is a path from a given cell.


```txr
@(bind (width height) (15 15))
@(do
   (defvar *r* (make-random-state nil))
   (defvar vi)
   (defvar pa)

   (defun neigh (loc)
     (let ((x (from loc))
           (y (to loc)))
       (list (- x 1)..y (+ x 1)..y
             x..(- y 1) x..(+ y 1))))

   (defun make-maze-rec (cu)
     (set [vi cu] t)
     (each ((ne (shuffle (neigh cu))))
       (cond ((not [vi ne])
              (push ne [pa cu])
              (push cu [pa ne])
              (make-maze-rec ne)))))

   (defun make-maze (w h)
     (let ((vi (hash :equal-based))
           (pa (hash :equal-based)))
       (each ((x (range -1 w)))
         (set [vi x..-1] t)
         (set [vi x..h] t))
       (each ((y (range* 0 h)))
         (set [vi -1..y] t)
         (set [vi w..y] t))
       (make-maze-rec 0..0)
       pa))

   (defun print-tops (pa w j)
     (each ((i (range* 0 w)))
       (if (memqual i..(- j 1) [pa i..j])
         (put-string "+    ")
         (put-string "+----")))
     (put-line "+"))

   (defun print-sides (pa w j)
     (let ((str ""))
       (each ((i (range* 0 w)))
         (if (memqual (- i 1)..j [pa i..j])
           (set str `@str     `)
           (set str `@str|    `)))
       (put-line `@str|\n@str|`)))

   (defun print-maze (pa w h)
     (each ((j (range* 0 h)))
       (print-tops pa w j)
       (print-sides pa w j))
     (print-tops pa w h)))
@;;
@(bind m @(make-maze width height))
@(do (print-maze m width height))
```


```txt
+----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+
|    |         |                        |                                  |
|    |         |                        |                                  |
+    +    +    +    +    +----+----+    +    +----+----+----+    +    +----+
|    |    |         |         |         |         |         |    |         |
|    |    |         |         |         |         |         |    |         |
+    +----+----+----+----+    +----+----+    +----+    +    +    +----+    +
|                   |         |              |         |    |    |         |
|                   |         |              |         |    |    |         |
+----+----+----+    +    +    +    +----+----+    +----+    +    +    +----+
|              |    |    |    |    |         |    |    |    |    |         |
|              |    |    |    |    |         |    |    |    |    |         |
+    +----+    +    +    +----+    +    +----+    +    +    +    +----+    +
|         |    |    |                   |         |    |    |         |    |
|         |    |    |                   |         |    |    |         |    |
+----+    +    +    +----+----+----+----+    +----+    +    +----+----+    +
|         |    |                   |         |         |              |    |
|         |    |                   |         |         |              |    |
+    +----+    +----+----+----+    +    +----+    +----+----+----+    +    +
|    |                        |         |                        |    |    |
|    |                        |         |                        |    |    |
+----+    +    +----+----+----+----+----+----+----+----+----+    +    +    +
|         |    |                                       |         |         |
|         |    |                                       |         |         |
+    +----+    +    +----+----+    +----+----+----+    +    +    +----+    +
|    |         |    |    |         |              |         |    |         |
|    |         |    |    |         |              |         |    |         |
+    +----+    +    +    +    +----+----+    +    +----+----+    +    +----+
|         |    |         |    |              |              |    |    |    |
|         |    |         |    |              |              |    |    |    |
+    +    +----+    +----+    +    +----+----+----+----+----+    +    +    +
|    |              |         |         |                   |    |         |
|    |              |         |         |                   |    |         |
+    +----+----+----+    +----+----+    +    +----+----+    +    +----+    +
|              |    |    |              |    |         |         |         |
|              |    |    |              |    |         |         |         |
+----+----+    +    +    +----+    +----+    +    +    +----+----+    +----+
|    |              |         |                   |              |    |    |
|    |              |         |                   |              |    |    |
+    +    +----+----+----+    +    +----+----+----+----+----+    +    +    +
|         |                   |              |              |    |         |
|         |                   |              |              |    |         |
+    +----+    +----+----+----+----+----+----+    +----+    +----+----+    +
|         |                                            |                   |
|         |                                            |                   |
+----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+
```


===Quality Breadth-First===

The following is a complete, self-contained command line utility. We also drop use of the TXR pattern extraction language and work purely in TXR Lisp.

The algorithm is quite different from the previous. This version is not recursive. This algorithm divides the maze cells into visited cells, frontier cells and unvisited cells.  As in the DFS version, border cells outside of the maze area are pre-initialized as visited, for convenience. The frontier set initially contains the upper left hand corner.

The algorithm's main loop iterates while there are frontier cells. As the generation progresses, unvisited cells adjacent to frontier cells added to the frontier set. Frontier cells that are only surrounded by other frontier cells or visited cells are removed from the frontier set and become visited cells.  Eventually, all unvisited cells become frontier cells and then visited cells, at which point the frontier set becomes empty and the algorithm terminates.

At every step, the algorithm picks the first cell in the frontier list. In the code, the frontier cells are kept in a hash called <code>fr</code> and also in a queue <code>q</code>.  The algorithm tries to extend the frontier around the frontier cell which is at the head of the queue <code>q</code> by randomly choosing an adjacent unvisited cell. (If there is no such cell, the node is not a frontier node any more and is popped from the queue and <code>fr</code> set).  If an unvisited node is picked, then a two-way path is broken from the given frontier cell to that cell, and that cell is added to the frontier set. '''Important:''' the new frontier cell is added to the head of the queue, rather than the tail.

The algorithm is modified by a "straightness" parameter, which is used to initialize a counter. Every time a new frontier node is added to the front of the queue, the counter decrements. When it reaches zero, the frontier queue is scrambled, and the counter is reset.  As long as the count is nonzero, the maze growth proceeds from the previously traversed node, because the new node is placed at the head of the queue. This behavior mimics the DFS algorithm, resulting in long corridors without a lot of branching.

At the user interface level, the straightness parameter is represented as a percentage.  This percentage is converted to a number of cells based on the width and height of the maze. For instance if the straightness parameter is 15, and the maze size is 20x20, it means that 15% out of 400 cells, or 60 cells will be traversed before the queue is scrambled. Then another 60 will be traversed and the queue will be scrambled, and so forth.


```txrlisp
(defvar vi)  ;; visited hash
(defvar pa)  ;; path connectivity hash
(defvar sc)  ;; count, derived from straightness fator

(defun rnd-pick (list)
  (if list [list (rand (length list))]))

(defun neigh (loc)
  (let ((x (from loc))
        (y (to loc)))
    (list (- x 1)..y (+ x 1)..y
          x..(- y 1) x..(+ y 1))))

(defun make-maze-impl (cu)
  (let ((q (list cu))
        (c sc))
    (set [vi cu] t)
    (while q
      (let* ((cu (first q))
             (ne (rnd-pick (remove-if vi (neigh cu)))))
        (cond (ne (set [vi ne] t)
                  (push ne [pa cu])
                  (push cu [pa ne])
                  (push ne q)
                  (cond ((<= (dec c) 0)
                         (set q (shuffle q))
                         (set c sc))))
              (t (pop q)))))))

(defun make-maze (w h sf)
  (let ((vi (hash :equal-based))
        (pa (hash :equal-based))
        (sc (max 1 (trunc (* sf w h) 100))))
    (each ((x (range -1 w)))
      (set [vi x..-1] t)
      (set [vi x..h] t))
    (each ((y (range* 0 h)))
      (set [vi -1..y] t)
      (set [vi w..y] t))
    (make-maze-impl 0..0)
    pa))

(defun print-tops (pa w j)
  (each ((i (range* 0 w)))
    (if (memqual i..(- j 1) [pa i..j])
      (put-string "+    ")
      (put-string "+----")))
  (put-line "+"))

(defun print-sides (pa w j)
  (let ((str ""))
    (each ((i (range* 0 w)))
      (if (memqual (- i 1)..j [pa i..j])
        (set str `@str     `)
        (set str `@str|    `)))
    (put-line `@str|\n@str|`)))

(defun print-maze (pa w h)
  (each ((j (range* 0 h)))
    (print-tops pa w j)
    (print-sides pa w j))
  (print-tops pa w h))

(defun usage ()
  (let ((invocation (ldiff *full-args* *args*)))
    (put-line "usage: ")
    (put-line `@invocation <width> <height> [<straightness>]`)
    (put-line "straightness-factor is a percentage, defaulting to 15")
    (exit 1)))

(let ((args [mapcar int-str *args*])
      (*random-state* (make-random-state nil)))
  (if (memq nil args)
    (usage))
  (tree-case args
    ((w h s ju . nk) (usage))
    ((w h : (s 15)) (set w (max 1 w))
                    (set h (max 1 h))
                    (print-maze (make-maze w h s) w h))
    (else (usage))))
```


Three mazes are generated, at the lowest,
intermediate and highest "straightness factors".

It is immediately obvious that the style of each maze
is quite different.


```txt

# 10x10 maze with zero percent "straightness factor"
$ txr maze-generation3.txr 10 10 0
+----+----+----+----+----+----+----+----+----+----+
|                   |    |                        |
|                   |    |                        |
+    +    +----+----+    +    +    +----+----+----+
|    |         |              |         |         |
|    |         |              |         |         |
+    +    +----+    +----+----+----+----+    +    +
|    |    |                   |              |    |
|    |    |                   |              |    |
+    +----+    +----+    +----+    +----+----+----+
|                   |                             |
|                   |                             |
+----+    +    +    +    +    +    +----+----+----+
|         |    |    |    |    |                   |
|         |    |    |    |    |                   |
+----+    +    +----+----+    +----+----+----+    +
|         |              |                   |    |
|         |              |                   |    |
+    +----+    +----+----+    +    +    +----+    +
|    |                   |    |    |         |    |
|    |                   |    |    |         |    |
+    +----+    +    +    +    +    +    +    +    +
|    |         |    |    |    |    |    |    |    |
|    |         |    |    |    |    |    |    |    |
+----+    +    +----+    +    +    +----+----+    +
|         |         |    |    |         |         |
|         |         |    |    |         |         |
+    +    +    +    +----+----+----+----+----+    +
|    |    |    |                        |         |
|    |    |    |                        |         |
+----+----+----+----+----+----+----+----+----+----+


# with 10% straightnes factor
$ txr maze-generation3.txr 10 10 10
+----+----+----+----+----+----+----+----+----+----+
|    |              |         |         |         |
|    |              |         |         |         |
+    +    +----+    +    +    +    +    +----+    +
|              |         |         |              |
|              |         |         |              |
+    +----+----+    +----+----+----+----+----+----+
|    |         |         |                        |
|    |         |         |                        |
+----+    +    +----+    +    +----+----+    +    +
|         |              |         |    |    |    |
|         |              |         |    |    |    |
+    +----+----+    +----+    +    +    +    +----+
|    |                   |    |         |    |    |
|    |                   |    |         |    |    |
+    +    +----+----+----+----+----+----+    +    +
|    |                   |                        |
|    |                   |                        |
+    +    +----+    +    +    +    +----+----+----+
|    |    |         |    |    |    |         |    |
|    |    |         |    |    |    |         |    |
+    +----+    +----+    +----+    +    +    +    +
|    |         |                   |    |         |
|    |         |                   |    |         |
+    +    +----+----+    +----+    +    +----+----+
|    |         |         |         |         |    |
|    |         |         |         |         |    |
+----+----+    +----+    +    +----+----+    +    +
|                   |    |                        |
|                   |    |                        |
+----+----+----+----+----+----+----+----+----+----+

# with 100 percent straight factor
$ txr maze-generation3.txr 10 10 100
+----+----+----+----+----+----+----+----+----+----+
|         |                             |         |
|         |                             |         |
+----+    +----+    +----+----+    +    +    +    +
|    |         |              |    |    |    |    |
|    |         |              |    |    |    |    |
+    +----+    +----+----+----+    +    +    +    +
|         |    |         |         |    |    |    |
|         |    |         |         |    |    |    |
+    +----+    +    +    +    +----+    +----+    +
|    |         |    |    |         |              |
|    |         |    |    |         |              |
+    +    +----+    +    +    +    +----+----+    +
|    |    |         |    |    |         |         |
|    |    |         |    |    |         |         |
+    +    +----+    +    +----+    +    +----+----+
|    |              |         |    |              |
|    |              |         |    |              |
+    +----+----+----+----+    +----+----+----+    +
|              |         |              |         |
|              |         |              |         |
+    +----+----+    +    +----+----+    +    +    +
|         |         |         |    |         |    |
|         |         |         |    |         |    |
+    +    +    +----+    +    +    +----+----+    +
|    |         |         |                   |    |
|    |         |         |                   |    |
+    +----+----+    +----+----+----+----+----+    +
|              |                                  |
|              |                                  |
+----+----+----+----+----+----+----+----+----+----+
```



## XPL0


```XPL0
code Ran=1, CrLf=9, Text=12;            \intrinsic routines
def  Cols=20, Rows=6;                   \dimensions of maze (cells)
int  Cell(Cols+1, Rows+1, 3);           \cells (plus right and bottom borders)
def  LeftWall, Ceiling, Connected;      \attributes of each cell (= 0, 1 and 2)

proc ConnectFrom(X, Y);         \Connect cells starting from cell X,Y
int  X, Y;
int  Dir, Dir0;
[Cell(X, Y, Connected):= true;  \mark current cell as connected
Dir:= Ran(4);                   \randomly choose a direction
Dir0:= Dir;                     \save this initial direction
repeat  case Dir of             \try to connect to cell at Dir
          0: if X+1<Cols & not Cell(X+1, Y, Connected) then     \go right
                [Cell(X+1, Y, LeftWall):= false; ConnectFrom(X+1, Y)];
          1: if Y+1<Rows & not Cell(X, Y+1, Connected) then     \go down
                [Cell(X, Y+1, Ceiling):= false;  ConnectFrom(X, Y+1)];
          2: if X-1>=0 & not Cell(X-1, Y, Connected) then       \go left
                [Cell(X, Y, LeftWall):= false;   ConnectFrom(X-1, Y)];
          3: if Y-1>=0 & not Cell(X, Y-1, Connected) then       \go up
                [Cell(X, Y, Ceiling):= false;    ConnectFrom(X, Y-1)]
        other   [];             \(never occurs)
        Dir:= Dir+1 & $03;      \next direction
until   Dir = Dir0;
];

int  X, Y;
[for Y:= 0 to Rows do
    for X:= 0 to Cols do
        [Cell(X, Y, LeftWall):= true;           \start with all walls and
         Cell(X, Y, Ceiling):= true;            \ ceilings in place
         Cell(X, Y, Connected):= false;         \ and all cells disconnected
        ];
Cell(0, 0, LeftWall):= false;                   \make left and right doorways
Cell(Cols, Rows-1, LeftWall):= false;
ConnectFrom(Ran(Cols), Ran(Rows));              \randomly pick a starting cell
for Y:= 0 to Rows do                            \display the maze
        [CrLf(0);
        for X:= 0 to Cols do
                Text(0, if X#Cols & Cell(X, Y, Ceiling)  then "+--" else "+  ");
        CrLf(0);
        for X:= 0 to Cols do
                Text(0, if Y#Rows & Cell(X, Y, LeftWall) then "|  " else "   ");
        ];
]
```


Output:

```txt

+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
         |                 |        |                       |
+  +--+  +--+  +--+--+--+  +  +--+  +  +--+--+--+--+--+--+  +
|  |  |  |     |  |        |  |     |     |           |     |
+  +  +  +  +--+  +  +--+--+  +  +--+--+  +--+--+  +--+  +--+
|  |     |  |     |  |        |           |     |     |     |
+  +--+--+  +  +--+  +  +--+--+--+--+--+--+  +  +--+  +--+  +
|     |     |     |  |  |           |        |           |  |
+--+  +  +--+--+  +  +  +  +--+--+  +  +--+--+--+--+--+--+  +
|  |  |  |        |  |  |  |     |     |     |     |        |
+  +  +  +  +--+--+  +  +  +  +  +--+--+  +  +  +  +  +--+--+
|        |              |     |           |     |
+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+


```



## zkl

Generate a maze without marked entry or exit points.
```zkl

fcn make_maze(w = 16, h = 8){
   // make arrays with lists of lists (all mutable)
   vis:=(w.pump(List().write,0)+1)*h + w.pump(List().write,1);
   ver:=(w.pump(List().write,T(Void,"|   ")) + "|")*h + T;
   hor:=(w.pump(List().write,T(Void,"+---")) + "+")*(h + 1);

   fcn(x,y,vis,ver,hor){
      vis[y][x] = 1;

      d:=L(T(x - 1, y), T(x, y + 1), T(x + 1, y), T(x, y - 1)).shuffle();
      foreach xx,yy in (d){
	 if(vis[yy][xx]) continue;
	 if(xx==x) hor[y.max(yy)][x]="+   ";
	 if(yy==y) ver[y][x.max(xx)]="    ";
	 self.fcn(xx,yy,vis,ver,hor);
      }
   }((0).random(w),(0).random(h),vis,ver,hor);
   foreach a,b in (hor.zip(ver)) { println(a.concat(),"\n",b.concat()) }
   return(ver,hor);
}
make_maze();
```

```txt

+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
|           |               |       |                       |   |
+---+---+   +   +   +---+   +   +   +   +   +---+---+---+   +   +
|           |   |   |       |   |       |   |           |   |   |
+   +---+   +   +   +---+---+   +---+---+   +   +---+   +   +   +
|   |       |   |   |       |       |       |   |       |   |   |
+   +   +---+   +   +   +   +   +---+   +---+   +   +---+   +   +
|   |       |   |       |       |       |       |   |       |   |
+   +   +---+   +---+---+---+---+   +---+   +---+   +   +---+   +
|   |   |       |           |       |       |   |       |       |
+   +---+   +---+   +   +   +   +---+   +---+   +---+---+   +---+
|           |       |   |   |   |   |   |                   |   |
+   +---+---+---+---+   +   +   +   +   +   +---+---+---+---+   +
|   |       |       |   |   |   |           |               |   |
+   +   +   +   +   +   +   +   +---+---+---+   +---+---+   +   +
|       |       |       |   |                           |       |
+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

```

