+++
title = "Flipping bits game"
description = ""
date = 2019-10-10T06:53:36Z
aliases = []
[extra]
id = 15108
[taxonomies]
categories = ["task"]
tags = []
+++

;The game:
Given an N by N square array of zeroes or ones in an initial
configuration, and a target configuration of zeroes and ones
The task is to transform one to the other in as few moves as 
possible by inverting whole numbered rows or whole lettered 
columns at once, as one move.

In an inversion any 1 becomes 0, and any 0 becomes 1 for that
whole row or column.

## Task

Create a program to score for the Flipping bits game.
# The game should create an original random target configuration and a starting configuration.
# Ensure that the starting position is ''never'' the target position.
# The target position must be guaranteed as reachable from the starting position. (One possible way to do this is to generate the start position by legal flips from a random target position. The flips will always be reversible back to the target from the given start position).
# The number of moves taken so far should be shown.



Show an example of a short game here, on this page, for a 3 by 3 array of bits.





## Ada

This solution determines the size of the playground from the command line.


```Ada
with Ada.Text_IO, Ada.Command_Line, Ada.Numerics.Discrete_Random;

procedure Flip_Bits is
   
   subtype Letter is Character range 'a' .. 'z';
   
   Last_Col: constant letter := Ada.Command_Line.Argument(1)(1);
   Last_Row: constant Positive := Positive'Value(Ada.Command_Line.Argument(2));
   
   package Boolean_Rand is new Ada.Numerics.Discrete_Random(Boolean);
   Gen: Boolean_Rand.Generator;
   
   type Matrix is array
     (Letter range 'a' .. Last_Col, Positive range 1 .. Last_Row) of Boolean;
   
   function Rand_Mat return Matrix is
      M: Matrix;
   begin
      for I in M'Range(1) loop
	 for J in M'Range(2) loop
	    M(I,J) := Boolean_Rand.Random(Gen);
	 end loop;
      end loop;
      return M;
   end Rand_Mat;
   
   function Rand_Mat(Start: Matrix) return Matrix is
      M: Matrix := Start;
   begin
      for I in M'Range(1) loop
	 if  Boolean_Rand.Random(Gen) then
	    for J in M'Range(2) loop
	       M(I,J) := not M(I, J);
	    end loop;
	 end if;
      end loop;
      for I in M'Range(2) loop
	 if  Boolean_Rand.Random(Gen) then
	    for J in M'Range(1) loop
	       M(J,I) := not M(J, I);
	    end loop;
	 end if;
      end loop;
      return M;
   end Rand_Mat;
   
   procedure Print(Message: String; Mat: Matrix) is
      package NIO is new Ada.Text_IO.Integer_IO(Natural);
   begin
      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put_Line(Message);
      Ada.Text_IO.Put("   ");
      for Ch in Matrix'Range(1) loop
	 Ada.Text_IO.Put(" " & Ch);
      end loop;
      Ada.Text_IO.New_Line;
      for I in Matrix'Range(2) loop
	 NIO.Put(I, Width => 3);
	 for Ch in Matrix'Range(1) loop
	    Ada.Text_IO.Put(if Mat(Ch, I) then " 1" else " 0");
	 end loop;
         Ada.Text_IO.New_Line;
      end loop;
   end Print;

   Current, Target: Matrix;
   Moves: Natural := 0;
       
begin
   -- choose random Target and start ("Current") matrices
   Boolean_Rand.Reset(Gen);
   Target := Rand_Mat;
   loop
      Current := Rand_Mat(Target);
      exit when Current /= Target;
   end loop;
   Print("Target:", Target);
   
   -- print and modify Current matrix, until it is identical to Target
   while Current /= Target loop
     Moves := Moves + 1;
     Print("Current move #" & Natural'Image(Moves), Current);
      Ada.Text_IO.Put_Line("Flip row 1 .." & Positive'Image(Last_Row) &
			     " or column 'a' .. '" & Last_Col & "'");
      declare
	 S: String := Ada.Text_IO.Get_Line;
	 function Let(S: String) return Character is (S(S'First));
	 function Val(Str: String) return Positive is (Positive'Value(Str));
      begin
	 if Let(S) in 'a' .. Last_Col then
	    for I in Current'Range(2) loop
	       Current(Let(S), I) := not Current(Let(S), I);
	    end loop;
	 else 
	    for I in Current'Range(1) loop
	       Current(I, Val(S)) := not Current(I, Val(S));
	    end loop;
	 end if;
      end;
   end loop;
   
   -- summarize the outcome
   Ada.Text_IO.Put_Line("Done after" & Natural'Image(Moves) & " Moves.");
end Flip_Bits;
```


For a 3*3-Matrix, the command line input is "c 3".


```txt
>./flip_bits c 3

Target:
    a b c
  1 1 0 1
  2 1 0 0
  3 1 0 0

Current move # 1
    a b c
  1 1 0 0
  2 1 0 1
  3 0 1 0
Flip row 1 .. 3 or column 'a' .. 'c'
3

Current move # 2
    a b c
  1 1 0 0
  2 1 0 1
  3 1 0 1
Flip row 1 .. 3 or column 'a' .. 'c'
c
Done after 2 Moves.
```



## AutoHotkey

Uploads are currently disabled, so since a GUI is used, I can't show an example.

```ahk
size := 3 ; max 26
Gui, Add, Button, , O
Loop, %size%
{
	x := chr(A_Index+64)
	If x = A
		Loop, %size%
			Gui, Add, Button, y+4 gFlip, % A_Index
	Gui, Add, Button, ym gFlip, % x
	Loop, %size%
	{
		y := A_Index
		Random, %x%%y%, 0, 1
		Gui, Add, Edit, v%x%%y% ReadOnly, % %x%%y%
	}
}
Gui, Add, Text, ym, Moves:`nTarget:
Loop, %size%
{
	x := chr(A_Index+64)
	Loop, %size%
	{
		y := A_Index
		Gui, Add, Edit, % y=1 ? x="A" ? "xp+0 ym+30" : "x+14 ym+30" : "" . "ReadOnly vt" x y, % t%x%%y% := %x%%y%
	}
}Gui, Add, Text, xp-18 ym w30 Right vMoves, % Moves:=1

; randomize
While (i < size)
{
	Random, z, 1, %size%
	Random, x, 0, 1
	z := x ? chr(z+64) : z
	Solution .= z ; to cheat
	If Flip(z, size)
		i := 0 ; ensure we are not at the solution
	Else
		i++ ; count
}
Gui, Show, NA
Return

Flip(z, size) {
	Loop, %size%
	{
		If z is alpha
			GuiControl, , %z%%A_Index%, % %z%%A_Index% := !%z%%A_Index%
		Else
		{
			AIndex := chr(A_Index+64)
			GuiControl, , %AIndex%%z%, % %AIndex%%z% := !%AIndex%%z%
		}
	}
	Loop, %size%
	{
		x := chr(A_Index+64)
		Loop, %size%
		{
			y := A_Index
			If (%x%%y% != t%x%%y%)
				Return 0
		}
	}
	Return 1
}

Flip:
	GuiControl, , Moves, % Moves++
	If Flip(A_GuiControl, size)
	{
		Msgbox Success in %Moves% moves!
		Reload
	}
Return

ButtonO:
	Reload
Return

GuiEscape:
GuiClose:
	ExitApp
Return
```



## C


```c

#include <stdio.h>
#include <stdlib.h>

int i, j;

void fliprow(int **b, int sz, int n)
{
	for(i = 0; i < sz; i++)
		b[n+1][i] = !b[n+1][i];
}

void flipcol(int **b, int sz, int n)
{
	for(i = 1; i <= sz; i++)
		b[i][n] = !b[i][n];
}

void initt(int **t, int sz)
{
	for(i = 1; i <= sz; i++)
		for(j = 0; j < sz; j++)
			t[i][j] = rand()%2;
}

void initb(int **t, int **b, int sz)
{
	for(i = 1; i <= sz; i++)
		for(j = 0; j < sz; j++)
			b[i][j] = t[i][j];
	
	for(i = 1; i <= sz; i++)
		fliprow(b, sz, rand()%sz+1);
	for(i = 0; i < sz; i++)
		flipcol(b, sz, rand()%sz);
}

void printb(int **b, int sz)
{
	printf(" ");
	for(i = 0; i < sz; i++)
		printf(" %d", i);
	printf("\n");

	for(i = 1; i <= sz; i++)
	{
		printf("%d", i-1);
		for(j = 0; j < sz; j++)
			printf(" %d", b[i][j]);
		printf("\n");
	}
	
	printf("\n");
}

int eq(int **t, int **b, int sz)
{
	for(i = 1; i <= sz; i++)
		for(j = 0; j < sz; j++)
			if(b[i][j] != t[i][j])
				return 0;
	return 1;
}

void main()
{
	int sz = 3;
	int eql = 0;
	int mov = 0;
	int **t = malloc(sz*(sizeof(int)+1));
	for(i = 1; i <= sz; i++)
		t[i] = malloc(sz*sizeof(int));

	int **b = malloc(sz*(sizeof(int)+1));
	for(i = 1; i <= sz; i++)
		b[i] = malloc(sz*sizeof(int));
	char roc;
	int n;
	initt(t, sz);
	initb(t, b, sz);
	
	while(eq(t, b, sz))
		initb(t, b, sz);
	
	while(!eql)
	{
		printf("Target: \n");
		printb(t, sz);
		printf("Board: \n");
		printb(b, sz);
		printf("What to flip: ");
		scanf(" %c", &roc);
		scanf(" %d", &n);

		switch(roc)
		{
			case 'r':
				fliprow(b, sz, n);
				break;
			case 'c':
				flipcol(b, sz, n);
				break;
			default:
				perror("Please specify r or c and an number");
				break;
		}

		printf("Moves Taken: %d\n", ++mov);

		if(eq(t, b, sz))
		{
			printf("You win!\n");
			eql = 1;
		}
	}
}

```

```txt

Target: 
  0 1 2
0 1 0 1
1 1 1 1
2 0 0 1

Board: 
  0 1 2
0 1 0 0
1 1 1 0
2 1 1 1

What to flip: r2
Moves Taken: 1
Target: 
  0 1 2
0 1 0 1
1 1 1 1
2 0 0 1

Board: 
  0 1 2
0 1 0 0
1 1 1 0
2 0 0 0

What to flip: c2
Moves Taken: 2
You win!

```



## C++


```cpp

#include <time.h>
#include <iostream>
#include <string>

typedef unsigned char byte;
using namespace std;

class flip
{
public:
    flip() { field = 0; target = 0; }
    void play( int w, int h ) { wid = w; hei = h; createField(); gameLoop(); }

private:
    void gameLoop()
    {
	int moves = 0;
	while( !solved() )
	{
	    display(); string r; cout << "Enter rows letters and/or column numbers: "; cin >> r;
	    for( string::iterator i = r.begin(); i != r.end(); i++ )
	    {
		byte ii = ( *i );
		if( ii - 1 >= '0' && ii - 1 <= '9' ) { flipCol( ii - '1' ); moves++; }
		else if( ii >= 'a' && ii <= 'z' ) { flipRow( ii - 'a' ); moves++; }
	    }
	}
	cout << endl << endl << "** Well done! **" << endl << "Used " << moves << " moves." << endl << endl;
    }

    void display()
    { system( "cls" ); output( "TARGET:", target ); output( "YOU:", field ); }

    void output( string t, byte* f )
    {
	cout << t << endl;
	cout << " "; for( int x = 0; x < wid; x++ ) cout << " " << static_cast<char>( x + '1' ); cout << endl;
	for( int y = 0; y < hei; y++ )
	{
	    cout << static_cast<char>( y + 'a' ) << " ";
	    for( int x = 0; x < wid; x++ )
		cout << static_cast<char>( f[x + y * wid] + 48 ) << " ";
	    cout << endl;
	}
	cout << endl << endl;
    }

    bool solved()
    {
	for( int y = 0; y < hei; y++ )
	    for( int x = 0; x < wid; x++ )
		if( target[x + y * wid] != field[x + y * wid] ) return false;
	return true;
    }

    void createTarget()
    {
	for( int y = 0; y < hei; y++ )
	    for( int x = 0; x < wid; x++ )
		if( frnd() < .5f ) target[x + y * wid] = 1;
	        else target[x + y * wid] = 0;
	memcpy( field, target, wid * hei );
    }

    void flipCol( int c )
    { for( int x = 0; x < hei; x++ ) field[c + x * wid] = !field[c + x * wid]; }
	
    void flipRow( int r )
    { for( int x = 0; x < wid; x++ ) field[x + r * wid] = !field[x + r * wid]; }

    void calcStartPos()
    {
	int flips = ( rand() % wid + wid + rand() % hei + hei ) >> 1;
	for( int x = 0; x < flips; x++ )
	{ if( frnd() < .5f ) flipCol( rand() % wid ); else flipRow( rand() % hei ); }
    }

    void createField()
    {
        if( field ){ delete [] field; delete [] target; }
        int t = wid * hei; field = new byte[t]; target = new byte[t];
	memset( field, 0, t ); memset( target, 0, t ); createTarget();
	while( true ) { calcStartPos(); if( !solved() ) break; }
    }

    float frnd() { return static_cast<float>( rand() ) / static_cast<float>( RAND_MAX ); }

    byte* field, *target; int wid, hei;
};

int main( int argc, char* argv[] )
{ srand( time( NULL ) ); flip g; g.play( 3, 3 ); return system( "pause" ); }

```

```txt

TARGET:
  1 2 3
a 0 1 0 
b 0 1 0 
c 0 1 0 

YOU:
  1 2 3
a 0 0 0 
b 0 0 0 
c 1 1 1 

Enter rows letters and/or column numbers: 2c

** Well done! **
Used 2 moves.

```



## Clojure


```clojure
(defn cols [board]
  (mapv vec (apply map list board)))

(defn flipv [v]
  (mapv #(if (> % 0) 0 1) v))

(defn flip-row [board n]
  (assoc board n (flipv (get board n))))

(defn flip-col [board n]
  (cols (flip-row (cols board) n)))

(defn play-rand [board n]
  (if (= n 0)
    board
    (let [f (if (= (rand-int 2) 0) flip-row flip-col)]
      (recur (f board (rand-int (count board))) (dec n)))))

(defn rand-binary-vec [size]
  (vec (take size (repeatedly #(rand-int 2)))))

(defn rand-binary-board [size]
  (vec (take size (repeatedly #(rand-binary-vec size)))))

(defn numbers->letters [coll]
  (map #(char (+ 97 %)) coll))

(defn column-labels [size]
  (apply str (interpose " " (numbers->letters (range size)))))

(defn print-board [board]
  (let [size (count board)]
    (println "\t " (column-labels size))
    (dotimes [n size] (println (inc n) "\t" (board n)))))

(defn key->move [key]
  (let [start (int (first key))
        row-value (try (Long/valueOf key) (catch NumberFormatException e))]
    (cond
      (<= 97 start 122) [:col (- start 97)]
      (<= 65 start 90) [:col (- start 65)]
      (> row-value 0) [:row (dec row-value)]
      :else nil)))

(defn play-game [target-board current-board n]
  (println "\nTurn " n)
  (print-board current-board)
  (if (= target-board current-board)
    (println "You win!")
    (let [move (key->move (read-line))
          axis (first move)
          idx (second move)]
      (cond
        (= axis :row) (play-game target-board (flip-row current-board idx) (inc n))
        (= axis :col) (play-game target-board (flip-col current-board idx) (inc n))
        :else (println "Quitting!")))))

(defn -main
  "Flip the Bits Game!"
  [& args]
  (if-not (empty? args)
    (let [target-board (rand-binary-board (Long/valueOf (first args)))]
      (println "Target")
      (print-board target-board)
      (play-game target-board (play-rand target-board 3) 0))))
```


```txt

Target
          a b c
1        [1 0 1]
2        [0 1 1]
3        [0 1 1]

Turn  0
          a b c
1        [1 0 1]
2        [0 1 1]
3        [1 0 0]
3

Turn  1
          a b c
1        [1 0 1]
2        [0 1 1]
3        [0 1 1]
You win!

```



## D

```d
import std.stdio, std.random, std.ascii, std.string, std.range,
       std.algorithm, std.conv;

enum N = 3; // Board side.
static assert(N <= lowercase.length);
enum columnIDs = lowercase[0 .. N];
alias Board = ubyte[N][N];

void flipBits(ref Board board, in size_t count=1) {
    foreach (immutable _; 0 .. count)
        board[uniform(0, $)][uniform(0, $)] ^= 1;
}

void notRow(ref Board board, in size_t i) pure nothrow {
    board[i][] ^= 1;
}

void notColumn(ref Board board, in size_t i) pure nothrow {
    foreach (ref row; board)
        row[i] ^= 1;
}

Board generateGameBoard(in ref Board target) {
    // board is generated with many flips, to keep parity unchanged.
    Board board = target;
    while (board == target)
        foreach (immutable _; 0 .. 2 * N)
            [&notRow, &notColumn][uniform(0, 2)](board, uniform(0, N));
    return board;
}

void show(in ref Board board, in string comment) {
    comment.writeln;
    writefln("     %-(%c %)", columnIDs);
    foreach (immutable j, const row; board)
        writefln("  %2d %-(%d %)", j + 1, row);
}

void main() {
    "T prints the target, and Q exits.\n".writeln;
    // Create target and flip some of its bits randomly.
    Board target;
    flipBits(target, uniform(0, N) + 1);
    show(target, "Target configuration is:");

    auto board = generateGameBoard(target);
    immutable prompt = format("  1-%d / %s-%s to flip, or T, Q: ",
                              N, columnIDs[0], columnIDs.back);
    uint move = 1;
    while (board != target) {
        show(board, format("\nMove %d:", move));
        prompt.write;
        immutable ans = readln.strip;

        if (ans.length == 1 && columnIDs.canFind(ans)) {
            board.notColumn(columnIDs.countUntil(ans));
            move++;
        } else if (iota(1, N + 1).map!text.canFind(ans)) {
            board.notRow(ans.to!uint - 1);
            move++;
        } else if (ans == "T") {
            show(target, "Target configuration is:");
        } else if (ans == "Q") {
            return "Game stopped.".writeln;
        } else
            writefln("  Wrong input '%s'. Try again.\n", ans.take(9));
    }

    "\nWell done!".writeln;
}
```

```txt
T prints the target, and Q exits.

Target configuration is:
     a b c
   1 1 1 1
   2 0 0 0
   3 0 0 0

Move 1:
     a b c
   1 1 0 0
   2 1 0 0
   3 1 0 0
  1-3 / a-c to flip, or T, Q: a

Move 2:
     a b c
   1 0 0 0
   2 0 0 0
   3 0 0 0
  1-3 / a-c to flip, or T, Q: 1

Well done!
```



## Elixir

```elixir
defmodule Flip_game do
  @az  Enum.map(?a..?z, &List.to_string([&1]))
  @in2i Enum.concat(Enum.map(1..26, fn i -> {to_string(i), i} end),
                    Enum.with_index(@az) |> Enum.map(fn {c,i} -> {c,-i-1} end))
        |> Enum.into(Map.new)
  
  def play(n) when n>2 do
    target = generate_target(n)
    display(n, "Target: ", target)
    board = starting_config(n, target)
    play(n, target, board, 1)
  end
  
  def play(n, target, board, moves) do
    display(n, "Board: ", board)
    ans = IO.gets("row/column to flip: ") |> String.strip |> String.downcase
    new_board = case @in2i[ans] do
                  i when i in 1..n   -> flip_row(n, board, i)
                  i when i in -1..-n -> flip_column(n, board, -i)
                  _ -> IO.puts "invalid input: #{ans}"
                       board
                end
    if target == new_board do
      display(n, "Board: ", new_board)
      IO.puts "You solved the game in #{moves} moves"
    else
      IO.puts ""
      play(n, target, new_board, moves+1)
    end
  end
  
  defp generate_target(n) do
    for i <- 1..n, j <- 1..n, into: Map.new, do: {{i, j}, :rand.uniform(2)-1}
  end
  
  defp starting_config(n, target) do
    Enum.concat(1..n, -1..-n)
    |> Enum.take_random(n)
    |> Enum.reduce(target, fn x,acc ->
         if x>0, do: flip_row(n, acc, x),
               else: flip_column(n, acc, -x)
       end)
  end
  
  defp flip_row(n, board, row) do
    Enum.reduce(1..n, board, fn col,acc ->
      Map.update!(acc, {row,col}, fn bit -> 1 - bit end)
    end)
  end
  
  defp flip_column(n, board, col) do
    Enum.reduce(1..n, board, fn row,acc ->
      Map.update!(acc, {row,col}, fn bit -> 1 - bit end)
    end)
  end
  
  defp display(n, title, board) do
    IO.puts title
    IO.puts "   #{Enum.join(Enum.take(@az,n), " ")}"
    Enum.each(1..n, fn row ->
      :io.fwrite "~2w ", [row]
      IO.puts Enum.map_join(1..n, " ", fn col -> board[{row, col}] end)
    end)
  end
end

Flip_game.play(3)
```


```txt

Target:
   a b c
 1 1 0 1
 2 0 1 1
 3 0 1 1
Board:
   a b c
 1 0 1 1
 2 0 1 0
 3 1 0 1
row/column to flip: 2

Board:
   a b c
 1 0 1 1
 2 1 0 1
 3 1 0 1
row/column to flip: a

Board:
   a b c
 1 1 1 1
 2 0 0 1
 3 0 0 1
row/column to flip: b
Board:
   a b c
 1 1 0 1
 2 0 1 1
 3 0 1 1
You solved the game in 3 moves

```



## Fortran

This version uses some routines (like rand(), srand() and date_and_time()) from the GNU Fortran compiler. Formats are used to print data on the screen in an appropriate manner. The number of rows (or columns) is a variable and the current implementation allows for any number between 1 and 10. Incorrect inputs are also verified.


```Fortran

!Implemented by Anant Dixit (October 2014)
program flipping_bits
implicit none
character(len=*), parameter :: cfmt = "(A3)", ifmt = "(I3)"
integer :: N, i, j, io, seed(8), moves, input
logical, allocatable :: Brd(:,:), Trgt(:,:)
logical :: solved
double precision :: r

do
  write(*,*) 'Enter the number of squares (between 1 and 10) you would like: '
  read(*,*,iostat=io) N
  if(N.gt.0 .and. N.le.10 .and. io.eq.0) exit
  write(*,*) 'Please, an integer between 1 and 10'
end do

allocate(Brd(N,N),Trgt(N,N))
call date_and_time(values=seed)
call srand(1000*seed(7)+seed(8)+60000*seed(6))
do i = 1,N
  do j = 1,N
    r = rand()
    if(r.gt.0.5D0) then
      Brd(i,j) = .TRUE.
      Trgt(i,j) = .TRUE.
    else
      Brd(i,j) = .FALSE.
      Trgt(i,j) = .FALSE.
    end if
  end do
end do
! Random moves taken by the program to `create' a target
moves = N
do i = 1,moves
  r = 1+2.0D0*dble(N)*rand() - 1.0D-17 !Only to make sure that the number is between 1 and 2N (less than 2N-1)
  if(floor(r).le.N) then
    do j = 1,N
      Trgt(floor(r),j) = .NOT.Trgt(floor(r),j)
    end do
  else
    r = r-N
    do j = 1,N
      Trgt(j,floor(r)) = .NOT.Trgt(j,floor(r))
    end do
  end if
end do

!This part checks if the target and the starting configurations are same or not.
do
  input = N
  call next_move(Brd,Trgt,N,input,solved)
  call next_move(Brd,Trgt,N,input,solved)
  if(solved) then
    r = 1+2.0D0*dble(N)*rand() - 1.0D-17
    if(floor(r).le.N) then
      do j = 1,N
        Trgt(floor(r),j) = .NOT.Trgt(floor(r),j)
      end do
    else
      r = r-N
      do j = 1,N
        Trgt(j,floor(r)) = .NOT.Trgt(j,floor(r))
      end do
    end if
  else
    exit
  end if
end do

write(*,*) 'Welcome to the Flipping Bits game!'
write(*,*) 'You have the current position'

moves = 0
call display(Brd,Trgt,N)
input = N
do
  write(*,*) 'Number of moves so far:', moves
  write(*,*) 'Select the column or row you wish to flip: '
  read(*,*,iostat=io) input
  if(io.eq.0 .and. input.gt.0 .and. input.le.(2*N)) then
    moves = moves+1
    write(*,*) 'Flipping ', input
    call next_move(Brd,Trgt,N,input,solved)
    call display(Brd,Trgt,N)
    if(solved) exit
  else
    write(*,*) 'Please enter a valid column or row number. To quit, press Ctrl+C!'
  end if
end do

write(*,*) 'Congratulations! You finished the game!'
write(*,ifmt,advance='no') moves
write(*,*) ' moves were taken by you!!'
deallocate(Brd,Trgt)
end program

subroutine display(Brd,Trgt,N)
implicit none
!arguments
integer :: N
logical :: Brd(N,N), Trgt(N,N)
!local
character(len=*), parameter :: cfmt = "(A3)", ifmt = "(I3)"
integer :: i, j
write(*,*) 'Current Configuration: '
do i = 0,N
  if(i.eq.0) then
    write(*,cfmt,advance='no') 'R/C'
    write(*,cfmt,advance='no') ' | '
  else
    write(*,ifmt,advance='no') i
  end if
end do
write(*,*) 
do i = 0,N
  if(i.eq.0) then
    do j = 0,N+2
      write(*,cfmt,advance='no') '---'
    end do
  else
    write(*,ifmt,advance='no') i+N
    write(*,cfmt,advance='no') ' | '
    do j = 1,N
      if(Brd(i,j)) then
        write(*,ifmt,advance='no') 1
      else
        write(*,ifmt,advance='no') 0
      end if
    end do
  end if
  write(*,*)
end do

write(*,*)
write(*,*)

write(*,*) 'Target Configuration'
do i = 0,N
  if(i.eq.0) then
    write(*,cfmt,advance='no') 'R/C'
    write(*,cfmt,advance='no') ' | '
  else
    write(*,ifmt,advance='no') i
  end if
end do
write(*,*) 
do i = 0,N
  if(i.eq.0) then
    do j = 0,N+2
      write(*,cfmt,advance='no') '---'
    end do
  else
    write(*,ifmt,advance='no') i+N
    write(*,cfmt,advance='no') ' | '
    do j = 1,N
      if(Trgt(i,j)) then
        write(*,ifmt,advance='no') 1
      else
        write(*,ifmt,advance='no') 0
      end if
    end do
  end if
  write(*,*)
end do
write(*,*)
write(*,*)
end subroutine

subroutine next_move(Brd,Trgt,N,input,solved)
implicit none
!arguments
integer :: N, input
logical :: Brd(N,N), Trgt(N,N), solved
!others
integer :: i,j

if(input.gt.N) then
  input = input-N
  do i = 1,N
    Brd(input,i) = .not.Brd(input,i)
  end do
else
  do i = 1,N
    Brd(i,input) = .not.Brd(i,input)
  end do
end if
solved = .TRUE.
do i = 1,N
  do j = 1,N
    if( (.not.Brd(i,j).and.Trgt(i,j)) .or. (Brd(i,j).and..not.Trgt(i,j)) ) then
      solved = .FALSE.
      exit
    end if
  end do
  if(.not.solved) exit
end do
end subroutine

```


Example:


```txt

./flipping_bits 
 Enter the number of squares (between 1 and 10) you would like: 
3
 Welcome to the Flipping Bits game!
 You have the current position
 Current Configuration: 
R/C |   1  2  3
------------
  4 |   1  0  0
  5 |   1  1  0
  6 |   0  0  0


 Target Configuration
R/C |   1  2  3
------------
  4 |   1  1  1
  5 |   1  0  1
  6 |   1  0  0


 Select the column or row you wish to flip: 
2
 Current Configuration: 
R/C |   1  2  3
------------
  4 |   1  1  0
  5 |   1  0  0
  6 |   0  1  0


 Target Configuration
R/C |   1  2  3
------------
  4 |   1  1  1
  5 |   1  0  1
  6 |   1  0  0


 Select the column or row you wish to flip: 
3
 Current Configuration: 
R/C |   1  2  3
------------
  4 |   1  1  1
  5 |   1  0  1
  6 |   0  1  1


 Target Configuration
R/C |   1  2  3
------------
  4 |   1  1  1
  5 |   1  0  1
  6 |   1  0  0


 Select the column or row you wish to flip: 
6
 Current Configuration: 
R/C |   1  2  3
------------
  4 |   1  1  1
  5 |   1  0  1
  6 |   1  0  0


 Target Configuration
R/C |   1  2  3
------------
  4 |   1  1  1
  5 |   1  0  1
  6 |   1  0  0


 Congratulations! You finished the game!
  3  moves were taken by you!!

```



## Go



```go
package main

import (
	"fmt"
	"math/rand"
	"time"
)

func main() {

	rand.Seed(time.Now().UnixNano())
    
	var n int = 3 // Change to define board size
	var moves int = 0
	
	a := make([][]int, n)
	for i := range a {
		a[i] = make([]int, n)
		for j := range a {
			a[i][j] = rand.Intn(2)
		}
	}

    b := make([][]int, len(a))
	for i := range a {
		b[i] = make([]int, len(a[i]))
		copy(b[i], a[i])
	}
	
	for i := rand.Intn(100); i > 0 || compareSlices(a, b) == true; i-- {
		b = flipCol(b, rand.Intn(n) + 1)
		b = flipRow(b, rand.Intn(n) + 1)
	}
	
	fmt.Println("Target:")
	drawBoard(a)
	fmt.Println("\nBoard:")
	drawBoard(b)
	
	var rc rune
	var num int
	
	for {
		for{
			fmt.Printf("\nFlip row (r) or column (c)  1 .. %d (c1, ...): ", n)
			_, err := fmt.Scanf("%c%d", &rc, &num)
			if err != nil {
				fmt.Println(err)
				continue
			}
			if num < 1 || num > n {
				fmt.Println("Wrong command!")
				continue
			}
			break
		}
		
		switch rc {
			case 'c':
				fmt.Printf("Column %v will be flipped\n", num)
				flipCol(b, num)
			case 'r':
				fmt.Printf("Row %v will be flipped\n", num)
				flipRow(b, num)
			default:
				fmt.Println("Wrong command!")
				continue
		}
		
		moves++
		fmt.Println("\nMoves taken: ", moves)
		
		fmt.Println("Target:")
		drawBoard(a)
		fmt.Println("\nBoard:")
		drawBoard(b)
	
		if compareSlices(a, b) {
			fmt.Printf("Finished. You win with %d moves!\n", moves)
			break
		}
	}
}

func drawBoard (m [][]int) {
	fmt.Print("   ")
	for i := range m {
		fmt.Printf("%d ", i+1)	
	}
	for i := range m {
		fmt.Println()
		fmt.Printf("%d ", i+1)
		for _, val := range m[i] {
			fmt.Printf(" %d", val)
		}
	}
	fmt.Print("\n")
}

func flipRow(m [][]int, row int) ([][]int) {
	for j := range m {
		m[row-1][j] ^= 1
	}
	return m
}
 
func flipCol(m [][]int, col int) ([][]int) {
	for j := range m {
		m[j][col-1] ^= 1
	}
	return m
}

func compareSlices(m [][]int, n[][]int) bool {
	o := true
	for i := range m {
		for j := range m {
			if m[i][j] != n[i][j] { o = false }
		}
	}
	return o 
}
```

```txt

Target:
   1 2 3 
1  0 1 1
2  0 1 1
3  1 1 1

Board:
   1 2 3 
1  1 0 0
2  1 0 0
3  1 1 1

Flip row (r) or column (c)  1 .. 3 (c1, ...): r1
Row 1 will be flipped

Moves taken:  1
Target:
   1 2 3 
1  0 1 1
2  0 1 1
3  1 1 1

Board:
   1 2 3 
1  0 1 1
2  1 0 0
3  1 1 1

Flip row (r) or column (c)  1 .. 3 (c1, ...): r2
Row 2 will be flipped

Moves taken:  2
Target:
   1 2 3 
1  0 1 1
2  0 1 1
3  1 1 1

Board:
   1 2 3 
1  0 1 1
2  0 1 1
3  1 1 1
Finished. You win with 2 moves!
```



## Haskell

Maximum game size is 9x9 because the array indices are the characters 1 until 9.

```Haskell
import Data.List (intersperse)

import System.Random (randomRIO)

import Data.Array (Array, (!), (//), array, bounds)

import Control.Monad (zipWithM_, replicateM, foldM, when)

type Board = Array (Char, Char) Int

flp :: Int -> Int
flp 0 = 1
flp 1 = 0

numRows, numCols :: Board -> String
numRows t =
  let ((a, _), (b, _)) = bounds t
  in [a .. b]

numCols t =
  let ((_, a), (_, b)) = bounds t
  in [a .. b]

flipRow, flipCol :: Board -> Char -> Board
flipRow t r =
  let e =
        [ (ix, flp (t ! ix))
        | ix <- zip (repeat r) (numCols t) ]
  in t // e

flipCol t c =
  let e =
        [ (ix, flp (t ! ix))
        | ix <- zip (numRows t) (repeat c) ]
  in t // e

printBoard :: Board -> IO ()
printBoard t = do
  let rows = numRows t
      cols = numCols t
      f 0 = '0'
      f 1 = '1'
      p r xs = putStrLn $ [r, ' '] ++ intersperse ' ' (map f xs)
  putStrLn $ "  " ++ intersperse ' ' cols
  zipWithM_
    p
    rows
    [ [ t ! (y, x)
      | x <- cols ]
    | y <- rows ]

-- create a random goal board, and flip rows and columns randomly
-- to get a starting board
setupGame :: Char -> Char -> IO (Board, Board)
setupGame sizey sizex
                -- random cell value at (row, col)
 = do
  let mk rc = (\v -> (rc, v)) <$> randomRIO (0, 1)
      rows = ['a' .. sizey]
      cols = ['1' .. sizex]
  goal <-
    array (('a', '1'), (sizey, sizex)) <$>
    mapM
      mk
      [ (r, c)
      | r <- rows 
      , c <- cols ]
  start <-
    do let change :: Board -> Int -> IO Board
           -- flip random row
           change t 0 = flipRow t <$> randomRIO ('a', sizey)
           -- flip random col
           change t 1 = flipCol t <$> randomRIO ('1', sizex)
       numMoves <- randomRIO (3, 15) -- how many flips (3 - 15)
       -- determine if rows or cols are flipped
       moves <- replicateM numMoves $ randomRIO (0, 1)
       -- make changes and get a starting board
       foldM change goal moves
  if goal /= start -- check if boards are different
    then return (goal, start) -- all ok, return both boards
    else setupGame sizey sizex -- try again

main :: IO ()
main = do
  putStrLn "Select a board size (1 - 9).\nPress any other key to exit."
  sizec <- getChar
  when (sizec `elem` ['1' .. '9']) $
    do let size = read [sizec] - 1
       (g, s) <- setupGame (['a' ..] !! size) (['1' ..] !! size)
       turns g s 0
  where
    turns goal current moves = do
      putStrLn "\nGoal:"
      printBoard goal
      putStrLn "\nBoard:"
      printBoard current
      when (moves > 0) $
        putStrLn $ "\nYou've made " ++ show moves ++ " moves so far."
      putStrLn $
        "\nFlip a row (" ++
        numRows current ++ ") or a column (" ++ numCols current ++ ")"
      v <- getChar
      if v `elem` numRows current
        then check $ flipRow current v
        else if v `elem` numCols current
               then check $ flipCol current v
               else tryAgain
      where
        check t =
          if t == goal
            then putStrLn $ "\nYou've won in " ++ show (moves + 1) ++ " moves!"
            else turns goal t (moves + 1)
        tryAgain = do
          putStrLn ": Invalid row or column."
          turns goal current moves
```

```txt
Select a board size (1 - 9).
Press any other key to exit.
3
Goal:
  1 2 3
a 1 1 0
b 1 0 0
c 0 0 0

Board:
  1 2 3
a 1 0 0
b 1 1 0
c 1 0 1

Flip a row (abc) or a column (123)
2
Goal:
  1 2 3
a 1 1 0
b 1 0 0
c 0 0 0

Board:
  1 2 3
a 1 1 0
b 1 0 0
c 1 1 1

You've made 1 moves so far.

Flip a row (abc) or a column (123)
c
You've won in 2 moves!
```



## J


Using J's command line as the game ui:


```J
start=:3 :0
  Moves=:0
  N=:i.y
  Board=: ?2$~,~y
  'fr fc'=. (2,y)$}.#:(+?&.<:@<:)2x^2*y
  End=: fr~:fc~:"1 Board
  Board;End
)

abc=:'abcdefghij'
move=:3 :0
  fc=. N e.abc i. y ([-.-.)abc
  fr=. N e._-.~_ "."0 abc-.~":y
  Board=: fr~:fc~:"1 Board
  smoutput (":Moves=:Moves++/fr,fc),' moves'
  if. Board-:End do.
    'yes'
  else.
    Board;End
  end.
)
```


Example:


```J
   start 3
┌─────┬─────┐
│1 1 1│1 0 1│
│1 1 0│0 1 1│
│1 0 0│0 0 1│
└─────┴─────┘
   move 'b2'
2 moves
┌─────┬─────┐
│1 0 1│1 0 1│
│1 0 0│0 1 1│
│0 0 1│0 0 1│
└─────┴─────┘
   move '1'
3 moves
yes
```


Note that any size game may be generated but this version only recognizes column flips for the first ten columns.


## Java

[[File:Flipping_bits_java.gif|200px|thumb|right]]
```java
import java.awt.*;
import java.awt.event.*;
import java.util.*;
import javax.swing.*;

public class FlippingBitsGame extends JPanel {
    final int maxLevel = 7;
    final int minLevel = 3;

    private Random rand = new Random();
    private int[][] grid, target;
    private Rectangle box;
    private int n = maxLevel;
    private boolean solved = true;

    FlippingBitsGame() {
        setPreferredSize(new Dimension(640, 640));
        setBackground(Color.white);
        setFont(new Font("SansSerif", Font.PLAIN, 18));

        box = new Rectangle(120, 90, 400, 400);

        startNewGame();

        addMouseListener(new MouseAdapter() {
            @Override
            public void mousePressed(MouseEvent e) {
                if (solved) {
                    startNewGame();
                } else {
                    int x = e.getX();
                    int y = e.getY();

                    if (box.contains(x, y))
                        return;

                    if (x > box.x && x < box.x + box.width) {
                        flipCol((x - box.x) / (box.width / n));

                    } else if (y > box.y && y < box.y + box.height)
                        flipRow((y - box.y) / (box.height / n));

                    if (solved(grid, target))
                        solved = true;

                    printGrid(solved ? "Solved!" : "The board", grid);
                }
                repaint();
            }
        });
    }

    void startNewGame() {
        if (solved) {

            n = (n == maxLevel) ? minLevel : n + 1;

            grid = new int[n][n];
            target = new int[n][n];

            do {
                shuffle();

                for (int i = 0; i < n; i++)
                    target[i] = Arrays.copyOf(grid[i], n);

                shuffle();

            } while (solved(grid, target));

            solved = false;
            printGrid("The target", target);
            printGrid("The board", grid);
        }
    }

    void printGrid(String msg, int[][] g) {
        System.out.println(msg);
        for (int[] row : g)
            System.out.println(Arrays.toString(row));
        System.out.println();
    }

    boolean solved(int[][] a, int[][] b) {
        for (int i = 0; i < n; i++)
            if (!Arrays.equals(a[i], b[i]))
                return false;
        return true;
    }

    void shuffle() {
        for (int i = 0; i < n * n; i++) {
            if (rand.nextBoolean())
                flipRow(rand.nextInt(n));
            else
                flipCol(rand.nextInt(n));
        }
    }

    void flipRow(int r) {
        for (int c = 0; c < n; c++) {
            grid[r][c] ^= 1;
        }
    }

    void flipCol(int c) {
        for (int[] row : grid) {
            row[c] ^= 1;
        }
    }

    void drawGrid(Graphics2D g) {
        g.setColor(getForeground());

        if (solved)
            g.drawString("Solved! Click here to play again.", 180, 600);
        else
            g.drawString("Click next to a row or a column to flip.", 170, 600);

        int size = box.width / n;

        for (int r = 0; r < n; r++)
            for (int c = 0; c < n; c++) {
                g.setColor(grid[r][c] == 1 ? Color.blue : Color.orange);
                g.fillRect(box.x + c * size, box.y + r * size, size, size);
                g.setColor(getBackground());
                g.drawRect(box.x + c * size, box.y + r * size, size, size);
                g.setColor(target[r][c] == 1 ? Color.blue : Color.orange);
                g.fillRect(7 + box.x + c * size, 7 + box.y + r * size, 10, 10);
            }
    }

    @Override
    public void paintComponent(Graphics gg) {
        super.paintComponent(gg);
        Graphics2D g = (Graphics2D) gg;
        g.setRenderingHint(RenderingHints.KEY_ANTIALIASING,
                RenderingHints.VALUE_ANTIALIAS_ON);

        drawGrid(g);
    }

    public static void main(String[] args) {
        SwingUtilities.invokeLater(() -> {
            JFrame f = new JFrame();
            f.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
            f.setTitle("Flipping Bits Game");
            f.setResizable(false);
            f.add(new FlippingBitsGame(), BorderLayout.CENTER);
            f.pack();
            f.setLocationRelativeTo(null);
            f.setVisible(true);
        });
    }
}
```


```txt
The target
[0, 1, 0]
[0, 1, 0]
[0, 1, 0]

The board
[0, 1, 1]
[0, 1, 1]
[0, 1, 1]

Solved!
[0, 1, 0]
[0, 1, 0]
[0, 1, 0]
```


## JavaScript


```JavaScript
function numOfRows(board) { return board.length; }
function numOfCols(board) { return board[0].length; }
function boardToString(board) {
    // First the top-header
    var header = '   ';
    for (var c = 0; c < numOfCols(board); c++)
        header += c + ' ';
    
    // Then the side-header + board
    var sideboard = [];
    for (var r = 0; r < numOfRows(board); r++) {
        sideboard.push(r + ' [' + board[r].join(' ') + ']');
    }
    
    return header + '\n' + sideboard.join('\n');
}
function flipRow(board, row) {
    for (var c = 0; c < numOfCols(board); c++) {
        board[row][c] = 1 - board[row][c];
    }
}
function flipCol(board, col) {
    for (var r = 0; r < numOfRows(board); r++) {
        board[r][col] = 1 - board[r][col];
    }
}

function playFlippingBitsGame(rows, cols) {
    rows = rows | 3;
    cols = cols | 3;
    var targetBoard = [];
    var manipulatedBoard = [];
    // Randomly generate two identical boards.
    for (var r = 0; r < rows; r++) {
        targetBoard.push([]);
        manipulatedBoard.push([]);
        for (var c = 0; c < cols; c++) {
            targetBoard[r].push(Math.floor(Math.random() * 2));
            manipulatedBoard[r].push(targetBoard[r][c]);
        }
    }
    // Naive-scramble one of the boards.
    while (boardToString(targetBoard) == boardToString(manipulatedBoard)) {
        var scrambles = rows * cols;
        while (scrambles-- > 0) {
            if (0 == Math.floor(Math.random() * 2)) {
                flipRow(manipulatedBoard, Math.floor(Math.random() * rows));
            }
            else {
                flipCol(manipulatedBoard, Math.floor(Math.random() * cols));
            }
        }
    }
    // Get the user to solve.
    alert(
        'Try to match both boards.\n' +
        'Enter `r<num>` or `c<num>` to manipulate a row or col or enter `q` to quit.'
        );
    var input = '', letter, num, moves = 0;
    while (boardToString(targetBoard) != boardToString(manipulatedBoard) && input != 'q') {
        input = prompt(
            'Target:\n' + boardToString(targetBoard) +
            '\n\n\n' +
            'Board:\n' + boardToString(manipulatedBoard)
            );
        try {
            letter = input.charAt(0);
            num = parseInt(input.slice(1));
            if (letter == 'q')
				break;
            if (isNaN(num)
                || (letter != 'r' && letter != 'c')
                || (letter == 'r' && num >= rows)
                || (letter == 'c' && num >= cols)
                ) {
                throw new Error('');
            }
            if (letter == 'r') {
                flipRow(manipulatedBoard, num);
            }
            else {
                flipCol(manipulatedBoard, num);
            }
            moves++;
        }
        catch(e) {
            alert('Uh-oh, there seems to have been an input error');
        }
    }
    if (input == 'q') {
        alert('~~ Thanks for playing ~~');
    }
    else {
        alert('Completed in ' + moves + ' moves.');
    }
}
```


```txt
Try to match both boards.
Enter `r<num>` or `c<num>` to manipulate a row or col or enter `q` to quit.

Target:
   0 1 2 
0 [0 1 1]
1 [1 0 0]
2 [1 0 1]


Board:
   0 1 2 
0 [0 0 0]
1 [1 1 1]
2 [0 0 1]
r2

Target:
   0 1 2 
0 [0 1 1]
1 [1 0 0]
2 [1 0 1]


Board:
   0 1 2 
0 [0 0 0]
1 [0 0 0]
2 [0 0 1]
c0

Target:
   0 1 2 
0 [0 1 1]
1 [1 0 0]
2 [1 0 1]


Board:
   0 1 2 
0 [1 0 0]
1 [1 0 0]
2 [1 0 1]
r0

Completed in 3 moves.
```



## Julia


```julia
module FlippingBitsGame

using Printf, Random
import Base.size, Base.show, Base.==
 
struct Configuration
    M::BitMatrix
end
 
Base.size(c::Configuration) = size(c.M)
function Base.show(io::IO, conf::Configuration)
    M = conf.M
    nrow, ncol = size(M)
    print(io, " " ^ 3)
    for c in 1:ncol
        @printf(io, "%3i", c)
    end
    println(io, "\n", " " ^ 4, "-" ^ 3ncol)
    for r in 1:nrow
        @printf(io, "%2i |", r)
        for c in 1:ncol
            @printf(io, "%2c ", ifelse(M[r, c], 'T', 'F'))
        end
        println(io)
    end
    return nothing
end
Base.:(==)(a::Configuration, b::Configuration) = a.M == b.M
 
struct Index{D}
    i::Int
end
const ColIndex = Index{:C}
const RowIndex = Index{:R}
 
function flipbits!(conf::Configuration, c::ColIndex)
    col = @view conf.M[:, c.i]
    @. col = !col
    return conf
end
function flipbits!(conf::Configuration, r::RowIndex)
    row = @view conf.M[r.i, :]
    @. row = !row
    return conf
end
 
randomconfig(nrow::Integer, ncol::Integer) = Configuration(bitrand(nrow, ncol))

function randommoves!(conf::Configuration, nflips::Integer)
    nrow, ncol = size(conf)
    for _ in Base.OneTo(nflips)
        if rand() < 0.5
            flipbits!(conf, ColIndex(rand(1:ncol)))
        else
            flipbits!(conf, RowIndex(rand(1:nrow)))
        end
    end
    return conf
end
 
function play()
    nrow::Int, ncol::Int = 0, 0
    while nrow < 2 || ncol < 2
        print("Insert the size of the matrix (nrow [> 1] *space* ncol [> 1]):")
        nrow, ncol = parse.(Int, split(readline()))
    end
    mat = randomconfig(nrow, ncol)
    obj = deepcopy(mat)
    randommoves!(obj, 100)
    nflips = 0
    while mat != obj
        println("\n", nflips, " flips until now.")
        println("Current configuration:")
        println(mat)
        println("Objective configuration:")
        println(obj)
        print("Insert R[ind] to flip row, C[ind] to flip a column, Q to quit: ")
        line  = readline()
        input = match(r"([qrc])(\d+)"i, line)
        if input ≢ nothing && all(input.captures .≢ nothing)
            dim = Symbol(uppercase(input.captures[1]))
            ind = Index{dim}(parse(Int, input.captures[2]))
            flipbits!(mat, ind)
            nflips += 1
        elseif occursin("q", line)
            println("\nSEE YOU SOON!")
            return
        else
            println("\nINPUT NOT VALID, RETRY!\n")
        end
    end
    println("\nSUCCED! In ", nflips, " flips.")
    println(mat)
    return
end
 
end  # module FlippingBitsGame

using .FlippingBitsGame

FlippingBitsGame.play()

```
```txt
Insert the size of the matrix (nrow [> 1] *space* ncol [> 1]):3 3

0 flips until now.
Current configuration:
     1  2  3
    ---------
 1 | F  F  F 
 2 | T  F  T 
 3 | F  F  T 

Objective configuration:
     1  2  3
    ---------
 1 | F  F  T 
 2 | F  T  T 
 3 | F  F  F 

Insert R[ind] to flip row, C[ind] to flip a column, Q to quit: c3

1 flips until now.
Current configuration:
     1  2  3
    ---------
 1 | F  F  T 
 2 | T  F  F 
 3 | F  F  F 

Objective configuration:
     1  2  3
    ---------
 1 | F  F  T 
 2 | F  T  T 
 3 | F  F  F 

Insert R[ind] to flip row, C[ind] to flip a column, Q to quit: r2

SUCCED! In 2 flips.
     1  2  3
    ---------
 1 | F  F  T 
 2 | F  T  T 
 3 | F  F  F 

```



## Kotlin


```scala
// version 1.1.3

import java.util.Random

val rand = Random()
val target = Array(3) { IntArray(3) { rand.nextInt(2) } }
val board  = Array(3) { IntArray(3) }

fun flipRow(r: Int) {
    for (c in 0..2) board[r][c] = if (board[r][c] == 0) 1 else 0
}

fun flipCol(c: Int) {
    for (r in 0..2) board[r][c] = if (board[r][c] == 0) 1 else 0
}

/** starting from the target we make 9 random row or column flips */
fun initBoard() {
    for (i in 0..2) {
        for (j in 0..2) board[i][j] = target[i][j]
    }
    repeat(9) {
        val rc = rand.nextInt(2)
        if (rc == 0)
            flipRow(rand.nextInt(3))
        else
            flipCol(rand.nextInt(3))
    }
}

fun printBoard(label: String, isTarget: Boolean = false) {
    val a = if (isTarget) target else board
    println("$label:")
    println("  | a b c")
    println("---------")
    for (r in 0..2) {
        print("${r + 1} |")
        for (c in 0..2) print(" ${a[r][c]}")
        println()
    }
    println()
}

fun gameOver(): Boolean {
    for (r in 0..2) {
        for (c in 0..2) if (board[r][c] != target[r][c]) return false
    }
    return true
}

fun main(args: Array<String>) {
     // initialize board and ensure it differs from the target i.e. game not already over!
    do {
        initBoard()
    }
    while(gameOver())

    printBoard("TARGET", true)
    printBoard("OPENING BOARD")
    var flips = 0

    do {
        var isRow = true
        var n = -1
        do {
            print("Enter row number or column letter to be flipped: ")
            val input = readLine()!!
            val ch = if (input.isNotEmpty()) input[0].toLowerCase() else '0'
            if (ch !in "123abc") {
                println("Must be 1, 2, 3, a, b or c")
                continue
            }
            if (ch in '1'..'3') {
                n = ch.toInt() - 49
            }
            else {
                isRow = false
                n = ch.toInt() - 97
            }
        }
        while (n == -1)

        flips++
        if (isRow) flipRow(n) else flipCol(n)
        val plural = if (flips == 1) "" else "S"
        printBoard("\nBOARD AFTER $flips FLIP$plural")
    }
    while (!gameOver())

    val plural = if (flips == 1) "" else "s"
    println("You've succeeded in $flips flip$plural")
}
```


A sample game:

```txt

TARGET:
  | a b c
---------
1 | 0 1 0
2 | 0 1 0
3 | 1 1 1

OPENING BOARD:
  | a b c
---------
1 | 1 0 0
2 | 0 1 1
3 | 0 0 1

Enter row number or column letter to be flipped: 1

BOARD AFTER 1 FLIP:
  | a b c
---------
1 | 0 1 1
2 | 0 1 1
3 | 0 0 1

Enter row number or column letter to be flipped: 3

BOARD AFTER 2 FLIPS:
  | a b c
---------
1 | 0 1 1
2 | 0 1 1
3 | 1 1 0

Enter row number or column letter to be flipped: c

BOARD AFTER 3 FLIPS:
  | a b c
---------
1 | 0 1 0
2 | 0 1 0
3 | 1 1 1

You've succeeded in 3 flips!

```



## Lua


```lua

target, board, moves, W, H = {}, {}, 0, 3, 3

function getIndex( i, j ) return i + j * W - W end

function flip( d, r )
    function invert( a ) if a == 1 then return 0 end return 1 end
    local idx
    if d == 1 then
        for i = 1, W do
            idx = getIndex( i, r )
            board[idx] = invert( board[idx] )
        end
    else
        for i = 1, H do
            idx = getIndex( r, i )
            board[idx] = invert( board[idx] )
        end
    end
    moves = moves + 1
end
function createTarget()
    target, board = {}, {}
    local idx
    for j = 1, H do
        for i = 1, W do
            idx = getIndex( i, j )
            if math.random() < .5 then target[idx] = 0
            else target[idx] = 1
            end
            board[idx] = target[idx]
        end
    end
    for i = 1, 103 do
        if math.random() < .5 then flip( 1, math.random( H ) )
        else flip( 2, math.random( W ) )
        end
    end
    moves = 0
end
function getUserInput()
    io.write( "Input row and/or column: " ); local r = io.read()
    local a
    for i = 1, #r do
        a = string.byte( r:sub( i, i ):lower() )
        if a >= 48 and a <= 57 then flip( 2, a - 48 ) end
        if a >= 97 and a <= string.byte( 'z' ) then flip( 1, a - 96 ) end
    end
end
function solved()
    local idx
    for j = 1, H do
        for i = 1, W do
            idx = getIndex( i, j )
            if target[idx] ~= board[idx] then return false end
        end
    end
    return true
end
function display()
    local idx
    io.write( "\nTARGET\n   " )
    for i = 1, W do io.write( string.format( "%d  ", i ) ) end; print()
    for j = 1, H do
        io.write( string.format( "%s  ", string.char( 96 + j ) ) )
        for i = 1, W do
            idx = getIndex( i, j )
            io.write( string.format( "%d  ", target[idx] ) )
        end; io.write( "\n" )
    end
    io.write( "\nBOARD\n   " )
    for i = 1, W do io.write( string.format( "%d  ", i ) ) end; print()
    for j = 1, H do
        io.write( string.format( "%s  ", string.char( 96 + j ) ) )
        for i = 1, W do
            idx = getIndex( i, j )
            io.write( string.format( "%d  ", board[idx] ) )
        end; io.write( "\n" )
    end
    io.write( string.format( "Moves: %d\n", moves ) )
end
function play()
    while true do
        createTarget()
        repeat
            display()
            getUserInput()
        until solved()
        display()
        io.write( "Very well!\nPlay again(Y/N)? " );
        if io.read():lower() ~= "y" then return end
    end
end
--[[entry point]]--
math.randomseed( os.time() )
play()

```

```txt

TARGET
   1  2  3
a  0  0  0
b  0  0  0
c  1  0  0

BOARD
   1  2  3
a  1  1  0
b  0  0  1
c  0  1  0
Moves: 0
Input row and/or column: 3ac

TARGET
   1  2  3
a  0  0  0
b  0  0  0
c  1  0  0

BOARD
   1  2  3
a  0  0  0
b  0  0  0
c  1  0  0
Moves: 3
Very well!
Play again(Y/N)?

```



## Maple

Click [http://maplecloud.maplesoft.com/application.jsp?appId=5721146878066688 here] to play this game online.

```maple
FlippingBits := module()
	export ModuleApply;
	local gameSetup, flip, printGrid, checkInput;
	local board;
	
	gameSetup := proc(n)
		local r, c, i, toFlip, target;
		randomize():
		target := Array( 1..n, 1..n, rand(0..1) );
		board := copy(target);
		for i to rand(3..9)() do
			toFlip := [0, 0];
			toFlip[1] := StringTools[Random](1, "rc");
			toFlip[2] := convert(rand(1..n)(), string);
			flip(toFlip);
		end do;
		return target;
	end proc;
	
	flip := proc(line)
		local i, lineNum;
		lineNum := parse(op(line[2..-1]));
		for i to upperbound(board)[1] do
			if line[1] = "R" then
				board[lineNum, i] := `if`(board[lineNum, i] = 0, 1, 0);
			else
				board[i, lineNum] := `if`(board[i, lineNum] = 0, 1, 0);
			end if;
		end do;
		return NULL;
	end proc;
	
	printGrid := proc(grid)
		local r, c;
		for r to upperbound(board)[1] do
			for c to upperbound(board)[1] do
				printf("%a ", grid[r, c]);
			end do;
			printf("\n");
		end do;
		printf("\n");
		return NULL;
	end proc;

	checkInput := proc(input)
		try
			if input[1] = "" then
				return false, "";
			elif not input[1] = "R" and not input[1] = "C" then
				return false, "Please start with 'r' or 'c'.";
			elif not type(parse(op(input[2..-1])), posint) then
				error;
			elif parse(op(input[2..-1])) < 1 or parse(op(input[2..-1])) > upperbound(board)[1] then
				return false, "Row or column number too large or too small.";
			end if;
		catch:
			return false, "Please indicate a row or column number."
		end try;
		return true, "";
	end proc;

	ModuleApply := proc(n)
		local gameOver, toFlip, target, answer, restart;
		restart := true;
		while restart do
			target := gameSetup(n);
			while ArrayTools[IsEqual](target, board) do
				target := gameSetup(n);
			end do;
			gameOver := false;
			while not gameOver do
				printf("The Target:\n");
				printGrid(target);
				printf("The Board:\n");
				printGrid(board);
				if ArrayTools[IsEqual](target, board) then
					printf("You win!! Press enter to play again or type END to quit.\n\n");
					answer := StringTools[UpperCase](readline());
					gameOver := true;
					if answer = "END" then
						restart := false
					end if;
				else
					toFlip := ["", ""];
					while not checkInput(toFlip)[1]  and not gameOver do
						ifelse (not op(checkInput(toFlip)[2..-1]) = "", printf("%s\n\n", op(checkInput(toFlip)[2..-1])), NULL);
						printf("Please enter a row or column to flip. (ex: r1 or c2) Press enter for a new game or type END to quit.\n\n");
						answer := StringTools[UpperCase](readline());
						if answer = "END" or answer = "" then
							gameOver := true;
							if answer = "END" then
								restart := false;
							end if;
						end if;
						toFlip := [substring(answer, 1), substring(answer, 2..-1)];
					end do;
					if not gameOver then
						flip(toFlip);
					end if;
				end if;
			end do;
		end do;
		printf("Game Over!\n");
	end proc;
end module:

FlippingBits(3);
```

```txt

The Target:
1 1 1 
1 1 1 
1 0 1 

The Board:
0 1 1 
0 1 1 
0 0 1 

Please enter a row or column to flip. (ex: r1 or c2) Press enter for a new game or type END to quit.

The Target:
1 1 1 
1 1 1 
1 0 1 

The Board:
1 1 1 
1 1 1 
1 0 1 

You win!! Press enter to play again or type END to quit.

```



## MATLAB

Size can be passed in as an argument or entered after a prompt.

```MATLAB
function FlippingBitsGame(n)
% Play the flipping bits game on an n x n array
    
    % Generate random target array
    fprintf('Welcome to the Flipping Bits Game!\n')
    if nargin < 1
        n = input('What dimension array should we use? ');
    end
    Tar = logical(randi([0 1], n));
    
    % Generate starting array by randomly flipping rows or columns
    Cur = Tar;
    while all(Cur(:) == Tar(:))
        nFlips = randi([3*n max(10*n, 100)]);
        randDim = randi([0 1], nFlips, 1);
        randIdx = randi([1 n], nFlips, 1);
        for k = 1:nFlips
            if randDim(k)
                Cur(randIdx(k), :) = ~Cur(randIdx(k), :);
            else
                Cur(:, randIdx(k)) = ~Cur(:, randIdx(k));
            end
        end
    end
    
    % Print rules
    fprintf('Given a %d x %d logical array,\n', n, n)
    fprintf('and a target array configuration,\n')
    fprintf('attempt to transform the array to the target\n')
    fprintf('by inverting the bits in a whole row or column\n')
    fprintf('at once in as few moves as possible.\n')
    fprintf('Enter the corresponding letter to invert a column,\n')
    fprintf('or the corresponding number to invert a row.\n')
    fprintf('0 will reprint the target array, and no entry quits.\n\n')
    fprintf('Target:\n')
    PrintArray(Tar)
    
    % Play until player wins or quits
    move = true;
    nMoves = 0;
    while ~isempty(move) && any(Cur(:) ~= Tar(:))
        fprintf('Move %d:\n', nMoves)
        PrintArray(Cur)
        move = lower(input('Enter move: ', 's'));
        if length(move) > 1
            fprintf('Invalid move, try again\n')
        elseif move
            r = str2double(move);
            if isnan(r)
                c = move-96;
                if c > n || c < 1
                    fprintf('Invalid move, try again\n')
                else
                    Cur(:, c) = ~Cur(:, c);
                    nMoves = nMoves+1;
                end
            else
                if r > n || r < 0
                    fprintf('Invalid move, try again\n')
                elseif r == 0
                    fprintf('Target:\n')
                    PrintArray(Tar)
                else
                    Cur(r, :) = ~Cur(r, :);
                    nMoves = nMoves+1;
                end
            end
        end
    end
    
    if all(Cur(:) == Tar(:))
        fprintf('You win in %d moves! Try not to flip out!\n', nMoves)
    else
        fprintf('Quitting? The challenge a bit much for you?\n')
    end
end

function PrintArray(A)
    [nRows, nCols] = size(A);
    fprintf('        ')
    fprintf(' %c', (1:nCols)+96)
    fprintf('\n')
    for r = 1:nRows
        fprintf('%8d%s\n', r, sprintf(' %d', A(r, :)))
    end
    fprintf('\n')
end
```

Normal play and winning:

```txt
Welcome to the Flipping Bits Game!
What dimension array should we use? 3
Given a 3 x 3 logical array,
and a target array configuration,
attempt to transform the array to the target
by inverting the bits in a whole row or column
at once in as few moves as possible.
Enter the corresponding letter to invert a column,
or the corresponding number to invert a row.
0 will reprint the target array, and no entry quits.

Target:
         a b c
       1 0 0 0
       2 0 1 0
       3 0 0 0

Move 0:
         a b c
       1 1 1 1
       2 1 0 1
       3 1 1 1

Enter move: a
Move 1:
         a b c
       1 0 1 1
       2 0 0 1
       3 0 1 1

Enter move: b
Move 2:
         a b c
       1 0 0 1
       2 0 1 1
       3 0 0 1

Enter move: c
You win in 3 moves! Try not to flip out!
```


Bad input, reprinting target, and quitting:

```txt
Welcome to the Flipping Bits Game!
What dimension array should we use? 3
Given a 3 x 3 logical array,
and a target array configuration,
attempt to transform the array to the target
by inverting the bits in a whole row or column
at once in as few moves as possible.
Enter the corresponding letter to invert a column,
or the corresponding number to invert a row.
0 will reprint the target array, and no entry quits.

Target:
         a b c
       1 0 0 1
       2 1 0 0
       3 1 0 0

Move 0:
         a b c
       1 0 0 0
       2 0 1 0
       3 0 1 0

Enter move: a
Move 1:
         a b c
       1 1 0 0
       2 1 1 0
       3 1 1 0

Enter move: b
Move 2:
         a b c
       1 1 1 0
       2 1 0 0
       3 1 0 0

Enter move: 0
Target:
         a b c
       1 0 0 1
       2 1 0 0
       3 1 0 0

Move 2:
         a b c
       1 1 1 0
       2 1 0 0
       3 1 0 0

Enter move: hello
Invalid move, try again
Move 2:
         a b c
       1 1 1 0
       2 1 0 0
       3 1 0 0

Enter move: d
Invalid move, try again
Move 2:
         a b c
       1 1 1 0
       2 1 0 0
       3 1 0 0

Enter move: 4
Invalid move, try again
Move 2:
         a b c
       1 1 1 0
       2 1 0 0
       3 1 0 0

Enter move: 
Quitting? The challenge a bit much for you?
```



## MiniScript


```MiniScript
// Flipping Bits game.
// Transform a start grid to an end grid by flipping rows or columns.

size = 3

board = []
goal = []
for i in range(1,size)
    row = []
    for j in range(1,size)
        row.push (rnd > 0.5)
    end for
    board.push row
    goal.push row[0:]
end for

flipRow = function(n)
    for j in range(0, size-1)
        board[n-1][j] = not board[n-1][j]
    end for
end function

flipCol = function(n)
    for i in range(0, size-1)
        board[i][n-1] = not board[i][n-1]
    end for
end function

flipAny = function(s)
    s = s[0].upper
    if s >= "A" then flipCol s.code - 64 else flipRow val(s)
end function

for scramble in range(20)
    if rnd < 0.5 then flipRow ceil(rnd*size) else flipCol ceil(rnd*size)
end for

solved = function()
    for i in range(0, size-1)
        for j in range(0, size-1)
            if board[i][j] != goal[i][j] then return false
        end for
    end for
    return true
end function

moveCount = 0
while true
    print "  CURRENT:" + " "*(4+size*3) + "GOAL:"
    for i in range(1,size)
        s = i + " " + str(board[i-1])
        s = s + " "*(3+size*3) + str(goal[i-1])
        print s
    end for
    s = "   "
    for i in range(1,size)
        s = s + char(64+i) + "  "
    end for
    print s
    if solved then break
    moveCount = moveCount + 1
    inp = input("Move " + moveCount + "? ")
    flipAny(inp)
end while
print "You did it!"
```


```txt
  CURRENT:             GOAL:
1 [0, 0, 0]            [0, 1, 1]
2 [1, 0, 1]            [1, 1, 0]
3 [1, 1, 0]            [0, 1, 0]
   A  B  C  
Move 1? 1
  CURRENT:             GOAL:
1 [1, 1, 1]            [0, 1, 1]
2 [1, 0, 1]            [1, 1, 0]
3 [1, 1, 0]            [0, 1, 0]
   A  B  C  
Move 2? a
  CURRENT:             GOAL:
1 [0, 1, 1]            [0, 1, 1]
2 [0, 0, 1]            [1, 1, 0]
3 [0, 1, 0]            [0, 1, 0]
   A  B  C  
Move 3? 2
  CURRENT:             GOAL:
1 [0, 1, 1]            [0, 1, 1]
2 [1, 1, 0]            [1, 1, 0]
3 [0, 1, 0]            [0, 1, 0]
   A  B  C  
You did it!
```


## OCaml


```ocaml
module FlipGame =
struct
  type t = bool array array

  let make side = Array.make_matrix side side false

  let flipcol b n =
    for i = 0 to (Array.length b - 1) do
      b.(n).(i) <- not b.(n).(i)
    done

  let fliprow b n =
    for i = 0 to (Array.length b - 1) do
      b.(i).(n) <- not b.(i).(n)
    done

  let randflip b =
    let n = Random.int (Array.length b - 1) in
    match Random.bool () with
    | true -> fliprow b n
    | false -> flipcol b n

  let rec game side steps =
    let start, target = make side, make side in
    for i = 1 to steps do
      randflip start;
      randflip target
    done;
    if start = target then game side steps (* try again *) else
      (start, target)

  let print b =
    for i = 0 to Array.length b - 1 do
      for j = 0 to Array.length b - 1 do
        Printf.printf " %d " (if b.(j).(i) then 1 else 0)
      done;
      print_newline ()
    done;
    print_newline ()

  let draw_game board target =
    print_endline "TARGET"; print target;
    print_endline "BOARD"; print board
end

let play () =
  let module G = FlipGame in
  let board, target = G.game 3 10 in
  let steps = ref 0 in
  while board <> target do
    G.draw_game board target;
    print_string "> ";
    flush stdout;
    incr steps;
    match String.split_on_char ' ' (read_line ()) with
    | ["row"; row] ->
      (match int_of_string_opt row with
       | Some n -> G.fliprow board n
       | None -> print_endline "(nothing happens)")
    | ["col"; col] ->
      (match int_of_string_opt col with
       | Some n -> G.flipcol board n
       | None -> print_endline "(nothing happens)")
    | _ -> ()
  done;
  G.draw_game board target;
  Printf.printf "\n\nGame solved in %d steps\n" !steps

let () =
  if not !Sys.interactive then
    (Random.self_init ();
      play ())
```


```txt
$ ocamlc flipgame.ml -o flipgame
$ ./flipgame
TARGET
 0  0  1 
 0  0  1 
 1  1  0 

BOARD
 0  1  0 
 1  0  1 
 0  1  0 

> col 1
TARGET
 0  0  1 
 0  0  1 
 1  1  0 

BOARD
 0  0  0 
 1  1  1 
 0  0  0 

> row 2
TARGET
 0  0  1 
 0  0  1 
 1  1  0 

BOARD
 0  0  0 
 1  1  1 
 1  1  1 

> col 2
TARGET
 0  0  1 
 0  0  1 
 1  1  0 

BOARD
 0  0  1 
 1  1  0 
 1  1  0 

> row 1
TARGET
 0  0  1 
 0  0  1 
 1  1  0 

BOARD
 0  0  1 
 0  0  1 
 1  1  0 



Game solved in 4 steps
```



## Perl

Pass the size of the puzzle on the command line.  It defaults to 4.  You can play any
size game between 2 and 26.  While playing, the game accepts anything which looks like
valid rows or columns, and disregards any irrelevant text in between.


```perl
#!perl
use strict;
use warnings qw(FATAL all);

my $n = shift(@ARGV) || 4;
if( $n < 2 or $n > 26 ) {
	die "You can't play a size $n game\n";
}

my $n2 = $n*$n;

my (@rows, @cols);
for my $i ( 0 .. $n-1 ) {
	my $row = my $col = "\x00" x $n2;
	vec($row, $i * $n + $_, 8) ^= 1 for 0 .. $n-1;
	vec($col, $i + $_ * $n, 8) ^= 1 for 0 .. $n-1;
	push @rows, $row;
	push @cols, $col;
}

my $goal = "0" x $n2;
int(rand(2)) or (vec($goal, $_, 8) ^= 1) for 0 .. $n2-1;
my $start = $goal;
{
	for(@rows, @cols) {
		$start ^= $_ if int rand 2;
	}
	redo if $start eq $goal; 
}

my @letters = ('a'..'z')[0..$n-1];
sub to_strings {
	my $board = shift;
	my @result = join(" ", "  ", @letters);
	for( 0 .. $n-1 ) {
		my $res = sprintf("%2d ",$_+1);
		$res .= join " ", split //, substr $board, $_*$n, $n;
		push @result, $res;
	}
	\@result;
}

my $fmt;
my ($stext, $etext) = ("Starting board", "Ending board");
my $re = join "|", reverse 1 .. $n, @letters;
my $moves_so_far = 0;
while( 1 ) {
	my ($from, $to) = (to_strings($start), to_strings($goal));
	unless( $fmt ) {
		my $len = length $from->[0];
		$len = length($stext) if $len < length $stext;
		$fmt = join($len, "%", "s%", "s\n");
	}
	printf $fmt, $stext, $etext;
	printf $fmt, $from->[$_], $to->[$_] for 0 .. $n;
	last if $start eq $goal;
	INPUT_LOOP: {
		printf "Move #%s: Type one or more row numbers and/or column letters: ",
			$moves_so_far+1;
		my $input = <>;
		die unless defined $input;
		my $did_one;
		for( $input =~ /($re)/gi ) {
			$did_one = 1;
			if( /\d/ ) {
				$start ^= $rows[$_-1];
			} else {
				$_ = ord(lc) - ord('a');
				$start ^= $cols[$_];
			}
			++$moves_so_far;
		}
		redo INPUT_LOOP unless $did_one;
	}
}
print "You won after $moves_so_far moves.\n";
```

```txt
$ perl FlippingBitsGame.pl 3
Starting board  Ending board
         a b c         a b c
       1 0 0 1       1 1 1 1
       2 0 1 1       2 1 0 1
       3 0 0 0       3 0 0 1
Move #1: Type one or more row numbers and/or column letters: 12
Starting board  Ending board
         a b c         a b c
       1 1 1 0       1 1 1 1
       2 1 0 0       2 1 0 1
       3 0 0 0       3 0 0 1
Move #3: Type one or more row numbers and/or column letters: c
Starting board  Ending board
         a b c         a b c
       1 1 1 1       1 1 1 1
       2 1 0 1       2 1 0 1
       3 0 0 1       3 0 0 1
You won after 3 moves.
```


The same game could have been won after typing 1 2 c in any order, with multiple lines or even "12c" on a single line.


## Perl 6

Pass in a parameter to set the square size for the puzzle. (Defaults to 4.) Arbitrarily limited to between 1 and 26. Yes, you can choose to solve a 1 element square puzzle, but it won't be very challenging. Accepts upper or lower case letters for columns. Disregards any out-of-range indices. Enter a blank or 0 (zero) to exit.


```perl6
sub MAIN ($square = 4) {
    say "{$square}? Seriously?" and exit if $square < 1 or $square > 26;
    my %bits = map { $_ => %( map { $_ => 0 }, ('A' .. *)[^ $square] ) },
        (1 .. *)[^ $square];
    scramble %bits;
    my $target = build %bits;
    scramble %bits until build(%bits) ne $target;
    display($target, %bits);
    my $turns = 0;
    while my $flip = prompt "Turn {++$turns}: Flip which row / column? " {
        flip $flip.match(/\w/).uc, %bits;
        if display($target, %bits) {
            say "Hurray! You solved it in $turns turns.";
            last;
        }
    }
}

sub display($goal, %hash) {
    shell('clear');
    say "Goal\n$goal\nYou";
    my $this = build %hash;
    say $this;
    return ($goal eq $this);
}

sub flip ($a, %hash) {
    given $a {
        when any(keys %hash) {
            %hash{$a}{$_} = %hash{$a}{$_} +^ 1 for %hash{$a}.keys
        };
        when any(keys %hash{'1'}) {
            %hash{$_}{$a} = %hash{$_}{$a} +^ 1 for %hash.keys
        };
    }
}

sub build (%hash) {
    my $string = '   ';
    $string ~= sprintf "%2s ", $_ for sort keys %hash{'1'};
    $string ~= "\n";
    for %hash.keys.sort: +* -> $key {
        $string ~= sprintf "%2s ", $key;
        $string ~= sprintf "%2s ", %hash{$key}{$_} for sort keys %hash{$key};
        $string ~=  "\n";
    };
    $string
}

sub scramble(%hash) {
    my @keys = keys %hash;
    @keys.push: | keys %hash{'1'};
    flip $_,  %hash for @keys.pick( @keys/2 );
}
```

A sample 3 x 3 game might look like this:

```txt
Goal
    A  B  C 
 1  1  1  0 
 2  0  0  1 
 3  1  1  0 

You
    A  B  C 
 1  0  0  0 
 2  1  1  1 
 3  1  1  1 

Turn 1: Flip which row / column? 2

Goal
    A  B  C 
 1  1  1  0 
 2  0  0  1 
 3  1  1  0 

You
    A  B  C 
 1  0  0  0 
 2  0  0  0 
 3  1  1  1 

Turn 2: Flip which row / column? 1

Goal
    A  B  C 
 1  1  1  0 
 2  0  0  1 
 3  1  1  0 

You
    A  B  C 
 1  1  1  1 
 2  0  0  0 
 3  1  1  1 

Turn 3: Flip which row / column? c

Goal
    A  B  C 
 1  1  1  0 
 2  0  0  1 
 3  1  1  0 

You
    A  B  C 
 1  1  1  0 
 2  0  0  1 
 3  1  1  0 

Hurray! You solved it in 3 turns.
```



## Phix


```Phix
integer w, h

string board, target

procedure new_board()
    board = ""
    h = prompt_number("Enter number of rows(1..9):",{1,9})
    w = prompt_number("Enter number of columns(1..26):",{1,26})
    string line = ""
    for j=1 to w do line &= 'A'+j-1 end for
    board = "  "&line&"\n"
    for i=1 to h do
        line = '0'+i&" "
        for j=1 to w do line &= '0'+rand(2)-1 end for
        board &= line&"\n"
    end for
end procedure

procedure show_bt()
sequence sb = split(board,'\n'),
         st = split(target,'\n')
    printf(1,"board:%s     target:%s\n",{sb[1],st[1]})
    for i=2 to length(sb)-1 do
        printf(1,"      %s            %s\n",{sb[i],st[i]})
    end for
end procedure

procedure flip(integer ch, bool bShow=true)
integer k
    if ch>='A' and ch<='A'+w-1 then
        -- flip_column
        ch = ch-'A'+1
        for i=1 to h do
            k = 2+ch+i*(w+3)
            board[k] = '0'+'1'-board[k]
        end for
        k = 2+ch
    elsif ch>='1' and ch<='0'+h then
        -- flip_row
        ch -= '0'
        for i=1 to w do
            k = 2+i+(ch)*(w+3)
            board[k] = '0'+'1'-board[k]
        end for
        k = 1+(ch)*(w+3)
    else
        ?9/0    -- sanity check
    end if
    if bShow then
        integer wasch = board[k]
        board[k] = '*'
        show_bt()
        board[k] = wasch
    end if
end procedure

procedure scramble_board()
integer lim = 10000
    while 1 do
        for i=1 to lim do
            if rand(2)=1 then
                flip('A'-1+rand(w),false)
            else
                flip('0'+rand(h),false)
            end if
        end for
        if board!=target then exit end if   
        lim -= 1 -- sidestep the degenerate 1x1 case
    end while
end procedure

function solve_board()
-- not guaranteed optimal (the commented-out length check clogs it on larger boards)
string original = board, moves
sequence next = {{0,board,""}},
         legal_moves = tagset('A'+w-1,'A')&tagset('0'+h,'1')
atom t2 = time()+2 -- in case board is illegal/unsolveable
    while time()<t2 do
        for lm=1 to length(legal_moves) do
            integer c = legal_moves[lm]
            {?,board,moves} = next[1]
            flip(c,false)
            moves &= c
            if board = target then
                board = original
                return moves
            end if
            next = append(next,{sum(sq_eq(board,target)),board,moves})
            for i=length(next) to 3 by -1 do
                if next[i][1]<=next[i-1][1] then exit end if
--              if length(next[i][3])>length(next[i-1][3]) then exit end if
                {next[i-1],next[i]} = {next[i],next[i-1]}
            end for
        end for
        next = next[2..$]
    end while
    board = original
    return 0
end function

constant ESC = #1B

procedure main()
    integer moves = 0, solves = 0, ch
    bool took_hint = false
    new_board()
    target = board
    scramble_board()
    show_bt()
    object soln = solve_board()
    while 1 do
        string solve = iff(string(soln)?sprintf(" solveable in %d,",length(soln)):"") 
        printf(1,"moves taken %d,%s enter your move (A..%c or 1..%c) or ?:",{moves,solve,'A'+w-1,'0'+h})
        while 1 do
            ch = upper(wait_key())
            if ch<=#FF then exit end if -- (ignore control keys)
        end while
        printf(1,"%c\n",ch)
        if (ch>='A' and ch<='A'+w-1)
        or (ch>='1' and ch<='0'+h) then
            flip(ch)
            if board=target then
                printf(1,"\nWell %s!\n\n",{iff(took_hint?"cheated","done")})
                exit
            end if
            moves += 1
            soln = iff(string(soln) and ch=soln[1]?soln[2..$]:solve_board())
        elsif string(soln) and
             (ch='H' -- (nb consumed above if w>=8)
           or ch='.') then
            took_hint = true
            printf(1,"hint: %c\n",soln[1])
        elsif ch='Q' -- (nb consumed above if w>=17)
           or ch=ESC then
            exit
        elsif string(soln) and
             (ch='S' -- (nb consumed above if w>=19)
           or ch='!') then
            for i=1 to length(soln) do
                printf(1,"auto-solving, move %d:\n",i)
                flip(soln[i])
                sleep(2)
            end for
            exit    
        else
            puts(1,"press ")
            if string(soln) then
                puts(1,"'!' (or 's' if width<19) to solve the board automatically,\n")
                puts(1,"      '.' (or 'h' if width<8) to show hint,\n")
            end if
            puts(1,"      escape (or 'q' if width<17) to quit\n")
        end if
    end while
end procedure
main()
```

```txt

Enter number of rows(1..9):2
Enter number of columns(1..26):2
board:  AB     target:  AB
      1 11            1 10
      2 00            2 10
moves taken 0, solveable in 2, enter your move (A..B or 1..2) or ?:?
press '!' (or 's' if width<19) to solve the board automatically,
      '.' (or 'h' if width<8) to show hint,
      escape (or 'q' if width<17) to quit
moves taken 0, solveable in 2, enter your move (A..B or 1..2) or ?:H
hint: A
moves taken 0, solveable in 2, enter your move (A..B or 1..2) or ?:A
board:  *B     target:  AB
      1 01            1 10
      2 10            2 10
moves taken 1, solveable in 1, enter your move (A..B or 1..2) or ?:H
hint: 1
moves taken 1, solveable in 1, enter your move (A..B or 1..2) or ?:1
board:  AB     target:  AB
      * 10            1 10
      2 10            2 10

Well cheated!

```

An auto solve:

```txt

Enter number of rows(1..9):2
Enter number of columns(1..26):2
board:  AB     target:  AB
      1 10            1 00
      2 10            2 11
moves taken 0, solveable in 2, enter your move (A..B or 1..2) or ?:!
auto-solving, move 1:
board:  *B     target:  AB
      1 00            1 00
      2 00            2 11
auto-solving, move 2:
board:  AB     target:  AB
      1 00            1 00
      * 11            2 11

```

The maximum board size is 9x26:

```txt

Enter number of rows(1..9):9
Enter number of columns(1..26):26
board:  ABCDEFGHIJKLMNOPQRSTUVWXYZ     target:  ABCDEFGHIJKLMNOPQRSTUVWXYZ
      1 11010000101000110010101101            1 00010011000001110010101001
      2 10000011010000110100001100            2 10111111000110001011110111
      3 00100011010011100110111101            3 11100000111010100110111001
      4 00101110001111100001001000            4 00010010011001011110110011
      5 01001001111011101011100011            5 10001010010010101011100111
      6 01010001101111101110111011            6 10010010000110101110111111
      7 10011000011101100100000111            7 01011011110100100100000011
      8 01111100111110000001100101            8 01000000101000111110011110
      9 00111000101100111000011011            9 11111011000101111000011111
moves taken 0, solveable in 11, enter your move (A..Z or 1..9) or ?:

```

And the minimum is 1x1:

```txt

Enter number of rows(1..9):1
Enter number of columns(1..26):1
board:  A     target:  A
      1 0            1 1
moves taken 0, solveable in 1, enter your move (A..A or 1..1) or ?:1
board:  A     target:  A
      * 1            1 1
Well done!

```



## PL/I


```PL/I
(subscriptrange, stringrange, stringsize):
flip: procedure options (main);
   declare n fixed binary;

   put skip list ('This is the bit-flipping game.  What size of board do you want?');
   get list (n);
   put skip list
      ('Your task is to change your board so as match the board on the right (the objective)');

   begin;
      declare initial(n,n) bit (1), objective(n,n) bit (1);
      declare (i, j, k, move) fixed binary;
      declare ch character(1);
      declare alphabet character (26) initial ('abcdefghijklmnopqrstuvwxyz');

      on subrg
         begin; put skip list ('Your row or column ' || trim(ch) || ' is out of range'); stop; end;

      initial, objective = iand(random()*99, 1) = 1;

      /* Set up the objective array: */
      do i = 1 to n-1;
         j = random()*n+1;  objective(j,*) = ^objective(j,*);
         j = random()*n+1;  objective(*,j) = ^objective(*,j);
      end;

      do move = 0 by 1;
         put skip edit ( center('You', n*3), center('The objective', 3*n+4) ) (x(3), a);
         put skip edit ( (substr(alphabet, i, 1) do i = 1 to n) ) (x(5), (n) a(3));
         put      edit ( (substr(alphabet, i, 1) do i = 1 to n) ) (x(3), (n) a(3));
         do i = 1 to n;
            put skip edit (i, initial(i,*), objective(i,*)) ((n+1) f(3), x(3), (n) F(3));
         end;

         if all(initial = objective) then leave;

         put skip(2) list
            ('Please type a row number or column letter whose bits you want to flip: ');
         get edit (ch) (L); put edit (ch) (a);
         k = index(alphabet, ch);
         if k > 0 then
            initial(*, k) = ^initial(*,k); /* Flip column k */
         else
            initial(ch,*) = ^initial(ch,*); /* Flip row ch */
      end;
      put skip(2) list ('Congratulations. You solved it in ' || trim(move) || ' moves.');
   end;

end flip;
```

```txt

This is the bit-flipping game.  What size of board do you want? 

Your task is to change your board so as match the board on the right (the objective) 
      You      The objective
     a  b  c     a  b  c  
  1  1  1  1     0  1  1
  2  0  0  0     1  0  0
  3  1  1  1     1  0  0

Please type a row number or column letter whose bits you want to flip:  
3
      You      The objective
     a  b  c     a  b  c  
  1  1  1  1     0  1  1
  2  0  0  0     1  0  0
  3  0  0  0     1  0  0

Please type a row number or column letter whose bits you want to flip:  
a
      You      The objective
     a  b  c     a  b  c  
  1  0  1  1     0  1  1
  2  1  0  0     1  0  0
  3  1  0  0     1  0  0

Congratulations. You solved it in 2 moves.

```



## Python


```python
"""
Given a %i by %i sqare array of zeroes or ones in an initial
configuration, and a target configuration of zeroes and ones
The task is to transform one to the other in as few moves as 
possible by inverting whole numbered rows or whole lettered 
columns at once.
In an inversion any 1 becomes 0 and any 0 becomes 1 for that
whole row or column.

"""

from random import randrange
from copy import deepcopy
from string import ascii_lowercase


try:    # 2to3 fix
    input = raw_input
except:
    pass

N = 3   # N x N Square arrray

board  = [[0]* N for i in range(N)]

def setbits(board, count=1):
    for i in range(count):
        board[randrange(N)][randrange(N)] ^= 1

def shuffle(board, count=1):
    for i in range(count):
        if randrange(0, 2):
            fliprow(randrange(N))
        else:
            flipcol(randrange(N))


def pr(board, comment=''):
    print(str(comment))
    print('     ' + ' '.join(ascii_lowercase[i] for i in range(N)))
    print('  ' + '\n  '.join(' '.join(['%2s' % j] + [str(i) for i in line])
                             for j, line in enumerate(board, 1)))

def init(board):
    setbits(board, count=randrange(N)+1)
    target = deepcopy(board)
    while board == target:
        shuffle(board, count=2 * N)
    prompt = '  X, T, or 1-%i / %s-%s to flip: ' % (N, ascii_lowercase[0], 
                                                    ascii_lowercase[N-1])
    return target, prompt

def fliprow(i):
    board[i-1][:] = [x ^ 1 for x in board[i-1] ]
    
def flipcol(i):
    for row in board:
        row[i] ^= 1

if __name__ == '__main__':
    print(__doc__ % (N, N))
    target, prompt = init(board)
    pr(target, 'Target configuration is:')
    print('')
    turns = 0
    while board != target:
        turns += 1
        pr(board, '%i:' % turns)
        ans = input(prompt).strip()
        if (len(ans) == 1 
            and ans in ascii_lowercase and ascii_lowercase.index(ans) < N):
            flipcol(ascii_lowercase.index(ans))
        elif ans and all(ch in '0123456789' for ch in ans) and 1 <= int(ans) <= N:
            fliprow(int(ans))
        elif ans == 'T':
            pr(target, 'Target configuration is:')
            turns -= 1
        elif ans == 'X':
            break
        else:
            print("  I don't understand %r... Try again. "
                  "(X to exit or T to show target)\n" % ans[:9])
            turns -= 1
    else:
        print('\nWell done!\nBye.')
```


```txt
Given a 3 by 3 sqare array of zeroes or ones in an initial
configuration, and a target configuration of zeroes and ones
The task is to transform one to the other in as few moves as 
possible by inverting whole numbered rows or whole lettered 
columns at once.
In an inversion any 1 becomes 0 and any 0 becomes 1 for that
whole row or column.


Target configuration is:
     a b c
   1 0 1 0
   2 0 0 0
   3 0 0 0

1:
     a b c
   1 1 0 0
   2 0 0 1
   3 0 0 1
  X, T, or 1-3 / a-c to flip: 1
2:
     a b c
   1 0 1 1
   2 0 0 1
   3 0 0 1
  X, T, or 1-3 / a-c to flip: c

Well done!
Bye.
```


;Showing bad/other inputs:

```txt
Target configuration is:
     a b c
   1 0 0 0
   2 0 0 0
   3 0 0 1

1:
     a b c
   1 1 0 1
   2 0 1 0
   3 0 1 1
  X, T, or 1-3 / a-c to flip: 3
2:
     a b c
   1 1 0 1
   2 0 1 0
   3 1 0 0
  X, T, or 1-3 / a-c to flip: 4
  I don't understand '4'... Try again. (X to exit or T to show target)

2:
     a b c
   1 1 0 1
   2 0 1 0
   3 1 0 0
  X, T, or 1-3 / a-c to flip: c
3:
     a b c
   1 1 0 0
   2 0 1 1
   3 1 0 1
  X, T, or 1-3 / a-c to flip: d
  I don't understand 'd'... Try again. (X to exit or T to show target)

3:
     a b c
   1 1 0 0
   2 0 1 1
   3 1 0 1
  X, T, or 1-3 / a-c to flip: T
Target configuration is:
     a b c
   1 0 0 0
   2 0 0 0
   3 0 0 1
3:
     a b c
   1 1 0 0
   2 0 1 1
   3 1 0 1
  X, T, or 1-3 / a-c to flip: X
```



## QB64

<lang>
RANDOMIZE TIMER
DIM SHARED cellsPerSide, legalMoves$, startB$, currentB$, targetB$, moveCount

restart
DO
    displayStatus
    IF currentB$ = targetB$ THEN 'game done!
        PRINT " Congratulations, done in"; moveCount; " moves."
        PRINT "": PRINT " Press y for yes, if you want to start over > ";
        yes$ = getKey$: PRINT yes$: _DELAY .4: vcls
        IF yes$ = "y" THEN restart ELSE nomore = -1
    ELSE 'get next move
        m$ = " ": PRINT
        WHILE INSTR(legalMoves$, m$) = 0
            PRINT " Press a lettered column or a numbered row to flip (or 0,q,?,!) > ";
            m$ = getKey$: PRINT m$: _DELAY .4
            IF m$ = "!" THEN
                showSolution = -1: m$ = " ": EXIT WHILE
            ELSEIF m$ = "?" THEN: m$ = " ": cp CSRLIN, "Hint: " + hint$
            ELSEIF m$ = "0" OR m$ = "q" THEN: vcls: CLOSE: END
            ELSEIF m$ = "" THEN: m$ = " "
            END IF
        WEND
        IF showSolution THEN 'run the solution from hints function
            showSolution = 0: mv$ = hint$
            cp CSRLIN + 1, "For the next move, the AI has chosen: " + mv$
            cp CSRLIN + 1, "Running the solution with 4 sec screen delays..."
            _DELAY 4: vcls
            WHILE mv$ <> "Done?"
                moveCount = moveCount + 1: makeMove mv$: displayStatus: mv$ = hint$
                cp CSRLIN + 1, "For the next move, the AI has chosen: " + mv$
                cp CSRLIN + 1, "Running the solution with 4 sec screen delays..."
                _DELAY 4: vcls
            WEND
            displayStatus
            cp CSRLIN + 1, "Done! Current board matches Target"
            cp CSRLIN + 1, "Press y for yes, if you want to start over: > "
            yes$ = getKey$: PRINT yes$: _DELAY .4: vcls
            IF yes$ = "y" THEN restart ELSE nomore = -1
        ELSE
            vcls: moveCount = moveCount + 1: makeMove m$
        END IF
    END IF
LOOP UNTIL nomore
CLOSE

SUB displayStatus
    COLOR 9: showBoard 2, 2, currentB$, "Current:"
    COLOR 12: showBoard 2, 2 + 2 * cellsPerSide + 6, targetB$, "Target:"
    COLOR 13: PRINT: PRINT " Number of moves taken so far is" + STR$(moveCount)
    COLOR 14
END SUB


FUNCTION hint$ 'compare the currentB to targetB and suggest letter or digit or done
    FOR i = 1 TO 2 * cellsPerSide 'check cols first then rows as listed in legalMoves$
        r$ = MID$(legalMoves$, i, 1)
        IF i <= cellsPerSide THEN
            currentbit$ = MID$(currentB$, i, 1): targetBit$ = MID$(targetB$, i, 1)
            IF currentbit$ <> targetBit$ THEN flag = -1: EXIT FOR
        ELSE
            j = i - cellsPerSide
            currentbit$ = MID$(currentB$, (j - 1) * cellsPerSide + 1, 1)
            targetBit$ = MID$(targetB$, (j - 1) * cellsPerSide + 1, 1)
            IF currentbit$ <> targetBit$ THEN flag = -1: EXIT FOR
        END IF
    NEXT
    IF flag THEN hint$ = r$ ELSE hint$ = "Done?"
END FUNCTION

SUB restart
    CLOSE
    OPEN "Copy Flipping Bits Game.txt" FOR OUTPUT AS #3
    cellsPerSide = 0: legalMoves$ = "": moveCount = 0
    COLOR 9: cp 3, "Flipping Bits Game, now with AI!  b+ 2017-12-18"
    COLOR 5
    cp 5, "You will be presented with a square board marked Current and"
    cp 6, "another marked Target. The object of the game is to match"
    cp 7, "the Current board to Target in the least amount of moves."
    cp 9, "To make a move, enter a letter for a column to flip or"
    cp 10, "a digit for a row to flip. In a flip, all 1's are"
    cp 11, "changed to 0's and all 0's changed to 1's."
    cp 13, "You may enter 0 or q at any time to quit."
    cp 14, "You may press ? when prompted for move to get a hint."
    cp 15, "You may press ! to have the program solve the puzzle."
    COLOR 14: PRINT: PRINT
    WHILE cellsPerSide < 2 OR cellsPerSide > 9
        LOCATE CSRLIN, 13: PRINT "Please press how many cells you want per side 2 to 9 > ";
        in$ = getKey$: PRINT in$: _DELAY .4
        IF in$ = "0" OR in$ = "q" THEN END ELSE cellsPerSide = VAL(in$)
    WEND
    vcls
    FOR i = 1 TO cellsPerSide: legalMoves$ = legalMoves$ + CHR$(96 + i): NEXT
    FOR i = 1 TO cellsPerSide: legalMoves$ = legalMoves$ + LTRIM$(STR$(i)): NEXT
    startB$ = startBoard$: currentB$ = startB$: targetB$ = makeTarget$: currentB$ = startB$
END SUB

FUNCTION startBoard$
    FOR i = 1 TO cellsPerSide ^ 2: r$ = r$ + LTRIM$(STR$(INT(RND * 2))): NEXT
    startBoard$ = r$
END FUNCTION

SUB showBoard (row, col, board$, title$)
    LOCATE row - 1, col: PRINT title$
    FOR i = 1 TO cellsPerSide
        LOCATE row, col + 2 * (i - 1) + 3: PRINT MID$(legalMoves$, i, 1);
    NEXT
    PRINT
    FOR i = 1 TO cellsPerSide
        LOCATE row + i, col - 1: PRINT STR$(i);
        FOR j = 1 TO cellsPerSide
            LOCATE row + i, col + 2 * j: PRINT " " + MID$(board$, (i - 1) * cellsPerSide + j, 1);
        NEXT
        PRINT
    NEXT
END SUB

SUB makeMove (move$)
    ac = ASC(move$)
    IF ac > 96 THEN 'letter
        col = ac - 96
        FOR i = 1 TO cellsPerSide
            bit$ = MID$(currentB$, (i - 1) * cellsPerSide + col, 1)
            IF bit$ = "0" THEN
                MID$(currentB$, (i - 1) * cellsPerSide + col, 1) = "1"
            ELSE
                MID$(currentB$, (i - 1) * cellsPerSide + col, 1) = "0"
            END IF
        NEXT
    ELSE 'number
        row = ac - 48
        FOR i = 1 TO cellsPerSide
            bit$ = MID$(currentB$, (row - 1) * cellsPerSide + i, 1)
            IF bit$ = "0" THEN
                MID$(currentB$, (row - 1) * cellsPerSide + i, 1) = "1"
            ELSE
                MID$(currentB$, (row - 1) * cellsPerSide + i, 1) = "0"
            END IF
        NEXT
    END IF
END SUB

FUNCTION makeTarget$
    WHILE currentB$ = startB$
        FOR i = 1 TO cellsPerSide * cellsPerSide
            m$ = MID$(legalMoves$, INT(RND * LEN(legalMoves$)) + 1, 1): makeMove m$
        NEXT
    WEND
    makeTarget$ = currentB$
END FUNCTION

SUB cp (row, text$) 'center print at row
    LOCATE row, (80 - LEN(text$)) / 2: PRINT text$;
END SUB

SUB vcls 'print the screen to file then clear it
    DIM s$(23)
    FOR lines = 1 TO 23
        FOR t = 1 TO 80: scan$ = scan$ + CHR$(SCREEN(lines, t)): NEXT
        s$(lines) = RTRIM$(scan$): scan$ = ""
    NEXT
    FOR fini = 23 TO 1 STEP -1
        IF s$(fini) <> "" THEN EXIT FOR
    NEXT
    PRINT #3, ""
    FOR i = 1 TO fini: PRINT #3, s$(i): NEXT
    PRINT #3, "": PRINT #3, STRING$(80, "-"): CLS
END SUB

FUNCTION getKey$ 'just want printable characters
    k$ = ""
    WHILE LEN(k$) = 0
        k$ = INKEY$
        IF LEN(k$) THEN 'press something so respond
            IF LEN(k$) = 2 OR ASC(k$) > 126 OR ASC(k$) < 32 THEN k$ = "*": BEEP
        END IF
    WEND
    getKey$ = k$
END FUNCTION

```

'''Output:'''
<lang>
Flipping Bits Game, now with AI!  b+ 2017-12-18

         You will be presented with a square board marked Current and
           another marked Target. The object of the game is to match
           the Current board to Target in the least amount of moves.

            To make a move, enter a letter for a column to flip or
               a digit for a row to flip. In a flip, all 1's are
                  changed to 0's and all 0's changed to 1's.

                   You may enter 0 or q at any time to quit.
             You may press ? when prompted for move to get a hint.
             You may press ! to have the program solve the puzzle.

            Please press how many cells you want per side 2 to 9 > l
            Please press how many cells you want per side 2 to 9 > *
            Please press how many cells you want per side 2 to 9 > 3

--------------------------------------------------------------------------------

 Current:    Target:
    a b c       a b c
 1  1 1 0    1  0 1 1
 2  1 1 0    2  0 1 1
 3  0 0 1    3  0 1 1

 Number of moves taken so far is 0

 Press a lettered column or a numbered row to flip (or 0,q,?,!) > l
 Press a lettered column or a numbered row to flip (or 0,q,?,!) > 9
 Press a lettered column or a numbered row to flip (or 0,q,?,!) > *
 Press a lettered column or a numbered row to flip (or 0,q,?,!) > a

--------------------------------------------------------------------------------

 Current:    Target:
    a b c       a b c
 1  0 1 0    1  0 1 1
 2  0 1 0    2  0 1 1
 3  1 0 1    3  0 1 1

 Number of moves taken so far is 1

 Press a lettered column or a numbered row to flip (or 0,q,?,!) > ?
                                   Hint: c
 Press a lettered column or a numbered row to flip (or 0,q,?,!) > c

--------------------------------------------------------------------------------

 Current:    Target:
    a b c       a b c
 1  0 1 1    1  0 1 1
 2  0 1 1    2  0 1 1
 3  1 0 0    3  0 1 1

 Number of moves taken so far is 2

 Press a lettered column or a numbered row to flip (or 0,q,?,!) > l
 Press a lettered column or a numbered row to flip (or 0,q,?,!) > 9
 Press a lettered column or a numbered row to flip (or 0,q,?,!) > !

                   For the next move, the AI has chosen: 3
               Running the solution with 4 sec screen delays...

--------------------------------------------------------------------------------

 Current:    Target:
    a b c       a b c
 1  0 1 1    1  0 1 1
 2  0 1 1    2  0 1 1
 3  0 1 1    3  0 1 1

 Number of moves taken so far is 3

                 For the next move, the AI has chosen: Done?
               Running the solution with 4 sec screen delays...

--------------------------------------------------------------------------------

 Current:    Target:
    a b c       a b c
 1  0 1 1    1  0 1 1
 2  0 1 1    2  0 1 1
 3  0 1 1    3  0 1 1

 Number of moves taken so far is 3

                      Done! Current board matches Target
                Press y for yes, if you want to start over: > m

--------------------------------------------------------------------------------

```



## Racket

<lang>#lang racket
(define (flip-row! pzzl r)
  (define N (integer-sqrt (bytes-length pzzl)))
  (for* ((c (in-range N)))
    (define idx (+ c (* N r)))
    (bytes-set! pzzl idx (- 1 (bytes-ref pzzl idx)))))

(define (flip-col! pzzl c)
  (define N (integer-sqrt (bytes-length pzzl)))
  (for* ((r (in-range N)))
    (define idx (+ c (* N r)))
    (bytes-set! pzzl idx (- 1 (bytes-ref pzzl idx)))))

(define (new-game N (flips 10))
  (define N2 (sqr N))
  (define targ (list->bytes (for/list ((_ N2)) (random 2))))
  (define strt (bytes-copy targ))
  (for ((_ flips))
    (case (random 2)
      ((0) (flip-col! strt (random N)))
      ((1) (flip-row! strt (random N)))))
  (if (equal? strt targ) (new-game N) (values targ strt)))

(define (show-games #:sep (pzl-sep " | ") . pzzls)
  (define N (integer-sqrt (bytes-length (first pzzls))))
  (define caption (string-join (for/list ((c (in-range N))) (~a (add1 c))) ""))
  (define ruler   (string-join (for/list ((c (in-range N))) "-") ""))
  
  (define ((pzzle-row r) p)
    (string-join (for/list ((c (in-range N))) (~a (bytes-ref p (+ c (* N r))))) ""))
  
  (displayln
   (string-join
    (list*
     (format "  ~a" (string-join (for/list ((_ pzzls)) caption) pzl-sep))
     (format "  ~a" (string-join (for/list ((_ pzzls)) ruler) pzl-sep))
     (for/list ((r (in-range N)) (R (in-naturals (char->integer #\a))))
       (format "~a ~a" (integer->char R) (string-join (map (pzzle-row r) pzzls) pzl-sep))))
    "\n")))

(define (play N)
  (define-values (end start) (new-game N))
  (define (turn n (show? #t))
    (cond
      [(equal? end start) (printf "you won on turn #~a~%" n)]
      [else
       (when show? ;; don't show after whitespace
         (printf "turn #~a~%" n)       
         (show-games start end))
       (match (read-char)
         [(? eof-object?) (printf "sad to see you go :-(~%")]
         [(? char-whitespace?) (turn n #f)]
         [(? char-numeric? c)
          (define cnum (- (char->integer c) (char->integer #\1)))
          (cond [(< -1 cnum N)
                 (printf "flipping col ~a~%" (add1 cnum))
                 (flip-col! start cnum)
                 (turn (add1 n))]
                [else (printf "column number out of range ~a > ~a~%" (add1 cnum) N)
                      (turn n)])]
         [(? char-lower-case? c)
          (define rnum (- (char->integer c) (char->integer #\a)))
          (cond [(< -1 rnum N)
                 (printf "flipping row ~a~%" (add1 rnum))
                 (flip-row! start rnum)
                 (turn (add1 n))]
                [else (printf "row number out of range ~a > ~a~%" (add1 rnum) (sub1 N))
                      (turn n)])]
         [else (printf "unrecognised character in input: ~s~%" else)
               (turn n)])]))  
  (turn 0))
```


```txt
(play 3)
turn #0
  123 | 123
  --- | ---
a 001 | 101
b 110 | 101
c 100 | 000
1
flipping col 1
turn #1
  123 | 123
  --- | ---
a 101 | 101
b 010 | 101
c 000 | 000
b
flipping row 2
you won on turn #2
```



## REXX

This REXX version allows the specification (on the invocation line) for:
:::*   the size of the array (grid)   [the default is '''3''',   the maximum is '''26''']
:::*   the number of bits (for the target) to be flipped is the size of the grid   (the whole row or column)

Programming note:   none of the command line parameters   ('''N'''   and   '''u''')   are checked for errors (so as to make the 

program simpler).   A fair amount of coding was added to check for a legal "move".

```rexx
/*REXX program presents a  "flipping bit"  puzzle.  The user can solve via it via  C.L. */
parse arg  N u seed .                            /*get optional arguments from the C.L. */
if N==''  |  N==","     then N=3                 /*Size given?   Then use default of  3.*/
if u==''  |  u==","     then u=N                 /*the number of bits initialized to ON.*/
if datatype(seed, 'W')  then call random ,,seed  /*is there a seed (for repeatability?) */
col@= 'a b c d e f g h i j k l m n o p q r s t u v w x y z'     /*literal for column id.*/
cols=space(col@, 0);        upper cols           /*letters to be used for the columns.  */
@.=0;  !.=0                                      /*set both arrays to  "off" characters.*/
tries=0                                          /*number of player's attempts (so far).*/
         do  while  show(0) < u                  /* [↓]   turn  "on"  U  number of bits.*/
         r=random(1, N);      c=random(1, N)     /*get a random  row  and  column.      */
         @.r.c=1       ;      !.r.c=1            /*set (both)  row and column  to ON.   */
         end   /*while*/                         /* [↑]  keep going 'til   U   bits set.*/
oz=z                                             /*save the original array string.      */
call show 1, '   ◄═══target═══╣', , 1            /*display the target for user to attain*/
       do random(1,2); call flip 'R',random(1,N) /*flip a   row    of  bits.            */
                       call flip 'C',random(1,N) /*  "  "  column   "    "              */
       end   /*random*/                          /* [↑]  just perform  1  or  2  times. */
if z==oz  then call flip 'R', random(1, N)       /*ensure it's not target we're flipping*/
       do  until  z==oz;      call prompt        /*prompt until they get it right.      */
       call flip left(?, 1),  substr(?, 2)       /*flip a user selected row or column.  */
       call show 0                               /*get image (Z) of the updated array.  */
       end   /*until*/
call show 1, '   ◄───your array'                 /*display the array to the terminal.   */
say '─────────Congrats!    You did it in'     tries     "tries."
exit tries                                       /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
halt:  say 'program was halted by user.';  exit  /*the REXX program was halted by user. */
hdr:   aaa=arg(1);  if oo==1  then aaa=translate(aaa, "╔═║", '┌─│');    say aaa;    return
isInt: return datatype( arg(1), 'W')             /*returns   1   if  arg  is an integer.*/
isLet: return datatype( arg(1), 'M')             /*returns   1   if  arg  is a letter.  */
terr:  if ok  then say  '───────── ***error***:  illegal'  arg(1);        ok=0;     return
/*──────────────────────────────────────────────────────────────────────────────────────*/
flip: arg x,#;   do c=1  for N  while x=='R';  @.#.c = \@.#.c;  end  /*c*/
                 do r=1  for N  while x=='C';  @.r.# = \@.r.#;  end  /*r*/;         return
/*──────────────────────────────────────────────────────────────────────────────────────*/
prompt: if tries\==0  then  say '─────────bit array after play: '         tries
        signal on halt                           /*another method for the player to quit*/
        !='─────────Please enter a    row number    or a    column letter,      or  Quit:'
        call show 1, '   ◄───your array'         /*display the array to the terminal.   */
           do forever  until ok;       ok=1;     say;     say !;     pull ?  _  .  1  aa
           if abbrev('QUIT', ?, 1)     then do;  say '─────────quitting···';  exit 0;  end
           if ?==''                    then do;  call show 1,"   ◄═══target═══╣",.,1; ok=0
                                                 call show 1,"   ◄───your array"
                                            end                       /* [↑] reshow targ*/
           if _ \== ''                 then call terr 'too many args entered:'   aa
           if \isInt(?) & \isLet(?)    then call terr 'row/column: '             ?
           if  isLet(?)                then a=pos(?, cols)
           if  isLet(?) & (a<1 | a>N | length(?)>1)  then call terr 'column: '   ?
           if  isLet(?)                then ?='C'pos(?, cols)
           if  isInt(?) & (?<1 | ?>N)  then call terr 'row: '                    ?
           if  isInt(?)                then ?='R' || (?/1)            /*normalize number*/
           end   /*forever*/                                          /*end of da checks*/
        tries= tries + 1                                              /*bump da counter.*/
        return ?                                                      /*return response.*/
/*──────────────────────────────────────────────────────────────────────────────────────*/
show: $=0;              _=;            parse arg tell,tx,o,oo         /*$≡num of ON bits*/
      if tell  then do; say;     say '     '   subword(col@, 1, N)   "  column letter"
                        call hdr 'row ┌'copies('─', N+N+1)            /*prepend col hdrs*/
                    end                                               /* [↑]  grid hdrs.*/
      z=                                                              /* [↓] build grid.*/
        do   r=1  for N                                               /*show  grid rows.*/
          do c=1  for N;  if o==.  then do;  z=z || !.r.c;  _=_ !.r.c;  $=$ + !.r.c;   end
                                   else do;  z=z || @.r.c;  _=_ @.r.c;  $=$ + @.r.c;   end
          end   /*c*/                                                 /*··· and sum ONs.*/
        if tx\==''  then tar.r=_ tx                                   /*build da target?*/
        if tell     then call hdr right(r, 2)   ' │'_   tx; _=        /*show the grid?  */
        end     /*r*/                                                 /*show a grid row.*/
      if tell  then say;                     return $                 /*show blank line?*/
```

Note that the user's input is also shown   (annotated).

Also note that the 2<sup>nd</sup> answer was a blank (or nothing), which caused the program to re-show the target array. 

```txt

      a b c   column letter
row ╔═══════
 1  ║ 0 1 0    ◄═══target═══╣
 2  ║ 0 0 0    ◄═══target═══╣
 3  ║ 0 1 1    ◄═══target═══╣


      a b c   column letter
row ┌───────
 1  │ 1 0 0    ◄───your array
 2  │ 1 1 0    ◄───your array
 3  │ 0 1 0    ◄───your array


─────────Please enter a    row number    or a    column letter,      or  Quit:
b                                    ◄■■■■■■■■■■■■■ user input
─────────bit array after play:  1

      a b c   column letter
row ┌───────
 1  │ 1 1 0    ◄───your array
 2  │ 1 0 0    ◄───your array
 3  │ 0 0 0    ◄───your array


─────────Please enter a    row number    or a    column letter,      or  Quit:
a                                    ◄■■■■■■■■■■■■■ user input

─────────bit array after play:  2

      a b c   column letter
row ┌───────
 1  │ 0 1 0    ◄───your array
 2  │ 0 0 0    ◄───your array
 3  │ 1 0 0    ◄───your array


─────────Please enter a    row number    or a    column letter,      or  Quit:
3                                    ◄■■■■■■■■■■■■■ user input

      a b c   column letter
row ┌───────
 1  │ 0 1 0    ◄───your array
 2  │ 0 0 0    ◄───your array
 3  │ 0 1 1    ◄───your array

─────────Congrats!    You did it in 3 tries.

```

```txt

      a b c d e   column letter
row ╔═══════════
 1  ║ 1 0 0 0 0    ◄═══target═══╣
 2  ║ 1 0 0 0 0    ◄═══target═══╣
 3  ║ 1 0 0 0 0    ◄═══target═══╣
 4  ║ 0 0 0 0 0    ◄═══target═══╣
 5  ║ 0 0 1 1 0    ◄═══target═══╣


      a b c d e   column letter
row ┌───────────
 1  │ 0 0 1 1 0    ◄───your array
 2  │ 1 1 0 0 1    ◄───your array
 3  │ 1 1 0 0 1    ◄───your array
 4  │ 0 1 0 0 1    ◄───your array
 5  │ 0 1 1 1 1    ◄───your array


─────────Please enter a    row number    or a    column letter,      or  Quit:
q
─────────quitting···

```



## Ring


```ring

load "guilib.ring"
load "stdlib.ring"

size = 3
flip = newlist(size,size)
board = newlist(size,size)
colflip = list(size)
rowflip = list(size)

new qapp 
        {
        win1 = new qwidget() {
                  setwindowtitle("Flipping bits game")
                  setgeometry(465,115,800,600)
                  label1 = new qlabel(win1) {
                              setgeometry(285,60,120,40)
                              settext("Target")
                  }
                  label2 = new qlabel(win1) {
                              setgeometry(285,220,120,40)
                              settext("Board")
                  }
                  for n = 1 to size
                       for m = 1 to size
                            flip[n][m] = new qpushbutton(win1) {
                                             setgeometry(200+n*40,60+m*40,40,40)
                                             settext(string(random(1)))
                                             }
                       next
                  next
                  for n = 1 to size
                       for m = 1 to size
                            board[n][m] = new qpushbutton(win1) {
                                                 setgeometry(200+n*40,260+m*40,40,40)
                                                 setclickevent("draw(" + n + "," + m +")")
                                                 }
                       next
                  next
                  for n = 1 to size
                       colflip[n]= new qpushbutton(win1) {
                                              setgeometry(200+n*40,260,40,40)
                                              settext("Go")
                                              setclickevent("coldraw(" + n + ")")
                                              }
                  next
                  for n = 1 to size
                       rowflip[n]= new qpushbutton(win1) {
                                               setgeometry(200,260+n*40,40,40)
                                               settext("Go")
                                               setclickevent("rowdraw(" + n + ")")
                                               }
                  next
                  scramblebutton = new qpushbutton(win1) {
                                                   setgeometry(240,460,120,40)
                                                   settext("Scramble Board")
                                                   setclickevent("scramble(flip)")
                                                   }
        scramblebegin(flip)
        show()
        }
        exec()
        }

func coldraw(n)
        for row = 1 to size
             board[n][row] {temp = text()}
             if temp = "0"
                board[n][row].settext("1")
             else
                board[n][row].settext("0")
             ok
         next

func rowdraw(n)
        for col = 1 to size
             board[col][n] {temp = text()}
             if temp = "0"
                board[col][n].settext("1")
             else
                board[col][n].settext("0")
             ok
         next

func scramble(flip)
        for col = 1 to size
             for row = 1 to size
                  flip[col][row]{temp = text()}
                  board[col][row].settext(temp)
             next
        next
        for mix = 1 to size*10
             colorrow = random(1) + 1
             colrow = random(size-1) + 1
             if colorrow = 1
                rc = "coldraw"
             else
                rc = "rowdraw"
             ok
             go = rc + "(" + colrow + ")"
             eval(go)
        next

func scramblebegin(flip)
        for col = 1 to size
             for row = 1 to size
                  flip[col][row]{temp = text()}
                  board[col][row].settext(temp)
             next
        next

```

Output:

[https://1drv.ms/v/s!AqDUIunCqVnIg1Tbq1vXuLSg5B6L Flipping bits game]


## Ruby


```ruby
class FlipBoard
  def initialize(size)
    raise ArgumentError.new("Invalid board size: #{size}") if size < 2
    
    @size = size
    @board = Array.new(size**2, 0)
    
    randomize_board
    loop do
      @target = generate_target
      break unless solved?
    end 
    
    # these are used for validating user input
    @columns = [*'a'...('a'.ord+@size).chr]
    @rows = (1..@size).map(&:to_s)
  end
  
  ############################################################
  
  def play
    moves = 0
    puts "your target:", target
    
    until solved? 
      puts "", "move #{moves}:", self
      print "Row/column to flip: "
      ans = $stdin.gets.strip
      
      if @columns.include? ans
        flip_column @columns.index(ans)
        moves += 1
      elsif @rows.include? ans
        flip_row @rows.index(ans)
        moves += 1
      else
        puts "invalid input: " + ans
      end
    end
    
    puts "", "you solved the game in #{moves} moves", self
  end
  
  # the target formation as a string
  def target
    format_array @target
  end
  
  # the current formation as a string
  def to_s
    format_array @board
  end
  
  ############################################################
  private
  
  def solved?
    @board == @target
  end
  
  # flip a random number of bits on the board
  def randomize_board
    (@size + rand(@size)).times do
      flip_bit rand(@size), rand(@size)
    end
  end
  
  # generate a random number of flip_row/flip_column calls
  def generate_target
    orig_board = @board.clone
    (@size + rand(@size)).times do 
      rand(2).zero? ? flip_row( rand(@size) ) : flip_column( rand(@size) )
    end
    target, @board = @board, orig_board
    target
  end
  
  def flip_row(row)
    @size.times {|col| flip_bit(row, col)}
  end
  
  def flip_column(col)
    @size.times {|row| flip_bit(row, col)}
  end
  
  def flip_bit(row, col)
    @board[@size * row + col] ^= 1
  end
  
  def format_array(ary)
    str = "   " + @columns.join(" ") + "\n"
    @size.times do |row|
      str << "%2s " % @rows[row] + ary[@size*row, @size].join(" ") + "\n"
    end
    str
  end
end

######################################################################
begin
  FlipBoard.new(ARGV.shift.to_i).play
rescue => e
  puts e.message
end
```


Sample game:

```txt

$ ruby flipping_bits.rb 3
your target:
   a b c
 1 1 0 1
 2 0 1 1
 3 0 1 0

move 0:
   a b c
 1 0 0 1
 2 0 0 0
 3 0 0 1
Row/column to flip: 1

move 1:
   a b c
 1 1 1 0
 2 0 0 0
 3 0 0 1
Row/column to flip: b

move 2:
   a b c
 1 1 0 0
 2 0 1 0
 3 0 1 1
Row/column to flip: c

you solved the game in 3 moves
   a b c
 1 1 0 1
 2 0 1 1
 3 0 1 0

```



## Scala


### Java Swing Interoperability

```Scala
import java.awt.{BorderLayout, Color, Dimension, Font, Graphics, Graphics2D, Rectangle, RenderingHints}
import java.awt.event.{MouseAdapter, MouseEvent}

import javax.swing.{JFrame, JPanel}

object FlippingBitsGame extends App {

  class FlippingBitsGame extends JPanel {
    private val maxLevel: Int = 7
    private val box: Rectangle = new Rectangle(120, 90, 400, 400)

    private var n: Int = maxLevel

    private var grid: Array[Array[Boolean]] = _
    private var target: Array[Array[Boolean]] = _

    private var solved: Boolean = true

    override def paintComponent(gg: Graphics): Unit = {
      def drawGrid(g: Graphics2D): Unit = {
        if (solved) g.drawString("Solved! Click here to play again.", 180, 600)
        else g.drawString("Click next to a row or a column to flip.", 170, 600)
        val size: Int = box.width / n
        for {r <- 0 until n
             c <- 0 until n} {
          g.setColor(if (grid(r)(c)) Color.blue else Color.orange)
          g.fillRect(box.x + c * size, box.y + r * size, size, size)
          g.setColor(getBackground)
          g.drawRect(box.x + c * size, box.y + r * size, size, size)
          g.setColor(if (target(r)(c)) Color.blue else Color.orange)
          g.fillRect(7 + box.x + c * size, 7 + box.y + r * size, 10, 10)
        }
      }

      super.paintComponent(gg)
      val g: Graphics2D = gg.asInstanceOf[Graphics2D]
      g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
      drawGrid(g)
    }

    private def printGrid(msg: String, g: Array[Array[Boolean]]): Unit = {
      println(msg)
      for (row <- g) println(row.mkString(", "))
      println()
    }

    private def startNewGame(): Unit = {
      val rand = scala.util.Random

      if (solved) {
        val minLevel: Int = 3
        n = if (n == maxLevel) minLevel else n + 1
        grid = Array.ofDim[Boolean](n, n)
        target = Array.ofDim[Boolean](n, n)

        do {
          def shuffle(): Unit = for (i <- 0 until n * n)
            if (rand.nextBoolean()) flipRow(rand.nextInt(n)) else flipCol(rand.nextInt(n))

          shuffle()
          for (i <- grid.indices) grid(i).copyToArray(target(i)) //, n)
          shuffle()
        } while (solved(grid, target))
        solved = false
        printGrid("The target", target)
        printGrid("The board", grid)
      }
    }

    private def solved(a: Array[Array[Boolean]], b: Array[Array[Boolean]]): Boolean =
      a.indices.forall(i => a(i) sameElements b(i))

    private def flipRow(r: Int): Unit = for (c <- 0 until n) grid(r)(c) ^= true

    private def flipCol(c: Int): Unit = for (row <- grid) row(c) ^= true

    setPreferredSize(new Dimension(640, 640))
    setBackground(Color.white)
    setFont(new Font("SansSerif", Font.PLAIN, 18))
    startNewGame()
    addMouseListener(new MouseAdapter() {
      override def mousePressed(e: MouseEvent): Unit = {
        if (solved) startNewGame()
        else {
          val x: Int = e.getX
          val y: Int = e.getY
          if (box.contains(x, y)) return
          if (x > box.x && x < box.x + box.width) flipCol((x - box.x) / (box.width / n))
          else if (y > box.y && y < box.y + box.height) flipRow((y - box.y) / (box.height / n))
          solved = solved(grid, target)
          printGrid(if (solved) "Solved!" else "The board", grid)
        }
        repaint()
      }
    })

  }

  new JFrame("Flipping Bits Game") {
    add(new FlippingBitsGame(), BorderLayout.CENTER)
    pack()
    setDefaultCloseOperation(javax.swing.WindowConstants.EXIT_ON_CLOSE)
    setLocationRelativeTo(null)
    setResizable(false)
    setVisible(true)
  }

}
```

## Swift

```swift
import Foundation

struct Board: Equatable, CustomStringConvertible {
    let size: Int
    private var tiles: [Bool]

    init(size: Int) {
        self.size = size
        tiles = Array(count: size * size, repeatedValue: false)
    }

    subscript(x: Int, y: Int) -> Bool {
        get {
            return tiles[y * size + x]
        }
        set {
            tiles[y * size + x] = newValue
        }
    }

    mutating func randomize() {
        for i in 0..<tiles.count {
            tiles[i] = Bool(random() % 2)
        }
    }

    mutating func flipRow(row: Int) {
        for i in 0..<size {
            self[row, i] = !self[row, i]
        }
    }

    mutating func flipColumn(column: Int) {
        for i in 0..<size {
            self[i, column] = !self[i, column]
        }
    }

    var description: String {
        var desc = "\n\ta\tb\tc\n"
        for i in 0..<size {
            desc += "\(i+1):\t"
            for j in 0..<size {
                desc += "\(Int(self[i, j]))\t"
            }
            desc += "\n"
        }

        return desc
    }
}

func ==(lhs: Board, rhs: Board) -> Bool {
    return lhs.tiles == rhs.tiles
}

class FlippingGame: CustomStringConvertible {

    var board: Board
    var target: Board
    var solved: Bool { return board == target }

    init(boardSize: Int) {
        target = Board(size: 3)
        board = Board(size: 3)
        generateTarget()
    }

    func generateTarget() {
        target.randomize()
        board = target
        let size = board.size
        while solved {
            for _ in 0..<size + (random() % size + 1) {
                if random() % 2 == 0 {
                    board.flipColumn(random() % size)
                }
                else {
                    board.flipRow(random() % size)
                }
            }
        }
    }

    func getMove() -> Bool {
        print(self)
        print("Flip what? ", terminator: "")

        guard
            let move = readLine(stripNewline: true)
            where move.characters.count == 1
            else { return false }

        var moveValid = true

        if let row = Int(move) {
            board.flipRow(row - 1)
        }
        else if let column = move.lowercaseString.utf8.first where column < 100 && column > 96  {
            board.flipColumn(numericCast(column) - 97)
        }
        else {
            moveValid = false
        }

        return moveValid
    }

    var description: String {
        var str = ""
        print("Target: \n \(target)", toStream: &str)
        print("Board: \n \(board)", toStream: &str)

        return str
    }
}

func playGame(game: FlippingGame) -> String {
    game.generateTarget()
    var numMoves = 0
    while !game.solved {
        numMoves++
        print("Move #\(numMoves)")
        while !game.getMove() {}
    }
    print("You win!")
    print("Number of moves: \(numMoves)")
    print("\n\nPlay Again? ", terminator: "")

    return readLine(stripNewline: true)!.lowercaseString
}

let game = FlippingGame(boardSize: 3)
repeat { } while playGame(game) == "y"

```

```txt

Move #1
Target: 
 
	a	b	c
1:	1	1	0	
2:	0	0	1	
3:	1	0	1	

Board: 
 
	a	b	c
1:	0	1	0	
2:	1	0	1	
3:	0	0	1	


Flip what? a
You win!
Number of moves: 1


Play Again? n


```



## Tcl

```tcl
package require Tcl 8.6

oo::class create Flip {
    variable board target s
    constructor {size} {
	set s $size
	set target [my RandomConfiguration]
	set board $target
	while {$board eq $target} {
	    for {set i 0} {$i < $s} {incr i} {
		if {rand()<.5} {
		    my SwapRow $i
		}
		if {rand()<.5} {
		    my SwapColumn $i
		}
	    }
	}
    }

    method RandomConfiguration {{p 0.5}} {
	for {set row 0} {$row < $s} {incr row} {
	    set r {}
	    for {set col 0} {$col < $s} {incr col} {
		lappend r [expr {rand() < $p}]
	    }
	    lappend result $r
	}
	return $result
    }

    method SwapRow {rowId} {
	for {set i 0} {$i < $s} {incr i} {
	    lset board $rowId $i [expr {![lindex $board $rowId $i]}]
	}
    }
    method SwapColumn {columnId} {
	for {set i 0} {$i < $s} {incr i} {
	    lset board $i $columnId [expr {![lindex $board $i $columnId]}]
	}
    }

    method Render {configuration {prefixes {}}} {
	join [lmap r $configuration p $prefixes {
	    format %s%s $p [join [lmap c $r {string index ".X" $c}] ""]
	}] "\n"
    }
    method GetInput {prompt} {
	puts -nonewline "${prompt}: "
	flush stdout
	gets stdin
    }

    method play {} {
	set p0 {}
	set p {}
	set top [format "%*s " [string length $s] ""]
	for {set i 1;set j 97} {$i<=$s} {incr i;incr j} {
	    append top [format %c $j]
	    lappend p [format "%*d " [string length $s] $i]
	    lappend p0 [format "%*s " [string length $s] ""]
	}

	set moves 0
	puts "You are trying to get to:\n[my Render $target $p0]\n"
	while true {
	    puts "Current configuration (#$moves):\n$top\n[my Render $board $p]"

	    # Test for if we've won
	    if {$board eq $target} break

	    # Ask the user for a move
	    set i [my GetInput "Pick a column (letter) or row (number) to flip"]
 
	    # Parse the move and apply it
	    if {[string is lower -strict $i] && [set c [expr {[scan $i "%c"] - 97}]]<$s} {
		my SwapColumn $c
		incr moves
	    } elseif {[string is integer -strict $i] && $i>0 && $i<=$s} {
		my SwapRow [expr {$i - 1}]
		incr moves
	    } else {
		puts "Error: bad selection"
	    }
	    puts ""
	}
	puts "\nYou win! (You took $moves moves.)"
    }
}

Flip create flip 3
flip play

```

```txt

You are trying to get to:
  .XX
  XXX
  X.X

Current configuration (#0):
  abc
1 .X.
2 ..X
3 X..
Pick a column (letter) or row (number) to flip: 2

Current configuration (#1):
  abc
1 .X.
2 XX.
3 X..
Pick a column (letter) or row (number) to flip: c

Current configuration (#2):
  abc
1 .XX
2 XXX
3 X.X

You win! (You took 2 moves.)

```

