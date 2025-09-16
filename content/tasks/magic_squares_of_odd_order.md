+++
title = "Magic squares of odd order"
description = ""
date = 2019-06-15T07:05:12Z
aliases = []
[extra]
id = 17416
[taxonomies]
categories = ["task"]
tags = []
languages = [
  "360_assembly",
  "ada",
  "algol_68",
  "algol_w",
  "applescript",
  "applesoft_basic",
  "autohotkey",
  "awk",
  "basic",
  "batch_file",
  "bc",
  "befunge",
  "c",
  "common_lisp",
  "cpp",
  "d",
  "echolisp",
  "elixir",
  "erre",
  "factor",
  "fortran",
  "freebasic",
  "go",
  "haskell",
  "j",
  "java",
  "javascript",
  "jq",
  "julia",
  "kotlin",
  "liberty_basic",
  "lua",
  "mathematica",
  "maxima",
  "nim",
  "oforth",
  "pari_gp",
  "pascal",
  "perl",
  "perl_6",
  "phix",
  "pl_i",
  "purebasic",
  "python",
  "racket",
  "related_tasks",
  "rexx",
  "ring",
  "ruby",
  "rust",
  "scala",
  "seed7",
  "sidef",
  "swift",
  "tcl",
  "ubasic_4th",
  "vba",
  "vbscript",
  "visual_basic",
  "visual_basic_dotnet",
  "zkl",
]
+++

A magic square is an   <big>'''NxN'''</big>   square matrix whose numbers (usually integers) consist of consecutive numbers arranged so that the sum of each row and column,   ''and''   both long (main) diagonals are equal to the same sum (which is called the   ''magic number''   or   ''magic constant'').

The numbers are usually (but not always) the first   '''N'''<sup>2</sup>   positive integers.

A magic square whose rows and columns add up to a magic number but whose main diagonals do not, is known as a ''semimagic square''.

{| class="wikitable" style="float:right;border: 4px solid blue; background:lightgreen; color:black; margin-left:auto;margin-right:auto;text-align:center;width:15em;height:15em;table-layout:fixed;font-size:150%"
|-
| <big><big>'''8'''</big></big> || <big><big>'''1'''</big></big> || <big><big>'''6'''</big></big>
|-
| <big><big>'''3'''</big></big> || <big><big>'''5'''</big></big> || <big><big>'''7'''</big></big>
|-
| <big><big>'''4'''</big></big> || <big><big>'''9'''</big></big> || <big><big>'''2'''</big></big>
|}


## Task

For any odd   '''N''',   [[wp:Magic square#Method_for_constructing_a_magic_square_of_odd_order|generate a magic square]] with the integers   ''' 1''' ──► '''N''',   and show the results here.

Optionally, show the ''magic number''.

You should demonstrate the generator by showing at least a magic square for   '''N''' = '''5'''.


## Related tasks

* [[Magic squares of singly even order]]
* [[Magic squares of doubly even order]]




## See also

* MathWorld&trade; entry: [http://mathworld.wolfram.com/MagicSquare.html Magic_square]
* [http://www.1728.org/magicsq1.htm Odd Magic Squares (1728.org)]





## 360 Assembly

```360asm
*        Magic squares of odd order - 20/10/2015
MAGICS   CSECT
         USING  MAGICS,R15         set base register
         LA     R6,1               i=1
LOOPI    C      R6,N               do i=1 to n
         BH     ELOOPI
         LR     R8,R6              i
         SLA    R8,1               i*2
         LA     R9,PG              pgi=@pg
         LA     R7,1               j=1
LOOPJ    C      R7,N               do j=1 to n
         BH     ELOOPJ
         LR     R5,R8              i*2
         SR     R5,R7              -j
         A      R5,N               +n
         BCTR   R5,0               -1
         XR     R4,R4              clear high reg
         D      R4,N               /n
         LR     R5,R4              //n
         M      R4,N               *n
         LR     R2,R5              (i*2-j+n-1)//n*n
         LR     R5,R8              i*2
         AR     R5,R7              -j
         S      R5,=F'2'           -2
         XR     R4,R4              clear high reg
         D      R4,N               /n
         AR     R2,R4              +(i*2+j-2)//n
         LA     R2,1(R2)           +1
         XDECO  R2,PG+80           (i*2-j+n-1)//n*n+(i*2+j-2)//n+1
         MVC    0(5,R9),PG+87      put in buffer
         LA     R9,5(R9)           pgi=pgi+5
         LA     R7,1(R7)           j=j+1
         B      LOOPJ
ELOOPJ   XPRNT  PG,80
         LA     R6,1(R6)           i=i+1
         B      LOOPI
ELOOPI   XR     R15,R15            set return code
         BR     R14                return to caller
N        DC     F'9'               <== input
PG       DC     CL92' '            buffer
         YREGS
         END    MAGICS
```

```txt

    2   75   67   59   51   43   35   27   10
   22   14    6   79   71   63   46   38   30
   42   34   26   18    1   74   66   58   50
   62   54   37   29   21   13    5   78   70
   73   65   57   49   41   33   25   17    9
   12    4   77   69   61   53   45   28   20
   32   24   16    8   81   64   56   48   40
   52   44   36   19   11    3   76   68   60
   72   55   47   39   31   23   15    7   80

```



## Ada


```Ada
with Ada.Text_IO, Ada.Command_Line;

procedure Magic_Square is
   N: constant Positive := Positive'Value(Ada.Command_Line.Argument(1));

   subtype Constants is Natural range 1 .. N*N;
   package CIO is new Ada.Text_IO.Integer_IO(Constants);
   Undef: constant Natural := 0;

   subtype Index is Natural range 0 .. N-1;
   function Inc(I: Index) return Index is (if I = N-1 then 0 else I+1);
   function Dec(I: Index) return Index is (if I = 0 then N-1 else I-1);

   A: array(Index, Index) of Natural := (others => (others => Undef));
     -- initially undefined; at the end holding the magic square

   X: Index := 0; Y: Index := N/2; -- start position for the algorithm
begin
   for I in Constants loop -- write 1, 2, ..., N*N into the magic array
      A(X, Y) := I; -- write I into the magic array
      if A(Dec(X), Inc(Y)) = Undef then
	 X := Dec(X); Y := Inc(Y); -- go right-up
      else
	 X := Inc(X); -- go down
      end if;
   end loop;

   for Row in Index loop -- output the magic array
      for Collumn in Index loop
	 CIO.Put(A(Row, Collumn),
		 Width => (if N*N < 10 then 2 elsif N*N < 100 then 3 else 4));
      end loop;
      Ada.Text_IO.New_Line;
   end loop;
end Magic_Square;
```


```txt
>./magic_square 3
 8 1 6
 3 5 7
 4 9 2
>./magic_square 11
  68  81  94 107 120   1  14  27  40  53  66
  80  93 106 119  11  13  26  39  52  65  67
  92 105 118  10  12  25  38  51  64  77  79
 104 117   9  22  24  37  50  63  76  78  91
 116   8  21  23  36  49  62  75  88  90 103
   7  20  33  35  48  61  74  87  89 102 115
  19  32  34  47  60  73  86  99 101 114   6
  31  44  46  59  72  85  98 100 113   5  18
  43  45  58  71  84  97 110 112   4  17  30
  55  57  70  83  96 109 111   3  16  29  42
  56  69  82  95 108 121   2  15  28  41  54
```



## ALGOL W


```algolw
begin
    % construct a magic square of odd order - as a procedure can't return an %
    % array, the caller must supply one that is big enough                   %
    logical procedure magicSquare( integer array square ( *, * )
                                 ; integer value order
                                 ) ;
        if not odd( order ) or order < 1 then begin
            % can't make a magic square of the specified order               %
            false
            end
        else begin
            % order is OK - construct the square using de la Loubère's       %
            % algorithm as in the wikipedia page                             %

            % ensure a row/col position is on the square                     %
            integer procedure inSquare( integer value pos ) ;
                if pos < 1 then order else if pos > order then 1 else pos;
            % move "up" a row in the square                                  %
            integer procedure up   ( integer value row ) ; inSquare( row - 1 );
            % move "accross right" in the square                             %
            integer procedure right( integer value col ) ; inSquare( col + 1 );

            integer  row, col;
            % initialise square                                              %
            for i := 1 until order do for j := 1 until order do square( i, j ) := 0;

            % initial position is the middle of the top row                  %
            col := ( order + 1 ) div 2;
            row := 1;
            % construct square                                               %
            for i := 1 until ( order * order ) do begin
                square( row, col ) := i;
                if square( up( row ), right( col ) ) not = 0 then begin
                    % the up/right position is already taken, move down      %
                    row := row + 1;
                    end
                else begin
                    % can move up/right                                      %
                    row := up(    row );
                    col := right( col );
                end
            end for_i;
            % sucessful result                                               %
            true
        end magicSquare ;

    % prints the magic square                                                %
    procedure printSquare( integer array  square ( *, * )
                         ; integer value  order
                         ) ;
    begin
        integer sum, w;

        % set integer width to accomodate the largest number in the square   %
        w := ( order * order ) div 10;
        i_w := s_w := 1;
        while w > 0 do begin i_w := i_w + 1; w := w div 10 end;

        for i := 1 until order do sum := sum + square( 1, i );
        write( "maqic square of order ", order, ": sum: ", sum );
        for i := 1 until order do begin
            write( square( i, 1 ) );
            for j := 2 until order do writeon( square( i, j ) )
        end for_i

    end printSquare ;

    % test the magic square generation                                       %

    integer array sq ( 1 :: 11, 1 :: 11 );

    for i := 1, 3, 5, 7 do begin
        if magicSquare( sq, i ) then printSquare( sq, i )
                                else write( "can't generate square" );
    end for_i

end.
```

```txt

maqic square of order 1 : sum: 1
1
maqic square of order 3 : sum: 15
8 1 6
3 5 7
4 9 2
maqic square of order  5 : sum: 65
17 24  1  8 15
23  5  7 14 16
 4  6 13 20 22
10 12 19 21  3
11 18 25  2  9
maqic square of order  7 : sum: 175
30 39 48  1 10 19 28
38 47  7  9 18 27 29
46  6  8 17 26 35 37
 5 14 16 25 34 36 45
13 15 24 33 42 44  4
21 23 32 41 43  3 12
22 31 40 49  2 11 20

```



## ALGOL 68


```algol68
# construct a magic square of odd order                                      #
PROC magic square = ( INT order ) [,]INT:
    IF NOT ODD order OR order < 1
    THEN
        # can't make a magic square of the specified order                   #
        LOC [ 1 : 0, 1 : 0 ]INT
    ELSE
        # order is OK - construct the square using de la Loubère's           #
        # algorithm as in the wikipedia page                                 #

        [ 1 : order, 1 : order ]INT square;
        FOR i TO order DO FOR j TO order DO square[ i, j ] := 0 OD OD;

        # as square [ 1, 1 ] if the top-left, moving "up" reduces the row    #
        # operator to advance "up" the square                                #
        OP   PREV = ( INT pos )INT: IF pos = 1 THEN order ELSE pos - 1 FI;
        # operator to advance "across right" or "down" the square            #
        OP   NEXT = ( INT pos )INT: ( pos MOD order ) + 1;

        # fill in the square, starting from the middle of the top row        #
        INT col := ( order + 1 ) OVER 2;
        INT row := 1;
        FOR i TO order * order DO
            square[ row, col ] := i;
            IF square[ PREV row, NEXT col ] /= 0
            THEN
                # the up/right position is already taken, move down          #
                row := NEXT row
            ELSE
                # can move up and right                                      #
                row := PREV row;
                col := NEXT col
            FI
        OD;

        square
    FI # magic square # ;

# prints the magic square                                                    #
PROC print square = ( [,]INT square )VOID:
    BEGIN
        INT order = 1 UPB square;
        # calculate print width: negative so a leading "+" is not printed    #
        INT width := -1;
        INT mag   := order * order;
        WHILE mag >= 10 DO mag OVERAB 10; width MINUSAB 1 OD;
        # calculate the "magic sum"                                          #
        INT sum := 0;
        FOR i TO order DO sum +:= square[ 1, i ] OD;
        # print the square                                                   #
        print( ( "maqic square of order ", whole( order, 0 ), ": sum: ", whole( sum, 0 ), newline ) );
        FOR i TO order DO
            FOR j TO order DO write( ( " ", whole( square[ i, j ], width ) ) ) OD;
            write( ( newline ) )
        OD
    END # print square # ;

# test the magic square generation                                           #
FOR order BY 2 TO 7 DO print square( magic square( order ) ) OD
```

```txt

maqic square of order 1: sum: 1
 1
maqic square of order 3: sum: 15
 8 1 6
 3 5 7
 4 9 2
maqic square of order 5: sum: 65
 17 24  1  8 15
 23  5  7 14 16
  4  6 13 20 22
 10 12 19 21  3
 11 18 25  2  9
maqic square of order 7: sum: 175
 30 39 48  1 10 19 28
 38 47  7  9 18 27 29
 46  6  8 17 26 35 37
  5 14 16 25 34 36 45
 13 15 24 33 42 44  4
 21 23 32 41 43  3 12
 22 31 40 49  2 11 20

```




## AppleScript

Composing functions ( cycleRows . transpose . cycleRows ),
and lifting AppleScript handlers into first class script objects,
to allow for first class functions and closures.


```AppleScript
-- oddMagicSquare :: Int -> [[Int]]
on oddMagicSquare(n)
    cond((n mod 2) > 0, ¬
        cycleRows(transpose(cycleRows(table(n)))), ¬
        missing value)
end oddMagicSquare

-- TEST -----------------------------------------------------------------------
on run
    -- Orders 3, 5, 11

    -- wikiTableMagic :: Int -> String
    script wikiTableMagic
        on |λ|(n)
            formattedTable(oddMagicSquare(n))
        end |λ|
    end script

    intercalate(linefeed & linefeed, map(wikiTableMagic, {3, 5, 11}))
end run

-- table :: Int -> [[Int]]
on table(n)
    set lstTop to enumFromTo(1, n)

    script cols
        on |λ|(row)
            script rows
                on |λ|(x)
                    (row * n) + x
                end |λ|
            end script

            map(rows, lstTop)
        end |λ|
    end script

    map(cols, enumFromTo(0, n - 1))
end table

-- cycleRows :: [[a]] -> [[a]]
on cycleRows(lst)
    script rotationRow
        -- rotatedList :: [a] -> Int -> [a]
        on rotatedList(lst, n)
            if n = 0 then return lst

            set lng to length of lst
            set m to (n + lng) mod lng
            items -m thru -1 of lst & items 1 thru (lng - m) of lst
        end rotatedList

        on |λ|(row, i)
            rotatedList(row, (((length of row) + 1) div 2) - (i))
        end |λ|
    end script

    map(rotationRow, lst)
end cycleRows


-- GENERIC FUNCTIONS ----------------------------------------------------------

-- intercalate :: Text -> [Text] -> Text
on intercalate(strText, lstText)
    set {dlm, my text item delimiters} to {my text item delimiters, strText}
    set strJoined to lstText as text
    set my text item delimiters to dlm
    return strJoined
end intercalate

-- map :: (a -> b) -> [a] -> [b]
on map(f, xs)
    tell mReturn(f)
        set lng to length of xs
        set lst to {}
        repeat with i from 1 to lng
            set end of lst to |λ|(item i of xs, i, xs)
        end repeat
        return lst
    end tell
end map

-- Lift 2nd class handler function into 1st class script wrapper
-- mReturn :: Handler -> Script
on mReturn(f)
    if class of f is script then
        f
    else
        script
            property |λ| : f
        end script
    end if
end mReturn

-- enumFromTo :: Int -> Int -> [Int]
on enumFromTo(m, n)
    if m > n then
        set d to -1
    else
        set d to 1
    end if
    set lst to {}
    repeat with i from m to n by d
        set end of lst to i
    end repeat
    return lst
end enumFromTo

-- splitOn :: Text -> Text -> [Text]
on splitOn(strDelim, strMain)
    set {dlm, my text item delimiters} to {my text item delimiters, strDelim}
    set xs to text items of strMain
    set my text item delimiters to dlm
    return xs
end splitOn

-- transpose :: [[a]] -> [[a]]
on transpose(xss)
    script column
        on |λ|(_, iCol)
            script row
                on |λ|(xs)
                    item iCol of xs
                end |λ|
            end script

            map(row, xss)
        end |λ|
    end script

    map(column, item 1 of xss)
end transpose

-- WIKI DISPLAY ---------------------------------------------------------------

-- formattedTable :: [[Int]] -> String
on formattedTable(lstTable)
    set n to length of lstTable
    set w to 2.5 * n
    "magic(" & n & ")" & linefeed & linefeed & wikiTable(lstTable, ¬
        false, "text-align:center;width:" & ¬
        w & "em;height:" & w & "em;table-layout:fixed;")
end formattedTable

-- wikiTable :: [Text] -> Bool -> Text -> Text
on wikiTable(xs, blnHdr, strStyle)
    script wikiRows
        on |λ|(lstRow, iRow)
            set strDelim to cond(blnHdr and (iRow = 0), "!", "|")
            set strDbl to strDelim & strDelim
            linefeed & "|-" & linefeed & strDelim & space & ¬
                intercalate(space & strDbl & space, lstRow)
        end |λ|
    end script

    linefeed & "{| class=\"wikitable\" " & ¬
        cond(strStyle ≠ "", "style=\"" & strStyle & "\"", "") & ¬
        intercalate("", ¬
            map(wikiRows, xs)) & linefeed & "|}" & linefeed
end wikiTable

-- cond :: Bool -> a -> a -> a
on cond(bool, f, g)
    if bool then
        f
    else
        g
    end if
end cond
```

magic(3)

{| class="wikitable" style="text-align:center;width:7.5em;height:7.5em;table-layout:fixed;"
|-
| 8 || 3 || 4
|-
| 1 || 5 || 9
|-
| 6 || 7 || 2
|}

magic(5)

{| class="wikitable" style="text-align:center;width:12.5em;height:12.5em;table-layout:fixed;"
|-
| 17 || 23 || 4 || 10 || 11
|-
| 24 || 5 || 6 || 12 || 18
|-
| 1 || 7 || 13 || 19 || 25
|-
| 8 || 14 || 20 || 21 || 2
|-
| 15 || 16 || 22 || 3 || 9
|}

magic(11)

{| class="wikitable" style="text-align:center;width:27.5em;height:27.5em;table-layout:fixed;"
|-
| 68 || 80 || 92 || 104 || 116 || 7 || 19 || 31 || 43 || 55 || 56
|-
| 81 || 93 || 105 || 117 || 8 || 20 || 32 || 44 || 45 || 57 || 69
|-
| 94 || 106 || 118 || 9 || 21 || 33 || 34 || 46 || 58 || 70 || 82
|-
| 107 || 119 || 10 || 22 || 23 || 35 || 47 || 59 || 71 || 83 || 95
|-
| 120 || 11 || 12 || 24 || 36 || 48 || 60 || 72 || 84 || 96 || 108
|-
| 1 || 13 || 25 || 37 || 49 || 61 || 73 || 85 || 97 || 109 || 121
|-
| 14 || 26 || 38 || 50 || 62 || 74 || 86 || 98 || 110 || 111 || 2
|-
| 27 || 39 || 51 || 63 || 75 || 87 || 99 || 100 || 112 || 3 || 15
|-
| 40 || 52 || 64 || 76 || 88 || 89 || 101 || 113 || 4 || 16 || 28
|-
| 53 || 65 || 77 || 78 || 90 || 102 || 114 || 5 || 17 || 29 || 41
|-
| 66 || 67 || 79 || 91 || 103 || 115 || 6 || 18 || 30 || 42 || 54
|}


## AutoHotkey


```autohotkey

msgbox % OddMagicSquare(5)
msgbox % OddMagicSquare(7)
return

OddMagicSquare(oddN){
	sq := oddN**2
	obj := {}
	loop % oddN
		obj[A_Index] := {} 	; dis is row
	mid := Round((oddN+1)/2)
	sum := Round(sq*(sq+1)/2/oddN)
	obj[1][mid] := 1
	cR := 1 , cC := mid
	loop % sq-1
	{
		done := 0 , a := A_index+1
		while !done {
			nR := cR-1 , nC := cC+1
			if !nR
				nR := oddN
			if (nC>oddN)
				nC := 1
			if obj[nR][nC] 	;filled
				cR += 1
			else cR := nR , cC := nC
			if !obj[cR][cC]
				obj[cR][cC] := a , done := 1
		}
	}

	str := "Magic Constant for " oddN "x" oddN " is " sum "`n"
	for k,v in obj
	{
		for k2,v2 in v
			str .= " " v2
		str .= "`n"
	}
	return str
}

```

```txt

Magic Constant for 5x5 is 65
 17 24 1 8 15
 23 5 7 14 16
 4 6 13 20 22
 10 12 19 21 3
 11 18 25 2 9


Magic Constant for 7x7 is 175
 30 39 48 1 10 19 28
 38 47 7 9 18 27 29
 46 6 8 17 26 35 37
 5 14 16 25 34 36 45
 13 15 24 33 42 44 4
 21 23 32 41 43 3 12
 22 31 40 49 2 11 20

```



## AWK


```AWK

# syntax: GAWK -f MAGIC_SQUARES_OF_ODD_ORDER.AWK
BEGIN {
    build(5)
    build(3,1) # verify sum
    build(7)
    exit(0)
}
function build(n,check,  arr,i,width,x,y) {
    if (n !~ /^[0-9]*[13579]$/ || n < 3) {
      printf("error: %s is invalid\n",n)
      return
    }
    printf("\nmagic constant for %dx%d is %d\n",n,n,(n*n+1)*n/2)
    x = 0
    y = int(n/2)
    for (i=1; i<=(n*n); i++) {
      arr[x,y] = i
      if (arr[(x+n-1)%n,(y+n+1)%n]) {
        x = (x+n+1) % n
      }
      else {
        x = (x+n-1) % n
        y = (y+n+1) % n
      }
    }
    width = length(n*n)
    for (x=0; x<n; x++) {
      for (y=0; y<n; y++) {
        printf("%*s ",width,arr[x,y])
      }
      printf("\n")
    }
    if (check) { verify(arr,n) }
}
function verify(arr,n,  total,x,y) { # verify sum of each row, column and diagonal
    print("\nverify")
# horizontal
    for (x=0; x<n; x++) {
      total = 0
      for (y=0; y<n; y++) {
        printf("%d ",arr[x,y])
        total += arr[x,y]
      }
      printf("\t: %d row %d\n",total,x+1)
    }
# vertical
    for (y=0; y<n; y++) {
      total = 0
      for (x=0; x<n; x++) {
        printf("%d ",arr[x,y])
        total += arr[x,y]
      }
      printf("\t: %d column %d\n",total,y+1)
    }
# left diagonal
    total = 0
    for (x=y=0; x<n; x++ y++) {
      printf("%d ",arr[x,y])
      total += arr[x,y]
    }
    printf("\t: %d diagonal top left to bottom right\n",total)
# right diagonal
    x = n - 1
    total = 0
    for (y=0; y<n; y++ x--) {
      printf("%d ",arr[x,y])
      total += arr[x,y]
    }
    printf("\t: %d diagonal bottom left to top right\n",total)
}

```

```txt

magic constant for 5x5 is 65
17 24  1  8 15
23  5  7 14 16
 4  6 13 20 22
10 12 19 21  3
11 18 25  2  9

magic constant for 3x3 is 15
8 1 6
3 5 7
4 9 2

verify
8 1 6  : 15 row 1
3 5 7  : 15 row 2
4 9 2  : 15 row 3
8 3 4  : 15 column 1
1 5 9  : 15 column 2
6 7 2  : 15 column 3
8 5 2  : 15 diagonal top left to bottom right
4 5 6  : 15 diagonal bottom left to top right

magic constant for 7x7 is 175
30 39 48  1 10 19 28
38 47  7  9 18 27 29
46  6  8 17 26 35 37
 5 14 16 25 34 36 45
13 15 24 33 42 44  4
21 23 32 41 43  3 12
22 31 40 49  2 11 20

```



## BASIC

=
## Applesoft BASIC
=
Even if the code works for any odd number, N=9 is the maximum for a 40 column wide screen. Line <code>130</code> is a user defined modulo function, and <code>140</code> helps calculate the addends for the number that will go in the current position.

```Applesoft BASIC

  100 :
  110  REM  MAGIC SQUARE OF ODD ORDER
  120 :
  130  DEF FN MOD(A) = A -  INT (A / N) * N
  140  DEF FN NR(J) =  FN MOD((J + 2 * I + 1))
  200  INPUT "ENTER N: ";N
  210  IF N < 3 OR (N - INT (N / 2) * 2) = 0 GOTO 200
  220  FOR I = 0 TO (N - 1)
  230  FOR J = 0 TO (N - 1): HTAB 4 * (J + 1)
  240  PRINT N * FN NR(N - J - 1) + FN NR(J) + 1;
  250  NEXT J: PRINT
  260  NEXT I
  270  PRINT "MAGIC CONSTANT: ";N * (N * N + 1) / 2

```

```txt
ENTER N: 5
   2   23  19  15  6
   14  10  1   22  18
   21  17  13  9   5
   8   4   25  16  12
   20  11  7   3   24
MAGIC CONSTANT: 65
```


==={{header|IS-BASIC}}===
<lang IS-BASIC>100 PROGRAM "MagicN.bas"
110 DO
120   INPUT PROMPT "The square order: ":N
130 LOOP UNTIL MOD(N,2)>0 AND INT(N)=N AND N>0
140 FOR I=1 TO N
150   FOR J=1 TO N
160     PRINT USING " ###":MOD((I*2-J+N-1),N)*N+MOD(I*2+J-2,N)+1;
170   NEXT
180   PRINT
190 NEXT
200 PRINT "The magic number is:";N*(N^2+1)/2
```



## Batch File


```dos
@echo off
rem Magic squares of odd order
setlocal EnableDelayedExpansion
set n=9
echo The square order is: %n%
for /l %%i in (1,1,%n%) do (
    set w=
    for /l %%j in (1,1,%n%) do (
        set /a v1=%%i*2-%%j+n-1
        set /a v1=v1%%n*n
        set /a v2=%%i*2+%%j+n-2
        set /a v2=v2%%n
        set /a v=v1+v2+1
		set v=     !v!
	    set w=!w!!v:~-5!)
	echo !w!)
set /a w=n*(n*n+1)/2
echo The magic number is: %w%
pause
```

```txt
The square order is: 9
    2   75   67   59   51   43   35   27   10
   22   14    6   79   71   63   46   38   30
   42   34   26   18    1   74   66   58   50
   62   54   37   29   21   13    5   78   70
   73   65   57   49   41   33   25   17    9
   12    4   77   69   61   53   45   28   20
   32   24   16    8   81   64   56   48   40
   52   44   36   19   11    3   76   68   60
   72   55   47   39   31   23   15    7   80
The magic number is: 369
Press any key to continue ...
```




## bc

```bc
define magic_constant(n) {
    return(((n * n + 1) / 2) * n)
}

define print_magic_square(n) {
    auto i, x, col, row, len, old_scale

    old_scale = scale
    scale = 0
    len = length(n * n)

    print "Magic constant for n=", n, ": ", magic_constant(n), "\n"
    for (row = 1; row <= n; row++) {
        for (col = 1; col <= n; col++) {
            x = n * ((row + col - 1 + (n / 2)) % n) + \
                ((row + 2 * col - 2) % n) + 1
            for (i = 0; i < len - length(x); i++) {
                print " "
            }
            print x
            if (col != n) print " "
        }
        print "\n"
    }

    scale = old_scale
}

temp = print_magic_square(5)
```


```txt
Magic constant for n=5: 65
17 24  1  8 15
23  5  7 14 16
 4  6 13 20 22
10 12 19 21  3
11 18 25  2  9
```



## Befunge

The size, ''n'', is specified by the first value on the stack.

```befunge
500p0>
:::00g%00g\-1-\00g/2*+1+00g%00g*\:00g%v
@<$<_^#!-*:g00:,+9!%g00:+1.+1+%g00+1+*2/g00\<
```


```txt
2 	23 	19 	15 	6
14 	10 	1 	22 	18
21 	17 	13 	9 	5
8 	4 	25 	16 	12
20 	11 	7 	3 	24
```



## C

Generates an associative magic square.  If the size is larger than 3, the square is also [http://en.wikipedia.org/wiki/Pandiagonal_magic_square panmagic].

```c
#include <stdio.h>
#include <stdlib.h>

int f(int n, int x, int y)
{
	return (x + y*2 + 1)%n;
}

int main(int argc, char **argv)
{
	int i, j, n;

	//Edit: Add argument checking
	if(argc!=2) return 1;

	//Edit: Input must be odd and not less than 3.
	n = atoi(argv[1]);
	if (n < 3 || (n%2) == 0) return 2;

	for (i = 0; i < n; i++) {
		for (j = 0; j < n; j++)
			printf("% 4d", f(n, n - j - 1, i)*n + f(n, j, i) + 1);
		putchar('\n');
	}
	printf("\n Magic Constant: %d.\n", (n*n+1)/2*n);

	return 0;
}
```

```txt
$ ./magic 5
   2  23  19  15   6
  14  10   1  22  18
  21  17  13   9   5
   8   4  25  16  12
  20  11   7   3  24

 Magic Constant: 65.
```



## C++


```cpp

#include <iostream>
#include <sstream>
#include <iomanip>
using namespace std;

class magicSqr
{
public:
    magicSqr() { sqr = 0; }
    ~magicSqr() { if( sqr ) delete [] sqr; }

    void create( int d )
    {
        if( sqr ) delete [] sqr;
        if( !( d & 1 ) ) d++; sz = d;
        sqr = new int[sz * sz];
        memset( sqr, 0, sz * sz * sizeof( int ) );
        fillSqr();
    }

    void display()
    {
        cout << "Odd Magic Square: " << sz << " x " << sz << "\n";
        cout << "It's Magic Sum is: " << magicNumber() << "\n\n";
        ostringstream cvr; cvr << sz * sz;
        int l = cvr.str().size();

	for( int y = 0; y < sz; y++ )
	{
	    int yy = y * sz;
	    for( int x = 0; x < sz; x++ )
		cout << setw( l + 2 ) << sqr[yy + x];

	    cout << "\n";
	}
        cout << "\n\n";
    }

private:
    void fillSqr()
    {
	int sx = sz / 2, sy = 0, c = 0;
	while( c < sz * sz )
	{
	    if( !sqr[sx + sy * sz] )
	    {
		sqr[sx + sy * sz]= c + 1;
		inc( sx ); dec( sy );
		c++;
	    }
	    else
	    {
		dec( sx ); inc( sy ); inc( sy );
	    }
	}
    }

    int magicNumber()
    { return sz * ( ( sz * sz ) + 1 ) / 2; }

    void inc( int& a )
    { if( ++a == sz ) a = 0; }

    void dec( int& a )
    { if( --a < 0 ) a = sz - 1; }

    bool checkPos( int x, int y )
    { return( isInside( x ) && isInside( y ) && !sqr[sz * y + x] ); }

    bool isInside( int s )
    { return ( s < sz && s > -1 ); }

    int* sqr;
    int sz;
};

int main( int argc, char* argv[] )
{
    magicSqr s;
    s.create( 5 );
    s.display();
    return 0;
}

```

```txt

Odd Magic Square: 5 x 5
It's Magic Sum is: 65

  17  24   1   8  15
  23   5   7  14  16
   4   6  13  20  22
  10  12  19  21   3
  11  18  25   2   9

Odd Magic Square: 7 x 7
It's Magic Sum is: 175

  30  39  48   1  10  19  28
  38  47   7   9  18  27  29
  46   6   8  17  26  35  37
   5  14  16  25  34  36  45
  13  15  24  33  42  44   4
  21  23  32  41  43   3  12
  22  31  40  49   2  11  20

```



## Common Lisp


```lisp
(defun magic-square (n)
  (loop for i from 1 to n
        collect
          (loop for j from 1 to n
                collect
                  (+ (* n (mod (+ i j (floor n 2) -1)
                               n))
                     (mod (+ i (* 2 j) -2)
                          n)
                     1))))

(defun magic-constant (n)
  (* n
     (/ (1+ (* n n))
        2)))

(defun output (n)
  (format T "Magic constant for n=~a: ~a~%" n (magic-constant n))
  (let* ((size (length (write-to-string (* n n))))
         (format-str (format NIL "~~{~~{~~~ad~~^ ~~}~~%~~}~~%" size)))
    (format T format-str (magic-square n))))
```


```txt
> (output 5)
Magic constant for n=5: 65
17 24  1  8 15
23  5  7 14 16
 4  6 13 20 22
10 12 19 21  3
11 18 25  2  9
```



## D

```d
void main(in string[] args)
{
    import std.stdio, std.conv, std.range, std.algorithm, std.exception;

    immutable n = args.length == 2 ? args[1].to!uint : 5;
    enforce(n > 0 && n % 2 == 1, "Only odd n > 1");
    immutable len = text(n ^^ 2).length.text;
   // writeln(len);

    foreach (immutable r; 1 .. n + 1)
    {
        foreach (immutable c; 1 .. n + 1)
        {
            auto a = (n * ((r + c - 1 + (n / 2)) % n)) + ((r + (2 * c) - 2) % n) + 1;
            // n(( I + J - 1 + ( n / 2 ) ) mod n ) + (( I + 2J - 2 ) mod n ) + 1
//        writeln("n = ",n, " r = ",r," c = ",c, " a = ",a );
          writef("%" ~ len ~ "d%s",a, " ");
        }
        writeln("");
    }
    ;

    writeln("\nMagic constant: ", ((n * n + 1) * n) / 2);
}}
```

```txt
17 24  1  8 15
23  5  7 14 16
 4  6 13 20 22
10 12 19 21  3
11 18 25  2  9

Magic constant: 65
```



### Alternative Version

```d
import std.stdio, std.conv, std.string, std.range, std.algorithm;

uint[][] magicSquare(immutable uint n) pure nothrow @safe
in {
    assert(n > 0 && n % 2 == 1);
} out(mat) {
    // mat is square of the right size.
    assert(mat.length == n);
    assert(mat.all!(row => row.length == n));

    immutable magic = mat[0].sum;

    // The sum of all rows is the same magic number.
    assert(mat.all!(row => row.sum == magic));

    // The sum of all columns is the same magic number.
    //assert(mat.transposed.all!(col => col.sum == magic));
    assert(mat.dup.transposed.all!(col => col.sum == magic));

    // The sum of the main diagonals is the same magic number.
    assert(mat.enumerate.map!(ir => ir[1][ir[0]]).sum == magic);
    //assert(mat.enumerate.map!({i, r} => r[i]).sum == magic);
    assert(mat.enumerate.map!(ir => ir[1][ir[0]]).sum == magic);
} body {
    enum M = (in uint x) pure nothrow @safe @nogc => (x + n - 1) % n;
    auto m = new uint[][](n, n);

    uint i = 0;
    uint j = n / 2;
    foreach (immutable uint k; 1 .. n ^^ 2 + 1) {
        m[i][j] = k;
        if (m[M(i)][M(j)]) {
            i = (i + 1) % n;
        } else {
            i = M(i);
            j = M(j);
        }
    }

    return m;
}

void showSquare(in uint[][] m)
in {
    assert(m.all!(row => row.length == m[0].length));
} body {
    immutable maxLen = text(m.length ^^ 2).length.text;
    writefln("%(%(%" ~ maxLen ~ "d %)\n%)", m);
    writeln("\nMagic constant: ", m[0].sum);
}

int main(in string[] args) {
    if (args.length == 1) {
        5.magicSquare.showSquare;
        return 0;
    } else if (args.length == 2) {
        immutable n = args[1].to!uint;
        if (n > 0 && n % 2 == 1) {
            n.magicSquare.showSquare;
            return 0;
        }
    }

    stderr.writefln("Requires n odd and larger than 0.");
    return 1;
}
```

```txt
15  8  1 24 17
16 14  7  5 23
22 20 13  6  4
 3 21 19 12 10
 9  2 25 18 11

Magic constant: 65
```



## EchoLisp

The '''make-ms''' procedure allows to construct different magic squares for a same n, by modifying the grid filling moves. (see MathWorld reference)

```scheme

(lib 'matrix)

;; compute next i,j = f(move,i,j)
(define-syntax-rule (path imove jmove)
(begin (set! i (imove i n)) (set! j (jmove j n))))

;; We define the ordinary and break moves
;; (1 , -1), (0, 1) King's move
(define (inext i n) (modulo (1+ i) n))
(define (jnext j n) (modulo (1- j) n))
(define (ibreak i n) i)
(define (jbreak j n) (modulo (1+ j) n))

(define (make-ms n)
    (define n2+1 (1+ (* n n)))
    (define ms (make-array n n))
    (define i (quotient n 2))
    (define j 0)
    (array-set! ms i j 1)

    (for ((ns (in-range 2  n2+1)))
        (if (zero? (array-ref ms (inext i n ) (jnext j n )))
            (path inext jnext) ;; ordinary move if empty target
            (path ibreak jbreak)) ;; else break move

        (if (zero? (array-ref ms i j))
            (array-set! ms i j ns)
            (error ns "illegal path"))
    )
    (writeln 'order n 'magic-number  (/ ( * n n2+1) 2))
    (array-print ms))


```

```txt

(make-ms 7)
order     7     magic-number     175
  30   38   46   5    13   21   22
  39   47   6    14   15   23   31
  48   7    8    16   24   32   40
  1    9    17   25   33   41   49
  10   18   26   34   42   43   2
  19   27   35   36   44   3    11
  28   29   37   45   4    12   20

;; Changing the moves allow to generate other magic squares
;; (2 ,1) (1,-2) Knight's move !
(define (inext i n) (modulo (+ 2 i) n))
(define (jnext j n) (modulo (1+ j) n))
(define (ibreak i n) (modulo (1+ i) n))
(define (jbreak j n) (modulo (- j 2) n))
(make-ms 7)

order     7     magic-number     175
  37   48   3    14   18   22   33
  11   15   26   30   41   45   7
  34   38   49   4    8    19   23
  1    12   16   27   31   42   46
  24   35   39   43   5    9    20
  47   2    13   17   28   32   36
  21   25   29   40   44   6    10

;; (2 ,1) (1,-1)
(define (inext i n) (modulo (+ 2 i) n))
(define (jnext j n) (modulo (1+ j) n))
(define (ibreak i n) (modulo (1+ i) n))
(define (jbreak j n) (modulo (1- j) n))
(make-ms 7)

order     7     magic-number     175
  48   22   3    33   14   37   18
  30   11   41   15   45   26   7
  19   49   23   4    34   8    38
  1    31   12   42   16   46   27
  39   20   43   24   5    35   9
  28   2    32   13   36   17   47
  10   40   21   44   25   6    29

```




## Elixir

```elixir
defmodule RC do
  def odd_magic_square(n) when rem(n,2)==1 do
    for i <- 0..n-1 do
      for j <- 0..n-1, do: n * rem(i+j+1+div(n,2),n) + rem(i+2*j+2*n-5,n) + 1
    end
  end

  def print_square(sq) do
    width = List.flatten(sq) |> Enum.max |> to_char_list |> length
    fmt = String.duplicate(" ~#{width}w", length(sq)) <> "~n"
    Enum.each(sq, fn row -> :io.format fmt, row end)
  end
end

Enum.each([3,5,11], fn n ->
  IO.puts "\nSize #{n}, magic sum #{div(n*n+1,2)*n}"
  RC.odd_magic_square(n) |> RC.print_square
end)
```


```txt

Size 3, magic sum 15
 8 1 6
 3 5 7
 4 9 2

Size 5, magic sum 65
 16 23  5  7 14
 22  4  6 13 20
  3 10 12 19 21
  9 11 18 25  2
 15 17 24  1  8

Size 11, magic sum 671
  73  86  99 101 114   6  19  32  34  47  60
  85  98 100 113   5  18  31  44  46  59  72
  97 110 112   4  17  30  43  45  58  71  84
 109 111   3  16  29  42  55  57  70  83  96
 121   2  15  28  41  54  56  69  82  95 108
   1  14  27  40  53  66  68  81  94 107 120
  13  26  39  52  65  67  80  93 106 119  11
  25  38  51  64  77  79  92 105 118  10  12
  37  50  63  76  78  91 104 117   9  22  24
  49  62  75  88  90 103 116   8  21  23  36
  61  74  87  89 102 115   7  20  33  35  48

```



## ERRE


```ERRE

PROGRAM MAGIC_SQUARE

!$INTEGER

PROCEDURE Magicsq(size,filename$)

LOCAL DIM sq[25,25] ! array to hold square

IF (size AND 1)=0 OR size<3 THEN
     PRINT PRINT(CHR$(7)) ! beep
     PRINT("error: size is not odd or size is smaller then 3")
     PAUSE(3)
     EXIT PROCEDURE
END IF

! filename$ <> "" then save magic square in a file
! filename$ can contain directory name
! if filename$ exist it will be overwriten, no error checking

! start in the middle of the first row
   nr=1   x=size-(size DIV 2) y=1
   max=size*size

! create format string for using
   frmt$=STRING$(LEN(STR$(max)),"#")

! main loop for creating magic square
   REPEAT
      IF sq[x,y]=0 THEN
        sq[x,y]=nr
        IF nr MOD size=0 THEN
           y=y+1
         ELSE
           x=x+1
           y=y-1
        END IF
        nr=nr+1
      END IF
      IF x>size THEN
         x=1
         WHILE sq[x,y]<>0 DO
            x=x+1
         END WHILE
      END IF
      IF y<1 THEN
         y=size
         WHILE sq[x,y]<>0 DO
            y=y-1
         END WHILE
      END IF
   UNTIL nr>max

! printing square's bigger than 19 result in a wrapping of the line
   PRINT("Odd magic square size:";size;"*";size)
   PRINT("The magic sum =";((max+1) DIV 2)*size)
   PRINT

   FOR y=1 TO size DO
      FOR x=1 TO size DO
          WRITE(frmt$;sq[x,y];)
      END FOR
      PRINT
   END FOR

  ! output magic square to a file with the name provided
  IF filename$<>"" THEN
        OPEN("O",1,filename$)
           PRINT(#1,"Odd magic square size:";size;" *";size)
           PRINT(#1,"The magic sum =";((max+1) DIV 2)*size)
           PRINT(#1,)

           FOR y=1 TO size DO
             FOR x=1 TO size DO
               WRITE(#1,frmt$;sq[x,y];)
             END FOR
             PRINT(#1,)
           END FOR
  END IF
  CLOSE(1)

END PROCEDURE

BEGIN
PRINT(CHR$(12);)  ! CLS
Magicsq(5,"")
Magicsq(11,"")
!----------------------------------------------------
! the next line will also print the square to a file
! called 'magic_square_19txt'
!----------------------------------------------------
Magicsq(19,"msq_19.txt")

END PROGRAM

```

Same as FreeBasic version

```txt
Odd magic square size: 5 * 5        Odd magic square size: 11 * 11
The magic sum = 65                  The magic sum = 671

 17 24  1  8 15                       68  81  94 107 120   1  14  27  40  53  66
 23  5  7 14 16                       80  93 106 119  11  13  26  39  52  65  67
  4  6 13 20 22                       92 105 118  10  12  25  38  51  64  77  79
 10 12 19 21  3                      104 117   9  22  24  37  50  63  76  78  91
 11 18 25  2  9                      116   8  21  23  36  49  62  75  88  90 103
                                       7  20  33  35  48  61  74  87  89 102 115
                                      19  32  34  47  60  73  86  99 101 114   6
                                      31  44  46  59  72  85  98 100 113   5  18
                                      43  45  58  71  84  97 110 112   4  17  30
                                      55  57  70  83  96 109 111   3  16  29  42
Only the first 2 square shown.        56  69  82  95 108 121   2  15  28  41  54
```



## Factor

This solution uses the method from the paper linked in the J entry: http://www.jsoftware.com/papers/eem/magicsq.htm

```factor
USING: formatting io kernel math math.matrices math.ranges
sequences sequences.extras ;
IN: rosetta-code.magic-squares-odd

: inc-matrix ( n -- matrix )
    [ 0 ] dip dup [ 1 + dup ] make-matrix nip ;

: rotator ( n -- seq ) 2/ dup [ neg ] dip [a,b] ;

: odd-magic-square ( n -- matrix )
    [ inc-matrix ] [ rotator [ rotate ] 2map flip ] dup tri ;

: show-square ( n -- )
    dup "Order: %d\n" printf odd-magic-square dup
    [ [ "%4d" printf ] each nl ] each first sum
    "Magic number: %d\n\n" printf ;

3 5 11 [ show-square ] tri@
```

```txt

Order: 3
   8   1   6
   3   5   7
   4   9   2
Magic number: 15

Order: 5
  17  24   1   8  15
  23   5   7  14  16
   4   6  13  20  22
  10  12  19  21   3
  11  18  25   2   9
Magic number: 65

Order: 11
  68  81  94 107 120   1  14  27  40  53  66
  80  93 106 119  11  13  26  39  52  65  67
  92 105 118  10  12  25  38  51  64  77  79
 104 117   9  22  24  37  50  63  76  78  91
 116   8  21  23  36  49  62  75  88  90 103
   7  20  33  35  48  61  74  87  89 102 115
  19  32  34  47  60  73  86  99 101 114   6
  31  44  46  59  72  85  98 100 113   5  18
  43  45  58  71  84  97 110 112   4  17  30
  55  57  70  83  96 109 111   3  16  29  42
  56  69  82  95 108 121   2  15  28  41  54
Magic number: 671

```



## Fortran

```fortran
program Magic_Square
  implicit none

  integer, parameter :: order = 15
  integer :: i, j

  write(*, "(a, i0)") "Magic Square Order: ", order
  write(*, "(a)")     "----------------------"
  do i = 1, order
    do j = 1, order
      write(*, "(i4)", advance = "no") f1(order, i, j)
    end do
    write(*,*)
  end do
  write(*, "(a, i0)") "Magic number = ", f2(order)

contains

integer function f1(n, x, y)
  integer, intent(in) :: n, x, y

  f1 = n * mod(x + y - 1 + n/2, n) + mod(x + 2*y - 2, n) + 1
end function

integer function f2(n)
  integer, intent(in) :: n

  f2 = n * (1 + n * n) / 2
end function
end program
```

Output:

```txt
Magic Square Order: 15
----------------------
 122 139 156 173 190 207 224   1  18  35  52  69  86 103 120
 138 155 172 189 206 223  15  17  34  51  68  85 102 119 121
 154 171 188 205 222  14  16  33  50  67  84 101 118 135 137
 170 187 204 221  13  30  32  49  66  83 100 117 134 136 153
 186 203 220  12  29  31  48  65  82  99 116 133 150 152 169
 202 219  11  28  45  47  64  81  98 115 132 149 151 168 185
 218  10  27  44  46  63  80  97 114 131 148 165 167 184 201
   9  26  43  60  62  79  96 113 130 147 164 166 183 200 217
  25  42  59  61  78  95 112 129 146 163 180 182 199 216   8
  41  58  75  77  94 111 128 145 162 179 181 198 215   7  24
  57  74  76  93 110 127 144 161 178 195 197 214   6  23  40
  73  90  92 109 126 143 160 177 194 196 213   5  22  39  56
  89  91 108 125 142 159 176 193 210 212   4  21  38  55  72
 105 107 124 141 158 175 192 209 211   3  20  37  54  71  88
 106 123 140 157 174 191 208 225   2  19  36  53  70  87 104
Magic number = 1695
```


## FreeBASIC


```FreeBASIC
' version 23-06-2015
' compile with: fbc -s console

Sub magicsq(size As Integer, filename As String ="")

    If (size And 1) = 0 Or size < 3 Then
        Print : Beep ' alert
        Print "error: size is not odd or size is smaller then 3"
        Sleep 3000,1  'wait 3 seconds, ignore key press
        Exit Sub
    End If

    ' filename <> "" then save magic square in a file
    ' filename can contain directory name
    ' if filename exist it will be overwriten, no error checking

    Dim As Integer sq(size,size) ' array to hold square
    ' start in the middle of the first row
    Dim As Integer nr = 1, x = size - (size \ 2), y = 1
    Dim As Integer max = size * size
    ' create format string for using
    Dim As String frmt = String(Len(Str(max)) +1, "#")

    ' main loop for creating magic square
    Do
        If sq(x, y) = 0 Then
            sq(x, y) = nr
            If nr Mod size = 0 Then
                y += 1
            Else
                x += 1
                y -= 1
            End If
            nr += 1
        End If
        If x > size Then
            x = 1
            Do While sq(x,y) <> 0
                x += 1
            Loop
        End If
        If y < 1 Then
            y = size
            Do While sq(x,y) <> 0
                y -= 1
            Loop
        EndIf
    Loop Until nr > max

    ' printing square's bigger than 19 result in a wrapping of the line
    Print "Odd magic square size:"; size; " *"; size
    Print "The magic sum ="; ((max +1) \ 2) * size
    Print

    For y = 1 To size
        For x = 1 To size
            Print Using frmt; sq(x,y);
        Next
        Print
    Next
    print

    ' output magic square to a file with the name provided
    If filename <> "" Then
        nr = FreeFile
        Open filename For Output As #nr
        Print #nr, "Odd magic square size:"; size; " *"; size
        Print #nr, "The magic sum ="; ((max +1) \ 2) * size
        Print #nr,

        For y = 1 To size
            For x = 1 To size
                Print #nr, Using frmt; sq(x,y);
            Next
            Print #nr,
        Next
    End If
    Close

End Sub

' ------=< MAIN >=------

magicsq(5)
magicsq(11)
' the next line will also print the square to a file called: magic_square_19.txt
magicsq(19, "magic_square_19.txt")


' empty keyboard buffer
While Inkey <> "" : Wend
Print : Print "hit any key to end program"
Sleep
End
```

```txt
Odd magic square size: 5 * 5        Odd magic square size: 11 * 11
The magic sum = 65                  The magic sum = 671

 17 24  1  8 15                       68  81  94 107 120   1  14  27  40  53  66
 23  5  7 14 16                       80  93 106 119  11  13  26  39  52  65  67
  4  6 13 20 22                       92 105 118  10  12  25  38  51  64  77  79
 10 12 19 21  3                      104 117   9  22  24  37  50  63  76  78  91
 11 18 25  2  9                      116   8  21  23  36  49  62  75  88  90 103
                                       7  20  33  35  48  61  74  87  89 102 115
                                      19  32  34  47  60  73  86  99 101 114   6
                                      31  44  46  59  72  85  98 100 113   5  18
                                      43  45  58  71  84  97 110 112   4  17  30
                                      55  57  70  83  96 109 111   3  16  29  42
Only the first 2 square shown.        56  69  82  95 108 121   2  15  28  41  54
```



## Go

```go
package main

import (
    "fmt"
    "log"
)

func ms(n int) (int, []int) {
    M := func(x int) int { return (x + n - 1) % n }
    if n <= 0 || n&1 == 0 {
        n = 5
        log.Println("forcing size", n)
    }
    m := make([]int, n*n)
    i, j := 0, n/2
    for k := 1; k <= n*n; k++ {
        m[i*n+j] = k
        if m[M(i)*n+M(j)] != 0 {
            i = (i + 1) % n
        } else {
            i, j = M(i), M(j)
        }
    }
    return n, m
}

func main() {
    n, m := ms(5)
    i := 2
    for j := 1; j <= n*n; j *= 10 {
        i++
    }
    f := fmt.Sprintf("%%%dd", i)
    for i := 0; i < n; i++ {
        for j := 0; j < n; j++ {
            fmt.Printf(f, m[i*n+j])
        }
        fmt.Println()
    }
}
```

```txt

  15   8   1  24  17
  16  14   7   5  23
  22  20  13   6   4
   3  21  19  12  10
   9   2  25  18  11
```



## Haskell


### =Translating imperative code=

```haskell
-- as a translation from imperative code, this is probably not a "good" implementation
import Data.List

type Var = (Int, Int, Int, Int) -- sx sy sz c

magicSum :: Int -> Int
magicSum x = ((x * x + 1) `div` 2) * x

wrapInc :: Int -> Int -> Int
wrapInc max x
   | x + 1 == max    = 0
   | otherwise       = x + 1

wrapDec :: Int -> Int -> Int
wrapDec max x
   | x == 0    = max - 1
   | otherwise = x - 1

isZero :: [[Int]] -> Int -> Int -> Bool
isZero m x y = m !! x !! y == 0

setAt :: (Int,Int) -> Int -> [[Int]] -> [[Int]]
setAt (x, y) val table
   | (upper, current : lower) <- splitAt x table,
     (left, this : right) <- splitAt y current
         = upper ++ (left ++ val : right) : lower
   | otherwise = error "Outside"

create :: Int -> [[Int]]
create x = replicate x $ replicate x 0

cells :: [[Int]] -> Int
cells m = x*x where x = length m

fill :: Var -> [[Int]] -> [[Int]]
fill (sx, sy, sz, c) m
   | c < cells m =
      if isZero m sx sy
      then fill ((wrapInc sz sx), (wrapDec sz sy), sz, c + 1) (setAt (sx, sy) (c + 1) m)
      else fill ((wrapDec sz sx), (wrapInc sz(wrapInc sz sy)), sz, c) m
   | otherwise = m

magicNumber :: Int -> [[Int]]
magicNumber d = transpose $ fill (d `div` 2, 0, d, 0) (create d)

display :: [[Int]] -> String
display (x:xs)
   | null xs = vdisplay x
   | otherwise = vdisplay x ++ ('\n' : display xs)

vdisplay :: [Int] -> String
vdisplay (x:xs)
   | null xs = show x
   | otherwise = show x ++ " " ++ vdisplay xs


magicSquare x = do
   putStr "Magic Square of "
   putStr $ show x
   putStr " = "
   putStrLn $ show $ magicSum x
   putStrLn $ display $ magicNumber x
```



### =Transpose . cycled=


Defining the magic square as two applications of ('''transpose . cycled''') to a simply ordered square.


```Haskell
import Data.List (transpose, maximumBy)
import Data.List.Split (chunksOf)
import Data.Ord (comparing)

magicSquare :: Int -> [[Int]]
magicSquare n
  | 1 == mod n 2 = applyN 2 (transpose . cycled) $ plainSquare n
  | otherwise = []

-- TEST ---------------------------------------------------
main :: IO ()
main = mapM_ putStrLn $ (showSquare . magicSquare) <$> [3, 5, 7]

-- GENERIC ------------------------------------------------
applyN :: Int -> (a -> a) -> a -> a
applyN n f = foldr (.) id (replicate n f)

cycled :: [[Int]] -> [[Int]]
cycled rows =
  let n = length rows
      d = quot n 2
  in zipWith
       (\d xs -> take n $ drop (n - d) (cycle xs))
       [d,subtract 1 d .. -d]
       rows

plainSquare :: Int -> [[Int]]
plainSquare = chunksOf <*> enumFromTo 1 . (^ 2)

-- FORMATTING ---------------------------------------------
justifyRight :: Int -> a -> [a] -> [a]
justifyRight n c = (drop . length) <*> (replicate n c ++)

showSquare
  :: Show a
  => [[a]] -> String
showSquare rows =
  let srows = fmap show <$> rows
      w = 1 + maximum (length <$> concat srows)
  in unlines $ concatMap (justifyRight w ' ') <$> srows
```

```txt
 8 1 6
 3 5 7
 4 9 2

 17 24  1  8 15
 23  5  7 14 16
  4  6 13 20 22
 10 12 19 21  3
 11 18 25  2  9

 30 39 48  1 10 19 28
 38 47  7  9 18 27 29
 46  6  8 17 26 35 37
  5 14 16 25 34 36 45
 13 15 24 33 42 44  4
 21 23 32 41 43  3 12
 22 31 40 49  2 11 20
```



### =Siamese method=

Encoding the traditional [[wp:Siamese_method|'Siamese' method]]


```haskell
import qualified Data.Map.Strict as M
import Control.Monad (forM_)
import Data.Maybe (isJust, fromJust)
import Data.List (transpose, intercalate)

magic :: Int -> [[Int]]
magic = mapAsTable <*> siamMap

-- SIAMESE METHOD FUNCTIONS ----------------------------------------------------

-- Highest zero-based index of grid -> 'Siamese' indices keyed by coordinates
siamMap :: Int -> M.Map (Int, Int) Int
siamMap n =
  if odd n
    then let h = quot n 2
             uBound = n - 1
             sPath uBound sMap (x, y) h =
               let newMap = M.insert (x, y) h sMap
               in if y == uBound && x == quot uBound 2
                    then newMap
                    else sPath
                           uBound
                           newMap
                           (nextSiam uBound sMap (x, y))
                           (h + 1)
         in sPath uBound (M.fromList []) (quot uBound 2, 0) 1
    else M.fromList []

-- Highest index of square -> Siam xys so far -> xy -> next xy coordinate
nextSiam :: Int -> M.Map (Int, Int) Int -> (Int, Int) -> (Int, Int)
nextSiam uBound sMap (x, y) =
  let alt (a, b)
        | a > uBound && b < 0 = (uBound, 1) -- Top right corner ?
        | a > uBound = (0, b) -- beyond right edge ?
        | b < 0 = (a, uBound) -- above top edge ?
        | isJust (M.lookup (a, b) sMap) = (a - 1, b + 2) -- already filled ?
        | otherwise = (a, b) -- Up one, right one.
  in alt (x + 1, y - 1)

-- DISPLAY AND TEST FUNCTIONS --------------------------------------------------

-- Size of square -> integers keyed by coordinates -> rows of integers
mapAsTable :: Int -> M.Map (Int, Int) Int -> [[Int]]
mapAsTable nCols xyMap =
  let axis = [0 .. nCols - 1]
  in fmap (fromJust . flip M.lookup xyMap) <$>
     (axis >>= \y -> [axis >>= \x -> [(x, y)]])

checked :: [[Int]] -> (Int, Bool)
checked square =
  let diagonals =
        fmap (flip (zipWith (!!)) [0 ..]) . ((:) <*> (return . reverse))
      h:t = sum <$> square ++ transpose square ++ diagonals square
  in (h, all (h ==) t)

table :: String -> [[String]] -> [String]
table delim rows =
  let justifyRight c n s = drop (length s) (replicate n c ++ s)
  in intercalate delim <$>
     transpose
       ((fmap =<< justifyRight ' ' . maximum . fmap length) <$> transpose rows)

main :: IO ()
main =
  forM_ [3, 5, 7] $
  \n -> do
    let test = magic n
    putStrLn $ unlines (table " " (fmap show <$> test))
    print $ checked test
    putStrLn ""
```

```txt
8 1 6
3 5 7
4 9 2

(15,True)

17 24  1  8 15
23  5  7 14 16
 4  6 13 20 22
10 12 19 21  3
11 18 25  2  9

(65,True)

30 39 48  1 10 19 28
38 47  7  9 18 27 29
46  6  8 17 26 35 37
 5 14 16 25 34 36 45
13 15 24 33 42 44  4
21 23 32 41 43  3 12
22 31 40 49  2 11 20

(175,True)
```


==Icon and {{header|Unicon}}==

This is a Unicon-specific solution because of the use of the <tt>[: ... :]</tt> construct.

```unicon
procedure main(A)
    n := integer(!A) | 3
    write("Magic number: ",n*(n*n+1)/2)
    sq := buildSquare(n)
    showSquare(sq)
end

procedure buildSquare(n)
    sq := [: |list(n)\n :]
    r := 0
    c := n/2
    every i := !(n*n) do {
        /sq[r+1,c+1] := i
        nr := (n+r-1)%n
        nc := (c+1)%n
        if /sq[nr+1,nc+1] then (r := nr,c := nc) else r := (r+1)%n
        }
    return sq
end

procedure showSquare(sq)
    n := *sq
    s := *(n*n)+2
    every r := !sq do every writes(right(!r,s)|"\n")
end
```


```txt

->ms 5
Magic number: 65
  17  24   1   8  15
  23   5   7  14  16
   4   6  13  20  22
  10  12  19  21   3
  11  18  25   2   9
->

```



## J


Based on http://www.jsoftware.com/papers/eem/magicsq.htm


```J
ms=: i:@<.@-: |."0 1&|:^:2 >:@i.@,~
```


In other words, generate a square of counting integers, like this:

```j
>
:@i.@,~ 3
1 2 3
4 5 6
7 8 9
```


Then generate a list of integers centering on 0 up to half of that value, like this:

```J
   i:@<.@-: 3
_1 0 1
```


Finally, rotate each corresponding row and column of the table by the corresponding value in the list. We can use the same instructions to rotate both rows and columns if we transpose the matrix before rotating (and perform this transpose+rotate twice).

Example use:


```J
   ms 5
 9 15 16 22  3
20 21  2  8 14
 1  7 13 19 25
12 18 24  5  6
23  4 10 11 17
   ~.+/ms 5
65
   ~.+/ms 101
515201
```



## Java


```java
public class MagicSquare {

    public static void main(String[] args) {
        int n = 5;
        for (int[] row : magicSquareOdd(n)) {
            for (int x : row)
                System.out.format("%2s ", x);
            System.out.println();
        }
        System.out.printf("\nMagic constant: %d ", (n * n + 1) * n / 2);
    }

    public static int[][] magicSquareOdd(final int base) {
        if (base % 2 == 0 || base < 3)
            throw new IllegalArgumentException("base must be odd and > 2");

        int[][] grid = new int[base][base];
        int r = 0, number = 0;
        int size = base * base;

        int c = base / 2;
        while (number++ < size) {
            grid[r][c] = number;
            if (r == 0) {
                if (c == base - 1) {
                    r++;
                } else {
                    r = base - 1;
                    c++;
                }
            } else {
                if (c == base - 1) {
                    r--;
                    c = 0;
                } else {
                    if (grid[r - 1][c + 1] == 0) {
                        r--;
                        c++;
                    } else {
                        r++;
                    }
                }
            }
        }
        return grid;
    }
}
```


```txt
17 24  1  8 15
23  5  7 14 16
 4  6 13 20 22
10 12 19 21  3
11 18 25  2  9

Magic constant: 65
```




## JavaScript



### ES5

( and referring to http://www.jsoftware.com/papers/eem/magicsq.htm )


```JavaScript
(function () {

  // n -> [[n]]
  function magic(n) {
    return n % 2 ? rotation(
      transposed(
        rotation(
          table(n)
        )
      )
    ) : null;
  }

  // [[a]] -> [[a]]
  function rotation(lst) {
    return lst.map(function (row, i) {
      return rotated(
        row, ((row.length + 1) / 2) - (i + 1)
      );
    })
  }

  // [[a]] -> [[a]]
  function transposed(lst) {
    return lst[0].map(function (col, i) {
      return lst.map(function (row) {
        return row[i];
      })
    });
  }

  // [a] -> n -> [a]
  function rotated(lst, n) {
    var lng = lst.length,
      m = (typeof n === 'undefined') ? 1 : (
        n < 0 ? lng + n : (n > lng ? n % lng : n)
      );

    return m ? (
      lst.slice(-m).concat(lst.slice(0, lng - m))
    ) : lst;
  }

  // n -> [[n]]
  function table(n) {
    var rngTop = rng(1, n);

    return rng(0, n - 1).map(function (row) {
      return rngTop.map(function (x) {
        return row * n + x;
      });
    });
  }

  // [m..n]
  function rng(m, n) {
    return Array.apply(null, Array(n - m + 1)).map(
      function (x, i) {
        return m + i;
      });
  }

  /******************** TEST WITH 3, 5, 11 ***************************/

  // Results as right-aligned wiki tables
  function wikiTable(lstRows, blnHeaderRow, strStyle) {
    var css = strStyle ? 'style="' + strStyle + '"' : '';

    return '{| class="wikitable" ' + css + lstRows.map(
      function (lstRow, iRow) {
        var strDelim = ((blnHeaderRow && !iRow) ? '!' : '|'),
          strDbl = strDelim + strDelim;

        return '\n|-\n' + strDelim + ' ' + lstRow.join(' ' + strDbl + ' ');
      }).join('') + '\n|}';
  }

  return [3, 5, 11].map(
    function (n) {
      var w = 2.5 * n;
      return 'magic(' + n + ')\n\n' + wikiTable(
        magic(n), false, 'text-align:center;width:' + w + 'em;height:' + w + 'em;table-layout:fixed;'
      )
    }
  ).join('\n\n')
})();
```


Output:

magic(3)

{| class="wikitable" style="text-align:center;width:7.5em;height:7.5em;table-layout:fixed;"
|-
| 8 || 3 || 4
|-
| 1 || 5 || 9
|-
| 6 || 7 || 2
|}

magic(5)

{| class="wikitable" style="text-align:center;width:12.5em;height:12.5em;table-layout:fixed;"
|-
| 17 || 23 || 4 || 10 || 11
|-
| 24 || 5 || 6 || 12 || 18
|-
| 1 || 7 || 13 || 19 || 25
|-
| 8 || 14 || 20 || 21 || 2
|-
| 15 || 16 || 22 || 3 || 9
|}

magic(11)

{| class="wikitable" style="text-align:center;width:27.5em;height:27.5em;table-layout:fixed;"
|-
| 68 || 80 || 92 || 104 || 116 || 7 || 19 || 31 || 43 || 55 || 56
|-
| 81 || 93 || 105 || 117 || 8 || 20 || 32 || 44 || 45 || 57 || 69
|-
| 94 || 106 || 118 || 9 || 21 || 33 || 34 || 46 || 58 || 70 || 82
|-
| 107 || 119 || 10 || 22 || 23 || 35 || 47 || 59 || 71 || 83 || 95
|-
| 120 || 11 || 12 || 24 || 36 || 48 || 60 || 72 || 84 || 96 || 108
|-
| 1 || 13 || 25 || 37 || 49 || 61 || 73 || 85 || 97 || 109 || 121
|-
| 14 || 26 || 38 || 50 || 62 || 74 || 86 || 98 || 110 || 111 || 2
|-
| 27 || 39 || 51 || 63 || 75 || 87 || 99 || 100 || 112 || 3 || 15
|-
| 40 || 52 || 64 || 76 || 88 || 89 || 101 || 113 || 4 || 16 || 28
|-
| 53 || 65 || 77 || 78 || 90 || 102 || 114 || 5 || 17 || 29 || 41
|-
| 66 || 67 || 79 || 91 || 103 || 115 || 6 || 18 || 30 || 42 || 54
|}


### ES6


### =Cycled . transposed . cycled=

(2nd Haskell version: ''cycledRows . transpose . cycledRows'')

```JavaScript
(() => {

    // magicSquare :: Int -> [[Int]]
    const magicSquare = n =>
        n % 2 !== 0 ? (
            compose([transpose, cycled, transpose, cycled, enumSquare])(n)
        ) : [];

    // Size of square -> rows containing integers [1..]
    // enumSquare :: Int -> [[Int]]
    const enumSquare = n =>
        chunksOf(n, enumFromTo(1, n * n));

    // Table of integers -> Table with rows rotated by descending deltas
    // cycled :: [[Int]] -> [[Int]]
    const cycled = rows => {
        const d = Math.floor(rows.length / 2);
        return zipWith(listCycle, enumFromTo(d, -d), rows)
    };

    // Number of positions to shift to right -> List -> Wrap-cycled list
    // listCycle :: Int -> [a] -> [a]
    const listCycle = (n, xs) => {
        const d = -(n % xs.length);
        return (d !== 0 ? xs.slice(d)
            .concat(xs.slice(0, d)) : xs);
    };

    // GENERIC FUNCTIONS ------------------------------------------------------

    // chunksOf :: Int -> [a] -> [[a]]
    const chunksOf = (n, xs) =>
        xs.reduce((a, _, i, xs) =>
            i % n ? a : a.concat([xs.slice(i, i + n)]), []);

    // compose :: [(a -> a)] -> (a -> a)
    const compose = fs => x => fs.reduceRight((a, f) => f(a), x);

    // enumFromTo :: Int -> Int -> Maybe Int -> [Int]
    const enumFromTo = (m, n, step) => {
        const d = (step || 1) * (n >= m ? 1 : -1);
        return Array.from({
            length: Math.floor((n - m) / d) + 1
        }, (_, i) => m + (i * d));
    };

    // intercalate :: String -> [a] -> String
    const intercalate = (s, xs) => xs.join(s);

    // min :: Ord a => a -> a -> a
    const min = (a, b) => b < a ? b : a;

    // show :: a -> String
    const show = JSON.stringify;

    // transpose :: [[a]] -> [[a]]
    const transpose = xs =>
        xs[0].map((_, iCol) => xs.map(row => row[iCol]));

    // unlines :: [String] -> String
    const unlines = xs => xs.join('\n');

    // zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
    const zipWith = (f, xs, ys) =>
        Array.from({
            length: min(xs.length, ys.length)
        }, (_, i) => f(xs[i], ys[i]));

    // TEST -------------------------------------------------------------------
    return intercalate('\n\n', [3, 5, 7]
        .map(magicSquare)
        .map(xs => unlines(xs.map(show))));
})();
```

```txt
[8,1,6]
[3,5,7]
[4,9,2]

[17,24,1,8,15]
[23,5,7,14,16]
[4,6,13,20,22]
[10,12,19,21,3]
[11,18,25,2,9]

[30,39,48,1,10,19,28]
[38,47,7,9,18,27,29]
[46,6,8,17,26,35,37]
[5,14,16,25,34,36,45]
[13,15,24,33,42,44,4]
[21,23,32,41,43,3,12]
[22,31,40,49,2,11,20]
```


====Traditional 'Siamese' method====
Encoding the traditional [[wp:Siamese_method|'Siamese' method]]
```JavaScript
(() => {

    // Number of rows -> n rows of integers
    // oddMagicTable :: Int -> [[Int]]
    const oddMagicTable = n =>
        mapAsTable(n, siamMap(quot(n, 2)));

    // Highest index of square -> Siam xys so far -> xy -> next xy coordinate
    // nextSiam :: Int -> M.Map (Int, Int) Int -> (Int, Int) -> (Int, Int)
    const nextSiam = (uBound, sMap, [x, y]) => {
        const [a, b] = [x + 1, y - 1];
        return (a > uBound && b < 0) ? (
                [uBound, 1]             // Move down if obstructed by corner
            ) : a > uBound ? (
                [0, b]                  // Wrap at right edge
            ) : b < 0 ? (
                [a, uBound]             // Wrap at upper edge
            ) : mapLookup(sMap, [a, b])
            .nothing ? (                // Unimpeded default: one up one right
                [a, b]
            ) : [a - 1, b + 2];         // Position occupied: move down
    };

    // Order of table -> Siamese indices keyed by coordinates
    // siamMap :: Int -> M.Map (Int, Int) Int
    const siamMap = n => {
        const
            uBound = 2 * n,
            sPath = (uBound, sMap, xy, n) => {
                const [x, y] = xy,
                newMap = mapInsert(sMap, xy, n);
                return (y == uBound && x == quot(uBound, 2) ? (
                    newMap
                ) : sPath(
                    uBound, newMap, nextSiam(uBound, newMap, [x, y]), n + 1));
            };
        return sPath(uBound, {}, [n, 0], 1);
    };

    // Size of square -> integers keyed by coordinates -> rows of integers
    // mapAsTable :: Int -> M.Map (Int, Int) Int -> [[Int]]
    const mapAsTable = (nCols, dct) => {
        const axis = enumFromTo(0, nCols - 1);
        return map(row => map(k => fromJust(mapLookup(dct, k)), row),
            bind(axis, y => [bind(axis, x => [
                [x, y]
            ])]));
    };

    // GENERIC FUNCTIONS ------------------------------------------------------

    // bind :: [a] -> (a -> [b]) -> [b]
    const bind = (xs, f) => [].concat.apply([], xs.map(f));

    // curry :: Function -> Function
    const curry = (f, ...args) => {
        const go = xs => xs.length >= f.length ? (f.apply(null, xs)) :
            function () {
                return go(xs.concat(Array.from(arguments)));
            };
        return go([].slice.call(args, 1));
    };

    // enumFromTo :: Int -> Int -> [Int]
    const enumFromTo = (m, n) =>
        Array.from({
            length: Math.floor(n - m) + 1
        }, (_, i) => m + i);

    // fromJust :: M a -> a
    const fromJust = m => m.nothing ? {} : m.just;

    // fst :: [a, b] -> a
    const fst = pair => pair.length === 2 ? pair[0] : undefined;

    // intercalate :: String -> [a] -> String
    const intercalate = (s, xs) => xs.join(s);

    // justifyRight :: Int -> Char -> Text -> Text
    const justifyRight = (n, cFiller, strText) =>
        n > strText.length ? (
            (cFiller.repeat(n) + strText)
            .slice(-n)
        ) : strText;

    // length :: [a] -> Int
    const length = xs => xs.length;

    // log :: a -> IO ()
    const log = (...args) =>
        console.log(
            args
            .map(show)
            .join(' -> ')
        );

    // map :: (a -> b) -> [a] -> [b]
    const map = (f, xs) => xs.map(f);

    // mapInsert :: Dictionary -> k -> v -> Dictionary
    const mapInsert = (dct, k, v) =>
        (dct[(typeof k === 'string' && k) || show(k)] = v, dct);

    // mapKeys :: Map k a -> [k]
    const mapKeys = dct =>
        sortBy(mappendComparing([snd, fst]),
            map(JSON.parse, Object.keys(dct)));

    // mapLookup :: Dictionary -> k -> Maybe v
    const mapLookup = (dct, k) => {
        const
            v = dct[(typeof k === 'string' && k) || show(k)],
            blnJust = (typeof v !== 'undefined');
        return {
            nothing: !blnJust,
            just: v
        };
    };

    // mappendComparing :: [(a -> b)] -> (a -> a -> Ordering)
    const mappendComparing = fs => (x, y) =>
        fs.reduce((ord, f) => {
            if (ord !== 0) return ord;
            const
                a = f(x),
                b = f(y);
            return a < b ? -1 : a > b ? 1 : 0
        }, 0);

    // maximum :: [a] -> a
    const maximum = xs =>
        xs.reduce((a, x) => (x > a || a === undefined ? x : a), undefined);

    // Integral a => a -> a -> a
    const quot = (n, m) => Math.floor(n / m);

    // show :: a -> String
    const show = x => JSON.stringify(x);
    //
    // snd :: (a, b) -> b
    const snd = tpl => Array.isArray(tpl) ? tpl[1] : undefined;
    //
    // sortBy :: (a -> a -> Ordering) -> [a] -> [a]
    const sortBy = (f, xs) => xs.slice()
        .sort(f);

    // table :: String -> [[String]] -> [String]
    const table = (delim, rows) =>
        map(curry(intercalate)(delim),
            transpose(map(col =>
                map(curry(justifyRight)(maximum(map(length, col)))(' '), col),
                transpose(rows))));

    // transpose :: [[a]] -> [[a]]
    const transpose = xs =>
        xs[0].map((_, col) => xs.map(row => row[col]));

    // unlines :: [String] -> String
    const unlines = xs => xs.join('\n');

    // TEST -------------------------------------------------------------------

    return intercalate('\n\n',
        bind([3, 5, 7],
            n => unlines(table("  ",
                map(xs => map(show, xs), oddMagicTable(n))))));
})();
```

```txt
8  1  6
3  5  7
4  9  2

17  24   1   8  15
23   5   7  14  16
 4   6  13  20  22
10  12  19  21   3
11  18  25   2   9

30  39  48   1  10  19  28
38  47   7   9  18  27  29
46   6   8  17  26  35  37
 5  14  16  25  34  36  45
13  15  24  33  42  44   4
21  23  32  41  43   3  12
22  31  40  49   2  11  20
```



## jq

'''Adapted from [[#AWK]]'''

```jq
def odd_magic_square:
  if type != "number" or . % 2 == 0 or . <= 0
  then error("odd_magic_square requires an odd positive integer")
  else
    . as $n
    | reduce range(1; 1 + ($n*$n)) as $i
         ( [0, (($n-1)/2), []];
 	  .[0] as $x | .[1] as $y
          | .[2]
	  | setpath([$x, $y]; $i )
          | if getpath([(($x+$n-1) % $n), (($y+$n+1) % $n)])
            then [(($x+$n+1) % $n), $y, .]
            else [ (($x+$n-1) % $n), (($y+$n+1) % $n), .]
 	    end )  | .[2]
  end ;
```

'''Examples'''

```jq
def task:
  def pp: if length == 0 then empty
          else "\(.[0])", (.[1:] | pp )
          end;
  "The magic sum for a square of size \(.) is \( (.*. + 1)*./2 ):",
    (odd_magic_square | pp)
;

(3, 5, 9) | task
```

```sh
$ jq -n -r -M -c -f odd_magic_square.jq
The magic sum for a square of size 3 is 15:
[8,1,6]
[3,5,7]
[4,9,2]
The magic sum for a square of size 5 is 65:
[17,24,1,8,15]
[23,5,7,14,16]
[4,6,13,20,22]
[10,12,19,21,3]
[11,18,25,2,9]
The magic sum for a square of size 9 is 369:
[47,58,69,80,1,12,23,34,45]
[57,68,79,9,11,22,33,44,46]
[67,78,8,10,21,32,43,54,56]
[77,7,18,20,31,42,53,55,66]
[6,17,19,30,41,52,63,65,76]
[16,27,29,40,51,62,64,75,5]
[26,28,39,50,61,72,74,4,15]
[36,38,49,60,71,73,3,14,25]
[37,48,59,70,81,2,13,24,35]
```



## Julia


```julia
# v0.6.0

function magicsquareodd(base::Int)
    if base & 1 == 0 || base < 3; error("base must be odd and >3") end

    square = fill(0, base, base)
    r, number = 1, 1
    size = base * base

    c = div(base, 2) + 1
    while number ≤ size
        square[r, c] = number
        fr = r == 1 ? base : r - 1
        fc = c == base ? 1 : c + 1
        if square[fr, fc] != 0
            fr = r == base ? 1 : r + 1
            fc = c
        end
        r, c = fr, fc
        number += 1
    end

    return square
end

for n in 3:2:7
    println("Magic square with size $n - magic constant = ", div(n ^ 3 + n, 2))
    println("----------------------------------------------------")
    square = magicsquareodd(n)
    for i in 1:n
        println(square[i, :])
    end
    println()
end
```


```txt
Magic square with size 3 - magic constant = 15
----------------------------------------------------
[8, 1, 6]
[3, 5, 7]
[4, 9, 2]

Magic square with size 5 - magic constant = 65
----------------------------------------------------
[17, 24, 1, 8, 15]
[23, 5, 7, 14, 16]
[4, 6, 13, 20, 22]
[10, 12, 19, 21, 3]
[11, 18, 25, 2, 9]

Magic square with size 7 - magic constant = 175
----------------------------------------------------
[30, 39, 48, 1, 10, 19, 28]
[38, 47, 7, 9, 18, 27, 29]
[46, 6, 8, 17, 26, 35, 37]
[5, 14, 16, 25, 34, 36, 45]
[13, 15, 24, 33, 42, 44, 4]
[21, 23, 32, 41, 43, 3, 12]
[22, 31, 40, 49, 2, 11, 20]
```



## Kotlin

```scala
// version 1.0.6

fun f(n: Int, x: Int, y: Int) = (x + y * 2 + 1) % n

fun main(args: Array<String>) {
    var n: Int
    while (true) {
        print("Enter the order of the magic square : ")
        n = readLine()!!.toInt()
        if (n < 1 || n % 2 == 0) println("Must be odd and >= 1, try again")
        else break
    }
    println()
    for (i in 0 until n) {
        for (j in 0 until n) print("%4d".format(f(n, n - j - 1, i) * n + f(n, j, i) + 1))
        println()
    }
    println("\nThe magic constant is ${(n * n + 1) / 2 * n}")
}
```

Sample input/output:
```txt

Enter the order of the magic square : 9

   2  75  67  59  51  43  35  27  10
  22  14   6  79  71  63  46  38  30
  42  34  26  18   1  74  66  58  50
  62  54  37  29  21  13   5  78  70
  73  65  57  49  41  33  25  17   9
  12   4  77  69  61  53  45  28  20
  32  24  16   8  81  64  56  48  40
  52  44  36  19  11   3  76  68  60
  72  55  47  39  31  23  15   7  80

The magic constant is 369

```



## Liberty BASIC


```lb

Dim m(1,1)

Call magicSquare 5
Call magicSquare 17

End

Sub magicSquare n
    ReDim m(n,n)
    inc = 1
    count = 1
    row = 1
    col=(n+1)/2
    While count <= n*n
        m(row,col) = count
        count = count + 1
        If inc < n Then
            inc = inc + 1
            row = row - 1
            col = col + 1
            If row <> 0 Then
                If col > n Then col = 1
            Else
                row = n
            End If
        Else
            inc = 1
            row = row + 1
        End If
    Wend
    Call printSquare n
End Sub

Sub printSquare n
    'Arbitrary limit to fit width of A4 paper
    If n < 23 Then
        Print n;" x ";n;" Magic Square --- ";
        Print "Magic constant is ";Int((n*n+1)/2*n)
        For row = 1 To n
            For col = 1 To n
                Print Using("####",m(row,col));
            Next col
            Print
            Print
        Next row
    Else
        Notice "Magic Square will not fit on one sheet of paper."
    End If
End Sub

```

```txt

5 x 5 Magic Square --- Magic constant is 65
  17  24   1   8  15

  23   5   7  14  16

   4   6  13  20  22

  10  12  19  21   3

  11  18  25   2   9

17 x 17 Magic Square --- Magic constant is 2465
 155 174 193 212 231 250 269 288   1  20  39  58  77  96 115 134 153

 173 192 211 230 249 268 287  17  19  38  57  76  95 114 133 152 154

 191 210 229 248 267 286  16  18  37  56  75  94 113 132 151 170 172

 209 228 247 266 285  15  34  36  55  74  93 112 131 150 169 171 190

 227 246 265 284  14  33  35  54  73  92 111 130 149 168 187 189 208

 245 264 283  13  32  51  53  72  91 110 129 148 167 186 188 207 226

 263 282  12  31  50  52  71  90 109 128 147 166 185 204 206 225 244

 281  11  30  49  68  70  89 108 127 146 165 184 203 205 224 243 262

  10  29  48  67  69  88 107 126 145 164 183 202 221 223 242 261 280

  28  47  66  85  87 106 125 144 163 182 201 220 222 241 260 279   9

  46  65  84  86 105 124 143 162 181 200 219 238 240 259 278   8  27

  64  83 102 104 123 142 161 180 199 218 237 239 258 277   7  26  45

  82 101 103 122 141 160 179 198 217 236 255 257 276   6  25  44  63

 100 119 121 140 159 178 197 216 235 254 256 275   5  24  43  62  81

 118 120 139 158 177 196 215 234 253 272 274   4  23  42  61  80  99

 136 138 157 176 195 214 233 252 271 273   3  22  41  60  79  98 117

 137 156 175 194 213 232 251 270 289   2  21  40  59  78  97 116 135

```



## Lua

For all three kinds of Magic Squares(Odd, singly and doubly even)<br />
See [[Magic_squares/Lua]].


## Mathematica

Rotate rows and columns of the initial matrix with rows filled in order 1 2 3 .... N^2

Method from http://www.jsoftware.com/papers/eem/magicsq.htm


```Mathematica

rp[v_, pos_] := RotateRight[v, (Length[v] + 1)/2 - pos];
rho[m_] := MapIndexed[rp, m];
magic[n_] :=
  rho[Transpose[rho[Table[i*n + j, {i, 0, n - 1}, {j, 1, n}]]]];

square = magic[11] // Grid
Print["Magic number is ", Total[square[[1, 1]]]]

```

{{out}} (alignment lost in translation to text):

 {68, 80, 92, 104, 116, 7, 19, 31, 43, 55, 56},
 {81, 93, 105, 117, 8, 20, 32, 44, 45, 57, 69},
 {94, 106, 118, 9, 21, 33, 34, 46, 58, 70, 82},
 {107, 119, 10, 22, 23, 35, 47, 59, 71, 83, 95},
 {120, 11, 12, 24, 36, 48, 60, 72, 84, 96, 108},
 {1, 13, 25, 37, 49, 61, 73, 85, 97, 109, 121},
 {14, 26, 38, 50, 62, 74, 86, 98, 110, 111, 2},
 {27, 39, 51, 63, 75, 87, 99, 100, 112, 3, 15},
 {40, 52, 64, 76, 88, 89, 101, 113, 4, 16, 28},
 {53, 65, 77, 78, 90, 102, 114, 5, 17, 29, 41},
 {66, 67, 79, 91, 103, 115, 6, 18, 30, 42, 54}

Magic number is 671

Output from code that checks the results
Rows

{671,671,671,671,671,671,671,671,671,671,671}

Columns

{671,671,671,671,671,671,671,671,671,671,671}

Diagonals

671

671


## Maxima


```Maxima
wrap1(i):= if i>%n% then 1 else if i<1 then %n% else i;
wrap(P):=maplist('wrap1, P);

uprigth(P):= wrap(P + [-1, 1]);
down(P):= wrap(P + [1, 0]);

magic(n):=block([%n%: n,
  M: zeromatrix (n, n),
  P: [1, (n + 1)/2],
  m: 1, Pc],
  do (
    M[P[1],P[2]]: m,
    m: m + 1,
    if m>n^2 then return(M),
    Pc: uprigth(P),
    if M[Pc[1],Pc[2]]=0 then P: Pc
    else while(M[P[1],P[2]]#0) do P: down(P)));
```


Usage:

```output
(%i6) magic(3);
                                  [ 8  1  6 ]
                                  [         ]
(%o6)                             [ 3  5  7 ]
                                  [         ]
                                  [ 4  9  2 ]
(%i7) magic(5);
                            [ 17  24  1   8   15 ]
                            [                    ]
                            [ 23  5   7   14  16 ]
                            [                    ]
(%o7)                       [ 4   6   13  20  22 ]
                            [                    ]
                            [ 10  12  19  21  3  ]
                            [                    ]
                            [ 11  18  25  2   9  ]
(%i8) magic(7);
                        [ 30  39  48  1   10  19  28 ]
                        [                            ]
                        [ 38  47  7   9   18  27  29 ]
                        [                            ]
                        [ 46  6   8   17  26  35  37 ]
                        [                            ]
(%o8)                   [ 5   14  16  25  34  36  45 ]
                        [                            ]
                        [ 13  15  24  33  42  44  4  ]
                        [                            ]
                        [ 21  23  32  41  43  3   12 ]
                        [                            ]
                        [ 22  31  40  49  2   11  20 ]
/* magic number for n=7 */
(%i9) lsum(q, q, first(magic(7)));
(%o9)                                 175
```



## Nim

```nim
import strutils

proc `^`*(base: int, exp: int): int =
  var (base, exp) = (base, exp)
  result = 1

  while exp != 0:
    if (exp and 1) != 0:
      result *= base
    exp = exp shr 1
    base *= base

proc magic(n) =
  for row in 1 .. n:
    for col in 1 .. n:
      let cell = (n * ((row + col - 1 + n div 2) mod n) +
                  ((row + 2 * col - 2) mod n) + 1)
      stdout.write align($cell, len($(n^2)))," "
    echo ""
  echo "\nAll sum to magic number ", ((n * n + 1) * n div 2)

for n in [5, 3, 7]:
  echo "\nOrder ",n,"\n
### =
"
  magic(n)
```

```txt
Order 5

### =

17 24  1  8 15
23  5  7 14 16
 4  6 13 20 22
10 12 19 21  3
11 18 25  2  9

All sum to magic number 65

Order 3

### =

8 1 6
3 5 7
4 9 2

All sum to magic number 15

Order 7

### =

30 39 48  1 10 19 28
38 47  7  9 18 27 29
46  6  8 17 26 35 37
 5 14 16 25 34 36 45
13 15 24 33 42 44  4
21 23 32 41 43  3 12
22 31 40 49  2 11 20

All sum to magic number 175
```



## Oforth



```Oforth
: magicSquare(n)
| i j wd |
   n sq log asInteger 1+ ->wd
   n loop: i [
      n loop: j [
         i j + 1- n 2 / + n mod n *
         i j + j + 2 - n mod 1 + +
         System.Out swap <<w(wd) " " << drop
         ]
      printcr
      ]
   System.Out "Magic constant is : " << n sq 1 + 2 / n * << cr ;
```


```txt

5 magicSquare
17 24  1  8 15
23  5  7 14 16
 4  6 13 20 22
10 12 19 21  3
11 18 25  2  9
Magic constant is : 65

```



## Pascal

```pascal
PROGRAM magic;
(* Magic squares of odd order *)
CONST
  n=9;
VAR
  i,j :INTEGER;
BEGIN (*magic*)
  WRITELN('The square order is: ',n);
  FOR i:=1 TO n DO
  BEGIN
    FOR j:=1 TO n DO
      WRITE((i*2-j+n-1) MOD n*n + (i*2+j-2) MOD n+1:5);
    WRITELN
  END;
  WRITELN('The magic number is: ',n*(n*n+1) DIV 2)
END (*magic*).
```

```txt

The square order is: 9
    2   75   67   59   51   43   35   27   10
   22   14    6   79   71   63   46   38   30
   42   34   26   18    1   74   66   58   50
   62   54   37   29   21   13    5   78   70
   73   65   57   49   41   33   25   17    9
   12    4   77   69   61   53   45   28   20
   32   24   16    8   81   64   56   48   40
   52   44   36   19   11    3   76   68   60
   72   55   47   39   31   23   15    7   80
The magic number is: 369

```


### improved

shuffles columns and rows and changed col<-> row to get different looks. n! x n! * 2 different arrangements.
See last column of version before moved to the top row.

```pascal
PROGRAM magic;
{$IFDEF FPC }{$MODE DELPHI}{$ELSE}{$APPTYPE CONSOLE}{$ENDIF}
uses
  sysutils;
(* Magic squares of odd order *)
type
  tsquare = array of array of LongInt;
  trowcol = array of NativeInt;

function GenShuffleRowCol(n: nativeInt):trowcol;
var
  i,j,tmp: NativeInt;
begin
  setlength(result,0);
  IF n > 0 then
  Begin
    setlength(result,n);
    For i := 0 to n-1 do
      result[i] := i;
    //shuffle
    For i := n-1 downto 1 do
    Begin
      j := random(i+1);//j == [0..i]
      tmp := result[i];result[i]:= result[j];result[j]:= tmp;
    end;
  end;
end;

function MagicSqrOdd(n:nativeInt;SwapColRoW:boolean):tsquare;
VAR
  rowIdx,colIdx,row,col,num :NativeInt;
  cols,rows :trowcol;
BEGIN
  rows:= GenShuffleRowCol(n);
  cols:= GenShuffleRowCol(n);
  setlength(result,n,n);
  FOR rowIdx:= 0 TO n-1 DO
  BEGIN
    row := rows[rowIdx];
    FOR colIdx:=0 TO n-1 DO
    Begin
      col := cols[colIdx];
      //corrected formula cause row :0..n*1-> corrected to 1..n
      num := (row*2-col+n+2) MOD n*n + (row*2+col+1) MOD n+1;
      IF SwapColRoW then
        result[colIdx,rowIdx] := num
      else
        result[rowIdx,colIdx] := num;
    end;
  END;
END;

function MagicSqrCheck(const Mq:tsquare):boolean;
var
  row,col,rowsum,mn,n,itm: NativeInt;
  colSum:trowcol;
begin
  n := length(Mq[0]);
  mn := n*(n*n+1) DIV 2;
  setlength(colsum,n);//automatic initialised to zero
  For row := n-1 downto 0 do
  Begin
    //check one row
    rowsum := 0;
    For col := n-1 downto 0 do
    Begin
      itm := Mq[row,col];
      write(itm:4);
      inc(rowsum,itm);
      //sum up the columns too, for I'm just here
      inc(colSum[col],itm);
    end;
    writeln;
    result := (rowsum=mn);
    IF Not(result) then begin writeln(row:4,col:4,rowsum:10);EXIT;end;
  end;
  //check columns
  For col := n-1 downto 0 do
  Begin
    result := (colSum[col]=mn);
    IF Not(result) then begin writeln(col:4,colSum[col]:10);EXIT;end;
  end;
  writeln;
end;


var
  n,mn : nativeInt;
  Mq : tsquare;
Begin
  randomize;
  n := 9;
  mn := n*(n*n+1) DIV 2;
  WRITELN('The square order is: ',n);
  WRITELN('The magic number is: ',mn);
  Mq := MagicSqrOdd(n,random(2)=0);
  writeln(MagicSqrCheck(Mq));
end.
```

```txt

The square order is: 9
The magic number is: 369
  70  30  20   9  40  50  10  80  60
  13  63  53  33  64  74  43  23   3
  54  14   4  65  24  34  75  55  44
   5  46  45  25  56  66  35  15  76
  37   6  77  57  16  26  67  47  36
  29  79  69  49   8  18  59  39  19
  78  38  28  17  48  58  27   7  68
  62  22  12  73  32  42   2  72  52
  21  71  61  41  81   1  51  31  11

TRUE
```



## PARI/GP

The index-fiddling differs from Perl since GP vectors start at 1.

```parigp
magicSquare(n)={
  my(M=matrix(n,n),j=n\2+1,i=1);
  for(l=1,n^2,
    M[i,j]=l;
    if(M[(i-2)%n+1,j%n+1],
      i=i%n+1
    ,
      i=(i-2)%n+1;
      j=j%n+1
    )
  );
  M;
}
magicSquare(7)
```

```txt
[30 39 48  1 10 19 28]

[38 47  7  9 18 27 29]

[46  6  8 17 26 35 37]

[ 5 14 16 25 34 36 45]

[13 15 24 33 42 44  4]

[21 23 32 41 43  3 12]

[22 31 40 49  2 11 20]
```



## Perl


See [[Magic_squares/Perl|Magic squares/Perl]] for a general magic square generator.

```perl

```



## Perl 6

See [[Magic_squares/Perl_6|Magic squares/Perl 6]] for a general magic square generator.
With a parameter of 5:

```txt
17 24  1  8 15
23  5  7 14 16
 4  6 13 20 22
10 12 19 21  3
11 18 25  2  9

The magic number is 65
```


With a parameter of 19:

```txt
192 213 234 255 276 297 318 339 360   1  22  43  64  85 106 127 148 169 190
212 233 254 275 296 317 338 359  19  21  42  63  84 105 126 147 168 189 191
232 253 274 295 316 337 358  18  20  41  62  83 104 125 146 167 188 209 211
252 273 294 315 336 357  17  38  40  61  82 103 124 145 166 187 208 210 231
272 293 314 335 356  16  37  39  60  81 102 123 144 165 186 207 228 230 251
292 313 334 355  15  36  57  59  80 101 122 143 164 185 206 227 229 250 271
312 333 354  14  35  56  58  79 100 121 142 163 184 205 226 247 249 270 291
332 353  13  34  55  76  78  99 120 141 162 183 204 225 246 248 269 290 311
352  12  33  54  75  77  98 119 140 161 182 203 224 245 266 268 289 310 331
 11  32  53  74  95  97 118 139 160 181 202 223 244 265 267 288 309 330 351
 31  52  73  94  96 117 138 159 180 201 222 243 264 285 287 308 329 350  10
 51  72  93 114 116 137 158 179 200 221 242 263 284 286 307 328 349   9  30
 71  92 113 115 136 157 178 199 220 241 262 283 304 306 327 348   8  29  50
 91 112 133 135 156 177 198 219 240 261 282 303 305 326 347   7  28  49  70
111 132 134 155 176 197 218 239 260 281 302 323 325 346   6  27  48  69  90
131 152 154 175 196 217 238 259 280 301 322 324 345   5  26  47  68  89 110
151 153 174 195 216 237 258 279 300 321 342 344   4  25  46  67  88 109 130
171 173 194 215 236 257 278 299 320 341 343   3  24  45  66  87 108 129 150
172 193 214 235 256 277 298 319 340 361   2  23  44  65  86 107 128 149 170

The magic number is 3439
```



## Phix


```Phix
function magic_square(integer n)
    if mod(n,2)!=1 or n<1 then return false end if
    sequence square = repeat(repeat(0,n),n)
    for i=1 to n do
        for j=1 to n do
            square[i,j] = n*mod(2*i-j+n-1,n) + mod(2*i+j-2,n) + 1
        end for
    end for
    return square
end function

procedure check(sequence sq)
    integer n = length(sq)
    integer magic = n*(n*n+1)/2
    integer bd=0, fd=0
    for i=1 to length(sq) do
        if sum(sq[i])!=magic then ?9/0 end if
        if sum(columnize(sq,i))!=magic then ?9/0 end if
        bd += sq[i,i]
        fd += sq[n-i+1,n-i+1]
    end for
    if bd!=magic or fd!=magic then ?9/0 end if
end procedure

for i=1 to 7 by 2 do
    sequence square = magic_square(i)
    printf(1,"maqic square of order %d, sum: %d\n", {i,sum(square[i])})
    string fmt = sprintf("%%%dd",length(sprintf("%d",i*i)))
    pp(square,{pp_Nest,1,pp_IntFmt,fmt,pp_StrFmt,1,pp_Pause,0})
    check(square)
end for
```

```txt

maqic square of order 1, sum: 1
maqic square of order 3, sum: 15
{{2,9,4},
 {7,5,3},
 {6,1,8}}
maqic square of order 5, sum: 65
{{ 2,23,19,15, 6},
 {14,10, 1,22,18},
 {21,17,13, 9, 5},
 { 8, 4,25,16,12},
 {20,11, 7, 3,24}}
maqic square of order 7, sum: 175
{{ 2,45,39,33,27,21, 8},
 {18,12, 6,49,36,30,24},
 {34,28,15, 9, 3,46,40},
 {43,37,31,25,19,13, 7},
 {10, 4,47,41,35,22,16},
 {26,20,14, 1,44,38,32},
 {42,29,23,17,11, 5,48}}

```



## PL/I


```pli
magic: procedure options (main);  /* 18 April 2014 */
   declare n fixed binary;

   put skip list ('What is the order of the magic square?');
   get list (n);
   if n < 3 | iand(n, 1) = 0 then
      do; put skip list ('The value is out of range'); stop; end;
   put skip list ('The order is ' || trim(n));

   begin;
      declare m(n, n) fixed, (i, j, k) fixed binary;

      on subrg snap put data (i, j, k);
      m = 0;
      i = 1; j = (n+1)/2;

      do k = 1 to n*n;
         if m(i,j) = 0 then
            m(i,j) = k;
         else
            do;
               i = i + 2; j = j + 1;
               if i > n then i = mod(i,n);
               if j > n then j = 1;
               m(i,j) = k;
            end;
         i = i - 1; j = j - 1;
         if i < 1 then i = n;
         if j < 1 then j = n;
      end;

      do i = 1 to n;
         put skip edit (m(i, *)) (f(4));
      end;

      put skip list ('The magic number is' || sum(m(1,*)));
   end;
end magic;
```

```txt
What is the order of the magic square?

The order is 5
  15   8   1  24  17
  16  14   7   5  23
  22  20  13   6   4
   3  21  19  12  10
   9   2  25  18  11
The magic number is                65
What is the order of the magic square?

The order is 7
  28  19  10   1  48  39  30
  29  27  18   9   7  47  38
  37  35  26  17   8   6  46
  45  36  34  25  16  14   5
   4  44  42  33  24  15  13
  12   3  43  41  32  23  21
  20  11   2  49  40  31  22
The magic number is               175
```



## PureBasic

```purebasic
#N=9
Define.i i,j

If OpenConsole("Magic squares")
  PrintN("The square order is: "+Str(#N))
  For i=1 To #N
    For j=1 To #N
      Print(RSet(Str((i*2-j+#N-1) % #N*#N + (i*2+j-2) % #N+1),5))
    Next
    PrintN("")
  Next
  PrintN("The magic number is: "+Str(#N*(#N*#N+1)/2))
EndIf
Input()
```

```txt

The square order is: 9
    2   75   67   59   51   43   35   27   10
   22   14    6   79   71   63   46   38   30
   42   34   26   18    1   74   66   58   50
   62   54   37   29   21   13    5   78   70
   73   65   57   49   41   33   25   17    9
   12    4   77   69   61   53   45   28   20
   32   24   16    8   81   64   56   48   40
   52   44   36   19   11    3   76   68   60
   72   55   47   39   31   23   15    7   80
The magic number is: 369

```



## Python


### Procedural


```python
>>>
 def magic(n):
    for row in range(1, n + 1):
        print(' '.join('%*i' % (len(str(n**2)), cell) for cell in
                       (n * ((row + col - 1 + n // 2) % n) +
                       ((row + 2 * col - 2) % n) + 1
                       for col in range(1, n + 1))))
    print('\nAll sum to magic number %i' % ((n * n + 1) * n // 2))


>>> for n in (5, 3, 7):
	print('\nOrder %i\n
### =
' % n)
	magic(n)



Order 5

### =

17 24  1  8 15
23  5  7 14 16
 4  6 13 20 22
10 12 19 21  3
11 18 25  2  9

All sum to magic number 65

Order 3

### =

8 1 6
3 5 7
4 9 2

All sum to magic number 15

Order 7

### =

30 39 48  1 10 19 28
38 47  7  9 18 27 29
46  6  8 17 26 35 37
 5 14 16 25 34 36 45
13 15 24 33 42 44  4
21 23 32 41 43  3 12
22 31 40 49  2 11 20

All sum to magic number 175
>>>
```



### Composition of pure functions

Two applications of ('''transposed . cycled''') to a sequentially ordered square:
```python
'''Magic squares of odd order N'''

from itertools import cycle, islice, repeat
from functools import reduce


# magicSquare :: Int -> [[Int]]
def magicSquare(n):
    '''Magic square of odd order n.'''
    return applyN(2)(
        compose(transposed)(cycled)
    )(plainSquare(n)) if 1 == n % 2 else []


# plainSquare :: Int -> [[Int]]
def plainSquare(n):
    '''The sequence of integers from 1 to N^2,
       subdivided into N sub-lists of equal length,
       forming N rows, each of N integers.
    '''
    return chunksOf(n)(
        enumFromTo(1)(n ** 2)
    )


# cycled :: [[Int]] -> [[Int]]
def cycled(rows):
    '''A table in which the rows are
       rotated by descending deltas.
    '''
    n = len(rows)
    d = n // 2
    return list(map(
        lambda d, xs: take(n)(
            drop(n - d)(cycle(xs))
        ),
        enumFromThenTo(d)(d - 1)(-d),
        rows
    ))


# TEST ----------------------------------------------------
# main :: IO ()
def main():
    '''Magic squares of order 3, 5, 7'''
    print(
        fTable(__doc__ + ':')(lambda x: '\n' + repr(x))(
            showSquare
        )(magicSquare)([3, 5, 7])
    )


# GENERIC -------------------------------------------------

# applyN :: Int -> (a -> a) -> a -> a
def applyN(n):
    '''n applications of f.
       (Church numeral n).
    '''
    def go(f):
        return lambda x: reduce(
            lambda a, g: g(a), repeat(f, n), x
        )
    return lambda f: go(f)


# chunksOf :: Int -> [a] -> [[a]]
def chunksOf(n):
    '''A series of lists of length n,
       subdividing the contents of xs.
       Where the length of xs is not evenly divible,
       the final list will be shorter than n.'''
    return lambda xs: reduce(
        lambda a, i: a + [xs[i:n + i]],
        range(0, len(xs), n), []
    ) if 0 < n else []


# compose (<<<) :: (b -> c) -> (a -> b) -> a -> c
def compose(g):
    '''Right to left function composition.'''
    return lambda f: lambda x: g(f(x))


# drop :: Int -> [a] -> [a]
# drop :: Int -> String -> String
def drop(n):
    '''The sublist of xs beginning at
       (zero-based) index n.'''
    def go(xs):
        if isinstance(xs, (list, tuple, str)):
            return xs[n:]
        else:
            take(n)(xs)
            return xs
    return lambda xs: go(xs)


# enumFromThenTo :: Int -> Int -> Int -> [Int]
def enumFromThenTo(m):
    '''Integer values enumerated from m to n
       with a step defined by nxt-m.
    '''
    def go(nxt, n):
        d = nxt - m
        return range(m, n - 1 if d < 0 else 1 + n, d)
    return lambda nxt: lambda n: list(go(nxt, n))


# enumFromTo :: (Int, Int) -> [Int]
def enumFromTo(m):
    '''Integer enumeration from m to n.'''
    return lambda n: list(range(m, 1 + n))


# take :: Int -> [a] -> [a]
# take :: Int -> String -> String
def take(n):
    '''The prefix of xs of length n,
       or xs itself if n > length xs.
    '''
    return lambda xs: (
        xs[0:n]
        if isinstance(xs, (list, tuple))
        else list(islice(xs, n))
    )


# transposed :: Matrix a -> Matrix a
def transposed(m):
    '''The rows and columns of the argument transposed.
       (The matrix containers and rows can be lists or tuples).
    '''
    if m:
        inner = type(m[0])
        z = zip(*m)
        return (type(m))(
            map(inner, z) if tuple != inner else z
        )
    else:
        return m


# DISPLAY -------------------------------------------------

# fTable :: String -> (a -> String) ->
#                     (b -> String) -> (a -> b) -> [a] -> String
def fTable(s):
    '''Heading -> x display function -> fx display function ->
                     f -> xs -> tabular string.
    '''
    def go(xShow, fxShow, f, xs):
        ys = [xShow(x) for x in xs]
        w = max(map(len, ys))
        return s + '\n' + '\n'.join(map(
            lambda x, y: y.rjust(w, ' ') + ' -> ' + fxShow(f(x)),
            xs, ys
        ))
    return lambda xShow: lambda fxShow: lambda f: lambda xs: go(
        xShow, fxShow, f, xs
    )


# indented :: Int -> String -> String
def indented(n):
    '''String indented by n multiples
       of four spaces
    '''
    return lambda s: (n * 4 * ' ') + s


# showSquare :: [[Int]] -> String
def showSquare(rows):
    '''Lines representing rows of lists.'''
    w = 1 + len(str(reduce(max, map(max, rows), 0)))
    return '\n' + '\n'.join(
        map(
            lambda row: indented(1)(''.join(
                map(lambda x: str(x).rjust(w, ' '), row)
            )),
            rows
        )
    )


# MAIN ---
if __name__ == '__main__':
    main()
```

```txt
Magic squares of odd order N:

3 ->
     8 1 6
     3 5 7
     4 9 2

5 ->
     17 24  1  8 15
     23  5  7 14 16
      4  6 13 20 22
     10 12 19 21  3
     11 18 25  2  9

7 ->
     30 39 48  1 10 19 28
     38 47  7  9 18 27 29
     46  6  8 17 26 35 37
      5 14 16 25 34 36 45
     13 15 24 33 42 44  4
     21 23 32 41 43  3 12
     22 31 40 49  2 11 20
```



## Racket


```racket
#lang racket
;; Using "helpful formulae" in:
;; http://en.wikipedia.org/wiki/Magic_square#Method_for_constructing_a_magic_square_of_odd_order
(define (squares n) n)

(define (last-no n) (sqr n))

(define (middle-no n) (/ (add1 (sqr n)) 2))

(define (M n) (* n (middle-no n)))

(define ((Ith-row-Jth-col n) I J)
  (+ (* (modulo (+ I J -1 (exact-floor (/ n 2))) n) n)
     (modulo (+ I (* 2 J) -2) n)
     1))

(define (magic-square n)
  (define IrJc (Ith-row-Jth-col n))
  (for/list ((I (in-range 1 (add1 n)))) (for/list ((J (in-range 1 (add1 n)))) (IrJc I J))))

(define (fmt-list-of-lists l-o-l width)
  (string-join
   (for/list ((row l-o-l))
     (string-join (map (λ (x) (~a #:align 'right #:width width x)) row) "  "))
   "\n"))

(define (show-magic-square n)
  (format "MAGIC SQUARE ORDER:~a~%~a~%MAGIC NUMBER:~a~%"
          n (fmt-list-of-lists (magic-square n) (+ (order-of-magnitude (last-no n)) 1)) (M n)))

(displayln (show-magic-square 3))
(displayln (show-magic-square 5))
(displayln (show-magic-square 9))
```

```txt
MAGIC SQUARE ORDER:3
8  1  6
3  5  7
4  9  2
Magic Number:15

MAGIC SQUARE ORDER:5
17  24   1   8  15
23   5   7  14  16
 4   6  13  20  22
10  12  19  21   3
11  18  25   2   9
Magic Number:65

MAGIC SQUARE ORDER:9
47  58  69  80   1  12  23  34  45
57  68  79   9  11  22  33  44  46
67  78   8  10  21  32  43  54  56
77   7  18  20  31  42  53  55  66
 6  17  19  30  41  52  63  65  76
16  27  29  40  51  62  64  75   5
26  28  39  50  61  72  74   4  15
36  38  49  60  71  73   3  14  25
37  48  59  70  81   2  13  24  35
Magic Number:369
```



## REXX

This REXX version will also generate a square of an even order, but it'll not be a ''magic square''.

```rexx
/*REXX program generates and displays magic squares (odd N will be a true magic square).*/
parse arg N .                                    /*obtain the optional argument from CL.*/
if N=='' | N==","  then N=5                      /*Not specified?  Then use the default.*/
NN=N*N;    w=length(NN)                          /*W:  width of largest number (output).*/
r=1;       c=(n+1) % 2                           /*define the initial  row  and  column.*/
@.=.                                             /*assign a default value for entire  @.*/
    do j=1  for NN                               /* [↓]  filling uses the Siamese method*/
    if r<1 & c>N then do; r=r+2;  c=c-1;  end    /*the  row   is under,  column is over.*/
    if r<1       then r=N                        /* "    "     "   "     make row=last. */
    if r>N       then r=1                        /* "    "     "  over,    "   "  first.*/
    if c>N       then c=1                        /* "  column  "  over,    "  col=first.*/
    if @.r.c\==. then do; r=min(N,r+2);  c=max(1,c-1);  end     /*at the previous cell? */
    @.r.c=j;              r=r-1;  c=c+1          /*assign # ───► cell; next row & column*/
    end   /*j*/
                                                 /* [↓]  display square with aligned #'s*/
          do   r=1  for N;  _=                   /*display  one matrix row  at a time.  */
            do c=1  for N;  _=_ right(@.r.c, w)  /*construct a row of the magic square. */
            end   /*c*/
          say substr(_, 2)                       /*display a row of the magic square.   */
          end     /*r*/
say                                              /* [↓]  If an odd square, show magic #.*/
if N//2  then say  'The magic number  (or magic constant is): '         N * (NN+1) % 2
                                                 /*stick a fork in it,  we're all done. */
```

```txt

17 24  1  8 15
23  5  7 14 16
 4  6 13 20 22
10 12 19 21  3
11 18 25  2  9

The magic number  (or magic constant is):  65

```

'''output'''   when using the input of:   '''3'''

```txt

8 1 6
3 5 7
4 9 2

The magic number  (or magic constant is):  15

```

'''output'''   when using the input of:   '''19'''

```txt

192 213 234 255 276 297 318 339 360   1  22  43  64  85 106 127 148 169 190
212 233 254 275 296 317 338 359  19  21  42  63  84 105 126 147 168 189 191
232 253 274 295 316 337 358  18  20  41  62  83 104 125 146 167 188 209 211
252 273 294 315 336 357  17  38  40  61  82 103 124 145 166 187 208 210 231
272 293 314 335 356  16  37  39  60  81 102 123 144 165 186 207 228 230 251
292 313 334 355  15  36  57  59  80 101 122 143 164 185 206 227 229 250 271
312 333 354  14  35  56  58  79 100 121 142 163 184 205 226 247 249 270 291
332 353  13  34  55  76  78  99 120 141 162 183 204 225 246 248 269 290 311
352  12  33  54  75  77  98 119 140 161 182 203 224 245 266 268 289 310 331
 11  32  53  74  95  97 118 139 160 181 202 223 244 265 267 288 309 330 351
 31  52  73  94  96 117 138 159 180 201 222 243 264 285 287 308 329 350  10
 51  72  93 114 116 137 158 179 200 221 242 263 284 286 307 328 349   9  30
 71  92 113 115 136 157 178 199 220 241 262 283 304 306 327 348   8  29  50
 91 112 133 135 156 177 198 219 240 261 282 303 305 326 347   7  28  49  70
111 132 134 155 176 197 218 239 260 281 302 323 325 346   6  27  48  69  90
131 152 154 175 196 217 238 259 280 301 322 324 345   5  26  47  68  89 110
151 153 174 195 216 237 258 279 300 321 342 344   4  25  46  67  88 109 130
171 173 194 215 236 257 278 299 320 341 343   3  24  45  66  87 108 129 150
172 193 214 235 256 277 298 319 340 361   2  23  44  65  86 107 128 149 170

The magic number  (or magic constant is):  3439

```



## Ring


```ring

n=9
see "the square order is : " + n + nl
for i=1 to n
    for j = 1 to n
        x = (i*2-j+n-1) % n*n + (i*2+j-2) % n + 1
        see "" + x + " "
    next
    see nl
next
see "'the magic number is : " + n*(n*n+1) / 2 + nl

```

Output:

```txt

the square order is : 9

2 75 67 59 51 43 35 27 10
22 14 6 79 71 63 46 38 30
42 34 26 18 1 74 66 58 50
62 54 37 29 21 13 5 78 70
73 65 57 49 41 33 25 17 9
12 4 77 69 61 53 45 28 20
32 24 16 8 81 64 56 48 40
52 44 36 19 11 3 76 68 60
72 55 47 39 31 23 15 7 80

the magic number is : 369

```



## Ruby


```ruby
def odd_magic_square(n)
  raise ArgumentError "Need odd positive number" if n.even? || n <= 0
  n.times.map{|i| n.times.map{|j| n*((i+j+1+n/2)%n) + ((i+2*j-5)%n) + 1} }
end

[3, 5, 9].each do |n|
  puts "\nSize #{n}, magic sum #{(n*n+1)/2*n}"
  fmt = "%#{(n*n).to_s.size + 1}d" * n
  odd_magic_square(n).each{|row| puts fmt % row}
end

```

```txt

Size 3, magic sum 15
 8 1 6
 3 5 7
 4 9 2

Size 5, magic sum 65
 16 23  5  7 14
 22  4  6 13 20
  3 10 12 19 21
  9 11 18 25  2
 15 17 24  1  8

Size 9, magic sum 369
 50 61 72 74  4 15 26 28 39
 60 71 73  3 14 25 36 38 49
 70 81  2 13 24 35 37 48 59
 80  1 12 23 34 45 47 58 69
  9 11 22 33 44 46 57 68 79
 10 21 32 43 54 56 67 78  8
 20 31 42 53 55 66 77  7 18
 30 41 52 63 65 76  6 17 19
 40 51 62 64 75  5 16 27 29

```



## Rust


```rust
fn main() {
    let n = 9;
    let mut square = vec![vec![0; n]; n];
    for (i, row) in square.iter_mut().enumerate() {
        for (j, e) in row.iter_mut().enumerate() {
            *e = n * (((i + 1) + (j + 1) - 1 + (n >> 1)) % n) + (((i + 1) + (2 * (j + 1)) - 2) % n) + 1;
            print!("{:3} ", e);
        }
        println!("");
    }
    let sum = n * (((n * n) + 1) / 2);
    println!("The sum of the square is {}.", sum);
}
```


```txt
 47  58  69  80   1  12  23  34  45
 57  68  79   9  11  22  33  44  46
 67  78   8  10  21  32  43  54  56
 77   7  18  20  31  42  53  55  66
  6  17  19  30  41  52  63  65  76
 16  27  29  40  51  62  64  75   5
 26  28  39  50  61  72  74   4  15
 36  38  49  60  71  73   3  14  25
 37  48  59  70  81   2  13  24  35
The sum of the square is 369.
```



## Scala


```scala
  def magicSquare( n:Int ) : Option[Array[Array[Int]]] = {
    require(n % 2 != 0, "n must be an odd number")

    val a = Array.ofDim[Int](n,n)

    // Make the horizontal by starting in the middle of the row and then taking a step back every n steps
    val ii = Iterator.continually(0 to n-1).flatten.drop(n/2).sliding(n,n-1).take(n*n*2).toList.flatten

    // Make the vertical component by moving up (subtracting 1) but every n-th step, step down (add 1)
    val jj = Iterator.continually(n-1 to 0 by -1).flatten.drop(n-1).sliding(n,n-2).take(n*n*2).toList.flatten

    // Combine the horizontal and vertical components to create the path
    val path = (ii zip jj) take (n*n)

    // Fill the array by following the path
    for( i<-1 to (n*n); p=path(i-1) ) { a(p._1)(p._2) = i }

    Some(a)
  }

  def output() :  Unit = {
    def printMagicSquare(n: Int): Unit = {

      val ms = magicSquare(n)
      val magicsum = (n * n + 1) / 2

      assert(
        if( ms.isDefined ) {
          val a = ms.get
          a.forall(_.sum == magicsum) &&
            a.transpose.forall(_.sum == magicsum) &&
            (for(i<-0 until n) yield { a(i)(i) }).sum == magicsum
        }
        else { false }
      )

      if( ms.isDefined ) {
        val a = ms.get
        for (y <- 0 to n * 2; x <- 0 until n) (x, y) match {
          case (0, 0) => print("╔════╤")
          case (i, 0) if i == n - 1 => print("════╗\n")
          case (i, 0) => print("════╤")

          case (0, j) if j % 2 != 0 => print("║ " + f"${ a(0)((j - 1) / 2) }%2d" + " │")
          case (i, j) if j % 2 != 0 && i == n - 1 => print(" " + f"${ a(i)((j - 1) / 2) }%2d" + " ║\n")
          case (i, j) if j % 2 != 0 => print(" " + f"${ a(i)((j - 1) / 2) }%2d" + " │")

          case (0, j) if j == (n * 2) => print("╚════╧")
          case (i, j) if j == (n * 2) && i == n - 1 => print("════╝\n")
          case (i, j) if j == (n * 2) => print("════╧")

          case (0, _) => print("╟────┼")
          case (i, _) if i == n - 1 => print("────╢\n")
          case (i, _) => print("────┼")
        }
      }
    }

    printMagicSquare(7)
  }
```


```txt
╔════╤════╤════╤════╤════╤════╤════╗
║ 30 │ 39 │ 48 │  1 │ 10 │ 19 │ 28 ║
╟────┼────┼────┼────┼────┼────┼────╢
║ 38 │ 47 │  7 │  9 │ 18 │ 27 │ 29 ║
╟────┼────┼────┼────┼────┼────┼────╢
║ 46 │  6 │  8 │ 17 │ 26 │ 35 │ 37 ║
╟────┼────┼────┼────┼────┼────┼────╢
║  5 │ 14 │ 16 │ 25 │ 34 │ 36 │ 45 ║
╟────┼────┼────┼────┼────┼────┼────╢
║ 13 │ 15 │ 24 │ 33 │ 42 │ 44 │  4 ║
╟────┼────┼────┼────┼────┼────┼────╢
║ 21 │ 23 │ 32 │ 41 │ 43 │  3 │ 12 ║
╟────┼────┼────┼────┼────┼────┼────╢
║ 22 │ 31 │ 40 │ 49 │  2 │ 11 │ 20 ║
╚════╧════╧════╧════╧════╧════╧════╝
```



## Seed7


```seed7
$ include "seed7_05.s7i";

const func integer: succ (in integer: num, in integer: max) is
  return succ(num mod max);

const func integer: pred (in integer: num, in integer: max) is
  return succ((num - 2) mod max);

const proc: main is func
  local
    var integer: size is 3;
    var array array integer: magic is 0 times 0 times 0;
    var integer: row is 1;
    var integer: column is 1;
    var integer: number is 0;
  begin
    if length(argv(PROGRAM)) >= 1 then
      size := integer parse (argv(PROGRAM)[1]);
    end if;
    magic := size times size times 0;
    column := succ(size div 2);
    for number range 1 to size ** 2 do
      magic[row][column] := number;
      if magic[pred(row, size)][succ(column, size)] = 0 then
        row := pred(row, size);
        column := succ(column, size);
      else
        row := succ(row, size);
      end if;
    end for;
    for key row range magic do
      for key column range magic[row] do
        write(magic[row][column] lpad 4);
      end for;
      writeln;
    end for;
  end func;
```


```txt

> s7 magicSquaresOfOddOrder 7
SEED7 INTERPRETER Version 5.0.5203  Copyright (c) 1990-2014 Thomas Mertes
  30  39  48   1  10  19  28
  38  47   7   9  18  27  29
  46   6   8  17  26  35  37
   5  14  16  25  34  36  45
  13  15  24  33  42  44   4
  21  23  32  41  43   3  12
  22  31  40  49   2  11  20

```



## Sidef


```ruby
func magic_square(n {.is_pos && .is_odd}) {
    var i = 0
    var j = int(n/2)

    var magic_square = []
    for l in (1 .. n**2) {
        magic_square[i][j] = l

        if (magic_square[i.dec % n][j.inc % n]) {
            i = (i.inc % n)
        }
        else {
            i = (i.dec % n)
            j = (j.inc % n)
        }
    }

    return magic_square
}

func print_square(sq) {
    var f = "%#{(sq.len**2).len}d";
    for row in sq {
        say row.map{ f % _ }.join(' ')
    }
}

var(n=5) = ARGV»to_i»()...
var sq = magic_square(n)
print_square(sq)

say "\nThe magic number is: #{sq[0].sum}"
```


```txt

17 24  1  8 15
23  5  7 14 16
 4  6 13 20 22
10 12 19 21  3
11 18 25  2  9

The magic number is: 65

```


## Swift


```Swift 5
extension String: Error {}

struct Point: CustomStringConvertible {
    var x: Int
    var y: Int

    init(_ _x: Int,
         _ _y: Int) {
        self.x = _x
        self.y = _y
    }

    var description: String {
        return "(\(x), \(y))\n"
    }
}

extension Point: Equatable,Comparable {
    static func == (lhs: Point, rhs: Point) -> Bool {
        return lhs.x == rhs.x && lhs.y == rhs.y
    }
    static func < (lhs: Point, rhs: Point) -> Bool {
        return lhs.y != rhs.y ? lhs.y < rhs.y : lhs.x < rhs.x
    }
}

class MagicSquare: CustomStringConvertible {
    var grid:[Int:Point] = [:]
    var number: Int = 1
    init(base n:Int) {
        grid = [:]
        number = n
    }

    func createOdd() throws -> MagicSquare {
        guard number < 1 || number % 2 != 0 else {
            throw "Must be odd and >= 1, try again"
            return self
        }
        var x = 0
        var y = 0
        let middle = Int(number/2)
        x = middle
        grid[1] = Point(x,y)
        for i in 2 ... number*number {
            let oldXY = Point(x,y)
            x += 1
            y -= 1

            if x >= number {x -= number}
            if y < 0 {y +=  number}

            var tempCoord = Point(x,y)
            if let _ = grid.firstIndex(where: { (k,v) -> Bool in
                v == tempCoord
            })
            {
                x = oldXY.x
                y = oldXY.y + 1
                if y >= number {y -= number}
                tempCoord = Point(x,y)
            }
            grid[i] = tempCoord
        }
        print(self)
        return self
    }

    fileprivate func gridToText(_ result: inout String) {
        let sorted = sortedGrid()
        let sc = sorted.count
        var i = 0
        for c in sorted {
            result += " \(c.key)"
            if c.key < 10 && sc > 10 { result += " "}
            if c.key < 100 && sc > 100 { result += " "}
            if c.key < 1000 && sc > 1000 { result += " "}
            if i%number==(number-1) { result += "\n"}
            i += 1
        }
        result += "\nThe magic number is \(number * (number * number + 1) / 2)"
        result += "\nRows and Columns are "

        result += checkRows() == checkColumns() ? "Equal" : " Not Equal!"
        result += "\nRows and Columns and Diagonals are "
        let allEqual = (checkDiagonals() == checkColumns() && checkDiagonals() == checkRows())
        result += allEqual ? "Equal" : " Not Equal!"
        result += "\n"
    }

    var description: String {
        var result = "base \(number)\n"
        gridToText(&result)
        return result
    }
}

extension MagicSquare {
    private func sortedGrid()->[(key:Int,value:Point)] {
        return grid.sorted(by: {$0.1 < $1.1})
    }

    private func checkRows() -> (Bool, Int?)
    {
        var result = Set<Int>()
        var index = 0
        var rowtotal = 0
        for (cell, _) in sortedGrid()
        {
            rowtotal += cell
            if index%number==(number-1)
            {
                result.insert(rowtotal)
                rowtotal = 0
            }
            index += 1
        }
        return (result.count == 1, result.first ?? nil)
    }

    private func checkColumns() -> (Bool, Int?)
    {
        var result = Set<Int>()
        var sorted = sortedGrid()
        for i in 0 ..< number {
            var rowtotal = 0
            for cell in stride(from: i, to: sorted.count, by: number) {
                rowtotal += sorted[cell].key
            }
            result.insert(rowtotal)
        }
        return (result.count == 1, result.first)
    }

    private func checkDiagonals() -> (Bool, Int?)
    {
        var result = Set<Int>()
        var sorted = sortedGrid()

        var rowtotal = 0
        for cell in stride(from: 0, to: sorted.count, by: number+1) {
            rowtotal += sorted[cell].key
        }
        result.insert(rowtotal)
        rowtotal = 0
        for cell in stride(from: number-1, to: sorted.count-(number-1), by: number-1) {
            rowtotal += sorted[cell].key
        }
        result.insert(rowtotal)

        return (result.count == 1, result.first)
    }
}

try MagicSquare(base: 3).createOdd()
try MagicSquare(base: 5).createOdd()
try MagicSquare(base: 7).createOdd()


```

Demonstrating:
base 3
 8 1 6
 3 5 7
 4 9 2

The magic number is 15<br />
Rows and Columns are Equal<br />
Rows and Columns and Diagonals are Equal<br />

base 5
 17 24 1  8  15
 23 5  7  14 16
 4  6  13 20 22
 10 12 19 21 3
 11 18 25 2  9

The magic number is 65<br />
Rows and Columns are Equal<br />
Rows and Columns and Diagonals are Equal<br />

base 7
 30 39 48 1  10 19 28
 38 47 7  9  18 27 29
 46 6  8  17 26 35 37
 5  14 16 25 34 36 45
 13 15 24 33 42 44 4
 21 23 32 41 43 3  12
 22 31 40 49 2  11 20

The magic number is 175<br />
Rows and Columns are Equal<br />
Rows and Columns and Diagonals are Equal<br />


## Tcl


```tcl
proc magicSquare {order} {
    if {!($order & 1) || $order < 0} {
	error "order must be odd and positive"
    }
    set s [lrepeat $order [lrepeat $order 0]]
    set x [expr {$order / 2}]
    set y 0
    for {set i 1} {$i <= $order**2} {incr i} {
	lset s $y $x $i
	set x [expr {($x + 1) % $order}]
	set y [expr {($y - 1) % $order}]
	if {[lindex $s $y $x]} {
	    set x [expr {($x - 1) % $order}]
	    set y [expr {($y + 2) % $order}]
	}
    }
    return $s
}
```

Demonstrating:
```tcl
package require Tcl 8.6

set square [magicSquare 5]
puts [join [lmap row $square {join [lmap n $row {format "%2s" $n}]}] "\n"]
puts "magic number = [tcl::mathop::+ {*}[lindex $square 0]]"
```

```txt

17 24  1  8 15
23  5  7 14 16
 4  6 13 20 22
10 12 19 21  3
11 18 25  2  9
magic number = 65

```


=={{header|TI-83 BASIC}}==
```ti83b
9→N
DelVar [A]:{N,N}→dim([A])
For(I,1,N)
For(J,1,N)
Remainder(I*2-J+N-1,N)*N+Remainder(I*2+J-2,N)+1→[A](I,J)
End
End
[A]
```

```txt

[[2  75 67 59 51 43 35 27 10]
 [22 14 6  79 71 63 46 38 30]
 [42 34 26 18 1  74 66 58 50]
 [62 54 37 29 21 13 5  78 70]
 [73 65 57 49 41 33 25 17 9 ]
 [12 4  77 69 61 53 45 28 20]
 [32 24 16 8  81 64 56 48 40]
 [52 44 36 19 11 3  76 68 60]
 [72 55 47 39 31 23 15 7  80]]

```



## uBasic/4tH

<lang>' ------=< MAIN >=------

Proc _magicsq(5)
Proc _magicsq(11)
End

_magicsq Param (1) Local (4)

    ' reset the array
    For b@ = 0 to 255
        @(b@) = 0
    Next

    If  ((a@ % 2) = 0) + (a@ < 3) + (a@ > 15) Then
        Print "error: size is not odd or size is smaller then 3 or bigger than 15"
        Return
    EndIf

    ' start in the middle of the first row
    b@ = 1
    c@ = a@ - (a@ / 2)
    d@ = 1
    e@ = a@ * a@

    ' main loop for creating magic square
    Do
        If @(c@*a@+d@) = 0 Then
            @(c@*a@+d@) = b@
            If (b@ % a@) = 0 Then
                d@ = d@ + 1
            Else
                c@ = c@ + 1
                d@ = d@ - 1
            EndIf
            b@ = b@ + 1
        EndIf
        If c@ > a@ Then
            c@ = 1
            Do While @(c@*a@+d@) # 0
                c@ = c@ + 1
            Loop
        EndIf
        If d@ < 1 Then
            d@ = a@
            Do While @(c@*a@+d@) # 0
                d@ = d@ - 1
            Loop
        EndIf
    Until b@ > e@
    Loop

    Print "Odd magic square size: "; a@; " * "; a@
    Print "The magic sum = "; ((e@+1) / 2) * a@
    Print

    For d@ = 1 To a@
        For c@ = 1 To a@
            Print Using "____"; @(c@*a@+d@);
        Next
        Print
    Next
    Print
Return

```

```txt
Odd magic square size: 5 * 5
The magic sum = 65

  17  24   1   8  15
  23   5   7  14  16
   4   6  13  20  22
  10  12  19  21   3
  11  18  25   2   9

Odd magic square size: 11 * 11
The magic sum = 671

  68  81  94 107 120   1  14  27  40  53  66
  80  93 106 119  11  13  26  39  52  65  67
  92 105 118  10  12  25  38  51  64  77  79
 104 117   9  22  24  37  50  63  76  78  91
 116   8  21  23  36  49  62  75  88  90 103
   7  20  33  35  48  61  74  87  89 102 115
  19  32  34  47  60  73  86  99 101 114   6
  31  44  46  59  72  85  98 100 113   5  18
  43  45  58  71  84  97 110 112   4  17  30
  55  57  70  83  96 109 111   3  16  29  42
  56  69  82  95 108 121   2  15  28  41  54


0 OK, 0:64

```



## VBA

Works with Excel VBA.

```vb
Sub magicsquare()
    'Magic squares of odd order
    Const n = 9
    Dim i As Integer, j As Integer, v As Integer
    Debug.Print "The square order is: " & n
    For i = 1 To n
        For j = 1 To n
            Cells(i, j) = ((i * 2 - j + n - 1) Mod n) * n + ((i * 2 + j - 2) Mod n) + 1
        Next j
    Next i
    Debug.Print "The magic number of"; n; "x"; n; "square is:"; n * (n * n + 1) \ 2
End Sub 'magicsquare

```




## VBScript

```vb

Sub magic_square(n)
	Dim ms()
	ReDim ms(n-1,n-1)
	inc = 0
	count = 1
	row = 0
	col = Int(n/2)
	Do While count <= n*n
		ms(row,col) = count
		count = count + 1
		If inc < n-1 Then
			inc = inc + 1
			row = row - 1
			col = col + 1
			If row >= 0 Then
				If col > n-1 Then
					col = 0
				End If
			Else
				row = n-1
			End If
		Else
			inc = 0
			row = row + 1
		End If
	Loop
	For i = 0 To n-1
		For j = 0 To n-1
			If j = n-1 Then
				WScript.StdOut.Write ms(i,j)
			Else
				WScript.StdOut.Write ms(i,j) & vbTab
			End If
		Next
		WScript.StdOut.WriteLine
	Next
End Sub

magic_square(5)

```


```txt

17	24	1	8	15
23	5	7	14	16
4	6	13	20	22
10	12	19	21	3
11	18	25	2	9

```



## Visual Basic

```vb
Sub magicsquare()
    'Magic squares of odd order
    Const n = 9
    Dim i As Integer, j As Integer, v As Integer
    Debug.Print "The square order is: " & n
    For i = 1 To n
        For j = 1 To n
            v = ((i * 2 - j + n - 1) Mod n) * n + ((i * 2 + j - 2) Mod n) + 1
            Debug.Print Right(Space(5) & v, 5);
        Next j
        Debug.Print
    Next i
    Debug.Print "The magic number is: " & n * (n * n + 1) \ 2
End Sub 'magicsquare

```

```txt

The square order is: 9
    2   75   67   59   51   43   35   27   10
   22   14    6   79   71   63   46   38   30
   42   34   26   18    1   74   66   58   50
   62   54   37   29   21   13    5   78   70
   73   65   57   49   41   33   25   17    9
   12    4   77   69   61   53   45   28   20
   32   24   16    8   81   64   56   48   40
   52   44   36   19   11    3   76   68   60
   72   55   47   39   31   23   15    7   80
The magic number is: 369

```




## Visual Basic .NET

```vbnet
Sub magicsquare()
    'Magic squares of odd order
    Const n = 9
    Dim i, j, v As Integer
    Console.WriteLine("The square order is: " & n)
    For i = 1 To n
        For j = 1 To n
            v = ((i * 2 - j + n - 1) Mod n) * n + ((i * 2 + j - 2) Mod n) + 1
            Console.Write(" " & Right(Space(5) & v, 5))
        Next j
        Console.WriteLine("")
    Next i
    Console.WriteLine("The magic number is: " & n * (n * n + 1) \ 2)
End Sub 'magicsquare
```

```txt

The square order is: 9
    2   75   67   59   51   43   35   27   10
   22   14    6   79   71   63   46   38   30
   42   34   26   18    1   74   66   58   50
   62   54   37   29   21   13    5   78   70
   73   65   57   49   41   33   25   17    9
   12    4   77   69   61   53   45   28   20
   32   24   16    8   81   64   56   48   40
   52   44   36   19   11    3   76   68   60
   72   55   47   39   31   23   15    7   80
The magic number is: 369

```



## zkl

```zkl
fcn rmod(n,m){ n=n%m; if (n<0) n+=m; n } // Ruby: -5%3-->1
fcn odd_magic_square(n){ //-->list of n*n numbers, row order
   if (n.isEven or n <= 0) throw(Exception.ValueError("Need odd positive number"));
   [[(i,j); n; n; '{ n*((i+j+1+n/2):rmod(_,n)) + ((i+2*j-5):rmod(_,n)) + 1 }]]
}

T(3, 5, 9).pump(Void,fcn(n){
   "\nSize %d, magic sum %d".fmt(n,(n*n+1)/2*n).println();
   fmt := "%%%dd".fmt((n*n).toString().len() + 1) * n;
   odd_magic_square(n).pump(Console.println,T(Void.Read,n-1),fmt.fmt);
});
```

```txt

Size 3, magic sum 15
 8 1 6
 3 5 7
 4 9 2

Size 5, magic sum 65
 16 23  5  7 14
 22  4  6 13 20
  3 10 12 19 21
  9 11 18 25  2
 15 17 24  1  8

Size 9, magic sum 369
 50 61 72 74  4 15 26 28 39
 60 71 73  3 14 25 36 38 49
 70 81  2 13 24 35 37 48 59
 80  1 12 23 34 45 47 58 69
  9 11 22 33 44 46 57 68 79
 10 21 32 43 54 56 67 78  8
 20 31 42 53 55 66 77  7 18
 30 41 52 63 65 76  6 17 19
 40 51 62 64 75  5 16 27 29

```

