+++
title = "Pascal matrix generation"
description = ""
date = 2019-06-07T04:12:02Z
aliases = []
[extra]
id = 19012
[taxonomies]
categories = []
tags = []
+++

{{task}}

A pascal matrix is a two-dimensional square matrix holding numbers from   [[Pascal's triangle]],   also known as   [[Evaluate binomial coefficients|binomial coefficients]]   and which can be shown as    <big><sup>n</sup>C<sub>r</sub>.</big>

Shown below are truncated   5-by-5   matrices   M[i, j]   for   i,j   in range   0..4. 


A Pascal upper-triangular matrix that is populated with   <big><sup>j</sup>C<sub>i</sub>:</big>

```txt

[[1, 1, 1, 1, 1],
 [0, 1, 2, 3, 4],
 [0, 0, 1, 3, 6],
 [0, 0, 0, 1, 4],
 [0, 0, 0, 0, 1]]

```


A Pascal lower-triangular matrix that is populated with   <big><sup>i</sup>C<sub>j</sub></big>   (the transpose of the upper-triangular matrix):

```txt

[[1, 0, 0, 0, 0],
 [1, 1, 0, 0, 0],
 [1, 2, 1, 0, 0],
 [1, 3, 3, 1, 0],
 [1, 4, 6, 4, 1]]

```


A Pascal symmetric matrix that is populated with   <big><sup>i+j</sup>C<sub>i</sub>:</big> 

```txt

[[1, 1, 1, 1, 1],
 [1, 2, 3, 4, 5],
 [1, 3, 6, 10, 15],
 [1, 4, 10, 20, 35],
 [1, 5, 15, 35, 70]]

```



;Task:
Write functions capable of generating each of the three forms of   n-by-n   matrices.

Use those functions to display upper, lower, and symmetric Pascal   5-by-5   matrices on this page. 

The output should distinguish between different matrices and the rows of each matrix   (no showing a list of 25 numbers assuming the reader should split it into rows).


;Note:  
The   [[Cholesky decomposition]]   of a Pascal symmetric matrix is the Pascal lower-triangle matrix of the same size. 


 


## 360 Assembly


```360asm
*        Pascal matrix generation - 10/06/2018
PASCMATR CSECT
         USING  PASCMATR,R13       base register
         B      72(R15)            skip savearea
         DC     17F'0'             savearea
         SAVE   (14,12)            save previous context
         ST     R13,4(R15)         link backward
         ST     R15,8(R13)         link forward
         LR     R13,R15            set addressability
         MVC    MAT,=F'1'          mat(1,1)=1
         LA     R6,1               i=1
       DO WHILE=(C,R6,LE,N)        do i=1 to n;
         LA     R7,1                 j=1
       DO WHILE=(C,R7,LE,N)          do j=1 to n;       
         LR     R2,R6                  i
         LA     R3,1(R7)               r3=j+1
         LR     R1,R6                  i
         BCTR   R1,0                   -1
         MH     R1,NN                  *nn
         AR     R1,R7                  ~(i,j)
         SLA    R1,2                   *4
         L      R4,MAT-4(R1)           r4=mat(i,j)
         LR     R5,R6                  i
         MH     R5,NN                  *nn
         AR     R5,R7                  ~(i+1,j)
         SLA    R5,2                   *4
         L      R5,MAT-4(R5)           r5=mat(i+1,j)
         AR     R4,R5                  r4=mat(i,j)+mat(i+1,j)
         MH     R2,NN                  *nn
         AR     R2,R3                  ~(i+1,j+1)
         SLA    R2,2                   *4
         ST     R4,MAT-4(R2)           mat(i+1,j+1)=mat(i,j)+mat(i+1,j)
         LA     R7,1(R7)               j++
       ENDDO    ,                    enddo j
         LA     R6,1(R6)             i++
       ENDDO    ,                  enddo i
         MVC    TITLE,=CL20'Upper:'
         BAL    R14,PRINTMAT       call printmat
         MVC    MAT,=F'1'          mat(1,1)=1
         LA     R6,1               i=1
       DO WHILE=(C,R6,LE,N)        do i=1 to n;
         LA     R7,1                 j=1
       DO WHILE=(C,R7,LE,N)          do j=1 to n;       
         LR     R2,R6                  i
         LA     R3,1(R7)               r3=j+1
         LR     R1,R6                  i
         BCTR   R1,0                   -1
         MH     R1,NN                  *nn
         LR     R0,R7                  j
         AR     R1,R0                  ~(i,j)
         SLA    R1,2                   *4
         L      R4,MAT-4(R1)           r4=mat(i,j)
         LA     R5,1(R7)               j+1
         LR     R1,R6                  i
         BCTR   R1,0                   -1
         MH     R1,NN                  *nn
         AR     R1,R5                  ~(i,j+1)
         SLA    R1,2                   *4
         L      R5,MAT-4(R1)           r5=mat(i,j+1)
         AR     R4,R5                  mat(i,j)+mat(i,j+1)
         MH     R2,NN                  *nn
         AR     R2,R3                  ~(i+1,j+1)
         SLA    R2,2                   *4
         ST     R4,MAT-4(R2)           mat(i+1,j+1)=mat(i,j)+mat(i,j+1)
         LA     R7,1(R7)               j++
       ENDDO    ,                    enddo j
         LA     R6,1(R6)             i++
       ENDDO    ,                  enddo i
         MVC    TITLE,=CL20'Lower:'
         BAL    R14,PRINTMAT       call printmat
         MVC    MAT+24,=F'1'       mat(2,1)=1
         LA     R6,1               i=1
       DO WHILE=(C,R6,LE,N)        do i=1 to n;
         LA     R7,1                 j=1
       DO WHILE=(C,R7,LE,N)          do j=1 to n;       
         LR     R2,R6                  i
         LA     R3,1(R7)               r3=j+1                 j
         LR     R1,R6                  i
         BCTR   R1,0                   -1
         MH     R1,NN                  *nn
         AR     R1,R3                  ~(i,j+1)
         SLA    R1,2                   *4
         L      R4,MAT-4(R1)           r4=mat(i,j+1)
         LR     R5,R6                  i
         MH     R5,NN                  *nn
         AR     R5,R7                  j
         SLA    R5,2                   *4
         L      R5,MAT-4(R5)           r5=mat(i+1,j)
         AR     R4,R5                  mat(i,j+1)+mat(i+1,j)
         MH     R2,NN                  *nn
         AR     R2,R3                  ~(i+1,j+1)
         SLA    R2,2                   *4
         ST     R4,MAT-4(R2)         mat(i+1,j+1)=mat(i,j+1)+mat(i+1,j)
         LA     R7,1(R7)               j++
       ENDDO    ,                    enddo j
         LA     R6,1(R6)             i++
       ENDDO    ,                  enddo i
         MVC    TITLE,=CL20'Symmetric:'
         BAL    R14,PRINTMAT       call printmat
         L      R13,4(0,R13)       restore previous savearea pointer
         RETURN (14,12),RC=0       restore registers from calling sav
PRINTMAT XPRNT  TITLE,L'TITLE      print title  -----------------------
         LA     R10,PG             pgi=0
         LA     R6,1               i=1
       DO WHILE=(C,R6,LE,N)        do i=1 to n;
         LA     R7,1                 j=1
       DO WHILE=(C,R7,LE,N)          do j=1 to n;       
         LR     R2,R6                  i
         LR     R3,R7                  j
         LA     R3,1(R3)               j+1
         MH     R2,NN                  *nn
         AR     R2,R3                  ~(i+1,j+1)
         SLA    R2,2                   *4
         L      R2,MAT-4(R2)           mat(i+1,j+1)
         XDECO  R2,XDEC                edit mat(i+1,j+1)
         MVC    0(5,R10),XDEC+7        output mat(i+1,j+1)
         LA     R10,5(R10)             pgi+=5
         LA     R7,1(R7)               j++
       ENDDO    ,                    enddo j
         XPRNT  PG,L'PG              print
         LA     R10,PG               pgi=0
         LA     R6,1(R6)             i++
       ENDDO    ,                  enddo i
         BR     R14                return to caller -------------------
X        EQU    5                  matrix size
N        DC     A(X)               n=x
NN       DC     AL2(X+1)           nn=x+1
MAT      DC     ((X+1)*(X+1))F'0'  mat(x+1,x+1)
TITLE    DC     CL20' '            title
PG       DC     CL80' '            buffer
PGI      DC     H'0'               buffer index
XDEC     DS     CL12               temp
         YREGS
         END    PASCMATR
```

{{out}}

```txt

Upper:
    1    1    1    1    1
    0    1    2    3    4
    0    0    1    3    6
    0    0    0    1    4
    0    0    0    0    1
Lower:
    1    0    0    0    0
    1    1    0    0    0
    1    2    1    0    0
    1    3    3    1    0
    1    4    6    4    1
Symmetric:
    1    1    1    1    1
    1    2    3    4    5
    1    3    6   10   15
    1    4   10   20   35
    1    5   15   35   70

```



## Ada


```ada
-- for I/O
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Float_Text_IO; use Ada.Float_Text_IO;

-- for estimating the maximum width of a column
with Ada.Numerics.Generic_Elementary_Functions;

procedure PascalMatrix is

  type Matrix is array (Positive range <>, Positive range <>) of Natural;

  -- instantiate Generic_Elementary_Functions for Float type
  package Math is new Ada.Numerics.Generic_Elementary_Functions(Float_Type => Float);
  use Math;
  
  procedure Print(m: in Matrix) is
    -- determine the maximum width of a column
    w: Float := Log(Float(m'Length(1)**(m'Length(1)/2)), 10.0);
    width: Positive := Natural(Float'Ceiling(w)) + 1;
    begin
      for i in m'First(1)..m'Last(1) loop
        Put("( ");
        for j in m'First(2)..m'Last(2) loop
          Put(m(i,j), width);
        end loop;
        Put(" )"); New_Line(1);
      end loop;
    end Print;
  
  function Upper_Triangular(n: in Positive) return Matrix is
    result: Matrix(1..n, 1..n) := (
                                    1 => ( others => 1 ),
                                    others => ( others => 0 )
                                  );
    begin
      for i in 2..n loop
        result(i,i) := 1;
        for j in i+1..n loop
          result(i,j) := result(i,j-1) + result(i-1,j-1);
        end loop;
      end loop;
      return result;
    end Upper_Triangular;
  
  function Lower_Triangular(n: in Positive) return Matrix is
    result: Matrix(1..n, 1..n) := (
                                    others => ( 1 => 1, others => 0 )
                                  );
    begin
      for i in 2..n loop
        result(i,i) := 1;
        for j in i+1..n loop
          result(j,i) := result(j-1,i) + result(j-1,i-1);
        end loop;
      end loop;
      return result;
    end Lower_Triangular;
  
  function Symmetric(n: in Positive) return Matrix is
    result: Matrix(1..n, 1..n) := (
                                   1 => ( others => 1 ),
                                   others => ( 1 => 1, others => 0 )
                                  );
    begin
      for i in 2..n loop
        for j in 2..n loop
          result(i,j) := result(i,j-1) + result(i-1,j);
        end loop;
      end loop;
      return result;
    end Symmetric;
  
  n: Positive;
  
  begin
    Put("What dimension Pascal matrix would you like? ");
    Get(n);
    Put("Upper triangular:"); New_Line(1);
    Print(Upper_Triangular(n));
    Put("Lower triangular:"); New_Line(1);
    Print(Lower_Triangular(n));
    Put("Symmetric:"); New_Line(1);
    Print(Symmetric(n));
  end PascalMatrix;
```

{{out}}

```txt
What dimension Pascal matrix would you like? 5
Upper triangular:
(   1  1  1  1  1 )
(   0  1  2  3  4 )
(   0  0  1  3  6 )
(   0  0  0  1  4 )
(   0  0  0  0  1 )
Lower triangular:
(   1  0  0  0  0 )
(   1  1  0  0  0 )
(   1  2  1  0  0 )
(   1  3  3  1  0 )
(   1  4  6  4  1 )
Symmetric:
(   1  1  1  1  1 )
(   1  2  3  4  5 )
(   1  3  6 10 15 )
(   1  4 10 20 35 )
(   1  5 15 35 70 )
```



## ALGOL 68


```algol68
BEGIN
    # returns an upper Pascal matrix of size n #
    PROC upper pascal matrix = ( INT n )[,]INT:
         BEGIN
            [ 1 : n, 1 : n ]INT result;
            FOR j        TO n DO result[ 1, j ] := 1 OD;
            FOR i FROM 2 TO n DO
                result[ i, 1 ] := 0;
                FOR j FROM 2 TO n DO
                    result[ i, j ] := result[ i - 1, j - 1 ] + result[ i, j - 1 ]
                OD
            OD;
            result
         END # upper pascal matrix # ;

    # returns a lower Pascal matrix of size n #
    PROC lower pascal matrix = ( INT n )[,]INT:
         BEGIN
            [ 1 : n, 1 : n ]INT result;
            FOR i        TO n DO result[ i, 1 ] := 1 OD;
            FOR j FROM 2 TO n DO
                result[ 1, j ] := 0;
                FOR i FROM 2 TO n DO
                    result[ i, j ] := result[ i - 1, j - 1 ] + result[ i - 1, j ]
                OD
            OD;
            result
         END # lower pascal matrix # ;

    # returns a symmetric Pascal matrix of size n #
    PROC symmetric pascal matrix = ( INT n )[,]INT:
         BEGIN
            [ 1 : n, 1 : n ]INT result;
            FOR i TO n DO
                result[ i, 1 ] := 1;
                result[ 1, i ] := 1
            OD;
            FOR j FROM 2 TO n DO
                FOR i FROM 2 TO n DO
                    result[ i, j ] := result[ i, j - 1 ] + result[ i - 1, j ]
                OD
            OD;
            result
         END # symmetric pascal matrix # ;

    # print the matrix m with the specified field width #
    PROC print matrix = ( [,]INT m, INT field width )VOID:
         BEGIN
             FOR i FROM 1 LWB m TO 1 UPB m DO
                 FOR j FROM 2 LWB m TO 2 UPB m DO
                     print( ( " ", whole( m[ i, j ], - field width ) ) )
                 OD;
                 print( ( newline ) )
             OD
         END # print matrix # ;

    print( ( "upper:",     newline ) ); print matrix( upper pascal matrix(     5 ), 2 );
    print( ( "lower:",     newline ) ); print matrix( lower pascal matrix(     5 ), 2 );
    print( ( "symmetric:", newline ) ); print matrix( symmetric pascal matrix( 5 ), 2 )

END
```

{{out}}

```txt

upper:
  1  1  1  1  1
  0  1  2  3  4
  0  0  1  3  6
  0  0  0  1  4
  0  0  0  0  1
lower:
  1  0  0  0  0
  1  1  0  0  0
  1  2  1  0  0
  1  3  3  1  0
  1  4  6  4  1
symmetric:
  1  1  1  1  1
  1  2  3  4  5
  1  3  6 10 15
  1  4 10 20 35
  1  5 15 35 70

```



## ALGOL W

{{Trans|ALGOL_68}}

```algolw
begin
    % initialises m to an upper Pascal matrix of size n %
    % the bounds of m must be at least 1 :: n, 1 :: n   %
    procedure upperPascalMatrix ( integer array m( *, * )
                                ; integer value n
                                ) ;
    begin
        for j := 1 until n do m( 1, j ) := 1;
        for i := 2 until n do begin
            m( i, 1 ) := 0;
            for j := 2 until n do m( i, j ) := m( i - 1, j - 1 ) + m( i, j - 1 )
        end for_i
    end upperPascalMatrix ;

    % initialises m to a lower Pascal matrix of size n  %
    % the bounds of m must be at least 1 :: n, 1 :: n   %
    procedure lowerPascalMatrix ( integer array m( *, * )
                               ; integer value n
                               ) ;
    begin
        for i := 1 until n do m( i, 1 ) := 1;
        for j := 2 until n do begin
            m( 1, j ) := 0;
            for i := 2 until n do m( i, j ) := m( i - 1, j - 1 ) + m( i - 1, j )
        end for_j
    end lowerPascalMatrix ;

    % initialises m to a symmetric Pascal matrix of size n %
    % the bounds of m must be at least 1 :: n, 1 :: n   %
    procedure symmetricPascalMatrix ( integer array m( *, * )
                                    ; integer value n
                                    ) ;
    begin
        for i := 1 until n do begin
            m( i, 1 ) := 1;
            m( 1, i ) := 1
        end for_i;
        for j := 2 until n do for i := 2 until n do m( i, j ) := m( i, j - 1 ) + m( i - 1, j )
    end symmetricPascalMatrix ;

    begin % test the pascal matrix procedures %

        % print the matrix m with the specified field width %
        % the bounds of m must be at least 1 :: n, 1 :: n   %
        procedure printMatrix ( integer array m( *, * )
                              ; integer value n
                              ; integer value fieldWidth
                              ) ;
        begin
            for i := 1 until n do begin
                write(                         i_w := fieldWidth, s_w := 0, " ", m( i, 1 ) );
                for j := 2 until n do writeon( i_w := fieldWidth, s_w := 0, " ", m( i, j ) )
            end for_i
        end printMatrix ;

        integer array m( 1 :: 10, 1 :: 10 );
        integer n, w;

        n := 5; w := 2;
        upperPascalMatrix(     m, n ); write( "upper:"     ); printMatrix( m, n, w );
        lowerPascalMatrix(     m, n ); write( "lower:"     ); printMatrix( m, n, w );
        symmetricPascalMatrix( m, n ); write( "symmetric:" ); printMatrix( m, n, w )

    end

end.
```

{{out}}

```txt

upper:
  1  1  1  1  1
  0  1  2  3  4
  0  0  1  3  6
  0  0  0  1  4
  0  0  0  0  1
lower:
  1  0  0  0  0
  1  1  0  0  0
  1  2  1  0  0
  1  3  3  1  0
  1  4  6  4  1
symmetric:
  1  1  1  1  1
  1  2  3  4  5
  1  3  6 10 15
  1  4 10 20 35
  1  5 15 35 70

```



## AppleScript

By composition of generic functions:

```AppleScript
-- PASCAL MATRIX -------------------------------------------------------------

-- pascalMatrix :: ((Int, Int) -> (Int, Int)) -> Int -> [[Int]]
on pascalMatrix(f, n)
    chunksOf(n, map(compose(my bc, f), range({{0, 0}, {n - 1, n - 1}})))
end pascalMatrix

-- Binomial coefficient
-- bc :: (Int, Int) -> Int
on bc(nk)
    set {n, k} to nk
    script bc_
        on |λ|(a, x)
            floor((a * (n - x + 1)) / x)
        end |λ|
    end script
    foldl(bc_, 1, enumFromTo(1, k))
end bc


-- TEST ----------------------------------------------------------------------
on run
    set matrixSize to 5
    
    script symm
        on |λ|(ab)
            set {a, b} to ab
            {a + b, a}
        end |λ|
    end script
    
    script format
        on |λ|(s, xs)
            unlines(concat({{s}, map(my show, xs), {""}}))
        end |λ|
    end script
    
    unlines(zipWith(format, ¬
        {"Lower", "Upper", "Symmetric"}, ¬
        |<*>|(map(curry(pascalMatrix), [|id|, swap, symm]), {matrixSize})))
end run


-- GENERIC FUNCTIONS ---------------------------------------------------------

-- A list of functions applied to a list of arguments
-- (<*> | ap) :: [(a -> b)] -> [a] -> [b]
on |<*>|(fs, xs)
    set {nf, nx} to {length of fs, length of xs}
    set acc to {}
    repeat with i from 1 to nf
        tell mReturn(item i of fs)
            repeat with j from 1 to nx
                set end of acc to |λ|(contents of (item j of xs))
            end repeat
        end tell
    end repeat
    return acc
end |<*>|

-- chunksOf :: Int -> [a] -> [[a]]
on chunksOf(k, xs)
    script
        on go(ys)
            set {a, b} to splitAt(k, ys)
            if isNull(a) then
                {}
            else
                {a} & go(b)
            end if
        end go
    end script
    result's go(xs)
end chunksOf

-- compose :: (b -> c) -> (a -> b) -> (a -> c)
on compose(f, g)
    script
        on |λ|(x)
            mReturn(f)'s |λ|(mReturn(g)'s |λ|(x))
        end |λ|
    end script
end compose

-- concat :: [[a]] -> [a] | [String] -> String
on concat(xs)
    if length of xs > 0 and class of (item 1 of xs) is string then
        set acc to ""
    else
        set acc to {}
    end if
    repeat with i from 1 to length of xs
        set acc to acc & item i of xs
    end repeat
    acc
end concat

-- cons :: a -> [a] -> [a]
on cons(x, xs)
    {x} & xs
end cons

-- curry :: (Script|Handler) -> Script
on curry(f)
    script
        on |λ|(a)
            script
                on |λ|(b)
                    |λ|(a, b) of mReturn(f)
                end |λ|
            end script
        end |λ|
    end script
end curry

-- enumFromTo :: Int -> Int -> [Int]
on enumFromTo(m, n)
    set lst to {}
    repeat with i from m to n
        set end of lst to i
    end repeat
    return lst
end enumFromTo

-- floor :: Num -> Int
on floor(x)
    if x < 0 and x mod 1 is not 0 then
        (x div 1) - 1
    else
        (x div 1)
    end if
end floor

-- foldl :: (a -> b -> a) -> a -> [b] -> a
on foldl(f, startValue, xs)
    tell mReturn(f)
        set v to startValue
        set lng to length of xs
        repeat with i from 1 to lng
            set v to |λ|(v, item i of xs, i, xs)
        end repeat
        return v
    end tell
end foldl

-- foldr :: (b -> a -> a) -> a -> [b] -> a
on foldr(f, startValue, xs)
    tell mReturn(f)
        set v to startValue
        set lng to length of xs
        repeat with i from lng to 1 by -1
            set v to |λ|(item i of xs, v, i, xs)
        end repeat
        return v
    end tell
end foldr

-- id :: a -> a
on |id|(x)
    x
end |id|

-- intercalate :: Text -> [Text] -> Text
on intercalate(strText, lstText)
    set {dlm, my text item delimiters} to {my text item delimiters, strText}
    set strJoined to lstText as text
    set my text item delimiters to dlm
    return strJoined
end intercalate

-- isNull :: [a] -> Bool
on isNull(xs)
    if class of xs is string then
        xs = ""
    else
        xs = {}
    end if
end isNull

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

-- min :: Ord a => a -> a -> a
on min(x, y)
    if y < x then
        y
    else
        x
    end if
end min

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

-- quot :: Int -> Int -> Int
on quot(m, n)
    m div n
end quot

-- range :: Ix a => (a, a) -> [a]
on range({a, b})
    if class of a is list then
        set {xs, ys} to {a, b}
    else
        set {xs, ys} to {{a}, {b}}
    end if
    set lng to length of xs
    
    if lng = length of ys then
        if lng > 1 then
            script
                on |λ|(_, i)
                    enumFromTo(item i of xs, item i of ys)
                end |λ|
            end script
            sequence(map(result, xs))
        else
            enumFromTo(a, b)
        end if
    else
        {}
    end if
end range

-- sequence :: Monad m => [m a] -> m [a]
-- sequence :: [a] -> [[a]]
on sequence(xs)
    traverse(|id|, xs)
end sequence

-- show :: a -> String
on show(e)
    set c to class of e
    if c = list then
        script serialized
            on |λ|(v)
                show(v)
            end |λ|
        end script
        
        "[" & intercalate(", ", map(serialized, e)) & "]"
    else if c = record then
        script showField
            on |λ|(kv)
                set {k, ev} to kv
                "\"" & k & "\":" & show(ev)
            end |λ|
        end script
        
        "{" & intercalate(", ", ¬
            map(showField, zip(allKeys(e), allValues(e)))) & "}"
    else if c = date then
        "\"" & iso8601Z(e) & "\""
    else if c = text then
        "\"" & e & "\""
    else if (c = integer or c = real) then
        e as text
    else if c = class then
        "null"
    else
        try
            e as text
        on error
            ("«" & c as text) & "»"
        end try
    end if
end show

-- splitAt :: Int -> [a] -> ([a],[a])
on splitAt(n, xs)
    if n > 0 and n < length of xs then
        if class of xs is text then
            {items 1 thru n of xs as text, items (n + 1) thru -1 of xs as text}
        else
            {items 1 thru n of xs, items (n + 1) thru -1 of xs}
        end if
    else
        if n < 1 then
            {{}, xs}
        else
            {xs, {}}
        end if
    end if
end splitAt

-- swap :: (a, b) -> (b, a)
on swap(ab)
    set {a, b} to ab
    {b, a}
end swap

-- traverse :: (a -> [b]) -> [a] -> [[b]]
on traverse(f, xs)
    script
        property mf : mReturn(f)
        on |λ|(x, a)
            |<*>|(map(curry(cons), mf's |λ|(x)), a)
        end |λ|
    end script
    foldr(result, {{}}, xs)
end traverse

-- unlines :: [String] -> String
on unlines(xs)
    intercalate(linefeed, xs)
end unlines

-- zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
on zipWith(f, xs, ys)
    set lng to min(length of xs, length of ys)
    set lst to {}
    tell mReturn(f)
        repeat with i from 1 to lng
            set end of lst to |λ|(item i of xs, item i of ys)
        end repeat
        return lst
    end tell
end zipWith
```

{{Out}}

```txt
Lower
[1, 0, 0, 0, 0]
[1, 1, 0, 0, 0]
[1, 2, 1, 0, 0]
[1, 3, 3, 1, 0]
[1, 4, 6, 4, 1]

Upper
[1, 1, 1, 1, 1]
[0, 1, 2, 3, 4]
[0, 0, 1, 3, 6]
[0, 0, 0, 1, 4]
[0, 0, 0, 0, 1]

Symmetric
[1, 1, 1, 1, 1]
[1, 2, 3, 4, 5]
[1, 3, 6, 10, 15]
[1, 4, 10, 20, 35]
[1, 5, 15, 35, 70]
```



## C


```c

#include <stdio.h>
#include <stdlib.h>

void pascal_low(int **mat, int n) {
    int i, j;

    for (i = 0; i < n; ++i)
        for (j = 0; j < n; ++j)
            if (i < j)
                mat[i][j] = 0;
            else if (i == j || j == 0)
                mat[i][j] = 1;
            else
                mat[i][j] = mat[i - 1][j - 1] + mat[i - 1][j];
}

void pascal_upp(int **mat, int n) {
    int i, j;

    for (i = 0; i < n; ++i)
        for (j = 0; j < n; ++j)
            if (i > j)
                mat[i][j] = 0;
            else if (i == j || i == 0)
                mat[i][j] = 1;
            else
                mat[i][j] = mat[i - 1][j - 1] + mat[i][j - 1];
}

void pascal_sym(int **mat, int n) {
    int i, j;

    for (i = 0; i < n; ++i)
        for (j = 0; j < n; ++j)
            if (i == 0 || j == 0)
                mat[i][j] = 1;
            else
                mat[i][j] = mat[i - 1][j] + mat[i][j - 1];
}

int main(int argc, char * argv[]) {
    int **mat;
    int i, j, n;

    /* Input size of the matrix */
    n = 5;

    /* Matrix allocation */
    mat = calloc(n, sizeof(int *));
    for (i = 0; i < n; ++i)
        mat[i] = calloc(n, sizeof(int));

    /* Matrix computation */
    printf("
###  Pascal upper matrix 
\n");
    pascal_upp(mat, n);
    for (i = 0; i < n; i++)
        for (j = 0; j < n; j++)
            printf("%4d%c", mat[i][j], j < n - 1 ? ' ' : '\n');

    printf("
###  Pascal lower matrix 
\n");
    pascal_low(mat, n);
    for (i = 0; i < n; i++)
        for (j = 0; j < n; j++)
            printf("%4d%c", mat[i][j], j < n - 1 ? ' ' : '\n');

    printf("
###  Pascal symmetric matrix 
\n");
    pascal_sym(mat, n);
    for (i = 0; i < n; i++)
        for (j = 0; j < n; j++)
            printf("%4d%c", mat[i][j], j < n - 1 ? ' ' : '\n');

    return 0;
}

```

{{out}}

```txt


###  Pascal upper matrix 

   1    1    1    1    1
   0    1    2    3    4
   0    0    1    3    6
   0    0    0    1    4
   0    0    0    0    1

###  Pascal lower matrix 

   1    0    0    0    0
   1    1    0    0    0
   1    2    1    0    0
   1    3    3    1    0
   1    4    6    4    1

###  Pascal symmetric matrix 

   1    1    1    1    1
   1    2    3    4    5
   1    3    6   10   15
   1    4   10   20   35
   1    5   15   35   70

```



## C++

{{works with|GCC|version 7.2.0 (Ubuntu 7.2.0-8ubuntu3.2) }}

```cpp>#include <iostream

#include <vector>

typedef std::vector<std::vector<int>> vv;

vv pascal_upper(int n) {
    vv matrix(n);
    for (int i = 0; i < n; ++i) {
        for (int j = 0; j < n; ++j) {
                if (i > j) matrix[i].push_back(0);
                else if (i == j || i == 0) matrix[i].push_back(1);
                else matrix[i].push_back(matrix[i - 1][j - 1] + matrix[i][j - 1]);
            }
        }
        return matrix;
    }

vv pascal_lower(int n) {
    vv matrix(n);
    for (int i = 0; i < n; ++i) {
        for (int j = 0; j < n; ++j) {
            if (i < j) matrix[i].push_back(0);
            else if (i == j || j == 0) matrix[i].push_back(1);
            else matrix[i].push_back(matrix[i - 1][j - 1] + matrix[i - 1][j]);
        }
    }
    return matrix;
}

vv pascal_symmetric(int n) {
    vv matrix(n);
    for (int i = 0; i < n; ++i) {
        for (int j = 0; j < n; ++j) {
            if (i == 0 || j == 0) matrix[i].push_back(1);
            else matrix[i].push_back(matrix[i][j - 1] + matrix[i - 1][j]);
        }
    }
    return matrix;
}


void print_matrix(vv matrix) {
    for (std::vector<int> v: matrix) {
        for (int i: v) {
            std::cout << " " << i;
        }
        std::cout << std::endl;
    }
}

int main() {
    std::cout << "PASCAL UPPER MATRIX" << std::endl;
    print_matrix(pascal_upper(5));
    std::cout << "PASCAL LOWER MATRIX" << std::endl;
    print_matrix(pascal_lower(5));
    std::cout << "PASCAL SYMMETRIC MATRIX" << std::endl;
    print_matrix(pascal_symmetric(5));
}
```

{{out}}

```txt

PASCAL UPPER MATRIX
 1 1 1 1 1
 0 1 2 3 4
 0 0 1 3 6
 0 0 0 1 4
 0 0 0 0 1
PASCAL LOWER MATRIX
 1 0 0 0 0
 1 1 0 0 0
 1 2 1 0 0
 1 3 3 1 0
 1 4 6 4 1
PASCAL SYMMETRIC MATRIX
 1 1 1 1 1
 1 2 3 4 5
 1 3 6 10 15
 1 4 10 20 35
 1 5 15 35 70

```



## C sharp


```csharp
using System;

public static class PascalMatrixGeneration
{
    public static void Main() {
        Print(GenerateUpper(5));
        Console.WriteLine();
        Print(GenerateLower(5));
        Console.WriteLine();
        Print(GenerateSymmetric(5));
    }

    static int[,] GenerateUpper(int size) {
        int[,] m = new int[size, size];
        for (int c = 0; c < size; c++) m[0, c] = 1;
        for (int r = 1; r < size; r++) {
            for (int c = r; c < size; c++) {
                m[r, c] = m[r-1, c-1] + m[r, c-1];
            }
        }
        return m;
    }

    static int[,] GenerateLower(int size) {
        int[,] m = new int[size, size];
        for (int r = 0; r < size; r++) m[r, 0] = 1;
        for (int c = 1; c < size; c++) {
            for (int r = c; r < size; r++) {
                m[r, c] = m[r-1, c-1] + m[r-1, c];
            }
        }
        return m;
    }

    static int[,] GenerateSymmetric(int size) {
        int[,] m = new int[size, size];
        for (int i = 0; i < size; i++) m[0, i] = m[i, 0] = 1;
        for (int r = 1; r < size; r++) {
            for (int c = 1; c < size; c++) {
                m[r, c] = m[r-1, c] + m[r, c-1];
            }
        }
        return m;
    }

    static void Print(int[,] matrix) {
        string[,] m = ToString(matrix);
        int width = m.Cast<string>().Select(s => s.Length).Max();
        int rows = matrix.GetLength(0), columns = matrix.GetLength(1);
        for (int row = 0; row < rows; row++) {
            Console.WriteLine("|" + string.Join(" ", Range(0, columns).Select(column => m[row, column].PadLeft(width, ' '))) + "|");
        }
    }

    static string[,] ToString(int[,] matrix) {
        int rows = matrix.GetLength(0), columns = matrix.GetLength(1);
        string[,] m = new string[rows, columns];
        for (int r = 0; r < rows; r++) {
            for (int c = 0; c < columns; c++) {
                m[r, c] = matrix[r, c].ToString();
            }
        }
        return m;
    }
    
}
```

{{out}}

```txt

|1 1 1 1 1|
|0 1 2 3 4|
|0 0 1 3 6|
|0 0 0 1 4|
|0 0 0 0 1|

|1 0 0 0 0|
|1 1 0 0 0|
|1 2 1 0 0|
|1 3 3 1 0|
|1 4 6 4 1|

| 1  1  1  1  1|
| 1  2  3  4  5|
| 1  3  6 10 15|
| 1  4 10 20 35|
| 1  5 15 35 70|
```



## Common Lisp


```lisp
(defun pascal-lower (n &aux (a (make-array (list n n) :initial-element 0)))
    (dotimes (i n)
        (setf (aref a i 0) 1))
    (dotimes (i (1- n) a)
        (dotimes (j (1- n))
            (setf (aref a (1+ i) (1+ j))
                (+ (aref a i j)
                   (aref a i (1+ j)))))))
                   
(defun pascal-upper (n &aux (a (make-array (list n n) :initial-element 0)))
    (dotimes (i n)
        (setf (aref a 0 i) 1))
    (dotimes (i (1- n) a)
        (dotimes (j (1- n))
            (setf (aref a (1+ j) (1+ i))
                (+ (aref a j i)
                   (aref a (1+ j) i))))))

(defun pascal-symmetric (n &aux (a (make-array (list n n) :initial-element 0)))
    (dotimes (i n)
        (setf (aref a i 0) 1 (aref a 0 i) 1))
    (dotimes (i (1- n) a)
        (dotimes (j (1- n))
            (setf (aref a (1+ i) (1+ j))
                (+ (aref a (1+ i) j)
                   (aref a i (1+ j)))))))

? (pascal-lower 4)
#2A((1 0 0 0) (1 1 0 0) (1 2 1 0) (1 3 3 1))
? (pascal-upper 4)
#2A((1 1 1 1) (0 1 2 3) (0 0 1 3) (0 0 0 1))
? (pascal-symmetric 4)
#2A((1 1 1 1) (1 2 3 4) (1 3 6 10) (1 4 10 20))

;In case one really insists in printing the array row by row:

(defun print-matrix (a)
    (let ((p (array-dimension a 0))
          (q (array-dimension a 1)))
        (dotimes (i p)
            (dotimes (j q)
                (princ (aref a i j))
                (princ #\Space))
            (terpri))))

? (print-matrix (pascal-lower 5))
1 0 0 0 0
1 1 0 0 0
1 2 1 0 0
1 3 3 1 0
1 4 6 4 1

? (print-matrix (pascal-upper 5))
1 1 1 1 1
0 1 2 3 4
0 0 1 3 6
0 0 0 1 4
0 0 0 0 1

? (print-matrix (pascal-symmetric 5))
1 1 1 1 1
1 2 3 4 5
1 3 6 10 15
1 4 10 20 35
1 5 15 35 70
```



## D

{{trans|Python}}

```d
import std.stdio, std.bigint, std.range, std.algorithm;

auto binomialCoeff(in uint n, in uint k) pure nothrow {
    BigInt result = 1;
    foreach (immutable i; 1 .. k + 1)
        result = result * (n - i + 1) / i;
    return result;
}

auto pascalUpp(in uint n) pure nothrow {
    return n.iota.map!(i => n.iota.map!(j => binomialCoeff(j, i)));
}

auto pascalLow(in uint n) pure nothrow {
    return n.iota.map!(i => n.iota.map!(j => binomialCoeff(i, j)));
}

auto pascalSym(in uint n) pure nothrow {
    return n.iota.map!(i => n.iota.map!(j => binomialCoeff(i + j, i)));
}

void main() {
    enum n = 5;
    writefln("Upper:\n%(%(%2d %)\n%)", pascalUpp(n));
    writefln("\nLower:\n%(%(%2d %)\n%)", pascalLow(n));
    writefln("\nSymmetric:\n%(%(%2d %)\n%)", pascalSym(n));
}
```

{{out}}

```txt
Upper:
 1  1  1  1  1
 0  1  2  3  4
 0  0  1  3  6
 0  0  0  1  4
 0  0  0  0  1

Lower:
 1  0  0  0  0
 1  1  0  0  0
 1  2  1  0  0
 1  3  3  1  0
 1  4  6  4  1

Symmetric:
 1  1  1  1  1
 1  2  3  4  5
 1  3  6 10 15
 1  4 10 20 35
 1  5 15 35 70
```



## Elixir


```elixir
defmodule Pascal do
  defp ij(n), do: for i <- 1..n, j <- 1..n, do: {i,j}
  
  def upper_triangle(n) do
    Enum.reduce(ij(n), Map.new, fn {i,j},acc ->
      val = cond do
              i==1 -> 1
              j<i  -> 0
              true -> Map.get(acc, {i-1, j-1}) + Map.get(acc, {i, j-1})
            end
      Map.put(acc, {i,j}, val)
    end) |> print(1..n)
  end
  
  def lower_triangle(n) do
    Enum.reduce(ij(n), Map.new, fn {i,j},acc ->
      val = cond do
              j==1 -> 1
              i<j  -> 0
              true -> Map.get(acc, {i-1, j-1}) + Map.get(acc, {i-1, j})
            end
      Map.put(acc, {i,j}, val)
    end) |> print(1..n)
  end
  
  def symmetic_triangle(n) do
    Enum.reduce(ij(n), Map.new, fn {i,j},acc ->
      val = if i==1 or j==1, do: 1,
                           else: Map.get(acc, {i-1, j}) + Map.get(acc, {i, j-1})
      Map.put(acc, {i,j}, val)
    end) |> print(1..n)
  end
  
  def print(matrix, range) do
    Enum.each(range, fn i ->
      Enum.map(range, fn j -> Map.get(matrix, {i,j}) end) |> IO.inspect
    end)
  end
end

IO.puts "Pascal upper-triangular matrix:"
Pascal.upper_triangle(5)
IO.puts "Pascal lower-triangular matrix:"
Pascal.lower_triangle(5)
IO.puts "Pascal symmetric matrix:"
Pascal.symmetic_triangle(5)
```


{{out}}

```txt

Pascal upper-triangular matrix:
[1, 1, 1, 1, 1]
[0, 1, 2, 3, 4]
[0, 0, 1, 3, 6]
[0, 0, 0, 1, 4]
[0, 0, 0, 0, 1]
Pascal lower-triangular matrix:
[1, 0, 0, 0, 0]
[1, 1, 0, 0, 0]
[1, 2, 1, 0, 0]
[1, 3, 3, 1, 0]
[1, 4, 6, 4, 1]
Pascal symmetric matrix:
[1, 1, 1, 1, 1]
[1, 2, 3, 4, 5]
[1, 3, 6, 10, 15]
[1, 4, 10, 20, 35]
[1, 5, 15, 35, 70]

```



## Factor

{{works with|Factor|0.99}}

```factor
USING: arrays fry io kernel math math.combinatorics
math.matrices prettyprint sequences ;
IN: rosetta-code.pascal-matrix

: pascal ( n quot -- m )
    '[
        dup 2array matrix-coordinates [ first2 @ nCk ]
        matrix-map
    ] call ; inline

: lower ( n -- m ) [ ] pascal ;
: upper ( n -- m ) lower flip ;
: symmetric ( n -- m ) [ dup [ + ] dip ] pascal ;

: pascal-matrix-demo ( -- )
    5 [ lower "Lower:" ] [ upper "Upper:" ]
    [ symmetric "Symmetric:" ] tri
    [ print simple-table. nl ] 2tri@ ;

MAIN: pascal-matrix-demo
```

{{out}}

```txt

Lower:
1 0 0 0 0
1 1 0 0 0
1 2 1 0 0
1 3 3 1 0
1 4 6 4 1

Upper:
1 1 1 1 1
0 1 2 3 4
0 0 1 3 6
0 0 0 1 4
0 0 0 0 1

Symmetric:
1 1 1  1  1
1 2 3  4  5
1 3 6  10 15
1 4 10 20 35
1 5 15 35 70

```



## Fortran

The following program uses features of Fortran 2003.


```fortran
module pascal

implicit none

contains
    function pascal_lower(n) result(a)
        integer :: n, i, j
        integer, allocatable :: a(:, :)
        allocate(a(n, n))
        a = 0
        do i = 1, n
            a(i, 1) = 1
        end do
        do i = 2, n
            do j = 2, i
                a(i, j) = a(i - 1, j) + a(i - 1, j - 1)
            end do
        end do
    end function
    
    function pascal_upper(n) result(a)
        integer :: n, i, j
        integer, allocatable :: a(:, :)
        allocate(a(n, n))
        a = 0
        do i = 1, n
            a(1, i) = 1
        end do
        do i = 2, n
            do j = 2, i
                a(j, i) = a(j, i - 1) + a(j - 1, i - 1)
            end do
        end do
    end function

    function pascal_symmetric(n) result(a)
        integer :: n, i, j
        integer, allocatable :: a(:, :)
        allocate(a(n, n))
        a = 0
        do i = 1, n
            a(i, 1) = 1
            a(1, i) = 1
        end do
        do i = 2, n
            do j = 2, n
                a(i, j) = a(i - 1, j) + a(i, j - 1)
            end do
        end do
    end function

    subroutine print_matrix(a)
        integer :: a(:, :)
        integer :: n, i
        n = ubound(a, 1)
        do i = 1, n
            print *, a(i, :)
        end do
    end subroutine
end module

program ex_pascal
    use pascal
    implicit none
    integer :: n
    integer, allocatable :: a(:, :)
    print *, "Size?"
    read *, n
    print *, "Lower Pascal Matrix"
    a = pascal_lower(n)
    call print_matrix(a)
    print *, "Upper Pascal Matrix"
    a = pascal_upper(n)
    call print_matrix(a)
    print *, "Symmetric Pascal Matrix"
    a = pascal_symmetric(n)
    call print_matrix(a)
end program
```


<lang> Size?
5
 Lower Pascal Matrix
           1           0           0           0           0
           1           1           0           0           0
           1           2           1           0           0
           1           3           3           1           0
           1           4           6           4           1
 Upper Pascal Matrix
           1           1           1           1           1
           0           1           2           3           4
           0           0           1           3           6
           0           0           0           1           4
           0           0           0           0           1
 Symmetric Pascal Matrix
           1           1           1           1           1
           1           2           3           4           5
           1           3           6          10          15
           1           4          10          20          35
           1           5          15          35          70
```



## Go

{{trans|Kotlin}}

```go
package main

import (
    "fmt"
    "strings"
)

func binomial(n, k int) int {
    if n < k {
        return 0
    }
    if n == 0 || k == 0 {
        return 1
    }
    num := 1
    for i := k + 1; i <= n; i++ {
        num *= i
    }
    den := 1
    for i := 2; i <= n-k; i++ {
        den *= i
    }
    return num / den
}

func pascalUpperTriangular(n int) [][]int {
    m := make([][]int, n)
    for i := 0; i < n; i++ {
        m[i] = make([]int, n)
        for j := 0; j < n; j++ {
            m[i][j] = binomial(j, i)
        }
    }
    return m
}

func pascalLowerTriangular(n int) [][]int {
    m := make([][]int, n)
    for i := 0; i < n; i++ {
        m[i] = make([]int, n)
        for j := 0; j < n; j++ {
            m[i][j] = binomial(i, j)
        }
    }
    return m
}

func pascalSymmetric(n int) [][]int {
    m := make([][]int, n)
    for i := 0; i < n; i++ {
        m[i] = make([]int, n)
        for j := 0; j < n; j++ {
            m[i][j] = binomial(i+j, i)
        }
    }
    return m
}

func printMatrix(title string, m [][]int) {
    n := len(m)
    fmt.Println(title)
    fmt.Print("[")
    for i := 0; i < n; i++ {
        if i > 0 {
            fmt.Print(" ")
        }
        mi := strings.Replace(fmt.Sprint(m[i]), " ", ", ", -1)
        fmt.Print(mi)
        if i < n-1 {
            fmt.Println(",")
        } else {
            fmt.Println("]\n")
        }
    }
}

func main() {
    printMatrix("Pascal upper-triangular matrix", pascalUpperTriangular(5))
    printMatrix("Pascal lower-triangular matrix", pascalLowerTriangular(5))
    printMatrix("Pascal symmetric matrix", pascalSymmetric(5))
}
```


{{out}}

```txt

Pascal upper-triangular matrix
[[1, 1, 1, 1, 1],
 [0, 1, 2, 3, 4],
 [0, 0, 1, 3, 6],
 [0, 0, 0, 1, 4],
 [0, 0, 0, 0, 1]]

Pascal lower-triangular matrix
[[1, 0, 0, 0, 0],
 [1, 1, 0, 0, 0],
 [1, 2, 1, 0, 0],
 [1, 3, 3, 1, 0],
 [1, 4, 6, 4, 1]]

Pascal symmetric matrix
[[1, 1, 1, 1, 1],
 [1, 2, 3, 4, 5],
 [1, 3, 6, 10, 15],
 [1, 4, 10, 20, 35],
 [1, 5, 15, 35, 70]]

```



## Haskell


```haskell
import Data.List (transpose)
import System.Environment (getArgs)
import Text.Printf (printf)

-- Pascal's triangle.
pascal :: [[Int]]
pascal = iterate (\row -> 1 : zipWith (+) row (tail row) ++ [1]) [1]

-- The n by n Pascal lower triangular matrix.
pascLow :: Int -> [[Int]]
pascLow n = zipWith (\row i -> row ++ replicate (n-i) 0) (take n pascal) [1..]

-- The n by n Pascal upper triangular matrix.
pascUp :: Int -> [[Int]]
pascUp = transpose . pascLow

-- The n by n Pascal symmetric matrix.
pascSym :: Int -> [[Int]]
pascSym n = take n . map (take n) . transpose $ pascal

-- Format and print a matrix.
printMat :: String -> [[Int]] -> IO ()
printMat title mat = do
  putStrLn $ title ++ "\n"
  mapM_ (putStrLn . concatMap (printf " %2d")) mat
  putStrLn "\n"

main :: IO ()
main = do
  ns <- fmap (map read) getArgs
  case ns of
    [n] -> do printMat "Lower triangular" $ pascLow n
              printMat "Upper triangular" $ pascUp  n
              printMat "Symmetric"        $ pascSym n
    _   -> error "Usage: pascmat <number>"
```


{{out}}

```txt

Lower triangular

  1  0  0  0  0
  1  1  0  0  0
  1  2  1  0  0
  1  3  3  1  0
  1  4  6  4  1


Upper triangular

  1  1  1  1  1
  0  1  2  3  4
  0  0  1  3  6
  0  0  0  1  4
  0  0  0  0  1


Symmetric

  1  1  1  1  1
  1  2  3  4  5
  1  3  6 10 15
  1  4 10 20 35
  1  5 15 35 70

```



Or, in terms of binomial coefficients and coordinate transformations:


```haskell
import Data.Ix (range)
import Data.Tuple (swap)
import Data.List.Split (chunksOf)

-- (Transform on coordinate pair) -> Matrix size -> Matrix values
pascalMatrix :: ((Int, Int) -> (Int, Int)) -> Int -> [Int]
pascalMatrix f n = (bc . f) <$> range ((0, 0), (n - 1, n - 1))

-- Binomial coefficient
bc :: (Int, Int) -> Int
bc (n, k) = foldr (\x a -> quot (a * (n - x + 1)) x) 1 [k,k - 1 .. 1]

-- TEST ----------------------------------------------------------------------
matrixSize = 5 :: Int

main :: IO ()
main =
  mapM_
    putStrLn
    (unlines . (\(s, xs) -> s : (show <$> chunksOf matrixSize xs)) <$>
     zip
       ["Lower", "Upper", "Symmetric"]
       (pascalMatrix <$>
        [ id                    -- Lower
        , swap                  -- Upper
        , \(a, b) -> (a + b, b) -- Symmetric
        ] <*>
        [matrixSize]))
```

{{Out}}

```txt
Lower
[1,0,0,0,0]
[1,1,0,0,0]
[1,2,1,0,0]
[1,3,3,1,0]
[1,4,6,4,1]

Upper
[1,1,1,1,1]
[0,1,2,3,4]
[0,0,1,3,6]
[0,0,0,1,4]
[0,0,0,0,1]

Symmetric
[1,1,1,1,1]
[1,2,3,4,5]
[1,3,6,10,15]
[1,4,10,20,35]
[1,5,15,35,70]
```



## J



```J
   !/~ i. 5
1 1 1 1 1
0 1 2 3 4
0 0 1 3 6
0 0 0 1 4
0 0 0 0 1
   !~/~ i. 5
1 0 0 0 0
1 1 0 0 0
1 2 1 0 0
1 3 3 1 0
1 4 6 4 1
   (["0/ ! +/)~ i. 5
1 1  1  1  1
1 2  3  4  5
1 3  6 10 15
1 4 10 20 35
1 5 15 35 70
```


Explanation:

<code>x!y</code> is [http://www.jsoftware.com/jwiki/Vocabulary/bang#dyadic the number of ways of picking x balls (unordered) from a bag of y balls] and <code>x!/y</code> for list x and list y gives a table where rows correspond to the elements of x and the columns correspond to the elements of y. Meanwhile <code>!/~y</code> is equivalent to <code>y!/y</code> (and <code>i.y</code> just counts the first y non-negative integers).

Also, <code>x!~y</code> is <code>y!x</code> (and the second example otherwise follows the same pattern as the first example.

For the final example we use an unadorned <code>!</code> but prepare tables for its x and y values. Its right argument is a sum table, and its left argument is a left identity table. They look like this:


```J
   (+/)~ i. 5
0 1 2 3 4
1 2 3 4 5
2 3 4 5 6
3 4 5 6 7
4 5 6 7 8
   (["0/)~ i. 5
0 0 0 0 0
1 1 1 1 1
2 2 2 2 2
3 3 3 3 3
4 4 4 4 4
```


The parenthesis in these last two examples are redundant - they could have been omitted without changing the result, but were left in place for emphasis.


## Java

Translation of [[Pascal_matrix_generation#Python|Python]] via [[Pascal_matrix_generation#D|D]]
{{works with|Java|8}}

```java
import static java.lang.System.out;
import java.util.List;
import java.util.function.Function;
import java.util.stream.*;
import static java.util.stream.Collectors.toList;
import static java.util.stream.IntStream.range;

public class PascalMatrix {
    static int binomialCoef(int n, int k) {
        int result = 1;
        for (int i = 1; i <= k; i++)
            result = result * (n - i + 1) / i;
        return result;
    }

    static List<IntStream> pascal(int n, Function<Integer, IntStream> f) {
        return range(0, n).mapToObj(i -> f.apply(i)).collect(toList());
    }

    static List<IntStream> pascalUpp(int n) {
        return pascal(n, i -> range(0, n).map(j -> binomialCoef(j, i)));
    }

    static List<IntStream> pascalLow(int n) {
        return pascal(n, i -> range(0, n).map(j -> binomialCoef(i, j)));
    }

    static List<IntStream> pascalSym(int n) {
        return pascal(n, i -> range(0, n).map(j -> binomialCoef(i + j, i)));
    }

    static void print(String label, List<IntStream> result) {
        out.println("\n" + label);
        for (IntStream row : result) {
            row.forEach(i -> out.printf("%2d ", i));
            System.out.println();
        }
    }

    public static void main(String[] a) {
        print("Upper: ", pascalUpp(5));
        print("Lower: ", pascalLow(5));
        print("Symmetric:", pascalSym(5));
    }
}
```



```txt
Upper: 
 1  1  1  1  1 
 0  1  2  3  4 
 0  0  1  3  6 
 0  0  0  1  4 
 0  0  0  0  1 

Lower: 
 1  0  0  0  0 
 1  1  0  0  0 
 1  2  1  0  0 
 1  3  3  1  0 
 1  4  6  4  1 

Symmetric:
 1  1  1  1  1 
 1  2  3  4  5 
 1  3  6 10 15 
 1  4 10 20 35 
 1  5 15 35 70 
```



## JavaScript

In terms of a binomial coefficient, and a function on a coordinate pair.
{{Trans|Haskell}}

### ES6


```JavaScript
(() => {
    'use strict';

    // PASCAL MATRIX ---------------------------------------------------------

    // (Function on a coordinate pair) -> Matrix size -> Matrix rows
    // pascalMatrix :: ((Int, Int) -> (Int, Int)) -> Int -> [[Int]]
    const pascalMatrix = (f, n) =>
        chunksOf(n, map(compose(bc, f), range([
            [0, 0],
            [n - 1, n - 1]
        ])));

    // Binomial coefficient
    // bc :: (Int, Int) -> Int
    const bc = ([n, k]) => {
        return enumFromTo(1, k)
            .reduce((a, x) => {
                return Math.floor((a * (n - x + 1)) / x)
            }, 1)
    };

    // GENERIC FUNCTIONS -----------------------------------------------------

    // A list of functions applied to a list of arguments
    // <*> :: [(a -> b)] -> [a] -> [b]
    const ap = (fs, xs) => //
        [].concat.apply([], fs.map(f => //
            [].concat.apply([], xs.map(x => [f(x)]))));

    // chunksOf :: Int -> [a] -> [[a]]
    const chunksOf = (n, xs) =>
        xs.reduce((a, _, i, xs) =>
            i % n ? a : a.concat([xs.slice(i, i + n)]), []);

    // compose :: (b -> c) -> (a -> b) -> (a -> c)
    const compose = (f, g) => x => f(g(x));

    // concat :: [[a]] -> [a] | [String] -> String
    const concat = xs => {
        if (xs.length > 0) {
            const unit = typeof xs[0] === 'string' ? '' : [];
            return unit.concat.apply(unit, xs);
        } else return [];
    };

    // cons :: a -> [a] -> [a]
    const cons = (x, xs) => [x].concat(xs);

    // curry :: ((a, b) -> c) -> a -> b -> c
    const curry = f => a => b => f(a, b);

    // enumFromTo :: Int -> Int -> [Int]
    const enumFromTo = (m, n) =>
        Array.from({
            length: Math.floor(n - m) + 1
        }, (_, i) => m + i);

    // id :: a -> a
    const id = x => x;

    // log :: a -> IO ()
    const log = (...args) =>
        console.log(
            args
            .map(JSON.stringify)
            .join(' -> ')
        );

    // map :: (a -> b) -> [a] -> [b]
    const map = (f, xs) => xs.map(f);

    // range :: Ix a => (a, a) -> [a]
    const range = ([a, b]) => {
        const [as, bs] = a instanceof Array ? [a, b] : [
                [a],
                [b]
            ],
            an = as.length;
        return (an === bs.length) ? (
            an > 1 ? (
                sequence(as.map((_, i) => enumFromTo(as[i], bs[i])))
            ) : enumFromTo(a, b)
        ) : [];
    };

    // Evaluate left to right, and collect the results
    // sequence :: Monad m => [m a] -> m [a]
    const sequence = xs => traverse(id, xs);

    // show ::
    // (a -> String) f,  Num n =>
    // a -> maybe f -> maybe n -> String
    const show = JSON.stringify;

    // swap :: (a, b) -> (b, a)
    const swap = ([a, b]) => [b, a];

    // Map each element of a structure to an action,
    // evaluate these actions from left to right,
    // and collect the results.
    // traverse :: (a -> [b]) -> [a] -> [[b]]
    const traverse = (f, xs) => {
        const cons_f = (a, x) => ap(f(x)
            .map(curry(cons)), a);
        return xs.reduceRight(cons_f, [
            []
        ]);
    };

    // unlines :: [String] -> String
    const unlines = xs => xs.join('\n');

    // zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
    const zipWith = (f, xs, ys) => {
        const ny = ys.length;
        return (xs.length <= ny ? xs : xs.slice(0, ny))
            .map((x, i) => f(x, ys[i]));
    };

    // TEST ------------------------------------------------------------------
    const matrixSize = 5;
    return unlines(
        zipWith(
            (s, xs) => unlines(concat([
                [s], xs.map(show), ['']
            ])), ["Lower", "Upper", "Symmetric"],
            ap(
                map(curry(pascalMatrix), [
                    id,                    // Lower
                    swap,                  // Upper
                    ([a, b]) => [a + b, a] // Symmetric
                ]), [matrixSize]
            )
        )
    );
})();
```

{{Out}}

```txt
Lower
[1,0,0,0,0]
[1,1,0,0,0]
[1,2,1,0,0]
[1,3,3,1,0]
[1,4,6,4,1]

Upper
[1,1,1,1,1]
[0,1,2,3,4]
[0,0,1,3,6]
[0,0,0,1,4]
[0,0,0,0,1]

Symmetric
[1,1,1,1,1]
[1,2,3,4,5]
[1,3,6,10,15]
[1,4,10,20,35]
[1,5,15,35,70]
```



## Julia

Julia has a built-in <code>binomial</code> function to compute the binomial coefficients, and we can construct the Pascal matrices with this function using list comprehensions:

```Julia>julia
 [binomial(j,i) for i in 0:4, j in 0:4]
5×5 Array{Int64,2}:
 1  1  1  1  1
 0  1  2  3  4
 0  0  1  3  6
 0  0  0  1  4
 0  0  0  0  1

julia> [binomial(i,j) for i in 0:4, j in 0:4]
5×5 Array{Int64,2}:
 1  0  0  0  0
 1  1  0  0  0
 1  2  1  0  0
 1  3  3  1  0
 1  4  6  4  1

julia> [binomial(j+i,i) for i in 0:4, j in 0:4]
5×5 Array{Int64,2}:
 1  1   1   1   1
 1  2   3   4   5
 1  3   6  10  15
 1  4  10  20  35
 1  5  15  35  70

```



## jq

{{works with|jq|1.4}}

```jq
# Generic functions

# Note: 'transpose' is defined in recent versions of jq 
def transpose:
  if (.[0] | length) == 0 then []
  else [map(.[0])] + (map(.[1:]) | transpose)
  end ;

# Create an m x n matrix with init as the initial value
def matrix(m; n; init):
  if m == 0 then []
  elif m == 1 then [range(0;n) | init]
  elif m > 0 then
    matrix(1;n;init) as $row
    | [range(0;m) | $row ]
  else error("matrix\(m);_;_) invalid")
  end ;

# A simple pretty-printer for a 2-d matrix
def pp:
  def pad(n): tostring | (n - length) * " " + .;
  def row: reduce .[] as $x (""; . + ($x|pad(4)));
  reduce .[] as $row (""; . + "\n\($row|row)");
```


```jq
# n is input
def pascal_upper:
    . as $n
    | matrix($n; $n; 0)
    | .[0] = [range(0; $n) | 1 ] 
    | reduce range(1; $n) as $i
        (.; reduce range($i; $n) as $j
              (.; .[$i][$j] = .[$i-1][$j-1] + .[$i][$j-1]) ) ;

def pascal_lower:
  pascal_upper | transpose ;

# n is input
def pascal_symmetric:
    . as $n
    | matrix($n; $n; 1)
    | reduce range(1; $n) as $i
        (.; reduce range(1; $n) as $j
              (.; .[$i][$j] = .[$i-1][$j] + .[$i][$j-1]) ) ;
```

'''Example''':

```jq
5
| ("\nUpper:", (pascal_upper | pp),
   "\nLower:", (pascal_lower | pp),
   "\nSymmetric:", (pascal_symmetric | pp)
   )
```

{{out}}

```sh
$ jq -r -n -f Pascal_matrix_generation.jq

Upper:

   1   1   1   1   1
   0   1   2   3   4
   0   0   1   3   6
   0   0   0   1   4
   0   0   0   0   1

Lower:

   1   0   0   0   0
   1   1   0   0   0
   1   2   1   0   0
   1   3   3   1   0
   1   4   6   4   1

Symmetric:

   1   1   1   1   1
   1   2   3   4   5
   1   3   6  10  15
   1   4  10  20  35
   1   5  15  35  70
```



## Kotlin


```scala
// version 1.1.3

fun binomial(n: Int, k: Int): Int {
    if (n < k) return 0 
    if (n == 0 || k == 0) return 1
    val num = (k + 1..n).fold(1) { acc, i -> acc * i }
    val den = (2..n - k).fold(1) { acc, i -> acc * i }
    return num / den
}

fun pascalUpperTriangular(n: Int) = List(n) { i -> IntArray(n) { j -> binomial(j, i) } }

fun pascalLowerTriangular(n: Int) = List(n) { i -> IntArray(n) { j -> binomial(i, j) } }

fun pascalSymmetric(n: Int)       = List(n) { i -> IntArray(n) { j -> binomial(i + j, i) } }

fun printMatrix(title: String, m: List<IntArray>) {
    val n = m.size
    println(title)
    print("[")
    for (i in 0 until n) {
        if (i > 0) print(" ")
        print(m[i].contentToString())
        if (i < n - 1) println(",") else println("]\n")
    }
}

fun main(args: Array<String>) {
    printMatrix("Pascal upper-triangular matrix", pascalUpperTriangular(5))
    printMatrix("Pascal lower-triangular matrix", pascalLowerTriangular(5))
    printMatrix("Pascal symmetric matrix", pascalSymmetric(5))
}
```


{{out}}

```txt

Pascal upper-triangular matrix
[[1, 1, 1, 1, 1],
 [0, 1, 2, 3, 4],
 [0, 0, 1, 3, 6],
 [0, 0, 0, 1, 4],
 [0, 0, 0, 0, 1]]

Pascal lower-triangular matrix
[[1, 0, 0, 0, 0],
 [1, 1, 0, 0, 0],
 [1, 2, 1, 0, 0],
 [1, 3, 3, 1, 0],
 [1, 4, 6, 4, 1]]

Pascal symmetric matrix
[[1, 1, 1, 1, 1],
 [1, 2, 3, 4, 5],
 [1, 3, 6, 10, 15],
 [1, 4, 10, 20, 35],
 [1, 5, 15, 35, 70]]

```



## Lua


```Lua
function factorial (n)
    local f = 1
    for i = 2, n do
        f = f * i
    end
    return f
end 

function binomial (n, k)
    if k > n then return 0 end
    return factorial(n) / (factorial(k) * factorial(n - k))
end

function pascalMatrix (form, size)
    local matrix = {}
    for row = 1, size do
        matrix[row] = {}
        for col = 1, size do
            if form == "upper" then
                matrix[row][col] = binomial(col - 1, row - 1)
            end
            if form == "lower" then
                matrix[row][col] = binomial(row - 1, col - 1)
            end
            if form == "symmetric" then
                matrix[row][col] = binomial(row + col - 2, col - 1)
            end
        end
    end
    matrix.form = form:sub(1, 1):upper() .. form:sub(2, -1)
    return matrix
end

function show (mat)
    print(mat.form .. ":")
    for i = 1, #mat do
        for j = 1, #mat[i] do
            io.write(mat[i][j] .. "\t")
        end
        print()
    end
    print()
end

for _, form in pairs({"upper", "lower", "symmetric"}) do
    show(pascalMatrix(form, 5))
end
```

{{out}}

```txt
Upper:
1       1       1       1       1
0       1       2       3       4
0       0       1       3       6
0       0       0       1       4
0       0       0       0       1

Lower:
1       0       0       0       0
1       1       0       0       0
1       2       1       0       0
1       3       3       1       0
1       4       6       4       1

Symmetric:
1       1       1       1       1
1       2       3       4       5
1       3       6       10      15
1       4       10      20      35
1       5       15      35      70
```



## Mathematica

One solution is to generate a symmetric Pascal matrix then use the built in method to 
compute the upper Pascal matrix. This would be done as follows:

```Mathematica
symPascal[size_] := NestList[Accumulate, Table[1, {k, size}], size - 1]

upperPascal[size_] := CholeskyDecomposition[symPascal@size]

lowerPascal[size_] := Transpose@CholeskyDecomposition[symPascal@size]

Column[MapThread[
  Labeled[Grid[#1@5], #2, Top] &, {{upperPascal, lowerPascal, 
    symPascal}, {"Upper", "Lower", "Symmetric"}}]]
```

{{out}}

```txt

Upper
1	1	1	1	1
0	1	2	3	4
0	0	1	3	6
0	0	0	1	4
0	0	0	0	1

Lower
1	0	0	0	0
1	1	0	0	0
1	2	1	0	0
1	3	3	1	0
1	4	6	4	1

Symmetric
1	1	1	1	1
1	2	3	4	5
1	3	6	10	15
1	4	10	20	35
1	5	15	35	70

```


It is also possible to directly compute a lower Pascal matrix as follows:

```Mathematica
lowerPascal[size_] := 
 MatrixExp[
  SparseArray[{Band[{2, 1}] -> Range[size - 1]}, {size, size}]]]
```

But since the builtin function MatrixExp works by first computing eigenvalues this is
likely to be slower for large Pascal matrices


## PARI/GP


```parigp

Pl(n)={matpascal(n-1)}
printf("%d",Pl(5))

```

{{out}}

```txt

[1 0 0 0 0]

[1 1 0 0 0]

[1 2 1 0 0]

[1 3 3 1 0]

[1 4 6 4 1]

```


```parigp

Pu(n)={Pl(n)~}
printf("%d",Pu(5))

```

{{out}}

```txt

[1 1 1 1 1]

[0 1 2 3 4]

[0 0 1 3 6]

[0 0 0 1 4]

[0 0 0 0 1]

```


```parigp

Ps(n)={matrix(n,n,n,g,binomial(n+g-2,n-1))}
printf("%d",Ps(5))

```

{{out}}

```txt

[1 1  1  1  1]

[1 2  3  4  5]

[1 3  6 10 15]

[1 4 10 20 35]

[1 5 15 35 70]

```



## Pascal


```pascal
program Pascal_matrix(Output);

const N = 5;

type NxN_Matrix = array[0..N,0..N] of integer;

var PM,PX : NxN_Matrix;

function Pascal_sym(x : integer; p : NxN_Matrix) : NxN_Matrix;
var I,J : integer;
  begin
    for I := 1 to x do
    begin
      for J := 1 to x do p[I,J] := p[I-1,J]+p[I,J-1]
    end;
    Pascal_sym := p;
  end;

function Pascal_upp(x : integer; p : NxN_Matrix) : NxN_Matrix;
var I,J : integer;
  begin
    for I := 1 to x do
    begin
      for J := 1 to x do p[I,J] := p[I-1,J-1]+p[I,J-1]
    end;
    Pascal_upp := p
  end;

function Pascal_low(x : integer; p : NxN_Matrix) : NxN_Matrix;
var p1,p2 : NxN_Matrix;
  I,J : integer;
  begin
    p1 := Pascal_upp(x,p);
    p2 := p1;
    for I := 1 to x do
    begin
      for J := 1 to x do p1[J,I] := p2[I,J]
    end;
    Pascal_low := p1
  end;

procedure PrintMatrix(titel : ansistring; x : integer; p : NxN_Matrix);
var I,J : integer;
  begin
    writeln(titel);
    for I := 1 to x do
    begin
      for J := 1 to x do write(p[I,J]:5);
      writeln('');
    end;
  end;

begin
  PX[0,0] := 0;
  PM[0,0] := 1;
  PM := Pascal_upp(N, PM);
  PrintMatrix('Upper:', N, PM);
  writeln('');
  PM := PX;
  PM[0,0] := 1;
  PM := Pascal_low(N, PM);
  PrintMatrix('Lower:', N, PM);
  writeln('');
  PM := PX;
  PM[1,0] := 1;
  PM := Pascal_sym(N, PM);
  PrintMatrix('Symmetric', N, PM);
  writeln('');
  readln;
end.
```

{{out}}

```txt
Upper:
    1    1    1    1    1
    0    1    2    3    4
    0    0    1    3    6
    0    0    0    1    4
    0    0    0    0    1

Lower:
    1    0    0    0    0
    1    1    0    0    0
    1    2    1    0    0
    1    3    3    1    0
    1    4    6    4    1

Symmetric
    1    1    1    1    1
    1    2    3    4    5
    1    3    6   10   15
    1    4   10   20   35
    1    5   15   35   70
```



## Perl


```Perl
#!/usr/bin/perl
use warnings;
use strict;
use feature qw{ say };


sub upper {
    my ($i, $j) = @_;
    my @m;
    for my $x (0 .. $i - 1) {
        for my $y (0 .. $j - 1) {
            $m[$x][$y] = $x > $y          ? 0
                       : ! $x || $x == $y ? 1
                                          : $m[$x-1][$y-1] + $m[$x][$y-1];
        }
    }
    return \@m
}


sub lower {
    my ($i, $j) = @_;
    my @m;
    for my $x (0 .. $i - 1) {
        for my $y (0 .. $j - 1) {
            $m[$x][$y] = $x < $y          ? 0
                       : ! $x || $x == $y ? 1
                                          : $m[$x-1][$y-1] + $m[$x-1][$y];
        }
    }
    return \@m
}


sub symmetric {
    my ($i, $j) = @_;
    my @m;
    for my $x (0 .. $i - 1) {
        for my $y (0 .. $j - 1) {
            $m[$x][$y] = ! $x || ! $y ? 1
                                      : $m[$x-1][$y] + $m[$x][$y-1];
        }
    }
    return \@m
}


sub pretty {
    my $m = shift;
    for my $row (@$m) {
        say join ', ', @$row;
    }
}


pretty(upper(5, 5));
say '-' x 14;
pretty(lower(5, 5));
say '-' x 14;
pretty(symmetric(5, 5));
```

{{out}}

```txt
1, 1, 1, 1, 1
0, 1, 2, 3, 4
0, 0, 1, 3, 6
0, 0, 0, 1, 4
0, 0, 0, 0, 1
--------------
1, 0, 0, 0, 0
1, 1, 0, 0, 0
1, 2, 1, 0, 0
1, 3, 3, 1, 0
1, 4, 6, 4, 1
--------------
1, 1, 1, 1, 1
1, 2, 3, 4, 5
1, 3, 6, 10, 15
1, 4, 10, 20, 35
1, 5, 15, 35, 70
```


## Perl 6

{{Works with|rakudo|2016-12}}
Here is a rather more general solution than required.  The <tt>grow-matrix</tt> function will grow any N by N matrix into an N+1 x N+1 matrix, using any function of the three leftward/upward neighbors, here labelled "West", "North", and "Northwest".  We then define three iterator functions that can grow Pascal matrices, and use those iterators to define three constants, each of which is an infinite sequence of ever-larger Pascal matrices.  Normal subscripting then pulls out the ones of the specified size.

```perl6
# Extend a matrix in 2 dimensions based on 3 neighbors.
sub grow-matrix(@matrix, &func) {
    my $n = @matrix.shape eq '*' ?? 1 !! @matrix.shape[0];
    my @m[$n+1;$n+1];
    for ^$n X ^$n -> ($i, $j) {
       @m[$i;$j] = @matrix[$i;$j];
    }
#                     West         North        NorthWest
    @m[$n; 0] = func( 0,           @m[$n-1;0],  0            );
    @m[ 0;$n] = func( @m[0;$n-1],  0,           0            );
    @m[$_;$n] = func( @m[$_;$n-1], @m[$_-1;$n], @m[$_-1;$n-1]) for 1 ..^ $n;
    @m[$n;$_] = func( @m[$n;$_-1], @m[$n-1;$_], @m[$n-1;$_-1]) for 1 ..  $n;
    @m;
}

# I am but mad north-northwest...
sub madd-n-nw(@m) { grow-matrix @m, -> $w, $n, $nw {  $n + $nw } }
sub madd-w-nw(@m) { grow-matrix @m, -> $w, $n, $nw {  $w + $nw } }
sub madd-w-n (@m) { grow-matrix @m, -> $w, $n, $nw {  $w + $n  } }

# Define 3 infinite sequences of Pascal matrices.
constant upper-tri = [1], &madd-w-nw ... *;
constant lower-tri = [1], &madd-n-nw ... *;
constant symmetric = [1], &madd-w-n  ... *;

show_m upper-tri[4];
show_m lower-tri[4];
show_m symmetric[4];

sub show_m (@m) {
my \n = @m.shape[0];
for ^n X ^n -> (\i, \j) {
    print @m[i;j].fmt("%{1+max(@m).chars}d"); 
    print "\n" if j+1 eq n;
}
say '';
}
```

{{out}}

```txt
 1 1 1 1 1
 0 1 2 3 4
 0 0 1 3 6
 0 0 0 1 4
 0 0 0 0 1

 1 0 0 0 0
 1 1 0 0 0
 1 2 1 0 0
 1 3 3 1 0
 1 4 6 4 1

  1  1  1  1  1
  1  2  3  4  5
  1  3  6 10 15
  1  4 10 20 35
  1  5 15 35 70
```



## Phix

{{trans|Fortran}}

```Phix
function pascal_upper(integer n)
    sequence res = repeat(repeat(0,n),n)
    res[1] = repeat(1,n)
    for i=2 to n do
        for j=2 to i do
            res[j,i] = res[j,i-1]+res[j-1,i-1]
        end for
    end for
    return res
end function

function pascal_lower(integer n)
    sequence res = repeat(repeat(0,n),n)
    for i=1 to n do
        res[i,1] = 1
    end for
    for i=2 to n do
        for j=2 to i do
            res[i,j] = res[i-1,j]+res[i-1,j-1]
        end for
    end for
    return res
end function

function pascal_symmetric(integer n)
    sequence res = repeat(repeat(0,n),n)
    for i=1 to n do
        res[i,1] = 1
        res[1,i] = 1
    end for
    for i=2 to n do
        for j = 2 to n do
            res[i,j] = res[i-1,j]+res[i,j-1]
        end for
    end for
    return res
end function

ppOpt({pp_Nest,1,pp_StrFmt,-2,pp_IntFmt,"%2d"})
puts(1,"
###  Pascal upper matrix 
\n")
pp(pascal_upper(5))
puts(1,"
###  Pascal lower matrix 
\n")
pp(pascal_lower(5))
puts(1,"
###  Pascal symmetrical matrix 
\n")
pp(pascal_symmetric(5))
```

{{out}}

```txt


###  Pascal upper matrix 

{{ 1, 1, 1, 1, 1},
 { 0, 1, 2, 3, 4},
 { 0, 0, 1, 3, 6},
 { 0, 0, 0, 1, 4},
 { 0, 0, 0, 0, 1}}

###  Pascal lower matrix 

{{ 1, 0, 0, 0, 0},
 { 1, 1, 0, 0, 0},
 { 1, 2, 1, 0, 0},
 { 1, 3, 3, 1, 0},
 { 1, 4, 6, 4, 1}}

###  Pascal symmetrical matrix 

{{ 1, 1, 1, 1, 1},
 { 1, 2, 3, 4, 5},
 { 1, 3, 6,10,15},
 { 1, 4,10,20,35},
 { 1, 5,15,35,70}}

```



## PicoLisp


```PicoLisp
(setq
   Low '(A B)
   Upp '(B A)
   Sym '((+ A B) A) )

(de binomial (N K)
   (let f
      '((N)
         (if (=0 N) 1 (apply * (range 1 N))) )
      (if (> K N)
         0
         (/
            (f N)
            (* (f (- N K)) (f K)) ) ) ) )
(de pascal (N Z)
   (for Lst
      (mapcar
         '((A)
            (mapcar
               '((B) (apply binomial (mapcar eval Z)))
               (range 0 N) ) )
         (range 0 N) )
      (for L Lst
         (prin (align 2 L) " ") )
      (prinl) )
   (prinl) )

(pascal 4 Low)
(pascal 4 Upp)
(pascal 4 Sym)
```

{{out}}

```txt
 1  0  0  0  0
 1  1  0  0  0
 1  2  1  0  0
 1  3  3  1  0
 1  4  6  4  1

 1  1  1  1  1
 0  1  2  3  4
 0  0  1  3  6
 0  0  0  1  4
 0  0  0  0  1

 1  1  1  1  1
 1  2  3  4  5
 1  3  6 10 15
 1  4 10 20 35
 1  5 15 35 70
```



## PL/I

{{trans|Rexx}}

```pli
*process source attributes xref or(!);
 pat: Proc Options(main);
 Dcl (HBOUND,MAX,RIGHT) Builtin;
 Dcl SYSPRINT Print;
 Dcl N Bin Fixed(31) Init(5);
 Dcl pd Char(500) Var;
 Dcl fact(0:10) Bin Fixed(31);
 Dcl pt(0:500) Bin Fixed(31);
 Call mk_fact(fact);

 Call Pascal(n,'U',pt); Call show('Pascal upper triangular matrix');
 Call Pascal(n,'L',pt); Call show('Pascal lower triangular matrix');
 Call Pascal(n,'S',pt); Call show('Pascal symmetric matrix'       );

 Pascal: proc(n,which,dd);
 Dcl n Bin Fixed(31);
 Dcl which Char(1);
 Dcl (i,j,k) Bin Fixed(31);
 Dcl dd(0:500) Bin Fixed(31);
 k=0;
 dd(0)=0;
 do i=0 To n-1;
   Do j=0 To n-1;
     k+=1;
     Select(which);
       When('U') dd(k)=comb((j),  (i));
       When('L') dd(k)=comb((i),  (j));
       When('S') dd(k)=comb((i+j),(i));
       Otherwise;
       End;
     dd(0)=max(dd(0),dd(k));
     End;
   End;
 End;

 mk_fact: Proc(f);
 Dcl f(0:*) Bin Fixed(31);
 Dcl i Bin Fixed(31);
 f(0)=1;
 Do i=1 To hbound(f);
  f(i)=f(i-1)*i;
  End;
 End;

 comb: proc(x,y) Returns(pic'z9');
 Dcl (x,y) Bin Fixed(31);
 Dcl (j,z) Bin Fixed(31);
 Dcl res Pic'Z9';
 Select;
   When(x=y) res=1;
   When(y>x) res=0;
   Otherwise Do;
     If x-y<y then
       y=x-y;
     z=1;
     do j=x-y+1 to x;
       z=z*j;
       End;
     res=z/fact(y);
     End;
   End;
 Return(res);
 End;

 show: Proc(head);
 Dcl head Char(*);
 Dcl (n,r,c,pl) Bin Fixed(31) Init(0);
 Dcl row Char(50) Var;
 Dcl p Pic'z9';
 If pt(0)<10 Then pl=1;
             Else pl=2;
 Dcl sep(5) Char(1) Init((4)(1)',',']');
 Put Edit(' ',head)(Skip,a);
 do r=1 To 5;
   if r=1 then row='[[';
          else row=' [';
   do c=1 To 5;
     n+=1;
     p=pt(n);
     row=row!!right(p,pl)!!sep(c);
     End;
   Put Edit(row)(Skip,a);
   End;
 Put Edit(']')(A);
 End;

 End;
```

{{out}}

```txt
Pascal upper triangular matrix
[[1,1,1,1,1]
 [0,1,2,3,4]
 [0,0,1,3,6]
 [0,0,0,1,4]
 [0,0,0,0,1]]

Pascal lower triangular matrix
[[1,0,0,0,0]
 [1,1,0,0,0]
 [1,2,1,0,0]
 [1,3,3,1,0]
 [1,4,6,4,1]]

Pascal symmetric matrix
[[ 1, 1, 1, 1, 1]
 [ 1, 2, 3, 4, 5]
 [ 1, 3, 6,10,15]
 [ 1, 4,10,20,35]
 [ 1, 5,15,35,70]]
```



## PureBasic


```PureBasic
EnableExplicit
Define.i x=5, I, J

Macro Print_Pascal_matrix(typ)
  PrintN(typ)
  For I=1 To x
    For J=1 To x : Print(RSet(Str(p(I,J)),3," ")+Space(3)) : Next  
    PrintN("")
  Next
  Print(~"\n\n")  
EndMacro

Procedure Pascal_sym(n.i,Array p.i(2))  
  Define.i I,J  
  p(1,0)=1
  For I=1 To n
    For J=1 To n : p(I,J)=p(I-1,J)+p(I,J-1) : Next
  Next
EndProcedure

Procedure Pascal_upp(n.i,Array p.i(2))  
  Define.i I,J  
  p(0,0)=1
  For I=1 To n
    For J=1 To n : p(I,J)=p(I-1,J-1)+p(I,J-1) : Next
  Next  
EndProcedure

Procedure Pascal_low(n.i,Array p.i(2))
  Define.i I,J
  Pascal_upp(n,p())
  Dim p2.i(n,n)
  CopyArray(p(),p2())  
  For I=1 To n
    For J=1 To n : Swap p(J,I),p2(I,J) : Next
  Next  
EndProcedure

OpenConsole()

Dim p.i(x,x)
Pascal_upp(x,p())
Print_Pascal_matrix("Upper:")

Dim p.i(x,x)
Pascal_low(x,p())
Print_Pascal_matrix("Lower:")

Dim p.i(x,x)
Pascal_sym(x,p())
Print_Pascal_matrix("Symmetric:")

Input()
End
```

{{out}}

```txt
Upper:
  1     1     1     1     1
  0     1     2     3     4
  0     0     1     3     6
  0     0     0     1     4
  0     0     0     0     1


Lower:
  1     0     0     0     0
  1     1     0     0     0
  1     2     1     0     0
  1     3     3     1     0
  1     4     6     4     1


Symmetric:
  1     1     1     1     1
  1     2     3     4     5
  1     3     6    10    15
  1     4    10    20    35
  1     5    15    35    70
```



## Python


### Python: Summing adjacent values


```python
from pprint import pprint as pp

def pascal_upp(n):
    s = [[0] * n for _ in range(n)]
    s[0] = [1] * n
    for i in range(1, n):
        for j in range(i, n):
            s[i][j] = s[i-1][j-1] + s[i][j-1]
    return s

def pascal_low(n):
    # transpose of pascal_upp(n)
    return [list(x) for x in zip(*pascal_upp(n))]

def pascal_sym(n):
    s = [[1] * n for _ in range(n)]
    for i in range(1, n):
        for j in range(1, n):
            s[i][j] = s[i-1][j] + s[i][j-1]
    return s
    

if __name__ == "__main__":
    n = 5
    print("\nUpper:")
    pp(pascal_upp(n))
    print("\nLower:")
    pp(pascal_low(n))
    print("\nSymmetric:")
    pp(pascal_sym(n))
```


{{out}}

```txt
Upper:
[[1, 1, 1, 1, 1],
 [0, 1, 2, 3, 4],
 [0, 0, 1, 3, 6],
 [0, 0, 0, 1, 4],
 [0, 0, 0, 0, 1]]

Lower:
[[1, 0, 0, 0, 0],
 [1, 1, 0, 0, 0],
 [1, 2, 1, 0, 0],
 [1, 3, 3, 1, 0],
 [1, 4, 6, 4, 1]]

Symmetric:
[[1, 1, 1, 1, 1],
 [1, 2, 3, 4, 5],
 [1, 3, 6, 10, 15],
 [1, 4, 10, 20, 35],
 [1, 5, 15, 35, 70]]
```



### Python: Using a binomial coefficient generator function


```python
def binomialCoeff(n, k):
    result = 1
    for i in range(1, k+1):
        result = result * (n-i+1) // i
    return result

def pascal_upp(n):
    return [[binomialCoeff(j, i) for j in range(n)] for i in range(n)]

def pascal_low(n):
    return [[binomialCoeff(i, j) for j in range(n)] for i in range(n)]

def pascal_sym(n):
    return [[binomialCoeff(i+j, i) for j in range(n)] for i in range(n)]
```


{{out}}
(As above)


## R


```r
lower.pascal <- function(n) {
  a <- matrix(0, n, n)
  a[, 1] <- 1
  if (n > 1) {
    for (i in 2:n) {
      j <- 2:i
      a[i, j] <- a[i - 1, j - 1] + a[i - 1, j]
    }
  }
  a
}

# Alternate version
lower.pascal.alt <- function(n) {
  a <- matrix(0, n, n)
  a[, 1] <- 1
  if (n > 1) {
    for (j in 2:n) {
      i <- j:n
      a[i, j] <- cumsum(a[i - 1, j - 1])
    }
  }
  a
}

# While it's possible to modify lower.pascal to get the upper matrix,
# here we simply transpose the lower one.
upper.pascal <- function(n) t(lower.pascal(n))

symm.pascal <- function(n) {
  a <- matrix(0, n, n)
  a[, 1] <- 1
  for (i in 2:n) {
    a[, i] <- cumsum(a[, i - 1])
  }
  a
}
```


The results follow


```r>
 lower.pascal(5)
     [,1] [,2] [,3] [,4] [,5]
[1,]    1    0    0    0    0
[2,]    1    1    0    0    0
[3,]    1    2    1    0    0
[4,]    1    3    3    1    0
[5,]    1    4    6    4    1
> lower.pascal.alt(5)
     [,1] [,2] [,3] [,4] [,5]
[1,]    1    0    0    0    0
[2,]    1    1    0    0    0
[3,]    1    2    1    0    0
[4,]    1    3    3    1    0
[5,]    1    4    6    4    1
> upper.pascal(5)
     [,1] [,2] [,3] [,4] [,5]
[1,]    1    1    1    1    1
[2,]    0    1    2    3    4
[3,]    0    0    1    3    6
[4,]    0    0    0    1    4
[5,]    0    0    0    0    1
> symm.pascal(5)
     [,1] [,2] [,3] [,4] [,5]
[1,]    1    1    1    1    1
[2,]    1    2    3    4    5
[3,]    1    3    6   10   15
[4,]    1    4   10   20   35
[5,]    1    5   15   35   70
```


## Racket



```racket
#lang racket
(require math/number-theory)

(define (pascal-upper-matrix n)
  (for/list ((i n)) (for/list ((j n)) (j . binomial . i))))

(define (pascal-lower-matrix n)
  (for/list ((i n)) (for/list ((j n)) (i . binomial . j))))

(define (pascal-symmetric-matrix n)
  (for/list ((i n)) (for/list ((j n)) ((+ i j) . binomial . j))))

(define (matrix->string m)
  (define col-width
    (for*/fold ((rv 1)) ((r m) (c r))
      (if (zero? c) rv (max rv (+ 1 (order-of-magnitude c))))))
  (string-append
   (string-join
   (for/list ((r m))
     (string-join (map (λ (c) (~a #:width col-width #:align 'right c)) r) " ")) "\n")
   "\n"))

(printf "Upper:~%~a~%" (matrix->string (pascal-upper-matrix 5)))
(printf "Lower:~%~a~%" (matrix->string (pascal-lower-matrix 5)))
(printf "Symmetric:~%~a~%" (matrix->string (pascal-symmetric-matrix 5)))
```

{{out}}

```txt
Upper:
1 1 1 1 1
0 1 2 3 4
0 0 1 3 6
0 0 0 1 4
0 0 0 0 1

Lower:
1 0 0 0 0
1 1 0 0 0
1 2 1 0 0
1 3 3 1 0
1 4 6 4 1

Symmetric:
 1  1  1  1  1
 1  2  3  4  5
 1  3  6 10 15
 1  4 10 20 35
 1  5 15 35 70


```



## REXX


### separate generation

Commentary:   <sup>1</sup>/<sub>3</sub>   of the REXX program deals with the displaying of the matrix.

```rexx
/*REXX program  generates and displays  three forms of an   NxN   Pascal matrix.        */
numeric digits 50                                /*be able to calculate huge factorials.*/
parse arg N .                                    /*obtain the optional matrix size  (N).*/
if N==''  then N=5                               /*Not specified?  Then use the default.*/
                        call show  N,  upp(N),  'Pascal upper triangular matrix'
                        call show  N,  low(N),  'Pascal lower triangular matrix'
                        call show  N,  sym(N),  'Pascal symmetric matrix'
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
upp:  procedure; parse arg N;  $=                /*gen Pascal upper triangular matrix.  */
            do i=0  for N;  do j=0  for N; $=$ comb(j,   i);   end; end;   return $
/*──────────────────────────────────────────────────────────────────────────────────────*/
low:  procedure; parse arg N;  $=                /*gen Pascal lower triangular matrix.  */
            do i=0  for N;  do j=0  for N; $=$ comb(i,   j);   end; end;   return $
/*──────────────────────────────────────────────────────────────────────────────────────*/
sym:  procedure; parse arg N;  $=                /*generate  Pascal symmetric  matrix.  */
            do i=0  for N;  do j=0  for N; $=$ comb(i+j, i);   end; end;   return $
/*──────────────────────────────────────────────────────────────────────────────────────*/
!:    procedure; parse arg x;  !=1;    do j=2  to x;  !=!*j;  end;         return !
/*──────────────────────────────────────────────────────────────────────────────────────*/
comb: procedure; parse arg x,y;        if x=y  then return 1                /* {=} case.*/
                                       if y>x  then return 0                /* {>} case.*/
      if x-y<y  then y=x-y; _=1;   do j=x-y+1  to x; _=_*j; end;           return _ / !(y)
/*──────────────────────────────────────────────────────────────────────────────────────*/
show: procedure; parse arg s,@;   w=0;    #=0                               /*get args. */
                           do x=1  for s**2;  w=max(w,1+length(word(@,x)));  end
                 say;   say center(arg(3), 50, '─')                         /*show title*/
                           do    r=1  for s;  if r==1  then $='[['          /*row  1    */
                                                       else $=' ['          /*rows 2   N*/
                              do c=1  for s;  #=#+1;   e= (c==s)            /*e ≡ "end".*/
                              $=$ || right(word(@, #), w) || left(',', \e) || left("]", e)
                              end   /*c*/                                   /* [↑]  row.*/
                           say $ || left(',', r\==s)left("]", r==s)         /*show row. */
                           end     /*r*/
                 return
```

'''output'''   using the default input:

```txt

──────────Pascal upper triangular matrix──────────
[[ 1, 1, 1, 1, 1],
 [ 0, 1, 2, 3, 4],
 [ 0, 0, 1, 3, 6],
 [ 0, 0, 0, 1, 4],
 [ 0, 0, 0, 0, 1]]

──────────Pascal lower triangular matrix──────────
[[ 1, 0, 0, 0, 0],
 [ 1, 1, 0, 0, 0],
 [ 1, 2, 1, 0, 0],
 [ 1, 3, 3, 1, 0],
 [ 1, 4, 6, 4, 1]]

─────────────Pascal symmetric matrix──────────────
[[  1,  1,  1,  1,  1],
 [  1,  2,  3,  4,  5],
 [  1,  3,  6, 10, 15],
 [  1,  4, 10, 20, 35],
 [  1,  5, 15, 35, 70]]

```



### consolidated generation

This REXX version uses a consolidated generation subroutine, even though this Rosetta Code implies to use   '''functions'''   (instead of a single function).

```rexx
/*REXX program  generates and displays  three forms  of an   NxN   Pascal matrix.       */
numeric digits 50                                /*be able to calculate huge factorials.*/
parse arg N .                                    /*obtain the optional matrix  size (N).*/
if N==''  then N=5                               /*Not specified?  Then use the default.*/
                  call show N, Pmat(N, 'upper'), 'Pascal upper triangular matrix'
                  call show N, Pmat(N, 'lower'), 'Pascal lower triangular matrix'
                  call show N, Pmat(N, 'sym')  , 'Pascal symmetric matrix'
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
Pmat: procedure; parse arg N;    $=              /*generate a format of a Pascal matrix.*/
      arg , ?                                    /*get uppercase version of the 2nd arg.*/
               do i=0  for N; do j=0  for N      /*          [↓]  pick a format to use. */
                              if abbrev('UPPER'      ,?,1)  then $=$ comb(j  , i)
                              if abbrev('LOWER'      ,?,1)  then $=$ comb(i  , j)
                              if abbrev('SYMMETRICAL',?,1)  then $=$ comb(i+j, j)
                              end  /*j*/         /*     ↑                               */
               end   /*i*/                       /*     │                               */
      return $                                   /*     └───min. abbreviation is 1 char.*/
/*──────────────────────────────────────────────────────────────────────────────────────*/
!:    procedure; parse arg x;  !=1;    do j=2  to x;  !=!*j;  end;      return !
/*──────────────────────────────────────────────────────────────────────────────────────*/
comb: procedure; parse arg x,y;        if x=y  then return 1                /* {=} case.*/
                                       if y>x  then return 0                /* {>} case.*/
      if x-y<y  then y=x-y; _=1;   do j=x-y+1  to x; _=_*j; end;        return _ / !(y)
/*──────────────────────────────────────────────────────────────────────────────────────*/
show: procedure; parse arg s,@;   w=0;    #=0                               /*get args. */
                           do x=1  for s**2;  w=max(w,1+length(word(@,x)));  end
                 say;   say center(arg(3), 50, '─')                         /*show title*/
                           do   r=1  for s;  if r==1  then $='[['           /*row  1    */
                                                      else $=' ['           /*rows 2   N*/
                              do c=1  for s;  #=#+1;   e= (c==s)            /*e ≡ "end".*/
                              $=$ || right(word(@, #), w) || left(', ',\e) || left("]", e)
                              end   /*c*/                                   /* [↑]  row.*/
                           say $ || left(',', r\==s)left(']', r==s)         /*show row. */
                           end     /*r*/
                 return
```

'''output'''   is identical to the 1<sup>st</sup> REXX version. 




## Ring


```ring

# Project : Pascal matrix generation

load "stdlib.ring"
res = newlist(5,5)

see "
###  Pascal upper matrix 
" + nl
result = pascalupper(5)
showarray(result)

see nl + "
###  Pascal lower matrix 
" + nl
result = pascallower(5)
showarray(result)

see nl + "
###  Pascal symmetrical matrix 
" + nl
result = pascalsymmetric(5)
showarray(result)

func pascalupper(n)
    for m=1 to n
          for p=1 to n
               res[m][p] = 0
          next
    next 
    for p=1 to n
         res[1][p] = 1
    next    
    for i=2 to n 
        for j=2 to i 
            res[j][i] = res[j][i-1]+res[j-1][i-1]
        end 
    end 
    return res
 
func pascallower(n)
        for m=1 to n
              for p=1 to n
                   res[m][p] = 0
              next
        next
       for p=1 to n  
             res[p][1] = 1
       next
       for i=2 to n 
            for j=2 to i 
                 res[i][j] = res[i-1][j]+res[i-1][j-1]
            next
        next
        return res
 
func pascalsymmetric(n)
        for m=1 to n
              for p=1 to n
                   res[m][p] = 0
              next
        next
        for p=1 to n 
              res[p][1] = 1
              res[1][p] = 1
        next
        for i=2 to n 
             for j = 2 to n 
                  res[i][j] = res[i-1][j]+res[i][j-1]
             next
        next
        return res

func showarray(result)
        for n=1 to 5
              for m=1 to 5
                   see "" + result[n][m] + " "
              next
             see nl
        next

```

Output:

```txt


###  Pascal upper matrix 

1 1 1 1 1 
0 1 2 3 4 
0 0 1 3 6 
0 0 0 1 4 
0 0 0 0 1 


###  Pascal lower matrix 

1 0 0 0 0 
1 1 0 0 0 
1 2 1 0 0 
1 3 3 1 0 
1 4 6 4 1 


###  Pascal symmetrical matrix 

1 1 1 1 1 
1 2 3 4 5 
1 3 6 10 15 
1 4 10 20 35 
1 5 15 35 70 

```



## Ruby

'''Summing adjacent values:'''

```ruby
#Upper, lower, and symetric Pascal Matrix - Nigel Galloway: May 3rd., 21015
require 'pp'

ng = (g = 0..4).collect{[]}
g.each{|i| g.each{|j| ng[i][j] = i==0 ? 1 : j<i ? 0 : ng[i-1][j-1]+ng[i][j-1]}}
pp ng; puts
g.each{|i| g.each{|j| ng[i][j] = j==0 ? 1 : i<j ? 0 : ng[i-1][j-1]+ng[i-1][j]}}
pp ng; puts
g.each{|i| g.each{|j| ng[i][j] = (i==0 or j==0) ? 1 : ng[i-1][j  ]+ng[i][j-1]}}
pp ng
```

{{out}}

```txt

[[1, 1, 1, 1, 1],
 [0, 1, 2, 3, 4],
 [0, 0, 1, 3, 6],
 [0, 0, 0, 1, 4],
 [0, 0, 0, 0, 1]]

[[1, 0, 0, 0, 0],
 [1, 1, 0, 0, 0],
 [1, 2, 1, 0, 0],
 [1, 3, 3, 1, 0],
 [1, 4, 6, 4, 1]]

[[1, 1, 1, 1, 1],
 [1, 2, 3, 4, 5],
 [1, 3, 6, 10, 15],
 [1, 4, 10, 20, 35],
 [1, 5, 15, 35, 70]]

```



### Binomial coefficient:


```ruby
require 'pp'

def binomial_coeff(n,k) (1..k).inject(1){|res,i| res * (n-i+1) / i}             end

def pascal_upper(n)     (0...n).map{|i| (0...n).map{|j| binomial_coeff(j,i)}}   end
def pascal_lower(n)     (0...n).map{|i| (0...n).map{|j| binomial_coeff(i,j)}}   end
def pascal_symmetric(n) (0...n).map{|i| (0...n).map{|j| binomial_coeff(i+j,j)}} end

puts "Pascal upper-triangular matrix:"
pp pascal_upper(5)

puts "\nPascal lower-triangular matrix:"
pp pascal_lower(5)

puts "\nPascal symmetric matrix:"
pp pascal_symmetric(5)
```


{{out}}

```txt

Pascal upper-triangular matrix:
[[1, 1, 1, 1, 1],
 [0, 1, 2, 3, 4],
 [0, 0, 1, 3, 6],
 [0, 0, 0, 1, 4],
 [0, 0, 0, 0, 1]]

Pascal lower-triangular matrix:
[[1, 0, 0, 0, 0],
 [1, 1, 0, 0, 0],
 [1, 2, 1, 0, 0],
 [1, 3, 3, 1, 0],
 [1, 4, 6, 4, 1]]

Pascal symmetric matrix:
[[1, 1, 1, 1, 1],
 [1, 2, 3, 4, 5],
 [1, 3, 6, 10, 15],
 [1, 4, 10, 20, 35],
 [1, 5, 15, 35, 70]]

```



## Scala


```scala
//Pascal Matrix Generator

object pascal{
	def main( args:Array[String] ){
		
		println("Enter the order of matrix")
		val n = scala.io.StdIn.readInt()
		
		var F = new Factorial()
		
		var mx = Array.ofDim[Int](n,n)
		
		for( i <- 0 to (n-1); j <- 0 to (n-1) ){
			
			if( i>=j ){			//iCj
				mx(i)(j) = F.fact(i) / ( ( F.fact(j) )*( F.fact(i-j) ) )
			}
		}
		
		println("iCj:")
		for( i <- 0 to (n-1) ){		//iCj print
			for( j <- 0 to (n-1) ){
				print( mx(i)(j)+" " )
			}
			println("")
		}
		
		println("jCi:")
		for( i <- 0 to (n-1) ){		//jCi print
			for( j <- 0 to (n-1) ){
				print( mx(j)(i)+" " )
			}
			println("")
		}
		
		//(i+j)C j
		for( i <- 0 to (n-1); j <- 0 to (n-1) ){
			
			mx(i)(j) = F.fact(i+j) / ( ( F.fact(j) )*( F.fact(i) ) )
		}
		//print (i+j)Cj
		println("(i+j)Cj:")
		for( i <- 0 to (n-1) ){
			for( j <- 0 to (n-1) ){
				print( mx(i)(j)+" " )
			}
			println("")
		}
		
	}
}

class Factorial(){
	
	def fact( a:Int ): Int = {
		
		var b:Int = 1
		
		for( i <- 2 to a ){
			b = b*i
		}
		return b
	}
}

```



## Sidef

{{trans|Perl 6}}

```ruby
func grow_matrix(matrix, callback) {
    var m = matrix
    var s = m.len
    m[s][0] = callback(0, m[s-1][0], 0)
    m[0][s] = callback(m[0][s-1], 0, 0)
    {|i| m[i+1][s] = callback(m[i+1][s-1], m[i][s], m[i][s-1])} * (s-1)
    {|i| m[s][i+1] = callback(m[s][i], m[s-1][i+1], m[s-1][i])} * (s)
    return m
}

func transpose(matrix) {
    matrix[0].range.map{|i| matrix.map{_[i]} }
}

func madd_n_nw(m) { grow_matrix(m, ->(_, n, nw) { n + nw }) }
func madd_w_nw(m) { grow_matrix(m, ->(w, _, nw) { w + nw }) }
func madd_w_n(m)  { grow_matrix(m, ->(w, n, _)  { w + n  }) }

var functions = [madd_n_nw, madd_w_nw, madd_w_n].map { |f|
    func(n) {
        var r = [[1]]
        { f(r) } * n
        transpose(r)
    }
}

functions.map { |f|
    f(4).map { .map{ '%2s' % _ }.join(' ') }.join("\n")
}.join("\n\n").say
```

{{out}}

```txt

 1  1  1  1  1
 0  1  2  3  4
 0  0  1  3  6
 0  0  0  1  4
 0  0  0  0  1

 1  0  0  0  0
 1  1  0  0  0
 1  2  1  0  0
 1  3  3  1  0
 1  4  6  4  1

 1  1  1  1  1
 1  2  3  4  5
 1  3  6 10 15
 1  4 10 20 35
 1  5 15 35 70

```



## Stata


Here are variants for the lower matrix.


```stata
mata
function pascal1(n) {
	return(comb(J(1,n,0::n-1),J(n,1,0..n-1)))
}

function pascal2(n) {
	a = I(n)
	a[.,1] = J(n,1,1)
	for (i=3; i<=n; i++) {
		a[i,2..i-1] = a[i-1,2..i-1]+a[i-1,1..i-2]
	}
	return(a)
}

function pascal3(n) {
	a = J(n,n,0)
	for (i=1; i<n; i++) {
		a[i+1,i] = i
	}
	s = p = I(n)
	k = 1
	for (i=0; i<n; i++) {
		p = p*a/k++
		s = s+p
	}
	return(s)
}
end
```


One could trivially write functions for the upper matrix (same operations with transposed matrices).
The symmetric matrix can be generated using loops. However, when the lower matrix is known the other two are
immediately deduced:


```stata
: a = pascal3(5)
: a
       1   2   3   4   5
    +---------------------+
  1 |  1   0   0   0   0  |
  2 |  1   1   0   0   0  |
  3 |  1   2   1   0   0  |
  4 |  1   3   3   1   0  |
  5 |  1   4   6   4   1  |
    +---------------------+

: a'
       1   2   3   4   5
    +---------------------+
  1 |  1   1   1   1   1  |
  2 |  0   1   2   3   4  |
  3 |  0   0   1   3   6  |
  4 |  0   0   0   1   4  |
  5 |  0   0   0   0   1  |
    +---------------------+

: a*a'
[symmetric]
        1    2    3    4    5
    +--------------------------+
  1 |   1                      |
  2 |   1    2                 |
  3 |   1    3    6            |
  4 |   1    4   10   20       |
  5 |   1    5   15   35   70  |
    +--------------------------+
```


The last is a symmetric matrix, but Stata only shows the lower triangular part of symmetric matrices.


## Tcl


```Tcl

package require math

namespace eval pascal {
    proc upper {n} {
        for {set i 0} {$i < $n} {incr i} {
            for {set j 0} {$j < $n} {incr j} {
                puts -nonewline \t[::math::choose $j $i]
            }
            puts ""
        }
    }
    proc lower {n} {
        for {set i 0} {$i < $n} {incr i} {
            for {set j 0} {$j < $n} {incr j} {
                puts -nonewline \t[::math::choose $i $j]
            }
            puts ""
        }
    }
    proc symmetric {n} {
        for {set i 0} {$i < $n} {incr i} {
            for {set j 0} {$j < $n} {incr j} {
                puts -nonewline \t[::math::choose [expr {$i+$j}] $i]
            }
            puts ""
        }
    }
}

foreach type {upper lower symmetric} {
    puts "\n* $type"
    pascal::$type 5
}

```

{{out}}

```txt

* upper 
        1       1       1       1       1
        0       1       2       3       4
        0       0       1       3       6
        0       0       0       1       4
        0       0       0       0       1

* lower 
        1       0       0       0       0
        1       1       0       0       0
        1       2       1       0       0
        1       3       3       1       0
        1       4       6       4       1

* symmetric
        1       1       1       1       1
        1       2       3       4       5
        1       3       6       10      15
        1       4       10      20      35
        1       5       15      35      70

```



## VBA

{{trans|Phix}}

```vb
Option Base 1
Private Function pascal_upper(n As Integer)
    Dim res As Variant: ReDim res(n, n)
    For j = 1 To n
        res(1, j) = 1
    Next j
    For i = 2 To n
        res(i, 1) = 0
        For j = 2 To i
            res(j, i) = res(j, i - 1) + res(j - 1, i - 1)
        Next j
        For j = i + 1 To n
            res(j, i) = 0
        Next j
    Next i
    pascal_upper = res
End Function
 
Private Function pascal_symmetric(n As Integer)
    Dim res As Variant: ReDim res(n, n)
    For i = 1 To n
        res(i, 1) = 1
        res(1, i) = 1
    Next i
    For i = 2 To n
        For j = 2 To n
            res(i, j) = res(i - 1, j) + res(i, j - 1)
        Next j
    Next i
    pascal_symmetric = res
End Function

Private Sub pp(m As Variant)
    For i = 1 To UBound(m)
        For j = 1 To UBound(m, 2)
            Debug.Print Format(m(i, j), "@@@");
        Next j
        Debug.Print
    Next i
End Sub

Public Sub main()
    Debug.Print "
###  Pascal upper matrix 
"
    pp pascal_upper(5)
    Debug.Print "
###  Pascal lower matrix 
"
    pp WorksheetFunction.Transpose(pascal_upper(5))
    Debug.Print "
###  Pascal symmetrical matrix 
"
    pp pascal_symmetric(5)
End Sub
```
{{out}}

```txt

###  Pascal upper matrix 

  1  1  1  1  1
  0  1  2  3  4
  0  0  1  3  6
  0  0  0  1  4
  0  0  0  0  1

###  Pascal lower matrix 

  1  0  0  0  0
  1  1  0  0  0
  1  2  1  0  0
  1  3  3  1  0
  1  4  6  4  1

###  Pascal symmetrical matrix 

  1  1  1  1  1
  1  2  3  4  5
  1  3  6 10 15
  1  4 10 20 35
  1  5 15 35 70
```


## VBScript


```vb

Function pascal_upper(i,j)
	WScript.StdOut.Write "Pascal Upper"
	WScript.StdOut.WriteLine
	For l = i To j
		For m = i To j
			If l <= m Then
				WScript.StdOut.Write binomial(m,l) & vbTab
			Else
				WScript.StdOut.Write 0 & vbTab
			End If
		Next
		WScript.StdOut.WriteLine
	Next
	WScript.StdOut.WriteLine
End Function

Function pascal_lower(i,j)
	WScript.StdOut.Write "Pascal Lower"
	WScript.StdOut.WriteLine
	For l = i To j
		For m = i To j
			If l >= m Then
				WScript.StdOut.Write binomial(l,m) & vbTab
			Else
				WScript.StdOut.Write 0 & vbTab
			End If
		Next
		WScript.StdOut.WriteLine
	Next
	WScript.StdOut.WriteLine	
End Function

Function pascal_symmetric(i,j)
	WScript.StdOut.Write "Pascal Symmetric"
	WScript.StdOut.WriteLine
	For l = i To j 
		For m = i To j
			WScript.StdOut.Write binomial(l+m,m) & vbTab
		Next
		WScript.StdOut.WriteLine
	Next
End Function

Function binomial(n,k)
	binomial = factorial(n)/(factorial(n-k)*factorial(k))
End Function

Function factorial(n)
	If n = 0 Then
		factorial = 1
	Else
		For i = n To 1 Step -1
			If i = n Then
				factorial = n
			Else
				factorial = factorial * i
			End If
		Next
	End If
End Function

'Test driving
Call pascal_upper(0,4)
Call pascal_lower(0,4)
Call pascal_symmetric(0,4)

```


{{Out}}

```txt

Pascal Upper
1	1	1	1	1	
0	1	2	3	4	
0	0	1	3	6	
0	0	0	1	4	
0	0	0	0	1	

Pascal Lower
1	0	0	0	0	
1	1	0	0	0	
1	2	1	0	0	
1	3	3	1	0	
1	4	6	4	1	

Pascal Symmetric
1	1	1	1	1	
1	2	3	4	5	
1	3	6	10	15	
1	4	10	20	35	
1	5	15	35	70	

```



## zkl

{{trans|Python}}

```zkl
fcn binomial(n,k){ (1).reduce(k,fcn(p,i,n){ p*(n-i+1)/i },1,n) }
fcn pascal_upp(n){ [[(i,j); n; n; '{ binomial(j,i) }]]:toMatrix(_) } // [[..]] is list comprehension
fcn pascal_low(n){ [[(i,j); n; n; binomial]]:toMatrix(_) }
fcn pascal_sym(n){ [[(i,j); n; n; '{ binomial(i+j,i) }]]:toMatrix(_) }
fcn toMatrix(ns){ // turn a string of numbers into a square matrix (list of lists)
   cols:=ns.len().toFloat().sqrt().toInt();
   ns.pump(List,T(Void.Read,cols-1),List.create)
}
```


```zkl
fcn prettyPrint(m){ // m is a list of lists
   fmt:=("%3d "*m.len() + "\n").fmt;
   m.pump(String,'wrap(col){ fmt(col.xplode()) });
}
const N=5;
println("Upper:\n",    pascal_upp(N):prettyPrint(_));
println("Lower:\n",    pascal_low(N):prettyPrint(_));
println("Symmetric:\n",pascal_sym(N):prettyPrint(_));
```

{{out}}

```txt

Upper:
  1   1   1   1   1 
  0   1   2   3   4 
  0   0   1   3   6 
  0   0   0   1   4 
  0   0   0   0   1 

Lower:
  1   0   0   0   0 
  1   1   0   0   0 
  1   2   1   0   0 
  1   3   3   1   0 
  1   4   6   4   1 

Symmetric:
  1   1   1   1   1 
  1   2   3   4   5 
  1   3   6  10  15 
  1   4  10  20  35 
  1   5  15  35  70 

```

