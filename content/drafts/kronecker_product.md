+++
title = "Kronecker product"
description = ""
date = 2019-07-12T13:09:41Z
aliases = []
[extra]
id = 21344
[taxonomies]
categories = []
tags = []
+++

{{Wikipedia}}
{{task}}


;Task:
Implement the   [[wp:Kronecker_product|Kronecker product]]   of two matrices (arbitrary sized) resulting in a block matrix. <br />


;Test cases:
Show results for each of the following two samples:



Sample 1 (from Wikipedia):

```txt



          ┌   ┐     ┌   ┐     ┌           ┐
          │1 2│     │0 5│     │ 0  5  0 10│
          │3 4│  x  │6 7│  =  │ 6  7 12 14│
	  └   ┘     └   ┘     │ 0 15  0 20│
	                      │18 21 24 28│
                              └           ┘

```


Sample 2:

```txt

          ┌     ┐     ┌       ┐     ┌                       ┐
          │0 1 0│     │1 1 1 1│     │0 0 0 0 1 1 1 1 0 0 0 0│
          │1 1 1│  x  │1 0 0 1│  =  │0 0 0 0 1 0 0 1 0 0 0 0│
          │0 1 0│     │1 1 1 1│     │0 0 0 0 1 1 1 1 0 0 0 0│
	  └     ┘     └       ┘     │1 1 1 1 1 1 1 1 1 1 1 1│
                                    │1 0 0 1 1 0 0 1 1 0 0 1│
                                    │1 1 1 1 1 1 1 1 1 1 1 1│
                                    │0 0 0 0 1 1 1 1 0 0 0 0│
                                    │0 0 0 0 1 0 0 1 0 0 0 0│
                                    │0 0 0 0 1 1 1 1 0 0 0 0│
                                    └                       ┘

```



See implementations and results below in JavaScript and PARI/GP languages.


;Related task:
* [[Kronecker_product_based_fractals| Kronecker product based fractals]].





## 360 Assembly


```360asm
*        Kronecker product         06/04/2017
KRONECK  CSECT
         USING  KRONECK,R13        base register
         B      72(R15)            skip savearea
         DC     17F'0'             savearea
         STM    R14,R12,12(R13)    save previous context
         ST     R13,4(R15)         link backward
         ST     R15,8(R13)         link forward
         LR     R13,R15            set addressability
         LA     R1,1               first example
         BAL    R14,PRODUCT        call product(a1,b1)
         BAL    R14,PRINT          call print(r)
         XPRNT  =C'---',3          separator
         LA     R1,2               second example
         BAL    R14,PRODUCT        call product(a2,b2)
         BAL    R14,PRINT          call print(r)
         L      R13,4(0,R13)       restore previous savearea pointer
         LM     R14,R12,12(R13)    restore previous context
         XR     R15,R15            rc=0
         BR     R14                exit
*------- ----   ----------------------------------------
PRODUCT  EQU    *                  product(o)
         STC    R1,OO              store o
       IF CLI,OO,EQ,X'01' THEN     if o=1 then
         MVC    MM(8),DIM1           (m,n)=hbound(a1) (p,q)=hbound(b1)
         LA     R1,A1                @a1
         LA     R2,B1                @b1
       ELSE     ,                  else
         MVC    MM(8),DIM2           (m,n)=hbound(a2) (p,q)=hbound(b2)
         LA     R1,A2                @a2
         LA     R2,B2                @b2
       ENDIF    ,                  endif
         ST     R1,ADDRA           addra=@a1
         ST     R2,ADDRB           addrb=@b1
         LH     R1,MM              m
         MH     R1,PP              p
         STH    R1,RI              ri=m*p
         LH     R2,NN              n
         MH     R2,QQ              *q
         STH    R2,RJ              rj=n*q
         LA     R6,1               i=1
       DO WHILE=(CH,R6,LE,MM)      do i=1 to m
         LA     R7,1                 j=1
       DO WHILE=(CH,R7,LE,NN)        do j=1 to n
         LA     R8,1                   k=1
       DO WHILE=(CH,R8,LE,PP)          do k=1 to p
         LA     R9,1                     l=1
       DO WHILE=(CH,R9,LE,QQ)            do l=1 to q
         LR     R1,R6                      i
         BCTR   R1,0
         MH     R1,NN                      *hbound(a,2)
         AR     R1,R7                      j
         BCTR   R1,0
         SLA    R1,2
         L      R4,ADDRA                   @a
         L      R2,0(R4,R1)                r2=a(i,j)
         LR     R1,R8                      k
         BCTR   R1,0
         MH     R1,QQ                      *hbound(b1,2)
         AR     R1,R9                      l
         BCTR   R1,0
         SLA    R1,2
         L      R4,ADDRB                   @b
         L      R3,0(R4,R1)                r3=b(k,l)
         LR     R5,R2                      r2
         MR     R4,R3                      *r3
         LR     R0,R5                      r0=a(i,j)*b(k,l)
         LR     R1,R6                      i
         BCTR   R1,0                       i-1
         MH     R1,PP                      *p
         AR     R1,R8                      r1=p*(i-1)+k
         LR     R2,R7                      j
         BCTR   R2,0                       j-1
         MH     R2,QQ                      *q
         AR     R2,R9                      r2=q*(j-1)+l
         BCTR   R1,0
         MH     R1,=AL2(NR)                *nr
         AR     R1,R2                      r1=r1+r2
         SLA    R1,2
         ST     R0,R-4(R1)                 r(p*(i-1)+k,q*(j-1)+l)=r0
         LA     R9,1(R9)                   l++
       ENDDO    ,                        enddo l
         LA     R8,1(R8)                 k++
       ENDDO    ,                      enddo k
         LA     R7,1(R7)               j++
       ENDDO    ,                    enddo j
         LA     R6,1(R6)             i++
       ENDDO    ,                  enddo i
         BR     R14                return
*------- ----   ----------------------------------------
PRINT    EQU    *                  print
         LA     R6,1               i
       DO WHILE=(CH,R6,LE,RI)      do i=1 to ri
         MVC    PG,=CL80' '          init buffer
         LA     R10,PG               pgi=0
         LA     R7,1                 j
       DO WHILE=(CH,R7,LE,RJ)        do j=1 to rj
         LR     R1,R6                  i
         BCTR   R1,0
         MH     R1,=AL2(NR)            *nr
         AR     R1,R7                  +j
         SLA    R1,2
         L      R2,R-4(R1)             r(i,j)
         XDECO  R2,XDEC                edit
         MVC    0(ND,R10),XDEC+12-ND   output
         LA     R10,ND(R10)            pgi=pgi+nd
         LA     R7,1(R7)               j++
       ENDDO    ,                    enddo j
         XPRNT  PG,L'PG              print buffer
         LA     R6,1(R6)             i++
       ENDDO    ,                  enddo j
         BR     R14                return
*        ----   ----------------------------------------
NR       EQU    32                 dim result max
ND       EQU    3                  digits to print
A1       DC     F'1',F'2'              a1(2,2)
         DC     F'3',F'4'
B1       DC     F'0',F'5'              b1(2,2)
         DC     F'6',F'7'
DIM1     DC     H'2',H'2',H'2',H'2'    dim a1 , dim b1
A2       DC     F'0',F'1',F'0'         a2(3,3)
         DC     F'1',F'1',F'1'
         DC     F'0',F'1',F'0'
B2       DC     F'1',F'1',F'1',F'1'    b2(3,4)
         DC     F'1',F'0',F'0',F'1'
         DC     F'1',F'1',F'1',F'1'
DIM2     DC     H'3',H'3',H'3',H'4'    dim a2 , dim b2
ADDRA    DS     A                  @a
ADDRB    DS     A                  @b
RI       DS     H                  ri
RJ       DS     H                  rj
MM       DS     H                  m
NN       DS     H                  n
PP       DS     H                  p
QQ       DS     H                  q
OO       DS     X                  o
PG       DS     CL80               buffer
XDEC     DS     CL12
         LTORG
R        DS     (NR*NR)F           r(nr,nr)
         YREGS
         END    KRONECK
```

{{out}}

```txt

  0  5  0 10
  6  7 12 14
  0 15  0 20
 18 21 24 28
---
  0  0  0  0  1  1  1  1  0  0  0  0
  0  0  0  0  1  0  0  1  0  0  0  0
  0  0  0  0  1  1  1  1  0  0  0  0
  1  1  1  1  1  1  1  1  1  1  1  1
  1  0  0  1  1  0  0  1  1  0  0  1
  1  1  1  1  1  1  1  1  1  1  1  1
  0  0  0  0  1  1  1  1  0  0  0  0
  0  0  0  0  1  0  0  1  0  0  0  0
  0  0  0  0  1  1  1  1  0  0  0  0

```



## Ada


{{works with|Ada|Ada|83}}


```Ada
with Ada.Text_IO;
with Ada.Integer_Text_IO;

procedure Kronecker_Product is
   type Matrix is array (Positive range <>, Positive range <>) of Integer;

   function "*"(Left, Right : in Matrix) return Matrix is
      result : Matrix
        (1 .. Left'Length(1) * Right'Length(1),
         1 .. Left'Length(2) * Right'Length(2));
      LI : Natural := 0;
      LJ : Natural := 0;
   begin
      for I in 0 .. result'Length(1) - 1 loop
         for J in 0 .. result'Length(2) - 1 loop
            result (I + 1, J + 1) :=
              Left(Left'First(1) + (LI), Left'First(2) + (LJ))
              * Right
                (Right'First(1) + (I mod Right'Length(1)),
                 Right'First(2) + (J mod Right'Length(2)));
            if (J+1) mod Right'Length(2) = 0 then
               LJ := LJ + 1;
            end if;
         end loop;
         if (I+1) mod Right'Length(1) = 0 then
            LI := LI + 1;
         end if;
         LJ := 0;
      end loop;
      return result;
   end "*";

   Left1   : constant Matrix := ((1, 2), (3, 4));
   Right1  : constant Matrix := ((0, 5), (6, 7));
   result1 : constant Matrix := Left1 * Right1;
   Left2   : constant Matrix := ((0, 1, 0), (1, 1, 1), (0, 1, 0));
   Right2  : constant Matrix := ((1, 1, 1, 1), (1, 0, 0, 1), (1, 1, 1, 1));
   result2 : constant Matrix := Left2 * Right2;
begin
   for I in result1'Range(1) loop
      for J in result1'Range(2) loop
         Ada.Integer_Text_IO.Put(Ada.Text_IO.Standard_Output, result1(I, J));
      end loop;
      Ada.Text_IO.New_Line;
   end loop;

   Ada.Text_IO.New_Line;

   for I in result2'Range(1) loop
      for J in result2'Range(2) loop
         Ada.Integer_Text_IO.Put(Ada.Text_IO.Standard_Output, result2(I, J));
      end loop;
      Ada.Text_IO.New_Line;
   end loop;
end Kronecker_Product;
```

{{out}}

```txt
          0          5          0         10
          6          7         12         14
          0         15          0         20
         18         21         24         28

          0          0          0          0          1          1          1          1          0          0          0          0
          0          0          0          0          1          0          0          1          0          0          0          0
          0          0          0          0          1          1          1          1          0          0          0          0
          1          1          1          1          1          1          1          1          1          1          1          1
          1          0          0          1          1          0          0          1          1          0          0          1
          1          1          1          1          1          1          1          1          1          1          1          1
          0          0          0          0          1          1          1          1          0          0          0          0
          0          0          0          0          1          0          0          1          0          0          0          0
          0          0          0          0          1          1          1          1          0          0          0          0

```




## ALGOL 68


```algol68
BEGIN
    # multiplies in-place the elements of the matrix a by the scaler b #
    OP *:= = ( REF[,]INT a, INT b )REF[,]INT:
    BEGIN
        FOR i FROM 1 LWB a TO 1 UPB a DO
            FOR j FROM 2 LWB a TO 2 UPB a DO
                a[ i, j ] *:= b
            OD
        OD;
        a
    END # *:= # ;
    # returns the Kronecker Product of the two matrices a and b #
    # the result will have lower bounds of 1                    #
    PRIO X = 6;
    OP   X = ( [,]INT a, b )[,]INT:
    BEGIN
        # normalise the matrices to have lower bounds of 1 #
        [,]INT l = a[ AT 1, AT 1 ];
        [,]INT r = b[ AT 1, AT 1 ];
        # construct the result #
        INT r 1 size = 1 UPB r;
        INT r 2 size = 2 UPB r;
        [ 1 : 1 UPB l * 1 UPB r, 1 : 2 UPB l * 2 UPB r ]INT k;
        FOR i FROM 1 LWB l TO 1 UPB l DO
            FOR j FROM 2 LWB l TO 2 UPB l DO
                ( k[ 1 + ( ( i - 1 ) * r 1 size ) : i * r 1 size
                   , 1 + ( ( j - 1 ) * r 2 size ) : j * r 2 size
                   ] := r
                ) *:= l[ i, j ]
            OD
        OD;
        k
    END # X # ;
    # prints matrix a with the specified field width #
    PROC print matrix = ( [,]INT a, INT field width )VOID:
    FOR i FROM 1 LWB a TO 1 UPB a DO
        FOR j FROM 2 LWB a TO 2 UPB a DO
            print( ( " ", whole( a[ i, j ], field width ) ) )
        OD;
        print( ( newline ) )
    OD # print matrix # ;
    # task test cases #
    print matrix( [,]INT( ( 1, 2 )
                        , ( 3, 4 )
                        )
                X [,]INT( ( 0, 5 )
                        , ( 6, 7 )
                        )
                , -2
                );
    print( ( newline ) );
    print matrix( [,]INT( ( 0, 1, 0 )
                        , ( 1, 1, 1 )
                        , ( 0, 1, 0 )
                        )
                X [,]INT( ( 1, 1, 1, 1 )
                        , ( 1, 0, 0, 1 )
                        , ( 1, 1, 1, 1 )
                        )
                , -1
                )
END

```

{{out}}

```txt

  0  5  0 10
  6  7 12 14
  0 15  0 20
 18 21 24 28

 0 0 0 0 1 1 1 1 0 0 0 0
 0 0 0 0 1 0 0 1 0 0 0 0
 0 0 0 0 1 1 1 1 0 0 0 0
 1 1 1 1 1 1 1 1 1 1 1 1
 1 0 0 1 1 0 0 1 1 0 0 1
 1 1 1 1 1 1 1 1 1 1 1 1
 0 0 0 0 1 1 1 1 0 0 0 0
 0 0 0 0 1 0 0 1 0 0 0 0
 0 0 0 0 1 1 1 1 0 0 0 0

```



## AppleScript


```applescript
-- KRONECKER PRODUCT OF TWO MATRICES ------------------------------------------

-- kprod :: [[Num]] -> [[Num]] -> [[Num]]
on kprod(xs, ys)
    script concatTranspose
        on |λ|(m)
            map(my concat, my transpose(m))
        end |λ|
    end script

    script
        -- Multiplication by N over a list of lists
        -- f :: [[Num]] -> Num -> [[Num]]
        on f(mx, n)
            script
                on product(a, b)
                    a * b
                end product

                on |λ|(xs)
                    map(curry(product)'s |λ|(n), xs)
                end |λ|
            end script

            map(result, mx)
        end f

        on |λ|(zs)
            map(curry(f)'s |λ|(ys), zs)
        end |λ|
    end script

    concatMap(concatTranspose, map(result, xs))
end kprod

-- TEST ----------------------------------------------------------------------
on run
    unlines(map(show, ¬
        kprod({{1, 2}, {3, 4}}, ¬
            {{0, 5}, {6, 7}}))) & ¬
        linefeed & linefeed & ¬
        unlines(map(show, ¬
            kprod({{0, 1, 0}, {1, 1, 1}, {0, 1, 0}}, ¬
                {{1, 1, 1, 1}, {1, 0, 0, 1}, {1, 1, 1, 1}})))
end run

-- GENERIC FUNCTIONS ---------------------------------------------------------

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

-- concatMap :: (a -> [b]) -> [a] -> [b]
on concatMap(f, xs)
    concat(map(f, xs))
end concatMap

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

-- show :: a -> String
on show(e)
    set c to class of e
    if c = list then
        script serialized
            on |λ|(v)
                show(v)
            end |λ|
        end script

        "{" & intercalate(", ", map(serialized, e)) & "}"
    else if c = record then
        script showField
            on |λ|(kv)
                set {k, v} to kv
                k & ":" & show(v)
            end |λ|
        end script

        "{" & intercalate(", ", ¬
            map(showField, zip(allKeys(e), allValues(e)))) & "}"
    else if c = date then
        ("date \"" & e as text) & "\""
    else if c = text then
        "\"" & e & "\""
    else
        try
            e as text
        on error
            ("«" & c as text) & "»"
        end try
    end if
end show

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

-- unlines :: [String] -> String
on unlines(xs)
    intercalate(linefeed, xs)
end unlines
```

{{Out}}

```txt
{0, 5, 0, 10}
{6, 7, 12, 14}
{0, 15, 0, 20}
{18, 21, 24, 28}

{0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0}
{0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0}
{0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0}
{1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1}
{1, 0, 0, 1, 1, 0, 0, 1, 1, 0, 0, 1}
{1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1}
{0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0}
{0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0}
{0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0}
```



## AWK


```AWK

# syntax: GAWK -f KRONECKER_PRODUCT.AWK
BEGIN {
    A[++a] = "1 2" ; B[++b] = "0 5"
    A[++a] = "3 4" ; B[++b] = "6 7"
    main("sample 1",1234)
    A[++a] = "0 1 0" ; B[++b] = "1 1 1 1"
    A[++a] = "1 1 1" ; B[++b] = "1 0 0 1"
    A[++a] = "0 1 0" ; B[++b] = "1 1 1 1"
    main("sample 2",3)
    exit(0)
}
function main(desc,option) {
#
# option: allows complete flexibility of output; they may be combined
#   1  show A and B matrix
#   2  show A x B
#   3  show product
#   4  show Arow,Acol x Brow,Bcol
#
    printf("%s\n\n",desc)
    if (option ~ /[1234]/) {
      a_rows = show_array(A,"A",option)
      b_rows = show_array(B,"B",option)
      if (option ~ /2/) { prn("A x B",2) }
      if (option ~ /3/) { prn("Product",3) }
      if (option ~ /4/) { prn("Arow,Acol x Brow,Bcol",4) }
    }
    else {
      print("nothing to print")
    }
    print("")
    a = b = 0 # reset
    delete A
    delete B
}
function prn(desc,option,  a_cols,b_cols,w,x,y,z,AA,BB) {
    printf("%s:\n",desc)
    for (w=1; w<=a_rows; w++) {
      a_cols = split(A[w],AA," ")
      for (x=1; x<=b_rows; x++) {
        b_cols = split(B[x],BB," ")
        printf("[ ")
        for (y=1; y<=a_cols; y++) {
          for (z=1; z<=b_cols; z++) {
            if (option ~ /2/) { printf("%sx%s ",AA[y],BB[z]) }
            if (option ~ /3/) { printf("%2s ",AA[y] * BB[z]) }
            if (option ~ /4/) { printf("%s,%sx%s,%s ",w,y,x,z) }
          }
        }
        printf("]\n")
      }
    }
}
function show_array(arr,desc,option,  i,n) {
    for (i in arr) {
      n++
    }
    if (option ~ /1/) {
      printf("Matrix %s:\n",desc)
      for (i=1; i<=n; i++) {
        printf("[ %s ]\n",arr[i])
      }
    }
    return(n)
}

```

{{out}}

```txt

sample 1

Matrix A:
[ 1 2 ]
[ 3 4 ]
Matrix B:
[ 0 5 ]
[ 6 7 ]
A x B:
[ 1x0 1x5 2x0 2x5 ]
[ 1x6 1x7 2x6 2x7 ]
[ 3x0 3x5 4x0 4x5 ]
[ 3x6 3x7 4x6 4x7 ]
Product:
[  0  5  0 10 ]
[  6  7 12 14 ]
[  0 15  0 20 ]
[ 18 21 24 28 ]
Arow,Acol x Brow,Bcol:
[ 1,1x1,1 1,1x1,2 1,2x1,1 1,2x1,2 ]
[ 1,1x2,1 1,1x2,2 1,2x2,1 1,2x2,2 ]
[ 2,1x1,1 2,1x1,2 2,2x1,1 2,2x1,2 ]
[ 2,1x2,1 2,1x2,2 2,2x2,1 2,2x2,2 ]

sample 2

Product:
[  0  0  0  0  1  1  1  1  0  0  0  0 ]
[  0  0  0  0  1  0  0  1  0  0  0  0 ]
[  0  0  0  0  1  1  1  1  0  0  0  0 ]
[  1  1  1  1  1  1  1  1  1  1  1  1 ]
[  1  0  0  1  1  0  0  1  1  0  0  1 ]
[  1  1  1  1  1  1  1  1  1  1  1  1 ]
[  0  0  0  0  1  1  1  1  0  0  0  0 ]
[  0  0  0  0  1  0  0  1  0  0  0  0 ]
[  0  0  0  0  1  1  1  1  0  0  0  0 ]

```



## C

Entering and printing matrices on the console is tedious even for matrices with 4 or more rows and columns. This implementation reads and writes the matrices from and to files. Matrices are taken as double type in order to cover as many use cases as possible.


```C

#include<stdlib.h>
#include<stdio.h>

int main(){

	char input[100],output[100];
	int i,j,k,l,rowA,colA,rowB,colB,rowC,colC,startRow,startCol;
	double **matrixA,**matrixB,**matrixC;

	printf("Enter full path of input file : ");
	fscanf(stdin,"%s",input);

	printf("Enter full path of output file : ");
	fscanf(stdin,"%s",output);

	FILE* inputFile = fopen(input,"r");

	fscanf(inputFile,"%d%d",&rowA,&colA);

	matrixA = (double**)malloc(rowA * sizeof(double*));

	for(i=0;i<rowA;i++){
		matrixA[i] = (double*)malloc(colA*sizeof(double));
		for(j=0;j<colA;j++){
			fscanf(inputFile,"%lf",&matrixA[i][j]);
		}
	}

	fscanf(inputFile,"%d%d",&rowB,&colB);

	matrixB = (double**)malloc(rowB * sizeof(double*));

	for(i=0;i<rowB;i++){
		matrixB[i] = (double*)malloc(colB*sizeof(double));
		for(j=0;j<colB;j++){
			fscanf(inputFile,"%lf",&matrixB[i][j]);
		}
	}

	fclose(inputFile);

	rowC = rowA*rowB;
	colC = colA*colB;

	matrixC = (double**)malloc(rowC*sizeof(double*));

	for(i=0;i<rowA*rowB;i++){
		matrixC[i] = (double*)malloc(colA*colB*sizeof(double));
	}

	for(i=0;i<rowA;i++){
		for(j=0;j<colA;j++){
			startRow = i*rowB;
			startCol = j*colB;
			for(k=0;k<rowB;k++){
				for(l=0;l<colB;l++){
					matrixC[startRow+k][startCol+l] = matrixA[i][j]*matrixB[k][l];
				}
			}
		}
	}

	FILE* outputFile = fopen(output,"w");

	for(i=0;i<rowC;i++){
		for(j=0;j<colC;j++){
			fprintf(outputFile,"%lf\t",matrixC[i][j]);
		}
		fprintf(outputFile,"\n");
	}

	fclose(outputFile);

	printf("\n\n\nKronecker product of the two matrices written to %s.",output);
}

```


Input file :

```txt

3 3
0 1 0
1 1 1
0 1 0
3 4
1 1 1 1
1 0 0 1
1 1 1 1

```


Console interaction :

```txt

Enter full path of input file : input3.txt
Enter full path of output file : output3.txt



Kronecker product of the two matrices written to output3.txt.

```


Output file :

```txt

0.000000	0.000000	0.000000	0.000000	1.000000	1.000000	1.000000	1.000000	0.000000	0.000000	0.000000	0.000000
0.000000	0.000000	0.000000	0.000000	1.000000	0.000000	0.000000	1.000000	0.000000	0.000000	0.000000	0.000000
0.000000	0.000000	0.000000	0.000000	1.000000	1.000000	1.000000	1.000000	0.000000	0.000000	0.000000	0.000000
1.000000	1.000000	1.000000	1.000000	1.000000	1.000000	1.000000	1.000000	1.000000	1.000000	1.000000	1.000000
1.000000	0.000000	0.000000	1.000000	1.000000	0.000000	0.000000	1.000000	1.000000	0.000000	0.000000	1.000000
1.000000	1.000000	1.000000	1.000000	1.000000	1.000000	1.000000	1.000000	1.000000	1.000000	1.000000	1.000000
0.000000	0.000000	0.000000	0.000000	1.000000	1.000000	1.000000	1.000000	0.000000	0.000000	0.000000	0.000000
0.000000	0.000000	0.000000	0.000000	1.000000	0.000000	0.000000	1.000000	0.000000	0.000000	0.000000	0.000000
0.000000	0.000000	0.000000	0.000000	1.000000	1.000000	1.000000	1.000000	0.000000	0.000000	0.000000	0.000000


```



## C#


```c#
using System;
using System.Collections;
using System.Collections.Generic;
using static System.Linq.Enumerable;

public static class KroneckerProduct
{
    public static void Main() {
        int[,] left = { {1, 2}, {3, 4} };
        int[,] right = { {0, 5}, {6, 7} };
        Print(Multiply(left, right));

        left = new [,] { {0, 1, 0}, {1, 1, 1}, {0, 1, 0} };
        right = new [,] { {1, 1, 1, 1}, {1, 0, 0, 1}, {1, 1, 1, 1} };
        Print(Multiply(left, right));
    }

    static int[,] Multiply(int[,] left, int[,] right) {
        (int lRows, int lColumns) = (left.GetLength(0), left.GetLength(1));
        (int rRows, int rColumns) = (right.GetLength(0), right.GetLength(1));
        int[,] result = new int[lRows * rRows, lColumns * rColumns];

        foreach (var (r, c) in from r in Range(0, lRows) from c in Range(0, lColumns) select (r, c)) {
            Copy(r * rRows, c * rColumns, left[r, c]);
        }
        return result;

        void Copy(int startRow, int startColumn, int multiplier) {
            foreach (var (r, c) in from r in Range(0, rRows) from c in Range(0, rColumns) select (r, c)) {
                result[startRow + r, startColumn + c] = right[r, c] * multiplier;
            }
        }
    }

    static void Print(int[,] matrix) {
        (int rows, int columns) = (matrix.GetLength(0), matrix.GetLength(1));
        int width = matrix.Cast<int>().Select(LengthOf).Max();
        for (int row = 0; row < rows; row++) {
            Console.WriteLine("| " + string.Join(" ", Range(0, columns).Select(column => (matrix[row, column] + "").PadLeft(width, ' '))) + " |");
        }
        Console.WriteLine();
    }

    private static int LengthOf(int i) {
        if (i < 0) return LengthOf(-i) + 1;
        int length = 0;
        while (i > 0) {
            length++;
            i /= 10;
        }
        return length;
    }

}
```

{{out}}

```txt

|  0  5  0 10 |
|  6  7 12 14 |
|  0 15  0 20 |
| 18 21 24 28 |

| 0 0 0 0 1 1 1 1 0 0 0 0 |
| 0 0 0 0 1 0 0 1 0 0 0 0 |
| 0 0 0 0 1 1 1 1 0 0 0 0 |
| 1 1 1 1 1 1 1 1 1 1 1 1 |
| 1 0 0 1 1 0 0 1 1 0 0 1 |
| 1 1 1 1 1 1 1 1 1 1 1 1 |
| 0 0 0 0 1 1 1 1 0 0 0 0 |
| 0 0 0 0 1 0 0 1 0 0 0 0 |
| 0 0 0 0 1 1 1 1 0 0 0 0 |
```



## D

{{Trans|Go}}

```D

import std.stdio, std.outbuffer;

alias Matrix = uint[][];

string toString(Matrix m) {
    auto ob = new OutBuffer();
    foreach(row; m) {
        //The format specifier inside the %(...%) is
        //automatically applied to each element of a range
        //Thus prints each line flanked by |
        ob.writefln("|%(%2d %)|", row);
    }
    return ob.toString;
}

Matrix kronecker(Matrix m1, Matrix m2) {
    Matrix p = new uint[][m1.length*m2.length];
    foreach(r1i, r1; m1) {
        foreach(r2i, r2; m2) {
            auto rp = new uint[r1.length*r2.length];
            foreach(c1i, e1; r1) {
                    foreach(c2i, e2; r2) {
                        rp[c1i*r2.length+c2i] = e1*e2;
                    }
            }
            p[r1i*m2.length+r2i] = rp;
        }
    }
    return p;
}

void sample(Matrix m1, Matrix m2) {
    auto res = kronecker(m1, m2);
    writefln("Matrix A:\n%s\nMatrix B:\n%s\nA (X) B:\n%s", m1.toString, m2.toString, res.toString);
}

void main() {
    Matrix A = [[1,2],[3,4]];
    Matrix B = [[0,5],[6,7]];

    sample(A,B);

    Matrix C =
    [[0,1,0],
     [1,1,1],
     [0,1,0]];
    Matrix D =
    [[1,1,1,1],
     [1,0,0,1],
     [1,1,1,1]];

    sample(C,D);
}

```


Output:

```txt

Matrix A:
| 1  2|
| 3  4|

Matrix B:
| 0  5|
| 6  7|

A (X) B:
| 0  5  0 10|
| 6  7 12 14|
| 0 15  0 20|
|18 21 24 28|

Matrix A:
| 0  1  0|
| 1  1  1|
| 0  1  0|

Matrix B:
| 1  1  1  1|
| 1  0  0  1|
| 1  1  1  1|

A (X) B:
| 0  0  0  0  1  1  1  1  0  0  0  0|
| 0  0  0  0  1  0  0  1  0  0  0  0|
| 0  0  0  0  1  1  1  1  0  0  0  0|
| 1  1  1  1  1  1  1  1  1  1  1  1|
| 1  0  0  1  1  0  0  1  1  0  0  1|
| 1  1  1  1  1  1  1  1  1  1  1  1|
| 0  0  0  0  1  1  1  1  0  0  0  0|
| 0  0  0  0  1  0  0  1  0  0  0  0|
| 0  0  0  0  1  1  1  1  0  0  0  0|

```



## Factor


```factor
USING: kernel math.matrices prettyprint ;
IN: rosetta-code.kronecker

{ { 1 2 } { 3 4 } }
{ { 0 5 } { 6 7 } }
{ { 0 1 0 } { 1 1 1 } { 0 1 0 } }
{ { 1 1 1 1 } { 1 0 0 1 } { 1 1 1 1 } }
[ kron . ] 2bi@
```

{{out}}

```txt

{ { 0 5 0 10 } { 6 7 12 14 } { 0 15 0 20 } { 18 21 24 28 } }
{
    { 0 0 0 0 1 1 1 1 0 0 0 0 }
    { 0 0 0 0 1 0 0 1 0 0 0 0 }
    { 0 0 0 0 1 1 1 1 0 0 0 0 }
    { 1 1 1 1 1 1 1 1 1 1 1 1 }
    { 1 0 0 1 1 0 0 1 1 0 0 1 }
    { 1 1 1 1 1 1 1 1 1 1 1 1 }
    { 0 0 0 0 1 1 1 1 0 0 0 0 }
    { 0 0 0 0 1 0 0 1 0 0 0 0 }
    { 0 0 0 0 1 1 1 1 0 0 0 0 }
}

```



## Fortran

The plan is to pass the two arrays to a subroutine, which will return their Kronecker product as a third parameter. This relies on the expanded array-handling facilities introduced with F90, especially the ability of a subroutine to allocate an array of a size of its choosing, this array being a parameter to the subroutine. Some compilers offering the "allocate" statement do not handle this! Further features of the MODULE protocol of F90 allow arrays passed to a subroutine to have their sizes ascertained in the subroutine (via function UBOUND, ''etc.'') rather than this information being supplied via the programmer coding additional parameters. This is not all to the good: multi-dimensional arrays must therefore be the actual size of their usage rather than say A(100,100) but only using the first fifty elements (in one place) and the first thirty in another. Thus, for such usage the array must be re-allocated the correct size each time, and, the speed of access to such arrays is reduced - see [[Sequence_of_primorial_primes#Fixed-size_data_aggregates]] for an example. Similarly, suppose a portion of a large array is to be passed as a parameter, as is enabled by F90 syntax such as <code>A(3:7,9:12)</code> to select a 5x4 block: those elements will ''not'' be in contiguous memory locations, as is expected by the subroutine, so they will be copied into a temporary storage area that will become the parameter and their values will be copied back on return. Copy-in copy-out, instead of by reference. With large arrays, this imposes a large overhead. A further detail of the MODULE protocol when passing arrays is that if the parameter's declaration does not specify the lower bound, it will be treated as if it were one even if the actual array is declared otherwise - see [[Array_length#Fortran]] for example.

In older-style Fortran, the arrays would be of some "surely-big-enough" size, fixed at compile time, and there would be additional parameters describing the bounds in use for each invocation. Since no array-assignment statements were available, there would be additional DO-loops to copy each block of values. In all versions of Fortran, the ordering of array elements in storage is "column-major" so that the DATA statement appears to initialise the arrays with their transpose - see [[Matrix_transposition#Fortran]] for example. As a result, the default output order for an array, if written as just <code>WRITE (6,*) A</code> will be that of the transposed order, just as with the default order of the DATA statement's data. To show the desired order of A(''row'',''column''), the array must be written with explicit specification of the order of elements, as is done by subroutine SHOW: columns across the page, rows running down the page.
```Fortran
      MODULE ARRAYMUSH	!A rather small collection.
       CONTAINS		!For the specific problem only.
        SUBROUTINE KPRODUCT(A,B,AB)	!AB = Kronecker product of A and B, both two-dimensional arrays.
Considers the arrays to be addressed as A(row,column), despite any storage order arrangements.        .
Creating array AB to fit here, adjusting the caller's array AB, may not work on some compilers.
         INTEGER A(:,:),B(:,:)		!Two-dimensional arrays, lower bound one.
         INTEGER, ALLOCATABLE:: AB(:,:)	!To be created to fit.
         INTEGER R,RA,RB,C,CA,CB,I	!Assistants.
          RA = UBOUND(A,DIM = 1)	!Ascertain the upper bounds of the incoming arrays.
          CA = UBOUND(A,DIM = 2)	!Their lower bounds will be deemed one,
          RB = UBOUND(B,DIM = 1)	!And the upper bound as reported will correspond.
          CB = UBOUND(B,DIM = 2)	!UBOUND(A) would give an array of two values, RA and CA, more for higher dimensionality.
          WRITE (6,1) "A",RA,CA,"B",RB,CB,"A.k.B",RA*RB,CA*CB	!Announce.
    1     FORMAT (3(A," is ",I0,"x",I0,1X))	!Three sets of sizes.
          IF (ALLOCATED(AB)) DEALLOCATE(AB)	!Discard any lingering storage.
          ALLOCATE (AB(RA*RB,CA*CB))		!Obtain the exact desired size.
          R = 0		!Syncopation: start the row offset.
          DO I = 1,RA	!Step down the rows of A.
            C = 0		!For each row, start the column offset.
            DO J = 1,CA		!Step along the columns of A.
              AB(R + 1:R + RB,C + 1:C + CB) = A(I,J)*B	!Place a block of B values.
              C = C + CB		!Advance a block of columns.
            END DO		!On to the next column of A.
            R = R + RB		!Advance a block of rows.
          END DO	!On to the next row of A.
        END SUBROUTINE KPRODUCT	!No tests for bad parameters, or lack of storage...

        SUBROUTINE SHOW(F,A)	!Write array A in row,column order.
         INTEGER F	!Output file unit number.
         INTEGER A(:,:)	!The 2-D array, lower bound one.
         INTEGER R	!The row stepper.
          DO R = 1,UBOUND(A,DIM = 1)	!Each row gets its own line.
            WRITE (F,1) A(R,:)		!Write all the columns of that row.
    1       FORMAT (666I3)		!This suffices for the example.
          END DO			!On to the next row.
        END SUBROUTINE SHOW	!WRITE (F,*) A or similar would show A as if transposed.
      END MODULE ARRAYMUSH	!That was simple enough.

      PROGRAM POKE
      USE ARRAYMUSH
      INTEGER A(2,2),B(2,2)		!First test: square arrays.
      INTEGER, ALLOCATABLE:: AB(:,:)	!To be created for each result.
      INTEGER C(3,3),D(3,4)		!Second test: some rectilinearity.
      DATA A/1,3, 2,4/,B/0,6, 5,7/	!Furrytran uses "column-major" order for successive storage elements.
      DATA C/0,1,0, 1,1,1, 0,1,0/	!So, the first three values go down the rows of the first column.
      DATA D/1,1,1, 1,0,1, 1,0,1, 1,1,1/!And then follow the values for the next column, etc.

      WRITE (6,*) "First test..."
      CALL KPRODUCT(A,B,AB)
      CALL SHOW (6,AB)

      WRITE (6,*)
      WRITE (6,*) "Second test..."
      CALL KPRODUCT(C,D,AB)
      CALL SHOW (6,AB)

      END
```


Output:

```txt

 First test...
A is 2x2 B is 2x2 A.k.B is 4x4
  0  5  0 10
  6  7 12 14
  0 15  0 20
 18 21 24 28

 Second test...
A is 3x3 B is 3x4 A.k.B is 9x12
  0  0  0  0  1  1  1  1  0  0  0  0
  0  0  0  0  1  0  0  1  0  0  0  0
  0  0  0  0  1  1  1  1  0  0  0  0
  1  1  1  1  1  1  1  1  1  1  1  1
  1  0  0  1  1  0  0  1  1  0  0  1
  1  1  1  1  1  1  1  1  1  1  1  1
  0  0  0  0  1  1  1  1  0  0  0  0
  0  0  0  0  1  0  0  1  0  0  0  0
  0  0  0  0  1  1  1  1  0  0  0  0

```


An alternative approach is not to produce the array AB at all, just calculate its elements as needed. Using the array dimension variables as defined above,
```Fortran
AB(i,j) = A((i - 1)/RB + 1,(j - 1)/CB + 1)*B(MOD(i - 1,RB) + 1,MOD(j - 1,CB) + 1))
```
 with the subtracting and adding of one necessary because array indexing starts with row one and column one. With F90, they could start at zero (or any desired value) but if so, you will have to be very careful with counting. For instance, <code>DO I = 1,RA</code> must become <code>DO I = 0,RA - 1</code> and so forth.


## FreeBASIC


```freebasic
' version 06-04-2017
' compile with: fbc -s console

Sub kronecker_product(a() As Long, b() As Long, frmt As String = "#")

    Dim As Long i, j, k, l
    Dim As Long la1 = LBound(a, 1) : Dim As Long ua1 = UBound(a, 1)
    Dim As Long la2 = LBound(a, 2) : Dim As Long ua2 = UBound(a, 2)
    Dim As Long lb1 = LBound(b, 1) : Dim As Long ub1 = UBound(b, 1)
    Dim As Long lb2 = LBound(b, 2) : Dim As Long ub2 = UBound(b, 2)

    For i = la1 To ua1
        For k = lb1 To ub1
            Print "[";
            For j = la2 To ua2
                For l = lb2 To ub2
                    Print Using frmt; a(i, j) * b(k, l);
                    If j = ua1 And l = ub2 Then
                        Print "]"
                    Else
                        Print " ";
                    End If
                Next
            Next
        Next
    Next

End Sub

' ------=< MAIN >=-----

Dim As Long a(1 To 2, 1 To 2) = {{1, 2}, _
                                 {3, 4}}
Dim As Long b(1 To 2, 1 To 2) = {{0, 5}, _
                                 {6, 7}}
kronecker_product(a(), b(), "##")

Print
Dim As Long x(1 To 3, 1 To 3) = {{0, 1, 0}, _
                                 {1, 1, 1}, _
                                 {0, 1, 0}}
Dim As Long y(1 To 3, 1 To 4) = {{1, 1, 1, 1}, _
                                 {1, 0, 0, 1}, _
                                 {1, 1, 1, 1}}
kronecker_product(x(), y())

' empty keyboard buffer
While InKey <> "" : Wend
Print : Print "hit any key to end program"
Sleep
End
```

{{out}}

```txt
[ 0  5  0 10]
[ 6  7 12 14]
[ 0 15  0 20]
[18 21 24 28]

[0 0 0 0 1 1 1 1 0 0 0 0]
[0 0 0 0 1 0 0 1 0 0 0 0]
[0 0 0 0 1 1 1 1 0 0 0 0]
[1 1 1 1 1 1 1 1 1 1 1 1]
[1 0 0 1 1 0 0 1 1 0 0 1]
[1 1 1 1 1 1 1 1 1 1 1 1]
[0 0 0 0 1 1 1 1 0 0 0 0]
[0 0 0 0 1 0 0 1 0 0 0 0]
[0 0 0 0 1 1 1 1 0 0 0 0]
```



## Go


### Implementation


```go
package main

import (
    "fmt"
    "strings"
)

type uintMatrix [][]uint

func (m uintMatrix) String() string {
    var max uint
    for _, r := range m {
        for _, e := range r {
            if e > max {
                max = e
            }
        }
    }
    w := len(fmt.Sprint(max))
    b := &strings.Builder{}
    for _, r := range m {
        fmt.Fprintf(b, "|%*d", w, r[0])
        for _, e := range r[1:] {
            fmt.Fprintf(b, " %*d", w, e)
        }
        fmt.Fprintln(b, "|")
    }
    return b.String()
}

func kronecker(m1, m2 uintMatrix) uintMatrix {
    p := make(uintMatrix, len(m1)*len(m2))
    for r1i, r1 := range m1 {
        for r2i, r2 := range m2 {
            rp := make([]uint, len(r1)*len(r2))
            for c1i, e1 := range r1 {
                for c2i, e2 := range r2 {
                    rp[c1i*len(r2)+c2i] = e1 * e2
                }
            }
            p[r1i*len(m2)+r2i] = rp
        }
    }
    return p
}

func sample(m1, m2 uintMatrix) {
    fmt.Println("m1:")
    fmt.Print(m1)
    fmt.Println("m2:")
    fmt.Print(m2)
    fmt.Println("m1 ⊗ m2:")
    fmt.Print(kronecker(m1, m2))
}

func main() {
    sample(uintMatrix{
        {1, 2},
        {3, 4},
    }, uintMatrix{
        {0, 5},
        {6, 7},
    })
    sample(uintMatrix{
        {0, 1, 0},
        {1, 1, 1},
        {0, 1, 0},
    }, uintMatrix{
        {1, 1, 1, 1},
        {1, 0, 0, 1},
        {1, 1, 1, 1},
    })
}
```

{{out}}

```txt

m1:
|1 2|
|3 4|
m2:
|0 5|
|6 7|
m1 ⊗ m2:
| 0  5  0 10|
| 6  7 12 14|
| 0 15  0 20|
|18 21 24 28|
m1:
|0 1 0|
|1 1 1|
|0 1 0|
m2:
|1 1 1 1|
|1 0 0 1|
|1 1 1 1|
m1 ⊗ m2:
|0 0 0 0 1 1 1 1 0 0 0 0|
|0 0 0 0 1 0 0 1 0 0 0 0|
|0 0 0 0 1 1 1 1 0 0 0 0|
|1 1 1 1 1 1 1 1 1 1 1 1|
|1 0 0 1 1 0 0 1 1 0 0 1|
|1 1 1 1 1 1 1 1 1 1 1 1|
|0 0 0 0 1 1 1 1 0 0 0 0|
|0 0 0 0 1 0 0 1 0 0 0 0|
|0 0 0 0 1 1 1 1 0 0 0 0|

```



### Library go.matrix


```go
package main

import (
    "fmt"

    "github.com/skelterjohn/go.matrix"
)

func main() {
    fmt.Println(matrix.Kronecker(
        matrix.MakeDenseMatrixStacked([][]float64{
            {1, 2},
            {3, 4},
        }),
        matrix.MakeDenseMatrixStacked([][]float64{
            {0, 5},
            {6, 7},
        })))
    fmt.Println()
    fmt.Println(matrix.Kronecker(
        matrix.MakeDenseMatrixStacked([][]float64{
            {0, 1, 0},
            {1, 1, 1},
            {0, 1, 0},
        }),
        matrix.MakeDenseMatrixStacked([][]float64{
            {1, 1, 1, 1},
            {1, 0, 0, 1},
            {1, 1, 1, 1},
        })))
}
```

{{out}}

```txt

{ 0,  5,  0, 10,
  6,  7, 12, 14,
  0, 15,  0, 20,
 18, 21, 24, 28}

{0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0,
 0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0,
 0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0,
 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
 1, 0, 0, 1, 1, 0, 0, 1, 1, 0, 0, 1,
 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
 0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0,
 0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0,
 0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0}

```


### Library gonum/matrix

Gonum/matrix doesn't have the Kronecker product, but here's an implementation using available methods.

```go
package main

import (
    "fmt"

    "github.com/gonum/matrix/mat64"
)

func kronecker(a, b mat64.Matrix) *mat64.Dense {
    ar, ac := a.Dims()
    br, bc := b.Dims()
    k := mat64.NewDense(ar*br, ac*bc, nil)
    for i := 0; i < ar; i++ {
        for j := 0; j < ac; j++ {
            s := k.Slice(i*br, (i+1)*br, j*bc, (j+1)*bc).(*mat64.Dense)
            s.Scale(a.At(i, j), b)
        }
    }
    return k
}

func main() {
    fmt.Println(mat64.Formatted(kronecker(
        mat64.NewDense(2, 2, []float64{
            1, 2,
            3, 4,
        }),
        mat64.NewDense(2, 2, []float64{
            0, 5,
            6, 7,
        }))))
    fmt.Println()
    fmt.Println(mat64.Formatted(kronecker(
        mat64.NewDense(3, 3, []float64{
            0, 1, 0,
            1, 1, 1,
            0, 1, 0,
        }),
        mat64.NewDense(3, 4, []float64{
            1, 1, 1, 1,
            1, 0, 0, 1,
            1, 1, 1, 1,
        }))))
}
```

{{out}}

```txt

⎡ 0   5   0  10⎤
⎢ 6   7  12  14⎥
⎢ 0  15   0  20⎥
⎣18  21  24  28⎦

⎡0  0  0  0  1  1  1  1  0  0  0  0⎤
⎢0  0  0  0  1  0  0  1  0  0  0  0⎥
⎢0  0  0  0  1  1  1  1  0  0  0  0⎥
⎢1  1  1  1  1  1  1  1  1  1  1  1⎥
⎢1  0  0  1  1  0  0  1  1  0  0  1⎥
⎢1  1  1  1  1  1  1  1  1  1  1  1⎥
⎢0  0  0  0  1  1  1  1  0  0  0  0⎥
⎢0  0  0  0  1  0  0  1  0  0  0  0⎥
⎣0  0  0  0  1  1  1  1  0  0  0  0⎦

```



## Haskell


```haskell
import Data.List (transpose)

kprod
  :: Num a
  => [[a]] -> [[a]] -> [[a]]
kprod xs ys =
  let f = fmap . fmap . (*) -- Multiplication by n over list of lists
  in fmap concat . transpose =<< fmap (`f` ys) <$> xs

main :: IO ()
main = do
  mapM_ print $ kprod [[1, 2], [3, 4]] [[0, 5], [6, 7]]
  putStrLn []
  mapM_ print $
    kprod
      [[0, 1, 0], [1, 1, 1], [0, 1, 0]]
      [[1, 1, 1, 1], [1, 0, 0, 1], [1, 1, 1, 1]]
```

{{Out}}

```txt

[0,5,0,10]
[6,7,12,14]
[0,15,0,20]
[18,21,24,28]

[0,0,0,0,1,1,1,1,0,0,0,0]
[0,0,0,0,1,0,0,1,0,0,0,0]
[0,0,0,0,1,1,1,1,0,0,0,0]
[1,1,1,1,1,1,1,1,1,1,1,1]
[1,0,0,1,1,0,0,1,1,0,0,1]
[1,1,1,1,1,1,1,1,1,1,1,1]
[0,0,0,0,1,1,1,1,0,0,0,0]
[0,0,0,0,1,0,0,1,0,0,0,0]
[0,0,0,0,1,1,1,1,0,0,0,0]
```



## J


We can build Kronecker product from tensor outer product by transposing some dimensions of the result and then merging some dimensions.

Explicit implementation:


```J
KP=: dyad def ',/"2 ,/ 1 3 |: x */ y'
```


Tacit:


```J
KP=: 1 3 ,/"2@(,/)@|: */
```


these definitions are functionally equivalent.

Task examples:


```J
   M=: 1+i.2 2
   N=: (+4**)i.2 2
   P=: -.0 2 6 8 e.~i.3 3
   Q=: -.5 6 e.~i.3 4
   M KP N
 0  5  0 10
 6  7 12 14
 0 15  0 20
18 21 24 28
   P KP Q
0 0 0 0 1 1 1 1 0 0 0 0
0 0 0 0 1 0 0 1 0 0 0 0
0 0 0 0 1 1 1 1 0 0 0 0
1 1 1 1 1 1 1 1 1 1 1 1
1 0 0 1 1 0 0 1 1 0 0 1
1 1 1 1 1 1 1 1 1 1 1 1
0 0 0 0 1 1 1 1 0 0 0 0
0 0 0 0 1 0 0 1 0 0 0 0
0 0 0 0 1 1 1 1 0 0 0 0
```



## Java



```Java

package kronecker;

/**
 * Defines a function to calculate the Kronecker product of two
 * rectangular matrices and tests it with two examples.
 */
public class Product {
  /**
   * Find the Kronecker product of the arguments.
   * @param a The first matrix to multiply.
   * @param b The second matrix to multiply.
   * @return A new matrix: the Kronecker product of the arguments.
   */
  public static int[][] product(final int[][] a, final int[][] b) {
    // Create matrix c as the matrix to fill and return.
    // The length of a matrix is its number of rows.
    final int[][] c = new int[a.length*b.length][];
    // Fill in the (empty) rows of c.
    // The length of each row is the number of columns.
    for (int ix = 0; ix < c.length; ix++) {
      final int num_cols = a[0].length*b[0].length;
      c[ix] = new int[num_cols];
    }
    // Now fill in the values: the products of each pair.
    // Go through all the elements of a.
    for (int ia = 0; ia < a.length; ia++) {
      for (int ja = 0; ja < a[ia].length; ja++) {
        // For each element of a, multiply it by all the elements of b.
        for (int ib = 0; ib < b.length; ib++) {
          for (int jb = 0; jb < b[ib].length; jb++) {
             c[b.length*ia+ib][b[ib].length*ja+jb] = a[ia][ja] * b[ib][jb];
          }
        }
      }
    }

    // Return the completed product matrix c.
    return c;
  }

  /**
   * Print an integer matrix, lining up the columns by the width
   * of the longest printed element.
   * @param m The matrix to print.
   */
  public static void print_matrix(final int[][] m) {
    // Printing the matrix neatly is the most complex part.
    // For clean formatting, convert each number to a string
    // and find length of the longest of these strings.
    // Build a matrix of these strings to print later.
    final String[][] sts = new String[m.length][];
    int max_length = 0;  // Safe, since all lengths are positive here.
    for (int im = 0; im < m.length; im++) {
      sts[im] = new String[m[im].length];
      for (int jm = 0; jm < m[im].length; jm++) {
        final String st = String.valueOf(m[im][jm]);
        if (st.length() > max_length) {
          max_length = st.length();
        }
        sts[im][jm] = st;
      }
    }

    // Now max_length holds the length of the longest string.
    // Build a format string to right justify the strings in a field
    // of this length.
    final String format = String.format("%%%ds", max_length);
    for (int im = 0; im < m.length; im++) {
      System.out.print("|");
      // Stop one short to avoid a trailing space.
      for (int jm = 0; jm < m[im].length - 1; jm++) {
        System.out.format(format, m[im][jm]);
        System.out.print(" ");
      }
      System.out.format(format, m[im][m[im].length - 1]);
      System.out.println("|");
    }
  }

  /**
   * Run a test by printing the arguments, computing their
   * Kronecker product, and printing it.
   * @param a The first matrix to multiply.
   * @param b The second matrix to multiply.
   */
  private static void test(final int[][] a, final int[][] b) {
    // Print out matrices and their product.
    System.out.println("Testing Kronecker product");
    System.out.println("Size of matrix a: " + a.length + " by " + a[0].length);
    System.out.println("Matrix a:");
    print_matrix(a);
    System.out.println("Size of matrix b: " + b.length + " by " + b[0].length);
    System.out.println("Matrix b:");
    print_matrix(b);
    System.out.println("Calculating matrix c as Kronecker product");
    final int[][] c = product(a, b);
    System.out.println("Size of matrix c: " + c.length + " by " + c[0].length);
    System.out.println("Matrix c:");
    print_matrix(c);
  }

  /**
   * Create the matrices for the first test and run the test.
   */
  private static void test1() {
    // Test 1: Create a and b.
    final int[][] a = new int[2][];  // 2 by 2
    a[0] = new int[]{1, 2};
    a[1] = new int[]{3, 4};
    final int[][] b = new int[2][];  // 2 by 2
    b[0] = new int[]{0, 5};
    b[1] = new int[]{6, 7};
    // Run the test.
    test(a, b);
  }

  /**
   * Create the matrices for the first test and run the test.
   */
  private static void test2() {
    // Test 2: Create a and b.
    final int[][] a = new int[3][];  // 3 by 3
    a[0] = new int[]{0, 1, 0};
    a[1] = new int[]{1, 1, 1};
    a[2] = new int[]{0, 1, 0};
    final int[][] b = new int[3][];  // 3 by 4
    b[0] = new int[]{1, 1, 1, 1};
    b[1] = new int[]{1, 0, 0, 1};
    b[2] = new int[]{1, 1, 1, 1};
    // Run the test.
    test(a, b);
  }

  /**
   * Run the program to run the two tests.
   * @param args Command line arguments (not used).
   */
  public static void main(final String[] args) {
    // Test the product method.
    test1();
    test2();
  }

}

```


{{Output}}

```txt

Testing Kronecker product
Size of matrix a: 2 by 2
Matrix a:
|1 2|
|3 4|
Size of matrix b: 2 by 2
Matrix b:
|0 5|
|6 7|
Calculating matrix c as Kronecker product
Size of matrix c: 4 by 4
Matrix c:
| 0  5  0 10|
| 6  7 12 14|
| 0 15  0 20|
|18 21 24 28|
Testing Kronecker product
Size of matrix a: 3 by 3
Matrix a:
|0 1 0|
|1 1 1|
|0 1 0|
Size of matrix b: 3 by 4
Matrix b:
|1 1 1 1|
|1 0 0 1|
|1 1 1 1|
Calculating matrix c as Kronecker product
Size of matrix c: 9 by 12
Matrix c:
|0 0 0 0 1 1 1 1 0 0 0 0|
|0 0 0 0 1 0 0 1 0 0 0 0|
|0 0 0 0 1 1 1 1 0 0 0 0|
|1 1 1 1 1 1 1 1 1 1 1 1|
|1 0 0 1 1 0 0 1 1 0 0 1|
|1 1 1 1 1 1 1 1 1 1 1 1|
|0 0 0 0 1 1 1 1 0 0 0 0|
|0 0 0 0 1 0 0 1 0 0 0 0|
|0 0 0 0 1 1 1 1 0 0 0 0|

```



## JavaScript


### Imperative


### =Version #1.=

{{Works with|Chrome}}

```javascript

// matkronprod.js
// Prime function:
// mkp arrow function: Return the Kronecker product of the a and b matrices.
// Note: both a and b must be matrices, i.e., 2D rectangular arrays.
mkp=(a,b)=>a.map(a=>b.map(b=>a.map(y=>b.map(x=>r.push(y*x)),t.push(r=[]))),t=[])&&t;
// Helper functions:
// Log title and matrix mat to console
function matl2cons(title,mat) {console.log(title); console.log(mat.join`\n`)}
// Print title to document
function pttl2doc(title) {document.write('<b>'+title+'</b><br />')}
// Print title and matrix mat to document
function matp2doc(title,mat) {
  document.write('<b>'+title+'</b>:<br />');
  for (var i = 0; i < mat.length; i++) {
    document.write('  '+mat[i].join(' ')+'<br />');
  }
}

```


;Required tests:

```html

<!-- KronProdTest.html -->
<html><head>
  <title>Kronecker product: Sample 1 (from Wikipedia) and Sample 2</title>
  <script src="matkronprod.js"></script>
  <script>
  var mr,ttl='Kronecker product of A and B matrices';
  [ {a:[[1,2],[3,4]],b:[[0,5],[6,7]] },
    {a:[[0,1,0],[1,1,1],[0,1,0]],b:[[1,1,1,1],[1,0,0,1],[1,1,1,1]] }
  ].forEach(m=>{
    console.log(ttl); pttl2doc(ttl);
    matl2cons('A',m.a); matp2doc('A',m.a);
    matl2cons('B',m.b); matp2doc('B',m.b);
    mr=mkp(m.a,m.b);
    matl2cons('A x B',mr); matp2doc('A x B',mr);
    })
  </script>
</head><body></body>
</html>

```

{{Output}} '''Console and page results'''

```txt

Kronecker product of A and B matrices
A
1,2
3,4
B
0,5
6,7
A x B
0,5,0,10
6,7,12,14
0,15,0,20
18,21,24,28
Kronecker product of A and B matrices
A
0,1,0
1,1,1
0,1,0
B
1,1,1,1
1,0,0,1
1,1,1,1
A x B
0,0,0,0,1,1,1,1,0,0,0,0
0,0,0,0,1,0,0,1,0,0,0,0
0,0,0,0,1,1,1,1,0,0,0,0
1,1,1,1,1,1,1,1,1,1,1,1
1,0,0,1,1,0,0,1,1,0,0,1
1,1,1,1,1,1,1,1,1,1,1,1
0,0,0,0,1,1,1,1,0,0,0,0
0,0,0,0,1,0,0,1,0,0,0,0
0,0,0,0,1,1,1,1,0,0,0,0

```



### =Version #2.=

This version is more understandable for sure.
{{trans|PARI/GP}}
{{Works with|Chrome}}

```javascript

// matkronprod2.js
// Prime function:
// mkp2(): Return the Kronecker product of the a and b matrices
// Note: both a and b must be matrices, i.e., 2D rectangular arrays.
function mkp2(a,b) {
  var m=a.length, n=a[0].length, p=b.length, q=b[0].length;
  var rtn=m*p, ctn=n*q; var r=new Array(rtn);
  for (var i=0; i<rtn; i++) {r[i]=new Array(ctn)
    for (var j=0;j<ctn;j++) {r[i][j]=0}
  }
  for (var i=0; i<m; i++) {
    for (var j=0; j<n; j++) {
      for (var k=0; k<p; k++) {
        for (var l=0; l<q; l++) {
          r[p*i+k][q*j+l]=a[i][j]*b[k][l];
        }}}}//all4forend
  return(r);
}
// Helper functions:
// Log title and matrix mat to console
function matl2cons(title,mat) {console.log(title); console.log(mat.join`\n`)}
// Print title to document
function pttl2doc(title) {document.write('<b>'+title+'</b>
')}
// Print title and matrix mat to document
function matp2doc(title,mat) {
  document.write('<b>'+title+'</b>:
');
  for (var i=0; i < mat.length; i++) {
    document.write('  '+mat[i].join(' ')+'
');
  }
}

```

;Required tests:

```html

<!-- KronProdTest2.html -->
<html><head>
  <title>Kronecker product v.2: Sample 1 (from Wikipedia) and Sample 2</title>
  <script src="matkronprod2.js"></script>
  <script>
  var mr,ttl='Kronecker product of A and B matrices';
  [ {a:[[1,2],[3,4]],b:[[0,5],[6,7]] },
    {a:[[0,1,0],[1,1,1],[0,1,0]],b:[[1,1,1,1],[1,0,0,1],[1,1,1,1]] }
  ].forEach(m=>{
    console.log(ttl); pttl2doc(ttl);
    matl2cons('A',m.a); matp2doc('A',m.a);
    matl2cons('B',m.b); matp2doc('B',m.b);
    mr=mkp2(m.a,m.b);
    matl2cons('A x B',mr); matp2doc('A x B',mr);
    })
  </script>
</head><body></body>
</html>

```


{{Output}} '''Console and page results'''

```txt

Output is identical to Version #1 above.

```



### Functional


### =ES6=

{{Trans|Haskell}}
(As JavaScript is now widely embedded in non-browser contexts, a non-HTML string value is returned here, rather than invoking a DOM method, which will not always be available to a JavaScript interpreter)

```javascript
(() => {
    'use strict';

    // GENERIC FUNCTIONS ------------------------------------------------------

    // concat :: [[a]] -> [a]
    const concat = xs => [].concat.apply([], xs);

    // concatMap :: (a -> [b]) -> [a] -> [b]
    const concatMap = (f, xs) => [].concat.apply([], xs.map(f));

    // 2 or more arguments
    // curry :: Function -> Function
    const curry = (f, ...args) => {
        const go = xs => xs.length >= f.length ? (f.apply(null, xs)) :
            function () {
                return go(xs.concat([].slice.apply(arguments)));
            };
        return go([].slice.call(args, 1));
    };

    // map :: (a -> b) -> [a] -> [b]
    const map = curry((f, xs) => xs.map(f));

    // show :: a -> String
    const show = x => JSON.stringify(x); //, null, 2);

    // transpose :: [[a]] -> [[a]]
    const transpose = xs =>
        xs[0].map((_, col) => xs.map(row => row[col]));

    // unlines :: [String] -> String
    const unlines = xs => xs.join('\n');


    // KRONECKER PRODUCT OF TWO MATRICES --------------------------------------

    // kprod :: [[Num]] -> [[Num]] -> [[Num]]
    const kprod = (xs, ys) =>
        concatMap(
            m => map(concat, transpose(m)),
            map(map(f(ys)), xs)
        );

    // (* n) mapped over each element in a matrix
    // f :: [[Num]] -> Num -> [[Num]]
    const f = curry((mx, n) => map(map(x => x * n), mx));

    // TEST -------------------------------------------------------------------
    return unlines(map(rows => unlines(map(show, rows)), [
        kprod([
            [1, 2],
            [3, 4]
        ], [
            [0, 5],
            [6, 7]
        ]), [], // One empty output line
        kprod([
            [0, 1, 0],
            [1, 1, 1],
            [0, 1, 0]
        ], [
            [1, 1, 1, 1],
            [1, 0, 0, 1],
            [1, 1, 1, 1]
        ])
    ]));
})();
```

{{Out}}

```txt
[0,5,0,10]
[6,7,12,14]
[0,15,0,20]
[18,21,24,28]

[0,0,0,0,1,1,1,1,0,0,0,0]
[0,0,0,0,1,0,0,1,0,0,0,0]
[0,0,0,0,1,1,1,1,0,0,0,0]
[1,1,1,1,1,1,1,1,1,1,1,1]
[1,0,0,1,1,0,0,1,1,0,0,1]
[1,1,1,1,1,1,1,1,1,1,1,1]
[0,0,0,0,1,1,1,1,0,0,0,0]
[0,0,0,0,1,0,0,1,0,0,0,0]
[0,0,0,0,1,1,1,1,0,0,0,0]
```



## jq

In this entry, matrices are JSON arrays of numeric arrays.  For the sake of illustration, the ancillary functions, though potentially independently useful, are defined here as inner functions.

```jq
def kprod(a; b):

  # element-wise multiplication of a matrix by a number, "c"
  def multiply(c): map( map(. * c) );

  # "right" should be a vector with the same length as the input
  def laminate(right):
    [range(0; right|length) as $i
    | (.[$i] + [right[$i]]) ];

  # "matrix" and the input matrix should have the same number of rows
  def addblock(matrix):
    reduce (matrix|transpose)[] as $v (.; laminate($v));

  (a[0]|length) as $m
  | reduce range(0; a|length) as $i ([];
      . + reduce range(0; $m) as $j ([];
        addblock( b | multiply(a[$i][$j]) ) ));
```


Examples:

```jq

def left:  [[ 1, 2], [3, 4]];
def right: [[ 0, 5], [6, 7]];

kprod(left;right)
```

{{out}}

```txt
[[0,5,0,10],[6,7,12,14],[0,15,0,20],[18,21,24,28]]
```



```jq

def left:  [[0, 1, 0], [1, 1, 1], [0, 1, 0]];
def right: [[1, 1, 1, 1], [1, 0, 0, 1], [1, 1, 1, 1]];

kprod(left;right)
```

{{out}}

```txt
[[0,0,0,0,1,1,1,1,0,0,0,0],
 [0,0,0,0,1,0,0,1,0,0,0,0],
 [0,0,0,0,1,1,1,1,0,0,0,0],
 [1,1,1,1,1,1,1,1,1,1,1,1],
 [1,0,0,1,1,0,0,1,1,0,0,1],
 [1,1,1,1,1,1,1,1,1,1,1,1],
 [0,0,0,0,1,1,1,1,0,0,0,0],
 [0,0,0,0,1,0,0,1,0,0,0,0],
 [0,0,0,0,1,1,1,1,0,0,0,0]]

```



## Julia


```julia
# v0.6

# Julia has a builtin kronecker product function
a = [1 2; 3 4]
b = [0 5; 6 7]
k = kron(a, b)
println("$a × $b =")
for row in 1:size(k)[1]
    println(k[row,:])
end
println()

a = [0 1 0; 1 1 1; 0 1 0]
b = [1 1 1 1; 1 0 0 1; 1 1 1 1]
k = kron(a, b)
println("$a × $b =")
for row in 1:size(k)[1]
    println(k[row,:])
end
```


{{out}}

```txt
[1 2; 3 4] × [0 5; 6 7] =
[0, 5, 0, 10]
[6, 7, 12, 14]
[0, 15, 0, 20]
[18, 21, 24, 28]

[0 1 0; 1 1 1; 0 1 0] × [1 1 1 1; 1 0 0 1; 1 1 1 1] =
[0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0]
[0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0]
[0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0]
[1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1]
[1, 0, 0, 1, 1, 0, 0, 1, 1, 0, 0, 1]
[1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1]
[0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0]
[0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0]
[0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0]
```



## Kotlin

{{trans|JavaScript (Imperative #2)}}

```scala
// version 1.1.2 (JVM)

typealias Matrix = Array<IntArray>

fun kroneckerProduct(a: Matrix, b: Matrix): Matrix {
    val m = a.size
    val n = a[0].size
    val p = b.size
    val q = b[0].size
    val rtn = m * p
    val ctn = n * q
    val r: Matrix = Array(rtn) { IntArray(ctn) } // all elements zero by default
    for (i in 0 until m)
        for (j in 0 until n)
            for (k in 0 until p)
                for (l in 0 until q)
                    r[p * i + k][q * j + l] = a[i][j] * b[k][l]
    return r
}

fun printMatrix(text: String, m: Matrix) {
    println(text)
    for (i in 0 until m.size) println(m[i].contentToString())
    println()
}

fun printAll(a: Matrix, b: Matrix, r: Matrix) {
    printMatrix("Matrix A:", a)
    printMatrix("Matrix B:", b)
    printMatrix("Kronecker product:", r)
}

fun main(args: Array<String>) {
    var a: Matrix
    var b: Matrix
    var r: Matrix
    a = arrayOf(
        intArrayOf(1, 2),
        intArrayOf(3, 4)
    )
    b = arrayOf(
        intArrayOf(0, 5),
        intArrayOf(6, 7)
    )
    r = kroneckerProduct(a, b)
    printAll(a, b, r)

    a = arrayOf(
        intArrayOf(0, 1, 0),
        intArrayOf(1, 1, 1),
        intArrayOf(0, 1, 0)
    )
    b = arrayOf(
        intArrayOf(1, 1, 1, 1),
        intArrayOf(1, 0, 0, 1),
        intArrayOf(1, 1, 1, 1)
    )
    r = kroneckerProduct(a, b)
    printAll(a, b, r)
}
```


{{out}}

```txt

Matrix A:
[1, 2]
[3, 4]

Matrix B:
[0, 5]
[6, 7]

Kronecker product:
[0, 5, 0, 10]
[6, 7, 12, 14]
[0, 15, 0, 20]
[18, 21, 24, 28]

Matrix A:
[0, 1, 0]
[1, 1, 1]
[0, 1, 0]

Matrix B:
[1, 1, 1, 1]
[1, 0, 0, 1]
[1, 1, 1, 1]

Kronecker product:
[0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0]
[0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0]
[0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0]
[1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1]
[1, 0, 0, 1, 1, 0, 0, 1, 1, 0, 0, 1]
[1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1]
[0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0]
[0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0]
[0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0]

```



## Lua


```lua

function prod( a, b )
    print( "\nPRODUCT:" )
    for m = 1, #a do
        for p = 1, #b do
            for n = 1, #a[m] do
                for q = 1, #b[p] do
                    io.write( string.format( "%3d ", a[m][n] * b[p][q] ) )
                end
            end
            print()
        end
    end
end
--[[entry point]]--
a = { { 1, 2 }, { 3, 4 } }; b = { { 0, 5 }, { 6, 7 } }
prod( a, b )
a = { { 0, 1, 0 }, { 1, 1, 1 }, { 0, 1, 0 } }
b = { { 1, 1, 1, 1 }, { 1, 0, 0, 1 }, { 1, 1, 1, 1 } }
prod( a, b )

```

{{out}}

```txt

PRODUCT:
  0   5   0  10
  6   7  12  14
  0  15   0  20
 18  21  24  28

PRODUCT:
  0   0   0   0   1   1   1   1   0   0   0   0
  0   0   0   0   1   0   0   1   0   0   0   0
  0   0   0   0   1   1   1   1   0   0   0   0
  1   1   1   1   1   1   1   1   1   1   1   1
  1   0   0   1   1   0   0   1   1   0   0   1
  1   1   1   1   1   1   1   1   1   1   1   1
  0   0   0   0   1   1   1   1   0   0   0   0
  0   0   0   0   1   0   0   1   0   0   0   0
  0   0   0   0   1   1   1   1   0   0   0   0

```



## Mathematica


```mathematica
KroneckerProduct[{{1, 2}, {3, 4}}, {{0, 5}, {6, 7}}]//MatrixForm

KroneckerProduct[{{0, 1, 0}, {1, 1, 1}, {0, 1, 0}},
 {{1, 1, 1, 1}, {1, 0, 0, 1}, {1, 1, 1, 1}}]//MatrixForm
```


{{out}}

```txt

0	5	0	10
6	7	12	14
0	15	0	20
18	21	24	28


0	0	0	0	1	1	1	1	0	0	0	0
0	0	0	0	1	0	0	1	0	0	0	0
0	0	0	0	1	1	1	1	0	0	0	0
1	1	1	1	1	1	1	1	1	1	1	1
1	0	0	1	1	0	0	1	1	0	0	1
1	1	1	1	1	1	1	1	1	1	1	1
0	0	0	0	1	1	1	1	0	0	0	0
0	0	0	0	1	0	0	1	0	0	0	0
0	0	0	0	1	1	1	1	0	0	0	0

```





## Octave


```octave>>
 kron([1 2; 3 4], [0 5; 6 7])
ans =

    0    5    0   10
    6    7   12   14
    0   15    0   20
   18   21   24   28

>> kron([0 1 0; 1 1 1; 0 1 0], [1 1 1 1; 1 0 0 1; 1 1 1 1])
ans =

   0   0   0   0   1   1   1   1   0   0   0   0
   0   0   0   0   1   0   0   1   0   0   0   0
   0   0   0   0   1   1   1   1   0   0   0   0
   1   1   1   1   1   1   1   1   1   1   1   1
   1   0   0   1   1   0   0   1   1   0   0   1
   1   1   1   1   1   1   1   1   1   1   1   1
   0   0   0   0   1   1   1   1   0   0   0   0
   0   0   0   0   1   0   0   1   0   0   0   0
   0   0   0   0   1   1   1   1   0   0   0   0
```



## PARI/GP


###  Version #1

{{Works with|PARI/GP|2.9.1 and above}}

```parigp

\\ Print title and matrix mat rows. 4/17/16 aev
matprows(title,mat)={print(title); for(i=1,#mat[,1], print(mat[i,]))}
\\
\\ Create and return the Kronecker product of the a and b matrices. 4/17/16 aev
matkronprod(a,b,pflg=0)={
my(m=#a[,1],n=#a[1,],p=#b[,1],q=#b[1,],r,rtn,ctn);
rtn=m*p; ctn=n*q;
if(pflg,print(" *** Kronecker product - a: ",m," x ",n," b: ",p," x ",q," result r: ",rtn," x ",ctn));
r=matrix(rtn,ctn);
for(i=1,m, for(j=1,n, for(k=1,p, for(l=1,q,
    r[p*(i-1)+k,q*(j-1)+l]=a[i,j]*b[k,l];
))));\\all4fend
if(pflg,print(r)); return(r);
}
{\\ Requireq tests:
my(a,b,r);
\\ Sample 1
a=[1,2;3,4];
b=[0,5;6,7];
r=matkronprod(a,b);
matprows("Sample 1 result:",r);
\\ Sample 2
a=[0,1,0;1,1,1;0,1,0];
b=[1,1,1,1;1,0,0,1;1,1,1,1];
r=matkronprod(a,b);
matprows("Sample 2 result:",r);
}

```


{{Output}}

```txt

Sample 1 result:
[0, 5, 0, 10]
[6, 7, 12, 14]
[0, 15, 0, 20]
[18, 21, 24, 28]
Sample 2 result:
[0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0]
[0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0]
[0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0]
[1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1]
[1, 0, 0, 1, 1, 0, 0, 1, 1, 0, 0, 1]
[1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1]
[0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0]
[0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0]
[0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0]

```


###  Version #2

This version is from B. Allombert. 12/12/17
{{Works with|PARI/GP|2.9.1 and above}}

```parigp

\\ Print title and matrix mat rows. aev
matprows(title,mat)={print(title); for(i=1,#mat[,1], print(mat[i,]))}
\\
\\ Create and return the Kronecker product of the a and b matrices. 12/12/17 ba
kronprod(a,b)={return(matconcat(matrix(#a[,1],#a,i,j,a[i,j]*b)))}
{\\ Requireq tests:
my(a,b,r);
\\ Sample 1
a=[1,2;3,4];
b=[0,5;6,7];
r=kronprod(a,b);
matprows("Sample 1 result:",r);
\\ Sample 2
a=[0,1,0;1,1,1;0,1,0];
b=[1,1,1,1;1,0,0,1;1,1,1,1];
r=kronprod(a,b);
matprows("Sample 2 result:",r);
}

```


{{Output}}

```txt

Sample 1 result:
[0, 5, 0, 10]
[6, 7, 12, 14]
[0, 15, 0, 20]
[18, 21, 24, 28]
Sample 2 result:
[0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0]
[0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0]
[0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0]
[1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1]
[1, 0, 0, 1, 1, 0, 0, 1, 1, 0, 0, 1]
[1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1]
[0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0]
[0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0]
[0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0]

```



## Perl


```perl
#!/usr/bin/perl
use strict;
use warnings;
use PDL;
use PDL::NiceSlice;

sub kron{
	my $A = shift;
	my $B = shift;
	my ($r0, $c0) = $A->dims;
	my ($r1, $c1) = $B->dims;
	my $kron = zeroes($r0 * $r1, $c0 * $c1);
	for(my $i = 0; $i < $r0; ++$i){
		for(my $j = 0; $j < $c0; ++$j){
			$kron(
				($i * $r1) : (($i + 1) * $r1 - 1),
				($j * $c1) : (($j + 1) * $c1 - 1)
			) .= $A($i,$j) * $B;
		}
	}
	return $kron;
}

my @mats = (
	[pdl([[1,2], [3,4]]), pdl([[0,5], [6,7]])],
	[pdl([[0,1,0], [1,1,1], [0,1,0]]), pdl([[1,1,1,1], [1,0,0,1], [1,1,1,1]])],
);
for my $mat(@mats){
	print "A = $mat->[0]\n";
	print "B = $mat->[1]\n";
	print "kron(A,B) = " . kron($mat->[0], $mat->[1]) . "\n";
}
```



## Perl 6

{{works with|rakudo|2017.01-34-g700a077}}

```perl6
sub kronecker_product ( @a, @b ) {
    return (@a X @b).map: { .[0].list X* .[1].list };
}

.say for kronecker_product([ <1 2>, <3 4> ],
                           [ <0 5>, <6 7> ]);
say '';
.say for kronecker_product([ <0 1 0>,   <1 1 1>,   <0 1 0>  ],
                           [ <1 1 1 1>, <1 0 0 1>, <1 1 1 1>]);

```

{{out}}

```txt
(0 5 0 10)
(6 7 12 14)
(0 15 0 20)
(18 21 24 28)

(0 0 0 0 1 1 1 1 0 0 0 0)
(0 0 0 0 1 0 0 1 0 0 0 0)
(0 0 0 0 1 1 1 1 0 0 0 0)
(1 1 1 1 1 1 1 1 1 1 1 1)
(1 0 0 1 1 0 0 1 1 0 0 1)
(1 1 1 1 1 1 1 1 1 1 1 1)
(0 0 0 0 1 1 1 1 0 0 0 0)
(0 0 0 0 1 0 0 1 0 0 0 0)
(0 0 0 0 1 1 1 1 0 0 0 0)
```



## Phix


```Phix
function kronecker(sequence a, b)
    integer ar = length(a),
            ac = length(a[1]),
            br = length(b),
            bc = length(b[1])
    sequence res = repeat(repeat(0,ac*bc),ar*br)
    for ia=1 to ar do
        integer i0 = (ia-1)*br
        for ja=1 to ac do
            integer j0 = (ja-1)*bc
            for ib=1 to br do
                integer i = i0+ib
                for jb=1 to bc do
                    integer j = j0+jb
                    res[i,j] = a[ia,ja]*b[ib,jb]
                end for
            end for
        end for
    end for
    return res
end function

constant a = {{1,2},
              {3,4}},
         b = {{0,5},
              {6,7}},
         c = {{0,1,0},
              {1,1,1},
              {0,1,0}},
         d = {{1,1,1,1},
              {1,0,0,1},
              {1,1,1,1}}

pp(kronecker(a,b),{pp_Nest,1,pp_IntFmt,"%2d"})
pp(kronecker(c,d),{pp_Nest,1})
```

{{out}}

```txt

{{ 0, 5, 0,10},
 { 6, 7,12,14},
 { 0,15, 0,20},
 {18,21,24,28}}
{{0,0,0,0,1,1,1,1,0,0,0,0},
 {0,0,0,0,1,0,0,1,0,0,0,0},
 {0,0,0,0,1,1,1,1,0,0,0,0},
 {1,1,1,1,1,1,1,1,1,1,1,1},
 {1,0,0,1,1,0,0,1,1,0,0,1},
 {1,1,1,1,1,1,1,1,1,1,1,1},
 {0,0,0,0,1,1,1,1,0,0,0,0},
 {0,0,0,0,1,0,0,1,0,0,0,0},
 {0,0,0,0,1,1,1,1,0,0,0,0}}

```



## PureBasic


```PureBasic
EnableExplicit
DataSection
  Matrix_A_B_Dimension_Bsp1:
  Data.i 2,2,?MatrixA_Werte_Bsp1,2,2,?MatrixB_Werte_Bsp1

  Matrix_A_B_Dimension_Bsp2:
  Data.i 3,3,?MatrixA_Werte_Bsp2,3,4,?MatrixB_Werte_Bsp2

  MatrixA_Werte_Bsp1:
  Data.i 1,2,3,4

  MatrixA_Werte_Bsp2:
  Data.i 0,1,0,1,1,1,0,1,0

  MatrixB_Werte_Bsp1:
  Data.i 0,5,6,7

  MatrixB_Werte_Bsp2:
  Data.i 1,1,1,1,1,0,0,1,1,1,1,1
EndDataSection

Define.i ma, na, mb, nb, adr1, adr2, i, j, k, l
Define mk$

Gosub Bsp1_Matrix_A_B : Gosub LoadMatrix : Gosub Bsp2_Matrix_A_B : Gosub LoadMatrix : End

LoadMatrix:
Read.i ma
Read.i na
Read.i adr1
Read.i mb
Read.i nb
Read.i adr2

Dim mxa.i(ma,na)
Dim mxb.i(mb,nb)
NewMap mxc.i()

For i=1 To ma
  For j=1 To na
    mxa(i,j)=PeekI(adr1)
    adr1+SizeOf(Integer)
  Next
Next

For i=1 To mb
  For j=1 To nb
    mxb(i,j)=PeekI(adr2)
    adr2+SizeOf(Integer)
  Next
Next

OpenConsole("Kronecker product")
PrintN("Matrix A:")
For i=1 To ma ; Zeile
  Print("|")
  For j=1 To na ; Spalte
    Print(RSet(Str(mxa(i,j)),2," ")+" ")
  Next
  PrintN("|")
Next
PrintN("")

PrintN("Matrix B:")
For i=1 To mb ; Zeile
  Print("|")
  For j=1 To nb ; Spalte
    Print(RSet(Str(mxb(i,j)),2," ")+" ")
  Next
  PrintN("|")
Next
PrintN("")

PrintN("Matrix C=AxB")
For i=1 To ma ; Zeile MA
  For j=1 To na ; Spalte MA
    For k=1 To mb ; Zeile MB
      For l=1 To nb ; Spalte MB
        mxc(Str(i)+","+Str(j)+","+Str(k)+","+Str(l))=mxa(i,j)*mxb(k,l)
      Next
    Next
  Next
Next

For i=1 To ma ; Zeile MA
  For k=1 To mb; Zeile MB
    Print("|")
    For j=1 To na ; Spalte MA
      For l=1 To nb ; Spalte MB
        mk$=Str(i)+","+Str(j)+","+Str(k)+","+Str(l)
        If FindMapElement(mxc(),mk$)
          Print(RSet(Str(mxc()),2," ")+" ")
        EndIf
      Next
    Next
    PrintN("|")
  Next
Next
PrintN("Press return") : Input()
Return

Bsp1_Matrix_A_B:
  Restore Matrix_A_B_Dimension_Bsp1
Return

Bsp2_Matrix_A_B:
  Restore Matrix_A_B_Dimension_Bsp2
Return
```

{{out}}

```txt
Matrix A:
| 1  2 |
| 3  4 |

Matrix B:
| 0  5 |
| 6  7 |

Matrix C=AxB
| 0  5  0 10 |
| 6  7 12 14 |
| 0 15  0 20 |
|18 21 24 28 |
Press return

Matrix A:
| 0  1  0 |
| 1  1  1 |
| 0  1  0 |

Matrix B:
| 1  1  1  1 |
| 1  0  0  1 |
| 1  1  1  1 |

Matrix C=AxB
| 0  0  0  0  1  1  1  1  0  0  0  0 |
| 0  0  0  0  1  0  0  1  0  0  0  0 |
| 0  0  0  0  1  1  1  1  0  0  0  0 |
| 1  1  1  1  1  1  1  1  1  1  1  1 |
| 1  0  0  1  1  0  0  1  1  0  0  1 |
| 1  1  1  1  1  1  1  1  1  1  1  1 |
| 0  0  0  0  1  1  1  1  0  0  0  0 |
| 0  0  0  0  1  0  0  1  0  0  0  0 |
| 0  0  0  0  1  1  1  1  0  0  0  0 |
Press return
```



## Python


### Version 1

In Python, the numpy library has the [https://docs.scipy.org/doc/numpy-1.13.0/reference/generated/numpy.kron.html kron] function. The following is an implementation for "bare" lists of lists.


```Python
#!/usr/bin/env python3

# Sample 1
a1 = [[1, 2], [3, 4]]
b1 = [[0, 5], [6, 7]]

# Sample 2
a2 = [[0, 1, 0], [1, 1, 1], [0, 1, 0]]
b2 = [[1, 1, 1, 1], [1, 0, 0, 1], [1, 1, 1, 1]]

def kronecker(matrix1, matrix2):
    final_list = []
    sub_list = []

    count = len(matrix2)

    for elem1 in matrix1:
        counter = 0
        check = 0
        while check < count:
            for num1 in elem1:
                for num2 in matrix2[counter]:
                    sub_list.append(num1 * num2)
            counter += 1
            final_list.append(sub_list)
            sub_list = []
            check +=1

    return final_list

# Result 1
result1 = kronecker(a1, b1)
for elem in result1:
    print(elem)

print("")

# Result 2
result2 = kronecker(a2, b2)
for elem in result2:
    print(elem)
```


Result:

```txt

[0, 5, 0, 10]
[6, 7, 12, 14]
[0, 15, 0, 20]
[18, 21, 24, 28]

[0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0]
[0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0]
[0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0]
[1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1]
[1, 0, 0, 1, 1, 0, 0, 1, 1, 0, 0, 1]
[1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1]
[0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0]
[0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0]
[0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0]

```


### Version 2

This version was initially based on and uses a similar looping structure to version 1, but it is much less readable for those not familiar with Python list comprehensions. Nevertheless I think it serves as a wonderful example of what list comprehensions can be good for. My original version of this code took an iterable as input and recursively computed the Kronecker product of any number of matrices, which is a very common use case in arenas where the Kronecker product is used. I have reduced this example to match the task description, but I encourage learners to attempt to reimplement it.

Code:

```Python
# Sample 1
r = [[1, 2], [3, 4]]
s = [[0, 5], [6, 7]]

# Sample 2
t = [[0, 1, 0], [1, 1, 1], [0, 1, 0]]
u = [[1, 1, 1, 1], [1, 0, 0, 1], [1, 1, 1, 1]]

def kronecker(matrix1, matrix2):
    return [[num1 * num2 for num1 in elem1 for num2 in matrix2[row]] for elem1 in matrix1 for row in range(len(matrix2))]

# Result 1:
for row in kronecker(r, s):
    print(row)
print()

# Result 2
for row in kronecker(t, u):
    print(row)
```



### Version 3

We can still get the power of list comprehensions (without generating the unreadably long single lines of code referred to above) by de-sugaring them down to the underlying list monad pattern.

Version three rewrites the list comprehension above in terms of '''concatMap''' (the 'bind' or 'insert' operator for list monads), to which we pass a function that returns its value wrapped in a list. (Where values are filtered out by a condition, we return an empty list).

Note, for example, that the innermost expression here has to be <code>lambda num1: [num1 * num2]</code>, rather than just <code>lambda num1: num1 * num2</code>.

The outermost part of the '''concatMap''' nest corresponds to the rightmost part of the list comprehension expression.

(Versions 2 and 3 produce the same output from the same test)

```python
from itertools import (chain)


# kronecker :: [[a]] -> [[a]] -> [[a]]
def kronecker(m1, m2):
    return concatMap(
        lambda row2: concatMap(
            lambda elem2: [concatMap(
                lambda num2: concatMap(
                    lambda num1: [num1 * num2],
                    elem2
                ),
                m1[row2]
            )],
            m2
        ),
        range(len(m2))
    )


# concatMap :: (a -> [b]) -> [a] -> [b]
def concatMap(f, xs):
    return list(
        chain.from_iterable(
            map(f, xs)
        )
    )


if __name__ == '__main__':
    # Sample 1
    r = [[1, 2], [3, 4]]
    s = [[0, 5], [6, 7]]

    # Sample 2
    t = [[0, 1, 0], [1, 1, 1], [0, 1, 0]]
    u = [[1, 1, 1, 1], [1, 0, 0, 1], [1, 1, 1, 1]]

    # Result 1:
    for row in kronecker(r, s):
        print(row)
    print()

    # Result 2
    for row in kronecker(t, u):
        print(row)
```

 {{Out}}

```txt
[0, 5, 0, 10]
[6, 7, 12, 14]
[0, 15, 0, 20]
[18, 21, 24, 28]

[0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0]
[0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0]
[0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0]
[1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1]
[1, 0, 0, 1, 1, 0, 0, 1, 1, 0, 0, 1]
[1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1]
[0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0]
[0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0]
[0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0]
```



## R

R has built-in Kronecker product operator for a and b matrices: '''a %x% b'''.

```r

## Sample using:
a <- matrix(c(1,1,1,1), ncol=2, nrow=2, byrow=TRUE);
b <- matrix(c(0,1,1,0), ncol=2, nrow=2, byrow=TRUE);
a %x% b

```

{{Output}}

```txt

     [,1] [,2] [,3] [,4]
[1,]    0    1    0    1
[2,]    1    0    1    0
[3,]    0    1    0    1
[4,]    1    0    1    0

Note: This resultant matrix could be used as initial for Checkerboard fractal.

```



## Racket


Uses typed racket, since the 'math/...' libraries are much more performant in that language.


```racket
#lang typed/racket/base

(require math/array
         math/matrix
         racket/match)

(define-type (M A) (Matrix A))

(define #:forall (A B C) (general-⊗ [m1 : (M A)] [m2 : (M B)] [× : (A B -> C)]) : (M C)
  (match-let* ((`(#(,rs1 ,cs1) . #(,rs2 ,cs2)) (cons (array-shape m1) (array-shape m2)))
               (rs (* rs1 rs2))
               (cs (* cs1 cs2)))
    (for*/matrix: rs cs ((r (in-range rs)) (c (in-range cs))) : C
      (let-values (((rq rr) (quotient/remainder r rs2))
                   ((cq cr) (quotient/remainder c cs2)))
        (× (array-ref m1 (vector rq cq)) (array-ref m2 (vector rr cr)))))))

;; Narrow to Number
(define (Kronecker-product [m1 : (M Number)] [m2 : (M Number)]) (general-⊗ m1 m2 *))

;; ---------------------------------------------------------------------------------------------------
(module+ test
  (Kronecker-product (matrix [[1 2]
                              [3 4]])
                     (matrix [[0 5]
                              [6 7]]))

  (Kronecker-product (matrix [[0 1 0]
                              [1 1 1]
                              [0 1 0]])
                     (matrix [[1 1 1 1]
                              [1 0 0 1]
                              [1 1 1 1]])))
```


{{out}}

```txt
(mutable-array #[#[0 5 0 10] #[6 7 12 14] #[0 15 0 20] #[18 21 24 28]])
(mutable-array
 #[#[0 0 0 0 1 1 1 1 0 0 0 0]
   #[0 0 0 0 1 0 0 1 0 0 0 0]
   #[0 0 0 0 1 1 1 1 0 0 0 0]
   #[1 1 1 1 1 1 1 1 1 1 1 1]
   #[1 0 0 1 1 0 0 1 1 0 0 1]
   #[1 1 1 1 1 1 1 1 1 1 1 1]
   #[0 0 0 0 1 1 1 1 0 0 0 0]
   #[0 0 0 0 1 0 0 1 0 0 0 0]
   #[0 0 0 0 1 1 1 1 0 0 0 0]])
```



## REXX

A little extra coding was added to make the matrix glyphs and element alignment look nicer.

```rexx
/*REXX program calculates the   Kronecker product   of   two arbitrary size   matrices. */
w=0                                              /*W:  max width of any matrix element. */
     aMat= 2x2  1 2 3 4                          /*define  A  matrix size  and elements.*/
     bMat= 2x2  0 5 6 7                          /*   "    B     "     "    "     "     */
call makeMat 'A', aMat                           /*construct   A   matrix from elements.*/
call makeMat 'B', bMat                           /*    "       B      "     "     "     */
call KronMat 'Kronecker product'                 /*calculate the  Kronecker  product.   */
w=0;       say;     say copies('░', 55);    say  /*display a fence between the 2 outputs*/
     aMat= 3x3  0 1 0 1 1 1 0 1 0                /*define  A  matrix size  and elements.*/
     bMat= 3x4  1 1 1 1 1 0 0 1 1 1 1 1          /*   "    B     "     "    "     "     */
call makeMat 'A', aMat                           /*construct   A   matrix from elements.*/
call makeMat 'B', bMat                           /*    "       B      "     "     "     */
call KronMat 'Kronecker product'                 /*calculate the  Kronecker  product.   */
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
KronMat: parse arg what;                  parse var  @.a.shape   aRows aCols
         #=0;                             parse var  @.b.shape   bRows bCols
               do       rA=1  for aRows
                 do     rB=1  for bRows;  #=#+1;          ##=0;        _=
                   do   cA=1  for aCols;  x=@.a.rA.cA
                     do cB=1  for bCols;  y=@.b.rB.cB;    ##=##+1;     xy=x*y;      _=_ xy
                     @.what.#.##=xy;      w=max(w, length(xy) )
                     end   /*cB*/
                   end     /*cA*/
                 end       /*rB*/
               end         /*rA*/
         call showMat what, aRows*bRows || 'X' || aRows*bCols;         return
/*──────────────────────────────────────────────────────────────────────────────────────*/
makeMat: parse arg what, size elements;   arg , row 'X' col .;       @.what.shape=row  col
         #=0;    do   r=1  for row               /* [↓]  bump item#; get item; max width*/
                   do c=1  for col;   #=#+1;   _=word(elements, #);   w=max(w, length(_) )
                   @.what.r.c=_
                   end   /*c*/                   /* [↑] define an element of WHAT matrix*/
                 end     /*r*/
         call showMat what, size;         return
/*──────────────────────────────────────────────────────────────────────────────────────*/
showMat: parse arg what, size .;  z='┌';  parse var  size  row  "X"  col;    $=left('', 6)
         say;                             say $ copies('═',7) "matrix" what  copies('═',7)
              do   r=1  for row;  _= '│'
                do c=1  for col;  _=_  right(@.what.r.c, w);  if r==1  then z=z left('',w)
                end   /*c*/
              if r==1  then do; z=z '┐';  say $ $ z;  end /*show the top part of matrix.*/
              say $ $ _ '│'
              end     /*r*/
         say $ $ translate(z, '└┘', "┌┐");   return       /*show the bot part of matrix.*/
```

{{out|output|text=  when using the default inputs:}}

```txt

       ═══════ matrix A ═══════
              ┌     ┐
              │ 1 2 │
              │ 3 4 │
              └     ┘

       ═══════ matrix B ═══════
              ┌     ┐
              │ 0 5 │
              │ 6 7 │
              └     ┘

       ═══════ matrix Kronecker product ═══════
              ┌             ┐
              │  0  5  0 10 │
              │  6  7 12 14 │
              │  0 15  0 20 │
              │ 18 21 24 28 │
              └             ┘

░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░


       ═══════ matrix A ═══════
              ┌       ┐
              │ 0 1 0 │
              │ 1 1 1 │
              │ 0 1 0 │
              └       ┘

       ═══════ matrix B ═══════
              ┌         ┐
              │ 1 1 1 1 │
              │ 1 0 0 1 │
              │ 1 1 1 1 │
              └         ┘

       ═══════ matrix Kronecker product ═══════
              ┌                         ┐
              │ 0 0 0 0 1 1 1 1 0 0 0 0 │
              │ 0 0 0 0 1 0 0 1 0 0 0 0 │
              │ 0 0 0 0 1 1 1 1 0 0 0 0 │
              │ 1 1 1 1 1 1 1 1 1 1 1 1 │
              │ 1 0 0 1 1 0 0 1 1 0 0 1 │
              │ 1 1 1 1 1 1 1 1 1 1 1 1 │
              │ 0 0 0 0 1 1 1 1 0 0 0 0 │
              │ 0 0 0 0 1 0 0 1 0 0 0 0 │
              │ 0 0 0 0 1 1 1 1 0 0 0 0 │
              └                         ┘

```



## Ring


```ring

# Project : Kronecker product

a = [[1, 2], [3, 4]]
b = [[0, 5], [6, 7]]
la1 = 1
ua1 = 2
la2 = 1
ua2 = 2
lb1 = 1
ub1 = 2
lb2 = 1
ub2 = 2
kroneckerproduct(a,b)
see nl

la1 = 1
ua1 = 3
la2 = 1
ua2 = 3
lb1 = 1
ub1 = 3
lb2 = 1
ub2 = 3
x = [[0, 1, 0], [1, 1, 1], [0, 1, 0]]
y = [[1, 1, 1, 1], [1, 0, 0, 1], [1, 1, 1, 1]]
kroneckerproduct(x, y)

func kroneckerproduct(a,b)


for i = la1 to ua1
      for k = lb1 to ub1
            see "["
            for j = la2 to ua2
                 for l = lb2 to ub2
                       see a[i][j] * b[k][l]
                       if j = ua1 and l = ub2
                          see "]" + nl
                       else
                          see " "
                       ok
                 next
            next
      next
next

```

Output:

```txt

[0 5 0 10]
[6 7 12 14]
[0 15 0 20]
[18 21 24 28]

[0 0 0 1 1 1 0 0 0]
[0 0 0 1 0 0 0 0 0]
[0 0 0 1 1 1 0 0 0]
[1 1 1 1 1 1 1 1 1]
[1 0 0 1 0 0 1 0 0]
[1 1 1 1 1 1 1 1 1]
[0 0 0 1 1 1 0 0 0]
[0 0 0 1 0 0 0 0 0]
[0 0 0 1 1 1 0 0 0]

```



## Sidef

{{trans|Perl 6}}

```ruby
func kronecker_product(a, b) {
    a ~X b -> map { _[0] ~X* _[1] }
}

kronecker_product([[1, 2], [3, 4]],
                  [[0, 5], [6, 7]]).each { .say }

say ''
kronecker_product([[0,1,0],  [1,1,1],   [0,1,0]],
                  [[1,1,1,1],[1,0,0,1], [1,1,1,1]]).each { .say }
```

{{out}}

```txt

[0, 5, 0, 10]
[6, 7, 12, 14]
[0, 15, 0, 20]
[18, 21, 24, 28]

[0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0]
[0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0]
[0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0]
[1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1]
[1, 0, 0, 1, 1, 0, 0, 1, 1, 0, 0, 1]
[1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1]
[0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0]
[0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0]
[0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0]

```


## Simula


```simula
BEGIN

    PROCEDURE OUTMATRIX(A, W); INTEGER ARRAY A; INTEGER W;
    BEGIN
        INTEGER I, J;
        INTEGER LA1, UA1;
        INTEGER LA2, UA2;

        LA1 := LOWERBOUND(A, 1); UA1 := UPPERBOUND(A, 1);
        LA2 := LOWERBOUND(A, 2); UA2 := UPPERBOUND(A, 2);

        FOR I := LA1 STEP 1 UNTIL UA1 DO
        BEGIN
            OUTTEXT("[");
            FOR J := LA2 STEP 1 UNTIL UA2 DO
            BEGIN
                IF NOT (J = LA2) THEN OUTCHAR(' ');
                OUTINT(A(I, J), W)
            END;
            OUTTEXT("]");
            OUTIMAGE
        END
    END OUTMATRIX;

    PROCEDURE KRONECKERPRODUCT(A, B, C); INTEGER ARRAY A, B, C;
    BEGIN
        INTEGER I, J, K, L, CI, CJ;
        INTEGER LA1, UA1;
        INTEGER LA2, UA2;
        INTEGER LB1, UB1;
        INTEGER LB2, UB2;

        LA1 := LOWERBOUND(A, 1); UA1 := UPPERBOUND(A, 1);
        LA2 := LOWERBOUND(A, 2); UA2 := UPPERBOUND(A, 2);
        LB1 := LOWERBOUND(B, 1); UB1 := UPPERBOUND(B, 1);
        LB2 := LOWERBOUND(B, 2); UB2 := UPPERBOUND(B, 2);

        CI := 1;
        FOR I := LA1 STEP 1 UNTIL UA1 DO
            FOR K := LB1 STEP 1 UNTIL UB1 DO
            BEGIN
                CJ := 1;
                FOR J := LA2 STEP 1 UNTIL UA2 DO
                    FOR L := LB2 STEP 1 UNTIL UB2 DO
                    BEGIN
                        C(CI, CJ) := A(I, J) * B(K, L);
                        CJ := CJ + 1
                    END;
                CI := CI + 1
            END
    END KRONECKERPRODUCT;

    ! --- EXAMPLE 1 --- ;
    BEGIN
        INTEGER ARRAY A(1:2, 1:2);
        INTEGER ARRAY B(1:2, 1:2);
        INTEGER ARRAY C(1:4, 1:4);


        ! {{1, 2}, {3, 4}} ;

        A(1, 1) := 1;
        A(1, 2) := 2;

        A(2, 1) := 3;
        A(2, 2) := 4;

        ! {{0, 5}, {6, 7}} ;

        B(1, 1) := 0;
        B(1, 2) := 5;

        B(2, 1) := 6;
        B(2, 2) := 7;

        OUTMATRIX(A, 2); OUTTEXT("   *"); OUTIMAGE;
        OUTMATRIX(B, 2); OUTTEXT("   ="); OUTIMAGE;

        KRONECKERPRODUCT(A, B, C);

        OUTMATRIX(C, 2); OUTIMAGE

        ! OUTPUT:

        ! [ 0  5  0 10]
        ! [ 6  7 12 14]
        ! [ 0 15  0 20]
        ! [18 21 24 28] ;

    END EXAMPLE 1;

    ! --- EXAMPLE 2 --- ;
    BEGIN
        INTEGER ARRAY X(1:3, 1:3);
        INTEGER ARRAY Y(1:3, 1:4);
        INTEGER ARRAY C(1:9, 1:12);

        ! {{0, 1, 0}, {1, 1, 1}, {0, 1, 0}} ;

        X(1,1) := 0;
        X(1,2) := 1;
        X(1,3) := 0;

        X(2,1) := 1;
        X(2,2) := 1;
        X(2,3) := 1;

        X(3,1) := 0;
        X(3,2) := 1;
        X(3,3) := 0;

       ! {{1, 1, 1, 1}, {1, 0, 0, 1}, {1, 1, 1, 1}} ;

        Y(1,1) := 1;
        Y(1,2) := 1;
        Y(1,3) := 1;
        Y(1,4) := 1;

        Y(2,1) := 1;
        Y(2,2) := 0;
        Y(2,3) := 0;
        Y(2,4) := 1;

        Y(3,1) := 1;
        Y(3,2) := 1;
        Y(3,3) := 1;
        Y(3,4) := 1;

        OUTIMAGE;

        OUTMATRIX(X, 1); OUTTEXT("   *"); OUTIMAGE;
        OUTMATRIX(Y, 1); OUTTEXT("   ="); OUTIMAGE;

        KRONECKERPRODUCT(X, Y, C);

        OUTMATRIX(C, 1); OUTIMAGE;

        ! OUTPUT:

        ! [0 0 0 0 1 1 1 1 0 0 0 0]
        ! [0 0 0 0 1 0 0 1 0 0 0 0]
        ! [0 0 0 0 1 1 1 1 0 0 0 0]
        ! [1 1 1 1 1 1 1 1 1 1 1 1]
        ! [1 0 0 1 1 0 0 1 1 0 0 1]
        ! [1 1 1 1 1 1 1 1 1 1 1 1]
        ! [0 0 0 0 1 1 1 1 0 0 0 0]
        ! [0 0 0 0 1 0 0 1 0 0 0 0]
        ! [0 0 0 0 1 1 1 1 0 0 0 0] ;

    END EXAMPLE 2;
END
```

{{out}}

```txt

[ 1  2]
[ 3  4]
   *
[ 0  5]
[ 6  7]
   =
[ 0  5  0 10]
[ 6  7 12 14]
[ 0 15  0 20]
[18 21 24 28]


[0 1 0]
[1 1 1]
[0 1 0]
   *
[1 1 1 1]
[1 0 0 1]
[1 1 1 1]
   =
[0 0 0 0 1 1 1 1 0 0 0 0]
[0 0 0 0 1 0 0 1 0 0 0 0]
[0 0 0 0 1 1 1 1 0 0 0 0]
[1 1 1 1 1 1 1 1 1 1 1 1]
[1 0 0 1 1 0 0 1 1 0 0 1]
[1 1 1 1 1 1 1 1 1 1 1 1]
[0 0 0 0 1 1 1 1 0 0 0 0]
[0 0 0 0 1 0 0 1 0 0 0 0]
[0 0 0 0 1 1 1 1 0 0 0 0]


```



## Stata

In Mata, the Kronecker product is the operator '''#'''.


```stata
. mata
------------------------------------------------- mata (type end to exit) ----------
: a=1,2\3,4

: b=0,5\6,7

: a#b
        1    2    3    4
    +---------------------+
  1 |   0    5    0   10  |
  2 |   6    7   12   14  |
  3 |   0   15    0   20  |
  4 |  18   21   24   28  |
    +---------------------+

: a=0,1,0\1,1,1\0,1,0

: b=1,1,1,1\1,0,0,1\1,1,1,1

: a#b
        1    2    3    4    5    6    7    8    9   10   11   12
    +-------------------------------------------------------------+
  1 |   0    0    0    0    1    1    1    1    0    0    0    0  |
  2 |   0    0    0    0    1    0    0    1    0    0    0    0  |
  3 |   0    0    0    0    1    1    1    1    0    0    0    0  |
  4 |   1    1    1    1    1    1    1    1    1    1    1    1  |
  5 |   1    0    0    1    1    0    0    1    1    0    0    1  |
  6 |   1    1    1    1    1    1    1    1    1    1    1    1  |
  7 |   0    0    0    0    1    1    1    1    0    0    0    0  |
  8 |   0    0    0    0    1    0    0    1    0    0    0    0  |
  9 |   0    0    0    0    1    1    1    1    0    0    0    0  |
    +-------------------------------------------------------------+
: end
```



## SuperCollider


```SuperCollider
// the iterative version is derived from the javascript one here:
(
f = { |a, b|
	var m = a.size;
	var n = a[0].size;
	var p = b.size;
	var q = b[0].size;
	var rtn = m * p;
	var ctn = n * q;
	var res = { 0.dup(ctn) }.dup(rtn);
	m.do { |i|
		n.do { |j|
			p.do { |k|
				q.do { |l|
					res[p*i+k][q*j+l] = a[i][j] * b[k][l];
				}
			}
		}
	};
	res
};
)

// Like APL/J, SuperCollider has applicative operators, so here is a shorter version.
// the idea is to first replace every element of b with its product with all of a
// and then reshape the matrix appropriately
// note that +++ is lamination: [[1, 2, 3], [4, 5, 6]] +++ [100, 200] returns [ [ 1, 2, 3, 100 ], [ 4, 5, 6, 200 ] ].

(
f = { |a, b|
	a.collect { |x|
		x.collect { |y| b * y }.reduce('+++')
	}.reduce('++')
}
)

// or shorter:
(a *.2 b).collect(_.reduce('+++')).reduce('++')


```



```SuperCollider
// to apply either of the two functions:
(
x = f.(
	[
		[0, 1, 0],
		[1, 1, 1],
		[0, 1, 0]
	],
	[
		[1, 1, 1, 1],
		[1, 0, 0, 1],
		[1, 1, 1, 1]
	]
)
)

```


Results in:


```txt

[
	[ 0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0 ],
	[ 0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0 ],
	[ 0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0 ],
	[ 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 ],
	[ 1, 0, 0, 1, 1, 0, 0, 1, 1, 0, 0, 1 ],
	[ 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 ],
	[ 0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0 ],
	[ 0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0 ],
	[ 0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0 ]
]

```


And:


```SuperCollider
(
x = f.(
	[
		[ 1, 2 ],
		[ 3, 4 ]
	],
	[
		[ 0, 5 ],
		[ 6, 7 ]
	]
)
)

```


returns:

```txt

[
	[ 0, 5, 0, 10 ],
	[ 6, 7, 12, 14 ],
	[ 0, 15, 0, 20 ],
	[ 18, 21, 24, 28 ]
]

```



## Tcl


```Tcl
# some helpers for matrices in nice string form:
proc parse_matrix {s} {
    split [string trim $s] \n
}

proc print_matrix {m} {
    foreach row $m {
        puts [join [lmap x $row {format %3s $x}]]
    }
}

# obvious imperative version using [foreach]
proc kroenecker {A B} {
    foreach arow $A {
        foreach brow $B {
            set row {}
            foreach a $arow {
                foreach b $brow {
                    lappend row [expr {$a * $b}]
                }
            }
            lappend result $row
        }
    }
    return $result
}

proc lolcat {args} {    ;# see https://wiki.tcl.tk/41507
    concat {*}[uplevel 1 lmap $args]
}

# more compact but obtuse, using [lmap] and [lolcat]
proc kroenecker {A B} {
    lolcat arow $A {
        lmap brow $B {
            lolcat a $arow {
                lmap b $brow {
                    expr {$a * $b}
                }
            }
        }
    }
}

# demo:
set inputs {
    {1 2
     3 4}
    {0 5
     6 7}

    {0 1 0
     1 1 1
     0 1 0}
    {1 1 1 1
     1 0 0 1
     1 1 1 1}
}

foreach {a b} $inputs {
    set a [parse_matrix $a]
    set b [parse_matrix $b]
    print_matrix [kroenecker $a $b]
    puts ""
}
```


{{out}}

```txt
  0   5   0  10
  6   7  12  14
  0  15   0  20
 18  21  24  28

  0   0   0   0   1   1   1   1   0   0   0   0
  0   0   0   0   1   0   0   1   0   0   0   0
  0   0   0   0   1   1   1   1   0   0   0   0
  1   1   1   1   1   1   1   1   1   1   1   1
  1   0   0   1   1   0   0   1   1   0   0   1
  1   1   1   1   1   1   1   1   1   1   1   1
  0   0   0   0   1   1   1   1   0   0   0   0
  0   0   0   0   1   0   0   1   0   0   0   0
  0   0   0   0   1   1   1   1   0   0   0   0

```



## VBScript


```vb
' Kronecker product - 05/04/2017
dim a(),b(),r()

sub kroneckerproduct '(a,b)
    m=ubound(a,1): n=ubound(a,2)
    p=ubound(b,1): q=ubound(b,2)
    rtn=m*p
    ctn=n*q
    redim r(rtn,ctn)
    for i=1 to m
        for j=1 to n
            for k=1 to p
                for l=1 to q
                    r(p*(i-1)+k,q*(j-1)+l)=a(i,j)*b(k,l)
    next: next: next: next
end sub 'kroneckerproduct

sub printmatrix(text,m,w)
    wscript.stdout.writeline text
    select case m
        case "a": ni=ubound(a,1): nj=ubound(a,2)
        case "b": ni=ubound(b,1): nj=ubound(b,2)
        case "r": ni=ubound(r,1): nj=ubound(r,2)
    end select
    for i=1 to ni
        for j=1 to nj
            select case m
                case "a": k=a(i,j)
                case "b": k=b(i,j)
                case "r": k=r(i,j)
            end select
            wscript.stdout.write right(space(w)&k,w)
        next
        wscript.stdout.writeline
    next
end sub 'printmatrix

sub printall(w)
    printmatrix "matrix a:", "a", w
    printmatrix "matrix b:", "b", w
    printmatrix "kronecker product:", "r", w
end sub 'printall

sub main()
    xa=array( 1, 2, _
              3, 4)
    redim a(2,2)
    k=0: for i=1 to ubound(a,1): for j=1 to ubound(a,1)
        a(i,j)=xa(k): k=k+1
    next:next
    xb=array( 0, 5, _
              6, 7)
    redim b(2,2)
    k=0: for i=1 to ubound(b,1): for j=1 to ubound(b,1)
        b(i,j)=xb(k): k=k+1
    next:next
    kroneckerproduct
    printall 3

    xa=array( 0, 1, 0, _
              1, 1, 1, _
              0, 1, 0)
    redim a(3,3)
    k=0: for i=1 to ubound(a,1): for j=1 to ubound(a,1)
        a(i,j)=xa(k): k=k+1
    next:next
    xb=array( 1, 1, 1, 1, _
              1, 0, 0, 1, _
              1, 1, 1, 1)
    redim b(3,4)
    k=0: for i=1 to ubound(b,1): for j=1 to ubound(b,1)
        b(i,j)=xb(k): k=k+1
    next:next
    kroneckerproduct
    printall 2
end sub 'main

main
```

{{out}}

```txt

matrix a:
  1  2
  3  4
matrix b:
  0  5
  6  7
kronecker product:
  0  5  0 10
  6  7 12 14
  0 15  0 20
 18 21 24 28
matrix a:
 0 1 0
 1 1 1
 0 1 0
matrix b:
 1 1 1
 1 1 0
 0 1 1
kronecker product:
 0 0 0 0 1 1 1 0 0 0 0 0
 0 0 0 0 1 1 0 0 0 0 0 0
 0 0 0 0 0 1 1 0 0 0 0 0
 1 1 1 0 1 1 1 0 1 1 1 0
 1 1 0 0 1 1 0 0 1 1 0 0
 0 1 1 0 0 1 1 0 0 1 1 0
 0 0 0 0 1 1 1 0 0 0 0 0
 0 0 0 0 1 1 0 0 0 0 0 0
 0 0 0 0 0 1 1 0 0 0 0 0

```



## zkl


```zkl
var [const] GSL=Import.lib("zklGSL");    // libGSL (GNU Scientific Library)
fcn kronecker(A,B){
   m,n, p,q := A.rows,A.cols, B.rows,B.cols;
   r:=GSL.Matrix(m*p, n*q);
   foreach i,j,k,l in (m,n,p,q){ r[p*i + k, q*j + l]=A[i,j]*B[k,l] }
   r
}
```


```zkl
A:=GSL.Matrix(2,2).set(1,2, 3,4);
B:=GSL.Matrix(2,2).set(0,5, 6,7);
kronecker(A,B).format(3,0).println();	// format(width,precision)

A:=GSL.Matrix(3,3).set(0,1,0,
		       1,1,1,
		       0,1,0);
B:=GSL.Matrix(3,4).set(1,1,1,1,
		       1,0,0,1,
		       1,1,1,1);
kronecker(A,B).format(2,0).println();
```

{{out}}

```txt

  0,  5,  0, 10
  6,  7, 12, 14
  0, 15,  0, 20
 18, 21, 24, 28
 0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0
 0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0
 0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0
 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1
 1, 0, 0, 1, 1, 0, 0, 1, 1, 0, 0, 1
 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1
 0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0
 0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0
 0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0

```

