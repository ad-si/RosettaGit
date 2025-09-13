+++
title = "Proper divisors"
description = ""
date = 2019-10-11T23:33:51Z
aliases = []
[extra]
id = 18388
[taxonomies]
categories = ["task"]
tags = []
+++

The   [http://planetmath.org/properdivisor proper divisors]   of a positive integer '''N''' are those numbers, other than '''N''' itself, that divide '''N''' without remainder.

For '''N''' > 1 they will always include 1,   but for '''N''' == 1 there are no proper divisors.


;Examples:
The proper divisors of     6     are   1, 2, and 3.

The proper divisors of   100   are   1, 2, 4, 5, 10, 20, 25, and 50.


## Task

# Create a routine to generate all the proper divisors of a number.
# use it to show the proper divisors of the numbers 1 to 10 inclusive.
# Find a number in the range 1 to 20,000 with the most proper divisors. Show the number and just the count of how many proper divisors it has.



Show all output here.


## Related tasks

*   [[Amicable pairs]]
*   [[Abundant, deficient and perfect number classifications]]
*   [[Aliquot sequence classifications]]
*   [[Factors of an integer]]
*   [[Prime decomposition]]





## 360 Assembly

This program uses two ASSIST macros (XDECO, XPRNT) to keep the code as short as possible.

```360asm
*        Proper divisors           14/06/2016
PROPDIV  CSECT
         USING  PROPDIV,R13        base register
         B      72(R15)            skip savearea
         DC     17F'0'             savearea
         STM    R14,R12,12(R13)    prolog
         ST     R13,4(R15)         "
         ST     R15,8(R13)         "
         LR     R13,R15            "
         LA     R10,1              n=1
LOOPN1   C      R10,=F'10'         do n=1 to 10
         BH     ELOOPN1
         LR     R1,R10             n
         BAL    R14,PDIV           pdiv(n)
         ST     R0,NN              nn=pdiv(n)
         MVC    PG,PGT             init buffer
         LA     R11,PG             pgi=0
         XDECO  R10,XDEC           edit n
         MVC    0(3,R11),XDEC+9    output n
         LA     R11,7(R11)         pgi=pgi+7
         L      R1,NN              nn
         XDECO  R1,XDEC            edit nn
         MVC    0(3,R11),XDEC+9    output nn
         LA     R11,20(R11)        pgi=pgi+20
         LA     R5,1               i=1
LOOPNI   C      R5,NN              do i=1 to nn
         BH     ELOOPNI
         LR     R1,R5              i
         SLA    R1,2               *4
         L      R2,TDIV-4(R1)      tdiv(i)
         XDECO  R2,XDEC            edit tdiv(i)
         MVC    0(3,R11),XDEC+9    output tdiv(i)
         LA     R11,3(R11)         pgi=pgi+3
         LA     R5,1(R5)           i=i+1
         B      LOOPNI
ELOOPNI  XPRNT  PG,80              print buffer
         LA     R10,1(R10)         n=n+1
         B      LOOPN1
ELOOPN1  SR     R0,R0              0
         ST     R0,M               m=0
         LA     R10,1              n=1
LOOPN2   C      R10,=F'20000'      do n=1 to 20000
         BH     ELOOPN2
         LR     R1,R10             n
         BAL    R14,PDIV           nn=pdiv(n)
         C      R0,M               if nn>m
         BNH    NNNHM
         ST     R10,II             ii=n
         ST     R0,M               m=nn
NNNHM    LA     R10,1(R10)         n=n+1
         B      LOOPN2
ELOOPN2  MVC    PG,PGR             init buffer
         L      R1,II              ii
         XDECO  R1,XDEC            edit ii
         MVC    PG(5),XDEC+7       output ii
         L      R1,M               m
         XDECO  R1,XDEC            edit m
         MVC    PG+9(4),XDEC+8     output m
         XPRNT  PG,80              print buffer
         L      R13,4(0,R13)       epilog
         LM     R14,R12,12(R13)    "
         XR     R15,R15            "
         BR     R14                exit
*------- pdiv   --function(x)----->number of divisors---
PDIV     ST     R1,X               x
         C      R1,=F'1'           if x=1
         BNE    NOTONE
         LA     R0,0               return(0)
         BR     R14
NOTONE   LR     R4,R1              x
         N      R4,=X'00000001'    mod(x,2)
         LA     R4,1(R4)           +1
         ST     R4,ODD             odd=mod(x,2)+1
         LA     R8,1               ia=1
         LA     R0,1               1
         ST     R0,TDIV            tdiv(1)=1
         SR     R9,R9              ib=0
         L      R7,ODD             odd
         LA     R7,1(R7)           j=odd+1
LOOPJ    LR     R5,R7              do j=odd+1 by odd
         MR     R4,R7              j*j
         C      R5,X               while j*j<x
         BNL    ELOOPJ
         L      R4,X               x
         SRDA   R4,32              .
         DR     R4,R7              /j
         LTR    R4,R4              if mod(x,j)=0
         BNZ    ITERJ
         LA     R8,1(R8)           ia=ia+1
         LR     R1,R8              ia
         SLA    R1,2               *4 (F)
         ST     R7,TDIV-4(R1)      tdiv(ia)=j
         LA     R9,1(R9)           ib=ib+1
         L      R4,X               x
         SRDA   R4,32              .
         DR     R4,R7              j
         LR     R2,R9              ib
         SLA    R2,2               *4 (F)
         ST     R5,TDIVB-4(R2)     tdivb(ib)=x/j
ITERJ    A      R7,ODD             j=j+odd
         B      LOOPJ
ELOOPJ   LR     R5,R7              j
         MR     R4,R7              j*j
         C      R5,X               if j*j=x
         BNE    JTJNEX
         LA     R8,1(R8)           ia=ia+1
         LR     R1,R8              ia
         SLA    R1,2               *4 (F)
         ST     R7,TDIV-4(R1)      tdiv(ia)=j
JTJNEX   LA     R1,TDIV(R1)        @tdiv(ia+1)
         LA     R2,TDIVB-4(R2)     @tdivb(ib)
         LTR    R6,R9              do i=ib to 1 by -1
         BZ     ELOOPI
LOOPI    MVC    0(4,R1),0(R2)      tdiv(ia)=tdivb(i)
         LA     R8,1(R8)           ia=ia+1
         LA     R1,4(R1)           r1+=4
         SH     R2,=H'4'           r2-=4
         BCT    R6,LOOPI           i=i-1
ELOOPI   LR     R0,R8              return(ia)
         BR     R14                return to caller
*        ----   ----------------------------------------
TDIV     DS     80F
TDIVB    DS     40F
M        DS     F
NN       DS     F
II       DS     F
X        DS     F
ODD      DS     F
PGT      DC     CL80'... has .. proper divisors:'
PGR      DC     CL80'..... has ... proper divisors.'
PG       DC     CL80' '
XDEC     DS     CL12
         YREGS
         END    PROPDIV
```

```txt

  1 has  0 proper divisors:
  2 has  1 proper divisors:  1
  3 has  1 proper divisors:  1
  4 has  2 proper divisors:  1  2
  5 has  1 proper divisors:  1
  6 has  3 proper divisors:  1  2  3
  7 has  1 proper divisors:  1
  8 has  3 proper divisors:  1  2  4
  9 has  2 proper divisors:  1  3
 10 has  3 proper divisors:  1  2  5
15120 has  79 proper divisors.

```



## Ada

The first part of the task is to ''create a routine to generate a list of the proper divisors''. To ease the re-use of this routine for other tasks, such as ''Abundant, Deficient and Perfect Number Classification''
[[http://rosettacode.org/wiki/Abundant,_deficient_and_perfect_number_classifications#Ada]],
''Abundant Odd Number''
[[http://rosettacode.org/wiki/Abundant_odd_numbers#Ada]],
and ''Amicable Pairs''
[[http://rosettacode.org/wiki/Amicable_pairs#Ada]], we define this routine as a function of a generic package:


```Ada
generic
   type Result_Type (<>) is limited private;
   None: Result_Type;
   with function One(X: Positive) return Result_Type;
   with function Add(X, Y: Result_Type) return Result_Type
      is <>;
package Generic_Divisors is

  function Process
    (N: Positive; First: Positive := 1) return Result_Type is
      (if First**2 > N or First = N then None
      elsif (N mod First)=0 then
	(if First = 1 or First*First = N
	   then Add(One(First), Process(N, First+1))
	   else Add(One(First),
		    Add(One((N/First)), Process(N, First+1))))
      else Process(N, First+1));

end Generic_Divisors;
```


Now we instantiate the ''generic package'' to solve the other two parts of the task. Observe that there are two different instantiations of the package: one to generate a list of proper divisors, another one to count the number of proper divisors without actually generating such a list:


```Ada
with Ada.Text_IO, Ada.Containers.Generic_Array_Sort, Generic_Divisors;

procedure Proper_Divisors is

begin
   -- show the proper divisors of the numbers 1 to 10 inclusive.
   declare
      type Pos_Arr is array(Positive range <>) of Positive;
      subtype Single_Pos_Arr is Pos_Arr(1 .. 1);
      Empty: Pos_Arr(1 .. 0);

      function Arr(P: Positive) return Single_Pos_Arr is ((others => P));

      package Divisor_List is new Generic_Divisors
	(Result_Type => Pos_Arr, None => Empty, One => Arr, Add =>  "&");

      procedure Sort is new Ada.Containers.Generic_Array_Sort
	(Positive, Positive, Pos_Arr);
   begin
      for I in 1 .. 10 loop
	 declare
	    List: Pos_Arr := Divisor_List.Process(I);
	 begin
	    Ada.Text_IO.Put
	      (Positive'Image(I) & " has" &
		 Natural'Image(List'Length) & " proper divisors:");
	    Sort(List);
	    for Item of List loop
	       Ada.Text_IO.Put(Positive'Image(Item));
	    end loop;
	    Ada.Text_IO.New_Line;
	 end;
      end loop;
   end;

   -- find a number 1 .. 20,000 with the most proper divisors
   declare
      Number: Positive := 1;
      Number_Count: Natural := 0;
      Current_Count: Natural;

      function Cnt(P: Positive) return Positive is (1);

      package Divisor_Count is new Generic_Divisors
	(Result_Type => Natural, None => 0, One => Cnt, Add =>  "+");

   begin
      for Current in 1 .. 20_000 loop
	 Current_Count := Divisor_Count.Process(Current);
	 if Current_Count > Number_Count then
	    Number := Current;
	    Number_Count := Current_Count;
	 end if;
      end loop;
      Ada.Text_IO.Put_Line
	(Positive'Image(Number) & " has the maximum number of" &
	   Natural'Image(Number_Count) & " proper divisors.");
   end;
end Proper_Divisors;
```


```txt
 1 has 0 proper divisors:
 2 has 1 proper divisors: 1
 3 has 1 proper divisors: 1
 4 has 2 proper divisors: 1 2
 5 has 1 proper divisors: 1
 6 has 3 proper divisors: 1 2 3
 7 has 1 proper divisors: 1
 8 has 3 proper divisors: 1 2 4
 9 has 2 proper divisors: 1 3
 10 has 3 proper divisors: 1 2 5

 15120 has the maximum number of 79 proper divisors.
```



## ALGOL 68

```algol68
# MODE to hold an element of a list of proper divisors            #
MODE DIVISORLIST = STRUCT( INT divisor, REF DIVISORLIST next );

# end of divisor list value                                       #
REF DIVISORLIST nil divisor list = REF DIVISORLIST(NIL);

# resturns a DIVISORLIST containing the proper divisors of n      #
# if n = 1, 0 or -1, we return no divisors                        #
PROC proper divisors = ( INT n )REF DIVISORLIST:
     BEGIN
         REF DIVISORLIST result   := nil divisor list;
         REF DIVISORLIST end list := result;
         INT abs n  = ABS n;
         IF abs n > 1 THEN
             # build the list of divisors backeards, so they are  #
             # returned in ascending order                        #
             INT root n = ENTIER sqrt( abs n );
             FOR d FROM root n BY -1 TO 2 DO
                 IF abs n MOD d = 0 THEN
                     # found another divisor                      #
                     result := HEAP DIVISORLIST
                            := DIVISORLIST( d, result );
                     IF end list IS nil divisor list THEN
                         # first result                           #
                         end list := result
                     FI;
                     IF d * d /= n THEN
                         # add the other divisor to the end of    #
                         # the list                               #
                         next OF end list := HEAP DIVISORLIST
                                          := DIVISORLIST( abs n OVER d, nil divisor list );
                         end list         := next OF end list
                     FI
                 FI
             OD;
             # 1 is always a proper divisor of numbers > 1        #
             result := HEAP DIVISORLIST
                    := DIVISORLIST( 1, result )
         FI;
         result
     END # proper divisors # ;

# returns the number of divisors in a DIVISORLIST                 #
PROC count divisors = ( REF DIVISORLIST list )INT:
     BEGIN
        INT result := 0;
        REF DIVISORLIST divisors := list;
        WHILE divisors ISNT nil divisor list DO
            result +:= 1;
            divisors := next OF divisors
        OD;
        result
     END # count divisors # ;

# find the proper divisors of 1 : 10                              #
FOR n TO 10 DO
    REF DIVISORLIST divisors := proper divisors( n );
    print( ( "Proper divisors of: ", whole( n, -2 ), ": " ) );
    WHILE divisors ISNT nil divisor list DO
        print( ( " ", whole( divisor OF divisors, 0 ) ) );
        divisors := next OF divisors
    OD;
    print( ( newline ) )
OD;

# find the first/only number in 1 : 20 000 with the most divisors  #
INT max number         = 20 000;
INT max divisors      :=      0;
INT has max divisors  :=      0;
INT with max divisors :=      0;
FOR d TO max number DO
    INT divisor count = count divisors( proper divisors( d ) );
    IF divisor count > max divisors THEN
        # found a number with more divisors than the previous max  #
        max divisors       := divisor count;
        has max divisors   := d;
        with max divisors  := 1
    ELIF divisor count = max divisors THEN
        # found another number with that many divisors             #
        with max divisors +:= 1
    FI
OD;
print( ( whole( has max divisors, 0 )
       , " is the "
       , IF with max divisors < 2 THEN "only" ELSE "first" FI
       , " number upto "
       , whole( max number, 0 )
       , " with "
       , whole( max divisors, 0 )
       , " divisors"
       , newline
       ) )
```

```txt

Proper divisors of:  1:
Proper divisors of:  2:  1
Proper divisors of:  3:  1
Proper divisors of:  4:  1 2
Proper divisors of:  5:  1
Proper divisors of:  6:  1 2 3
Proper divisors of:  7:  1
Proper divisors of:  8:  1 2 4
Proper divisors of:  9:  1 3
Proper divisors of: 10:  1 2 5
15120 is the first number upto 20000 with 79 divisors

```


=={{header|Algol-M}}==
Algol-M's maximum allowed integer value of 16,383 prevented searching up to 20,000 for the number with the most divisors, so the code here searches only up to 10,000.

```algol

BEGIN

% COMPUTE P MOD Q %
INTEGER FUNCTION MOD (P, Q);
INTEGER P, Q;
BEGIN
    MOD := P - Q * (P / Q);
END;

% COUNT, AND OPTIONALLY DISPLAY, PROPER DIVISORS OF N %
INTEGER FUNCTION DIVISORS(N, DISPLAY);
INTEGER N, DISPLAY;
BEGIN
    INTEGER I, LIMIT, COUNT, START, DELTA;
    IF MOD(N, 2) = 0 THEN
      BEGIN
        START := 2;
        DELTA := 1;
      END
    ELSE  % ONLY NEED TO CHECK ODD DIVISORS %
      BEGIN
        START := 3;
        DELTA := 2;
      END;
    % 1 IS A DIVISOR OF ANY NUMBER > 1 %
    IF N > 1 THEN COUNT := 1 ELSE COUNT := 0;
    IF (DISPLAY <> 0) AND (COUNT <> 0) THEN WRITEON(1);
    % CHECK REMAINING POTENTIAL DIVISORS %
    I := START;
    LIMIT := N / START;
    WHILE I <= LIMIT DO
      BEGIN
        IF MOD(N, I) = 0 THEN
          BEGIN
            IF DISPLAY <> 0 THEN WRITEON(I);
            COUNT := COUNT + 1;
          END;
        I := I + DELTA;
        IF COUNT = 1 THEN LIMIT := N / I;
      END;
   DIVISORS := COUNT;
END;

COMMENT MAIN PROGRAM BEGINS HERE;
INTEGER I, NDIV, TRUE, FALSE, HIGHDIV, HIGHNUM;
TRUE := -1;
FALSE := 0;

WRITE("PROPER DIVISORS OF FIRST TEN NUMBERS:");
FOR I := 1 STEP 1 UNTIL 10 DO
  BEGIN
     WRITE(I, " : ");
     NDIV := DIVISORS(I, TRUE);
  END;

WRITE("SEARCHING FOR NUMBER UP TO 10000 WITH MOST DIVISORS ...");
HIGHDIV := 1;
HIGHNUM := 1;
FOR I := 1 STEP 1 UNTIL 10000 DO
  BEGIN
     NDIV := DIVISORS(I, FALSE);
     IF NDIV > HIGHDIV THEN
       BEGIN
         HIGHDIV := NDIV;
         HIGHNUM := I;
       END;
  END;
WRITE("THE NUMBER IS:", HIGHNUM);
WRITE("IT HAS", HIGHDIV, " DIVISORS");

END

```

```txt

PROPER DIVISORS OF FIRST TEN NUMBERS:
     1 :
     2 :      1
     3 :      1
     4 :      1     2
     5 :      1
     6 :      1     2     3
     7 :      1
     8 :      1     2     4
     9 :      1     3
    10 :      1     2     5
SEARCHING FOR NUMBER UP TO 10000 WITH MOST DIVISORS:
THE NUMBER IS:  7560
IT HAS    63 DIVISORS

```



## AppleScript

```AppleScript
-- PROPER DIVISORS -----------------------------------------------------------

-- properDivisors :: Int -> [Int]
on properDivisors(n)
    if n = 1 then
        {1}
    else
        set realRoot to n ^ (1 / 2)
        set intRoot to realRoot as integer
        set blnPerfectSquare to intRoot = realRoot

        -- isFactor :: Int -> Bool
        script isFactor
            on |λ|(x)
                n mod x = 0
            end |λ|
        end script

        -- Factors up to square root of n,
        set lows to filter(isFactor, enumFromTo(1, intRoot))

        -- and quotients of these factors beyond the square root,

        -- integerQuotient :: Int -> Int
        script integerQuotient
            on |λ|(x)
                (n / x) as integer
            end |λ|
        end script

        -- excluding n itself (last item)
        items 1 thru -2 of (lows & map(integerQuotient, ¬
            items (1 + (blnPerfectSquare as integer)) thru -1 of reverse of lows))
    end if
end properDivisors


-- TEST ----------------------------------------------------------------------
on run
    -- numberAndDivisors :: Int -> [Int]
    script numberAndDivisors
        on |λ|(n)
            {num:n, divisors:properDivisors(n)}
        end |λ|
    end script

    -- maxDivisorCount :: Record -> Int -> Record
    script maxDivisorCount
        on |λ|(a, n)
            set intDivisors to length of properDivisors(n)

            if intDivisors ≥ divisors of a then
                {num:n, divisors:intDivisors}
            else
                a
            end if
        end |λ|
    end script

    {oneToTen:map(numberAndDivisors, ¬
        enumFromTo(1, 10)), mostDivisors:foldl(maxDivisorCount, ¬
        {num:0, divisors:0}, enumFromTo(1, 20000))} ¬

end run


-- GENERIC FUNCTIONS ---------------------------------------------------------

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

-- filter :: (a -> Bool) -> [a] -> [a]
on filter(f, xs)
    tell mReturn(f)
        set lst to {}
        set lng to length of xs
        repeat with i from 1 to lng
            set v to item i of xs
            if |λ|(v, i, xs) then set end of lst to v
        end repeat
        return lst
    end tell
end filter

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
```

```AppleScript
{oneToTen:{{num:1, divisors:{1}}, {num:2, divisors:{1}}, {num:3, divisors:{1}},
{num:4, divisors:{1, 2}}, {num:5, divisors:{1}}, {num:6, divisors:{1, 2, 3}},
{num:7, divisors:{1}}, {num:8, divisors:{1, 2, 4}}, {num:9, divisors:{1, 3}},
{num:10, divisors:{1, 2, 5}}},
mostDivisors:{num:18480, divisors:79}}
```



## Arc


```Arc


;; Given num, return num and the list of its divisors
(= divisor (fn (num)
   (= dlist '())
   (when (is 1 num) (= dlist '(1 0)))
   (when (is 2 num) (= dlist '(2 1)))
   (unless (or (is 1 num) (is 2 num))
   (up i 1 (+ 1 (/ num 2))
     (if (is 0 (mod num i))
         (push i dlist)))
   (= dlist (cons num dlist)))
   dlist))

;; Find out what number has the most divisors between 2 and 20,000.
;; Print a list of the largest known number's divisors as it is found.
(= div-lists (fn (cnt (o show 0))
  (= tlist '()) (= clist tlist)
  (when (> show 0) (prn tlist))
  (up i 1 cnt
    (divisor i)
    (when (is 1 show) (prn dlist))
    (when (>= (len dlist) (len tlist))
        (= tlist dlist)
        (when (is show 2) (prn tlist))
        (let c (- (len dlist) 1)
        (push (list i c) clist))))

  (= many-divisors (list ((clist 0) 1)))
  (for n 0 (is ((clist n) 1) ((clist 0) 1)) (= n (+ 1 n))
    (push ((clist n) 0) many-divisors))
  (= many-divisors (rev many-divisors))
  (prn "The number with the most divisors under " cnt
       " has " (many-divisors 0) " divisors.")
  (prn "It is the number "
  (if (> 2 (len many-divisors)) (cut (many-divisors) 1)
      (many-divisors 1)) ".")
  (prn "There are " (- (len many-divisors) 1) " numbers"
       " with this trait, and they are "
       (map [many-divisors _] (range 1 (- (len many-divisors) 1))))
  (prn (map [divisor _] (cut many-divisors 1)))
  many-divisors))

;; Do the tasks
(div-lists 10 1)
(div-lists 20000)
;; This took about 10 minutes on my machine.

```

```Arc

(1 0)
(2 1)
(3 1)
(4 2 1)
(5 1)
(6 3 2 1)
(7 1)
(8 4 2 1)
(9 3 1)
(10 5 2 1)
The number with the most divisors under 10 has 3 divisors.
It is the number 10.
There are 3 numbers with this trait, and they are (10 8 6)
((10 5 2 1) (8 4 2 1) (6 3 2 1))
'(3 10 8 6)


The number with the most divisors under 20000 has 79 divisors.
It is the number 18480.
There are 2 numbers with this trait, and they are (18480 15120)

```



## AWK


```AWK

# syntax: GAWK -f PROPER_DIVISORS.AWK
BEGIN {
    show = 0 # show divisors: 0=no, 1=yes
    print("    N  cnt  DIVISORS")
    for (i=1; i<=20000; i++) {
      divisors(i)
      if (i <= 10 || i == 100) { # including 100 as it was an example in task description
        printf("%5d  %3d  %s\n",i,Dcnt,Dstr)
      }
      if (Dcnt < max_cnt) {
        continue
      }
      if (Dcnt > max_cnt) {
        rec = ""
        max_cnt = Dcnt
      }
      rec = sprintf("%s%5d  %3d  %s\n",rec,i,Dcnt,show?Dstr:"divisors not shown")
    }
    printf("%s",rec)
    exit(0)
}
function divisors(n,  i) {
    if (n == 1) {
      Dcnt = 0
      Dstr = ""
      return
    }
    Dcnt = Dstr = 1
    for (i=2; i<n; i++) {
      if (n % i == 0) {
        Dcnt++
        Dstr = sprintf("%s %s",Dstr,i)
      }
    }
    return
}

```

<p>output:</p>

```txt

    N  cnt  DIVISORS
    1    0
    2    1  1
    3    1  1
    4    2  1 2
    5    1  1
    6    3  1 2 3
    7    1  1
    8    3  1 2 4
    9    2  1 3
   10    3  1 2 5
  100    8  1 2 4 5 10 20 25 50
15120   79  divisors not shown
18480   79  divisors not shown

```



## BaCon


```qbasic

FUNCTION ProperDivisor(nr, show)

    LOCAL probe, total

    FOR probe = 1 TO nr-1
        IF MOD(nr, probe) = 0 THEN
            IF show THEN PRINT " ", probe;
            INCR total
        END IF
    NEXT

    RETURN total

END FUNCTION

FOR x = 1 TO 10
    PRINT x, ":";
    IF ProperDivisor(x, 1) = 0 THEN PRINT " 0";
    PRINT
NEXT

FOR x = 1 TO 20000
    DivisorCount = ProperDivisor(x, 0)
    IF DivisorCount > MaxDivisors THEN
        MaxDivisors = DivisorCount
        MagicNumber = x
    END IF
NEXT

PRINT "Most proper divisors for number in the range 1-20000: ", MagicNumber, " with ", MaxDivisors, " divisors."

```

```txt

1: 0
2: 1
3: 1
4: 1 2
5: 1
6: 1 2 3
7: 1
8: 1 2 4
9: 1 3
10: 1 2 5
Most proper divisors for number in the range 1-20000: 15120 with 79 divisors.

```



## C


### Brute Force

C has tedious boilerplate related to allocating memory for dynamic arrays, so we just skip the problem of storing values altogether.

```c

#include <stdio.h>
#include <stdbool.h>

int proper_divisors(const int n, bool print_flag)
{
    int count = 0;

    for (int i = 1; i < n; ++i) {
        if (n % i == 0) {
            count++;
            if (print_flag)
                printf("%d ", i);
        }
    }

    if (print_flag)
        printf("\n");

    return count;
}

int main(void)
{
    for (int i = 1; i <= 10; ++i) {
        printf("%d: ", i);
        proper_divisors(i, true);
    }

    int max = 0;
    int max_i = 1;

    for (int i = 1; i <= 20000; ++i) {
        int v = proper_divisors(i, false);
        if (v >= max) {
            max = v;
            max_i = i;
        }
    }

    printf("%d with %d divisors\n", max_i, max);
    return 0;
}

```

```txt

1:
2: 1
3: 1
4: 1 2
5: 1
6: 1 2 3
7: 1
8: 1 2 4
9: 1 3
10: 1 2 5
18480 with 79 divisors

```


### Number Theoretic

There is no need to go through all the divisors if only the count is needed, this implementation refines the brute force approach by solving the second part of the task via a Number Theory formula. The running time is noticeably faster than the brute force method above. Output is same as the above.

```C

#include <stdio.h>
#include <stdbool.h>

int proper_divisors(const int n, bool print_flag)
{
    int count = 0;

    for (int i = 1; i < n; ++i) {
        if (n % i == 0) {
            count++;
            if (print_flag)
                printf("%d ", i);
        }
    }

    if (print_flag)
        printf("\n");

    return count;
}

int countProperDivisors(int n){
	int prod = 1,i,count=0;

	while(n%2==0){
		count++;
		n /= 2;
	}

	prod *= (1+count);

	for(i=3;i*i<=n;i+=2){
		count = 0;

		while(n%i==0){
			count++;
			n /= i;
		}

		prod *= (1+count);
	}

	if(n>2)
		prod *= 2;

	return prod - 1;
}

int main(void)
{
    for (int i = 1; i <= 10; ++i) {
        printf("%d: ", i);
        proper_divisors(i, true);
    }

    int max = 0;
    int max_i = 1;

    for (int i = 1; i <= 20000; ++i) {
        int v = countProperDivisors(i);
        if (v >= max) {
            max = v;
            max_i = i;
        }
    }

    printf("%d with %d divisors\n", max_i, max);
    return 0;
}

```



## C#


```c#
namespace RosettaCode.ProperDivisors
{
    using System;
    using System.Collections.Generic;
    using System.Linq;

    internal static class Program
    {
        private static IEnumerable<int> ProperDivisors(int number)
        {
            return
                Enumerable.Range(1, number / 2)
                    .Where(divisor => number % divisor == 0);
        }

        private static void Main()
        {
            foreach (var number in Enumerable.Range(1, 10))
            {
                Console.WriteLine("{0}: {{{1}}}", number,
                    string.Join(", ", ProperDivisors(number)));
            }

            var record = Enumerable.Range(1, 20000).Select(number => new
            {
                Number = number,
                Count = ProperDivisors(number).Count()
            }).OrderByDescending(currentRecord => currentRecord.Count).First();
            Console.WriteLine("{0}: {1}", record.Number, record.Count);
        }
    }
}
```

```txt
1: {}
2: {1}
3: {1}
4: {1, 2}
5: {1}
6: {1, 2, 3}
7: {1}
8: {1, 2, 4}
9: {1, 3}
10: {1, 2, 5}
15120: 79
```


## C++


```cpp
#include <vector>
#include <iostream>
#include <algorithm>

std::vector<int> properDivisors ( int number ) {
   std::vector<int> divisors ;
   for ( int i = 1 ; i < number / 2 + 1 ; i++ )
      if ( number % i == 0 )
	 divisors.push_back( i ) ;
   return divisors ;
}

int main( ) {
   std::vector<int> divisors ;
   unsigned int maxdivisors = 0 ;
   int corresponding_number = 0 ;
   for ( int i = 1 ; i < 11 ; i++ ) {
      divisors =  properDivisors ( i ) ;
      std::cout << "Proper divisors of " << i << ":\n" ;
      for ( int number : divisors ) {
	 std::cout << number << " " ;
      }
      std::cout << std::endl ;
      divisors.clear( ) ;
   }
   for ( int i = 11 ; i < 20001 ; i++ ) {
      divisors =  properDivisors ( i ) ;
      if ( divisors.size( ) > maxdivisors ) {
	 maxdivisors = divisors.size( ) ;
	 corresponding_number = i ;
      }
      divisors.clear( ) ;
   }

   std::cout << "Most divisors has " << corresponding_number <<
      " , it has " << maxdivisors << " divisors!\n" ;
   return 0 ;
}

```

```txt

Proper divisors of 1:

Proper divisors of 2:
1
Proper divisors of 3:
1
Proper divisors of 4:
1 2
Proper divisors of 5:
1
Proper divisors of 6:
1 2 3
Proper divisors of 7:
1
Proper divisors of 8:
1 2 4
Proper divisors of 9:
1 3
Proper divisors of 10:
1 2 5
Most divisors has 15120 , it has 79 divisors!

```



## Ceylon


```ceylon
shared void run() {

	function divisors(Integer int) =>
			if(int <= 1)
			then {}
			else (1..int / 2).filter((Integer element) => element.divides(int));

	for(i in 1..10) {
		print("``i`` => ``divisors(i)``");
	}

	value start = 1;
	value end = 20k;

	value mostDivisors =
			map {for(i in start..end) i->divisors(i).size}
			.inverse()
			.max(byKey(byIncreasing(Integer.magnitude)));

	print("the number(s) with the most divisors between ``start`` and ``end`` is/are:
	       ``mostDivisors?.item else "nothing"`` with ``mostDivisors?.key else "no"`` divisors");
}
```

```txt
1 => []
2 => { 1 }
3 => { 1 }
4 => { 1, 2 }
5 => { 1 }
6 => { 1, 2, 3 }
7 => { 1 }
8 => { 1, 2, 4 }
9 => { 1, 3 }
10 => { 1, 2, 5 }
the number(s) with the most divisors between 1 and 20000 is/are:
[15120, 18480] with 79 divisors
```



## Clojure


```lisp
(ns properdivisors
  (:gen-class))

(defn proper-divisors [n]
  " Proper divisors of n"
  (if (= n 1)
    []
  (filter #(= 0 (rem n %)) (range 1 n))))

;; Property divisors of numbers 1 to 20,000 inclusive
(def data (for [n (range 1 (inc 20000))]
            [n (proper-divisors n)]))

;; Find Max
(defn maximal-key [k x & xs]
  " Normal max-key only finds one key that produces maximum, while this function finds them all "
  (reduce (fn [ys x]
            (let [c (compare (k x) (k (peek ys)))]
              (cond
                (pos? c) [x]
                (neg? c) ys
                :else    (conj ys x))))
          [x]
          xs))

(println "n\tcnt\tPROPER DIVISORS")
(doseq [n (range 1 11)]
  (let [factors (proper-divisors n)]
    (println n "\t" (count factors) "\t" factors)))

(def max-data (apply maximal-key (fn [[i pd]] (count pd)) data))

(doseq [[n factors] max-data]
  (println n " has " (count factors) " divisors"))


```

```txt

n	cnt	PROPER DIVISORS
1 	 0 	 []
2 	 1 	 (1)
3 	 1 	 (1)
4 	 2 	 (1 2)
5 	 1 	 (1)
6 	 3 	 (1 2 3)
7 	 1 	 (1)
8 	 3 	 (1 2 4)
9 	 2 	 (1 3)
10 	 3 	 (1 2 5)
15120  has  79  divisors
18480  has  79  divisors

```


## Common Lisp

Ideally, the smallest-divisor function would only try prime numbers instead of odd numbers.

```lisp
(defun proper-divisors-recursive (product &optional (results '(1)))
   "(int,list)->list::Function to find all proper divisors of a +ve integer."

   (defun smallest-divisor (x)
      "int->int::Find the smallest divisor of an integer > 1."
      (if (evenp x) 2
          (do ((lim (truncate (sqrt x)))
               (sd 3 (+ sd 2)))
              ((or (integerp (/ x sd)) (> sd lim)) (if (> sd lim) x sd)))))

   (defun pd-rec (fac)
      "(int,int)->nil::Recursive function to find proper divisors of a +ve integer"
      (when (not (member fac results))
         (push fac results)
         (let ((hifac (/ fac (smallest-divisor fac))))
            (pd-rec hifac)
            (pd-rec (/ product hifac)))))

   (pd-rec product)
   (butlast (sort (copy-list results) #'<)))

(defun task (method &optional (n 1) (most-pds '(0)))
   (dotimes (i 19999)
      (let ((npds (length (funcall method (incf n))))
            (hiest (car most-pds)))
         (when (>= npds hiest)
            (if (> npds hiest)
                (setf most-pds (list npds (list n)))
                (setf most-pds (list npds (cons n (second most-pds))))))))
   most-pds)

(defun main ()
   (format t "Task 1:Proper Divisors of [1,10]:~%")
   (dotimes (i 10) (format t "~A:~A~%" (1+ i) (proper-divisors-recursive (1+ i))))
   (format t "Task 2:Count & list of numbers <=20,000 with the most Proper Divisors:~%~A~%"
           (task #'proper-divisors-recursive)))
```

```txt
CL-USER(10): (main)
Task 1:Proper Divisors of [1,10]:
1:NIL
2:(1)
3:(1)
4:(1 2)
5:(1)
6:(1 2 3)
7:(1)
8:(1 2 4)
9:(1 3)
10:(1 2 5)
Task 2:Count & list of numbers <=20,000 with the most Proper Divisors:
(79 (18480 15120))
NIL
```



## Component Pascal

```oberon2

MODULE RosettaProperDivisor;
IMPORT StdLog;

PROCEDURE Pd*(n: LONGINT;OUT r: ARRAY OF LONGINT):LONGINT;
VAR
	i,j: LONGINT;
BEGIN
	i := 1;j := 0;
	IF n >  1 THEN
		WHILE (i < n) DO
			IF (n MOD i) = 0 THEN
				IF (j < LEN(r)) THEN r[j] := i END; INC(j)
			END;
			INC(i)
		END;
	END;
	RETURN j
END Pd;

PROCEDURE Do*;
VAR
	r: ARRAY 128 OF LONGINT;
	i,j,found,max,idxMx: LONGINT;
	mx: ARRAY 128 OF LONGINT;
BEGIN
	FOR i := 1 TO 10 DO
		found := Pd(i,r);
		IF found > LEN(r) THEN (* Error. more pd than r can admit *) HALT(1) END;
		StdLog.Int(i);StdLog.String("[");StdLog.Int(found);StdLog.String("]:> ");
		FOR j := 0 TO found - 1 DO
			StdLog.Int(r[j]);StdLog.Char(' ');
		END;
		StdLog.Ln
	END;

	max := 0;idxMx := 0;
  FOR i := 1 TO 20000 DO
  	found := Pd(i,r);
  	IF found > max THEN
    	idxMx:= 0;mx[idxMx] := i;max := found
	  ELSIF found = max THEN
    	INC(idxMx);mx[idxMx] := i
  	END;
  END;
	StdLog.String("Found: ");StdLog.Int(idxMx + 1);
  StdLog.String(" Numbers with the longest proper divisors [");
	StdLog.Int(max);StdLog.String("]: ");StdLog.Ln;
	FOR i := 0 TO idxMx DO
  	StdLog.Int(mx[i]);StdLog.Ln
	END
END Do;

END RosettaProperDivisor.

^Q RosettaProperDivisor.Do~

```

```txt

 1[ 0]:>
 2[ 1]:>  1
 3[ 1]:>  1
 4[ 2]:>  1  2
 5[ 1]:>  1
 6[ 3]:>  1  2  3
 7[ 1]:>  1
 8[ 3]:>  1  2  4
 9[ 2]:>  1  3
 10[ 3]:>  1  2  5
Found:  2 Numbers with the longest proper divisors [ 79]:
 15120
 18480

```


## D

Currently the lambda of the filter allocates a closure on the GC-managed heap.

```d
void main() /*@safe*/ {
    import std.stdio, std.algorithm, std.range, std.typecons;

    immutable properDivs = (in uint n) pure nothrow @safe /*@nogc*/ =>
        iota(1, (n + 1) / 2 + 1).filter!(x => n % x == 0 && n != x);

    iota(1, 11).map!properDivs.writeln;
    iota(1, 20_001).map!(n => tuple(properDivs(n).count, n)).reduce!max.writeln;
}
```

```txt
[[], [1], [1], [1, 2], [1], [1, 2, 3], [1], [1, 2, 4], [1, 3], [1, 2, 5]]
Tuple!(uint, int)(79, 18480)
```

The Run-time is about 0.67 seconds with the ldc2 compiler.


## Dyalect


```dyalect
func properDivs(n) {
    if n == 1 {
        return
    }
    for x in 1..(n-1) {
        if n % x == 0 {
            yield x
        }
    }
}

for i in 1..10 {
    print("\(i): \(properDivs(i).toArray())")
}

var (num, max) = (0,0)

for i in 1..20000 {
    const count = properDivs(i).len()
    if count > max {
        set (num, max) = (i, count)
    }
}

print("\(num): \(max)")
```


```txt
1: []
2: [1]
3: [1]
4: [1, 2]
5: [1]
6: [1, 2, 3]
7: [1]
8: [1, 2, 4]
9: [1, 3]
10: [1, 2, 5]
15120: 79
```



## EchoLisp


```scheme

(lib 'list) ;; list-delete

;; let n = product p_i^a_i , p_i prime
;; number of divisors = product (a_i + 1) - 1
(define (numdivs n)
    (1- (apply * (map (lambda(g) (1+ (length g))) (group (prime-factors n))))))

(remember 'numdivs)

;; prime powers
;; input : a list g of grouped prime factors ( 3 3 3 ..)
;; returns (1 3 9 27 ...)
(define (ppows g (mult 1))
	(for/fold (ppows '(1)) ((a g))
	(set! mult (* mult a))
	(cons mult ppows)))

;; proper divisors
;; decomp n into ((2 2 ..) ( 3 3 ..)  ) prime factors groups
;; combines (1 2 4 8 ..) (1 3 9 ..) lists
;; remove n from the list

(define (divs n)
   (if (<= n 1) null
    (list-delete
        (for/fold (divs'(1)) ((g (map  ppows (group (prime-factors n)))))
		    (for*/list ((a divs) (b g)) (* a b)))
    n )))

;; find number(s) with max # of proper divisors
;; returns list of (n . maxdivs)  for n in range 2..N

(define (most-proper N)
    (define maxdivs 1)
    (define ndivs 0)
    (for/fold (most-proper null) ((n (in-range 2 N)))
       (set! ndivs (numdivs n))
        #:continue (< ndivs maxdivs)
        (when (> ndivs maxdivs)
        (set!-values (most-proper maxdivs) (values null ndivs)))
        (cons (cons n maxdivs) most-proper)))



```

```scheme

(for ((i (in-range 1 11))) (writeln i (divs i)))
1     null
2     (1)
3     (1)
4     (2 1)
5     (1)
6     (2 3 1)
7     (1)
8     (4 2 1)
9     (3 1)
10     (2 5 1)


(most-proper 20000)
    → ((18480 . 79) (15120 . 79))
(most-proper 1_000_000)
    → ((997920 . 239) (982800 . 239) (942480 . 239) (831600 . 239) (720720 . 239))

(lib 'bigint)
(numdivs 95952222101012742144)  → 666 ;; 🎩

```



## Eiffel


```Eiffel

class
	APPLICATION

create
	make

feature

	make
			-- Test the feature proper_divisors.
		local
			list: LINKED_LIST [INTEGER]
			count, number: INTEGER
		do
			across
				1 |..| 10 as c
			loop
				list := proper_divisors (c.item)
				io.put_string (c.item.out + ": ")
				across
					list as l
				loop
					io.put_string (l.item.out + " ")
				end
				io.new_line
			end
			across
				1 |..| 20000 as c
			loop
				list := proper_divisors (c.item)
				if list.count > count then
					count := list.count
					number := c.item
				end
			end
			io.put_string (number.out + " has with " + count.out + " divisors the highest number of proper divisors.")
		end

	proper_divisors (n: INTEGER): LINKED_LIST [INTEGER]
			-- Proper divisors of 'n'.
		do
			create Result.make
			across
				1 |..| (n - 1) as c
			loop
				if n \\ c.item = 0 then
					Result.extend (c.item)
				end
			end
		end

end

```

```txt

1:
2: 1
3: 1
4: 1 2
5: 1
6: 1 2 3
7: 1
8: 1 2 4
9: 1 3
10: 1 2 5
15120 has with 79 divisors the highest number of proper divisors.

```



## Elixir

```elixir
defmodule Proper do
  def divisors(1), do: []
  def divisors(n), do: [1 | divisors(2,n,:math.sqrt(n))] |> Enum.sort

  defp divisors(k,_n,q) when k>q, do: []
  defp divisors(k,n,q) when rem(n,k)>0, do: divisors(k+1,n,q)
  defp divisors(k,n,q) when k * k == n, do: [k | divisors(k+1,n,q)]
  defp divisors(k,n,q)                , do: [k,div(n,k) | divisors(k+1,n,q)]

  def most_divisors(limit) do
    {length,nums} = Enum.group_by(1..limit, fn n -> length(divisors(n)) end)
                    |> Enum.max_by(fn {length,_nums} -> length end)
    IO.puts "With #{length}, Number #{inspect nums} has the most divisors"
  end
end

Enum.each(1..10, fn n ->
  IO.puts "#{n}: #{inspect Proper.divisors(n)}"
end)
Proper.most_divisors(20000)
```


```txt

1: []
2: [1]
3: [1]
4: [1, 2]
5: [1]
6: [1, 2, 3]
7: [1]
8: [1, 2, 4]
9: [1, 3]
10: [1, 2, 5]
With 79, Number [18480, 15120] has the most divisors

```


=={{Header|Erlang}}==


```erlang
-module(properdivs).
-export([divs/1,sumdivs/1,longest/1]).

divs(0) -> [];
divs(1) -> [];
divs(N) -> lists:sort([1] ++ divisors(2,N,math:sqrt(N))).

divisors(K,_N,Q) when K > Q -> [];
divisors(K,N,Q) when N rem K =/= 0 ->
    divisors(K+1,N,Q);
divisors(K,N,Q) when K * K  == N ->
    [K] ++ divisors(K+1,N,Q);
divisors(K,N,Q) ->
    [K, N div K] ++ divisors(K+1,N,Q).

sumdivs(N) -> lists:sum(divs(N)).

longest(Limit) -> longest(Limit,0,0,1).

longest(L,Current,CurLeng,Acc) when Acc >= L ->
    io:format("With ~w, Number ~w has the most divisors~n", [CurLeng,Current]);
longest(L,Current,CurLeng,Acc) ->
    A = length(divs(Acc)),
    if A > CurLeng ->
        longest(L,Acc,A,Acc+1);
        true -> longest(L,Current,CurLeng,Acc+1)
    end.
```

```txt

1> [io:format("X: ~w, N: ~w~n", [N,properdivs:divs(N)]) ||  N <- lists:seq(1,10)].
X: 1, N: []
X: 2, N: [1]
X: 3, N: [1]
X: 4, N: [1,2]
X: 5, N: [1]
X: 6, N: [1,2,3]
X: 7, N: [1]
X: 8, N: [1,2,4]
X: 9, N: [1,3]
X: 10, N: [1,2,5]
[ok,ok,ok,ok,ok,ok,ok,ok,ok,ok]

2> properdivs:longest(20000).
With 79, Number 15120 has the most divisors

```



## F#


```F#

let mutable a=0
let mutable b=0
let mutable c=0
let mutable d=0
let mutable e=0
let mutable f=0
for k=1 to 10 do
    b <- 0
    f <- k/2
    printf "divisor "
    for l=1 to f do
        if k%l=0 then
           b <- b+1
           printf " %i," l
    printf "no of divisor %i" b
    printfn ""
for i=1 to 20000 do
    b <- 0
    f <- i/2
    for j=1 to f do
       if i%j=0 then
         b <- b+1
    if b=c then
         d <- 0
         d <- i
    if c<b then
        c <- b

printfn "%i has %i divisor" d c


```


A purely functional approach.

```fsharp

// the simple function with the answer
let propDivs n = [1..n/2] |> List.filter (fun x->n % x = 0)

// to cache the result length; helpful for a long search
let propDivDat n = propDivs n |> fun xs -> n, xs.Length, xs

// UI: always the longest and messiest
let show (n,count,divs) =
  let showCount = count |> function | 0-> "no proper divisors" | 1->"1 proper divisor" | _-> sprintf "%d proper divisors" count
  let showDiv = divs |> function | []->"" | x::[]->sprintf ": %d" x | _->divs |> Seq.map string |> String.concat "," |> sprintf ": %s"
  printfn "%d has %s%s" n showCount showDiv

// generate output
[1..10] |> List.iter (propDivDat >> show)

// use a sequence: we don't really need to hold this data, just iterate over it
Seq.init 20000 ( ((+) 1) >> propDivDat)
|> Seq.fold (fun a b ->match a,b with | (_,c1,_),(_,c2,_) when c2 > c1 -> b | _-> a) (0,0,[])
|> fun (n,count,_) -> (n,count,[]) |> show

```


```txt

1 has no proper divisors
2 has 1 proper divisor: 1
3 has 1 proper divisor: 1
4 has 2 proper divisors: 1,2
5 has 1 proper divisor: 1
6 has 3 proper divisors: 1,2,3
7 has 1 proper divisor: 1
8 has 3 proper divisors: 1,2,4
9 has 2 proper divisors: 1,3
10 has 3 proper divisors: 1,2,5
15120 has 79 proper divisors

```



## Factor


```Factor

USING: math.primes.factors math.ranges ;
10 [1,b] [ divisors but-last ] map [ 1 + pprint bl . ] each-index
20000 [1,b] [ divisors but-last length ] map dup supremum
swap dupd index 1 + pprint " with " write pprint " divisors." print

```


```txt

1 { }
2 { 1 }
3 { 1 }
4 { 1 2 }
5 { 1 }
6 { 1 2 3 }
7 { 1 }
8 { 1 2 4 }
9 { 1 3 }
10 { 1 2 5 }
15120 with 79 divisors.

```



## Fortran

Compiled using G95 compiler, run on x86 system under Puppy Linux

```Fortran


      function icntprop(num  )
      icnt=0
      do i=1 , num-1
          if (mod(num , i)  .eq. 0)  then
          icnt = icnt + 1
          if (num .lt. 11) print *,'    ',i
          end if
          end do
      icntprop =  icnt
      end function

      limit = 20000
      maxcnt = 0
      print *,'N   divisors'
      do j=1,limit,1
      if (j .lt. 11) print *,j
      icnt = icntprop(j)

      if (icnt .gt. maxcnt) then
      maxcnt = icnt
      maxj = j
      end if

      end do

      print *,' '
      print *,' from 1 to ',limit
      print *,maxj,' has max proper divisors: ',maxcnt
      end

```

```txt


 N   divisors
 1
 2
      1
 3
      1
 4
      1
      2
 5
      1
 6
      1
      2
      3
 7
      1
 8
      1
      2
      4
 9
      1
      3
 10
      1
      2
      5

  from 1 to  20000
 15120  has max proper divisors:  79

```



## FreeBASIC


```freebasic

' FreeBASIC v1.05.0 win64

Sub ListProperDivisors(limit As Integer)
  If limit < 1 Then Return
  For i As Integer = 1 To limit
     Print Using "##"; i;
     Print " ->";
     If i = 1 Then
       Print " (None)"
       Continue For
     End if
     For j As Integer = 1 To i \ 2
       If i Mod j = 0 Then Print " "; j;
     Next j
     Print
  Next i
End Sub

Function CountProperDivisors(number As Integer) As Integer
  If number < 2 Then Return 0
  Dim count As Integer = 0
  For i As Integer = 1 To number \ 2
    If number Mod i = 0 Then count += 1
  Next
  Return count
End Function

Dim As Integer n, count, most = 1, maxCount = 0

Print "The proper divisors of the following numbers are :"
Print
ListProperDivisors(10)

For n As Integer = 2 To 20000
  count = CountProperDivisors(n)
  If count > maxCount Then
    maxCount = count
    most = n
  EndIf
Next

Print
Print Str(most); " has the most proper divisors, namely"; maxCount
Print
Print "Press any key to exit the program"
Sleep
End

```


```txt

The proper divisors of the following numbers are :

 1 -> (None)
 2 ->  1
 3 ->  1
 4 ->  1  2
 5 ->  1
 6 ->  1  2  3
 7 ->  1
 8 ->  1  2  4
 9 ->  1  3
10 ->  1  2  5

15120 has the most proper divisors, namely 79

```



## Frink

Frink's built-in factorization routines efficiently find factors of arbitrary-sized integers.


```frink

for n = 1 to 10
   println["$n\t" + join[" ", properDivisors[n]]]

println[]

d = new dict
for n = 1 to 20000
{
   c = length[properDivisors[n]]
   d.addToList[c, n]
}

most = max[keys[d]]
println[d@most + " have $most factors"]

properDivisors[n] := allFactors[n, true, false, true]

```

```txt

1
2       1
3       1
4       1 2
5       1
6       1 2 3
7       1
8       1 2 4
9       1 3
10      1 2 5

[15120, 18480] have 79 factors

```



## GFA Basic


<lang>
OPENW 1
CLEARW 1
'
' Array f% is used to hold the divisors
DIM f%(SQR(20000)) ! cannot redim arrays, so set size to largest needed
'
' 1. Show proper divisors of 1 to 10, inclusive
'
FOR i%=1 TO 10
  num%=@proper_divisors(i%)
  PRINT "Divisors for ";i%;":";
  FOR j%=1 TO num%
    PRINT " ";f%(j%);
  NEXT j%
  PRINT
NEXT i%
'
' 2. Find (smallest) number <= 20000 with largest number of proper divisors
'
result%=1 ! largest so far
number%=0 ! its number of divisors
FOR i%=1 TO 20000
  num%=@proper_divisors(i%)
  IF num%>number%
    result%=i%
    number%=num%
  ENDIF
NEXT i%
PRINT "Largest number of divisors is ";number%;" for ";result%
'
~INP(2)
CLOSEW 1
'
' find the proper divisors of n%, placing results in f%
' and return the number found
'
FUNCTION proper_divisors(n%)
  LOCAL i%,root%,count%
  '
  ARRAYFILL f%(),0
  count%=1 ! index of next slot in f% to fill
  '
  IF n%>1
    f%(count%)=1
    count%=count%+1
    root%=SQR(n%)
    FOR i%=2 TO root%
      IF n% MOD i%=0
        f%(count%)=i%
        count%=count%+1
        IF i%*i%<>n% ! root% is an integer, so check if i% is actual squa- lists:seq(1,10)].
X: 1, N: []
X: 2, N: [1]
X: 3, N: [1]
X: 4, N: [1,2]
X: 5, N: [1]
X: 6, N: [1,2,3]
X: 7, N: [1]
X: 8, N: [1,2,4]
X: 9, N: [1,3]
X: 10, N: [1,2,5]
[ok,ok,ok,ok,ok,ok,ok,ok,ok,ok]

2> properdivs:longest(20000).
With 79, Number 15120 has the most divisors
re root of n%
          f%(count%)=n%/i%
          count%=count%+1
        ENDIF
      ENDIF
    NEXT i%
  ENDIF
  '
  RETURN count%-1
ENDFUNC

```


Output is:

```txt

Divisors for 1:
Divisors for 2: 1
Divisors for 3: 1
Divisors for 4: 1 2
Divisors for 5: 1
Divisors for 6: 1 2 3
Divisors for 7: 1
Divisors for 8: 1 2 4
Divisors for 9: 1 3
Divisors for 10: 1 2 5
Largest number of divisors is 79 for 15120

```



## Go

```go
package main

import (
    "fmt"
    "strconv"
)

func listProperDivisors(limit int) {
    if limit < 1 {
        return
    }
    width := len(strconv.Itoa(limit))
    for i := 1; i <= limit; i++ {
        fmt.Printf("%*d -> ", width, i)
        if i == 1 {
            fmt.Println("(None)")
            continue
        }
        for j := 1; j <= i/2; j++ {
            if i%j == 0 {
                fmt.Printf(" %d", j)
            }
        }
        fmt.Println()
    }
}

func countProperDivisors(n int) int {
    if n < 2 {
        return 0
    }
    count := 0
    for i := 1; i <= n/2; i++ {
        if n%i == 0 {
            count++
        }
    }
    return count
}

func main() {
    fmt.Println("The proper divisors of the following numbers are :\n")
    listProperDivisors(10)
    fmt.Println()
    maxCount := 0
    most := []int{1}
    for n := 2; n <= 20000; n++ {
        count := countProperDivisors(n)
        if count == maxCount {
            most = append(most, n)
        } else if count > maxCount {
            maxCount = count
            most = most[0:1]
            most[0] = n
        }
    }
    fmt.Print("The following number(s) <= 20000 have the most proper divisors, ")
    fmt.Println("namely", maxCount, "\b\n")
    for _, n := range most {
        fmt.Println(n)
    }
}
```


```txt

The proper divisors of the following numbers are :

 1 -> (None)
 2 ->  1
 3 ->  1
 4 ->  1 2
 5 ->  1
 6 ->  1 2 3
 7 ->  1
 8 ->  1 2 4
 9 ->  1 3
10 ->  1 2 5

The following number(s) <= 20000 have the most proper divisors, namely 79

15120
18480

```



## Haskell


```Haskell
import Data.Ord
import Data.List

divisors :: (Integral a) => a -> [a]
divisors n = filter ((0 ==) . (n `mod`)) [1 .. (n `div` 2)]

main :: IO ()
main = do
  putStrLn "divisors of 1 to 10:"
  mapM_ (print . divisors) [1 .. 10]
  putStrLn "a number with the most divisors within 1 to 20000 (number, count):"
  print $ maximumBy (comparing snd)
    [(n, length $ divisors n) | n <- [1 .. 20000]]
```

```txt
divisors of 1 to 10:
[]
[1]
[1]
[1,2]
[1]
[1,2,3]
[1]
[1,2,4]
[1,3]
[1,2,5]
a number with the most divisors within 1 to 20000 (number, count):
(18480,79)
```


Or, for a little more efficiency, filtering only up to the root, and deriving the higher proper divisors from the lower ones, as quotients:


```haskell
import Data.List (maximumBy)
import Data.Ord (comparing)
import Data.Bool (bool)

properDivisors
  :: Integral a
  => a -> [a]
properDivisors n =
  let root = (floor . sqrt . fromIntegral) n
      lows = filter ((0 ==) . rem n) [1 .. root]
  in init (lows ++ bool id tail (n == root * root) (reverse (quot n <$> lows)))

main :: IO ()
main = do
  putStrLn "Proper divisors of 1 to 10:"
  mapM_ (print . properDivisors) [1 .. 10]
  mapM_
    putStrLn
    [ ""
    , "A number in the range 1 to 20,000 with the most proper divisors,"
    , "as (number, count of proper divisors):"
    , ""
    ]
  print $
    maximumBy (comparing snd) $
    (,) <*> (length . properDivisors) <$> [1 .. 20000]
```

```txt
Proper divisors of 1 to 10:
[]
[1]
[1]
[1,2]
[1]
[1,2,3]
[1]
[1,2,4]
[1,3]
[1,2,5]

A number in the range 1 to 20,000 with the most proper divisors,
as (number, count of proper divisors):

(18480,79)
```



## J


The proper divisors of an integer are the [[Factors of an integer]] without the integer itself.

So, borrowing from [[Factors of an integer#J|the J implementation]] of that related task:


```J
factors=: [: /:~@, */&>@{@((^ i.@>:)&.>/)@q:~&__
properDivisors=: factors -. ]
```


Proper divisors of numbers 1 through 10:


```J
   (,&": ' -- ' ,&": properDivisors)&>1+i.10
1 --
2 -- 1
3 -- 1
4 -- 1 2
5 -- 1
6 -- 1 2 3
7 -- 1
8 -- 1 2 4
9 -- 1 3
10 -- 1 2 5
```


Number(s) not exceeding 20000 with largest number of proper divisors (and the count of those divisors):


```J
   (, #@properDivisors)&> 1+I.(= >./) #@properDivisors@> 1+i.20000
15120 79
18480 79
```


Note that it's a bit more efficient to simply count factors here, when selecting the candidate numbers.


```J
      (, #@properDivisors)&> 1+I.(= >./) #@factors@> 1+i.20000
15120 79
18480 79
```


We could also arbitrarily toss either 15120 or 18480 (keeping the other number), if it were important that we produce only one result.


## Java

```java5
import java.util.Collections;
import java.util.LinkedList;
import java.util.List;

public class Proper{
    public static List<Integer> properDivs(int n){
        List<Integer> divs = new LinkedList<Integer>();
        if(n == 1) return divs;
        divs.add(1);
        for(int x = 2; x < n; x++){
            if(n % x == 0) divs.add(x);
        }

        Collections.sort(divs);

        return divs;
    }

    public static void main(String[] args){
        for(int x = 1; x <= 10; x++){
            System.out.println(x + ": " + properDivs(x));
        }

        int x = 0, count = 0;
        for(int n = 1; n <= 20000; n++){
            if(properDivs(n).size() > count){
                x = n;
                count = properDivs(n).size();
            }
        }
        System.out.println(x + ": " + count);
    }
}
```

```txt
1: []
2: [1]
3: [1]
4: [1, 2]
5: [1]
6: [1, 2, 3]
7: [1]
8: [1, 2, 4]
9: [1, 3]
10: [1, 2, 5]
15120: 79
```



## JavaScript



### ES5



```JavaScript
(function () {

    // Proper divisors
    function properDivisors(n) {
        if (n < 2) return [];
        else {
            var rRoot = Math.sqrt(n),
                intRoot = Math.floor(rRoot),

                lows = range(1, intRoot).filter(function (x) {
                    return (n % x) === 0;
                });

            return lows.concat(lows.slice(1).map(function (x) {
                return n / x;
            }).reverse().slice((rRoot === intRoot) | 0));
        }
    }

    // [m..n]
    function range(m, n) {
        var a = Array(n - m + 1),
            i = n + 1;
        while (i--) a[i - 1] = i;
        return a;
    }

    var tblOneToTen = [
            ['Number', 'Proper Divisors', 'Count']
        ].concat(range(1, 10).map(function (x) {
            var ds = properDivisors(x);

            return [x, ds.join(', '), ds.length];
        })),

        dctMostBelow20k = range(1, 20000).reduce(function (a, x) {
            var lng = properDivisors(x).length;

            return lng > a.divisorCount ? {
                n: x,
                divisorCount: lng
            } : a;
        }, {
            n: 0,
            divisorCount: 0
        });


    // [[a]] -> bool -> s -> s
    function wikiTable(lstRows, blnHeaderRow, strStyle) {
        return '{| class="wikitable" ' + (
            strStyle ? 'style="' + strStyle + '"' : ''
        ) + lstRows.map(function (lstRow, iRow) {
            var strDelim = ((blnHeaderRow && !iRow) ? '!' : '|');

            return '\n|-\n' + strDelim + ' ' + lstRow.map(function (v) {
                return typeof v === 'undefined' ? ' ' : v;
            }).join(' ' + strDelim + strDelim + ' ');
        }).join('') + '\n|}';
    }

    return wikiTable(
        tblOneToTen,
        true
    ) + '\n\nMost proper divisors below 20,000:\n\n  ' + JSON.stringify(
        dctMostBelow20k
    );

})();
```


{| class="wikitable"
|-
! Number !! Proper Divisors !! Count
|-
| 1 ||  || 0
|-
| 2 || 1 || 1
|-
| 3 || 1 || 1
|-
| 4 || 1, 2 || 2
|-
| 5 || 1 || 1
|-
| 6 || 1, 2, 3 || 3
|-
| 7 || 1 || 1
|-
| 8 || 1, 2, 4 || 3
|-
| 9 || 1, 3 || 2
|-
| 10 || 1, 2, 5 || 3
|}

Most proper divisors below 20,000:

  {"n":15120,"divisorCount":79}


### ES6



```JavaScript
(() => {

    // properDivisors :: Int -> [Int]
    let properDivisors = n => {
            let rRoot = Math.sqrt(n),
                intRoot = Math.floor(rRoot),
                blnPerfectSquare = rRoot === intRoot,

                lows = range(1, intRoot)
                .filter(x => (n % x) === 0);

            // for perfect squares, we can drop
            // the head of the 'highs' list
            return lows.concat(lows
                    .map(x => n / x)
                    .reverse()
                    .slice(blnPerfectSquare | 0)
                )
                .slice(0, -1); // except n itself
        },

        // range :: Int -> Int -> [Int]
        range = (m, n) => Array.from({
            length: (n - m) + 1
        }, (_, i) => m + i);



    return {
        properDivisorsOf1to10: range(1, 10)
            .reduce((a, x) => (
                a[x.toString()] = properDivisors(x),
                a
            ), {}),

        intMaxDivisorsUnder20k: range(1, 20000)
            .reduce((a, x) => {
                let intDivisors = properDivisors(x)
                    .length;

                return intDivisors >= a.divisors ? {
                    max: x,
                    divisors: intDivisors
                } : a;

            }, {
                max: 0,
                divisors: 0
            })
    };

})();
```



```JavaScript
{
  "properDivisorsOf1to10":{
    "1":[], "2":[1], "3":[1], "4":[1, 2], "5":[1],
    "6":[1, 2, 3], "7":[1], "8":[1, 2, 4], "9":[1, 3], "10":[1, 2, 5]
  },
  "intMaxDivisorsUnder20k":{"max":18480, "divisors":79}
}
```



## jq

In the following, proper_divisors returns a stream. In order to count the number of items in the stream economically, we first define "count(stream)":

```jq
def count(stream): reduce stream as $i (0; . + 1);

# unordered
def proper_divisors:
  . as $n
  | if $n > 1 then 1,
      ( range(2; 1 + (sqrt|floor)) as $i
        | if ($n % $i) == 0 then $i,
            (($n / $i) | if . == $i then empty else . end)
         else empty
	 end)
    else empty
    end;

# The first integer in 1 .. n inclusive
# with the maximal number of proper divisors in that range:
def most_proper_divisors(n):
  reduce range(1; n+1) as $i
    ( [null, 0];
      count( $i | proper_divisors ) as $count
      | if $count > .[1] then [$i, $count] else . end);
```

'''The tasks:'''

```jq
"The proper divisors of the numbers 1 to 10 inclusive are:",
(range(1;11) as $i | "\($i): \( [ $i | proper_divisors] )"),
"",
"The pair consisting of the least number in the range 1 to 20,000 with",
"the maximal number proper divisors together with the corresponding",
"count of proper divisors is:",
most_proper_divisors(20000)
```

```sh
$ jq -n -c -r -f /Users/peter/jq/proper_divisors.jq
The proper divisors of the numbers 1 to 10 inclusive are:
1: []
2: [1]
3: [1]
4: [1,2]
5: [1]
6: [1,2,3]
7: [1]
8: [1,2,4]
9: [1,3]
10: [1,2,5]

The pair consisting of the least number in the range 1 to 20,000 with
the maximal number proper divisors together with the corresponding
count of proper divisors is:
[15120,79]
```



## Julia

Use <code>factor</code> to obtain the prime factorization of the target number.  I adopted the argument handling style of <code>factor</code> in my <code>properdivisors</code> function.

```Julia

function properdivisors{T<:Integer}(n::T)
    0 < n || throw(ArgumentError("number to be factored must be ≥ 0, got $n"))
    1 < n || return T[]
    !isprime(n) || return T[one(T), n]
    f = factor(n)
    d = T[one(T)]
    for (k, v) in f
        c = T[k^i for i in 0:v]
        d = d*c'
        d = reshape(d, length(d))
    end
    sort!(d)
    return d[1:end-1]
end

lo = 1
hi = 10
println("List the proper divisors for ", lo, " through ", hi, ".")
for i in lo:hi
    println(@sprintf("%4d", i), " ", properdivisors(i))
end

hi = 2*10^4
println("\nFind the numbers within [", lo, ",", hi, "] having the most divisors.")

maxdiv = 0
nlst = Int[]

for i in lo:hi
    ndiv = length(properdivisors(i))
    if ndiv > maxdiv
        maxdiv = ndiv
        nlst = [i]
    elseif ndiv == maxdiv
        push!(nlst, i)
    end
end

println(nlst, " have the maximum proper divisor count of ", maxdiv, ".")

```


```txt

List the proper divisors for 1 through 10.
   1 []
   2 [1,2]
   3 [1,3]
   4 [1,2]
   5 [1,5]
   6 [1,2,3]
   7 [1,7]
   8 [1,2,4]
   9 [1,3]
  10 [1,2,5]

Find the numbers within [1,20000] having the most divisors.
[15120,18480] have the maximum proper divisor count of 79.

```



## Kotlin


```scala
// version 1.0.5-2

fun listProperDivisors(limit: Int) {
    if (limit < 1) return
    for(i in 1..limit) {
        print(i.toString().padStart(2) + " -> ")
        if (i == 1) {
            println("(None)")
            continue
        }
        (1..i/2).filter{ i % it == 0 }.forEach { print(" $it") }
        println()
    }
}

fun countProperDivisors(n: Int): Int {
    if (n < 2) return 0
    return (1..n/2).count { (n % it) == 0 }
}

fun main(args: Array<String>) {
    println("The proper divisors of the following numbers are :\n")
    listProperDivisors(10)
    println()
    var count: Int
    var maxCount = 0
    val most: MutableList<Int> = mutableListOf(1)
    for (n in 2..20000) {
        count = countProperDivisors(n)
        if (count == maxCount)
            most.add(n)
        else if (count > maxCount) {
            maxCount = count
            most.clear()
            most.add(n)
        }
    }
    println("The following number(s) have the most proper divisors, namely " + maxCount + "\n")
    for (n in most) println(n)
}
```


```txt

The proper divisors of the following numbers are :

 1 -> (None)
 2 ->  1
 3 ->  1
 4 ->  1 2
 5 ->  1
 6 ->  1 2 3
 7 ->  1
 8 ->  1 2 4
 9 ->  1 3
10 ->  1 2 5

The following number(s) have the most proper divisors, namely 79

15120
18480

```



## Lua


```Lua
-- Return a table of the proper divisors of n
function propDivs (n)
    if n < 2 then return {} end
    local divs, sqr = {1}, math.sqrt(n)
    for d = 2, sqr do
        if n % d == 0 then
            table.insert(divs, d)
            if d ~= sqr then table.insert(divs, n/d) end
        end
    end
    table.sort(divs)
    return divs
end

-- Show n followed by all values in t
function show (n, t)
    io.write(n .. ":\t")
    for _, v in pairs(t) do io.write(v .. " ") end
    print()
end

-- Main procedure
local mostDivs, numDivs, answer = 0
for i = 1, 10 do show(i, propDivs(i)) end
for i = 1, 20000 do
    numDivs = #propDivs(i)
    if numDivs > mostDivs then
        mostDivs = numDivs
        answer = i
    end
end
print(answer .. " has " .. mostDivs .. " proper divisors.")
```

```txt
1:
2:      1
3:      1
4:      1 2
5:      1
6:      1 2 3
7:      1
8:      1 2 4
9:      1 3
10:     1 2 5
15120 has 79 proper divisors.
```


=={{header|Mathematica}} / {{header|Wolfram Language}}==

A Function that yields the proper divisors of an integer n:

```Mathematica
ProperDivisors[n_Integer /; n > 0] := Most@Divisors@n;
```


Proper divisors of n from 1 to 10:

```Mathematica
Grid@Table[{n, ProperDivisors[n]}, {n, 1, 10}]
```

```txt
1	{}
2	{1}
3	{1}
4	{1,2}
5	{1}
6	{1,2,3}
7	{1}
8	{1,2,4}
9	{1,3}
10	{1,2,5}
```


The number with the most divisors between 1 and 20,000:

```Mathematica
Fold[
 Last[SortBy[{#1, {#2, Length@ProperDivisors[#2]}}, Last]] &,
 {0, 0},
 Range[20000]]
```

```txt
{18480, 79}
```


An alternate way to find the number with the most divisors between 1 and 20,000:

```Mathematica
Last@SortBy[
  Table[
    {n, Length@ProperDivisors[n]},
    {n, 1, 20000}],
  Last]
```

```txt
{15120, 79}
```



## Matlab


```matlab

function D=pd(N)
K=1:ceil(N/2);
D=K(~(rem(N, K)));

```

```txt

for I=1:10
   disp([num2str(I) ' : ' num2str(pd(I))])
end
1 : 1
2 : 1
3 : 1
4 : 1  2
5 : 1
6 : 1  2  3
7 : 1
8 : 1  2  4
9 : 1  3
10 : 1  2  5

maxL=0; maxI=0;
for I=1:20000
   L=length(pd(I));
   if L>maxL
      maxL=L; maxI=I;
   end
end
maxI

maxI =

       15120

maxL

maxL =

    79

```

=={{header|Modula-2}}==

```modula2
MODULE ProperDivisors;
FROM FormatString IMPORT FormatString;
FROM Terminal IMPORT WriteString,WriteLn,ReadChar;

PROCEDURE WriteInt(n : INTEGER);
VAR buf : ARRAY[0..15] OF CHAR;
BEGIN
    FormatString("%i", buf, n);
    WriteString(buf)
END WriteInt;

PROCEDURE proper_divisors(n : INTEGER; print_flag : BOOLEAN) : INTEGER;
VAR count,i : INTEGER;
BEGIN
    count := 0;
    FOR i:=1 TO n-1 DO
        IF n MOD i = 0 THEN
            INC(count);
            IF print_flag THEN
                WriteInt(i);
                WriteString(" ")
            END
        END
    END;
    IF print_flag THEN WriteLn END;
    RETURN count;
END proper_divisors;

VAR
    buf : ARRAY[0..63] OF CHAR;
    i,max,max_i,v : INTEGER;
BEGIN
    FOR i:=1 TO 10 DO
        WriteInt(i);
        WriteString(": ");
        proper_divisors(i, TRUE)
    END;

    max := 0;
    max_i := 1;

    FOR i:=1 TO 20000 DO
        v := proper_divisors(i, FALSE);
        IF v>= max THEN
            max := v;
            max_i := i
        END
    END;

    FormatString("%i with %i divisors\n", buf, max_i, max);
    WriteString(buf);

    ReadChar
END ProperDivisors.
```



## Objeck


```objeck
use Collection;

class Proper{
  function : Main(args : String[]) ~ Nil {
    for(x := 1; x <= 10; x++;) {
      Print(x, ProperDivs(x));
    };

    x := 0;
    count := 0;

    for(n := 1; n <= 20000; n++;) {
      if(ProperDivs(n)->Size() > count) {
        x := n;
        count := ProperDivs(n)->Size();
      };
    };
    "{$x}: {$count}"->PrintLine();
  }

  function : ProperDivs(n : Int) ~ IntVector {
    divs := IntVector->New();

    if(n = 1) {
      return divs;
    };
    divs->AddBack(1);

    for(x := 2; x < n; x++;) {
      if(n % x = 0) {
        divs->AddBack(x);
      };
    };
    divs->Sort();

    return divs;
  }

  function : Print(x : Int, result : IntVector) ~ Nil {
    "{$x}: "->Print();
    result->ToArray()->ToString()->PrintLine();
  }
}

```


Output:

```txt

1: []
2: [1]
3: [1]
4: [1,2]
5: [1]
6: [1,2,3]
7: [1]
8: [1,2,4]
9: [1,3]
10: [1,2,5]
15120: 79

```


=={{header|Oberon-2}}==

```oberon2

MODULE ProperDivisors;
IMPORT
  Out;

CONST
    initialSize = 128;
TYPE
  Result* = POINTER TO ResultDesc;
  ResultDesc = RECORD
    found-: LONGINT; (* number of slots in pd *)
    pd-: POINTER TO ARRAY OF LONGINT;
    cap: LONGINT;   (* Capacity *)
  END;

VAR
  i,found,max,idxMx: LONGINT;
  mx: ARRAY 32 OF LONGINT;
  rs: Result;

  PROCEDURE (r: Result) Init(size: LONGINT);
  BEGIN
    r.found := 0;
    r.cap := size;
    NEW(r.pd,r.cap);
  END Init;

  PROCEDURE (r: Result) Add(n: LONGINT);
  BEGIN
    (* Out.String("--->");Out.LongInt(n,0);Out.String(" At: ");Out.LongInt(r.found,0);Out.Ln; *)
    IF (r.found < LEN(r.pd^) - 1) THEN
      r.pd[r.found] := n;
    ELSE
      (* expand pd for more room *)
    END;
    INC(r.found);
  END Add;

  PROCEDURE (r:Result) Show();
  VAR
    i: LONGINT;
  BEGIN
      Out.String("(Result:");Out.LongInt(r.found + 1,0);(* Out.String("/");Out.LongInt(r.cap,0);*)
      Out.String("-");
      IF r.found > 0 THEN
        FOR i:= 0 TO r.found - 1 DO
          Out.LongInt(r.pd[i],0);
          IF i = r.found - 1 THEN Out.Char(')') ELSE Out.Char(',') END
        END
      END;
      Out.Ln
  END Show;

  PROCEDURE (r:Result) Reset();
  BEGIN
    r.found := 0;
  END Reset;

  PROCEDURE GetFor(n: LONGINT;VAR rs: Result);
  VAR
    i: LONGINT;
  BEGIN
    IF n > 1 THEN
      rs.Add(1);i := 2;
      WHILE (i < n) DO
        IF (n MOD i) = 0 THEN rs.Add(i) END;
        INC(i)
      END
    END;
  END GetFor;

BEGIN
  NEW(rs);rs.Init(initialSize);
  FOR i := 1 TO 10 DO
    Out.LongInt(i,4);Out.Char(':');
    GetFor(i,rs);
    rs.Show();
    rs.Reset();
  END;
  Out.LongInt(100,4);Out.Char(':');GetFor(100,rs);rs.Show();rs.Reset();
  max := 0;idxMx := 0;found := 0;
  FOR i := 1 TO 20000 DO
    GetFor(i,rs);
    IF rs.found > max THEN
      idxMx:= 0;mx[idxMx] := i;max := rs.found
    ELSIF rs.found = max THEN
      INC(idxMx);mx[idxMx] := i
    END;
    rs.Reset()
  END;
  Out.String("Found: ");Out.LongInt(idxMx + 1,0);
  Out.String(" Numbers with most proper divisors ");
  Out.LongInt(max,0);Out.String(": ");Out.Ln;
  FOR i := 0 TO idxMx DO
    Out.LongInt(mx[i],0);Out.Ln
  END
END ProperDivisors.

```

```txt

   1:(Result:1-
   2:(Result:2-1)
   3:(Result:2-1)
   4:(Result:3-1,2)
   5:(Result:2-1)
   6:(Result:4-1,2,3)
   7:(Result:2-1)
   8:(Result:4-1,2,4)
   9:(Result:3-1,3)
  10:(Result:4-1,2,5)
 100:(Result:9-1,2,4,5,10,20,25,50)
Found: 2 Numbers with most proper divisors 79:
15120
18480

```



## Oforth



```Oforth
Integer method: properDivs  self 2 / seq filter(#[ self swap mod 0 == ]) }

10 seq apply(#[ dup print " : " print properDivs println ])
20000 seq map(#[ dup properDivs size Pair new ]) reduce(#maxKey) println
```

```txt

1 : []
2 : [1]
3 : [1]
4 : [1, 2]
5 : [1]
6 : [1, 2, 3]
7 : [1]
8 : [1, 2, 4]
9 : [1, 3]
10 : [1, 2, 5]
[79, 15120]

```



## PARI/GP


```parigp
proper(n)=if(n==1, [], my(d=divisors(n)); d[2..#d]);
apply(proper, [1..10])
r=at=0; for(n=1,20000, t=numdiv(n); if(t>r, r=t; at=n)); [at, numdiv(t)-1]
```

```txt
%1 = [[], [2], [3], [2, 4], [5], [2, 3, 6], [7], [2, 4, 8], [3, 9], [2, 5, 10]]
%2 = [15120, 7]
```



## Pascal

Using prime factorisation

```pascal
{$IFDEF FPC}{$MODE DELPHI}{$ELSE}{$APPTYPE CONSOLE}{$ENDIF}
uses
  sysutils;
const
  MAXPROPERDIVS = 1920;
type
  tRes = array[0..MAXPROPERDIVS] of LongWord;
  tPot = record
           potPrim,
           potMax :LongWord;
         end;

  tprimeFac = record
                 pfPrims : array[1..10] of tPot;
                 pfCnt,
                 pfNum   : LongWord;
               end;
  tSmallPrimes = array[0..6541] of longWord;

var
  SmallPrimes: tSmallPrimes;

procedure InitSmallPrimes;
var
  pr,testPr,j,maxprimidx: Longword;
  isPrime : boolean;
Begin
  maxprimidx := 0;
  SmallPrimes[0] := 2;
  pr := 3;
  repeat
    isprime := true;
    j := 0;
    repeat
      testPr := SmallPrimes[j];
      IF testPr*testPr > pr then
        break;
      If pr mod testPr = 0 then
      Begin
        isprime := false;
        break;
      end;
      inc(j);
    until false;

    if isprime then
    Begin
      inc(maxprimidx);
      SmallPrimes[maxprimidx]:= pr;
    end;
    inc(pr,2);
  until pr > 1 shl 16 -1;
end;

procedure PrimeFacOut(primeDecomp:tprimeFac);
var
  i : LongWord;
begin
  with primeDecomp do
  Begin
    write(pfNum,' = ');
    For i := 1 to pfCnt-1 do
      with pfPrims[i] do
        If potMax = 1 then
          write(potPrim,'*')
        else
          write(potPrim,'^',potMax,'*');
    with pfPrims[pfCnt] do
      If potMax = 1 then
        write(potPrim)
      else
        write(potPrim,'^',potMax);
  end;
end;

procedure PrimeDecomposition(n:LongWord;var res:tprimeFac);
var
  i,pr,cnt,quot{to minimize divisions} : LongWord;
Begin
  res.pfNum := n;
  res.pfCnt:= 0;
  i := 0;
  cnt := 0;
  repeat
    pr := SmallPrimes[i];
    IF pr*pr>n then
      Break;

    quot := n div pr;
    IF pr*quot = n then
      with res do
      Begin
        inc(pfCnt);
        with pfPrims[pfCnt] do
        Begin
          potPrim := pr;
          potMax := 0;
          repeat
            n := quot;
            quot := quot div pr;
            inc(potMax);
          until pr*quot <> n;
        end;
      end;
     inc(i);
  until false;
  //a big prime left over?
  IF n <> 1 then
    with res do
    Begin
      inc(pfCnt);
      with pfPrims[pfCnt] do
      Begin
        potPrim := n;
        potMax := 1;
      end;
    end;
end;

function CntProperDivs(const primeDecomp:tprimeFac):LongWord;
//count of proper divisors
var
   i: LongWord;
begin
  result := 1;
  with primeDecomp do
    For i := 1 to pfCnt do
      result := result*(pfPrims[i].potMax+1);
  //remove
  dec(result);
end;

function findProperdivs(n:LongWord;var res:TRes):LongWord;
//simple trial division to get a sorted list of all proper divisors
var
  i,j: LongWord;
Begin
  result := 0;
  i := 1;
  j := n;
  while j>i do
  begin
    j := n DIV i;
    IF i*j = n then
    Begin
      //smaller factor part at the beginning upwards
      res[result]:= i;
      IF i <> j then
        //bigger factor at the end downwards
        res[MAXPROPERDIVS-result]:= j
      else
        //n is square number
        res[MAXPROPERDIVS-result]:= 0;
      inc(result);
    end;
    inc(i);
  end;

  If result>0 then
  Begin
    //move close together
    i := result;
    j := MAXPROPERDIVS-result+1;
    result := 2*result-1;
    repeat
      res[i] := res[j];
      inc(j);
      inc(i);
    until i > result;

    if res[result-1] = 0 then
      dec(result);
  end;
end;

procedure AllFacsOut(n: Longword);
var
  res:TRes;
  i,k,j:LongInt;
Begin
   j := findProperdivs(n,res);
   write(n:5,' : ');
   For k := 0 to j-2 do write(res[k],',');
   IF j>=1 then
     write(res[j-1]);
   writeln;
end;

var
  primeDecomp: tprimeFac;
  rs : tRes;
  i,j,max,maxcnt: LongWord;
BEGIN
  InitSmallPrimes;
  For i := 1 to 10 do
    AllFacsOut(i);
  writeln;
  max    := 0;
  maxCnt := 0;
  For i := 1 to 20*1000 do
  Begin
    PrimeDecomposition(i,primeDecomp);
    j := CntProperDivs(primeDecomp);
    IF j> maxCnt then
    Begin
      maxcnt := j;
      max := i;
    end;
  end;
  PrimeDecomposition(max,primeDecomp);
  j := CntProperDivs(primeDecomp);

  PrimeFacOut(primeDecomp);writeln('  ',j:10,' factors'); writeln;
  //https://en.wikipedia.org/wiki/Highly_composite_number <= HCN
  //http://wwwhomes.uni-bielefeld.de/achim/highly.txt the first 1200 HCN
  max := 3491888400;
  PrimeDecomposition(max,primeDecomp);
  j := CntProperDivs(primeDecomp);
  PrimeFacOut(primeDecomp);writeln('  ',j:10,' factors'); writeln;
END.
```
```txt

    1 :
    2 : 1
    3 : 1
    4 : 1,2
    5 : 1
    6 : 1,2,3
    7 : 1
    8 : 1,2,4
    9 : 1,3
   10 : 1,2,5

15120 = 2^4*3^3*5*7          79 factors

3491888400 = 2^4*3^3*5^2*7*11*13*17*19        1919 factors

real    0m0.004s
```



## Perl


### Using a module for divisors

```perl
use ntheory qw/divisors/;
sub proper_divisors {
  my $n = shift;
  # Like Pari/GP, divisors(0) = (0,1) and divisors(1) = ()
  return 1 if $n == 0;
  my @d = divisors($n);
  pop @d;  # divisors are in sorted order, so last entry is the input
  @d;
}
say "$_: ", join " ", proper_divisors($_) for 1..10;
# 1. For the max, we can do a traditional loop.
my($max,$ind) = (0,0);
for (1..20000) {
  my $nd = scalar proper_divisors($_);
 ($max,$ind) = ($nd,$_) if $nd > $max;
}
say "$max $ind";
# 2. Or we can use List::Util's max with decoration (this exploits its implementation)
{
  use List::Util qw/max/;
  no warnings 'numeric';
  say max(map { scalar(proper_divisors($_)) . " $_" } 1..20000);
}
```

```txt
1:
2: 1
3: 1
4: 1 2
5: 1
6: 1 2 3
7: 1
8: 1 2 4
9: 1 3
10: 1 2 5
79 15120
79 18480
```


Note that the first code will choose the first max, while the second chooses the last.


## Perl 6

Once your threshold is over 1000, the maximum proper divisors will always include 2, 3 and 5 as divisors, so only bother to check multiples of 2, 3 and 5.

There really isn't any point in using concurrency for a limit of 20_000. The setup and bookkeeping drowns out any benefit. Really doesn't start to pay off until the limit is 50_000 and higher. Try swapping in the commented out race map iterator line below for comparison.

```perl6
sub propdiv (\x) {
    my @l = 1 if x > 1;
    (2 .. x.sqrt.floor).map: -> \d {
        unless x % d { @l.push: d; my \y = x div d; @l.push: y if y != d }
    }
    @l
}

put "$_ [{propdiv($_)}]" for 1..10;

my @candidates;
loop (my int $c = 30; $c <= 20_000; $c += 30) {
#(30, *+30 …^ * > 500_000).race.map: -> $c {
    my \mx = +propdiv($c);
    next if mx < @candidates - 1;
    @candidates[mx].push: $c
}

say "max = {@candidates - 1}, candidates = {@candidates.tail}";
```

```txt
1 []
2 [1]
3 [1]
4 [1 2]
5 [1]
6 [1 2 3]
7 [1]
8 [1 2 4]
9 [1 3]
10 [1 2 5]
max = 79, candidates = 15120 18480
```



## Phix

The factors routine is an auto-include. The actual implementation of it, from builtins\pfactors.e is

```Phix
global function factors(atom n, integer include1=0)
-- returns a list of all integer factors of n
--  if include1 is 0 (the default), result does not contain either 1 or n
--  if include1 is 1, and n>1, the result contains 1 and n
--  if include1 is -1, and n>1, the result contains 1 but not n
sequence lfactors = {}, hfactors = {}
atom hfactor
integer p = 2,
        lim = floor(sqrt(n))

    if n!=1 and include1!=0 then
        lfactors = {1}
        if include1=1 then
            hfactors = {n}
        end if
    end if
    while p<=lim do
        if remainder(n,p)=0 then
            lfactors = append(lfactors,p)
            hfactor = n/p
            if hfactor=p then exit end if
            hfactors = prepend(hfactors,hfactor)
        end if
        p += 1
    end while
    return lfactors & hfactors
end function
```

The compiler knows where to find that, so the main program is just:

```Phix
for i=1 to 10 do
    ?{i,factors(i,-1)}
end for

integer maxd = 0, k
sequence candidates = {}

    for i=1 to 20000 do
        k = length(factors(i,-1))
        if k>=maxd then
            if k=maxd then
                candidates &= i
            else
                candidates = {i}
                maxd = k
            end if
        end if
    end for

    printf(1,"%d divisors:", maxd)
    ?candidates
    {} = wait_key()
```

```txt

{1,{}}
{2,{1}}
{3,{1}}
{4,{1,2}}
{5,{1}}
{6,{1,2,3}}
{7,{1}}
{8,{1,2,4}}
{9,{1,3}}
{10,{1,2,5}}
79 divisors:{15120,18480}

```



## PHP


```php
<?php
function ProperDivisors($n) {
  yield 1;
  $large_divisors = [];
  for ($i = 2; $i <= sqrt($n); $i++) {
    if ($n % $i == 0) {
      yield $i;
      if ($i*$i != $n) {
        $large_divisors[] = $n / $i;
      }
    }
  }
  foreach (array_reverse($large_divisors) as $i) {
    yield $i;
  }
}

assert([1, 2, 4, 5, 10, 20, 25, 50] ==
        iterator_to_array(ProperDivisors(100)));

foreach (range(1, 10) as $n) {
  echo "$n =>";
  foreach (ProperDivisors($n) as $divisor) {
    echo " $divisor";
  }
  echo "\n";
}

$divisorsCount = [];
for ($i = 1; $i < 20000; $i++) {
  $divisorsCount[sizeof(iterator_to_array(ProperDivisors($i)))][] = $i;
}
ksort($divisorsCount);

echo "Numbers with most divisors: ", implode(", ", end($divisorsCount)), ".\n";
echo "They have ", key($divisorsCount), " divisors.\n";


```


Outputs:


```txt
1 => 1
2 => 1
3 => 1
4 => 1 2
5 => 1
6 => 1 2 3
7 => 1
8 => 1 2 4
9 => 1 3
10 => 1 2 5
Numbers with most divisors: 15120, 18480.
They have 79 divisors.
```



## PicoLisp


```PicoLisp
# Generate all proper divisors.
(de propdiv (N)
   (head -1 (filter
      '((X) (=0 (% N X)))
      (range 1 N) )) )

# Obtaining the values from 1 to 10 inclusive.
(mapcar propdiv (range 1 10))
# Output:
# (NIL (1) (1) (1 2) (1) (1 2 3) (1) (1 2 4) (1 3) (1 2 5))
```

===Brute-force===

```PicoLisp
(de propdiv (N)
   (cdr
      (rot
         (make
            (for I N
               (and (=0 (% N I)) (link I)) ) ) ) ) )
(de countdiv (N)
   (let C -1
      (for I N
         (and (=0 (% N I)) (inc 'C)) )
      C ) )
(let F (-5 -8)
   (tab F "N" "LIST")
   (for I 10
      (tab F
         I
         (glue " + " (propdiv I)) ) ) )
(println
   (maxi
      countdiv
      (range 1 20000) ) )
```


### Factorization


```PicoLisp
(de accu1 (Var Key)
   (if (assoc Key (val Var))
      (con @ (inc (cdr @)))
      (push Var (cons Key 2)) )
   Key )
(de factor (N)
   (let
      (R NIL
         D 2
         L (1 2 2 . (4 2 4 2 4 6 2 6 .))
         M (sqrt N) )
      (while (>= M D)
         (if (=0 (% N D))
            (setq M
               (sqrt (setq N (/ N (accu1 'R D)))) )
            (inc 'D (pop 'L)) ) )
      (accu1 'R N)
      (dec (apply * (mapcar cdr R))) ) )
(bench
   (println
      (maxi
         factor
         (range 1 20000) )
      @@ ) )
```

Output:

```txt

15120 79
0.081 sec

```



## PL/I


```pli
*process source xref;
 (subrg):
 cpd: Proc Options(main);
 p9a=time();
 Dcl (p9a,p9b) Pic'(9)9';
 Dcl cnt(3) Bin Fixed(31) Init((3)0);
 Dcl x Bin Fixed(31);
 Dcl pd(300) Bin Fixed(31);
 Dcl sumpd   Bin Fixed(31);
 Dcl npd     Bin Fixed(31);
 Dcl hi      Bin Fixed(31) Init(0);
 Dcl (xl(10),xi) Bin Fixed(31);
 Dcl i       Bin Fixed(31);
 Do x=1 To 10;
   Call proper_divisors(x,pd,npd);
   Put Edit(x,' -> ',(pd(i) Do i=1 To npd))(Skip,f(2),a,10(f(2)));
   End;
 xi=0;
 Do x=1 To 20000;
   Call proper_divisors(x,pd,npd);
   Select;
     When(npd>hi) Do;
       xi=1;
       xl(1)=x;
       hi=npd;
       End;
     When(npd=hi) Do;
       xi+=1;
       xl(xi)=x;
       End;
     Otherwise;
     End;
   End;
 Put Edit(hi,' -> ',(xl(i) Do i=1 To xi))(Skip,f(3),a,10(f(6)));

 x= 166320; Call proper_divisors(x,pd,npd);
 Put Edit(x,' -> ',npd)(Skip,f(8),a,f(4));
 x=1441440; Call proper_divisors(x,pd,npd);
 Put Edit(x,' -> ',npd)(Skip,f(8),a,f(4));


 p9b=time();
 Put Edit((p9b-p9a)/1000,' seconds elapsed')(Skip,f(6,3),a);
 Return;

 proper_divisors: Proc(n,pd,npd);
 Dcl (n,pd(300),npd) Bin Fixed(31);
 Dcl (d,delta)       Bin Fixed(31);
 npd=0;
 If n>1 Then Do;
   If mod(n,2)=1 Then  /* odd number  */
     delta=2;
   Else                /* even number */
     delta=1;
   Do d=1 To n/2 By delta;
     If mod(n,d)=0 Then Do;
       npd+=1;
       pd(npd)=d;
       End;
     End;
   End;
 End;

 End;
```

```txt

 1 ->
 2 ->  1
 3 ->  1
 4 ->  1 2
 5 ->  1
 6 ->  1 2 3
 7 ->  1
 8 ->  1 2 4
 9 ->  1 3
10 ->  1 2 5
 79 ->  15120 18480
  166320 ->  159
 1441440 ->  287
 0.530 seconds elapsed
```



## PowerShell


### version 1


```PowerShell

function proper-divisor ($n) {
    if($n -ge 2) {
        $lim = [Math]::Floor([Math]::Sqrt($n))
        $less, $greater = @(1), @()
        for($i = 2; $i -lt $lim; $i++){
            if($n%$i -eq 0) {
                $less += @($i)
                $greater = @($n/$i) + $greater
            }
        }
        if(($lim -ne 1) -and ($n%$lim -eq 0)) {$less += @($lim)}
        $($less + $greater)
    } else {@()}
}
"$(proper-divisor 100)"
"$(proper-divisor 496)"
"$(proper-divisor 2048)"

```



### version 2


```PowerShell

function proper-divisor ($n) {
    if($n -ge 2) {
        $lim = [Math]::Floor($n/2)+1
        $proper = @(1)
        for($i = 2; $i -lt $lim; $i++){
            if($n%$i -eq 0) {
                $proper += @($i)
            }
        }
        $proper
    } else {@()}
}
"$(proper-divisor 100)"
"$(proper-divisor 496)"
"$(proper-divisor 2048)"

```



### version 3


```PowerShell

function eratosthenes ($n) {
    if($n -gt 1){
        $prime = @(0..$n| foreach{$true})
        $m = [Math]::Floor([Math]::Sqrt($n))
        function multiple($i) {
            for($j = $i*$i; $j -le $n; $j += $i) {
                $prime[$j] = $false
            }
        }
        multiple 2
        for($i = 3; $i -le $m; $i += 2) {
            if($prime[$i]) {multiple $i}
        }
        2
        for($i = 3; $i -le $n; $i += 2) {
            if($prime[$i]) {$i}
        }

    } else {
        Write-Error "$n is not greater than 1"
    }
}
function prime-decomposition ($n) {
    $array = eratosthenes $n
    $prime = @()
    foreach($p in $array) {
        while($n%$p -eq 0) {
            $n /= $p
            $prime += @($p)
        }
    }
    $prime
}
function proper-divisor ($n) {
    if($n -ge 2) {
        $array = prime-decomposition $n
        $lim = $array.Count
        function state($res, $i){
            if($i -lt $lim) {
                state ($res) ($i + 1)
                state ($res*$array[$i]) ($i + 1)
            } elseif($res -lt $n) {$res}
        }
        state 1 0 | sort -Unique
    } else {@()}
}
"$(proper-divisor 100)"
"$(proper-divisor 496)"
"$(proper-divisor 2048)"

```

<b>Output:</b>

```txt

1 2 4 5 10 20 25 50
1 2 4 8 16 31 62 124 248
1 2 4 8 16 32 64 128 256 512 1024

```



## Prolog


Taking a cue from [http://stackoverflow.com/a/171779 an SO answer]:


```prolog
divisor(N, Divisor) :-
    UpperBound is round(sqrt(N)),
    between(1, UpperBound, D),
    0 is N mod D,
    (
        Divisor = D
     ;
        LargerDivisor is N/D,
        LargerDivisor =\= D,
        Divisor = LargerDivisor
    ).

proper_divisor(N, D) :-
    divisor(N, D),
    D =\= N.


%% Task 1
%

proper_divisors(N, Ds) :-
    setof(D, proper_divisor(N, D), Ds).


%% Task 2
%

show_proper_divisors_of_range(Low, High) :-
    findall( N:Ds,
             ( between(Low, High, N),
               proper_divisors(N, Ds) ),
             Results ),
    maplist(writeln, Results).


%% Task 3
%

proper_divisor_count(N, Count) :-
    proper_divisors(N, Ds),
    length(Ds, Count).

find_most_proper_divisors_in_range(Low, High, Result) :-
    aggregate_all( max(Count, N),
                   ( between(Low, High, N),
                     proper_divisor_count(N, Count) ),
                   max(MaxCount, Num) ),
    Result = (num(Num)-divisor_count(MaxCount)).
```


Output:


```prolog
?- show_proper_divisors_of_range(1,10).
2:[1]
3:[1]
4:[1,2]
5:[1]
6:[1,2,3]
7:[1]
8:[1,2,4]
9:[1,3]
10:[1,2,5]
true.

?- find_most_proper_divisors_in_range(1,20000,Result).
Result = num(15120)-divisor_count(79).

```



## PureBasic


```PureBasic

EnableExplicit

Procedure ListProperDivisors(Number, List Lst())
  If Number < 2 : ProcedureReturn : EndIf
  Protected i
  For i = 1 To Number / 2
    If Number % i = 0
      AddElement(Lst())
      Lst() = i
    EndIf
  Next
EndProcedure

Procedure.i CountProperDivisors(Number)
  If Number < 2 : ProcedureReturn 0 : EndIf
  Protected i, count = 0
  For i = 1 To Number / 2
    If Number % i = 0
      count + 1
    EndIf
  Next
  ProcedureReturn count
EndProcedure

Define n, count, most = 1, maxCount = 0
If OpenConsole()
  PrintN("The proper divisors of the following numbers are : ")
  PrintN("")
  NewList lst()
  For n = 1 To 10
    ListProperDivisors(n, lst())
    Print(RSet(Str(n), 3) + " -> ")
    If ListSize(lst()) = 0
      Print("(None)")
    Else
      ForEach lst()
        Print(Str(lst()) + " ")
      Next
    EndIf
    ClearList(lst())
    PrintN("")
  Next
  For n = 2 To 20000
    count = CountProperDivisors(n)
    If count > maxCount
      maxCount = count
      most = n
    EndIf
  Next
  PrintN("")
  PrintN(Str(most) + " has the most proper divisors, namely " + Str(maxCount))
  PrintN("")
  PrintN("Press any key to close the console")
  Repeat: Delay(10) : Until Inkey() <> ""
  CloseConsole()
EndIf

```


```txt

The proper divisors of the following numbers are :

  1 -> (None)
  2 -> 1
  3 -> 1
  4 -> 1 2
  5 -> 1
  6 -> 1 2 3
  7 -> 1
  8 -> 1 2 4
  9 -> 1 3
 10 -> 1 2 5

15120 has the most proper divisors, namely 79

```



## Python


### Python: Literal

A very literal interpretation

```python>>>
 def proper_divs2(n):
...     return {x for x in range(1, (n + 1) // 2 + 1) if n % x == 0 and n != x}
...
>>> [proper_divs2(n) for n in range(1, 11)]
[set(), {1}, {1}, {1, 2}, {1}, {1, 2, 3}, {1}, {1, 2, 4}, {1, 3}, {1, 2, 5}]
>>>
>>> n, length = max(((n, len(proper_divs2(n))) for n in range(1, 20001)), key=lambda pd: pd[1])
>>> n
15120
>>> length
79
>>>
```




### Python: From prime factors

I found [http://stackoverflow.com/a/171784/10562 a reference] on how to generate factors from all the prime factors and the number of times each prime factor goes into N - its multiplicity.

For example, given N having prime factors P(i) with associated multiplicity M(i}) then the factors are given by:

```txt

for m[0] in range(M(0) + 1):
    for m[1] in range(M[1] + 1):
        ...
                for m[i - 1] in range(M[i - 1] + 1):
                    mult = 1
                    for j in range(i):
                        mult *= P[j] ** m[j]
                    yield mult
```


This version is over an order of magnitude faster for generating the proper divisors of the first 20,000 integers; at the expense of simplicity.

```python
from math import sqrt
from functools import lru_cache, reduce
from collections import Counter
from itertools import product


MUL = int.__mul__


def prime_factors(n):
    'Map prime factors to their multiplicity for n'
    d = _divs(n)
    d = [] if d == [n] else (d[:-1] if d[-1] == d else d)
    pf = Counter(d)
    return dict(pf)

@lru_cache(maxsize=None)
def _divs(n):
    'Memoized recursive function returning prime factors of n as a list'
    for i in range(2, int(sqrt(n)+1)):
        d, m  = divmod(n, i)
        if not m:
            return [i] + _divs(d)
    return [n]


def proper_divs(n):
    '''Return the set of proper divisors of n.'''
    pf = prime_factors(n)
    pfactors, occurrences = pf.keys(), pf.values()
    multiplicities = product(*(range(oc + 1) for oc in occurrences))
    divs = {reduce(MUL, (pf**m for pf, m in zip(pfactors, multis)), 1)
            for multis in multiplicities}
    try:
        divs.remove(n)
    except KeyError:
        pass
    return divs or ({1} if n != 1 else set())


if __name__ == '__main__':
    rangemax = 20000

    print([proper_divs(n) for n in range(1, 11)])
    print(*max(((n, len(proper_divs(n))) for n in range(1, 20001)), key=lambda pd: pd[1]))
```


```txt
[set(), {1}, {1}, {1, 2}, {1}, {1, 2, 3}, {1}, {1, 2, 4}, {1, 3}, {1, 2, 5}]
15120 79
```



## R


```r

# Proper divisors. 12/10/16 aev
require(numbers);
V <- sapply(1:20000, Sigma, k = 0, proper = TRUE); ind <- which(V==max(V));
cat("  *** max number of divisors:", max(V), "\n"," *** for the following indices:",ind, "\n");

```


```txt

Loading required package: numbers
  *** max number of divisors: 79
  *** for the following indices: 15120 18480

```



## Racket



###  Short version



```racket
#lang racket
(require math)
(define (proper-divisors n) (drop-right (divisors n) 1))
(for ([n (in-range 1 (add1 10))])
  (printf "proper divisors of: ~a\t~a\n" n (proper-divisors n)))
(define most-under-20000
  (for/fold ([best '(1)]) ([n (in-range 2 (add1 20000))])
    (define divs (proper-divisors n))
    (if (< (length (cdr best)) (length divs)) (cons n divs) best)))
(printf "~a has ~a proper divisors\n"
        (car most-under-20000) (length (cdr most-under-20000)))
```


```txt
proper divisors of: 1	()
proper divisors of: 2	(1)
proper divisors of: 3	(1)
proper divisors of: 4	(1 2)
proper divisors of: 5	(1)
proper divisors of: 6	(1 2 3)
proper divisors of: 7	(1)
proper divisors of: 8	(1 2 4)
proper divisors of: 9	(1 3)
proper divisors of: 10	(1 2 5)
15120 has 79 proper divisors
```




###  Long version


The '''main''' module will only be executed when this file is executed. When used as a library, it will not be used.

```racket
#lang racket/base
(provide fold-divisors ; name as per "Abundant..."
         proper-divisors)

(define (fold-divisors v n k0 kons)
  (define n1 (add1 n))
  (cond
    [(>= n1 (vector-length v))
     (define rv (make-vector n1 k0))
     (for* ([n (in-range 1 n1)] [m (in-range (* 2 n) n1 n)])
       (vector-set! rv m (kons n (vector-ref rv m))))
     rv]
    [else v]))

(define proper-divisors
  (let ([p.d-v (vector)])
    (λ (n)
      (set! p.d-v (reverse (fold-divisors p.d-v n null cons)))
      (vector-ref p.d-v n))))

(module+ main
  (for ([n (in-range 1 (add1 10))])
    (printf "proper divisors of: ~a\t~a\n" n (proper-divisors n)))

  (define count-proper-divisors
    (let ([p.d-v (vector)])
      (λ(n) (set! p.d-v (fold-divisors p.d-v n 0 (λ (d n) (add1 n))))
            (vector-ref p.d-v n))))

  (void (count-proper-divisors 20000))

  (define-values [C I]
    (for*/fold ([C 0] [I 1])
               ([i (in-range 1 (add1 20000))]
                [c (in-value (count-proper-divisors i))]
                #:when [> c C])
      (values c i)))
  (printf "~a has ~a proper divisors\n" I C))
```


The output is the same as the short version above.


## REXX


### version 1


```rexx
Call time 'R'
Do x=1 To 10
  Say x '->' proper_divisors(x)
  End

hi=1
Do x=1 To 20000
  /* If x//1000=0 Then Say x */
  npd=count_proper_divisors(x)
  Select
    When npd>hi Then Do
      list.npd=x
      hi=npd
      End
    When npd=hi Then
      list.hi=list.hi x
    Otherwise
      Nop
    End
  End

Say hi '->' list.hi

Say ' 166320 ->' count_proper_divisors(166320)
Say '1441440 ->' count_proper_divisors(1441440)

Say time('E') 'seconds elapsed'
Exit

proper_divisors: Procedure
Parse Arg n
If n=1 Then Return ''
pd=''
/* Optimization reduces 37 seconds to 28 seconds */
If n//2=1 Then  /* odd number  */
  delta=2
Else            /* even number */
  delta=1
Do d=1 To n%2 By delta
  If n//d=0 Then
    pd=pd d
  End
Return space(pd)

count_proper_divisors: Procedure
Parse Arg n
Return words(proper_divisors(n))
```

```txt
1 ->
2 -> 1
3 -> 1
4 -> 1 2
5 -> 1
6 -> 1 2 3
7 -> 1
8 -> 1 2 4
9 -> 1 3
10 -> 1 2 5
79 -> 15120 18480
 166320 -> 159
1441440 -> 287
28.342000 seconds elapsed
```



### version 2

The following REXX version is an adaptation of the   ''optimized''   version for the REXX language example for   ''Factors of an integer''.

This REXX version handles all integers   (negative, zero, positive)   and automatically adjusts the precision (decimal digits).

It also allows the specification of the ranges (for display and for finding the maximum),   and allows for extra numbers to be

specified.

With the (function) optimization, it's over   '''20'''   times faster.

```rexx
/*REXX program finds proper divisors (and count) of integer ranges; finds the max count.*/
parse arg bot top inc range xtra                 /*obtain optional arguments from the CL*/
if   bot=='' |   bot==","  then    bot=     1    /*Not specified?  Then use the default.*/
if   top=='' |   top==","  then    top=    10    /* "      "         "   "   "     "    */
if   inc=='' |   inc==","  then    inc=     1    /* "      "         "   "   "     "    */
if range=='' | range==","  then  range= 20000    /* "      "         "   "   "     "    */
w= max( length(top), length(bot), length(range)) /*determine the biggest number of these*/
numeric digits max(9, w + 1)                     /*have enough digits for  //  operator.*/
@.= 'and'                                        /*a literal used to separate #s in list*/
      do n=bot  to top  by inc                   /*process the first range specified.   */
      q= Pdivs(n);    #= words(q)                /*get proper divs; get number of Pdivs.*/
      if q=='∞'  then #= q                       /*adjust number of Pdivisors for zero. */
      say right(n, max(20, w) )   'has'   center(#, 4)     "proper divisors: "    q
      end   /*n*/
m=0                                              /*M ≡ maximum number of Pdivs (so far).*/
      do r=1  for range;    q= Pdivs(r)          /*process the second range specified.  */
      #= words(q);          if #<m  then iterate /*get proper divs; get number of Pdivs.*/
      if #<m  then iterate                       /*Less then max?   Then ignore this #. */
      @.#= @.#  @.  r;      m=#                  /*add this Pdiv to max list; set new M.*/
      end   /*r*/                                /* [↑]   process 2nd range of integers.*/
say
say m  ' is the highest number of proper divisors in range 1──►'range,
       ", and it's for: "       subword(@.m, 3)
say                                              /* [↓]  handle any given extra numbers.*/
      do i=1  for words(xtra);  n= word(xtra, i) /*obtain an extra number from XTRA list*/
      w= max(w, 1 + length(n) )                  /*use maximum width for aligned output.*/
      numeric digits max(9, 1 + length(n) )      /*have enough digits for  //  operator.*/
      q= Pdivs(n);              #= words(q)      /*get proper divs; get number of Pdivs.*/
      say  right(n, max(20, w) )     'has'     center(#, 4)      "proper divisors."
      end   /*i*/                                /* [↑] support extra specified integers*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
Pdivs: procedure; parse arg x,b;  x= abs(x);   if x==1  then return ''          /*unity?*/
       odd= x // 2;                            if x==0  then return '∞'         /*zero ?*/
       a= 1                                      /* [↓]  use all, or only odd #s.    ___*/
           do j=2+odd  by 1+odd  while j*j < x   /*divide by some integers up to    √ X */
           if x//j==0  then do;  a=a j;  b=x%j b /*if ÷, add both divisors to α & ß.    */
                            end
           end   /*j*/                           /* [↑]  %  is the REXX integer division*/
                                                 /* [↓]  adjust for a square.        ___*/
       if j*j==x  then  return  a j b            /*Was  X  a square?    If so, add  √ X */
                        return  a   b            /*return the divisors  (both lists).   */
```

```txt

                   0 has  ∞   proper divisors:  ∞
                   1 has  0   proper divisors:
                   2 has  1   proper divisors:  1
                   3 has  1   proper divisors:  1
                   4 has  2   proper divisors:  1 2
                   5 has  1   proper divisors:  1
                   6 has  3   proper divisors:  1 2 3
                   7 has  1   proper divisors:  1
                   8 has  3   proper divisors:  1 2 4
                   9 has  2   proper divisors:  1 3
                  10 has  3   proper divisors:  1 2 5

79 is the highest number of proper divisors in range 1──►20000, and it's for:  15120 and 18480

              166320 has 159  proper divisors.
             1441440 has 287  proper divisors.
         11796480000 has 329  proper divisors.

```



### version 3

When factoring           20,000 integers,   this REXX version is about   '''10%'''   faster than the REXX version 2.

When factoring            200,000 integers,   this REXX version is about   '''30%'''   faster.

When factoring                    2,000,000 integers,   this REXX version is about   '''40%'''   faster.

When factoring                        20,000,000 integers,   this REXX version is about   '''38%'''   faster.

It accomplishes a faster speed by incorporating the calculation of an   ''integer square root''   of an integer   (without using any floating point arithmetic).

```rexx
/*REXX program finds proper divisors (and count) of integer ranges; finds the max count.*/
parse arg bot top inc range xtra                 /*obtain optional arguments from the CL*/
if   bot=='' |   bot==","  then    bot=    1     /*Not specified?  Then use the default.*/
if   top=='' |   top==","  then    top=   10     /* "      "         "   "   "     "    */
if   inc=='' |   inc==","  then    inc=    1     /* "      "         "   "   "     "    */
if range=='' | range==","  then  range=20000     /* "      "         "   "   "     "    */
w= max( length(top), length(bot), length(range)) /*determine the biggest number of these*/
numeric digits max(9, w + 1)                     /*have enough digits for  //  operator.*/
@.= 'and'                                        /*a literal used to separate #s in list*/
      do n=bot  to top  by inc                   /*process the first range specified.   */
      q= Pdivs(n);    #= words(q)                /*get proper divs; get number of Pdivs.*/
      if q=='∞'  then #= q                       /*adjust number of Pdivisors for zero. */
      say right(n, max(20, w) )   'has'   center(#, 4)     "proper divisors: "    q
      end   /*n*/
m=0                                              /*M ≡ maximum number of Pdivs (so far).*/
      do r=1  for range;    q= Pdivs(r)          /*process the second range specified.  */
      #= words(q);          if #<m  then iterate /*get proper divs; get number of Pdivs.*/
      if #<m  then iterate                       /*Less then max?   Then ignore this #. */
      @.#= @.#  @.  r;      m=#                  /*add this Pdiv to max list; set new M.*/
      end   /*r*/                                /* [↑]   process 2nd range of integers.*/
say
say m  ' is the highest number of proper divisors in range 1──►'range,
       ", and it's for: "       subword(@.m, 3)
say                                              /* [↓]  handle any given extra numbers.*/
      do i=1  for words(xtra);  n= word(xtra, i) /*obtain an extra number from XTRA list*/
      w= max(w, 1 + length(n) )                  /*use maximum width for aligned output.*/
      numeric digits max(9, 1 + length(n) )      /*have enough digits for  //  operator.*/
      q= Pdivs(n);              #= words(q)      /*get proper divs; get number of Pdivs.*/
      say  right(n, max(20, w) )     'has'     center(#, 4)      "proper divisors."
      end   /*i*/                                /* [↑] support extra specified integers*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
Pdivs: procedure; parse arg x 1 z,b;  x= abs(x);   if x==1  then return ''      /*unity?*/
       odd= x // 2;                                if x==0  then return '∞'     /*zero ?*/
       r= 0;         q= 1                        /* [↓] ══integer square root══     ___ */
            do while q<=z; q=q*4; end            /*R:  an integer which will be    √ X  */
            do while q>1;  q=q%4; _= z-r-q;  r=r%2;  if _>=0  then  do;  z=_;  r=r+q;  end
            end   /*while q>1*/                  /* [↑]  compute the integer sqrt of  X.*/
       a=1                                       /* [↓]  use all, or only odd #s.   ___ */
           do j=2 +odd  by 1 +odd to r -(r*r==x) /*divide by some integers up to   √ X  */
           if x//j==0  then do;  a=a j;  b=x%j b /*if ÷, add both divisors to α & ß.    */
                            end
           end   /*j*/                           /* [↑]  %  is the REXX integer division*/
                                                 /* [↓]  adjust for a square.        ___*/
       if j*j==x  then  return  a j b            /*Was  X  a square?    If so, add  √ X */
                        return  a   b            /*return the divisors  (both lists).   */
```

## Ring


```ring

# Project : Proper divisors

limit = 10
for n=1 to limit
    if n=1
       see "" + 1 + " -> (None)" + nl
       loop
    ok
    see "" + n + " -> "
    for m=1 to n-1
        if n%m = 0
           see " " + m
        ok
    next
    see nl
next

```

Output:

```txt

1 -> (None)
2 ->  1
3 ->  1
4 ->  1 2
5 ->  1
6 ->  1 2 3
7 ->  1
8 ->  1 2 4
9 ->  1 3
10 ->  1 2 5

```



## Ruby


```ruby
require "prime"

class Integer
  def proper_divisors
    return [] if self == 1
    primes = prime_division.flat_map{|prime, freq| [prime] * freq}
    (1...primes.size).each_with_object([1]) do |n, res|
      primes.combination(n).map{|combi| res << combi.inject(:*)}
    end.flatten.uniq
  end
end

(1..10).map{|n| puts "#{n}: #{n.proper_divisors}"}

size, select = (1..20_000).group_by{|n| n.proper_divisors.size}.max
select.each do |n|
  puts "#{n} has #{size} divisors"
end
```


```txt

1: []
2: [1]
3: [1]
4: [1, 2]
5: [1]
6: [1, 2, 3]
7: [1]
8: [1, 2, 4]
9: [1, 3]
10: [1, 2, 5]
15120 has 79 divisors
18480 has 79 divisors

```



### An Alternative Approach


```ruby
#Determine the integer within a range of integers that has the most proper divisors
#Nigel Galloway: December 23rd., 2014
require "prime"
n, g = 0
(1..20000).each{|i| e = i.prime_division.inject(1){|n,g| n * (g[1]+1)}
                    n, g = e, i if e > n}
puts "#{g} has #{n-1} proper divisors"
```


In the range 1..200000

```txt

15120 has 79 proper divisors

```

and in the ranges 1..2000000 & 1..20000000

```txt

166320 has 159 proper divisors
1441440 has 287 proper divisors

```



## Rust



```rust
trait ProperDivisors {
    fn proper_divisors(&self) -> Option<Vec<u64>>;
}

impl ProperDivisors for u64 {
    fn proper_divisors(&self) -> Option<Vec<u64>> {
        if self.le(&1) {
            return None;
        }
        let mut divisors: Vec<u64> = Vec::new();

        for i in 1..*self {
            if *self % i == 0 {
                divisors.push(i);
            }
        }
        Option::from(divisors)
    }
}

fn main() {
    for i in 1..11 {
        println!("Proper divisors of {:2}: {:?}", i,
                 i.proper_divisors().unwrap_or(vec![]));
    }

    let mut most_idx: u64 = 0;
    let mut most_divisors: Vec<u64> = Vec::new();
    for i in 1..20_001 {
        let divs = i.proper_divisors().unwrap_or(vec![]);
        if divs.len() > most_divisors.len() {
            most_divisors = divs;
            most_idx = i;
        }
    }
    println!("In 1 to 20000, {} has the most proper divisors at {}", most_idx,
             most_divisors.len());
}

```

```txt
Proper divisors of  1: []
Proper divisors of  2: [1]
Proper divisors of  3: [1]
Proper divisors of  4: [1, 2]
Proper divisors of  5: [1]
Proper divisors of  6: [1, 2, 3]
Proper divisors of  7: [1]
Proper divisors of  8: [1, 2, 4]
Proper divisors of  9: [1, 3]
Proper divisors of 10: [1, 2, 5]
In 1 to 20000, 15120 has the most proper divisors at 79

```


=={{header|S-Basic}}==

```Basic

$constant false = 0
$constant true = FFFFH

rem - compute p mod q
function mod(p, q = integer) = integer
end = p - q * (p/q)

rem - count, and optionally display, proper divisors of n
function divisors(n, display = integer) = integer
  var i, limit, count, start, delta = integer
  if mod(n, 2) = 0 then
    begin
      start = 2
      delta = 1
    end
  else
    begin
      start = 3
      delta = 2
    end
  if n < 2 then count = 0 else count = 1
  if display and (count = 1) then print using "#####"; 1;
  i = start
  limit = n / start
  while i <= limit do
    begin
      if mod(n, i) = 0 then
        begin
          if display then print using "#####"; i;
          count = count + 1
        end
      i = i + delta
      if count = 1 then limit = n / i
    end
  if display then print
end = count

rem - main program begins here
var i, ndiv, highdiv, highnum = integer

print "Proper divisors of first 10 numbers:"
for i = 1 to 10
  print using "### : "; i;
  ndiv = divisors(i, true)
next i

print "Searching for number with most divisors ..."
highdiv = 1
highnum = 1
for i = 1 to 20000
  ndiv = divisors(i, false)
  if ndiv > highdiv then
    begin
      highdiv = ndiv
      highnum = i
    end
next i
print "Searched up to"; i
print highnum; " has the most divisors: "; highdiv

end

```

```txt

Proper divisors of first 10 numbers:
  1 :
  2 :     1
  3 :     1
  4 :     1     2
  5 :     1
  6 :     1     2    3
  7 :     1
  8 :     1     2    4
  9 :     1     3
 10 :     1     2    5
Searching for number with most divisors ...
Searched up to 20000
 15120 has the most divisors:  79

```



## Scala



### Simple proper divisors


```Scala
def properDivisors(n: Int) = (1 to n/2).filter(i => n % i == 0)
def format(i: Int, divisors: Seq[Int]) = f"$i%5d    ${divisors.length}%2d   ${divisors mkString " "}"

println(f"    n   cnt   PROPER DIVISORS")
val (count, list) = (1 to 20000).foldLeft( (0, List[Int]()) ) { (max, i) =>
    val divisors = properDivisors(i)
    if (i <= 10 || i == 100) println( format(i, divisors) )
    if (max._1 < divisors.length) (divisors.length, List(i))
    else if (max._1 == divisors.length) (divisors.length, max._2 ::: List(i))
    else max
}

list.foreach( number => println(f"$number%5d    ${properDivisors(number).length}") )
```

```txt
    n   cnt   PROPER DIVISORS
    1     0
    2     1   1
    3     1   1
    4     2   1 2
    5     1   1
    6     3   1 2 3
    7     1   1
    8     3   1 2 4
    9     2   1 3
   10     3   1 2 5
  100     8   1 2 4 5 10 20 25 50
15120    79
18480    79
```



### Proper divisors for integers for big integers

If ''Long''s are enough to you you can replace every ''BigInt'' with ''Long'' and the one ''BigInt(1)'' with ''1L''


```Scala
import scala.annotation.tailrec

def factorize(x: BigInt): List[BigInt] = {
  @tailrec
  def foo(x: BigInt, a: BigInt = 2, list: List[BigInt] = Nil): List[BigInt] = a * a > x match {
    case false if x % a == 0 => foo(x / a, a, a :: list)
    case false => foo(x, a + 1, list)
    case true => x :: list
  }

  foo(x)
}

def properDivisors(n: BigInt): List[BigInt] = {
  val factors = factorize(n)
  val products = (1 until factors.length).flatMap(i => factors.combinations(i).map(_.product).toList).toList
  (BigInt(1) :: products).filter(_ < n)
}
```



## Seed7


```seed7
$ include "seed7_05.s7i";

const proc: writeProperDivisors (in integer: n) is func
  local
    var integer: i is 0;
  begin
    for i range 1 to n div 2 do
      if n rem i = 0 then
        write(i <& " ");
      end if;
    end for;
    writeln;
  end func;

const func integer: countProperDivisors (in integer: n) is func
  result
    var integer: count is 0;
  local
    var integer: i is 0;
  begin
    for i range 1 to n div 2 step succ(n rem 2) do
      if n rem i = 0 then
        incr(count);
      end if;
    end for;
  end func;

const proc: main is func
  local
    var integer: i is 0;
    var integer: v is 0;
    var integer: max is 0;
    var integer: max_i is 1;
  begin
    for i range 1 to 10 do
      write(i <& ": ");
      writeProperDivisors(i);
    end for;
    for i range 1 to 20000 do
      v := countProperDivisors(i);
      if v > max then
        max := v;
        max_i := i;
      end if;
    end for;
    writeln(max_i <& " with " <& max <& " divisors");
  end func;
```


```txt

1:
2: 1
3: 1
4: 1 2
5: 1
6: 1 2 3
7: 1
8: 1 2 4
9: 1 3
10: 1 2 5
15120 with 79 divisors

```



## Sidef

```ruby
func propdiv (n) {
    n.divisors.slice(0, -2)
}

{|i| printf("%2d: %s\n", i, propdiv(i)) } << 1..10

var max = 0
var candidates = []

for i in (1..20_000) {
    var divs = propdiv(i).len
    if (divs > max) {
        candidates = []
        max = divs
    }
    candidates << i if (divs == max)
}

say "max = #{max}, candidates = #{candidates}"
```

```txt

 1: []
 2: [1]
 3: [1]
 4: [1, 2]
 5: [1]
 6: [1, 2, 3]
 7: [1]
 8: [1, 2, 4]
 9: [1, 3]
10: [1, 2, 5]
max = 79, candidates = [15120, 18480]

```



## Swift

Simple function:

```Swift
func properDivs1(n: Int) -> [Int] {

    return filter (1 ..< n) { n % $0 == 0 }
}
```

More efficient function:

```Swift
import func Darwin.sqrt

func sqrt(x:Int) -> Int { return Int(sqrt(Double(x))) }

func properDivs(n: Int) -> [Int] {

    if n == 1 { return [] }

    var result = [Int]()

    for div in filter (1 ... sqrt(n), { n % $0 == 0 }) {

        result.append(div)

        if n/div != div && n/div != n { result.append(n/div) }
    }

    return sorted(result)

}
```

Rest of the task:

```Swift
for i in 1...10 {
    println("\(i): \(properDivs(i))")
}

var (num, max) = (0,0)

for i in 1...20_000 {

    let count = properDivs(i).count
    if (count > max) { (num, max) = (i, count) }
}

println("\(num): \(max)")
```

```txt
1: []
2: [1]
3: [1]
4: [1, 2]
5: [1]
6: [1, 2, 3]
7: [1]
8: [1, 2, 4]
9: [1, 3]
10: [1, 2, 5]
15120: 79

```



## tbas


```vb

dim _proper_divisors(100)

sub proper_divisors(n)
	dim i
	dim _proper_divisors_count = 0
	if n <> 1 then
		for i = 1 to (n \ 2)
			if n %% i = 0 then
				_proper_divisors_count = _proper_divisors_count + 1
				_proper_divisors(_proper_divisors_count) = i
			end if
		next
	end if
	return _proper_divisors_count
end sub

sub show_proper_divisors(n, tabbed)
	dim cnt = proper_divisors(n)
	print str$(n) + ":"; tab(4);"(" + str$(cnt) + " items) ";
	dim j
	for j = 1 to cnt
		if tabbed then
			print str$(_proper_divisors(j)),
		else
			print str$(_proper_divisors(j));
		end if
		if (j < cnt) then print ",";
	next
	print
end sub

dim i
for i = 1 to 10
    show_proper_divisors(i, false)
next

dim c
dim maxindex = 0
dim maxlength = 0
for t = 1 to 20000
	c = proper_divisors(t)
	if c > maxlength then
		maxindex = t
		maxlength = c
	end if
next

print "A maximum at ";
show_proper_divisors(maxindex, false)

```


```txt

>tbas proper_divisors.bas
1:  (0 items)
2:  (1 items) 1
3:  (1 items) 1
4:  (2 items) 1,2
5:  (1 items) 1
6:  (3 items) 1,2,3
7:  (1 items) 1
8:  (3 items) 1,2,4
9:  (2 items) 1,3
10: (3 items) 1,2,5
A maximum at 15120:(79 items) 1,2,3,4,5,6,7,8,9,10,12,14,15,16,18,20,21,24,27,28,30,
35,36,40,42,45,48,54,56,60,63,70,72,80,84,90,105,108,112,120,126,135,
140,144,168,180,189,210,216,240,252,270,280,315,336,360,378,420,432,
504,540,560,630,720,756,840,945,1008,1080,1260,1512,1680,1890,2160,
2520,3024,3780,5040,7560

```



## Tcl

Note that if a number, <math>k</math>, greater than 1 divides <math>n</math> exactly, both <math>k</math> and <math>n/k</math> are
proper divisors. (The raw answers are not sorted; the pretty-printer code sorts.)

```tcl
proc properDivisors {n} {
    if {$n == 1} return
    set divs 1
    for {set i 2} {$i*$i <= $n} {incr i} {
	if {!($n % $i)} {
	    lappend divs $i
	    if {$i*$i < $n} {
		lappend divs [expr {$n / $i}]
	    }
	}
    }
    return $divs
}

for {set i 1} {$i <= 10} {incr i} {
    puts "$i => {[join [lsort -int [properDivisors $i]] ,]}"
}
set maxI [set maxC 0]
for {set i 1} {$i <= 20000} {incr i} {
    set c [llength [properDivisors $i]]
    if {$c > $maxC} {
	set maxI $i
	set maxC $c
    }
}
puts "max: $maxI => (...$maxC…)"
```

```txt

1 => {}
2 => {1}
3 => {1}
4 => {1,2}
5 => {1}
6 => {1,2,3}
7 => {1}
8 => {1,2,4}
9 => {1,3}
10 => {1,2,5}
max: 15120 => (...79...)

```



## VBA


```vb
Public Sub Proper_Divisor()
Dim t() As Long, i As Long, l As Long, j As Long, c As Long
    For i = 1 To 10
        Debug.Print "Proper divisor of " & i & " : " & Join(S(i), ", ")
    Next
    For i = 2 To 20000
        l = UBound(S(i)) + 1
        If l > c Then c = l: j = i
    Next
    Debug.Print "Number in the range 1 to 20,000 with the most proper divisors is : " & j
    Debug.Print j & " count " & c & " proper divisors"
End Sub

Private Function S(n As Long) As String()
'returns the proper divisors of n
Dim j As Long, t() As String, c As Long
    't = list of proper divisor of n
    If n > 1 Then
        For j = 1 To n \ 2
            If n Mod j = 0 Then
                ReDim Preserve t(c)
                t(c) = j
                c = c + 1
            End If
        Next
    End If
    S = t
End Function
```

```txt
Proper divisor of 1 :
Proper divisor of 2 : 1
Proper divisor of 3 : 1
Proper divisor of 4 : 1, 2
Proper divisor of 5 : 1
Proper divisor of 6 : 1, 2, 3
Proper divisor of 7 : 1
Proper divisor of 8 : 1, 2, 4
Proper divisor of 9 : 1, 3
Proper divisor of 10 : 1, 2, 5
Number in the range 1 to 20,000 with the most proper divisors is : 15120
15120 count 79 proper divisors
```



## Visual Basic .NET

```vbnet
Module Module1

    Function ProperDivisors(number As Integer) As IEnumerable(Of Integer)
        Return Enumerable.Range(1, number / 2).Where(Function(divisor As Integer) number Mod divisor = 0)
    End Function

    Sub Main()
        For Each number In Enumerable.Range(1, 10)
            Console.WriteLine("{0}: {{{1}}}", number, String.Join(", ", ProperDivisors(number)))
        Next

        Dim record = Enumerable.Range(1, 20000).Select(Function(number) New With {.Number = number, .Count = ProperDivisors(number).Count()}).OrderByDescending(Function(currentRecord) currentRecord.Count).First()
        Console.WriteLine("{0}: {1}", record.Number, record.Count)
    End Sub

End Module
```

```txt
1: {}
2: {1}
3: {1}
4: {1, 2}
5: {1}
6: {1, 2, 3}
7: {1}
8: {1, 2, 4}
9: {1, 3}
10: {1, 2, 5}
15120: 79
```



## zkl

This is the simple version :

```zkl
fcn properDivs(n){ [1.. (n + 1)/2 + 1].filter('wrap(x){ n%x==0 and n!=x }) }
```

This version is MUCH faster (the output isn't ordered however):

```zkl
fcn properDivs(n){
   if(n==1) return(T);
   ( pd:=[1..(n).toFloat().sqrt()].filter('wrap(x){ n%x==0 }) )
   .pump(pd,'wrap(pd){ if(pd!=1 and (y:=n/pd)!=pd ) y else Void.Skip })
}
```



```zkl
[1..10].apply(properDivs).println();
[1..20_001].apply('wrap(n){ T(properDivs(n).len(),n) })
   .reduce(fcn([(a,_)]ab, [(c,_)]cd){ a>c and ab or cd },T(0,0))
   .println();
```

```txt

L(L(),L(1),L(1),L(1,2),L(1),L(1,2,3),L(1),L(1,2,4),L(1,3),L(1,2,5))
L(79,18480)

```

