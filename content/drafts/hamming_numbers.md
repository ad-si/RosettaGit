+++
title = "Hamming numbers"
description = ""
date = 2019-08-29T02:02:29Z
aliases = []
[extra]
id = 5030
[taxonomies]
categories = []
tags = []
+++

{{task}}
'''[[wp:Hamming numbers|Hamming numbers]]''' are numbers of the form
       <big><big> H  =  2<sup>i</sup> &times; 3<sup>j</sup> &times; 5<sup>k</sup> </big></big>
where
       <big>      i, j, k  ≥  0 </big>

''Hamming numbers''   are also known as   ''ugly numbers''   and also   ''5-smooth numbers''   (numbers whose prime divisors are less or equal to 5).


;Task:
Generate the sequence of Hamming numbers, ''in increasing order''.   In particular:
# Show the   first twenty   Hamming numbers.
# Show the   1691<sup>st</sup>   Hamming number (the last one below   2<sup>31</sup>).
# Show the   one million<sup>th</sup>   Hamming number (if the language – or a convenient library – supports arbitrary-precision integers).


;Related tasks:
* [https://rosettacode.org/wiki/Humble_numbers humble numbers]


;References:
* Wikipedia entry:   [[wp:Hamming numbers|Hamming numbers]]     (this link is re-directed to   '''Regular number''').
* Wikipedia entry:   [[wp:Smooth number|Smooth number]]
* OEIS entry:   [[oeis:A051037|A051037   5-smooth   or   Hamming numbers]]
* [http://dobbscodetalk.com/index.php?option=com_content&task=view&id=913&Itemid=85 Hamming problem] from Dr. Dobb's CodeTalk (dead link as of Sep 2011; parts of the thread [http://drdobbs.com/blogs/architecture-and-design/228700538 here] and [http://www.jsoftware.com/jwiki/Essays/Hamming%20Number here]).





## 360 Assembly


```360asm
*        Hamming numbers           12/03/2017
HAM      CSECT
         USING  HAM,R13            base register
         B      72(R15)            skip savearea
         DC     17F'0'             savearea
         STM    R14,R12,12(R13)    save previous context
         ST     R13,4(R15)         link backward
         ST     R15,8(R13)         link forward
         LR     R13,R15            set addressability
         LA     R6,1               ii=1
       DO WHILE=(C,R6,LE,=F'20')   do ii=1 to 20
         BAL    R14,PRTHAM           call prtham
         LA     R6,1(R6)             ii++
       ENDDO    ,                  enddo ii
         LA     R6,1691            ii=1691
         BAL    R14,PRTHAM         call prtham
         L      R13,4(0,R13)       restore previous savearea pointer
         LM     R14,R12,12(R13)    restore previous context
         XR     R15,R15            rc=0
         BR     R14                exit
PRTHAM   EQU    *             ---- prtham
         ST     R14,R14PRT         save return addr
         LR     R1,R6              ii
         XDECO  R1,XDEC            edit
         MVC    PG+2(4),XDEC+8     output ii
         LR     R1,R6              ii
         BAL    R14,HAMMING        call hamming(ii)
         XDECO  R0,XDEC            edit
         MVC    PG+8(10),XDEC+2    output hamming(ii)
         XPRNT  PG,L'PG            print buffer
         L      R14,R14PRT         restore return addr
         BR     R14           ---- return
HAMMING  EQU    *             ---- hamming(ll)
         ST     R14,R14HAM         save return addr
         ST     R1,LL              ll
         MVC    HH,=F'1'           h(1)=1
         SR     R0,R0              0
         ST     R0,I               i=0
         ST     R0,J               j=0
         ST     R0,K               k=0
         MVC    X2,=F'2'           x2=2
         MVC    X3,=F'3'           x3=3
         MVC    X5,=F'5'           x5=5
         LA     R7,1               n=1
         L      R2,LL              ll
         BCTR   R2,0               -1
         ST     R2,LLM1            ll-1
       DO WHILE=(C,R7,LE,LLM1)     do n=1 to ll-1
         L      R4,X2                m=x2
       IF C,R4,GT,X3 THEN            if m>x3 then
         L      R4,X3                  m=x3
       ENDIF    ,                    endif
       IF C,R4,GT,X5 THEN            if m>x5 then
         L      R4,X5                  m=x5
       ENDIF    ,                    endif
         LR     R1,R7                n
         SLA    R1,2                 *4
         ST     R4,HH(R1)            h(n+1)=m
       IF C,R4,EQ,X2 THEN            if m=x2 then
         L      R1,I                   i
         LA     R1,1(R1)               i+1
         ST     R1,I                   i=i+1
         SLA    R1,2                   *4
         L      R2,HH(R1)              h(i+1)
         MH     R2,=H'2'               *2
         ST     R2,X2                  x2=2*h(i+1)
       ENDIF    ,                    endif
       IF C,R4,EQ,X3 THEN            if m=x3 then
         L      R1,J                   j
         LA     R1,1(R1)               j+1
         ST     R1,J                   j=j+1
         SLA    R1,2                   *4
         L      R2,HH(R1)              h(j+1)
         MH     R2,=H'3'               *3
         ST     R2,X3                  x3=3*h(j+1)
       ENDIF    ,                    endif
       IF C,R4,EQ,X5 THEN            if m=x5 then
         L      R1,K                   k
         LA     R1,1(R1)               k+1
         ST     R1,K                   k=k+1
         SLA    R1,2                   *4
         L      R2,HH(R1)              h(k+1)
         MH     R2,=H'5'               *5
         ST     R2,X5                  x5=5*h(k+1)
       ENDIF    ,                    endif
         LA     R7,1(R7)             n++
       ENDDO    ,                  enddo n
         L      R1,LL              ll
         SLA    R1,2               *4
         L      R0,HH-4(R1)        return h(ll)
         L      R14,R14HAM         restore return addr
         BR     R14           ---- return
R14HAM   DS     A                  return addr of hamming
R14PRT   DS     A                  return addr of print
LL       DS     F                  ll
LLM1     DS     F                  ll-1
I        DS     F                  i
J        DS     F                  j
K        DS     F                  k
X2       DS     F                  x2
X3       DS     F                  x3
X5       DS     F                  x5
PG       DC     CL80'H(xxxx)=xxxxxxxxxx'
XDEC     DS     CL12               temp
         LTORG                     positioning literal pool
HH       DS     1691F              array h(1691)
         YREGS
         END    HAM
```

{{out}}

```txt

H(   1)=         1
H(   2)=         2
H(   3)=         3
H(   4)=         4
H(   5)=         5
H(   6)=         6
H(   7)=         8
H(   8)=         9
H(   9)=        10
H(  10)=        12
H(  11)=        15
H(  12)=        16
H(  13)=        18
H(  14)=        20
H(  15)=        24
H(  16)=        25
H(  17)=        27
H(  18)=        30
H(  19)=        32
H(  20)=        36
H(1691)=2125764000

```




## Ada

{{works with|GNAT}}
GNAT provides the datatypes Integer, Long_Integer and Long_Long_Integer, which are not large enough to store hamming numbers. In this program, we represent them as the factors for each of the prime numbers 2, 3 and 5, and only convert them to a base-10 numbers for display. We use the gmp library binding part of GNATCOLL, though a simple 'pragma import' would be enough.

This version is very fast (20ms for the million-th hamming number), thanks to a good algorithm. We also do not manipulate large numbers directly (gmp lib), but only the factors of the prime.

It will fail to compute the billion'th number because we use an array of the stack to store all numbers. It is possible to get rid of this array though it will make the code slightly less readable.


```Ada

with Ada.Numerics.Generic_Elementary_Functions;
with Ada.Text_IO; use Ada.Text_IO;
with GNATCOLL.GMP.Integers;
with GNATCOLL.GMP.Lib;

procedure Hamming is

   type Log_Type is new Long_Long_Float;
   package Funcs is new Ada.Numerics.Generic_Elementary_Functions (Log_Type);

   type Factors_Array is array (Positive range <>) of Positive;

   generic
      Factors : Factors_Array := (2, 3, 5);
      --  The factors for smooth numbers. Hamming numbers are 5-smooth.
   package Smooth_Numbers is
      type Number is private;
      function Compute (Nth : Positive) return Number;
      function Image (N : Number) return String;

   private
      type Exponent_Type is new Natural;
      type Exponents_Array is array (Factors'Range) of Exponent_Type;
      --  Numbers are stored as the exponents of the prime factors.

      type Number is record
         Exponents : Exponents_Array;
         Log       : Log_Type;
         --  The log of the value, used to ease sorting.
      end record;

      function "=" (N1, N2 : Number) return Boolean
        is (for all F in Factors'Range => N1.Exponents (F) = N2.Exponents (F));
   end Smooth_Numbers;

   package body Smooth_Numbers is
      One : constant Number := (Exponents => (others => 0), Log => 0.0);
      Factors_Log : array (Factors'Range) of Log_Type;

      function Image (N : Number) return String is
         use GNATCOLL.GMP.Integers, GNATCOLL.GMP.Lib;
         R, Tmp : Big_Integer;
      begin
         Set (R, "1");
         for F in Factors'Range loop
            Set (Tmp, Factors (F)'Image);
            Raise_To_N (Tmp, GNATCOLL.GMP.Unsigned_Long (N.Exponents (F)));
            Multiply (R, Tmp);
         end loop;
         return Image (R);
      end Image;

      function Compute (Nth : Positive) return Number is
         Candidates : array (Factors'Range) of Number;

         Values     : array (1 .. Nth) of Number;
         --  Will result in Storage_Error for very large values of Nth

         Indices    : array (Factors'Range) of Natural :=
            (others => Values'First);
         Current    : Number;
         Tmp        : Number;
      begin
         for F in Factors'Range loop
            Factors_Log (F) := Funcs.Log (Log_Type (Factors (F)));
            Candidates (F) := One;
            Candidates (F).Exponents (F) := 1;
            Candidates (F).Log := Factors_Log (F);
         end loop;

         Values (1) := One;

         for Count in 2 .. Nth loop
            --  Find next value (the lowest of the candidates)
            Current := Candidates (Factors'First);
            for F in Factors'First + 1 .. Factors'Last loop
               if Candidates (F).Log < Current.Log then
                  Current := Candidates (F);
               end if;
            end loop;

            Values (Count) := Current;

            --  Update the candidates. There might be several candidates with
            --  the same value
            for F in Factors'Range loop
               if Candidates (F) = Current then
                  Indices (F) := Indices (F) + 1;

                  Tmp := Values (Indices (F));
                  Tmp.Exponents (F) := Tmp.Exponents (F) + 1;
                  Tmp.Log := Tmp.Log + Factors_Log (F);

                  Candidates (F) := Tmp;
               end if;
            end loop;
         end loop;

         return Values (Nth);
      end Compute;
   end Smooth_Numbers;

   package Hamming is new Smooth_Numbers ((2, 3, 5));

begin
   for N in 1 .. 20 loop
      Put (" " & Hamming.Image (Hamming.Compute (N)));
   end loop;
   New_Line;

   Put_Line (Hamming.Image (Hamming.Compute (1691)));
   Put_Line (Hamming.Image (Hamming.Compute (1_000_000)));
end Hamming;

```

{{out}}

```txt

 1 2 3 4 5 6 8 9 10 12 15 16 18 20 24 25 27 30 32 36
2125764000
519312780448388736089589843750000000000000000000000000000000000000000000000000000000

```



## ALGOL 68

{{works with|Algol 68 Genie 1.19.0}}
Hamming numbers are generated in a trivial iterative way as in the Python version below. This program keeps the series needed to generate the numbers as short as possible using flexible rows; on the downside, it spends considerable time on garbage collection.

```algol68
PR precision=100 PR

MODE SERIES = FLEX [1 : 0] UNT, # Initially, no elements #
     UNT = LONG LONG INT; # A 100-digit unsigned integer #

PROC hamming number = (INT n) UNT: # The n-th Hamming number #
     CASE n
     IN 1, 2, 3, 4, 5, 6, 8, 9, 10, 12 # First 10 in a table #
     OUT # Additional operators #
         OP MIN = (INT i, j) INT: (i < j | i | j), MIN = (UNT i, j) UNT: (i < j | i | j);
         PRIO MIN = 9;
         OP LAST = (SERIES h) UNT: h[UPB h]; # Last element of a series #
         OP +:= = (REF SERIES s, UNT elem) VOID:
            # Extend a series by one element, only keep the elements you need #
            (INT lwb = (i MIN j) MIN k, upb = UPB s;
             REF SERIES new s = HEAP FLEX [lwb : upb + 1] UNT;
             (new s[lwb : upb] := s[lwb : upb], new s[upb + 1] := elem);
             s := new s
            );
         # Determine the n-th hamming number iteratively #
         SERIES h := 1, # Series, initially one element #
         UNT m2 := 2, m3 := 3, m5 := 5, # Multipliers #
         INT i := 1, j := 1, k := 1; # Counters #
         TO n - 1
         DO h +:= (m2 MIN m3) MIN m5;
            (LAST h = m2 | m2 := 2 * h[i +:= 1]);
            (LAST h = m3 | m3 := 3 * h[j +:= 1]);
            (LAST h = m5 | m5 := 5 * h[k +:= 1])
         OD;
         LAST h
     ESAC;

FOR k TO 20
DO print ((whole (hamming number (k), 0), blank))
OD;
print ((newline, whole (hamming number (1 691), 0)));
print ((newline, whole (hamming number (1 000 000), 0)))
```

{{out}}

```txt

1 2 3 4 5 6 8 9 10 12 15 16 18 20 24 25 27 30 32 36
2125764000
519312780448388736089589843750000000000000000000000000000000000000000000000000000000

```



## ALGOL W

Algol W only has 32 bit integers, so we just show the first 20 Hamming Numbers and Hamming number 1691.


This uses the algorithm from Dr Dobbs (as in the Python version). The Coffee Script solution has some notes on how it works.

```algolw
begin
    % returns the minimum of a and b                    %
    integer procedure min ( integer value a, b ) ; if a < b then a else b;
    % find and print Hamming Numbers                        %
    % Algol W only supports 32-bit integers so we just find %
    % the 1691 32-bit Hamming Numbers                       %
    integer MAX_HAMMING;
    MAX_HAMMING := 1691;
    begin
        integer array H( 1 :: MAX_HAMMING );
        integer p2, p3, p5, last2, last3, last5;
        H( 1 ) := 1;
        last2  := last3 := last5 := 1;
        p2     := 2;
        p3     := 3;
        p5     := 5;
        for hPos := 2 until MAX_HAMMING do begin
            integer m;
            % the next Hamming number is the lowest of the next multiple of 2, 3, and 5 %
            m := min( min( p2, p3 ), p5 );
            H( hPos ) := m;
            if m = p2 then begin
                last2 := last2 + 1;
                p2    := 2 * H( last2 )
            end if_used_power_of_2 ;
            if m = p3 then begin
                last3 := last3 + 1;
                p3    := 3 * H( last3 )
            end if_used_power_of_3 ;
            if m = p5 then begin
                last5 := last5 + 1;
                p5    := 5 * H( last5 )
            end if_used_power_of_5 ;
        end for_hPos ;
        i_w := 1;
        s_w := 1;
        write( H( 1 ) );
        for i := 2 until 20 do writeon( H( i ) );
        write( H( MAX_HAMMING ) )
    end
end.
```

{{out}}

```txt

1 2 3 4 5 6 8 9 10 12 15 16 18 20 24 25 27 30 32 36
2125764000

```



## ATS


```ATS

//
// How to compile:
// patscc -DATS_MEMALLOC_LIBC -o hamming hamming.dats
//
#include
"share/atspre_staload.hats"

fun
min3
(
  A: arrayref(int, 3)
) : natLt(3) = i where
{
  var x: int = A[0]
  var i: natLt(3) = 0
  val () = if A[1] < x then (x := A[1]; i := 1)
  val () = if A[2] < x then (x := A[2]; i := 2)
} (* end of [min3] *)

fun
hamming
{n:pos}
(
  n: int(n)
) : int = let
//
var A = @[int](2, 3, 5)
val A = $UNSAFE.cast{arrayref(int, 3)}(addr@A)
var I = @[int](1, 1, 1)
val I = $UNSAFE.cast{arrayref(int, 3)}(addr@I)
val H = arrayref_make_elt<int> (i2sz(succ(n)), 0)
val () = H[0] := 1
//
fun
loop{k:pos}
  (k: int(k)) : void =
(
//
if
k < n
then let
  val i = min3(A)
  val k =
  (
    if A[i] > H[k-1] then (H[k] := A[i]; k+1) else k
  ) : intBtwe(k, k+1)
  val ii = I[i]
  val () = I[i] := ii+1
  val ii = $UNSAFE.cast{natLte(n)}(ii)
  val () = if i = 0 then A[i] := 2*H[ii]
  val () = if i = 1 then A[i] := 3*H[ii]
  val () = if i = 2 then A[i] := 5*H[ii]
in
  loop(k)
end // end of [then]
else () // end of [else]
//
) (* end of [loop] *)
//
in
  loop (1); H[n-1]
end (* end of [hamming] *)

implement
main0 () =
{
val () =
loop(1) where
{
fun
loop
{n:pos}
(
  n: int(n)
) : void =
if
n <= 20
then let
  val () =
  println! ("hamming(",n,") = ", hamming(n))
in
  loop(n+1)
end // end of [then]
// end of [if]
} (* end of [val] *)
val n = 1691
val () = println! ("hamming(",n,") = ", hamming(n))
//
} (* end of [main0] *)

```

{{out}}

```txt

hamming(1) = 1
hamming(2) = 2
hamming(3) = 3
hamming(4) = 4
hamming(5) = 5
hamming(6) = 6
hamming(7) = 8
hamming(8) = 9
hamming(9) = 10
hamming(10) = 12
hamming(11) = 15
hamming(12) = 16
hamming(13) = 18
hamming(14) = 20
hamming(15) = 24
hamming(16) = 25
hamming(17) = 27
hamming(18) = 30
hamming(19) = 32
hamming(20) = 36
hamming(1691) = 2125764000

```



## AutoHotkey


```AutoHotKey
SetBatchLines, -1
Msgbox % hamming(1,20)
Msgbox % hamming(1690)
return

hamming(first,last=0)
{
	if (first < 1)
		ans=ERROR

	if (last = 0)
		last := first

	i:=0, j:=0, k:=0

	num1 := ceil((last * 20)**(1/3))
	num2 := ceil(num1 * ln(2)/ln(3))
	num3 := ceil(num1 * ln(2)/ln(5))

	loop
	{
		H := (2**i) * (3**j) * (5**k)
		if (H > 0)
			ans = %H%`n%ans%
		i++
		if (i > num1)
		{
			i=0
			j++
			if (j > num2)
			{
				j=0
				k++
			}
		}
		if (k > num3)
			break
	}
	Sort ans, N

	Loop, parse, ans, `n, `r
	{
		if (A_index > last)
			break
		if (A_index < first)
			continue
		Output = %Output%`n%A_LoopField%
	}

	return Output
}
```



## AWK


```AWK

# syntax: gawk -M -f hamming_numbers.awk
BEGIN {
    for (i=1; i<=20; i++) {
      printf("%d ",hamming(i))
    }
    printf("\n1691: %d\n",hamming(1691))
    printf("\n1000000: %d\n",hamming(1000000))
    exit(0)
}
function hamming(limit,  h,i,j,k,n,x2,x3,x5) {
    h[0] = 1
    x2 = 2
    x3 = 3
    x5 = 5
    for (n=1; n<=limit; n++) {
      h[n] = min(x2,min(x3,x5))
      if (h[n] == x2) { x2 = 2 * h[++i] }
      if (h[n] == x3) { x3 = 3 * h[++j] }
      if (h[n] == x5) { x5 = 5 * h[++k] }
    }
    return(h[limit-1])
}
function min(x,y) {
    return((x < y) ? x : y)
}

```

{{out}}

```txt

1 2 3 4 5 6 8 9 10 12 15 16 18 20 24 25 27 30 32 36
1691: 2125764000
1000000: 519312780448388736089589843750000000000000000000000000000000000000000000000000000000

```



## BBC BASIC


```bbcbasic
      @% = &1010
      FOR h% = 1 TO 20
        PRINT "H("; h% ") = "; FNhamming(h%)
      NEXT
      PRINT "H(1691) = "; FNhamming(1691)
      END

      DEF FNhamming(l%)
      LOCAL i%, j%, k%, n%, m, x2, x3, x5, h%()
      DIM h%(l%) : h%(0) = 1
      x2 = 2 : x3 = 3 : x5 = 5
      FOR n% = 1 TO l%-1
        m = x2
        IF m > x3 m = x3
        IF m > x5 m = x5
        h%(n%) = m
        IF m = x2 i% += 1 : x2 = 2 * h%(i%)
        IF m = x3 j% += 1 : x3 = 3 * h%(j%)
        IF m = x5 k% += 1 : x5 = 5 * h%(k%)
      NEXT
      = h%(l%-1)
```

{{out}}

```txt

H(1) = 1
H(2) = 2
H(3) = 3
H(4) = 4
H(5) = 5
H(6) = 6
H(7) = 8
H(8) = 9
H(9) = 10
H(10) = 12
H(11) = 15
H(12) = 16
H(13) = 18
H(14) = 20
H(15) = 24
H(16) = 25
H(17) = 27
H(18) = 30
H(19) = 32
H(20) = 36
H(1691) = 2125764000

```



## BC


```bc
cat hamming_numbers.bc
define min(x,y) {
 if (x < y) {
	return x
 } else {
	return y
 }
}
define hamming(limit) {
 i = 0
 j = 0
 k = 0
 h[0] = 1
 x2 = 2
 x3 = 3
 x5 = 5
 for (n=1; n<=limit; n++) {
  h[n] = min(x2,min(x3,x5))
  if (h[n] == x2) { x2 = 2 * h[++i] }
  if (h[n] == x3) { x3 = 3 * h[++j] }
  if (h[n] == x5) { x5 = 5 * h[++k] }
 }
 return (h[limit-1])
}
for (lab=1; lab<=20; lab++) {
 hamming(lab)
}
hamming(1691)
hamming(1000000)
quit

```


{{out}}

```txt

$ bc hamming_numbers.bc
bc 1.06.95
Copyright 1991-1994, 1997, 1998, 2000, 2004, 2006 Free Software Foundation, Inc.
This is free software with ABSOLUTELY NO WARRANTY.
For details type `warranty'.
1
2
3
4
5
6
8
9
10
12
15
16
18
20
24
25
27
30
32
36
2125764000
51931278044838873608958984375000000000000000000000000000000000000000\
0000000000000000

```



## Bracmat

{{trans|D}}

```bracmat
( ( hamming
  =   x2 x3 x5 n i j k min
    .   tbl$(h,!arg)        { This creates an array. Arrays are always global in Bracmat. }
      & 1:?(0$h)
      & 2:?x2
      & 3:?x3
      & 5:?x5
      & 0:?n:?i:?j:?k
      &   whl
        ' ( !n+1:<!arg:?n
          & !x2:?min
          & (!x3:<!min:?min|)
          & (!x5:<!min:?min|)
          & !min:?(!n$h)               { !n is index into array h }
          & (   !x2:!min
              & 2*!((1+!i:?i)$h):?x2
            |
            )
          & (   !x3:!min
              & 3*!((1+!j:?j)$h):?x3
            |
            )
          & (   !x5:!min
              & 5*!((1+!k:?k)$h):?x5
            |
            )
          )
      & !((!arg+-1)$h) (tbl$(h,0)&)    { We delete the array by setting its size to 0 }
  )
& 0:?I
& whl'(!I+1:~>20:?I&put$(hamming$!I " "))
& out$
& out$(hamming$1691)
& out$(hamming$1000000)
);
```

{{out}}

```txt
1  2  3  4  5  6  8  9  10  12  15  16  18  20  24  25  27  30  32  36
2125764000
519312780448388736089589843750000000000000000000000000000000000000000000000000000000
```



## C

Using a min-heap to keep track of numbers.  Does not handle big integers.

```c
#include <stdio.h>
#include <stdlib.h>

typedef unsigned long long ham;

size_t alloc = 0, n = 1;
ham *q = 0;

void qpush(ham h)
{
	int i, j;
	if (alloc <= n) {
		alloc = alloc ? alloc * 2 : 16;
		q = realloc(q, sizeof(ham) * alloc);
	}

	for (i = n++; (j = i/2) && q[j] > h; q[i] = q[j], i = j);
	q[i] = h;
}

ham qpop()
{
	int i, j;
	ham r, t;
	/* outer loop for skipping duplicates */
	for (r = q[1]; n > 1 && r == q[1]; q[i] = t) {
		/* inner loop is the normal down heap routine */
		for (i = 1, t = q[--n]; (j = i * 2) < n;) {
			if (j + 1 < n && q[j] > q[j+1]) j++;
			if (t <= q[j]) break;
			q[i] = q[j], i = j;
		}
	}

	return r;
}

int main()
{
	int i;
	ham h;

	for (qpush(i = 1); i <= 1691; i++) {
		/* takes smallest value, and queue its multiples */
		h = qpop();
		qpush(h * 2);
		qpush(h * 3);
		qpush(h * 5);

		if (i <= 20 || i == 1691)
			printf("%6d: %llu\n", i, h);
	}

	/* free(q); */
	return 0;
}
```


### Alternative

Standard algorithm.  Numbers are stored as exponents of factors instead of big integers, while GMP is only used for display.  It's much more efficient this way.

```c
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <gmp.h>

/* number of factors.  best be mutually prime -- duh. */
#define NK 3
#define MAX_HAM (1 << 24)
#define MAX_POW 1024
int n_hams = 0, idx[NK] = {0}, fac[] = { 2, 3, 5, 7, 11};

/*  k-smooth numbers are stored as their exponents of each factor;
    v is the log of the number, for convenience. */
typedef struct {
	int e[NK];
	double v;
} ham_t, *ham;

ham_t *hams, values[NK] = {{{0}, 0}};
double inc[NK][MAX_POW];

/* most of the time v can be just incremented, but eventually
 * floating point precision will bite us, so better recalculate */
inline
void _setv(ham x) {
	int i;
	for (x->v = 0, i = 0; i < NK; i++)
		x->v += inc[i][x->e[i]];
}

inline
int _eq(ham a, ham b) {
	int i;
	for (i = 0; i < NK && a->e[i] == b->e[i]; i++);

	return i == NK;
}

ham get_ham(int n)
{
	int i, ni;
	ham h;

	n--;
	while (n_hams < n) {
		for (ni = 0, i = 1; i < NK; i++)
			if (values[i].v < values[ni].v)
				ni = i;

		*(h = hams + ++n_hams) = values[ni];

		for (ni = 0; ni < NK; ni++) {
			if (! _eq(values + ni, h)) continue;
			values[ni] = hams[++idx[ni]];
			values[ni].e[ni]++;
			_setv(values + ni);
		}
	}

	return hams + n;
}

void show_ham(ham h)
{
	static mpz_t das_ham, tmp;
	int i;

 	mpz_init_set_ui(das_ham, 1);
	mpz_init_set_ui(tmp, 1);
	for (i = 0; i < NK; i++) {
		mpz_ui_pow_ui(tmp, fac[i], h->e[i]);
		mpz_mul(das_ham, das_ham, tmp);
	}
	gmp_printf("%Zu\n", das_ham);
}

int main()
{
	int i, j;
	hams = malloc(sizeof(ham_t) * MAX_HAM);

	for (i = 0; i < NK; i++) {
		values[i].e[i] = 1;
		inc[i][1] = log(fac[i]);
		_setv(values + i);

		for (j = 2; j < MAX_POW; j++)
			inc[i][j] = j * inc[i][1];
	}

	printf("     1,691: "); show_ham(get_ham(1691));
	printf(" 1,000,000: "); show_ham(get_ham(1e6));
	printf("10,000,000: "); show_ham(get_ham(1e7));
	return 0;
}
```

{{out}}

```txt
     1,691: 2125764000
 1,000,000: 519312780448388736089589843750000000000000000000000000000000000000000000000000000000
10,000,000: 16244105063830431823239 ..<a gadzillion digits>.. 000000000000000000000
```


## C#
{{trans|D}}

```c#
using System;
using System.Numerics;
using System.Linq;

namespace Hamming {

    class MainClass {

        public static BigInteger Hamming(int n) {
            BigInteger two = 2, three = 3, five = 5;
            var h = new BigInteger[n];
            h[0] = 1;
            BigInteger x2 = 2, x3 = 3, x5 = 5;
            int i = 0, j = 0, k = 0;

            for (int index = 1; index < n; index++) {
                h[index] = BigInteger.Min(x2, BigInteger.Min(x3, x5));
                if (h[index] == x2) x2 = two * h[++i];
                if (h[index] == x3) x3 = three * h[++j];
                if (h[index] == x5) x5 = five * h[++k];
            }
            return h[n - 1];
        }

        public static void Main(string[] args) {
            Console.WriteLine(string.Join(" ", Enumerable.Range(1, 20).ToList().Select(x => Hamming(x))));
            Console.WriteLine(Hamming(1691));
            Console.WriteLine(Hamming(1000000));
        }
    }
}
```

{{out}}

```txt
1 2 3 4 5 6 8 9 10 12 15 16 18 20 24 25 27 30 32 36
2125764000
519312780448388736089589843750000000000000000000000000000000000000000000000000000000
```



### Generic version for any set of numbers

The algorithm is similar to the one above.

```c#
using System;
using System.Numerics;
using System.Linq;

namespace Hamming {

    class MainClass {

        public static BigInteger[] Hamming(int n, int[] a) {
            var primes = a.Select(x => (BigInteger)x).ToArray();
            var values = a.Select(x => (BigInteger)x).ToArray();
            var indexes = new int[a.Length];
            var results = new BigInteger[n];
            results[0] = 1;
            for (int iter = 1; iter < n; iter++) {
                results[iter] = values[0];
                for (int p = 1; p < primes.Length; p++)
                    if (results[iter] > values[p])
                        results[iter] = values[p];
                for (int p = 0; p < primes.Length; p++)
                    if (results[iter] == values[p])
                        values[p] = primes[p] * results[++indexes[p]];
            }
            return results;
        }

        public static void Main(string[] args) {
            foreach (int[] primes in new int[][] { new int[] {2,3,5}, new int[] {2,3,5,7} }) {
                Console.WriteLine("{0}-Smooth:", primes.Last());
                Console.WriteLine(string.Join(" ", Hamming(20, primes)));
                Console.WriteLine(Hamming(1691, primes).Last());
                Console.WriteLine(Hamming(1000000, primes).Last());
                Console.WriteLine();
            }
        }
    }
}
```


{{out}}

```txt
5-Smooth:
1 2 3 4 5 6 8 9 10 12 15 16 18 20 24 25 27 30 32 36
2125764000
519312780448388736089589843750000000000000000000000000000000000000000000000000000000

7-Smooth:
1 2 3 4 5 6 7 8 9 10 12 14 15 16 18 20 21 24 25 27
3317760
4157409948433216829957008507500000000
```



### Fast version


Like some of the other implementations on this page, this version represents each number as
a list of exponents which would be applied to each prime number. So the number 60 would be
represented as int[3] { 2, 1, 1 } which is interpreted as 2^2 * 3^1 * 5^1.

As often happens, optimizing for speed caused a marked increase in code size and complexity.
Clearly the versions I wrote above are easier to read & understand. They were also much quicker
to write. But the generic version above runs in 3+ seconds for the 1000000th 5-smooth number
whereas this version does it in 0.35 seconds, 8-10 times faster.

I've tried to comment it as best I could, without bloating the code too much.

--Mike Lorenz


```c#
using System;
using System.Linq;
using System.Numerics;

namespace HammingFast {

    class MainClass {

        private static int[] _primes = { 2, 3, 5, 7, 11, 13, 17, 19, 23, 29 };

        public static BigInteger Big(int[] exponents) {
            BigInteger val = 1;
            for (int i = 0; i < exponents.Length; i++)
                for (int e = 0; e < exponents[i]; e++)
                    val = val * _primes[i];
            return val;
        }

        public static int[] Hamming(int n, int nprimes) {
            var hammings  = new int[n, nprimes];                    // array of hamming #s we generate
            var hammlogs  = new double[n];                          // log values for above
            var primelogs = new double[nprimes];                    // pre-calculated prime log values
            var indexes   = new int[nprimes];                       // intermediate hamming values as indexes into hammings
            var listheads = new int[nprimes, nprimes];              // intermediate hamming list heads
            var listlogs  = new double[nprimes];                    // log values of list heads
            for (int p = 0; p < nprimes; p++) {
                listheads[p, p] = 1;                                // init list heads to prime values
                primelogs[p]    = Math.Log(_primes[p]);             // pre-calc prime log values
                listlogs[p]     = Math.Log(_primes[p]);             // init list head log values
            }
            for (int iter = 1; iter < n; iter++) {
                int min = 0;                                        // find index of min item in list heads
                for (int p = 1; p < nprimes; p++)
                    if (listlogs[p] < listlogs[min])
                        min = p;
                hammlogs[iter] = listlogs[min];                     // that's the next hamming number
                for (int i = 0; i < nprimes; i++)
                    hammings[iter, i] = listheads[min, i];
                for (int p = 0; p < nprimes; p++) {                 // update each list head if it matches new value
                    bool equal = true;                              // test each exponent to see if number matches
                    for (int i = 0; i < nprimes; i++) {
                        if (hammings[iter, i] != listheads[p, i]) {
                            equal = false;
                            break;
                        }
                    }
                    if (equal) {                                    // if it matches...
                        int x = ++indexes[p];                       // set index to next hamming number
                        for (int i = 0; i < nprimes; i++)           // copy each hamming exponent
                            listheads[p, i] = hammings[x, i];
                        listheads[p, p] += 1;                       // increment exponent = mult by prime
                        listlogs[p] = hammlogs[x] + primelogs[p];   // add log(prime) to log(value) = mult by prime
                    }
                }
            }

            var result = new int[nprimes];
            for (int i = 0; i < nprimes; i++)
                result[i] = hammings[n - 1, i];
            return result;
        }

        public static void Main(string[] args) {
            foreach (int np in new int[] { 3, 4, 5 }) {
                Console.WriteLine("{0}-Smooth:", _primes[np - 1]);
                Console.WriteLine(string.Join(" ", Enumerable.Range(1, 20).Select(x => Big(Hamming(x, np)))));
                Console.WriteLine(Big(Hamming(1691, np)));
                Console.WriteLine(Big(Hamming(1000000, np)));
                Console.WriteLine();
            }
        }
    }
}
```


{{out}}

```txt
5-Smooth:
1 2 3 4 5 6 8 9 10 12 15 16 18 20 24 25 27 30 32 36
2125764000
519312780448388736089589843750000000000000000000000000000000000000000000000000000000

7-Smooth:
1 2 3 4 5 6 7 8 9 10 12 14 15 16 18 20 21 24 25 27
3317760
4157409948433216829957008507500000000

11-Smooth:
1 2 3 4 5 6 7 8 9 10 11 12 14 15 16 18 20 21 22 24
296352
561912530929780078125000
```



### C# Enumerator Version

I wanted to fix the enumerator (old) version, as it wasn't working.  It became a bit of an obsession... after a few iterations I came up with the following, which is the fastest C# version on my computer - your mileage may vary.  It combines the speed of the Log method; Log(2)+Log(3)=Log(2*3) to help determine which is the next one to use. Then I have added some logic (using the series property) to ensure that exponent sets are never duplicated - which speeds the calculations up a bit.... Adding this trick to the Fast Version will probably result in the fastest version, but I'll leave that to someone else to implement.  Finally it's all enumerated through a crazy one-way-linked-list-type-structure that only exists as long as the enumerator and is left up to the garbage collector to remove the bits no longer needed... I hope it's commented enough... follow it if you dare!


```c#
using System;
using System.Collections.Generic;
using System.Linq;
using System.Numerics;

namespace HammingTest
{
    class HammingNode
    {
        public double log;
        public int[] exponents;
        public HammingNode next;
        public int series;
    }

    class HammingListEnumerator : IEnumerable<BigInteger>
    {
        private int[] primes;
        private double[] primelogs;
        private HammingNode next;
        private HammingNode[] values;
        private HammingNode[] indexes;

        public HammingListEnumerator(IEnumerable<int> seeds)
        {
            // Ensure our seeds are properly ordered, and generate their log values
            primes = seeds.OrderBy(x => x).ToArray();
            primelogs = primes.Select(x => Math.Log10(x)).ToArray();
            // Start at 1 (log(1)=0, exponents are all 0, series = none)
            next = new HammingNode { log = 0, exponents = new int[primes.Length], series = primes.Length };
            // Set all exponent sequences to the start, and calculate the first value for each exponent
            indexes = new HammingNode[primes.Length];
            values = new HammingNode[primes.Length];
            for(int i = 0; i < primes.Length; ++i)
            {
                indexes[i] = next;
                values[i] = AddExponent(next, i);
            }
        }

        // Make a copy of a node, and increment the specified exponent value
        private HammingNode AddExponent(HammingNode node, int i)
        {
            HammingNode ret = new HammingNode { log = node.log + primelogs[i], exponents = (int[])node.exponents.Clone(), series = i };
            ++ret.exponents[i];
            return ret;
        }

        private void GetNext()
        {
            // Find which exponent value is the lowest
            int min = 0;
            for(int i = 1; i < values.Length; ++i)
                if(values[i].log < values[min].log)
                    min = i;

            // Add it to the end of the 'list', and move to it
            next.next = values[min];
            next = values[min];

            // Find the next node in an allowed sequence (skip those that would be duplicates)
            HammingNode val = indexes[min].next;
            while(val.series < min)
                val = val.next;

            // Keep the current index, and calculate the next value in the series for that exponent
            indexes[min] = val;
            values[min] = AddExponent(val, min);
        }

        // Skip values without having to calculate the BigInteger value from the exponents
        public HammingListEnumerator Skip(int count)
        {
            for(int i = count; i > 0; --i)
                GetNext();

            return this;
        }

        // Calculate the BigInteger value from the exponents
        internal BigInteger ValueOf(HammingNode n)
        {
            BigInteger val = 1;
            for(int i = 0; i < n.exponents.Length; ++i)
                for(int e = 0; e < n.exponents[i]; e++)
                    val = val * primes[i];
            return val;
        }

        public IEnumerator<BigInteger> GetEnumerator()
        {
            while(true)
            {
                yield return ValueOf(next);
                GetNext();
            }
        }

        System.Collections.IEnumerator System.Collections.IEnumerable.GetEnumerator()
        {
            return this.GetEnumerator();
        }
    }

    class Program
    {
        static void Main(string[] args)
        {
            foreach(int[] primes in new int[][] {
                new int[] { 2, 3, 5 },
                new int[] { 2, 3, 5, 7 },
                new int[] { 2, 3, 5, 7, 9}})
            {
                HammingListEnumerator hammings = new HammingListEnumerator(primes);
                System.Diagnostics.Debug.WriteLine("{0}-Smooth:", primes.Last());
                System.Diagnostics.Debug.WriteLine(String.Join(" ", hammings.Take(20).ToArray()));
                System.Diagnostics.Debug.WriteLine(hammings.Skip(1691 - 20).First());
                System.Diagnostics.Debug.WriteLine(hammings.Skip(1000000 - 1691).First());
                System.Diagnostics.Debug.WriteLine("");
            }
        }
    }
}

```

{{out}}

```txt
5-Smooth:
1 2 3 4 5 6 8 9 10 12 15 16 18 20 24 25 27 30 32 36
2125764000
519312780448388736089589843750000000000000000000000000000000000000000000000000000000

7-Smooth:
1 2 3 4 5 6 7 8 9 10 12 14 15 16 18 20 21 24 25 27
3317760
4157409948433216829957008507500000000

11-Smooth:
1 2 3 4 5 6 7 8 9 10 11 12 14 15 16 18 20 21 22 24
296352
561912530929780078125000
```


'''Alternate Generic Enumerating version'''

YMMV, but unlike the author of the above code, I found the above version to be much slower on my machine than the "Generic version".  The following version is actually just a little slower than the Generic version but uses much less memory due to avoiding duplicates and only keeping in memory those "lazy list" streams necessary for calculation from 1/5 of the current range to 1/2 (for Smooth-5 numbers), and not successive values in those ranges but only the values the are the multiples of previous ranges.  Like the Haskell code from which it is translated, the head of the streams is not retained so can be garbage collected when no longer necessary and it is recommended that the primes be processed in reverse order so that the least dense streams are processed first for slightly less memory use and operations.

It also shows that one can use somewhat functional programming techniques in C#.

The class implements its own partial version of a lazy list using the Lazy class and uses lambda closures for the recursive use of the successive streams to avoid stack use.  It uses Aggregate to implement the Haskell "foldl" function.  The code demonstrates that even though C# is primarily imperative in paradigm, with its ability to implement closures using delegates/lambdas, it can express some algorithms such as this mostly functionally.

It isn't nearly as fast as a Haskell, Scala or even Clojure and Scheme (GambitC) versions of this algorithm, being about five times slower is primarily due to its use of many small heap based instances of classes, both for the LazyList's and for closures (implemented using at least one class to hold the captured free variables) and the inefficiency of DotNet's allocation and garbage collection of many small instance objects (although about twice as fast as F#'s implementation, whose closures must require even more small object instances); it seems Haskell and the (Java) JVM are much more efficient at doing these allocations/garbage collections for many small objects.  The slower speed to a relatively minor extent is also due to less efficient BigInteger operations:
{{trans|Haskell}}

```c#
using System;
using System.Collections;
using System.Collections.Generic;
using System.Linq;
using System.Numerics;

namespace Hamming {

  class Hammings : IEnumerable<BigInteger> {
    private class LazyList<T> {
      public T v; public Lazy<LazyList<T>> cont;
      public LazyList(T v, Lazy<LazyList<T>> cont) {
        this.v = v; this.cont = cont;
      }
    }
    private uint[] primes;
    private Hammings() { } // must have an argument!!!
    public Hammings(uint[] prms) { this.primes = prms; }
    private LazyList<BigInteger> merge(LazyList<BigInteger> xs,
                                       LazyList<BigInteger> ys) {
      if (xs == null) return ys; else {
        var x = xs.v; var y = ys.v;
        if (BigInteger.Compare(x, y) < 0) {
          var cont = new Lazy<LazyList<BigInteger>>(() =>
                       merge(xs.cont.Value, ys));
          return new LazyList<BigInteger>(x, cont);
        }
        else {
          var cont = new Lazy<LazyList<BigInteger>>(() =>
                       merge(xs, ys.cont.Value));
          return new LazyList<BigInteger>(y, cont);
        }
      }
    }
    private LazyList<BigInteger> llmult(uint mltplr,
                                        LazyList<BigInteger> ll) {
      return new LazyList<BigInteger>(mltplr * ll.v,
                                      new Lazy<LazyList<BigInteger>>(() =>
                                        llmult(mltplr, ll.cont.Value)));
    }
    public IEnumerator<BigInteger> GetEnumerator() {
      Func<LazyList<BigInteger>,uint,LazyList<BigInteger>> u =
        (acc, p) => { LazyList<BigInteger> r = null;
                      var cont = new Lazy<LazyList<BigInteger>>(() => r);
                      r = new LazyList<BigInteger>(1, cont);
                      r = this.merge(acc, llmult(p, r));
                      return r; };
      yield return 1;
      for (var stt = primes.Aggregate(null, u); ; stt = stt.cont.Value)
        yield return stt.v;
    }
    IEnumerator IEnumerable.GetEnumerator() {
      return this.GetEnumerator();
    }
  }

  class Program {
    static void Main(string[] args) {
      Console.WriteLine("Calculates the Hamming sequence of numbers.\r\n");

      var primes = new uint[] { 5, 3, 2 };
      Console.WriteLine(String.Join(" ", (new Hammings(primes)).Take(20).ToArray()));
      Console.WriteLine((new Hammings(primes)).ElementAt(1691 - 1));

      var n = 1000000;

      var elpsd = -DateTime.Now.Ticks;

      var num = (new Hammings(primes)).ElementAt(n - 1);

      elpsd += DateTime.Now.Ticks;

      Console.WriteLine(num);
      Console.WriteLine("The {0}th hamming number took {1} milliseconds", n, elpsd / 10000);

      Console.Write("\r\nPress any key to exit:");
      Console.ReadKey(true);
      Console.WriteLine();
    }
  }
}
```



### Fast enumerating logarithmic version


The so-called "fast" generic version above isn't really all that fast due to all the extra array accesses required by the generic implementation and that it doesn't avoid duplicates as the above functional code does avoid.  It also uses a lot of memory as it has arrays that are the size of the range for which the Hamming numbers are calculated.

The following code eliminates or reduces all of those problems by being non-generic, eliminating duplicate calculations, saving memory by "draining" the growable List's used in blocks as back pointer indexes are used (thus using memory at the same rate as the functional version), thus avoiding excessive allocations/garbage collections; it also is enumerates through the Hamming numbers although that comes at a slight cost in overhead function calls:
{{trans|Nim}}

```c#
using System;
using System.Collections;
using System.Collections.Generic;
using System.Linq;
using System.Numerics;

class HammingsLogArr : IEnumerable<Tuple<uint, uint, uint>> {
  public static BigInteger trival(Tuple<uint, uint, uint> tpl) {
    BigInteger rslt = 1;
    for (var i = 0; i < tpl.Item1; ++i) rslt *= 2;
    for (var i = 0; i < tpl.Item2; ++i) rslt *= 3;
    for (var i = 0; i < tpl.Item3; ++i) rslt *= 5;
    return rslt;
  }
  private const double lb3 = 1.5849625007211561814537389439478; // Math.Log(3) / Math.Log(2);
  private const double lb5 = 2.3219280948873623478703194294894; // Math.Log(5) / Math.Log(2);
  private struct logrep {
    public uint x2, x3, x5;
    public double lg;
    public logrep(uint x, uint y, uint z, double lg) {
      this.x2 = x; this.x3 = y; this.x5 = z; this.lg = lg;
    }
    public static bool operator <(logrep x, logrep y) {
      return x.lg < y.lg;
    }
    public static bool operator >(logrep x, logrep y) {
      return x.lg > y.lg;
    }
    public logrep mul2() {
      return new logrep(this.x2 + 1, this.x3, this.x5, this.lg + 1.0);
    }
    public logrep mul3() {
      return new logrep(this.x2, this.x3 + 1, this.x5, this.lg + lb3);
    }
    public logrep mul5() {
      return new logrep(this.x2, this.x3, this.x5 + 1, this.lg + lb5);
    }
  }
  public IEnumerator<Tuple<uint, uint, uint>> GetEnumerator() {
    var one = new logrep();
    var m = new List<logrep>(); var h = new List<logrep>();
    var x5 = one.mul5();
    var mrg = one.mul3();
    var x53 = one.mul3().mul3(); // already advanced one step
    var x532 = one.mul2();
    var i = 0; var j = 0;
    yield return Tuple.Create(0u, 0u, 0u); // trivial case for one representation
    while (true) {
      if (i >= h.Capacity >> 1) { h.RemoveRange(0, i); i = 0; } // assume capacity stays the same...
      if (x532 < mrg) { h.Add(x532); x532 = h[i].mul2(); i++; }
      else {
        h.Add(mrg);
        if (j >= m.Capacity) { m.RemoveRange(0, j); j = 0; }
        if (x53 < x5) { mrg = x53; x53 = m[j].mul3(); j++; }
        else { mrg = x5; x5 = x5.mul5(); }
        m.Add(mrg);
      }
      var rslt = h[h.Count - 1];
      yield return Tuple.Create(rslt.x2, rslt.x3, rslt.x5);
    }
  }
  IEnumerator IEnumerable.GetEnumerator() {
    return this.GetEnumerator();
  }
}

class Program {
  static void Main(string[] args) {
    Console.WriteLine(String.Join(" ", (new HammingsLogArr()).Take(20)
                                        .Select(t => HammingsLogArr.trival(t))
                                        .ToArray()));
    Console.WriteLine(HammingsLogArr.trival(NthHamming.findNth(1691)));

    var n = 1000000UL;
    var elpsd = -DateTime.Now.Ticks;

    var rslt = (new HammingsLogArr()).ElementAt((int)n - 1);

    elpsd += DateTime.Now.Ticks;

    Console.WriteLine("2^{0} times 3^{1} times 5^{2}", rslt.Item1, rslt.Item2, rslt.Item3);
    var lgrthm = Math.Log10(2.0) * ((double)rslt.Item1 +
                  ((double)rslt.Item2 * Math.Log(3.0) + (double)rslt.Item3 * Math.Log(5.0)) / Math.Log(2.0));
    var pwr = Math.Floor(lgrthm); var mntsa = Math.Pow(10.0, lgrthm - pwr);
    Console.WriteLine("Approximately:  {0}E+{1}", mntsa, pwr);
    var s = HammingsLogArr.trival(rslt).ToString();
    var lngth = s.Length;
    Console.WriteLine("Decimal digits:  {0}", lngth);
    if (lngth <= 10000) {
      var i = 0;
      for (; i < lngth - 100; i += 100) Console.WriteLine(s.Substring(i, 100));
      Console.WriteLine(s.Substring(i));
    }
    Console.WriteLine("The {0}th hamming number took {1} milliseconds", n, elpsd / 10000);

    Console.Write("\r\nPress any key to exit:");
    Console.ReadKey(true);
    Console.WriteLine();
  }
}
```

{{output}}

```txt
1 2 3 4 5 6 8 9 10 12 15 16 18 20 24 25 27 30 32 36
2125764000
2^55 times 3^47 times 5^64
Approximately:  5.19312780448414E+83
Decimal digits:  84
519312780448388736089589843750000000000000000000000000000000000000000000000000000000
The 1000000th hamming number took 55 milliseconds
```


The above code is about 30 times faster than the functional code due to both eliminating the lambda closures that were the main problem with that code as well as eliminating the BigInteger operations.  It has about O(n) empirical performance and can find the billionth Hamming number in about 60 seconds.

===Extremely fast non-enumerating version calculating the error band===

The above code is about as fast as one can go generating sequences; however, if one is willing to forego sequences and just calculate the nth Hamming number (again), then some reading on the relationship between the size of numbers to the sequence numbers is helpful (Wikipedia: regular number). One finds that there is a very distinct relationship and that it quite quickly reduces to quite a small error band proportional to the log of the output value for larger ranges. Thus, the following code just scans for logarithmic representations to insert into a sequence for this top error band and extracts the correct nth representation from that band. It reduces time complexity to O(n^(2/3)) from O(n) for the sequence versions, but even more amazingly, reduces memory requirements to O(n^(1/3)) from O(n^(2/3)) and thus makes it possible to calculate very large values in the sequence on common personal computers. The code is as follows:
{{trans|Nim}}

```c#
using System;
using System.Collections;
using System.Collections.Generic;
using System.Linq;
using System.Numerics;

static class NthHamming {
  public static BigInteger trival(Tuple<uint, uint, uint> tpl) {
    BigInteger rslt = 1;
    for (var i = 0; i < tpl.Item1; ++i) rslt *= 2;
    for (var i = 0; i < tpl.Item2; ++i) rslt *= 3;
    for (var i = 0; i < tpl.Item3; ++i) rslt *= 5;
    return rslt;
  }

  private struct logrep {
    public uint x2, x3, x5;
    public double lg;
    public logrep(uint x, uint y, uint z, double lg) {
      this.x2 = x; this.x3 = y; this.x5 = z; this.lg = lg;
    }
  }

  private const double lb3 = 1.5849625007211561814537389439478; // Math.Log(3) / Math.Log(2);
  private const double lb5 = 2.3219280948873623478703194294894; // Math.Log(5) / Math.Log(2);
  private const double fctr = 6.0 * lb3 * lb5;
  private const double crctn = 2.4534452978042592646620291867186; // Math.Log(Math.sqrt(30.0)) / Math.Log(2.0)

  public static Tuple<uint, uint, uint> findNth(UInt64 n) {
    if (n < 1) throw new Exception("NthHamming.findNth:  argument must be > 0!");
    if (n < 2) return Tuple.Create(0u, 0u, 0u); // trivial case for argument of one
    var lgest = Math.Pow(fctr * (double)n, 1.0/3.0) - crctn; // from WP formula
    var frctn = (n < 1000000000) ? 0.509 : 0.105;
    var lghi = Math.Pow(fctr * ((double)n + frctn * lgest), 1.0/3.0) - crctn;
    var lglo = 2.0 * lgest - lghi; // upper and lower bound of upper "band"
    var count = 0UL; // need 64 bit precision in case...
    var bnd = new List<logrep>();
    for (uint k = 0, klmt = (uint)(lghi / lb5) + 1; k < klmt; ++k) {
      var p = (double)k * lb5;
      for (uint j = 0, jlmt = (uint)((lghi - p) / lb3) + 1; j < jlmt; ++j) {
        var q = p + (double)j * lb3;
        var ir = lghi - q;
        var lg = q + Math.Floor(ir); // current log2 value (estimated)
        count += (ulong)ir + 1;
        if (lg >= lglo) bnd.Add(new logrep((UInt32)ir, j, k, lg));
      }
    }
    if (n > count) throw new Exception("NthHamming.findNth:  band high estimate is too low!");
    var ndx = (int)(count - n);
    if (ndx >= bnd.Count) throw new Exception("NthHamming.findNth:  band low estimate is too high!");
    bnd.Sort((a, b) => (b.lg < a.lg) ? -1 : 1); // sort in decending order

    var rslt = bnd[ndx];
    return Tuple.Create(rslt.x2, rslt.x3, rslt.x5);
  }
}

class Program {
  static void Main(string[] args) {
    Console.WriteLine(String.Join(" ", Enumerable.Range(1,20).Select(i =>
                                          NthHamming.trival(NthHamming.findNth((ulong)i))).ToArray()));
    Console.WriteLine(NthHamming.trival((new HammingsLogArr()).ElementAt(1691 - 1)));

    var n = 1000000000000UL;
    var elpsd = -DateTime.Now.Ticks;

    var rslt = NthHamming.findNth(n);

    elpsd += DateTime.Now.Ticks;

    Console.WriteLine("2^{0} times 3^{1} times 5^{2}", rslt.Item1, rslt.Item2, rslt.Item3);
    var lgrthm = Math.Log10(2.0) * ((double)rslt.Item1 +
                  ((double)rslt.Item2 * Math.Log(3.0) + (double)rslt.Item3 * Math.Log(5.0)) / Math.Log(2.0));
    var pwr = Math.Floor(lgrthm); var mntsa = Math.Pow(10.0, lgrthm - pwr);
    Console.WriteLine("Approximately:  {0}E+{1}", mntsa, pwr);
    var s = HammingsLogArr.trival(rslt).ToString();
    var lngth = s.Length;
    Console.WriteLine("Decimal digits:  {0}", lngth);
    if (lngth <= 10000) {
      var i = 0;
      for (; i < lngth - 100; i += 100) Console.WriteLine(s.Substring(i, 100));
      Console.WriteLine(s.Substring(i));
    }
    Console.WriteLine("The {0}th hamming number took {1} milliseconds", n, elpsd / 10000);

    Console.Write("\r\nPress any key to exit:");
    Console.ReadKey(true);
    Console.WriteLine();
  }
}
```


The output is the same as above except that the time is too small to be measured. The billionth number in the sequence can be calculated in just about 10 milliseconds, the trillionth in about one second, the thousand trillionth in about a hundred seconds, and it should be possible to calculate the 10^19th value in less than a day (untested) on common personal computers. The (2^64 - 1)th value (18446744073709551615) cannot be calculated due to a slight overflow problem as it approaches that limit.


## C++


### C++11 For Each Generator


```cpp

#include <iostream>
#include <vector>
// Hamming like sequences Generator
//
// Nigel Galloway. August 13th., 2012
//
class Ham {
private:
	std::vector<unsigned int> _H, _hp, _hv, _x;
public:
	bool operator!=(const Ham& other) const {return true;}
	Ham begin() const {return *this;}
        Ham end() const {return *this;}
	unsigned int operator*() const {return _x.back();}
	Ham(const std::vector<unsigned int> &pfs):_H(pfs),_hp(pfs.size(),0),_hv({pfs}),_x({1}){}
	const Ham& operator++() {
	  for (int i=0; i<_H.size(); i++) for (;_hv[i]<=_x.back();_hv[i]=_x[++_hp[i]]*_H[i]);
	  _x.push_back(_hv[0]);
	  for (int i=1; i<_H.size(); i++) if (_hv[i]<_x.back()) _x.back()=_hv[i];
	  return *this;
	}
};

```


===5-Smooth===

```cpp

int main() {
  int count = 1;
  for (unsigned int i : Ham({2,3,5})) {
    if (count <= 62) std::cout << i << ' ';
    if (count++ == 1691) {
      std::cout << "\nThe one thousand six hundred and ninety first Hamming Number is " << i << std::endl;
      break;
    }
  }
  return 0;
}

```

Produces:

```txt

1 2 3 4 5 6 8 9 10 12 15 16 18 20 24 25 27 30 32 36 40 45 48 50 54 60 64 72 75 80 81 90 96 100 108 120 125 128 135 144 150 160 162 180 192 200 216 225 240 243 250 256 270 288 300 320 324 360 375 384 400 405
The one thousand six hundred and ninety first Hamming Number is 2125764000

```


===7-Smooth===

```cpp

int main() {
  int count = 1;
  for (unsigned int i : Ham({2,3,5,7})) {
    std::cout << i << ' ';
    if (count++ == 64) break;
  }
  std::cout << std::endl;
  return 0;
}

```

Produces:

```txt

1 2 3 4 5 6 7 8 9 10 12 14 15 16 18 20 21 24 25 27 28 30 32 35 36 40 42 45 48 49 50 54 56 60 63 64 70 72 75 80 81 84 90 96 98 100 105 108 112 120 125 126 128 135 140 144 147 150 160 162 168 175 180 189

```



### Avoiding Duplicates with Functional Code


If converted to use multi-precision integers (via GMP, as in this code), the above code is slower than it needs to be due to several reasons:  1) It uses the an adaptation of the original Dijkstra's algorithm, which is somewhat slower due to repeated calculations (2 time 3, 3 times 2, etc.), 2) the generator is written generally to handle any series of multiples, and 3) for finding the nth Hamming number, the code as `for (auto hmg : Ham({5, 3, 2})` means that there is a copy of the sometimes very large multi-precision number which can consume more time than the actual computation.  The following code isn't particularly fast due to other reasons that will be discussed, but avoids duplication of calculations by a modification of the algorithm; it is written functionally as a LazyList (which could easily also have iteration abilities added, with the a basic LazyList type defined here since there is no library available:
{{trans|Haskell}}
{{works with|C++11}}

```cpp
#include <chrono>

#include <iostream>
#include <gmpxx.h>
#include <functional>
#include <memory>

template<class T>
class Lazy {
public:
	T _v;
private:
	std::function<T()> _f;

public:
	explicit Lazy(std::function<T()> thnk)
		: _v(T()), _f(thnk) {};
	T value() { // not thread safe!
		if (this->_f != nullptr) {
			this->_v = this->_f();
			this->_f = nullptr;
		}
		return this->_v;
	}
};

template<class T>
class LazyList {
public:
	T head;
	std::shared_ptr<Lazy<LazyList<T>>> tail;
	LazyList(): head(T()) {} // only used in initializing Lazy...
	LazyList(T head, std::function<LazyList<T>()> thnk)
		: head(head), tail(std::make_shared<Lazy<LazyList<T>>>(thnk)) {}
	// default Copy/Move constructors and assignment operators seem to work well enough
	bool isEmpty() { return this->tail == nullptr; }
};

typedef std::shared_ptr<mpz_class> PBI;
typedef LazyList<PBI> LL;
typedef std::function<LL(LL)> FLL2LL;

LL merge(LL a, LL b) {
	auto ha = a.head; auto hb = b.head;
	if (*ha < *hb) {
		return LL(ha, [=]() { return merge(a.tail->value(), b); });
	} else {
		return LL(hb, [=]() { return merge(a, b.tail->value()); });
	}
}

LL smult(int m, LL s) {
	const auto im = mpz_class(m);
	const auto psmlt =
			std::make_shared<FLL2LL>([](LL ss) { return ss; });
	*psmlt = [=](LL ss) {
		return LL(std::make_shared<mpz_class>(*ss.head * im),
				  [=]() { return (*psmlt)(ss.tail->value()); });
	};
	return (*psmlt)(s); // worker wrapper pattern with recursive closure as worker...
}

LL u(LL s, int n) {
	const auto r = std::make_shared<LL>(LL()); // interior mutable...
	*r = smult(n, LL(std::make_shared<mpz_class>(1), [=]() { return *r; }));
	if (!s.isEmpty()) { *r = merge(s, *r); }
	return *r;
}

LL hammings() {
	auto r = LL();
	for (auto pn : std::vector<int>({5, 3, 2})) {
		r = u(r, pn);
	}
	return LL(std::make_shared<mpz_class>(1), [=]() { return r; });
}

int main() {
	auto hmgs = hammings();
	for (auto i = 0; i < 20; ++i) {
		std::cout << *hmgs.head << " ";
		hmgs = hmgs.tail->value();
	}
	std::cout << "\n";

	hmgs = hammings();
	for (auto i = 1; i < 1691; ++i) hmgs = hmgs.tail->value();
	std::cout << *hmgs.head << "\n";

	auto start = std::chrono::steady_clock::now();
	hmgs = hammings();
	for (auto i = 1; i < 1000000; ++i) hmgs = hmgs.tail->value();
	auto stop = std::chrono::steady_clock::now();

	auto ms = std::chrono::duration_cast<std::chrono::milliseconds>(stop - start);
	std::cout << *hmgs.head << " in " << ms.count() << "milliseconds.\n";
}
```


{{output}}

```txt
1 2 3 4 5 6 8 9 10 12 15 16 18 20 24 25 27 30 32 36
2125764000
519312780448388736089589843750000000000000000000000000000000000000000000000000000000 in 1079 milliseconds.
```


Note that the repeat loop to increment to the desired value is written so as not to oopy unnecessary Hamming values, unlike the use of the first Gnerator class.

This shows that one can program functionally in C++, but the performance is many times slower than a language more suitable for functional paradigms such as Haskell or even Kotlin; this is likely due to the cost of memory allocation with (multi-threading-safe) reference counting and that the memory system isn't tuned to many small allocations/de-allocations such as are generally necessary with functional programming.  One can easily see how to adapt this algorithm to make it work for the general case by just haveing an argument which is an vector of the required multipliers used in the `hammings` function.


### Avoiding Duplicates with Imperative Code


To show that it is the execution time for the functional LazyList that is taking the time, here is the same algorithm implemented imperatively using vectors, also avoiding duplicate calculations; it is not written as a general function for any set of multipliers as the extra vector addressing takes some extra time; again, it minimizes copying of values:
{{trans|Rust}}
{{works with|C++11}}

```cpp
#include <chrono>

#include <iostream>
#include <vector>
#include <gmpxx.h>

class Hammings {
private:
	const mpz_class _two = 2, _three = 3, _five = 5;
	std::vector<mpz_class> _m = {}, _h = {1};
	mpz_class _x5 = 5, _x53 = 9, _mrg = 3, _x532 = 2;
	int _i = 1, _j = 0;
public:
	Hammings() {_m.reserve(65536); _h.reserve(65536); };
	bool operator!=(const Hammings& other) const { return true; }
	Hammings begin() const { return *this; }
	Hammings end() const { return *this; }
	mpz_class operator*() { return _h.back(); }
	const Hammings& operator++() {
		if (_i > _h.capacity() / 2) {
			_h.erase(_h.begin(), _h.begin() + _i);
			_i = 0;
		}
		if (_x532 < _mrg) {
			_h.push_back(_x532);
			_x532 = _h[_i++] * _two;
		} else {
			_h.push_back(_mrg);
			if (_x53 < _x5) {
				_mrg = _x53;
				_x53 = _m[_j++] * _three;
			} else {
				_mrg = _x5;
				_x5 = _x5 * _five;
			}
			if (_j > _m.capacity() / 2) {
				_m.erase(_m.begin(), _m.begin() + _j);
				_j = 0;
			}
			_m.push_back(_mrg);
		}
		return *this;
	}
};

int main() {
	auto cnt = 1;
	for (auto hmg : Hammings()) {
		if (cnt <= 20) std::cout << hmg << " ";
		if (cnt == 20) std::cout << "\n";
		if (cnt++ >= 1691) {
			std::cout << hmg << "\n";
			break;
		}
	}

	auto start = std::chrono::steady_clock::now();
	hmgs = hammings();
	auto&& hmgitr = Hammings();
	for (auto i = 1; i < 1000000; ++i) ++hmgitr;
	auto stop = std::chrono::steady_clock::now();

	auto ms = std::chrono::duration_cast<std::chrono::milliseconds>(stop - start);
	std::cout << *hmgitr << " in " << ms.count() << "milliseconds.\n";
}
```


{{output}}

```txt
1 2 3 4 5 6 8 9 10 12 15 16 18 20 24 25 27 30 32 36
2125764000
519312780448388736089589843750000000000000000000000000000000000000000000000000000000 in 79 milliseconds.
```


This code takes about the same amount of time as Haskell for the functional algorithm calculating multi-precision values (also uses GMP; not including the list processing time), but greatly reduces the C++ processing time compared to the functional version due to the use of imperative code and vectors.


## Clojure

This version implements Dijkstra's merge solution, so is closely related to the Haskell version.

```clojure
(defn smerge [xs ys]
  (lazy-seq
    (let [x (first xs),
          y (first ys),
          [z xs* ys*]
          (cond
            (< x y) [x (rest xs) ys]
            (> x y) [y xs (rest ys)]
            :else   [x (rest xs) (rest ys)])]
      (cons z (smerge xs* ys*)))))

(def hamming
  (lazy-seq
    (->> (map #(*' 5 %) hamming)
         (smerge (map #(*' 3 %) hamming))
         (smerge (map #(*' 2 %) hamming))
         (cons 1))))
```

Note that the above version uses a lot of space and time after calculating a few hundred thousand elements of the sequence. This is no doubt due to not avoiding the generation of duplicates in the sequences as well as its "holding on to the head": it maintains the entire generated sequences in memory.

'''Avoiding duplicates and reducing memory use'''

In order to fix the problems with the above program as to memory use and extra time expended, the following code implements the Haskell idea as a function so that it does not retain the pointers to the streams used so that they can be garbage collected from the beginning as they are consumed. it avoids duplicate number generation by using intermediate streams for each of the multiples and building each on the results of the last; also, it orders the streams from least dense to most so that the intermediate streams retained are as short as possible, with the "s5" stream only from one fifth to a third of the current value, the "s35" stream only between a third and a half of the current output value, and the s235 stream only between a half and the current output - as the sequence is not very dense with increasing range, mot many values need be retained:
{{trans|Haskell}}

```clojure
(defn hamming
  "Computes the unbounded sequence of Hamming 235 numbers."
  []
  (letfn [(merge [xs ys]
            (if (nil? xs) ys
              (let [xv (first xs), yv (first ys)]
                (if (< xv yv) (cons xv (lazy-seq (merge (next xs) ys)))
                              (cons yv (lazy-seq (merge xs (next ys)))))))),
          (smult [m s] ;; equiv to map (* m) s -- faster
            (cons (*' m (first s)) (lazy-seq (smult m (next s))))),
          (u [s n] (let [r (atom nil)]
                      (reset! r (merge s (smult n (cons 1 (lazy-seq @r)))))))]
    (cons 1 (lazy-seq (reduce u nil (list 5 3 2))))))
```


Much of the time expended for larger ranges (say 10 million or more) is due to the time doing extended precision arithmetic, with also a significant percentage spent in garbage collection.  Following is the output from the REPL after compiling the program:

After compiling code in REPL:
{{output}}

```txt

(take 20 (hamming))
(1 2 3 4 5 6 8 9 10 12 15 16 18 20 24 25 27 30 32 36)
(->> (hamming) (drop 1690) (first) (time))
"Elapsed time: 1.105582 msecs"
2125764000
(->> (hamming) (drop 999999) (first) (time))
"Elapsed time: 447.561128 msecs"
519312780448388736089589843750000000000000000000000000000000000000000000000000000000N

```


So that generated '.class' files in a folder or a generated '.jar' file (possibly standalone, containing the run time library) run at about the same speed as inside the IDE (after compilation), the Leiningen "project.clj" file needs to be modified to contain the following line so as to eliminate JVM options that slow the performance:

```txt
  :jvm-opts ^:replace []
```



## CoffeeScript


```coffeescript
# Generate hamming numbers in order.  Hamming numbers have the
# property that they don't evenly divide any prime numbers outside
# a given set, such as [2, 3, 5].

generate_hamming_sequence = (primes, max_n) ->
  # We use a lazy algorithm, only ever keeping N candidates
  # in play, one for each of our seed primes.  Let's say
  # primes is [2,3,5].  Our virtual streams are these:
  #
  # hammings:    1,2,3,4,5,6,8,10,12,15,16,18,20,...
  # hammings*2:  2,4,6,9.10,12,16,20,24,30,32,36,40...
  # hammings*3:  3,6,9,12,15,18,24,30,36,45,...
  # hammings*5:  5,10,15,20,25,30,40,50,...
  #
  # After encountering 40 for the last time, our candidates
  # will be
  #   50 = 2 * 25
  #   45 = 3 * 15
  #   50 = 5 * 10
  # Then, after 45
  #   50 = 2 * 25
  #   48 = 3 * 16 <= new
  #   50 = 5 * 10
  hamming_numbers = [1]
  candidates = ([p, p, 1] for p in primes)
  last_number = 1
  while hamming_numbers.length < max_n
    # Get the next candidate Hamming Number tuple.
    i = min_idx(candidates)
    candidate = candidates[i]
    [n, p, seq_idx] = candidate

    # Add to sequence unless it's a duplicate.
    if n > last_number
      hamming_numbers.push n
      last_number = n

    # Replace the candidate with its successor (based on
    # p = 2, 3, or 5).
    #
    # This is the heart of the algorithm.  Let's say, over the
    # primes [2,3,5], we encounter the hamming number 32 based on it being
    # 2 * 16, where 16 is the 12th number in the sequence.
    # We'll be passed in [32, 2, 12] as candidate, and
    # hamming_numbers will be [1,2,3,4,5,6,8,9,10,12,16,18,...]
    # by now.  The next candidate we need to enqueue is
    # [36, 2, 13], where the numbers mean this:
    #
    #    36 - next multiple of 2 of a Hamming number
    #     2 - prime number
    #    13 - 1-based index of 18 in the sequence
    #
    # When we encounter [36, 2, 13], we will then enqueue
    # [40, 2, 14], based on 20 being the 14th hamming number.
    q = hamming_numbers[seq_idx]
    candidates[i] = [p*q, p, seq_idx+1]

  hamming_numbers

min_idx = (arr) ->
  # Don't waste your time reading this--it just returns
  # the index of the smallest tuple in an array, respecting that
  # the tuples may contain integers. (CS compiles to JS, which is
  # kind of stupid about sorting.  There are libraries to work around
  # the limitation, but I wanted this code to be standalone.)
  less_than = (tup1, tup2) ->
    i = 0
    while i < tup2.length
      return true if tup1[i] <= tup2[i]
      return false if tup1[i] > tup2[i]
      i += 1

  min_i = 0
  for i in [1...arr.length]
    if less_than arr[i], arr[min_i]
      min_i = i
  return min_i

primes = [2, 3, 5]
numbers = generate_hamming_sequence(primes, 10000)
console.log numbers[1690]
console.log numbers[9999]
```



## Common Lisp

Maintaining three queues, popping the smallest value every time.

```lisp
(defun next-hamm (factors seqs)
  (let ((x (apply #'min (map 'list #'first seqs))))
    (loop for s in seqs
	  for f in factors
	  for i from 0
	  with add = t do
	  (if (= x (first s)) (pop s))
	  ;; prevent a value from being added to multiple lists
	  (when add
	    (setf (elt seqs i) (nconc s (list (* x f))))
	    (if (zerop (mod x f)) (setf add nil)))
    finally (return x))))

(loop with factors = '(2 3 5)
      with seqs    = (loop for i in factors collect '(1))
      for n from 1 to 1000001 do
      (let ((x (next-hamm factors seqs)))
	(if (or (< n 21)
		(= n 1691)
		(= n 1000000)) (format t "~d: ~d~%" n x))))
```

A much faster method:

```lisp
(defun hamming (n)
  (let ((fac '(2 3 5))
	(idx (make-array 3 :initial-element 0))
	(h (make-array (1+ n)
		       :initial-element 1
		       :element-type 'integer)))
    (loop for i from 1 to n
	  with e with x = '(1 1 1) do
	  (setf e (setf (aref h i) (apply #'min x))
		x (loop for y in x
			for f in fac
			for j from 0
			collect (if (= e y) (* f (aref h (incf (aref idx j)))) y))))
    (aref h n)))

(loop for i from 1 to 20 do
      (format t "~2d: ~d~%" i (hamming i)))

(loop for i in '(1691 1000000) do
      (format t "~d: ~d~%" i (hamming i)))
```

{{out}}

```txt
 1: 1
 2: 2
 3: 3
 4: 4
 5: 5
 6: 6
 7: 8
 8: 9
 9: 10
10: 12
11: 15
12: 16
13: 18
14: 20
15: 24
16: 25
17: 27
18: 30
19: 32
20: 36
1691: 2125764000
1000000: 519312780448388736089589843750000000000000000000000000000000000000000000000000000000
```



## D


### Basic Version

This version keeps all numbers in memory, computing all the Hamming numbers up to the needed one. Performs constant number of operations per Hamming number produced.

```d
import std.stdio, std.bigint, std.algorithm, std.range, core.memory;

auto hamming(in uint n) pure nothrow /*@safe*/ {
    immutable BigInt two = 2, three = 3, five = 5;
    auto h = new BigInt[n];
    h[0] = 1;
    BigInt x2 = 2, x3 = 3, x5 = 5;
    size_t i, j, k;

    foreach (ref el; h.dropOne) {
        el = min(x2, x3, x5);
        if (el == x2) x2 = two   * h[++i];
        if (el == x3) x3 = three * h[++j];
        if (el == x5) x5 = five  * h[++k];
    }
    return h.back;
}

void main() {
    GC.disable;
    iota(1, 21).map!hamming.writeln;
    1_691.hamming.writeln;
    1_000_000.hamming.writeln;
}
```

{{out}}

```txt
[1, 2, 3, 4, 5, 6, 8, 9, 10, 12, 15, 16, 18, 20, 24, 25, 27, 30, 32, 36]
2125764000
519312780448388736089589843750000000000000000000000000000000000000000000000000000000
```

Runtime is about 1.6 seconds with LDC2.


### Alternative Version 1

This keeps <math>O(n^{2/3})</math> numbers in memory, but over-computes a sequence by a factor of about <math>\Theta(n^{2/3})</math>, calculating extra multiples past that as well. Incurs an extra <math>O(\log n)</math> factor of operations per each number produced (reinserting its multiples into a tree). Doesn't stop when the target number is reached, instead continuing until it is no longer needed:
{{trans|Java}}

```d
import std.stdio, std.bigint, std.container, std.algorithm, std.range,
       core.memory;

BigInt hamming(in int n)
in {
   assert(n > 0);
} body {
    auto frontier = redBlackTree(2.BigInt, 3.BigInt, 5.BigInt);
    auto lowest = 1.BigInt;
    foreach (immutable _; 1 .. n) {
        lowest = frontier.front;
        frontier.removeFront;
        frontier.insert(lowest * 2);
        frontier.insert(lowest * 3);
        frontier.insert(lowest * 5);
    }
    return lowest;
}

void main() {
    GC.disable;
    writeln("First 20 Hamming numbers: ", iota(1, 21).map!hamming);
    writeln("hamming(1691) = ", 1691.hamming);
    writeln("hamming(1_000_000) = ", 1_000_000.hamming);
}
```

{{out}}

```txt
First 20 Hamming numbers: [1, 2, 3, 4, 5, 6, 8, 9, 10, 12, 15, 16, 18, 20, 24, 25, 27, 30, 32, 36]
hamming(1691) = 2125764000
hamming(1_000_000) = 519312780448388736089589843750000000000000000000000000000000000000000000000000000000
```

About 3.2 seconds run time with LDC2.


### Alternative Version 2

Does exactly what the first version does, creating an array and filling it with Hamming numbers, keeping the three back pointers into the sequence for next multiples calculations, except that it represents the numbers as their coefficients triples and their logarithm values (for comparisons), thus saving on BigInt calculations.
{{trans|C}}

```d
import std.stdio: writefln;
import std.bigint: BigInt;
import std.conv: text;
import std.numeric: gcd;
import std.algorithm: copy, map;
import std.array: array;
import core.stdc.stdlib: calloc;
import std.math: log; // ^^

// Number of factors.
enum NK = 3;

enum MAX_HAM = 10_000_000;
static assert(gcd(NK, MAX_HAM) == 1);

enum int[NK] factors = [2, 3, 5];


/// K-smooth numbers (stored as their exponents of each factor).
struct Hamming {
    double v; // Log of the number, for convenience.
    ushort[NK] e; // Exponents of each factor.

    public static __gshared immutable double[factors.length] inc =
        factors[].map!log.array;

    bool opEquals(in ref Hamming y) const pure nothrow @nogc {
        //return this.e == y.e; // Too much slow.
        foreach (immutable i; 0 .. this.e.length)
            if (this.e[i] != y.e[i])
                return false;
        return true;
    }

    void update() pure nothrow @nogc {
        //this.v = dotProduct(inc, this.e); // Too much slow.
        this.v = 0.0;
        foreach (immutable i; 0 .. this.e.length)
            this.v += inc[i] * this.e[i];
    }

    string toString() const {
        BigInt result = 1;
        foreach (immutable i, immutable f; factors)
            result *= f.BigInt ^^ this.e[i];
        return result.text;
    }
}

// Global variables.
__gshared Hamming[] hams;
__gshared Hamming[NK] values;

nothrow @nogc static this() {
    // Slower than calloc if you don't use all the MAX_HAM items.
    //hams = new Hamming[MAX_HAM];

    auto ptr = cast(Hamming*)calloc(MAX_HAM, Hamming.sizeof);
    static const err = new Error("Not enough memory.");
    if (!ptr)
        throw err;
    hams = ptr[0 .. MAX_HAM];

    foreach (immutable i, ref v; values) {
        v.e[i] = 1;
        v.v = Hamming.inc[i];
    }
}


ref Hamming getHam(in size_t n) nothrow @nogc
in {
    assert(n <= MAX_HAM);
} body {
    // Most of the time v can be just incremented, but eventually
    // floating point precision will bite us, so better recalculate.
    __gshared static size_t[NK] idx;
    __gshared static int n_hams;

    for (; n_hams < n; n_hams++) {
        {
            // Find the index of the minimum v.
            size_t ni = 0;
            foreach (immutable i; 1 .. NK)
                if (values[i].v < values[ni].v)
                    ni = i;

            hams[n_hams] = values[ni];
            hams[n_hams].update;
        }

        foreach (immutable i; 0 .. NK)
            if (values[i] == hams[n_hams]) {
                values[i] = hams[idx[i]];
                idx[i]++;
                values[i].e[i]++;
                values[i].update;
            }
    }

    return hams[n - 2];
}


void main() {
    foreach (immutable n; [1691, 10 ^^ 6, MAX_HAM])
        writefln("%8d: %s", n, n.getHam);
}
```

The output is similar to the second C version.
Runtime is about 0.11 seconds if MAX_HAM = 1_000_000 (as the task requires),
and 0.90 seconds if MAX_HAM = 10_000_000.


### Alternative Version 3

This version is similar to the precedent, but frees unused values.
It's a little slower than the precedent version, but it uses much less RAM,
so it allows to compute the result for larger n.

```d
import std.stdio: writefln;
import std.bigint: BigInt;
import std.conv: text;
import std.algorithm: map;
import std.array: array;
import core.stdc.stdlib: malloc, calloc, free;
import std.math: log; // ^^

// Number of factors.
enum NK = 3;

__gshared immutable int[NK] primes = [2, 3, 5];
__gshared immutable double[NK] lnPrimes = primes[].map!log.array;

/// K-smooth numbers (stored as their exponents of each factor).

struct Hamming {
    double ln; // Log of the number.
    ushort[NK] e; // Exponents of each factor.
    Hamming* next;
    size_t n;

    // Recompute the logarithm from the exponents.
    void recalculate() pure nothrow @safe @nogc {
        this.ln = 0.0;
        foreach (immutable i, immutable ei; this.e)
            this.ln += lnPrimes[i] * ei;
    }

    string toString() const {
        BigInt result = 1;
        foreach (immutable i, immutable f; primes)
            result *= f.BigInt ^^ this.e[i];
        return result.text;
    }
}

Hamming getHam(in size_t n) nothrow @nogc
in {
    assert(n && n != size_t.max);
} body {
    static struct Candidate {
        typeof(Hamming.ln) ln;
        typeof(Hamming.e) e;

        void increment(in size_t n) pure nothrow @safe @nogc {
            e[n] += 1;
            ln += lnPrimes[n];
        }

        bool opEquals(T)(in ref T y) const pure nothrow @safe @nogc {
            // return this.e == y.e; // Slow.
            return !((this.e[0] ^ y.e[0]) |
                     (this.e[1] ^ y.e[1]) |
                     (this.e[2] ^ y.e[2]));
        }

        int opCmp(T)(in ref T y) const pure nothrow @safe @nogc {
            return (ln > y.ln) ? 1 : (ln < y.ln ? -1 : 0);
        }
    }

    static struct HammingIterator { // Not a Range.
        Candidate cand;
        Hamming* base;
        size_t primeIdx;

        this(in size_t i, Hamming* b) pure nothrow @safe @nogc {
            primeIdx = i;
            base = b;
            cand.e = base.e;
            cand.ln = base.ln;
            cand.increment(primeIdx);
        }

        void next() pure nothrow @safe @nogc {
            base = base.next;
            cand.e = base.e;
            cand.ln = base.ln;
            cand.increment(primeIdx);
        }
    }

    HammingIterator[NK] its;
    Hamming* head = cast(Hamming*)calloc(Hamming.sizeof, 1);
    Hamming* freeList, cur = head;
    Candidate next;

    foreach (immutable i, ref it; its)
        it = HammingIterator(i, cur);

    for (size_t i = cur.n = 1; i < n; ) {
        auto leastReferenced = size_t.max;
        next.ln = double.max;
        foreach (ref it; its) {
            if (it.cand == *cur)
                it.next;
            if (it.base.n < leastReferenced)
                leastReferenced = it.base.n;
            if (it.cand < next)
                next = it.cand;
        }

        // Collect unferenced numbers.
        while (head.n < leastReferenced) {
            auto tmp = head;
            head = head.next;
            tmp.next = freeList;
            freeList = tmp;
        }

        if (!freeList) {
            cur.next = cast(Hamming*)malloc(Hamming.sizeof);
        } else {
            cur.next = freeList;
            freeList = freeList.next;
        }

        cur = cur.next;
        version (fastmath) {
            cur.ln = next.ln;
            cur.e = next.e;
        } else {
            cur.e = next.e;
            cur.recalculate; // Prevent FP error accumulation.
        }

        cur.n = i++;
        cur.next = null;
    }

    auto result = *cur;
    version (leak) {}
    else {
        while (head) {
            auto tmp = head;
            head = head.next;
            tmp.free;
        }

        while (freeList) {
            auto tmp = freeList;
            freeList = freeList.next;
            tmp.free;
        }
    }

    return result;
}

void main() {
    foreach (immutable n; [1691, 10 ^^ 6, 10_000_000])
        writefln("%8d: %s", n, n.getHam);
}
```

The output is the same as the second alternative version.

=={{Header|Dart}}==

In order to produce reasonable ranges of Hamming numbers, one needs the BigInt type, but processing of many BigInt's in generating a sequence slows the code; for that reason the following code records the determined values as a combination of an approximation of the log base two value and the triple of the powers of two, three and five, only generating the final output values as BigInt's as required:

```dart
import 'dart:math';

final lb2of2 = 1.0;
final lb2of3 = log(3.0) / log(2.0);
final lb2of5 = log(5.0) / log(2.0);

class Trival {
  final double log2;
  final int twos;
  final int threes;
  final int fives;
  Trival mul2() {
    return Trival(this.log2 + lb2of2, this.twos + 1, this.threes, this.fives);
  }
  Trival mul3() {
    return Trival(this.log2 + lb2of3, this.twos, this.threes + 1, this.fives);
  }
  Trival mul5() {
    return Trival(this.log2 + lb2of5, this.twos, this.threes, this.fives + 1);
  }
  @override String toString() {
    return this.log2.toString() + " "
      + this.twos.toString() + " "
      + this.threes.toString() + " "
      + this.fives.toString();
  }
  const Trival(this.log2, this.twos, this.threes, this.fives);
}

Iterable<Trival> makeHammings() sync* {
  var one = Trival(0.0, 0, 0, 0);
  yield(one);
  var s532 = one.mul2();
  var mrg = one.mul3();
  var s53 = one.mul3().mul3(); // equivalent to 9 for advance step
  var s5 = one.mul5();
  var i = -1; var j = -1;
  List<Trival> h = [];
  List<Trival> m = [];
  Trival rslt;
  while (true) {
    if (s532.log2 < mrg.log2) {
      rslt = s532; h.add(s532); ++i; s532 = h[i].mul2();
    } else {
      rslt = mrg; h.add(mrg);
      if (s53.log2 < s5.log2) {
        mrg = s53; m.add(s53); ++j; s53 = m[j].mul3();
      } else {
        mrg = s5; m.add(s5); s5 = s5.mul5();
      }
      if (j > (m.length >> 1)) {m.removeRange(0, j); j = 0; }
    }
    if (i > (h.length >> 1)) {h.removeRange(0, i); i = 0; }
    yield(rslt);
  }
}

BigInt trival2Int(Trival tv) {
  return BigInt.from(2).pow(tv.twos)
           * BigInt.from(3).pow(tv.threes)
           * BigInt.from(5).pow(tv.fives);
}

void main() {
  final numhams = 1000000000000;
  var hamseqstr = "The first 20 Hamming numbers are:  ( ";
  makeHammings().take(20)
      .forEach((h) => hamseqstr += trival2BigInt(h).toString() + " ");
  print(hamseqstr + ")");
  var nthhamseqstr = "The first 20 Hamming numbers are:  ( ";
  for (var i = 1; i <= 20; ++i) {
    nthhamseqstr += trival2BigInt(nthHamming(i)).toString() + " ";
  }
  print(nthhamseqstr + ")");
  final strt = DateTime.now().millisecondsSinceEpoch;
  final answr = makeHammings().skip(999999).first;
  final elpsd = DateTime.now().millisecondsSinceEpoch - strt;
  print("The ${numhams}th Hamming number is:  $answr");
  print("in full as:  ${trival2BigInt(answr)}");
  print("This test took $elpsd milliseconds.");
}
```

{{Output}}

```txt
The first 20 Hamming numbers are:  ( 1 2 3 4 5 6 8 9 10 12 15 16 18 20 24 25 27 30 32 36 )
The 1000000th Hamming number is:  278.096635606686 55 47 64
in full as:  519312780448388736089589843750000000000000000000000000000000000000000000000000000000
This test took 311 milliseconds.
```


Due to using a mutable extendable List (Array) and mutation, the above generator is reasonably fast, and as well has the feature that List memory is recovered as it is no longer required, with a considerable saving in both execution speed and memory requirement.

'''Alternate extremely fast version using an "error band"'''

Although not a Hamming sequence generator, the following code uses the known characteristics of the distribution of Hamming numbers to just scan through to find all possibilities in a relatively narrow "error band" which then can be sorted based on the log base two approximation and the nth element determined inside that band; it has a huge advantage that memory requirements drop to O(n^(1/3)) and asymptotic execution complexity drops from O(n) to O(n^(2/3)) for an extremely fast execution speed (thanks to WillNess for the start of this algorithm as referenced in the Haskell section):
{{translated from|Haskell}}

```dart
import 'dart:math';

final lb2of2 = 1.0;
final lb2of3 = log(3.0) / log(2.0);
final lb2of5 = log(5.0) / log(2.0);

class Trival {
  final double log2;
  final int twos;
  final int threes;
  final int fives;
  Trival mul2() {
    return Trival(this.log2 + lb2of2, this.twos + 1, this.threes, this.fives);
  }
  Trival mul3() {
    return Trival(this.log2 + lb2of3, this.twos, this.threes + 1, this.fives);
  }
  Trival mul5() {
    return Trival(this.log2 + lb2of5, this.twos, this.threes, this.fives + 1);
  }
  @override String toString() {
    return this.log2.toString() + " "
      + this.twos.toString() + " "
      + this.threes.toString() + " "
      + this.fives.toString();
  }
  const Trival(this.log2, this.twos, this.threes, this.fives);
}

BigInt trival2BigInt(Trival tv) {
  return BigInt.from(2).pow(tv.twos)
           * BigInt.from(3).pow(tv.threes)
           * BigInt.from(5).pow(tv.fives);
}

Trival nthHamming(int n) {
  if (n < 1) throw Exception("nthHamming:  argument must be higher than 0!!!");
  if (n < 7) {
    if (n & (n - 1) == 0) {
      final bts = n.bitLength - 1;
      return Trival(bts.toDouble(), bts, 0, 0);
    }
    switch (n) {
      case 3: return Trival(lb2of3, 0, 1, 0);
      case 5: return Trival(lb2of5, 0, 0, 1);
      case 6: return Trival(lb2of2 + lb2of3, 1, 1, 0);
    }
  }
  final fctr = 6.0 * lb2of3 * lb2of5;
  final crctn = log(sqrt(30.0)) / log(2.0);
  final lb2est = pow(fctr * n.toDouble(), 1.0/3.0) - crctn;
  final lb2rng = 2.0/lb2est;
  final lb2hi = lb2est + 1.0/lb2est;
  List<Trival> ebnd = [];
  var cnt = 0;
  for (var k = 0; k < (lb2hi / lb2of5).ceil(); ++k) {
    final lb2p = lb2hi - k * lb2of5;
    for (var j = 0; j < (lb2p / lb2of3).ceil(); ++j) {
      final lb2q = lb2p - j * lb2of3;
      final i = lb2q.floor(); final lb2frac = lb2q - i;
      cnt += i + 1;
      if (lb2frac <= lb2rng) {
        final lb2v = i * lb2of2 + j * lb2of3 + k * lb2of5;
        ebnd.add(Trival(lb2v, i, j, k));
      }
    }
  }
  ebnd.sort((a, b) => b.log2.compareTo(a.log2)); // descending order
  final ndx = cnt - n;
  if (ndx < 0) throw Exception("nthHamming:  not enough triples generated!!!");
  if (ndx >= ebnd.length) throw Exception("nthHamming:  error band is too narrow!!!");
  return ebnd[ndx];
}

void main() {
  final numhams = 1000000;
  var nthhamseqstr = "The first 20 Hamming numbers are:  ( ";
  for (var i = 1; i <= 20; ++i) {
    nthhamseqstr += trival2BigInt(nthHamming(i)).toString() + " ";
  }
  print(nthhamseqstr + ")");
  final strt = DateTime.now().millisecondsSinceEpoch;
  final answr = nthHamming(numhams);
  final elpsd = DateTime.now().millisecondsSinceEpoch - strt0;
  print("The ${numhams}th Hamming number is:  $answr");
  print("in full as:  ${trival2BigInt(answr)}");
  print("This test took $elpsd milliseconds.");
}
```


The output from the above code is the same as the above version but it is so fast that the time to find the millionth Hamming number is too small to be measured other than the Dart VM JIT time.  It can find the billionth prime in a fraction of a second and the trillionth prime in seconds.

'''Increasing the range above 1e13 by using a BigInt log base two representation'''

For arguments higher than about 1e13, the precision of the Double log base two approximations used above is not adequate to do an accurate sort, but the algorithm continues to work (although perhaps slightly slower) by changing the code to use BigInt log base two representations as follows:

```dart
import 'dart:math';

final biglb2of2 = BigInt.from(1) << 100; // 100 bit representations...
final biglb2of3 = (BigInt.from(1784509131911002) << 50) + BigInt.from(134114660393120);
final biglb2of5 = (BigInt.from(2614258625728952) << 50) + BigInt.from(773584997695443);

class BigTrival {
  final BigInt log2;
  final int twos;
  final int threes;
  final int fives;
  @override String toString() {
    return this.log2.toString() + " "
      + this.twos.toString() + " "
      + this.threes.toString() + " "
      + this.fives.toString();
  }
  const BigTrival(this.log2, this.twos, this.threes, this.fives);
}

BigInt bigtrival2BigInt(BigTrival tv) {
  return BigInt.from(2).pow(tv.twos)
           * BigInt.from(3).pow(tv.threes)
           * BigInt.from(5).pow(tv.fives);
}

BigTrival nthHamming(int n) {
  if (n < 1) throw Exception("nthHamming:  argument must be higher than 0!!!");
  if (n < 7) {
    if (n & (n - 1) == 0) {
      final bts = n.bitLength - 1;
      return BigTrival(BigInt.from(bts) << 100, bts, 0, 0);
    }
    switch (n) {
      case 3: return BigTrival(biglb2of3, 0, 1, 0);
      case 5: return BigTrival(biglb2of5, 0, 0, 1);
      case 6: return BigTrival(biglb2of2 + biglb2of3, 1, 1, 0);
    }
  }
  final fctr = lb2of3 * lb2of5 * 6;
  final crctn = log(sqrt(30.0)) / log(2.0);
  final lb2est = pow(fctr * n.toDouble(), 1.0/3.0) - crctn;
  final lb2rng = 2.0/lb2est;
  final lb2hi = lb2est + 1.0/lb2est;
  List<BigTrival> ebnd = [];
  var cnt = 0;
  for (var k = 0; k < (lb2hi / lb2of5).ceil(); ++k) {
    final lb2p = lb2hi - k * lb2of5;
    for (var j = 0; j < (lb2p / lb2of3).ceil(); ++j) {
      final lb2q = lb2p - j * lb2of3;
      final i = lb2q.floor(); final lb2frac = lb2q - i;
      cnt += i + 1;
      if (lb2frac <= lb2rng) {
//        final lb2v = i * lb2of2 + j * lb2of3 + k * lb2of5;
//        ebnd.add(Trival(lb2v, i, j, k));
        final lb2v = BigInt.from(i) * biglb2of2
                        + BigInt.from(j) * biglb2of3
                        + BigInt.from(k) * biglb2of5;
        ebnd.add(BigTrival(lb2v, i, j, k));
      }
    }
  }
  ebnd.sort((a, b) => b.log2.compareTo(a.log2)); // descending order
  final ndx = cnt - n;
  if (ndx < 0) throw Exception("nthHamming:  not enough triples generated!!!");
  if (ndx >= ebnd.length) throw Exception("nthHamming:  error band is too narrow!!!");
  return ebnd[ndx];
}

void main() {
  final numhams = 1000000000;
  var nthhamseqstr = "The first 20 Hamming numbers are:  ( ";
  for (var i = 1; i <= 20; ++i) {
    nthhamseqstr += bigtrival2BigInt(nthHamming(i)).toString() + " ";
  }
  print(nthhamseqstr + ")");
  final strt = DateTime.now().millisecondsSinceEpoch;
  final answr = nthHamming(numhams);
  final elpsd = DateTime.now().millisecondsSinceEpoch - strt;
  print("The ${numhams}th Hamming number is:  $answr");
  print("in full as:  ${bigtrival2BigInt(answr)}");
  print("This test took $elpsd milliseconds.");
}
```


With these changes, the algorithm can find the 1e19'th prime in the order af days depending on the CPU used.


## DCL


```DCL
$ limit = p1
$
$ n = 0
$ h_'n = 1
$ x2 = 2
$ x3 = 3
$ x5 = 5
$ i = 0
$ j = 0
$ k = 0
$
$ n = 1
$ loop:
$  x = x2
$  if x3 .lt. x then $ x = x3
$  if x5 .lt. x then $ x = x5
$  h_'n = x
$  if x2 .eq. h_'n
$  then
$   i = i + 1
$   x2 = 2 * h_'i
$  endif
$  if x3 .eq. h_'n
$  then
$   j = j + 1
$   x3 = 3 * h_'j
$  endif
$  if x5 .eq. h_'n
$  then
$   k = k + 1
$   x5 = 5 * h_'k
$  endif
$  n = n + 1
$  if n .le. limit then $ goto loop
$
$ i = 0
$ loop2:
$  write sys$output h_'i
$  i = i + 1
$  if i .lt. 20 then $ goto loop2
$
$ n = limit - 1
$ write sys$output h_'n
```

{{out}}

```txt

Here's the output;

$ @hamming 1691
1
2
3
4
5
6
8
9
10
12
15
16
18
20
24
25
27
30
32
36
2125764000

```



## Eiffel


```Eiffel

note
	description    : "Initial part, in order, of the sequence of Hamming numbers"
	math           : "[
			   Hamming numbers, also known as regular numbers and 5-smooth numbers, are natural integers
			   that have 2, 3 and 5 as their only prime factors.
		         ]"
	computer_arithmetic :
	                 "[
			   This version avoids integer overflow and stops at the last representable number in the sequence.
		         ]"
	output         : "[
    			   Per requirements of the RosettaCode example, execution will produce items of indexes 1 to 20 and 1691.
    			   The algorithm (procedure `hamming') is more general and will produce the first `n' Hamming numbers
    			   for any `n'.
    			  ]"
	source         : "This problem was posed in Edsger W. Dijkstra, A Discipline of Programming, Prentice Hall, 1978"
	date           : "8 August 2012"
	authors        : "Bertrand Meyer", "Emmanuel Stapf"
	revision       : "1.0"
	libraries      : "Relies on SORTED_TWO_WAY_LIST from EiffelBase"
	implementation : "[
			   Using SORTED_TWO_WAY_LIST provides an elegant illustration of how to implement
			   a lazy scheme in Eiffel through the use of object-oriented data structures.
			 ]"
	warning        : "[
			   The formatting (<lang>) specifications for Eiffel in RosettaCode are slightly obsolete:
			   `note' and other newer keywords not supported, red color for manifest strings.
			   This should be fixed soon.
		         ]"

class
	APPLICATION

create
	make

feature {NONE} -- Initialization

	make
			-- Print first 20 Hamming numbers, in order, and the 1691-st one.
		local
			Hammings: like hamming
				-- List of Hamming numbers, up to 1691-st one.
		do
			Hammings := hamming (1691)
			across 1 |..| 20 as i loop
				io.put_natural (Hammings.i_th (i.item)); io.put_string (" ")
			end
			io.put_new_line; io.put_natural (Hammings.i_th (1691)); io.put_new_line
		end

feature -- Basic operations

	hamming (n: INTEGER): ARRAYED_LIST [NATURAL]
			-- First `n' elements (in order) of the Hamming sequence,
			-- or as many of them as will not produce overflow.
		local
			sl: SORTED_TWO_WAY_LIST [NATURAL]
			overflow: BOOLEAN
			first, next: NATURAL
		do
			create Result.make (n); create sl.make
			sl.extend (1); sl.start
			across 1 |..| n as i invariant
				-- "The numbers output so far are the first `i' - 1 Hamming numbers, in order".
				-- "Result.first is the `i'-th Hamming number."
			until sl.is_empty loop
				first := sl.first; sl.start
				Result.extend (first); sl.remove
				across << 2, 3, 5 >> as multiplier loop
					next := multiplier.item * first
					overflow := overflow or next <= first
					if not overflow and then not sl.has (next) then sl.extend (next) end
				end
			end
		end
end

```


{{out}}

```txt
1
1 2 3 4 5 6 8 9 10 12 15 16 18 20 24 25 27 30 32 36
2125764000
```



## Elixir


```elixir
defmodule Hamming do
  def generater do
    queues = [{2, queue}, {3, queue}, {5, queue}]
    Stream.unfold({1, queues}, fn {n, q} -> next(n, q) end)
  end

  defp next(n, queues) do
    queues = Enum.map(queues, fn {m, queue} -> {m, push(queue, m*n)} end)
    min    = Enum.map(queues, fn {_, queue} -> top(queue) end) |> Enum.min
    queues = Enum.map(queues, fn {m, queue} ->
               {m, (if min==top(queue), do: erase_top(queue), else: queue)}
             end)
    {n, {min, queues}}
  end

  defp queue, do: {[], []}

  defp push({input, output}, term), do: {[term | input], output}

  defp top({input, []}), do: List.last(input)
  defp top({_, [h|_]}), do: h

  defp erase_top({input, []}), do: erase_top({[], Enum.reverse(input)})
  defp erase_top({input, [_|t]}), do: {input, t}
end

IO.puts "first twenty Hamming numbers:"
IO.inspect Hamming.generater |> Enum.take(20)
IO.puts "1691st Hamming number:"
IO.puts Hamming.generater |> Enum.take(1691) |> List.last
IO.puts "one millionth Hamming number:"
IO.puts Hamming.generater |> Enum.take(1_000_000) |> List.last
```


{{out}}

```txt

first twenty Hamming numbers:
[1, 2, 3, 4, 5, 6, 8, 9, 10, 12, 15, 16, 18, 20, 24, 25, 27, 30, 32, 36]
1691st Hamming number:
2125764000
one millionth Hamming number:
519312780448388736089589843750000000000000000000000000000000000000000000000000000000

```



## ERRE

For bigger numbers, you have to use an external program, like MULPREC.R

```ERRE
PROGRAM HAMMING

!$DOUBLE

DIM H[2000]

PROCEDURE HAMMING(L%->RES)
      LOCAL I%,J%,K%,N%,M,X2,X3,X5
      H[0]=1
      X2=2  X3=3  X5=5
      FOR N%=1 TO L%-1 DO
        M=X2
        IF M>X3 THEN M=X3 END IF
        IF M>X5 THEN M=X5 END IF
        H[N%]=M
        IF M=X2 THEN I%+=1  X2=2*H[I%]  END IF
        IF M=X3 THEN J%+=1  X3=3*H[J%]  END IF
        IF M=X5 THEN K%+=1  X5=5*H[K%]  END IF
      END FOR
      RES=H[L%-1]
END PROCEDURE

BEGIN
      FOR H%=1 TO 20 DO
        HAMMING(H%->RES)
        PRINT("H(";H%;")=";RES)
      END FOR
      HAMMING(1691->RES)
      PRINT("H(1691)=";RES)
END PROGRAM
```

{{out}}
```txt
H( 1 )= 1
H( 2 )= 2
H( 3 )= 3
H( 4 )= 4
H( 5 )= 5
H( 6 )= 6
H( 7 )= 8
H( 8 )= 9
H( 9 )= 10
H( 10 )= 12
H( 11 )= 15
H( 12 )= 16
H( 13 )= 18
H( 14 )= 20
H( 15 )= 24
H( 16 )= 25
H( 17 )= 27
H( 18 )= 30
H( 19 )= 32
H( 20 )= 36
H(1691)= 2125764000

```



## F#

This version implements Dijkstra's merge solution, so is closely related to the Haskell classic version.

```fsharp
type LazyList<'a> = Cons of 'a * Lazy<LazyList<'a>> // ': fix colouring

let rec hammings() =
  let rec (-|-) (Cons(x, nxf) as xs) (Cons(y, nyf) as ys) =
    if x < y then Cons(x, lazy(nxf.Value -|- ys))
    elif x > y then Cons(y, lazy(xs -|- nyf.Value))
    else Cons(x, lazy(nxf.Value -|- nyf.Value))
  let rec inf_map f (Cons(x, nxf)) =
    Cons(f x, lazy(inf_map f nxf.Value))
  Cons(1I, lazy(let x = inf_map ((*) 2I) hamming
                let y = inf_map ((*) 3I) hamming
                let z = inf_map ((*) 5I) hamming
                x -|- y -|- z))

// testing...
[<EntryPoint>]
let main args =
  let rec iterLazyListFor f n (Cons(v, rf)) =
    if n > 0 then f v; iterLazyListFor f (n - 1) rf.Value
  let rec nthLazyList n ((Cons(v, rf)) as ll) =
    if n <= 1 then v else nthLazyList (n - 1) rf.Value
  printf "( "; iterLazyListFor (printf "%A ") 20 (hammings()); printfn ")"
  printfn "%A" (hammings() |> nthLazyList 1691)
  printfn "%A" (hammings() |> nthLazyList 1000000)
  0
```


The above code memory residency is quite high as it holds the entire lazy sequence in memory due to the reference preventing garbage collection as the sequence is consumed,

The following code reduces that high memory residency by making the routine a function and using internal local stream references for the intermediate streams so that they can be collected as the stream is consumed as long as no reference is held to the main results stream (which is not in the sample test functions); it also avoids duplication of factors by successively building up streams and further reduces memory use by ordering of the streams so that the least dense are determined first:
{{trans|Haskell}}

```fsharp
type LazyList<'a> = Cons of 'a * Lazy<LazyList<'a>> // ': fix colouring

let hamming() =
  let rec merge (Cons(x, f) as xs) (Cons(y, g) as ys) =
    if x < y then Cons(x, lazy(merge (f.Force()) ys))
    else Cons(y, lazy(merge xs (g.Force())))
  let rec smult m (Cons(x, rxs)) =
    Cons(m * x, lazy(smult m (rxs.Force())))
  let rec first = smult 5I (Cons(1I, lazy first))
  let u s n =
    let rec r = merge s (smult n (Cons(1I, lazy r))) in r
  (Cons(1I, lazy(Seq.fold u first [| 3I; 2I |])))
```


The above code can by used just by substituting it for the "hamming" binding and substituting "hamming()" for "hamming" in the main testing function calls (three places).

Both codes output the same results as follows but the second is over three times faster:
{{output}}

```txt

( 1 2 3 4 5 6 8 9 10 12 15 16 18 20 24 25 27 30 32 36 )
2125764000
519312780448388736089589843750000000000000000000000000000000000000000000000000000000

```


Both codes are over 10 times slower as compared to Haskell (or Kotline or Scala or Clojure) when all are written in exactly the same style, perhaps due in some small degree to the BigInteger implementation being much slower for these operations than GMP and the JVM's implementation of BigInteger, but is also about twice as slow as the same algorithm written in C#.  This seems to show up one of F#'s "warts" in that the implementation of closure functions enclosing free variables necessary to implement lazy as in the LazyList seem to be particularly inefficient even as compared to C#'s lambda functions (which are also less efficient than the others mentioned).


### Fast somewhat imperative sequence version using logarithms


Since the above pure functional approach isn't very efficient, a more imperative approach using "growable" arrays which are "drained" of unnecessary older values in blocks once the back pointer indices are advanced is used in the following code.  The code also implements an algorithm to avoid duplicate calculations and thus does the same number of operations as the above code but faster due to using integer and floating point operations rather an BigInteger ones.  Due to the "draining" the memory use is the same as the above by a constant factor.  However, note that other than the contents of these arrays, pure functional code using immutable values is used.  Note that the implementation of IEnumerable using sequences in F# is also not very efficient and a "roll-your-own" IEnumerable implementation would likely be two or three times faster:


```fsharp
open System.Numerics

let hammingsLog() = // imperative arrays, eliminates the BigInteger operations...
  let lb3 = 1.5849625007211561814537389439478 // Math.Log(3) / Math.Log(2);
  let lb5 = 2.3219280948873623478703194294894 // Math.Log(5) / Math.Log(2);
  let inline mul2 (lg, x2, x3, x5) = (lg + 1.0, x2 + 1u, x3, x5)
  let inline mul3 (lg, x2, x3, x5) = (lg + lb3, x2, x3 + 1u, x5)
  let inline mul5 (lg, x2, x3, x5) = (lg + lb5, x2, x3, x5 + 1u)
  let one = (0.0, 0u, 0u, 0u)
  let s532, mrg = one |> mul2, one |> mul3
  let s53 = one |> mul3 |> mul3 // equivalent to 9 for advance step
  let s5 = one |> mul5
  let h = ResizeArray<_>()
  let m = ResizeArray<_>()
  let inline drplg (_, x2, x3, x5) = (x2, x3, x5)
  let inline nontriv() = Seq.unfold (fun (i, j, s532, mrg, s53, s5) -> // THIS STILL IS PATTERN MATCHING!!!!!
    let inline (<) (lga, _, _, _) (lgb, _, _, _) = lga < lgb
    let nv, ni, nj, ns532, nmrg, ns53, ns5 =
      if s532 < mrg then h.Add(s532)
                         s532, i + 1, j, h.[i] |> mul2, mrg, s53, s5
      else if s53 < s5 then h.Add(mrg); m.Add(s53)
                            mrg, i, j + 1, s532, s53, m.[j] |> mul3, s5
           else h.Add(mrg); m.Add(s5)
                mrg, i, j, s532, s5, s53, s5 |> mul5
    let nj = if nj >= m.Capacity / 2 then m.RemoveRange(0, nj); 0 else nj
    let ni = if ni >= h.Capacity / 2 then h.RemoveRange(0, ni); 0 else ni
    Some (drplg nv, (ni, nj, ns532, nmrg, ns53, ns5))) (0,0,s532,mrg,s53,s5)
  seq { yield drplg one; yield! nontriv() } // this is very slow

let trival (x2, x3, x5) = // convert trival to BigInteger
  let rec loop n mlt rslt =
    if n <= 0u then rslt
    else loop (n - 1u) mlt (mlt * rslt)
  loop x2 2I 1I |> loop x3 3I |> loop x5 5I

[<EntryPoint>]
let main argv =
  printf "( "; hammingsLog() |> Seq take 20 |> Seq.iter (printf "%A " << trival); printfn ")"
  printfn "%A" (hammingsLog() |> Seq.nth (1691 - 1))
  let strt = System.DateTime.Now.Ticks

  let rslt = (hammingsLog()) |> Seq.nth (1000000 - 1)

  let stop = System.DateTime.Now.Ticks

  printfn "%A" (rslt |> trival)
  printfn "\r\nFound this last up to %A in %A milliseconds." topNum ((stop - strt) / 10000L)

  printf "\r\nPress any key to exit:"
  System.Console.ReadKey(true) |> ignore
  printfn ""
  0 // return an integer exit code
```


The above code produces the same outputs as above, but takes only about 300 milliseconds rather than over three seconds.

===Extremely fast non-enumerating version sorting values in error band===

If one is willing to forego sequences and just calculate the nth Hamming number, then some reading on the relationship between the size of numbers to the sequence numbers is helpful (Wikipedia: regular number). One finds that there is a very distinct relationship and that it quite quickly reduces to quite a small error band proportional to the log of the output value for larger ranges. Thus, the following code just scans for logarithmic representations to insert into a sequence for this top error band and extracts the correct nth representation from that band. It reduces time complexity to O(n^(2/3)) from O(n) for the sequence versions, but even more amazingly, reduces memory requirements to O(n^(1/3)) from O(n^(2/3)) and thus makes it possible to calculate very large values in the sequence on common personal computers. The code is as follows:
{{trans|Haskell}}

```fsharp
let nthHamming n =
  if n < 1UL then failwith "nthHamming; argument must be > 0!"
  if n < 2UL then 0u, 0u, 0u else // trivial case for first value of one
  let lb3 = 1.5849625007211561814537389439478 // Math.Log(3) / Math.Log(2);
  let lb5 = 2.3219280948873623478703194294894 // Math.Log(5) / Math.Log(2);
  let fctr = 6.0 * lb3 * lb5
  let crctn = 2.4534452978042592646620291867186 // Math.Log(Math.sqrt(30.0)) / Math.Log(2.0)
  let lbest = (fctr * double n) ** (1.0/3.0) - crctn // from WP formula
  let lbhi = lbest + 1.0 / lbest
  let lblo = 2.0 * lbest - lbhi // upper and lower bound of upper "band"
  let klmt = uint32 (lbhi / lb5)
  let rec loopk k kcnt kbnd =
    if k > klmt then kcnt, kbnd else
      let p = lbhi - double k * lb5
      let jlmt = uint32 (p / lb3)
      let rec loopj j jcnt jbnd =
        if j > jlmt then loopk (k + 1u) jcnt jbnd else
          let q = p - double j * lb3
          let i = uint32 q
          let lg = lbhi - q + double i // current log 2 value (estimated)
          let nbnd = if lg >= lblo then (lg, (uint32 i, j, k)) :: jbnd else jbnd
          loopj (j + 1u) (jcnt + uint64 i + 1UL) nbnd in loopj 0u kcnt kbnd
  let count, bnd = loopk 0u 0UL [] // 64-bit value so doesn't overflow
  if n > count then failwith "nthHamming:  band high estimate is too low!"
  let ndx = int (count - n)
  if ndx >= bnd.Length then failwith "NthHamming.findNth:  band low estimate is too high!"
  let sbnd = bnd |> List.sortBy (fun (lg, _) -> -lg) // sort in decending order
  let _, rslt = sbnd.[ndx]
  rslt

[<EntryPoint>]
let main argv =
  let topNum = 1000000UL
  printf "( "; {1..20} |> Seq.iter (printf "%A " << trival << nthHamming << uint64); printfn ")"
  printfn "%A" (nthHamming 1691UL |> trival)
  let rslt = nthHammingx topNum
  let strt = System.DateTime.Now.Ticks

  let rslt = nthHamming topNum

  let stop = System.DateTime.Now.Ticks

  let x2, x3, x5 = rslt
  printfn "2**%A times 3**%A times 5**%A" x2 x3 x5
  let lgrthm = log10 2.0 * (double x2 + (double x3 * log 3.0 + double x5 * log 5.0) / log 2.0)
  let exp = floor lgrthm |> int
  let mntsa = 10.0 ** (lgrthm - double exp)
  printfn "Approximately %AE+%A" mntsa exp
  let s = trival rslt |> string
  let lngth = s.Length
  printfn "Digits:  %A" lngth
  if lngth <= 10000 then
    {0..100..lngth-1}
      |> Seq.iter (fun i ->
        printfn "%s" (s.Substring(i, if i + 100 < lngth then 100 else lngth - i)))

  printfn "\r\nFound this last up to %A in %A milliseconds." topNum ((stop - strt) / 10000L)

  printf "\r\nPress any key to exit:"
  System.Console.ReadKey(true) |> ignore
  printfn ""
  0 // return an integer exit code
```

{{output}}

```txt
( 1 2 3 4 5 6 8 9 10 12 15 16 18 20 24 25 27 30 32 36 )
2125764000
2**55u times 3**47u times 5**64u
Approximately 5.193127804E+83
Digits:  84
519312780448388736089589843750000000000000000000000000000000000000000000000000000000

Found this last up to 1000000UL in 0L milliseconds.
```


Even though the above code is implemented in a completely functional style using immutable bindings and (non-lazy) lists (without closures), it is about as fast as implementations in the fastest of languages.  It is faster than the Haskell version due to that version using lazy lists with the overhead of creating the requisite "thunks".

It takes too short a time to be measured to calculate the millionth Hamming number, the billionth number in the sequence can be calculated in just about 15 milliseconds, the trillionth in about one second, the thousand trillionth in about a hundred seconds, and it should be possible to calculate the 10^19th value in less than a day (untested) on common personal computers. The (2^64 - 1)th value (18446744073709551615) cannot be calculated due to a slight overflow problem as it approaches that limit.

'''Enhancement to by able to find Hamming numbers beyond the ten trillionth one'''

Due to the limited 53-bit mantissa of 64-bit double floating piint numbers, the above code can't properly sort the error band for input arguments somewhere above 10**13; the following code makes the sort accurate by using a multi-precision logarithm representation of sufficient precision so that the sort is accurate for arguments well beyond the uint64 input argument range, at about a doubling in cost in execution speed:
{{trans|Haskell}}

```fsharp
let nthHamming n =
  if n < 1UL then failwith "nthHamming:  argument must be > 0!"
  if n < 2UL then 0u, 0u, 0u else // trivial case for first value of one
  let lb3 = 1.5849625007211561814537389439478 // Math.Log(3) / Math.Log(2);
  let lb5 = 2.3219280948873623478703194294894 // Math.Log(5) / Math.Log(2);
  let fctr = 6.0 * lb3 * lb5
  let crctn = 2.4534452978042592646620291867186 // Math.Log(Math.sqrt(30.0)) / Math.Log(2.0)
  let lbest = (fctr * double n) ** (1.0/3.0) - crctn // from WP formula
  let lbhi = lbest + 1.0/lbest
  let lblo = 2.0 * lbest - lbhi // upper and lower bound of upper "band"
  let bglb2 = 1267650600228229401496703205376I
  let bglb3 = 2009178665378409109047848542368I
  let bglb5 = 2943393543170754072109742145491I
  let klmt = uint32 (lbhi / lb5)
  let rec loopk k kcnt kbnd =
    if k > klmt then kcnt, kbnd else
      let p = lbhi - double k * lb5
      let jlmt = uint32 (p / lb3)
      let rec loopj j jcnt jbnd =
        if j > jlmt then loopk (k + 1u) jcnt jbnd else
          let q = p - double j * lb3
          let i = uint32 q
          let lg = lbhi - q + double i // current log 2 value (estimated)
          let nbnd = if lg < lblo then jbnd else
                       let bglg = bglb2 * bigint i + bglb3 * bigint j + bglb5 * bigint k in
                       (bglg, (uint32 i, j, k)) :: jbnd
          loopj (j + 1u) (jcnt + uint64 i + 1UL) nbnd in loopj 0u kcnt kbnd
  let count, bnd = loopk 0u 0UL [] // 64-bit value so doesn't overflow
  if n > count then failwith "nthHamming:  band high estimate is too low!"
  let ndx = int (count - n)
  if ndx >= bnd.Length then failwith "NthHamming.findNth:  band low estimate is too high!"
  let sbnd = bnd |> List.sortBy (fun (lg, _) -> -lg) // sort in decending order
  let _, rslt = sbnd.[ndx]
  rslt
```



## Factor

{{trans|Scala}}

```factor
USING: accessors deques dlists fry kernel make math math.order
;
IN: rosetta.hamming

TUPLE: hamming-iterator 2s 3s 5s ;

: <hamming-iterator> ( -- hamming-iterator )
    hamming-iterator new
        1 1dlist >>2s
        1 1dlist >>3s
        1 1dlist >>5s ;

: enqueue ( n hamming-iterator -- )
    [ [ 2 * ] [ 2s>> ] bi* push-back ]
    [ [ 3 * ] [ 3s>> ] bi* push-back ]
    [ [ 5 * ] [ 5s>> ] bi* push-back ] 2tri ;

: next ( hamming-iterator -- n )
    dup [ 2s>> ] [ 3s>> ] [ 5s>> ] tri
    3dup [ peek-front ] tri@ min min
    [
        '[
            dup peek-front _ =
            [ pop-front* ] [ drop ] if
        ] tri@
    ] [ swap enqueue ] [ ] tri ;

: next-n ( hamming-iterator n -- seq )
    swap '[ _ [ _ next , ] times ] { } make ;

: nth-from-now ( hamming-iterator n -- m )
    1 - over '[ _ next drop ] times next ;
```


  <hamming-iterator> 20 next-n .
  <hamming-iterator> 1691 nth-from-now .
  <hamming-iterator> 1000000 nth-from-now .

{{trans|Haskell}}
Lazy lists are quite slow in Factor, but still.

```factor
USING: combinators fry kernel lists lists.lazy locals math ;
IN: rosetta.hamming-lazy

:: sort-merge ( xs ys -- result )
    xs car :> x
    ys car :> y
    {
        { [ x y < ] [ [ x ] [ xs cdr ys sort-merge ] lazy-cons ] }
        { [ x y > ] [ [ y ] [ ys cdr xs sort-merge ] lazy-cons ] }
        [ [ x ] [ xs cdr ys cdr sort-merge ] lazy-cons ]
    } cond ;

:: hamming ( -- hamming )
    f :> h!
    [ 1 ] [
        h 2 3 5 [ '[ _ * ] lazy-map ] tri-curry@ tri
        sort-merge sort-merge
    ] lazy-cons h! h ;
```


  20 hamming ltake list>array .
  1690 hamming lnth .
  999999 hamming lnth .

=={{header|Fōrmulæ}}==

In [http://wiki.formulae.org/Hamming_numbers this] page you can see the solution of this task.

Fōrmulæ programs are not textual, visualization/edition of programs is done showing/manipulating structures but not text ([http://wiki.formulae.org/Editing_F%C5%8Drmul%C3%A6_expressions more info]). Moreover, there can be multiple visual representations of the same program. Even though it is possible to have textual representation &mdash;i.e. XML, JSON&mdash; they are intended for transportation effects more than visualization and edition.

The option to show Fōrmulæ programs and their results is showing images. Unfortunately images cannot be uploaded in Rosetta Code.


## Forth

{{works with |Gforth| 0.7.0}}
This version uses a compact representation of Hamming numbers: each
64-bit cell represents a number 2^l*3^m*5^n, where l, n, and m are
bitfields in the cell (20 bits for now).  It also uses a fixed-point logarithm to compare the Hamming numbers and prints them in factored form.  This code has been tested up to the 10^9th Hamming number.

```Forth
\ manipulating and computing with Hamming numbers:

: extract2 ( h -- l )
    40 rshift ;

: extract3 ( h -- m )
    20 rshift $fffff and ;

: extract5 ( h -- n )
    $fffff and ;

' + alias h* ( h1 h2 -- h )

: h. { h -- }
    ." 2^"  h extract2 0 .r
    ." *3^" h extract3 0 .r
    ." *5^" h extract5 . ;

\ the following numbers have been produced with bc -l as follows
1 62 lshift constant ldscale2
 7309349404307464679 constant ldscale3 \ 2^62*l(3)/l(2) (rounded up)
10708003330985790206 constant ldscale5 \ 2^62*l(5)/l(2) (rounded down)

: hld { h -- ud }
    \ ud is a scaled fixed-point representation of the logarithm dualis of h
    h extract2 ldscale2 um*
    h extract3 ldscale3 um* d+
    h extract5 ldscale5 um* d+ ;

: h<= ( h1 h2 -- f )
    2dup = if
        2drop true exit
    then
    hld rot hld assert( 2over 2over d<> )
    du>= ;

: hmin ( h1 h2 -- h )
    2dup h<= if
        drop
    else
        nip
    then ;

\ actual algorithm

0 value seq
variable seqlast 0 seqlast !

: lastseq ( -- u )
    \ last stored number in the sequence
    seq seqlast @ th @ ;

: genseq ( h1 "name" -- )
    \ h1 is the factor for the sequence
    create , 0 , \ factor and index of element used for last return
  does> ( -- u2 )
    \ u2 is the next number resulting from multiplying h1 with numbers
    \ in the sequence that is larger than the last number in the
    \ sequence
    dup @ lastseq { h1 l } cell+ dup @ begin ( index-addr index )
        seq over th @ h1 h* dup l h<= while
            drop 1+ repeat
    >r swap ! r> ;

$10000000000 genseq s2
$00000100000 genseq s3
$00000000001 genseq s5

: nextseq ( -- )
    s2 s3 hmin s5 hmin , 1 seqlast +! ;

: nthseq ( u1 -- h )
    \ the u1 th element in the sequence
    dup seqlast @ u+do
        nextseq
    loop
    1- 0 max cells seq + @ ;

: .nseq ( u1 -- )
    dup seqlast @ u+do
        nextseq
    loop
    0 u+do
        seq i th @ h.
    loop ;

here to seq
0 , \ that's 1

20 .nseq
cr    1691 nthseq h.
cr 1000000 nthseq h.
```

{{out}}

```txt

2^0*3^0*5^0 2^1*3^0*5^0 2^0*3^1*5^0 2^2*3^0*5^0 2^0*3^0*5^1 2^1*3^1*5^0 2^3*3^0*5^0 2^0*3^2*5^0 2^1*3^0*5^1 2^2*3^1*5^0 2^0*3^1*5^1 2^4*3^0*5^0 2^1*3^2*5^0 2^2*3^0*5^1 2^3*3^1*5^0 2^0*3^0*5^2 2^0*3^3*5^0 2^1*3^1*5^1 2^5*3^0*5^0 2^2*3^2*5^0
2^5*3^12*5^3
2^55*3^47*5^64

```

A smaller, less capable solution is presented here. It solves two out of three requirements and is ANS-Forth compliant.

```Forth
2000 cells constant /hamming
create hamming /hamming allot
                   ( n1 n2 n3 n4 n5 n6 n7 -- n3 n4 n5 n6 n1 n2 n8)
: min? >r dup r> min >r 2rot r> ;

: hit?             ( n1 n2 n3 n4 n5 n6 n7 n8 -- n3 n4 n9 n10 n1 n2 n7)
  >r 2dup =        \ compare number with found minimum
  if -rot drop 1+ hamming over cells + @ r@ * rot then
  r> drop >r 2rot r>
;                  \ if so, increment and rotate

: hamming#         ( n1 -- n2)
  1 hamming ! >r   \ set first cell and initialize parms
  0 5 over 3 over 2
  r@ 1 ?do         \ determine minimum and set cell
     dup min? min? min? dup hamming i cells + !
     2 hit? 5 hit? 3 hit? drop
  loop             \ find if minimum equals value
  2drop 2drop 2drop hamming r> 1- cells + @
;                  \ clean up stack and fetch hamming number

: test
  cr 21 1 ?do i . i hamming# . cr loop
  1691 hamming# . cr
;
```



## Fortran

{{works with|Fortran|90 and later}}
Using big_integer_module from here [http://fortran.com/big_integer_module.f95]

```fortran
program Hamming_Test
  use big_integer_module
  implicit none

  call Hamming(1,20)
  write(*,*)
  call Hamming(1691)
  write(*,*)
  call Hamming(1000000)

contains

subroutine Hamming(first, last)

  integer, intent(in) :: first
  integer, intent(in), optional :: last
  integer :: i, n, i2, i3, i5, lim
  type(big_integer), allocatable :: hnums(:)

  if(present(last)) then
    lim = last
  else
    lim = first
  end if

  if(first < 1 .or. lim > 2500000 ) then
    write(*,*) "Invalid input"
    return
  end if

  allocate(hnums(lim))

  i2 = 1 ;  i3 = 1 ; i5 = 1
  hnums(1) = 1
  n = 1
  do while(n < lim)
    n = n + 1
    hnums(n) = mini(2*hnums(i2), 3*hnums(i3), 5*hnums(i5))
    if(2*hnums(i2) == hnums(n)) i2 = i2 + 1
    if(3*hnums(i3) == hnums(n)) i3 = i3 + 1
    if(5*hnums(i5) == hnums(n)) i5 = i5 + 1
  end do

  if(present(last)) then
    do i = first, last
      call print_big(hnums(i))
      write(*, "(a)", advance="no") " "
    end do
  else
    call print_big(hnums(first))
  end if

  deallocate(hnums)
end subroutine

function mini(a, b, c)
  type(big_integer) :: mini
  type(big_integer), intent(in) :: a, b, c

  if(a < b ) then
    if(a < c) then
      mini = a
    else
      mini = c
    end if
  else if(b < c) then
    mini = b
  else
    mini = c
  end if
end function mini
end program
```

{{out}}

```txt
1 2 3 4 5 6 8 9 10 12 15 16 18 20 24 25 27 30 32 36
2125764000
519312780448388736089589843750000000000000000000000000000000000000000000000000000000
```



## FreeBASIC


```freebasic
' FB 1.05.0 Win64

' The biggest integer which FB supports natively is 8 bytes so unable
' to calculate 1 millionth Hamming number without using an external
' "bigint" library such as GMP

Function min(x As Integer, y As Integer) As Integer
  Return IIf(x < y, x, y)
End Function

Function hamming(n As Integer) As Integer
  Dim h(1 To n) As Integer
  h(1) = 1
  Dim As Integer i = 1, j = 1, k = 1
  Dim As Integer x2 = 2, x3 = 3, x5 = 5

  For m As Integer = 2 To n
    h(m) = min(x2, min(x3, x5))
    If h(m) = x2 Then
      i += 1
      x2 = 2 * h(i)
    End If
    If h(m) = x3 Then
      j += 1
      x3 = 3 * h(j)
    End if
    If h(m) = x5 Then
      k += 1
      x5 = 5 * h(k)
    End If
  Next

  Return h(n)
End Function

Print "The first 20 Hamming numbers are :"
For i As Integer = 1 To 20
  Print hamming(i); " ";
Next
Print : Print
Print "The 1691st hamming number is :"
Print hamming(1691)
Print
Print "Press any key to quit"
Sleep
```


{{out}}

```txt

The first 20 Hamming numbers are :
 1  2  3  4  5  6  8  9  10  12  15  16  18  20  24  25  27  30  32  36

The 1691st Hamming number is :
 2125764000

```



## FunL

{{trans|Scala}}

```funl
native scala.collection.mutable.Queue

val hamming =
  q2 = Queue()
  q3 = Queue()
  q5 = Queue()

  def enqueue( n ) =
    q2.enqueue( n*2 )
    q3.enqueue( n*3 )
    q5.enqueue( n*5 )

  def stream =
    val n = min( min(q2.head(), q3.head()), q5.head() )

    if q2.head() == n then q2.dequeue()
    if q3.head() == n then q3.dequeue()
    if q5.head() == n then q5.dequeue()

    enqueue( n )
    n # stream()

  for q <- [q2, q3, q5] do q.enqueue( 1 )

  stream()
```


{{trans|Haskell}}

```funl
val hamming = 1 # merge( map((2*), hamming), merge(map((3*), hamming), map((5*), hamming)) )

def
  merge( inx@x:_, iny@y:_ )
    | x < y     = x # merge( inx.tail(), iny )
    | x > y     = y # merge( inx, iny.tail() )
    | otherwise = merge( inx, iny.tail() )

println( hamming.take(20) )
println( hamming(1690) )
println( hamming(2000) )
```


{{out}}


```txt

[1, 2, 3, 4, 5, 6, 8, 9, 10, 12, 15, 16, 18, 20, 24, 25, 27, 30, 32, 36]
2125764000
8100000000

```



## Go

===Concise version using dynamic-programming===

```go
package main

import (
    "fmt"
    "math/big"
)

func min(a, b *big.Int) *big.Int {
    if a.Cmp(b) < 0 {
        return a
    }
    return b
}

func hamming(n int) []*big.Int {
    h := make([]*big.Int, n)
    h[0] = big.NewInt(1)
    two, three, five    := big.NewInt(2), big.NewInt(3), big.NewInt(5)
    next2, next3, next5 := big.NewInt(2), big.NewInt(3), big.NewInt(5)
    i, j, k := 0, 0, 0
    for m := 1; m < len(h); m++ {
        h[m] = new(big.Int).Set(min(next2, min(next3, next5)))
        if h[m].Cmp(next2) == 0 { i++; next2.Mul(  two, h[i]) }
        if h[m].Cmp(next3) == 0 { j++; next3.Mul(three, h[j]) }
        if h[m].Cmp(next5) == 0 { k++; next5.Mul( five, h[k]) }
    }
    return h
}

func main() {
    h := hamming(1e6)
    fmt.Println(h[:20])
    fmt.Println(h[1691-1])
    fmt.Println(h[len(h)-1])
}
```

{{out}}

```txt

[1 2 3 4 5 6 8 9 10 12 15 16 18 20 24 25 27 30 32 36]
2125764000
519312780448388736089589843750000000000000000000000000000000000000000000000000000000

```

===Longer version using dynamic-programming and logarithms===
More than 10 times faster.

```go
package main

import (
	"flag"
	"fmt"
	"log"
	"math"
	"math/big"
	"os"
)

var (
	// print the whole sequence or just one element?

	seqMode = flag.Bool("s", false, "sequence mode")
	// precomputed base-2 logarithms for 3 and 5
	lg3, lg5 float64 = math.Log2(3), math.Log2(5)

	// state of the three multiplied sequences
	front = [3]cursor{
		{0, 0, 1},   // 2
		{1, 0, lg3}, // 3
		{2, 0, lg5}, // 5
	}

	// table for dynamic-programming stored results
	table [][3]int16
)

type cursor struct {
	f  int     // index (0, 1, 2) corresponding to factor (2, 3, 5)
	i  int     // index into table for the entry being multiplied
	lg float64 // base-2 logarithm of the multiple (for ordering)
}

func (c *cursor) val() [3]int16 {
	x := table[c.i]
	x[c.f]++ // multiply by incrementing the exponent
	return x
}

func (c *cursor) advance() {
	c.i++
	// skip entries that would produce duplicates
	for (c.f < 2 && table[c.i][2] > 0) || (c.f < 1 && table[c.i][1] > 0) {
		c.i++
	}
	x := c.val()
	c.lg = float64(x[0]) + lg3*float64(x[1]) + lg5*float64(x[2])
}

func step() {
	table = append(table, front[0].val())
	front[0].advance()
	// re-establish sorted order
	if front[0].lg > front[1].lg {
		front[0], front[1] = front[1], front[0]
		if front[1].lg > front[2].lg {
			front[1], front[2] = front[2], front[1]
		}
	}
}
func show(elem [3]int16) {
	z := big.NewInt(1)
	for i, base := range []int64{2, 3, 5} {
		b := big.NewInt(base)
		x := big.NewInt(int64(elem[i]))
		z.Mul(z, b.Exp(b, x, nil))
	}
	fmt.Println(z)
}

func main() {
	log.SetPrefix(os.Args[0] + ": ")
	log.SetOutput(os.Stderr)
	flag.Parse()
	if flag.NArg() != 1 {
		log.Fatalln("need one positive integer argument")
	}
	var ordinal int // ordinal of last sequence element to compute
	_, err := fmt.Sscan(flag.Arg(0), &ordinal)
	if err != nil || ordinal <= 0 {
		log.Fatalln("argument must be a positive integer")
	}
	table = make([][3]int16, 1, ordinal)
	for i, n := 1, ordinal; i < n; i++ {
		if *seqMode {
			show(table[i-1])
		}
		step()
	}
	show(table[ordinal-1])
}
```

{{out}}

```txt

$ ./hamming -s 20 | xargs
1 2 3 4 5 6 8 9 10 12 15 16 18 20 24 25 27 30 32 36
$ time ./hamming 1000000
519312780448388736089589843750000000000000000000000000000000000000000000000000000000

real	0m0.110s
user	0m0.090s
sys	0m0.020s
$ uname -a
Linux lance 3.0-ARCH #1 SMP PREEMPT Sat Aug 6 16:18:35 CEST 2011 x86_64 Intel(R) Core(TM)2 Duo CPU P8400 @ 2.26GHz GenuineIntel GNU/Linux

```



### Low Memory Use Enumerating Version Eliminating Duplicates


While the above code is fast due to avoiding big.Int operations and being tuned to avoid duplication of work, it has two problems:  It uses memory at about six times the value of "n", the nth value and has a practical upper range where the logarithm estimate used to compare currently processed value round off error will become too big so that values will become not in proper order for some values for large ranges.  This latter problem could be fixed by using double precision of two 64-bit uint's for the accumulated estimate, but the algorithm would still consume quite a lot of memory.

The following algorithm implements a continuously increasing enumeration of the Hamming numbers at about the same speed as the first solution by eliminating duplicate calculations, by organizing the streams/lazylists so that the least dense ones are processed first, and by using local variables that don't retain the heads of the streams/lazylists so that they can be garbage collected as consumed.  In this way, the billionth value can be calculated using only about a billion bytes of memory (one sixth of the above), with most of that used for storage of the necessary big.Int's.  If a tweaked logarithm algorithm were used, it would reduce the memory use almost to zero and would speed it up, although not to the same extent as the immediately above code as much of the remaining time would be spent in allocation of new stream/lazylist values and garbage collection.

The program implements the memoized streams/lazylists with a "roll-your-own" implementation and only the necessary methods as required by this algorithm as Go does not have a library to supply such, and uses a function closure to implement a simple form of enumeration of the Hamming values.  It used "llmult to perform the function of the "map" function used in the Haskell code, which is to produce a new stream which has each value of the input stream multiplied by a constant.  Instead of the Haskell "foldl" function, this program uses a simple Go "for" comprehension of the input primes array.
{{trans|Haskell}}

```go
// Hamming project main.go
package main

import (
	"fmt"
	"math/big"
	"time"
)

type lazyList struct {
	head  *big.Int
	tail  *lazyList
	contf func() *lazyList
}

func (oll *lazyList) next() *lazyList {
	if oll.contf != nil { // not thread-safe
		oll.tail = oll.contf()
		oll.contf = nil
	}
	return oll.tail
}

func merge(a *lazyList, b *lazyList) *lazyList {
	rslt := new(lazyList)
	x := a.head
	y := b.head
	if x.Cmp(y) < 0 {
		rslt.head = x
		rslt.contf = func() *lazyList {
			return merge(a.next(), b)
		}
	} else {
		rslt.head = y
		rslt.contf = func() *lazyList {
			return merge(a, b.next())
		}
	}
	return rslt
}

func llmult(m *big.Int, ll *lazyList) *lazyList {
	rslt := new(lazyList)
	rslt.head = new(big.Int).Set(big.NewInt(0)).Mul(m, ll.head)
	rslt.contf = func() *lazyList {
		return llmult(m, ll.next())
	}
	return rslt
}

func u(s *lazyList, n *big.Int) *lazyList {
	rslt := new(lazyList)
	cr := new(lazyList)
	cr.head = big.NewInt(1)
	cr.contf = func() *lazyList {
		return rslt
	}
	if s == nil {
		rslt = llmult(n, cr)
	} else {
		rslt = merge(s, llmult(n, cr))
	}
	return rslt
}

func Hamming() func() *big.Int {
	prms := []int64{5, 3, 2}
	curr := new(lazyList)
	curr.head = big.NewInt(1)
	curr.contf = func() *lazyList {
		var r *lazyList = nil
		for _, v := range prms {
			r = u(r, big.NewInt(v))
		}
		return r
	}
	return func() *big.Int {
		temp := curr
		curr = curr.next()
		return temp.head
	}
}

func main() {
	n := 1000000

	hamiter := Hamming()
	rarr := make([]*big.Int, 20)
	for i, _ := range rarr {
		rarr[i] = hamiter()
	}
	fmt.Println(rarr)

	hamiter = Hamming()
	for i := 1; i < 1691; i++ {
		hamiter()
	}
	fmt.Println(hamiter())

	strt := time.Now()

	hamiter = Hamming()
	for i := 1; i < n; i++ {
		hamiter()
	}
	rslt := hamiter()

	end := time.Now()
	fmt.Printf("Found the %vth Hamming number as %v in %v.\r\n", n, rslt.String(), end.Sub(strt))
}

```


The outputs are about the same as the above versions.  In order to perform this algorithm, one can see how much more verbose Go is than more functional languages such as Haskell or F# for this primarily functional algorithm.

===Fast imperative version avoiding duplicates, reducing memory, and using logarithmic representation===

While the above version can calculate to larger ranges due to somewhat reduced memory use, it is still somewhat limited as to range by memory limits due to the increasing size of the big integers used, limited in speed due to those big integer calculations, and also limited in speed due to Go's slow memory allocations and de-allocations.  The following code uses combined techniques to overcome all three limitations: 1) as for other solutions on this page, it uses a representation using integer exponents of 2, 3, and 5 and a scaled integer logarithm only for value comparisons (scaled such that round-off errors aren't a factor over the applicable range); thus memory use per element is constant rather than growing with range for big integers, and operations are simple integer comparisons and additions and are thus very fast. 2) memory reductions are by draining the used arrays by batches (rather than one by one as above) in place to reduce the time required for constant allocations and de-allocations.  The code is as follows:
{{trans|Rust}}

```golang
package main

import (
	"fmt"
	"math/big"
	"time"
)

// constants as expanded integers to minimize round-off errors, and
// reduce execution time using integer operations not float...
const cLAA2 uint64 = 35184372088832 // 2.0f64.ln() * 2.0f64.powi(45)).round() as u64;
const cLBA2 uint64 = 55765910372219 // 3.0f64.ln() / 2.0f64.ln() * 2.0f64.powi(45)).round() as u64;
const cLCA2 uint64 = 81695582054030 // 5.0f64.ln() / 2.0f64.ln() * 2.0f64.powi(45)).round() as u64;

type logelm struct { // log representation of an element with only allowable powers
    exp2 uint16
    exp3 uint16
    exp5 uint16
    logr uint64 // log representation used for comparison only - not exact
}

func (self *logelm) lte(othr *logelm) bool {
    if self.logr <= othr.logr {
        return true
    } else {
        return false
    }
}
func (self *logelm) mul2() logelm {
    return logelm{
        exp2: self.exp2 + 1,
        exp3: self.exp3,
        exp5: self.exp5,
        logr: self.logr + cLAA2,
    }
}
func (self *logelm) mul3() logelm {
    return logelm{
        exp2: self.exp2,
        exp3: self.exp3 + 1,
        exp5: self.exp5,
        logr: self.logr + cLBA2,
    }
}
func (self *logelm) mul5() logelm {
    return logelm{
        exp2: self.exp2,
        exp3: self.exp3,
        exp5: self.exp5 + 1,
        logr: self.logr + cLCA2,
    }
}

func log_nodups_hamming(n uint) *big.Int {
    if n < 1 {
        panic("log_nodups_hamming: argument < 1!")
    }
    if n < 2 { // trivial case of first in sequence
        return big.NewInt(1)
    }
    if n > 1.2e15 {
        panic("log_nodups_hamming: argument too large!")
    }

    one := logelm{}
    next5, merge := one.mul5(), one.mul3()
    next53, next532 := merge.mul3(), one.mul2()

    g := make([]logelm, 1, 65536)
    g[0] = one // never used, just so append works
    h := make([]logelm, 1, 65536)
    h[0] = one // never used, just so append works

    i, j := 1, 1
    for m := uint(1); m < n; m++ {
    cph := cap(h)
    if i >= cph/2 {
            nm := copy(h[0:i], h[i:])
            h = h[0:nm:cph]
            i = 0
        }
        if next532.lte(&merge) {
            h = append(h, next532)
            next532 = h[i].mul2()
            i++
        } else {
            h = append(h, merge)
            if next53.lte(&next5) {
                merge = next53
                next53 = g[j].mul3()
                j++
            } else {
                merge = next5
                next5 = next5.mul5()
            }
            cpg := cap(g)
            if j >= cpg/2 {
                nm := copy(g[0:j], g[j:])
                g = g[0:nm:cpg]
                j = 0
            }
            g = append(g, merge)
        }
    }

    two, three, five := big.NewInt(2), big.NewInt(3), big.NewInt(5)
    o := h[len(h)-1] // convert last element to big integer...
    ob := big.NewInt(1)
    for i := uint16(0); i < o.exp2; i++ {
        ob.Mul(two, ob)
    }
    for i := uint16(0); i < o.exp3; i++ {
        ob.Mul(three, ob)
    }
    for i := uint16(0); i < o.exp5; i++ {
        ob.Mul(five, ob)
    }
    return ob
}

func main() {
    n := uint(1e6)

    rarr := make([]*big.Int, 20)
    for i, _ := range rarr {
        rarr[i] = log_nodumps_hamming(i)
    }
    fmt.Println(rarr)

    fmt.Println(log_nodups_hamming(1691))

    strt := time.Now()

    rslt := log_nodups_hamming(n)

    end := time.Now()

    rs := rslt.String()
    lrs := len(rs)
    fmt.Printf("%v digits:\r\n", lrs)
    ndx := 0
    for ; ndx < lrs-100; ndx += 100 {
        fmt.Println(rs[ndx : ndx+100])
    }
    fmt.Println(rs[ndx:])

    fmt.Printf("This last found the %vth hamming number in %v.\r\n", n, end.Sub(strt))
}
```

{{output}}

```txt
[1 2 3 4 5 6 8 9 10 12 15 16 18 20 24 25 27 30 32 36]
2125764000
84 digits:
519312780448388736089589843750000000000000000000000000000000000000000000000000000000
This last found the 1000000th hamming number in 10.0006ms.
```


The above code can produce the billionth hamming number (844 digits) in about 14 seconds and given a machine with over 9 Gigabytes, it can calculate to the limit of 1.2e13 (about 19,335 digits) in about a day or so.  Functional enumerating versions as the immediately precedent code, even if adapted to the logarithm algorithm, will take longer because of the time required for enumeration, but much worse is the time required for allocations/de-allocations (garbage collection) of individual elements rather than as here in batches and for the majority of times done in place not requiring allocation/de-allocation at all.


### Extremely fast version inserting logarithms into the top error band


The above code is not as fast as one can go as it is limited by the need to calculate all Hamming numbers in the sequence up to the required one:  some reading on the relationship between the size of numbers to the sequence numbers is helpful (Wikipedia: regular number). One finds that there is a very distinct relationship and that it quite quickly reduces to quite a small error band proportional to the log of the output value for larger ranges. Thus, the following code just scans for logarithmic representations to insert into a sequence for this top error band and extracts the correct nth representation from that band. It reduces time complexity to O(n^(2/3)) from O(n) for the sequence versions, but even more amazingly, reduces memory requirements to O(n^(1/3)) from O(n^(2/3)) and thus makes it possible to calculate very large values in the sequence on common personal computers. The code is as follows:
{{trans|Nim}}

```golang
package main

import (
   "fmt"
   "math"
   "math/big"
   "sort"
   "time"
)

type logrep struct {
   lg         float64
   x2, x3, x5 uint32
}
type logreps []logrep
func (s logreps) Len() int { // necessary methods for sorting
   return len(s)
}
func (s logreps) Swap(i, j int) {
   s[i], s[j] = s[j], s[i]
}
func (s logreps) Less(i, j int) bool {
   return s[j].lg < s[i].lg // sort in decreasing order (reverse order compare)
}

func nthHamming(n uint64) (uint32, uint32, uint32) {
   if n < 2 {
      if n < 1 {
         panic("nthHamming:  argument is zero!")
      }
      return 0, 0, 0
   }
   const lb3 = 1.5849625007211561814537389439478 // math.Log2(3.0)
   const lb5 = 2.3219280948873623478703194294894 // math.Log2(5.0)
   fctr := 6.0 * lb3 * lb5
   crctn := math.Log2(math.Sqrt(30.0)) // from WP formula
   lgest := math.Pow(fctr*float64(n), 1.0/3.0) - crctn
   var frctn float64
   if n < 1000000000 {
      frctn = 0.509
   } else {
      frctn = 0.106
   }
   lghi := math.Pow(fctr*(float64(n)+frctn*lgest), 1.0/3.0) - crctn
   lglo := 2.0*lgest - lghi // and a lower limit of the upper "band"
   var count uint64 = 0
   bnd := make(logreps, 0) // give it one value so doubling size works
   klmt := uint32(lghi/lb5) + 1
   for k := uint32(0); k < klmt; k++ {
      p := float64(k) * lb5
      jlmt := uint32((lghi-p)/lb3) + 1
      for j := uint32(0); j < jlmt; j++ {
         q := p + float64(j)*lb3
         ir := lghi - q
         lg := q + math.Floor(ir) // current log value estimated
         count += uint64(ir) + 1
         if lg >= lglo {
            bnd = append(bnd, logrep{lg, uint32(ir), j, k})
         }
      }
   }
   if n > count {
      panic("nthHamming:  band high estimate is too low!")
   }
   ndx := int(count - n)
   if ndx >= bnd.Len() {
      panic("nthHamming:  band low estimate is too high!")
   }
   sort.Sort(bnd) // sort decreasing order due definition of Less above

   rslt := bnd[ndx]
   return rslt.x2, rslt.x3, rslt.x5
}

func convertTpl2BigInt(x2, x3, x5 uint32) *big.Int {
   result := big.NewInt(1)
   two := big.NewInt(2)
   three := big.NewInt(3)
   five := big.NewInt(5)
   for i := uint32(0); i < x2; i++ {
      result.Mul(result, two)
   }
   for i := uint32(0); i < x3; i++ {
      result.Mul(result, three)
   }
   for i := uint32(0); i < x5; i++ {
      result.Mul(result, five)
   }
   return result
}

func main() {
   for i := 1; i <= 20; i++ {
      fmt.Printf("%v ", convertTpl2BigInt(nthHamming(uint64(i))))
   }
   fmt.Println()
   fmt.Println(convertTpl2BigInt(nthHamming(1691)))

   strt := time.Now()
   x2, x3, x5 := nthHamming(uint64(1e6))
   end := time.Now()

   fmt.Printf("2^%v times 3^%v times 5^%v\r\n", x2, x3, x5)
   lrslt := convertTpl2BigInt(x2, x3, x5)
   lgrslt := (float64(x2) + math.Log2(3.0)*float64(x3) +
               math.Log2(5.0)*float64(x5)) * math.Log10(2.0)
   exp := math.Floor(lgrslt)
   mant := math.Pow(10.0, lgrslt-exp)
   fmt.Printf("Approximately:  %vE+%v\r\n", mant, exp)
   rs := lrslt.String()
   lrs := len(rs)
   fmt.Printf("%v digits:\r\n", lrs)
   if lrs <= 10000 {
      ndx := 0
      for ; ndx < lrs-100; ndx += 100 {
         fmt.Println(rs[ndx : ndx+100])
      }
      fmt.Println(rs[ndx:])
   }

   fmt.Printf("This last found the %vth hamming number in %v.\r\n", n, end.Sub(strt))
}
```

{{output}}

```txt
1 2 3 4 5 6 8 9 10 12 15 16 18 20 24 25 27 30 32 36
2125764000
2^55 times 3^47 times 5^64
Approximately:  5.193127804483804E+83
84 digits:
519312780448388736089589843750000000000000000000000000000000000000000000000000000000
This last found the 1000000th hamming number in 0s.
```


As can be seen above, the time to calculate the millionth Hamming number is now too small to be measured. The billionth number in the sequence can be calculated in just about 15 milliseconds, the trillionth in about 1.5 seconds, the thousand trillionth in about 150 seconds, and it should be possible to calculate the 10^19th value in less than a day (untested) on common personal computers. The (2^64 - 1)th value (18446744073709551615th value) cannot be calculated due to a slight overflow problem as it approaches that limit.


## Haskell


### The classic version


```haskell
hamming = 1 : map (2*) hamming `union` map (3*) hamming `union` map (5*) hamming

union a@(x:xs) b@(y:ys) = case compare x y of
            LT -> x : union  xs  b
            EQ -> x : union  xs  ys
            GT -> y : union  a   ys

main = do
    print $ take 20 hamming
    print  (hamming !! (1691-1), hamming !! (1692-1))
    print $ hamming !! (1000000-1)

-- Output:
-- [1,2,3,4,5,6,8,9,10,12,15,16,18,20,24,25,27,30,32,36]
-- (2125764000,2147483648)
-- 519312780448388736089589843750000000000000000000000000000000000000000000000000000000
```

Runs in about a second on [http://ideone.com/q3fma Ideone.com].
The nested <code>union</code>s' effect is to produce the minimal value at each step,
with duplicates removed. As Haskell evaluation model is ''on-demand'',
the three <code>map</code> expressions are in effect iterators, maintaining hidden pointers back into the shared named storage with which they were each created ''(a name is a pointer/handle in Haskell; to name is to point at, to refer to, to take a handle on)''.

The amount of operations is constant for each number produced, so the time complexity should be <math>O(n)</math>. [http://ideone.com/k8PU3 Empirically], it is slightly above that and worsening, suggestive of extra cost of bignum arithmetics. [http://ideone.com/k8PU3 Using triples representation] with logarithm values for comparisons amends this problem, but runs ~ 1.2x slower for the 1,000,000.

This is what that [http://drdobbs.com/blogs/architecture-and-design/228700538 DDJ blog] "pseudo-C" code was transcribing, mentioned at [[#Python|the Python entry]] that started this task (<small> curiously, it is in almost word-for-word correspondence with Edsger Dijkstra's code from his book A Discipline of Programming, [http://web.cecs.pdx.edu/~cs410aph/Lectures/Smalltalk%20II/Dijkstra%20on%20Hamming's%20Problem.pdf p. 132] </small>). [[#D|D]], [[#Go|Go]], [[#PARI/GP|PARI/GP]], [[#Prolog|Prolog]] all implement the same idea of back-pointers into shared storage. A Haskell run-time system can actually [http://ideone.com/q3fma free up the storage] automatically at the start of the shared list and only keep the needed portion of it, from the <code>(5*)</code> back-pointer, &ndash; which is about <math>O(n^{2/3})</math> in length &ndash; behind the scenes, as long as there's no re-use evident in the code.


### Avoiding generation of duplicates


The classic version can be sped up quite a bit (about twice, with roughly the same  [http://en.wikipedia.org/wiki/Analysis_of_algorithms#Empirical_orders_of_growth empirical orders of growth]) by avoiding generation of duplicate values:

```haskell
hamming = 1 : foldr u [] [2,3,5] where
        u n s = -- fix (merge s . map (n*) . (1:))
                r where
                r = merge s (map (n*) (1:r))

merge [] b = b
merge a@(x:xs) b@(y:ys) | x < y     = x : merge xs b
                        | otherwise = y : merge a ys

main = do
	print $ take 20 (hamming ())
	print $ (hamming ()) !! 1690
	print $ (hamming ()) !! (1000000-1)
```



### Explicit multiples reinserting

This is a common approach which explicitly maintains an internal buffer of <math>O(n^{2/3})</math> elements, removing the numbers from its front and reinserting their 2- 3- and 5-multiples in order. It overproduces the sequence, stopping when the ''n''-th number is no longer needed instead of when it's first found. Also overworks by maintaining this buffer in total order where just heap would be sufficient. Worse, this particular version uses a sequential list for its buffer. That means <math>O(n^{2/3})</math> operations for each number, instead of <math>O(1)</math> of the above version (and thus <math>O(n^{5/3})</math> overall). Translation of [[#Java|Java]] (which does use priority queue though, so should have ''O''&zwj;&thinsp;&zwj;(''n''&zwj;&thinsp;&zwj;log''n'') operations overall). Uses <code>union</code> from the "classic" version above:

```Haskell
hamm n = drop n $ iterate (\(_,(a:t))-> (a,union t [2*a,3*a,5*a])) (0,[1])
```

{{out}}

```Haskell
*Main> map fst $ take 20 $ hamm 1
[1,2,3,4,5,6,8,9,10,12,15,16,18,20,24,25,27,30,32,36]

*Main> map fst $ take 2 $ hamm 1691
[2125764000,2147483648]

*Main> mapM_ print $ take 10 $ hamm 1
(1,[2,3,5])
(2,[3,4,5,6,10])
(3,[4,5,6,9,10,15])
(4,[5,6,8,9,10,12,15,20])
(5,[6,8,9,10,12,15,20,25])
(6,[8,9,10,12,15,18,20,25,30])
(8,[9,10,12,15,16,18,20,24,25,30,40])
(9,[10,12,15,16,18,20,24,25,27,30,40,45])
(10,[12,15,16,18,20,24,25,27,30,40,45,50])
(12,[15,16,18,20,24,25,27,30,36,40,45,50,60])

*Main> map (length.snd.head.hamm) [2000,4000,8000,16000]
[402,638,1007,1596]
```

Runs too slowly to reach 1,000,000, with empirical orders of growth above ''~''&zwj;&thinsp;&zwj;(''n''&zwj;&thinsp;&zwj;<sup>1.7</sup>&zwj;&thinsp;&zwj;) and worsening. Last two lines show the internal buffer's length for several sample ''n''&zwj;&thinsp;&zwj;s.


### Enumeration by a chain of folded merges



```Haskell
hamm = foldr merge1 [] . iterate (map (5*)) .
         foldr merge1 [] . iterate (map (3*)) $ iterate (2*) 1
    where
    merge1 (x:xs) ys = x : merge xs ys

{- 1,  2,  4,  8,  16,  32, ...
   3,  6, 12, 24,  48,  96, ...
   9, 18, 36, 72, 144, 288, ...
   27, ... -}
```


Uses <code>merge</code>, as there's no need for duplicates-removing <code>union</code> because each number is produced only once here, too.

The merges are arranged in a chain of folds. Might be suitable for parallel execution, because of their large number.

Twice slower than the classic version at producing ''1,000,000th'' Hamming number, and worsening, running at ~n<sup>1.14..1.16</sup> empirically (vs. the classic version's linear operations). This is surprisingly efficient considering the large number of merges going on (about ''300'' for the ''1Mth'' number, and ~3n<sup>1/3</sup> in general).

Can be ''significantly'' improved, both in time complexity and absolute run time, by replacing the linear fold with the tree-shaped <code>mergeAll</code> from the <code>Data.List.Ordered</code> module of <code>data-ordlist</code> package.


### Direct calculation through triples enumeration

It is also possible to more or less directly calculate the n-th Hamming number by enumerating (and counting) all the <code>(i,j,k)</code> triples below its [http://en.wikipedia.org/wiki/Regular_number#Number_theory estimated value] &ndash; with ordering according to their exponents, <code>i*ln2 + j*ln3 + k*ln5</code> &ndash; while storing only the "band" of topmost triples close enough to the target value (more in the [http://drdobbs.com/blogs/architecture-and-design/228700538 original post on DDJ]). The savings come from enumerating only pairs of indices, and finding the corresponding third index by a direct calculation, thus achieving the O(n^(2/3)) time complexity. Space complexity, with constant empirical estimation correction, is ~n^(2/3); but is further tweaked to ~n^(1/3) (following the idea from the entry below).

The total count of thus produced triples is then the band's topmost value's index in the Hamming sequence, 1-based. The ''n''th number in the sequence is then found by a simple lookup in the sorted band, provided it was wide enough. This produces the 1,000,000-th value   instantaneously. Following the 2017-10 IDEOne update to a faster 64-bit system, the 1 trillionth number [https://ideone.com/GfMUOT is found in 0.78s] on Ideone.com:

```haskell
-- directly find n-th Hamming number, in ~ O(n^{2/3}) time
-- based on "top band" idea by Louis Klauder, from DDJ discussion
-- by Will Ness, original post: drdobbs.com/blogs/architecture-and-design/228700538

{-# OPTIONS -O3 -XStrict -XBangPatterns #-}

import Data.List (sortBy, foldl') -- ' fix apostrophe coloring
import Data.Function (on)

main = let (r,t) = nthHam 1000000000000 in print t -- >> print (trival t)

trival (i,j,k) = 2^i * 3^j * 5^k

nthHam :: Int -> (Double, (Int, Int, Int))                -- ( 64bit: use Int!!!     NB! )
nthHam n                                                  -- n: 1-based: 1,2,3...
  | n <= 0    = error $ "n is 1--based: must be n > 0: " ++ show n
  | n < 2     = ( 0.0, (0, 0, 0) ) -- trivial case so estimation works for rest
  | w >= 1    = error $ "Breach of contract: (w < 1):  " ++ show w
  | m <  0    = error $ "Not enough triples generated: " ++ show ((c,n) :: (Int, Int))
  | m >= nb   = error $ "Generated band is too narrow: " ++ show (m,nb)
  | otherwise = sortBy (flip compare `on` fst) b !! m     -- m-th from top in sorted band
 where
  lb3 = logBase 2 3;  lb5 = logBase 2 5;  lb30_2 = logBase 2 30 / 2
  v = (6*lb3*lb5* fromIntegral n)**(1/3) - lb30_2         -- estimated logval, base 2
  estval n = (v + (1/v), 2/v)                             -- the space tweak! (thx, GBG!)
  (hi,w) = estval n                                       --   hi > logval > hi-w
  m      = fromIntegral (c - n)                           -- target index, from top
  nb     = length (b :: [(Double, (Int, Int, Int))])      -- length of the band
  (c,b) = foldl_ (\(c,b) (i,t)-> let c2=c+i in c2 `seq`   -- ( total count, the band )
                     case t of []-> (c2,b);[v]->(c2,v:b) ) (0,[])  -- ( =~= mconcat )
            [ ( fromIntegral i+1,                         -- total triples w/ this (j,k)
                [ (r,(i,j,k)) | frac < w ] )              -- store it, if inside band
              | k <- [ 0 .. floor ( hi   /lb5) ],  let p = fromIntegral k*lb5,
                j <- [ 0 .. floor ((hi-p)/lb3) ],  let q = fromIntegral j*lb3 + p,
                let (i,frac) = pr  (hi-q) ;            r = hi - frac  -- r = i + q
            ] where  pr      = properFraction             -- pr 1.24 => (1,0.24)
                     foldl_  = foldl'
```

{{out}}

```txt
-- time: 0.00s    memory: 4.8MB
(55,47,64)
519312780448388736089589843750000000000000000000000000000000000000000000000000000000
```


'''Using loops for a faster code, and a narrower band to save space'''

The DDJ article is not quite true to the Wikipedia article referenced in that it doesn't use the fact that the error term in the estimation of the log of the resulting value for the nth Hamming number is directly proportional to this same log result.  Using this fact, we are able to reduce the span of the "band" to only a constant fraction of the estimated log result for large n, and thus reduce memory space requirements to O(n^(1/3)) from O(n^(2/3)) for a considerable space saving for larger ranges.

As well, although it isn't quite as elegant in a Haskell style sense, one can get an additional constant factor in execution time by replacing the "loops" based on list comprehensions to tail-recursive function call "loops", as in the following code:


```haskell
{-# OPTIONS_GHC -O3 -XStrict #-}

import Data.Word
import Data.List (sortBy)
import Data.Function (on)

nthHam :: Word64 -> (Int, Int, Int)
nthHam n                                               -- n: 1-based 1,2,3...
  | n < 2     = case n of
                  0 -> error "nthHam:  Argument is zero!"
                  _ -> (0, 0, 0)                       -- trivial case for 1
  | m <  0    = error $ "Not enough triples generated: " ++ show (c,n)
  | m >= nb   = error $ "Generated band is too narrow: " ++ show (m,nb)
  | otherwise = case res of (_, tv) -> tv -- 2^i * 3^j * 5^k
 where
  lb3     = logBase 2 3; lb5 = logBase 2 5.0
  lbrt30  = logBase 2 $ sqrt 30 :: Double -- estimate adjustment as per WP
  lg2est  = (6 * lb3 * lb5 * fromIntegral n)**(1/3) - lbrt30 -- estimated logval, base 2
  (hi,lo) = (lg2est + 1/lg2est, 2 * lg2est - hi) -- hi > log2est > lo
  (c, b)  = let klmt = floor (hi / lb5)
                loopk k ck bndk =
                  if k > klmt then (ck, bndk) else
                  let p = hi - fromIntegral k * lb5; jlmt = floor (p / lb3)
                      loopj j cj bndj =
                        if j > jlmt then loopk (k + 1) cj bndj else
                        let q = p - fromIntegral j * lb3
                            (i, frac) = properFraction q
                            nj = j + 1; ncj = cj + fromIntegral i + 1
                            r = hi - frac
                            nbndj = i `seq` bndj `seq`
                                    if r < lo then bndj
                                    else case (r, (i, j, k)) of
                                           nhd -> nhd `seq` nhd : bndj
                        in ncj `seq` nbndj `seq` loopj nj ncj nbndj
                  in loopj 0 ck bndk
            in loopk 0 0 []
  (m,nb)  = ( fromIntegral $ c - n, length b )         -- m 0-based from top, |band|
  (s,res) = ( sortBy (flip compare `on` fst) b, s!!m ) -- sorted decreasing, result<

main = putStrLn $ show $ nthHam 1000000000000
```


This implementation can likely calculate the 10^19th Hamming number in less than a day and can't quite reach the (2^64-1)th (18446744073709551615th) Hamming due to a slight range overflow as it approaches this limit.  Maximum memory used to these limits is less than a few hundred Megabytes, so possible on typical personal computers given the required day or two of computing time.

[https://ideone.com/RnAh5X On IdeOne (64-bit)], this takes 0.03 seconds for the 10 billionth and 0.70 seconds for the trillionth number (October 2017 update to a faster 64-bit system).

'''Using "roll-your-own" extended precision logarithm values in the error band to extend range'''

All of these codes using algorithms can't do an accurate sort of the error band for arguments somewhere above 10^13 due to the limited precision of the Double logarithm values, but this is easily fixed by using "roll-your-own" Integer logarithm values as follows with very little cost in execution time as it only applies to the relatively very small error band:

```haskell
{-# OPTIONS_GHC -O3 -XStrict #-}

import Data.Word
import Data.List (sortBy)
import Data.Function (on)

nthHam :: Word64 -> (Int, Int, Int)
nthHam n                                               -- n: 1-based 1,2,3...
  | n < 2     = case n of
                  0 -> error "nthHam:  Argument is zero!"
                  _ -> (0, 0, 0)                       -- trivial case for 1
  | m <  0    = error $ "Not enough triples generated: " ++ show (c,n)
  | m >= nb   = error $ "Generated band is too narrow: " ++ show (m,nb)
  | otherwise = case res of (_, tv) -> tv -- 2^i * 3^j * 5^k
 where
  lb3     = logBase 2 3; lb5 = logBase 2 5.0
  lbrt30  = logBase 2 $ sqrt 30 :: Double -- estimate adjustment as per WP
  lg2est  = (6 * lb3 * lb5 * fromIntegral n)**(1/3) - lbrt30 -- estimated logval, base 2
  (hi,lo) = (lg2est + 1/lg2est, 2 * lg2est - hi) -- hi > log2est > lo
  bglb2 = 1267650600228229401496703205376 :: Integer
  bglb3 = 2009178665378409109047848542368 :: Integer
  bglb5 = 2943393543170754072109742145491 :: Integer
  (c, b)  = let klmt = floor (hi / lb5)
                loopk k ck bndk =
                  if k > klmt then (ck, bndk) else
                  let p = hi - fromIntegral k * lb5; jlmt = floor (p / lb3)
                      loopj j cj bndj =
                        if j > jlmt then loopk (k + 1) cj bndj else
                        let q = p - fromIntegral j * lb3
                            (i, frac) = properFraction q
                            nj = j + 1; ncj = cj + fromIntegral i + 1
                            r = hi - frac
                            nbndj = i `seq` bndj `seq`
                                    if r < lo then bndj
                                    else
                                      let bglg = bglb2 * fromIntegral i +
                                                   bglb3 * fromIntegral j +
                                                   bglb5 * fromIntegral k in
                                      bglg `seq` case (bglg, (i, j, k)) of
                                                   nhd -> nhd `seq` nhd : bndj
                        in ncj `seq` nbndj `seq` loopj nj ncj nbndj
                  in loopj 0 ck bndk
            in loopk 0 0 []
  (m,nb)  = ( fromIntegral $ c - n, length b )         -- m 0-based from top, |band|
--  (s,res) = (b, s!!m)
  (s,res) = ( sortBy (flip compare `on` fst) b, s!!m ) -- sorted decreasing, result<

main = putStrLn $ show $ nthHam 1000000000000
```


All of these codes run a constant factor faster using the forced "Strict" mode, which shows that it is very difficult to anticipate the Haskell strictness analyser, especially in the case of the first code using List comprehensions.

=={{header|Icon}} and {{header|Unicon}}==
This solution uses Unicon's object oriented extensions. An Icon only version has not been provided.

Lazy evaluation is used to improve performance.

```Unicon
# Lazily generate the three Hamming numbers that can be derived directly
#   from a known Hamming number h
class Triplet : Class (cv, ce)

    method nextVal()
        suspend cv := @ce
    end

    initially (baseNum)
        cv := 2*baseNum
        ce := create (3|5)*baseNum
end

# Generate Hamming numbers, in order.  Default is first 30
#  But an optional argument can be used to generate more (or less)
#   e.g. hamming 5000 generates the first 5000.
procedure main(args)
    limit := integer(args[1]) | 30
    every write("\t", generateHamming() \ limit)
end

# Do the work.   Start with known Hamming number 1 and maintain
#   a set of triplet Hamming numbers as they get derived from that
#   one.  Most of the code here is to figure out which Hamming
#   number is next in sequence (while removing duplicates)
procedure generateHamming()
    triplers := set()
    insert(triplers, Triplet(1))

    suspend 1
    repeat {
        # Pick a Hamming triplet that *may* have the next smallest number
        t1 := !triplers         # any will do to start

        every t1 ~=== (t2 := !triplers) do {
            if t1.cv > t2.cv then {
               # oops we were wrong, switch assumption
               t1 := t2
               }
            else if t1.cv = t2.cv then {
               # t2's value is a duplicate, so
               # advance triplet t2, if none left in t2, remove it
               t2.nextVal() | delete(triplers, t2)
               }
           }

        # Ok, t1 has the next Hamming number, grab it
        suspend t1.cv
        insert(triplers, Triplet(t1.cv))
        # Advance triplet t1, if none left in t1, remove it
        t1.nextVal() | delete(triplers, t1)
        }
end
```



## J

'''Solution:'''

A concise tacit expression using a (right) fold:

```j
hamming=: {. (/:~@~.@] , 2 3 5 * {)/@(1x ,~ i.@-)
```

'''Example usage:'''

```j
   hamming 20
1 2 3 4 5 6 8 9 10 12 15 16 18 20 24 25 27 30 32 36

   {: hamming 1691
2125764000
```

For the millionth (and billionth (1e9)) Hamming number see the <code>nh</code> verb from [[j:Essays/Hamming Number|Hamming Number essay]] on the J wiki.

'''Explanation:'''

I'll explain this J-sentence by dividing it in three parts from left to right omitting the leftmost <code>{.</code>:
* sort and remove duplicates

```j
 /:~@~.@]
```

* produce (the next) 3 elements by selection and multiplication:

```j
 2 3 5 * {
```

note that LHA (2 3 5 * {) RHA [http://www.jsoftware.com/help/dictionary/intro05.htm is equivalent] to

```j
 2 3 5 * LHA { RHA
```

* the RH part forms an array of descending indices and the initial Hamming number 1

```j
 (1x ,~ i.@-)
```

e.g. if we want the first 5 Hamming numbers, it produces the array:
 4 3 2 1 0 1
in other words, we compute a sequence which begins with the desired hamming sequence and then take the first n elements (which will be our desired hamming sequence)

```j
   ({. (/:~@~.@] , 2 3 5 * {)/@(1x ,~ i.@-)) 7
1 2 3 4 5 6 8
```

This starts using a descending sequence with 1 appended:

```j
    (1x ,~ i.@-) 7
6 5 4 3 2 1 0 1
```

and then the fold expression is inserted between these list elements and the result computed:

```j
   6(/:~@~.@] , 2 3 5 * {) 5(/:~@~.@] , 2 3 5 * {) 4(/:~@~.@] , 2 3 5 * {) 3(/:~@~.@] , 2 3 5 * {) 2(/:~@~.@] , 2 3 5 * {) 1(/:~@~.@] , 2 3 5 * {) 0(/:~@~.@] , 2 3 5 * {) 1
1 2 3 4 5 6 8 9 10 12 15 18 20 25 30 16 24 40
```

(Note: A train of verbs in J is evaluated by supplying arguments to the every other verb (counting from the right) and the combining these results with the remaining verbs.  Also: <code>{</code> has been implemented so that an index of 0 will select the only item from an array with no dimensions.)


## Java

{{works with|Java|1.5+}}
Has a common shortcoming of overproducing the sequence by about <math>O(n^{2/3})</math> entries, until the ''n''-th number is no longer needed, instead of stopping as soon as it is reached. See [[#Explicit_multiples_reinserting|Haskell]] for an illustration.

Inserting the top number's three multiples deep into the priority queue as it does, incurs extra cost for each number produced. To not worsen the expected algorithm complexity, the priority queue should have (amortized) <math>O(1)</math> implementation for both insertion and deletion operations but it looks like it's <math>O(\log n)</math> in Java.

```java5
import java.math.BigInteger;
import java.util.PriorityQueue;

final class Hamming {
    private static BigInteger THREE = BigInteger.valueOf(3);
    private static BigInteger FIVE = BigInteger.valueOf(5);

    private static void updateFrontier(BigInteger x,
                                       PriorityQueue<BigInteger> pq) {
        pq.offer(x.shiftLeft(1));
        pq.offer(x.multiply(THREE));
        pq.offer(x.multiply(FIVE));
    }

    public static BigInteger hamming(int n) {
        if (n <= 0)
            throw new IllegalArgumentException("Invalid parameter");
        PriorityQueue<BigInteger> frontier = new PriorityQueue<BigInteger>();
        updateFrontier(BigInteger.ONE, frontier);
        BigInteger lowest = BigInteger.ONE;
        for (int i = 1; i < n; i++) {
            lowest = frontier.poll();
            while (frontier.peek().equals(lowest))
                frontier.poll();
            updateFrontier(lowest, frontier);
        }
        return lowest;
    }

    public static void main(String[] args) {
        System.out.print("Hamming(1 .. 20) =");
        for (int i = 1; i < 21; i++)
             System.out.print(" " + hamming(i));
        System.out.println("\nHamming(1691) = " + hamming(1691));
        System.out.println("Hamming(1000000) = " + hamming(1000000));
    }
}
```

{{out}}

```txt
Hamming(1 .. 20) = 1 2 3 4 5 6 8 9 10 12 15 16 18 20 24 25 27 30 32 36
Hamming(1691) = 2125764000
Hamming(1000000) = 519312780448388736089589843750000000000000000000000000000000000000000000000000000000
```


Another possibility is to realize that Hamming numbers can be represented and stored as triples of nonnegative integers which are the powers of 2, 3 and 5, and do all operations needed by the algorithms directly on these triples without converting to <math>BigInteger</math>, which saves tremendous memory and time. Also, the search frontier through this three-dimensional grid can be generated in an order that never reaches the same state twice, so we don't need to keep track which states have already been encountered, saving even more memory. The objects of <math>HammingTriple</math> encode Hamming numbers in three fields <math>a</math>, <math>b</math> and <math>c</math>. Multiplying by 2, 3 and 5 can now be done just by incrementing that field. The order comparison of triples needed by the priority queue is implemented with simple logarithm formulas of multiplication and addition, resorting to conversion to <math>BigInteger</math> only in the rare cases that the floating point arithmetic produces too close results.


```java5

import java.math.BigInteger;
import java.util.*;


public class HammingTriple implements Comparable<HammingTriple> {

    // Precompute a couple of constants that we need all the time
    private static final BigInteger two = BigInteger.valueOf(2);
    private static final BigInteger three = BigInteger.valueOf(3);
    private static final BigInteger five = BigInteger.valueOf(5);
    private static final double logOf2 = Math.log(2);
    private static final double logOf3 = Math.log(3);
    private static final double logOf5 = Math.log(5);

    // The powers of this triple
    private int a, b, c;

    public HammingTriple(int a, int b, int c) {
        this.a = a; this.b = b; this.c = c;
    }

    public String toString() {
        return "[" + a + ", " + b + ", " + c + "]";
    }

    public BigInteger getValue() {
        return two.pow(a).multiply(three.pow(b)).multiply(five.pow(c));
    }

    public boolean equals(Object other) {
        if(other instanceof HammingTriple) {
            HammingTriple h = (HammingTriple) other;
            return this.a == h.a && this.b == h.b && this.c == h.c;
        }
        else { return false; }
    }

    // Return 0 if this == other, +1 if this > other, and -1 if this < other
    public int compareTo(HammingTriple other) {
        // equality
        if(this.a == other.a && this.b == other.b && this.c == other.c) {
            return 0;
        }
        // this dominates
        if(this.a >= other.a && this.b >= other.b && this.c >= other.c) {
            return +1;
        }
        // other dominates
        if(this.a <= other.a && this.b <= other.b && this.c <= other.c) {
            return -1;
        }

        // take the logarithms for comparison
        double log1 = this.a * logOf2 + this.b * logOf3 + this.c * logOf5;
        double log2 = other.a * logOf2 + other.b * logOf3 + other.c * logOf5;

        // are these different enough to be reliable?
        if(Math.abs(log1 - log2) > 0.0000001) {
            return (log1 < log2) ? -1: +1;
        }

        // oh well, looks like we have to do this the hard way
        return this.getValue().compareTo(other.getValue());
        // (getting this far should be pretty rare, though)
    }

    public static BigInteger computeHamming(int n, boolean verbose) {
        if(verbose) {
            System.out.println("Hamming number #" + n);
        }
        long startTime = System.currentTimeMillis();

        // The elements of the search frontier
        PriorityQueue<HammingTriple> frontierQ = new PriorityQueue<HammingTriple>();
        int maxFrontierSize = 1;

        // Initialize the frontier
        frontierQ.offer(new HammingTriple(0, 0, 0)); // 1

        while(true) {
            if(frontierQ.size() > maxFrontierSize) {
                maxFrontierSize = frontierQ.size();
            }
            // Pop out the next Hamming number from the frontier
            HammingTriple curr = frontierQ.poll();

            if(--n == 0) {
                if(verbose) {
                    System.out.println("Time: " + (System.currentTimeMillis() - startTime) + " ms");
                    System.out.println("Frontier max size: " + maxFrontierSize);
                    System.out.println("As powers: " + curr.toString());
                    System.out.println("As value: " + curr.getValue());
                }
                return curr.getValue();
            }

            // Current times five, if at origin in (a,b) plane
            if(curr.a == 0 && curr.b == 0) {
                frontierQ.offer(new HammingTriple(curr.a, curr.b, curr.c + 1));
            }
            // Current times three, if at line a == 0
            if(curr.a == 0) {
                frontierQ.offer(new HammingTriple(curr.a, curr.b + 1, curr.c));
            }
            // Current times two, unconditionally
            curr.a++;
            frontierQ.offer(curr); // reuse the current HammingTriple object
        }
    }
}

```


```txt

Hamming number #1000000
Time: 650 ms
Frontier max size: 10777
As powers: [55, 47, 64]
As value: 519312780448388736089589843750000000000000000000000000000000000000000000000000000000

Hamming number #1000000000
Time: 1763306 ms
Frontier max size: 1070167
As powers: [1334, 335, 404]
As value: 62160757555652448616308163328720720039470565190896527065916324096423370220027531418244175407
772567327803701726166152919355404186200255249167295000868314547113136940786355040041603128729517887
0364794838245609107270160079056207179759030665476588225699039176388785014115448224991592743918456282
8227449023750262318234797192076792208033475638322151983772515798004125909334741121595323950448656375
1044570269974247729669174417794061727369755885568000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000

```



### Alternative


This uses memoized streams - similar in principle to the classic lazy-evaluated sequence, but with a java flavor. Hope you like this recipe!


```java

import java.math.BigInteger;

public class Hamming
{
    public static void main(String args[])
    {
        Stream hamming = makeHamming();
        System.out.print("H[1..20] ");
        for (int i=0; i<20; i++) {
            System.out.print(hamming.value());
            System.out.print(" ");
            hamming = hamming.advance();
        }
        System.out.println();

        System.out.print("H[1691] ");
        hamming = makeHamming();
        for (int i=1; i<1691; i++) {
            hamming = hamming.advance();
        }
        System.out.println(hamming.value());

        hamming = makeHamming();
        System.out.print("H[10^6] ");
        for (int i=1; i<1000000; i++) {
            hamming = hamming.advance();
        }
        System.out.println(hamming.value());
    }

    public interface Stream
    {
        BigInteger value();
        Stream advance();
    }

    public static class MultStream implements Stream
    {
        MultStream(int mult)
        { m_mult = BigInteger.valueOf(mult); }
        MultStream setBase(Stream s)
        { m_base = s; return this; }
        public BigInteger value()
        { return m_mult.multiply(m_base.value()); }
        public Stream advance()
        { return setBase(m_base.advance()); }

        private final BigInteger m_mult;
        private Stream m_base;
    }

    private final static class RegularStream implements Stream
    {
        RegularStream(Stream[] streams, BigInteger val)
        {
            m_streams = streams;
            m_val = val;
        }
        public BigInteger value()
        { return m_val; }

        public Stream advance()
        {
            // memoized value for the next stream instance.
            if (m_advance != null) { return m_advance; }

            int minidx = 0 ;
            BigInteger next = nextStreamValue(0);
            for (int i=1; i<m_streams.length; i++) {
                BigInteger v = nextStreamValue(i);
                if (v.compareTo(next) < 0) {
                    next = v;
                    minidx = i;
                }
            }
            RegularStream ret = new RegularStream(m_streams, next);
            // memoize the value!
            m_advance = ret;
            m_streams[minidx].advance();
            return ret;
        }
        private BigInteger nextStreamValue(int streamidx)
        {
            // skip past duplicates in the streams we're merging.
            BigInteger ret = m_streams[streamidx].value();
            while (ret.equals(m_val)) {
                m_streams[streamidx] = m_streams[streamidx].advance();
                ret = m_streams[streamidx].value();
            }
            return ret;
        }
        private final Stream[] m_streams;
        private final BigInteger m_val;
        private RegularStream m_advance = null;
    }

    private final static Stream makeHamming()
    {
        MultStream nums[] = new MultStream[] {
            new MultStream(2),
            new MultStream(3),
            new MultStream(5)
        };
        Stream ret = new RegularStream(nums, BigInteger.ONE);
        for (int i=0; i<nums.length; i++) {
            nums[i].setBase(ret);
        }
        return ret;
    }
}

```



```txt

$ java Hamming
H[1..20] 1 2 3 4 5 6 8 9 10 12 15 16 18 20 24 25 27 30 32 36
H[1691] 2125764000
H[10^6] 519312780448388736089589843750000000000000000000000000000000000000000000000000000000
$

```



## JavaScript

{{works with|JavaScript|1.7}} {{works with|Firefox|2}}
{{trans|Ruby}}
This does not calculate the 1,000,000th Hamming number.

Note the use of <code>'''for''' (x in obj)</code> to iterate over the ''properties'' of an object, versus <code>'''for each''' (y in obj)</code> to iterate over the ''values'' of the properties of an object.

```javascript
function hamming() {
    var queues = {2: [], 3: [], 5: []};
    var base;
    var next_ham = 1;
    while (true) {
        yield next_ham;

        for (base in queues) {queues[base].push(next_ham * base)}

        next_ham = [ queue[0] for each (queue in queues) ].reduce(function(min, val) {
            return Math.min(min,val)
        });

        for (base in queues) {if (queues[base][0] == next_ham) queues[base].shift()}
    }
}

var ham = hamming();
var first20=[], i=1;

for (; i <= 20; i++)
    first20.push(ham.next());
print(first20.join(', '));
print('...');
for (; i <= 1690; i++)
    ham.next();
print(i + " => " + ham.next());
```

{{out}}

```txt
1, 2, 3, 4, 5, 6, 8, 9, 10, 12, 15, 16, 18, 20, 24, 25, 27, 30, 32, 36
...
1691 => 2125764000
```


===Fast & complete version===

{{trans|C#}}

A translation of my fast C# version. I was curious to see how much slower JavaScript is. The result: it runs about 5x times slower than C#, though YMMV. You can try it yourself here: http://jsfiddle.net/N7AFN/

--Mike Lorenz


```javascript><html

<head></head>
<body>
    <div id="main"></div>
</body>
<script src="http://code.jquery.com/jquery-latest.min.js"></script>
<script src="http://peterolson.github.com/BigInteger.js/BigInteger.min.js"></script>
<script type="text/javascript">
    var _primes = [2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37];

    function log(text) {
        $('#main').append(text + "\n");
    }

    function big(exponents) {
        var i, e, val = bigInt.one;
        for (i = 0; i < exponents.length; i++)
            for (e = 0; e < exponents[i]; e++)
                val = val.times(_primes[i]);
        return val.toString();
    }

    function hamming(n, nprimes) {
        var i, iter, p, q, min, equal, x;

        var hammings = new Array(n);                            // array of hamming #s we generate
        hammings[0] = new Array(nprimes);
        for (p = 0; p < nprimes; p++) {
            hammings[0][p] = 0;
        }

        var hammlogs = new Array(n);                            // log values for above
        hammlogs[0] = 0;

        var primelogs = new Array(nprimes);                     // pre-calculated prime log values
        var listlogs  = new Array(nprimes);                     // log values of list heads
        for (p = 0; p < nprimes; p++) {
            primelogs[p] = listlogs[p] = Math.log(_primes[p]);
        }

        var indexes = new Array(nprimes);                       // intermediate hamming values as indexes into hammings
        for (p = 0; p < nprimes; p++) {
            indexes[p] = 0;
        }

        var listheads = new Array(nprimes);                     // intermediate hamming list heads
        for (p = 0; p < nprimes; p++) {
            listheads[p] = new Array(nprimes);
            for (q = 0; q < nprimes; q++) {
                listheads[p][q] = 0;
            }
            listheads[p][p] = 1;
        }

        for (iter = 1; iter < n; iter++) {
            min = 0;
            for (p = 1; p < nprimes; p++)
                if (listlogs[p] < listlogs[min])
                    min = p;
            hammlogs[iter] = listlogs[min];                     // that's the next hamming number
            hammings[iter] = listheads[min].slice();
            for (p = 0; p < nprimes; p++) {                     // update each list head if it matches new value
                equal = true;                                   // test each exponent to see if number matches
                for (i = 0; i < nprimes; i++) {
                    if (hammings[iter][i] != listheads[p][i]) {
                        equal = false;
                        break;
                    }
                }
                if (equal) {                                    // if it matches...
                    x = ++indexes[p];                           // set index to next hamming number
                    listheads[p] = hammings[x].slice();         // copy hamming number
                    listheads[p][p] += 1;                       // increment exponent = mult by prime
                    listlogs[p] = hammlogs[x] + primelogs[p];   // add log(prime) to log(value) = mult by prime
                }
            }
        }

        return hammings[n - 1];
    }

    $(document).ready(function() {
        var i, nprimes;
        var t = [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,1691,1000000];

        for (nprimes = 3; nprimes <= 4; nprimes++) {
            var start = new Date();
            log('<h1>' + _primes[nprimes - 1] + '-Smooth:' + '</h1>');
            log('<table>');
            for (i = 0; i < t.length; i++)
                log('<tr>' + '<td>' + t[i] + ':' + '</td><td>' + big(hamming(t[i], nprimes)) + '</td>');
            var end = new Date();
            log('<tr>' + '<td>' + 'Elapsed time:' + '</td><td>' + (end-start)/1000 + ' seconds' +  '</td>');
            log('</table>');
        }
    });
</script>
</html>
```


{{out}}

```txt
5-Smooth:

1:		1
2:		2
3:		3
4:		4
5:		5
6:		6
7:		8
8:		9
9:		10
10:		12
11:		15
12:		16
13:		18
14:		20
15:		24
16:		25
17:		27
18:		30
19:		32
20:		36
1691:		2125764000
1000000:	519312780448388736089589843750000000000000000000000000000000000000000000000000000000
Elapsed time:	1.73 seconds

7-Smooth:

1:		1
2:		2
3:		3
4:		4
5:		5
6:		6
7:		7
8:		8
9:		9
10:		10
11:		12
12:		14
13:		15
14:		16
15:		18
16:		20
17:		21
18:		24
19:		25
20:		27
1691:		3317760
1000000:	4157409948433216829957008507500000000
Elapsed time:	1.989 seconds
```



## jq

{{works with| jq | 1.4}}

We take the primary challenge here to be to write a Hamming number generator
that can generate a given number of Hamming numbers, or the n-th Hamming number,
without storing previously generated numbers.

To motivate a more complex version, in Part 1 of this section
hamming(n) is defined as a generator of Hamming numbers, as numbers.
This function uses an efficient algorithm and can run indefinitely,
but it has one disadvantage: currently, jq converts large integers
to floating point approximations, and thus precision is lost.  For
example, it reports the millionth Hamming number as
1.926511252902403e+44.

In Part 2, the algorithm in the first part is modified to use the
[p,q,r] representation of Hamming numbers, where p, q, and r are the
relevant exponents respectively of 2, 3, and 5.

The task description focuses on finding the n-th element of an
infinite sequence and so it should be mentioned that using jq
versions greater than 1.4, it would be possible to simply the
generator so that is always unbounded, and then harness it with
new builtins such as "limit" and "nth".

### = Hamming number generator =


```jq
# Return the index in the input array of the min_by(f) value
def index_min_by(f):
  . as $in
  | if length == 0 then null
    else .[0] as $first
    | reduce range(0; length) as $i
        ([0, $first, ($first|f)];   # state: [ix; min; f|min]
         ($in[$i]|f) as $v
         | if $v < .[2] then [ $i, $in[$i], $v ] else . end)
    | .[0]
    end;

# Emit n Hamming numbers if n>0; the nth if n<0
def hamming(n):

  # input: [twos, threes, fives] of which at least one is assumed to be non-empty
  # output: the index of the array holding the min of the firsts
  def next: map( .[0] ) | index_min_by(.);

  # input: [value, [twos, threes, fives] ....]
  # ix is the index in [twos, threes, fives] of the array to be popped
  # output: [popped, updated_arrays ...]
  def pop(ix):
    .[1] as $triple
    | setpath([0];    $triple[ix][0])
    | setpath([1,ix]; $triple[ix][1:]);

  # input: [x, [twos, threes, fives], count]
  # push value*2 to twos, value*3 to threes, value*5 to fives and increment count
  def push(v):
    [.[0], [.[1][0] + [2*v], .[1][1] + [3*v], .[1][2] + [5*v]], .[2] + 1];

  # _hamming is the workhorse
  # input: [previous, [twos, threes, fives], count]
  def _hamming:
    .[0] as $previous
    | if (n > 0 and .[2] == n) or (n<0 and .[2] == -n) then $previous
      else (.[1]|next) as $ix     # $ix cannot be null
      | pop($ix)
      | .[0] as $next
      | (if $next == $previous then empty elif n>=0 then $previous else empty end),
        (if $next == $previous then . else push($next) end | _hamming)
      end;
  [1, [[2],[3],[5]], 1] | _hamming;

. as $n | hamming($n)
```

'''Examples''':

```jq
# First twenty:
hamming(20)
# See elsewhere for output

# 1691st Hamming number:
hamming(-1691)
# => 2125764000

# Millionth:
hamming(-1000000)
# => 1.926511252902403e+44

```


### = Hamming numbers as triples =

In this section, Hamming numbers are represented as triples,
[p,q,r], where p, q and r are the relevant powers of 2, 3, and 5
respectively.  We therefore begin with some functions for managing
Hamming numbers represented in this manner:

```jq
# The log (base e) of a Hamming triple:
def ln_hamming:
  if length != 3 then error("ln_hamming: \(.)") else . end
  | (.[0] * (2|log)) + (.[1] * (3|log)) + (.[2] * (5|log));

# The numeric value of a Hamming triple:
def hamming_tof: ln_hamming | exp;

def hamming_toi:
  def pow(n): . as $in | reduce range(0;n) as $i (1; . * $in);
  . as $in | (2|pow($in[0])) * (3|pow($in[1])) * (5|pow($in[2]));

# Return the index in the input array of the min_by(f) value
def index_min_by(f):
  . as $in
  | if length == 0 then null
    else .[0] as $first
    | reduce range(0; length) as $i
        ([0, $first, ($first|f)];   # state: [ix; min; f|min]
         ($in[$i]|f) as $v
         | if $v < .[2] then [ $i, $in[$i], $v ] else . end)
    | .[0]
    end;

# Emit n Hamming numbers (as triples) if n>0; the nth if n<0; otherwise indefinitely.
def hamming(n):

  # n must be 2, 3 or 5
  def hamming_times(n): n as $n
    | if $n==2 then .[0] += 1 elif $n==3 then .[1] += 1 else .[2] += 1 end;

  # input: [twos, threes, fives] of which at least one is assumed to be non-empty
  # output: the index of the array holding the min of the firsts
  def next: map( .[0] ) | index_min_by( ln_hamming );

  # input: [value, [twos, threes, fives] ....]
  # ix is the index in [twos, threes, fives] of the array to be popped
  # output: [popped, updated_arrays ...]
  def pop(ix):
    .[1] as $triple
    | setpath([0];    $triple[ix][0])
    | setpath([1,ix]; $triple[ix][1:]);

  # input: [x, [twos, threes, fives], count]
  # push value*2 to twos, value*3 to threes, value*5 to fives and increment count
  def push(v):
    [.[0], [.[1][0] + [v|hamming_times(2)], .[1][1] + [v|hamming_times(3)],
            .[1][2] + [v|hamming_times(5)]], .[2] + 1];

  # _hamming is the workhorse
  # input: [previous, [twos, threes, fives], count]
  def _hamming:
    .[0] as $previous
    | if (n > 0 and .[2] == n) or (n<0 and .[2] == -n) then $previous
      else (.[1]|next) as $ix     # $ix cannot be null
      | pop($ix)
      | .[0] as $next
      | (if $next == $previous then empty elif n>=0 then $previous else empty end),
        (if $next == $previous then . else push($next) end | _hamming)
      end;
  [[0,0,0], [ [[1,0,0]] ,[[0,1,0]], [[0,0,1]] ], 1] | _hamming;

```

'''Examples'''

```jq
# The first twenty Hamming numbers as integers:
hamming(-20) | hamming_toi
# => (see elsewhere)

# 1691st as a Hamming triple:
hamming(-1691)
# => [5,12,3]

# The millionth:
hamming(-1000000)
# => [55,47,64]
```



## Julia

Simple brute force algorithm, derived from the discussion at ProgrammingPraxis.com.

```julia
function hammingsequence(N)
    if N < 1
        throw("Hamming sequence exponent must be a positive integer")
    end
    ham = N > 4000 ? Vector{BigInt}([1]) : Vector{Int}([1])
    base2, base3, base5 = (1, 1, 1)
    for i in 1:N-1
        x = min(2ham[base2], 3ham[base3], 5ham[base5])
        push!(ham, x)
        if 2ham[base2] <= x
            base2 += 1
        end
        if 3ham[base3] <= x
            base3 += 1
        end
        if 5ham[base5] <= x
            base5 += 1
        end
    end
    ham
end

println(hammingsequence(20))
println(hammingsequence(1691)[end])
println(hammingsequence(1000000)[end])
```
{{output}}
```txt

[1, 2, 3, 4, 5, 6, 8, 9, 10, 12, 15, 16, 18, 20, 24, 25, 27, 30, 32, 36]
2125764000
519312780448388736089589843750000000000000000000000000000000000000000000000000000000

```


The above code is terribly inefficient, just as said, but can be improved by about a factor of two by using intermediate variables (next2, next3, and next5) to avoid recalculating the long multi-precision integers for each comparison, as it seems that the Julia compiler (version 1.0.2) is not doing common sub expression elimination:

```julia
function hammingsequence(N::Int)
    if N < 1
        throw("Hamming sequence index must be a positive integer")
    end
    ham = Vector{BigInt}([1])
    base2, base3, base5 = 1, 1, 1
    next2, next3, next5 = BigInt(2), BigInt(3), BigInt(5)
    for _ in 1:N-1
        x = min(next2, next3, next5)
        push!(ham, x)
        next2 <= x && (base2 += 1; next2 = 2ham[base2])
        next3 <= x && (base3 += 1; next3 = 3ham[base3])
        next5 <= x && (base5 += 1; next5 = 5ham[base5])
    end
    ham
end
```


===Infinite generator, avoiding duplicates, using logarithms for faster processing===

The above code is slow for several reasons, partly because it is doing many multi-precision integer multiplications requiring much memory allocation and garbage collection for which the current Julia version 1.0.2 is quite slow, but also because there are many repeated calculations (3 time 2 equals 2 times three, etc.).  The following code is about 30 times faster by using floating point logarithms for multiplication and comparison; it also is an infinite generator (an iterator), which means that memory consumption can be greatly reduced by eliminating values which are no longer of any use:

```julia
function trival((twos, threes, fives))
    BigInt(2)^twos * BigInt(3)^threes * BigInt(5)^fives
end

mutable struct Hammings
    ham532 :: Vector{Tuple{Float64,Tuple{Int,Int,Int}}}
    ham53 :: Vector{Tuple{Float64,Tuple{Int,Int,Int}}}
    ndx532 :: Int
    ndx53 :: Int
    next2 :: Tuple{Float64,Tuple{Int,Int,Int}}
    next3 :: Tuple{Float64,Tuple{Int,Int,Int}}
    next5 :: Tuple{Float64,Tuple{Int,Int,Int}}
    next53 :: Tuple{Float64,Tuple{Int,Int,Int}}
    Hammings() = new(
        Vector{Tuple{Float64,Tuple{Int,Int,Int}}}(),
        Vector{Tuple{Float64,Tuple{Int,Int,Int}}}(),
        1, 1,
        (1.0, (1, 0, 0)), (log(2, 3), (0, 1, 0)),
        (log(2, 5), (0, 0, 1)), (0.0, (0, 0, 0))
    )
end
Base.eltype(::Type{Hammings}) = Tuple{Int,Int,Int}
function Base.iterate(HM::Hammings, st = HM) # :: Union{Nothing,Tuple{Tuple{Float64,Tuple{Int,Int,Int}},Hammings}}
    log2of2, log2of3, log2of5 = 1.0, log(2,3), log(2,5)
    if st.next2[1] < st.next53[1]
        push!(st.ham532, st.next2); st.ndx532 += 1
        last, (twos, threes, fives) = st.ham532[st.ndx532]
        st.next2 = (log2of2 + last, (twos + 1, threes, fives))
    else
        push!(st.ham532, st.next53)
        if st.next3[1] < st.next5[1]
            st.next53 = st.next3; push!(st.ham53, st.next3)
            last, (_, threes, fives) = st.ham53[st.ndx53]; st.ndx53 += 1
            st.next3 = (log2of3 + last, (0, threes + 1, fives))
        else
            st.next53 = st.next5; push!(st.ham53, st.next5)
            last, (_, _, fives) = st.next5
            st.next5 = (log2of5 + last, (0, 0, fives + 1))
        end
    end
    len53 = length(st.ham53)
    if st.ndx53 > (len53 >>> 1)
        nlen53 = len53 - st.ndx53 + 1
        copyto!(st.ham53, 1, st.ham53, st.ndx53, nlen53)
        resize!(st.ham53, nlen53); st.ndx53 = 1
    end
    len532 = length(st.ham532)
    if st.ndx532 > (len532 >>> 1)
        nlen532 = len532 - st.ndx532 + 1
        copyto!(st.ham532, 1, st.ham532, st.ndx532, nlen532)
        resize!(st.ham532, nlen532); st.ndx532 = 1
    end
    _, tri = st.ham532[end]
    tri, st
#    convert(Union{Nothing,Tuple{Tuple{Float64,Tuple{Int,Int,Int}},Hammings}},(st.ham532[end], st))
#   (length(st.ham532), length(st.ham53)), st
end

foreach(x -> print(trival(x)," "), (Iterators.take(Hammings(), 20))); println()
let count = 1691; for t in Hammings() count <= 1 && (println(trival(t)); break); count -= 1 end end
let count = 1000000; for t in Hammings() count <= 1 && (println(trival(t)); break); count -= 1 end end
```



### Determination of the nth Hamming number by processing of error band


For some phenomenal speed in determining the nth Hamming/regular number, one doesn't need to find all the values up to that limit but rather only the values within an error band which is a factor of two either way from the correct value; this has the advantage that the number of processing loops are reduced from O(n^3) to O(n^(2/3)) for a considerable saving for larger ranges and has the further advantage that memory consumption is reduced to O(n^(1/3)) meaning that huge ranges can be computed on a common desktop computer.  The folwingcode can compute the trillionth (10^12th) Hamming number is a couple of seconds:

```julia
function nthhamming(n :: UInt64) # :: Tuple{UInt32, UInt32, UInt32}
    # take care of trivial cases too small for band size estimation to work...
    n < 1 && throw("nthhamming:  argument must be greater than zero!!!")
    n < 2 && return (0, 0, 0)
    n < 3 && return (1, 0, 0)

    # some constants...
    log2of2, log2of3, log2of5 = 1.0, log(2, 3), log(2, 5)
    fctr, crctn = 6.0 * log2of3 * log2of5, log(2, sqrt(30))
    log2est = (fctr * Float64(n))^(1.0 / 3.0) - crctn # log2 answer from WP formula
    log2hi = log2est + 1.0 / log2est; width = 2.0 / log2est # up to 2X higher/lower

    # loop to find the count of regular numbers and band of possible candidates...
    count :: UInt64 = 0; band = Vector{Tuple{Float64,Tuple{UInt32,UInt32,UInt32}}}()
    fiveslmt = UInt32(ceil(log2hi / log2of5)); fives :: UInt32 = 0
    while fives < fiveslmt
        log2p = log2hi - fives * log2of5
        threeslmt = UInt32(ceil(log2p / log2of3)); threes :: UInt32 = 0
        while threes < threeslmt
            log2q = log2p - threes * log2of3
            twos = UInt32(floor(log2q)); frac = log2q - twos; count += twos + 1
            frac <= width && push!(band, (log2hi - frac, (twos, threes, fives)))
            threes += 1
        end
        fives += 1
    end

    # process the band found including checks for validity and range...
    n > count && throw("nthhamming:  band high estimate is too low!!!")
    ndx = count - n + 1
    ndx > length(band) && throw("nthhamming:  band width estimate is too narrow!!!")
    sort!(band, by=(tpl -> let (lg,_) = tpl; -lg end)) # sort in decending order

    # get and return the answer...
    _, tri = band[ndx]
    tri
end

foreach(x-> print(trival(nthhamming(UInt(x))), " "), 1:20); println()
println(trival(nthhamming(UInt64(1691))))
println(trival(nthhamming(UInt64(1000000))))
```


Above about a range of 10^13, a Float64 logarithm doesn't have enough precision to be able to sort the error band properly, so a refinement of using a "roll-your-own" extended precision logarithm must be used, as follows:

```julia
function nthhamming(n :: UInt64) # :: Tuple{UInt32, UInt32, UInt32}
    # take care of trivial cases too small for band size estimation to work...
    n < 1 && throw("nthhamming:  argument must be greater than zero!!!")
    n < 2 && return (0, 0, 0)
    n < 3 && return (1, 0, 0)

    # some constants...
    log2of2, log2of3, log2of5 = 1.0, log(2, 3), log(2, 5)
    fctr, crctn = 6.0 * log2of3 * log2of5, log(2, sqrt(30))
    log2est = (fctr * Float64(n))^(1.0 / 3.0) - crctn # log2 answer from WP formula
    log2hi = log2est + 1.0 / log2est; width = 2.0 / log2est # up to 2X higher/lower

    # some really really big constants representing the "roll-your-own" big logs...
    biglog2of2 = BigInt(1267650600228229401496703205376)
    biglog2of3 = BigInt(2009178665378409109047848542368)
    biglog2of5 = BigInt(2943393543170754072109742145491)

    # loop to find the count of regular numbers and band of possible candidates...
    count :: UInt64 = 0; band = Vector{Tuple{BigInt,Tuple{UInt32,UInt32,UInt32}}}()
    fiveslmt = UInt32(ceil(log2hi / log2of5)); fives :: UInt32 = 0
    while fives < fiveslmt
        log2p = log2hi - fives * log2of5
        threeslmt = UInt32(ceil(log2p / log2of3)); threes :: UInt32 = 0
        while threes < threeslmt
            log2q = log2p - threes * log2of3
            twos = UInt32(floor(log2q)); frac = log2q - twos; count += twos + 1
            if frac <= width
                biglog = biglog2of2 * twos + biglog2of3 * threes + biglog2of5 * fives
                push!(band, (biglog, (twos, threes, fives)))
            end
            threes += 1
        end
        fives += 1
    end

    # process the band found including checks for validity and range...
    n > count && throw("nthhamming:  band high estimate is too low!!!")
    ndx = count - n + 1
    ndx > length(band) && throw("nthhamming:  band width estimate is too narrow!!!")
    sort!(band, by=(tpl -> let (lg,_) = tpl; -lg end)) # sort in decending order

    # get and return the answer...
    _, tri = band[ndx]
    tri
end
```


The above code can find the trillionth Hamming number in about two seconds (very little slower) and the thousand trillionth value in about 192 seconds.  This routine would be able to find the million trillionth Hamming number in about 20,000 seconds or about five and a half hours.


## Kotlin

{{trans|Java}}


```scala
import java.math.BigInteger
import java.util.*

val Three = BigInteger.valueOf(3)!!
val Five = BigInteger.valueOf(5)!!

fun updateFrontier(x : BigInteger, pq : PriorityQueue<BigInteger>) {
    pq.add(x.shiftLeft(1))
    pq.add(x.multiply(Three))
    pq.add(x.multiply(Five))
}

fun hamming(n : Int) : BigInteger {
    val frontier = PriorityQueue<BigInteger>()
    updateFrontier(BigInteger.ONE, frontier)
    var lowest = BigInteger.ONE
    for (i in 1 .. n-1) {
        lowest = frontier.poll() ?: lowest
        while (frontier.peek() == lowest)
            frontier.poll()
        updateFrontier(lowest, frontier)
    }
    return lowest
}

fun main(args : Array<String>) {
    System.out.print("Hamming(1 .. 20) =")
    for (i in 1 .. 20)
        System.out.print(" ${hamming(i)}")
    System.out.println("\nHamming(1691) = ${hamming(1691)}")
    System.out.println("Hamming(1000000) = ${hamming(1000000)}")
}
```


{{out}}

```txt
Hamming(1 .. 20) = 1 2 3 4 5 6 8 9 10 12 15 16 18 20 24 25 27 30 32 36
Hamming(1691) = 2125764000
Hamming(1000000) = 519312780448388736089589843750000000000000000000000000000000000000000000000000000000
```



### Overloaded function:


```scala
import java.math.BigInteger
import java.util.*

val One = BigInteger.ONE!!
val Three = BigInteger.valueOf(3)!!
val Five = BigInteger.valueOf(5)!!

fun PriorityQueue<BigInteger>.update(x: BigInteger) : PriorityQueue<BigInteger> {
    add(x.shiftLeft(1))
    add(x.multiply(Three))
    add(x.multiply(Five))
    return this
}

fun hamming(n: Int): BigInteger {
    val frontier = PriorityQueue<BigInteger>().update(One)
    var lowest = One
    repeat(n - 1) {
        lowest = frontier.poll() ?: lowest
        while (frontier.peek() == lowest)
            frontier.poll()
        frontier.update(lowest)
    }
    return lowest
}

fun hamming(i : Iterable<Int>) : Iterable<BigInteger> = i.map { hamming(it) }

fun main(args: Array<String>) {
    val r = 1..20
    println("Hamming($r) = " + hamming(r))
    arrayOf(1691, 1000000).forEach { println("Hamming($it) = " + hamming(it)) }
}
```



### Recursive function:


```scala
import java.math.BigInteger
import java.util.*

val One = BigInteger.ONE!!
val Three = BigInteger.valueOf(3)!!
val Five = BigInteger.valueOf(5)!!

infix fun PriorityQueue<BigInteger>.update(x: BigInteger) : PriorityQueue<BigInteger> {
    add(x.shiftLeft(1))
    add(x.multiply(Three))
    add(x.multiply(Five))
    return this
}

fun hamming(a: Any?): Any = when (a) {
    is Number -> {
        val pq = PriorityQueue<BigInteger>() update One
        var lowest = One
        repeat(a.toInt() - 1) {
            lowest = pq.poll() ?: lowest
            while (pq.peek() == lowest) pq.poll()
            pq update lowest
        }
        lowest
    }
    is Iterable<*> -> a.map { hamming(it) }
    else -> throw IllegalArgumentException("cannot parse argument")
}

fun main(args: Array<String>) {
    arrayOf(1..20, 1691, 1000000).forEach { println("Hamming($it) = " + hamming(it)) }
}
```


{{out}}

```txt
Hamming(1..20) = [1, 2, 3, 4, 5, 6, 8, 9, 10, 12, 15, 16, 18, 20, 24, 25, 27, 30, 32, 36]
Hamming(1691) = 2125764000
Hamming(1000000) = 519312780448388736089589843750000000000000000000000000000000000000000000000000000000
```


===Functional Style Eliminating Duplicates, Optional Sequence Output===

The following code implements a functional version, with the only mutable state that required to implement a recursive binding as commented in the code.  It is fast because it uses non-genereric functions so that much of the boxing/unboxing can be optimized away, and it takes very little memory because of the avoiding duplicates, the order that the primes are processed with least dense first, and because it is implemented in such a way so as to use only local bindings for the heads of the lazy lists so that they can be consumed as used and garbage collected away.  Kotlin does not have a lazy list like Haskell or a memoized lazy Stream like Scala, so the code implements a basic version of LazyList to be used by the algorithm (Java 8 Streams are not memoized as required here):
{{trans|scala}}

```scala
import java.math.BigInteger as BI

data class LazyList<T>(val head: T, val lztail: Lazy<LazyList<T>?>) {
    fun toSequence() = generateSequence(this) { it.lztail.value }
            .map { it.head }
}

fun hamming(): LazyList<BI> {
    fun merge(s1: LazyList<BI>, s2: LazyList<BI>): LazyList<BI> {
        val s1v = s1.head; val s2v = s2.head
        if (s1v < s2v) {
            return LazyList(s1v, lazy({->merge(s1.lztail.value!!, s2)}))
        } else {
            return LazyList(s2v, lazy({->merge(s1, s2.lztail.value!!)}))
        }
    }
    fun llmult(m: BI, s: LazyList<BI>): LazyList<BI> {
        fun llmlt(ss: LazyList<BI>): LazyList<BI> {
            return LazyList(m * ss.head, lazy({->llmlt(ss.lztail.value!!)}))
        }
        return llmlt(s)
    }
    fun u(s: LazyList<BI>?, n: Long): LazyList<BI> {
        var r: LazyList<BI>? = null // mutable nullable so can do the below
        if (s == null) { // recursively referenced variables are ugly!!!
            r = llmult(BI.valueOf(n), LazyList(BI.valueOf(1), lazy{ -> r }))
        } else { // recursively referenced variables only work with lazy
            r = merge(s, llmult(BI.valueOf(n), // or a loop race limit
                                LazyList(BI.valueOf(1), lazy{ -> r })))
        }
        return r
    }
    val prms = arrayOf(5L, 3L, 2L)
    val thunk = {->prms.fold<Long,LazyList<BI>?>(null, {s, n -> u(s,n)})!!}
    return LazyList(BI.valueOf(1), lazy(thunk))
}

fun main(args: Array<String>) {
	tailrec fun nth(n: Int, h: LazyList<BI>): BI =
		if (n > 1) { nth(n - 1, h.lztail.value!!) }
		else { h.head } // non-generic faster: boxing optimized away
	println(hamming().toSequence().take(20).toList())
	println(nth(1691, hamming()))
	val strt = System.currentTimeMillis()
	println(nth(1000000, hamming()))
	val stop = System.currentTimeMillis()
	println("Took ${stop - strt} milliseconds for the last.")
}
```

{{output}}

```txt
[1, 2, 3, 4, 5, 6, 8, 9, 10, 12, 15, 16, 18, 20, 24, 25, 27, 30, 32, 36]
2125764000
519312780448388736089589843750000000000000000000000000000000000000000000000000000000
Took 381 milliseconds for the last.
```

Run on a AMD Bulldozer FX8120 3.1 GHz which is about half the speed as an equivalent Intel (but also half the price).


## Liberty BASIC

LB has unlimited precision integers.

```lb

dim h( 1000000)

for i =1 to 20
    print hamming( i); " ";
next i

print
print "H( 1691)", hamming( 1691)
print "H( 1000000)", hamming( 1000000)

end

function hamming( limit)
    h( 0) =1
    x2 =2: x3 =3: x5 =5
    i  =0: j  =0: k  =0
    for n =1 to limit
        h( n) = min( x2, min( x3, x5))
        if x2 = h( n) then i = i +1: x2 =2 *h( i)
        if x3 = h( n) then j = j +1: x3 =3 *h( j)
        if x5 = h( n) then k = k +1: x5 =5 *h( k)
    next n
    hamming =h( limit -1)
end function
```


```txt

1 2 3 4 5 6 8 9 10 12 15 16 18 20 24 25 27 30 32 36
H( 1691)
2125764000
H( 1000000) 519312780448388736089589843750000000000000000000000000000000000000000000000000000000

```



## Logo


```logo
to init.ham
  ; queues
  make "twos   [1]
  make "threes [1]
  make "fives  [1]
end
to next.ham
                          localmake "ham first :twos
  if less? first :threes :ham [make "ham first :threes]
  if less? first :fives  :ham [make "ham first :fives]

  if equal? :ham first :twos   [ignore dequeue "twos]
  if equal? :ham first :threes [ignore dequeue "threes]
  if equal? :ham first :fives  [ignore dequeue "fives]

  queue "twos   :ham * 2
  queue "threes :ham * 3
  queue "fives  :ham * 5

  output :ham
end

init.ham
repeat      20 [print  next.ham]
repeat 1690-20 [ignore next.ham]
print next.ham
```



## Lua


```lua
function hiter()
  hammings = {1}
  prev, vals = {1, 1, 1}
  index = 1
  local function nextv()
    local n, v = 1, hammings[prev[1]]*2
	if hammings[prev[2]]*3 < v then n, v = 2, hammings[prev[2]]*3 end
	if hammings[prev[3]]*5 < v then n, v = 3, hammings[prev[3]]*5 end
	prev[n] = prev[n] + 1
	if hammings[index] == v then return nextv() end
	index = index + 1
	hammings[index] = v
	return v
  end
  return nextv
end

j = hiter()
for i = 1, 20 do
  print(j())
end
n, l = 0, 0
while n < 2^31 do n, l = j(), n end
print(l)
```


## M2000 Interpreter


### For Long Only

We have to exit loop (and function) before calculating new X2 or X3 or X4 and get overflow error

```M2000 Interpreter

Module hamming_long {
	function hamming(l as long, &h(),&last()) {
		l=if(l<1->1&, l)
		long oldlen=len(h())
		if oldlen<l then dim h(l) else =h(l-1): exit
		def long  i, j, k, n, m, x2, x3, x5, ll
		stock last(0) out x2,x3,x5,i,j,k
		n=oldlen : ll=l-1
		{	m=x2
			if m>x3 then m=x3
			if m>x5 then m=x5
			h(n)=m
			if n>=1690 then =h(n):break
			if m=x2 then i++:x2=2&*h(i)
			if m=x3 then j++:x3=3&*h(j)
			if m=x5 then k++:x5=5&*h(k)
			if n<ll then n++: loop
		}
		stock last(0) in x2,x3,x5,i,j,k
		=h(ll)
	}
	dim h(1)=1&, last()
	def long i
	const nl$={
	}
	document doc$
	last()=(2&,3&,5&,0&,0&,0&)
	for i=1 to 20
		Doc$=format$("{0::-10} {1::-10}", i, hamming(i,&h(), &last()))+nl$
	next i
	i=1691
	Doc$=format$("{0::-10} {1::-10}", i, hamming(i,&h(), &last()))+nl$
	print #-2,Doc$
	clipboard Doc$
}
 hamming_long

```

{{out}}
<pre style="height:30ex;overflow:scroll">
         1          1
         2          2
         3          3
         4          4
         5          5
         6          6
         7          8
         8          9
         9         10
        10         12
        11         15
        12         16
        13         18
        14         20
        15         24
        16         25
        17         27
        18         30
        19         32
        20         36
      1691 2125764000
</pre >


### Using Decimal type

Max hamming number is the 43208th

We have to exit loop (and function) before calculating new X2 or X3 or X4 and get overflow error


```M2000 Interpreter

Module hamming {
	function hamming(l as long, &h(),&last()) {
		l=if(l<1->1&, l)
		oldlen=len(h())
		if oldlen<l then dim h(l) else =h(l-1): exit
		def decimal  i, j, k, m, x2, x3, x5
		stock last(0) out x2,x3,x5,i,j,k
		n=oldlen : ll=l-1&
		{	m=x2
			if m>x3 then m=x3
			if m>x5 then m=x5
			h(n)=m
			if n>=43207& then =h(n):break
			if m=x2 then i++:x2=2@*h(i)
			if m=x3 then j++:x3=3@*h(j)
			if m=x5 then k++:x5=5@*h(k)
			if n<ll then n++: loop
		}
		stock last(0) in x2,x3,x5,i,j,k
		=h(ll)
	}
	dim h(1)=1@, last()
	last()=(2@,3@,5@,0@,0@,0@)
	Document doc$
	const nl$={
	}
	for i=1 to 20
		Doc$=format$("{0::-10} {1::-28}", i, hamming(i,&h(), &last()))+nl$
	next i
	i=1691
	Doc$=format$("{0::-10} {1::-28}", i, hamming(i,&h(), &last()))+nl$
	i=9999
	Doc$=format$("{0::-10} {1::-28}", i, hamming(i,&h(), &last()))+nl$
	i=43208
	Doc$=format$("{0::-10} {1::-28}", i, hamming(i,&h(), &last()))+nl$
	print #-2, Doc$
	clipboard Doc$
}
hamming

```


{{out}}
<pre style="height:30ex;overflow:scroll">
         1                            1
         2                            2
         3                            3
         4                            4
         5                            5
         6                            6
         7                            8
         8                            9
         9                           10
        10                           12
        11                           15
        12                           16
        13                           18
        14                           20
        15                           24
        16                           25
        17                           27
        18                           30
        19                           32
        20                           36
      1691                   2125764000
      9999           288230376151711744
     43208 9164837199872000000000000000

</pre >

=={{header|Mathematica}} / {{header|Wolfram Language}}==

```mathematica
HammingList[N_] := Module[{A, B, C}, {A, B, C} = (N^(1/3))*{2.8054745679851933, 1.7700573778298891, 1.2082521307023026} - {1, 1, 1};
 Take[ Sort@Flatten@Table[ 2^x * 3^y * 5^z ,
{x, 0, A}, {y, 0, (-B/A)*x + B}, {z, 0, C - (C/A)*x - (C/B)*y}], N]];
```


```txt
HammingList[20]
-> {1, 2, 3, 4, 5, 6, 8, 9, 10, 12, 15, 16, 18, 20, 24, 25, 27, 30, 32, 36}
HammingList[1691] // Last
-> 2125764000
HammingList[1000000] // Last
->519312780448388736089589843750000000000000000000000000000000000000000000000000000000
```


=={{header|MATLAB}} / {{header|Octave}}==
{{trans|Julia}}

The ''n'' parameter was chosen by trial and error. You have to pick an ''n'' large enough that the powers of 2, 3 and 5 will all be greater than ''n'' at the 1691st Hamming number.


```Matlab
n = 40;

powers_2 = 2.^[0:n-1];
powers_3 = 3.^[0:n-1];
powers_5 = 5.^[0:n-1];

matrix = powers_2' * powers_3;
powers_23 = sort(reshape(matrix,n*n,1));


matrix = powers_23 * powers_5;
powers_235 = sort(reshape(matrix,n*n*n,1));

%
% Remove the integer overflow values.
%
powers_235 = powers_235(powers_235 > 0);

disp(powers_235(1:20))
disp(powers_235(1691))
```



## MUMPS


```MUMPS
Hamming(n)	New count,ok,next,number,which
	For which=2,3,5 Set number=1
	For count=1:1:n Do
	. Set ok=0 Set:count<21 ok=1 Set:count=1691 ok=1 Set:count=n ok=1
	. Write:ok !,$Justify(count,5),": ",number
	. For which=2,3,5 Set next(number*which)=which
	. Set number=$Order(next(""))
	. Kill next(number)
	. Quit
	Quit
Do Hamming(2000)

    1: 1
    2: 2
    3: 3
    4: 4
    5: 5
    6: 6
    7: 8
    8: 9
    9: 10
   10: 12
   11: 15
   12: 16
   13: 18
   14: 20
   15: 24
   16: 25
   17: 27
   18: 30
   19: 32
   20: 36
 1691: 2125764000
 2000: 8062156800
```



## Nim

{{libheader|bigints}}

Classic Dijkstra algorithm:

```nim
import bigints, math

proc hamming(limit: int): BigInt =
  var
    h = newSeq[BigInt](limit)
    x2 = initBigInt(2)
    x3 = initBigInt(3)
    x5 = initBigInt(5)
    i, j, k = 0
  for i in 0..h.high: h[i] = initBigInt(1)

  for n in 1 .. < limit:
    h[n] = min(x2, x3, x5)
    if x2 == h[n]:
      inc i
      x2 = h[i] * 2
    if x3 == h[n]:
      inc j
      x3 = h[j] * 3
    if x5 == h[n]:
      inc k
      x5 = h[k] * 5

  result = h[h.high]

for i in 1 .. 20:
  write stdout, hamming(i), " "
echo ""
echo hamming(1691)
echo hamming(1_000_000)
```

{{out}}

```txt
1 2 3 4 5 6 8 9 10 12 15 16 18 20 24 25 27 30 32 36
2125764000
519312780448388736089589843750000000000000000000000000000000000000000000000000000000
```


The above takes over a second to produce the millionth Hamming number on many machines.


### Slightly more efficient version


The following code improves on the above by reducing the number of computationally-time-expensive BigInt comparisions slightly:

```nim
import bigints, times

proc hamming(limit: int): BigInt =
  doAssert limit > 0
  var
    h = newSeq[BigInt](limit)
    x2 = initBigInt(2)
    x3 = initBigInt(3)
    x5 = initBigInt(5)
    i, j, k = 0
  h[0] = initBigInt 1

  # BigInt comparisons are expensive, reduce them...
  proc min3(x, y, z: BigInt): (int, BigInt) =
    let (cs, r1) = if y == z: (0x6, y)
                   elif y < z: (2, y) else: (4, z)
    if x == r1: (cs or 1, x)
    elif x < r1: (1, x) else: (cs, r1)

  for n in 1 .. < limit:
    let (cs, e1) = min3(x2, x3, x5)
    h[n] = e1
    if (cs and 1) != 0: i += 1; x2 = h[i] * 2
    if (cs and 2) != 0: j += 1; x3 = h[j] * 3
    if (cs and 4) != 0: k += 1; x5 = h[k] * 5

  h[h.high]

for i in 1 .. 20:
  write stdout, hamming(i), " "
echo ""
echo hamming(1691)

let strt = epochTime()
let rslt = hamming(1_000_000)
let stop = epochTime()

echo rslt
echo "This last took ", (stop - strt)*1000, " milliseconds."
```

{{output}}

```txt
1 2 3 4 5 6 8 9 10 12 15 16 18 20 24 25 27 30 32 36
2125764000
519312780448388736089589843750000000000000000000000000000000000000000000000000000000
This last took 899.1901874542236 milliseconds.
```


It can be shown that the above reduces the execution time by about 20 per cent.

===Functional iterator sequence, eliminating duplicate calculations and reducing memory use===

The above code still wastes quite a lot of time doing redundant BigInt calculations (ie. 2 times 3, 3 times 2, etc.) and as well consumes a huge amount of memory for larger Hamming number determination as it uses an array as large as the range.  The below code eliminates duplicate calculations and reduces memory use by using a Nim version of a lazy list internally so that unused back calculated values can be eliminated by the garbage collector.  Thus, execution time for BigInt calculations is reduced by a constant factor of about two and a half and memory use is reduced from O(n) to O(n^(2/3)) in the following code:
{{trans|Haskell}}

```nim
import bigints, math, sequtils, algorithm, times

iterator func_hamming() : BigInt =
  type Thunk[T] = proc(): T {.closure.}
  type Lazy[T] = ref object of RootObj # tuple[val: T, thnk: Thunk[T]]
    val: T
    thnk: Thunk[T]
  proc force[T](me: var Lazy[T]): T = # not thread-safe; needs lock on thunk
    if me.thnk != nil: me.val = me.thnk(); me.thnk = nil
    me.val
  type LazyList[T] = ref object of RootObj # tuple[hd: T, tl: Lazy[LazyList[T]]]
    hd: T
    tl: Lazy[LazyList[T]]
  type Mytype = LazyList[BigInt]
  proc merge(x, y: Mytype): Mytype =
    let xh = x.hd; let yh = y.hd
    if xh < yh:
      let mthnk = proc(): Mytype = merge x.tl.force, y
      let mlzy = Lazy[Mytype](thnk: mthnk)
      Mytype(hd: xh, tl: mlzy)
    else:
      let mthnk = proc(): Mytype = merge x, y.tl.force
      let mlzy = Lazy[Mytype](thnk: mthnk)
      Mytype(hd: yh, tl: mlzy)
  proc smult(m: int32, s: Mytype): Mytype =
    proc smults(ss: Mytype): Mytype =
      let mthnk = proc(): Mytype = ss.tl.force.smults
      let mlzy = Lazy[Mytype](thnk: mthnk)
      Mytype(hd: ss.hd * m, tl: mlzy)
    s.smults
  proc u(s: Mytype, n: int32): Mytype =
    var r: Mytype
    let mthnk = proc(): Mytype = r
    let mlzy = Lazy[Mytype](thnk: mthnk)
    let frst = Mytype(hd: initBigInt 1, tl: mlzy)
    if s == nil: r = smult(n, frst) else: r = merge(s, smult(n, frst))
    r
  var hmg: Mytype = nil
  for p in [5i32, 3i32, 2i32]: hmg = u(hmg, p)
  yield initBigInt 1
  while true: # loop almost forever
    yield initBigInt hmg.hd
    hmg = hmg.tl.force

var cnt = 1
for h in func_hamming():
  if cnt > 20: break
  write stdout, h, " "; cnt += 1
echo ""
cnt = 1
for h in func_hamming():
  if cnt < 1691: cnt += 1; continue
  else: echo h; break

let strt = epochTime()
var rslt: BigInt
cnt = 1
for h in func_hamming():
  if cnt < 1000000: cnt += 1; continue
  else: rslt = h; break
let stop = epochTime()

echo rslt
echo "This last took ", (stop - strt)*1000, " milliseconds."
```

{{output}}

```txt
1 2 3 4 5 6 8 9 10 12 15 16 18 20 24 25 27 30 32 36
2125764000
519312780448388736089589843750000000000000000000000000000000000000000000000000000000
This last took 1218.756675720215 milliseconds.
```


The output is the same as above except that time is about 1219 millications: even though there are many less calculations, the time required is not reduced at all and even increased.  This means that the current Nim memory allocation/deallocation (garbage collection) is not that efficient for the required many small allocations/deallocations as compared to truly great ones such as that used by Haskell or the Java Virtual Machine (JVM), but it isn't that bad either as compared to say the DotNet Framework one.

The beauty of Nim inline iterators as used here is that they are zero overhead (tested) so there is no run time penalty for using them.


### Imperative iterator implementation of the above functional version


The above claims with respect to the inefficiency of Nim's memory allocation can be proven by the following code which uses imperative techniques to implement the same algorithm, using sequences for storage, indexes for back pointers to the results of previous calculations, and custom deleting unused values in chunks in place (using constantly growing capacity) so that the same size of sequence can be longer used and many less new memory allocations need be made:


```nim
import bigints, math, sequtils, times

iterator nodups_hamming(): BigInt =
  var
    m = newSeq[BigInt](1) # give it two values so doubling size works
    h = newSeq[BigInt](1) # reasonably size
    x5 = initBigInt 5
    mrg = initBigInt 3
    x53 = initBigInt 9 # already advanced one step
    x532 = initBigInt 2
    ih, jm, i, j = 0

  yield initBigInt 1 # trivial case of 1
  while true:
    let cph = h.len # move in-place to avoid allocation
    if i >= cph div 2: # move in-place to avoid allocation
      var s = i; var d = 0
      while s < ih: shallowCopy(h[d], h[s]); s += 1; d += 1
      ih -= i; i = 0
    if i >= cph div 2: moveMem(h[0].unsafeAddr,
                               h[i].unsafeAddr,
                               (ih - i) * h[i].sizeof); ih -= i; i = 0
    if ih >= cph: h.setLen(2 * cph)
    if x532 < mrg: h[ih] = x532; x532 = h[i] * 2; i += 1
    else:
      h[ih] = mrg
      let cpm = m.len
      if j >= cpm div 2: # move in-place to avoid allocation
        var s = j; var d = 0
        while s < jm: shallowCopy(m[d], m[s]); s += 1; d += 1
        jm -= j; j = 0
      if jm >= cpm: m.setLen(2 * cpm)
      if x53 < x5: mrg = x53; x53 = m[j] * 3; j += 1
      else: mrg = x5; x5 = x5 * 5
      m[jm] = mrg
      jm += 1
    ih += 1

    yield h[ih - 1]
```


The iterator can be used just by substituting it's name for the previous iterator name in the above output functions.

The output is the same except for the time reduced to 562 milliseconds, which shows that a high percentage of the previous time was not used by BigInt calculations (as this code does exactly the same number of calculations) but rather by the memory allocatons/deallocations required for pure functional lazy algorithms.  This may show that the current Nim version (0.14.2) is not so suitable for pure lazy functional algorithms, nor is it as terse as many modern functional languages (Haskell, OcaML, F#, Scala, etc.).


### Much faster iterating version using logarithmic calculations


Still, much of the above time is used by BigInt calculations and still many heap allocations/deallocations, as BigInt's have an internal sequence to contain the infinite precision binary digits.  The following code uses an internal logarithmic representation of the values rather than BigInt and thus all mathematic operations required are just integer and floating point additions and comparison; as well, since these don't require heap space there is almost no allocation/deallocation at all for greatly increased speed:


```nim
import bigints, math, sequtils, times

proc convertTrival2BigInt(tpl: (uint32, uint32, uint32)): BigInt =
  result = initBigInt 1
  let (x, y, z) = tpl
  for _ in 1 .. x: result *= 2
  for _ in 1 .. y: result *= 3
  for _ in 1 .. z: result *= 5

iterator log_nodups_hamming(): (uint32, uint32, uint32) =
  let lb3 = 3.0f64.log2; let lb5 = 5.0f64.log2
  type Logrep = (float64, (uint32, uint32, uint32))
  proc `<`(me: Logrep, othr: Logrep): bool =
    let (lme, _) = me; let (lothr, _) = othr
    lme < lothr
  proc mul2(me: Logrep): Logrep =
    let (lr, tpl) = me; let (x2, x3, x5) = tpl
    (lr + 1.0f64, (x2 + 1, x3, x5))
  proc mul3(me: Logrep): Logrep =
    let (lr, tpl) = me; let (x2, x3, x5) = tpl
    (lr + lb3, (x2, x3 + 1, x5))
  proc mul5(me: Logrep): Logrep =
    let (lr, tpl) = me; let (x2, x3, x5) = tpl
    (lr + lb5, (x2, x3, x5 + 1))

  let one: Logrep = (0.0f64, (0u32, 0u32, 0u32))
  var
    m = newSeq[Logrep](1) # give it two values so doubling size works
    h = newSeq[Logrep](1) # reasonably size
    x5 = one.mul5 # initBigInt 5
    mrg = one.mul3 # initBigInt 3
    x53 = one.mul3().mul3 # initBigInt 9 # already advanced one step
    x532 = one.mul2 # initBigInt 2
    ih, jm, i, j = 0

  yield (0u32, 0u32, 0u32)
  while true:
    let cph = h.len # move in-place to avoid allocation
    if i >= cph div 2: # move in-place to avoid allocation
      var s = i; var d = 0
      while s < ih: shallowCopy(h[d], h[s]); s += 1; d += 1
      ih -= i; i = 0
    if ih >= cph: h.setLen(2 * cph)
    if x532 < mrg: h[ih] = x532; x532 = h[i].mul2; i += 1
    else:
      h[ih] = mrg
      let cpm = m.len
      if j >= cpm div 2: # move in-place to avoid allocation
        var s = j; var d = 0
        while s < jm: shallowCopy(m[d], m[s]); s += 1; d += 1
        jm -= j; j = 0
      if jm >= cpm: m.setLen(2 * cpm)
      if x53 < x5: mrg = x53; x53 = m[j].mul3; j += 1
      else: mrg = x5; x5 = x5.mul5
      m[jm] = mrg
      jm += 1
    ih += 1

    let (_, rslt) = h[ih - 1]
    yield rslt

var cnt = 1
for h in log_nodups_hamming():
  if cnt > 20: break
  write stdout, h.convertTrival2BigInt, " "; cnt += 1
echo ""
cnt = 1
for h in log_nodups_hamming():
  if cnt < 1691: cnt += 1; continue
  else: echo h.convertTrival2BigInt; break

let strt = epochTime()
var rslt: (uint32, uint32, uint32)
cnt = 1
for h in log_nodups_hamming():
  if cnt < 1000000: cnt += 1; continue
  else: rslt = h; break # """
let stop = epochTime()

let (x2, x3, x5) = rslt
writeLine stdout, "2^", x2, " + 3^", x3, " + 5^", x5
let lgrslt = (x2.float64 + x3.float64 * 3.0f64.log2 +
               x5.float64 * 5.0f64.log2) * 2.0f64.log10
let (whl, frac) = lgrslt.splitDecimal
echo "Approximately:  ", 10.0f64.pow(frac), "E+", whl.uint64
let brslt = rslt.convertTrival2BigInt()
let s = brslt.to_string
let ls = s.len
echo "Number of digits:  ", ls
if ls <= 2000:
  for i in countup(0, ls - 1, 100):
    if i + 100 < ls: echo s[i .. i + 99]
    else: echo s[i .. ls - 1]
echo "This last took ", (stop - strt)*1000, " milliseconds."
```

{{output}}

```txt
1 2 3 4 5 6 8 9 10 12 15 16 18 20 24 25 27 30 32 36
2125764000

2^55 + 3^47 + 5^64
Approximately:  5.193127804483804E+83
Number of digits:  84
519312780448388736089589843750000000000000000000000000000000000000000000000000000000
This last took 15.62285423278809 milliseconds.
```


The output is about the same except that it only takes about 15.6 milliseconds to calculate the millionth Hamming number; the billionth can be calculated in about 15 seconds.


### Extremely fast version inserting logarithms into the top error band


The above code is about as fast as one can go generating sequences; however, if one is willing to forego sequences and just calculate the nth Hamming number (again), then some reading on the relationship between the size of numbers to the sequence numbers is helpful (Wikipedia: regular number).  One finds that there is a very distinct relationship and that it quite quickly reduces to quite a small error band proportional to the log of the output value for larger ranges.  Thus, the following code just scans for logarithmic representations to insert into a sequence for this top error band and extracts the correct nth representation from that band.  It reduces time complexity to O(n^(2/3)) from O(n) for the sequence versions, but even more amazingly, reduces memory requirements to O(n^(1/3)) from O(n^(2/3)) and thus makes it possible to calculate very large values in the sequence on common personal computers.  The code is as follows:
{{trans|Rust}}

```nim
import bigints, math, sequtils, algorithm, times

proc convertTrival2BigInt(tpl: (uint32, uint32, uint32)): BigInt =
  result = initBigInt 1
  let (x, y, z) = tpl
  for _ in 1 .. x: result *= 2
  for _ in 1 .. y: result *= 3
  for _ in 1 .. z: result *= 5

proc nth_hamming(n: uint64): (uint32, uint32, uint32) =
  doAssert n > 0u64
  if n < 2: return (0u32, 0u32, 0u32) # trivial case for 1

  type Logrep = (float64, (uint32, uint32, uint32))

  let
    lb3 = 3.0f64.log2
    lb5 = 5.0f64.log2
    fctr = 6.0f64 * lb3 * lb5
    crctn = 30.0f64.sqrt().log2 # log base 2 of sqrt 30
    lgest = (fctr * n.float64).pow(1.0f64/3.0f64) - crctn # from WP formula
    frctn = if n < 1000000000: 0.509f64 else: 0.105f64
    lghi = (fctr * (n.float64 + frctn * lgest)).pow(1.0f64/3.0f64) - crctn
    lglo = 2.0f64 * lgest - lghi # and a lower limit of the upper "band"
  var count = 0u64 # need to use extended precision, might go over
  var bnd = newSeq[Logrep](1) # give itone value so doubling size works
  let klmt = uint32(lghi / lb5) + 1
  for k in 0 .. < klmt: # i, j, k values can be just u32 values
    let p = k.float64 * lb5
    let jlmt = uint32((lghi - p) / lb3) + 1
    for j in 0 .. < jlmt:
      let q = p + j.float64 * lb3
      let ir = lghi - q
      let lg = q + ir.floor # current log value (estimated)
      count += ir.uint64 + 1;
      if lg >= lglo:
        bnd.add((lg, (ir.uint32, j, k)))
  if n > count: raise newException(Exception, "nth_hamming: band high estimate is too low!")
  let ndx = (count - n).int
  if ndx >= bnd.len: raise newException(Exception, "nth_hamming: band low estimate is too high!")
  bnd.sort((proc (a, b: Logrep): int = # sort decreasing order
    let (la, _) = a; let (lb, _) = b
    la.cmp lb), SortOrder.Descending)

  let (_, rslt) = bnd[ndx]
  rslt

for _ in 1 .. 20:
  write stdout, nth_hamming(i.uint64).convertTrival2BigInt, " "
echo ""
echo nth_hamming(1691).convertTrival2BigInt

let strt = epochTime()
let rslt = nth_hamming(1_000_000u64)
let stop = epochTime()

let (x2, x3, x5) = rslt
writeLine stdout, "2^", x2, " + 3^", x3, " + 5^", x5
let lgrslt = (x2.float64 + x3.float64 * 3.0f64.log2 +
               x5.float64 * 5.0f64.log2) * 2.0f64.log10
let (whl, frac) = lgrslt.splitDecimal
echo "Approximately:  ", 10.0f64.pow(frac), "E+", whl.uint64
let brslt = rslt.convertTrival2BigInt()
let s = brslt.to_string
let ls = s.len
echo "Number of digits:  ", ls
if ls <= 2000:
  for i in countup(0, ls - 1, 100):
    if i + 100 < ls: echo s[i .. i + 99]
    else: echo s[i .. ls - 1]

echo "This last took ", (stop - strt)*1000, " milliseconds."
```


The output is the same as above except that the time is too small to be measured.  The billionth number in the sequence can be calculated in just about 15 milliseconds, the trillionth in about 1.5 seconds, the thousand trillionth in about 150 seconds, and it should be possible to calculate the 10^19th value in less than a day (untested) on common personal computers.  The (2^64 - 1)th value (18446744073709551615) cannot be calculated due to a slight overflow problem as it approaches that limit.  However, this version is not usable much about the 1e13th Hamming number due to the log base two (double) not having enough precision to accurately sort the values put into the error band array.

'''Alternate version with a greatly increased range without error'''

To solve the problem of inadequate precision in the double log base two representation, the following code uses a BigInt representation of the log value with about twice the significant bits, which is then sufficient to extend the usable range well beyond any reasonable requirement:

```nim
import bigints, math, sequtils, algorithm, times

proc nth_hamming(n: uint64): (uint32, uint32, uint32) =
  doAssert n > 0u64
  if n < 2: return (0u32, 0u32, 0u32) # trivial case for 1

  type Logrep = (float64, (uint32, uint32, uint32))

  let
    lb3 = 3.0f64.log2
    lb5 = 5.0f64.log2
    fctr = 6.0f64 * lb3 * lb5
    crctn = 30.0f64.sqrt().log2 # log base 2 of sqrt 30
    lgest = (fctr * n.float64).pow(1.0f64/3.0f64) - crctn # from WP formula
    frctn = if n < 1000000000: 0.509f64 else: 0.105f64
    lghi = (fctr * (n.float64 + frctn * lgest)).pow(1.0f64/3.0f64) - crctn
    lglo = 2.0f64 * lgest - lghi # and a lower limit of the upper "band"
  var count = 0u64 # need to use extended precision, might go over
  var bnd = newSeq[Logrep](1) # give itone value so doubling size works
  let klmt = uint32(lghi / lb5) + 1
  for k in 0 ..< klmt: # i, j, k values can be just u32 values
    let p = k.float64 * lb5
    let jlmt = uint32((lghi - p) / lb3) + 1
    for j in 0 ..< jlmt:
      let q = p + j.float64 * lb3
      let ir = lghi - q
      let lg = q + ir.floor # current log value (estimated)
      count += ir.uint64 + 1;
      if lg >= lglo:
        bnd.add((lg, (ir.uint32, j, k)))
  if n > count: raise newException(Exception, "nth_hamming: band high estimate is too low!")
  let ndx = (count - n).int
  if ndx >= bnd.len: raise newException(Exception, "nth_hamming: band low estimate is too high!")
  bnd.sort((proc (a, b: Logrep): int = # sort decreasing order
    let (la, _) = a; let (lb, _) = b
    la.cmp lb), SortOrder.Descending)

  let (_, rslt) = bnd[ndx]
  rslt

let num_hammings = 1_000_000_000_000u64

for i in 1 .. 20:
  write stdout, nth_hamming(i.uint64).convertTrival2BigInt, " "
echo ""
echo nth_hamming(1691).convertTrival2BigInt

let strt = epochTime()
let rslt = nth_hamming(num_hammings)
let stop = epochTime()

let (x2, x3, x5) = rslt
writeLine stdout, "2^", x2, " + 3^", x3, " + 5^", x5
let lgrslt = (x2.float64 + x3.float64 * 3.0f64.log2 +
                x5.float64 * 5.0f64.log2) * 2.0f64.log10
let (whl, frac) = lgrslt.splitDecimal
echo "Approximately:  ", 10.0f64.pow(frac), "E+", whl.uint64
let brslt = rslt.convertTrival2BigInt()
let s = brslt.to_string
let ls = s.len
echo "Number of digits:  ", ls
if ls <= 2000:
  for i in countup(0, ls - 1, 100):
    if i + 100 < ls: echo s[i .. i + 99]
    else: echo s[i .. ls - 1]

echo "This last took ", (stop - strt)*1000, " milliseconds."
```


The above code has the same output as before and doesn't take an appreciable amount time longer to execute; thus, it successfully extends the usable range of the algorithm to days of execution time.


## OCaml


A simple implementation using an integer Set as a priority queue. The semantics of the standard library Set provide a minimum element and prevent duplicate entries. <i>min_elt</i> and <i>add</i> are <em>O</em>(log N).


```OCaml
module ISet = Set.Make(struct type t = int let compare=compare end)

let pq = ref (ISet.singleton 1)

let next () =
  let m = ISet.min_elt !pq in
  pq := ISet.(remove m !pq  |> add (2*m) |> add (3*m) |> add (5*m));
  m

let () =

  print_string "The first 20 are: ";

  for i = 1 to 20
  do
    Printf.printf "%d " (next ())
  done;

  for i = 21 to 1690
  do
    ignore (next ())
  done;

  Printf.printf "\nThe 1691st is %d\n" (next ());
```


Output:
<blockquote>The first 20 are: 1 2 3 4 5 6 8 9 10 12 15 16 18 20 24 25 27 30 32 36
The 1691st is 2125764000
</blockquote>


### Arbitrary precision


An arbitrary precision version for the one millionth number. Compile with eg: <i>ocamlopt -o hamming.exe nums.cmxa hamming.ml</i>

```OCaml
open Big_int

module APSet = Set.Make(
  struct
    type t = big_int
    let compare = compare_big_int
  end)

let pq = ref (APSet.singleton (big_int_of_int 1))

let next () =
  let m = APSet.min_elt !pq in
  let ( * ) = mult_int_big_int in
  pq := APSet.(remove m !pq  |> add (2*m) |> add (3*m) |> add (5*m));
  m

let () =
  let n = 1_000_000 in

  for i = 1 to (n-1)
  do
    ignore (next ())
  done;

  Printf.printf "\nThe %dth is %s\n" n (string_of_big_int (next ()));
```

Output:
<blockquote>The 1000000th is 519312780448388736089589843750000000000000000000000000000000000000000000000000000000</blockquote>


## Oz



### Lazy Version


{{trans|Haskell}}

```oz
declare
  fun lazy {HammingFun}
     1|{FoldL1 [{MultHamming 2} {MultHamming 3} {MultHamming 5}] LMerge}
  end

  Hamming = {HammingFun}

  fun {MultHamming N}
     {LMap Hamming fun {$ X} N*X end}
  end

  fun lazy {LMap Xs F}
     case Xs
     of nil  then nil
     [] X|Xr then {F X}|{LMap Xr F}
     end
  end

  fun lazy {LMerge Xs=X|Xr Ys=Y|Yr}
     if     X < Y then X|{LMerge Xr Ys}
     elseif X > Y then Y|{LMerge Xs Yr}
     else              X|{LMerge Xr Yr}
     end
  end

  fun {FoldL1 X|Xr F}
     {FoldL Xr F X}
  end
in
  {ForAll {List.take Hamming 20} System.showInfo}
  {System.showInfo {Nth Hamming 1690}}
  {System.showInfo {Nth Hamming 1000000}}
```




### Strict Version


The strict version uses iterators and a priority queue.
Note that it can calculate other variations of the hamming numbers too. By changing K, it will calculate the p(K)-smooth numbers. (E.g. K = 3, it will use the first three primes 2,3 and 5, thus resulting in the 5-smooth numbers, see [https://en.wikipedia.org/wiki/Hamming_numbers])

```oz

functor
import
	Application
	System
define

class Multiplier
	attr 	lst
		factor
		current

	meth init(Factor Lst)
		lst	:= Lst
		factor	:= Factor
		{self next}
	end
	meth next
		local
			A
			AS
		in
			A|AS = @lst
			current := A*@factor
			lst := AS
		end
	end
	meth peek(?X)
		X = @current
	end

	meth dump
		{System.showInfo "DUMP"}
		{System.showInfo "Factor: "#@factor}
		{System.showInfo "current: "#@current}
	end
end

% a priority queue of multipliers. The one which currently holds the smallest value is put on front
class PriorityQueue
	attr	mults
			current	% for duplicate detection

	meth init(Mults)
		mults	:= Mults
		current	:= 0
	end

	meth insert(Mult)
		local
			fun {Insert M Lst}
				local
					Av
					Mv
				in
					case Lst of
						nil	then M|Lst
					[] A|AS	then 	{A peek(Av)}
										{M peek(Mv)}
										if	Av < Mv then
											A|{Insert M AS}
										else	M|A|AS
										end
					end
				end
			end
		in
			mults	:= {Insert Mult @mults}
		end
	end

	meth next(Tail NextTail)
		local
			M
			Ms
			X
			Curr
		in
			M|Ms	= @mults
			{M peek(X)}	% gets value of lowest iterator
			Curr	= @current
			if Curr == X then
				skip
			else
				Tail	= X|NextTail	% if we found a new value: append
			end
			{M next}
			mults	:= Ms
			{self insert(M)}
			if Curr == X then
				{self next(Tail NextTail)}
			else
				current := X
			end
		end
	end

end


local

	% Sieve of erasthothenes, adapted from http://rosettacode.org/wiki/Sieve_of_Eratosthenes#Oz
	fun {Sieve N}
		 S = {Array.new 2 N true}
		 M = {Float.toInt {Sqrt {Int.toFloat N}}}
	in
		 for I in 2..M do
	if S.I then
		 for J in I*I..N;I do
				S.J := false
		 end
	end
		 end
		 S
	end

	fun {Primes N}
		 S = {Sieve N}
	in
		 for I in 2..N collect:C do
	if S.I then {C I} end
		 end
	end


	% help method to extract args
	proc {GetNK ArgList N K}
		case ArgList of
		A|B|_ then
			N={StringToInt A}
			K={StringToInt B}
		end
	end


	proc {Generate N PriorQ Tail}
	local
		NewTail
	in
		if N == 0 then
			Tail = nil
		else
			{PriorQ next(Tail NewTail)}
			{Generate (N-1) PriorQ NewTail}
		end
	end
	end

	K = 3
	PrimeFactors
	Lst
	Tail
in
	ArgList = {Application.getArgs plain}
	Lst	= 1|Tail
	PrimeFactors	= {List.take {Primes K*K} K}
	Mults	= {List.map PrimeFactors fun {$ A} {New Multiplier init(A Lst) } end}
	PriorQ	= {New PriorityQueue init(Mults)}
	{Generate 20 PriorQ Tail}
	{ForAll Lst System.showInfo}
	{Application.exit 0}
end
end

```

Strict version made by pietervdvn; do what you want with the code.


## PARI/GP

This is a basic implementation; finding the millionth term requires 1 second and 54 MB. Much better algorithms exist.

```parigp
Hupto(n)={
  my(v=vector(n),x2=2,x3=3,x5=5,i=1,j=1,k=1,t);
  v[1]=1;
  for(m=2,n,
    v[m]=t=min(x2,min(x3,x5));
    if(x2 == t, x2 = v[i++] << 1);
    if(x3 == t, x3 = 3 * v[j++]);
    if(x5 == t, x5 = 5 * v[k++]);
  );
  v
};
H(n)=Hupto(n)[n];

Hupto(20)
H(1691)
H(10^6)
```

{{out}}

```txt
%1 = [1, 2, 3, 4, 5, 6, 8, 9, 10, 12, 15, 16, 18, 20, 24, 25, 27, 30, 32, 36]
%2 = 2125764000
%3 = 519312780448388736089589843750000000000000000000000000000000000000000000000000000000
```



## Pascal

Simple brute force til 2^32-1.I was astonished by the speed.The inner loop is taken 2^32 -1 times.DIV by constant is optimized to Mul and shift.
Using FPC_64 3.1.1, i4330  3.5 Ghz

```pascal

program HammNumb;
{$IFDEF FPC}
  {$MODE DELPHI}
  {$OPTIMIZATION ON}
{$ELSE}
  {$APPTYPE CONSOLE}
{$ENDIF}
{
type
  NativeUInt = longWord;
}
var
  pot   : array[0..2] of NativeUInt;

function NextHammNumb(n:NativeUInt):NativeUInt;
var
  q,p,nr : NativeUInt;
begin
  repeat
    nr := n+1;
    n := nr;

    p := 0;
    while NOT(ODD(nr)) do
    begin
      inc(p);
      nr := nr div 2;
    end;
    Pot[0]:= p;

    p := 0;
    q := nr div 3;
    while q*3=nr do
    Begin
      inc(P);
      nr := q;
      q := nr div 3;
    end;
    Pot[1] := p;

    p := 0;
    q := nr div 5;
    while q*5=nr do
    Begin
      inc(P);
      nr := q;
      q := nr div 5;
    end;
    Pot[2] := p;

  until nr = 1;
  result:= n;
end;

procedure Check;
var
  i,n: NativeUint;
begin
  n := 1;
  for i := 1 to 20 do
  begin
    n := NextHammNumb(n);
    write(n,' ');
  end;
  writeln;
  writeln;
  n := 1;
  for i := 1 to 1690 do
    n := NextHammNumb(n);
  writeln('No ',i:4,' | ',n,' = 2^',Pot[0],' 3^',Pot[1],' 5^',Pot[2]);
end;

Begin
  Check;
End.
```

Output

```txt

2 3 4 5 6 8 9 10 12 15 16 18 20 24 25 27 30 32 36 40

No 1690 | 2125764000 = 2^5 3^12 5^3

real    0m17.328s
user    0m17.310s
```


### a fast alternative

The Pascal code above is by far slower.Easily to use for smooth-3 .. smooth-37.

Big(O) is nearly linear to sub-linear . 1E7-> 0.028s => x10 =>1e8 ->0.273s => x1000 => 100'200'300'400 ~ 1e11 35.907s // estimated 270 s!
This depends extreme on sorting speed.

http://rosettacode.org/wiki/Hamming_numbers#Direct_calculation_through_triples_enumeration is head to head, but still faster for very big numbers  >1e8 (10^8: 4 MB 0.27 sec)

100'200'300'400 calculates in 8.33 s

For fpc 3.1.1_64 linux on 3.5 Ghz i4330, depends on 64-Bit by a factor of 4 slower on 32-Bit

/* For 12 primes  "smooth-37"  1e8  it takes 02.807 s */

I collect only the factors between p^n and p^(n+1), in a recursive way in different lists

5 is a list consisting only 5^? = 1 factor

3 is a sorted list 3^?..3^?+1 and inserted values of 5

2 is a sorted list 2^?..2^?+1 and inserted values of list 3

Changing sizeOf(tElem) to  32 {maxPrimFakCnt = 3+8} instead of 16 ( x2) {maxPrimFakCnt = 3} results in increasing the runtime by x4 ( 2^2 )


```pascal
program hammNumb;
{$IFDEF FPC}
   {$MODE DELPHI}
   {$OPTIMIZATION ON,ASMCSE,CSE,PEEPHOLE}
   {$ALIGN 16}
{$ELSE}
  {$APPTYPE CONSOLE}
{$ENDIF}
uses
  sysutils;
const
  maxPrimFakCnt = 3;//3 or 3+8 if tNumber= double, else -1 for extended to keep data aligned
  minElemCnt = 10;
type
  tPrimList = array of NativeUint;
  tnumber = double;
  tpNumber= ^tnumber;
  tElem = record
             n   : tnumber;//ln(prime[0]^Pots[0]*...
             Pots: array[0..maxPrimFakCnt] of word;
           end;
  tpElem  = ^tElem;
  tElems  = array of tElem;
  tElemArr  = array [0..0] of tElem;
  tpElemArr  = ^tElemArr;

  tpFaktorRec = ^tFaktorRec;
  tFaktorRec = record
                 frElems  : tElems;
                 frInsElems: tElems;
                 frAktIdx : NativeUint;
                 frMaxIdx : NativeUint;
                 frPotNo  : NativeUint;
                 frActPot : NativeUint;
                 frNextFr : tpFaktorRec;
                 frActNumb: tElem;
                 frLnPrime: tnumber;
               end;
  tArrFR = array of tFaktorRec;

var
  Pl : tPrimList;
  ActIndex  : NativeUint;
  ArrInsert : tElems;

procedure PlInit(n: integer);
const
 cPl : array[0..11] of byte=(2,3,5,7,11,13,17,19,23,29,31,37);
var
  i : integer;
Begin
  IF n>High(cPl)+1 then
     n := High(cPl)
  else
      IF n < 0 then
         n := 1;
  setlength(Pl,n);
  dec(n);
  For i := 0 to n do
    Pl[i] := cPl[i];
end;

procedure AusgabeElem(pElem: tElem);
var
  i : integer;
Begin
  with pElem do
  Begin
    IF n < 23 then
      write(round(exp(n)):16)
    else
      write('ln ',n:13:7);
    For i := 0 to maxPrimFakCnt do
      write(' ',PL[i]:2,'^',Pots[i]);
  end;
  writeln
end;

//LoE == List of Elements
function LoEGetNextNumber(pFR :tpFaktorRec):tElem;forward;

procedure LoECreate(const Pl: tPrimList;var FA:tArrFR);
var
  i : integer;
Begin
  setlength(ArrInsert,100);
  setlength(FA,Length(PL));
  For i := 0 to High(PL) do
    with FA[i] do
    Begin
      //automatic zeroing
      IF i < High(PL) then
      Begin
        setlength(frElems,minElemCnt);
        setlength(frInsElems,minElemCnt);
        frNextFr := @FA[i+1]
      end
      else
      Begin
        setlength(frElems,2);
        setlength(frInsElems,0);
        frNextFr := NIL;
      end;
      frPotNo  := i;
      frLnPrime:= ln(PL[i]);
      frMaxIdx := 0;
      frAktIdx := 0;
      frActPot := 1;
      With frElems[0] do
      Begin
        n := frLnPrime;
        Pots[i]:= 1;
      end;
      frActNumb := frElems[0];
  end;
end;


procedure LoEFree(var FA:tArrFR);
var
  i : integer;
Begin
  For i := High(FA) downto Low(FA) do
    setlength(FA[i].frElems,0);
  setLength(FA,0);
end;

function LoEGetActElem(pFr:tpFaktorRec):tElem;
Begin
  with pFr^ do
    result := frElems[frAktIdx];
end;

function LoEGetActLstNumber(pFr:tpFaktorRec):tpNumber;
Begin
  with pFr^ do
    result := @frElems[frAktIdx].n;
end;

procedure LoEIncInsArr(var a:tElems);
Begin
  setlength(a,Length(a)*8 div 5);
end;

procedure LoEIncreaseElems(pFr:tpFaktorRec;minCnt:NativeUint);
var
  newLen: NativeUint;
Begin
  with pFR^ do
  begin
    newLen := Length(frElems);
    minCnt := minCnt+frMaxIdx;
    repeat
      newLen := newLen*8 div 5 +1;
    until newLen > minCnt;
    setlength(frElems,newLen);
  end;
end;

procedure LoEInsertNext(pFr:tpFaktorRec;Limit:tnumber);
var
  pNum : tpNumber;
  pElems : tpElemArr;
  cnt,i,u : NativeInt;
begin
  with pFr^ do
  Begin
    //collect numbers of heigher primes
    cnt := 0;
    pNum := LoEGetActLstNumber(frNextFr);
    while Limit > pNum^ do
    Begin
      frInsElems[cnt] := LoEGetNextNumber(frNextFr);
//   writeln( 'Ins ',frInsElems[cnt].n:10:8,' < ',pNum^:10:8);

      inc(cnt);
      IF cnt > High(frInsElems) then
        LoEIncInsArr(frInsElems);
      pNum := LoEGetActLstNumber(frNextFr);
    end;

    if cnt = 0 then
     EXIT;

    i := frMaxIdx;
    u := frMaxIdx+cnt+1;

    IF u > High(frElems) then
      LoEIncreaseElems(pFr,cnt);

    IF frPotNo =  0 then
      inc(ActIndex,u);
    //Merge
    pElems := @frElems[0];
    dec(cnt);
    dec(u);
    frMaxIdx:= u;
    repeat
// writeln(i:10,cnt:10,u:10); writeln( pElems^[i].n:10:8,' < ',frInsElems[cnt].n:10:8);
      IF pElems^[i].n < frInsElems[cnt].n then
      Begin
        pElems^[u] := frInsElems[cnt];
        dec(cnt);
      end
      else
      Begin
        pElems^[u] := pElems^[i];
        dec(i);
      end;
      dec(u);
    until (i<0) or (cnt<0);
    IF i < 0 then
      For u := cnt downto 0 do
        pElems^[u] := frInsElems[u];

  end;
end;

procedure LoEAppendNext(pFr:tpFaktorRec;Limit:tnumber);
var
  pNum : tpNumber;
  pElems : tpElemArr;
  i : NativeInt;
begin
  with pFr^ do
  Begin
    i := frMaxIdx+1;
    pElems := @frElems[0];
    pNum := LoEGetActLstNumber(frNextFr);
    while Limit > pNum^ do
    Begin
      IF i > High(frElems) then
      Begin
        LoEIncreaseElems(pFr,10);
        pElems := @frElems[0];
      end;
      pElems^[i] := LoEGetNextNumber(frNextFr);
      inc(i);
      pNum := LoEGetActLstNumber(frNextFr);
    end;
    inc(ActIndex,i);
    frMaxIdx:= i-1;
  end;
end;

procedure LoENextList(pFr:tpFaktorRec);
var
  pElems : tpElemArr;
  j : NativeUint;
begin
  with pFR^ do
  Begin
    //increase Elements by factor
    pElems := @frElems[0];
    for j := frMaxIdx Downto 0 do
      with pElems^[j] do
      Begin
        n := n+frLnPrime;
        inc(Pots[frPotNo]);
      end;
    //x^j -> x^(j+1)
    j := frActPot+1;
    with frActNumb do
    begin
      n:= j*frLnPrime;
      Pots[frPotNo]:= j;
    end;
    frActPot := j;
    //if something follows
    IF frNextFr <> NIL then
      LoEInsertNext(pFR,frActNumb.n);
    frAktIdx := 0;
  end;
end;

function LoEGetNextNumber(pFR :tpFaktorRec):tElem;
Begin
  with pFr^ do
  Begin
    result := frElems[frAktIdx];
    inc(frAktIdx);
    IF frMaxIdx < frAktIdx then
      LoENextList(pFr);
  end;
end;

procedure LoEGetNumber(pFR :tpFaktorRec;no:NativeUint);
Begin
  dec(no);
  while ActIndex < no do
    LoENextList(pFR);
  with pFr^ do
    frAktIdx := (no-(ActIndex-frMaxIdx)-1);

end;

var
  T1,T0: tDateTime;
  FA: tArrFR;
  i : integer;
Begin
  PlInit(3);// 3 -> 2,3,5
  LoECreate(Pl,FA);
  i := 1;
  i := 1;
  T0 := time;

  For i := 1 to 20 do
    AusgabeElem(LoEGetNextNumber(@FA[0]));

  LoEGetNumber(@FA[0],1691);
  AusgabeElem(LoEGetNextNumber(@FA[0]));


  LoEGetNumber(@FA[0],1000*1000);
  AusgabeElem(LoEGetNextNumber(@FA[0]));
  LoEGetNumber(@FA[0],100*1000*1000);
  T1 := time;
  AusgabeElem(LoEGetNextNumber(@FA[0]));
  Writeln('Timed 100*1000*1000 in ',FormatDateTime('HH:NN:SS.ZZZ',T1-T0));


  Writeln('Actual Index ',ActIndex );
  AusgabeElem(LoEGetNextNumber(@FA[0]));
  For i := 0 to High(FA) do
    writeln(pL[i]:2,
     ' elemcount  ',FA[i].frMaxIdx+1:7,' out of',length(FA[i].frElems):7);
  LoEFree(FA);
End.
```

Output

```txt

2,3,4,5....,36,40 ... shortened

      2125764000  2^5  3^12  5^3  0^0
ln   192.7618989  2^55  3^47  5^64  0^0
ln   900.9063136  2^2  3^454  5^249  0^0
Timed 100*1000*1000 in 00:00:00.276
Actual Index 100061507
ln   900.9063159  2^142  3^80  5^444  0^0
 2 elemcount   230300 out of 348159
 3 elemcount      561 out of    772
 5 elemcount        1 out of      2

real    0m0.278s
user    0m0.273s
sys     0m0.003s

      2125764000  2^5  3^12  5^3  0^0
ln   192.7618989  2^55  3^47  5^64  0^0
ln   417.2530468  2^80  3^92  5^162  0^0
00:00:00.028

Actual Index 10201068944--> hamming Nr: 100200300400 see http://ideone.com/q3fma
ln  4215.6152353  2^942  3^2276  5^660  0^0
2 elemcount  5028911 out of 5841156
3 elemcount    2620 out of     3165
5 elemcount       1 out of        2

real    0m35.963s
user    0m35.907s
sys     0m0.023s

...
change zu use 12 primes [2..37] ( 32 bit ) -> 2.2x runtime  over using 3 primes
Begin
  PlInit(12)

ln    40.8834947  2^14  3^0  5^6  7^4 11^2 13^1 17^0 19^1 23^0 29^0 31^1 37^0
Actual Index  100269652
Timed 100000000 in 00:00:02.807

 2 elemcount   14322779 out of 14953361
 3 elemcount    3387290 out of  3650722
 5 elemcount     891236 out of   891289
 7 elemcount     289599 out of   348159
11 elemcount      92240 out of   135999
13 elemcount      28272 out of    33202
17 elemcount       9394 out of    12969
19 elemcount       2639 out of     3165
23 elemcount        676 out of      772
29 elemcount        119 out of      188
31 elemcount         15 out of       17
37 elemcount          1 out of        2

```



## Perl


```perl
use strict;
use warnings;
use List::Util 'min';

# If you want the large output, uncomment either the one line
# marked (1) or the two lines marked (2)
#use Math::GMP qw/:constant/;        # (1) uncomment this to use Math::GMP
#use Math::GMPz;                     # (2) uncomment this plus later line for Math::GMPz

sub ham_gen {
    my @s = ([1], [1], [1]);
    my @m = (2, 3, 5);
    #@m = map { Math::GMPz->new($_) } @m;     # (2) uncomment for Math::GMPz

    return sub {
	my $n = min($s[0][0], $s[1][0], $s[2][0]);
	for (0 .. 2) {
	     shift @{$s[$_]} if $s[$_][0] == $n;
	     push @{$s[$_]}, $n * $m[$_]
	}
	return $n
    }
}

my $h = ham_gen;
my $i = 0;
++$i, print $h->(), " " until $i > 20;
print "...\n";

++$i, $h->() until $i == 1690;
print ++$i, "-th: ", $h->(), "\n";

# You will need to pick one of the bigint choices
#++$i, $h->() until $i == 999999;
#print ++$i, "-th: ", $h->(), "\n";

```

{{out}}

```txt
1 2 3 4 5 6 8 9 10 12 15 16 18 20 24 25 27 30 32 36 40 ...
1691-th: 2125764000
1000000-th: 519312780448388736089589843750000000000000000000000000000000000000000000000000000000
```


The core module bigint (Math::BigInt) is very slow, even with the GMP backend, and not supported here. Alternatives shown are Math::GMP and Math::GMPz (about 4x faster).


## Perl 6



###  Merge version

{{Works with|rakudo|2015-11-04}}
The limit scaling is not <em>required</em>, but it cuts down on a bunch of unnecessary calculation.

```perl6
my $limit = 32;

sub powers_of ($radix) { 1, |[\*] $radix xx * }

my @hammings =
  (   powers_of(2)[^ $limit ]       X*
      powers_of(3)[^($limit * 2/3)] X*
      powers_of(5)[^($limit * 1/2)]
   ).sort;

say @hammings[^20];
say @hammings[1690]; # zero indexed
```

{{out}}

```txt
(1 2 3 4 5 6 8 9 10 12 15 16 18 20 24 25 27 30 32 36)
2125764000
```



###  Iterative version

{{Works with|rakudo|6.c}}

This version uses a lazy list, storing a maximum of two extra value from the highest index requested


```perl6
my \Hammings := gather {
  my %i = 2, 3, 5 Z=> (Hammings.iterator for ^3);
  my %n = 2, 3, 5 Z=> 1 xx 3;

  loop {
    take my $n := %n{2, 3, 5}.min;

    for 2, 3, 5 -> \k {
      %n{k} = %i{k}.pull-one * k if %n{k} == $n;
    }
  }
}

say Hammings.[^20];
say Hammings.[1691 - 1];
say Hammings.[1000000 - 1];

```


{{out}}

```txt
(1 2 3 4 5 6 8 9 10 12 15 16 18 20 24 25 27 30 32 36)
2125764000
519312780448388736089589843750000000000000000000000000000000000000000000000000000000
```



## Phix

{{trans|AWK}}
{{libheader|mpfr}}
standard and gmp versions

```Phix
function hamming(integer N)
sequence h = repeat(1,N)
atom x2 = 2, x3 = 3, x5 = 5, hn
integer i = 1, j = 1, k = 1
    for n=2 to N do
        hn = min(x2,min(x3,x5))
        h[n] = hn
        if hn=x2 then i += 1 x2 = 2*h[i] end if
        if hn=x3 then j += 1 x3 = 3*h[j] end if
        if hn=x5 then k += 1 x5 = 5*h[k] end if
    end for
    return h[N]
end function

include builtins\mpfr.e

function mpz_min(mpz a, b)
    return iff(mpz_cmp(a,b)<0?a:b)
end function

function mpz_hamming(integer N)
sequence h = mpz_inits(N,1)
mpz x2 = mpz_init(2),
    x3 = mpz_init(3),
    x5 = mpz_init(5),
    hn = mpz_init()
integer i = 1, j = 1, k = 1
    for n=2 to N do
        mpz_set(hn,mpz_min(x2,mpz_min(x3,x5)))
        mpz_set(h[n],hn)
        if mpz_cmp(hn,x2)=0 then i += 1 mpz_mul_si(x2,h[i],2) end if
        if mpz_cmp(hn,x3)=0 then j += 1 mpz_mul_si(x3,h[j],3) end if
        if mpz_cmp(hn,x5)=0 then k += 1 mpz_mul_si(x5,h[k],5) end if
    end for
    return h[N]
end function

sequence s = {}
for i=1 to 20 do
    s = append(s,hamming(i))
end for
?s
?hamming(1691)
?{hamming(1000000),"wrong!"} --(the hn=x2 etc fail, so multiplies are all wrong)

mpfr_printf(1,"%Zd\n",mpz_hamming(1691))
mpfr_printf(1,"%Zd\n",mpz_hamming(1000000))
```

{{out}}

```txt

{1,2,3,4,5,6,8,9,10,12,15,16,18,20,24,25,27,30,32,36}
2125764000.0
{2.461927256e+29,"wrong!"}
2125764000
519312780448388736089589843750000000000000000000000000000000000000000000000000000000

```


### A much faster logarithmic version

This proved much easier to implement than scanning the other entries suggested [not copied, they all frighten me].

At some point, comparing logs will no doubt get it wrong, but I have no idea when that might happen.

```Phix
-- numbers kept as {log,{pow2,pow3,pow5}},
--  value is ~= exp(log), == (2^pow2)*(3^pow3)*(5^pow5)
enum LOG, POWS
enum POW2, POW3, POW5

function lnmin(sequence a, sequence b)
    return iff(a[LOG]<b[LOG]?a:b)
end function

constant ln1 = log(1), ln2 = log(2), ln3 = log(3), ln5 = log(5)

function hamming(integer N)
sequence h = repeat(0,N)
sequence x2 = {ln2,{1,0,0}},
         x3 = {ln3,{0,1,0}},
         x5 = {ln5,{0,0,1}}
integer i = 1, j = 1, k = 1
    h[1] = {ln1,{0,0,0}}
    for n=2 to N do
        h[n] = lnmin(x2,lnmin(x3,x5))
        sequence p = h[n][POWS]
        if p=x2[POWS] then i += 1 x2 = h[i] x2[LOG] += ln2 x2[POWS][POW2] += 1 end if
        if p=x3[POWS] then j += 1 x3 = h[j] x3[LOG] += ln3 x3[POWS][POW3] += 1 end if
        if p=x5[POWS] then k += 1 x5 = h[k] x5[LOG] += ln5 x5[POWS][POW5] += 1 end if
    end for
    return h[N]
end function

function hint(sequence hm)
-- (obviously not accurate above 53 bits on a 32-bit system, or 64 bits on a 64 bit system)
    sequence p = hm[POWS]
    return power(2,p[POW2])*power(3,p[POW3])*power(5,p[POW5])
end function

sequence s = {}
for i=1 to 20 do
    s = append(s,hint(hamming(i)))
end for
?s
?hint(hamming(1691))
?hint(hamming(1000000))
printf(1," %d (approx)\n",hint(hamming(1000000)))

include builtins\mpfr.e

function mpz_hint(sequence hm)
-- (as accurate as you like)
    integer {p2,p3,p5} = hm[POWS]
    mpz {tmp2,tmp3,tmp5} = mpz_inits(3)
    mpz_ui_pow_ui(tmp2,2,p2)
    mpz_ui_pow_ui(tmp3,3,p3)
    mpz_ui_pow_ui(tmp5,5,p5)
    mpz_mul(tmp3,tmp3,tmp5)
    mpz_mul(tmp2,tmp2,tmp3)
    return mpz_get_str(tmp2)
end function

?mpz_hint(hamming(1000000))
```

{{out}}

```txt

{1,2,3,4,5,6,8,9,10,12,15,16,18,20,24,25,27,30,32,36}
2125764000.0
5.193127805e+83
 519312780448389068266824288284848486280402222226888608420684482660084484246042460000 (approx)
"519312780448388736089589843750000000000000000000000000000000000000000000000000000000"

```



## PicoLisp


```PicoLisp
(de hamming (N)
   (let (L (1)  H)
      (do N
         (for (X L X (cadr X))      # Find smallest result
            (setq H (car X)) )
         (idx 'L H NIL)             # Remove it
         (for I (2 3 5)             # Generate next results
            (idx 'L (* I H) T) ) )
      H ) )

(println (make (for N 20 (link (hamming N)))))
(println (hamming 1691))   # very fast
(println (hamming 1000000))   # runtime about 13 minutes on i5-3570S
```

{{out}}

```txt
(1 2 3 4 5 6 8 9 10 12 15 16 18 20 24 25 27 30 32 36)
2125764000
519312780448388736089589843750000000000000000000000000000000000000000000000000000000
```



## PL/I

{{improve|PL/I|
 output does not match the program.

 The PL/I program lists the first '''2,197''' Hamming numbers, not '''20'''.

 Also, the wrong number is being displayed, it isn't H(1653), it should be H(1691).
}}

```PL/I
(subscriptrange):
Hamming: procedure options (main); /* 14 November 2013 */
   declare (H(3000), t) fixed (15);
   declare (i, j, k, m, n) fixed binary;
   declare swaps bit (1);

   on underflow ;

   m = 0; n = 12;
   do k = 0 to n;
      do j = 0 to n;
         do i = 0 to n;
            m = m + 1;
            H(m) = 2**i * 3**j * 5**k;
         end;
      end;
   end;
   /* sort */
   swaps = '1'b;
   do while (swaps); /* Cocktail-shaker sort is adequate, because values are largely sorted */
      swaps = '0'b;
      do i = 1 to m-1, i-1 to 1 by -1;
         if H(i) > H(i+1) then /* swap */
            do; t = H(i); H(i) = H(i+1); H(i+1) = t; swaps = '1'b; end;
      end;
   end;
   do i = 1 to m;
      put skip data (H(i));
   end;
   put skip data (H(1653));
end Hamming;
```

Results:

```txt

H(1)=                 1;
H(2)=                 2;
H(3)=                 3;
H(4)=                 4;
H(5)=                 5;
H(6)=                 6;
H(7)=                 8;
H(8)=                 9;
H(9)=                10;
H(10)=                12;
H(11)=                15;
H(12)=                16;
H(13)=                18;
H(14)=                20;
H(15)=                24;
H(16)=                25;
H(17)=                27;
H(18)=                30;
H(19)=                32;
H(20)=                36;
```



## Prolog


### Generator idiom


```Prolog
%% collect N elements produced by a generator in a row

take( 0, Next, Z-Z, Next).
take( N, Next, [A|B]-Z, NZ):- N>0, !, next(Next,A,Next1),
  N1 is N-1,
  take(N1,Next1,B-Z,NZ).

%% a generator provides specific {next} implementation

next( hamm( A2,B,C3,D,E5,F,[H|G] ), H, hamm(X,U,Y,V,Z,W,G) ):-
  H is min(A2, min(C3,E5)),
  (   A2 =:= H -> B=[N2|U],X is N2*2 ; (X,U)=(A2,B) ),
  (   C3 =:= H -> D=[N3|V],Y is N3*3 ; (Y,V)=(C3,D) ),
  (   E5 =:= H -> F=[N5|W],Z is N5*5 ; (Z,W)=(E5,F) ).

mkHamm( hamm(1,X,1,X,1,X,X) ).       % Hamming numbers generator init state

main(N) :-
    mkHamm(G),take(20,G,A-[],_),           write(A), nl,
    take(1691-1,G,_,G2),take(2,G2,B-[],_),     write(B), nl,
    take(  N  -1,G,_,G3),take(2,G3,[C1|_]-_,_),   write(C1), nl.
```

SWI Prolog 6.2.6 produces (in about 7 ideone seconds):
  ?- time( main(1000000) ).
  [1,2,3,4,5,6,8,9,10,12,15,16,18,20,24,25,27,30,32,36]
  [2125764000,2147483648]
  519312780448388736089589843750000000000000000000000000000000000000000000000000000000
  % 10,017,142 inferences


### Laziness flavor

Works with SWI-Prolog. Laziness is simulate with '''freeze/2''' and '''ground/2'''.

Took inspiration from this code : http://chr.informatik.uni-ulm.de/~webchr (click on ''hamming.pl: Solves Hamming Problem'').

```Prolog
hamming(N) :-
     % to stop cleanly
     nb_setval(go, 1),

     % display list
     (	 N = 20 -> watch_20(20, L); watch(1,N,L)),

     % go
     L=[1|L235],
     multlist(L,2,L2),
     multlist(L,3,L3),
     multlist(L,5,L5),
     merge_(L2,L3,L23),
     merge_(L5,L23,L235).


%% multlist(L,N,LN)
%% multiply each element of list L with N, resulting in list LN
%% here only do multiplication for 1st element, then use multlist recursively
multlist([X|L],N,XLN) :-
	% the trick to stop
	nb_getval(go, 1) ->

	% laziness flavor
	when(ground(X),
	     (	 XN is X*N,
		 XLN=[XN|LN],
		 multlist(L,N,LN)));

	true.

merge_([X|In1],[Y|In2],XYOut) :-
	% the trick to stop
	nb_getval(go, 1) ->

	% laziness flavor
	(   X < Y -> XYOut = [X|Out], In11 = In1, In12 = [Y|In2]
	;   X = Y -> XYOut = [X|Out], In11 = In1, In12 = In2
	;            XYOut = [Y|Out], In11 = [X | In1], In12 = In2),
	freeze(In11,freeze(In12, merge_(In11,In12,Out)));

	true.

%% display nth element
watch(Max, Max, [X|_]) :-
	% laziness flavor
	when(ground(X),
	     (format('~w~n', [X]),

	      % the trick to stop
	      nb_linkval(go, 0))).


watch(N, Max, [_X|L]):-
	 N1 is N + 1,
	 watch(N1, Max, L).


%% display nth element
watch_20(1, [X|_]) :-
	% laziness flavor
	when(ground(X),
	     (format('~w~n', [X]),

	      % the trick to stop
	      nb_linkval(go, 0))).


watch_20(N, [X|L]):-
	% laziness flavor
	when(ground(X),
	     (format('~w ', [X]),
	      N1 is N - 1,
	      watch_20(N1, L))).
```

{{out}}

```txt
?- hamming(20).
1 2 3 4 5 6 8 9 10 12 15 16 18 20 24 25 27 30 32 36
true .

?- hamming(1691).
2125764000
true .

?- hamming(1000000).
519312780448388736089589843750000000000000000000000000000000000000000000000000000000
true .

```



## PureBasic


```PureBasic
#X2 = 2
#X3 = 3
#X5 = 5

Macro Ham(w)
  PrintN("H("+Str(w)+") = "+Str(Hamming(w)))
EndMacro

Procedure.i Hamming(l.i)
  Define.i i,j,k,n,m,x=#X2,y=#X3,z=#X5
  Dim h.i(l) : h(0)=1
  For n=1 To l-1
    m=x
    If m>y : m=y : EndIf
    If m>z : m=z : EndIf
    h(n)=m
    If m=x : i+1 : x=#X2*h(i) : EndIf
    If m=y : j+1 : y=#X3*h(j) : EndIf
    If m=z : k+1 : z=#X5*h(k) : EndIf
  Next
  ProcedureReturn h(l-1)
EndProcedure

OpenConsole("Hamming numbers")
For h.i=1 To 20
  Ham(h)
Next
Ham(1691)
Input()
```

{{out}}
```txt
H(1) = 1
H(2) = 2
H(3) = 3
H(4) = 4
H(5) = 5
H(6) = 6
H(7) = 8
H(8) = 9
H(9) = 10
H(10) = 12
H(11) = 15
H(12) = 16
H(13) = 18
H(14) = 20
H(15) = 24
H(16) = 25
H(17) = 27
H(18) = 30
H(19) = 32
H(20) = 36
H(1691) = 2125764000
```



## Python

===Version based on example from Dr. Dobb's CodeTalk===

```python
from itertools import islice

def hamming2():
    '''\
    This version is based on a snippet from:
        https://web.archive.org/web/20081219014725/http://dobbscodetalk.com:80
                         /index.php?option=com_content&task=view&id=913&Itemid=85
        http://www.drdobbs.com/architecture-and-design/hamming-problem/228700538
        Hamming problem
        Written by Will Ness
        December 07, 2008

        When expressed in some imaginary pseudo-C with automatic
        unlimited storage allocation and BIGNUM arithmetics, it can be
        expressed as:
            hamming = h where
              array h;
              n=0; h[0]=1; i=0; j=0; k=0;
              x2=2*h[ i ]; x3=3*h[j]; x5=5*h[k];
              repeat:
                h[++n] = min(x2,x3,x5);
                if (x2==h[n]) { x2=2*h[++i]; }
                if (x3==h[n]) { x3=3*h[++j]; }
                if (x5==h[n]) { x5=5*h[++k]; }
    '''
    h = 1
    _h=[h]    # memoized
    multipliers  = (2, 3, 5)
    multindeces  = [0 for i in multipliers] # index into _h for multipliers
    multvalues   = [x * _h[i] for x,i in zip(multipliers, multindeces)]
    yield h
    while True:
        h = min(multvalues)
        _h.append(h)
        for (n,(v,x,i)) in enumerate(zip(multvalues, multipliers, multindeces)):
            if v == h:
                i += 1
                multindeces[n] = i
                multvalues[n]  = x * _h[i]
        # cap the memoization
        mini = min(multindeces)
        if mini >= 1000:
            del _h[:mini]
            multindeces = [i - mini for i in multindeces]
        #
        yield h
```

{{out}}

```txt
>>> list(islice(hamming2(), 20))
[1, 2, 3, 4, 5, 6, 8, 9, 10, 12, 15, 16, 18, 20, 24, 25, 27, 30, 32, 36]
>>> list(islice(hamming2(), 1690, 1691))
[2125764000]
>>> list(islice(hamming2(), 999999, 1000000))
[519312780448388736089589843750000000000000000000000000000000000000000000000000000000]

```



### Another implementation of same approach

This version uses a lot of memory, it doesn't try to limit memory usage.

```python
import psyco

def hamming(limit):
    h = [1] * limit
    x2, x3, x5 = 2, 3, 5
    i = j = k = 0

    for n in xrange(1, limit):
        h[n] = min(x2, x3, x5)
        if x2 == h[n]:
            i += 1
            x2 = 2 * h[i]
        if x3 == h[n]:
            j += 1
            x3 = 3 * h[j]
        if x5 == h[n]:
            k += 1
            x5 = 5 * h[k]

    return h[-1]

psyco.bind(hamming)
print [hamming(i) for i in xrange(1, 21)]
print hamming(1691)
print hamming(1000000)
```



### Implementation based on priority queue

This is inspired by the Picolisp implementation further down, but uses a priority queue instead of a search tree.  Computes 3x more numbers than necessary, but discards them quickly so memory usage is not too bad.


```python
from heapq import heappush, heappop
from itertools import islice

def h():
    heap = [1]
    while True:
        h = heappop(heap)
        while heap and h==heap[0]:
            heappop(heap)
        for m in [2,3,5]:
            heappush(heap, m*h)
        yield h

print list(islice(h(), 20))
print list(islice(h(), 1690, 1691))
print list(islice(h(), 999999, 1000000)) # runtime 9.5 sec on i5-3570S

```


==="Cyclical Iterators"===
The original author is Raymond Hettinger and the code was first published [http://code.activestate.com/recipes/576961/ here] under the MIT license. Uses iterators dubbed "cyclical" in a sense that they are referring back (explicitly, with <code>p2, p3, p5</code> iterators) to the previously produced values, same as the above versions (through indecies into shared storage) and the classic [[#Haskell|Haskell]] version (implicitly timed by lazy evaluation).

Memory is efficiently maintained automatically by the <code>tee</code> function for each of the three generator expressions, i.e. only that much is maintained as needed to produce the next value (although it looks like the storage is not shared so three copies are maintained implicitly there).

```python
from itertools import tee, chain, groupby, islice
from heapq import merge

def raymonds_hamming():
    # Generate "5-smooth" numbers, also called "Hamming numbers"
    # or "Regular numbers".  See: http://en.wikipedia.org/wiki/Regular_number
    # Finds solutions to 2**i * 3**j * 5**k  for some integers i, j, and k.

    def deferred_output():
        for i in output:
            yield i

    result, p2, p3, p5 = tee(deferred_output(), 4)
    m2 = (2*x for x in p2)                          # multiples of 2
    m3 = (3*x for x in p3)                          # multiples of 3
    m5 = (5*x for x in p5)                          # multiples of 5
    merged = merge(m2, m3, m5)
    combined = chain([1], merged)                   # prepend a starting point
    output = (k for k,g in groupby(combined))       # eliminate duplicates

    return result

print list(islice(raymonds_hamming(), 20))
print islice(raymonds_hamming(), 1689, 1690).next()
print islice(raymonds_hamming(), 999999, 1000000).next()
```

Results are the same as before.

====Non-sharing recursive generator====
Another formulation along the same lines, but greatly simplified, found [http://programmingpraxis.com/2011/08/30/hamming-numbers/#comment-3486 here].
Lacks data sharing, i.e. calls self recursively thus creating a separate copy of the data stream fed to the tee() call, again and again, instead of using its own output.
This [http://ideone.com/PIkWEN gravely impacts the efficiency]. Not to be used.


```python
from heapq import merge
from itertools import tee

def hamming_numbers():
    last = 1
    yield last

    a,b,c = tee(hamming_numbers(), 3)

    for n in merge((2*i for i in a), (3*i for i in b), (5*i for i in c)):
        if n != last:
            yield n
            last = n
```



### =Cyclic generator method #2.=

Cyclic generator method #2. Considerably faster due to early elimination (before merge) of duplicates. Currently the faster Python version. Direct copy of [[Hamming_numbers#Avoiding_generation_of_duplicates | Haskell code]].

```python
from itertools import islice, chain, tee

def merge(r, s):
    # This is faster than heapq.merge.
    rr = r.next()
    ss = s.next()
    while True:
        if rr < ss:
            yield rr
            rr = r.next()
        else:
            yield ss
            ss = s.next()

def p(n):
    def gen():
        x = n
        while True:
            yield x
            x *= n
    return gen()

def pp(n, s):
    def gen():
        for x in (merge(s, chain([n], (n * y for y in fb)))):
            yield x
    r, fb = tee(gen())
    return r

def hamming(a, b = None):
    if not b:
        b = a + 1
    seq = (chain([1], pp(5, pp(3, p(2)))))
    return list(islice(seq, a - 1, b - 1))

print hamming(1, 21)
print hamming(1691)[0]
print hamming(1000000)[0]
```



## Qi

{{incomplete|Qi|Parts 2 & 3 of task missing.}}
{{trans|Clojure}}

```qi
(define smerge
  [X|Xs] [Y|Ys] -> [X | (freeze (smerge (thaw Xs) [Y|Ys]))] where (< X Y)
  [X|Xs] [Y|Ys] -> [Y | (freeze (smerge [X|Xs] (thaw Ys)))] where (> X Y)
  [X|Xs] [_|Ys] -> [X | (freeze (smerge (thaw Xs) (thaw Ys)))])

(define smerge3
  Xs Ys Zs -> (smerge Xs (smerge Ys Zs)))

(define smap
  F [S|Ss] -> [(F S)|(freeze (smap F (thaw Ss)))])

(set hamming [1 | (freeze (smerge3 (smap (* 2) (value hamming))
                                   (smap (* 3) (value hamming))
                                   (smap (* 5) (value hamming))))])

(define stake
  _      0 -> []
  [S|Ss] N -> [S|(stake (thaw Ss) (1- N))])

(stake (value hamming) 20)
```

{{out}}

```txt

[1 2 3 4 5 6 8 9 10 12 15 16 18 20 24 25 27 30 32 36]

```



## R

Recursively find the Hamming numbers below <math>2^{31}</math>.  Shown are results for tasks 1 and 2.  Arbitrary precision integers are not supported natively.

```R
hamming=function(hamms,limit) {
  tmp=hamms
  for(h in c(2,3,5)) {
    tmp=c(tmp,h*hamms)
  }
  tmp=unique(tmp[tmp<=limit])
  if(length(tmp)>length(hamms)) {
    hamms=hamming(tmp,limit)
  }
  hamms
}
h <- sort(hamming(1,limit=2^31-1))
print(h[1:20])
print(h[length(h)])
```

{{out}}

```txt

[1]  1  2  3  4  5  6  8  9 10 12 15 16 18 20 24 25 27 30 32 36
[1] 2125764000

```



###  Alternate version

The '''nextn''' R function provides the needed functionality:


```r
hamming <- function(n) {
  a <- numeric(n)
  a[1] <- 1
  for (i in 2:n) {
    a[i] <- nextn(a[i-1]+1)
  }
  a
}
```


'''Output'''


```txt
 hamming(20)
 [1]  1  2  3  4  5  6  8  9 10 12 15 16 18 20 24 25 27 30 32 36
```



## Racket


```racket
#lang racket
(require racket/stream)
(define first stream-first)
(define rest  stream-rest)

(define (merge s1 s2)
  (define x1 (first s1))
  (define x2 (first s2))
  (cond [(= x1 x2) (merge s1 (rest s2))]
        [(< x1 x2) (stream-cons x1 (merge (rest s1) s2))]
        [else      (stream-cons x2 (merge s1 (rest s2)))]))

(define (mult k) (λ(x) (* x k)))

(define hamming
  (stream-cons
   1 (merge (stream-map (mult 2) hamming)
            (merge (stream-map (mult 3) hamming)
                   (stream-map (mult 5) hamming)))))

(for/list ([i 20] [x hamming]) x)
(stream-ref hamming 1690)
(stream-ref hamming 999999)
```

{{out}}

```txt
'(1 2 3 4 5 6 8 9 10 12 15 16 18 20 24 25 27 30 32 36)
2125764000
519312780448388736089589843750000000000000000000000000000000000000000000000000000000
```


'''Translation of Haskell code avoiding duplicates'''

The above version consumes quite a lot of memory as streams are retained since the head of the stream is a global defined binding "hamming".  The following code implements (hamming) as a function and all heads of streams are locally defined so that they can be garbage collected as they are consumed; as well it is formulated so that no duplicate values are generated so as to simplify the calculation and minimize the number of values in the streams, to further the last it also evaluates the least dense stream first.  The following code is about three times faster than the above code:
{{trans|Haskell}}

```racket
#lang racket
(require racket/stream)
(define first stream-first)
(define rest  stream-rest)

(define (hamming)
  (define (merge s1 s2)
    (let ([x1 (first s1)]
          [x2 (first s2)])
      (if (< x1 x2) ; don't have to handle duplicate case
          (stream-cons x1 (merge (rest s1) s2))
          (stream-cons x2 (merge s1 (rest s2))))))
  (define (smult m s) ; faster than using map (* m)
    (define (smlt ss)
      (stream-cons (* m (first ss)) (smlt (rest ss))))
    (smlt s))
  (define (u s n)
    (if (stream-empty? s) ; checking here more efficient than in merge
        (letrec ([r (smult n (stream-cons 1 r))])
          r)
        (letrec ([r (merge s (smult n (stream-cons 1 r)))])
          r)))
  (stream-cons 1 (stream-fold u empty-stream '(5 3 2))))

(for/list ([i 20] [x (hamming)]) x) (newline)
(stream-ref (hamming) 1690) (newline)
(stream-ref (hamming) 999999) (newline)
```


The output of the above code is the same as that of the earlier code.


## Raven

{{trans|Liberty Basic}}

```raven
define hamming use $limit
    [ ] as $h
    1 $h 0 set
    2 as $x2   3 as $x3    5 as $x5
    0 as $i    0 as $j     0 as $k
    1 $limit 1 + 1 range each as $n
        $x3 $x5 min $x2 min    $h $n   set
        $h $n get   $x2 =  if
            $i  1 +   as $i
            $h $i get    2 *     as $x2
        $h $n get   $x3 =  if
            $j  1 +   as $j
            $h $j get    3 *     as $x3
        $h $n get   $x5 =  if
            $k  1 +   as $k
            $h $k get    5 *     as $x5
    $h   $limit 1 -   get

1 21 1 range each as $lim
    $lim hamming print " " print
"\n" print

"Hamming(1691) is: " print    1691 hamming print    "\n" print

# Raven can't handle > 2^31 using integers
#
#"Hamming(1000000) is: " print   1000000 hamming print    "\n" print
```

{{out}}

```txt
1 2 3 4 5 6 8 9 10 12 15 16 18 20 24 25 27 30 32 36
Hamming(1691) is: 2125764000
```



## REXX

Both REXX versions compute and produce the Hamming numbers in numerical order.

### idiomatic

This REXX program was a direct copy from my old REXX subroutine to compute   '''UGLY'''   numbers,

it computes   ''just enough''   Hamming numbers   (one Hamming number after the current number).

```rexx
/*REXX program computes  Hamming numbers:  1 ──► 20,  # 1691,  and  the  one millionth. */
numeric digits 100                               /*ensure enough decimal digits.        */
call hamming 1, 20                               /*show the 1st ──► twentieth Hamming #s*/
call hamming 1691                                /*show the 1,691st Hamming number.     */
call hamming 1000000                             /*show the  1 millionth Hamming number.*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
hamming:  procedure;   parse arg x,y;    if y==''  then y= x;        w= length(y)
                                 #2= 1;     #3= 1;       #5= 1;      @.= 0;         @.1 =1
              do n=2  for y-1
              @.n= min(2*@.#2, 3*@.#3, 5*@.#5)     /*pick the minimum of 3 (Hamming) #s.*/
              if 2*@.#2 == @.n   then #2 = #2 + 1  /*number already defined?  Use next #*/
              if 3*@.#3 == @.n   then #3 = #3 + 1  /*   "      "       "       "    "  "*/
              if 5*@.#5 == @.n   then #5 = #5 + 1  /*   "      "       "       "    "  "*/
              end   /*n*/                          /* [↑]  maybe assign next 3 Hamming#s*/
                           do j=x  to y;     say  'Hamming('right(j, w)") ="    @.j
                           end   /*j*/

          say right( 'length of last Hamming number ='     length(@.y), 70);           say
          return
```

{{out|output|text=  when using the default inputs:}}

```txt

Hamming( 1) = 1
Hamming( 2) = 2
Hamming( 3) = 3
Hamming( 4) = 4
Hamming( 5) = 5
Hamming( 6) = 6
Hamming( 7) = 8
Hamming( 8) = 9
Hamming( 9) = 10
Hamming(10) = 12
Hamming(11) = 15
Hamming(12) = 16
Hamming(13) = 18
Hamming(14) = 20
Hamming(15) = 24
Hamming(16) = 25
Hamming(17) = 27
Hamming(18) = 30
Hamming(19) = 32
Hamming(20) = 36
                                     length of last Hamming number = 2

Hamming(1691) = 2125764000
                                    length of last Hamming number = 10

Hamming(1000000) = 519312780448388736089589843750000000000000000000000000000000000000000000000000000000
                                    length of last Hamming number = 84

```



### unrolled

This REXX version is roughly twice as fast as the 1<sup>st</sup> REXX version.

```rexx
/*REXX program computes  Hamming numbers:  1 ──► 20,   # 1691,   and  the one millionth.*/
numeric digits 100                               /*ensure enough decimal digits.        */
call hamming       1, 20                         /*show the 1st ──► twentieth Hamming #s*/
call hamming    1691                             /*show the 1,691st Hamming number.     */
call hamming 1000000                             /*show the  1 millionth Hamming number.*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
hamming: procedure;  parse arg x,y;    if y==''  then y= x;            w= length(y)
                                       #2= 1;    #3= 1;     #5= 1;     @.= 0;       @.1= 1
            do n=2  for y-1
            _2= @.#2 + @.#2                      /*this is faster than:      @.#2 * 2   */
            _3= @.#3 * 3
            _5= @.#5 * 5
                              m = _2             /*assume a minimum (of the 3 Hammings).*/
            if _3  < m   then m = _3             /*is this number less than the minimum?*/
            if _5  < m   then m = _5             /* "   "     "     "    "   "     "    */
               @.n = m                           /*now,  assign the next Hamming number.*/
            if _2 == m   then #2= #2 + 1         /*number already defined?   Use next #.*/
            if _3 == m   then #3= #3 + 1         /*   "      "       "        "    "  " */
            if _5 == m   then #5= #5 + 1         /*   "      "       "        "    "  " */
            end   /*n*/                          /* [↑]  maybe assign next Hamming #'s. */
                         do j=x  to y;      say 'Hamming('right(j, w)") ="     @.j
                         end   /*j*/

         say right( 'length of last Hamming number ='     length(@.y), 70);            say
         return
```

{{out|output|text=  is identical to the 1<sup>st</sup> REXX version.}}




## Ring


```ring

see "h(1) = 1" + nl
for nr = 1 to 19
     see "h(" + (nr+1) + ") = " + hamming(nr) + nl
next
see "h(1691) = " + hamming(1690) + nl
see nl

func hamming limit
     h = list(1690)
     h[1] =1
     x2 = 2 x3 = 3 x5 =5
     i  = 0 j  = 0 k  =0
     for n =1 to limit
         h[n]  = min(x2, min(x3, x5))
         if x2 = h[n]  i = i +1  x2 =2 *h[i] ok
         if x3 = h[n]  j = j +1  x3 =3 *h[j] ok
         if x5 = h[n]  k = k +1  x5 =5 *h[k] ok
     next
     hamming = h[limit]
     return hamming

```

Output:

```txt

h(1) = 1
h(2) = 2
h(3) = 3
h(4) = 4
h(5) = 5
h(6) = 6
h(7) = 8
h(8) = 9
h(9) = 10
h(10) = 12
h(11) = 15
h(12) = 16
h(13) = 18
h(14) = 20
h(15) = 24
h(16) = 25
h(17) = 27
h(18) = 30
h(19) = 32
h(20) = 36
h(1691) = 2125764000

```



## Ruby

{{trans|Scala}}
{{works with|Ruby|1.9.3}}

```ruby
hamming = Enumerator.new do |yielder|
  next_ham = 1
  queues = [[ 2, []], [3, []], [5, []] ]

  loop do
    yielder << next_ham   # or: yielder.yield(next_ham)

    queues.each {|m,queue| queue << next_ham * m}
    next_ham = queues.collect{|m,queue| queue.first}.min
    queues.each {|m,queue| queue.shift if queue.first==next_ham}
  end
end
```

And the "main" part of the task

```ruby
start = Time.now

hamming.each.with_index(1) do |ham, idx|
  case idx
  when (1..20), 1691
    puts "#{idx} => #{ham}"
  when 1_000_000
    puts "#{idx} => #{ham}"
    break
  end
end

puts "elapsed: #{Time.now - start} seconds"
```

{{out}}
<pre style='height: 30ex; overflow: scroll'>
1 => 1
2 => 2
3 => 3
4 => 4
5 => 5
6 => 6
7 => 8
8 => 9
9 => 10
10 => 12
11 => 15
12 => 16
13 => 18
14 => 20
15 => 24
16 => 25
17 => 27
18 => 30
19 => 32
20 => 36
1691 => 2125764000
[1000000, 519312780448388736089589843750000000000000000000000000000000000000000000000000000000]
elapsed: 6.522811 seconds

```



## Run BASIC


```runbasic

dim h(1000000)
for i =1 to 20
    print hamming(i);" ";
next i

print
print "Hamming List First(1691)   =";chr$(9);hamming(1691)
print "Hamming List Last(1000000) =";chr$(9);hamming(1000000)

end

function hamming(limit)
    h(0) =1
    x2 = 2: x3 = 3: x5 =5
    i  = 0: j  = 0: k  =0
    for n =1 to limit
        h(n)  = min(x2, min(x3, x5))
        if x2 = h(n) then i = i +1: x2 =2 *h(i)
        if x3 = h(n) then j = j +1: x3 =3 *h(j)
        if x5 = h(n) then k = k +1: x5 =5 *h(k)
    next n
    hamming = h(limit -1)
end function
```


```txt
1 2 3 4 5 6 8 9 10 12 15 16 18 20 24 25 27 30 32 36
Hamming List First(1691)   =	2125764000
Hamming List Last(1000000) =	519312780448388736089589843750000000000000000000000000000000000000000000000000000000
```



## Rust

{{libheader|num}}

### Basic version

{{trans|D}}
Improved by minimizing the number of BigUint comparisons:

```rust
extern crate num;
num::bigint::BigUint;

use std::time::Instant;

fn basic_hamming(n: usize) -> BigUint {
    let two = BigUint::from(2u8);
    let three = BigUint::from(3u8);
    let five = BigUint::from(5u8);
    let mut h = vec![BigUint::from(0u8); n];
    h[0] = BigUint::from(1u8);
    let mut x2 = BigUint::from(2u8);
    let mut x3 = BigUint::from(3u8);
    let mut x5 = BigUint::from(5u8);
    let mut i = 0usize; let mut j = 0usize; let mut k = 0usize;

    // BigUint comparisons are expensive, so do it only as necessary...
    fn min3(x: &BigUint, y: &BigUint, z: &BigUint) -> (usize, BigUint) {
        let (cs, r1) = if y == z { (0x6, y) }
                        else if y < z { (2, y) } else { (4, z) };
        if x == r1 { (cs | 1, x.clone()) }
        else if x < r1 { (1, x.clone()) } else { (cs, r1.clone()) }
    }

    let mut c = 1;
    while c < n { // satisfy borrow checker with extra blocks: {  }
        let (cs, e1) = { min3(&x2, &x3, &x5) };
        h[c] = e1; // vector now owns the generated value
        if (cs & 1) != 0 { i += 1; x2 = &two * &h[i] }
        if (cs & 2) != 0 { j += 1; x3 = &three * &h[j] }
        if (cs & 4) != 0 { k += 1; x5 = &five * &h[k] }
        c += 1;
    }

    match h.pop() {
        Some(v) => v,
        _ => panic!("basic_hamming: arg is zero; no elements")
    }
}

fn main() {
    print!("[");
    for (i, h) in (1..21).map(basic_hamming).enumerate() {
        if i != 0 { print!(",") }
        print!(" {}", h)
    }
    println!(" ]");
    println!("{}", basic_hamming(1691));

    let strt = Instant::now();

    let rslt = basic_hamming(1000000);

    let elpsd = strt.elapsed();
    let secs = elpsd.as_secs();
    let millis = (elpsd.subsec_nanos() / 1000000)as u64;
    let dur = secs * 1000 + millis;

    let rs = rslt.to_str_radix(10);
    let mut s = rs.as_str();
    println!("{} digits:", s.len());
        while s.len() > 100 {
            let (f, r) = s.split_at(100);
            s = r;
            println!("{}", f);
        }
        println!("{}", s);

    println!("This last took {} milliseconds", dur);
}
```

{{output}}

```txt
[ 1, 2, 3, 4, 5, 6, 8, 9, 10, 12, 15, 16, 18, 20, 24, 25, 27, 30, 32, 36 ]
2125764000
84 digits:
519312780448388736089589843750000000000000000000000000000000000000000000000000000000
This last took 677 milliseconds.
```



### Eliminating duplicate calculations


Much of the time above is wasted doing big integer multiplications that are duplicated elsewhere as in 2 times 3 and 3 times 2, etc.  The following code eliminates such duplicate multiplications and reduces the number of comparisons, as follows:

```rust
fn nodups_hamming(n: usize) -> BigUint {
    let two = BigUint::from(2u8);
    let three = BigUint::from(3u8);
    let five = BigUint::from(5u8);
    let mut m = vec![BigUint::from(0u8); 1];
    m[0] = BigUint::from(1u8);
    let mut h = vec![BigUint::from(0u8); n];
    h[0] = BigUint::from(1u8);
    if n > 1 {
        m.push(BigUint::from(3u8)); // for initial x53 advance
        h[1] = BigUint::from(2u8); // for initial x532 advance
    }
    let mut x5 = BigUint::from(5u8);
    let mut x53 = BigUint::from(9u8); // 3 times 3 because already merged one step
    let mut mrg = BigUint::from(3u8);
    let mut x532 = BigUint::from(2u8);

    let mut i = 0usize; let mut j = 1usize;
    let mut c = 1usize;
    while c < n { // satisfy borrow checker with extra blocks: {  }
        if &x532 < &mrg { h[c] = x532; i += 1; x532 = &two * &h[i]; }
        else {	h[c] = mrg;
                if &x53 < &x5 { mrg = x53; j += 1; x53 = &three * &m[j]; }
                else { mrg = x5.clone(); x5 = &five * &x5; };
                m.push(mrg.clone()); };
        c += 1;
    }
    match h.pop() {
        Some(v) => v,
        _ => panic!("nodups_hamming: arg is zero; no elements")
    }
}
```


Substitute the calls to the above code for the calls to "basic_hamming" (three places) in the "main" function above.  The output is the same except that the time expended is less (249 milliseconds), for over two and a half times faster.


### Much faster logarithmic version with low memory use


The above versions spend much of their time doing BigUint calculations.  The below version eliminates much of that time by using integer powers of 2, 3, and 5 representations and all normal integer calculations except for the final conversion to a BitUint for the final result for about a 30 times speed-up.

Another problem is that the above versions use so much memory that they can't compute even the billionth hamming number without running out of memory on a 16 Gigabyte machine.  This version greatly reduces the memory use to about O(n^(2/3)) by eliminating no longer required back values in batches so that with about 9 Gigabytes it will calculate the hamming numbers to 1.2e13 (it's limit due to the ranges of the exponents) in a day or so.  The code is as follows:

```rust
fn log_nodups_hamming(n: u64) -> BigUint {
    if n <= 0 { panic!("nodups_hamming: arg is zero; no elements") }
    if n < 2 { return BigUint::from(1u8) } // trivial case for n == 1
    if n > 1.2e13 as u64 { panic!("log_nodups_hamming: argument too large to guarantee results!") }

    // constants as expanded integers to minimize round-off errors, and
    // reduce execution time using integer operations not float...
    const LAA2: u64 = 35184372088832; // 2.0f64.powi(45)).round() as u64;
    const LBA2: u64 = 55765910372219; // 3.0f64.log2() * 2.0f64.powi(45)).round() as u64;
    const LCA2: u64 = 81695582054030; // 5.0f64.log2() * 2.0f64.powi(45)).round() as u64;

    #[derive(Clone, Copy)]
    struct Logelm { // log representation of an element with only allowable powers
        exp2: u16,
        exp3: u16,
        exp5: u16,
        logr: u64 // log representation used for comparison only - not exact
    }

    impl Logelm {
        fn lte(&self, othr: &Logelm) -> bool {
            if self.logr <= othr.logr { true } else { false }
        }
        fn mul2(&self) -> Logelm {
            Logelm { exp2: self.exp2 + 1, logr: self.logr + LAA2, .. *self }
        }
        fn mul3(&self) -> Logelm {
            Logelm { exp3: self.exp3 + 1, logr: self.logr + LBA2, .. *self }
        }
        fn mul5(&self) -> Logelm {
            Logelm { exp5: self.exp5 + 1, logr: self.logr + LCA2, .. *self }
        }
    }

    let one = Logelm { exp2: 0, exp3: 0, exp5: 0, logr: 0 };
    let mut x532 = one.mul2();
    let mut mrg = one.mul3();
    let mut x53 = one.mul3().mul3(); // advance as mrg has the former value...
    let mut x5 = one.mul5();

    let mut h = Vec::with_capacity(65536); // vec!(one.clone(); 0);
    let mut m = Vec::<Logelm>::with_capacity(65536); // vec!(one.clone(); 0);

    let mut i = 0usize; let mut j = 0usize;
    for _ in 1 .. n {
        let cph = h.capacity();
        if i > cph / 2 { // drain extra unneeded values...
            h.drain(0 .. i);
            i = 0;
        }
        if x532.lte(&mrg) {
            h.push(x532);
            x532 = h[i].mul2();
            i += 1;
        } else {
            h.push(mrg);
            if x53.lte(&x5) {
                mrg = x53;
                x53 = m[j].mul3();
                j += 1;
            } else {
                mrg = x5;
                x5 = x5.mul5();
            }
            let cpm = m.capacity();
            if j > cpm / 2 { // drain extra unneeded values...
                m.drain(0 .. j);
                j = 0;
            }
            m.push(mrg);
        }
    }

    let o = &h[&h.len() - 1];
    let two = BigUint::from(2u8);
    let three = BigUint::from(3u8);
    let five = BigUint::from(5u8);
    let mut ob = BigUint::from(1u8); // convert to BigUint at the end
    for _ in 0 .. o.exp2 { ob = ob * &two }
    for _ in 0 .. o.exp3 { ob = ob * &three }
    for _ in 0 .. o.exp5 { ob = ob * &five }
    ob
}
```


Again, this function can be used with the same "main" as above and the outputs are the same except that the execution time is only 7 milliseconds.  It calculates the hamming number to a billion and just over a second and to one hundred billion in just over 100 seconds - O(n) time complexity.  As well as eliminating duplicate calculations and calculating using exponents rather than BitUint operations, it also reduces the time required as compared to other similar algorithms by scaling the logarithms of two, three, and five into 64-bit integers so no floating point operations are required.  The scaling is such that round-off errors will not affect the order of results for well past the usable range.

Memory used is greatly reduced to O(n^(2/3)) by draining the arrays of back values no longer required in batches (rather than one by one) so that less time is used.  It also saves time by not requiring as many allocations and de-allocations as the draining is done in place, thus making the current capacity of arrays longer usable.


### Sequence version


As the task actually asks for a sequence of Hamming numbers, any of the above three solutions can easily be adapted to output an Iterator sequence; in this case the last fastest one is converted as follows:


```rust
extern crate num; // requires dependency on the num library
use num::bigint::BigUint;

use std::time::Instant;

fn log_nodups_hamming_iter() -> Box<Iterator<Item = (u16, u16, u16)>> {
    // constants as expanded integers to minimize round-off errors, and
    // reduce execution time using integer operations not float...
    const LAA2: u64 = 35184372088832; // 2.0f64.powi(45)).round() as u64;
    const LBA2: u64 = 55765910372219; // 3.0f64.log2() * 2.0f64.powi(45)).round() as u64;
    const LCA2: u64 = 81695582054030; // 5.0f64.log2() * 2.0f64.powi(45)).round() as u64;

    #[derive(Clone, Copy)]
    struct Logelm { // log representation of an element with only allowable powers
        exp2: u16,
        exp3: u16,
        exp5: u16,
        logr: u64 // log representation used for comparison only - not exact
    }
    impl Logelm {
        fn lte(&self, othr: &Logelm) -> bool {
            if self.logr <= othr.logr { true } else { false }
        }
        fn mul2(&self) -> Logelm {
            Logelm { exp2: self.exp2 + 1, logr: self.logr + LAA2, .. *self }
        }
        fn mul3(&self) -> Logelm {
            Logelm { exp3: self.exp3 + 1, logr: self.logr + LBA2, .. *self }
        }
        fn mul5(&self) -> Logelm {
            Logelm { exp5: self.exp5 + 1, logr: self.logr + LCA2, .. *self }
        }
    }

    let one = Logelm { exp2: 0, exp3: 0, exp5: 0, logr: 0 };
    let mut x532 = one.mul2();
    let mut mrg = one.mul3();
    let mut x53 = one.mul3().mul3(); // advance as mrg has the former value...
    let mut x5 = one.mul5();

    let mut h = Vec::with_capacity(65536);
    let mut m = Vec::<Logelm>::with_capacity(65536);

    let mut i = 0usize; let mut j = 0usize;
    Box::new((0u64 .. ).map(move |it| if it < 1 { (0, 0, 0) } else {
        let cph = h.capacity();
        if i > cph / 2 {
            h.drain(0 .. i);
            i = 0;
        }
        if x532.lte(&mrg) {
            h.push(x532);
            x532 = h[i].mul2();
            i += 1;
        } else {
            h.push(mrg);
            if x53.lte(&x5) {
                mrg = x53;
                x53 = m[j].mul3();
                j += 1;
            } else {
                mrg = x5;
                x5 = x5.mul5();
            }
            let cpm = m.capacity();
            if j > cpm / 2 {
                m.drain(0 .. j);
                j = 0;
            }
            m.push(mrg);
        }
        let o = &h[&h.len() - 1];
        (o.exp2, o.exp3, o.exp5)
    }))
}

fn convert_log2big(o: (u16, u16, u16)) -> BigUint {
    let two = BigUint::from(2u8);
    let three = BigUint::from(3u8);
    let five = BigUint::from(5u8);
    let (x2, x3, x5) = o;
    let mut ob = BigUint::from(1u8); // convert to BigUint at the end
    for _ in 0 .. x2 { ob = ob * &two }
    for _ in 0 .. x3 { ob = ob * &three }
    for _ in 0 .. x5 { ob = ob * &five }
    ob
}

fn main() {
    print!("[");
    for (i, h) in log_nodups_hamming_iter().take(20).map(convert_log2big).enumerate() {
        if i != 0 { print!(",") }
        print!(" {}", h)
    }
    println!(" ]");
    println!("{}", convert_log2big(log_nodups_hamming_iter().take(1691).last().unwrap()));

    let strt = Instant::now();

//  let rslt = convert_log2big(log_nodups_hamming_iter().take(1000000000).last().unwrap());
    let mut it = log_nodups_hamming_iter().into_iter();
    for _ in 0 .. 100-1 { // a little faster; less one level of iteration
        let _ = it.next();
    }
    let rslt = convert_log2big(it.next().unwrap());

    let elpsd = strt.elapsed();
    let secs = elpsd.as_secs();
    let millis = (elpsd.subsec_nanos() / 1000000)as u64;
    let dur = secs * 1000 + millis;

    println!("2^{} times 3^{} times 5^{}", rslt.0, rslt.1, rslt.2);
    let rs = convert_log2big(rslt).to_str_radix(10);
    let mut s = rs.as_str();
    println!("{} digits:", s.len());
    let lg3 = 3.0f64.log2();
    let lg5 = 5.0f64.log2();
    let lg = (rslt.0 as f64 + rslt.1 as f64 * lg3
	         + rslt.2 as f64 * lg5) * 2.0f64.log10();
    println!("Approximately {}E+{}", 10.0f64.powf(lg.fract()), lg.trunc());
    if s.len() <= 10000 {
        while s.len() > 100 {
            let (f, r) = s.split_at(100);
            s = r;
            println!("{}", f);
        }
        println!("{}", s);
    }

    println!("This last took {} milliseconds.", dur);
}
```

{{output}}

```txt
[ 1, 2, 3, 4, 5, 6, 8, 9, 10, 12, 15, 16, 18, 20, 24, 25, 27, 30, 32, 36 ]
2125764000
2^55 times 3^47 times 5^64
84 digits:
Approximately 5.193127804483804E+83
519312780448388736089589843750000000000000000000000000000000000000000000000000000000
This last took 17 milliseconds.
```


The above final output is the same as the last one, but the function is called differently; also note that it is somewhat slower than the last version due to the extra function calls required to enumerate over an Iterator.  It can enumerate the Hamming numbers up to a billion in about 20 seconds instead of the about ten seconds for the last version - about O(n) time complexity, and has the same O(n^(2/3)) space complexity as the last version.


### Functional sequence version avoiding duplicates


It has been said by some that Rust is basically a functional language; however that isn't quite true in several respects, at least as per the following:
# It does not guaranty tail call optimization for functions, thus sometimes requiring imperative forms of code to produce that effect.
# It does not have currying or partial application of function arguments without using kludges of nested function/closure calls.
# move closures cannot use recursive shared values without using interior mutability inside a reference counted value (required here)
# Closures are not recursive without using a trick involving shared state reference counted values (demonstrated here).
# It currently does not have a standard library implementation of a lazily computed value (required to implement a Lazy List, and
# It accordingly is not as easy as in most other languages to implement Co-Inductive Streams or (also memoized) Lazy Lists (a form of Lazy List is required here).


Many of these come about due to the Rust memory model where pieces of programs "own" data and its disposal but can assign references to other pieces of code (with limits if mutability is required), instead of the Garbage Collected model used by most other functional languages where variables are owned by the system and program code just uses references to that data other than for primitives which are owned by whoever uses them.

The lack of the Lazy type and thus the Lazy List type is partly due to Rust's still being relatively unstable, as Lazy requires a "thunk" (a zero argument move closure acting on owned data - FnOnce in Rust), and in Rust these must be boxed (allocated on the heap) to be usable; however "Box<FnOnce() -> T>" does not currently work, the stopgap measure of "FnBox" is marked as unstable and thus can only be used in (unstable) "nightly" version of the compiler, and the "thunk" standard library which implemented the "Invoke" trait boilerplate code that made it possible was removed from the current version in anticipation of something better.  In this code I have accordingly resurrected just enough of the old standard library code to make this work.

[http://https://github.com/reem/rust-lazy/blob/master/src/single.rs Jeremy Reems had implemented Lazy] and [http://https://github.com/reem/rust-lazylist/blob/master/src/lib.rs also LazyList], but they haven't been maintained for many years and don't compile (likely waiting for stable features).  According, I have implemented enough of this functionality as required by this algorithm, as per the following code (tested on Rust version 1.13):
{{trans|Haskell}}

```rust
extern crate num;
use num::bigint::BigUint;

use std::rc::Rc;
use std::iter::FromIterator;
use std::cell::{UnsafeCell, RefCell};
use std::mem;

use std::time::Instant;

// since Box<FnOnce() -> T + 'a> doesn't currently work and
// FnBox, which does work, (version 1.13) is UnStable;
// use the boilerplate Invoke trait and Thunk
// from the old removed thunk standard library...

pub trait Invoke<R = ()> {
    fn invoke(self: Box<Self>) -> R;
}

impl<R, F: FnOnce() -> R> Invoke<R> for F {
    #[inline(always)]
    fn invoke(self: Box<F>) -> R { (*self)() }
}

pub struct Thunk<'a, R>(Box<Invoke<R> + 'a>);

impl<'a, R: 'a> Thunk<'a, R> {
    #[inline(always)]
    fn new<F: 'a + FnOnce() -> R>(func: F) -> Thunk<'a, R> {
        Thunk(Box::new(func))
    }
    #[inline(always)]
    fn invoke(self) -> R { self.0.invoke() }
}

// actual Lazy implementation starts here...

use self::LazyState::*;

pub struct Lazy<'a, T: 'a>(UnsafeCell<LazyState<'a, T>>);

enum LazyState<'a, T: 'a> {
    Unevaluated(Thunk<'a, T>),
    EvaluationInProgress,
    Evaluated(T)
}

impl<'a, T: 'a> Lazy<'a, T>{
    #[inline]
    pub fn new<'b, F>(thunk: F) -> Lazy<'b, T>
            where F: 'b + FnOnce() -> T {
        Lazy(UnsafeCell::new(Unevaluated(Thunk::new(thunk))))
    }
    #[inline]
    pub fn evaluated(val: T) -> Lazy<'a, T> {
        Lazy(UnsafeCell::new(Evaluated(val)))
    }
    #[inline]
    fn force<'b>(&'b self) { // not thread-safe
        unsafe {
            match *self.0.get() {
                Evaluated(_) => return, // nothing required; already Evaluated
                EvaluationInProgress => panic!("Lazy::force called recursively!!!"),
                _ => () // need to do following something else if Unevaluated...
            } // following eliminates recursive race; drops neither on replace...
            match mem::replace(&mut *self.0.get(), EvaluationInProgress) {
                Unevaluated(thnk) => { // thnk can't call force on the same Lazy
                    *self.0.get() = Evaluated(thnk.invoke());
                },
                _ => unreachable!() // already took care of other cases in above match.
            }
        }
    }
    #[inline]
    pub fn value<'b>(&'b self) -> &'b T {
        self.force(); // evaluatate if not evealutated
        match unsafe { &*self.0.get() } {
            &Evaluated(ref v) => v, // return value
            _ => { unreachable!() } // previous force guarantees never not Evaluated
        }
    }
    #[inline]
    pub fn unwrap<'b>(self) -> T where T: 'b { // consumes the object to produce the value
        self.force(); // evaluatate if not evealutated
        match unsafe { self.0.into_inner() } {
            Evaluated(v) => v,
            _ => unreachable!() // previous code guarantees never not Evaluated
        }
    }
}

// now for immutable persistent shareable (memoized) LazyList via Lazy above

type RcLazyListNode<'a, T: 'a> = Rc<Lazy<'a, LazyList<'a, T>>>;

use self::LazyList::*;

#[derive(Clone)]
enum LazyList<'a, T: 'a + Clone> {
    /// The Empty List
    Empty,
    /// A list with one member and possibly another list.
    Cons(T, RcLazyListNode<'a, T>)
}

impl<'a, T: 'a + Clone> LazyList<'a, T> {
    #[inline]
    pub fn cons<F>(v: T, cntf: F) -> LazyList<'a, T>
            where F: 'a + FnOnce() -> LazyList<'a, T> {
        Cons(v, Rc::new(Lazy::new(cntf)))
    }
    #[inline]
    pub fn head<'b>(&'b self) -> &'b T {
        if let Cons(ref hd, _) = *self { return hd }
        panic!("LazyList::head called on an Empty LazyList!!!")
    }
    #[inline]
    pub fn tail<'b>(&'b self) -> &'b Lazy<'a, LazyList<'a, T>> {
        if let Cons(_, ref rlln) = *self { return &*rlln }
        panic!("LazyList::tail called on an Empty LazyList!!!")
    }
    #[inline]
    pub fn unwrap(self) -> (T, RcLazyListNode<'a, T>) { // consumes the object
        if let Cons(hd, rlln) = self {
            return (hd, rlln) }
        panic!("LazyList::unwrap called on an Empty LazyList!!!")
    }
}

impl<'a, T: 'a + Clone> Iterator for LazyList<'a, T> {
    type Item = T;
    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        if let Empty = *self { return None }
        let oldll = mem::replace(self, Empty);
        let (hd, rlln) = oldll.unwrap();
        let mut newll = rlln.value().clone();
        mem::swap(self, &mut newll); // self now contains tail, newll contains the Empty
        Some(hd)
    }
}

// implements worker wrapper recursion closures using shared RcMFn variable...

type RcMFn<'a, T: 'a> = Rc<UnsafeCell<Box<FnMut(T) -> T + 'a>>>;

//#[derive(Clone)]
//struct RcMFn<'a, T: 'a>(Rc<UnsafeCell<Box<FnMut() -> T + 'a>>>);

trait RcMFnMethods<'a, T> {
    fn create<F: FnMut(T) -> T + 'a>(v: F) -> RcMFn<'a, T>;
    fn invoke(&self, v: T) -> T;
    fn set<F: FnMut(T) -> T + 'a>(&self, v: F);
}

impl<'a, T: 'a> RcMFnMethods<'a, T> for RcMFn<'a, T> {
    fn create<F: FnMut(T) -> T + 'a>(v: F) -> RcMFn<'a, T> { // creates new value wrapper
        Rc::new(UnsafeCell::new(Box::new(v)))
    }
    #[inline(always)] // needs to be faster to be worth it
    fn invoke(&self, v: T) -> T {
        unsafe { (*(*(*self).get()))(v) }
    }
    fn set<F: FnMut(T) -> T + 'a>(&self, v: F) {
        unsafe { *self.get() = Box::new(v); }
    }
}

// implementation for a reference-counted, interior-mutable variable
// necessary for such things as sharing data and recursive variables

type RcMVar<T> = Rc<RefCell<T>>;

//#[derive(Clone)]
//struct RcMVar<T>(Rc<RefCell<T>>);

trait RcMVarMethods<T> {
    fn create(v: T) -> Self;
    fn get(self: &Self) -> T;
    fn set(self: &Self, v: T);
}

impl<T: Clone> RcMVarMethods<T> for RcMVar<T> {
    fn create(v: T) -> RcMVar<T> { // creates new value wrapped in RcMVar
        Rc::new(RefCell::new(v))
    }
    #[inline]
    fn get(&self) -> T {
        self.borrow().clone()
    }
    fn set(&self, v: T) {
        *self.borrow_mut() = v;
    }
}

// finally what the task objective requires...

fn hammings() -> Box<Iterator<Item = Rc<BigUint>>> {
    type LL<'a> = LazyList<'a, Rc<BigUint>>;
    fn merge<'a>(x: LL<'a>, y: LL<'a>) -> LL<'a> {
        let lte = { x.head() <= y.head() }; // private context for borrow
        if lte {
            let (hdx, tlx) = x.unwrap();
            LL::cons(hdx, move || merge(tlx.value().clone(), y))
        } else {
            let (hdy, tly) = y.unwrap();
            LL::cons(hdy, move || merge(x, tly.value().clone()))
        }
    }
    fn smult<'a>(m: BigUint, s: LL<'a>) -> LL<'a> { // like map m * but faster...
        let smlt = RcMFn::create(move |ss: LL<'a>| ss);
        let csmlt = smlt.clone();
        smlt.set(move |ss: LL<'a>| {
            let (hd, tl) = ss.unwrap();
            let ccsmlt = csmlt.clone();
            LL::cons(Rc::new(&m * &*hd), move || ccsmlt.invoke(tl.value().clone()))
        });
        smlt.invoke(s)
    }
    fn u<'a>(s: LL<'a>, n: usize) -> LL<'a> {
        let nb = BigUint::from(n);
        let rslt = RcMVar::create(Empty);
        let crslt = rslt.clone(); // same interior data...
        let cll = LL::cons(Rc::new(BigUint::from(1u8)), move || crslt.get()); // gets future value
        // below sets future value for above closure...
        rslt.set(if let Empty = s { smult(nb, cll) } else { merge(s, smult(nb, cll)) });
        rslt.get()
    }
    fn rll<'a>() -> LL<'a> { [5, 3, 2].into_iter()
                                .fold(Empty, |ll, n| u(ll, *n) ) }
    let hmng = LL::cons(Rc::new(BigUint::from(1u8)), move || rll());
    Box::new(hmng.into_iter())
}

// and the required test outputs...

fn main() {
    print!("[");
    for (i, h) in hammings().take(20).enumerate() {
        if i != 0 { print!(",") }
        print!(" {}", h)
    }
    println!(" ]");

    println!("{}", hammings().take(1691).last().unwrap());

    let strt = Instant::now();

    let rslt = hammings().take(1000000).last().unwrap();

    let elpsd = strt.elapsed();
    let secs = elpsd.as_secs();
    let millis = (elpsd.subsec_nanos() / 1000000)as u64;
    let dur = secs * 1000 + millis;

    println!("{}", rslt);

    println!("This last took {} milliseconds.", dur);
}
```

As can be seen, there is little code necessary for the "hammings" and "main" functions if the rest were available in libraries, as they really should be once Rust is more stable.
{{output}}

```txt
[ 1, 2, 3, 4, 5, 6, 8, 9, 10, 12, 15, 16, 18, 20, 24, 25, 27, 30, 32, 36 ]
2125764000
519312780448388736089589843750000000000000000000000000000000000000000000000000000000
This last took 408 milliseconds.
```


In order to run this fast. the BigUint LazyList values are wrapped in a reference counted heap wrapper to make it more efficient for cloning operations as necessary to extract interior values from the nested RcLazyListNode structure.

This is reasonably fast, with it a little slower than some languages as the BigUint library isn't as fast as some, but about 75% of the time is spent on LazyList processing.  This is likely due to the many heap allocations and de-allocations required as well as the time required to process all of the reference counting.  At that, on the same machine (Intel Sky Lake i5-6500 @ 3.2 Gigahertz) it is still about four times faster than F# and two times faster than C# running the same functional algorithm.  However, it is about three times slower than Java JVM based languages (Scala, Kotlin, Clojure, etc.) and about four times slower than Haskell, likely due to those languages having very efficient memory management for many frequent small-byte-sizes per allocation/collection as for such functional algorithms, and as well not requiring reference counting due to garbage collection.

'''So Rust can be used to implement purely functional algorithms, but it isn't the best at it neither for conciseness of code nor for performance.'''

===Extremely fast non-sequence version by calculation of top band of Hamming numbers===

One might ask "What could possibly be done to further speed up finding Hamming numbers?": the answer is quite a lot, but one has to dump the ability to iterate a sequence as that depends on being able to refer to past calculated values by back pointers to the memorized O(n^(2/3)) arrays or lists and thus quite large amounts of memory.  If one just wants to find very large Hamming numbers individually, one looks to the [mathematical analysis of Hamming/regular numbers on Wikipedia](https://en.wikipedia.org/wiki/Regular_number) and finds there is quite an exact relationship between 'n', the sequence number, and the logarithmic magnitude of the resulting Hamming number, and that the error term is directly proportional to the logarithm of that output number.  This means that only the band of Hamming values as wide of this error and including the estimated value need to be generated, and that we need only iterate over two of the three prime exponents, thus O(n^(2/3)) time complexity and O(n^(1/3)) space complexity.  The following code was adapted from [an article in DDJ](http://www.drdobbs.com/architecture-and-design/hamming-problem/228700538) and the Haskell code with the further refinements to decrease the memory requirements as described above:
{{trans|Haskell}}

```rust
extern crate num; // requires dependency on the num library
use num::bigint::BigUint;

use std::time::Instant;

fn nth_hamming(n: u64) -> (u32, u32, u32) {
    if n < 2 {
        if n <= 0 { panic!("nth_hamming: argument is zero; no elements") }
        return (0, 0, 0) // trivial case for n == 1
    }

    let lg3 = 3.0f64.ln() / 2.0f64.ln(); // log base 2 of 3
    let lg5 = 5.0f64.ln() / 2.0f64.ln(); // log base 2 of 5
    let fctr = 6.0f64 * lg3 * lg5;
    let crctn = 30.0f64.sqrt().ln() / 2.0f64.ln(); // log base 2 of sqrt 30
    let lgest = (fctr * n as f64).powf(1.0f64/3.0f64)
                    - crctn; // from WP formula
    let frctn = if n < 1000000000 { 0.509f64 } else { 0.105f64 };
    let lghi = (fctr * (n as f64 + frctn * lgest)).powf(1.0f64/3.0f64)
                    - crctn; // calculate hi log limit based on log(N) - WP article
    let lglo = 2.0f64 * lgest - lghi; // and a lower limit of the upper "band"
    let mut count = 0; // need to use extended precision, might go over
    let mut bnd = Vec::with_capacity(0);
    let klmt = (lghi / lg5) as u32 + 1;
    for k in 0 .. klmt { // i, j, k values can be just u32 values
        let p = k as f64 * lg5;
        let jlmt = ((lghi - p) / lg3) as u32 + 1;
        for j in 0 .. jlmt {
            let q = p + j as f64 * lg3;
            let ir = lghi - q;
            let lg = q + (ir as u32) as f64; // current log value (estimated)
            count += ir as u64 + 1;
            if lg >= lglo {
                bnd.push((lg, (ir as u32, j, k)))
            }
        }
    }
    if n > count { panic!("nth_hamming: band high estimate is too low!") };
    let ndx = (count - n) as usize;
    if ndx >= bnd.len() { panic!("nth_hamming: band low estimate is too high!") };
    bnd.sort_by(|a, b| b.0.partial_cmp(&a.0).unwrap()); // sort decreasing order

    bnd[ndx].1
}

fn convert_log2big(o: (u32, u32, u32)) -> BigUint {
    let two = BigUint::from(2u8);
    let three = BigUint::from(3u8);
    let five = BigUint::from(5u8);
    let (x2, x3, x5) = o;
    let mut ob = BigUint::from(1u8); // convert to BigUint at the end
    for _ in 0 .. x2 { ob = ob * &two }
    for _ in 0 .. x3 { ob = ob * &three }
    for _ in 0 .. x5 { ob = ob * &five }
    ob
}

fn main() {
    print!("[");
    for (i, h) in (1 .. 21).map(nth_hamming).enumerate() {
        if i != 0 { print!(",") }
        print!(" {}", convert_log2big(h))
    }
    println!(" ]");
    println!("{}", convert_log2big(nth_hamming(1691)));

    let strt = Instant::now();

    let rslt = nth_hamming(1000000);

    let elpsd = strt.elapsed();
    let secs = elpsd.as_secs();
    let millis = (elpsd.subsec_nanos() / 1000000)as u64;
    let dur = secs * 1000 + millis;

    println!("2^{} times 3^{} times 5^{}", rslt.0, rslt.1, rslt.2);
    let rs = convert_log2big(rslt).to_str_radix(10);
    let mut s = rs.as_str();
    println!("{} digits:", s.len());
    let lg3 = 3.0f64.log2();
    let lg5 = 5.0f64.log2();
    let lg = (rslt.0 as f64 + rslt.1 as f64 * lg3
            + rslt.2 as f64 * lg5) * 2.0f64.log10();
    println!("Approximately {}E+{}", 10.0f64.powf(lg.fract()), lg.trunc());
    if s.len() <= 10000 {
        while s.len() > 100 {
            let (f, r) = s.split_at(100);
            s = r;
            println!("{}", f);
        }
        println!("{}", s);
    }

    println!("This last took {} milliseconds.", dur);
}
```



```txt
[ 1, 2, 3, 4, 5, 6, 8, 9, 10, 12, 15, 16, 18, 20, 24, 25, 27, 30, 32, 36 ]
2125764000
2^55 times 3^47 times 5^64
84 digits:
Approximately 5.193127804483804E+83
519312780448388736089589843750000000000000000000000000000000000000000000000000000000
This last took 0 milliseconds.
```


The above code takes too little time to calculate the millionth Hamming numbers to be measured (as seen above), calculates the billionth number in under 10 milliseconds, calculates the trillionth in less than a second, and the thousand trillionth (10^15) in just over a minute (72 seconds).  However, the program needs to be tuned for correctness for ranges of about the 100 trillionth value and above as the precision of the log approximation is not sufficient above about that level to maintain the proper sort order, and thus the answers will start to be out by one value or more.  The answers are likely correct up to that point as they are the same to a trillion as the equivalent Haskell program, although this version is much faster due to no garbage collection (the Haskell version spends about half its time garbage collecting) and doing the calculations using loops and array/vector accesses rather than the lazy list processing used in the Haskell version.  The program should be able to determine the 10^19th hamming number in a few hours and can't quite find the 2^64th (18446744073709551615th) Hamming number due to a slight overflow near the limit.

The above code uses the library vector sort capabilities; custom sorting versions could be written but with the reduced array size, sorting is a very small percentage of the execution time and maximum space requirements are only a few 10's of Megabytes so that neither the time nor the space used for sorting are a concern.

Note that I'm not knocking Haskell, just that (as here) many Haskell programmers like to use lazy list processing which has its costs; the Haskell version could be re-written to use arrays and functional loops and likely be about the same speed although perhaps not as concise.  By simply converting the Haskell program to force strictness and to use this same method of determining the width of the upper band, the Haskell program would have the same time and space complexity as here, but would still be a constant factor of almost eight times slower due to the list processing (with a constant factor for extra space as well).  Use of a mutable array or vector would solve that, but unfortunately not as easily as here as there would be the question of "unboxed" versus "boxed" arrays/vectors, and the complexities of implementing the (faster) unboxed type in which to sort the band - in short, not as easy as here in Rust.


## Scala


```scala
class Hamming extends Iterator[BigInt] {
  import scala.collection.mutable.Queue
  val qs = Seq.fill(3)(new Queue[BigInt])
  def enqueue(n: BigInt) = qs zip Seq(2, 3, 5) foreach { case (q, m) => q enqueue n * m }
  def next = {
    val n = qs map (_.head) min;
    qs foreach { q => if (q.head == n) q.dequeue }
    enqueue(n)
    n
  }
  def hasNext = true
  qs foreach (_ enqueue 1)
}
```

However, the usage of closures adds a significant amount of time. The code below, though a bit uglier because of the repetitions, is twice as fast:

```scala
class Hamming extends Iterator[BigInt] {
  import scala.collection.mutable.Queue
  val q2 = new Queue[BigInt]
  val q3 = new Queue[BigInt]
  val q5 = new Queue[BigInt]
  def enqueue(n: BigInt) = {
    q2 enqueue n * 2
    q3 enqueue n * 3
    q5 enqueue n * 5
  }
  def next = {
    val n = q2.head min q3.head min q5.head
    if (q2.head == n) q2.dequeue
    if (q3.head == n) q3.dequeue
    if (q5.head == n) q5.dequeue
    enqueue(n)
    n
  }
  def hasNext = true
  List(q2, q3, q5) foreach (_ enqueue 1)
}
```

Usage:

```txt

scala> new Hamming take 20 toList
res87: List[BigInt] = List(1, 2, 3, 4, 5, 6, 8, 9, 10, 12, 15, 16, 18, 20, 24, 25, 27, 30, 32, 36)

scala> new Hamming drop 1690 next
res88: BigInt = 2125764000

scala> new Hamming drop 999999 next
res89: BigInt = 519312780448388736089589843750000000000000000000000000000000000000000000000000000000

```

There's also a fairly mechanical translation from Haskell using purely functional lazy streams
{{trans|Haskell}}

```scala
val hamming : Stream[BigInt] = {
   def merge(inx : Stream[BigInt], iny : Stream[BigInt]) : Stream[BigInt] = {
      if (inx.head < iny.head) inx.head #:: merge(inx.tail, iny) else
      if (iny.head < inx.head) iny.head #:: merge(inx, iny.tail) else
      merge(inx, iny.tail)
   }

   1 #:: merge(hamming map (_ * 2), merge(hamming map (_ * 3), hamming map (_ * 5)))
}
```

Use of "force" ensures that the stream is computed before being printed, otherwise it would just be left suspended and you'd see "Stream(1, ?)"

```txt

scala> (hamming take 20).force
res0: scala.collection.immutable.Stream[BigInt] = Stream(1, 2, 3, 4, 5, 6, 8, 9, 10, 12, 15, 16, 18, 20, 24, 25, 27, 30, 32, 36)

```

To get the nth code find the n-1th element because indexes are 0 based

```txt

scala> hamming(1690)
res1: BigInt = 2125764000

```

To calculate the 1000000th code I had to increase the JVM heap from the default

```txt

scala> hamming(999999)
res2: BigInt = 519312780448388736089589843750000000000000000000000000000000000000000000000000000000

```


'''Translation of Haskell code avoiding duplicates'''

One can fix the problems of the memory use of the above code resulting from the entire stream being held in memory due to the use a "val hamming: Stream[BigInt]" by using a function "def hamming(): Stream[BigInt]" and making temporary local variables for intermediate streams so that the beginnings of the streams are garbage collected as the output stream is consumed; one can also implement the other Haskell algorithm to avoid factor duplication by building each stream on successive streams, again with memory conserved by building the least dense first:

```scala
  def hamming(): Stream[BigInt] = {
    def merge(a: Stream[BigInt], b: Stream[BigInt]): Stream[BigInt] = {
      if (a.isEmpty) b else {
        val av = a.head; val bv = b.head
        if (av < bv) av #:: merge(a.tail, b)
        else bv #:: merge(a, b.tail) } }
    def smult(m:Int, s: Stream[BigInt]): Stream[BigInt] =
      (m * s.head) #:: smult(m, s.tail) // equiv to map (m *) s; faster
    def u(s: Stream[BigInt], n: Int): Stream[BigInt] = {
      lazy val r: Stream[BigInt] = merge(s, smult(n, 1 #:: r))
      r }
    1 #:: List(5, 3, 2).foldLeft(Stream.empty[BigInt]) { u } }
```

Usage:

```txt

scala> hamming() take 20 force
res0: scala.collection.immutable.Stream[BigInt] = Stream(1, 2, 3, 4, 5, 6, 8, 9, 10, 12, 15, 16, 18, 20, 24, 25, 27, 30, 32, 36)

scala> hamming() drop 1690 head
res1: BigInt = 2125764000

scala> hamming() drop 999999 head
res2: BigInt = 519312780448388736089589843750000000000000000000000000000000000000000000000000000000

```

It only takes under a half second to find the millionth number in the sequence in the last output.


## Scheme


```scheme
(define-syntax lons
  (syntax-rules ()
    ((_ lar ldr) (delay (cons lar (delay ldr))))))

(define (lar lons)
  (car (force lons)))

(define (ldr lons)
  (force (cdr (force lons))))

(define (lap proc . llists)
  (lons (apply proc (map lar llists)) (apply lap proc (map ldr llists))))

(define (take n llist)
  (if (zero? n)
      (list)
      (cons (lar llist) (take (- n 1) (ldr llist)))))

(define (llist-ref n llist)
  (if (= n 1)
      (lar llist)
      (llist-ref (- n 1) (ldr llist))))

(define (merge llist-1 . llists)
  (define (merge-2 llist-1 llist-2)
    (cond ((null? llist-1) llist-2)
          ((null? llist-2) llist-1)
          ((< (lar llist-1) (lar llist-2))
           (lons (lar llist-1) (merge-2 (ldr llist-1) llist-2)))
          ((> (lar llist-1) (lar llist-2))
           (lons (lar llist-2) (merge-2 llist-1 (ldr llist-2))))
          (else (lons (lar llist-1) (merge-2 (ldr llist-1) (ldr llist-2))))))
  (if (null? llists)
      llist-1
      (apply merge (cons (merge-2 llist-1 (car llists)) (cdr llists)))))

(define hamming
  (lons 1
        (merge (lap (lambda (x) (* x 2)) hamming)
               (lap (lambda (x) (* x 3)) hamming)
               (lap (lambda (x) (* x 5)) hamming))))

(display (take 20 hamming))
(newline)
(display (llist-ref 1691 hamming))
(newline)
(display (llist-ref 1000000 hamming))
(newline)
```

{{out}}

```txt
(1 2 3 4 5 6 8 9 10 12 15 16 18 20 24 25 27 30 32 36)
2125764000
out of memory
```


===Avoiding Generation of Duplicates, including reduced memory use===

{{trans|Haskell}}

Although the algorithm above is true to the classic Dijkstra version and although the algorithm does require a form of lazy list/stream processing in order to utilize memoization and avoid repeated recalculations/comparisons, the stream implementation can be simplified, and the modified algorithm as per the Haskell code avoids duplicate generations of factors.  As well, the following code implements the algorithm as a procedure/function so that it restarts the calculation from the beginning on every new call and so that internal stream variables are not top level so that the garbage collector can collect the beginning of all intermediate and final streams when they are no longer referenced; in this way total memory used (after interspersed garbage collections) is almost zero for a sequence of the first million numbers.  Note that Scheme R5RS does not define "map" or "foldl" functions, so these are provided (a simplified "smult" which is faster than using map for this one purpose):

```scheme
(define (hamming)
  (define (foldl f z l)
    (define (foldls zs ls)
      (if (null? ls) zs (foldls (f zs (car ls)) (cdr ls))))
    (foldls z l))
  (define (merge a b)
    (if (null? a) b
      (let ((x (car a)) (y (car b)))
        (if (< x y) (cons x (delay (merge (force (cdr a)) b)))
                    (cons y (delay (merge a (force (cdr b)))))))))
  (define (smult m s) (cons (* m (car s)) ;; equiv to map (* m) s; faster
             (delay (smult m (force (cdr s))))))
  (define (u s n) (letrec ((a (merge s (smult n (cons 1 (delay a)))))) a))
  (cons 1 (delay (foldl u '() '(5 3 2)))))

;;; test...
(define (stream-take->list n strm)
  (if (= n 0) (list) (cons (car strm)
    (stream-take->list (- n 1) (force (cdr strm))))))
(define (stream-ref strm nth)
  (do ((nxt strm (force (cdr nxt))) (cnt 0 (+ cnt 1)))
      ((>= cnt nth) (car nxt))))
(display (stream-take->list 20 (hamming))) (newline)
(display (stream-ref (hamming) (- 1691 1))) (newline)
(display (stream-ref (hamming) (- 1000000 1))) (newline)
```


{{output}}

```txt
{1 2 3 4 5 6 8 9 10 12 15 16 18 20 24 25 27 30 32 36}
2125764000
519312780448388736089589843750000000000000000000000000000000000000000000000000000000
```


The "stream-ref" procedure is zero based as is the Scheme standard for array indices, thus the subtraction of one from the desired nth number in the sequence.


## Seed7


```seed7
$ include "seed7_05.s7i";
  include "bigint.s7i";

const func bigInteger: min (in bigInteger: a, in bigInteger: b, in bigInteger: c) is func
  result
    var bigInteger: min is 0_;
  begin
    if a < b then
      min := a;
    else
      min := b;
    end if;
    if c < min then
      min := c;
    end if;
  end func;

const func bigInteger: hamming (in integer: n) is func
  result
    var bigInteger: hammingNum is 1_;
  local
    var array bigInteger: hammingNums is 0 times 0_;
    var integer: index is 0;
    var bigInteger: x2 is 2_;
    var bigInteger: x3 is 3_;
    var bigInteger: x5 is 5_;
    var integer: i is 1;
    var integer: j is 1;
    var integer: k is 1;
  begin
    hammingNums := n times 1_;
    for index range 2 to n do
      hammingNum := min(x2, x3, x5);
      hammingNums[index] := hammingNum;
      if x2 = hammingNum then
        incr(i);
        x2 := 2_ * hammingNums[i];
      end if;
      if x3 = hammingNum then
        incr(j);
        x3 := 3_ * hammingNums[j];
      end if;
      if x5 = hammingNum then
        incr(k);
        x5 := 5_ * hammingNums[k];
      end if;
    end for;
  end func;

const proc: main is func
  local
    var integer: n is 0;
  begin
    for n range 1 to 20 do
      write(hamming(n) <& " ");
    end for;
    writeln;
    writeln(hamming(1691));
    writeln(hamming(1000000));
  end func;
```

{{out}}

```txt

1 2 3 4 5 6 8 9 10 12 15 16 18 20 24 25 27 30 32 36
2125764000
519312780448388736089589843750000000000000000000000000000000000000000000000000000000

```



## Sidef


```ruby
func ham_gen {
    var s = [[1], [1], [1]]
    var m = [2, 3, 5]
 
    func {
        var n = [s[0][0], s[1][0], s[2][0]].min
        { |i|
            s[i].shift if (s[i][0] == n)
            s[i].append(n * m[i])
        } << ^3
        return n
    }
}

var h = ham_gen()

var i = 20;
say i.of { h() }.join(' ')

{ h() } << (i+1 ..^ 1691)
say h()
```


{{out}}

```txt

1 2 3 4 5 6 8 9 10 12 15 16 18 20 24 25 27 30 32 36
2125764000

```



## Smalltalk

{{works with|GNU Smalltalk}}
This is a straightforward implementation of the pseudocode snippet found in the Python section. Smalltalk supports arbitrary-precision integers, but the implementation is too slow to try it with 1 million.

```smalltalk
Object subclass: Hammer [
  Hammer class >> hammingNumbers: howMany [
    |h i j k x2 x3 x5|
      h := OrderedCollection new.
      i := 0. j := 0. k := 0.
      h add: 1.
      x2 := 2. x3 := 2. x5 := 5.
      [ ( h size) < howMany ] whileTrue: [
        |m|
        m := { x2. x3. x5 } sort first.
        (( h indexOf: m ) = 0) ifTrue: [ h add: m ].
        ( x2 = (h last) ) ifTrue: [ i := i + 1. x2 := 2 * (h at: i) ].
        ( x3 = (h last) ) ifTrue: [ j := j + 1. x3 := 3 * (h at: j) ].
        ( x5 = (h last) ) ifTrue: [ k := k + 1. x5 := 5 * (h at: k) ].
      ].
      ^ h sort
  ]
].

(Hammer hammingNumbers: 20) displayNl.
(Hammer hammingNumbers: 1690) last displayNl.
```


{{works with|Pharo Smalltalk}}

```smalltalk

limit := 10 raisedToInteger: 84.
tape := Set new.

hammingProcess := [:newHamming|
	(newHamming <= limit)
		ifTrue:
			[| index |
			index := tape scanFor: newHamming.
			(tape array at: index)
				ifNil:
					[tape atNewIndex: index put: newHamming asSetElement.
					hammingProcess value: newHamming * 2.
					hammingProcess value: newHamming * 3.
					hammingProcess value: newHamming * 5]]].

hammingProcess value: 1.

sc := tape asSortedCollection.
sc first: 20. "a SortedCollection(1 2 3 4 5 6 8 9 10 12 15 16 18 20 24 25 27 30 32 36)"
sc at: 1691. "2125764000"
sc at: 1000000. "519312780448388736089589843750000000000000000000000000000000000000000000000000000000"

```



## SQL

This uses SQL99's "WITH RECURSIVE" (more like co-recursion) to build a table of Hamming numbers, then selects out the desired ones.  With sqlite it is very fast.  It doesn't try to get the millionth number because sqlite doesn't have bignums.


```sql

CREATE TEMPORARY TABLE factors(n INT);
INSERT INTO factors VALUES(2);
INSERT INTO factors VALUES(3);
INSERT INTO factors VALUES(5);

CREATE TEMPORARY TABLE hamming AS
    WITH RECURSIVE ham AS (
          SELECT 1 as h
          UNION
          SELECT h*n x FROM ham JOIN factors ORDER BY x
          LIMIT 1700
        )
    SELECT h FROM ham;

sqlite> SELECT h FROM hamming ORDER BY h LIMIT 20;
1
2
3
4
5
6
8
9
10
12
15
16
18
20
24
25
27
30
32
36
sqlite> SELECT h FROM hamming ORDER BY h LIMIT 1 OFFSET 1690;
2125764000

```



## Tcl

This uses coroutines to simplify the description of what's going on.
{{works with|Tcl|8.6}}

```tcl
package require Tcl 8.6

# Simple helper: Tcl-style list "map"
proc map {varName list script} {
    set l {}
    upvar 1 $varName v
    foreach v $list {lappend l [uplevel 1 $script]}
    return $l
}

# The core of a coroutine to compute the product of a hamming sequence.
#
# Tricky bit: we don't automatically advance to the next value, and instead
# wait to be told that the value has been consumed (i.e., is the result of
# the [yield] operation).
proc ham {key multiplier} {
    global hammingCache
    set i 0
    yield [info coroutine]
    # Cannot use [foreach]; that would take a snapshot of the list in
    # the hammingCache variable, so missing updates.
    while 1 {
	set n [expr {[lindex $hammingCache($key) $i] * $multiplier}]
	# If the number selected was ours, we advance to compute the next
	if {[yield $n] == $n} {
	    incr i
	}
    }
}

# This coroutine computes the hamming sequence given a list of multipliers.
# It uses the [ham] helper from above to generate indivdual multiplied
# sequences. The key into the cache is the list of multipliers.
#
# Note that it is advisable for the values to be all co-prime wrt each other.
proc hammingCore args {
    global hammingCache
    set hammingCache($args) 1
    set hammers [map x $args {coroutine ham$x,$args ham $args $x}]
    yield
    while 1 {
	set n [lindex $hammingCache($args) [incr i]-1]
	lappend hammingCache($args) \
	    [tcl::mathfunc::min {*}[map h $hammers {$h $n}]]
	yield $n
    }
}

# Assemble the pieces so as to compute the classic hamming sequence.
coroutine hamming hammingCore 2 3 5
# Print the first 20 values of the sequence
for {set i 1} {$i <= 20} {incr i} {
    puts [format "hamming\[%d\] = %d" $i [hamming]]
}
for {} {$i <= 1690} {incr i} {set h [hamming]}
puts "hamming{1690} = $h"
for {} {$i <= 1000000} {incr i} {set h [hamming]}
puts "hamming{1000000} = $h"
```

{{out}}

```txt

hamming{1} = 1
hamming{2} = 2
hamming{3} = 3
hamming{4} = 4
hamming{5} = 5
hamming{6} = 6
hamming{7} = 8
hamming{8} = 9
hamming{9} = 10
hamming{10} = 12
hamming{11} = 15
hamming{12} = 16
hamming{13} = 18
hamming{14} = 20
hamming{15} = 24
hamming{16} = 25
hamming{17} = 27
hamming{18} = 30
hamming{19} = 32
hamming{20} = 36
hamming{1690} = 2123366400
hamming{1000000} = 519312780448388736089589843750000000000000000000000000000000000000000000000000000000

```

A faster version can be built that also works on Tcl 8.5 (or earlier, if only small hamming numbers are being computed):

```tcl
variable hamming 1 hi2 0 hi3 0 hi5 0
proc hamming {n} {
    global hamming hi2 hi3 hi5
    set h2 [expr {[lindex $hamming $hi2]*2}]
    set h3 [expr {[lindex $hamming $hi3]*3}]
    set h5 [expr {[lindex $hamming $hi5]*5}]
    while {[llength $hamming] < $n} {
	lappend hamming [set h [expr {
	    $h2<$h3
	        ? $h2<$h5 ? $h2 : $h5
	        : $h3<$h5 ? $h3 : $h5
	}]]
	if {$h==$h2} {
	    set h2 [expr {[lindex $hamming [incr hi2]]*2}]
	}
	if {$h==$h3} {
	    set h3 [expr {[lindex $hamming [incr hi3]]*3}]
	}
	if {$h==$h5} {
	    set h5 [expr {[lindex $hamming [incr hi5]]*5}]
	}
    }
    return [lindex $hamming [expr {$n - 1}]]
}

# Print the first 20 values of the sequence
for {set i 1} {$i <= 20} {incr i} {
    puts [format "hamming\[%d\] = %d" $i [hamming $i]]
}
puts "hamming{1690} = [hamming 1690]"
puts "hamming{1691} = [hamming 1691]"
puts "hamming{1692} = [hamming 1692]"
puts "hamming{1693} = [hamming 1693]"
puts "hamming{1000000} = [hamming 1000000]"
```



## uBasic/4tH

uBasic's single array does not have the required size to calculate the 1691st number, let alone the millionth.
<lang>For H = 1 To 20
  Print "H("; H; ") = "; Func (_FnHamming(H))
Next

End

_FnHamming Param (1)
  @(0) = 1

  X = 2 : Y = 3 : Z = 5
  I = 0 : J = 0 : K = 0

  For N = 1 To a@ - 1
    M = X
    If M > Y Then M = Y
    If M > Z Then M = Z
    @(N) = M

    If M = X Then I = I + 1 : X = 2 * @(I)
    If M = Y Then J = J + 1 : Y = 3 * @(J)
    If M = Z Then K = K + 1 : Z = 5 * @(K)
  Next

Return (@(a@-1))
```

{{out}}

```txt
H(1) = 1
H(2) = 2
H(3) = 3
H(4) = 4
H(5) = 5
H(6) = 6
H(7) = 8
H(8) = 9
H(9) = 10
H(10) = 12
H(11) = 15
H(12) = 16
H(13) = 18
H(14) = 20
H(15) = 24
H(16) = 25
H(17) = 27
H(18) = 30
H(19) = 32
H(20) = 36

0 OK, 0:379
```



## UNIX Shell

{{works with|ksh93}}
Large numbers are not supported.

```bash
typeset -a hamming=(1)
function nextHamming {
    typeset -Sa q2 q3 q5
    integer h=${hamming[${#hamming[@]}-1]}
    q2+=( $(( h*2 )) )
    q3+=( $(( h*3 )) )
    q5+=( $(( h*5 )) )
    h=$( min3 ${q2[0]} ${q3[0]} ${q5[0]} )
    (( ${q2[0]} == h )) && ashift q2 >/dev/null
    (( ${q3[0]} == h )) && ashift q3 >/dev/null
    (( ${q5[0]} == h )) && ashift q5 >/dev/null
    hamming+=($h)
}

function ashift {
    nameref ary=$1
    print -- "${ary[0]}"
    ary=( "${ary[@]:1}" )
}

function min3 {
    if (( $1 < $2 )); then
        (( $1 < $3 )) && print -- $1 || print -- $3
    else
        (( $2 < $3 )) && print -- $2 || print -- $3
    fi
}

for ((i=1; i<=20; i++)); do
    nextHamming
    printf "%d\t%d\n" $i ${hamming[i-1]}
done
for ((; i<=1690; i++)); do nextHamming; done
nextHamming
printf "%d\t%d\n" $i ${hamming[i-1]}
print "elapsed: $SECONDS"
```


{{out}}

```txt

1	1
2	2
3	3
4	4
5	5
6	6
7	8
8	9
9	10
10	12
11	15
12	16
13	18
14	20
15	24
16	25
17	27
18	30
19	32
20	36
1690	2125764000
elapsed: 0.568
```



## Ursala

Smooth is defined as a second order function taking a list of
primes and returning a function that takes a natural number <math>n</math> to the <math>n</math>-th smooth
number with respect to them. An elegant but inefficient formulation based on the J solution is the
following.

```Ursala
#import std
#import nat

smooth"p" "n" = ~&z take/"n" nleq-< (rep(length "n") ^Ts/~& product*K0/"p") <1>
```

This test program

```Ursala
main = smooth<2,3,5>* nrange(1,20)
```

yields this list of the first 20 Hamming numbers.

```txt

<1,2,3,4,5,6,8,9,10,12,15,16,18,20,24,25,27,30,32,36>

```

Although all calculations are performed using unlimited precision, the version
above is impractical for large numbers. A more hardcore approach is the following.

```Ursala
#import std
#import nat

smooth"p" "n" =

~&H\"p" *-<1>; @NiXS ~&/(1,1); ~&ll~="n"->lr -+
   ^\~&rlPrrn2rrm2Zlrrmz3EZYrrm2lNCTrrm2QAX*rhlPNhrnmtPA2XtCD ~&lrPrhl2E?/~&l ^|/successor@l ~&hl,
   ^|/~& nleq-<&l+ * ^\~&r ~&l|| product@rnmhPX+-

#cast %nL

main = smooth<2,3,5>* nrange(1,20)--<1691,1000000>
```

{{out}}
The great majority of time is spent calculating the millionth Hamming number.

```txt
<
   1,
   2,
   3,
   4,
   5,
   6,
   8,
   9,
   10,
   12,
   15,
   16,
   18,
   20,
   24,
   25,
   27,
   30,
   32,
   36,
   2125764000,
   519312780448388736089589843750000000000000000000000000000000000000000000000000000000>
```



## VBA


```vb
'RosettaCode Hamming numbers
'This is a well known hard problem in number theory:
'counting the number of lattice points in a
'n-dimensional tetrahedron, here n=3.
Public a As Double, b As Double, c As Double, d As Double
Public p As Double, q As Double, r As Double
Public cnt() As Integer 'stores the number of lattice points indexed on the exponents of 3 and 5
Public hn(2) As Integer 'stores the exponents of 2, 3 and 5
Public Declare Function GetTickCount Lib "kernel32.dll" () As Long
Private Function log10(x As Double) As Double
    log10 = WorksheetFunction.log10(x)
End Function
Private Function pow(x As Variant, y As Variant) As Double
    pow = WorksheetFunction.Power(x, y)
End Function
Private Sub init(N As Long)
    'Computes a, b and c as the vertices
    '(a,0,0), (0,b,0), (0,0,c) of a tetrahedron
    'with apex (0,0,0) and volume N
    'volume N=a*b*c/6
    Dim k As Double
    k = log10(2) * log10(3) * log10(5) * 6 * N
    k = pow(k, 1 / 3)
    a = k / log10(2)
    b = k / log10(3)
    c = k / log10(5)
    p = -b * c
    q = -a * c
    r = -a * b
End Sub
Private Function x_given_y_z(y As Integer, z As Integer) As Double
    x_given_y_z = -(q * y + r * z + a * b * c) / p
End Function
Private Function cmp(i As Integer, j As Integer, k As Integer, gn() As Integer) As Boolean
    cmp = (i * log10(2) + j * log10(3) + k * log10(5)) > (gn(0) * log10(2) + gn(1) * log10(3) + gn(2) * log10(5))
End Function
Private Function count(N As Long, step As Integer) As Long
    'Loop over y and z, compute x and
    'count number of lattice points within tetrahedron.
    'Step 1 is indirectly called by find_seed to calibrate the plane through A, B and C
    'Step 2 fills the matrix cnt with the number of lattice points given the exponents of 3 and 5
    'Step 3 the plane is lowered marginally so one or two candidates stick out
    Dim M As Long, j As Integer, k As Integer
    If step = 2 Then ReDim cnt(0 To Int(b) + 1, 0 To Int(c) + 1)
    M = 0: j = 0: k = 0
    Do While -c * j - b * k + b * c > 0
        Do While -c * j - b * k + b * c > 0
            Select Case step
                Case 1: M = M + Int(x_given_y_z(j, k))
                Case 2
                    cnt(j, k) = Int(x_given_y_z(j, k))
                Case 3
                    If Int(x_given_y_z(j, k)) < cnt(j, k) Then
                        'This is a candidate, and ...
                        If cmp(cnt(j, k), j, k, hn) Then
                            'it is bigger dan what is already in hn
                            hn(0) = cnt(j, k)
                            hn(1) = j
                            hn(2) = k
                        End If
                    End If
            End Select
            k = k + 1
        Loop
        k = 0
        j = j + 1
    Loop
    count = M
End Function
Private Sub list_upto(ByVal N As Integer)
    Dim count As Integer
    count = 1
    Dim hn As Integer
    hn = 1
    Do While count < N
        k = hn
        Do While k Mod 2 = 0
            k = k / 2
        Loop
        Do While k Mod 3 = 0
            k = k / 3
        Loop
        Do While k Mod 5 = 0
            k = k / 5
        Loop
        If k = 1 Then
            Debug.Print hn; " ";
            count = count + 1
        End If
        hn = hn + 1
    Loop
    Debug.Print
End Sub
Private Function find_seed(N As Long, step As Integer) As Long
    Dim initial As Long, total As Long
    initial = N
    Do 'a simple iterative goal search, takes a handful iterations only
        init initial
        total = count(initial, step)
        initial = initial + N - total
    Loop Until total = N
    find_seed = initial
End Function
Private Sub find_hn(N As Long)
    Dim fs As Long, err As Long
    'Step 1: find fs such that the number of lattice points is exactly N
    fs = find_seed(N, 1)
    'Step 2: fill the matrix cnt
    init fs
    err = count(fs, 2)
    'Step 3: lower the plane by diminishing fs, the candidates for
    'the Nth Hamming number will stick out and be recorded in hn
    init fs - 1
    err = count(fs - 1, 3)
    Debug.Print "2^" & hn(0) - 1; " * 3^" & hn(1); " * 5^" & hn(2); "=";
    If N < 1692 Then
        'The task set a limit on the number size
        Debug.Print pow(2, hn(0) - 1) * pow(3, hn(1)) * pow(5, hn(2))
    Else
        Debug.Print
        If N <= 1000000 Then
            'The big Hamming Number will end in a lot of zeroes. The common exponents of 2 and 5
            'are split off to be printed separately.
            If hn(0) - 1 < hn(2) Then
                'Conversion to Decimal datatype with CDec allows to print numbers upto 10^28
                Debug.Print CDec(pow(3, hn(1))) * CDec(pow(5, hn(2) - hn(0) + 1)) & String$(hn(0) - 1, "0")
            Else
                Debug.Print CDec(pow(2, hn(0) - 1 - hn(2))) * CDec(pow(3, hn(1))) & String$(hn(2), "0")
            End If
        End If
    End If
End Sub
Public Sub main()
    Dim start_time As Long, finis_time As Long
    start_time = GetTickCount
    Debug.Print "The first twenty Hamming numbers are:"
    list_upto 20
    Debug.Print "Hamming number 1691 is: ";
    find_hn 1691
    Debug.Print "Hamming number 1000000 is: ";
    find_hn 1000000
    finis_time = GetTickCount
    Debug.Print "Execution time"; (finis_time - start_time); " milliseconds"
End Sub
```
{{out}}

```txt
The first twenty Hamming numbers are:
 1   2   3   4   5   6   8   9   10   12   15   16   18   20   24   25   27   30   32
Hamming number 1691 is: 2^5 * 3^12 * 5^3= 2125764000
Hamming number 1000000 is: 2^55 * 3^47 * 5^64=
519312780448388671875000000000000000000000000000000000000000000000000000000000000000
Execution time 79  milliseconds

```


## VBScript

{{trans|BBC BASIC}}

```vb

For h = 1 To 20
	WScript.StdOut.Write "H(" & h & ") = " & Hamming(h)
	WScript.StdOut.WriteLine
Next
WScript.StdOut.Write "H(" & 1691 & ") = " & Hamming(1691)
WScript.StdOut.WriteLine

Function Hamming(l)
	Dim h() : Redim h(l) : h(0) = 1
	i = 0 : j = 0 : k = 0
	x2 = 2 : x3 = 3 : x5 = 5
	For n = 1 To l-1
		m = x2
		If m > x3 Then m = x3 End If
		If m > x5 Then m = x5 End If
		h(n) = m
		If m = x2 Then i = i + 1 : x2 = 2 * h(i) End If
		If m = x3 Then j = j + 1 : x3 = 3 * h(j) End If
		If m = x5 Then k = k + 1 : x5 = 5 * h(k) End If
	Next
	Hamming = h(l-1)
End Function

```


{{Out}}

```txt

H(1) = 1
H(2) = 2
H(3) = 3
H(4) = 4
H(5) = 5
H(6) = 6
H(7) = 8
H(8) = 9
H(9) = 10
H(10) = 12
H(11) = 15
H(12) = 16
H(13) = 18
H(14) = 20
H(15) = 24
H(16) = 25
H(17) = 27
H(18) = 30
H(19) = 32
H(20) = 36
H(1691) = 2125764000

```



## Yabasic

{{trans|Run BASIC}}

```Yabasic
dim h(1000000)
for i =1 to 20
    print hamming(i)," ";
next i

print
print "Hamming List First(1691)   = ",hamming(1691)
end

sub hamming(limit)
    local x2,x3,x5,i,j,k,n

    h(0) =1
    x2 = 2: x3 = 3: x5 =5
    i  = 0: j  = 0: k  =0
    for n =1 to limit
        h(n)  = min(x2, min(x3, x5))
        if x2 = h(n) then i = i +1: x2 =2 *h(i):end if
        if x3 = h(n) then j = j +1: x3 =3 *h(j):end if
        if x5 = h(n) then k = k +1: x5 =5 *h(k):end if
    next n
    return h(limit -1)
end sub
```



## zkl


```zkl
var BN=Import("zklBigNum");  // only needed for large N
fcn hamming(N){
   h:=List.createLong(N+1); (0).pump(N+1,h.write,Void); // fill list with stuff
   h[0]=1;
#if 1  // regular (64 bit) ints
   x2:=2; x3:=3; x5:=5; i:=j:=k:=0;
#else  // big ints
   x2:=BN(2); x3:=BN(3); x5:=BN(5); i:=j:=k:=0;
#endif
   foreach n in ([1..N]){
      z:=(x2<x3) and x2 or x3; z=(z<x5) and z or x5; h[n]=z;
      if (h[n] == x2) { x2 = h[i+=1]*2 }
      if (h[n] == x3) { x3 = h[j+=1]*3 }
      if (h[n] == x5) { x5 = h[k+=1]*5 }
   }
   return(h[N-1])
}
[1..20].apply(hamming).println();
hamming(1691).println();
```

{{out}}

```txt

L(1,2,3,4,5,6,8,9,10,12,15,16,18,20,24,25,27,30,32,36)
2125764000

```

While the other algorithms save [lots of] space, run time still sucks when n > 100,000 so memory usage might as well too. Changing the #if 0 to 1 will use Big Int and lots of space.
{{out}}

```txt

hamming(0d1_000_000).println();
519312780448388736089589843750000000000000000000000000000000000000000000000000000000

```


### Direct calculation through triples enumeration

OK, I was wrong, calculating the nth Hamming number can be fast and efficient.
{{trans|Haskell}} as direct a translation as I can, except using a nested for loop instead of list comprehension (which makes it easier to keep the count).

```zkl
#-- directly find n-th Hamming number, in ~ O(n^{2/3}) time
#-- by Will Ness, based on "top band" idea by Louis Klauder, from DDJ discussion
#--   http://drdobbs.com/blogs/architecture-and-design/228700538

var BN=Import("zklBigNum");
var lg3 = (3.0).log()/(2.0).log(), lg5 = (5.0).log()/(2.0).log();
fcn logval(i,j,k){ lg5*k + lg3*j + i }
fcn trival(i,j,k){ BN(2).pow(i) * BN(3).pow(j) * BN(5).pow(k) }
fcn estval(n){ (6.0*lg3*lg5*n).pow(1.0/3) } #-- estimated logval, base 2
fcn rngval(n){
   if(n > 500000) return(2.4496 , 0.0076);	#-- empirical estimation
   if(n > 50000)  return(2.4424 , 0.0146);	#--   correction, base 2
   if(n > 500)	  return(2.3948 , 0.0723);	#--     (dist,width)
   if(n > 1)	  return(2.2506 , 0.2887);	#-- around (log $ sqrt 30),
		  return(2.2506 , 0.5771);	#--   says WP
}

fcn nthHam(n){ // -> (Double, (Int, Int, Int))  #-- n: 1-based: 1,2,3...
  d,w := rngval(n);				#-- correction dist, width
  hi  := estval(n.toFloat()) - d;		#--   hi > logval > hi-w
  c,b := band(hi,w);				#-- total count, the band
  s   := b.sort(fcn(a,b){ a[0]>b[0] });		#-- sorted decreasing, result
  m   := c - n;					#-- m 0-based from top
  nb  := b.len();				#-- |band|
  res := s[m];					#-- result

  if(w >= 1) throw(Exception.Generic("Breach of contract: (w < 1):  " + w));
  if(m <  0) throw(Exception.Generic("Not enough triples generated: " +c+n));
  if(m >= nb)throw(Exception.Generic("Generated band is too narrow: " +m+nb));
  return(res);
}

fcn band(hi,w){ //--> #-- total count, the band
   b := Sink(List); cnt := 0;
   foreach k in ([0 .. (hi/lg5).floor()]){        p := lg5*k;
      foreach j in ([0 .. ((hi-p)/lg3).floor()]){ q := lg3*j + p;
         i,frac := (hi-q).modf(); r := hi-frac;		#-- r = i + q
	 cnt+=(i+1);					#-- total count
	 if(frac<w) b.write(T(r,T(i,j,k)));		#-- store it, if inside band
      }
   }
   return(cnt,b.close());
}
```

<lang>fcn printHam(n){
   r,t:=nthHam(n); i,j,k:=t; h:=trival(i,j,k);
   println("Hamming(%,d)-->2^%d * 3^%d * 5^%d-->\n%s".fmt(n,i,j,k,h));
}

printHam(1691);            //(5,12,3), 10 digits
printHam(0d1_000_000);     //(55,47,64), 84 digits
printHam(0d10_000_000);    //(80,92,162), 182 digits, 80 zeros at end
printHam(0d1_000_000_000); //(1334,335,404), 845 digits
```

{{out}}

```txt

Hamming(1,691)-->2^5 * 3^12 * 5^3-->
2125764000
Hamming(1,000,000)-->2^55 * 3^47 * 5^64-->
519312780448388736089589843750000000000000000000000000000000000000000000000000000000
Hamming(10,000,000)-->2^80 * 3^92 * 5^162-->
162441050638304318232392153117595750351085388205966408633356724833252116013682098127901554107666015625 <80 zeros>
Hamming(1,000,000,000)-->2^1334 * 3^335 * 5^404-->
621607575556524486163081633287207200394705651908965270659163240.......

```



## ZX Spectrum Basic

{{trans|BBC_BASIC}}

```zxbasic
10 FOR h=1 TO 20: GO SUB 1000: NEXT h
20 LET h=1691: GO SUB 1000
30 STOP
1000 REM Hamming
1010 DIM a(h)
1030 LET a(1)=1: LET x2=2: LET x3=3: LET x5=5: LET i=1: LET j=1: LET k=1
1040 FOR n=2 TO h
1050 LET m=x2
1060 IF m>x3 THEN LET m=x3
1070 IF m>x5 THEN LET m=x5
1080 LET a(n)=m
1090 IF m=x2 THEN LET i=i+1: LET x2=2*a(i)
1100 IF m=x3 THEN LET j=j+1: LET x3=3*a(j)
1110 IF m=x5 THEN LET k=k+1: LET x5=5*a(k)
1120 NEXT n
1130 PRINT "H(";h;")= ";a(h)
1140 RETURN
```

