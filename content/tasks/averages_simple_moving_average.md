+++
title = "Averages/Simple moving average"
description = ""
date = 2019-10-06T05:04:22Z
aliases = []
[extra]
id = 4400
[taxonomies]
categories = ["Probability and statistics", "task"]
tags = []
+++

## Task

Computing the [[wp:Moving_average#Simple_moving_average|simple moving average]] of a series of numbers.

{{task heading}}

Create a [[wp:Stateful|stateful]] function/class/instance that takes a period and returns a routine that takes a number as argument and returns a simple moving average of its arguments so far.

{{task heading|Description}}

A simple moving average is a method for computing an average of a stream of numbers by only averaging the last   P   numbers from the stream,   where   P   is known as the period.

It can be implemented by calling an initialing routine with   P   as its argument,   I(P),   which should then return a routine that when called with individual, successive members of a stream of numbers, computes the mean of (up to), the last   P   of them, lets call this   SMA().

The word   ''stateful''   in the task description refers to the need for   SMA()   to remember certain information between calls to it:
*   The period,   P
*   An ordered container of at least the last   P   numbers from each of its individual calls.



''Stateful''    also means that successive calls to   I(),   the initializer,   should return separate routines that do   ''not''   share saved state so they could be used on two independent streams of data.

Pseudo-code for an implementation of   SMA   is:

```txt

function SMA(number: N):
    stateful integer: P
    stateful list:    stream
    number:           average

    stream.append_last(N)
    if stream.length() > P:
        # Only average the last P elements of the stream
        stream.delete_first()
    if stream.length() == 0:
        average = 0
    else:
        average = sum( stream.values() ) / stream.length()
    return average

```


{{task heading|See also}}

{{Related tasks/Statistical measures}}

<hr>


## 11l

{{trans|D}}

```11l
T SMA
   [Float] data
   sum = 0.0
   index = 0
   n_filled = 0
   Int period

   F (period)
      .period = period
      .data = [0.0] * period

   F add(v)
      .sum += v - .data[.index]
      .data[.index] = v
      .index = (.index + 1) % .period
      .n_filled = min(.period, .n_filled + 1)
      R .sum / .n_filled

V sma3 = SMA(3)
V sma5 = SMA(5)

L(e) [1, 2, 3, 4, 5, 5, 4, 3, 2, 1]
   print(‘Added #., sma(3) = #.6, sma(5) = #.6’.format(e, sma3.add(e), sma5.add(e)))
```

{{out}}

```txt

Added 1, sma(3) = 1.000000, sma(5) = 1.000000
Added 2, sma(3) = 1.500000, sma(5) = 1.500000
Added 3, sma(3) = 2.000000, sma(5) = 2.000000
Added 4, sma(3) = 3.000000, sma(5) = 2.500000
Added 5, sma(3) = 4.000000, sma(5) = 3.000000
Added 5, sma(3) = 4.666667, sma(5) = 3.800000
Added 4, sma(3) = 4.666667, sma(5) = 4.200000
Added 3, sma(3) = 4.000000, sma(5) = 4.200000
Added 2, sma(3) = 3.000000, sma(5) = 3.800000
Added 1, sma(3) = 2.000000, sma(5) = 3.000000

```



## 360 Assembly

{{trans|PL/I}}

```360asm
*        Averages/Simple moving average  26/08/2015
AVGSMA   CSECT
         USING  AVGSMA,R12
         LR     R12,R15
         ST     R14,SAVER14
         ZAP    II,=P'0'           ii=0
         LA     R7,1
         LH     R3,NA
         SRA    R3,1               na/2
LOOPA    CR     R7,R3              do i=1 to na/2
         BH     ELOOPA
         AP     II,=P'1000'        ii=ii+1000
         LR     R1,R7              i
         MH     R1,=H'6'
         LA     R4,A-6(R1)
         MVC    0(6,R4),II         a(i)=ii
         LH     R1,NA              na
         SR     R1,R7              -i
         MH     R1,=H'6'
         LA     R4,A(R1)
         MVC    0(6,R4),II         a(na+1-i)=ii
         LA     R7,1(R7)
         B      LOOPA
ELOOPA   XPRNT  =CL30' n     sma3        sma5       ',30
         XPRNT  =CL30' ----- ----------- -----------',30
         LA     R7,1               i=1
LOOP     CH     R7,NA              do i=1 to na
         BH     RETURN
         STH    R7,N               n=i
         XDECO  R7,C               i
         MVC    BUF+1(5),C+7
         MVC    P,=H'3'            p=3
         BAL    R14,SMA
         MVC    C(13),EDMASK
         ED     C(13),SS           sma(3,i)
         MVC    BUF+7(11),C+2
         MVC    P,=H'5'            p=5
         BAL    R14,SMA
         MVC    C(13),EDMASK
         ED     C(13),SS           sma(5,i)
         MVC    BUF+19(11),C+2
         XPRNT  BUF,30             output i,sma3,sma5
         LA     R7,1(R7)
         B      LOOP
*        *****  sub sma(p,n) returns(PL6)
SMA      LH     R5,N
         SH     R5,P
         A      R5,=F'1'           ia=n-p+1
         C      R5,=F'1'
         BH     OKIA
         LA     R5,1               ia=1
OKIA     LH     R6,NA              ib=na
         CH     R6,N
         BL     OKIB
         LH     R6,N               ib=n
OKIB     ZAP    II,=P'0'           ii=0
         ZAP    SS,=P'0'           ss=0
         LR     R3,R5              k=ia
LOOPK    CR     R3,R6              do k=ia to ib
         BH     ELOOPK
         AP     II,=P'1'           ii=ii+1
         LR     R1,R3
         MH     R1,=H'6'
         LA     R4,A-6(R1)
         MVC    C(6),0(R4)         ss=ss+a(k)
         AP     SS,C(6)
         LA     R3,1(R3)
         B      LOOPK
ELOOPK   ZAP    C,SS
         DP     C,II
         ZAP    SS,C(10)           ss=ss/ii
         BR     R14
RETURN   L      R14,SAVER14        restore caller address
         XR     R15,R15
         BR     R14
SAVER14  DS     F
NN       EQU    10
NA       DC     AL2(NN)
A        DS     (NN)PL6
II       DS     PL6
SS       DS     PL6
P        DS     H
N        DS     H
C        DS     CL16
BUF      DC     CL30'                              '  buffer
EDMASK   DC     X'4020202020202021204B202020'  CL13
         YREGS
         END    AVGSMA
```

{{out}}

```txt

 n     sma3        sma5
 ----- ----------- -----------
     1       1.000       1.000
     2       1.500       1.500
     3       2.000       2.000
     4       3.000       2.500
     5       4.000       3.000
     6       4.666       3.800
     7       4.666       4.200
     8       4.000       4.200
     9       3.000       3.800
    10       2.000       3.000

```



## Ada

{{works with|Ada 2005}}

moving.ads:

```Ada
generic
   Max_Elements : Positive;
   type Number is digits <>;
package Moving is
   procedure Add_Number (N : Number);
   function Moving_Average (N : Number) return Number;
   function Get_Average return Number;
end Moving;
```


moving.adb:

```Ada
with Ada.Containers.Vectors;

package body Moving is
   use Ada.Containers;

   package Number_Vectors is new Ada.Containers.Vectors
     (Element_Type => Number,
      Index_Type   => Natural);

   Current_List : Number_Vectors.Vector := Number_Vectors.Empty_Vector;

   procedure Add_Number (N : Number) is
   begin
      if Natural (Current_List.Length) >= Max_Elements then
         Current_List.Delete_First;
      end if;
      Current_List.Append (N);
   end Add_Number;

   function Get_Average return Number is
      Average : Number := 0.0;
      procedure Sum (Position : Number_Vectors.Cursor) is
      begin
         Average := Average + Number_Vectors.Element (Position);
      end Sum;
   begin
      Current_List.Iterate (Sum'Access);
      if Current_List.Length > 1 then
         Average := Average / Number (Current_List.Length);
      end if;
      return Average;
   end Get_Average;

   function Moving_Average (N : Number) return Number is
   begin
      Add_Number (N);
      return Get_Average;
   end Moving_Average;

end Moving;
```


main.adb:

```Ada
with Ada.Text_IO;
with Moving;
procedure Main is
   package Three_Average is new Moving (Max_Elements => 3, Number => Float);
   package Five_Average is new Moving (Max_Elements => 5, Number => Float);
begin
   for I in 1 .. 5 loop
      Ada.Text_IO.Put_Line ("Inserting" & Integer'Image (I) &
        " into max-3: " & Float'Image (Three_Average.Moving_Average (Float (I))));
      Ada.Text_IO.Put_Line ("Inserting" & Integer'Image (I) &
        " into max-5: " & Float'Image (Five_Average.Moving_Average (Float (I))));
   end loop;
   for I in reverse 1 .. 5 loop
      Ada.Text_IO.Put_Line ("Inserting" & Integer'Image (I) &
        " into max-3: " & Float'Image (Three_Average.Moving_Average (Float (I))));
      Ada.Text_IO.Put_Line ("Inserting" & Integer'Image (I) &
        " into max-5: " & Float'Image (Five_Average.Moving_Average (Float (I))));
   end loop;
end Main;
```


{{out}}

```txt
Inserting 1 into max-3:  1.00000E+00
Inserting 1 into max-5:  1.00000E+00
Inserting 2 into max-3:  1.50000E+00
Inserting 2 into max-5:  1.50000E+00
Inserting 3 into max-3:  2.00000E+00
Inserting 3 into max-5:  2.00000E+00
Inserting 4 into max-3:  3.00000E+00
Inserting 4 into max-5:  2.50000E+00
Inserting 5 into max-3:  4.00000E+00
Inserting 5 into max-5:  3.00000E+00
Inserting 5 into max-3:  4.66667E+00
Inserting 5 into max-5:  3.80000E+00
Inserting 4 into max-3:  4.66667E+00
Inserting 4 into max-5:  4.20000E+00
Inserting 3 into max-3:  4.00000E+00
Inserting 3 into max-5:  4.20000E+00
Inserting 2 into max-3:  3.00000E+00
Inserting 2 into max-5:  3.80000E+00
Inserting 1 into max-3:  2.00000E+00
Inserting 1 into max-5:  3.00000E+00
```



## ALGOL 68

{{trans|C}}

{{works with|ALGOL 68|Standard - no extensions to language used}}

{{works with|ALGOL 68G|Any - tested with release [http://sourceforge.net/projects/algol68/files/algol68g/algol68g-1.18.0/algol68g-1.18.0-9h.tiny.el5.centos.fc11.i386.rpm/download 1.18.0-9h.tiny]}}

<!-- {{works with|ELLA ALGOL 68|Any (with appropriate job cards) - tested with release [http://sourceforge.net/projects/algol68/files/algol68toc/algol68toc-1.8.8d/algol68toc-1.8-8d.fc9.i386.rpm/download 1.8.8d.fc9.i386]}} -->
Note: This following code is a direct translation of the [[Average/Simple_moving_average#C|C]] code sample.  It mimics C's var_list implementation, and so it probably isn't the most natural way of dong this actual task in '''ALGOL 68'''.

```Algol68
MODE SMAOBJ  = STRUCT(
  LONG REAL sma,
  LONG REAL sum,
  INT period,
  REF[]LONG REAL values,
  INT lv
);

MODE SMARESULT = UNION (
  REF SMAOBJ # handle #,
  LONG REAL # sma #,
  REF[]LONG REAL # values #
);

MODE SMANEW = INT,
     SMAFREE = STRUCT(REF SMAOBJ free obj),
     SMAVALUES = STRUCT(REF SMAOBJ values obj),
     SMAADD = STRUCT(REF SMAOBJ add obj, LONG REAL v),
     SMAMEAN = STRUCT(REF SMAOBJ mean obj, REF[]LONG REAL v);

MODE ACTION = UNION ( SMANEW, SMAFREE, SMAVALUES, SMAADD, SMAMEAN );

PROC sma = ([]ACTION action)SMARESULT:
(
  SMARESULT result;
  REF SMAOBJ obj;
  LONG REAL v;

  FOR i FROM LWB action TO UPB action DO
    CASE action[i] IN
    (SMANEW period):( # args: INT period #
       HEAP SMAOBJ handle;
       sma OF handle := 0.0;
       period OF handle := period;
       values OF handle := HEAP [period OF handle]LONG REAL;
       lv OF handle := 0;
       sum OF handle := 0.0;
       result := handle
    ),
    (SMAFREE args):( # args: REF SMAOBJ free obj #
       free obj OF args := REF SMAOBJ(NIL) # let the garbage collector do it's job #
    ),
    (SMAVALUES args):( # args: REF SMAOBJ values obj #
       result := values OF values obj OF args
    ),
    (SMAMEAN args):( # args: REF SMAOBJ mean obj #
       result := sma OF mean obj OF args
    ),
    (SMAADD args):( # args: REF SMAOBJ add obj, LONG REAL v #
       obj := add obj OF args;
       v := v OF args;
       IF lv OF obj < period OF obj THEN
         (values OF obj)[lv OF obj+:=1] := v;
         sum OF obj +:= v;
         sma OF obj := sum OF obj / lv OF obj
       ELSE
         sum OF obj -:= (values OF obj)[ 1+ lv OF obj MOD period OF obj];
         sum OF obj +:= v;
         sma OF obj := sum OF obj / period OF obj;
         (values OF obj)[ 1+ lv OF obj  MOD  period OF obj ] := v; lv OF obj+:=1
       FI;
       result := sma OF obj
    )
    OUT
      SKIP
    ESAC
  OD;
  result
);

[]LONG REAL v = ( 1, 2, 3, 4, 5, 5, 4, 3, 2, 1 );

main: (
  INT i;

  REF SMAOBJ h3 := ( sma(SMANEW(3)) | (REF SMAOBJ obj):obj );
  REF SMAOBJ h5 := ( sma(SMANEW(5)) | (REF SMAOBJ obj):obj );

  FOR i FROM LWB v TO UPB v DO
    printf(($"next number "g(0,6)", SMA_3 = "g(0,6)", SMA_5 = "g(0,6)l$,
           v[i], (sma(SMAADD(h3, v[i]))|(LONG REAL r):r), ( sma(SMAADD(h5, v[i])) | (LONG REAL r):r )
    ))
  OD#;

  sma(SMAFREE(h3));
  sma(SMAFREE(h5))
#
)
```

{{out}}

```txt

next number 1.000000, SMA_3 = 1.000000, SMA_5 = 1.000000
next number 2.000000, SMA_3 = 1.500000, SMA_5 = 1.500000
next number 3.000000, SMA_3 = 2.000000, SMA_5 = 2.000000
next number 4.000000, SMA_3 = 3.000000, SMA_5 = 2.500000
next number 5.000000, SMA_3 = 4.000000, SMA_5 = 3.000000
next number 5.000000, SMA_3 = 4.666667, SMA_5 = 3.800000
next number 4.000000, SMA_3 = 4.666667, SMA_5 = 4.200000
next number 3.000000, SMA_3 = 4.000000, SMA_5 = 4.200000
next number 2.000000, SMA_3 = 3.000000, SMA_5 = 3.800000
next number 1.000000, SMA_3 = 2.000000, SMA_5 = 3.000000

```



## AutoHotkey

ahk forum: [http://www.autohotkey.com/forum/post-276695.html#276695 discussion]
For Integers:

```AutoHotkey
MsgBox % MovingAverage(5,3)  ; 5, averaging length <- 3
MsgBox % MovingAverage(1)    ; 3
MsgBox % MovingAverage(-3)   ; 1
MsgBox % MovingAverage(8)    ; 2
MsgBox % MovingAverage(7)    ; 4

MovingAverage(x,len="") {    ; for integers (faster)
  Static
  Static sum:=0, n:=0, m:=10 ; default averaging length = 10
  If (len>"")                ; non-blank 2nd parameter: set length, reset
     sum := n := i := 0, m := len
  If (n < m)                 ; until the buffer is not full
     sum += x, n++           ;   keep summing data
  Else                       ; when buffer is full
     sum += x-v%i%           ;   add new, subtract oldest
  v%i% := x, i := mod(i+1,m) ; remember last m inputs, cycle insertion point
  Return sum/n
}
```

For floating point numbers:

```AutoHotkey
MovingAverage(x,len="") {    ; for floating point numbers
  Static
  Static n:=0, m:=10         ; default averaging length = 10
  If (len>"")                ; non-blank 2nd parameter: set length, reset
     n := i := 0, m := len
  n += n < m, sum := 0
  v%i% := x, i := mod(i+1,m) ; remember last m inputs, cycle insertion point
  Loop %n%                   ; recompute sum to avoid error accumulation
     j := A_Index-1, sum += v%j%
  Return sum/n
}
```



## AWK


```awk
#!/usr/bin/awk -f
# Moving average over the first column of a data file
BEGIN {
    P = 5;
}

{
    x = $1;
    i = NR % P;
    MA += (x - Z[i]) / P;
    Z[i] = x;
    print MA;
}
```



## BBC BASIC

{{works with|BBC BASIC for Windows}}

```bbcbasic
      MAXPERIOD = 10
      FOR n = 1 TO 5
        PRINT "Number = ";n TAB(12) " SMA3 = ";FNsma(n,3) TAB(30) " SMA5 = ";FNsma(n,5)
      NEXT
      FOR n = 5 TO 1 STEP -1
        PRINT "Number = ";n TAB(12) " SMA3 = ";FNsma(n,3) TAB(30) " SMA5 = ";FNsma(n,5)
      NEXT
      END

      DEF FNsma(number, period%)
      PRIVATE nums(), accum(), index%(), window%()
      DIM nums(MAXPERIOD,MAXPERIOD), accum(MAXPERIOD)
      DIM index%(MAXPERIOD), window%(MAXPERIOD)
      accum(period%) += number - nums(period%,index%(period%))
      nums(period%,index%(period%)) = number
      index%(period%) = (index%(period%) + 1) MOD period%
      IF window%(period%)<period% window%(period%) += 1
      = accum(period%) / window%(period%)
```

{{out}}

```txt

Number = 1   SMA3 = 1          SMA5 = 1
Number = 2   SMA3 = 1.5        SMA5 = 1.5
Number = 3   SMA3 = 2          SMA5 = 2
Number = 4   SMA3 = 3          SMA5 = 2.5
Number = 5   SMA3 = 4          SMA5 = 3
Number = 5   SMA3 = 4.66666667 SMA5 = 3.8
Number = 4   SMA3 = 4.66666667 SMA5 = 4.2
Number = 3   SMA3 = 4          SMA5 = 4.2
Number = 2   SMA3 = 3          SMA5 = 3.8
Number = 1   SMA3 = 2          SMA5 = 3

```



## Bracmat


```bracmat
( ( I
  =   buffer
    .   (new$=):?freshEmptyBuffer
      &
        ' ( buffer avg
          .   ( avg
              =   L S n
                .   0:?L:?S
                  &   whl
                    ' ( !arg:%?n ?arg
                      & !n+!S:?S
                      & 1+!L:?L
                      )
                  & (!L:0&0|!S*!L^-1)
              )
            & (buffer=$freshEmptyBuffer)
            & !arg !(buffer.):?(buffer.)
            & ( !(buffer.):?(buffer.) [($arg) ?
              |
              )
            & avg$!(buffer.)
          )
  )
& ( pad
  =   len w
    .   @(!arg:? [?len)
      & @("     ":? [!len ?w)
      & !w !arg
  )
& I$3:(=?sma3)
& I$5:(=?sma5)
& 1 2 3 4 5 5 4 3 2 1:?K
&   whl
  ' ( !K:%?k ?K
    &   out
      $ (str$(!k " - sma3:" pad$(sma3$!k) "  sma5:" pad$(sma5$!k)))
    )
);
```

{{out}}

```txt
1 - sma3:    1  sma5:    1
2 - sma3:  3/2  sma5:  3/2
3 - sma3:    2  sma5:    2
4 - sma3:    3  sma5:  5/2
5 - sma3:    4  sma5:    3
5 - sma3: 14/3  sma5: 19/5
4 - sma3: 14/3  sma5: 21/5
3 - sma3:    4  sma5: 21/5
2 - sma3:    3  sma5: 19/5
1 - sma3:    2  sma5:    3
```



## Brat

Object version

```brat

SMA = object.new

SMA.init = { period |
  my.period = period
  my.list = []
  my.average = 0
}

SMA.prototype.add = { num |
  true? my.list.length >= my.period
    { my.list.deq }

  my.list << num
  my.average = my.list.reduce(:+) / my.list.length
}

sma3 = SMA.new 3
sma5 = SMA.new 5
[1, 2, 3, 4, 5, 5, 4, 3, 2, 1].each { n |
  p n, " - SMA3: ", sma3.add(n), " SMA5: ", sma5.add(n)
}
```


Function version


```brat
sma = { period |
  list = []

  { num |
    true? list.length >= period
      { list.deq }

    list << num
    list.reduce(:+) / list.length
  }
}

sma3 = sma 3
sma5 = sma 5
[1, 2, 3, 4, 5, 5, 4, 3, 2, 1].each { n |
  p n, " - SMA3: ", sma3(n), " SMA5: ", sma5(n)
}
```


{{out}}

```txt
1 - SMA3: 1 SMA5: 1
2 - SMA3: 1.5 SMA5: 1.5
3 - SMA3: 2 SMA5: 2
4 - SMA3: 3 SMA5: 2.5
5 - SMA3: 4 SMA5: 3
5 - SMA3: 4.6666666666667 SMA5: 3.8
4 - SMA3: 4.6666666666667 SMA5: 4.2
3 - SMA3: 4 SMA5: 4.2
2 - SMA3: 3 SMA5: 3.8
1 - SMA3: 2 SMA5: 3
```



## C


```c
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>

typedef struct sma_obj {
  double sma;
  double sum;
  int period;
  double *values;
  int lv;
} sma_obj_t;

typedef union sma_result {
  sma_obj_t *handle;
  double sma;
  double *values;
} sma_result_t;

enum Action { SMA_NEW, SMA_FREE, SMA_VALUES, SMA_ADD, SMA_MEAN };
sma_result_t sma(enum Action action, ...)
{
  va_list vl;
  sma_result_t r;
  sma_obj_t *o;
  double v;

  va_start(vl, action);
  switch(action) {
  case SMA_NEW: // args: int period
    r.handle = malloc(sizeof(sma_obj_t));
    r.handle->sma = 0.0;
    r.handle->period = va_arg(vl, int);
    r.handle->values = malloc(r.handle->period * sizeof(double));
    r.handle->lv = 0;
    r.handle->sum = 0.0;
    break;
  case SMA_FREE: // args: sma_obj_t *handle
    r.handle = va_arg(vl, sma_obj_t *);
    free(r.handle->values);
    free(r.handle);
    r.handle = NULL;
    break;
  case SMA_VALUES: // args: sma_obj_t *handle
    o = va_arg(vl, sma_obj_t *);
    r.values = o->values;
    break;
  case SMA_MEAN: // args: sma_obj_t *handle
    o = va_arg(vl, sma_obj_t *);
    r.sma = o->sma;
    break;
  case SMA_ADD: // args: sma_obj_t *handle, double value
    o = va_arg(vl, sma_obj_t *);
    v = va_arg(vl, double);
    if ( o->lv < o->period ) {
      o->values[o->lv++] = v;
      o->sum += v;
      o->sma = o->sum / o->lv;
    } else {
      o->sum -= o->values[ o->lv % o->period];
      o->sum += v;
      o->sma = o->sum / o->period;
      o->values[ o->lv % o->period ] = v; o->lv++;
    }
    r.sma = o->sma;
    break;
  }
  va_end(vl);
  return r;
}
```



```c
double v[] = { 1, 2, 3, 4, 5, 5, 4, 3, 2, 1 };

int main()
{
  int i;

  sma_obj_t *h3 = sma(SMA_NEW, 3).handle;
  sma_obj_t *h5 = sma(SMA_NEW, 5).handle;

  for(i=0; i < sizeof(v)/sizeof(double) ; i++) {
    printf("next number %lf, SMA_3 = %lf, SMA_5 = %lf\n",
	   v[i], sma(SMA_ADD, h3, v[i]).sma, sma(SMA_ADD, h5, v[i]).sma);
  }

  sma(SMA_FREE, h3);
  sma(SMA_FREE, h5);
  return 0;
}
```



## C++


```cpp

#include <iostream>
#include <stddef.h>
#include <assert.h>

using std::cout;
using std::endl;

class SMA {
public:
	SMA(unsigned int period) :
		period(period), window(new double[period]), head(NULL), tail(NULL),
				total(0) {
		assert(period >= 1);
	}
	~SMA() {
		delete[] window;
	}

	// Adds a value to the average, pushing one out if nescessary
	void add(double val) {
		// Special case: Initialization
		if (head == NULL) {
			head = window;
			*head = val;
			tail = head;
			inc(tail);
			total = val;
			return;
		}

		// Were we already full?
		if (head == tail) {
			// Fix total-cache
			total -= *head;
			// Make room
			inc(head);
		}

		// Write the value in the next spot.
		*tail = val;
		inc(tail);

		// Update our total-cache
		total += val;
	}

	// Returns the average of the last P elements added to this SMA.
	// If no elements have been added yet, returns 0.0
	double avg() const {
		ptrdiff_t size = this->size();
		if (size == 0) {
			return 0; // No entries => 0 average
		}
		return total / (double) size; // Cast to double for floating point arithmetic
	}

private:
	unsigned int period;
	double * window; // Holds the values to calculate the average of.

	// Logically, head is before tail
	double * head; // Points at the oldest element we've stored.
	double * tail; // Points at the newest element we've stored.

	double total; // Cache the total so we don't sum everything each time.

	// Bumps the given pointer up by one.
	// Wraps to the start of the array if needed.
	void inc(double * & p) {
		if (++p >= window + period) {
			p = window;
		}
	}

	// Returns how many numbers we have stored.
	ptrdiff_t size() const {
		if (head == NULL)
			return 0;
		if (head == tail)
			return period;
		return (period + tail - head) % period;
	}
};

int main(int argc, char * * argv) {
	SMA foo(3);
	SMA bar(5);

	int data[] = { 1, 2, 3, 4, 5, 5, 4, 3, 2, 1 };
	for (int * itr = data; itr < data + 10; itr++) {
		foo.add(*itr);
		cout << "Added " << *itr << " avg: " << foo.avg() << endl;
	}
	cout << endl;
	for (int * itr = data; itr < data + 10; itr++) {
		bar.add(*itr);
		cout << "Added " << *itr << " avg: " << bar.avg() << endl;
	}

	return 0;
}

```


## C#

{{works with|C sharp|C#|3}}


```c#
using System;
using System.Collections.Generic;
using System.Linq;

namespace SMA {
    class Program {
        static void Main(string[] args) {
            var nums = Enumerable.Range(1, 5).Select(n => (double)n);
            nums = nums.Concat(nums.Reverse());

            var sma3 = SMA(3);
            var sma5 = SMA(5);

            foreach (var n in nums) {
                Console.WriteLine("{0}    (sma3) {1,-16} (sma5) {2,-16}", n, sma3(n), sma5(n));
            }
        }

        static Func<double, double> SMA(int p) {
            Queue<double> s = new Queue<double>(p);
            return (x) => {
                if (s.Count >= p) {
                    s.Dequeue();
                }
                s.Enqueue(x);
                return s.Average();
            };
        }
    }
}
```


{{out}}

```txt

1    (sma3) 1                (sma5) 1
2    (sma3) 1.5              (sma5) 1.5
3    (sma3) 2                (sma5) 2
4    (sma3) 3                (sma5) 2.5
5    (sma3) 4                (sma5) 3
5    (sma3) 4.66666666666667 (sma5) 3.8
4    (sma3) 4.66666666666667 (sma5) 4.2
3    (sma3) 4                (sma5) 4.2
2    (sma3) 3                (sma5) 3.8
1    (sma3) 2                (sma5) 3

```



## Clojure

This version uses a persistent queue to hold the most recent ''p'' values.
Each function returned from ''init-moving-average'' has its state in an atom holding a queue value.

```clojure
(import '[clojure.lang PersistentQueue])

(defn enqueue-max [q p n]
  (let [q (conj q n)]
    (if (<= (count q) p) q (pop q))))

(defn avg [coll] (/ (reduce + coll) (count coll)))

(defn init-moving-avg [p]
  (let [state (atom PersistentQueue/EMPTY)]
    (fn [n]
      (avg (swap! state enqueue-max p n)))))
```



## CoffeeScript


```coffeescript

I = (P) ->
  # The cryptic name "I" follows the problem description;
  # it returns a function that computes a moving average
  # of successive values over the period P, using closure
  # variables to maintain state.
  cq = circular_queue(P)
  num_elems = 0
  sum = 0

  SMA = (n) ->
    sum += n
    if num_elems < P
      cq.add(n)
      num_elems += 1
      sum / num_elems
    else
      old = cq.replace(n)
      sum -= old
      sum / P

circular_queue = (n) ->
  # queue that only ever stores up to n values;
  # Caller shouldn't call replace until n values
  # have been added.
  i = 0
  arr = []

  add: (elem) ->
    arr.push elem
  replace: (elem) ->
    # return value whose age is "n"
    old_val = arr[i]
    arr[i] = elem
    i = (i + 1) % n
    old_val

# The output of the code below should convince you that
# calling I multiple times returns functions with independent
# state.
sma3 = I(3)
sma7 = I(7)
sma11 = I(11)
for i in [1..10]
  console.log i, sma3(i), sma7(i), sma11(i)

```

{{out}}

```txt

> coffee moving_average.coffee
1 1 1 1
2 1.5 1.5 1.5
3 2 2 2
4 3 2.5 2.5
5 4 3 3
6 5 3.5 3.5
7 6 4 4
8 7 5 4.5
9 8 6 5
10 9 7 5.5

```



## Common Lisp


This implementation uses a circular list to store the numbers within the window; at the beginning of each iteration <var>pointer</var> refers to the list cell which holds the value just moving out of the window and to be replaced with the just-added value.


```lisp
(defun simple-moving-average (period &aux
    (sum 0) (count 0) (values (make-list period)) (pointer values))
  (setf (rest (last values)) values)  ; construct circularity
  (lambda (n)
    (when (first pointer)
      (decf sum (first pointer)))     ; subtract old value
    (incf sum n)                      ; add new value
    (incf count)
    (setf (first pointer) n)
    (setf pointer (rest pointer))     ; advance pointer
    (/ sum (min count period))))
```



## D


### Using a Closure

Currently this <code>sma</code> can't be @nogc because it allocates a closure on the heap. Some escape analysis could remove the heap allocation.

```d
import std.stdio, std.traits, std.algorithm;

auto sma(T, int period)() pure nothrow @safe {
    T[period] data = 0;
    T sum = 0;
    int index, nFilled;

    return (in T v) nothrow @safe @nogc {
        sum += -data[index] + v;
        data[index] = v;
        index = (index + 1) % period;
        nFilled = min(period, nFilled + 1);
        return CommonType!(T, float)(sum) / nFilled;
    };
}

void main() {
    immutable s3 = sma!(int, 3);
    immutable s5 = sma!(double, 5);

    foreach (immutable e; [1, 2, 3, 4, 5, 5, 4, 3, 2, 1])
        writefln("Added %d, sma(3) = %f, sma(5) = %f", e, s3(e), s5(e));
}
```

{{out}}

```txt
Added 1, sma(3) = 1.000000, sma(5) = 1.000000
Added 2, sma(3) = 1.500000, sma(5) = 1.500000
Added 3, sma(3) = 2.000000, sma(5) = 2.000000
Added 4, sma(3) = 3.000000, sma(5) = 2.500000
Added 5, sma(3) = 4.000000, sma(5) = 3.000000
Added 5, sma(3) = 4.666667, sma(5) = 3.800000
Added 4, sma(3) = 4.666667, sma(5) = 4.200000
Added 3, sma(3) = 4.000000, sma(5) = 4.200000
Added 2, sma(3) = 3.000000, sma(5) = 3.800000
Added 1, sma(3) = 2.000000, sma(5) = 3.000000
```



### Using a Struct

This version avoids the heap allocation of the closure
keeping the data in the stack frame of the main function.
Same output:

```d
import std.stdio, std.traits, std.algorithm;

struct SMA(T, int period) {
    T[period] data = 0;
    T sum = 0;
    int index, nFilled;

    auto opCall(in T v) pure nothrow @safe @nogc {
        sum += -data[index] + v;
        data[index] = v;
        index = (index + 1) % period;
        nFilled = min(period, nFilled + 1);
        return CommonType!(T, float)(sum) / nFilled;
    }
}

void main() {
    SMA!(int, 3) s3;
    SMA!(double, 5) s5;

    foreach (immutable e; [1, 2, 3, 4, 5, 5, 4, 3, 2, 1])
        writefln("Added %d, sma(3) = %f, sma(5) = %f", e, s3(e), s5(e));
}
```

To avoid the floating point approximations keep piling up and growing, the code could perform a periodic sum on the whole circular queue array.


## Dyalect


{{trans|C#}}


```dyalect
func avg(xs) {
    var acc = 0.0
    var c = 0
    for x in xs {
        c += 1
        acc += x
    }
    acc / c
}

func sma(p) {
    var s = []
    x => {
        if s.len() >= p {
            s.removeAt(0)
        }
        s.insert(s.len(), x)
        avg(s)
    };
}

var nums = Iterator.concat(1.0..5.0, 5.0..1.0)
var sma3 = sma(3)
var sma5 = sma(5)

for n in nums {
    print("\(n)\t(sma3) \(sma3(n))\t(sma5) \(sma5(n))")
}
```



## E


This implementation produces two (function) objects sharing state.
It is idiomatic in E to separate input from output (read from write)
rather than combining them into one object.

The structure is the same as the implementation of [[Standard Deviation#E]].


```e
pragma.enable("accumulator")
def makeMovingAverage(period) {
    def values := ([null] * period).diverge()
    var index := 0
    var count := 0

    def insert(v) {
        values[index] := v
        index := (index + 1) %% period
        count += 1
    }

    /** Returns the simple moving average of the inputs so far, or null if there
        have been no inputs. */
    def average() {
        if (count > 0) {
            return accum 0 for x :notNull in values { _ + x } / count.min(period)
        }
    }

    return [insert, average]
}
```


<div style="overflow: auto; max-height: 12em;">
```e
? for period in [3, 5] {
>     def [insert, average] := makeMovingAverage(period)
>     println(`Period $period:`)
>     for value in [1,2,3,4,5,5,4,3,2,1] {
>         insert(value)
>         println(value, "\t", average())
>     }
>     println()
> }

Period 3:
1	1.0
2	1.5
3	2.0
4	3.0
5	4.0
5	4.666666666666667
4	4.666666666666667
3	4.0
2	3.0
1	2.0

Period 5:
1	1.0
2	1.5
3	2.0
4	2.5
5	3.0
5	3.8
4	4.2
3	4.2
2	3.8
1	3.0
```
</div>


## EchoLisp


```scheme

(lib 'tree) ;; queues operations


(define (make-sma p)
	(define Q (queue (gensym)))
	(lambda (item)
		(q-push Q item)
		(when (> (queue-length Q) p) (q-pop Q))
		(// (for/sum ((x (queue->list Q))) x)  (queue-length Q))))


```

{{out}}

```txt

(define serie '(1 2 3 4 5 5 4 3 2 1))
(define sma-3 (make-sma 3))
(define sma-5 (make-sma 5))

(for ((x serie)) (printf "%3d %10d %10d" x (sma-3 x) (sma-5 x)))

  1          1          1
  2        1.5        1.5
  3          2          2
  4          3        2.5
  5          4          3
  5     4.6667        3.8
  4     4.6667        4.2
  3          4        4.2
  2          3        3.8
  1          2          3

```



## Elena

ELENA 4.x :

```elena
import system'routines;
import system'collections;
import extensions;

class SMA
{
    object thePeriod;
    object theList;

    constructor new(period)
    {
        thePeriod := period;
        theList :=new List();
    }

    append(n)
    {
        theList.append(n);

        var count := theList.Length;
        count =>
            0 { ^0.0r }
            : {
                if (count > thePeriod)
                {
                    theList.removeAt:0;

                    count := thePeriod
                };

                var sum := theList.summarize(new Real());

                ^ sum / count
            }
    }
}

public program()
{
    var SMA3 := SMA.new:3;
    var SMA5 := SMA.new:5;

    for (int i := 1, i <= 5, i += 1) {
        console.printPaddingRight(30, "sma3 + ", i, " = ", SMA3.append:i);
        console.printLine("sma5 + ", i, " = ", SMA5.append:i)
    };

    for (int i := 5, i >= 1, i -= 1) {
        console.printPaddingRight(30, "sma3 + ", i, " = ", SMA3.append:i);
        console.printLine("sma5 + ", i, " = ", SMA5.append:i)
    };

    console.readChar()
}
```

{{out}}

```txt

sma3 + 1 = 1.0                sma5 + 1 = 1.0
sma3 + 2 = 1.5                sma5 + 2 = 1.5
sma3 + 3 = 2.0                sma5 + 3 = 2.0
sma3 + 4 = 3.0                sma5 + 4 = 2.5
sma3 + 5 = 4.0                sma5 + 5 = 3.0
sma3 + 5 = 4.666666666667     sma5 + 5 = 3.8
sma3 + 4 = 4.666666666667     sma5 + 4 = 4.2
sma3 + 3 = 4.0                sma5 + 3 = 4.2
sma3 + 2 = 3.0                sma5 + 2 = 3.8
sma3 + 1 = 2.0                sma5 + 1 = 3.0

```



## Elixir

The elixir program below generates an anonymous function with an embedded period `p`, which is used as the period of the simple moving average.  The `run` function reads numeric input and passes it to the newly created anonymous function, and then "inspects" the result to STDOUT.


```elixir
$ cat simple-moving-avg.exs
#!/usr/bin/env elixir

defmodule Math do
  def average([]), do: nil
  def average(enum) do
    Enum.sum(enum) / length(enum)
  end
end

defmodule SMA do

  def sma(l, p \\ 10) do
    IO.puts("\nSimple moving average(period=#{p}):")
    Enum.chunk(l, p, 1)
    |> Enum.map(&(%{"input": &1, "avg": Float.round(Math.average(&1), 3)}))
  end

  defmacro gen_func(p) do
    quote do
      fn l -> SMA.sma(l, unquote(p)) end
    end
  end

  def read_numeric_input do
    IO.stream(:stdio, :line)
    |> Enum.map(&(String.split(&1, ~r{\s+})))
    |> List.flatten()
    |> Enum.reject(&(is_nil(&1) || String.length(&1) == 0))
    |> Enum.map(&(Integer.parse(&1) |> elem(0)))
  end

  def run do
    sma_func_10 = gen_func(10)
    sma_func_15 = gen_func(15)
    numbers = read_numeric_input
    sma_func_10.(numbers) |> IO.inspect
    sma_func_15.(numbers) |> IO.inspect
  end
end

SMA.run
```



```bash
#!/bin/bash
elixir ./simple-moving-avg.exs <<EOF
1 2 3 4 5 6 7 8 9 8 7 6 5 4 3 2 1
2 4 6 8 10 12 14 12 10 8 6 4 2
EOF
```


The output is shown below, with the average, followed by the grouped input, forming the basis of each moving average.


```txt
$ ./simple-moving-avg.sh

Simple moving average(period=10):
[%{avg: 5.3, input: [1, 2, 3, 4, 5, 6, 7, 8, 9, 8]},
 %{avg: 5.9, input: [2, 3, 4, 5, 6, 7, 8, 9, 8, 7]},
 %{avg: 6.3, input: [3, 4, 5, 6, 7, 8, 9, 8, 7, 6]},
 %{avg: 6.5, input: [4, 5, 6, 7, 8, 9, 8, 7, 6, 5]},
 %{avg: 6.5, input: [5, 6, 7, 8, 9, 8, 7, 6, 5, 4]},
 %{avg: 6.3, input: [6, 7, 8, 9, 8, 7, 6, 5, 4, 3]},
 %{avg: 5.9, input: [7, 8, 9, 8, 7, 6, 5, 4, 3, 2]},
 %{avg: 5.3, input: [8, 9, 8, 7, 6, 5, 4, 3, 2, 1]},
 %{avg: 4.7, input: [9, 8, 7, 6, 5, 4, 3, 2, 1, 2]},
 %{avg: 4.2, input: [8, 7, 6, 5, 4, 3, 2, 1, 2, 4]},
 %{avg: 4.0, input: [7, 6, 5, 4, 3, 2, 1, 2, 4, 6]},
 %{avg: 4.1, input: [6, 5, 4, 3, 2, 1, 2, 4, 6, 8]},
 %{avg: 4.5, input: [5, 4, 3, 2, 1, 2, 4, 6, 8, 10]},
 %{avg: 5.2, input: [4, 3, 2, 1, 2, 4, 6, 8, 10, 12]},
 %{avg: 6.2, input: [3, 2, 1, 2, 4, 6, 8, 10, 12, 14]},
 %{avg: 7.1, input: [2, 1, 2, 4, 6, 8, 10, 12, 14, 12]},
 %{avg: 7.9, input: [1, 2, 4, 6, 8, 10, 12, 14, 12, 10]},
 %{avg: 8.6, input: [2, 4, 6, 8, 10, 12, 14, 12, 10, 8]},
 %{avg: 9.0, input: [4, 6, 8, 10, 12, 14, 12, 10, 8, 6]},
 %{avg: 9.0, input: [6, 8, 10, 12, 14, 12, 10, 8, 6, 4]},
 %{avg: 8.6, input: [8, 10, 12, 14, 12, 10, 8, 6, 4, 2]}]

Simple moving average(period=15):
[%{avg: 5.2, input: [1, 2, 3, 4, 5, 6, 7, 8, 9, 8, 7, 6, 5, 4, 3]},
 %{avg: 5.267, input: [2, 3, 4, 5, 6, 7, 8, 9, 8, 7, 6, 5, 4, 3, 2]},
 %{avg: 5.2, input: [3, 4, 5, 6, 7, 8, 9, 8, 7, 6, 5, 4, 3, 2, 1]},
 %{avg: 5.133, input: [4, 5, 6, 7, 8, 9, 8, 7, 6, 5, 4, 3, 2, 1, 2]},
 %{avg: 5.133, input: [5, 6, 7, 8, 9, 8, 7, 6, 5, 4, 3, 2, 1, 2, 4]},
 %{avg: 5.2, input: [6, 7, 8, 9, 8, 7, 6, 5, 4, 3, 2, 1, 2, 4, 6]},
 %{avg: 5.333, input: [7, 8, 9, 8, 7, 6, 5, 4, 3, 2, 1, 2, 4, 6, 8]},
 %{avg: 5.533, input: [8, 9, 8, 7, 6, 5, 4, 3, 2, 1, 2, 4, 6, 8, 10]},
 %{avg: 5.8, input: [9, 8, 7, 6, 5, 4, 3, 2, 1, 2, 4, 6, 8, 10, 12]},
 %{avg: 6.133, input: [8, 7, 6, 5, 4, 3, 2, 1, 2, 4, 6, 8, 10, 12, 14]},
 %{avg: 6.4, input: [7, 6, 5, 4, 3, 2, 1, 2, 4, 6, 8, 10, 12, 14, 12]},
 %{avg: 6.6, input: [6, 5, 4, 3, 2, 1, 2, 4, 6, 8, 10, 12, 14, 12, 10]},
 %{avg: 6.733, input: [5, 4, 3, 2, 1, 2, 4, 6, 8, 10, 12, 14, 12, 10, 8]},
 %{avg: 6.8, input: [4, 3, 2, 1, 2, 4, 6, 8, 10, 12, 14, 12, 10, 8, 6]},
 %{avg: 6.8, input: [3, 2, 1, 2, 4, 6, 8, 10, 12, 14, 12, 10, 8, 6, 4]},
 %{avg: 6.733, input: [2, 1, 2, 4, 6, 8, 10, 12, 14, 12, 10, 8, 6, 4, 2]}]
```



## Erlang


```erlang
main() ->
    SMA3 = sma(3),
    SMA5 = sma(5),
    Ns = [1, 2, 3, 4, 5, 5, 4, 3, 2, 1],
    lists:foreach(
      fun (N) ->
              io:format("Added ~b, sma(3) -> ~f, sma(5) -> ~f~n",[N,next(SMA3,N),next(SMA5,N)])
      end, Ns),
    stop(SMA3),
    stop(SMA5).

sma(W) ->
    {sma,spawn(?MODULE,loop,[W,[]])}.

loop(Window, Numbers) ->
    receive
        {_Pid, stop} ->
            ok;
        {Pid, N} when is_number(N) ->
            case length(Numbers) < Window of
                true ->
                    Next = Numbers++[N];
                false ->
                    Next = tl(Numbers)++[N]
            end,
            Pid ! {average, lists:sum(Next)/length(Next)},
            loop(Window,Next);
        _ ->
            ok
    end.

stop({sma,Pid}) ->
    Pid ! {self(),stop},
    ok.

next({sma,Pid},N) ->
    Pid ! {self(), N},
    receive
        {average, Ave} ->
            Ave
    end.
```


{{out}}

```erlang>9
 sma:main().
Added 1, sma(3) -> 1.000000, sma(5) -> 1.000000
Added 2, sma(3) -> 1.500000, sma(5) -> 1.500000
Added 3, sma(3) -> 2.000000, sma(5) -> 2.000000
Added 4, sma(3) -> 3.000000, sma(5) -> 2.500000
Added 5, sma(3) -> 4.000000, sma(5) -> 3.000000
Added 5, sma(3) -> 4.666667, sma(5) -> 3.800000
Added 4, sma(3) -> 4.666667, sma(5) -> 4.200000
Added 3, sma(3) -> 4.000000, sma(5) -> 4.200000
Added 2, sma(3) -> 3.000000, sma(5) -> 3.800000
Added 1, sma(3) -> 2.000000, sma(5) -> 3.000000
ok
```


Erlang has closures, but immutable variables. A solution then is to use processes and a simple message passing based API.


## Euler Math Toolbox


Matrix languages have routines to compute the gliding avarages for a given sequence of items.


```Euler Math Toolbox

>n=1000; m=100; x=random(1,n);
>x10=fold(x,ones(1,m)/m);
>x10=fftfold(x,ones(1,m)/m)[m:n]; // more efficient

```


It is less efficient to loop as in the following commands.


```Euler Math Toolbox

>function store (x:number, v:vector, n:index) ...
$if cols(v)<n then return v|x;
$else
$  v=rotleft(v); v[-1]=x;
$  return v;
$endif;
$endfunction
>v=zeros(1,0); for k=1:20; v=store(k,v,10); mean(v), end;
 1
 1.5
 2
 2.5
 3
 3.5
 4
 4.5
 5
 5.5
 6.5
 7.5
 8.5
 9.5
 10.5
 11.5
 12.5
 13.5
 14.5
 15.5
>v
 [ 11  12  13  14  15  16  17  18  19  20 ]

```


=={{header|F_Sharp|F#}}==

```fsharp
let sma period f (list:float list) =
    let sma_aux queue v =
        let q = Seq.truncate period (v :: queue)
        Seq.average q, Seq.toList q
    List.fold (fun s v ->
        let avg,state = sma_aux s v
        f avg
        state) [] list

printf "sma3: "
[ 1.;2.;3.;4.;5.;5.;4.;3.;2.;1.] |> sma 3 (printf "%.2f ")
printf "\nsma5: "
[ 1.;2.;3.;4.;5.;5.;4.;3.;2.;1.] |> sma 5 (printf "%.2f ")
printfn ""
```

{{out}}

```txt
sma3: 1.00 1.50 2.00 3.00 4.00 4.67 4.67 4.00 3.00 2.00
sma5: 1.00 1.50 2.00 2.50 3.00 3.80 4.20 4.20 3.80 3.00
```



## Factor

The <code>I</code> word creates a quotation (anonymous function) that closes over a sequence and a period. This quotation handles adding/removing numbers to the simple moving average (SMA). We can then add a number to the SMA using <code>sma-add</code> and get the SMA's sequence and mean with <code>sma-query</code>. Quotations adhere to the <code>sequence</code> protocol so we can obtain the sequence of numbers simply by calling <code>first</code> on the SMA quotation.

```factor
USING: kernel interpolate io locals math.statistics prettyprint
random sequences ;
IN: rosetta-code.simple-moving-avg

:: I ( P -- quot )
    V{ } clone :> v!
    [ v swap suffix! P short tail* v! ] ;

: sma-add ( quot n -- quot' ) swap tuck call( x x -- x ) ;

: sma-query ( quot -- avg v ) first concat dup mean swap ;

: simple-moving-average-demo ( -- )
    5 I 10 <iota> [
        over sma-query unparse
        [I After ${2} numbers Sequence is ${0} Mean is ${1}I] nl
        100 random sma-add
    ] each drop ;

MAIN: simple-moving-average-demo
```

{{out}}

```txt

After 0 numbers Sequence is V{ } Mean is 0
After 1 numbers Sequence is V{ 41 } Mean is 41
After 2 numbers Sequence is V{ 41 31 } Mean is 36
After 3 numbers Sequence is V{ 41 31 2 } Mean is 24+2/3
After 4 numbers Sequence is V{ 41 31 2 24 } Mean is 24+1/2
After 5 numbers Sequence is V{ 41 31 2 24 70 } Mean is 33+3/5
After 6 numbers Sequence is V{ 31 2 24 70 80 } Mean is 41+2/5
After 7 numbers Sequence is V{ 2 24 70 80 96 } Mean is 54+2/5
After 8 numbers Sequence is V{ 24 70 80 96 84 } Mean is 70+4/5
After 9 numbers Sequence is V{ 70 80 96 84 7 } Mean is 67+2/5

```



## Fantom



```fantom

class MovingAverage
{
  Int period
  Int[] stream

  new make (Int period)
  {
    this.period = period
    stream = [,]
  }

  // add number to end of stream and remove numbers from start if
  // stream is larger than period
  public Void addNumber (Int number)
  {
    stream.add (number)
    while (stream.size > period)
    {
      stream.removeAt (0)
    }
  }

  // compute average of numbers in stream
  public Float average ()
  {
    if (stream.isEmpty)
      return 0.0f
    else
      1.0f * (Int)(stream.reduce(0, |a,b| { (Int) a + b })) / stream.size
  }
}

class Main
{
  public static Void main ()
  { // test by adding random numbers and printing average after each number
    av := MovingAverage (5)

    10.times |i|
    {
      echo ("After $i numbers list is ${av.stream} average is ${av.average}")
      av.addNumber (Int.random(0..100))
    }
  }
}

```


{{out}} for a period of 5:

```txt

After 0 numbers list is [,] average is 0.0
After 1 numbers list is [64] average is 64.0
After 2 numbers list is [64, 50] average is 57.0
After 3 numbers list is [64, 50, 26] average is 46.666666666666664
After 4 numbers list is [64, 50, 26, 77] average is 54.25
After 5 numbers list is [64, 50, 26, 77, 82] average is 59.8
After 6 numbers list is [50, 26, 77, 82, 95] average is 66.0
After 7 numbers list is [26, 77, 82, 95, 11] average is 58.2
After 8 numbers list is [77, 82, 95, 11, 23] average is 57.6
After 9 numbers list is [82, 95, 11, 23, 50] average is 52.2

```



## Forth


```forth
: f+! ( f addr -- ) dup f@ f+ f! ;
: ,f0s ( n -- ) falign 0 do 0e f, loop ;

: period @ ;
: used cell+ ;
: head 2 cells + ;
: sum  3 cells + faligned ;
: ring ( addr -- faddr )
  dup sum float+ swap head @ floats + ;

: update ( fvalue addr -- addr )
       dup ring f@ fnegate dup sum f+!
  fdup dup ring f!         dup sum f+!
  dup head @ 1+  over period mod  over head ! ;

: moving-average
  create ( period -- ) dup , 0 , 0 , 1+ ,f0s
  does>  ( fvalue -- avg )
    update
    dup used @ over period < if 1 over used +! then
    dup sum f@ used @ 0 d>f f/ ;

3 moving-average sma
1e sma f.  \ 1.
2e sma f.  \ 1.5
3e sma f.  \ 2.
4e sma f.  \ 3.
```



## Fortran

{{works with|Fortran|90 and later}}

```fortran
program Movavg
  implicit none

  integer :: i

  write (*, "(a)") "SIMPLE MOVING AVERAGE: PERIOD = 3"

  do i = 1, 5
    write (*, "(a, i2, a, f8.6)") "Next number:", i, "   sma = ", sma(real(i))
  end do
  do i = 5, 1, -1
    write (*, "(a, i2, a, f8.6)") "Next number:", i, "   sma = ", sma(real(i))
  end do

contains

function sma(n)
  real :: sma
  real, intent(in) :: n
  real, save :: a(3) = 0
  integer, save :: count = 0

  if (count < 3) then
    count = count + 1
    a(count) = n
  else
    a = eoshift(a, 1, n)
  end if

  sma = sum(a(1:count)) / real(count)
end function

end program Movavg
```



## FreeBASIC


```freebasic
' FB 1.05.0 Win64

Type FuncType As Function(As Double) As Double

' These 'shared' variables are available to all functions defined below
Dim Shared p As UInteger
Dim Shared list() As Double

Function sma(n As Double) As Double
  Redim Preserve list(0 To UBound(list) + 1)
  list(UBound(list)) = n
  Dim start As Integer = 0
  Dim length As Integer = UBound(list) + 1
  If length > p Then
    start = UBound(list) - p + 1
    length = p
  End If
  Dim sum As Double = 0.0
  For i As Integer = start To UBound(list)
    sum += list(i)
  Next
  Return sum / length
End Function

Function initSma(period As Uinteger) As FuncType
  p = period
  Erase list '' ensure the array is empty on each initialization
  Return @sma
End Function

Dim As FuncType ma = initSma(3)
Print "Period = "; p
Print
For i As Integer = 0 To 9
  Print "Add"; i; " => moving average ="; ma(i)
Next
Print
ma = initSma(5)
Print "Period = "; p
Print
For i As Integer = 9 To 0 Step -1
  Print "Add"; i; " => moving average ="; ma(i)
Next
Print
Print "Press any key to quit"
Sleep
```


{{out}}

```txt

Period = 3

Add 0 => moving average = 0
Add 1 => moving average = 0.5
Add 2 => moving average = 1
Add 3 => moving average = 2
Add 4 => moving average = 3
Add 5 => moving average = 4
Add 6 => moving average = 5
Add 7 => moving average = 6
Add 8 => moving average = 7
Add 9 => moving average = 8

Period = 5

Add 9 => moving average = 9
Add 8 => moving average = 8.5
Add 7 => moving average = 8
Add 6 => moving average = 7.5
Add 5 => moving average = 7
Add 4 => moving average = 6
Add 3 => moving average = 5
Add 2 => moving average = 4
Add 1 => moving average = 3
Add 0 => moving average = 2

```



## GAP


```gap
MovingAverage := function(n)
  local sma, buffer, pos, sum, len;
  buffer := List([1 .. n], i -> 0);
  pos := 0;
  len := 0;
  sum := 0;
  sma := function(x)
    pos := RemInt(pos, n) + 1;
    sum := sum + x - buffer[pos];
    buffer[pos] := x;
    len := Minimum(len + 1, n);
    return sum/len;
  end;
  return sma;
end;

f := MovingAverage(3);
f(1);  #  1
f(2);  #  3/2
f(3);  #  2
f(4);  #  3
f(5);  #  4
f(5);  #  14/3
f(4);  #  14/3
f(3);  #  4
f(2);  #  3
f(1);  #  2
```


## Go


```go
package main

import "fmt"

func sma(n int) func(float64) float64 {
    s := make([]float64, 0, n)
    i, sum, rn := 0, 0., 1/float64(n)
    return func(x float64) float64 {
        if len(s) < n {
            sum += x
            s = append(s, x)
            return sum / float64(len(s))
        }
        s[i] = x
        i++
        if i == n {
            i = 0
        }
        sum = 0
        for _, x = range s {
            sum += x
        }
        return sum * rn
    }
}

func main() {
    sma3 := sma(3)
    sma5 := sma(5)
    fmt.Println("x       sma3   sma5")
    for _, x := range []float64{1, 2, 3, 4, 5, 5, 4, 3, 2, 1} {
        fmt.Printf("%5.3f  %5.3f  %5.3f\n", x, sma3(x), sma5(x))
    }
}
```

{{out}}

```txt

x       sma3   sma5
1.000  1.000  1.000
2.000  1.500  1.500
3.000  2.000  2.000
4.000  3.000  2.500
5.000  4.000  3.000
5.000  4.667  3.800
4.000  4.667  4.200
3.000  4.000  4.200
2.000  3.000  3.800
1.000  2.000  3.000

```



## Groovy

{{trans|Ruby}}

```groovy
def simple_moving_average = { size ->
    def nums = []
    double total = 0.0
    return { newElement ->
        nums += newElement
        oldestElement = nums.size() > size ? nums.remove(0) : 0
        total += newElement - oldestElement
        total / nums.size()
    }
}

ma5 = simple_moving_average(5)

(1..5).each{ printf( "%1.1f ", ma5(it)) }
(5..1).each{ printf( "%1.1f ", ma5(it)) }
```

{{out}}

```txt
1.0 1.5 2.0 2.5 3.0 3.8 4.2 4.2 3.8 3.0
```



## Haskell

Conform version to the requirement, function SMA called multiple times with just a number:
{{works with|GHC|6.10.4}}

```Haskell
{-# LANGUAGE BangPatterns #-}

import Control.Monad
import Data.List
import Data.IORef

data Pair a b = Pair !a !b

mean :: Fractional a => [a] -> a
mean = divl . foldl' (\(Pair s l) x -> Pair (s+x) (l+1)) (Pair 0.0 0)
  where divl (_,0) = 0.0
        divl (s,l) = s / fromIntegral l

series = [1,2,3,4,5,5,4,3,2,1]

mkSMA :: Int -> IO (Double -> IO Double)
mkSMA period = avgr <$> newIORef []
  where avgr nsref x = readIORef nsref >>= (\ns ->
            let xs = take period (x:ns)
            in writeIORef nsref xs $> mean xs)

main = mkSMA 3 >>= (\sma3 -> mkSMA 5 >>= (\sma5 ->
    mapM_ (str <$> pure n <*> sma3 <*> sma5) series))
  where str n mm3 mm5 =
    concat ["Next number = ",show n,", SMA_3 = ",show mm3,", SMA_5 = ",show mm5]
```

{{out}}

```txt
Next number = 1.0, SMA_3 = 1.0, SMA_5 = 1.0
Next number = 2.0, SMA_3 = 1.5, SMA_5 = 1.5
Next number = 3.0, SMA_3 = 2.0, SMA_5 = 2.0
Next number = 4.0, SMA_3 = 3.0, SMA_5 = 2.5
Next number = 5.0, SMA_3 = 4.0, SMA_5 = 3.0
Next number = 5.0, SMA_3 = 4.666666666666667, SMA_5 = 3.8
Next number = 4.0, SMA_3 = 4.666666666666667, SMA_5 = 4.2
Next number = 3.0, SMA_3 = 4.0, SMA_5 = 4.2
Next number = 2.0, SMA_3 = 3.0, SMA_5 = 3.8
Next number = 1.0, SMA_3 = 2.0, SMA_5 = 3.0
```



{{works with|GHC|6.10.4}}

```Haskell
import Data.List
import Control.Arrow
import Control.Monad

sMA p = map (head *** head ).tail.
      scanl (\(y,_) -> (id &&& return. av) . (: if length y == p then init y else y)) ([],[])
    where av = liftM2 (/) sum (fromIntegral.length)

printSMA n p = mapM_ (\(n,a) -> putStrLn $ "Next number: " ++ show n ++ "  Average: " ++ show a)
  . take n . sMA p $ [1..5]++[5,4..1]++[3..]
```


Stateful function using the state monad to keep track of state

{{works with|GHC|7.8.3}}

```Haskell

import Control.Monad
import Control.Monad.State

period :: Int
period = 3

type SMAState = [Float]

computeSMA :: Float -> State SMAState Float
computeSMA x = do
  previousValues <- get
  let values = previousValues ++ [x]
  let newAverage = if length values <= period then (sum values) / (fromIntegral $ length remainingValues :: Float)
                   else (sum remainingValues) / (fromIntegral $ length remainingValues :: Float)
                     where remainingValues = dropIf period values
  put $ dropIf period values
  return newAverage

dropIf :: Int -> [a] -> [a]
dropIf x xs = drop ((length xs) - x) xs

demostrateSMA :: State SMAState [Float]
demostrateSMA = mapM computeSMA [1, 2, 3, 4, 5, 5, 4, 3, 2, 1]

main :: IO ()
main = print $ evalState demostrateSMA []

```


{{out}}

```txt

[1.0,1.5,2.0,3.0,4.0,4.6666665,4.6666665,4.0,3.0,2.0]

```



## HicEst


```HicEst
REAL :: n=10, nums(n)

nums = (1,2,3,4,5, 5,4,3,2,1)
DO i = 1, n
   WRITE() "num=", i, "SMA3=", SMA(3,nums(i)), "SMA5=",SMA(5,nums(i))
ENDDO

END ! of "main"

FUNCTION SMA(period, num) ! maxID independent streams
 REAL :: maxID=10, now(maxID), Periods(maxID), Offsets(maxID), Pool(1000)

   ID = INDEX(Periods, period)
   IF( ID == 0) THEN ! initialization
     IDs = IDs + 1
     ID = IDs
     Offsets(ID) = SUM(Periods) + 1
     Periods(ID) = period
   ENDIF

   now(ID) = now(ID) + 1
   ALIAS(Pool,Offsets(ID),   Past,Periods(ID)) ! renames relevant part of data pool
   Past = Past($+1) ! shift left
   Past(Periods(ID)) = num
   SMA = SUM(Past) / MIN( now(ID), Periods(ID) )
 END
```


```txt
num=1 SMA3=1 SMA5=1
num=2 SMA3=1.5 SMA5=1.5
num=3 SMA3=2 SMA5=2
num=4 SMA3=3 SMA5=2.5
num=5 SMA3=4 SMA5=3
num=6 SMA3=4.666666667 SMA5=3.8
num=7 SMA3=4.666666667 SMA5=4.2
num=8 SMA3=4 SMA5=4.2
num=9 SMA3=3 SMA5=3.8
num=10 SMA3=2 SMA5=3
```


== {{header|Icon}} and {{header|Unicon}} ==

```unicon
procedure main(A)
    sma := buildSMA(3)  # Use better name than "I".
    every write(sma(!A))
end

procedure buildSMA(P)
    local stream
    c := create {
        stream := []
        while n := (avg@&source)[1] do {
           put(stream, n)
           if *stream > P then pop(stream)
           every (avg := 0.0) +:= !stream
           avg := avg/*stream
           }
        }
    return (@c, c)
end
```

Note: This program uses Unicon specific co-expression calling syntax.  It can be easily modified to run under Icon.

and a sample run:


```txt
->ravg 3 1 4 1 5 9 2 6 3 8
3.0
2.0
2.666666666666667
2.0
3.333333333333333
5.0
5.333333333333333
5.666666666666667
3.666666666666667
5.666666666666667
->

```


If the <tt>Utils</tt> package is imported from the [https://tapestry.tucson.az.us/unilib Unicon code library] then a (Unicon only) solution is:


```Unicon
import Utils

procedure main(A)
    sma1 := closure(SMA,[],3)
    sma2 := closure(SMA,[],4)
    every every n := !A do write(left(sma1(n),20), sma2(n))
end

procedure SMA(stream,P,n)
    put(stream, n)
    if *stream > P then pop(stream)
    every (avg := 0.0) +:= !stream
    return avg / *stream
end
```


with the sample run:


```txt
->ravg 3 1 4 1 5 9 2 6 3 8
3.0                 3.0
2.0                 2.0
2.666666666666667   2.666666666666667
2.0                 2.25
3.333333333333333   2.75
5.0                 4.75
5.333333333333333   4.25
5.666666666666667   5.5
3.666666666666667   5.0
5.666666666666667   4.75
->

```



## J


'''Note''': J is block-oriented, not stream oriented.  That is, J expresses algorithms with the semantics that all the data is available at once (rather than maintaining state and waiting for the next item).

In that context, moving average is expressed very concisely in J as '''<code>(+/%#)\</code>''', though it is worth noting that this approach does not provide averages for the initial cases where not all data would be available yet:


```J
   5 (+/%#)\ 1 2 3 4 5 5 4 3 2 1 NB. not a solution for this task
3 3.8 4.2 4.2 3.8 3
```


In the context of the task, we need to produce a stateful function to consume streams.  Since J does not have native lexical closure, we need to [http://www.jsoftware.com/jwiki/Guides/Lexical%20Closure implement it].  Thus the [[Talk:Averages/Simple_moving_average#J_Implementation|streaming solution]] is more complex:

```j
   lex =:  1 :'(a[n__a=.m#_.[a=.18!:3$~0)&(4 :''(+/%#)(#~1-128!:5)n__x=.1|.!.y n__x'')'
```

'''Example:'''

```j
   sma =: 5 lex
   sma&> 1 2 3 4 5 5 4 3 2 1
1 1.5 2 2.5 3 3.8 4.2 4.2 3.8 3
```

Here, the <code>&></code> is analogous to the "for each" of other languages.

Or, a more traditional approach could be used:


```j
avg=: +/ % #
SEQ=:''
moveAvg=:4 :0"0
   SEQ=:SEQ,y
   avg ({.~ x -@<. #) SEQ
)

   5 moveAvg 1 2 3 4 5 5 4 3 2 1
1 1.5 2 2.5 3 3.8 4.2 4.2 3.8 3
```



## Java

{{works with|Java|1.5+}}

```java5
import java.util.LinkedList;
import java.util.Queue;

public class MovingAverage {
    private final Queue<Double> window = new LinkedList<Double>();
    private final int period;
    private double sum;

    public MovingAverage(int period) {
        assert period > 0 : "Period must be a positive integer";
        this.period = period;
    }

    public void newNum(double num) {
        sum += num;
        window.add(num);
        if (window.size() > period) {
            sum -= window.remove();
        }
    }

    public double getAvg() {
        if (window.isEmpty()) return 0.0; // technically the average is undefined
        return sum / window.size();
    }

    public static void main(String[] args) {
        double[] testData = {1, 2, 3, 4, 5, 5, 4, 3, 2, 1};
        int[] windowSizes = {3, 5};
        for (int windSize : windowSizes) {
            MovingAverage ma = new MovingAverage(windSize);
            for (double x : testData) {
                ma.newNum(x);
                System.out.println("Next number = " + x + ", SMA = " + ma.getAvg());
            }
            System.out.println();
        }
    }
}
```

{{out}}

```txt
Next number = 1.0, SMA = 1.0
Next number = 2.0, SMA = 1.5
Next number = 3.0, SMA = 2.0
Next number = 4.0, SMA = 3.0
Next number = 5.0, SMA = 4.0
Next number = 5.0, SMA = 4.666666666666667
Next number = 4.0, SMA = 4.666666666666667
Next number = 3.0, SMA = 4.0
Next number = 2.0, SMA = 3.0
Next number = 1.0, SMA = 2.0

Next number = 1.0, SMA = 1.0
Next number = 2.0, SMA = 1.5
Next number = 3.0, SMA = 2.0
Next number = 4.0, SMA = 2.5
Next number = 5.0, SMA = 3.0
Next number = 5.0, SMA = 3.8
Next number = 4.0, SMA = 4.2
Next number = 3.0, SMA = 4.2
Next number = 2.0, SMA = 3.8
Next number = 1.0, SMA = 3.0
```



## JavaScript


### Using for loop


```javascript
function simple_moving_averager(period) {
    var nums = [];
    return function(num) {
        nums.push(num);
        if (nums.length > period)
            nums.splice(0,1);  // remove the first element of the array
        var sum = 0;
        for (var i in nums)
            sum += nums[i];
        var n = period;
        if (nums.length < period)
            n = nums.length;
        return(sum/n);
    }
}

var sma3 = simple_moving_averager(3);
var sma5 = simple_moving_averager(5);
var data = [1,2,3,4,5,5,4,3,2,1];
for (var i in data) {
    var n = data[i];
    // using WSH
    WScript.Echo("Next number = " + n + ", SMA_3 = " + sma3(n) + ", SMA_5 = " + sma5(n));
}
```

{{out}}

```txt
Next number = 1, SMA_3 = 1, SMA_5 = 1
Next number = 2, SMA_3 = 1.5, SMA_5 = 1.5
Next number = 3, SMA_3 = 2, SMA_5 = 2
Next number = 4, SMA_3 = 3, SMA_5 = 2.5
Next number = 5, SMA_3 = 4, SMA_5 = 3
Next number = 5, SMA_3 = 4.666666666666667, SMA_5 = 3.8
Next number = 4, SMA_3 = 4.666666666666667, SMA_5 = 4.2
Next number = 3, SMA_3 = 4, SMA_5 = 4.2
Next number = 2, SMA_3 = 3, SMA_5 = 3.8
Next number = 1, SMA_3 = 2, SMA_5 = 3
```



### Using reduce/filter

[http://jsfiddle.net/79xe381e/ JS Fiddle]


```javascript
// single-sided
Array.prototype.simpleSMA=function(N) {
return this.map(
  function(el,index, _arr) {
      return _arr.filter(
      function(x2,i2) {
        return i2 <= index && i2 > index - N;
        })
      .reduce(
      function(current, last, index, arr){
        return (current + last);
        })/index || 1;
      });
};

g=[0,1,2,3,4,5,6,7,8,9,10];
console.log(g.simpleSMA(3));
console.log(g.simpleSMA(5));
console.log(g.simpleSMA(g.length));
```

{{out}}

```txt

[1, 1, 1.5, 2, 2.25, 2.4, 2.5, 2.5714285714285716, 2.625, 2.6666666666666665, 2.7]
[1, 1, 1.5, 2, 2.5, 3, 3.3333333333333335, 3.5714285714285716, 3.75, 3.888888888888889, 4]
[1, 1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5, 5.5]

```



## Julia


```julia>using Statistics</lang

The function wants specified the type of the data in the buffer and, if you want, the limit of the buffer.

```julia
function movingaverage(::Type{T} = Float64; lim::Integer = -1) where T<:Real
	buffer = Vector{T}(0)
	if lim == -1
		# unlimited buffer
		return (y::T) -> begin
			push!(buffer, y)
			return mean(buffer)
		end
	else
		# limited size buffer
		return (y) -> begin
			push!(buffer, y)
			if length(buffer) > lim shift!(buffer) end
			return mean(buffer)
		end
	end
end

test = movingaverage()
@show test(1.0) # mean([1])
@show test(2.0) # mean([1, 2])
@show test(3.0) # mean([1, 2, 3])
```


{{out}}

```txt
test(1.0) = 1.0
test(2.0) = 1.5
test(3.0) = 2.0
```



## K


Non-stateful:

```K

  v:v,|v:1+!5
  v
1 2 3 4 5 5 4 3 2 1

  avg:{(+/x)%#x}
  sma:{avg'x@(,\!y),(1+!y)+\:!y}

  sma[v;5]
1 1.5 2 2.5 3 3.8 4.2 4.2 3.8 3

```


Stateful:

```K

  sma:{n::x#_n; {n::1_ n,x; {avg x@&~_n~'x} n}}

  sma[5]' v
1 1.5 2 2.5 3 3.8 4.2 4.2 3.8 3

```



## Kotlin


```scala
// version 1.0.6

fun initMovingAverage(p: Int): (Double) -> Double {
    if (p < 1) throw IllegalArgumentException("Period must be a positive integer")
    val list = mutableListOf<Double>()
    return {
        list.add(it)
        if (list.size > p) list.removeAt(0)
        list.average()
    }
}

fun main(args: Array<String>) {
    val sma4 = initMovingAverage(4)
    val sma5 = initMovingAverage(5)
    val numbers = listOf(1.0, 2.0, 3.0, 4.0, 5.0, 5.0, 4.0, 3.0, 2.0, 1.0)
    println("num\tsma4\tsma5\n")
    for (number in numbers) println("${number}\t${sma4(number)}\t${sma5(number)}")
}
```


{{out}}

```txt

num     sma4    sma5

1.0     1.0     1.0
2.0     1.5     1.5
3.0     2.0     2.0
4.0     2.5     2.5
5.0     3.5     3.0
5.0     4.25    3.8
4.0     4.5     4.2
3.0     4.25    4.2
2.0     3.5     3.8
1.0     2.5     3.0

```



## Lasso

{{incorrect|Lasso|routine is called with a list of multiple numbers rather than being called with individual numbers in succession.}}

```Lasso
define simple_moving_average(a::array,s::integer)::decimal => {
	#a->size == 0 ? return 0.00
	#s == 0 ? return 0.00
	#a->size == 1 ? return decimal(#a->first)
	#s == 1 ? return decimal(#a->last)
	local(na = array)
	if(#a->size <= #s) => {
		#na = #a
	else
		local(ar = #a->ascopy)
		#ar->reverse
		loop(#s) => { #na->insert(#ar->get(loop_count)) }
	}
	#s > #na->size ? #s = #na->size
	return (with e in #na sum #e) / decimal(#s)
}
// tests:
'SMA 3 on array(1,2,3,4,5,5,4,3,2,1): '
simple_moving_average(array(1,2,3,4,5,5,4,3,2,1),3)

'\rSMA 5 on array(1,2,3,4,5,5,4,3,2,1): '
simple_moving_average(array(1,2,3,4,5,5,4,3,2,1),5)

'\r\rFurther example: \r'
local(mynumbers = array, sma_num = 5)
loop(10) => {^
	#mynumbers->insert(integer_random(1,100))
	#mynumbers->size + ' numbers: ' + #mynumbers
	 ' SMA3 is: ' + simple_moving_average(#mynumbers,3)
	 ', SMA5 is: ' + simple_moving_average(#mynumbers,5)
	'\r'
^}
```


{{out}}

```txt
SMA 3 on array(1,2,3,4,5,5,4,3,2,1): 2.000000
SMA 5 on array(1,2,3,4,5,5,4,3,2,1): 3.000000

Further example:
1 numbers: array(91) SMA3 is: 91.000000, SMA5 is: 91.000000
2 numbers: array(91, 30) SMA3 is: 60.500000, SMA5 is: 60.500000
3 numbers: array(91, 30, 99) SMA3 is: 73.333333, SMA5 is: 73.333333
4 numbers: array(91, 30, 99, 73) SMA3 is: 67.333333, SMA5 is: 73.250000
5 numbers: array(91, 30, 99, 73, 22) SMA3 is: 64.666667, SMA5 is: 63.000000
6 numbers: array(91, 30, 99, 73, 22, 35) SMA3 is: 43.333333, SMA5 is: 51.800000
7 numbers: array(91, 30, 99, 73, 22, 35, 93) SMA3 is: 50.000000, SMA5 is: 64.400000
8 numbers: array(91, 30, 99, 73, 22, 35, 93, 24) SMA3 is: 50.666667, SMA5 is: 49.400000
9 numbers: array(91, 30, 99, 73, 22, 35, 93, 24, 8) SMA3 is: 41.666667, SMA5 is: 36.400000
10 numbers: array(91, 30, 99, 73, 22, 35, 93, 24, 8, 80) SMA3 is: 37.333333, SMA5 is: 48.000000
```



## Liberty BASIC

The interesting thing here is how to implement an equivalent of a stateful function.
For sample output see http://libertybasic.conforums.com/index.cgi?board=open&action=display&num=1322956720

```lb

    dim v$( 100)                                                            '   Each array term stores a particular SMA of period p in p*10 bytes

    nomainwin

    WindowWidth  =1080
    WindowHeight = 780

    graphicbox #w.gb1,   20,   20, 1000,  700

    open "Running averages to smooth data" for window as #w

    #w "trapclose quit"

    #w.gb1 "down"

    old.x         =  0
    old.y.orig    =500  '   black
    old.y.3.SMA   =350  '     red
    old.y.20.SMA  =300  '   green

    for i =0 to 999 step 1
        scan
        v       =1.1 +sin( i /1000 *2 *3.14159265) + 0.2 *rnd( 1)               '   sin wave with added random noise
        x       =i /6.28318 *1000
        y.orig  =500 -v /2.5 *500

        #w.gb1 "color black ; down ; line "; i-1; " "; old.y.orig;  " "; i; " "; y.orig;         " ; up"

        y.3.SMA =500 -SMA( 1, v,  3) /2.5 *500                                  '   SMA given ID of 1 is to do 3-term  running average
        #w.gb1 "color red   ; down ; line "; i-1; " "; old.y.3.SMA +50;  " "; i; " "; y.3.SMA  +50;  " ; up"

        y.20.SMA =500 -SMA( 2, v, 20) /2.5 *500                                 '   SMA given ID of 2 is to do 20-term running average
        #w.gb1 "color green ; down ; line "; i-1; " "; old.y.20.SMA +100; " "; i; " "; y.20.SMA +100; " ; up"

        'print "Supplied with "; v; ", so SMAs are now "; using( "###.###", SMA( 1, v, 3)); " over 3 terms or "; using( "###.###", SMA( 2, v, 5)) ; " over 5 terms."  '   ID, latest data, period

        old.y.orig    =y.orig
        old.y.3.SMA   =y.3.SMA
        old.y.20.SMA  =y.20.SMA
    next i

    wait

sub quit j$
    close #w
    end
end sub



function SMA( ID, Number, Period)
    v$( ID) =right$( "          " +str$( Number), 10) +v$( ID)              '   add new number at left, lose last number on right
    v$( ID) =left$( v$( ID), Period *10)
    'print "{"; v$( ID); "}",

    k      =0   '   number of terms read
    total  =0   '   sum of terms read

    do
        p$     =mid$( v$( ID), 1 +k *10, 10)
        if p$ ="" then exit do
        vv     =val( p$)
        total  =total +vv
        k      =k +1
    loop until p$ =""

    if k <Period then SMA =total / k else  SMA =total /Period
end function

```




## Logo

Although Logo does not support closures, some varieties of Logo support enough metaprogramming to accomplish this task.

{{works with|UCB Logo}}

UCB Logo has a DEFINE primitive to construct functions from structured instruction lists. In addition, UCB Logo supports a compact template syntax for quoting lists (backquote "`") and replacing components of quoted lists (comma ","). These facilities can be used together in order to create templated function-defining-functions.


```logo
to average :l
  output quotient apply "sum :l count :l
end

to make.sma :name :period
  localmake "qn word :name ".queue
  make :qn []
  define :name `[ [n]              ; parameter list
    [if equal? count :,:qn ,:period [ignore dequeue ",:qn]]
    [queue ",:qn :n]
    [output average :,:qn]
  ]
end

make.sma "avg3 3

show map "avg3 [1 2 3 4 5]     ; [1 1.5 2 3 4]

show text "avg3     ; examine what substitutions took place
[[n] [if equal? count :avg3.queue 3 [ignore dequeue "avg3.queue]] [queue "avg3.queue :n] [output average :avg3.queue]]

; the internal queue is in the global namespace, easy to inspect
show :avg3.queue    ; [3 4 5]
```


If namespace pollution is a concern, UCB Logo supplies a GENSYM command to obtain unique names in order to avoid collisions.


```logo
  ...
  localmake "qn word :name gensym
  ...

; list user-defined functions and variables
show procedures     ; [average avg3 make.sma]
show names          ; [[[] [avg3.g1]]
```



## Lua



```lua
function sma(period)
	local t = {}
	function sum(a, ...)
		if a then return a+sum(...) else return 0 end
	end
	function average(n)
		if #t == period then table.remove(t, 1) end
		t[#t + 1] = n
		return sum(unpack(t)) / #t
	end
	return average
end

sma5 = sma(5)
sma10 = sma(10)
print("SMA 5")
for v=1,15 do print(sma5(v)) end
print("\nSMA 10")
for v=1,15 do print(sma10(v)) end

```


=={{header|Mathematica}} / {{header|Wolfram Language}}==
This version uses a list entry so it can use the built-in function.

```Mathematica
MA[x_List, r_] := Join[Table[Mean[x[[1;;y]]],{y,r-1}], MovingAverage[x,r]]
```


This version is stateful instead.

```Mathematica
MAData = {{}, 0};
MAS[x_, t_: Null] :=
 With[{r = If[t === Null, MAData[[2]], t]},
  Mean[MAData[[1]] =
    If[Length[#] > (MAData[[2]] = r), #[[-r ;; -1]], #] &@
     Append[MAData[[1]], x]]]
```


Tests:

```txt

MA[{1, 2, 3, 4, 5, 5, 4, 3, 2, 1}, 5]
=> {1, 3/2, 2, 5/2, 3, 19/5, 21/5, 21/5, 19/5, 3}

MAS[1, 5]  => 1
MAS[2]     => 3/2
MAS[3]     => 2
MAS[4]     => 5/2
MAS[5]     => 3
MAS[5]     => 19/5
MAS[4]     => 21/5
MAS[3]     => 21/5
MAS[2]     => 19/5
MAS[1]     => 3

```


=={{header|MATLAB}} / {{header|Octave}}==

Matlab and Octave provide very efficient and fast functions, that can be applied to vectors (i.e. series of data samples)

```Matlab
 [m,z] = filter(ones(1,P),P,x);
```

m is the moving average, z returns the state at the end of the data series, which can be used to continue the moving average.

```Matlab
 [m,z] = filter(ones(1,P),P,x,z);
```



## Mercury

In Mercury, an idiomatic "moving averages" function would be 'stateless' - or rather, it would have ''explicit state'' that its callers would have to thread through uses of it:


```Mercury
    % state(period, list of floats from [newest, ..., oldest])
:- type state ---> state(int, list(float)).

:- func init(int) = state.
init(Period) = state(Period, []).

:- pred sma(float::in, float::out, state::in, state::out) is det.
sma(N, Average, state(P, L0), state(P, L)) :-
        take_upto(P, [N|L0], L),
        Average = foldl((+), L, 0.0) / float(length(L)).
```


Some notes about this solution: unless P = 0, length(L) can never be 0, as L always incorporates at least N (a step that is accomplished in the arguments to list.take_upto/3).  If the implementation of the 'state' type is hidden, and if init/1 checks for P = 0, users of this code can never cause a division-by-zero error in sma/4.  Although this solution doesn't try to be as stateful as the task description would like, explicit state is by far simpler and more natural and more straightforward than the alternative in Mercury.  Finally, [http://www.mercury.csse.unimelb.edu.au/information/doc-release/mercury_ref/State-variables.html#State-variables state variables] (and higher-order functions that anticipate threaded state) remove much of the potential ugliness or error in threading the same state through many users.


## MiniScript


We define an SMA class, which can be configured with the desired window size (P).

```MiniScript
SMA = {}
SMA.P = 5  // (a default; may be overridden)
SMA.buffer = null
SMA.next = function(n)
    if self.buffer == null then self.buffer = []
    self.buffer.push n
    if self.buffer.len > self.P then self.buffer.pull
    return self.buffer.sum / self.buffer.len
end function

sma3 = new SMA
sma3.P = 3
sma5 = new SMA

for i in range(10)
    num = round(rnd*100)
    print "num: " + num + "  sma3: " + sma3.next(num) + "  sma5: " + sma5.next(num)
end for
```


{{out}}

```txt
num: 81 sma3: 81 sma5: 81
num: 82 sma3: 81.5 sma5: 81.5
num: 78 sma3: 80.333333 sma5: 80.333333
num: 54 sma3: 71.333333 sma5: 73.75
num: 94 sma3: 75.333333 sma5: 77.8
num: 8 sma3: 52 sma5: 63.2
num: 40 sma3: 47.333333 sma5: 54.8
num: 98 sma3: 48.666667 sma5: 58.8
num: 48 sma3: 62 sma5: 57.6
num: 41 sma3: 62.333333 sma5: 47
num: 94 sma3: 61 sma5: 64.2
```




## NetRexx

{{trans|Java}}

```NetRexx
/* NetRexx */
options replace format comments java crossref symbols nobinary

numeric digits 20

class RAvgSimpleMoving public

  properties private
    window = java.util.Queue
    period
    sum

  properties constant
    exMsg = 'Period must be a positive integer'

  -- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  method RAvgSimpleMoving(period_) public
    if \period_.datatype('w') then signal RuntimeException(exMsg)
    if period_ <= 0           then signal RuntimeException(exMsg)
    window = LinkedList()
    period = period_
    sum    = 0
    return

  -- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  method newNum(num) public
    sum = sum + num
    window.add(num)
    if window.size() > period then do
      rmv = (Rexx window.remove())
      sum = sum - rmv
      end
    return

  -- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  method getAvg() public returns Rexx
    if window.isEmpty() then do
      avg = 0
      end
    else do
      avg = sum / window.size()
      end
    return avg

  -- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  method run_samples(args = String[]) public static
    testData = [Rexx 1, 2, 3, 4, 5, 5, 4, 3, 2, 1]
    windowSizes = [Rexx 3, 5]
    loop windSize over windowSizes
      ma = RAvgSimpleMoving(windSize)
      loop xVal over testData
        ma.newNum(xVal)
        say 'Next number =' xVal.right(5)', SMA =' ma.getAvg().format(10, 9)
        end xVal
      say
      end windSize

    return

  -- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  method main(args = String[]) public static
    run_samples(args)
    return

```

{{out}}
<pre style="height: 25ex; overflow: scroll">
Next number =   1.0, SMA =          1.000000000
Next number =   2.0, SMA =          1.500000000
Next number =   3.0, SMA =          2.000000000
Next number =   4.0, SMA =          3.000000000
Next number =   5.0, SMA =          4.000000000
Next number =   5.0, SMA =          4.666666667
Next number =   4.0, SMA =          4.666666667
Next number =   3.0, SMA =          4.000000000
Next number =   2.0, SMA =          3.000000000
Next number =   1.0, SMA =          2.000000000

Next number =   1.0, SMA =          1.000000000
Next number =   2.0, SMA =          1.500000000
Next number =   3.0, SMA =          2.000000000
Next number =   4.0, SMA =          2.500000000
Next number =   5.0, SMA =          3.000000000
Next number =   5.0, SMA =          3.800000000
Next number =   4.0, SMA =          4.200000000
Next number =   3.0, SMA =          4.200000000
Next number =   2.0, SMA =          3.800000000
Next number =   1.0, SMA =          3.000000000


```



## Nim


```nim
import queues

proc simplemovingaverage(period: int): auto =
  assert period > 0

  var
    summ, n = 0.0
    values = initQueue[float]()
  for i in 1..period:
    values.add(0)

  proc sma(x: float): float =
    values.add(x)
    summ += x - values.dequeue()
    n = min(n+1, float(period))
    result = summ / n

  return sma

var sma = simplemovingaverage(3)
for i in 1..5: echo sma(float(i))
for i in countdown(5,1): echo sma(float(i))

echo ""

var sma2 = simplemovingaverage(5)
for i in 1..5: echo sma2(float(i))
for i in countdown(5,1): echo sma2(float(i))
```

{{out}}

```txt
1.0000000000000000e+00
1.5000000000000000e+00
2.0000000000000000e+00
3.0000000000000000e+00
4.0000000000000000e+00
4.6666666666666670e+00
4.6666666666666670e+00
4.0000000000000000e+00
3.0000000000000000e+00
2.0000000000000000e+00

1.0000000000000000e+00
1.5000000000000000e+00
2.0000000000000000e+00
2.5000000000000000e+00
3.0000000000000000e+00
3.7999999999999998e+00
4.2000000000000002e+00
4.2000000000000002e+00
3.7999999999999998e+00
3.0000000000000000e+00
```



## Objeck

{{trans|Java}}

```objeck

﻿use Collection;

class MovingAverage {
  @window : FloatQueue;
  @period : Int;
  @sum : Float;

  New(period : Int) {
    @window := FloatQueue->New();
    @period := period;
  }

  method : NewNum(num : Float) ~ Nil {
    @sum += num;
    @window->Add(num);
    if(@window->Size() > @period) {
      @sum -= @window->Remove();
    };
  }

  method : GetAvg() ~ Float {
    if(@window->IsEmpty()) {
      return 0; # technically the average is undefined
    };

    return @sum / @window->Size();
  }

  function : Main(args : String[]) ~ Nil {
    testData := [1.0, 2.0, 3.0, 4.0, 5.0, 5.0, 4.0, 3.0, 2.0, 1.0];
    windowSizes := [3.0, 5.0];

    each(i : windowSizes) {
      windSize := windowSizes[i];
      ma := MovingAverage->New(windSize);
      each(j : testData) {
        x := testData[j];
        ma->NewNum(x);
        avg := ma->GetAvg();
        "Next number = {$x}, SMA = {$avg}"->PrintLine();
      };
      IO.Console->PrintLine();
    };
  }
}

```


{{out}}

```txt

Next number = 1.0, SMA = 1.0
Next number = 2.0, SMA = 1.500
Next number = 3.0, SMA = 2.0
Next number = 4.0, SMA = 3.0
Next number = 5.0, SMA = 4.0
Next number = 5.0, SMA = 4.667
Next number = 4.0, SMA = 4.667
Next number = 3.0, SMA = 4.0
Next number = 2.0, SMA = 3.0
Next number = 1.0, SMA = 2.0

Next number = 1.0, SMA = 1.0
Next number = 2.0, SMA = 1.500
Next number = 3.0, SMA = 2.0
Next number = 4.0, SMA = 2.500
Next number = 5.0, SMA = 3.0
Next number = 5.0, SMA = 3.800
Next number = 4.0, SMA = 4.200
Next number = 3.0, SMA = 4.200
Next number = 2.0, SMA = 3.800
Next number = 1.0, SMA = 3.0

```


=={{header|Objective-C}}==


```objc>#import <Foundation/Foundation.h


@interface MovingAverage : NSObject {
	unsigned int period;
	NSMutableArray *window;
	double sum;
}
- (instancetype)initWithPeriod:(unsigned int)thePeriod;
@end

@implementation MovingAverage

// init with default period
- (instancetype)init {
	self = [super init];
	if(self) {
		period = 10;
		window = [[NSMutableArray alloc] init];
		sum = 0.0;
	}
	return self;
}

// init with specified period
- (instancetype)initWithPeriod:(unsigned int)thePeriod {
	self = [super init];
	if(self) {
		period = thePeriod;
		window = [[NSMutableArray alloc] init];
		sum = 0.0;
	}
	return self;
}

// add a new number to the window
- (void)add:(double)val {
	sum += val;
	[window addObject:@(val)];
	if([window count] > period) {
		NSNumber *n = window[0];
		sum -= [n doubleValue];
		[window removeObjectAtIndex:0];
	}
}

// get the average value
- (double)avg {
	if([window count] == 0) {
		return 0; // technically the average is undefined
	}
	return sum / [window count];
}

// set the period, resizes current window
- (void)setPeriod:(unsigned int)thePeriod {
	// make smaller?
	if(thePeriod < [window count]) {
		for(int i = 0; i < thePeriod; ++i) {
			NSNumber *n = window[0];
			sum -= [n doubleValue];
			[window removeObjectAtIndex:0];
		}
	}
	period = thePeriod;
}

// get the period (window size)
- (unsigned int)period {
	return period;
}

// clear the window and current sum
- (void)clear {
	[window removeAllObjects];
	sum = 0;
}

@end

int main (int argc, const char * argv[]) {
	@autoreleasepool {
		double testData[10] = {1,2,3,4,5,5,4,3,2,1};
		int periods[2] = {3,5};
		for(int i = 0; i < 2; ++i) {
			MovingAverage *ma = [[MovingAverage alloc] initWithPeriod:periods[i]];
			for(int j = 0; j < 10; ++j) {
				[ma add:testData[j]];
				NSLog(@"Next number = %f, SMA = %f", testData[j], [ma avg]);
			}
			NSLog(@"\n");
		}
	}
	return 0;
}
```


{{out}}

```txt

Next number = 1.000000, SMA = 1.000000
Next number = 2.000000, SMA = 1.500000
Next number = 3.000000, SMA = 2.000000
Next number = 4.000000, SMA = 3.000000
Next number = 5.000000, SMA = 4.000000
Next number = 5.000000, SMA = 4.666667
Next number = 4.000000, SMA = 4.666667
Next number = 3.000000, SMA = 4.000000
Next number = 2.000000, SMA = 3.000000
Next number = 1.000000, SMA = 2.000000

Next number = 1.000000, SMA = 1.000000
Next number = 2.000000, SMA = 1.500000
Next number = 3.000000, SMA = 2.000000
Next number = 4.000000, SMA = 2.500000
Next number = 5.000000, SMA = 3.000000
Next number = 5.000000, SMA = 3.800000
Next number = 4.000000, SMA = 4.200000
Next number = 3.000000, SMA = 4.200000
Next number = 2.000000, SMA = 3.800000
Next number = 1.000000, SMA = 3.000000

```



## OCaml


```ocaml
let sma (n, s, q) x =
  let l = Queue.length q and s = s +. x in
  Queue.push x q;
  if l < n then
    (n, s, q), s /. float (l + 1)
  else (
    let s = s -. Queue.pop q in
    (n, s, q), s /. float l
  )

let _ =
  let periodLst = [ 3; 5 ] in
  let series = [ 1.; 2.; 3.; 4.; 5.; 5.; 4.; 3.; 2.; 1. ] in

  List.iter (fun d ->
    Printf.printf "SIMPLE MOVING AVERAGE: PERIOD = %d\n" d;
    ignore (
      List.fold_left (fun o x ->
	let o, m = sma o x in
	Printf.printf "Next number = %-2g, SMA = %g\n" x m;
	o
      ) (d, 0., Queue.create ()) series;
    );
    print_newline ();
  ) periodLst
```


{{out}}

```txt

SIMPLE MOVING AVERAGE: PERIOD = 3
Next number = 1 , SMA = 1
Next number = 2 , SMA = 1.5
Next number = 3 , SMA = 2
Next number = 4 , SMA = 3
Next number = 5 , SMA = 4
Next number = 5 , SMA = 4.66667
Next number = 4 , SMA = 4.66667
Next number = 3 , SMA = 4
Next number = 2 , SMA = 3
Next number = 1 , SMA = 2

SIMPLE MOVING AVERAGE: PERIOD = 5
Next number = 1 , SMA = 1
Next number = 2 , SMA = 1.5
Next number = 3 , SMA = 2
Next number = 4 , SMA = 2.5
Next number = 5 , SMA = 3
Next number = 5 , SMA = 3.8
Next number = 4 , SMA = 4.2
Next number = 3 , SMA = 4.2
Next number = 2 , SMA = 3.8
Next number = 1 , SMA = 3

```


More imperatively:

```ocaml
let sma_create period =
  let q = Queue.create ()
  and sum = ref 0.0 in
  fun x ->
    sum := !sum +. x;
    Queue.push x q;
    if Queue.length q > period then
      sum := !sum -. Queue.pop q;
    !sum /. float (Queue.length q)

let () =
  let periodLst = [ 3; 5 ] in
  let series = [ 1.; 2.; 3.; 4.; 5.; 5.; 4.; 3.; 2.; 1. ] in

  List.iter (fun d ->
    Printf.printf "SIMPLE MOVING AVERAGE: PERIOD = %d\n" d;
    let sma = sma_create d in
    List.iter (fun x ->
      Printf.printf "Next number = %-2g, SMA = %g\n" x (sma x);
    ) series;
    print_newline ();
  ) periodLst
```



## Oforth


createSMA returns a closure.
The list of values is included into a channel so this code is thread-safe : multiple tasks running in parallel can call the closure returned.


```oforth
import: parallel

: createSMA(period)
| ch |
   Channel new [ ] over send drop ->ch
   #[ ch receive + left(period) dup avg swap ch send drop ] ;
```


Usage:


```oforth
: test
| sma3 sma5 l |
   3 createSMA -> sma3
   5 createSMA -> sma5
   [ 1, 2, 3, 4, 5, 5, 4, 3, 2, 1 ] ->l
   "SMA3" .cr l apply( #[ sma3 perform . ] ) printcr
   "SMA5" .cr l apply( #[ sma5 perform . ] ) ;
```


{{out}}

```txt

>test
SMA3
1 1.5 2 3 4 4.66666666666667 4.66666666666667 4 3 2
SMA5
1 1.5 2 2.5 3 3.8 4.2 4.2 3.8 3 ok

```



## ooRexx

ooRexx does not have stateful functions, but the same effect can be achieved by using object instances.

```ooRexx

testdata = .array~of(1, 2, 3, 4, 5, 5, 4, 3, 2, 1)

-- run with different period sizes
loop period over .array~of(3, 5)
    say "Period size =" period
    say
    movingaverage = .movingaverage~new(period)
    loop number over testdata
        average = movingaverage~addnumber(number)
        say "   Next number =" number", moving average =" average
    end
    say
end

::class movingaverage
::method init
  expose period queue sum
  use strict arg period
  sum = 0
  -- the circular queue makes this easy
  queue = .circularqueue~new(period)

-- add a number to the average set
::method addNumber
  expose queue sum
  use strict arg number
  sum += number
  -- add this to the queue
  old = queue~queue(number)
  -- if we pushed an element off the end of the queue,
  -- subtract this from our sum
  if old \= .nil then sum -= old
  -- and return the current item
  return sum / queue~items

-- extra method to retrieve current average
::method average
  expose queue sum
  -- undefined really, but just return 0
  if queue~isempty then return 0
  -- return current queue
  return sum / queue~items

```

{{out}}

```txt

Period size = 3

   Next number = 1, moving average = 1
   Next number = 2, moving average = 1.5
   Next number = 3, moving average = 2
   Next number = 4, moving average = 3
   Next number = 5, moving average = 4
   Next number = 5, moving average = 4.66666667
   Next number = 4, moving average = 4.66666667
   Next number = 3, moving average = 4
   Next number = 2, moving average = 3
   Next number = 1, moving average = 2

Period size = 5

   Next number = 1, moving average = 1
   Next number = 2, moving average = 1.5
   Next number = 3, moving average = 2
   Next number = 4, moving average = 2.5
   Next number = 5, moving average = 3
   Next number = 5, moving average = 3.8
   Next number = 4, moving average = 4.2
   Next number = 3, moving average = 4.2
   Next number = 2, moving average = 3.8
   Next number = 1, moving average = 3

```



## OxygenBasic


```oxygenbasic
def max 1000

Class MovingAverage
'
### ============


indexbase 1
double average,invperiod,mdata[max]
sys    index,period

method Setup(double a,p)
sys i
Period=p
invPeriod=1/p
index=0
average=a
for i=1 to period
  mdata[i]=a
next
end method

method Data(double v) as double
sys i
index++
if index>period then index=1 'recycle
i=index+1 'for oldest data
if i>period then i=1 'recycle
mdata[index]=v
average=average+invperiod*(v-mdata[i])
end method

end class

'TEST
'====

MovingAverage A

A.Setup 100,10 'initial value and period

A.data 50
'...
print A.average 'reult 95

```



## Oz


```oz
declare

  fun {CreateSMA Period}
     Xs = {NewCell nil}
  in
     fun {$ X}
        Xs := {List.take X|@Xs Period}

        {FoldL @Xs Number.'+' 0.0}
        /
        {Int.toFloat {Min Period {Length @Xs}}}
     end
  end

in

  for Period in [3 5] do
     SMA = {CreateSMA Period}
  in
     {System.showInfo "\nSTART PERIOD "#Period}
     for I in 1..5 do
        {System.showInfo "  Number = "#I#" , SMA = "#{SMA {Int.toFloat I}}}
     end
     for I in 5..1;~1 do
        {System.showInfo "  Number = "#I#" , SMA = "#{SMA {Int.toFloat I}}}
     end
  end
```



## PARI/GP

Partial implementation: does not (yet?) create different stores on each invocation.

```parigp
sma_per(n)={
  sma_v=vector(n);
  sma_i = 0;
  n->if(sma_i++>#sma_v,sma_v[sma_i=1]=n;0,sma_v[sma_i]=n;0)+sum(i=1,#sma_v,sma_v[i])/#sma_v
};
```


## Pascal

{{works with|Free Pascal}}
Like in other implementations the sum of the last p values is only updated by subtracting the oldest value and addindg the new. To minimize rounding errors after p values the sum is corrected to the real sum.

```Pascal
program sma;
type
  tsma = record
            smaValue : array of double;
            smaAverage,
            smaSumOld,
            smaSumNew,
            smaRezActLength : double;
            smaActLength,
            smaLength,
            smaPos   :NativeInt;
            smaIsntFull: boolean;
         end;

procedure smaInit(var sma:tsma;p: NativeUint);
Begin
  with sma do
  Begin
    setlength(smaValue,0);
    setlength(smaValue,p);
    smaLength:= p;
    smaActLength := 0;
    smaAverage:= 0.0;
    smaSumOld := 0.0;
    smaSumNew := 0.0;
    smaPos := p-1;
    smaIsntFull := true
    end;
end;

function smaAddValue(var sma:tsma;v: double):double;
Begin
  with sma do
  Begin
    IF smaIsntFull then
    Begin
      inc(smaActLength);
      smaRezActLength := 1/smaActLength;
      smaIsntFull :=  smaActLength < smaLength ;
    end;
    smaSumOld := smaSumOld+v-smaValue[smaPos];
    smaValue[smaPos] := v;
    smaSumNew := smaSumNew+v;

    smaPos := smaPos-1;
    if smaPos < 0 then
    begin
      smaSumOld:= smaSumNew;
      smaSumNew:= 0.0;
      smaPos := smaLength-1;
    end;
    smaAverage := smaSumOld *smaRezActLength;
    smaAddValue:= smaAverage;
  end;
end;

var
 sma3,sma5:tsma;
 i : LongInt;
begin
  smaInit(sma3,3);
  smaInit(sma5,5);
  For i := 1 to 5 do
  Begin
    write('Inserting ',i,' into sma3 ',smaAddValue(sma3,i):0:4);
    writeln(' Inserting ',i,' into sma5 ',smaAddValue(sma5,i):0:4);
  end;
  For i := 5 downto 1 do
  Begin
    write('Inserting ',i,' into sma3 ',smaAddValue(sma3,i):0:4);
    writeln(' Inserting ',i,' into sma5 ',smaAddValue(sma5,i):0:4);
  end;
  //speed test
  smaInit(sma3,3);
  For i := 1 to 100000000 do
    smaAddValue(sma3,i);
  writeln('100''000''000 insertions ',sma3.smaAverage:0:4);
end.
```

;output:

```txt

time ./sma
Inserting 1 into sma3 1.0000 Inserting 1 into sma5 1.0000
Inserting 2 into sma3 1.5000 Inserting 2 into sma5 1.5000
Inserting 3 into sma3 2.0000 Inserting 3 into sma5 2.0000
Inserting 4 into sma3 3.0000 Inserting 4 into sma5 2.5000
Inserting 5 into sma3 4.0000 Inserting 5 into sma5 3.0000
Inserting 5 into sma3 4.6667 Inserting 5 into sma5 3.8000
Inserting 4 into sma3 4.6667 Inserting 4 into sma5 4.2000
Inserting 3 into sma3 4.0000 Inserting 3 into sma5 4.2000
Inserting 2 into sma3 3.0000 Inserting 2 into sma5 3.8000
Inserting 1 into sma3 2.0000 Inserting 1 into sma5 3.0000
100'000'000 insertions 99999999.0000

real  0m0.780s { 64-Bit }
```


## Perl


Using an initializer function which returns an anonymous closure which closes over an instance ''(separate for each call to the initializer!)'' of the lexical variables <code>$period</code>, <code>@list</code>, and <code>$sum</code>:


```perl
sub sma_generator {
    my $period = shift;
    my (@list, $sum);

    return sub {
        my $number = shift;
        push @list, $number;
        $sum += $number;
        $sum -= shift @list if @list > $period;
        return $sum / @list;
    }
}

# Usage:
my $sma = sma_generator(3);
for (1, 2, 3, 2, 7) {
    printf "append $_ --> sma = %.2f  (with period 3)\n", $sma->($_);
}
```


{{out}}

```txt

append 1 --> sma = 1.00  (with period 3)
append 2 --> sma = 1.50  (with period 3)
append 3 --> sma = 2.00  (with period 3)
append 2 --> sma = 2.33  (with period 3)
append 7 --> sma = 4.00  (with period 3)

```



## Perl 6


{{works with|Rakudo|2016.08}}

```perl6
sub sma-generator (Int $P where * > 0) {
    sub ($x) {
        state @a = 0 xx $P;
        @a.push($x).shift;
        @a.sum / $P;
    }
}

# Usage:
my &sma = sma-generator 3;

for 1, 2, 3, 2, 7 {
    printf "append $_ --> sma = %.2f  (with period 3)\n", sma $_;
}
```


{{out}}

```txt

append 1 --> sma = 0.33  (with period 3)
append 2 --> sma = 1.00  (with period 3)
append 3 --> sma = 2.00  (with period 3)
append 2 --> sma = 2.33  (with period 3)
append 7 --> sma = 4.00  (with period 3)

```



## Phix

First create a separate file sma.e to encapsulate the private variables. Note in particular the complete lack of any special magic/syntax: it is just a table with some indexes.

```Phix

sequence sma = {}       -- {{period,history,circnxt}}  (private to sma.e)
integer sma_free = 0

global function new_sma(integer period)
integer res
    if sma_free then
        res = sma_free
        sma_free = sma[sma_free]
        sma[res] = {period,{},0}
    else
        sma = append(sma,{period,{},0})
        res = length(sma)
    end if
    return res
end function

global procedure add_sma(integer sidx, atom val)
integer period, circnxt
sequence history
    {period,history,circnxt} = sma[sidx]
    sma[sidx][2] = 0 -- (kill refcount)
    if length(history)<period then
        history = append(history,val)
    else
        circnxt += 1
        if circnxt>period then
            circnxt = 1
        end if
        sma[sidx][3] = circnxt
        history[circnxt] = val
    end if
    sma[sidx][2] = history
end procedure

global function get_sma_average(integer sidx)
sequence history = sma[sidx][2]
integer l = length(history)
    if l=0 then return 0 end if
    return sum(history)/l
end function

global function moving_average(integer sidx, atom val)
    add_sma(sidx,val)
    return get_sma_average(sidx)
end function

global procedure free_sma(integer sidx)
    sma[sidx] = sma_free
    sma_free = sidx
end procedure
```

and the main file is:

```Phix
include sma.e

constant sma3 = new_sma(3)
constant sma5 = new_sma(5)
constant s = {1,2,3,4,5,5,4,3,2,1}
integer si

for i=1 to length(s) do
    si = s[i]
    printf(1,"%2g: sma3=%8g, sma5=%8g\n",{si,moving_average(sma3,si),moving_average(sma5,si)})
end for
```

{{out}}

```txt

 1: sma3=       1, sma5=       1
 2: sma3=     1.5, sma5=     1.5
 3: sma3=       2, sma5=       2
 4: sma3=       3, sma5=     2.5
 5: sma3=       4, sma5=       3
 5: sma3= 4.66667, sma5=     3.8
 4: sma3= 4.66667, sma5=     4.2
 3: sma3=       4, sma5=     4.2
 2: sma3=       3, sma5=     3.8
 1: sma3=       2, sma5=       3

```



## PicoLisp


```PicoLisp
(de sma (@Len)
   (curry (@Len (Data)) (N)
      (push 'Data N)
      (and (nth Data @Len) (con @))  # Truncate
      (*/ (apply + Data) (length Data)) ) )
```


```PicoLisp
(def 'sma3 (sma 3))
(def 'sma5 (sma 5))

(scl 2)
(for N (1.0 2.0 3.0 4.0 5.0 5.0 4.0 3.0 2.0 1.0)
   (prinl
      (format N *Scl)
      "   (sma3) "
      (format (sma3 N) *Scl)
      "   (sma5) "
      (format (sma5 N) *Scl) ) )
```

{{out}}

```txt
1.00   (sma3) 1.00   (sma5) 1.00
2.00   (sma3) 1.50   (sma5) 1.50
3.00   (sma3) 2.00   (sma5) 2.00
4.00   (sma3) 3.00   (sma5) 2.50
5.00   (sma3) 4.00   (sma5) 3.00
5.00   (sma3) 4.67   (sma5) 3.80
4.00   (sma3) 4.67   (sma5) 4.20
3.00   (sma3) 4.00   (sma5) 4.20
2.00   (sma3) 3.00   (sma5) 3.80
1.00   (sma3) 2.00   (sma5) 3.00
```



## PL/I


### version 1


```pli
SMA: procedure (N) returns (float byaddr);
   declare N fixed;
   declare A(*) fixed controlled,
           (p, q) fixed binary static initial (0);

   if allocation(A) = 0 then signal error;

   p = p + 1; if q < 20 then q = q + 1;
   if p > hbound(A, 1) then p = 1;
   A(p) = N;
   return (sum(float(A))/q);

I: ENTRY (Period);
   declare Period fixed binary;

   if allocation(A) > 0 then FREE A;
   allocate A(Period);
   A = 0;
   p = 0;
end SMA;
```


### version 2

{{trans|REXX}}

```pli
*process source attributes xref;
 mat: Proc Options(main);
 Dcl a(10) Dec Fixed(8,6);
 Dcl s     Dec Fixed(10,8);
 Dcl n Bin Fixed(31) init(hbound(a)); /* number of items in the list. */
 Dcl p Bin Fixed(31) init(3);         /* the 1st period               */
 Dcl q Bin Fixed(31) init(5);         /* the 2nd period               */
 Dcl m Bin Fixed(31);
 Call i(a);

 Put Edit('            SMA with   SMA with',
          '  number    period 3   period 5',
          ' --------  ---------- ----------')
         (Skip,a);
 Do m=1 To n;
   Put Edit(m,sma(p,m),sma(q,m))(Skip,f(5),2(f(13,6)));
   End;

 i: Proc(a);
 Dcl a(*) Dec Fixed(8,6);
 Dcl (j,m) Bin Fixed(31);
 Do j=1 To hbound(a)/2;
   a(j)=j;                            /* ··· increasing values.       */
   End;
 Do k=hbound(a)/2 To 1 By -1;
   a(j)=k;                            /* ··· decreasing values.       */
   j+=1;
   End;
 End;

 sma: Proc(p,j) Returns(Dec Fixed(8,6));
 Dcl s Dec fixed(8,6) Init(0);
 Dcl i Bin Fixed(31) Init(0);
 Dcl j Bin Fixed(31) Init((hbound(a)+1));
 Dcl (p,i,k,ka,kb) Bin Fixed(31);
   ka=max(1,j-p+1);
   kb=j+p;
   Do k=ka To kb While(k<=j);
     i+=1;
     s+=a(k)
     End;
   s=s/i+0.5e-6;
   Return(s);
 End;
 End;
```

 {{out}}

```txt
            SMA with   SMA with
  number    period 3   period 5
 --------  ---------- ----------
    1     1.000000     1.000000
    2     1.500000     1.500000
    3     2.000000     2.000000
    4     3.000000     2.500000
    5     4.000000     3.000000
    6     4.666667     3.800000
    7     4.666667     4.200000
    8     4.000000     4.200000
    9     3.000000     3.800000
   10     2.000000     3.000000
```



## PureBasic


```PureBasic
Procedure.d SMA(Number, Period=0)
  Static P
  Static NewList L()
  Protected Sum=0
  If Period<>0
    P=Period
  EndIf
  LastElement(L())
  AddElement(L())
  L()=Number
  While ListSize(L())>P
    FirstElement(L())
    DeleteElement(L(),1)
  Wend
  ForEach L()
    sum+L()
  Next
  ProcedureReturn sum/ListSize(L())
EndProcedure
```



## Python

{{Works with|Python|3.x}}

Both implementations use the [http://www.doughellmann.com/PyMOTW/collections/index.html deque] datatype.

### Procedural


```python
from collections import deque

def simplemovingaverage(period):
    assert period == int(period) and period > 0, "Period must be an integer >0"

    summ = n = 0.0
    values = deque([0.0] * period)     # old value queue

    def sma(x):
        nonlocal summ, n

        values.append(x)
        summ += x - values.popleft()
        n = min(n+1, period)
        return summ / n

    return sma
```



### Class based


```python
from collections import deque

class Simplemovingaverage():
    def __init__(self, period):
        assert period == int(period) and period > 0, "Period must be an integer >0"
        self.period = period
        self.stream = deque()

    def __call__(self, n):
        stream = self.stream
        stream.append(n)    # appends on the right
        streamlength = len(stream)
        if streamlength > self.period:
            stream.popleft()
            streamlength -= 1
        if streamlength == 0:
            average = 0
        else:
            average = sum( stream ) / streamlength

        return average
```


'''Tests'''

```python
if __name__ == '__main__':
    for period in [3, 5]:
        print ("\nSIMPLE MOVING AVERAGE (procedural): PERIOD =", period)
        sma = simplemovingaverage(period)
        for i in range(1,6):
            print ("  Next number = %-2g, SMA = %g " % (i, sma(i)))
        for i in range(5, 0, -1):
            print ("  Next number = %-2g, SMA = %g " % (i, sma(i)))
    for period in [3, 5]:
        print ("\nSIMPLE MOVING AVERAGE (class based): PERIOD =", period)
        sma = Simplemovingaverage(period)
        for i in range(1,6):
            print ("  Next number = %-2g, SMA = %g " % (i, sma(i)))
        for i in range(5, 0, -1):
            print ("  Next number = %-2g, SMA = %g " % (i, sma(i)))
```


{{out}}

```txt
SIMPLE MOVING AVERAGE (procedural): PERIOD = 3
  Next number = 1 , SMA = 1
  Next number = 2 , SMA = 1.5
  Next number = 3 , SMA = 2
  Next number = 4 , SMA = 3
  Next number = 5 , SMA = 4
  Next number = 5 , SMA = 4.66667
  Next number = 4 , SMA = 4.66667
  Next number = 3 , SMA = 4
  Next number = 2 , SMA = 3
  Next number = 1 , SMA = 2

SIMPLE MOVING AVERAGE (procedural): PERIOD = 5
  Next number = 1 , SMA = 1
  Next number = 2 , SMA = 1.5
  Next number = 3 , SMA = 2
  Next number = 4 , SMA = 2.5
  Next number = 5 , SMA = 3
  Next number = 5 , SMA = 3.8
  Next number = 4 , SMA = 4.2
  Next number = 3 , SMA = 4.2
  Next number = 2 , SMA = 3.8
  Next number = 1 , SMA = 3

SIMPLE MOVING AVERAGE (class based): PERIOD = 3
  Next number = 1 , SMA = 1
  Next number = 2 , SMA = 1.5
  Next number = 3 , SMA = 2
  Next number = 4 , SMA = 3
  Next number = 5 , SMA = 4
  Next number = 5 , SMA = 4.66667
  Next number = 4 , SMA = 4.66667
  Next number = 3 , SMA = 4
  Next number = 2 , SMA = 3
  Next number = 1 , SMA = 2

SIMPLE MOVING AVERAGE (class based): PERIOD = 5
  Next number = 1 , SMA = 1
  Next number = 2 , SMA = 1.5
  Next number = 3 , SMA = 2
  Next number = 4 , SMA = 2.5
  Next number = 5 , SMA = 3
  Next number = 5 , SMA = 3.8
  Next number = 4 , SMA = 4.2
  Next number = 3 , SMA = 4.2
  Next number = 2 , SMA = 3.8
  Next number = 1 , SMA = 3
```



## R

This is easiest done with two functions: one to handle the state (i.e. the numbers already entered), and one to calculate the average.

```R
#concat concatenates the new values to the existing vector of values, then discards any values that are too old.
lastvalues <- local(
{
   values <- c();
   function(x, len)
   {
      values <<- c(values, x);
      lenv <- length(values);
      if(lenv > len) values <<- values[(len-lenv):-1]
      values
   }
})

#moving.average accepts a numeric scalars input (and optionally a length, i.e. the number of values to retain) and calculates the stateful moving average.
moving.average <- function(latestvalue, len=3)
{
   #Check that all inputs are numeric scalars
   is.numeric.scalar <- function(x) is.numeric(x) && length(x)==1L
   if(!is.numeric.scalar(latestvalue) || !is.numeric.scalar(len))
   {
      stop("all arguments must be numeric scalars")
   }

   #Calculate mean of variables so far
   mean(lastvalues(latestvalue, len))
}
moving.average(5)  # 5
moving.average(1)  # 3
moving.average(-3) # 1
moving.average(8)  # 2
moving.average(7)  # 4
```



## Racket


```Racket
#lang racket

(require data/queue)

(define (simple-moving-average period)
  (define queue (make-queue))
  (define sum 0.0)

  (lambda (x)
    (enqueue! queue x)
    (set! sum (+ sum x))
    (when (> (queue-length queue) period)
      (set! sum (- sum (dequeue! queue))))
    (/ sum (queue-length queue))))

;; Tests
(define sma3 (simple-moving-average 3))
(define sma5 (simple-moving-average 5))
(for/lists (lst1 lst2)
           ([i '(1 2 3 4 5 5 4 3 2 1)])
  (values (sma3 i) (sma5 i)))

```



## REXX

The same list of numbers was used as in the   '''ALGOL68'''   example.

The 1<sup>st</sup> and 2<sup>nd</sup> periods (number of values) were parametrized,   as well as the total number of values.

```rexx
/*REXX program illustrates and displays a simple moving average using a constructed list*/
parse arg p q n .                                /*obtain optional arguments from the CL*/
if p=='' | p==","  then p=  3                    /*Not specified?  Then use the default.*/
if q=='' | q==","  then q=  5                    /* "      "         "   "   "     "    */
if n=='' | n==","  then n= 10                    /* "      "         "   "   "     "    */
@.= 0                                            /*default value, only needed for odd N.*/
      do j=1    for n%2;       @.j= j            /*build 1st half of list, increasing #s*/
      end   /*j*/

      do k=n%2  by -1  to 1;   @.j= k;   j= j+1  /*  "   2nd   "   "   "   decreasing " */
      end   /*k*/

                      say '  number  '     " SMA with period" p' '   " SMA with period" q
                      say ' ──────── '     "───────────────────"     '───────────────────'
                                           pad='     '
      do m=1  for n;  say center(@.m, 10)  pad left(SMA(p, m), 19)     left(SMA(q, m), 19)
      end   /*m*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
SMA: procedure expose @.;  parse arg p,j;                          i= 0    ;    $= 0
                 do k=max(1, j-p+1)  to j+p  for p  while k<=j;    i= i + 1;    $= $ + @.k
                 end   /*k*/
     return $/i                                  /*SMA   ≡   simple moving average.     */
```

{{out|output|text=  when using the generated default input numbers:}}

```txt

  number    SMA with period 3   SMA with period 5
 ────────  ─────────────────── ───────────────────
    1            1                   1
    2            1.5                 1.5
    3            2                   2
    4            3                   2.5
    5            4                   3
    5            4.66666667          3.8
    4            4.66666667          4.2
    3            4                   4.2
    2            3                   3.8
    1            2                   3

```



## Ring


### version 1


```ring

load "stdlib.ring"
decimals(8)
maxperiod = 20
nums = newlist(maxperiod,maxperiod)
accum = list(maxperiod)
index = list(maxperiod)
window = list(maxperiod)
for i = 1 to maxperiod
    index[i] = 1
    accum[i] = 0
    window[i] = 0
next
for i = 1 to maxperiod
    for j = 1 to maxperiod
        nums[i][j] = 0
    next
next
for n = 1 to 5
    see "number = " + n + "  sma3 = " + left((string(sma(n,3)) + "        "),9) + "  sma5 = " + sma(n,5) + nl
next
for n = 5 to 1 step -1
    see "number = " + n + "  sma3 = " + left((string(sma(n,3)) + "        "),9) + "  sma5 = " + sma(n,5) + nl
next
see nl

func sma number, period
     accum[period] += number - nums[period][index[period]]
     nums[period][index[period]] = number
     index[period]= (index[period] + 1) % period + 1
     if window[period]<period window[period] += 1 ok
     return (accum[period] / window[period])

```

Output:

```txt

number = 1  sma3 = 1          sma5 = 1
number = 2  sma3 = 1.5000000  sma5 = 1.50000000
number = 3  sma3 = 2          sma5 = 2
number = 4  sma3 = 3          sma5 = 2.50000000
number = 5  sma3 = 4          sma5 = 3
number = 5  sma3 = 4.6666666  sma5 = 3.80000000
number = 4  sma3 = 4.6666666  sma5 = 4.20000000
number = 3  sma3 = 4          sma5 = 4.20000000
number = 2  sma3 = 3          sma5 = 3.80000000
number = 1  sma3 = 2          sma5 = 3

```



### version 2


```ring

load "stdlib.ring"
decimals(8)
maxperiod = 20
nums = newlist(maxperiod,maxperiod)
accum = list(maxperiod)
index = list(maxperiod)
window = list(maxperiod)
for i = 1 to maxperiod
    index[i] = 1
    accum[i] = 0
    window[i] = 0
next
for i = 1 to maxperiod
    for j = 1 to maxperiod
        nums[i][j] = 0
    next
next
for n = 1 to 5
    see "number = " + n + "  sma3 = " + left((string(sma(n,3)) + "        "),9) + "  sma5 = " + sma(n,5) + nl
next
for n = 5 to 1 step -1
    see "number = " + n + "  sma3 = " + left((string(sma(n,3)) + "        "),9) + "  sma5 = " + sma(n,5) + nl
next
see nl

func sma number, period
accum[period] += number - nums[period][index[period]]
nums[period][index[period]] = number
index[period]= (index[period] + 1) % period + 1
if window[period]<period window[period] += 1 ok
return (accum[period] / window[period])

```

Output:

```txt

number = 1  sma3 = 1          sma5 = 1
number = 2  sma3 = 1.5000000  sma5 = 1.50000000
number = 3  sma3 = 2          sma5 = 2
number = 4  sma3 = 3          sma5 = 2.50000000
number = 5  sma3 = 4          sma5 = 3
number = 5  sma3 = 4.6666666  sma5 = 3.80000000
number = 4  sma3 = 4.6666666  sma5 = 4.20000000
number = 3  sma3 = 4          sma5 = 4.20000000
number = 2  sma3 = 3          sma5 = 3.80000000
number = 1  sma3 = 2          sma5 = 3

```



### version 3


```ring


### RING: Function Moving Average.   Bert Mariani 2016-06-22

###------------------------------
### Data array of Google prices

aGOOGPrices = ["658","675","670","664","664","663","663","662","675","693","689","675",
"636","633","632","607","607","617","617","581","593","570","574","571","575","596",
"596","601","583","635","587","574","552","531","536","502","488","482","490","503",
"507","521","534","525","534","559","552","554","555","555","552","579","580","577",
"575","562","560","559","558","569","573","577","574","559","552","553","560","569",
"582","579","593","598","593","598","593","586","602","591","594","595","603","614",
"620","625","635","627","632","631","620","626","616","606","602","659","683","671",
"670","659","673","679"]

###-------------------------------------------------------------
### CALL the Function:  MovingAverage  arrayOfPrices timePeriod

aGOOGMvgAvg = MovingAverage( aGOOGPrices, 10 )

aGOOGMvgAvg = MovingAverage( aGOOGPrices, 30 )

###-------------------------------------------------------------
### FUNCTION: MovingAverage

Func MovingAverage arrayPrices, timePeriod

    arrayMvgAvg  = []             ### Output Results to this array
    z = len(arrayPrices)          ### array data length
    sumPrices  = 0

    ###--------------------------------
    ### First MAvg Sum 1 to timePeriod
    ###--------------------------------

    for i = 1 to  timePeriod
        sumPrices = sumPrices + arrayPrices[i]
        mvgAvg    = sumPrices / i
        Add( arrayMvgAvg, mvgAvg)
    next

    ###-----------------------------------------------
    ### Second MAvg Sum  timePeriod +1 to End of Data
    ###-----------------------------------------------

    for i = timePeriod + 1 to z
        sumPrices = sumPrices - arrayPrices[i-timePeriod] + arrayPrices[i]
        mvgAvg    = sumPrices / timePeriod
        Add (arrayMvgAvg, mvgAvg
    next

return arrayMvgAvg

###-------------------------------------------------------------
OUTPUT Google Prices moving average using timePeriod = 10

Index 88 CurPrice 631 Sum 17735 MvgAvg 591.17
Index 89 CurPrice 620 Sum 17797 MvgAvg 593.23
Index 90 CurPrice 626 Sum 17854 MvgAvg 595.13
Index 91 CurPrice 616 Sum 17897 MvgAvg 596.57
Index 92 CurPrice 606 Sum 17926 MvgAvg 597.53
Index 93 CurPrice 602 Sum 17954 MvgAvg 598.47
Index 94 CurPrice 659 Sum 18054 MvgAvg 601.80
Index 95 CurPrice 683 Sum 18185 MvgAvg 606.17
Index 96 CurPrice 671 Sum 18303 MvgAvg 610.10
Index 97 CurPrice 670 Sum 18413 MvgAvg 613.77
Index 98 CurPrice 659 Sum 18503 MvgAvg 616.77
Index 99 CurPrice 673 Sum 18594 MvgAvg 619.80
Index 100 CurPrice 679 Sum 18694 MvgAvg 623.13
###-------------------------------------------------------------


```



## Ruby

A closure:

```ruby
def simple_moving_average(size)
  nums = []
  sum = 0.0
  lambda do |hello|
    nums << hello
    goodbye = nums.length > size ? nums.shift : 0
    sum += hello - goodbye
    sum / nums.length
  end
end

ma3 = simple_moving_average(3)
ma5 = simple_moving_average(5)

(1.upto(5).to_a + 5.downto(1).to_a).each do |num|
  printf "Next number = %d, SMA_3 = %.3f, SMA_5 = %.1f\n",
    num, ma3.call(num), ma5.call(num)
end
```


A class

```ruby
class MovingAverager
  def initialize(size)
    @size = size
    @nums = []
    @sum = 0.0
  end
  def <<(hello)
    @nums << hello
    goodbye = @nums.length > @size ? @nums.shift : 0
    @sum += hello - goodbye
    self
  end
  def average
    @sum / @nums.length
  end
  alias to_f average
  def to_s
    average.to_s
  end
end

ma3 = MovingAverager.new(3)
ma5 = MovingAverager.new(5)

(1.upto(5).to_a + 5.downto(1).to_a).each do |num|
  printf "Next number = %d, SMA_3 = %.3f, SMA_5 = %.1f\n",
    num, ma3 << num, ma5 <<num
end
```


## Rust


### Vector Based


```rust
struct SimpleMovingAverage {
    period: usize,
    numbers: Vec<usize>
}

impl SimpleMovingAverage {
    fn new(p: usize) -> SimpleMovingAverage {
        SimpleMovingAverage {
            period: p,
            numbers: Vec::new()
        }
    }

    fn add_number(&mut self, number: usize) -> f64 {
        self.numbers.push(number);

        if self.numbers.len() > self.period {
            self.numbers.remove(0);
        }

        if self.numbers.is_empty() {
            return 0f64;
        }else {
            let sum = self.numbers.iter().fold(0, |acc, x| acc+x);
            return sum as f64 / self.numbers.len() as f64;
        }
    }
}

fn main() {
    for period in [3, 5].iter() {
        println!("Moving average with period {}", period);

        let mut sma = SimpleMovingAverage::new(*period);
        for i in [1, 2, 3, 4, 5, 5, 4, 3, 2, 1].iter() {
            println!("Number: {} | Average: {}", i, sma.add_number(*i));
        }
    }
}
```


===Double-ended Queue Based===

```rust
use std::collections::VecDeque;

struct SimpleMovingAverage {
    period: usize,
    numbers: VecDeque<usize>
}

impl SimpleMovingAverage {
    fn new(p: usize) -> SimpleMovingAverage {
        SimpleMovingAverage {
            period: p,
            numbers: VecDeque::new()
        }
    }

    fn add_number(&mut self, number: usize) -> f64 {
        self.numbers.push_back(number);

        if self.numbers.len() > self.period {
            self.numbers.pop_front();
        }

        if self.numbers.is_empty() {
            return 0f64;
        }else {
            let sum = self.numbers.iter().fold(0, |acc, x| acc+x);
            return sum as f64 / self.numbers.len() as f64;
        }
    }
}

fn main() {
    for period in [3, 5].iter() {
        println!("Moving average with period {}", period);

        let mut sma = SimpleMovingAverage::new(*period);
        for i in [1, 2, 3, 4, 5, 5, 4, 3, 2, 1].iter() {
            println!("Number: {} | Average: {}", i, sma.add_number(*i));
        }
    }
}
```



```txt
Moving average with period 3
Number: 1 | Average: 1
Number: 2 | Average: 1.5
Number: 3 | Average: 2
Number: 4 | Average: 3
Number: 5 | Average: 4
Number: 5 | Average: 4.666666666666667
Number: 4 | Average: 4.666666666666667
Number: 3 | Average: 4
Number: 2 | Average: 3
Number: 1 | Average: 2
Moving average with period 5
Number: 1 | Average: 1
Number: 2 | Average: 1.5
Number: 3 | Average: 2
Number: 4 | Average: 2.5
Number: 5 | Average: 3
Number: 5 | Average: 3.8
Number: 4 | Average: 4.2
Number: 3 | Average: 4.2
Number: 2 | Average: 3.8
Number: 1 | Average: 3

```



## Run Basic


```runbasic
data 1,2,3,4,5,5,4,3,2,1
dim sd(10)                          ' series data
global sd                           ' make it global so we all see it
for i = 1 to 10:read sd(i): next i

x = sma(3)                          ' simple moving average for 3 periods
x = sma(5)                          ' simple moving average for 5 periods

function sma(p)                     ' the simple moving average function
print "----- SMA:";p;" -----"
  for i = 1 to 10
    sumSd = 0
    for j = max((i - p) + 1,1) to i
      sumSd = sumSd + sd(j)         ' sum series data for the period
    next j
  if p > i then p1 = i else p1 = p
  print  sd(i);" sma:";p;" ";sumSd / p1
  next i
end function
```


```txt
----- SMA:3 -----
1 sma:3 1
2 sma:3 1.5
3 sma:3 2
4 sma:3 3
5 sma:3 4
5 sma:3 4.6666665
4 sma:3 4.6666665
3 sma:3 4
2 sma:3 3
1 sma:3 2
----- SMA:5 -----
1 sma:5 1
2 sma:5 1.5
3 sma:5 2
4 sma:5 2.5
5 sma:5 3
5 sma:5 3.79999995
4 sma:5 4.1999998
3 sma:5 4.1999998
2 sma:5 3.79999995
1 sma:5 3
```



## Scala


```scala
class MovingAverage(period: Int) {
  private var queue = new scala.collection.mutable.Queue[Double]()
  def apply(n: Double) = {
    queue.enqueue(n)
    if (queue.size > period)
      queue.dequeue
    queue.sum / queue.size
  }
  override def toString = queue.mkString("(", ", ", ")")+", period "+period+", average "+(queue.sum / queue.size)
  def clear = queue.clear
}
```



```txt

scala> List(3,5) foreach { period =>
     |   println("SIMPLE MOVING AVERAGE: PERIOD = "+period)
     |   val sma = new MovingAverage(period)
     |   1.0 to 5.0 by 1.0 foreach {i => println("  Next number = %-2g, SMA = %g " format (i, sma(i)))}
     |   5.0 to 1.0 by -1.0 foreach {i => println("  Next number = %-2g, SMA = %g " format (i, sma(i)))}
     |   println(sma+"\n")
     | }
SIMPLE MOVING AVERAGE: PERIOD = 3
  Next number = 1.00000, SMA = 1.00000
  Next number = 2.00000, SMA = 1.50000
  Next number = 3.00000, SMA = 2.00000
  Next number = 4.00000, SMA = 3.00000
  Next number = 5.00000, SMA = 4.00000
  Next number = 5.00000, SMA = 4.66667
  Next number = 4.00000, SMA = 4.66667
  Next number = 3.00000, SMA = 4.00000
  Next number = 2.00000, SMA = 3.00000
  Next number = 1.00000, SMA = 2.00000
(3.0, 2.0, 1.0), period 3, average 2.0

SIMPLE MOVING AVERAGE: PERIOD = 5
  Next number = 1.00000, SMA = 1.00000
  Next number = 2.00000, SMA = 1.50000
  Next number = 3.00000, SMA = 2.00000
  Next number = 4.00000, SMA = 2.50000
  Next number = 5.00000, SMA = 3.00000
  Next number = 5.00000, SMA = 3.80000
  Next number = 4.00000, SMA = 4.20000
  Next number = 3.00000, SMA = 4.20000
  Next number = 2.00000, SMA = 3.80000
  Next number = 1.00000, SMA = 3.00000
(5.0, 4.0, 3.0, 2.0, 1.0), period 5, average 3.0

```



## Scheme


```scheme
(define ((simple-moving-averager size . nums) num)
  (set! nums (cons num (if (= (length nums) size) (reverse (cdr (reverse nums))) nums)))
  (/ (apply + nums) (length nums)))

(define av (simple-moving-averager 3))
(map av '(1 2 3 4 5 5 4 3 2 1))

```

{{out}}

```txt

(1 3/2 2 3 4 14/3 14/3 4 3 2)

```




## Sidef

Implemented with closures:

```ruby
func simple_moving_average(period) {

    var list = []
    var sum = 0

    func (number) {
        list.append(number)
        sum += number
        if (list.len > period) {
            sum -= list.shift
        }
        (sum / list.length)
    }
}

var ma3 = simple_moving_average(3)
var ma5 = simple_moving_average(5)

for num (1..5, flip(1..5)) {
  printf("Next number = %d, SMA_3 = %.3f, SMA_5 = %.1f\n",
    num, ma3.call(num), ma5.call(num))
}
```


Implemented as a class:

```ruby
class sma_generator(period, list=[], sum=0) {

    method SMA(number) {
        list.append(number)
        sum += number
        if (list.len > period) {
            sum -= list.shift
        }
        (sum / list.len)
    }
}

var ma3 = sma_generator(3)
var ma5 = sma_generator(5)

for num (1..5, flip(1..5)) {
  printf("Next number = %d, SMA_3 = %.3f, SMA_5 = %.1f\n",
    num, ma3.SMA(num), ma5.SMA(num))
}
```


{{out}}

```txt

Next number = 1, SMA_3 = 1.000, SMA_5 = 1.0
Next number = 2, SMA_3 = 1.500, SMA_5 = 1.5
Next number = 3, SMA_3 = 2.000, SMA_5 = 2.0
Next number = 4, SMA_3 = 3.000, SMA_5 = 2.5
Next number = 5, SMA_3 = 4.000, SMA_5 = 3.0
Next number = 5, SMA_3 = 4.667, SMA_5 = 3.8
Next number = 4, SMA_3 = 4.667, SMA_5 = 4.2
Next number = 3, SMA_3 = 4.000, SMA_5 = 4.2
Next number = 2, SMA_3 = 3.000, SMA_5 = 3.8
Next number = 1, SMA_3 = 2.000, SMA_5 = 3.0

```



## Smalltalk

{{works with|GNU Smalltalk}}


```smalltalk
Object subclass: MovingAverage [
    |valueCollection period collectedNumber sum|
    MovingAverage class >> newWithPeriod: thePeriod [
	|r|
	r := super basicNew.
	^ r initWithPeriod: thePeriod
    ]
    initWithPeriod: thePeriod [
    	valueCollection := OrderedCollection new: thePeriod.
	period := thePeriod.
	collectedNumber := 0.
	sum := 0
    ]
    sma [   collectedNumber < period
            ifTrue: [ ^ sum / collectedNumber ]
            ifFalse: [ ^ sum / period ] ]
    add: value [
        collectedNumber < period
   	ifTrue: [
	   sum := sum + value.
	   valueCollection add: value.
	   collectedNumber := collectedNumber + 1.
	]
	ifFalse: [
	   sum := sum - (valueCollection removeFirst).
	   sum := sum + value.
	   valueCollection add: value
	].
	^ self sma
    ]
].
```



```smalltalk
|sma3 sma5|

sma3 := MovingAverage newWithPeriod: 3.
sma5 := MovingAverage newWithPeriod: 5.

#( 1 2 3 4 5 5 4 3 2 1 ) do: [ :v |
  ('Next number %1, SMA_3 = %2, SMA_5 = %3' % {
         v . (sma3 add: v) asFloat . (sma5 add: v) asFloat
    }) displayNl
]
```



## Tcl

{{works with|Tcl|8.6}} or {{libheader|TclOO}}

```tcl
oo::class create SimpleMovingAverage {
    variable vals idx
    constructor {{period 3}} {
        set idx end-[expr {$period-1}]
        set vals {}
    }
    method val x {
        set vals [lrange [list {*}$vals $x] $idx end]
        expr {[tcl::mathop::+ {*}$vals]/double([llength $vals])}
    }
}
```

Demonstration:

```tcl
SimpleMovingAverage create averager3
SimpleMovingAverage create averager5 5
foreach n {1 2 3 4 5 5 4 3 2 1} {
    puts "Next number = $n, SMA_3 = [averager3 val $n], SMA_5 = [averager5 val $n]"
}
```

{{out}}

```txt
Next number = 1, SMA_3 = 1.0, SMA_5 = 1.0
Next number = 2, SMA_3 = 1.5, SMA_5 = 1.5
Next number = 3, SMA_3 = 2.0, SMA_5 = 2.0
Next number = 4, SMA_3 = 3.0, SMA_5 = 2.5
Next number = 5, SMA_3 = 4.0, SMA_5 = 3.0
Next number = 5, SMA_3 = 4.666666666666667, SMA_5 = 3.8
Next number = 4, SMA_3 = 4.666666666666667, SMA_5 = 4.2
Next number = 3, SMA_3 = 4.0, SMA_5 = 4.2
Next number = 2, SMA_3 = 3.0, SMA_5 = 3.8
Next number = 1, SMA_3 = 2.0, SMA_5 = 3.0
```


=={{header|TI-83 BASIC}}==

Continuously prompts for an input <tt>I</tt>, which is added to the end of a list <tt>L1</tt>.  L1 can be found by pressing "2ND"/"1", and <tt>mean</tt> can be found in "List"/"OPS"

Press <tt>ON</tt> to terminate the program.


```ti83b
:1->C
:While 1
:Prompt I
:C->dim(L1)
:I->L1(C)
:Disp mean(L1)
:1+C->C
:End
```


=={{header|TI-89 BASIC}}==


Function that returns a list containing the averaged data of the supplied argument

```ti89b
movinavg(list,p)
Func
  Local r, i, z

  For i,1,dim(list)
    max(i-p,0)→z
    sum(mid(list,z+1,i-z))/(i-z)→r[i]
  EndFor
  r
EndFunc


```


Program that returns a simple value at each invocation:

```ti89b
movinav2(x_,v_)
Prgm
  If getType(x_)="STR" Then
    {}→list
    v_→p
    Return
  EndIf

  right(augment(list,{x_}),p)→list
  sum(list)/dim(list)→#v_
EndPrgm

```


Example1: Using the function

movinavg({1,2,3,4,5,6,7,8,9,10},5)


list is the list being averaged: {1,2,3,4,5,6,7,8,9,10}

p is the period: 5

returns the averaged list: {1, 3/2, 2, 5/2, 3, 4, 5, 6, 7, 8}



Example 2: Using the program

movinav2("i",5)  - Initializing moving average calculation, and define period of 5

movinav2(3, "x"):x - new data in the list (value 3), and result will be stored on variable x, and displayed

movinav2(4, "x"):x - new data (value 4), and the new result will be stored on variable x, and displayed (4+3)/2

...




Description of the function movinavg:

variable r - is the result (the averaged list) that will be returned

variable i - is the index variable, and it points to the end of the sub-list the list being averaged.

variable z - an helper variable



The function uses variable i to determine which values of the list will be considered in the next average calculation.

At every iteration, variable i points to the last value in the list that will be used in the average calculation.

So we only need to figure out which will be the first value in the list.

Usually we'll have to consider p elements, so the first element will be the one indexed by (i-p+1).

However on the first iterations that calculation will usually be negative, so the following equation will avoid negative indexes: max(i-p+1,1) or, arranging the equation, max(i-p,0)+1.

But the number of elements on the first iterations will also be smaller, the correct value will be (end index - begin index + 1)   or, arranging the equation,   (i - (max(i-p,0)+1) +1) ,and then,  (i-max(i-p,0)).

Variable z holds the common value (max(i-p),0) so the begin_index will be (z+1) and the number_of_elements will be (i-z)


mid(list,z+1, i-z) will return the list of value that will be averaged

sum(...) will sum them

sum(...)/(i-z) → r[i] will average them and store the result in the appropriate place in the result list



## VBA

This is a "simple" moving average.

```vb
Class sma
'to be stored in a class module with name "sma"
Private n As Integer 'period
Private arr() As Double 'circular list
Private index As Integer 'pointer into arr
Private oldsma As Double

Public Sub init(size As Integer)
    n = size
    ReDim arr(n - 1)
    index = 0
End Sub

Public Function sma(number As Double) As Double
    sma = oldsma + (-arr(index) + number) / n
    oldsma = sma
    arr(index) = number
    index = (index + 1) Mod n
End Function

Normal module
Public Sub main()
    s = [{1,2,3,4,5,5,4,3,2,1}]
    Dim sma3 As New sma
    Dim sma5 As New sma
    sma3.init 3
    sma5.init 5
    For i = 1 To UBound(s)
        Debug.Print i, Format(sma3.sma(CDbl(s(i))), "0.00000"),
        Debug.Print Format(sma5.sma(CDbl(s(i))), "0.00000")
    Next i
End Sub
```
{{out}}

```txt
 1            0,33333       0,20000
 2            1,00000       0,60000
 3            2,00000       1,20000
 4            3,00000       2,00000
 5            4,00000       3,00000
 6            4,66667       3,80000
 7            4,66667       4,20000
 8            4,00000       4,20000
 9            3,00000       3,80000
 10           2,00000       3,00000
```


## VBScript


```vb
data = "1,2,3,4,5,5,4,3,2,1"
token = Split(data,",")
stream = ""
WScript.StdOut.WriteLine "Number" & vbTab & "SMA3" & vbTab & "SMA5"
For j = LBound(token) To UBound(token)
	If Len(stream) = 0 Then
		stream = token(j)
	Else
		stream = stream & "," & token(j)
	End If
	WScript.StdOut.WriteLine token(j) & vbTab & Round(SMA(stream,3),2) & vbTab & Round(SMA(stream,5),2)
Next

Function SMA(s,p)
	If Len(s) = 0 Then
		SMA = 0
		Exit Function
	End If
	d = Split(s,",")
	sum = 0
	If UBound(d) + 1 >= p Then
		c = 0
		For i = UBound(d) To LBound(d) Step -1
			sum = sum + Int(d(i))
			c = c + 1
			If c = p Then
				Exit For
			End If
		Next
		SMA = sum / p
	Else
		For i = UBound(d) To LBound(d) Step -1
			sum = sum + Int(d(i))
		Next
		SMA = sum / (UBound(d) + 1)
	End If
End Function
```


{{Out}}

```txt

Number	        SMA3	        SMA5
1		1		1
2		1.5		1.5
3		2		2
4		3		2.5
5		4		3
5		4.67	        3.8
4		4.67	        4.2
3		4		4.2
2		3		3.8
1		2		3

```



## zkl


```zkl
fcn SMA(P){
   fcn(n,ns,P){
      sz:=ns.append(n.toFloat()).len();
      if(P>sz) return(0.0);
      if(P<sz) ns.del(0);
      ns.sum(0.0)/P;
   }.fp1(List.createLong(P+1),P)  // pre-allocate a list of length P+1
}
```

fp1 creates a partial application fixing the (in this case) the second and third
parameters

```zkl
T(1,2,3,4,5,5,4,3,2,1).apply(SMA(3)).println();
T(1,2,3,4,5,5,4,3,2,1).apply(SMA(5)).println();
```

{{out}}

```txt

L(0,0,2,3,4,4.66667,4.66667,4,3,2)
L(0,0,0,0,3,3.8,4.2,4.2,3.8,3)

```

