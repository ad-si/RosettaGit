+++
title = "Knapsack problem/0-1"
description = ""
date = 2019-08-08T07:31:10Z
aliases = []
[extra]
id = 6039
[taxonomies]
categories = []
tags = []
+++

{{task|Classic CS problems and programs}}
[[Category:Memoization]]
[[Category:Puzzles]]
{{omit from|GUISS}}

A tourist wants to make a good trip at the weekend with his friends.

They will go to the mountains to see the wonders of nature, so he needs to pack well for the trip.

He has a good knapsack for carrying things, but knows that he can carry a maximum of only 4kg in it,    and it will have to last the whole day.

He creates a list of what he wants to bring for the trip but the total weight of all items is too much.

He then decides to add columns to his initial list detailing their weights and a numerical value representing how important the item is for the trip.


Here is the list:
{| style="text-align: left; width: 80%;" border="4" cellpadding="2" cellspacing="2"
|+ Table of potential knapsack items
|- style="background-color: rgb(255, 204, 255);"
! item !! weight (dag) !! value
|-
| map || 9 || 150
|-
| compass || 13 || 35
|-
| water || 153 || 200
|-
| sandwich || 50 || 160
|-
| glucose || 15 || 60
|-
| tin || 68 || 45
|-
| banana || 27 || 60
|-
| apple || 39 || 40
|-
| cheese || 23 || 30
|-
| beer || 52 || 10
|-
| suntan cream || 11 || 70
|-
| camera || 32 || 30
|-
| T-shirt || 24 || 15
|-
| trousers || 48 || 10
|-
| umbrella || 73 || 40
|-
| waterproof trousers || 42 || 70
|-
| waterproof overclothes || 43 || 75
|-
| note-case || 22 || 80
|-
| sunglasses || 7 || 20
|-
| towel || 18 || 12
|-
| socks || 4 || 50
|-
| book || 30 || 10
|- style="background-color: rgb(255, 204, 255);"
| knapsack || &le;400 dag || ?
|}



The tourist can choose to take any combination of items from the list,
but only one of each item is available.

He may not cut or diminish the items, so he can only take whole units of any item.


;Task:
Show which items the tourist can carry in his knapsack so that their total weight does not
exceed 400 dag [4 kg],   and their total value is maximized.

[dag = decagram = 10 grams]


;Related tasks:
*   [[Knapsack problem/Bounded]]
*   [[Knapsack problem/Unbounded]]
*   [[Knapsack problem/Continuous]]
*   [[A* search algorithm]]





## 360 Assembly

Non recurvive brute force version.

```360asm
*      Knapsack problem/0-1      16/02/2017
KNAPSA01 CSECT
       USING  KNAPSA01,R13
       B      72(R15)
       DC     17F'0'
       STM    R14,R12,12(R13)
       ST     R13,4(R15)
       ST     R15,8(R13)
       LR     R13,R15            end of prolog
       L      R0,N               n
       LA     R1,1
POWER  MH     R1,=H'2'           *2
       BCT    R0,POWER
       BCTR   R1,0               -1
       ST     R1,IMAX            imax=2**n-1
       SR     R6,R6              i=0
       DO WHILE=(C,R6,LE,IMAX)   do i=0 to imax
         SR     R10,R10            im=0
         SR     R8,R8              iw=0
         SR     R9,R9              iv=0
         LA     R7,1               j=1
         DO WHILE=(C,R7,LE,N)      do j=1 to n
           LR     R1,R6              i
           LR     R2,R7              j
           BAL    R14,TSTBIT         call tstbit(i,j)
           IF C,R0,EQ,=F'1' THEN     if tstbit(i,j)=1 then
             LA     R10,1(R10)         im=im+1
             LR     R3,R7              j
             BCTR   R3,0
             SLA    R3,5
             LA     R1,24(R3)
             A      R8,DATA(R1)        iw=iw+data(j).w
             LA     R1,28(R3)
             A      R9,DATA(R1)        iv=iv+data(j).v
           ENDIF  ,                  endif
           LA     R7,1(R7)           j=j+1
         ENDDO  ,                  enddo j
         IF C,R8,LE,MAXW,AND,C,R9,GT,XV THEN  if w<=maxw and iv>xv then
           ST     R6,XB              xb=i
           ST     R10,XM             xm=im
           ST     R8,XW              xw=iw
           ST     R9,XV              xv=iv
         ENDIF  ,                  endif
         LA     R6,1(R6)           i=i+1
       ENDDO  ,                  enddo i
       MVC    PG(2),=C'n='
       L      R1,N               n
       XDECO  R1,XDEC            edit n
       MVC    PG+2(2),XDEC+10
       XPRNT  PG,L'PG            print buffer
       LA     R6,1
       DO WHILE=(C,R6,LE,N)      do i=1 to n
         L      R1,XB              xb
         LR     R2,R6              i
         BAL    R14,TSTBIT         call tstbit(xb,i)
         IF C,R0,EQ,=F'1' THEN     if tstbit(xb,i)=1 then
           LR     R1,R6              i
           BCTR   R1,0
           SLA    R1,5
           LA     R2,DATA(R1)        @data(i).n
           MVC    PG(24),0(R2)
           XPRNT  PG,24              print item
         ENDIF  ,                  endif
         LA     R6,1(R6)           i=i+1
       ENDDO  ,                  enddo i
       L      R1,XM              xm
       XDECO  R1,XDEC            edit xm
       MVC    PGT+6(2),XDEC+10
       L      R1,XW              xw
       XDECO  R1,XDEC            edit xw
       MVC    PGT+16(3),XDEC+9
       L      R1,XV              xv
       XDECO  R1,XDEC            edit xv
       MVC    PGT+26(4),XDEC+8
       XPRNT  PGT,L'PGT          print buffer
       L      R13,4(0,R13)       epilog
       LM     R14,R12,12(R13)
       XR     R15,R15
       BR     R14                exit
TSTBIT EQU    *                  R1 value to test the R2 bit
       LA     R3,32              32
       SR     R3,R2              (32-i)
       STC    R3,XSLL+3
       LR     R0,R1              n
       EX     0,XSLL             SLL R0,(32-i)
       SRL    R0,31
       BR     R14                return R0
XSLL   SLL    R0,0               shift left logical
*
MAXW   DC     F'400'             maximum weight
N      DC     A((DATAE-DATA)/32)
IMAX   DS     F                  number of combinations
XB     DS     F                  max vector
XM     DS     F                  max items
XW     DS     F                  max weight
XV     DS     F                  max value
PG     DC     CL80' '
PGT    DC     CL32'items=.. weight=... value=....'
XDEC   DS     CL12
DATA   DC     CL24'map',F'9',F'150'
       DC     CL24'compass',F'13',F'35'
       DC     CL24'water',F'153',F'200'
       DC     CL24'sandwich',F'50',F'160'
       DC     CL24'glucose',F'15',F'60'
       DC     CL24'tin',F'68',F'45'
       DC     CL24'banana',F'27',F'60'
       DC     CL24'apple',F'39',F'40'
       DC     CL24'cheese',F'23',F'30'
       DC     CL24'beer',F'52',F'10'
       DC     CL24'suntan cream',F'11',F'70'
       DC     CL24'camera',F'32',F'30'
       DC     CL24'T-shirt',F'24',F'15'
       DC     CL24'trousers',F'48',F'10'
       DC     CL24'umbrella',F'73',F'40'
       DC     CL24'book',F'30',F'10'
       DC     CL24'waterproof trousers',F'42',F'70'
       DC     CL24'waterproof overclothes',F'43',F'75'
       DC     CL24'note-case',F'22',F'80'
       DC     CL24'sunglasses',F'7',F'20'
       DC     CL24'towel',F'18',F'12'
       DC     CL24'socks',F'4',F'50'
DATAE  DC     0C
       YREGS
       END    KNAPSA01
```

{{out}}

```txt

n=22
map
compass
water
sandwich
glucose
banana
suntan cream
waterproof trousers
waterproof overclothes
note-case
sunglasses
socks
items=12 weight=396 value=1030

```



## Ada


```Ada
with Ada.Text_IO;
with Ada.Strings.Unbounded;

procedure Knapsack_01 is
   package US renames Ada.Strings.Unbounded;

   type Item is record
      Name   : US.Unbounded_String;
      Weight : Positive;
      Value  : Positive;
      Taken  : Boolean;
   end record;

   type Item_Array is array (Positive range <>) of Item;

   function Total_Weight (Items : Item_Array; Untaken : Boolean := False) return Natural is
      Sum : Natural := 0;
   begin
      for I in Items'Range loop
         if Untaken or else Items (I).Taken then
            Sum := Sum + Items (I).Weight;
         end if;
      end loop;
      return Sum;
   end Total_Weight;

   function Total_Value (Items : Item_Array; Untaken : Boolean := False) return Natural is
      Sum : Natural := 0;
   begin
      for I in Items'Range loop
         if Untaken or else Items (I).Taken then
            Sum := Sum + Items (I).Value;
         end if;
      end loop;
      return Sum;
   end Total_Value;

   function Max (Left, Right : Natural) return Natural is
   begin
      if Right > Left then
         return Right;
      else
         return Left;
      end if;
   end Max;

   procedure Solve_Knapsack_01 (Items : in out Item_Array;
                                Weight_Limit : Positive := 400) is
      type W_Array is array (0..Items'Length, 0..Weight_Limit) of Natural;
      W : W_Array := (others => (others => 0));
   begin
      -- fill W
      for I in Items'Range loop
         for J in 1 .. Weight_Limit loop
            if Items (I).Weight > J then
               W (I, J) := W (I - 1, J);
            else
               W (I, J) := Max (W (I - 1, J),
                                W (I - 1, J - Items (I).Weight) + Items (I).Value);
            end if;
         end loop;
      end loop;
      declare
         Rest : Natural := Weight_Limit;
      begin
         for I in reverse Items'Range loop
            if W (I, Rest) /= W (I - 1, Rest) then
               Items (I).Taken := True;
               Rest := Rest - Items (I).Weight;
            end if;
         end loop;
      end;
   end Solve_Knapsack_01;

   All_Items : Item_Array :=
     ( (US.To_Unbounded_String ("map"),                      9, 150, False),
       (US.To_Unbounded_String ("compass"),                 13,  35, False),
       (US.To_Unbounded_String ("water"),                  153, 200, False),
       (US.To_Unbounded_String ("sandwich"),                50, 160, False),
       (US.To_Unbounded_String ("glucose"),                 15,  60, False),
       (US.To_Unbounded_String ("tin"),                     68,  45, False),
       (US.To_Unbounded_String ("banana"),                  27,  60, False),
       (US.To_Unbounded_String ("apple"),                   39,  40, False),
       (US.To_Unbounded_String ("cheese"),                  23,  30, False),
       (US.To_Unbounded_String ("beer"),                    52,  10, False),
       (US.To_Unbounded_String ("suntan cream"),            11,  70, False),
       (US.To_Unbounded_String ("camera"),                  32,  30, False),
       (US.To_Unbounded_String ("t-shirt"),                 24,  15, False),
       (US.To_Unbounded_String ("trousers"),                48,  10, False),
       (US.To_Unbounded_String ("umbrella"),                73,  40, False),
       (US.To_Unbounded_String ("waterproof trousers"),     42,  70, False),
       (US.To_Unbounded_String ("waterproof overclothes"),  43,  75, False),
       (US.To_Unbounded_String ("note-case"),               22,  80, False),
       (US.To_Unbounded_String ("sunglasses"),               7,  20, False),
       (US.To_Unbounded_String ("towel"),                   18,  12, False),
       (US.To_Unbounded_String ("socks"),                    4,  50, False),
       (US.To_Unbounded_String ("book"),                    30,  10, False) );

begin
   Solve_Knapsack_01 (All_Items, 400);
   Ada.Text_IO.Put_Line ("Total Weight: " & Natural'Image (Total_Weight (All_Items)));
   Ada.Text_IO.Put_Line ("Total Value:  " & Natural'Image (Total_Value  (All_Items)));
   Ada.Text_IO.Put_Line ("Items:");
   for I in All_Items'Range loop
      if All_Items (I).Taken then
         Ada.Text_IO.Put_Line ("   " & US.To_String (All_Items (I).Name));
      end if;
   end loop;
end Knapsack_01;
```

{{out}}

```txt

Total Weight:  396
Total Value:   1030
Items:
   map
   compass
   water
   sandwich
   glucose
   banana
   suntan cream
   waterproof trousers
   waterproof overclothes
   note-case
   sunglasses
   socks

```



## APL


```APL
    ∇ ret←NapSack;sum;b;list;total
[1]   total←400
[2]   list←("map" 9 150)("compass" 13 35)("water" 153 200)("sandwich" 50 160)("glucose" 15 60) ("tin" 68 45)("banana" 27 60)("apple" 39 40)("cheese" 23 30)("beer" 52 10) ("suntan cream" 11 70)("camera" 32 30)("t-shirt" 24 15)("trousers" 48 10) ("umbrella" 73 40)("waterproof trousers" 42 70)("waterproof overclothes" 43 75) ("note-case" 22 80) ("sunglasses" 7 20) ("towel" 18 12) ("socks" 4 50) ("book" 30 10)
[3]   list←list[⍒3⊃¨list]
[4]
[5]   ret←⍬
[6]   :while 0≠⍴list
[7]       ret←ret,(b←total>sum←+\2⊃¨list)/list
[8]       list←1↓(~b)/list
[9]       total←total-sum←¯1↑(total>sum)/sum
[10]  :end
[11]  ret←⊃ret,⊂'TOTALS:' (+/2⊃¨ret)(+/3⊃¨ret)
    ∇
```

{{out}}

```txt

NapSack
 water                  153  200
 sandwich                50  160
 map                      9  150
 note-case               22   80
 waterproof overclothes  43   75
 suntan cream            11   70
 waterproof trousers     42   70
 glucose                 15   60
 banana                  27   60
 socks                    4   50
 compass                 13   35
 sunglasses               7   20
 TOTALS:                396 1030

Average runtime: 0.000168 seconds

```


## AWK


```AWK

# syntax: GAWK -f KNAPSACK_PROBLEM_0-1.AWK
BEGIN {
#   arr["item,weight"] = value
    arr["map,9"] = 150
    arr["compass,13"] = 35
    arr["water,153"] = 200
    arr["sandwich,50"] = 160
    arr["glucose,15"] = 60
    arr["tin,68"] = 45
    arr["banana,27"] = 60
    arr["apple,39"] = 40
    arr["cheese,23"] = 30
    arr["beer,52"] = 10
    arr["suntan cream,11"] = 70
    arr["camera,32"] = 30
    arr["T-shirt,24"] = 15
    arr["trousers,48"] = 10
    arr["umbrella,73"] = 40
    arr["waterproof trousers,42"] = 70
    arr["waterproof overclothes,43"] = 75
    arr["note-case,22"] = 80
    arr["sunglasses,7"] = 20
    arr["towel,18"] = 12
    arr["socks,4"] = 50
    arr["book,30"] = 10
    sack_size = 400 # dag
    PROCINFO["sorted_in"] = "@val_num_desc"
    for (i in arr) {
      if (total_weight >= sack_size) {
        break
      }
      split(i,tmp,",")
      weight = tmp[2]
      if (total_weight + weight <= sack_size) {
        printf("%s\n",tmp[1])
        total_items++
        total_value += arr[i]
        total_weight += weight
      }
    }
    printf("items=%d (out of %d) weight=%d value=%d\n",total_items,length(arr),total_weight,total_value)
    exit(0)
}

```

{{out}}

```txt

water
sandwich
map
note-case
waterproof overclothes
waterproof trousers
suntan cream
banana
glucose
socks
compass
sunglasses
items=12 (out of 22) weight=396 value=1030

```



## Batch File


```dos

:: Initiate command line environment
@echo off
setlocal enabledelayedexpansion

:: Establish arrays we'll be using
set items=map compass water sandwich glucose tin banana apple cheese beer suntancream camera tshirt trousers umbrella waterprooftrousers waterproofoverclothes notecase sunglasses towel socks book
set weight=9 13 153 50 15 68 27 39 23 52 11 32 24 48 73 42 43 22 7 18 4 30
set importance=150 35 200 160 60 45 60 40 30 10 70 30 15 10 40 70 75 80 20 12 50 10

:: Put the above 3 arrays into their own variables with the form of "item[]", "w[]" and "i[]"
set tempnum=0
for %%i in (%items%) do (
  set /a tempnum+=1
  set item!tempnum!=%%i
)
set tempnum=0
for %%i in (%weight%) do (
  set /a tempnum+=1
  set w!tempnum!=%%i
)
set tempnum=0
for %%i in (%importance%) do (
  set /a tempnum+=1
  set i!tempnum!=%%i
)
:: Define the array "r[]" as the ratio between the importance ("i[]") and the weight ("w[]").
for /l %%i in (1,1,22) do set /a r%%i=!i%%i!*100/!w%%i! & rem batch doesn't support decimals, so the numerator is multiplied by 100 to get past this

set totalimportance=0
set totalweight=0
set amount=0

:: Find the largest number in "r[]" and define some temp variables based off it
:load
set tempr=0
set tempitem=0
for /l %%i in (1,1,22) do (
  if !r%%i! gtr !tempr! (
    set tempr=!r%%i!
    set tempitem=%%i
    set /a testweight=%totalweight%+!w%%i!
    if !tempr!==0 goto end
    if !testweight! geq 400 goto end
  )
)

:: Do basic error checking using the temp variables from above and either output and end the program or send back to load
set /a totaltempweight=%totalweight%+!w%tempitem%!

if %totaltempweight% gtr 400 (
  set !r%tempitem%!=0
  goto load
)

set totalweight=%totaltempweight%
set /a totalimportance+=!i%tempitem%!
set taken=%taken% !item%tempitem%!
set /a amount+=1
set r%tempitem%=0 & rem set the ratio variable of the item we just added to the knapsack as 0 to stop it repeat

goto load

:end
echo List of things taken [%amount%]: %taken%
echo Total Value: %totalimportance%  Total Weight: %totalweight%
pause>nul

```


{{out}}

```txt

List of things taken [12]:  map socks suntancream glucose notecase sandwich sunglasses compass banana waterproofoverclothes waterprooftrousers water
Total Value: 1030  Total Weight: 396

```



## BBC BASIC

{{works with|BBC BASIC for Windows}}

```bbcbasic
      HIMEM = PAGE + 8000000
      nItems% = 22
      maxWeight% = 400

      DIM Tag{ivalue%, list%(nItems%-1), lp%}
      DIM items{(nItems%-1)name$, weight%, ivalue%}
      FOR item% = 0 TO nItems%-1
        READ items{(item%)}.name$, items{(item%)}.weight%, items{(item%)}.ivalue%
      NEXT

      DATA "map", 9, 150, "compass", 13, 35, "water", 153, 200, "sandwich", 50, 160
      DATA "glucose", 15, 60, "tin", 68, 45, "banana", 27, 60, "apple", 39, 40
      DATA "cheese", 23, 30, "beer", 52, 10, "suntan cream", 11, 70, "camera", 32, 30
      DATA "t-shirt", 24, 15, "trousers", 48, 10, "umbrella", 73, 40, "book", 30, 10
      DATA "waterproof trousers", 42, 70, "waterproof overclothes", 43, 75
      DATA "note-case", 22, 80, "sunglasses", 7, 20, "towel", 18, 12, "socks", 4, 50

      carry% = FN_Knapsack(items{()}, nItems% - 1, maxWeight%, cache{()})
      FOR i% = 0 TO cache{(carry%)}.lp%-1
        n% = cache{(carry%)}.list%(i%)
        TotalWeight% += items{(n%)}.weight%
        TotalValue% += items{(n%)}.ivalue%
        PRINT items{(n%)}.name$ " "
      NEXT
      PRINT '"Total weight = " ; TotalWeight%
      PRINT "Total value  = " ; TotalValue%
      END

      DEF FN_Knapsack(i{()}, i%, w%, RETURN m{()})
      LOCAL included{}, excluded{}, tmp%, index%
      DIM m{(16384)} = Tag{}, included{} = Tag{}, excluded{} = Tag{}

      index% = i% << 9 OR w%
      IF m{(index%)}.ivalue% THEN = index%

      IF i% = 0 THEN
        IF i{(0)}.weight% > w% THEN
          m{(index%)}.ivalue% = 0 : REM Item doesn't fit
        ELSE
          m{(index%)}.ivalue% = i{(0)}.ivalue%
          m{(index%)}.list%(m{(index%)}.lp%) = 0
          m{(index%)}.lp% += 1
        ENDIF
        = index%
      ENDIF

      tmp% = FN_Knapsack(i{()}, i% - 1, w%, m{()})
      excluded{} = m{(tmp%)}
      IF i{(i%)}.weight% > w% THEN
        m{(index%)} = excluded{} : REM Item weighs too much
        = index%
      ELSE
        tmp% = FN_Knapsack(i{()}, i% - 1, w% - i{(i%)}.weight%, m{()})
        included{} = m{(tmp%)}
        included.ivalue% += i{(i%)}.ivalue%
        included.list%(included.lp%) = i%
        included.lp% += 1
      ENDIF

      IF included.ivalue% > excluded.ivalue% THEN
        m{(index%)} = included{}
      ELSE
        m{(index%)} = excluded{}
      ENDIF
      = index%
```

{{out}}

```txt

map
compass
water
sandwich
glucose
banana
suntan cream
waterproof trousers
waterproof overclothes
note-case
sunglasses
socks

Total weight = 396
Total value  = 1030

```



## Bracmat


```bracmat
(knapsack=
  ( things
  =   (map.9.150)
      (compass.13.35)
      (water.153.200)
      (sandwich.50.160)
      (glucose.15.60)
      (tin.68.45)
      (banana.27.60)
      (apple.39.40)
      (cheese.23.30)
      (beer.52.10)
      ("suntan cream".11.70)
      (camera.32.30)
      (T-shirt.24.15)
      (trousers.48.10)
      (umbrella.73.40)
      ("waterproof trousers".42.70)
      ("waterproof overclothes".43.75)
      (note-case.22.80)
      (sunglasses.7.20)
      (towel.18.12)
      (socks.4.50)
      (book.30.10)
  )
& 0:?maxvalue
& :?sack
& ( add
  =     cumwght
        cumvalue
        cumsack
        name
        wght
        val
        tings
        n
        ncumwght
        ncumvalue
    .     !arg
        : (?cumwght.?cumvalue.?cumsack.(?name.?wght.?val) ?tings)
      & -1:?n
      &   whl
        ' ( 1+!n:~>1:?n
          & !cumwght+!n*!wght:~>400:?ncumwght
          & !cumvalue+!n*!val:?ncumvalue
          & (   !tings:
              & (   !ncumvalue:>!maxvalue:?maxvalue
                  &     !cumsack
                        (!n:0&|!name)
                    : ?sack
                |
                )
            |   add
              $ ( !ncumwght
                . !ncumvalue
                .   !cumsack
                    (!n:0&|!name)
                . !tings
                )
            )
          )
  )
& add$(0.0..!things)
& out$(!maxvalue.!sack));

!knapsack;
```

{{out}}

```bracmat
  1030
.   map
    compass
    water
    sandwich
    glucose
    banana
    suntan cream
    waterproof trousers
    waterproof overclothes
    note-case
    sunglasses
    socks
```



## C



```c
#include <stdio.h>
#include <stdlib.h>

typedef struct {
    char *name;
    int weight;
    int value;
} item_t;

item_t items[] = {
    {"map",                      9,   150},
    {"compass",                 13,    35},
    {"water",                  153,   200},
    {"sandwich",                50,   160},
    {"glucose",                 15,    60},
    {"tin",                     68,    45},
    {"banana",                  27,    60},
    {"apple",                   39,    40},
    {"cheese",                  23,    30},
    {"beer",                    52,    10},
    {"suntan cream",            11,    70},
    {"camera",                  32,    30},
    {"T-shirt",                 24,    15},
    {"trousers",                48,    10},
    {"umbrella",                73,    40},
    {"waterproof trousers",     42,    70},
    {"waterproof overclothes",  43,    75},
    {"note-case",               22,    80},
    {"sunglasses",               7,    20},
    {"towel",                   18,    12},
    {"socks",                    4,    50},
    {"book",                    30,    10},
};

int *knapsack (item_t *items, int n, int w) {
    int i, j, a, b, *mm, **m, *s;
    mm = calloc((n + 1) * (w + 1), sizeof (int));
    m = malloc((n + 1) * sizeof (int *));
    m[0] = mm;
    for (i = 1; i <= n; i++) {
        m[i] = &mm[i * (w + 1)];
        for (j = 0; j <= w; j++) {
            if (items[i - 1].weight > j) {
                m[i][j] = m[i - 1][j];
            }
            else {
                a = m[i - 1][j];
                b = m[i - 1][j - items[i - 1].weight] + items[i - 1].value;
                m[i][j] = a > b ? a : b;
            }
        }
    }
    s = calloc(n, sizeof (int));
    for (i = n, j = w; i > 0; i--) {
        if (m[i][j] > m[i - 1][j]) {
            s[i - 1] = 1;
            j -= items[i - 1].weight;
        }
    }
    free(mm);
    free(m);
    return s;
}

int main () {
    int i, n, tw = 0, tv = 0, *s;
    n = sizeof (items) / sizeof (item_t);
    s = knapsack(items, n, 400);
    for (i = 0; i < n; i++) {
        if (s[i]) {
            printf("%-22s %5d %5d\n", items[i].name, items[i].weight, items[i].value);
            tw += items[i].weight;
            tv += items[i].value;
        }
    }
    printf("%-22s %5d %5d\n", "totals:", tw, tv);
    return 0;
}

```

{{out}}

```txt

map                        9   150
compass                   13    35
water                    153   200
sandwich                  50   160
glucose                   15    60
banana                    27    60
suntan cream              11    70
waterproof trousers       42    70
waterproof overclothes    43    75
note-case                 22    80
sunglasses                 7    20
socks                      4    50
totals:                  396  1030

```



## C++


```cpp
#include <vector>
#include <string>
#include <iostream>
#include <boost/tuple/tuple.hpp>
#include <set>

int findBestPack( const std::vector<boost::tuple<std::string , int , int> > & ,
      std::set<int> & , const int  ) ;

int main( ) {
   std::vector<boost::tuple<std::string , int , int> > items ;
   //
### ========fill the vector with data=================

   items.push_back( boost::make_tuple( "" , 0  ,  0 ) ) ;
   items.push_back( boost::make_tuple( "map" , 9 , 150 ) ) ;
   items.push_back( boost::make_tuple( "compass" , 13 , 35 ) ) ;
   items.push_back( boost::make_tuple( "water" , 153 , 200 ) ) ;
   items.push_back( boost::make_tuple( "sandwich", 50 , 160 ) ) ;
   items.push_back( boost::make_tuple( "glucose" , 15 , 60 ) ) ;
   items.push_back( boost::make_tuple( "tin", 68 , 45 ) ) ;
   items.push_back( boost::make_tuple( "banana", 27 , 60 ) ) ;
   items.push_back( boost::make_tuple( "apple" , 39 , 40 ) ) ;
   items.push_back( boost::make_tuple( "cheese" , 23 , 30 ) ) ;
   items.push_back( boost::make_tuple( "beer" , 52 , 10 ) ) ;
   items.push_back( boost::make_tuple( "suntan creme" , 11 , 70 ) ) ;
   items.push_back( boost::make_tuple( "camera" , 32 , 30 ) ) ;
   items.push_back( boost::make_tuple( "T-shirt" , 24 , 15 ) ) ;
   items.push_back( boost::make_tuple( "trousers" , 48 , 10 ) ) ;
   items.push_back( boost::make_tuple( "umbrella" , 73 , 40 ) ) ;
   items.push_back( boost::make_tuple( "waterproof trousers" , 42 , 70 ) ) ;
   items.push_back( boost::make_tuple( "waterproof overclothes" , 43 , 75 ) ) ;
   items.push_back( boost::make_tuple( "note-case" , 22 , 80 ) ) ;
   items.push_back( boost::make_tuple( "sunglasses" , 7 , 20 ) ) ;
   items.push_back( boost::make_tuple( "towel" , 18 , 12 ) ) ;
   items.push_back( boost::make_tuple( "socks" , 4 , 50 ) ) ;
   items.push_back( boost::make_tuple( "book" , 30 , 10 ) ) ;
   const int maximumWeight = 400 ;
   std::set<int> bestItems ; //these items will make up the optimal value
   int bestValue = findBestPack( items , bestItems , maximumWeight ) ;
   std::cout << "The best value that can be packed in the given knapsack is " <<
      bestValue << " !\n" ;
   int totalweight = 0 ;
   std::cout << "The following items should be packed in the knapsack:\n" ;
   for ( std::set<int>::const_iterator si = bestItems.begin( ) ;
	 si != bestItems.end( ) ; si++ ) {
      std::cout << (items.begin( ) + *si)->get<0>( ) << "\n" ;
      totalweight += (items.begin( ) + *si)->get<1>( ) ;
   }
   std::cout << "The total weight of all items is " << totalweight << " !\n" ;
   return 0 ;
}

int findBestPack( const std::vector<boost::tuple<std::string , int , int> > & items ,std::set<int> & bestItems , const int weightlimit ) {
   //dynamic programming approach sacrificing storage space for execution
   //time , creating a table of optimal values for every weight and a
   //second table of sets with the items collected so far in the knapsack
   //the best value is in the bottom right corner of the values table,
   //the set of items in the bottom right corner of the sets' table.
   const int n = items.size( ) ;
   int bestValues [ n ][ weightlimit ] ;
   std::set<int> solutionSets[ n ][ weightlimit ] ;
   std::set<int> emptyset ;
   for ( int i = 0 ; i < n ; i++ ) {
      for ( int j = 0 ; j < weightlimit  ; j++ ) {
	 bestValues[ i ][ j ] = 0 ;
	 solutionSets[ i ][ j ] = emptyset ;
       }
    }
    for ( int i = 0 ; i < n ; i++ ) {
       for ( int weight = 0 ; weight < weightlimit ; weight++ ) {
	  if ( i == 0 )
	     bestValues[ i ][ weight ] = 0 ;
	  else  {
	     int itemweight = (items.begin( ) + i)->get<1>( ) ;
	     if ( weight < itemweight ) {
		bestValues[ i ][ weight ] = bestValues[ i - 1 ][ weight ] ;
		solutionSets[ i ][ weight ] = solutionSets[ i - 1 ][ weight ] ;
	     } else { // weight >= itemweight
		if ( bestValues[ i - 1 ][ weight - itemweight ] +
		   (items.begin( ) + i)->get<2>( ) >
		        bestValues[ i - 1 ][ weight ] ) {
		   bestValues[ i ][ weight ] =
		       bestValues[ i - 1 ][ weight - itemweight ] +
	        	(items.begin( ) + i)->get<2>( ) ;
		  solutionSets[ i ][ weight ] =
		      solutionSets[ i - 1 ][ weight - itemweight ] ;
		  solutionSets[ i ][ weight ].insert( i ) ;
	     }
	     else {
		bestValues[ i ][ weight ] = bestValues[ i - 1 ][ weight ] ;
		solutionSets[ i ][ weight ] = solutionSets[ i - 1 ][ weight ] ;
	     }
	  }
       }
      }
    }
    bestItems.swap( solutionSets[ n - 1][ weightlimit - 1 ] ) ;
    return bestValues[ n - 1 ][ weightlimit - 1 ] ;
}
```

{{out}}

```txt

The best value that can be packed in the given knapsack is 1030 !
The following items should be packed in the knapsack:
map
compass
water
sandwich
glucose
banana
suntan creme
waterproof trousers
waterproof overclothes
note-case
sunglasses
socks
The total weight of all items is 396 !

```


=={{header|C_sharp}}==
All combinations, eight treads, break when weight is to large.

```c#
using System;  // 4790@3.6
using System.Threading.Tasks;
class Program
{
    static void Main()
    {
        var sw = System.Diagnostics.Stopwatch.StartNew();
        Console.Write(knapSack(400) + "\n" + sw.Elapsed);  // 60 ms
        Console.Read();
    }

    static string knapSack(uint w1)
    {
        uint sol = 0, v1 = 0;
        Parallel.For(1, 9, t =>
        {
            uint j, wi, k, vi, i1 = 1u << w.Length;
            for (uint i = (uint)t; i < i1; i += 8)
            {
                k = wi = vi = 0;
                for (j = i; j > 0; j >>= 1, k++)
                    if ((j & 1) > 0)
                    {
                        if ((wi += w[k]) > w1) break;
                        vi += v[k];
                    }
                if (wi <= w1 && v1 < vi)
                    lock (locker)
                        if (v1 < vi) { v1 = vi; sol = i; }
            }
        });
        string str = "";
        for (uint k = 0; sol > 0; sol >>= 1, k++)
            if ((sol & 1) > 0) str += items[k] + "\n";
        return str;
    }

    static readonly object locker = new object();

    static byte[] w = { 9, 13, 153, 50, 15, 68, 27, 39, 23, 52, 11,
                          32, 24, 48, 73, 42, 43, 22, 7, 18, 4, 30 },

                  v = { 150, 35, 200, 160, 60, 45, 60, 40, 30, 10, 70,
                          30, 15, 10, 40, 70, 75, 80, 20, 12, 50, 10 };

    static string[] items = {"map","compass","water","sandwich","glucose","tin",
                             "banana","apple","cheese","beer","suntan cream",
                             "camera","T-shirt","trousers","umbrella",
                             "waterproof trousers","waterproof overclothes",
                             "note-case","sunglasses","towel","socks","book"};
}
```

A dynamic version.

```c#
using System
class program
{
    static void Main()
    {
        knapSack(40);
        var sw = System.Diagnostics.Stopwatch.StartNew();
        Console.Write(knapSack(400) + "\n" + sw.Elapsed);  // 31 µs
        Console.Read();
    }

    static string knapSack(uint w1)
    {
        uint n = (uint)w.Length; var K = new uint[n + 1, w1 + 1];
        for (uint vi, wi, w0, x, i = 0; i < n; i++)
            for (vi = v[i], wi = w[i], w0 = 1; w0 <= w1; w0++)
            {
                x = K[i, w0];
                if (wi <= w0) x = max(vi + K[i, w0 - wi], x);
                K[i + 1, w0] = x;
            }
        string str = "";
        for (uint v1 = K[n, w1]; v1 > 0; n--)
            if (v1 != K[n - 1, w1])
            {
                v1 -= v[n - 1]; w1 -= w[n - 1]; str += items[n - 1] + "\n";
            }
        return str;
    }

    static uint max(uint a, uint b) { return a > b ? a : b; }

    static byte[] w =  { 9, 13, 153, 50, 15, 68, 27, 39, 23, 52, 11,
                          32, 24, 48, 73, 42, 43, 22, 7, 18, 4, 30 },

                  v =  { 150, 35, 200, 160, 60, 45, 60, 40, 30, 10, 70,
                          30, 15, 10, 40, 70, 75, 80, 20, 12, 50, 10 };

    static string[] items =  {"map","compass","water","sandwich","glucose","tin",
                              "banana","apple","cheese","beer","suntan cream",
                              "camera","T-shirt","trousers","umbrella",
                              "waterproof trousers","waterproof overclothes",
                              "note-case","sunglasses","towel","socks","book"};
}
```


## C#
{{libheader|System}}
{{libheader|System.Collections.Generic}}

```c#
using System;
using System.Collections.Generic;

namespace Tests_With_Framework_4
{

class Bag : IEnumerable<Bag.Item>
        {
            List<Item> items;
            const int MaxWeightAllowed = 400;

            public Bag()
            {
                items = new List<Item>();
            }

            void AddItem(Item i)
            {
                if ((TotalWeight + i.Weight) <= MaxWeightAllowed)
                    items.Add(i);
            }

            public void Calculate(List<Item> items)
            {
                foreach (Item i in Sorte(items))
                {
                    AddItem(i);
                }
            }

            List<Item> Sorte(List<Item> inputItems)
            {
                List<Item> choosenItems = new List<Item>();
                for (int i = 0; i < inputItems.Count; i++)
                {
                    int j = -1;
                    if (i == 0)
                    {
                        choosenItems.Add(inputItems[i]);
                    }
                    if (i > 0)
                    {
                        if (!RecursiveF(inputItems, choosenItems, i, choosenItems.Count - 1, false, ref j))
                        {
                            choosenItems.Add(inputItems[i]);
                        }
                    }
                }
                return choosenItems;
            }

            bool RecursiveF(List<Item> knapsackItems, List<Item> choosenItems, int i, int lastBound, bool dec, ref int indxToAdd)
            {
                if (!(lastBound < 0))
                {
                    if ( knapsackItems[i].ResultWV < choosenItems[lastBound].ResultWV )
                    {
                        indxToAdd = lastBound;
                    }
                    return RecursiveF(knapsackItems, choosenItems, i, lastBound - 1, true, ref indxToAdd);
                }
                if (indxToAdd > -1)
                {
                    choosenItems.Insert(indxToAdd, knapsackItems[i]);
                    return true;
                }
                return false;
            }

            #region IEnumerable<Item> Members
            IEnumerator<Item> IEnumerable<Item>.GetEnumerator()
            {
                foreach (Item i in items)
                    yield return i;
            }
            #endregion

            #region IEnumerable Members
            System.Collections.IEnumerator System.Collections.IEnumerable.GetEnumerator()
            {
                return items.GetEnumerator();
            }
            #endregion

            public int TotalWeight
            {
                get
                {
                    var sum = 0;
                    foreach (Item i in this)
                    {
                        sum += i.Weight;
                    }
                    return sum;
                }
            }

            public class Item
            {
                public string Name { get; set; } public int Weight { get; set; } public int Value { get; set; } public int ResultWV { get { return  Weight-Value; } }
                public override string ToString()
                {
                    return "Name : " + Name + "        Wieght : " + Weight + "       Value : " + Value + "     ResultWV : " + ResultWV;
                }
            }
        }

    class Program
    {
        static void Main(string[] args)
        {List<Bag.Item> knapsackItems = new List<Bag.Item>();
            knapsackItems.Add(new Bag.Item() { Name = "Map", Weight = 9, Value = 150 });
            knapsackItems.Add(new Bag.Item() { Name = "Water", Weight = 153, Value = 200 });
            knapsackItems.Add(new Bag.Item() { Name = "Compass", Weight = 13, Value = 35 });
            knapsackItems.Add(new Bag.Item() { Name = "Sandwitch", Weight = 50, Value = 160 });
            knapsackItems.Add(new Bag.Item() { Name = "Glucose", Weight = 15, Value = 60 });
            knapsackItems.Add(new Bag.Item() { Name = "Tin", Weight = 68, Value = 45 });
            knapsackItems.Add(new Bag.Item() { Name = "Banana", Weight = 27, Value = 60 });
            knapsackItems.Add(new Bag.Item() { Name = "Apple", Weight = 39, Value = 40 });
            knapsackItems.Add(new Bag.Item() { Name = "Cheese", Weight = 23, Value = 30 });
            knapsackItems.Add(new Bag.Item() { Name = "Beer", Weight = 52, Value = 10 });
            knapsackItems.Add(new Bag.Item() { Name = "Suntan Cream", Weight = 11, Value = 70 });
            knapsackItems.Add(new Bag.Item() { Name = "Camera", Weight = 32, Value = 30 });
            knapsackItems.Add(new Bag.Item() { Name = "T-shirt", Weight = 24, Value = 15 });
            knapsackItems.Add(new Bag.Item() { Name = "Trousers", Weight = 48, Value = 10 });
            knapsackItems.Add(new Bag.Item() { Name = "Umbrella", Weight = 73, Value = 40 });
            knapsackItems.Add(new Bag.Item() { Name = "WaterProof Trousers", Weight = 42, Value = 70 });
            knapsackItems.Add(new Bag.Item() { Name = "Note-Case", Weight = 22, Value = 80 });
            knapsackItems.Add(new Bag.Item() { Name = "Sunglasses", Weight = 7, Value = 20 });
            knapsackItems.Add(new Bag.Item() { Name = "Towel", Weight = 18, Value = 12 });
            knapsackItems.Add(new Bag.Item() { Name = "Socks", Weight = 4, Value = 50 });
            knapsackItems.Add(new Bag.Item() { Name = "Book", Weight = 30, Value = 10 });
            knapsackItems.Add(new Bag.Item() { Name = "waterproof overclothes ", Weight = 43, Value = 75 });

            Bag b = new Bag();
            b.Calculate(knapsackItems);
            b.All(x => { Console.WriteLine(x); return true; });
            Console.WriteLine(b.Sum(x => x.Weight));
            Console.ReadKey();
        }
    }
}
```

("Bag" might not be the best name for the class, since "bag" is sometimes also used to refer to a multiset data structure.)


## Ceylon



<b>module.ceylon</b>:


```ceylon

module knapsack "1.0.0" {
}

```


<b>run.ceylon:</b>


```ceylon

shared void run() {
    value knapsack = pack(items, empty(400));

    print(knapsack);
}

class Item(name,weight,theValue) {
    String name;
    shared Integer weight;
    shared Float theValue;

    shared actual String string = "item(``name``, ``weight``, ``theValue``)";
}

class Knapsack(items,theValue,weight,available) {
    shared Item[] items;
    shared Float theValue;
    shared Integer weight;
    shared Integer available;

    shared Boolean canAccept(Item item)
        => item.weight <= available;

    String itemsString = items.fold("")((total, remaining) => "``total``\t\n``remaining.string``" );

    shared actual String string = "Total value: ``theValue``\nTotal weight: ``weight``\nItems:\n``itemsString``";
}

Knapsack empty(Integer capacity)
    => Knapsack([], 0.0, 0, capacity);


Item[] items =
        [
         Item("map", 9, 150.0),
         Item("compass", 13, 35.0),
         Item("water", 153, 200.0),
         Item("sandwich", 50, 160.0),
         Item("glucose", 15, 60.0),
         Item("tin", 68, 45.0),
         Item("banana", 27, 60.0),
         Item("apple", 39, 40.0),
         Item("cheese", 23, 30.0),
         Item("beer", 52, 10.0),
         Item("cream", 11, 70.0),
         Item("camera", 32, 30.0),
         Item("tshirt", 24, 15.0),
         Item("trousers", 48, 10.0),
         Item("umbrella", 73, 40.0),
         Item("trousers", 42, 70.0),
         Item("overclothes", 43, 75.0),
         Item("notecase", 22, 80.0),
         Item("sunglasses", 7, 20.0),
         Item("towel", 18, 12.0),
         Item("socks", 4, 50.0),
         Item("book", 30, 10.0)
        ];


Knapsack add(Item item, Knapsack knapsack)
    => Knapsack { items = knapsack.items.withTrailing(item);
                  theValue = knapsack.theValue + item.theValue;
                  weight = knapsack.weight + item.weight;
                  available = knapsack.available - item.weight; };

Float rating(Item item) => item.theValue / item.weight.float;

Knapsack pack(Item[] items, Knapsack knapsack)
    // Sort the items by decreasing rating, that is, value divided by weight
    => let (itemsSorted =
                items.group(rating)
                     .sort(byDecreasing((Float->[Item+] entry) => entry.key))
                     .map(Entry.item)
                     .flatMap((element) => element)
                     .sequence())

    packRecursive(itemsSorted,knapsack);

Knapsack packRecursive(Item[] sortedItems, Knapsack knapsack)
    => if (exists firstItem=sortedItems.first, knapsack.canAccept(firstItem))
        then packRecursive(sortedItems.rest, add(firstItem,knapsack))
        else knapsack;

```



{{out}}

```txt

Total value: 1030.0
Total weight: 396
Items:

item(map, 9, 150.0)
item(socks, 4, 50.0)
item(cream, 11, 70.0)
item(glucose, 15, 60.0)
item(notecase, 22, 80.0)
item(sandwich, 50, 160.0)
item(sunglasses, 7, 20.0)
item(compass, 13, 35.0)
item(banana, 27, 60.0)
item(overclothes, 43, 75.0)
item(trousers, 42, 70.0)
item(water, 153, 200.0)

```



## Clojure

Uses the dynamic programming solution from [[wp:Knapsack_problem#0-1_knapsack_problem|Wikipedia]].
First define the ''items'' data:

```clojure
(def item-data
  [ "map"         9 150
    "compass"    13  35
    "water"     153 200
    "sandwich"   50 160
    "glucose"    15  60
    "tin"        68  45
    "banana"     27  60
    "apple"      39  40
    "cheese"     23  30
    "beer"       52  10
    "suntan cream"   11  70
    "camera"     32  30
    "t-shirt"    24  15
    "trousers"   48  10
    "umbrella"   73  40
    "waterproof trousers"    42  70
    "waterproof overclothes" 43  75
    "note-case"  22  80
    "sunglasses"  7  20
    "towel"      18  12
    "socks"       4  50
    "book"       30  10])

(defstruct item :name :weight :value)

(def items (vec (map #(apply struct item %) (partition 3 item-data))))
```

''m'' is as per the Wikipedia formula, except that it returns a pair ''[value indexes]'' where ''indexes'' is a vector of index values in ''items''. ''value'' is the maximum value attainable using items 0..''i'' whose total weight doesn't exceed ''w''; ''indexes'' are the item indexes that produces the value.

```clojure
(declare mm) ;forward decl for memoization function

(defn m [i w]
  (cond
    (< i 0) [0 []]
    (= w 0) [0 []]
    :else
    (let [{wi :weight vi :value} (get items i)]
      (if (> wi w)
        (mm (dec i) w)
        (let [[vn sn :as no]  (mm (dec i) w)
              [vy sy :as yes] (mm (dec i) (- w wi))]
          (if (> (+ vy vi) vn)
            [(+ vy vi) (conj sy i)]
            no))))))

(def mm (memoize m))
```

Call ''m'' and print the result:

```clojure
(use '[clojure.string :only [join]])

(let [[value indexes] (m (-> items count dec) 400)
      names (map (comp :name items) indexes)]
  (println "items to pack:" (join ", " names))
  (println "total value:" value)
  (println "total weight:" (reduce + (map (comp :weight items) indexes))))
```

{{out}}

```txt
items to pack: map, compass, water, sandwich, glucose, banana, suntan cream, waterproof trousers,
waterproof overclothes, note-case, sunglasses, socks
total value: 1030
total weight: 396
```



## Common Lisp

Cached method.

```lisp
;;; memoize
(defmacro mm-set (p v) `(if ,p ,p (setf ,p ,v)))

(defun knapsack (max-weight items)
  (let ((cache (make-array (list (1+ max-weight) (1+ (length items)))
			   :initial-element nil)))

    (labels ((knapsack1 (spc items)
	(if (not items) (return-from knapsack1 (list 0 0 '())))
	(mm-set (aref cache spc (length items))
		(let* ((i (first items))
		       (w (second i))
		       (v (third i))
		       (x (knapsack1 spc (cdr items))))
		  (if (> w spc) x
		    (let* ((y (knapsack1 (- spc w) (cdr items)))
			   (v (+ v (first y))))
		      (if (< v (first x)) x
			(list v (+ w (second y)) (cons i (third y))))))))))

      (knapsack1 max-weight items))))

(print
  (knapsack 400
	    '((map 9 150) (compass 13 35) (water 153 200) (sandwich 50 160)
	      (glucose 15 60) (tin 68 45)(banana 27 60) (apple 39 40)
	      (cheese 23 30) (beer 52 10) (cream 11 70) (camera 32 30)
	      (T-shirt 24 15) (trousers 48 10) (umbrella 73 40)
	      (trousers 42 70) (overclothes 43 75) (notecase 22 80)
	      (glasses 7 20) (towel 18 12) (socks 4 50) (book 30 10))))
```

{{out}}

```txt
(1030 396
 ((MAP 9 150) (COMPASS 13 35) (WATER 153 200) (SANDWICH 50 160) (GLUCOSE 15 60)
  (BANANA 27 60) (CREAM 11 70) (TROUSERS 42 70) (OVERCLOTHES 43 75)
  (NOTECASE 22 80) (GLASSES 7 20) (SOCKS 4 50)))
```



## Crystal

Branch and bound solution

```Ruby
require "bit_array"

struct BitArray
  def clone
    BitArray.new(size).tap { |new| new.to_slice.copy_from (to_slice) }
  end
end

record Item, name : String, weight : Int32, value : Int32

record Selection, mask : BitArray, cur_index : Int32, total_value : Int32

class Knapsack
  @threshold_value = 0
  @threshold_choice : Selection?
  getter checked_nodes = 0

  def knapsack_step(taken, items, remaining_weight)
    if taken.total_value > @threshold_value
      @threshold_value = taken.total_value
      @threshold_choice = taken
    end
    candidate_index = items.index(taken.cur_index) { |item| item.weight <= remaining_weight }
    return nil unless candidate_index
    @checked_nodes += 1
    candidate = items[candidate_index]
    # candidate is a best of available items, so if we fill remaining value with it
    # and still don't reach the threshold, the branch is wrong
    return nil if taken.total_value + 1.0 * candidate.value / candidate.weight * remaining_weight < @threshold_value
    # now recursively check both variants
    mask = taken.mask.clone
    mask[candidate_index] = true
    knapsack_step Selection.new(mask, candidate_index + 1, taken.total_value + candidate.value), items, remaining_weight - candidate.weight
    mask = taken.mask.clone
    mask[candidate_index] = false
    knapsack_step Selection.new(mask, candidate_index + 1, taken.total_value), items, remaining_weight
  end

  def select(items, max_weight)
    @checked_variants = 0
    # sort by descending relative value
    list = items.sort_by { |item| -1.0 * item.value / item.weight }
    # use heuristic of relative value as an initial estimate for branch&bounds
    w = max_weight
    heur_list = list.take_while { |item| w -= item.weight; w > 0 }
    nothing = Selection.new(BitArray.new(items.size), 0, 0)
    @threshold_value = heur_list.sum(&.value) - 1
    @threshold_choice = nothing
    knapsack_step(nothing, list, max_weight)
    selected = @threshold_choice.not_nil!
    result = [] of Item
    selected.mask.each_with_index { |v, i| result << list[i] if v }
    result
  end
end

possible = [
  Item.new("map", 9, 150),
  Item.new("compass", 13, 35),
  Item.new("water", 153, 200),
  Item.new("sandwich", 50, 160),
  Item.new("glucose", 15, 60),
  Item.new("tin", 68, 45),
  Item.new("banana", 27, 60),
  Item.new("apple", 39, 40),
  Item.new("cheese", 23, 30),
  Item.new("beer", 52, 10),
  Item.new("suntan cream", 11, 70),
  Item.new("camera", 32, 30),
  Item.new("T-shirt", 24, 15),
  Item.new("trousers", 48, 10),
  Item.new("umbrella", 73, 40),
  Item.new("waterproof trousers", 42, 70),
  Item.new("waterproof overclothes", 43, 75),
  Item.new("note-case", 22, 80),
  Item.new("sunglasses", 7, 20),
  Item.new("towel", 18, 12),
  Item.new("socks", 4, 50),
  Item.new("book", 30, 10),
]

solver = Knapsack.new
used = solver.select(possible, 400)
puts "optimal choice: #{used.map(&.name)}"
puts "total weight #{used.sum(&.weight)}, total value #{used.sum(&.value)}"
puts "checked nodes: #{solver.checked_nodes}"

```

{{out}}

```txt
optimal choice: ["map", "socks", "suntan cream", "glucose", "note-case", "sandwich", "sunglasses", "compass", "banana", "waterproof overclothes", "waterproof trousers", "water"]
total weight 396, total value 1030
checked nodes: 992
```


## D


### Dynamic Programming Version

{{trans|Python}}

```d
import std.stdio, std.algorithm, std.typecons, std.array, std.range;

struct Item { string name; int weight, value; }

Item[] knapsack01DinamicProgramming(immutable Item[] items, in int limit)
pure nothrow @safe {
    auto tab = new int[][](items.length + 1, limit + 1);

    foreach (immutable i, immutable it; items)
        foreach (immutable w; 1 .. limit + 1)
            tab[i + 1][w] = (it.weight > w) ? tab[i][w] :
                max(tab[i][w], tab[i][w - it.weight] + it.value);

    typeof(return) result;
    int w = limit;
    foreach_reverse (immutable i, immutable it; items)
        if (tab[i + 1][w] != tab[i][w]) {
            w -= it.weight;
            result ~= it;
        }

    return result;
}

void main() @safe {
    enum int limit = 400;
    immutable Item[] items = [
        {"apple",      39,  40}, {"banana",        27,  60},
        {"beer",       52,  10}, {"book",          30,  10},
        {"camera",     32,  30}, {"cheese",        23,  30},
        {"compass",    13,  35}, {"glucose",       15,  60},
        {"map",         9, 150}, {"note-case",     22,  80},
        {"sandwich",   50, 160}, {"socks",          4,  50},
        {"sunglasses",  7,  20}, {"suntan cream",  11,  70},
        {"t-shirt",    24,  15}, {"tin",           68,  45},
        {"towel",      18,  12}, {"trousers",      48,  10},
        {"umbrella",   73,  40}, {"water",        153, 200},
        {"waterproof overclothes", 43, 75},
        {"waterproof trousers",    42, 70}];

    immutable bag = knapsack01DinamicProgramming(items, limit);
    writefln("Items:\n%-(  %s\n%)", bag.map!q{ a.name }.retro);
    const t = reduce!q{ a[] += [b.weight, b.value] }([0, 0], bag);
    writeln("\nTotal weight and value: ", t[0] <= limit ? t : [0, 0]);
}
```

{{out}}

```txt
Items:
  banana
  compass
  glucose
  map
  note-case
  sandwich
  socks
  sunglasses
  suntan cream
  water
  waterproof overclothes
  waterproof trousers

Total weight and value: [396, 1030]
```



### Brute Force Version

{{trans|C}}

```d
struct Item { string name; int weight, value; }

immutable Item[] items = [
    {"apple",      39,  40}, {"banana",        27,  60},
    {"beer",       52,  10}, {"book",          30,  10},
    {"camera",     32,  30}, {"cheese",        23,  30},
    {"compass",    13,  35}, {"glucose",       15,  60},
    {"map",         9, 150}, {"note-case",     22,  80},
    {"sandwich",   50, 160}, {"socks",          4,  50},
    {"sunglasses",  7,  20}, {"suntan cream",  11,  70},
    {"t-shirt",    24,  15}, {"tin",           68,  45},
    {"towel",      18,  12}, {"trousers",      48,  10},
    {"umbrella",   73,  40}, {"water",        153, 200},
    {"waterproof overclothes", 43, 75},
    {"waterproof trousers",    42, 70}];

struct Solution { uint bits; int value; }
static assert(items.length <= Solution.bits.sizeof * 8);

void solve(in int weight, in int idx, ref Solution s)
pure nothrow @nogc @safe {
    if (idx < 0) {
        s.bits = s.value = 0;
        return;
    }

    if (weight < items[idx].weight) {
        solve(weight, idx - 1, s);
        return;
     }

    Solution v1, v2;
    solve(weight, idx - 1, v1);
    solve(weight - items[idx].weight, idx - 1, v2);

    v2.value += items[idx].value;
    v2.bits |= (1 << idx);

    s = (v1.value >= v2.value) ? v1 : v2;
}

void main() @safe {
    import std.stdio;

    auto s = Solution(0, 0);
    solve(400, items.length - 1, s);

    writeln("Items:");
    int w = 0;
    foreach (immutable i, immutable it; items)
        if (s.bits & (1 << i)) {
            writeln("  ", it.name);
            w += it.weight;
        }
    writefln("\nTotal value: %d; weight: %d", s.value, w);
}
```

The runtime is about 0.09 seconds.
{{out}}

```txt
Items:
  banana
  compass
  glucose
  map
  note-case
  sandwich
  socks
  sunglasses
  suntan cream
  water
  waterproof overclothes
  waterproof trousers

Total value: 1030; weight: 396
```



### Short Dynamic Programming Version

{{trans|Haskell}}

```d
import std.stdio, std.algorithm, std.typecons, std.array, std.range;

struct Item { string name; int w, v; }
alias Pair = Tuple!(int,"tot", string[],"names");

immutable Item[] items = [{"apple",39,40}, {"banana", 27, 60},
    {"beer", 52, 10}, {"book", 30, 10}, {"camera", 32, 30},
    {"cheese", 23, 30}, {"compass", 13, 35}, {"glucose", 15, 60},
    {"map", 9, 150}, {"note-case", 22, 80}, {"sandwich", 50, 160},
    {"socks", 4, 50}, {"sunglasses", 7, 20}, {"suntan cream", 11, 70},
    {"t-shirt", 24, 15}, {"tin", 68, 45}, {"towel", 18, 12},
    {"trousers", 48, 10}, {"umbrella", 73, 40}, {"water", 153, 200},
    {"overclothes", 43, 75}, {"waterproof trousers", 42, 70}];

auto addItem(Pair[] lst, in Item it) pure /*nothrow*/ {
    auto aux = lst.map!(vn => Pair(vn.tot + it.v, vn.names ~ it.name));
    return lst[0..it.w] ~ lst[it.w..$].zip(aux).map!q{ a[].max }.array;
}

void main() {
    reduce!addItem(Pair().repeat.take(400).array, items).back.writeln;
}
```

Runtime about 0.04 seconds.
{{out}}

```txt
Tuple!(int, "tot", string[], "names")(1030, ["banana", "compass", "glucose", "map", "note-case", "sandwich", "socks", "sunglasses", "suntan cream", "water", "overclothes", "waterproof trousers"])
```



## Dart


```dart
List solveKnapsack(items, maxWeight) {
  int MIN_VALUE=-100;
  int N = items.length; // number of items
  int W = maxWeight; // maximum weight of knapsack

  List profit = new List(N+1);
  List weight = new List(N+1);

  // generate random instance, items 1..N
  for(int n = 1; n<=N; n++) {
    profit[n] = items[n-1][2];
    weight[n] = items[n-1][1];

  }

  // opt[n][w] = max profit of packing items 1..n with weight limit w
  // sol[n][w] = does opt solution to pack items 1..n with weight limit w include item n?
  List<List<int>> opt = new List<List<int>>(N+1);
  for (int i=0; i<N+1; i++) {
    opt[i] = new List<int>(W+1);
    for(int j=0; j<W+1; j++) {
      opt[i][j] = MIN_VALUE;
    }
  }

  List<List<bool>> sol = new List<List<bool>>(N+1);
  for (int i=0; i<N+1; i++) {
    sol[i] = new List<bool>(W+1);
    for(int j=0; j<W+1; j++) {
      sol[i][j] = false;
    }
  }

  for(int n=1; n<=N; n++) {
    for (int w=1; w <= W; w++) {
      // don't take item n
      int option1 = opt[n-1][w];

      // take item n
      int option2 = MIN_VALUE;
      if (weight[n] <= w) {
        option2 = profit[n] + opt[n-1][w - weight[n]];
      }

      // select better of two options
      opt[n][w] = Math.max(option1, option2);
      sol[n][w] = (option2 > option1);
    }
  }

  // determine which items to take
  List<List> packItems = new List<List>();
  List<bool> take = new List(N+1);
  for (int n = N, w = W; n > 0; n--) {
    if (sol[n][w]) {
      take[n] = true;
      w = w - weight[n];
      packItems.add(items[n-1]);
    } else {
      take[n] = false;
    }
  }

  return packItems;

}

main() {
  List knapsackItems = [];
  knapsackItems.add(["map", 9, 150]);
  knapsackItems.add(["compass", 13, 35]);
  knapsackItems.add(["water", 153, 200]);
  knapsackItems.add(["sandwich", 50, 160]);
  knapsackItems.add(["glucose", 15, 60]);
  knapsackItems.add(["tin", 68, 45]);
  knapsackItems.add(["banana", 27, 60]);
  knapsackItems.add(["apple", 39, 40]);
  knapsackItems.add(["cheese", 23, 30]);
  knapsackItems.add(["beer", 52, 10]);
  knapsackItems.add(["suntan cream", 11, 70]);
  knapsackItems.add(["camera", 32, 30]);
  knapsackItems.add(["t-shirt", 24, 15]);
  knapsackItems.add(["trousers", 48, 10]);
  knapsackItems.add(["umbrella", 73, 40]);
  knapsackItems.add(["waterproof trousers", 42, 70]);
  knapsackItems.add(["waterproof overclothes", 43, 75]);
  knapsackItems.add(["note-case", 22, 80]);
  knapsackItems.add(["sunglasses", 7, 20]);
  knapsackItems.add(["towel", 18, 12]);
  knapsackItems.add(["socks", 4, 50]);
  knapsackItems.add(["book", 30, 10]);
  int maxWeight = 400;
  Stopwatch sw = new Stopwatch.start();
  List p = solveKnapsack(knapsackItems, maxWeight);
  sw.stop();
  int totalWeight = 0;
  int totalValue = 0;
  print(["item","profit","weight"]);
  p.forEach((var i) { print("${i}"); totalWeight+=i[1]; totalValue+=i[2]; });
  print("Total Value = ${totalValue}");
  print("Total Weight = ${totalWeight}");
  print("Elapsed Time = ${sw.elapsedInMs()}ms");

}
```

{{out}}

```txt
[item, profit, weight]
[socks, 4, 50]
[sunglasses, 7, 20]
[note-case, 22, 80]
[waterproof overclothes, 43, 75]
[waterproof trousers, 42, 70]
[suntan cream, 11, 70]
[banana, 27, 60]
[glucose, 15, 60]
[sandwich, 50, 160]
[water, 153, 200]
[compass, 13, 35]
[map, 9, 150]
Total Value = 1030
Total Weight = 396
Elapsed Time = 6ms
```



## EasyLang

<lang>name$[] = [ "map" "compass" "water" "sandwich" "glucose" "tin" "banana" "apple" "cheese" "beer" "suntan cream" "camera" "t-shirt" "trousers" "umbrella" "waterproof trousers" "waterproof overclothes" "note-case" "sunglasses" "towel" "socks" "book" ]
weight[] = [ 9 13 153 50 15 68 27 39 23 52 11 32 24 48 73 42 43 22 7 18 4 30 ]
value[] = [ 150 35 200 160 60 45 60 40 30 10 70 30 15 10 40 70 75 80 20 12 50 10 ]
max_w = 400
#
func solve i w . items[] wres vres .
  if i < 0
    wres = 0
    vres = 0
    items[] = [ ]
  elif weight[i] > w
    call solve i - 1 w items[] wres vres
  else
    call solve i - 1 w items[] wres vres
    call solve i - 1 w - weight[i] items1[] w1 v1
    v1 += value[i]
    if v1 > vres
      swap items[] items1[]
      items[] &= i
      wres = w1 + weight[i]
      vres = v1
    .
  .
.
call solve len weight[] - 1 max_w items[] w v
print "weight: " & w
print "value: " & v
print "items:"
for i range len items[]
  print "  " & name$[items[i]]
.
```



## EchoLisp


```scheme

(require 'struct)
(require 'hash)
(require 'sql)

(define H (make-hash))
(define T (make-table (struct goodies (name poids valeur ))))
(define-syntax-rule (name i) (table-xref T i 0))
(define-syntax-rule (poids i) (table-xref T i 1))
(define-syntax-rule (valeur i) (table-xref T i 2))

;;  make an unique hash-key from (i rest)
(define (t-idx i r)  (string-append i "|" r))
;; retrieve best score for item i, remaining r availbble weight
(define (t-get i r)  (or (hash-ref H (t-idx i r)) 0))

;; compute best score (i), assuming best (i-1 rest) is known
(define (score i restant)
	(if (< i 0) 0
	(hash-ref! H (t-idx i restant)
		(if ( >= restant (poids i))
			(max
				(score (1- i) restant)
			    (+ (score (1- i) (- restant (poids i))) (valeur i)))
		    (score (1- i) restant)))))

;; compute best scores, starting from last item
(define (task W)
        (define restant W)
        (define N (1- (table-count T)))
		(writeln 'total-value (score N W))
		(for/list  ((i (in-range N -1 -1)))
			#:continue (= (t-get i restant) (t-get (1- i) restant))
			(set! restant (- restant (poids i)))
			(name i)))

```

{{out}}

```scheme

;; init table
(define goodies
          '((map 9 150) ; 9 is weight, 150 is value
            (compass 13 35) (water 153 200) (sandwich 50 160)
            (glucose 15 60) (tin 68 45)(banana 27 60) (apple 39 40)
            (fromage 23 30) (beer 52 10) (🌞-suntan-cream 11 70) (camera 32 30)
            (T-shirt 24 15) (pantalons 48 10) (umbrella 73 40)
            (☔️-trousers 42 70) (☔️-overclothes 43 75) (note-case 22 80)
            (🌞-sun-glasses 7 20) (towel 18 12) (socks 4 50) (book 30 10)))
(list->table goodies T)


(task 400)
total-value     1030
    → (socks 🌞-sun-glasses note-case ☔️-overclothes ☔️-trousers 🌞-suntan-cream banana
    glucose sandwich water compass map)


(length (hash-keys H))
   → 4939  ;; number of entries "i | weight" in hash table

```



## Eiffel


```Eiffel

class
	APPLICATION

create
	make

feature {NONE} -- Initialization

	make
		local
			knapsack: KNAPSACKZEROONE
		do
			create knapsack.make (400)
			knapsack.add_item (create {ITEM}.make ("", 0, 0))
			knapsack.add_item (create {ITEM}.make ("map", 9, 150))
			knapsack.add_item (create {ITEM}.make ("compass", 13, 35))
			knapsack.add_item (create {ITEM}.make ("water", 153, 200))
			knapsack.add_item (create {ITEM}.make ("sandwich", 50, 160))
			knapsack.add_item (create {ITEM}.make ("glucose", 15, 60))
			knapsack.add_item (create {ITEM}.make ("tin", 68, 45))
			knapsack.add_item (create {ITEM}.make ("banana", 27, 60))
			knapsack.add_item (create {ITEM}.make ("apple", 39, 40))
			knapsack.add_item (create {ITEM}.make ("cheese", 23, 30))
			knapsack.add_item (create {ITEM}.make ("beer", 52, 10))
			knapsack.add_item (create {ITEM}.make ("suntan cream", 11, 70))
			knapsack.add_item (create {ITEM}.make ("camera", 32, 30))
			knapsack.add_item (create {ITEM}.make ("T-shirt", 24, 15))
			knapsack.add_item (create {ITEM}.make ("trousers", 48, 10))
			knapsack.add_item (create {ITEM}.make ("umbrella, ella ella", 73, 40))
			knapsack.add_item (create {ITEM}.make ("waterproof trousers", 42, 70))
			knapsack.add_item (create {ITEM}.make ("waterproof overclothes", 43, 75))
			knapsack.add_item (create {ITEM}.make ("note-case", 22, 80))
			knapsack.add_item (create {ITEM}.make ("sunglasses", 7, 20))
			knapsack.add_item (create {ITEM}.make ("towel", 18, 12))
			knapsack.add_item (create {ITEM}.make ("socks", 4, 50))
			knapsack.add_item (create {ITEM}.make ("book", 30, 10))
			knapsack.compute_solution
		end

end

```


```Eiffel

class
	ITEM

create
	make, make_from_other

feature

	name: STRING

	weight: INTEGER

	value: INTEGER

	make_from_other (other: ITEM)
			-- Item with name, weight and value set to 'other's name, weight and value.
		do
			name := other.name
			weight := other.weight
			value := other.value
		end

	make (a_name: String; a_weight, a_value: INTEGER)
			-- Item with name, weight and value set to 'a_name', 'a_weight' and 'a_value'.
		require
			a_name /= Void
			a_weight >= 0
			a_value >= 0
		do
			name := a_name
			weight := a_weight
			value := a_value
		end

end

```


```Eiffel

class
	KNAPSACKZEROONE

create
	make

feature

	items: ARRAY [ITEM]

	max_weight: INTEGER

feature

	make (a_max_weight: INTEGER)
			-- Make an empty knapsack.
		require
			a_max_weight >= 0
		do
			create items.make_empty
			max_weight := a_max_weight
		end

	add_item (item: ITEM)
			-- Add 'item' to knapsack.
		local
			temp: ITEM
		do
			create temp.make_from_other (item)
			items.force (item, items.count + 1)
		end

	compute_solution
		local
			M: ARRAY [INTEGER]
			n: INTEGER
			i, j: INTEGER
			w_i, v_i: INTEGER
			item_i: ITEM
			final_items: LINKED_LIST [ITEM]
		do
			n := items.count
			create M.make_filled (0, 1, n * max_weight)
			from
				i := 2
			until
				(i > n)
			loop
				from
					j := 1
				until
					j > max_weight
				loop
					item_i := items [i]
					w_i := item_i.weight
					if w_i <= j then
						v_i := item_i.value
						M [(i - 1) * max_weight + j] := max (M [(i - 2) * max_weight + j], M [(i - 2) * max_weight + j - w_i + 1] + v_i)
					else
						M [(i - 1) * max_weight + j] := M [(i - 2) * max_weight + j]
					end
					j := j + 1
				end
				i := i + 1
			end
			io.put_string ("The final value of the knapsack will be: ")
			io.put_integer (M [(n - 1) * max_weight + max_weight]);
			io.new_line
				--compute the items that fit into the knapsack
			create final_items.make
			io.put_string ("We'll take the following items: %N");
			from
				i := n
				j := max_weight
			until
				i <= 1 or j <= 1
			loop
				item_i := items [i]
				w_i := item_i.weight
				if w_i <= j then
					v_i := item_i.value
					if M [(i - 1) * max_weight + j] = M [(i - 2) * max_weight + j] then
					else
						final_items.extend (item_i)
						io.put_string (item_i.name)
						io.new_line
						j := j - w_i
					end
				else
				end
				i := i - 1
			end
		end

feature {NONE}

	max (a, b: INTEGER): INTEGER
			-- Max of 'a' and 'b'.
		do
			Result := a
			if a < b then
				Result := b
			end
		end

end

```

{{out}}

```txt

The final value of the knapsack will be: 1030
We'll take the following items:
socks
sunglasses
note-case
waterproof overclothes
waterproof trousers
suntan cream
banana
glucose
sandwich
water
compass
map

```



## Elixir

{{trans|Erlang}}

```elixir
defmodule Knapsack do
  def solve([], _total_weight, item_acc, value_acc, weight_acc), do:
    {item_acc, value_acc, weight_acc}
  def solve([{_item, item_weight, _item_value} | t],
            total_weight,
            item_acc,
            value_acc,
            weight_acc) when item_weight > total_weight, do:
    solve(t, total_weight, item_acc, value_acc, weight_acc)
  def solve([{item_name, item_weight, item_value} | t],
            total_weight,
            item_acc,
            value_acc,
            weight_acc) do
    {_tail_item_acc, tail_value_acc, _tail_weight_acc} = tail_res =
        solve(t, total_weight, item_acc, value_acc, weight_acc)
    {_head_item_acc, head_value_acc, _head_weight_acc} = head_res =
        solve(t,
              total_weight - item_weight,
              [item_name | item_acc],
              value_acc + item_value,
              weight_acc + item_weight)
    if tail_value_acc > head_value_acc, do: tail_res, else: head_res
  end
end

stuff = [{"map",                      9,   150},
         {"compass",                 13,    35},
         {"water",                  153,   200},
         {"sandwich",                50,   160},
         {"glucose",                 15,    60},
         {"tin",                     68,    45},
         {"banana",                  27,    60},
         {"apple",                   39,    40},
         {"cheese",                  23,    30},
         {"beer",                    52,    10},
         {"suntan cream",            11,    70},
         {"camera",                  32,    30},
         {"T-shirt",                 24,    15},
         {"trousers",                48,    10},
         {"umbrella",                73,    40},
         {"waterproof trousers",     42,    70},
         {"waterproof overclothes",  43,    75},
         {"note-case",               22,    80},
         {"sunglasses",               7,    20},
         {"towel",                   18,    12},
         {"socks",                    4,    50},
         {"book",                    30,    10}]
max_weight = 400

go = fn (stuff, max_weight) ->
  {time, {item_list, total_value, total_weight}} = :timer.tc(fn ->
    Knapsack.solve(stuff, max_weight, [], 0, 0)
  end)
  IO.puts "Items:"
  Enum.each(item_list, fn item -> IO.inspect item end)
  IO.puts "Total value: #{total_value}"
  IO.puts "Total weight: #{total_weight}"
  IO.puts "Time elapsed in milliseconds: #{time/1000}"
end
go.(stuff, max_weight)
```


{{out}}

```txt

Items:
"socks"
"sunglasses"
"note-case"
"waterproof overclothes"
"waterproof trousers"
"suntan cream"
"banana"
"glucose"
"sandwich"
"water"
"compass"
"map"
Total value: 1030
Total weight: 396
Time elapsed in milliseconds: 733.0

```



## Emacs Lisp


{{Trans|Common Lisp}} with changes (memoization without macro)


```lisp

(defun ks (max-w items)
  (let ((cache (make-vector (1+ (length items)) nil)))
    (dotimes (n (1+ (length items)))
      (setf (aref cache n) (make-hash-table :test 'eql)))
    (defun ks-emb (spc items)
      (let ((slot (gethash spc (aref cache (length items)))))
        (cond
         ((null items) (list 0 0 '()))
         (slot slot)
         (t (puthash spc
                  (let*
                      ((i (car items))
                       (w (nth 1 i))
                       (v (nth 2 i))
                       (x (ks-emb spc (cdr items))))
                    (cond
                     ((> w spc) x)
                     (t
                      (let* ((y (ks-emb (- spc w) (cdr items)))
                             (v (+ v (car y))))
                        (cond
                         ((< v (car x)) x)
                         (t
                          (list v (+ w (nth 1 y)) (cons i (nth 2 y)))))))))
                  (aref cache (length items)))))))
    (ks-emb max-w items)))

(ks 400
    '((map 9 150) (compass 13 35) (water 153 200) (sandwich 50 160)
      (glucose 15 60) (tin 68 45)(banana 27 60) (apple 39 40)
      (cheese 23 30) (beer 52 10) (cream 11 70) (camera 32 30)
      (T-shirt 24 15) (trousers 48 10) (umbrella 73 40)
      (waterproof-trousers 42 70) (overclothes 43 75) (notecase 22 80)
      (glasses 7 20) (towel 18 12) (socks 4 50) (book 30 10)))

```


{{out}}

```txt

(1030 396 ((map 9 150) (compass 13 35) (water 153 200) (sandwich 50 160) (glucose 15 60)
(banana 27 60) (cream 11 70) (waterproof-trousers 42 70) (overclothes 43 75) (notecase 22 80)
(glasses 7 20) (socks 4 50)))

```


Another way without cache :

```lisp

(defun best-rate (l1 l2)
  "predicate for sorting a list of elements regarding the value/weight rate"
  (let*
      ((r1 (/ (* 1.0 (nth 2 l1)) (nth 1 l1)))
       (r2 (/ (* 1.0 (nth 2 l2)) (nth 1 l2))))
    (cond
     ((> r1 r2) t)
     (t nil))))

(defun ks1 (l max)
  "return a complete list - complete means 'less than max-weight
but add the next element is impossible'"
(let ((l (sort l 'best-rate)))
  (cond
   ((null l) l)
   ((<= (nth 1 (car l)) max)
    (cons (car l) (ks1 (cdr l) (- max (nth 1 (car l))))))
   (t (ks1 (cdr l) max)))))

(defun totval (lol)
  "totalize values of a list - lol is not for laughing
but for list of list"
  (cond
   ((null lol) 0)
   (t
    (+
     (nth 2 (car lol))
     (totval (cdr lol))))))

(defun ks (l max)
  "browse the list to find the best subset to put in the f***ing knapsack"
    (cond
     ((null (cdr l)) (list (car l)))
     (t
      (let*
          ((x (ks1 l max))
           (y (ks (cdr l) max)))
        (cond
         ((> (totval x) (totval y)) x)
         (t y))))))

        (ks '((map 9 150) (compass 13 35) (water 153 200) (sandwich 50 160)
              (glucose 15 60) (tin 68 45)(banana 27 60) (apple 39 40)
              (cheese 23 30) (beer 52 10) (cream 11 70) (camera 32 30)
              (T-shirt 24 15) (trousers 48 10) (umbrella 73 40)
              (waterproof-trousers 42 70) (overclothes 43 75) (notecase 22 80)
              (glasses 7 20) (towel 18 12) (socks 4 50) (book 30 10)) 400)

```


{{out}} with org-babel in Emacs

```txt

| map                 |   9 |  150 |
| socks               |   4 |   50 |
| cream               |  11 |   70 |
| glucose             |  15 |   60 |
| notecase            |  22 |   80 |
| sandwich            |  50 |  160 |
| glasses             |   7 |   20 |
| compass             |  13 |   35 |
| banana              |  27 |   60 |
| overclothes         |  43 |   75 |
| waterproof-trousers |  42 |   70 |
| water               | 153 |  200 |
|                     | 396 | 1030 |


```



## Erlang



```Erlang


-module(knapsack_0_1).

-export([go/0,
         solve/5]).

-define(STUFF,
        [{"map",                      9,   150},
         {"compass",                 13,    35},
         {"water",                  153,   200},
         {"sandwich",                50,   160},
         {"glucose",                 15,    60},
         {"tin",                     68,    45},
         {"banana",                  27,    60},
         {"apple",                   39,    40},
         {"cheese",                  23,    30},
         {"beer",                    52,    10},
         {"suntan cream",            11,    70},
         {"camera",                  32,    30},
         {"T-shirt",                 24,    15},
         {"trousers",                48,    10},
         {"umbrella",                73,    40},
         {"waterproof trousers",     42,    70},
         {"waterproof overclothes",  43,    75},
         {"note-case",               22,    80},
         {"sunglasses",               7,    20},
         {"towel",                   18,    12},
         {"socks",                    4,    50},
         {"book",                    30,    10}
        ]).

-define(MAX_WEIGHT, 400).

go() ->
    StartTime = os:timestamp(),
    {ItemList, TotalValue, TotalWeight} =
        solve(?STUFF, ?MAX_WEIGHT, [], 0, 0),
    TimeElapsed = timer:now_diff(os:timestamp(), StartTime),
    io:format("Items: ~n"),
    [io:format("~p~n", [Item]) || Item <- ItemList],
    io:format(
      "Total value: ~p~nTotal weight: ~p~nTime elapsed in milliseconds: ~p~n",
      [TotalValue, TotalWeight, TimeElapsed/1000]).

solve([], _TotalWeight, ItemAcc, ValueAcc, WeightAcc) ->
    {ItemAcc, ValueAcc, WeightAcc};
solve([{_Item, ItemWeight, _ItemValue} | T],
      TotalWeight,
      ItemAcc,
      ValueAcc,
      WeightAcc) when ItemWeight > TotalWeight ->
    solve(T, TotalWeight, ItemAcc, ValueAcc, WeightAcc);
solve([{ItemName, ItemWeight, ItemValue} | T],
      TotalWeight,
      ItemAcc,
      ValueAcc,
      WeightAcc) ->
    {_TailItemAcc, TailValueAcc, _TailWeightAcc} = TailRes =
        solve(T, TotalWeight, ItemAcc, ValueAcc, WeightAcc),
    {_HeadItemAcc, HeadValueAcc, _HeadWeightAcc} = HeadRes =
        solve(T,
              TotalWeight - ItemWeight,
              [ItemName | ItemAcc],
              ValueAcc + ItemValue,
              WeightAcc + ItemWeight),

    case TailValueAcc > HeadValueAcc of
        true ->
            TailRes;
        false ->
            HeadRes
    end.

```


{{out}}

```txt

1> knapsack_0_1:go().
Items:
"socks"
"sunglasses"
"note-case"
"waterproof overclothes"
"waterproof trousers"
"suntan cream"
"banana"
"glucose"
"sandwich"
"water"
"compass"
"map"
Total value: 1030
Total weight: 396
Time elapsed in milliseconds: 133.445
ok

```



## Euler Math Toolbox



```Euler Math Toolbox

>items=["map","compass","water","sandwich","glucose", ...
>  "tin","banana","apple","cheese","beer","suntan creame", ...
>  "camera","t-shirt","trousers","umbrella","waterproof trousers", ...
>  "waterproof overclothes","note-case","sunglasses", ...
>  "towel","socks","book"];
>ws = [9,13,153,50,15,68,27,39,23,52,11, ...
>  32,24,48,73,42,43,22,7,18,4,30];
>vs = [150,35,200,160,60,45,60,40,30,10,70, ...
>  30,15,10,40,70,75,80,20,12,50,10];
>A=ws_id(cols(ws));
>c=vs;
>b=[400]_ones(cols(vs),1);
>sol = intsimplex(A,b,c,eq=-1,>max,>check);
>items[nonzeros(sol)]
 map
 compass
 water
 sandwich
 glucose
 banana
 suntan creame
 waterproof trousers
 waterproof overclothes
 note-case
 sunglasses
 socks

```


=={{header|F_Sharp|F#}}==
===Using A* Algorithm===

```fsharp

//Solve Knapsack 0-1 using A* algorithm
let knapStar items maxW=
  let l=List.length items
  let p=System.Collections.Generic.SortedSet<float*int*float*float*list<int>>() //H*; level; value of items taken so far; weight so far
  p.Add (0.0,0,0.0,0.0,[])|>ignore
  let H items maxW=let rec H n g a=match g with |(_,w,v)::e->let t=n+w
                                                             if t<=maxW then H t e (a+v) else a+(v/w)*(maxW-n)
                                                |_->a
                   H 0.0 items 0.0
  let pAdd ((h,_,_,_,_) as n) bv=if h>bv then p.Add n |> ignore
  let fH n (bv,t) w' v' t'=let _,w,v=List.item n items
                           let e=max bv (if w<=(maxW-w') then v'+v else bv)
                           let rt=n::t'
                           if n+1<l then pAdd ((v'+H (List.skip (n+1) items) maxW),n+1,v',w',t') bv
                                         if w<=(maxW-w') then pAdd ((v'+v+H (List.skip (n+1) items) (maxW-w')),n+1,v'+v,w'+w,rt) bv
                           if e>bv then (e,rt) else (bv,t)
  let rec fN (bv,t)=
    let h,zl,zv,zw,zt as r=p.Max
    p.Remove r |> ignore
    if bv>=h then t else fN (fH zl (bv,t) zw zv zt)
  fN (fH 0 (0.0,[]) 0.0 0.0 [])

```

{{out}}

```fsharp

let itemsf = [
  "map",                     9.0,  150.0;
  "compass",                13.0,   35.0;
  "water",                 153.0,  200.0;
  "sandwich",               50.0,  160.0;
  "glucose",                15.0,   60.0;
  "tin",                    68.0,   45.0;
  "banana",                 27.0,   60.0;
  "apple",                  39.0,   40.0;
  "cheese",                 23.0,   30.0;
  "beer",                   52.0,   10.0;
  "suntan cream",           11.0,   70.0;
  "camera",                 32.0,   30.0;
  "t-shirt",                24.0,   15.0;
  "trousers",               48.0,   10.0;
  "umbrella",               73.0,   40.0;
  "waterproof trousers",    42.0,   70.0;
  "waterproof overclothes", 43.0,   75.0;
  "note-case",              22.0,   80.0;
  "sunglasses",              7.0,   20.0;
  "towel",                  18.0,   12.0;
  "socks",                   4.0,   50.0;
  "book",                   30.0,   10.0;]|> List.sortBy(fun(_,n,g)->n/g)

```


```txt

> let x=knapStar itemsf 400.0;;
> x|>Seq.map (fun n->Seq.item n itemsf)|>Seq.sumBy(fun (_,_,n)->(+n));;
val it : float = 1030.0
> x|>Seq.map (fun n->Seq.item n itemsf)|>Seq.sumBy(fun (_,n,_)->(+n));;
val it : float = 396.0
> x|>Seq.iter(fun n->printfn "%A" (List.item n itemsf));;
("map", 9.0, 150.0)
("socks", 4.0, 50.0)
("suntan cream", 11.0, 70.0)
("glucose", 15.0, 60.0)
("note-case", 22.0, 80.0)
("sandwich", 50.0, 160.0)
("sunglasses", 7.0, 20.0)
("compass", 13.0, 35.0)
("banana", 27.0, 60.0)
("waterproof overclothes", 43.0, 75.0)
("waterproof trousers", 42.0, 70.0)
("water", 153.0, 200.0)

```



## Factor

Using dynamic programming:

```factor
USING: accessors arrays fry io kernel locals make math
math.order math.parser math.ranges sequences sorting ;
IN: rosetta.knappsack.0-1

TUPLE: item
    name weight value ;

CONSTANT: items {
        T{ item f "map" 9 150 }
        T{ item f "compass" 13 35 }
        T{ item f "water" 153 200 }
        T{ item f "sandwich" 50 160 }
        T{ item f "glucose" 15 60 }
        T{ item f "tin" 68 45 }
        T{ item f "banana" 27 60 }
        T{ item f "apple" 39 40 }
        T{ item f "cheese" 23 30 }
        T{ item f "beer" 52 10 }
        T{ item f "suntan cream" 11 70 }
        T{ item f "camera" 32 30 }
        T{ item f "t-shirt" 24 15 }
        T{ item f "trousers" 48 10 }
        T{ item f "umbrella" 73 40 }
        T{ item f "waterproof trousers" 42 70 }
        T{ item f "waterproof overclothes" 43 75 }
        T{ item f "note-case" 22 80 }
        T{ item f "sunglasses" 7 20 }
        T{ item f "towel" 18 12 }
        T{ item f "socks" 4 50 }
        T{ item f "book" 30 10 }
    }

CONSTANT: limit 400

: make-table ( -- table )
    items length 1 + [ limit 1 + 0 <array> ] replicate ;

:: iterate ( item-no table -- )
    item-no table nth :> prev
    item-no 1 + table nth :> curr
    item-no items nth :> item
    limit [1,b] [| weight |
        weight prev nth
        weight item weight>> - dup 0 >=
        [ prev nth item value>> + max ]
        [ drop ] if
        weight curr set-nth
    ] each ;

: fill-table ( table -- )
    [ items length iota ] dip
    '[ _ iterate ] each ;

:: extract-packed-items ( table -- items )
    [
        limit :> weight!
        items length iota <reversed> [| item-no |
            item-no table nth :> prev
            item-no 1 + table nth :> curr
            weight [ curr nth ] [ prev nth ] bi =
            [
                item-no items nth
                [ name>> , ] [ weight>> weight swap - weight! ] bi
            ] unless
        ] each
    ] { } make ;

: solve-knappsack ( -- items value )
    make-table [ fill-table ]
    [ extract-packed-items ] [ last last ] tri ;

: main ( -- )
    solve-knappsack
    "Total value: " write number>string print
    "Items packed: " print
    natural-sort
    [ "   " write print ] each ;
```


```txt

 ( scratchpad ) main
 Total value: 1030
 Items packed:
    banana
    compass
    glucose
    map
    note-case
    sandwich
    socks
    sunglasses
    suntan cream
    water
    waterproof overclothes
    waterproof trousers

```



## Forth


```Forth

\ Rosetta Code Knapp-sack 0-1 problem.  Tested under GForth 0.7.3.
\ 22 items. On current processors a set fits nicely in one CELL (32 or 64 bits).
\ Brute force approach: for every possible set of 22 items,
\ check for admissible solution then for optimal set.

: offs HERE over - ;
        400 VALUE WLIMIT
        0 VALUE ITEM
        0 VALUE VAL
        0 VALUE /ITEM
        0 VALUE ITEMS#
Create Sack
HERE
        9 ,                     offs TO VAL
        150 ,                   offs TO ITEM
        s" map            " s,  offs TO /ITEM
DROP
 13 ,  35 , s" compass        " s,
153 , 200 , s" water          " s,
 50 , 160 , s" sandwich       " s,
 15 ,  60 , s" glucose        " s,
 68 ,  45 , s" tin            " s,
 27 ,  60 , s" banana         " s,
 39 ,  40 , s" apple          " s,
 23 ,  30 , s" cheese         " s,
 52 ,  10 , s" beer           " s,
 11 ,  70 , s" suntan cream   " s,
 32 ,  30 , s" camera         " s,
 24 ,  15 , s" T-shirt        " s,
 48 ,  10 , s" trousers       " s,
 73 ,  40 , s" umbrella       " s,
 42 ,  70 , s" wp trousers    " s,
 43 ,  75 , s" wp overclothes " s,
 22 ,  80 , s" note-case      " s,
  7 ,  20 , s" sunglasses     " s,
 18 ,  12 , s" towel          " s,
  4 ,  50 , s" socks          " s,
 30 ,  10 , s" book           " s,
        HERE VALUE END-SACK
        VARIABLE Sol            \ Solution  Set
        VARIABLE Vmax           \ Temporary Maximum Value
        VARIABLE Sum            \ Temporary Sum (for speed-up)
: ]sum          ( Rtime: set -- sum  ;Ctime: hilimit.a start.a -- )
\ Loop unwinding & precomputing addresses
        ]
        ]] Sum OFF [[
        DO              ]] dup [[  1  ]] LITERAL AND IF [[  I  ]] LITERAL @ Sum +! THEN 2/ [[
        /ITEM +LOOP     ]] drop Sum @ [[
; IMMEDIATE
: solve         ( -- )
        Vmax OFF
        [ 1 END-SACK Sack - /ITEM / lshift 1- ]L 0
        DO
                I [ END-SACK Sack ]sum ( by weight ) WLIMIT <
                IF
                        I [ END-SACK VAL + Sack VAL + ]sum ( by value )
                        dup Vmax @ >
                        IF  Vmax ! I Sol !  ELSE  drop  THEN
                THEN
        LOOP
;
: .solution     ( -- )
        Sol @ END-SACK ITEM + Sack ITEM +
        DO
                dup 1 AND  IF  I count type cr  THEN
                2/
        /ITEM +LOOP
        drop
        ." Weight: " Sol @ [ END-SACK Sack ]sum .  ."  Value: " Sol @ [ END-SACK VAL + Sack VAL + ]sum .
;

```

{{out}}

```txt

map
compass
water
sandwich
glucose
banana
suntan cream
wp trousers
wp overclothes
note-case
sunglasses
socks
Weight: 396  Value: 1030

```



## FutureBasic


```futurebasic

output file "Knapsack Problem Solution"

include "ConsoleWindow"

def tab 20

_numberOfObjects = 21
_weightOfKnapsack = 400

dim as short n : n = _numberOfObjects     /* The number of objects available to pack */
dim as Str31 s(_numberOfObjects)     /* The names of available objects */
dim as short c(_numberOfObjects)     /* The *COST* of the ith object i.e. how much weight you must carry to pack the object */
dim as short v(_numberOfObjects)     /* The *VALUE* of the ith object i.e. on a scale of 1 to 200, how important is it that the object included */
dim as short W : W = _weightOfKnapsack    /* The maximum weight your knapsack will carry in ounces*/

s(0) = "map"
s(1) = "compass"
s(2) = "water"
s(3) = "sandwich"
s(4) = "glucose"
s(5) = "tin"
s(6) = "banana"
s(7) = "apple"
s(8) = "cheese"
s(9) = "beer"
s(10) = "suntan cream"
s(11) = "camera"
s(12) = "T-shirt"
s(13) = "trousers"
s(14) = "umbrella"
s(15) = "waterproof pants"
s(16) = "raincoat"
s(17) = "note-case"
s(18) = "sunglasses"
s(19) = "towel"
s(20) = "socks"
s(21) = "socks"

c(0) = 9
c(1) = 13
c(2) = 153
c(3) = 50
c(4) = 15
c(5) = 68
c(6) = 27
c(7) = 39
c(8) = 23
c(9) = 52
c(10) = 11
c(11) = 32
c(12) = 24
c(13) = 48
c(14) = 73
c(15) = 42
c(16) = 43
c(17) = 22
c(18) = 7
c(19) = 18
c(20) = 4
c(21) = 30

v(0) = 150
v(1) = 35
v(2) = 200
v(3) = 160
v(4) = 60
v(5) = 45
v(6) = 60
v(7) = 40
v(8) = 30
v(9) = 10
v(10) = 70
v(11) = 30
v(12) = 15
v(13) = 10
v(14) = 40
v(15) = 70
v(16) = 75
v(17) = 80
v(18) = 20
v(19) = 12
v(20) = 50
v(21) = 10


local fn FillKnapsack
dim as short  cur_w
dim as double tot_v : tot_v = 0
dim as short  i, maxi, finalWeight : finalWeight = 0
dim as short  finalValue : finalValue = 0
dim as short  used(_numberOfObjects)

for i = 0 to n
   used(i) = 0
next

cur_w = W
while cur_w > -1

   maxi = -1

   BeginCCode
   for ( i = 0; i < n; ++i)
      if ((used[i] == 0) && ((maxi == -1) || ((float)v[i]/c[i] > (float)v[maxi]/c[maxi])))
   maxi = i;
   EndC

   used(maxi) = 1
   cur_w -= c(maxi)
   tot_v += v(maxi)

   if (cur_w >= 0)
      print s(maxi), c(maxi), v(maxi)

      finalWeight = finalWeight + c(maxi)
      finalValue = finalValue + v(maxi)

   else
      print
      print "Add"; int( ( (double)cur_w/c(maxi) * 100 ) +100 ); "% more of "; s(maxi); " into the knapsack to fill remaining space."

      tot_v -= v(maxi)
      tot_v += (1 + (double )cur_w/c(maxi)) * v(maxi)
   end if
wend

print
print "Filled the bag with objects whose total value is"; finalValue; "."
print "Total weight of packed objects is"; finalWeight; " ounces."

end fn

dim as short i, totalValue, totalWeight

print
print "Available Items", "Weight in ounces", "Value (Scale of 1 to 200)"
for i = 0 to _numberOfObjects
   print s(i), c(i), v(i)
   totalValue += v(i)
   totalWeight += c(i)
next

print
print "Total capacity of knapsack:"; W; " ounces"; "."
print "Total value of all"; _numberOfObjects; " objects:"; totalValue; "."
print "Total weight of all"; _numberOfObjects; " objects:"; totalWeight; " ounces."
print
print
print "Most optimal packing considering weight and value:"
print
print "Item", "Weight", "Value"

fn FillKnapsack

```


Output:

```txt


Available Items     Weight in ounces    Value (Scale of 1 to 200)
map                  9                   150
compass              13                  35
water                153                 200
sandwich             50                  160
glucose              15                  60
tin                  68                  45
banana               27                  60
apple                39                  40
cheese               23                  30
beer                 52                  10
suntan cream         11                  70
camera               32                  30
T-shirt              24                  15
trousers             48                  10
umbrella             73                  40
waterproof pants     42                  70
raincoat             43                  75
note-case            22                  80
sunglasses           7                   20
towel                18                  12
socks                4                   50
socks                30                  10

Total capacity of knapsack: 400 ounces.
Total value of all 21 objects: 1272.
Total weight of all 21 objects: 803 ounces.


Most optimal packing considering weight and value:

Item                Weight              Value
map                  9                   150
socks                4                   50
suntan cream         11                  70
glucose              15                  60
note-case            22                  80
sandwich             50                  160
sunglasses           7                   20
compass              13                  35
banana               27                  60
raincoat             43                  75
waterproof pants     42                  70
water                153                 200

Add 17% more of cheese into the knapsack to fill remaining space.

Filled the bag with objects whose total value is 1030.
Total weight of packed objects is 396 ounces.

```



## Go

From WP, "0-1 knapsack problem" under [http://en.wikipedia.org/wiki/Knapsack_problem#Dynamic_Programming_Algorithm|Solving The Knapsack Problem], although the solution here simply follows the recursive defintion and doesn't even use the array optimization.

```go
package main

import "fmt"

type item struct {
    string
    w, v int
}

var wants = []item{
    {"map", 9, 150},
    {"compass", 13, 35},
    {"water", 153, 200},
    {"sandwich", 50, 160},
    {"glucose", 15, 60},
    {"tin", 68, 45},
    {"banana", 27, 60},
    {"apple", 39, 40},
    {"cheese", 23, 30},
    {"beer", 52, 10},
    {"suntan cream", 11, 70},
    {"camera", 32, 30},
    {"T-shirt", 24, 15},
    {"trousers", 48, 10},
    {"umbrella", 73, 40},
    {"waterproof trousers", 42, 70},
    {"waterproof overclothes", 43, 75},
    {"note-case", 22, 80},
    {"sunglasses", 7, 20},
    {"towel", 18, 12},
    {"socks", 4, 50},
    {"book", 30, 10},
}

const maxWt = 400

func main() {
    items, w, v := m(len(wants)-1, maxWt)
    fmt.Println(items)
    fmt.Println("weight:", w)
    fmt.Println("value:", v)
}

func m(i, w int) ([]string, int, int) {
    if i < 0 || w == 0 {
        return nil, 0, 0
    } else if wants[i].w > w {
        return m(i-1, w)
    }
    i0, w0, v0 := m(i-1, w)
    i1, w1, v1 := m(i-1, w-wants[i].w)
    v1 += wants[i].v
    if v1 > v0 {
        return append(i1, wants[i].string), w1 + wants[i].w, v1
    }
    return i0, w0, v0
}
```

{{out}}

```txt

[map compass water sandwich glucose banana suntan cream waterproof trousers waterproof overclothes note-case sunglasses socks]
weight: 396
value: 1030

```

'''Alternative test case'''

Data for which a greedy algorithm might give an incorrect result:

```go

var wants = []item{
    {"sunscreen", 15, 2},
    {"GPS", 25, 2},
    {"beer", 35, 3},
}

const maxWt = 40

```

{{out}}

```txt

[sunscreen GPS]
weight: 40
value: 4

```



## Groovy

Solution #1: brute force

```groovy
def totalWeight = { list -> list*.weight.sum() }
def totalValue = { list -> list*.value.sum() }

def knapsack01bf = { possibleItems ->
    possibleItems.subsequences().findAll{ ss ->
        def w = totalWeight(ss)
        350 < w && w < 401
    }.max(totalValue)
}
```

Solution #2: dynamic programming

```groovy
def knapsack01dp = { possibleItems ->
    def n = possibleItems.size()
    def m = (0..n).collect{ i -> (0..400).collect{ w -> []} }
    (1..400).each { w ->
        (1..n).each { i ->
            def wi = possibleItems[i-1].weight
            m[i][w] = wi > w ? m[i-1][w] : ([m[i-1][w], m[i-1][w-wi] + [possibleItems[i-1]]].max(totalValue))
        }
    }
    m[n][400]
}
```

Test:

```groovy
def items = [
        [name:"map", weight:9, value:150],
        [name:"compass", weight:13, value:35],
        [name:"water", weight:153, value:200],
        [name:"sandwich", weight:50, value:160],
        [name:"glucose", weight:15, value:60],
        [name:"tin", weight:68, value:45],
        [name:"banana", weight:27, value:60],
        [name:"apple", weight:39, value:40],
        [name:"cheese", weight:23, value:30],
        [name:"beer", weight:52, value:10],
        [name:"suntan cream", weight:11, value:70],
        [name:"camera", weight:32, value:30],
        [name:"t-shirt", weight:24, value:15],
        [name:"trousers", weight:48, value:10],
        [name:"umbrella", weight:73, value:40],
        [name:"waterproof trousers", weight:42, value:70],
        [name:"waterproof overclothes", weight:43, value:75],
        [name:"note-case", weight:22, value:80],
        [name:"sunglasses", weight:7, value:20],
        [name:"towel", weight:18, value:12],
        [name:"socks", weight:4, value:50],
        [name:"book", weight:30, value:10],
]

[knapsack01bf, knapsack01dp].each { knapsack01 ->
    def start = System.currentTimeMillis()
    def packingList = knapsack01(items)
    def elapsed = System.currentTimeMillis() - start

    println "\n\n\nElapsed Time: ${elapsed/1000.0} s"
    println "Total Weight: ${totalWeight(packingList)}"
    println " Total Value: ${totalValue(packingList)}"
    packingList.each {
        printf ("  item: %-25s  weight:%4d  value:%4d\n", it.name, it.weight, it.value)
    }
}
```

{{out}}

```txt
Elapsed Time: 132.267 s
Total Weight: 396
 Total Value: 1030
  item: map                        weight:   9  value: 150
  item: compass                    weight:  13  value:  35
  item: water                      weight: 153  value: 200
  item: sandwich                   weight:  50  value: 160
  item: glucose                    weight:  15  value:  60
  item: banana                     weight:  27  value:  60
  item: suntan cream               weight:  11  value:  70
  item: waterproof trousers        weight:  42  value:  70
  item: waterproof overclothes     weight:  43  value:  75
  item: note-case                  weight:  22  value:  80
  item: sunglasses                 weight:   7  value:  20
  item: socks                      weight:   4  value:  50



Elapsed Time: 0.27 s
Total Weight: 396
 Total Value: 1030
  item: map                        weight:   9  value: 150
  item: compass                    weight:  13  value:  35
  item: water                      weight: 153  value: 200
  item: sandwich                   weight:  50  value: 160
  item: glucose                    weight:  15  value:  60
  item: banana                     weight:  27  value:  60
  item: suntan cream               weight:  11  value:  70
  item: waterproof trousers        weight:  42  value:  70
  item: waterproof overclothes     weight:  43  value:  75
  item: note-case                  weight:  22  value:  80
  item: sunglasses                 weight:   7  value:  20
  item: socks                      weight:   4  value:  50
```



## Haskell

Brute force:

```haskell
inv = [("map",9,150), ("compass",13,35), ("water",153,200), ("sandwich",50,160),
	("glucose",15,60), ("tin",68,45), ("banana",27,60), ("apple",39,40),
	("cheese",23,30), ("beer",52,10), ("cream",11,70), ("camera",32,30),
	("tshirt",24,15), ("trousers",48,10), ("umbrella",73,40), ("trousers",42,70),
	("overclothes",43,75), ("notecase",22,80), ("sunglasses",7,20), ("towel",18,12),
	("socks",4,50), ("book",30,10)]

-- get all combos of items under total weight sum; returns value sum and list
combs [] _ = [ (0, []) ]
combs ((name,w,v):rest) cap = combs rest cap ++
		      if w > cap then [] else map (prepend (name,w,v)) (combs rest (cap - w))
		      	where prepend (name,w,v) (v2, lst) = (v2 + v, (name,w,v):lst)

main = do
	putStr "Total value: "; print value
	mapM_ print items
		where (value, items) = maximum $ combs inv 400
```

{{out}}

```txt

Total value: 1030
("map",9,150)
("compass",13,35)
("water",153,200)
("sandwich",50,160)
("glucose",15,60)
("banana",27,60)
("cream",11,70)
("trousers",42,70)
("overclothes",43,75)
("notecase",22,80)
("sunglasses",7,20)
("socks",4,50)

```

Much faster brute force solution that computes the maximum before prepending, saving most of the prepends:

```haskell
inv = [("map",9,150), ("compass",13,35), ("water",153,200), ("sandwich",50,160),
	("glucose",15,60), ("tin",68,45), ("banana",27,60), ("apple",39,40),
	("cheese",23,30), ("beer",52,10), ("cream",11,70), ("camera",32,30),
	("tshirt",24,15), ("trousers",48,10), ("umbrella",73,40), ("trousers",42,70),
	("overclothes",43,75), ("notecase",22,80), ("sunglasses",7,20), ("towel",18,12),
	("socks",4,50), ("book",30,10)]

combs [] _ = (0, [])
combs ((name,w,v):rest) cap
	| w <= cap  = max skipthis $ prepend (name,w,v) (combs rest (cap - w))
	| otherwise = skipthis
	where	prepend (name,w,v) (v2, lst) = (v2 + v, (name,w,v):lst)
		skipthis = combs rest cap

main = do print $ combs inv 400
```

{{out}}

```txt
(1030,[("map",9,150),("compass",13,35),("water",153,200),("sandwich",50,160),("glucose",15,60),("banana",27,60),("cream",11,70),("trousers",42,70),("overclothes",43,75),("notecase",22,80),("sunglasses",7,20),("socks",4,50)])
```


Dynamic programming with a list for caching (this can be adapted to bounded problem easily):

```haskell
inv = [("map",9,150), ("compass",13,35), ("water",153,200), ("sandwich",50,160),
       ("glucose",15,60), ("tin",68,45), ("banana",27,60), ("apple",39,40),
       ("cheese",23,30), ("beer",52,10), ("cream",11,70), ("camera",32,30),
       ("tshirt",24,15), ("trousers",48,10), ("umbrella",73,40),
       ("waterproof trousers",42,70), ("overclothes",43,75), ("notecase",22,80),
       ("sunglasses",7,20), ("towel",18,12), ("socks",4,50), ("book",30,10)]

knapsack = foldr addItem (repeat (0,[])) where
	addItem (name,w,v) list = left ++ zipWith max right newlist where
		newlist = map (\(val, names)->(val + v, name:names)) list
		(left,right) = splitAt w list

main = print $ (knapsack inv) !! 400
```

{{out}}
 (1030,["map","compass","water","sandwich","glucose","banana","cream","waterproof trousers","overclothes","notecase","sunglasses","socks"])

=={{header|Icon}} and {{header|Unicon}}==
Translation from Wikipedia pseudo-code.  Memoization can be enabled with a command line argument that causes the procedure definitions to be swapped which effectively hooks the procedure.

```Icon
link printf

global wants                    # items wanted for knapsack

procedure main(A) # kanpsack 0-1
   if !A == ("--trace"|"-t") then &trace := -1     # trace everything (debug)
   if !A == ("--memoize"|"-m") then m :=: Memo_m   # hook (swap) procedure

   printf("Knapsack-0-1: with maximum weight allowed=%d.\n",maxw  := 400)
   showwanted(wants := get_wants())
   showcontents(bag := m(*wants,maxw))
   printf("Performance: time=%d ms collections=%d\n",&time,&collections)
end

record packing(items,weight,value)

procedure Memo_m(i,w)           #: Hook procedure to memoize the knapsack
static memoT
initial memoT := table()
   return \memoT[k := i||","||w] | ( memoT[k] := Memo_m(i,w) )
end

procedure m(i,w)                #: Solve the Knapsack 0-1 as per Wikipedia
static nil
initial nil := packing([],0,0)
   if 0 = (i | w) then
      return nil
   else if wants[i].weight > w then
           return m(i-1, w)
        else {
            x0 := m(i-1,w)
            x1 := m(i-1,w-wants[i].weight)
            if ( x1.value + wants[i].value) > x0.value then
               return packing(x1.items ||| wants[i].items,
                              x1.weight + wants[i].weight,
                              x1.value + wants[i].value)
            else
               return x0
        }
end

procedure showwanted(wants)     #: show the list of wanted items
   every (tw := 0) +:= (!wants).weight
   printf("Packing list has total weight=%d and includes %d items [",tw,*wants)
   every printf(" %s",!(!wants).items|"]\n")
end

procedure showcontents(bag)     #: show the list of the packed bag
   printf("The bag weighs=%d holding %d items [",bag.weight,*bag.items)
   every printf(" %s",!bag.items|"]\n")
end

procedure get_wants()           #: setup list of wanted items
   return  [ packing(["map"], 9, 150),
             packing(["compass"], 13, 35),
             packing(["water"], 153, 200),
             packing(["sandwich"], 50, 160),
             packing(["glucose"], 15, 60),
             packing(["tin"], 68, 45),
             packing(["banana"], 27, 60),
             packing(["apple"], 39, 40),
             packing(["cheese"], 23, 30),
             packing(["beer"], 52, 10),
             packing(["suntan cream"], 11, 70),
             packing(["camera"], 32, 30),
             packing(["T-shirt"], 24, 15),
             packing(["trousers"], 48, 10),
             packing(["umbrella"], 73, 40),
             packing(["waterproof trousers"], 42, 70),
             packing(["waterproof overclothes"], 43, 75),
             packing(["note-case"], 22, 80),
             packing(["sunglasses"], 7, 20),
             packing(["towel"], 18, 12),
             packing(["socks"], 4, 50),
             packing(["book"], 30, 10) ]
end
```

{{libheader|Icon Programming Library}}
[http://www.cs.arizona.edu/icon/library/src/procs/printf.icn printf.icn provides printf]
{{out}}

```txt

Knapsack-0-1: with maximum weight allowed=400.
Packing list has total weight=803 and includes 22 items [ map compass water sandwich glucose tin banana apple cheese beer suntan cream camera T-shirt trousers umbrella waterproof trousers waterproof overclothes note-case sunglasses towel socks book ]
The bag weighs=396 holding 12 items [ map compass water sandwich glucose banana suntan cream waterproof trousers waterproof overclothes note-case sunglasses socks ]
Performance: time=37 ms collections=0

```

The above shows memoized performance.  Un-memoized results on the same PC took time=9728 ms collections=4.


## J

Static solution:

```J
'names values'=:|:".;._2]0 :0
  'map';                       9         150
  'compass';                  13          35
  'water';                   153         200
  'sandwich';                 50         160
  'glucose';                  15          60
  'tin';                      68          45
  'banana';                   27          60
  'apple';                    39          40
  'cheese';                   23          30
  'beer';                     52          10
  'suntan cream';             11          70
  'camera';                   32          30
  'tshirt';                   24          15
  'trousers';                 48          10
  'umbrella';                 73          40
  'waterproof trousers';      42          70
  'waterproof overclothes';   43          75
  'notecase';                 22          80
  'sunglasses';                7          20
  'towel';                    18          12
  'socks';                     4          50
  'book';                     30          10
)

X=: +/ .*"1
plausible=: (] (] #~ 400 >: X) #:@i.@(2&^)@#)@:({."1)
best=: (plausible ([ {~  [ (i. >./)@:X {:"1@]) ]) values
```

Illustration of answer:

```J
   +/best#values  NB. total weight and value
396 1030
   best#names
map
compass
water
sandwich
glucose
banana
suntan cream
waterproof trousers
waterproof overclothes
notecase
sunglasses
socks
```


'''Alternative test case'''


```J
'names values'=:|:".;._2]0 :0
    'sunscreen'; 15 2
    'GPS'; 25 2
    'beer'; 35 3
)

X=: +/ .*"1
plausible=: (] (] #~ 40 >: X) #:@i.@(2&^)@#)@:({."1)
best=: (plausible ([ {~  [ (i. >./)@:X {:"1@]) ]) values
```


Illustration:


```J
   +/best#values
40 4
   best#names
sunscreen
GPS
```



## Java

General dynamic solution after [[wp:Knapsack_problem#0-1_knapsack_problem|wikipedia]].

```java
package hu.pj.alg.test;

import hu.pj.alg.ZeroOneKnapsack;
import hu.pj.obj.Item;
import java.util.*;
import java.text.*;

public class ZeroOneKnapsackForTourists {

    public ZeroOneKnapsackForTourists() {
        ZeroOneKnapsack zok = new ZeroOneKnapsack(400); // 400 dkg = 400 dag = 4 kg

        // making the list of items that you want to bring
        zok.add("map", 9, 150);
        zok.add("compass", 13, 35);
        zok.add("water", 153, 200);
        zok.add("sandwich", 50, 160);
        zok.add("glucose", 15, 60);
        zok.add("tin", 68, 45);
        zok.add("banana", 27, 60);
        zok.add("apple", 39, 40);
        zok.add("cheese", 23, 30);
        zok.add("beer", 52, 10);
        zok.add("suntan cream", 11, 70);
        zok.add("camera", 32, 30);
        zok.add("t-shirt", 24, 15);
        zok.add("trousers", 48, 10);
        zok.add("umbrella", 73, 40);
        zok.add("waterproof trousers", 42, 70);
        zok.add("waterproof overclothes", 43, 75);
        zok.add("note-case", 22, 80);
        zok.add("sunglasses", 7, 20);
        zok.add("towel", 18, 12);
        zok.add("socks", 4, 50);
        zok.add("book", 30, 10);

        // calculate the solution:
        List<Item> itemList = zok.calcSolution();

        // write out the solution in the standard output
        if (zok.isCalculated()) {
            NumberFormat nf  = NumberFormat.getInstance();

            System.out.println(
                "Maximal weight           = " +
                nf.format(zok.getMaxWeight() / 100.0) + " kg"
            );
            System.out.println(
                "Total weight of solution = " +
                nf.format(zok.getSolutionWeight() / 100.0) + " kg"
            );
            System.out.println(
                "Total value              = " +
                zok.getProfit()
            );
            System.out.println();
            System.out.println(
                "You can carry the following materials " +
                "in the knapsack:"
            );
            for (Item item : itemList) {
                if (item.getInKnapsack() == 1) {
                    System.out.format(
                        "%1$-23s %2$-3s %3$-5s %4$-15s \n",
                        item.getName(),
                        item.getWeight(), "dag  ",
                        "(value = " + item.getValue() + ")"
                    );
                }
            }
        } else {
            System.out.println(
                "The problem is not solved. " +
                "Maybe you gave wrong data."
            );
        }

    }

    public static void main(String[] args) {
        new ZeroOneKnapsackForTourists();
    }

} // class
```


```java
package hu.pj.alg;

import hu.pj.obj.Item;
import java.util.*;

public class ZeroOneKnapsack {

    protected List<Item> itemList  = new ArrayList<Item>();
    protected int maxWeight        = 0;
    protected int solutionWeight   = 0;
    protected int profit           = 0;
    protected boolean calculated   = false;

    public ZeroOneKnapsack() {}

    public ZeroOneKnapsack(int _maxWeight) {
        setMaxWeight(_maxWeight);
    }

    public ZeroOneKnapsack(List<Item> _itemList) {
        setItemList(_itemList);
    }

    public ZeroOneKnapsack(List<Item> _itemList, int _maxWeight) {
        setItemList(_itemList);
        setMaxWeight(_maxWeight);
    }

    // calculte the solution of 0-1 knapsack problem with dynamic method:
    public List<Item> calcSolution() {
        int n = itemList.size();

        setInitialStateForCalculation();
        if (n > 0  &&  maxWeight > 0) {
            List< List<Integer> > c = new ArrayList< List<Integer> >();
            List<Integer> curr = new ArrayList<Integer>();

            c.add(curr);
            for (int j = 0; j <= maxWeight; j++)
                curr.add(0);
            for (int i = 1; i <= n; i++) {
                List<Integer> prev = curr;
                c.add(curr = new ArrayList<Integer>());
                for (int j = 0; j <= maxWeight; j++) {
                    if (j > 0) {
                        int wH = itemList.get(i-1).getWeight();
                        curr.add(
                            (wH > j)
                            ?
                            prev.get(j)
                            :
                            Math.max(
                                prev.get(j),
                                itemList.get(i-1).getValue() + prev.get(j-wH)
                            )
                        );
                    } else {
                        curr.add(0);
                    }
                } // for (j...)
            } // for (i...)
            profit = curr.get(maxWeight);

            for (int i = n, j = maxWeight; i > 0  &&  j >= 0; i--) {
                int tempI   = c.get(i).get(j);
                int tempI_1 = c.get(i-1).get(j);
                if (
                    (i == 0  &&  tempI > 0)
                    ||
                    (i > 0  &&  tempI != tempI_1)
                )
                {
                    Item iH = itemList.get(i-1);
                    int  wH = iH.getWeight();
                    iH.setInKnapsack(1);
                    j -= wH;
                    solutionWeight += wH;
                }
            } // for()
            calculated = true;
        } // if()
        return itemList;
    }

    // add an item to the item list
    public void add(String name, int weight, int value) {
        if (name.equals(""))
            name = "" + (itemList.size() + 1);
        itemList.add(new Item(name, weight, value));
        setInitialStateForCalculation();
    }

    // add an item to the item list
    public void add(int weight, int value) {
        add("", weight, value); // the name will be "itemList.size() + 1"!
    }

    // remove an item from the item list
    public void remove(String name) {
        for (Iterator<Item> it = itemList.iterator(); it.hasNext(); ) {
            if (name.equals(it.next().getName())) {
                it.remove();
            }
        }
        setInitialStateForCalculation();
    }

    // remove all items from the item list
    public void removeAllItems() {
        itemList.clear();
        setInitialStateForCalculation();
    }

    public int getProfit() {
        if (!calculated)
            calcSolution();
        return profit;
    }

    public int getSolutionWeight() {return solutionWeight;}
    public boolean isCalculated() {return calculated;}
    public int getMaxWeight() {return maxWeight;}

    public void setMaxWeight(int _maxWeight) {
        maxWeight = Math.max(_maxWeight, 0);
    }

    public void setItemList(List<Item> _itemList) {
        if (_itemList != null) {
            itemList = _itemList;
            for (Item item : _itemList) {
                item.checkMembers();
            }
        }
    }

    // set the member with name "inKnapsack" by all items:
    private void setInKnapsackByAll(int inKnapsack) {
        for (Item item : itemList)
            if (inKnapsack > 0)
                item.setInKnapsack(1);
            else
                item.setInKnapsack(0);
    }

    // set the data members of class in the state of starting the calculation:
    protected void setInitialStateForCalculation() {
        setInKnapsackByAll(0);
        calculated     = false;
        profit         = 0;
        solutionWeight = 0;
    }

} // class
```


```java
package hu.pj.obj;

public class Item {

    protected String name    = "";
    protected int weight     = 0;
    protected int value      = 0;
    protected int bounding   = 1; // the maximal limit of item's pieces
    protected int inKnapsack = 0; // the pieces of item in solution

    public Item() {}

    public Item(Item item) {
        setName(item.name);
        setWeight(item.weight);
        setValue(item.value);
        setBounding(item.bounding);
    }

    public Item(int _weight, int _value) {
        setWeight(_weight);
        setValue(_value);
    }

    public Item(int _weight, int _value, int _bounding) {
        setWeight(_weight);
        setValue(_value);
        setBounding(_bounding);
    }

    public Item(String _name, int _weight, int _value) {
        setName(_name);
        setWeight(_weight);
        setValue(_value);
    }

    public Item(String _name, int _weight, int _value, int _bounding) {
        setName(_name);
        setWeight(_weight);
        setValue(_value);
        setBounding(_bounding);
    }

    public void setName(String _name) {name = _name;}
    public void setWeight(int _weight) {weight = Math.max(_weight, 0);}
    public void setValue(int _value) {value = Math.max(_value, 0);}

    public void setInKnapsack(int _inKnapsack) {
        inKnapsack = Math.min(getBounding(), Math.max(_inKnapsack, 0));
    }

    public void setBounding(int _bounding) {
        bounding = Math.max(_bounding, 0);
        if (bounding == 0)
            inKnapsack = 0;
    }

    public void checkMembers() {
        setWeight(weight);
        setValue(value);
        setBounding(bounding);
        setInKnapsack(inKnapsack);
    }

    public String getName() {return name;}
    public int getWeight() {return weight;}
    public int getValue() {return value;}
    public int getInKnapsack() {return inKnapsack;}
    public int getBounding() {return bounding;}

} // class
```

{{out}}

```txt

Maximal weight           = 4 kg
Total weight of solution = 3,96 kg
Total value              = 1030

You can carry te following materials in the knapsack:
map                     9   dag   (value = 150)
compass                 13  dag   (value = 35)
water                   153 dag   (value = 200)
sandwich                50  dag   (value = 160)
glucose                 15  dag   (value = 60)
banana                  27  dag   (value = 60)
suntan cream            11  dag   (value = 70)
waterproof trousers     42  dag   (value = 70)
waterproof overclothes  43  dag   (value = 75)
note-case               22  dag   (value = 80)
sunglasses              7   dag   (value = 20)
socks                   4   dag   (value = 50)

```



## JavaScript

Also available at [https://gist.github.com/truher/4715551|this gist].

```javascript
/*global portviz:false, _:false */
/*
 * 0-1 knapsack solution, recursive, memoized, approximate.
 *
 * credits:
 *
 * the Go implementation here:
 *   http://rosettacode.org/mw/index.php?title=Knapsack_problem/0-1
 *
 * approximation details here:
 *   http://math.mit.edu/~goemans/18434S06/knapsack-katherine.pdf
 */
portviz.knapsack = {};
(function() {
  this.combiner = function(items, weightfn, valuefn) {
    // approximation guarantees result >= (1-e) * optimal
    var _epsilon = 0.01;
    var _p = _.max(_.map(items,valuefn));
    var _k = _epsilon * _p / items.length;

    var _memo = (function(){
      var _mem = {};
      var _key = function(i, w) {
        return i + '::' + w;
      };
      return {
        get: function(i, w) {
          return _mem[_key(i,w)];
        },
        put: function(i, w, r) {
          _mem[_key(i,w)]=r;
          return r;
        }
      };
    })();

    var _m = function(i, w) {

      i = Math.round(i);
      w = Math.round(w);


      if (i < 0 || w === 0) {
        // empty base case
        return {items: [], totalWeight: 0, totalValue: 0};
      }

      var mm = _memo.get(i,w);
      if (!_.isUndefined(mm)) {
        return mm;
      }

      var item = items[i];
      if (weightfn(item) > w) {
        //item does not fit, try the next item
        return _memo.put(i, w, _m(i-1, w));
      }
      // this item could fit.
      // are we better off excluding it?
      var excluded = _m(i-1, w);
      // or including it?
      var included = _m(i-1, w - weightfn(item));
      if (included.totalValue + Math.floor(valuefn(item)/_k) > excluded.totalValue) {
        // better off including it
        // make a copy of the list
        var i1 = included.items.slice();
        i1.push(item);
        return _memo.put(i, w,
          {items: i1,
           totalWeight: included.totalWeight + weightfn(item),
           totalValue: included.totalValue + Math.floor(valuefn(item)/_k)});
      }
      //better off excluding it
      return _memo.put(i,w, excluded);
    };
    return {
      /* one point */
      one: function(maxweight) {
        var scaled = _m(items.length - 1, maxweight);
        return {
          items: scaled.items,
          totalWeight: scaled.totalWeight,
          totalValue: scaled.totalValue * _k
        };
      },
      /* the entire EF */
      ef: function(maxweight, step) {
        return _.map(_.range(0, maxweight+1, step), function(weight) {
          var scaled = _m(items.length - 1, weight);
          return {
            items: scaled.items,
            totalWeight: scaled.totalWeight,
            totalValue: scaled.totalValue * _k
          };
        });
      }
    };
  };
}).apply(portviz.knapsack);

/*global portviz:false, _:false */
/*
 * after rosettacode.org/mw/index.php?title=Knapsack_problem/0-1
 */
var allwants = [
  {name:"map", weight:9, value: 150},
  {name:"compass", weight:13, value: 35},
  {name:"water", weight:153, value: 200},
  {name:"sandwich", weight: 50, value: 160},
  {name:"glucose", weight:15, value: 60},
  {name:"tin", weight:68, value: 45},
  {name:"banana", weight:27, value: 60},
  {name:"apple", weight:39, value: 40},
  {name:"cheese", weight:23, value: 30},
  {name:"beer", weight:52, value: 10},
  {name:"suntan cream", weight:11, value: 70},
  {name:"camera", weight:32, value: 30},
  {name:"T-shirt", weight:24, value: 15},
  {name:"trousers", weight:48, value: 10},
  {name:"umbrella", weight:73, value: 40},
  {name:"waterproof trousers", weight:42, value: 70},
  {name:"waterproof overclothes", weight:43, value: 75},
  {name:"note-case", weight:22, value: 80},
  {name:"sunglasses", weight:7, value: 20},
  {name:"towel", weight:18, value: 12},
  {name:"socks", weight:4, value: 50},
  {name:"book", weight:30, value: 10}
];

var near = function(actual, expected, tolerance) {
  if (expected === 0 && actual === 0) return true;
  if (expected === 0) {
    return Math.abs(expected - actual) / actual < tolerance;
  }
  return Math.abs(expected - actual) / expected < tolerance;
};

test("one knapsack", function() {
  var combiner =
    portviz.knapsack.combiner(allwants,
      function(x){return x.weight;},
      function(x){return x.value;});
  var oneport = combiner.one(400);
  ok(near(oneport.totalValue, 1030, 0.01), "correct total value");
  ok(near(oneport.totalValue, 1030, 0.01), "correct total value");
  equal(oneport.totalWeight, 396, "correct total weight");
});

test("frontier", function() {
  var combiner =
    portviz.knapsack.combiner(allwants,
      function(x){return x.weight;},
      function(x){return x.value;});
  var ef = combiner.ef(400, 1);
  equal(ef.length, 401, "401 because it includes the endpoints");
  ef = combiner.ef(400, 40);
  equal(ef.length, 11, "11 because it includes the endpoints");
  var expectedTotalValue = [
    0,
    330,
    445,
    590,
    685,
    755,
    810,
    860,
    902,
    960,
    1030
  ] ;
  _.each(ef, function(element, index) {
    // 15% error!  bleah!
    ok(near(element.totalValue, expectedTotalValue[index], 0.15),
      'actual ' + element.totalValue + ' expected ' + expectedTotalValue[index]);
  });
  deepEqual(_.pluck(ef, 'totalWeight'), [
    0,
    39,
    74,
    118,
    158,
    200,
    236,
    266,
    316,
    354,
    396
  ]);
  deepEqual(_.map(ef, function(x){return x.items.length;}), [
    0,
    4,
    6,
    7,
    9,
    10,
    10,
    12,
    14,
    11,
    12
   ]);
});
```



## jq

{{ works with|jq|1.4}}

"dynamic_knapsack(W)" implements a dynamic programming algorithm based
on computing m[i,W] as the maximum value that can be attained with
weight no greater than W using the first i items (with i = 0
corresponding to no items).  Here, m[i,W] is set to [V, ary]
where ary is an array of the names of the accepted items.

```jq
# Input should be the array of objects giving name, weight and value.
# Because of the way addition is defined on null and because of the
# way setpath works, there is no need to initialize the matrix m in
# detail.
def dynamic_knapsack(W):
  . as $objects
  | length as $n
  | reduce range(1; $n+1) as $i                           # i is the number of items
      # state: m[i][j] is an array of [value, array_of_object_names]
      (null;                           # see above remark about initialization of m
       $objects[$i-1] as $o
       | reduce range(0; W+1) as $j
           ( .;
             if $o.weight <= $j then
               .[$i-1][$j][0] as $v1                               # option 1: do not add this object
               | (.[$i-1][$j - $o.weight][0] + $o.value) as $v2    # option 2: add it
               | (if $v1 > $v2 then
                       [$v1, .[$i-1][$j][1]]                       # do not add this object
                  else [$v2, .[$i-1][$j - $o.weight][1]+[$o.name]] # add it
                  end) as $mx
               | .[$i][$j] = $mx
             else
                 .[$i][$j] = .[$i-1][$j]
             end))
  | .[$n][W];
```

'''Example''':

```jq
def objects: [
 {name: "map",                    "weight": 9,   "value": 150},
 {name: "compass",                "weight": 13,  "value": 35},
 {name: "water",                  "weight": 153, "value": 200},
 {name: "sandwich",               "weight": 50,  "value": 160},
 {name: "glucose",                "weight": 15,  "value": 60},
 {name: "tin",                    "weight": 68,  "value": 45},
 {name: "banana",                 "weight": 27,  "value": 60},
 {name: "apple",                  "weight": 39,  "value": 40},
 {name: "cheese",                 "weight": 23,  "value": 30},
 {name: "beer",                   "weight": 52,  "value": 10},
 {name: "suntancream",            "weight": 11,  "value": 70},
 {name: "camera",                 "weight": 32,  "value": 30},
 {name: "T-shirt",                "weight": 24,  "value": 15},
 {name: "trousers",               "weight": 48,  "value": 10},
 {name: "umbrella",               "weight": 73,  "value": 40},
 {name: "waterproof trousers",    "weight": 42,  "value": 70},
 {name: "waterproof overclothes", "weight": 43,  "value": 75},
 {name: "note-case",              "weight": 22,  "value": 80},
 {name: "sunglasses",             "weight": 7,   "value": 20},
 {name: "towel",                  "weight": 18,  "value": 12},
 {name: "socks",                  "weight": 4,   "value": 50},
 {name: "book",                   "weight": 30,  "value": 10}
];

objects | dynamic_knapsack(400)[]
```

{{out}}

```sh
$jq -M -c -n -f knapsack.jq
1030
["map","compass","water","sandwich","glucose","banana","suntancream","waterproof trousers","waterproof overclothes","note-case","sunglasses","socks"]
```



## Julia

This solution uses the [https://github.com/JuliaOpt/MathProgBase.jl MathProgBase] package (with the [https://github.com/JuliaOpt/Cbc.jl Cbc] solver package installed).  It is the <code>mixintprog</code> function from this package that does the heavy lifting of this solution.

<code>KPDSupply</code> has one more field than is needed, <code>quant</code>.  This field is may be useful in a solution to the bounded version of this task.

'''Type and Functions''':

```julia
struct KPDSupply{T<:Integer}
    item::String
    weight::T
    value::T
    quant::T
end

KPDSupply{T<:Integer}(itm::AbstractString, w::T, v::T, q::T=one(T)) = KPDSupply(itm, w, v, q)
Base.show(io::IO, kdps::KPDSupply) = print(io, kdps.quant, " ", kdps.item, " ($(kdps.weight) kg, $(kdps.value) €)")

using MathProgBase, Cbc
function solve(gear::Vector{<:KPDSupply}, capacity::Integer)
    w = getfield.(gear, :weight)
    v = getfield.(gear, :value)
    sol = mixintprog(-v, w', '<', capacity, :Bin, 0, 1, CbcSolver())
    gear[sol.sol .≈ 1]
end
```


'''Main''':

```julia
gear = [KPDSupply("map", 9, 150),
        KPDSupply("compass", 13, 35),
        KPDSupply("water", 153, 200),
        KPDSupply("sandwich", 50, 160),
        KPDSupply("glucose", 15, 60),
        KPDSupply("tin", 68, 45),
        KPDSupply("banana", 27, 60),
        KPDSupply("apple", 39, 40),
        KPDSupply("cheese", 23, 30),
        KPDSupply("beer", 52, 10),
        KPDSupply("suntan cream", 11, 70),
        KPDSupply("camera", 32, 30),
        KPDSupply("T-shirt", 24, 15),
        KPDSupply("trousers", 48, 10),
        KPDSupply("umbrella", 73, 40),
        KPDSupply("waterproof trousers", 42, 70),
        KPDSupply("waterproof overclothes", 43, 75),
        KPDSupply("note-case", 22, 80),
        KPDSupply("sunglasses", 7, 20),
        KPDSupply("towel", 18, 12),
        KPDSupply("socks", 4, 50),
        KPDSupply("book", 30, 10)]

pack = solve(gear, 400)
println("The hicker should pack: \n - ", join(pack, "\n - "))
println("\nPacked weight: ", mapreduce(x -> x.weight, +, pack), " kg")
println("Packed value: ", mapreduce(x -> x.value, +, pack), " €")
```


{{out}}

```txt
The hicker should pack:
 - 1 map (9 kg, 150 €)
 - 1 compass (13 kg, 35 €)
 - 1 water (153 kg, 200 €)
 - 1 sandwich (50 kg, 160 €)
 - 1 glucose (15 kg, 60 €)
 - 1 banana (27 kg, 60 €)
 - 1 suntan cream (11 kg, 70 €)
 - 1 waterproof trousers (42 kg, 70 €)
 - 1 waterproof overclothes (43 kg, 75 €)
 - 1 note-case (22 kg, 80 €)
 - 1 sunglasses (7 kg, 20 €)
 - 1 socks (4 kg, 50 €)

Packed weight: 396 kg
Packed value: 1030 €
```



## Kotlin

{{trans|Go}}

```scala
// version 1.1.2

data class Item(val name: String, val weight: Int, val value: Int)

val wants = listOf(
    Item("map", 9, 150),
    Item("compass", 13, 35),
    Item("water", 153, 200),
    Item("sandwich", 50, 160),
    Item("glucose", 15, 60),
    Item("tin", 68, 45),
    Item("banana", 27, 60),
    Item("apple", 39, 40),
    Item("cheese", 23, 30),
    Item("beer", 52, 10),
    Item("suntan cream", 11, 70),
    Item("camera", 32, 30),
    Item("T-shirt", 24, 15),
    Item("trousers", 48, 10),
    Item("umbrella", 73, 40),
    Item("waterproof trousers", 42, 70),
    Item("waterproof overclothes", 43, 75),
    Item("note-case", 22, 80),
    Item("sunglasses", 7, 20),
    Item("towel", 18, 12),
    Item("socks", 4, 50),
    Item("book", 30, 10)
)

const val MAX_WEIGHT = 400

fun m(i: Int, w: Int): Triple<MutableList<Item>, Int, Int> {
    val chosen = mutableListOf<Item>()
    if (i < 0 || w == 0) return Triple(chosen, 0, 0)
    else if (wants[i].weight > w) return m(i - 1, w)
    val (l0, w0, v0) = m(i - 1, w)
    var (l1, w1, v1) = m(i - 1, w - wants[i].weight)
    v1 += wants[i].value
    if (v1 > v0) {
        l1.add(wants[i])
        return Triple(l1, w1 + wants[i].weight, v1)
    }
    return Triple(l0, w0, v0)
}

fun main(args: Array<String>) {
    val (chosenItems, totalWeight, totalValue) = m(wants.size - 1, MAX_WEIGHT)
    println("Knapsack Item Chosen    Weight Value")
    println("----------------------  ------ -----")
    for (item in chosenItems.sortedByDescending { it.value} )
        println("${item.name.padEnd(24)}  ${"%3d".format(item.weight)}    ${"%3d".format(item.value)}")
    println("----------------------  ------ -----")
    println("Total ${chosenItems.size} Items Chosen     $totalWeight   $totalValue")
}
```


{{out}}

```txt

Knapsack Item Chosen    Weight Value
----------------------  ------ -----
water                     153    200
sandwich                   50    160
map                         9    150
note-case                  22     80
waterproof overclothes     43     75
suntan cream               11     70
waterproof trousers        42     70
glucose                    15     60
banana                     27     60
socks                       4     50
compass                    13     35
sunglasses                  7     20
----------------------  ------ -----
Total 12 Items Chosen     396   1030

```



## LSL

To test it yourself, rez a box on the ground, add the following as a New Script, create a notecard named "Knapsack_Problem_0_1_Data.txt" with the data shown below.

```LSL
string sNOTECARD = "Knapsack_Problem_0_1_Data.txt";
integer iMAX_WEIGHT = 400;
integer iSTRIDE = 4;
list lList = [];
default {
	integer iNotecardLine = 0;
	state_entry() {
		llOwnerSay("Reading '"+sNOTECARD+"'");
		llGetNotecardLine(sNOTECARD, iNotecardLine);
	}
	dataserver(key kRequestId, string sData) {
		if(sData==EOF) {
			//llOwnerSay("EOF");
			lList = llListSort(lList, iSTRIDE, FALSE);
			integer iTotalWeight = 0;
			integer iTotalValue = 0;
			list lKnapsack = [];
			integer x = 0;
			while(x*iSTRIDE<llGetListLength(lList)) {
				float fValueWeight = (float)llList2String(lList, x*iSTRIDE);
				string sItem = (string)llList2String(lList, x*iSTRIDE+1);
				integer iWeight = (integer)llList2String(lList, x*iSTRIDE+2);
				integer iValue = (integer)llList2String(lList, x*iSTRIDE+3);
				if(iTotalWeight+iWeight<iMAX_WEIGHT) {
					iTotalWeight += iWeight;
					iTotalValue += iValue;
					lKnapsack += [sItem, iWeight, iValue, fValueWeight];
				}
				x++;
			}
			for(x=0 ; x*iSTRIDE<llGetListLength(lKnapsack) ; x++) {
				llOwnerSay((string)x+": "+llList2String(lList, x*iSTRIDE+1)+", "+llList2String(lList, x*iSTRIDE+2)+", "+llList2String(lList, x*iSTRIDE+3));

			}
			llOwnerSay("iTotalWeight="+(string)iTotalWeight);
			llOwnerSay("iTotalValue="+(string)iTotalValue);
		} else {
			//llOwnerSay((string)iNotecardLine+": "+sData);
			if(llStringTrim(sData, STRING_TRIM)!="") {
				list lParsed = llParseString2List(sData, [","], []);
				string sItem = llStringTrim(llList2String(lParsed, 0), STRING_TRIM);
				integer iWeight = (integer)llStringTrim(llList2String(lParsed, 1), STRING_TRIM);
				integer iValue = (integer)llStringTrim(llList2String(lParsed, 2), STRING_TRIM);
				float fValueWeight = (1.0*iValue)/iWeight;
				lList += [fValueWeight, sItem, iWeight, iValue];
			}
			llGetNotecardLine(sNOTECARD, ++iNotecardLine);
		}
	}
}
```

Notecard:

```txt
map, 9, 150
compass, 13, 35
water, 153, 200
sandwich, 50, 160
glucose, 15, 60
tin, 68, 45
banana, 27, 60
apple, 39, 40
cheese, 23, 30
beer, 52, 10
suntan cream, 11, 70
camera, 32, 30
T-shirt, 24, 15
trousers, 48, 10
umbrella, 73, 40
waterproof trousers, 42, 70
waterproof overclothes, 43, 75
note-case, 22, 80
sunglasses, 7, 20
towel, 18, 12
socks, 4, 50
book, 30, 10
```

{{out}}

```txt
Reading 'Knapsack_Problem_0_1_Data.txt'
0: map, 9, 150
1: socks, 4, 50
2: suntan cream, 11, 70
3: glucose, 15, 60
4: note-case, 22, 80
5: sandwich, 50, 160
6: sunglasses, 7, 20
7: compass, 13, 35
8: banana, 27, 60
9: waterproof overclothes, 43, 75
10: waterproof trousers, 42, 70
11: water, 153, 200
iTotalWeight=396
iTotalValue=1030
```



## Maple


```Maple
weights := [9,13,153,50,15,68,27,39,23,52,11,32,24,48,73,42,43,22,7,18,4,30]:
vals := [150,35,200,160,60,45,60,40,30,10,70,30,15,10,40,70,75,80,20,12,50,10]:
items := ["map","compass","water","sandwich","glucose","tin","banana","apple","cheese","beer","suntan cream","camera","T-shirt","trousers","umbrella","waterproof trousers","waterproof overclothes","note-case","sunglasses","towel","socks","book"]:
acc := Array(1..numelems(vals)+1,1..400+1,1,fill=0):
len := numelems(weights):
for i from 2 to len+1 do #number of items picked + 1
	for j from 2 to 401 do #weight capacity left + 1
		if weights[i-1] > j-1 then
			acc[i,j] := acc[i-1, j]:
		else
			acc[i,j] := max(acc[i-1,j], acc[i-1, j-weights[i-1]]+vals[i-1]):
		end if:
	end do:
end do:
printf("Total Value is %d\n", acc[len+1, 401]):
count := 0:
i := len+1:
j := 401:
while (i>1 and j>1) do
	if acc[i,j] <> acc[i-1,j] then
		printf("Item: %s\n", items[i-1]):
		count := count+weights[i-1]:
		j := j-weights[i-1]:
		i := i-1:
	else
		i := i-1:
	end if:
end do:
printf("Total Weight is %d\n", count):
```

{{Out}}

```txt
Total Value is 1030
Item: socks
Item: sunglasses
Item: note-case
Item: waterproof overclothes
Item: waterproof trousers
Item: suntan cream
Item: banana
Item: glucose
Item: sandwich
Item: water
Item: compass
Item: map
Total Weight is 396
```



## Mathematica

Used the

```mathematica
#[[Flatten@
     Position[LinearProgramming[-#[[;; , 3]], -{#[[;; , 2]]}, -{400},
       {0, 1} & /@ #, Integers], 1], 1]] &@
 {{"map", 9, 150},
  {"compass", 13, 35},
  {"water", 153, 200},
  {"sandwich", 50, 160},
  {"glucose", 15, 60},
  {"tin", 68, 45},
  {"banana", 27, 60},
  {"apple", 39, 40},
  {"cheese", 23, 30},
  {"beer", 52, 10},
  {"suntan cream", 11, 70},
  {"camera", 32, 30},
  {"T-shirt", 24, 15},
  {"trousers", 48, 10},
  {"umbrella", 73, 40},
  {"waterproof trousers", 42, 70},
  {"waterproof overclothes", 43, 75},
  {"note-case", 22, 80},
  {"sunglasses", 7, 20},
  {"towel", 18, 12},
  {"socks", 4, 50},
  {"book", 30, 10}}
```

{{Out}}

```txt
{"map", "compass", "water", "sandwich", "glucose", "banana", "suntan cream", "waterproof trousers", "waterproof overclothes", "note-case", "sunglasses", "socks"}
```



## Mathprog


```mathprog
/*Knapsack

  This model finds the integer optimal packing of a knapsack

  Nigel_Galloway
  January 9th., 2012
*/

set Items;
param weight{t in Items};
param value{t in Items};

var take{t in Items}, binary;

knap_weight : sum{t in Items} take[t] * weight[t] <= 400;

maximize knap_value: sum{t in Items} take[t] * value[t];

data;

param : Items          : weight   value :=
         map		  9	   150
         compass          13	   35
         water		  153	   200
         sandwich	  50	   160
         glucose	  15	   60
         tin		  68	   45
         banana		  27	   60
         apple		  39	   40
         cheese		  23	   30
         beer		  52	   10
         suntancream	  11	   70
         camera		  32	   30
         T-shirt	  24	   15
         trousers	  48	   10
         umbrella	  73	   40
         w-trousers	  42	   70
         w-overclothes	  43	   75
         note-case	  22	   80
         sunglasses	  7        20
         towel		  18	   12
         socks		  4        50
         book		  30	   10
;

end;
```

The solution may be found at [[Knapsack problem/0-1/Mathprog]].
Activity=1 means take, Activity=0 means don't take.


## MAXScript


```MAXScript

global globalItems = #()
global usedMass = 0
global neededItems = #()
global totalValue = 0
struct kn_item
(
	item, weight, value
)

itemStrings = #("map#9#150","compass#13#35","water#153#200", \
		"sandwich#50#160","glucose#15#60","tin#68#45", \
		"banana#27#60","apple#39#40","cheese#23#30", \
		"beer#52#10","suntan cream#11#70","camera#32#30", \
		"T-shirt#24#15","trousers#48#10","umbrella#73#40", \
		"waterproof trousers#42#70","waterproof overclothes#43#75", \
		"note-case#22#80","sunglasses#7#20", "towel#18#12", \
		"socks#4#50","book#30#10")

fn sortByValue a b =
(
	if a[1].value > b[1].value then return -1
	else
	(
		if a[1].value == b[1].value then return 0
			else return 1
	)
)
fn chooseBestItem maximumWeight: items: =
(
	local itemsCopy = deepcopy items
	local possibleItems = #()
	for i = 1 to itemsCopy.count do
	(
		if itemsCopy[i].weight <= maximumWeight do append possibleItems (#(itemsCopy[i],i))
	)
	qsort possibleItems sortByValue
	if possibleItems.count > 0 then return possibleItems[1] else return 0
)

for i = 1 to itemStrings.count do
(
	local split = filterstring itemStrings[i] "#"
	local itemStruct = kn_item item:split[1] weight:(split[2] as integer) \
				value:(split[3] as integer)
	appendifunique globalItems itemstruct
)

while usedMass < 400 do
(
	local item = chooseBestItem maximumweight:(400-usedMass) items:(globalItems)
	if item != 0 then
	(
		deleteitem globalItems (item[2])
		appendifunique neededItems item[1]
		usedMass += item[1].weight
	) else exit
)
for i in neededitems do
(
	format "Item name: %, weight: %, value:%\n" i.item i.weight i.value
	totalValue += i.value
)
format "Total mass: %, Total Value: %\n" usedMass totalValue

```

{{out}}

```MAXScript

Item name: water, weight: 153, value:200
Item name: sandwich, weight: 50, value:160
Item name: map, weight: 9, value:150
Item name: note-case, weight: 22, value:80
Item name: waterproof overclothes, weight: 43, value:75
Item name: suntan cream, weight: 11, value:70
Item name: waterproof trousers, weight: 42, value:70
Item name: glucose, weight: 15, value:60
Item name: banana, weight: 27, value:60
Item name: socks, weight: 4, value:50
Item name: compass, weight: 13, value:35
Item name: sunglasses, weight: 7, value:20
OK
Total mass: 396, Total Value: 1030
OK

```



## OCaml

A brute force solution:

```ocaml
let items = [
  "map",                     9,  150;
  "compass",                13,   35;
  "water",                 153,  200;
  "sandwich",               50,  160;
  "glucose",                15,   60;
  "tin",                    68,   45;
  "banana",                 27,   60;
  "apple",                  39,   40;
  "cheese",                 23,   30;
  "beer",                   52,   10;
  "suntan cream",           11,   70;
  "camera",                 32,   30;
  "t-shirt",                24,   15;
  "trousers",               48,   10;
  "umbrella",               73,   40;
  "waterproof trousers",    42,   70;
  "waterproof overclothes", 43,   75;
  "note-case",              22,   80;
  "sunglasses",              7,   20;
  "towel",                  18,   12;
  "socks",                   4,   50;
  "book",                   30,   10;
]

let comb =
  List.fold_left (fun acc x -> let acc2 = List.rev_map (fun li -> x::li) acc in
                                 List.rev_append acc acc2) [[]]

let score =
  List.fold_left (fun (w_tot,v_tot) (_,w,v) -> (w + w_tot, v + v_tot)) (0,0)

let () =
  let combs = comb items in
  let vals = List.rev_map (fun this -> (score this, this)) combs in
  let poss = List.filter (fun ((w,_), _) -> w <= 400) vals in
  let _, res = List.fold_left (fun (((_,s1),_) as v1) (((_,s2),_) as v2) ->
                 if s2 > s1 then v2 else v1)
                 (List.hd poss) (List.tl poss) in
  List.iter (fun (name,_,_) -> print_endline name) res;
;;
```



## Oz

Using constraint programming.

```oz
declare
  %% maps items to pairs of Weight(hectogram) and Value
  Problem = knapsack('map':9#150
                     'compass':13#35
                     'water':153#200
                     'sandwich':50#160
                     'glucose':15#60
                     'tin':68#45
                     'banana':27#60
                     'apple':39#40
                     'cheese':23#30
                     'beer':52#10
                     'suntan cream':11#70
                     'camera':32#30
                     't-shirt':24#15
                     'trousers':48#10
                     'umbrella':73#40
                     'waterproof trousers':42#70
                     'waterproof overclothes':43#75
                     'note-case':22#80
                     'sunglasses':7#20
                     'towel':18#12
                     'socks':4#50
                     'book':30#10
                    )

  %% item -> Weight
  Weights = {Record.map Problem fun {$ X} X.1 end}
  %% item -> Value
  Values =  {Record.map Problem fun {$ X} X.2 end}

  proc {Knapsack Solution}
     %% a solution maps items to finite domain variables
     %% with the domain {0,1}
     Solution = {Record.map Problem fun {$ _} {FD.int 0#1} end}
     %% no more than 400 hectograms
     {FD.sumC Weights Solution '=<:' 400}
     %% search through valid solutions
     {FD.distribute naive Solution}
  end

  proc {PropagateLargerValue Old New}
     %% propagate that new solutions must yield a higher value
     %% than previously found solutions (essential for performance)
     {FD.sumC Values New '>:' {Value Old}}
  end

  fun {Value Candidate}
     {Record.foldL {Record.zip Candidate Values Number.'*'} Number.'+' 0}
  end

  fun {Weight Candidate}
     {Record.foldL {Record.zip Candidate Weights Number.'*'} Number.'+' 0}
  end

  [Best] = {SearchBest Knapsack PropagateLargerValue}
in
  {System.showInfo "Items: "}
  {ForAll
     {Record.arity {Record.filter Best fun {$ T} T == 1 end}}
     System.showInfo}
  {System.printInfo "\n"}
  {System.showInfo "total value: "#{Value Best}}
  {System.showInfo "total weight: "#{Weight Best}}
```

{{out}}

```txt

Items:
banana
compass
glucose
map
note-case
sandwich
socks
sunglasses
suntan cream
water
waterproof overclothes
waterproof trousers

total value: 1030
total weight: 396

```

Typically runs in less than 150 milliseconds.




## Pascal

Uses a stringlist to store the items. I used the algorithm given on Wikipedia (Knapsack problem) to find the maximum value. It is written in pseudocode that translates very easily to Pascal.

```pascal

program project1;
uses
  sysutils, classes, math;

const
  MaxWeight = 400;
  N = 21;

type
  TMaxArray = array[0..N, 0..MaxWeight] of integer;

  TEquipment = record
    Description : string;
    Weight : integer;
    Value : integer;
  end;

  TEquipmentList = array[1..N] of TEquipment;

var
   M:TMaxArray;
   MaxValue, WeightLeft, i, j, Sum : integer;
   S,KnapSack:TStringList;
   L:string;
   List:TEquipmentList;

begin
   //Put all the items into an array called List
   L:='map ,9 ,150,compass ,13 ,35 ,water ,153 ,200 ,sandwich,50 ,160 ,glucose ,15 ,60 ,tin,68 ,45 ,banana,27,60 ,apple ,39 ,40 ,cheese ,23 ,30 ,beer ,52 ,10 ,suntancreme ,11 ,70 ,camera ,32 ,30 ,T-shirt ,24 ,15 ,trousers ,48 ,40 ,waterprooftrousers ,42 ,70 ,waterproofoverclothes ,43 ,75 ,notecase ,22 ,80 ,sunglasses ,7 ,20 ,towel ,18 ,12 ,socks ,4 ,50 ,book ,30 ,10';
   S:=TStringList.create;
   S.Commatext:=L;

   For i:= 1 to N do
   begin
      List[i].Description:=S[3*i - 3];
      List[i].Weight:=strtoint(S[3*i - 2]);
      List[i].Value:=strtoint(S[3*i - 1]);
   end;

   //create M, a table linking the possible items for each weight
   //and recording the value at that point
   for j := 0 to MaxWeight do
       M[0, j] := 0

   for i := 1 to N do
       for j := 0 to MaxWeight do
           if List[i].weight > j then
               M[i, j] := M[i-1, j]
           else
               M[i, j] := max(M[i-1, j], M[i-1, j-List[i].weight] + List[i].value);

   //get the highest total value by testing every value in table M
   for i:=1 to N do
       for j:= 0 to MaxWeight do
           If M[i,j] > MaxValue then
               MaxValue := m[i,j];

   writeln('Highest total value : ',MaxValue);

  //Work backwards through the items to find those items that go in the Knapsack (a stringlist)
   KnapSack := TStringList.create;
   WeightLeft := MaxWeight;
   For i:= N downto 1 do
       if M[i,WeightLeft] = MaxValue then
          if M[i-1, WeightLeft - List[i].Weight] = MaxValue - List[i].Value then
          begin
            Knapsack.add(List[i].Description + ' ' + IntToStr(List[i].Weight)+ ' ' + inttostr(List[i].Value));
            MaxValue := MaxValue - List[i].Value;
            WeightLeft := WeightLeft - List[i].Weight;
          end

   //Show the items in the knapsack
   writeln('Number of items     : ',KnapSack.count);
   writeln('-------------------------');
   For i:= KnapSack.count-1 downto 0 do
     writeln(KnapSack[i]);

   KnapSack.free;
   S.free;
   writeln('-------------------------');
   writeln('done');
   readln;
end.

```


Output

```txt

Highest total value : 1030
Number of items     : 12
-------------------------
map 9 150
compass 13 35
water 153 200
sandwich 50 160
glucose 15 60
banana 27 60
suntancreme 11 70
waterprooftrousers 42 70
waterproofoverclothes 43 75
notecase 22 80
sunglasses 7 20
socks 4 50
-------------------------
done

```



## Perl

The dynamic programming solution from Wikipedia.

```perl
my $raw = <<'TABLE';
map			9	150
compass			13	35
water			153	200
sandwich		50	160
glucose			15	60
tin			68	45
banana			27	60
apple			39	40
cheese			23	30
beer			52	10
suntancream		11	70
camera			32	30
T-shirt			24	15
trousers		48	10
umbrella		73	40
waterproof trousers	42	70
waterproof overclothes	43	75
note-case		22	80
sunglasses		7	20
towel			18	12
socks			4	50
book			30	10
TABLE

my (@name, @weight, @value);
for (split "\n", $raw) {
    for ([ split /\t+/ ]) {
        push @name,   $_->[0];
        push @weight, $_->[1];
        push @value,  $_->[2];
    }
}

my $max_weight = 400;
my @p = map [map undef, 0 .. 1+$max_weight], 0 .. $#name;

sub optimal {
    my ($i, $w) = @_;
    return [0, []] if $i < 0;
    return $p[$i][$w] if $p[$i][$w];

    if ($weight[$i] > $w) {
        $p[$i][$w] = optimal($i - 1, $w)
    } else {
        my $x = optimal($i - 1, $w);
        my $y = optimal($i - 1, $w - $weight[$i]);

        if ($x->[0] > $y->[0] + $value[$i]) {
            $p[$i][$w] = $x
        } else {
            $p[$i][$w] = [  $y->[0] + $value[$i], [ @{$y->[1]}, $name[$i] ]]
        }
    }
    return $p[$i][$w]
}

my $solution = optimal($#name, $max_weight);
print "$solution->[0]: @{$solution->[1]}\n";
```

{{out}}

```txt
1030: map compass water sandwich glucose banana suntancream waterproof trousers waterproof overclothes note-case sunglasses socks
```



## Perl 6


### = Brute force =

{{works with|Rakudo|2017.01}}

```perl6
my class KnapsackItem { has $.name; has $.weight; has $.unit; }

multi sub pokem ([],           $,  $v = 0) { $v }
multi sub pokem ([$,  *@],     0,  $v = 0) { $v }
multi sub pokem ([$i, *@rest], $w, $v = 0) {
  my $key = "{+@rest} $w $v";
  (state %cache){$key} or do {
    my @skip = pokem @rest, $w, $v;
    if $w >= $i.weight { # next one fits
      my @put = pokem @rest, $w - $i.weight, $v + $i.unit;
      return (%cache{$key} = |@put, $i.name).list if @put[0] > @skip[0];
    }
    return (%cache{$key} = |@skip).list;
  }
}

my $MAX_WEIGHT = 400;
my @table = flat map -> $name,  $weight,  $unit {
     KnapsackItem.new: :$name, :$weight, :$unit;
},
    'map',                      9, 150,
    'compass',                 13,  35,
    'water',                  153, 200,
    'sandwich',                50, 160,
    'glucose',                 15,  60,
    'tin',                     68,  45,
    'banana',                  27,  60,
    'apple',                   39,  40,
    'cheese',                  23,  30,
    'beer',                    52,  10,
    'suntan cream',            11,  70,
    'camera',                  32,  30,
    'T-shirt',                 24,  15,
    'trousers',                48,  10,
    'umbrella',                73,  40,
    'waterproof trousers',     42,  70,
    'waterproof overclothes',  43,  75,
    'note-case',               22,  80,
    'sunglasses',               7,  20,
    'towel',                   18,  12,
    'socks',                    4,  50,
    'book',                    30,  10;

my ($value, @result) = pokem @table, $MAX_WEIGHT;
say "Value = $value\nTourist put in the bag:\n  " ~ @result;
```

{{out}}

```txt
Value = 1030
Tourist put in the bag:
  socks sunglasses note-case waterproof overclothes waterproof trousers suntan cream banana glucose sandwich water compass map
```



### = Dynamic programming =

Not as idiomatic as the previous example, but much faster.
{{trans|Perl}}

```perl6
my $raw = qq:to/TABLE/;
map			9	150
compass			13	35
water			153	200
sandwich		50	160
glucose			15	60
tin			68	45
banana			27	60
apple			39	40
cheese			23	30
beer			52	10
suntancream		11	70
camera			32	30
T-shirt			24	15
trousers		48	10
umbrella		73	40
waterproof trousers	42	70
waterproof overclothes	43	75
note-case		22	80
sunglasses		7	20
towel			18	12
socks			4	50
book			30	10
TABLE

my (@name, @weight, @value);

for split(["\n", /\t+/], $raw, :skip-empty) -> $n,$w,$v {
    @name.push:   $n;
    @weight.push: $w;
    @value.push:  $v;
}

my $max_weight = 400;
my @p = [Nil xx $max_weight+2 ] xx @name;

sub optimal ($i, $w) {
    return [0, []] if $i < 0;
    return |@p[$i][$w] if @p[$i][$w];

    if @weight[$i] > $w {
        @p[$i][$w] = optimal($i-1, $w);
    } else {
        my @x = optimal($i-1, $w);
        my @y = optimal($i-1, $w - @weight[$i]);

        if (@x[0] > @y[0] + @value[$i] ) {
            @p[$i][$w] = @x;
        } else {
            @p[$i][$w] = @y[0] + @value[$i], [|@y[1], @name[$i]];
        }
    }
    return |@p[$i][$w]
}

my @solution = optimal(-1+@name, $max_weight);
say "@solution[0]: " ~ join ' ', @solution[1];
```

{{out}}

```txt
1030: map compass water sandwich glucose banana suntancream waterproof trousers waterproof overclothes note-case sunglasses socks
```



## Phix

Trivial simplification of the [[Knapsack_problem/Bounded#Phix|Knapsack/Bounded]] solution. See that page for discussion of the terminate flag.
In this case I have switched the optimisation on.

```Phix
integer terminate=0

integer attempts = 0
function knapsack(sequence res, goodies, atom points, weight, at=1, sequence chosen={})
    atom {witem,pitem} = goodies[at][2]
    integer n = (witem<=weight)
    chosen &= n
    points += n*pitem   -- increase value
    weight -= n*witem   -- decrease weight left
    if at=length(goodies) then
        attempts += 1
        if length(res)=0
        or res<{points,weight} then
            res = {points,weight,chosen}
        end if
        terminate = (n=1)
    else
        while n>=0 and not terminate do
            res = knapsack(res,goodies,points,weight,at+1,chosen)
            n -= 1
            chosen[$] = n
            points -= pitem
            weight += witem
        end while
    end if
    return res
end function

function byweightedvalue(object a, b)
    -- sort by weight/value
    return compare(a[2][1]/a[2][2],b[2][1]/b[2][2])
    -- nb other sort orders break the optimisation
end function

constant goodies = custom_sort(routine_id("byweightedvalue"),{
-- item                     weight value
{"map",                     {9,     150}},
{"compass",                 {13,    35 }},
{"water",                   {153,   200}},
{"sandwich",                {50,    160}},
{"glucose",                 {15,    60 }},
{"tin",                     {68,    45 }},
{"banana",                  {27,    60 }},
{"apple",                   {39,    40 }},
{"cheese",                  {23,    30 }},
{"beer",                    {52,    10 }},
{"suntan cream",            {11,    70 }},
{"camera",                  {32,    30 }},
{"T-shirt",                 {24,    15 }},
{"trousers",                {48,    10 }},
{"umbrella",                {73,    40 }},
{"waterproof trousers",     {42,    70 }},
{"waterproof overclothes",  {43,    75 }},
{"note-case",               {22,    80 }},
{"sunglasses",              {7,     20 }},
{"towel",                   {18,    12 }},
{"socks",                   {4,     50 }},
{"book",                    {30,    10 }}})

atom t0 = time()
object {points,weight,counts} = knapsack({},goodies,0,400)
printf(1,"Value %d, weight %g [%d attempts, %3.2fs]:\n",{points,400-weight,attempts,time()-t0})
for i=1 to length(counts) do
    integer c = counts[i]
    if c then
        printf(1,"%s\n",{goodies[i][1]})
    end if
end for
```

{{out}}

```txt

Value 1030, weight 396 [9 attempts, 0.00s]:
map
socks
suntan cream
glucose
note-case
sandwich
sunglasses
compass
banana
waterproof overclothes
waterproof trousers
water

```

without the optimisation:

```txt

Value 1030, weight 396 [1216430 attempts, 0.84s]:

```



## PHP


```php
#########################################################
# 0-1 Knapsack Problem Solve with memoization optimize and index returns
# $w = weight of item
# $v = value of item
# $i = index
# $aW = Available Weight
# $m = Memo items array
# PHP Translation from Python, Memoization,
# and index return functionality added by Brian Berneker
#
#########################################################

function knapSolveFast2($w, $v, $i, $aW, &$m) {

	global $numcalls;
	$numcalls ++;
	// echo "Called with i=$i, aW=$aW
";

	// Return memo if we have one
	if (isset($m[$i][$aW])) {
		return array( $m[$i][$aW], $m['picked'][$i][$aW] );
	} else {

		// At end of decision branch
		if ($i == 0) {
			if ($w[$i] <= $aW) { // Will this item fit?
				$m[$i][$aW] = $v[$i]; // Memo this item
				$m['picked'][$i][$aW] = array($i); // and the picked item
				return array($v[$i],array($i)); // Return the value of this item and add it to the picked list

			} else {
				// Won't fit
				$m[$i][$aW] = 0; // Memo zero
				$m['picked'][$i][$aW] = array(); // and a blank array entry...
				return array(0,array()); // Return nothing
			}
		}

		// Not at end of decision branch..
		// Get the result of the next branch (without this one)
		list ($without_i, $without_PI) = knapSolveFast2($w, $v, $i-1, $aW, $m);

		if ($w[$i] > $aW) { // Does it return too many?

			$m[$i][$aW] = $without_i; // Memo without including this one
			$m['picked'][$i][$aW] = $without_PI; // and a blank array entry...
			return array($without_i, $without_PI); // and return it

		} else {

			// Get the result of the next branch (WITH this one picked, so available weight is reduced)
			list ($with_i,$with_PI) = knapSolveFast2($w, $v, ($i-1), ($aW - $w[$i]), $m);
			$with_i += $v[$i];  // ..and add the value of this one..

			// Get the greater of WITH or WITHOUT
			if ($with_i > $without_i) {
				$res = $with_i;
				$picked = $with_PI;
				array_push($picked,$i);
			} else {
				$res = $without_i;
				$picked = $without_PI;
			}

			$m[$i][$aW] = $res; // Store it in the memo
			$m['picked'][$i][$aW] = $picked; // and store the picked item
			return array ($res,$picked); // and then return it
		}
	}
}



$items4 = array("map","compass","water","sandwich","glucose","tin","banana","apple","cheese","beer","suntan cream","camera","t-shirt","trousers","umbrella","waterproof trousers","waterproof overclothes","note-case","sunglasses","towel","socks","book");
$w4 = array(9,13,153,50,15,68,27,39,23,52,11,32,24,48,73,42,43,22,7,18,4,30);
$v4 = array(150,35,200,160,60,45,60,40,30,10,70,30,15,10,40,70,75,80,20,12,50,10);

## Initialize
$numcalls = 0; $m = array(); $pickedItems = array();

## Solve
list ($m4,$pickedItems) = knapSolveFast2($w4, $v4, sizeof($v4) -1, 400, $m);

# Display Result
echo "<b>Items:</b>
".join(", ",$items4)."
";
echo "<b>Max Value Found:</b>
$m4 (in $numcalls calls)
";
echo "<b>Array Indices:</b>
".join(",",$pickedItems)."
";


echo "<b>Chosen Items:</b>
";
echo "<table border cellspacing=0>";
echo "<tr><td>Item</td><td>Value</td><td>Weight</td></tr>";
$totalVal = $totalWt = 0;
foreach($pickedItems as $key) {
	$totalVal += $v4[$key];
	$totalWt += $w4[$key];
	echo "<tr><td>".$items4[$key]."</td><td>".$v4[$key]."</td><td>".$w4[$key]."</td></tr>";
}
echo "<tr><td align=right><b>Totals</b></td><td>$totalVal</td><td>$totalWt</td></tr>";
echo "</table><hr>";
```

{{out}}
<div class="solutionoutput">
<b>Items:</b>
map, compass, water, sandwich, glucose, tin, banana, apple, cheese, beer, suntan cream, camera, t-shirt, trousers, umbrella, waterproof trousers, waterproof overclothes, note-case, sunglasses, towel, socks, book
<b>Max Value Found:</b>
1030 (in 8725 calls)
<b>Array Indices:</b>
0,1,2,3,4,6,10,15,16,17,18,20
<b>Chosen Items:</b>
<table border cellspacing=0><tr><td>Item</td><td>Value</td><td>Weight</td></tr><tr><td>map</td><td>150</td><td>9</td></tr><tr><td>compass</td><td>35</td><td>13</td></tr><tr><td>water</td><td>200</td><td>153</td></tr><tr><td>sandwich</td><td>160</td><td>50</td></tr><tr><td>glucose</td><td>60</td><td>15</td></tr><tr><td>banana</td><td>60</td><td>27</td></tr><tr><td>suntan cream</td><td>70</td><td>11</td></tr><tr><td>waterproof trousers</td><td>70</td><td>42</td></tr><tr><td>waterproof overclothes</td><td>75</td><td>43</td></tr><tr><td>note-case</td><td>80</td><td>22</td></tr><tr><td>sunglasses</td><td>20</td><td>7</td></tr><tr><td>socks</td><td>50</td><td>4</td></tr><tr><td align=right><b>Totals</b></td><td>1030</td><td>396</td></tr></table><hr>
</div>
Minimal PHP Algorithm for totals only translated from Python version as discussed in the YouTube posted video at: http://www.youtube.com/watch?v=ZKBUu_ahSR4

```php
#########################################################
# 0-1 Knapsack Problem Solve
# $w = weight of item
# $v = value of item
# $i = index
# $aW = Available Weight
# PHP Translation by Brian Berneker
#########################################################

function knapSolve($w,$v,$i,$aW) {

	global $numcalls;
	$numcalls ++;
	// echo "Called with i=$i, aW=$aW
";

	if ($i == 0) {
		if ($w[$i] <= $aW) {
			return $v[$i];
		} else {
			return 0;
		}
	}

	$without_i = knapSolve($w, $v, $i-1, $aW);
	if ($w[$i] > $aW) {
		return $without_i;
	} else {
		$with_i = $v[$i] + knapSolve($w, $v, ($i-1), ($aW - $w[$i]));
		return max($with_i, $without_i);
	}

}


#########################################################
# 0-1 Knapsack Problem Solve (with "memo"-ization optimization)
# $w = weight of item
# $v = value of item
# $i = index
# $aW = Available Weight
# $m = 'memo' array
# PHP Translation by Brian Berneker
#########################################################

function knapSolveFast($w,$v,$i,$aW,&$m) { // Note: We use &$m because the function writes to the $m array

	global $numcalls;
	$numcalls ++;
	// echo "Called with i=$i, aW=$aW
";

	// Return memo if we have one
	if (isset($m[$i][$aW])) {
		return $m[$i][$aW];
	} else {

		if ($i == 0) {
			if ($w[$i] <= $aW) {
				$m[$i][$aW] = $v[$i]; // save memo
				return $v[$i];
			} else {
				$m[$i][$aW] = 0; // save memo
				return 0;
			}
		}

		$without_i = knapSolveFast($w, $v, $i-1, $aW,$m);
		if ($w[$i] > $aW) {
			$m[$i][$aW] = $without_i; // save memo
			return $without_i;
		} else {
			$with_i = $v[$i] + knapSolveFast($w, $v, ($i-1), ($aW - $w[$i]),$m);
			$res = max($with_i, $without_i);
			$m[$i][$aW] = $res; // save memo
			return $res;
		}
	}
}


$w3 = array(1, 1, 1, 2, 2, 2, 4, 4, 4, 44, 96, 96, 96);
$v3 = array(1, 1, 1, 2, 2, 2, 4, 4, 4, 44, 96, 96, 96);

$numcalls = 0;
$m = array();
$m3 = knapSolveFast($w3, $v3, sizeof($v3) -1, 54,$m);
print_r($w3); echo "
FAST: ";
echo "<b>Max: $m3</b> ($numcalls calls)

";


$numcalls = 0;
$m = array();
$m3 = knapSolve($w3, $v3, sizeof($v3) -1, 54 );
print_r($w3); echo "
";
echo "<b>Max: $m3</b> ($numcalls calls)

";
```

{{out}}

```txt

Array ( [0] => 1 [1] => 1 [2] => 1 [3] => 2 [4] => 2 [5] => 2 [6] => 4 [7] => 4 [8] => 4 [9] => 44 [10] => 96 [11] => 96 [12] => 96 )
FAST: Max: 54 (191 calls)

Array ( [0] => 1 [1] => 1 [2] => 1 [3] => 2 [4] => 2 [5] => 2 [6] => 4 [7] => 4 [8] => 4 [9] => 44 [10] => 96 [11] => 96 [12] => 96 )
Max: 54 (828 calls)

```



## PicoLisp


```PicoLisp
(de *Items
   ("map" 9 150)                    ("compass" 13 35)
   ("water" 153 200)                ("sandwich" 50 160)
   ("glucose" 15 60)                ("tin" 68 45)
   ("banana" 27 60)                 ("apple" 39 40)
   ("cheese" 23 30)                 ("beer" 52 10)
   ("suntan cream" 11 70)           ("camera" 32 30)
   ("t-shirt" 24 15)                ("trousers" 48 10)
   ("umbrella" 73 40)               ("waterproof trousers" 42 70)
   ("waterproof overclothes" 43 75) ("note-case" 22 80)
   ("sunglasses" 7 20)              ("towel" 18 12)
   ("socks" 4 50)                   ("book" 30 10) )

# Dynamic programming solution
(de knapsack (Lst W)
   (when Lst
      (cache '*KnapCache (cons W Lst)
         (let X (knapsack (cdr Lst) W)
            (if (ge0 (- W (cadar Lst)))
               (let Y (cons (car Lst) (knapsack (cdr Lst) @))
                  (if (> (sum caddr X) (sum caddr Y)) X Y) )
               X ) ) ) ) )

(let K (knapsack *Items 400)
   (for I K
      (apply tab I (3 -24 6 6) NIL) )
   (tab (27 6 6) NIL (sum cadr K) (sum caddr K)) )
```

{{out}}

```txt

   map                          9   150
   compass                     13    35
   water                      153   200
   sandwich                    50   160
   glucose                     15    60
   banana                      27    60
   suntan cream                11    70
   waterproof trousers         42    70
   waterproof overclothes      43    75
   note-case                   22    80
   sunglasses                   7    20
   socks                        4    50
                              396  1030

```



## Prolog

{{Works with|SWI-Prolog}}

### Using the clpfd library

{{libheader|clpfd}}

```Prolog
:- use_module(library(clpfd)).

knapsack :-
        L = [
             item(map,  9,      150),
             item(compass,      13,     35),
             item(water,        153,    200),
             item(sandwich, 50,         160),
             item(glucose,      15,     60),
             item(tin,  68,     45),
             item(banana,       27,     60),
             item(apple,        39,     40),
             item(cheese,       23,     30),
             item(beer,         52,     10),
             item('suntan cream',       11,     70),
             item(camera,       32,     30),
             item('t-shirt',    24,     15),
             item(trousers, 48,         10),
             item(umbrella, 73,         40),
             item('waterproof trousers',        42,     70),
             item('waterproof overclothes',     43,     75),
             item('note-case',22,       80),
             item(sunglasses,   7,      20),
             item(towel,        18,     12),
             item(socks,        4,      50),
             item(book,         30,     10 )],
        length(L, N),
        length(R, N),
        R ins 0..1,
        maplist(arg(2), L, LW),
        maplist(arg(3), L, LV),
        scalar_product(LW, R, #=<, 400),
        scalar_product(LV, R, #=, VM),
        labeling([max(VM)], R),
        scalar_product(LW, R, #=, WM),
        %% affichage des résultats
        compute_lenword(L, 0, Len),
        sformat(A1, '~~w~~t~~~w|', [Len]),
        sformat(A2, '~~t~~w~~~w|', [4]),
        sformat(A3, '~~t~~w~~~w|', [5]),
        print_results(A1,A2,A3, L, R, WM, VM).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% to show the results in a good way
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
compute_lenword([], N, N).
compute_lenword([item(Name, _, _)|T], N, NF):-
        atom_length(Name, L),
        (   L > N -> N1 = L; N1 = N),
        compute_lenword(T, N1, NF).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
print_results(A1,A2,A3, [], [], WM, WR) :-
        sformat(W1, A1, [' ']),
        sformat(W2, A2, [WM]),
        sformat(W3, A3, [WR]),
        format('~w~w~w~n', [W1,W2,W3]).


print_results(A1,A2,A3, [_H|T], [0|TR], WM, VM) :-
        print_results(A1,A2,A3, T, TR, WM, VM).

print_results(A1, A2, A3, [item(Name, W, V)|T], [1|TR], WM, VM) :-
        sformat(W1, A1, [Name]),
        sformat(W2, A2, [W]),
        sformat(W3, A3, [V]),
        format('~w~w~w~n', [W1,W2,W3]),
        print_results(A1, A2, A3, T, TR, WM, VM).
```

{{out}}

```txt

?- knapsack
map                      9  150
compass                 13   35
water                  153  200
sandwich                50  160
glucose                 15   60
banana                  27   60
suntan cream            11   70
waterproof trousers     42   70
waterproof overclothes  43   75
note-case               22   80
sunglasses               7   20
socks                    4   50
                       396 1030

```


### Using the simplex library

{{libheader|simplex}}
Library written by <b>Markus Triska</b>. The problem is solved in about 3 seconds.

```Prolog
:- use_module(library(simplex)).

knapsack  :-
	L = [
	     (map, 	9, 	150),
	     (compass, 	13, 	35),
	     (water, 	153, 	200),
	     (sandwich, 50, 	160),
	     (glucose, 	15, 	60),
	     (tin, 	68, 	45),
	     (banana, 	27, 	60),
	     (apple, 	39, 	40),
	     (cheese, 	23, 	30),
	     (beer, 	52, 	10),
	     ('suntan cream', 	11, 	70),
	     (camera, 	32, 	30),
	     ('t-shirt', 	24, 	15),
	     (trousers, 48, 	10),
	     (umbrella, 73, 	40),
	     ('waterproof trousers', 	42, 	70),
	     ('waterproof overclothes', 	43, 	75),
	     ('note-case',22, 	80),
	     (sunglasses, 	7, 	20),
	     (towel, 	18, 	12),
	     (socks, 	4, 	50),
	     (book, 	30, 	10 )],
	 gen_state(S0),
	 length(L, N),
	 numlist(1, N, LN),
	 time(( create_constraint_N(LN, S0, S1),
		maplist(create_constraint_WV, LN, L, LW, LV),
		constraint(LW =< 400, S1, S2),
		maximize(LV, S2, S3)
	      )),
	compute_lenword(L, 0, Len),
	sformat(A1, '~~w~~t~~~w|', [Len]),
	sformat(A2, '~~t~~w~~~w|', [4]),
	sformat(A3, '~~t~~w~~~w|', [5]),
	print_results(S3, A1,A2,A3, L, LN, 0, 0).


create_constraint_N([], S, S).

create_constraint_N([HN|TN], S1, SF) :-
	constraint(integral(x(HN)), S1, S2),
	constraint([x(HN)] =< 1, S2, S3),
	constraint([x(HN)] >= 0, S3, S4),
	create_constraint_N(TN, S4, SF).

create_constraint_WV(N, (_, W, V), W * x(N), V * x(N)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
compute_lenword([], N, N).
compute_lenword([(Name, _, _)|T], N, NF):-
	atom_length(Name, L),
	(   L > N -> N1 = L; N1 = N),
	compute_lenword(T, N1, NF).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
print_results(_S, A1, A2, A3, [], [], WM, VM) :-
	sformat(W1, A1, [' ']),
	sformat(W2, A2, [WM]),
	sformat(W3, A3, [VM]),
	format('~w~w~w~n', [W1,W2,W3]).


print_results(S, A1, A2, A3, [(Name, W, V)|T], [N|TN], W1, V1) :-
	variable_value(S, x(N), X),
	(   X = 0 -> W1 = W2, V1 = V2
	;   sformat(S1, A1, [Name]),
	    sformat(S2, A2, [W]),
	    sformat(S3, A3, [V]),
	    format('~w~w~w~n', [S1,S2,S3]),
	    W2 is W1 + W,
	    V2 is V1 + V),
	print_results(S, A1, A2, A3, T, TN, W2, V2).
```



## PureBasic

Solution uses memoization.

```PureBasic
Structure item
  name.s
  weight.i ;units are dekagrams (dag)
  Value.i
EndStructure

Structure memo
  Value.i
  List picked.i()
EndStructure

Global itemCount = 0 ;this will be increased as needed to match count
Global Dim items.item(itemCount)

Procedure addItem(name.s, weight, Value)
  If itemCount >= ArraySize(items())
    Redim items.item(itemCount + 10)
  EndIf
  With items(itemCount)
    \name = name
    \weight = weight
    \Value = Value
  EndWith
  itemCount + 1
EndProcedure

;build item list
addItem("map", 9, 150)
addItem("compass", 13, 35)
addItem("water", 153, 200)
addItem("sandwich", 50, 160)
addItem("glucose", 15, 60)
addItem("tin", 68, 45)
addItem("banana", 27, 60)
addItem("apple", 39, 40)
addItem("cheese", 23, 30)
addItem("beer", 52, 10)
addItem("suntan cream", 11, 70)
addItem("camera", 32, 30)
addItem("t-shirt", 24, 15)
addItem("trousers", 48, 10)
addItem("umbrella", 73, 40)
addItem("waterproof trousers", 42, 70)
addItem("waterproof overclothes", 43, 75)
addItem("note-case", 22, 80)
addItem("sunglasses", 7, 20)
addItem("towel", 18, 12)
addItem("socks", 4, 50)
addItem("book", 30, 10)

Procedure knapsackSolveFast(Array item.item(1), i, aw, Map m.memo())
  Protected.memo without_i, with_i, result, *tmp, memoIndex.s = Hex((i << 16) + aw, #PB_Long)
  If FindMapElement(m(), memoIndex)
    ProcedureReturn @m()
  Else
    If i = 0
      If item(0)\weight <= aw
        ;item fits
        m(memoIndex)\Value = item(0)\Value ;memo this item's value
        AddElement(m()\picked())
        m()\picked() = 0 ;memo item's index also
      Else
        ;item doesn't fit, memo a zero Value
        m(memoIndex)\Value = 0
      EndIf
      ProcedureReturn @m()
    EndIf

    ;test if a greater value results with or without item included
    *tmp = knapsackSolveFast(item(), i - 1, aw, m()) ;find value without this item
    CopyStructure(*tmp, @without_i, memo)
    If item(i)\weight > aw
      ;item weighs too much, memo without including this item
      m(memoIndex) = without_i
      ProcedureReturn @m()
    Else
      *tmp = knapsackSolveFast(item(), i - 1, aw - item(i)\weight, m()) ;find value when item is included
      CopyStructure(*tmp, @with_i, memo)
      with_i\Value + item(i)\Value
      AddElement(with_i\picked())
      with_i\picked() = i ;add item to with's picked list
    EndIf

    ;set the result to the larger value
    If with_i\Value > without_i\Value
      result = with_i
    Else
      result = without_i
    EndIf

    m(memoIndex) = result ;memo the result
    ProcedureReturn @m()
  EndIf
EndProcedure

Procedure.s knapsackSolve(Array item.item(1), i, aw)
  Protected *result.memo, output.s, totalWeight
  NewMap m.memo()
  *result = knapsackSolveFast(item(), i, aw, m())
  output = "Knapsack:" + #CRLF$
  ForEach *result\picked()
    output + LSet(item(*result\picked())\name, 24) + RSet(Str(item(*result\picked())\weight), 5) + RSet(Str(item(*result\picked())\Value), 5) + #CRLF$
    totalWeight + item(*result\picked())\weight
  Next
  output + LSet("TOTALS:", 24) + RSet(Str(totalWeight), 5) + RSet(Str(*result\Value), 5)
  ProcedureReturn output
EndProcedure

If OpenConsole()
  #maxWeight = 400
  Define *result.memo
  PrintN(knapsackSolve(items(), itemCount - 1, #maxWeight))

  Print(#CRLF$ + #CRLF$ + "Press ENTER to exit"): Input()
  CloseConsole()
EndIf
```

{{out}}

```txt

Knapsack:
map                         9  150
compass                    13   35
water                     153  200
sandwich                   50  160
glucose                    15   60
banana                     27   60
suntan cream               11   70
waterproof trousers        42   70
waterproof overclothes     43   75
note-case                  22   80
sunglasses                  7   20
socks                       4   50
TOTALS:                   396 1030

```



## Python


### Brute force algorithm


```python
from itertools import combinations

def anycomb(items):
    ' return combinations of any length from the items '
    return ( comb
             for r in range(1, len(items)+1)
             for comb in combinations(items, r)
             )

def totalvalue(comb):
    ' Totalise a particular combination of items'
    totwt = totval = 0
    for item, wt, val in comb:
        totwt  += wt
        totval += val
    return (totval, -totwt) if totwt <= 400 else (0, 0)

items = (
    ("map", 9, 150), ("compass", 13, 35), ("water", 153, 200), ("sandwich", 50, 160),
    ("glucose", 15, 60), ("tin", 68, 45), ("banana", 27, 60), ("apple", 39, 40),
    ("cheese", 23, 30), ("beer", 52, 10), ("suntan cream", 11, 70), ("camera", 32, 30),
    ("t-shirt", 24, 15), ("trousers", 48, 10), ("umbrella", 73, 40),
    ("waterproof trousers", 42, 70), ("waterproof overclothes", 43, 75),
    ("note-case", 22, 80), ("sunglasses", 7, 20), ("towel", 18, 12),
    ("socks", 4, 50), ("book", 30, 10),
    )
bagged = max( anycomb(items), key=totalvalue) # max val or min wt if values equal
print("Bagged the following items\n  " +
      '\n  '.join(sorted(item for item,_,_ in bagged)))
val, wt = totalvalue(bagged)
print("for a total value of %i and a total weight of %i" % (val, -wt))
```

{{out}}

```txt

Bagged the following items
  banana
  compass
  glucose
  map
  note-case
  sandwich
  socks
  sunglasses
  suntan cream
  water
  waterproof overclothes
  waterproof trousers
for a total value of 1030 and a total weight of 396

```


###  Dynamic programming solution


```python
try:
    xrange
except:
    xrange = range

def totalvalue(comb):
    ' Totalise a particular combination of items'
    totwt = totval = 0
    for item, wt, val in comb:
        totwt  += wt
        totval += val
    return (totval, -totwt) if totwt <= 400 else (0, 0)

items = (
    ("map", 9, 150), ("compass", 13, 35), ("water", 153, 200), ("sandwich", 50, 160),
    ("glucose", 15, 60), ("tin", 68, 45), ("banana", 27, 60), ("apple", 39, 40),
    ("cheese", 23, 30), ("beer", 52, 10), ("suntan cream", 11, 70), ("camera", 32, 30),
    ("t-shirt", 24, 15), ("trousers", 48, 10), ("umbrella", 73, 40),
    ("waterproof trousers", 42, 70), ("waterproof overclothes", 43, 75),
    ("note-case", 22, 80), ("sunglasses", 7, 20), ("towel", 18, 12),
    ("socks", 4, 50), ("book", 30, 10),
    )

def knapsack01_dp(items, limit):
    table = [[0 for w in range(limit + 1)] for j in xrange(len(items) + 1)]

    for j in xrange(1, len(items) + 1):
        item, wt, val = items[j-1]
        for w in xrange(1, limit + 1):
            if wt > w:
                table[j][w] = table[j-1][w]
            else:
                table[j][w] = max(table[j-1][w],
                                  table[j-1][w-wt] + val)

    result = []
    w = limit
    for j in range(len(items), 0, -1):
        was_added = table[j][w] != table[j-1][w]

        if was_added:
            item, wt, val = items[j-1]
            result.append(items[j-1])
            w -= wt

    return result


bagged = knapsack01_dp(items, 400)
print("Bagged the following items\n  " +
      '\n  '.join(sorted(item for item,_,_ in bagged)))
val, wt = totalvalue(bagged)
print("for a total value of %i and a total weight of %i" % (val, -wt))
```


### Recursive dynamic programming algorithm


```python
def total_value(items, max_weight):
    return  sum([x[2] for x in items]) if sum([x[1] for x in items]) < max_weight else 0

cache = {}
def solve(items, max_weight):
    if not items:
        return ()
    if (items,max_weight) not in cache:
        head = items[0]
        tail = items[1:]
        include = (head,) + solve(tail, max_weight - head[1])
        dont_include = solve(tail, max_weight)
        if total_value(include, max_weight) > total_value(dont_include, max_weight):
            answer = include
        else:
            answer = dont_include
        cache[(items,max_weight)] = answer
    return cache[(items,max_weight)]

items = (
    ("map", 9, 150), ("compass", 13, 35), ("water", 153, 200), ("sandwich", 50, 160),
    ("glucose", 15, 60), ("tin", 68, 45), ("banana", 27, 60), ("apple", 39, 40),
    ("cheese", 23, 30), ("beer", 52, 10), ("suntan cream", 11, 70), ("camera", 32, 30),
    ("t-shirt", 24, 15), ("trousers", 48, 10), ("umbrella", 73, 40),
    ("waterproof trousers", 42, 70), ("waterproof overclothes", 43, 75),
    ("note-case", 22, 80), ("sunglasses", 7, 20), ("towel", 18, 12),
    ("socks", 4, 50), ("book", 30, 10),
    )
max_weight = 400

solution = solve(items, max_weight)
print "items:"
for x in solution:
    print x[0]
print "value:", total_value(solution, max_weight)
print "weight:", sum([x[1] for x in solution])
```



## Racket


```racket

#lang racket
; An ITEM a list of three elements:
;   a name, a weight, and, a value

; A SOLUTION to a knapsack01 problems is a list of three elements:
;   the total value, the total weight, and, names of the items to bag

(define (add i s) ; add an item i to the solution s
  (match-define (list in iw iv) i)
  (match-define (list v w is) s)
  (list (+ v iv) (+ w iw) (cons in is)))

(define (knapsack max-weight items)
  ; return a solution to the knapsack01 problem
  (define ht (make-hash)) ; (weight number-of-items) -> items
  (define (get w no-items) (hash-ref ht (list w no-items) #f))
  (define (update w is x)  (hash-set! ht (list w (length is)) is) x)
  (define (knapsack1 left items)
    ; return a solution to the (left, items) problem
    (cond
      ; if there are no items, then bag no items:
      [(empty? items) (list 0 0 '())]
      ; look up the best solution:
      [(or (get left (length items))
           ; the solution haven't been cached, so we
           ; must compute it and update the cache:
           (update
            left items
            (match items
              ; let us name the first item
              [(cons (and (list i w v) x) more)
               ; if the first item weighs more than the space left,
               ; we simply find a solution, where it is omitted:
               (cond [(> w left) (knapsack left more)]
                     ; there is room for the first item, but
                     ; we need to choose the best solutions
                     ; between those with it and that without:
                     [else
                      (define without (knapsack left more))
                      (define value-without (first without))
                      (define with (knapsack (- left w) more))
                      (define value-with (+ (first with) v))
                      ; choose the solutions with greatest value
                      (if (> value-without value-with)
                          without
                          (add x with))])])))]))
  (knapsack1 max-weight items))

(knapsack 400
          '((map 9 150) ; 9 is weight, 150 is value
            (compass 13 35) (water 153 200) (sandwich 50 160)
            (glucose 15 60) (tin 68 45)(banana 27 60) (apple 39 40)
            (cheese 23 30) (beer 52 10) (cream 11 70) (camera 32 30)
            (T-shirt 24 15) (trousers 48 10) (umbrella 73 40)
            (trousers 42 70) (overclothes 43 75) (notecase 22 80)
            (glasses 7 20) (towel 18 12) (socks 4 50) (book 30 10)))

```

{{out}}

```racket

'(1030 396 (map compass water sandwich glucose banana cream trousers overclothes notecase glasses socks))

```

Brute Force and Memoized Recursive Alternate

```racket

#lang racket

(define items '((map 9 150) (compass 13 35) (water 153 200) (sandwich 50 160)
      (glucose 15 60) (tin 68 45)(banana 27 60) (apple 39 40)
      (cheese 23 30) (beer 52 10) (cream 11 70) (camera 32 30)
      (T-shirt 24 15) (trousers 48 10) (umbrella 73 40)
      (trousers 42 70) (overclothes 43 75) (notecase 22 80)
      (glasses 7 20) (towel 18 12) (socks 4 50) (book 30 10)))

(define max-weight 400)

(define (item-value item)
  (caddr item))

(define (item-weight item)
  (cadr item))

(define (pack-weight pack)
  (apply + (map item-weight pack)))

(define (pack-value pack)
  (apply + (map item-value pack)))

(define (max-pack-value pack-with pack-without max-weight)
  (if (and
       (not (> (pack-weight pack-with) max-weight))
       (> (pack-value pack-with) (pack-value pack-without)))
      pack-with pack-without))

(define (display-solution pack)
    (displayln (list 'weight: (pack-weight pack)
                     'value:  (pack-value pack)
                     'items:  (map car pack))))

```

Brute Force

```racket

(define (show-brute)

  (define empty-accumulator '())

  (define (knapsack-brute included items)
    (cond
      ((null? items) included)
      (else
       (max-pack-value
        (knapsack-brute (cons (car items) included) (cdr items))
        (knapsack-brute included (cdr items))
        max-weight
        ))))

  (display-solution (reverse (knapsack-brute empty-accumulator items))))

(show-brute); takes around five seconds on my machine

```

Recursive Alternate

```racket

(define (show-memoized)

  (define (memoize func)
    (let ([result-ht (make-hash)])
      (lambda args ; this is the rest-id pattern
        (when (not (hash-has-key? result-ht args))
          (hash-set! result-ht args (apply func args)))
        (hash-ref result-ht args))))

  (define knapsack
    (memoize
     (lambda (max-weight items)
       (cond
         ((null? items) '())
         (else
          (let ([item (car items)] [items (cdr items)])
            (max-pack-value
             (cons item (knapsack (- max-weight (item-weight item)) items))
             (knapsack max-weight items)
             max-weight)))))))

  (display-solution (knapsack max-weight items)))

(show-memoized)

```


{{out}}

```racket

(weight: 396 value: 1030 items: (map compass water sandwich glucose banana cream trousers overclothes notecase glasses socks))

```



## R


```r

Full_Data<-structure(list(item = c("map", "compass", "water", "sandwich",
"glucose", "tin", "banana", "apple", "cheese", "beer", "suntan_cream",
"camera", "T-shirt", "trousers", "umbrella", "waterproof_trousers",
"waterproof_overclothes", "note-case", "sunglasses", "towel",
"socks", "book"), weigth = c(9, 13, 153, 50, 15, 68, 27, 39,
23, 52, 11, 32, 24, 48, 73, 42, 43, 22, 7, 18, 4, 30), value = c(150,
35, 200, 160, 60, 45, 60, 40, 30, 10, 70, 30, 15, 10, 40, 70,
75, 80, 20, 12, 50, 10)), .Names = c("item", "weigth", "value"
), row.names = c(NA, 22L), class = "data.frame")


Bounded_knapsack<-function(Data,W)
{
	K<-matrix(NA,nrow=W+1,ncol=dim(Data)[1]+1)
	0->K[1,]->K[,1]
	matrix_item<-matrix('',nrow=W+1,ncol=dim(Data)[1]+1)
	for(j in 1:dim(Data)[1])
	{
		for(w in 1:W)
		{
			wj<-Data$weigth[j]
			item<-Data$item[j]
			value<-Data$value[j]
			if( wj > w )
			{
				K[w+1,j+1]<-K[w+1,j]
				matrix_item[w+1,j+1]<-matrix_item[w+1,j]
			}
			else
			{
				if( K[w+1,j] >= K[w+1-wj,j]+value )
				{
					K[w+1,j+1]<-K[w+1,j]
					matrix_item[w+1,j+1]<-matrix_item[w+1,j]
				}
				else
				{
					K[w+1,j+1]<-K[w+1-wj,j]+value
					matrix_item[w+1,j+1]<-item
				}
			}
		}
	}
return(list(K=K,Item=matrix_item))
}

backtracking<-function(knapsack, Data)
{
	W<-dim(knapsack$K)[1]
	itens<-c()
	col<-dim(knapsack$K)[2]
	selected_item<-knapsack$Item[W,col]
	while(selected_item!='')
	{
		selected_item<-knapsack$Item[W,col]
		if(selected_item!='')
		{
			selected_item_value<-Data[Data$item == selected_item,]
			if(-knapsack$K[W - selected_item_value$weigth,col-1]+knapsack$K[W,col]==selected_item_value$value)
			{
				W <- W - selected_item_value$weigth
				itens<-c(itens,selected_item)
			}
			col <- col - 1
		}
	}
return(itens)
}

print_output<-function(Data,W)
{
	Bounded_knapsack(Data,W)->Knap
	backtracking(Knap, Data)->Items
	output<-paste('You must carry:', paste(Items, sep = ', '), sep=' ' )
	return(output)
}

print_output(Full_Data, 400)


```


{{out}}
<lang>
 [1] "You must carry: socks"
 [2] "You must carry: sunglasses"
 [3] "You must carry: note-case"
 [4] "You must carry: waterproof_overclothes"
 [5] "You must carry: waterproof_trousers"
 [6] "You must carry: suntan_cream"
 [7] "You must carry: banana"
 [8] "You must carry: glucose"
 [9] "You must carry: sandwich"
[10] "You must carry: water"
[11] "You must carry: compass"
[12] "You must carry: map"

```



## REXX

Originally, the combination generator/checker subroutine was recursive and made the program solution generic (and more concise).

However, a recursive solution also made the solution much more slower, so the combination generator/checker was "unrolled" and converted into discrete combination checks (based on the number of allowable items).   The unused combinatorial checks were discarded and only the pertinent code was retained.   It made no sense to include all the unused subroutines here as space appears to be a premium for some entries in Rosetta Code.

The term   ''allowable items''   refers to all items that are of allowable weight (those that weigh within the weight criteria).   An half metric─ton anvil was added to the list to show how overweight items are pruned from the list of items.

```rexx
/*REXX program solves a  knapsack problem  (22  {+1}  items with a weight restriction). */
maxWeight=400                                    /*the maximum weight for the knapsack. */
     say 'maximum weight allowed for a knapsack:'  commas(maxWeight);          say
call gen@                                        /*generate the   @   array of choices. */
call sortD                                       /*  sort    "    "     "    "    "     */
call build                                       /*build some associative arrays from @.*/
call findBest                                    /*go ye forth and find the best choises*/
call results                                     /*display the best choices for knapsack*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
build:    do j=1  for obj; parse var @.j x w v . /*construct a list of knapsack choices.*/
          if w>maxWeight  then iterate           /*Is weight greater than max?   Ignore.*/
          totW=totW + w;        totV=totV + v    /*add the totals (for output alignment)*/
          maxL=max(maxL, length(x) )             /*determine maximum width for an item. */
          #=#+1;  i.#=x;  w.#=w;  v.#=v          /*bump the number of items (choices).  */
          end   /*j*/                            /* [↑]  build indexable arrays of items*/
      maxL= maxL + maxL%4 + 4                    /*extend width of name for shown table.*/
      maxW= max(maxW, length( commas(totW) ) )   /*find the maximum width for  weight.  */
      maxV= max(maxV, length( commas(totV) ) )   /*  "   "     "      "    "   value.   */
      call hdr 'potential knapsack items'        /*display a header for list of choices.*/
          do j=1  for obj; parse var @.j i w v . /*show all the choices in a nice format*/
          if w<=maxWeight  then call show i,w,v  /*Is weight within limits?  Then show. */
          end   /*j*/                            /* [↑]  display the list of choices.   */
      $=0
      say;  say 'number of allowable items: '  #
      return
/*──────────────────────────────────────────────────────────────────────────────────────*/
commas: procedure;  parse arg _;   n=_'.9';    #=123456789;     b=verify(n, #, "M")
        e=verify(n, #'0', , verify(n, #"0.", 'M')) - 4;         comma=','
           do j=e  to b  by -3;   _=insert(comma, _, j);   end  /*j*/;            return _
/*──────────────────────────────────────────────────────────────────────────────────────*/
findBest:                                m=maxWeight   /*items are in decreasing weight.*/
   do j1 =0          for #+1;                                 w1 =    w.j1 ; z1 =    v.j1
   do j2 =j1 +(j1 >0) to #; if w.j2 +w1 >m  then iterate j1 ; w2 =w1 +w.j2 ; z2 =z1 +v.j2
   do j3 =j2 +(j2 >0) to #; if w.j3 +w2 >m  then iterate j2 ; w3 =w2 +w.j3 ; z3 =z2 +v.j3
   do j4 =j3 +(j3 >0) to #; if w.j4 +w3 >m  then iterate j3 ; w4 =w3 +w.j4 ; z4 =z3 +v.j4
   do j5 =j4 +(j4 >0) to #; if w.j5 +w4 >m  then iterate j4 ; w5 =w4 +w.j5 ; z5 =z4 +v.j5
   do j6 =j5 +(j5 >0) to #; if w.j6 +w5 >m  then iterate j5 ; w6 =w5 +w.j6 ; z6 =z5 +v.j6
   do j7 =j6 +(j6 >0) to #; if w.j7 +w6 >m  then iterate j6 ; w7 =w6 +w.j7 ; z7 =z6 +v.j7
   do j8 =j7 +(j7 >0) to #; if w.j8 +w7 >m  then iterate j7 ; w8 =w7 +w.j8 ; z8 =z7 +v.j8
   do j9 =j8 +(j8 >0) to #; if w.j9 +w8 >m  then iterate j8 ; w9 =w8 +w.j9 ; z9 =z8 +v.j9
   do j10=j9 +(j9 >0) to #; if w.j10+w9 >m  then iterate j9 ; w10=w9 +w.j10; z10=z9 +v.j10
   do j11=j10+(j10>0) to #; if w.j11+w10>m  then iterate j10; w11=w10+w.j11; z11=z10+v.j11
   do j12=j11+(j11>0) to #; if w.j12+w11>m  then iterate j11; w12=w11+w.j12; z12=z11+v.j12
   do j13=j12+(j12>0) to #; if w.j13+w12>m  then iterate j12; w13=w12+w.j13; z13=z12+v.j13
   do j14=j13+(j13>0) to #; if w.j14+w13>m  then iterate j13; w14=w13+w.j14; z14=z13+v.j14
   do j15=j14+(j14>0) to #; if w.j15+w14>m  then iterate j14; w15=w14+w.j15; z15=z14+v.j15
   do j16=j15+(j15>0) to #; if w.j16+w15>m  then iterate j15; w16=w15+w.j16; z16=z15+v.j16
   do j17=j16+(j16>0) to #; if w.j17+w16>m  then iterate j16; w17=w16+w.j17; z17=z16+v.j17
   do j18=j17+(j17>0) to #; if w.j18+w17>m  then iterate j17; w18=w17+w.j18; z18=z17+v.j18
   do j19=j18+(j18>0) to #; if w.j19+w18>m  then iterate j18; w19=w18+w.j19; z19=z18+v.j19
   do j20=j19+(j19>0) to #; if w.j20+w19>m  then iterate j19; w20=w19+w.j20; z20=z19+v.j20
   do j21=j20+(j20>0) to #; if w.j21+w20>m  then iterate j20; w21=w20+w.j21; z21=z20+v.j21
   do j22=j21+(j21>0) to #; if w.j22+w21>m  then iterate j21; w22=w21+w.j22; z22=z21+v.j22
   if z22>$  then do;  ?=;  $=z22;    do j=1  for 22;  ?=? value("J"j);  end /*j*/;    end
   end;end;end;end;end;end;end;end;end;end;end;end;end;end;end;end;end;end;end;end;end;end
return
/*──────────────────────────────────────────────────────────────────────────────────────*/
gen@:          @.  =                           ;     @.12= 'camera                 32  30'
               @.1 = 'map               9 150' ;     @.13= 'T-shirt                24  15'
               @.2 = 'compass          13  35' ;     @.14= 'trousers               48  10'
               @.3 = 'water           153 200' ;     @.15= 'umbrella               73  40'
               @.4 = 'sandwich         50 160' ;     @.16= 'waterproof_trousers    42  70'
               @.5 = 'glucose          15  60' ;     @.17= 'waterproof_overclothes 43  75'
               @.6 = 'tin              68  45' ;     @.18= 'note-case              22  80'
               @.7 = 'banana           27  60' ;     @.19= 'sunglasses              7  20'
               @.8 = 'apple            39  40' ;     @.20= 'towel                  18  12'
               @.9 = 'cheese           23  30' ;     @.21= 'socks                   4  50'
               @.10= 'beer             52  10' ;     @.22= 'book                   30  10'
               @.11= 'suntan_cream     11  70' ;     @.23= 'anvil              100000   1'
      maxL = length('potential knapsack items')  /*maximum width for the table items.   */
      maxW = length('weight')                    /*   "      "    "   "    "   weights. */
      maxV = length('value')                     /*   "      "    "   "    "   values.  */
      #=0;  i.=;  w.=0;  v.=0;  totW=0;  totV=0  /*initialize some REX variables stuff. */
      return
/*──────────────────────────────────────────────────────────────────────────────────────*/
hdr:    say;  call show center(arg(1),maxL),center('weight',maxW),center("value",maxV)
hdr2:         call show copies('═',maxL),copies('═',maxW),copies('═',maxV);       return
/*──────────────────────────────────────────────────────────────────────────────────────*/
results:          do #;  ?=strip( space(?), "L", 0);  end  /*h*/  /*elide leading zeroes*/
        bestC=?;    bestW=0;         totP=words(bestC);     say;    call hdr 'best choice'
                  do j=1  for totP;  _=word(bestC, j);      _w=w._;      _v=v._
                      do k=j+1  to totP;     __=word(bestC, k);   if i._\==i.__ then leave
                      j=j+1;  w._=w._ + _w;  v._=v._ + _v
                      end    /*k*/
                  call show i._, w._, v._;   bestW=bestW + w._
                  end        /*j*/
        call hdr2                   ;   say;                @bestTK= 'best total knapsack'
        call show @bestTK  'weight' ,   bestW    /*display a nicely formatted winner wt.*/
        call show @bestTK  'value'  ,,  $        /*     "  "    "       "     winner val*/
        call show @bestTK  'items'  ,,, totP     /*     "  "    "       "     pieces.   */
        return
/*──────────────────────────────────────────────────────────────────────────────────────*/
show:   parse arg _i,_w,_v,_p;  say translate( left(_i,maxL,'─'), , "_") ,
                                right(commas(_w),maxW) right(commas(_v),maxV) _p;   return
/*──────────────────────────────────────────────────────────────────────────────────────*/
sortD:     do j=1  while @.j\==''; y=word(@.j,2) /*process each of the knapsack choices.*/
                do k=j+1  while @.k\==''         /*find a possible heavier knapsack item*/
                ?=word(@.k,2);  if ?>y  then do; _=@.k; @.k=@.j; @.j=_; y=?; end  /*swap*/
                end   /*k*/
           end        /*j*/                      /* [↑]  sort choices by decreasing wt. */
        obj=j-1;                   return        /*decrement  J  for the  DO  loop index*/
```

{{out|output|text=  when using the default input:}}

```txt

maximum weight allowed for a knapsack: 400


     potential knapsack items      weight value
══════════════════════════════════ ══════ ═════
water─────────────────────────────    153   200
umbrella──────────────────────────     73    40
tin───────────────────────────────     68    45
beer──────────────────────────────     52    10
sandwich──────────────────────────     50   160
trousers──────────────────────────     48    10
waterproof overclothes────────────     43    75
waterproof trousers───────────────     42    70
apple─────────────────────────────     39    40
camera────────────────────────────     32    30
book──────────────────────────────     30    10
banana────────────────────────────     27    60
T-shirt───────────────────────────     24    15
cheese────────────────────────────     23    30
note-case─────────────────────────     22    80
towel─────────────────────────────     18    12
glucose───────────────────────────     15    60
compass───────────────────────────     13    35
suntan cream──────────────────────     11    70
map───────────────────────────────      9   150
sunglasses────────────────────────      7    20
socks─────────────────────────────      4    50

number of allowable items:  22


           best choice             weight value
══════════════════════════════════ ══════ ═════
water─────────────────────────────    153   200
sandwich──────────────────────────     50   160
waterproof overclothes────────────     43    75
waterproof trousers───────────────     42    70
banana────────────────────────────     27    60
note-case─────────────────────────     22    80
glucose───────────────────────────     15    60
compass───────────────────────────     13    35
suntan cream──────────────────────     11    70
map───────────────────────────────      9   150
sunglasses────────────────────────      7    20
socks─────────────────────────────      4    50
══════════════════════════════════ ══════ ═════

best total knapsack weight────────    396
best total knapsack value─────────        1,030
best total knapsack items─────────              12

```



## Ring


```ring

# Project : Knapsack problem/0-1

knap = [["map",9,150],
            ["compass",13,35],
            ["water",153,20],
            ["sandwich",50,160],
            ["glucose",15,60],
            ["tin",68,45],
            ["banana",27,60],
            ["apple",39,40],
            ["cheese",23,30],
            ["beer",52,10],
            ["suntan cream",11,70],
            ["camera",32,30],
            ["T-shirt",24,15],
            ["trousers",48,10],
            ["umbrella",73,40],
            ["waterproof trousers",42,70],
            ["waterproof overclothes",43,75],
            ["note-case",22,80],
            ["sunglasses",7,20],
            ["towel",18,12],
            ["socks",4,50],
            ["book",30,10]]
knapsack = createDimList([pow(2, len(knap)),len(knap)+2])
lenknap = list(pow(2, len(knap)))

sacksize = 400
powerset(knap)

for n = 1 to pow(2, len(knap))-2
      for m = n + 1 to pow(2, len(knap))-1
      if knapsack[m][lenknap[m]-1] <= sacksize and
         knapsack[m][lenknap[m]] > knapsack[n][lenknap[n]]
         temp = knapsack[n]
         lentemp = lenknap[n]
         knapsack[n] = knapsack[m]
         knapsack[n+1] = temp
         lenknap[n] = lenknap[m]
         lenknap[n+1] = lentemp
      ok
      next
next

for n = 1 to lenknap[1] - 2
      see knapsack[1][n] + nl
next

see "Total weight = " + knapsack[1][lenknap[1]-1] + nl
see "Total value  = " + knapsack[1][lenknap[1]] + nl

func powerset(list)
        n1 = 0
        for i = 2 to (2 << len(list)) - 1 step 2
             n2 = 0
             n1 = n1 + 1
             weight = 0
             value = 0
             for j = 1 to len(list)
                  if i & (1 << j)
                     n2 = n2 + 1
                     knapsack[n1][n2] = list[j][1]
                     weight = weight + list[j][2]
                     value = value + list[j][3]
                     knapsack[n1][n2+1] = weight
                     knapsack[n1][n2+2] = value
                  ok
             next
             lenknap[n1] = n2+2
         next

func createDimList(dimArray)
        sizeList = len(dimArray)
        newParms = []
        for i = 2 to sizeList
            Add(newParms, dimArray[i])
        next
        alist = list(dimArray[1])
        if sizeList = 1
           return aList
        ok
        for t in alist
              t = createDimList(newParms)
        next
        return alist

```

Output:

```txt

map
compass
water
sandwich
glucose
banana
suntan cream
waterproof trousers
waterproof overclothes
note-case
sunglasses
socks

Total weight = 396
Total value  = 1030

```



## Ruby


###  Brute force


```ruby
KnapsackItem = Struct.new(:name, :weight, :value)
potential_items = [
  KnapsackItem['map', 9, 150],              KnapsackItem['compass', 13, 35],
  KnapsackItem['water', 153, 200],          KnapsackItem['sandwich', 50, 160],
  KnapsackItem['glucose', 15, 60],          KnapsackItem['tin', 68, 45],
  KnapsackItem['banana', 27, 60],           KnapsackItem['apple', 39, 40],
  KnapsackItem['cheese', 23, 30],           KnapsackItem['beer', 52, 10],
  KnapsackItem['suntan cream', 11, 70],     KnapsackItem['camera', 32, 30],
  KnapsackItem['t-shirt', 24, 15],          KnapsackItem['trousers', 48, 10],
  KnapsackItem['umbrella', 73, 40],         KnapsackItem['waterproof trousers', 42, 70],
  KnapsackItem['waterproof overclothes', 43, 75], KnapsackItem['note-case', 22, 80],
  KnapsackItem['sunglasses', 7, 20],        KnapsackItem['towel', 18, 12],
  KnapsackItem['socks', 4, 50],             KnapsackItem['book', 30, 10],
]
knapsack_capacity = 400

class Array
  # do something for each element of the array's power set
  def power_set
    yield [] if block_given?
    self.inject([[]]) do |ps, elem|
      ps.each_with_object([]) do |i,r|
        r << i
        new_subset = i + [elem]
        yield new_subset if block_given?
        r << new_subset
      end
    end
  end
end

maxval, solutions = potential_items.power_set.group_by {|subset|
  weight = subset.inject(0) {|w, elem| w + elem.weight}
  weight>knapsack_capacity ? 0 : subset.inject(0){|v, elem| v + elem.value}
}.max

puts "value: #{maxval}"
solutions.each do |set|
  wt, items = 0, []
  set.each {|elem| wt += elem.weight; items << elem.name}
  puts "weight: #{wt}"
  puts "items: #{items.join(',')}"
end
```


{{out}}
<pre style='width: full; overflow: scroll'>
value: 1030
weight: 396
items: map,compass,water,sandwich,glucose,banana,suntan cream,waterproof trousers,waterproof overclothes,note-case,sunglasses,socks

```



###  Dynamic Programming

Translated from http://sites.google.com/site/mikescoderama/Home/0-1-knapsack-problem-in-p

```ruby
KnapsackItem = Struct.new(:name, :weight, :value)

def dynamic_programming_knapsack(items, max_weight)
  num_items = items.size
  cost_matrix = Array.new(num_items){Array.new(max_weight+1, 0)}

  num_items.times do |i|
    (max_weight + 1).times do |j|
      if(items[i].weight > j)
        cost_matrix[i][j] = cost_matrix[i-1][j]
      else
        cost_matrix[i][j] = [cost_matrix[i-1][j], items[i].value + cost_matrix[i-1][j-items[i].weight]].max
      end
    end
  end
  used_items = get_used_items(items, cost_matrix)
  [get_list_of_used_items_names(items, used_items),                     # used items names
   items.zip(used_items).map{|item,used| item.weight*used}.inject(:+),  # total weight
   cost_matrix.last.last]                                               # total value
end

def get_used_items(items, cost_matrix)
  i = cost_matrix.size - 1
  currentCost = cost_matrix[0].size - 1
  marked = cost_matrix.map{0}

  while(i >= 0 && currentCost >= 0)
    if(i == 0 && cost_matrix[i][currentCost] > 0 ) || (cost_matrix[i][currentCost] != cost_matrix[i-1][currentCost])
      marked[i] = 1
      currentCost -= items[i].weight
    end
    i -= 1
  end
  marked
end

def get_list_of_used_items_names(items, used_items)
  items.zip(used_items).map{|item,used| item.name if used>0}.compact.join(', ')
end

if $0 == __FILE__
  items = [
    KnapsackItem['map'                   ,   9, 150], KnapsackItem['compass'            , 13,  35],
    KnapsackItem['water'                 , 153, 200], KnapsackItem['sandwich'           , 50, 160],
    KnapsackItem['glucose'               ,  15,  60], KnapsackItem['tin'                , 68,  45],
    KnapsackItem['banana'                ,  27,  60], KnapsackItem['apple'              , 39,  40],
    KnapsackItem['cheese'                ,  23,  30], KnapsackItem['beer'               , 52,  10],
    KnapsackItem['suntan cream'          ,  11,  70], KnapsackItem['camera'             , 32,  30],
    KnapsackItem['t-shirt'               ,  24,  15], KnapsackItem['trousers'           , 48,  10],
    KnapsackItem['umbrella'              ,  73,  40], KnapsackItem['waterproof trousers', 42,  70],
    KnapsackItem['waterproof overclothes',  43,  75], KnapsackItem['note-case'          , 22,  80],
    KnapsackItem['sunglasses'            ,   7,  20], KnapsackItem['towel'              , 18,  12],
    KnapsackItem['socks'                 ,   4,  50], KnapsackItem['book'               , 30,  10]
  ]

  names, weight, value = dynamic_programming_knapsack(items, 400)
  puts
  puts 'Dynamic Programming:'
  puts
  puts "Found solution: #{names}"
  puts "total weight: #{weight}"
  puts "total value: #{value}"
end
```

{{out}}
<pre style='width: full; overflow: scroll'>
Dynamic Programming:

Found solution: map, compass, water, sandwich, glucose, banana, suntan cream, waterproof trousers, waterproof overclothes, note-case, sunglasses, socks
total weight: 396
total value: 1030

```



## Rust

Dynamic Programming solution.

```rust
use std::cmp;

struct Item {
    name: &'static str,
    weight: usize,
    value: usize
}

fn knapsack01_dyn(items: &[Item], max_weight: usize) -> Vec<&Item> {
    let mut best_value = vec![vec![0; max_weight + 1]; items.len() + 1];
    for (i, it) in items.iter().enumerate() {
        for w in 1 .. max_weight + 1 {
            best_value[i + 1][w] =
                if it.weight > w {
                    best_value[i][w]
                } else {
                    cmp::max(best_value[i][w], best_value[i][w - it.weight] + it.value)
                }
        }
    }

    let mut result = Vec::with_capacity(items.len());
    let mut left_weight = max_weight;

    for (i, it) in items.iter().enumerate().rev() {
        if best_value[i + 1][left_weight] != best_value[i][left_weight] {
            result.push(it);
            left_weight -= it.weight;
        }
    }

    result
}


fn main () {
    const MAX_WEIGHT: usize = 400;

    const ITEMS: &[Item] = &[
        Item { name: "map",                    weight: 9,   value: 150 },
        Item { name: "compass",                weight: 13,  value: 35 },
        Item { name: "water",                  weight: 153, value: 200 },
        Item { name: "sandwich",               weight: 50,  value: 160 },
        Item { name: "glucose",                weight: 15,  value: 60 },
        Item { name: "tin",                    weight: 68,  value: 45 },
        Item { name: "banana",                 weight: 27,  value: 60 },
        Item { name: "apple",                  weight: 39,  value: 40 },
        Item { name: "cheese",                 weight: 23,  value: 30 },
        Item { name: "beer",                   weight: 52,  value: 10 },
        Item { name: "suntancream",            weight: 11,  value: 70 },
        Item { name: "camera",                 weight: 32,  value: 30 },
        Item { name: "T-shirt",                weight: 24,  value: 15 },
        Item { name: "trousers",               weight: 48,  value: 10 },
        Item { name: "umbrella",               weight: 73,  value: 40 },
        Item { name: "waterproof trousers",    weight: 42,  value: 70 },
        Item { name: "waterproof overclothes", weight: 43,  value: 75 },
        Item { name: "note-case",              weight: 22,  value: 80 },
        Item { name: "sunglasses",             weight: 7,   value: 20 },
        Item { name: "towel",                  weight: 18,  value: 12 },
        Item { name: "socks",                  weight: 4,   value: 50 },
        Item { name: "book",                   weight: 30,  value: 10 }
    ];

    let items = knapsack01_dyn(ITEMS, MAX_WEIGHT);

    // We reverse the order because we solved the problem backward.
    for it in items.iter().rev() {
        println!("{}", it.name);
    }

    println!("Total weight: {}", items.iter().map(|w| w.weight).sum::<usize>());
    println!("Total value: {}", items.iter().map(|w| w.value).sum::<usize>());
}
```

{{out}}

```txt
map
compass
water
sandwich
glucose
banana
suntancream
waterproof trousers
waterproof overclothes
note-case
sunglasses
socks
Total weight: 396
Total value: 1030
```



## SAS


Use MILP solver in SAS/OR:

```sas
/* create SAS data set */
data mydata;
   input item $1-23 weight value;
   datalines;
map                      9  150
compass                 13   35
water                  153  200
sandwich                50  160
glucose                 15   60
tin                     68   45
banana                  27   60
apple                   39   40
cheese                  23   30
beer                    52   10
suntan cream            11   70
camera                  32   30
T-shirt                 24   15
trousers                48   10
umbrella                73   40
waterproof trousers     42   70
waterproof overclothes  43   75
note-case               22   80
sunglasses               7   20
towel                   18   12
socks                    4   50
book                    30   10
;

/* call OPTMODEL procedure in SAS/OR */
proc optmodel;
   /* declare sets and parameters, and read input data */
   set <str> ITEMS;
   num weight {ITEMS};
   num value {ITEMS};
   read data mydata into ITEMS=[item] weight value;

   /* declare variables, objective, and constraints */
   var NumSelected {ITEMS} binary;
   max TotalValue = sum {i in ITEMS} value[i] * NumSelected[i];
   con WeightCon:
      sum {i in ITEMS} weight[i] * NumSelected[i] <= 400;

   /* call mixed integer linear programming (MILP) solver */
   solve;

   /* print optimal solution */
   print TotalValue;
   print {i in ITEMS: NumSelected[i].sol > 0.5} NumSelected;
quit;
```


Output:

```txt

TotalValue
1030

[1] NumSelected
banana 1
compass 1
glucose 1
map 1
note-case 1
sandwich 1
socks 1
sunglasses 1
suntan cream 1
water 1
waterproof overclothes 1
waterproof trousers 1

```



## Scala

{{works with|Scala|2.9.2}}

```Scala
object Knapsack extends App {

  case class Item(name: String, weight: Int, value: Int)

  val elapsed: (=> Unit) => Long = f => {val s = System.currentTimeMillis; f; (System.currentTimeMillis - s)/1000}

  //===== brute force (caution: increase the heap!)
### ==============================

  val ks01b: List[Item] => Unit = loi => {
    val tw:Set[Item]=>Int=ps=>(ps:\0)((a,b)=>a.weight+b) //total weight
    val tv:Set[Item]=>Int=ps=>(ps:\0)((a,b)=>a.value+b) //total value
    val pis = (loi.toSet.subsets).toList.filterNot(_==Set())

   #[test]
fn test_dp_results() {
    let dp_results = knap_01_dp(items, 400);
    let dp_weights= dp_results.iter().fold(0, |a, &b| a + b.weight);
    let dp_values = dp_results.iter().fold(0, |a, &b| a + b.value);
    assert_eq!(dp_weights, 396);
    assert_eq!(dp_values, 1030);
} val res = pis.map(ss=>Pair(ss,tw(ss)))
      .filter(p=>p._2>350 && p._2<401).map(p=>Pair(p,tv(p._1)))
      .sortWith((s,t)=>s._2.compareTo(t._2) < 0)
      .last
    println{val h = "packing list of items (brute force):"; h+"\n"+"="*h.size}
    res._1._1.foreach{p=>print("  "+p.name+": weight="+p.weight+" value="+p.value+"\n")}
    println("\n"+"  resulting items: "+res._1._1.size+" of "+loi.size)
    println("  total weight: "+res._1._2+", total value: "+res._2)
  }

  //
### == dynamic programming =======================================================

  val ks01d: List[Item] => Unit = loi => {
    val W = 400
    val N = loi.size

    val m = Array.ofDim[Int](N+1,W+1)
    val plm = (List((for {w <- 0 to W} yield Set[Item]()).toArray)++(
                for {
                  n <- 0 to N-1
                  colN = (for {w <- 0 to W} yield Set[Item](loi(n))).toArray
                } yield colN)).toArray

    1 to N foreach {n =>
      0 to W foreach {w =>
        def in = loi(n-1)
        def wn = loi(n-1).weight
        def vn = loi(n-1).value
        if (w<wn) {
          m(n)(w) = m(n-1)(w)
          plm(n)(w) = plm(n-1)(w)
        }
        else {
          if (m(n-1)(w)>=m(n-1)(w-wn)+vn) {
            m(n)(w) = m(n-1)(w)
            plm(n)(w) = plm(n-1)(w)
          }
          else {
            m(n)(w) = m(n-1)(w-wn)+vn
	    plm(n)(w) = plm(n-1)(w-wn)+in
	  }
	}
      }
    }

    println{val h = "packing list of items (dynamic programming):"; h+"\n"+"="*h.size}
    plm(N)(W).foreach{p=>print("  "+p.name+": weight="+p.weight+" value="+p.value+"\n")}
    println("\n"+"  resulting items: "+plm(N)(W).size+" of "+loi.size)
    println("  total weight: "+(0/:plm(N)(W).map{item=>item.weight})(_+_)+", total value: "+m(N)(W))
  }

  val items = List(
     Item("map", 9, 150)
    ,Item("compass", 13, 35)
    ,Item("water", 153, 200)
    ,Item("sandwich", 50, 160)
    ,Item("glucose", 15, 60)
    ,Item("tin", 68, 45)
    ,Item("banana", 27, 60)
    ,Item("apple", 39, 40)
    ,Item("cheese", 23, 30)
    ,Item("beer", 52, 10)
    ,Item("suntan cream", 11, 70)
    ,Item("camera", 32, 30)
    ,Item("t-shirt", 24, 15)
    ,Item("trousers", 48, 10)
    ,Item("umbrella", 73, 40)
    ,Item("waterproof trousers", 42, 70)
    ,Item("waterproof overclothes", 43, 75)
    ,Item("note-case", 22, 80)
    ,Item("sunglasses", 7, 20)
    ,Item("towel", 18, 12)
    ,Item("socks", 4, 50)
    ,Item("book", 30, 10)
  )

  List(ks01b, ks01d).foreach{f=>
    val t = elapsed{f(items)}
    println("  elapsed time: "+t+" sec"+"\n")
  }
}
```

{{out}}

```txt
packing list of items (brute force):

### ==============================

  waterproof overclothes: weight=43 value=75
  note-case: weight=22 value=80
  socks: weight=4 value=50
  sandwich: weight=50 value=160
  banana: weight=27 value=60
  glucose: weight=15 value=60
  map: weight=9 value=150
  water: weight=153 value=200
  suntan cream: weight=11 value=70
  sunglasses: weight=7 value=20
  waterproof trousers: weight=42 value=70
  compass: weight=13 value=35

  resulting items: 12 of 22
  total weight: 396, total value: 1030
  elapsed time: 19 sec

packing list of items (dynamic programming):

### ======================================

  waterproof overclothes: weight=43 value=75
  note-case: weight=22 value=80
  socks: weight=4 value=50
  sandwich: weight=50 value=160
  banana: weight=27 value=60
  glucose: weight=15 value=60
  map: weight=9 value=150
  water: weight=153 value=200
  suntan cream: weight=11 value=70
  sunglasses: weight=7 value=20
  waterproof trousers: weight=42 value=70
  compass: weight=13 value=35

  resulting items: 12 of 22
  total weight: 396, total value: 1030
  elapsed time: 0 sec
```



## Sidef

{{trans|Perl}}

```ruby
var raw = <<'TABLE'
map,                      9, 150
compass,                 13,  35
water,                  153, 200
sandwich,                50, 160
glucose,                 15,  60
tin,                     68,  45
banana,                  27,  60
apple,                   39,  40
cheese,                  23,  30
beer,                    52,  10
suntancream,             11,  70
camera,                  32,  30
T-shirt,                 24,  15
trousers,                48,  10
umbrella,                73,  40
waterproof trousers,     42,  70
waterproof overclothes,  43,  75
note-case,               22,  80
sunglasses,               7,  20
towel,                   18,  12
socks,                    4,  50
book,                    30,  10
TABLE

struct KnapsackItem {
    String name,
    Number weight,
    Number value,
}

var items = []
raw.each_line{ |row|
    var fields = row.split(/\s*,\s*/)
    items << KnapsackItem(
          name: fields[0],
        weight: fields[1].to_n,
         value: fields[2].to_n,
    )
}

var max_weight = 400
var p = [
    items.len.of { [[0, []], max_weight.of(nil)...] }...,
    max_weight.inc.of {[0, []]}
]

func optimal(i, w) {
    if (!defined p[i][w]) {
        var item = items[i];
        if (item.weight > w) {
            p[i][w] = optimal(i.dec, w)
        }
        else {
            var x = optimal(i.dec, w)
            var y = optimal(i.dec, w - item.weight)

            if (x[0] > (y[0] + item.value)) {
                p[i][w] = x;
            }
            else {
                p[i][w] = [y[0] + item.value, [y[1]..., item.name]]
            }
        }
    }
    return p[i][w]
}

var sol = optimal(items.end, max_weight)
say "#{sol[0]}: #{sol[1]}"
```

{{out}}

```txt

1030: map compass water sandwich glucose banana suntancream waterproof trousers waterproof overclothes note-case sunglasses socks

```



## SQL

A brute force solution that runs in SQL Server 2005 or later using a recursive CTE.
Displays the top 5 solutions and runs in about 39 seconds.


```SQL

WITH KnapsackItems (item, [weight], value) AS
(
    SELECT 'map',9,  150
    UNION ALL SELECT 'compass',13,  35
    UNION ALL SELECT 'water',153,  200
    UNION ALL SELECT 'sandwich',50,  160
    UNION ALL SELECT 'glucose',15,  60
    UNION ALL SELECT 'tin',68,  45
    UNION ALL SELECT 'banana',27,  60
    UNION ALL SELECT 'apple',39,  40
    UNION ALL SELECT 'cheese',23,  30
    UNION ALL SELECT 'beer',52,  10
    UNION ALL SELECT 'suntan cream',11,  70
    UNION ALL SELECT 'camera',32,  30
    UNION ALL SELECT 'T-shirt',24,  15
    UNION ALL SELECT 'trousers',48,  10
    UNION ALL SELECT 'umbrella',73,  40
    UNION ALL SELECT 'waterproof trousers',42,  70
    UNION ALL SELECT 'waterproof overclothes',43,  75
    UNION ALL SELECT 'note-case',22,  80
    UNION ALL SELECT 'sunglasses',7,  20
    UNION ALL SELECT 'towel',18,  12
    UNION ALL SELECT 'socks',4,  50
    UNION ALL SELECT 'book',30,  10
)
SELECT *
INTO #KnapsackItems
FROM KnapsackItems;

WITH UNIQUEnTuples (n, Tuples, ID, [weight], value) AS (
    SELECT 1, CAST(item AS VARCHAR(8000)), item, [weight], value
    FROM #KnapsackItems
    UNION ALL
    SELECT 1 + n.n, t.item + ',' + n.Tuples, item, n.[weight] + t.[weight], n.value + t.value
    FROM UNIQUEnTuples n
    CROSS APPLY (
        SELECT item, [weight], value
        FROM #KnapsackItems t
        WHERE t.item < n.ID AND n.[weight] + t.[weight] < 400) t
    )
SELECT TOP 5 *
FROM UNIQUEnTuples
ORDER BY value DESC, n, Tuples;

GO
DROP TABLE #KnapsackItems;

```

{{out}}

```txt

weight  value  Solution
396     1030   banana,compass,glucose,map,note-case,sandwich,socks,sunglasses,suntan cream,water,waterproof overclothes,waterproof trousers
389     1010   banana,compass,glucose,map,note-case,sandwich,socks,suntan cream,water,waterproof overclothes,waterproof trousers
399     1005   banana,cheese,glucose,map,note-case,sandwich,socks,suntan cream,water,waterproof overclothes,waterproof trousers
395     1002   banana,cheese,compass,glucose,map,note-case,sandwich,socks,sunglasses,suntan cream,towel,water,waterproof overclothes
393     1000   apple,banana,compass,glucose,map,note-case,sandwich,socks,sunglasses,suntan cream,water,waterproof overclothes

```



## Swift


{{trans|Python}}


###  Dynamic Programming



```swift
struct KnapsackItem {
  var name: String
  var weight: Int
  var value: Int
}

func knapsack(items: [KnapsackItem], limit: Int) -> [KnapsackItem] {
  var table = Array(repeating: Array(repeating: 0, count: limit + 1), count: items.count + 1)

  for j in 1..<items.count+1 {
    let item = items[j-1]

    for w in 1..<limit+1 {
      if item.weight > w {
        table[j][w] = table[j-1][w]
      } else {
        table[j][w] = max(table[j-1][w], table[j-1][w-item.weight] + item.value)
      }
    }
  }

  var result = [KnapsackItem]()
  var w = limit

  for j in stride(from: items.count, to: 0, by: -1) where table[j][w] != table[j-1][w] {
    let item = items[j-1]

    result.append(item)

    w -= item.weight
  }

  return result
}

let items = [
  KnapsackItem(name: "map", weight: 9, value: 150), KnapsackItem(name: "compass", weight: 13, value: 35),
  KnapsackItem(name: "water", weight: 153, value: 200), KnapsackItem(name: "sandwich", weight: 50, value: 160),
  KnapsackItem(name: "glucose", weight: 15, value: 60), KnapsackItem(name: "tin", weight: 68, value: 45),
  KnapsackItem(name: "banana", weight: 27, value: 60), KnapsackItem(name: "apple", weight: 39, value: 40),
  KnapsackItem(name: "cheese", weight: 23, value: 30), KnapsackItem(name: "beer", weight: 52, value: 10),
  KnapsackItem(name: "suntan cream", weight: 11, value: 70), KnapsackItem(name: "camera", weight: 32, value: 30),
  KnapsackItem(name: "t-shirt", weight: 24, value: 15), KnapsackItem(name: "trousers", weight: 48, value: 10),
  KnapsackItem(name: "umbrella", weight: 73, value: 40), KnapsackItem(name: "waterproof trousers", weight: 42, value: 70),
  KnapsackItem(name: "waterproof overclothes", weight: 43, value: 75), KnapsackItem(name: "note-case", weight: 22, value: 80),
  KnapsackItem(name: "sunglasses", weight: 7, value: 20), KnapsackItem(name: "towel", weight: 18, value: 12),
  KnapsackItem(name: "socks", weight: 4, value: 50), KnapsackItem(name: "book", weight: 30, value: 10)
]

let kept = knapsack(items: items, limit: 400)

print("Kept: ")

for item in kept {
  print("  \(item.name)")
}

let (tValue, tWeight) = kept.reduce((0, 0), { ($0.0 + $1.value, $0.1 + $1.weight) })

print("For a total value of \(tValue) and a total weight of \(tWeight)")
```


{{out}}


```txt
Kept:
  socks
  sunglasses
  note-case
  waterproof overclothes
  waterproof trousers
  suntan cream
  banana
  glucose
  sandwich
  water
  compass
  map
For a total value of 1030 and a total weight of 396
```



## Tcl

As the saying goes, “when in doubt, try brute force”. Since there's only 22 items we can simply iterate over all possible choices.

```tcl
# The list of items to consider, as list of lists
set items {
    {map			9	150}
    {compass			13	35}
    {water			153	200}
    {sandwich			50	160}
    {glucose			15	60}
    {tin			68	45}
    {banana			27	60}
    {apple			39	40}
    {cheese			23	30}
    {beer			52	10}
    {{suntan cream}		11	70}
    {camera			32	30}
    {t-shirt			24	15}
    {trousers			48	10}
    {umbrella			73	40}
    {{waterproof trousers}	42	70}
    {{waterproof overclothes}	43	75}
    {note-case			22	80}
    {sunglasses			7	20}
    {towel			18	12}
    {socks			4	50}
    {book			30	10}
}

# Simple extraction functions
proc names {chosen} {
    set names {}
    foreach item $chosen {lappend names [lindex $item 0]}
    return $names
}
proc weight {chosen} {
    set weight 0
    foreach item $chosen {incr weight [lindex $item 1]}
    return $weight
}
proc value {chosen} {
    set value 0
    foreach item $chosen {incr value [lindex $item 2]}
    return $value
}

# Recursive function for searching over all possible choices of items
proc knapsackSearch {items {chosen {}}} {
    # If we've gone over the weight limit, stop now
    if {[weight $chosen] > 400} {
	return
    }
    # If we've considered all of the items (i.e., leaf in search tree)
    # then see if we've got a new best choice.
    if {[llength $items] == 0} {
	global best max
	set v [value $chosen]
	if {$v > $max} {
	    set max $v
	    set best $chosen
	}
	return
    }
    # Branch, so recurse for chosing the current item or not
    set this [lindex $items 0]
    set rest [lrange $items 1 end]
    knapsackSearch $rest $chosen
    knapsackSearch $rest [lappend chosen $this]
}

# Initialize a few global variables
set best {}
set max 0
# Do the brute-force search
knapsackSearch $items
# Pretty-print the results
puts "Best filling has weight of [expr {[weight $best]/100.0}]kg and score [value $best]"
puts "Best items:\n\t[join [lsort [names $best]] \n\t]"
```

{{out}}

```txt

Best filling has weight of 3.96kg and score 1030
Best items:
	banana
	compass
	glucose
	map
	note-case
	sandwich
	socks
	sunglasses
	suntan cream
	water
	waterproof overclothes
	waterproof trousers

```



## Ursala

This solution follows a very similar approach to the one used in [[Knapsack problem/Bounded#Ursala]], which is to treat it as a mixed integer programming problem and solve it using an off-the-shelf library ([http://lpsolve.sourceforge.net lpsolve]).

```Ursala
#import std
#import nat
#import flo
#import lin

#import nat

items = # name: (weight,value)

<
   'map': (9,150),
   'compass': (13,35),
   'water': (153,200),
   'sandwich': (50,160),
   'glucose': (15,60),
   'tin': (68,45),
   'banana': (27,60),
   'apple': (39,40),
   'cheese': (23,30),
   'beer': (52,10),
   'suntan cream': (11,70),
   'camera': (32,30),
   't-shirt': (24,15),
   'trousers': (48,10),
   'umbrella': (73,40),
   'waterproof trousers': (42,70),
   'waterproof overclothes': (43,75),
   'note-case': (22,80),
   'sunglasses': (7,20),
   'towel': (18,12),
   'socks': (4,50),
   'book': (30,10)>

system =

linear_system$[
   binaries: ~&nS,
   lower_bounds: {'(slack)': 0.}!,
   costs: * ^|/~& negative+ float@r,
   equations: ~&iNC\400.+ :/(1.,'(slack)')+ * ^|rlX/~& float@l]

#show+

main = ~&tnS solution system items
```

Binary valued variables are a more specific constraint than the general mixed integer programming problem, but can be accommodated as shown using the <code>binaries</code> field in the <code>linear_system</code> specification. The additional <code>slack</code> variable is specified as continuous and non-negative with no cost or benefit so as to make the constraint equation solvable without affecting the solution.
{{out}}

```txt

banana
compass
glucose
map
note-case
sandwich
socks
sunglasses
suntan cream
water
waterproof overclothes
waterproof trousers

```



## VBA


```vb
'Knapsack problem/0-1 - 12/02/2017
Option Explicit
Const maxWeight = 400
Dim DataList As Variant
Dim xList(64, 3) As Variant
Dim nItems As Integer
Dim s As String, xss As String
Dim xwei As Integer, xval As Integer, nn As Integer

Sub Main()
    Dim i As Integer, j As Integer
    DataList = Array("map", 9, 150, "compass", 13, 35, "water", 153, 200, "sandwich", 50, 160, _
           "glucose", 15, 60, "tin", 68, 45, "banana", 27, 60, "apple", 39, 40, _
           "cheese", 23, 30, "beer", 52, 10, "suntan cream", 11, 70, "camera", 32, 30, _
           "T-shirt", 24, 15, "trousers", 48, 10, "umbrella", 73, 40, "book", 30, 10, _
           "waterproof trousers", 42, 70, "waterproof overclothes", 43, 75, _
           "note-case", 22, 80, "sunglasses", 7, 20, "towel", 18, 12, "socks", 4, 50)
    nItems = (UBound(DataList) + 1) / 3
    j = 0
    For i = 1 To nItems
        xList(i, 1) = DataList(j)
        xList(i, 2) = DataList(j + 1)
        xList(i, 3) = DataList(j + 2)
        j = j + 3
    Next i
    s = ""
    For i = 1 To nItems
        s = s & Chr(i)
    Next
    nn = 0
    Call ChoiceBin(1, "")
    For i = 1 To Len(xss)
        j = Asc(Mid(xss, i, 1))
        Debug.Print xList(j, 1)
    Next i
    Debug.Print "count=" & Len(xss), "weight=" & xwei, "value=" & xval
End Sub 'Main

Private Sub ChoiceBin(n As String, ss As String)
    Dim r As String
    Dim i As Integer, j As Integer, iwei As Integer, ival As Integer
    Dim ipct As Integer
    If n = Len(s) + 1 Then
        iwei = 0: ival = 0
        For i = 1 To Len(ss)
            j = Asc(Mid(ss, i, 1))
            iwei = iwei + xList(j, 2)
            ival = ival + xList(j, 3)
        Next
        If iwei <= maxWeight And ival > xval Then
            xss = ss: xwei = iwei: xval = ival
        End If
    Else
        r = Mid(s, n, 1)
        Call ChoiceBin(n + 1, ss & r)
        Call ChoiceBin(n + 1, ss)
    End If
End Sub 'ChoiceBin
```

{{out}}

```txt

map
compass
water
sandwich
glucose
banana
suntan cream
waterproof trousers
waterproof overclothes
note-case
sunglasses
socks
count=12      weight=396    value=1030

```



## VBScript

Non recurvive unfolded force version. Created by an other script. It runs 13 times faster than the recursive one.

```vb
' Knapsack problem/0-1 - 13/02/2017
dim w(22),v(22),m(22)
data=array( "map", 9, 150, "compass", 13, 35, "water", 153, 200, _
 "sandwich", 50, 160 , "glucose", 15, 60, "tin", 68, 45, _
 "banana", 27, 60, "apple", 39, 40 , "cheese", 23, 30, "beer", 52, 10, _
 "suntan cream", 11, 70, "camera", 32, 30 , "T-shirt", 24, 15, _
 "trousers", 48, 10, "umbrella", 73, 40, "book", 30, 10 , _
 "waterproof trousers", 42, 70, "waterproof overclothes", 43, 75 , _
 "note-case", 22, 80, "sunglasses", 7, 20, "towel", 18, 12, "socks", 4, 50)
ww=400
xw=0:iw=0:iv=0
w(1)=iw:v(1)=iv
for i1=0 to 1:m(1)=i1:j=0
 if i1=1 then
  iw=w(1)+data(j*3+1):iv=v(1)+data(j*3+2)
  if iv>xv and iw<=ww then xw=iw:xv=iv:l=m
 end if 'i1
 if iw<=ww then
  w(2)=iw: v(2)=iv
  for i2=0 to 1:m(2)=i2:j=1
   if i2=1 then
    iw=w(2)+data(j*3+1):iv=v(2)+data(j*3+2)
    if iv>xv and iw<=ww then xw=iw:xv=iv:l=m
   end if 'i2
   if iw<=ww then
    w(3)=iw: v(3)=iv
    for i3=0 to 1:m(3)=i3:j=2
     if i3=1 then
      iw=w(3)+data(j*3+1):iv=v(3)+data(j*3+2)
      if iv>xv and iw<=ww then xw=iw:xv=iv:l=m
     end if 'i3
     if iw<=ww then
      w(4)=iw: v(4)=iv
      for i4=0 to 1:m(4)=i4:j=3
       if i4=1 then
        iw=w(4)+data(j*3+1):iv=v(4)+data(j*3+2)
        if iv>xv and iw<=ww then xw=iw:xv=iv:l=m
       end if 'i4
       if iw<=ww then
        w(5)=iw: v(5)=iv
        for i5=0 to 1:m(5)=i5:j=4
         if i5=1 then
          iw=w(5)+data(j*3+1):iv=v(5)+data(j*3+2)
          if iv>xv and iw<=ww then xw=iw:xv=iv:l=m
         end if 'i5
         if iw<=ww then
          w(6)=iw: v(6)=iv
          for i6=0 to 1:m(6)=i6:j=5
           if i6=1 then
            iw=w(6)+data(j*3+1):iv=v(6)+data(j*3+2)
            if iv>xv and iw<=ww then xw=iw:xv=iv:l=m
           end if 'i6
           if iw<=ww then
            w(7)=iw: v(7)=iv
            for i7=0 to 1:m(7)=i7:j=6
             if i7=1 then
              iw=w(7)+data(j*3+1):iv=v(7)+data(j*3+2)
              if iv>xv and iw<=ww then xw=iw:xv=iv:l=m
             end if 'i7
             if iw<=ww then
              w(8)=iw: v(8)=iv
              for i8=0 to 1:m(8)=i8:j=7
               if i8=1 then
                iw=w(8)+data(j*3+1):iv=v(8)+data(j*3+2)
                if iv>xv and iw<=ww then xw=iw:xv=iv:l=m
               end if 'i8
               if iw<=ww then
                w(9)=iw: v(9)=iv
                for i9=0 to 1:m(9)=i9:j=8
                 if i9=1 then
                  iw=w(9)+data(j*3+1):iv=v(9)+data(j*3+2)
                  if iv>xv and iw<=ww then xw=iw:xv=iv:l=m
                 end if 'i9
                 if iw<=ww then
                  w(10)=iw: v(10)=iv
                  for i10=0 to 1:m(10)=i10:j=9
                   if i10=1 then
                    iw=w(10)+data(j*3+1):iv=v(10)+data(j*3+2)
                    if iv>xv and iw<=ww then xw=iw:xv=iv:l=m
                   end if 'i10
                   if iw<=ww then
                    w(11)=iw: v(11)=iv
                    for i11=0 to 1:m(11)=i11:j=10
                     if i11=1 then
                      iw=w(11)+data(j*3+1):iv=v(11)+data(j*3+2)
                      if iv>xv and iw<=ww then xw=iw:xv=iv:l=m
                     end if 'i11
                     if iw<=ww then
                      w(12)=iw: v(12)=iv
                      for i12=0 to 1:m(12)=i12:j=11
                       if i12=1 then
                        iw=w(12)+data(j*3+1):iv=v(12)+data(j*3+2)
                        if iv>xv and iw<=ww then xw=iw:xv=iv:l=m
                       end if 'i12
                       if iw<=ww then
                        w(13)=iw: v(13)=iv
                        for i13=0 to 1:m(13)=i13:j=12
                         if i13=1 then
                          iw=w(13)+data(j*3+1):iv=v(13)+data(j*3+2)
                          if iv>xv and iw<=ww then xw=iw:xv=iv:l=m
                         end if 'i13
                         if iw<=ww then
                          w(14)=iw: v(14)=iv
                          for i14=0 to 1:m(14)=i14:j=13
                           if i14=1 then
                            iw=w(14)+data(j*3+1):iv=v(14)+data(j*3+2)
                            if iv>xv and iw<=ww then xw=iw:xv=iv:l=m
                           end if 'i14
                           if iw<=ww then
                            w(15)=iw: v(15)=iv
                            for i15=0 to 1:m(15)=i15:j=14
                             if i15=1 then
                              iw=w(15)+data(j*3+1):iv=v(15)+data(j*3+2)
                              if iv>xv and iw<=ww then xw=iw:xv=iv:l=m
                             end if 'i15
                             if iw<=ww then
                              w(16)=iw: v(16)=iv
                              for i16=0 to 1:m(16)=i16:j=15
                               if i16=1 then
                                iw=w(16)+data(j*3+1):iv=v(16)+data(j*3+2)
                                if iv>xv and iw<=ww then xw=iw:xv=iv:l=m
                               end if 'i16
                               if iw<=ww then
                                w(17)=iw: v(17)=iv
                                for i17=0 to 1:m(17)=i17:j=16
                                 if i17=1 then
                                  iw=w(17)+data(j*3+1):iv=v(17)+data(j*3+2)
                                  if iv>xv and iw<=ww then xw=iw:xv=iv:l=m
                                 end if 'i17
                                 if iw<=ww then
                                  w(18)=iw: v(18)=iv
                                  for i18=0 to 1:m(18)=i18:j=17
                                   if i18=1 then
                                    iw=w(18)+data(j*3+1):iv=v(18)+data(j*3+2)
                                    if iv>xv and iw<=ww then xw=iw:xv=iv:l=m
                                   end if 'i18
                                   if iw<=ww then
                                    w(19)=iw: v(19)=iv
                                    for i19=0 to 1:m(19)=i19:j=18
                                     if i19=1 then
                                      iw=w(19)+data(j*3+1):iv=v(19)+data(j*3+2)
                                      if iv>xv and iw<=ww then xw=iw:xv=iv:l=m
                                     end if 'i19
                                     if iw<=ww then
                                      w(20)=iw: v(20)=iv
                                      for i20=0 to 1:m(20)=i20:j=19
                                       if i20=1 then
                                        iw=w(20)+data(j*3+1):iv=v(20)+data(j*3+2)
                                        if iv>xv and iw<=ww then xw=iw:xv=iv:l=m
                                       end if 'i20
                                       if iw<=ww then
                                        w(21)=iw: v(21)=iv
                                        for i21=0 to 1:m(21)=i21:j=20
                                         if i21=1 then
                                          iw=w(21)+data(j*3+1):iv=v(21)+data(j*3+2)
                                          if iv>xv and iw<=ww then xw=iw:xv=iv:l=m
                                         end if 'i21
                                         if iw<=ww then
                                          w(22)=iw: v(22)=iv
                                          for i22=0 to 1:m(22)=i22:j=21
                                           nn=nn+1
                                           if i22=1 then
                                            iw=w(22)+data(j*3+1):iv=v(22)+data(j*3+2)
                                            if iv>xv and iw<=ww then xw=iw:xv=iv:l=m
                                           end if 'i22
                                           if iw<=ww then
                                           end if 'i22
                                          next:m(22)=0
                                         end if 'i21
                                        next:m(21)=0
                                       end if 'i20
                                      next:m(20)=0
                                     end if 'i19
                                    next:m(19)=0
                                   end if 'i18
                                  next:m(18)=0
                                 end if 'i17
                                next:m(17)=0
                               end if 'i16
                              next:m(16)=0
                             end if 'i15
                            next:m(15)=0
                           end if 'i14
                          next:m(14)=0
                         end if 'i13
                        next:m(13)=0
                       end if 'i12
                      next:m(12)=0
                     end if 'i11
                    next:m(11)=0
                   end if 'i10
                  next:m(10)=0
                 end if 'i9
                next:m(9)=0
               end if 'i8
              next:m(8)=0
             end if 'i7
            next:m(7)=0
           end if 'i6
          next:m(6)=0
         end if 'i5
        next:m(5)=0
       end if 'i4
      next:m(4)=0
     end if 'i3
    next:m(3)=0
   end if 'i2
  next:m(2)=0
 end if 'i1
next:m(1)=0
for i=1 to 22
 if l(i)=1 then wlist=wlist&vbCrlf&data((i-1)*3)
next
Msgbox mid(wlist,3)&vbCrlf&vbCrlf&"weight="&xw&vbCrlf&"value="&xv,,"Knapsack - nn="&nn
```

{{out}}

```txt

map
compass
water
sandwich
glucose
banana
suntan cream
waterproof trousers
waterproof overclothes
note-case
sunglasses
socks

weight=396
value=1030

```



## Visual Basic

{{works with|Visual Basic|VB6 Standard}}

```vb
'Knapsack problem/0-1 - 12/02/2017
Option Explicit
Const maxWeight = 400
Dim DataList As Variant
Dim xList(64, 3) As Variant
Dim nItems As Integer
Dim s  As String, xss As String
Dim xwei As Integer, xval As Integer, nn As Integer

Private Sub Form_Load()
    Dim i As Integer, j As Integer
    DataList = Array("map", 9, 150, "compass", 13, 35, "water", 153, 200, "sandwich", 50, 160, _
           "glucose", 15, 60, "tin", 68, 45, "banana", 27, 60, "apple", 39, 40, _
           "cheese", 23, 30, "beer", 52, 10, "suntan cream", 11, 70, "camera", 32, 30, _
           "T-shirt", 24, 15, "trousers", 48, 10, "umbrella", 73, 40, "book", 30, 10, _
           "waterproof trousers", 42, 70, "waterproof overclothes", 43, 75, _
           "note-case", 22, 80, "sunglasses", 7, 20, "towel", 18, 12, "socks", 4, 50)
    nItems = (UBound(DataList) + 1) / 3
    j = 0
    For i = 1 To nItems
        xList(i, 1) = DataList(j)
        xList(i, 2) = DataList(j + 1)
        xList(i, 3) = DataList(j + 2)
        j = j + 3
    Next i
    For i = 1 To nItems
        xListBox.AddItem xList(i, 1)
    Next i
End Sub

Private Sub cmdOK_Click()
    Dim i As Integer, j As Integer
    For i = 1 To xListBox.ListCount
        xListBox.RemoveItem 0
    Next i
    s = ""
    For i = 1 To nItems
        s = s & Chr(i)
    Next
    nn = 0
    Call ChoiceBin(1, "")
    For i = 1 To Len(xss)
        j = Asc(Mid(xss, i, 1))
        xListBox.AddItem xList(j, 1)
    Next i
    xListBox.AddItem "*Total* " & xwei & " " & xval
End Sub

Private Sub ChoiceBin(n As String, ss As String)
    Dim r As String
    Dim i As Integer, j As Integer, iwei As Integer, ival As Integer
    Dim ipct As Integer
    If n = Len(s) + 1 Then
        iwei = 0: ival = 0
        For i = 1 To Len(ss)
            j = Asc(Mid(ss, i, 1))
            iwei = iwei + xList(j, 2)
            ival = ival + xList(j, 3)
        Next
        If iwei <= maxWeight And ival > xval Then
            xss = ss: xwei = iwei: xval = ival
        End If
    Else
        r = Mid(s, n, 1)
        Call ChoiceBin(n + 1, ss & r)
        Call ChoiceBin(n + 1, ss)
    End If
End Sub 'ChoiceBin
```

{{out}}

```txt

map
compass
water
sandwich
glucose
banana
suntan cream
waterproof trousers
waterproof overclothes
note-case
sunglasses
socks
*Total*  weight=396  val=1030

```



## Visual Basic .NET

{{works with|Visual Basic .NET|2013}}

```vbnet
'Knapsack problem/0-1 - 12/02/2017
Public Class KnapsackBin
    Const knam = 0, kwei = 1, kval = 2
    Const maxWeight = 400
    Dim xList(,) As Object = { _
            {"map", 9, 150}, _
            {"compass", 13, 35}, _
            {"water", 153, 200}, _
            {"sandwich", 50, 160}, _
            {"glucose", 15, 60}, _
            {"tin", 68, 45}, _
            {"banana", 27, 60}, _
            {"ChoiceBinle", 39, 40}, _
            {"cheese", 23, 30}, _
            {"beer", 52, 10}, _
            {"suntan cream", 11, 70}, _
            {"camera", 32, 30}, _
            {"T-shirt", 24, 15}, _
            {"trousers", 48, 10}, _
            {"umbrella", 73, 40}, _
            {"waterproof trousers", 42, 70}, _
            {"waterproof overclothes", 43, 75}, _
            {"note-case", 22, 80}, _
            {"sunglasses", 7, 20}, _
            {"towel", 18, 12}, _
            {"socks", 4, 50}, _
            {"book", 30, 10}}
    Dim s, xss As String, xwei, xval, nn As Integer

    Private Sub KnapsackBin_Load(sender As Object, e As EventArgs) Handles MyBase.Load
        Dim i As Integer
        xListView.View = View.Details
        xListView.Columns.Add("item", 120, HorizontalAlignment.Left)
        xListView.Columns.Add("weight", 50, HorizontalAlignment.Right)
        xListView.Columns.Add("value", 50, HorizontalAlignment.Right)
        For i = 0 To UBound(xList, 1)
            xListView.Items.Add(New ListViewItem(New String() {xList(i, 0), _
                                xList(i, 1).ToString, xList(i, 2).ToString}))
        Next i
    End Sub 'KnapsackBin_Load

    Private Sub cmdOK_Click(sender As Object, e As EventArgs) Handles cmdOK.Click
        Dim i, j, nItems As Integer
        For i = xListView.Items.Count - 1 To 0 Step -1
            xListView.Items.RemoveAt(i)
        Next i
        Me.Refresh()
        nItems = UBound(xList, 1) + 1
        s = ""
        For i = 1 To nItems
            s = s & Chr(i - 1)
        Next
        nn = 0
        Call ChoiceBin(1, "")
        For i = 1 To Len(xss)
            j = Asc(Mid(xss, i, 1))
            xListView.Items.Add(New ListViewItem(New String() {xList(j, 0), _
                                xList(j, 1).ToString, xList(j, 2).ToString}))
        Next i
        xListView.Items.Add(New ListViewItem(New String() {"*Total*", xwei, xval}))
    End Sub 'cmdOK_Click

    Private Sub ChoiceBin(n As String, ss As String)
        Dim r As String, i, j, iwei, ival As Integer
        Dim ipct As Integer
        If n = Len(s) + 1 Then
            iwei = 0 : ival = 0
            For i = 1 To Len(ss)
                j = Asc(Mid(ss, i, 1))
                iwei = iwei + xList(j, 1)
                ival = ival + xList(j, 2)
            Next
            If iwei <= maxWeight And ival > xval Then
                xss = ss : xwei = iwei : xval = ival
            End If
        Else
            r = Mid(s, n, 1)
            Call ChoiceBin(n + 1, ss & r)
            Call ChoiceBin(n + 1, ss)
        End If
    End Sub 'ChoiceBin

End Class 'KnapsackBin

```

{{out}}

```txt

KnapsackBin_Load
cmdOK_Click
map
compass
water
sandwich
glucose
banana
suntan cream
waterproof trousers
waterproof overclothes
note-case
sunglasses
socks
*Total*  weight=396  val=1030

```



## XPL0


```XPL0
include c:\cxpl\codes;  \include 'code' declarations

int     Item, Items, Weights, Values,
        BestItems, BestValues,
        I, W, V, N;
def     Tab=9;
def     Name,                   Weight, Value;
[Item:= [["map                  ",   9, 150],
        ["compass               ",  13,  35],
        ["water                 ", 153, 200],
        ["sandwich              ",  50, 160],
        ["glucose               ",  15,  60],
        ["tin                   ",  68,  45],
        ["banana                ",  27,  60],
        ["apple                 ",  39,  40],
        ["cheese                ",  23,  30],
        ["beer                  ",  52,  10],
        ["suntan cream          ",  11,  70],
        ["camera                ",  32,  30],
        ["T-shirt               ",  24,  15],
        ["trousers              ",  48,  10],
        ["umbrella              ",  73,  40],
        ["waterproof trousers   ",  42,  70],
        ["waterproof overclothes",  43,  75],
        ["note-case             ",  22,  80],
        ["sunglasses            ",   7,  20],
        ["towel                 ",  18,  12],
        ["socks                 ",   4,  50],
        ["book                  ",  30,  10]];

BestValues:= 0;
for Items:= 0 to 1<<22-1 do     \for all possible combinations of Items...
        [I:= Items;  W:= 0;  V:= 0;  N:= 0;
        while I do              \add weights and values for each item (bit in I)
                [if I&1 then
                        [W:= W + Item(N,Weight);  V:= V + Item(N,Value)];
                I:= I>>1;  N:= N+1;
                ];
        if V>BestValues & W<=400 then   \save best combination found so far
                [BestValues:= V;  BestItems:= Items];
        ];

I:= BestItems;  W:= 0;  V:= 0;  N:= 0;          \show best combination of items
while I do
        [if I&1 then
                [Text(0, "  ");  Text(0, Item(N,Name));  ChOut(0, Tab);
                IntOut(0, Item(N,Weight));  ChOut(0, Tab);
                IntOut(0, Item(N,Value));  CrLf(0);
                W:= W + Item(N,Weight);
                V:= V + Item(N,Value);
                ];
        I:= I>>1;  N:= N+1;
        ];
Text(0, "Totals:                                ");
IntOut(0, W);  ChOut(0, Tab);
IntOut(0, V);  CrLf(0);
]
```


{{out}}

```txt

  map                           9       150
  compass                       13      35
  water                         153     200
  sandwich                      50      160
  glucose                       15      60
  banana                        27      60
  suntan cream                  11      70
  waterproof trousers           42      70
  waterproof overclothes        43      75
  note-case                     22      80
  sunglasses                    7       20
  socks                         4       50
Totals:                         396     1030

```



## zkl

{{trans|Haskell}}{{trans|D}}

```zkl
fcn addItem(pairs,it){   // pairs is list of (cost of:,names), it is (name,w,v)
   w,left,right:=it[1],pairs[0,w],pairs[w,*];
   left.extend(right.zipWith(
      fcn([(t1,_)]a,[(t2,_)]b){ t1>t2 and a or b  },
      pairs.apply('wrap([(tot,names)]){ T(tot + it[2], names + it[0]) })))
}//--> new list of pairs
```


```zkl
items:=T(T("apple",     39, 40),T("banana",   27,60), // item: (name,weight,value)
        T("beer",       52, 10),T("book",     30,10),T("camera",      32, 30),
	T("cheese",     23, 30),T("compass",  13,35),T("glucose",     15, 60),
	T("map",         9,150),T("note-case",22,80),T("sandwich",    50,160),
	T("socks",       4, 50),T("sunglasses",7,20),T("suntan cream",11, 70),
	T("t-shirt",    24, 15),T("tin",      68,45),T("towel",       18, 12),
	T("trousers",   48, 10),T("umbrella", 73,40),T("water",      153,200),
	T("overclothes",43, 75),T("waterproof trousers",42,70) );
const MAX_WEIGHT=400;
knapsack:=items.reduce(addItem,
   (MAX_WEIGHT).pump(List,T(0,T).copy))[-1];  // nearest to max weight
weight:=items.apply('wrap(it){ knapsack[1].holds(it[0]) and it[1] }).sum(0);
knapsack.println(weight);
```

{{out}}

```txt

L(1030,L("banana","compass","glucose","map","note-case","sandwich","socks","sunglasses","suntan cream","water","overclothes","waterproof trousers"))396

```

