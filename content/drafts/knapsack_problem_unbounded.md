+++
title = "Knapsack problem/Unbounded"
description = ""
date = 2019-05-17T03:23:43Z
aliases = []
[extra]
id = 3196
[taxonomies]
categories = []
tags = []
+++

{{task|Classic CS problems and programs}}

A traveler gets diverted and has to make an unscheduled stop in what turns out to be Shangri La.   Opting to leave, he is allowed to take as much as he likes of the following items, so long as it will fit in his knapsack, and he can carry it.

He knows that he can carry no more than   25   'weights' in total;   and that the capacity of his knapsack is   0.25   'cubic lengths'.

Looking just above the bar codes on the items he finds their weights and volumes.   He digs out his recent copy of a  financial paper and gets the value of each item.
<table
 style="text-align: left; width: 80%;" border="4"
 cellpadding="2" cellspacing="2"><tr><td
 style="font-weight: bold;" align="left" nowrap="nowrap"
 valign="middle">Item</td><td
 style="font-weight: bold;" align="left" nowrap="nowrap"
 valign="middle">Explanation</td><td
 style="font-weight: bold;" align="left" nowrap="nowrap"
 valign="middle">Value (each)</td><td
 style="font-weight: bold;" align="left" nowrap="nowrap"
 valign="middle">weight</td><td
 style="font-weight: bold;" align="left" nowrap="nowrap"
 valign="middle">Volume (each)</td></tr><tr><td
 align="left" nowrap="nowrap" valign="middle">panacea
(vials of)</td><td align="left" nowrap="nowrap"
 valign="middle">Incredible healing properties</td><td
 align="left" nowrap="nowrap" valign="middle">3000</td><td
 align="left" nowrap="nowrap" valign="middle">0.3</td><td
 align="left" nowrap="nowrap" valign="middle">0.025</td></tr><tr><td
 align="left" nowrap="nowrap" valign="middle">ichor
(ampules of)</td><td align="left" nowrap="nowrap"
 valign="middle">Vampires blood</td><td align="left"
 nowrap="nowrap" valign="middle">1800</td><td
 align="left" nowrap="nowrap" valign="middle">0.2</td><td
 align="left" nowrap="nowrap" valign="middle">0.015</td></tr><tr><td
 align="left" nowrap="nowrap" valign="middle">gold
(bars)</td><td align="left" nowrap="nowrap"
 valign="middle">Shiney shiney</td><td align="left"
 nowrap="nowrap" valign="middle">2500</td><td
 align="left" nowrap="nowrap" valign="middle">2.0</td><td
 align="left" nowrap="nowrap" valign="middle">0.002</td></tr><tr><td
 style="background-color: rgb(255, 204, 255);" align="left"
 nowrap="nowrap" valign="middle">Knapsack</td><td
 style="background-color: rgb(255, 204, 255);" align="left"
 nowrap="nowrap" valign="middle">For the carrying of</td><td
 style="background-color: rgb(255, 204, 255);" align="left"
 nowrap="nowrap" valign="middle">-</td><td
 style="background-color: rgb(255, 204, 255);" align="left"
 nowrap="nowrap" valign="middle">&lt;=25</td><td
 style="background-color: rgb(255, 204, 255);" align="left"
 nowrap="nowrap" valign="middle">&lt;=0.25 </td></tr>
</table>



He can only take whole units of any item, but there is much more of any item than he could ever carry


;Task:
Show how many of each item does he take to maximize the value of items he is carrying away with him.


;Note:
*   There are four solutions that maximize the value taken.   Only one ''need'' be given.


<!-- All solutions

# ((value, -weight, -volume), (#panacea, #ichor, #gold)
[((54500, -25.0, -0.24699999999999997), (0, 15, 11)),
 ((54500, -24.899999999999999, -0.247), (3, 10, 11)),
 ((54500, -24.800000000000001, -0.24700000000000003), (6, 5, 11)),
 ((54500, -24.699999999999999, -0.247), (9, 0, 11))]

# (9, 0, 11) also minimizes weight and volume within the limits of calculation
-->

;Related tasks:
*   [[Knapsack problem/Bounded]]
*   [[Knapsack problem/Continuous]]
*   [[Knapsack problem/0-1]]





## 360 Assembly

{{trans|Visual Basic}}
The program uses two ASSIST macros (XDECO,XPRNT) to keep the code as short as possible.

```360asm
*        Knapsack problem/Unbounded   04/02/2017
KNAPSACK CSECT
         USING  KNAPSACK,R13       base register
         B      72(R15)            skip savearea
         DC     17F'0'             savearea
         STM    R14,R12,12(R13)    prolog
         ST     R13,4(R15)         " <-
         ST     R15,8(R13)         " ->
         LR     R13,R15            " addressability
         MVC    S,=F'0'            s(1,kva)=0;
         LA     R11,0              ns=0
         LA     R1,KW              kw
         SLA    R1,2               *4
         L      R2,PANACEA-4(R1)   panacea(kw)
         L      R4,SACKW           sackw
         SRDA   R4,32              ~
         DR     R4,R2              sackw/panacea(kw)
         ST     R5,XP              xp=sackw/panacea(kw)
         LA     R1,KV              kv
         SLA    R1,2               *4
         L      R2,PANACEA-4(R1)   panacea(kv)
         L      R4,SACKV           sackv
         SRDA   R4,32              ~
         DR     R4,R2              r5=sackv/panacea(kv)
         C      R5,XP              if r5<xp
         BNL    EMINXP
         ST     R5,XP     xp=min(sackw/panacea(kw),sackv/panacea(kv))
EMINXP   LA     R1,KW              kw
         SLA    R1,2               *4
         L      R2,ICHOR-4(R1)     ichor(kw)
         L      R4,SACKW           sackw
         SRDA   R4,32              ~
         DR     R4,R2              sackw/ichor(kw)
         ST     R5,XI              xi=sackw/ichor(kw)
         LA     R1,KV              kv
         SLA    R1,2               *4
         L      R2,ICHOR-4(R1)     ichor(kv)
         L      R4,SACKV           sackv
         SRDA   R4,32              ~
         DR     R4,R2              r5=sackv/ichor(kv)
         C      R5,XI              if r5<xi
         BNL    EMINXI
         ST     R5,XI     xi=min(sackw/ichor(kw),sackv/ichor(kv))
EMINXI   LA     R1,KW              kw
         SLA    R1,2               *4
         L      R2,GOLD-4(R1)      gold(kw)
         L      R4,SACKW           sackw
         SRDA   R4,32              ~
         DR     R4,R2              sackw/gold(kw)
         ST     R5,XG              xg=sackw/gold(kw)
         LA     R1,KV              kv
         SLA    R1,2               *4
         L      R2,GOLD-4(R1)      gold(kv)
         L      R4,SACKV           sackv
         SRDA   R4,32              ~
         DR     R4,R2              r5=sackv/gold(kv)
         C      R5,XG              if r5<xg
         BNL    EMINXG
         ST     R5,XG     xg=min(sackw/gold(kw),sackv/gold(kv))
EMINXG   SR     R10,R10            ip=0
LOOPIP   C      R10,XP             do ip=0 to xp
         BH     ELOOPIP
         SR     R9,R9              ii=0
LOOPII   C      R9,XI              do ii=0 to xi
         BH     ELOOPII
         SR     R8,R8              ig=0
LOOPIG   C      R8,XG              do ig=0 to xg
         BH     ELOOPIG
         LA     R7,KVA             m=kva
LOOPM    C      R7,=A(KV)          do m=kva to kv
         BH     ELOOPM
         LR     R1,R7              m
         SLA    R1,2               *4
         LR     R5,R8              ig
         M      R4,GOLD-4(R1)      *gold(m)
         LR     R2,R5              r2=ig*gold(m)
         LR     R5,R9              ii
         M      R4,ICHOR-4(R1)     *ichor(m)
         AR     R2,R5              r2=ig*gold(m)+ii*ichor(m)
         LR     R5,R10             ip
         M      R4,PANACEA-4(R1)   *panacea(m)
         AR     R2,R5              r2=r2+ip*panacea(m)
         ST     R2,CUR-4(R1)       cur(m)=r2
         LA     R7,1(R7)           m=m+1
         B      LOOPM
ELOOPM   LA     R1,KVA             kva
         SLA    R1,2               *4
         L      R2,CUR-4(R1)       cur(kva)
         C      R2,S-4(R1)         if cur(kva)>=s(1,kva)
         BL     ENDIF
         LA     R1,KW              kw
         SLA    R1,2               *4
         L      R2,CUR-4(R1)       cur(kw)
         C      R2,SACKW           if cur(kw)<=sackw
         BH     ENDIF
         LA     R1,KV              kv
         SLA    R1,2               *4
         L      R2,CUR-4(R1)       cur(kv)
         C      R2,SACKV           if cur(kv)<=sackv
         BH     ENDIF
         LR     R6,R11             j=ns
LOOPJ    C      R6,=F'1'           do j=ns to 1 by -1
         BL     ELOOPJ
         LR     R1,R6              j
         MH     R1,=H'24'          *24
         LA     R2,S(R1)           s(j+1,1)
         LA     R3,S-24(R1)        s(j,1)
         MVC    0(24,R2),0(R3)     s(j+1,*)=s(j,*)
         BCTR   R6,0               j=j-1
         B      LOOPJ
ELOOPJ   LA     R1,KVA             kva
         SLA    R1,2               *4
         L      R2,CUR-4(R1)       cur(kva)
         ST     R2,S-4(R1)         s(1,kva)=cur(kva)
         LA     R1,KW              kw
         SLA    R1,2               *4
         L      R2,CUR-4(R1)       cur(kw)
         ST     R2,S-4(R1)         s(1,kw)=cur(kw)
         LA     R1,KV              kv
         SLA    R1,2               *4
         L      R2,CUR-4(R1)       cur(kv)
         ST     R2,S-4(R1)         s(1,kv)=cur(kv)
         LA     R1,KP              kp
         SLA    R1,2               *4
         ST     R10,S-4(R1)        s(1,kp)=ip
         LA     R1,KI              ki
         SLA    R1,2               *4
         ST     R9,S-4(R1)         s(1,ki)=ii
         LA     R1,KG              kg
         SLA    R1,2               *4
         ST     R8,S-4(R1)         s(1,kg)=ig
         L      R2,S               r2=s(1,1)
         C      R2,S+24            if s(1,1)>s(2,1)
         BNH    ELSE
         LA     R11,1              ns=1
         B      ENDIF
ELSE     LA     R11,1(R11)         ns+1
ENDIF    LA     R8,1(R8)           ig=ig+1
         B      LOOPIG
ELOOPIG  LA     R9,1(R9)           ii=ii+1
         B      LOOPII
ELOOPII  LA     R10,1(R10)         ip=ip+1
         B      LOOPIP
ELOOPIP  XPRNT  TITLE,72
         LA     R6,1               j=1
         LA     R3,S-4             r3=@item
LOOPJP   CR     R6,R11             do j=1 to ns
         BH     ELOOPJP
         LA     R3,4(R3)           ++
         L      R1,0(R3)           s(j,kva)
         XDECO  R1,PG              edit
         LA     R3,4(R3)           ++
         L      R1,0(R3)           s(j,kw)
         XDECO  R1,PG+12           edit
         LA     R3,4(R3)           ++
         L      R1,0(R3)           s(j,kv)
         XDECO  R1,PG+24           edit
         MVC    PG+20(2),PG+21     shift
         MVI    PG+22,C'.'         decimal point
         LA     R3,4(R3)           ++
         L      R1,0(R3)           s(j,kp)
         XDECO  R1,PG+36           edit
         MVC    PG+31(2),=C'0.'    decimal point
         LA     R3,4(R3)           ++
         L      R1,0(R3)           s(j,ki)
         XDECO  R1,PG+48           edit
         LA     R3,4(R3)           ++
         L      R1,0(R3)           s(j,kg)
         XDECO  R1,PG+60           edit
         XPRNT  PG,L'PG            print buffer
         LA     R6,1(R6)           j=j+1
         B      LOOPJP
ELOOPJP  L      R13,4(0,R13)       epilog
         LM     R14,R12,12(R13)    " restore
         XR     R15,R15            " rc=0
         BR     R14                exit
KVA      EQU    1
KW       EQU    2
KV       EQU    3
KP       EQU    4
KI       EQU    5
KG       EQU    6
SACKW    DC     F'250'
SACKV    DC     F'250'
PANACEA  DC     F'3000',F'3',F'25'
ICHOR    DC     F'1800',F'2',F'15'
GOLD     DC     F'2500',F'20',F'2'
XP       DS     F
XI       DS     F
XG       DS     F
CUR      DS     3F
S        DS     60F
TITLE    DC     CL36'       Value      Weight      Volume'
         DC     CL36'     Panacea       Ichor        Gold'
PG       DS     CL72
         YREGS
         END    KNAPSACK
```

{{out}}

```txt

       Value      Weight      Volume     Panacea       Ichor        Gold
       54500        24.7       0.247           9           0          11
       54500        24.8       0.247           6           5          11
       54500        24.9       0.247           3          10          11
       54500        25.0       0.247           0          15          11

```



## Ada

{{trans|Python}}

```Ada
with Ada.Text_IO;

procedure Knapsack_Unbounded is

   type Bounty is record
      Value  : Natural;
      Weight : Float;
      Volume : Float;
   end record;

   function Min (A, B : Float) return Float is
   begin
      if A < B then
         return A;
      else
         return B;
      end if;
   end Min;

   Panacea : Bounty := (3000,  0.3, 0.025);
   Ichor   : Bounty := (1800,  0.2, 0.015);
   Gold    : Bounty := (2500,  2.0, 0.002);
   Limits  : Bounty := (   0, 25.0, 0.250);
   Best    : Bounty := (   0,  0.0, 0.000);
   Current : Bounty := (   0,  0.0, 0.000);

   Best_Amounts : array (1 .. 3) of Natural := (0, 0, 0);

   Max_Panacea : Natural := Natural (Float'Floor (Min
                              (Limits.Weight / Panacea.Weight,
                               Limits.Volume / Panacea.Volume)));
   Max_Ichor   : Natural := Natural (Float'Floor (Min
                              (Limits.Weight / Ichor.Weight,
                               Limits.Volume / Ichor.Volume)));
   Max_Gold    : Natural := Natural (Float'Floor (Min
                              (Limits.Weight / Gold.Weight,
                               Limits.Volume / Gold.Volume)));

begin
   for Panacea_Count in 0 .. Max_Panacea loop
      for Ichor_Count in 0 .. Max_Ichor loop
         for Gold_Count in 0 .. Max_Gold loop
            Current.Value  := Panacea_Count * Panacea.Value +
                              Ichor_Count * Ichor.Value +
                              Gold_Count * Gold.Value;
            Current.Weight := Float (Panacea_Count) * Panacea.Weight +
                              Float (Ichor_Count) * Ichor.Weight +
                              Float (Gold_Count) * Gold.Weight;
            Current.Volume := Float (Panacea_Count) * Panacea.Volume +
                              Float (Ichor_Count) * Ichor.Volume +
                              Float (Gold_Count) * Gold.Volume;
            if Current.Value  >  Best.Value and
               Current.Weight <= Limits.Weight and
               Current.Volume <= Limits.Volume then
               Best := Current;
               Best_Amounts := (Panacea_Count, Ichor_Count, Gold_Count);
            end if;
         end loop;
      end loop;
   end loop;
   Ada.Text_IO.Put_Line ("Maximum value:" & Natural'Image (Best.Value));
   Ada.Text_IO.Put_Line ("Panacea:" & Natural'Image (Best_Amounts (1)));
   Ada.Text_IO.Put_Line ("Ichor:  " & Natural'Image (Best_Amounts (2)));
   Ada.Text_IO.Put_Line ("Gold:   " & Natural'Image (Best_Amounts (3)));
end Knapsack_Unbounded;
```



## ALGOL 68

{{trans|Python}}
```algol68
MODE BOUNTY = STRUCT(STRING name, INT value, weight, volume);

[]BOUNTY items = (
               ("panacea", 3000,   3,  25),
               ("ichor",   1800,   2,  15),
               ("gold",    2500,  20,   2)
      );

BOUNTY sack := ("sack",       0, 250, 250);

OP * = ([]INT a,b)INT: ( # dot product operator #
    INT sum := 0;
    FOR i TO UPB a DO sum +:= a[i]*b[i] OD;
    sum
);

OP INIT = (REF[]INT vector)VOID:
    FOR index FROM LWB vector TO UPB vector DO
        vector[index]:=0
    OD;

OP INIT = (REF[,]INT matrix)VOID:
    FOR row index FROM LWB matrix TO UPB matrix DO
        INIT matrix[row index,]
    OD;

PROC total value = ([]INT items count, []BOUNTY items, BOUNTY sack) STRUCT(INT value, weight, volume):(
    ###
    Given the count of each item in the sack return -1 if they can"t be carried or their total value.

    (also return the negative of the weight and the volume so taking the max of a series of return
    values will minimise the weight if values tie, and minimise the volume if values and weights tie).
    ###
    INT weight = items count * weight OF items;
    INT volume = items count * volume OF items;
    IF weight > weight OF sack OR volume > volume OF sack THEN
        (-1, 0, 0)
    ELSE
        ( items count * value OF items, -weight, -volume)
    FI
);

PRIO WRAP = 5; # wrap negative array indices as per python's indexing regime #
OP WRAP = (INT index, upb)INT:
  IF index>=0 THEN index ELSE upb + index + 1 FI;

PROC knapsack dp = ([]BOUNTY items, BOUNTY sack)[]INT:(
    ###
    Solves the Knapsack problem, with two sets of weights,
    using a dynamic programming approach
    ###

    # (weight+1) x (volume+1) table #
    # table[w,v] is the maximum value that can be achieved #
    # with a sack of weight w and volume v. #
    # They all start out as 0 (empty sack) #
    [0:weight OF sack, 0:volume OF sack]INT table; INIT table;

    FOR w TO 1 UPB table DO
        FOR v TO 2 UPB table DO
            ### Consider the optimal solution, and consider the "last item" added
            to the sack. Removing this item must produce an optimal solution
            to the subproblem with the sack"s weight and volume reduced by that
            of the item. So we search through all possible "last items": ###
            FOR item index TO UPB items DO
                BOUNTY item := items[item index];
                # Only consider items that would fit: #
                IF w >= weight OF item AND v >= volume OF item THEN
                    # Optimal solution to subproblem + value of item: #
                    INT candidate := table[w-weight OF item,v-volume OF item] + value OF item;
                    IF candidate > table[w,v] THEN
                        table[w,v] := candidate
                    FI
                FI
            OD
        OD
    OD;

    [UPB items]INT result; INIT result;
    INT w := weight OF sack, v := volume OF sack;
    WHILE table[w,v] /= 0 DO
        # Find the last item that was added: #
        INT needle = table[w,v];
        INT item index;
        FOR i TO UPB items WHILE
            item index := i;
            BOUNTY item = items[item index];
            INT candidate = table[w-weight OF item WRAP UPB table, v-volume OF item WRAP 2 UPB table] + value OF item;
#       WHILE # candidate NE needle DO
          SKIP
        OD;
        # Record it in the result, and remove it: #
        result[item index] +:= 1;
        w -:= weight OF items[item index];
        v -:= volume OF items[item index]
    OD;
    result
);

[]INT max items = knapsack dp(items, sack);
STRUCT (INT value, weight, volume) max :=  total value(max items, items, sack);
max := (value OF max, -weight OF max, -volume OF max);

FORMAT d = $zz-d$;

printf(($"The maximum value achievable (by dynamic programming) is "gl$, value OF max));
printf(($"  The number of ("n(UPB items-1)(g", ")g") items to achieve this is: ("n(UPB items-1)(f(d)",")f(d)") respectively"l$,
    name OF items, max items));
printf(($"  The weight to carry is "f(d)", and the volume used is "f(d)l$,
    weight OF max, volume OF max))
```
Output:

```txt
The maximum value achievable (by dynamic programming) is      +54500
  The number of (panacea, ichor, gold) items to achieve this is: (   9,   0,  11) respectively
  The weight to carry is  247, and the volume used is  247
```



## AutoHotkey

Brute Force.

```AutoHotkey
Item  = Panacea,Ichor,Gold
Value = 3000,1800,2500
Weight= 3,2,20                         ; *10
Volume= 25,15,2                        ; *1000

StringSplit I, Item,  `,               ; Put input in arrays
StringSplit W, Weight,`,
StringSplit $, Value, `,
StringSplit V, Volume,`,

SetFormat Float, 0.3
W := 250, V := 250, sW:=.1, sV:=.001   ; limits for the total, scale factors
p := -1, Wp := -W1, Vp := -V1          ; initial values
While (Wp+=W1) <= W && (Vp+=V1) <= V {
   p++, Wi := Wp-W2, Vi := Vp-V2, i := -1
   While (Wi+=W2) <= W && (Vi+=V2) <= V {
      i++, Wg := Wi-W3, Vg := Vi-V3, g := -1
      While (Wg+=W3) <= W && (Vg+=V3) <= V
         If ($ <= Val := p*$1 + i*$2 + ++g*$3)
             t := ($=Val ? t "`n    " : "    ")
           . p "`t   " i "`t   " g "`t  " Wg*sW "`t   " Vg*sV
           , $ := Val
   }
}
MsgBox Value = %$%`n`nPanacea`tIchor`tGold`tWeight`tVolume`n%t%
```



## Bracmat


```bracmat
(knapsack=
  ( things
  =   (panacea.3000.3/10.25/1000)
      (ichor.1800.2/10.15/1000)
      (gold.2500.2.2/1000)
  )
& 0:?maxvalue
& :?sack
& ( add
  =     cumwght
        cumvol
        cumvalue
        cumsack
        name
        wght
        val
        vol
        tings
        n
        ncumwght
        ncumvalue
        ncumvol
    .     !arg
        : ( ?cumwght
          . ?cumvol
          . ?cumvalue
          . ?cumsack
          . (?name.?val.?wght.?vol) ?tings
          )
      & -1:?n
      &   whl
        ' ( 1+!n:?n
          & !cumwght+!n*!wght:~>25:?ncumwght
          & !cumvol+!n*!vol:~>250/1000:?ncumvol
          & !cumvalue+!n*!val:?ncumvalue
          & (   !tings:
              & (   !ncumvalue:>!maxvalue:?maxvalue
                  &     !cumsack
                        ( !n:0&
                        |   ( !cumsack:&Take
                            | Finally
                            )
                            " take "
                            !n
                            " items of "
                            !name
                            ".\n"
                        )
                    : ?sack
                |
                )
            |   add
              $ ( !ncumwght
                . !ncumvol
                . !ncumvalue
                .   !cumsack
                    ( !n:0&
                    | "Take " !n " items of " !name ".\n"
                    )
                . !tings
                )
            )
          )
  )
& add$(0.0.0..!things)
& out$(str$(!sack "The value in the knapsack is " !maxvalue "."))
&
);

!knapsack;

```

Output:

```txt
Take 15 items of ichor.
Finally take 11 items of gold.
The value in the knapsack is 54500.
```



## C


figures out the best (highest value) set by brute forcing every possible subset.


```c
#include <stdio.h>
#include <stdlib.h>

typedef struct {
    char *name;
    double value;
    double weight;
    double volume;
} item_t;

item_t items[] = {
    {"panacea", 3000.0, 0.3, 0.025},
    {"ichor",   1800.0, 0.2, 0.015},
    {"gold",    2500.0, 2.0, 0.002},
};

int n = sizeof (items) / sizeof (item_t);
int *count;
int *best;
double best_value;

void knapsack (int i, double value, double weight, double volume) {
    int j, m1, m2, m;
    if (i == n) {
        if (value > best_value) {
            best_value = value;
            for (j = 0; j < n; j++) {
                best[j] = count[j];
            }
        }
        return;
    }
    m1 = weight / items[i].weight;
    m2 = volume / items[i].volume;
    m = m1 < m2 ? m1 : m2;
    for (count[i] = m; count[i] >= 0; count[i]--) {
        knapsack(
            i + 1,
            value + count[i] * items[i].value,
            weight - count[i] * items[i].weight,
            volume - count[i] * items[i].volume
        );
    }
}

int main () {
    count = malloc(n * sizeof (int));
    best = malloc(n * sizeof (int));
    best_value = 0;
    knapsack(0, 0.0, 25.0, 0.25);
    int i;
    for (i = 0; i < n; i++) {
        printf("%d %s\n", best[i], items[i].name);
    }
    printf("best value: %.0f\n", best_value);
    free(count); free(best);
    return 0;
}

```


{{output}}
```txt
9 panacea
0 ichor
11 gold
best value: 54500

```


=={{header|C_sharp|C#}}==

```c#
/*Knapsack

  This model finds the integer optimal packing of a knapsack

  Nigel_Galloway
  January 29th., 2012
*/
using Microsoft.SolverFoundation.Services;

namespace KnapU
{
    class Item {
        public string Name {get; set;}
        public int Value {get; set;}
        public double Weight {get; set;}
        public double Volume {get; set;}

        public Item(string name, int value, double weight, double volume) {
            Name = name;
            Value = value;
            Weight = weight;
            Volume = volume;
        }
    }

    class Program
    {
        static void Main(string[] args)
        {
            SolverContext context = SolverContext.GetContext();
            Model model = context.CreateModel();
            Item[] Knapsack = new Item[] {
                new Item("Panacea", 3000, 0.3, 0.025),
                new Item("Ichor", 1800, 0.2, 0.015),
                new Item("Gold", 2500, 2.0, 0.002)
            };
            Set items = new Set(Domain.Any, "items");
            Decision take = new Decision(Domain.IntegerNonnegative, "take", items);
            model.AddDecision(take);
            Parameter value = new Parameter(Domain.IntegerNonnegative, "value", items);
            value.SetBinding(Knapsack, "Value", "Name");
            Parameter weight = new Parameter(Domain.RealNonnegative, "weight", items);
            weight.SetBinding(Knapsack, "Weight", "Name");
            Parameter volume = new Parameter(Domain.RealNonnegative, "volume", items);
            volume.SetBinding(Knapsack, "Volume", "Name");
            model.AddParameters(value, weight, volume);
            model.AddConstraint("knap_weight", Model.Sum(Model.ForEach(items, t => take[t] * weight[t])) <= 25);
            model.AddConstraint("knap_vol", Model.Sum(Model.ForEach(items, t => take[t] * volume[t])) <= 0.25);
            model.AddGoal("knap_value", GoalKind.Maximize, Model.Sum(Model.ForEach(items, t => take[t] * value[t])));
            Solution solution = context.Solve(new SimplexDirective());
            Report report = solution.GetReport();
            System.Console.Write("{0}", report);
        }
    }
}
```

Produces:

```txt


### Solver Foundation Service Report

Date: 28/01/2012 17:18:56
Version: Microsoft Solver Foundation 3.0.1.10599 Express Edition
Model Name: DefaultModel
Capabilities Applied: MILP
Solve Time (ms): 210
Total Time (ms): 376
Solve Completion Status: Optimal
Solver Selected: Microsoft.SolverFoundation.Solvers.SimplexSolver
Directives:
Simplex(TimeLimit = -1, MaximumGoalCount = -1, Arithmetic = Default, Pricing = D
efault, IterationLimit = -1, Algorithm = Default, Basis = Default, GetSensitivit
y = False)
Algorithm: Dual
Arithmetic: Double
Variables: 3 -> 3 + 3
Rows: 3 -> 3
Nonzeros: 9
Eliminated Slack Variables: 0
Pricing (double): SteepestEdge
Basis: Current
Pivot Count: 0
Phase 1 Pivots: 0 + 0
Phase 2 Pivots: 0 + 0
Factorings: 3 + 0
Degenerate Pivots: 0 (0.00 %)
Branches: 21

### Solution Details

Goals:
knap_value: 54500

Decisions:
take(Panacea): 9
take(Ichor): 0
take(Gold): 11

```



## C#


```c#
/*    Items  Value  Weight  Volume
          a     30       3      25
          b     18       2      15
          c     25      20       2

                     <=250   <=250      */
using System;
class Program
{
    static void Main()
    {
        uint[] r = items1();
        Console.WriteLine(r[0] + " v  " + r[1] + " a  " + r[2] + " b");  // 0 15 11
        var sw = System.Diagnostics.Stopwatch.StartNew();
        for (int i = 1000; i > 0; i--) items1();
        Console.Write(sw.Elapsed); Console.Read();
    }

    static uint[] items0()  // 1.2 µs
    {
        uint v, v0 = 0, a, b, c, a0 = 0, b0 = 0, c0 = 0;
        for (a = 0; a <= 10; a++)
            for (b = 0; a * 5 + b * 3 <= 50; b++)
                for (c = 0; a * 25 + b * 15 + c * 2 <= 250 && a * 3 + b * 2 + c * 20 <= 250; c++)
                    if (v0 < (v = a * 30 + b * 18 + c * 25))
                    {
                        v0 = v; a0 = a; b0 = b; c0 = c;
                        //Console.WriteLine("{0,5} {1,5} {2,5} {3,5}", v, a, b, c);
                    }
        return new uint[] { a0, b0, c0 };
    }

    static uint[] items1()  // 0,22 µs
    {
        uint v, v0 = 0, a, b, c, a0 = 0, b0 = 0, c0 = 0, c1 = 0;
        for (a = 0; a <= 10; a++)
            for (b = 0; a * 5 + b * 3 <= 50; b++)
            {
                c = (250 - a * 25 - b * 15) / 2;
                if ((c1 = (250 - a * 3 - b * 2) / 20) < c) c = c1;
                if (v0 < (v = a * 30 + b * 18 + c * 25))
                { v0 = v; a0 = a; b0 = b; c0 = c; }
            }
        return new uint[] { a0, b0, c0 };
    }
}
```


## Clojure


```lisp
(defstruct item :value :weight :volume)

(defn total [key items quantities]
  (reduce + (map * quantities (map key items))))

(defn max-count [item max-weight max-volume]
  (let [mcw (/ max-weight (:weight item))
        mcv (/ max-volume (:volume item))]
    (min mcw mcv)))
```

We have an <tt>item</tt> struct to contain the data for both contents and the knapsack. The <tt>total</tt> function returns the sum of a particular attribute across all items times their quantities.  Finally,  the <tt>max-count</tt> function returns the most of that item that could fit given the constraints (used as the upper bound on the combination).  Now the real work:

```lisp
(defn knapsacks []
  (let [pan (struct item 3000 0.3 0.025)
        ich (struct item 1800 0.2 0.015)
        gol (struct item 2500 2.0 0.002)
        types [pan ich gol]
        max-w 25.0
        max-v 0.25
        iters #(range (inc (max-count % max-w max-v)))]
    (filter (complement nil?)
      (pmap
        #(let [[p i g] %
                w (total :weight types %)
                v (total :volume types %)]
          (if (and (<= w max-w) (<= v max-v))
            (with-meta (struct item (total :value types %) w v) {:p p :i i :g g})))
        (for [p (iters pan)
              i (iters ich)
              g (iters gol)]
          [p i g])))))
```

The <tt>knapsacks</tt> function returns a lazy sequence of all valid knapsacks, with the particular content quantities as metadata. The work of realizing each knapsack is done in parallel via the <tt>pmap</tt> function.  The following then finds the best by value, and prints the result.

```lisp
(defn best-by-value [ks]
  (reduce #(if (> (:value %1) (:value %2)) %1 %2) ks))

(defn print-knapsack[k]
  (let [ {val :value w :weight  v :volume} k
        {p :p i :i g :g} ^k]
    (println "Maximum value:" (float val))
    (println "Total weight: " (float w))
    (println "Total volume: " (float v))
    (println "Containing:   " p "Panacea," i "Ichor," g "Gold")))
```

Calling <tt>(print-knapsack (best-by-value (knapsacks)))</tt> would result in something like:

```txt
Maximum value: 54500
Total weight:  24.7
Total volume:  0.247
Containing:    9 Panacea, 0 Ichor, 11 Gold
```

Further, we could find all "best" knapsacks rather simply (albeit at the cost of some efficiency):

```lisp
(defn all-best-by-value [ks]
  (let [b (best-by-value ks)]
    (filter #(= (:value b) (:value %)) ks)))

(defn print-knapsacks [ks]
  (doseq [k ks]
    (print-knapsack k)
    (println)))
```

Calling <tt>(print-knapsacks (all-best-by-value (knapsacks)))</tt> would result in something like:

```txt

Maximum value: 54500.0
Total weight:  25.0
Total volume:  0.247
Containing:    0 Panacea, 15 Ichor, 11 Gold

Maximum value: 54500.0
Total weight:  24.9
Total volume:  0.247
Containing:    3 Panacea, 10 Ichor, 11 Gold

Maximum value: 54500.0
Total weight:  24.8
Total volume:  0.247
Containing:    6 Panacea, 5 Ichor, 11 Gold

Maximum value: 54500.0
Total weight:  24.7
Total volume:  0.247
Containing:    9 Panacea, 0 Ichor, 11 Gold

```



## Common Lisp

A dynamic programming <i>O(maxVolume &times; maxWeight &times; nItems)</i> solution, where volumes and weights are integral values.

```lisp
(defun fill-knapsack (items max-volume max-weight)
  "Items is a list of lists of the form (name value weight volume) where weight
and value are integers. max-volume and max-weight, also integers, are the
maximum volume and weight of the knapsack. fill-knapsack returns a list of the
form (total-value inventory total-volume total-weight) where total-value is the
total-value of a knapsack packed with inventory (a list whose elements are
elements of items), and total-weight and total-volume are the total weights and
volumes of the inventory."
  ;; maxes is a table indexed by volume and weight, where maxes[volume,weight]
  ;; is a list of the form (value inventory used-volume used-weight) where
  ;; inventory is a list of items of maximum value fitting within volume and
  ;; weight, value is the maximum value, and used-volume/used-weight are the
  ;; actual volume/weight of the inventory.
  (let* ((VV (1+ max-volume))
         (WW (1+ max-weight))
         (maxes (make-array (list VV WW))))
    ;; fill in the base cases where volume or weight is 0
    (dotimes (v VV) (setf (aref maxes v 0) (list 0 '() 0 0)))
    (dotimes (w WW) (setf (aref maxes 0 w) (list 0 '() 0 0)))
    ;; populate the rest of the table. The best value for a volume/weight
    ;; combination is the best way of adding an item to any of the inventories
    ;; from [volume-1,weight], [volume,weight-1], or [volume-1,weight-1], or the
    ;; best of these, if no items can be added.
    (do ((v 1 (1+ v))) ((= v VV) (aref maxes max-volume max-weight))
      (do ((w 1 (1+ w))) ((= w WW))
        (let ((options (sort (list (aref maxes v (1- w))
                                   (aref maxes (1- v) w)
                                   (aref maxes (1- v) (1- w)))
                             '> :key 'first)))
          (destructuring-bind (b-value b-items b-volume b-weight) (first options)
            (dolist (option options)
              (destructuring-bind (o-value o-items o-volume o-weight) option
                (dolist (item items)
                  (destructuring-bind (_ i-value i-volume i-weight) item
                    (declare (ignore _))
                    (when (and (<= (+ o-volume i-volume) v)
                               (<= (+ o-weight i-weight) w)
                               (>  (+ o-value  i-value)  b-value))
                      (setf b-value  (+ o-value  i-value)
                            b-volume (+ o-volume i-volume)
                            b-weight (+ o-weight i-weight)
                            b-items  (list* item o-items)))))))
            (setf (aref maxes v w)
                  (list b-value b-items b-volume b-weight))))))))
```


Use, having multiplied volumes and weights as to be integral:


```txt
> (pprint (fill-knapsack '((panacea 3000  3 25)
                           (ichor   1800  2 15)
                           (gold    2500 20  2))
                         250
                         250))

(54500              ; total-value
 ((ICHOR 1800 2 15) ; 15 ichor
  ...
  (ICHOR 1800 2 15)
  (GOLD 2500 20 2)  ; 11 gold
  ...
  (GOLD 2500 20 2))
 250                ; total volume
 247)               ; total weight
```



## D

{{trans|Python}}

```d
void main() @safe /*@nogc*/ {
    import std.stdio, std.algorithm, std.typecons, std.conv;

    static struct Bounty {
        int value;
        double weight, volume;
    }

    immutable Bounty panacea = {3000,  0.3, 0.025};
    immutable Bounty ichor =   {1800,  0.2, 0.015};
    immutable Bounty gold =    {2500,  2.0, 0.002};
    immutable Bounty sack =    {   0, 25.0, 0.25 };

    immutable maxPanacea = min(sack.weight / panacea.weight,
                               sack.volume / panacea.volume).to!int;
    immutable maxIchor   = min(sack.weight / ichor.weight,
                               sack.volume / ichor.volume).to!int;
    immutable maxGold    = min(sack.weight / gold.weight,
                               sack.volume / gold.volume).to!int;

    Bounty best = {0, 0, 0};
    Tuple!(int, int, int) bestAmounts;

    foreach (immutable nPanacea; 0 .. maxPanacea)
        foreach (immutable nIchor; 0 .. maxIchor)
            foreach (immutable nGold; 0 .. maxGold) {
                immutable Bounty current = {
                    value: nPanacea * panacea.value +
                           nIchor * ichor.value +
                           nGold * gold.value,
                    weight: nPanacea * panacea.weight +
                            nIchor * ichor.weight +
                            nGold * gold.weight,
                    volume: nPanacea * panacea.volume +
                            nIchor * ichor.volume +
                            nGold * gold.volume};

                if (current.value > best.value &&
                    current.weight <= sack.weight &&
                    current.volume <= sack.volume) {
                    best = Bounty(current.value, current.weight, current.volume);
                    bestAmounts = tuple(nPanacea, nIchor, nGold);
                }
            }

    writeln("Maximum value achievable is ", best.value);
    writefln("This is achieved by carrying (one solution) %d" ~
             " panacea, %d ichor and %d gold", bestAmounts[]);
    writefln("The weight to carry is %4.1f and the volume used is %5.3f",
             best.weight, best.volume);
}
```

{{out}}

```txt
Maximum value achievable is 54500
This is achieved by carrying (one solution) 0 panacea, 15 ichor and 11 gold
The weight to carry is 25.0 and the volume used is 0.247
```



### Alternative Version

The output is the same.

```d
void main() {
  import std.stdio, std.algorithm, std.typecons, std.range, std.conv;

  alias Bounty = Tuple!(int,"value", double,"weight", double,"volume");

  immutable panacea = Bounty(3000,  0.3, 0.025);
  immutable ichor =   Bounty(1800,  0.2, 0.015);
  immutable gold =    Bounty(2500,  2.0, 0.002);
  immutable sack =    Bounty(   0, 25.0, 0.25);

  immutable maxPanacea = min(sack.weight / panacea.weight, sack.volume / panacea.volume).to!int;
  immutable maxIchor   = min(sack.weight / ichor.weight,   sack.volume / ichor.volume).to!int;
  immutable maxGold    = min(sack.weight / gold.weight,    sack.volume / gold.volume).to!int;

  immutable best =
    cartesianProduct(maxPanacea.iota, maxIchor.iota, maxGold.iota)
    .map!(t => tuple(Bounty(t[0] * panacea.value  + t[1] * ichor.value  + t[2] * gold.value,
                            t[0] * panacea.weight + t[1] * ichor.weight + t[2] * gold.weight,
                            t[0] * panacea.volume + t[1] * ichor.volume + t[2] * gold.volume), t))
    .filter!(t => t[0].weight <= sack.weight && t[0].volume <= sack.volume)
    .reduce!max;

  writeln("Maximum value achievable is ", best[0].value);
  writefln("This is achieved by carrying (one solution) %d panacea, %d ichor and %d gold", best[1][]);
  writefln("The weight to carry is %4.1f and the volume used is %5.3f", best[0][1..$]);
}
```



## E

This is a mostly brute-force general solution (the first author of this example does not know dynamic programming); the only optimization is that when considering the last (third) treasure type, it does not bother filling with anything but the maximum amount.

```e
pragma.enable("accumulator")

/** A data type representing a bunch of stuff (or empty space). */
def makeQuantity(value, weight, volume, counts) {
  def quantity {
    to __printOn(out) {
      for name => n in counts { out.print(`$n $name  `) }
      out.print(`(val=$value wt=$weight vol=$volume)`)
    }
    to value () { return value  }
    to weight() { return weight }
    to volume() { return volume }
    to counts() { return counts }
    to subtract(other) { return quantity + other * -1 }
    to add(other) {
      return makeQuantity(value  + other.value (),
                          weight + other.weight(),
                          volume + other.volume(),
                          accum counts for name => n in other.counts() { _.with(name, n+counts.fetch(name, fn {0})) })
    }
    to multiply(scalar) {
      return makeQuantity(value  * scalar,
                          weight * scalar,
                          volume * scalar,
                          accum [].asMap() for name => n in counts { _.with(name, n*scalar) })
    }
    /** a.fit(b) the greatest integer k such that a - b * k does not have negative weight or volume. */
    to fit(item) {
      return (weight // item.weight()) \
         .min(volume // item.volume())
    }
  }
  return quantity
}

/** Fill the space with the treasures, returning candidate results as spaceAvailable - the items. */
def fill(spaceAvailable, treasures) {
  if (treasures.size().isZero()) { # nothing to pick
    return [spaceAvailable]
  }

  # Pick one treasure type
  def [unit] + otherTreasures := treasures

  var results := []
  for count in (0..spaceAvailable.fit(unit)).descending() {
    results += fill(spaceAvailable - unit * count, otherTreasures)
    if (otherTreasures.size().isZero()) {
      break # If there are no further kinds, there is no point in taking less than the most
    }
  }
  return results
}

def chooseBest(emptyKnapsack, treasures) {
  var maxValue := 0
  var best := []
  for result in fill(emptyKnapsack, treasures) {
    def taken := emptyKnapsack - result # invert the backwards result fill() returns
    if (taken.value() > maxValue) {
      best := [taken]
      maxValue := taken.value()
    } else if (taken.value() <=> maxValue) {
      best with= taken
    }
  }
  return best
}

def printBest(emptyKnapsack, treasures) {
  for taken in chooseBest(emptyKnapsack, treasures) { println(`  $taken`) }
}

def panacea := makeQuantity(3000, 0.3, 0.025, ["panacea" => 1])
def ichor   := makeQuantity(1800, 0.2, 0.015, ["ichor"   => 1])
def gold    := makeQuantity(2500, 2.0, 0.002, ["gold"    => 1])
def emptyKnapsack \
            := makeQuantity(   0,  25, 0.250, [].asMap())

printBest(emptyKnapsack, [panacea, ichor, gold])
```



## EchoLisp

Use a cache, and multiply by 10^n to get an integer problem.

```scheme

(require 'struct)
(require 'hash)
(require 'sql)

(define H null) ;; cache
(define T (make-table (struct goodies (name valeur poids volume ))))
(define-syntax-rule (name i) (table-xref T i 0))
(define-syntax-rule (valeur i) (table-xref T i 1))
(define-syntax-rule (poids i) (table-xref T i 2))
(define-syntax-rule (volume i) (table-xref T i 3))


(define goodies
  '(("🍁-panacea"  3000 300 25)
   ("🌵-ichor"    1800 200 15)
   ("⭐️-gold"     2500 2000 2)))

(list->table goodies T)

;; i = item index, p= remaining weight, v = remaining volume

;;  make an unique hash-key from (i p v)
(define (t-key i p v)  (string-append i "|" p "|" v))

;; retrieve best core for item i
;; returns ( score . quantity)

(define (t-get i p v)
   (if ( < i 0) (cons 0 0)
      (hash-ref H (t-key i p v )))) ;; may be #f

;; compute best quantity.score (i), assuming best (i-1 p v) is known
(define (score-qty i p v (q) (score)(smax)(qmax))
	 (or
	 (t-get i p v) ;; already known
	 (begin
 		(set! q (min (quotient p (poids i)) (quotient v (volume i)))) ;; max possible q
 		(set! smax -Infinity)
		    ( for ((k (1+ q))) ;; try all legal quantities
		      (set! score (+
		         (first (score-qty (1- i) (- p (* k (poids i))) (- v (* k (volume i)))))
		         (* k (valeur i))))
		     #:continue (< score smax)
		      (set! smax score)
		      (set! qmax k))
		 (hash-set H (t-key i p v) (cons smax qmax)))))


;; compute best scores, starting from last item
(define (task P V)
        (define N (1- (table-count T)))
        (define qty 0)
        (set! H (make-hash))
	(writeln 'total-value (first (score-qty N P V)))

	(for/list  ((i (in-range N -1 -1)))
			(set! qty (rest (t-get i P V)))
			#:continue (= qty 0)
			(begin0
			(cons (name i) (t-get i P V))
			(set! P (- P (* (poids i) qty)))
			(set! V (- V (* (volume i) qty))))))

;; output
(task 25000 250)
total-value     54500
    → (("⭐️-gold" 54500 . 11) ("🌵-ichor" 27000 . 15))

(length (hash-keys H)) ;; # entries in cache
    → 218


```



## Eiffel


```Eiffel

class
	KNAPSACK

create
	make

feature

	make
		do
			create panacea;
			panacea := [3000, 0.3, 0.025]
			create ichor;
			ichor := [1800, 0.2, 0.015]
			create gold;
			gold := [2500, 2.0, 0.002]
			create sack;
			sack := [0, 25.0, 0.25]
			find_solution
		end

feature {NONE}

	panacea: TUPLE [value: INTEGER; weight: REAL_64; volume: REAL_64]

	ichor: TUPLE [value: INTEGER; weight: REAL_64; volume: REAL_64]

	gold: TUPLE [value: INTEGER; weight: REAL_64; volume: REAL_64]

	sack: TUPLE [value: INTEGER; weight: REAL_64; volume: REAL_64]

	find_solution
			-- Solution for unbounded Knapsack Problem.
		local
			totalweight, totalvolume: REAL_64
			maxpanacea, maxichor, maxvalue, maxgold: INTEGER
			n: ARRAY [INTEGER]
			r: TUPLE [value: INTEGER; weight: REAL_64; volume: REAL_64]
		do
			maxpanacea := minimum (sack.weight / panacea.weight, sack.volume / panacea.volume).rounded
			maxichor := minimum (sack.weight / ichor.weight, sack.volume / ichor.volume).rounded
			maxgold := minimum (sack.weight / gold.weight, sack.volume / gold.volume).rounded
			create n.make_filled (0, 1, 3)
			create r
			across
				0 |..| maxpanacea as p
			loop
				across
					0 |..| maxichor as i
				loop
					across
						0 |..| maxgold as g
					loop
						r.value := g.item * gold.value + i.item * ichor.value + p.item * panacea.value
						r.weight := g.item * gold.weight + i.item * ichor.weight + p.item * panacea.weight
						r.volume := g.item * gold.volume + i.item * ichor.volume + p.item * panacea.volume
						if r.value > maxvalue and r.weight <= sack.weight and r.volume <= sack.volume then
							maxvalue := r.value
							totalweight := r.weight
							totalvolume := r.volume
							n [1] := p.item
							n [2] := i.item
							n [3] := g.item
						end
					end
				end
			end
			io.put_string ("Maximum value achievable is " + maxValue.out + ".%N")
			io.put_string ("This is achieved by carrying " + n [1].out + " panacea, " + n [2].out + " ichor and " + n [3].out + " gold.%N")
			io.put_string ("The weight is " + totalweight.out + " and the volume is " + totalvolume.truncated_to_real.out + ".")
		end

	minimum (a, b: REAL_64): REAL_64
			-- Smaller of 'a' and 'b'.
		do
			Result := a
			if a > b then
				Result := b
			end
		end

end

```

{{out}}

```txt

Maximum value achievable is 54500.
This is achieved by carrying 0 panacea, 15 ichor and 11 gold.
The weight is 25 and the volume is 0.247.

```



## Elixir

Brute Force:

```elixir
defmodule Item do
  defstruct volume: 0.0, weight: 0.0, value: 0
  def new(volume, weight, value) do
    %__MODULE__{volume: volume, weight: weight, value: value}
  end
end

defmodule Knapsack do
  def solve_unbounded(items, maximum) do
    {max_volume, max_weight} = {maximum.volume, maximum.weight}
    max_items = Enum.map(items, fn {name,item} ->
      {name, trunc(min(max_volume / item.volume, max_weight / item.weight))}
    end)
    Enum.map(max_items, fn {name,max} -> for i <- 0..max, do: {name,i} end)
    |> product
    |> total(items)
    |> Enum.filter(fn {_kw, {volume,weight,_}} -> volume <= max_volume and
                                                  weight <= max_weight end)
    |> Enum.group_by(fn {_kw, {_,_,value}} -> value end)
    |> Enum.max
    |> print
  end

  defp product([x]), do: x
  defp product([a,b]), do: for x <- a, y <- b, do: [x,y]
  defp product([h|t]), do: for x <- h, y <- product(t), do: [x | y]

  defp total(lists, items) do
    Enum.map(lists, fn kwlist ->
      total = Enum.reduce(kwlist, {0,0,0}, fn {name,n},{volume,weight,value} ->
        {volume + n * items[name].volume,
         weight + n * items[name].weight,
         value  + n * items[name].value}
      end)
      {kwlist, total}
    end)
  end

  defp print({max_value, data}) do
    IO.puts "Maximum value achievable is #{max_value}\tvolume  weight  value"
    Enum.each(data, fn {kw,{volume,weight,value}} ->
      :io.format "~s =>\t~6.3f, ~5.1f, ~6w~n", [(inspect kw), volume, weight, value]
    end)
  end
end

items = %{panacea: Item.new(0.025, 0.3, 3000),
          ichor:   Item.new(0.015, 0.2, 1800),
          gold:    Item.new(0.002, 2.0, 2500) }
maximum = Item.new(0.25, 25, 0)
Knapsack.solve_unbounded(items, maximum)
```


{{out}}

```txt

Maximum value achievable is 54500       volume  weight  value
[gold: 11, ichor: 0, panacea: 9] =>      0.247,  24.7,  54500
[gold: 11, ichor: 5, panacea: 6] =>      0.247,  24.8,  54500
[gold: 11, ichor: 10, panacea: 3] =>     0.247,  24.9,  54500
[gold: 11, ichor: 15, panacea: 0] =>     0.247,  25.0,  54500

```



## Factor

This is a brute force solution. It is general enough to be able to provide solutions for any number of different items.

```factor
USING: accessors combinators kernel locals math math.order
math.vectors sequences sequences.product combinators.short-circuit ;
IN: knapsack

CONSTANT: values { 3000 1800 2500 }
CONSTANT: weights { 0.3 0.2 2.0 }
CONSTANT: volumes { 0.025 0.015 0.002 }

CONSTANT: max-weight 25.0
CONSTANT: max-volume 0.25

TUPLE: bounty amounts value weight volume ;

: <bounty> ( items -- bounty )
    [ bounty new ] dip {
        [ >>amounts ]
        [ values v. >>value ]
        [ weights v. >>weight ]
        [ volumes v. >>volume ]
    } cleave ;

: valid-bounty? ( bounty -- ? )
    { [ weight>> max-weight <= ]
      [ volume>> max-volume <= ] } 1&& ;

M:: bounty <=> ( a b -- <=> )
    a valid-bounty? [
        b valid-bounty? [
            a b [ value>> ] compare
        ] [ +gt+ ] if
    ] [ b valid-bounty? +lt+ +eq+ ? ] if ;

: find-max-amounts ( -- amounts )
    weights volumes [
        [ max-weight swap / ]
        [ max-volume swap / ] bi* min >integer
    ] 2map ;

: best-bounty ( -- bounty )
    find-max-amounts [ 1 + iota ] map <product-sequence>
    [ <bounty> ] [ max ] map-reduce ;
```



## Forth


```forth
\ : value ; immediate
: weight cell+ ;
: volume 2 cells + ;
: number 3 cells + ;

\      item value weight volume number
create panacea 30 ,   3 ,  25 , 0 ,
create ichor   18 ,   2 ,  15 , 0 ,
create gold    25 ,  20 ,   2 , 0 ,
create sack     0 , 250 , 250 ,

: fits? ( item -- ? )
  dup weight @ sack weight @ > if drop false exit then
      volume @ sack volume @ > 0= ;

: add ( item -- )
  dup        @        sack        +!
  dup weight @ negate sack weight +!
  dup volume @ negate sack volume +!
  1 swap number +! ;

: take ( item -- )
  dup        @ negate sack        +!
  dup weight @        sack weight +!
  dup volume @        sack volume +!
  -1 swap number +! ;

variable max-value
variable max-pan
variable max-ich
variable max-au

: .solution
  cr
  max-pan @ . ." Panaceas, "
  max-ich @ . ." Ichors, and "
  max-au  @ . ." Gold for a total value of "
  max-value @ 100 * . ;

: check
  sack @ max-value @ <= if exit then
  sack           @ max-value !
  panacea number @ max-pan   !
  ichor   number @ max-ich   !
  gold    number @ max-au    !
  ( .solution ) ;    \ and change <= to < to see all solutions

: solve-gold
  gold fits? if gold add  recurse  gold take
  else check then ;

: solve-ichor
  ichor fits? if ichor add  recurse  ichor take then
  solve-gold ;

: solve-panacea
  panacea fits? if panacea add  recurse  panacea take then
  solve-ichor ;

solve-panacea .solution
```

Or like this...

```forth
0 VALUE vials
0 VALUE ampules
0 VALUE bars
0 VALUE bag

#250   3 /  #250 #25 /   MIN 1+ CONSTANT maxvials
#250    2/  #250 #15 /   MIN 1+ CONSTANT maxampules
#250 #20 /  #250    2/   MIN 1+ CONSTANT maxbars

: RESULTS ( v a b -- k )
	3DUP #20 *  SWAP 2*      +  SWAP     3 * +  #250 > IF  3DROP -1 EXIT  ENDIF
	3DUP    2*  SWAP #15   * +  SWAP   #25 * +  #250 > IF  3DROP -1 EXIT  ENDIF
	#2500    *  SWAP #1800 * +  SWAP #3000 * + ;

: .SOLUTION ( -- )
	CR ." The traveller's knapsack contains "
	   vials   DEC. ." vials of panacea, "
	   ampules DEC. ." ampules of ichor, "
	CR bars    DEC. ." bars of gold, a total value of "
	   vials ampules bars RESULTS 0DEC.R ." ." ;

: KNAPSACK ( -- )
	-1 TO bag
	maxvials 0 ?DO
	maxampules 0 ?DO
	     maxbars 0 ?DO
                              K J I RESULTS DUP
			    bag  > IF  TO bag  K TO vials J TO ampules I TO bars
				ELSE  DROP
			        ENDIF
		     LOOP
		   LOOP
		 LOOP
	.SOLUTION ;
```

With the result...

 FORTH> knapsack
 The traveller's knapsack contains 0 vials of panacea, 15 ampules of ichor,
 11 bars of gold, a total value of 54500. ok


## Fortran

{{works with|Fortran|90 and later}}
A straight forward 'brute force' approach

```fortran
PROGRAM KNAPSACK

  IMPLICIT NONE

  REAL :: totalWeight, totalVolume
  INTEGER :: maxPanacea, maxIchor, maxGold, maxValue = 0
  INTEGER :: i, j, k
  INTEGER :: n(3)

  TYPE Bounty
    INTEGER :: value
    REAL :: weight
    REAL :: volume
  END TYPE Bounty

  TYPE(Bounty) :: panacea, ichor, gold, sack, current

  panacea = Bounty(3000, 0.3, 0.025)
  ichor   = Bounty(1800, 0.2, 0.015)
  gold    = Bounty(2500, 2.0, 0.002)
  sack    = Bounty(0, 25.0, 0.25)

  maxPanacea = MIN(sack%weight / panacea%weight, sack%volume / panacea%volume)
  maxIchor = MIN(sack%weight / ichor%weight, sack%volume / ichor%volume)
  maxGold = MIN(sack%weight / gold%weight, sack%volume / gold%volume)

  DO i = 0, maxPanacea
     DO j = 0, maxIchor
        Do k = 0, maxGold
           current%value = k * gold%value + j * ichor%value + i * panacea%value
           current%weight = k * gold%weight + j * ichor%weight + i * panacea%weight
           current%volume = k * gold%volume + j * ichor%volume + i * panacea%volume
           IF (current%weight > sack%weight .OR. current%volume > sack%volume) CYCLE
           IF (current%value > maxValue) THEN
              maxValue = current%value
              totalWeight = current%weight
              totalVolume = current%volume
              n(1) = i ; n(2) = j ; n(3) = k
           END IF
        END DO
     END DO
  END DO

  WRITE(*, "(A,I0)") "Maximum value achievable is ", maxValue
  WRITE(*, "(3(A,I0),A)") "This is achieved by carrying ", n(1), " panacea, ", n(2), " ichor and ", n(3), " gold items"
  WRITE(*, "(A,F4.1,A,F5.3)") "The weight to carry is ", totalWeight, " and the volume used is ", totalVolume

END PROGRAM KNAPSACK
```

Sample output
 Maximum value achievable is 54500
 This is achieved by carrying 0 panacea, 15 ichor and 11 gold items
 The weight to carry is 25.0 and the volume used is 0.247


## Go

Recursive brute-force.

```go
package main

import "fmt"

type Item struct {
	Name           string
	Value          int
	Weight, Volume float64
}

type Result struct {
	Counts []int
	Sum    int
}

func min(a, b int) int {
	if a < b {
		return a
	}
	return b
}

func Knapsack(items []Item, weight, volume float64) (best Result) {
	if len(items) == 0 {
		return
	}
	n := len(items) - 1
	maxCount := min(int(weight/items[n].Weight), int(volume/items[n].Volume))
	for count := 0; count <= maxCount; count++ {
		sol := Knapsack(items[:n],
			weight-float64(count)*items[n].Weight,
			volume-float64(count)*items[n].Volume)
		sol.Sum += items[n].Value * count
		if sol.Sum > best.Sum {
			sol.Counts = append(sol.Counts, count)
			best = sol
		}
	}
	return
}

func main() {
	items := []Item{
		{"Panacea", 3000, 0.3, 0.025},
		{"Ichor", 1800, 0.2, 0.015},
		{"Gold", 2500, 2.0, 0.002},
	}
	var sumCount, sumValue int
	var sumWeight, sumVolume float64

	result := Knapsack(items, 25, 0.25)

	for i := range result.Counts {
		fmt.Printf("%-8s x%3d  -> Weight: %4.1f  Volume: %5.3f  Value: %6d\n",
			items[i].Name, result.Counts[i], items[i].Weight*float64(result.Counts[i]),
			items[i].Volume*float64(result.Counts[i]), items[i].Value*result.Counts[i])

		sumCount += result.Counts[i]
		sumValue += items[i].Value * result.Counts[i]
		sumWeight += items[i].Weight * float64(result.Counts[i])
		sumVolume += items[i].Volume * float64(result.Counts[i])
	}

	fmt.Printf("TOTAL (%3d items) Weight: %4.1f  Volume: %5.3f  Value: %6d\n",
		sumCount, sumWeight, sumVolume, sumValue)
}
```


Output:

```txt

Panacea  x  9  -> Weight:  2.7  Volume: 0.225  Value:  27000
Ichor    x  0  -> Weight:  0.0  Volume: 0.000  Value:      0
Gold     x 11  -> Weight: 22.0  Volume: 0.022  Value:  27500
TOTAL ( 20 items) Weight: 24.7  Volume: 0.247  Value:  54500

```



## Groovy

Solution: dynamic programming

```groovy
def totalWeight = { list -> list.collect{ it.item.weight * it.count }.sum() }
def totalVolume = { list -> list.collect{ it.item.volume * it.count }.sum() }
def totalValue = { list -> list.collect{ it.item.value * it.count }.sum() }

def knapsackUnbounded = { possibleItems, BigDecimal weightMax, BigDecimal volumeMax ->
    def n = possibleItems.size()
    def wm = weightMax.unscaledValue()
    def vm = volumeMax.unscaledValue()
    def m = (0..n).collect{ i -> (0..wm).collect{ w -> (0..vm).collect{ v -> [] } } }
    (1..wm).each { w ->
        (1..vm).each { v ->
            (1..n).each { i ->
                def item = possibleItems[i-1]
                def wi = item.weight.unscaledValue()
                def vi = item.volume.unscaledValue()
                def bi = [w.intdiv(wi),v.intdiv(vi)].min()
                m[i][w][v] = (0..bi).collect{ count ->
                    m[i-1][w - wi * count][v - vi * count] + [[item:item, count:count]]
                }.max(totalValue).findAll{ it.count }
            }
        }
    }
    m[n][wm][vm]
}
```


Test:

```groovy
Set solutions = []
items.eachPermutation { itemList ->
    def start = System.currentTimeMillis()
    def packingList = knapsackUnbounded(itemList, 25.0, 0.250)
    def elapsed = System.currentTimeMillis() - start

    println "\n  Item Order: ${itemList.collect{ it.name.split()[0] }}"
    println "Elapsed Time: ${elapsed/1000.0} s"

    solutions << (packingList as Set)
}

solutions.each { packingList ->
    println "\nTotal Weight: ${totalWeight(packingList)}"
    println "Total Volume: ${totalVolume(packingList)}"
    println " Total Value: ${totalValue(packingList)}"
    packingList.each {
        printf ('  item: %-22s  count:%2d  weight:%4.1f  Volume:%5.3f\n',
                it.item.name, it.count, it.item.weight * it.count, it.item.volume * it.count)
    }
}
```


Output:

```txt
  Item Order: [panacea, ichor, gold]
Elapsed Time: 26.883 s

  Item Order: [panacea, gold, ichor]
Elapsed Time: 27.17 s

  Item Order: [ichor, panacea, gold]
Elapsed Time: 25.884 s

  Item Order: [ichor, gold, panacea]
Elapsed Time: 26.126 s

  Item Order: [gold, panacea, ichor]
Elapsed Time: 26.596 s

  Item Order: [gold, ichor, panacea]
Elapsed Time: 26.47 s

Total Weight: 25.0
Total Volume: 0.247
 Total Value: 54500
  item: gold (bars)             count:11  weight:22.0  Volume:0.022
  item: ichor (ampules of)      count:15  weight: 3.0  Volume:0.225

Total Weight: 24.7
Total Volume: 0.247
 Total Value: 54500
  item: gold (bars)             count:11  weight:22.0  Volume:0.022
  item: panacea (vials of)      count: 9  weight: 2.7  Volume:0.225
```


While this solver can only be used to detect two of the four possible solutions, the other two may be discovered by noting that 5 ampules of ichor and 3 vials of panacea have the same value and the same volume and only differ by 0.1 in weight. Thus the other two solutions can be derived by substitution as follows:

```txt
Total Weight: 24.9
Total Volume: 0.247
 Total Value: 54500
  item: gold (bars)             count:11  weight:22.0  Volume:0.022
  item: ichor (ampules of)      count:10  weight: 2.0  Volume:0.150
  item: panacea (vials of)      count: 3  weight: 0.9  Volume:0.075

Total Weight: 24.8
Total Volume: 0.247
 Total Value: 54500
  item: gold (bars)             count:11  weight:22.0  Volume:0.022
  item: ichor (ampules of)      count: 5  weight: 1.0  Volume:0.075
  item: panacea (vials of)      count: 6  weight: 1.8  Volume:0.150
```



## Haskell

This is a brute-force solution: it generates a list of every legal combination of items (<tt>options</tt>) and then finds the option of greatest value.

```haskell
import Data.List (maximumBy)
import Data.Ord (comparing)

(maxWgt, maxVol) = (25, 0.25)
items =
   [Bounty  "panacea"  3000  0.3  0.025,
    Bounty  "ichor"    1800  0.2  0.015,
    Bounty  "gold"     2500  2.0  0.002]

data Bounty = Bounty
   {itemName :: String,
    itemVal :: Int,
    itemWgt, itemVol :: Double}

names = map itemName items
vals = map itemVal items
wgts = map itemWgt items
vols = map itemVol items

dotProduct :: (Num a, Integral b) => [a] -> [b] -> a
dotProduct factors = sum . zipWith (*) factors . map fromIntegral

options :: [[Int]]
options = filter fits $ mapM f items
  where f (Bounty _ _ w v) = [0 .. m]
          where m = floor $ min (maxWgt / w) (maxVol / v)
        fits opt = dotProduct wgts opt <= maxWgt &&
                   dotProduct vols opt <= maxVol

showOpt :: [Int] -> String
showOpt opt = concat (zipWith showItem names opt) ++
    "total weight: " ++ show (dotProduct wgts opt) ++
    "\ntotal volume: " ++ show (dotProduct vols opt) ++
    "\ntotal value: " ++ show (dotProduct vals opt) ++ "\n"
  where showItem name num = name ++ ": " ++ show num ++ "\n"

main = putStr $ showOpt $ best options
  where best = maximumBy $ comparing $ dotProduct vals
```


Output:


```txt
panacea: 9
ichor: 0
gold: 11
total weight: 24.7
total volume: 0.247
total value: 54500
```



## HicEst


```hicest
CHARACTER list*1000

NN = ALIAS($Panacea, $Ichor, $Gold, wSack, wPanacea, wIchor, wGold, vSack, vPanacea, vIchor, vGold)
NN =      (3000,     1800,   2500,  25,    0.3,      0.2,    2.0,   0.25,  0.025,    0.015,  0.002)
maxItems = ALIAS(maxPanacea, maxIchor, maxGold)
maxItems = ( MIN( wSack/wPanacea, vSack/vPanacea), MIN( wSack/wIchor, vSack/vIchor), MIN( wSack/wGold, vSack/vGold) )

maxValue = 0
DO Panaceas = 0, maxPanacea
  DO Ichors = 0, maxIchor
    DO Golds = 0, maxGold
      weight = Panaceas*wPanacea + Ichors*wIchor + Golds*wGold
      IF( weight <= wSack ) THEN
        volume = Panaceas*vPanacea + Ichors*vIchor + Golds*vGold
        IF( volume <= vSack ) THEN
          value =  Panaceas*$Panacea + Ichors*$Ichor + Golds*$Gold
          IF( value > maxValue ) THEN
            maxValue = value
            ! this restarts the list, removing all previous entries:
            WRITE(Text=list, Name) value, Panaceas, Ichors, Golds, weight, volume, $CR//$LF
          ELSEIF( value == maxValue ) THEN
            WRITE(Text=list, Name, APPend) value, Panaceas, Ichors, Golds, weight, volume, $CR//$LF
          ENDIF
        ENDIF
      ENDIF
    ENDDO
  ENDDO
ENDDO
```


```hicest
value=54500; Panaceas=0; Ichors=15; Golds=11; weight=25; volume=0.247;
value=54500; Panaceas=3; Ichors=10; Golds=11; weight=24.9; volume=0.247;
value=54500; Panaceas=6; Ichors=5; Golds=11; weight=24.8; volume=0.247;
value=54500; Panaceas=9; Ichors=0; Golds=11; weight=24.7; volume=0.247;
```



## J

Brute force solution.

```j
mwv=: 25 0.25
prods=: <;. _1 ' panacea: ichor: gold:'
hdrs=: <;. _1 ' weight: volume: value:'
vls=: 3000 1800 2500
ws=: 0.3 0.2 2.0
vs=: 0.025 0.015 0.002

ip=: +/ .*
prtscr=: (1!:2)&2

KS=: 3 : 0
 os=. (#:i.@(*/)) mwv >:@<.@<./@:% ws,:vs
 bo=.os#~(ws,:vs) mwv&(*./@:>)@ip"_ 1 os
 mo=.bo{~{.\: vls ip"1 bo
 prtscr &.> prods ([,' ',":@])&.>mo
 prtscr &.> hdrs ('total '&,@[,' ',":@])&.> mo ip"1 ws,vs,:vls
 LF
)
```

Example output:

```txt
   KS''
panacea: 3
ichor: 10
gold: 11
total weight: 24.9
total volume: 0.247
total value: 54500
```



## Java

With recursion for more than 3 items.

```java
package hu.pj.alg;

import hu.pj.obj.Item;
import java.text.*;

public class UnboundedKnapsack {

    protected Item []  items = {
                               new Item("panacea", 3000,  0.3, 0.025),
                               new Item("ichor"  , 1800,  0.2, 0.015),
                               new Item("gold"   , 2500,  2.0, 0.002)
                               };
    protected final int    n = items.length; // the number of items
    protected Item      sack = new Item("sack"   ,    0, 25.0, 0.250);
    protected Item      best = new Item("best"   ,    0,  0.0, 0.000);
    protected int  []  maxIt = new int [n];  // maximum number of items
    protected int  []    iIt = new int [n];  // current indexes of items
    protected int  [] bestAm = new int [n];  // best amounts

    public UnboundedKnapsack() {
        // initializing:
        for (int i = 0; i < n; i++) {
            maxIt [i] = Math.min(
                           (int)(sack.getWeight() / items[i].getWeight()),
                           (int)(sack.getVolume() / items[i].getVolume())
                        );
        } // for (i)

        // calc the solution:
        calcWithRecursion(0);

        // Print out the solution:
        NumberFormat nf = NumberFormat.getInstance();
        System.out.println("Maximum value achievable is: " + best.getValue());
        System.out.print("This is achieved by carrying (one solution): ");
        for (int i = 0; i < n; i++) {
            System.out.print(bestAm[i] + " " + items[i].getName() + ", ");
        }
        System.out.println();
        System.out.println("The weight to carry is: " + nf.format(best.getWeight()) +
                           "   and the volume used is: " + nf.format(best.getVolume())
                          );

    }

    // calculation the solution with recursion method
    // item : the number of item in the "items" array
    public void calcWithRecursion(int item) {
        for (int i = 0; i <= maxIt[item]; i++) {
            iIt[item] = i;
            if (item < n-1) {
                calcWithRecursion(item+1);
            } else {
                int    currVal = 0;   // current value
                double currWei = 0.0; // current weight
                double currVol = 0.0; // current Volume
                for (int j = 0; j < n; j++) {
                    currVal += iIt[j] * items[j].getValue();
                    currWei += iIt[j] * items[j].getWeight();
                    currVol += iIt[j] * items[j].getVolume();
                }

                if (currVal > best.getValue()
                    &&
                    currWei <= sack.getWeight()
                    &&
                    currVol <= sack.getVolume()
                )
                {
                    best.setValue (currVal);
                    best.setWeight(currWei);
                    best.setVolume(currVol);
                    for (int j = 0; j < n; j++) bestAm[j] = iIt[j];
                } // if (...)
            } // else
        } // for (i)
    } // calcWithRecursion()

    // the main() function:
    public static void main(String[] args) {
        new UnboundedKnapsack();
    } // main()

} // class
```



```java
package hu.pj.obj;

public class Item {
    protected String name = "";
    protected int value = 0;
    protected double weight = 0;
    protected double volume = 0;

    public Item() {
    }

    public Item(String name, int value, double weight, double volume) {
        setName(name);
        setValue(value);
        setWeight(weight);
        setVolume(volume);
    }

    public int getValue() {
        return value;
    }

    public void setValue(int value) {
        this.value = Math.max(value, 0);
    }

    public double getWeight() {
        return weight;
    }

    public void setWeight(double weight) {
        this.weight = Math.max(weight, 0);
    }

    public double getVolume() {
        return volume;
    }

    public void setVolume(double volume) {
        this.volume = Math.max(volume, 0);
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

} // class
```


output:

```txt
Maximum value achievable is: 54500
This is achieved by carrying (one solution): 0 panacea, 15 ichor, 11 gold,
The weight to carry is: 25   and the volume used is: 0,247
```



## JavaScript

Brute force.

```javascript
var gold = { 'value': 2500, 'weight': 2.0, 'volume': 0.002 },
    panacea = { 'value': 3000, 'weight': 0.3, 'volume': 0.025 },
    ichor = { 'value': 1800, 'weight': 0.2, 'volume': 0.015 },

    items = [gold, panacea, ichor],
    knapsack = {'weight': 25, 'volume': 0.25},
    max_val = 0,
    solutions = [],
    g, p, i, item, val;

for (i = 0; i < items.length; i += 1) {
    item = items[i];
    item.max = Math.min(
        Math.floor(knapsack.weight / item.weight),
        Math.floor(knapsack.volume / item.volume)
    );
}

for (g = 0; g <= gold.max; g += 1) {
    for (p = 0; p <= panacea.max; p += 1) {
        for (i = 0; i <= ichor.max; i += 1) {
            if (i * ichor.weight + g * gold.weight + p * panacea.weight > knapsack.weight) {
                continue;
            }
            if (i * ichor.volume + g * gold.volume + p * panacea.volume > knapsack.volume) {
                continue;
            }
            val = i * ichor.value + g * gold.value + p * panacea.value;
            if (val > max_val) {
                solutions = [];
                max_val = val;
            }
            if (val === max_val) {
                solutions.push([g, p, i]);
            }
        }
    }
}

document.write("maximum value: " + max_val + '
');
for (i = 0; i < solutions.length; i += 1) {
    item = solutions[i];
    document.write("(gold: " + item[0] + ", panacea: " + item[1] + ", ichor: " + item[2] + ")
");
}

output:

```txt
maximum value: 54500
(gold: 11, panacea: 0, ichor: 15)
(gold: 11, panacea: 3, ichor: 10)
(gold: 11, panacea: 6, ichor: 5)
(gold: 11, panacea: 9, ichor: 0)
```

```



## Julia

{{libheader|JuMP}}
```julia

using JuMP
using GLPKMathProgInterface

model = Model(solver=GLPKSolverMIP())

@variable(model, vials_of_panacea >= 0, Int)
@variable(model, ampules_of_ichor >= 0, Int)
@variable(model, bars_of_gold >= 0, Int)

@objective(model, Max, 3000*vials_of_panacea + 1800*ampules_of_ichor + 2500*bars_of_gold)

@constraint(model, 0.3*vials_of_panacea + 0.2*ampules_of_ichor + 2.0*bars_of_gold <= 25.0)
@constraint(model, 0.025*vials_of_panacea + 0.015*ampules_of_ichor + 0.002*bars_of_gold <= 0.25)

println("The optimization problem to be solved is:")
println(model)

status = solve(model)

println("Objective value: ", getobjectivevalue(model))
println("vials of panacea = ", getvalue(vials_of_panacea))
println("ampules of ichor = ", getvalue(ampules_of_ichor))
println("bars of gold = ", getvalue(bars_of_gold))
```

{{output}}

```txt

The optimization problem to be solved is:
Max 3000 vials_of_panacea + 1800 ampules_of_ichor + 2500 bars_of_gold
Subject to
 0.3 vials_of_panacea + 0.2 ampules_of_ichor + 2 bars_of_gold <= 25
 0.025 vials_of_panacea + 0.015 ampules_of_ichor + 0.002 bars_of_gold <= 0.25
 vials_of_panacea >= 0, integer
 ampules_of_ichor >= 0, integer
 bars_of_gold >= 0, integer

Objective value: 54500.0
vials of panacea = 9.0
ampules of ichor = 0.0
bars of gold = 11.0

```



## Kotlin

{{trans|C}}
Recursive brute force approach:

```scala
// version 1.1.2

data class Item(val name: String, val value: Double, val weight: Double, val volume: Double)

val items = listOf(
    Item("panacea", 3000.0, 0.3, 0.025),
    Item("ichor", 1800.0, 0.2, 0.015),
    Item("gold", 2500.0, 2.0, 0.002)
)

val n = items.size
val count = IntArray(n)
val best  = IntArray(n)
var bestValue = 0.0

const val MAX_WEIGHT = 25.0
const val MAX_VOLUME = 0.25

fun knapsack(i: Int, value: Double, weight: Double, volume: Double) {
    if (i == n) {
        if (value > bestValue) {
            bestValue = value
            for (j in 0 until n) best[j] = count[j]
        }
        return
    }
    val m1 = Math.floor(weight / items[i].weight).toInt()
    val m2 = Math.floor(volume / items[i].volume).toInt()
    val m  = minOf(m1, m2)
    count[i] = m
    while (count[i] >= 0) {
        knapsack(
            i + 1,
            value  + count[i] * items[i].value,
            weight - count[i] * items[i].weight,
            volume - count[i] * items[i].volume
        )
        count[i]--
    }
}

fun main(args: Array<String>) {
    knapsack(0, 0.0, MAX_WEIGHT, MAX_VOLUME)
    println("Item Chosen  Number Value  Weight  Volume")
    println("-----------  ------ -----  ------  ------")
    var itemCount = 0
    var sumNumber = 0
    var sumWeight = 0.0
    var sumVolume = 0.0
    for (i in 0 until n) {
        if (best[i] == 0) continue
        itemCount++
        val name   = items[i].name
        val number = best[i]
        val value  = items[i].value  * number
        val weight = items[i].weight * number
        val volume = items[i].volume * number
        sumNumber += number
        sumWeight += weight
        sumVolume += volume
        print("${name.padEnd(11)}   ${"%2d".format(number)}    ${"%5.0f".format(value)}   ${"%4.1f".format(weight)}")
        println("    ${"%4.2f".format(volume)}")
    }
    println("-----------  ------ -----  ------  ------")
    print("${itemCount} items       ${"%2d".format(sumNumber)}    ${"%5.0f".format(bestValue)}   ${"%4.1f".format(sumWeight)}")
    println("    ${"%4.2f".format(sumVolume)}")
}
```


{{out}}

```txt

Item Chosen  Number Value  Weight  Volume
-----------  ------ -----  ------  ------
panacea        9    27000    2.7    0.23
gold          11    27500   22.0    0.02
-----------  ------ -----  ------  ------
2 items       20    54500   24.7    0.25

```



## M4

A brute force solution:

```M4
divert(-1)
define(`set2d',`define(`$1[$2][$3]',`$4')')
define(`get2d',`defn(`$1[$2][$3]')')
define(`for',
   `ifelse($#,0,``$0'',
   `ifelse(eval($2<=$3),1,
   `pushdef(`$1',$2)$4`'popdef(`$1')$0(`$1',incr($2),$3,`$4')')')')

define(`min',
   `define(`ma',eval($1))`'define(`mb',eval($2))`'ifelse(eval(ma<mb),1,ma,mb)')

define(`setv',
   `set2d($1,$2,1,$3)`'set2d($1,$2,2,$4)`'set2d($1,$2,3,$5)`'set2d($1,$2,4,$6)')

dnl  name,value (each),weight,volume
setv(a,0,`knapsack',0,250,250)
setv(a,1,`panacea',3000,3,25)
setv(a,2,`ichor',1800,2,15)
setv(a,3,`gold',2500,20,2)

define(`mv',0)
for(`x',0,min(get2d(a,0,3)/get2d(a,1,3),get2d(a,0,4)/get2d(a,1,4)),
   `for(`y',0,min((get2d(a,0,3)-x*get2d(a,1,3))/get2d(a,2,3),
         (get2d(a,0,4)-x*get2d(a,1,4))/get2d(a,2,4)),
      `
define(`z',min((get2d(a,0,3)-x*get2d(a,1,3)-y*get2d(a,2,3))/get2d(a,3,3),
   (get2d(a,0,4)-x*get2d(a,1,4)-y*get2d(a,2,4))/get2d(a,3,4)))
define(`cv',eval(x*get2d(a,1,2)+y*get2d(a,2,2)+z*get2d(a,3,2)))
ifelse(eval(cv>mv),1,
   `define(`mv',cv)`'define(`best',(x,y,z))',
   `ifelse(cv,mv,`define(`best',best (x,y,z))')')
      ')')
divert
mv best
```

Output:

```txt
54500 (0,15,11) (3,10,11) (6,5,11) (9,0,11)
```



## Lua


```lua
items = {   ["panaea"] = { ["value"] = 3000, ["weight"] = 0.3, ["volume"] = 0.025 },
            ["ichor"]  = { ["value"] = 1800, ["weight"] = 0.2, ["volume"] = 0.015 },
            ["gold"]   = { ["value"] = 2500, ["weight"] = 2.0, ["volume"] = 0.002 }
        }

max_weight = 25
max_volume = 0.25

max_num_items = {}
for i in pairs( items ) do
   max_num_items[i] = math.floor( math.min( max_weight / items[i].weight, max_volume / items[i].volume ) )
end

best = { ["value"] = 0.0, ["weight"] = 0.0, ["volume"] = 0.0 }
best_amounts = {}

for i = 1, max_num_items["panaea"] do
    for j = 1, max_num_items["ichor"] do
        for k = 1, max_num_items["gold"] do
            current = { ["value"]  = i*items["panaea"]["value"] + j*items["ichor"]["value"] + k*items["gold"]["value"],
                        ["weight"] = i*items["panaea"]["weight"] + j*items["ichor"]["weight"] + k*items["gold"]["weight"],
                        ["volume"] = i*items["panaea"]["volume"] + j*items["ichor"]["volume"] + k*items["gold"]["volume"]
                      }

            if current.value > best.value and current.weight <= max_weight and current.volume <= max_volume then
                best = { ["value"] = current.value, ["weight"] = current.weight, ["volume"] = current.volume }
                best_amounts = { ["panaea"] = i, ["ichor"] = j, ["gold"] = k }
            end
        end
    end
end

print( "Maximum value:", best.value )
for k, v in pairs( best_amounts ) do
    print( k, v )
end
```



## Mathematica

Brute force algorithm:

```Mathematica
{pva,pwe,pvo}={3000,3/10,1/40};
{iva,iwe,ivo}={1800,2/10,3/200};
{gva,gwe,gvo}={2500,2,2/1000};
wemax=25;
vomax=1/4;
{pmax,imax,gmax}=Floor/@{Min[vomax/pvo,wemax/pwe],Min[vomax/ivo,wemax/iwe],Min[vomax/gvo,wemax/gwe]};

data=Flatten[Table[{{p,i,g}.{pva,iva,gva},{p,i,g}.{pwe,iwe,gwe},{p,i,g}.{pvo,ivo,gvo},{p,i,g}},{p,0,pmax},{i,0,imax},{g,0,gmax}],2];
data=Select[data,#[[2]]<=25&&#[[3]]<=1/4&];
First[SplitBy[Sort[data,First[#1]>First[#2]&],First]]
```

gives back an array of the best solution(s), with each element being value, weight, volume, {number of vials, number of ampules, number of bars}:

```Mathematica
{{54500,247/10,247/1000,{9,0,11}},{54500,124/5,247/1000,{6,5,11}},{54500,249/10,247/1000,{3,10,11}},{54500,25,247/1000,{0,15,11}}}
```

if we call the three items by their first letters the best packings are:

```Mathematica
p:9 i:0 v:11
p:6 i:5 v:11
p:3 i:10 v:11
p:0 i:15 v:11
```

The volume for all of those is the same, the 'best' solution would be the one that has the least weight: that would the first solution.


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
param volume{t in Items};

var take{t in Items}, integer, >=0;

knap_weight : sum{t in Items} take[t] * weight[t] <= 25;
knap_vol    : sum{t in Items} take[t] * volume[t] <= 0.25;

maximize knap_value: sum{t in Items} take[t] * value[t];

data;

param : Items          : weight   value     volume :=
         panacea          0.3     3000      0.025
         ichor            0.2	  1800	    0.015
         gold		  2.0	  2500      0.002
;

end;
```

The solution produced is at [[Knapsack problem/Unbounded/Mathprog]].

=={{header|Modula-3}}==
{{trans|Fortran}}
Note that unlike [[Fortran]] and [[C]], Modula-3 does not do any hidden casting, which is why <tt>FLOAT</tt> and <tt>FLOOR</tt> are used.

```modula3
MODULE Knapsack EXPORTS Main;

FROM IO IMPORT Put;
FROM Fmt IMPORT Int, Real;

TYPE Bounty = RECORD
  value: INTEGER;
  weight, volume: REAL;
END;

VAR totalWeight, totalVolume: REAL;
    maxPanacea, maxIchor, maxGold, maxValue: INTEGER := 0;
    n: ARRAY [1..3] OF INTEGER;
    panacea, ichor, gold, sack, current: Bounty;

BEGIN
  panacea := Bounty{3000, 0.3, 0.025};
  ichor   := Bounty{1800, 0.2, 0.015};
  gold    := Bounty{2500, 2.0, 0.002};
  sack    := Bounty{0, 25.0, 0.25};

  maxPanacea := FLOOR(MIN(sack.weight / panacea.weight, sack.volume / panacea.volume));
  maxIchor := FLOOR(MIN(sack.weight / ichor.weight, sack.volume / ichor.volume));
  maxGold := FLOOR(MIN(sack.weight / gold.weight, sack.volume / gold.volume));

  FOR i := 0 TO maxPanacea DO
    FOR j := 0 TO maxIchor DO
      FOR k := 0 TO maxGold DO
        current.value := k * gold.value + j * ichor.value + i * panacea.value;
        current.weight := FLOAT(k) * gold.weight + FLOAT(j) * ichor.weight + FLOAT(i) * panacea.weight;
        current.volume := FLOAT(k) * gold.volume + FLOAT(j) * ichor.volume + FLOAT(i) * panacea.volume;
        IF current.weight > sack.weight OR current.volume > sack.volume THEN
          EXIT;
        END;
        IF current.value > maxValue THEN
          maxValue := current.value;
          totalWeight := current.weight;
          totalVolume := current.volume;
          n[1] := i; n[2] := j; n[3] := k;
        END;
      END;
    END;
  END;
  Put("Maximum value achievable is " & Int(maxValue) & "\n");
  Put("This is achieved by carrying " & Int(n[1]) & " panacea, " & Int(n[2]) & " ichor and " & Int(n[3]) & " gold items\n");
  Put("The weight of this carry is " & Real(totalWeight) & " and the volume used is " & Real(totalVolume) & "\n");
END Knapsack.
```

Output:

```txt
Maximum value achievable is 54500
This is achieved by carrying 0 panacea, 15 ichor and 11 gold items
The weight of this carry is 25 and the volume used is 0.247
```



## OCaml

This is a brute-force solution: it generates a list of every legal combination of items and then finds the best results:

```ocaml
type bounty = { name:string; value:int; weight:float; volume:float }

let bounty n d w v = { name = n; value = d; weight = w; volume = v }

let items =
  [ bounty "panacea"  3000  0.3  0.025;
    bounty "ichor"    1800  0.2  0.015;
    bounty "gold"     2500  2.0  0.002; ]

let max_wgt = 25.0 and max_vol = 0.25

let itmax =
  let f it =
    let rec aux n =
      if float n *. it.weight >= max_wgt
      || float n *. it.volume >= max_vol
      then (n)
      else aux (succ n)
    in
    aux 0
  in
  List.map f items

let mklist n m =
  let rec aux i acc =
    if i > m then (List.rev acc)
    else aux (succ i) (i::acc)
  in
  aux n []

let comb_items = List.map (mklist 0) itmax

let combs ll =
  let f hd acc =
    List.concat
      (List.map (fun l -> List.map (fun v -> (v::l)) hd) acc)
  in
  List.fold_right f ll [[]]

let possibles = combs comb_items

let packs =
  let f l =
    let g (v, wgt, vol) n it =
      (v + n * it.value,
       wgt +. float n *. it.weight,
       vol +. float n *. it.volume)
    in
    List.fold_left2 g (0, 0.0, 0.0) l items
  in
  List.map f possibles

let packs = List.combine packs possibles

let results =
  let f (_, wgt, vol) = (wgt <= max_wgt && vol <= max_vol) in
  List.filter (fun v -> f(fst v)) packs

let best_results =
  let max_value = List.fold_left (fun v1 ((v2,_,_),_) -> max v1 v2) 0 results in
  List.filter (fun ((v,_,_),_) -> v = max_value) results

let items_name = List.map (fun it -> it.name) items

let print ((v, wgt, vol), ns) =
  Printf.printf "\
    Maximum value: %d \n \
    Total weight:  %g \n \
    Total volume:  %g \n \
    Containing:    " v wgt vol;
  let f n name = string_of_int n ^ " " ^ name in
  let ss = List.map2 f ns items_name in
  print_endline(String.concat ", " ss);
  print_newline()

let () = List.iter print best_results
```


outputs:

```txt
Maximum value: 54500
 Total weight:  24.7
 Total volume:  0.247
 Containing:    9 panacea, 0 ichor, 11 gold

Maximum value: 54500
 Total weight:  24.8
 Total volume:  0.247
 Containing:    6 panacea, 5 ichor, 11 gold

Maximum value: 54500
 Total weight:  24.9
 Total volume:  0.247
 Containing:    3 panacea, 10 ichor, 11 gold

Maximum value: 54500
 Total weight:  25
 Total volume:  0.247
 Containing:    0 panacea, 15 ichor, 11 gold
```



## OOCalc

OpenOffice.org Calc has (several) linear solvers. To solve this task, first copy in the table from the task description, then add the extra columns:
*  Number: (How many chosen, n)
*  value of n
*  weight of n
*  volume of n
Add a TOTALS row to sum the value/weight/volume of n.

The sheet should then look like this:
:[[File:Unbounded Knapsack problem fomulae.PNG|center|table pre solving]]

Open the "Tools->Solver..." menu item and fill in the following items:
:[[File:Unbounded Knapsack problem solver.PNG|center|solver menu]]
*  Options... (opens a separate popup window, then continue)
:[[File:Unbounded Knapsack problem solve optionsr.PNG|center|solver popup options menu]]

OK the solver options window leaving the Solver window open, then select solve
to produce in seconds:
:[[File:Unbounded Knapsack problem result.PNG|center|Table solved]]


## Oz

Using constraint propagation and branch and bound search:

```oz
declare
  proc {Knapsack Sol}
     solution(panacea:P = {FD.decl}
              ichor:  I = {FD.decl}
              gold:   G = {FD.decl} ) = Sol
  in
                                           {Show 0#Sol}
      3 * P +  2 * I + 20 * G =<: 250      {Show 1#Sol}
     25 * P + 15 * I +  2 * G =<: 250      {Show 2#Sol}
     {FD.distribute naive Sol}             {Show d#Sol}
  end

  fun {Value solution(panacea:P ichor:I gold:G)}
     3000 * P + 1800 * I + 2500 * G
  end

  {System.showInfo "Search:"}
  [Best] = {SearchBest Knapsack proc {$ Old New}
                                   {Value Old} <: {Value New}
                                end}
in
  {System.showInfo "\nResult:"}
  {Show Best}
  {System.showInfo "total value: "#{Value Best}}
```


If you study the output, you see how the weight and volume equations automagically constrain the domain of the three variables.
Afterwards SearchBest only has to evaluate 38 different combinations to find an optimal solution:


```txt
Search:
0#solution(gold:_{0#134217726} ichor:_{0#134217726} panacea:_{0#134217726})
1#solution(gold:_{0#12} ichor:_{0#125} panacea:_{0#83})
2#solution(gold:_{0#12} ichor:_{0#16} panacea:_{0#10})
d#solution(gold:0 ichor:0 panacea:0)
d#solution(gold:0 ichor:1 panacea:0)
d#solution(gold:0 ichor:2 panacea:0)
d#solution(gold:0 ichor:3 panacea:0)
d#solution(gold:0 ichor:4 panacea:0)
d#solution(gold:0 ichor:5 panacea:0)
d#solution(gold:0 ichor:6 panacea:0)
d#solution(gold:0 ichor:7 panacea:0)
d#solution(gold:0 ichor:8 panacea:0)
d#solution(gold:0 ichor:9 panacea:0)
d#solution(gold:0 ichor:10 panacea:0)
d#solution(gold:0 ichor:11 panacea:0)
d#solution(gold:0 ichor:12 panacea:0)
d#solution(gold:0 ichor:13 panacea:0)
d#solution(gold:0 ichor:14 panacea:0)
d#solution(gold:0 ichor:15 panacea:0)
d#solution(gold:0 ichor:16 panacea:0)
d#solution(gold:1 ichor:15 panacea:0)
d#solution(gold:1 ichor:16 panacea:0)
d#solution(gold:2 ichor:15 panacea:0)
d#solution(gold:2 ichor:16 panacea:0)
d#solution(gold:3 ichor:15 panacea:0)
d#solution(gold:3 ichor:16 panacea:0)
d#solution(gold:4 ichor:15 panacea:0)
d#solution(gold:4 ichor:16 panacea:0)
d#solution(gold:5 ichor:15 panacea:0)
d#solution(gold:5 ichor:16 panacea:0)
d#solution(gold:6 ichor:15 panacea:0)
d#solution(gold:7 ichor:14 panacea:0)
d#solution(gold:7 ichor:15 panacea:0)
d#solution(gold:8 ichor:14 panacea:0)
d#solution(gold:8 ichor:15 panacea:0)
d#solution(gold:9 ichor:14 panacea:0)
d#solution(gold:9 ichor:15 panacea:0)
d#solution(gold:10 ichor:14 panacea:0)
d#solution(gold:10 ichor:15 panacea:0)
d#solution(gold:11 ichor:14 panacea:0)
d#solution(gold:11 ichor:15 panacea:0)

Result:
solution(gold:11 ichor:15 panacea:0)
total value: 54500

```



## Pascal

With ideas from C, Fortran and Modula-3.

```pascal
Program Knapsack(output);

uses
  math;

type
  bounty = record
    value: longint;
    weight, volume: real;
  end;

const
  panacea: bounty = (value:3000; weight:  0.3; volume: 0.025);
  ichor:   bounty = (value:1800; weight:  0.2; volume: 0.015);
  gold:    bounty = (value:2500; weight:  2.0; volume: 0.002);
  sack:    bounty = (value:   0; weight: 25.0; volume: 0.25);

var
  totalweight, totalvolume: real;
  maxpanacea, maxichor, maxgold: longint;
  maxvalue: longint = 0;
  n: array [1..3] of longint;
  current: bounty;
  i, j, k: longint;

begin
  maxpanacea := round(min(sack.weight / panacea.weight, sack.volume / panacea.volume));
  maxichor   := round(min(sack.weight / ichor.weight,   sack.volume / ichor.volume));
  maxgold    := round(min(sack.weight / gold.weight,    sack.volume / gold.volume));

  for i := 0 to maxpanacea do
    for j := 0 to maxichor do
      for k := 0 to maxgold do
      begin
        current.value  := k * gold.value  + j * ichor.value  + i * panacea.value;
        current.weight := k * gold.weight + j * ichor.weight + i * panacea.weight;
        current.volume := k * gold.volume + j * ichor.volume + i * panacea.volume;
        if (current.value > maxvalue)      and
	   (current.weight <= sack.weight) and
           (current.volume <= sack.volume) then
	begin
          maxvalue    := current.value;
          totalweight := current.weight;
          totalvolume := current.volume;
          n[1] := i;
	  n[2] := j;
	  n[3] := k;
        end;
      end;

  writeln ('Maximum value achievable is ', maxValue);
  writeln ('This is achieved by carrying ', n[1], ' panacea, ', n[2], ' ichor and ', n[3], ' gold items');
  writeln ('The weight of this carry is ', totalWeight:6:3, ' and the volume used is ', totalVolume:6:4);
end.
```

Output:

```txt
:> ./Knapsack
Maximum value achievable is 54500
This is achieved by carrying 0 panacea, 15 ichor and 11 gold items
The weight of this carry is 25.000 and the volume used is 0.2470

```



## Perl

Dynamic programming solution.  Before you ask, no, it's actually slower for the given data set.  See the alternate data set.

```perl
my (@names, @val, @weight, @vol, $max_vol, $max_weight, $vsc, $wsc);

if (1) { # change 1 to 0 for different data set
        @names  = qw(panacea    icor    gold);
        @val    = qw(3000       1800    2500);
        @weight = qw(3          2       20  );
        @vol    = qw(25         15      2   );
        $max_weight = 250;
        $max_vol = 250;
        $vsc = 1000;
        $wsc = 10;
} else { # with these numbers cache would have been useful
        @names  = qw(panacea    icor    gold    banana  monkey  );
        @val    = qw(17         11      5       3       34      );
        @weight = qw(14         3       2       2       10      );
        @vol    = qw(3          4       2       1       12      );
        $max_weight = 150;
        $max_vol = 100;
        $vsc = $wsc = 1;
}

my @cache;
my ($hits, $misses) = (0, 0);
sub solu {
        my ($i, $w, $v) = @_;
        return [0, []] if $i < 0;

        if ($cache[$i][$w][$v]) {
                $hits ++;
                return $cache[$i][$w][$v]
        }
        $misses ++;

        my $x = solu($i - 1, $w, $v);

        my ($w1, $v1);
        for (my $t = 1; ; $t++) {
                last if ($w1 = $w - $t * $weight[$i]) < 0;
                last if ($v1 = $v - $t * $vol[$i]) < 0;

                my $y = solu($i - 1, $w1, $v1);

                if ( (my $tmp = $y->[0] + $val[$i] * $t) > $x->[0] ) {
                        $x = [ $tmp, [ @{$y->[1]}, [$i, $t] ] ];
                }
        }

        $cache[$i][$w][$v] = $x
}

my $x = solu($#names, $max_weight, $max_vol);
print   "Max value $x->[0], with:\n",
        "    Item\tQty\tWeight   Vol    Value\n", '-'x 50, "\n";

my ($wtot, $vtot) = (0, 0);
for (@{$x->[1]}) {
        my $i = $_->[0];
        printf  "    $names[$i]:\t% 3d  % 8d% 8g% 8d\n",
                $_->[1],
                $weight[$i] * $_->[1] / $wsc,
                $vol[$i] * $_->[1] / $vsc,
                $val[$i] * $_->[1];

        $wtot += $weight[$i] * $_->[1];
        $vtot += $vol[$i] * $_->[1];
}
print   "-" x 50, "\n";
printf  "    Total:\t     % 8d% 8g% 8d\n",
        $wtot/$wsc, $vtot/$vsc, $x->[0];

print "\nCache hit: $hits\tmiss: $misses\n";
```


Output:
```txt
Max value 54500, with:
    Item        Qty     Weight   Vol    Value
--------------------------------------------------
    panacea:      9         2   0.225   27000
    gold:        11        22   0.022   27500
--------------------------------------------------
    Total:                 24   0.247   54500

Cache hit: 0    miss: 218
```

Cache info is not pertinent to this task, just some info.


## Perl 6

{{Works with|rakudo|2019.03.1}}
Brute force, looked a lot at the Ruby solution.


```perl6
class KnapsackItem {
  has $.volume;
  has $.weight;
  has $.value;
  has $.name;

  method new($volume,$weight,$value,$name) {
    self.bless(:$volume, :$weight, :$value, :$name)
  }
};

my KnapsackItem $panacea .= new: 0.025, 0.3, 3000, "panacea";
my KnapsackItem $ichor   .= new: 0.015, 0.2, 1800, "ichor";
my KnapsackItem $gold    .= new: 0.002, 2.0, 2500, "gold";
my KnapsackItem $maximum .= new: 0.25,  25,  0   , "max";

my $max_val = 0;
my @solutions;
my %max_items;

for $panacea, $ichor, $gold -> $item {
    %max_items{$item.name} = floor min
                            $maximum.volume / $item.volume,
			    $maximum.weight / $item.weight;
}

for 0..%max_items<panacea>
       X 0..%max_items<ichor>
           X 0..%max_items<gold>
 -> ($p, $i, $g)
{
  next if $panacea.volume * $p + $ichor.volume * $i + $gold.volume * $g > $maximum.volume;
  next if $panacea.weight * $p + $ichor.weight * $i + $gold.weight * $g > $maximum.weight;
  given $panacea.value * $p + $ichor.value * $i + $gold.value * $g {
    if $_ > $max_val { $max_val = $_; @solutions = (); }
    when $max_val    { @solutions.push: $[$p,$i,$g] }
  }
}

say "maximum value is $max_val\npossible solutions:";
say "panacea\tichor\tgold";
.join("\t").say for @solutions;
```

'''Output:'''

```txt
maximum value is 54500
possible solutions:
panacea	ichor	gold
0	15	11
3	10	11
6	5	11
9	0	11
```



## Phix

For each goodie, fill yer boots, then (except for the last) recursively try with fewer and fewer.

Increase profit and decrease weight/volume to pick largest profit for least weight and space.

```Phix
--
-- demo\rosetta\knapsack.exw
--
### ===================

--
integer attempts = 0
function knapsack(sequence res, goodies, atom profit, weight, volume, at=1, sequence chosen={})
    atom {pitem,witem,vitem} = goodies[at][2]
    integer n = min(floor(weight/witem),floor(volume/vitem))
    chosen &= n
    profit += n*pitem   -- increase profit
    weight -= n*witem   -- decrease weight left
    volume -= n*vitem   -- decrease space left
    if at=length(goodies) then
        attempts += 1
        if length(res)=0
        or res<{profit,weight,volume} then
            res = {profit,weight,volume,chosen}
        end if
    else
        while n>=0 do
            res = knapsack(res,goodies,profit,weight,volume,at+1,chosen)
            n -= 1
            chosen[$] = n
            profit -= pitem
            weight += witem
            volume += vitem
        end while
    end if
    return res
end function

constant goodies = {
-- item           profit weight volume
{"panacea",      {3000,   0.3, 0.025}},
{"ichor",        {1800,   0.2, 0.015}},
{"shiney shiney",{2500,   2.0, 0.002}}}

sequence res -- {profit,(weight left),(space left),{counts}}
res = knapsack({},goodies,0,25,0.25)
integer {p,i,g} = res[4]
sequence {d,pwv} = columnize(goodies),
         {?,w,v} = columnize(pwv)
atom weight = sum(sq_mul(res[4],w)),
     volume = sum(sq_mul(res[4],v))
printf(1,"Profit %d: %d %s, %d %s, %d %s\n",
        {res[1],p,d[1],i,d[2],g,d[3]})
printf(1," [weight:%g, volume:%g, %d attempts]\n",
        {weight,volume,attempts})
```

{{out}}

```txt

Profit 54500: 9 panacea, 0 ichor, 11 shiney shiney
 [weight:24.7, volume:0.247, 98 attempts]

```

You get the same result whatever order the goodies are in, but with a different number of attempts, gold/ichor/panacea being the
highest at 204.


## PicoLisp

Brute force solution

```PicoLisp
(de *Items
   ("panacea"  3  25  3000)
   ("ichor"    2  15  1800)
   ("gold"    20   2  2500) )

(de knapsack (Lst W V)
   (when Lst
      (let X (knapsack (cdr Lst) W V)
         (if (and (ge0 (dec 'W (cadar Lst))) (ge0 (dec 'V (caddar Lst))))
            (maxi
               '((L) (sum cadddr L))
               (list
                  X
                  (cons (car Lst) (knapsack (cdr Lst) W V))
                  (cons (car Lst) (knapsack Lst W V)) ) )
            X ) ) ) )

(let K (knapsack *Items 250 250)
   (for (L K  L)
      (let (N 1  X)
         (while (= (setq X (pop 'L)) (car L))
            (inc 'N) )
         (apply tab X (4 2 8 5 5 7) N "x") ) )
   (tab (14 5 5 7) NIL (sum cadr K) (sum caddr K) (sum cadddr K)) )
```

Output:

```txt
  15 x   ichor    2   15   1800
  11 x    gold   20    2   2500
                250  247  54500
```



## PowerShell

{{works with|PowerShell|3.0}}

```powershell
#  Define the items to pack
$Item = @(
    [pscustomobject]@{ Name = 'panacea'; Unit = 'vials'  ; value = 3000; Weight = 0.3; Volume = 0.025 }
    [pscustomobject]@{ Name = 'ichor'  ; Unit = 'ampules'; value = 1800; Weight = 0.2; Volume = 0.015 }
    [pscustomobject]@{ Name = 'gold'   ; Unit = 'bars'   ; value = 2500; Weight = 2.0; Volume = 0.002 }
    )

#  Define our maximums
$MaxWeight = 25
$MaxVolume = 0.25

#  Set our default value to beat
$OptimalValue = 0

#  Iterate through the possible quantities of item 0, without going over the weight or volume limit
ForEach ( $Qty0 in 0..( [math]::Min( [math]::Truncate( $MaxWeight / $Item[0].Weight ), [math]::Truncate( $MaxVolume / $Item[0].Volume ) ) ) )
    {
    #  Calculate the remaining space
    $RemainingWeight = $MaxWeight - $Qty0 * $Item[0].Weight
    $RemainingVolume = $MaxVolume - $Qty0 * $Item[0].Volume

    #  Iterate through the possible quantities of item 1, without going over the weight or volume limit
    ForEach ( $Qty1 in 0..( [math]::Min( [math]::Truncate( $RemainingWeight / $Item[1].Weight ), [math]::Truncate( $RemainingVolume / $Item[1].Volume ) ) ) )
        {
        #  Calculate the remaining space
        $RemainingWeight2 = $RemainingWeight - $Qty1 * $Item[1].Weight
        $RemainingVolume2 = $RemainingVolume - $Qty1 * $Item[1].Volume

        #  Calculate the maximum quantity of item 2 for the remaining space, without going over the weight or volume limit
        $Qty2 = [math]::Min( [math]::Truncate( $RemainingWeight2 / $Item[2].Weight ), [math]::Truncate( $RemainingVolume2 / $Item[2].Volume ) )

        #  Calculate the total value of the items packed
        $TrialValue =   $Qty0 * $Item[0].Value +
                        $Qty1 * $Item[1].Value +
                        $Qty2 * $Item[2].Value

        #  Describe the trial solution
        $Solution  = "$Qty0 $($Item[0].Unit) of $($Item[0].Name), "
        $Solution += "$Qty1 $($Item[1].Unit) of $($Item[1].Name), and "
        $Solution += "$Qty2 $($Item[2].Unit) of $($Item[2].Name) worth a total of $TrialValue."

        #  If the trial value is higher than previous most valuable trial...
        If ( $TrialValue -gt $OptimalValue )
            {
            #  Set the new number to beat
            $OptimalValue = $TrialValue

            #  Overwrite the previous optimal solution(s) with the trial solution
            $Solutions  = @( $Solution )
            }

        #  Else if the trial value matches the previous most valuable trial...
       ElseIf ( $TrialValue -eq $OptimalValue )
            {
            #  Add the trial solution to the list of optimal solutions
            $Solutions += @( $Solution )
            }
        }
    }

#  Show the results
$Solutions
```

{{out}}

```txt
0 vials of panacea, 15 ampules of ichor, and 11 bars of gold worth a total of 54500.
3 vials of panacea, 10 ampules of ichor, and 11 bars of gold worth a total of 54500.
6 vials of panacea, 5 ampules of ichor, and 11 bars of gold worth a total of 54500.
9 vials of panacea, 0 ampules of ichor, and 11 bars of gold worth a total of 54500.
```



## Prolog

Works with SWI-Prolog and <b>library simplex</b> written by <b>Markus Triska</b>.

```Prolog
:- use_module(library(simplex)).

% tuples (name, Explantion, Value, weights, volume).
knapsack :-
	L =[(	panacea, 'Incredible healing properties', 3000,	0.3,	0.025),
	    (	ichor,   'Vampires blood',                1800,	0.2,	0.015),
	    (	gold ,	 'Shiney shiney',	          2500,	2.0,	0.002)],

	 gen_state(S0),
	 length(L, N),
	 numlist(1, N, LN),

	 % to get statistics
	 time((create_constraint_N(LN, L, S0, S1, [], LVa, [], LW, [], LVo),
	       constraint(LW =< 25.0, S1, S2),
	       constraint(LVo =< 0.25, S2, S3),
	       maximize(LVa, S3, S4)
	      )),

	% we display the results
	compute_lenword(L, 0, Len),
	sformat(A0, '~~w~~t~~~w|', [3]),
	sformat(A1, '~~w~~t~~~w|', [Len]),
	sformat(A2, '~~t~~w~~~w|', [10]),
	sformat(A3, '~~t~~2f~~~w|', [10]),
	sformat(A4, '~~t~~3f~~~w|', [10]),
	sformat(A33, '~~t~~w~~~w|', [10]),
	sformat(A44, '~~t~~w~~~w|', [10]),

	sformat(W0, A0, ['Nb']),
	sformat(W1, A1, ['Items']),
	sformat(W2, A2, ['Value']),
	sformat(W3, A33, ['Weigth']),
	sformat(W4, A44, ['Volume']),
	format('~w~w~w~w~w~n', [W0, W1,W2,W3,W4]),

	print_results(S4, A0, A1, A2, A3, A4, L, LN, 0, 0, 0).


create_constraint_N([], [], S, S, LVa, LVa, LW, LW, LVo, LVo).

create_constraint_N([HN|TN], [(_, _,Va, W, Vo) | TL], S1, SF, LVa, LVaF, LW, LWF, LVo, LVoF) :-
	constraint(integral(x(HN)), S1, S2),
	constraint([x(HN)] >= 0, S2, S3),
	create_constraint_N(TN, TL, S3, SF,
			    [Va * x(HN) | LVa], LVaF,
			    [W * x(HN) | LW], LWF,
			    [Vo * x(HN) | LVo], LVoF).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
compute_lenword([], N, N).
compute_lenword([(Name, _, _, _, _)|T], N, NF):-
	atom_length(Name, L),
	(   L > N -> N1 = L; N1 = N),
	compute_lenword(T, N1, NF).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
print_results(_S, A0, A1, A2, A3, A4, [], [], VaM, WM, VoM) :-
	sformat(W0, A0, [' ']),
	sformat(W1, A1, [' ']),
	sformat(W2, A2, [VaM]),
	sformat(W3, A3, [WM]),
	sformat(W4, A4, [VoM]),
	format('~w~w~w~w~w~n', [W0, W1,W2,W3,W4]).


print_results(S, A0, A1, A2, A3, A4, [(Name, _, Va, W, Vo)|T], [N|TN], Va1, W1, Vo1) :-
	variable_value(S, x(N), X),
	(   X = 0 -> Va1 = Va2, W1 = W2, Vo1 = Vo2
	;
	    sformat(S0, A0, [X]),
	    sformat(S1, A1, [Name]),
	    Vatemp is X * Va,
	    Wtemp is X * W,
	    Votemp is X * Vo,
	    sformat(S2, A2, [Vatemp]),
	    sformat(S3, A3, [Wtemp]),
	    sformat(S4, A4, [Votemp]),
	    format('~w~w~w~w~w~n', [S0,S1,S2,S3,S4]),
	    Va2 is Va1 + Vatemp,
	    W2 is W1 + Wtemp,
	    Vo2 is Vo1 + Votemp ),
	print_results(S, A0, A1, A2, A3, A4, T, TN, Va2, W2, Vo2).
```

Output :

```txt
 ?- knapsack.
% 145,319 inferences, 0.078 CPU in 0.079 seconds (99% CPU, 1860083 Lips)
Nb Items       Value    Weigth    Volume
15 ichor       27000      3.00     0.225
11 gold        27500     22.00     0.022
               54500     25.00     0.247
true
```



## PureBasic

{{trans|Fortran}}

```PureBasic
Define.f TotalWeight, TotalVolyme
Define.i maxPanacea, maxIchor, maxGold, maxValue
Define.i i, j ,k
Dim n.i(2)

Enumeration
  #Panacea
  #Ichor
  #Gold
  #Sack
  #Current
EndEnumeration

Structure Bounty
  value.i
  weight.f
  volyme.f
EndStructure

Dim Item.Bounty(4)
CopyMemory(?panacea,@Item(#Panacea),SizeOf(Bounty))
CopyMemory(?ichor,  @Item(#Ichor),  SizeOf(Bounty))
CopyMemory(?gold,   @Item(#gold),   SizeOf(Bounty))
CopyMemory(?sack,   @Item(#Sack),   SizeOf(Bounty))

Procedure.f min(a.f, b.f)
  If a<b
    ProcedureReturn a
  Else
    ProcedureReturn b
  EndIf
EndProcedure

maxPanacea=min(Item(#Sack)\weight/Item(#Panacea)\weight,Item(#Sack)\volyme/Item(#Panacea)\volyme)
maxIchor  =min(Item(#Sack)\weight/Item(#Ichor)\weight,  Item(#Sack)\volyme/Item(#Ichor)\volyme)
maxGold   =min(Item(#Sack)\weight/Item(#Gold)\weight,   Item(#Sack)\volyme/Item(#Gold)\volyme)

For i=0 To maxPanacea
  For j=0 To maxIchor
    For k=0 To maxGold
      Item(#Current)\value=k*Item(#Gold)\value  +j*item(#Ichor)\value +i*item(#Panacea)\value
      Item(#Current)\weight=k*Item(#Gold)\weight+j*Item(#Ichor)\weight+i*Item(#Panacea)\weight
      Item(#Current)\volyme=k*Item(#Gold)\volyme+j*Item(#Ichor)\volyme+i*Item(#Panacea)\volyme
      If Item(#Current)\weight>Item(#Sack)\weight Or Item(#Current)\volyme>Item(#Sack)\volyme
        Continue
      EndIf
      If Item(#Current)\value>maxValue
        maxValue=Item(#Current)\value
        TotalWeight=Item(#Current)\weight
        TotalVolyme=Item(#Current)\volyme
        n(#Panacea)=i: n(#Ichor)=j: n(#Gold)=k
      EndIf
    Next k
  Next j
Next i

If OpenConsole()
  Define txt$
  txt$="Maximum value achievable is "+Str(maxValue)+#CRLF$
  txt$+"This is achieved by carrying "+Str(n(#Panacea))+" panacea, "
  txt$+Str(n(#Ichor))+" ichor and "+Str(n(#Gold))+" gold items."+#CRLF$
  txt$+"The weight to carry is "+StrF(totalWeight,2)
  txt$+" and the volume used is "+StrF(TotalVolyme,2)
  PrintN(txt$)

  Print(#CRLF$+"Press Enter to quit"): Input()
EndIf

DataSection
panacea:
  Data.i 3000
  Data.f 0.3, 0.025
ichor:
  Data.i 1800
  Data.f 0.2, 0.015
gold:
  Data.i 2500
  Data.f 2.0, 0.002
sack:
  Data.i  0
  Data.f  25.0, 0.25
EndDataSection
```


Outputs
<tt>
 Maximum value achievable is 54500
 This is achieved by carrying 0 panacea, 15 ichor and 11 gold items
 The weight to carry is 25.00 and the volume used is 0.25

 Press Enter to quit</tt>


## Python

See [[Knapsack Problem/Python]]


## R

Brute force method

```r
# Define consts
weights <- c(panacea=0.3, ichor=0.2, gold=2.0)
volumes <- c(panacea=0.025, ichor=0.015, gold=0.002)
values <- c(panacea=3000, ichor=1800, gold=2500)
sack.weight <- 25
sack.volume <- 0.25
max.items <- floor(pmin(sack.weight/weights, sack.volume/volumes))

# Some utility functions
getTotalValue <- function(n) sum(n*values)
getTotalWeight <- function(n) sum(n*weights)
getTotalVolume <- function(n) sum(n*volumes)
willFitInSack <- function(n) getTotalWeight(n) <= sack.weight && getTotalVolume(n) <= sack.volume

# Find all possible combination, then eliminate those that won't fit in the sack
knapsack <- expand.grid(lapply(max.items, function(n) seq.int(0, n)))
ok <- apply(knapsack, 1, willFitInSack)
knapok <- knapsack[ok,]

# Find the solutions with the highest value
vals <- apply(knapok, 1, getTotalValue)
knapok[vals == max(vals),]
```

      panacea ichor gold
 2067       9     0   11
 2119       6     5   11
 2171       3    10   11
 2223       0    15   11


Using Dynamic Programming


```r

Data_<-structure(list(item = c("Panacea", "Ichor", "Gold"), value = c(3000,
1800, 2500), weight = c(3, 2, 20), volume = c(25, 15, 2)), .Names = c("item",
"value", "weight", "volume"), row.names = c(NA, 3L), class = "data.frame")

knapsack_volume<-function(Data, W, Volume, full_K)
{

	# Data must have the colums with names: item, value, weight and volume.
	K<-list() # hightest values
	K_item<-list() # itens that reach the hightest value
	K<-rep(0,W+1) # The position '0'
	K_item<-rep('',W+1) # The position '0'
	for(w in 1:W)
	{
		temp_w<-0
		temp_item<-''
		temp_value<-0
		for(i in 1:dim(Data)[1]) # each row
		{
			wi<-Data$weight[i] # item i
			vi<- Data$value[i]
			item<-Data$item[i]
			volume_i<-Data$volume[i]
			if(wi<=w & volume_i <= Volume)
			{
				back<- full_K[[Volume-volume_i+1]][w-wi+1]
				temp_wi<-vi + back

				if(temp_w < temp_wi)
				{
					temp_value<-temp_wi
					temp_w<-temp_wi
					temp_item <- item
				}
			}
		}
	K[[w+1]]<-temp_value
	K_item[[w+1]]<-temp_item
	}
return(list(K=K,Item=K_item))
}


Un_knapsack<-function(Data,W,V)
{
	K<-list();K_item<-list()
	K[[1]]<-rep(0,W+1) #the line 0
	K_item[[1]]<-rep('', W+1) #the line 0
	for(v in 1:V)
	{
		best_volum_v<-knapsack_volume(Data, W, v, K)
		K[[v+1]]<-best_volum_v$K
		K_item[[v+1]]<-best_volum_v$Item
	}

return(list(K=data.frame(K),Item=data.frame(K_item,stringsAsFactors=F)))
}

retrieve_info<-function(knapsack, Data)
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
			W <- W - selected_item_value$weight
			itens<-c(itens,selected_item)
			col <- col - selected_item_value$volume
		}
	}
return(itens)
}

main_knapsack<-function(Data, W, Volume)
{
	knapsack_result<-Un_knapsack(Data,W,Volume)
	items<-table(retrieve_info(knapsack_result, Data))
	K<-knapsack_result$K[W+1, Volume+1]
	cat(paste('The Total profit is: ', K, '\n'))
	cat(paste('You must carry:', names(items), '(x',items, ') \n'))
}

main_knapsack(Data_, 250, 250)

```


```r

Output:
The Total profit is:  54500
You must carry: Gold (x 11 )
You must carry: Panacea (x 9 )

```



## Racket



```racket

#lang racket

(struct item (name explanation value weight volume) #:prefab)

(define items
  (list
   (item "panacea (vials of)" "Incredible healing properties" 3000 0.3 0.025)
   (item "ichor (ampules of)" "Vampires blood"                1800 0.2 0.015)
   (item "gold (bars)"        "Shiney shiney"                 2500 2.0 0.002)))

(define (fill-sack items volume-left weight-left sack sack-value)
  (match items
    ['() (values (list sack) sack-value)]
    [(cons (and (item _ _ item-val weight volume) item) items)
     (define max-q-wgt (floor (/ weight-left weight)))
     (define max-q-vol (floor (/ volume-left volume)))
     (for/fold ([best (list sack)] [best-val sack-value])
               ([n (exact-round (add1 (min max-q-vol max-q-wgt)))])
       (define-values [best* best-val*]
         (fill-sack items
                    (- volume-left (* n volume))
                    (- weight-left (* n weight))
                    (cons (cons n item) sack)
                    (+ sack-value (* n item-val))))
       (cond [(> best-val* best-val) (values best* best-val*)]
             [(= best-val* best-val) (values (append best best*) best-val*)]
             [else                   (values best best-val)]))]))

(define (display-sack sack total)
  (for ([sk sack])
    (define qty (car sk))
    (define name (item-name (cdr sk)))
    (if (zero? qty)
      (printf "Leave ~a\n" name)
      (printf "Take ~a ~a\n" qty name)))
  (printf "GRAND TOTAL: ~a\n\n" total))

(call-with-values (λ() (fill-sack items 0.25 25 '() 0))
                  (λ(sacks total) (for ([s sacks]) (display-sack s total))))

```


{{out}}

```txt
Take 11 gold (bars)
Take 15 ichor (ampules of)
Leave panacea (vials of)
GRAND TOTAL: 54500

Take 11 gold (bars)
Take 10 ichor (ampules of)
Take 3 panacea (vials of)
GRAND TOTAL: 54500

Take 11 gold (bars)
Take 5 ichor (ampules of)
Take 6 panacea (vials of)
GRAND TOTAL: 54500

Take 11 gold (bars)
Leave ichor (ampules of)
Take 9 panacea (vials of)
GRAND TOTAL: 54500
```



## REXX

{{trans|Fortran}}

### displays 1st solution


```rexx
/*REXX program solves the knapsack/unbounded problem: highest value, weight, and volume.*/
                        /*   value                weight               volume */
maxPanacea=0            /*  ═══════               ══════               ══════ */
maxIchor  =0;    panacea.$ = 3000  ;   panacea.w =  0.3 ;   panacea.v = 0.025
maxGold   =0;      ichor.$ = 1800  ;     ichor.w =  0.2 ;     ichor.v = 0.015
max$      =0;       gold.$ = 2500  ;      gold.w =  2   ;      gold.v = 0.002
now.      =0;       sack.$ =    0  ;      sack.w = 25   ;      sack.v = 0.25

maxPanacea= min(sack.w / panacea.w,     sack.v / panacea.v)
maxIchor  = min(sack.w /   ichor.w,     sack.v /   ichor.v)
maxGold   = min(sack.w /    gold.w,     sack.v /    gold.v)

  do     p=0  to maxPanacea
    do   i=0  to maxIchor
      do g=0  to maxGold
      now.$= g * gold.$     +     i * ichor.$     +     p * panacea.$
      now.w= g * gold.w     +     i * ichor.w     +     p * panacea.w
      now.v= g * gold.v     +     i * ichor.v     +     p * panacea.v
      if now.w > sack.w  | now.v  > sack.v  then iterate
      if now.$ > max$  then do;   maxP=p;        maxI=i;         maxG=g
                                  max$=now.$;    maxW=now.w;     maxV=now.v
                            end
      end  /*g  (gold)   */
    end    /*i  (ichor)  */
  end      /*p  (panacea)*/

Ctot = maxP + maxI + maxG;               L = length(Ctot) + 1
say '    panacea in sack:'   right(maxP, L)
say '     ichors in sack:'   right(maxI, L)
say ' gold items in sack:'   right(maxG, L)
say '════════════════════'   copies("═", L)
say 'carrying a total of:'   right(cTot, L)
                           say left('', 40)     "total  value: "        max$ / 1
                           say left('', 40)     "total weight: "        maxW / 1
                           say left('', 40)     "total volume: "        maxV / 1
                                                 /*stick a fork in it,  we're all done. */
```

'''output'''

```txt

    panacea in sack:   0
     ichors in sack:  15
 gold items in sack:  11
════════════════════ ═══
carrying a total of:  26
                                         total  value:  54500
                                         total weight:  25
                                         total volume:  0.247

```



### displays all solutions


```rexx
/*REXX program solves the knapsack/unbounded problem: highest value, weight, and volume.*/

maxPanacea=0             /*   value               weight               volume */
maxIchor  =0             /*  ═══════              ═══════              ══════ */
maxGold   =0;     panacea.$ = 3000  ;  panacea.w =  0.3  ;  panacea.v = 0.025
max$      =0;       ichor.$ = 1800  ;    ichor.w =  0.2  ;    ichor.v = 0.015
now.      =0;        gold.$ = 2500  ;     gold.w =  2    ;     gold.v = 0.002
#         =0;        sack.$ =    0  ;     sack.w = 25    ;     sack.v = 0.25
L         =0
maxPanacea= min(sack.w / panacea.w,     sack.v / panacea.v)
maxIchor  = min(sack.w /   ichor.w,     sack.v /   ichor.v)
maxGold   = min(sack.w /    gold.w,     sack.v /    gold.v)

  do     p=0  to maxPanacea
    do   i=0  to maxIchor
      do g=0  to maxGold
      now.$ = g * gold.$     +     i * ichor.$     +     p * panacea.$
      now.w = g * gold.w     +     i * ichor.w     +     p * panacea.w
      now.v = g * gold.v     +     i * ichor.v     +     p * panacea.v
      if now.w > sack.w  | now.v  > sack.v  then iterate i
      if now.$ > max$  then do;  #=0;           max$=now.$;    end
      if now.$ = max$  then do;  #=#+1;         maxP.#=p;      maxI.#=i;    maxG.#=g
                                 max$.#=now.$;  maxW.#=now.w;  maxV.#=now.v
                                 L=max(L, length(p + i + g) )
                            end
      end  /*g  (gold)   */
    end    /*i  (ichor)  */
  end      /*p  (panacea)*/
                                                L=L + 1
     do j=1  for #;      say;     say copies('▒', 70)                "solution"  j
     say '    panacea in sack:'   right(maxP.j, L)
     say '     ichors in sack:'   right(maxI.j, L)
     say ' gold items in sack:'   right(maxG.j, L)
     say '════════════════════'   copies("═",   L)
     say 'carrying a total of:'   right(maxP.j + maxI.j + maxG.j, L)
                         say left('', 40)     "total  value: "        max$.j / 1
                         say left('', 40)     "total weight: "        maxW.j / 1
                         say left('', 40)     "total volume: "        maxV.j / 1
     end  /*j*/
                                                 /*stick a fork in it,  we're all done. */
```

'''output'''

```txt

▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒ solution 1
    panacea in sack:   0
     ichors in sack:  15
 gold items in sack:  11
════════════════════ ═══
carrying a total of:  26
                                         total  value:  54500
                                         total weight:  25
                                         total volume:  0.247

▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒ solution 2
    panacea in sack:   3
     ichors in sack:  10
 gold items in sack:  11
════════════════════ ═══
carrying a total of:  24
                                         total  value:  54500
                                         total weight:  24.9
                                         total volume:  0.247

▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒ solution 3
    panacea in sack:   6
     ichors in sack:   5
 gold items in sack:  11
════════════════════ ═══
carrying a total of:  22
                                         total  value:  54500
                                         total weight:  24.8
                                         total volume:  0.247

▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒ solution 4
    panacea in sack:   9
     ichors in sack:   0
 gold items in sack:  11
════════════════════ ═══
carrying a total of:  20
                                         total  value:  54500
                                         total weight:  24.7
                                         total volume:  0.247

```



## Ruby

Brute force method, {{trans|Tcl}}

```ruby
KnapsackItem = Struct.new(:volume, :weight, :value)
panacea = KnapsackItem.new(0.025, 0.3, 3000)
ichor   = KnapsackItem.new(0.015, 0.2, 1800)
gold    = KnapsackItem.new(0.002, 2.0, 2500)
maximum = KnapsackItem.new(0.25,  25,  0)

max_items = {}
for item in [panacea, ichor, gold]
  max_items[item] = [(maximum.volume/item.volume).to_i, (maximum.weight/item.weight).to_i].min
end

maxval = 0
solutions = []

0.upto(max_items[ichor]) do |i|
  0.upto(max_items[panacea]) do |p|
    0.upto(max_items[gold]) do |g|
      break if i*ichor.weight + p*panacea.weight + g*gold.weight > maximum.weight
      break if i*ichor.volume + p*panacea.volume + g*gold.volume > maximum.volume
      val = i*ichor.value + p*panacea.value + g*gold.value
      if val > maxval
        maxval = val
        solutions = [[i, p, g]]
      elsif val == maxval
        solutions << [i, p, g]
      end
    end
  end
end

puts "The maximal solution has value #{maxval}"
solutions.each do |i, p, g|
  printf "  ichor=%2d, panacea=%2d, gold=%2d -- weight:%.1f, volume=%.3f\n",
    i, p, g,
    i*ichor.weight + p*panacea.weight + g*gold.weight,
    i*ichor.volume + p*panacea.volume + g*gold.volume
end
```

{{out}}

```txt

The maximal solution has value 54500
  ichor= 0, panacea= 9, gold=11 -- weight:24.7, volume=0.247
  ichor= 5, panacea= 6, gold=11 -- weight:24.8, volume=0.247
  ichor=10, panacea= 3, gold=11 -- weight:24.9, volume=0.247
  ichor=15, panacea= 0, gold=11 -- weight:25.0, volume=0.247
```



## SAS

This is yet another brute force solution.

```SAS
data one;
   wtpanacea=0.3;    wtichor=0.2;    wtgold=2.0;
   volpanacea=0.025; volichor=0.015; volgold=0.002;
   valpanacea=3000;  valichor=1800;  valgold=2500;
   maxwt=25; maxvol=0.25;

   /* we can prune the possible selections */
   maxpanacea = floor(min(maxwt/wtpanacea, maxvol/volpanacea));
   maxichor = floor(min(maxwt/wtichor, maxvol/volichor));
   maxgold = floor(min(maxwt/wtgold, maxvol/volgold));
   do i1 = 0 to maxpanacea;
      do i2 = 0 to maxichor;
         do i3 = 0 to maxgold;
            panacea = i1; ichor=i2; gold=i3; output;
         end;
      end;
   end;
run;
data one; set one;
   vals = valpanacea*panacea + valichor*ichor + valgold*gold;
   totalweight = wtpanacea*panacea + wtichor*ichor + wtgold*gold;
   totalvolume = volpanacea*panacea + volichor*ichor + volgold*gold;
   if (totalweight le maxwt) and (totalvolume le maxvol);
run;
proc sort data=one;
   by descending vals;
run;
proc print data=one (obs=4);
   var panacea ichor gold vals;
run;
```

Output:

```txt

 Obs    panacea    ichor    gold     vals

   1       0         15      11     54500
   2       3         10      11     54500
   3       6          5      11     54500
   4       9          0      11     54500

```


Use SAS/OR:

```sas
/* create SAS data set */
data mydata;
   input Item $1-19 Value weight Volume;
   datalines;
panacea (vials of) 3000 0.3 0.025
ichor (ampules of) 1800 0.2 0.015
gold (bars)        2500 2.0 0.002
;

/* call OPTMODEL procedure in SAS/OR */
proc optmodel;
   /* declare sets and parameters, and read input data */
   set <str> ITEMS;
   num value {ITEMS};
   num weight {ITEMS};
   num volume {ITEMS};
   read data mydata into ITEMS=[item] value weight volume;

   /* declare variables, objective, and constraints */
   var NumSelected {ITEMS} >= 0 integer;
   max TotalValue = sum {i in ITEMS} value[i] * NumSelected[i];
   con WeightCon:
      sum {i in ITEMS} weight[i] * NumSelected[i] <= 25;
   con VolumeCon:
      sum {i in ITEMS} volume[i] * NumSelected[i] <= 0.25;

   /* call mixed integer linear programming (MILP) solver */
   solve;

   /* print optimal solution */
   print TotalValue;
   print NumSelected;

   /* to get all optimal solutions, call CLP solver instead */
   solve with CLP / findallsolns;

   /* print all optimal solutions */
   print TotalValue;
   for {s in 1.._NSOL_} print {i in ITEMS} NumSelected[i].sol[s];
quit;
```


MILP solver output:

```txt
TotalValue
54500

[1] NumSelected
gold (bars) 11
ichor (ampules of) 0
panacea (vials of) 9

```


CLP solver output:

```txt

TotalValue
54500

[1]
gold (bars) 11
ichor (ampules of) 15
panacea (vials of) 0

[1]
gold (bars) 11
ichor (ampules of) 10
panacea (vials of) 3

[1]
gold (bars) 11
ichor (ampules of) 5
panacea (vials of) 6

[1]
gold (bars) 11
ichor (ampules of) 0
panacea (vials of) 9
```


## Scala

===Functional approach (Tail recursive)===

```scala
import scala.annotation.tailrec

object UnboundedKnapsack extends App {
  private val (maxWeight, maxVolume) = (BigDecimal(25.0), BigDecimal(0.25))
  private val items = Seq(Item("panacea", 3000, 0.3, 0.025), Item("ichor", 1800, 0.2, 0.015), Item("gold", 2500, 2.0, 0.002))

  @tailrec
  private def packer(notPacked: Seq[Knapsack], packed: Seq[Knapsack]): Seq[Knapsack] = {
    def fill(knapsack: Knapsack): Seq[Knapsack] = items.map(i => Knapsack(i +: knapsack.bagged))

    def stuffer(Seq: Seq[Knapsack]): Seq[Knapsack] = // Cause brute force
      Seq.map(k => Knapsack(k.bagged.sortBy(_.name))).distinct

    if (notPacked.isEmpty) packed.sortBy(-_.totValue).take(4)
    else packer(stuffer(notPacked.flatMap(fill)).filter(_.isNotFull), notPacked ++ packed)
  }

  private case class Item(name: String, value: Int, weight: BigDecimal, volume: BigDecimal)

  private case class Knapsack(bagged: Seq[Item]) {
    def isNotFull: Boolean = totWeight <= maxWeight && totVolume <= maxVolume

    override def toString = s"[${show(bagged)} | value: $totValue, weight: $totWeight, volume: $totVolume]"

    def totValue: Int = bagged.map(_.value).sum

    private def totVolume = bagged.map(_.volume).sum

    private def totWeight = bagged.map(_.weight).sum

    private def show(is: Seq[Item]) =
      (items.map(_.name) zip items.map(i => is.count(_ == i)))
        .map { case (i, c) => f"$i:$c%3d" }
        .mkString(", ")
  }

  packer(items.map(i => Knapsack(Seq(i))), Nil).foreach(println)
}
```


{{out}}

```txt

[panacea:  0, ichor: 15, gold: 11 | value: 54500, weight: 25.0, volume: 0.247]
[panacea:  3, ichor: 10, gold: 11 | value: 54500, weight: 24.9, volume: 0.247]
[panacea:  6, ichor:  5, gold: 11 | value: 54500, weight: 24.8, volume: 0.247]
[panacea:  9, ichor:  0, gold: 11 | value: 54500, weight: 24.7, volume: 0.247]

```

{{Out}}See it in running in your browser by [https://scalafiddle.io/sf/1fRBQs5/2 ScalaFiddle (JavaScript)] or by [https://scastie.scala-lang.org/S4oGElcwQkCMesfuSRKfkw Scastie (JVM)].

## Seed7


```seed7
$ include "seed7_05.s7i";
  include "float.s7i";

const type: bounty is new struct
    var integer: value is 0;
    var float: weight is 0.0;
    var float: volume is 0.0;
  end struct;

const func bounty: bounty (in integer: value, in float: weight, in float: volume) is func
  result
    var bounty: bountyVal is bounty.value;
  begin
    bountyVal.value := value;
    bountyVal.weight := weight;
    bountyVal.volume := volume;
  end func;

const proc: main is func
  local
    const bounty: panacea is bounty(3000, 0.3, 0.025);
    const bounty: ichor   is bounty(1800, 0.2, 0.015);
    const bounty: gold    is bounty(2500, 2.0, 0.002);
    const bounty: sack    is bounty(0,   25.0, 0.25);
    const integer: maxPanacea is trunc(min(sack.weight / panacea.weight, sack.volume / panacea.volume));
    const integer: maxIchor   is trunc(min(sack.weight / ichor.weight,   sack.volume / ichor.volume));
    const integer: maxGold    is trunc(min(sack.weight / gold.weight,    sack.volume / gold.volume));
    var bounty: current is bounty.value;
    var bounty: best is bounty.value;
    var array integer: bestAmounts is 3 times 0;
    var integer: numPanacea is 0;
    var integer: numIchor is 0;
    var integer: numGold is 0;
  begin
    for numPanacea range 0 to maxPanacea do
      for numIchor range 0 to maxIchor do
        for numGold range 0 to maxGold do
          current.value  := numGold      * gold.value  + numIchor      * ichor.value  + numPanacea      * panacea.value;
          current.weight := flt(numGold) * gold.weight + flt(numIchor) * ichor.weight + flt(numPanacea) * panacea.weight;
          current.volume := flt(numGold) * gold.volume + flt(numIchor) * ichor.volume + flt(numPanacea) * panacea.volume;
          if current.value > best.value and current.weight <= sack.weight and current.volume <= sack.volume then
            best := current;
            bestAmounts := [] (numPanacea, numIchor, numGold);
          end if;
        end for;
      end for;
    end for;
    writeln("Maximum value achievable is " <& best.value);
    writeln("This is achieved by carrying " <& bestAmounts[1] <& " panacea, " <& bestAmounts[2] <& " ichor and " <& bestAmounts[3] <& " gold items");
    writeln("The weight of this carry is " <& best.weight <& " and the volume used is " <& best.volume digits 4);
  end func;
```


Output:

```txt

Maximum value achievable is 54500
This is achieved by carrying 0 panacea, 15 ichor and 11 gold items
The weight of this carry is 25.0 and the volume used is 0.2470

```



## Sidef

{{trans|Perl}}

```ruby
struct KnapsackItem {
    Number volume,
    Number weight,
    Number value,
    String name,
}

var items = [
    KnapsackItem(25,  3, 3000, "panacea")
    KnapsackItem(15,  2, 1800, "ichor"  )
    KnapsackItem( 2, 20, 2500, "gold"   )
]

var (
    max_weight = 250,
    max_vol = 250,
    vsc = 1000,
    wsc = 10
)

func solve(i, w, v) is cached {
    return [0, []] if i.is_neg;

    var x = solve(i.dec, w, v);

    var (w1, v1);
    Inf.times { |t|
        var item = items[i];
        break if ((w1 = (w - t*item.weight)).is_neg)
        break if ((v1 = (v - t*item.volume)).is_neg)

        var y = solve(i.dec, w1, v1);
        if ((var tmp = (y[0] + t*item.value)) > x[0]) {
            x = [tmp, [y[1]..., [i, t]]];
        }
    }

    return x
}

var x = solve(items.end, max_weight, max_vol)

print <<"EOT"
Max value #{x[0]}, with:
    Item        Qty     Weight   Vol    Value
#{"-" * 50}
EOT

var (wtot=0, vtot=0);
x[1].each { |s|
    var item = items[s[0]];
    "    #{item.name}:\t% 3d  % 8d% 8g% 8d\n".printf(
        s[1],
        item.weight * s[1] / wsc,
        item.volume * s[1] / vsc,
        item.value  * s[1]
    );
    wtot += (item.weight * s[1]);
    vtot += (item.volume * s[1]);
}

print <<"EOT"
#{"-" * 50}
    Total:\t     #{"%8d%8g%8d" % (wtot/wsc, vtot/vsc, x[0])}
EOT
```

{{out}}

```txt

Max value 54500, with:
    Item        Qty     Weight   Vol    Value
--------------------------------------------------
    panacea:	  9         2   0.225   27000
    gold:	 11        22   0.022   27500
--------------------------------------------------
    Total:	           24   0.247   54500

```



## Tcl

The following code uses brute force, but that's tolerable as long as it takes just a split second to find all 4 solutions. The use of arrays makes the quote quite legible:

```Tcl
#!/usr/bin/env tclsh
proc main argv {
    array set value  {panacea 3000  ichor 1800  gold 2500}
    array set weight {panacea 0.3   ichor 0.2   gold 2.0   max 25}
    array set volume {panacea 0.025 ichor 0.015 gold 0.002 max 0.25}

    foreach i {panacea ichor gold} {
        set max($i) [expr {min(int($volume(max)/$volume($i)),
                               int($weight(max)/$weight($i)))}]
    }
    set maxval 0
    for {set i 0} {$i < $max(ichor)} {incr i} {
        for {set p 0} {$p < $max(panacea)} {incr p} {
            for {set g 0} {$g < $max(gold)} {incr g} {
                if {$i*$weight(ichor) + $p*$weight(panacea) + $g*$weight(gold)
                    > $weight(max)} continue
                if {$i*$volume(ichor) + $p*$volume(panacea) + $g*$volume(gold)
                    > $volume(max)} continue
                set val [expr {$i*$value(ichor)+$p*$value(panacea)+$g*$value(gold)}]
                if {$val == $maxval} {
                    lappend best [list i $i p $p g $g]
                } elseif {$val > $maxval} {
                    set maxval $val
                    set best [list [list i $i p $p g $g]]
                }
            }
        }
    }
    puts "maxval: $maxval, best: $best"
}
main $argv
```



```txt
$ time tclsh85 /Tcl/knapsack.tcl
maxval: 54500, best: {i 0 p 9 g 11} {i 5 p 6 g 11} {i 10 p 3 g 11} {i 15 p 0 g 11}

real    0m0.188s
user    0m0.015s
sys     0m0.015s
```



## Ursala

The algorithm is to enumerate all packings with up to the maximum of each item,
filter them by the volume and weight restrictions, partition the remaining packings
by value, and search for the maximum value class.

```Ursala
#import nat
#import flo

vol = iprod/<0.025,0.015,0.002>+ float*
val = iprod/<3000.,1800.,2500.>+ float*
wgt = iprod/<0.3,0.2,2.0>+ float*

packings = ~&lrlrNCCPCS ~&K0=> iota* <11,17,13>

solutions = fleq$^rS&hl |=&l ^(val,~&)* (fleq\25.+ wgt)*~ (fleq\0.25+ vol)*~ packings

#cast %nmL

human_readable = ~&p/*<'panacea','ichor','gold'> solutions
```

output:

```txt
<
   <'panacea': 0,'ichor': 15,'gold': 11>,
   <'panacea': 3,'ichor': 10,'gold': 11>,
   <'panacea': 6,'ichor': 5,'gold': 11>,
   <'panacea': 9,'ichor': 0,'gold': 11>>
```



## Visual Basic

See: [[Knapsack Problem/Visual Basic]]

The above Link contains a longer version (which perhaps runs a bit faster),
whilst the one below is focussing more on expressing/solving the problem
in less lines of code.

```vb
Function Min(E1, E2): Min = IIf(E1 < E2, E1, E2): End Function 'small Helper-Function

Sub Main()
Const Value = 0, Weight = 1, Volume = 2, PC = 3, IC = 4, GC = 5
Dim P&, I&, G&, A&, M, Cur(Value To Volume)
Dim S As New Collection: S.Add Array(0) '<- init Solutions-Coll.

Const SackW = 25, SackV = 0.25
Dim Panacea: Panacea = Array(3000, 0.3, 0.025)
Dim Ichor:     Ichor = Array(1800, 0.2, 0.015)
Dim Gold:       Gold = Array(2500, 2, 0.002)

  For P = 0 To Int(Min(SackW / Panacea(Weight), SackV / Panacea(Volume)))
    For I = 0 To Int(Min(SackW / Ichor(Weight), SackV / Ichor(Volume)))
      For G = 0 To Int(Min(SackW / Gold(Weight), SackV / Gold(Volume)))
        For A = Value To Volume: Cur(A) = G * Gold(A) + I * Ichor(A) + P * Panacea(A): Next
        If Cur(Value) >= S(1)(Value) And Cur(Weight) <= SackW And Cur(Volume) <= SackV Then _
          S.Add Array(Cur(Value), Cur(Weight), Cur(Volume), P, I, G), , 1
  Next G, I, P

  Debug.Print "Value", "Weight", "Volume", "PanaceaCount", "IchorCount", "GoldCount"
  For Each M In S '<- enumerate the Attributes of the Maxima
    If M(Value) = S(1)(Value) Then Debug.Print M(Value), M(Weight), M(Volume), M(PC), M(IC), M(GC)
  Next
End Sub
```

Output:
```txt
 Value        Weight        Volume        PanaceaCount  IchorCount    GoldCount
 54500         24.7          0.247         9             0             11
 54500         24.8          0.247         6             5             11
 54500         24.9          0.247         3             10            11
 54500         25            0.247         0             15            11

```



## zkl

{{trans|D}}

```zkl
panacea:=T(3000,  0.3, 0.025);  // (value,weight,volume)
ichor  :=T(1800,  0.2, 0.015);
gold   :=T(2500,  2.0, 0.002);
sack   :=T(   0, 25.0, 0.250);  const VAL=0, W=1, VOL=2;

maxes:=T(panacea,ichor,gold)
   .apply('wrap(t){ (sack[W]/t[W]).min(sack[VOL]/t[VOL]).toInt().walker() });
best:=Utils.Helpers.cprod3(maxes.xplode())
    .apply('wrap(t){
       T(T(panacea[VAL]*t[0] + ichor[VAL]*t[1] + gold[VAL]*t[2],
           panacea[W]  *t[0] + ichor[W]  *t[1] + gold[W]  *t[2],
           panacea[VOL]*t[0] + ichor[VOL]*t[1] + gold[VOL]*t[2]), t)
    })
    .filter('wrap(t){ t[0][W]<=sack[W] and t[0][VOL]<=sack[VOL] })
    .reduce(fcn(a,b){ a[0][VAL] > b[0][VAL] and a or b });

println("Maximum value achievable is %,d".fmt(best[0][VAL]));
println(("This is achieved by carrying (one solution):"
         "  %d panacea, %d ichor and %d gold").fmt(best[1].xplode()));
println("The weight to carry is %4.1f and the volume used is %5.3f"
        .fmt(best[0][1,*].xplode()));
```

cprod3 is the Cartesian product of three lists or iterators.
{{out}}

```txt

Maximum value achievable is 54,500
This is achieved by carrying (one solution):  9 panacea, 0 ichor and 11 gold
The weight to carry is 24.7 and the volume used is 0.247

```


[[Category:Puzzles]]
