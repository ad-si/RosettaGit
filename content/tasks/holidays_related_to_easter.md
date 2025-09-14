+++
title = "Holidays related to Easter"
description = ""
date = 2019-09-25T00:49:06Z
aliases = []
[extra]
id = 7622
[taxonomies]
categories = ["task", "sciences"]
tags = []
languages = [
  "360_assembly",
  "ada",
  "algol_68",
  "bbc_basic",
  "bc",
  "befunge",
  "c",
  "cobol",
  "common_lisp",
  "csharp",
  "d",
  "elixir",
  "erlang",
  "forth",
  "fortran",
  "go",
  "j",
  "java",
  "julia",
  "kotlin",
  "lua",
  "mathematica",
  "maxima",
  "pari_gp",
  "perl",
  "perl_6",
  "phix",
  "picolisp",
  "pl_i",
  "powershell",
  "python",
  "racket",
  "rexx",
  "ruby",
  "scala",
  "scheme",
  "seed7",
  "sidef",
  "tcl",
  "tuscript",
  "vba",
]
+++

## Task

Calculate the dates of:
:::*   [[wp:Easter|Easter]]
:::*   [[wp:Ascension Thursday|Ascension Thursday]]
:::*   [[wp:Pentecost|Pentecost]]
:::*   [[wp:Trinity Sunday|Trinity Sunday]]
:::*   [[wp:Corpus Christi (feast)|Corpus Christi feast]]


As an example, calculate for the first year of each century from;
:::*   years     400   to   2100 [[wp:Common Era|CE]]   and for
:::*   years         2010   to   2020 CE.


;Note:
From year 325 CE on,   [[wp:Easter Sunday|Easter Sunday]]   is the Sunday following the ﬁrst   [[wp:Ecclesiastical full moon|Ecclesiastical full moon]]   not earlier than the equinox date in 325 — 21 March.   The Ecclesiastical full moon does not always correspond to the astronomical full moon since in   year 325   ﬁne details of Lunar dynamics were not yet fully understood.

[[wp:Metonic cycle|Metonic cycle]]:   Taking a year to be 1/19th of this 6940-day cycle gives a year length of 365 + 1/4 + 1/76 days (the unrounded cycle is much more accurate),   which is slightly more than 12 synodic months.   To keep the 12-month lunar year in pace with the solar year,   an    [[wp:intercalary|intercalary]]   13th month would have to be added on seven occasions during the nineteen-year period.    Meton introduced a formula for intercalation in circa   432 BC.





## 360 Assembly

For maximum compatibility, this program uses only the basic instruction set (S/360)
and two ASSIST macros (XDECO, XPRNT) to keep the code as short as possible.

```360asm
*        Holidays related to Easter    29/05/2016
HOLIDAYS CSECT
         USING  HOLIDAYS,R13       base register
         B      72(R15)            skip savearea
         DC     17F'0'             savearea
         STM    R14,R12,12(R13)    prolog
         ST     R13,4(R15)         "
         ST     R15,8(R13)         "
         LR     R13,R15            "
         LA     R9,2               nn=2
LOOPNN   C      R9,=F'2'           if nn=2
         BNE    NN2
NN1      MVC    I1,=F'400'         i1=400
         MVC    I2,=F'2100'        i2=2100
         MVC    I3,=F'100'         i3=100
         B      NN3
NN2      MVC    I1,=F'2010'        i1=2010
         MVC    I2,=F'2020'        i2=2020
         MVC    I3,=F'1'           i3=1
NN3      MVC    PG(L'PGT),PGT      pg=pgt
         L      R1,I1              i1
         XDECO  R1,XDEC            edit i1
         MVC    PG+24(4),XDEC+8    output i1
         L      R1,I2              i2
         XDECO  R1,XDEC            edit i2
         MVC    PG+32(4),XDEC+8    output i2
         L      R1,I3              i3
         XDECO  R1,XDEC            edit i3
         MVC    PG+42(3),XDEC+9    output i3
         XPRNT  PG,L'PGT           print buffer
         L      R6,I1              y=i1
LOOPY    C      R6,I2              do y=i1 to i2 by i3
         BH     ELOOPY             leave y
         LR     R4,R6              y
         SRDA   R4,32              ~
         D      R4,=F'19'          /19
         ST     R4,A               a=y//19
         LR     R4,R6              y
         SRDA   R4,32              ~
         D      R4,=F'100'         /100
         ST     R5,B               b=y/100
         ST     R4,C               c=y//100
         L      R4,B               b
         SRDA   R4,32              ~
         D      R4,=F'4'           /4
         ST     R5,D               d=b/4
         ST     R4,E               e=b//4
         L      R4,B               b
         LA     R4,8(R4)           +8
         SRDA   R4,32              ~
         D      R4,=F'25'          /25
         ST     R5,F               f=(b+8)/25
         L      R4,B               b
         S      R4,F               -f
         LA     R4,1(R4)           +1
         SRDA   R4,32              ~
         D      R4,=F'3'           /3
         ST     R5,G               g=(b-f+1)/3
         L      R5,A               a
         M      R4,=F'19'          *19
         LR     R4,R5              .
         A      R4,B               +b
         S      R4,D               -d
         S      R4,G               -g
         LA     R4,15(R4)          +15
         SRDA   R4,32              ~
         D      R4,=F'30'          /30
         ST     R4,H               h=(19*a+b-d-g+15)//30
         L      R4,C               c
         SRDA   R4,32              ~
         D      R4,=F'4'           /4
         ST     R5,I               i=c/4
         ST     R4,K               k=c//4
         L      R4,E               e
         SLA    R4,1               <<1 <=> *2
         LA     R4,32(R4)          +32
         L      R2,I               i
         SLA    R2,1               <<1 <=> *2
         AR     R2,R4              32+2*e+2*i
         S      R2,H               -h
         S      R2,K               -k
         SRDA   R2,32              ~
         D      R2,=F'7'           /7
         ST     R2,L               l=(32+2*e+2*i-h-k)//7
         L      R5,H               h
         M      R4,=F'11'          *11
         A      R5,A               +a
         L      R3,L               l
         M      R2,=F'22'          *22
         AR     R3,R5              a+11*h+22*l
         LR     R4,R3              .
         SRDA   R4,32              ~
         D      R4,=F'451'         /451
         ST     R5,M               m=(a+11*h+22*l)/451
         L      R2,H               h
         A      R2,L               +l
         L      R5,M               m
         M      R4,=F'7'           *7
         SR     R2,R5              (h+l)-(7*m)
         LA     R2,114(R2)         +114
         ST     R2,N               n=h+l-7*m+114
         L      R4,N               n
         SRDA   R4,32              ~
         D      R4,=F'31'          /31
         ST     R5,XM              xm=n/31
         LA     R4,1(R4)           +1
         ST     R4,XD              xd=n//31+1
         LA     R10,PG             pgi=0
         MVC    PG,=CL84' '        pg=' '
         XDECO  R6,XDEC            edit y
         MVC    0(4,R10),XDEC+8    output y
         LA     R10,4(R10)         pgi=pgi+3
         LA     R8,5               loop counter for loopi
         LA     R7,1               i=1
LOOPI    LR     R1,R7              do i=1 to 5; r1=i
         SLA    R1,1               *2
         LH     R2,OFFSET-2(R1)    offset(i)
         L      R4,XD              xd
         AR     R4,R2              wd=xd+offset(i)
WHILE    L      R1,XM              xm
         SLA    R1,1               *2
         LH     R2,DAYS-2(R1)      days(xm)
         CR     R4,R2              while wd>days(xm)
         BNH    WEND               leave while
         SR     R4,R2              wd=wd-days(xm)
         L      R2,XM              xm
         LA     R2,1(R2)           xm+1
         ST     R2,XM              xm=xm+1
         B      WHILE              loop while
WEND     ST     R4,XD              xd=wd
         LA     R10,1(R10)         pgi=pgi+1
         LR     R1,R7              i
         MH     R1,=AL2(L'HOLIDAY) *9
         LA     R14,HOLIDAY-9(R1)  @holiday(i)
         MVC    0(L'HOLIDAY,R10),0(R14)  output holiday(i)
         LA     R10,9(R10)         pgi=pgi+9
         L      R1,XD              xd
         XDECO  R1,XDEC            edit xd
         MVC    0(3,R10),XDEC+9    output xd
         LA     R10,3(R10)         pgi=pgi+3
         L      R1,XM              xm
         MH     R1,=AL2(L'MONTH)   *3
         LA     R14,MONTH-3(R1)    @month(xm)
         MVC    0(L'MONTH,R10),0(R14)  output month(xm)
         LA     R10,3(R10)         pgi=pgi+3
         LA     R7,1(R7)           i+1
         BCT    R8,LOOPI           next i
         XPRNT  PG,L'PG            print buffer
         A      R6,I3              y=y+i3
         B      LOOPY              next y
ELOOPY   BCT    R9,LOOPNN          next nn
         L      R13,4(0,R13)       epilog
         LM     R14,R12,12(R13)    "
         XR     R15,R15            "
         BR     R14                exit
A        DS     F                  a=y//19
B        DS     F                  b=y/100
C        DS     F                  c=y//100
D        DS     F                  d=b/4
E        DS     F                  e=b//4
F        DS     F                  f=(b+8)/25
G        DS     F                  g=(b-f+1)/3
H        DS     F                  h=(19*a+b-d-g+15)//30
I        DS     F                  i=c/4
K        DS     F                  k=c//4
L        DS     F                  l=(32+2*e+2*i-h-k)//7
M        DS     F                  m=(a+11*h+22*l)/451
N        DS     F                  n=h+l-7*m+114
XM       DS     F                  month
XD       DS     F                  day
I1       DS     F                  from year i1
I2       DS     F                  to   year i2
I3       DS     F                  step year i3
MONTH    DC     CL3'jan',CL3'feb',CL3'mar',CL3'apr',CL3'may',CL3'jun'
DAYS     DC     H'31',H'28',H'31',H'30',H'31',H'30'
HOLIDAY  DC     CL9'Easter',CL9'Ascension',CL9'Pentecost'
         DC     CL9'Trinity',CL9'Corpus'
OFFSET   DC     H'0',H'39',H'10',H'7',H'4'
PGT      DC     CL45'Christian holidays from .... to .... step ...'
PG       DC     CL84' '            buffer
XDEC     DS     CL12               temp for edit
         YREGS
         END    HOLIDAYS
```

```txt

Christian holidays from  400 to 2100 step 100
 400 Easter     2apr Ascension 11may Pentecost 21may Trinity   28may Corpus     1jun
 500 Easter     4apr Ascension 13may Pentecost 23may Trinity   30may Corpus     3jun
 600 Easter    13apr Ascension 22may Pentecost  1jun Trinity    8jun Corpus    12jun
 700 Easter    15apr Ascension 24may Pentecost  3jun Trinity   10jun Corpus    14jun
 800 Easter    23apr Ascension  1jun Pentecost 11jun Trinity   18jun Corpus    22jun
 900 Easter    28mar Ascension  6may Pentecost 16may Trinity   23may Corpus    27may
1000 Easter    30mar Ascension  8may Pentecost 18may Trinity   25may Corpus    29may
1100 Easter     8apr Ascension 17may Pentecost 27may Trinity    3jun Corpus     7jun
1200 Easter     9apr Ascension 18may Pentecost 28may Trinity    4jun Corpus     8jun
1300 Easter    18apr Ascension 27may Pentecost  6jun Trinity   13jun Corpus    17jun
1400 Easter    20apr Ascension 29may Pentecost  8jun Trinity   15jun Corpus    19jun
1500 Easter     1apr Ascension 10may Pentecost 20may Trinity   27may Corpus    31may
1600 Easter     2apr Ascension 11may Pentecost 21may Trinity   28may Corpus     1jun
1700 Easter    11apr Ascension 20may Pentecost 30may Trinity    6jun Corpus    10jun
1800 Easter    13apr Ascension 22may Pentecost  1jun Trinity    8jun Corpus    12jun
1900 Easter    15apr Ascension 24may Pentecost  3jun Trinity   10jun Corpus    14jun
2000 Easter    23apr Ascension  1jun Pentecost 11jun Trinity   18jun Corpus    22jun
2100 Easter    28mar Ascension  6may Pentecost 16may Trinity   23may Corpus    27may
Christian holidays from 2010 to 2020 step   1
2010 Easter     4apr Ascension 13may Pentecost 23may Trinity   30may Corpus     3jun
2011 Easter    24apr Ascension  2jun Pentecost 12jun Trinity   19jun Corpus    23jun
2012 Easter     8apr Ascension 17may Pentecost 27may Trinity    3jun Corpus     7jun
2013 Easter    31mar Ascension  9may Pentecost 19may Trinity   26may Corpus    30may
2014 Easter    20apr Ascension 29may Pentecost  8jun Trinity   15jun Corpus    19jun
2015 Easter     5apr Ascension 14may Pentecost 24may Trinity   31may Corpus     4jun
2016 Easter    27mar Ascension  5may Pentecost 15may Trinity   22may Corpus    26may
2017 Easter    16apr Ascension 25may Pentecost  4jun Trinity   11jun Corpus    15jun
2018 Easter     1apr Ascension 10may Pentecost 20may Trinity   27may Corpus    31may
2019 Easter    21apr Ascension 30may Pentecost  9jun Trinity   16jun Corpus    20jun
2020 Easter    12apr Ascension 21may Pentecost 31may Trinity    7jun Corpus    11jun

```



## Ada

Ada.Calendar can only handle years in the range 1901 to 2399.

```Ada
with Ada.Calendar.Arithmetic;
with Ada.Calendar.Formatting;
with Ada.Text_IO;

procedure Main is
   use Ada.Calendar;
   use Arithmetic;

   function Get_Easter (Year : Year_Number) return Time is
      A : Integer := Year mod 19;
      B : Integer := Year / 100;
      C : Integer := Year mod 100;
      D : Integer := B / 4;
      E : Integer := B mod 4;
      F : Integer := (B + 8) / 25;
      G : Integer := (B - F + 1) / 3;
      H : Integer := (19 * A + B - D - G + 15) mod 30;
      I : Integer := C / 4;
      K : Integer := C mod 4;
      L : Integer := (32 + 2 * E + 2 * I - H - K) mod 7;
      M : Integer := (A + 11 * H + 22 * L) / 451;
      N : Integer := H + L - 7 * M + 114;
   begin
      return Time_Of (Year, N / 31, N mod 31 + 1, 43200.0);
   end Get_Easter;

   procedure Print_Easter (Year : Year_Number) is
      Days_To_Ascension : constant Day_Count := 39;
      Days_To_Pentecost : constant Day_Count := 49;
      Days_To_Trinity   : constant Day_Count := 56;
      Days_To_Corpus    : constant Day_Count := 60;
      Easter : Time := Get_Easter (Year);
   begin
      Ada.Text_IO.Put (Integer'Image (Year));
      Ada.Text_IO.Put (": Easter: " &
                       Formatting.Image (Easter) (6 .. 10));
      Ada.Text_IO.Put (", Ascension: " &
                       Formatting.Image (Easter + Days_To_Ascension) (6 .. 10));
      Ada.Text_IO.Put (", Pentecost: " &
                       Formatting.Image (Easter + Days_To_Pentecost) (6 .. 10));
      Ada.Text_IO.Put (", Trinity: " &
                       Formatting.Image (Easter + Days_To_Trinity) (6 .. 10));
      Ada.Text_IO.Put_Line (", Corpus: " &
                       Formatting.Image (Easter + Days_To_Corpus) (6 .. 10));
   end Print_Easter;
begin
   Ada.Text_IO.Put_Line
     ("Christian holidays, related to Easter, for years from 2010 to 2020 CE:");
   for I in 2010 .. 2020 loop
      Print_Easter (I);
   end loop;
end Main;
```


output:

```txt
Christian holidays, related to Easter, for years from 2010 to 2020 CE:
 2010: Easter: 04-04, Ascension: 05-13, Pentecost: 05-23, Trinity: 05-30, Corpus: 06-03
 2011: Easter: 04-24, Ascension: 06-02, Pentecost: 06-12, Trinity: 06-19, Corpus: 06-23
 2012: Easter: 04-08, Ascension: 05-17, Pentecost: 05-27, Trinity: 06-03, Corpus: 06-07
 2013: Easter: 03-31, Ascension: 05-09, Pentecost: 05-19, Trinity: 05-26, Corpus: 05-30
 2014: Easter: 04-20, Ascension: 05-29, Pentecost: 06-08, Trinity: 06-15, Corpus: 06-19
 2015: Easter: 04-05, Ascension: 05-14, Pentecost: 05-24, Trinity: 05-31, Corpus: 06-04
 2016: Easter: 03-27, Ascension: 05-05, Pentecost: 05-15, Trinity: 05-22, Corpus: 05-26
 2017: Easter: 04-16, Ascension: 05-25, Pentecost: 06-04, Trinity: 06-11, Corpus: 06-15
 2018: Easter: 04-01, Ascension: 05-10, Pentecost: 05-20, Trinity: 05-27, Corpus: 05-31
 2019: Easter: 04-21, Ascension: 05-30, Pentecost: 06-09, Trinity: 06-16, Corpus: 06-20
 2020: Easter: 04-12, Ascension: 05-21, Pentecost: 05-31, Trinity: 06-07, Corpus: 06-11
```



## ALGOL 68

Note: Base code specimen extracted from [http://www.xs4all.nl/~jmvdveer/a68g-doc.pdf Algol 68 Genie Documentation] Part III - ''Example a68g programs.''
<!--
Part I, II, III and V are distributed under the conditions of the GNU Free Documentation
License: Permission is granted to copy, distribute and / or modify the text under the terms of
the GNU Free Documentation License, Version 1.2 or any later version published by the Free
Software Foundation; with no Invariant Sections, no Front-Cover Texts, and no Back-Cover Texts.
-->

```algol68
MODE YEAR = INT, MONTH = INT, WEEK = INT, DAY = INT;

MODE DATE = STRUCT(
  YEAR year,  #DAY year day, #
  MONTH month, DAY month day,#
  WEEK week,  #DAY week day);

FORMAT mon fmt =      $c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                         "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")$,
FORMAT week day fmt = $c("Sat", "Sun", "Mon", "Tue", "Wed", "Thu", "Fri")$;

PROC year days = (YEAR year)DAY: # Ignore 1752 CE for the moment #
  ( month days(year, 2) = 28 | 365 | 366 );

PROC month days = (YEAR year, MONTH month) DAY:
  ( month | 31,
            28 + ABS (year MOD 4 = 0 AND year MOD 100 /= 0 OR year MOD 400 = 0),
            31, 30, 31, 30, 31, 31, 30, 31, 30, 31);

MONTH year months = 12;
DAY week days = 7; # France 1793 to 1805 had 10 day weeks! #

OP + = (DATE in date, DAY in days)DATE:
    BEGIN # todo: eliminate loops, handle year <= 1752 #
       DAY days := in days;
       DATE date := in date;
    # Normalize the days to be less then 1 year #
       WHILE days < 0 DO
         year OF date -:= 1;
         days +:= year days(year OF date)
       OD;
       WHILE days > year days(year OF date) DO
         days -:= year days(year OF date);
         year OF date +:= 1
       OD;
       month day OF date +:= days;
    # Normalize the days to be the same month #
       WHILE month day OF date > month days(year OF date, month OF date) DO
          month day OF date -:= month days(year OF date, month OF date);
          month OF date +:= 1;
          IF month OF date > year months THEN
             month OF date -:= year months;
             year OF date +:= 1
          FI
       OD;
       week day OF date := week day(date);
       date
    END;

OP +:= = (REF DATE date, DAY in days)DATE:
   date := date + in days;

PROC easter = (YEAR year)DATE:
   BEGIN
      COMMENT
         Easter date algorithm from J.M. Oudin (1940), reprinted in:
         P.K. Seidelmann ed., "Explanatory Supplement to the Astronomical
         Almanac" [1992] (Chapter 12, "Calendars", by L.E. Doggett)
      COMMENT
      DATE date; year OF date := year;
      INT c = year OVER 100, n = year MOD 19; # 19 years: Metonic cycle #
      INT i := (c - c OVER 4 - (c - (c - 17) OVER 25) OVER 3 + 19 * n + 15) MOD 30;
      i -:= (i OVER 28) * (1 - (i OVER 28) * (29 OVER (i + 1)) * ((21 - n) OVER 11));
      INT l = i - (year + year OVER 4 + i + 2 - c + c OVER 4) MOD 7;
      month OF date := 3 + (l + 40) OVER 44;
      month day OF date := l + 28 - 31 * (month OF date OVER 4);
      week day OF date := week day(date);
      date
   END;

PROC week day = (DATE date)DAY:
   # Zeller’s Congruence algorithm from 1887. #
   BEGIN
      INT year := year OF date, month := month OF date, month day := month day OF date, c;
      (month <= 2 | (month +:= 12, year -:= 1));
      c := year OVER 100;
      year MODAB 100;
      1 + (month day + ((month + 1) * 26) OVER 10
         + year + year OVER 4 + c OVER 4 - 2 * c) MOD 7
   END;

FORMAT wdmdm fmt = $f(week day fmt)z-dxf(mon fmt)$,

MODE EASTERRELATED = STRUCT(DATE easter, ascension, pentecost, trinity, corpus christi);

PROC easter related init = (YEAR year)EASTERRELATED:
BEGIN
  DATE date;
  EASTERRELATED holidays;
# Easter date, always a Sunday. #
   easter OF holidays := date := easter(year);
# Ascension day is 39 days after Easter.#
   ascension OF holidays := ( date +:= 39);
# Pentecost is 10 days after Ascension day.#
   pentecost OF holidays := date +:= 10;
# Trinity is 7 days after Pentecost.#
   trinity OF holidays := date +:= 7;
# Corpus Christi is 4 days after Trinity.#
   corpus christi OF holidays := date +:= 4;
   holidays
END;

# Note: Y10K bug here... :-) #
FORMAT easter related fmt = $g(-4)" Easter: "f(wdmdm fmt) ", Ascension: "f(wdmdm fmt)
    ", Pentecost: "f(wdmdm fmt) ", Trinity: "f(wdmdm fmt)", Corpus: "f(wdmdm fmt)$;

PROC easter related print = (YEAR year)VOID:
BEGIN
  EASTERRELATED holidays = easter related init(year);
  PROC wdmdm = (DATE date)STRUCT(DAY week day, DAY month day, MONTH month):
    (week day OF date, month day OF date, month OF date);
  printf((easter related fmt, year,
     wdmdm(easter OF holidays),  wdmdm(ascension OF holidays), wdmdm(pentecost OF holidays),
     wdmdm(trinity OF holidays), wdmdm(corpus christi OF holidays),
  $l$))
END;

printf (($"Christian holidays, related to Easter, for each centennial from 400 to 2100 CE:"l$));
FOR year FROM 400 BY 100 TO 2100 DO easter related print(year) OD;

printf (($l"Christian holidays, related to Easter, for years from 2010 to 2020 CE:"l$));
FOR year FROM 2010 TO 2020 DO easter related print(year) OD
```

Output:

```txt

Christian holidays, related to Easter, for each centennial from 400 to 2100 CE:
 400 Easter: Sun  2 Apr, Ascension: Thu 11 May, Pentecost: Sun 21 May, Trinity: Sun 28 May, Corpus: Thu  1 Jun
 500 Easter: Sun  4 Apr, Ascension: Thu 13 May, Pentecost: Sun 23 May, Trinity: Sun 30 May, Corpus: Thu  3 Jun
 600 Easter: Sun 13 Apr, Ascension: Thu 22 May, Pentecost: Sun  1 Jun, Trinity: Sun  8 Jun, Corpus: Thu 12 Jun
 700 Easter: Sun 15 Apr, Ascension: Thu 24 May, Pentecost: Sun  3 Jun, Trinity: Sun 10 Jun, Corpus: Thu 14 Jun
 800 Easter: Sun 23 Apr, Ascension: Thu  1 Jun, Pentecost: Sun 11 Jun, Trinity: Sun 18 Jun, Corpus: Thu 22 Jun
 900 Easter: Sun 28 Mar, Ascension: Thu  6 May, Pentecost: Sun 16 May, Trinity: Sun 23 May, Corpus: Thu 27 May
1000 Easter: Sun 30 Mar, Ascension: Thu  8 May, Pentecost: Sun 18 May, Trinity: Sun 25 May, Corpus: Thu 29 May
1100 Easter: Sun  8 Apr, Ascension: Thu 17 May, Pentecost: Sun 27 May, Trinity: Sun  3 Jun, Corpus: Thu  7 Jun
1200 Easter: Sun  9 Apr, Ascension: Thu 18 May, Pentecost: Sun 28 May, Trinity: Sun  4 Jun, Corpus: Thu  8 Jun
1300 Easter: Sun 18 Apr, Ascension: Thu 27 May, Pentecost: Sun  6 Jun, Trinity: Sun 13 Jun, Corpus: Thu 17 Jun
1400 Easter: Sun 20 Apr, Ascension: Thu 29 May, Pentecost: Sun  8 Jun, Trinity: Sun 15 Jun, Corpus: Thu 19 Jun
1500 Easter: Sun  1 Apr, Ascension: Thu 10 May, Pentecost: Sun 20 May, Trinity: Sun 27 May, Corpus: Thu 31 May
1600 Easter: Sun  2 Apr, Ascension: Thu 11 May, Pentecost: Sun 21 May, Trinity: Sun 28 May, Corpus: Thu  1 Jun
1700 Easter: Sun 11 Apr, Ascension: Thu 20 May, Pentecost: Sun 30 May, Trinity: Sun  6 Jun, Corpus: Thu 10 Jun
1800 Easter: Sun 13 Apr, Ascension: Thu 22 May, Pentecost: Sun  1 Jun, Trinity: Sun  8 Jun, Corpus: Thu 12 Jun
1900 Easter: Sun 15 Apr, Ascension: Thu 24 May, Pentecost: Sun  3 Jun, Trinity: Sun 10 Jun, Corpus: Thu 14 Jun
2000 Easter: Sun 23 Apr, Ascension: Thu  1 Jun, Pentecost: Sun 11 Jun, Trinity: Sun 18 Jun, Corpus: Thu 22 Jun
2100 Easter: Sun 28 Mar, Ascension: Thu  6 May, Pentecost: Sun 16 May, Trinity: Sun 23 May, Corpus: Thu 27 May

Christian holidays, related to Easter, for years from 2010 to 2020 CE:
2010 Easter: Sun  4 Apr, Ascension: Thu 13 May, Pentecost: Sun 23 May, Trinity: Sun 30 May, Corpus: Thu  3 Jun
2011 Easter: Sun 24 Apr, Ascension: Thu  2 Jun, Pentecost: Sun 12 Jun, Trinity: Sun 19 Jun, Corpus: Thu 23 Jun
2012 Easter: Sun  8 Apr, Ascension: Thu 17 May, Pentecost: Sun 27 May, Trinity: Sun  3 Jun, Corpus: Thu  7 Jun
2013 Easter: Sun 31 Mar, Ascension: Thu  9 May, Pentecost: Sun 19 May, Trinity: Sun 26 May, Corpus: Thu 30 May
2014 Easter: Sun 20 Apr, Ascension: Thu 29 May, Pentecost: Sun  8 Jun, Trinity: Sun 15 Jun, Corpus: Thu 19 Jun
2015 Easter: Sun  5 Apr, Ascension: Thu 14 May, Pentecost: Sun 24 May, Trinity: Sun 31 May, Corpus: Thu  4 Jun
2016 Easter: Sun 27 Mar, Ascension: Thu  5 May, Pentecost: Sun 15 May, Trinity: Sun 22 May, Corpus: Thu 26 May
2017 Easter: Sun 16 Apr, Ascension: Thu 25 May, Pentecost: Sun  4 Jun, Trinity: Sun 11 Jun, Corpus: Thu 15 Jun
2018 Easter: Sun  1 Apr, Ascension: Thu 10 May, Pentecost: Sun 20 May, Trinity: Sun 27 May, Corpus: Thu 31 May
2019 Easter: Sun 21 Apr, Ascension: Thu 30 May, Pentecost: Sun  9 Jun, Trinity: Sun 16 Jun, Corpus: Thu 20 Jun
2020 Easter: Sun 12 Apr, Ascension: Thu 21 May, Pentecost: Sun 31 May, Trinity: Sun  7 Jun, Corpus: Thu 11 Jun

```



## BBC BASIC

As discussed on the Talk page, the task is not well defined for historical dates because the change from the Julian to the Gregorian calendar happened on different dates in different regions.  Therefore only dates from 1800 onwards are output, by which time most countries (particularly those likely to celebrate the Christian Easter) had adopted the Gregorian calendar.

```bbcbasic
      INSTALL @lib$+"DATELIB"

      PRINT "Year      Easter    Ascension Pentecost Trinity   Corpus"
      FOR year% = 1800 TO 2100 STEP 100
        e% = FNeaster(year%) : f$ = "dd MMM"
        PRINT ;year%, FN_date$(e%,f$), FN_date$(e%+39,f$), \
        \     FN_date$(e%+49,f$), FN_date$(e%+56,f$), FN_date$(e%+60,f$)
      NEXT
      PRINT
      FOR year% = 2010 TO 2020
        e% = FNeaster(year%) : f$ = "dd MMM"
        PRINT ;year%, FN_date$(e%,f$), FN_date$(e%+39,f$), \
        \     FN_date$(e%+49,f$), FN_date$(e%+56,f$), FN_date$(e%+60,f$)
      NEXT
      END

      DEF FNeaster(year%)
      LOCAL a%, b%, c%, d%, e%
      a% = year% MOD 19
      b% = year% >>> 2
      c% = b% DIV 25 + 1
      d% = (c% * 3) >>> 2
      e% = ((a% * 19) - ((c% * 8 + 5) DIV 25) + d% + 15) MOD 30
      e% += (29578 - a% - e% * 32) >>> 10
      e% -= ((year% MOD 7) + b% - d% + e% + 2) MOD 7
      d% = e% >>> 5
      = FN_mjd(e% - d% * 31, d% + 3, year%)
```

'''Output:'''

```txt

Year      Easter    Ascension Pentecost Trinity   Corpus
1800      13 Apr    22 May    01 Jun    08 Jun    12 Jun
1900      15 Apr    24 May    03 Jun    10 Jun    14 Jun
2000      23 Apr    01 Jun    11 Jun    18 Jun    22 Jun
2100      28 Mar    06 May    16 May    23 May    27 May

2010      04 Apr    13 May    23 May    30 May    03 Jun
2011      24 Apr    02 Jun    12 Jun    19 Jun    23 Jun
2012      08 Apr    17 May    27 May    03 Jun    07 Jun
2013      31 Mar    09 May    19 May    26 May    30 May
2014      20 Apr    29 May    08 Jun    15 Jun    19 Jun
2015      05 Apr    14 May    24 May    31 May    04 Jun
2016      27 Mar    05 May    15 May    22 May    26 May
2017      16 Apr    25 May    04 Jun    11 Jun    15 Jun
2018      01 Apr    10 May    20 May    27 May    31 May
2019      21 Apr    30 May    09 Jun    16 Jun    20 Jun
2020      12 Apr    21 May    31 May    07 Jun    11 Jun

```



## bc

These programs use several extensions: <code>else</code> branch, <code>print</code> statement, logical boolean operators, and long names.


### Western holidays

This program switches calendar from Julian to Gregorian during October 1582. Its algorithm takes from [http://www.merlyn.demon.co.uk/zel-1886.htm ''Kalendar-Formeln'' by Zeller (1886)].


```bc
scale = 0

/* Format d days after March 21. Assumes March, April, May or June. */
define format(d) {
	if (d > 80) {		print "     Jun ",  d - 71
	} else if (d > 71) { 	print "     Jun 0", d - 71
	} else if (d > 49) {	print "     May ",  d - 40
	} else if (d > 40) {	print "     May 0", d - 40
	} else if (d > 19) {	print "     Apr ",  d - 10
	} else if (d > 10) {	print "     Apr 0", d - 10
	} else {		print "     Mar ",  d + 21
	}
}

/*
 * For year n, print Easter and related holidays.
 * Assumes n >= 1, scale = 0.
 */
define easter(n) {
	auto a, b, d, e, j

	/*
	 * Calculate e = day of Easter, following the 1886 paper,
	 * Kalender-Formeln (Calendar Formulae) by Chr. Zeller.
	 *   http://www.merlyn.demon.co.uk/zel-1886.htm
	 *
	 * With bc, p % 7 gives the wrong result when p < 0. To give the
	 * correct result, this code uses "+ 6 * j" where one might have
	 * used "- j". This works because because 6 == -1 (mod 7).
	 */
	if (n < 1583) {
		/* Julian calendar (before October 1582) */
		b = (19 * (n % 19) + 15) % 30
		d = (b + n + n / 4) % 7
	} else {
		/* Gregorian calendar (after October 1582) */
		j = n / 100
		a = n % 19
		b = (19 * a + 15 + j - (8 * j + 13) / 25 - j / 4) % 30
		d = (b + n + n / 4 + 6 * j + j / 4 + 2) % 7
		if (d == 0 && (b == 29 || (b == 28 && a > 10))) d = 7
	}
	e = b + 7 - d		/* This counts days after 21 March. */

	if(n < 1000) " "
	print n
	z = format(e)		/* Easter */
	z = format(e + 39)	/* Ascension Thursday */
	z = format(e + 49)	/* Pentecost */
	z = format(e + 56)	/* Trinity Sunday */
	z = format(e + 60)	/* Corpus Christi */
	print "\n"
}

print "         Easter     Ascension             Trinity    Corpus\n"
print "  AD     Sunday     Thursday   Pentecost  Sunday     Christi\n"
for (year =  400; year <= 2000; year += 100) z = easter(year)
for (year = 2010; year <= 2020; year +=   1) z = easter(year)
z = easter(2100)
quit
```


```txt
         Easter     Ascension             Trinity    Corpus
  AD     Sunday     Thursday   Pentecost  Sunday     Christi
 400     Apr 01     May 10     May 20     May 27     May 31
 500     Apr 02     May 11     May 21     May 28     Jun 01
 600     Apr 10     May 19     May 29     Jun 05     Jun 09
 700     Apr 11     May 20     May 30     Jun 06     Jun 10
 800     Apr 19     May 28     Jun 07     Jun 14     Jun 18
 900     Apr 20     May 29     Jun 08     Jun 15     Jun 19
1000     Mar 31     May 09     May 19     May 26     May 30
1100     Apr 01     May 10     May 20     May 27     May 31
1200     Apr 09     May 18     May 28     Jun 04     Jun 08
1300     Apr 10     May 19     May 29     Jun 05     Jun 09
1400     Apr 18     May 27     Jun 06     Jun 13     Jun 17
1500     Apr 19     May 28     Jun 07     Jun 14     Jun 18
1600     Apr 02     May 11     May 21     May 28     Jun 01
1700     Apr 11     May 20     May 30     Jun 06     Jun 10
1800     Apr 13     May 22     Jun 01     Jun 08     Jun 12
1900     Apr 15     May 24     Jun 03     Jun 10     Jun 14
2000     Apr 23     Jun 01     Jun 11     Jun 18     Jun 22
2010     Apr 04     May 13     May 23     May 30     Jun 03
2011     Apr 24     Jun 02     Jun 12     Jun 19     Jun 23
2012     Apr 08     May 17     May 27     Jun 03     Jun 07
2013     Mar 31     May 09     May 19     May 26     May 30
2014     Apr 20     May 29     Jun 08     Jun 15     Jun 19
2015     Apr 05     May 14     May 24     May 31     Jun 04
2016     Mar 27     May 05     May 15     May 22     May 26
2017     Apr 16     May 25     Jun 04     Jun 11     Jun 15
2018     Apr 01     May 10     May 20     May 27     May 31
2019     Apr 21     May 30     Jun 09     Jun 16     Jun 20
2020     Apr 12     May 21     May 31     Jun 07     Jun 11
2100     Mar 28     May 06     May 16     May 23     May 27
```



### Eastern holidays

This program switches calendar from Julian to Gregorian during March 1924. Pentecost and Trinity Sunday are the same day; [[wp:All Saints' Sunday|All Saints' Sunday]] is the next Sunday.


```bc
scale = 0

/* Format d days after March 21. Assumes March, April, May or June. */
define format(d) {
	if (d > 80) {		print "     Jun ",  d - 71
	} else if (d > 71) { 	print "     Jun 0", d - 71
	} else if (d > 49) {	print "     May ",  d - 40
	} else if (d > 40) {	print "     May 0", d - 40
	} else if (d > 19) {	print "     Apr ",  d - 10
	} else if (d > 10) {	print "     Apr 0", d - 10
	} else {		print "     Mar ",  d + 21
	}
}

/*
 * For year n, print Easter and related holidays.
 * Assumes n >= 1, scale = 0.
 */
define easter(n) {
	auto a, b, d, e, j

	/*
	 * Calculate e = day of Easter, following the 1886 paper,
	 * Kalender-Formeln (Calendar Formulae) by Chr. Zeller.
	 *   http://www.merlyn.demon.co.uk/zel-1886.htm
	 */
	b = (19 * (n % 19) + 15) % 30
	d = (b + n + n / 4) % 7
	e = b + 7 - d		/* This counts days after 21 March. */

	/*
	 * Starting at 1924 March 10 (Julian) / March 23 (Gregorian),
	 * change to the Gregorian calendar. Easter 1924 is April 27,
	 * after this change.
	 */
	if (n >= 1924) {
		/* This formula only works from March to December. */
		e += n / 100 - n / 400 - 2
	}

	if(n < 1000) " "
	print n
	z = format(e)		/* Easter */
	z = format(e + 39)	/* Ascension Thursday */
	z = format(e + 49)	/* Pentecost */
	z = format(e + 56)	/* All Saints' Sunday */
	print "\n"
}

print "         Easter     Ascension             All\n"
print "  AD     Sunday     Thursday   Pentecost  Saints\n"
for (year =  400; year <= 2000; year += 100) z = easter(year)
for (year = 2010; year <= 2020; year +=   1) z = easter(year)
z = easter(2100)
quit
```


```txt
         Easter     Ascension             All
  AD     Sunday     Thursday   Pentecost  Saints
 400     Apr 01     May 10     May 20     May 27
 500     Apr 02     May 11     May 21     May 28
 600     Apr 10     May 19     May 29     Jun 05
 700     Apr 11     May 20     May 30     Jun 06
 800     Apr 19     May 28     Jun 07     Jun 14
 900     Apr 20     May 29     Jun 08     Jun 15
1000     Mar 31     May 09     May 19     May 26
1100     Apr 01     May 10     May 20     May 27
1200     Apr 09     May 18     May 28     Jun 04
1300     Apr 10     May 19     May 29     Jun 05
1400     Apr 18     May 27     Jun 06     Jun 13
1500     Apr 19     May 28     Jun 07     Jun 14
1600     Mar 23     May 01     May 11     May 18
1700     Mar 31     May 09     May 19     May 26
1800     Apr 08     May 17     May 27     Jun 03
1900     Apr 09     May 18     May 28     Jun 04
2000     Apr 30     Jun 08     Jun 18     Jun 25
2010     Apr 04     May 13     May 23     May 30
2011     Apr 24     Jun 02     Jun 12     Jun 19
2012     Apr 15     May 24     Jun 03     Jun 10
2013     May 05     Jun 13     Jun 23     Jun 30
2014     Apr 20     May 29     Jun 08     Jun 15
2015     Apr 12     May 21     May 31     Jun 07
2016     May 01     Jun 09     Jun 19     Jun 26
2017     Apr 16     May 25     Jun 04     Jun 11
2018     Apr 08     May 17     May 27     Jun 03
2019     Apr 28     Jun 06     Jun 16     Jun 23
2020     Apr 19     May 28     Jun 07     Jun 14
2100     May 02     Jun 10     Jun 20     Jun 27
```



## Befunge

Follows the same basic algorithm as C#, D, Java and many more.

First two rows initialise the set of years to be processed; second two output the header; the main algorithm starts on row six.

```Befunge
037*>1- : 9`#v_2*4+>1-:3`#v_v
    ^\+*"(2":<     ^\*"d":< >$   v
v"Pentcst"9"Trinity"9"Corpus"+550<
>9"nsnecsA"9"retsaE"9"raeY">:#,_$v
         MarAprMayJun
v-/4::/"d"\*/2"&"%/2"&"::,9.:_@#:<
>\:8+55*/-1+3/-35*++65*%00p::"d"v,
v:%7+*84+*2%4/"d"\-g00-%4\*2/4:%<+
>10p","2/*00g56+*+\"&"2/%+19"2"*v5
v  \+55\7\4\0+/2","-\+g01g00*7/+<5
>"'"\>:9\>:156*+`!#v_156*+-\3v>$$^
  +9,^   ^\+3\-*65_v#`*65:\+ <^_
*84,g4+1,g4:+1,g4:\<    >:#\!#.^#,
```

```txt
Year	Easter	Ascensn	Pentcst	Trinity	Corpus
400 	Apr 2 	May 11 	May 21 	May 28 	Jun 1
500 	Apr 4 	May 13 	May 23 	May 30 	Jun 3
600 	Apr 13 	May 22 	Jun 1 	Jun 8 	Jun 12
700 	Apr 15 	May 24 	Jun 3 	Jun 10 	Jun 14
800 	Apr 23 	Jun 1 	Jun 11 	Jun 18 	Jun 22
900 	Mar 28 	May 6 	May 16 	May 23 	May 27
1000 	Mar 30 	May 8 	May 18 	May 25 	May 29
1100 	Apr 8 	May 17 	May 27 	Jun 3 	Jun 7
1200 	Apr 9 	May 18 	May 28 	Jun 4 	Jun 8
1300 	Apr 18 	May 27 	Jun 6 	Jun 13 	Jun 17
1400 	Apr 20 	May 29 	Jun 8 	Jun 15 	Jun 19
1500 	Apr 1 	May 10 	May 20 	May 27 	May 31
1600 	Apr 2 	May 11 	May 21 	May 28 	Jun 1
1700 	Apr 11 	May 20 	May 30 	Jun 6 	Jun 10
1800 	Apr 13 	May 22 	Jun 1 	Jun 8 	Jun 12
1900 	Apr 15 	May 24 	Jun 3 	Jun 10 	Jun 14
2000 	Apr 23 	Jun 1 	Jun 11 	Jun 18 	Jun 22
2100 	Mar 28 	May 6 	May 16 	May 23 	May 27
2010 	Apr 4 	May 13 	May 23 	May 30 	Jun 3
2011 	Apr 24 	Jun 2 	Jun 12 	Jun 19 	Jun 23
2012 	Apr 8 	May 17 	May 27 	Jun 3 	Jun 7
2013 	Mar 31 	May 9 	May 19 	May 26 	May 30
2014 	Apr 20 	May 29 	Jun 8 	Jun 15 	Jun 19
2015 	Apr 5 	May 14 	May 24 	May 31 	Jun 4
2016 	Mar 27 	May 5 	May 15 	May 22 	May 26
2017 	Apr 16 	May 25 	Jun 4 	Jun 11 	Jun 15
2018 	Apr 1 	May 10 	May 20 	May 27 	May 31
2019 	Apr 21 	May 30 	Jun 9 	Jun 16 	Jun 20
2020 	Apr 12 	May 21 	May 31 	Jun 7 	Jun 11
```



## C

{{trans|ALGOL 68|Note: This specimen retains the original [[Holidays related to Easter#ALGOL 68|ALGOL 68]] coding style -
[http://rosettacode.org/mw/index.php?title=Holidays_related_to_Easter&diff=85840&oldid=85839 diff].}}

```c
#include <stdio.h>

typedef int year_t, month_t, week_t, day_t;

typedef struct{
  year_t year; /* day_t year_day, */
  month_t month;  day_t month_day;/*
  week_t week, */ day_t week_day; } date_t;

const char *mon_fmt[] =      {0, "Jan", "Feb", "Mar", "Apr", "May", "Jun",
                            "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"};
const char *week_day_fmt[] = {0, "Sat", "Sun", "Mon", "Tue", "Wed", "Thu", "Fri"};

day_t month_days(year_t year, month_t month)
  { day_t days[]={0, 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31};
    return (month==2) ? 28 + (( year % 4 == 0 && year % 100 != 0 ) || year % 400 == 0) : days[month];
  }

day_t year_days(year_t year) /* Ignore 1752 CE for the moment */
  { return (month_days(year, 2) == 28) ? 365 : 366; }

month_t year_months = 12;
week_t week_days = 7; /* France 1793 to 1805 had 10 day weeks! */

date_t plusab(date_t *date, day_t days)
    { /* todo: eliminate loops, handle year <== 1752 */
    /* Normalize the days to be less then 1 year */
       while(days < 0){
         date->year -= 1;
         days += year_days(date->year);
       };
       while(days > year_days(date->year)){
         days -= year_days(date->year);
         date->year += 1;
       };
       date->month_day += days;
    /* Normalize the days to be the same month */
       while(date->month_day > month_days(date->year, date->month)){
          date->month_day -= month_days(date->year, date->month);
          date->month += 1;
          if(date->month > year_months){
             date->month -= year_months;
             date->year += 1;
          }
       }
       date->week_day = week_day(*date);
       return *date;
    }

date_t easter (year_t year)
   {
      /*
         Easter date algorithm from J.M. Oudin (1940), reprinted in:
         P.K. Seidelmann ed., "Explanatory Supplement to the Astronomical
         Almanac" [1992] (Chapter 12, "Calendars", by L.E. Doggett)
      */
      date_t date; date.year = year;
      int c = year / 100, n = year % 19; /* 19 years: Metonic cycle */
      int i = (c - c / 4 - (c - (c - 17) / 25) / 3 + 19 * n + 15) % 30;
      i -= (i / 28) * (1 - (i / 28) * (29 / (i + 1)) * ((21 - n) / 11));
      int l = i - (year + year / 4 + i + 2 - c + c / 4) % 7;
      date.month = 3 + (l + 40) / 44;
      date.month_day = l + 28 - 31 * (date.month / 4);
      date.week_day = week_day(date);
      return date;
   }

day_t week_day (date_t date)
   /* Zeller’s Congruence algorithm from 1887. */
   {
      int year = date.year, month = date.month, month_day = date.month_day, c;
      if(month <= 2){month += 12; year -= 1;}
      c = year / 100;
      year %= 100;
      return 1 + ((month_day + ((month + 1) * 26) / 10
         + year + year / 4 + c / 4 - 2 * c) % 7 + 7) % 7;
   }

#define wdmdm_fmt "%s %2d %s"

typedef struct{date_t easter, ascension, pentecost, trinity, corpus_christi;}easter_related_t;

easter_related_t easter_related_init (year_t year)
{
   date_t date;
   easter_related_t holidays;
/* Easter date, always a Sunday. */
   holidays.easter = date = easter(year);
/* Ascension day is 39 days after Easter.*/
   holidays.ascension = plusab(&date, 39);
/* Pentecost is 10 days after Ascension day.*/
   holidays.pentecost = plusab(&date, 10);
/* Trinity is 7 days after Pentecost.*/
   holidays.trinity = plusab(&date, 7);
/* Corpus Christi is 4 days after Trinity.*/
   holidays.corpus_christi = plusab(&date, 4);
   return holidays;
}

/* note: y10k bug here... :-) */
#define easter_related_fmt "%4d Easter: "wdmdm_fmt", Ascension: "wdmdm_fmt\
    ", Pentecost: "wdmdm_fmt", Trinity: "wdmdm_fmt", Corpus: "wdmdm_fmt"\n"

void easter_related_print(year_t year)
{
  easter_related_t holidays = easter_related_init(year);
#define wdmdm(date) week_day_fmt[date.week_day], date.month_day, mon_fmt[date.month]
  printf(easter_related_fmt, year,
    wdmdm(holidays.easter),  wdmdm(holidays.ascension), wdmdm(holidays.pentecost),
    wdmdm(holidays.trinity), wdmdm(holidays.corpus_christi));
}

int main(){
  year_t year;

  printf ("Christian holidays, related to Easter, for each centennial from 400 to 2100 CE:\n");
  for(year=400; year<=2100; year+=100){ easter_related_print(year); }

  printf ("\nChristian holidays, related to Easter, for years from 2010 to 2020 CE:\n");
  for(year=2010; year<=2020; year++){ easter_related_print(year); }
  return 0;
}
```

Output:

```txt

Christian holidays, related to Easter, for each centennial from 400 to 2100 CE:
 400 Easter: Sun  2 Apr, Ascension: Thu 11 May, Pentecost: Sun 21 May, Trinity: Sun 28 May, Corpus: Thu  1 Jun
 500 Easter: Sun  4 Apr, Ascension: Thu 13 May, Pentecost: Sun 23 May, Trinity: Sun 30 May, Corpus: Thu  3 Jun
 600 Easter: Sun 13 Apr, Ascension: Thu 22 May, Pentecost: Sun  1 Jun, Trinity: Sun  8 Jun, Corpus: Thu 12 Jun
 700 Easter: Sun 15 Apr, Ascension: Thu 24 May, Pentecost: Sun  3 Jun, Trinity: Sun 10 Jun, Corpus: Thu 14 Jun
 800 Easter: Sun 23 Apr, Ascension: Thu  1 Jun, Pentecost: Sun 11 Jun, Trinity: Sun 18 Jun, Corpus: Thu 22 Jun
 900 Easter: Sun 28 Mar, Ascension: Thu  6 May, Pentecost: Sun 16 May, Trinity: Sun 23 May, Corpus: Thu 27 May
1000 Easter: Sun 30 Mar, Ascension: Thu  8 May, Pentecost: Sun 18 May, Trinity: Sun 25 May, Corpus: Thu 29 May
1100 Easter: Sun  8 Apr, Ascension: Thu 17 May, Pentecost: Sun 27 May, Trinity: Sun  3 Jun, Corpus: Thu  7 Jun
1200 Easter: Sun  9 Apr, Ascension: Thu 18 May, Pentecost: Sun 28 May, Trinity: Sun  4 Jun, Corpus: Thu  8 Jun
1300 Easter: Sun 18 Apr, Ascension: Thu 27 May, Pentecost: Sun  6 Jun, Trinity: Sun 13 Jun, Corpus: Thu 17 Jun
1400 Easter: Sun 20 Apr, Ascension: Thu 29 May, Pentecost: Sun  8 Jun, Trinity: Sun 15 Jun, Corpus: Thu 19 Jun
1500 Easter: Sun  1 Apr, Ascension: Thu 10 May, Pentecost: Sun 20 May, Trinity: Sun 27 May, Corpus: Thu 31 May
1600 Easter: Sun  2 Apr, Ascension: Thu 11 May, Pentecost: Sun 21 May, Trinity: Sun 28 May, Corpus: Thu  1 Jun
1700 Easter: Sun 11 Apr, Ascension: Thu 20 May, Pentecost: Sun 30 May, Trinity: Sun  6 Jun, Corpus: Thu 10 Jun
1800 Easter: Sun 13 Apr, Ascension: Thu 22 May, Pentecost: Sun  1 Jun, Trinity: Sun  8 Jun, Corpus: Thu 12 Jun
1900 Easter: Sun 15 Apr, Ascension: Thu 24 May, Pentecost: Sun  3 Jun, Trinity: Sun 10 Jun, Corpus: Thu 14 Jun
2000 Easter: Sun 23 Apr, Ascension: Thu  1 Jun, Pentecost: Sun 11 Jun, Trinity: Sun 18 Jun, Corpus: Thu 22 Jun
2100 Easter: Sun 28 Mar, Ascension: Thu  6 May, Pentecost: Sun 16 May, Trinity: Sun 23 May, Corpus: Thu 27 May

Christian holidays, related to Easter, for years from 2010 to 2020 CE:
2010 Easter: Sun  4 Apr, Ascension: Thu 13 May, Pentecost: Sun 23 May, Trinity: Sun 30 May, Corpus: Thu  3 Jun
2011 Easter: Sun 24 Apr, Ascension: Thu  2 Jun, Pentecost: Sun 12 Jun, Trinity: Sun 19 Jun, Corpus: Thu 23 Jun
2012 Easter: Sun  8 Apr, Ascension: Thu 17 May, Pentecost: Sun 27 May, Trinity: Sun  3 Jun, Corpus: Thu  7 Jun
2013 Easter: Sun 31 Mar, Ascension: Thu  9 May, Pentecost: Sun 19 May, Trinity: Sun 26 May, Corpus: Thu 30 May
2014 Easter: Sun 20 Apr, Ascension: Thu 29 May, Pentecost: Sun  8 Jun, Trinity: Sun 15 Jun, Corpus: Thu 19 Jun
2015 Easter: Sun  5 Apr, Ascension: Thu 14 May, Pentecost: Sun 24 May, Trinity: Sun 31 May, Corpus: Thu  4 Jun
2016 Easter: Sun 27 Mar, Ascension: Thu  5 May, Pentecost: Sun 15 May, Trinity: Sun 22 May, Corpus: Thu 26 May
2017 Easter: Sun 16 Apr, Ascension: Thu 25 May, Pentecost: Sun  4 Jun, Trinity: Sun 11 Jun, Corpus: Thu 15 Jun
2018 Easter: Sun  1 Apr, Ascension: Thu 10 May, Pentecost: Sun 20 May, Trinity: Sun 27 May, Corpus: Thu 31 May
2019 Easter: Sun 21 Apr, Ascension: Thu 30 May, Pentecost: Sun  9 Jun, Trinity: Sun 16 Jun, Corpus: Thu 20 Jun
2020 Easter: Sun 12 Apr, Ascension: Thu 21 May, Pentecost: Sun 31 May, Trinity: Sun  7 Jun, Corpus: Thu 11 Jun

```



## C#

```c#
using System;
using System.Collections;
using System.Collections.Specialized;
using System.Linq;

internal class Program
{
    private static readonly OrderedDictionary _holidayOffsets = new OrderedDictionary
                                                                    {
                                                                        {"Easter", 0},
                                                                        {"Ascension", 39},
                                                                        {"Pentecost", 49},
                                                                        {"Trinity", 56},
                                                                        {"Corpus", 60},
                                                                    };

    static void Main(string[] args)
    {
        Console.WriteLine("Christian holidays, related to Easter, for each centennial from 400 to 2100 CE:");
        for (int year = 400; year <= 2100; year += 100)
            OutputHolidays(year);

        Console.WriteLine();
        Console.WriteLine("Christian holidays, related to Easter, for years from 2010 to 2020 CE:");
        for (int year = 2010; year <= 2020; year += 1)
            OutputHolidays(year);
    }

    static void OutputHolidays(int year)
    {
        var easter = CalculateEaster(year);
        var holidays = from kp in _holidayOffsets.OfType<DictionaryEntry>()
                       let holiday = easter.AddDays(Convert.ToInt32(kp.Value))
                       select kp.Key + ": " + string.Format("{0,2:ddd} {0,2:%d} {0:MMM}", holiday);
        Console.WriteLine("{0,4} {1}", year, string.Join(", ", holidays.ToArray()));
    }

    static DateTime CalculateEaster(int year)
    {
        var a = year % 19;
        var b = year / 100;
        var c = year %100;
        var d = b / 4;
        var e = b % 4;
        var f = (b + 8) / 25;
        var g = (b - f + 1) / 3;
        var h = (19 * a + b - d - g + 15) % 30;
        var i = c / 4;
        var k = c % 4;
        var l = (32 + 2 * e + 2 * i - h - k) % 7;
        var m = (a + 11 * h + 22 * l) / 451;
        var numerator = h + l - 7 * m + 114;
        var month = numerator / 31;
        var day = (numerator % 31) + 1;
        return new DateTime(year, month, day);
    }
}

```


Output:

```txt

Christian holidays, related to Easter, for each centennial from 400 to 2100 CE:
 400 Easter: Sun  2 Apr, Ascension: Thu 11 May, Pentecost: Sun 21 May, Trinity: Sun 28 May, Corpus: Thu  1 Jun
 500 Easter: Sun  4 Apr, Ascension: Thu 13 May, Pentecost: Sun 23 May, Trinity: Sun 30 May, Corpus: Thu  3 Jun
 600 Easter: Sun 13 Apr, Ascension: Thu 22 May, Pentecost: Sun  1 Jun, Trinity: Sun  8 Jun, Corpus: Thu 12 Jun
 700 Easter: Sun 15 Apr, Ascension: Thu 24 May, Pentecost: Sun  3 Jun, Trinity: Sun 10 Jun, Corpus: Thu 14 Jun
 800 Easter: Sun 23 Apr, Ascension: Thu  1 Jun, Pentecost: Sun 11 Jun, Trinity: Sun 18 Jun, Corpus: Thu 22 Jun
 900 Easter: Sun 28 Mar, Ascension: Thu  6 May, Pentecost: Sun 16 May, Trinity: Sun 23 May, Corpus: Thu 27 May
1000 Easter: Sun 30 Mar, Ascension: Thu  8 May, Pentecost: Sun 18 May, Trinity: Sun 25 May, Corpus: Thu 29 May
1100 Easter: Sun  8 Apr, Ascension: Thu 17 May, Pentecost: Sun 27 May, Trinity: Sun  3 Jun, Corpus: Thu  7 Jun
1200 Easter: Sun  9 Apr, Ascension: Thu 18 May, Pentecost: Sun 28 May, Trinity: Sun  4 Jun, Corpus: Thu  8 Jun
1300 Easter: Sun 18 Apr, Ascension: Thu 27 May, Pentecost: Sun  6 Jun, Trinity: Sun 13 Jun, Corpus: Thu 17 Jun
1400 Easter: Sun 20 Apr, Ascension: Thu 29 May, Pentecost: Sun  8 Jun, Trinity: Sun 15 Jun, Corpus: Thu 19 Jun
1500 Easter: Sun  1 Apr, Ascension: Thu 10 May, Pentecost: Sun 20 May, Trinity: Sun 27 May, Corpus: Thu 31 May
1600 Easter: Sun  2 Apr, Ascension: Thu 11 May, Pentecost: Sun 21 May, Trinity: Sun 28 May, Corpus: Thu  1 Jun
1700 Easter: Sun 11 Apr, Ascension: Thu 20 May, Pentecost: Sun 30 May, Trinity: Sun  6 Jun, Corpus: Thu 10 Jun
1800 Easter: Sun 13 Apr, Ascension: Thu 22 May, Pentecost: Sun  1 Jun, Trinity: Sun  8 Jun, Corpus: Thu 12 Jun
1900 Easter: Sun 15 Apr, Ascension: Thu 24 May, Pentecost: Sun  3 Jun, Trinity: Sun 10 Jun, Corpus: Thu 14 Jun
2000 Easter: Sun 23 Apr, Ascension: Thu  1 Jun, Pentecost: Sun 11 Jun, Trinity: Sun 18 Jun, Corpus: Thu 22 Jun
2100 Easter: Sun 28 Mar, Ascension: Thu  6 May, Pentecost: Sun 16 May, Trinity: Sun 23 May, Corpus: Thu 27 May

Christian holidays, related to Easter, for years from 2010 to 2020 CE:
2010 Easter: Sun  4 Apr, Ascension: Thu 13 May, Pentecost: Sun 23 May, Trinity: Sun 30 May, Corpus: Thu  3 Jun
2011 Easter: Sun 24 Apr, Ascension: Thu  2 Jun, Pentecost: Sun 12 Jun, Trinity: Sun 19 Jun, Corpus: Thu 23 Jun
2012 Easter: Sun  8 Apr, Ascension: Thu 17 May, Pentecost: Sun 27 May, Trinity: Sun  3 Jun, Corpus: Thu  7 Jun
2013 Easter: Sun 31 Mar, Ascension: Thu  9 May, Pentecost: Sun 19 May, Trinity: Sun 26 May, Corpus: Thu 30 May
2014 Easter: Sun 20 Apr, Ascension: Thu 29 May, Pentecost: Sun  8 Jun, Trinity: Sun 15 Jun, Corpus: Thu 19 Jun
2015 Easter: Sun  5 Apr, Ascension: Thu 14 May, Pentecost: Sun 24 May, Trinity: Sun 31 May, Corpus: Thu  4 Jun
2016 Easter: Sun 27 Mar, Ascension: Thu  5 May, Pentecost: Sun 15 May, Trinity: Sun 22 May, Corpus: Thu 26 May
2017 Easter: Sun 16 Apr, Ascension: Thu 25 May, Pentecost: Sun  4 Jun, Trinity: Sun 11 Jun, Corpus: Thu 15 Jun
2018 Easter: Sun  1 Apr, Ascension: Thu 10 May, Pentecost: Sun 20 May, Trinity: Sun 27 May, Corpus: Thu 31 May
2019 Easter: Sun 21 Apr, Ascension: Thu 30 May, Pentecost: Sun  9 Jun, Trinity: Sun 16 Jun, Corpus: Thu 20 Jun
2020 Easter: Sun 12 Apr, Ascension: Thu 21 May, Pentecost: Sun 31 May, Trinity: Sun  7 Jun, Corpus: Thu 11 Jun

```



## COBOL


```cobol

       identification division.
       program-id. Easter.

       environment division.
       configuration section.
       repository.
           function date-of-integer intrinsic
           function integer-of-date intrinsic
           function mod intrinsic.

       data division.
       working-storage section.
       77  days                   pic 9(2).
       77  a                      pic 9(2).
       77  b                      pic 9(2).
       77  c                      pic 9(2).
       77  d                      pic 9(2).
       77  e                      pic 9(2).
       77  f                      pic 9(2).
       77  g                      pic 9(2).
       77  h                      pic 9(2).
       77  i                      pic 9(2).
       77  k                      pic 9(2).
       77  l                      pic 9(2).
       77  m                      pic 9(2).
       77  week-day               pic 9(1).
       77  numerator              pic 9(4).
       77  integer-date           pic 9(18).

       01  month-tab              value "JanFebMarAprMayJunJulAugSepOctNovDec".
           05 month-abreviated    pic x(3) occurs 12.

       01  week-day-tab           value "SunMonTueWedThuFriSat".
           05 week-day-abreviated pic x(3) occurs 7.

       01  easter-date            pic 9(8).
       01  filler redefines easter-date.
           05 easter-year         pic 9(4).
           05 easter-month        pic 9(2).
           05 easter-day          pic 9(2).

       01  holiday-date           pic 9(8).
       01  filler redefines holiday-date.
           05 holiday-year        pic 9(4).
           05 holiday-month       pic 9(2).
           05 holiday-day         pic 9(2).

       01  edt-date.
           05 edt-week-day        pic x(3).
           05 filler              pic x value space.
           05 edt-day             pic z(2).
           05 filler              pic x value space.
           05 edt-month           pic x(3).

       procedure division.
       main.
           display "Christian holidays, related to Easter, for each centennial from 1700 to 2100 CE:"
           perform varying easter-year from 1700 by 100 until easter-year > 2100
              perform output-holydays
           end-perform
           display " "

           display "Christian holidays, related to Easter, for years from 2010 to 2020 CE:"
           perform varying easter-year from 2010 by 1 until easter-year > 2020
              perform output-holydays
           end-perform
           display " "

           stop run
           .
       output-holydays.
           display easter-year " " no advancing
           perform calculate-easter

           move 0 to days
           perform add-days
           display " Easter: " edt-date no advancing

           move 39 to days
           perform add-days
           display " Ascension: " edt-date no advancing

           move 49 to days
           perform add-days
           display " Pentecost: " edt-date no advancing

           move 56 to days
           perform add-days
           display " Trinity: " edt-date no advancing

           move 60 to days
           perform add-days
           display " Corpus: " edt-date
           .
       calculate-easter.
           compute a = mod(easter-year, 19)
           compute b = easter-year / 100
           compute c = mod(easter-year, 100)
           compute d = b / 4
           compute e = mod(b, 4)
           compute f = (b + 8) / 25
           compute g = (b - f + 1) / 3
           compute h = mod((19 * a + b - d - g + 15), 30)
           compute i = c / 4
           compute k = mod(c, 4)
           compute l = mod((32 + 2 * e + 2 * i - h - k), 7)
           compute m = (a + 11 * h + 22 * l) / 451
           compute numerator = h + l - 7 * m + 114
           compute easter-month = numerator / 31
           compute easter-day = mod(numerator, 31) + 1
           .
       add-days.
           if days = 0
              move easter-date to holiday-date
              move 1 to week-day
           else
              compute holiday-date = date-of-integer(integer-of-date(easter-date) + days)
              compute week-day = mod(integer-of-date(easter-date) + days, 7) + 1
           end-if
           move week-day-abreviated(week-day) to edt-week-day
           move month-abreviated(holiday-month) to edt-month
           move holiday-day to edt-day
           .

```

Implemented the task for 1700 and greater years because COBOL is a Business language and their routines only works after Day zero that is equals to 00:00:00 31 December 1600


```txt
Christian holidays, related to Easter, for each centennial from 1700 to 2100 CE:
1700  Easter: Sun 11 Apr Ascension: Thu 20 May Pentecost: Sun 30 May Trinity: Sun  6 Jun Corpus: Thu 10 Jun
1800  Easter: Sun 13 Apr Ascension: Thu 22 May Pentecost: Sun  1 Jun Trinity: Sun  8 Jun Corpus: Thu 12 Jun
1900  Easter: Sun 15 Apr Ascension: Thu 24 May Pentecost: Sun  3 Jun Trinity: Sun 10 Jun Corpus: Thu 14 Jun
2000  Easter: Sun 23 Apr Ascension: Thu  1 Jun Pentecost: Sun 11 Jun Trinity: Sun 18 Jun Corpus: Thu 22 Jun
2100  Easter: Sun 28 Mar Ascension: Thu  6 May Pentecost: Sun 16 May Trinity: Sun 23 May Corpus: Thu 27 May

Christian holidays, related to Easter, for years from 2010 to 2020 CE:
2010  Easter: Sun  4 Apr Ascension: Thu 13 May Pentecost: Sun 23 May Trinity: Sun 30 May Corpus: Thu  3 Jun
2011  Easter: Sun 24 Apr Ascension: Thu  2 Jun Pentecost: Sun 12 Jun Trinity: Sun 19 Jun Corpus: Thu 23 Jun
2012  Easter: Sun  8 Apr Ascension: Thu 17 May Pentecost: Sun 27 May Trinity: Sun  3 Jun Corpus: Thu  7 Jun
2013  Easter: Sun 31 Mar Ascension: Thu  9 May Pentecost: Sun 19 May Trinity: Sun 26 May Corpus: Thu 30 May
2014  Easter: Sun 20 Apr Ascension: Thu 29 May Pentecost: Sun  8 Jun Trinity: Sun 15 Jun Corpus: Thu 19 Jun
2015  Easter: Sun  5 Apr Ascension: Thu 14 May Pentecost: Sun 24 May Trinity: Sun 31 May Corpus: Thu  4 Jun
2016  Easter: Sun 27 Mar Ascension: Thu  5 May Pentecost: Sun 15 May Trinity: Sun 22 May Corpus: Thu 26 May
2017  Easter: Sun 16 Apr Ascension: Thu 25 May Pentecost: Sun  4 Jun Trinity: Sun 11 Jun Corpus: Thu 15 Jun
2018  Easter: Sun  1 Apr Ascension: Thu 10 May Pentecost: Sun 20 May Trinity: Sun 27 May Corpus: Thu 31 May
2019  Easter: Sun 21 Apr Ascension: Thu 30 May Pentecost: Sun  9 Jun Trinity: Sun 16 Jun Corpus: Thu 20 Jun
2020  Easter: Sun 12 Apr Ascension: Thu 21 May Pentecost: Sun 31 May Trinity: Sun  7 Jun Corpus: Thu 11 Jun
```




## Common Lisp


```lisp
; Easter sunday. Result is a list '(month day)
;
; See:
; Jean Meeus, "Astronomical Formulae for Calculators",
; 4th edition, Willmann-Bell, 1988, p.31

(defun easter (year)
  (let (a b c d e f g h i k l m n p)
    (setq a (rem year 19))
    (multiple-value-setq (b c) (floor year 100))
    (multiple-value-setq (d e) (floor b 4))
    (setq f (floor (+ b 8) 25))
    (setq g (floor (- b f -1) 3))
    (setq h (rem (+ (* 19 a) (- b d g) 15) 30))
    (multiple-value-setq (i k) (floor c 4))
    (setq l (rem (+ 32 e e i (- i h k)) 7))
    (setq m (floor (+ a (* 11 h) (* 22 l)) 451))
    (multiple-value-setq (n p) (floor (+ h l (- 114 (* 7 m))) 31))
    (list n (1+ p))))
```



## D

```d
import std.stdio, std.datetime;

void printEasterRelatedHolidays(in int year) {
    static struct Holyday {
        string name;
        int offs;
    }
    static immutable Holyday[] holidayOffsets = [{"Easter",     0},
                                                 {"Ascension", 39},
                                                 {"Pentecost", 10},
                                                 {"Trinity",    7},
                                                 {"Corpus",     4}];

    // Calculate Easter.
    Date date;
    {
        immutable a = year % 19;
        immutable b = year / 100;
        immutable c = year %100;
        immutable d = b / 4;
        immutable e = b % 4;
        immutable f = (b + 8) / 25;
        immutable g = (b - f + 1) / 3;
        immutable h = (19 * a + b - d - g + 15) % 30;
        immutable i = c / 4;
        immutable k = c % 4;
        immutable l = (32 + 2 * e + 2 * i - h - k) % 7;
        immutable m = (a + 11 * h + 22 * l) / 451;
        immutable n = h + l - 7 * m + 114;
        immutable month = n / 31;
        immutable day = (n % 31) + 1;
        date = Date(year, month, day);
    }

    writef("%4d ", year);
    foreach (immutable hd; holidayOffsets) {
        date += days(hd.offs);
        writef("%s: %2d %s  ", hd.name, date.day, date.month);
    }
    writeln();
}

void main() {
    writeln("Christian holidays, related to Easter," ~
            " for each centennial from 400 to 2100 CE:");
    for (int y = 400; y <= 2100; y += 100)
        printEasterRelatedHolidays(y);

    writeln("\nChristian holidays, related to Easter," ~
            " for years from 2010 to 2020 CE:");
    foreach (immutable y; 2010 .. 2021)
        printEasterRelatedHolidays(y);
}
```

```txt
Christian holidays, related to Easter, for each centennial from 400 to 2100 CE:
 400 Easter:  2 apr  Ascension: 11 may  Pentecost: 21 may  Trinity: 28 may  Corpus:  1 jun
 500 Easter:  4 apr  Ascension: 13 may  Pentecost: 23 may  Trinity: 30 may  Corpus:  3 jun
 600 Easter: 13 apr  Ascension: 22 may  Pentecost:  1 jun  Trinity:  8 jun  Corpus: 12 jun
 700 Easter: 15 apr  Ascension: 24 may  Pentecost:  3 jun  Trinity: 10 jun  Corpus: 14 jun
 800 Easter: 23 apr  Ascension:  1 jun  Pentecost: 11 jun  Trinity: 18 jun  Corpus: 22 jun
 900 Easter: 28 mar  Ascension:  6 may  Pentecost: 16 may  Trinity: 23 may  Corpus: 27 may
1000 Easter: 30 mar  Ascension:  8 may  Pentecost: 18 may  Trinity: 25 may  Corpus: 29 may
1100 Easter:  8 apr  Ascension: 17 may  Pentecost: 27 may  Trinity:  3 jun  Corpus:  7 jun
1200 Easter:  9 apr  Ascension: 18 may  Pentecost: 28 may  Trinity:  4 jun  Corpus:  8 jun
1300 Easter: 18 apr  Ascension: 27 may  Pentecost:  6 jun  Trinity: 13 jun  Corpus: 17 jun
1400 Easter: 20 apr  Ascension: 29 may  Pentecost:  8 jun  Trinity: 15 jun  Corpus: 19 jun
1500 Easter:  1 apr  Ascension: 10 may  Pentecost: 20 may  Trinity: 27 may  Corpus: 31 may
1600 Easter:  2 apr  Ascension: 11 may  Pentecost: 21 may  Trinity: 28 may  Corpus:  1 jun
1700 Easter: 11 apr  Ascension: 20 may  Pentecost: 30 may  Trinity:  6 jun  Corpus: 10 jun
1800 Easter: 13 apr  Ascension: 22 may  Pentecost:  1 jun  Trinity:  8 jun  Corpus: 12 jun
1900 Easter: 15 apr  Ascension: 24 may  Pentecost:  3 jun  Trinity: 10 jun  Corpus: 14 jun
2000 Easter: 23 apr  Ascension:  1 jun  Pentecost: 11 jun  Trinity: 18 jun  Corpus: 22 jun
2100 Easter: 28 mar  Ascension:  6 may  Pentecost: 16 may  Trinity: 23 may  Corpus: 27 may

Christian holidays, related to Easter, for years from 2010 to 2020 CE:
2010 Easter:  4 apr  Ascension: 13 may  Pentecost: 23 may  Trinity: 30 may  Corpus:  3 jun
2011 Easter: 24 apr  Ascension:  2 jun  Pentecost: 12 jun  Trinity: 19 jun  Corpus: 23 jun
2012 Easter:  8 apr  Ascension: 17 may  Pentecost: 27 may  Trinity:  3 jun  Corpus:  7 jun
2013 Easter: 31 mar  Ascension:  9 may  Pentecost: 19 may  Trinity: 26 may  Corpus: 30 may
2014 Easter: 20 apr  Ascension: 29 may  Pentecost:  8 jun  Trinity: 15 jun  Corpus: 19 jun
2015 Easter:  5 apr  Ascension: 14 may  Pentecost: 24 may  Trinity: 31 may  Corpus:  4 jun
2016 Easter: 27 mar  Ascension:  5 may  Pentecost: 15 may  Trinity: 22 may  Corpus: 26 may
2017 Easter: 16 apr  Ascension: 25 may  Pentecost:  4 jun  Trinity: 11 jun  Corpus: 15 jun
2018 Easter:  1 apr  Ascension: 10 may  Pentecost: 20 may  Trinity: 27 may  Corpus: 31 may
2019 Easter: 21 apr  Ascension: 30 may  Pentecost:  9 jun  Trinity: 16 jun  Corpus: 20 jun
2020 Easter: 12 apr  Ascension: 21 may  Pentecost: 31 may  Trinity:  7 jun  Corpus: 11 jun
```



## Elixir

```elixir
defmodule Holiday do
  @offsets  [ Easter: 0, Ascension: 39, Pentecost: 49, Trinity: 56, Corpus: 60 ]
  @mon  { "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec" }

  def easter_date(year) do
    a = rem(year, 19)
    b = div(year, 100)
    c = rem(year, 100)
    d = div(b, 4)
    e = rem(b, 4)
    f = div((b + 8), 25)
    g = div((b - f + 1), 3)
    h = rem((19*a + b - d - g + 15), 30)
    i = div(c, 4)
    k = rem(c, 4)
    l = rem((32 + 2*e + 2*i - h - k), 7)
    m = div((a + 11*h + 22*l), 451)
    numerator = h + l - 7*m + 114
    month = div(numerator, 31)
    day = rem(numerator, 31) + 1
    {year, month, day}
  end

  defp holidays(year) do
    IO.write String.rjust("#{year}:", 5)
    gday = :calendar.date_to_gregorian_days(easter_date(year))
    Enum.map_join(Keyword.values(@offsets), fn d ->
      {_year, month, day} = :calendar.gregorian_days_to_date(gday + d)
      String.rjust("#{day}  #{elem(@mon, month-1)}", 11)
    end)
  end

  def task do
    IO.puts "Year:" <> Enum.map_join(Keyword.keys(@offsets), &String.rjust("#{&1}",11))
    Enum.each(Enum.take_every(400..2100, 100), fn year -> IO.puts holidays(year) end)
    IO.puts ""
    Enum.each(2010..2020, fn year -> IO.puts holidays(year) end)
  end
end

Holiday.task
```


```txt

Year:     Easter  Ascension  Pentecost    Trinity     Corpus
 400:     2  Apr    11  May    21  May    28  May     1  Jun
 500:     4  Apr    13  May    23  May    30  May     3  Jun
 600:    13  Apr    22  May     1  Jun     8  Jun    12  Jun
 700:    15  Apr    24  May     3  Jun    10  Jun    14  Jun
 800:    23  Apr     1  Jun    11  Jun    18  Jun    22  Jun
 900:    28  Mar     6  May    16  May    23  May    27  May
1000:    30  Mar     8  May    18  May    25  May    29  May
1100:     8  Apr    17  May    27  May     3  Jun     7  Jun
1200:     9  Apr    18  May    28  May     4  Jun     8  Jun
1300:    18  Apr    27  May     6  Jun    13  Jun    17  Jun
1400:    20  Apr    29  May     8  Jun    15  Jun    19  Jun
1500:     1  Apr    10  May    20  May    27  May    31  May
1600:     2  Apr    11  May    21  May    28  May     1  Jun
1700:    11  Apr    20  May    30  May     6  Jun    10  Jun
1800:    13  Apr    22  May     1  Jun     8  Jun    12  Jun
1900:    15  Apr    24  May     3  Jun    10  Jun    14  Jun
2000:    23  Apr     1  Jun    11  Jun    18  Jun    22  Jun
2100:    28  Mar     6  May    16  May    23  May    27  May

2010:     4  Apr    13  May    23  May    30  May     3  Jun
2011:    24  Apr     2  Jun    12  Jun    19  Jun    23  Jun
2012:     8  Apr    17  May    27  May     3  Jun     7  Jun
2013:    31  Mar     9  May    19  May    26  May    30  May
2014:    20  Apr    29  May     8  Jun    15  Jun    19  Jun
2015:     5  Apr    14  May    24  May    31  May     4  Jun
2016:    27  Mar     5  May    15  May    22  May    26  May
2017:    16  Apr    25  May     4  Jun    11  Jun    15  Jun
2018:     1  Apr    10  May    20  May    27  May    31  May
2019:    21  Apr    30  May     9  Jun    16  Jun    20  Jun
2020:    12  Apr    21  May    31  May     7  Jun    11  Jun

```



## Erlang

```erlang
-module(holidays).
-export([task/0]).

offsets(easter)    ->  0;
offsets(ascension) -> 39;
offsets(pentecost) -> 49;
offsets(trinity)   -> 56;
offsets(corpus)    -> 60.

month(Month) ->
    element(Month, { "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec" } ).

easter_date(Year) ->
    A = Year rem 19,
    B = Year div 100,
    C = Year rem 100,
    D = B div 4,
    E = B rem 4,
    F = (B + 8) div 25,
    G = (B - F + 1) div 3,
    H = (19*A + B - D - G + 15) rem 30,
    I = C div 4,
    K = C rem 4,
    L = (32 + 2*E + 2*I - H - K) rem 7,
    M = (A + 11*H + 22*L) div 451,
    Numerator = H + L - 7*M + 114,
    Month = Numerator div 31,
    Day = Numerator rem 31 + 1,
    {Year, Month, Day}.

holidays() ->
    [ easter, ascension, pentecost, trinity, corpus ].

holidays(Year) ->
    io:format("~4w:", [Year]),
    Gday = calendar:date_to_gregorian_days(easter_date(Year)),
    holidays(Gday, holidays()).

holidays(_, []) ->
    io:format("~n");
holidays(Gday, [H | T]) ->
    Offset = offsets(H),
    {_Year, Month, Day} = calendar:gregorian_days_to_date(Gday + Offset),
    io:format("~6w  ~3s", [Day, month(Month)]),
    holidays(Gday, T).

title([]) ->
    io:format("~n");
title([H | T]) ->
    io:format("  ~9s", [H]),
    title(T).

task() ->
    io:format("Year:"),
    title(holidays()),
    task(400, 2100, 100),
    task(2010, 2020, 1).

task(Year, Max, _) when Year > Max ->
    io:format("~n");
task(Year, Max, Step) ->
    holidays(Year),
    task(Year+Step, Max, Step).
```


```txt

1> holidays:task().
Year:     easter  ascension  pentecost    trinity     corpus
 400:     2  Apr    11  May    21  May    28  May     1  Jun
 500:     4  Apr    13  May    23  May    30  May     3  Jun
 600:    13  Apr    22  May     1  Jun     8  Jun    12  Jun
 700:    15  Apr    24  May     3  Jun    10  Jun    14  Jun
 800:    23  Apr     1  Jun    11  Jun    18  Jun    22  Jun
 900:    28  Mar     6  May    16  May    23  May    27  May
1000:    30  Mar     8  May    18  May    25  May    29  May
1100:     8  Apr    17  May    27  May     3  Jun     7  Jun
1200:     9  Apr    18  May    28  May     4  Jun     8  Jun
1300:    18  Apr    27  May     6  Jun    13  Jun    17  Jun
1400:    20  Apr    29  May     8  Jun    15  Jun    19  Jun
1500:     1  Apr    10  May    20  May    27  May    31  May
1600:     2  Apr    11  May    21  May    28  May     1  Jun
1700:    11  Apr    20  May    30  May     6  Jun    10  Jun
1800:    13  Apr    22  May     1  Jun     8  Jun    12  Jun
1900:    15  Apr    24  May     3  Jun    10  Jun    14  Jun
2000:    23  Apr     1  Jun    11  Jun    18  Jun    22  Jun
2100:    28  Mar     6  May    16  May    23  May    27  May

2010:     4  Apr    13  May    23  May    30  May     3  Jun
2011:    24  Apr     2  Jun    12  Jun    19  Jun    23  Jun
2012:     8  Apr    17  May    27  May     3  Jun     7  Jun
2013:    31  Mar     9  May    19  May    26  May    30  May
2014:    20  Apr    29  May     8  Jun    15  Jun    19  Jun
2015:     5  Apr    14  May    24  May    31  May     4  Jun
2016:    27  Mar     5  May    15  May    22  May    26  May
2017:    16  Apr    25  May     4  Jun    11  Jun    15  Jun
2018:     1  Apr    10  May    20  May    27  May    31  May
2019:    21  Apr    30  May     9  Jun    16  Jun    20  Jun
2020:    12  Apr    21  May    31  May     7  Jun    11  Jun

ok

```



## Forth


```forth

variable year

: ea_a year @ 19 mod ;
: ea_b year @ 100 / ;
: ea_c year @ 100 mod ;
: ea_d ea_b 4 / ;
: ea_e ea_b 4 mod ;
: ea_f ea_b 8 + 25 / ;
: ea_g ea_b ea_f - 1 + 3 / ;
: ea_h 19 ea_a * ea_b + ea_d - ea_g - 15 + 30 mod ;
: ea_i ea_c 4 / ;
: ea_k ea_c 4 mod ;
: ea_l 32 2 ea_e * + 2 ea_i * + ea_h - ea_k - 7 mod ;
: ea_m ea_a 11 ea_h * + 22 ea_i * + 451 / ;
: ea_numerator ea_h ea_l + 7 ea_m * - 114 + ;
: ea_month ea_numerator 31 / ;
: ea_day ea_numerator 31 mod 1 + ;
: bs ( -- backspace )
  8 emit ;

: monthname ( monthnum -- name )
  case
  1 of ." Jan" endof
  2 of ." Feb" endof
  3 of ." Mar" endof
  4 of ." Apr" endof
  5 of ." May" endof
  6 of ." Jun" endof
  7 of ." Jul" endof
  8 of ." Aug" endof
  9 of ." Sep" endof
  10 of ." Oct" endof
  11 of ." Nov" endof
  12 of ." Dec" endof
  dup . ." wrong input"
  endcase
;

: eastern ( year -- )
  dup year ! . bs ." :" space ea_day . bs space ea_month monthname ;

: main
  cr .\" year: eastern" cr 2200 400
  DO i eastern cr 100
  +LOOP
  cr 2021 2010
  DO i eastern cr
  LOOP
  ;

```

Output:

```txt

year: eastern
400: 2 Apr
500: 4 Apr
600: 13 Apr
700: 15 Apr
800: 23 Apr
900: 28 Mar
1000: 30 Mar
1100: 8 Apr
1200: 9 Apr
1300: 18 Apr
1400: 20 Apr
1500: 1 Apr
1600: 2 Apr
1700: 11 Apr
1800: 13 Apr
1900: 15 Apr
2000: 23 Apr
2100: 28 Mar

2010: 4 Apr
2011: 24 Apr
2012: 8 Apr
2013: 31 Mar
2014: 20 Apr
2015: 5 Apr
2016: 27 Mar
2017: 16 Apr
2018: 1 Apr
2019: 21 Apr
2020: 12 Apr

```



## Fortran


```fortran
      subroutine easter(year,month,day)
c Easter sunday
c
c Input
c   year
c Output
c   month
c   day
c
c See:
c Jean Meeus, "Astronomical Formulae for Calculators",
c 4th edition, Willmann-Bell, 1988, p.31
      implicit integer(a-z)
      a=mod(year,19)
      b=year/100
      c=mod(year,100)
      d=b/4
      e=mod(b,4)
      f=(b+8)/25
      g=(b-f+1)/3
      h=mod(19*a+b-d-g+15,30)
      i=c/4
      k=mod(c,4)
      l=mod(32+2*e+2*i-h-k,7)
      m=(a+11*h+22*l)/451
      n=h+l-7*m+114
      month=n/31
      day=mod(n,31)+1
      end
```



## Go

This solution takes the ALGOL 68 as a reference solution, using its math for calculating the holidays, and reproducing its output.

```go
package main

import (
    "fmt"
    "time"
)

type holiday struct {
    time.Time
}

func easter(y int) holiday {
    c := y / 100
    n := mod(y, 19)
    i := mod(c-c/4-(c-(c-17)/25)/3+19*n+15, 30)
    i -= (i / 28) * (1 - (i/28)*(29/(i+1))*((21-n)/11))
    l := i - mod(y+y/4+i+2-c+c/4, 7)
    m := 3 + (l+40)/44
    d := l + 28 - 31*(m/4)
    return holiday{time.Date(y, time.Month(m), d, 0, 0, 0, 0, time.UTC)}
}

func mod(a, n int) int {
    r := a % n
    if r < 0 {
        return r + n
    }
    return r
}

func (h holiday) addDays(d int) holiday {
    return holiday{h.Add(time.Duration(d) * 24 * time.Hour)}
}

type easterRelated struct {
    easter, ascension, pentecost, trinity, corpusChristi holiday
}

func newEasterRelated(y int) (er easterRelated) {
    er.easter = easter(y)
    er.ascension = er.easter.addDays(39)
    er.pentecost = er.ascension.addDays(10)
    er.trinity = er.pentecost.addDays(7)
    er.corpusChristi = er.trinity.addDays(4)
    return
}

func (er easterRelated) print() {
    const wdmdm = ": Mon _2 Jan"
    fmt.Printf("%4d %s, %s, %s, %s, %s\n",
        er.easter.Year(),
        er.easter.Format("Easter"+wdmdm),
        er.ascension.Format("Ascension"+wdmdm),
        er.pentecost.Format("Pentecost"+wdmdm),
        er.trinity.Format("Trinity"+wdmdm),
        er.corpusChristi.Format("Corpus"+wdmdm))
}

func main() {
    fmt.Println("Christian holidays, related to Easter, " +
        "for each centennial from 400 to 2100 CE:")
    for y := 400; y <= 2100; y += 100 {
        newEasterRelated(y).print()
    }
    fmt.Println("\nChristian holidays, related to Easter, " +
        "for years from 2010 to 2020 CE:")
    for y := 2010; y <= 2020; y++ {
        newEasterRelated(y).print()
    }
}
```

Output:

```txt

Christian holidays, related to Easter, for each centennial from 400 to 2100 CE:
 400 Easter: Sun  2 Apr, Ascension: Thu 11 May, Pentecost: Sun 21 May, Trinity: Sun 28 May, Corpus: Thu  1 Jun
 500 Easter: Sun  4 Apr, Ascension: Thu 13 May, Pentecost: Sun 23 May, Trinity: Sun 30 May, Corpus: Thu  3 Jun
 600 Easter: Sun 13 Apr, Ascension: Thu 22 May, Pentecost: Sun  1 Jun, Trinity: Sun  8 Jun, Corpus: Thu 12 Jun
 700 Easter: Sun 15 Apr, Ascension: Thu 24 May, Pentecost: Sun  3 Jun, Trinity: Sun 10 Jun, Corpus: Thu 14 Jun
 800 Easter: Sun 23 Apr, Ascension: Thu  1 Jun, Pentecost: Sun 11 Jun, Trinity: Sun 18 Jun, Corpus: Thu 22 Jun
 900 Easter: Sun 28 Mar, Ascension: Thu  6 May, Pentecost: Sun 16 May, Trinity: Sun 23 May, Corpus: Thu 27 May
1000 Easter: Sun 30 Mar, Ascension: Thu  8 May, Pentecost: Sun 18 May, Trinity: Sun 25 May, Corpus: Thu 29 May
1100 Easter: Sun  8 Apr, Ascension: Thu 17 May, Pentecost: Sun 27 May, Trinity: Sun  3 Jun, Corpus: Thu  7 Jun
1200 Easter: Sun  9 Apr, Ascension: Thu 18 May, Pentecost: Sun 28 May, Trinity: Sun  4 Jun, Corpus: Thu  8 Jun
1300 Easter: Sun 18 Apr, Ascension: Thu 27 May, Pentecost: Sun  6 Jun, Trinity: Sun 13 Jun, Corpus: Thu 17 Jun
1400 Easter: Sun 20 Apr, Ascension: Thu 29 May, Pentecost: Sun  8 Jun, Trinity: Sun 15 Jun, Corpus: Thu 19 Jun
1500 Easter: Sun  1 Apr, Ascension: Thu 10 May, Pentecost: Sun 20 May, Trinity: Sun 27 May, Corpus: Thu 31 May
1600 Easter: Sun  2 Apr, Ascension: Thu 11 May, Pentecost: Sun 21 May, Trinity: Sun 28 May, Corpus: Thu  1 Jun
1700 Easter: Sun 11 Apr, Ascension: Thu 20 May, Pentecost: Sun 30 May, Trinity: Sun  6 Jun, Corpus: Thu 10 Jun
1800 Easter: Sun 13 Apr, Ascension: Thu 22 May, Pentecost: Sun  1 Jun, Trinity: Sun  8 Jun, Corpus: Thu 12 Jun
1900 Easter: Sun 15 Apr, Ascension: Thu 24 May, Pentecost: Sun  3 Jun, Trinity: Sun 10 Jun, Corpus: Thu 14 Jun
2000 Easter: Sun 23 Apr, Ascension: Thu  1 Jun, Pentecost: Sun 11 Jun, Trinity: Sun 18 Jun, Corpus: Thu 22 Jun
2100 Easter: Sun 28 Mar, Ascension: Thu  6 May, Pentecost: Sun 16 May, Trinity: Sun 23 May, Corpus: Thu 27 May

Christian holidays, related to Easter, for years from 2010 to 2020 CE:
2010 Easter: Sun  4 Apr, Ascension: Thu 13 May, Pentecost: Sun 23 May, Trinity: Sun 30 May, Corpus: Thu  3 Jun
2011 Easter: Sun 24 Apr, Ascension: Thu  2 Jun, Pentecost: Sun 12 Jun, Trinity: Sun 19 Jun, Corpus: Thu 23 Jun
2012 Easter: Sun  8 Apr, Ascension: Thu 17 May, Pentecost: Sun 27 May, Trinity: Sun  3 Jun, Corpus: Thu  7 Jun
2013 Easter: Sun 31 Mar, Ascension: Thu  9 May, Pentecost: Sun 19 May, Trinity: Sun 26 May, Corpus: Thu 30 May
2014 Easter: Sun 20 Apr, Ascension: Thu 29 May, Pentecost: Sun  8 Jun, Trinity: Sun 15 Jun, Corpus: Thu 19 Jun
2015 Easter: Sun  5 Apr, Ascension: Thu 14 May, Pentecost: Sun 24 May, Trinity: Sun 31 May, Corpus: Thu  4 Jun
2016 Easter: Sun 27 Mar, Ascension: Thu  5 May, Pentecost: Sun 15 May, Trinity: Sun 22 May, Corpus: Thu 26 May
2017 Easter: Sun 16 Apr, Ascension: Thu 25 May, Pentecost: Sun  4 Jun, Trinity: Sun 11 Jun, Corpus: Thu 15 Jun
2018 Easter: Sun  1 Apr, Ascension: Thu 10 May, Pentecost: Sun 20 May, Trinity: Sun 27 May, Corpus: Thu 31 May
2019 Easter: Sun 21 Apr, Ascension: Thu 30 May, Pentecost: Sun  9 Jun, Trinity: Sun 16 Jun, Corpus: Thu 20 Jun
2020 Easter: Sun 12 Apr, Ascension: Thu 21 May, Pentecost: Sun 31 May, Trinity: Sun  7 Jun, Corpus: Thu 11 Jun

```


=={{header|Icon}} and {{header|Unicon}}==
This is a modified translation of the C# code with specialized (limited) date calculation procedures added.

Aside from the already noted issues and inconsistencies with extending these calculations back centuries, the calculation of the correct and proper date of Easter has historically been responsible for massive conflict in Western history including excommunications, near wars, and contributing to the splitting of the Catholic Church.  For more see [http://mangsbatpage.433rd.com/2008/03/calculating-easter.html Calculating Easter @ Mang's Bat Page].


```Icon
link printf

procedure main()
   printf("Christian holidays, related to Easter, for each centennial from 400 to 2100 CE:\n")
   every year := 400 to 2100 by 100 do
      OutputHolidays(year)

   printf("\nChristian holidays, related to Easter, for years from 2010 to 2020 CE:\n")
      every year := 2010 to 2020 do
          OutputHolidays(year)
end

procedure OutputHolidays(year)
static ho,hoo,monL,dayL
initial {
   ho := table()
   ho["Easter"] := 0
   ho["Ascension"] := 39
   ho["Pentecost"] := 49
   ho["Trinity"] := 56
   ho["Corpus"] := 60
   hoo := ["Easter","Ascension","Pentecost","Trinity","Corpus"] # order
   monL := ["Jan", "Feb", "Mar", "Apr", "May", "Jun",
             "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"]
   dayL := ["Sun", "Mon", "Tue", "Wed", "Thu", "Fri","Sat"]
   }

   easter := CalculateEaster(year)
   printf("%d ",year)
   every d := Add2Easter(easter,ho[k := !hoo]) do
      printf("%d: %s %d %s ", k, dayL[d.wday], d.day, monL[d.month])
   printf("\n")
end

record daterec(year,month,day,wday)

procedure CalculateEaster(year)  #: Calculates Gregorian Easter
   a := year % 19
   b := year / 100
   c := year % 100
   d := b / 4
   e := b % 4
   f := (b + 8) / 25
   g := (b - f + 1) / 3
   h := (19 * a + b - d - g + 15) % 30
   i := c / 4
   k := c % 4
   l := (32 + 2 * e + 2 * i - h - k) % 7
   m := (a + 11 * h + 22 * l) / 451
   n := h + l - 7 * m + 114
   return daterec(year, n / 31, (n % 31) + 1,1)  # year,month, day, "Sun"=1
end

procedure Add2Easter(dr,days)    #: very limited date addition
static dom
initial dom := [,,31,30,31,30,]  # limited days per month
   dr := copy(dr)
   dr.day +:= days
   dr.wday := (dr.wday + days - 1 ) % 7 + 1 # rely upon Easter=Sunday
   while dr.day > dom[dr.month] do {
      dr.day   -:= dom[dr.month]
      dr.month +:= 1
      }
   return dr
end
```


Output:
```txt
Christian holidays, related to Easter, for each centennial from 400 to 2100 CE:
400 Easter: Sun 2 Apr Ascension: Thu 11 May Pentecost: Sun 21 May Trinity: Sun 28 May Corpus: Thu 1 Jun
500 Easter: Sun 4 Apr Ascension: Thu 13 May Pentecost: Sun 23 May Trinity: Sun 30 May Corpus: Thu 3 Jun
600 Easter: Sun 13 Apr Ascension: Thu 22 May Pentecost: Sun 1 Jun Trinity: Sun 8 Jun Corpus: Thu 12 Jun
700 Easter: Sun 15 Apr Ascension: Thu 24 May Pentecost: Sun 3 Jun Trinity: Sun 10 Jun Corpus: Thu 14 Jun
800 Easter: Sun 23 Apr Ascension: Thu 1 Jun Pentecost: Sun 11 Jun Trinity: Sun 18 Jun Corpus: Thu 22 Jun
900 Easter: Sun 28 Mar Ascension: Thu 6 May Pentecost: Sun 16 May Trinity: Sun 23 May Corpus: Thu 27 May
1000 Easter: Sun 30 Mar Ascension: Thu 8 May Pentecost: Sun 18 May Trinity: Sun 25 May Corpus: Thu 29 May
1100 Easter: Sun 8 Apr Ascension: Thu 17 May Pentecost: Sun 27 May Trinity: Sun 3 Jun Corpus: Thu 7 Jun
1200 Easter: Sun 9 Apr Ascension: Thu 18 May Pentecost: Sun 28 May Trinity: Sun 4 Jun Corpus: Thu 8 Jun
1300 Easter: Sun 18 Apr Ascension: Thu 27 May Pentecost: Sun 6 Jun Trinity: Sun 13 Jun Corpus: Thu 17 Jun
1400 Easter: Sun 20 Apr Ascension: Thu 29 May Pentecost: Sun 8 Jun Trinity: Sun 15 Jun Corpus: Thu 19 Jun
1500 Easter: Sun 1 Apr Ascension: Thu 10 May Pentecost: Sun 20 May Trinity: Sun 27 May Corpus: Thu 31 May
1600 Easter: Sun 2 Apr Ascension: Thu 11 May Pentecost: Sun 21 May Trinity: Sun 28 May Corpus: Thu 1 Jun
1700 Easter: Sun 11 Apr Ascension: Thu 20 May Pentecost: Sun 30 May Trinity: Sun 6 Jun Corpus: Thu 10 Jun
1800 Easter: Sun 13 Apr Ascension: Thu 22 May Pentecost: Sun 1 Jun Trinity: Sun 8 Jun Corpus: Thu 12 Jun
1900 Easter: Sun 15 Apr Ascension: Thu 24 May Pentecost: Sun 3 Jun Trinity: Sun 10 Jun Corpus: Thu 14 Jun
2000 Easter: Sun 23 Apr Ascension: Thu 1 Jun Pentecost: Sun 11 Jun Trinity: Sun 18 Jun Corpus: Thu 22 Jun
2100 Easter: Sun 28 Mar Ascension: Thu 6 May Pentecost: Sun 16 May Trinity: Sun 23 May Corpus: Thu 27 May

Christian holidays, related to Easter, for years from 2010 to 2020 CE:
2010 Easter: Sun 4 Apr Ascension: Thu 13 May Pentecost: Sun 23 May Trinity: Sun 30 May Corpus: Thu 3 Jun
2011 Easter: Sun 24 Apr Ascension: Thu 2 Jun Pentecost: Sun 12 Jun Trinity: Sun 19 Jun Corpus: Thu 23 Jun
2012 Easter: Sun 8 Apr Ascension: Thu 17 May Pentecost: Sun 27 May Trinity: Sun 3 Jun Corpus: Thu 7 Jun
2013 Easter: Sun 31 Mar Ascension: Thu 9 May Pentecost: Sun 19 May Trinity: Sun 26 May Corpus: Thu 30 May
2014 Easter: Sun 20 Apr Ascension: Thu 29 May Pentecost: Sun 8 Jun Trinity: Sun 15 Jun Corpus: Thu 19 Jun
2015 Easter: Sun 5 Apr Ascension: Thu 14 May Pentecost: Sun 24 May Trinity: Sun 31 May Corpus: Thu 4 Jun
2016 Easter: Sun 27 Mar Ascension: Thu 5 May Pentecost: Sun 15 May Trinity: Sun 22 May Corpus: Thu 26 May
2017 Easter: Sun 16 Apr Ascension: Thu 25 May Pentecost: Sun 4 Jun Trinity: Sun 11 Jun Corpus: Thu 15 Jun
2018 Easter: Sun 1 Apr Ascension: Thu 10 May Pentecost: Sun 20 May Trinity: Sun 27 May Corpus: Thu 31 May
2019 Easter: Sun 21 Apr Ascension: Thu 30 May Pentecost: Sun 9 Jun Trinity: Sun 16 Jun Corpus: Thu 20 Jun
2020 Easter: Sun 12 Apr Ascension: Thu 21 May Pentecost: Sun 31 May Trinity: Sun 7 Jun Corpus: Thu 11 Jun
```



## J



### Issues and Ambiguities


Caution: This task is currently self-contradictory, thus consistent results are impossible.  See the talk page for some further discussion of this issue.

That said, the only calendar where the specified date range can be meaningful is the Julian calendar, which is currently used by Eastern Christianity for determining when to celebrate easter.

However, Corpus Christi is a Western Catholic holiday.  Then again, Corpus Christi was not celebrated anywhere prior to the 13th century and in some countries it is celebrated on a Thursday and in other countries it is celebrated on a Sunday.  So I have chosen to ignore this holiday for now.

In Eastern Christianity, Trinity Sunday is the same day as Pentecost.


###  Julian Easters


This code is based on the above rationale, and http://www.merlyn.demon.co.uk/estr-bcp.htm and the wikipedia pages referenced in the task description:


```j
jed=:3 :0
  pfm=. 21 + 30 | _4 + 19 * 1 + 19|y
  sn=. 6 - 7 | 4 + <.@*&1.25 y
  dys=. 1 40 50 50 +/~sn (] + 7 | 4 + -) pfm
  y,"0 1(+/\0 0 0 31 30 31 30) (I.,"0]-<:@I.{[) dys
)
```


Required example:


```j
   jed (400 + 100* i.17),(2010 + i.11),2100
```

output:
<pre style="height: 24ex; overflow: scroll"> 400 4  1
 400 5 10
 400 5 20
 400 5 20

 500 4  2
 500 5 11
 500 5 21
 500 5 21

 600 4 10
 600 5 19
 600 5 29
 600 5 29

 700 4 11
 700 5 20
 700 5 30
 700 5 30

 800 4 19
 800 5 28
 800 6  7
 800 6  7

 900 4 20
 900 5 29
 900 6  8
 900 6  8

1000 3 31
1000 5  9
1000 5 19
1000 5 19

1100 4  1
1100 5 10
1100 5 20
1100 5 20

1200 4  9
1200 5 18
1200 5 28
1200 5 28

1300 4 10
1300 5 19
1300 5 29
1300 5 29

1400 4 18
1400 5 27
1400 6  6
1400 6  6

1500 4 19
1500 5 28
1500 6  7
1500 6  7

1600 3 23
1600 5  1
1600 5 11
1600 5 11

1700 3 31
1700 5  9
1700 5 19
1700 5 19

1800 4  8
1800 5 17
1800 5 27
1800 5 27

1900 4  9
1900 5 18
1900 5 28
1900 5 28

2000 4 17
2000 5 26
2000 6  5
2000 6  5

2010 3 22
2010 4 30
2010 5 10
2010 5 10

2011 4 11
2011 5 20
2011 5 30
2011 5 30

2012 4  2
2012 5 11
2012 5 21
2012 5 21

2013 4 22
2013 5 31
2013 6 10
2013 6 10

2014 4  7
2014 5 16
2014 5 26
2014 5 26

2015 3 30
2015 5  8
2015 5 18
2015 5 18

2016 4 18
2016 5 27
2016 6  6
2016 6  6

2017 4  3
2017 5 12
2017 5 22
2017 5 22

2018 3 26
2018 5  4
2018 5 14
2018 5 14

2019 4 15
2019 5 24
2019 6  3
2019 6  3

2020 4  6
2020 5 15
2020 5 25
2020 5 25

2100 4 18
2100 5 27
2100 6  6
2100 6  6
```



###  Gregorian Easters


Other entries on this page are showing gregorian easter results despite the nonsensical character of those results (for example, no country celebrated easter using the Gregorian calendar before 1583).

Nevertheless, here is an implementation which reproduces those numbers, based on the same resource I used for the Julian easters:


```j
ged=:3 :0
  ce =. <. y%100
  GN =.  1 + 19 | y
  CY =.      30 | 23 + (<.4 %~ 3 * ce+1) - <. 25 %~ 13 + ce*8
  YR =.     400 | y
  SN =. 6 -   7 | 6 + YR + (<. YR%4) -  <. YR%100
  dm =. 21 + 30 | 3 + CY + 19*GN
  DM =. dm - 49 < dm+11 < GN
  dys=. 0 39 49 56 60 +/~ DM + 1 + 7 | 60+SN-DM
  y,"0 1(+/\0 0 0 31 30 31 30) (I.,"0]-<:@I.{[) dys
)
```


And here is the required example (and note that I am including the same Corpus Christi feast date here that others are using, because that makes sense with recent Gregorian dates even though it is a nonsense value for earlier dates):


```j
   ged (400 + 100* i.17),(2010 + i.11),2100
```

output (in each block of dates, they are, in order  Easter, Ascension Thursday, Pentecost, Trinity Sunday, and Corpus Christi feast):
<pre style="height: 24ex; overflow: scroll"> 400 4  2
 400 5 11
 400 5 21
 400 5 28
 400 6  1

 500 4  4
 500 5 13
 500 5 23
 500 5 30
 500 6  3

 600 4 13
 600 5 22
 600 6  1
 600 6  8
 600 6 12

 700 4 15
 700 5 24
 700 6  3
 700 6 10
 700 6 14

 800 4 23
 800 6  1
 800 6 11
 800 6 18
 800 6 22

 900 3 28
 900 5  6
 900 5 16
 900 5 23
 900 5 27

1000 3 30
1000 5  8
1000 5 18
1000 5 25
1000 5 29

1100 4  8
1100 5 17
1100 5 27
1100 6  3
1100 6  7

1200 4  9
1200 5 18
1200 5 28
1200 6  4
1200 6  8

1300 4 18
1300 5 27
1300 6  6
1300 6 13
1300 6 17

1400 4 20
1400 5 29
1400 6  8
1400 6 15
1400 6 19

1500 4  1
1500 5 10
1500 5 20
1500 5 27
1500 5 31

1600 4  2
1600 5 11
1600 5 21
1600 5 28
1600 6  1

1700 4 11
1700 5 20
1700 5 30
1700 6  6
1700 6 10

1800 4 13
1800 5 22
1800 6  1
1800 6  8
1800 6 12

1900 4 15
1900 5 24
1900 6  3
1900 6 10
1900 6 14

2000 4 23
2000 6  1
2000 6 11
2000 6 18
2000 6 22

2010 4  4
2010 5 13
2010 5 23
2010 5 30
2010 6  3

2011 4 24
2011 6  2
2011 6 12
2011 6 19
2011 6 23

2012 4  8
2012 5 17
2012 5 27
2012 6  3
2012 6  7

2013 3 31
2013 5  9
2013 5 19
2013 5 26
2013 5 30

2014 4 20
2014 5 29
2014 6  8
2014 6 15
2014 6 19

2015 4  5
2015 5 14
2015 5 24
2015 5 31
2015 6  4

2016 3 27
2016 5  5
2016 5 15
2016 5 22
2016 5 26

2017 4 16
2017 5 25
2017 6  4
2017 6 11
2017 6 15

2018 4  1
2018 5 10
2018 5 20
2018 5 27
2018 5 31

2019 4 21
2019 5 30
2019 6  9
2019 6 16
2019 6 20

2020 4 12
2020 5 21
2020 5 31
2020 6  7
2020 6 11

2100 3 28
2100 5  6
2100 5 16
2100 5 23
2100 5 27
```



## Java

Translation of C Sharp via D

```java
import java.text.DateFormatSymbols;
import java.util.*;

public class EasterRelatedHolidays {

    final static Map<String, Integer> holidayOffsets;

    static {
        holidayOffsets = new LinkedHashMap<>();
        holidayOffsets.put("Easter", 0);
        holidayOffsets.put("Ascension", 39);
        holidayOffsets.put("Pentecost", 10);
        holidayOffsets.put("Trinity", 7);
        holidayOffsets.put("Corpus", 4);
    }

    public static void main(String[] args) {
        System.out.println("Christian holidays, related to Easter,"
                + " for each centennial from 400 to 2100 CE:");

        for (int y = 400; y <= 2100; y += 100)
            printEasterRelatedHolidays(y);

        System.out.println("\nChristian holidays, related to Easter,"
                + " for years from 2010 to 2020 CE:");
        for (int y = 2010; y < 2021; y++)
            printEasterRelatedHolidays(y);
    }

    static void printEasterRelatedHolidays(int year) {
        final int a = year % 19;
        final int b = year / 100;
        final int c = year % 100;
        final int d = b / 4;
        final int e = b % 4;
        final int f = (b + 8) / 25;
        final int g = (b - f + 1) / 3;
        final int h = (19 * a + b - d - g + 15) % 30;
        final int i = c / 4;
        final int k = c % 4;
        final int l = (32 + 2 * e + 2 * i - h - k) % 7;
        final int m = (a + 11 * h + 22 * l) / 451;
        final int n = h + l - 7 * m + 114;
        final int month = n / 31 - 1;
        final int day = (n % 31) + 1;

        Calendar date = new GregorianCalendar(year, month, day);
        String[] months = new DateFormatSymbols(Locale.US).getShortMonths();

        System.out.printf("%4d ", year);
        for (String hd : holidayOffsets.keySet()) {
            date.add(Calendar.DATE, holidayOffsets.get(hd));
            System.out.printf("%s: %2d %s  ", hd,
                    date.get(Calendar.DAY_OF_MONTH),
                    months[date.get(Calendar.MONTH)]);
        }
        System.out.println();
    }
}
```


Output:


```txt
Christian holidays, related to Easter, for each centennial from 400 to 2100 CE:
 400 Easter:  2 Apr  Ascension: 11 May  Pentecost: 21 May  Trinity: 28 May  Corpus:  1 Jun
 500 Easter:  4 Apr  Ascension: 13 May  Pentecost: 23 May  Trinity: 30 May  Corpus:  3 Jun
 600 Easter: 13 Apr  Ascension: 22 May  Pentecost:  1 Jun  Trinity:  8 Jun  Corpus: 12 Jun
 700 Easter: 15 Apr  Ascension: 24 May  Pentecost:  3 Jun  Trinity: 10 Jun  Corpus: 14 Jun
 800 Easter: 23 Apr  Ascension:  1 Jun  Pentecost: 11 Jun  Trinity: 18 Jun  Corpus: 22 Jun
 900 Easter: 28 Mar  Ascension:  6 May  Pentecost: 16 May  Trinity: 23 May  Corpus: 27 May
1000 Easter: 30 Mar  Ascension:  8 May  Pentecost: 18 May  Trinity: 25 May  Corpus: 29 May
1100 Easter:  8 Apr  Ascension: 17 May  Pentecost: 27 May  Trinity:  3 Jun  Corpus:  7 Jun
1200 Easter:  9 Apr  Ascension: 18 May  Pentecost: 28 May  Trinity:  4 Jun  Corpus:  8 Jun
1300 Easter: 18 Apr  Ascension: 27 May  Pentecost:  6 Jun  Trinity: 13 Jun  Corpus: 17 Jun
1400 Easter: 20 Apr  Ascension: 29 May  Pentecost:  8 Jun  Trinity: 15 Jun  Corpus: 19 Jun
1500 Easter:  1 Apr  Ascension: 10 May  Pentecost: 20 May  Trinity: 27 May  Corpus: 31 May
1600 Easter:  2 Apr  Ascension: 11 May  Pentecost: 21 May  Trinity: 28 May  Corpus:  1 Jun
1700 Easter: 11 Apr  Ascension: 20 May  Pentecost: 30 May  Trinity:  6 Jun  Corpus: 10 Jun
1800 Easter: 13 Apr  Ascension: 22 May  Pentecost:  1 Jun  Trinity:  8 Jun  Corpus: 12 Jun
1900 Easter: 15 Apr  Ascension: 24 May  Pentecost:  3 Jun  Trinity: 10 Jun  Corpus: 14 Jun
2000 Easter: 23 Apr  Ascension:  1 Jun  Pentecost: 11 Jun  Trinity: 18 Jun  Corpus: 22 Jun
2100 Easter: 28 Mar  Ascension:  6 May  Pentecost: 16 May  Trinity: 23 May  Corpus: 27 May

Christian holidays, related to Easter, for years from 2010 to 2020 CE:
2010 Easter:  4 Apr  Ascension: 13 May  Pentecost: 23 May  Trinity: 30 May  Corpus:  3 Jun
2011 Easter: 24 Apr  Ascension:  2 Jun  Pentecost: 12 Jun  Trinity: 19 Jun  Corpus: 23 Jun
2012 Easter:  8 Apr  Ascension: 17 May  Pentecost: 27 May  Trinity:  3 Jun  Corpus:  7 Jun
2013 Easter: 31 Mar  Ascension:  9 May  Pentecost: 19 May  Trinity: 26 May  Corpus: 30 May
2014 Easter: 20 Apr  Ascension: 29 May  Pentecost:  8 Jun  Trinity: 15 Jun  Corpus: 19 Jun
2015 Easter:  5 Apr  Ascension: 14 May  Pentecost: 24 May  Trinity: 31 May  Corpus:  4 Jun
2016 Easter: 27 Mar  Ascension:  5 May  Pentecost: 15 May  Trinity: 22 May  Corpus: 26 May
2017 Easter: 16 Apr  Ascension: 25 May  Pentecost:  4 Jun  Trinity: 11 Jun  Corpus: 15 Jun
2018 Easter:  1 Apr  Ascension: 10 May  Pentecost: 20 May  Trinity: 27 May  Corpus: 31 May
2019 Easter: 21 Apr  Ascension: 30 May  Pentecost:  9 Jun  Trinity: 16 Jun  Corpus: 20 Jun
2020 Easter: 12 Apr  Ascension: 21 May  Pentecost: 31 May  Trinity:  7 Jun  Corpus: 11 Jun
```




## Julia


```julia
# v0.6

using Dates

function easter(year::Int)::Date
    a = rem(year, 19)
    b, c = divrem(year, 100)
    d = rem(19a + b - div(b, 4) - div(b - div(b + 8, 25) + 1, 3) + 15, 30)
    e = rem(32 + 2 * rem(b, 4) + 2 * div(c, 4) - d - rem(c, 4), 7)
    f = d + e - 7 * div(a + 11d + 22e, 451) + 114
    month, day = divrem(f, 31)
    day += 1
    return Date(year, month, day)
end

function holiday_values(year::Int)::Dict{String,Date}
    offsets = Dict{String,Day}("Easter" => Day(0), "Ascension" => Day(39),
    "Pentecost" => Day(49), "Trinity" => Day(56), "Corpus" => Day(60))
    easterdate = easter(year)
    rst = Dict{String,Date}(holiday => easterdate + days for (holiday, days) in offsets)
    return rst
end

function holiday2str(year::Int)::String
    function holiday2str(holiday::String, date::Date)::String
        dayname, daynumb, month = dayabbr(date), day(date), monthabbr(date)
        return @sprintf "%s: %s %2i %s" holiday dayname daynumb month
    end
    cal = holiday_values(year)
    rst = join(collect(holiday2str(hday, cal[hday]) for hday in
    ("Easter", "Ascension", "Pentecost", "Trinity", "Corpus")), ", ")
    return "$year -> " * rst
end

println("\nChristian holidays, related to Easter, for each centennial from 400 to 2100 CE:")
for yr in 400:100:2200
    println(holiday2str(yr))
end

println("\nChristian holidays, related to Easter, for years from 2010 to 2020 CE:")
for yr in 2010:2020
    println(holiday2str(yr))
end
```


```txt
Christian holidays, related to Easter, for each centennial from 400 to 2100 CE:
400 -> Easter: Sun  2 Apr, Ascension: Thu 11 May, Pentecost: Sun 21 May, Trinity: Sun 28 May, Corpus: Thu  1 Jun
500 -> Easter: Sun  4 Apr, Ascension: Thu 13 May, Pentecost: Sun 23 May, Trinity: Sun 30 May, Corpus: Thu  3 Jun
600 -> Easter: Sun 13 Apr, Ascension: Thu 22 May, Pentecost: Sun  1 Jun, Trinity: Sun  8 Jun, Corpus: Thu 12 Jun
700 -> Easter: Sun 15 Apr, Ascension: Thu 24 May, Pentecost: Sun  3 Jun, Trinity: Sun 10 Jun, Corpus: Thu 14 Jun
800 -> Easter: Sun 23 Apr, Ascension: Thu  1 Jun, Pentecost: Sun 11 Jun, Trinity: Sun 18 Jun, Corpus: Thu 22 Jun
900 -> Easter: Sun 28 Mar, Ascension: Thu  6 May, Pentecost: Sun 16 May, Trinity: Sun 23 May, Corpus: Thu 27 May
1000 -> Easter: Sun 30 Mar, Ascension: Thu  8 May, Pentecost: Sun 18 May, Trinity: Sun 25 May, Corpus: Thu 29 May
1100 -> Easter: Sun  8 Apr, Ascension: Thu 17 May, Pentecost: Sun 27 May, Trinity: Sun  3 Jun, Corpus: Thu  7 Jun
1200 -> Easter: Sun  9 Apr, Ascension: Thu 18 May, Pentecost: Sun 28 May, Trinity: Sun  4 Jun, Corpus: Thu  8 Jun
1300 -> Easter: Sun 18 Apr, Ascension: Thu 27 May, Pentecost: Sun  6 Jun, Trinity: Sun 13 Jun, Corpus: Thu 17 Jun
1400 -> Easter: Sun 20 Apr, Ascension: Thu 29 May, Pentecost: Sun  8 Jun, Trinity: Sun 15 Jun, Corpus: Thu 19 Jun
1500 -> Easter: Sun  1 Apr, Ascension: Thu 10 May, Pentecost: Sun 20 May, Trinity: Sun 27 May, Corpus: Thu 31 May
1600 -> Easter: Sun  2 Apr, Ascension: Thu 11 May, Pentecost: Sun 21 May, Trinity: Sun 28 May, Corpus: Thu  1 Jun
1700 -> Easter: Sun 11 Apr, Ascension: Thu 20 May, Pentecost: Sun 30 May, Trinity: Sun  6 Jun, Corpus: Thu 10 Jun
1800 -> Easter: Sun 13 Apr, Ascension: Thu 22 May, Pentecost: Sun  1 Jun, Trinity: Sun  8 Jun, Corpus: Thu 12 Jun
1900 -> Easter: Sun 15 Apr, Ascension: Thu 24 May, Pentecost: Sun  3 Jun, Trinity: Sun 10 Jun, Corpus: Thu 14 Jun
2000 -> Easter: Sun 23 Apr, Ascension: Thu  1 Jun, Pentecost: Sun 11 Jun, Trinity: Sun 18 Jun, Corpus: Thu 22 Jun
2100 -> Easter: Sun 28 Mar, Ascension: Thu  6 May, Pentecost: Sun 16 May, Trinity: Sun 23 May, Corpus: Thu 27 May
2200 -> Easter: Sun  6 Apr, Ascension: Thu 15 May, Pentecost: Sun 25 May, Trinity: Sun  1 Jun, Corpus: Thu  5 Jun

Christian holidays, related to Easter, for years from 2010 to 2020 CE:
2010 -> Easter: Sun  4 Apr, Ascension: Thu 13 May, Pentecost: Sun 23 May, Trinity: Sun 30 May, Corpus: Thu  3 Jun
2011 -> Easter: Sun 24 Apr, Ascension: Thu  2 Jun, Pentecost: Sun 12 Jun, Trinity: Sun 19 Jun, Corpus: Thu 23 Jun
2012 -> Easter: Sun  8 Apr, Ascension: Thu 17 May, Pentecost: Sun 27 May, Trinity: Sun  3 Jun, Corpus: Thu  7 Jun
2013 -> Easter: Sun 31 Mar, Ascension: Thu  9 May, Pentecost: Sun 19 May, Trinity: Sun 26 May, Corpus: Thu 30 May
2014 -> Easter: Sun 20 Apr, Ascension: Thu 29 May, Pentecost: Sun  8 Jun, Trinity: Sun 15 Jun, Corpus: Thu 19 Jun
2015 -> Easter: Sun  5 Apr, Ascension: Thu 14 May, Pentecost: Sun 24 May, Trinity: Sun 31 May, Corpus: Thu  4 Jun
2016 -> Easter: Sun 27 Mar, Ascension: Thu  5 May, Pentecost: Sun 15 May, Trinity: Sun 22 May, Corpus: Thu 26 May
2017 -> Easter: Sun 16 Apr, Ascension: Thu 25 May, Pentecost: Sun  4 Jun, Trinity: Sun 11 Jun, Corpus: Thu 15 Jun
2018 -> Easter: Sun  1 Apr, Ascension: Thu 10 May, Pentecost: Sun 20 May, Trinity: Sun 27 May, Corpus: Thu 31 May
2019 -> Easter: Sun 21 Apr, Ascension: Thu 30 May, Pentecost: Sun  9 Jun, Trinity: Sun 16 Jun, Corpus: Thu 20 Jun
2020 -> Easter: Sun 12 Apr, Ascension: Thu 21 May, Pentecost: Sun 31 May, Trinity: Sun  7 Jun, Corpus: Thu 11 Jun
```



## Kotlin

```scala
// version 1.1.2

import java.util.Calendar
import java.util.GregorianCalendar

val holidayOffsets = listOf(
    "Easter" to 0,
    "Ascension" to 39,
    "Pentecost" to 49,
    "Trinity" to 56,
    "C/Christi" to 60
)

fun String.padCenter(n: Int): String {
    val len = this.length
    if (n <= len) return this
    return this.padStart((n + len) / 2).padEnd(n)
}

fun calculateEaster(year: Int): GregorianCalendar {
    val a = year % 19
    val b = year / 100
    val c = year % 100
    val d = b / 4
    val e = b % 4
    val f = (b + 8) / 25
    val g = (b - f + 1) / 3
    val h = (19 * a + b - d - g + 15) % 30
    val i = c / 4
    val k = c % 4
    val l = (32 + 2 * e + 2 * i - h - k) % 7
    val m = (a + 11 * h + 22 * l) / 451
    val n = h + l - 7 * m + 114
    val month = n / 31 - 1  // months indexed from 0
    val day = (n % 31) + 1
    return GregorianCalendar(year, month, day)
}

fun outputHolidays(year: Int) {
    val date = calculateEaster(year)
    print("%4d  ".format(year))
    var po = 0
    for ((h, o) in holidayOffsets) {
        date.add(Calendar.DATE, o - po)
        po = o
        print("${"%1\$td %1\$tb".format(date).padCenter(h.length)}  ")
    }
    println()
}

fun main(args: Array<String>) {
    println("Year  Easter  Ascension  Pentecost  Trinity  C/Christi")
    println(" CE   Sunday  Thursday    Sunday    Sunday   Thursday ")
    println("----  ------  ---------  ---------  -------  ---------")
    for (year in 400..2100 step 100) outputHolidays(year)
    println()
    for (year in 2010..2020) outputHolidays(year)
}
```


```txt

Year  Easter  Ascension  Pentecost  Trinity  C/Christi
 CE   Sunday  Thursday    Sunday    Sunday   Thursday
----  ------  ---------  ---------  -------  ---------
 400  02 Apr   11 May     21 May    28 May    01 Jun
 500  04 Apr   13 May     23 May    30 May    03 Jun
 600  13 Apr   22 May     01 Jun    08 Jun    12 Jun
 700  15 Apr   24 May     03 Jun    10 Jun    14 Jun
 800  23 Apr   01 Jun     11 Jun    18 Jun    22 Jun
 900  28 Mar   06 May     16 May    23 May    27 May
1000  30 Mar   08 May     18 May    25 May    29 May
1100  08 Apr   17 May     27 May    03 Jun    07 Jun
1200  09 Apr   18 May     28 May    04 Jun    08 Jun
1300  18 Apr   27 May     06 Jun    13 Jun    17 Jun
1400  20 Apr   29 May     08 Jun    15 Jun    19 Jun
1500  01 Apr   10 May     20 May    27 May    31 May
1600  02 Apr   11 May     21 May    28 May    01 Jun
1700  11 Apr   20 May     30 May    06 Jun    10 Jun
1800  13 Apr   22 May     01 Jun    08 Jun    12 Jun
1900  15 Apr   24 May     03 Jun    10 Jun    14 Jun
2000  23 Apr   01 Jun     11 Jun    18 Jun    22 Jun
2100  28 Mar   06 May     16 May    23 May    27 May

2010  04 Apr   13 May     23 May    30 May    03 Jun
2011  24 Apr   02 Jun     12 Jun    19 Jun    23 Jun
2012  08 Apr   17 May     27 May    03 Jun    07 Jun
2013  31 Mar   09 May     19 May    26 May    30 May
2014  20 Apr   29 May     08 Jun    15 Jun    19 Jun
2015  05 Apr   14 May     24 May    31 May    04 Jun
2016  27 Mar   05 May     15 May    22 May    26 May
2017  16 Apr   25 May     04 Jun    11 Jun    15 Jun
2018  01 Apr   10 May     20 May    27 May    31 May
2019  21 Apr   30 May     09 Jun    16 Jun    20 Jun
2020  12 Apr   21 May     31 May    07 Jun    11 Jun

```



## Lua

The function 'easter' is a Lua translation of a C function by Claus Tøndering.  This script relies heavily on the 'time' library, available from scilua.org.  Dates before 1582 are not supported.

```Lua
local Time = require("time")

function div (x, y) return math.floor(x / y) end

function easter (year)
    local G = year % 19
    local C = div(year, 100)
    local H = (C - div(C, 4) - div((8 * C + 13), 25) + 19 * G + 15) % 30
    local I = H - div(H, 28) * (1 - div(29, H + 1)) * (div(21 - G, 11))
    local J = (year + div(year, 4) + I + 2 - C + div(C, 4)) % 7
    local L = I - J
    local month = 3 + div(L + 40, 44)
    return month, L + 28 - 31 * div(month, 4)
end

function holidays (year)
    local dates = {}
    dates.easter = Time.date(year, easter(year))
    dates.ascension = dates.easter + Time.days(39)
    dates.pentecost = dates.easter + Time.days(49)
    dates.trinity   = dates.easter + Time.days(56)
    dates.corpus    = dates.easter + Time.days(60)
    return dates
end

function puts (...)
    for k, v in pairs{...} do io.write(tostring(v):sub(6, 10), "\t") end
end

function show (year, d)
    io.write(year, "\t")
    puts(d.easter, d.ascension, d.pentecost, d.trinity, d.corpus)
    print()
end

print("Year\tEaster\tAscen.\tPent.\tTrinity\tCorpus")
for year = 1600, 2100, 100 do show(year, holidays(year)) end
for year = 2010, 2020 do show(year, holidays(year)) end
```

Output:

```txt
Year    Easter  Ascen.  Pent.   Trinity Corpus
1600    04-02   05-11   05-21   05-28   06-01
1700    04-11   05-20   05-30   06-06   06-10
1800    04-13   05-22   06-01   06-08   06-12
1900    04-15   05-24   06-03   06-10   06-14
2000    04-23   06-01   06-11   06-18   06-22
2100    03-28   05-06   05-16   05-23   05-27
2010    04-04   05-13   05-23   05-30   06-03
2011    04-24   06-02   06-12   06-19   06-23
2012    04-08   05-17   05-27   06-03   06-07
2013    03-31   05-09   05-19   05-26   05-30
2014    04-20   05-29   06-08   06-15   06-19
2015    04-05   05-14   05-24   05-31   06-04
2016    03-27   05-05   05-15   05-22   05-26
2017    04-16   05-25   06-04   06-11   06-15
2018    04-01   05-10   05-20   05-27   05-31
2019    04-21   05-30   06-09   06-16   06-20
2020    04-12   05-21   05-31   06-07   06-11
```



## Mathematica


```Mathematica
Needs["Calendar`"];DateFormat[x_]:=DateString[x,{"DayNameShort"," ","DayShort"," ","MonthName"}]
Map[StringJoin[ToString[#]," Easter: ",DateFormat[EasterSunday[#]],
", Ascension: ",DateFormat[DaysPlus[EasterSunday[#],39]],
", Pentecost: ",DateFormat[DaysPlus[EasterSunday[#],49]],
", Trinity: ",DateFormat[DaysPlus[EasterSunday[#],56]],
", Corpus: ",DateFormat[DaysPlus[EasterSunday[#],60]]]&
,Range[400,2100,100]~Join~Range[2010,2020]]

Output
{400 Easter: Sun 2 April, Ascension: Thu 11 May, Pentecost: Sun 21 May, Trinity: Sun 28 May, Corpus: Thu 1 June,
 500 Easter: Sun 4 April, Ascension: Thu 13 May, Pentecost: Sun 23 May, Trinity: Sun 30 May, Corpus: Thu 3 June,
 600 Easter: Sun 13 April, Ascension: Thu 22 May, Pentecost: Sun 1 June, Trinity: Sun 8 June, Corpus: Thu 12 June,
 700 Easter: Sun 15 April, Ascension: Thu 24 May, Pentecost: Sun 3 June, Trinity: Sun 10 June, Corpus: Thu 14 June,
 800 Easter: Sun 23 April, Ascension: Thu 1 June, Pentecost: Sun 11 June, Trinity: Sun 18 June, Corpus: Thu 22 June,
 900 Easter: Sun 28 March, Ascension: Thu 6 May, Pentecost: Sun 16 May, Trinity: Sun 23 May, Corpus: Thu 27 May,
1000 Easter: Sun 30 March, Ascension: Thu 8 May, Pentecost: Sun 18 May, Trinity: Sun 25 May, Corpus: Thu 29 May,
1100 Easter: Sun 8 April, Ascension: Thu 17 May, Pentecost: Sun 27 May, Trinity: Sun 3 June, Corpus: Thu 7 June,
1200 Easter: Sun 9 April, Ascension: Thu 18 May, Pentecost: Sun 28 May, Trinity: Sun 4 June, Corpus: Thu 8 June,
1300 Easter: Sun 18 April, Ascension: Thu 27 May, Pentecost: Sun 6 June, Trinity: Sun 13 June, Corpus: Thu 17 June,
1400 Easter: Sun 20 April, Ascension: Thu 29 May, Pentecost: Sun 8 June, Trinity: Sun 15 June, Corpus: Thu 19 June,
1500 Easter: Sun 1 April, Ascension: Thu 10 May, Pentecost: Sun 20 May, Trinity: Sun 27 May, Corpus: Thu 31 May,
1600 Easter: Sun 2 April, Ascension: Thu 11 May, Pentecost: Sun 21 May, Trinity: Sun 28 May, Corpus: Thu 1 June,
1700 Easter: Sun 11 April, Ascension: Thu 20 May, Pentecost: Sun 30 May, Trinity: Sun 6 June, Corpus: Thu 10 June,
1800 Easter: Sun 13 April, Ascension: Thu 22 May, Pentecost: Sun 1 June, Trinity: Sun 8 June, Corpus: Thu 12 June,
1900 Easter: Sun 15 April, Ascension: Thu 24 May, Pentecost: Sun 3 June, Trinity: Sun 10 June, Corpus: Thu 14 June,
2000 Easter: Sun 23 April, Ascension: Thu 1 June, Pentecost: Sun 11 June, Trinity: Sun 18 June, Corpus: Thu 22 June,
2100 Easter: Sun 28 March, Ascension: Thu 6 May, Pentecost: Sun 16 May, Trinity: Sun 23 May, Corpus: Thu 27 May,
2010 Easter: Sun 4 April, Ascension: Thu 13 May, Pentecost: Sun 23 May, Trinity: Sun 30 May, Corpus: Thu 3 June,
2011 Easter: Sun 24 April, Ascension: Thu 2 June, Pentecost: Sun 12 June, Trinity: Sun 19 June, Corpus: Thu 23 June,
2012 Easter: Sun 8 April, Ascension: Thu 17 May, Pentecost: Sun 27 May, Trinity: Sun 3 June, Corpus: Thu 7 June,
2013 Easter: Sun 31 March, Ascension: Thu 9 May, Pentecost: Sun 19 May, Trinity: Sun 26 May, Corpus: Thu 30 May,
2014 Easter: Sun 20 April, Ascension: Thu 29 May, Pentecost: Sun 8 June, Trinity: Sun 15 June, Corpus: Thu 19 June,
2015 Easter: Sun 5 April, Ascension: Thu 14 May, Pentecost: Sun 24 May, Trinity: Sun 31 May, Corpus: Thu 4 June,
2016 Easter: Sun 27 March, Ascension: Thu 5 May, Pentecost: Sun 15 May, Trinity: Sun 22 May, Corpus: Thu 26 May,
2017 Easter: Sun 16 April, Ascension: Thu 25 May, Pentecost: Sun 4 June, Trinity: Sun 11 June, Corpus: Thu 15 June,
2018 Easter: Sun 1 April, Ascension: Thu 10 May, Pentecost: Sun 20 May, Trinity: Sun 27 May, Corpus: Thu 31 May,
2019 Easter: Sun 21 April, Ascension: Thu 30 May, Pentecost: Sun 9 June, Trinity: Sun 16 June, Corpus: Thu 20 June,
2020 Easter: Sun 12 April, Ascension: Thu 21 May, Pentecost: Sun 31 May, Trinity: Sun 7 June, Corpus: Thu 11 June}
```



## Maxima


```maxima
/* Easter sunday. Result is a list [month, day]

See:
Jean Meeus, "Astronomical Formulae for Calculators",
4th edition, Willmann-Bell, 1988, p.31 */

easter_sunday(year):=block([a,b,c,d,e,f,g,h,i,k,l,m,n,p],
a:remainder(year,19),[b,c]:divide(year,100),[d,e]:divide(b,4),
f:quotient(b+8,25),g:quotient(b-f+1,3),h:remainder(19*a+b-d-g+15,30),
[i,k]:divide(c,4),l:remainder(32+2*e+2*i-h-k,7),m:quotient(a+11*h+22*l,451),
[n,p]:divide(h+l-7*m+114,31),[n,p+1])$
```


=={{header|MK-61/52}}==
<lang>П2	1	9	ПП	86	П3	ИП2	4	ПП	86
П4	ИП2	7	ПП	86	П5	1	9	ИП3	*
1	5	+	3	0	ПП	86	П6	2	ИП4
*	4	ИП5	*	+	6	ИП6	*	+	6
+	7	ПП	86	ИП6	+	П1	3	П4	ИП2
1	-	2	10^x	/	[x]	^	^	4	/
[x]	-	2	0	+	ИП1	+	П3	3	1
-	x>=0	76	П3	КИП4	ИП3	3	0	-	x>=0
83	П3	КИП4	ИП3	ИП4	С/П	П0	<->	П1	<->
/	[x]	ИП0	*	ИП1	-	/-/	В/О
```


Calculated, of course, the Orthodox Easter. Enter the number of the year, the result of: the day in the register Y, a month in the register X.

For the subsequent calculation of Ascension and Pentecost (Trinity Day):

<lang>П0	<->	П1	<->	-	1	3	+	П1	3
1	-	x>=0	19	П1	ИП0	1	+	П0	ИП0
1	+	П0	ИП1	ИП0	С/П	ИП1	1	0	+
П1	3	1	-	x>=0	41	П1	ИП0	1	+
П0	ИП1	ИП0	С/П
```


''Example'': Easter in 2014 is 20.04; Ascension is 29.05; Pentecost is 08.06.

To calculate the dates of holidays of western heretics (catholics, protestants, other sectarians), it is necessary to replace the coefficients ''15'' on the address <u>20</u> and ''6'' on the address <u>39</u> by: ''22'' and ''2'' for the 16th and 17th centuries; ''23'' and ''3'' for 18th; ''23'' and ''4'' for 19th; ''24'' and ''5'' for 20th and 21th; ''25'' and ''6'' for 22th century, etc.


## PARI/GP

Code handles Gregorian / Julian holidays and dates correctly. Calendar reform was 1582-10-04.


```parigp
/*
 * Normalized Julian Day Number from date (base 1899-12-30 00:00:00)
 * D = Vec [year, month, day]
 * return day number
 */
njd(D) =
{
  my (m, y);

  if (D[2] > 2, y = D[1]; m = D[2] + 1, y = D[1] - 1; m = D[2] + 13);

  (1461 * y) \ 4 + (306001 * m) \ 10000 + D[3] - 694024

/* Calendar reform ? */
    + if (100 * (100 * D[1] + D[2]) + D[3] > 15821004, 2 - y \ 100 + y \ 400)
}

/*
 * Date from Normalized Julian Day Number (base 1899-12-30 00:00:00)
 * n = Normalized Julian Day Number
 * return Vec [year, month, day]
 */
njdate(n) =
{
  my (a = n + 2415019, b, c, d, m, D, M, Y);

/* Calendar reform ? */
  if (a >= 2299161, b = (4 * a - 7468865) \ 146097; a += 1 + b - b \ 4);

  a += 1524;
  b = (20 * a - 2442) \ 7305;
  c = (1461 * b) \ 4;
  d = ((a - c) * 10000) \ 306001;
  m = d - 1 - 12 * (d > 13);

  [b - 4715 - (m > 2), m, a - c - (306001 * d) \ 10000]
}

/*
 * Date of Easter
 * Y = year
 * return Vec [year, month, day]
 */
easter(y) =
{
  my (a, b, d, m);

  if (y > 1582,           /* calendar reform ? */
/* Gregorian Easter */
    a = y % 19;
    b = y % 100;
    d = y \ 100;
    m = (19 * a + d - d \ 4 - (d - (d + 8) \ 25 + 1) \ 3 + 15) % 30;
    d = (32 + (d % 4) * 2 + (b \ 4) * 2 - m - b % 4) % 7;
    m += d - (a + 11 * m + 22 * d) \ 451 * 7 + 114;
  ,
/* Julian Easter */
    d = ((y % 19) * 19 + 15) % 30;
    m = d + ((y % 4) * 2 + (y % 7) * 4 - d + 34) % 7 + 114;
  );

  [y, m \ 31, m % 31 + 1]
}

holiday(y) =
{
  my (e = njd(easter(y)), n);

  n = njdate(e   ); printf("%4d: Easter: %02d-%02d, ", y, n[2], n[3]);
  n = njdate(e+39); printf("Ascension: %02d-%02d, ", n[2], n[3]);
  n = njdate(e+49); printf("Pentecost: %02d-%02d, ", n[2], n[3]);
  n = njdate(e+56); printf("Trinity: %02d-%02d, ", n[2], n[3]);
  n = njdate(e+60); printf("Corpus: %02d-%02d\n", n[2], n[3]);
}

print("Christian holidays, related to Easter, for years from 400 to 2100 CE:");
forstep (y = 400, 2100, 100, holiday(y));

print("\nChristian holidays, related to Easter, for years from 2010 to 2020 CE:");
for (y = 2010, 2020, holiday(y));

```


Output:

```txt

Christian holidays, related to Easter, for years from 400 to 2100 CE:
 400: Easter: 04-01, Ascension: 05-10, Pentecost: 05-20, Trinity: 05-27, Corpus: 05-31
 500: Easter: 04-02, Ascension: 05-11, Pentecost: 05-21, Trinity: 05-28, Corpus: 06-01
 600: Easter: 04-10, Ascension: 05-19, Pentecost: 05-29, Trinity: 06-05, Corpus: 06-09
 700: Easter: 04-11, Ascension: 05-20, Pentecost: 05-30, Trinity: 06-06, Corpus: 06-10
 800: Easter: 04-19, Ascension: 05-28, Pentecost: 06-07, Trinity: 06-14, Corpus: 06-18
 900: Easter: 04-20, Ascension: 05-29, Pentecost: 06-08, Trinity: 06-15, Corpus: 06-19
1000: Easter: 03-31, Ascension: 05-09, Pentecost: 05-19, Trinity: 05-26, Corpus: 05-30
1100: Easter: 04-01, Ascension: 05-10, Pentecost: 05-20, Trinity: 05-27, Corpus: 05-31
1200: Easter: 04-09, Ascension: 05-18, Pentecost: 05-28, Trinity: 06-04, Corpus: 06-08
1300: Easter: 04-10, Ascension: 05-19, Pentecost: 05-29, Trinity: 06-05, Corpus: 06-09
1400: Easter: 04-18, Ascension: 05-27, Pentecost: 06-06, Trinity: 06-13, Corpus: 06-17
1500: Easter: 04-19, Ascension: 05-28, Pentecost: 06-07, Trinity: 06-14, Corpus: 06-18
1600: Easter: 04-02, Ascension: 05-11, Pentecost: 05-21, Trinity: 05-28, Corpus: 06-01
1700: Easter: 04-11, Ascension: 05-20, Pentecost: 05-30, Trinity: 06-06, Corpus: 06-10
1800: Easter: 04-13, Ascension: 05-22, Pentecost: 06-01, Trinity: 06-08, Corpus: 06-12
1900: Easter: 04-15, Ascension: 05-24, Pentecost: 06-03, Trinity: 06-10, Corpus: 06-14
2000: Easter: 04-23, Ascension: 06-01, Pentecost: 06-11, Trinity: 06-18, Corpus: 06-22
2100: Easter: 03-28, Ascension: 05-06, Pentecost: 05-16, Trinity: 05-23, Corpus: 05-27

Christian holidays, related to Easter, for years from 2010 to 2020 CE:
2010: Easter: 04-04, Ascension: 05-13, Pentecost: 05-23, Trinity: 05-30, Corpus: 06-03
2011: Easter: 04-24, Ascension: 06-02, Pentecost: 06-12, Trinity: 06-19, Corpus: 06-23
2012: Easter: 04-08, Ascension: 05-17, Pentecost: 05-27, Trinity: 06-03, Corpus: 06-07
2013: Easter: 03-31, Ascension: 05-09, Pentecost: 05-19, Trinity: 05-26, Corpus: 05-30
2014: Easter: 04-20, Ascension: 05-29, Pentecost: 06-08, Trinity: 06-15, Corpus: 06-19
2015: Easter: 04-05, Ascension: 05-14, Pentecost: 05-24, Trinity: 05-31, Corpus: 06-04
2016: Easter: 03-27, Ascension: 05-05, Pentecost: 05-15, Trinity: 05-22, Corpus: 05-26
2017: Easter: 04-16, Ascension: 05-25, Pentecost: 06-04, Trinity: 06-11, Corpus: 06-15
2018: Easter: 04-01, Ascension: 05-10, Pentecost: 05-20, Trinity: 05-27, Corpus: 05-31
2019: Easter: 04-21, Ascension: 05-30, Pentecost: 06-09, Trinity: 06-16, Corpus: 06-20
2020: Easter: 04-12, Ascension: 05-21, Pentecost: 05-31, Trinity: 06-07, Corpus: 06-11

```



## Perl

```perl
#!/usr/bin/perl

use strict; use warnings;
use Date::Calc qw(:all);

my @abbr = qw( Not Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec );

my %c_hols = (
 	Easter=>     0,
 	Ascension=> 39,
 	Pentecost=> 49,
 	Trinity=>   56,
 	Corpus=>    60
);

sub easter {
	my $year=shift;

	my $ay=$year % 19;
	my $by=int($year / 100);
	my $cy=$year % 100;
	my $dy=int($by/4);
	my $ey=$by % 4;
	my $fy=int(($by+8)/25);
	my $gy=int(($by-$fy+1)/3);
	my $hy=($ay*19+$by-$dy-$gy+15) % 30;
	my $iy=int($cy/4);
	my $ky=$cy % 4;
	my $ly=(32+2*$ey+2*$iy-$hy-$ky) % 7;
	my $m_y=int(($ay+11*$hy+22*$ly)/451);

	my $month=int(($hy+$ly-7*$m_y+114)/31);
	my $day=(($hy+$ly-7*$m_y+114) % 31)+1;

	return ($month, $day, $year);
}

sub cholidays {
	my $year=shift;
	my ($emon, $eday)=easter($year);
	my @fields;
	printf("%4s: ", $year);

	foreach my $hol (sort { $c_hols{$a}<=>$c_hols{$b} } keys %c_hols) {
		my ($ye,$mo,$da)=Add_Delta_Days($year,$emon,$eday,$c_hols{$hol});
		my $month=$abbr[$mo];
		push @fields, sprintf("%s: %02s %s",$hol,$da,$month);
	}
	print join (", ",@fields);
	print "\n";
}


print 	"Christian holidays, related to Easter, for each centennial from ",
		"400 to 2100 CE:\n";
for (my $year=400; $year<=2100; $year+=100) {
	cholidays($year);
}

print	"Christian holidays, related to Easter, ",
		"for years from 2010 to 2020 CE:\n";


cholidays($_) for(2010..2020);

```


Output:

```txt

Christian holidays, related to Easter, for each centennial from 400 to 2100 CE:
 400: Easter: 02 Apr, Ascension: 11 May, Pentecost: 21 May, Trinity: 28 May, Corpus: 01 Jun
 500: Easter: 04 Apr, Ascension: 13 May, Pentecost: 23 May, Trinity: 30 May, Corpus: 03 Jun
 600: Easter: 13 Apr, Ascension: 22 May, Pentecost: 01 Jun, Trinity: 08 Jun, Corpus: 12 Jun
 700: Easter: 15 Apr, Ascension: 24 May, Pentecost: 03 Jun, Trinity: 10 Jun, Corpus: 14 Jun
 800: Easter: 23 Apr, Ascension: 01 Jun, Pentecost: 11 Jun, Trinity: 18 Jun, Corpus: 22 Jun
 900: Easter: 28 Mar, Ascension: 06 May, Pentecost: 16 May, Trinity: 23 May, Corpus: 27 May
1000: Easter: 30 Mar, Ascension: 08 May, Pentecost: 18 May, Trinity: 25 May, Corpus: 29 May
1100: Easter: 08 Apr, Ascension: 17 May, Pentecost: 27 May, Trinity: 03 Jun, Corpus: 07 Jun
1200: Easter: 09 Apr, Ascension: 18 May, Pentecost: 28 May, Trinity: 04 Jun, Corpus: 08 Jun
1300: Easter: 18 Apr, Ascension: 27 May, Pentecost: 06 Jun, Trinity: 13 Jun, Corpus: 17 Jun
1400: Easter: 20 Apr, Ascension: 29 May, Pentecost: 08 Jun, Trinity: 15 Jun, Corpus: 19 Jun
1500: Easter: 01 Apr, Ascension: 10 May, Pentecost: 20 May, Trinity: 27 May, Corpus: 31 May
1600: Easter: 02 Apr, Ascension: 11 May, Pentecost: 21 May, Trinity: 28 May, Corpus: 01 Jun
1700: Easter: 11 Apr, Ascension: 20 May, Pentecost: 30 May, Trinity: 06 Jun, Corpus: 10 Jun
1800: Easter: 13 Apr, Ascension: 22 May, Pentecost: 01 Jun, Trinity: 08 Jun, Corpus: 12 Jun
1900: Easter: 15 Apr, Ascension: 24 May, Pentecost: 03 Jun, Trinity: 10 Jun, Corpus: 14 Jun
2000: Easter: 23 Apr, Ascension: 01 Jun, Pentecost: 11 Jun, Trinity: 18 Jun, Corpus: 22 Jun
2100: Easter: 28 Mar, Ascension: 06 May, Pentecost: 16 May, Trinity: 23 May, Corpus: 27 May
Christian holidays, related to Easter, for years from 2010 to 2020 CE:
2010: Easter: 04 Apr, Ascension: 13 May, Pentecost: 23 May, Trinity: 30 May, Corpus: 03 Jun
2011: Easter: 24 Apr, Ascension: 02 Jun, Pentecost: 12 Jun, Trinity: 19 Jun, Corpus: 23 Jun
2012: Easter: 08 Apr, Ascension: 17 May, Pentecost: 27 May, Trinity: 03 Jun, Corpus: 07 Jun
2013: Easter: 31 Mar, Ascension: 09 May, Pentecost: 19 May, Trinity: 26 May, Corpus: 30 May
2014: Easter: 20 Apr, Ascension: 29 May, Pentecost: 08 Jun, Trinity: 15 Jun, Corpus: 19 Jun
2015: Easter: 05 Apr, Ascension: 14 May, Pentecost: 24 May, Trinity: 31 May, Corpus: 04 Jun
2016: Easter: 27 Mar, Ascension: 05 May, Pentecost: 15 May, Trinity: 22 May, Corpus: 26 May
2017: Easter: 16 Apr, Ascension: 25 May, Pentecost: 04 Jun, Trinity: 11 Jun, Corpus: 15 Jun
2018: Easter: 01 Apr, Ascension: 10 May, Pentecost: 20 May, Trinity: 27 May, Corpus: 31 May
2019: Easter: 21 Apr, Ascension: 30 May, Pentecost: 09 Jun, Trinity: 16 Jun, Corpus: 20 Jun
2020: Easter: 12 Apr, Ascension: 21 May, Pentecost: 31 May, Trinity: 07 Jun, Corpus: 11 Jun

```



## Perl 6

```perl6>my @abbr = < Nil Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec
;

my @holidays =
    Easter    => 0,
    Ascension => 39,
    Pentecost => 49,
    Trinity   => 56,
    Corpus    => 60;

sub easter($year) {
    my \ay = $year % 19;
    my \by = $year div 100;
    my \cy = $year % 100;
    my \dy = by div 4;
    my \ey = by % 4;
    my \fy = (by + 8) div 25;
    my \gy = (by - fy + 1) div 3;
    my \hy = (ay * 19 + by - dy - gy + 15) % 30;
    my \iy = cy div 4;
    my \ky = cy % 4;
    my \ly = (32 + 2 * ey + 2 * iy - hy - ky) % 7;
    my \md = hy + ly - 7 * ((ay + 11 * hy + 22 * ly) div 451) + 114;
    my \month = md div 31;
    my \day = md % 31 + 1;

    return month, day;
}

sub cholidays($year) {
    my ($emon, $eday) = easter($year);
    printf "%4s: ", $year;
    say join ', ', gather for @holidays -> $holiday {
	my $d = Date.new($year,$emon,$eday) + $holiday.value;
	take "{$holiday.key}: $d.day-of-month.fmt('%02s') @abbr[$d.month]";
    }
}

for flat (400,500 ... 2000), (2010 ... 2020), 2100 -> $year {
    cholidays($year);
}
```

```txt
 400: Easter: 02 Apr, Ascension: 11 May, Pentecost: 21 May, Trinity: 28 May, Corpus: 01 Jun
 500: Easter: 04 Apr, Ascension: 13 May, Pentecost: 23 May, Trinity: 30 May, Corpus: 03 Jun
 600: Easter: 13 Apr, Ascension: 22 May, Pentecost: 01 Jun, Trinity: 08 Jun, Corpus: 12 Jun
 700: Easter: 15 Apr, Ascension: 24 May, Pentecost: 03 Jun, Trinity: 10 Jun, Corpus: 14 Jun
 800: Easter: 23 Apr, Ascension: 01 Jun, Pentecost: 11 Jun, Trinity: 18 Jun, Corpus: 22 Jun
 900: Easter: 28 Mar, Ascension: 06 May, Pentecost: 16 May, Trinity: 23 May, Corpus: 27 May
1000: Easter: 30 Mar, Ascension: 08 May, Pentecost: 18 May, Trinity: 25 May, Corpus: 29 May
1100: Easter: 08 Apr, Ascension: 17 May, Pentecost: 27 May, Trinity: 03 Jun, Corpus: 07 Jun
1200: Easter: 09 Apr, Ascension: 18 May, Pentecost: 28 May, Trinity: 04 Jun, Corpus: 08 Jun
1300: Easter: 18 Apr, Ascension: 27 May, Pentecost: 06 Jun, Trinity: 13 Jun, Corpus: 17 Jun
1400: Easter: 20 Apr, Ascension: 29 May, Pentecost: 08 Jun, Trinity: 15 Jun, Corpus: 19 Jun
1500: Easter: 01 Apr, Ascension: 10 May, Pentecost: 20 May, Trinity: 27 May, Corpus: 31 May
1600: Easter: 02 Apr, Ascension: 11 May, Pentecost: 21 May, Trinity: 28 May, Corpus: 01 Jun
1700: Easter: 11 Apr, Ascension: 20 May, Pentecost: 30 May, Trinity: 06 Jun, Corpus: 10 Jun
1800: Easter: 13 Apr, Ascension: 22 May, Pentecost: 01 Jun, Trinity: 08 Jun, Corpus: 12 Jun
1900: Easter: 15 Apr, Ascension: 24 May, Pentecost: 03 Jun, Trinity: 10 Jun, Corpus: 14 Jun
2000: Easter: 23 Apr, Ascension: 01 Jun, Pentecost: 11 Jun, Trinity: 18 Jun, Corpus: 22 Jun
2010: Easter: 04 Apr, Ascension: 13 May, Pentecost: 23 May, Trinity: 30 May, Corpus: 03 Jun
2011: Easter: 24 Apr, Ascension: 02 Jun, Pentecost: 12 Jun, Trinity: 19 Jun, Corpus: 23 Jun
2012: Easter: 08 Apr, Ascension: 17 May, Pentecost: 27 May, Trinity: 03 Jun, Corpus: 07 Jun
2013: Easter: 31 Mar, Ascension: 09 May, Pentecost: 19 May, Trinity: 26 May, Corpus: 30 May
2014: Easter: 20 Apr, Ascension: 29 May, Pentecost: 08 Jun, Trinity: 15 Jun, Corpus: 19 Jun
2015: Easter: 05 Apr, Ascension: 14 May, Pentecost: 24 May, Trinity: 31 May, Corpus: 04 Jun
2016: Easter: 27 Mar, Ascension: 05 May, Pentecost: 15 May, Trinity: 22 May, Corpus: 26 May
2017: Easter: 16 Apr, Ascension: 25 May, Pentecost: 04 Jun, Trinity: 11 Jun, Corpus: 15 Jun
2018: Easter: 01 Apr, Ascension: 10 May, Pentecost: 20 May, Trinity: 27 May, Corpus: 31 May
2019: Easter: 21 Apr, Ascension: 30 May, Pentecost: 09 Jun, Trinity: 16 Jun, Corpus: 20 Jun
2020: Easter: 12 Apr, Ascension: 21 May, Pentecost: 31 May, Trinity: 07 Jun, Corpus: 11 Jun
2100: Easter: 28 Mar, Ascension: 06 May, Pentecost: 16 May, Trinity: 23 May, Corpus: 27 May
```



## Phix

The Phix timedate routines make no attempt to support pre-1752 dates, but the algorithm seems to work.

```Phix
--
-- demo\rosetta\Easter.exw
--
### =================

--
function easter(integer year)
-- from https://en.wikipedia.org/wiki/Computus#Anonymous_Gregorian_algorithm
    integer a = mod(year,19),
            b = floor(year/100),
            c = mod(year,100),
            d = floor(b/4),
            e = mod(b,4),
            f = floor((b+8)/25),
            g = floor((b-f+1)/3),
            h = mod(19*a+b-d-g+15,30),
            i = floor(c/4),
            k = mod(c,4),
            l = mod(32+2*e+2*i-h-k,7),
            m = floor((a+11*h+22*l)/451),
            n = h+l-7*m+114,
            month = floor(n/31),
            day = mod(n,31)+1
    return {year,month,day,0,0,0,0,0,0}
end function

constant dates = {{"Easter   ",0},
                  {"Ascension",39},
                  {"Pentecost",49},
                  {"Trinity  ",56},
                  {"Corpus   ",60}}

constant fmt = join(repeat(" %12s",length(dates))),
         fmt0 = "   "&fmt&"\n",
         fmtN = "%4d"&fmt&"\n"

include builtins\timedate.e

set_timedate_formats({"Ddd ddth Mmm"})

procedure show(integer year)
    if year=0 then
        printf(1,fmt0, columnize(dates,1)[1])
    else
        timedate e = easter(year)
        sequence args = {year}
        for i=1 to length(dates) do
            string d = format_timedate(adjust_timedate(e,timedelta(days:=dates[i][2])))
            args = append(args,d)
        end for
        printf(1,fmtN, args)
    end if
end procedure
show(0)
for year=400 to 2000 by 100 do
    show(year)
end for
show(0)
for year=2010 to 2020 do
    show(year)
end for
```

```txt

       Easter        Ascension     Pentecost     Trinity       Corpus
 400 Sun 02nd Apr  Thu 11th May  Sun 21st May  Sun 28th May  Thu 01st Jun
 500 Sun 04th Apr  Thu 13th May  Sun 23rd May  Sun 30th May  Thu 03rd Jun
 600 Sun 13th Apr  Thu 22nd May  Sun 01st Jun  Sun 08th Jun  Thu 12th Jun
 700 Sun 15th Apr  Thu 24th May  Sun 03rd Jun  Sun 10th Jun  Thu 14th Jun
 800 Sun 23rd Apr  Thu 01st Jun  Sun 11th Jun  Sun 18th Jun  Thu 22nd Jun
 900 Sun 28th Mar  Thu 06th May  Sun 16th May  Sun 23rd May  Thu 27th May
1000 Sun 30th Mar  Thu 08th May  Sun 18th May  Sun 25th May  Thu 29th May
1100 Sun 08th Apr  Thu 17th May  Sun 27th May  Sun 03rd Jun  Thu 07th Jun
1200 Sun 09th Apr  Thu 18th May  Sun 28th May  Sun 04th Jun  Thu 08th Jun
1300 Sun 18th Apr  Thu 27th May  Sun 06th Jun  Sun 13th Jun  Thu 17th Jun
1400 Sun 20th Apr  Thu 29th May  Sun 08th Jun  Sun 15th Jun  Thu 19th Jun
1500 Sun 01st Apr  Thu 10th May  Sun 20th May  Sun 27th May  Thu 31st May
1600 Sun 02nd Apr  Thu 11th May  Sun 21st May  Sun 28th May  Thu 01st Jun
1700 Sun 11th Apr  Thu 20th May  Sun 30th May  Sun 06th Jun  Thu 10th Jun
1800 Sun 13th Apr  Thu 22nd May  Sun 01st Jun  Sun 08th Jun  Thu 12th Jun
1900 Sun 15th Apr  Thu 24th May  Sun 03rd Jun  Sun 10th Jun  Thu 14th Jun
2000 Sun 23rd Apr  Thu 01st Jun  Sun 11th Jun  Sun 18th Jun  Thu 22nd Jun
       Easter        Ascension     Pentecost     Trinity       Corpus
2010 Sun 04th Apr  Thu 13th May  Sun 23rd May  Sun 30th May  Thu 03rd Jun
2011 Sun 24th Apr  Thu 02nd Jun  Sun 12th Jun  Sun 19th Jun  Thu 23rd Jun
2012 Sun 08th Apr  Thu 17th May  Sun 27th May  Sun 03rd Jun  Thu 07th Jun
2013 Sun 31st Mar  Thu 09th May  Sun 19th May  Sun 26th May  Thu 30th May
2014 Sun 20th Apr  Thu 29th May  Sun 08th Jun  Sun 15th Jun  Thu 19th Jun
2015 Sun 05th Apr  Thu 14th May  Sun 24th May  Sun 31st May  Thu 04th Jun
2016 Sun 27th Mar  Thu 05th May  Sun 15th May  Sun 22nd May  Thu 26th May
2017 Sun 16th Apr  Thu 25th May  Sun 04th Jun  Sun 11th Jun  Thu 15th Jun
2018 Sun 01st Apr  Thu 10th May  Sun 20th May  Sun 27th May  Thu 31st May
2019 Sun 21st Apr  Thu 30th May  Sun 09th Jun  Sun 16th Jun  Thu 20th Jun
2020 Sun 12th Apr  Thu 21st May  Sun 31st May  Sun 07th Jun  Thu 11th Jun

```



## PicoLisp


```PicoLisp
(load "@lib/cal.l")  # For 'easter' function

(de dayMon (Dat)
   (let D (date Dat)
      (list (day Dat *Day) " " (align 2 (caddr D)) " " (get *Mon (cadr D))) ) )

(for Y (append (range 400 2100 100) (range 2010 2020))
   (let E (easter Y)
      (prinl
         (align 4 Y)
         " Easter: " (dayMon E)
         ", Ascension: " (dayMon (+ E 39))
         ", Pentecost: " (dayMon (+ E 49))
         ", Trinity: " (dayMon (+ E 56))
         ", Corpus: " (dayMon (+ E 60)) ) ) )
```

Output:

```txt
 400 Easter: Sun  2 Apr, Ascension: Thu 11 May, Pentecost: Sun 21 May, Trinity: Sun 28 May, Corpus: Thu  1 Jun
 500 Easter: Sun  4 Apr, Ascension: Thu 13 May, Pentecost: Sun 23 May, Trinity: Sun 30 May, Corpus: Thu  3 Jun
 600 Easter: Sun 13 Apr, Ascension: Thu 22 May, Pentecost: Sun  1 Jun, Trinity: Sun  8 Jun, Corpus: Thu 12 Jun
 700 Easter: Sun 15 Apr, Ascension: Thu 24 May, Pentecost: Sun  3 Jun, Trinity: Sun 10 Jun, Corpus: Thu 14 Jun
 800 Easter: Sun 23 Apr, Ascension: Thu  1 Jun, Pentecost: Sun 11 Jun, Trinity: Sun 18 Jun, Corpus: Thu 22 Jun
 900 Easter: Sun 28 Mar, Ascension: Thu  6 May, Pentecost: Sun 16 May, Trinity: Sun 23 May, Corpus: Thu 27 May
1000 Easter: Sun 30 Mar, Ascension: Thu  8 May, Pentecost: Sun 18 May, Trinity: Sun 25 May, Corpus: Thu 29 May
1100 Easter: Sun  8 Apr, Ascension: Thu 17 May, Pentecost: Sun 27 May, Trinity: Sun  3 Jun, Corpus: Thu  7 Jun
1200 Easter: Sun  9 Apr, Ascension: Thu 18 May, Pentecost: Sun 28 May, Trinity: Sun  4 Jun, Corpus: Thu  8 Jun
1300 Easter: Sun 18 Apr, Ascension: Thu 27 May, Pentecost: Sun  6 Jun, Trinity: Sun 13 Jun, Corpus: Thu 17 Jun
1400 Easter: Sun 20 Apr, Ascension: Thu 29 May, Pentecost: Sun  8 Jun, Trinity: Sun 15 Jun, Corpus: Thu 19 Jun
1500 Easter: Sun  1 Apr, Ascension: Thu 10 May, Pentecost: Sun 20 May, Trinity: Sun 27 May, Corpus: Thu 31 May
1600 Easter: Sun  2 Apr, Ascension: Thu 11 May, Pentecost: Sun 21 May, Trinity: Sun 28 May, Corpus: Thu  1 Jun
1700 Easter: Sun 11 Apr, Ascension: Thu 20 May, Pentecost: Sun 30 May, Trinity: Sun  6 Jun, Corpus: Thu 10 Jun
1800 Easter: Sun 13 Apr, Ascension: Thu 22 May, Pentecost: Sun  1 Jun, Trinity: Sun  8 Jun, Corpus: Thu 12 Jun
1900 Easter: Sun 15 Apr, Ascension: Thu 24 May, Pentecost: Sun  3 Jun, Trinity: Sun 10 Jun, Corpus: Thu 14 Jun
2000 Easter: Sun 23 Apr, Ascension: Thu  1 Jun, Pentecost: Sun 11 Jun, Trinity: Sun 18 Jun, Corpus: Thu 22 Jun
2100 Easter: Sun 28 Mar, Ascension: Thu  6 May, Pentecost: Sun 16 May, Trinity: Sun 23 May, Corpus: Thu 27 May
2010 Easter: Sun  4 Apr, Ascension: Thu 13 May, Pentecost: Sun 23 May, Trinity: Sun 30 May, Corpus: Thu  3 Jun
2011 Easter: Sun 24 Apr, Ascension: Thu  2 Jun, Pentecost: Sun 12 Jun, Trinity: Sun 19 Jun, Corpus: Thu 23 Jun
2012 Easter: Sun  8 Apr, Ascension: Thu 17 May, Pentecost: Sun 27 May, Trinity: Sun  3 Jun, Corpus: Thu  7 Jun
2013 Easter: Sun 31 Mar, Ascension: Thu  9 May, Pentecost: Sun 19 May, Trinity: Sun 26 May, Corpus: Thu 30 May
2014 Easter: Sun 20 Apr, Ascension: Thu 29 May, Pentecost: Sun  8 Jun, Trinity: Sun 15 Jun, Corpus: Thu 19 Jun
2015 Easter: Sun  5 Apr, Ascension: Thu 14 May, Pentecost: Sun 24 May, Trinity: Sun 31 May, Corpus: Thu  4 Jun
2016 Easter: Sun 27 Mar, Ascension: Thu  5 May, Pentecost: Sun 15 May, Trinity: Sun 22 May, Corpus: Thu 26 May
2017 Easter: Sun 16 Apr, Ascension: Thu 25 May, Pentecost: Sun  4 Jun, Trinity: Sun 11 Jun, Corpus: Thu 15 Jun
2018 Easter: Sun  1 Apr, Ascension: Thu 10 May, Pentecost: Sun 20 May, Trinity: Sun 27 May, Corpus: Thu 31 May
2019 Easter: Sun 21 Apr, Ascension: Thu 30 May, Pentecost: Sun  9 Jun, Trinity: Sun 16 Jun, Corpus: Thu 20 Jun
2020 Easter: Sun 12 Apr, Ascension: Thu 21 May, Pentecost: Sun 31 May, Trinity: Sun  7 Jun, Corpus: Thu 11 Jun
```



## PL/I

<lang>(subscriptrange, size, fofl):
Easter: procedure options (main);
   declare months(12) character (9) varying static initial (
           'January', 'February', 'March',     'April',   'May',      'June',
           'July',    'August',   'September', 'October', 'November', 'December');
   declare (year, month, day) fixed binary;

   do year = 2000 to 2020;
      call Easter_Sunday (year, month, day);
      put skip edit ('In ', year, ' Easter occurs on ', day, ' of ', months(month))
         (a, f(4), a, f(2), a, a);
   end;

/* Given the year, this procedure computes the month and day when Easter Sunday falls. */
Easter_Sunday: procedure (year,month,day);
/*
  See:
  Jean Meeus, "Astronomical Formulae for Calculators",
  4th edition, Willmann-Bell, 1988, p.31
*/

   declare (year nonassignable, month, day) fixed binary;
   declare (a, b, c, d, e, f, g, h, i, j, k, l, m, n) fixed binary;

   a = mod(year, 19);
   b = year/100;
   c = mod(year, 100);
   d = b/4;
   e = mod(b,4);
   f = (b+8)/25;
   g = (b-f+1)/3;
   h = mod(19*a+b-d-g+15, 30);
   i = c/4;
   k = mod(c,4);
   l = mod(32+2*e+2*i-h-k, 7);
   m = (a+11*h+22*l)/451;
   n = h+l-7*m+114;
   month = n/31;
   day = mod(n, 31)+1;
end Easter_Sunday;

end Easter;
```

Results:

```txt

In 2000 Easter occurs on 23 of April
In 2001 Easter occurs on 15 of April
In 2002 Easter occurs on 31 of March
In 2003 Easter occurs on 20 of April
In 2004 Easter occurs on 11 of April
In 2005 Easter occurs on 27 of March
In 2006 Easter occurs on 16 of April
In 2007 Easter occurs on  8 of April
In 2008 Easter occurs on 23 of March
In 2009 Easter occurs on 12 of April
In 2010 Easter occurs on  4 of April
In 2011 Easter occurs on 24 of April
In 2012 Easter occurs on  8 of April
In 2013 Easter occurs on 31 of March
In 2014 Easter occurs on 20 of April
In 2015 Easter occurs on  5 of April
In 2016 Easter occurs on 27 of March
In 2017 Easter occurs on 16 of April
In 2018 Easter occurs on  1 of April
In 2019 Easter occurs on 21 of April
In 2020 Easter occurs on 12 of April

```



## PowerShell


```PowerShell

function Get-Easter
{
    [CmdletBinding()]
    [OutputType([PSCustomObject])]
    Param
    (
        [Parameter(Mandatory=$true,
                   ValueFromPipeline=$true,
                   ValueFromPipelineByPropertyName=$true,
                   Position=0)]
        [int]
        $Year
    )

    Begin
    {
        $holidayOffset = [ordered]@{
            Easter    = 0
            Ascension = 39
            Pentecost = 49
            Trinity   = 56
            Corpus    = 60
        }

        function Get-DateOfEaster ([int]$Year)
        {
            [int]$a = $Year % 19
            [int]$b = [Math]::Truncate($Year / 100)
            [int]$c = $Year % 100
            [int]$d = [Math]::Truncate($b / 4)
            [int]$e = $b % 4
            [int]$f = [Math]::Truncate(($b + 8) / 25)
            [int]$g = [Math]::Truncate(($b - $f + 1) / 3)
            [int]$h = ((19 * $a) + $b - $d - $g + 15) % 30
            [int]$i = [Math]::Truncate($c / 4)
            [int]$j = $c % 4
            [int]$k = (32 + 2 * ($e + $i) - $h - $j) % 7
            [int]$l = [Math]::Truncate(($a + (11 * $h) + (22 * $k)) / 451)
            [int]$m = [Math]::Truncate(($h + $k - (7 * $l) + 114) / 31)
            [int]$d = (($h + $k - (7 * $l) + 114) % 31) + 1

            Get-Date -Year $Year -Month $m -Day $d
        }

        function Get-Holiday ([int]$Year)
        {
            $easter = Get-DateOfEaster -Year $Year

            $holidays = foreach ($key in $holidayOffset.Keys)
            {
                $easter.AddDays($holidayOffset.$key)
            }

            [PSCustomObject]@{
                Year      = $Year
                Easter    = $holidays[0].ToString("ddd dd MMM")
                Ascension = $holidays[1].ToString("ddd dd MMM")
                Pentecost = $holidays[2].ToString("ddd dd MMM")
                Trinity   = $holidays[3].ToString("ddd dd MMM")
                Corpus    = $holidays[4].ToString("ddd dd MMM")
            }
        }
    }
    Process
    {
        foreach ($y in $Year)
        {
            Get-Holiday -Year $y
        }
    }
}

```


```PowerShell

$years = for ($i = 400; $i -le 2100; $i+=100) {$i}
$years0400to2100 = $years | Get-Easter
$years2010to2020 = 2010..2020 | Get-Easter

Write-Host "Christian holidays, related to Easter, for each centennial from 400 to 2100 AD:"
$years0400to2100 | Format-Table
Write-Host "Christian holidays, related to Easter, for years from 2010 to 2020 AD:"
$years2010to2020 | Format-Table

```

```txt

Christian holidays, related to Easter, for each centennial from 400 to 2100 AD:

Year Easter           Ascension        Pentecost        Trinity          Corpus
---- ------           ---------        ---------        -------          ------
 400 Sun 02 Apr       Thu 11 May       Sun 21 May       Sun 28 May       Thu 01 Jun
 500 Sun 04 Apr       Thu 13 May       Sun 23 May       Sun 30 May       Thu 03 Jun
 600 Sun 13 Apr       Thu 22 May       Sun 01 Jun       Sun 08 Jun       Thu 12 Jun
 700 Sun 15 Apr       Thu 24 May       Sun 03 Jun       Sun 10 Jun       Thu 14 Jun
 800 Sun 23 Apr       Thu 01 Jun       Sun 11 Jun       Sun 18 Jun       Thu 22 Jun
 900 Sun 28 Mar       Thu 06 May       Sun 16 May       Sun 23 May       Thu 27 May
1000 Sun 30 Mar       Thu 08 May       Sun 18 May       Sun 25 May       Thu 29 May
1100 Sun 08 Apr       Thu 17 May       Sun 27 May       Sun 03 Jun       Thu 07 Jun
1200 Sun 09 Apr       Thu 18 May       Sun 28 May       Sun 04 Jun       Thu 08 Jun
1300 Sun 18 Apr       Thu 27 May       Sun 06 Jun       Sun 13 Jun       Thu 17 Jun
1400 Sun 20 Apr       Thu 29 May       Sun 08 Jun       Sun 15 Jun       Thu 19 Jun
1500 Sun 01 Apr       Thu 10 May       Sun 20 May       Sun 27 May       Thu 31 May
1600 Sun 02 Apr       Thu 11 May       Sun 21 May       Sun 28 May       Thu 01 Jun
1700 Sun 11 Apr       Thu 20 May       Sun 30 May       Sun 06 Jun       Thu 10 Jun
1800 Sun 13 Apr       Thu 22 May       Sun 01 Jun       Sun 08 Jun       Thu 12 Jun
1900 Sun 15 Apr       Thu 24 May       Sun 03 Jun       Sun 10 Jun       Thu 14 Jun
2000 Sun 23 Apr       Thu 01 Jun       Sun 11 Jun       Sun 18 Jun       Thu 22 Jun
2100 Sun 28 Mar       Thu 06 May       Sun 16 May       Sun 23 May       Thu 27 May


Christian holidays, related to Easter, for years from 2010 to 2020 AD:

Year Easter           Ascension        Pentecost        Trinity          Corpus
---- ------           ---------        ---------        -------          ------
2010 Sun 04 Apr       Thu 13 May       Sun 23 May       Sun 30 May       Thu 03 Jun
2011 Sun 24 Apr       Thu 02 Jun       Sun 12 Jun       Sun 19 Jun       Thu 23 Jun
2012 Sun 08 Apr       Thu 17 May       Sun 27 May       Sun 03 Jun       Thu 07 Jun
2013 Sun 31 Mar       Thu 09 May       Sun 19 May       Sun 26 May       Thu 30 May
2014 Sun 20 Apr       Thu 29 May       Sun 08 Jun       Sun 15 Jun       Thu 19 Jun
2015 Sun 05 Apr       Thu 14 May       Sun 24 May       Sun 31 May       Thu 04 Jun
2016 Sun 27 Mar       Thu 05 May       Sun 15 May       Sun 22 May       Thu 26 May
2017 Sun 16 Apr       Thu 25 May       Sun 04 Jun       Sun 11 Jun       Thu 15 Jun
2018 Sun 01 Apr       Thu 10 May       Sun 20 May       Sun 27 May       Thu 31 May
2019 Sun 21 Apr       Thu 30 May       Sun 09 Jun       Sun 16 Jun       Thu 20 Jun
2020 Sun 12 Apr       Thu 21 May       Sun 31 May       Sun 07 Jun       Thu 11 Jun

```



## Python

Unfortunately, at present Python doesn't support date formatting for any dates before 1900. So while it is trivial to get the date for easter, it takes a bit more work to format the date.


```python
from dateutil.easter import *
import datetime, calendar

class Holiday(object):
    def __init__(self, date, offset=0):
        self.holiday = date + datetime.timedelta(days=offset)

    def __str__(self):
        dayofweek = calendar.day_name[self.holiday.weekday()][0:3]
        month = calendar.month_name[self.holiday.month][0:3]
        return '{0} {1:2d} {2}'.format(dayofweek, self.holiday.day, month)

def get_holiday_values(year):
    holidays = {'year': year}
    easterDate = easter(year)
    holidays['easter'] = Holiday(easterDate)
    holidays['ascension'] = Holiday(easterDate, 39)
    holidays['pentecost'] = Holiday(easterDate, 49)
    holidays['trinity'] = Holiday(easterDate, 56)
    holidays['corpus'] = Holiday(easterDate, 60)
    return holidays

def print_holidays(holidays):
    print '{year:4d} Easter: {easter}, Ascension: {ascension}, Pentecost: {pentecost}, Trinity: {trinity}, Corpus: {corpus}'.format(**holidays)

if __name__ == "__main__":
    print "Christian holidays, related to Easter, for each centennial from 400 to 2100 CE:"
    for year in range(400, 2200, 100):
        print_holidays(get_holiday_values(year))

    print ''
    print "Christian holidays, related to Easter, for years from 2010 to 2020 CE:"
    for year in range(2010, 2021):
        print_holidays(get_holiday_values(year))


```


Output:

```txt

Christian holidays, related to Easter, for each centennial from 400 to 2100 CE:
 400 Easter: Sun  2 Apr, Ascension: Thu 11 May, Pentecost: Sun 21 May, Trinity: Sun 28 May, Corpus: Thu  1 Jun
 500 Easter: Sun  4 Apr, Ascension: Thu 13 May, Pentecost: Sun 23 May, Trinity: Sun 30 May, Corpus: Thu  3 Jun
 600 Easter: Sun 13 Apr, Ascension: Thu 22 May, Pentecost: Sun  1 Jun, Trinity: Sun  8 Jun, Corpus: Thu 12 Jun
 700 Easter: Sun 15 Apr, Ascension: Thu 24 May, Pentecost: Sun  3 Jun, Trinity: Sun 10 Jun, Corpus: Thu 14 Jun
 800 Easter: Sun 23 Apr, Ascension: Thu  1 Jun, Pentecost: Sun 11 Jun, Trinity: Sun 18 Jun, Corpus: Thu 22 Jun
 900 Easter: Sun 28 Mar, Ascension: Thu  6 May, Pentecost: Sun 16 May, Trinity: Sun 23 May, Corpus: Thu 27 May
1000 Easter: Sun 30 Mar, Ascension: Thu  8 May, Pentecost: Sun 18 May, Trinity: Sun 25 May, Corpus: Thu 29 May
1100 Easter: Sun  8 Apr, Ascension: Thu 17 May, Pentecost: Sun 27 May, Trinity: Sun  3 Jun, Corpus: Thu  7 Jun
1200 Easter: Sun  9 Apr, Ascension: Thu 18 May, Pentecost: Sun 28 May, Trinity: Sun  4 Jun, Corpus: Thu  8 Jun
1300 Easter: Sun 18 Apr, Ascension: Thu 27 May, Pentecost: Sun  6 Jun, Trinity: Sun 13 Jun, Corpus: Thu 17 Jun
1400 Easter: Sun 20 Apr, Ascension: Thu 29 May, Pentecost: Sun  8 Jun, Trinity: Sun 15 Jun, Corpus: Thu 19 Jun
1500 Easter: Sun  1 Apr, Ascension: Thu 10 May, Pentecost: Sun 20 May, Trinity: Sun 27 May, Corpus: Thu 31 May
1600 Easter: Sun  2 Apr, Ascension: Thu 11 May, Pentecost: Sun 21 May, Trinity: Sun 28 May, Corpus: Thu  1 Jun
1700 Easter: Sun 11 Apr, Ascension: Thu 20 May, Pentecost: Sun 30 May, Trinity: Sun  6 Jun, Corpus: Thu 10 Jun
1800 Easter: Sun 13 Apr, Ascension: Thu 22 May, Pentecost: Sun  1 Jun, Trinity: Sun  8 Jun, Corpus: Thu 12 Jun
1900 Easter: Sun 15 Apr, Ascension: Thu 24 May, Pentecost: Sun  3 Jun, Trinity: Sun 10 Jun, Corpus: Thu 14 Jun
2000 Easter: Sun 23 Apr, Ascension: Thu  1 Jun, Pentecost: Sun 11 Jun, Trinity: Sun 18 Jun, Corpus: Thu 22 Jun
2100 Easter: Sun 28 Mar, Ascension: Thu  6 May, Pentecost: Sun 16 May, Trinity: Sun 23 May, Corpus: Thu 27 May

Christian holidays, related to Easter, for years from 2010 to 2020 CE:
2010 Easter: Sun  4 Apr, Ascension: Thu 13 May, Pentecost: Sun 23 May, Trinity: Sun 30 May, Corpus: Thu  3 Jun
2011 Easter: Sun 24 Apr, Ascension: Thu  2 Jun, Pentecost: Sun 12 Jun, Trinity: Sun 19 Jun, Corpus: Thu 23 Jun
2012 Easter: Sun  8 Apr, Ascension: Thu 17 May, Pentecost: Sun 27 May, Trinity: Sun  3 Jun, Corpus: Thu  7 Jun
2013 Easter: Sun 31 Mar, Ascension: Thu  9 May, Pentecost: Sun 19 May, Trinity: Sun 26 May, Corpus: Thu 30 May
2014 Easter: Sun 20 Apr, Ascension: Thu 29 May, Pentecost: Sun  8 Jun, Trinity: Sun 15 Jun, Corpus: Thu 19 Jun
2015 Easter: Sun  5 Apr, Ascension: Thu 14 May, Pentecost: Sun 24 May, Trinity: Sun 31 May, Corpus: Thu  4 Jun
2016 Easter: Sun 27 Mar, Ascension: Thu  5 May, Pentecost: Sun 15 May, Trinity: Sun 22 May, Corpus: Thu 26 May
2017 Easter: Sun 16 Apr, Ascension: Thu 25 May, Pentecost: Sun  4 Jun, Trinity: Sun 11 Jun, Corpus: Thu 15 Jun
2018 Easter: Sun  1 Apr, Ascension: Thu 10 May, Pentecost: Sun 20 May, Trinity: Sun 27 May, Corpus: Thu 31 May
2019 Easter: Sun 21 Apr, Ascension: Thu 30 May, Pentecost: Sun  9 Jun, Trinity: Sun 16 Jun, Corpus: Thu 20 Jun
2020 Easter: Sun 12 Apr, Ascension: Thu 21 May, Pentecost: Sun 31 May, Trinity: Sun  7 Jun, Corpus: Thu 11 Jun

```



## Racket

The Scheme version works unchanged for Racket.


## REXX


```rexx
/* REXX **********************************************************************************
* Test frame for computing Christian holidays, related to Easter
* 16.04.2013 Walter Pachl
*****************************************************************************************/
oid='ee.txt'; 'erase' oid

month.3='mar'; days.3=31
month.4='apr'; days.4=30
month.5='may'; days.5=31
month.6='jun'; days.6=30
Call o 'Christian holidays, related to Easter, for each centennial',
                                                'from 400 to 2100 CE:'
Do y=400 To 2100 By 100
  Call line y
  End
Call o ' '
Call o 'Christian holidays, related to Easter, for years',
                                                'from 2010 to 2020 CE:'
Do y=2010 To 2020
  Call line y
  End
Exit

line: Parse Arg y
Parse Value easter(y) With y m d
Parse Value add(d,m,39) With ad am
Parse Value add(ad,am,10) With pd pm
Parse Value add(pd,pm,7) With td tm
Parse Value add(td,tm,4) With cd cm
ol=right(y,4) 'Easter:' right(d,2) month.m
ol=ol'  Ascension:' right(ad,2) month.am ' Pentecost:' right(pd,2) month.pm
ol=ol'  Trinity:' right(td,2) month.tm'  Corpus:' right(cd,2) month.cm
Call o ol
Return

o: Return lineout(oid,arg(1))

add: Procedure Expose days.
Parse Arg d,m,dd
res=d+dd
Do While res>days.m
  res=res-days.m
  m=m+1
  End
Return res m

easter: Procedure
/***********************************************************
* translated from FORTRAN (mod -> //; / -> %)
* Input  year
* Output year month day of Easter Sunday
* 16.04.2013 Walter Pachl
c See:
c Jean Meeus, "Astronomical Formulae for Calculators",
c 4th edition, Willmann-Bell, 1988, p.31
*********************************************************/
Parse Arg year
a=year//19
b=year%100
c=year//100
d=b%4
e=b//4
f=(b+8)%25
g=(b-f+1)%3
h=(19*a+b-d-g+15)//30
i=c%4
k=c//4
l=(32+2*e+2*i-h-k)//7
m=(a+11*h+22*l)%451
n=h+l-7*m+114
month=n%31
day=n//31+1
Return year month day

```

Output:

```txt

Christian holidays, related to Easter, for each centennial from 400 to 2100 CE:
 400 Easter:  2 apr  Ascension: 11 may  Pentecost: 21 may  Trinity: 28 may  Corpus:  1 jun
 500 Easter:  4 apr  Ascension: 13 may  Pentecost: 23 may  Trinity: 30 may  Corpus:  3 jun
 600 Easter: 13 apr  Ascension: 22 may  Pentecost:  1 jun  Trinity:  8 jun  Corpus: 12 jun
 700 Easter: 15 apr  Ascension: 24 may  Pentecost:  3 jun  Trinity: 10 jun  Corpus: 14 jun
 800 Easter: 23 apr  Ascension:  1 jun  Pentecost: 11 jun  Trinity: 18 jun  Corpus: 22 jun
 900 Easter: 28 mar  Ascension:  6 may  Pentecost: 16 may  Trinity: 23 may  Corpus: 27 may
1000 Easter: 30 mar  Ascension:  8 may  Pentecost: 18 may  Trinity: 25 may  Corpus: 29 may
1100 Easter:  8 apr  Ascension: 17 may  Pentecost: 27 may  Trinity:  3 jun  Corpus:  7 jun
1200 Easter:  9 apr  Ascension: 18 may  Pentecost: 28 may  Trinity:  4 jun  Corpus:  8 jun
1300 Easter: 18 apr  Ascension: 27 may  Pentecost:  6 jun  Trinity: 13 jun  Corpus: 17 jun
1400 Easter: 20 apr  Ascension: 29 may  Pentecost:  8 jun  Trinity: 15 jun  Corpus: 19 jun
1500 Easter:  1 apr  Ascension: 10 may  Pentecost: 20 may  Trinity: 27 may  Corpus: 31 may
1600 Easter:  2 apr  Ascension: 11 may  Pentecost: 21 may  Trinity: 28 may  Corpus:  1 jun
1700 Easter: 11 apr  Ascension: 20 may  Pentecost: 30 may  Trinity:  6 jun  Corpus: 10 jun
1800 Easter: 13 apr  Ascension: 22 may  Pentecost:  1 jun  Trinity:  8 jun  Corpus: 12 jun
1900 Easter: 15 apr  Ascension: 24 may  Pentecost:  3 jun  Trinity: 10 jun  Corpus: 14 jun
2000 Easter: 23 apr  Ascension:  1 jun  Pentecost: 11 jun  Trinity: 18 jun  Corpus: 22 jun
2100 Easter: 28 mar  Ascension:  6 may  Pentecost: 16 may  Trinity: 23 may  Corpus: 27 may

Christian holidays, related to Easter, for years from 2010 to 2020 CE:
2010 Easter:  4 apr  Ascension: 13 may  Pentecost: 23 may  Trinity: 30 may  Corpus:  3 jun
2011 Easter: 24 apr  Ascension:  2 jun  Pentecost: 12 jun  Trinity: 19 jun  Corpus: 23 jun
2012 Easter:  8 apr  Ascension: 17 may  Pentecost: 27 may  Trinity:  3 jun  Corpus:  7 jun
2013 Easter: 31 mar  Ascension:  9 may  Pentecost: 19 may  Trinity: 26 may  Corpus: 30 may
2014 Easter: 20 apr  Ascension: 29 may  Pentecost:  8 jun  Trinity: 15 jun  Corpus: 19 jun
2015 Easter:  5 apr  Ascension: 14 may  Pentecost: 24 may  Trinity: 31 may  Corpus:  4 jun
2016 Easter: 27 mar  Ascension:  5 may  Pentecost: 15 may  Trinity: 22 may  Corpus: 26 may
2017 Easter: 16 apr  Ascension: 25 may  Pentecost:  4 jun  Trinity: 11 jun  Corpus: 15 jun
2018 Easter:  1 apr  Ascension: 10 may  Pentecost: 20 may  Trinity: 27 may  Corpus: 31 may
2019 Easter: 21 apr  Ascension: 30 may  Pentecost:  9 jun  Trinity: 16 jun  Corpus: 20 jun
2020 Easter: 12 apr  Ascension: 21 may  Pentecost: 31 may  Trinity:  7 jun  Corpus: 11 jun
```



## Ruby

```ruby
require 'date'

def easter_date(year)
  # Anonymous Gregorian algorithm
  # http://en.wikipedia.org/wiki/Computus#Algorithms

  a    = year % 19
  b, c = year.divmod(100)
  d, e = b.divmod(4)
  f    = (b + 8) / 25
  g    = (b - f + 1) / 3
  h    = (19*a + b - d - g + 15) % 30
  i, k = c.divmod(4)
  l    = (32 + 2*e + 2*i - h - k) % 7
  m    = (a + 11*h + 22*l) / 451
  numerator = h + l - 7*m + 114
  month = numerator / 31
  day = (numerator % 31) + 1
  Date.new(year, month, day)
end

OFFSETS = [
  [:easter,     0],
  [:ascension, 39],
  [:pentecost, 49],
  [:trinity,   56],
  [:corpus,    60],
]

def emit_dates(year)
  e = easter_date year
  dates = OFFSETS.collect {|item, offset| (e + offset).strftime("  %e %b")}
  puts "%4s: %s" % [year, dates.join(',  ')]
end

puts "year:" + OFFSETS.collect{|item, offset| "%9s" % item}.join(', ')
400.step(2100, 100).each {|year| emit_dates year}
puts
(2010 .. 2020).each {|year| emit_dates year}
```


outputs
<pre style="height: 30ex; overflow: scroll">
year:   easter, ascension, pentecost,   trinity,    corpus
 400:    2 Apr,    11 May,    21 May,    28 May,     1 Jun
 500:    4 Apr,    13 May,    23 May,    30 May,     3 Jun
 600:   13 Apr,    22 May,     1 Jun,     8 Jun,    12 Jun
 700:   15 Apr,    24 May,     3 Jun,    10 Jun,    14 Jun
 800:   23 Apr,     1 Jun,    11 Jun,    18 Jun,    22 Jun
 900:   28 Mar,     6 May,    16 May,    23 May,    27 May
1000:   30 Mar,     8 May,    18 May,    25 May,    29 May
1100:    8 Apr,    17 May,    27 May,     3 Jun,     7 Jun
1200:    9 Apr,    18 May,    28 May,     4 Jun,     8 Jun
1300:   18 Apr,    27 May,     6 Jun,    13 Jun,    17 Jun
1400:   20 Apr,    29 May,     8 Jun,    15 Jun,    19 Jun
1500:    1 Apr,    10 May,    20 May,    27 May,    31 May
1600:    2 Apr,    11 May,    21 May,    28 May,     1 Jun
1700:   11 Apr,    20 May,    30 May,     6 Jun,    10 Jun
1800:   13 Apr,    22 May,     1 Jun,     8 Jun,    12 Jun
1900:   15 Apr,    24 May,     3 Jun,    10 Jun,    14 Jun
2000:   23 Apr,     1 Jun,    11 Jun,    18 Jun,    22 Jun
2100:   28 Mar,     6 May,    16 May,    23 May,    27 May

2010:    4 Apr,    13 May,    23 May,    30 May,     3 Jun
2011:   24 Apr,     2 Jun,    12 Jun,    19 Jun,    23 Jun
2012:    8 Apr,    17 May,    27 May,     3 Jun,     7 Jun
2013:   31 Mar,     9 May,    19 May,    26 May,    30 May
2014:   20 Apr,    29 May,     8 Jun,    15 Jun,    19 Jun
2015:    5 Apr,    14 May,    24 May,    31 May,     4 Jun
2016:   27 Mar,     5 May,    15 May,    22 May,    26 May
2017:   16 Apr,    25 May,     4 Jun,    11 Jun,    15 Jun
2018:    1 Apr,    10 May,    20 May,    27 May,    31 May
2019:   21 Apr,    30 May,     9 Jun,    16 Jun,    20 Jun
2020:   12 Apr,    21 May,    31 May,     7 Jun,    11 Jun

```



## Scala

```scala
import java.util._
import scala.swing._

def easter(year: Int) = {
  // Start at March 21.
  val cal = new GregorianCalendar(year, 2, 21);

  /*
   * Calculate e = day of Easter, following the 1886 paper,
   * Kalender-Formeln (Calendar Formulae) by Chr. Zeller.
   *   http://www.merlyn.demon.co.uk/zel-1886.htm
   *
   * With Scala, p % 7 gives the wrong result when p < 0. To give the
   * correct result, this code uses "+ 6 * j" where one might have used
   * "- j". This works because because 6 == -1 (mod 7).
   */
  var b = 0
  var d = 0
  if (cal.getTime().before(cal.getGregorianChange())) {
    // Julian calendar
    b = (19 * (year % 19) + 15) % 30
    d = (b + year + year / 4) % 7
  } else {
    // Gregorian calendar
    val j = year / 100
    val a = year % 19
    b = (19 * a + 15 + j - (8 * j + 13) / 25 - j / 4) % 30
    d = (b + year + year / 4 + 6 * j + j / 4 + 2) % 7
    if (d == 0 && (b == 29 || (b == 28 && a > 10))) d = 7
  }
  val e = b + 7 - d  // This counts days after 21 March.

  val df = new java.text.SimpleDateFormat("EEE dd MMM");
  def advance(days: Int) = {
    cal.add(Calendar.DAY_OF_MONTH, days)
    df.format(cal.getTime())
  }

  val ary = new Array[Any](6)
  ary(0) = year
  ary(1) = advance(e)   // Easter
  ary(2) = advance(39)  // Ascension Thursday
  ary(3) = advance(10)  // Pentecost
  ary(4) = advance(7)   // Trinity Sunday
  ary(5) = advance(4)   // Corpus Christi
  ary
}

val columns = Array("AD", "Easter", "Ascension Thursday", "Pentecost",
  "Trinity Sunday", "Corpus Christi")
val rows = (400.to(2000, 100) ++ 2010.to(2020) ++ Array(2100))
  .map(easter(_)).toArray

new MainFrame {
  title = "Holidays related to Easter"
  contents = new ScrollPane {
    contents = new Table(rows, columns)
  }
  size = new java.awt.Dimension(600, 400)
  visible = true
}
```



## Scheme


```scheme
; Easter sunday. Result is a list '(month day)
;
; See:
; Jean Meeus, "Astronomical Formulae for Calculators",
; 4th edition, Willmann-Bell, 1988, p.31

(define (easter year)
        (let* ((a (remainder year 19))
               (b (quotient year 100))
               (c (remainder year 100))
               (d (quotient b 4))
               (e (remainder b 4))
               (f (quotient (+ b 8) 25))
               (g (quotient (+ 1 (- b f)) 3))
               (h (remainder (+ (* 19 a) (- b d g) 15) 30))
               (i (quotient c 4))
               (k (remainder c 4))
               (l (remainder (+ e e i i (- 32 h k)) 7))
               (m (quotient (+ a (* 11 h) (* 22 l)) 451))
               (n (+ h l (- 114 (* 7 m)))))
              (list (quotient n 31) (+ 1 (remainder n 31)))))
```



## Seed7


```seed7
$ include "seed7_05.s7i";
  include "time.s7i";
  include "duration.s7i";

const func time: easterDate (in integer: year) is func
  result
    var time: result is time.value;
  local
    var integer: H1 is 0;
    var integer: H2 is 0;
    var integer: M is 0;
    var integer: N is 0;
    var integer: a is 0;
    var integer: b is 0;
    var integer: c is 0;
    var integer: d is 0;
    var integer: e is 0;
    var integer: f is 0;
  begin
    H1 := year mdiv 100;
    H2 := year mdiv 400;
     M := 15 + H1 - H2 - (8 * H1 + 13) mdiv 25;
     N := 4 + H1 - H2;

    # Gauss formula:
     a := year mod 19;
     b := year mod 4;
     c := year mod 7;
     d := (19 * a + M) mod 30;
     e := (2 * b + 4 * c + 6 * d + N) mod 7;
     f := 22 + d + e;

    if f = 57 then
      f := 50;
    end if;
    if d = 28 and e = 6 and a > 10 then
      f := 49;
    end if;

    result.year := year;
    if f <= 31 then
      result.month := 3;
    else
      result.month := 4;
      f -:= 31;
    end if;
    result.day := f;
  end func;

const array string: weekday is [] ("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun");
const array string: month is [] ("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec");
const array string: holiday is [] ("Easter", "Ascension", "Pentecost", "Trinity", "Corpus");
const array integer: delta is [] (0, 39, 49, 56, 60);

const func string: usDate (in time: aTime) is
  return weekday[dayOfWeek(aTime)] <& aTime.day lpad 3 <& " " <& month[aTime.month];

const func string: holiday (in integer: index, in time: easter) is
  return holiday[index] <& ": " <& usDate(easter + delta[index] . DAYS);

const proc: writeHolidays (in integer: year) is func
  local
	var time: easter is time.value;
  begin
    easter := easterDate(year);
    writeln(year lpad 4 <& " " <&
        holiday(1, easter) <& ", " <&
        holiday(2, easter) <& ", " <&
        holiday(3, easter) <& ", " <&
        holiday(4, easter) <& ", " <&
        holiday(5, easter));
  end func;

const proc: main is func
  local
    var integer: year is 0;
  begin
    writeln("Christian holidays, related to Easter, for each centennial from 400 to 2100 CE:");
    for year range 400 to 2100 step 100 do
	  writeHolidays(year);
    end for;
    writeln;
	writeln("Christian holidays, related to Easter, for years from 2010 to 2020 CE:");
    for year range 2010 to 2020 do
	  writeHolidays(year);
    end for;
  end func;
```


Original source of the function easterDate, which uses the Gauss formula: [http://seed7.sourceforge.net/algorith/date.htm#easterDate]

Output:

```txt

Christian holidays, related to Easter, for each centennial from 400 to 2100 CE:
 400 Easter: Sun  2 Apr, Ascension: Thu 11 May, Pentecost: Sun 21 May, Trinity: Sun 28 May, Corpus: Thu  1 Jun
 500 Easter: Sun  4 Apr, Ascension: Thu 13 May, Pentecost: Sun 23 May, Trinity: Sun 30 May, Corpus: Thu  3 Jun
 600 Easter: Sun 13 Apr, Ascension: Thu 22 May, Pentecost: Sun  1 Jun, Trinity: Sun  8 Jun, Corpus: Thu 12 Jun
 700 Easter: Sun 15 Apr, Ascension: Thu 24 May, Pentecost: Sun  3 Jun, Trinity: Sun 10 Jun, Corpus: Thu 14 Jun
 800 Easter: Sun 23 Apr, Ascension: Thu  1 Jun, Pentecost: Sun 11 Jun, Trinity: Sun 18 Jun, Corpus: Thu 22 Jun
 900 Easter: Sun 28 Mar, Ascension: Thu  6 May, Pentecost: Sun 16 May, Trinity: Sun 23 May, Corpus: Thu 27 May
1000 Easter: Sun 30 Mar, Ascension: Thu  8 May, Pentecost: Sun 18 May, Trinity: Sun 25 May, Corpus: Thu 29 May
1100 Easter: Sun  8 Apr, Ascension: Thu 17 May, Pentecost: Sun 27 May, Trinity: Sun  3 Jun, Corpus: Thu  7 Jun
1200 Easter: Sun  9 Apr, Ascension: Thu 18 May, Pentecost: Sun 28 May, Trinity: Sun  4 Jun, Corpus: Thu  8 Jun
1300 Easter: Sun 18 Apr, Ascension: Thu 27 May, Pentecost: Sun  6 Jun, Trinity: Sun 13 Jun, Corpus: Thu 17 Jun
1400 Easter: Sun 20 Apr, Ascension: Thu 29 May, Pentecost: Sun  8 Jun, Trinity: Sun 15 Jun, Corpus: Thu 19 Jun
1500 Easter: Sun  1 Apr, Ascension: Thu 10 May, Pentecost: Sun 20 May, Trinity: Sun 27 May, Corpus: Thu 31 May
1600 Easter: Sun  2 Apr, Ascension: Thu 11 May, Pentecost: Sun 21 May, Trinity: Sun 28 May, Corpus: Thu  1 Jun
1700 Easter: Sun 11 Apr, Ascension: Thu 20 May, Pentecost: Sun 30 May, Trinity: Sun  6 Jun, Corpus: Thu 10 Jun
1800 Easter: Sun 13 Apr, Ascension: Thu 22 May, Pentecost: Sun  1 Jun, Trinity: Sun  8 Jun, Corpus: Thu 12 Jun
1900 Easter: Sun 15 Apr, Ascension: Thu 24 May, Pentecost: Sun  3 Jun, Trinity: Sun 10 Jun, Corpus: Thu 14 Jun
2000 Easter: Sun 23 Apr, Ascension: Thu  1 Jun, Pentecost: Sun 11 Jun, Trinity: Sun 18 Jun, Corpus: Thu 22 Jun
2100 Easter: Sun 28 Mar, Ascension: Thu  6 May, Pentecost: Sun 16 May, Trinity: Sun 23 May, Corpus: Thu 27 May

Christian holidays, related to Easter, for years from 2010 to 2020 CE:
2010 Easter: Sun  4 Apr, Ascension: Thu 13 May, Pentecost: Sun 23 May, Trinity: Sun 30 May, Corpus: Thu  3 Jun
2011 Easter: Sun 24 Apr, Ascension: Thu  2 Jun, Pentecost: Sun 12 Jun, Trinity: Sun 19 Jun, Corpus: Thu 23 Jun
2012 Easter: Sun  8 Apr, Ascension: Thu 17 May, Pentecost: Sun 27 May, Trinity: Sun  3 Jun, Corpus: Thu  7 Jun
2013 Easter: Sun 31 Mar, Ascension: Thu  9 May, Pentecost: Sun 19 May, Trinity: Sun 26 May, Corpus: Thu 30 May
2014 Easter: Sun 20 Apr, Ascension: Thu 29 May, Pentecost: Sun  8 Jun, Trinity: Sun 15 Jun, Corpus: Thu 19 Jun
2015 Easter: Sun  5 Apr, Ascension: Thu 14 May, Pentecost: Sun 24 May, Trinity: Sun 31 May, Corpus: Thu  4 Jun
2016 Easter: Sun 27 Mar, Ascension: Thu  5 May, Pentecost: Sun 15 May, Trinity: Sun 22 May, Corpus: Thu 26 May
2017 Easter: Sun 16 Apr, Ascension: Thu 25 May, Pentecost: Sun  4 Jun, Trinity: Sun 11 Jun, Corpus: Thu 15 Jun
2018 Easter: Sun  1 Apr, Ascension: Thu 10 May, Pentecost: Sun 20 May, Trinity: Sun 27 May, Corpus: Thu 31 May
2019 Easter: Sun 21 Apr, Ascension: Thu 30 May, Pentecost: Sun  9 Jun, Trinity: Sun 16 Jun, Corpus: Thu 20 Jun
2020 Easter: Sun 12 Apr, Ascension: Thu 21 May, Pentecost: Sun 31 May, Trinity: Sun  7 Jun, Corpus: Thu 11 Jun

```



## Sidef

```ruby
require('Date::Calc')

var abbr = < Nil Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec >

var holidays = [
    [Easter    => 0],
    [Ascension => 39],
    [Pentecost => 49],
    [Trinity   => 56],
    [Corpus    => 60],
]

func easter(year) {
    var ay = (year % 19)
    var by = (year // 100)
    var cy = (year % 100)
    var dy = (by // 4)
    var ey = (by % 4)
    var fy = ((by + 8) // 25)
    var gy = ((by - fy + 1) // 3)
    var hy = ((19*ay + by - dy - gy + 15) % 30)
    var iy = (cy // 4)
    var ky = (cy % 4)
    var ly = ((32 + 2*ey + 2*iy - hy - ky) % 7)
    var md = (hy + ly - 7*((ay + 11*hy + 22*ly) // 451) + 114)
    var month = (md // 31)
    var day = (md % 31 + 1)
    return(month, day)
}
 
func cholidays(year) {
    var (emon, eday) = easter(year)
    printf("%4s: ", year)
    say gather {
        holidays.each { |holiday|
            var (_, mo, da) = %S<Date::Calc>.Add_Delta_Days(year, emon, eday, holiday[1])
            take("#{holiday[0]}: #{'%02d' % da} #{abbr[mo]}")
        }
    }.join(', ')
}
 
for year in (400..2100 `by` 100, 2010..2020) {
    cholidays(year)
}
```



## Tcl


```tcl
package require Tcl 8.5; # Advanced date handling engine

# Easter computation code from http://www.assa.org.au/edm.html
proc EasterDate year {
    set FirstDig [expr {$year / 100}]
    set Remain19 [expr {$year % 19}]

    # calculate Paschal Full Moon date
    set temp [expr {($FirstDig - 15)/2 + 202 - 11*$Remain19}]
    if {$FirstDig in {21 24 25 27 28 29 30 31 32 34 35 38}} {
	incr temp -1
    } elseif {$FirstDig in {33 36 37 39 40}} {
	incr temp -2
    }
    set temp [expr {$temp % 30}]

    set tA [expr {$temp + 21}]
    if {$temp == 29} {incr tA -1}
    if {$temp == 28 && $Remain19 > 10} {incr tA -1}

    # find the next Sunday
    set tB [expr {($tA - 19) % 7}]

    set tC [expr {(40 - $FirstDig) % 4}]
    if {$tC == 3} {incr tC}
    if {$tC > 1} {incr tC}

    set temp [expr {$year % 100}]
    set tD [expr {($temp + $temp/4) % 7}]

    set tE [expr {((20 - $tB - $tC - $tD) % 7) + 1}]
    set d [expr {$tA + $tE}]

    # return the date
    if {$d > 31} {
	return [format "%02d April %04d" [expr {$d - 31}] $year]
    } else {
	return [format "%02d March %04d" $d $year]
    }
}

# Use the Easter calculator to work out the data for the feasts
proc DateInfo year {
    set fields [format %4d: $year]\t
    set easter [clock scan [EasterDate $year] -format "%d %B %Y"]
    foreach {name delta} {
	Easter     0
	Ascension 39
	Pentecost 49
	Trinity   56
	Corpus    60
    } {
	set when [clock add $easter $delta days]
	append fields [clock format $when -format "${name}: %d %b,  "]
    }
    return [string trimright $fields " ,"]
}

# Print the required info
puts "Christian holidays, related to Easter, for each centennial from 400 to 2100 CE:"
for {set year 400} {$year <= 2100} {incr year 100} {
    puts [DateInfo $year]
}
puts ""
puts "Christian holidays, related to Easter, for years from 2010 to 2020 CE:"
for {set year 2010} {$year <= 2020} {incr year} {
    puts [DateInfo $year]
}
```

Output:

```txt

Christian holidays, related to Easter, for each centennial from 400 to 2100 CE:
 400:	Easter: 02 Apr,  Ascension: 11 May,  Pentecost: 21 May,  Trinity: 28 May,  Corpus: 01 Jun
 500:	Easter: 04 Apr,  Ascension: 13 May,  Pentecost: 23 May,  Trinity: 30 May,  Corpus: 03 Jun
 600:	Easter: 13 Apr,  Ascension: 22 May,  Pentecost: 01 Jun,  Trinity: 08 Jun,  Corpus: 12 Jun
 700:	Easter: 15 Apr,  Ascension: 24 May,  Pentecost: 03 Jun,  Trinity: 10 Jun,  Corpus: 14 Jun
 800:	Easter: 23 Apr,  Ascension: 01 Jun,  Pentecost: 11 Jun,  Trinity: 18 Jun,  Corpus: 22 Jun
 900:	Easter: 28 Mar,  Ascension: 06 May,  Pentecost: 16 May,  Trinity: 23 May,  Corpus: 27 May
1000:	Easter: 30 Mar,  Ascension: 08 May,  Pentecost: 18 May,  Trinity: 25 May,  Corpus: 29 May
1100:	Easter: 08 Apr,  Ascension: 17 May,  Pentecost: 27 May,  Trinity: 03 Jun,  Corpus: 07 Jun
1200:	Easter: 09 Apr,  Ascension: 18 May,  Pentecost: 28 May,  Trinity: 04 Jun,  Corpus: 08 Jun
1300:	Easter: 18 Apr,  Ascension: 27 May,  Pentecost: 06 Jun,  Trinity: 13 Jun,  Corpus: 17 Jun
1400:	Easter: 20 Apr,  Ascension: 29 May,  Pentecost: 08 Jun,  Trinity: 15 Jun,  Corpus: 19 Jun
1500:	Easter: 01 Apr,  Ascension: 10 May,  Pentecost: 20 May,  Trinity: 27 May,  Corpus: 31 May
1600:	Easter: 02 Apr,  Ascension: 11 May,  Pentecost: 21 May,  Trinity: 28 May,  Corpus: 01 Jun
1700:	Easter: 11 Apr,  Ascension: 20 May,  Pentecost: 30 May,  Trinity: 06 Jun,  Corpus: 10 Jun
1800:	Easter: 13 Apr,  Ascension: 22 May,  Pentecost: 01 Jun,  Trinity: 08 Jun,  Corpus: 12 Jun
1900:	Easter: 15 Apr,  Ascension: 24 May,  Pentecost: 03 Jun,  Trinity: 10 Jun,  Corpus: 14 Jun
2000:	Easter: 23 Apr,  Ascension: 01 Jun,  Pentecost: 11 Jun,  Trinity: 18 Jun,  Corpus: 22 Jun
2100:	Easter: 28 Mar,  Ascension: 06 May,  Pentecost: 16 May,  Trinity: 23 May,  Corpus: 27 May

Christian holidays, related to Easter, for years from 2010 to 2020 CE:
2010:	Easter: 04 Apr,  Ascension: 13 May,  Pentecost: 23 May,  Trinity: 30 May,  Corpus: 03 Jun
2011:	Easter: 24 Apr,  Ascension: 02 Jun,  Pentecost: 12 Jun,  Trinity: 19 Jun,  Corpus: 23 Jun
2012:	Easter: 08 Apr,  Ascension: 17 May,  Pentecost: 27 May,  Trinity: 03 Jun,  Corpus: 07 Jun
2013:	Easter: 31 Mar,  Ascension: 09 May,  Pentecost: 19 May,  Trinity: 26 May,  Corpus: 30 May
2014:	Easter: 20 Apr,  Ascension: 29 May,  Pentecost: 08 Jun,  Trinity: 15 Jun,  Corpus: 19 Jun
2015:	Easter: 05 Apr,  Ascension: 14 May,  Pentecost: 24 May,  Trinity: 31 May,  Corpus: 04 Jun
2016:	Easter: 27 Mar,  Ascension: 05 May,  Pentecost: 15 May,  Trinity: 22 May,  Corpus: 26 May
2017:	Easter: 16 Apr,  Ascension: 25 May,  Pentecost: 04 Jun,  Trinity: 11 Jun,  Corpus: 15 Jun
2018:	Easter: 01 Apr,  Ascension: 10 May,  Pentecost: 20 May,  Trinity: 27 May,  Corpus: 31 May
2019:	Easter: 21 Apr,  Ascension: 30 May,  Pentecost: 09 Jun,  Trinity: 16 Jun,  Corpus: 20 Jun
2020:	Easter: 12 Apr,  Ascension: 21 May,  Pentecost: 31 May,  Trinity: 07 Jun,  Corpus: 11 Jun

```



## VBA

```vb
Public dates As Variant
Private Function easter(year_ As Integer) As Date
'-- from https://en.wikipedia.org/wiki/Computus#Anonymous_Gregorian_algorithm
    Dim a As Integer, b As Integer, c As Integer, d As Integer, e As Integer
    Dim f As Integer, g As Integer, h As Integer, i As Integer, k As Integer
    Dim l As Integer, m As Integer, n As Integer
    a = year_ Mod 19
    b = year_ \ 100
    c = year_ Mod 100
    d = b \ 4
    e = b Mod 4
    f = (b + 8) \ 25
    g = (b - f + 1) \ 3
    h = (19 * a + b - d - g + 15) Mod 30
    i = c \ 4
    k = c Mod 4
    l = (32 + 2 * e + 2 * i - h - k) Mod 7
    m = (a + 11 * h + 22 * l) \ 451
    n = h + l - 7 * m + 114
    month_ = n \ 31
    day_ = n Mod 31 + 1
    easter = DateSerial(year_, month_, day_)
End Function

Private Sub show(year_ As Integer)
    If year_ = 0 Then
        Debug.Print , "Easter", "Ascension", "Pentecost", "Trinity", "Corpus"
    Else
        Dim e As Date
        e = easter(year_)
        Debug.Print Format(year_, "@@@@"),
        For i = 1 To UBound(dates)
            Debug.Print Format(e + dates(i, 2), "ddd dd mmm"),
        Next i
        Debug.Print
    End If
End Sub

Public Sub main()
    Dim year_ As Integer
    dates = [{"Easter   ",0; "Ascension",39; "Pentecost",49; "Trinity  ",56; "Corpus   ",60}]
    show 0
    For year_ = 400 To 2000 Step 100
        show year_
    Next year_
    Debug.Print
    show 0
    For year_ = 2010 To 2020
        show year_
    Next year_
End Sub
```
```txt
              Easter        Ascension     Pentecost     Trinity       Corpus
 400          zo 02 apr     do 11 mei     zo 21 mei     zo 28 mei     do 01 jun
 500          zo 04 apr     do 13 mei     zo 23 mei     zo 30 mei     do 03 jun
 600          zo 13 apr     do 22 mei     zo 01 jun     zo 08 jun     do 12 jun
 700          zo 15 apr     do 24 mei     zo 03 jun     zo 10 jun     do 14 jun
 800          zo 23 apr     do 01 jun     zo 11 jun     zo 18 jun     do 22 jun
 900          zo 28 mrt     do 06 mei     zo 16 mei     zo 23 mei     do 27 mei
1000          zo 30 mrt     do 08 mei     zo 18 mei     zo 25 mei     do 29 mei
1100          zo 08 apr     do 17 mei     zo 27 mei     zo 03 jun     do 07 jun
1200          zo 09 apr     do 18 mei     zo 28 mei     zo 04 jun     do 08 jun
1300          zo 18 apr     do 27 mei     zo 06 jun     zo 13 jun     do 17 jun
1400          zo 20 apr     do 29 mei     zo 08 jun     zo 15 jun     do 19 jun
1500          zo 01 apr     do 10 mei     zo 20 mei     zo 27 mei     do 31 mei
1600          zo 02 apr     do 11 mei     zo 21 mei     zo 28 mei     do 01 jun
1700          zo 11 apr     do 20 mei     zo 30 mei     zo 06 jun     do 10 jun
1800          zo 13 apr     do 22 mei     zo 01 jun     zo 08 jun     do 12 jun
1900          zo 15 apr     do 24 mei     zo 03 jun     zo 10 jun     do 14 jun
2000          zo 23 apr     do 01 jun     zo 11 jun     zo 18 jun     do 22 jun

              Easter        Ascension     Pentecost     Trinity       Corpus
2010          zo 04 apr     do 13 mei     zo 23 mei     zo 30 mei     do 03 jun
2011          zo 24 apr     do 02 jun     zo 12 jun     zo 19 jun     do 23 jun
2012          zo 08 apr     do 17 mei     zo 27 mei     zo 03 jun     do 07 jun
2013          zo 31 mrt     do 09 mei     zo 19 mei     zo 26 mei     do 30 mei
2014          zo 20 apr     do 29 mei     zo 08 jun     zo 15 jun     do 19 jun
2015          zo 05 apr     do 14 mei     zo 24 mei     zo 31 mei     do 04 jun
2016          zo 27 mrt     do 05 mei     zo 15 mei     zo 22 mei     do 26 mei
2017          zo 16 apr     do 25 mei     zo 04 jun     zo 11 jun     do 15 jun
2018          zo 01 apr     do 10 mei     zo 20 mei     zo 27 mei     do 31 mei
2019          zo 21 apr     do 30 mei     zo 09 jun     zo 16 jun     do 20 jun
2020          zo 12 apr     do 21 mei     zo 31 mei     zo 07 jun     do 11 jun
```


## TUSCRIPT

For dates before October 15, 1582 the Julian Calendar is taken as basis.

```tuscript

$$ MODE TUSCRIPT
SET years=*
LOOP period1=400,2100,100
SET years=APPEND(years,period1)
ENDLOOP
LOOP period2=2010,2020
SET years=APPEND(years,period2)
ENDLOOP
SET years=DIGIT SORT (years)
LOOP year=years
SET dayofweek=DATE (EASTER,day,month,year,nreaster)
PRINT "   Easter: ",year," ",month," ",day
 LOOP nr="39'49'56'60",feast="Ascension'Pentecost'  Trinity'   Corpus"
 SET number=nreaster+nr
 SET dayofweek= DATE (DATE,day,month,year,number)
 PRINT feast,": ",year," ",month," ",day
 ENDLOOP
ENDLOOP

```

Output:
<pre style='height:30ex;overflow:scroll'>
   Easter: 400 4 1
Ascension: 400 5 10
Pentecost: 400 5 20
  Trinity: 400 5 27
   Corpus: 400 5 31
   Easter: 500 4 2
Ascension: 500 5 11
Pentecost: 500 5 21
  Trinity: 500 5 28
   Corpus: 500 6 1
   Easter: 600 4 10
Ascension: 600 5 19
Pentecost: 600 5 29
  Trinity: 600 6 5
   Corpus: 600 6 9
   Easter: 700 4 11
Ascension: 700 5 20
Pentecost: 700 5 30
  Trinity: 700 6 6
   Corpus: 700 6 10
   Easter: 800 4 19
Ascension: 800 5 28
Pentecost: 800 6 7
  Trinity: 800 6 14
   Corpus: 800 6 18
   Easter: 900 4 20
Ascension: 900 5 29
Pentecost: 900 6 8
  Trinity: 900 6 15
   Corpus: 900 6 19
   Easter: 1000 3 31
Ascension: 1000 5 9
Pentecost: 1000 5 19
  Trinity: 1000 5 26
   Corpus: 1000 5 30
   Easter: 1100 4 1
Ascension: 1100 5 10
Pentecost: 1100 5 20
  Trinity: 1100 5 27
   Corpus: 1100 5 31
   Easter: 1200 4 9
Ascension: 1200 5 18
Pentecost: 1200 5 28
  Trinity: 1200 6 4
   Corpus: 1200 6 8
   Easter: 1300 4 10
Ascension: 1300 5 19
Pentecost: 1300 5 29
  Trinity: 1300 6 5
   Corpus: 1300 6 9
   Easter: 1400 4 18
Ascension: 1400 5 27
Pentecost: 1400 6 6
  Trinity: 1400 6 13
   Corpus: 1400 6 17
   Easter: 1500 4 19
Ascension: 1500 5 28
Pentecost: 1500 6 7
  Trinity: 1500 6 14
   Corpus: 1500 6 18
   Easter: 1600 4 2
Ascension: 1600 5 11
Pentecost: 1600 5 21
  Trinity: 1600 5 28
   Corpus: 1600 6 1
   Easter: 1700 4 11
Ascension: 1700 5 20
Pentecost: 1700 5 30
  Trinity: 1700 6 6
   Corpus: 1700 6 10
   Easter: 1800 4 13
Ascension: 1800 5 22
Pentecost: 1800 6 1
  Trinity: 1800 6 8
   Corpus: 1800 6 12
   Easter: 1900 4 15
Ascension: 1900 5 24
Pentecost: 1900 6 3
  Trinity: 1900 6 10
   Corpus: 1900 6 14
   Easter: 2000 4 23
Ascension: 2000 6 1
Pentecost: 2000 6 11
  Trinity: 2000 6 18
   Corpus: 2000 6 22
   Easter: 2010 4 4
Ascension: 2010 5 13
Pentecost: 2010 5 23
  Trinity: 2010 5 30
   Corpus: 2010 6 3
   Easter: 2011 4 24
Ascension: 2011 6 2
Pentecost: 2011 6 12
  Trinity: 2011 6 19
   Corpus: 2011 6 23
   Easter: 2012 4 8
Ascension: 2012 5 17
Pentecost: 2012 5 27
  Trinity: 2012 6 3
   Corpus: 2012 6 7
   Easter: 2013 3 31
Ascension: 2013 5 9
Pentecost: 2013 5 19
  Trinity: 2013 5 26
   Corpus: 2013 5 30
   Easter: 2014 4 20
Ascension: 2014 5 29
Pentecost: 2014 6 8
  Trinity: 2014 6 15
   Corpus: 2014 6 19
   Easter: 2015 4 5
Ascension: 2015 5 14
Pentecost: 2015 5 24
  Trinity: 2015 5 31
   Corpus: 2015 6 4
   Easter: 2016 3 27
Ascension: 2016 5 5
Pentecost: 2016 5 15
  Trinity: 2016 5 22
   Corpus: 2016 5 26
   Easter: 2017 4 16
Ascension: 2017 5 25
Pentecost: 2017 6 4
  Trinity: 2017 6 11
   Corpus: 2017 6 15
   Easter: 2018 4 1
Ascension: 2018 5 10
Pentecost: 2018 5 20
  Trinity: 2018 5 27
   Corpus: 2018 5 31
   Easter: 2019 4 21
Ascension: 2019 5 30
Pentecost: 2019 6 9
  Trinity: 2019 6 16
   Corpus: 2019 6 20
   Easter: 2020 4 12
Ascension: 2020 5 21
Pentecost: 2020 5 31
  Trinity: 2020 6 7
   Corpus: 2020 6 11
   Easter: 2100 3 28
Ascension: 2100 5 6
Pentecost: 2100 5 16
  Trinity: 2100 5 23
   Corpus: 2100 5 27

```


