+++
title = "Calendar"
description = ""
date = 2019-08-11T14:57:41Z
aliases = []
[extra]
id = 9818
[taxonomies]
categories = []
tags = []
+++

{{task|Date and time}}
Create a routine that will generate a text calendar for any year.
Test the calendar by generating a calendar for the year 1969, on a device of the time.
Choose one of the following devices:

* A line printer with a width of 132 characters.
* An [[wp:IBM_3270#Displays|IBM 3278 model 4 terminal]] (80×43 display with accented characters). Target formatting the months of the year to fit nicely across the 80 character width screen. Restrict number of lines in test output to 43.


(Ideally, the program will generate well-formatted calendars for any page width from 20 characters up.)

Kudos (κῦδος) for routines that also transition from [https://en.wikipedia.org/wiki/Julian_calendar Julian] to [https://en.wikipedia.org/wiki/Gregorian_calendar Gregorian calendar].

This task is inspired by [http://www.ee.ryerson.ca/~elf/hack/realmen.html Real Programmers Don't Use PASCAL] by Ed Post, Datamation, volume 29 number 7, July 1983.
 THE REAL PROGRAMMER'S NATURAL HABITAT
 "Taped to the wall is a line-printer Snoopy calender for the year 1969."
For further Kudos see task [[Calendar - for "real" programmers|CALENDAR]], where all code is to be in UPPERCASE.

For economy of size, do not actually include Snoopy generation in either the code or the output, instead just output a place-holder.


;Related task:
:*   [[Five weekends]]





## 360 Assembly

{{trans|Free Basic}}
This program uses no external functions but two ASSIST macros (XDECO, XPRNT) to keep the code as short as possible.

```360asm
*        calendar                  08/06/2016
CALENDAR CSECT
         USING  CALENDAR,R13       base register
         B      72(R15)            skip savearea
         DC     17F'0'             savearea
         STM    R14,R12,12(R13)    prolog
         ST     R13,4(R15)         "
         ST     R15,8(R13)         "
         LR     R13,R15            "
         L      R4,YEAR            year
         SRDA   R4,32              .
         D      R4,=F'4'           year//4
         LTR    R4,R4              if year//4=0
         BNZ    LYNOT
         L      R4,YEAR            year
         SRDA   R4,32              .
         D      R4,=F'100'         year//100
         LTR    R4,R4              if year//100=0
         BNZ    LY
         L      R4,YEAR            year
         SRDA   R4,32              .
         D      R4,=F'400'         if year//400
         LTR    R4,R4              if year//400=0
         BNZ    LYNOT
LY       MVC    ML+2,=H'29'        ml(2)=29  leapyear
LYNOT    SR     R10,R10            ltd1=0
         LA     R6,1               i=1
LOOPI1   C      R6,=F'31'          do i=1 to 31
         BH     ELOOPI1
         XDECO  R6,XDEC            edit i
         LA     R14,TD1            td1
         AR     R14,R10            td1+ltd1
         MVC    0(3,R14),XDEC+9    sub(td1,ltd1+1,3)=pic(i,3)
         LA     R10,3(R10)         ltd1+3
         LA     R6,1(R6)           i=i+1
         B      LOOPI1
ELOOPI1  LA     R6,1               i=1
LOOPI2   C      R6,=F'12'          do i=1 to 12
         BH     ELOOPI2
         ST     R6,M               m=i
         MVC    D,=F'1'            d=1
         MVC    YY,YEAR            yy=year
         L      R4,M               m
         C      R4,=F'3'           if m<3
         BNL    GE3
         L      R2,M               m
         LA     R2,12(R2)          m+12
         ST     R2,M               m=m+12
         L      R2,YY              yy
         BCTR   R2,0               yy-1
         ST     R2,YY              yy=yy-1
GE3      L      R2,YY              yy
         LR     R1,R2              yy
         SRA    R1,2               yy/4
         AR     R2,R1              yy+(yy/4)
         L      R4,YY              yy
         SRDA   R4,32              .
         D      R4,=F'100'         yy/100
         SR     R2,R5              yy+(yy/4)-(yy/100)
         L      R4,YY              yy
         SRDA   R4,32              .
         D      R4,=F'400'         yy/400
         AR     R2,R5              yy+(yy/4)-(yy/100)+(yy/400)
         A      R2,D               r2=yy+(yy/4)-(yy/100)+(yy/400)+d
         LA     R5,153             153
         M      R4,M               153*m
         LA     R5,8(R5)           153*m+8
         D      R4,=F'5'           (153*m+8)/5
         AR     R5,R2              ((153*m+8)/5+r2
         LA     R4,0               .
         D      R4,=F'7'           r4=mod(r5,7)  0=sun 1=mon ... 6=sat
         LTR    R4,R4              if j=0
         BNZ    JNE0
         LA     R4,7               j=7
JNE0     BCTR   R4,0               j-1
         MH     R4,=H'3'           j*3
         LR     R10,R4             j1=j*3
         LR     R1,R6              i
         SLA    R1,1               *2
         LH     R11,ML-2(R1)       ml(i)
         MH     R11,=H'3'          j2=ml(i)*3
         MVC    TD2,BLANK          td2=' '
         LA     R4,TD1             @td1
         LR     R5,R11             j2
         LA     R2,TD2             @td2
         AR     R2,R10             @td2+j1
         LR     R3,R5              j2
         MVCL   R2,R4              sub(td2,j1+1,j2)=sub(td1,1,j2)
         LR     R1,R6              i
         MH     R1,=H'144'         *144
         LA     R14,DA-144(R1)     @da(i)
         MVC    0(144,R14),TD2     da(i)=td2
         LA     R6,1(R6)           i=i+1
         B      LOOPI2
ELOOPI2  L      R1,YEAR            year
         XDECO  R1,PG+23           edit year
         XPRNT  PG,35              print year
         MVC    WDLINE,BLANK       wdline=' '
         LA     R10,1              lwdline=1
         LA     R8,1               k=1
LOOPK3   C      R8,=F'3'           do k=1 to 3
         BH     ELOOPK3
         LA     R4,WDLINE          @wdline
         AR     R4,R10             +lwdline
         MVC    0(20,R4),WDNA      sub(wdline,lwdline+1,20)=wdna
         LA     R10,20(R10)        lwdline=lwdline+20
         C      R8,=F'3'           if k<3
         BNL    ITERK3
         LA     R10,2(R10)         lwdline=lwdline+2
ITERK3   LA     R8,1(R8)           k=k+1
         B      LOOPK3
ELOOPK3  LA     R6,1               i=1
LOOPI4   C      R6,=F'12'          do i=1 to 12 by 3
         BH     ELOOPI4
         MVC    MOLINE,BLANK       moline=' '
         LA     R10,6              lmoline=6
         LR     R8,R6              k=i
LOOPK4   LA     R2,2(R6)           i+2
         CR     R8,R2              do k=i to i+2
         BH     ELOOPK4
         LR     R1,R8              k
         MH     R1,=H'10'          *10
         LA     R3,MO-10(R1)       mo(k)
         LA     R4,MOLINE          @moline
         AR     R4,R10             +lmoline
         MVC    0(10,R4),0(R3)     sub(moline,lmoline+1,10)=mo(k)
         LA     R10,22(R10)        lmoline=lmoline+22
         LA     R8,1(R8)           k=k+1
         B      LOOPK4
ELOOPK4  XPRNT  MOLINE,L'MOLINE    print months
         XPRNT  WDLINE,L'WDLINE    print days of week
         LA     R7,1               j=1
LOOPJ4   C      R7,=F'106'         do j=1 to 106 by 21
         BH     ELOOPJ4
         MVC    PG,BLANK           clear buffer
         LA     R9,PG              pgi=0
         LR     R8,R6              k=i
LOOPK5   LA     R2,2(R6)           i+2
         CR     R8,R2              do k=i to i+2
         BH     ELOOPK5
         LR     R1,R8              k
         MH     R1,=H'144'         *144
         LA     R4,DA-144(R1)      da(k)
         BCTR   R4,0               -1
         AR     R4,R7              +j
         MVC    0(21,R9),0(R4)     substr(da(k),j,21)
         LA     R9,22(R9)          pgi=pgi+22
         LA     R8,1(R8)           k=k+1
         B      LOOPK5
ELOOPK5  XPRNT  PG,L'PG            print buffer
         LA     R7,21(R7)          j=j+21
         B      LOOPJ4
ELOOPJ4  LA     R6,3(R6)           i=i+3
         B      LOOPI4
ELOOPI4  L      R13,4(0,R13)       epilog
         LM     R14,R12,12(R13)    "
         XR     R15,R15            "
         BR     R14                exit
         LTORG
YEAR     DC     F'1969'            <==
MO       DC     CL10' January  ',CL10' February ',CL10'  March   '
         DC     CL10'  April   ',CL10'   May    ',CL10'   June   '
         DC     CL10'   July   ',CL10'  August  ',CL10'September '
         DC     CL10' October  ',CL10' November ',CL10' December '
ML       DC     H'31',H'28',H'31',H'30',H'31',H'30'
         DC     H'31',H'31',H'30',H'31',H'30',H'31'
WDNA     DC     CL20'Mo Tu We Th Fr Sa Su'
M        DS     F
D        DS     F
YY       DS     F
TD1      DS     CL93
TD2      DS     CL144
MOLINE   DS     CL66
WDLINE   DS     CL66
PG       DC     CL66' '
XDEC     DS     CL12
BLANK    DC     CL144' '
DA       DS     12CL144
         YREG
         END    CALENDAR
```

{{out}}

```txt

                               2016
       January               February               March
 Mo Tu We Th Fr Sa Su  Mo Tu We Th Fr Sa Su  Mo Tu We Th Fr Sa Su
              1  2  3   1  2  3  4  5  6  7      1  2  3  4  5  6
  4  5  6  7  8  9 10   8  9 10 11 12 13 14   7  8  9 10 11 12 13
 11 12 13 14 15 16 17  15 16 17 18 19 20 21  14 15 16 17 18 19 20
 18 19 20 21 22 23 24  22 23 24 25 26 27 28  21 22 23 24 25 26 27
 25 26 27 28 29 30 31  29                    28 29 30 31

        April                  May                   June
 Mo Tu We Th Fr Sa Su  Mo Tu We Th Fr Sa Su  Mo Tu We Th Fr Sa Su
              1  2  3                     1         1  2  3  4  5
  4  5  6  7  8  9 10   2  3  4  5  6  7  8   6  7  8  9 10 11 12
 11 12 13 14 15 16 17   9 10 11 12 13 14 15  13 14 15 16 17 18 19
 18 19 20 21 22 23 24  16 17 18 19 20 21 22  20 21 22 23 24 25 26
 25 26 27 28 29 30     23 24 25 26 27 28 29  27 28 29 30
                       30 31
         July                 August              September
 Mo Tu We Th Fr Sa Su  Mo Tu We Th Fr Sa Su  Mo Tu We Th Fr Sa Su
              1  2  3   1  2  3  4  5  6  7            1  2  3  4
  4  5  6  7  8  9 10   8  9 10 11 12 13 14   5  6  7  8  9 10 11
 11 12 13 14 15 16 17  15 16 17 18 19 20 21  12 13 14 15 16 17 18
 18 19 20 21 22 23 24  22 23 24 25 26 27 28  19 20 21 22 23 24 25
 25 26 27 28 29 30 31  29 30 31              26 27 28 29 30

       October               November              December
 Mo Tu We Th Fr Sa Su  Mo Tu We Th Fr Sa Su  Mo Tu We Th Fr Sa Su
                 1  2      1  2  3  4  5  6            1  2  3  4
  3  4  5  6  7  8  9   7  8  9 10 11 12 13   5  6  7  8  9 10 11
 10 11 12 13 14 15 16  14 15 16 17 18 19 20  12 13 14 15 16 17 18
 17 18 19 20 21 22 23  21 22 23 24 25 26 27  19 20 21 22 23 24 25
 24 25 26 27 28 29 30  28 29 30              26 27 28 29 30 31
 31

```



## Ada


We first specify a "Printable_Calendar" package (printable_calendar.ads).
One can easily override the default names for weekdays and months, see
the [[Calendar_-_for_"real"_programmers#Ada]] task.


```Ada
with Ada.Calendar.Formatting;

package Printable_Calendar is

   subtype String20 is String(1 .. 20);
   type Month_Rep_Type is array (Ada.Calendar.Month_Number) of String20;

   type Description is record
      Weekday_Rep: String20;
      Month_Rep: Month_Rep_Type;
   end record;
   -- for internationalization, you only need to define a new description

   Default_Description: constant Description :=
     (Weekday_Rep =>
       "Mo Tu We Th Fr Sa So",
      Month_Rep   =>
      ("      January       ", "      February      ", "       March        ",
       "       April        ", "        May         ", "        June        ",
       "        July        ", "       August       ", "      September     ",
       "       October      ", "      November      ", "      December      "));

   type Calendar (<>) is tagged private;

   -- Initialize a calendar for devices with 80- or 132-characters per row
   function Init_80(Des: Description := Default_Description) return Calendar;
   function Init_132(Des: Description := Default_Description) return Calendar;

   -- the following procedures output to standard IO; override if neccessary
   procedure New_Line(Cal: Calendar);
   procedure Put_String(Cal: Calendar; S: String);

   -- the following procedures do the real stuff
   procedure Print_Line_Centered(Cal: Calendar'Class; Line: String);
   procedure Print(Cal: Calendar'Class;
                   Year:  Ada.Calendar.Year_Number;
                   Year_String: String); -- this is the main Thing

private
      type Calendar is tagged record
      Columns, Rows, Space_Between_Columns: Positive;
      Left_Space: Natural;

      Weekday_Rep: String20;
      Month_Rep: Month_Rep_Type;
   end record;

end Printable_Calendar;
```


We continue with the implementation (printable_calendar.ads):


```Ada
with Ada.Text_IO;

package body Printable_Calendar is

   use Ada.Calendar;
   package F renames Ada.Calendar.Formatting;

   function Days_Per_Month(Year: Year_Number; Month: Month_Number)
                          return Day_Number is
   begin
      case Month is
         when 1 | 3 | 5 | 7 | 8 | 10 | 12 => return 31;
         when 4 | 6 | 9 | 11              => return 30;
         when 2 =>
            if Year mod 4 /= 0 then
               return 28;
            elsif Year mod 100 /= 0 then
               return 29;
            elsif Year mod 400 /= 0 then
               return 28;
            else
               return 29;
            end if;
      end case;
   end Days_Per_Month;

   type Full_Month_Rep is array (1 .. 6) of String20;
   function Generate_Printable_Month (Y: Ada.Calendar.Year_Number;
                                      M: Ada.Calendar.Month_Number)
                                     return Full_Month_Rep is

      X: Full_Month_Rep := (others => "                    ");
      -- If X=Generate_Printable_Month(2011, 01), the result could be
      --  "       January      ", -- Month_Rep(01)
      --  "Mo Tu We Th Fr Sa Su"  -- Weekday_Rep
      --  "                1  2"  -- X(1)
      --  " 3  4  5  6  7  8  9"  -- X(2)
      --  "10 11 12 13 14 15 16"  -- X(3)
      --  "17 18 19 20 21 22 23"  -- X(4)
      --  "24 25 26 27 28 29 30"  -- X(5)
      --  "31                  "  -- X(6)

      Row:       Integer range 1 .. 6  := 1;
      Day_Index: constant array(F.Day_Name) of Positive
        := (1, 4, 7, 10, 13, 16, 19);
   begin
      for I in 1 .. Days_Per_Month(Y, M) loop
         declare
           Weekday: constant F.Day_Name    := F.Day_Of_Week(F.Time_Of(Y, M, I));
           Pos: constant Positive          := Day_Index(Weekday);
           Cleartext_Name: constant String := Day_Number'Image(I);
           L: constant Positive            := Cleartext_Name'Last;
         begin
            X(Row)(Pos .. Pos+1) := Cleartext_Name(L-1 .. L);
            if F."="(Weekday, F.Sunday) then
               Row := Row + 1;
            end if;
         end;
      end loop;
      return X;
   end Generate_Printable_Month;


   procedure Print(Cal: Calendar'class;
                   Year:  Ada.Calendar.Year_Number;
                   Year_String: String) is

      The_Month: Month_Number := Month_Number'First;

      procedure Write_Space(Length: Natural) is
      begin
         for I in 1 .. Length loop
            Cal.Put_String(" ");
         end loop;
      end Write_Space;

      Year_Rep: array(Month_Number) of Full_Month_Rep;

   begin
      -- print the year
      Cal.Print_Line_Centered(Year_String);

      -- generate a printable form for all the months
      for Month in Month_Number loop
         Year_Rep(Month) := Generate_Printable_Month(Year, Month);
      end loop;

      begin
         while True loop

            -- new line
            Cal.New_Line;

            -- write month names
            Write_Space(Cal.Left_Space);
            for Month in The_Month .. The_Month+Cal.Columns-2 loop
               Cal.Put_String(Cal.Month_Rep(Month));
               Write_Space(Cal.Space_Between_Columns);
            end loop;
            Cal.Put_String(Cal.Month_Rep(The_Month+Cal.Columns-1));
            Cal.New_Line;

            -- write "Mo Tu .. So" - or whatever is defined by Weekday_Rep
            Write_Space(Cal.Left_Space);
            for Month in The_Month .. The_Month+Cal.Columns-2 loop
               Cal.Put_String(Cal.Weekday_Rep);
               Write_Space(Cal.Space_Between_Columns);
            end loop;
            Cal.Put_String(Cal.Weekday_Rep);
            Cal.New_Line;

            -- write the dates
            for I in 1 .. 6 loop
               Write_Space(Cal.Left_Space);
               for Month in The_Month .. The_Month+Cal.Columns-2 loop
                  Cal.Put_String(Year_Rep(Month)(I));
                  Write_Space(Cal.Space_Between_Columns);
               end loop;
            Cal.Put_String(Year_Rep(The_Month+Cal.Columns-1)(I));
            Cal.New_Line;
            end loop;

            The_Month := The_Month + Cal.Columns;
            -- this will eventually raise Constraint_Error to terminate the loop
         end loop;
      exception
         when Constraint_Error => null;
      end;
   end Print;

   procedure New_Line(Cal: Calendar) is
   begin
      Ada.Text_IO.New_Line;
   end New_Line;

   procedure Put_String(Cal: Calendar; S: String) is
   begin
      Ada.Text_IO.Put(S);
   end Put_String;

   procedure Print_Line_Centered(Cal: Calendar'Class; Line: String) is
      Width : constant Positive := Cal.Columns*20
        + (Cal.Columns-1)*Cal.Space_Between_Columns
        + Cal.Left_Space;
   begin
      if Line'Length    >= Width-1 then
         Cal.Put_String(Line);
         Cal.New_Line;
      else
         Print_Line_Centered(Cal, " " & Line & " ");
      end if;
   end Print_Line_Centered;

   function Init_80(Des: Description := Default_Description) return Calendar is
      X: Calendar:=
        (Columns => 3, Rows => 4, Space_Between_Columns => 4,
         Left_Space => 1,
         Weekday_Rep =>  Des.Weekday_Rep,
         Month_Rep   =>  Des.Month_Rep
        );
   begin
      return X;
   end Init_80;

   function Init_132(Des: Description := Default_Description) return Calendar is
      X: Calendar:=
        (Columns => 6, Rows => 2, Space_Between_Columns => 2,
         Left_Space => 1,
         Weekday_Rep =>  Des.Weekday_Rep,
         Month_Rep   =>  Des.Month_Rep
        );
   begin
      return X;
   end Init_132;

end Printable_Calendar;
```


Now, the main program is really simple:


```Ada
with Printable_Calendar;

procedure Cal is

   C: Printable_Calendar.Calendar := Printable_Calendar.Init_80;

begin
   C.Print_Line_Centered("[reserved for Snoopy]");
   C.New_Line;
   C.Print(1969, "Nineteen-Sixty-Nine");
end Cal;
```


Here is the output:


```txt
                        [reserved for Snoopy]

                         Nineteen-Sixty-Nine

       January                 February                 March
 Mo Tu We Th Fr Sa So    Mo Tu We Th Fr Sa So    Mo Tu We Th Fr Sa So
        1  2  3  4  5                    1  2                    1  2
  6  7  8  9 10 11 12     3  4  5  6  7  8  9     3  4  5  6  7  8  9
 13 14 15 16 17 18 19    10 11 12 13 14 15 16    10 11 12 13 14 15 16
 20 21 22 23 24 25 26    17 18 19 20 21 22 23    17 18 19 20 21 22 23
 27 28 29 30 31          24 25 26 27 28          24 25 26 27 28 29 30
                                                 31

        April                    May                     June
 Mo Tu We Th Fr Sa So    Mo Tu We Th Fr Sa So    Mo Tu We Th Fr Sa So
     1  2  3  4  5  6              1  2  3  4                       1
  7  8  9 10 11 12 13     5  6  7  8  9 10 11     2  3  4  5  6  7  8
 14 15 16 17 18 19 20    12 13 14 15 16 17 18     9 10 11 12 13 14 15
 21 22 23 24 25 26 27    19 20 21 22 23 24 25    16 17 18 19 20 21 22
 28 29 30                26 27 28 29 30 31       23 24 25 26 27 28 29
                                                 30

         July                   August                 September
 Mo Tu We Th Fr Sa So    Mo Tu We Th Fr Sa So    Mo Tu We Th Fr Sa So
     1  2  3  4  5  6                 1  2  3     1  2  3  4  5  6  7
  7  8  9 10 11 12 13     4  5  6  7  8  9 10     8  9 10 11 12 13 14
 14 15 16 17 18 19 20    11 12 13 14 15 16 17    15 16 17 18 19 20 21
 21 22 23 24 25 26 27    18 19 20 21 22 23 24    22 23 24 25 26 27 28
 28 29 30 31             25 26 27 28 29 30 31    29 30


        October                November                December
 Mo Tu We Th Fr Sa So    Mo Tu We Th Fr Sa So    Mo Tu We Th Fr Sa So
        1  2  3  4  5                    1  2     1  2  3  4  5  6  7
  6  7  8  9 10 11 12     3  4  5  6  7  8  9     8  9 10 11 12 13 14
 13 14 15 16 17 18 19    10 11 12 13 14 15 16    15 16 17 18 19 20 21
 20 21 22 23 24 25 26    17 18 19 20 21 22 23    22 23 24 25 26 27 28
 27 28 29 30 31          24 25 26 27 28 29 30    29 30 31
```



To get a 132-character-wide output, you just have to replace "Init_80" by "Init_132" in the main program.


## ALGOL 68

{{works with|ALGOL 68|Revision 1 - no extensions to language used}}
{{works with|ALGOL 68G|Any - tested with release [http://sourceforge.net/projects/algol68/files/algol68g/algol68g-1.18.0/algol68g-1.18.0-9h.tiny.el5.centos.fc11.i386.rpm/download 1.18.0-9h.tiny].}}
{{wont work with|ELLA ALGOL 68|Any (with appropriate job cards) - tested with release [http://sourceforge.net/projects/algol68/files/algol68toc/algol68toc-1.8.8d/algol68toc-1.8-8d.fc9.i386.rpm/download 1.8-8d] - due to extensive use of '''format'''[ted] ''transput''.}}

```algol68
#!/usr/local/bin/a68g --script #

PROC print calendar = (INT year, page width)VOID: (

  []STRING month names = (
      "January","February","March","April","May","June",
      "July","August","September","October","November","December"),
    weekday names = ("Su","Mo","Tu","We","Th","Fr","Sa");
  FORMAT weekday fmt = $g,n(UPB weekday names - LWB weekday names)(" "g)$;

  # Juggle the calendar format to fit the printer/screen width #
  INT day width = UPB weekday names[1], day gap=1;
  INT month width = (day width+day gap) * UPB weekday names-1;
  INT month heading lines = 2;
  INT month lines = (31 OVER UPB weekday names+month heading lines+2); # +2 for head/tail weeks #
  INT year cols = (page width+1) OVER (month width+1);
  INT year rows = (UPB month names-1)OVER year cols + 1;
  INT month gap = (page width - year cols*month width + 1)OVER year cols;
  INT year width = year cols*(month width+month gap)-month gap;
  INT year lines = year rows*month lines;

  MODE MONTHBOX = [month lines, month width]CHAR;
  MODE YEARBOX  = [year  lines, year  width]CHAR;

  INT week start = 1; # Sunday #

  PROC days in month = (INT year, month)INT:
    CASE month IN 31,
      IF year MOD 4 EQ 0 AND year MOD 100 NE 0 OR year MOD 400 EQ 0 THEN 29 ELSE 28 FI,
      31, 30, 31, 30, 31, 31, 30, 31, 30, 31
    ESAC;

  PROC day of week = (INT year, month, day)INT: (
    # Day of the week by Zeller’s Congruence algorithm from 1887 #
    INT y := year, m := month, d := day, c;
    IF m <= 2 THEN m +:= 12; y -:= 1 FI;
    c := y OVER 100;
    y %*:= 100;
    (d - 1 + ((m + 1) * 26) OVER 10 + y + y OVER 4 + c OVER 4 - 2 * c) MOD 7
  );

  MODE SIMPLEOUT = UNION(STRING, []STRING, INT);

  PROC cputf = (REF[]CHAR out, FORMAT fmt, SIMPLEOUT argv)VOID:(
    FILE f; STRING s; associate(f,s);
    putf(f, (fmt, argv));
    out[:UPB s]:=s;
    close(f)
  );

  PROC month repr = (INT year, month)MONTHBOX:(
    MONTHBOX month box; FOR line TO UPB month box DO month box[line,]:=" "* 2 UPB month box OD;
    STRING month name = month names[month];

  # center the title #
    cputf(month box[1,(month width - UPB month name ) OVER 2+1:], $g$, month name);
    cputf(month box[2,], weekday fmt, weekday names);

    INT first day := day of week(year, month, 1);
    FOR day TO days in month(year, month) DO
      INT line = (day+first day-week start) OVER UPB weekday names + month heading lines + 1;
      INT char =((day+first day-week start) MOD UPB weekday names)*(day width+day gap) + 1;
      cputf(month box[line,char:char+day width-1],$g(-day width)$, day)
    OD;
    month box
  );

  PROC year repr = (INT year)YEARBOX:(
    YEARBOX year box;
    FOR line TO UPB year box DO year box[line,]:=" "* 2 UPB year box OD;
    FOR month row FROM 0 TO year rows-1 DO
      FOR month col FROM 0 TO year cols-1 DO
        INT month = month row * year cols + month col + 1;
        IF month > UPB month names THEN
          done
        ELSE
          INT month col width = month width+month gap;
          year box[
            month row*month lines+1 : (month row+1)*month lines,
            month col*month col width+1 : (month col+1)*month col width-month gap
          ] := month repr(year, month)
        FI
      OD
    OD;
    done: year box
  );

  INT center = (year cols*(month width+month gap) - month gap - 1) OVER 2;
  INT indent = (page width - year width) OVER 2;

  printf((
    $n(indent + center - 9)k  g l$, "[Insert Snoopy here]",
    $n(indent + center - 1)k 4d l$, year, $l$,
    $n(indent)k n(year width)(g)  l$, year repr(year)
  ))
);

main: (
CO inspired by http://www.ee.ryerson.ca/~elf/hack/realmen.html
           Real Programmers Don't Use PASCAL - Ed Post
            Datamation, volume 29 number 7, July 1983
              THE REAL PROGRAMMER'S NATURAL HABITAT
"Taped to the wall is a line-printer Snoopy calender for the year 1969."
CO
  INT mankind stepped on the moon = 1969,
      line printer width = 80; # as at 1969! #
  print calendar(mankind stepped on the moon, line printer width)
)
```

Output:

```txt

                             [Insert Snoopy here]
                                     1969

        January                    February                    March
  Su Mo Tu We Th Fr Sa       Su Mo Tu We Th Fr Sa       Su Mo Tu We Th Fr Sa
            1  2  3  4                          1                          1
   5  6  7  8  9 10 11        2  3  4  5  6  7  8        2  3  4  5  6  7  8
  12 13 14 15 16 17 18        9 10 11 12 13 14 15        9 10 11 12 13 14 15
  19 20 21 22 23 24 25       16 17 18 19 20 21 22       16 17 18 19 20 21 22
  26 27 28 29 30 31          23 24 25 26 27 28          23 24 25 26 27 28 29
                                                        30 31
         April                       May                        June
  Su Mo Tu We Th Fr Sa       Su Mo Tu We Th Fr Sa       Su Mo Tu We Th Fr Sa
         1  2  3  4  5                    1  2  3        1  2  3  4  5  6  7
   6  7  8  9 10 11 12        4  5  6  7  8  9 10        8  9 10 11 12 13 14
  13 14 15 16 17 18 19       11 12 13 14 15 16 17       15 16 17 18 19 20 21
  20 21 22 23 24 25 26       18 19 20 21 22 23 24       22 23 24 25 26 27 28
  27 28 29 30                25 26 27 28 29 30 31       29 30

          July                      August                   September
  Su Mo Tu We Th Fr Sa       Su Mo Tu We Th Fr Sa       Su Mo Tu We Th Fr Sa
         1  2  3  4  5                       1  2           1  2  3  4  5  6
   6  7  8  9 10 11 12        3  4  5  6  7  8  9        7  8  9 10 11 12 13
  13 14 15 16 17 18 19       10 11 12 13 14 15 16       14 15 16 17 18 19 20
  20 21 22 23 24 25 26       17 18 19 20 21 22 23       21 22 23 24 25 26 27
  27 28 29 30 31             24 25 26 27 28 29 30       28 29 30
                             31
        October                    November                   December
  Su Mo Tu We Th Fr Sa       Su Mo Tu We Th Fr Sa       Su Mo Tu We Th Fr Sa
            1  2  3  4                          1           1  2  3  4  5  6
   5  6  7  8  9 10 11        2  3  4  5  6  7  8        7  8  9 10 11 12 13
  12 13 14 15 16 17 18        9 10 11 12 13 14 15       14 15 16 17 18 19 20
  19 20 21 22 23 24 25       16 17 18 19 20 21 22       21 22 23 24 25 26 27
  26 27 28 29 30 31          23 24 25 26 27 28 29       28 29 30 31
                             30

```



## ALGOL W

{{trans|Simula}} which is a {{trans|C}}
This is pretty much the same as the Simula version.


The main differences are that the "for" loop counter in Algol W is a separate variable that only exists during execution of the loop
whereas in Simula the loop counter must be declared outside the loop and has its value changed as the loop executes
- hence some of the "for" loops have been replaced by "while" loops.

In Algol W, the condition of a "while" loop can be a block and the boolean "and" operator short circuits, which allow us to avoid the goto statements.

Also, the Algol W string type is fixed length, unlike the Simula text type.

```algolw
BEGIN
    INTEGER WIDTH, YEAR;
    INTEGER COLS, LEAD, GAP;
    STRING(2) ARRAY WDAYS (0::6);
    RECORD MONTH ( STRING(9) MNAME; INTEGER DAYS, START_WDAY, AT_POS );
    REFERENCE(MONTH) ARRAY MONTHS(0::11);
    WIDTH := 80; YEAR := 1969;

    BEGIN
        WDAYS(0) := "Su"; WDAYS(1) := "Mo"; WDAYS(2) := "Tu";
        WDAYS(3) := "We"; WDAYS(4) := "Th"; WDAYS(5) := "Fr"; WDAYS(6) := "Sa";
        MONTHS( 0) := MONTH(" January",  31, 0, 0 );
        MONTHS( 1) := MONTH(" February", 28, 0, 0 );
        MONTHS( 2) := MONTH("  March",   31, 0, 0 );
        MONTHS( 3) := MONTH("  April",   30, 0, 0 );
        MONTHS( 4) := MONTH("   May",    31, 0, 0 );
        MONTHS( 5) := MONTH("   June",   30, 0, 0 );
        MONTHS( 6) := MONTH("   July",   31, 0, 0 );
        MONTHS( 7) := MONTH("  August",  31, 0, 0 );
        MONTHS( 8) := MONTH("September", 30, 0, 0 );
        MONTHS( 9) := MONTH(" October",  31, 0, 0 );
        MONTHS(10) := MONTH(" November", 30, 0, 0 );
        MONTHS(11) := MONTH(" December", 31, 0, 0 )
    END;

    BEGIN

        PROCEDURE SPACE(INTEGER VALUE N);
        BEGIN
            WHILE N > 0 DO BEGIN
                WRITEON(" "); N := N-1;
            END
        END SPACE;

        PROCEDURE INIT_MONTHS;
        BEGIN
            INTEGER I;

            IF YEAR REM 4 = 0 AND YEAR REM 100 NOT = 0 OR YEAR REM 400 = 0 THEN
                DAYS(MONTHS(1)) := 29;

            YEAR := YEAR-1;
            START_WDAY(MONTHS(0))
                := (YEAR * 365 + YEAR DIV 4 - YEAR DIV 100 + YEAR DIV 400 + 1) REM 7;

            FOR I := 1 STEP 1 UNTIL 12-1 DO
                START_WDAY(MONTHS(I)) :=
                    (START_WDAY(MONTHS(I-1)) + DAYS(MONTHS(I-1))) REM 7;

            COLS := (WIDTH + 2) DIV 22;
            WHILE 12 REM COLS NOT = 0 DO
                COLS := COLS-1;
            GAP := IF COLS - 1 NOT = 0 THEN (WIDTH - 20 * COLS) DIV (COLS - 1) ELSE 0;
            IF GAP > 4 THEN
                GAP := 4;
            LEAD := (WIDTH - (20 + GAP) * COLS + GAP + 1) DIV 2;
            YEAR := YEAR+1
        END INIT_MONTHS;

        PROCEDURE PRINT_ROW(INTEGER VALUE ROW);
        BEGIN
            INTEGER C, I, FROM, UP_TO;
            INTEGER PROCEDURE PREINCREMENT(INTEGER VALUE RESULT I);
            BEGIN I := I+1; I
            END PREINCREMENT;
            INTEGER PROCEDURE POSTINCREMENT(INTEGER VALUE RESULT I);
            BEGIN INTEGER PREV_VALUE;
                PREV_VALUE := I; I := I+1; PREV_VALUE
            END POSTINCREMENT;
            FROM := ROW * COLS;
            UP_TO := FROM + COLS;
            SPACE(LEAD);
            FOR C := FROM STEP 1 UNTIL UP_TO-1 DO BEGIN
                I := 9 % LENGTH OF MNAME(MONTHS(C)) % ;
                SPACE((20 - I) DIV 2);
                WRITEON(MNAME(MONTHS(C)));
                SPACE(20 - I - (20 - I) DIV 2 + (IF C = UP_TO - 1 THEN 0 ELSE GAP));
            END;
            WRITE();

            SPACE(LEAD);
            FOR C := FROM STEP 1 UNTIL UP_TO-1 DO BEGIN
                FOR I := 0 STEP 1 UNTIL 7-1 DO BEGIN
                    WRITEON(WDAYS(I)); IF I NOT = 6 THEN WRITEON(" ")
                END;
                IF C < UP_TO - 1 THEN
                    SPACE(GAP)
                ELSE
                    WRITE();
            END;
            WHILE BEGIN
                C := FROM;
                WHILE C < UP_TO AND AT_POS(MONTHS(C)) >= DAYS(MONTHS(C)) DO
                    C := C + 1;

                C NOT = UP_TO
            END DO BEGIN

                SPACE(LEAD);
                C := FROM;
                WHILE C < UP_TO DO BEGIN
                    I := 0;
                    WHILE I < START_WDAY(MONTHS(C)) DO BEGIN
                        I := I + 1;
                        SPACE(3)
                    END;
                    WHILE POSTINCREMENT(I) < 7 AND AT_POS(MONTHS(C)) < DAYS(MONTHS(C)) DO BEGIN
                        WRITEON(I_W := 2, S_W := 0, PREINCREMENT(AT_POS(MONTHS(C))));
                        IF I < 7 OR C < UP_TO - 1 THEN
                            SPACE(1)
                    END;
                    WHILE POSTINCREMENT(I) <= 7 AND C < UP_TO-1 DO
                        SPACE(3);
                    IF C < UP_TO - 1 THEN
                        SPACE(GAP - 1);
                    START_WDAY(MONTHS(C)) := 0;
                    C := C + 1
                END;
                WRITE();
            END;
            WRITE()
        END PRINT_ROW;

        PROCEDURE PRINT_YEAR;
        BEGIN
            INTEGER ROW, STRLEN, Y;
            STRLEN := 1;
            Y      := YEAR;
            WHILE Y > 9 DO BEGIN Y := Y DIV 10; STRLEN := STRLEN + 1 END;
            SPACE((WIDTH - STRLEN) DIV 2);
            WRITEON(I_W := 1, YEAR);
            WRITE(); WRITE();
            WHILE ROW * COLS < 12 DO BEGIN
                PRINT_ROW(ROW);
                ROW := ROW+1
            END
        END PRINT_YEAR;

        INIT_MONTHS;
        PRINT_YEAR
    END
END.
```

{{out}}

```txt


                                      1969

            January                 February                 March
      Su Mo Tu We Th Fr Sa    Su Mo Tu We Th Fr Sa    Su Mo Tu We Th Fr Sa
                1  2  3  4                       1                       1
       5  6  7  8  9 10 11     2  3  4  5  6  7  8     2  3  4  5  6  7  8
      12 13 14 15 16 17 18     9 10 11 12 13 14 15     9 10 11 12 13 14 15
      19 20 21 22 23 24 25    16 17 18 19 20 21 22    16 17 18 19 20 21 22
      26 27 28 29 30 31       23 24 25 26 27 28       23 24 25 26 27 28 29
                                                      30 31

             April                    May                     June
      Su Mo Tu We Th Fr Sa    Su Mo Tu We Th Fr Sa    Su Mo Tu We Th Fr Sa
             1  2  3  4  5                 1  2  3     1  2  3  4  5  6  7
       6  7  8  9 10 11 12     4  5  6  7  8  9 10     8  9 10 11 12 13 14
      13 14 15 16 17 18 19    11 12 13 14 15 16 17    15 16 17 18 19 20 21
      20 21 22 23 24 25 26    18 19 20 21 22 23 24    22 23 24 25 26 27 28
      27 28 29 30             25 26 27 28 29 30 31    29 30

              July                   August                September
      Su Mo Tu We Th Fr Sa    Su Mo Tu We Th Fr Sa    Su Mo Tu We Th Fr Sa
             1  2  3  4  5                    1  2        1  2  3  4  5  6
       6  7  8  9 10 11 12     3  4  5  6  7  8  9     7  8  9 10 11 12 13
      13 14 15 16 17 18 19    10 11 12 13 14 15 16    14 15 16 17 18 19 20
      20 21 22 23 24 25 26    17 18 19 20 21 22 23    21 22 23 24 25 26 27
      27 28 29 30 31          24 25 26 27 28 29 30    28 29 30
                              31

            October                 November                December
      Su Mo Tu We Th Fr Sa    Su Mo Tu We Th Fr Sa    Su Mo Tu We Th Fr Sa
                1  2  3  4                       1        1  2  3  4  5  6
       5  6  7  8  9 10 11     2  3  4  5  6  7  8     7  8  9 10 11 12 13
      12 13 14 15 16 17 18     9 10 11 12 13 14 15    14 15 16 17 18 19 20
      19 20 21 22 23 24 25    16 17 18 19 20 21 22    21 22 23 24 25 26 27
      26 27 28 29 30 31       23 24 25 26 27 28 29    28 29 30 31
                              30



```



## AutoHotkey


```AutoHotkey
Calendar(Yr){
	LastDay := [], Day := []
	Titles =
	(ltrim
	______January_________________February_________________March_______
	_______April____________________May____________________June________
	________July___________________August_________________September_____
	______October_________________November________________December______
	)
	StringSplit, title, titles, `n
	Res := "________________________________" Yr "`r`n"

	loop 4 {										; 4 Vertical Sections
		Day[1]:=Yr SubStr("0" A_Index*3 -2, -1) 01
		Day[2]:=Yr SubStr("0" A_Index*3 -1, -1) 01
		Day[3]:=Yr SubStr("0" A_Index*3   , -1) 01
		Res .= "`r`n" title%A_Index% "`r`nSu Mo Tu We Th Fr Sa    Su Mo Tu We Th Fr Sa    Su Mo Tu We Th Fr Sa"
		loop , 6 {									; 6 Weeks max per month
			Week := A_Index, Res .= "`r`n"
			loop, 21 {								; 3 weeks times 7 days
				Mon := Ceil(A_Index/7), ThisWD := Mod(A_Index-1,7)+1
				FormatTime, WD, % Day[Mon], WDay
				FormatTime, dd, % Day[Mon], dd
				if (WD>ThisWD) {
					Res .= "__ "
					continue
				}
				dd := ((Week>3) && dd <10) ? "__" : dd, Res .= dd " ", LastDay[Mon] := Day[Mon], Day[Mon] +=1, Days
				Res .= ((wd=7) && A_Index < 21) ? "___" : ""
				FormatTime, dd, % Day[Mon], dd
			}
		}
		Res .= "`r`n"
	}
	StringReplace, Res, Res,_,%A_Space%, all
	Res:=RegExReplace(Res,"`am)(^|\s)\K0", " ")
	return res
}
```

Examples:
```AutoHotkey
Gui, font,s8, COURIER
Gui, add, edit, vYr w40 r1 Limit4 Number, 1969
Gui, add, edit, vEdit2 w580 r38
Gui, Add, Button, Default Hidden gSubmit
Gui, show

Submit:
Gui, Submit, NoHide
GuiControl,, Edit2, % Calendar(Yr)
return

GuiEscape:
GuiClose:
ExitApp
return
```

Outputs:
```txt
                                1969

       January                February                  March
Su Mo Tu We Th Fr Sa    Su Mo Tu We Th Fr Sa    Su Mo Tu We Th Fr Sa
          1  2  3  4                       1                       1
 5  6  7  8  9 10 11     2  3  4  5  6  7  8     2  3  4  5  6  7  8
12 13 14 15 16 17 18     9 10 11 12 13 14 15     9 10 11 12 13 14 15
19 20 21 22 23 24 25    16 17 18 19 20 21 22    16 17 18 19 20 21 22
26 27 28 29 30 31       23 24 25 26 27 28       23 24 25 26 27 28 29
                                                30 31

        April                    May                    June
Su Mo Tu We Th Fr Sa    Su Mo Tu We Th Fr Sa    Su Mo Tu We Th Fr Sa
       1  2  3  4  5                 1  2  3     1  2  3  4  5  6  7
 6  7  8  9 10 11 12     4  5  6  7  8  9 10     8  9 10 11 12 13 14
13 14 15 16 17 18 19    11 12 13 14 15 16 17    15 16 17 18 19 20 21
20 21 22 23 24 25 26    18 19 20 21 22 23 24    22 23 24 25 26 27 28
27 28 29 30             25 26 27 28 29 30 31    29 30


        July                   August                 September
Su Mo Tu We Th Fr Sa    Su Mo Tu We Th Fr Sa    Su Mo Tu We Th Fr Sa
       1  2  3  4  5                    1  2        1  2  3  4  5  6
 6  7  8  9 10 11 12     3  4  5  6  7  8  9     7  8  9 10 11 12 13
13 14 15 16 17 18 19    10 11 12 13 14 15 16    14 15 16 17 18 19 20
20 21 22 23 24 25 26    17 18 19 20 21 22 23    21 22 23 24 25 26 27
27 28 29 30 31          24 25 26 27 28 29 30    28 29 30
                        31

       October                November                December
Su Mo Tu We Th Fr Sa    Su Mo Tu We Th Fr Sa    Su Mo Tu We Th Fr Sa
          1  2  3  4                       1        1  2  3  4  5  6
 5  6  7  8  9 10 11     2  3  4  5  6  7  8     7  8  9 10 11 12 13
12 13 14 15 16 17 18     9 10 11 12 13 14 15    14 15 16 17 18 19 20
19 20 21 22 23 24 25    16 17 18 19 20 21 22    21 22 23 24 25 26 27
26 27 28 29 30 31       23 24 25 26 27 28 29    28 29 30 31
                        30
```



## AutoIt


```AutoIt

#include <Date.au3>

; Set the count of characters in each line, minimum is 20 - one month.
Global $iPrintSize = 132

; Set the count of months, you want to print side by side. With "0" it calculates automatically.
; The number will corrected, if it not allowed to print in an rectangle.
; If your print size is to small for given count, it will set back to automatically calculation.
Global $iSideBySide = 3

; Set the count of spaces between months.
Global $iSpace = 4

_CreateCalendar( 1969 )


Func _CreateCalendar($_iYear)
	Local $aMon[12] = ['       January      ', '      February      ', '        March       ', _
	                   '        April       ', '         May        ', '        June        ', _
	                   '        July        ', '       August       ', '      September     ', _
	                   '       October      ', '      November      ', '      December      ']
	Local $sHead = 'Mo Tu We Th Fr Sa Su'
	Local $aDaysInMonth[12] = [31,28,31,30,31,30,31,31,30,31,30,31]
	If _DateIsLeapYear($_iYear) Then $aDaysInMonth[1] = 29

	; == assign date in weekday table for the whole year
	Local $aAllDaysInMonth[6][7][12]   ; [ lines ][ weekdays ][ months ]
	Local $iDay = 1, $iShift
	For $i = 1 To 12
		$iShift = _DateToDayOfWeekISO($_iYear, $i, 1) -1
		For $j = 0 To 5
			For $k = $iShift To 6
				$aAllDaysInMonth[$j][$k][$i-1] = $iDay
				$iDay += 1
				If $iDay > $aDaysInMonth[$i-1] Then ExitLoop(2)
			Next
			$iShift = 0
		Next
		$iDay = 1
	Next

	; == check given side by side count, calculate if needed
	If $iSideBySide > 0 Then
		If $iPrintSize < ($iSideBySide *(20 +$iSpace) -$iSpace) Then $iSideBySide = 0
	EndIf
	Switch $iSideBySide
		Case 0
			$iSideBySide = Int($iPrintSize /(20 +$iSpace))
			If $iPrintSize < 20 Then Return _PrintLine('Escape: Size Error')
			If $iPrintSize < (20 +$iSpace) Then $iSideBySide = 1
		Case 5
			$iSideBySide = 4
		Case 7 To 11
			$iSideBySide = 6
	EndSwitch

	; == create space string
	Local $sSpace = ''
	For $i = 1 To $iSpace
		$sSpace &= ' '
	Next

	; == print header
	_PrintLine(@LF)
	_PrintLine('[ here is Snoopy ]', @LF)
	_PrintLine(StringRegExpReplace($_iYear, '(\d)(\d)(\d)(\d)', '$1 $2 $3 $4'), @LF)

	; == create data for each line, in dependence to count of months in one line
	Local $sLine, $iRight, $sTmp1, $sTmp2
	For $n = 0 To 12 /$iSideBySide -1
		$sTmp1 = ''
		$sTmp2 = ''
		For $z = 0 To $iSideBySide -1
			$sTmp1 &= $aMon[$iSideBySide *$n+$z] & $sSpace
			$sTmp2 &= $sHead & $sSpace
		Next
		_PrintLine(StringTrimRight($sTmp1, $iSpace))
		_PrintLine(StringTrimRight($sTmp2, $iSpace))
		For $j = 0 To 5
			$sLine = ''
			For $i = 1 To $iSideBySide
				For $k = 0 To 6
					$iRight = 3
					If $k = 0 Then $iRight = 2
					$sLine &= StringRight('   ' & $aAllDaysInMonth[$j][$k][$iSideBySide*$n+$i-1], $iRight)
				Next
				If $i < $iSideBySide Then $sLine &= $sSpace
			Next
			_PrintLine($sLine)
		Next
	Next
EndFunc  ;==>_CreateCalendar

Func _PrintLine($_sLine, $_sLF='')
	Local $iLen = StringLen($_sLine)
	Local $sSpace = '', $sLeft = ''
	For $i = 1 To $iPrintSize-1
		$sSpace &= ' '
	Next
	If $iLen < $iPrintSize Then $sLeft = StringLeft($sSpace, Int(($iPrintSize-$iLen)/2))
	ConsoleWrite($sLeft & $_sLine & $_sLF & @LF)
EndFunc  ;==>_PrintLine

```

Output

```txt



                                                         [ here is Snoopy ]

                                                              1 9 6 9

                                       January                February                  March
                                Mo Tu We Th Fr Sa Su    Mo Tu We Th Fr Sa Su    Mo Tu We Th Fr Sa Su
                                       1  2  3  4  5                    1  2                    1  2
                                 6  7  8  9 10 11 12     3  4  5  6  7  8  9     3  4  5  6  7  8  9
                                13 14 15 16 17 18 19    10 11 12 13 14 15 16    10 11 12 13 14 15 16
                                20 21 22 23 24 25 26    17 18 19 20 21 22 23    17 18 19 20 21 22 23
                                27 28 29 30 31          24 25 26 27 28          24 25 26 27 28 29 30
                                                                                31
                                        April                    May                    June
                                Mo Tu We Th Fr Sa Su    Mo Tu We Th Fr Sa Su    Mo Tu We Th Fr Sa Su
                                    1  2  3  4  5  6              1  2  3  4                       1
                                 7  8  9 10 11 12 13     5  6  7  8  9 10 11     2  3  4  5  6  7  8
                                14 15 16 17 18 19 20    12 13 14 15 16 17 18     9 10 11 12 13 14 15
                                21 22 23 24 25 26 27    19 20 21 22 23 24 25    16 17 18 19 20 21 22
                                28 29 30                26 27 28 29 30 31       23 24 25 26 27 28 29
                                                                                30
                                        July                   August                 September
                                Mo Tu We Th Fr Sa Su    Mo Tu We Th Fr Sa Su    Mo Tu We Th Fr Sa Su
                                    1  2  3  4  5  6                 1  2  3     1  2  3  4  5  6  7
                                 7  8  9 10 11 12 13     4  5  6  7  8  9 10     8  9 10 11 12 13 14
                                14 15 16 17 18 19 20    11 12 13 14 15 16 17    15 16 17 18 19 20 21
                                21 22 23 24 25 26 27    18 19 20 21 22 23 24    22 23 24 25 26 27 28
                                28 29 30 31             25 26 27 28 29 30 31    29 30

                                       October                November                December
                                Mo Tu We Th Fr Sa Su    Mo Tu We Th Fr Sa Su    Mo Tu We Th Fr Sa Su
                                       1  2  3  4  5                    1  2     1  2  3  4  5  6  7
                                 6  7  8  9 10 11 12     3  4  5  6  7  8  9     8  9 10 11 12 13 14
                                13 14 15 16 17 18 19    10 11 12 13 14 15 16    15 16 17 18 19 20 21
                                20 21 22 23 24 25 26    17 18 19 20 21 22 23    22 23 24 25 26 27 28
                                27 28 29 30 31          24 25 26 27 28 29 30    29 30 31


```

--[[User:BugFix|BugFix]] ([[User talk:BugFix|talk]]) 00:40, 15 November 2013 (UTC)


## AWK


```AWK


Works with Gnu awk version 3.1.5 and with BusyBox v1.20.0.git awk
To change the output width, change the value assigned to variable pagewide

#!/bin/gawk -f
BEGIN{
  wkdays = "Su Mo Tu We Th Fr Sa"
  pagewide = 80
  blank=" "
  for (i=1; i<pagewide; i++) blank = blank " "
  #  month name accessed as substr(month,num*10,10)
  #      where num is number of month, 1-12
  month= "           January  February    March     April  "
  month=    month "   May      June       July     August  "
  month=    month " September October   November  December "
  #  table of days per month accessed as substr(days,2*month,2)
  days =" 312831303130313130313031"
  line1 = ""
  line2 = ""
  line3 = ""
  line4 = ""
  line5 = ""
  line6 = ""
  line7 = ""
  line8 = ""
# print " year: " year " starts on: " dow(year)
  }
function center(text,   half) {
  half = (pagewide - length(text))/2
  return substr(blank,1,half) text substr(blank,1,half)
  }
function min(a,b) {
  if (a < b) return a
  else return b
  }
function makewk (fst,lst,day,  i,wstring ){
  wstring=""
  for (i=1;i<day;i++) wstring=wstring "   "
  for (i=fst;i<=lst;i++) wstring=wstring sprintf("%2d ",i)
  return substr(wstring "                     ",1,20)
  }
function dow (year, y){
  y=year
  y= (y*365+int(y/4) - int(y/100) + int(y/400) +1) %7
# leap year adjustment
  leap = 0
  if (year % 4 == 0)   leap = 1
  if (year % 100 == 0) leap = 0
  if (year % 400 == 0) leap = 1
  y = y - leap
  if (y==-1) y=6
  if (y==0) y=7
  return (y)
  }
function prmonth (nmonth, newdow,monsize  ){
  line1 = line1  "     " (substr(month,10*nmonth,10)) "       "
  line2 = line2  (wkdays) "  "
  line3 = line3  (makewk(1,8-newdow,newdow)) "  "
  line4 = line4  (makewk(9-newdow,15-newdow,1)) "  "
  line5 = line5  (makewk(16-newdow,22-newdow,1)) "  "
  line6 = line6  (makewk(23-newdow,29-newdow,1)) "  "
  line7 = line7  (makewk(30-newdow,min(monsize,36-newdow),1)) "  "
  line8 = line8  (makewk(37-newdow,monsize,1)) "  "
if (length(line3) + 22 > pagewide) {
  print center(line1)
  print center(line2)
  print center(line3)
  print center(line4)
  print center(line5)
  print center(line6)
  print center(line7)
  print center(line8)
  line1 = ""
  line2 = ""
  line3 = ""
  line4 = ""
  line5 = ""
  line6 = ""
  line7 = ""
  line8 = ""
  }
}
/q/{
  exit }
{
  monsize=substr(days,2*1,2)
  newdow=dow($1)
  print center("[ picture of Snoopy goes here ]")
  print center(sprintf("%d",$1)  )
  # January - December
  for (i=1; i<13; i++) {
  prmonth(i,newdow,monsize)
  newdow=(monsize+newdow) %7
  if (newdow == 0) newdow = 7
  monsize=substr(days,2+2*i,2)
  if (leap == 1 && monsize == 28) monsize = 29
  }
 }



```

Output:

```txt

                        [ picture of Snoopy goes here ]
                                      1969
              January              February                March
       Su Mo Tu We Th Fr Sa  Su Mo Tu We Th Fr Sa  Su Mo Tu We Th Fr Sa
                 1  2  3  4                     1                     1
        5  6  7  8  9 10 11   2  3  4  5  6  7  8   2  3  4  5  6  7  8
       12 13 14 15 16 17 18   9 10 11 12 13 14 15   9 10 11 12 13 14 15
       19 20 21 22 23 24 25  16 17 18 19 20 21 22  16 17 18 19 20 21 22
       26 27 28 29 30 31     23 24 25 26 27 28     23 24 25 26 27 28 29
                                                   30 31
               April                 May                  June
       Su Mo Tu We Th Fr Sa  Su Mo Tu We Th Fr Sa  Su Mo Tu We Th Fr Sa
              1  2  3  4  5               1  2  3   1  2  3  4  5  6  7
        6  7  8  9 10 11 12   4  5  6  7  8  9 10   8  9 10 11 12 13 14
       13 14 15 16 17 18 19  11 12 13 14 15 16 17  15 16 17 18 19 20 21
       20 21 22 23 24 25 26  18 19 20 21 22 23 24  22 23 24 25 26 27 28
       27 28 29 30           25 26 27 28 29 30 31  29 30

               July                 August               September
       Su Mo Tu We Th Fr Sa  Su Mo Tu We Th Fr Sa  Su Mo Tu We Th Fr Sa
              1  2  3  4  5                  1  2      1  2  3  4  5  6
        6  7  8  9 10 11 12   3  4  5  6  7  8  9   7  8  9 10 11 12 13
       13 14 15 16 17 18 19  10 11 12 13 14 15 16  14 15 16 17 18 19 20
       20 21 22 23 24 25 26  17 18 19 20 21 22 23  21 22 23 24 25 26 27
       27 28 29 30 31        24 25 26 27 28 29 30  28 29 30
                             31
             October               November              December
       Su Mo Tu We Th Fr Sa  Su Mo Tu We Th Fr Sa  Su Mo Tu We Th Fr Sa
                 1  2  3  4                     1      1  2  3  4  5  6
        5  6  7  8  9 10 11   2  3  4  5  6  7  8   7  8  9 10 11 12 13
       12 13 14 15 16 17 18   9 10 11 12 13 14 15  14 15 16 17 18 19 20
       19 20 21 22 23 24 25  16 17 18 19 20 21 22  21 22 23 24 25 26 27
       26 27 28 29 30 31     23 24 25 26 27 28 29  28 29 30 31
                             30


```



## BaCon

Choosing 132 character output.

```freebasic
DECLARE month$[] = { "January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December" }
DECLARE month[] = { 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 }
year$ = "1969"
' Leap year
INCR month[1], IIF(MOD(VAL(year$), 4) = 0 OR MOD(VAL(year$), 100) = 0 AND MOD(VAL(year$), 400) <> 0, 1, 0)
PRINT ALIGN$("[SNOOPY HERE]", 132, 2)
PRINT ALIGN$(year$, 132, 2)
FOR nr = 0 TO 11
    row = 3
    GOTOXY 1+(nr %6)*22, row+(nr/6)*9
    PRINT ALIGN$(month$[nr], 21, 2);
    INCR row
    GOTOXY 1+(nr %6)*22, row+(nr/6)*9
    PRINT ALIGN$("Mo Tu We Th Fr Sa Su", 21, 2);
    INCR row
    ' Each day
    FOR day = 1 TO month[nr]
        ' Zeller
        J = VAL(LEFT$(year$, 2))
        K = VAL(MID$(year$, 3, 2))
        m = nr+1
        IF nr < 2 THEN
            INCR m, 12
            DECR K
        END IF
        h = (day + ((m+1)*26)/10 + K + (K/4) + (J/4) + 5*J)
        daynr = MOD(h, 7) - 2
        IF daynr < 0 THEN INCR daynr, 7
        IF daynr = 0 AND day > 1 THEN INCR row
        GOTOXY 1+(nr %6)*22+daynr*3, row+(nr/6)*9
        PRINT day;
    NEXT
NEXT
```


{{out}}

```txt
                                                           [SNOOPY HERE]
                                                                1969
       January              February                March                 April                  May                  June
Mo Tu We Th Fr Sa Su  Mo Tu We Th Fr Sa Su  Mo Tu We Th Fr Sa Su  Mo Tu We Th Fr Sa Su  Mo Tu We Th Fr Sa Su  Mo Tu We Th Fr Sa Su
      1  2  3  4  5                  1  2                  1  2      1  2  3  4  5  6            1  2  3  4                     1
6  7  8  9  10 11 12  3  4  5  6  7  8  9   3  4  5  6  7  8  9   7  8  9  10 11 12 13  5  6  7  8  9  10 11  2  3  4  5  6  7  8
13 14 15 16 17 18 19  10 11 12 13 14 15 16  10 11 12 13 14 15 16  14 15 16 17 18 19 20  12 13 14 15 16 17 18  9  10 11 12 13 14 15
20 21 22 23 24 25 26  17 18 19 20 21 22 23  17 18 19 20 21 22 23  21 22 23 24 25 26 27  19 20 21 22 23 24 25  16 17 18 19 20 21 22
27 28 29 30 31        24 25 26 27 28        24 25 26 27 28 29 30  28 29 30              26 27 28 29 30 31     23 24 25 26 27 28 29
                                            31                                                                30

        July                 August               September              October              November              December
Mo Tu We Th Fr Sa Su  Mo Tu We Th Fr Sa Su  Mo Tu We Th Fr Sa Su  Mo Tu We Th Fr Sa Su  Mo Tu We Th Fr Sa Su  Mo Tu We Th Fr Sa Su
   1  2  3  4  5  6               1  2  3   1  2  3  4  5  6  7         1  2  3  4  5                  1  2   1  2  3  4  5  6  7
7  8  9  10 11 12 13  4  5  6  7  8  9  10  8  9  10 11 12 13 14  6  7  8  9  10 11 12  3  4  5  6  7  8  9   8  9  10 11 12 13 14
14 15 16 17 18 19 20  11 12 13 14 15 16 17  15 16 17 18 19 20 21  13 14 15 16 17 18 19  10 11 12 13 14 15 16  15 16 17 18 19 20 21
21 22 23 24 25 26 27  18 19 20 21 22 23 24  22 23 24 25 26 27 28  20 21 22 23 24 25 26  17 18 19 20 21 22 23  22 23 24 25 26 27 28
28 29 30 31           25 26 27 28 29 30 31  29 30                 27 28 29 30 31        24 25 26 27 28 29 30  29 30 31

```



## Batch File


```dos
::Calender Task from Rosetta Code Wiki
::Batch File Implementation

@echo off
setlocal enabledelayedexpansion

	%== Set a valid year [will not be validated] ==%
set y=1969

	%== Set the variables for months (feb_l=the normal 28 days) ==%
set jan_l=31&set apr_l=30
set mar_l=31&set jun_l=30
set may_l=31&set sep_l=30
set jul_l=31&set nov_l=30
set aug_l=31&set feb_l=28
set oct_l=31
set dec_l=31

	%== Compute day for first day of the year ==%
set /a d=(y/4+y)-(y/100-y/400)

	%== Check if that year is a leap year ==%
set /a "op1=y%%4","op2=y%%100","op3=y%%400"
if not "%op1%"=="0" (goto :no_leap)
if not "%op2%"=="0" (goto :yes_leap)
if not "%op3%"=="0" (goto :no_leap)
	:yes_leap
		%== Ooops... Leap year. Change feb_l to 29. ==%
		set feb_l=29
		set/a d-=1
	:no_leap

	%== Compute weekday of the first day... ==%
set /a d%%=7

	%== Generate everything that's inside the calendar ==%
for %%a in (jan feb mar apr may jun jul aug sep oct nov dec) do (
	set %%a=
	set chars_added=0
	for /l %%b in (1,1,!d!) do (set "%%a=!%%a!   "&set /a chars_added+=3)
	for /l %%c in (1,1,!%%a_l!) do (
		if %%c lss 10 (set "%%a=!%%a! %%c ") else (set "%%a=!%%a!%%c ")
		set /a chars_added+=3
	)
	for /l %%d in (!chars_added!,1,124) do set "%%a=!%%a! "
	set /a d=^(d+%%a_l^)%%7
)

	%== Display the calendar ==%
cls
echo.
echo.                              [SNOOPY]
echo.
echo. YEAR = %y%
echo.
echo.       January                February                March
echo. Su Mo Tu We Th Fr Sa   Su Mo Tu We Th Fr Sa   Su Mo Tu We Th Fr Sa
echo. %jan:~0,20%   %feb:~0,20%   %mar:~0,20%
echo. %jan:~21,20%   %feb:~21,20%   %mar:~21,20%
echo. %jan:~42,20%   %feb:~42,20%   %mar:~42,20%
echo. %jan:~63,20%   %feb:~63,20%   %mar:~63,20%
echo. %jan:~84,20%   %feb:~84,20%   %mar:~84,20%
echo. %jan:~105%   %feb:~105%   %mar:~105%
echo.
echo.        April                   May                    June
echo. Su Mo Tu We Th Fr Sa   Su Mo Tu We Th Fr Sa   Su Mo Tu We Th Fr Sa
echo. %apr:~0,20%   %may:~0,20%   %jun:~0,20%
echo. %apr:~21,20%   %may:~21,20%   %jun:~21,20%
echo. %apr:~42,20%   %may:~42,20%   %jun:~42,20%
echo. %apr:~63,20%   %may:~63,20%   %jun:~63,20%
echo. %apr:~84,20%   %may:~84,20%   %jun:~84,20%
echo. %apr:~105%   %may:~105%   %jun:~105%
echo.
echo.         July                  August                September
echo. Su Mo Tu We Th Fr Sa   Su Mo Tu We Th Fr Sa   Su Mo Tu We Th Fr Sa
echo. %jul:~0,20%   %aug:~0,20%   %sep:~0,20%
echo. %jul:~21,20%   %aug:~21,20%   %sep:~21,20%
echo. %jul:~42,20%   %aug:~42,20%   %sep:~42,20%
echo. %jul:~63,20%   %aug:~63,20%   %sep:~63,20%
echo. %jul:~84,20%   %aug:~84,20%   %sep:~84,20%
echo. %jul:~105%   %aug:~105%   %sep:~105%
echo.
echo.       October                November               December
echo. Su Mo Tu We Th Fr Sa   Su Mo Tu We Th Fr Sa   Su Mo Tu We Th Fr Sa
echo. %oct:~0,20%   %nov:~0,20%   %dec:~0,20%
echo. %oct:~21,20%   %nov:~21,20%   %dec:~21,20%
echo. %oct:~42,20%   %nov:~42,20%   %dec:~42,20%
echo. %oct:~63,20%   %nov:~63,20%   %dec:~63,20%
echo. %oct:~84,20%   %nov:~84,20%   %dec:~84,20%
echo. %oct:~105%   %nov:~105%   %dec:~105%
echo.
pause
endlocal
```

{{Out}}

```txt


                              [SNOOPY]

 YEAR = 1969

       January                February                March
 Su Mo Tu We Th Fr Sa   Su Mo Tu We Th Fr Sa   Su Mo Tu We Th Fr Sa
           1  2  3  4                      1                      1
  5  6  7  8  9 10 11    2  3  4  5  6  7  8    2  3  4  5  6  7  8
 12 13 14 15 16 17 18    9 10 11 12 13 14 15    9 10 11 12 13 14 15
 19 20 21 22 23 24 25   16 17 18 19 20 21 22   16 17 18 19 20 21 22
 26 27 28 29 30 31      23 24 25 26 27 28      23 24 25 26 27 28 29
                                               30 31

        April                   May                    June
 Su Mo Tu We Th Fr Sa   Su Mo Tu We Th Fr Sa   Su Mo Tu We Th Fr Sa
        1  2  3  4  5                1  2  3    1  2  3  4  5  6  7
  6  7  8  9 10 11 12    4  5  6  7  8  9 10    8  9 10 11 12 13 14
 13 14 15 16 17 18 19   11 12 13 14 15 16 17   15 16 17 18 19 20 21
 20 21 22 23 24 25 26   18 19 20 21 22 23 24   22 23 24 25 26 27 28
 27 28 29 30            25 26 27 28 29 30 31   29 30


         July                  August                September
 Su Mo Tu We Th Fr Sa   Su Mo Tu We Th Fr Sa   Su Mo Tu We Th Fr Sa
        1  2  3  4  5                   1  2       1  2  3  4  5  6
  6  7  8  9 10 11 12    3  4  5  6  7  8  9    7  8  9 10 11 12 13
 13 14 15 16 17 18 19   10 11 12 13 14 15 16   14 15 16 17 18 19 20
 20 21 22 23 24 25 26   17 18 19 20 21 22 23   21 22 23 24 25 26 27
 27 28 29 30 31         24 25 26 27 28 29 30   28 29 30
                        31

       October                November               December
 Su Mo Tu We Th Fr Sa   Su Mo Tu We Th Fr Sa   Su Mo Tu We Th Fr Sa
           1  2  3  4                      1       1  2  3  4  5  6
  5  6  7  8  9 10 11    2  3  4  5  6  7  8    7  8  9 10 11 12 13
 12 13 14 15 16 17 18    9 10 11 12 13 14 15   14 15 16 17 18 19 20
 19 20 21 22 23 24 25   16 17 18 19 20 21 22   21 22 23 24 25 26 27
 26 27 28 29 30 31      23 24 25 26 27 28 29   28 29 30 31
                        30

Press any key to continue . . .
```



## BBC BASIC

{{works with|BBC BASIC for Windows}}
The day and month names are in the language for which the PC is configured.

```bbcbasic
      INSTALL @lib$+"DATELIB"
      VDU 23,22,640;570;8,15,16,128

      year% = 1969
      PRINT TAB(38); year%
      DIM dom%(2), mjd%(2), dim%(2)

      FOR day% = 1 TO 7
        days$ += LEFT$(FN_date$(FN_mjd(day%, 1, 1905), "ddd"), 2) + " "
      NEXT

      FOR month% = 1 TO 10 STEP 3
        PRINT
        FOR col% = 0 TO 2
          mjd%(col%) = FN_mjd(1, month% + col%, year%)
          month$ = FN_date$(mjd%(col%), "MMMM")
          PRINT TAB(col%*24 + 16 - LEN(month$)/2) month$;
        NEXT
        FOR col% = 0 TO 2
          PRINT TAB(col%*24 + 6) days$;
          dim%(col%) = FN_dim(month% + col%, year%)
        NEXT
        dom%() = 1
        col% = 0
        REPEAT
          dow% = FN_dow(mjd%(col%))
          IF dom%(col%)<=dim%(col%) THEN
            PRINT TAB(col%*24 + dow%*3 + 6); dom%(col%);
            dom%(col%) += 1
            mjd%(col%) += 1
          ENDIF
          IF dow%=6 OR dom%(col%)>dim%(col%) col% = (col% + 1) MOD 3
        UNTIL dom%(0)>dim%(0) AND dom%(1)>dim%(1) AND dom%(2)>dim%(2)
        PRINT
      NEXT

```

Output:

```txt
                                      1969

            January                 February                 March
      Su Mo Tu We Th Fr Sa    Su Mo Tu We Th Fr Sa    Su Mo Tu We Th Fr Sa
               1  2  3  4                       1                       1
      5  6  7  8  9  10 11    2  3  4  5  6  7  8     2  3  4  5  6  7  8
      12 13 14 15 16 17 18    9  10 11 12 13 14 15    9  10 11 12 13 14 15
      19 20 21 22 23 24 25    16 17 18 19 20 21 22    16 17 18 19 20 21 22
      26 27 28 29 30 31       23 24 25 26 27 28       23 24 25 26 27 28 29
                                                      30 31

             April                    May                     June
      Su Mo Tu We Th Fr Sa    Su Mo Tu We Th Fr Sa    Su Mo Tu We Th Fr Sa
            1  2  3  4  5                 1  2  3     1  2  3  4  5  6  7
      6  7  8  9  10 11 12    4  5  6  7  8  9  10    8  9  10 11 12 13 14
      13 14 15 16 17 18 19    11 12 13 14 15 16 17    15 16 17 18 19 20 21
      20 21 22 23 24 25 26    18 19 20 21 22 23 24    22 23 24 25 26 27 28
      27 28 29 30             25 26 27 28 29 30 31    29 30

              July                   August                September
      Su Mo Tu We Th Fr Sa    Su Mo Tu We Th Fr Sa    Su Mo Tu We Th Fr Sa
            1  2  3  4  5                    1  2        1  2  3  4  5  6
      6  7  8  9  10 11 12    3  4  5  6  7  8  9     7  8  9  10 11 12 13
      13 14 15 16 17 18 19    10 11 12 13 14 15 16    14 15 16 17 18 19 20
      20 21 22 23 24 25 26    17 18 19 20 21 22 23    21 22 23 24 25 26 27
      27 28 29 30 31          24 25 26 27 28 29 30    28 29 30
                              31

            October                 November                December
      Su Mo Tu We Th Fr Sa    Su Mo Tu We Th Fr Sa    Su Mo Tu We Th Fr Sa
               1  2  3  4                       1        1  2  3  4  5  6
      5  6  7  8  9  10 11    2  3  4  5  6  7  8     7  8  9  10 11 12 13
      12 13 14 15 16 17 18    9  10 11 12 13 14 15    14 15 16 17 18 19 20
      19 20 21 22 23 24 25    16 17 18 19 20 21 22    21 22 23 24 25 26 27
      26 27 28 29 30 31       23 24 25 26 27 28 29    28 29 30 31
                              30
```



## Befunge

This is quite closely based on the [[Calendar#C|C]] sample, although the centering calculation has been adjusted to fix an off-by-one error, and the months have been made a constant height, regardless of how much space they require, to try and produce a more balanced layout.

The year is read from stdin, and the width is specified by the first value on the stack (set to 80 - <tt>"P"</tt> - in the current implementation).


```befunge
"P"00p&>:::4%!\"d"%*\45*:*%!+!!65*+31p:1-:::"I"5**\4/+\"d"/-\45*:*/+1+7%:0v
J!F?M!A M!J J!A!S O!N D!SaFrThWeTuMoSuvp01:_1#!-#%:#\>#+6<v-2g1+1g01p1p01:<
 January  February  March    April    >:45**00g\-\1-:v:<<6>+7%:10g2+:38*\`|
   May      June     July    August v02-1:+4*-4\`\4:/_$:^^:/*2+92+2:g00$$$<
September October  November December>p:45*+10g*\--2/00g4-2/>:#,1#*-#8\#4_$v
>20g>:#,1#*-#8\#4_$6"$S" v. .vp040\$_4#!8#\*#-,#1>#:<+5g03g01:,,:+55<0.p03<
^_v#:-1$_v#!:,*84,g1+1,< < ^ >::4%9*40g:8`!#v_$$$1+v^+g02_#v$#,$#+5<^_v#`\<
-#8\#4_$7>1-:2*64*+:1g>^ ^   ^,g+2/4\+p04+1:<>1#\-#<:66+\^ >>$10g30g>:#,1#*
> > $$55+,6>>40p:10g30g>:#,1#*-#8\#4_$>\:2*:1+1g2-50p1g640g-7*1+\-7v  v@,<6
2:+g01$_55+,^ > > > > #^>#g>#0>#2_v v*2!!\%+55:\/+55:**`0\!`g05:::\<  >2-^^
->:> >#^>#<>#<^#!:-1g04$$ < < < < < >4+8*+\:!!2*4+8*+,,48*,1+\1-:>#^_$$1+\1
```



## C

With arbitrary display width (>= 20 though) and auto spacing.

```c
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int width = 80, year = 1969;
int cols, lead, gap;

const char *wdays[] = { "Su", "Mo", "Tu", "We", "Th", "Fr", "Sa" };
struct months {
	const char *name;
	int days, start_wday, at;
} months[12] = {
	{ "January",	31, 0, 0 },
	{ "February",	28, 0, 0 },
	{ "March",	31, 0, 0 },
	{ "April",	30, 0, 0 },
	{ "May",	31, 0, 0 },
	{ "June",	30, 0, 0 },
	{ "July",	31, 0, 0 },
	{ "August",	31, 0, 0 },
	{ "September",	30, 0, 0 },
	{ "October",	31, 0, 0 },
	{ "November",	30, 0, 0 },
	{ "December",	31, 0, 0 }
};

void space(int n) { while (n-- > 0) putchar(' '); }

void init_months()
{
	int i;

	if ((!(year % 4) && (year % 100)) || !(year % 400))
		months[1].days = 29;

	year--;
	months[0].start_wday
		= (year * 365 + year/4 - year/100 + year/400 + 1) % 7;

	for (i = 1; i < 12; i++)
		months[i].start_wday =
			(months[i-1].start_wday + months[i-1].days) % 7;

	cols = (width + 2) / 22;
	while (12 % cols) cols--;
	gap = cols - 1 ? (width - 20 * cols) / (cols - 1) : 0;
	if (gap > 4) gap = 4;
	lead = (width - (20 + gap) * cols + gap + 1) / 2;
        year++;
}

void print_row(int row)
{
	int c, i, from = row * cols, to = from + cols;
	space(lead);
	for (c = from; c < to; c++) {
		i = strlen(months[c].name);
		space((20 - i)/2);
		printf("%s", months[c].name);
		space(20 - i - (20 - i)/2 + ((c == to - 1) ? 0 : gap));
	}
	putchar('\n');

	space(lead);
	for (c = from; c < to; c++) {
		for (i = 0; i < 7; i++)
			printf("%s%s", wdays[i], i == 6 ? "" : " ");
		if (c < to - 1) space(gap);
		else putchar('\n');
	}

	while (1) {
		for (c = from; c < to; c++)
			if (months[c].at < months[c].days) break;
		if (c == to) break;

		space(lead);
		for (c = from; c < to; c++) {
			for (i = 0; i < months[c].start_wday; i++) space(3);
			while(i++ < 7 && months[c].at < months[c].days) {
				printf("%2d", ++months[c].at);
				if (i < 7 || c < to - 1) putchar(' ');
			}
			while (i++ <= 7 && c < to - 1) space(3);
			if (c < to - 1) space(gap - 1);
			months[c].start_wday = 0;
		}
		putchar('\n');
	}
	putchar('\n');
}

void print_year()
{
	int row;
	char buf[32];
	sprintf(buf, "%d", year);
	space((width - strlen(buf)) / 2);
	printf("%s\n\n", buf);
	for (row = 0; row * cols < 12; row++)
		print_row(row);
}

int main(int c, char **v)
{
	int i, year_set = 0;
	for (i = 1; i < c; i++) {
		if (!strcmp(v[i], "-w")) {
			if (++i == c || (width = atoi(v[i])) < 20)
				goto bail;
		} else if (!year_set) {
			if (!sscanf(v[i], "%d", &year) || year <= 0)
				year = 1969;
			year_set = 1;
		} else
			goto bail;
	}

	init_months();
	print_year();
	return 0;

bail:	fprintf(stderr, "bad args\nUsage: %s year [-w width (>= 20)]\n", v[0]);
	exit(1);
}
```



## C++


```cpp

#include <windows.h>
#include <iostream>

//--------------------------------------------------------------------------------------------------
using namespace std;


//--------------------------------------------------------------------------------------------------
class calender
{
public:
    void drawCalender( int y )
    {
	year = y;
	for( int i = 0; i < 12; i++ )
	    firstdays[i] = getfirstday( i );

	isleapyear();
	build();
    }

private:
    void isleapyear()
    {
	isleap = false;

	if( !( year % 4 ) )
	{
	    if( year % 100 ) isleap = true;
	    else if( !( year % 400 ) ) isleap = true;
	}
    }

    int getfirstday( int m )
    {
	int y = year;

	int f = y + 1 + 3 * m - 1;
	m++;
	if( m < 3 ) y--;
	else f -= int( .4 * m + 2.3 );

	f += int( y / 4 ) - int( ( y / 100 + 1 ) * 0.75 );
	f %= 7;

	return f;
    }

    void build()
    {
	int days[] = { 31, isleap ? 29 : 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 };
	int lc = 0, lco = 0, ystr = 7, start = 2, fd = 0, m = 0;
	HANDLE h = GetStdHandle( STD_OUTPUT_HANDLE );
	COORD pos = { 0, ystr };
	draw();

	for( int i = 0; i < 4; i++ )
	{
	    for( int j = 0; j < 3; j++ )
	    {
		int d = firstdays[fd++], dm = days[m++];
		pos.X = d * 3 + start;
		SetConsoleCursorPosition( h, pos );

		for( int dd = 0; dd < dm; dd++ )
		{
		    if( dd < 9 ) cout << 0 << dd + 1 << " ";
		    else cout << dd + 1 << " ";

		    pos.X += 3;
		    if( pos.X - start > 20 )
		    {
			pos.X = start; pos.Y++;
			SetConsoleCursorPosition( h, pos );
		    }
		}

		start += 23;
		pos.X = start; pos.Y = ystr;
		SetConsoleCursorPosition( h, pos );
	    }
	    ystr += 9; start = 2;
	    pos.Y = ystr;
	}
    }

    void draw()
    {
	system( "cls" );
	cout << "+--------------------------------------------------------------------+" << endl;
	cout << "|                              [SNOOPY]                              |" << endl;
	cout << "|                                                                    |" << endl;
	cout << "|                             == " << year << " ==                             |" << endl;
	cout << "+----------------------+----------------------+----------------------+" << endl;
	cout << "|       JANUARY        |       FEBRUARY       |         MARCH        |" << endl;
	cout << "| SU MO TU WE TH FR SA | SU MO TU WE TH FR SA | SU MO TU WE TH FR SA |" << endl;
	cout << "|                      |                      |                      |" << endl;
	cout << "|                      |                      |                      |" << endl;
	cout << "|                      |                      |                      |" << endl;
	cout << "|                      |                      |                      |" << endl;
	cout << "|                      |                      |                      |" << endl;
	cout << "|                      |                      |                      |" << endl;
	cout << "+----------------------+----------------------+----------------------+" << endl;
	cout << "|        APRIL         |          MAY         |         JUNE         |" << endl;
	cout << "| SU MO TU WE TH FR SA | SU MO TU WE TH FR SA | SU MO TU WE TH FR SA |" << endl;
	cout << "|                      |                      |                      |" << endl;
	cout << "|                      |                      |                      |" << endl;
	cout << "|                      |                      |                      |" << endl;
	cout << "|                      |                      |                      |" << endl;
	cout << "|                      |                      |                      |" << endl;
	cout << "|                      |                      |                      |" << endl;
	cout << "+----------------------+----------------------+----------------------+" << endl;
	cout << "|         JULY         |        AUGUST        |       SEPTEMBER      |" << endl;
	cout << "| SU MO TU WE TH FR SA | SU MO TU WE TH FR SA | SU MO TU WE TH FR SA |" << endl;
	cout << "|                      |                      |                      |" << endl;
	cout << "|                      |                      |                      |" << endl;
	cout << "|                      |                      |                      |" << endl;
	cout << "|                      |                      |                      |" << endl;
	cout << "|                      |                      |                      |" << endl;
	cout << "|                      |                      |                      |" << endl;
	cout << "+----------------------+----------------------+----------------------+" << endl;
	cout << "|        OCTOBER       |       NOVEMBER       |       DECEMBER       |" << endl;
	cout << "| SU MO TU WE TH FR SA | SU MO TU WE TH FR SA | SU MO TU WE TH FR SA |" << endl;
	cout << "|                      |                      |                      |" << endl;
	cout << "|                      |                      |                      |" << endl;
	cout << "|                      |                      |                      |" << endl;
	cout << "|                      |                      |                      |" << endl;
	cout << "|                      |                      |                      |" << endl;
	cout << "|                      |                      |                      |" << endl;
	cout << "+----------------------+----------------------+----------------------+" << endl;
    }

    int firstdays[12], year;
    bool isleap;
};
//--------------------------------------------------------------------------------------------------
int main( int argc, char* argv[] )
{
    int y;
    calender cal;

    while( true )
    {
	system( "cls" );
	cout << "Enter the year( yyyy ) --- ( 0 to quit ): ";
	cin >> y;
	if( !y ) return 0;

	cal.drawCalender( y );
	cout << endl << endl << endl << endl << endl << endl << endl << endl;

	system( "pause" );
    }
    return 0;
}
//--------------------------------------------------------------------------------------------------

```

Output:

```txt

+--------------------------------------------------------------------+
|                              [SNOOPY]                              |
|                                                                    |
|                             == 1969 ==                             |
+----------------------+----------------------+----------------------+
|       JANUARY        |       FEBRUARY       |         MARCH        |
| SU MO TU WE TH FR SA | SU MO TU WE TH FR SA | SU MO TU WE TH FR SA |
|          01 02 03 04 |                   01 |                   01 |
| 05 06 07 08 09 10 11 | 02 03 04 05 06 07 08 | 02 03 04 05 06 07 08 |
| 12 13 14 15 16 17 18 | 09 10 11 12 13 14 15 | 09 10 11 12 13 14 15 |
| 19 20 21 22 23 24 25 | 16 17 18 19 20 21 22 | 16 17 18 19 20 21 22 |
| 26 27 28 29 30 31    | 23 24 25 26 27 28    | 23 24 25 26 27 28 29 |
|                      |                      | 30 31                |
+----------------------+----------------------+----------------------+
|        APRIL         |          MAY         |         JUNE         |
| SU MO TU WE TH FR SA | SU MO TU WE TH FR SA | SU MO TU WE TH FR SA |
|       01 02 03 04 05 |             01 02 03 | 01 02 03 04 05 06 07 |
| 06 07 08 09 10 11 12 | 04 05 06 07 08 09 10 | 08 09 10 11 12 13 14 |
| 13 14 15 16 17 18 19 | 11 12 13 14 15 16 17 | 15 16 17 18 19 20 21 |
| 20 21 22 23 24 25 26 | 18 19 20 21 22 23 24 | 22 23 24 25 26 27 28 |
| 27 28 29 30          | 25 26 27 28 29 30 31 | 29 30                |
|                      |                      |                      |
+----------------------+----------------------+----------------------+
|         JULY         |        AUGUST        |       SEPTEMBER      |
| SU MO TU WE TH FR SA | SU MO TU WE TH FR SA | SU MO TU WE TH FR SA |
|       01 02 03 04 05 |                01 02 |    01 02 03 04 05 06 |
| 06 07 08 09 10 11 12 | 03 04 05 06 07 08 09 | 07 08 09 10 11 12 13 |
| 13 14 15 16 17 18 19 | 10 11 12 13 14 15 16 | 14 15 16 17 18 19 20 |
| 20 21 22 23 24 25 26 | 17 18 19 20 21 22 23 | 21 22 23 24 25 26 27 |
| 27 28 29 30 31       | 24 25 26 27 28 29 30 | 28 29 30             |
|                      | 31                   |                      |
+----------------------+----------------------+----------------------+
|        OCTOBER       |       NOVEMBER       |       DECEMBER       |
| SU MO TU WE TH FR SA | SU MO TU WE TH FR SA | SU MO TU WE TH FR SA |
|          01 02 03 04 |                   01 |    01 02 03 04 05 06 |
| 05 06 07 08 09 10 11 | 02 03 04 05 06 07 08 | 07 08 09 10 11 12 13 |
| 12 13 14 15 16 17 18 | 09 10 11 12 13 14 15 | 14 15 16 17 18 19 20 |
| 19 20 21 22 23 24 25 | 16 17 18 19 20 21 22 | 21 22 23 24 25 26 27 |
| 26 27 28 29 30 31    | 23 24 25 26 27 28 29 | 28 29 30 31          |
|                      | 30                   |                      |
+----------------------+----------------------+----------------------+

```


=={{header|C sharp|C#}}==
An attempt to abuse the DateTime class for all static information. In the event that the number of days and months changes, so long as the DateTime class is updated accordingly, this should still print properly. It also abuses iterators to allow for a concise month printing method, but with the ability to still print x months per line.


```csharp


using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace CalendarStuff
{

    class Program
    {
        static void Main(string[] args)
        {
            Console.WindowHeight = 46;
            Console.Write(buildMonths(new DateTime(1969, 1, 1)));
            Console.Read();
        }
        private static string buildMonths(DateTime date)
        {
            StringBuilder sb = new StringBuilder();
            sb.AppendLine(center("[Snoop]", 24 * 3));
            sb.AppendLine();
            sb.AppendLine(center(date.Year.ToString(), 24 * 3));

            List<DateTime> dts = new List<DateTime>();
            while (true)
            {
                dts.Add(date);
                if (date.Year != ((date = date.AddMonths(1)).Year))
                {
                    break;
                }
            }
            var jd = dts.Select(a => buildMonth(a).GetEnumerator()).ToArray();

            int sCur=0;
            while (sCur<dts.Count)
            {
                sb.AppendLine();
                int curMonth=0;
                var j = jd.Where(a => curMonth++ >= sCur && curMonth - 1 < sCur + 3).ToArray(); //grab the next 3
                sCur += j.Length;
                bool breakOut = false;
                while (!breakOut)
                {
                    int inj = 1;
                    foreach (var cd in j)
                    {
                        if (cd.MoveNext())
                        {
                            sb.Append((cd.Current.Length == 21 ? cd.Current : cd.Current.PadRight(21, ' ')) + "     ");
                        }
                        else
                        {
                            sb.Append("".PadRight(21, ' ') + "     ");
                            breakOut = true;
                        }
                        if (inj++ % 3 == 0) sb.AppendLine();
                    }
                }

            }
            return sb.ToString();
        }


        private static IEnumerable<string> buildMonth(DateTime date)
        {
            yield return center(date.ToString("MMMM"),7*3);
            var j = DateTime.DaysInMonth(date.Year, date.Month);
            yield return Enum.GetNames(typeof(DayOfWeek)).Aggregate("", (current, result) => current + (result.Substring(0, 2).ToUpper() + " "));
            string cur = "";
            int total = 0;

            foreach (var day in Enumerable.Range(-((int)date.DayOfWeek),j + (int)date.DayOfWeek))
            {
                cur += (day < 0 ? "  " : ((day < 9 ? " " : "") + (day + 1))) +" ";
                if (total++ > 0 && (total ) % 7 == 0)
                {
                    yield return cur;
                    cur = "";
                }
            }
            yield return cur;
        }
        private static string center(string s, int len)
        {
            return (s.PadLeft((len - s.Length) / 2 + s.Length, ' ').PadRight((len), ' '));
        }
    }
}


```



## COBOL

the program calls subroutine DATE2DOW to convert any YYYY-MM-DD to Day of Week (1=Sunday).  the group names WS-CFGN
and WS-CFGW may be moved to WS-CFG to use narrow or wide print line size respectively.

```COBOL

       IDENTIFICATION DIVISION.
       PROGRAM-ID.  CALEND.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       DATA DIVISION.

       WORKING-STORAGE SECTION.
       01  WS-DAY-NAMES-DEF.
         03 FILLER PIC X(09) VALUE 'SUNDAY   '.
         03 FILLER PIC X(09) VALUE 'MONDAY   '.
         03 FILLER PIC X(09) VALUE 'TUESDAY  '.
         03 FILLER PIC X(09) VALUE 'WEDNESDAY'.
         03 FILLER PIC X(09) VALUE 'THURSDAY '.
         03 FILLER PIC X(09) VALUE 'FRIDAY   '.
         03 FILLER PIC X(09) VALUE 'SATURDAY '.
       01  FILLER REDEFINES WS-DAY-NAMES-DEF.
         03  WS-DAY-NAME       PIC X(09) OCCURS 07 TIMES.

       01  WS-MTH-INFO-DEF.
         03 FILLER PIC X(11) VALUE 'JANUARY  31'.
         03 FILLER PIC X(11) VALUE 'FEBRUARY 28'.
         03 FILLER PIC X(11) VALUE 'MARCH    31'.
         03 FILLER PIC X(11) VALUE 'APRIL    30'.
         03 FILLER PIC X(11) VALUE 'MAY      31'.
         03 FILLER PIC X(11) VALUE 'JUNE     30'.
         03 FILLER PIC X(11) VALUE 'JULY     31'.
         03 FILLER PIC X(11) VALUE 'AUGUST   31'.
         03 FILLER PIC X(11) VALUE 'SEPTEMBER30'.
         03 FILLER PIC X(11) VALUE 'OCTOBER  31'.
         03 FILLER PIC X(11) VALUE 'NOVEMBER 30'.
         03 FILLER PIC X(11) VALUE 'DECEMBER 31'.
       01  FILLER REDEFINES WS-MTH-INFO-DEF.
         03  WS-MTH-INFO-TABLE OCCURS 12 TIMES.
           05  WS-MTH-INFO-NAME   PIC X(09).
           05  WS-MTH-INFO-DAYS   PIC 9(02).

       01  WS-MTH-AREA.
         03  WS-MTH-DD         PIC S99.
         03  WS-DAY1           PIC   9.
         03  WS-DAYS           PIC  99.
         03  WS-DD             PIC   9.
         03  WS-WK             PIC   9.
         03  WS-MM             PIC  99.
         03  WS-QQ             PIC  99.

         03  WS-MTH-MONTH  OCCURS 12 TIMES.
           05  WS-MTH-WEEK OCCURS 6 TIMES.
             07  WS-DAY-FLD      OCCURS 7 TIMES.
               09  WS-DAY        PIC ZZ.
       01  INPDATE-RECORD.
           05  INPD-YEAR          PIC 9(04).
           05  FILLER             PIC X(01).
           05  INPD-MONTH         PIC 9(02).
           05  FILLER             PIC X(01).
           05  INPD-DAY           PIC 9(02).
       01  WMS-DOW                PIC 9(01).
       01  WS-PRT                 PIC X(132).
       01  WS-COL                 PIC  9(03) VALUE 0.
       01  WS-PP                  PIC  9(03) VALUE 0.
       01  WS-CFGN.
         03  FILLER               PIC  9(03) VALUE  80.
         03  FILLER               PIC  9(02) VALUE  5.
         03  FILLER               PIC  9(01) VALUE  1.
         03  FILLER               PIC  9(02) VALUE  5.
         03  FILLER               PIC  9(01) VALUE  2.
       01  WS-CFGW.
         03  FILLER               PIC  9(03) VALUE 120.
         03  FILLER               PIC  9(02) VALUE 10.
         03  FILLER               PIC  9(01) VALUE  2.
         03  FILLER               PIC  9(02) VALUE 10.
         03  FILLER               PIC  9(01) VALUE  3.
       01  WS-CFG.
         03  WS-LS                PIC  9(03) VALUE 120.
         03  WS-LMAR              PIC  9(02) VALUE 10.
         03  WS-SPBD              PIC  9(01) VALUE  2.
         03  WS-SPBC              PIC  9(02) VALUE 10.
         03  WS-DNMW              PIC  9(01) VALUE  3.
       PROCEDURE DIVISION.
           MOVE '1969-01-01' TO INPDATE-RECORD
           MOVE WS-CFGN   TO WS-CFG
           IF  (FUNCTION MOD ( INPD-YEAR , 400 ) = 0
           OR  (FUNCTION MOD ( INPD-YEAR , 4   ) = 0
               AND
               FUNCTION MOD ( INPD-YEAR , 100 ) NOT = 0))
             MOVE 29         TO WS-MTH-INFO-DAYS (02)
           ELSE
             MOVE 28         TO WS-MTH-INFO-DAYS (02)
           END-IF

           PERFORM VARYING WS-MM FROM 1 BY +1
           UNTIL WS-MM > 12
           MOVE WS-MM TO INPD-MONTH
           CALL 'DATE2DOW' USING INPDATE-RECORD, WMS-DOW
           COMPUTE WS-MTH-DD = 1 - WMS-DOW
           COMPUTE WS-DAYS = WS-MTH-INFO-DAYS (INPD-MONTH)
           PERFORM VARYING WS-WK FROM 1 BY +1
           UNTIL WS-WK > 6
             PERFORM VARYING WS-DD FROM 1 BY +1
             UNTIL WS-DD > 7
               COMPUTE WS-MTH-DD = WS-MTH-DD + 1
               IF (WS-MTH-DD < 1)
               OR (WS-MTH-DD > WS-DAYS)
                 MOVE 0         TO WS-DAY (WS-MM, WS-WK, WS-DD)
               ELSE
                 MOVE WS-MTH-DD TO WS-DAY (WS-MM, WS-WK, WS-DD)
               END-IF
             END-PERFORM
           END-PERFORM
           END-PERFORM

           COMPUTE WS-MM = 0
           PERFORM VARYING WS-QQ FROM 1 BY +1
           UNTIL WS-QQ > 4

             INITIALIZE WS-PRT
             COMPUTE WS-PP = 1
             PERFORM VARYING WS-COL FROM 1 BY +1
             UNTIL WS-COL > 3
             COMPUTE WS-MM = 3 * (WS-QQ - 1) + WS-COL

               IF WS-COL = 1
                 COMPUTE WS-PP = WS-PP + WS-LMAR + 2 - WS-DNMW
               ELSE
                 COMPUTE WS-PP = WS-PP + WS-SPBC + 2 - WS-DNMW
               END-IF
               MOVE WS-MTH-INFO-NAME (WS-MM)
                             TO WS-PRT(WS-PP:9)
               COMPUTE WS-PP
               =       WS-PP + ( 2 * 7 + WS-SPBD * 6 + WS-SPBD - 1)
               -       4
               MOVE INPD-YEAR TO WS-PRT (WS-PP:4)
               COMPUTE WS-PP = WS-PP + 4
             END-PERFORM
             DISPLAY WS-PRT (1:WS-LS)

             INITIALIZE WS-PRT
             COMPUTE WS-PP = 1
             PERFORM VARYING WS-COL FROM 1 BY +1
             UNTIL WS-COL > 3
             COMPUTE WS-MM = 3 * (WS-QQ - 1) + WS-COL

               IF WS-COL = 1
                 COMPUTE WS-PP = WS-PP + WS-LMAR + 2 - WS-DNMW
               ELSE
                 COMPUTE WS-PP = WS-PP + WS-SPBC + 2 - WS-DNMW
               END-IF
               PERFORM VARYING WS-DD FROM 1 BY +1
               UNTIL WS-DD > 7
                 IF WS-DD > 1
                   COMPUTE WS-PP = WS-PP + WS-SPBD + 2 - WS-DNMW
                 END-IF
                 MOVE WS-DAY-NAME (WS-DD) (1:WS-DNMW)
                             TO WS-PRT (WS-PP:WS-DNMW)
                 COMPUTE WS-PP = WS-PP + WS-DNMW
               END-PERFORM
             END-PERFORM
             DISPLAY WS-PRT (1:WS-LS)

             PERFORM VARYING WS-WK FROM 1 BY +1
             UNTIL WS-WK > 6
               INITIALIZE WS-PRT
               COMPUTE WS-PP = 1
               PERFORM VARYING WS-COL FROM 1 BY +1
               UNTIL WS-COL > 3
               COMPUTE WS-MM = 3 * (WS-QQ - 1) + WS-COL

                 IF WS-COL = 1
                   COMPUTE WS-PP = WS-PP + WS-LMAR
                 ELSE
                   COMPUTE WS-PP = WS-PP + WS-SPBC
                 END-IF
                 PERFORM VARYING WS-DD FROM 1 BY +1
                 UNTIL WS-DD > 7
                   IF WS-DD > 1
                     COMPUTE WS-PP = WS-PP + WS-SPBD
                   END-IF
                   MOVE WS-DAY (WS-MM, WS-WK, WS-DD)
                               TO WS-PRT (WS-PP:2)
                   COMPUTE WS-PP = WS-PP + 2
                 END-PERFORM
               END-PERFORM
               DISPLAY WS-PRT (1:WS-LS)
             END-PERFORM
             DISPLAY ' '
           END-PERFORM
           GOBACK
           .
       END PROGRAM CALEND.
       IDENTIFICATION DIVISION.
       PROGRAM-ID.  DATE2DOW.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WMS-WORK-AREA.
         03  WMS-YEAR       PIC 9(04).
         03  WMS-MONTH      PIC 9(02).
         03  WMS-CSYS       PIC 9(01) VALUE 1.
         03  WMS-SUM        pic 9(04).
       LINKAGE SECTION.
       01  INPDATE-RECORD.
           05  INPD-YEAR          PIC 9(04).
           05  FILLER             PIC X(01).
           05  INPD-MONTH         PIC 9(02).
           05  FILLER             PIC X(01).
           05  INPD-DAY           PIC 9(02).
       01  WMS-DOW                PIC 9(01).
       PROCEDURE DIVISION USING INPDATE-RECORD, WMS-DOW.
       1010-CONVERT-DATE-TO-DOW.
           IF INPD-MONTH < 3
               COMPUTE WMS-MONTH = INPD-MONTH + 12
               COMPUTE WMS-YEAR  = INPD-YEAR - 1
           ELSE
               COMPUTE WMS-MONTH = INPD-MONTH
               COMPUTE WMS-YEAR  = INPD-YEAR
           END-IF
           COMPUTE WMS-SUM  =
                            ( INPD-DAY + 2 * WMS-MONTH + WMS-YEAR
                            + FUNCTION INTEGER (6 * (WMS-MONTH + 1) / 10)
                            + FUNCTION INTEGER ( WMS-YEAR / 4   )
                            - FUNCTION INTEGER ( WMS-YEAR / 100 )
                            + FUNCTION INTEGER ( WMS-YEAR / 400 )
                            + WMS-CSYS )
           COMPUTE WMS-DOW = FUNCTION MOD (WMS-SUM, 7) + 1
           GOBACK
           .
       END PROGRAM DATE2DOW.

```

Output (based on 80 character wide display)

```txt

     JANUARY         1969     FEBRUARY        1969     MARCH           1969
     SU MO TU WE TH FR SA     SU MO TU WE TH FR SA     SU MO TU WE TH FR SA
               1  2  3  4                        1                        1
      5  6  7  8  9 10 11      2  3  4  5  6  7  8      2  3  4  5  6  7  8
     12 13 14 15 16 17 18      9 10 11 12 13 14 15      9 10 11 12 13 14 15
     19 20 21 22 23 24 25     16 17 18 19 20 21 22     16 17 18 19 20 21 22
     26 27 28 29 30 31        23 24 25 26 27 28        23 24 25 26 27 28 29
                                                       30 31

     APRIL           1969     MAY             1969     JUNE            1969
     SU MO TU WE TH FR SA     SU MO TU WE TH FR SA     SU MO TU WE TH FR SA
            1  2  3  4  5                  1  2  3      1  2  3  4  5  6  7
      6  7  8  9 10 11 12      4  5  6  7  8  9 10      8  9 10 11 12 13 14
     13 14 15 16 17 18 19     11 12 13 14 15 16 17     15 16 17 18 19 20 21
     20 21 22 23 24 25 26     18 19 20 21 22 23 24     22 23 24 25 26 27 28
     27 28 29 30              25 26 27 28 29 30 31     29 30


     JULY            1969     AUGUST          1969     SEPTEMBER       1969
     SU MO TU WE TH FR SA     SU MO TU WE TH FR SA     SU MO TU WE TH FR SA
            1  2  3  4  5                     1  2         1  2  3  4  5  6
      6  7  8  9 10 11 12      3  4  5  6  7  8  9      7  8  9 10 11 12 13
     13 14 15 16 17 18 19     10 11 12 13 14 15 16     14 15 16 17 18 19 20
     20 21 22 23 24 25 26     17 18 19 20 21 22 23     21 22 23 24 25 26 27
     27 28 29 30 31           24 25 26 27 28 29 30     28 29 30
                              31

     OCTOBER         1969     NOVEMBER        1969     DECEMBER        1969
     SU MO TU WE TH FR SA     SU MO TU WE TH FR SA     SU MO TU WE TH FR SA
               1  2  3  4                        1         1  2  3  4  5  6
      5  6  7  8  9 10 11      2  3  4  5  6  7  8      7  8  9 10 11 12 13
     12 13 14 15 16 17 18      9 10 11 12 13 14 15     14 15 16 17 18 19 20
     19 20 21 22 23 24 25     16 17 18 19 20 21 22     21 22 23 24 25 26 27
     26 27 28 29 30 31        23 24 25 26 27 28 29     28 29 30 31
                              30

```



## Common Lisp

Depends on quicklisp.

```lisp
(ql:quickload '(date-calc))

(defparameter *day-row* "Su Mo Tu We Th Fr Sa")
(defparameter *calendar-margin* 3)

(defun month-to-word (month)
  "Translate a MONTH from 1 to 12 into its word representation."
  (svref #("January" "February" "March" "April"
           "May" "June" "July" "August"
           "September" "October" "November" "December")
         (1- month)))

(defun month-strings (year month)
  "Collect all of the strings that make up a calendar for a given
MONTH and YEAR."
  `(,(date-calc:center (month-to-word month) (length *day-row*))
     ,*day-row*
     ;; We can assume that a month calendar will always fit into a 7 by 6 block
     ;; of values. This makes it easy to format the resulting strings.
     ,@ (let ((days (make-array (* 7 6) :initial-element nil)))
          (loop :for i :from (date-calc:day-of-week year month 1)
             :for day :from 1 :to (date-calc:days-in-month year month)
             :do (setf (aref days i) day))
          (loop :for i :from 0 :to 5
             :collect
             (format nil "~{~:[  ~;~2,d~]~^ ~}"
                     (loop :for day :across (subseq days (* i 7) (+ 7 (* i 7)))
                        :append (if day (list day day) (list day))))))))

(defun calc-columns (characters margin-size)
  "Calculate the number of columns given the number of CHARACTERS per
column and the MARGIN-SIZE between them."
  (multiple-value-bind (cols excess)
      (truncate characters (+ margin-size (length *day-row*)))
    (incf excess margin-size)
    (if (>= excess (length *day-row*))
        (1+ cols)
        cols)))

(defun take (n list)
  "Take the first N elements of a LIST."
  (loop :repeat n :for x :in list :collect x))

(defun drop (n list)
  "Drop the first N elements of a LIST."
  (cond ((or (<= n 0) (null list)) list)
        (t (drop (1- n) (cdr list)))))

(defun chunks-of (n list)
  "Split the LIST into chunks of size N."
  (assert (> n 0))
  (loop :for x := list :then (drop n x)
     :while x
     :collect (take n x)))

(defun print-calendar (year &key (characters 80) (margin-size 3))
  "Print out the calendar for a given YEAR, optionally specifying
a width limit in CHARACTERS and MARGIN-SIZE between months."
  (assert (>= characters (length *day-row*)))
  (assert (>= margin-size 0))
  (let* ((calendars (loop :for month :from 1 :to 12
                       :collect (month-strings year month)))
         (column-count (calc-columns characters margin-size))
         (total-size (+ (* column-count (length *day-row*))
                        (* (1- column-count) margin-size)))
         (format-string (concatenate 'string
                                     "~{~a~^~" (write-to-string margin-size) ",0@T~}~%")))
    (format t "~a~%~a~%~%"
            (date-calc:center "[Snoopy]" total-size)
            (date-calc:center (write-to-string year) total-size))
    (loop :for row :in (chunks-of column-count calendars)
       :do (apply 'mapcar
                  (lambda (&rest heads)
                    (format t format-string heads))
                  row))))
```

{{out}}

```txt
CL-USER> (print-calendar 1969)
                             [Snoopy]
                               1969

      January                February                March
Su Mo Tu We Th Fr Sa   Su Mo Tu We Th Fr Sa   Su Mo Tu We Th Fr Sa
          1  2  3  4                      1                      1
 5  6  7  8  9 10 11    2  3  4  5  6  7  8    2  3  4  5  6  7  8
12 13 14 15 16 17 18    9 10 11 12 13 14 15    9 10 11 12 13 14 15
19 20 21 22 23 24 25   16 17 18 19 20 21 22   16 17 18 19 20 21 22
26 27 28 29 30 31      23 24 25 26 27 28      23 24 25 26 27 28 29
                                              30 31
       April                   May                    June
Su Mo Tu We Th Fr Sa   Su Mo Tu We Th Fr Sa   Su Mo Tu We Th Fr Sa
       1  2  3  4  5                1  2  3
 6  7  8  9 10 11 12    4  5  6  7  8  9 10    1  2  3  4  5  6  7
13 14 15 16 17 18 19   11 12 13 14 15 16 17    8  9 10 11 12 13 14
20 21 22 23 24 25 26   18 19 20 21 22 23 24   15 16 17 18 19 20 21
27 28 29 30            25 26 27 28 29 30 31   22 23 24 25 26 27 28
                                              29 30
        July                  August               September
Su Mo Tu We Th Fr Sa   Su Mo Tu We Th Fr Sa   Su Mo Tu We Th Fr Sa
       1  2  3  4  5                   1  2       1  2  3  4  5  6
 6  7  8  9 10 11 12    3  4  5  6  7  8  9    7  8  9 10 11 12 13
13 14 15 16 17 18 19   10 11 12 13 14 15 16   14 15 16 17 18 19 20
20 21 22 23 24 25 26   17 18 19 20 21 22 23   21 22 23 24 25 26 27
27 28 29 30 31         24 25 26 27 28 29 30   28 29 30
                       31
      October                November               December
Su Mo Tu We Th Fr Sa   Su Mo Tu We Th Fr Sa   Su Mo Tu We Th Fr Sa
          1  2  3  4                      1       1  2  3  4  5  6
 5  6  7  8  9 10 11    2  3  4  5  6  7  8    7  8  9 10 11 12 13
12 13 14 15 16 17 18    9 10 11 12 13 14 15   14 15 16 17 18 19 20
19 20 21 22 23 24 25   16 17 18 19 20 21 22   21 22 23 24 25 26 27
26 27 28 29 30 31      23 24 25 26 27 28 29   28 29 30 31
                       30
NIL
```



## D


```d
import std.stdio, std.datetime, std.string, std.conv;

void printCalendar(in uint year, in uint nCols)
in {
    assert(nCols > 0 && nCols <= 12);
} body {
    immutable rows = 12 / nCols + (12 % nCols != 0);
    auto date = Date(year, 1, 1);
    int offs = date.dayOfWeek;
    const months = "January February March April May June
        July August September October November December".split;

    string[8][12] mons;
    foreach (immutable m; 0 .. 12) {
        mons[m][0] = months[m].center(21);
        mons[m][1] = " Su Mo Tu We Th Fr Sa";
        immutable dim = date.daysInMonth;
        foreach (immutable d; 1 .. 43) {
            immutable day = d > offs && d <= offs + dim;
            immutable str = day ? format(" %2s", d-offs) : "   ";
            mons[m][2 + (d - 1) / 7] ~= str;
        }
        offs = (offs + dim) % 7;
        date.add!"months"(1);
    }

    "[Snoopy Picture]".center(nCols * 24 + 4).writeln;
    writeln(year.text.center(nCols * 24 + 4), "\n");
    foreach (immutable r; 0 .. rows) {
        string[8] s;
        foreach (immutable c; 0 .. nCols) {
            if (r * nCols + c > 11)
                break;
            foreach (immutable i, line; mons[r * nCols + c])
                s[i] ~= format("   %s", line);
        }
        writefln("%-(%s\n%)\n", s);
    }
}

void main() {
    printCalendar(1969, 3);
}
```



```txt
                              [Snoopy Picture]
                                    1969

          January                February                  March
    Su Mo Tu We Th Fr Sa    Su Mo Tu We Th Fr Sa    Su Mo Tu We Th Fr Sa
              1  2  3  4                       1                       1
     5  6  7  8  9 10 11     2  3  4  5  6  7  8     2  3  4  5  6  7  8
    12 13 14 15 16 17 18     9 10 11 12 13 14 15     9 10 11 12 13 14 15
    19 20 21 22 23 24 25    16 17 18 19 20 21 22    16 17 18 19 20 21 22
    26 27 28 29 30 31       23 24 25 26 27 28       23 24 25 26 27 28 29
                                                    30 31

           April                    May                    June
    Su Mo Tu We Th Fr Sa    Su Mo Tu We Th Fr Sa    Su Mo Tu We Th Fr Sa
           1  2  3  4  5                 1  2  3     1  2  3  4  5  6  7
     6  7  8  9 10 11 12     4  5  6  7  8  9 10     8  9 10 11 12 13 14
    13 14 15 16 17 18 19    11 12 13 14 15 16 17    15 16 17 18 19 20 21
    20 21 22 23 24 25 26    18 19 20 21 22 23 24    22 23 24 25 26 27 28
    27 28 29 30             25 26 27 28 29 30 31    29 30


           July                   August                 September
    Su Mo Tu We Th Fr Sa    Su Mo Tu We Th Fr Sa    Su Mo Tu We Th Fr Sa
           1  2  3  4  5                    1  2        1  2  3  4  5  6
     6  7  8  9 10 11 12     3  4  5  6  7  8  9     7  8  9 10 11 12 13
    13 14 15 16 17 18 19    10 11 12 13 14 15 16    14 15 16 17 18 19 20
    20 21 22 23 24 25 26    17 18 19 20 21 22 23    21 22 23 24 25 26 27
    27 28 29 30 31          24 25 26 27 28 29 30    28 29 30
                            31

          October                November                December
    Su Mo Tu We Th Fr Sa    Su Mo Tu We Th Fr Sa    Su Mo Tu We Th Fr Sa
              1  2  3  4                       1        1  2  3  4  5  6
     5  6  7  8  9 10 11     2  3  4  5  6  7  8     7  8  9 10 11 12 13
    12 13 14 15 16 17 18     9 10 11 12 13 14 15    14 15 16 17 18 19 20
    19 20 21 22 23 24 25    16 17 18 19 20 21 22    21 22 23 24 25 26 27
    26 27 28 29 30 31       23 24 25 26 27 28 29    28 29 30 31
                            30
```



## Fortran

Already having a routine to produce a calendar simplified matters. However, it added to each row of days a row of annotations for those days (Xmas, etc. and also +2hh and -2hh for the days with two half-hour changes in length due to daylight saving: NZ also had daylight saving with changes of one half-hour) which meant that the field allowance was always four. With the annotations abandoned, this could be reduced to three, and, the first day column on a line does not need a leading space. Since the method employed variables for the layout, it could easily be twiddled to have three months per line (thus fitting into a line length of 80) or six (using most of a line length of 132) and so it became a matter of pulling together the needed routines from various places.

```Fortran

       MODULE DATEGNASH	!Assorted vexations. Time and calendar games, with local flavourings added.

       TYPE DateBag		!Pack three parts into one.
        INTEGER DAY,MONTH,YEAR	!The usual suspects.
       END TYPE DateBag		!Simple enough.

       CHARACTER*9 MONTHNAME(12),DAYNAME(0:6)	!Re-interpretations.
       PARAMETER (MONTHNAME = (/"January","February","March","April",
     1  "May","June","July","August","September","October","November",
     2  "December"/))
       PARAMETER (DAYNAME = (/"Sunday","Monday","Tuesday","Wednesday",
     1  "Thursday","Friday","Saturday"/))	!Index this array with DayNum mod 7.
       CHARACTER*3 MTHNAME(12)		!The standard abbreviations.
       PARAMETER (MTHNAME = (/"JAN","FEB","MAR","APR","MAY","JUN",
     1  "JUL","AUG","SEP","OCT","NOV","DEC"/))

       INTEGER*4 JDAYSHIFT		!INTEGER*2 just isn't enough.
       PARAMETER (JDAYSHIFT = 2415020)	!Thus shall 31/12/1899 give 0, a Sunday, via DAYNUM.
       CONTAINS
       INTEGER FUNCTION LSTNB(TEXT)  !Sigh. Last Not Blank.
Concocted yet again by R.N.McLean (whom God preserve) December MM.
Code checking reveals that the Compaq compiler generates a copy of the string and then finds the length of that when using the latter-day intrinsic LEN_TRIM. Madness!
Can't   DO WHILE (L.GT.0 .AND. TEXT(L:L).LE.' ')	!Control chars. regarded as spaces.
Curse the morons who think it good that the compiler MIGHT evaluate logical expressions fully.
Crude GO TO rather than a DO-loop, because compilers use a loop counter as well as updating the index variable.
Comparison runs of GNASH showed a saving of ~3% in its mass-data reading through the avoidance of DO in LSTNB alone.
Crappy code for character comparison of varying lengths is avoided by using ICHAR which is for single characters only.
Checking the indexing of CHARACTER variables for bounds evoked astounding stupidities, such as calculating the length of TEXT(L:L) by subtracting L from L!
Comparison runs of GNASH showed a saving of ~25-30% in its mass data scanning for this, involving all its two-dozen or so single-character comparisons, not just in LSTNB.
        CHARACTER*(*),INTENT(IN):: TEXT	!The bumf. If there must be copy-in, at least there need not be copy back.
        INTEGER L		!The length of the bumf.
         L = LEN(TEXT)		!So, what is it?
    1    IF (L.LE.0) GO TO 2	!Are we there yet?
         IF (ICHAR(TEXT(L:L)).GT.ICHAR(" ")) GO TO 2	!Control chars are regarded as spaces also.
         L = L - 1		!Step back one.
         GO TO 1		!And try again.
    2    LSTNB = L		!The last non-blank, possibly zero.
        RETURN			!Unsafe to use LSTNB as a variable.
       END FUNCTION LSTNB	!Compilers can bungle it.
      CHARACTER*2 FUNCTION I2FMT(N)	!These are all the same.
       INTEGER*4 N			!But, the compiler doesn't offer generalisations.
        IF (N.LT.0) THEN	!Negative numbers cop a sign.
          IF (N.LT.-9) THEN	!But there's not much room left.
            I2FMT = "-!"	!So this means 'overflow'.
           ELSE			!Otherwise, room for one negative digit.
            I2FMT = "-"//CHAR(ICHAR("0") - N)	!Thus. Presume adjacent character codes, etc.
          END IF		!So much for negative numbers.
        ELSE IF (N.LT.10) THEN	!Single digit positive?
          I2FMT = " " //CHAR(ICHAR("0") + N)	!Yes. This.
        ELSE IF (N.LT.100) THEN	!Two digit positive?
          I2FMT = CHAR(N/10      + ICHAR("0"))	!Yes.
     1           //CHAR(MOD(N,10) + ICHAR("0")) !These.
        ELSE			!Otherwise,
          I2FMT = "+!" 	!Positive overflow.
        END IF			!So much for that.
      END FUNCTION I2FMT	!No WRITE and FORMAT unlimbering.
      CHARACTER*8 FUNCTION I8FMT(N)	!Oh for proper strings.
       INTEGER*4 N
       CHARACTER*8 HIC
        WRITE (HIC,1) N
    1   FORMAT (I8)
        I8FMT = HIC
      END FUNCTION I8FMT

      SUBROUTINE SAY(OUT,TEXT)	!Gutted version that maintains no file logging output, etc.
       INTEGER OUT
       CHARACTER*(*) TEXT
        WRITE (6,1) TEXT(1:LSTNB(TEXT))
    1   FORMAT (A)
      END SUBROUTINE SAY

       INTEGER*4 FUNCTION DAYNUM(YY,M,D)	!Computes (JDayN - JDayShift), not JDayN.
C   Conversion from a Gregorian calendar date to a Julian day number, JDayN.
C   Valid for any Gregorian calendar date producing a Julian day number
C greater than zero, though remember that the Gregorian calendar
C was not used before y1582m10d15 and often, not after that either.
C thus in England (et al) when Wednesday 2'nd September 1752 (Julian style)
C was followed by Thursday the 14'th, occasioning the Eleven Day riots
C because creditors demanded a full month's payment instead of 19/30'ths.
C   The zero of the Julian day number corresponds to the first of January
C 4713BC on the *Julian* calendar's naming scheme, as extended backwards
C with current usage into epochs when it did not exist: the proleptic Julian calendar.
c This function employs the naming scheme of the *Gregorian* calendar,
c and if extended backwards into epochs when it did not exist (thus the
c proleptic Gregorian calendar) it would compute a zero for y-4713m11d24 *if*
c it is supposed there was a year zero between 1BC and 1AD (as is convenient
c for modern mathematics and astronomers and their simple calculations), *but*
c 1BC immediately preceeds 1AD without any year zero in between (and is a leap year)
c thus the adjustment below so that the date is y-4714m11d24 or 4714BCm11d24,
c not that this name was in use at the time...
c   Although the Julian calendar (introduced by himself in what we would call 45BC,
c which was what the Romans occasionally called 709AUC) was provoked by the
c "years of confusion" resulting from arbitrary application of the rules
c for the existing Roman calendar, other confusions remain unresolved,
c so precise dating remains uncertain despite apparently precise specifications
c (and much later, Dennis the Short chose wrongly for the birth of Christ)
c and the Roman practice of inclusive reckoning meant that every four years
c was interpreted as every third (by our exclusive reckoning) so that the
c leap years were not as we now interpret them. This was resolved by Augustus
c but exactly when (and what date name is assigned) and whose writings used
c which system at the time of writing is a matter of more confusion,
c and this has continued for centuries.
C   Accordingly, although an algorithm may give a regular sequence of date names,
c that does not mean that those date names were used at the time even if the
c calendar existed then, because the interpretation of the algorithm varied.
c This in turn means that a date given as being on the Julian calendar
c prior to about 10AD is not as definite as it may appear and its alignment
c with the astronomical day number is uncertain even though the calculation
c is quite definite.
c
C   Computationally, year 1 is preceded by year 0, in a smooth progression.
C But there was never a year zero despite what astronomers like to say,
C so the formula's year 0 corresponds to 1BC, year -1 to 2BC, and so on back.
C Thus y-4713 in this counting would be 4714BC on the Gregorian calendar,
C were it to have existed then which it didn't.
C   To conform to the civil usage, the incoming YY, presumed a proper BC (negative)
C and AD (positive) year is converted into the computational counting sequence, Y,
C and used in the formula. If a YY = 0 is (improperly) offered, it will manifest
C as 1AD. Thus YY = -4714 will lead to calculations with Y = -4713.
C Thus, 1BC is a leap year on the proleptic Gregorian calendar.
C   For their convenience, astronomers decreed that a day starts at noon, so that
C in Europe, observations through the night all have the same day number.
C The current Western civil calendar however has the day starting just after midnight
C and that day's number lasts until the following midnight.
C
C   There is no constraint on the values of D, which is just added as it stands.
C This means that if D = 0, the daynumber will be that of the last day of the
C previous month. Likewise, M = 0 or M = 13 will wrap around so that Y,M + 1,0
C will give the last day of month M (whatever its length) as one day before
C the first day of the next month.
C
C   Example: Y = 1970, M = 1, D = 1;  JDAYN = 2440588, a Thursday but MOD(2440588,7) = 3.
C   and with the adjustment JDAYSHIFT, DAYNUM = 25568; mod 7 = 4 and DAYNAME(4) = "Thursday".
C   The Julian Day number 2440588.0 is for NOON that Thursday, 2440588.5 is twelve hours later.
C   And Julian Day number 2440587.625 is for three a.m. Thursday.
C
C   DAYNUM and MUNYAD are the infamous routines of H. F. Fliegel and T.C. van Flandern,
C   presented in Communications of the ACM, Vol. 11, No. 10 (October, 1968).
Carefully typed in again by R.N.McLean (whom God preserve) December XXMMIIX.
C   Though I remain puzzled as to why they used I,J,K for Y,M,D,
C given that the variables were named in the INTEGER statement anyway.
        INTEGER*4 JDAYN		!Without rebasing, this won't fit in INTEGER*2.
        INTEGER YY,Y,M,MM,D	!NB! Full year number, so 1970, not 70.
Caution: integer division in Fortran does not produce fractional results.
C The fractional part is discarded so that 4/3 gives 1 and -4/3 gives -1.
C Thus 4/3 might be Trunc(4/3) or 4 div 3 in other languages. Beware of negative numbers!
         Y = YY		!I can fiddle this copy without damaging the original's value.
         IF (Y.LT.1) Y = Y + 1	!Thus YY = -2=2BC, -1=1BC, +1=1AD, ... becomes Y = -1, 0, 1, ...
         MM = (M - 14)/12	!Calculate once. Note that this is integer division, truncating.
         JDAYN = D - 32075	!This is the proper astronomer's Julian Day Number.
     a    + 1461*(Y + 4800  + MM)/4
     b    +  367*(M - 2     - MM*12)/12
     c    -    3*((Y + 4900 + MM)/100)/4
         DAYNUM = JDAYN - JDAYSHIFT	!Thus, *NOT* the actual *Julian* Day Number.
       END FUNCTION DAYNUM		!But one such that Mod(n,7) gives day names.

Could compute the day of the year somewhat as follows...
c  DN:=D + (61*Month + (Month div 8)) div 2 - 30
c        + if Month > 2 then FebLength - 30 else 0;

       TYPE(DATEBAG) FUNCTION MUNYAD(DAYNUM)	!Oh for palindromic programming!
Conversion from a Julian day number to a Gregorian calendar date. See JDAYN/DAYNUM.
        INTEGER*4 DAYNUM,JDAYN	!Without rebasing, this won't fit in INTEGER*2.
        INTEGER Y,M,D,L,N		!Y will be a full year number: 1950 not 50.
         JDAYN = DAYNUM + JDAYSHIFT	!Revert to a proper Julian day number.
         L = JDAYN + 68569	!Further machinations of H. F. Fliegel and T.C. van Flandern.
         N = 4*L/146097
         L = L - (146097*N + 3)/4
         Y = 4000*(L + 1)/1461001
         L = L - 1461*Y/4 + 31
         M = 80*L/2447
         D = L - 2447*M/80
         L = M/11
         M = M + 2 - 12*L
         Y = 100*(N - 49) + Y + L
         IF (Y.LT.1) Y = Y - 1	!The other side of conformity to BC/AD, as in DAYNUM.
         MUNYAD%YEAR  = Y	!Now place for the world to see.
         MUNYAD%MONTH = M
         MUNYAD%DAY   = D
       END FUNCTION MUNYAD	!A year has 365.2421988 days...

       INTEGER FUNCTION PMOD(N,M)	!Remainder, mod M; always positive even if N is negative.
c   For date calculations, the MOD function is expected to yield positive remainders,
c in line with the idea that MOD(a,b) = MOD(a ± b,b) as is involved in shifting the zero
c of the daynumber count by a multiple of seven when considering the day of the week.
c For this reason, the zero day was chosen to be 31/12/1899, a Sunday, so that all
c day numbers would be positive. But, there was generation at Reefton in 1886.
c   For some computers, the positive interpretation is implemented, for others, not.
c In the case MOD(N,M) = N - Truncate(N/M)*M, MOD(-6,7) = -6 even though MOD(1,7) = 1.
        INTEGER N,M			!The numbers. M presumed positive.
         PMOD = MOD(MOD(N,M) + M,M)	!Double do does de deed.
       END FUNCTION PMOD		!Simple enough.

      SUBROUTINE CALENDAR(Y1,Y2,COLUMNS)	!Print a calendar, with holiday annotations.
Careful with the MOD function. MOD(-6,7) may be negative on some systems, positive on others. Thus, PMOD.
       INTEGER Y1,Y2,YEAR		!Ah yes. Year stuff.
       INTEGER M,M1,M2,MONTH		!And within each year are the months.
       INTEGER*4 DN1,DN2,DN,D		!But days are handled via day numbers.
       INTEGER W,G			!Layout: width and gap.
       INTEGER L,LINE			!Vertical layout.
       INTEGER COL,COLUMNS,COLWIDTH	!Horizontal layout.
       INTEGER CODE			!Days are not all alike.
       CHARACTER*200 STRIPE(6),SPECIAL(6),MLINE,DLINE	!Scratchpads.
        IF (Y1.LE.0) CALL SAY(MSG,"Despite the insinuations of "
     1   //"astronomers seduced by the ease of their arithmetic, "
     2   //"there is no year zero. 1AD is preceded by 1BC, "
     3   //"corresponding to year -1, 2BC to year -2, etc.")
        IF (Y1.LT.1582) CALL SAY(MSG,"This Gregorian calendar"
     1   //" scheme did not exist prior to 1582.")
c        COLUMNS = 4	!Number of months across the page.
c        W = 4		!Width of a day's field.
c        G = 3		!Added gap between month columns.
        W = 3		!Abandon the annotation of the day's class, so just a space and two digits.
        G = 1		!Set the gap to one.
        COLWIDTH = 7*W + G	!Seven days to a week, plus a gap.
      Y:DO YEAR = Y1,Y2		!Step through the years.
          CALL SAY(MSG,"")	!Space out between each year's schedule.
          IF (YEAR.EQ.0) THEN	!This year number is improper.
            CALL SAY(MSG,"There is no year zero.")	!Declare correctness.
            CYCLE Y		!Skip this year.
          END IF		!Otherwise, no evasions.
          MLINE = ""		!Prepare a field..
          L = (COLUMNS*COLWIDTH - G - 8)/2	!Find the centre.
          IF (YEAR.GT.0) THEN	!Ordinary Anno Domine years?
            MLINE(L:) = I8FMT(YEAR)	!Yes. Place the year number.
           ELSE			!Otherwise, we're in BC.
            MLINE(L - 1:) = I8FMT(-YEAR)//"BC"	!There is no year zero.
          END IF		!So much for year games.
          CALL SAY(MSG,MLINE)		!Splot the year.
          DO MONTH = 1,12,COLUMNS	!Step through the months of this YEAR.
            M1 = MONTH			!The first of this lot.
            M2 = MIN(12,M1 + COLUMNS - 1)	!The last.
            MLINE = ""			!Scrub the month names.
            DLINE = ""			!Wipe the day names in case COLUMNS does not divide 12.
            STRIPE = ""			!Scrub the day table.
            SPECIAL = ""		!And the associated special day remarks.
c            L0 = W - 1			!Locate the first day number's first column.
            L0 = 1			!Cram: no space in front of the Sunday day-of-the-month.
            DO M = M1,M2		!Work through the months.
              L = (COLWIDTH - G - LSTNB(MONTHNAME(M)))/2 - 1	!Centre the month name.
              MLINE(L0 + L:) = MONTHNAME(M)	!Splot.
              DO D = 0,6		!Prepare this month's day name heading.
                L = L0 + (3 - W) + D*W	!Locate its first column.
                DLINE(L:L + 2) = DAYNAME(D)(1:W - 1)	!Squish.
              END DO		!On to the next day.
              DN1 = DAYNUM(YEAR,M,1)	!Day number of the first day of the month.
              DN2 = DAYNUM(YEAR,M + 1,0)!Thus the last, without annoyance.
              COL = MOD(PMOD(DN1,7) + 7,7)	!What day of the week is the first day?
              LINE = 1			!Whichever it is, it is on the first line.
              D = 1			!Day of the month, not number of the day.
              DO DN = DN1,DN2		!Step through the day numbers of this month.
                L = L0 + COL*W		!Finger the starting column.
                STRIPE(LINE)(L:L + 1) = I2FMT(D)	!Place the two-digit day number.
                D = D + 1		!Advance to the next day of the current month
                COL = COL + 1		!So, one more day along in the week.
                IF (COL.GT.6) THEN	!A fresh week is needed?
                  LINE = LINE + 1	!Yes.
                  COL = 0		!Start the new week.
                END IF		!So much for the end of a week.
              END DO		!On to the next day of this month.
              L0 = L0 + 7*W + G	!Locate the start column of the next month's column.
            END DO	!On to the next month in this layer.
            CALL SAY(MSG,MLINE)	!Name the months.
C            CALL SAY(MSG,"")	!Set off.
            CALL SAY(MSG,DLINE)	!Give the day name headings.
            DO LINE = 1,6	!Now roll the day number table.
              IF (STRIPE(LINE).NE."") THEN	!Perhaps there was no use of the sixth line.
                CALL SAY(MSG,STRIPE(LINE))	!Ah well. Show the day numbers.
              END IF				!So much for that week line.
            END DO			!On to the next week line.
          END DO		!On to the next batch of months of the YEAR.
        END DO Y		!On to the next YEAR.
        CALL SAY(MSG,"")	!Take a breath.
      END SUBROUTINE CALENDAR	!Enough of this.
      END MODULE DATEGNASH	!An ad-hoc assemblage.

      PROGRAM SHOW1968		!Put it to the test.
       USE DATEGNASH
       INTEGER NCOL
        DO NCOL = 1,6
          CALL CALENDAR(1969,1969,NCOL)
        END DO
      END

```

Selected output, lacking alas the outbursts from Snoopy: three months /line

```txt

                               1969
      January              February                March
Su Mo Tu We Th Fr Sa  Su Mo Tu We Th Fr Sa  Su Mo Tu We Th Fr Sa
          1  2  3  4                     1                     1
 5  6  7  8  9 10 11   2  3  4  5  6  7  8   2  3  4  5  6  7  8
12 13 14 15 16 17 18   9 10 11 12 13 14 15   9 10 11 12 13 14 15
19 20 21 22 23 24 25  16 17 18 19 20 21 22  16 17 18 19 20 21 22
26 27 28 29 30 31     23 24 25 26 27 28     23 24 25 26 27 28 29
                                            30 31
       April                  May                  June
Su Mo Tu We Th Fr Sa  Su Mo Tu We Th Fr Sa  Su Mo Tu We Th Fr Sa
       1  2  3  4  5               1  2  3   1  2  3  4  5  6  7
 6  7  8  9 10 11 12   4  5  6  7  8  9 10   8  9 10 11 12 13 14
13 14 15 16 17 18 19  11 12 13 14 15 16 17  15 16 17 18 19 20 21
20 21 22 23 24 25 26  18 19 20 21 22 23 24  22 23 24 25 26 27 28
27 28 29 30           25 26 27 28 29 30 31  29 30
       July                 August               September
Su Mo Tu We Th Fr Sa  Su Mo Tu We Th Fr Sa  Su Mo Tu We Th Fr Sa
       1  2  3  4  5                  1  2      1  2  3  4  5  6
 6  7  8  9 10 11 12   3  4  5  6  7  8  9   7  8  9 10 11 12 13
13 14 15 16 17 18 19  10 11 12 13 14 15 16  14 15 16 17 18 19 20
20 21 22 23 24 25 26  17 18 19 20 21 22 23  21 22 23 24 25 26 27
27 28 29 30 31        24 25 26 27 28 29 30  28 29 30
                      31
      October              November              December
Su Mo Tu We Th Fr Sa  Su Mo Tu We Th Fr Sa  Su Mo Tu We Th Fr Sa
          1  2  3  4                     1      1  2  3  4  5  6
 5  6  7  8  9 10 11   2  3  4  5  6  7  8   7  8  9 10 11 12 13
12 13 14 15 16 17 18   9 10 11 12 13 14 15  14 15 16 17 18 19 20
19 20 21 22 23 24 25  16 17 18 19 20 21 22  21 22 23 24 25 26 27
26 27 28 29 30 31     23 24 25 26 27 28 29  28 29 30 31
                      30

```

And with six...

```txt

                                                                1969
      January              February                March                 April                  May                  June
Su Mo Tu We Th Fr Sa  Su Mo Tu We Th Fr Sa  Su Mo Tu We Th Fr Sa  Su Mo Tu We Th Fr Sa  Su Mo Tu We Th Fr Sa  Su Mo Tu We Th Fr Sa
          1  2  3  4                     1                     1         1  2  3  4  5               1  2  3   1  2  3  4  5  6  7
 5  6  7  8  9 10 11   2  3  4  5  6  7  8   2  3  4  5  6  7  8   6  7  8  9 10 11 12   4  5  6  7  8  9 10   8  9 10 11 12 13 14
12 13 14 15 16 17 18   9 10 11 12 13 14 15   9 10 11 12 13 14 15  13 14 15 16 17 18 19  11 12 13 14 15 16 17  15 16 17 18 19 20 21
19 20 21 22 23 24 25  16 17 18 19 20 21 22  16 17 18 19 20 21 22  20 21 22 23 24 25 26  18 19 20 21 22 23 24  22 23 24 25 26 27 28
26 27 28 29 30 31     23 24 25 26 27 28     23 24 25 26 27 28 29  27 28 29 30           25 26 27 28 29 30 31  29 30
                                            30 31
       July                 August               September              October              November              December
Su Mo Tu We Th Fr Sa  Su Mo Tu We Th Fr Sa  Su Mo Tu We Th Fr Sa  Su Mo Tu We Th Fr Sa  Su Mo Tu We Th Fr Sa  Su Mo Tu We Th Fr Sa
       1  2  3  4  5                  1  2      1  2  3  4  5  6            1  2  3  4                     1      1  2  3  4  5  6
 6  7  8  9 10 11 12   3  4  5  6  7  8  9   7  8  9 10 11 12 13   5  6  7  8  9 10 11   2  3  4  5  6  7  8   7  8  9 10 11 12 13
13 14 15 16 17 18 19  10 11 12 13 14 15 16  14 15 16 17 18 19 20  12 13 14 15 16 17 18   9 10 11 12 13 14 15  14 15 16 17 18 19 20
20 21 22 23 24 25 26  17 18 19 20 21 22 23  21 22 23 24 25 26 27  19 20 21 22 23 24 25  16 17 18 19 20 21 22  21 22 23 24 25 26 27
27 28 29 30 31        24 25 26 27 28 29 30  28 29 30              26 27 28 29 30 31     23 24 25 26 27 28 29  28 29 30 31
                      31                                                                30

```


## FreeBASIC


```freebasic
' version 17-02-2016
' compile with: fbc -s console

' TRUE/FALSE are built-in constants since FreeBASIC 1.04
' For older versions they have to be defined.
#Ifndef TRUE
    #Define FALSE 0
    #Define TRUE Not FALSE
#EndIf

Function WD(m As Integer, d As Integer, y As Integer) As Integer
    ' Zellerish
    ' 0 = Sunday, 1 = Monday, 2 = Tuesday, 3 = Wednesday
    ' 4 = Thursday, 5 = Friday, 6 = Saturday

    If m < 3 Then        ' if m = 1 or m = 2 then
        m += 12
        y -= 1
    End If
    Return (y + (y \ 4) - (y \ 100) + (y \ 400) + d + ((153 * m + 8) \ 5)) Mod 7
End Function

Function LEAPYEAR(y As Integer) As Integer
    If (y Mod 4) <> 0 Then Return FALSE
    If (y Mod 100) = 0 AndAlso (y Mod 400) <> 0 Then Return FALSE
    Return TRUE
End Function

' ------=< main >=------

Dim As String wdn = "Mo Tu We Th Fr Sa Su"  ' weekday names
Dim As String mo(1 To 12) => {"January", "February", "March", "April", _
                         "May", "June", "July", "August", "September", _
                                    "October", "November", "December"}
Dim As String tmp1, tmp2, d(1 To 12)

Dim As UInteger ml(1 To 12) => {31,28,31,30,31,30,31,31,30,31,30,31}
Dim As UInteger i, i1, j, k, y = 1969
Dim As UInteger m_row = 6

Do
    While InKey <> "" : Wend  ' clear keyboard buffer
    Print : Print " For wich year do want a calendar"
    Print " Year must be greater then 1752"
    Input " Input year (enter = 1969)";tmp1
    If tmp1 = "" Then Exit Do
    i = Val(tmp1)
    If i < 1752 Then
        Print
        Print " Can only make a calendar for a year after 1752"
        Beep : Sleep 5000, 1 : Print
    Else
        y = i : Exit Do
    End If
Loop

Cls
Do
    While InKey <> "" : Wend  ' clear keyboard buffer
    Print : Print " Make device choice"
    Print " 132 characters Line printer, 6x2 months (Enter or 1)"
    Print " 80x43 display,               3x4 months          (2)"
    Do
        tmp1 = InKey
        If tmp1 = Chr(13) Or tmp1 = "1" Then Exit Do, Do
        If tmp1 = "2" Then
            m_row = 3
            Exit Do, Do
        End If
    Loop Until tmp1 <> ""
    Print : Print " Enter, 1 or 2 only"
    Beep : Sleep 5000, 1 : Print
Loop
Cls

Dim As UInteger char_line = m_row * 22 - 1
If LEAPYEAR(y) = TRUE Then ml(2) = 29

tmp1 = ""
For i = 1 To 31
    tmp1 = tmp1 + Right(("  " + Str(i)), 3)
Next

For i = 1 To 12
    tmp2 = ""
    j = WD(i,1, y)
    If j = 0 Then j = 7
    j = j - 1
    tmp2 = Space(j * 3) + Left(tmp1, ml(i) * 3) + Space(21)
    d(i) = tmp2
Next

Print
tmp1 = Str(y)
Print Space((char_line + (char_line And 1) - Len(tmp1)) \ 2); tmp1
Print

tmp2 = " "    ' make the weekday names line
For i = 1 To m_row
    tmp2 = tmp2 + wdn
    If i < m_row Then tmp2 = tmp2 + "  "
Next

For i = 1 To 12 Step m_row
    tmp1 = ""
    For j = i To i + m_row -2 ' make the month names line
        tmp1 = tmp1 + Left(Space((22 - Len(mo(j))) \ 2) + mo(j) + Space(21), 22)
    Next
    tmp1 = tmp1 + Space((22 - Len(mo(i + m_row -1))) \ 2) + mo(i + m_row -1)
    Print tmp1
    Print tmp2
    For j = 1 To 85 Step 21
        For k = i To i + m_row -2
            Print Mid(d(k), j ,21); " ";
        Next
        Print Mid(d(i + m_row -1), j ,21)
    Next
    Print
Next

' empty keyboard buffer
While InKey <> "" : Wend
'Print : Print "hit any key to end program
Sleep
End
```

{{out}}

```txt
                                                                1969

       January               February               March                 April                  May                   June
 Mo Tu We Th Fr Sa Su  Mo Tu We Th Fr Sa Su  Mo Tu We Th Fr Sa Su  Mo Tu We Th Fr Sa Su  Mo Tu We Th Fr Sa Su  Mo Tu We Th Fr Sa Su
        1  2  3  4  5                  1  2                  1  2      1  2  3  4  5  6            1  2  3  4                     1
  6  7  8  9 10 11 12   3  4  5  6  7  8  9   3  4  5  6  7  8  9   7  8  9 10 11 12 13   5  6  7  8  9 10 11   2  3  4  5  6  7  8
 13 14 15 16 17 18 19  10 11 12 13 14 15 16  10 11 12 13 14 15 16  14 15 16 17 18 19 20  12 13 14 15 16 17 18   9 10 11 12 13 14 15
 20 21 22 23 24 25 26  17 18 19 20 21 22 23  17 18 19 20 21 22 23  21 22 23 24 25 26 27  19 20 21 22 23 24 25  16 17 18 19 20 21 22
 27 28 29 30 31        24 25 26 27 28        24 25 26 27 28 29 30  28 29 30              26 27 28 29 30 31     23 24 25 26 27 28 29

         July                 August              September              October               November              December
 Mo Tu We Th Fr Sa Su  Mo Tu We Th Fr Sa Su  Mo Tu We Th Fr Sa Su  Mo Tu We Th Fr Sa Su  Mo Tu We Th Fr Sa Su  Mo Tu We Th Fr Sa Su
     1  2  3  4  5  6               1  2  3   1  2  3  4  5  6  7         1  2  3  4  5                  1  2   1  2  3  4  5  6  7
  7  8  9 10 11 12 13   4  5  6  7  8  9 10   8  9 10 11 12 13 14   6  7  8  9 10 11 12   3  4  5  6  7  8  9   8  9 10 11 12 13 14
 14 15 16 17 18 19 20  11 12 13 14 15 16 17  15 16 17 18 19 20 21  13 14 15 16 17 18 19  10 11 12 13 14 15 16  15 16 17 18 19 20 21
 21 22 23 24 25 26 27  18 19 20 21 22 23 24  22 23 24 25 26 27 28  20 21 22 23 24 25 26  17 18 19 20 21 22 23  22 23 24 25 26 27 28
 28 29 30 31           25 26 27 28 29 30 31  29 30                 27 28 29 30 31        24 25 26 27 28 29 30  29 30 31
```



## F Sharp


```fsharp
let getCalendar year =
    let day_of_week month year =
        let t =  [|0; 3; 2; 5; 0; 3; 5; 1; 4; 6; 2; 4|]
        let y = if month < 3 then year - 1 else year
        let m = month
        let d = 1
        (y + y / 4 - y / 100 + y / 400 + t.[m - 1] + d) % 7
        //0 = Sunday, 1 = Monday, ...

    let last_day_of_month month year =
        match month with
        | 2 -> if (0 = year % 4 && (0 = year % 400 || 0 <> year % 100)) then 29 else 28
        | 4 | 6 | 9 | 11 -> 30
        | _ -> 31

    let get_month_calendar year month =
        let min (x: int, y: int) = if x < y then x else y
        let ld = last_day_of_month month year
        let dw = 7 - (day_of_week month year)
        [|[|1..dw|];
          [|dw + 1..dw + 7|];
          [|dw + 8..dw + 14|];
          [|dw + 15..dw + 21|];
          [|dw + 22..min(ld, dw + 28)|];
          [|min(ld + 1, dw + 29)..ld|]|]

    let sb_fold (f:System.Text.StringBuilder -> 'a -> System.Text.StringBuilder) (sb:System.Text.StringBuilder) (xs:'a array)  =
        for x in xs do (f sb  x) |> ignore
        sb

    let sb_append (text:string) (sb:System.Text.StringBuilder) = sb.Append(text)

    let sb_appendln sb = sb |> sb_append "\n" |> ignore

    let sb_fold_in_range a b f sb = [|a..b|] |> sb_fold f sb |> ignore

    let mask_builder mask = Printf.StringFormat<string -> string>(mask)
    let center n (s:string) =
        let l = (n - s.Length) / 2 + s.Length
        let f n s = sprintf (mask_builder ("%" + (n.ToString()) + "s")) s
        (f l s) + (f (n - l) "")
    let left n (s:string)  = sprintf (mask_builder ("%-" + (n.ToString()) + "s")) s
    let right n (s:string) = sprintf (mask_builder ("%" + (n.ToString()) + "s")) s

    let array2string xs =
        let ys = xs |> Array.map (fun x -> sprintf "%2d " x)
        let sb = ys |> sb_fold (fun sb y -> sb.Append(y)) (new System.Text.StringBuilder())
        sb.ToString()

    let xsss =
        let m = get_month_calendar year
        [|1..12|] |> Array.map (fun i -> m i)

    let months = [|"January"; "February"; "March"; "April"; "May"; "June"; "July"; "August"; "September"; "October"; "November"; "December"|]

    let sb = new System.Text.StringBuilder()
    sb |> sb_append "\n" |> sb_append (center 74 (year.ToString())) |> sb_appendln
    for i in 0..3..9 do
      sb |> sb_appendln
      sb |> sb_fold_in_range i (i + 2) (fun sb i -> sb |> sb_append (center 21 months.[i]) |> sb_append "      ")
      sb |> sb_appendln
      sb |> sb_fold_in_range i (i + 2) (fun sb i -> sb |> sb_append "Su Mo Tu We Th Fr Sa " |> sb_append "      ")
      sb |> sb_appendln
      sb |> sb_fold_in_range i (i + 2) (fun sb i -> sb |> sb_append (right 21 (array2string (xsss.[i].[0]))) |> sb_append "      ")
      sb |> sb_appendln
      for j = 1 to 5 do
        sb |> sb_fold_in_range i (i + 2) (fun sb i -> sb |> sb_append (left 21 (array2string (xsss.[i].[j]))) |> sb_append "      ")
        sb |> sb_appendln
    sb.ToString()

let printCalendar year = getCalendar year
```


{{out}}

```txt
> printCalendar 1969;;
val it : string =
  "
                                   1969

       January                   February                     March
Su Mo Tu We Th Fr Sa       Su Mo Tu We Th Fr Sa       Su Mo Tu We Th Fr Sa
          1  2  3  4                          1                          1
 5  6  7  8  9 10 11        2  3  4  5  6  7  8        2  3  4  5  6  7  8
12 13 14 15 16 17 18        9 10 11 12 13 14 15        9 10 11 12 13 14 15
19 20 21 22 23 24 25       16 17 18 19 20 21 22       16 17 18 19 20 21 22
26 27 28 29 30 31          23 24 25 26 27 28          23 24 25 26 27 28 29
                                                      30 31

        April                       May                       June
Su Mo Tu We Th Fr Sa       Su Mo Tu We Th Fr Sa       Su Mo Tu We Th Fr Sa
       1  2  3  4  5                    1  2  3        1  2  3  4  5  6  7
 6  7  8  9 10 11 12        4  5  6  7  8  9 10        8  9 10 11 12 13 14
13 14 15 16 17 18 19       11 12 13 14 15 16 17       15 16 17 18 19 20 21
20 21 22 23 24 25 26       18 19 20 21 22 23 24       22 23 24 25 26 27 28
27 28 29 30                25 26 27 28 29 30 31       29 30


        July                      August                    September
Su Mo Tu We Th Fr Sa       Su Mo Tu We Th Fr Sa       Su Mo Tu We Th Fr Sa
       1  2  3  4  5                       1  2           1  2  3  4  5  6
 6  7  8  9 10 11 12        3  4  5  6  7  8  9        7  8  9 10 11 12 13
13 14 15 16 17 18 19       10 11 12 13 14 15 16       14 15 16 17 18 19 20
20 21 22 23 24 25 26       17 18 19 20 21 22 23       21 22 23 24 25 26 27
27 28 29 30 31             24 25 26 27 28 29 30       28 29 30
                           31

       October                   November                   December
Su Mo Tu We Th Fr Sa       Su Mo Tu We Th Fr Sa       Su Mo Tu We Th Fr Sa
          1  2  3  4                          1           1  2  3  4  5  6
 5  6  7  8  9 10 11        2  3  4  5  6  7  8        7  8  9 10 11 12 13
12 13 14 15 16 17 18        9 10 11 12 13 14 15       14 15 16 17 18 19 20
19 20 21 22 23 24 25       16 17 18 19 20 21 22       21 22 23 24 25 26 27
26 27 28 29 30 31          23 24 25 26 27 28 29       28 29 30 31
                           30
"
```



## Factor


```factor
USING: arrays calendar.format grouping io.streams.string kernel
math.ranges prettyprint sequences sequences.interleaved ;
IN: rosetta-code.calendar

: calendar ( year -- )
    12 [1,b] [ 2array [ month. ] with-string-writer ] with map
    3 <groups> [ "   " <interleaved> ] map 5 " " <repetition>
    <interleaved> simple-table. ;

: calendar-demo ( -- ) 1969 calendar ;

MAIN: calendar-demo
```

{{out}}

```txt

January 1969             February 1969            March 1969
Su Mo Tu We Th Fr Sa     Su Mo Tu We Th Fr Sa     Su Mo Tu We Th Fr Sa
          1  2  3  4                        1                        1
 5  6  7  8  9 10 11      2  3  4  5  6  7  8      2  3  4  5  6  7  8
12 13 14 15 16 17 18      9 10 11 12 13 14 15      9 10 11 12 13 14 15
19 20 21 22 23 24 25     16 17 18 19 20 21 22     16 17 18 19 20 21 22
26 27 28 29 30 31        23 24 25 26 27 28        23 24 25 26 27 28 29
                                                  30 31

April 1969               May 1969                 June 1969
Su Mo Tu We Th Fr Sa     Su Mo Tu We Th Fr Sa     Su Mo Tu We Th Fr Sa
       1  2  3  4  5                  1  2  3      1  2  3  4  5  6  7
 6  7  8  9 10 11 12      4  5  6  7  8  9 10      8  9 10 11 12 13 14
13 14 15 16 17 18 19     11 12 13 14 15 16 17     15 16 17 18 19 20 21
20 21 22 23 24 25 26     18 19 20 21 22 23 24     22 23 24 25 26 27 28
27 28 29 30              25 26 27 28 29 30 31     29 30


July 1969                August 1969              September 1969
Su Mo Tu We Th Fr Sa     Su Mo Tu We Th Fr Sa     Su Mo Tu We Th Fr Sa
       1  2  3  4  5                     1  2         1  2  3  4  5  6
 6  7  8  9 10 11 12      3  4  5  6  7  8  9      7  8  9 10 11 12 13
13 14 15 16 17 18 19     10 11 12 13 14 15 16     14 15 16 17 18 19 20
20 21 22 23 24 25 26     17 18 19 20 21 22 23     21 22 23 24 25 26 27
27 28 29 30 31           24 25 26 27 28 29 30     28 29 30
                         31

October 1969             November 1969            December 1969
Su Mo Tu We Th Fr Sa     Su Mo Tu We Th Fr Sa     Su Mo Tu We Th Fr Sa
          1  2  3  4                        1         1  2  3  4  5  6
 5  6  7  8  9 10 11      2  3  4  5  6  7  8      7  8  9 10 11 12 13
12 13 14 15 16 17 18      9 10 11 12 13 14 15     14 15 16 17 18 19 20
19 20 21 22 23 24 25     16 17 18 19 20 21 22     21 22 23 24 25 26 27
26 27 28 29 30 31        23 24 25 26 27 28 29     28 29 30 31
                         30

```



## FutureBasic


```futurebasic

include "ConsoleWindow"

dim as Str255 a

open "UNIX", 1,"cal 1969"
  do
    line input #1, a
    print a
  until eof(1)
close 1

```


Output:

```txt

                             1969

      January               February               March
Su Mo Tu We Th Fr Sa  Su Mo Tu We Th Fr Sa  Su Mo Tu We Th Fr Sa
          1  2  3  4                     1                     1
 5  6  7  8  9 10 11   2  3  4  5  6  7  8   2  3  4  5  6  7  8
12 13 14 15 16 17 18   9 10 11 12 13 14 15   9 10 11 12 13 14 15
19 20 21 22 23 24 25  16 17 18 19 20 21 22  16 17 18 19 20 21 22
26 27 28 29 30 31     23 24 25 26 27 28     23 24 25 26 27 28 29
                                            30 31
       April                  May                   June
Su Mo Tu We Th Fr Sa  Su Mo Tu We Th Fr Sa  Su Mo Tu We Th Fr Sa
       1  2  3  4  5               1  2  3   1  2  3  4  5  6  7
 6  7  8  9 10 11 12   4  5  6  7  8  9 10   8  9 10 11 12 13 14
13 14 15 16 17 18 19  11 12 13 14 15 16 17  15 16 17 18 19 20 21
20 21 22 23 24 25 26  18 19 20 21 22 23 24  22 23 24 25 26 27 28
27 28 29 30           25 26 27 28 29 30 31  29 30

        July                 August              September
Su Mo Tu We Th Fr Sa  Su Mo Tu We Th Fr Sa  Su Mo Tu We Th Fr Sa
       1  2  3  4  5                  1  2      1  2  3  4  5  6
 6  7  8  9 10 11 12   3  4  5  6  7  8  9   7  8  9 10 11 12 13
13 14 15 16 17 18 19  10 11 12 13 14 15 16  14 15 16 17 18 19 20
20 21 22 23 24 25 26  17 18 19 20 21 22 23  21 22 23 24 25 26 27
27 28 29 30 31        24 25 26 27 28 29 30  28 29 30
                      31
      October               November              December
Su Mo Tu We Th Fr Sa  Su Mo Tu We Th Fr Sa  Su Mo Tu We Th Fr Sa
          1  2  3  4                     1      1  2  3  4  5  6
 5  6  7  8  9 10 11   2  3  4  5  6  7  8   7  8  9 10 11 12 13
12 13 14 15 16 17 18   9 10 11 12 13 14 15  14 15 16 17 18 19 20
19 20 21 22 23 24 25  16 17 18 19 20 21 22  21 22 23 24 25 26 27
26 27 28 29 30 31     23 24 25 26 27 28 29  28 29 30 31
                      30

```



## Gambas


```gambas
Public Sub Main()

Shell "cal 1969"

End
```

Output:

```txt

                            1969
      January               February               March
Su Mo Tu We Th Fr Sa  Su Mo Tu We Th Fr Sa  Su Mo Tu We Th Fr Sa
          1  2  3  4                     1                     1
 5  6  7  8  9 10 11   2  3  4  5  6  7  8   2  3  4  5  6  7  8
12 13 14 15 16 17 18   9 10 11 12 13 14 15   9 10 11 12 13 14 15
19 20 21 22 23 24 25  16 17 18 19 20 21 22  16 17 18 19 20 21 22
26 27 28 29 30 31     23 24 25 26 27 28     23 24 25 26 27 28 29
                                            30 31

       April                  May                   June
Su Mo Tu We Th Fr Sa  Su Mo Tu We Th Fr Sa  Su Mo Tu We Th Fr Sa
       1  2  3  4  5               1  2  3   1  2  3  4  5  6  7
 6  7  8  9 10 11 12   4  5  6  7  8  9 10   8  9 10 11 12 13 14
13 14 15 16 17 18 19  11 12 13 14 15 16 17  15 16 17 18 19 20 21
20 21 22 23 24 25 26  18 19 20 21 22 23 24  22 23 24 25 26 27 28
27 28 29 30           25 26 27 28 29 30 31  29 30


        July                 August              September
Su Mo Tu We Th Fr Sa  Su Mo Tu We Th Fr Sa  Su Mo Tu We Th Fr Sa
       1  2  3  4  5                  1  2      1  2  3  4  5  6
 6  7  8  9 10 11 12   3  4  5  6  7  8  9   7  8  9 10 11 12 13
13 14 15 16 17 18 19  10 11 12 13 14 15 16  14 15 16 17 18 19 20
20 21 22 23 24 25 26  17 18 19 20 21 22 23  21 22 23 24 25 26 27
27 28 29 30 31        24 25 26 27 28 29 30  28 29 30
                      31

      October               November              December
Su Mo Tu We Th Fr Sa  Su Mo Tu We Th Fr Sa  Su Mo Tu We Th Fr Sa
          1  2  3  4                     1      1  2  3  4  5  6
 5  6  7  8  9 10 11   2  3  4  5  6  7  8   7  8  9 10 11 12 13
12 13 14 15 16 17 18   9 10 11 12 13 14 15  14 15 16 17 18 19 20
19 20 21 22 23 24 25  16 17 18 19 20 21 22  21 22 23 24 25 26 27
26 27 28 29 30 31     23 24 25 26 27 28 29  28 29 30 31
                      30

```



## Go


```Go
package main

import (
	"fmt"
	"time"
)

const pageWidth = 80

func main() {
	printCal(1969)
}

func printCal(year int) {
	thisDate := time.Date(year, 1, 1, 1, 1, 1, 1, time.UTC)
	var (
		dayArr                  [12][7][6]int // month, weekday, week
		month, lastMonth        time.Month
		weekInMonth, dayInMonth int
	)
	for thisDate.Year() == year {
		if month = thisDate.Month(); month != lastMonth {
			weekInMonth = 0
			dayInMonth = 1
		}
		weekday := thisDate.Weekday()
		if weekday == 0 && dayInMonth > 1 {
			weekInMonth++
		}
		dayArr[int(month)-1][weekday][weekInMonth] = thisDate.Day()
		lastMonth = month
		dayInMonth++
		thisDate = thisDate.Add(time.Hour * 24)
	}
	centre := fmt.Sprintf("%d", pageWidth/2)
	fmt.Printf("%"+centre+"s\n\n", "[SNOOPY]")
	centre = fmt.Sprintf("%d", pageWidth/2-2)
	fmt.Printf("%"+centre+"d\n\n", year)
	months := [12]string{
		" January ", " February", "  March  ", "  April  ",
		"   May   ", "   June  ", "   July  ", "  August ",
		"September", " October ", " November", " December"}
	days := [7]string{"Su", "Mo", "Tu", "We", "Th", "Fr", "Sa"}
	for qtr := 0; qtr < 4; qtr++ {
		for monthInQtr := 0; monthInQtr < 3; monthInQtr++ { // Month names
			fmt.Printf("      %s           ", months[qtr*3+monthInQtr])
		}
		fmt.Println()
		for monthInQtr := 0; monthInQtr < 3; monthInQtr++ { // Day names
			for day := 0; day < 7; day++ {
				fmt.Printf(" %s", days[day])
			}
			fmt.Printf("     ")
		}
		fmt.Println()
		for weekInMonth = 0; weekInMonth < 6; weekInMonth++ {
			for monthInQtr := 0; monthInQtr < 3; monthInQtr++ {
				for day := 0; day < 7; day++ {
					if dayArr[qtr*3+monthInQtr][day][weekInMonth] == 0 {
						fmt.Printf("   ")
					} else {
						fmt.Printf("%3d", dayArr[qtr*3+monthInQtr][day][weekInMonth])
					}
				}
				fmt.Printf("     ")
			}
			fmt.Println()
		}
		fmt.Println()
	}
}
```

Output:

```txt
                                [SNOOPY]

                                  1969

       January                   February                   March
 Su Mo Tu We Th Fr Sa      Su Mo Tu We Th Fr Sa      Su Mo Tu We Th Fr Sa
           1  2  3  4                         1                         1
  5  6  7  8  9 10 11       2  3  4  5  6  7  8       2  3  4  5  6  7  8
 12 13 14 15 16 17 18       9 10 11 12 13 14 15       9 10 11 12 13 14 15
 19 20 21 22 23 24 25      16 17 18 19 20 21 22      16 17 18 19 20 21 22
 26 27 28 29 30 31         23 24 25 26 27 28         23 24 25 26 27 28 29
                                                     30 31

        April                      May                       June
 Su Mo Tu We Th Fr Sa      Su Mo Tu We Th Fr Sa      Su Mo Tu We Th Fr Sa
        1  2  3  4  5                   1  2  3       1  2  3  4  5  6  7
  6  7  8  9 10 11 12       4  5  6  7  8  9 10       8  9 10 11 12 13 14
 13 14 15 16 17 18 19      11 12 13 14 15 16 17      15 16 17 18 19 20 21
 20 21 22 23 24 25 26      18 19 20 21 22 23 24      22 23 24 25 26 27 28
 27 28 29 30               25 26 27 28 29 30 31      29 30


         July                     August                  September
 Su Mo Tu We Th Fr Sa      Su Mo Tu We Th Fr Sa      Su Mo Tu We Th Fr Sa
        1  2  3  4  5                      1  2          1  2  3  4  5  6
  6  7  8  9 10 11 12       3  4  5  6  7  8  9       7  8  9 10 11 12 13
 13 14 15 16 17 18 19      10 11 12 13 14 15 16      14 15 16 17 18 19 20
 20 21 22 23 24 25 26      17 18 19 20 21 22 23      21 22 23 24 25 26 27
 27 28 29 30 31            24 25 26 27 28 29 30      28 29 30
                           31

       October                   November                  December
 Su Mo Tu We Th Fr Sa      Su Mo Tu We Th Fr Sa      Su Mo Tu We Th Fr Sa
           1  2  3  4                         1          1  2  3  4  5  6
  5  6  7  8  9 10 11       2  3  4  5  6  7  8       7  8  9 10 11 12 13
 12 13 14 15 16 17 18       9 10 11 12 13 14 15      14 15 16 17 18 19 20
 19 20 21 22 23 24 25      16 17 18 19 20 21 22      21 22 23 24 25 26 27
 26 27 28 29 30 31         23 24 25 26 27 28 29      28 29 30 31
                           30

```



## Haskell


```Haskell
import qualified Data.Text as T
import Data.Time
import Data.Time.Calendar
import Data.Time.Calendar.WeekDate
import Data.List.Split (chunksOf)
import Data.List

data Day = Su | Mo | Tu | We | Th | Fr | Sa
           deriving (Show, Eq, Ord, Enum)

data Month = January | February | March
           | April   | May      | June
           | July    | August   | September
           | October | November | December
             deriving (Show, Eq, Ord, Enum)

monthToInt :: Month -> Int
monthToInt = (+ 1) . fromEnum

dayFromDate :: Integer -> Month -> Int -> Int
dayFromDate year month day = day' `mod` 7
    where (_, _, day') = toWeekDate $ fromGregorian year (monthToInt month) day

nSpaces :: Int -> T.Text
nSpaces n = T.replicate n (T.pack " ")

space :: T.Text
space = nSpaces 1

calMarginWidth = 3

calMargin :: T.Text
calMargin = nSpaces calMarginWidth

calWidth = 20

listMonth :: Integer -> Month -> [T.Text]
listMonth year month = [monthHeader, weekHeader] ++ weeks'
    where
      monthHeader = (T.center calWidth ' ') . T.pack $ show month

      weekHeader = (T.intercalate space) $ map (T.pack . show) [(Su)..]

      monthLength = toInteger $
                    gregorianMonthLength year $
                    monthToInt month

      firstDay = dayFromDate year month 1

      days = replicate firstDay (nSpaces 2) ++
             map ((T.justifyRight 2 ' ') . T.pack . show) [1..monthLength]

      weeks = map (T.justifyLeft calWidth ' ') $
              map (T.intercalate space) $
              chunksOf 7 days

      weeks' = weeks ++ replicate (6 - length weeks) (nSpaces calWidth)

listCalendar :: Integer -> Int -> [[[T.Text]]]
listCalendar year calColumns = (chunksOf calColumns) . (map (listMonth year)) $
                               enumFrom January

calColFromCol :: Int -> Int
calColFromCol columns = c + if r >= calWidth then 1 else 0
    where (c, r) = columns `divMod` (calWidth + calMarginWidth)

colFromCalCol :: Int -> Int
colFromCalCol calCol = calCol * calWidth + ((calCol - 1) * calMarginWidth)

center :: Int -> String -> String
center i a = T.unpack . (T.center i ' ') $ T.pack a

printCal :: [[[T.Text]]] -> IO ()
printCal = mapM_ f where
  f c = mapM_ (putStrLn . T.unpack) rows
    where rows = map (T.intercalate calMargin) $ transpose c

printCalendar :: Integer -> Int -> IO ()
printCalendar year columns =
    if columns < 20
    then putStrLn $ "Cannot print less than 20 columns"
    else do
      putStrLn $ center columns' "[Maybe Snoopy]"
      putStrLn $ center columns' $ show year
      putStrLn ""
      printCal $ listCalendar year calcol'
    where
      calcol' = calColFromCol columns

      columns' = colFromCalCol calcol'
```


```txt
*Main> printCalendar 1969 80
                          [Maybe Snoopy]
                               1969

       January               February                 March
Su Mo Tu We Th Fr Sa   Su Mo Tu We Th Fr Sa   Su Mo Tu We Th Fr Sa
          1  2  3  4                      1                      1
 5  6  7  8  9 10 11    2  3  4  5  6  7  8    2  3  4  5  6  7  8
12 13 14 15 16 17 18    9 10 11 12 13 14 15    9 10 11 12 13 14 15
19 20 21 22 23 24 25   16 17 18 19 20 21 22   16 17 18 19 20 21 22
26 27 28 29 30 31      23 24 25 26 27 28      23 24 25 26 27 28 29
                                              30 31
        April                   May                   June
Su Mo Tu We Th Fr Sa   Su Mo Tu We Th Fr Sa   Su Mo Tu We Th Fr Sa
       1  2  3  4  5                1  2  3    1  2  3  4  5  6  7
 6  7  8  9 10 11 12    4  5  6  7  8  9 10    8  9 10 11 12 13 14
13 14 15 16 17 18 19   11 12 13 14 15 16 17   15 16 17 18 19 20 21
20 21 22 23 24 25 26   18 19 20 21 22 23 24   22 23 24 25 26 27 28
27 28 29 30            25 26 27 28 29 30 31   29 30

        July                  August                September
Su Mo Tu We Th Fr Sa   Su Mo Tu We Th Fr Sa   Su Mo Tu We Th Fr Sa
       1  2  3  4  5                   1  2       1  2  3  4  5  6
 6  7  8  9 10 11 12    3  4  5  6  7  8  9    7  8  9 10 11 12 13
13 14 15 16 17 18 19   10 11 12 13 14 15 16   14 15 16 17 18 19 20
20 21 22 23 24 25 26   17 18 19 20 21 22 23   21 22 23 24 25 26 27
27 28 29 30 31         24 25 26 27 28 29 30   28 29 30
                       31
       October               November               December
Su Mo Tu We Th Fr Sa   Su Mo Tu We Th Fr Sa   Su Mo Tu We Th Fr Sa
          1  2  3  4                      1       1  2  3  4  5  6
 5  6  7  8  9 10 11    2  3  4  5  6  7  8    7  8  9 10 11 12 13
12 13 14 15 16 17 18    9 10 11 12 13 14 15   14 15 16 17 18 19 20
19 20 21 22 23 24 25   16 17 18 19 20 21 22   21 22 23 24 25 26 27
26 27 28 29 30 31      23 24 25 26 27 28 29   28 29 30 31
                       30
```



## Huginn


```huginn
import DateTime as dt;
import Algorithms as algo;
import Text as text;

class Calendar {
  _monthNames_ = none;
  _dayNames_ = none;
  constructor() {
    t = dt.now();
    _monthNames_ = algo.materialize( algo.map( algo.range( 1, 13 ), @[t]( m ) { dt.format( "%B", t.set_date( 1, m, 1 ) ); } ), tuple );
    _dayNames_ = algo.materialize( algo.map( algo.range( 1, 8 ), @[t]( d ) { dt.format( "%a", t.set_date( 1, 1, d ) )[:2]; } ), tuple );
  }
  print_year( year_, cols_ ) {
    t = dt.now();
    print( "{:^66d}\n".format( year_ ) );
    for ( rm : algo.range( 12 / cols_ ) ) {
      m = rm * cols_;
      print( text.repeat( "{:^22s}", cols_ ).format( _monthNames_[m:m + cols_]... ) + "\n" );
      day = [];
      daysInMonth = [];
      for ( mc : algo.range( cols_ ) ) {
        print( " {} {} {} {} {} {} {} ".format( _dayNames_... ) );
        t.set_date( year_, m + mc + 1, 1 );
        day.push( - t.get_day_of_week() + 1 );
        daysInMonth.push( t.get_days_in_month() );
      }
      print( "\n" );
      haveDay = true;
      while ( haveDay ) {
        haveDay = false;
        for ( mc : algo.range( cols_ ) ) {
          for ( d : algo.range( 7 ) ) {
            if ( ( day[mc] > 0 ) && ( day[mc] <= daysInMonth[mc] ) ) {
              print( " {:2d}".format( day[mc] ) );
              haveDay = true;
            } else {
              print( "   " );
            }
            day[mc] += 1;
          }
          print( " " );
        }
        print( "\n" );
      }
    }
  }
}

main( argv_ ) {
  cal = Calendar();
  cols = size( argv_ ) > 2 ? integer( argv_[2] ) : 3;
  if ( 12 % cols != 0 ) {
    cols = 3;
  }
  cal.print_year(
    size( argv_ ) > 1
      ? integer( argv_[1] )
      : dt.now().get_year(),
    cols
  );
}
```


Output:
```txt
                               1969
       January               February               March
 Mo Tu We Th Fr Sa Su  Mo Tu We Th Fr Sa Su  Mo Tu We Th Fr Sa Su
        1  2  3  4  5                  1  2                  1  2
  6  7  8  9 10 11 12   3  4  5  6  7  8  9   3  4  5  6  7  8  9
 13 14 15 16 17 18 19  10 11 12 13 14 15 16  10 11 12 13 14 15 16
 20 21 22 23 24 25 26  17 18 19 20 21 22 23  17 18 19 20 21 22 23
 27 28 29 30 31        24 25 26 27 28        24 25 26 27 28 29 30
                                             31

        April                  May                   June
 Mo Tu We Th Fr Sa Su  Mo Tu We Th Fr Sa Su  Mo Tu We Th Fr Sa Su
     1  2  3  4  5  6            1  2  3  4                     1
  7  8  9 10 11 12 13   5  6  7  8  9 10 11   2  3  4  5  6  7  8
 14 15 16 17 18 19 20  12 13 14 15 16 17 18   9 10 11 12 13 14 15
 21 22 23 24 25 26 27  19 20 21 22 23 24 25  16 17 18 19 20 21 22
 28 29 30              26 27 28 29 30 31     23 24 25 26 27 28 29
                                             30

         July                 August              September
 Mo Tu We Th Fr Sa Su  Mo Tu We Th Fr Sa Su  Mo Tu We Th Fr Sa Su
     1  2  3  4  5  6               1  2  3   1  2  3  4  5  6  7
  7  8  9 10 11 12 13   4  5  6  7  8  9 10   8  9 10 11 12 13 14
 14 15 16 17 18 19 20  11 12 13 14 15 16 17  15 16 17 18 19 20 21
 21 22 23 24 25 26 27  18 19 20 21 22 23 24  22 23 24 25 26 27 28
 28 29 30 31           25 26 27 28 29 30 31  29 30

       October               November              December
 Mo Tu We Th Fr Sa Su  Mo Tu We Th Fr Sa Su  Mo Tu We Th Fr Sa Su
        1  2  3  4  5                  1  2   1  2  3  4  5  6  7
  6  7  8  9 10 11 12   3  4  5  6  7  8  9   8  9 10 11 12 13 14
 13 14 15 16 17 18 19  10 11 12 13 14 15 16  15 16 17 18 19 20 21
 20 21 22 23 24 25 26  17 18 19 20 21 22 23  22 23 24 25 26 27 28
 27 28 29 30 31        24 25 26 27 28 29 30  29 30 31
```


=={{header|Icon}} and {{header|Unicon}}==
The procedures ''printCalendar'' handles formatting of large components and uses co-expressions to keep the formatting of week elements in each column synchronized. The procedure ''CalendarFormatWeek'' is a generator that returns heading elements, alignment spacing, and individual days.

```Icon
procedure main(A)
   printCalendar(\A[1]|1969)
end

procedure printCalendar(year)          #: Print a 3 column x 80 char calendar
   cols := 3                                        # fixed width
   mons := []                                       # table of months
   "January February March April May June " ||
   "July August September October November December " ?
          while put(mons, tab(find(" "))) do move(1)

   write(center("[Snoopy Picture]",cols * 24 + 4))  # mandatory ..
   write(center(year,cols * 24 + 4), "\n")          # ... headers

   M := list(cols)                                  # coexpr container
   every  mon := 0 to 9 by cols do {                # go through months by cols
      writes("    ")
      every i := 1 to cols do {
         writes(center(mons[mon+i],24))             # header months
         M[i] := create CalendarFormatWeek(year,mon + i)  # formatting coexpr
         }
      write()
      every 1 to 7 do {                             # 1 to max rows
         every c := 1 to cols do {                  # for each column
            writes("    ")
            every 1 to 7 do writes(right(@M[c],3))  # each row day element
            }
         write()
         }
      }
   return
end

link datetime

procedure CalendarFormatWeek(year,m)                #: Format Week for Calendar
   static D
   initial D := [31,28,31,30,31,30,31,31,30,31,30,31]

   every suspend "Su"|"Mo"|"Tu"|"We"|"Th"|"Fr"|"Sa"          # header
   every 1 to (d := (julian(m,1,year)+1)%7) do suspend ""    # lead day alignment
   every suspend 1 to D[m] do d +:= 1                        # days
   if m = 2 & IsLeapYear(year) then suspend (d +:= 1, 29)    # LY adjustment
   every d to (6*7) do suspend ""                            # trailer alignment
end
```


{{libheader|Icon Programming Library}}
[http://www.cs.arizona.edu/icon/library/src/procs/datetime.icn datetime.icn provides julian and IsLeapYear]

Output:
```txt
                              [Snoopy Picture]
                                    1969

            January                 February                 March
     Su Mo Tu We Th Fr Sa     Su Mo Tu We Th Fr Sa     Su Mo Tu We Th Fr Sa
               1  2  3  4                        1                        1
      5  6  7  8  9 10 11      2  3  4  5  6  7  8      2  3  4  5  6  7  8
     12 13 14 15 16 17 18      9 10 11 12 13 14 15      9 10 11 12 13 14 15
     19 20 21 22 23 24 25     16 17 18 19 20 21 22     16 17 18 19 20 21 22
     26 27 28 29 30 31        23 24 25 26 27 28        23 24 25 26 27 28 29
                                                       30 31
             April                    May                     June
     Su Mo Tu We Th Fr Sa     Su Mo Tu We Th Fr Sa     Su Mo Tu We Th Fr Sa
            1  2  3  4  5                  1  2  3      1  2  3  4  5  6  7
      6  7  8  9 10 11 12      4  5  6  7  8  9 10      8  9 10 11 12 13 14
     13 14 15 16 17 18 19     11 12 13 14 15 16 17     15 16 17 18 19 20 21
     20 21 22 23 24 25 26     18 19 20 21 22 23 24     22 23 24 25 26 27 28
     27 28 29 30              25 26 27 28 29 30 31     29 30

              July                   August                September
     Su Mo Tu We Th Fr Sa     Su Mo Tu We Th Fr Sa     Su Mo Tu We Th Fr Sa
            1  2  3  4  5                     1  2         1  2  3  4  5  6
      6  7  8  9 10 11 12      3  4  5  6  7  8  9      7  8  9 10 11 12 13
     13 14 15 16 17 18 19     10 11 12 13 14 15 16     14 15 16 17 18 19 20
     20 21 22 23 24 25 26     17 18 19 20 21 22 23     21 22 23 24 25 26 27
     27 28 29 30 31           24 25 26 27 28 29 30     28 29 30
                              31
            October                 November                December
     Su Mo Tu We Th Fr Sa     Su Mo Tu We Th Fr Sa     Su Mo Tu We Th Fr Sa
               1  2  3  4                        1         1  2  3  4  5  6
      5  6  7  8  9 10 11      2  3  4  5  6  7  8      7  8  9 10 11 12 13
     12 13 14 15 16 17 18      9 10 11 12 13 14 15     14 15 16 17 18 19 20
     19 20 21 22 23 24 25     16 17 18 19 20 21 22     21 22 23 24 25 26 27
     26 27 28 29 30 31        23 24 25 26 27 28 29     28 29 30 31
                              30
```



## J

'''Solution:'''

```j
require 'dates format'               NB. J6.x
require 'dates general/misc/format'  NB. J7.x
calBody=: (1 1 }. _1 _1 }. ":)@(-@(<.@%&22)@[ ]\ calendar@])
calTitle=: (<: - 22&|)@[ center '[Insert Snoopy here]' , '' ,:~ ":@]
formatCalendar=: calTitle , calBody
```

'''Example use:'''

```j
   80 formatCalendar 1969
                      [Insert Snoopy here]
                              1969

         Jan         │         Feb         │         Mar
 Su Mo Tu We Th Fr Sa│ Su Mo Tu We Th Fr Sa│ Su Mo Tu We Th Fr Sa
           1  2  3  4│                    1│                    1
  5  6  7  8  9 10 11│  2  3  4  5  6  7  8│  2  3  4  5  6  7  8
 12 13 14 15 16 17 18│  9 10 11 12 13 14 15│  9 10 11 12 13 14 15
 19 20 21 22 23 24 25│ 16 17 18 19 20 21 22│ 16 17 18 19 20 21 22
 26 27 28 29 30 31   │ 23 24 25 26 27 28   │ 23 24 25 26 27 28 29
                     │                     │ 30 31
─────────────────────┼─────────────────────┼─────────────────────
         Apr         │         May         │         Jun
 Su Mo Tu We Th Fr Sa│ Su Mo Tu We Th Fr Sa│ Su Mo Tu We Th Fr Sa
        1  2  3  4  5│              1  2  3│  1  2  3  4  5  6  7
  6  7  8  9 10 11 12│  4  5  6  7  8  9 10│  8  9 10 11 12 13 14
 13 14 15 16 17 18 19│ 11 12 13 14 15 16 17│ 15 16 17 18 19 20 21
 20 21 22 23 24 25 26│ 18 19 20 21 22 23 24│ 22 23 24 25 26 27 28
 27 28 29 30         │ 25 26 27 28 29 30 31│ 29 30
                     │                     │
─────────────────────┼─────────────────────┼─────────────────────
         Jul         │         Aug         │         Sep
 Su Mo Tu We Th Fr Sa│ Su Mo Tu We Th Fr Sa│ Su Mo Tu We Th Fr Sa
        1  2  3  4  5│                 1  2│     1  2  3  4  5  6
  6  7  8  9 10 11 12│  3  4  5  6  7  8  9│  7  8  9 10 11 12 13
 13 14 15 16 17 18 19│ 10 11 12 13 14 15 16│ 14 15 16 17 18 19 20
 20 21 22 23 24 25 26│ 17 18 19 20 21 22 23│ 21 22 23 24 25 26 27
 27 28 29 30 31      │ 24 25 26 27 28 29 30│ 28 29 30
                     │ 31                  │
─────────────────────┼─────────────────────┼─────────────────────
         Oct         │         Nov         │         Dec
 Su Mo Tu We Th Fr Sa│ Su Mo Tu We Th Fr Sa│ Su Mo Tu We Th Fr Sa
           1  2  3  4│                    1│     1  2  3  4  5  6
  5  6  7  8  9 10 11│  2  3  4  5  6  7  8│  7  8  9 10 11 12 13
 12 13 14 15 16 17 18│  9 10 11 12 13 14 15│ 14 15 16 17 18 19 20
 19 20 21 22 23 24 25│ 16 17 18 19 20 21 22│ 21 22 23 24 25 26 27
 26 27 28 29 30 31   │ 23 24 25 26 27 28 29│ 28 29 30 31
                     │ 30                  │
```



## Java

{{trans|D}}

```java
import java.text.*;
import java.util.*;

public class CalendarTask {

    public static void main(String[] args) {
        printCalendar(1969, 3);
    }

    static void printCalendar(int year, int nCols) {
        if (nCols < 1 || nCols > 12)
            throw new IllegalArgumentException("Illegal column width.");

        Calendar date = new GregorianCalendar(year, 0, 1);

        int nRows = (int) Math.ceil(12.0 / nCols);
        int offs = date.get(Calendar.DAY_OF_WEEK) - 1;
        int w = nCols * 24;

        String[] monthNames = new DateFormatSymbols(Locale.US).getMonths();

        String[][] mons = new String[12][8];
        for (int m = 0; m < 12; m++) {

            String name = monthNames[m];
            int len = 11 + name.length() / 2;
            String format = MessageFormat.format("%{0}s%{1}s", len, 21 - len);

            mons[m][0] = String.format(format, name, "");
            mons[m][1] = " Su Mo Tu We Th Fr Sa";
            int dim = date.getActualMaximum(Calendar.DAY_OF_MONTH);

            for (int d = 1; d < 43; d++) {
                boolean isDay = d > offs && d <= offs + dim;
                String entry = isDay ? String.format(" %2s", d - offs) : "   ";
                if (d % 7 == 1)
                    mons[m][2 + (d - 1) / 7] = entry;
                else
                    mons[m][2 + (d - 1) / 7] += entry;
            }
            offs = (offs + dim) % 7;
            date.add(Calendar.MONTH, 1);
        }

        System.out.printf("%" + (w / 2 + 10) + "s%n", "[Snoopy Picture]");
        System.out.printf("%" + (w / 2 + 4) + "s%n%n", year);

        for (int r = 0; r < nRows; r++) {
            for (int i = 0; i < 8; i++) {
                for (int c = r * nCols; c < (r + 1) * nCols && c < 12; c++)
                    System.out.printf("   %s", mons[c][i]);
                System.out.println();
            }
            System.out.println();
        }
    }
}
```


{{out}}

```txt

                              [Snoopy Picture]
                                    1969

          January                 February                 March
    Su Mo Tu We Th Fr Sa    Su Mo Tu We Th Fr Sa    Su Mo Tu We Th Fr Sa
              1  2  3  4                       1                       1
     5  6  7  8  9 10 11     2  3  4  5  6  7  8     2  3  4  5  6  7  8
    12 13 14 15 16 17 18     9 10 11 12 13 14 15     9 10 11 12 13 14 15
    19 20 21 22 23 24 25    16 17 18 19 20 21 22    16 17 18 19 20 21 22
    26 27 28 29 30 31       23 24 25 26 27 28       23 24 25 26 27 28 29
                                                    30 31

           April                    May                     June
    Su Mo Tu We Th Fr Sa    Su Mo Tu We Th Fr Sa    Su Mo Tu We Th Fr Sa
           1  2  3  4  5                 1  2  3     1  2  3  4  5  6  7
     6  7  8  9 10 11 12     4  5  6  7  8  9 10     8  9 10 11 12 13 14
    13 14 15 16 17 18 19    11 12 13 14 15 16 17    15 16 17 18 19 20 21
    20 21 22 23 24 25 26    18 19 20 21 22 23 24    22 23 24 25 26 27 28
    27 28 29 30             25 26 27 28 29 30 31    29 30


            July                   August                September
    Su Mo Tu We Th Fr Sa    Su Mo Tu We Th Fr Sa    Su Mo Tu We Th Fr Sa
           1  2  3  4  5                    1  2        1  2  3  4  5  6
     6  7  8  9 10 11 12     3  4  5  6  7  8  9     7  8  9 10 11 12 13
    13 14 15 16 17 18 19    10 11 12 13 14 15 16    14 15 16 17 18 19 20
    20 21 22 23 24 25 26    17 18 19 20 21 22 23    21 22 23 24 25 26 27
    27 28 29 30 31          24 25 26 27 28 29 30    28 29 30
                            31

          October                 November                December
    Su Mo Tu We Th Fr Sa    Su Mo Tu We Th Fr Sa    Su Mo Tu We Th Fr Sa
              1  2  3  4                       1        1  2  3  4  5  6
     5  6  7  8  9 10 11     2  3  4  5  6  7  8     7  8  9 10 11 12 13
    12 13 14 15 16 17 18     9 10 11 12 13 14 15    14 15 16 17 18 19 20
    19 20 21 22 23 24 25    16 17 18 19 20 21 22    21 22 23 24 25 26 27
    26 27 28 29 30 31       23 24 25 26 27 28 29    28 29 30 31
                            30
```


## Julia


```julia

using Dates

const pagesizes = Dict( "lpr" => [132, 66], "tn3270" => [80, 43])

pagefit(prn) = haskey(pagesizes, prn) ?
    [div(pagesizes[prn][1], 22), div(pagesizes[prn][2], 12)] : [1, 1]
pagecols(prn) = haskey(pagesizes, prn) ? pagesizes[prn][1] : 20

function centerobject(x, cols)
    content = string(x)
    rpad(lpad(content, div(cols + length(content), 2)), cols)
end

function ljustlines(x, cols)
    arr = Vector{String}()
    for s in split(x, "\n")
        push!(arr, rpad(s, cols)[1:cols])
    end
    join(arr, "\n")
end

function formatmonth(yr, mo)
    dt = Date("$yr-$mo-01")
    dayofweekfirst = dayofweek(dt)
    numweeklines = 1
    str = centerobject(monthname(dt), 20) * "\nMo Tu We Th Fr Sa Su\n"
    str *= " " ^ (3 * (dayofweekfirst - 1)) * lpad(string(1), 2)
    for i = 2:daysinmonth(dt)
        if (i + dayofweekfirst + 5) % 7 == 0
            str *= "\n" * lpad(i, 2)
            numweeklines += 1
        else
            str *= lpad(string(i), 3)
        end
    end
    str *= numweeklines < 6 ? "\n\n\n" : "\n\n"
    ljustlines(str, 20)
end

function formatyear(displayyear, printertype)
    calmonths = [formatmonth(displayyear, mo) for mo in 1:12]
    columns = pagecols(printertype)
    monthsperline = pagefit(printertype)[1]
    joinspaces = max( (monthsperline > 1) ?
        div(columns - monthsperline * 20, monthsperline - 1) : 1, 1)
    str = "\n" * centerobject(displayyear, columns) * "\n"
    monthcal = [split(formatmonth(displayyear, i), "\n") for i in 1:12]
    for i in 1:monthsperline:length(calmonths) - 1
        for j in 1:length(monthcal[1])
            monthlines = map(x->monthcal[x][j], i:i + monthsperline - 1)
            str *= rpad(join(monthlines, " " ^ joinspaces), columns) * "\n"
        end
        str *= "\n"
    end
    str
end

function lineprintcalendar(years)
    for year in years, printer in keys(pagesizes)
        println(formatyear(year, printer))
    end
end

lineprintcalendar(1969)

```



## Kotlin

{{trans|D}}

```scala
import java.io.PrintStream
import java.text.DateFormatSymbols
import java.text.MessageFormat
import java.util.Calendar
import java.util.GregorianCalendar
import java.util.Locale

internal fun PrintStream.printCalendar(year: Int, nCols: Byte, locale: Locale?) {
    if (nCols < 1 || nCols > 12)
        throw IllegalArgumentException("Illegal column width.")
    val w = nCols * 24
    val nRows = Math.ceil(12.0 / nCols).toInt()

    val date = GregorianCalendar(year, 0, 1)
    var offs = date.get(Calendar.DAY_OF_WEEK) - 1

    val days = DateFormatSymbols(locale).shortWeekdays.slice(1..7).map { it.slice(0..1) }.joinToString(" ", " ")
    val mons = Array(12) { Array(8) { "" } }
    DateFormatSymbols(locale).months.slice(0..11).forEachIndexed { m, name ->
        val len = 11 + name.length / 2
        val format = MessageFormat.format("%{0}s%{1}s", len, 21 - len)
        mons[m][0] = String.format(format, name, "")
        mons[m][1] = days
        val dim = date.getActualMaximum(Calendar.DAY_OF_MONTH)
        for (d in 1..42) {
            val isDay = d > offs && d <= offs + dim
            val entry = if (isDay) String.format(" %2s", d - offs) else "   "
            if (d % 7 == 1)
                mons[m][2 + (d - 1) / 7] = entry
            else
                mons[m][2 + (d - 1) / 7] += entry
        }
        offs = (offs + dim) % 7
        date.add(Calendar.MONTH, 1)
    }

    printf("%" + (w / 2 + 10) + "s%n", "[Snoopy Picture]")
    printf("%" + (w / 2 + 4) + "s%n%n", year)

    for (r in 0 until nRows) {
        for (i in 0..7) {
            var c = r * nCols
            while (c < (r + 1) * nCols && c < 12) {
                printf("   %s", mons[c][i])
                c++
            }
            println()
        }
        println()
    }
}

fun main(args: Array<String>) {
    System.out.printCalendar(1969, 3, Locale.US)
}
```

{{out}}
See D output.


## Lingo


```lingo
----------------------------------------
-- @desc      Class "Calendar"
-- @file      parent script "Calendar"
----------------------------------------
property _months
property _weekdayStr
property _refDateObj
property _year
property _calStr

on new (me)
  me._months = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
  me._weekdayStr = "Mo Tu We Th Fr Sa Su"
  me._refDateObj = date(1905,1,2)
  return me
end

on make (me, year)
  me._year = year
  me._calStr = ""
  -- prefill cal string with spaces
  emptyLine = bytearray(68,32).readRawString(68)&RETURN
  repeat with i = 1 to 38
    put emptyLine after _calStr
  end repeat
  me._write (string(year), 32, 1)
  repeat with i = 1 to 12
    me._writeMonth(i)
  end repeat
  return me._calStr
end

on _writeMonth (me, monthNum)
  xOffset = (monthNum-1) mod 3 * 24
  yOffset = (monthNum-1)/3 * 9 + 2
  pre = (20 - me._months[monthNum].length)/2
  me._write(me._months[monthNum], 1+xOffset+pre, 1+yOffset)
  me._write(me._weekdayStr, 1+xOffset, 2+yOffset)
  y = 3
  x = ((date(me._year, monthNum, 1) - me._refDateObj) mod 7)+1
  repeat with i = 1 to 31
    if date(me._year, monthNum, i).month<>monthNum then exit repeat
    dayStr = string(i)
    if i<10 then put " " before dayStr
    me._write(dayStr, x*3-2+xOffset, y+yOffset)
    if x=7 then
      y = y+1
      x = 1
    else
      x = x+1
    end if
  end repeat
end

on _write (me, str, x, y)
  put str into char x to x+str.length-1 of line y of _calStr
end
```


Usage:

```lingo
calObj = script("Calendar").new()
calStr = calObj.make(1969)
put calStr
```


{{out}}

```txt

                               1969

      January                 February                 March
Mo Tu We Th Fr Sa Su    Mo Tu We Th Fr Sa Su    Mo Tu We Th Fr Sa Su
       1  2  3  4  5                    1  2                    1  2
 6  7  8  9 10 11 12     3  4  5  6  7  8  9     3  4  5  6  7  8  9
13 14 15 16 17 18 19    10 11 12 13 14 15 16    10 11 12 13 14 15 16
20 21 22 23 24 25 26    17 18 19 20 21 22 23    17 18 19 20 21 22 23
27 28 29 30 31          24 25 26 27 28          24 25 26 27 28 29 30
                                                31

       April                    May                     June
Mo Tu We Th Fr Sa Su    Mo Tu We Th Fr Sa Su    Mo Tu We Th Fr Sa Su
    1  2  3  4  5  6              1  2  3  4                       1
 7  8  9 10 11 12 13     5  6  7  8  9 10 11     2  3  4  5  6  7  8
14 15 16 17 18 19 20    12 13 14 15 16 17 18     9 10 11 12 13 14 15
21 22 23 24 25 26 27    19 20 21 22 23 24 25    16 17 18 19 20 21 22
28 29 30                26 27 28 29 30 31       23 24 25 26 27 28 29
                                                30

        July                   August                September
Mo Tu We Th Fr Sa Su    Mo Tu We Th Fr Sa Su    Mo Tu We Th Fr Sa Su
    1  2  3  4  5  6                 1  2  3     1  2  3  4  5  6  7
 7  8  9 10 11 12 13     4  5  6  7  8  9 10     8  9 10 11 12 13 14
14 15 16 17 18 19 20    11 12 13 14 15 16 17    15 16 17 18 19 20 21
21 22 23 24 25 26 27    18 19 20 21 22 23 24    22 23 24 25 26 27 28
28 29 30 31             25 26 27 28 29 30 31    29 30


      October                 November                December
Mo Tu We Th Fr Sa Su    Mo Tu We Th Fr Sa Su    Mo Tu We Th Fr Sa Su
       1  2  3  4  5                    1  2     1  2  3  4  5  6  7
 6  7  8  9 10 11 12     3  4  5  6  7  8  9     8  9 10 11 12 13 14
13 14 15 16 17 18 19    10 11 12 13 14 15 16    15 16 17 18 19 20 21
20 21 22 23 24 25 26    17 18 19 20 21 22 23    22 23 24 25 26 27 28
27 28 29 30 31          24 25 26 27 28 29 30    29 30 31

```



## Lua


```Lua
function print_cal(year)
  local months={"JANUARY","FEBRUARY","MARCH","APRIL","MAY","JUNE",
                "JULY","AUGUST","SEPTEMBER","OCTOBER","NOVEMBER","DECEMBER"}
  local daysTitle="MO TU WE TH FR SA SU"
  local daysPerMonth={31,28,31,30,31,30,31,31,30,31,30,31}
  local startday=((year-1)*365+math.floor((year-1)/4)-math.floor((year-1)/100)+math.floor((year-1)/400))%7
  if year%4==0 and year%100~=0 or year%400==0 then
    daysPerMonth[2]=29
  end
  local sep=5
  local monthwidth=daysTitle:len()
  local calwidth=3*monthwidth+2*sep

  function center(str, width)
    local fill1=math.floor((width-str:len())/2)
    local fill2=width-str:len()-fill1
    return string.rep(" ",fill1)..str..string.rep(" ",fill2)
  end

  function makeMonth(name, skip,days)
    local cal={
      center(name,monthwidth),
      daysTitle
    }
    local curday=1-skip
    while #cal<9 do
      line={}
      for i=1,7 do
        if curday<1 or curday>days then
          line[i]="  "
        else
          line[i]=string.format("%2d",curday)
        end
        curday=curday+1
      end
      cal[#cal+1]=table.concat(line," ")
    end
    return cal
  end

  local calendar={}
  for i,month in ipairs(months) do
    local dpm=daysPerMonth[i]
    calendar[i]=makeMonth(month, startday, dpm)
    startday=(startday+dpm)%7
  end


  print(center("[SNOOPY]",calwidth),"\n")
  print(center("--- "..year.." ---",calwidth),"\n")

  for q=0,3 do
    for l=1,9 do
      line={}
      for m=1,3 do
        line[m]=calendar[q*3+m][l]
      end
      print(table.concat(line,string.rep(" ",sep)))
    end
  end
end

print_cal(1969)
```

{{out}}

```txt

                               [SNOOPY]

                             --- 1969 ---

      JANUARY                  FEBRUARY                  MARCH
MO TU WE TH FR SA SU     MO TU WE TH FR SA SU     MO TU WE TH FR SA SU
       1  2  3  4  5                     1  2                     1  2
 6  7  8  9 10 11 12      3  4  5  6  7  8  9      3  4  5  6  7  8  9
13 14 15 16 17 18 19     10 11 12 13 14 15 16     10 11 12 13 14 15 16
20 21 22 23 24 25 26     17 18 19 20 21 22 23     17 18 19 20 21 22 23
27 28 29 30 31           24 25 26 27 28           24 25 26 27 28 29 30
                                                  31

       APRIL                     MAY                      JUNE
MO TU WE TH FR SA SU     MO TU WE TH FR SA SU     MO TU WE TH FR SA SU
    1  2  3  4  5  6               1  2  3  4                        1
 7  8  9 10 11 12 13      5  6  7  8  9 10 11      2  3  4  5  6  7  8
14 15 16 17 18 19 20     12 13 14 15 16 17 18      9 10 11 12 13 14 15
21 22 23 24 25 26 27     19 20 21 22 23 24 25     16 17 18 19 20 21 22
28 29 30                 26 27 28 29 30 31        23 24 25 26 27 28 29
                                                  30

        JULY                    AUGUST                 SEPTEMBER
MO TU WE TH FR SA SU     MO TU WE TH FR SA SU     MO TU WE TH FR SA SU
    1  2  3  4  5  6                  1  2  3      1  2  3  4  5  6  7
 7  8  9 10 11 12 13      4  5  6  7  8  9 10      8  9 10 11 12 13 14
14 15 16 17 18 19 20     11 12 13 14 15 16 17     15 16 17 18 19 20 21
21 22 23 24 25 26 27     18 19 20 21 22 23 24     22 23 24 25 26 27 28
28 29 30 31              25 26 27 28 29 30 31     29 30


      OCTOBER                  NOVEMBER                 DECEMBER
MO TU WE TH FR SA SU     MO TU WE TH FR SA SU     MO TU WE TH FR SA SU
       1  2  3  4  5                     1  2      1  2  3  4  5  6  7
 6  7  8  9 10 11 12      3  4  5  6  7  8  9      8  9 10 11 12 13 14
13 14 15 16 17 18 19     10 11 12 13 14 15 16     15 16 17 18 19 20 21
20 21 22 23 24 25 26     17 18 19 20 21 22 23     22 23 24 25 26 27 28
27 28 29 30 31           24 25 26 27 28 29 30     29 30 31

```



## M2000 Interpreter

Set console to 80 character by 43 lines. Produce calendar line by line, three columns of months.

To simulate printing in paper we print at the bottom line, so every new line scroll up the console screen.

Module Calendar get the Year and the Language Id (1033 for English, 1032 for Greek and any other which support the Window OS).


```M2000 Interpreter

Module Calendar (Year, LocaleId) {
      Function GetMax(Year, Month) {
            a=date(str$(Year)+"-"+str$(Month)+"-1")
            max=32
            do {
                  max--
                  m=val(str$(cdate(a,0,0,max), "m"))
            } until m=Month
             =max+1
      }
      Function SkipMo(Year, Month) {
            a=date(str$(Year)+"-"+str$(Month)+"-1")
            =(val(str$(a, "w"))-8) mod 7 +7
      }
      Function Title$(a$) {
            =Ucase$(left$(a$,1))+Lcase$(Mid$(a$, 2))
      }
      locale LocaleId
      Cursor 0,Height-1  ' last line, so each new line scroll all lines up
      Print Over $(2), Year
      Print
      For j=0 to 3 {
            Print
            For i=1 to 3 {
                  Month=i+j*3
                  Print  Part @((i-1)*29), $(2,22), Title$(Ucase$(locale$(55+Month)))
            }
            Print
            Dim Skip(1 to 3), Count(1 to 3), D(1 to 3)=1
            For i=1 to 3 {
                  Month=i+j*3
                  if i>1 Then Print String$(" ",8);
                  For k=42 to 48 :Print Title$(Ucase$(Left$(locale$(k),2)));" ";:Next k
                  Skip(i)=SkipMo(Year, Month)
                  Count(i)=GetMax(Year, Month)
            }
            Print
            For i=1 to 3 {
                      if i>1 Then Print String$(" ",8);
                      For k=1 to 7 {
                        skip(i)--
                        if  skip(i)>0 Then Print "   "; :continue
                        Count(i)--
                        Print format$("{0::-2} ", d(i));
                        d(i)++
                  }
            }
            Print
            Print @(0)
            For m=1 to 5 {
                  For i=1 to 3 {
                        if i>1 Then Print String$(" ",8);
                        For k=1 to 7 {
                              Count(i)--
                              if Count(i)<0 Then Print "   "; : Continue
                              Print format$("{0::-2} ", d(i));
                              d(i)++
                        }
                  }
            Print
            }
      }
}
Form 80,43
Calendar 1969, 1033 ' English
k=Key$ ' wait key
Calendar 2018, 1032  ' Greek

```



## Mathematica


Calendar is set of routines for handling calendars; It is built into Mathematica.
We're only going to use it for a two functions, namely,  '''DayOfWeek''' and '''DaysBetween'''.


```Mathematica

Needs["Calendar`"];

```


Monthly calendar takes a year and a month and returns a simply formatted calendar for that month.
It knows about leap years.


```Mathematica

monthlyCalendar[y_, m_] :=
    Module[{
           days = {Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday},
           months = {"January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"},
           d1, shortDays, offset, daysInMonth},

     d1 = DayOfWeek[{y, m, 1}];

     daysInMonth[year_, month_] := DaysBetween[{year, month, 1}, {If[month == 12, year + 1, year],  If[month == 12, 1, month + 1], 1}];

     shortDays = (StringTake[ToString[#], 3] & /@ days);

     offset = d1 /. Thread[days -> Range[0, 6]];

     Grid[
             Prepend[
                     Prepend[
                             Partition[
                                   PadRight[PadLeft[Range[daysInMonth[y, m]], daysInMonth[y, m] + offset, ""],
                                                  36, ""  ],
                                           7],
                                   shortDays],
                                    {months[[m]], SpanFromLeft}]]]

```


'''yearlyCalendar''' prints out calendars for each of the 12 months in the year.


```Mathematica

   yearlyCalendar[y_] := Grid[Partition[Table[monthlyCalendar[y, k], {k, 12}], 3], Spacings -> {4, 2}];

```




## OCaml



```ocaml
#load "unix.cma"

let lang = "en"  (* language: English *)

let usage () =
  Printf.printf "Usage:\n%s\n" Sys.argv.(0)

let month_pattern =
  [
    [  0;  4;  8 ];
    [  1;  5;  9 ];
    [  2;  6; 10 ];
    [  3;  7; 11 ];

    (*
    [  0;  1;  2;  3;  4;  5 ];
    [  6;  7;  8;  9; 10; 11 ];

    [  0;  1;  2;  3 ];
    [  4;  5;  6;  7 ];
    [  8;  9; 10; 11 ];
    *)
  ]

let month_langs = [
  "en", [|
    "January"; "February"; "March"; "April";
    "May"; "June"; "July"; "August"; "September";
    "October"; "November"; "December";
  |];
  "fr", [|
    "janvier"; "février"; "mars"; "avril"; "mai";
    "juin"; "juillet"; "août"; "septembre";
    "octobre"; "novembre"; "décembre";
  |];
]

let days_lang = [
  "en", [| "Monday"; "Tuesday"; "Wednesday";
    "Thursday"; "Friday"; "Saturday"; "Sunday" |];
  "fr", [| "lundi"; "mardi"; "mercredi";
    "jeudi"; "vendredi"; "samedi"; "dimanche" |];
]

let titles_lang = [
  "en", "( Snoopy's best pic )";
  "fr", "( Le meilleur profil de Snoopy )";
]

let days = List.assoc lang days_lang
let month = List.assoc lang month_langs
let title = List.assoc lang titles_lang

let monday_first = 6, [| 0; 1; 2; 3; 4; 5; 6 |]
let sunday_first = 0, [| 6; 0; 1; 2; 3; 4; 5 |]

let off, days_order = sunday_first
let off, days_order = monday_first


let shorten n s =
  let len = String.length s in
  if n >= len then s else
    let n = if s.[n-1] = '\xC3' then n+1 else n in
    if n >= len then s else
      (String.sub s 0 n)


let pad size c s =
  let len = String.length s in
  let n1 = (size - len) / 2 in
  let n2 = size - len - n1 in
  String.make n1 c ^ s ^
  String.make n2 c


let days = Array.map (shorten 2) days


let indices ofs =
  (ofs / 7, ofs mod 7)


let t_same t1 t2 =
  ( t1.Unix.tm_year = t2.Unix.tm_year &&
    t1.Unix.tm_mon  = t2.Unix.tm_mon &&
    t1.Unix.tm_mday = t2.Unix.tm_mday )


let current_year () =
  let t = Unix.localtime (Unix.time ()) in
  (t.Unix.tm_year + 1900)


let make_month t year month =
  let empty_day = 0 in
  let m = Array.make_matrix 6 7 empty_day in
  let ofs = ref 0 in
  for day = 1 to 31 do
    let tm =
      { t with
        Unix.tm_year = year - 1900;
        Unix.tm_mon = month;
        Unix.tm_mday = day;
      }
    in
    let _, this = Unix.mktime tm in
    if !ofs = 0 then ofs := (this.Unix.tm_wday + off) mod 7;
    if t_same this tm then
      let i, j = indices !ofs in
      m.(i).(j) <- day;
    incr ofs;
  done;
  (m)


let cal ~year =
  let empty = [| [| |] |] in
  let months = Array.make 12 empty in
  let t = Unix.gmtime 0.0 in
  for mon = 0 to 11 do
    months.(mon) <- make_month t year mon;
  done;
  (months)


let print_month_label mp =
  List.iter (fun i ->
    let mon = pad 20 ' ' month.(i) in
    Printf.printf " %s " mon
  ) mp;
  print_newline ()


let print_day_label mp =
  List.iter (fun _ ->
    Array.iter (fun i ->
      Printf.printf " %s" days.(i)
    ) days_order
    ; print_string " "
  ) mp;
  print_newline ()


let print_mon m mp =
  print_month_label mp;
  print_day_label mp;
  for w = 0 to pred 6 do
    print_string begin
      String.concat " " begin
        List.map (fun i ->
          let b = Buffer.create 132 in
          for d = 0 to pred 7 do
            match m.(i).(w).(d) with
            | 0 -> Buffer.add_string b "   "
            | d -> Printf.kprintf (Buffer.add_string b) " %2d" d
          done;
          (Buffer.contents b)
        ) mp
      end
    end
    ; print_string "\n"
  done


let print_cal ~y:m =
  List.iter (fun mon_row ->
    print_mon m mon_row
  ) month_pattern


let print_header lbl =
  let n = List.length (List.hd month_pattern) in
  let year_lbl = pad (23*n-7) ' ' lbl in
  Printf.printf " %s\n" year_lbl


let print_calendar ~year =
  print_header title;
  print_header (string_of_int year);
  print_cal (cal ~year)


let () =
  let args = List.tl (Array.to_list Sys.argv) in
  match args with
  | [] ->
      let year = current_year () in
      print_calendar ~year

  | ["--year"; _year] ->
      let year = int_of_string _year in
      print_calendar ~year

  | _ ->
      usage ()
```


{{out}}

```txt

$ ocaml calendar.ml --year 1969

                     ( Snoopy's best pic )
                              1969
       January                 May                September
 Mo Tu We Th Fr Sa Su  Mo Tu We Th Fr Sa Su  Mo Tu We Th Fr Sa Su
        1  2  3  4  5            1  2  3  4   1  2  3  4  5  6  7
  6  7  8  9 10 11 12   5  6  7  8  9 10 11   8  9 10 11 12 13 14
 13 14 15 16 17 18 19  12 13 14 15 16 17 18  15 16 17 18 19 20 21
 20 21 22 23 24 25 26  19 20 21 22 23 24 25  22 23 24 25 26 27 28
 27 28 29 30 31        26 27 28 29 30 31     29 30

       February                June                October
 Mo Tu We Th Fr Sa Su  Mo Tu We Th Fr Sa Su  Mo Tu We Th Fr Sa Su
                 1  2                     1         1  2  3  4  5
  3  4  5  6  7  8  9   2  3  4  5  6  7  8   6  7  8  9 10 11 12
 10 11 12 13 14 15 16   9 10 11 12 13 14 15  13 14 15 16 17 18 19
 17 18 19 20 21 22 23  16 17 18 19 20 21 22  20 21 22 23 24 25 26
 24 25 26 27 28        23 24 25 26 27 28 29  27 28 29 30 31
                       30
        March                  July                November
 Mo Tu We Th Fr Sa Su  Mo Tu We Th Fr Sa Su  Mo Tu We Th Fr Sa Su
                 1  2      1  2  3  4  5  6                  1  2
  3  4  5  6  7  8  9   7  8  9 10 11 12 13   3  4  5  6  7  8  9
 10 11 12 13 14 15 16  14 15 16 17 18 19 20  10 11 12 13 14 15 16
 17 18 19 20 21 22 23  21 22 23 24 25 26 27  17 18 19 20 21 22 23
 24 25 26 27 28 29 30  28 29 30 31           24 25 26 27 28 29 30
 31
        April                 August               December
 Mo Tu We Th Fr Sa Su  Mo Tu We Th Fr Sa Su  Mo Tu We Th Fr Sa Su
     1  2  3  4  5  6               1  2  3   1  2  3  4  5  6  7
  7  8  9 10 11 12 13   4  5  6  7  8  9 10   8  9 10 11 12 13 14
 14 15 16 17 18 19 20  11 12 13 14 15 16 17  15 16 17 18 19 20 21
 21 22 23 24 25 26 27  18 19 20 21 22 23 24  22 23 24 25 26 27 28
 28 29 30              25 26 27 28 29 30 31  29 30 31


```



## PowerShell


```PowerShell

Param([int]$Year = 1969)
Begin {
    $COL_WIDTH = 21
    $COLS = 3
    $MONTH_COUNT = 12
    $MONTH_LINES = 9

    Function CenterStr([string]$s, [int]$lineSize) {
        $padSize = [int](($lineSize - $s.Length) / 2)
        ($(if ($padSize -gt 0) { ' ' * $padSize } else { '' }) + $s).PadRight($lineSize,' ')
    }

    Function MonthLines([int]$month) {
        $dt = [System.DateTime]::new($Year, $month, 1)
        $line = CenterStr $dt.ToString("MMMM") $COL_WIDTH
        $line += 'Su Mo Tu We Th Fr Sa'
        $line += $('   ' * $dt.DayOfWeek.value__)
        $line += (-join ($(1..$($dt.AddMonths(1).AddDays(-1).Day)) | %{ $("" + $_).PadLeft(3) }))
        $line = $line.PadRight($MONTH_LINES * $COL_WIDTH)
        New-Object –TypeName PSObject –Prop(@{
            'Lines'=(0..($MONTH_LINES - 1)) | %{ $_ * $COL_WIDTH } | %{ -join $line[$_..($_ + $COL_WIDTH - 1)] }
            'Dt'=$dt})
    }
}
Process {
    Write-Output (CenterStr $Year ($COL_WIDTH * $COLS + 4))
    $(0..($MONTH_COUNT / $COLS - 1)) | %{
        $fm = $_ * $COLS
        $monthNums = $fm..($fm + $COLS - 1) | %{ $_ + 1 }
        $months = $monthNums | %{ MonthLines $_ }
        $(0..($MONTH_LINES - 1)) | %{
            $ml = $_
            Write-Output $(-join ($(0..($COLS - 1)) | %{ $(if ($_ -eq 0) { '' } else {'  '}) + $months[$_].Lines[$ml] }))
        }
    }
}

```



## Perl



```perl
#!/usr/bin/perl
use strict;
use warnings;
use Time::Local;

my @names = qw/ JAN FEB MAR APR MAY JUN JUL AUG SEP OCT NOV DEC/;

my $year = shift ||'2007';

for my $month (0..11) {
    print " $names[$month] $year\n";
    print calendar($year, $month), "\n\n";
}

sub calendar {
    my ($year, $month) = @_;
    my @mon_days = qw/31 28 31 30 31 30 31 31 30 31 30 31/;
    ++$mon_days[1] if $year % 4 == 0 && ($year % 400 == 0 || $year % 1
+00 != 0);

    my $cal = " Sun Mon Tue Wed Thu Fri Sat\n";

    # Months are indexed beginning at 0
    my $time = timegm(0,0,0,1,$month,$year);
    my $wday = (gmtime $time)[6];

    $cal .= "    " x $wday;

    my $mday = 1;

    while ($mday <= $mon_days[$month]) {
        $cal .= sprintf "%4s", $mday++;
        $cal .= "\n" if ($wday + $mday -1) % 7 == 0;
    }
    return $cal;
}
# Let's use this as a placeholder until a better solution arrives, OK?
```



## Perl 6

{{works with|Rakudo|2015.12}}

```perl6
my $months-per-col = 3;
my @week-day-names = <Mo Tu We Th Fr Sa Su>;
my @month-names = <Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec>;

my Int $year = +(@*ARGS.shift || 1969);

say fmt-year($year);
exit;

sub fmt-year ($year) {

        my $str = (' ' x 30) ~ $year ~ "\n";

        my Array @month-strs;
        @month-strs[$_] = fmt-month($year, $_).lines.Array for 1 .. 12;

        loop ( my $month = 1; $month <= 12; $month += $months-per-col ) {
                while @month-strs[$month] {
                        for ^$months-per-col {
                                next unless @month-strs[$month+$_];
                                $str ~= @month-strs[$month+$_].shift;
                                $str ~= " " x 3;
                        }
                        $str ~= "\n";
                }
                $str ~= "\n";
        }
        return $str;
}
sub fmt-month ($year, $month) {
        my $str = sprintf "%-20s\n", @month-names[$month-1];
        $str ~= @week-day-names~"\n";
        my $date = DateTime.new(year => $year, month => $month);
        my $week-day = $date.day-of-week;

        $str ~= ("  " xx $week-day-1).join(" ");

        for $date.day .. $date.days-in-month -> $day {

                $date = DateTime.new(year => $year, month => $month, day => $day);

                $str ~= " " if 1 < $week-day < 8;
                if $week-day == 8 {
                        $str ~= "\n";
                        $week-day = 1;
                }
                $str ~= sprintf "%2d", $day;

                $week-day++;
        }
        $str ~= " " if $week-day < 8;
        $str ~= ("  " xx 8-$week-day).join(" ");
        $str ~= "\n";
        return $str;
}

```



## Phix

Gregorian calender only.

```Phix
include builtins\timedate.e

function centre(string s, integer width)
integer pad = width-length(s),
        left = floor(pad/2),
        right = pad-left
    return repeat(' ',left)&s&repeat(' ',right)
end function

function one_month(integer year, integer month)
integer dow = day_of_week(year,month,1)
sequence ldm = adjust_timedate(iff(month=12?{year+1,1,1,0,0,0,0,0}
                                           :{year,month+1,1,0,0,0,0,0}),
                               timedelta(days:=-1))
sequence res = {centre(format_timedate(ldm,"Mmmm"),20),"Su Mo Tu We Th Fr Sa"}
integer lastday = ldm[DT_DAY]
string line = repeat(' ',20)
integer p = dow*3-2
    for d=1 to lastday do
        line[p..p+1] = sprintf("%2d",d)
        p += 3
        if dow=7 or d=lastday then
            res = append(res,line)
            line = repeat(' ',20)
            dow = 1
            p = 1
        else
            dow += 1
        end if
    end for
    return res
end function

procedure print_calendar(integer year, integer width)
sequence months = repeat(0,12)
integer wide = floor((width+2)/22)
    printf(1,centre("[Spot Reserved For Snoopy]",width)&"\n")
    printf(1,centre(sprintf("%d",year),width)&"\n")
    for month=1 to 12 do
        months[month] = one_month(year,month)
    end for
    for month=1 to 12 by wide do
        for k=1 to 9 do -- (more than enough)
            integer any = 0
            string line = "", this
            for j=0 to wide-1 do
                if length(line) then
                    line &= "  "
                end if
                if k>length(months[month+j]) then
                    line &= repeat(' ',20)
                else
                    line &= months[month+j][k]
                    any = 1
                end if
            end for
            if any=0 then exit end if
            printf(1,centre(line,width)&"\n")
        end for
    end for
end procedure

print_calendar(1969,80)
printf(1,join(repeat("1234567890",8),"")&"\n")
print_calendar(1969,132)
printf(1,join(repeat("1234567890",13),"")&"12\n")
```

{{out}}

```txt


                           [Spot Reserved For Snoopy]
                                      1969
              January               February               March
        Su Mo Tu We Th Fr Sa  Su Mo Tu We Th Fr Sa  Su Mo Tu We Th Fr Sa
                  1  2  3  4                     1                     1
         5  6  7  8  9 10 11   2  3  4  5  6  7  8   2  3  4  5  6  7  8
        12 13 14 15 16 17 18   9 10 11 12 13 14 15   9 10 11 12 13 14 15
        19 20 21 22 23 24 25  16 17 18 19 20 21 22  16 17 18 19 20 21 22
        26 27 28 29 30 31     23 24 25 26 27 28     23 24 25 26 27 28 29
                                                    30 31
               April                  May                   June
        Su Mo Tu We Th Fr Sa  Su Mo Tu We Th Fr Sa  Su Mo Tu We Th Fr Sa
               1  2  3  4  5               1  2  3   1  2  3  4  5  6  7
         6  7  8  9 10 11 12   4  5  6  7  8  9 10   8  9 10 11 12 13 14
        13 14 15 16 17 18 19  11 12 13 14 15 16 17  15 16 17 18 19 20 21
        20 21 22 23 24 25 26  18 19 20 21 22 23 24  22 23 24 25 26 27 28
        27 28 29 30           25 26 27 28 29 30 31  29 30
                July                 August              September
        Su Mo Tu We Th Fr Sa  Su Mo Tu We Th Fr Sa  Su Mo Tu We Th Fr Sa
               1  2  3  4  5                  1  2      1  2  3  4  5  6
         6  7  8  9 10 11 12   3  4  5  6  7  8  9   7  8  9 10 11 12 13
        13 14 15 16 17 18 19  10 11 12 13 14 15 16  14 15 16 17 18 19 20
        20 21 22 23 24 25 26  17 18 19 20 21 22 23  21 22 23 24 25 26 27
        27 28 29 30 31        24 25 26 27 28 29 30  28 29 30
                              31
              October               November              December
        Su Mo Tu We Th Fr Sa  Su Mo Tu We Th Fr Sa  Su Mo Tu We Th Fr Sa
                  1  2  3  4                     1      1  2  3  4  5  6
         5  6  7  8  9 10 11   2  3  4  5  6  7  8   7  8  9 10 11 12 13
        12 13 14 15 16 17 18   9 10 11 12 13 14 15  14 15 16 17 18 19 20
        19 20 21 22 23 24 25  16 17 18 19 20 21 22  21 22 23 24 25 26 27
        26 27 28 29 30 31     23 24 25 26 27 28 29  28 29 30 31
                              30
12345678901234567890123456789012345678901234567890123456789012345678901234567890
                                                     [Spot Reserved For Snoopy]
                                                                1969
       January               February               March                 April                  May                   June
 Su Mo Tu We Th Fr Sa  Su Mo Tu We Th Fr Sa  Su Mo Tu We Th Fr Sa  Su Mo Tu We Th Fr Sa  Su Mo Tu We Th Fr Sa  Su Mo Tu We Th Fr Sa
           1  2  3  4                     1                     1         1  2  3  4  5               1  2  3   1  2  3  4  5  6  7
  5  6  7  8  9 10 11   2  3  4  5  6  7  8   2  3  4  5  6  7  8   6  7  8  9 10 11 12   4  5  6  7  8  9 10   8  9 10 11 12 13 14
 12 13 14 15 16 17 18   9 10 11 12 13 14 15   9 10 11 12 13 14 15  13 14 15 16 17 18 19  11 12 13 14 15 16 17  15 16 17 18 19 20 21
 19 20 21 22 23 24 25  16 17 18 19 20 21 22  16 17 18 19 20 21 22  20 21 22 23 24 25 26  18 19 20 21 22 23 24  22 23 24 25 26 27 28
 26 27 28 29 30 31     23 24 25 26 27 28     23 24 25 26 27 28 29  27 28 29 30           25 26 27 28 29 30 31  29 30
                                             30 31
         July                 August              September              October               November              December
 Su Mo Tu We Th Fr Sa  Su Mo Tu We Th Fr Sa  Su Mo Tu We Th Fr Sa  Su Mo Tu We Th Fr Sa  Su Mo Tu We Th Fr Sa  Su Mo Tu We Th Fr Sa
        1  2  3  4  5                  1  2      1  2  3  4  5  6            1  2  3  4                     1      1  2  3  4  5  6
  6  7  8  9 10 11 12   3  4  5  6  7  8  9   7  8  9 10 11 12 13   5  6  7  8  9 10 11   2  3  4  5  6  7  8   7  8  9 10 11 12 13
 13 14 15 16 17 18 19  10 11 12 13 14 15 16  14 15 16 17 18 19 20  12 13 14 15 16 17 18   9 10 11 12 13 14 15  14 15 16 17 18 19 20
 20 21 22 23 24 25 26  17 18 19 20 21 22 23  21 22 23 24 25 26 27  19 20 21 22 23 24 25  16 17 18 19 20 21 22  21 22 23 24 25 26 27
 27 28 29 30 31        24 25 26 27 28 29 30  28 29 30              26 27 28 29 30 31     23 24 25 26 27 28 29  28 29 30 31
                       31                                                                30
123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012

```



## PicoLisp

This "calendar" is nicely formatted, and fits into 20 columns ;-)

```PicoLisp
(de cal (Year)
   (prinl "====== " Year " ======")
   (for Dat (range (date Year 1 1) (date Year 12 31))
      (let D (date Dat)
         (tab (3 3 4 8)
            (when (= 1 (caddr D))
               (get *Mon (cadr D)) )
            (caddr D)
            (day Dat *Day)
            (when (=0 (% (inc Dat) 7))
               (pack "Week " (week Dat)) ) ) ) ) )

(cal 1969)
```

Output:

```txt

### === 1969 ===

Jan  1 Wed
     2 Thu
     3 Fri
     4 Sat
     5 Sun
     6 Mon  Week 2
     7 Tue
....
    28 Sat
    29 Sun
    30 Mon Week 27
Jul  1 Tue
     2 Wed
     3 Thu
     4 Fri
....
    25 Thu
    26 Fri
    27 Sat
    28 Sun
    29 Mon Week 53
    30 Tue
    31 Wed
```



## Pike


the Calendar in Pike does not handle the transitions from the Julian to the Gregorian calendar, but it handles those separately.
the default calendar is ISO, other options are Gregorian, Julian, Bahai, Coptic, Islamic and Discordian.

this script also highlights holidays by region. regions may be chosen by 2-letter country name, as well as some special 'regions':                 christianity, orthodox, bahai, islamic, among others.


```Pike
#!/bin/env pike

int main(int argc, array(string) argv)
{
    object cal = Calendar;
    object year;
    string region = "us";

    array date = argv[1..];
    if (sizeof(date) && objectp(Calendar[date[0]]) && Calendar[date[0]]->Day)
    {
        cal = Calendar[date[0]];
        date = Array.shift(date)[1];
    }

    if (sizeof(date) && (int)date[0])
    {
        year = cal.Year((int)date[0]);
        date = Array.shift(date)[1];
    }

    if (sizeof(date))
        region = date[0];

    if (!year)
        year = cal.Year();

    print_year(year, region);
}

array make_month(object month, int field_width, void|string region)
{
    array out =({});
    mapping holidays = ([]);
    object today = Calendar.Day();

    if (region)
        holidays = Calendar.Events.find_region(region)->scan_events(month);

    array weekday_names =
        sprintf("%*.*s", field_width, field_width, month->week()->days()->week_day_shortname()[*]);

    out += ({ ({ month->month_name(), month->month_no(), month->year_name() }) });
    out += ({ weekday_names });
    out += showday(month->weeks()->days()[*][*], month, today, holidays, field_width);

    out += ({ ({ " "*field_width })*sizeof(weekday_names) });

    return out;
}

string print_month(object _month, void|int field_width, void|string region)
{
    if (!field_width)
        field_width = 2;
    array month = make_month(_month, field_width, region);
    string out = "";

    out += sprintf("%|*s\n", (field_width+1)*sizeof(month[1])-1, sprintf("%s", month[0][0]));
    out += sprintf((month[1..][*]*" ")*"\n");
    return out;
}

string print_year(object year, void|string region)
{
        array output = ({});
        int day_width = 2;
        int columns = Stdio.stdout.tcgetattr()->columns;
        int month_width = sizeof(make_month(year->month(), day_width)[1]) * (day_width+1) - 1;
        if (columns < month_width)
            columns = month_width;

        // try to find an optimal good looking solution to spread the months
        // across the terminal width
        // for the common calendar of 12 months this is easy but we need to
        // account for caledars that have more than 12 months
        float max_width = (float)((columns+2)/(month_width+2));
        float max_height = ceil(year->number_of_months()/max_width);
        float w = max_width;

        while(ceil(year->number_of_months()/(w-1)) == max_height)
            w--;

        foreach(print_month(year->months()[*], day_width, region)/w;; array row)
        {
            array rows = row[*]/"\n";
            int l = max(@sizeof(rows[*]));
            foreach(rows; int i;)
            {
                // the last line of each month is an empty line.
                // repeat the line as many times as needed to make the months equally long
                rows[i]+=({ rows[i][-1] })*(l-sizeof(rows[i]));
            }
            rows = Array.transpose(rows);
            output += rows[*]*"  ";
        }
        write("%*|s\n", sizeof(output[1]), year->format_nice());
        write(output * "\n");
        write("\n");
}

string showday(object day, object month, object today, mapping holidays, int field_width)
{
    string dayname;
    if (day->month() == month)
    {
        dayname = (string)day->month_day();
        dayname = " "*(sizeof((string)month->number_of_days())-sizeof(dayname))+dayname;
        if (day == today)
            dayname = sprintf("%|*.*s", field_width, field_width, dayname);
        else
            dayname = sprintf("%|*.*s", field_width, field_width, dayname);
        if (holidays[day])
            dayname = sprintf("%|s", dayname);
    }
    else
        dayname = " "*field_width;
    return dayname;
}

```

Output: (holidays lost in copy-paste)

```txt

                               1969
       January               February               March
 Mo Tu We Th Fr Sa Su  Mo Tu We Th Fr Sa Su  Mo Tu We Th Fr Sa Su
        1  2  3  4  5                  1  2                  1  2
  6  7  8  9 10 11 12   3  4  5  6  7  8  9   3  4  5  6  7  8  9
 13 14 15 16 17 18 19  10 11 12 13 14 15 16  10 11 12 13 14 15 16
 20 21 22 23 24 25 26  17 18 19 20 21 22 23  17 18 19 20 21 22 23
 27 28 29 30 31        24 25 26 27 28        24 25 26 27 28 29 30
                                             31

        April                  May                   June
 Mo Tu We Th Fr Sa Su  Mo Tu We Th Fr Sa Su  Mo Tu We Th Fr Sa Su
     1  2  3  4  5  6            1  2  3  4                     1
  7  8  9 10 11 12 13   5  6  7  8  9 10 11   2  3  4  5  6  7  8
 14 15 16 17 18 19 20  12 13 14 15 16 17 18   9 10 11 12 13 14 15
 21 22 23 24 25 26 27  19 20 21 22 23 24 25  16 17 18 19 20 21 22
 28 29 30              26 27 28 29 30 31     23 24 25 26 27 28 29
                                             30

         July                 August              September
 Mo Tu We Th Fr Sa Su  Mo Tu We Th Fr Sa Su  Mo Tu We Th Fr Sa Su
     1  2  3  4  5  6               1  2  3   1  2  3  4  5  6  7
  7  8  9 10 11 12 13   4  5  6  7  8  9 10   8  9 10 11 12 13 14
 14 15 16 17 18 19 20  11 12 13 14 15 16 17  15 16 17 18 19 20 21
 21 22 23 24 25 26 27  18 19 20 21 22 23 24  22 23 24 25 26 27 28
 28 29 30 31           25 26 27 28 29 30 31  29 30

       October               November              December
 Mo Tu We Th Fr Sa Su  Mo Tu We Th Fr Sa Su  Mo Tu We Th Fr Sa Su
        1  2  3  4  5                  1  2   1  2  3  4  5  6  7
  6  7  8  9 10 11 12   3  4  5  6  7  8  9   8  9 10 11 12 13 14
 13 14 15 16 17 18 19  10 11 12 13 14 15 16  15 16 17 18 19 20 21
 20 21 22 23 24 25 26  17 18 19 20 21 22 23  22 23 24 25 26 27 28
 27 28 29 30 31        24 25 26 27 28 29 30  29 30 31

```



## PL/I


```PL/I

calendar: procedure (year) options (main);
   declare year character (4) varying;
   declare (a, b, c) (0:5,0:6) character (3);
   declare name_month(12) static character (9) varying initial (
      'JANUARY', 'FEBRUARY', 'MARCH',     'APRIL',   'MAY',      'JUNE',
      'JULY',    'AUGUST',   'SEPTEMBER', 'OCTOBER', 'NOVEMBER', 'DECEMBER');
   declare i fixed;
   declare (mm, mmp1, mmp2) pic '99';

   put edit (center('CALENDAR FOR ' || YEAR, 67)) (a);
   put skip (2);

   do mm = 1 to 12 by 3;
      mmp1 = mm + 1; mmp2 = mm + 2;
      call prepare_month('01' || mm   || YEAR, a);
      call prepare_month('01' || mmp1 || YEAR, b);
      call prepare_month('01' || mmp2 || YEAR, c);

      put skip edit (center(name_month(mm),   23),
                     center(name_month(mmp1), 23),
                     center(name_month(mmp2), 23) ) (a);
      put skip edit ((3)'  M  T  W  T  F  S  S  ') (a);
      do i = 0 to 5;
         put skip edit (a(i,*), b(i,*), c(i,*)) (7 a, x(2));
      end;
   end;

prepare_month: procedure (start, month);
   declare month(0:5,0:6) character (3);
   declare start character (8);
   declare i pic 'ZZ9';
   declare offset fixed;
   declare (j, day) fixed binary (31);
   declare (this_month, next_month, k) fixed binary;

   day = days(start, 'DDMMYYYY');
   offset = weekday(day) - 1;
   if offset  = 0 then offset = 7;
   month = '';
   do j = day by 1;
      this_month = substr(daystodate(j,   'DDMMYYYY'), 3, 2);
      next_month = substr(daystodate(j+1, 'DDMMYYYY'), 3, 2);
      if this_month^= next_month then leave;
   end;
   i = 1;
   do k = offset-1 to offset+j-day-1;
      month(k/7, mod(k,7)) = i; i = i + 1;
   end;
end prepare_month;

end calendar;

```

Output:

```txt

                         CALENDAR FOR 1969


        JANUARY               FEBRUARY                 MARCH
  M  T  W  T  F  S  S    M  T  W  T  F  S  S    M  T  W  T  F  S  S
        1  2  3  4  5                   1  2                   1  2
  6  7  8  9 10 11 12    3  4  5  6  7  8  9    3  4  5  6  7  8  9
 13 14 15 16 17 18 19   10 11 12 13 14 15 16   10 11 12 13 14 15 16
 20 21 22 23 24 25 26   17 18 19 20 21 22 23   17 18 19 20 21 22 23
 27 28 29 30 31         24 25 26 27 28         24 25 26 27 28 29 30
                                               31
         APRIL                   MAY                   JUNE
  M  T  W  T  F  S  S    M  T  W  T  F  S  S    M  T  W  T  F  S  S
     1  2  3  4  5  6             1  2  3  4                      1
  7  8  9 10 11 12 13    5  6  7  8  9 10 11    2  3  4  5  6  7  8
 14 15 16 17 18 19 20   12 13 14 15 16 17 18    9 10 11 12 13 14 15
 21 22 23 24 25 26 27   19 20 21 22 23 24 25   16 17 18 19 20 21 22
 28 29 30               26 27 28 29 30 31      23 24 25 26 27 28 29
                                               30
         JULY                  AUGUST                SEPTEMBER
  M  T  W  T  F  S  S    M  T  W  T  F  S  S    M  T  W  T  F  S  S
     1  2  3  4  5  6                1  2  3    1  2  3  4  5  6  7
  7  8  9 10 11 12 13    4  5  6  7  8  9 10    8  9 10 11 12 13 14
 14 15 16 17 18 19 20   11 12 13 14 15 16 17   15 16 17 18 19 20 21
 21 22 23 24 25 26 27   18 19 20 21 22 23 24   22 23 24 25 26 27 28
 28 29 30 31            25 26 27 28 29 30 31   29 30

        OCTOBER               NOVEMBER               DECEMBER
  M  T  W  T  F  S  S    M  T  W  T  F  S  S    M  T  W  T  F  S  S
        1  2  3  4  5                   1  2    1  2  3  4  5  6  7
  6  7  8  9 10 11 12    3  4  5  6  7  8  9    8  9 10 11 12 13 14
 13 14 15 16 17 18 19   10 11 12 13 14 15 16   15 16 17 18 19 20 21
 20 21 22 23 24 25 26   17 18 19 20 21 22 23   22 23 24 25 26 27 28
 27 28 29 30 31         24 25 26 27 28 29 30   29 30 31


```

Extract for 2013:

```txt

                         CALENDAR FOR 2013


        JANUARY               FEBRUARY                 MARCH
  M  T  W  T  F  S  S    M  T  W  T  F  S  S    M  T  W  T  F  S  S
     1  2  3  4  5  6                1  2  3                1  2  3
  7  8  9 10 11 12 13    4  5  6  7  8  9 10    4  5  6  7  8  9 10
 14 15 16 17 18 19 20   11 12 13 14 15 16 17   11 12 13 14 15 16 17
 21 22 23 24 25 26 27   18 19 20 21 22 23 24   18 19 20 21 22 23 24
 28 29 30 31            25 26 27 28            25 26 27 28 29 30 31

```



## Prolog

Call the write_calendar(Year) predicate to print a calendar for a year.
Works with swi-prolog and requires the day_of_the_week library call.


```prolog
% Write out the calender, because format can actually span multiple lines, it is easier
% to write out the static parts in place and insert the generated parts into that format.
write_calendar(Year) :-
	month_x3_format(Year, 1, 2, 3, F1_3),
	month_x3_format(Year, 4, 5, 6, F4_6),
	month_x3_format(Year, 7, 8, 9, F7_9),
	month_x3_format(Year, 10, 11, 12, F10_12),

	format('

                                      ~w

            January                  February                   March
      Su Mo Tu We Th Fr Sa     Su Mo Tu We Th Fr Sa     Su Mo Tu We Th Fr Sa
~w

             April                     May                      June
      Su Mo Tu We Th Fr Sa     Su Mo Tu We Th Fr Sa     Su Mo Tu We Th Fr Sa
~w

              July                    August                  September
      Su Mo Tu We Th Fr Sa     Su Mo Tu We Th Fr Sa     Su Mo Tu We Th Fr Sa
~w

            October                  November                 December
      Su Mo Tu We Th Fr Sa     Su Mo Tu We Th Fr Sa     Su Mo Tu We Th Fr Sa
~w
', [Year, F1_3, F4_6, F7_9, F10_12]), !.

% Generate the data for a row of months and then create an atom one row at a time
% for all of the months.
month_x3_format(Year, M1, M2, M3, F) :-
	calc_month_rows(Year, M1, M1r),
	calc_month_rows(Year, M2, M2r),
	calc_month_rows(Year, M3, M3r),
	month_x3_format(M1r, M2r, M3r, F).

month_x3_format(M1, M2, M3, '') :- maplist(=('   '), M1), maplist(=('   '), M2), maplist(=('   '), M3).
month_x3_format(M1, M2, M3, F) :-
		month_format('      ', M1, M1r, F1),
		month_format(F1, M2, M2r, F2),
		month_format(F2, M3, M3r, F3),
		atom_concat(F3, '\n', F4),
		month_x3_format(M1r, M2r, M3r, Fr),
		atom_concat(F4, Fr, F).

month_format(Orig, [Su,Mo,Tu,We,Th,Fr,Sa|R], R, F) :-
	maplist(day_format, [Su,Mo,Tu,We,Th,Fr,Sa], Formatted),
	format(atom(F2), '~w~w~w~w~w~w~w    ', Formatted),
	atom_concat(Orig, F2, F).

day_format('   ', '   ') :- !.
day_format(D, F) :- D < 10, format(atom(F), '~w  ', D).
day_format(D, F) :- D >= 10, format(atom(F), '~w ', D).

% Calculate the days of a month, this is done by getting the first day of the month,
% then offsetting that with spaces from the start and then adding 1-NumDaysinMonth and
% finally spaces until the end. The maximum possible size is used and then truncated later.
calc_month_rows(Year, Month, Result) :-
	length(Result, 42), % max 6 rows of 7 days
	month_days(Month, Year, DaysInMonth),
	day_of_the_week(date(Year, Month, 1), FirstWeekDay),
	day_offset(FirstWeekDay, Offset),
	day_print_map(DaysInMonth, Offset, Result).

day_print_map(DaysInMonth, 0, [1|R]) :-
	day_print_map2(DaysInMonth, 2, R).
day_print_map(DaysInMonth, Offset, ['   '|R]) :-
	dif(Offset, 0),
	succ(NewOffset, Offset),
	day_print_map(DaysInMonth, NewOffset, R).

day_print_map2(D, D, [D|R])	:- day_print_map(R).
day_print_map2(D, N, [N|R]) :- dif(D,N), succ(N, N1), day_print_map2(D, N1, R).

day_print_map([]).
day_print_map(['   '|R]) :- day_print_map(R).

% Figure out the number of days in a month based on whether it is a leap year or not.
month_days(2, Year, Days) :-
	is_leap_year(Year) -> Days = 29
	; Days = 28.
month_days(Month, _, Days) :-
	dif(Month, 2),
	nth1(Month, [31, _, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31], Days).

% Figure out the space offset based on the day the month starts on.
day_offset(D, D) :- dif(D, 7).
day_offset(7, 0).

% Test for leap years
is_leap_year(Year) :-
	0 is Year mod 100 -> 0 is Year mod 400
	; 0 is Year mod 4.
```




## Python

The Python [https://docs.python.org/3/library/calendar.html calendar].prcal function prints calendars with the following formatting options: optional parameters w, l, and c are for date column width, lines per week, and number of spaces between month columns, respectively.


```python>>>
 import calendar
>>> help(calendar.prcal)
Help on method pryear in module calendar:

pryear(self, theyear, w=0, l=0, c=6, m=3) method of calendar.TextCalendar instance
    Print a years calendar.

>>> calendar.prcal(1969)
                                  1969

      January                   February                   March
Mo Tu We Th Fr Sa Su      Mo Tu We Th Fr Sa Su      Mo Tu We Th Fr Sa Su
       1  2  3  4  5                      1  2                      1  2
 6  7  8  9 10 11 12       3  4  5  6  7  8  9       3  4  5  6  7  8  9
13 14 15 16 17 18 19      10 11 12 13 14 15 16      10 11 12 13 14 15 16
20 21 22 23 24 25 26      17 18 19 20 21 22 23      17 18 19 20 21 22 23
27 28 29 30 31            24 25 26 27 28            24 25 26 27 28 29 30
                                                    31

       April                      May                       June
Mo Tu We Th Fr Sa Su      Mo Tu We Th Fr Sa Su      Mo Tu We Th Fr Sa Su
    1  2  3  4  5  6                1  2  3  4                         1
 7  8  9 10 11 12 13       5  6  7  8  9 10 11       2  3  4  5  6  7  8
14 15 16 17 18 19 20      12 13 14 15 16 17 18       9 10 11 12 13 14 15
21 22 23 24 25 26 27      19 20 21 22 23 24 25      16 17 18 19 20 21 22
28 29 30                  26 27 28 29 30 31         23 24 25 26 27 28 29
                                                    30

        July                     August                  September
Mo Tu We Th Fr Sa Su      Mo Tu We Th Fr Sa Su      Mo Tu We Th Fr Sa Su
    1  2  3  4  5  6                   1  2  3       1  2  3  4  5  6  7
 7  8  9 10 11 12 13       4  5  6  7  8  9 10       8  9 10 11 12 13 14
14 15 16 17 18 19 20      11 12 13 14 15 16 17      15 16 17 18 19 20 21
21 22 23 24 25 26 27      18 19 20 21 22 23 24      22 23 24 25 26 27 28
28 29 30 31               25 26 27 28 29 30 31      29 30

      October                   November                  December
Mo Tu We Th Fr Sa Su      Mo Tu We Th Fr Sa Su      Mo Tu We Th Fr Sa Su
       1  2  3  4  5                      1  2       1  2  3  4  5  6  7
 6  7  8  9 10 11 12       3  4  5  6  7  8  9       8  9 10 11 12 13 14
13 14 15 16 17 18 19      10 11 12 13 14 15 16      15 16 17 18 19 20 21
20 21 22 23 24 25 26      17 18 19 20 21 22 23      22 23 24 25 26 27 28
27 28 29 30 31            24 25 26 27 28 29 30      29 30 31
```



## Racket


```racket
#lang racket
(require racket/date net/base64 file/gunzip)
(define (calendar yr)
  (define (nsplit n l) (if (null? l) l (cons (take l n) (nsplit n (drop l n)))))
  (define months
    (for/list ([mn (in-naturals 1)]
               [mname '(January February March April May June July
                        August September October November December)])
      (define s (find-seconds 0 0 12 1 mn yr))
      (define pfx (date-week-day (seconds->date s)))
      (define days
        (let ([? (if (= mn 12) (λ(x y) y) (λ(x y) x))])
          (round (/ (- (find-seconds 0 0 12 1 (? (+ 1 mn) 1) (? yr (+ 1 yr))) s)
                    60 60 24))))
      (list* (~a mname #:width 20 #:align 'center) "Su Mo Tu We Th Fr Sa"
             (map string-join
                  (nsplit 7 `(,@(make-list pfx "  ")
                              ,@(for/list ([d days])
                                  (~a (+ d 1) #:width 2 #:align 'right))
                              ,@(make-list (- 42 pfx days) "  ")))))))
  (let ([s #"nZA7CsAgDED3nCLgoAU/3Uvv4SCE3qKD5OyNWvoBhdIHSswjMYp4YR2z80Tk8StOgP
             sY0EyrMZOE6WsL3u4G5lyV+d8MyVOy8hZBt7RSMca9Ac/KUIs1L/BOysb50XMtMzEj
             ZqiuRxIVqI+4kSpy7GqpXNsz+bfpfWIGOAA="]
        [o (open-output-string)])
    (inflate (open-input-bytes (base64-decode s)) o)
    (display (regexp-replace #rx"~a" (get-output-string o) (~a yr))))
  (for-each displayln
    (dropf-right (for*/list ([3ms (nsplit 3 months)] [s (apply map list 3ms)])
                   (regexp-replace #rx" +$" (string-join s "   ") ""))
                 (λ(s) (equal? "" s)))))

(calendar 1969)
```


{{out}}

```txt
           ,-~~-.___.                                         ----
          / ()=(()   \                                        1969
         (   (        0                                       ----
          \._\, ,----'
     ##XXXxxxxxxx
            /  ---'~;
           /    /~|-
         =(   ~~  |
   /~~~~~~~~~~~~~~~~~~~~~\
  /_______________________\
 /_________________________\
/___________________________\
   |____________________|
   |____________________|
   |____________________|
   |                    |

      January                February                March
Su Mo Tu We Th Fr Sa   Su Mo Tu We Th Fr Sa   Su Mo Tu We Th Fr Sa
          1  2  3  4                      1                      1
 5  6  7  8  9 10 11    2  3  4  5  6  7  8    2  3  4  5  6  7  8
12 13 14 15 16 17 18    9 10 11 12 13 14 15    9 10 11 12 13 14 15
19 20 21 22 23 24 25   16 17 18 19 20 21 22   16 17 18 19 20 21 22
26 27 28 29 30 31      23 24 25 26 27 28      23 24 25 26 27 28 29
                                              30 31
       April                   May                    June
Su Mo Tu We Th Fr Sa   Su Mo Tu We Th Fr Sa   Su Mo Tu We Th Fr Sa
       1  2  3  4  5                1  2  3    1  2  3  4  5  6  7
 6  7  8  9 10 11 12    4  5  6  7  8  9 10    8  9 10 11 12 13 14
13 14 15 16 17 18 19   11 12 13 14 15 16 17   15 16 17 18 19 20 21
20 21 22 23 24 25 26   18 19 20 21 22 23 24   22 23 24 25 26 27 28
27 28 29 30            25 26 27 28 29 30 31   29 30

        July                  August               September
Su Mo Tu We Th Fr Sa   Su Mo Tu We Th Fr Sa   Su Mo Tu We Th Fr Sa
       1  2  3  4  5                   1  2       1  2  3  4  5  6
 6  7  8  9 10 11 12    3  4  5  6  7  8  9    7  8  9 10 11 12 13
13 14 15 16 17 18 19   10 11 12 13 14 15 16   14 15 16 17 18 19 20
20 21 22 23 24 25 26   17 18 19 20 21 22 23   21 22 23 24 25 26 27
27 28 29 30 31         24 25 26 27 28 29 30   28 29 30
                       31
      October                November               December
Su Mo Tu We Th Fr Sa   Su Mo Tu We Th Fr Sa   Su Mo Tu We Th Fr Sa
          1  2  3  4                      1       1  2  3  4  5  6
 5  6  7  8  9 10 11    2  3  4  5  6  7  8    7  8  9 10 11 12 13
12 13 14 15 16 17 18    9 10 11 12 13 14 15   14 15 16 17 18 19 20
19 20 21 22 23 24 25   16 17 18 19 20 21 22   21 22 23 24 25 26 27
26 27 28 29 30 31      23 24 25 26 27 28 29   28 29 30 31
                       30
```



## Rebol


```rebol

do [if "" = y: ask "Year (ENTER for current):^/^/" [prin y: now/year]
foreach m system/locale/months [
  prin rejoin ["^/^/     " m "^/^/ "]
  foreach day system/locale/days [prin join copy/part day 2 " "]
  print ""  f: to-date rejoin ["1-"m"-"y]  loop f/weekday - 1 [prin "   "]
  repeat i 31 [
    if attempt [c: to-date rejoin [i"-"m"-"y]][
      prin join either 1 = length? form i ["  "][" "] i
      if c/weekday = 7 [print ""]
    ]
  ]
] ask "^/^/Press [ENTER] to Continue..."]

```



## REXX

Rather than build a program from scratch, I took my generalized REXX program that generated (as an

option) any calendar for any month, and by extension, any number of months (so that an entire

year could be displayed/written).   To make the program much smaller, a LOT of code was removed

(but it's still a sledgehammer doing the work of a small hammer).


Some of the code removed:
::* almost all error checking and the issuing of error messages.
::* the option of using "simple" characters as opposed to a true grid (of ASCII extended characters).
::* the option of highlighting of today's date.
::* the option of using colors.
::* the specifying of what lang:uage to use (over 90 languages, default is English).
::* the option of using indentation.
::* the option of using different date formats (to specify the date).
::* the option of using a time addition/subtraction for the date (*  -4score-7years).
::* the option of showing the phases of the moon.
::* the option of showing the day-of-the-year  (as on financial calendars and others).
::* the option of specifying a calendar fill character (those empty cells in a monthly calender).
::* the option of drawing Snoopy (three versions) and also many other animals/ASCII art.
::* the specifing of alternate outputs (a file/printer/XEDIT/stack/GLOBALVs/etc).
::* the option of displaying the year in blocked form (at the top of the calendar):

```txt

                   11          9999999       6666666       9999999
                  111         999999999     666666666     999999999
                 1111        99      99    66      66    99      99
                   11        99      99    66            99      99
                   11        99     999    66            99     999
                   11         999999999    66 666666      999999999
                   11          99999 99    6666666666      99999 99
                   11                99    666     66            99
                   11                99    66      66            99
                   11        99      99    66      66    99      99
                 111111       99999999      66666666      99999999
                 111111        999999        666666        999999

```


Most of the code is dealing with drawing a cell (grid) for each day-of-the-month, so the program

is a bit more complex than it has to be for this task.   Also, it was designed for any year, any

month(s), and any size screen/device to show (display) it.   Accommodations were made in the program

to allow the choice of what type of cells would be generated: small/smaller/smallest for height, and

narrow/narrower/narrowest for the width.   The size (width/depth) of the output device/terminal can be

specified.   The choice could've been performed programmatically, but I never believe that a program

could best choose over what the user wants, as it depends on esthetics and looks (fashion).

```rexx
/*REXX program to show any year's (monthly) calendar (with/without grid)*/

@abc='abcdefghijklmnopqrstuvwxyz'; @abcU=@abc; upper @abcU
calfill=' '; mc=12; _='1 3 1234567890' "fb"x
parse var _ grid calspaces # chk . cv_ days.1 days.2 days.3 daysn sd sw
_=0; parse var _ cols 1 jd 1 lowerCase 1 maxKalPuts 1 narrow 1,
                 narrower 1 narrowest 1 short 1 shorter 1 shortest 1,
                 small 1 smaller 1 smallest 1 upperCase
parse arg mm '/' dd "/" yyyy _ '(' ops;  uops=ops
if _\=='' | \is#(mm) | \is#(dd) | \is#(yyyy) then call erx 86

  do while ops\==''; ops=strip(ops,'L'); parse var ops _1 2 1 _ . 1 _o ops
  upper _
       select
       when  abb('CALSPaces')  then calspaces=nai()
       when  abb('DEPth')      then        sd=nai()
       when abbn('GRIDs')      then      grid=no()
       when abbn('LOWercase')  then lowerCase=no()
       when  abb('CALMONths')  then        mc=nai()
       when abbn('NARrow')     then    narrow=no()
       when abbn('NARROWER')   then  narrower=no()
       when abbn('NARROWESt')  then narrowest=no()
       when abbn('SHORt')      then     short=no()
       when abbn('SHORTER')    then   shorter=no()
       when abbn('SHORTESt')   then  shortest=no()
       when abbn('SMALl')      then     small=no()
       when abbn('SMALLER')    then   smaller=no()
       when abbn('SMALLESt')   then  smallest=no()
       when abbn('UPPercase')  then upperCase=no()
       when  abb('WIDth')      then        sw=nai()
       otherwise nop
       end    /*select*/
  end         /*do while opts\== ...*/

mc=int(mc,'monthscalender'); if mc>0 then cal=1
days='Sunday Monday Tuesday Wednesday Thursday Friday Saturday'
months='January February March April May June July August September October November December'
days=' 'days;  months=' 'months
cyyyy=right(date(),4);  hyy=left(cyyyy,2);  lyy=right(cyyyy,2)
dy.=31; _=30; parse var _ dy.4 1 dy.6 1 dy.9 1 dy.11; dy.2=28+ly(yyyy)
yy=right(yyyy,2); sd=p(sd 43); sw=p(sw 80); cw=10; cindent=1; calwidth=76
if small    then do; narrow=1   ; short=1   ; end
if smaller  then do; narrower=1 ; shorter=1 ; end
if smallest then do; narrowest=1; shortest=1; end
if shortest then shorter=1
if shorter  then short  =1
if narrow    then do; cw=9; cindent=3; calwidth=69; end
if narrower  then do; cw=4; cindent=1; calwidth=34; end
if narrowest then do; cw=2; cindent=1; calwidth=20; end
cv_=calwidth+calspaces+2
calfill=left(copies(calfill,cw),cw)
      do j=1 for 7;         _=word(days,j)
            do jw=1 for 3;  _d=strip(substr(_,cw*jw-cw+1,cw))
            if jw=1 then _d=centre(_d,cw+1)
                    else _d=left(_d,cw+1)
            days.jw=days.jw||_d
            end   /*jw*/
      __=daysn
      if narrower  then daysn=__||centre(left(_,3),5)
      if narrowest then daysn=__||center(left(_,2),3)
      end   /*j*/
_yyyy=yyyy; calPuts=0; cv=1; _mm=mm+0; month=word(months,mm)
dy.2=28+ly(_yyyy); dim=dy._mm; _dd=01; dow=dow(_mm,_dd,_yyyy); $dd=dd+0

/*─────────────────────────────now: the business of the building the cal*/
call calGen
               do _j=2 to mc
               if cv_\=='' then do
                                cv=cv+cv_
                                if cv+cv_>=sw then do; cv=1; call calPut
                                                   call fcalPuts;call calPb
                                                   end
                                              else calPuts=0
                                end
                           else do;call calPb;call calPut;call fcalPuts;end
               _mm=_mm+1;  if _mm==13 then do;  _mm=1; _yyyy=_yyyy+1;  end
               month=word(months,_mm); dy.2=28+ly(_yyyy); dim=dy._mm
               dow=dow(_mm,_dd,_yyyy); $dd=0; call calGen
               end   /*_j*/
call fcalPuts
return _

/*─────────────────────────────calGen subroutine────────────────────────*/
calGen: cellX=;cellJ=;cellM=;calCells=0;calline=0
call calPut
call calPutl copies('─',calwidth),"┌┐"; call calHd
call calPutl month ' ' _yyyy          ; call calHd
if narrowest | narrower then call calPutl daysn
                        else do jw=1 for 3
                             if space(days.jw)\=='' then call calPutl days.jw
                             end
calft=1; calfb=0
  do jf=1 for dow-1; call cellDraw calFill,calFill; end
  do jy=1 for dim; call cellDraw jy; end
calfb=1
  do 7; call cellDraw calFill,calFill; end
if sd>32 & \shorter then call calPut
return

/*─────────────────────────────cellDraw subroutine──────────────────────*/
cellDraw: parse arg zz,cdDOY;zz=right(zz,2);calCells=calCells+1
if calCells>7 then do
                   calLine=calLine+1
                   cellX=substr(cellX,2)
                   cellJ=substr(cellJ,2)
                   cellM=substr(cellM,2)
                   cellB=translate(cellX,,")(─-"#)
                   if calLine==1 then call cx
                   call calCsm; call calPutl cellX; call calCsj; call cx
                   cellX=; cellJ=; cellM=; calCells=1
                   end
cdDOY=right(cdDOY,cw); cellM=cellM'│'center('',cw)
cellX=cellX'│'centre(zz,cw); cellJ=cellJ'│'center('',cw)
return

/*═════════════════════════════general 1-line subs══════════════════════*/
abb:arg abbu;parse arg abb;return abbrev(abbu,_,abbl(abb))
abbl:return verify(arg(1)'a',@abc,'M')-1
abbn:parse arg abbn;return abb(abbn)|abb('NO'abbn)
calCsj:if sd>49&\shorter then call calPutl cellB;if sd>24&\short then call calPutl cellJ; return
calCsm:if sd>24&\short then call calPutl cellM;if sd>49&\shorter then call calPutl cellB;return
calHd:if sd>24&\shorter then call calPutl;if sd>32&\shortest then call calPutl;return
calPb:if \grid&shortest then call put chk;return
calPut:calPuts=calPuts+1;maxKalPuts=max(maxKalPuts,calPuts);if symbol('CT.'calPuts)\=='VAR' then ct.calPuts=;ct.calPuts=overlay(arg(1),ct.calPuts,cv);return
calPutl:call calPut copies(' ',cindent)left(arg(2)"│",1)center(arg(1),calwidth)||right('│'arg(2),1);return
cx:cx_='├┤';cx=copies(copies('─',cw)'┼',7);if calft then do;cx=translate(cx,'┬',"┼");calft=0;end;if calfb then do;cx=translate(cx,'┴',"┼");cx_='└┘';calfb=0;end;call calPutl cx,cx_;return
dow:procedure;arg m,d,y;if m<3 then do;m=m+12;y=y-1;end;yl=left(y,2);yr=right(y,2);w=(d+(m+1)*26%10+yr+yr%4+yl%4+5*yl)//7;if w==0 then w=7;return w
er:parse arg _1,_2;call '$ERR' "14"p(_1) p(word(_1,2) !fid(1)) _2;if _1<0 then return _1;exit result
err:call er '-'arg(1),arg(2);return ''
erx:call er '-'arg(1),arg(2);exit ''
fcalPuts: do j=1 for maxKalPuts;call put ct.j;end;ct.=;maxKalPuts=0;calPuts=0;return
int:int=numx(arg(1),arg(2));if \isint(int) then call erx 92,arg(1) arg(2);return int/1
is#:return verify(arg(1),#)==0
isint:return datatype(arg(1),'W')
lower:return translate(arg(1),@abc,@abcU)
ly:arg _;if length(_)==2 then _=hyy||_;ly=_//4==0;if ly==0 then return 0;ly=((_//100\==0)|_//400==0);return ly
na:if arg(1)\=='' then call erx 01,arg(2);parse var ops na ops;if na=='' then call erx 35,_o;return na
nai:return int(na(),_o)
nan:return numx(na(),_o)
no:if arg(1)\=='' then call erx 01,arg(2);return left(_,2)\=='NO'
num:procedure;parse arg x .,f,q;if x=='' then return x;if datatype(x,'N') then return x/1;x=space(translate(x,,','),0);if datatype(x,'N') then return x/1;return numnot()
numnot:if q==1 then return x;if q=='' then call er 53,x f;call erx 53,x f
numx:return num(arg(1),arg(2),1)
p:return word(arg(1),1)
put:_=arg(1);_=translate(_,,'_'chk);if \grid then _=ungrid(_);if lowerCase then _=lower(_);if upperCase then upper _;if shortest&_=' ' then return;call tell _;return
tell:say arg(1);return
ungrid:return translate(arg(1),,"│║─═┤┐└┴┬├┼┘┌╔╗╚╝╟╢╞╡╫╪╤╧╥╨╠╣")
```

'''output''' when using the input of: <tt> 1/1/1969 (noGrid smallest narrowest) </tt>

```txt

                          «Snoopy "picture" here»

     January   1969          February   1969            March   1969
  Su Mo Tu We Th Fr Sa     Su Mo Tu We Th Fr Sa     Su Mo Tu We Th Fr Sa
            1  2  3  4                        1                        1
   5  6  7  8  9 10 11      2  3  4  5  6  7  8      2  3  4  5  6  7  8
  12 13 14 15 16 17 18      9 10 11 12 13 14 15      9 10 11 12 13 14 15
  19 20 21 22 23 24 25     16 17 18 19 20 21 22     16 17 18 19 20 21 22
  26 27 28 29 30 31        23 24 25 26 27 28        23 24 25 26 27 28 29
                                                    30 31

      April   1969              May   1969              June   1969
  Su Mo Tu We Th Fr Sa     Su Mo Tu We Th Fr Sa     Su Mo Tu We Th Fr Sa
         1  2  3  4  5                  1  2  3      1  2  3  4  5  6  7
   6  7  8  9 10 11 12      4  5  6  7  8  9 10      8  9 10 11 12 13 14
  13 14 15 16 17 18 19     11 12 13 14 15 16 17     15 16 17 18 19 20 21
  20 21 22 23 24 25 26     18 19 20 21 22 23 24     22 23 24 25 26 27 28
  27 28 29 30              25 26 27 28 29 30 31     29 30

      July   1969             August   1969           September   1969
  Su Mo Tu We Th Fr Sa     Su Mo Tu We Th Fr Sa     Su Mo Tu We Th Fr Sa
         1  2  3  4  5                     1  2         1  2  3  4  5  6
   6  7  8  9 10 11 12      3  4  5  6  7  8  9      7  8  9 10 11 12 13
  13 14 15 16 17 18 19     10 11 12 13 14 15 16     14 15 16 17 18 19 20
  20 21 22 23 24 25 26     17 18 19 20 21 22 23     21 22 23 24 25 26 27
  27 28 29 30 31           24 25 26 27 28 29 30     28 29 30
                           31

     October   1969          November   1969          December   1969
  Su Mo Tu We Th Fr Sa     Su Mo Tu We Th Fr Sa     Su Mo Tu We Th Fr Sa
            1  2  3  4                        1         1  2  3  4  5  6
   5  6  7  8  9 10 11      2  3  4  5  6  7  8      7  8  9 10 11 12 13
  12 13 14 15 16 17 18      9 10 11 12 13 14 15     14 15 16 17 18 19 20
  19 20 21 22 23 24 25     16 17 18 19 20 21 22     21 22 23 24 25 26 27
  26 27 28 29 30 31        23 24 25 26 27 28 29     28 29 30 31
                           30

```

Output  when using the input of: <tt> 1/1/1969 (smallest narrowest width 156 calSpaces 5) </tt>

A width of 156 was used to illustrate showing a grid, otherwise it would fit in 132 columns.

```txt

 ┌────────────────────┐   ┌────────────────────┐   ┌────────────────────┐   ┌────────────────────┐   ┌────────────────────┐   ┌────────────────────┐
 │   January   1969   │   │  February   1969   │   │    March   1969    │   │    April   1969    │   │     May   1969     │   │    June   1969     │
 │Su Mo Tu We Th Fr Sa│   │Su Mo Tu We Th Fr Sa│   │Su Mo Tu We Th Fr Sa│   │Su Mo Tu We Th Fr Sa│   │Su Mo Tu We Th Fr Sa│   │Su Mo Tu We Th Fr Sa│
 ├──┬──┬──┬──┬──┬──┬──┤   ├──┬──┬──┬──┬──┬──┬──┤   ├──┬──┬──┬──┬──┬──┬──┤   ├──┬──┬──┬──┬──┬──┬──┤   ├──┬──┬──┬──┬──┬──┬──┤   ├──┬──┬──┬──┬──┬──┬──┤
 │  │  │  │ 1│ 2│ 3│ 4│   │  │  │  │  │  │  │ 1│   │  │  │  │  │  │  │ 1│   │  │  │ 1│ 2│ 3│ 4│ 5│   │  │  │  │  │ 1│ 2│ 3│   │ 1│ 2│ 3│ 4│ 5│ 6│ 7│
 ├──┼──┼──┼──┼──┼──┼──┤   ├──┼──┼──┼──┼──┼──┼──┤   ├──┼──┼──┼──┼──┼──┼──┤   ├──┼──┼──┼──┼──┼──┼──┤   ├──┼──┼──┼──┼──┼──┼──┤   ├──┼──┼──┼──┼──┼──┼──┤
 │ 5│ 6│ 7│ 8│ 9│10│11│   │ 2│ 3│ 4│ 5│ 6│ 7│ 8│   │ 2│ 3│ 4│ 5│ 6│ 7│ 8│   │ 6│ 7│ 8│ 9│10│11│12│   │ 4│ 5│ 6│ 7│ 8│ 9│10│   │ 8│ 9│10│11│12│13│14│
 ├──┼──┼──┼──┼──┼──┼──┤   ├──┼──┼──┼──┼──┼──┼──┤   ├──┼──┼──┼──┼──┼──┼──┤   ├──┼──┼──┼──┼──┼──┼──┤   ├──┼──┼──┼──┼──┼──┼──┤   ├──┼──┼──┼──┼──┼──┼──┤
 │12│13│14│15│16│17│18│   │ 9│10│11│12│13│14│15│   │ 9│10│11│12│13│14│15│   │13│14│15│16│17│18│19│   │11│12│13│14│15│16│17│   │15│16│17│18│19│20│21│
 ├──┼──┼──┼──┼──┼──┼──┤   ├──┼──┼──┼──┼──┼──┼──┤   ├──┼──┼──┼──┼──┼──┼──┤   ├──┼──┼──┼──┼──┼──┼──┤   ├──┼──┼──┼──┼──┼──┼──┤   ├──┼──┼──┼──┼──┼──┼──┤
 │19│20│21│22│23│24│25│   │16│17│18│19│20│21│22│   │16│17│18│19│20│21│22│   │20│21│22│23│24│25│26│   │18│19│20│21│22│23│24│   │22│23│24│25│26│27│28│
 ├──┼──┼──┼──┼──┼──┼──┤   ├──┼──┼──┼──┼──┼──┼──┤   ├──┼──┼──┼──┼──┼──┼──┤   ├──┼──┼──┼──┼──┼──┼──┤   ├──┼──┼──┼──┼──┼──┼──┤   ├──┼──┼──┼──┼──┼──┼──┤
 │26│27│28│29│30│31│  │   │23│24│25│26│27│28│  │   │23│24│25│26│27│28│29│   │27│28│29│30│  │  │  │   │25│26│27│28│29│30│31│   │29│30│  │  │  │  │  │
 └──┴──┴──┴──┴──┴──┴──┘   └──┴──┴──┴──┴──┴──┴──┘   ├──┼──┼──┼──┼──┼──┼──┤   └──┴──┴──┴──┴──┴──┴──┘   └──┴──┴──┴──┴──┴──┴──┘   └──┴──┴──┴──┴──┴──┴──┘
                                                   │30│31│  │  │  │  │  │
                                                   └──┴──┴──┴──┴──┴──┴──┘
 ┌────────────────────┐   ┌────────────────────┐   ┌────────────────────┐   ┌────────────────────┐   ┌────────────────────┐   ┌────────────────────┐
 │    July   1969     │   │   August   1969    │   │  September   1969  │   │   October   1969   │   │  November   1969   │   │  December   1969   │
 │Su Mo Tu We Th Fr Sa│   │Su Mo Tu We Th Fr Sa│   │Su Mo Tu We Th Fr Sa│   │Su Mo Tu We Th Fr Sa│   │Su Mo Tu We Th Fr Sa│   │Su Mo Tu We Th Fr Sa│
 ├──┬──┬──┬──┬──┬──┬──┤   ├──┬──┬──┬──┬──┬──┬──┤   ├──┬──┬──┬──┬──┬──┬──┤   ├──┬──┬──┬──┬──┬──┬──┤   ├──┬──┬──┬──┬──┬──┬──┤   ├──┬──┬──┬──┬──┬──┬──┤
 │  │  │ 1│ 2│ 3│ 4│ 5│   │  │  │  │  │  │ 1│ 2│   │  │ 1│ 2│ 3│ 4│ 5│ 6│   │  │  │  │ 1│ 2│ 3│ 4│   │  │  │  │  │  │  │ 1│   │  │ 1│ 2│ 3│ 4│ 5│ 6│
 ├──┼──┼──┼──┼──┼──┼──┤   ├──┼──┼──┼──┼──┼──┼──┤   ├──┼──┼──┼──┼──┼──┼──┤   ├──┼──┼──┼──┼──┼──┼──┤   ├──┼──┼──┼──┼──┼──┼──┤   ├──┼──┼──┼──┼──┼──┼──┤
 │ 6│ 7│ 8│ 9│10│11│12│   │ 3│ 4│ 5│ 6│ 7│ 8│ 9│   │ 7│ 8│ 9│10│11│12│13│   │ 5│ 6│ 7│ 8│ 9│10│11│   │ 2│ 3│ 4│ 5│ 6│ 7│ 8│   │ 7│ 8│ 9│10│11│12│13│
 ├──┼──┼──┼──┼──┼──┼──┤   ├──┼──┼──┼──┼──┼──┼──┤   ├──┼──┼──┼──┼──┼──┼──┤   ├──┼──┼──┼──┼──┼──┼──┤   ├──┼──┼──┼──┼──┼──┼──┤   ├──┼──┼──┼──┼──┼──┼──┤
 │13│14│15│16│17│18│19│   │10│11│12│13│14│15│16│   │14│15│16│17│18│19│20│   │12│13│14│15│16│17│18│   │ 9│10│11│12│13│14│15│   │14│15│16│17│18│19│20│
 ├──┼──┼──┼──┼──┼──┼──┤   ├──┼──┼──┼──┼──┼──┼──┤   ├──┼──┼──┼──┼──┼──┼──┤   ├──┼──┼──┼──┼──┼──┼──┤   ├──┼──┼──┼──┼──┼──┼──┤   ├──┼──┼──┼──┼──┼──┼──┤
 │20│21│22│23│24│25│26│   │17│18│19│20│21│22│23│   │21│22│23│24│25│26│27│   │19│20│21│22│23│24│25│   │16│17│18│19│20│21│22│   │21│22│23│24│25│26│27│
 ├──┼──┼──┼──┼──┼──┼──┤   ├──┼──┼──┼──┼──┼──┼──┤   ├──┼──┼──┼──┼──┼──┼──┤   ├──┼──┼──┼──┼──┼──┼──┤   ├──┼──┼──┼──┼──┼──┼──┤   ├──┼──┼──┼──┼──┼──┼──┤
 │27│28│29│30│31│  │  │   │24│25│26│27│28│29│30│   │28│29│30│  │  │  │  │   │26│27│28│29│30│31│  │   │23│24│25│26│27│28│29│   │28│29│30│31│  │  │  │
 └──┴──┴──┴──┴──┴──┴──┘   ├──┼──┼──┼──┼──┼──┼──┤   └──┴──┴──┴──┴──┴──┴──┘   └──┴──┴──┴──┴──┴──┴──┘   ├──┼──┼──┼──┼──┼──┼──┤   └──┴──┴──┴──┴──┴──┴──┘
                          │31│  │  │  │  │  │  │                                                     │30│  │  │  │  │  │  │
                          └──┴──┴──┴──┴──┴──┴──┘                                                     └──┴──┴──┴──┴──┴──┴──┘
                                                                      ,-~~-.___.
                                                                     / ()=(()   \
                                                                    (   (        0
                                                                     \._\, ,----'
                                                                ##XXXxxxxxxx
                                                                       /  ---'~;
                                                                      /    /~|-
                                                               _____=(   ~~  |______
                                                              /_____________________\
                                                             /_______________________\
                                                            /_________________________\
                                                           /___________________________\
                                                              |____________________|
                                                              |____________________|
                                                              |____________________|
                                                              |                    |

```



## Ring


```ring

# Project : Calendar

load "guilib.ring"
load "stdlib.ring"

new qapp
       {
        win1 = new qwidget() {

                   day = list(12)
                   pos = newlist(12,37)
                   month = list(12)
                   week = list(7)
                   weekday = list(7)
                   button = newlist(7,6)
                   monthsnames = list(12)
                   week = ["Su", "Mo", "Tu", "We", "Th", "Fr", "Sa"]
                   months = ["January", "February", "March", "April", "May", "June", "July",
                                   "August", "September", "October", "November", "December"]
                   daysnew = [[5,1], [6,2], [7,3], [1,4], [2,5], [3,6], [4,7]]
                   mo = [4,0,0,3,5,1,3,6,2,4,0,2]
                   mon = [31,28,31,30,31,30,31,31,30,31,30,31]
                   m2 = (((1969-1900)%7) + floor((1969 - 1900)/4) % 7) % 7
                   for n = 1 to 12
                        month[n] = (mo[n] + m2) % 7
                        x = (month[n] + 1) % 7 + 1
                       for m = 1 to len(daysnew)
                            if daysnew[m][1] = x
                              nr = m
                           ok
                       next
                       day[n] = daysnew[nr][2]
                  next
                  for m = 1 to 12
                       for n = 1 to day[m] - 1
                            pos[m][n] = "  "
                       next
                  next
                  for m = 1 to 12
                       for  n = day[m] to 37
                             if n < (mon[m] + day[m])
                                pos[m][n] = n - day[m] + 1
                             else
                                pos[m][n] = "  "
                             ok
                       next
                  next
                  setwindowtitle("Calendar")
                  setgeometry(100,100,650,800)
                  label1 = new qlabel(win1) {
                              setgeometry(10,10,800,600)
                              settext("")
                  }
                 year = new qpushbutton(win1)
                                                  {
                                                  setgeometry(280,20,60,20)
                                                  year.settext("1969")
                                                  }
                 for n = 1 to 4
                      nr = (n-1)*3+1
                     showmonths(nr)
                 next
                 for n = 1 to 12
                     showweeks(n)
                 next
                 for n = 1 to 12
                     showdays(n)
                 next
                 show()
        }
        exec()
        }

func showmonths(m)
       for n = m to m + 2
            monthsnames[n] = new qpushbutton(win1)
                                        {
                                        if n%3 = 1
                                           col = 120
                                           rownr = floor(n/3)
                                           if rownr = 0
                                              rownr = n/3
                                           ok
                                           if n = 1
                                              row = 40
                                           else
                                              row = 40+rownr*180
                                           ok
                                        else
                                           colnr = n%3
                                           if colnr = 0
                                              colnr = 3
                                           ok
                                           rownr = floor(n/3)
                                           if n%3 = 0
                                              rownr = floor(n/3)-1
                                           ok
                                           col = 120 + (colnr-1)*160
                                           row = 40 + rownr*180
                                        ok
                                        setgeometry(col,row,60,20)
                                        monthsnames[n].settext(months[n])
                                        }
       next

func showweeks(n)
        for m = 1 to 7
             col = m%7
             if col = 0 col = 7 ok
             weekday[m] = new qpushbutton(win1)
                                  {
                                  colnr = n % 3
                                  if colnr = 0
                                     colnr = 3
                                  ok
                                  rownr = floor(n/3)
                                  if n%3 = 0
                                     rownr = floor(n/3)-1
                                  ok
                                  colbegin = 60 + (colnr-1)*160
                                  rowbegin = 60 + (rownr)*180
                                  setgeometry(colbegin+col*20,rowbegin,20,20)
                                  weekday[m].settext(week[m])
                                  }
        next

func showdays(ind)
        rownr = floor(ind/3)
        if ind%3 = 0
           rownr = floor(ind/3)-1
        ok
        rowbegin = 60+rownr*180
        for m = 1 to 6
             for n = 1 to 7
                  col = n%7
                  if col = 0 col = 7 ok
                  row = m
                   button[n][m] = new qpushbutton(win1)
                                         {
                                          if ind%3 = 1
                                             colbegin = 60
                                          elseif ind%3 = 2
                                             colbegin = 220
                                          else
                                             colbegin = 380
                                          ok
                                          setgeometry(colbegin+col*20,rowbegin+row*20,20,20)
                                          nr = (m-1)*7+n
                                          if nr <= 37
                                             if pos[ind][nr] != "  "
                                                button[n][m].settext(string(pos[ind][nr]))
                                             ok
                                          ok
                                          }
             next
        next

```

Output image:

[https://www.dropbox.com/s/sj37yypiq45o5cd/CalmoSoftCalendar.jpg?dl=0 Calendar]


## Ruby

<code>Date</code> class, from the standard library, knows how many days in a month, and which day is Sunday, for both Julian and Gregorian calendars. This program uses <code>Date</code> class, plus its own assumptions, to create the calendar. This program assumes that every year has 12 months and starts with January 1, and every month fits in 6 weeks starting with Sunday.


```ruby
require 'date'

# Creates a calendar of _year_. Returns this calendar as a multi-line
# string fit to _columns_.
def cal(year, columns)

  # Start at January 1.
  #
  # Date::ENGLAND marks the switch from Julian calendar to Gregorian
  # calendar at 1752 September 14. This removes September 3 to 13 from
  # year 1752. (By fortune, it keeps January 1.)
  #
  date = Date.new(year, 1, 1, Date::ENGLAND)

  # Collect calendars of all 12 months.
  months = (1..12).collect do |month|
    rows = [Date::MONTHNAMES[month].center(20), "Su Mo Tu We Th Fr Sa"]

    # Make array of 42 days, starting with Sunday.
    days = []
    date.wday.times { days.push "  " }
    while date.month == month
      days.push("%2d" % date.mday)
      date += 1
    end
    (42 - days.length).times { days.push "  " }

    days.each_slice(7) { |week| rows.push(week.join " ") }
    next rows
  end

  # Calculate months per row (mpr).
  #  1. Divide columns by 22 columns per month, rounded down. (Pretend
  #     to have 2 extra columns; last month uses only 20 columns.)
  #  2. Decrease mpr if 12 months would fit in the same months per
  #     column (mpc). For example, if we can fit 5 mpr and 3 mpc, then
  #     we use 4 mpr and 3 mpc.
  mpr = (columns + 2).div 22
  mpr = 12.div((12 + mpr - 1).div mpr)

  # Use 20 columns per month + 2 spaces between months.
  width = mpr * 22 - 2

  # Join months into calendar.
  rows = ["[Snoopy]".center(width), "#{year}".center(width)]
  months.each_slice(mpr) do |slice|
    slice[0].each_index do |i|
      rows.push(slice.map {|a| a[i]}.join "  ")
    end
  end
  return rows.join("\n")
end


ARGV.length == 1 or abort "usage: #{$0} year"

# Guess width of terminal.
#  1. Obey environment variable COLUMNS.
#  2. Try to require 'io/console' from Ruby 1.9.3.
#  3. Try to run `tput co`.
#  4. Assume 80 columns.
columns = begin Integer(ENV["COLUMNS"] || "")
          rescue
            begin require 'io/console'; IO.console.winsize[1]
            rescue LoadError
              begin Integer(`tput cols`)
              rescue
                80; end; end; end

puts cal(Integer(ARGV[0]), columns)
```


Here is 1969 in 132 columns.


```txt
$ ruby cal.rb 1969
                                                             [Snoopy]
                                                               1969
      January               February               March                 April                  May                   June
Su Mo Tu We Th Fr Sa  Su Mo Tu We Th Fr Sa  Su Mo Tu We Th Fr Sa  Su Mo Tu We Th Fr Sa  Su Mo Tu We Th Fr Sa  Su Mo Tu We Th Fr Sa
          1  2  3  4                     1                     1         1  2  3  4  5               1  2  3   1  2  3  4  5  6  7
 5  6  7  8  9 10 11   2  3  4  5  6  7  8   2  3  4  5  6  7  8   6  7  8  9 10 11 12   4  5  6  7  8  9 10   8  9 10 11 12 13 14
12 13 14 15 16 17 18   9 10 11 12 13 14 15   9 10 11 12 13 14 15  13 14 15 16 17 18 19  11 12 13 14 15 16 17  15 16 17 18 19 20 21
19 20 21 22 23 24 25  16 17 18 19 20 21 22  16 17 18 19 20 21 22  20 21 22 23 24 25 26  18 19 20 21 22 23 24  22 23 24 25 26 27 28
26 27 28 29 30 31     23 24 25 26 27 28     23 24 25 26 27 28 29  27 28 29 30           25 26 27 28 29 30 31  29 30
                                            30 31
        July                 August              September              October               November              December
Su Mo Tu We Th Fr Sa  Su Mo Tu We Th Fr Sa  Su Mo Tu We Th Fr Sa  Su Mo Tu We Th Fr Sa  Su Mo Tu We Th Fr Sa  Su Mo Tu We Th Fr Sa
       1  2  3  4  5                  1  2      1  2  3  4  5  6            1  2  3  4                     1      1  2  3  4  5  6
 6  7  8  9 10 11 12   3  4  5  6  7  8  9   7  8  9 10 11 12 13   5  6  7  8  9 10 11   2  3  4  5  6  7  8   7  8  9 10 11 12 13
13 14 15 16 17 18 19  10 11 12 13 14 15 16  14 15 16 17 18 19 20  12 13 14 15 16 17 18   9 10 11 12 13 14 15  14 15 16 17 18 19 20
20 21 22 23 24 25 26  17 18 19 20 21 22 23  21 22 23 24 25 26 27  19 20 21 22 23 24 25  16 17 18 19 20 21 22  21 22 23 24 25 26 27
27 28 29 30 31        24 25 26 27 28 29 30  28 29 30              26 27 28 29 30 31     23 24 25 26 27 28 29  28 29 30 31
                      31                                                                30
```


The output in 80 columns for the Gregorian reform year (note missing days in september):

```txt
                            [Snoopy]
                              1752
      January               February               March
Su Mo Tu We Th Fr Sa  Su Mo Tu We Th Fr Sa  Su Mo Tu We Th Fr Sa
          1  2  3  4                     1   1  2  3  4  5  6  7
 5  6  7  8  9 10 11   2  3  4  5  6  7  8   8  9 10 11 12 13 14
12 13 14 15 16 17 18   9 10 11 12 13 14 15  15 16 17 18 19 20 21
19 20 21 22 23 24 25  16 17 18 19 20 21 22  22 23 24 25 26 27 28
26 27 28 29 30 31     23 24 25 26 27 28 29  29 30 31

       April                  May                   June
Su Mo Tu We Th Fr Sa  Su Mo Tu We Th Fr Sa  Su Mo Tu We Th Fr Sa
          1  2  3  4                  1  2      1  2  3  4  5  6
 5  6  7  8  9 10 11   3  4  5  6  7  8  9   7  8  9 10 11 12 13
12 13 14 15 16 17 18  10 11 12 13 14 15 16  14 15 16 17 18 19 20
19 20 21 22 23 24 25  17 18 19 20 21 22 23  21 22 23 24 25 26 27
26 27 28 29 30        24 25 26 27 28 29 30  28 29 30
                      31
        July                 August              September
Su Mo Tu We Th Fr Sa  Su Mo Tu We Th Fr Sa  Su Mo Tu We Th Fr Sa
          1  2  3  4                     1         1  2 14 15 16
 5  6  7  8  9 10 11   2  3  4  5  6  7  8  17 18 19 20 21 22 23
12 13 14 15 16 17 18   9 10 11 12 13 14 15  24 25 26 27 28 29 30
19 20 21 22 23 24 25  16 17 18 19 20 21 22
26 27 28 29 30 31     23 24 25 26 27 28 29
                      30 31
      October               November              December
Su Mo Tu We Th Fr Sa  Su Mo Tu We Th Fr Sa  Su Mo Tu We Th Fr Sa
 1  2  3  4  5  6  7            1  2  3  4                  1  2
 8  9 10 11 12 13 14   5  6  7  8  9 10 11   3  4  5  6  7  8  9
15 16 17 18 19 20 21  12 13 14 15 16 17 18  10 11 12 13 14 15 16
22 23 24 25 26 27 28  19 20 21 22 23 24 25  17 18 19 20 21 22 23
29 30 31              26 27 28 29 30        24 25 26 27 28 29 30
                                            31

```



## Rust


```rust
// Assume your binary name is 'calendar'.
// Command line:
// >>$ calendar 2019 150
// First argument: year number.
// Second argument (optional): text area width (in characters).

extern crate chrono;

use std::{env, cmp};
use chrono::{NaiveDate, Datelike};

const MONTH_WIDTH: usize = 22;

fn print_header(months: &[&str]) {
    const DAYS_OF_WEEK: &str = "SU MO TU WE TH FR SA  ";
    println!();
    for m in months {
        print!("{:^20}  ", m);
    }
    println!("\n{}", DAYS_OF_WEEK.repeat(months.len()));
}

fn get_week_str(days: i32, week_num: i32, start_day_of_week: i32) -> Option<String> {
    let start = week_num * 7 - start_day_of_week + 1;
    let end = (week_num + 1) * 7 - start_day_of_week;
    let mut ret = String::with_capacity(MONTH_WIDTH);
    if start > days {
        None
    } else {
        for i in start..(end + 1) {
            if i <= 0 || i > days {
                ret.push_str("  ");
            } else {
                if i < 10 {
                    ret.push_str(" ");
                }
                ret.push_str(&i.to_string());
            }
            ret.push_str(" ");
        }
        ret.push_str(" ");
        Some(ret)
    }
}

fn main() {
    const MONTH_NAMES: [&str; 12] = ["JANUARY", "FEBRUARY", "MARCH", "APRIL", "MAY", "JUNE", "JULY",
                                     "AUGUST", "SEPTEMBER", "OCTOBER", "NOVEMBER", "DECEMBER"];
    const DEFAULT_TEXT_WIDTH: usize = 100;

    let args: Vec<String> = env::args().collect();
    let year: i32 = args[1].parse().expect("The first argument must be a year");
    let width: usize = if args.len() > 2 {
        cmp::max(MONTH_WIDTH, args[2].parse().expect("The second argument should be text width"))
    } else {
        DEFAULT_TEXT_WIDTH
    };
    let months_in_row = width / MONTH_WIDTH;
    let month_rows = if MONTH_NAMES.len() % months_in_row == 0 {
        MONTH_NAMES.len() / months_in_row
    } else {
        MONTH_NAMES.len() / months_in_row + 1
    };

    let start_days_of_week: Vec<i32> =
        (1..13).map(|x| NaiveDate::from_ymd(year, x, 1).weekday().num_days_from_sunday() as i32).collect();

    let month_days: [i32; 12] = if NaiveDate::from_ymd_opt(year, 2, 29).is_some() {
        [31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    } else {
        [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    };

    println!("{year:^w$}", w=width, year=year.to_string());
    for i in 0..month_rows {
        let start = i * months_in_row;
        let end = cmp::min((i + 1) * months_in_row, MONTH_NAMES.len());
        print_header(&MONTH_NAMES[start..end]);
        let mut count = 0;
        let mut row_num = 0;
        while count < months_in_row {
            let mut row_str = String::with_capacity(width);
            for j in start..end {
                match get_week_str(month_days[j], row_num, start_days_of_week[j]) {
                    None => {
                        count += 1;
                        row_str.push_str(&" ".repeat(MONTH_WIDTH));
                    },
                    Some(week_str) => row_str.push_str(&week_str)
                }
            }
            if count < months_in_row {
                println!("{}", row_str);
            }
            row_num += 1;
        }
    }
}

```



## Scala

{{libheader|Scala}}


###  Scala Version 1


[[Category:Scala examples needing attention]]

```Scala
import java.util.{ Calendar, GregorianCalendar }
import language.postfixOps
import collection.mutable.ListBuffer

object CalendarPrint extends App {
  val locd = java.util.Locale.getDefault()
  val cal = new GregorianCalendar
  val monthsMax = cal.getMaximum(Calendar.MONTH)

  def JDKweekDaysToISO(dn: Int) = {
    val nday = dn - Calendar.MONDAY
    if (nday < 0) (dn + Calendar.THURSDAY) else nday
  }

  def daysInMonth(year: Int, monthMinusOne: Int): Int = {
    cal.set(year, monthMinusOne, 1)
    cal.getActualMaximum(Calendar.DAY_OF_MONTH)
  }

  def namesOfMonths() = {
    def f1(i: Int): String = {
      cal.set(2013, i, 1)
      cal.getDisplayName(Calendar.MONTH, Calendar.LONG, locd)
    }
    (0 to monthsMax) map f1
  }

  def offsets(year: Int): List[Int] = {
    val months = cal.getMaximum(Calendar.MONTH)
    def get1stDayOfWeek(i: Int) = {
      cal.set(year, i, 1)
      cal.get(Calendar.DAY_OF_WEEK)
    }
    (0 to monthsMax).toList map get1stDayOfWeek map { i => JDKweekDaysToISO(i) }
  }

  def headerNameOfDays() = {
    val mdow = cal.getDisplayNames(Calendar.DAY_OF_WEEK, Calendar.SHORT, locd) // map days of week
    val it = mdow.keySet.iterator
    val keySet = new ListBuffer[String]
    while (it.hasNext) keySet += it.next
    (keySet.map { key => (JDKweekDaysToISO(mdow.get(key)), key.take(2))
    }.sortWith(_._1 < _._1) map (_._2)).mkString(" ")
  }

  def getGregCal(year: Int) = {
    {

      def dayOfMonth(month: Int) = new Iterator[(Int, Int)] {
        cal.set(year, month, 1)
        var ldom = 0

        def next() = {
          val res = (cal.get(Calendar.MONTH), cal.get(Calendar.DAY_OF_MONTH))
          ldom = res._2
          cal.roll(Calendar.DAY_OF_MONTH, true)
          res
        }

        def hasNext() = (cal.get(Calendar.DAY_OF_MONTH) > ldom)
      }
      var ret: List[(Int, Int)] = Nil
      for (i <- 0 to monthsMax) ret = ret ++ (dayOfMonth(i).toSeq)
      (ret, offsets(year))
    }
  }

  def printCalendar(calendar: (List[(Int, Int)], List[Int]),
                    headerList: List[String] = Nil,
                    printerWidth: Int = 80) = {
    val mw = 20 // month width
    val gwf = 2 // gap width fixed
    val gwm = 2 // gap width minimum
    val fgw = true

    val arr = Array.ofDim[String](6, 7)

    def limits(printerWidth: Int): (Int, Int, Int, Int, Int) = {
      val pw = if (printerWidth < 20) 20 else if (printerWidth > 300) 300 else printerWidth

      // months side by side, gaps sum minimum
      val (msbs, gsm) = {
        val x1 = {
          val c = if (pw / mw > 12) 12 else pw / mw
          val a = (c - 1)
          if (c * mw + a * gwm <= pw) c else a
        } match {
          case 5                      => 4
          case a if (a > 6 && a < 12) => 6
          case other                  => other
        }
        (x1, (x1 - 1) * gwm)
      }

      def getFGW(msbs: Int, gsm: Int) = { val r = (pw - msbs * mw - gsm) / 2; (r, gwf, r) } // fixed gap width
      def getVGW(msbs: Int, gsm: Int) = pw - msbs * mw - gsm match { // variable gap width
        case a if (a < 2 * gwm) => (a / 2, gwm, a / 2)
        case b                  => { val x = (b + gsm) / (msbs + 1); (x, x, x) }
      }

      // left margin, gap width, right margin
      val (lm, gw, rm) = if (fgw) getFGW(msbs, gsm) else getVGW(msbs, gsm)
      (pw, msbs, lm, gw, rm)
    } // def limits(

    val (pw, msbs, lm, gw, rm) = limits(printerWidth)
    val monthsList = (0 to monthsMax).map { m => calendar._1.filter { _._1 == m } }
    val nom = namesOfMonths()
    val hnod = headerNameOfDays

    def fsplit(list: List[(Int, Int)]): List[String] = {
      def fap(p: Int) = (p / 7, p % 7)
      for (i <- 0 until 6) for (j <- 0 until 7) arr(i)(j) = "  "
      for (i <- 0 until list.size)
        arr(fap(i + calendar._2(list(i)._1))._1)(fap(i + calendar._2(list(i)._1))._2) =
          f"${(list(i)._2)}%2d"
      arr.toList.map(_.foldRight("")(_ + " " + _))
    }
    val monthsRows = monthsList.map(fsplit)

    def center(s: String, l: Int): String = {
      (if (s.size >= l) s
      else " " * ((l - s.size) / 2) + s + " " * ((l - s.size) / 2) + " ").substring(0, l)
    }

    val maxMonths = monthsMax + 1

    val rowblocks = (1 to maxMonths / msbs).map { i =>
      (0 to 5).map { j =>
        val lb = new ListBuffer[String]
        val k = (i - 1) * msbs
        (k to k + msbs - 1).map { l => lb += monthsRows(l)(j) }
        lb
      }
    }

    val mheaders = (1 to maxMonths / msbs).
      map { i => (0 to msbs - 1).map { j => center(nom(j + (i - 1) * msbs), 20) } }
    val dowheaders = (1 to maxMonths / msbs).
      map { i => (0 to msbs - 1).map { j => center(hnod, 20) } }

    headerList.foreach(xs => println(center(xs + '\n', pw)))
    (1 to 12 / msbs).foreach { i =>
      println(" " * lm + mheaders(i - 1).foldRight("")(_ + " " * gw + _))
      println(" " * lm + dowheaders(i - 1).foldRight("")(_ + " " * gw + _))
      rowblocks(i - 1).foreach { xs => println(" " * lm + xs.foldRight("")(_ + " " * (gw - 1) + _)) }
      println
    }
  } //  def printCal(

  printCalendar(getGregCal(1969), List("[Snoopy Picture]", "1969"))
  printCalendar(getGregCal(1582), List("[Snoopy Picture]", "1582"), printerWidth = 132)
}
```


{{out}}
<pre style="height:31ex;overflow:scroll">                               [Snoopy Picture]

                                     1969

              January               February               March
        Mo Tu We Th Fr Sa Su  Mo Tu We Th Fr Sa Su  Mo Tu We Th Fr Sa Su
               1  2  3  4  5                  1  2                  1  2
         6  7  8  9 10 11 12   3  4  5  6  7  8  9   3  4  5  6  7  8  9
        13 14 15 16 17 18 19  10 11 12 13 14 15 16  10 11 12 13 14 15 16
        20 21 22 23 24 25 26  17 18 19 20 21 22 23  17 18 19 20 21 22 23
        27 28 29 30 31        24 25 26 27 28        24 25 26 27 28 29 30
                                                    31

               April                  May                   June
        Mo Tu We Th Fr Sa Su  Mo Tu We Th Fr Sa Su  Mo Tu We Th Fr Sa Su
            1  2  3  4  5  6            1  2  3  4                     1
         7  8  9 10 11 12 13   5  6  7  8  9 10 11   2  3  4  5  6  7  8
        14 15 16 17 18 19 20  12 13 14 15 16 17 18   9 10 11 12 13 14 15
        21 22 23 24 25 26 27  19 20 21 22 23 24 25  16 17 18 19 20 21 22
        28 29 30              26 27 28 29 30 31     23 24 25 26 27 28 29
                                                    30

                July                 August              September
        Mo Tu We Th Fr Sa Su  Mo Tu We Th Fr Sa Su  Mo Tu We Th Fr Sa Su
            1  2  3  4  5  6               1  2  3   1  2  3  4  5  6  7
         7  8  9 10 11 12 13   4  5  6  7  8  9 10   8  9 10 11 12 13 14
        14 15 16 17 18 19 20  11 12 13 14 15 16 17  15 16 17 18 19 20 21
        21 22 23 24 25 26 27  18 19 20 21 22 23 24  22 23 24 25 26 27 28
        28 29 30 31           25 26 27 28 29 30 31  29 30


              October               November              December
        Mo Tu We Th Fr Sa Su  Mo Tu We Th Fr Sa Su  Mo Tu We Th Fr Sa Su
               1  2  3  4  5                  1  2   1  2  3  4  5  6  7
         6  7  8  9 10 11 12   3  4  5  6  7  8  9   8  9 10 11 12 13 14
        13 14 15 16 17 18 19  10 11 12 13 14 15 16  15 16 17 18 19 20 21
        20 21 22 23 24 25 26  17 18 19 20 21 22 23  22 23 24 25 26 27 28
        27 28 29 30 31        24 25 26 27 28 29 30  29 30 31


                                                         [Snoopy Picture]

                                                               1582

       January               February               March                 April                  May                   June
 Mo Tu We Th Fr Sa Su  Mo Tu We Th Fr Sa Su  Mo Tu We Th Fr Sa Su  Mo Tu We Th Fr Sa Su  Mo Tu We Th Fr Sa Su  Mo Tu We Th Fr Sa Su
  1  2  3  4  5  6  7            1  2  3  4            1  2  3  4                     1      1  2  3  4  5  6               1  2  3
  8  9 10 11 12 13 14   5  6  7  8  9 10 11   5  6  7  8  9 10 11   2  3  4  5  6  7  8   7  8  9 10 11 12 13   4  5  6  7  8  9 10
 15 16 17 18 19 20 21  12 13 14 15 16 17 18  12 13 14 15 16 17 18   9 10 11 12 13 14 15  14 15 16 17 18 19 20  11 12 13 14 15 16 17
 22 23 24 25 26 27 28  19 20 21 22 23 24 25  19 20 21 22 23 24 25  16 17 18 19 20 21 22  21 22 23 24 25 26 27  18 19 20 21 22 23 24
 29 30 31              26 27 28              26 27 28 29 30 31     23 24 25 26 27 28 29  28 29 30 31           25 26 27 28 29 30
                                                                   30

         July                 August              September              October               November              December
 Mo Tu We Th Fr Sa Su  Mo Tu We Th Fr Sa Su  Mo Tu We Th Fr Sa Su  Mo Tu We Th Fr Sa Su  Mo Tu We Th Fr Sa Su  Mo Tu We Th Fr Sa Su
                    1         1  2  3  4  5                  1  2   1  2  3  4 15 16 17   1  2  3  4  5  6  7         1  2  3  4  5
  2  3  4  5  6  7  8   6  7  8  9 10 11 12   3  4  5  6  7  8  9  18 19 20 21 22 23 24   8  9 10 11 12 13 14   6  7  8  9 10 11 12
  9 10 11 12 13 14 15  13 14 15 16 17 18 19  10 11 12 13 14 15 16  25 26 27 28 29 30 31  15 16 17 18 19 20 21  13 14 15 16 17 18 19
 16 17 18 19 20 21 22  20 21 22 23 24 25 26  17 18 19 20 21 22 23                        22 23 24 25 26 27 28  20 21 22 23 24 25 26
 23 24 25 26 27 28 29  27 28 29 30 31        24 25 26 27 28 29 30                        29 30                 27 28 29 30 31
 30 31


```



###  Scala Version 2

{{works with|Scala|2.10.1}}


```Scala
/**
 * Loosely based on the Ruby implementation.
 *
 * Focuses on immutability and Scala idioms,
 * while trying to remain readable and clean.
 */
object YearCalendarApp extends App with Extras {

  val defaultYear = 1752
  val columns = 86

  val year = args.headOption.map(_.toInt).getOrElse(defaultYear)

  yearCalendarLines(year, columns).foreach(println)

  def yearCalendarLines(year: Int, columns: Int): Seq[String] = {
    // At least one. Divide rest columns by width + 2 spaces (separator)
    val calendarsPerRow = 1 + (columns - 20) / (20 + 2)

    // Use 20 columns per month + 2 spaces between months
    val width = calendarsPerRow * 22 - 2

    List(
      "[Snoopy]".center(width),
      s"${year}".center(width)) ++
      // Get, group, transpose and join calendar lines
      allMonthCalendarLines(year).grouped(calendarsPerRow).flatMap { calGroup =>
        calGroup.transpose.map(stringsInRow => stringsInRow.mkString("  "))
      }
  }

  def allMonthCalendarLines(year: Int): Seq[Seq[String]] = {
    (1 to 12).map { monthNr =>
      val date = MonthCalendar(monthInYear = monthNr, year)

      // Make array of 42 days (7 * 6 weeks max.) starting with Sunday (which is 0)
      val daySlotsInMonth = {
        (Seq().padTo(date.dayOfWeek, "  ") ++
          date.daysInMonth.map { dayNr => "%2d".format(dayNr) }).
          padTo(42, "  ")
      }

      List(
        date.monthName.center(20),
        "Su Mo Tu We Th Fr Sa") ++
        daySlotsInMonth.grouped(7).map { weekSlots => weekSlots.mkString(" ") }
    }
  }

}
```



```Scala
/**
 * This provides extra classes needed for the main
 * algorithm.
 */
trait Extras {

  import java.util.Calendar
  import java.util.GregorianCalendar
  import java.text.SimpleDateFormat
  import java.util.Locale

  import scala.collection.mutable.Buffer

  implicit class MyString(str: String) {
    def center(width: Int): String = {
      val leftSpaces = (width / 2) - (str.length() / 2)
      val rightSpaces = width - (leftSpaces + str.length)
      (" " * leftSpaces) + str + (" " * rightSpaces)
    }
  }

  case class MonthCalendar(monthInYear: Int, year: Int) {
    private val javaCalendar = makeJavaCalendar(year, monthInYear)
    private def makeJavaCalendar(year: Int, monthInYear: Int): Calendar = {
      val calendar = new GregorianCalendar()
      // Actually, other countries changed already in 1582,
      // which is the JDK's default implementation.
      val gregorianDateChangeInEngland = {
        val d = Calendar.getInstance()
        d.set(1752, Calendar.SEPTEMBER, 14)
        d.getTime()
      }
      // For England we need to set this explicitly.
      calendar.setGregorianChange(gregorianDateChangeInEngland)
      calendar.set(Calendar.DAY_OF_MONTH, 1)
      calendar.set(Calendar.YEAR, year)
      calendar.set(Calendar.MONTH, monthInYear - 1)
      calendar
    }
    val monthName = javaCalendar.getDisplayName(Calendar.MONTH, Calendar.LONG, Locale.ENGLISH)
    val dayOfWeek = javaCalendar.get(Calendar.DAY_OF_WEEK) - 1
    val daysInMonth = {
      val tempCal = makeJavaCalendar(year, monthInYear)
      val dayNumbers = Buffer[Int]()
      while (tempCal.get(Calendar.MONTH) == monthInYear - 1) {
        dayNumbers += tempCal.get(Calendar.DAY_OF_MONTH)
        tempCal.add(Calendar.DATE, 1)
      }
      dayNumbers
    }
  }

}
```


Sample output for 1792:


```txt

                                       [Snoopy]
                                         1752
       January              February                March                 April
Su Mo Tu We Th Fr Sa  Su Mo Tu We Th Fr Sa  Su Mo Tu We Th Fr Sa  Su Mo Tu We Th Fr Sa
          1  2  3  4                     1   1  2  3  4  5  6  7            1  2  3  4
 5  6  7  8  9 10 11   2  3  4  5  6  7  8   8  9 10 11 12 13 14   5  6  7  8  9 10 11
12 13 14 15 16 17 18   9 10 11 12 13 14 15  15 16 17 18 19 20 21  12 13 14 15 16 17 18
19 20 21 22 23 24 25  16 17 18 19 20 21 22  22 23 24 25 26 27 28  19 20 21 22 23 24 25
26 27 28 29 30 31     23 24 25 26 27 28 29  29 30 31              26 27 28 29 30

         May                  June                  July                 August
Su Mo Tu We Th Fr Sa  Su Mo Tu We Th Fr Sa  Su Mo Tu We Th Fr Sa  Su Mo Tu We Th Fr Sa
                1  2      1  2  3  4  5  6            1  2  3  4                     1
 3  4  5  6  7  8  9   7  8  9 10 11 12 13   5  6  7  8  9 10 11   2  3  4  5  6  7  8
10 11 12 13 14 15 16  14 15 16 17 18 19 20  12 13 14 15 16 17 18   9 10 11 12 13 14 15
17 18 19 20 21 22 23  21 22 23 24 25 26 27  19 20 21 22 23 24 25  16 17 18 19 20 21 22
24 25 26 27 28 29 30  28 29 30              26 27 28 29 30 31     23 24 25 26 27 28 29
31                                                                30 31
      September              October              November              December
Su Mo Tu We Th Fr Sa  Su Mo Tu We Th Fr Sa  Su Mo Tu We Th Fr Sa  Su Mo Tu We Th Fr Sa
       1  2 14 15 16   1  2  3  4  5  6  7            1  2  3  4                  1  2
17 18 19 20 21 22 23   8  9 10 11 12 13 14   5  6  7  8  9 10 11   3  4  5  6  7  8  9
24 25 26 27 28 29 30  15 16 17 18 19 20 21  12 13 14 15 16 17 18  10 11 12 13 14 15 16
                      22 23 24 25 26 27 28  19 20 21 22 23 24 25  17 18 19 20 21 22 23
                      29 30 31              26 27 28 29 30        24 25 26 27 28 29 30
                                                                  31

```



## Seed7


```seed7
$ include "seed7_05.s7i";
  include "time.s7i";

const func string: center (in string: stri, in integer: length) is
  return ("" lpad (length - length(stri)) div 2 <& stri) rpad length;

const proc: printCalendar (in integer: year, in integer: cols) is func
  local
    var time: date is time.value;
    var integer: dayOfWeek is 0;
    const array string: monthNames is [] ("January", "February", "March", "April", "May", "June",
        "July", "August", "September", "October", "November", "December");
    var array array string: monthTable is 12 times 9 times "";
    var string: str is "";
    var integer: month is 0;
    var integer: position is 0;
    var integer: row is 0;
    var integer: column is 0;
    var integer: line is 0;
  begin
    for month range 1 to 12 do
      monthTable[month][1] := " " & center(monthNames[month], 20);
      monthTable[month][2] := " Mo Tu We Th Fr Sa Su";
      date := date(year, month, 1);
      dayOfWeek := dayOfWeek(date);
      for position range 1 to 43 do
        if position >= dayOfWeek and position - dayOfWeek < daysInMonth(date.year, date.month) then
          str := succ(position - dayOfWeek) lpad 3;
        else
          str := "" lpad 3;
        end if;
        monthTable[month][3 + pred(position) div 7] &:= str;
      end for;
    end for;
    writeln(center("[Snoopy Picture]", cols * 24 + 4));
    writeln(center(str(year),cols * 24 + 4));
    writeln;
    for row range 1 to succ(11 div cols) do
      for line range 1 to 9 do
        for column range 1 to cols do
          if pred(row) * cols + column <= 12 then
            write("   " & monthTable[pred(row) * cols + column][line]);
          end if;
        end for;
        writeln;
      end for;
    end for;
  end func;

const proc: main is func
  begin
    printCalendar(1969, 3);
  end func;
```


Original source: [http://seed7.sourceforge.net/algorith/date.htm#calendar]

The output of this program is:

```txt

                              [Snoopy Picture]
                                    1969

          January                 February                 March
    Mo Tu We Th Fr Sa Su    Mo Tu We Th Fr Sa Su    Mo Tu We Th Fr Sa Su
           1  2  3  4  5                    1  2                    1  2
     6  7  8  9 10 11 12     3  4  5  6  7  8  9     3  4  5  6  7  8  9
    13 14 15 16 17 18 19    10 11 12 13 14 15 16    10 11 12 13 14 15 16
    20 21 22 23 24 25 26    17 18 19 20 21 22 23    17 18 19 20 21 22 23
    27 28 29 30 31          24 25 26 27 28          24 25 26 27 28 29 30
                                                    31

           April                    May                     June
    Mo Tu We Th Fr Sa Su    Mo Tu We Th Fr Sa Su    Mo Tu We Th Fr Sa Su
        1  2  3  4  5  6              1  2  3  4                       1
     7  8  9 10 11 12 13     5  6  7  8  9 10 11     2  3  4  5  6  7  8
    14 15 16 17 18 19 20    12 13 14 15 16 17 18     9 10 11 12 13 14 15
    21 22 23 24 25 26 27    19 20 21 22 23 24 25    16 17 18 19 20 21 22
    28 29 30                26 27 28 29 30 31       23 24 25 26 27 28 29
                                                    30

            July                   August                September
    Mo Tu We Th Fr Sa Su    Mo Tu We Th Fr Sa Su    Mo Tu We Th Fr Sa Su
        1  2  3  4  5  6                 1  2  3     1  2  3  4  5  6  7
     7  8  9 10 11 12 13     4  5  6  7  8  9 10     8  9 10 11 12 13 14
    14 15 16 17 18 19 20    11 12 13 14 15 16 17    15 16 17 18 19 20 21
    21 22 23 24 25 26 27    18 19 20 21 22 23 24    22 23 24 25 26 27 28
    28 29 30 31             25 26 27 28 29 30 31    29 30


          October                 November                December
    Mo Tu We Th Fr Sa Su    Mo Tu We Th Fr Sa Su    Mo Tu We Th Fr Sa Su
           1  2  3  4  5                    1  2     1  2  3  4  5  6  7
     6  7  8  9 10 11 12     3  4  5  6  7  8  9     8  9 10 11 12 13 14
    13 14 15 16 17 18 19    10 11 12 13 14 15 16    15 16 17 18 19 20 21
    20 21 22 23 24 25 26    17 18 19 20 21 22 23    22 23 24 25 26 27 28
    27 28 29 30 31          24 25 26 27 28 29 30    29 30 31



```



## Sidef

{{trans|Perl 6}}

```ruby
require('DateTime')

define months_per_col = 3
define week_day_names = <Mo Tu We Th Fr Sa Su>
define month_names = <Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec>

func fmt_month (year, month) {
    var str = sprintf("%-20s\n", month_names[month-1])
    str += week_day_names.join(' ')+"\n"

    var dt = %s<DateTime>
    var date = dt.new(year => year, month => month)
    var week_day = date.day_of_week
    str += (week_day-1 `of` "  " -> join(" "))

    var last_day = dt.last_day_of_month(year => year, month => month).day
    for day (date.day .. last_day) {
        date = dt.new(year => year, month => month, day => day)
        str += " " if (week_day ~~ (2..7))
        if (week_day == 8) {
            str += "\n"
            week_day = 1
        }
        str += sprintf("%2d", day)
        ++week_day
    }
    str += " " if (week_day < 8)
    str += (8-week_day `of` "  " -> join(" "))
    str += "\n"
}

func fmt_year (year) {
    var month_strs = 12.of {|i| fmt_month(year, i+1).lines }

    var str = (' '*30 + year + "\n")
    for month (0..11 `by` months_per_col) {
        while (month_strs[month]) {
            for i (1..months_per_col) {
                month_strs[month + i - 1] || next
                str += month_strs[month + i - 1].shift
                str += ' '*3
            }
            str += "\n"
        }
        str += "\n"
    }

    return str
}

print fmt_year(ARGV ? Number(ARGV[0]) : 1969)
```

{{out}}

```txt

                              1969
Jan                    Feb                    Mar
Mo Tu We Th Fr Sa Su   Mo Tu We Th Fr Sa Su   Mo Tu We Th Fr Sa Su
       1  2  3  4  5                   1  2                   1  2
 6  7  8  9 10 11 12    3  4  5  6  7  8  9    3  4  5  6  7  8  9
13 14 15 16 17 18 19   10 11 12 13 14 15 16   10 11 12 13 14 15 16
20 21 22 23 24 25 26   17 18 19 20 21 22 23   17 18 19 20 21 22 23
27 28 29 30 31         24 25 26 27 28         24 25 26 27 28 29 30

Apr                    May                    Jun
Mo Tu We Th Fr Sa Su   Mo Tu We Th Fr Sa Su   Mo Tu We Th Fr Sa Su
    1  2  3  4  5  6             1  2  3  4                      1
 7  8  9 10 11 12 13    5  6  7  8  9 10 11    2  3  4  5  6  7  8
14 15 16 17 18 19 20   12 13 14 15 16 17 18    9 10 11 12 13 14 15
21 22 23 24 25 26 27   19 20 21 22 23 24 25   16 17 18 19 20 21 22
28 29 30               26 27 28 29 30 31      23 24 25 26 27 28 29

Jul                    Aug                    Sep
Mo Tu We Th Fr Sa Su   Mo Tu We Th Fr Sa Su   Mo Tu We Th Fr Sa Su
    1  2  3  4  5  6                1  2  3    1  2  3  4  5  6  7
 7  8  9 10 11 12 13    4  5  6  7  8  9 10    8  9 10 11 12 13 14
14 15 16 17 18 19 20   11 12 13 14 15 16 17   15 16 17 18 19 20 21
21 22 23 24 25 26 27   18 19 20 21 22 23 24   22 23 24 25 26 27 28
28 29 30 31            25 26 27 28 29 30 31   29 30

Oct                    Nov                    Dec
Mo Tu We Th Fr Sa Su   Mo Tu We Th Fr Sa Su   Mo Tu We Th Fr Sa Su
       1  2  3  4  5                   1  2    1  2  3  4  5  6  7
 6  7  8  9 10 11 12    3  4  5  6  7  8  9    8  9 10 11 12 13 14
13 14 15 16 17 18 19   10 11 12 13 14 15 16   15 16 17 18 19 20 21
20 21 22 23 24 25 26   17 18 19 20 21 22 23   22 23 24 25 26 27 28
27 28 29 30 31         24 25 26 27 28 29 30   29 30 31

```


## Simula

{{trans|C}}
The subtleties of the post-increment logic which is used in the c program took me some time to detect.
For symmetry I also implemented the pre-increment.

```simula
BEGIN
    INTEGER WIDTH, YEAR;
    INTEGER COLS, LEAD, GAP;
    TEXT ARRAY WDAYS(0:6);
    CLASS MONTH(MNAME); TEXT MNAME;
    BEGIN INTEGER DAYS, START_WDAY, AT_POS;
    END MONTH;
    REF(MONTH) ARRAY MONTHS(0:11);
    WIDTH := 80; YEAR := 1969;

    BEGIN
        TEXT T;
        INTEGER I, M;
        FOR T :- "Su", "Mo", "Tu", "We", "Th", "Fr", "Sa" DO BEGIN
            WDAYS(I) :- T; I := I+1;
        END;
        I := 0;
        FOR T :- "January", "February", "March",
                 "April", "May", "June",
                 "July", "August", "September",
                 "October", "November", "December" DO BEGIN
            MONTHS(I) :- NEW MONTH(T); I := I+1;
        END;
        I := 0;
        FOR M := 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 DO BEGIN
            MONTHS(I).DAYS := M; I := I+1;
        END;
    END;

    BEGIN

        PROCEDURE SPACE(N); INTEGER N;
        BEGIN
            WHILE N > 0 DO BEGIN
                OUTCHAR(' '); N := N-1;
            END;
        END SPACE;

        PROCEDURE INIT_MONTHS;
        BEGIN
            INTEGER I;

            IF MOD(YEAR,4) = 0 AND MOD(YEAR,100) <> 0 OR MOD(YEAR,400) = 0 THEN
                MONTHS(1).DAYS := 29;

            YEAR := YEAR-1;
            MONTHS(0).START_WDAY
                := MOD(YEAR * 365 + YEAR//4 - YEAR//100 + YEAR//400 + 1, 7);

            FOR I := 1 STEP 1 UNTIL 12-1 DO
                MONTHS(I).START_WDAY :=
                    MOD(MONTHS(I-1).START_WDAY + MONTHS(I-1).DAYS, 7);

            COLS := (WIDTH + 2) // 22;
            WHILE MOD(12,COLS) <> 0 DO
                COLS := COLS-1;
            GAP := IF COLS - 1 <> 0 THEN (WIDTH - 20 * COLS) // (COLS - 1) ELSE 0;
            IF GAP > 4 THEN
                GAP := 4;
            LEAD := (WIDTH - (20 + GAP) * COLS + GAP + 1) // 2;
            YEAR := YEAR+1;
        END INIT_MONTHS;

        PROCEDURE PRINT_ROW(ROW); INTEGER ROW;
        BEGIN
            INTEGER C, I, FROM, UP_TO;
            INTEGER PROCEDURE PREINCREMENT(I); NAME I; INTEGER I;
            BEGIN I := I+1; PREINCREMENT := I;
            END PREINCREMENT;
            INTEGER PROCEDURE POSTINCREMENT(I); NAME I; INTEGER I;
            BEGIN POSTINCREMENT := I; I := I+1;
            END POSTINCREMENT;
            FROM := ROW * COLS;
            UP_TO := FROM + COLS;
            SPACE(LEAD);
            FOR C := FROM STEP 1 UNTIL UP_TO-1 DO BEGIN
                I := MONTHS(C).MNAME.LENGTH;
                SPACE((20 - I)//2);
                OUTTEXT(MONTHS(C).MNAME);
                SPACE(20 - I - (20 - I)//2 + (IF C = UP_TO - 1 THEN 0 ELSE GAP));
            END;
            OUTIMAGE;

            SPACE(LEAD);
            FOR C := FROM STEP 1 UNTIL UP_TO-1 DO BEGIN
                FOR I := 0 STEP 1 UNTIL 7-1 DO BEGIN
                    OUTTEXT(WDAYS(I)); OUTTEXT(IF I = 6 THEN "" ELSE " ");
                END;
                IF C < UP_TO - 1 THEN
                    SPACE(GAP)
                ELSE
                    OUTIMAGE;
            END;

            WHILE TRUE DO BEGIN
                FOR C := FROM STEP 1 UNTIL UP_TO-1 DO
                    IF MONTHS(C).AT_POS < MONTHS(C).DAYS THEN
                        GO TO IBREAK;
            IBREAK:
                IF C = UP_TO THEN
                    GO TO OBREAK;

                SPACE(LEAD);
                FOR C := FROM STEP 1 UNTIL UP_TO-1 DO BEGIN
                    FOR I := 0 STEP 1 UNTIL MONTHS(C).START_WDAY-1 DO
                        SPACE(3);
                    WHILE POSTINCREMENT(I) < 7 AND MONTHS(C).AT_POS < MONTHS(C).DAYS DO BEGIN
                        OUTINT(PREINCREMENT(MONTHS(C).AT_POS),2);
                        IF I < 7 OR C < UP_TO - 1 THEN
                            SPACE(1);
                    END;
                    WHILE POSTINCREMENT(I) <= 7 AND C < UP_TO-1 DO
                        SPACE(3);
                    IF C < UP_TO - 1 THEN
                        SPACE(GAP - 1);
                    MONTHS(C).START_WDAY := 0;
                END;
                OUTIMAGE;
            END;
        OBREAK:
            OUTIMAGE;
        END PRINT_ROW;

        PROCEDURE PRINT_YEAR;
        BEGIN
            INTEGER ROW;
            TEXT BUF;
            INTEGER STRLEN;
            BUF :- BLANKS(32); BUF.PUTINT(YEAR); BUF.SETPOS(1);
            WHILE BUF.MORE AND THEN BUF.GETCHAR = ' ' DO
                STRLEN := STRLEN+1;
            BUF :- BUF.SUB(STRLEN+1, 32-STRLEN);
            SPACE((WIDTH - BUF.LENGTH) // 2);
            OUTTEXT(BUF); OUTIMAGE; OUTIMAGE;
            WHILE ROW * COLS < 12 DO BEGIN
                PRINT_ROW(ROW);
                ROW := ROW+1;
            END;
        END PRINT_YEAR;

        INIT_MONTHS;
        PRINT_YEAR;
    END;
END;
```

{{out}}

```txt

                                      1969

            January                 February                 March
      Su Mo Tu We Th Fr Sa    Su Mo Tu We Th Fr Sa    Su Mo Tu We Th Fr Sa
                1  2  3  4                       1                       1
       5  6  7  8  9 10 11     2  3  4  5  6  7  8     2  3  4  5  6  7  8
      12 13 14 15 16 17 18     9 10 11 12 13 14 15     9 10 11 12 13 14 15
      19 20 21 22 23 24 25    16 17 18 19 20 21 22    16 17 18 19 20 21 22
      26 27 28 29 30 31       23 24 25 26 27 28       23 24 25 26 27 28 29
                                                      30 31

             April                    May                     June
      Su Mo Tu We Th Fr Sa    Su Mo Tu We Th Fr Sa    Su Mo Tu We Th Fr Sa
             1  2  3  4  5                 1  2  3     1  2  3  4  5  6  7
       6  7  8  9 10 11 12     4  5  6  7  8  9 10     8  9 10 11 12 13 14
      13 14 15 16 17 18 19    11 12 13 14 15 16 17    15 16 17 18 19 20 21
      20 21 22 23 24 25 26    18 19 20 21 22 23 24    22 23 24 25 26 27 28
      27 28 29 30             25 26 27 28 29 30 31    29 30

              July                   August                September
      Su Mo Tu We Th Fr Sa    Su Mo Tu We Th Fr Sa    Su Mo Tu We Th Fr Sa
             1  2  3  4  5                    1  2        1  2  3  4  5  6
       6  7  8  9 10 11 12     3  4  5  6  7  8  9     7  8  9 10 11 12 13
      13 14 15 16 17 18 19    10 11 12 13 14 15 16    14 15 16 17 18 19 20
      20 21 22 23 24 25 26    17 18 19 20 21 22 23    21 22 23 24 25 26 27
      27 28 29 30 31          24 25 26 27 28 29 30    28 29 30
                              31

            October                 November                December
      Su Mo Tu We Th Fr Sa    Su Mo Tu We Th Fr Sa    Su Mo Tu We Th Fr Sa
                1  2  3  4                       1        1  2  3  4  5  6
       5  6  7  8  9 10 11     2  3  4  5  6  7  8     7  8  9 10 11 12 13
      12 13 14 15 16 17 18     9 10 11 12 13 14 15    14 15 16 17 18 19 20
      19 20 21 22 23 24 25    16 17 18 19 20 21 22    21 22 23 24 25 26 27
      26 27 28 29 30 31       23 24 25 26 27 28 29    28 29 30 31
                              30


```


## Smalltalk


This implementation has been developed using '''Cuis Smalltalk''' [https://github.com/Cuis-Smalltalk/Cuis-Smalltalk-Dev] with '''Aconcagua''' loaded.
To run it, evaluate:
```smalltalk>CalendarPrinter printOnTranscriptForYearNumber: 1969</lang


```Smalltalk

"Instance Variables:
- yearToPrint: The year to print the calendar of
- stream: The stream used to print the calendar
- monthsOfYear: A collection of all the months of yearToPrint, for example January 1969, February 1969, and so on
- currentMonths: Group of 3 months under print, for example from January to March or April to June, and so on
- monthOfYearCurrentDate: A collection that associates a month with the date to print for that month. It is used to
  know which day number should be printed for each month when iterating over the day of weeks of each row"
Object subclass: #CalendarPrinter
	instanceVariableNames: 'yearToPrint stream monthsOfYear currentMonths monthOfYearCurrentDate'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'CalendarPrinter'

CalendarPrinter class>>printOnTranscriptForYearNumber: aYearNumber
	(self for: (GregorianYear number: aYearNumber) on: Transcript) value

CalendarPrinter class>>for: aYear on: aStream
  	^self new initializeFor: aYear on: aStream

initializeFor: aYear on: aStream
	yearToPrint := aYear.
	stream := aStream.
	monthsOfYear := yearToPrint months.
	monthOfYearCurrentDate := monthsOfYear collect: [ :aMonthOfYear | aMonthOfYear firstDate ].

value
	"Prints the year number (header) and then its months"
	self
		printHeader;
		printMonths.

printHeader
	"Prints the year number centered"
	self center: yearToPrint number printString in: self numberOfCharactersPerLine.
	stream newLine; newLine.

printMonths
	"Prints each group of 3 months, for example from January to March, then April to June and so on"
	(January to: December by: 3*month) do: [ :aMonth | self printMonthsStartingOn: aMonth ].

printMonthsStartingOn: aMonth
	"For each group of 3 months starting in aMonth, prints the name of the month (header),
	the name of the days of the week (Mo Tu ...), and then the day numbers"
	currentMonths := aMonth to: aMonth next next.
	self
		printMonthsHeader;
		printDaysOfWeekHeader;
		printDayNumbers.

printMonthsHeader
	"Prints the current group of 3 months names, centered"
	currentMonths
		do: [ :currentMonth | self center: currentMonth printString in: self numberOfCharactersPerMonth ]
		separatedBy: [ stream space: 3 ].
	stream newLine.

printDaysOfWeekHeader
	"Prints the names of the days of week for each month of the current group of 3 months"
	currentMonths
		do: [ :currentMonth | self printOneMonthDaysOfWeekHeader ]
		separatedBy: [ stream space: 3 ].
	stream newLine.

printOneMonthDaysOfWeekHeader
	"Prints the name of the days of week"
	(Sunday to: Saturday)
		do: [ :aDayOfWeek | stream nextPutAll: (aDayOfWeek printString first: 2) ]
		separatedBy: [ stream space ]

printDayNumbers
	"While there are day numbers to print, prints them in a row"
	[self hasDayNumbersToPrint] whileTrue: [ self printDayNumbersRow ].

hasDayNumbersToPrint
	"If any of the group of 3 months currently printing has day numbers to print returns true, otherwise false"
	^currentMonths anySatisfy: [ :currentMonth | self isCurrentDateAtSameMonthOfYearAs: currentMonth ]

isCurrentDateAtSameMonthOfYearAs: currentMonth
	"Returns true if the date to print for currentMonth is actually for the currentMonth"
	^(self currentDateOf: currentMonth) month = currentMonth

printDayNumbersRow
	"For each month of the group of 3, prints a row of day numbers"
	currentMonths
		do: [ :currentMonth | self printDayNumbersRowOf: currentMonth ]
		separatedBy: [ stream space: 3 ].
	stream newLine

printDayNumbersRowOf: currentMonth
	"Prints the day numbers of the current week"
	(Sunday to: Saturday)
		do: [ :aDayOfWeek | self printDayNumberOf: currentMonth for: aDayOfWeek ]
		separatedBy: [ stream space ]

printDayNumberOf: currentMonth for: aDayOfWeek
	"If the current date of the current month corresponds to aDayOfWeeks, prints its day number,
	if not, leaves a space
	This is important to leave the spaces in the first row and last row for those day of week
	that do not have a day number related"
	| currentDate |
	currentDate := self currentDateOf: currentMonth.
	(self hasToPrint: currentDate of: currentMonth for: aDayOfWeek)
		ifTrue: [
			currentDate dayNumber printOn: stream length: 2 zeroPadded: false.
			self calculateNextCurrentDateOf: currentMonth ]
		ifFalse: [ stream space: 2 ]! !

hasToPrint: aDate of: aCurrentMonth for: aDayOfWeek
	"Returns whether aDate is part of aCurrentMonth and its day of week is aDayOfWeek.
	It is used to decide whether to print the date day number or leave an space"
	^(aDate month = aCurrentMonth) and: [aDate day = aDayOfWeek]

currentDateOf: currentMonth
	"Returns the date to print for currentMonth"
	^monthOfYearCurrentDate at: currentMonth number

calculateNextCurrentDateOf: currentMonth
	"Changes the date to print of the currentMonth to the next date"
	monthOfYearCurrentDate at: currentMonth number put: (self currentDateOf: currentMonth) next

center: stringToCenter in: aNumberOfCharacters
	"Prints stringToCenter centered in a line of aNumberOfCharacters total"
	| centerStart |
	centerStart := aNumberOfCharacters - stringToCenter size // 2.
	stream
		space: centerStart;
		nextPutAll: stringToCenter;
		space: aNumberOfCharacters - centerStart - stringToCenter size

numberOfCharactersPerLine
	"Returns the number of characters per line"
	^66

numberOfCharactersPerMonth
	"Returns the number of character per month"
	^20

```

{{out}}

```txt

                               1969
      January                February                March
Su Mo Tu We Th Fr Sa   Su Mo Tu We Th Fr Sa   Su Mo Tu We Th Fr Sa
          1  2  3  4                      1                      1
 5  6  7  8  9 10 11    2  3  4  5  6  7  8    2  3  4  5  6  7  8
12 13 14 15 16 17 18    9 10 11 12 13 14 15    9 10 11 12 13 14 15
19 20 21 22 23 24 25   16 17 18 19 20 21 22   16 17 18 19 20 21 22
26 27 28 29 30 31      23 24 25 26 27 28      23 24 25 26 27 28 29
                                              30 31
       April                   May                    June
Su Mo Tu We Th Fr Sa   Su Mo Tu We Th Fr Sa   Su Mo Tu We Th Fr Sa
       1  2  3  4  5                1  2  3    1  2  3  4  5  6  7
 6  7  8  9 10 11 12    4  5  6  7  8  9 10    8  9 10 11 12 13 14
13 14 15 16 17 18 19   11 12 13 14 15 16 17   15 16 17 18 19 20 21
20 21 22 23 24 25 26   18 19 20 21 22 23 24   22 23 24 25 26 27 28
27 28 29 30            25 26 27 28 29 30 31   29 30
        July                  August               September
Su Mo Tu We Th Fr Sa   Su Mo Tu We Th Fr Sa   Su Mo Tu We Th Fr Sa
       1  2  3  4  5                   1  2       1  2  3  4  5  6
 6  7  8  9 10 11 12    3  4  5  6  7  8  9    7  8  9 10 11 12 13
13 14 15 16 17 18 19   10 11 12 13 14 15 16   14 15 16 17 18 19 20
20 21 22 23 24 25 26   17 18 19 20 21 22 23   21 22 23 24 25 26 27
27 28 29 30 31         24 25 26 27 28 29 30   28 29 30
                       31
      October                November               December
Su Mo Tu We Th Fr Sa   Su Mo Tu We Th Fr Sa   Su Mo Tu We Th Fr Sa
          1  2  3  4                      1       1  2  3  4  5  6
 5  6  7  8  9 10 11    2  3  4  5  6  7  8    7  8  9 10 11 12 13
12 13 14 15 16 17 18    9 10 11 12 13 14 15   14 15 16 17 18 19 20
19 20 21 22 23 24 25   16 17 18 19 20 21 22   21 22 23 24 25 26 27
26 27 28 29 30 31      23 24 25 26 27 28 29   28 29 30 31
                       30

```



## SPL


```spl
year = 1969
#.output(#.str(year,">68<"))
> row, 0..3
  > col, 1..3
    cal[col] = mcal(row*3+col,year)
    size = #.size(cal[col],1)
  <
  > i, 1..8
    str = ""
    > col, 1..3
      ? col>1, str += "    "
      line = ""
      ? #.size(cal[col],1)!<i, line = cal[col][i]
      str += #.str(line,"<20<")
    <
    #.output(str)
  <
<
mcal(m,y)=
  months = ["January","February","March","April","May","June","July","August","September","October","November","December"]
  days = [31,28,31,30,31,30,31,31,30,31,30,31]
  ? y%4=0, days[2] = 29
  lines = #.array(0)
  lines[1] = #.str(months[m],">20<")
  lines[2] = "Mo Tu We Th Fr Sa Su"
  n = 3
  > i, 1..#.weekday(1,m,y)-1
    lines[n] += "   "
  <
  > d, 1..days[m]
    wd = #.weekday(d,m,y)
    lines[n] += #.str(d,">2>")
    ? wd<7, lines[n] += " "
    ? wd=7, n += 1
  <
  <= lines
.
```

{{out}}

```txt

                                1969
      January                 February                 March
Mo Tu We Th Fr Sa Su    Mo Tu We Th Fr Sa Su    Mo Tu We Th Fr Sa Su
       1  2  3  4  5                    1  2                    1  2
 6  7  8  9 10 11 12     3  4  5  6  7  8  9     3  4  5  6  7  8  9
13 14 15 16 17 18 19    10 11 12 13 14 15 16    10 11 12 13 14 15 16
20 21 22 23 24 25 26    17 18 19 20 21 22 23    17 18 19 20 21 22 23
27 28 29 30 31          24 25 26 27 28          24 25 26 27 28 29 30
                                                31
       April                    May                     June
Mo Tu We Th Fr Sa Su    Mo Tu We Th Fr Sa Su    Mo Tu We Th Fr Sa Su
    1  2  3  4  5  6              1  2  3  4                       1
 7  8  9 10 11 12 13     5  6  7  8  9 10 11     2  3  4  5  6  7  8
14 15 16 17 18 19 20    12 13 14 15 16 17 18     9 10 11 12 13 14 15
21 22 23 24 25 26 27    19 20 21 22 23 24 25    16 17 18 19 20 21 22
28 29 30                26 27 28 29 30 31       23 24 25 26 27 28 29
                                                30
        July                   August                September
Mo Tu We Th Fr Sa Su    Mo Tu We Th Fr Sa Su    Mo Tu We Th Fr Sa Su
    1  2  3  4  5  6                 1  2  3     1  2  3  4  5  6  7
 7  8  9 10 11 12 13     4  5  6  7  8  9 10     8  9 10 11 12 13 14
14 15 16 17 18 19 20    11 12 13 14 15 16 17    15 16 17 18 19 20 21
21 22 23 24 25 26 27    18 19 20 21 22 23 24    22 23 24 25 26 27 28
28 29 30 31             25 26 27 28 29 30 31    29 30

      October                 November                December
Mo Tu We Th Fr Sa Su    Mo Tu We Th Fr Sa Su    Mo Tu We Th Fr Sa Su
       1  2  3  4  5                    1  2     1  2  3  4  5  6  7
 6  7  8  9 10 11 12     3  4  5  6  7  8  9     8  9 10 11 12 13 14
13 14 15 16 17 18 19    10 11 12 13 14 15 16    15 16 17 18 19 20 21
20 21 22 23 24 25 26    17 18 19 20 21 22 23    22 23 24 25 26 27 28
27 28 29 30 31          24 25 26 27 28 29 30    29 30 31


```



## Tcl

Due to the prevalence of 80 column devices instead of 132 column ones, this code produces 3 months at a time instead of 4.

```tcl
package require Tcl 8.5

# Produce information about the days in a month, without any assumptions about
# what those days actually are.
proc calMonthDays {timezone locale year month} {
    set days {}
    set moment [clock scan [format "%04d-%02d-00 12:00" $year $month] \
	    -timezone $timezone -locale $locale -format "%Y-%m-%d %H:%M"]
    while 1 {
	set moment [clock add $moment 1 day]
	lassign [clock format $moment -timezone $timezone -locale $locale \
		-format "%m %d %u"] m d dow
	if {[scan $m %d] != $month} {
	    return $days
	}
	lappend days $moment [scan $d %d] $dow
    }
}

proc calMonth {year month timezone locale} {
    set dow 0
    set line ""
    set lines {}
    foreach {t day dayofweek} [calMonthDays $timezone $locale $year $month] {
	if {![llength $lines]} {lappend lines $t}
	if {$dow > $dayofweek} {
	    lappend lines [string trimright $line]
	    set line ""
	    set dow 0
	}
	while {$dow < $dayofweek-1} {
	    append line "   "
	    incr dow
	}
	append line [format "%2d " $day]
	set dow $dayofweek
    }
    lappend lines [string trimright $line]
}

proc cal3Month {year month timezone locale} {
    # Extract the month data
    set d1 [lassign [calMonth $year $month $timezone $locale] t1]; incr month
    set d2 [lassign [calMonth $year $month $timezone $locale] t2]; incr month
    set d3 [lassign [calMonth $year $month $timezone $locale] t3]
    # Print the header line of month names
    foreach t [list $t1 $t2 $t3] {
	set m [clock format $t -timezone $timezone -locale $locale -format "%B"]
	set l [expr {10 + [string length $m]/2}]
	puts -nonewline [format "%-25s" [format "%*s" $l $m]]
    }
    puts ""
    # Print the month days
    foreach l1 $d1 l2 $d2 l3 $d3 {
	puts [format "%-25s%-25s%s" $l1 $l2 $l3]
    }
}

proc cal {{year ""} {timezone :localtime} {locale en}} {
    if {$year eq ""} {
	set year [clock format [clock seconds] -format %Y]
    }
    puts [format "%40s" "-- $year --"]
    foreach m {1 4 7 10} {
	puts ""
	cal3Month $year $m $timezone $locale
    }
}

proc snoopy {} {
    puts [format "%43s\n" {[Snoopy Picture]}]
}

snoopy
cal
```

Which produces this output:

```txt

                           [Snoopy Picture]

                              -- 2011 --

      January                  February                  March
                1  2         1  2  3  4  5  6         1  2  3  4  5  6
 3  4  5  6  7  8  9      7  8  9 10 11 12 13      7  8  9 10 11 12 13
10 11 12 13 14 15 16     14 15 16 17 18 19 20     14 15 16 17 18 19 20
17 18 19 20 21 22 23     21 22 23 24 25 26 27     21 22 23 24 25 26 27
24 25 26 27 28 29 30     28                       28 29 30 31
31

       April                     May                      June
             1  2  3                        1            1  2  3  4  5
 4  5  6  7  8  9 10      2  3  4  5  6  7  8      6  7  8  9 10 11 12
11 12 13 14 15 16 17      9 10 11 12 13 14 15     13 14 15 16 17 18 19
18 19 20 21 22 23 24     16 17 18 19 20 21 22     20 21 22 23 24 25 26
25 26 27 28 29 30        23 24 25 26 27 28 29     27 28 29 30
                         30 31

        July                    August                 September
             1  2  3      1  2  3  4  5  6  7               1  2  3  4
 4  5  6  7  8  9 10      8  9 10 11 12 13 14      5  6  7  8  9 10 11
11 12 13 14 15 16 17     15 16 17 18 19 20 21     12 13 14 15 16 17 18
18 19 20 21 22 23 24     22 23 24 25 26 27 28     19 20 21 22 23 24 25
25 26 27 28 29 30 31     29 30 31                 26 27 28 29 30

      October                  November                 December
                1  2         1  2  3  4  5  6               1  2  3  4
 3  4  5  6  7  8  9      7  8  9 10 11 12 13      5  6  7  8  9 10 11
10 11 12 13 14 15 16     14 15 16 17 18 19 20     12 13 14 15 16 17 18
17 18 19 20 21 22 23     21 22 23 24 25 26 27     19 20 21 22 23 24 25
24 25 26 27 28 29 30     28 29 30                 26 27 28 29 30 31
31

```

If a different year is chosen, it's printed…

```tcl
snoopy
cal 1969
```


```txt

                           [Snoopy Picture]

                              -- 1969 --

      January                  February                  March
       1  2  3  4  5                     1  2                     1  2
 6  7  8  9 10 11 12      3  4  5  6  7  8  9      3  4  5  6  7  8  9
13 14 15 16 17 18 19     10 11 12 13 14 15 16     10 11 12 13 14 15 16
20 21 22 23 24 25 26     17 18 19 20 21 22 23     17 18 19 20 21 22 23
27 28 29 30 31           24 25 26 27 28           24 25 26 27 28 29 30
                                                  31

       April                     May                      June
    1  2  3  4  5  6               1  2  3  4                        1
 7  8  9 10 11 12 13      5  6  7  8  9 10 11      2  3  4  5  6  7  8
14 15 16 17 18 19 20     12 13 14 15 16 17 18      9 10 11 12 13 14 15
21 22 23 24 25 26 27     19 20 21 22 23 24 25     16 17 18 19 20 21 22
28 29 30                 26 27 28 29 30 31        23 24 25 26 27 28 29
                                                  30

        July                    August                 September
    1  2  3  4  5  6                  1  2  3      1  2  3  4  5  6  7
 7  8  9 10 11 12 13      4  5  6  7  8  9 10      8  9 10 11 12 13 14
14 15 16 17 18 19 20     11 12 13 14 15 16 17     15 16 17 18 19 20 21
21 22 23 24 25 26 27     18 19 20 21 22 23 24     22 23 24 25 26 27 28
28 29 30 31              25 26 27 28 29 30 31     29 30

      October                  November                 December
       1  2  3  4  5                     1  2      1  2  3  4  5  6  7
 6  7  8  9 10 11 12      3  4  5  6  7  8  9      8  9 10 11 12 13 14
13 14 15 16 17 18 19     10 11 12 13 14 15 16     15 16 17 18 19 20 21
20 21 22 23 24 25 26     17 18 19 20 21 22 23     22 23 24 25 26 27 28
27 28 29 30 31           24 25 26 27 28 29 30     29 30 31

```

The code also handles Julian/Gregorian switch dates correctly, but ''must'' be told what locale to format the switch as and for what timezone in that case. For example, Spain was one of the first countries to make the change:

```tcl
snoopy
cal 1582 :Europe/Madrid es_ES
```


```txt

                           [Snoopy Picture]

                              -- 1582 --

       enero                   febrero                   marzo
 1  2  3  4  5  6  7               1  2  3  4               1  2  3  4
 8  9 10 11 12 13 14      5  6  7  8  9 10 11      5  6  7  8  9 10 11
15 16 17 18 19 20 21     12 13 14 15 16 17 18     12 13 14 15 16 17 18
22 23 24 25 26 27 28     19 20 21 22 23 24 25     19 20 21 22 23 24 25
29 30 31                 26 27 28                 26 27 28 29 30 31

       abril                     mayo                    junio
                   1         1  2  3  4  5  6                  1  2  3
 2  3  4  5  6  7  8      7  8  9 10 11 12 13      4  5  6  7  8  9 10
 9 10 11 12 13 14 15     14 15 16 17 18 19 20     11 12 13 14 15 16 17
16 17 18 19 20 21 22     21 22 23 24 25 26 27     18 19 20 21 22 23 24
23 24 25 26 27 28 29     28 29 30 31              25 26 27 28 29 30
30

       julio                    agosto                 septiembre
                   1            1  2  3  4  5                     1  2
 2  3  4  5  6  7  8      6  7  8  9 10 11 12      3  4  5  6  7  8  9
 9 10 11 12 13 14 15     13 14 15 16 17 18 19     10 11 12 13 14 15 16
16 17 18 19 20 21 22     20 21 22 23 24 25 26     17 18 19 20 21 22 23
23 24 25 26 27 28 29     27 28 29 30 31           24 25 26 27 28 29 30
30 31

      octubre                 noviembre                diciembre
 1  2  3  4 15 16 17      1  2  3  4  5  6  7            1  2  3  4  5
18 19 20 21 22 23 24      8  9 10 11 12 13 14      6  7  8  9 10 11 12
25 26 27 28 29 30 31     15 16 17 18 19 20 21     13 14 15 16 17 18 19
                         22 23 24 25 26 27 28     20 21 22 23 24 25 26
                         29 30                    27 28 29 30 31

```

As can be seen, a Real Programmer has many intricacies to deal with!


## uBasic/4tH

The required "Snoopy" code is here. Use it when you feel like it.
<lang>Print "                      XXXX"
Print "                     X    XX"
Print "                    X  ***  X                XXXXX"
Print "                   X  *****  X            XXX     XX"
Print "                XXXX ******* XXX      XXXX          XX"
Print "              XX   X ******  XXXXXXXXX                XX XXX"
Print "            XX      X ****  X                           X** X"
Print "           X        XX    XX     X                      X***X"
Print "          X         //XXXX       X                      XXXX"
Print "         X         //   X                             XX"
Print "        X         //    X          XXXXXXXXXXXXXXXXXX/"
Print "        X     XXX//    X          X"
Print "        X    X   X     X         X"
Print "        X    X    X    X        X"
Print "         X   X    X    X        X                    XX"
Print "         X    X   X    X        X                 XXX  XX"
Print "          X    XXX      X        X               X  X X  X"
Print "          X             X         X              XX X  XXXX"
Print "           X             X         XXXXXXXX\\     XX   XX  X"
Print "            XX            XX              X     X    X  XX"
Print "              XX            XXXX   XXXXXX/     X     XXXX"
Print "                XXX             XX***         X     X"
Print "                   XXXXXXXXXXXXX *   *       X     X"
Print "                                *---* X     X     X"
Print "                               *-* *   XXX X     X"
Print "                               *- *       XXX   X"
Print "                              *- *X          XXX"
Print "                              *- *X  X          XXX"
Print "                             *- *X    X            XX"
Print "                             *- *XX    X             X"
Print "                            *  *X* X    X             X"
Print "                            *  *X * X    X             X"
Print "                           *  * X**  X   XXXX          X"
Print "                           *  * X**  XX     X          X"
Print "                          *  ** X** X     XX          X"
Print "                          *  **  X*  XXX   X         X"
Print "                         *  **    XX   XXXX       XXX"
Print "                        *  * *      XXXX      X     X"
Print "                       *   * *          X     X     X"
Print "
### =
*******   * *           X     X      XXXXXXXX\\"
Print "                *         * *      /XXXXX      XXXXXXXX\\      )"
Print "           =====**********  *     X                     )  \\  )"
Print "             ====*         *     X               \\  \\   )XXXXX"
Print "
### ===
**********       XXXXXXXXXXXXXXXXXXXXXX"
```

This is the code that generates the calendar:
<lang>Input "Year to print: "; Y             ' input required year

Push 63 : Gosub _Space                 ' center year
Print Y

For M = 1 To 7 Step 6                  ' two rows of six months
  Print

  For C = 0 To 5                       ' print months
    Push 1, M + C, Y : Gosub _FnMjd : @(C+6) = Pop()

    Push 5 : Gosub _Space              ' print month name
    Gosub (M + C)

    If C = 5 Then                      ' is it the last month?
      Print                            ' if so, terminate line
    Else                               ' if not, print eight spaces
      Push 8 : Gosub _Space
    EndIf
  Next

  For C = 0 To 5                       ' print days header
    Print "Su Mo Tu We Th Fr Sa";

    If C = 5 Then                      ' is it the last month?
      Print                            ' if so, terminate line
    Else                               ' if not, print two spaces
      Push 2 : Gosub _Space
    EndIf
                                       ' get days/month
    Push M + C, Y : Gosub _FnDim : @(C+12) = Pop()
    @(C) = 1                           ' initialize array while we're at it
  Next

  C =  0                               ' no columns printed yet
  Q =  0                               ' keep track of position

  Do                                   ' get day of the week
    Push @(C+6) : Gosub _FnDow : Z = Pop ()

    If @(C) - 1 < @(C + 12) Then
                                       ' if a position requested BEFORE
       If (Z*3 + C*22) < Q Then        ' the current position
          Print                        ' then terminate the line
          Q = 0                        ' and reset the counter
       Endif
                                       ' goto required position on screen
       Push (Z*3 + C*22) - Q : Gosub _Space
       Q = Z*3 + C*22                  ' set counter to that position

       If @(C) < 10 Then               ' if only one digit date
          Print " ";                   ' then compensate
       Endif

       Print @(C);                     ' print the date
       Q = Q + 2                       ' and update the position

       @(C) = @(C) + 1
       @(C+6) = @(C+6) + 1
    Endif
                                       ' increment columns,
    If (Z = 6) + (@(C)>@(C+12)) Then   ' wrap around if required
       C = (C + 1) % 6
    EndIf
                                       ' loop until all dates are printed
    Until (@(0)>@(0+12))*(@(1)>@(1+12))*(@(2)>@(2+12))*(@(3)>@(3+12))*(@(4)>@(4+12))*(@(5)>@(5+12))
  Loop

  Print                                ' terminate row with LF
Next

End

                                       ' convert D/M/Y to Julian date
_FnMjd                                 ' (day month year -- julian date)
  R = Pop()                            ' pop year
  S = Pop() - 3                        ' pop month
  T = Pop()                            ' pop day

  If S < 0 Then                        ' if month negative
    S = S + 12                         ' add 12 months
    R = R - 1                          ' and subtract a year
  Endif

  Push T + (153*S+2)/5 + R*365 + R/4 - R/100 + R/400 - 678882
Return

                                       ' calculate day of the week
_FnDow                                 ' (julian -- day-of-week)
  Push (Pop()+2400002)%7               ' convert julian date to day of week
Return

                                       ' calculate days in month
_FnDim                                 ' (month year -- #days)
  R = Pop()                            ' pop year
  S = Pop()                            ' pop month

  If S = 2 Then                        ' if February, get no. days
    Push 28 + ((R%4)=0) - ((R%100)=0) + ((R%400)=0)
  Else                                 ' otherwise, either 30 or 31
    If (S = 4) + (S = 6) + (S = 9) + (S = 11) Then
      Push 30
    Else
      Push 31
    Endif
  Endif
Return

                                       ' print a number of spaces
_Space                                 ' (#spaces --)
  For X = 1 To Pop()                   ' print n spaces
    Print " ";
  Next
Return

                                       ' month name subroutines
001 Print " January "; : Return        ' all have a fixed length
002 Print " February"; : Return        ' which simplifies things
003 Print "  March  "; : Return
004 Print "  April  "; : Return
005 Print "   May   "; : Return
006 Print "  June   "; : Return
007 Print "  July   "; : Return
008 Print " August  "; : Return
009 Print "September"; : Return
010 Print " October "; : Return
011 Print " November"; : Return
012 Print " December"; : Return
```

Output (missing Snoopy):

```txt
Year to print: 1969
                                                               1969

      January               February               March                 April                  May                  June
Su Mo Tu We Th Fr Sa  Su Mo Tu We Th Fr Sa  Su Mo Tu We Th Fr Sa  Su Mo Tu We Th Fr Sa  Su Mo Tu We Th Fr Sa  Su Mo Tu We Th Fr Sa
          1  2  3  4                     1                     1         1  2  3  4  5               1  2  3   1  2  3  4  5  6  7
 5  6  7  8  9 10 11   2  3  4  5  6  7  8   2  3  4  5  6  7  8   6  7  8  9 10 11 12   4  5  6  7  8  9 10   8  9 10 11 12 13 14
12 13 14 15 16 17 18   9 10 11 12 13 14 15   9 10 11 12 13 14 15  13 14 15 16 17 18 19  11 12 13 14 15 16 17  15 16 17 18 19 20 21
19 20 21 22 23 24 25  16 17 18 19 20 21 22  16 17 18 19 20 21 22  20 21 22 23 24 25 26  18 19 20 21 22 23 24  22 23 24 25 26 27 28
26 27 28 29 30 31     23 24 25 26 27 28     23 24 25 26 27 28 29  27 28 29 30           25 26 27 28 29 30 31  29 30
                                            30 31

       July                 August               September              October               November              December
Su Mo Tu We Th Fr Sa  Su Mo Tu We Th Fr Sa  Su Mo Tu We Th Fr Sa  Su Mo Tu We Th Fr Sa  Su Mo Tu We Th Fr Sa  Su Mo Tu We Th Fr Sa
       1  2  3  4  5                  1  2      1  2  3  4  5  6            1  2  3  4                     1      1  2  3  4  5  6
 6  7  8  9 10 11 12   3  4  5  6  7  8  9   7  8  9 10 11 12 13   5  6  7  8  9 10 11   2  3  4  5  6  7  8   7  8  9 10 11 12 13
13 14 15 16 17 18 19  10 11 12 13 14 15 16  14 15 16 17 18 19 20  12 13 14 15 16 17 18   9 10 11 12 13 14 15  14 15 16 17 18 19 20
20 21 22 23 24 25 26  17 18 19 20 21 22 23  21 22 23 24 25 26 27  19 20 21 22 23 24 25  16 17 18 19 20 21 22  21 22 23 24 25 26 27
27 28 29 30 31        24 25 26 27 28 29 30  28 29 30              26 27 28 29 30 31     23 24 25 26 27 28 29  28 29 30 31
                      31                                                                30

0 OK, 0:2753
```



## UNIX Shell

All Unix variations come with the cal command which makes the task very easy.

```bash

#!/bin/sh
echo "Snoopy goes here"
cal 1969

```


Output produced:

```txt

Snoopy goes here
                             1969

      January               February               March
Su Mo Tu We Th Fr Sa  Su Mo Tu We Th Fr Sa  Su Mo Tu We Th Fr Sa
          1  2  3  4                     1                     1
 5  6  7  8  9 10 11   2  3  4  5  6  7  8   2  3  4  5  6  7  8
12 13 14 15 16 17 18   9 10 11 12 13 14 15   9 10 11 12 13 14 15
19 20 21 22 23 24 25  16 17 18 19 20 21 22  16 17 18 19 20 21 22
26 27 28 29 30 31     23 24 25 26 27 28     23 24 25 26 27 28 29
                                            30 31
       April                  May                   June
Su Mo Tu We Th Fr Sa  Su Mo Tu We Th Fr Sa  Su Mo Tu We Th Fr Sa
       1  2  3  4  5               1  2  3   1  2  3  4  5  6  7
 6  7  8  9 10 11 12   4  5  6  7  8  9 10   8  9 10 11 12 13 14
13 14 15 16 17 18 19  11 12 13 14 15 16 17  15 16 17 18 19 20 21
20 21 22 23 24 25 26  18 19 20 21 22 23 24  22 23 24 25 26 27 28
27 28 29 30           25 26 27 28 29 30 31  29 30

        July                 August              September
Su Mo Tu We Th Fr Sa  Su Mo Tu We Th Fr Sa  Su Mo Tu We Th Fr Sa
       1  2  3  4  5                  1  2      1  2  3  4  5  6
 6  7  8  9 10 11 12   3  4  5  6  7  8  9   7  8  9 10 11 12 13
13 14 15 16 17 18 19  10 11 12 13 14 15 16  14 15 16 17 18 19 20
20 21 22 23 24 25 26  17 18 19 20 21 22 23  21 22 23 24 25 26 27
27 28 29 30 31        24 25 26 27 28 29 30  28 29 30
                      31
      October               November              December
Su Mo Tu We Th Fr Sa  Su Mo Tu We Th Fr Sa  Su Mo Tu We Th Fr Sa
          1  2  3  4                     1      1  2  3  4  5  6
 5  6  7  8  9 10 11   2  3  4  5  6  7  8   7  8  9 10 11 12 13
12 13 14 15 16 17 18   9 10 11 12 13 14 15  14 15 16 17 18 19 20
19 20 21 22 23 24 25  16 17 18 19 20 21 22  21 22 23 24 25 26 27
26 27 28 29 30 31     23 24 25 26 27 28 29  28 29 30 31
                      30

```



## Vedit macro language

This implementation uses the '''calendar.vdm''' macro that comes with Vedit. (So it serves as an example of how to call other macros from a macro.)
See [[Calendar - for "real" programmers|CALENDAR]] task for a stand-alone version.

Calendar.vdm draws one month calendar. That is then copied as columnar block and pasted to the 1969 calendar.


```vedit
Buf_Switch(Buf_Free)
Config_Tab(5,30,55)
#9 = 1                  // first day of week: 0=Su, 1=Mo
#3 = 3                  // number of months per line
#2 = 1969               // year
#1 = 1                  // starting month
Repeat(12/#3) {
    Repeat (#3) {
        Buf_Switch(Buf_Free)
        Call_File(122, "calendar.vdm", "DRAW_CALENDAR")
        Reg_Copy_Block(10, 1, EOB_Pos, COLSET, 1, 21)
        Buf_Quit(OK)
        EOL Ins_Char(9)
        #5 = Cur_Pos
        Reg_Ins(10)
        Goto_Pos(#5)
        #1++
    }
    EOF
    Ins_Newline(2)
}
```


The code above creates calendar for 80x43 display. For 132 column display, change #3 (number of months per line) to 4 or 6, and adjust the tab positions if required.
The following would work with all widhts, but the left/right margin would not be balanced.

```vedit
Config_Tab(22)
```


Output:

```txt

        January   1969           February  1969           March     1969

     Mo Tu We Th Fr Sa Su     Mo Tu We Th Fr Sa Su     Mo Tu We Th Fr Sa Su
     --------------------     --------------------     --------------------
            1  2  3  4  5                     1  2                     1  2
      6  7  8  9 10 11 12      3  4  5  6  7  8  9      3  4  5  6  7  8  9
     13 14 15 16 17 18 19     10 11 12 13 14 15 16     10 11 12 13 14 15 16
     20 21 22 23 24 25 26     17 18 19 20 21 22 23     17 18 19 20 21 22 23
     27 28 29 30 31           24 25 26 27 28           24 25 26 27 28 29 30
                                                       31

        April     1969           May       1969           June      1969

     Mo Tu We Th Fr Sa Su     Mo Tu We Th Fr Sa Su     Mo Tu We Th Fr Sa Su
     --------------------     --------------------     --------------------
         1  2  3  4  5  6               1  2  3  4                        1
      7  8  9 10 11 12 13      5  6  7  8  9 10 11      2  3  4  5  6  7  8
     14 15 16 17 18 19 20     12 13 14 15 16 17 18      9 10 11 12 13 14 15
     21 22 23 24 25 26 27     19 20 21 22 23 24 25     16 17 18 19 20 21 22
     28 29 30                 26 27 28 29 30 31        23 24 25 26 27 28 29
                                                       30

        July      1969           August    1969           September 1969

     Mo Tu We Th Fr Sa Su     Mo Tu We Th Fr Sa Su     Mo Tu We Th Fr Sa Su
     --------------------     --------------------     --------------------
         1  2  3  4  5  6                  1  2  3      1  2  3  4  5  6  7
      7  8  9 10 11 12 13      4  5  6  7  8  9 10      8  9 10 11 12 13 14
     14 15 16 17 18 19 20     11 12 13 14 15 16 17     15 16 17 18 19 20 21
     21 22 23 24 25 26 27     18 19 20 21 22 23 24     22 23 24 25 26 27 28
     28 29 30 31              25 26 27 28 29 30 31     29 30


        October   1969           November  1969           December  1969

     Mo Tu We Th Fr Sa Su     Mo Tu We Th Fr Sa Su     Mo Tu We Th Fr Sa Su
     --------------------     --------------------     --------------------
            1  2  3  4  5                     1  2      1  2  3  4  5  6  7
      6  7  8  9 10 11 12      3  4  5  6  7  8  9      8  9 10 11 12 13 14
     13 14 15 16 17 18 19     10 11 12 13 14 15 16     15 16 17 18 19 20 21
     20 21 22 23 24 25 26     17 18 19 20 21 22 23     22 23 24 25 26 27 28
     27 28 29 30 31           24 25 26 27 28 29 30     29 30 31

```



## Visual Basic .NET

'''Compiler:''' Roslyn Visual Basic (language version >= 15.8)
{{works with|.NET Core|2.1}}

Inspired by C#. Uses iterators to generate months row-by-row and interleave them to print multiple months beside each other. Attempts to scale to specified row/column count. Minimal validation of numeric arguments is performed, so crashes may occur for certain too-small row and column counts.

Does not handle 1752.

Options and imports statements (all parts must be in one file):

```vbnet
Option Compare Binary
Option Explicit On
Option Infer On
Option Strict On

Imports System.Globalization
Imports System.Text
Imports System.Runtime.InteropServices
Imports System.Runtime.CompilerServices
```


Helper to parse command-line arguments.

```vbnet
Module ArgHelper
    ReadOnly _ArgDict As New Dictionary(Of String, String)()

    Delegate Function TryParse(Of T, TResult)(value As T, <Out> ByRef result As TResult) As Boolean

    Sub InitializeArguments(args As String())
        For Each item In args
            item = item.ToUpperInvariant()

            If item.Length > 0 AndAlso item(0) <> """"c Then
                Dim colonPos = item.IndexOf(":"c, StringComparison.Ordinal)

                If colonPos <> -1 Then
                    ' Split arguments with colons into key(part before colon) / value(part after colon) pairs.
                    _ArgDict.Add(item.Substring(0, colonPos), item.Substring(colonPos + 1, item.Length - colonPos - 1))
                End If
            End If
        Next
    End Sub

    Sub FromArgument(Of T)(
            key As String,
            <Out> ByRef var As T,
            getDefault As Func(Of T),
            tryParse As TryParse(Of String, T),
            Optional validate As Predicate(Of T) = Nothing)

        Dim value As String = Nothing
        If _ArgDict.TryGetValue(key.ToUpperInvariant(), value) Then
            If Not (tryParse(value, var) AndAlso (validate Is Nothing OrElse validate(var))) Then
                Console.WriteLine($"Invalid value for {key}: {value}")
                Environment.Exit(-1)
            End If
        Else
            var = getDefault()
        End If
    End Sub
End Module
```


Program:

```vbnet
Module Program
    Sub Main(args As String())
        Dim dt As Date
        Dim columns, rows, monthsPerRow As Integer
        Dim vertStretch, horizStretch, resizeWindow As Boolean

        InitializeArguments(args)
        FromArgument("date", dt, Function() New Date(1969, 1, 1), AddressOf Date.TryParse)
        FromArgument("cols", columns, Function() 80, AddressOf Integer.TryParse, Function(v) v >= 20)
        FromArgument("rows", rows, Function() 43, AddressOf Integer.TryParse, Function(v) v >= 0)
        FromArgument("ms/row", monthsPerRow, Function() 0, AddressOf Integer.TryParse, Function(v) v <= 12 AndAlso v <= columns \ 20)
        FromArgument("vstretch", vertStretch, Function() True, AddressOf Boolean.TryParse)
        FromArgument("hstretch", horizStretch, Function() True, AddressOf Boolean.TryParse)
        FromArgument("wsize", resizeWindow, Function() True, AddressOf Boolean.TryParse)

        ' The scroll bar in command prompt seems to take up part of the last column.
        If resizeWindow Then
            Console.WindowWidth = columns + 1
            Console.WindowHeight = rows
        End If

        If monthsPerRow < 1 Then monthsPerRow = Math.Max(columns \ 22, 1)

        For Each row In GetCalendarRows(dt:=dt, width:=columns, height:=rows, monthsPerRow:=monthsPerRow, vertStretch:=vertStretch, horizStretch:=horizStretch)
            Console.Write(row)
        Next
    End Sub

    Iterator Function GetCalendarRows(
            dt As Date,
            width As Integer,
            height As Integer,
            monthsPerRow As Integer,
            vertStretch As Boolean,
            horizStretch As Boolean) As IEnumerable(Of String)

        Dim year = dt.Year
        Dim calendarRowCount As Integer = CInt(Math.Ceiling(12 / monthsPerRow))
        ' Make room for the three empty lines on top.
        Dim monthGridHeight As Integer = height - 3

        Yield "[Snoopy]".PadCenter(width) & Environment.NewLine
        Yield year.ToString(CultureInfo.InvariantCulture).PadCenter(width) & Environment.NewLine
        Yield Environment.NewLine

        Dim month = 0
        Do While month < 12
            Dim rowHighestMonth = Math.Min(month + monthsPerRow, 12)

            Dim cellWidth = width \ monthsPerRow

            ' Special case when displaying only one calendar cell in a row to make 20-wide work. Adds padding between cells otherwise.
            Dim cellContentWidth = If(monthsPerRow = 1, cellWidth, (cellWidth * 19) \ 20)

            Dim cellHeight = monthGridHeight \ calendarRowCount
            Dim cellContentHeight = (cellHeight * 19) \ 20

            ' Creates a month cell for the specified month (1-12).
            Dim getMonthFrom =
                Function(m As Integer) BuildMonth(
                    dt:=New Date(dt.Year, m, 1),
                    width:=cellContentWidth,
                    height:=cellContentHeight,
                    vertStretch:=vertStretch,
                    horizStretch:=horizStretch).Select(Function(x) x.PadCenter(cellWidth))

            ' The months in this row of the calendar.
            Dim monthsThisRow As IEnumerable(Of IEnumerable(Of String)) =
                Enumerable.Select(Enumerable.Range(month + 1, rowHighestMonth - month), getMonthFrom)

            Dim calendarRow As IEnumerable(Of String) =
                Interleaved(
                    monthsThisRow,
                    useInnerSeparator:=False,
                    useOuterSeparator:=True,
                    outerSeparator:=Environment.NewLine)

            Dim en = calendarRow.GetEnumerator()
            Dim hasNext = en.MoveNext()
            Do While hasNext

                Dim current As String = en.Current

                ' To maintain the (not strictly needed) contract of yielding complete rows, keep the newline after
                ' the calendar row with the last terminal row of the row.
                hasNext = en.MoveNext()
                Yield If(hasNext, current, current & Environment.NewLine)
            Loop

            month += monthsPerRow
        Loop
    End Function

    ''' <summary>
    ''' Interleaves the elements of the specified sub-sources by making successive passes through the source
    ''' enumerable, yielding a single element from each sub-source in sequence in each pass, optionally inserting a
    ''' separator between elements of adjacent sub-sources and optionally a different separator at the end of each
    ''' pass through all the sources. (i.e., between elements of the last and first source)
    ''' </summary>
    ''' <typeparam name="T">The type of the elements of the sub-sources.</typeparam>
    ''' <param name="sources">A sequence of the sequences whose elements are to be interleaved.</param>
    ''' <param name="useInnerSeparator">Whether to insert <paramref name="useInnerSeparator"/> between the elements ofadjacent sub-sources.</param>
    ''' <param name="innerSeparator">The separator between elements of adjacent sub-sources.</param>
    ''' <param name="useOuterSeparator">Whether to insert <paramref name="outerSeparator"/> between the elements of the last and first sub-sources.</param>
    ''' <param name="outerSeparator">The separator between elements of the last and first sub-source.</param>
    ''' <param name="whileAny">If <see langword="true"/>, the enumeration continues until every given subsource is empty;
    ''' if <see langword="false"/>, the enumeration stops as soon as any enumerable no longer has an element to supply for the next pass.</param>
    Iterator Function Interleaved(Of T)(
            sources As IEnumerable(Of IEnumerable(Of T)),
            Optional useInnerSeparator As Boolean = False,
            Optional innerSeparator As T = Nothing,
            Optional useOuterSeparator As Boolean = False,
            Optional outerSeparator As T = Nothing,
            Optional whileAny As Boolean = True) As IEnumerable(Of T)
        Dim sourceEnumerators As IEnumerator(Of T)() = Nothing

        Try
            sourceEnumerators = sources.Select(Function(x) x.GetEnumerator()).ToArray()
            Dim numSources = sourceEnumerators.Length
            Dim enumeratorStates(numSources - 1) As Boolean

            Dim anyPrevIters As Boolean = False
            Do
                ' Indices of first and last sub-sources that have elements.
                Dim firstActive = -1, lastActive = -1

                ' Determine whether each sub-source that still have elements.
                For i = 0 To numSources - 1
                    enumeratorStates(i) = sourceEnumerators(i).MoveNext()
                    If enumeratorStates(i) Then
                        If firstActive = -1 Then firstActive = i
                        lastActive = i
                    End If
                Next

                ' Determine whether to yield anything in this iteration based on whether whileAny is true.
                ' Not yielding anything this iteration implies that the enumeration has ended.
                Dim thisIterHasResults As Boolean = If(whileAny, firstActive <> -1, firstActive = 0 AndAlso lastActive = numSources - 1)
                If Not thisIterHasResults Then Exit Do

                ' Don't insert a separator on the first pass.
                If anyPrevIters Then
                    If useOuterSeparator Then Yield outerSeparator
                Else
                    anyPrevIters = True
                End If

                ' Go through and yield from the sub-sources that still have elements.
                For i = 0 To numSources - 1
                    If enumeratorStates(i) Then
                        ' Don't insert a separator before the first element.
                        If i > firstActive AndAlso useInnerSeparator Then Yield innerSeparator
                        Yield sourceEnumerators(i).Current
                    End If
                Next
            Loop

        Finally
            If sourceEnumerators IsNot Nothing Then
                For Each en In sourceEnumerators
                    en.Dispose()
                Next
            End If
        End Try
    End Function

    ''' <summary>
    ''' Returns the rows representing one month cell without trailing newlines. Appropriate leading and trailing
    ''' whitespace is added so that every row has the length of width.
    ''' </summary>
    ''' <param name="dt">A date within the month to represent.</param>
    ''' <param name="width">The width of the cell.</param>
    ''' <param name="height">The height.</param>
    ''' <param name="vertStretch">If <see langword="true" />, blank rows are inserted to fit the available height.
    ''' Otherwise, the cell has a constant height of </param>
    ''' <param name="horizStretch">If <see langword="true" />, the spacing between individual days is increased to
    ''' fit the available width. Otherwise, the cell has a constant width of 20 characters and is padded to be in
    ''' the center of the expected width.</param>
    Iterator Function BuildMonth(dt As Date, width As Integer, height As Integer, vertStretch As Boolean, horizStretch As Boolean) As IEnumerable(Of String)
        Const DAY_WDT = 2 ' Width of a day.
        Const ALLDAYS_WDT = DAY_WDT * 7 ' Width of al ldays combined.

        ' Normalize the date to January 1.
        dt = New Date(dt.Year, dt.Month, 1)

        ' Horizontal whitespace between days of the week. Constant of 6 represents 6 separators per line.
        Dim daySep As New String(" "c, Math.Min((width - ALLDAYS_WDT) \ 6, If(horizStretch, Integer.MaxValue, 1)))
        ' Number of blank lines between rows.
        Dim vertblankCount = If(Not vertStretch, 0, (height - 8) \ 7)

        ' Width of each day * 7 days in one row + day separator length * 6 separators per line.
        Dim blockWidth = ALLDAYS_WDT + daySep.Length * 6

        ' The whitespace at the beginning of each line.
        Dim leftPad As New String(" "c, (width - blockWidth) \ 2)
        ' The whitespace for blank lines.
        Dim fullPad As New String(" "c, width)

        ' Lines are "staged" in the stringbuilder.
        Dim sb As New StringBuilder(leftPad)
        Dim numLines = 0

        ' Get the current line so far form the stringbuilder and begin a new line.
        ' Returns the current line and trailing blank lines used for vertical padding (if any).
        ' Returns empty enumerable if the height requirement has been reached.
        Dim EndLine =
         Function() As IEnumerable(Of String)
             Dim finishedLine As String = sb.ToString().PadRight(width)
             sb.Clear()
             sb.Append(leftPad)

             ' Use an inner iterator to prevent lazy execution of side effects of outer function.
             Return If(numLines >= height,
                 Enumerable.Empty(Of String)(),
                 Iterator Function() As IEnumerable(Of String)
                     Yield finishedLine
                     numLines += 1

                     For i = 1 To vertblankCount
                         If numLines >= height Then Return
                         Yield fullPad
                         numLines += 1
                     Next
                 End Function())
         End Function

        ' Yield the month name.
        sb.Append(PadCenter(dt.ToString("MMMM", CultureInfo.InvariantCulture), blockWidth))
        For Each l In EndLine()
            Yield l
        Next

        ' Yield the header of weekday names.
        Dim weekNmAbbrevs = [Enum].GetNames(GetType(DayOfWeek)).Select(Function(x) x.Substring(0, 2))
        sb.Append(String.Join(daySep, weekNmAbbrevs))
        For Each l In EndLine()
            Yield l
        Next

        ' Day of week of first day of month.
        Dim startWkDy = CInt(dt.DayOfWeek)

        ' Initialize with empty space for the first line.
        Dim firstPad As New String(" "c, (DAY_WDT + daySep.Length) * startWkDy)
        sb.Append(firstPad)

        Dim d = dt
        Do While d.Month = dt.Month
            sb.AppendFormat(CultureInfo.InvariantCulture, $"{{0,{DAY_WDT}}}", d.Day)

            ' Each row ends on saturday.
            If d.DayOfWeek = DayOfWeek.Saturday Then
                For Each l In EndLine()
                    Yield l
                Next
            Else
                sb.Append(daySep)
            End If

            d = d.AddDays(1)
        Loop

        ' Keep adding empty lines until the height quota is met.
        Dim nextLines As IEnumerable(Of String)
        Do
            nextLines = EndLine()
            For Each l In nextLines
                Yield l
            Next
        Loop While nextLines.Any()
    End Function

    ''' <summary>
    ''' Returns a new string that center-aligns the characters in this string by padding to the left and right with
    ''' the specified character to a specified total length.
    ''' </summary>
    ''' <param name="s">The string to center-align.</param>
    ''' <param name="totalWidth">The number of characters in the resulting string.</param>
    ''' <param name="paddingChar">The padding character.</param>
    <Extension()>
    Private Function PadCenter(s As String, totalWidth As Integer, Optional paddingChar As Char = " "c) As String
        Return s.PadLeft(((totalWidth - s.Length) \ 2) + s.Length, paddingChar).PadRight(totalWidth, paddingChar)
    End Function
End Module

```


====Command-line arguments:====
Syntax: <name>:<value>, space-separated and optionally enclosed in quotes.
* date: Date within year to display. Accepts whatever the .NET DateTime parser is able to parse. Default: 1969-01-01.
* cols: Number of columns to display. Default: 80. Min: 20.
* rows: Number of rows to display. Default: 43. Min: 0.
* ms/row: Number of months per row (or as many as possible for values < 1). Default: 0. Max: 12 or cols*20.
* vstretch: Whether individual months are expanded with whitespace to fill vertical space. Default: true.
* hstretch: Whether individual months are expanded with whitespace to fill horizontal space. Default: true.
* wsize: Whether to resize the command window to the value given by ''cols'' and ''rows''. Set to false if the program is crashing with an error about the console buffer. Default: true.

{{out|note=80x43 [default with no args]}}

```txt
                                    [Snoopy]
                                      1969

         January                   February                   March
   Su Mo Tu We Th Fr Sa      Su Mo Tu We Th Fr Sa      Su Mo Tu We Th Fr Sa
             1  2  3  4                         1                         1
    5  6  7  8  9 10 11       2  3  4  5  6  7  8       2  3  4  5  6  7  8
   12 13 14 15 16 17 18       9 10 11 12 13 14 15       9 10 11 12 13 14 15
   19 20 21 22 23 24 25      16 17 18 19 20 21 22      16 17 18 19 20 21 22
   26 27 28 29 30 31         23 24 25 26 27 28         23 24 25 26 27 28 29
                                                       30 31

          April                      May                       June
   Su Mo Tu We Th Fr Sa      Su Mo Tu We Th Fr Sa      Su Mo Tu We Th Fr Sa
          1  2  3  4  5                   1  2  3       1  2  3  4  5  6  7
    6  7  8  9 10 11 12       4  5  6  7  8  9 10       8  9 10 11 12 13 14
   13 14 15 16 17 18 19      11 12 13 14 15 16 17      15 16 17 18 19 20 21
   20 21 22 23 24 25 26      18 19 20 21 22 23 24      22 23 24 25 26 27 28
   27 28 29 30               25 26 27 28 29 30 31      29 30


           July                     August                  September
   Su Mo Tu We Th Fr Sa      Su Mo Tu We Th Fr Sa      Su Mo Tu We Th Fr Sa
          1  2  3  4  5                      1  2          1  2  3  4  5  6
    6  7  8  9 10 11 12       3  4  5  6  7  8  9       7  8  9 10 11 12 13
   13 14 15 16 17 18 19      10 11 12 13 14 15 16      14 15 16 17 18 19 20
   20 21 22 23 24 25 26      17 18 19 20 21 22 23      21 22 23 24 25 26 27
   27 28 29 30 31            24 25 26 27 28 29 30      28 29 30
                             31

         October                   November                  December
   Su Mo Tu We Th Fr Sa      Su Mo Tu We Th Fr Sa      Su Mo Tu We Th Fr Sa
             1  2  3  4                         1          1  2  3  4  5  6
    5  6  7  8  9 10 11       2  3  4  5  6  7  8       7  8  9 10 11 12 13
   12 13 14 15 16 17 18       9 10 11 12 13 14 15      14 15 16 17 18 19 20
   19 20 21 22 23 24 25      16 17 18 19 20 21 22      21 22 23 24 25 26 27
   26 27 28 29 30 31         23 24 25 26 27 28 29      28 29 30 31
                             30


```


{{out|input=cols:132 rows:60 ms/row:4|note=132-column demonstrating stretching}}

```txt
                                                              [Snoopy]
                                                                1969

            January                          February                          March                            April

   Su  Mo  Tu  We  Th  Fr  Sa       Su  Mo  Tu  We  Th  Fr  Sa       Su  Mo  Tu  We  Th  Fr  Sa       Su  Mo  Tu  We  Th  Fr  Sa

                1   2   3   4                                1                                1                1   2   3   4   5

    5   6   7   8   9  10  11        2   3   4   5   6   7   8        2   3   4   5   6   7   8        6   7   8   9  10  11  12

   12  13  14  15  16  17  18        9  10  11  12  13  14  15        9  10  11  12  13  14  15       13  14  15  16  17  18  19

   19  20  21  22  23  24  25       16  17  18  19  20  21  22       16  17  18  19  20  21  22       20  21  22  23  24  25  26

   26  27  28  29  30  31           23  24  25  26  27  28           23  24  25  26  27  28  29       27  28  29  30

                                                                     30  31



              May                              June                             July                            August

   Su  Mo  Tu  We  Th  Fr  Sa       Su  Mo  Tu  We  Th  Fr  Sa       Su  Mo  Tu  We  Th  Fr  Sa       Su  Mo  Tu  We  Th  Fr  Sa

                    1   2   3        1   2   3   4   5   6   7                1   2   3   4   5                            1   2

    4   5   6   7   8   9  10        8   9  10  11  12  13  14        6   7   8   9  10  11  12        3   4   5   6   7   8   9

   11  12  13  14  15  16  17       15  16  17  18  19  20  21       13  14  15  16  17  18  19       10  11  12  13  14  15  16

   18  19  20  21  22  23  24       22  23  24  25  26  27  28       20  21  22  23  24  25  26       17  18  19  20  21  22  23

   25  26  27  28  29  30  31       29  30                           27  28  29  30  31               24  25  26  27  28  29  30

                                                                                                      31



           September                         October                          November                         December

   Su  Mo  Tu  We  Th  Fr  Sa       Su  Mo  Tu  We  Th  Fr  Sa       Su  Mo  Tu  We  Th  Fr  Sa       Su  Mo  Tu  We  Th  Fr  Sa

        1   2   3   4   5   6                    1   2   3   4                                1            1   2   3   4   5   6

    7   8   9  10  11  12  13        5   6   7   8   9  10  11        2   3   4   5   6   7   8        7   8   9  10  11  12  13

   14  15  16  17  18  19  20       12  13  14  15  16  17  18        9  10  11  12  13  14  15       14  15  16  17  18  19  20

   21  22  23  24  25  26  27       19  20  21  22  23  24  25       16  17  18  19  20  21  22       21  22  23  24  25  26  27

   28  29  30                       26  27  28  29  30  31           23  24  25  26  27  28  29       28  29  30  31

                                                                     30




```


{{out|input=cols:132 rows:30 ms/row:6|note=132-column unstretched}}

```txt
                                                              [Snoopy]
                                                                1969

       January               February               March                 April                  May                   June
 Su Mo Tu We Th Fr Sa  Su Mo Tu We Th Fr Sa  Su Mo Tu We Th Fr Sa  Su Mo Tu We Th Fr Sa  Su Mo Tu We Th Fr Sa  Su Mo Tu We Th Fr Sa
           1  2  3  4                     1                     1         1  2  3  4  5               1  2  3   1  2  3  4  5  6  7
  5  6  7  8  9 10 11   2  3  4  5  6  7  8   2  3  4  5  6  7  8   6  7  8  9 10 11 12   4  5  6  7  8  9 10   8  9 10 11 12 13 14
 12 13 14 15 16 17 18   9 10 11 12 13 14 15   9 10 11 12 13 14 15  13 14 15 16 17 18 19  11 12 13 14 15 16 17  15 16 17 18 19 20 21
 19 20 21 22 23 24 25  16 17 18 19 20 21 22  16 17 18 19 20 21 22  20 21 22 23 24 25 26  18 19 20 21 22 23 24  22 23 24 25 26 27 28
 26 27 28 29 30 31     23 24 25 26 27 28     23 24 25 26 27 28 29  27 28 29 30           25 26 27 28 29 30 31  29 30
                                             30 31


         July                 August              September              October               November              December
 Su Mo Tu We Th Fr Sa  Su Mo Tu We Th Fr Sa  Su Mo Tu We Th Fr Sa  Su Mo Tu We Th Fr Sa  Su Mo Tu We Th Fr Sa  Su Mo Tu We Th Fr Sa
        1  2  3  4  5                  1  2      1  2  3  4  5  6            1  2  3  4                     1      1  2  3  4  5  6
  6  7  8  9 10 11 12   3  4  5  6  7  8  9   7  8  9 10 11 12 13   5  6  7  8  9 10 11   2  3  4  5  6  7  8   7  8  9 10 11 12 13
 13 14 15 16 17 18 19  10 11 12 13 14 15 16  14 15 16 17 18 19 20  12 13 14 15 16 17 18   9 10 11 12 13 14 15  14 15 16 17 18 19 20
 20 21 22 23 24 25 26  17 18 19 20 21 22 23  21 22 23 24 25 26 27  19 20 21 22 23 24 25  16 17 18 19 20 21 22  21 22 23 24 25 26 27
 27 28 29 30 31        24 25 26 27 28 29 30  28 29 30              26 27 28 29 30 31     23 24 25 26 27 28 29  28 29 30 31
                       31                                                                30



```


{{out|input=cols:20 rows:120 wsize:false|note=20-column}}


```txt
      [Snoopy]
        1969

      January
Su Mo Tu We Th Fr Sa
          1  2  3  4
 5  6  7  8  9 10 11
12 13 14 15 16 17 18
19 20 21 22 23 24 25
26 27 28 29 30 31

      February
Su Mo Tu We Th Fr Sa
                   1
 2  3  4  5  6  7  8
 9 10 11 12 13 14 15
16 17 18 19 20 21 22
23 24 25 26 27 28

       March
Su Mo Tu We Th Fr Sa
                   1
 2  3  4  5  6  7  8
 9 10 11 12 13 14 15
16 17 18 19 20 21 22
23 24 25 26 27 28 29
30 31
       April
Su Mo Tu We Th Fr Sa
       1  2  3  4  5
 6  7  8  9 10 11 12
13 14 15 16 17 18 19
20 21 22 23 24 25 26
27 28 29 30

        May
Su Mo Tu We Th Fr Sa
             1  2  3
 4  5  6  7  8  9 10
11 12 13 14 15 16 17
18 19 20 21 22 23 24
25 26 27 28 29 30 31

        June
Su Mo Tu We Th Fr Sa
 1  2  3  4  5  6  7
 8  9 10 11 12 13 14
15 16 17 18 19 20 21
22 23 24 25 26 27 28
29 30

        July
Su Mo Tu We Th Fr Sa
       1  2  3  4  5
 6  7  8  9 10 11 12
13 14 15 16 17 18 19
20 21 22 23 24 25 26
27 28 29 30 31

       August
Su Mo Tu We Th Fr Sa
                1  2
 3  4  5  6  7  8  9
10 11 12 13 14 15 16
17 18 19 20 21 22 23
24 25 26 27 28 29 30
31
     September
Su Mo Tu We Th Fr Sa
    1  2  3  4  5  6
 7  8  9 10 11 12 13
14 15 16 17 18 19 20
21 22 23 24 25 26 27
28 29 30

      October
Su Mo Tu We Th Fr Sa
          1  2  3  4
 5  6  7  8  9 10 11
12 13 14 15 16 17 18
19 20 21 22 23 24 25
26 27 28 29 30 31

      November
Su Mo Tu We Th Fr Sa
                   1
 2  3  4  5  6  7  8
 9 10 11 12 13 14 15
16 17 18 19 20 21 22
23 24 25 26 27 28 29
30
      December
Su Mo Tu We Th Fr Sa
    1  2  3  4  5  6
 7  8  9 10 11 12 13
14 15 16 17 18 19 20
21 22 23 24 25 26 27
28 29 30 31


```



## XLISP

Targets the 132-column imaginary line printer. The day of the week on 1 January of a given year is found using Gauss's algorithm; <tt>LEAP-YEARP</tt> is taken from the Rosetta Code task on leap years.

```lisp
(defun calendar (year)
    (define months-list '(("JANUARY" 31) ("FEBRUARY" 28) ("MARCH" 31) ("APRIL" 30) ("MAY" 31) ("JUNE" 30) ("JULY" 31) ("AUGUST" 31) ("SEPTEMBER" 30) ("OCTOBER" 31) ("NOVEMBER" 30) ("DECEMBER" 31)))
    (define days #("      Sunday            " "Monday            " "Tuesday           " "Wednesday         " "Thursday          " "Friday            " "Saturday"))
    (defun gauss-algorithm (a)
        (rem (+ 1 (+ (* 5 (rem (- a 1) 4)) (* 4 (rem (- a 1) 100)) (* 6 (rem (- a 1) 400)))) 7))
    (defun range (start end)
        (if (<= start end)
            (cons start (range (+ start 1) end))))
    (defun string-repeat (s n)
        (if (= n 0)
            ""
            (string-append s (string-repeat s (- n 1)))))
    (defun print-month (number-of-days start-day)
        (defun print-days (day day-of-week end-day)
            (if (= day-of-week 7)
                (begin
                    (newline)
                    (define day-of-week 0)))
            (display (string-repeat " " 8))
            (if (= day-of-week 0)
                (display (string-repeat " " 6)))
            (if (< day 10)
                (display " "))
            (display day)
            (display (string-repeat " " 8))
            (if (= day end-day)
                (begin
                    (fresh-line)
                    (+ day-of-week 1))
                (print-days (+ day 1) (+ day-of-week 1) end-day)))
        (mapcar (lambda (n) (display (vector-ref days n))) (range 0 6))
        (newline)
        (display (string-repeat (string-repeat " " 18) start-day))
        (print-days 1 start-day number-of-days))
    (defun leap-yearp (y)
        (and (= (mod year 4) 0) (or (/= (mod year 100) 0) (= (mod year 400) 0))))
    (define months (make-table))
    (mapcar (lambda (month) (table-set! months (car month) (cadr month))) months-list)
    (if (leap-yearp year)
        (table-set! months "FEBRUARY" 29))
    (defun print-calendar (calendar-months weekday)
        (newline)
        (newline)
        (display (string-repeat " " 60))
        (display (caar calendar-months))
        (newline)
        (define next-month-starts (print-month (table-ref months (caar calendar-months)) weekday))
        (if (cdr calendar-months)
            (print-calendar (cdr calendar-months) next-month-starts)))
    (display (string-repeat " " 60))
    (display "******** SNOOPY CALENDAR ")
    (display year)
    (display " ********")
    (newline)
    (newline)
    (print-calendar months-list (gauss-algorithm year)))

(calendar 1969)
```

{{out}}

```txt
                                                            ******** SNOOPY CALENDAR 1969 ********



                                                            JANUARY
      Sunday            Monday            Tuesday           Wednesday         Thursday          Friday            Saturday
                                                               1                 2                 3                 4
               5                 6                 7                 8                 9                10                11
              12                13                14                15                16                17                18
              19                20                21                22                23                24                25
              26                27                28                29                30                31


                                                            FEBRUARY
      Sunday            Monday            Tuesday           Wednesday         Thursday          Friday            Saturday
                                                                                                                     1
               2                 3                 4                 5                 6                 7                 8
               9                10                11                12                13                14                15
              16                17                18                19                20                21                22
              23                24                25                26                27                28


                                                            MARCH
      Sunday            Monday            Tuesday           Wednesday         Thursday          Friday            Saturday
                                                                                                                     1
               2                 3                 4                 5                 6                 7                 8
               9                10                11                12                13                14                15
              16                17                18                19                20                21                22
              23                24                25                26                27                28                29
              30                31


                                                            APRIL
      Sunday            Monday            Tuesday           Wednesday         Thursday          Friday            Saturday
                                             1                 2                 3                 4                 5
               6                 7                 8                 9                10                11                12
              13                14                15                16                17                18                19
              20                21                22                23                24                25                26
              27                28                29                30


                                                            MAY
      Sunday            Monday            Tuesday           Wednesday         Thursday          Friday            Saturday
                                                                                 1                 2                 3
               4                 5                 6                 7                 8                 9                10
              11                12                13                14                15                16                17
              18                19                20                21                22                23                24
              25                26                27                28                29                30                31


                                                            JUNE
      Sunday            Monday            Tuesday           Wednesday         Thursday          Friday            Saturday

               1                 2                 3                 4                 5                 6                 7
               8                 9                10                11                12                13                14
              15                16                17                18                19                20                21
              22                23                24                25                26                27                28
              29                30


                                                            JULY
      Sunday            Monday            Tuesday           Wednesday         Thursday          Friday            Saturday
                                             1                 2                 3                 4                 5
               6                 7                 8                 9                10                11                12
              13                14                15                16                17                18                19
              20                21                22                23                24                25                26
              27                28                29                30                31


                                                            AUGUST
      Sunday            Monday            Tuesday           Wednesday         Thursday          Friday            Saturday
                                                                                                   1                 2
               3                 4                 5                 6                 7                 8                 9
              10                11                12                13                14                15                16
              17                18                19                20                21                22                23
              24                25                26                27                28                29                30
              31


                                                            SEPTEMBER
      Sunday            Monday            Tuesday           Wednesday         Thursday          Friday            Saturday
                           1                 2                 3                 4                 5                 6
               7                 8                 9                10                11                12                13
              14                15                16                17                18                19                20
              21                22                23                24                25                26                27
              28                29                30


                                                            OCTOBER
      Sunday            Monday            Tuesday           Wednesday         Thursday          Friday            Saturday
                                                               1                 2                 3                 4
               5                 6                 7                 8                 9                10                11
              12                13                14                15                16                17                18
              19                20                21                22                23                24                25
              26                27                28                29                30                31


                                                            NOVEMBER
      Sunday            Monday            Tuesday           Wednesday         Thursday          Friday            Saturday
                                                                                                                     1
               2                 3                 4                 5                 6                 7                 8
               9                10                11                12                13                14                15
              16                17                18                19                20                21                22
              23                24                25                26                27                28                29
              30


                                                            DECEMBER
      Sunday            Monday            Tuesday           Wednesday         Thursday          Friday            Saturday
                           1                 2                 3                 4                 5                 6
               7                 8                 9                10                11                12                13
              14                15                16                17                18                19                20
              21                22                23                24                25                26                27
              28                29                30                31
```



## XPL0


```XPL0
include c:\cxpl\codes;          \intrinsic 'code' declarations

func WeekDay(Year, Month, Day); \Return day of week (0=Sun 1=Mon..6=Sat)
int  Year, Month, Day;          \Works for years from 1583 onward
[if Month<=2 then [Month:= Month+12;  Year:= Year-1];
return rem((Day-1 + (Month+1)*26/10 + Year + Year/4 + Year/100*6 + Year/400)/7);
];

proc Space(N);                  \Display N space characters
int  N;
while N do [ChOut(0, ^ );  N:= N-1];

proc Calendar(Year);            \Display calendar for specified year
int  Year;
int  Month, Col, C, Line, MoName, Days, DayMax, Day(3);
[MoName:= [
  " January ", " February", "  March  ", "  April  ", "   May   ", "   June  ",
  "   July  ", "  August ", "September", "  October", " November", " December"];
Space(35);  Text(0, "[Snoopy]");  CrLf(0);
Space(37);  IntOut(0, Year);  CrLf(0);
CrLf(0);
for Month:= 1 to 12 do
  [for Col:= 0 to 3-1 do
    [Space(5);  Text(0, MoName(Month+Col-1));  Space(7);
    if Col<2 then Space(8);
    ];
  CrLf(0);
  for Col:= 0 to 3-1 do
    [Text(0, "Su Mo Tu We Th Fr Sa");
    if Col<2 then Space(9);
    ];
  CrLf(0);
  for Col:= 0 to 3-1 do         \day of first Sunday of month (can be negative)
    Day(Col):= 1 - WeekDay(Year, Month+Col, 1);
  for Line:= 0 to 6-1 do
    [for Col:= 0 to 3-1 do
      [Days:= [0, 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31];
      DayMax:= Days(Month+Col);
      if Month+Col=2 & (rem(Year/4)=0 & rem(Year/100)#0 ! rem(Year/400)=0) then
        DayMax:= DayMax+1;      \if February and leap year then add a day
      for C:= 0 to 7-1 do
        [if Day(Col)>=1 & Day(Col)<=DayMax then
          [IntOut(0, Day(Col));
          if Day(Col)<10 then Space(1); \left justify
          ]
        else Space(2);          \suppress out of range days
        Space(1);
        Day(Col):= Day(Col)+1;
        ];
      if Col<2 then Space(8);
      ];
    CrLf(0);
    ];
  CrLf(0);
  Month:= Month+2;              \2+1 months per Col(umn)
  ];
];

Calendar(1969)
```


{{out}}

```txt

                                   [Snoopy]
                                     1969

      January                      February                      March
Su Mo Tu We Th Fr Sa         Su Mo Tu We Th Fr Sa         Su Mo Tu We Th Fr Sa
         1  2  3  4                            1                            1
5  6  7  8  9  10 11         2  3  4  5  6  7  8          2  3  4  5  6  7  8
12 13 14 15 16 17 18         9  10 11 12 13 14 15         9  10 11 12 13 14 15
19 20 21 22 23 24 25         16 17 18 19 20 21 22         16 17 18 19 20 21 22
26 27 28 29 30 31            23 24 25 26 27 28            23 24 25 26 27 28 29
                                                          30 31

       April                         May                          June
Su Mo Tu We Th Fr Sa         Su Mo Tu We Th Fr Sa         Su Mo Tu We Th Fr Sa
      1  2  3  4  5                      1  2  3          1  2  3  4  5  6  7
6  7  8  9  10 11 12         4  5  6  7  8  9  10         8  9  10 11 12 13 14
13 14 15 16 17 18 19         11 12 13 14 15 16 17         15 16 17 18 19 20 21
20 21 22 23 24 25 26         18 19 20 21 22 23 24         22 23 24 25 26 27 28
27 28 29 30                  25 26 27 28 29 30 31         29 30


        July                        August                     September
Su Mo Tu We Th Fr Sa         Su Mo Tu We Th Fr Sa         Su Mo Tu We Th Fr Sa
      1  2  3  4  5                         1  2             1  2  3  4  5  6
6  7  8  9  10 11 12         3  4  5  6  7  8  9          7  8  9  10 11 12 13
13 14 15 16 17 18 19         10 11 12 13 14 15 16         14 15 16 17 18 19 20
20 21 22 23 24 25 26         17 18 19 20 21 22 23         21 22 23 24 25 26 27
27 28 29 30 31               24 25 26 27 28 29 30         28 29 30
                             31

       October                     November                     December
Su Mo Tu We Th Fr Sa         Su Mo Tu We Th Fr Sa         Su Mo Tu We Th Fr Sa
         1  2  3  4                            1             1  2  3  4  5  6
5  6  7  8  9  10 11         2  3  4  5  6  7  8          7  8  9  10 11 12 13
12 13 14 15 16 17 18         9  10 11 12 13 14 15         14 15 16 17 18 19 20
19 20 21 22 23 24 25         16 17 18 19 20 21 22         21 22 23 24 25 26 27
26 27 28 29 30 31            23 24 25 26 27 28 29         28 29 30 31
                             30

```



## Yabasic

{{trans|Lua}}

```Yabasic

clear screen

sub snoopy()
	local n, a$

	n = open("snoopy.txt", "r")

	while(not eof(#n))
		line input #n a$
		print "  "; : print color("black", "white") a$
	wend

	close #n
end sub

sub floor(n)
	return int(n + 0.5)
end sub

sub string.rep$(s$, n)
	local i, r$

	for i = 1 to n
		r$ = r$ + s$
	next i

	return r$
end sub

sub center$(s$, width)
	local fill1

	fill1 = floor(width - len(s$)) / 2

	return string.rep$(" ",fill1) + s$ + string.rep$(" ",fill1)
end sub

sub makeMonth(name, skip, days, cal$(), j)
	local cal, curday, line$, i

	curday = 1 - skip
	cal = 3

	cal$(j, 2) = " " + daysTitle$ + " "
	//cal$(j, 1) = center$(months$(name),len(cal$(j, 2)))
	cal$(j, 1) = left$(months$(name) + string.rep$(" ", 80), len(cal$(j, 2)))

	while(cal < 9)
  		line$ = ""
  		for i = 1 to 7
    		    if curday < 1 or curday > days then
      			line$ = line$ + "   "
    		    else
      			line$ = line$ + str$(curday, "###")
    		    end if
    		    curday = curday + 1
  		next
  		cal = cal + 1
  		cal$(j, cal) = line$ + " "
	wend
end sub

dim months$(12)
n = token("JANUARY,FEBRUARY,MARCH,APRIL,MAY,JUNE,JULY,AUGUST,SEPTEMBER,OCTOBER,NOVEMBER,DECEMBER", months$(), ",")
daysTitle$ = "MO TU WE TH FR SA SU"
dim daysPerMonth(12)
for n = 1 to 12
	read daysPerMonth(n)
next
data 31,28,31,30,31,30,31,31,30,31,30,31

sub print_cal(year)
	local i, q, l, m, startday, sep, monthwidth, calwidth, dpm, calendar$(12, 9), line$(3)

	startday=mod(((year-1)*365+floor((year-1)/4)-floor((year-1)/100)+floor((year-1)/400)),7)
 	if not mod(year,4) and mod(year,100) or not mod(year,400) then
    	    daysPerMonth(2)=29
  	end if

  	sep = 5
  	monthwidth = len(daysTitle$)
	calwidth = 3 * monthwidth + 2 * sep

	for i = 1 to 12
    	    dpm = daysPerMonth(i)
    	    makeMonth(i, startday, dpm, calendar$(), i)
    	    startday = mod(startday + dpm, 7)
	next

	snoopy()
 	print center$("--- " + str$(year) + " ---", calwidth), "\n"

 	print string.rep$(" ", sep + 1);
 	for q = 0 to 3
     	    for l = 1 to 9
       		for m = 1 to 3
         		print calendar$(q * 3 + m, l);
       		next
       		print
       		print string.rep$(" ", sep);
     	    next
     	    print
     	    print string.rep$(" ", sep + 1);
 	next
end sub

print_cal(2018)

```



## zkl

Gregorian calender only. Hard coded to a fit on a 80x43 terminal (somewhat half assed).

```zkl
var [const] D=Time.Date, days="Su Mo Tu We Th Fr Sa";

fcn center(text,m) { String(" "*((m-text.len())/2),text) }

fcn oneMonth(year,month){
   day1:=D.zeller(year,month,1);    //1969-1-1 -->3 (Wed, ISO 8601)
   dayz:=D.daysInMonth(year,month); //1969-1   -->31
   List(center(D.monthNames[month],days.len()),days).extend(
      (1).pump(dayz,(0).pump(day1,List,T(Void,""))).apply("%2s ".fmt)
      .pump(List,T(Void.Read,days.len()/3,False),String.create));
}

const M=70;	// mystery number
fcn oneYear(y=1969,title="3 Days of Peace & Music"){
   println(center(title,M),"\n",center(y.toString(),M),"\n");
   [1..12,3].pump(String,'wrap(m){  // 3 months per line
      mmm:=(m).pump(3,List,oneMonth.fp(y)); //L(L(7-8 lines), L(...), L(...))
      if(mmm.apply("len").sum() % 3)  // months have diff # of lines, pad
	 mmm=mmm.apply("append","");
      Utils.zipWith("%-25s%-25s%-25s\n".fmt,
           mmm.xplode()).concat() + (if (m<D.October) "\n" else "")
   })
}

if(vm.numArgs){ y:=vm.nthArg(0).toInt(); oneYear(y,"").println(); }
else oneYear().println();
```

oneMonth produces a list of strings that make up a one month calender. Each day is three characters, the first line is padded with blanks to line it up with the 1st day of the week. The entire month is one long list of "days" that is then chunked into a list of weeks (one line per week). oneYear takes 3 months and builds the page line by line.
{{out}}
<pre style="height:45ex">
                      3 Days of Peace & Music
                                 1969

      January                  February                  March
Su Mo Tu We Th Fr Sa     Su Mo Tu We Th Fr Sa     Su Mo Tu We Th Fr Sa
          1  2  3  4                        1                        1
 5  6  7  8  9 10 11      2  3  4  5  6  7  8      2  3  4  5  6  7  8
12 13 14 15 16 17 18      9 10 11 12 13 14 15      9 10 11 12 13 14 15
19 20 21 22 23 24 25     16 17 18 19 20 21 22     16 17 18 19 20 21 22
26 27 28 29 30 31        23 24 25 26 27 28        23 24 25 26 27 28 29
                                                  30 31

       April                     May                      June
Su Mo Tu We Th Fr Sa     Su Mo Tu We Th Fr Sa     Su Mo Tu We Th Fr Sa
       1  2  3  4  5                  1  2  3      1  2  3  4  5  6  7
 6  7  8  9 10 11 12      4  5  6  7  8  9 10      8  9 10 11 12 13 14
13 14 15 16 17 18 19     11 12 13 14 15 16 17     15 16 17 18 19 20 21
20 21 22 23 24 25 26     18 19 20 21 22 23 24     22 23 24 25 26 27 28
27 28 29 30              25 26 27 28 29 30 31     29 30


        July                    August                 September
Su Mo Tu We Th Fr Sa     Su Mo Tu We Th Fr Sa     Su Mo Tu We Th Fr Sa
       1  2  3  4  5                     1  2         1  2  3  4  5  6
 6  7  8  9 10 11 12      3  4  5  6  7  8  9      7  8  9 10 11 12 13
13 14 15 16 17 18 19     10 11 12 13 14 15 16     14 15 16 17 18 19 20
20 21 22 23 24 25 26     17 18 19 20 21 22 23     21 22 23 24 25 26 27
27 28 29 30 31           24 25 26 27 28 29 30     28 29 30
                         31

      October                  November                 December
Su Mo Tu We Th Fr Sa     Su Mo Tu We Th Fr Sa     Su Mo Tu We Th Fr Sa
          1  2  3  4                        1         1  2  3  4  5  6
 5  6  7  8  9 10 11      2  3  4  5  6  7  8      7  8  9 10 11 12 13
12 13 14 15 16 17 18      9 10 11 12 13 14 15     14 15 16 17 18 19 20
19 20 21 22 23 24 25     16 17 18 19 20 21 22     21 22 23 24 25 26 27
26 27 28 29 30 31        23 24 25 26 27 28 29     28 29 30 31
                         30

```

