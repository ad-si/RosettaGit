+++
title = "Horizontal sundial calculations"
description = ""
date = 2019-04-24T13:56:11Z
aliases = []
[extra]
id = 7588
[taxonomies]
categories = ["task", "sciences"]
tags = []
languages = [
  "ada",
  "algol_68",
  "autohotkey",
  "awk",
  "bbc_basic",
  "c",
  "cobol",
  "csharp",
  "d",
  "dwscript",
  "erre",
  "euphoria",
  "factor",
  "forth",
  "fortran",
  "freebasic",
  "futurebasic",
  "go",
  "haskell",
  "j",
  "java",
  "julia",
  "kotlin",
  "liberty_basic",
  "livecode",
  "logo",
  "microsoft_small_basic",
  "nim",
  "objeck",
  "ocaml",
  "octave",
  "oorexx",
  "pascal",
  "perl",
  "perl_6",
  "phix",
  "picolisp",
  "powershell",
  "purebasic",
  "python",
  "racket",
  "rexx",
  "ring",
  "ruby",
  "run_basic",
  "sather",
  "scala",
  "seed7",
  "sidef",
  "smalltalk",
  "tcl",
  "x86_assembly",
  "xbasic",
  "xpl0",
  "zkl",
  "zx_spectrum_basic",
]
+++

## Task

Create a program that calculates the hour, sun hour angle, dial hour line angle from 6am to 6pm for an ''operator'' entered location.


For example, the user is prompted for a location and inputs the latitude and longitude 4°57′S 150°30′W (4.95°S 150.5°W of [[wp:Jules Verne|Jules Verne]]'s ''[[wp:The Mysterious Island|Lincoln Island]]'', aka ''[[wp:Ernest Legouve Reef|Ernest Legouve Reef]])'', with a legal meridian of 150°W.

(Note: the "meridian" is approximately the same concept as the "longitude" - the distinction is that the meridian is used to determine when it is "noon" for official purposes. This will typically be slightly different from when the sun appears at its highest location, because of the structure of time zones. For most, but not all, time zones (hour wide zones with hour zero centred on Greenwich), the legal meridian will be an even multiple of 15 degrees.)

Wikipedia: A [[wp:sundial|sundial]] is a device that measures time by the position of the [[wp:Sun|Sun]].   In common designs such as the horizontal sundial, the sun casts a [[wp:shadow|shadow]] from its ''style'' (also called its [[wp:Gnomon|Gnomon]], a thin rod or a sharp, straight edge)<!--  The style is the time telling edge of the gnomon, Waugh,1973 p29--> onto a flat surface marked with lines indicating the hours of the day (also called the [[wp:Sundial#Terminology|dial face]] or dial plate). As the sun moves across the sky, the shadow-edge progressively aligns with different hour-lines on the plate.  Such designs rely on the style being aligned with the axis of the Earth's rotation.  Hence, if such a sundial is to tell the correct time, the style must point towards [[wp:true north|true north]] (not the [[wp:North Magnetic Pole|north]] or [[wp:Magnetic South Pole|south magnetic pole]]) and the style's angle with horizontal must equal the sundial's geographical [[wp:latitude|latitude]].





## Ada

sundial.adb:

```Ada
with Ada.Text_IO;
with Ada.Numerics.Elementary_Functions;
procedure Sundial is
   use Ada.Numerics.Elementary_Functions;
   use Ada.Numerics;
   package Float_IO is new Ada.Text_IO.Float_IO (Float);

   Latitude, Longitude, Meridian : Float;
   Latitude_Sine                 : Float;
begin
   Ada.Text_IO.Put ("Enter latitude:       ");
   Float_IO.Get (Latitude);
   Ada.Text_IO.Put ("Enter longitude:      ");
   Float_IO.Get (Longitude);
   Ada.Text_IO.Put ("Enter legal meridian: ");
   Float_IO.Get (Meridian);
   Ada.Text_IO.New_Line;

   Latitude_Sine := Sin (Latitude * Pi / 180.0);
   Ada.Text_IO.Put_Line
     ("   sine of latitude:" & Float'Image (Latitude_Sine));
   Ada.Text_IO.Put_Line
     ("   diff longitude:" & Float'Image (Longitude - Meridian));
   Ada.Text_IO.New_Line;

   Ada.Text_IO.Put_Line
     ("hour, sun hour angle, dial hour line angle from 6am to 6pm");
   for H in -6 .. 6 loop
      declare
         Hour_Angle : constant Float :=
            15.0 * Float (H) - (Longitude - Meridian);
         Line_Angle : constant Float :=
            Arctan (Latitude_Sine * Tan (Hour_Angle * Pi / 180.0)) * 180.0 /
            Pi;
      begin
         Ada.Text_IO.Put_Line
           ("HR=" &
            Integer'Image (H) &
            "; HRA=" &
            Float'Image (Hour_Angle) &
            "; HLA=" &
            Float'Image (Line_Angle));
      end;
   end loop;
end Sundial;
```

```txt
Enter latitude:       -4.95
Enter longitude:      -150.5
Enter legal meridian: -150

   sine of latitude:-8.62864E-02
   diff longitude:-5.00000E-01

hour, sun hour angle, dial hour line angle from 6am to 6pm
HR=-6; HRA=-8.95000E+01; HLA= 8.42248E+01
HR=-5; HRA=-7.45000E+01; HLA= 1.72829E+01
HR=-4; HRA=-5.95000E+01; HLA= 8.33371E+00
HR=-3; HRA=-4.45000E+01; HLA= 4.84671E+00
HR=-2; HRA=-2.95000E+01; HLA= 2.79487E+00
HR=-1; HRA=-1.45000E+01; HLA= 1.27835E+00
HR= 0; HRA= 5.00000E-01; HLA=-4.31443E-02
HR= 1; HRA= 1.55000E+01; HLA=-1.37079E+00
HR= 2; HRA= 3.05000E+01; HLA=-2.90964E+00
HR= 3; HRA= 4.55000E+01; HLA=-5.01802E+00
HR= 4; HRA= 6.05000E+01; HLA=-8.67140E+00
HR= 5; HRA= 7.55000E+01; HLA=-1.84510E+01
HR= 6; HRA= 9.05000E+01; HLA= 8.42248E+01
```



## ALGOL 68

Example extracted - with permission for a GPL - from Simon Wheaton-Smith's [http://www.illustratingshadows.com/stats-Algol.html Illustrating Time's Shadow] web page.
<!-- So... can I have your permission to cut and paste your hdial program as GPLed open-source on RosettaCode.Org?
On Wed, 2010-06-23 at 19:00 -0700, Simon [illustratingshadows wrote:
> Thanks for the email.
>
> Go for it. Everything I did was open source, you have my blessing.
>
> Simon
-->

```algol68
BEGIN
  REAL lat, slat, lng, ref;
  print ( "Enter latitude       => " ); read (lat);
  print ( "Enter longitude      => " ); read (lng);
  print ( "Enter legal meridian => " ); read (ref);
  new line(stand out);

  slat := sin(lat*2*pi/360) ;
  print ( ("    sine of latitude:   ", float(slat,8,2,1), new line ) );
  print ( ("    diff longitude:     ", fixed((lng - ref),0,3), new line, new line ) );

  print ( ("Hour, sun hour angle, dial hour line angle from 6am to 6pm", new line ));

  FOR h FROM -6 TO 6
  DO
     REAL hra , hla ;              # define hour angle and hour line angle #
     hra := 15 * h ;               # hour angle is 15 times the hour #
     hra := hra - (lng - ref);     # but correct for longitude difference #
     hla := arc tan ( slat * tan(hra*2*pi/360) ) * 360 / ( 2*pi) ;
     # page 132 of a68gdoc.pdf documentationfile #
     print ("HR="+whole(h,3)+"; HRA="+fixed(hra,8,3)+"; HLA="+fixed(hla,8,3));
     new line(stand out)
  OD
END
```

```txt

Enter latitude       => -4.95
Enter longitude      => -150.5
Enter legal meridian => -150

    sine of latitude:   -86.3e-3
    diff longitude:     -.500

Hour, sun hour angle, dial hour line angle from 6am to 6pm
HR= -6; HRA= -89.500; HLA= +84.225
HR= -5; HRA= -74.500; HLA= +17.283
HR= -4; HRA= -59.500; HLA=  +8.334
HR= -3; HRA= -44.500; HLA=  +4.847
HR= -2; HRA= -29.500; HLA=  +2.795
HR= -1; HRA= -14.500; HLA=  +1.278
HR= +0; HRA=  +0.500; HLA=  -0.043
HR= +1; HRA= +15.500; HLA=  -1.371
HR= +2; HRA= +30.500; HLA=  -2.910
HR= +3; HRA= +45.500; HLA=  -5.018
HR= +4; HRA= +60.500; HLA=  -8.671
HR= +5; HRA= +75.500; HLA= -18.451
HR= +6; HRA= +90.500; HLA= +84.225

```



## AutoHotkey

AutoHotkey is not a command-line programming language, let me make that clear. However, in translating the F# I found that the command line really is best for this type of app. The first 3 comments in the script describe the workarounds used to interface with the commandline.

```AutoHotkey
DllCall("AllocConsole")  ; Open a console window for this application
Pi := 4*ATan(1)
,Degrees := Pi/180

FileAppend, Enter Latitude: , CONOUT$ ; write to  stdout
FileReadLine, latitude, CONIN$, 1     ; read from stdin

FileAppend, Enter Longitude: , CONOUT$
FileReadLine, longitude, CONIN$, 1

FileAppend, Enter Legal meridian: , CONOUT$
FileReadLine, meridian, CONIN$, 1

sineLatitude := Sin(latitude*Degrees)
FileAppend, `n, CONOUT$
FileAppend, Sine of latitude: %sineLatitude%`n, CONOUT$
FileAppend, % "Difference of Longitudes (given longitude - meridian): " . longitude-meridian . "`n", CONOUT$
FileAppend, `n, CONOUT$

FileAppend, Numbers from 6 AM to 6 PM:`n, CONOUT$
FileAppend, Hour`t`tSun Hour Angle`t Dial hour line angle`n, CONOUT$


hour := -7
While (++hour < 7)
{
   clockHour := hour < 0 ? abs(hour) . "AM" : hour . "PM"
   shr := RTrim("" . (15.0*hour - (longitude-meridian)), "0") ; RTrim() removes trailing zeroes
   dhla := Atan(sineLatitude*Tan(shr*degrees))/Degrees
   FileAppend, %clockhour%`t`t%shr%`t`t%dhla%`n, CONOUT$
}
MsgBox close me when done.
```

```txt
Enter Latitude:-4.95
Enter Longitude:-150.5
Enter Legal meridian:-150

Sine of latitude: -0.086286
Difference of Longitudes (given longitude - meridian): -0.500000

Numbers from 6 AM to 6 PM:
Hour            Sun Hour Angle   Dial hour line angle
6AM             -89.5           84.224833
5AM             -74.5           17.282934
4AM             -59.5           8.333712
3AM             -44.5           4.846709
2AM             -29.5           2.794874
1AM             -14.5           1.278353
0PM             0.5             -0.043144
1PM             15.5            -1.370788
2PM             30.5            -2.909643
3PM             45.5            -5.018023
4PM             60.5            -8.671397
5PM             75.5            -18.450999
6PM             90.5            84.224833
```



## AWK


```AWK

# syntax: GAWK -f HORIZONTAL_SUNDIAL_CALCULATIONS.AWK
BEGIN {
    printf("enter latitude (degrees): ") ; getline latitude
    printf("enter longitude (degrees): ") ; getline longitude
    printf("enter legal meridian (degrees): ") ; getline meridian
    printf("\nhour  sun hour angle  dial hour line angle\n")
    slat = sin(dr(latitude))
    for (hour=-6; hour<=6; hour++) { # 6AM-6PM
      hra = 15 * hour - longitude + meridian
      hraRad = dr(hra)
      hla = rd(atan2(sin(hraRad)*slat,cos(hraRad)))
      printf("%4d %15.3f %21.3f\n",hour+12,hra,hla)
    }
    exit(0)
}
function dr(x) { return x * 3.14159265 / 180 } # degrees to radians
function rd(x) { return x * 180 / 3.14159265 } # radians to degrees

```

<p>output:</p>

```txt

enter latitude (degrees): -4.95
enter longitude (degrees): -150.5
enter legal meridian (degrees): -150

hour  sun hour angle  dial hour line angle
   6         -89.500                84.225
   7         -74.500                17.283
   8         -59.500                 8.334
   9         -44.500                 4.847
  10         -29.500                 2.795
  11         -14.500                 1.278
  12           0.500                -0.043
  13          15.500                -1.371
  14          30.500                -2.910
  15          45.500                -5.018
  16          60.500                -8.671
  17          75.500               -18.451
  18          90.500               -95.775

```


## BBC BASIC

```bbcbasic
      INSTALL @lib$+"FNUSING"

      INPUT "Enter latitude (degrees)      : " latitude
      INPUT "Enter longitude (degrees)     : " longitude
      INPUT "Enter legal meridian (degrees): " meridian

      PRINT '" Time", "Sun hour angle", "Dial hour line angle"

      FOR hour = 6 TO 18
        hra = 15*hour - longitude + meridian - 180
        hla = DEG(ATN(SIN(RAD(latitude)) * TAN(RAD(hra))))
        IF ABS(hra) > 90 hla += 180 * SGN(hra * latitude)
        PRINT FNusing("##.##", hour), FNusing("  ####.###  ", hra), FNusing("  ####.###", hla)
      NEXT hour
```

(note the correct '''negative''' value for time 18:00)

```txt
Enter latitude (degrees)      : -4.95
Enter longitude (degrees)     : -150.5
Enter legal meridian (degrees): -150.0

 Time     Sun hour angle      Dial hour line angle
 6.00        -89.500              84.225
 7.00        -74.500              17.283
 8.00        -59.500               8.334
 9.00        -44.500               4.847
10.00        -29.500               2.795
11.00        -14.500               1.278
12.00          0.500              -0.043
13.00         15.500              -1.371
14.00         30.500              -2.910
15.00         45.500              -5.018
16.00         60.500              -8.671
17.00         75.500             -18.451
18.00         90.500             -95.775
```



## C

```c
#include <stdio.h>
#include <math.h>

#define PICKVALUE(TXT, VM) do {			\
    printf("%s: ", TXT);			\
    scanf("%lf", &VM);				\
  } while(0);

#if !defined(M_PI)
#define M_PI 3.14159265358979323846
#endif

#define DR(X) ((X)*M_PI/180.0)
#define RD(X) ((X)*180.0/M_PI)

int main()
{
  double lat, slat, lng, ref;
  int h;

  PICKVALUE("Enter latitude", lat);
  PICKVALUE("Enter longitude", lng);
  PICKVALUE("Enter legal meridian", ref);
  printf("\n");

  slat = sin(DR(lat));
  printf("sine of latitude: %.3f\n", slat);
  printf("diff longitude: %.3f\n\n", lng - ref);

  printf("Hour, sun hour angle, dial hour line angle from 6am to 6pm\n");

  for(h = -6; h <= 6; h++)
  {
    double hla, hra;
    hra = 15.0*h;
    hra = hra - lng + ref;
    hla = RD(atan(slat * tan(DR(hra))));
    printf("HR= %3d;  \t  HRA=%7.3f;  \t  HLA= %7.3f\n",
	   h, hra, hla);
  }

  return 0;
}
```


## C#

```c#
using System;

namespace RosettaCode
{
  internal sealed class Program
  {
    private static void Main()
    {
      Func<double> getDouble = () => Convert.ToDouble(Console.ReadLine());
      double h = 0, lat, lng, lme, slat, hra, hla;

      Console.Write("Enter latitude       => ");
      lat = getDouble();
      Console.Write("Enter longitude      => ");
      lng = getDouble();
      Console.Write("Enter legal meridian => ");
      lme = getDouble();

      slat = Math.Sin(lat*2*Math.PI/360);
      Console.WriteLine("\n    sine of latitude:   {0:0.000}", slat);
      Console.WriteLine("    diff longitude:     {0:0.000}\n", lng-lme);
      Console.WriteLine("Hour, sun hour angle, dial hour line angle from 6am to 6pm");
      for (h = -6; h<6; h++)
      {
        hra = 15*h;
        hra -= lng-lme;
        hla = Math.Atan(slat*Math.Tan(hra*2*Math.PI/360))*360/(2*Math.PI);
        Console.WriteLine("HR= {0,7:0.000}; HRA {1,7:0.000}; HLA= {2,7:0.000}", h, hra, hla);
      }
    }
  }
}
```



## COBOL

```cobol
PROGRAM-ID. horizontal-sundial-calc.

DATA DIVISION.
WORKING-STORAGE SECTION.
01  latitude                       PIC S9(3)V9(5) COMP.
01  longitude                      PIC S9(3)V9(5) COMP.
01  legal-meridian                 PIC S9(3)V9(5) COMP.

01  lat-sine                       PIC S9(3)V9(5) COMP.
01  diff-longitude                 PIC S9(3)V9(5) COMP.

01  lat-sine-disp                  PIC -(3)9.9(5).
01  diff-longitude-disp            PIC -(3)9.9(5).

01  hour                           PIC S9 COMP.
01  sun-hour-angle                 PIC S9(3)V9(5) COMP.
01  dial-hour-line-angle           PIC S9(3)V9(5) COMP.

01  hour-disp                      PIC 99.
01  sun-hour-angle-disp            PIC -(3)9.9(5).
01  dial-hour-line-angle-disp      PIC -(3)9.9(5).

PROCEDURE DIVISION.
    DISPLAY "Enter latitude: " NO ADVANCING
    ACCEPT latitude
    DISPLAY "Enter longitude: " NO ADVANCING
    ACCEPT longitude
    DISPLAY "Enter legal meridian: " NO ADVANCING
    ACCEPT legal-meridian
    DISPLAY SPACE

    COMPUTE lat-sine, lat-sine-disp ROUNDED =
        FUNCTION SIN(latitude * 2 * FUNCTION PI / 360)
    DISPLAY "Sine of latitude: " FUNCTION TRIM(lat-sine-disp)

    SUBTRACT legal-meridian FROM longitude
        GIVING diff-longitude, diff-longitude-disp
    DISPLAY "Diff longitude: " FUNCTION TRIM(diff-longitude-disp)
    DISPLAY SPACE

    DISPLAY "Time   Sun hour angle  Dial hour line angle"
    PERFORM VARYING hour FROM -6 BY 1 UNTIL hour > 6
        COMPUTE sun-hour-angle ROUNDED = hour * 15 - diff-longitude
        COMPUTE dial-hour-line-angle ROUNDED = FUNCTION ATAN(lat-sine
            * FUNCTION TAN(sun-hour-angle * 2 * FUNCTION PI / 360))
            * 360 / (2 * FUNCTION PI)

        ADD 12 TO hour GIVING hour-disp
        MOVE sun-hour-angle TO sun-hour-angle-disp
        MOVE dial-hour-line-angle TO dial-hour-line-angle-disp
        DISPLAY hour-disp ":00 " sun-hour-angle-disp "      "
            dial-hour-line-angle-disp
    END-PERFORM
    .
```


```txt

Enter latitude: -4.95
Enter longitude: -150.5
Enter legal meridian: -150

Sine of latitude: -0.08629
Diff longitude: -0.50000

Time   Sun hour angle  Dial hour line angle
06:00  -89.50000        84.22441
07:00  -74.50000        17.28173
08:00  -59.50000         8.33311
09:00  -44.50000         4.84635
10:00  -29.50000         2.79467
11:00  -14.50000         1.27826
12:00    0.50000        -0.04314
13:00   15.50000        -1.37069
14:00   30.50000        -2.90943
15:00   45.50000        -5.01765
16:00   60.50000        -8.67077
17:00   75.50000       -18.44973
18:00   90.50000        84.22441

```



## D

```d
import std.stdio, std.math, std.conv, std.string;

double radians(in double x) pure nothrow { return x * (PI / 180); }
double degrees(in double x) pure nothrow { return x / (PI / 180); }

T input(T)(in string msg) {
    msg.write;
    return readln.strip.to!T;
}

void main() {
    immutable lat = input!double("Enter latitude       => ");
    immutable lng = input!double("Enter longitude      => ");
    immutable lme = input!double("Enter legal meridian => ");
    writeln;

    double slat = lat.radians.sin;
    writefln("    sine of latitude:   %.3f", slat);
    writefln("    diff longitude:     %.3f", lng - lme);
    writeln;
    "Hour, sun hour angle, dial hour line angle from 6am to 6pm".writeln;

    foreach (immutable h; -6 .. 7) {
        immutable double hra = 15 * h - (lng - lme);
        immutable double hla = atan(slat * hra.radians.tan).degrees;
        writefln("HR=%3d; HRA=%7.3f; HLA=%7.3f", h, hra, hla);
    }
}
```

```txt
Enter latitude       => -4.95
Enter longitude      => -150.5
Enter legal meridian => -150

    sine of latitude:   -0.086
    diff longitude:     -0.500

Hour, sun hour angle, dial hour line angle from 6am to 6pm
HR= -6; HRA=-89.500; HLA= 84.225
HR= -5; HRA=-74.500; HLA= 17.283
HR= -4; HRA=-59.500; HLA=  8.334
HR= -3; HRA=-44.500; HLA=  4.847
HR= -2; HRA=-29.500; HLA=  2.795
HR= -1; HRA=-14.500; HLA=  1.278
HR=  0; HRA=  0.500; HLA= -0.043
HR=  1; HRA= 15.500; HLA= -1.371
HR=  2; HRA= 30.500; HLA= -2.910
HR=  3; HRA= 45.500; HLA= -5.018
HR=  4; HRA= 60.500; HLA= -8.671
HR=  5; HRA= 75.500; HLA=-18.451
HR=  6; HRA= 90.500; HLA= 84.225

```



## DWScript

```delphi
procedure PrintSundial(lat, lng, lme : Float);
begin
   PrintLn(Format('latitude:        %7.2f', [lat]));
   PrintLn(Format('longitude:       %7.2f', [lng]));
   PrintLn(Format('legal meridian:  %7.2f', [lme]));

   var slat := Sin(DegToRad(lat));

   PrintLn(Format('sine of latitude: %.3f', [slat]));
   PrintLn(Format('diff longitude:   %.3f', [lng-lme]));
   PrintLn('');
   PrintLn('Hour, sun hour angle, dial hour line angle from 6am to 6pm');

   var h : Integer;
   for h:=-6 to 6 do begin
      var hra := 15 * h - (lng - lme);
      var hraRad := DegToRad(hra);
      var hla :=RadToDeg(ArcTan2(Sin(hraRad)*slat, Cos(hraRad)));
      PrintLn(Format('HR=%3d; HRA=%7.3f; HLA=%7.3f', [h, hra, hla]));
   end
end;

PrintSundial(-4.95, -150.5, -150);
```

```txt
latitude:          -4.95
longitude:       -150.50
legal meridian:  -150.00
sine of latitude: -0.086
diff longitude:   -0.500

Hour, sun hour angle, dial hour line angle from 6am to 6pm
HR= -6; HRA=-89.500; HLA= 84.225
HR= -5; HRA=-74.500; HLA= 17.283
HR= -4; HRA=-59.500; HLA=  8.334
HR= -3; HRA=-44.500; HLA=  4.847
HR= -2; HRA=-29.500; HLA=  2.795
HR= -1; HRA=-14.500; HLA=  1.278
HR=  0; HRA=  0.500; HLA= -0.043
HR=  1; HRA= 15.500; HLA= -1.371
HR=  2; HRA= 30.500; HLA= -2.910
HR=  3; HRA= 45.500; HLA= -5.018
HR=  4; HRA= 60.500; HLA= -8.671
HR=  5; HRA= 75.500; HLA=-18.451
HR=  6; HRA= 90.500; HLA=-95.775
```



## ERRE


```ERRE
PROGRAM SUN_DIAL

FUNCTION RAD(X)
    RAD=X*π/180
END FUNCTION

FUNCTION DEG(X)
    DEG=X*180/π
END FUNCTION

BEGIN

      INPUT("Enter latitude (degrees)      : ",latitude)
      INPUT("Enter longitude (degrees)     : ",longitude)
      INPUT("Enter legal meridian (degrees): ",meridian)

      PRINT
      PRINT(" Time    Sun hour angle  Dial hour line angle")
      PRINT("---------------------------------------------")

      FOR HOUR=6 TO 18 DO
        HRA=15*HOUR-LONGITUDE+MERIDIAN-180
        HLA=DEG(ATN(SIN(RAD(LATITUDE))*TAN(RAD(HRA))))
        IF ABS(HRA)>90 THEN HLA+=180*SGN(HRA*LATITUDE) END IF
        WRITE("##.##         ####.###       ####.###";HOUR;HRA;HLA)
      END FOR

END PROGRAM

```

```txt

Enter latitude (degrees)      : ? -4.95
Enter longitude (degrees)     : ? -150.5
Enter legal meridian (degrees): ? -150

 Time    Sun hour angle  Dial hour line angle
---------------------------------------------
 6.00          -89.500         84.225
 7.00          -74.500         17.283
 8.00          -59.500          8.334
 9.00          -44.500          4.847
10.00          -29.500          2.795
11.00          -14.500          1.278
12.00            0.500         -0.043
13.00           15.500         -1.371
14.00           30.500         -2.910
15.00           45.500         -5.018
16.00           60.500         -8.671
17.00           75.500        -18.451
18.00           90.500        -95.775

```



## Euphoria

```Euphoria

include std/console.e
include std/mathcons.e

atom lat = prompt_number("Enter Latitude: ",{})
atom lng = prompt_number("Enter Longitude: ",{})
atom lm = prompt_number("Enter Legal Meridian: ",{})
puts(1,'\n')

atom ha, hla

function D2R(atom degrees)
	return degrees * PI / 180
end function

function R2D(atom radians)
	return radians * 180 / PI
end function

function atan2(atom y, atom x)
	return 2*arctan((sqrt(power(x,2)+power(y,2)) - x)/y)
end function

atom s_lat = sin(D2R(lat))

puts(1,"Hour,  Sun Hour Angle, Dial Hour Line Angle\n")

for hour = -6 to 6 do
	ha = hour * 15 - lng + lm
	atom s = sin(D2R(ha))
	atom c = cos(D2R(ha))
	hla = R2D(atan2(s_lat*s,c))
	printf(1,"%3d\t\t\t%7.3f\t\t\t%7.3f\n",{hour+12,ha,hla})
end for

if getc(0) then end if

```

```txt

Enter Latitude: -4.95
Enter Longitude: -150.5
Enter Legal Meridian: -150

Hour,  Sun Hour Angle, Dial Hour Line Angle
  6	-89.500			 84.225
  7	-74.500			 17.283
  8	-59.500			  8.334
  9	-44.500			  4.847
 10	-29.500			  2.795
 11	-14.500			  1.278
 12	  0.500			 -0.043
 13	 15.500			 -1.371
 14	 30.500			 -2.910
 15	 45.500			 -5.018
 16	 60.500			 -8.671
 17	 75.500			-18.451
 18	 90.500			-95.775

```


=={{header|F_Sharp|F#}}==
```fsharp
// Learn more about F# at http://fsharp.net

open System

//(degree measure)*Degrees => Radian measure
//(radian measure)/Degrees => Degree measure
let Degrees = Math.PI / 180.0

Console.Write("Enter latitude: ")
let latitude = Console.ReadLine() |> Double.Parse

Console.Write("Enter longitude: ")
let longitude = Console.ReadLine() |> Double.Parse

Console.Write("Enter legal meridian: ")
let meridian = Console.ReadLine() |> Double.Parse

let sineLatitude = Math.Sin(latitude * Degrees)
Console.WriteLine()
Console.WriteLine("Sine of latitude: {0}",sineLatitude)
Console.WriteLine("Difference of Longitudes (given longitude - meridian): {0}",longitude-meridian)
Console.WriteLine()

printfn "Numbers from 6 AM to 6 PM: "
printfn "Hour\t\tSun hour angle\t Dial hour line angle"

for hour in -6..6 do
    let clockHour = if hour < 0 then String.Format("{0}AM",Math.Abs(hour)) else String.Format("{0}PM",hour)
    let shr = 15.0*(float)hour - (longitude - meridian)
    let dhla = Math.Atan(sineLatitude*Math.Tan(shr*Degrees))/Degrees;
    Console.WriteLine("{0}\t\t{1}\t\t{2:0.000}",clockHour,shr,dhla)
done

//To keep the console window open, can be omitted with block comment (" (* comment *) ")
Console.WriteLine("Press any key to continue...")
Console.ReadKey() |> ignore
```

```txt

Enter latitude: -4.95
Enter longitude: -150.5
Enter legal meridian: -150

Sine of latitude: -0.0862863657979234
Difference of Longitudes (given longitude - meridian): -0.5

Numbers from 6 AM to 6 PM:
Hour            Sun hour angle   Dial hour line angle
6AM             -89.5           84.225
5AM             -74.5           17.283
4AM             -59.5           8.334
3AM             -44.5           4.847
2AM             -29.5           2.795
1AM             -14.5           1.278
0PM             0.5             -0.043
1PM             15.5            -1.371
2PM             30.5            -2.910
3PM             45.5            -5.018
4PM             60.5            -8.671
5PM             75.5            -18.451
6PM             90.5            84.225
Press any key to continue...

```



## Factor

```factor
USING: formatting io kernel locals math math.functions math.libm
math.parser math.ranges math.trig sequences ;
IN: rosetta-code.sundial

: get-num ( str -- x ) write flush readln string>number ;

: get-input ( -- lat lng ref )
    "Enter latitude: " "Enter longitude: "
    "Enter legal meridian: " [ get-num ] tri@ ;

: .diff ( lat lng ref -- )
    - [ deg>rad sin ] dip
    "sine of latitude: %.3f\ndiff longitude: %.3f\n" printf ;

: line-angle ( lat hra-rad -- hla )
    [ deg>rad sin ] [ [ sin * ] [ cos ] bi ] bi* fatan2 rad>deg
    ;

:: .angles ( lat lng ref -- )
    "Hour, sun hour angle, dial hour line angle from 6am to 6pm"
    print
    -6 6 [a,b] [
        :> h 15.0 h * :> hra!
        ref hra lng - + hra!
        lat hra deg>rad line-angle :> hla
        h hra hla
        "HR= %3d;  \t  HRA=%7.3f;  \t  HLA= %7.3f\n" printf
    ] each ;

: sundial-demo ( -- ) get-input nl 3dup .diff nl .angles ;

MAIN: sundial-demo
```

```txt

Enter latitude: -4.95
Enter longitude: -150.5
Enter legal meridian: -150

sine of latitude: -0.086
diff longitude: -0.500

Hour, sun hour angle, dial hour line angle from 6am to 6pm
HR=  -6;  	  HRA=-89.500;  	  HLA=  84.225
HR=  -5;  	  HRA=-74.500;  	  HLA=  17.283
HR=  -4;  	  HRA=-59.500;  	  HLA=   8.334
HR=  -3;  	  HRA=-44.500;  	  HLA=   4.847
HR=  -2;  	  HRA=-29.500;  	  HLA=   2.795
HR=  -1;  	  HRA=-14.500;  	  HLA=   1.278
HR=   0;  	  HRA=  0.500;  	  HLA=  -0.043
HR=   1;  	  HRA= 15.500;  	  HLA=  -1.371
HR=   2;  	  HRA= 30.500;  	  HLA=  -2.910
HR=   3;  	  HRA= 45.500;  	  HLA=  -5.018
HR=   4;  	  HRA= 60.500;  	  HLA=  -8.671
HR=   5;  	  HRA= 75.500;  	  HLA= -18.451
HR=   6;  	  HRA= 90.500;  	  HLA= -95.775

```



## Forth


```forth
: faccept ( -- f )
  pad 32 accept pad swap >float 0= throw ;
: >radians ( deg -- rad ) 180e f/ pi f* ;
: >degrees ( rad -- deg ) pi f/ 180e f* ;
: sundial
  cr ." Enter latitude: "
  faccept >radians fsin
  cr ." Enter longitude: "
  faccept
  cr ." Enter legal meridian: "
  faccept f- fnegate   ( sin[latitude] -longitude )

  cr ." Hour : HourAngle , DialAngle"
  7 -6 do
    cr i 4 .r ." : "
    fover fover i 15 * s>d d>f f+
    fdup f. ." , "
    >radians fsincos fswap frot f* fswap fatan2 >degrees f.
  loop fdrop fdrop ;
```



## Fortran

{{works with|gfortran}} with <tt>-fbackslash</tt> option

```fortran
program SunDial

  real    :: lat, slat, lng, ref
  real    :: hra, hla
  integer :: h

  real, parameter :: pi = 3.14159265358979323846

  print *, "Enter latitude"
  read *, lat
  print *, "Enter longitude"
  read *, lng
  print *, "Enter legal meridian"
  read *, ref

  print *

  slat = sin(dr(lat))
  write(*, '(A,1F6.3)') "sine of latitude: ", slat
  write(*, '(A,1F6.3)') "diff longitude: ", lng - ref

  print *, "Hour, sun hour angle, dial hour line angle from 6am to 6pm"

  do h = -6, 6
     hra = 15.0*h
     hra = hra - lng + ref
     hla = rd( atan( slat * tan( dr(hra) ) ) )
     write(*, '(" HR= ",I3,";  \t  HRA=",F7.3,";  \t  HLA= ", F7.3)'), h, hra, hla
  end do

contains

  function dr(angle)
    real :: dr
    real, intent(in) :: angle
    dr = angle*pi/180.0
  end function dr

  function rd(angle)
    real :: rd
    real, intent(in) :: angle
    rd = angle*180.0/pi
  end function rd

end program SunDial
```


## freeBASIC

```freebasic
' version 04-11-2016
' compile with: fbc -s console

#Macro deg2rad (x)
    (x) * Atn(1) / 45
#EndMacro

#Macro rad2deg (x)
    (x) * 45 / Atn(1)
#EndMacro

' ------=< MAIN >=------

Dim As Double latitude, longitude, meridian, hra, hla
Dim As ULong h

Input "      Enter latitude (degrees): ", latitude
Input "     Enter longitude (degrees): ", longitude
Input "Enter legal meridian (degrees): ", meridian

Print
Print " Time  Sun hour angle  Dial hour line angle"

For h = 6 To 18
    hra = h * 15 - longitude + meridian - 180
    hla = rad2deg(Atn(Sin(deg2rad(latitude)) * Tan(deg2rad(hra))))
    If Abs(hra) > 90 Then hla += 180 * Sgn(hra * latitude)
    Print Using "##.##     ####.###         ####.###"; h; hra; hla
Next

' empty keyboard buffer
While InKey <> "" : Wend
Print : Print "hit any key to end program"
Sleep
End
```

```txt
      Enter latitude (degrees): -4.95
     Enter longitude (degrees): -150.5
Enter legal meridian (degrees): -150

 Time  Sun hour angle  Dial hour line angle
 6.00      -89.500           84.225
 7.00      -74.500           17.283
 8.00      -59.500            8.334
 9.00      -44.500            4.847
10.00      -29.500            2.795
11.00      -14.500            1.278
12.00        0.500           -0.043
13.00       15.500           -1.371
14.00       30.500           -2.910
15.00       45.500           -5.018
16.00       60.500           -8.671
17.00       75.500          -18.451
18.00       90.500          -95.775
```



## FutureBasic


```futurebasic

include "ConsoleWindow"

local fn rad2deg( theta as double ) as double
end fn = theta * 180 / pi

local fn deg2rad( theta as double ) as double
end fn = theta * pi / 180

local fn SolarHourAngle( latitude as double, longitude as double, meridian as double )
dim as long hour
dim as double hra, hla, time
dim as Str15 ap

print "Latitude  = "; latitude; chr$(13); "Longitude = "; longitude; chr$(13); "Meridian  = "; meridian
print : print "sine of latitude: "; sin(latitude * pi / 180 ); chr$(13); "  diff longitude: "; longitude - meridian
print : print "Time", "Sun hour angle", "Dial hour line angle"
for hour = 6 to 18
hra = ( 15 * hour ) - longitude + meridian - 180
hla = fn rad2deg( atn( sin( fn deg2rad( latitude ) ) * tan( fn deg2rad( hra ) )))
if abs( hra ) > 90 then hla = hla + 180 * sgn( hra * latitude )
if hour > 12 then time = hour - 12 : ap = " a.m." else time = hour : ap = " p.m."
print using "##"; time; ap, using "####.##"; hra, using "####.###"; hla
next hour
end fn

fn SolarHourAngle( -4.95, -150.5, -150.0 )

```


Output:

```txt

Latitude  = -4.95
Longitude = -150.5
Meridian  = -150

sine of latitude: -0.0862863658
  diff longitude: -0.5

Time            Sun hour angle  Dial hour line angle
 6 p.m.          -89.50           84.225
 7 p.m.          -74.50           17.283
 8 p.m.          -59.50            8.334
 9 p.m.          -44.50            4.847
10 p.m.          -29.50            2.795
11 p.m.          -14.50            1.278
12 p.m.            0.50           -0.043
 1 a.m.           15.50           -1.371
 2 a.m.           30.50           -2.910
 3 a.m.           45.50           -5.018
 4 a.m.           60.50           -8.671
 5 a.m.           75.50          -18.451
 6 a.m.           90.50          -95.775

```



## Go


```go
package main

import (
    "fmt"
    "math"
    "os"
)

func getnum(prompt string) (r float64) {
    fmt.Print(prompt)
    if _, err := fmt.Scan(&r); err != nil {
        fmt.Println(err)
        os.Exit(-1)
    }
    return
}

func main() {
    lat := getnum("Enter latitude       => ")
    lng := getnum("Enter longitude      => ")
    ref := getnum("Enter legal meridian => ")
    slat := math.Sin(lat * math.Pi / 180)
    diff := lng - ref
    fmt.Println("\n    sine of latitude:   ", slat)
    fmt.Println("    diff longitude:     ", diff)
    fmt.Println("\nHour, sun hour angle, dial hour line angle from 6am to 6pm")
    for h := -6.; h <= 6; h++ {
        hra := 15*h - diff
        s, c := math.Sincos(hra * math.Pi / 180)
        hla := math.Atan2(slat*s, c) * 180 / math.Pi
        fmt.Printf("%2.0f %8.3f %8.3f\n", h, hra, hla)
    }
}
```

```txt

Enter latitude       => -4.95
Enter longitude      => -150.5
Enter legal meridian => -150

    sine of latitude:    -0.08628636579792338
    diff longitude:      -0.5

Hour, sun hour angle, dial hour line angle from 6am to 6pm
-6  -89.500   84.225
-5  -74.500   17.283
-4  -59.500    8.334
-3  -44.500    4.847
-2  -29.500    2.795
-1  -14.500    1.278
 0    0.500   -0.043
 1   15.500   -1.371
 2   30.500   -2.910
 3   45.500   -5.018
 4   60.500   -8.671
 5   75.500  -18.451
 6   90.500  -95.775

```


=={{header|GW-BASIC}}==
```qbasic

10  ' Horizontal sundial calculations
20  PRINT "Enter latitude       => ";
30  INPUT LAT
40  PRINT "Enter longitude      => ";
50  INPUT LNG
60  PRINT "Enter legal meridian => ";
70  INPUT REF
80  PRINT
90  LET PI = 4 * ATN(1)
100 LET SLAT = SIN(LAT * PI / 180)
110 PRINT "    sine of latitude:   "; USING "#.##^^^^"; SLAT
120 PRINT "    diff longitude:     "; USING "####.###"; LNG - REF
130 PRINT
140 PRINT "Hour, sun hour angle, dial hour line angle from 6am to 6pm"
150 FOR H% = -6 TO 6
160  LET HRA = 15 * H%
170  LET HRA = HRA - (LNG - REF): ' correct for longitude difference
180  LET HLA = ATN(SLAT * TAN(HRA * PI / 180)) * 180 / PI
190  PRINT "HR="; USING "+##"; H%;
200  PRINT "; HRA="; USING "+###.###"; HRA;
210  PRINT "; HLA="; USING "+###.###"; HLA
220 NEXT H%
230 END

```

```txt

Enter latitude       => ? -4.95
Enter longitude      => ? -150.5
Enter legal meridian => ? -150

    sine of latitude:   -.86E-01
    diff longitude:       -0.500

Hour, sun hour angle, dial hour line angle from 6am to 6pm
HR= -6; HRA= -89.500; HLA= +84.225
HR= -5; HRA= -74.500; HLA= +17.283
HR= -4; HRA= -59.500; HLA=  +8.334
HR= -3; HRA= -44.500; HLA=  +4.847
HR= -2; HRA= -29.500; HLA=  +2.795
HR= -1; HRA= -14.500; HLA=  +1.278
HR= +0; HRA=  +0.500; HLA=  -0.043
HR= +1; HRA= +15.500; HLA=  -1.371
HR= +2; HRA= +30.500; HLA=  -2.910
HR= +3; HRA= +45.500; HLA=  -5.018
HR= +4; HRA= +60.500; HLA=  -8.671
HR= +5; HRA= +75.500; HLA= -18.451
HR= +6; HRA= +90.500; HLA= +84.225

```



## Haskell


```haskell
roundDec :: Int -> Double -> Double
roundDec d = (/ 10.0 ^ d) . fromIntegral . round . (* 10.0 ^ d)

radToDegr = ((180 / pi) *)

degrToRad = ((pi / 180) *)

main = do
  let lat = -4.95
      long = -150.5
      legalMerid = -150
      sinOfLat = sin $ degrToRad lat
      diff = legalMerid - long
  (putStrLn . unlines)
    [ "Latitude         " ++ show lat
    , "Longitude        " ++ show long
    , "Legal meridian   " ++ show legalMerid
    , "Sine of latitude " ++ show (roundDec 6 sinOfLat)
    , "Diff longitude   " ++ show (-diff)
    , "hour   sun hour angle   dial hour  line angle"
    ]
  mapM_
    (\h ->
        let sha = diff + 15 * h
            dhla = radToDegr . atan . (sinOfLat *) . tan $ degrToRad sha
        in putStrLn $
           take 7 (show h ++ repeat ' ') ++
           take 16 (show (roundDec 3 sha) ++ repeat ' ') ++
           " " ++ show (roundDec 3 dhla))
    [-6,-5 .. 6]
```

```txt
*Rosetta.HorSunDial> main
Latitude         -4.95
Longitude        -150.5
Legal meridian   -150.0
Sine of latitude -8.6286e-2
Diff longitude   -0.5
hour   sun hour angle   dial hour  line angle
-6.0   -89.5            84.225
-5.0   -74.5            17.283
-4.0   -59.5            8.334
-3.0   -44.5            4.847
-2.0   -29.5            2.795
-1.0   -14.5            1.278
0.0    0.5              -4.3e-2
1.0    15.5             -1.371
2.0    30.5             -2.91
3.0    45.5             -5.018
4.0    60.5             -8.671
5.0    75.5             -18.451
6.0    90.5             84.225
```


=={{header|Icon}} and {{header|Unicon}}==

```Icon
procedure main()
   PrintSundial(-4.95, -150.5, -150);
end

procedure PrintSundial(lat, lng, mer )
   write("latitude:        ", lat,
         "\nlongitude:       ", lng,
         "\nlegal meridian:  ", mer)

   slat := sin(dtor(lat))

   write("sine of latitude: ",slat,
         "\ndiff longitude:   ", lng-mer)
   write("\nHour, sun hour angle, dial hour line angle from 6am to 6pm")

   every h := -6 to 6 do {
      hraRad := dtor(hra := 15 * h - (lng - mer))
      hla :=rtod(atan(sin(hraRad)*slat, cos(hraRad)))
      write("HR=",
            right(if h <= 0 then 12+h else h,2),
            if h < 0 then "am" else "pm",
            " HRA=",hra,", HLA=",hla)
      }
end
```

```txt
latitude:        -4.95
longitude:       -150.5
legal meridian:  -150
sine of latitude: -0.08628636579792337
diff longitude:   -0.5

Hour, sun hour angle, dial hour line angle from 6am to 6pm
HR= 6am HRA=-89.5, HLA=84.22483260136025
HR= 7am HRA=-74.5, HLA=17.2829335027853
HR= 8am HRA=-59.5, HLA=8.333711921468083
HR= 9am HRA=-44.5, HLA=4.846708924373172
HR=10am HRA=-29.5, HLA=2.794873809318642
HR=11am HRA=-14.5, HLA=1.278352980919063
HR=12pm HRA=0.5, HLA=-0.04314426995813971
HR= 1pm HRA=15.5, HLA=-1.370787843187052
HR= 2pm HRA=30.5, HLA=-2.909643210076617
HR= 3pm HRA=45.5, HLA=-5.018023174356126
HR= 4pm HRA=60.5, HLA=-8.671396957302381
HR= 5pm HRA=75.5, HLA=-18.45099922256532
HR= 6pm HRA=90.5, HLA=-95.77516739863968
```


=={{header|IS-BASIC}}==
<lang IS-BASIC>100 PROGRAM "SunDial.bas"
110 OPTION ANGLE RADIANS
120 TEXT 80
130 LET DR=PI/180:LET RD=180/PI
140 INPUT PROMPT "Enter latitude:  ":LAT
150 INPUT PROMPT "Enter longitude: ":LNG
160 INPUT PROMPT "Enter legal meridian: ":REF
170 LET S=SIN(LAT*DR)
180 PRINT :PRINT "Sine of latitude :";S
190 PRINT "Diff longitude: ";LNG-REF
200 PRINT " Hour,",,"sun hour angle, dial hour line angle from 6am to 6 pm"
210 FOR H=6 TO 18
220   LET HRA=15*H-LNG+REF
230   LET HLA=ATN(S*TAN(HRA*DR))*RD
240   PRINT "HR = ";H,"HRA =";HRA,"HLA =";HLA
250 NEXT
```



## J


```j
require 'trig'
atan2=: {:@*.@j. NB. arc tangent of y divided by x

horiz=: verb define
  'lat lng ref'=. y
  out=. smoutput@,&":
  'Latitude         ' out lat
  'Longitude        ' out lng
  'Legal meridian   ' out ref
  'Sine of latitude ' out slat=. sin rfd lat
  'Diff longitude   ' out -diff=. ref - lng
  lbl=.'hour  ';'sun hour angle  ';'dial hour line angle'
  r=.((,. (,. (atan2 *&slat)/@+.@r.&.rfd)) diff + 15&*) i:6
  smoutput lbl ,: ('3.0';'7.3';'7.3') 8!:1 r
)
```

```j
   horiz _4.95 _150.5 _150
Latitude         _4.95
Longitude        _150.5
Legal meridian   _150
Sine of latitude _0.0862864
Diff longitude   _0.5
┌──────┬────────────────┬────────────────────┐
│hour  │sun hour angle  │dial hour line angle│
├──────┼────────────────┼────────────────────┤
│ -6   │-89.500         │ 84.225             │
│ -5   │-74.500         │ 17.283             │
│ -4   │-59.500         │  8.334             │
│ -3   │-44.500         │  4.847             │
│ -2   │-29.500         │  2.795             │
│ -1   │-14.500         │  1.278             │
│  0   │  0.500         │ -0.043             │
│  1   │ 15.500         │ -1.371             │
│  2   │ 30.500         │ -2.910             │
│  3   │ 45.500         │ -5.018             │
│  4   │ 60.500         │ -8.671             │
│  5   │ 75.500         │-18.451             │
│  6   │ 90.500         │-95.775             │
└──────┴────────────────┴────────────────────┘
```



## Java

{{trans|C}} (Substitutes in atan2 for the hour line angle calculation)

```java
import java.util.Scanner;

public class Sundial {
    public static void main(String[] args) {
        double lat, slat, lng, ref;
        Scanner sc = new Scanner(System.in);

        System.out.print("Enter latitude: ");
        lat = sc.nextDouble();
        System.out.print("Enter longitude: ");
        lng = sc.nextDouble();
        System.out.print("Enter legal meridian: ");
        ref = sc.nextDouble();
        System.out.println();

        slat = Math.sin(Math.toRadians(lat));
        System.out.printf("sine of latitude: %.3f\n", slat);
        System.out.printf("diff longitude: %.3f\n\n", lng - ref);

        System.out.printf("Hour, sun hour angle, dial hour line angle from 6am to 6pm\n");

        for (int h = -6; h <= 6; h++) {
            double hla, hra, hraRad;
            hra = 15.0 * h;
            hra = hra - lng + ref;
            hraRad = Math.toRadians(hra);
            hla = Math.toDegrees(Math.atan2(Math.sin(hraRad)*Math.sin(Math.toRadians(lat)), Math.cos(hraRad)));
            System.out.printf("HR= %3d;  \t  HRA=%7.3f;  \t  HLA= %7.3f\n",
                    h, hra, hla);
        }
    }
}
```

```txt
Enter latitude: -4.95
Enter longitude: -150.5
Enter legal meridian: -150

sine of latitude: -0.086
diff longitude: -0.500

Hour, sun hour angle, dial hour line angle from 6am to 6pm
HR=  -6;  	  HRA=-89.500;  	  HLA=  84.225
HR=  -5;  	  HRA=-74.500;  	  HLA=  17.283
HR=  -4;  	  HRA=-59.500;  	  HLA=   8.334
HR=  -3;  	  HRA=-44.500;  	  HLA=   4.847
HR=  -2;  	  HRA=-29.500;  	  HLA=   2.795
HR=  -1;  	  HRA=-14.500;  	  HLA=   1.278
HR=   0;  	  HRA=  0.500;  	  HLA=  -0.043
HR=   1;  	  HRA= 15.500;  	  HLA=  -1.371
HR=   2;  	  HRA= 30.500;  	  HLA=  -2.910
HR=   3;  	  HRA= 45.500;  	  HLA=  -5.018
HR=   4;  	  HRA= 60.500;  	  HLA=  -8.671
HR=   5;  	  HRA= 75.500;  	  HLA= -18.451
HR=   6;  	  HRA= 90.500;  	  HLA= -95.775
```



## Julia

```julia

print("Enter latitude       => ")
lat = parse(Float64, readline(STDIN))
print("Enter longitude      => ")
lng = parse(Float64, readline(STDIN))
print("Enter legal meridian => ")
ref = parse(Float64, readline(STDIN))
println()

slat = sin(deg2rad(lat))
@printf "    sine of latitude:   %.3f\n" slat
@printf "    diff longitude:     %.3f\n" (lng - ref)

println("\nHour, sun hour angle, dial hour line angle from 6am to 6pm\n")

for h in -6:6
  hra = 15 * h
  hra -= lng - ref
  hla = rad2deg(atan(slat * tan(deg2rad(hra))))
  @printf "HR = %3d; HRA = %7.3f; HLA = %7.3f\n" h hra hla
end

```


```txt

Enter latitude       => -4.95
Enter longitude      => -150.5
Enter legal meridian => -150

    sine of latitude:   -0.086
    diff longitude:     -0.500

Hour, sun hour angle, dial hour line angle from 6am to 6pm

HR =  -6; HRA = -89.500; HLA =  84.225
HR =  -5; HRA = -74.500; HLA =  17.283
HR =  -4; HRA = -59.500; HLA =   8.334
HR =  -3; HRA = -44.500; HLA =   4.847
HR =  -2; HRA = -29.500; HLA =   2.795
HR =  -1; HRA = -14.500; HLA =   1.278
HR =   0; HRA =   0.500; HLA =  -0.043
HR =   1; HRA =  15.500; HLA =  -1.371
HR =   2; HRA =  30.500; HLA =  -2.910
HR =   3; HRA =  45.500; HLA =  -5.018
HR =   4; HRA =  60.500; HLA =  -8.671
HR =   5; HRA =  75.500; HLA = -18.451
HR =   6; HRA =  90.500; HLA =  84.225

```



## Kotlin


```scala
import java.lang.Math.atan2
import java.lang.Math.cos
import java.lang.Math.sin
import java.lang.Math.toDegrees
import java.lang.Math.toRadians

// version 1.1.4

fun main(args: Array<String>) {
    println("Please enter the following in degrees:")
    print("  Latitude       : ")
    val lat = readLine()!!.toDouble()
    print("  Longitude      : ")
    val lng = readLine()!!.toDouble()
    print("  Legal Meridian : ")
    val mer = readLine()!!.toDouble()

    val slat = sin(toRadians(lat))
    val diff = lng - mer
    println("\nSine of latitude     = ${"%.6f".format(slat)}")
    println("Longitude - Meridian = ${"%.3f".format(diff)}\n")
    println("Hour   Sun Hour Angle  Dial Hour Line Angle")
    println("-----  --------------  --------------------")
    println("              °               °")
    for (h in -6..6) {
        var hr = h + 12
        val am = if (hr < 12) "AM" else "PM"
        if (hr > 12) hr -= 12
        val sha = 15.0 * h - diff
        val dhla = toDegrees(atan2(slat * sin(toRadians(sha)), cos(toRadians(sha))))
        println("%2d %s      %+7.3f         %+7.3f".format(hr, am, sha, dhla))
    }
}
```

Sample input/output:
```txt

Please enter the following in degrees:
  Latitude       : -4.95
  Longitude      : -150.5
  Legal Meridian : -150

Sine of latitude     = -0.086286
Longitude - Meridian = -0.500

Hour   Sun Hour Angle  Dial Hour Line Angle
-----  --------------  --------------------
              °               °
 6 AM      -89.500         +84.225
 7 AM      -74.500         +17.283
 8 AM      -59.500          +8.334
 9 AM      -44.500          +4.847
10 AM      -29.500          +2.795
11 AM      -14.500          +1.278
12 PM       +0.500          -0.043
 1 PM      +15.500          -1.371
 2 PM      +30.500          -2.910
 3 PM      +45.500          -5.018
 4 PM      +60.500          -8.671
 5 PM      +75.500         -18.451
 6 PM      +90.500         -95.775

```



## Liberty BASIC

Based on Algol & BBC BASIC versions. Note Liberty BASIC works in radians.

```lb
global pi
pi = 3.14159265
input "Enter latitude  (degrees)     : "; latitude      '     -4.95
input "Enter longitude (degrees)     : "; longitude     '   -150.5
input "Enter legal meridian (degrees): "; meridian      '   -150.0
print
print "Time       Sun hour angle   Dial hour line angle"
for hour = 6 TO 18
    hra = 15 * hour - longitude + meridian - 180
    hla = rad2deg(atn(sin(deg2rad(latitude)) * tan(deg2rad(hra))))
    if abs(hra) > 90 then hla = hla + 180 * sgn(hra * latitude)
    print using( "##.##", hour), using("####.###  ", hra), using("####.###", hla)
next hour
end

function rad2deg(theta)
    rad2deg = theta * 180 / pi
end function

function deg2rad(theta)
    deg2rad = theta * pi / 180
end function

function sgn(x)
    if x > 0 then sgn = 1 else sgn = -1
end function
```

```txt

Enter latitude  (degrees)     : -4.95
Enter longitude (degrees)     : -150.5
Enter legal meridian (degrees): -150.0

Time       Sun hour angle   Dial hour line angle
 6.00          -89.500        84.225
 7.00          -74.500        17.283
 8.00          -59.500         8.334
 9.00          -44.500         4.847
10.00          -29.500         2.795
11.00          -14.500         1.278
12.00            0.500        -0.043
13.00           15.500        -1.371
14.00           30.500        -2.910
15.00           45.500        -5.018
16.00           60.500        -8.671
17.00           75.500       -18.451
18.00           90.500       -95.775

```



## LiveCode

Translation of BASIC versions.

```LiveCode
on mouseUp
    ask "Enter lng,lat,meridian"
    if it is empty then exit mouseup
    // -150.5, -4.95, -150.0
    put item 1 of it into longitude
    put item 2 of it into latitude
    put item 3 of it into meridian

    repeat with hour = 6 TO 18
        put 15 *hour - longitude + meridian - 180 into hra
        put rad2deg(atan(sin(deg2rad(latitude)) * tan(deg2rad(hra)))) into hla
        if abs(hra) > 90 then put hla + 180 * sgn(hra *latitude) into hla
        put hour && hra && hla & cr after sunhours
    end repeat
    put sunhours
end mouseUp

function rad2deg theta
    return theta * (180 / pi)
end rad2deg

function deg2rad theta
    return theta * (pi / 180)
end deg2rad

function sgn x
    if x >0 then return 1 else return -1
end sgn
```




## Logo


```logo
type "|Enter latitude: |
make "lat readword
type "|Enter longitude: |
make "long readword
type "|Enter legal meridian: |
make "long :long - readword

print [Hour : HourAngle , DialAngle]
for [hour -6 6] [
   make "hra 15 * :hour - :long
   make "hla arctan product sin :lat quotient sin :hra cos :hra
   print (sentence "hour :hour ": :hra ", :hla)
]
```



## Microsoft Small Basic

```microsoftsmallbasic

TextWindow.Write("Enter latitude       => ")
lat = TextWindow.ReadNumber()
TextWindow.Write("Enter longitude      => ")
lng = TextWindow.ReadNumber()
TextWindow.Write("Enter legal meridian => ")
ref = TextWindow.ReadNumber()
sLat = Math.Sin(Math.GetRadians(lat))
TextWindow.WriteLine("")
TextWindow.Write("    sine of latitude:   ")
TextWindow.WriteLine(Math.Round(sLat * 10000) / 10000)
TextWindow.Write("    diff longitude:     ")
TextWindow.WriteLine(lng - ref)
TextWindow.WriteLine("")
TextWindow.WriteLine("Hour, sun hour angle, dial hour line angle from 6am to 6pm")
For hour = -6 To 6
  hourAngle = 15 * hour
  hourAngle = hourAngle - (lng - ref) ' correct for longitude difference
  hourLineAngle = math.GetDegrees(Math.ArcTan(sLat * Math.Tan(Math.GetRadians(hourAngle))))
  TextWindow.Write("HR=")
  TextWindow.CursorLeft = 3 + (3 - Text.GetLength(hour))
  TextWindow.Write(hour)
  TextWindow.Write("; HRA=")
  TextWindow.CursorLeft = 12 + (6 - Text.GetLength(hourAngle))
  TextWindow.Write(hourAngle)
  TextWindow.Write("; HLA=")
  TextWindow.CursorLeft = 24 + (4 - Text.GetLength(Math.Floor(hourLineAngle)))
  TextWindow.Write(Math.Round(hourLineAngle * 1000) / 1000)
  TextWindow.WriteLine("")
EndFor

```

 Enter latitude       => -4.95
 Enter longitude      => -150.5
 Enter legal meridian => -150

     sine of latitude:   -0.0863
     diff longitude:     -0.5

 Hour, sun hour angle, dial hour line angle from 6am to 6pm
 HR= -6; HRA= -89.5; HLA=  84.225
 HR= -5; HRA= -74.5; HLA=  17.283
 HR= -4; HRA= -59.5; HLA=   8.334
 HR= -3; HRA= -44.5; HLA=   4.847
 HR= -2; HRA= -29.5; HLA=   2.795
 HR= -1; HRA= -14.5; HLA=   1.278
 HR=  0; HRA=   0.5; HLA=  -0.043
 HR=  1; HRA=  15.5; HLA=  -1.371
 HR=  2; HRA=  30.5; HLA=  -2.91
 HR=  3; HRA=  45.5; HLA=  -5.018
 HR=  4; HRA=  60.5; HLA=  -8.671
 HR=  5; HRA=  75.5; HLA= -18.451
 HR=  6; HRA=  90.5; HLA=  84.225

=={{header|MK-61/52}}==
<lang>МГ	П2	->	МГ	П1	->	МГ	sin	П0
6	/-/	П3
ИП3	1	5	*	ИП1	ИП2	-	-	П4
tg	ИП0	*	arctg	ИП4	ИП3	С/П
ИП3	1	+	П3	7	-	x=0	12
Сx	С/П
```


''Input'': ''latitude'' ^ ''longitude'' ^ ''legal meridian'' С/П ... С/П ...; switch of the angle measure set to ''Г''.

''Example'': -4,57 ^ -150,3 ^ -150 С/П.

''Output'': hour in ''РX'', sun hour angle in ''РY'', dial hour line angle in ''РZ''.

=={{header|Modula-2}}==
```modula2

MODULE SunDial;

FROM STextIO IMPORT
  WriteString, WriteLn, SkipLine;
FROM SRealIO IMPORT
  ReadReal, WriteFixed, WriteFloat;
FROM SWholeIO IMPORT
  WriteInt;
FROM RealMath IMPORT
  sin, pi, arctan, tan;

VAR
  Lat, Slat, Lng, Ref: REAL;
  Hour: INTEGER;
  HourAngle, HourLineAngle: REAL;
BEGIN
  WriteString("Enter latitude       => ");
  ReadReal(Lat);
  SkipLine;
  WriteString("Enter longitude      => ");
  ReadReal(Lng);
  SkipLine;
  WriteString("Enter legal meridian => ");
  ReadReal(Ref);
  SkipLine;
  WriteLn;
  Slat := sin(Lat * pi / 180.0);
  WriteString("    sine of latitude:   ");
  WriteFloat(Slat, 2, 8);
  WriteLn;
  WriteString("    diff longitude:     ");
  WriteFixed(Lng - Ref, 3, 1);
  WriteLn;
  WriteLn;
  WriteString("Hour, sun hour angle, dial hour line angle from 6am to 6pm");
  WriteLn;
  FOR Hour := -6 TO 6 DO
    HourAngle := FLOAT(15 * Hour);
    HourAngle := HourAngle - (Lng - Ref); (* correct for longitude difference *)
    HourLineAngle := arctan(Slat * tan(HourAngle * pi / 180.0)) * 180.0 / pi;
    WriteString("HR=");
    WriteInt(Hour, 3);
    WriteString("; HRA=");
    WriteFixed(HourAngle, 3, 8);
    WriteString("; HLA=");
    WriteFixed(HourLineAngle, 3, 8);
    WriteLn;
  END;
END SunDial.

```

 Enter latitude       => -4.95
 Enter longitude      => -150.5
 Enter legal meridian => -150

     sine of latitude:   -8.6E-02
     diff longitude:     -0.500

 Hour, sun hour angle, dial hour line angle from 6am to 6pm
 HR= -6; HRA= -89.500; HLA=  84.225
 HR= -5; HRA= -74.500; HLA=  17.283
 HR= -4; HRA= -59.500; HLA=   8.334
 HR= -3; HRA= -44.500; HLA=   4.847
 HR= -2; HRA= -29.500; HLA=   2.795
 HR= -1; HRA= -14.500; HLA=   1.278
 HR=  0; HRA=   0.500; HLA=  -0.043
 HR=  1; HRA=  15.500; HLA=  -1.371
 HR=  2; HRA=  30.500; HLA=  -2.910
 HR=  3; HRA=  45.500; HLA=  -5.018
 HR=  4; HRA=  60.500; HLA=  -8.671
 HR=  5; HRA=  75.500; HLA= -18.451
 HR=  6; HRA=  90.500; HLA=  84.225


## Nim

```nim
import rdstdin, strutils, math, strfmt

proc radians(x): float = x * Pi / 180
proc degrees(x): float = x * 180 / Pi

let lat = parseFloat readLineFromStdin "Enter latitude       => "
let lng = parseFloat readLineFromStdin "Enter longitude      => "
let med = parseFloat readLineFromStdin "Enter legal meridian => "
echo ""

let slat = sin radians lat
echo "    sine of latitude:   {:.3f}".fmt(slat)
echo "    diff longitude:     {:.3f}".fmt(lng-med)
echo ""
echo "Hour, sun hour angle, dial hour line angle from 6am to 6pm"

for h in -6..6:
  let hra = float(15 * h) - lng + med
  let hla = degrees arctan(slat * tan radians hra)
  echo "HR={:3d}; HRA={:7.3f}; HLA={:7.3f}".fmt(h, hra, hla)
```

Output:

```txt
Enter latitude       => -4.95
Enter longitude      => -150.5
Enter legal meridian => -150

    sine of latitude:   -0.086
    diff longitude:     -0.500

Hour, sun hour angle, dial hour line angle from 6am to 6pm
HR= -6; HRA=-89.500; HLA= 84.225
HR= -5; HRA=-74.500; HLA= 17.283
HR= -4; HRA=-59.500; HLA=  8.334
HR= -3; HRA=-44.500; HLA=  4.847
HR= -2; HRA=-29.500; HLA=  2.795
HR= -1; HRA=-14.500; HLA=  1.278
HR=  0; HRA=  0.500; HLA= -0.043
HR=  1; HRA= 15.500; HLA= -1.371
HR=  2; HRA= 30.500; HLA= -2.910
HR=  3; HRA= 45.500; HLA= -5.018
HR=  4; HRA= 60.500; HLA= -8.671
HR=  5; HRA= 75.500; HLA=-18.451
HR=  6; HRA= 90.500; HLA= 84.225
```



## Objeck

```objeck

class Sundial {
  function : Main(args : String[]) ~ Nil {
    "Enter latitude: "->Print();
    lat := System.IO.Console->ReadString()->ToFloat();
    "Enter longitude: "->Print();
    lng := System.IO.Console->ReadString()->ToFloat();
    "Enter legal meridian: "->Print();
    ref := System.IO.Console->ReadString()->ToFloat();
    '\n'->PrintLine();

    slat := lat->ToRadians()->Sin();
    "sine of latitude: {$slat}"->PrintLine();
    value := lng - ref;
    "diff longitude: {$value}"->PrintLine();
    '\n'->PrintLine();

    "Hour\t\tsun hour angle\t\tdial hour line angle from 6am to 6pm"->PrintLine();
    for (h := -6; h <= 6; h+=1;) {
      hra := 15.0 * h;
      hra -= lng - ref;
      hla := (slat* (hra*2*Float->Pi()/360.0)->Tan())->ArcTan() * 360.0 / (2*Float->Pi());
      "HR={$h}\t\tHRA={$hra}\t\tHLA={$hla}"->PrintLine();
    };
  }
}

```



## OCaml

```ocaml
let () =
  let pi = 4. *. atan 1. in
  print_endline "Enter latitude		=> ";
  let lat = read_float () in
  print_endline "Enter longitude	=> ";
  let lng = read_float () in
  print_endline "Enter legal meridian	=> ";
  let ref = read_float () in
  print_newline ();

  let slat = sin (lat *. 2. *. pi /. 360.) in
  Printf.printf "    sine of latitude:   %.3f\n" slat;
  Printf.printf "    diff longitude:     %.3f\n" (lng -. ref);
  print_newline ();

  print_endline "Hour, sun hour angle, dial hour line angle from 6am to 6pm";

  for h = -6 to 6 do
    let hra = 15. *. float h in
    let hra = hra -. (lng -. ref) in
    let hla = atan (slat *. tan (hra *. 2. *. pi /. 360.)) *. 360. /. (2. *. pi) in
    Printf.printf "HR= %3d;  \t  HRA=%7.3f;  \t  HLA= %7.3f\n" h hra hla;
  done

```

```txt

Enter latitude		=>
-4.95
Enter longitude	=>
-150.5
Enter legal meridian	=>
-150.

    sine of latitude:   -0.086
    diff longitude:     -0.500

Hour, sun hour angle, dial hour line angle from 6am to 6pm
HR=  -6;  	  HRA=-89.500;  	  HLA=  84.225
HR=  -5;  	  HRA=-74.500;  	  HLA=  17.283
HR=  -4;  	  HRA=-59.500;  	  HLA=   8.334
HR=  -3;  	  HRA=-44.500;  	  HLA=   4.847
HR=  -2;  	  HRA=-29.500;  	  HLA=   2.795
HR=  -1;  	  HRA=-14.500;  	  HLA=   1.278
HR=   0;  	  HRA=  0.500;  	  HLA=  -0.043
HR=   1;  	  HRA= 15.500;  	  HLA=  -1.371
HR=   2;  	  HRA= 30.500;  	  HLA=  -2.910
HR=   3;  	  HRA= 45.500;  	  HLA=  -5.018
HR=   4;  	  HRA= 60.500;  	  HLA=  -8.671
HR=   5;  	  HRA= 75.500;  	  HLA= -18.451
HR=   6;  	  HRA= 90.500;  	  HLA=  84.225

```



## Octave


```octave
lat = input("Enter latitude: ");
lng = input("Enter longitude: ");
ref = input("Enter legal meridian: ");
slat = sind(lat);
printf("sine of latitude: %.3f\n", slat);
printf("diff longitude: %.3f\n\n", lng - ref);
printf("Hour, sun hour angle, dial hour line angle from 6am to 6pm\n");

hras = [-6:6] .* 15.0 .- lng .+ ref;
hlas = atand( tand(hras) .* slat );
printf("HR= %3d;  \t  HRA=%7.3f;  \t  HLA= %7.3f\n",
       [ [-6:6]; hras; hlas] );
```



## OoRexx

```oorexx
/*REXX pgm shows:  hour,  sun hour angle,  dial hour line angle,  6am ---> 6pm*/
/* Use trigonometric functions provided by rxCalc                             */
parse arg lat lng mer .               /*get the optional arguments from the CL*/
                          /*None specified?   Then use the default of Jules   */
                          /*Verne's Lincoln Island,  aka  Ernest Legouve Reef.*/

if lat=='' | lat==',' then lat=-4.95  /*Not specified?   Then use the default.*/
if lng=='' | lng==',' then lng=-150.5 /* "      "          "   "   "     "    */
if mer=='' | mer==',' then mer=-150   /* "      "          "   "   "     "    */
L=max(length(lat), length(lng), length(mer))
say '       latitude:' right(lat,L)
say '      longitude:' right(lng,L)
say ' legal meridian:' right(mer,L)
sineLat=rxCalcSin(lat,,'D')
w1=max(length('hour')     ,length('midnight'))+2
w2=max(length('sun hour') ,length('angle'))+2
w3=max(length('dial hour'),length('line angle'))+2
indent=left('',30)               /*make the presentation prettier.      */
say indent center('    ',w1) center('sun hour',w2) center('dial hour' ,w3)
say indent center('hour',w1) center('angle'   ,w2) center('line angle',w3)
call sep                         /*add a separator line for the eyeballs*/

do h=-6  to 6                    /*Okey dokey then, let's get busy.     */
  select
    when abs(h)==12  then hc='midnight'      /*above the arctic circle? */
    when h<0  then hc=-h 'am'    /*convert the hour for human beans.    */
    when h==0 then hc='noon'     /*  ... easier to understand now.      */
    when h>0  then hc=h 'pm'     /*  ... even more meaningful.          */
    end   /*select*/
  hra=15*h-lng+mer
  hla=rxCalcArctan(sineLat*rxCalctan(hra,,'D'),,'D')
  say indent center(hc,w1) right(format(hra,,1),w2) right(format(hla,,1),w3)
  end
call sep
Exit
sep: say indent copies('-',w1) copies('-',w2) copies('-',w3)
     Return
::Requires rxMath Library
```

Same as for REXX.


## Pascal


```pascal
Program SunDial;

Const
   pi  = 3.14159265358979323846;
   dr  = pi/180.0;
   rd  = 180.0/pi;
   tab =  chr(9);

Var
   lat, slat, lng, ref : Real;
   hla, hra	       : Real;
   h		       : Integer;

function tan(val : Real) : Real;
begin
   tan := sin(val)/cos(val)
end;

Begin
   Write('Enter latitude: '); Read(lat);
   Write('Enter longitude: '); Read(lng);
   Write('Enter legal meridian: '); Read(ref);
   WriteLn;
   slat := sin(lat * dr);
   WriteLn('sine of latitude: ', slat);
   WriteLn('diff longitude: ', lng - ref);
   WriteLn('Hour, sun hour angle, dial hour line angle from 6am to 6pm');
   for h := -6 to 6 do begin
      hra := 15.0 * h;
      hra := hra - lng + ref;
      hla := arctan(slat * tan(hra * dr)) * rd;
      WriteLn('HR= ', h:3, ';  ',
	      tab, '  HRA= ', hra:7:3, ';  ',
	      tab, '  HLA= ', hla:7:3)
   end
end.
```



## Perl

```perl
use utf8;
binmode STDOUT, ":utf8";

use constant π => 3.14159265;

sub d2r { $_[0] * π / 180 } # degrees to radians
sub r2d { $_[0] * 180 / π } # radians to degrees

print 'Enter latitude       => '; $latitude  = <>;
print 'Enter longitude      => '; $longitude = <>;
print 'Enter legal meridian => '; $meridian  = <>;

$lat_sin = sin( d2r($latitude) );
$offset = $meridian - $longitude;
print 'Sine of latitude: ' . sprintf "%.4f\n", $lat_sin;
print 'Longitude offset: ' . $offset . "\n";
print '=' x 48 . "\n";
print " Hour : Sun hour angle°: Dial hour line angle°\n";

for $hour (-6 .. 6) {
    my $sun_deg  = $hour * 15 + $offset;
    my $line_deg = r2d atan2( ( sin(d2r($sun_deg)) * $lat_sin ), cos(d2r($sun_deg)) );
    printf "%2d %s     %8.3f            %8.3f\n",
    ($hour + 12) % 12 || 12, ($hour < 0 ? 'AM' : 'PM'), $sun_deg, $line_deg;
}
```

```txt
Enter latitude       => -4.95
Enter longitude      => -150.5
Enter legal meridian => -150

Sine of latitude: -0.0863
Longitude offset: 0.5

### ==========================================

 Hour : Sun hour angle°: Dial hour line angle°
 6 AM      -89.500              84.225
 7 AM      -74.500              17.283
 8 AM      -59.500               8.334
 9 AM      -44.500               4.847
10 AM      -29.500               2.795
11 AM      -14.500               1.278
12 PM        0.500              -0.043
 1 PM       15.500              -1.371
 2 PM       30.500              -2.910
 3 PM       45.500              -5.018
 4 PM       60.500              -8.671
 5 PM       75.500             -18.451
 6 PM       90.500             -95.775
```



## Perl 6


```perl6
sub postfix:<°> ($a) { $a * pi / 180 } # degrees to radians
sub postfix:<®> ($a) { $a * 180 / pi } # radians to degrees

my $latitude  = prompt 'Enter latitude       => ';
my $longitude = prompt 'Enter longitude      => ';
my $meridian  = prompt 'Enter legal meridian => ';

my $lat_sin = sin( $latitude° );
say 'Sine of latitude: ', $lat_sin.fmt("%.4f");
say 'Longitude offset: ', my $offset = $meridian - $longitude;
say '=' x 48;
say ' Hour  : Sun hour angle° : Dial hour line angle°';

for -6 .. 6 -> $hour {
    my $sun_deg  = $hour * 15 + $offset;
    my $line_deg = atan2( ( sin($sun_deg°) * $lat_sin ), cos($sun_deg°) )®;
    printf "%2d %s      %7.3f             %7.3f\n",
    ($hour + 12) % 12 || 12, ($hour < 0 ?? 'AM' !! 'PM'), $sun_deg, $line_deg;
}
```

```txt

Enter latitude       => -4.95
Enter longitude      => -150.5
Enter legal meridian => -150
Sine of latitude: -0.0863
Longitude offset: 0.5

### ==========================================

 Hour  : Sun hour angle° : Dial hour line angle°
 6 AM      -89.500              84.225
 7 AM      -74.500              17.283
 8 AM      -59.500               8.334
 9 AM      -44.500               4.847
10 AM      -29.500               2.795
11 AM      -14.500               1.278
12 PM        0.500              -0.043
 1 PM       15.500              -1.371
 2 PM       30.500              -2.910
 3 PM       45.500              -5.018
 4 PM       60.500              -8.671
 5 PM       75.500             -18.451
 6 PM       90.500             -95.775

```



## Phix

Copy of [[Horizontal_sundial_calculations#Euphoria|Euphoria]]

```Phix
atom lat = prompt_number("Enter Latitude: ",{})
atom lng = prompt_number("Enter Longitude: ",{})
atom lm = prompt_number("Enter Legal Meridian: ",{})
puts(1,'\n')

atom ha, hla

function Deg2Rad(atom degrees)
    return degrees * PI / 180
end function

function Rad2Deg(atom radians)
    return radians * 180 / PI
end function

function atan2(atom y, atom x)
    return 2*arctan((sqrt(power(x,2)+power(y,2)) - x)/y)
end function

atom s_lat = sin(Deg2Rad(lat))

puts(1,"Hour,  Sun Hour Angle, Dial Hour Line Angle\n")

for hour = -6 to 6 do
    ha = hour * 15 - lng + lm
    atom s = sin(Deg2Rad(ha))
    atom c = cos(Deg2Rad(ha))
    hla = Rad2Deg(atan2(s_lat*s,c))
    printf(1,"%3d       %7.3f          %7.3f\n",{hour+12,ha,hla})
end for

{} = wait_key()
```

```txt

Enter Latitude: -4.95
Enter Longitude: -150.5
Enter Legal Meridian: -150

Hour,  Sun Hour Angle, Dial Hour Line Angle
  6       -89.500           84.225
  7       -74.500           17.283
  8       -59.500            8.334
  9       -44.500            4.847
 10       -29.500            2.795
 11       -14.500            1.278
 12         0.500           -0.043
 13        15.500           -1.371
 14        30.500           -2.910
 15        45.500           -5.018
 16        60.500           -8.671
 17        75.500          -18.451
 18        90.500          -95.775

```



## PicoLisp

```PicoLisp
(load "@lib/math.l")

(de prompt (Str . Arg)
   (prin Str " => ")
   (set (car Arg) (in NIL (read))) )

(use (Lat Lng Ref)
   (prompt "Enter latitude      " Lat)
   (prompt "Enter longitude     " Lng)
   (prompt "Enter legal meridian" Ref)
   (prinl)
   (let Slat (sin (*/ Lat pi 180.0))
      (prinl "    sine of latitude:   " (round Slat))
      (prinl "    diff longitude:     " (round (- Lng Ref)))
      (prinl)
      (prinl "Hour, sun hour angle, dial hour line angle from 6am to 6pm")
      (for H (range -6 6)
         (let Hra (- (* 15.0 H) (- Lng Ref))
            (let Hla (*/ (atan (*/ Slat (tan (*/ Hra pi 180.0)) 1.0)) 180.0 pi)
               (prinl
                  "HR="
                  (align 3 H)
                  "; HRA="
                  (align 8 (round Hra))
                  "; HLA="
                  (align 8 (round Hla)) ) ) ) ) ) )
```

```txt
Enter latitude       => -4.95
Enter longitude      => -150.5
Enter legal meridian => -150.          # Don't omit the '.' here

    sine of latitude:   -0.086
    diff longitude:     -0.500

Hour, sun hour angle, dial hour line angle from 6am to 6pm
HR= -6; HRA= -89.500; HLA=  84.225
HR= -5; HRA= -74.500; HLA=  17.283
HR= -4; HRA= -59.500; HLA=   8.334
HR= -3; HRA= -44.500; HLA=   4.847
HR= -2; HRA= -29.500; HLA=   2.795
HR= -1; HRA= -14.500; HLA=   1.278
HR=  0; HRA=   0.500; HLA=  -0.043
HR=  1; HRA=  15.500; HLA=  -1.371
HR=  2; HRA=  30.500; HLA=  -2.910
HR=  3; HRA=  45.500; HLA=  -5.018
HR=  4; HRA=  60.500; HLA=  -8.671
HR=  5; HRA=  75.500; HLA= -18.451
HR=  6; HRA=  90.500; HLA=  84.225
```



## PowerShell


```PowerShell

function Get-Sundial
{
    [CmdletBinding()]
    [OutputType([PSCustomObject])]
    Param
    (
        [Parameter(Mandatory=$true)]
        [ValidateRange(-90,90)]
        [double]
        $Latitude,


        [Parameter(Mandatory=$true)]
        [ValidateRange(-180,180)]
        [double]
        $Longitude,


        [Parameter(Mandatory=$true)]
        [ValidateRange(-180,180)]
        [double]
        $Meridian
    )

    [double]$sinLat = [Math]::Sin($Latitude*2*[Math]::PI/360)

    $object = [PSCustomObject]@{
        "Sine of Latitude"     = [Math]::Round($sinLat,3)
        "Longitude Difference" = $Longitude - $Meridian
    }

    [int[]]$hours = -6..6

    $hoursArray = foreach ($hour in $hours)
    {
        [double]$hra = (15 * $hour) - ($Longitude - $Meridian)
        [double]$hla = [Math]::Atan($sinLat*[Math]::Tan($hra*2*[Math]::PI/360))*360/(2*[Math]::PI)
        [PSCustomObject]@{
            "Hour"                 = "{0,8}" -f ((Get-Date -Hour ($hour + 12) -Minute 0).ToString("t"))
            "Sun Hour Angle"       = [Math]::Round($hra,3)
            "Dial Hour Line Angle" = [Math]::Round($hla,3)
        }
    }

    $object | Add-Member -MemberType NoteProperty -Name Hours -Value $hoursArray -PassThru
}

$sundial = Get-Sundial -Latitude -4.95 -Longitude -150.5 -Meridian -150
$sundial | Select-Object -Property "Sine of Latitude", "Longitude Difference" | Format-List
$sundial.Hours | Format-Table -AutoSize

```

```txt


Sine of Latitude     : -0.086
Longitude Difference : -0.5




Hour     Sun Hour Angle Dial Hour Line Angle
----     -------------- --------------------
 6:00 AM          -89.5               84.225
 7:00 AM          -74.5               17.283
 8:00 AM          -59.5                8.334
 9:00 AM          -44.5                4.847
10:00 AM          -29.5                2.795
11:00 AM          -14.5                1.278
12:00 PM            0.5               -0.043
 1:00 PM           15.5               -1.371
 2:00 PM           30.5                -2.91
 3:00 PM           45.5               -5.018
 4:00 PM           60.5               -8.671
 5:00 PM           75.5              -18.451
 6:00 PM           90.5               84.225


```



## PureBasic

```PureBasic
If OpenConsole()
  Define.f lat, slat, lng, ref
  Define.i h
  Print("Enter latitude       => "): lat=ValF(Input())
  Print("Enter longitude      => "): lng=ValF(Input())
  Print("Enter legal meridian => "): ref=ValF(Input())
  PrintN("")

  slat=Sin(lat*2*#PI/360)
  PrintN("    sine of latitude:   "+StrF(slat,3))
  PrintN("    diff longitude:     "+StrF((lng-ref),3)+#CRLF$)
  PrintN("Hour, sun hour angle, dial hour line angle from 6am to 6pm")

  For h=-6 To 6
    Define.f hra, hla
    hra=15*h
    hra=hra-(lng-ref)
    hla=ATan(slat*Tan(hra*2*#PI/360))*360/(2*#PI)
    PrintN("HR="+RSet(Str(h),3)+"; HRA="+RSet(StrF(hra,3),7)+"; HLA="+RSet(StrF(hla,3),7))
  Next

EndIf
```

```txt
Enter latitude       => -4.95
Enter longitude      => -150.5
Enter legal meridian => -150

    sine of latitude:   -0.086
    diff longitude:     -0.500

Hour, sun hour angle, dial hour line angle from 6am to 6pm
HR= -6; HRA=-89.500; HLA= 84.225
HR= -5; HRA=-74.500; HLA= 17.283
HR= -4; HRA=-59.500; HLA=  8.334
HR= -3; HRA=-44.500; HLA=  4.847
HR= -2; HRA=-29.500; HLA=  2.795
HR= -1; HRA=-14.500; HLA=  1.278
HR=  0; HRA=  0.500; HLA= -0.043
HR=  1; HRA= 15.500; HLA= -1.371
HR=  2; HRA= 30.500; HLA= -2.910
HR=  3; HRA= 45.500; HLA= -5.018
HR=  4; HRA= 60.500; HLA= -8.671
HR=  5; HRA= 75.500; HLA=-18.451
HR=  6; HRA= 90.500; HLA= 84.225

```



## Python

```python
from __future__ import print_function
import math
try: raw_input
except: raw_input = input

lat = float(raw_input("Enter latitude       => "))
lng = float(raw_input("Enter longitude      => "))
ref = float(raw_input("Enter legal meridian => "))
print()

slat = math.sin(math.radians(lat))
print("    sine of latitude:   %.3f" % slat)
print("    diff longitude:     %.3f" % (lng-ref))
print()
print("Hour, sun hour angle, dial hour line angle from 6am to 6pm")

for h in range(-6, 7):
  hra = 15 * h
  hra -= lng - ref
  hla = math.degrees(math.atan(slat * math.tan(math.radians(hra))))
  print("HR=%3d; HRA=%7.3f; HLA=%7.3f" % (h, hra, hla))
```

```txt

Enter latitude       => -4.95
Enter longitude      => -150.5
Enter legal meridian => -150

    sine of latitude:   -0.086
    diff longitude:     -0.500

Hour, sun hour angle, dial hour line angle from 6am to 6pm
HR= -6; HRA=-89.500; HLA= 84.225
HR= -5; HRA=-74.500; HLA= 17.283
HR= -4; HRA=-59.500; HLA=  8.334
HR= -3; HRA=-44.500; HLA=  4.847
HR= -2; HRA=-29.500; HLA=  2.795
HR= -1; HRA=-14.500; HLA=  1.278
HR=  0; HRA=  0.500; HLA= -0.043
HR=  1; HRA= 15.500; HLA= -1.371
HR=  2; HRA= 30.500; HLA= -2.910
HR=  3; HRA= 45.500; HLA= -5.018
HR=  4; HRA= 60.500; HLA= -8.671
HR=  5; HRA= 75.500; HLA=-18.451
HR=  6; HRA= 90.500; HLA= 84.225

```



## Racket

I must say, I'm a bit astonished by the fact that no one is bothered by the atan problem (HLA=84.225) that appears in the ALGOL 68 solution and all of the ones derived from it. Composing tan & atan produces the identity only in the range [-90,90], and you have to correct for angles outside of this.

Also, I apologize for the length; I added quite a bit of commenting, and I peeled things out into functions so I could test them. Hopefully, the result--though longer--is also more readable.


```Racket
#lang racket

;; print the table for a given latitude and longitude-offset,
;; given in degrees
(define (print-table lat long-offset)
  ;; print the table header
  (display
   (~a "    sine of latitude: "
       (~r (sin (deg->rad lat)) #:precision '(= 3))
       "\n"
       "    diff longitude:   "
       (~r long-offset #:precision '(= 3))
       "\n\nHour, sun hour angle, dial hour line angle "
       "from 6am to 6pm\n"))
  ;; print the table
  (for ([h (in-range -6 7)])
    (define hra (- (* 15 h) long-offset))
    (define hla (to-hla lat hra))
    (display (~a "HR="(pad-to 3 (~a h))"; "
                 "HRA="(pad-to 7 (~r hra #:precision '(= 3)))"; "
                 "HLA="(pad-to 7 (~r hla #:precision '(= 3)))"\n"))))


;; compute the angle on the gnomon corresponding to a
;; given angle of the sun (angles given and returned in degrees)
(define (to-hla lat ang)
  (define lat-sign (cond [(< lat 0) -1] [else 1]))
  ;; move to the right quadrant for
  ;; angles outside [-90,90]
  (define correction (* (cond [(< ang -90) -180]
                              [(> ang 90) 180]
                              [else 0])
                        lat-sign))
  (+ (rad->deg (atan (* (sin (deg->rad lat))
                        (tan (deg->rad ang)))))
     correction))

;; write the prompt, return the entered number
(define (prompt->num p)
  (printf "~a" p)
  (string->number (read-line)))

;; translate degrees to radians
(define (deg->rad d) (* 2 pi (/ d 360)))

;; translate radians to degrees
(define (rad->deg r) (* 360 (/ r (* 2 pi))))

;; add spaces to reach given length
(define (pad-to cols str)
  (define spaces-needed (max 0 (- cols (string-length str))))
  (string-append
   (list->string (for/list ([i spaces-needed]) #\space))
   str))


;; INPUT PARAMETERS, PRINT TABLE:
(define lat (prompt->num "Enter latitude       => "))
(define lng (prompt->num "Enter longitude      => "))
(define ref (prompt->num "Enter legal meridian => "))

(print-table lat (- lng ref))

;; test cases for angle conversion
(require rackunit)
(check < (to-hla 30 89) 90)
(check-= (to-hla 30 90) 90 1e-5)
(check > (to-hla 30 91) 90)
(check > (to-hla 30 -89) -90)
(check-= (to-hla 30 90) 90 1e-5)
(check < (to-hla 30 -91) -90)
(check < (to-hla -30 -89) 90)
(check-= (to-hla -30 -90) 90 1e-5)
(check > (to-hla -30 -91) 90)
(check > (to-hla -30 89) -90)
(check-= (to-hla -30 90) -90 1e-5)
(check < (to-hla -30 91) -90)

```

```txt
Welcome to DrRacket, version 5.3.3.5--2013-02-20(5eddac74/d) [3m].
Language: racket; memory limit: 512 MB.
Enter latitude       => -4.95
Enter longitude      => -150.5
Enter legal meridian => -150
    sine of latitude: -0.086
    diff longitude:   -0.500

Hour, sun hour angle, dial hour line angle from 6am to 6pm
HR= -6; HRA=-89.500; HLA= 84.225
HR= -5; HRA=-74.500; HLA= 17.283
HR= -4; HRA=-59.500; HLA=  8.334
HR= -3; HRA=-44.500; HLA=  4.847
HR= -2; HRA=-29.500; HLA=  2.795
HR= -1; HRA=-14.500; HLA=  1.278
HR=  0; HRA=  0.500; HLA= -0.043
HR=  1; HRA= 15.500; HLA= -1.371
HR=  2; HRA= 30.500; HLA= -2.910
HR=  3; HRA= 45.500; HLA= -5.018
HR=  4; HRA= 60.500; HLA= -8.671
HR=  5; HRA= 75.500; HLA=-18.451
HR=  6; HRA= 90.500; HLA=-95.775
>

```



## REXX

The REXX language doesn't have the usual trigonometric functions, nor for that matter, a   '''sqrt'''   (square root) function,

so these as well as   '''pi'''   were added to this program.

The   ''legal meridian''   is calculated instead of relying on a specified amount.

No attempt was made to explain the inner workings of the trigonometric functions.

```rexx
/*REXX program displays:   hour,  sun hour angle,  dial hour line angle,  6am ───► 6pm. */
numeric digits 60                                /*in case sundial is in polar regions. */
parse arg lat lng .                              /*obtain optional arguments from the CL*/
                              /*     ┌───────────◄ None specified?  Then use the default*/
                              /*     │             of Jules Verne's Lincoln Island,     */
                              /*     ↓             aka      Ernest Legouve Reef.        */
if lat=='' | lat==","  then lat=   -4.95         /*Not specified?  Then use the default.*/
if lng=='' | lng==","  then lng= -150.5          /* "      "         "   "   "     "    */
mer=format(lng/15, , 0) * 15                     /*calculate legal meridian longitude.  */
sineLat=sin( d2r(lat) )                          /*calculate sine of (radian) latitude. */
w1=max( length('hour'     ), length("midnight"  ))  + 2   /*compute the max hour  width.*/
w2=max( length('sun hour' ), length("angle"     ))  + 2   /*   "     "   " angle    "   */
w3=max( length('dial hour'), length("line angle"))  + 2   /*   "     "   " lineº    "   */
L=max(length(lat), length(lng), length(mer) )    /*find maximum length of three numbers.*/
     say '       latitude:'    right(lat, L)     /*display the  latitude to the terminal*/
     say '      longitude:'    right(lng, L)     /*   "     "  longitude  "  "     "    */
     say ' legal meridian:'    right(mer, L)     /*   "    legal meridian "  "     "    */
         indent=left('', 30)                     /*make prettier: indented presentation.*/
     say indent  center('    ', w1)   center("sun hour", w2)     center('dial hour' , w3)
     say indent  center('hour', w1)   center("angle"   , w2)     center('line angle', w3)
call sep                                         /*to help a one─eyed pirate's eyeball. */
        do h=-6  to 6                            /*Okey dokey then, now let's show stuff*/
             select
             when abs(h)==12  then hc='midnight' /*Holy smokes! Above the arctic circle.*/
             when h <0        then hc= -h  'am'  /*convert da hour for human beans (sic)*/
             when h==0        then hc='noon'     /*   ···  easier to understand now.    */
             when h >0        then hc= h   'pm'  /*   ···  even more meaningful.        */
             end   /*select*/
        hra=15 * h  -  lng + mer                 /*calculate sun hour angle (in degrees)*/
        hla=r2d( Atan(sineLat * tan( d2r(hra)))) /*this is the heavy lifting calculation*/
        if abs(hra)>90  then hla=hla + 180*sign(hra*lat)   /*adjust for negative angle. */
        say indent center(hc, w1)  right(format(hra, ,1), w2)   right(format(hla, ,1), w3)
        end        /*h*/
call sep                                         /*to help a one─eyed pirate's eyeball. */
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
pi:   pi= 3.1415926535897932384626433832795028841971693993751058209749445923078; return pi
d2d:  return arg(1)              // 360          /*normalize degrees ──► a unit circle. */
d2r:  return r2r( arg(1) * pi()   / 180)         /*convert degrees   ──► radians.       */
r2d:  return d2d( (arg(1) * 180   / pi() )  )    /*convert radians   ──► degrees.       */
r2r:  return arg(1)              //(pi() * 2)    /*normalize radians ──► a unit circle. */
sep:  say indent  copies('═', w1)    copies("═", w2)     copies('═', w3);  return
tan:  procedure; parse arg x;  _=cos(x);   if _=0  then call tanErr;       return sin(x)/_
tellErr: say;   say '*** error ***';     say;      say arg(1);      say;          exit 13
AsinErr: call tellErr 'Asin(x),  X  must be in the range of  -1 ──► +1,  X='  x
AcosErr: call tellErr 'Acos(x),  X  must be in the range of  -1 ──► +1,  X='  x
tanErr:  call tellErr 'tan(' || x") causes division by zero,  X="             x
Acos: procedure; arg x;  if x<-1 | x>1  then call AcosErr;      return .5 * pi() - Asin(x)
Atan: procedure; parse arg x; if abs(x)=1 then return pi()/4*x; return Asin(x/sqrt(1+x*x))
/*──────────────────────────────────────────────────────────────────────────────────────*/
Asin: procedure; parse arg x;   if x<-1 | x>1  then call AsinErr;    s=x*x
      if abs(x)>=sqrt(2)*.5  then return sign(x) * Acos(sqrt(1-s));  z=x;     o=x;     p=z
        do j=2 by 2; o=o*s*(j-1)/j; z=z+o/(j+1); if z=p  then leave; p=z; end;    return z
/*──────────────────────────────────────────────────────────────────────────────────────*/
sin:  procedure; parse arg x;   x=r2r(x);    numeric fuzz min(5, digits() - 3)
                 if abs(x)=pi()  then return ;                       return .sinCos(x,x,1)
/*──────────────────────────────────────────────────────────────────────────────────────*/
cos:  procedure; parse arg x;         x=r2r(x);          a=abs(x);          hpi=pi*.5
                 numeric fuzz min(6, digits() - 3);      if a=pi()    then return -1
                 if a=hpi | a=hpi*3  then return 0;      if a=pi()/3  then return .5
                 if a=pi() * 2 / 3   then return -.5;               return .sinCos(1,1,-1)
/*──────────────────────────────────────────────────────────────────────────────────────*/
.sinCos: parse arg z,_,i;             x=x*x;                         p=z
           do k=2 by 2; _= -_*x/(k*(k+i)); z=z+_; if z=p  then leave; p=z; end;   return z
/*──────────────────────────────────────────────────────────────────────────────────────*/
sqrt: procedure; parse arg x;  if x=0  then return 0;  d=digits();  numeric digits;  h=d+6
      m.=9; numeric form; parse value format(x,2,1,,0) 'E0' with g 'E' _ .; g=g*.5'e'_ % 2
        do j=0  while h>9;      m.j=h;              h=h%2+1;         end  /*j*/
        do k=j+5  to 0  by -1;  numeric digits m.k; g=(g+x/g)*.5;    end  /*k*/;  return g
```

```txt

       latitude:  -4.95
      longitude: -150.5
 legal meridian:   -150
                                           sun hour   dial hour
                                  hour      angle     line angle
                               ══════════ ══════════ ════════════
                                  6 am         -89.5         84.2
                                  5 am         -74.5         17.3
                                  4 am         -59.5          8.3
                                  3 am         -44.5          4.8
                                  2 am         -29.5          2.8
                                  1 am         -14.5          1.3
                                  noon           0.5          0.0
                                  1 pm          15.5         -1.4
                                  2 pm          30.5         -2.9
                                  3 pm          45.5         -5.0
                                  4 pm          60.5         -8.7
                                  5 pm          75.5        -18.5
                                  6 pm          90.5         84.2
                               ══════════ ══════════ ════════════

```



## Ring


```ring

# Project : Horizontal sundial calculations

load "stdlib.ring"
pi = 22/7
decimals(3)

latitude = -4.95
longitude = -150.5
meridian = -150.0

see "enter latitude (degrees): " + latitude + nl
see "enter longitude (degrees): " + longitude + nl
see "enter legal meridian (degrees): " + meridian + nl

see "time   " + "   sun hour angle" + "      dial hour line angle" + nl

for hour = 6 to 18
    hra = 15*hour - longitude + meridian - 180
    hla = 180/pi*(atan(sin(pi/180*latitude) * tan(pi/180*hra)))
    if fabs(hra) > 90
       hla = hla + 180 * sign(hra * latitude)
    ok
    see "" + hour + "           " + hra + "                  " + hla + nl
next

```

Output:

```txt

enter latitude (degrees): -4.950
enter longitude (degrees): -150.500
enter legal meridian (degrees): -150
time      sun hour angle      dial hour line angle
6           -89.500                  84.607
7           -74.500                  17.316
8           -59.500                  8.342
9           -44.500                  4.850
10           -29.500                  2.796
11           -14.500                  1.279
12           0.500                  -0.043
13           15.500                  -1.371
14           30.500                  -2.911
15           45.500                  -5.021
16           60.500                  -8.680
17           75.500                  -18.488
18           90.500                  -96.224

```



## Ruby

```ruby
include Math
DtoR = PI/180

print 'Enter latitude: '
lat = Float( gets )
print 'Enter longitude: '
lng = Float( gets )
print 'Enter legal meridian: '
ref = Float( gets )
puts

slat = sin( lat * DtoR )

puts "    sine of latitude:  %.3f"% slat
puts "    diff longitude:    %.3f"% (lng-ref)
puts
puts 'Hour, sun hour angle, dial hour line angle from 6am to 6pm'
-6.upto(6) do |h|
  hra = 15 * h
  hra -= lng - ref
  hla =  atan( slat * tan( hra * DtoR ))/ DtoR
  puts "HR =%3d; HRA =%7.3f; HLA =%7.3f" % [h, hra, hla]
end
```

```txt

Enter latitude: -4.95
Enter longitude: -150.5
Enter legal meridian: -150

    sine of latitude:  -0.086
    diff longitude:    -0.500

Hour, sun hour angle, dial hour line angle from 6am to 6pm
HR = -6; HRA =-89.500; HLA = 84.225
HR = -5; HRA =-74.500; HLA = 17.283
HR = -4; HRA =-59.500; HLA =  8.334
HR = -3; HRA =-44.500; HLA =  4.847
HR = -2; HRA =-29.500; HLA =  2.795
HR = -1; HRA =-14.500; HLA =  1.278
HR =  0; HRA =  0.500; HLA = -0.043
HR =  1; HRA = 15.500; HLA = -1.371
HR =  2; HRA = 30.500; HLA = -2.910
HR =  3; HRA = 45.500; HLA = -5.018
HR =  4; HRA = 60.500; HLA = -8.671
HR =  5; HRA = 75.500; HLA =-18.451
HR =  6; HRA = 90.500; HLA = 84.225

```



## Run BASIC


```runbasic
global pi
pi = 22 / 7

print "Enter latitude  (degrees)     : "; :input latitude      '    -4.95
print "Enter longitude (degrees)     : "; :input longitude     '   -150.5
print "Enter legal meridian (degrees): "; :input meridian      '   -150.0

print
print "Time     Sun hour angle   Dial hour line angle"

for hour = 6 TO 18
   hra = (15 * hour) - longitude + meridian -180
   hla =rad2deg( atn( sin( deg2rad( latitude)) *tan( deg2rad( hra))))
   if abs( hra) >90 then hla =hla +180 *sgn( hra *latitude)
   print using( "##", hour);"         ";using("####.##", hra);"         ";using("####.###", hla)
next hour

function rad2deg( theta)
    rad2deg =theta *180 /pi
end function

function deg2rad( theta)
    deg2rad =theta *pi /180
end function

function sgn( x)
    if x >0 then sgn =1 else sgn =-1
end function
end
```

```txt

Enter latitude: -4.95
Enter longitude: -150.5
Enter legal meridian: -150

Time     Sun hour angle   Dial hour line angle
 6          -89.50           84.606
 7          -74.50           17.316
 8          -59.50            8.342
 9          -44.50            4.850
10          -29.50            2.796
11          -14.50            1.279
12            0.50           -0.043
13           15.50           -1.371
14           30.50           -2.911
15           45.50           -5.021
16           60.50           -8.680
17           75.50          -18.488
18           90.50          -96.224
```



## Sather


```sather
class MAIN is

  getvalue(s:STR):FLT is
    #OUT + s + ": ";
    return #FLT(#IN.get_line.str);
  end;

  dr(a:FLT):FLT is
    return a * FLT::pi / 180.0;
  end;

  rd(a:FLT):FLT is
    return a * 180.0 / FLT::pi;
  end;

  main is
    lat ::= getvalue("Enter latitude");
    lng ::= getvalue("Enter longitude");
    ref ::= getvalue("Enter legal meridian");
    #OUT + "\n";
    slat ::= dr(lat).sin;
    #OUT + "sine of latitude: " + #FMT("%.3f\n", slat);
    #OUT + "diff longitude: " + #FMT("%.3f\n\n", lng - ref);
    #OUT + "Hour, sun hour angle, dial hour line angle from 6am to 6pm\n";
    loop h ::= (-6).upto!(6);
      hra ::= 15.0 * h.flt;
      hra := hra - lng + ref;
      hla ::= rd((dr(hra).tan * slat).atan);
      #OUT + #FMT("HR = %3d;  \t  HRA=%7.3f;  \t  HLA= %7.3f\n", h, hra, hla);
    end;
  end;
end;
```



## Scala


```Scala
import java.util.Scanner

import scala.math.{atan2, cos, sin, toDegrees, toRadians}

object Sundial extends App {
  var lat, slat,lng, ref = .0
  val sc = new Scanner(System.in)
  print("Enter latitude: ")
  lat = sc.nextDouble
  print("Enter longitude: ")
  lng = sc.nextDouble
  print("Enter legal meridian: ")
  ref = sc.nextDouble
  println()
  slat = Math.sin(Math.toRadians(lat))
  println(f"sine of latitude: $slat%.3f")
  println(f"diff longitude: ${lng - ref}%.3f\n")
  println("Hour, sun hour angle, dial hour line angle from 06h00 to 18h00")

  for (h <- -6 to 6) {
    val hra = 15.0 * h - lng + ref
    val hraRad = toRadians(hra)
    val hla = toDegrees(atan2(Math.sin(hraRad) * sin(Math.toRadians(lat)), cos(hraRad)))
    println(f"HR= $h%3d;\tHRA=$hra%7.3f;\tHLA= $hla%7.3f")
  }

}
```


## Seed7


```seed7
$ include "seed7_05.s7i";
  include "float.s7i";
  include "math.s7i";

const float: radianToDegrees is 57.295779513082320876798154814114;
const float: degreesToRadian is 0.017453292519943295769236907684883;

const proc: main is func
  local
    var float: lat is 0.0;
    var float: slat is 0.0;
    var float: lng is 0.0;
    var float: meridian is 0.0;
    var float: hla is 0.0;
    var float: hra is 0.0;
    var integer: h is 0;
  begin
    write("Enter latitude: ");
    readln(lat);
    write("Enter longitude: ");
    readln(lng);
    write("Enter legal meridian: ");
    readln(meridian);
    writeln;
    slat := sin(degreesToRadian * lat);
    writeln("sine of latitude: " <& slat digits 3);
    writeln("diff longitude: " <& lng - meridian digits 3);
    writeln;
    writeln("Hour, sun hour angle, dial hour line angle from 6am to 6pm");
    for h range -6 to 6 do
      hra := 15.0 * flt(h);
      hra := hra - lng + meridian;
      hla := radianToDegrees * atan(slat * tan(degreesToRadian * hra));
      writeln("HR= " <& h lpad 2 <& "; HRA= " <& hra digits 3 lpad 7 <&
              "; HLA= " <& hla digits 3 lpad 7);
    end for;
  end func;
```

```txt
Enter latitude: -4.95
Enter longitude: -150.5
Enter legal meridian: -150

sine of latitude: -0.086
diff longitude: -0.500

Hour, sun hour angle, dial hour line angle from 6am to 6pm
HR= -6; HRA= -89.500; HLA=  84.225
HR= -5; HRA= -74.500; HLA=  17.283
HR= -4; HRA= -59.500; HLA=   8.334
HR= -3; HRA= -44.500; HLA=   4.847
HR= -2; HRA= -29.500; HLA=   2.795
HR= -1; HRA= -14.500; HLA=   1.278
HR=  0; HRA=   0.500; HLA=  -0.043
HR=  1; HRA=  15.500; HLA=  -1.371
HR=  2; HRA=  30.500; HLA=  -2.910
HR=  3; HRA=  45.500; HLA=  -5.018
HR=  4; HRA=  60.500; HLA=  -8.671
HR=  5; HRA=  75.500; HLA= -18.451
HR=  6; HRA=  90.500; HLA=  84.225
```



## Sidef

```ruby
var latitude  = read('Enter latitude       => ', Number)
var longitude = read('Enter longitude      => ', Number)
var meridian  = read('Enter legal meridian => ', Number)
 
var lat_sin = latitude.deg2rad.sin
var offset = (meridian - longitude)
 
say('Sine of latitude: ', "%.4f" % lat_sin)
say('Longitude offset: ', offset)
say('=' * 48)
say(' Hour  : Sun hour angle° : Dial hour line angle°')
 
for hour (-6 .. 6) {
    var sun_deg  = (15*hour + offset)
    var line_deg = rad2deg(
        atan2(
            sin(deg2rad(sun_deg)) * lat_sin,
            cos(deg2rad(sun_deg))
        )
    )
    printf("%2d %s      %7.3f             %7.3f\n",
      (hour + 12) % 12 || 12, (hour < 0 ? 'AM' : 'PM'), sun_deg, line_deg)
}
```

```txt

Enter latitude       => -4.95
Enter longitude      => -150.5
Enter legal meridian => -150
Sine of latitude: -0.0863
Longitude offset: 0.5

### ==========================================

 Hour  : Sun hour angle° : Dial hour line angle°
 6 AM      -89.500              84.225
 7 AM      -74.500              17.283
 8 AM      -59.500               8.334
 9 AM      -44.500               4.847
10 AM      -29.500               2.795
11 AM      -14.500               1.278
12 PM        0.500              -0.043
 1 PM       15.500              -1.371
 2 PM       30.500              -2.910
 3 PM       45.500              -5.018
 4 PM       60.500              -8.671
 5 PM       75.500             -18.451
 6 PM       90.500             -95.775

```



## Smalltalk

```smalltalk
|lat slat lng ref hra hla pi|
pi := 1 arcTan * 4.
'Enter latitude:       ' display. lat := stdin nextLine asNumber.
'Enter longitude:      ' display. lng := stdin nextLine asNumber.
'Enter legal meridian: ' display. ref := stdin nextLine asNumber.
slat := lat degreesToRadians sin.
('sine of latitude: %1' % { slat }) displayNl.
('diff longitude: %1' % { lng - ref }) displayNl.

'Hour, sun hour angle, dial hour line angle from 6am to 6pm' displayNl.

-6 to: 6 do: [ :h |
  hra := 15.0 * h.
  hra := hra - lng + ref.
  hla := (hra degreesToRadians tan * slat) arcTan radiansToDegrees.
  ('HR= %1;  %4  HRA=%2;  %4  HLA= %3' % { h. hra. hla. $<9> }) displayNl.
]
```



## Tcl

```tcl
set PI 3.1415927
fconfigure stdout -buffering none
puts -nonewline "Enter latitude       => "; gets stdin lat
puts -nonewline "Enter longitude      => "; gets stdin lng
puts -nonewline "Enter legal meridian => "; gets stdin ref
puts ""

set slat [expr {sin($lat*$PI/180)}]
puts [format "    sine of latitude:   %8g" $slat]
puts [format "    diff longitude:     %3.3f" [expr {$lng - $ref}]]
puts ""
puts "Hour, sun hour angle, dial hour line angle from 6am to 6pm"

for {set h -6} {$h<=6} {incr h} {
    set hra [expr {15.0 * $h}];      # hour angle is 15 times the hour #
    set hra [expr {$hra-$lng+$ref}]; # but correct for longitude difference #
    set hla [expr {atan($slat * tan($hra*$PI/180)) * 180/$PI}]
    puts [format "HR=%+3d; HRA=%+8.3f; HLA=%+8.3f" $h $hra $hla]
}
```

```txt

Enter latitude       => -4.95
Enter longitude      => -150.5
Enter legal meridian => -150

    sine of latitude:   -0.0862864
    diff longitude:     -0.500

Hour, sun hour angle, dial hour line angle from 6am to 6pm
HR= -6; HRA= -89.500; HLA= +84.225
HR= -5; HRA= -74.500; HLA= +17.283
HR= -4; HRA= -59.500; HLA=  +8.334
HR= -3; HRA= -44.500; HLA=  +4.847
HR= -2; HRA= -29.500; HLA=  +2.795
HR= -1; HRA= -14.500; HLA=  +1.278
HR= +0; HRA=  +0.500; HLA=  -0.043
HR= +1; HRA= +15.500; HLA=  -1.371
HR= +2; HRA= +30.500; HLA=  -2.910
HR= +3; HRA= +45.500; HLA=  -5.018
HR= +4; HRA= +60.500; HLA=  -8.671
HR= +5; HRA= +75.500; HLA= -18.451
HR= +6; HRA= +90.500; HLA= +84.225

```



## x86 Assembly

It must be linked with the C standard library and startup code.

```asm
	global main
	extern printf, scanf

	section .text

getvalue:
	push	edx
	push	eax
	call	printf
	add	esp, 4
	push	in_ft
	call	scanf
	add	esp, 8
	ret

st0dr:
	fld	qword [drfact]
	fmul
	ret


main:
	lea	eax, [lat_t]
	lea	edx, [lat]
	call    getvalue
	lea	eax, [lng_t]
	lea	edx, [lng]
	call	getvalue
	lea	eax, [ref_t]
	lea	edx, [ref]
	call	getvalue

	push	newline
	call	printf
	add	esp, 4

	fld	qword [lat]
	call	st0dr
	fsin
	fst	qword [slat]

	sub	esp, 8
	fstp	qword [esp]
	push	sin_ft
	call	printf
	add	esp, 12

	fld	qword [lng]
	fld	qword [ref]
	fsubr	st0, st1
	sub	esp, 8
	fstp	qword [esp]
	push	diff_ft
	call	printf
	add 	esp, 12

	push	tab_t
	call	printf
	add	esp, 4

	mov	ecx, -6
.loop:
	cmp	ecx, 6
	jg	.endloop

	push	ecx
	fild	dword [esp]
	fld	qword [xv]
	fmulp
	fld	qword [lng]
	fsubp
	fld	qword [ref]
	faddp
	pop	ecx

	sub	esp, 20
	mov	dword [esp], ecx
	fst	qword [esp+4]

	call	st0dr

	fptan
	fxch
	fld	qword [slat]
	fmulp
	fxch
	fpatan

	fld	qword [rdinv]
	fmul

	fstp	qword [esp+12]

	push	o_ft
	call	printf
	mov	ecx, [esp+4]
	add 	esp, 24

	inc	ecx
	jmp	.loop
.endloop:

	xor	eax, eax
	ret


	section .data

lat:	dq	0.0
lng:	dq	0.0
ref:	dq	0.0
xv:	dq	15.0
slat:	dq	0.0
drfact:	dq	0.01745329251994329576
rdinv:	dq	57.29577951308232090712


	section .rodata

lat_t:	db "Enter latitude: ", 0
lng_t:	db "Enter longitude: ", 0
ref_t:	db "Enter legal meridian: ", 0

in_ft:	db "%lf", 0
newline:
	db 10, 0

sin_ft:
	db "sine of latitude: %.3f", 10, 0
diff_ft:
	db "diff longitude: %.3f", 10, 10, 0

tab_t:
	db "Hour, sun hour angle, dial hour line angle from 6am to 6pm", 10, 0

o_ft:
	db "HR= %3d;  ",9,"  HRA=%7.3f;  ",9,"  HLA= %7.3f", 10, 0
```



## XBasic

```xbasic

PROGRAM "sundial"
VERSION "0.0001"

IMPORT "xma"

DECLARE FUNCTION Entry()

FUNCTION Entry()
  lat! = SINGLE(INLINE$("Enter latitude       => "))
  lng! = SINGLE(INLINE$("Enter longitude      => "))
  ref! = SINGLE(INLINE$("Enter legal meridian => "))
  PRINT
  slat! = SIN(lat! * $$PI / 180.0)
  PRINT "    sine of latitude:   "; FORMAT$("#.##^^^^", slat!)
  PRINT "    diff longitude:     "; FORMAT$("#.###", lng! - ref!)
  PRINT
  PRINT "Hour, sun hour angle, dial hour line angle from 6am to 6pm"
  FOR hour@ = -6 TO 6
    hourAngle! = 15 * hour@
    hourAngle! = hourAngle! - (lng! - ref!) ' correct for longitude difference
    hourLineAngle! = ATAN(slat! * TAN(hourAngle! * $$PI / 180.0)) * 180.0 / $$PI
    PRINT "HR="; FORMAT$("###", hour@);
    PRINT "; HRA="; FORMAT$("####.###", hourAngle!);
    PRINT "; HLA="; FORMAT$("####.###", hourLineAngle!)
  NEXT hour@
END FUNCTION
END PROGRAM

```

```txt

Enter latitude       => -4.95
Enter longitude      => -150.5
Enter legal meridian => -150

    sine of latitude:   -8.63E-02
    diff longitude:     -0.500

Hour, sun hour angle, dial hour line angle from 6am to 6pm
HR= -6; HRA= -89.500; HLA=  84.225
HR= -5; HRA= -74.500; HLA=  17.283
HR= -4; HRA= -59.500; HLA=   8.334
HR= -3; HRA= -44.500; HLA=   4.847
HR= -2; HRA= -29.500; HLA=   2.795
HR= -1; HRA= -14.500; HLA=   1.278
HR=  0; HRA=   0.500; HLA=  -0.043
HR=  1; HRA=  15.500; HLA=  -1.371
HR=  2; HRA=  30.500; HLA=  -2.910
HR=  3; HRA=  45.500; HLA=  -5.018
HR=  4; HRA=  60.500; HLA=  -8.671
HR=  5; HRA=  75.500; HLA= -18.451
HR=  6; HRA=  90.500; HLA=  84.225

```



## XPL0


```XPL0
inc  c:\cxpl\codes;
def  Pi = 3.14159265358979323846,
     Deg2Rad = Pi/180.0,
     Rad2Deg = 180.0/Pi,
     Tab = $09;
real Lat, SinLat, Long, Mer;
real HA, HLA;                   \hour angle and hour line angle
int  H, T;                      \hour, time
[Text(0, "Latitude:       ");  Lat:=  RlIn(0);
 Text(0, "Longitude:      ");  Long:= RlIn(0);
 Text(0, "Legal meridian: ");  Mer:=  RlIn(0);
Text(0, "
Hour  Sun hour angle   Dial hour line angle
");
Format(4, 3);
SinLat:= Sin(Lat*Deg2Rad);
for H:= -6 to 6 do
   [HA:= float(15 * H);         \hour angle is 15 times the hour
    HA:= HA - (Long-Mer);       \ but corrected for longitude difference
    HLA:= ATan2( SinLat * Sin(HA*Deg2Rad), Cos(HA*Deg2Rad) ) * Rad2Deg;
    T:= H+12;  if T>12 then T:= T-12;
    if T<10 then ChOut(0, ^ );  IntOut(0, T);
    Text(0, if H>=0 then "pm    " else "am      ");
    RlOut(0, HA);  ChOut(0, Tab);  RlOut(0, HLA);  CrLf(0);
   ];
]
```


```txt

Latitude:       -4.95
Longitude:      -150.5
Legal meridian: -150

Hour  Sun hour angle   Dial hour line angle
 6am     -89.500          84.225
 7am     -74.500          17.283
 8am     -59.500           8.334
 9am     -44.500           4.847
10am     -29.500           2.795
11am     -14.500           1.278
12pm       0.500          -0.043
 1pm      15.500          -1.371
 2pm      30.500          -2.910
 3pm      45.500          -5.018
 4pm      60.500          -8.671
 5pm      75.500         -18.451
 6pm      90.500         -95.775

```



## zkl

```zkl
//(degree measure)*Degrees => Radian measure
//(radian measure)/Degrees => Degree measure
const pi=(0.0).pi, toDeg=(0.0).pi/180;

latitude :=ask(0,"Enter latitude: ").toFloat();
longitude:=ask(1,"Enter longitude: ").toFloat();
meridian :=ask(2,"Enter legal meridian: ").toFloat();

sineLatitude:=(latitude * toDeg).sin();
Console.writeln();
Console.writeln("Sine of latitude: ",sineLatitude);
Console.writeln("Difference of Longitudes (given longitude - meridian): ",longitude-meridian);
Console.writeln();

println("Numbers from 6 AM to 6 PM: ");
println("Hour\t\tSun hour angle\t Dial hour line angle");

foreach hour in ([-6..6]){
   clockHour:=( if(hour < 0) "%sAM".fmt(hour.abs()) else "%sPM".fmt(hour) );
   shr      :=15.0*hour - (longitude - meridian);
   dhla     :=(sineLatitude*(shr*toDeg).tan()).atan()/toDeg;
   Console.writeln("%s\t\t%5.1f\t\t%+7.3f".fmt(clockHour,shr,dhla));
}
```

```txt

$ zkl bbb -4.95 -150.5 -150

Sine of latitude: -0.0862864
Difference of Longitudes (given longitude - meridian): -0.5

Numbers from 6 AM to 6 PM:
Hour		Sun hour angle	 Dial hour line angle
6AM		-89.5		+84.225
5AM		-74.5		+17.283
4AM		-59.5		 +8.334
3AM		-44.5		 +4.847
2AM		-29.5		 +2.795
1AM		-14.5		 +1.278
0PM		  0.5		 -0.043
1PM		 15.5		 -1.371
2PM		 30.5		 -2.910
3PM		 45.5		 -5.018
4PM		 60.5		 -8.671
5PM		 75.5		-18.451
6PM		 90.5		+84.225

```

```txt
$ zkl bbb -4.95
Enter longitude: -150.5
Enter legal meridian: -150
<as above>

```



## ZX Spectrum Basic

```zxbasic
10 DEF FN r(x)=x*PI/180
20 DEF FN d(x)=x*180/PI
30 INPUT "Enter latitude (degrees): ";latitude
40 INPUT "Enter longitude (degrees): ";longitude
50 INPUT "Enter legal meridian (degrees): ";meridian
60 PRINT "Latitude: ";latitude
70 PRINT "Longitude:";longitude
80 PRINT "Legal meridian: ";meridian
90 PRINT '"      Sun         Dial"
100 PRINT "Time  hour angle  hour line ang."
110 PRINT "________________________________"
120 FOR h=6 TO 18
130 LET hra=15*h-longitude+meridian-180
140 LET hla=FN d(ATN (SIN (FN r(latitude))*TAN (FN r(hra))))
150 IF ABS (hra)>90 THEN LET hla=hla+180*SGN (hra*latitude)
160 PRINT h;"    ";hra;"        ";hla
170 NEXT h
```

