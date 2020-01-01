+++
title = "Angle difference between two bearings"
description = ""
date = 2019-10-06T00:24:38Z
aliases = []
[extra]
id = 21229
[taxonomies]
categories = []
tags = []
+++

[[Category:Geometry]]
{{task}}
Finding the angle between two bearings is often confusing.<ref>[https://stackoverflow.com/questions/16180595/find-the-angle-between-two-bearings]</ref>


;Task:
Find the angle which is the result of the subtraction '''b2 - b1''', where '''b1''' and '''b2''' are the bearings.


Input bearings are expressed in the range   '''-180'''   to   '''+180'''   degrees.

The result is also expressed in the range   '''-180'''   to   '''+180'''   degrees.


Compute the angle for the following pairs:
*  20 degrees ('''b1''') and 45 degrees ('''b2''')
* -45 and  45
* -85 and  90
* -95 and  90
* -45 and 125
* -45 and 145
*  29.4803 and  -88.6381
* -78.3251 and -159.036


;Optional extra:
Allow the input bearings to be any (finite) value.


;Test cases:
*  -70099.74233810938   and   29840.67437876723
* -165313.6666297357    and   33693.9894517456
*    1174.8380510598456 and -154146.66490124757
*   60175.77306795546   and   42213.07192354373





## 11l


```11l
F get_difference(b1, b2)
   R wrap(b2 - b1, -180.0, 180.0)

print(get_difference( 20.0, 45.0))
print(get_difference(-45.0, 45.0))
print(get_difference(-85.0, 90.0))
print(get_difference(-95.0, 90.0))
print(get_difference(-45.0, 125.0))
print(get_difference(-45.0, 145.0))
print(get_difference(-45.0, 125.0))
print(get_difference(-45.0, 145.0))
print(get_difference(29.4803, -88.6381))
print(get_difference(-78.3251, -159.036))
print(‘’)
print(get_difference(-70099.74233810938, 29840.67437876723))
print(get_difference(-165313.6666297357, 33693.9894517456))
print(get_difference(1174.8380510598456, -154146.66490124757))
print(get_difference(60175.77306795546, 42213.07192354373))
```


{{out}}

```txt

25
90
175
-175
170
-170
170
-170
-118.118
-80.7109

-139.583
-72.3439
-161.503
37.2989

```



## 360 Assembly

{{trans|Rexx}}

```360asm
*        Angle difference between two bearings - 06/06/2018
ANGLEDBB CSECT
         USING  ANGLEDBB,R13       base register
         B      72(R15)            skip savearea
         DC     17F'0'             savearea
         SAVE   (14,12)            save previous context
         ST     R13,4(R15)         link backward
         ST     R15,8(R13)         link forward
         LR     R13,R15            set addressability
         LA     R10,T-4            @t
         LA     R6,1               i=1
       DO WHILE=(C,R6,LE,N)        do i=1 to n
         LA     R10,4(R10)           next @t
         L      R7,0(R10)            a=t(i,1)
         LA     R10,4(R10)           next @t
         L      R8,0(R10)            b=t(i,2)
         LR     R4,R8                b
         SR     R4,R7                b-a
         SRDA   R4,32                ~
         D      R4,=F'3600000'       /360
         A      R4,=F'5400000'       +540
         SRDA   R4,32                ~
         D      R4,=F'3600000'       /360
         S      R4,=F'1800000'       x=((((b-a)//360)+540)//360)-180
         XDECO  R7,XDEC              edit a
         MVC    PG(8),XDEC           output a
         MVC    PG+9(4),XDEC+8       output a decimals
         XDECO  R8,XDEC              edit b
         MVC    PG+14(8),XDEC        output b
         MVC    PG+23(4),XDEC+8      output b decimals
         XDECO  R4,XDEC              edit x
         MVC    PG+28(8),XDEC        output x
         MVC    PG+37(4),XDEC+8      output x decimals
         XPRNT  PG,L'PG              print
         LA     R6,1(R6)             i++
       ENDDO    ,                  enddo i
         L      R13,4(0,R13)       restore previous savearea pointer
         RETURN (14,12),RC=0       restore registers from calling sav
N        DC     F'8'               number of pairs
T        DC     F'200000',F'450000',F'-450000',F'450000'
         DC     F'-850000',F'900000',F'-950000',F'900000'
         DC     F'-450000',F'1250000',F'450000',F'1450000'
         DC     F'294803',F'-886361',F'-783251',F'-1590360'
PG       DC     CL80'12345678.1234 12345678.1234 12345678.1234'
XDEC     DS     CL12               temp
         YREGS
         END    ANGLEDBB
```

{{out}}

```txt

      20.0000       45.0000       25.0000
     -45.0000       45.0000       90.0000
     -85.0000       90.0000      175.0000
     -95.0000       90.0000     -175.0000
     -45.0000      125.0000      170.0000
      45.0000      145.0000      100.0000
      29.4803      -88.6361     -118.1164
     -78.3251     -159.0360      -80.7109

```




## AWK


```AWK

# syntax: GAWK -f ANGLE_DIFFERENCE_BETWEEN_TWO_BEARINGS.AWK
BEGIN {
    fmt = "%11s %11s %11s\n"
    while (++i <= 11) { u = u "-" }
    printf(fmt,"B1","B2","DIFFERENCE")
    printf(fmt,u,u,u)
    main(20,45)
    main(-45,45)
    main(-85,90)
    main(-95,90)
    main(-45,125)
    main(-45,145)
    main(29.4803,-88.6381)
    main(-78.3251,-159.036)
    main(-70099.74233810938,29840.67437876723)
    main(-165313.6666297357,33693.9894517456)
    main(1174.8380510598456,-154146.66490124757)
    main(60175.77306795546,42213.07192354373)
    exit(0)
}
function main(b1,b2) {
    printf("%11.2f %11.2f %11.2f\n",b1,b2,angle_difference(b1,b2))
}
function angle_difference(b1,b2,  r) {
    r = (b2 - b1) % 360
    if (r < -180) {
      r += 360
    }
    if (r >= 180) {
      r -= 360
    }
    return(r)
}

```

{{out}}

```txt

         B1          B2  DIFFERENCE
----------- ----------- -----------
      20.00       45.00       25.00
     -45.00       45.00       90.00
     -85.00       90.00      175.00
     -95.00       90.00     -175.00
     -45.00      125.00      170.00
     -45.00      145.00     -170.00
      29.48      -88.64     -118.12
     -78.33     -159.04      -80.71
  -70099.74    29840.67     -139.58
 -165313.67    33693.99      -72.34
    1174.84  -154146.66     -161.50
   60175.77    42213.07       37.30

```



## Ada

Ada does not provide a built-in mod function for floating point types. This program supplies one.

```Ada

-----------------------------------------------------------------------
-- Angle difference between two bearings
-----------------------------------------------------------------------
with Ada.Text_IO; use Ada.Text_IO;

procedure Bearing_Angles is
   type Real is digits 8;
   Package Real_Io is new Ada.Text_IO.Float_IO(Real);
   use Real_IO;
   type Angles is record
      B1 : Real;
      B2 : Real;
   end record;
   type Angle_Arr is array(Positive range <>) of Angles;

   function fmod(Left, Right : Real) return Real is
      Result : Real;
   begin
      Result := Left - Right*Real'Truncation(Left / Right);
      return Result;
   end fmod;

   The_Angles : Angle_Arr := ((20.0,45.0),(-45.0, 45.0), (-85.0, 90.0),
                              (-95.0, 90.0), (-14.0, 125.0), (29.4803, -88.6381),
                              (-78.3251, -159.036),
                              (-70099.74233810938, 29840.67437876723),
                              (-165313.6666297357, 33693.9894517456),
                              (1174.8380510598456, -154146.66490124757),
                              (60175.77306795546, 42213.07192354373));
   Diff : Real;

begin

   for A of The_Angles loop
      Diff := fmod(A.b2 - A.b1, 360.0);
      If Diff < -180.0 then
         Diff := Diff + 360.0;
      elsif Diff > 180.0 then
         Diff := Diff - 360.0;
      end if;

      Put("Difference between ");
      Put(Item => A.B2, Fore => 7, Aft => 4, Exp => 0);
      Put(" and ");
      Put(Item => A.B1, Fore => 7, Aft => 4, Exp => 0);
      Put(" is ");
      Put(Item => Diff, Fore => 4, Aft => 4, Exp => 0);
      New_Line;
   end loop;

end Bearing_Angles;

```

{{out}}

```txt

Difference between      45.0000 and      20.0000 is   25.0000
Difference between      45.0000 and     -45.0000 is   90.0000
Difference between      90.0000 and     -85.0000 is  175.0000
Difference between      90.0000 and     -95.0000 is -175.0000
Difference between     125.0000 and     -14.0000 is  139.0000
Difference between     -88.6381 and      29.4803 is -118.1184
Difference between    -159.0360 and     -78.3251 is  -80.7109
Difference between   29840.6744 and  -70099.7423 is -139.5833
Difference between   33693.9895 and -165313.6666 is  -72.3439
Difference between -154146.6649 and    1174.8381 is -161.5030
Difference between   42213.0719 and   60175.7731 is   37.2989

```



## APL

Returns an angle in (-180,180]; so two opposite bearings have a difference of 180 degrees, which is more natural than -180 degrees.

```APL
[0]   D←B1 DIFF B2
[1]   D←180+¯360|180+B2-B1

```

{{out}}

```txt
      'B1' 'B2' 'DIFFERENCE'⍪(⊂'¯¯¯¯¯¯¯¯¯¯')⍪(⊃B),DIFF/¨B
         B1          B2  DIFFERENCE
 ¯¯¯¯¯¯¯¯¯¯  ¯¯¯¯¯¯¯¯¯¯  ¯¯¯¯¯¯¯¯¯¯
      20          45          25
     ¯45          45          90
     ¯85          90         175
     ¯95          90        ¯175
     ¯45         125         170
     ¯45         145        ¯170
      29.48      ¯88.64     ¯118.12
     ¯78.33     ¯159.04      ¯80.71
  ¯70099.74    29840.67     ¯139.59
 ¯165313.67     3369.99     ¯156.34
    1174.84  ¯154146.66     ¯161.5
   60175.77    42213.07       37.3

      270 DIFF 90.01
¯179.99
      270 DIFF 90
180

```



## Befunge


```befunge
012pv1   2      3        4              5         6           7    8
    >&:v   >859**%:459**1-`#v_     >12g!:12p#v_\-:459**1-`#v_     >.>
       >0`#^_8v             >859**-^                       >859**-^
       ^:+**95<                              >                      ^

```

The labelled points are:
1. Initialise write/not write and read input,
2. Put in the range 0-360 if negative,
3. Likewise if positive,
4. Put in range -180 - 180,
5. Check if write/not write step,
6. If write find difference,
7. Scale to -180 - 180,
8. Write out and onto next pair.

Unfortunately, due to the lack of floating-point arithmetic in befunge, it is impossible to do the full challenge, however, given the integer truncations of these values it works.

Input:

```txt

20 45
-45 45
-85 90
-95 90
-45 125
-45 145
29 -88
-78 -159
-70099 29840
-165313 33693
1174 -154146
60175 42213

```


{{out}}

```txt

25 90 175 -175 170 -170 -117 -81 -141 -74 -160 38

```



## C

This implementation either reads two bearings from the console or a file containing a list of bearings. Usage printed on incorrect invocation.

```C

#include<stdlib.h>
#include<stdio.h>
#include<math.h>

void processFile(char* name){

	int i,records;
	double diff,b1,b2;
	FILE* fp = fopen(name,"r");

	fscanf(fp,"%d\n",&records);

	for(i=0;i<records;i++){
		fscanf(fp,"%lf%lf",&b1,&b2);

		diff = fmod(b2-b1,360.0);
		printf("\nDifference between b2(%lf) and b1(%lf) is %lf",b2,b1,(diff<-180)?diff+360:((diff>=180)?diff-360:diff));
	}

	fclose(fp);
}

int main(int argC,char* argV[])
{
	double diff;

	if(argC < 2)
		printf("Usage : %s <bearings separated by a space OR full file name which contains the bearing list>",argV[0]);
	else if(argC == 2)
		processFile(argV[1]);
	else{
		diff = fmod(atof(argV[2])-atof(argV[1]),360.0);
		printf("Difference between b2(%s) and b1(%s) is %lf",argV[2],argV[1],(diff<-180)?diff+360:((diff>=180)?diff-360:diff));
	}

	return 0;
}

```

Invocation and output for two bearings :

```txt

C:\rosettaCode>bearingDiff.exe 29.4803 -88.6381
Difference between b2(-88.6381) and b1(29.4803) is -118.118400

```

File format for bearing list :

```txt

<Number of records>
<Each record consisting of two bearings separated by a space>

```

Input file :

```txt

12
20 45
-45 45
-85 90
-95 90
-45 125
-45 145
29.4803 -88.6381
-78.3251 -159.036
-70099.74233810938 29840.67437876723
-165313.6666297357 33693.9894517456
1174.8380510598456 -154146.66490124757
60175.77306795546 42213.07192354373

```

Invocation and output for above bearing list file :

```txt

C:\rosettaCode>bearingDiff.exe bearingList.txt

Difference between b2(45.000000) and b1(20.000000) is 25.000000
Difference between b2(45.000000) and b1(-45.000000) is 90.000000
Difference between b2(90.000000) and b1(-85.000000) is 175.000000
Difference between b2(90.000000) and b1(-95.000000) is -175.000000
Difference between b2(125.000000) and b1(-45.000000) is 170.000000
Difference between b2(145.000000) and b1(-45.000000) is -170.000000
Difference between b2(-88.638100) and b1(29.480300) is -118.118400
Difference between b2(-159.036000) and b1(-78.325100) is -80.710900
Difference between b2(29840.674379) and b1(-70099.742338) is -139.583283
Difference between b2(33693.989452) and b1(-165313.666630) is -72.343919
Difference between b2(-154146.664901) and b1(1174.838051) is -161.502952
Difference between b2(42213.071924) and b1(60175.773068) is 37.298856

```



## C++


```cpp
#include <cmath>
#include <iostream>
using namespace std;

double getDifference(double b1, double b2) {
	double r = fmod(b2 - b1, 360.0);
	if (r < -180.0)
		r += 360.0;
	if (r >= 180.0)
		r -= 360.0;
	return r;
}

int main()
{
	cout << "Input in -180 to +180 range" << endl;
	cout << getDifference(20.0, 45.0) << endl;
	cout << getDifference(-45.0, 45.0) << endl;
	cout << getDifference(-85.0, 90.0) << endl;
	cout << getDifference(-95.0, 90.0) << endl;
	cout << getDifference(-45.0, 125.0) << endl;
	cout << getDifference(-45.0, 145.0) << endl;
	cout << getDifference(-45.0, 125.0) << endl;
	cout << getDifference(-45.0, 145.0) << endl;
	cout << getDifference(29.4803, -88.6381) << endl;
	cout << getDifference(-78.3251, -159.036) << endl;

	cout << "Input in wider range" << endl;
	cout << getDifference(-70099.74233810938, 29840.67437876723) << endl;
	cout << getDifference(-165313.6666297357, 33693.9894517456) << endl;
	cout << getDifference(1174.8380510598456, -154146.66490124757) << endl;
	cout << getDifference(60175.77306795546, 42213.07192354373) << endl;

	return 0;
}
```


{{out}}

```txt
Input in -180 to +180 range
25
90
175
-175
170
-170
170
-170
-118.118
-80.7109
Input in wider range
-139.583
-72.3439
-161.503
37.2989
```



## C#


```csharp
using System;

namespace Angle_difference_between_two_bearings
{
	class Program
	{
		public static void Main(string[] args)
		{
			Console.WriteLine();
			Console.WriteLine("Hello World!");
			Console.WriteLine();

			// Calculate standard test cases
			Console.WriteLine(Delta_Bearing( 20M,45));
			Console.WriteLine(Delta_Bearing(-45M,45M));
			Console.WriteLine(Delta_Bearing(-85M,90M));
			Console.WriteLine(Delta_Bearing(-95M,90M));
			Console.WriteLine(Delta_Bearing(-45M,125M));
			Console.WriteLine(Delta_Bearing(-45M,145M));
			Console.WriteLine(Delta_Bearing( 29.4803M,-88.6381M));
			Console.WriteLine(Delta_Bearing(-78.3251M, -159.036M));

			// Calculate optional test cases
			Console.WriteLine(Delta_Bearing(-70099.74233810938M,   29840.67437876723M));
			Console.WriteLine(Delta_Bearing(-165313.6666297357M,   33693.9894517456M));
			Console.WriteLine(Delta_Bearing( 1174.8380510598456M, -154146.66490124757M));
			Console.WriteLine(Delta_Bearing( 60175.77306795546M,   42213.07192354373M));

			Console.WriteLine();
			Console.Write("Press any key to continue . . . ");
			Console.ReadKey(true);
		}

		static decimal Delta_Bearing(decimal b1, decimal b2)
		{
			/*
			 * Optimal solution
			 *
			decimal d = 0;

			d = (b2-b1)%360;

			if(d>180)
				d -= 360;
			else if(d<-180)
				d += 360;

			return d;
			 *
			 *
			 */


			//
			//
			//
			decimal d = 0;

			// Convert bearing to W.C.B
			if(b1<0)
				b1 += 360;
			if(b2<0)
				b2 += 360;

			///Calculate delta bearing
			//and
			//Convert result value to Q.B.
			d = (b2 - b1)%360;

			if(d>180)
				d -= 360;
			else if(d<-180)
				d += 360;

			return d;

			//
			//
			//
		}
	}
}
```

{{out| Output}}

```txt
Hello World!

25
90
175
-175
170
-170
-118,1184
-80,7109
-139,58328312339
-72,3439185187
-161,5029523074156
37,29885558827

Press any key to continue . . .
```



## Clojure


```clojure
(defn angle-difference [a b]
  (let [r (mod (- b a) 360)]
    (if (>= r 180)
      (- r 360)
      r)))

(angle-difference 20 45)  ; 25
(angle-difference -45 45) ; 90
(angle-difference -85 90) ; 175
(angle-difference -95 90) ; -175
(angle-difference -70099.74 29840.67) ; -139.59

```



## Common Lisp


```common lisp

(defun angle-difference (b1 b2)
	   (let ((diff (mod (- b2 b1) 360)))
	     (if (< diff -180)
		 (incf diff 360)
		 (if (> diff 180)
		     (decf diff 360)
		     diff))))

```

{{out| Output}}

```txt


CL-USER> (angle-difference 20 45)
25
CL-USER> (angle-difference -45 45)
90
CL-USER> (angle-difference -85 90)
175
CL-USER> (angle-difference -95 90)
-175
CL-USER> (angle-difference -70099.74 29840.67)
-139.58594


```



## D

{{trans|Java}}

```D
import std.stdio;

double getDifference(double b1, double b2) {
    double r = (b2 - b1) % 360.0;
    if (r < -180.0) {
        r += 360.0;
    }
    if (r >= 180.0) {
        r -= 360.0;
    }
    return r;
}

void main() {
    writeln("Input in -180 to +180 range");
    writeln(getDifference(20.0, 45.0));
    writeln(getDifference(-45.0, 45.0));
    writeln(getDifference(-85.0, 90.0));
    writeln(getDifference(-95.0, 90.0));
    writeln(getDifference(-45.0, 125.0));
    writeln(getDifference(-45.0, 145.0));
    writeln(getDifference(-45.0, 125.0));
    writeln(getDifference(-45.0, 145.0));
    writeln(getDifference(29.4803, -88.6381));
    writeln(getDifference(-78.3251, -159.036));

    writeln("Input in wider range");
    writeln(getDifference(-70099.74233810938, 29840.67437876723));
    writeln(getDifference(-165313.6666297357, 33693.9894517456));
    writeln(getDifference(1174.8380510598456, -154146.66490124757));
    writeln(getDifference(60175.77306795546, 42213.07192354373));
}
```


{{out}}

```txt
Input in -180 to +180 range
25
90
175
-175
170
-170
170
-170
-118.118
-80.7109
Input in wider range
-139.583
-72.3439
-161.503
37.2989
```


=={{header|F#|F sharp}}==

```fsharp
let deltaBearing (b1:double) (b2:double) =
    let r = (b2 - b1) % 360.0;
    if r > 180.0 then
        r - 360.0
    elif r < -180.0 then
        r + 360.0
    else
        r

[<EntryPoint>]
let main _ =
    printfn "%A" (deltaBearing      20.0                  45.0)
    printfn "%A" (deltaBearing     -45.0                  45.0)
    printfn "%A" (deltaBearing     -85.0                  90.0)
    printfn "%A" (deltaBearing     -95.0                  90.0)
    printfn "%A" (deltaBearing     -45.0                 125.0)
    printfn "%A" (deltaBearing     -45.0                 145.0)
    printfn "%A" (deltaBearing      29.4803              -88.6381)
    printfn "%A" (deltaBearing     -78.3251             -159.036)
    printfn "%A" (deltaBearing  -70099.74233810938     29840.67437876723)
    printfn "%A" (deltaBearing -165313.6666297357      33693.9894517456)
    printfn "%A" (deltaBearing    1174.8380510598456 -154146.66490124757)
    printfn "%A" (deltaBearing   60175.77306795546     42213.07192354373)
    0 // return an integer exit code
```

{{out}}

```txt
25.0
90.0
175.0
-175.0
170.0
-170.0
-118.1184
-80.7109
-139.5832831
-72.34391852
-161.5029523
37.29885559
```



## Factor

{{trans|F#}}
{{works with|Factor|0.99 development release 2019-03-17}}

```factor
USING: combinators generalizations kernel math prettyprint ;
IN: rosetta-code.bearings

: delta-bearing ( x y -- z )
    swap - 360 mod {
        { [ dup 180 > ] [ 360 - ] }
        { [ dup -180 < ] [ 360 + ] }
        [ ]
    } cond ;

: bearings-demo ( -- )
    20 45
    -45 45
    -85 90
    -95 90
    -45 125
    -45 145
    29.4803 -88.6381
    -78.3251 -159.036
    -70099.74233810938 29840.67437876723
    -165313.6666297357 33693.9894517456
    1174.8380510598456 -154146.66490124757
    60175.77306795546 42213.07192354373
    [ delta-bearing . ] 2 12 mnapply ;

MAIN: bearings-demo
```

{{out}}

```txt

25
90
175
-175
170
-170
-118.1184
-80.7109
-139.5832831233856
-72.34391851868713
-161.5029523074045
37.29885558826936

```



## Fortran

Rather than calculate angle differences and mess about with folding the results into ±180 and getting the sign right, why not use some mathematics? These days, trigonometrical functions are calculated swiftly by specialised hardware (well, microcode), and with the availability of functions working in degrees, matters are eased further nor is precision lost in converting from degrees to radians. So, the first step is to convert a bearing into an (x,y) unit vector via function CIS(t) = cos(t) + i.sin(t), which will handle all the annoyance of bearings specified in values above 360. Then, using the dot product of the two vectors allows the cosine of the angle to be known, and the cross product determines the sign.

However, this relies on the unit vectors being accurately so, and their subsequent dot product not exceeding one in size: given the rounding of results with the limited precision actual floating-point arithmetic, there may be problems. Proving that a calculation will not suffer these on a specific computer is difficult, especially as the desire for such a result may mean that any apparent pretext leading to that belief will be seized upon. Because calculations on the IBM pc and similar computers are conducted with 80-bit floating-point arithmetic, rounding errors for 64-bit results are likely to be small, but past experience leads to a "fog of fear" about the precise behaviour of floating-point arithmetic.

As it happens, the test data did not provoke any objections from the ACOSD function, but even so, a conversion to using ''arctan'' instead of ''arccos'' to recover angles would be safer. By using the four-quadrant ''arctan(x,y)'' function, the sign of the angle difference is also delivered and although that result could be in 0°-360° it turns out to be in ±180° as desired. On the other hand, the library of available functions did not include an arctan for complex parameters, so the complex number Z had to be split into its real and imaginary parts, thus requiring two appearances and to avoid repeated calculation, a temporary variable Z is needed. Otherwise, the statement could have been just <code>T = ATAN2D(Z1*CONJG(Z2))</code> and the whole calculation could be effected in one statement, <code>T = ATAN2D(CIS(90 - B1)*CONJG(CIS(90 - B2)))</code> And, since ''cis(t) = exp(i.t)'', <code>T = ATAN2D(EXP(CMPLX(0,90 - B1))*CONJG(EXP(CMPLX(0,90 - B2))))</code> - although using the arithmetic statement function does seem less intimidating.

The source style is F77 (even using the old-style arithmetic statement function) except for the convenience of generic functions taking the type of their parameters to save on the bother of DCMPLX instead of just CMPLX, etc. Floating-point constants in the test data are specified with ~D0, the exponential form that signifies double precision otherwise they would be taken as single precision values. Some compilers offer an option stating that all floating-point constants are to be taken as double precision. REAL*8 precision amounts to about sixteen decimal digits, so some of the supplied values will not be accurately represented, unless something beyond REAL*8 is available.
```Fortran
      SUBROUTINE BDIFF (B1,B2)	!Difference B2 - B1, as bearings. All in degrees, not radians.
       REAL*8 B1,B2	!Maximum precision, for large-angle folding.
       COMPLEX*16 CIS,Z1,Z2,Z	!Scratchpads.
       CIS(T) = CMPLX(COSD(T),SIND(T))	!Convert an angle into a unit vector.
        Z1 = CIS(90 - B1)	!Bearings run clockwise from north (y) around to east (x).
        Z2 = CIS(90 - B2)	!Mathematics runs counterclockwise from x (east).
        Z = Z1*CONJG(Z2)	!(Z1x,Z1y)(Z2x,-Z2y) = (Z1x.Z2x + Z1y.Z2y, Z1y.Z2x - Z1x.Z2y)
        T = ATAN2D(AIMAG(Z),REAL(Z))	!Madly, arctan(x,y) is ATAN(Y,X)!
        WRITE (6,10) B1,Z1,B2,Z2,T	!Two sets of numbers, and a result.
   10   FORMAT (2(F14.4,"(",F9.6,",",F9.6,")"),F9.3)	!Two lots, and a tail.
      END SUBROUTINE BDIFF	!Having functions in degrees saves some bother.

      PROGRAM ORIENTED
      REAL*8 B(24)	!Just prepare a wad of values.
      DATA B/20D0,45D0, -45D0,45D0, -85D0,90D0, -95D0,90D0,	!As specified.
     1      -45D0,125D0, -45D0,145D0, 29.4803D0,-88.6381D0,
     2      -78.3251D0,              -159.036D0,
     3   -70099.74233810938D0,      29840.67437876723D0,
     4  -165313.6666297357D0,       33693.9894517456D0,
     5     1174.8380510598456D0,  -154146.66490124757D0,
     6    60175.77306795546D0,      42213.07192354373D0/

      WRITE (6,1) ("B",I,"x","y", I = 1,2)	!Or, one could just list them twice.
    1 FORMAT (28X,"Bearing calculations, in degrees"//
     * 2(A13,I1,"(",A9,",",A9,")"),A9)	!Compare format 10, above.

      DO I = 1,23,2	!Step through the pairs.
        CALL BDIFF(B(I),B(I + 1))
      END DO

      END
```

The output shows the stages:

```txt

                            Bearing calculations, in degrees

            B1(        x,        y)            B2(        x,        y)
       20.0000( 0.342020, 0.939693)       45.0000( 0.707107, 0.707107)   25.000
      -45.0000(-0.707107, 0.707107)       45.0000( 0.707107, 0.707107)   90.000
      -85.0000(-0.996195, 0.087156)       90.0000( 1.000000, 0.000000)  175.000
      -95.0000(-0.996195,-0.087156)       90.0000( 1.000000, 0.000000) -175.000
      -45.0000(-0.707107, 0.707107)      125.0000( 0.819152,-0.573576)  170.000
      -45.0000(-0.707107, 0.707107)      145.0000( 0.573576,-0.819152) -170.000
       29.4803( 0.492124, 0.870525)      -88.6381(-0.999718, 0.023767) -118.118
      -78.3251(-0.979312, 0.202358)     -159.0360(-0.357781,-0.933805)  -80.711
   -70099.7423( 0.984016,-0.178078)    29840.6744(-0.633734, 0.773551) -139.584
  -165313.6666(-0.959667, 0.281138)    33693.9895(-0.559023,-0.829152)  -72.340
     1174.8381( 0.996437,-0.084339)  -154146.6649(-0.918252, 0.395996) -161.510
    60175.7731( 0.826820, 0.562467)    42213.0719( 0.998565,-0.053561)   37.297

```


## FreeBASIC


```freebasic
' version 28-01-2019
' compile with: fbc -s console

#Include "string.bi"

Function frmt(num As Double) As String

    Dim As String temp = Format(num, "#######.#############")
    Dim As Integer i = Len(temp) -1

    If temp[i] = Asc(".") Then temp[i] = 32
    If InStr(temp, ".") = 0 Then
        Return Right(Space(10) + temp, 9) + Space(13)
    End If
    temp = Space(10) + temp + Space(13)
    Return Mid(temp, InStr(temp, ".") -8, 22)

End Function

' ------=< MAIN >=------

Dim As Double b1, b2, bb1, bb2, diff

Print
Print "      b1                    b2                    difference"
Print " -----------------------------------------------------------"

Do
    Read b1, b2
    If b1 = 0 And b2 = 0 Then Exit Do
    diff = b2 - b1
    diff = diff - Int(diff / 360) * 360
    If diff > 180 Then diff -= 360
    Print frmt(b1); frmt(b2); frmt(diff)
Loop

Data   20,45,   -45,45,   -85,90
Data  -95,90,   -45,125,   -45,145
Data  29.4803,-88.6381,  -78.3251,-159.036
Data  -70099.74233810938,     29840.67437876723
Data -165313.6666297357,      33693.9894517456
Data    1174.8380510598456, -154146.66490124757
Data   60175.77306795546,     42213.07192354373

' empty keyboard buffer
While InKey <> "" : Wend
Print : Print "hit any key to end program"
Sleep
End
```

{{out}}

```txt
      b1                    b2                    difference
 -----------------------------------------------------------
      20                    45                    25
     -45                    45                    90
     -85                    90                   175
     -95                    90                  -175
     -45                   125                   170
     -45                   145                  -170
      29.4803              -88.6381             -118.1184
     -78.3251             -159.036               -80.7109
  -70099.7423381093831   29840.6743787672312    -139.5832831233856
 -165313.6666297357006   33693.9894517455978     -72.3439185186871
    1174.8380510598461 -154146.6649012475973    -161.5029523074336
   60175.7730679554588   42213.0719235437282      37.2988555882694
```



## Go

'''Basic task solution:'''

One feature of this solution is that if you can rely on the input bearings being in the range -180 to 180, you don't have to use math.Mod.  Another feature is the bearing type and method syntax.

```go
package main

import "fmt"

type bearing float64

var testCases = []struct{ b1, b2 bearing }{
    {20, 45},
    {-45, 45},
    {-85, 90},
    {-95, 90},
    {-45, 125},
    {-45, 145},
    {29.4803, -88.6381},
    {-78.3251, -159.036},
}

func main() {
    for _, tc := range testCases {
        fmt.Println(tc.b2.Sub(tc.b1))
    }
}

func (b2 bearing) Sub(b1 bearing) bearing {
    switch d := b2 - b1; {
    case d < -180:
        return d + 360
    case d > 180:
        return d - 360
    default:
        return d
    }
}
```

{{out}}

```txt

25
90
175
-175
170
-170
-118.1184
-80.7109

```

'''Optional extra solution:'''

A feature here is that the function body is a one-liner sufficient for the task test cases.

```go
package main

import (
    "fmt"
    "math"
)

var testCases = []struct{ b1, b2 float64 }{
    {20, 45},
    {-45, 45},
    {-85, 90},
    {-95, 90},
    {-45, 125},
    {-45, 145},
    {29.4803, -88.6381},
    {-78.3251, -159.036},
    {-70099.74233810938, 29840.67437876723},
    {-165313.6666297357, 33693.9894517456},
    {1174.8380510598456, -154146.66490124757},
    {60175.77306795546, 42213.07192354373},
}

func main() {
    for _, tc := range testCases {
        fmt.Println(angleDifference(tc.b2, tc.b1))
    }
}

func angleDifference(b2, b1 float64) float64 {
    return math.Mod(math.Mod(b2-b1, 360)+360+180, 360) - 180
}
```

{{out}}

```txt

25
90
175
-175
170
-170
-118.11840000000001
-80.71089999999998
-139.58328312338563
-72.34391851868713
-161.50295230740448
37.29885558826936

```



## Groovy

'''Solution A:'''
{{trans|C++}}

```groovy
def angleDifferenceA(double b1, double b2) {
    r = (b2 - b1) % 360.0
    (r > 180.0    ? r - 360.0
    : r <= -180.0 ? r + 360.0
                  : r)
}
```


'''Solution B:'''

In the spirit of the [[Angle_difference_between_two_bearings#Fortran|Fortran]] "Why branch when you can math?" solution, but without all the messy trigonometry.

```groovy
def angleDifferenceB(double b1, double b2) {
    ((b2 - b1) % 360.0 - 540.0) % 360.0 + 180.0
}
```

NOTE: We could ADD 540 and SUBTRACT 180 instead (as did many others, notably [[Angle_difference_between_two_bearings#360_Assembly|360_Assembly]], [[Angle_difference_between_two_bearings#NewLISP|NewLISP]], [[Angle_difference_between_two_bearings#Racket|Racket]], and [[Angle_difference_between_two_bearings#REXX|REXX]]). The difference is that my choice normalizes "about face" to +180° while the other (At least in [https://rosettacode.org/wiki/Category:Groovy Groovy] and other [https://rosettacode.org/wiki/Category:C C]-derived languages) normalizes "about face" to -180°.

'''Test:'''

```groovy
println "         b1                      b2                      diff A                  diff B"
[
    [b1:     20,               b2:     45             ],
    [b1:    -45,               b2:     45             ],
    [b1:    -85,               b2:     90             ],
    [b1:    -95,               b2:     90             ],
    [b1:    -45,               b2:    125             ],
    [b1:    -45,               b2:    145             ],
    [b1:     29.4803,          b2:    -88.6381        ],
    [b1:    -78.3251,          b2:   -159.036         ],
    [b1: -70099.74233810938,   b2:  29840.67437876723 ],
    [b1:-165313.6666297357,    b2:  33693.9894517456  ],
    [b1:   1174.8380510598456, b2:-154146.66490124757 ],
    [b1:  60175.77306795546,   b2:  42213.07192354373 ]
].each { bearings ->
    def (b1,b2) = bearings.values().collect{ it as double }
    printf ("%22.13f  %22.13f  %22.13f  %22.13f\n", b1, b2, angleDifferenceA(b1, b2), angleDifferenceB(b1, b2))
}
```


'''Output:'''

```txt
         b1                      b2                      diff A                  diff B
      20.0000000000000        45.0000000000000        25.0000000000000        25.0000000000000
     -45.0000000000000        45.0000000000000        90.0000000000000        90.0000000000000
     -85.0000000000000        90.0000000000000       175.0000000000000       175.0000000000000
     -95.0000000000000        90.0000000000000      -175.0000000000000      -175.0000000000000
     -45.0000000000000       125.0000000000000       170.0000000000000       170.0000000000000
     -45.0000000000000       145.0000000000000      -170.0000000000000      -170.0000000000000
      29.4803000000000       -88.6381000000000      -118.1184000000000      -118.1184000000000
     -78.3251000000000      -159.0360000000000       -80.7109000000000       -80.7109000000000
  -70099.7423381093800     29840.6743787672300      -139.5832831233856      -139.5832831233856
 -165313.6666297357000     33693.9894517456000       -72.3439185186871       -72.3439185186871
    1174.8380510598456   -154146.6649012475700      -161.5029523074045      -161.5029523074045
   60175.7730679554600     42213.0719235437300        37.2988555882694        37.2988555882694
```



## Haskell


```Haskell
import Text.Printf (printf)

type Radians = Float

type Degrees = Float

angleBetweenDegrees :: Degrees -> Degrees -> Degrees
angleBetweenDegrees a b = degrees $ bearingDelta (radians a) (radians b)

bearingDelta :: Radians -> Radians -> Radians
bearingDelta a b -- sign * dot-product
 = sign * acos ((ax * bx) + (ay * by))
  where
    (ax, ay) = (sin a, cos a)
    (bx, by) = (sin b, cos b)
    sign
      | ((ay * bx) - (by * ax)) > 0 = 1
      | otherwise = -1

degrees :: Radians -> Degrees
degrees = (/ pi) . (180 *)

radians :: Degrees -> Radians
radians = (/ 180) . (pi *)

-- TEST -----------------------------------------------------------------------
main :: IO ()
main =
  putStrLn . unlines $
  uncurry
    (((<*>) . printf "%6.2f° + %6.2f°  ->  %7.2f°") <*> angleBetweenDegrees) <$>
  [ (20.0, 45.0)
  , (-45.0, 45.0)
  , (-85.0, 90.0)
  , (-95.0, 90.0)
  , (-45.0, 125.0)
  , (-45.0, 145.0)
  ]
```

{{Out}}

```txt
 20.00° +  45.00°  ->    25.00°
-45.00° +  45.00°  ->    90.00°
-85.00° +  90.00°  ->   175.00°
-95.00° +  90.00°  ->  -175.00°
-45.00° + 125.00°  ->   170.00°
-45.00° + 145.00°  ->  -170.00°
```


=={{header|IS-BASIC}}==
<lang IS-BASIC>100 INPUT PROMPT "1. angle: ":A1
110 INPUT PROMPT "2. angle: ":A2
120 LET B=MOD(A2-A1,360)
130 IF B>180 THEN LET B=B-360
140 IF B<-180 THEN LET B=B+360
150 PRINT "Difference: ";B
```



## J



```j
relativeBearing=: (180 -~ 360 | 180 + -~)/"1
```


```j
tests=: _99&".;._2 noun define
20 45
-45 45
-85 90
-95 90
-45 125
-45 145
29.4803  -88.6381
-78.3251  -159.036
-70099.74233810938  29840.67437876723
-165313.6666297357  33693.9894517456
1174.8380510598456  -154146.66490124757
60175.77306795546  42213.07192354373
)
   tests ,. relativeBearing tests
      20       45       25
     _45       45       90
     _85       90      175
     _95       90     _175
     _45      125      170
     _45      145     _170
 29.4803 _88.6381 _118.118
_78.3251 _159.036 _80.7109
_70099.7  29840.7 _139.583
 _165314    33694 _72.3439
 1174.84  _154147 _161.503
 60175.8  42213.1  37.2989
```



## Java

{{trans|C++}}

```java
public class AngleDifference {

    public static double getDifference(double b1, double b2) {
        double r = (b2 - b1) % 360.0;
        if (r < -180.0)
            r += 360.0;
        if (r >= 180.0)
            r -= 360.0;
        return r;
    }

    public static void main(String[] args) {
        System.out.println("Input in -180 to +180 range");
        System.out.println(getDifference(20.0, 45.0));
        System.out.println(getDifference(-45.0, 45.0));
        System.out.println(getDifference(-85.0, 90.0));
        System.out.println(getDifference(-95.0, 90.0));
        System.out.println(getDifference(-45.0, 125.0));
        System.out.println(getDifference(-45.0, 145.0));
        System.out.println(getDifference(-45.0, 125.0));
        System.out.println(getDifference(-45.0, 145.0));
        System.out.println(getDifference(29.4803, -88.6381));
        System.out.println(getDifference(-78.3251, -159.036));

        System.out.println("Input in wider range");
        System.out.println(getDifference(-70099.74233810938, 29840.67437876723));
        System.out.println(getDifference(-165313.6666297357, 33693.9894517456));
        System.out.println(getDifference(1174.8380510598456, -154146.66490124757));
        System.out.println(getDifference(60175.77306795546, 42213.07192354373));
    }
}
```


{{out}}

```txt
Input in -180 to +180 range
25.0
90.0
175.0
-175.0
170.0
-170.0
170.0
-170.0
-118.1184
-80.7109
Input in wider range
-139.58328312338563
-72.34391851868713
-161.50295230740448
37.29885558826936
```



## Javascript


### ES5

This approach should be reliable but it is also very inefficient.


```javascript
function relativeBearing(b1Rad, b2Rad)
{
	b1y = Math.cos(b1Rad);
	b1x = Math.sin(b1Rad);
	b2y = Math.cos(b2Rad);
	b2x = Math.sin(b2Rad);
	crossp = b1y * b2x - b2y * b1x;
	dotp = b1x * b2x + b1y * b2y;
	if(crossp > 0.)
		return Math.acos(dotp);
	return -Math.acos(dotp);
}

function test()
{
	var deg2rad = 3.14159265/180.0;
	var rad2deg = 180.0/3.14159265;
	return "Input in -180 to +180 range\n"
		+relativeBearing(20.0*deg2rad, 45.0*deg2rad)*rad2deg+"\n"
		+relativeBearing(-45.0*deg2rad, 45.0*deg2rad)*rad2deg+"\n"
		+relativeBearing(-85.0*deg2rad, 90.0*deg2rad)*rad2deg+"\n"
		+relativeBearing(-95.0*deg2rad, 90.0*deg2rad)*rad2deg+"\n"
		+relativeBearing(-45.0*deg2rad, 125.0*deg2rad)*rad2deg+"\n"
		+relativeBearing(-45.0*deg2rad, 145.0*deg2rad)*rad2deg+"\n"

		+relativeBearing(29.4803*deg2rad, -88.6381*deg2rad)*rad2deg+"\n"
		+relativeBearing(-78.3251*deg2rad, -159.036*deg2rad)*rad2deg+"\n"

		+ "Input in wider range\n"
		+relativeBearing(-70099.74233810938*deg2rad, 29840.67437876723*deg2rad)*rad2deg+"\n"
		+relativeBearing(-165313.6666297357*deg2rad, 33693.9894517456*deg2rad)*rad2deg+"\n"
		+relativeBearing(1174.8380510598456*deg2rad, -154146.66490124757*deg2rad)*rad2deg+"\n"
		+relativeBearing(60175.77306795546*deg2rad, 42213.07192354373*deg2rad)*rad2deg+"\n";

}
```


{{out}}

```txt
Input in -180 to +180 range
25.000000000000004
90
174.99999999999997
-175.00000041135993
170.00000000000003
-170.00000041135996
-118.1184
-80.71089999999998
Input in wider range
-139.5833974814558
-72.34414600076728
-161.50277501127033
37.2988761562732
```



### ES6



```JavaScript
(() => {

    // bearingDelta :: Radians -> Radians -> Radians
    const bearingDelta = (ar, br) => {
        const [ax, ay] = [sin(ar), cos(ar)], [bx, by] = [sin(br), cos(br)],

        // Cross-product > 0 ?
        sign = ((ay * bx) - (by * ax)) > 0 ? +1 : -1;

        // Sign * dot-product
        return sign * acos((ax * bx) + (ay * by));
    };

    // Pi, sin, cos, acos :: Function
    const [Pi, sin, cos, acos] = ['PI', 'sin', 'cos', 'acos']
    .map(k => Math[k]),
        degRad = x => Pi * x / 180.0,
        radDeg = x => 180.0 * x / Pi;


    // TEST ------------------------------------------------------------------

    // justifyRight :: Int -> Char -> Text -> Text
    const justifyRight = (n, cFiller, strText) =>
        n > strText.length ? (
            (cFiller.repeat(n) + strText)
            .slice(-n)
        ) : strText;

    // showMap :: Degrees -> Degrees -> String
    const showMap = (da, db) =>
        justifyRight(6, ' ', `${da}° +`) +
        justifyRight(11, ' ', ` ${db}°  ->  `) +
        justifyRight(7, ' ', `${(radDeg(bearingDelta(degRad(da), degRad(db))))
            .toPrecision(4)}°`);

    return [
            [20, 45],
            [-45, 45],
            [-85, 90],
            [-95, 90],
            [-45, 125],
            [-45, 145]
        ].map(xy => showMap(...xy))
        .join('\n');
})();
```

{{Out}}

```txt

 20° +  45°  ->   25.00°
-45° +  45°  ->   90.00°
-85° +  90°  ->   175.0°
-95° +  90°  ->  -175.0°
-45° + 125°  ->   170.0°
-45° + 145°  ->  -170.0°
```



## Jsish


```javascript
/* Angle difference between bearings, in Jsish */
function angleDifference(bearing1:number, bearing2:number):number {
    var angle = (bearing2 - bearing1) % 360;
    if (angle < -180) angle += 360;
    if (angle >= 180) angle -= 360;
    return angle;
}

if (Interp.conf('unitTest')) {
    var dataSet = [[20, 45], [-45, 45], [-85, 90], [-95, 90], [-45, 125], [-45, 145],
                   [29.4803, -88.6381], [-78.3251, -159.036],
                   [-70099.74233810938, 29840.67437876723],
                   [-165313.6666297357, 33693.9894517456],
                   [1174.8380510598456, -154146.66490124757],
                   [60175.77306795546, 42213.07192354373]];
    printf("         Bearing 1          Bearing 2         Difference\n");
    for (var i = 0; i < dataSet.length; i++) {
        printf("%17S° %17S° %17S°\n", dataSet[i][0], dataSet[i][1],
               angleDifference(dataSet[i][0], dataSet[i][1])
        );
    }
}

/*
=!EXPECTSTART!=
         Bearing 1          Bearing 2         Difference
               20°                45°                25°
              -45°                45°                90°
              -85°                90°               175°
              -95°                90°              -175°
              -45°               125°               170°
              -45°               145°              -170°
          29.4803°          -88.6381°         -118.1184°
         -78.3251°          -159.036°          -80.7109°
-70099.7423381094°  29840.6743787672° -139.583283123386°
-165313.666629736°  33693.9894517456° -72.3439185186871°
 1174.83805105985° -154146.664901248° -161.502952307404°
 60175.7730679555°  42213.0719235437°  37.2988555882694°
=!EXPECTEND!=
*/
```


{{out}}

```txt
prompt$ jsish -u angleDifference.jsi
[PASS] angleDifference.jsi
```



## Julia

{{trans|Python}}


```julia
using Printf

function angdiff(a, b)
    r = (b - a) % 360.0
    if r ≥ 180.0
        r -= 360.0
    end

    return r
end

println("Input in -180 to +180 range:")
for (a, b) in [(20.0, 45.0), (-45.0, 45.0), (-85.0, 90.0), (-95.0, 90.0), (-45.0, 125.0), (-45.0, 145.0),
    (-45.0, 125.0), (-45.0, 145.0), (29.4803, -88.6381), (-78.3251, -159.036)]
    @printf("% 6.1f - % 6.1f = % 6.1f\n", a, b, angdiff(a, b))
end

println("\nInput in wider range:")
for (a, b) in [(-70099.74233810938, 29840.67437876723), (-165313.6666297357, 33693.9894517456),
    (1174.8380510598456, -154146.66490124757), (60175.77306795546, 42213.07192354373)]
    @printf("% 9.1f - % 9.1f = % 6.1f\n", a, b, angdiff(a, b))
end
```


{{out}}

```txt
Input in -180 to +180 range:
  20.0 -   45.0 =   25.0
 -45.0 -   45.0 =   90.0
 -85.0 -   90.0 =  175.0
 -95.0 -   90.0 = -175.0
 -45.0 -  125.0 =  170.0
 -45.0 -  145.0 = -170.0
 -45.0 -  125.0 =  170.0
 -45.0 -  145.0 = -170.0
  29.5 -  -88.6 = -118.1
 -78.3 - -159.0 =  -80.7

Input in wider range:
 -70099.7 -   29840.7 = -139.6
-165313.7 -   33694.0 =  -72.3
   1174.8 - -154146.7 = -161.5
  60175.8 -   42213.1 = -322.7
```



## K


```K

/ Angle difference between two angles
/ angledif.k

angdif: {[b1;b2]; :[(r:(b2-b1)!360.0)<-180.0;r+:360.0;r>180.0;r-:360.0];:r}

```


The output of a session is given below:
{{out}}

```txt

K Console - Enter \ for help

  \l angledif

  angdif[20;45]
25.0
  angdif[-45;45]
90.0
  angdif[-85;90]
175.0
  angdif[-95;90]
-175.0
  angdif[-45;125]
170.0
  angdif[29.4803;-88.6381]
-118.1184
  angdif[-78.3251;-159.036]
-80.7109
  angdif[-70099.74233810938;29840.67437876723]
-139.5833


```




## Kotlin


```scala
// version 1.1.2

class Angle(d: Double) {
    val value = when {
       d in -180.0 .. 180.0 -> d
       d > 180.0            -> (d - 180.0) % 360.0 - 180.0
       else                 -> (d + 180.0) % 360.0 + 180.0
    }

    operator fun minus(other: Angle) = Angle(this.value - other.value)
}

fun main(args: Array<String>) {
    val anglePairs = arrayOf(
         20.0 to 45.0,
        -45.0 to 45.0,
        -85.0 to 90.0,
        -95.0 to 90.0,
        -45.0 to 125.0,
        -45.0 to 145.0,
         29.4803 to -88.6381,
        -78.3251 to -159.036,
        -70099.74233810938 to 29840.67437876723,
        -165313.6666297357 to 33693.9894517456,
         1174.8380510598456 to -154146.66490124757,
         60175.77306795546 to 42213.07192354373
    )
    println("       b1            b2           diff")
    val f = "% 12.4f  % 12.4f  % 12.4f"
    for (ap in anglePairs) {
        val diff = Angle(ap.second) - Angle(ap.first)
        println(f.format(ap.first, ap.second, diff.value))
    }
}
```


{{out}}

```txt

       b1            b2           diff
     20.0000       45.0000       25.0000
    -45.0000       45.0000       90.0000
    -85.0000       90.0000      175.0000
    -95.0000       90.0000     -175.0000
    -45.0000      125.0000      170.0000
    -45.0000      145.0000     -170.0000
     29.4803      -88.6381     -118.1184
    -78.3251     -159.0360      -80.7109
 -70099.7423    29840.6744     -139.5833
-165313.6666    33693.9895      -72.3439
   1174.8381  -154146.6649     -161.5030
  60175.7731    42213.0719       37.2989

```




## Lua


Each bearing will be stored in an object that inherits methods to accomplish all parts of the task: accept a new number of degrees, automatically adjusting to the range [-180, 180]; construct new bearing objects; subtract another bearing from itself and return the difference; construct a list of new bearing objects given a list of arbitrary degree sizes; and format the number of degrees into a modest human-readable format. Bearings will be zero-initialized by default if no degree size is provided.


```lua
bearing = {degrees = 0} -- prototype object

function bearing:assign(angle)
	angle = tonumber(angle) or 0
	while angle > 180 do
		angle = angle - 360
	end
	while angle < -180 do
		angle = angle + 360
	end
	self.degrees = angle
end

function bearing:new(size)
	local child_object = {}
	setmetatable(child_object, {__index = self})
	child_object:assign(size)
	return child_object
end

function bearing:subtract(other)
	local difference = self.degrees - other.degrees
	return self:new(difference)
end

function bearing:list(sizes)
	local bearings = {}
	for index, size in ipairs(sizes) do
		table.insert(bearings, self:new(size))
	end
	return bearings
end

function bearing:text()
	return string.format("%.4f deg", self.degrees)
end

function main()
	local subtrahends = bearing:list{
		20, -45, -85, -95, -45, -45, 29.4803, -78.3251,
		-70099.74233810938, -165313.6666297357,
		1174.8380510598456, 60175.77306795546
	}
	local minuends = bearing:list{
		45, 45, 90, 90, 125, 145, -88.6381, -159.036,
		29840.67437876723, 33693.9894517456,
		-154146.66490124757, 42213.07192354373
	}
	for index = 1, #minuends do
		local b2, b1 = minuends[index], subtrahends[index]
		local b3 = b2:subtract(b1)
		local statement = string.format(
			"%s - %s = %s\n",
			b2:text(), b1:text(), b3:text()
		)
		io.write(statement)
	end
end

main()
```


{{out}}

```txt
45.0000 deg - 20.0000 deg = 25.0000 deg
45.0000 deg - -45.0000 deg = 90.0000 deg
90.0000 deg - -85.0000 deg = 175.0000 deg
90.0000 deg - -95.0000 deg = -175.0000 deg
125.0000 deg - -45.0000 deg = 170.0000 deg
145.0000 deg - -45.0000 deg = -170.0000 deg
-88.6381 deg - 29.4803 deg = -118.1184 deg
-159.0360 deg - -78.3251 deg = -80.7109 deg
-39.3256 deg - 100.2577 deg = -139.5833 deg
-146.0105 deg - -73.6666 deg = -72.3439 deg
-66.6649 deg - 94.8381 deg = -161.5030 deg
93.0719 deg - 55.7731 deg = 37.2989 deg
```



## Maple

{{trans|C++}}

```Maple
getDiff := proc(b1,b2)
	local r:
	r := frem(b2 - b1, 360):
	if r >= 180 then r := r - 360: fi:
	return r:
end proc:
getDiff(20,45);
getDiff(-45,45);
getDiff(-85,90);
getDiff(-95,90);
getDiff(-45,125);
getDiff(-45,145);
getDiff(29.4803, -88.6381);
getDiff(-78.3251,-159.036);
getDiff(-70099.74233810938,29840.67437876723);
getDiff(-165313.6666297357,33693.9894517456);
getDiff(1174.8380510598456,-154146.66490124757);
getDiff(60175.77306795546,42213.07192354373)
```

{{Out}}

```txt
25
90
175
-175
170
-170
-118.1184
-80.7109
-139.58328
-72.3340
-161.5030
37.29885
```



=={{header|Modula-2}}==
{{trans|Java}}

```modula2
FROM Terminal IMPORT *;

PROCEDURE WriteRealLn(value : REAL);
VAR str : ARRAY[0..16] OF CHAR;
BEGIN
    RealToStr(value, str);
    WriteString(str);
    WriteLn;
END WriteRealLn;

PROCEDURE AngleDifference(b1, b2 : REAL) : REAL;
VAR r : REAL;
BEGIN
    r := (b2 - b1);
    WHILE r < -180.0 DO
        r := r + 360.0;
    END;
    WHILE r >= 180.0 DO
        r := r - 360.0;
    END;
    RETURN r;
END AngleDifference;

BEGIN
    WriteString('Input in -180 to +180 range');
    WriteLn;
    WriteRealLn(AngleDifference(20.0, 45.0));
    WriteRealLn(AngleDifference(-45.0, 45.0));
    WriteRealLn(AngleDifference(-85.0, 90.0));
    WriteRealLn(AngleDifference(-95.0, 90.0));
    WriteRealLn(AngleDifference(-45.0, 125.0));
    WriteRealLn(AngleDifference(-45.0, 145.0));
    WriteRealLn(AngleDifference(29.4803, -88.6381));
    WriteRealLn(AngleDifference(-78.3251, -159.036));

    WriteString('Input in wider range');
    WriteLn;
    WriteRealLn(AngleDifference(-70099.74233810938, 29840.67437876723));
    WriteRealLn(AngleDifference(-165313.6666297357, 33693.9894517456));
    WriteRealLn(AngleDifference(1174.8380510598456, -154146.66490124757));
    WriteRealLn(AngleDifference(60175.77306795546, 42213.07192354373));

    ReadChar;
END Bearings.
```



## NewLISP

Taken from Racket solution


```lisp

#!/usr/bin/env newlisp
(define (bearing- bearing heading) (sub (mod (add (mod (sub bearing heading) 360.0) 540.0) 360.0) 180.0))

(bearing- 20 45)
(bearing- -45 45)
(bearing- -85 90)
(bearing- -95 90)
(bearing- -45 125)
(bearing- -45 145)
(bearing- 29.4803 -88.6381)
(bearing- -78.3251 -159.036)
(bearing- -70099.74233810938 29840.67437876723)
(bearing- -165313.6666297357 33693.9894517456)
(bearing- 1174.8380510598456 -154146.66490124757)
(bearing- 60175.77306795546 42213.07192354373))

```


{{out}}

```txt

-25
-90
-175
175
-170
170
118.11839999999995
80.71090000000004
139.58328312338563
72.34391851868713
161.50295230740448
-37.29885558826936
```



## Nim



```nim
import math
import strutils


proc delta(b1, b2: float) : float =
  result = (b2 - b1) mod 360.0

  if result < -180.0:
    result += 360.0
  elif result >= 180.0:
    result -= 360.0


let testVectors : seq[tuple[b1, b2: float]] = @[
      (20.00,       45.00 ),
     (-45.00,       45.00 ),
     (-85.00,       90.00 ),
     (-95.00,       90.00 ),
     (-45.00,      125.00 ),
     (-45.00,      145.00 ),
     ( 29.48,      -88.64 ),
     (-78.33,     -159.04 ),
  (-70099.74,    29840.67 ),
 (-165313.67,    33693.99 ),
    (1174.84,  -154146.66 ),
   (60175.77,    42213.07 ) ]

for vector in testVectors:
  echo vector.b1.formatFloat(ffDecimal, 2).align(13) &
       vector.b2.formatFloat(ffDecimal, 2).align(13) &
       delta(vector.b1, vector.b2).formatFloat(ffDecimal, 2).align(13)

```


{{out}}

```txt

        20.00        45.00        25.00
       -45.00        45.00        90.00
       -85.00        90.00       175.00
       -95.00        90.00      -175.00
       -45.00       125.00       170.00
       -45.00       145.00      -170.00
        29.48       -88.64      -118.12
       -78.33      -159.04       -80.71
    -70099.74     29840.67      -139.59
   -165313.67     33693.99       -72.34
      1174.84   -154146.66      -161.50
     60175.77     42213.07        37.30

```




## OCaml

{{trans|D}}

```ocaml
let get_diff b1 b2 =
  let r = mod_float (b2 -. b1) 360.0 in
  if r < -180.0
  then r +. 360.0
  else if r >= 180.0
  then r -. 360.0
  else r

let () =
  print_endline "Input in -180 to +180 range";
  Printf.printf " %g\n" (get_diff 20.0 45.0);
  Printf.printf " %g\n" (get_diff (-45.0) 45.0);
  Printf.printf " %g\n" (get_diff (-85.0) 90.0);
  Printf.printf " %g\n" (get_diff (-95.0) 90.0);
  Printf.printf " %g\n" (get_diff (-45.0) 125.0);
  Printf.printf " %g\n" (get_diff (-45.0) 145.0);
  Printf.printf " %g\n" (get_diff (-45.0) 125.0);
  Printf.printf " %g\n" (get_diff (-45.0) 145.0);
  Printf.printf " %g\n" (get_diff 29.4803 (-88.6381));
  Printf.printf " %g\n" (get_diff (-78.3251) (-159.036));

  print_endline "Input in wider range";
  Printf.printf " %g\n" (get_diff (-70099.74233810938) 29840.67437876723);
  Printf.printf " %g\n" (get_diff (-165313.6666297357) 33693.9894517456);
  Printf.printf " %g\n" (get_diff 1174.8380510598456 (-154146.66490124757));
  Printf.printf " %g\n" (get_diff 60175.77306795546 42213.07192354373);
;;
```


{{out}}

```txt
Input in -180 to +180 range
 25
 90
 175
 -175
 170
 -170
 170
 -170
 -118.118
 -80.7109
Input in wider range
 -139.583
 -72.3439
 -161.503
 37.2989
```



## PARI/GP


```parigp
centerliftmod1(x)=frac(x+1/2)-1/2;
anglediff(x,y)=centerliftmod1((y-x)/360)*360;
vecFunc(f)=v->call(f,v);
apply(vecFunc(anglediff), [[20,45], [-45,45], [-85,90], [-95,90], [-45,125], [-45,145], [29.4803,-88.6381], [-78.3251,-159.036], [-70099.74233810938,29840.67437876723], [-165313.6666297357,33693.9894517456], [1174.8380510598456,-154146.66490124757], [60175.77306795546,42213.07192354373]])
```

{{out}}

```txt
%1 = [25, 90, 175, -175, 170, -170, -118.11840000000000000000000000000000000, -80.710900000000000000000000000000000000, -139.58328312339000000000000000000000023, -72.343918518700000000000000000000000733, -161.50295230741560000000000000000000000, 37.298855588269999999999999999999999909]
```



## Pascal


This program is meant to be saved in the same folder as a file <code>angles.txt</code> containing the input. Each pair of angles to subtract appears on its own line in the input file.


```Pascal

Program Bearings;
{ Reads pairs of angles from a file and subtracts them }

Const
  fileName = 'angles.txt';

Type
  degrees = real;

Var
  subtrahend, minuend: degrees;
  angleFile: text;

function Simplify(angle: degrees): degrees;
{ Returns an number in the range [-180.0, 180.0] }
  begin
    while angle > 180.0 do
      angle := angle - 360.0;
    while angle < -180.0 do
      angle := angle + 360.0;
    Simplify := angle
  end;

function Difference(b1, b2: degrees): degrees;
{ Subtracts b1 from b2 and returns a simplified result }
  begin
    Difference := Simplify(b2 - b1)
  end;

procedure Subtract(b1, b2: degrees);
{ Subtracts b1 from b2 and shows the whole equation onscreen }
  var
    b3: degrees;
  begin
    b3 := Difference(b1, b2);
    writeln(b2:20:11, '   - ', b1:20:11, '   = ', b3:20:11)
  end;

Begin
  assign(angleFile, fileName);
  reset(angleFile);
  while not eof(angleFile) do
    begin
      readln(angleFile, subtrahend, minuend);
      Subtract(subtrahend, minuend)
    end;
  close(angleFile)
End.

```


{{in}}

                 20                    45
                -45                    45
                -85                    90
                -95                    90
                -45                   125
                -45                   145
            29.4803              -88.6381
           -78.3251              -159.036
 -70099.74233810938     29840.67437876723
 -165313.6666297357      33693.9894517456
 1174.8380510598456   -154146.66490124757
  60175.77306795546     42213.07192354373

{{out}}
      45.00000000000   -       20.00000000000   =       25.00000000000
      45.00000000000   -      -45.00000000000   =       90.00000000000
      90.00000000000   -      -85.00000000000   =      175.00000000000
      90.00000000000   -      -95.00000000000   =     -175.00000000000
     125.00000000000   -      -45.00000000000   =      170.00000000000
     145.00000000000   -      -45.00000000000   =     -170.00000000000
     -88.63810000000   -       29.48030000000   =     -118.11840000000
    -159.03600000000   -      -78.32510000000   =      -80.71090000000
   29840.67437876723   -   -70099.74233810938   =     -139.58328312339
   33693.98945174560   -  -165313.66662973570   =      -72.34391851869
 -154146.66490124760   -     1174.83805105985   =     -161.50295230740
   42213.07192354373   -    60175.77306795546   =       37.29885558827


## Perl

Perl's built-in modulo is integer-only, so import a suitable one from the <code>POSIX</code> core module

```perl
use POSIX 'fmod';

sub angle {
my($b1,$b2) = @_;
   my $b = fmod( ($b2 - $b1 + 720) , 360);
   $b -= 360 if $b >  180;
   $b += 360 if $b < -180;
   return $b;
}

@bearings = (
    20,  45,
   -45,  45,
   -85,  90,
   -95,  90,
   -45, 125,
   -45, 145,
    29.4803,  -88.6381,
   -78.3251, -159.036,
   -70099.74233810938,  29840.67437876723,
   -165313.6666297357,  33693.9894517456,
   1174.8380510598456, -154146.66490124757,
   60175.77306795546,   42213.07192354373
);

while(my ($b1,$b2) = splice(@bearings,0,2)) {
    printf "%10.2f %10.2f = %8.2f\n", $b1, $b2, angle($b1,$b2);
}

```

{{out}}

```txt
     20.00      45.00 =    25.00
    -45.00      45.00 =    90.00
    -85.00      90.00 =   175.00
    -95.00      90.00 =  -175.00
    -45.00     125.00 =   170.00
    -45.00     145.00 =  -170.00
     29.48     -88.64 =  -118.12
    -78.33    -159.04 =   -80.71
 -70099.74   29840.67 =  -139.58
-165313.67   33693.99 =   -72.34
   1174.84 -154146.66 =  -161.50
  60175.77   42213.07 =    37.30
```



## Perl 6

{{works with|Rakudo|2016.11}}


```perl6
sub infix:<∠> (Real $b1, Real $b2) {
   (my $b = ($b2 - $b1 + 720) % 360) > 180 ?? $b - 360 !! $b;
}

for 20, 45,
   -45, 45,
   -85, 90,
   -95, 90,
   -45, 125,
   -45, 145,
   29.4803, -88.6381,
   -78.3251, -159.036,
   -70099.74233810938, 29840.67437876723,
   -165313.6666297357, 33693.9894517456,
   1174.8380510598456, -154146.66490124757,
   60175.77306795546, 42213.07192354373

  -> $b1, $b2 { printf "%10.2f %10.2f = %8.2f\n", $b1, $b2, $b1 ∠ $b2 }
```

{{out}}

```txt
     20.00      45.00 =    25.00
    -45.00      45.00 =    90.00
    -85.00      90.00 =   175.00
    -95.00      90.00 =  -175.00
    -45.00     125.00 =   170.00
    -45.00     145.00 =  -170.00
     29.48     -88.64 =  -118.12
    -78.33    -159.04 =   -80.71
 -70099.74   29840.67 =  -139.58
-165313.67   33693.99 =   -72.34
   1174.84 -154146.66 =  -161.50
  60175.77   42213.07 =    37.30
```



## Phix


```Phix
function tz(atom a)
-- trim trailing zeroes and decimal point
    string res = sprintf("%16f",a)
    for i=length(res) to 1 by -1 do
        integer ch = res[i]
        if ch='0' or ch='.' then
            res[i] = ' '
        end if
        if ch!='0' then exit end if
    end for
    return res
end function

procedure test(atom b1, b2)
    atom diff = mod(b2-b1,360)
    diff -= iff(diff>180?360:0)
    printf(1,"%s %s %s\n",{tz(b1),tz(b2),tz(diff)})
end procedure

puts(1,"       b1               b2             diff\n")
puts(1,"---------------- ---------------- ----------------\n")
test(20,45)
test(-45,45)
test(-85,90)
test(-95,90)
test(-45,125)
test(-45,145)
test(29.4803,-88.6381)
test(-78.3251,-159.036)
test(-70099.74233810938,29840.67437876723)
test(-165313.6666297357,33693.9894517456)
test(1174.8380510598456,-154146.66490124757)
test(60175.77306795546,42213.07192354373)
```

{{out}}

```txt

       b1               b2             diff
---------------- ---------------- ----------------
       20               45               25
      -45               45               90
      -85               90              175
      -95               90             -175
      -45              125              170
      -45              145             -170
       29.4803         -88.6381        -118.1184
      -78.3251        -159.036          -80.7109
   -70099.742338     29840.674379      -139.583283
  -165313.66663      33693.989452       -72.343919
     1174.838051   -154146.664901      -161.502952
    60175.773068     42213.071924        37.298856

```



## Python

{{trans|C++}}

```python
from __future__ import print_function

def getDifference(b1, b2):
	r = (b2 - b1) % 360.0
	# Python modulus has same sign as divisor, which is positive here,
	# so no need to consider negative case
	if r >= 180.0:
		r -= 360.0
	return r

if __name__ == "__main__":
	print ("Input in -180 to +180 range")
	print (getDifference(20.0, 45.0))
	print (getDifference(-45.0, 45.0))
	print (getDifference(-85.0, 90.0))
	print (getDifference(-95.0, 90.0))
	print (getDifference(-45.0, 125.0))
	print (getDifference(-45.0, 145.0))
	print (getDifference(-45.0, 125.0))
	print (getDifference(-45.0, 145.0))
	print (getDifference(29.4803, -88.6381))
	print (getDifference(-78.3251, -159.036))

	print ("Input in wider range")
	print (getDifference(-70099.74233810938, 29840.67437876723))
	print (getDifference(-165313.6666297357, 33693.9894517456))
	print (getDifference(1174.8380510598456, -154146.66490124757))
	print (getDifference(60175.77306795546, 42213.07192354373))
```


{{out}}

```txt
Input in -180 to +180 range
25.0
90.0
175.0
-175.0
170.0
-170.0
170.0
-170.0
-118.11840000000001
-80.71089999999998
Input in wider range
-139.58328312338563
-72.34391851868713
-161.50295230740448
37.29885558826936
```



Or, generalising a little by deriving the degrees from a (Radians -> Radians) function, and formatting the output in columns:
{{Works with|Python|3.7}}

```python
'''Difference between two bearings'''

from math import (acos, cos, pi, sin)


# bearingDelta :: Radians -> Radians -> Radians
def bearingDelta(ar):
    '''Difference between two bearings,
       expressed in radians.'''
    def go(br):
        [(ax, ay), (bx, by)] = [
            (sin(x), cos(x)) for x in [ar, br]
        ]
        # cross-product > 0 ?
        sign = +1 if 0 < ((ay * bx) - (by * ax)) else -1
        # sign * dot-product
        return sign * acos((ax * bx) + (ay * by))
    return lambda br: go(br)


# TEST ----------------------------------------------------
# main :: IO ()
def main():
    '''Test and display'''

    # showMap :: Degrees -> Degrees -> String
    def showMap(da, db):
        return unwords(
            str(x).rjust(n) for n, x in
            [
                (22, str(da) + ' +'),
                (24, str(db) + '  -> '),
                (7, round(
                    degrees(
                        bearingDelta
                        (radians(da))
                        (radians(db))
                    ), 2)
                 )
            ]
        )

    print(__doc__ + ':')
    print(
        unlines(showMap(a, b) for a, b in [
            (20, 45),
            (-45, 45),
            (-85, 90),
            (-95, 90),
            (-45, 125),
            (-45, 145),
            (-70099.74233810938, 29840.67437876723),
            (-165313.6666297357, 33693.9894517456),
            (1174.8380510598456, -154146.66490124757),
            (60175.77306795546, 42213.07192354373)
        ]))


# GENERIC ----------------------------------------------


# radians :: Float x => Degrees x -> Radians x
def radians(x):
    '''Radians derived from degrees.'''
    return pi * x / 180


# degrees :: Float x => Radians x -> Degrees x
def degrees(x):
    '''Degrees derived from radians.'''
    return 180 * x / pi


# unlines :: [String] -> String
def unlines(xs):
    '''A single newline-delimited string derived
       from a list of strings.'''
    return '\n'.join(xs)


# unwords :: [String] -> String
def unwords(xs):
    '''A space-separated string derived from
       a list of words.'''
    return ' '.join(xs)


if __name__ == '__main__':
    main()
```

{{Out}}

```txt
Difference between two bearings:
                  20 +                  45  ->     25.0
                 -45 +                  45  ->     90.0
                 -85 +                  90  ->    175.0
                 -95 +                  90  ->   -175.0
                 -45 +                 125  ->    170.0
                 -45 +                 145  ->   -170.0
  -70099.74233810938 +   29840.67437876723  ->  -139.58
  -165313.6666297357 +    33693.9894517456  ->   -72.34
  1174.8380510598456 + -154146.66490124757  ->   -161.5
   60175.77306795546 +   42213.07192354373  ->     37.3
```



## Racket


''see my comments in discussion regards bearing-heading or vice versa''


```racket
#lang racket
(define (% a b) (- a (* b (truncate (/ a b)))))

(define (bearing- bearing heading)
  (- (% (+ (% (- bearing heading) 360) 540) 360) 180))

(module+ main
  (bearing- 20 45)
  (bearing- -45 45)
  (bearing- -85 90)
  (bearing- -95 90)
  (bearing- -45 125)
  (bearing- -45 145)
  (bearing- 29.4803 -88.6381)
  (bearing- -78.3251 -159.036)

  (bearing- -70099.74233810938 29840.67437876723)
  (bearing- -165313.6666297357 33693.9894517456)
  (bearing- 1174.8380510598456 -154146.66490124757)
  (bearing- 60175.77306795546 42213.07192354373))

(module+ test
  (require rackunit)

  (check-equal? (% 7.5 10) 7.5)
  (check-equal? (% 17.5 10) 7.5)
  (check-equal? (% -7.5 10) -7.5)
  (check-equal? (% -17.5 10) -7.5))
```


{{out}}

```txt
-25
-90
-175
175
-170
170
118.11839999999995
80.71090000000004
139.58328312338563
72.34391851868713
161.50295230740448
-37.29885558826936
```



## REXX

Some extra coding was added for a better visual presentation;   the angles were centered,   the answers were aligned.

```rexx
/*REXX pgm calculates difference between two angles (in degrees), normalizes the result.*/
numeric digits 25                                    /*use enough dec. digits for angles*/
call show      20,                    45             /*display angular difference (deg).*/
call show     -45,                    45             /*   "       "        "        "   */
call show     -85,                    90             /*   "       "        "        "   */
call show     -95,                    90             /*   "       "        "        "   */
call show     -45,                   125             /*   "       "        "        "   */
call show      45,                   145             /*   "       "        "        "   */
call show      29.4803,              -88.6361        /*   "       "        "        "   */
call show     -78.3251,             -159.036         /*   "       "        "        "   */
call show  -70099.74233810938,     29840.67437876723 /*   "       "        "        "   */
call show -165313.6666297357,      33693.9894517456  /*   "       "        "        "   */
call show    1174.8380510598456, -154146.66490124757 /*   "       "        "        "   */
call show   60175.773067955546,    42213.07192354373 /*   "       "        "        "   */
exit                                                 /*stick a fork in it,  we're done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
show: parse arg a,b;    d=digits();     $='º'    /*obtain the 2 angles (are in degrees).*/
      x=format( ( ( ((b-a) // 360) + 540) // 360) - 180, 4, d)   /*compute and format.  */
      if pos(., x)\==0  then x=strip( strip(x, 'T', 0), "T", .)  /*strip trailing chaff.*/
      say center(a || $, d)      '─'      center(b || $, d)       " ────► "      x || $
      return                                     /* [↑]  display the angular difference.*/
```

{{out|output}}

```txt

           20º            ─            45º             ────►    25º
          -45º            ─            45º             ────►    90º
          -85º            ─            90º             ────►   175º
          -95º            ─            90º             ────►  -175º
          -45º            ─           125º             ────►   170º
           45º            ─           145º             ────►   100º
        29.4803º          ─         -88.6361º          ────►  -118.1164º
        -78.3251º         ─         -159.036º          ────►   -80.7109º
   -70099.74233810938º    ─    29840.67437876723º      ────►  -139.58328312339º
   -165313.6666297357º    ─     33693.9894517456º      ────►   -72.3439185187º
   1174.8380510598456º    ─   -154146.66490124757º     ────►  -161.5029523074156º
   60175.773067955546º    ─    42213.07192354373º      ────►    37.298855588184º

```



## Ring


```ring

# Project : Angle difference between two bearings

decimals(4)
see "Input in -180 to +180 range:" + nl
see getDifference(20.0, 45.0) + nl
see getDifference(-45.0, 45.0) + nl
see getDifference(-85.0, 90.0) + nl
see getDifference(-95.0, 90.0) + nl
see getDifference(-45.0, 125.0) + nl
see getDifference(-45.0, 145.0) + nl
see getDifference(-45.0, 125.0) + nl
see getDifference(-45.0, 145.0) + nl
see getDifference(29.4803, -88.6381) + nl
see getDifference(-78.3251, -159.036) + nl

func getDifference(b1, b2)
     r = (b2 - b1) % 360.0
     if r >= 180.0
        r = r - 360.0
     end
     return r

```

Output:

```txt

Input in -180 to +180 range:
25
90
175
-175
170
-170
170
-170
-118.1184
-80.7109

```



## Ruby

{{trans|C++}}

```ruby
def getDifference(b1, b2)
	r = (b2 - b1) % 360.0
	# Ruby modulus has same sign as divisor, which is positive here,
	# so no need to consider negative case
	if r >= 180.0
		r -= 360.0
	end
	return r
end

if __FILE__ == $PROGRAM_NAME
	puts "Input in -180 to +180 range"
	puts getDifference(20.0, 45.0)
	puts getDifference(-45.0, 45.0)
	puts getDifference(-85.0, 90.0)
	puts getDifference(-95.0, 90.0)
	puts getDifference(-45.0, 125.0)
	puts getDifference(-45.0, 145.0)
	puts getDifference(-45.0, 125.0)
	puts getDifference(-45.0, 145.0)
	puts getDifference(29.4803, -88.6381)
	puts getDifference(-78.3251, -159.036)

	puts "Input in wider range"
	puts getDifference(-70099.74233810938, 29840.67437876723)
	puts getDifference(-165313.6666297357, 33693.9894517456)
	puts getDifference(1174.8380510598456, -154146.66490124757)
	puts getDifference(60175.77306795546, 42213.07192354373)
end
```


{{out}}

```txt
Input in -180 to +180 range
25.0
90.0
175.0
-175.0
170.0
-170.0
170.0
-170.0
-118.11840000000001
-80.71089999999998
Input in wider range
-139.58328312338563
-72.34391851868713
-161.50295230740448
37.29885558826936
```


## Rust


```rust

/// Calculate difference between two bearings, in -180 to 180 degrees range
pub fn angle_difference(bearing1: f64, bearing2: f64) -> f64 {
    let diff = (bearing2 - bearing1) % 360.0;
    if diff < -180.0 {
        360.0 + diff
    } else if diff > 180.0 {
        -360.0 + diff
    } else {
        diff
    }

}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_angle_difference() {
        assert_eq!(25.00, angle_difference(20.00, 45.00));
        assert_eq!(90.00, angle_difference(-45.00, 45.00));
        assert_eq!(175.00, angle_difference(-85.00, 90.00));
        assert_eq!(-175.00, angle_difference(-95.00, 90.00));
        assert_eq!(170.00, angle_difference(-45.00, 125.00));
        assert_eq!(-170.00, angle_difference(-45.00, 145.00));
        approx_eq(-118.1184, angle_difference(29.4803, -88.6381));
        approx_eq(-80.7109, angle_difference(-78.3251 , -159.036));
        approx_eq(-139.5832, angle_difference(-70099.74233810938, 29840.67437876723));
        approx_eq(-72.3439, angle_difference(-165313.6666297357, 33693.9894517456));
        approx_eq(-161.5029, angle_difference(1174.8380510598456, -154146.66490124757));
        approx_eq(37.2988, angle_difference(60175.77306795546, 42213.07192354373));
    }

    // approximate equality on floats.
    // see also https://crates.io/crates/float-cmp
    fn approx_eq(f1: f64, f2: f64) {
        assert!((f2-f1).abs() < 0.0001, "{} != {}", f1, f2)
    }
}

```



## Scala

{{Out}}Best seen running in your browser either by [https://scalafiddle.io/sf/lH5eUix/0 ScalaFiddle (ES aka JavaScript, non JVM)] or [https://scastie.scala-lang.org/IgQSmzcjSpSMxvWWoZUc9w Scastie (remote JVM)].

```Scala
object AngleDifference extends App {
  private def getDifference(b1: Double, b2: Double) = {
    val r = (b2 - b1) % 360.0
    if (r < -180.0) r + 360.0 else if (r >= 180.0) r - 360.0 else r
  }

  println("Input in -180 to +180 range")
  println(getDifference(20.0, 45.0))
  println(getDifference(-45.0, 45.0))
  println(getDifference(-85.0, 90.0))
  println(getDifference(-95.0, 90.0))
  println(getDifference(-45.0, 125.0))
  println(getDifference(-45.0, 145.0))
  println(getDifference(-45.0, 125.0))
  println(getDifference(-45.0, 145.0))
  println(getDifference(29.4803, -88.6381))
  println(getDifference(-78.3251, -159.036))

  println("Input in wider range")
  println(getDifference(-70099.74233810938, 29840.67437876723))
  println(getDifference(-165313.6666297357, 33693.9894517456))
  println(getDifference(1174.8380510598456, -154146.66490124757))
  println(getDifference(60175.77306795546, 42213.07192354373))

}
```



## Seed7

The library [http://seed7.sourceforge.net/libraries/float.htm float.s7i] supports the
operator [http://seed7.sourceforge.net/libraries/float.htm#(in_float)mod(in_float) mod],
which can be used to solve this task easily.


```seed7
$ include "seed7_05.s7i";
  include "float.s7i";

const func float: getDifference (in float: b1, in float: b2) is func
  result
    var float: difference is 0.0;
  begin
    difference := (b2 - b1) mod 360.0;
    if difference > 180.0 then
      difference -:= 360.0;
    end if;
  end func;

const proc: main is func
  begin
    writeln("Input in -180 to +180 range");
    writeln(getDifference(20.0, 45.0));
    writeln(getDifference(-45.0, 45.0));
    writeln(getDifference(-85.0, 90.0));
    writeln(getDifference(-95.0, 90.0));
    writeln(getDifference(-45.0, 125.0));
    writeln(getDifference(-45.0, 145.0));
    writeln(getDifference(-45.0, 125.0));
    writeln(getDifference(-45.0, 145.0));
    writeln(getDifference(29.4803, -88.6381));
    writeln(getDifference(-78.3251, -159.036));
    writeln("Input in wider range");
    writeln(getDifference(-70099.74233810938, 29840.67437876723));
    writeln(getDifference(-165313.6666297357, 33693.9894517456));
    writeln(getDifference(1174.8380510598456, -154146.66490124757));
    writeln(getDifference(60175.77306795546, 42213.07192354373));
  end func;
```


{{out}}

```txt

Input in -180 to +180 range
25.0
90.0
175.0
-175.0
170.0
-170.0
170.0
-170.0
-118.1184
-80.7109
Input in wider range
-139.583283123386
-72.3439185186871
-161.502952307404
37.2988555882694

```



## Sidef


```ruby
func bearingAngleDiff(b1, b2) {
    (var b = ((b2 - b1 + 720) % 360)) > 180 ? (b - 360) : b
}

printf("%25s %25s %25s\n", "B1", "B2", "Difference")
printf("%25s %25s %25s\n", "-"*20, "-"*20, "-"*20)


for b1,b2 in ([
                       20,                       45
                      -45,                       45
                      -85,                       90
                      -95,                       90
                      -45,                      125
                      -45,                      145
                  29.4803,                 -88.6381
                 -78.3251,                 -159.036
       -70099.74233810938,        29840.67437876723
       -165313.6666297357,         33693.9894517456
       1174.8380510598456,      -154146.66490124757
        60175.77306795546,        42213.07192354373
    ].slices(2)
) {
    printf("%25s %25s %25s\n", b1, b2, bearingAngleDiff(b1, b2))
}
```

{{out}}

```txt
                       B1                        B2                Difference
     --------------------      --------------------      --------------------
                       20                        45                        25
                      -45                        45                        90
                      -85                        90                       175
                      -95                        90                      -175
                      -45                       125                       170
                      -45                       145                      -170
                  29.4803                  -88.6381                 -118.1184
                 -78.3251                  -159.036                  -80.7109
       -70099.74233810938         29840.67437876723          -139.58328312339
       -165313.6666297357          33693.9894517456            -72.3439185187
       1174.8380510598456       -154146.66490124757        -161.5029523074156
        60175.77306795546         42213.07192354373            37.29885558827
```



## Swift



```swift
func angleDifference(a1: Double, a2: Double) -> Double {
  let diff = (a2 - a1).truncatingRemainder(dividingBy: 360)

  if diff < -180.0 {
    return 360.0 + diff
  } else if diff > 180.0 {
    return -360.0 + diff
  } else {
    return diff
  }
}

let testCases = [
  (20.0, 45.0),
  (-45, 45),
  (-85, 90),
  (-95, 90),
  (-45, 125),
  (-45, 145),
  (29.4803, -88.6381),
  (-78.3251, -159.036),
  (-70099.74233810938, 29840.67437876723),
  (-165313.6666297357, 33693.9894517456),
  (1174.8380510598456, -154146.66490124757),
  (60175.77306795546, 42213.07192354373)
]

print(testCases.map(angleDifference))
```


{{out}}


```txt
[25.0, 90.0, 175.0, -175.0, 170.0, -170.0, -118.1184, -80.7109, -139.58328312338563, -72.34391851868713, -161.50295230740448, 37.29885558826936]
```



## Tcl


```tcl

proc angleDiff {b1 b2} {
  set angle [::tcl::mathfunc::fmod [expr ($b2 - $b1)] 360]
  if {$angle < -180.0} {
    set angle [expr $angle + 360.0]
  }
  if {$angle >= 180.0} {
    set angle [expr $angle - 360.0]
  }
  return $angle
}

puts "Input in -180 to +180 range"
puts [angleDiff 20.0 45.0]
puts [angleDiff -45.0 45.0]
puts [angleDiff -85.0 90.0]
puts [angleDiff -95.0 90.0]
puts [angleDiff -45.0 125.0]
puts [angleDiff -45.0 145.0]
puts [angleDiff -45.0 125.0]
puts [angleDiff -45.0 145.0]
puts [angleDiff 29.4803 -88.6381]
puts [angleDiff -78.3251 -159.036]

puts "Input in wider range"
puts [angleDiff -70099.74233810938 29840.67437876723]
puts [angleDiff -165313.6666297357 33693.9894517456]
puts [angleDiff 1174.8380510598456 -154146.66490124757]
puts [angleDiff 60175.77306795546 42213.07192354373]

```

{{out}}

```txt

Input in -180 to +180 range
25.0
90.0
175.0
-175.0
170.0
-170.0
170.0
-170.0
-118.1184
-80.7109
Input in wider range
-139.58328312338563
-72.34391851868713
-161.50295230740448
37.29885558826936

```




## VBA

{{trans|Phix}}
```vb
Private Function tx(a As Variant) As String
    Dim s As String
    s = CStr(Format(a, "0.######"))
    If Right(s, 1) = "," Then
        s = Mid(s, 1, Len(s) - 1) & "       "
    Else
        i = InStr(1, s, ",")
        s = s & String$(6 - Len(s) + i, " ")
    End If
    tx = s
End Function
Private Sub test(b1 As Variant, b2 As Variant)
    Dim diff As Variant
    diff = (b2 - b1) - ((b2 - b1) \ 360) * 360
    diff = diff - IIf(diff > 180, 360, 0)
    diff = diff + IIf(diff < -180, 360, 0)
    Debug.Print Format(tx(b1), "@@@@@@@@@@@@@@@@"); Format(tx(b2), "@@@@@@@@@@@@@@@@@"); Format(tx(diff), "@@@@@@@@@@@@@@@@@")
End Sub
Public Sub angle_difference()
    Debug.Print "       b1               b2             diff"
    Debug.Print "---------------- ---------------- ----------------"
    test 20, 45
    test -45, 45
    test -85, 90
    test -95, 90
    test -45, 125
    test -45, 145
    test 29.4803, -88.6381
    test -78.3251, -159.036
    test -70099.7423381094, 29840.6743787672
    test -165313.666629736, 33693.9894517456
    test 1174.83805105985, -154146.664901248
    test 60175.7730679555, 42213.0719235437
End Sub
```
{{out}}

```txt
       b1               b2             diff
---------------- ---------------- ----------------
       20               45               25
      -45               45               90
      -85               90              175
      -95               90             -175
      -45              125              170
      -45              145             -170
       29,4803         -88,6381        -118,1184
      -78,3251        -159,036          -80,7109
   -70099,742338     29840,674379      -139,583283
  -165313,66663      33693,989452       -72,343919
     1174,838051   -154146,664901      -161,502952
    60175,773068     42213,071924        37,298856
```


## Visual Basic .NET

{{trans|C#}}

```vbnet
Module Module1

    Function Delta_Bearing(b1 As Decimal, b2 As Decimal) As Decimal
        Dim d As Decimal = 0

        ' Convert bearing to W.C.B
        While b1 < 0
            b1 += 360
        End While
        While b1 > 360
            b1 -= 360
        End While

        While b2 < 0
            b2 += 360
        End While
        While b2 > 0
            b2 -= 360
        End While

        ' Calculate delta bearing
        d = (b2 - b1) Mod 360
        ' Convert result to Q.B
        If d > 180 Then
            d -= 360
        ElseIf d < -180 Then
            d += 360
        End If

        Return d
    End Function

    Sub Main()
        ' Calculate standard test cases
        Console.WriteLine(Delta_Bearing(20, 45))
        Console.WriteLine(Delta_Bearing(-45, 45))
        Console.WriteLine(Delta_Bearing(-85, 90))
        Console.WriteLine(Delta_Bearing(-95, 90))
        Console.WriteLine(Delta_Bearing(-45, 125))
        Console.WriteLine(Delta_Bearing(-45, 145))
        Console.WriteLine(Delta_Bearing(29.4803, -88.6381))
        Console.WriteLine(Delta_Bearing(-78.3251, -159.036))

        ' Calculate optional test cases
        Console.WriteLine(Delta_Bearing(-70099.742338109383, 29840.674378767231))
        Console.WriteLine(Delta_Bearing(-165313.6666297357, 33693.9894517456))
        Console.WriteLine(Delta_Bearing(1174.8380510598456, -154146.66490124757))
        Console.WriteLine(Delta_Bearing(60175.773067955459, 42213.071923543728))
    End Sub

End Module
```

{{out}}

```txt
25
90
175
-175
170
-170
-118.1184
-80.7109
-139.5832831234
-72.3439185184
-161.50295230785
37.2988555882
```



## zkl

{{trans|Perl 6}}

```zkl
fcn bearingAngleDiff(b1,b2){  // -->Float, b1,b2 can be int or float
  ( (b:=(0.0 + b2 - b1 + 720)%360) > 180 ) and b - 360 or b;
}
```


```zkl
T( 20,45, -45,45, -85,90, -95,90, -45,125, -45,145 )
.pump(Console.println,Void.Read,
      fcn(b1,b2){ "%.1f\UB0; + %.1f\UB0; = %.1f\UB0;"
                  .fmt(b1,b2,bearingAngleDiff(b1,b2)) });
```

{{out}}

```txt

20.0° + 45.0° = 25.0°
-45.0° + 45.0° = 90.0°
-85.0° + 90.0° = 175.0°
-95.0° + 90.0° = -175.0°
-45.0° + 125.0° = 170.0°
-45.0° + 145.0° = -170.0°

```


'''References'''

<references/>
