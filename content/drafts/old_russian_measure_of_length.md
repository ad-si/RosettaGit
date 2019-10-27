+++
title = "Old Russian measure of length"
description = ""
date = 2019-06-07T15:55:05Z
aliases = []
[extra]
id = 12759
[taxonomies]
categories = []
tags = []
+++

{{draft task}}
The program should make the conversion of the old Russian measures of length in the metric and vice versa.
Is an example of the transformation of several variables that are linearly. The program accepts a single value in the selected unit, and return it in the rest: ''vershoks'', ''arshins'', ''sazhens'', ''versts'', ''meters'', ''centimeters'' and ''kilometers''.

See [https://en.wikipedia.org/wiki/Obsolete_Russian_units_of_measurement#Length Old Russian measure of length]

=={{header|МК-61/52}}==
<lang>П7	1	0	0	*	П8	1	ВП	5	/
П9	ИП7	1	0	6	7	/	П0	5	0
0	*	ПC	3	*	ПA	1	6	*	ПB
С/П	ПB	1	6	/	ПA	3	/	ПC	5
0	0	/	П0	1	0	6	7	/	БП
00
```


''Instruction:''
l, meters = РX В/О С/П; l, vershoks = БП 3 1 С/П; l, arshins = РX БП 3 5 С/П; l, sazhens = РX БП 3 8 С/П; l, versts = РX БП 4 3 С/П; РX = РB = l, vershoks; РA = l, arshins; РB = l, versts; РC = l, sazhens; Р7 = l, meters; Р8 = l, centimeters; Р9 = l, kilometers.

''Example:''
100 m = 2249,2971 vershoks = 140,58107 arshins = 46,860366 sazhens = 0,093790712 versts;
3 vershoks = 13,3375 cm; 2 sazhens = 96 vershoks = 6 arshins = 4,268 m; 1 verst = 1,067 km.


## AWK


```AWK

# syntax: GAWK -f OLD_RUSSIAN_MEASURE_OF_LENGTH.AWK
BEGIN {
    units = "kilometer meter centimeter tochka liniya diuym vershok piad fut arshin sazhen versta milia"
    values = "1000.0 1.0 0.01 0.000254 0.00254 0.0254 0.04445 0.1778 0.3048 0.7112 2.1336 1066.8 7467.6"
    u_leng = split(units,u_arr," ")
    v_leng = split(values,v_arr," ")
    if (u_leng != v_leng) {
      print("error: lengths of units & values are unequal")
      exit(1)
    }
    print("enter length & measure or C/R to exit")
}
{   if ($0 == "") {
      exit(0)
    }
    measure = tolower($2)
    sub(/s$/,"",measure)
    for (i=1; i<=u_leng; i++) {
      if (u_arr[i] == measure) {
        for (j=1; j<=u_leng; j++) {
          str = sprintf("%.8f",$1*v_arr[i]/v_arr[j])
          sub(/0+$/,"",str)
          printf("%10s %s\n",u_arr[j],str)
        }
        print("")
        next
      }
    }
    printf("error: invalid measure; choose from: %s\n\n",units)
}

```

{{out}}

```txt

enter length & measure or C/R to exit
1 meter
 kilometer 0.001
     meter 1.
centimeter 100.
    tochka 3937.00787402
    liniya 393.7007874
     diuym 39.37007874
   vershok 22.49718785
      piad 5.62429696
       fut 3.2808399
    arshin 1.40607424
    sazhen 0.46869141
    versta 0.00093738
     milia 0.00013391

1 milia
 kilometer 7.4676
     meter 7467.6
centimeter 746760.
    tochka 29400000.
    liniya 2940000.
     diuym 294000.
   vershok 168000.
      piad 42000.
       fut 24500.
    arshin 10500.
    sazhen 3500.
    versta 7.
     milia 1.

```



## BBC BASIC


```bbcbasic>REM 
oldrussian
@% = &90E
PROColdrus(1, "meter")
PRINT
PROColdrus(10, "arshin")
END
:
DEF PROColdrus(length, unit$)
LOCAL units$(), values(), unit%, i%
DIM units$(12)
DIM values(12)
units$() = "kilometer", "meter", "centimeter", "milia", "versta", "sazhen", "arshin", "fut", "piad", "vershok", "diuym", "liniya", "tochka"
values() = 1000, 1, 0.01, 7467.6, 1066.8, 2.1336, 0.7112, 0.3048, 0.1778, 0.04445, 0.0254, 0.00254, 0.000254
unit% = -1
FOR i% = 0 TO 12
  IF units$(i%) = unit$ THEN unit% = i%
NEXT
IF unit% = -1 THEN
  PRINT "Unknown unit '"; unit$; "'"
ELSE
  PRINT; length; " "; unit$; " ="
  FOR i% = 0 TO 12
    IF i% <> unit% THEN PRINT length / values(i%) * values(unit%); " "; units$(i%)
  NEXT
ENDIF
ENDPROC
```

{{out}}

```txt
1 meter =
         0.001 kilometer
           100 centimeter
0.000133911832 milia
0.000937382827 versta
   0.468691414 sazhen
    1.40607424 arshin
     3.2808399 fut
    5.62429696 piad
    22.4971879 vershok
    39.3700787 diuym
    393.700787 liniya
    3937.00787 tochka

10 arshin =
      0.007112 kilometer
         7.112 meter
         711.2 centimeter
0.000952380952 milia
 0.00666666667 versta
    3.33333333 sazhen
    23.3333333 fut
            40 piad
           160 vershok
           280 diuym
          2800 liniya
         28000 tochka
```



## C

Accepts length and unit as input, prints out length in all other units. Usage printed on incorrect invocation. 

```C

#include<string.h>
#include<stdlib.h>
#include<ctype.h>
#include<stdio.h>

#define UNITS_LENGTH 13

int main(int argC,char* argV[])
{
	int i,reference;
	char *units[UNITS_LENGTH] = {"kilometer","meter","centimeter","tochka","liniya","diuym","vershok","piad","fut","arshin","sazhen","versta","milia"};
    double factor, values[UNITS_LENGTH] = {1000.0,1.0,0.01,0.000254,0.00254,0.0254,0.04445,0.1778,0.3048,0.7112,2.1336,1066.8,7467.6};
	
	if(argC!=3)
		printf("Usage : %s followed by length as <value> <unit>");
	else{
		for(i=0;argV[2][i]!=00;i++)
			argV[2][i] = tolower(argV[2][i]);
		
		for(i=0;i<UNITS_LENGTH;i++){
			if(strstr(argV[2],units[i])!=NULL){
				reference = i;
				factor = atof(argV[1])*values[i];
				break;
			}
		}
		
		printf("%s %s is equal in length to : \n",argV[1],argV[2]);
		
		for(i=0;i<UNITS_LENGTH;i++){
			if(i!=reference)
				printf("\n%lf %s",factor/values[i],units[i]);
		}
	}
	
	return 0;
}

```

Output :

```txt

C:\rosettaCode>metricRussian.exe 1 meter
1 meter is equal in length to :

0.001000 kilometer
100.000000 centimeter
3937.007874 tochka
393.700787 liniya
39.370079 diuym
22.497188 vershok
5.624297 piad
3.280840 fut
1.406074 arshin
0.468691 sazhen
0.000937 versta
0.000134 milia
C:\rosettaCode>metricRussian.exe 3 arshin
3 arshin is equal in length to :

0.002134 kilometer
2.133600 meter
213.360000 centimeter
8400.000000 tochka
840.000000 liniya
84.000000 diuym
48.000000 vershok
12.000000 piad
7.000000 fut
1.000000 sazhen
0.002000 versta
0.000286 milia

```



## C++


```cpp

#include <iostream>
#include <iomanip>
//-------------------------------------------------------------------------------------------
using namespace std;

//-------------------------------------------------------------------------------------------
class ormConverter
{
public:
    ormConverter() :  AR( 0.7112f ), CE( 0.01f ), DI( 0.0254f ), FU( 0.3048f ), KI( 1000.0f ), LI( 0.00254f ), ME( 1.0f ),
		      MI( 7467.6f ), PI( 0.1778f ), SA( 2.1336f ), TO( 0.000254f ), VE( 0.04445f ), VR( 1066.8f ) {}
    void convert( char c, float l )
    {
	system( "cls" );
	cout << endl << l;
	switch( c )
	{
	    case 'A': cout << " Arshin to:";     l *= AR; break;
	    case 'C': cout << " Centimeter to:"; l *= CE; break;
	    case 'D': cout << " Diuym to:";      l *= DI; break;
	    case 'F': cout << " Fut to:";        l *= FU; break;
	    case 'K': cout << " Kilometer to:";  l *= KI; break;
	    case 'L': cout << " Liniya to:";     l *= LI; break;
	    case 'M': cout << " Meter to:";      l *= ME; break;
	    case 'I': cout << " Milia to:";      l *= MI; break;
	    case 'P': cout << " Piad to:";       l *= PI; break;
	    case 'S': cout << " Sazhen to:";     l *= SA; break;
	    case 'T': cout << " Tochka to:";     l *= TO; break;
	    case 'V': cout << " Vershok to:";    l *= VE; break;
	    case 'E': cout << " Versta to:";     l *= VR;
	}

	float ar = l / AR, ce = l / CE, di = l / DI, fu = l / FU, ki = l / KI, li = l / LI, me = l / ME,
	      mi = l / MI, pi = l / PI, sa = l / SA, to = l / TO, ve = l / VE, vr = l / VR;
	cout << left << endl << "
### ===========
" << endl
	     << setw( 12 ) << "Arshin:" << ar << endl << setw( 12 ) << "Centimeter:" << ce << endl
	     << setw( 12 ) << "Diuym:" << di << endl << setw( 12 ) << "Fut:" << fu << endl
	     << setw( 12 ) << "Kilometer:" << ki << endl << setw( 12 ) << "Liniya:" << li << endl
	     << setw( 12 ) << "Meter:" << me << endl << setw( 12 ) << "Milia:" << mi << endl
	     << setw( 12 ) << "Piad:" << pi << endl << setw( 12 ) << "Sazhen:" << sa << endl
	     << setw( 12 ) << "Tochka:" << to << endl << setw( 12 ) << "Vershok:" << ve << endl
	     << setw( 12 ) << "Versta:" << vr << endl << endl << endl;
    }
private:
    const float AR, CE, DI, FU, KI, LI, ME, MI, PI, SA, TO, VE, VR;
};
//-------------------------------------------------------------------------------------------
int _tmain(int argc, _TCHAR* argv[])
{
    ormConverter c;
    char s; float l;
    while( true )
    {
	cout << "What unit:\n(A)rshin, (C)entimeter, (D)iuym, (F)ut\n(K)ilometer, (L)iniya, (M)eter, m(I)lia, (P)iad\n(S)azhen, (T)ochka, (V)ershok, v(E)rsta, (Q)uit\n";
	cin >> s; if( s & 32 ) s ^= 32; if( s == 'Q' ) return 0;
	cout << "Length (0 to Quit): "; cin >> l; if( l == 0 ) return 0;
	c.convert( s, l ); system( "pause" ); system( "cls" );
    }
    return 0;
}
//-------------------------------------------------------------------------------------------

```

Output:

```txt

1 Milia to:

### ===========

Arshin:     10500
Centimeter: 746760
Diuym:      294000
Fut:        24500
Kilometer:  7.4676
Liniya:     2.94e+006
Meter:      7467.6
Milia:      1
Piad:       42000
Sazhen:     3500
Tochka:     2.94e+007
Vershok:    168000
Versta:     7

1 Meter to:

### ===========

Arshin:     1.40607
Centimeter: 100
Diuym:      39.3701
Fut:        3.28084
Kilometer:  0.001
Liniya:     393.701
Meter:      1
Milia:      0.000133912
Piad:       5.6243
Sazhen:     0.468691
Tochka:     3937.01
Vershok:    22.4972
Versta:     0.000937383

```



## D

{{trans|Perl 6}}

```d
import std.stdio, std.string, std.algorithm, std.conv;

void main(in string[] args) {
    auto factor = ["arshin":         0.7112,
                   "centimeter":     0.01,
                   "diuym":          0.0254,
                   "fut":            0.3048,
                   "kilometer":  1_000.0,
                   "liniya":         0.00254,
                   "meter":          1.0,
                   "milia":      7_467.6,
                   "piad":           0.1778,
                   "sazhen":         2.1336,
                   "tochka":         0.000254,
                   "vershok":        0.04445,
                   "versta":     1_066.8];

    if (args.length != 3 || !isNumeric(args[1]) || args[2] !in factor)
        return writeln("Please provide args Value and Unit.");

    immutable magnitude = args[1].to!double;
    immutable meters = magnitude * factor[args[2]];
    writefln("%s %s to:\n", args[1], args[2]);
    foreach (immutable key; factor.keys.schwartzSort!(k => factor[k]))
       writefln("%10s: %s", key, meters / factor[key]);
}
```

{{out}}

```txt
1 meter to:

    tochka: 3937.01
    liniya: 393.701
centimeter: 100
     diuym: 39.3701
   vershok: 22.4972
      piad: 5.6243
       fut: 3.28084
    arshin: 1.40607
     meter: 1
    sazhen: 0.468691
 kilometer: 0.001
    versta: 0.000937382
     milia: 0.000133911

1 milia to:

    tochka: 2.94e+07
    liniya: 2.94e+06
centimeter: 746760
     diuym: 294000
   vershok: 168000
      piad: 42000
       fut: 24500
    arshin: 10500
     meter: 7467.6
    sazhen: 3500
 kilometer: 7.4676
    versta: 7
     milia: 1
```



## Factor

This solution makes use of Factor's rather robust <code>units</code> vocabulary. Units may be defined in terms of any unit and converted to any unit through the power of inverse functions (via the <code>inverse</code> vocabulary). This means that we also have access to a variety of units defined in <code>units.si</code>, <code>units.imperial</code>, etc.

```factor
USING: formatting inverse io kernel math prettyprint quotations
sequences units.imperial units.si vocabs ;
IN: rosetta-code.units.russian

: arshin  ( n -- d ) 2+1/3 * feet ;
: tochka  ( n -- d ) 1/2800 * arshin ;
: liniya  ( n -- d ) 1/280 * arshin ;
: dyuim   ( n -- d ) 1/28 * arshin ;
: vershok ( n -- d ) 1/16 * arshin ;
: ladon   ( n -- d ) 7+1/2 * cm ;
: piad    ( n -- d ) 1/4 * arshin ;
: fut     ( n -- d ) 3/7 * arshin ;
: lokot   ( n -- d ) 45 * cm ;
: shag    ( n -- d ) 71 * cm ;
: sazhen  ( n -- d ) 3 * arshin ;
: versta  ( n -- d ) 1,500 * arshin ;
: milya   ( n -- d ) 10,500 * arshin ;

<PRIVATE

: convert ( quot -- )
    [ unparse rest rest but-last write "to:" print ] [ call ] bi
    "rosetta-code.units.russian" vocab-words { cm m km } append
    [ dup "%8u : " printf 1quotation [undo] call( x -- x ) . ]
    with each nl ; inline

: main ( -- )
    [ 6 m ] [ 1+7/8 milya ] [ 2 furlongs ] [ convert ] tri@ ;

PRIVATE>

MAIN: main
```

{{out}}

```txt

6 m to:
  versta : 5/889
 vershok : 134+874/889
    shag : 8+32/71
  tochka : 23622+6/127
     fut : 19+87/127
   ladon : 80
    piad : 33+663/889
  sazhen : 2+722/889
   lokot : 13+1/3
   milya : 5/6223
   dyuim : 236+28/127
  liniya : 2362+26/127
  arshin : 8+388/889
      cm : 600
       m : 6
      km : 3/500

1+7/8 milya to:
  versta : 13+1/8
 vershok : 315000
    shag : 19720+55/71
  tochka : 55125000
     fut : 45937+1/2
   ladon : 186690
    piad : 78750
  sazhen : 6562+1/2
   lokot : 31115
   milya : 1+7/8
   dyuim : 551250
  liniya : 5512500
  arshin : 19687+1/2
      cm : 1400175
       m : 14001+3/4
      km : 14+7/4000

2 furlongs to:
  versta : 66/175
 vershok : 9051+3/7
    shag : 566+238/355
  tochka : 1584000
     fut : 1320
   ladon : 5364+12/25
    piad : 2262+6/7
  sazhen : 188+4/7
   lokot : 894+2/25
   milya : 66/1225
   dyuim : 15840
  liniya : 158400
  arshin : 565+5/7
      cm : 40233+3/5
       m : 402+42/125
      km : 12573/31250

```



## Fortran


```fortran
PROGRAM RUS
 IMPLICIT NONE
 REAL, PARAMETER:: E_m = 1.
 REAL, PARAMETER:: E_mm = 1.E-3
 REAL, PARAMETER:: E_km = 1.E+3
 REAL, PARAMETER:: E_cm = 1.E-2
 REAL, PARAMETER:: E_arshin = 71.12 * E_cm
 REAL, PARAMETER:: E_fut = 3./7. * E_arshin
 REAL, PARAMETER:: E_piad = 1./4. * E_arshin
 REAL, PARAMETER:: E_vershok = 1./16. * E_arshin
 REAL, PARAMETER:: E_dyuim = 1./28. * E_arshin
 REAL, PARAMETER:: E_liniya = 1./280. * E_arshin
 REAL, PARAMETER:: E_tochka = 1./2800. * E_arshin
 REAL, PARAMETER:: E_ladon = 7.5 * E_cm
 REAL, PARAMETER:: E_lokot = 45 * E_cm
 REAL, PARAMETER:: E_sazhen = 3. * E_arshin
 REAL, PARAMETER:: E_versta = 1500. * E_arshin
 REAL, PARAMETER:: E_milya = 10500. * E_arshin
 INTEGER, PARAMETER:: N = 16
 CHARACTER(LEN=7), DIMENSION(N):: nam = (/&
  &'m      ', 'mm     ', 'km     ', 'cm     ',&
  &'arshin ', 'fut    ', 'piad   ', 'vershok',&
  &'dyuim  ', 'liniya ', 'tochka ', 'ladon  ',&
  &'lokot  ', 'sazhen ', 'versta ', 'milya  ' /)
 REAL, DIMENSION(N):: wert = (/ &
  &1., E_mm, E_km, E_cm,&
  &E_arshin, E_fut, E_piad, E_vershok,&
  &E_dyuim, E_liniya, E_tochka, E_ladon,&
  &E_lokot, E_sazhen, E_versta, E_milya /)
 CHARACTER(LEN=7):: RD_U
 REAL:: RD_V
 INTEGER:: I, J
 DO I=1, N
  WRITE(*, '(A, " ")', ADVANCE='NO')  nam(I)
 END DO
 WRITE (*, *)
 WRITE(*, '(A)', ADVANCE='NO') 'value unit -> '
 READ(*, *) RD_V, RD_U
 RD_U = ADJUSTL(RD_U)
 J = 1
 DO WHILE (NAM(J) .NE. RD_U)
  J = J + 1
  IF (J .GT. N) STOP "Unit not known: "//RD_U
 END DO
 RD_V = RD_V * wert(J)
 DO I=1, N
  J = J + 1
  IF (J .GT. N) J = 1
  WRITE (*, '(F20.3, " ", A)') RD_V / wert(J), nam(J)
 END DO
END PROGRAM RUS
```

{{out}}

```txt
m       mm      km      cm      arshin  fut     piad    vershok dyuim   liniya  tochka  ladon   lokot   sazhen  versta  milya   
value unit -> 1 m
            1000.000 mm     
               0.001 km     
             100.000 cm     
               1.406 arshin 
               3.281 fut    
               5.624 piad   
              22.497 vershok
              39.370 dyuim  
             393.701 liniya 
            3937.008 tochka 
              13.333 ladon  
               2.222 lokot  
               0.469 sazhen 
               0.001 versta 
               0.000 milya  
               1.000 m 

m       mm      km      cm      arshin  fut     piad    vershok dyuim   liniya  tochka  ladon   lokot   sazhen  versta  milya   
value unit -> 10 arshin
              23.333 fut    
              40.000 piad   
             160.000 vershok
             280.000 dyuim  
            2800.000 liniya 
           28000.002 tochka 
              94.827 ladon  
              15.804 lokot  
               3.333 sazhen 
               0.007 versta 
               0.001 milya  
               7.112 m      
            7112.000 mm     
               0.007 km     
             711.200 cm     
              10.000 arshin
```



## FreeBASIC


```freebasic
' FB 1.05.0 Win64

Dim units(1 To 13) As String = {"tochka", "liniya", "dyuim", "vershok", "piad", "fut", _
                                "arshin", "sazhen", "versta", "milia", _
                                "centimeter", "meter", "kilometer"}

' all expressed in centimeters
Dim convs(1 To 13) As Single = {0.0254, 0.254, 2.54, 4.445, 17.78, 30.48, _
                                71.12, 213.36, 10668, 74676, _
                                1, 100, 10000}
Dim unit As Integer
Dim value As Single
Dim yn As String

Do
  Shell("cls")
  Print 
  For i As Integer = 1 To 13
    Print Using "##"; i;
    Print " "; units(i)
  Next
  Print
  Do
    Input "Please choose a unit 1 to 13 : "; unit
  Loop Until unit >= 1 AndAlso unit <= 13  
  Print
  Do
    Input "Now enter a value in that unit : "; value
  Loop Until value >= 0 
  Print
  Print "The equivalent in the remaining units is : "
  Print
  For i As Integer = 1 To 13
    If i = unit Then Continue For
    Print " "; units(i), " : "; value * convs(unit) / convs(i)
  Next
  Print
  Do
    Input "Do another one y/n : "; yn
    yn = LCase(yn)
  Loop Until yn = "y" OrElse yn = "n"
Loop Until yn = "n"
     
End
```

Sample input/output:
{{out}}

```txt


 1 tochka
 2 liniya
 3 dyuim
 4 vershok
 5 piad
 6 fut
 7 arshin
 8 sazhen
 9 versta
10 milia
11 centimeter
12 meter
13 kilometer

Please choose a unit 1 to 13 : ? 13

Now enter a value in that unit : ? 1

The equivalent in the remaining units is :

 tochka        :  393700.8
 liniya        :  39370.08
 dyuim         :  3937.008
 vershok       :  2249.719
 piad          :  562.4297
 fut           :  328.084
 arshin        :  140.6074
 sazhen        :  46.86914
 versta        :  0.9373828
 milia         :  0.1339118
 centimeter    :  10000
 meter         :  100

Do another one y/n : ? n

```



## Go

{{trans|Kotlin}}

```go
package main

import (
    "bufio"
    "fmt"
    "os"
    "strconv"
    "strings"
)

func main() {
    units := []string{
        "tochka", "liniya", "dyuim", "vershok", "piad", "fut",
        "arshin", "sazhen", "versta", "milia",
        "centimeter", "meter", "kilometer",
    }

    convs := []float32{
        0.0254, 0.254, 2.54, 4.445, 17.78, 30.48,
        71.12, 213.36, 10668, 74676,
        1, 100, 10000,
    }

    scanner := bufio.NewScanner(os.Stdin)
    for {
        for i, u := range units {
            fmt.Printf("%2d %s\n", i+1, u)
        }
        fmt.Println()
        var unit int
        var err error
        for {
            fmt.Print("Please choose a unit 1 to 13 : ")
            scanner.Scan()
            unit, err = strconv.Atoi(scanner.Text())
            if err == nil && unit >= 1 && unit <= 13 {
                break
            }
        }
        unit--
        var value float64
        for {
            fmt.Print("Now enter a value in that unit : ")
            scanner.Scan()
            value, err = strconv.ParseFloat(scanner.Text(), 32)
            if err == nil && value >= 0 {
                break
            }
        }
        fmt.Println("\nThe equivalent in the remaining units is:\n")
        for i, u := range units {
            if i == unit {
                continue
            }
            fmt.Printf(" %10s : %g\n", u, float32(value)*convs[unit]/convs[i])
        }
        fmt.Println()
        yn := ""
        for yn != "y" && yn != "n" {
            fmt.Print("Do another one y/n : ")
            scanner.Scan()
            yn = strings.ToLower(scanner.Text())
        }
        if yn == "n" {
            return
        }
    }
}
```


{{out}}
Sample input/output:

```txt

 1 tochka
 2 liniya
 3 dyuim
 4 vershok
 5 piad
 6 fut
 7 arshin
 8 sazhen
 9 versta
10 milia
11 centimeter
12 meter
13 kilometer

Please choose a unit 1 to 13 : 13
Now enter a value in that unit : 1

The equivalent in the remaining units is:

     tochka : 393700.78
     liniya : 39370.08
      dyuim : 3937.0078
    vershok : 2249.7188
       piad : 562.4297
        fut : 328.08398
     arshin : 140.60742
     sazhen : 46.86914
     versta : 0.9373828
      milia : 0.13391183
 centimeter : 10000
      meter : 100

Do another one y/n : n

```



## Haskell



```Haskell
module Main where

import Text.Printf (printf)
import System.Environment (getArgs, getProgName)

tochka     = ("tochka"    , 0.000254)
liniya     = ("liniya"    , 0.00254)
centimeter = ("centimeter", 0.01)
diuym      = ("diuym"     , 0.0254)
vershok    = ("vershok"   , 0.04445)
piad       = ("piad"      , 0.1778)
fut        = ("fut"       , 0.3048)
arshin     = ("arshin"    , 0.7112)
meter      = ("meter"     , 1.0)
sazhen     = ("sazhen"    , 2.1336)
kilometer  = ("kilometer" , 1000.0)
versta     = ("versta"    , 1066.8)
milia      = ("milia"     , 7467.6)

units :: [(String, Double)]
units = [tochka, liniya, centimeter, diuym, vershok, piad, fut, arshin, meter, sazhen, kilometer, versta, milia]


convert :: Double -> Double -> IO ()
convert num factor = mapM_ (\(unit, fac) -> printf "| %-10s | %-22f|\n" unit  (num * factor / fac)) units

main :: IO ()
main = do
  args <- getArgs
  case args of
    [x,y] | [(num, "")]   <- reads x :: [(Double, String)]
          , (Just factor) <- lookup y units -> convert num factor
    (_) -> do
      name <- getProgName
      printf "Arguments were wrong - please use ./%s <number> <unit>\n" name
```


Output:


```txt


./ruslen 8 meter

| tochka     | 31496.062992125986    |
| liniya     | 3149.606299212598     |
| centimeter | 800.0                 |
| diuym      | 314.96062992125985    |
| vershok    | 179.97750281214846    |
| piad       | 44.994375703037115    |
| fut        | 26.246719160104984    |
| arshin     | 11.248593925759279    |
| meter      | 8.0                   |
| sazhen     | 3.749531308586427     |
| kilometer  | 0.008                 |
| versta     | 0.0074990626171728535 |
| milia      | 0.0010712946595961218 |

./ruslen 1 arshin

| tochka     | 2800.0000000000005    |
| liniya     | 280.0                 |
| centimeter | 71.12                 |
| diuym      | 28.000000000000004    |
| vershok    | 16.0                  |
| piad       | 4.0                   |
| fut        | 2.3333333333333335    |
| arshin     | 1.0                   |
| meter      | 0.7112                |
| sazhen     | 0.33333333333333337   |
| kilometer  | 0.0007112             |
| versta     | 0.0006666666666666668 |
| milia      | 0.00009523809523809524|

./ruslen 8 notaunit

Arguments were wrong - please use ./ruslen <number> <unit>


./ruslen notanumber meter

Arguments were wrong - please use ./ruslen <number> <unit>


```



## J

Translation of python.

```J

NB. Use, linux.
NB. $ /usr/local/j64-801/bin/jconsole j.ijs 8 meter

UNIT2MULT =: /:~ _ ".&.>@:{:@:]`1:`]}"1<;._2@:(,&':');._2 'arshin:0.7112,centimeter:0.01,diuym:0.0254,fut:0.3048,kilometer:1000.0,liniya:0.00254,meter:1.0,milia:7467.6,piad:0.1778,sazhen:2.1336,tochka:0.000254,vershok:0.04445,versta:1066.8,'

exit 3 : 0 :: 1: a: -.~ _3 {. ARGV
 if. 3 ~: # y do. smoutput 'ERROR. Need two arguments - number then units'
 else.
  VALUE =: _ ". _2 {:: y
  if. _ = | VALUE do. smoutput 'ERROR. First argument must be a (float) number'
  else.
   UNIT =: {: y
   UNITS =: 0&{"1 UNIT2MULT
   if. UNIT-.@:e.UNITS do. smoutput 'ERROR. Only know the following units: ' , deb , ,&' '&> UNITS
   else.
    smoutput deb(,,&' '&>_2{.y),'to:'
    smoutput UNITS ,. (VALUE * (< 1 ,~ UNITS i. UNIT) {:: UNIT2MULT) %&.> {:"1 UNIT2MULT
   end.
  end.
 end.
)

```


```txt

$ /usr/local/j64-801/bin/jconsole j.ijs 8 meter
8 meter to:
┌──────────┬──────────┐
│arshin    │11.2486   │
├──────────┼──────────┤
│centimeter│800       │
├──────────┼──────────┤
│diuym     │314.961   │
├──────────┼──────────┤
│fut       │26.2467   │
├──────────┼──────────┤
│kilometer │0.008     │
├──────────┼──────────┤
│liniya    │3149.61   │
├──────────┼──────────┤
│meter     │8         │
├──────────┼──────────┤
│milia     │0.00107129│
├──────────┼──────────┤
│piad      │44.9944   │
├──────────┼──────────┤
│sazhen    │3.74953   │
├──────────┼──────────┤
│tochka    │31496.1   │
├──────────┼──────────┤
│vershok   │179.978   │
├──────────┼──────────┤
│versta    │0.00749906│
└──────────┴──────────┘
$ 

```



## Java


```java
public class OldRussianMeasures {

    final static String[] keys = {"tochka", "liniya", "centimeter", "diuym",
        "vershok", "piad", "fut", "arshin", "meter", "sazhen", "kilometer",
        "versta", "milia"};

    final static double[] values = {0.000254, 0.00254, 0.01,0.0254,
        0.04445, 0.1778, 0.3048, 0.7112, 1.0, 2.1336, 1000.0,
        1066.8, 7467.6};

    public static void main(String[] a) {
        if (a.length == 2 && a[0].matches("[+-]?\\d*(\\.\\d+)?")) {
            double inputVal = lookup(a[1]);
            if (!Double.isNaN(inputVal)) {
                double magnitude = Double.parseDouble(a[0]);
                double meters = magnitude * inputVal;
                System.out.printf("%s %s to: %n%n", a[0], a[1]);
                for (String k: keys)
                    System.out.printf("%10s: %g%n", k, meters / lookup(k));
                return;
            }
        }
        System.out.println("Please provide a number and unit");

    }

    public static double lookup(String key) {
        for (int i = 0; i < keys.length; i++)
            if (keys[i].equals(key))
                return values[i];
        return Double.NaN;
    }
}
```



```txt
1 meter to: 

    tochka: 3937,01
    liniya: 393,701
centimeter: 100,000
     diuym: 39,3701
   vershok: 22,4972
      piad: 5,62430
       fut: 3,28084
    arshin: 1,40607
     meter: 1,00000
    sazhen: 0,468691
 kilometer: 0,00100000
    versta: 0,000937383
     milia: 0,000133912

1 milia to: 

    tochka: 2,94000e+07
    liniya: 2,94000e+06
centimeter: 746760
     diuym: 294000
   vershok: 168000
      piad: 42000,0
       fut: 24500,0
    arshin: 10500,0
     meter: 7467,60
    sazhen: 3500,00
 kilometer: 7,46760
    versta: 7,00000
     milia: 1,00000
```



## Julia

{{works with|Julia|0.6}}
{{trans|Python}}


```julia
using DataStructures

const unit2mult = Dict(
    "arshin" => 0.7112, "centimeter" => 0.01,     "diuym"   => 0.0254,
    "fut"    => 0.3048, "kilometer"  => 1000.0,   "liniya"  => 0.00254,
    "meter"  => 1.0,    "milia"      => 7467.6,   "piad"    => 0.1778,
    "sazhen" => 2.1336, "tochka"     => 0.000254, "vershok" => 0.04445,
    "versta" => 1066.8)

@assert length(ARGS) == 2 "need two arguments - number then units"

global value
try value = parse(Float64, ARGS[1])
catch error("first argument must be a (float) number") end

if isnull(value) error("first argument must be a (float) number") end
unit = ARGS[2]
@assert unit ∈ keys(unit2mult) "only know the following units:\n" * join(keys(unit2mult), ", ")

println("$value $unit to:")
for (unt, mlt) in sort(unit2mult)
    @printf("  %10s: %g\n", unt, value * unit2mult[unit] / mlt)
end
```


{{out}}

```txt
$ julia Old_Russian_measure_of_length.jl 2.3 meter
2.3 meter to:
      arshin: 3.23397
  centimeter: 230
       diuym: 90.5512
         fut: 7.54593
   kilometer: 0.0023
      liniya: 905.512
       meter: 2.3
       milia: 0.000307997
        piad: 12.9359
      sazhen: 1.07799
      tochka: 9055.12
     vershok: 51.7435
      versta: 0.00215598
```



## Kotlin

{{trans|FreeBASIC}}

```scala
// version 1.0.6

/* clears console on Windows 10 */
fun cls() = ProcessBuilder("cmd", "/c", "cls").inheritIO().start().waitFor()

fun main(args: Array<String>) {
    val units = listOf("tochka", "liniya", "dyuim", "vershok", "piad", "fut",
                       "arshin", "sazhen", "versta", "milia",
                       "centimeter", "meter", "kilometer")
    val convs = arrayOf(0.0254f, 0.254f, 2.54f, 4.445f, 17.78f, 30.48f, 
                        71.12f, 213.36f, 10668.0f, 74676.0f, 
                        1.0f, 100.0f, 10000.0f)
    var unit: Int
    var value: Float
    var yn : String
    do {
        cls()
        println()
        for (i in 0 until units.size) println("${"%2d".format(i + 1)} ${units[i]}")
        println()
        do {
            print("Please choose a unit 1 to 13 : ")
            unit = try { readLine()!!.toInt() } catch (e: NumberFormatException) { 0 }
        }
        while (unit !in 1..13)
        unit--
        do {
            print("Now enter a value in that unit : ")
            value = try { readLine()!!.toFloat() } catch (e: NumberFormatException) { -1.0f }
        }
        while (value < 0.0f)
        println("\nThe equivalent in the remaining units is:\n")
        for (i in 0 until units.size) {
            if (i == unit) continue            
            println(" ${units[i].padEnd(10)} : ${value * convs[unit] / convs[i]}")
        }
        println()
        do {
            print("Do another one y/n : ")
            yn = readLine()!!.toLowerCase()
        }
        while (yn != "y" && yn != "n")            
    }
    while (yn == "y")
}
```

Sample input/output
{{out}}

```txt


 1 tochka
 2 liniya
 3 dyuim
 4 vershok
 5 piad
 6 fut
 7 arshin
 8 sazhen
 9 versta
10 milia
11 centimeter
12 meter
13 kilometer

Please choose a unit 1 to 13 : 13
Now enter a value in that unit : 1

The equivalent in the remaining units is:

 tochka     : 393700.78
 liniya     : 39370.08
 dyuim      : 3937.0078
 vershok    : 2249.7188
 piad       : 562.4297
 fut        : 328.08398
 arshin     : 140.60742
 sazhen     : 46.86914
 versta     : 0.9373828
 milia      : 0.13391183
 centimeter : 10000.0
 meter      : 100.0

Do another one y/n : n

```



## Perl

Displaying converted values to 5 significant digits.
{{trans|Perl 6}}

```perl
sub convert {
    my($magnitude, $unit) = @_;
     my %factor = (
        tochka     => 0.000254,
        liniya     => 0.00254,
        diuym      => 0.0254,
        vershok    => 0.04445,
        piad       => 0.1778,
        fut        => 0.3048,
        arshin     => 0.7112,
        sazhen     => 2.1336,
        versta     => 1066.8,
        milia      => 7467.6,
        centimeter => 0.01,
        meter      => 1.0,
        kilometer  => 1000.0,
    );

    my $base= $magnitude * $factor{$unit};
    my $result .= "$magnitude $unit to:\n";
    for (sort { $factor{$a} <=> $factor{$b} } keys %factor) {
        $result .= sprintf "%10s: %s\n", $_, sigdig($base / $factor{$_}, 5) unless $_ eq $unit
    }
    return $result;
}

sub sigdig {
    my($num,$sig) = @_;
    return $num unless $num =~ /\./;

    $num =~ /([1-9]\d*\.?\d*)/;
    my $prefix = $`;
    my $match  = $&;
    $sig++ if $match =~ /\./;
    my $digits = substr $match, 0, $sig;
    my $nextd  = substr $match, $sig, 1;
    $digits =~ s/(.)$/{1+$1}/e if $nextd > 5;
    return $prefix . $digits;
}

print convert(1,'meter'), "\n\n";
print convert(1,'milia'), "\n";
```

{{out}}

```txt
    tochka: 3937.0
    liniya: 393.70
centimeter: 100
     diuym: 39.370
   vershok: 22.497
      piad: 5.6243
       fut: 3.2808
    arshin: 1.4061
    sazhen: 0.46869
 kilometer: 0.001
    versta: 0.00093738
     milia: 0.00013391

1 milia to:
    tochka: 29400000
    liniya: 2940000
centimeter: 746760
     diuym: 294000
   vershok: 168000
      piad: 42000
       fut: 24500
    arshin: 10500
     meter: 7467.6
    sazhen: 3500
 kilometer: 7.4676
    versta: 7
```



## Perl 6

{{Works with|rakudo|2015.12}}
Fairly straightfoward. Define a hash of conversion factors then apply them. Makes no attempt to do correct pluralization because I have no idea what the correct plurals are and little interest in researching them. Conversion factors from Wikipedia: [[wp:Obsolete_Russian_units_of_measurement#Length|Obsolete Russian units of measurement]].


```perl6
convert(1, 'meter');

say '*' x 40, "\n";

convert(1, 'milia');

sub convert (Real $magnitude, $unit) {
     my %factor = 
        tochka     => 0.000254,
        liniya     => 0.00254,
        diuym      => 0.0254,
        vershok    => 0.04445,
        piad       => 0.1778,
        fut        => 0.3048,
        arshin     => 0.7112,
        sazhen     => 2.1336,
        versta     => 1066.8,
        milia      => 7467.6,
        centimeter => 0.01,
        meter      => 1.0,
        kilometer  => 1000.0,
    ;

    my $meters = $magnitude * %factor{$unit.lc};

    say "$magnitude $unit to:\n", '_' x 40;

    printf "%10s: %s\n", $_,  $meters / %factor{$_} unless $_ eq $unit.lc
      for %factor.keys.sort:{ +%factor{$_} }
}

```



```txt

1 meter to:
________________________________________
    tochka: 3937.007874
    liniya: 393.700787
centimeter: 100
     diuym: 39.370079
   vershok: 22.497188
      piad: 5.624297
       fut: 3.280840
    arshin: 1.406074
    sazhen: 0.468691
 kilometer: 0.001
    versta: 0.000937
     milia: 0.000134
****************************************

1 milia to:
________________________________________
    tochka: 29400000
    liniya: 2940000
centimeter: 746760
     diuym: 294000
   vershok: 168000
      piad: 42000
       fut: 24500
    arshin: 10500
     meter: 7467.6
    sazhen: 3500
 kilometer: 7.4676
    versta: 7

```



## Phix


```Phix
constant units = {{"-- metric ---",0},
                  {"kilometer",1000},
                  {"km","kilometer"},
                  {"meter",1},
                  {"m","meter"},
                  {"centimeter",0.01},
                  {"cm","centimeter"},
                  {" old russian ",0},
                  {"tochka",0.000254},
                  {"liniya",0.00254},
                  {"diuym",0.0254},
                  {"vershok",0.04445},
                  {"piad",0.1778},
                  {"fut",0.3048},
                  {"arshin",0.7112},
                  {"sazhen",2.1336},
                  {"versta",1066.8},
                  {"milia",7467.6}},
        {names,facts} = columnize(units)

function strim(atom v)
    string res = sprintf("%,f",v)
    integer l = length(res)
    while l do
        integer c = res[l]
        if c!='0' then
            l -= 1+(c='.')
            exit
        end if
        l -= 1
    end while
    res = res[1..l+1]
    return res
end function

while true do
    string input = prompt_string("\nEnter length & measure or CR to exit:")
    if input="" then exit end if
    input = lower(trim(input))
    string fmt = iff(find(' ',input)?"%f %s":"%f%s")
    sequence r = scanf(input,fmt)
    if length(r)!=1 then
        printf(1,"enter eg 1km or 1 kilometer\n")
    else
        {atom v, string name} = r[1]
        integer k = find(name,names)
        if k=0 or facts[k]=0 then
            printf(1,"unrecognised unit: %s\n",{name})
        else
            if string(facts[k]) then
                -- abbreviation, eg cm->centimeter
                k = find(facts[k],names)
            end if
            for i=1 to length(names) do
                object f = facts[i]
                if f=0 then             -- header
                    printf(1,"--------------%s--------------\n",{names[i]})
                elsif atom(facts[i]) then -- not abbrev
                    printf(1,"%20s %s\n",{strim(v*facts[k]/facts[i]),names[i]})
                end if
            end for
        end if
    end if
end while
```

{{out}}

```txt

Enter length & measure or CR to exit:7.4676 km
---------------- metric -----------------
              7.4676 kilometer
             7,467.6 meter
             746,760 centimeter
-------------- old russian --------------
          29,400,000 tochka
           2,940,000 liniya
             294,000 diuym
             168,000 vershok
              42,000 piad
              24,500 fut
              10,500 arshin
               3,500 sazhen
                   7 versta
                   1 milia

Enter length & measure or CR to exit:

```



## Python

Run as:
:<code>commandname <value> <unit></code>


```python
from sys import argv
 
unit2mult = {"arshin": 0.7112, "centimeter": 0.01,     "diuym":   0.0254,
             "fut":    0.3048, "kilometer":  1000.0,   "liniya":  0.00254,
             "meter":  1.0,    "milia":      7467.6,   "piad":    0.1778,
             "sazhen": 2.1336, "tochka":     0.000254, "vershok": 0.04445,
             "versta": 1066.8}
 
if __name__ == '__main__':
    assert len(argv) == 3, 'ERROR. Need two arguments - number then units'
    try:
        value = float(argv[1])
    except:
        print('ERROR. First argument must be a (float) number')
        raise
    unit = argv[2]
    assert unit in unit2mult, ( 'ERROR. Only know the following units: ' 
                                + ' '.join(unit2mult.keys()) )

    print("%g %s to:" % (value, unit))
    for unt, mlt in sorted(unit2mult.items()):
        print('  %10s: %g' % (unt, value * unit2mult[unit] / mlt))
```


{{out}}

```txt
1 meter to:
      arshin: 1.40607
  centimeter: 100
       diuym: 39.3701
         fut: 3.28084
   kilometer: 0.001
      liniya: 393.701
       meter: 1
       milia: 0.000133912
        piad: 5.6243
      sazhen: 0.468691
      tochka: 3937.01
     vershok: 22.4972
      versta: 0.000937383
```


{{out}}

```txt
1 milia to:
      arshin: 10500
  centimeter: 746760
       diuym: 294000
         fut: 24500
   kilometer: 7.4676
      liniya: 2.94e+06
       meter: 7467.6
       milia: 1
        piad: 42000
      sazhen: 3500
      tochka: 2.94e+07
     vershok: 168000
      versta: 7
```


{{out}}
When given a wrong number

```txt
ERROR. First argument must be a (float) number
Traceback (most recent call last):
  File "C:\Users\Paddy\Google Drive\Code\old_russian_lengths.py", line 18, in <module>
    value = float(argv[1])
ValueError: could not convert string to float: '1xx'
```


{{out}}
When given a wrong unit

```txt
Traceback (most recent call last):
  File "C:\Users\Paddy\Google Drive\Code\old_russian_lengths.py", line 24, in <module>
    + ' '.join(unit2mult.keys()) )
AssertionError: ERROR. Only know the following units: kilometer tochka versta fut diuym liniya vershok meter arshin piad centimeter sazhen milia
```



## Racket

Follows the Perl 6 solution, produces similar output.

```racket

#lang racket

(define units
  '([tochka        0.000254]
    [liniya        0.00254]
    [diuym         0.0254]
    [vershok       0.04445]
    [piad          0.1778]
    [fut           0.3048]
    [arshin        0.7112]
    [sazhen        2.1336]
    [versta     1066.8]
    [milia      7467.6]
    [centimeter    0.01]
    [meter         1.0]
    [kilometer  1000.0]))

(define (show u)
  (printf "1 ~s to:\n" u)
  (define n (cadr (assq u units)))
  (for ([u2 units] #:unless (eq? u (car u2)))
    (displayln (~a (~a (car u2) #:width 10 #:align 'right) ": "
                   (~r (/ n (cadr u2)) #:precision 4))))
  (newline))

(show 'meter)
(show 'milia)

```



## REXX

Program features:
::*   shows all   ''other''   units of measurements when any unit is specified. 
::*   accepts abbreviations of the length units
::*   does rounding so results are more meaningful and recognizable
::*   does error checking on the user input
::*   added other old Russian units of measurements
::*   uses the correct length unit names when not plural
::*   columnarized the output   (instead of a horizontal stream). 

```rexx
/*REXX program converts a   metric  or  old Russian length   to various other lengths.  */
                              numeric digits 200                      /*lots of digits. */
  /*──translation───*/
  /*tip, top        */        vershok  = 22.492971                    /*1.75 inch.      */
  /*palm, quarter   */        piad     = vershok    /   4             /*(also) chetvert.*/
  /*yard            */        arshin   = vershok    /  16
  /*fathom          */        sazhen   = arshin     /   3
  /*turn (of a plow)*/        verst    = sazhen     / 500             /*(also) a versta.*/
  /*mile            */        milia    = verst      /   1.5
  /*inch            */        diuym    = arshin     *  28
  /*foot            */        fut      = diuym      /  12             /*sounds like foot*/
  /*line            */        liniya   = diuym      *  10
  /*point           */        tochka   = diuym      * 100

KM=1000;    CM=100                               /*define a couple of metric multipliers*/
sw=linesize() -1                                 /*get the linesize (screen width)  - 1.*/
parse arg N what _ __                            /*obtain the user's input from the C.L.*/
if N==''               then call  err  'no arguments specified.'
if \datatype(N, 'N')   then call  err  'units not numeric: '    N
if _\==''              then call  err  'too many arguments specified: '   _   __
n=n/1                                            /*normalize it  (004──►4  7.──►7,  etc.*/
if what==''  then what='meters'                  /*None specified?  Then assume meters. */
whatU=what;  upper whatU                         /*an uppercase version for ABBREV bif. */
                   select                        /* [↓]  convert the length ───► meters.*/
                   when abbrev('METRES'     , whatU    ) |,
                        abbrev('METERS'     , whatU    )      then  m=N
                   when abbrev('KILOMETRES' , whatU, 2 ) |,
                        abbrev('KILOMETERS' , whatU, 2 ) |,
                        abbrev('KMS'        , whatU,   )      then  m=N * KM
                   when abbrev('CENTIMETRES', whatU, 2 ) |,
                        abbrev('CENTIMETERS', whatU, 2 ) |,
                        abbrev('CMS'        , whatU, 2 )      then  m=N / CM
                   when abbrev('ARSHINS'    , whatU    )      then  m=N / arshin
                   when abbrev('DIUYM'      , whatU    )      then  m=N / diuym
                   when abbrev('FUT'        , whatU    )      then  m=N / fut
                   when abbrev('LINIYA'     , whatU    )      then  m=N / liniya
                   when abbrev('PIADS'      , whatU    ) |,
                        abbrev('CHETVERTS'  , whatU, 2 )      then  m=N / piad
                   when abbrev('SAZHENS'    , whatU    )      then  m=N / sazhen
                   when abbrev('TOCHKA'     , whatU    )      then  m=N / tochka
                   when abbrev('VERSHOKS'   , whatU, 5 )      then  m=N / vershok
                   when abbrev('VERSTAS'    , whatU, 5 ) |,
                        abbrev('VERSTS'     , whatU, 2 )      then  m=N / verst
                   when abbrev('MILIA'      , whatU, 2 )      then  m=N / milia
                   otherwise     call err   'invalid measure name: '    what
                   end   /*select*/
                                          say centre('metric',      sw, "─")
call tell m/KM         ,   'kilometer'
call tell m            ,   'meter'
call tell m*CM         ,   'centimeter'
                                          say centre('old Russian', sw, "─")
call tell m*milia      ,   'milia'
call tell m*verst      ,   'verst'
call tell m*sazhen     ,   'sazhen'
call tell m*arshin     ,   'arshin'
call tell m*fut        ,   'fut'
call tell m*piad       ,   'piad'
call tell m*vershok    ,   'vershok'
call tell m*diuym      ,   'diuym'
call tell m*liniya     ,   'liniya'
call tell m*tochka     ,   'tochka'              /* ◄─── TELL shows eight decimal digits*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
err:   say center(' error ', sw % 2, "*");  do j=1  to arg();  say arg(j);  end;   exit 13
s:     if arg(1)=1  then return arg(3);     return word(arg(2) 's',1)         /*plurals.*/
tell:  parse arg $;  numeric digits 8;  $=$/1;  say right($, 40) arg(2)s($);       return
```

This REXX program makes use of   '''LINESIZE'''   REXX program (or BIF) which is used to determine the screen width (or linesize) of the terminal (console). 

The   '''LINESIZE.REX'''   REXX program is included here   ──►   [[LINESIZE.REX]].


{{out|output|text=  when using the input of:     <tt> 100   metres </tt>}}

```txt

────────────────────────────────────metric─────────────────────────────────────
                                     0.1 kilometers
                                     100 meters
                                   10000 centimeters
──────────────────────────────────old Russian──────────────────────────────────
                             0.062480475 milias
                             0.093720713 versts
                               46.860356 sazhens
                               140.58107 arshins
                               328.02249 futs
                               562.32428 piads
                               2249.2971 vershoks
                               3936.2699 diuyms
                               39362.699 liniyas
                               393626.99 tochkas

```

{{out|output|text=  when using the input of:     <tt> 1.4058107   arshins </tt>}}

```txt

────────────────────────────────────metric─────────────────────────────────────
                                   0.001 kilometers
                                       1 meter
                                     100 centimeters
──────────────────────────────────old Russian──────────────────────────────────
                           0.00062480476 milias
                           0.00093720713 versts
                              0.46860357 sazhens
                               1.4058107 arshins
                                3.280225 futs
                               5.6232428 piads
                               22.492971 vershoks
                                 39.3627 diuyms
                                 393.627 liniyas
                                 3936.27 tochkas

```

{{out|output|text=  when using the input of:     <tt> -46.860366   sazhens </tt>}}

```txt

────────────────────────────────────metric─────────────────────────────────────
                             -0.10000002 kilometers
                              -100.00002 meters
                              -10000.002 centimeters
──────────────────────────────────old Russian──────────────────────────────────
                            -0.062480488 milias
                            -0.093720732 versts
                              -46.860366 sazhens
                               -140.5811 arshins
                              -328.02256 futs
                              -562.32439 piads
                              -2249.2976 vershoks
                              -3936.2707 diuyms
                              -39362.707 liniyas
                              -393627.07 tochkas

```



## Ring


```ring

# Project : Old Russian measure of length

decimals(7)
units = ["tochka", "liniya", "dyuim", "vershok", "piad", "fut", 
             "arshin", "sazhen", "versta", "milia", 
             "centimeter", "meter", "kilometer"]
 
convs = [0.0254, 0.254, 2.54, 4.445, 17.78, 30.48, 
             71.12, 213.36, 10668, 74676, 
             1, 100, 10000]
 
yn = "y"
unit = 1
p = 1
while yn != "n"
      for i = 1 to 13
          see "" + i + " " + units[i] + nl
      next
      see nl
      see "please choose a unit 1 to 13 : "
      give unit
      see nl
      see "now enter a value in that unit : "
      give value
      see nl
      see "the equivalent in the remaining units is : "
      see nl
      for i = 1 to 13
          if i = unit
             loop
          ok
          see "" + units[i] + " : " + (value * convs[number(unit)] / convs[i]) + nl
      next
      see nl
      while yn = "y" or yn = "n"
              see "do another one y/n : "
              give yn
              yn = lower(yn)
      end 
end

```

Output:

```txt

1 tochka
2 liniya
3 dyuim
4 vershok
5 piad
6 fut
7 arshin
8 sazhen
9 versta
10 milia
11 centimeter
12 meter
13 kilometer

please choose a unit 1 to 13 : 13

now enter a value in that unit : 1

the equivalent in the remaining units is : 
tochka : 393700.7874016
liniya : 39370.0787402
dyuim : 3937.0078740
vershok : 2249.7187852
piad : 562.4296963
fut : 328.0839895
arshin : 140.6074241
sazhen : 46.8691414
versta : 0.9373828
milia : 0.1339118
centimeter : 10000
meter : 100

```



## Scala


```Scala
import scala.collection.immutable.HashMap

object OldRussianLengths extends App {

  private def measures = HashMap("tochka" -> 0.000254,
    "liniya"-> 0.000254, "centimeter"-> 0.01,    "diuym"-> 0.0254, "vershok"-> 0.04445,
    "piad"  -> 0.1778,   "fut"       -> 0.3048, "arshin"-> 0.7112, "meter"  -> 1.0,
    "sazhen"-> 2.1336,   "kilometer" -> 1000.0, "versta"-> 1066.8, "milia"  -> 7467.6
  ).withDefaultValue(Double.NaN)

  if (args.length == 2 && args(0).matches("[+-]?\\d*(\\.\\d+)?")) {
    val inputVal = measures(args(1))

    def meters = args(0).toDouble * inputVal

    if (!java.lang.Double.isNaN(inputVal)) {
      printf("%s %s to: %n%n", args(0), args(1))
      for (k <- measures) println(f"${k._1}%10s:  ${meters / k._2}%g")
    }
  } else println("Please provide a number and unit on the command line.")

}
```


## Sidef

{{trans|Perl 6}}

```ruby
func convert (magnitude, unit) {
     var factor = Hash(
        tochka     => 0.000254,
        liniya     => 0.00254,
        diuym      => 0.0254,
        vershok    => 0.04445,
        piad       => 0.1778,
        fut        => 0.3048,
        arshin     => 0.7112,
        sazhen     => 2.1336,
        versta     => 1066.8,
        milia      => 7467.6,
        centimeter => 0.01,
        meter      => 1,
        kilometer  => 1000,
    )

    var meters = (magnitude * factor{unit.lc})
    say("#{magnitude} #{unit} to:\n", '-' * 40)

    for u,f in (factor.sort_by { |_,v| v }) {
        printf("%10s: %s\n", u, meters / f) if (u != unit.lc)
    }
}

convert(1, 'meter')
say('')
convert(1, 'milia')
```

{{out}}

```txt
1 meter to:
----------------------------------------
    tochka: 3937.007874015748031496062992125984251968503937007874
    liniya: 393.700787401574803149606299212598425196850393700787
centimeter: 100
     diuym: 39.370078740157480314960629921259842519685039370079
   vershok: 22.497187851518560179977502812148481439820022497188
      piad: 5.624296962879640044994375703037120359955005624297
       fut: 3.28083989501312335958005249343832020997375328084
    arshin: 1.406074240719910011248593925759280089988751406074
    sazhen: 0.468691413573303337082864641919760029996250468691
 kilometer: 0.001
    versta: 0.000937382827146606674165729283839520059992500937
     milia: 0.000133911832449515239166532754834217151427500134

1 milia to:
----------------------------------------
    tochka: 29400000
    liniya: 2940000
centimeter: 746760
     diuym: 294000
   vershok: 168000
      piad: 42000
       fut: 24500
    arshin: 10500
     meter: 7467.6
    sazhen: 3500
 kilometer: 7.4676
    versta: 7
```



## Tcl


Tcllib already has a [http://core.tcl.tk/tcllib/doc/trunk/embedded/www/tcllib/files/modules/units/units.html units] package which knows about a lot of things, and can be taught more.  Since the other examples in this page provide a nicely tabulated conversions for 1 meter, we can copy that directly to get started ...


```Tcl

package require units

set russian_units {
      arshin  1.40607
  centimeter  100
       diuym  39.3701
         fut  3.28084
   kilometer  0.001
      liniya  393.701
       meter  1
       milia  0.000133912
        piad  5.6243
      sazhen  0.468691
      tochka  3937.01
     vershok  22.4972
      versta  0.000937383
}

proc add_russian_units {} {
    foreach {name factor} $::russian_units {
        if {$name eq "meter"} continue
        set factor [expr {1/$factor}]
        units::new $name "$factor meters"   ;# teach units about the new unit
    }
}

proc demo {} {  ;# show some examples
    foreach base {"1 meter" "1 milia"} {
        puts "$base to:"
        foreach {unit _} $::russian_units {
            puts [format " %-12s: %s" $unit [units::convert $base $unit]]
        }
        puts ""
    }
}

add_russian_units
demo

```


{{out}}

```txt

1 meter to:
 arshin      : 1.40607
 centimeter  : 100.0
 diuym       : 39.3701
 fut         : 3.28084
 kilometer   : 0.001
 liniya      : 393.701
 meter       : 1.0
 milia       : 0.000133912
 piad        : 5.6243
 sazhen      : 0.46869099999999997
 tochka      : 3937.01
 vershok     : 22.4972
 versta      : 0.000937383

1 milia to:
 arshin      : 10499.955194456059
 centimeter  : 746759.065655057
 diuym       : 293999.7909074616
 fut         : 24499.97012963737
 kilometer   : 7.46759065655057
 liniya      : 2939997.909074616
 meter       : 7467.59065655057
 milia       : 1.0
 piad        : 41999.970129637375
 sazhen      : 3499.9925324093433
 tochka      : 29399979.09074616
 vershok     : 167999.8805185495
 versta      : 6.999992532409343

```


One nice side effect of this implementation is that units can work with more complex dimensions:


```Tcl

% units::convert "1 piad^2" "hectare"
3.1612805858160454e-6
% units::convert "100 km/h" "versta/minute"
1.562305

```

number>
