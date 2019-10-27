+++
title = "Department Numbers"
description = ""
date = 2019-10-08T10:22:28Z
aliases = []
[extra]
id = 21382
[taxonomies]
categories = []
tags = []
+++

[[Category:Puzzles]]
{{task}}

There is a highly organized city that has decided to assign a number to each of their departments:
::*   police department
::*   sanitation department
::*   fire department 


Each department can have a number between '''1''' and '''7'''   (inclusive).

The three department numbers are to be unique (different from each other) and must add up to the number '''12'''.

The Chief of the Police doesn't like odd numbers and wants to have an even number for his department.


;Task:
Write a program which outputs all valid combinations.


Possible output:

1 2 9 

5 3 4





## 11l

{{trans|C}}

```11l
print(‘Police     Sanitation         Fire’)
print(‘----------------------------------’)

L(police) (2..6).step(2)
   L(sanitation) 1..7
      L(fire) 1..7
         I police!=sanitation & sanitation!=fire & fire!=police & police+fire+sanitation==12
            print(police"\t\t"sanitation"\t\t"fire)
```

{{Output}}

```txt

Police     Sanitation         Fire
----------------------------------
2               3               7
2               4               6
2               6               4
2               7               3
4               1               7
4               2               6
4               3               5
4               5               3
4               6               2
4               7               1
6               1               5
6               2               4
6               4               2
6               5               1

```



## Aime


```aime
integer p, s, f;

p = 0;
while ((p += 2) <= 7) {
    s = 0;
    while ((s += 1) <= 7) {
        f = 0;
        while ((f += 1) <= 7) {
            if (p + s + f == 12 && p != s && p != f && s != f) {
                o_form(" ~ ~ ~\n", p, s, f);
            }
        }
    }
}
```



## ALGOL 68

As noted in the Fortran sample, once the police and sanitation departments are posited, the fire department value is fixed

```algol68
BEGIN
    # show possible department number allocations for police, sanitation and fire departments #
    # the police department number must be even, all department numbers in the range 1 .. 7   #
    # the sum of the department numbers must be 12                                            #
    INT max department number =  7;
    INT department sum        = 12;
    print( ( "police sanitation fire", newline ) );
    FOR police FROM 2 BY 2 TO max department number DO
        FOR sanitation TO max department number DO
            IF sanitation /= police THEN
                INT fire = ( department sum - police ) - sanitation;
                IF  fire > 0 AND fire <= max department number
                AND fire /= sanitation
                AND fire /= police
                THEN
                    print( ( whole( police,      -6 )
                           , whole( sanitation, -11 )
                           , whole( fire,        -5 )
                           , newline
                           )
                         )
                FI
            FI
        OD
    OD
END
```

{{out}}

```txt

police sanitation fire
     2          3    7
     2          4    6
     2          6    4
     2          7    3
     4          1    7
     4          2    6
     4          3    5
     4          5    3
     4          6    2
     4          7    1
     6          1    5
     6          2    4
     6          4    2
     6          5    1

```



## ALGOL W

{{Trans|ALGOL 68}}

```algolw
begin
    % show possible department number allocations for police, sanitation and fire departments %
    % the police department number must be even, all department numbers in the range 1 .. 7   %
    % the sum of the department numbers must be 12                                            %
    integer MAX_DEPARTMENT_NUMBER, DEPARTMENT_SUM;
    MAX_DEPARTMENT_NUMBER :=  7;
    DEPARTMENT_SUM        := 12;
    write( "police sanitation fire" );
    for police := 2 step 2  until MAX_DEPARTMENT_NUMBER do begin
        for sanitation := 1 until MAX_DEPARTMENT_NUMBER do begin
            IF sanitation not = police then begin
                integer fire;
                fire := ( DEPARTMENT_SUM - police ) - sanitation;
                if  fire > 0 and fire <= MAX_DEPARTMENT_NUMBER and fire not = sanitation and fire not = police then begin
                    write( s_w := 0, i_w := 6, police, i_w := 11, sanitation, i_w := 5, fire )
                end if_valid_combination
            end if_sanitation_ne_police
        end for_sanitation
    end for_police
end.
```

{{out}}

```txt

police sanitation fire
     2          3    7
     2          4    6
     2          6    4
     2          7    3
     4          1    7
     4          2    6
     4          3    5
     4          5    3
     4          6    2
     4          7    1
     6          1    5
     6          2    4
     6          4    2
     6          5    1

```



## AppleScript

Briefly, composing a solution from generic functions:


```AppleScript
on run
    script
        on |λ|(x)
            script
                on |λ|(y)
                    script
                        on |λ|(z)
                            if y ≠ z and 1 ≤ z and z ≤ 7 then
                                {{x, y, z} as string}
                            else
                                {}
                            end if
                        end |λ|
                    end script
                    
                    concatMap(result, {12 - (x + y)}) --Z
                end |λ|
            end script
            
            concatMap(result, {1, 2, 3, 4, 5, 6, 7}) --Y
        end |λ|
    end script
    
    unlines(concatMap(result, {2, 4, 6})) --X
end run


-- GENERIC FUNCTIONS ----------------------------------------------------------

-- concatMap :: (a -> [b]) -> [a] -> [b]
on concatMap(f, xs)
    set lst to {}
    set lng to length of xs
    tell mReturn(f)
        repeat with i from 1 to lng
            set lst to (lst & |λ|(contents of item i of xs, i, xs))
        end repeat
    end tell
    return lst
end concatMap

-- intercalate :: Text -> [Text] -> Text
on intercalate(strText, lstText)
    set {dlm, my text item delimiters} to {my text item delimiters, strText}
    set strJoined to lstText as text
    set my text item delimiters to dlm
    return strJoined
end intercalate

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

-- unlines :: [String] -> String
on unlines(xs)
    intercalate(linefeed, xs)
end unlines
```

{{Out}}

```txt
237
246
264
273
417
426
435
453
462
471
615
624
642
651
```


Or more generally:
{{Trans|JavaScript}}
{{Trans|Haskell}}

```AppleScript
-- NUMBERING CONSTRAINTS ------------------------------------------------------

-- options :: Int -> Int -> Int -> [(Int, Int, Int)]
on options(lo, hi, total)
    set ds to enumFromTo(lo, hi)
    
    script Xeven
        on |λ|(x)
            script Ydistinct
                on |λ|(y)
                    script ZinRange
                        on |λ|(z)
                            if y ≠ z and lo ≤ z and z ≤ hi then
                                {{x, y, z}}
                            else
                                {}
                            end if
                        end |λ|
                    end script
                    
                    concatMap(ZinRange, {total - (x + y)}) -- Z IS IN RANGE
                end |λ|
            end script
            
            script notX
                on |λ|(d)
                    d ≠ x
                end |λ|
            end script
            
            concatMap(Ydistinct, filter(notX, ds)) -- Y IS NOT X
        end |λ|
    end script
    
    concatMap(Xeven, filter(my even, ds)) -- X IS EVEN
end options


-- TEST -----------------------------------------------------------------------
on run
    set xs to options(1, 7, 12)
    
    intercalate("\n\n", ¬
        {"(Police, Sanitation, Fire)", ¬
            unlines(map(show, xs)), ¬
            "Number of options: " & |length|(xs)})
end run


-- GENERIC FUNCTIONS ----------------------------------------------------------

-- concatMap :: (a -> [b]) -> [a] -> [b]
on concatMap(f, xs)
    set lst to {}
    set lng to length of xs
    tell mReturn(f)
        repeat with i from 1 to lng
            set lst to (lst & |λ|(contents of item i of xs, i, xs))
        end repeat
    end tell
    return lst
end concatMap

-- enumFromTo :: Int -> Int -> [Int]
on enumFromTo(m, n)
    if n < m then
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

-- even :: Int -> Bool
on even(x)
    x mod 2 = 0
end even

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

-- intercalate :: Text -> [Text] -> Text
on intercalate(strText, lstText)
    set {dlm, my text item delimiters} to {my text item delimiters, strText}
    set strJoined to lstText as text
    set my text item delimiters to dlm
    return strJoined
end intercalate

-- length :: [a] -> Int
on |length|(xs)
    length of xs
end |length|

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

-- show :: a -> String
on show(e)
    set c to class of e
    if c = list then
        script serialized
            on |λ|(v)
                show(v)
            end |λ|
        end script
        
        "[" & intercalate(", ", map(serialized, e)) & "]"
    else if c = record then
        script showField
            on |λ|(kv)
                set {k, ev} to kv
                "\"" & k & "\":" & show(ev)
            end |λ|
        end script
        
        "{" & intercalate(", ", ¬
            map(showField, zip(allKeys(e), allValues(e)))) & "}"
    else if c = date then
        "\"" & iso8601Z(e) & "\""
    else if c = text then
        "\"" & e & "\""
    else if (c = integer or c = real) then
        e as text
    else if c = class then
        "null"
    else
        try
            e as text
        on error
            ("«" & c as text) & "»"
        end try
    end if
end show

-- unlines :: [String] -> String
on unlines(xs)
    intercalate(linefeed, xs)
end unlines
```

{{Out}}

```txt
(Police, Sanitation, Fire)

[2, 3, 7]
[2, 4, 6]
[2, 6, 4]
[2, 7, 3]
[4, 1, 7]
[4, 2, 6]
[4, 3, 5]
[4, 5, 3]
[4, 6, 2]
[4, 7, 1]
[6, 1, 5]
[6, 2, 4]
[6, 4, 2]
[6, 5, 1]

Number of options: 14
```



## AutoHotkey


```AutoHotkey
perm(elements, n, opt:="", Delim:="", str:="", res:="", j:=0, dup:="") {	
	res := IsObject(res) ? res : [], dup := IsObject(dup) ? dup : []
	if (n > j) 
		Loop, parse, elements, % Delim
			res := !(InStr(str, A_LoopField) && !(InStr(opt, "rep"))) ? perm(elements, n, opt, Delim, trim(str Delim A_LoopField, Delim), res, j+1, dup) : res
	else if !(dup[x := perm_sort(str, Delim)] && (InStr(opt, "comb")))
		dup[x] := 1, res.Insert(str)
	return res, j++
}

perm_sort(str, Delim){
	Loop, Parse, str, % Delim
		res .= A_LoopField "`n"
	Sort, res, D`n
	return StrReplace(res, "`n", Delim)
}
```

Example:
```AutoHotkey
elements := "1234567", n := 3
for k, v in perm(elements, n)
	if (SubStr(v, 1, 1) + SubStr(v, 2, 1) + SubStr(v, 3, 1) = 12) && (SubStr(v, 1, 1) / 2 = Floor(SubStr(v, 1, 1)/2))
		res4 .= v "`n"

MsgBox, 262144, , % res4
return
```

Outputs:
```txt
237
246
264
273
417
426
435
453
462
471
615
624
642
651
```



## AWK


```AWK

# syntax: GAWK -f DEPARTMENT_NUMBERS.AWK
BEGIN {
    print(" # FD PD SD")
    for (fire=1; fire<=7; fire++) {
      for (police=1; police<=7; police++) {
        for (sanitation=1; sanitation<=7; sanitation++) {
          if (rules() ~ /^1+$/) {
            printf("%2d %2d %2d %2d\n",++count,fire,police,sanitation)
          }
        }
      }
    }
    exit(0)
}
function rules(  stmt1,stmt2,stmt3) {
    stmt1 = fire != police && fire != sanitation && police != sanitation
    stmt2 = fire + police + sanitation == 12
    stmt3 = police % 2 == 0
    return(stmt1 stmt2 stmt3)
}

```

{{out}}

```txt

 # FD PD SD
 1  1  4  7
 2  1  6  5
 3  2  4  6
 4  2  6  4
 5  3  2  7
 6  3  4  5
 7  4  2  6
 8  4  6  2
 9  5  4  3
10  5  6  1
11  6  2  4
12  6  4  2
13  7  2  3
14  7  4  1

```



## BASIC

==={{header|IS-BASIC}}===
<lang IS-BASIC>100 PRINT "Police","San.","Fire"
110 FOR P=2 TO 7 STEP 2
120   FOR S=1 TO 7
130     IF S<>P THEN
131       LET F=(12-P)-S
140       IF F>0 AND F<=7 AND F<>S AND F<>P THEN PRINT P,S,F
141     END IF
150   NEXT
160 NEXT
```


=
## Sinclair ZX81 BASIC
=
Works with 1k of RAM. This program ought not to need more than minimal changes to be compatible with any old-style BASIC dialect.

```basic
10 PRINT "POLICE SANITATION FIRE"
20 FOR P=2 TO 7 STEP 2
30 FOR S=1 TO 7
40 IF S=P THEN NEXT S
50 LET F=(12-P)-S
60 IF F>0 AND F<=7 AND F<>S AND F<>P THEN PRINT "   ";P;"       ";S;"       ";F
70 NEXT S
80 NEXT P
```

{{out}}

```txt
POLICE SANITATION FIRE
   2       3       7
   2       4       6
   2       6       4
   2       7       3
   4       1       7
   4       2       6
   4       3       5
   4       5       3
   4       6       2
   4       7       1
   6       1       5
   6       2       4
   6       4       2
   6       5       1
```


=
## BBC BASIC
=
{{trans|ALGOL 68}}

```bbcbasic>REM 
deptnums
max_dept_num% = 7
dept_sum% = 12
PRINT "police sanitation fire"
FOR police% = 2 TO max_dept_num% STEP 2
  FOR sanitation% = 1 TO max_dept_num%
    IF sanitation% <> police% THEN
      fire% = (dept_sum% - police%) - sanitation%
      IF fire% > 0 AND fire% <= max_dept_num% AND fire% <> sanitation% AND fire% <> police% THEN PRINT "   "; police%; "       "; sanitation%; "       "; fire%
    ENDIF
  NEXT
NEXT
END
```

{{out}}

```txt
police sanitation fire
   2       3       7
   2       4       6
   2       6       4
   2       7       3
   4       1       7
   4       2       6
   4       3       5
   4       5       3
   4       6       2
   4       7       1
   6       1       5
   6       2       4
   6       4       2
   6       5       1
```



## C

Weird that such a simple task was still not implemented in C, would be great to see some really creative ( obfuscated ) solutions for this one.

```C

#include<stdio.h>

int main()
{
	int police,sanitation,fire;
	
	printf("Police     Sanitation         Fire\n");
	printf("----------------------------------");
	
	for(police=2;police<=6;police+=2){
		for(sanitation=1;sanitation<=7;sanitation++){
			for(fire=1;fire<=7;fire++){
				if(police!=sanitation && sanitation!=fire && fire!=police && police+fire+sanitation==12){
					printf("\n%d\t\t%d\t\t%d",police,sanitation,fire);
				}
			}
		}
	}
	
	return 0;
}

```

Output:

```txt

Police     Sanitation         Fire
----------------------------------
2               3               7
2               4               6
2               6               4
2               7               3
4               1               7
4               2               6
4               3               5
4               5               3
4               6               2
4               7               1
6               1               5
6               2               4
6               4               2
6               5               1

```



## C sharp


```csharp
using System;
public class Program
{
    public static void Main() {
        for (int p = 2; p <= 7; p+=2) {
            for (int s = 1; s <= 7; s++) {
                int f = 12 - p - s;
                if (s >= f) break;
                if (f > 7) continue;
                if (s == p || f == p) continue; //not even necessary
                Console.WriteLine($"Police:{p}, Sanitation:{s}, Fire:{f}");
                Console.WriteLine($"Police:{p}, Sanitation:{f}, Fire:{s}");
            }
        }
    }
}
```

{{out}}

```txt

Police:2, Sanitation:3, Fire:7
Police:2, Sanitation:7, Fire:3
Police:2, Sanitation:4, Fire:6
Police:2, Sanitation:6, Fire:4
Police:4, Sanitation:1, Fire:7
Police:4, Sanitation:7, Fire:1
Police:4, Sanitation:2, Fire:6
Police:4, Sanitation:6, Fire:2
Police:4, Sanitation:3, Fire:5
Police:4, Sanitation:5, Fire:3
Police:6, Sanitation:1, Fire:5
Police:6, Sanitation:5, Fire:1
Police:6, Sanitation:2, Fire:4
Police:6, Sanitation:4, Fire:2

```



## C++


```cpp

#include <iostream>
#include <iomanip>

int main( int argc, char* argv[] ) {
    int sol = 1;
    std::cout << "\t\tFIRE\t\tPOLICE\t\tSANITATION\n";
    for( int f = 1; f < 8; f++ ) {
        for( int p = 1; p < 8; p++ ) {
            for( int s = 1; s < 8; s++ ) {
                if( f != p && f != s && p != s && !( p & 1 ) && ( f + s + p == 12 ) ) {
                std::cout << "SOLUTION #" << std::setw( 2 ) << sol++ << std::setw( 2 ) 
                << ":\t" << std::setw( 2 ) << f << "\t\t " << std::setw( 3 ) << p 
                << "\t\t" << std::setw( 6 ) << s << "\n";
                }
            }
        }
    }
    return 0;
}
```

{{out}}

```txt

                FIRE            POLICE          SANITATION
SOLUTION # 1:    1                 4                 7
SOLUTION # 2:    1                 6                 5
SOLUTION # 3:    2                 4                 6
SOLUTION # 4:    2                 6                 4
SOLUTION # 5:    3                 2                 7
SOLUTION # 6:    3                 4                 5
SOLUTION # 7:    4                 2                 6
SOLUTION # 8:    4                 6                 2
SOLUTION # 9:    5                 4                 3
SOLUTION #10:    5                 6                 1
SOLUTION #11:    6                 2                 4
SOLUTION #12:    6                 4                 2
SOLUTION #13:    7                 2                 3
SOLUTION #14:    7                 4                 1

```



## Clojure


```clojure
(let [n (range 1 8)]
  (for [police n
        sanitation n
        fire n
        :when (distinct? police sanitation fire)
        :when (even? police)
        :when (= 12 (+ police sanitation fire))]
    (println police sanitation fire)))
```

{{Out}}

```txt

2 3 7
2 4 6
2 6 4
2 7 3
4 1 7
4 2 6
4 3 5
4 5 3
4 6 2
4 7 1
6 1 5
6 2 4
6 4 2
6 5 1

```



## D


{{Trans|C++}}

```D

import std.stdio, std.range;

void main() {
    int sol = 1;
    writeln("\t\tFIRE\t\tPOLICE\t\tSANITATION");
    foreach( f; iota(1,8) ) {
        foreach( p; iota(1,8) ) {
            foreach( s; iota(1,8) ) {
                if( f != p && f != s && p != s && !( p & 1 ) && ( f + s + p == 12 ) ) {
                    writefln("SOLUTION #%2d:\t%2d\t\t%3d\t\t%6d", sol++, f, p, s);
                }
            }
        }
    }
}

```

Output:

```txt

		FIRE		POLICE		SANITATION
SOLUTION # 1:	 1		  4		     7
SOLUTION # 2:	 1		  6		     5
SOLUTION # 3:	 2		  4		     6
SOLUTION # 4:	 2		  6		     4
SOLUTION # 5:	 3		  2		     7
SOLUTION # 6:	 3		  4		     5
SOLUTION # 7:	 4		  2		     6
SOLUTION # 8:	 4		  6		     2
SOLUTION # 9:	 5		  4		     3
SOLUTION #10:	 5		  6		     1
SOLUTION #11:	 6		  2		     4
SOLUTION #12:	 6		  4		     2
SOLUTION #13:	 7		  2		     3
SOLUTION #14:	 7		  4		     1  

```


=={{header|F_Sharp|F#}}==

```fsharp

// A function to generate department numbers. Nigel Galloway: May 2nd., 2018
type dNum = {Police:int; Fire:int; Sanitation:int}
let fN n=n.Police%2=0&&n.Police+n.Fire+n.Sanitation=12&&n.Police<>n.Fire&&n.Police<>n.Sanitation&&n.Fire<>n.Sanitation
List.init (7*7*7) (fun n->{Police=n%7+1;Fire=(n/7)%7+1;Sanitation=(n/49)+1})|>List.filter fN|>List.iter(printfn "%A")

```

{{out}}

```txt

{Police = 6;
 Fire = 5;
 Sanitation = 1;}
{Police = 4;
 Fire = 7;
 Sanitation = 1;}
{Police = 6;
 Fire = 4;
 Sanitation = 2;}
{Police = 4;
 Fire = 6;
 Sanitation = 2;}
{Police = 4;
 Fire = 5;
 Sanitation = 3;}
{Police = 2;
 Fire = 7;
 Sanitation = 3;}
{Police = 6;
 Fire = 2;
 Sanitation = 4;}
{Police = 2;
 Fire = 6;
 Sanitation = 4;}
{Police = 6;
 Fire = 1;
 Sanitation = 5;}
{Police = 4;
 Fire = 3;
 Sanitation = 5;}
{Police = 4;
 Fire = 2;
 Sanitation = 6;}
{Police = 2;
 Fire = 4;
 Sanitation = 6;}
{Police = 4;
 Fire = 1;
 Sanitation = 7;}
{Police = 2;
 Fire = 3;
 Sanitation = 7;}

```



## Factor


```factor
USING: formatting io kernel math math.combinatorics math.ranges
sequences sets ;
IN: rosetta-code.department-numbers

7 [1,b] 3 <k-permutations>
[ [ first even? ] [ sum 12 = ] bi and ] filter

"{ Police, Sanitation, Fire }" print nl
[ "%[%d, %]\n" printf ] each
```

{{out}}

```txt

{ Police, Sanitation, Fire }

{ 2, 3, 7 }
{ 2, 4, 6 }
{ 2, 6, 4 }
{ 2, 7, 3 }
{ 4, 1, 7 }
{ 4, 2, 6 }
{ 4, 3, 5 }
{ 4, 5, 3 }
{ 4, 6, 2 }
{ 4, 7, 1 }
{ 6, 1, 5 }
{ 6, 2, 4 }
{ 6, 4, 2 }
{ 6, 5, 1 }

```



## Fortran

This uses the ability standardised in F90 of labelling a DO-loop so that its start and end are linked by usage of the same name, with this checked by the compiler. Further, in avoiding the use of the dreaded GO TO statement, the CYCLE statement can be employed instead with the same effect, and it too can bear the same name so that it is clear which loop is involved. These names prefix the DO-loop, and so, force some additional indentation. They are not statement labels and must be unique themselves. Notably, they cannot be the same text as the name of the index variable for their DO-loop, unlike the lead given by BASIC with its <code>FOR I ... NEXT I</code> arrangement.

The method is just to generate all the possibilities, discarding those that fail the specified tests. However, the requirement that the codes add up to twelve means that after the first two are chosen the third is determined, and blandly looping through all the possibilities is too much brute force and ignorance, though other collections of rules could make that bearable.

Since the modernisers of Fortran made a point of specifying that it does not specify the manner of evaluation of compound boolean expressions, specifically, that there is to be no reliance on [[Short-circuit_evaluation]], both parts of the compound expression of the line labelled 5 "may" be evaluated even though the first may have determined the result. Prior to the introduction of LOGICAL variables with F66, one employed integer arithmetic as is demonstrated in the arithmetic-IF test of the line labelled 6. On the B6700, this usage ran faster than the corresponding boolean expression - possibly because there was no test for short-circuiting the expression when the first part of a multiply was zero...

Note that the syntax enables ''two'' classes of labels: the old-style numerical label in columns one to five, and the special label-like prefix of a DO-loop that is not in columns one to five. And yes, a line can have both. 
```Fortran
      INTEGER P,S,F	!Department codes for Police, Sanitation, and Fire. Values 1 to 7 only.
    1  PP:DO P = 2,7,2	!The police demand an even number. They're special and use violence.
    2   SS:DO S = 1,7		!The sanitation department accepts any value.
    3        IF (P.EQ.S) CYCLE SS	!But it must differ from the others.
    4        F = 12 - (P + S)		!The fire department accepts any number, but the sum must be twelve.
    5        IF (F.LE.0 .OR. F.GT.7) CYCLE SS	!Ensure that the only option is within range.
    6        IF ((F - S)*(F - P)) 7,8,7		!And F is to differ from S and from P
    7        WRITE (6,"(3I2)") P,S,F		!If we get here, we have a possible set.
    8      END DO SS		!Next S
    9    END DO PP	!Next P.
      END	!Well, that was straightforward.
```

Output:

```txt

 2 3 7
 2 4 6
 2 6 4
 2 7 3
 4 1 7
 4 2 6
 4 3 5
 4 5 3
 4 6 2
 4 7 1
 6 1 5
 6 2 4
 6 4 2
 6 5 1

```



## FreeBASIC


```freebasic
' version 15-08-2017
' compile with: fbc -s console

Dim As Integer fire, police, sanitation

Print "police fire sanitation"
Print "----------------------"

For police = 2 To 7 Step 2
    For fire = 1 To 7
        If fire = police Then Continue For
        sanitation = 12 - police - fire
        If sanitation = fire Or sanitation = police Then Continue For
        If sanitation >= 1 And sanitation <= 7 Then
            Print Using "   #     #       # "; police; fire; sanitation
        End If
    Next
Next

' empty keyboard buffer
While Inkey <> "" : Wend
Print : Print "hit any key to end program"
Sleep
End
```

{{out}}

```txt
police fire sanitation
----------------------
   2     3       7 
   2     4       6 
   2     6       4 
   2     7       3 
   4     1       7 
   4     2       6 
   4     3       5 
   4     5       3 
   4     6       2 
   4     7       1 
   6     1       5 
   6     2       4 
   6     4       2 
   6     5       1 
```



## Gambas

'''[https://gambas-playground.proko.eu/?gist=cdfeadc26aaeb0e23f4523626b6fe7c9 Click this link to run this code]'''

```gambas
Public Sub Main()
Dim siC0, siC1, siC2 As Short
Dim sOut As New String[]
Dim sTemp As String

For siC0 = 2 To 6 Step 2
  For siC1 = 1 To 7
    For siC2 = 1 To 7
      If sic0 + siC1 + siC2 = 12 Then
        If siC0 <> siC1 And siC1 <> siC2 And siC0 <> siC2 Then sOut.Add(Str(siC0) & Str(siC1) & Str(siC2))
      End If
    Next
  Next
Next

Print "\tPolice\tFire\tSanitation"
siC0 = 0

For Each sTemp In sOut
  Inc sic0
  Print "[" & Format(Str(siC0), "00") & "]\t" & Left(sTemp, 1) & "\t" & Mid(sTemp, 2, 1) & "\t" & Right(sTemp, 1)
Next

End
```

Output:

```txt

        Police  Fire    Sanitation
[01]    2       3       7
[02]    2       4       6
[03]    2       6       4
[04]    2       7       3
[05]    4       1       7
[06]    4       2       6
[07]    4       3       5
[08]    4       5       3
[09]    4       6       2
[10]    4       7       1
[11]    6       1       5
[12]    6       2       4
[13]    6       4       2
[14]    6       5       1

```



## Go

{{trans|Kotlin}}

```go
package main

import "fmt"

func main() {
    fmt.Println("Police  Sanitation  Fire")
    fmt.Println("------  ----------  ----")
    count := 0
    for i := 2; i < 7; i += 2 {
        for j := 1; j < 8; j++ {
            if j == i { continue }
            for k := 1; k < 8; k++ {
                if k == i || k == j { continue }
                if i + j + k != 12 { continue }
                fmt.Printf("  %d         %d         %d\n", i, j, k)
                count++
            }
        }
    }
    fmt.Printf("\n%d valid combinations\n", count)
}
```


{{out}}

```txt

Police  Sanitation  Fire
------  ----------  ----
  2         3         7
  2         4         6
  2         6         4
  2         7         3
  4         1         7
  4         2         6
  4         3         5
  4         5         3
  4         6         2
  4         7         1
  6         1         5
  6         2         4
  6         4         2
  6         5         1

14 valid combinations

```



## Haskell

Bare minimum:

```haskell
main :: IO ()
main =
  mapM_ print $
  [2, 4, 6] >>=
  \x ->
     [1 .. 7] >>=
     \y ->
        [12 - (x + y)] >>=
        \z ->
           case y /= z && 1 <= z && z <= 7 of
             True -> [(x, y, z)]
             _ -> []
```

or, resugaring this into list comprehension format:

```Haskell
main :: IO ()
main =
  mapM_
    print
    [ (x, y, z)
    | x <- [2, 4, 6] 
    , y <- [1 .. 7] 
    , z <- [12 - (x + y)] 
    , y /= z && 1 <= z && z <= 7 ]
```

Do notation:

```Haskell
main :: IO ()
main =
  mapM_ print $
  do x <- [2, 4, 6]
     y <- [1 .. 7]
     z <- [12 - (x + y)]
     if y /= z && 1 <= z && z <= 7
       then [(x, y, z)]
       else []
```

Unadorned brute force – more than enough at this small scale:

```Haskell
import Data.List (nub)

main :: IO ()
main =
  let xs = [1 .. 7]
  in mapM_ print $
     xs >>=
     \x ->
        xs >>=
        \y ->
           xs >>=
           \z ->
              [ (x, y, z)
              | even x && 3 == length (nub [x, y, z]) && 12 == sum [x, y, z] ]
```

{{Out}}

```txt
(2,3,7)
(2,4,6)
(2,6,4)
(2,7,3)
(4,1,7)
(4,2,6)
(4,3,5)
(4,5,3)
(4,6,2)
(4,7,1)
(6,1,5)
(6,2,4)
(6,4,2)
(6,5,1)
```


Or, more generally:

```Haskell
options :: Int -> Int -> Int -> [(Int, Int, Int)]
options lo hi total =
  (\ds ->
      filter even ds >>=
      \x ->
         filter (/= x) ds >>=
         \y ->
            [total - (x + y)] >>=
            \z ->
               case y /= z && lo <= z && z <= hi of
                 True -> [(x, y, z)]
                 _ -> [])
    [lo .. hi]

-- TEST -----------------------------------------------------------------------
main :: IO ()
main = do
  let xs = options 1 7 12
  putStrLn "(Police, Sanitation, Fire)\n"
  mapM_ print xs
  mapM_ putStrLn ["\nNumber of options: ", show (length xs)]
```


Reaching again for a little more syntactic sugar, the options function above could also be re-written either as a list comprehension,

```Haskell
options :: Int -> Int -> Int -> [(Int, Int, Int)]
options lo hi total =
  let ds = [lo .. hi]
  in [ (x, y, z)
     | x <- filter even ds 
     , y <- filter (/= x) ds 
     , let z = total - (x + y) 
     , y /= z && lo <= z && z <= hi ]
```


or in Do notation:

```haskell
import Control.Monad (guard)

options :: Int -> Int -> Int -> [(Int, Int, Int)]
options lo hi total =
  let ds = [lo .. hi]
  in do x <- filter even ds
        y <- filter (/= x) ds
        let z = total - (x + y)
        guard $ y /= z && lo <= z && z <= hi
        return (x, y, z)
```

{{Out}}

```txt
(Police, Sanitation, Fire)

(2,3,7)
(2,4,6)
(2,6,4)
(2,7,3)
(4,1,7)
(4,2,6)
(4,3,5)
(4,5,3)
(4,6,2)
(4,7,1)
(6,1,5)
(6,2,4)
(6,4,2)
(6,5,1)

Number of options: 
14
```



## J

'''Solution:'''

```j
require 'stats'
permfrom=: ,/@(perm@[ {"_ 1 comb)  NB. get permutations of length x from y possible items

alluniq=: # = #@~.           NB. check items are unique
addto12=: 12 = +/            NB. check items add to 12
iseven=: -.@(2&|)            NB. check items are even
policeeven=: {.@iseven       NB. check first item is even
conditions=: policeeven *. addto12 *. alluniq

Validnums=: >: i.7           NB. valid Department numbers

getDeptNums=: [: (#~ conditions"1) Validnums {~ permfrom
```

'''Example usage:'''

```j
   3 getDeptNums 7
4 1 7
4 7 1
6 1 5
6 5 1
2 3 7
2 7 3
2 4 6
2 6 4
4 2 6
4 6 2
6 2 4
6 4 2
4 3 5
4 5 3
```



### Alternate approach



```J
   (#~ 12=+/"1) 1+3 comb 7 [ load'stats'
1 4 7
1 5 6
2 3 7
2 4 6
3 4 5
```


Note that we are only showing the distinct [[combinations]] here, not all [[permutations]] of those combinations.


## Java

{{trans|Kotlin}}

```Java
public class DepartmentNumbers {
    public static void main(String[] args) {
        System.out.println("Police  Sanitation  Fire");
        System.out.println("------  ----------  ----");
        int count = 0;
        for (int i = 2; i <= 6; i += 2) {
            for (int j = 1; j <= 7; ++j) {
                if (j == i) continue;
                for (int k = 1; k <= 7; ++k) {
                    if (k == i || k == j) continue;
                    if (i + j + k != 12) continue;
                    System.out.printf("  %d         %d         %d\n", i, j, k);
                    count++;
                }
            }
        }
        System.out.printf("\n%d valid combinations", count);
    }
}
```

{{out}}

```txt
Police  Sanitation  Fire
------  ----------  ----
  2         3         7
  2         4         6
  2         6         4
  2         7         3
  4         1         7
  4         2         6
  4         3         5
  4         5         3
  4         6         2
  4         7         1
  6         1         5
  6         2         4
  6         4         2
  6         5         1
```



## Javascript


### ES5

Briefly:

```JavaScript
(function () {
    'use strict';

    // concatMap :: (a -> [b]) -> [a] -> [b]
    function concatMap(f, xs) {
        return [].concat.apply([], xs.map(f));
    };

    return '(Police, Sanitation, Fire)\n' +
        concatMap(function (x) {
            return concatMap(function (y) {
                return concatMap(function (z) {
                    return z !== y && 1 <= z && z <= 7 ? [
                        [x, y, z]
                    ] : [];
                }, [12 - (x + y)]);
            }, [1, 2, 3, 4, 5, 6, 7]);
        }, [2, 4, 6])
        .map(JSON.stringify)
        .join('\n');
})();
```

{{Out}}

```txt
(Police, Sanitation, Fire)
[2,3,7]
[2,4,6]
[2,6,4]
[2,7,3]
[4,1,7]
[4,2,6]
[4,3,5]
[4,5,3]
[4,6,2]
[4,7,1]
[6,1,5]
[6,2,4]
[6,4,2]
[6,5,1]
```


Or, more generally:
{{Trans|Haskell}}

```JavaScript
(function () {
    'use strict';

    // NUMBERING CONSTRAINTS --------------------------------------------------

    // options :: Int -> Int -> Int -> [(Int, Int, Int)]
    function options(lo, hi, total) {
        var bind = flip(concatMap),
            ds = enumFromTo(lo, hi);

        return bind(filter(even, ds),
            function (x) { // X is even,
                return bind(filter(function (d) { return d !== x; }, ds),
            function (y) { // Y is distinct from X,
                return bind([total - (x + y)],
            function (z) { // Z sums with x and y to total, and is in ds.
                return z !== y && lo <= z && z <= hi ? [
                    [x, y, z]
                ] : [];
            })})})};

    // GENERIC FUNCTIONS ------------------------------------------------------

    // concatMap :: (a -> [b]) -> [a] -> [b]
    function concatMap(f, xs) {
        return [].concat.apply([], xs.map(f));
    };

    // enumFromTo :: Int -> Int -> [Int]
    function enumFromTo(m, n) {
        return Array.from({
            length: Math.floor(n - m) + 1
        }, function (_, i) {
            return m + i;
        });
    };

    // even :: Integral a => a -> Bool
    function even(n) {
        return n % 2 === 0;
    };

    // filter :: (a -> Bool) -> [a] -> [a]
    function filter(f, xs) {
        return xs.filter(f);
    };

    // flip :: (a -> b -> c) -> b -> a -> c
    function flip(f) {
        return function (a, b) {
            return f.apply(null, [b, a]);
        };
    };

    // length :: [a] -> Int
    function length(xs) {
        return xs.length;
    };

    // map :: (a -> b) -> [a] -> [b]
    function map(f, xs) {
        return xs.map(f);
    };

    // show :: a -> String
    function show(x) {
        return JSON.stringify(x);
    }; //, null, 2);

    // unlines :: [String] -> String
    function unlines(xs) {
        return xs.join('\n');
    };

    // TEST -------------------------------------------------------------------
    var xs = options(1, 7, 12);
    return '(Police, Sanitation, Fire)\n\n' +
        unlines(map(show, xs)) + '\n\nNumber of options: ' + length(xs);
})();
```

{{Out}}

```txt
(Police, Sanitation, Fire)

[2,3,7]
[2,4,6]
[2,6,4]
[2,7,3]
[4,1,7]
[4,2,6]
[4,3,5]
[4,5,3]
[4,6,2]
[4,7,1]
[6,1,5]
[6,2,4]
[6,4,2]
[6,5,1]

Number of options: 14
```



### ES6

Briefly:

```JavaScript
(() => {

    // concatMap :: (a -> [b]) -> [a] -> [b]
    const concatMap = (f, xs) => [].concat.apply([], xs.map(f));

    return '(Police, Sanitation, Fire)\n' +
        concatMap(x =>
            concatMap(y =>
                concatMap(z =>
                    z !== y && 1 <= z && z <= 7 ? [
                        [x, y, z]
                    ] : [], [12 - (x + y)]
                ), [1, 2, 3, 4, 5, 6, 7]
            ), [2, 4, 6]
        )
        .map(JSON.stringify)
        .join('\n');
})();
```

{{Out}}

```txt
(Police, Sanitation, Fire)
[2,3,7]
[2,4,6]
[2,6,4]
[2,7,3]
[4,1,7]
[4,2,6]
[4,3,5]
[4,5,3]
[4,6,2]
[4,7,1]
[6,1,5]
[6,2,4]
[6,4,2]
[6,5,1]
```


Or, more generally, by composition of generic functions:
{{Trans|Haskell}}

```JavaScript
(() => {
    'use strict';

    // NUMBERING CONSTRAINTS --------------------------------------------------

    // options :: Int -> Int -> Int -> [(Int, Int, Int)]
    const options = (lo, hi, total) => {
        const
            bind = flip(concatMap),
            ds = enumFromTo(lo, hi);

        return bind(filter(even, ds),
            x => bind(filter(d => d !== x, ds),
                y => bind([total - (x + y)],
                    z => (z !== y && lo <= z && z <= hi) ? [
                        [x, y, z]
                    ] : []
                )
            )
        )
    };

    // GENERIC FUNCTIONS ------------------------------------------------------

    // concatMap :: (a -> [b]) -> [a] -> [b]
    const concatMap = (f, xs) => [].concat.apply([], xs.map(f));

    // enumFromTo :: Int -> Int -> [Int]
    const enumFromTo = (m, n) =>
        Array.from({
            length: Math.floor(n - m) + 1
        }, (_, i) => m + i);

    // even :: Integral a => a -> Bool
    const even = n => n % 2 === 0;

    // filter :: (a -> Bool) -> [a] -> [a]
    const filter = (f, xs) => xs.filter(f);

    // flip :: (a -> b -> c) -> b -> a -> c
    const flip = f => (a, b) => f.apply(null, [b, a]);

    // length :: [a] -> Int
    const length = xs => xs.length;

    // map :: (a -> b) -> [a] -> [b]
    const map = (f, xs) => xs.map(f);

    // show :: a -> String
    const show = x => JSON.stringify(x) //, null, 2);

    // unlines :: [String] -> String
    const unlines = xs => xs.join('\n');

    // TEST -------------------------------------------------------------------
    const xs = options(1, 7, 12);
    return '(Police, Sanitation, Fire)\n\n' +
        unlines(map(show, xs)) +
        '\n\nNumber of options: ' + length(xs);
})();
```

{{Out}}

```txt
(Police, Sanitation, Fire)

[2,3,7]
[2,4,6]
[2,6,4]
[2,7,3]
[4,1,7]
[4,2,6]
[4,3,5]
[4,5,3]
[4,6,2]
[4,7,1]
[6,1,5]
[6,2,4]
[6,4,2]
[6,5,1]

Number of options: 14
```



## jq

In this section, we present three solutions. 

The first illustrates how a straightforward generate-and-test algorithm using familiar for-loops can be translated into jq.

The second illustrates how essentially the same algorithm can be written in a more economical way, without sacrificing comprehensibility.

The third illustrates how the first can easily be made more efficient by adding some pruning.
 
The solutions in all cases are presented as a stream of JSON objects such as:
 
    {"fire":1,"police":4,"sanitation":7}

as these are self-explanatory, though it would be trivial to present them in another format. For brevity, the solutions are omitted here.

'''Nested for-loop'''

```jq
def check(fire; police; sanitation):
    (fire != police) and (fire != sanitation) and (police != sanitation)
    and (fire + police + sanitation == 12)
    and (police % 2 == 0);

range(1;8) as $fire
| range(1;8) as $police
| range(1;8) as $sanitation
| select( check($fire; $police; $sanitation) )
| {fire: $fire, police: $police, sanitation: $sanitation}
```


'''In Brief'''

```jq
{fire: range(1;8), police: range(1;8), sanitation: range(1;8)}
| select( .fire != .police and .fire != .sanitation and .police != .sanitation
      and .fire + .police + .sanitation == 12
      and .police % 2 == 0 )
```


'''Pruning'''

```jq
range(1;8) as $fire
| (range(1;8) | select(. != $fire)) as $police
| (range(1;8) | select(. != $fire and . != $police)) as $sanitation
| {fire: $fire, police: $police, sanitation: $sanitation}
| select( ([.[]] | add) == 12)
```



## Julia


```julia
using Printf

function findsolution(rng=1:7)
    rst = Matrix{Int}(0, 3)
    for p in rng, f in rng, s in rng
        if p != s != f != p && p + s + f == 12 && iseven(p)
            rst = [rst; p s f]
        end
    end
    return rst
end

function printsolutions(sol::Matrix{Int})
    println("      Pol.   Fire   San.")
    println("      ----   ----   ----")
    for row in 1:size(sol, 1)
        @printf("%2i | %4i%7i%7i\n", row, sol[row, :]...)
    end
end

printsolutions(findsolution())

```


{{out}}

```txt
      Pol.   Fire   San.
      ----   ----   ----
 1 |    2      7      3
 2 |    2      6      4
 3 |    2      4      6
 4 |    2      3      7
 5 |    4      7      1
 6 |    4      6      2
 7 |    4      5      3
 8 |    4      3      5
 9 |    4      2      6
10 |    4      1      7
11 |    6      5      1
12 |    6      4      2
13 |    6      2      4
14 |    6      1      5
```



## Kotlin


```scala
// version 1.1.2

fun main(args: Array<String>) {
    println("Police  Sanitation  Fire")
    println("------  ----------  ----")
    var count = 0
    for (i in 2..6 step 2) {
        for (j in 1..7) {
            if (j == i) continue
            for (k in 1..7) {
                if (k == i || k == j) continue
                if (i + j + k != 12) continue
                println("  $i         $j         $k")
                count++
            }
        }
    }
    println("\n$count valid combinations")
}
```


{{out}}

```txt

Police  Sanitation  Fire
------  ----------  ----
  2         3         7
  2         4         6
  2         6         4
  2         7         3
  4         1         7
  4         2         6
  4         3         5
  4         5         3
  4         6         2
  4         7         1
  6         1         5
  6         2         4
  6         4         2
  6         5         1

14 valid combinations

```



## Lua


```lua

print( "Fire", "Police", "Sanitation" )
sol = 0
for f = 1, 7 do
    for p = 1, 7 do
        for s = 1, 7 do
            if s + p + f == 12 and p % 2 == 0 and f ~= p and f ~= s and p ~= s then
                print( f, p, s ); sol = sol + 1
            end
        end
    end
end
print( string.format( "\n%d solutions found", sol ) )

```

{{out}}

```txt

Fire    Police  Sanitation
1       4       7
1       6       5
2       4       6
2       6       4
3       2       7
3       4       5
4       2       6
4       6       2
5       4       3
5       6       1
6       2       4
6       4       2
7       2       3
7       4       1

14 solutions found

```



## Maple


```Maple
#determines if i, j, k are exclusive numbers
exclusive_numbers := proc(i, j, k)
	if (i = j) or (i = k) or (j = k) then
		return false;
	end if;
	return true;
end proc;

#outputs all possible combinations of numbers that statisfy given conditions
department_numbers := proc()
	local i, j, k;
	printf("Police		Sanitation	Fire\n");
	for i to 7 do
		for j to 7 do
			k := 12 - i - j;
			if (k <= 7) and (k >= 1) and (i mod 2 = 0) and exclusive_numbers(i,j,k) then
				printf("%d		%d		%d\n", i, j, k);
			end if;
		end do;
	end do;
end proc;

department_numbers();

```

{{out|Output}}

```txt

Police		Sanitation	Fire
2		3		7
2		4		6
2		6		4
2		7		3
4		1		7
4		2		6
4		3		5
4		5		3
4		6		2
4		7		1
6		1		5
6		2		4
6		4		2
6		5		1

```




## Mathematica


```Mathematica
Select[Permutations[Range[7], {3}], Total[#] == 12 && EvenQ[First[#]] &]
```

{{out}}

```txt
{{2, 3, 7}, {2, 4, 6}, {2, 6, 4}, {2, 7, 3}, {4, 1, 7}, {4, 2, 6}, {4, 3, 5}, {4, 5, 3}, 
{4, 6, 2}, {4, 7, 1}, {6, 1, 5}, {6, 2, 4}, {6, 4, 2}, {6, 5, 1}}
```



## Objeck

{{Trans|C++}}

```objeck
class Program {
  function : Main(args : String[]) ~ Nil {
    sol := 1;
    "\t\tFIRE\tPOLICE\tSANITATION"->PrintLine();
    for( f := 1; f < 8; f+=1; ) {
      for( p := 1; p < 8; p+=1; ) {
        for( s:= 1; s < 8; s+=1; ) {
          if( f <> p & f <> s & p <> s & ( p and 1 ) = 0 & ( f + s + p = 12 ) ) {
            "SOLUTION #{$sol}: \t{$f}\t{$p}\t{$s}"->PrintLine();
            sol += 1;
          };
        };
      };
    };  
  }
}
```


Output:

```txt

                FIRE    POLICE  SANITATION
SOLUTION #1:    1       4       7
SOLUTION #2:    1       6       5
SOLUTION #3:    2       4       6
SOLUTION #4:    2       6       4
SOLUTION #5:    3       2       7
SOLUTION #6:    3       4       5
SOLUTION #7:    4       2       6
SOLUTION #8:    4       6       2
SOLUTION #9:    5       4       3
SOLUTION #10:   5       6       1
SOLUTION #11:   6       2       4
SOLUTION #12:   6       4       2
SOLUTION #13:   7       2       3
SOLUTION #14:   7       4       1

```


=={{header|Modula-2}}==

```modula2
MODULE DepartmentNumbers;
FROM Conversions IMPORT IntToStr;
FROM Terminal IMPORT WriteString,WriteLn,ReadChar;

PROCEDURE WriteInt(num : INTEGER);
VAR str : ARRAY[0..16] OF CHAR;
BEGIN
    IntToStr(num,str);
    WriteString(str);
END WriteInt;

VAR i,j,k,count : INTEGER;
BEGIN
    count:=0;

    WriteString("Police  Sanitation  Fire");
    WriteLn;
    WriteString("------  ----------  ----");
    WriteLn;

    FOR i:=2 TO 6 BY 2 DO
        FOR j:=1 TO 7 DO
            IF j=i THEN CONTINUE; END;
            FOR k:=1 TO 7 DO
                IF (k=i) OR (k=j) THEN CONTINUE; END;
                IF i+j+k # 12 THEN CONTINUE; END;
                WriteString("  ");
                WriteInt(i);
                WriteString("         ");
                WriteInt(j);
                WriteString("         ");
                WriteInt(k);
                WriteLn;
                INC(count);
            END;
        END;
    END;

    WriteLn;
    WriteInt(count);
    WriteString(" valid combinations");
    WriteLn;

    ReadChar;
END DepartmentNumbers.
```



## PARI/GP



```parigp
forstep(p=2,6,2, for(f=1,7, s=12-p-f; if(p!=f && p!=s && f!=s && s>0 && s<8, print(p" "f" "s))))
```

{{out}}

```txt
2 3 7
2 4 6
2 6 4
2 7 3
4 1 7
4 2 6
4 3 5
4 5 3
4 6 2
4 7 1
6 1 5
6 2 4
6 4 2
6 5 1
```



## Perl



```Perl

#!/usr/bin/perl

my @even_numbers;

for (1..7)
{
  if ( $_ % 2 == 0)
  {
    push @even_numbers, $_;
  }
}
	
print "Police\tFire\tSanitation\n";

foreach my $police_number (@even_numbers)
{
  for my $fire_number (1..7)
  {
    for my $sanitation_number (1..7)
    {
      if ( $police_number + $fire_number + $sanitation_number == 12 && 
           $police_number != $fire_number && 
           $fire_number != $sanitation_number && 
           $sanitation_number != $police_number)
      {
        print "$police_number\t$fire_number\t$sanitation_number\n";
      }
    }
  }	
}

```



Above Code cleaned up and shortened


```Perl

#!/usr/bin/perl

use strict;   # Not necessary but considered good perl style
use warnings; # this one too

print "Police\t-\tFire\t-\tSanitation\n";
for my $p ( 1..7 )  # Police Department
{
  for my $f ( 1..7) # Fire Department
  {
    for my $s ( 1..7 ) # Sanitation Department
    {
      if ( $p % 2 == 0 && $p + $f + $s == 12 && $p != $f && $f != $s  && $s != $p && $f != $s) # Check if the combination of numbers is valid
      {
        print "$p\t-\t$f\t-\t$s\n";
      }
    }
  }
}

```


Output:

```Perl

Police  -       Fire    -       Sanitation
2       -       3       -       7
2       -       4       -       6
2       -       6       -       4
2       -       7       -       3
4       -       1       -       7
4       -       2       -       6
4       -       3       -       5
4       -       5       -       3
4       -       6       -       2
4       -       7       -       1
6       -       1       -       5
6       -       2       -       4
6       -       4       -       2
6       -       5       -       1

```



## Perl 6


```perl6
for (1..7).combinations(3).grep(*.sum == 12) {
    for   .permutations\  .grep(*.[0] %%  2) {
        say <police fire sanitation> Z=> .list;
    }
}

```

{{out}}

```txt

(police => 4 fire => 1 sanitation => 7)
(police => 4 fire => 7 sanitation => 1)
(police => 6 fire => 1 sanitation => 5)
(police => 6 fire => 5 sanitation => 1)
(police => 2 fire => 3 sanitation => 7)
(police => 2 fire => 7 sanitation => 3)
(police => 2 fire => 4 sanitation => 6)
(police => 2 fire => 6 sanitation => 4)
(police => 4 fire => 2 sanitation => 6)
(police => 4 fire => 6 sanitation => 2)
(police => 6 fire => 2 sanitation => 4)
(police => 6 fire => 4 sanitation => 2)
(police => 4 fire => 3 sanitation => 5)
(police => 4 fire => 5 sanitation => 3)

```



## Phix


```Phix
printf(1,"Police  Sanitation  Fire\n")
printf(1,"------  ----------  ----\n")
integer solutions = 0
for police=2 to 7 by 2 do
    for sanitation=1 to 7 do
        if sanitation!=police then
            integer fire = 12-(police+sanitation)
            if fire>=1
            and fire<=7
            and fire!=police
            and fire!=sanitation then
                printf(1,"  %d         %d         %d\n", {police,sanitation,fire})
                solutions += 1
            end if
        end if
    end for
end for
printf(1,"\n%d solutions found\n", solutions)
```

{{out}}

```txt

Police  Sanitation  Fire
------  ----------  ----
  2         3         7
  2         4         6
  2         6         4
  2         7         3
  4         1         7
  4         2         6
  4         3         5
  4         5         3
  4         6         2
  4         7         1
  6         1         5
  6         2         4
  6         4         2
  6         5         1

14 solutions found

```



## Python


### Procedural


```python
from itertools import permutations
 
def solve():
    c, p, f, s = "\\,Police,Fire,Sanitation".split(',')
    print(f"{c:>3}  {p:^6} {f:^4} {s:^10}")
    c = 1
    for p, f, s in permutations(range(1, 8), r=3):
        if p + s + f == 12 and p % 2 == 0:
            print(f"{c:>3}: {p:^6} {f:^4} {s:^10}")
            c += 1
 
if __name__ == '__main__':
    solve()
```


{{out}}

```txt
  \  Police Fire Sanitation
  1:   2     3       7     
  2:   2     4       6     
  3:   2     6       4     
  4:   2     7       3     
  5:   4     1       7     
  6:   4     2       6     
  7:   4     3       5     
  8:   4     5       3     
  9:   4     6       2     
 10:   4     7       1     
 11:   6     1       5     
 12:   6     2       4     
 13:   6     4       2     
 14:   6     5       1     
```



### Composition of pure functions

Expressing the options directly and declaratively in terms of a ''bind'' operator, without importing ''permutations'':
{{Works with|Python|3}}

```python
'''Department numbers'''

from itertools import (chain)
from operator import (ne)


# options :: Int -> Int -> Int -> [(Int, Int, Int)]
def options(lo, hi, total):
    '''Eligible integer triples.'''
    ds = enumFromTo(lo)(hi)
    return bind(filter(even, ds))(
        lambda x: bind(filter(curry(ne)(x), ds))(
            lambda y: bind([total - (x + y)])(
                lambda z: [(x, y, z)] if (
                    z != y and lo <= z <= hi
                ) else []
            )
        )
    )


# TEST ----------------------------------------------------
# main :: IO ()
def main():
    '''Test'''

    xs = options(1, 7, 12)
    print(('Police', 'Sanitation', 'Fire'))
    for tpl in xs:
        print(tpl)
    print('\nNo. of options: ' + str(len(xs)))


# GENERIC ABSTRACTIONS ------------------------------------

# bind (>>=) :: [a] -> (a -> [b]) -> [b]
def bind(xs):
    '''List monad injection operator.
       Two computations sequentially composed,
       with any value produced by the first
       passed as an argument to the second.'''
    return lambda f: list(
        chain.from_iterable(
            map(f, xs)
        )
    )


# curry :: ((a, b) -> c) -> a -> b -> c
def curry(f):
    '''A curried function derived
       from an uncurried function.'''
    return lambda a: lambda b: f(a, b)


# enumFromTo :: (Int, Int) -> [Int]
def enumFromTo(m):
    '''Integer enumeration from m to n.'''
    return lambda n: list(range(m, 1 + n))


# even :: Int -> Bool
def even(x):
    '''True if x is an integer
       multiple of two.'''
    return 0 == x % 2


if __name__ == '__main__':
    main()
```

{{Out}}

```txt
('Police', 'Sanitation', 'Fire')
(2, 3, 7)
(2, 4, 6)
(2, 6, 4)
(2, 7, 3)
(4, 1, 7)
(4, 2, 6)
(4, 3, 5)
(4, 5, 3)
(4, 6, 2)
(4, 7, 1)
(6, 1, 5)
(6, 2, 4)
(6, 4, 2)
(6, 5, 1)

No. of options: 14
```



### List comprehension


Nested ''bind'' (or ''concatMap'') expressions (like those above) can also be translated into list comprehension notation:
{{Works with|Python|3.7}}

```python
'''Department numbers'''

from operator import ne


# options :: Int -> Int -> Int -> [(Int, Int, Int)]
def options(lo, hi, total):
    '''Eligible triples.'''
    ds = enumFromTo(lo)(hi)
    return [
        (x, y, z)
        for x in filter(even, ds)
        for y in filter(curry(ne)(x), ds)
        for z in [total - (x + y)]
        if y != z and lo <= z <= hi
    ]


# Or with less tightly-constrained generation,
# and more winnowing work downstream:

# options2 :: Int -> Int -> Int -> [(Int, Int, Int)]
def options2(lo, hi, total):
    '''Eligible triples.'''
    ds = enumFromTo(lo)(hi)
    return [
        (x, y, z)
        for x in ds
        for y in ds
        for z in [total - (x + y)]
        if even(x) and y not in [x, z] and lo <= z <= hi
    ]


# GENERIC -------------------------------------------------


# curry :: ((a, b) -> c) -> a -> b -> c
def curry(f):
    '''A curried function derived
       from an uncurried function.'''
    return lambda a: lambda b: f(a, b)


# enumFromTo :: (Int, Int) -> [Int]
def enumFromTo(m):
    '''Integer enumeration from m to n.'''
    return lambda n: list(range(m, 1 + n))


# even :: Int -> Bool
def even(x):
    '''True if x is an integer
       multiple of two.'''
    return 0 == x % 2


# unlines :: [String] -> String
def unlines(xs):
    '''A single string derived by the intercalation
       of a list of strings with the newline character.'''
    return '\n'.join(xs)


# TEST ----------------------------------------------------
# main :: IO ()
def main():
    '''Test'''

    xs = options(1, 7, 12)
    print(('Police', 'Sanitation', 'Fire'))
    print(unlines(map(str, xs)))
    print('\nNo. of options: ' + str(len(xs)))


if __name__ == '__main__':
    main()
```

{{Out}}

```txt
('Police', 'Sanitation', 'Fire')
(2, 3, 7)
(2, 4, 6)
(2, 6, 4)
(2, 7, 3)
(4, 1, 7)
(4, 2, 6)
(4, 3, 5)
(4, 5, 3)
(4, 6, 2)
(4, 7, 1)
(6, 1, 5)
(6, 2, 4)
(6, 4, 2)
(6, 5, 1)

No. of options: 14 
```



## Racket


We filter the Cartesian product of the lists of candidate department numbers.


```racket
#lang racket
(cons '(police fire sanitation)
      (filter (λ (pfs) (and (not (check-duplicates pfs))
                            (= 12 (apply + pfs))
                            pfs))
              (cartesian-product (range 2 8 2) (range 1 8) (range 1 8))))

```


{{out}}


```txt
'((police fire sanitation)
  (2 3 7)
  (2 4 6)
  (2 6 4)
  (2 7 3)
  (4 1 7)
  (4 2 6)
  (4 3 5)
  (4 5 3)
  (4 6 2)
  (4 7 1)
  (6 1 5)
  (6 2 4)
  (6 4 2)
  (6 5 1))
```



## REXX


### bare bones


```rexx
/*REXX program finds/displays all possible variants of (3) department numbering  puzzle.*/
say 'police fire sanitation'                     /*display a crude title for the output.*/
  do     p=2   to 7  by 2                        /*try numbers for the police department*/
    do   f=1  for 7                              /* "     "     "   "  fire        "    */
      do s=1  for 7;             $=p+f+s         /* "     "     "   "  sanitation  "    */
      if f\==p & s\==p & s\==f & $==12  then say  center(p,6)   center(f,5)   center(s,10)
      end   /*s*/
    end     /*f*/
  end       /*p*/                                /*stick a fork in it,  we're all done. */
```

{{out|output|text=  when using the default inputs:}}

```txt

police fire sanitation
  2      3       7
  2      4       6
  2      6       4
  2      7       3
  4      1       7
  4      2       6
  4      3       5
  4      5       3
  4      6       2
  4      7       1
  6      1       5
  6      2       4
  6      4       2
  6      5       1

```


### options and optimizing

A little extra code was added to allow the specification for the high department number as well as the sum.

Two optimizing statements were added (for speed),   but for this simple puzzle they aren't needed.

Also, extra code was added to nicely format a title (header) for the output, as well as displaying the number of solutions found.

```rexx
/*REXX program finds/displays all possible variants of (3) department numbering  puzzle.*/
parse arg high sum .                             /*obtain optional arguments from the CL*/
if high=='' | high==","  then high= 7            /*Not specified?  Then use the default.*/
if  sum=='' |  sum==","  then  sum=12            /* "      "         "   "   "     "    */
@pd= ' police ';   @fd= " fire "  ;   @sd= ' sanitation ' /*define names of departments.*/
@dept=  ' department ';     L=length(@dept)               /*literal; and also its length*/
#=0                                              /*initialize the number of solutions.  */
    do PD=2  by 2  to high                       /*try numbers for the police department*/
       do FD=1   for  high                       /* "     "     "   "  fire       "     */
       if FD==PD       then iterate              /*Same FD# & PD#?  They must be unique.*/
       if FD+PD>sum-1  then iterate PD           /*Is sum too large?   Try another PD#. */    /* ◄■■■■■■ optimizing code*/
          do SD=1  for  high                     /*try numbers for the sanitation dept. */
          if SD==PD | SD==FD  then iterate       /*Is SD# ¬unique?  They must be unique,*/
          $=PD+FD+SD                             /*compute sum of department numbers.   */
          if $>  sum   then iterate FD           /*Is the sum too high?  Try another FD#*/    /* ◄■■■■■■ optimizing code*/
          if $\==sum   then iterate              /*Is the sum ¬correct?   "     "    SD#*/
          #=# + 1                                /*bump the number of solutions (so far)*/
          if #==1 then do                        /*Is this the 1st solution?   Show hdr.*/
                       say center(@pd, L)      center(@fd, L)      center(@sd, L)
                       say copies(center(   @dept, L)' ', 3)
                       say copies(center('number', L)' ', 3)
                       say center('', L, "═")  center('', L, "═")  center('', L, "═")
                       end
          say  center(PD, L)   center(FD, L)   center(SD, L)       /*display a solution.*/
          end   /*SD*/
       end      /*FD*/
    end         /*PD*/
say                                              /*display a blank line before the #sols*/
if #==0  then #= 'no'                            /*use a better word for bupkis.        */
say #  "solutions found."                        /*stick a fork in it,  we're all done. */
```

{{out|output|text=  when using the default inputs:}}

```txt

   police        fire      sanitation
 department   department   department
   number       number       number
════════════ ════════════ ════════════
     2            3            7
     2            4            6
     2            6            4
     2            7            3
     4            1            7
     4            2            6
     4            3            5
     4            5            3
     4            6            2
     4            7            1
     6            1            5
     6            2            4
     6            4            2
     6            5            1

14 solutions found.

```



## Ring


```ring

sanitation= 0
see "police fire sanitation" + nl

for police = 2 to 7 step 2
    for fire = 1 to 7
        if fire = police
           loop
        ok
        sanitation = 12 - police - fire
        if sanitation = fire or sanitation = police 
           loop
        ok  
        if sanitation >= 1 and sanitation <= 7 
           see "   " + police + "      " + fire + "       " + sanitation + nl
        ok
    next
next

```

Output:

```txt

police fire sanitation
   2    3       7
   2    4       6
   2    6       4
   2    7       3
   4    1       7
   4    2       6
   4    3       5
   4    5       3
   4    6       2
   4    7       1
   6    1       5
   6    2       4
   6    4       2
   6    5       1

```



## Ruby


```ruby

(1..7).to_a.permutation(3){|p| puts p.join if p.first.even? && p.sum == 12 }

```

{{Output}}

```txt
237
246
264
273
417
426
435
453
462
471
615
624
642
651

```



## Rust

{{trans|C}}

```rust

extern crate num_iter;

fn main()
{
	println!("Police Sanitation Fire");
	println!("----------------------");

	for police in num_iter::range_step(2,7,2){
		for sanitation in 1..8 {
			for fire in 1..8 {
				if police!=sanitation && sanitation!=fire && fire!=police && police+fire+sanitation==12 {
					println!("{:6}{:11}{:4}",police,sanitation,fire);
				}
			}
		}
	}

}

```



## Scala


```scala
val depts = {
  (1 to 7).permutations.map{ n => (n(0),n(1),n(2)) }.toList.distinct  // All permutations of possible department numbers
  .filter{ n => n._1 % 2 == 0 }                                       // Keep only even numbers favored by Police Chief
  .filter{ n => n._1 + n._2 + n._3 == 12 }                            // Keep only numbers that add to 12
}

{
println( "(Police, Sanitation, Fire)")
println( depts.mkString("\n") )
}

```

{{output}}

```txt
(Police, Sanitation, Fire)
(2,3,7)
(2,4,6)
(2,6,4)
(2,7,3)
(4,1,7)
(4,2,6)
(4,3,5)
(4,5,3)
(4,6,2)
(4,7,1)
(6,1,5)
(6,2,4)
(6,4,2)
(6,5,1)

```



## Sidef

{{trans|Perl 6}}

```ruby
@(1..7)->combinations(3, {|*a|
    a.sum == 12 || next
    a.permutations {|*b|
        b[0].is_even || next
        say (%w(police fire sanitation) ~Z b -> join(" "))
    }
})
```

{{out}}

```txt

["police", 4] ["fire", 1] ["sanitation", 7]
["police", 4] ["fire", 7] ["sanitation", 1]
["police", 6] ["fire", 1] ["sanitation", 5]
["police", 6] ["fire", 5] ["sanitation", 1]
["police", 2] ["fire", 3] ["sanitation", 7]
["police", 2] ["fire", 7] ["sanitation", 3]
["police", 2] ["fire", 4] ["sanitation", 6]
["police", 2] ["fire", 6] ["sanitation", 4]
["police", 4] ["fire", 2] ["sanitation", 6]
["police", 4] ["fire", 6] ["sanitation", 2]
["police", 6] ["fire", 2] ["sanitation", 4]
["police", 6] ["fire", 4] ["sanitation", 2]
["police", 4] ["fire", 3] ["sanitation", 5]
["police", 4] ["fire", 5] ["sanitation", 3]

```



## Swift


Functional approach:


```swift
let res = [2, 4, 6].map({x in
  return (1...7)
      .filter({ $0 != x })
      .map({y -> (Int, Int, Int)? in
        let z = 12 - (x + y)

        guard y != z && 1 <= z && z <= 7 else {
          return nil
        }

        return (x, y, z)
      }).compactMap({ $0 })
}).flatMap({ $0 })

for result in res {
  print(result)
}
```


Iterative approach:


```swift
var res = [(Int, Int, Int)]()

for x in [2, 4, 6] {
  for y in 1...7 where x != y {
    let z = 12 - (x + y)

    guard y != z && 1 <= z && z <= 7 else {
      continue
    }

    res.append((x, y, z))
  }
}

for result in res {
  print(result)
}
```


{{out}}

```txt
(2, 3, 7)
(2, 4, 6)
(2, 6, 4)
(2, 7, 3)
(4, 1, 7)
(4, 2, 6)
(4, 3, 5)
(4, 5, 3)
(4, 6, 2)
(4, 7, 1)
(6, 1, 5)
(6, 2, 4)
(6, 4, 2)
(6, 5, 1)
```



## Tcl


Since Tool Command Language is a multi-paradigm language, very different solutions are possible.

<b>VERSION A</b> - using procedures and list operations

```tcl

# Procedure named ".." returns list of integers from 1 to max.
proc .. max {
  for {set i 1} {$i <= $max} {incr i} {
    lappend l $i
  }
  return $l
}

# Procedure named "anyEqual" returns true if any elements are equal,
# false otherwise.
proc anyEqual l {
  if {[llength [lsort -unique $l]] != [llength $l]} {
    return 1
  }
  return 0
}

# Procedure named "odd" tells whether a value is odd or not.
proc odd n {
  expr $n %2 != 0
}


# Procedure named "sum" sums its parameters.
proc sum args {
  expr [join $args +]
}


# Create lists of candidate numbers using proc ".."
set sanitation [.. 7]
set fire $sanitation
# Filter even numbers for police stations (remove odd ones).
set police [lmap e $sanitation {
  if [odd $e] continue
  set e
}]


# Try all combinations and display acceptable ones.
set valid 0
foreach p $police {
  foreach s $sanitation {
    foreach f $fire {
      # Check for equal elements in list.
      if [anyEqual [list $p $s $f]] continue
      # Check for sum of list elements.
      if {[sum $p $s $f] != 12} continue
      puts "$p $s $f"
      incr valid
    }
  }
}
puts "$valid valid combinations found."

```


<b>VERSION B</b> - using simple for loops with number literals

```tcl

set valid 0
for {set police 2} {$police <= 6} {incr police 2} {
  for {set sanitation 1} {$sanitation <= 7} {incr sanitation} {
    if {$police == $sanitation} continue
    for {set fire 1} {$fire <= 7} {incr fire} {
      if {$police == $fire || $sanitation == $fire} continue
      if {[expr $police + $sanitation + $fire] != 12} continue
      puts "$police $sanitation $fire"
      incr valid
    }
  }
}
puts "$valid valid combinations found."

```


<b>VERSION C</b> - using simple for loops with number variables

```tcl

set min 1
set max 7
set valid 0
for {set police $min} {$police <= $max} {incr police} {
  if {[expr $police % 2] == 1} continue ;# filter even numbers for police
  for {set sanitation $min} {$sanitation <= $max} {incr sanitation} {
    if {$police == $sanitation} continue
    for {set fire $min} {$fire <= $max} {incr fire} {
      if {$police == $fire || $sanitation == $fire} continue
      if {[expr $police + $sanitation + $fire] != 12} continue
      puts "$police $sanitation $fire"
      incr valid
    }
  }
}

puts "$valid valid combinations found."

```


<b>VERSION D</b> - using list filter with lambda expressions

```tcl

# Procedure named ".." returns list of integers from 1 to max.
proc .. max {
  for {set i 1} {$i <= $max} {incr i} {
    lappend l $i
  }
  return $l
}


# Procedure named "..." returns list of n lists of integers from 1 to max.
proc ... {max n} {
  foreach i [.. $n] {
    lappend result [.. $max]
  }
  return $result
}

# Procedure named "crossProduct" returns cross product of lists
proc crossProduct {listOfLists} {
  set result [list [list]]
  foreach factor $listOfLists {
    set newResult {}
    foreach combination $result {
      foreach elt $factor {
        lappend newResult [linsert $combination end $elt]
      }
    }
    set result $newResult
  }
  return $result
}

# Procedure named "filter" filters list elements by using a 
# condition λ (lambda) expression
proc filter {l condition} {
  return [lmap el $l {
    if {![apply $condition $el]} continue
    set el
  }]
}

# Here the fun using lambda expressions begins. The following is the main program.

# Set λ expressions
set λPoliceEven {_ {expr [lindex $_ 0] % 2 == 0}}
set λNoEquals {_ {expr [llength [lsort -unique $_]] == [llength $_]}}
set λSumIs12 {_ {expr [join $_ +] == 12}}

# Create all combinations and filter acceptable ones
set numbersOk [filter [filter [filter [crossProduct [... 7 3]] ${λPoliceEven}] ${λSumIs12}] ${λNoEquals}]
puts [join $numbersOk \n]
puts "[llength $numbersOk] valid combinations found."

```

{{output}}
All four versions (A, B, C and D) produce the same result:

```txt

2 3 7
2 4 6
2 6 4
2 7 3
4 1 7
4 2 6
4 3 5
4 5 3
4 6 2
4 7 1
6 1 5
6 2 4
6 4 2
6 5 1
14 valid combinations found.

```



## Visual Basic .NET

{{trans|C#}}

```vbnet
Module Module1

    Sub Main()
        For p = 2 To 7 Step 2
            For s = 1 To 7
                Dim f = 12 - p - s
                If s >= f Then
                    Exit For
                End If
                If f > 7 Then
                    Continue For
                End If
                If s = p OrElse f = p Then
                    Continue For 'not even necessary
                End If
                Console.WriteLine($"Police:{p}, Sanitation:{s}, Fire:{f}")
                Console.WriteLine($"Police:{p}, Sanitation:{f}, Fire:{s}")
            Next
        Next
    End Sub

End Module
```

{{out}}

```txt
Police:2, Sanitation:3, Fire:7
Police:2, Sanitation:7, Fire:3
Police:2, Sanitation:4, Fire:6
Police:2, Sanitation:6, Fire:4
Police:4, Sanitation:1, Fire:7
Police:4, Sanitation:7, Fire:1
Police:4, Sanitation:2, Fire:6
Police:4, Sanitation:6, Fire:2
Police:4, Sanitation:3, Fire:5
Police:4, Sanitation:5, Fire:3
Police:6, Sanitation:1, Fire:5
Police:6, Sanitation:5, Fire:1
Police:6, Sanitation:2, Fire:4
Police:6, Sanitation:4, Fire:2
```



## zkl


```zkl
Utils.Helpers.pickNFrom(3,[1..7].walk())    // 35 combos
.filter(fcn(numbers){ numbers.sum(0)==12 }) // which all sum to 12 (==5)
.println();
```

{{output}}

```txt

L(L(1,4,7),L(1,5,6),L(2,3,7),L(2,4,6),L(3,4,5))

```

Note: The sum of three odd numbers is odd, so a+b+c=12 means at least one even 
nmber (1 even, two odd or 3 even). Futher, 2a+b=12, a,b in (2,4,6) has one
solution: a=2,b=4

For a table with repeated solutions using nested loops:

```zkl
println("Police  Fire  Sanitation");
foreach p,f,s in ([2..7,2], [1..7], [1..7])
   { if((p!=s!=f) and p+f+s==12) println(p,"\t",f,"\t",s) }
```

{{out}}

```txt

Police  Fire  Sanitation
2	3	7
2	4	6
2	6	4
2	7	3
4	1	7
4	2	6
4	3	5
4	5	3
4	6	2
4	7	1
6	1	5
6	2	4
6	4	2
6	5	1

```

