+++
title = "Mutual recursion"
description = ""
date = 2019-03-27T06:23:01Z
aliases = []
[extra]
id = 4063
[taxonomies]
categories = ["task", "recursion"]
tags = []
languages = [
  "abap",
  "acl2",
  "ada",
  "aime",
  "algol_68",
  "algol_w",
  "applescript",
  "autohotkey",
  "awk",
  "bacon",
  "basic",
  "bbc_basic",
  "bc",
  "bracmat",
  "brat",
  "c",
  "ceylon",
  "clojure",
  "coffeescript",
  "common_lisp",
  "cpp",
  "csharp",
  "d",
  "dart",
  "delphi",
  "e",
  "eiffel",
  "elena",
  "elixir",
  "erlang",
  "euphoria",
  "factor",
  "false",
  "fantom",
  "forth",
  "fortran",
  "freebasic",
  "go",
  "groovy",
  "haskell",
  "idris",
  "io",
  "j",
  "java",
  "javascript",
  "jq",
  "jsish",
  "julia",
  "kotlin",
  "liberty_basic",
  "libreoffice_basic",
  "logo",
  "lsl",
  "lua",
  "m2000_interpreter",
  "m4",
  "maple",
  "mathematica",
  "matlab",
  "maxima",
  "mercury",
  "mmix",
  "nemerle",
  "nim",
  "objeck",
  "ocaml",
  "octave",
  "oforth",
  "ol",
  "order",
  "oz",
  "pari_gp",
  "pascal",
  "perl",
  "perl_6",
  "phix",
  "php",
  "picolisp",
  "pl_i",
  "postscript",
  "powershell",
  "prolog",
  "pure",
  "purebasic",
  "python",
  "r",
  "racket",
  "rebol",
  "rexx",
  "ring",
  "ruby",
  "run_basic",
  "rust",
  "sather",
  "scala",
  "scheme",
  "seed7",
  "sidef",
  "smalltalk",
  "snobol4",
  "snusp",
  "spl",
  "standard_ml",
  "swift",
  "tcl",
  "txr",
  "ubasic_4th",
  "unix_shell",
  "ursala",
  "vba",
  "x86_assembly",
  "xpl0",
  "yabasic",
  "zkl",
]
+++

## Task

Two functions are said to be mutually recursive if the first calls the second,
and in turn the second calls the first.

Write two mutually recursive functions that compute members of the [[wp:Hofstadter sequence#Hofstadter Female and Male sequences|Hofstadter Female and Male sequences]] defined as:
<big>
:<math>
\begin{align}
F(0)&=1\ ;\ M(0)=0 \\
F(n)&=n-M(F(n-1)), \quad n>0 \\
M(n)&=n-F(M(n-1)), \quad n>0.
\end{align}
</math>
</big>


(If a language does not allow for a solution using mutually recursive functions
then state this rather than give a solution by other means).





## ABAP

This works for ABAP Version 7.40 and can be implemented in procedural ABAP as well, but with classes it is much more readable. As this allows a method with a returning value to be an input for a subsequent method call.


```ABAP

report z_mutual_recursion.

class hoffstadter_sequences definition.
  public section.
    class-methods:
      f
        importing
          n             type int4
        returning
          value(result) type int4,

      m
        importing
          n             type int4
        returning
          value(result) type int4.
endclass.


class hoffstadter_sequences implementation.
  method f.
    result = cond int4(
      when n eq 0
      then 1
      else n - m( f( n - 1 ) ) ).
  endmethod.


  method m.
    result = cond int4(
      when n eq 0
      then 0
      else n - f( m( n - 1 ) ) ).
  endmethod.
endclass.


start-of-selection.
  write: |{ reduce string(
    init results = |f(0 - 19): { hoffstadter_sequences=>f( 0 ) }|
    for i = 1 while i < 20
    next results = |{ results }, { hoffstadter_sequences=>f( i ) }| ) }|, /.

  write: |{ reduce string(
    init results = |m(0 - 19): { hoffstadter_sequences=>m( 0 ) }|
    for i = 1 while i < 20
    next results = |{ results }, { hoffstadter_sequences=>m( i ) }| ) }|, /.

```


```txt

f(0 - 19): 1, 1, 2, 2, 3, 3, 4, 5, 5, 6, 6, 7, 8, 8, 9, 9, 10, 11, 11, 12

m(0 - 19): 0, 0, 1, 2, 2, 3, 4, 4, 5, 6, 6, 7, 7, 8, 9, 9, 10, 11, 11, 12

```



## ACL2


```lisp
(mutual-recursion
 (defun f (n)
    (declare (xargs :mode :program))
    (if (zp n)
        1
        (- n (m (f (1- n))))))

 (defun m (n)
    (declare (xargs :mode :program))
    (if (zp n)
        0
        (- n (f (m (1- n)))))))
```



## Ada


```Ada
with Ada.Text_Io; use Ada.Text_Io;
procedure Mutual_Recursion is
   function M(N : Integer) return Integer;
   function F(N : Integer) return Integer is
   begin
      if N = 0 then
         return 1;
      else
         return N - M(F(N - 1));
      end if;
   end F;
   function M(N : Integer) return Integer is
   begin
      if N = 0 then
         return 0;
      else
         return N - F(M(N-1));
      end if;
   end M;
begin
   for I in 0..19 loop
      Put_Line(Integer'Image(F(I)));
   end loop;
   New_Line;
   for I in 0..19 loop
      Put_Line(Integer'Image(M(I)));
   end loop;
end Mutual_recursion;
```


```ada
with Ada.Text_Io; use Ada.Text_Io;
procedure Mutual_Recursion is
   function M(N: Natural) return Natural;
   function F(N: Natural) return Natural;

   function M(N: Natural) return Natural is
       (if N = 0 then 0 else N – F(M(N–1)));

   function F(N: Natural) return Natural is
       (if N =0 then 1 else N – M(F(N–1)));
begin
   for I in 0..19 loop
      Put_Line(Integer'Image(F(I)));
   end loop;
   New_Line;
   for I in 0..19 loop
      Put_Line(Integer'Image(M(I)));
   end loop;

end Mutual_recursion;
```



## Aime

```aime
integer F(integer n);
integer M(integer n);

integer F(integer n)
{
    integer r;
    if (n) {
	r = n - M(F(n - 1));
    } else {
	r = 1;
    }
    return r;
}

integer M(integer n)
{
    integer r;
    if (n) {
	r = n - F(M(n - 1));
    } else {
	r = 0;
    }
    return r;
}

integer main(void)
{
    integer i;
    i = 0;
    while (i < 20) {
	o_winteger(3, F(i));
	i += 1;
    }
    o_byte('\n');
    i = 0;
    while (i < 20) {
	o_winteger(3, M(i));
	i += 1;
    }
    o_byte('\n');
    return 0;
}
```



## ALGOL 68

```algol68
PROC (INT)INT m; # ONLY required for ELLA ALGOL 68RS - an official subset OF full ALGOL 68 #

PROC f = (INT n)INT:
  IF n = 0 THEN 1
  ELSE n - m(f(n-1)) FI;

m := (INT n)INT:
  IF n = 0 THEN 0
  ELSE n - f(m(n-1)) FI;

main:
(
  FOR i FROM 0 TO 19 DO
    print(whole(f(i),-3))
  OD;
  new line(stand out);
  FOR i FROM 0 TO 19 DO
    print(whole(m(i),-3))
  OD;
  new line(stand out)
)
```

```txt

  1  1  2  2  3  3  4  5  5  6  6  7  8  8  9  9 10 11 11 12
  0  0  1  2  2  3  4  4  5  6  6  7  7  8  9  9 10 11 11 12

```



## ALGOL W


```algolw
begin
    % define mutually recursive funtions F and M that compute the elements   %
    % of the Hofstadter Female and Male sequences                            %

    integer procedure F ( integer value n ) ;
        if n = 0 then 1 else n - M( F( n - 1 ) );

    integer procedure M ( integer value n ) ;
        if n = 0 then 0 else n - F( M( n - 1 ) );

    % print the first few elements of the sequences                          %
    i_w := 2; s_w := 1; % set I/O formatting                                 %
    write( "F: " );
    for i := 0 until 20 do writeon( F( i ) );
    write( "M: " );
    for i := 0 until 20 do writeon( M( i ) );

end.
```




## AppleScript



```AppleScript
-- f :: Int -> Int
on f(x)
    if x = 0 then
        1
    else
        x - m(f(x - 1))
    end if
end f

-- m :: Int -> Int
on m(x)
    if x = 0 then
        0
    else
        x - f(m(x - 1))
    end if
end m


-- TEST
on run
    set xs to range(0, 19)

    {map(f, xs), map(m, xs)}
end run


-- GENERIC FUNCTIONS

-- map :: (a -> b) -> [a] -> [b]
on map(f, xs)
    tell mReturn(f)
        set lng to length of xs
        set lst to {}
        repeat with i from 1 to lng
            set end of lst to lambda(item i of xs, i, xs)
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
            property lambda : f
        end script
    end if
end mReturn

-- range :: Int -> Int -> [Int]
on range(m, n)
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
end range
```


```AppleScript
{{1, 1, 2, 2, 3, 3, 4, 5, 5, 6, 6, 7, 8, 8, 9, 9, 10, 11, 11, 12},
 {0, 0, 1, 2, 2, 3, 4, 4, 5, 6, 6, 7, 7, 8, 9, 9, 10, 11, 11, 12}}
```



## AutoHotkey


```AutoHotkey
Loop 20
   i := A_Index-1, t .= "`n" i "`t   " M(i) "`t     " F(i)
MsgBox x`tmale`tfemale`n%t%

F(n) {
   Return n ? n - M(F(n-1)) : 1
}

M(n) {
   Return n ? n - F(M(n-1)) : 0
}
```


This one is an alternative to the above.


```AutoHotkey
main()
Return

F(n)
{
  If (n == 0)
    Return 1
  Else
    Return n - M(F(n-1))
}

M(n)
{
  If (n == 0)
    Return 0
  Else
    Return n - F(M(n-1)) ;
}

main()
{
  i = 0
  While, i < 20
  {
    male .= M(i) . "`n"
    female .= F(i) . "`n"
    i++
  }
  MsgBox % "male:`n" . male
  MsgBox % "female:`n" . female
}
```



## AWK

In AWK it is enough that both functions are defined somewhere. It matters not whether the BEGIN block is before or after the function definitions.


```awk
cat mutual_recursion.awk:
#!/usr/local/bin/gawk -f

# User defined functions
function F(n)
{ return n == 0 ? 1 : n - M(F(n-1)) }

function M(n)
{ return n == 0 ? 0 : n - F(M(n-1)) }

BEGIN {
  for(i=0; i <= 20; i++) {
    printf "%3d ", F(i)
  }
  print ""
  for(i=0; i <= 20; i++) {
    printf "%3d ", M(i)
  }
  print ""
}
```


```txt

$ awk -f mutual_recursion.awk
  1   1   2   2   3   3   4   5   5   6   6   7   8   8   9   9  10  11  11  12  13
  0   0   1   2   2   3   4   4   5   6   6   7   7   8   9   9  10  11  11  12  12

```



## BaCon


```freebasic
' Mutually recursive
FUNCTION F(int n) TYPE int
    RETURN IIF(n = 0, 1, n - M(F(n -1)))
END FUNCTION

FUNCTION M(int n) TYPE int
    RETURN IIF(n = 0, 0, n - F(M(n - 1)))
END FUNCTION

' Get iteration limit, default 20
SPLIT ARGUMENT$ BY " " TO arg$ SIZE args
limit = IIF(args > 1, VAL(arg$[1]), 20)

FOR i = 0 TO limit
    PRINT F(i) FORMAT "%2d "
NEXT
PRINT
FOR i = 0 TO limit
    PRINT M(i) FORMAT "%2d "
NEXT
PRINT
```


```txt
prompt$ ./mutually-recursive
 1  1  2  2  3  3  4  5  5  6  6  7  8  8  9  9 10 11 11 12 13
 0  0  1  2  2  3  4  4  5  6  6  7  7  8  9  9 10 11 11 12 12
```



## BASIC

```qbasic
DECLARE FUNCTION f! (n!)
DECLARE FUNCTION m! (n!)

FUNCTION f! (n!)
    IF n = 0 THEN
        f = 1
    ELSE
        f = m(f(n - 1))
    END IF
END FUNCTION

FUNCTION m! (n!)
    IF n = 0 THEN
        m = 0
    ELSE
        m = f(m(n - 1))
    END IF
END FUNCTION
```


=
## BBC BASIC
=

```bbcbasic
      @% = 3 : REM Column width
      PRINT "F sequence:"
      FOR i% = 0 TO 20
        PRINT FNf(i%) ;
      NEXT
      PRINT
      PRINT "M sequence:"
      FOR i% = 0 TO 20
        PRINT FNm(i%) ;
      NEXT
      PRINT
      END

      DEF FNf(n%) IF n% = 0 THEN = 1 ELSE = n% - FNm(FNf(n% - 1))

      DEF FNm(n%) IF n% = 0 THEN = 0 ELSE = n% - FNf(FNm(n% - 1))
```

```txt

F sequence:
  1  1  2  2  3  3  4  5  5  6  6  7  8  8  9  9 10 11 11 12 13
M sequence:
  0  0  1  2  2  3  4  4  5  6  6  7  7  8  9  9 10 11 11 12 12

```


==={{header|IS-BASIC}}===
<lang IS-BASIC>100 PROGRAM "Hofstad.bas"
110 PRINT "F sequence:"
120 FOR I=0 TO 20
130   PRINT F(I);
140 NEXT
150 PRINT :PRINT "M sequence:"
160 FOR I=0 TO 20
170   PRINT M(I);
180 NEXT
190 DEF F(N)
200   IF N=0 THEN
210     LET F=1
220   ELSE
230     LET F=N-M(F(N-1))
240   END IF
250 END DEF
260 DEF M(N)
270   IF N=0 THEN
280     LET M=0
290   ELSE
300     LET M=N-F(M(N-1))
310   END IF
320 END DEF
```



## Bc



```bc
cat mutual_recursion.bc:
define f(n) {
  if ( n == 0 ) return(1);
  return(n - m(f(n-1)));
}

define m(n) {
  if ( n == 0 ) return(0);
  return(n - f(m(n-1)));
}
```


POSIX bc doesn't have the <tt>print</tt> statement.


```bc
/* GNU bc */
for(i=0; i < 19; i++) {
  print f(i); print " ";
}
print "\n";
for(i=0; i < 19; i++) {
  print m(i); print " ";
}
print "\n";
quit
```


```txt

GNU bc mutual_recursion.bc
bc 1.06.95
Copyright 1991-1994, 1997, 1998, 2000, 2004, 2006 Free Software Foundation, Inc.
This is free software with ABSOLUTELY NO WARRANTY.
For details type `warranty'.
1 1 2 2 3 3 4 5 5 6 6 7 8 8 9 9 10 11 11 12 13
0 0 1 2 2 3 4 4 5 6 6 7 7 8 9 9 10 11 11 12 12

```



## Bracmat


```bracmat
 (F=.!arg:0&1|!arg+-1*M$(F$(!arg+-1)));
 (M=.!arg:0&0|!arg+-1*F$(M$(!arg+-1)));

 -1:?n&whl'(!n+1:~>20:?n&put$(F$!n " "))&put$\n
 1  1  2  2  3  3  4  5  5  6  6  7  8  8  9  9  10  11  11  12  13

 -1:?n&whl'(!n+1:~>20:?n&put$(M$!n " "))&put$\n
 0  0  1  2  2  3  4  4  5  6  6  7  7  8  9  9  10  11  11  12  12
```



## Brat


```brat
female = null #yes, this is necessary

male = { n |
  true? n == 0
    { 0 }
    { n - female male(n - 1) }
}

female = { n |
  true? n == 0
    { 1 }
    { n - male female(n - 1 ) }
}

p 0.to(20).map! { n | female n }
p 0.to(20).map! { n | male n }
```



## C


To let C see functions that will be used, it is enough to declare them. Normally this is done in a header file; in this example we do it directly in the code. If we do not declare them explicitly, they get an implicit declaration (if implicit declaration matches the use, everything's fine; but it is better however to write an explicit declaration)


```c
#include <stdio.h>
#include <stdlib.h>

/* let us declare our functions; indeed here we need
   really only M declaration, so that F can "see" it
   and the compiler won't complain with a warning */
int F(const int n);
int M(const int n);

int F(const int n)
{
  return (n == 0) ? 1 : n - M(F(n - 1));
}

int M(const int n)
{
  return (n == 0) ? 0 : n - F(M(n - 1));
}

int main(void)
{
  int i;
  for (i = 0; i < 20; i++)
    printf("%2d ", F(i));
  printf("\n");
  for (i = 0; i < 20; i++)
    printf("%2d ", M(i));
  printf("\n");
  return EXIT_SUCCESS;
}
```



## C++

C++ has prior declaration rules similar to those stated above for [[Mutual Recursion#C|C]], if we would use two functions. Instead here we define M and F as static (class) methods of a class, and specify the bodies inline in the declaration of the class. Inlined methods in the class can still call other methods or access fields in the class, no matter what order they are declared in, without any additional pre-declaration. This is possible because all the possible methods and fields are declared somewhere in the class declaration, which is known the first time the class declaration is parsed.

```cpp
#include <iostream>
#include <vector>
#include <iterator>

class Hofstadter
{
public:
  static int F(int n) {
    if ( n == 0 ) return 1;
    return n - M(F(n-1));
  }
  static int M(int n) {
    if ( n == 0 ) return 0;
    return n - F(M(n-1));
  }
};

using namespace std;

int main()
{
  int i;
  vector<int> ra, rb;

  for(i=0; i < 20; i++) {
    ra.push_back(Hofstadter::F(i));
    rb.push_back(Hofstadter::M(i));
  }
  copy(ra.begin(), ra.end(),
       ostream_iterator<int>(cout, " "));
  cout << endl;
  copy(rb.begin(), rb.end(),
       ostream_iterator<int>(cout, " "));
  cout << endl;
  return 0;
}
```


The following version shows better what's going on and why we ''seemingly'' didn't need pre-declaration (like C) when "encapsulating" the functions as static (class) methods.

This version is equivalent to the above but does not inline the definition of the methods into the definition of the class. Here the method declarations in the class definition serves as the "pre-declaration" for the methods, as in C.


```cpp
class Hofstadter
{
public:
  static int F(int n);
  static int M(int n);
};

int Hofstadter::F(int n)
{
  if ( n == 0 ) return 1;
  return n - M(F(n-1));
}

int Hofstadter::M(int n)
{
  if ( n == 0 ) return 0;
  return n - F(M(n-1));
}
```


## C#

```c#
namespace RosettaCode {
    class Hofstadter {
        static public int F(int n) {
            int result = 1;
            if (n > 0) {
                result = n - M(F(n-1));
            }

            return result;
        }

        static public int M(int n) {
            int result = 0;
            if (n > 0) {
                result = n - F(M(n - 1));
            }

            return result;
        }
    }
}
```



## Ceylon



```ceylon
Integer f(Integer n)
    =>  if (n > 0)
        then n - m(f(n-1))
        else 1;

Integer m(Integer n)
    =>  if (n > 0)
        then n - f(m(n-1))
        else 0;

shared void run() {
    printAll((0:20).map(f));
    printAll((0:20).map(m));
}
```


```txt

1, 1, 2, 2, 3, 3, 4, 5, 5, 6, 6, 7, 8, 8, 9, 9, 10, 11, 11, 12
0, 0, 1, 2, 2, 3, 4, 4, 5, 6, 6, 7, 7, 8, 9, 9, 10, 11, 11, 12

```



## Clojure



```lisp
(declare F) ; forward reference

(defn M [n]
  (if (zero? n)
    0
    (- n (F (M (dec n))))))

(defn F [n]
  (if (zero? n)
    1
    (- n (M (F (dec n))))))
```



## CoffeeScript


```coffeescript

F = (n) ->
  if n is 0 then 1 else n - M F n - 1

M = (n) ->
  if n is 0 then 0 else n - F M n - 1

console.log [0...20].map F
console.log [0...20].map M

```


<lang>
> coffee mutual_recurse.coffee
[ 1, 1, 2, 2, 3, 3, 4, 5, 5, 6, 6, 7, 8, 8, 9, 9, 10, 11, 11, 12 ]
[ 0, 0, 1, 2, 2, 3, 4, 4, 5, 6, 6, 7, 7, 8, 9, 9, 10, 11, 11, 12 ]

```



## Common Lisp



```lisp
(defun m (n)
    (if (zerop n)
        0
        (- n (f (m (- n 1))))))

(defun f (n)
    (if (zerop n)
        1
        (- n (m (f (- n 1))))))
```



## D


```d
import std.stdio, std.algorithm, std.range;

int male(in int n) pure nothrow {
    return n ? n - male(n - 1).female : 0;
}

int female(in int n) pure nothrow {
    return n ? n - female(n - 1).male : 1;
}

void main() {
    20.iota.map!female.writeln;
    20.iota.map!male.writeln;
}
```

```txt
[1, 1, 2, 2, 3, 3, 4, 5, 5, 6, 6, 7, 8, 8, 9, 9, 10, 11, 11, 12]
[0, 0, 1, 2, 2, 3, 4, 4, 5, 6, 6, 7, 7, 8, 9, 9, 10, 11, 11, 12]
```


=={{header|Déjà Vu}}==

```dejavu
F n:
	if n:
		- n M F -- n
	else:
		1

M n:
	if n:
		- n F M -- n
	else:
		0

for i range 0 10:
	!.( M i F i )
```

```txt
0 1
0 1
1 2
2 2
2 3
3 3
4 4
4 5
5 5
6 6
6 6
```



## Dart


```dart
int M(int n) => n==0?1:n-F(M(n-1));
int F(int n) => n==0?0:n-M(F(n-1));

main() {
  String f="",m="";
  for(int i=0;i<20;i++) {
    m+="${M(i)} ";
    f+="${F(i)} ";
  }
  print("M: $m");
  print("F: $f");
}
```



## Delphi


```Delphi

unit Hofstadter;

interface

type
  THofstadterFemaleMaleSequences = class
  public
    class function F(n: Integer): Integer;
    class function M(n: Integer): Integer;
  end;

implementation

class function THofstadterFemaleMaleSequences.F(n: Integer): Integer;
begin
  Result:= 1;
  if (n > 0) then
    Result:= n - M(F(n-1));
end;

class function THofstadterFemaleMaleSequences.M(n: Integer): Integer;
begin
  Result:= 0;
  if (n > 0) then
    Result:= n - F(M(n - 1));
end;

end.

```



## E


In E, nouns (variable names) always refer to preceding definitions, so to have mutual recursion, either one must be forward-declared or we must use a recursive def construct. Either one of these is syntactic sugar for first binding the noun to an E ''promise'' (a reference with an undetermined target), then ''resolving'' the promise to the value.

Recursive def:


```e
def [F, M] := [
  fn n { if (n <=> 0) { 1 } else { n - M(F(n - 1)) } },
  fn n { if (n <=> 0) { 0 } else { n - F(M(n - 1)) } },
]
```


Forward declaration:


```e
def M
def  F(n) { return if (n <=> 0) { 1 } else { n - M(F(n - 1)) } }
bind M(n) { return if (n <=> 0) { 0 } else { n - F(M(n - 1)) } }
```


<code>def M</code> binds <var>M</var> to a promise, and stashes the ''resolver'' for that promise where <code>bind</code> can get to it. When <code>def F...</code> is executed, the function F closes over the promise which is the value of M. <code>bind M...</code> uses the resolver to resolve <var>M</var> to the provided definition. The recursive def operates similarly, except that it constructs promises for every variable on the left side (<code>[F, M]</code>), executes the right side (<code>[fn ..., fn ...]</code>) and collects the values, then resolves each promise to its corresponding value.

But you don't have to worry about that to use it.

## Eiffel


```Eiffel

class
	APPLICATION

create
	make

feature

	make
			-- Test of the mutually recursive functions Female and Male.
		do
			across
				0 |..| 19 as c
			loop
				io.put_string (Female (c.item).out + " ")
			end
			io.new_line
			across
				0 |..| 19 as c
			loop
				io.put_string (Male (c.item).out + " ")
			end
		end

	Female (n: INTEGER): INTEGER
			-- Female sequence of the Hofstadter Female and Male sequences.
		require
			n_not_negative: n >= 0
		do
			Result := 1
			if n /= 0 then
				Result := n - Male (Female (n - 1))
			end
		end

	Male (n: INTEGER): INTEGER
			-- Male sequence of the Hofstadter Female and Male sequences.
		require
			n_not_negative: n >= 0
		do
			Result := 0
			if n /= 0 then
				Result := n - Female (Male (n - 1))
			end
		end

end

```

```txt

1 1 2 2 3 3 4 5 5 6 6 7 8 8 9 9 10 11 11 12
0 0 1 2 2 3 4 4 5 6 6 7 7 8 9 9 10 11 11 12

```


## Elena

ELENA 4.x :

```elena
import extensions;
import system'collections;

F = (n => (n == 0) ? 1 : (n - M(F(n-1))) );
M = (n => (n == 0) ? 0 : (n - F(M(n-1))) );

public program()
{
    var ra := new ArrayList();
    var rb := new ArrayList();

    for(int i := 0, i <= 19, i += 1)
    {
        ra.append(F(i));
        rb.append(M(i))
    };

    console.printLine(ra.asEnumerable());
    console.printLine(rb.asEnumerable())
}
```

```txt

1,1,2,2,3,3,4,5,5,6,6,7,8,8,9,9,10,11,11,12
0,0,1,2,2,3,4,4,5,6,6,7,7,8,9,9,10,11,11,12

```



## Elixir


```elixir
defmodule MutualRecursion do
  def f(0), do: 1
  def f(n), do: n - m(f(n - 1))
  def m(0), do: 0
  def m(n), do: n - f(m(n - 1))
end

IO.inspect Enum.map(0..19, fn n -> MutualRecursion.f(n) end)
IO.inspect Enum.map(0..19, fn n -> MutualRecursion.m(n) end)
```


```txt

[1, 1, 2, 2, 3, 3, 4, 5, 5, 6, 6, 7, 8, 8, 9, 9, 10, 11, 11, 12]
[0, 0, 1, 2, 2, 3, 4, 4, 5, 6, 6, 7, 7, 8, 9, 9, 10, 11, 11, 12]

```



## Erlang


```erlang
-module(mutrec).
-export([mutrec/0, f/1, m/1]).

f(0) -> 1;
f(N) -> N - m(f(N-1)).

m(0) -> 0;
m(N) -> N - f(m(N-1)).

mutrec() -> lists:map(fun(X) -> io:format("~w ", [f(X)]) end, lists:seq(0,19)),
	    io:format("~n", []),
	    lists:map(fun(X) -> io:format("~w ", [m(X)]) end, lists:seq(0,19)),
	    io:format("~n", []).
```



## Euphoria


```Euphoria
integer idM, idF

function F(integer n)
    if n = 0 then
        return 1
    else
        return n - call_func(idM,{F(n-1)})
    end if
end function

idF = routine_id("F")

function M(integer n)
    if n = 0 then
        return 0
    else
        return n - call_func(idF,{M(n-1)})
    end if
end function

idM = routine_id("M")
```


=={{header|F_Sharp|F#}}==


```fsharp
let rec f n =
    match n with
    | 0 -> 1
    | _ -> n - (m (f (n-1)))
and m n =
    match n with
    | 0 -> 0
    | _ -> n - (f (m (n-1)))
```


Like OCaml, the <code>let '''rec''' ''f'' .. '''and''' ''m'' ...</code> construct indicates that the functions call themselves (<code>'''rec'''</code>) and each other (<code>'''and'''</code>).


## Factor

In Factor, if you need a word before it's defined, you have to <code>DEFER:</code> it.
<lang>DEFER: F
: M ( n -- n' ) dup 0 = [ dup 1 - M F - ] unless ;
: F ( n -- n' ) dup 0 = [ drop 1 ] [ dup 1 - F M - ] if ;
```



## FALSE


```false
[$[$1-f;!m;!-1-]?1+]f:
[$[$1-m;!f;!-  ]?  ]m:
[0[$20\>][\$@$@!." "1+]#%%]t:
 f; t;!"
"m; t;!
```



## Fantom



```fantom

class Main
{
  static Int f (Int n)
  {
    if (n <= 0) // ensure n > 0
      return 1
    else
      return n - m(f(n-1))
  }

  static Int m (Int n)
  {
    if (n <= 0) // ensure n > 0
      return 0
    else
      return n - f(m(n-1))
  }

  public static Void main ()
  {
    50.times |Int n| { echo (f(n)) }
  }
}

```


=={{header|Fōrmulæ}}==

In [http://wiki.formulae.org/Mutual_recursion this] page you can see the solution of this task.

Fōrmulæ programs are not textual, visualization/edition of programs is done showing/manipulating structures but not text ([http://wiki.formulae.org/Editing_F%C5%8Drmul%C3%A6_expressions more info]). Moreover, there can be multiple visual representations of the same program. Even though it is possible to have textual representation &mdash;i.e. XML, JSON&mdash; they are intended for transportation effects more than visualization and edition.

The option to show Fōrmulæ programs and their results is showing images. Unfortunately images cannot be uploaded in Rosetta Code.


## Forth

Forward references required for mutual recursion may be set up using DEFER.

```forth
defer m

: f ( n -- n )
  dup 0= if 1+ exit then
  dup 1- recurse m - ;

:noname ( n -- n )
  dup 0= if exit then
  dup 1- recurse f - ;
is m

: test ( xt n -- ) cr 0 do i over execute . loop drop ;

' m defer@ 20 test \ 0 0 1 2 2 3 4 4 5 6 6 7 7 8 9 9 10 11 11 12
' f 20 test        \ 1 1 2 2 3 3 4 5 5 6 6 7 8 8 9 9 10 11 11 12
```



## Fortran


As long as the code of the two functions is inside the same "block" (module or program) we don't need special care. Otherwise, we should "load" at least the interface of the other function (each module will load mutually the other; of course the compiler won't enter in a infinite loop), e.g. by using a "<tt>use</tt>" (we do that if M and F function are inside different modules)

```fortran
module MutualRec
  implicit none
contains
  pure recursive function m(n) result(r)
    integer :: r
    integer, intent(in) :: n
    if ( n == 0 ) then
       r = 0
       return
    end if
    r = n - f(m(n-1))
  end function m

  pure recursive function f(n) result(r)
    integer :: r
    integer, intent(in) :: n
    if ( n == 0 ) then
       r = 1
       return
    end if
    r = n - m(f(n-1))
  end function f

end module
```


I've added the attribute <tt>pure</tt> so that we can use them in a <tt>forall</tt> statement.


```fortran
program testmutrec
  use MutualRec
  implicit none

  integer :: i
  integer, dimension(20) :: a = (/ (i, i=0,19) /), b = (/ (i, i=0,19) /)
  integer, dimension(20) :: ra, rb

  forall(i=1:20)
     ra(i) = m(a(i))
     rb(i) = f(b(i))
  end forall

  write(*,'(20I3)') rb
  write(*,'(20I3)') ra

end program testmutrec
```



## FreeBASIC


```freebasic
' FB 1.05.0 Win64

' Need forward declaration of M as it's used
' by F before its defined
Declare Function M(n As Integer) As Integer

Function F(n As Integer) As Integer
   If n = 0 Then
     Return 1
   End If
   Return n - M(F(n - 1))
End Function

Function M(n As Integer) As Integer
   If n = 0 Then
     Return 0
   End If
   Return n - F(M(n - 1))
End Function

Dim As Integer n = 24
Print "n :";
For i As Integer = 0 to n : Print Using "###"; i;    : Next
Print
Print String(78, "-")
Print "F :";
For i As Integer = 0 To n : Print Using "###"; F(i); : Next
Print
Print "M :";
For i As Integer = 0 To n : Print Using "###"; M(i); : Next
Print
Print "Press any key to quit"
Sleep
```


```txt

n :  0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24
------------------------------------------------------------------------------
F :  1  1  2  2  3  3  4  5  5  6  6  7  8  8  9  9 10 11 11 12 13 13 14 14 15
M :  0  0  1  2  2  3  4  4  5  6  6  7  7  8  9  9 10 11 11 12 12 13 14 14 15

```



## Go

It just works. No special pre-declaration is necessary.

```go
package main
import "fmt"

func F(n int) int {
  if n == 0 { return 1 }
  return n - M(F(n-1))
}

func M(n int) int {
  if n == 0 { return 0 }
  return n - F(M(n-1))
}

func main() {
  for i := 0; i < 20; i++ {
    fmt.Printf("%2d ", F(i))
  }
  fmt.Println()
  for i := 0; i < 20; i++ {
    fmt.Printf("%2d ", M(i))
  }
  fmt.Println()
}
```



## Groovy

Solution:

```groovy
def f, m  // recursive closures must be declared before they are defined
f = { n -> n == 0 ? 1 : n - m(f(n-1)) }
m = { n -> n == 0 ? 0 : n - f(m(n-1)) }
```


Test program:

```groovy
println 'f(0..20): ' + (0..20).collect { f(it) }
println 'm(0..20): ' + (0..20).collect { m(it) }
```


```txt
f(0..20): [1, 1, 2, 2, 3, 3, 4, 5, 5, 6, 6, 7, 8, 8, 9, 9, 10, 11, 11, 12, 13]
m(0..20): [0, 0, 1, 2, 2, 3, 4, 4, 5, 6, 6, 7, 7, 8, 9, 9, 10, 11, 11, 12, 12]
```



## Haskell

Haskell's definitions constructs (at the top level, or inside a <code>let</code> or <code>where</code> construct) are always mutually-recursive:

```haskell
f 0 = 1
f n | n > 0 = n - m (f $ n-1)

m 0 = 0
m n | n > 0 = n - f (m $ n-1)

main = do
       print $ map f [0..19]
       print $ map m [0..19]
```


=={{header|Icon}} and {{header|Unicon}}==

```Icon
procedure main(arglist)
every write(F(!arglist))   # F of all arguments
end

procedure F(n)
if integer(n) >= 0 then
   return (n = 0, 1) |  n - M(F(n-1))
end

procedure M(n)
if integer(n) >= 0 then
   return (0 = n) | n - F(M(n-1))
end
```



## Idris


```idris
mutual {
  F : Nat -> Nat
  F Z = (S Z)
  F (S n) = (S n) `minus` M(F(n))

  M : Nat -> Nat
  M Z = Z
  M (S n) = (S n) `minus` F(M(n))
}
```



## Io


```Io
f := method(n, if( n == 0, 1, n - m(f(n-1))))
m := method(n, if( n == 0, 0, n - f(m(n-1))))

Range
0 to(19) map(n,f(n)) println
0 to(19) map(n,m(n)) println
```

```txt
list(1, 1, 2, 2, 3, 3, 4, 5, 5, 6, 6, 7, 8, 8, 9, 9, 10, 11, 11, 12)
list(0, 0, 1, 2, 2, 3, 4, 4, 5, 6, 6, 7, 7, 8, 9, 9, 10, 11, 11, 12)
```



## J


```j
F =: 1:`(-  M @ $: @ <:) @.* M."0
M =: 0:`(-  F @ $: @ <:) @.* M."0
```


Example use:


```j
   F i. 20
1 1 2 2 3 3 4 5 5 6 6 7 8 8 9 9 10 11 11 12
```


That said, note that numbers are defined recursively, so other some approaches using numbers which give equivalent results should be acceptable.


## Java

```java5
public static int f(final int n)
{
 return n == 0 ? 1 : n - m(f(n - 1));
}

public static int m(final int n)
{
  return n == 0 ? 0 : n - f(m(n - 1));
}

public static void main(final String args[])
{
 for (int i = 0; i < 20; i++)
  System.out.println(f(i));
 System.out.println();
 for (i = 0; i < 20; i++)
  System.out.println(m(i));
}
```



## JavaScript


```JavaScript
function f(num) {
 return (num === 0) ? 1 : num - m(f(num - 1));
}

function m(num) {
 return (num === 0) ? 0 : num - f(m(num - 1));
}

function range(m, n) {
  return Array.apply(null, Array(n - m + 1)).map(
    function (x, i) { return m + i; }
  );
}

var a = range(0, 19);

//return a new array of the results and join with commas to print
console.log(a.map(function (n) { return f(n); }).join(', '));
console.log(a.map(function (n) { return m(n); }).join(', '));
```

```txt
1,1,2,2,3,3,4,5,5,6,6,7,8,8,9,9,10,11,11,12
0,0,1,2,2,3,4,4,5,6,6,7,7,8,9,9,10,11,11,12
```


ES6 implementation

```JavaScript>var f = num =
 (num === 0) ? 1 : num - m(f(num - 1));
var m = num => (num === 0) ? 0 : num - f(m(num - 1));

function range(m, n) {
  return Array.apply(null, Array(n - m + 1)).map(
    function (x, i) { return m + i; }
  );
}

var a = range(0, 19);

//return a new array of the results and join with commas to print
console.log(a.map(n => f(n)).join(', '));
console.log(a.map(n => m(n)).join(', '));
```


More ES6 implementation


```JavaScript
var range = (m, n) => Array(... Array(n - m + 1)).map((x, i) => m + i)
```



## jq

jq supports mutual recursion but requires functions to be defined before they are used.
In the present case, this can be accomplished by defining an inner function.

He we define F and M as arity-0 filters:

```jq

def M:
  def F: if . == 0 then 1 else . - ((. - 1) | F | M) end;
  if . == 0 then 0 else . - ((. - 1) | M | F) end;

def F:
  if . == 0 then 1 else . - ((. - 1) | F | M) end;
```
Example:
```jq

[range(0;20) | F],
[range(0;20) | M]
```

```sh
$ jq -n -c -f Mutual_recursion.jq

[1,1,2,2,3,3,4,5,5,6,6,7,8,8,9,9,10,11,11,12]
[0,0,1,2,2,3,4,4,5,6,6,7,7,8,9,9,10,11,11,12]
```



## Jsish


```javascript
/* Mutual recursion, is jsish */
function f(num):number { return (num === 0) ? 1 : num - m(f(num - 1)); }
function m(num):number { return (num === 0) ? 0 : num - f(m(num - 1)); }

function range(n=10, start=0, step=1):array {
   var a = Array(n).fill(0);
   for (var i in a) a[i] = start+i*step;
   return a;
}

var a = range(21);
puts(a.map(function (n) { return f(n); }).join(', '));
puts(a.map(function (n) { return m(n); }).join(', '));

/*
=!EXPECTSTART!=
1, 1, 2, 2, 3, 3, 4, 5, 5, 6, 6, 7, 8, 8, 9, 9, 10, 11, 11, 12, 13
0, 0, 1, 2, 2, 3, 4, 4, 5, 6, 6, 7, 7, 8, 9, 9, 10, 11, 11, 12, 12
=!EXPECTEND!=
*/
```


```txt
prompt$ jsish -u mutual-recursion.jsi
[PASS] mutual-recursion.jsi

prompt$ jsish mutual-recursion.jsi
1, 1, 2, 2, 3, 3, 4, 5, 5, 6, 6, 7, 8, 8, 9, 9, 10, 11, 11, 12
0, 0, 1, 2, 2, 3, 4, 4, 5, 6, 6, 7, 7, 8, 9, 9, 10, 11, 11, 12
```



## Julia


```julia
F(n) = n < 1 ? one(n) : n - M(F(n - 1))
M(n) = n < 1 ? zero(n) : n - F(M(n - 1))
```

```txt

julia> [F(i) for i = 0:19], [M(i) for i = 0:19]
([1,1,2,2,3,3,4,5,5,6,6,7,8,8,9,9,10,11,11,12],[0,0,1,2,2,3,4,4,5,6,6,7,7,8,9,9,10,11,11,12])

```



## Kotlin


```scala
// version 1.0.6

fun f(n: Int): Int =
    when {
        n == 0 -> 1
        else   -> n - m(f(n - 1))
    }

fun m(n: Int): Int =
    when {
        n == 0 -> 0
        else   -> n - f(m(n - 1))
    }

fun main(args: Array<String>) {
    val n = 24
    print("n :")
    for (i in 0..n) print("%3d".format(i))
    println()
    println("-".repeat(78))
    print("F :")
    for (i in 0..24) print("%3d".format(f(i)))
    println()
    print("M :")
    for (i in 0..24) print("%3d".format(m(i)))
    println()
}
```


```txt

n :  0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24
------------------------------------------------------------------------------
F :  1  1  2  2  3  3  4  5  5  6  6  7  8  8  9  9 10 11 11 12 13 13 14 14 15
M :  0  0  1  2  2  3  4  4  5  6  6  7  7  8  9  9 10 11 11 12 12 13 14 14 15

```



## Liberty BASIC


```lb

print "F sequence."
for i = 0 to 20
print f(i);" ";
next
print
print "M sequence."
for i = 0 to 20
print m(i);" ";
next

end

function f(n)
    if n = 0 then
        f = 1
    else
        f = n - m(f(n - 1))
    end if
    end function

function m(n)
    if n = 0 then
        m = 0
    else
        m = n - f(m(n - 1))
    end if
    end function

```

```txt
F sequence.
1 1 2 2 3 3 4 5 5 6 6 7 8 8 9 9 10 11 11 12 13
M sequence.
0 0 1 2 2 3 4 4 5 6 6 7 7 8 9 9 10 11 11 12 12
```



## LibreOffice Basic


```LibreOffice Basic
'// LibreOffice Basic Implementation of Hofstadter Female-Male sequences

'// Utility functions
sub setfont(strfont)
	ThisComponent.getCurrentController.getViewCursor.charFontName = strfont
end sub

sub newline
	oVC = thisComponent.getCurrentController.getViewCursor
	oText = oVC.text
	oText.insertControlCharacter(oVC, com.sun.star.text.ControlCharacter.PARAGRAPH_BREAK, False)
end sub

sub out(sString)
	oVC = ThisComponent.getCurrentController.getViewCursor
	oText = oVC.text
	oText.insertString(oVC, sString, false)
end sub

sub outln(optional sString)
	if not ismissing (sString) then out(sString)
	newline
end sub

function intformat(n as integer,nlen as integer) as string
	dim nstr as string
	nstr = CStr(n)
	while len(nstr) < nlen
		nstr = " " & nstr
	wend
	intformat = nstr
end function

'// Hofstadter Female-Male function definitions
function F(n as long) as long
	if n = 0 Then
		F =  1
	elseif n > 0 Then
		F = n - M(F(n - 1))
	endif
end function

function M(n)
	if n = 0 Then
		M = 0
	elseif n > 0 Then
		M = n - F(M(n - 1))
	endif
end function

'// Hofstadter Female Male sequence demo routine
sub Hofstadter_Female_Male_Demo
	'// Introductory Text
	setfont("LM Roman 10")
	outln("Rosetta Code Hofstadter Female and Male Sequence Challenge")
	outln
	out("Two functions are said to be mutually recursive if the first calls the second,")
	outln(" and in turn the second calls the first.")
	out("Write two mutually recursive functions that compute members of the Hofstadter")
	outln(" Female and Male sequences defined as:")
	outln
	setfont("LM Mono Slanted 10")
	outln(chr(9)+"F(0) = 1 ; M(0)=0")
	outln(chr(9)+"F(n) = n - M(F(n-1)), n > 0")
	outln(chr(9)+"M(n) = n - F(M(n-1)), n > 0")
	outln
	'// Sequence Generation
	const nmax as long = 20
	dim n as long
	setfont("LM Mono 10")
	out("n    = "
	for n = 0 to nmax
		out(" " + intformat(n, 2))
	next n
	outln
	out("F(n) = "
	for n = 0 to nmax
		out(" " + intformat(F(n),2))
	next n
	outln
	out("M(n) = "
	for n = 0 to nmax
		out(" " + intformat(M(n), 2))
	next n
	outln

end sub

------------------------------
Output
------------------------------
n    =   0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
F(n) =   1  1  2  2  3  3  4  5  5  6  6  7  8  8  9  9 10 11 11 12 13
M(n) =   0  0  1  2  2  3  4  4  5  6  6  7  7  8  9  9 10 11 11 12 12

```



## Logo

Like Lisp, symbols in Logo are late-bound so no special syntax is required for forward references.


```logo
to m :n
  if 0 = :n [output 0]
  output :n - f m :n-1
end
to f :n
  if 0 = :n [output 1]
  output :n - m f :n-1
end

show cascade 20 [lput m #-1 ?] []
[1 1 2 2 3 3 4 5 5 6 6 7 8 8 9 9 10 11 11 12]
show cascade 20 [lput f #-1 ?] []
[0 0 1 2 2 3 4 4 5 6 6 7 7 8 9 9 10 11 11 12]
```



## LSL

To test it yourself; rez a box on the ground, and add the following as a New Script.

```LSL
integer iDEPTH = 100;
integer f(integer n) {
	if(n==0) {
		return 1;
	} else {
		return n-m(f(n - 1));
	}
}
integer m(integer n) {
	if(n==0) {
		return 0;
	} else {
		return n-f(m(n - 1));
	}
}
default {
	state_entry() {
		integer x = 0;
		string s = "";
		for(x=0 ; x<iDEPTH ; x++) {
			s += (string)(f(x))+" ";
		}
		llOwnerSay(llList2CSV(llParseString2List(s, [" "], [])));
		s = "";
		for(x=0 ; x<iDEPTH ; x++) {
			s += (string)(m(x))+" ";
		}
		llOwnerSay(llList2CSV(llParseString2List(s, [" "], [])));
	}
}
```

```txt
1, 1, 2, 2, 3, 3, 4, 5, 5, 6, 6, 7, 8, 8, 9, 9, 10, 11, 11, 12, 13, 13, 14, 14, 15, 16, 16, 17, 17, 18, 19, 19, 20, 21, 21, 22, 22, 23, 24, 24, 25, 25, 26, 27, 27, 28, 29, 29, 30, 30, 31, 32, 32, 33, 34, 34, 35, 35, 36, 37, 37, 38, 38, 39, 40, 40, 41, 42, 42, 43, 43, 44, 45, 45, 46, 46, 47, 48, 48, 49, 50, 50, 51, 51, 52, 53, 53, 54, 55, 55, 56, 56, 57, 58, 58, 59, 59, 60, 61, 61
0, 0, 1, 2, 2, 3, 4, 4, 5, 6, 6, 7, 7, 8, 9, 9, 10, 11, 11, 12, 12, 13, 14, 14, 15, 16, 16, 17, 17, 18, 19, 19, 20, 20, 21, 22, 22, 23, 24, 24, 25, 25, 26, 27, 27, 28, 29, 29, 30, 30, 31, 32, 32, 33, 33, 34, 35, 35, 36, 37, 37, 38, 38, 39, 40, 40, 41, 42, 42, 43, 43, 44, 45, 45, 46, 46, 47, 48, 48, 49, 50, 50, 51, 51, 52, 53, 53, 54, 54, 55, 56, 56, 57, 58, 58, 59, 59, 60, 61, 61
```



## Lua


```lua

function m(n) return n > 0 and n - f(m(n-1)) or 0 end
function f(n) return n > 0 and n - m(f(n-1)) or 1 end
```


It is important to note, that if m and f are to be locally scoped functions rather than global, that they would need to be forward declared:


```lua

local m,n
function m(n) return n > 0 and n - f(m(n-1)) or 0 end
function f(n) return n > 0 and n - m(f(n-1)) or 1 end
```



## M2000 Interpreter

A function can call a global function and must be global to call it again by the second function

A group's function can call sibling function from same group. We can use This.F() or simply .f() to use group's f() member.

We can use subroutines, which can call each other, in a module, and we can use the modules stack of values to get results from subs. Subs running as parts of module, and see same variables and same stack of values. Arguments are local to sub, and we can define local variables too.

Last module export to clipboard and that used for output here.

```M2000 Interpreter

\\ set console 70 characters by 40 lines
Form 70, 40
Module CheckSubs {
      Flush
      Document one$, two$
      For i =0 to 20
            Print format$("{0::-3}",i);
            f(i)
            \\  number pop then top value of stack
            one$=format$("{0::-3}",number)
            m(i)
            two$=format$("{0::-3}",number)
      Next i
      Print
      Print one$
      Print two$
      Sub f(x)
            if x<=0 then Push 1 : Exit sub
            f(x-1)  ' leave result to for m(x)
            m()
            push x-number
      End Sub
      Sub m(x)
            if x<=0 then Push 0 : Exit sub
            m(x-1)
            f()
            push x-number
      End Sub
}
CheckSubs

Module Checkit {
      Function global f(n) {
            if n=0 then =1: exit
            if n>0 then  =n-m(f(n-1))
      }
      Function global m(n) {
            if n=0 then =0
            if n>0 then  =n-f(m(n-1))

      }
      Document one$, two$
      For i =0 to 20
            Print format$("{0::-3}",i);
            one$=format$("{0::-3}",f(i))
            two$=format$("{0::-3}",m(i))
      Next i
      Print
      Print one$
      Print two$
}
Checkit
Module Checkit2 {
      Group Alfa {
            function f(n) {
                  if n=0 then =1: exit
                  if n>0 then  =n-.m(.f(n-1))
            }
            function m(n) {
                  if n=0 then =0
                  if n>0 then  =n-.f(.m(n-1))
            }
      }
      Document one$, two$
      For i =0 to 20
            Print format$("{0::-3}",i);
            one$=format$("{0::-3}",Alfa.f(i))
            two$=format$("{0::-3}",Alfa.m(i))
      Next i
      Print
      Print one$
      Print two$
      Clipboard one$+{
      }+two$
}
Checkit2


```

```txt

  1  1  2  2  3  3  4  5  5  6  6  7  8  8  9  9 10 11 11 12 13
  0  0  1  2  2  3  4  4  5  6  6  7  7  8  9  9 10 11 11 12 12

```



## M4



```m4
define(`female',`ifelse(0,$1,1,`eval($1 - male(female(decr($1))))')')dnl
define(`male',`ifelse(0,$1,0,`eval($1 - female(male(decr($1))))')')dnl
define(`loop',`ifelse($1,$2,,`$3($1) loop(incr($1),$2,`$3')')')dnl
loop(0,20,`female')
loop(0,20,`male')
```



## Maple


```Maple
female_seq := proc(n)
	if (n = 0) then
		return 1;
	else
		return n - male_seq(female_seq(n-1));
	end if;
end proc;

male_seq  := proc(n)
	if (n = 0) then
		return 0;
	else
		return n - female_seq(male_seq(n-1));
	end if;
end proc;
seq(female_seq(i), i=0..10);
seq(male_seq(i), i=0..10);
```

```txt
1, 1, 2, 2, 3, 3, 4, 5, 5, 6, 6
0, 0, 1, 2, 2, 3, 4, 4, 5, 6, 6
```




## Mathematica

Without caching:

```Mathematica
f[0]:=1
m[0]:=0
f[n_]:=n-m[f[n-1]]
m[n_]:=n-f[m[n-1]]
```

With caching:

```Mathematica
f[0]:=1
m[0]:=0
f[n_]:=f[n]=n-m[f[n-1]]
m[n_]:=m[n]=n-f[m[n-1]]
```

Example finding f(1) to f(30) and m(1) to m(30):

```Mathematica
m /@ Range[30]
f /@ Range[30]
```

gives back:

```Mathematica
{0,1,2,2,3,4,4,5,6,6,7,7,8,9,9,10,11,11,12,12,13,14,14,15,16,16,17,17,18,19}
{1,2,2,3,3,4,5,5,6,6,7,8,8,9,9,10,11,11,12,13,13,14,14,15,16,16,17,17,18,19}
```



## MATLAB

female.m:

```MATLAB
function Fn = female(n)

    if n == 0
        Fn = 1;
        return
    end

    Fn = n - male(female(n-1));
end
```


male.m:

```MATLAB
function Mn = male(n)

    if n == 0
        Mn = 0;
        return
    end

    Mn = n - female(male(n-1));
end
```


```MATLAB>>
 n = (0:10);
>> arrayfun(@female,n)

ans =

     1     1     2     2     3     3     4     5     5     6     6

>> arrayfun(@male,n)

ans =

     0     0     1     2     2     3     4     4     5     6     6
```



## Maxima



```maxima
f[0]: 1$
m[0]: 0$
f[n] := n - m[f[n - 1]]$
m[n] := n - f[m[n - 1]]$

makelist(f[i], i, 0, 10);
[1, 1, 2, 2, 3, 3, 4, 5, 5, 6, 6]

makelist(m[i], i, 0, 10);
[0, 0, 1, 2, 2, 3, 4, 4, 5, 6, 6]

remarray(m, f)$

f(n) := if n = 0 then 1 else n - m(f(n - 1))$
m(n) := if n = 0 then 0 else n - f(m(n - 1))$

makelist(f(i), i, 0, 10);
[1, 1, 2, 2, 3, 3, 4, 5, 5, 6, 6]

makelist(m(i), i, 0, 10);
[0, 0, 1, 2, 2, 3, 4, 4, 5, 6, 6]

remfunction(f, m)$
```



## Mercury

<lang>
:- module mutual_recursion.
:- interface.

:- import_module io.
:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module int, list.

main(!IO) :-
   io.write(list.map(f, 0..19), !IO), io.nl(!IO),
   io.write(list.map(m, 0..19), !IO), io.nl(!IO).

:- func f(int) = int.

f(N) = ( if N = 0 then 1 else N - m(f(N - 1)) ).

:- func m(int) = int.

m(N) = ( if N = 0 then 0 else N - f(m(N - 1)) ).

```



## MMIX


```mmix
	LOC	Data_Segment

	GREG	@
NL	BYTE	#a,0
	GREG	@
buf	OCTA	0,0

t	IS	$128
Ja	IS	$127

	LOC #1000

	GREG @
// print 2 digits integer with trailing space to StdOut
// reg $3 contains int to be printed
bp	IS	$71
0H	GREG	#0000000000203020
prtInt	STO	0B,buf		% initialize buffer
	LDA	bp,buf+7	% points after LSD
				% REPEAT
1H	SUB	bp,bp,1		%  move buffer pointer
	DIV	$3,$3,10		%  divmod (x,10)
	GET	t,rR		%  get remainder
	INCL	t,'0'		%  make char digit
	STB	t,bp		%  store digit
	PBNZ	$3,1B		% UNTIL no more digits
	LDA	$255,bp
	TRAP	0,Fputs,StdOut	% print integer
	GO	Ja,Ja,0		% 'return'

// Female function
F	GET	$1,rJ		% save return addr
	PBNZ	$0,1F		% if N != 0 then F N
	INCL	$0,1		% F 0 = 1
	PUT	rJ,$1		% restore return addr
	POP	1,0		% return 1
1H	SUBU	$3,$0,1		% N1 = N - 1
	PUSHJ	$2,F		% do F (N - 1)
	ADDU	$3,$2,0		% place result in arg. reg.
	PUSHJ	$2,M		% do M F ( N - 1)
	PUT	rJ,$1		% restore ret addr
	SUBU	$0,$0,$2
	POP	1,0		% return N - M F ( N - 1 )

// Male function
M	GET	$1,rJ
	PBNZ	$0,1F
	PUT	rJ,$1
	POP	1,0		% return M 0 = 0
1H	SUBU	$3,$0,1
	PUSHJ	$2,M
	ADDU	$3,$2,0
	PUSHJ	$2,F
	PUT	rJ,$1
	SUBU	$0,$0,$2
	POP	1,0		$ return N - F M ( N - 1 )

// do a female run
Main	SET	$1,0		% for (i=0; i<25; i++){
1H	ADDU	$4,$1,0		%
	PUSHJ	$3,F		%  F (i)
	GO	Ja,prtInt	%  print F (i)
	INCL	$1,1
	CMP	t,$1,25
	PBNZ	t,1B		% }
	LDA	$255,NL
	TRAP	0,Fputs,StdOut
// do a male run
	SET	$1,0		% for (i=0; i<25; i++){
1H	ADDU	$4,$1,0		%
	PUSHJ	$3,M		%  M (i)
	GO	Ja,prtInt	%  print M (i)
	INCL	$1,1
	CMP	t,$1,25
	PBNZ	t,1B		% }
	LDA	$255,NL
	TRAP	0,Fputs,StdOut
	TRAP	0,Halt,0
```


 ~/MIX/MMIX/Rosetta> mmix mutualrecurs1
 1 1 2 2 3 3 4 5 5 6 6 7 8 8 9 9 10 11 11 12 13 13 14 14 15
 0 0 1 2 2 3 4 4 5 6 6 7 7 8 9 9 10 11 11 12 12 13 14 14 15


## Nemerle


```Nemerle
using System;
using System.Console;

module Hofstadter
{
    F(n : int) : int
    {
        |0 => 1
        |_ => n - M(F(n - 1))
    }

    M(n : int) : int
    {
        |0 => 0
        |_ => n - F(M(n - 1))
    }

    Main() : void
    {
        foreach (n in [0 .. 20]) Write("{0} ", F(n));
        WriteLine();
        foreach (n in [0 .. 20]) Write("{0} ", M(n));
    }
}
```



## Nim


```nim
proc m(n): int

proc f(n): int =
  if n == 0: 1
  else: n - m(f(n-1))

proc m(n): int =
  if n == 0: 0
  else: n - f(m(n-1))

for i in 1 .. 10:
  echo f(i)
  echo m(i)
```


=={{header|Objective-C}}==

Objective-C has prior declaration rules similar to those stated above for [[Mutual Recursion#C|C]], for C-like types. In this example we show the use of a two class method; this works since we need an <tt>interface</tt> block that is like declaration of functions in C code.


```objc>#import <Foundation/Foundation.h


@interface Hofstadter : NSObject
+ (int)M: (int)n;
+ (int)F: (int)n;
@end

@implementation Hofstadter
+ (int)M: (int)n
{
  if ( n == 0 ) return 0;
  return n - [self F: [self M: (n-1)]];
}
+ (int)F: (int)n
{
  if ( n == 0 ) return 1;
  return n - [self M: [self F: (n-1)]];
}
@end

int main()
{
  int i;

  for(i=0; i < 20; i++) {
    printf("%3d ", [Hofstadter F: i]);
  }
  printf("\n");
  for(i=0; i < 20; i++) {
    printf("%3d ", [Hofstadter M: i]);
  }
  printf("\n");
  return 0;
}
```



## Objeck

```objeck

class MutualRecursion {
  function : Main(args : String[]) ~ Nil {
    for(i := 0; i < 20; i+=1;) {
      f(i)->PrintLine();
    };
    "---"->PrintLine();
    for (i := 0; i < 20; i+=1;) {
      m(i)->PrintLine();
    };
  }

  function : f(n : Int) ~ Int {
    return n = 0 ? 1 : n - m(f(n - 1));
  }

  function : m(n : Int) ~ Int {
    return n = 0 ? 0 : n - f(m(n - 1));
  }
}

```



## OCaml


```ocaml
let rec f = function
  | 0 -> 1
  | n -> n - m(f(n-1))
and m = function
  | 0 -> 0
  | n -> n - f(m(n-1))
;;
```


The <code>let '''rec''' ''f'' ... '''and''' ''m'' ...</code> construct indicates that the functions call themselves (<code>'''rec'''</code>) and each other (<code>'''and'''</code>).


## Octave


We don't need to pre-declare or specify in some other way a function that will be defined later; but both must be declared before their use.

(The code is written to handle vectors, as the testing part shows)


```octave
function r = F(n)
  for i = 1:length(n)
    if (n(i) == 0)
      r(i) = 1;
    else
      r(i) = n(i) - M(F(n(i)-1));
    endif
  endfor
endfunction

function r = M(n)
  for i = 1:length(n)
    if (n(i) == 0)
      r(i) = 0;
    else
      r(i) = n(i) - F(M(n(i)-1));
    endif
  endfor
endfunction
```



```octave
# testing
ra = F([0:19]);
rb = M([0:19]);
disp(ra);
disp(rb);
```




## Oforth


Oforth can declare methods objects without any implementation. This allows to implement mutual recursion. This does not work with functions (declaration and implementation must be together).


```Oforth
Method new: M

Integer method: F
   self 0 == ifTrue: [ 1 return ]
   self self 1 - F M - ;

Integer method: M
   self 0 == ifTrue: [ 0 return ]
   self self 1 - M F - ;

0 20 seqFrom map(#F) println
0 20 seqFrom map(#M) println
```


```txt

[1, 1, 2, 2, 3, 3, 4, 5, 5, 6, 6, 7, 8, 8, 9, 9, 10, 11, 11, 12, 13]
[0, 0, 1, 2, 2, 3, 4, 4, 5, 6, 6, 7, 7, 8, 9, 9, 10, 11, 11, 12, 12]

```



## Ol

The `letrec` indicates that the definitions can be recursive, and fact that we placed these two in the same letrec block makes them mutually recursive.

```scheme

(letrec ((F (lambda (n)
               (if (= n 0) 1
                  (- n (M (F (- n 1)))))))
         (M (lambda (n)
               (if (= n 0) 0
                  (- n (F (M (- n 1))))))))
   (print (F 19)))
; produces 12

```



## Order


Since Order is powered by the C preprocessor, definitions follow the same rule as CPP macros: they can appear in any order relative to each other as long as all are defined before the ORDER_PP block that calls them.


```c
#include <order/interpreter.h>

#define ORDER_PP_DEF_8f                         \
ORDER_PP_FN(8fn(8N,                             \
                8if(8is_0(8N),                  \
                    1,                          \
                    8sub(8N, 8m(8f(8dec(8N)))))))

#define ORDER_PP_DEF_8m                         \
ORDER_PP_FN(8fn(8N,                             \
                8if(8is_0(8N),                  \
                    0,                          \
                    8sub(8N, 8f(8m(8dec(8N)))))))

//Test
ORDER_PP(8for_each_in_range(8fn(8N, 8print(8f(8N))), 0, 19))
ORDER_PP(8for_each_in_range(8fn(8N, 8print(8m(8N))), 0, 19))
```



## Oz


```oz
declare
  fun {F N}
     if N == 0 then 1
     elseif N > 0 then N - {M {F N-1}}
     end
  end

  fun {M N}
     if N == 0 then 0
     elseif N > 0 then N - {F {M N-1}}
     end
  end
in
  {Show {Map {List.number 0 9 1} F}}
  {Show {Map {List.number 0 9 1} M}}
```



## PARI/GP


```parigp
F(n)=if(n,n-M(F(n-1)),1)
M(n)=if(n,n-F(M(n-1)),0)
```



## Pascal


In Pascal we need to pre-declare functions/procedures; to do so, the <tt>forward</tt> statement is used.


```pascal
Program MutualRecursion;

{M definition comes after F which uses it}
function M(n : Integer) : Integer; forward;

function F(n : Integer) : Integer;
begin
   if n = 0 then
      F := 1
   else
      F := n - M(F(n-1));
end;

function M(n : Integer) : Integer;
begin
   if n = 0 then
      M := 0
   else
      M := n - F(M(n-1));
end;

var
   i : Integer;

begin
   for i := 0 to 19 do begin
      write(F(i) : 4)
   end;
   writeln;
   for i := 0 to 19 do begin
      write(M(i) : 4)
   end;
   writeln;
end.
```



## Perl


```perl
sub F { my $n = shift; $n ? $n - M(F($n-1)) : 1 }
sub M { my $n = shift; $n ? $n - F(M($n-1)) : 0 }

# Usage:
foreach my $sequence (\&F, \&M) {
    print join(' ', map $sequence->($_), 0 .. 19), "\n";
}
```

```txt

1 1 2 2 3 3 4 5 5 6 6 7 8 8 9 9 10 11 11 12
0 0 1 2 2 3 4 4 5 6 6 7 7 8 9 9 10 11 11 12

```



## Perl 6

A direct translation of the definitions of <math>F</math> and <math>M</math>:

```perl6
multi F(0) { 1 }; multi M(0) { 0 }
multi F(\𝑛) { 𝑛 - M(F(𝑛 - 1)) }
multi M(\𝑛) { 𝑛 - F(M(𝑛 - 1)) }

say map &F, ^20;
say map &M, ^20;
```

```txt

1 1 2 2 3 3 4 5 5 6 6 7 8 8 9 9 10 11 11 12
0 0 1 2 2 3 4 4 5 6 6 7 7 8 9 9 10 11 11 12

```



## Phix

You should normally explicitly declare forward routines (strictly necessary only when using optional or named parameters), since it often makes things easier to understand. There would be no point pre-declaring F, since it is not called before it is defined anyway.

```Phix
forward function M(integer n)

function F(integer n)
    return iff(n?n-M(F(n-1)):1)
end function

function M(integer n)
    return iff(n?n-F(M(n-1)):0)
end function

for i=0 to 20 do
    printf(1," %d",F(i))
end for
printf(1,"\n")
for i=0 to 20 do
    printf(1," %d",M(i))
end for
```

```txt

 1 1 2 2 3 3 4 5 5 6 6 7 8 8 9 9 10 11 11 12 13
 0 0 1 2 2 3 4 4 5 6 6 7 7 8 9 9 10 11 11 12 12

```



## PHP



```php
<?php
function F($n)
{
  if ( $n == 0 ) return 1;
  return $n - M(F($n-1));
}

function M($n)
{
  if ( $n == 0) return 0;
  return $n - F(M($n-1));
}

$ra = array();
$rb = array();
for($i=0; $i < 20; $i++)
{
  array_push($ra, F($i));
  array_push($rb, M($i));
}
echo implode(" ", $ra) . "\n";
echo implode(" ", $rb) . "\n";
?>
```



## PicoLisp


```PicoLisp
(de f (N)
   (if (=0 N)
      1
      (- N (m (f (dec N)))) ) )

(de m (N)
   (if (=0 N)
      0
      (- N (f (m (dec N)))) ) )
```



## PL/I


```PL/I
test: procedure options (main);

M: procedure (n) returns (fixed) recursive;    /* 8/1/2010 */
   declare n fixed;
   if n <= 0 then return (0);
   else return ( n - F(M(n-1)) );
end M;

F: procedure (n) returns (fixed) recursive;
   declare n fixed;
   if n <= 0 then return (1);
   else return ( n - M(F(n-1)) );
end F;

   declare i fixed;

   do i = 1 to 15;
      put skip list ( F(i), M(i) );
   end;
end test;
```



## PostScript

<lang>
/female{
/n exch def
n 0 eq
{1}
{
n n 1 sub female male sub
}ifelse
}def

/male{
/n exch def
n 0 eq
{0}
{
n n 1 sub male female sub
}ifelse
}def

```


```postscript

/F {
{
    {0 eq} {pop 1} is?
    {0 gt} {dup 1 sub F M sub} is?
} cond
}.

/M {
{
    {0 eq} {pop 0} is?
    {0 gt} {dup 1 sub M F sub} is?
} cond
}.


```



## PowerShell


```powershell
function F($n) {
    if ($n -eq 0) { return 1 }
    return $n - (M (F ($n - 1)))
}

function M($n) {
    if ($n -eq 0) { return 0 }
    return $n - (F (M ($n - 1)))
}
```



## Prolog


```prolog
female(0,1).
female(N,F) :- N>0,
	       N1 is N-1,
	       female(N1,R),
	       male(R, R1),
	       F is N-R1.

male(0,0).
male(N,F) :- N>0,
	     N1 is N-1,
	     male(N1,R),
	     female(R, R1),
	     F is N-R1.
```


```prolog
flist(S) :- for(X, 0, S), female(X, R), format('~d ', [R]), fail.
mlist(S) :- for(X, 0, S), male(X, R), format('~d ', [R]), fail.
```


'''Testing'''


```txt
| ?- flist(19).
1 1 2 2 3 3 4 5 5 6 6 7 8 8 9 9 10 11 11 12

no
| ?- mlist(19).
0 0 1 2 2 3 4 4 5 6 6 7 7 8 9 9 10 11 11 12
```



## Pure

The Pure definitions very closely maps to the mathematical definitions.


```pure
F 0 = 1;
M 0 = 0;
F n = n - M(F(n-1)) if n>0;
M n = n - F(M(n-1)) if n>0;
```



```pure>
 let females = map F (0..10); females;
[1,1,2,2,3,3,4,5,5,6,6]
> let males = map M (0..10); males;
[0,0,1,2,2,3,4,4,5,6,6]
```


## PureBasic


```PureBasic
Declare M(n)

Procedure F(n)
  If n = 0
    ProcedureReturn 1
  ElseIf n > 0
    ProcedureReturn n - M(F(n - 1))
  EndIf
EndProcedure

Procedure M(n)
  If n = 0
    ProcedureReturn 0
  ElseIf n > 0
    ProcedureReturn n - F(M(n - 1))
  EndIf
EndProcedure

Define i
If OpenConsole()

  For i = 0 To 19
    Print(Str(F(i)))
    If i = 19
      Continue
    EndIf
    Print(", ")
  Next

  PrintN("")
  For i = 0 To 19
    Print(Str(M(i)))
    If i = 19
      Continue
    EndIf
    Print(", ")
  Next

  Print(#CRLF$ + #CRLF$ + "Press ENTER to exit")
  Input()
  CloseConsole()
EndIf
```

```txt
1, 1, 2, 2, 3, 3, 4, 5, 5, 6, 6, 7, 8, 8, 9, 9, 10, 11, 11, 12
0, 0, 1, 2, 2, 3, 4, 4, 5, 6, 6, 7, 7, 8, 9, 9, 10, 11, 11, 12
```



## Python

{{works with|Python|3.0}}.
```python
def F(n): return 1 if n == 0 else n - M(F(n-1))
def M(n): return 0 if n == 0 else n - F(M(n-1))

print ([ F(n) for n in range(20) ])
print ([ M(n) for n in range(20) ])
```


```txt
[1, 1, 2, 2, 3, 3, 4, 5, 5, 6, 6, 7, 8, 8, 9, 9, 10, 11, 11, 12]
[0, 0, 1, 2, 2, 3, 4, 4, 5, 6, 6, 7, 7, 8, 9, 9, 10, 11, 11, 12]
```


In python there is no need to pre-declare ''M'' for it to be used in the definition of ''F''. (However ''M'' must be defined before ''F'' calls it).


## R


```R
F <- function(n) ifelse(n == 0, 1, n - M(F(n-1)))
M <- function(n) ifelse(n == 0, 0, n - F(M(n-1)))
```



```R
print.table(lapply(0:19, M))
print.table(lapply(0:19, F))
```



## REBOL


```REBOL
REBOL [
    Title: "Mutual Recursion"
    URL: http://rosettacode.org/wiki/Mutual_Recursion
	References: [http://en.wikipedia.org/wiki/Hofstadter_sequence#Hofstadter_Female_and_Male_sequences]
]

f: func [
	"Female."
	n [integer!] "Value."
] [either 0 = n [1][n - m f n - 1]]

m: func [
	"Male."
	n [integer!] "Value."
] [either 0 = n [0][n - f m n - 1]]

fs: []  ms: []  for i 0 19 1 [append fs f i  append ms m i]
print ["F:" mold fs  crlf  "M:" mold ms]
```


```txt
F: [1 1 2 2 3 3 4 5 5 6 6 7 8 8 9 9 10 11 11 12]
M: [0 0 1 2 2 3 4 4 5 6 6 7 7 8 9 9 10 11 11 12]
```



## Racket


```Racket
#lang racket
(define (F n)
  (if (>= 0 n)
      1
      (- n (M (F (sub1 n))))))

(define (M n)
  (if (>= 0 n)
      0
      (- n (F (M (sub1 n))))))
```


## REXX


### vanilla

This version uses vertical formatting of the output.

```rexx
/*REXX program shows  mutual recursion  (via the Hofstadter Male and Female sequences). */
parse arg lim .;       if lim=''  then lim=40;    w=length(lim);      pad=left('', 20)

     do j=0  to lim;   jj=right(j, w);    ff=right(F(j), w);          mm=right(M(j), w)
     say   pad     'F('jj") ="            ff   pad   'M('jj") ="      mm
     end   /*j*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
F: procedure;  parse arg n;   if n==0  then return 1;           return  n - M( F(n-1) )
M: procedure;  parse arg n;   if n==0  then return 0;           return  n - F( M(n-1) )
```

'''output'''   when using the default input of:   <tt> 40 </tt>

```txt

                     F( 0) =  1                      M( 0) =  0
                     F( 1) =  1                      M( 1) =  0
                     F( 2) =  2                      M( 2) =  1
                     F( 3) =  2                      M( 3) =  2
                     F( 4) =  3                      M( 4) =  2
                     F( 5) =  3                      M( 5) =  3
                     F( 6) =  4                      M( 6) =  4
                     F( 7) =  5                      M( 7) =  4
                     F( 8) =  5                      M( 8) =  5
                     F( 9) =  6                      M( 9) =  6
                     F(10) =  6                      M(10) =  6
                     F(11) =  7                      M(11) =  7
                     F(12) =  8                      M(12) =  7
                     F(13) =  8                      M(13) =  8
                     F(14) =  9                      M(14) =  9
                     F(15) =  9                      M(15) =  9
                     F(16) = 10                      M(16) = 10
                     F(17) = 11                      M(17) = 11
                     F(18) = 11                      M(18) = 11
                     F(19) = 12                      M(19) = 12
                     F(20) = 13                      M(20) = 12
                     F(21) = 13                      M(21) = 13
                     F(22) = 14                      M(22) = 14
                     F(23) = 14                      M(23) = 14
                     F(24) = 15                      M(24) = 15
                     F(25) = 16                      M(25) = 16
                     F(26) = 16                      M(26) = 16
                     F(27) = 17                      M(27) = 17
                     F(28) = 17                      M(28) = 17
                     F(29) = 18                      M(29) = 18
                     F(30) = 19                      M(30) = 19
                     F(31) = 19                      M(31) = 19
                     F(32) = 20                      M(32) = 20
                     F(33) = 21                      M(33) = 20
                     F(34) = 21                      M(34) = 21
                     F(35) = 22                      M(35) = 22
                     F(36) = 22                      M(36) = 22
                     F(37) = 23                      M(37) = 23
                     F(38) = 24                      M(38) = 24
                     F(39) = 24                      M(39) = 24
                     F(40) = 25                      M(40) = 25

```



### with memoization

This version uses memoization as well as a horizontal (aligned) output format.


The optimization due to memoization is faster by many orders of magnitude.

```rexx
/*REXX program shows  mutual recursion  (via the Hofstadter Male and Female sequences). */
parse arg lim .;      if lim==''  then lim=40             /*assume the default for LIM? */
w=length(lim);        $m.=.;    $m.0=0;     $f.=.;    $f.0=1;     Js=;     Fs=;     Ms=

               do j=0  to lim
               Js=Js right(j, w);      Fs=Fs right(F(j), w);      Ms=Ms right(M(j), w)
               end   /*j*/
say 'Js='  Js                                    /*display the list of  Js  to the term.*/
say 'Fs='  Fs                                    /*   "     "    "   "  Fs   "  "    "  */
say 'Ms='  Ms                                    /*   "     "    "   "  Ms   "  "    "  */
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
F: procedure expose $m. $f.; parse arg n;  if $f.n==. then $f.n=n-M(F(n-1));   return $f.n
M: procedure expose $m. $f.; parse arg n;  if $m.n==. then $m.n=n-F(M(n-1));   return $m.n
```

'''output'''   when using the default input of:   <tt> 99 </tt>

```txt

Js=  0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50 51 52 53 54 55 56 57 58 59 60 61 62 63 64 65 66 67 68 69 70 71 72 73 74 75 76 77 78 79 80 81 82 83 84 85 86 87 88 89 90 91 92 93 94 95 96 97 98 99
Fs=  1  1  2  2  3  3  4  5  5  6  6  7  8  8  9  9 10 11 11 12 13 13 14 14 15 16 16 17 17 18 19 19 20 21 21 22 22 23 24 24 25 25 26 27 27 28 29 29 30 30 31 32 32 33 34 34 35 35 36 37 37 38 38 39 40 40 41 42 42 43 43 44 45 45 46 46 47 48 48 49 50 50 51 51 52 53 53 54 55 55 56 56 57 58 58 59 59 60 61 61
Ms=  0  0  1  2  2  3  4  4  5  6  6  7  7  8  9  9 10 11 11 12 12 13 14 14 15 16 16 17 17 18 19 19 20 20 21 22 22 23 24 24 25 25 26 27 27 28 29 29 30 30 31 32 32 33 33 34 35 35 36 37 37 38 38 39 40 40 41 42 42 43 43 44 45 45 46 46 47 48 48 49 50 50 51 51 52 53 53 54 54 55 56 56 57 58 58 59 59 60 61 61

```


===with memoization, specific entry===
This version is identical in function to the previous example, but it also can compute and

display a specific request (indicated by a negative number for the argument).

```rexx
/*REXX program shows  mutual recursion  (via the Hofstadter Male and Female sequences). */
/*───────────────── If LIM is negative, a single result is shown for the abs(lim) entry.*/

parse arg lim .;      if lim==''  then lim=99;        aLim=abs(lim)
w=length(aLim);       $m.=.;    $m.0=0;     $f.=.;    $f.0=1;     Js=;     Fs=;     Ms=

               do j=0  to Alim
               Js=Js right(j, w);      Fs=Fs right(F(j), w);       Ms=Ms right(M(j), w)
               end   /*j*/

if lim>0  then  say 'Js='   Js;        else  say  'J('aLim")="     word(Js, aLim+1)
if lim>0  then  say 'Fs='   Fs;        else  say  'F('aLim")="     word(Fs, aLim+1)
if lim>0  then  say 'Ms='   Ms;        else  say  'M('aLim")="     word(Ms, aLim+1)
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
F: procedure expose $m. $f.; parse arg n;  if $f.n==. then $f.n=n-M(F(n-1));   return $f.n
M: procedure expose $m. $f.; parse arg n;  if $m.n==. then $m.n=n-F(M(n-1));   return $m.n
```

'''output'''   when using the input of:   <tt> -70000 </tt>

```txt

J(70000)= 70000
F(70000)= 43262
M(70000)= 43262

```

'''output'''   when using the input of a negative   <big>¼</big>   million:   <tt> -250000 </tt>

```txt

J(250000)= 250000
F(250000)= 154509
M(250000)= 154509

```



## Ring


```ring

see "F sequence : "
for i = 0 to 20
    see "" + f(i) + " "
next
see nl
see "M sequence : "
for i = 0 to 20
    see "" + m(i) + " "
next

func f n
     fr = 1
     if n != 0 fr = n - m(f(n - 1)) ok
     return fr

func m n
     mr = 0
     if n != 0 mr = n - f(m(n - 1)) ok
     return mr

```



## Ruby


```ruby
def F(n)
  n == 0 ? 1 : n - M(F(n-1))
end
def M(n)
  n == 0 ? 0 : n - F(M(n-1))
end

p (Array.new(20) {|n| F(n) })
p (Array.new(20) {|n| M(n) })
```


```txt
[1, 1, 2, 2, 3, 3, 4, 5, 5, 6, 6, 7, 8, 8, 9, 9, 10, 11, 11, 12]
[0, 0, 1, 2, 2, 3, 4, 4, 5, 6, 6, 7, 7, 8, 9, 9, 10, 11, 11, 12]
```


In ruby there is no need to pre-declare ''M'' for it to be used in the definition of ''F''. (However ''M'' must be defined before ''F'' calls it).


## Run BASIC


```Runbasic
print "F sequence:";
for i = 0 to 20
  print f(i);" ";
next i
print :print "M sequence:";
for i = 0 to 20
  print m(i);" ";
next i
end

function f(n)
 f = 1
 if n <> 0 then f = n - m(f(n - 1))
end function

function m(n)
 m = 0
 if n <> 0 then m = n - f(m(n - 1))
end function
```

```txt
F sequence:1 1 2 2 3 3 4 5 5 6 6 7 8 8 9 9 10 11 11 12 13
M sequence:0 0 1 2 2 3 4 4 5 6 6 7 7 8 9 9 10 11 11 12 12
```



## Rust


```rust
fn f(n: u32) -> u32 {
    match n {
        0 => 1,
        _ => n - m(f(n - 1))
    }
}

fn m(n: u32) -> u32 {
    match n {
        0 => 0,
        _ => n - f(m(n - 1))
    }
}

fn main() {
    for i in (0..20).map(f) {
        print!("{} ", i);
    }
    println!("");

    for i in (0..20).map(m) {
        print!("{} ", i);
    }
    println!("")
}
```

```txt
1 1 2 2 3 3 4 5 5 6 6 7 8 8 9 9 10 11 11 12
0 0 1 2 2 3 4 4 5 6 6 7 7 8 9 9 10 11 11 12
```



=={{header|S-lang}}==
<lang S-lang>% Forward definitions: [also deletes any existing definition]
define f();
define m();

define f(n) {
  if (n == 0) return 1;
  else if (n < 0) error("oops");
  return n - m(f(n - 1));
}

define m(n) {
  if (n == 0) return 0;
  else if (n < 0) error("oops");
  return n - f(m(n - 1));
}

foreach $1 ([0:19])
  () = printf("%d  ", f($1));
() = printf("\n");
foreach $1 ([0:19])
  () = printf("%d  ", m($1));
() = printf("\n");
```

```txt
1  1  2  2  3  3  4  5  5  6  6  7  8  8  9  9  10  11  11  12
0  0  1  2  2  3  4  4  5  6  6  7  7  8  9  9  10  11  11  12
```



## Sather


```sather
class MAIN is

  f(n:INT):INT
    pre n >= 0
  is
    if n = 0 then return 1; end;
    return n - m(f(n-1));
  end;

  m(n:INT):INT
    pre n >= 0
  is
    if n = 0 then return 0; end;
    return n - f(m(n-1));
  end;

  main is
    loop i ::= 0.upto!(19);
      #OUT + #FMT("%2d ", f(i));
    end;
    #OUT + "\n";
    loop i ::= 0.upto!(19);
      #OUT + #FMT("%2d ", m(i));
    end;
  end;
end;
```


There's no need to pre-declare F or M.


## Scala


```scala
def F(n:Int):Int =
  if (n == 0) 1 else n - M(F(n-1))
def M(n:Int):Int =
  if (n == 0) 0 else n - F(M(n-1))

println((0 until 20).map(F).mkString(", "))
println((0 until 20).map(M).mkString(", "))
```


```txt
1, 1, 2, 2, 3, 3, 4, 5, 5, 6, 6, 7, 8, 8, 9, 9, 10, 11, 11, 12
0, 0, 1, 2, 2, 3, 4, 4, 5, 6, 6, 7, 7, 8, 9, 9, 10, 11, 11, 12
```



## Scheme

<code>define</code> declarations are automatically mutually recursive:

```scheme
(define (F n)
  (if (= n 0) 1
      (- n (M (F (- n 1))))))

(define (M n)
  (if (= n 0) 0
      (- n (F (M (- n 1))))))
```


If you wanted to use a <code>let</code>-like construct to create local bindings, you would do the following. The <code>define</code> construct above is just a syntactic sugar for the following where the entire rest of the scope is used as the body.

```scheme
(letrec ((F (lambda (n)
              (if (= n 0) 1
                  (- n (M (F (- n 1)))))))
         (M (lambda (n)
              (if (= n 0) 0
                  (- n (F (M (- n 1))))))))
  (F 19)) # evaluates to 12
```


The <code>letrec</code> indicates that the definitions can be recursive, and fact that we placed these two in the same <code>letrec</code> block makes them mutually recursive.


## Seed7


```seed7
$ include "seed7_05.s7i";

const func integer: m (in integer: n) is forward;

const func integer: f (in integer: n) is func
  result
    var integer: res is 0;
  begin
    if n = 0 then
      res := 1;
    else
      res := n - m(f(n - 1));
    end if;
  end func;

const func integer: m (in integer: n) is func
  result
    var integer: res is 0;
  begin
    if n = 0 then
      res := 0;
    else
      res := n - f(m(n - 1));
    end if;
  end func;

const proc: main is func
  local
    var integer: i is 0;
  begin
    for i range 0 to 19 do
      write(f(i) lpad 3);
    end for;
    writeln;
    for i range 0 to 19 do
      write(m(i) lpad 3);
    end for;
    writeln;
  end func;
```


```txt

  1  1  2  2  3  3  4  5  5  6  6  7  8  8  9  9 10 11 11 12
  0  0  1  2  2  3  4  4  5  6  6  7  7  8  9  9 10 11 11 12

```



## Sidef


```ruby
func F(){}
func M(){}
 
F = func(n) { n > 0 ? (n - M(F(n-1))) : 1 }
M = func(n) { n > 0 ? (n - F(M(n-1))) : 0 }
 
[F, M].each { |seq|
    {|i| seq.call(i)}.map(^20).join(' ').say
}
```

```txt
1 1 2 2 3 3 4 5 5 6 6 7 8 8 9 9 10 11 11 12
0 0 1 2 2 3 4 4 5 6 6 7 7 8 9 9 10 11 11 12
```



## Smalltalk


Using block closure.


```smalltalk
|F M ra rb|

F := [ :n |
  (n == 0)
    ifTrue: [ 1 ]
    ifFalse: [ n - (M value: (F value: (n-1))) ]
].

M := [ :n |
  (n == 0)
    ifTrue: [ 0 ]
    ifFalse: [ n - (F value: (M value: (n-1))) ]
].

ra := OrderedCollection new.
rb := OrderedCollection new.
0 to: 19 do: [ :i |
  ra add: (F value: i).
  rb add: (M value: i)
].

ra displayNl.
rb displayNl.
```



## SNOBOL4



```SNOBOL4
        define('f(n)') :(f_end)
f       f = eq(n,0) 1 :s(return)
        f = n - m(f(n - 1)) :(return)
f_end

        define('m(n)') :(m_end)
m       m = eq(n,0) 0 :s(return)
        m = n - f(m(n - 1)) :(return)
m_end

*       # Test and display
L1      s1 = s1 m(i) ' ' ; s2 = s2 f(i) ' '
        i = le(i,25) i + 1 :s(L1)
        output = 'M: ' s1; output = 'F: ' s2
end
```


```txt
M: 0 0 1 2 2 3 4 4 5 6 6 7 7 8 9 9 10 11 11 12 12 13 14 14 15 16 16
F: 1 1 2 2 3 3 4 5 5 6 6 7 8 8 9 9 10 11 11 12 13 13 14 14 15 16 16
```



## SNUSP

The program shown calculates F(3) and demonstrates simple and mutual recursion.

```SNUSP
       /======\
F==!/=!\?\+#  | />-<-\
    |    \@\-@/@\===?/<#
    |      |    |
$+++/======|====/
    !    /=/       /+<<-\
    |    \!/======?\>>=?/<#  dup
    |      \<<+>+>-/
    !      !
    \======|====\
    |      |    |
    |  /===|==\ |
M==!\=!\?\#|  | |
         \@/-@/@/===?\<#
        ^       \>-<-/
        | ^  ^ ^ ^
        | |  | | subtract from n
        | |  | mutual recursion
        | |  recursion
        | n-1
        check for zero
```



## SPL


```spl
f(n)=
  ? n=0, <= 1
  <= n-m(f(n-1))
.
m(n)=
  ? n=0, <= 0
  <= n-f(m(n-1))
.
> i, 0..20
  fs += " "+f(i)
  ms += " "+m(i)
<
#.output("F:",fs)
#.output("M:",ms)
```

```txt

F: 1 1 2 2 3 3 4 5 5 6 6 7 8 8 9 9 10 11 11 12 13
M: 0 0 1 2 2 3 4 4 5 6 6 7 7 8 9 9 10 11 11 12 12

```



## Standard ML


```sml
fun f 0 = 1
  | f n = n - m (f (n-1))
and m 0 = 0
  | m n = n - f (m (n-1))
;
```


The <code>'''fun'''</code> construct creates recursive functions, and the <code>'''and'''</code> allows a group of functions to call each other. The above is just a shortcut for the following:


```sml>val rec f = fn 0 =
 1
             | n => n - m (f (n-1))
and m = fn 0 => 0
         | n => n - f (m (n-1))
;
```


which indicates that the functions call themselves (<code>'''rec'''</code>) and each other (<code>'''and'''</code>).


## Swift

It just works. No special pre-declaration is necessary.

```swift
func F(n: Int) -> Int {
  return n == 0 ? 1 : n - M(F(n-1))
}

func M(n: Int) -> Int {
  return n == 0 ? 0 : n - F(M(n-1))
}

for i in 0..20 {
  print("\(F(i)) ")
}
println()
for i in 0..20 {
  print("\(M(i)) ")
}
println()
```



## Tcl


```tcl
proc m {n} {
    if { $n == 0 } { expr 0; } else {
	expr {$n - [f [m [expr {$n-1}] ]]};
    }
}
proc f {n} {
    if { $n == 0 } { expr 1; } else {
	expr {$n - [m [f [expr {$n-1}] ]]};
    }
}

for {set i 0} {$i < 20} {incr i} {
    puts -nonewline [f $i];
    puts -nonewline " ";
}
puts ""
for {set i 0} {$i < 20} {incr i} {
    puts -nonewline [m $i];
    puts -nonewline " ";
}
puts ""
```


=={{header|TI-89 BASIC}}==


```ti89b
Define F(n) = when(n=0, 1, n - M(F(n - 1)))
Define M(n) = when(n=0, 0, n - F(M(n - 1)))
```



## TXR



```txrlisp
(defun f (n)
  (if (>= 0 n)
    1
    (- n (m (f (- n 1))))))

(defun m (n)
  (if (>= 0 n)
    0
    (- n (f (m (- n 1))))))

(each ((n (range 0 15)))
  (format t "f(~s) = ~s; m(~s) = ~s\n" n (f n) n (m n)))
```



```txt
$ txr mutual-recursion.txr
f(0) = 1; m(0) = 0
f(1) = 1; m(1) = 0
f(2) = 2; m(2) = 1
f(3) = 2; m(3) = 2
f(4) = 3; m(4) = 2
f(5) = 3; m(5) = 3
f(6) = 4; m(6) = 4
f(7) = 5; m(7) = 4
f(8) = 5; m(8) = 5
f(9) = 6; m(9) = 6
f(10) = 6; m(10) = 6
f(11) = 7; m(11) = 7
f(12) = 8; m(12) = 7
f(13) = 8; m(13) = 8
f(14) = 9; m(14) = 9
f(15) = 9; m(15) = 9
```



## uBasic/4tH

uBasic/4tH supports mutual recursion. However, the underlying system can't support the stress this puts on the stack - at least not for the full sequence. This version uses [https://en.wikipedia.org/wiki/Memoization memoization] to alleviate the stress and speed up execution.
<lang>LOCAL(1)                               ' main uses locals as well

FOR a@ = 0 TO 200                      ' set the array
  @(a@) = -1
NEXT

PRINT "F sequence:"                    ' print the F-sequence
FOR a@ = 0 TO 20
  PRINT FUNC(_f(a@));" ";
NEXT
PRINT

PRINT "M sequence:"                    ' print the M-sequence
FOR a@ = 0 TO 20
  PRINT FUNC(_m(a@));" ";
NEXT
PRINT

END


_f PARAM(1)                            ' F-function
  IF a@ = 0 THEN RETURN (1)            ' memoize the solution
  IF @(a@) < 0 THEN @(a@) = a@ - FUNC(_m(FUNC(_f(a@ - 1))))
RETURN (@(a@))                         ' return array element


_m PARAM(1)                            ' M-function
  IF a@ = 0 THEN RETURN (0)            ' memoize the solution
  IF @(a@+100) < 0 THEN @(a@+100) = a@ - FUNC(_f(FUNC(_m(a@ - 1))))
RETURN (@(a@+100))                     ' return array element
```

```txt
F sequence:
1 1 2 2 3 3 4 5 5 6 6 7 8 8 9 9 10 11 11 12 13
M sequence:
0 0 1 2 2 3 4 4 5 6 6 7 7 8 9 9 10 11 11 12 12

0 OK, 0:199
```


## UNIX Shell

```bash
M()
{
    local n
    n=$1
    if [[ $n -eq 0 ]]; then
	echo -n 0
    else
	echo -n $(( n - $(F $(M $((n-1)) ) ) ))
    fi
}

F()
{
    local n
    n=$1
    if [[ $n -eq 0 ]]; then
	echo -n 1
    else
	echo -n $(( n - $(M $(F $((n-1)) ) ) ))
    fi
}

for((i=0; i < 20; i++)); do
    F $i
    echo -n " "
done
echo
for((i=0; i < 20; i++)); do
    M $i
    echo -n " "
done
echo
```



## Ursala


Forward declarations are not an issue in Ursala, which allows any
definition to depend on any symbol declared within the same
scope. However, cyclic dependences are not accepted unless the
programmer explicitly accounts for their semantics. If the recurrence
can be solved using a fixed point combinator, the compiler can be
directed to use one by the <code>#fix</code> directive as shown, in this case
with one of a family of functional fixed point combinators from
a library. (There are easier ways to define these functions in Ursala
than by mutual recursion, but fixed points are useful for other things as well.)


```Ursala
#import std
#import nat
#import sol

#fix general_function_fixer 0

F = ~&?\1! difference^/~& M+ F+ predecessor
M = ~&?\0! difference^/~& F+ M+ predecessor
```

This test program applies both functions to the first
twenty natural numbers.

```Ursala
#cast %nLW

test = ^(F*,M*) iota 20
```

```txt

(
   <1,1,2,2,3,3,4,5,5,6,6,7,8,8,9,9,10,11,11,12>,
   <0,0,1,2,2,3,4,4,5,6,6,7,7,8,9,9,10,11,11,12>)
```



## VBA


```vb
Private Function F(ByVal n As Integer) As Integer
    If n = 0 Then
        F = 1
    Else
        F = n - M(F(n - 1))
    End If
End Function

Private Function M(ByVal n As Integer) As Integer
    If n = 0 Then
        M = 0
    Else
        M = n - F(M(n - 1))
    End If
End Function

Public Sub MR()
    Dim i As Integer
    For i = 0 To 20
        Debug.Print F(i);
    Next i
    Debug.Print
    For i = 0 To 20
        Debug.Print M(i);
    Next i
End Sub
```
```txt
 1  1  2  2  3  3  4  5  5  6  6  7  8  8  9  9  10  11  11  12  13
 0  0  1  2  2  3  4  4  5  6  6  7  7  8  9  9  10  11  11  12  12
```


## x86 Assembly

Since all "labels" (symbols), if not ''local'', can be seen by the whole code in the same source unit, we don't need special care to let the subroutine <tt>func_f</tt> call <tt>func_m</tt>. If the function would have been in another source unit, we should have declared it <tt>extern</tt> (the linker will resolve the symbol), as done for <tt>printf</tt>.

(It must be linked with the C standard library <tt>libc</tt> or similar and a startup code; lazyly a <tt>gcc mutrec.o</tt> works, being <tt>mutrec.o</tt> produced by e.g. <tt>nasm -f elf mutrec.asm</tt>)

```asm
	global	main
	extern	printf

	section	.text

func_f
	mov	eax, [esp+4]
	cmp	eax, 0
	jz	f_ret
	dec	eax
	push	eax
	call	func_f
	mov	[esp+0], eax
	call	func_m
	add	esp, 4
	mov	ebx, [esp+4]
	sub	ebx, eax
	mov	eax, ebx
	ret
f_ret
	mov	eax, 1
	ret

func_m
	mov	eax, [esp+4]
	cmp	eax, 0
	jz	m_ret
	dec	eax
	push	eax
	call	func_m
	mov	[esp+0], eax
	call	func_f
	add	esp, 4
	mov	ebx, [esp+4]
	sub	ebx, eax
	mov	eax, ebx
	ret
m_ret
	xor	eax, eax
	ret

main
	mov	edx, func_f
	call	output_res
	mov	edx, func_m
	call	output_res
	ret

output_res
	xor	ecx, ecx
loop0
	push	ecx
	call	edx

        push    edx

	push	eax
	push	form
	call	printf
	add	esp, 8

	pop     edx
        pop     ecx

	inc	ecx
	cmp	ecx, 20
	jnz	loop0

	push	newline
	call	printf
	add	esp, 4

	ret


	section	.rodata
form
	db	'%d ',0
newline
	db	10,0

	end
```



## XPL0


```XPL0
code    ChOut=8, CrLf=9, IntOut=11;

ffunc M; \forward-referenced function declaration

func F(N);
int N;
return if N=0 then 1 else N - M(F(N-1));

func M(N);
int N;
return if N=0 then 0 else N - F(M(N-1));

int I;
[for I:= 0 to 19 do [IntOut(0, F(I));  ChOut(0, ^ )];
CrLf(0);
 for I:= 0 to 19 do [IntOut(0, M(I));  ChOut(0, ^ )];
CrLf(0);
]
```


```txt

1 1 2 2 3 3 4 5 5 6 6 7 8 8 9 9 10 11 11 12
0 0 1 2 2 3 4 4 5 6 6 7 7 8 9 9 10 11 11 12

```



## Yabasic

```Yabasic
// User defined functions
sub F(n)
    if n = 0 return 1
    return n - M(F(n-1))
end sub


sub M(n)
   if n = 0 return 0
   return n - F(M(n-1))
end sub


for i = 0 to 20
    print F(i) using "###";
next
print
for i = 0 to 20
    print M(i) using "###";
next
print
```



## zkl

This works if the functions are in a file or on one line (in the REPL) as
zkl doesn't like referencing undefined objects. You could also pass/close the other function.

```zkl
fcn f(n){ if(n==0)return(1); n-m(f(n-1,m),f) }
fcn m(n){ if(n==0)return(0); n-f(m(n-1,f),m) }
[0..19].apply(f).println();  // or foreach n in ([0..19]){ print(f(n)," ") }
[0..19].apply(m).println();  // or foreach n in ([0..19]){ print(m(n)," ") }
```

```txt

L(1,1,2,2,3,3,4,5,5,6,6,7,8,8,9,9,10,11,11,12)
L(0,0,1,2,2,3,4,4,5,6,6,7,7,8,9,9,10,11,11,12)

```

