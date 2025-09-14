+++
title = "Compound data type"
description = ""
date = 2019-10-12T07:45:40Z
aliases = []
[extra]
id = 1969
[taxonomies]
categories = ["task", "Basic language learning"]
tags = []
languages = [
  "11l",
  "acl2",
  "actionscript",
  "ada",
  "algol_68",
  "algol_w",
  "amigae",
  "arm_assembly",
  "arturo",
  "autohotkey",
  "awk",
  "axe",
  "basic",
  "bbc_basic",
  "bracmat",
  "brlcad",
  "c",
  "clean",
  "clojure",
  "cobol",
  "coffeescript",
  "common_lisp",
  "cpp",
  "csharp",
  "d",
  "delphi",
  "e",
  "echolisp",
  "ela",
  "elena",
  "elixir",
  "elm",
  "erlang",
  "euphoria",
  "factor",
  "fantom",
  "forth",
  "fortran",
  "freebasic",
  "go",
  "groovy",
  "haskell",
  "idl",
  "j",
  "java",
  "javascript",
  "jq",
  "json",
  "julia",
  "konsolscript",
  "kotlin",
  "lasso",
  "lfe",
  "lingo",
  "logo",
  "lua",
  "maxima",
  "maxscript",
  "netrexx",
  "nim",
  "objeck",
  "ocaml",
  "oforth",
  "oorexx",
  "openedge_progress",
  "oxygenbasic",
  "oz",
  "pari_gp",
  "pascal",
  "perl",
  "perl_6",
  "phix",
  "php",
  "picolisp",
  "pl_i",
  "pop11",
  "powershell",
  "prolog",
  "purebasic",
  "python",
  "r",
  "racket",
  "related_tasks",
  "rexx",
  "ring",
  "ruby",
  "rust",
  "scala",
  "scheme",
  "seed7",
  "shen",
  "sidef",
  "simpol",
  "snobol4",
  "standard_ml",
  "stata",
  "swift",
  "tcl",
  "txr",
  "unix_shell",
  "ursala",
  "vba",
  "vim_script",
  "visual_basic_dotnet",
  "xslt",
  "zkl",
  "zonnon",
]
+++

## Task

Create a compound data type:
  <big> Point(x,y) </big>


A compound data type is one that holds multiple independent values.


## Related tasks

*   [[Enumeration]]


## 11l


```11l
T Point
   Int x, y

   F (x, y)
      .x = x
      .y = y
```



## ACL2


```Lisp
(defstructure point
   (x (:assert (rationalp x)))
   (y (:assert (rationalp y))))

(assign p1 (make-point :x 1 :y 2))
(point-x (@ p1)) ; Access the x value of the point
(assign p1 (update-point (@ p1) :x 3)) ; Update the x value
(point-x (@ p1))
(point-p (@ p1)) ; Recognizer for points
```


```txt
((:X . 1) (:Y . 2))
1
((:X . 3) (:Y . 2))
3
T
```



## ActionScript


```actionscript
package
{
    public class Point
    {
        public var x:Number;
        public var y:Number;

        public function Point(x:Number, y:Number)
        {
            this.x = x;
            this.y = y;
        }
    }
}
```



## Ada


### Tagged Type

Ada tagged types are extensible through inheritance. The reserved word ''tagged'' causes the compiler to create a tag for the type. The tag identifies the position of the type in an inheritance hierarchy.

```ada
type Point is tagged record
   X : Integer := 0;
   Y : Integer := 0;
end record;
```



### Record Type

Ada record types are not extensible through inheritance. Without the reserved word ''tagged'' the record does not belong to an inheritance hierarchy.

```ada
type Point is record
   X : Integer := 0;
   Y : Integer := 0;
end record;
```



### =Parameterized Types=

An Ada record type can contain a discriminant. The discriminant is used to choose between internal structural representations. Parameterized types were introduced to Ada before tagged types. Inheritance is generally a cleaner solution to multiple representations than is a parameterized type.

```ada
type Person (Gender : Gender_Type) is record
   Name   : Name_String;
   Age    : Natural;
   Weight : Float;
   Case Gender is
      when Male =>
         Beard_Length : Float;
      when Female =>
         null;
   end case;
end record;
```

In this case every person will have the attributes of gender, name, age, and weight. A person with a male gender will also have a beard length.


## ALGOL 68


### Tagged Type

ALGOL 68 has only tagged-union/discriminants.  And the tagging was strictly done by the ''type'' (MODE) of the members.

```algol68
MODE UNIONX = UNION(
   STRUCT(REAL r, INT i),
   INT,
   REAL,
   STRUCT(INT ii),
   STRUCT(REAL rr),
   STRUCT([]REAL r)
);
```

To extract the apropriate member of a UNION a '''conformity-clause''' has to be used.

```algol68
UNIONX data := 6.6;
CASE data IN
   (INT i): printf(($"r: "gl$,i)),
   (REAL r): printf(($"r: "gl$,r)),
   (STRUCT(REAL r, INT i) s): printf(($"r&i: "2(g)l$,s)),
   (STRUCT([]REAL r) s): printf(($"r: "n(UPB r OF s)(g)l$,s))
OUT
  printf($"Other cases"l$)
ESAC;
```

The '''conformity-clause''' does mean that ALGOL 68 avoids the need for
[[duck typing]], but it also makes the tagged-union kinda tough to use,
except maybe in certain special cases.

### Record Type

ALGOL 68 record types are not extensible through inheritance but they
may be part of a larger STRUCT composition.

```algol68
MODE POINT = STRUCT(
   INT x,
   INT y
);
```


### =Parameterized Types=

An ALGOL 68 record type can contain a tagged-union/discriminant. The
tagged-union/discriminant is used to choose between internal structural
representations.

```algol68
MODE PERSON = STRUCT(
   STRING name,
   REAL age,
   REAL weight,
   UNION (
      STRUCT (REAL beard length),
      VOID
   ) gender details
);
```

In this case every PERSON will have the attributes of gender details, name, age,
and weight. A PERSON may or may not have a beard.  The sex is implied by the tagging.


## ALGOL W


```algolw
begin
    % create the compound data type %
    record Point( real x, y );
    % declare a Point variable %
    reference(Point) p;
    % assign a value to p %
    p := Point( 1, 0.5 );
    % access the fields of p - note Algol W uses x(p) where many languages would use p.x %
    write( x(p), y(p) )
end.
```



## AmigaE


```amigae
OBJECT point
  x, y
ENDOBJECT

PROC main()
  DEF pt:PTR TO point,

  NEW pt
  -> Floats are also stored as integer types making
  -> the float conversion operator necessary.
  pt.x := !10.4
  pt.y := !3.14
  END pt
ENDPROC
```



## ARM Assembly

```ARM Assembly


/* ARM assembly Raspberry PI  */
/*  program structure.s   */

/************************************/
/* Constantes                       */
/************************************/
.equ STDOUT, 1     @ Linux output console
.equ EXIT,   1     @ Linux syscall
.equ WRITE,  4     @ Linux syscall

/*******************************************/
/* Structures                             */
/********************************************/
    .struct  0
point_x:                                        @ x coordinate
    .struct  point_x + 4
point_y:                                        @ y coordinate
    .struct  point_y + 4
point_end:                                      @ end structure point
/*********************************/
/* Initialized data              */
/*********************************/
.data
sMessResult:        .ascii "value x : "
sMessValeur:        .fill 11, 1, ' '            @ size => 11
szCarriageReturn:   .asciz "\n"


/*********************************/
/* UnInitialized data            */
/*********************************/
.bss
stPoint:           .skip point_end               @ reservation place in memory
/*********************************/
/*  code section                 */
/*********************************/
.text
.global main
main:                                             @ entry of program
    ldr r1,iAdrstPoint
    mov r0,#5                                     @ x value
    str r0,[r1,#point_x]
    mov r0,#10                                    @ y value
    str r0,[r1,#point_y]
                                                  @ display value
    ldr r2,iAdrstPoint
    ldr r0,[r2,#point_x]
    ldr r1,iAdrsMessValeur
    bl conversion10                               @ call conversion decimal
    ldr r0,iAdrsMessResult
    bl affichageMess                              @ display message


100:                                              @ standard end of the program
    mov r0, #0                                    @ return code
    mov r7, #EXIT                                 @ request to exit program
    svc #0                                        @ perform the system call

iAdrsMessValeur:          .int sMessValeur
iAdrszCarriageReturn:     .int szCarriageReturn
iAdrsMessResult:          .int sMessResult
iAdrstPoint:              .int stPoint

/******************************************************************/
/*     display text with size calculation                         */
/******************************************************************/
/* r0 contains the address of the message */
affichageMess:
    push {r0,r1,r2,r7,lr}                          @ save  registres
    mov r2,#0                                      @ counter length
1:                                                 @ loop length calculation
    ldrb r1,[r0,r2]                                @ read octet start position + index
    cmp r1,#0                                      @ if 0 its over
    addne r2,r2,#1                                 @ else add 1 in the length
    bne 1b                                         @ and loop
                                                   @ so here r2 contains the length of the message
    mov r1,r0                                      @ address message in r1
    mov r0,#STDOUT                                 @ code to write to the standard output Linux
    mov r7, #WRITE                                 @ code call system "write"
    svc #0                                         @ call systeme
    pop {r0,r1,r2,r7,lr}                           @ restaur des  2 registres */
    bx lr                                          @ return
/******************************************************************/
/*     Converting a register to a decimal unsigned                */
/******************************************************************/
/* r0 contains value and r1 address area   */
/* r0 return size of result (no zero final in area) */
/* area size => 11 bytes          */
.equ LGZONECAL,   10
conversion10:
    push {r1-r4,lr}                                 @ save registers
    mov r3,r1
    mov r2,#LGZONECAL

1:                                                  @ start loop
    bl divisionpar10U                               @ unsigned  r0 <- dividende. quotient ->r0 reste -> r1
    add r1,#48                                      @ digit
    strb r1,[r3,r2]                                 @ store digit on area
    cmp r0,#0                                       @ stop if quotient = 0
    subne r2,#1                                     @ else previous position
    bne 1b                                          @ and loop
                                                    @ and move digit from left of area
    mov r4,#0
2:
    ldrb r1,[r3,r2]
    strb r1,[r3,r4]
    add r2,#1
    add r4,#1
    cmp r2,#LGZONECAL
    ble 2b
                                                      @ and move spaces in end on area
    mov r0,r4                                         @ result length
    mov r1,#' '                                       @ space
3:
    strb r1,[r3,r4]                                   @ store space in area
    add r4,#1                                         @ next position
    cmp r4,#LGZONECAL
    ble 3b                                            @ loop if r4 <= area size

100:
    pop {r1-r4,lr}                                    @ restaur registres
    bx lr                                             @return

/***************************************************/
/*   division par 10   unsigned                    */
/***************************************************/
/* r0 dividende   */
/* r0 quotient    */
/* r1 remainder   */
divisionpar10U:
    push {r2,r3,r4, lr}
    mov r4,r0                                          @ save value
    ldr r3,iMagicNumber                                @ r3 <- magic_number    raspberry 1 2
    umull r1, r2, r3, r0                               @ r1<- Lower32Bits(r1*r0) r2<- Upper32Bits(r1*r0)
    mov r0, r2, LSR #3                                 @ r2 <- r2 >> shift 3
    add r2,r0,r0, lsl #2                               @ r2 <- r0 * 5
    sub r1,r4,r2, lsl #1                               @ r1 <- r4 - (r2 * 2)  = r4 - (r0 * 10)
    pop {r2,r3,r4,lr}
    bx lr                                              @ leave function
iMagicNumber:  	.int 0xCCCCCCCD


```



## Arturo



### Using a dictionary


```arturo
point #{
	x 10
	y 20
}

print point
```


```txt
#{ x 10, y 20 }
```



### Using a class



```arturo
Point #{
	x 0
	y 0

	init {
		x &0
		y &1
	}
}

point $(new ~Point 10 20)

print point
```


```txt
#{ init <function: 0x1077534A0>, x 10, y 20 }
```



## AutoHotkey

[[wp:Monkey_patch|monkeypatched]] example.


```AutoHotkey
point := Object()
point.x := 1
point.y := 0

```



## AWK

As usual, arrays are the only data type more complex than a number or a string.

Use quotes around constant strings as element selectors:

```awk
BEGIN {
	p["x"]=10
	p["y"]=42

	z = "ZZ"
	p[ z ]=999

	p[ 4 ]=5

	for (i in p) print( i, ":", p[i] )
}
```

```txt

4 : 5
x : 10
y : 42
ZZ : 999
```



## Axe

Axe does not have language support for custom data structures. However, they can be implemented from scratch using memory directly.

```axe
Lbl POINT
r₂→{r₁}ʳ
r₃→{r₁+2}ʳ
r₁
Return
```


To initialize a POINT at memory address L₁ with (x, y) = (5, 10):

```axe
POINT(L₁,5,10)
```


The caller must ensure the buffer has enough free space to contain the object (in this case, 4 bytes).


## BASIC

```qb
TYPE Point
   x AS INTEGER
   y AS INTEGER
 END TYPE
```



## BBC BASIC

```bbcbasic
      DIM Point{x%, y%}
```



## Bracmat

Normally, values are compounded by putting them in a tree structure. For examples, the values <code>3</code> and <code>4</code> can be put in a small tree <code>(3.4)</code>.
But since the task requires the values to be <i>independent</i>, the values must be changeable, which they are not in <code>(3.4)</code>.
So we go object oriented and create a 'type' Point. We show that <code>x</code> and <code>y</code> are independent by changing the value of <code>x</code> and checking that <code>y</code> didn't change.
Bracmat does not have other typing systems than duck typing. The variable <code>Point</code> is not a class, but an object in its own right. The <code>new$</code> function creates a copy of <code>Point</code>.

```Bracmat
( ( Point
  =   (x=)
      (y=)
      (new=.!arg:(?(its.x).?(its.y)))
  )
& new$(Point,(3.4)):?pt
& out$(!(pt..x) !(pt..y))
  { Show independcy by changing x, but not y }
& 7:?(pt..x)
& out$(!(pt..x) !(pt..y))
);
```

```txt
3 4
7 4
```



## Brlcad


In brlcad, the datatypes are geometric primitives or combinations. Here we create a lamp using a combination of previously created components:

 c lamp base stem bulb shade chord plug


## C



```c
typedef struct Point
{
  int x;
  int y;
} Point;
```


## C#


```c#
struct Point
{
  public int x, y;
  public Point(int x, int y) {
    this.x = x;
    this.y = y;
  }
}
```



## C++


```cpp
struct Point
{
  int x;
  int y;
};
```


It is also possible to add a constructor (this allows the use of <tt>Point(x, y)</tt> in expressions):

```cpp
struct Point
{
  int x;
  int y;
  Point(int ax, int ay): x(ax), y(ax) {}
};
```


Point can also be parametrized on the coordinate type:

```cpp
template<typename Coordinate>

struct point
{
  Coordinate x, y;
};

// A point with integer coordinates
Point<int> point1 = { 3, 5 };

// a point with floating point coordinates
Point<float> point2 = { 1.7, 3.6 };
```

Of course, a constructor can be added in this case as well.


## Clean


### Record type


```clean
:: Point = { x :: Int, y :: Int }
```


### Parameterized Algebraic type


```clean
:: Point a = Point a a  // usage: (Point Int)
```


### Synonym type


```clean
:: Point :== (Int, Int)
```



## Clojure


```clojure
(defrecord Point [x y])
```

This defines a datatype with constructor ''Point.'' and accessors '':x'' and '':y'' :

```clojure
(def p (Point. 0 1))
(assert (= 0 (:x p)))
(assert (= 1 (:y p)))
```



## COBOL


```cobol

01 Point.
   05 x            pic 9(3).
   05 y            pic 9(3).

```



## CoffeeScript


```coffeescript

# Lightweight JS objects (with CS sugar).
point =
  x: 5
  y: 3

console.log point.x, point.y # 5 3

# Heavier OO style
class Point
  constructor: (@x, @y) ->
  distance_from: (p2) ->
    dx = p2.x - @x
    dy = p2.y - @y
    Math.sqrt dx*dx + dy*dy

p1 = new Point(1, 6)
p2 = new Point(6, 18)
console.log p1 # { x: 1, y: 6 }
console.log p1.distance_from # [Function]
console.log p1.distance_from p2 # 13

```



## Common Lisp



```lisp
CL-USER> (defstruct point (x 0) (y 0))  ;If not provided, x or y default to 0
POINT
```

In addition to defining the ''point'' data type, the defstruct macro also created constructor and accessor functions:

```lisp
CL-USER> (setf a (make-point))          ;The default constructor using the default values for x and y
#S(POINT :X 0 :Y 0)
CL-USER> (setf b (make-point :x 5.5 :y #C(0 1)))  ;Dynamic datatypes are the default
#S(POINT :X 5.5 :Y #C(0 1))                       ;y has been set to the imaginary number i (using the Common Lisp complex number data type)
CL-USER> (point-x b)                    ;The default name for the accessor functions is structname-slotname
5.5
CL-USER> (point-y b)
#C(0 1)
CL-USER> (setf (point-y b) 3)           ;The accessor is setfable
3
CL-USER> (point-y b)
3
```



## D


```d
void main() {
    // A normal POD struct
    // (if it's nested and it's not static then it has a hidden
    // field that points to the enclosing function):
    static struct Point {
        int x, y;
    }

    auto p1 = Point(10, 20);

    // It can also be parametrized on the coordinate type:
    static struct Pair(T) {
        T x, y;
    }

    // A pair with integer coordinates:
    auto p2 = Pair!int(3, 5);

    // A pair with floating point coordinates:
    auto p3 = Pair!double(3, 5);

    // Classes (static inner):
    static class PointClass {
        int x, y;
        this(int x_, int y_) {
            this.x = x_;
            this.y = y_;
        }
    }

    auto p4 = new PointClass(1, 2);

    // There are also library-defined tuples:
    import std.typecons;

    alias Tuple!(int,"x", int,"y") PointXY;

    auto p5 = PointXY(3, 5);

    // And even built-in "type tuples":
    import std.typetuple;

    alias TypeTuple!(int, 5) p6;

    static assert(is(p6[0] == int));
    static assert(p6[1] == 5);
}
```



## Delphi

As defined in Types.pas:


```Delphi
  TPoint = record
    X: Longint;
    Y: Longint;
  end;

```



## E



```e
def makePoint(x, y) {
    def point {
        to getX() { return x }
        to getY() { return y }
    }
    return point
}
```



## EchoLisp


```scheme

(lib 'struct)
    (struct Point (x y))
    (Point 3 4)
  → #<Point> (3 4)

;; run-time type checking is possible
(lib 'types)
    (struct Point (x y))
    (struct-type Point Number Number)
    (Point 3 4)
    (Point 3 'albert)
❌ error: #number? : type-check failure : albert → 'Point:y'

```



## Ela


Ela supports algebraic types:


```ela
type Maybe = None | Some a
```


Except of regular algebraic types, Ela also provides a support for open algebraic types - which can be extended any time with new constructors:


```ela
opentype Several = One | Two | Three

//Add new constructor to an existing type
data Several = Four
```



## Elena


```elena
struct Point
{
    prop int X;

    prop int Y;

    constructor new(int x, int y)
    {
        X := x;
        Y := y
    }
}
```



## Elixir


```elixir
iex(1)> defmodule Point do
...(1)>   defstruct x: 0, y: 0
...(1)> end
{:module, Point, <<70, 79, 82, ...>>, %Point{x: 0, y: 0}}
iex(2)> origin = %Point{}
%Point{x: 0, y: 0}
iex(3)> pa = %Point{x: 10, y: 20}
%Point{x: 10, y: 20}
iex(4)> pa.x
10
iex(5)> %Point{pa | y: 30}
%Point{x: 10, y: 30}
iex(6)> %Point{x: px, y: py} = pa        # pattern matching
%Point{x: 10, y: 20}
iex(7)> px
10
iex(8)> py
20
```



## Elm


```elm

--Compound Data type can hold multiple independent values
--In Elm data can be compounded using List, Tuple, Record
--In a List
point = [2,5]
--This creates a list having x and y which are independent and can be accessed by List functions
--Note that x and y must be of same data type

--Tuple is another useful data type that stores different independent values
point = (3,4)
--Here we can have multiple data types
point1 = ("x","y")
point2 = (3,4.5)
--The order of addressing matters
--Using a Record is the best option
point = {x=3,y=4}
--To access
point.x
point.y
--Or Use it as a function
.x point
.y point
--Also to alter the value
{point | x=7}
{point | y=2}
{point | x=3,y=4}
--Each time a new record is generated
--END

```


## Erlang


```erlang

-module(records_test).
-compile(export_all).

-record(point,{x,y}).

test() ->
    P1 = #point{x=1.0,y=2.0}, % creates a new point record
    io:fwrite("X: ~f, Y: ~f~n",[P1#point.x,P1#point.y]),
    P2 = P1#point{x=3.0}, % creates a new point record with x set to 3.0, y is copied from P1
    io:fwrite("X: ~f, Y: ~f~n",[P2#point.x,P2#point.y]).

```



## Euphoria

```euphoria

enum x, y

sequence point = {0,0}

printf(1,"x = %d, y = %3.3f\n",point)

point[x] = 'A'
point[y] = 53.42

printf(1,"x = %d, y = %3.3f\n",point)
printf(1,"x = %s, y = %3.3f\n",point)

```

```txt

x = 0, y = 0.000
x = 65, y = 53.420
x = A, y = 53.420

```


=={{header|F_Sharp|F#}}==
See the OCaml section as well. Here we create a list of points and print them out.

```fsharp
type Point = { x : int; y : int }

let points = [
    {x = 1; y = 1};
    {x = 5; y = 5} ]

Seq.iter (fun p -> printfn "%d,%d" p.x p.y) points
```



## Factor


```factor>TUPLE: point x y ;</lang



## Fantom



```fantom

// define a class to contain the two fields
// accessors to get/set the field values are automatically generated
class Point
{
  Int x
  Int y
}

class Main
{
  public static Void main ()
  {
    // empty constructor, so x,y set to 0
    point1 := Point()
    // constructor uses with-block, to initialise values
    point2 := Point { x = 1; y = 2}
    echo ("Point 1 = (" + point1.x + ", " + point1.y + ")")
    echo ("Point 2 = (" + point2.x + ", " + point2.y + ")")
  }
}

```


```txt

Point 1 = (0, 0)
Point 2 = (1, 2)

```



## Forth

There is no standard structure syntax in Forth, but it is easy to define words for creating and accessing data structures.


```forth>: pt
x ( point -- x ) ;
: pt>y ( point -- y ) CELL+ ;
: .pt ( point -- ) dup pt>x @ . pt>y @ . ;    \ or for this simple structure, 2@ . .

create point 6 , 0 ,
7 point pt>y !
.pt    \ 6 7
```


Some Forths have mechanisms for declaring complex structures. For example, GNU Forth uses this syntax:


```forth
struct
  cell% field pt>x
  cell% field pt>y
end-struct point%
```



## Fortran

In ISO Fortran 90 or later, use a TYPE declaration, "constructor" syntax, and field delimiter syntax:

```fortran
program typedemo
    type rational                                           ! Type declaration
        integer :: numerator
        integer :: denominator
    end type rational

    type( rational ), parameter :: zero = rational( 0, 1 )  ! Variables initialized
    type( rational ), parameter :: one  = rational( 1, 1 )  ! by constructor syntax
    type( rational ), parameter :: half = rational( 1, 2 )
    integer :: n, halfd, halfn
    type( rational ) :: &
        one_over_n(20) = (/ (rational( 1, n ), n = 1, 20) /) ! Array initialized with
                                                             ! constructor inside
                                                             ! implied-do array initializer
    integer :: oon_denoms(20)

    halfd = half%denominator                       ! field access with "%" delimiter
    halfn = half%numerator

    oon_denoms = one_over_n%denominator            ! Access denominator field in every
                                                   ! rational array element & store
end program typedemo                               ! as integer array
```



## FreeBASIC


```freebasic
' FB 1.05.0 Win64

Type Point
  As Integer x, y
End Type

Dim p As Point = (1, 2)
Dim p2 As Point = (3, 4)
Print p.x, p.y
Print p2.x, p2.y
Sleep
```


```txt

 1             2
 3             4

```



## Go


```go
package main

import "fmt"

type point struct {
    x, y float64
}

func main() {
    fmt.Println(point{3, 4})
}
```



## Groovy



### Declaration


```groovy
class Point {
    int x
    int y

    // Default values make this a 0-, 1-, and 2-argument constructor
    Point(int x = 0, int y = 0) { this.x = x; this.y = y }
    String toString() { "{x:${x}, y:${y}}" }
}
```



### Instantiation


### ==Direct==


```groovy
// Default Construction with explicit property setting:
def p0 = new Point()
assert 0 == p0.x
assert 0 == p0.y
p0.x = 36
p0.y = -2
assert 36 == p0.x
assert -2 == p0.y

// Direct Construction:
def p1 = new Point(36, -2)
assert 36 == p1.x
assert -2 == p1.y

def p2 = new Point(36)
assert 36 == p2.x
assert 0 == p2.y
```


=====List-to-argument Substitution=====
There are several ways that a List can be substituted for constructor arguments via "type coercion" (casting).

```groovy
// Explicit coersion from list with "as" keyword
def p4 = [36, -2] as Point
assert 36 == p4.x
assert -2 == p4.y

// Explicit coersion from list with Java/C-style casting
p4 = (Point) [36, -2]
println p4
assert 36 == p4.x
assert -2 == p4.y

// Implicit coercion from list (by type of variable)
Point p6 = [36, -2]
assert 36 == p6.x
assert -2 == p6.y

Point p8 = [36]
assert 36 == p8.x
assert 0 == p8.y
```


=====Map-to-property Substitution=====
There are several ways to construct an object using a map (or a comma-separated list of map entries) that substitutes entries for class properties. The process is properly (A) instantiation, followed by (B) property mapping. Because the instantiation is not tied to the mapping, it requires the existence of a no-argument constructor.

```groovy
// Direct map-based construction
def p3 = new Point([x: 36, y: -2])
assert 36 == p3.x
assert -2 == p3.y

// Direct map-entry-based construction
p3 = new Point(x: 36, y: -2)
assert 36 == p3.x
assert -2 == p3.y

p3 = new Point(x: 36)
assert 36 == p3.x
assert 0 == p3.y

p3 = new Point(y: -2)
assert 0 == p3.x
assert -2 == p3.y

// Explicit coercion from map with "as" keyword
def p5 = [x: 36, y: -2] as Point
assert 36 == p5.x
assert -2 == p5.y

// Implicit coercion from map (by type of variable)
Point p7 = [x: 36, y: -2]
assert 36 == p7.x
assert -2 == p7.y

Point p9 = [y:-2]
assert 0 == p9.x
assert -2 == p9.y
```



## Haskell



### Algebraic Data Type

See [[wp:Algebraic_data_type|algebraic data type]]. The different options ("Empty", "Leaf", "Node") are called ''constructors'', and is associated with 0 or more arguments with the declared types.

  data Tree = Empty
            | Leaf Int
            | Node Tree Tree
  deriving (Eq, Show)

  t1 = Node (Leaf 1) (Node (Leaf 2) (Leaf 3))


### Tagged Type

This is a special case of the algebraic data type above with only one constructor.
  data Point = Point Integer Integer
  instance Show Point where
      show (Point x y) = "("++(show x)++","++(show y)++")"
  p = Point 6 7


### Record Type

Entries in an algebraic data type constructor can be given field names.
 data Point = Point { x :: Integer, y :: Integer }
 deriving (Eq, Show)

The ''deriving'' clause here provides default instances for equality and conversion to string.

Different equivalent ways of constructing a point:

 p  = Point 2 3
 p' = Point { x=4, y=5 }

The field name is also a function that extracts the field value out of the record
 x p' -- evaluates to 4


### Tuple Type


You can make a tuple literal by using a comma-delimited list surrounded by parentheses, without needing to declare the type first:


```haskell
p = (2,3)
```


The type of <code>p</code> is <code>(Int, Int)</code>, using the same comma-delimited list syntax as the literal.


### Discriminated Type

Just an algebraic data type with multiple constructors being records
 data Person =
     Male   { name :: String, age :: Integer, weight :: Double,
              beard_length :: Double }
   | Female { name :: String, age :: Integer, weight :: Double }
   deriving (Eq, Show)

Note that the field names may be identical in alternatives.

=={{header|Icon}} and {{header|Unicon}}==

```icon
record Point(x,y)
```



## IDL



```idl
point = {x: 6 , y: 0 }
point.y = 7
print, point
;=> {       6       7}
```




## J


In a "real" J application, points would be represented by arrays of 2 (or N) numbers.  None the less, sometimes objects (in the OO sense) are a better representation than arrays, so J supports them:


```j
   NB.  Create a "Point" class
   coclass'Point'

   NB. Define its constuctor
   create =: 3 : 0
     'X Y' =: y
   )

   NB.  Instantiate an instance (i.e. an object)
   cocurrent 'base'
   P =: 10 20 conew 'Point'

   NB.  Interrogate its members
   X__P
10
   Y__P
20
```



## Java

We use a class:

```java
public class Point
{
  public int x, y;
  public Point() { this(0); }
  public Point(int x0) { this(x0,0); }
  public Point(int x0, int y0) { x = x0; y = y0; }

  public static void main(String args[])
  {
    Point point = new Point(1,2);
    System.out.println("x = " + point.x );
    System.out.println("y = " + point.y );
  }
}
```



## JavaScript



```javascript
//using object literal syntax
var point = {x : 1, y : 2};

//using constructor
var Point = function (x, y) {
  this.x = x;
  this.y = y;
};
point = new Point(1, 2);

//using ES6 class syntax
class Point {
  constructor(x, y) {
    this.x = x;
    this.y = y;
  }
}
point = new Point(1, 2);
```



## jq


```jq
{"x":1, "y":2}
```


If the emphasis in the task description is on "type", then an alternative approach would be to include a "type" key, e.g.

```jq
{"x":1, "y":2, type: "Point"}
```


Using this approach, one can distinguish between objects of type "Point" and those that happen to have keys named "x" and "y".


## JSON



```json
{"x":1,"y":2}
```



## Julia

'''Define the type''':

```julia
struct Point{T<:Real}
    x::T
    y::T
end
```

The components of <code>Point</code> can be any sort of real number, though they do have to be of the same type.

'''Define a few simple operations for Point''':

```julia
Base.:(==)(u::Point, v::Point) = u.x == v.x && u.y == v.y
Base.:-(u::Point) = Point(-u.x, -u.y)
Base.:+(u::Point, v::Point) = Point(u.x + v.x, u.y + v.y)
Base.:-(u::Point, v::Point) = u + (-v)
```


'''Have fun''':

```julia
a, b, c = Point(1, 2), Point(3, 7), Point(2, 4)
@show a b c
@show a + b
@show -a + b
@show a - b
@show a + b + c
@show a == b
@show a + a == c
```


```txt
a = Point{Int64}(1, 2)
b = Point{Int64}(3, 7)
c = Point{Int64}(2, 4)
a + b = Point{Int64}(4, 9)
-a + b = Point{Int64}(2, 5)
a - b = Point{Int64}(-2, -5)
a + b + c = Point{Int64}(6, 13)
a == b = false
a + a == c = true
```



## KonsolScript


```KonsolScript
Var:Create(
  Point,
    Number x,
    Number y
)
```


Instanciate it with...

```KonsolScript
function main() {
  Var:Point point;
}
```



## Kotlin


```scala
data class Point(var x: Int, var y: Int)

fun main(args: Array<String>) {
    val p = Point(1, 2)
    println(p)
    p.x = 3
    p.y = 4
    println(p)
}
```


```txt

Point(x=1, y=2)
Point(x=3, y=4)

```



## Lasso

In Lasso, a point could just be stored in the pair type. However, assuming we want to be able to access the points using the member methods [Point->x] and [Point->y], let's just create a type that inherits from the pair type:

```lasso>define Point =
 type {
    parent pair

    public onCreate(x,y) => {
        ..onCreate(#x=#y)
    }

    public x => .first
    public y => .second
}

local(point) = Point(33, 42)
#point->x
#point->y
```


```txt
33
43
```



## LFE


Simply define a record in the LFE REPL (can also be used in include files, modules, etc.):


```lisp

(defrecord point
  x
  y)

```


Creating points:


```txt

> (make-point x 0 y 0)
#(point 0 0)
> (set p (make-point x 1.1 y -4.2))
#(point 1.1 -4.2)

```


Accessing:


```txt

> (point-x p)
1.1
> (point-y p)
-4.2

```


Updates (note that since LFE has no mutable data, persisted updates would need to rebind the new value to the old variable name):


```txt

> (set-point-x p 3.1)
#(point 3.1 -4.2)
> (set-point-y p 4.2)
#(point 1.1 4.2)

```


Metadata, etc.:


```txt

> (fields-point)
(x y)
> (is-point #(x y))
false
> (is-point p)
true

```



## Lingo

Point and Vector types are built-in. A custom "MyPoint" type can be implemented like this:

```lingo
-- parent script "MyPoint"
property x
property y
on new (me, px, py)
  me.x = px
  me.y = py
  return me
end
```


```lingo
p = script("MyPoint").new(23, 42)
put p.x, p.y
-- 23 42
```

Construction could also be simplified by using a global wrapper function:

```lingo
-- in some movie script
on MyPoint (x, y)
  return script("MyPoint").new(x, y)
end
```


```lingo
p = MyPoint(23, 42)
put p.x, p.y
-- 23 42
```



## Logo

In Logo, a point is represented by a list of two numbers. For example, this will draw a triangle:

```logo
setpos [100 100] setpos [100 0] setpos [0 0]
show pos  ; [0 0]
```

Access is via normal list operations like FIRST and BUTFIRST (BF). X is FIRST point, Y is LAST point. For example, a simple drawing program which exits if mouse X is negative:

```logo
until [(first mousepos) < 0] [ifelse button? [pendown] [penup]  setpos mousepos]
```



## Lua



### = Simple Table =


Lua could use a simple table to store a compound data type Point(x, y):


```lua

a = {x = 1; y = 2}
b = {x = 3; y = 4}
c = {
    x = a.x + b.x;
    y = a.y + b.y
}
print(a.x, a.y)  --> 1 2
print(c.x, c.y)  --> 4 6

```



### = Prototype Object =


Furthermore, Lua could create a prototype object (OOP class emulation) to represent a compound data type Point(x, y) as the following:

```lua

cPoint = {}                                           -- metatable (behaviour table)
function newPoint(x, y)                               -- constructor
    local pointPrototype = {}                         -- prototype declaration
    function pointPrototype:getX() return x end       -- public method
    function pointPrototype:getY() return y end       -- public method
    function pointPrototype:getXY() return x, y end   -- public method
    function pointPrototype:type() return "point" end -- public method
    return setmetatable(pointPrototype, cPoint)       -- set behaviour and return the pointPrototype
end--newPoint

```


In the above example, the methods are declared inside the constructor so that they could access the closured values <code>x</code> and <code>y</code> (see usage example). The <code>pointPrototype:type</code> method could be used to extend the original <code>type</code> function available in Lua:


```lua

local oldtype = type;                   -- store original type function
function type(v)
    local vType = oldtype(v)
    if (vType=="table" and v.type) then
        return v:type()                 -- bypass original type function if possible
    else
        return vType
    end--if vType=="table"
end--type

```


The usage of metatable <code>cPoint</code> which stores the behavior of the <code>pointPrototype</code> enables additional behaviour to be added to the data type, such as:


```lua

function cPoint.__add(op1, op2)  -- add the x and y components
    if type(op1)=="point" and type(op2)=="point" then
        return newPoint(
            op1:getX()+op2:getX(),
            op1:getY()+op2:getY())
    end--if type(op1)
end--cPoint.__add
function cPoint.__sub(op1, op2)  -- subtract the x and y components
    if (type(op1)=="point" and type(op2)=="point") then
        return newPoint(
            op1:getX()-op2:getX(),
            op1:getY()-op2:getY())
    end--if type(op1)
end--cPoint.__sub

```


Usage example:


```lua

a = newPoint(1, 2)
b = newPoint(3, 4)
c = a + b             -- using __add behaviour
print(a:getXY())      --> 1 2
print(type(a))        --> point
print(c:getXY())      --> 4 6
print((a-b):getXY())  --> -2 -2  -- using __sub behaviour

```


=={{header|Mathematica}} / {{header|Wolfram Language}}==
Expressions like point[x, y] can be used without defining.

```mathematica
In[1]:= a = point[2, 3]

Out[1]= point[2, 3]

In[2]:= a[[2]]

Out[2]= 3

In[3]:= a[[2]] = 4; a

Out[3]= point[2, 4]
```


Or you can just define a function.

```mathematica
p[x] = 2; p[y] = 3;
```

Data will be stored as down values of the symbol ''p''.

=={{header|MATLAB}} / {{header|Octave}}==


```MATLAB
 point.x=3;
 point.y=4;
```

Alternatively, coordinates can be also stored as vectors

```MATLAB
 point = [3,4];
```




## Maxima


```maxima
defstruct(point(x, y))$

p: new(point)$

q: point(1, 2)$

p@x: 5$
```



## MAXScript

Point is a built-in object type in MAX, so...

```maxscript
struct myPoint (x, y)
newPoint = myPoint x:3 y:4
```

In practice however, you'd use MAX's built in Point2 type

```maxscript>newPoint = Point2 3 4</lang


=={{header|Modula-2}}==

```modula2
TYPE Point = RECORD
  x, y     : INTEGER
END;
```


Usage:

```modula2
VAR    point   : Point;
...
point.x := 12;
point.y := 7;
```


=={{header|Modula-3}}==

```modula3
TYPE Point = RECORD
  x, y: INTEGER;
END;
```


Usage:

```txt

VAR point: Point;
...
point := Point{3, 4};

```


or


```txt

point := Point{x := 3, y := 4};

```



## NetRexx

Like Java, NetRexx uses the <tt>class</tt> instruction to create compound types.  Unlike Java; NetRexx provides keywords to automatically generate getters and setters for <tt>class</tt> properties and will automatically generate intermediate methods based on defaults provided in method prototypes.

```NetRexx
/* NetRexx */
options replace format comments java crossref symbols nobinary

class RCompoundDataType
  method main(args = String[]) public static
    pp = Point(2, 4)
    say pp
    return

class RCompoundDataType.Point -- inner class "Point"
  properties indirect -- have NetRexx create getters & setters
    x = Integer
    y = Integer

  method Point(x_ = 0, y_ = 0) public -- providing default values for x_ & y_ lets NetRexx generate intermediate constructors Point() & Point(x_)
    this.x = Integer(x_)
    this.y = Integer(y_)
    return

  method toString() public returns String
    res = 'X='getX()',Y='getY()
    return res

```

```txt

X=2,Y=4

```



## Nim


```nim
type Point = tuple[x, y: int]

var p: Point = (12, 13)
var p2: Point = (x: 100, y: 200)
```


=={{header|Oberon-2}}==

```oberon2

MODULE Point;
TYPE
	Object* = POINTER TO ObjectDesc;
	ObjectDesc* = RECORD
		x-,y-: INTEGER;
	END;

	PROCEDURE (p: Object) Init(x,y: INTEGER);
	BEGIN
		p.x := x; p.y := y
	END Init;

	PROCEDURE New*(x,y: INTEGER): Object;
	VAR
		p: Object;
	BEGIN
		NEW(p);p.Init(x,y);RETURN p;
	END New;

END Point.

```



## Objeck

Classes are used for compound data types.

```objeck

class Point {
  @x : Int;
  @y : Int;

  New() {
    @x := 0;
    @y := 0;
  }

  New(x : Int, y : Int) {
    @x := x;
    @y := y;
  }

  New(p : Point) {
    @x := p->GetX();
    @y := p->GetY();
  }

  method : public : GetX() ~ Int {
    return @x;
  }

  method : public : GetY() ~ Int {
    return @y;
  }

  method : public : SetX(x : Int) ~ Nil {
    @x := x;
  }

  method : public : SetY(y : Int) ~ Nil {
    @y := y;
  }
}

```



## OCaml



### Algebraic Data Type

See [[wp:Algebraic_data_type|algebraic data type]]. The different options ("Empty", "Leaf", "Node") are called ''constructors'', and is associated with 0 or more arguments with the declared types; multiple arguments are declared with a syntax that looks like a tuple type, but it is not really a tuple.


```ocaml
type tree = Empty
          | Leaf of int
          | Node of tree * tree

let t1 = Node (Leaf 1, Node (Leaf 2, Leaf 3))
```



### Record Type



```ocaml
type point = { x : int; y : int }
```


How to construct a point:


```ocaml
let p = { x = 4; y = 5 }
```


You can use the dot (".") to access fields.

```ocaml
p.x (* evaluates to 4 *)
```


Fields can be optionally declared to be mutable:

```ocaml
type mutable_point = { mutable x2 : int; mutable y2 : int }
```


Then they can be assigned using the assignment operator "<-"

```ocaml
let p2 = { x2 = 4; y2 = 5 } in
  p2.x2 <- 6;
  p2 (* evaluates to { x2 = 6; y2 = 5 } *)
```



### Tuple Type


You can make a tuple literal by using a comma-delimited list, optionally surrounded by parentheses, without needing to declare the type first:


```ocaml
let p = (2,3)
```


The type of <code>p</code> is a product (indicated by <code>*</code>) of the types of the components:
 # let p = (2,3);;
 val p : int * int = (2, 3)


## Oforth


Using a class :


```oforth
Object Class new: Point(x, y)
```



## ooRexx

ooRexx uses class for compound data types.

```ooRexx

p = .point~new(3,4)
say "x =" p~x
say "y =" p~y

::class point
::method init
  expose x y
  use strict arg x = 0, y = 0   -- defaults to 0 for any non-specified coordinates

::attribute x
::attribute y

```



## OpenEdge/Progress


The temp-table is a in memory database table. So you can query sort and iterate it, but is the data structure that comes closest.

<lang Progress (Openedge ABL)>def temp-table point
  field x as int
  field y as int
  .
```


Another option would be a simple class.


## OxygenBasic


```oxygenbasic

'SHORT FORM
type point float x,y

'FULL FORM
type point
  float x
  float y
end type

```



## Oz

A point can be represented by using a record value:

```oz
P = point(x:1 y:2)
```


Now we can access the components by name: P.x and P.y
Often such values are deconstructed by pattern matching:

```oz
case P of point(x:X y:Y) then
   {Show X}
   {Show Y}
end
```



## PARI/GP


```parigp
point.x=1;
point.y=2;
```



## Pascal


```pascal
type point = record
              x, y: integer;
             end;
```



## Perl



### Array


```perl
my @point = (3, 8);
```



### Hash


```perl
my %point = (
   x => 3,
   y => 8
);
```



### Class instance


```perl
package Point;

use strict;
use base 'Class::Struct'
    x => '$',
    y => '$',
;

my $point = Point->new(x => 3, y => 8);
```



## Perl 6

### Array


```perl6
my @point = 3, 8;

my Int @point = 3, 8; # or constrain to integer elements
```



### Hash


```perl6
my %point = x => 3, y => 8;

my Int %point = x => 3, y => 8; # or constrain the hash to have integer values
```



### Class instance


```perl6
class Point { has $.x is rw; has $.y is rw; }
my Point $point .= new(x => 3, y => 8);
```


===[http://design.perl6.org/S32/Containers.html#Set Set]===

```perl6
my $s1 = set <a b c d>; # order is not preserved
my $s2 = set <c d e f>;
say $s1 (&) $s2; # OUTPUT«set(c, e)»
say $s1 ∩ $s2; # we also do Unicode
```



## Phix

The sequence is a natural compound data type. The following would be the same without the type point and declaring p as a sequence, apart from the run-time error. There would be no difficulty defining point to have a string and two atoms.

```Phix
enum x,y
type point(object p)
    return sequence(p) and length(p)=y and atom(p[x]) and atom(p[y])
end type

point p = {175,3.375}

p[x] -= p[y]*20

puts(1,"point p is ")
?p
printf(1,"p[x]:%g, p[y]:%g\n",{p[x],p[y]})

p[x] = 0            -- fine
p[y] = "string"     -- run-time error
```

```txt

point p is {107.5,3.375}
p[x]:107.5, p[y]:3.375
C:\Program Files (x86)\Phix\test.exw:15
type check failure, p is {0,"string"}

```



## PHP



```php
# Using pack/unpack
$point = pack("ii", 1, 2);

$u = unpack("ix/iy", $point);
echo $x;
echo $y;

list($x,$y) = unpack("ii", $point);
echo $x;
echo $y;
```



```php
# Using array
$point = array('x' => 1, 'y' => 2);

list($x, $y) = $point;
echo $x, ' ', $y, "\n";

# or simply:
echo $point['x'], ' ', $point['y'], "\n";
```



```php
# Using class
class Point {
  function __construct($x, $y) { $this->x = $x; $this->y = $y; }
  function __tostring() { return $this->x . ' ' . $this->y . "\n"; }
}
$point = new Point(1, 2);
echo $point; # will call __tostring() in later releases of PHP 5.2; before that, it won't work so good.
```



## PicoLisp


```PicoLisp
(class +Point)

(dm T (X Y)
   (=: x X)
   (=: y Y) )

(setq P (new '(+Point) 3 4))

(show P)
```

```txt
$52717735311266 (+Point)
   y 4
   x 3
```



## PL/I


```PL/I

define structure
  1 point,
     2 x float,
     2 y float;



```



## Pop11



```pop11
uses objectclass;
define :class Point;
   slot x = 0;
   slot y = 0;
enddefine;
```



## PowerShell

```PowerShell

class Point {
  [Int]$a
  [Int]$b
  Point() {
      $this.a = 0
      $this.b = 0
  }
  Point([Int]$a, [Int]$b) {
      $this.a = $a
      $this.b = $b
  }
  [Int]add() {return $this.a + $this.b}
  [Int]mul() {return $this.a * $this.b}
}
$p1  = [Point]::new()
$p2 = [Point]::new(3,2)
$p1.add()
$p2.mul()

```

<b>Output:</b>

```txt

0
6

```



## Prolog

Prolog terms ARE compound data types, there is no need to specifically define a type.
for the purpose of this exercise you could define a rule like so:

```prolog
point(10, 20).
```

This will create static point that can be called:

```prolog
?- point(X,Y).
X = 10,
Y = 20.
```

terms can be passed around as values and can have a complex nested structure of any size, eg:

```prolog
person_location(person(name(N), age(A)), point(X, Y)).
```



## PureBasic


A basic [http://www.purebasic.com/documentation/reference/structures.html structure] is implemented as;

```PureBasic
Structure MyPoint
  x.i
  y.i
EndStructure
```



## Python


The simplest way it to use a tuple, or a list if it should be mutable:

```python
X, Y = 0, 1
p = (3, 4)
p = [3, 4]

print p[X]
```


If needed, you can use class:


```python
class Point:
    def __init__(self, x=0, y=0):
        self.x = x
        self.y = y

p = Point()
print p.x
```


One could also simply instantiate a generic object and "monkeypatch" it:


```python
class MyObject(object): pass
point = MyObject()
point.x, point.y = 0, 1
# objects directly instantiated from "object()"  cannot be "monkey patched"
# however this can generally be done to it's subclasses
```



###  Dictionary

Mutable. Can add keys (attributes)

```python
pseudo_object = {'x': 1, 'y': 2}
```




###  Named Tuples


As of Python 2.6 one can use the ''collections.namedtuple'' factory to create classes which associate field names with elements of a tuple.  This allows one to perform all normal operations on the contained tuples (access by indices or slices, packing and unpacking) while also allowing elements to be accessed by name.


```python>>>
 from collections import namedtuple
>>> help(namedtuple)
Help on function namedtuple in module collections:

namedtuple(typename, field_names, verbose=False)
    Returns a new subclass of tuple with named fields.

    >>> Point = namedtuple('Point', 'x y')
    >>> Point.__doc__                   # docstring for the new class
    'Point(x, y)'
    >>> p = Point(11, y=22)             # instantiate with positional args or keywords
    >>> p[0] + p[1]                     # indexable like a plain tuple
    33
    >>> x, y = p                        # unpack like a regular tuple
    >>> x, y
    (11, 22)
    >>> p.x + p.y                       # fields also accessable by name
    33
    >>> d = p._asdict()                 # convert to a dictionary
    >>> d['x']
    11
    >>> Point(**d)                      # convert from a dictionary
    Point(x=11, y=22)
    >>> p._replace(x=100)               # _replace() is like str.replace() but targets named fields
    Point(x=100, y=22)

>>>
```



## R

R uses the list data type for compound data.

```R
mypoint <- list(x=3.4, y=6.7)
# $x
# [1] 3.4
# $y
# [1] 6.7
mypoint$x    # 3.4

list(a=1:10, b="abc", c=runif(10), d=list(e=1L, f=TRUE))
# $a
# [1]  1  2  3  4  5  6  7  8  9 10
# $b
# [1] "abc"
# $c
#  [1] 0.64862897 0.73669435 0.11138945 0.10408015 0.46843836 0.32351247
#  [7] 0.20528914 0.78512472 0.06139691 0.76937113
# $d
# $d$e
# [1] 1
# $d$f
# [1] TRUE
```



## Racket


The most common method uses structures (similar to records):


```racket

#lang racket
(struct point (x y))

```


Alternatively, you can define a class:


```racket

#lang racket
(define point% ; classes are suffixed with % by convention
  (class object%
    (super-new)
    (init-field x y)))

```



## REXX


```rexx
x= -4.9
y=  1.7

point=x y
```

:: ---or---

```rexx
x= -4.1
y=  1/4e21

point=x y

bpoint=point

gpoint=5.6  7.3e-12
```



## Ring


```ring

see new point {x=10 y=20} class point x y

```

Output

```ring

x: 10.000000
y: 20.000000

```



## Ruby


```ruby
Point = Struct.new(:x,:y)
pt = Point.new(6,7)
puts pt.x        #=> 6
pt.y = 3
puts pt          #=> #<struct Point x=6, y=3>

# The other way of accessing
pt = Point[2,3]
puts pt[:x]      #=> 2
pt['y'] = 5
puts pt          #=> #<struct Point x=2, y=5>

pt.each_pair{|member, value| puts "#{member} : #{value}"}
                 #=> x : 2
                 #=> y : 5
```



## Rust


### Structs

There are three kinds of <code>struct</code>s in Rust, two of which would be suitable to represent a point.

====C-like struct====

```rust
 // Defines a generic struct where x and y can be of any type T
struct Point<T> {
    x: T,
    y: T,
}
fn main() {
    let p = Point { x: 1.0, y: 2.5 }; // p is of type Point<f64>
    println!("{}, {}", p.x, p.y);
}
```



### =Tuple struct=

These are basically just named tuples.

```rust>struct Point<T
(T, T);
fn main() {
    let p = Point(1.0, 2.5);
    println!("{},{}", p.0, p.1);
}
```


### Tuples


```rust
 fn main() {
    let p = (0.0, 2.4);
    println!("{},{}", p.0, p.1);
}
```



## Scala


```scala
case class Point(x: Int = 0, y: Int = 0)

val p = Point(1, 2)
println(p.y)   //=> 2
```



## Scheme

Using [http://srfi.schemers.org/srfi-9/srfi-9.html SRFI 9]:

```scheme
(define-record-type point
    (make-point x y)
    point?
    (x point-x)
    (y point-y))
```



## Seed7


```seed7
const type: Point is new struct
    var integer: x is 0;
    var integer: y is 0;
  end struct;
```



## Shen


```shen
(datatype point
  X : number; Y : number;

### ==============

  [point X Y] : point;)
```

Pairs (distinct from cons cells) are also supported, in which case a point would be denoted by (number * number):

```shen
(2+) (@p 1 2)
(@p 1 2) : (number * number)
```



## Sidef


```ruby
struct Point {x, y};
var point = Point(1, 2);
say point.y;                #=> 2
```



## SIMPOL

The <code>point</code> type is pre-defined in [SIMPOL], so we will call this mypoint.


```simpol
type mypoint
  embed
  integer x
  integer y
end type
```


The <code>embed</code> keyword is used here as a toggle to indicate that all following properties are embedded in the type. The other toggle is <code>reference</code>, which only places a reference to an object in the type, but the reference assigned before the property can be used. These keywords can also be placed on the same line, but then they only apply to that line of the type definition.

A type in [SIMPOL] can be just a container of values and other structures, but it can also include methods. These are implemented outside the type definition, but must be part of the same compiled unit.


```simpol
type mypoint
  embed
  integer x
  integer y
end type

function mypoint.new(mypoint me, integer x, integer y)
  me.x = x
  me.y = y
end function me
```



## SNOBOL4



```snobol
	data('point(x,y)')
	p1 = point(10,20)
	p2 = point(10,40)
	output = "Point 1 (" x(p1) "," y(p1) ")"
	output = "Point 2 (" x(p2) "," y(p2) ")"
end
```



## Standard ML



### Algebraic Data Type

See [[wp:Algebraic_data_type|algebraic data type]]. The different options ("Empty", "Leaf", "Node") are called ''constructors'', and is associated with 0 or 1 arguments with the declared types; multiple arguments are handled with tuples.


```sml
datatype tree = Empty
              | Leaf of int
              | Node of tree * tree

val t1 = Node (Leaf 1, Node (Leaf 2, Leaf 3))
```



### Tuple Type


You can make a tuple literal by using a comma-delimited list surrounded by parentheses, without needing to declare the type first:


```sml
val p = (2,3)
```


The type of <code>p</code> is a product (indicated by <code>*</code>) of the types of the components:
 - val p = (2,3);
 val p = (2,3) : int * int

You can extract elements of the tuple using the <code>#N</code> syntax:
 - #2 p;
 val it = 3 : int
The <code>#2</code> above extracts the second field of its argument.


### Record Type


Records are like tuples but with field names.

You can make a record literal by using a comma-delimited list of <code>key = value</code> pairs surrounded by curly braces, without needing to declare the type first:


```sml
val p = { x = 4, y = 5 }
```


The type of <code>p</code> is a comma-delimited list of <code>key:type</code> pairs of the types of the fields:
 - val p = { x = 4, y = 5 };
 val p = {x=4,y=5} : {x:int, y:int}

You can extract elements of the tuple using the <code>#name</code> syntax:
 - #y p;
 val it = 5 : int
The <code>#y</code> above extracts the field named "y" of its argument.


## Stata

See '''[https://www.stata.com/help.cgi?m2_struct struct]''' in Stata help.


```stata
mata
struct Point {
	real scalar x, y
}

// dumb example
function test() {
	struct Point scalar a
	a.x = 10
	a.y = 20
	printf("%f\n",a.x+a.y)
}

test()
30
end
```


## Swift


```Swift
// Structure
struct Point {
    var x:Int
    var y:Int
}

// Tuple
typealias PointTuple = (Int, Int)

// Class
class PointClass {
    var x:Int!
    var y:Int!

    init(x:Int, y:Int) {
        self.x = x
        self.y = y
    }
}
```



## Tcl

This can be done using an associative array:

```tcl
array set point {x 4 y 5}
set point(y) 7
puts "Point is {$point(x),$point(y)}"
# => Point is {4,7}
```

Or a dictionary:
```tcl
set point [dict create x 4 y 5]
dict set point y 7
puts "Point is {[dict get $point x],[dict get $point y]}"
```

Or an object:
```tcl
oo::class create Point {
    variable x y
    constructor {X Y} {set x $X;set y $Y}
    method x {args} {set x {*}$args}
    method y {args} {set y {*}$args}
    method show {} {return "{$x,$y}"}
}
Point create point 4 5
point y 7
puts "Point is [point show]"
```


=={{header|TI-89 BASIC}}==

TI-89 BASIC does not have user-defined data structures. The specific example of a point is best handled by using the built-in vectors or complex numbers.


## TXR


In TXR Lisp, a structure type can be created:


```txrlisp
(defstruct point nil (x 0) (y 0))
```


If it is okay for the coordinates to be initialized to <tt>nil</tt>, it can be condensed to:


```txrlisp
(defstruct point nil x y)
```


The <tt>nil</tt> denotes that a <tt>point</tt> has no supertype: it doesn't inherit from anything.

This structure type can then be instantiated using the <tt>new</tt> macro (not the only way):


```txrlisp
(new point)         ;; -> #S(point x 0 y 0)
(new point x 1)     ;; -> #S(point x 1 y 0)
(new point x 1 y 1) ;; -> #S(point x 1 y 1)
```


A structure can support optional by-order-of-arguments ("boa") construction by providing a "boa constructor". The <tt>defstruct</tt> syntactic sugar does this if a function-like syntax is used in place of the structure name:


```txrlisp
(defstruct (point x y) nil (x 0) (y 0))
```


The existing construction methods continue to work, but in addition, this is now possible:


```txrlisp
(new (point 3 4)) -> #S(point x 3 y 4)
```


Slot access syntax is supported. If variable <tt>p</tt> holds a point, then <tt>p.x</tt> designates the <tt>x</tt> slot, as a syntactic place which can be accessed and stored:


```txrlisp
(defun displace-point-destructively (p delta)
  (inc p.x delta.x)
  (inc p.y delta.y))
```



## UNIX Shell

ksh93 allows you to define new compound types with the <tt>typeset -T</tt> command.

```bash
typeset -T Point=(
  typeset x
  typeset y
)
Point p
p.x=1
p.y=2
echo $p
echo ${p.x} ${p.y}
Point q=(x=3 y=4)
echo ${q.x} ${q.y}
```

```txt
( x=1 y=2 )
1 2
3 4
```


You can also declare compound variables "on the fly" without using a defined type:

```bash
point=()
point.x=5
point.y=6
echo $point
echo ${point.x} ${point.y}
```

```txt
( x=5 y=6 )
5 6
```



## Ursala

A record type with two untyped fields named <code>x</code> and <code>y</code> can be declared like this.

```Ursala>point :: x y</lang

A constant instance of the record can be declared like this.

```Ursala
p = point[x: 'foo',y: 'bar']
```

A function returning a value of this type can be defined like this,

```Ursala
f = point$[x: g,y: h]
```

where <code>g</code> and <code>h</code> are functions. Then <code>f(p)</code> would evaluate to
<code>point[x: g(p),y: h(p)]</code> for a given argument <code>p</code>. Accessing the fields of
a record can be done like this.

```Ursala
t = ~x p
u = ~y p
```

where <code>p</code> is any expression of the defined type. A real application wouldn't be written
this way because pairs of values <code>(x,y)</code> are a common idiom.


## VBA


```vb
Type point
    x As Integer
    y As Integer
End Type
```


## Vim Script

One cannot create new data types in Vim Script. A point could be represented by a dictionary:


```vim
function MakePoint(x, y)    " 'Constructor'
    return {"x": a:x, "y": a:y}
endfunction

let p1 = MakePoint(3, 2)
let p2 = MakePoint(-1, -4)

echon "Point 1: x = " p1.x ", y = " p1.y "\n"
echon "Point 2: x = " p2.x ", y = " p2.y "\n"
```


```txt
Point 1: x = 3, y = 2
Point 2: x = -1, y = -4
```



## Visual Basic .NET



###  Structures


A simple structure with two public, mutable fields:

```vbnet
Structure Point
   Public X, Y As Integer
End Structure
```



###  Immutable Structures


It is generally recommended in .NET that mutable structures only be used in niche cases where they provide needed performance, e.g. when the creation of massive numbers of class instances would cause excessive garbage collection pressure, as high-performance code dealing with structs generally is of a paradigm considered "impure" from an object-oriented perspective that relies on passing by reference and directly exposing fields.

The semantics of value types in .NET mean that a new copy of a structure is created whenever one is passed by value to or from a method or property. This is particularly vexing when properties are involved, as it is not possible to mutate a structure that is returned by a property, due to the returned structure actually being an independent copy of whatever the property originally returned. The only workaround would be to store the value of the property in a temporary variable, mutate that variable, and assign the mutated variable back to the property, which involves another copy operation. When a structure is large, this copying can significantly affect performance.

On another note, algorithms relying on immutable data structures are often more easily parallelized, as they eliminate the race conditions caused by concurrent reading and writing.

Below is the same <code>Point</code> as above, except with an immutable API.


```vbnet
Structure ImmutablePoint
   ReadOnly Property X As Integer
   ReadOnly Property Y As Integer

   Public Sub New(x As Integer, y As Integer)
       Me.X = x
       Me.Y = y
   End Sub
End Structure
```



## XSLT

Data types in XSLT are expressed as XML nodes. Members of a node can be either attributes or child  nodes. Access to data is via XPath expressions.


### Attributes

Attributes are often used for simple values. This is how a point might be represented in SVG, for example.

```xml
<point x="20" y="30"/>

<!-- context is a point node. The '@' prefix selects named attributes of the current node. -->
<fo:block>Point = <xsl:value-of select="@x"/>, <xsl:value-of select="@y"/></fo:block>
```



### Children

More complex, multivariate, and nested data structures can be represented using child nodes.

```xml><circle

  <point>
    <x>20</x>
    <y>30</y>
  </point>
  <radius>10</radius>
</circle>

<!-- context is a circle node. Children are accessed using a path-like notation (hence the name "XPath"). -->
```

 <fo:block>Circle center = <xsl:value-of select="point/x"/>, <xsl:value-of select="point/y"/></fo:block>


## zonnon


```zonnon

{ref,public} (* class *)
	Point = object(ord,abs: integer)
	var
		(* instance variables *)
		{public,immutable} x,y: integer;

	(* method *)
	procedure {public} Ord():integer;
	begin
		return y
	end Ord;

	(* method *)
	procedure {public} Abs():integer;
	begin
		return x
	end Abs;

	(* constructor *)
	begin
		self.x := ord;
		self.y := abs;
	end Point;

```



## zkl

The OO solution:

```zkl
class Point{ var x,y;
   fcn init(x,y){self.x=x.toFloat(); self.y=y.toFloat(); }
   fcn toString{ "P(%f,%f)".fmt(x,y) }
   fcn __opADD(P){} //+: add Point, constant or whatever
   //... __opEQ == etc
}
Point(1,2).println() //-->P(1.000000,2.000000)
```

which can be pretty heavy weight. [read only] lists can work just as well:

```zkl
point:=T(1,2); points:=T( T(1,2), L(3,4) )
```


