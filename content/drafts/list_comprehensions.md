+++
title = "List comprehensions"
description = ""
date = 2018-11-24T23:37:50Z
aliases = []
[extra]
id = 2197
[taxonomies]
categories = []
tags = []
+++

{{task|Basic language learning}}
{{Omit From|Modula-3}}
{{omit from|ACL2}}
{{omit from|BBC BASIC}}

A [[wp:List_comprehension|list comprehension]] is a special syntax in some programming languages to describe lists. It is similar to the way mathematicians describe sets, with a ''set comprehension'', hence the name.

Some attributes of a list comprehension are:
# They should be distinct from (nested) for loops and the use of map and filter functions within the syntax of the language.
# They should return either a list or an iterator (something that returns successive members of a collection, in order).
# The syntax has parts corresponding to that of [[wp:Set-builder_notation|set-builder notation]].  


;Task:
Write a list comprehension that builds the list of all [[Pythagorean triples]] with elements between   '''1'''   and   '''n'''. 

If the language has multiple ways for expressing such a construct (for example, direct list comprehensions and generators), write one example for each.





## ABAP



```ABAP

CLASS lcl_pythagorean_triplet DEFINITION CREATE PUBLIC.
  PUBLIC SECTION.
    TYPES: BEGIN OF ty_triplet,
             x TYPE i,
             y TYPE i,
             z TYPE i,
           END OF ty_triplet,
           tty_triplets TYPE STANDARD TABLE OF ty_triplet WITH NON-UNIQUE EMPTY KEY.

    CLASS-METHODS:
      get_triplets
        IMPORTING
          n                 TYPE i
        RETURNING
          VALUE(r_triplets) TYPE tty_triplets.

  PRIVATE SECTION.
    CLASS-METHODS:
      _is_pythagorean
        IMPORTING
          i_triplet               TYPE ty_triplet
        RETURNING
          VALUE(r_is_pythagorean) TYPE abap_bool.
ENDCLASS.

CLASS lcl_pythagorean_triplet IMPLEMENTATION.
  METHOD get_triplets.
    DATA(triplets) = VALUE tty_triplets( FOR x = 1 THEN x + 1 WHILE x <= n
                                         FOR y = x THEN y + 1 WHILE y <= n
                                         FOR z = y THEN z + 1 WHILE z <= n
                                            ( x = x y = y z = z ) ).

    LOOP AT triplets ASSIGNING FIELD-SYMBOL(<triplet>).
      IF _is_pythagorean( <triplet> ) = abap_true.
        INSERT <triplet> INTO TABLE r_triplets.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD _is_pythagorean.
    r_is_pythagorean = COND #( WHEN i_triplet-x * i_triplet-x + i_triplet-y * i_triplet-y = i_triplet-z * i_triplet-z THEN abap_true
                               ELSE abap_false ).
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  cl_demo_output=>display( lcl_pythagorean_triplet=>get_triplets( n = 20 ) ).

```

 
{{out}}

```txt

X Y Z 
3 4 5 
5 12 13 
6 8 10 
8 15 17 
9 12 15 
12 16 20 

```



## Ada

{{works with|Ada 2005|Standard - no additional library}}
There is no equivalent construct in Ada. In Ada05, the predefined library Ada.Containers 
implements 3 types of Doubly_Linked_Lists : Basic; Indefinite; Restricted.

```Ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Containers.Doubly_Linked_Lists;

procedure Pythagore_Set is

   type Triangles is array (1 .. 3) of Positive;

   package Triangle_Lists is new Ada.Containers.Doubly_Linked_Lists (
      Triangles);
   use Triangle_Lists;

   function Find_List (Upper_Bound : Positive) return List is
      L : List := Empty_List;
   begin
      for A in 1 .. Upper_Bound loop
         for B in A + 1 .. Upper_Bound loop
            for C in B + 1 .. Upper_Bound loop
               if ((A * A + B * B) = C * C) then
                  Append (L, (A, B, C));
               end if;
            end loop;
         end loop;
      end loop;
      return L;
   end Find_List;

   Triangle_List : List;
   C             : Cursor;
   T             : Triangles;

begin
   Triangle_List := Find_List (Upper_Bound => 20);

   C := First (Triangle_List);
   while Has_Element (C) loop
      T := Element (C);
      Put
        ("(" &
         Integer'Image (T (1)) &
         Integer'Image (T (2)) &
         Integer'Image (T (3)) &
         ") ");
      Next (C);
   end loop;
end Pythagore_Set;

```


program output:

```txt
( 3 4 5) ( 5 12 13) ( 6 8 10) ( 8 15 17) ( 9 12 15) ( 12 16 20)
```



## ALGOL 68

ALGOL 68 does not have list comprehension, however it is ''sometimes'' reasonably
generous about where a '''flex''' array is declared.  And with the addition
of an append '''operator''' "+:=" for lists they can be similarly manipulated.

{{trans|Python}}

{{works with|ALGOL 68|Standard - no extensions to language used}}

{{works with|ALGOL 68G|Any - tested with release mk15-0.8b.fc9.i386}}

{{works with|ELLA ALGOL 68|Any (with appropriate job cards) - tested with release 1.8.8d.fc9.i386}}

```algol68
MODE XYZ = STRUCT(INT x,y,z);

OP +:= = (REF FLEX[]XYZ lhs, XYZ rhs)FLEX[]XYZ: (
  [UPB lhs+1]XYZ out;
  out[:UPB lhs] := lhs;
  out[UPB out] := rhs;
  lhs := out
);

INT n = 20;
print (([]XYZ(
  FLEX[0]XYZ xyz;
  FOR x TO n DO FOR y FROM x+1 TO n DO FOR z FROM y+1 TO n DO IF x*x + y*y = z*z THEN xyz +:= XYZ(x,y,z) FI OD OD OD;
  xyz), new line
))
```

Output:
<div style="width:full;overflow:scroll">
```txt

         +3         +4         +5         +5        +12        +13         +6         +8        +10         +8        +15        +17         +9        +12        +15        +12        +16        +20

```
</div>



## AppleScript


AppleScript does not provide built-in notation for list comprehensions. The list monad pattern which underlies list comprehension notation can, however, be used in any language which supports the use of higher order functions and closures. AppleScript can do that, although with limited elegance and clarity, and not entirely without coercion.

{{trans|JavaScript}}

```AppleScript
-- List comprehension by direct and unsugared use of list monad

-- pythagoreanTriples :: Int -> [(Int, Int, Int)]
on pythagoreanTriples(n)
    script x
        on |λ|(x)
            script y
                on |λ|(y)
                    script z
                        on |λ|(z)
                            if x * x + y * y = z * z then
                                [[x, y, z]]
                            else
                                []
                            end if
                        end |λ|
                    end script
                    
                    |>>=|(enumFromTo(1 + y, n), z)
                end |λ|
            end script
            
            |>>=|(enumFromTo(1 + x, n), y)
        end |λ|
    end script
    
    |>>=|(enumFromTo(1, n), x)
end pythagoreanTriples

-- TEST -----------------------------------------------------------------------
on run
    --   Pythagorean triples drawn from integers in the range [1..n]
    --  {(x, y, z) | x <- [1..n], y <- [x+1..n], z <- [y+1..n], (x^2 + y^2 = z^2)}
    
    pythagoreanTriples(25)
    
    --> {{3, 4, 5}, {5, 12, 13}, {6, 8, 10}, {7, 24, 25}, {8, 15, 17}, 
    --   {9, 12, 15}, {12, 16, 20}, {15, 20, 25}}
end run


-- GENERIC FUNCTIONS ----------------------------------------------------------

-- Monadic (>>=) (or 'bind') for lists is simply flip concatMap
-- (concatMap with arguments reversed)
-- It applies a function f directly to each value in the list,
-- and returns the set of results as a concat-flattened list

-- The concatenation eliminates any empty lists,
-- combining list-wrapped results into a single results list

-- (>>=) :: Monad m => m a -> (a -> m b) -> m b
on |>>=|(xs, f)
    concatMap(f, xs)
end |>>=|

-- concatMap :: (a -> [b]) -> [a] -> [b]
on concatMap(f, xs)
    set acc to {}
    tell mReturn(f)
        repeat with x in xs
            set acc to acc & |λ|(contents of x)
        end repeat
    end tell
    return acc
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
```

{{Out}}

```AppleScript
{{3, 4, 5}, {5, 12, 13}, {6, 8, 10}, {7, 24, 25}, {8, 15, 17}, {9, 12, 15}, {12, 16, 20}, {15, 20, 25}}
```



## AutoHotkey

{{works with|AutoHotkey_L}}
List Comprehension is not built in. 
```AutoHotkey

comprehend("show", range(1, 20), "triples")
return

comprehend(doToVariable, inSet, satisfying)
{
  set := %satisfying%(inSet.begin, inSet.end)
  index := 1
  While % set[index, 1]
  {
    item := set[index, 1] . ", " . set[index, 2] . ", " . set[index, 3]
    %doToVariable%(item)
    index += 1
  }
  return
}


show(var)
{
  msgbox % var
}

range(begin, end)
 {
   set := object()
   set.begin := begin
   set.end := end
   return set
 }
 
!r::reload
!q::exitapp

triples(begin, end)
{
  
  set := object()
  index := 1
  range := end - begin
  
  loop, % range
  {
    x := begin + A_Index 
    loop, % range
    {
      y := A_Index + x 
      if y > 20
	break
loop, % range
{
  z := A_Index + y 
  if z > 20
    break
  isTriple := ((x ** 2 + y ** 2) == z ** 2)
  if isTriple
  {
    set[index, 1] := x 
    set[index, 2] := y
    set[index, 3] := z
    index += 1
  ; msgbox % "triple: "  x . y . z
  }
  
}
}
}
return set
}

```



## Bracmat

Bracmat does not have built-in list comprehension, but nevertheless there is a solution for fixed n that does not employ explicit loop syntax. By their positions in the pattern and the monotonically increasing values in the subject, it is guaranteed that <code>x</code> always is smaller than <code>y</code> and that <code>y</code> always is smaller than <code>z</code>. The combination of flags <code>%@</code> ensures that <code>x</code>, <code>y</code> and <code>z</code> pick minimally one (<code>%</code>) and at most one (<code>@</code>) element from the subject list.  

```bracmat

  :?py                         { Initialize the accumulating result list. }
& (     1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20   { This is the subject }
    :   ?                      { Here starts the pattern }
        %@?x
        ?
        %@?y
        ?
        %@?z
        ( ?
        & -1*!z^2+!x^2+!y^2:0
        & (!x,!y,!z) !py:?py
        & ~                    { This 'failure' expression forces backtracking }
        )                      { Here ends the pattern }
  | out$!py                    { You get here when backtracking has 
                                 exhausted all combinations of x, y and z }
  );
```

Output:

```txt
  (12,16,20)
  (9,12,15)
  (8,15,17)
  (6,8,10)
  (5,12,13)
  (3,4,5)
```



## C


C doesn't have a built-in syntax for this, but any problem can be solved if you throw enough macros at it:
{{works with|GCC}}
The program below is C11 compliant. For C99 compilers change line 57 :

```C

for (int i = f + 1; i <= t; i ++) { e = e->nx = listNew(sizeof i, &i); }

```

to

```C

int i;
for (i = f + 1; i <= t; i ++) { e = e->nx = listNew(sizeof i, &i); }

```

Output remains unchanged.

```c

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#ifndef __GNUC__
#include <setjmp.h>
struct LOOP_T; typedef struct LOOP_T LOOP;
struct LOOP_T {
    jmp_buf b; LOOP * p;
} LOOP_base, * LOOP_V = &LOOP_base;
#define FOR(I, C, A, ACT) (LOOP_V = &(LOOP){ .p = LOOP_V }, \
                           (I), setjmp(LOOP_V->b), \
                           ((C) ? ((ACT),(A), longjmp(LOOP_V->b, 1), 0) : 0), \
                           LOOP_V = LOOP_V->p, 0)
#else
#define FOR(I, C, A, ACT) (({for(I;C;A){ACT;}}), 0)    // GNU version
#endif

typedef struct List { struct List * nx; char val[]; } List;
typedef struct { int _1, _2, _3; } Triple;

#define SEQ(OUT, SETS, PRED) (SEQ_var=&(ITERATOR){.l=NULL,.p=SEQ_var}, \
                              M_FFOLD(((PRED)?APPEND(OUT):0),M_ID SETS), \
                              SEQ_var->p->old=SEQ_var->l,SEQ_var=SEQ_var->p,SEQ_var->old)
typedef struct ITERATOR { List * l, * old; struct ITERATOR * p; } ITERATOR;
ITERATOR * FE_var, SEQ_base, * SEQ_var = &SEQ_base;
#define FOR_EACH(V, T, L, ACT) (FE_var=&(ITERATOR){.l=(L),.p=FE_var}, \
                                FOR((V) = *(T*)&FE_var->l->val, FE_var->l?((V)=*(T*)&FE_var->l->val,1):0, \
                                FE_var->l=FE_var->l->nx, ACT), FE_var=FE_var->p)

#define M_FFOLD(ID, ...) M_ID(M_CONC(M_FFOLD_, M_NARGS(__VA_ARGS__)) (ID, __VA_ARGS__))
#define FORSET(V, T, L) V, T, L
#define APPEND(T, val) (SEQ_var->l?listAppend(SEQ_var->l,sizeof(T),&val):(SEQ_var->l=listNew(sizeof(T),&val)))

#define M_FFOLD_1(ID, E) FOR_EACH M_IDP(FORSET E, ID)
#define M_FFOLD_2(ID, E, ...) FOR_EACH M_IDP(FORSET E, M_FFOLD_1(ID, __VA_ARGS__))
#define M_FFOLD_3(ID, E, ...) FOR_EACH M_IDP(FORSET E, M_FFOLD_2(ID, __VA_ARGS__))  //...

#define M_NARGS(...) M_NARGS_(__VA_ARGS__, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0)
#define M_NARGS_(_10, _9, _8, _7, _6, _5, _4, _3, _2, _1, N, ...) N
#define M_CONC(A, B) M_CONC_(A, B)
#define M_CONC_(A, B) A##B
#define M_ID(...) __VA_ARGS__
#define M_IDP(...) (__VA_ARGS__)

#define R(f, t) int,intRangeList(f, t)
#define T(a, b, c) Triple,((Triple){(a),(b),(c)})

List * listNew(int sz, void * val) {
 List * l = malloc(sizeof(List) + sz); l->nx = NULL; memcpy(l->val, val, sz); return l;
}
List * listAppend(List * l, int sz, void * val) {
 while (l->nx) { l = l->nx; } l->nx = listNew(sz, val); return l;
}
List * intRangeList(int f, int t) {
 List * l = listNew(sizeof f, &f), * e = l;
 for (int i = f + 1; i <= t; i ++) { e = e->nx = listNew(sizeof i, &i); }
 return l;
}

int main(void) {
    volatile int x, y, z; const int n = 20;
    
    List * pTriples = SEQ(
                          T(x, y, z),
                          (
                           (x, R(1, n)),
                           (y, R(x, n)),
                           (z, R(y, n))
                          ),
                          (x*x + y*y == z*z)
                         );
    
    volatile Triple t;
    FOR_EACH(t, Triple, pTriples,  printf("%d, %d, %d\n", t._1, t._2, t._3)  );
    
    return 0;
}

```

Output:

```txt
3, 4, 5
5, 12, 13
6, 8, 10
8, 15, 17
9, 12, 15
12, 16, 20
```

Either GCC's "statement expressions" extension, or a minor piece of undefined behaviour, are required for this to work (technically <code>setjmp</code>, which powers the non-GCC version, isn't supposed to be part of a comma expression, but almost all compilers will treat it as intended). Variables used by one of these looping expressions should be declared <code>volatile</code> to prevent them from being modified by <code>setjmp</code>.

The list implementation in this example is a) terrible and b) leaks memory, neither of which are important to the example. In reality you would want to combine any lists being generated in expressions with an automatic memory management system (GC, autorelease pools, something like that).

=={{header|C sharp|C#}}==

### LINQ


```csharp
using System.Linq;

static class Program
{
  static void Main()
  {
    var ts =
      from a in Enumerable.Range(1, 20)
      from b in Enumerable.Range(a, 21 - a)
      from c in Enumerable.Range(b, 21 - b)
      where a * a + b * b == c * c
      select new { a, b, c };

      foreach (var t in ts)
        System.Console.WriteLine("{0}, {1}, {2}", t.a, t.b, t.c);
  }
}

```



## C++

There is no equivalent construct in C++. The code below uses two nested loops and an ''if'' statement:


```cpp>#include <vector

#include <cmath>
#include <iostream>
#include <algorithm>
#include <iterator>

void list_comprehension( std::vector<int> & , int ) ;

int main( ) {
   std::vector<int> triangles ;
   list_comprehension( triangles , 20 ) ;
   std::copy( triangles.begin( ) , triangles.end( ) ,
	 std::ostream_iterator<int>( std::cout , " " ) ) ;
   std::cout << std::endl ;
   return 0 ;
}

void list_comprehension( std::vector<int> & numbers , int upper_border ) {
   for ( int a = 1 ; a < upper_border ; a++ ) {
      for ( int b = a + 1 ; b < upper_border ; b++ ) {
	 double c = pow( a * a + b * b , 0.5 ) ; //remembering Mr. Pythagoras
	 if ( ( c * c ) < pow( upper_border , 2 ) + 1 ) {
	    if ( c == floor( c ) ) {
	       numbers.push_back( a ) ;
	       numbers.push_back( b ) ;	      
	       numbers.push_back( static_cast<int>( c ) ) ;
	    }
	 }
      }
   }
}

```


This produces the following output:

```txt
3 4 5 5 12 13 6 8 10 8 15 17 9 12 15 12 16 20
```


{{works with|C++11}}


```cpp>#include <functional

#include <iostream>

void PythagoreanTriples(int limit, std::function<void(int,int,int)> yield)
{
    for (int a = 1; a < limit; ++a) {
        for (int b = a+1; b < limit; ++b) {
            for (int c = b+1; c <= limit; ++c) {
                if (a*a + b*b == c*c) {
                    yield(a, b, c);
                }
            }
        }
    }
}

int main()
{
    PythagoreanTriples(20, [](int x, int y, int z)
    {
        std::cout << x << "," << y << "," << z << "\n";
    });
    return 0;
}
```


Output:

```txt
3,4,5
5,12,13
6,8,10
8,15,17
9,12,15
12,16,20

```



## Clojure


```lisp
(defn pythagorean-triples [n]
  (for [x (range 1 (inc n))
	y (range x (inc n))
	z (range y (inc n))
	:when (= (+ (* x x) (* y y)) (* z z))]
    [x y z]))
```



## CoffeeScript


```coffeescript
flatten = (arr) -> arr.reduce ((memo, b) -> memo.concat b), []

pyth = (n) ->
  flatten (for x in [1..n]
    flatten (for y in [x..n]
      for z in [y..n] when x*x + y*y is z*z
        [x, y, z]
    ))

console.dir pyth 20
```


<code>pyth</code> can also be written more concisely as


```coffeescript
pyth = (n) -> flatten (flatten ([x, y, z] for z in [y..n] when x*x + y*y is z*z for y in [x..n]) for x in [1..n])
```



## Common Lisp

Common Lisp's <tt>loop</tt> macro has all of the features of list comprehension, except for nested iteration. <tt>(loop for x from 1 to 3 for y from x to 3 collect (list x y))</tt> only returns <tt>((1 1) (2 2) (3 3))</tt>. You can nest your macro calls, so <tt>(loop for x from 1 to 3 append (loop for y from x to 3 collect (list x y)))</tt> returns <tt>((1 1) (1 2) (1 3) (2 2) (2 3) (3 3))</tt>.

Here are the Pythagorean triples:


```lisp
(defun pythagorean-triples (n)
  (loop for x from 1 to n
        append (loop for y from x to n
                     append (loop for z from y to n
                                  when (= (+ (* x x) (* y y)) (* z z))
                                  collect (list x y z)))))
```


We can also define a new macro for list comprehensions. We can implement them easily with the help of the <code>iterate</code> package.


```lisp
(defun nest (l)
  (if (cdr l)
    `(,@(car l) ,(nest (cdr l)))
    (car l)))

(defun desugar-listc-form (form)
  (if (string= (car form) 'for)
    `(iter ,form)
    form))

(defmacro listc (expr &body (form . forms) &aux (outer (gensym)))
  (nest
    `((iter ,outer ,form)
      ,@(mapcar #'desugar-listc-form forms)
      (in ,outer (collect ,expr)))))
```


We can then define a function to compute Pythagorean triples as follows:


```lisp
(defun pythagorean-triples (n)
  (listc (list x y z)
    (for x from 1 to n)
    (for y from x to n)
    (for z from y to n)
    (when (= (+ (expt x 2) (expt y 2)) (expt z 2)))))
```



## D

D doesn't have list comprehensions. One implementation:

```d
import std.stdio, std.typetuple, std.range;

TA[] select(TA, TI1, TC1, TI2, TC2, TI3, TC3, TP)
           (lazy TA mapper,
            ref TI1 iter1, TC1 items1,
            ref TI2 iter2, lazy TC2 items2,
            ref TI3 iter3, lazy TC3 items3,
            lazy TP where) {
  Appender!(TA[]) result;
  auto iters = TypeTuple!(iter1, iter2, iter3);

  foreach (el1; items1) {
    iter1 = el1;
    foreach (el2; items2) {
      iter2 = el2;
      foreach (el3; items3) {
        iter3 = el3;
        if (where())
          result ~= mapper();
      }
    }
  }

  TypeTuple!(iter1, iter2, iter3) = iters;
  return result.data;
}

void main() {
  enum int n = 21;
  int x, y, z;
  auto r = select([x,y,z], x, iota(1,n+1), y, iota(x,n+1), z,
                  iota(y, n + 1), x*x + y*y == z*z);
  writeln(r);
}
```

{{out}}

```txt
[[3, 4, 5], [5, 12, 13], [6, 8, 10], [8, 15, 17], [9, 12, 15], [12, 16, 20]]
```



## E



```e
pragma.enable("accumulator") # considered experimental

accum [] for x in 1..n { for y in x..n { for z in y..n { if (x**2 + y**2 <=> z**2) { _.with([x,y,z]) } } } }
```



## EchoLisp


```lisp

;; copied from Racket

(for*/list ([x (in-range 1 21)]
[y (in-range x 21)]
[z (in-range y 21)])
#:when (= (+ (* x x) (* y y)) (* z z))
(list x y z))
    → ((3 4 5) (5 12 13) (6 8 10) (8 15 17) (9 12 15) (12 16 20))

```



## Efene



```efene
pythag = fn (N) {
    [(A, B, C) for A in lists.seq(1, N) \
         for B in lists.seq(A, N) \
         for C in lists.seq(B, N) \
         if A + B + C <= N and A * A + B * B == C * C]
}

@public 
run = fn () {
    io.format("~p~n", [pythag(20)])
}

```



## Ela



```ela
pyth n = [(x,y,z) \\ x <- [1..n], y <- [x..n], z <- [y..n] | x**2 + y**2 == z**2]
```



## Elixir


```txt

iex(30)> pytha3 = fn(n) ->
...(30)>   for x <- 1..n, y <- x..n, z <- y..n, x*x+y*y == z*z, do: {x,y,z}
...(30)> end
#Function<6.90072148/1 in :erl_eval.expr/5>
iex(31)> pytha3.(20)
[{3, 4, 5}, {5, 12, 13}, {6, 8, 10}, {8, 15, 17}, {9, 12, 15}, {12, 16, 20}]

```



## Erlang



```erlang
pythag(N) ->
    [ {A,B,C} || A <- lists:seq(1,N),
                 B <- lists:seq(A,N),
                 C <- lists:seq(B,N),
                 A+B+C =< N,
                 A*A+B*B == C*C ].
```


=={{header|F Sharp|F#}}==

```fsharp
let pyth n = [ for a in [1..n] do
               for b in [a..n] do
               for c in [b..n] do
               if (a*a+b*b = c*c) then yield (a,b,c)]
```



## Factor

Factor does not support list comprehensions by default. The <code>backtrack</code> vocabulary can make for a faithful imitation, however.

```factor
USING: backtrack kernel locals math math.ranges ;

:: pythagorean-triples ( n -- seq )
    [
          n [1,b] amb-lazy :> a
        a n [a,b] amb-lazy :> b
        b n [a,b] amb-lazy :> c
        a a * b b * + c c * = must-be-true { a b c }
    ] bag-of ;
```



## Fortran


Complex numbers simplify the task.  However, the reshape intrinsic function along with implicit do loops can generate high rank matrices.

```FORTRAN

!-*- mode: compilation; default-directory: "/tmp/" -*-
!Compilation started at Fri Jun  7 23:39:20
!
!a=./f && make $a && $a
!gfortran -std=f2008 -Wall -fopenmp -ffree-form -fall-intrinsics -fimplicit-none f.f08 -o f
!   3   4   5
!   5  12  13
!   6   8  10
!   8  15  17
!   9  12  15
!  12  16  20
!
!Compilation finished at Fri Jun  7 23:39:20
 
program list_comprehension
  integer, parameter :: n = 20
  integer, parameter :: m = n*(n+1)/2
  integer :: i, j
  complex, dimension(m) :: a
  real, dimension(m) :: b
  logical, dimension(m) :: c
  integer, dimension(3, m) :: d
  a = [ ( ( cmplx(i,j), i=j,n), j=1,n) ] ! list comprehension, implicit do loop
  b = abs(a)
  c = (b .eq. int(b)) .and. (b .le. n)
  i = sum(merge(1,0,c))
  d(2,:i) = int(real(pack(a, c))) ! list comprehensions: array
  d(1,:i) = int(imag(pack(a, c))) ! assignments and operations.
  d(3,:i) = int(pack(b,c))
  print '(3i4)',d(:,:i)
end program list_comprehension

```



## FunL


```funl
def triples( n ) = [(a, b, c) | a <- 1..n-2, b <- a+1..n-1, c <- b+1..n if a^2 + b^2 == c^2]

println( triples(20) )
```


{{out}}


```txt

[(3, 4, 5), (5, 12, 13), (6, 8, 10), (8, 15, 17), (9, 12, 15), (12, 16, 20)]

```



## GAP


```gap
# We keep only primitive pythagorean triples
pyth := n ->
  Filtered(Cartesian([1 .. n], [1 .. n], [1 .. n]),
  u -> u[3]^2 = u[1]^2 + u[2]^2 and u[1] < u[2] 
  and GcdInt(u[1], u[2]) = 1);

pyth(100);
# [ [ 3, 4, 5 ], [ 5, 12, 13 ], [ 7, 24, 25 ], [ 8, 15, 17 ], [ 9, 40, 41 ], [ 11, 60, 61 ], [ 12, 35, 37 ], 
#   [ 13, 84, 85 ], [ 16, 63, 65 ], [ 20, 21, 29 ], [ 28, 45, 53 ], [ 33, 56, 65 ], [ 36, 77, 85 ], [ 39, 80, 89 ], 
#   [ 48, 55, 73 ], [ 65, 72, 97 ] ]
```



## Go

Go doesn't have special syntax for list comprehensions but we can build a function which behaves similarly.

```go
package main

import "fmt"

type (
    seq  []int
    sofs []seq
)

func newSeq(start, end int) seq {
    if end < start {
        end = start
    }
    s := make(seq, end-start+1)
    for i := 0; i < len(s); i++ {
        s[i] = start + i
    }
    return s
}

func newSofs() sofs {
    return sofs{seq{}}
}

func (s sofs) listComp(in seq, expr func(sofs, seq) sofs, pred func(seq) bool) sofs {
    var s2 sofs
    for _, t := range expr(s, in) {
        if pred(t) {
            s2 = append(s2, t)
        }
    }
    return s2
}

func (s sofs) build(t seq) sofs {
    var u sofs
    for _, ss := range s {
        for _, tt := range t {
            uu := make(seq, len(ss))
            copy(uu, ss)
            uu = append(uu, tt)
            u = append(u, uu)
        }
    }
    return u
}

func main() {
    pt := newSofs()
    in := newSeq(1, 20)
    expr := func(s sofs, t seq) sofs {
        return s.build(t).build(t).build(t)
    }
    pred := func(t seq) bool {
        if len(t) != 3 {
            return false
        }
        return t[0]*t[0]+t[1]*t[1] == t[2]*t[2] && t[0] < t[1] && t[1] < t[2]
    }
    pt = pt.listComp(in, expr, pred)
    fmt.Println(pt)
}
```


{{out}}

```txt

[[3 4 5] [5 12 13] [6 8 10] [8 15 17] [9 12 15] [12 16 20]]

```



## Haskell


```haskell
pyth :: Int -> [(Int, Int, Int)]
pyth n =
  [ (x, y, z)
  | x <- [1 .. n] 
  , y <- [x .. n] 
  , z <- [y .. n] 
  , x ^ 2 + y ^ 2 == z ^ 2 ]
```


List-comprehensions and do notation are two alternative and equivalent forms of syntactic sugar in Haskell.

The list comprehension above could be re-sugared in Do notation as:


```haskell
pyth :: Int -> [(Int, Int, Int)]
pyth n = do
  x <- [1 .. n]
  y <- [x .. n]
  z <- [y .. n]
  if x ^ 2 + y ^ 2 == z ^ 2
  then [(x, y, z)]
  else []
```


and both of the above could be de-sugared to:

```haskell
pyth :: Int -> [(Int, Int, Int)]
pyth n =
  [1 .. n] >>=
  \x ->
     [x .. n] >>=
     \y ->
        [y .. n] >>=
        \z ->
           case x ^ 2 + y ^ 2 == z ^ 2 of
             True -> [(x, y, z)]
             _ -> []
```


which can be further specialised (given the particular context of the list monad, 

in which (>>=) is flip concatMap, pure is flip (:) [], and empty is []) to:


```haskell
pyth :: Int -> [(Int, Int, Int)]
pyth n =
  concatMap
    (\x ->
        concatMap
          (\y ->
              concatMap
                (\z ->
                    if x ^ 2 + y ^ 2 == z ^ 2
                      then [(x, y, z)]
                      else [])
                [y .. n])
          [x .. n])
    [1 .. n]

main :: IO ()
main = print $ pyth 25
```

{{Out}}

```txt
[(3,4,5),(5,12,13),(6,8,10),(7,24,25),(8,15,17),(9,12,15),(12,16,20),(15,20,25)]
```


Finally an alternative to the list comprehension from the beginning. First introduce all triplets:


```haskell
triplets n = [(x, y, z) | x <- [1 .. n], y <- [x .. n], z <- [y .. n]]
```


If we apply this to our list comprehension we get this tidy line of code:


```haskell
[(x, y, z) | (x, y, z) <- triplets n, x^2 + y^2 == z^2]
```



## Hy


```clojure
(defn triples [n]
  (list-comp (, a b c) [a (range 1 (inc n))
                        b (range a (inc n))
                        c (range b (inc n))]
             (= (pow c 2)
                (+ (pow a 2)
                   (pow b 2)))))

(print (triples 15))
; [(3, 4, 5), (5, 12, 13), (6, 8, 10), (9, 12, 15)]
```


=={{header|Icon}} and {{header|unicon}} ==

Icon's (and Unicon's) natural goal-directly evaluation produces result
sequences and can be used to form list comprehensions.  For example, the
expression:


```unicon

    |(x := seq(), x^2 > 3, x*2)

```


is capable of producing successive elements from the infinite list described in the
Wikipedia article.  For example, to produce the first 100 elements:


```unicon

   procedure main()
      every write(|(x := seq(), x^2 > 3, x*2) \ 100
   end

```


While result sequences are lexically bound to the code that describes them,
that code can be embedded in a co-expression to allow access to the result
sequence throughout the code.  So Pythagorean triples can be produced
with (works in both languages):


```unicon
procedure main(a)
    n := integer(!a) | 20
    s := create (x := 1 to n, y := x to n, z := y to n, x^2+y^2 = z^2, [x,y,z])
    while a := @s do write(a[1]," ",a[2]," ",a[3])
end
```


Sample output:

```txt

->lc
3 4 5
5 12 13
6 8 10
8 15 17
9 12 15
12 16 20
->

```



## Ioke


```ioke
for(
  x <- 1..20, 
  y <- x..20, 
  z <- y..20,
  x * x + y * y == z * z,
  [x, y, z]
)
```



## J


```J
require'stats'
buildSet=:conjunction def '(#~ v) u y'
triples=: 1 + 3&comb
isPyth=: 2&{"1 = 1&{"1 +&.:*: 0&{"1
pythTr=: triples buildSet isPyth
```


The idiom here has two major elements:

First, you need a statement indicating the values of interest.  In this case, (1+3&comb) which when used as a function of n specifies a list of triples each in the range 1..n.

Second, you need a statement of the form (#~ B) where B returns true for the desired members, and false for the undesired members.

In the above example, the word isPyth is our predicate (represented as B in the preceding paragraph).  This corresponds to the constraint clause in set builder notation.

In the above example the word triples represents the universe of potential solutions (some of which will be valid, some not).  This corresponds to the generator part of set builder notation.

The argument to isPyth will be the candidate solutions (the result of triples).  The argument to triples will be the largest element desired in a triple.

Example use:


```J
   pythTr 20
 3  4  5
 5 12 13
 6  8 10
 8 15 17
 9 12 15
12 16 20
```



## Java

Java can stream a list, allowing something like a list comprehension. The syntax is (unsurprisingly) verbose, so you might wonder how good the likeness is. I've labeled the parts according to the description in Wikipedia.

Using list-of-arrays made the syntax easier than list-of-lists, but meant that you need the "output expression" part to get to something easily printable.

```Java
// Boilerplate
import java.util.Arrays;
import java.util.List;
import static java.util.function.Function.identity;
import static java.util.stream.Collectors.toList;
import static java.util.stream.IntStream.range;
public interface PythagComp{
    static void main(String... args){
        System.out.println(run(20));
    }

    static List<List<Integer>> run(int n){
        return
            // Here comes the list comprehension bit
            // input stream - bit clunky
            range(1, n).mapToObj(
                x -> range(x, n).mapToObj(
                    y -> range(y, n).mapToObj(
                        z -> new Integer[]{x, y, z}
                    )
                )
            )
                .flatMap(identity())
                .flatMap(identity())
                // predicate
                .filter(a -> a[0]*a[0] + a[1]*a[1] == a[2]*a[2])
                // output expression
                .map(Arrays::asList)
                // the result is a list
                .collect(toList())
        ;
    }
}

```

{{Out}}

```txt
[[3, 4, 5], [5, 12, 13], [6, 8, 10], [8, 15, 17], [9, 12, 15]]
```



## JavaScript



### ES5


ES5 does not provide built-in notation for list comprehensions. The list monad pattern which underlies list comprehension notation can, however, be used in any language which supports the use of higher order functions. The following shows how we can achieve the same result by directly using a list monad in ES5, without the abbreviating convenience of any specific syntactic sugar. 


```javascript
// USING A LIST MONAD DIRECTLY, WITHOUT SPECIAL SYNTAX FOR LIST COMPREHENSIONS

(function (n) {

    return mb(r(1,     n), function (x) {  // x <- [1..n]
    return mb(r(1 + x, n), function (y) {  // y <- [1+x..n]
    return mb(r(1 + y, n), function (z) {  // z <- [1+y..n]
       
       return x * x + y * y === z * z ? [[x, y, z]] : [];
       
    })})});


    // LIBRARY FUNCTIONS
    
    // Monadic bind for lists
    function mb(xs, f) {
        return [].concat.apply([], xs.map(f));
    }
    
    // Monadic return for lists is simply lambda x -> [x]
    // as in [[x, y, z]] : [] above

    // Integer range [m..n]
    function r(m, n) {
        return Array.apply(null, Array(n - m + 1))
            .map(function (n, x) {
                return m + x;
            });
    }

})(100);
```


Output:


```txt
[[3, 4, 5], [5, 12, 13], [6, 8, 10], [7, 24, 25], [8, 15, 17], [9, 12, 15], [9, 40, 41], [10, 24, 26], [11, 60, 61], [12, 16, 20], [12, 35, 37], [13, 84, 85], [14, 48, 50], [15, 20, 25], [15, 36, 39], [16, 30, 34], [16, 63, 65], [18, 24, 30], [18, 80, 82], [20, 21, 29], [20, 48, 52], [21, 28, 35], [21, 72, 75], [24, 32, 40], [24, 45, 51], [24, 70, 74], [25, 60, 65], [27, 36, 45], [28, 45, 53], [28, 96, 100], [30, 40, 50], [30, 72, 78], [32, 60, 68], [33, 44, 55], [33, 56, 65], [35, 84, 91], [36, 48, 60], [36, 77, 85], [39, 52, 65], [39, 80, 89], [40, 42, 58], [40, 75, 85], [42, 56, 70], [45, 60, 75], [48, 55, 73], [48, 64, 80], [51, 68, 85], [54, 72, 90], [57, 76, 95], [60, 63, 87], [60, 80, 100], [65, 72, 97]]
```



### ES6

{{trans|Python}}

{{works with|JavaScript|1.7+ (Firefox 2+)}} {{works with|SpiderMonkey|1.7}}

See [https://developer.mozilla.org/en/New_in_JavaScript_1.7#Array_comprehensions here] for more details


```javascript
function range(begin, end) {
    for (let i = begin; i < end; ++i)
        yield i;
}

function triples(n) {
    return [
        [x, y, z]
        for each(x in range(1, n + 1))
        for each(y in range(x, n + 1))
        for each(z in range(y, n + 1))
        if (x * x + y * y == z * z)
    ]
}

for each(var triple in triples(20))
print(triple);
```


outputs:

```txt
3,4,5
5,12,13
6,8,10
8,15,17
9,12,15
12,16,20
```



List comprehension notation was not, in the end, included in the final ES6 standard, and the code above will not run in fully ES6-compliant browsers or interpreters, but we can still go straight to the underlying monadic logic of list comprehensions and obtain: 

<code>[ (x, y, z)
| x <- [1 .. n], y <- [x .. n], z <- [y .. n], x ^ 2 + y ^ 2 == z ^ 2 ]</code>

by using <code>concatMap</code> (the monadic bind function for lists), and <code>x => [x]</code> (monadic pure/return for lists):


```JavaScript
(n => {
    'use strict';

    // GENERIC FUNCTIONS ------------------------------------------------------

    // concatMap :: (a -> [b]) -> [a] -> [b]
    const concatMap = (f, xs) => [].concat.apply([], xs.map(f));

    // enumFromTo :: Int -> Int -> [Int]
    const enumFromTo = (m, n) =>
        Array.from({
            length: Math.floor(n - m) + 1
        }, (_, i) => m + i);


    // EXAMPLE ----------------------------------------------------------------

    // [(x, y, z) | x <- [1..n], y <- [x..n], z <- [y..n], x ^ 2 + y ^ 2 == z ^ 2]

    return concatMap(x =>
           concatMap(y =>
           concatMap(z =>

                x * x + y * y === z * z ? [
                    [x, y, z]
                ] : [],

           enumFromTo(y, n)),
           enumFromTo(x, n)),
           enumFromTo(1, n));
})(20);
```


Or, expressed in terms of bind (>>=)


```Javascript
(n => {
    'use strict';

    // GENERIC FUNCTIONS ------------------------------------------------------

    // bind (>>=) :: Monad m => m a -> (a -> m b) -> m b
    const bind = (m, mf) =>
        Array.isArray(m) ? (
            bindList(m, mf)
        ) : bindMay(m, mf);

    // bindList (>>=) :: [a] -> (a -> [b]) -> [b]
    const bindList = (xs, mf) => [].concat.apply([], xs.map(mf));

    // enumFromTo :: Enum a => a -> a -> [a]
    const enumFromTo = (m, n) =>
        (typeof m !== 'number' ? (
            enumFromToChar
        ) : enumFromToInt)
        .apply(null, [m, n]);

    // enumFromToInt :: Int -> Int -> [Int]
    const enumFromToInt = (m, n) =>
        Array.from({
            length: Math.floor(n - m) + 1
        }, (_, i) => m + i);


    // EXAMPLE ----------------------------------------------------------------

    // [(x, y, z) | x <- [1..n], y <- [x..n], z <- [y..n], x ^ 2 + y ^ 2 == z ^ 2]

    return   bind(enumFromTo(1, n),
        x => bind(enumFromTo(x, n),
        y => bind(enumFromTo(y, n),
        z => x * x + y * y === z * z ? [
                    [x, y, z]
            ] : []
        )));

})(20);
```

{{Out}}

```JavaScript
[[3, 4, 5], [5, 12, 13], [6, 8, 10], [8, 15, 17], [9, 12, 15], [12, 16, 20]]
```



## jq

'''Direct approach''':
```jq
def triples(n):
  range(1;n+1) as $x | range($x;n+1) as $y | range($y;n+1) as $z
  | select($x*$x + $y*$y == $z*$z)
  | [$x, $y, $z] ;

```


'''Using listof(stream; criterion)'''

```jq
# listof( stream; criterion) constructs an array of those 
# elements in the stream that satisfy the criterion
def listof( stream; criterion): [ stream|select(criterion) ];

def listof_triples(n):
  listof( range(1;n+1) as $x | range($x;n+1) as $y | range($y;n+1) as $z 
          | [$x, $y, $z]; 
          .[0] * .[0] +  .[1] * .[1] ==  .[2] * .[2] ) ;

listof_triples(20)
```

{{out}}

```txt

$ jq -c -n -f list_of_triples.jq
[[3,4,5],[5,12,13],[6,8,10],[8,15,17],[9,12,15],[12,16,20]]

```



## Julia

Array comprehension:


```Julia

julia> n = 20
20

julia> [(x, y, z) for x = 1:n for y = x:n for z = y:n if x^2 + y^2 == z^2]
6-element Array{Tuple{Int64,Int64,Int64},1}:
 (3,4,5)
 (5,12,13)
 (6,8,10)
 (8,15,17)
 (9,12,15)
 (12,16,20)

```


A Julia generator comprehension (note the outer round brackets), returns an iterator over the same result rather than an explicit array:


```Julia

julia> ((x, y, z) for x = 1:n for y = x:n for z = y:n if x^2 + y^2 == z^2)
Base.Flatten{Base.Generator{UnitRange{Int64},##33#37}}(Base.Generator{UnitRange{Int64},##33#37}(#33,1:20))

julia> collect(ans)
6-element Array{Tuple{Int64,Int64,Int64},1}:
 (3,4,5)
 (5,12,13)
 (6,8,10)
 (8,15,17)
 (9,12,15)
 (12,16,20)

```


Array comprehensions may also be N-dimensional, not just vectors:


```Julia

julia> [i + j for i in 1:5, j in 1:5]
5×5 Array{Int64,2}:
 2  3  4  5   6
 3  4  5  6   7
 4  5  6  7   8
 5  6  7  8   9
 6  7  8  9  10

julia> [i + j for i in 1:5, j in 1:5, k in 1:2]
5×5×2 Array{Int64,3}:
[:, :, 1] =
 2  3  4  5   6
 3  4  5  6   7
 4  5  6  7   8
 5  6  7  8   9
 6  7  8  9  10

[:, :, 2] =
 2  3  4  5   6
 3  4  5  6   7
 4  5  6  7   8
 5  6  7  8   9
 6  7  8  9  10

```



## Kotlin


```scala
// version 1.0.6

fun pythagoreanTriples(n: Int) = 
    (1..n).flatMap { 
        x -> (x..n).flatMap { 
            y -> (y..n).filter {
                z ->  x * x + y * y == z * z 
            }.map { Triple(x, y, it) } 
        }
    }

fun main(args: Array<String>) {
    println(pythagoreanTriples(20))
}
```


{{out}}

```txt

[(3, 4, 5), (5, 12, 13), (6, 8, 10), (8, 15, 17), (9, 12, 15), (12, 16, 20)]

```



## Lasso

Lasso uses query expressions for list manipulation.

```lasso
#!/usr/bin/lasso9

local(n = 20)
local(triples = 
  with x in generateSeries(1, #n),
       y in generateSeries(#x, #n),
       z in generateSeries(#y, #n)
    where #x*#x + #y*#y == #z*#z
  select (:#x, #y, #z)
)
#triples->join('\n')
```

Output:

```lasso
staticarray(3, 4, 5)
staticarray(5, 12, 13)
staticarray(6, 8, 10)
staticarray(8, 15, 17)
staticarray(9, 12, 15)
staticarray(12, 16, 20)
```



## Lua

Lua doesn't have list comprehensions built in, but they can be constructed from chained coroutines:


```lua

LC={}
LC.__index = LC

function LC:new(o)
  o = o or {}
  setmetatable(o, self)
  return o
end

function LC:add_iter(func)
  local prev_iter = self.iter
  self.iter = coroutine.wrap(
    (prev_iter == nil) and (function() func{} end)
    or (function() for arg in prev_iter do func(arg) end end))
  return self
end

function maybe_call(maybe_func, arg)
  if type(maybe_func) == "function" then return maybe_func(arg) end
  return maybe_func
end

function LC:range(key, first, last)
  return self:add_iter(function(arg)
    for value=maybe_call(first, arg), maybe_call(last, arg) do
      arg[key] = value
      coroutine.yield(arg)
    end
  end)
end

function LC:where(pred)
  return self:add_iter(function(arg) if pred(arg) then coroutine.yield(arg) end end)
end

```


We can then define a function to compute Pythagorean triples as follows:


```lua

function get(key)
  return (function(arg) return arg[key] end)
end

function is_pythagorean(arg)
  return (arg.x^2 + arg.y^2 == arg.z^2)
end

function list_pythagorean_triples(n)
  return LC:new():range("x",1,n):range("y",1,get("x")):range("z", get("y"), n):where(is_pythagorean).iter
end

for arg in list_pythagorean_triples(100) do
  print(arg.x, arg.y, arg.z)
end

```



## Mathematica


```mathematica
Select[Tuples[Range[100], 3], #1[[1]]^2 + #1[[2]]^2 == #1[[3]]^2 &]
```



```mathematica
Pick[#, (#^2).{1, 1, -1}, 0] &@Tuples[Range[100], 3]
```


=={{header|MATLAB}} / {{header|Octave}}==

In Matlab/Octave, one does not think much about lists rather than vectors and matrices. Probably, the find() operation comes closes to the task

```Matlab
N = 20
[a,b] = meshgrid(1:N, 1:N);
c = sqrt(a.^2 + b.^2); 
[x,y] = find(c == fix(c));
disp([x, y, sqrt(x.^2 + y.^2)])
```


{{out}}


```txt
    4    3    5
    3    4    5
   12    5   13
    8    6   10
    6    8   10
   15    8   17
   12    9   15
    5   12   13
    9   12   15
   16   12   20
    8   15   17
   20   15   25
   12   16   20
   15   20   25

```



## Mercury


''Solutions'' behaves like list comprehension since compound goals resemble set-builder notation.


```mercury

:- module pythtrip.
:- interface.
:- import_module io.
:- import_module int.

:- type triple ---> triple(int, int, int).

:- pred pythTrip(int::in,triple::out) is nondet.
:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module list.
:- import_module solutions.
:- import_module math.

pythTrip(Limit,triple(X,Y,Z)) :-
    nondet_int_in_range(1,Limit,X),
    nondet_int_in_range(X,Limit,Y),
    nondet_int_in_range(Y,Limit,Z),
    pow(Z,2) = pow(X,2) + pow(Y,2).

main(!IO) :-
     solutions((pred(Triple::out) is nondet :- pythTrip(20,Triple)),Result),
     write(Result,!IO).

```



## Nemerle

Demonstrating a list comprehension and an iterator.  List comprehension adapted from Haskell example, iterator adapted from C# example.

```Nemerle
using System;
using System.Console;
using System.Collections.Generic;

module Program
{

    PythTriples(n : int) : list[int * int * int]
    {
        $[ (x, y, z) | x in [1..n], y in [x..n], z in [y..n], ((x**2) + (y**2)) == (z**2) ]
    }

    GetPythTriples(n : int) : IEnumerable[int * int * int]
    {
        foreach (x in [1..n])
        {
            foreach (y in [x..n])
            {
                foreach (z in [y..n])
                {
                    when (((x**2) + (y**2)) == (z**2))
                    {
                        yield (x, y, z)
                    }
                }
            }
        }
    }

    Main() : void
    {
        WriteLine("Pythagorean triples up to x = 20: {0}", PythTriples(20));

        foreach (triple in GetPythTriples(20))
        {
            Write(triple)
        }
    }
}
```



## Nim

There are no list comprehensions in Nim, but thanks to the strong metaprogramming capabilities we can implement our own:

```nim
import macros

type ListComprehension = object
var lc*: ListComprehension

macro `[]`*(lc: ListComprehension, x, t): expr =
  expectLen(x, 3)
  expectKind(x, nnkInfix)
  expectKind(x[0], nnkIdent)
  assert($x[0].ident == "|")

  result = newCall(
    newDotExpr(
      newIdentNode("result"),
      newIdentNode("add")),
    x[1])

  for i in countdown(x[2].len-1, 0):
    let y = x[2][i]
    expectKind(y, nnkInfix)
    expectMinLen(y, 1)
    if y[0].kind == nnkIdent and $y[0].ident == "<-":
      expectLen(y, 3)
      result = newNimNode(nnkForStmt).add(y[1], y[2], result)
    else:
      result = newIfStmt((y, result))

  result = newNimNode(nnkCall).add(
    newNimNode(nnkPar).add(
      newNimNode(nnkLambda).add(
        newEmptyNode(),
        newEmptyNode(),
        newEmptyNode(),
        newNimNode(nnkFormalParams).add(
          newNimNode(nnkBracketExpr).add(
            newIdentNode("seq"),
            t)),
        newEmptyNode(),
        newEmptyNode(),
        newStmtList(
          newAssignment(
            newIdentNode("result"),
            newNimNode(nnkPrefix).add(
              newIdentNode("@"),
              newNimNode(nnkBracket))),
          result))))

const n = 20
echo lc[(x,y,z) | (x <- 1..n, y <- x..n, z <- y..n, x*x + y*y == z*z),
        tuple[a,b,c: int]]
```

Output:

```txt
@[(a: 3, b: 4, c: 5), (a: 5, b: 12, c: 13), (a: 6, b: 8, c: 10), (a: 8, b: 15, c: 17), (a: 9, b: 12, c: 15), (a: 12, b: 16, c: 20)]
```



## OCaml


[http://batteries.forge.ocamlcore.org/ OCaml Batteries Included] has uniform comprehension syntax for lists, arrays, enumerations (like streams), lazy lists (like lists but evaluated on-demand), sets, hashtables, etc.

Comprehension are of the form
<code>[? expression | x <- enumeration ; condition; condition ; ...]</code>

For instance, 

```ocaml
#   [? 2 * x | x <- 0 -- max_int ; x * x > 3];;
- : int Enum.t = <abstr>
```

or, to compute a list,

```ocaml
#   [? List: 2 * x | x <- 0 -- 100 ; x * x > 3];;
- : int list = [2; 4; 6; 8; 10]
```

or, to compute a set,

```ocaml
#   [? PSet: 2 * x | x <- 0 -- 100 ; x * x > 3];;
- : int PSet.t = <abstr>
```


etc..

A standard OCaml distribution also includes a number of camlp4 extensions, including one that provides list comprehensions:

```ocaml
# #camlp4o;;
# #require "camlp4.listcomprehension";;
/home/user//.opam/4.06.1+trunk+flambda/lib/ocaml/camlp4/Camlp4Parsers/Camlp4ListComprehension.cmo: loaded
# [ x * 2 | x <- [1;2;3;4] ];;
- : int list = [2; 4; 6; 8]
# [ x * 2 | x <- [1;2;3;4]; x > 2 ];;
- : int list = [6; 8]
```



## Oz

Oz does not have list comprehension.

However, there is a list comprehension package available [http://oz-code.googlecode.com/files/ListComprehension.zip here]. It uses the <em>unofficial and deprecated</em> macro system. Usage example:


```oz
functor
import
   LazyList
   Application
   System
define

   fun {Pyth N}
      <<list [X Y Z] with
	 X <- {List.number 1 N 1}
	 Y <- {List.number X N 1}
	 Z <- {List.number Y N 1}
	 where X*X + Y*Y == Z*Z
      >>
   end

   {ForAll {Pyth 20} System.show}

   {Application.exit 0}
end
```



## PARI/GP

GP 2.6.0 added support for a new comprehension syntax:
{{works with|PARI/GP|2.6.0 and above}}

```parigp
f(n)=[v|v<-vector(n^3,i,vector(3,j,i\n^(j-1)%n)),norml2(v)==2*v[3]^2]
```


Older versions of GP can emulate this through <code>select</code>:
{{PARI/GP select}}

```parigp
f(n)=select(v->norml2(v)==2*v[3]^2,vector(n^3,i,vector(3,j,i\n^(j-1)%n)))
```


Version 2.4.2 (obsolete, but widespread on Windows systems) requires inversion:
{{works with|PARI/GP|2.4.2}}

```parigp
f(n)=select(vector(n^3,i,vector(3,j,i\n^(j-1)%n)),v->norml2(v)==2*v[3]^2)
```



## Perl


Perl 5 does not have built-in list comprehension syntax. The closest approach are the list <code>map</code> and <code>grep</code> (elsewhere often known as filter) operators:


```perl
sub triples ($) {
  my ($n) = @_;
  map { my $x = $_; map { my $y = $_; map { [$x, $y, $_] } grep { $x**2 + $y**2 == $_**2 } 1..$n } 1..$n } 1..$n;
}
```


<code>map</code> binds <code>$_</code> to each element of the input list and collects the results from the block. <code>grep</code> returns every element of the input list for which the block returns true. The <code>..</code> operator generates a list of numbers in a specific range.


```perl
for my $t (triples(10)) {
  print "@$t\n";
}
```



## Perl 6

Perl 6 has single-dimensional list comprehensions that fall out naturally from nested modifiers; multidimensional comprehensions are also supported via the cross operator; however, Perl 6 does not (yet) support multi-dimensional list comprehensions with dependencies between the lists, so the most straightforward way is currently:

```perl6
my $n = 20;
gather for 1..$n -> $x {
         for $x..$n -> $y {
           for $y..$n -> $z {
             take $x,$y,$z if $x*$x + $y*$y == $z*$z;
           }
         }
       }
```


Note that <tt>gather</tt>/<tt>take</tt> is the primitive in Perl 6 corresponding to generators or coroutines in other languages.  It is not, however, tied to function call syntax in Perl 6.  We can get away with that because lists are lazy, and the demand for more of the list is implicit; it does not need to be driven by function calls.


## Phix

Phix does not have builtin support for list comprehensions.

However, consider the fact that the compiler essentially converts an expression such as s[i] into calls 
to the low-level back end (machine code) routines :%opRepe and :%opSubse depending on context (although 
somtimes it will inline things and sometimes for better performance it will use opRepe1/opRepe1ip/opRepe1is and 
opSubse1/opSubse1i/opSubse1is/opSubse1ip variants, but that's just detail). It also maps ? to hll print().

Thinking laterally, Phix also does not have any special syntax for dictionaries, instead they are supported
via an autoinclude with the following standard hll routines:

```Phix
global function new_dict(integer pool_only=0)
global procedure destroy_dict(integer tid, integer justclear=0)
global procedure setd(object key, object data, integer tid=1)
global function getd(object key, integer tid=1)
global procedure destroy_dict(integer tid, integer justclear=0)
global function getd_index(object key, integer tid=1)
global function getd_by_index(integer node, integer tid=1)
global procedure deld(object key, integer tid=1)
global procedure traverse_dict(integer rid, object user_data=0, integer tid=1)
global function dict_size(integer tid=1)
```

Clearly it would be relatively trivial for the compiler, just like it does with s[i], to map some other new 
dictionary syntax to calls to these routines (not that it would ever use the default tid of 1, and admittedly 
traverse_dict might prove a bit trickier than the rest). Since Phix is open source, needs no other tools, and 
compiles itself in 10s, that is not as unreasonable for you (yes, you) to attempt as it might first sound.

With all that in mind, the following (which works just fine as it is) might be a first step to formal list comprehension support:

```Phix
--
-- demo\rosetta\List_comprehensions.exw
-- 
### ==============================

--
function list_comprehension(sequence s, integer rid, integer nargs, integer level=1, sequence args={})
sequence res = {}
    args &= 0
    for i=1 to length(s) do
        args[$] = s[i]
        if level<nargs then
            res &= list_comprehension(s,rid,nargs,level+1,args)
        else
            res &= call_func(rid,args)
        end if
    end for
    return res 
end function

function triangle(integer a, b, c)
    if a<b and a*a+b*b=c*c then
        return {{a,b,c}}
    end if
    return {}
end function

?list_comprehension(tagset(20),routine_id("triangle"),3)
```

{{out}}

```txt

{{3,4,5},{5,12,13},{6,8,10},{8,15,17},{9,12,15},{12,16,20}}

```



## PicoLisp

PicoLisp doesn't have list comprehensions.
We might use a generator function, pipe, coroutine or pilog predicate.

### Using a generator function


```PicoLisp
(de pythag (N)
   (job '((X . 1) (Y . 1) (Z . 0))
      (loop
         (when (> (inc 'Z) N)
            (when (> (inc 'Y) N)
               (setq Y (inc 'X)) )
            (setq Z Y) )
         (T (> X N))
         (T (= (+ (* X X) (* Y Y)) (* Z Z))
            (list X Y Z) ) ) ) )

(while (pythag 20)
   (println @) )
```


### Using a pipe


```PicoLisp
(pipe
   (for X 20
      (for Y (range X 20)
         (for Z (range Y 20)
            (when (= (+ (* X X) (* Y Y)) (* Z Z))
               (pr (list X Y Z)) ) ) ) )
   (while (rd)
      (println @) ) )
```


### Using a coroutine

Coroutines are available only in the 64-bit version.

```PicoLisp
(de pythag (N)
   (co 'pythag
      (for X N
         (for Y (range X N)
            (for Z (range Y N)
               (when (= (+ (* X X) (* Y Y)) (* Z Z))
                  (yield (list X Y Z)) ) ) ) ) ) )

(while (pythag 20)
   (println @) )
```


Output in all three cases:

```txt
(3 4 5)
(5 12 13)
(6 8 10)
(8 15 17)
(9 12 15)
(12 16 20)
```


### Using Pilog

{{works with|PicoLisp|3.0.9.7}}

```PicoLisp
(be pythag (@N @X @Y @Z)
   (for @X @N)
   (for @Y @X @N)
   (for @Z @Y @N)
   (^ @ 
      (let (X (-> @X)  Y (-> @Y)  Z (-> @Z))
         (= (+ (* X X) (* Y Y)) (* Z Z)) ) ) )
```

Test:

```PicoLisp
: (? (pythag 20 @X @Y @Z))
 @X=3 @Y=4 @Z=5
 @X=5 @Y=12 @Z=13
 @X=6 @Y=8 @Z=10
 @X=8 @Y=15 @Z=17
 @X=9 @Y=12 @Z=15
 @X=12 @Y=16 @Z=20
-> NIL
```



## Prolog

SWI-Prolog does not have list comprehension, however we can simulate it.


```Prolog
% We need operators
:- op(700, xfx, <-).
:- op(450, xfx, ..).
:- op(1100, yfx, &).

% use for explicit list usage
my_bind(V, [H|_]) :- V = H.
my_bind(V, [_|T]) :- my_bind(V, T).

% we need to define the intervals of numbers
Vs <- M..N :-
        integer(M),
	integer(N),
	M =< N,
	between(M, N, Vs).

% for explicit list comprehension like Vs <- [1,2,3]
Vs <- Xs :-
	is_list(Xs),
	my_bind(Vs, Xs).

% finally we define list comprehension
% prototype is Vs <- {Var, Dec, Pred} where
% Var is the list of variables to output
% Dec is the list of intervals of the variables
% Pred is the list of predicates
Vs <- {Var & Dec & Pred} :-
	findall(Var,  maplist(call, [Dec, Pred]), Vs).

% for list comprehension without Pred
Vs <- {Var & Dec} :-
	findall(Var, maplist(call, [Dec]), Vs).

```

Examples of use :<BR>
List of Pythagorean triples :

```txt
 ?- V <- {X, Y, Z & X <- 1..20, Y <- X..20, Z <- Y..20 & X*X+Y*Y =:= Z*Z}.
V = [ (3,4,5), (5,12,13), (6,8,10), (8,15,17), (9,12,15), (12,16,20)] ;
false.

```

List of double of x, where x^2 is greater than 50 :

```txt
 ?- V <- {Y & X <- 1..20 & X*X > 50, Y is 2 * X}.
V = [16,18,20,22,24,26,28,30,32,34,36,38,40] ;
false.

```



```txt
?- Vs <- {X, Y & X <- [1,2,3], Y <- [1,2,3, 4] & X = Y}.
Vs = [ (1, 1), (2, 2), (3, 3)].

```



```txt
?- Vs <- {X, Y & X <- [1,2,3], Y <- 1..5 & X = Y}.
Vs = [ (1, 1), (2, 2), (3, 3)].

```



```txt
?- Vs <- {X, Y & X <- [1,2], Y <- [4,5] }.
Vs = [ (1, 4), (1, 5), (2, 4), (2, 5)].

```



## Python

List comprehension:


```python
[(x,y,z) for x in xrange(1,n+1) for y in xrange(x,n+1) for z in xrange(y,n+1) if x**2 + y**2 == z**2]
```


A Python generator expression (note the outer round brackets), returns an iterator over the same result rather than an explicit list:


```python
((x,y,z) for x in xrange(1,n+1) for y in xrange(x,n+1) for z in xrange(y,n+1) if x**2 + y**2 == z**2)
```


A slower but more readable version: 


```python
[(x, y, z) for (x, y, z) in itertools.product(xrange(1,n+1),repeat=3) if x**2 + y**2 == z**2 and x <= y <= z]
```


Or as an iterator:


```python
((x, y, z) for (x, y, z) in itertools.product(xrange(1,n+1),repeat=3) if x**2 + y**2 == z**2 and x <= y <= z)
```


Alternatively we shorten the initial list comprehension but this time without compromising on speed. First we introduce a generator which generates all triplets:


```python
def triplets(n):
    for x in xrange(1, n + 1):
        for y in xrange(x, n + 1):
            for z in xrange(y, n + 1):
                yield x, y, z
```


Apply this to our list comprehension gives:


```python
[(x, y, z) for (x, y, z) in triplets(n) if x**2 + y**2 == z**2]
```


Or as an iterator:


```python
((x, y, z) for (x, y, z) in triplets(n) if x**2 + y**2 == z**2)
```


More generally, the list comprehension syntax can be understood as a concise syntactic sugaring of a use of the list monad, in which non-matches are returned as empty lists, matches are wrapped as single-item lists, and concatenation flattens the output, eliminating the empty lists.

The monadic 'bind' operator for lists is concatMap, traditionally used with its first two arguments flipped. The following three formulations of a '''pts''' (pythagorean triangles) function are equivalent:


```python
from functools import (reduce)
from operator import (add)


# pts :: Int -> [(Int, Int, Int)]
def pts(n):
    m = 1 + n
    return [(x, y, z) for x in xrange(1, m)
            for y in xrange(x, m)
            for z in xrange(y, m) if x**2 + y**2 == z**2]


# pts2 :: Int -> [(Int, Int, Int)]
def pts2(n):
    m = 1 + n
    return bindList(
        xrange(1, m)
    )(lambda x: bindList(
        xrange(x, m)
    )(lambda y: bindList(
        xrange(y, m)
    )(lambda z: [(x, y, z)] if x**2 + y**2 == z**2 else [])))


# pts3 :: Int -> [(Int, Int, Int)]
def pts3(n):
    m = 1 + n
    return concatMap(
        lambda x: concatMap(
            lambda y: concatMap(
                lambda z: [(x, y, z)] if x**2 + y**2 == z**2 else []
            )(xrange(y, m))
        )(xrange(x, m))
    )(xrange(1, m))


# GENERIC ---------------------------------------------------------

# concatMap :: (a -> [b]) -> [a] -> [b]
def concatMap(f):
    return lambda xs: (
        reduce(add, map(f, xs), [])
    )


# (flip concatMap)
# bindList :: [a] -> (a -> [b])  -> [b]
def bindList(xs):
    return lambda f: (
        reduce(add, map(f, xs), [])
    )


def main():
    for f in [pts, pts2, pts3]:
        print (f(20))


main()
```

{{Out}}

```txt
[(3, 4, 5), (5, 12, 13), (6, 8, 10), (8, 15, 17), (9, 12, 15), (12, 16, 20)]
[(3, 4, 5), (5, 12, 13), (6, 8, 10), (8, 15, 17), (9, 12, 15), (12, 16, 20)]
[(3, 4, 5), (5, 12, 13), (6, 8, 10), (8, 15, 17), (9, 12, 15), (12, 16, 20)]
```



## R

R has inherent list comprehension: 

```R

x = (0:10)
> x^2
 [1]   0   1   4   9  16  25  36  49  64  81 100
> Reduce(function(y,z){return (y+z)},x)
[1] 55
> x[x[(0:length(x))] %% 2==0]
[1]  0  2  4  6  8 10

```


R's "data frame" functions can be used to achieve the same code clarity (at the cost of expanding the entire grid into memory before the filtering step)


```R

subset(expand.grid(x=1:n, y=1:n, z=1:n), x^2 + y^2 == z^2)

```



## Racket


```racket

#lang racket
(for*/list ([x (in-range 1 21)]
            [y (in-range x 21)]
            [z (in-range y 21)]
            #:when (= (+ (* x x) (* y y)) (* z z)))
  (list x y z))

```



## Rascal


```Rascal
 
public list[tuple[int, int, int]]  PythTriples(int n) = [<a, b, c> | a <- [1..n], b <- [1..n], c <- [1 .. n], a*a + b*b == c*c];

```



## REXX

There is no native comprehensive support for lists ''per se'',   except that

normal lists can be processed quite easily and without much effort.


### vertical list


```rexx
/*REXX program displays a vertical list of Pythagorean triples up to a specified number.*/
parse arg n .                                    /*obtain optional argument from the CL.*/
if n=='' | n==","  then n= 100                   /*Not specified?  Then use the default.*/
say  'Pythagorean triples  (a² + b² = c²,   c ≤'  n"):"     /*display the list's title. */
$=                                               /*assign a  null  to the triples list. */
            do     a=1   for n-2;  aa=a*a
              do   b=a+1  to n-1;  ab=aa + b*b
                do c=b+1  to n  ;  cc= c*c
                if ab<cc   then leave            /*Too small?   Then try the next  B.   */
                if ab==cc  then do;  $=$  '{'a","   ||   b','c"}";  leave;  end
                end   /*c*/
              end     /*b*/
            end       /*a*/
#= words($)
            do j=1  for #
            say  left('', 20)      word($, j)    /*display  a  member  of the list,     */
            end       /*j*/                      /* [↑]   list the members vertically.  */
say
say #  'members listed.'                         /*stick a fork in it,  we're all done. */
```

{{out|output|text=  when using the default input:}}
<pre style="height:45ex">
Pythagorean triples  (a² + b² = c²,   c ≤ 100):
                     {3,4,5}
                     {5,12,13}
                     {6,8,10}
                     {7,24,25}
                     {8,15,17}
                     {9,12,15}
                     {9,40,41}
                     {10,24,26}
                     {11,60,61}
                     {12,16,20}
                     {12,35,37}
                     {13,84,85}
                     {14,48,50}
                     {15,20,25}
                     {15,36,39}
                     {16,30,34}
                     {16,63,65}
                     {18,24,30}
                     {18,80,82}
                     {20,21,29}
                     {20,48,52}
                     {21,28,35}
                     {21,72,75}
                     {24,32,40}
                     {24,45,51}
                     {24,70,74}
                     {25,60,65}
                     {27,36,45}
                     {28,45,53}
                     {28,96,100}
                     {30,40,50}
                     {30,72,78}
                     {32,60,68}
                     {33,44,55}
                     {33,56,65}
                     {35,84,91}
                     {36,48,60}
                     {36,77,85}
                     {39,52,65}
                     {39,80,89}
                     {40,42,58}
                     {40,75,85}
                     {42,56,70}
                     {45,60,75}
                     {48,55,73}
                     {48,64,80}
                     {51,68,85}
                     {54,72,90}
                     {57,76,95}
                     {60,63,87}
                     {60,80,100}
                     {65,72,97}
52 members listed.

```



### horizontal list


```rexx
/*REXX program displays a vertical list of Pythagorean triples up to a specified number.*/
parse arg n .                                    /*get the optional argument from the CL*/
if n=='' | n==","  then n= 100                   /*Not specified?  Then use the default.*/
say  'Pythagorean triples  (a² + b² = c²,   c ≤'  n"):"     /*display the list's title. */
$=                                               /*assign a  null  to the triples list. */
            do     a=1   for n-2;  aa= a*a
              do   b=a+1  to n-1;  ab= aa + b*b
                do c=b+1  to n  ;  cc= c*c
                if ab<cc   then leave            /*Too small?   Then try the next  B.   */
                if ab==cc  then do;  $=$  '{'a","   ||   b','c"}";  leave;  end
                end   /*c*/
              end     /*b*/
            end       /*a*/                      /*stick a fork in it,  we're all done. */
#= words($)                                      /*number of members in the list.       */
say;        say  strip($)                        /*show the Pythagorean triples to term.*/
say;        say     #    'members listed.'       /*triples are listed in order of 1st #.*/
```

{{out|output|text=  when using the following input:   <tt> 35 </tt>}}

```txt

Pythagorean triples  (a² + b² = c²,   c ≤ 35):

{3,4,5} {5,12,13} {6,8,10} {7,24,25} {8,15,17} {9,12,15} {10,24,26} {12,16,20} {15,20,25} {16,30,34} {18,24,30} {20,21,29} {21,28,35}

13 members listed.

```



## Ring


```ring

for  x = 1 to 20 
     for  y = x to 20 
          for  z = y to 20
               if pow(x,2) + pow(y,2) = pow(z,2)
                  see "[" + x + "," + y + "," + z + "]" + nl ok
          next 
     next
next

```



## Ruby

Ruby has no special syntax for list comprehensions.

Enumerable#select comprises a list from one variable, like Perl grep() or Python filter(). Some lists need Array#map! to transform the variable.

* <tt>(1..100).select { |x| x % 3 == 0 }</tt> is the list of all x from 1 to 100 such that x is a multiple of 3.
* <tt>methods.select { |name| name.length <= 5 }</tt> is the list of all methods from <tt>self</tt> with names not longer than 5 characters.
* <tt>Dir["/bin/*"].select { |p| File.setuid? p }.map! { |p| File.basename p }</tt> is the list of all files in /bin with a setuid bit.

Ruby's object-oriented style enforces writing 1..100 before x. Think not of x in 1..100. Think of 1..100 giving x.


###  Ruby 1.9.2 

{{works with|Ruby|1.9.2}}


```ruby
n = 20
 
# select Pythagorean triplets
r = ((1..n).flat_map { |x|
       (x..n).flat_map { |y|
         (y..n).flat_map { |z|
           [[x, y, z]].keep_if { x * x + y * y == z * z }}}})

p r # print the array _r_
```


Output: <tt>[[3, 4, 5], [5, 12, 13], [6, 8, 10], [8, 15, 17], [9, 12, 15], [12, 16, 20]]</tt>

Ruby 1.9.2 introduces two new methods: Enumerable#flat_map joins all the arrays from the block. Array#keep_if is an alternative to Enumerable#select that modifies the original array. (We avoid Array#select! because it might not return the array.)

* The <tt><nowiki>[[x, y, z]].keep_if { ... }</nowiki></tt> returns either an array of one Pythagorean triplet, or an empty array.
* The inner <tt>(y..n).flat_map { ... }</tt> concatenates those arrays for all z given some x, y.
* The middle <tt>(x..n).flat_map { ... }</tt> concatenates the inner arrays for all y given some x.
* The outer <tt>(1..n).flat_map { ... }</tt> concatenates the middle arrays for all x.



Illustrating a way to avoid all loops (but no list comprehensions) :

```ruby
n = 20
p (1..n).to_a.combination(3).select{|a,b,c| a*a + b*b == c*c}

```



## Run BASIC


```runbasic
for  x = 1 to 20 
 for  y = x to 20 
  for  z = y to 20
   if x^2 + y^2 = z^2 then  print "[";x;",";y;",";z;"]"
  next z
 next y
next x
```
Output:

```txt
[3,4,5]
[5,12,13]
[6,8,10]
[8,15,17]
[9,12,15]
[12,16,20]
```



## Scala


```scala
def pythagoranTriangles(n: Int) = for {
  x <- 1 to 21
  y <- x to 21
  z <- y to 21
  if x * x + y * y == z * z
} yield (x, y, z)
```


which is a syntactic sugar for:


```scala
 def pythagoranTriangles(n: Int) = (1 to n) flatMap (x => 
  (x to n) flatMap (y => 
    (y to n) filter (z => x * x + y * y == z * z) map (z => 
      (x, y, z))))
```


Alas, the type of collection returned depends on the type of the collection
being comprehended. In the example above, we are comprehending a <code>Range</code>.
Since a <code>Range</code> of triangles doesn't make sense, it returns the
closest (supertype) collection for which it does, an <code>IndexedSeq</code>.

To get a <code>List</code> out of it, just pass a <code>List</code> to it:


```scala
def pythagoranTriangles(n: Int) = for {
  x <- List.range(1, n + 1)
  y <- x to 21
  z <- y to 21
  if x * x + y * y == z * z
} yield (x, y, z)
```


Sample:


```txt

scala> pythagoranTriangles(21)
res36: List[(Int, Int, Int)] = List((3,4,5), (5,12,13), (6,8,10), (8,15,17), (9,12,15), (12,16,20))

```



## Scheme

Scheme has no native list comprehensions, but SRFI-42 [http://srfi.schemers.org/srfi-42/srfi-42.html] provides them:


```scheme

(list-ec (:range x 1 21)
         (:range y x 21)
         (:range z y 21)
         (if (= (* z z) (+ (* x x) (* y y))))
         (list x y z))

```



```txt

((3 4 5) (5 12 13) (6 8 10) (8 15 17) (9 12 15) (12 16 20))

```



## Sidef

{{trans|Perl 6}}

```ruby
var n = 20
say gather {
    for x in (1 .. n) {
        for y in (x .. n) {
           for z in (y .. n) {
             take([x,y,z]) if (x*x + y*y == z*z)
           }
        }
    }
}
```

{{out}}

```txt

[[3, 4, 5], [5, 12, 13], [6, 8, 10], [8, 15, 17], [9, 12, 15], [12, 16, 20]]

```



## Smalltalk

{{works with|Pharo|1.3-13315}}

```smalltalk

| test |

test := [ :a :b :c | a*a+(b*b)=(c*c) ].

(1 to: 20) 
    combinations: 3 atATimeDo: [ :x | 
        (test valueWithArguments: x) 
            ifTrue: [ ':-)' logCr: x ] ].

"output on Transcript:
#(3 4 5)
#(5 12 13)
#(6 8 10)
#(8 15 17)
#(9 12 15)
#(12 16 20)"

```



## Stata


Stata does no have list comprehensions, but the Mata matrix language helps simplify this task.


```stata
function grid(n,p) {
	return(colshape(J(1,p,1::n),1),J(n,1,1::p))
}

n = 20
a = grid(n,n)
a = a,sqrt(a[.,1]:^2+a[.,2]:^2)
a[selectindex(floor(a[.,3]):==a[.,3] :& a[.,3]:<=n),]
```


'''Output'''


```txt
         1    2    3
     +----------------+
   1 |   3    4    5  |
   2 |   4    3    5  |
   3 |   5   12   13  |
   4 |   6    8   10  |
   5 |   8    6   10  |
   6 |   8   15   17  |
   7 |   9   12   15  |
   8 |  12    5   13  |
   9 |  12    9   15  |
  10 |  12   16   20  |
  11 |  15    8   17  |
  12 |  16   12   20  |
     +----------------+
```



## SuperCollider


```supercollider

var pyth = { |n|
  all {: [x,y,z],
    x <- (1..n),
    y <- (x..n),
    z <- (y..n),
    (x**2) + (y**2) == (z**2)
    }
};

pyth.(20) // example call
```

returns

```supercollider
[ [ 3, 4, 5 ], [ 5, 12, 13 ], [ 6, 8, 10 ], [ 8, 15, 17 ], [ 9, 12, 15 ], [ 12, 16, 20 ] ]
```



## Tcl

Tcl does not have list comprehensions built-in to the language, but they can be constructed.

```tcl
package require Tcl 8.5

# from http://wiki.tcl.tk/12574 
proc lcomp {expression args} {
    # Check the number of arguments.
    if {[llength $args] < 2} {
        error "wrong # args: should be \"lcomp expression var1 list1\
            ?... varN listN? ?condition?\""
    }

    # Extract condition from $args, or use default.
    if {[llength $args] % 2 == 1} {
        set condition [lindex $args end]
        set args [lrange $args 0 end-1]
    } else {
        set condition 1
    }

    # Collect all var/list pairs and store in reverse order.
    set varlst [list]
    foreach {var lst} $args {
        set varlst [concat [list $var] [list $lst] $varlst]
    }

    # Actual command to be executed, repeatedly.
    set script {lappend result [subst $expression]}

    # If necessary, make $script conditional.
    if {$condition ne "1"} {
        set script [list if $condition $script]
    }

    # Apply layers of foreach constructs around $script.
    foreach {var lst} $varlst {
        set script [list foreach $var $lst $script]
    }

    # Do it!
    set result [list]
    {*}$script ;# Change to "eval $script" if using Tcl 8.4 or older.
    return $result
}

set range {1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20}
puts [lcomp {$x $y $z} x $range y $range z $range {$x < $y && $x**2 + $y**2 == $z**2}]
```


```txt
{3 4 5} {5 12 13} {6 8 10} {8 15 17} {9 12 15} {12 16 20}
```


=={{header|TI-89 BASIC}}==

TI-89 BASIC does not have a true list comprehension, but it has the seq() operator which can be used for some similar purposes.


```ti89b
{1, 2, 3, 4} → a
seq(a[i]^2, i, 1, dim(a))
```


produces {1, 4, 9, 16}. When the input is simply a numeric range, an input list is not needed; this produces the same result:


```ti89b
seq(x^2, x, 1, 4)
```



## Visual Basic .NET


{{trans|C#}}


```vbnet
Module ListComp
    Sub Main()
        Dim ts = From a In Enumerable.Range(1, 20) _
                 From b In Enumerable.Range(a, 21 - a) _
                 From c In Enumerable.Range(b, 21 - b) _
                 Where a * a + b * b = c * c _
                 Select New With { a, b, c }
        
        For Each t In ts
            System.Console.WriteLine("{0}, {1}, {2}", t.a, t.b, t.c)
        Next
    End Sub
End Module
```


Output:

```txt

3, 4, 5
5, 12, 13
6, 8, 10
8, 15, 17
9, 12, 15
12, 16, 20

```



## Visual Prolog


VP7 has explicit list comprehension syntax.


```visualProlog

implement main
    open core, std

domains
    pythtrip = pt(integer, integer, integer).

class predicates
    pythTrips : (integer) -> pythtrip nondeterm (i).

clauses
    pythTrips(Limit) = pt(X,Y,Z) :-
        X = fromTo(1,Limit),
        Y = fromTo(X,Limit),
        Z = fromTo(Y,Limit),
        Z^2 = X^2 + Y^2.

    run():-
        console::init(),
        Triples = [ X || X = pythTrips(20) ],
        console::write(Triples),
        Junk = console::readLine(),
        succeed(). 
end implement main

goal
    mainExe::run(main::run).

```



## Wrapl


```wrapl
ALL WITH x <- 1:to(n), y <- x:to(n), z <- y:to(n) DO (x^2 + y^2 = z^2) & [x, y, z];
```



## zkl


```zkl
var n=20;
[[(x,y,z); [1..n]; {[x..n]}; {[y..n]},{ x*x + y*y == z*z }; _]]
//-->L(L(3,4,5),L(5,12,13),L(6,8,10),L(8,15,17),L(9,12,15),L(12,16,20))
```

Lazy:

```zkl
var n=20;
lp:=[& (x,y,z);  // three variables, [& means lazy/iterator
    [1..n];      // x: a range
    {[x..n]};    // y: another range, fcn(x) is implicit (via "{")
    {[y..n]},    // z with a filter/guard, expands to fcn(x,y){[y..n]}
       { x*x + y*y == z*z }; // the filter, fcn(x,y,z)
    { T(x,y,z) } // the result, could also be written as fcn(x,y,z){ T(x,y,z) }
                // or just T (read only list) as T(x,y,z) creates a new list
               // with values x,y,z, or just _ (which means return arglist)
]];
lp.walk(2) //-->L(L(3,4,5),L(5,12,13))
```



{{omit from|Axe}}
{{omit from|Maxima}}
