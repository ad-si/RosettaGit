+++
title = "Church Numerals"
description = ""
date = 2019-10-22T01:35:38Z
aliases = []
[extra]
id = 21963
[taxonomies]
categories = []
tags = []
+++

{{task}}

;Task:
In [[wp:Church_encoding#Church_numerals|the Church encoding of natural numbers]], the number N is encoded by a function that applies its first argument N times to its second argument.

* '''Church zero''' always returns the identity function, regardless of its first argument. In other words, the first argument is not applied to the second argument at all.
* '''Church one''' applies its first argument f just once to its second argument x, yielding '''f(x)'''
* '''Church two''' applies its first argument f twice to its second argument x, yielding '''f(f(x))'''
* and each successive Church numeral applies its first argument one additional time to its second argument, '''f(f(f(x)))''', '''f(f(f(f(x))))''' ...  The Church numeral 4, for example, returns a quadruple composition of the function supplied as its first argument.


Arithmetic operations on natural numbers can be similarly [[wp:Church_encoding#Calculation_with_Church_numerals|represented as functions on Church numerals]].

In your language define:

* Church Zero,
* a Church successor function (a function on a Church numeral which returns the next Church numeral in the series),
* functions for Addition, Multiplication and Exponentiation over Church numerals,
* a function to convert integers to corresponding Church numerals,
* and a function to convert Church numerals to corresponding integers.


You should:

* Derive Church numerals three and four in terms of Church zero and a Church successor function.
* use Church numeral arithmetic to obtain the the sum and the product of Church 3 and Church 4,
* similarly obtain 4^3 and 3^4 in terms of Church numerals, using a Church numeral exponentiation function,
* convert each result back to an integer, and return it or print it to the console.





## AppleScript


Implementing '''churchFromInt''' as a fold seems to protect Applescript from overflowing its (famously shallow) stack with even quite low Church numerals.


```applescript
on run
    set cThree to churchFromInt(3)
    set cFour to churchFromInt(4)
    
    map(intFromChurch, ¬
        {churchAdd(cThree, cFour), churchMult(cThree, cFour), ¬
            churchExp(cFour, cThree), churchExp(cThree, cFour)})
end run

-- churchZero :: (a -> a) -> a -> a
on churchZero(f, x)
    x
end churchZero

-- churchSucc :: ((a -> a) -> a -> a) -> (a -> a) -> a -> a
on churchSucc(n)
    script
        on |λ|(f)
            script
                property mf : mReturn(f)
                on |λ|(x)
                    mf's |λ|(mReturn(n)'s |λ|(mf)'s |λ|(x))
                end |λ|
            end script
        end |λ|
    end script
end churchSucc

-- churchFromInt(n) :: Int -> (b -> b) -> b -> b
on churchFromInt(n)
    script
        on |λ|(f)
            foldr(my compose, my |id|, replicate(n, f))
        end |λ|
    end script
end churchFromInt

-- intFromChurch :: ((Int -> Int) -> Int -> Int) -> Int
on intFromChurch(cn)
    mReturn(cn)'s |λ|(my succ)'s |λ|(0)
end intFromChurch

on churchAdd(m, n)
    script
        on |λ|(f)
            script
                property mf : mReturn(m)
                property nf : mReturn(n)
                on |λ|(x)
                    nf's |λ|(f)'s |λ|(mf's |λ|(f)'s |λ|(x))
                end |λ|
            end script
        end |λ|
    end script
end churchAdd

on churchMult(m, n)
    script
        on |λ|(f)
            script
                property mf : mReturn(m)
                property nf : mReturn(n)
                on |λ|(x)
                    mf's |λ|(nf's |λ|(f))'s |λ|(x)
                end |λ|
            end script
        end |λ|
    end script
end churchMult

on churchExp(m, n)
    n's |λ|(m)
end churchExp


-- GENERIC -----------------------------------------------------------

-- compose (<<<) :: (b -> c) -> (a -> b) -> a -> c
on compose(f, g)
    script
        property mf : mReturn(f)
        property mg : mReturn(g)
        on |λ|(x)
            mf's |λ|(mg's |λ|(x))
        end |λ|
    end script
end compose

-- id :: a -> a
on |id|(x)
    x
end |id|

-- foldr :: (a -> b -> b) -> b -> [a] -> b
on foldr(f, startValue, xs)
    tell mReturn(f)
        set v to startValue
        set lng to length of xs
        repeat with i from lng to 1 by -1
            set v to |λ|(item i of xs, v, i, xs)
        end repeat
        return v
    end tell
end foldr

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
-- mReturn :: First-class m => (a -> b) -> m (a -> b)
on mReturn(f)
    if class of f is script then
        f
    else
        script
            property |λ| : f
        end script
    end if
end mReturn

-- Egyptian multiplication - progressively doubling a list, appending
-- stages of doubling to an accumulator where needed for binary 
-- assembly of a target length
-- replicate :: Int -> a -> [a]
on replicate(n, a)
    set out to {}
    if n < 1 then return out
    set dbl to {a}
    
    repeat while (n > 1)
        if (n mod 2) > 0 then set out to out & dbl
        set n to (n div 2)
        set dbl to (dbl & dbl)
    end repeat
    return out & dbl
end replicate

-- succ :: Int -> Int
on succ(x)
    1 + x
end succ
```

{{Out}}

```txt
{7, 12, 64, 81}
```



## C sharp


```csharp
using System;

public delegate Numeral Numeral(Numeral f);

public static class ChurchNumeral
{
    public static readonly Numeral Zero = f => x => x;

    public static Numeral Successor(this Numeral n) => f => x => f(n(f)(x));
    public static Numeral Add(this Numeral m, Numeral n) => f => x => m(f)(n(f)(x));
    public static Numeral Multiply(this Numeral m, Numeral n) => f => m(n(f));
    public static Numeral Pow(this Numeral m, Numeral n) => n(m);

    public static Numeral FromInt(int i) => i < 0 ? throw new ArgumentException("Negative church numeral.")
        : i == 0 ? Zero : Successor(FromInt(i - 1));

    public static int ToInt(this Numeral f) {
        int count = 0;
        f(x => { count++; return x; })(null);
        return count;
    }

    public static void Main2() {
        Numeral c3 = FromInt(3);
        Numeral c4 = c3.Successor();
        int sum = c3.Add(c4).ToInt();
        int product = c3.Multiply(c4).ToInt();
        int exp43 = c4.Pow(c3).ToInt();
        int exp34 = c3.Pow(c4).ToInt();
        Console.WriteLine($"{sum} {product} {exp43} {exp34}");
    }

}
```

{{out}}

```txt

7 12 64 81

```


=={{header|Fōrmulæ}}==

In [http://wiki.formulae.org/Church_numerals this] page you can see the solution of this task.

Fōrmulæ programs are not textual, visualization/edition of programs is done showing/manipulating structures but not text ([http://wiki.formulae.org/Editing_F%C5%8Drmul%C3%A6_expressions more info]). Moreover, there can be multiple visual representations of the same program. Even though it is possible to have textual representation &mdash;i.e. XML, JSON&mdash; they are intended for transportation effects more than visualization and edition.

The option to show Fōrmulæ programs and their results is showing images. Unfortunately images cannot be uploaded in Rosetta Code.


## F#


```fsharp
type INumeral =
    abstract Apply : ('a -> 'a) -> 'a -> 'a

let zero = {new INumeral with override __.Apply _ x = x}
let successor (n: INumeral) = {new INumeral with override __.Apply f x = f (n.Apply f x)}
let addition (m: INumeral) (n: INumeral) = {new INumeral with override __.Apply f x = m.Apply f (n.Apply f x)}
let multiplication (m: INumeral) (n: INumeral) = {new INumeral with override __.Apply f x = m.Apply (n.Apply f) x}
let exponential (m: INumeral) (n: INumeral) = {new INumeral with override __.Apply f x = n.Apply m.Apply f x}

let ntoi (n: INumeral) = n.Apply ((+) 1) 0
let iton i = List.fold (>>) id (List.replicate i successor) zero

let c3 = iton 3
let c4 = successor c3

[addition c3 c4;
multiplication c3 c4;
exponential c4 c3;
exponential c3 c4]
|> List.map ntoi
|> printfn "%A"

```

{{out}}

```txt
[7; 12; 64; 81]
```



## Go


```go
package main

import "fmt"

type any = interface{}

type fn func(any) any

type church func(fn) fn

func zero(f fn) fn {
    return func(x any) any {
        return x
    }
}

func (c church) succ() church {
    return func(f fn) fn {
        return func(x any) any {
            return f(c(f)(x))
        }
    }
}

func (c church) add(d church) church {
    return func(f fn) fn {
        return func(x any) any {
            return c(f)(d(f)(x))
        }
    }
}

func (c church) mul(d church) church {
    return func(f fn) fn {
        return func(x any) any {
            return c(d(f))(x)
        }
    }
}

func (c church) pow(d church) church {
    di := d.toInt()
    prod := c
    for i := 1; i < di; i++ {
        prod = prod.mul(c)
    }
    return prod
}

func (c church) toInt() int {
    return c(incr)(0).(int)
}

func intToChurch(i int) church {
    if i == 0 {
        return zero
    } else {
        return intToChurch(i - 1).succ()
    }
}

func incr(i any) any {
    return i.(int) + 1
}

func main() {
    z := church(zero)
    three := z.succ().succ().succ()
    four := three.succ()

    fmt.Println("three        ->", three.toInt())
    fmt.Println("four         ->", four.toInt())
    fmt.Println("three + four ->", three.add(four).toInt())
    fmt.Println("three * four ->", three.mul(four).toInt())
    fmt.Println("three ^ four ->", three.pow(four).toInt())
    fmt.Println("four ^ three ->", four.pow(three).toInt())
    fmt.Println("5 -> five    ->", intToChurch(5).toInt())
}
```


{{out}}

```txt

three        -> 3
four         -> 4
three + four -> 7
three * four -> 12
three ^ four -> 81
four ^ three -> 64
5 -> five    -> 5

```



## Haskell


```haskell
churchZero = const id
 
churchSucc = (<*>) (.)
 
churchAdd = (<*>) . fmap (.)
 
churchMult = (.)
 
churchExp = flip id
 
churchFromInt :: Int -> ((a -> a) -> a -> a)
churchFromInt 0 = churchZero
churchFromInt n = churchSucc $ churchFromInt (n - 1)
 
-- Or as a fold:
-- churchFromInt n = foldr (.) id . replicate n
 
-- Or as an iterate:
-- churchFromInt n = iterate churchSucc churchZero !! n
 
intFromChurch :: ((Int -> Int) -> Int -> Int) -> Int
intFromChurch cn = cn succ 0
 
-- TEST --------------------------------------------
[cThree, cFour] = churchFromInt <$> [3, 4]
 
main :: IO ()
main =
  print $
  intFromChurch <$>
  [ churchAdd cThree cFour
  , churchMult cThree cFour
  , churchExp cFour cThree
  , churchExp cThree cFour
  ]
```

{{Out}}

```txt
[7,12,64,81]
```



## Javascript


```javascript
(() => {
    'use strict';

    const churchZero = f => identity;

    const churchSucc = n => f => compose(f)(n(f));

    const churchAdd = m => n => f => compose(n(f))(m(f));

    const churchMult = m => n => f => n(m(f));

    const churchExp = m => n => n(m);

    const intFromChurch = n => n(succ)(0);

    const churchFromInt = n =>
        f => foldl(compose)(
            identity
        )(replicate(n)(f));

    // Or, by explicit recursion:
    const churchFromInt_ = x => {
        const go = i =>
            0 === i ? (
                churchZero
            ) : churchSucc(go(i - 1));
        return go(x);
    };


    // TEST -----------------------------------------------
    // main :: IO ()
    const main = () => {
        const [cThree, cFour] = map(churchFromInt)([3, 4]);

        return map(intFromChurch)([
            churchAdd(cThree)(cFour),
            churchMult(cThree)(cFour),
            churchExp(cFour)(cThree),
            churchExp(cThree)(cFour),
        ]);
    };


    // GENERIC FUNCTIONS ----------------------------------

    // compose (>>>) :: (a -> b) -> (b -> c) -> a -> c
    const compose = f => g => x => f(g(x));

    // foldl :: (a -> b -> a) -> a -> [b] -> a
    const foldl = f => a => xs =>
        xs.reduce(uncurry(f), a);

    // identity :: a -> a
    const identity = x => x;

    // map :: (a -> b) -> [a] -> [b]
    const map = f => xs =>
        (Array.isArray(xs) ? (
            xs
        ) : xs.split('')).map(f);

    // replicate :: Int -> a -> [a]
    const replicate = n => x =>
        Array.from({
            length: n
        }, () => x);

    // succ :: Enum a => a -> a
    const succ = x => 1 + x;

    // uncurry :: (a -> b -> c) -> ((a, b) -> c)
    const uncurry = f =>
        function() {
            const
                args = Array.from(arguments),
                a = 1 < args.length ? (
                    args
                ) : args[0]; // Tuple object.
            return f(a[0])(a[1]);
        };

    // MAIN ---
    console.log(JSON.stringify(main()));
})();
```

{{Out}}

```txt
[7,12,64,81]
```



## Julia

We could overload the Base operators, but that is not needed here.

```julia

id(x) = x -> x
zero() = x -> id(x)
add(m) = n -> (f -> (x -> n(f)(m(f)(x))))
mult(m) = n -> (f -> (x -> n(m(f))(x)))
exp(m) = n -> n(m)
succ(i::Int) = i + 1
succ(cn) = f -> (x -> f(cn(f)(x)))
church2int(cn) = cn(succ)(0)
int2church(n) = n < 0 ? throw("negative Church numeral") : (n == 0 ? zero() : succ(int2church(n - 1)))

function runtests()
    church3 = int2church(3)
    church4 = int2church(4)
    println("Church 3 + Church 4 = ", church2int(add(church3)(church4)))
    println("Church 3 * Church 4 = ", church2int(mult(church3)(church4)))
    println("Church 4 ^ Church 3 = ", church2int(exp(church4)(church3)))
    println("Church 3 ^ Church 4 = ", church2int(exp(church3)(church4)))
end

runtests()

```
 {{output}} 
```txt

Church 3 + Church 4 = 7
Church 3 * Church 4 = 12
Church 4 ^ Church 3 = 64
Church 3 ^ Church 4 = 81

```



## Lambdatalk



```Scheme

{def succ {lambda {:n :f :x} {:f {:n :f :x}}}}
{def add {lambda {:n :m :f :x} {{:n :f} {:m :f :x}}}} 
{def mul {lambda {:n :m :f} {:m {:n :f}}}}
{def power {lambda {:n :m} {:m :n}}}

{def church {lambda {:n} {{:n {+ {lambda {:x} {+ :x 1}}}} 0}}}

{def zero  {lambda {:f :x} :x}}
{def three {succ {succ {succ zero}}}}
{def four {succ {succ {succ {succ zero}}}}}

3+4 = {church {add   {three} {four}}} -> 7
3*4 = {church {mul   {three} {four}}} -> 12
3^4 = {church {power {three} {four}}} -> 81
4^3 = {church {power {four} {three}}} -> 64

```



## Lua


```lua

function churchZero()
  return function(x) return x end 
end 

function churchSucc(c)
  return function(f) 
    return function(x)
      return f(c(f)(x))
    end 
  end 
end 

function churchAdd(c, d)
  return function(f) 
    return function(x) 
      return c(f)(d(f)(x))
    end 
  end 
end 

function churchMul(c, d)
  return function(f) 
      return c(d(f))
  end 
end 

function churchExp(c, e) 
  return e(c)
end 

function numToChurch(n) 
  local ret = churchZero
  for i = 1, n do 
    ret = succ(ret) 
  end 
  return ret 
end 

function churchToNum(c)
  return c(function(x) return x + 1 end)(0) 
end 

three =  churchSucc(churchSucc(churchSucc(churchZero)))
four = churchSucc(churchSucc(churchSucc(churchSucc(churchZero))))

print("'three'\t=", churchToNum(three))
print("'four' \t=", churchToNum(four))
print("'three' * 'four' =", churchToNum(churchMul(three, four)))
print("'three' + 'four' =", churchToNum(churchAdd(three, four)))
print("'three' ^ 'four' =", churchToNum(churchExp(three, four)))
print("'four' ^ 'three' =", churchToNum(churchExp(four, three)))
```

{{out}}

```txt
'three' =   3
'four'  =   4
'three' * 'four' =  12
'three' + 'four' =  7
'three' ^ 'four' =  81
'four' ^ 'three' =  64
```



## Perl

{{trans|Perl 6}}

```perl
use 5.020;
use feature qw<signatures>;
no warnings qw<experimental::signatures>;

use constant zero  => sub ($f) {
                      sub ($x) { $x }};

use constant succ  => sub ($n) {
                      sub ($f) {
                      sub ($x) { $f->($n->($f)($x)) }}};

use constant add   => sub ($n) {
                      sub ($m) {
                      sub ($f) {
                      sub ($x) { $m->($f)($n->($f)($x)) }}}};

use constant mult  => sub ($n) {
                      sub ($m) {
                      sub ($f) {
                      sub ($x) { $m->($n->($f))($x) }}}};

use constant power => sub ($b) {
                      sub ($e) { $e->($b) }};

use constant countup   => sub ($i) { $i + 1 };
use constant countdown => sub ($i) { $i == 0 ? zero : succ->( __SUB__->($i - 1) ) };
use constant to_int    => sub ($f) { $f->(countup)->(0) };
use constant from_int  => sub ($x) { countdown->($x) };

use constant three => succ->(succ->(succ->(zero)));
use constant four  => from_int->(4);

say join ' ', map { to_int->($_) } (
    add  ->( three )->( four  ),
    mult ->( three )->( four  ),
    power->( four  )->( three ),
    power->( three )->( four  ),
);
```

{{out}}

```txt
7 12 64 81
```



## Perl 6


### Traditional subs and sigils

{{trans|Python}}

```perl6
constant $zero  = sub (Code $f) {
                  sub (     $x) { $x }}

constant $succ  = sub (Code $n) {
                  sub (Code $f) {
                  sub (     $x) { $f($n($f)($x)) }}}

constant $add   = sub (Code $n) {
                  sub (Code $m) {
                  sub (Code $f) {
                  sub (     $x) { $m($f)($n($f)($x)) }}}}

constant $mult  = sub (Code $n) {
                  sub (Code $m) {
                  sub (Code $f) {
                  sub (     $x) { $m($n($f))($x) }}}}

constant $power = sub (Code $b) {
                  sub (Code $e) { $e($b) }}

sub to_int (Code $f) {
    sub countup (Int $i) { $i + 1 }
    return $f(&countup).(0)
}

sub from_int (Int $x) {
    multi sub countdown (     0) { $zero }
    multi sub countdown (Int $i) { $succ( countdown($i - 1) ) }
    return countdown($x);
}

constant $three = $succ($succ($succ($zero)));
constant $four  = from_int(4);

say map &to_int,
    $add(   $three )( $four  ),
    $mult(  $three )( $four  ),
    $power( $four  )( $three ),
    $power( $three )( $four  ),
;
```


### Arrow subs without sigils

{{trans|Julia}}

```perl6
my \zero  = -> \f {                 -> \x { x               }}
my \succ  = -> \n {         -> \f { -> \x { f.(n.(f)(x))    }}}
my \add   = -> \n { -> \m { -> \f { -> \x { m.(f)(n.(f)(x)) }}}}
my \mult  = -> \n { -> \m { -> \f { -> \x { m.(n.(f))(x)    }}}}
my \power = -> \b {                 -> \e { e.(b)           }}

my \to_int   = -> \f { f.( -> \i { i + 1 } ).(0) }
my \from_int = -> \i { i == 0 ?? zero !! succ.( &?BLOCK(i - 1) ) }

my \three = succ.(succ.(succ.(zero)));
my \four  = from_int.(4);

say map -> \f { to_int.(f) },
    add.(   three )( four  ),
    mult.(  three )( four  ),
    power.( four  )( three ),
    power.( three )( four  ),
;
```

{{out}}

```txt
(7 12 64 81)
```



## Phix

{{trans|Go}}

```Phix
type church(object c)
-- eg {r_add,1,{a,b}}
    return sequence(c) and length(c)=3 
       and integer(c[1]) and integer(c[2]) 
       and sequence(c[3]) and length(c[3])=2
end type

function succ(church c)
-- eg {r_add,1,{a,b}} => {r_add,2,{a,b}}  aka  a+b -> a+b+b
    c[2] += 1
    return c
end function

-- three normal integer-handling routines...
function add(integer n, a, b)
    for i=1 to n do
        a += b
    end for
    return a
end function
constant r_add = routine_id("add")

function mul(integer n, a, b)
    for i=1 to n do
        a *= b
    end for
    return a
end function
constant r_mul = routine_id("mul")

function pow(integer n, a, b)
    for i=1 to n do
        a = power(a,b)
    end for
    return a
end function
constant r_pow = routine_id("pow")

-- ...and three church constructors to match
--    (no maths here, just pure static data)
function addch(church c, d)
    church res = {r_add,1,{c,d}}
    return res
end function

function mulch(church c, d)
    church res = {r_mul,1,{c,d}}
    return res
end function

function powch(church c, d)
    church res = {r_pow,1,{c,d}}
    return res
end function

function tointch(church c)
-- note this is where the bulk of any processing happens
    {integer rid, integer n, object x} = c
    for i=1 to length(x) do
        if church(x[i]) then x[i] = tointch(x[i]) end if
    end for
    return call_func(rid,n&x)
end function

constant church zero = {r_add,0,{0,1}}

function inttoch(integer i)
    if i=0 then
        return zero
    else
        return succ(inttoch(i-1))
    end if
end function

church three = succ(succ(succ(zero))),
       four = succ(three)
printf(1,"three        -> %d\n",tointch(three))
printf(1,"four         -> %d\n",tointch(four))
printf(1,"three + four -> %d\n",tointch(addch(three,four)))
printf(1,"three * four -> %d\n",tointch(mulch(three,four)))
printf(1,"three ^ four -> %d\n",tointch(powch(three,four)))
printf(1,"four ^ three -> %d\n",tointch(powch(four,three)))
printf(1,"5 -> five    -> %d\n",tointch(inttoch(5)))
```

{{out}}

```txt

three        -> 3
four         -> 4
three + four -> 7
three * four -> 12
three ^ four -> 81
four ^ three -> 64
5 -> five    -> 5

```



## Prolog

Prolog terms can be used to represent church numerals. 

```prolog
church_zero(z).

church_successor(Z, c(Z)).

church_add(z, Z, Z).
church_add(c(X), Y, c(Z)) :-
    church_add(X, Y, Z).

church_multiply(z, _, z).
church_multiply(c(X), Y, R) :-
    church_add(Y, S, R),
    church_multiply(X, Y, S).

% N ^ M
church_power(z, z, z).
church_power(N, c(z), N).
church_power(N, c(c(Z)), R) :-
    church_multiply(N, R1, R),
    church_power(N, c(Z), R1).

int_church(0, z).
int_church(I, c(Z)) :-
    int_church(Is, Z),
    succ(Is, I).

run :-
    int_church(3, Three),
    church_successor(Three, Four),
    church_add(Three, Four, Sum),
    church_multiply(Three, Four, Product),
    church_power(Four, Three, Power43),
    church_power(Three, Four, Power34),

    int_church(ISum, Sum),
    int_church(IProduct, Product),
    int_church(IPower43, Power43),
    int_church(IPower34, Power34),

    !,
    maplist(format('~w '), [ISum, IProduct, IPower43, IPower34]),
    nl.
```

{{out}}

```txt

7 12 81 64 

```



## Python

{{Works with|Python|3.7}}

```python
'''Church numerals'''

from itertools import repeat
from functools import reduce


# CHURCH ENCODINGS OF NUMERALS AND OPERATIONS -------------

def churchZero():
    '''The identity function.
       No applications of any supplied f
       to its argument.
    '''
    return lambda f: identity


def churchSucc(cn):
    '''The successor of a given
       Church numeral. One additional
       application of f. Equivalent to
       the arithmetic addition of one.
    '''
    return lambda f: compose(f)(cn(f))


def churchAdd(m):
    '''The arithmetic sum of two Church numerals.'''
    return lambda n: lambda f: compose(m(f))(n(f))


def churchMult(m):
    '''The arithmetic product of two Church numerals.'''
    return lambda n: compose(m)(n)


def churchExp(m):
    '''Exponentiation of Church numerals. m^n'''
    return lambda n: n(m)


def churchFromInt(n):
    '''The Church numeral equivalent of
       a given integer.
    '''
    return lambda f: (
        foldl
        (compose)
        (identity)
        (replicate(n)(f))
    )


# OR, alternatively:
def churchFromInt_(n):
    '''The Church numeral equivalent of a given
       integer, by explicit recursion.
    '''
    if 0 == n:
        return churchZero()
    else:
        return churchSucc(churchFromInt(n - 1))


def intFromChurch(cn):
    '''The integer equivalent of a
       given Church numeral.
    '''
    return cn(succ)(0)


# TEST ----------------------------------------------------
# main :: IO ()
def main():
    'Tests'

    cThree = churchFromInt(3)
    cFour = churchFromInt(4)

    print(list(map(intFromChurch, [
        churchAdd(cThree)(cFour),
        churchMult(cThree)(cFour),
        churchExp(cFour)(cThree),
        churchExp(cThree)(cFour),
    ])))


# GENERIC FUNCTIONS ---------------------------------------

# compose (flip (.)) :: (a -> b) -> (b -> c) -> a -> c
def compose(f):
    '''A left to right composition of two
       functions f and g'''
    return lambda g: lambda x: g(f(x))


# foldl :: (a -> b -> a) -> a -> [b] -> a
def foldl(f):
    '''Left to right reduction of a list,
       using the binary operator f, and
       starting with an initial value a.
    '''
    def go(acc, xs):
        return reduce(lambda a, x: f(a)(x), xs, acc)
    return lambda acc: lambda xs: go(acc, xs)


# identity :: a -> a
def identity(x):
    '''The identity function.'''
    return x


# replicate :: Int -> a -> [a]
def replicate(n):
    '''A list of length n in which every
       element has the value x.
    '''
    return lambda x: list(repeat(x, n))


# succ :: Enum a => a -> a
def succ(x):
    '''The successor of a value.
       For numeric types, (1 +).
    '''
    return 1 + x if isinstance(x, int) else (
        chr(1 + ord(x))
    )


if __name__ == '__main__':
    main()
```

{{Out}}

```txt
[7, 12, 64, 81]
```



## Racket



```racket
#lang racket
 
(define zero (λ (f) (λ (x) x)))
(define zero* (const identity)) ; zero renamed

(define one (λ (f) f))
(define one* identity) ; one renamed

(define succ (λ (n) (λ (f) (λ (x) (f ((n f) x))))))
(define succ* (λ (n) (λ (f) (λ (x) ((n f) (f x)))))) ; different impl

(define add (λ (n) (λ (m) (λ (f) (λ (x) ((m f) ((n f) x)))))))
(define add* (λ (n) (n succ)))

(define succ** (add one))

(define mult (λ (n) (λ (m) (λ (f) (m (n f))))))
(define mult* (λ (n) (λ (m) ((m (add n)) zero))))

(define expt  (λ (n) (λ (m) (m n))))
(define expt* (λ (n) (λ (m) ((m (mult n)) one))))

(define (nat->church n)
  (cond
    [(zero? n) zero]
    [else (succ (nat->church (sub1 n)))]))

(define (church->nat n) ((n add1) 0))

(define three (nat->church 3))
(define four (nat->church 4))

(church->nat ((add three) four))
(church->nat ((mult three) four))
(church->nat ((expt three) four))
(church->nat ((expt four) three))
```


{{out}}

```txt

7
12
81
64

```



## Rust


```rust
use std::rc::Rc;
use std::ops::{Add, Mul};

#[derive(Clone)]
struct Church<'a, T: 'a> {
    runner: Rc<dyn Fn(Rc<dyn Fn(T) -> T + 'a>) -> Rc<dyn Fn(T) -> T + 'a> + 'a>,
}

impl<'a, T> Church<'a, T> {
    fn zero() -> Self {
        Church {
            runner: Rc::new(|_f| {
                Rc::new(|x| x)
            })
        }
    }

    fn succ(self) -> Self {
        Church {
            runner: Rc::new(move |f| {
                let g = self.runner.clone();
                Rc::new(move |x| f(g(f.clone())(x)))
            })
        }
    }

    fn run(&self, f: impl Fn(T) -> T + 'a) -> Rc<dyn Fn(T) -> T + 'a> {
        (self.runner)(Rc::new(f))
    }

    fn exp(self, rhs: Church<'a, Rc<dyn Fn(T) -> T + 'a>>) -> Self
    {
        Church {
            runner: (rhs.runner)(self.runner)
        }
    }
}

impl<'a, T> Add for Church<'a, T> {
    type Output = Church<'a, T>;

    fn add(self, rhs: Church<'a, T>) -> Church<T> {
        Church {
            runner: Rc::new(move |f| {
                let self_runner = self.runner.clone();
                let rhs_runner = rhs.runner.clone();
                Rc::new(move |x| (self_runner)(f.clone())((rhs_runner)(f.clone())(x)))
            })
        }
    }
}

impl<'a, T> Mul for Church<'a, T> {
    type Output = Church<'a, T>;

    fn mul(self, rhs: Church<'a, T>) -> Church<T> {
        Church {
            runner: Rc::new(move |f| {
                (self.runner)((rhs.runner)(f))
            })
        }
    }
}

impl<'a, T> From<i32> for Church<'a, T> {
    fn from(n: i32) -> Church<'a, T> {
        let mut ret = Church::zero();
        for _ in 0..n {
            ret = ret.succ();
        }
        ret
    }
}

impl<'a> From<&Church<'a, i32>> for i32  {
    fn from(c: &Church<'a, i32>) -> i32 {
        c.run(|x| x + 1)(0)
    }
}

fn three<'a, T>() -> Church<'a, T> {
    Church::zero().succ().succ().succ()
}

fn four<'a, T>() -> Church<'a, T> {
    Church::zero().succ().succ().succ().succ()
}

fn main() {
    println!("three =\t{}", i32::from(&three()));
    println!("four =\t{}", i32::from(&four()));

    println!("three + four =\t{}", i32::from(&(three() + four())));
    println!("three * four =\t{}", i32::from(&(three() * four())));

    println!("three ^ four =\t{}", i32::from(&(three().exp(four()))));
    println!("four ^ three =\t{}", i32::from(&(four().exp(three()))));
}
```

{{Out}}

```txt
three =	3
four =	4
three + four =	7
three * four =	12
three ^ four =	81
four ^ three =	64
```



## Swift


```swift
func succ<A, B, C>(_ n: @escaping (@escaping (A) -> B) -> (C) -> A) -> (@escaping (A) -> B) -> (C) -> B {
  return {f in
    return {x in
      return f(n(f)(x))
    }
  }
}

func zero<A, B>(_ a: A) -> (B) -> B {
  return {b in
    return b
  }
}

func three<A>(_ f: @escaping (A) -> A) -> (A) -> A {
  return {x in
    return succ(succ(succ(zero)))(f)(x)
  }
}

func four<A>(_ f: @escaping (A) -> A) -> (A) -> A {
  return {x in
    return succ(succ(succ(succ(zero))))(f)(x)
  }
}

func add<A, B, C>(_ m: @escaping (B) -> (A) -> C) -> (@escaping (B) -> (C) -> A) -> (B) -> (C) -> C {
  return {n in
    return {f in
      return {x in
        return m(f)(n(f)(x))
      }
    }
  }
}

func mult<A, B, C>(_ m: @escaping (A) -> B) -> (@escaping (C) -> A) -> (C) -> B {
  return {n in
    return {f in
      return m(n(f))
    }
  }
}

func exp<A, B, C>(_ m: A) -> (@escaping (A) -> (B) -> (C) -> C) -> (B) -> (C) -> C {
  return {n in
    return {f in
      return {x in
        return n(m)(f)(x)
      }
    }
  }
}

func church<A>(_ x: Int) -> (@escaping (A) -> A) -> (A) -> A {
  guard x != 0 else { return zero }

  return {f in
    return {a in
      return f(church(x - 1)(f)(a))
    }
  }
}

func unchurch<A>(_ f: (@escaping (Int) -> Int) -> (Int) -> A) -> A {
  return f({i in
    return i + 1
  })(0)
}

let a = unchurch(add(three)(four))
let b = unchurch(mult(three)(four))
// We can even compose operations
let c = unchurch(exp(mult(four)(church(1)))(three))
let d = unchurch(exp(mult(three)(church(1)))(four))

print(a, b, c, d)
```

{{out}}

```txt
7 12 64 81
```



## zkl


```zkl
class Church{  // kinda heavy, just an int + fcn churchAdd(ca,cb) would also work
   fcn init(N){ var n=N; }	// Church Zero is Church(0)
   fcn toInt(f,x){ do(n){ x=f(x) } x } // c(3)(f,x) --> f(f(f(x)))
   fcn succ{ self(n+1) }
   fcn __opAdd(c){ self(n+c.n)      }
   fcn __opMul(c){ self(n*c.n)      }
   fcn pow(c)    { self(n.pow(c.n)) }
   fcn toString{ String("Church(",n,")") }
}
```


```zkl
c3,c4 := Church(3),c3.succ();
f,x := Op("+",1),0;
println("f=",f,", x=",x);
println("%s+%s=%d".fmt(c3,c4, (c3+c4).toInt(f,x)      ));
println("%s*%s=%d".fmt(c3,c4, (c3*c4).toInt(f,x)      ));
println("%s^%s=%d".fmt(c4,c3, (c4.pow(c3)).toInt(f,x) ));
println("%s^%s=%d".fmt(c3,c4, (c3.pow(c4)).toInt(f,x) ));
println();
T(c3+c4,c3*c4,c4.pow(c3),c3.pow(c4)).apply("toInt",f,x).println();
```

{{out}}

```txt

f=Op(+1), x=0
Church(3)+Church(4)=7
Church(3)*Church(4)=12
Church(4)^Church(3)=64
Church(3)^Church(4)=81

L(7,12,64,81)

```

OK, that was the easy sleazy cheat around way to do it. 
The wad of nested functions way is as follows:

```zkl
fcn churchZero{ return(fcn(x){ x }) } // or fcn churchZero{ self.fcn.idFcn }
fcn churchSucc(c){ return('wrap(f){ return('wrap(x){ f(c(f)(x)) }) }) }
fcn churchAdd(c1,c2){ return('wrap(f){ return('wrap(x){ c1(f)(c2(f)(x)) }) }) }
fcn churchMul(c1,c2){ return('wrap(f){ c1(c2(f)) }) }
fcn churchPow(c1,c2){ return('wrap(f){ c2(c1)(f) }) }
fcn churchToInt(c,f,x){ c(f)(x) }
fcn churchFromInt(n){ c:=churchZero; do(n){ c=churchSucc(c) } c }
//fcn churchFromInt(n){ (0).reduce(n,churchSucc,churchZero) } // what ever
```


```zkl
c3,c4 := churchFromInt(3),churchSucc(c3);
f,x   := Op("+",1),0;	// x>=0, ie natural number
T(c3,c4,churchAdd(c3,c4),churchMul(c3,c4),churchPow(c4,c3),churchPow(c3,c4))
   .apply(churchToInt,f,x).println();
```

{{out}}

```txt

L(3,4,7,12,64,81)

```

