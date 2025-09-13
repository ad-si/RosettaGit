+++
title = "Monads/Maybe monad"
description = ""
date = 2019-10-21T12:40:52Z
aliases = []
[extra]
id = 20016
[taxonomies]
categories = ["task"]
tags = []
+++

Demonstrate in your programming language the following:

#Construct a Maybe Monad by writing the 'bind' function and the 'unit' (sometimes known as 'return') function for that Monad (or just use what the language already has implemented)
#Make two functions, each which take a number and return a monadic number, e.g. Int -> Maybe Int and Int -> Maybe String
#Compose the two functions with bind


A [[wp:Monad_(functional_programming)|Monad]] is a single type which encapsulates several other types, eliminating boilerplate code. In practice it acts like a dynamically typed computational sequence, though in many cases the type issues can be resolved at compile time.

A Maybe Monad is a monad which specifically encapsulates the type of an undefined value.



## ALGOL 68

```algol68
BEGIN

    # This is a translation of the Javascript sample, main differences are because Algol 68    #
    # is strongly typed and "on-the-fly" constructon of functions is not really possible -     #
    # we need to define a STRUCT where Javascript would just construct a new function.         #
    # As Algol 68 does not allow procedure overloading, we use custom operators (which do      #
    # allow overloading) so the techniques used here could be extended to procedures with      #
    # signatures other than PROC(REAL)REAL                                                     #
    # The comments are generally the same as in the javascript original, changed as necessary  #
    # to reflect Algol 68...                                                                   #
 
    # START WITH SOME SIMPLE (UNSAFE) PARTIAL FUNCTIONS:                                       #
 
    # error in n < 0 #
    PROC unsafe reciprocal = (REAL n)REAL: 1 / n;
    # error if n < 0 #
    PROC unsafe root       = (REAL n)REAL: sqrt(n);
    # error if n <= 0 #
    PROC unsafe log        = (REAL n)REAL: ln(n);
 
 
    # NOW DERIVE SAFE VERSIONS OF THESE SIMPLE FUNCTIONS:                                      #
    # These versions use a validity test, and return a wrapped value                           #
    # with a boolean is valid property as well as a value property                             #

    MODE SAFEFUNCTION = STRUCT( PROC(REAL)REAL fn, PROC(REAL)BOOL fn safety check ); 
    MODE MAYBE        = STRUCT( BOOL is valid, REAL value );
 
    SAFEFUNCTION safe reciprocal = ( unsafe reciprocal, ( REAL n )BOOL: n /= 0 );
    SAFEFUNCTION safe root       = ( unsafe root,       ( REAL n )BOOL: n >= 0 );
    SAFEFUNCTION safe log        = ( unsafe log,        ( REAL n )BOOL: n >  0 );
 
COMMENT 
    the original Javascript contains this:

    // THE DERIVATION OF THE SAFE VERSIONS USED THE 'UNIT' OR 'RETURN' 
    // FUNCTION OF THE MAYBE MONAD
 
    // Named maybe() here, the unit function of the Maybe monad wraps a raw value 
    // in a datatype with two elements: .isValid (Bool) and .value (Number)
 
    // a -> Ma
    function maybe(n) {
        return {
            isValid: (typeof n !== 'undefined'),
            value: n
        };
    }

    However Algol 68 is strongly typed, so the type (MODE) of the function parameters
    cannot be anything other than REAL. We therefore use "MAYBE( TRUE, n )" instead.
COMMENT

    # THE PROBLEM FOR FUNCTION NESTING (COMPOSITION) OF THE SAFE FUNCTIONS                     #
    # IS THAT THEIR INPUT AND OUTPUT TYPES ARE DIFFERENT                                       #
 
    # Our safe versions of the functions take simple numeric arguments (i.e. REAL), but return #
    # wrapped results. If we feed a wrapped result as an input to another safe function, the   #
    # compiler will object. The solution is to write a higher order                            #
    # function (sometimes called 'bind' or 'chain') which handles composition, taking a        #
    # a safe function and a wrapped value as arguments,                                        #
 
    # The 'bind' function of the Maybe monad:                                                  #
    # 1. Applies a 'safe' function directly to the raw unwrapped value, and                    #
    # 2. returns the wrapped result.                                                           #
 
    # Ma -> (a -> Mb) -> Mb #
    # defined as an operator to allow overloading to other PROC modes                          #
    PRIO BIND = 1;
    OP   BIND = (MAYBE maybe n, SAFEFUNCTION mf )MAYBE:
        IF is valid OF maybe n THEN mf CALL ( value OF maybe n ) ELSE maybe n FI;
    # we need an operator to call the wrapped function                                         #
    PRIO CALL = 1;
    OP   CALL = ( SAFEFUNCTION f, REAL value )MAYBE:
         BEGIN
             BOOL is valid = ( fn safety check OF f )( value );
             MAYBE( is valid, IF is valid THEN ( fn OF f )( value ) ELSE value FI )
         END; # CALL #

    # Using the bind function, we can nest applications of safe functions,                     #
    # without the compiler choking on unexpectedly wrapped values returned from                #
    # other functions of the same kind.                                                        #
    REAL root one over four = value OF ( MAYBE( TRUE, 4 ) BIND safe reciprocal BIND safe root );

#   print( ( root one over four, newline ) ); #
    # -> 0.5 #
 
    # We can compose a chain of safe functions (of any length) with a simple foldr/reduceRight #
    # which starts by 'lifting' the numeric argument into a Maybe wrapping,                    #
    # and then nests function applications (working from right to left)                        #
    # again, defined as an operator here to allow extension to other PROC modes                #
    # also, as Algol 68 doesn't have builtin foldr/reduceRight, we need a loop...              #
    PRIO SAFECOMPOSE = 1;
    OP   SAFECOMPOSE = ( []SAFEFUNCTION lst functions, REAL value )MAYBE:
         BEGIN
             MAYBE result := MAYBE( TRUE, value );
             FOR fn pos FROM UPB lst functions BY -1 TO LWB lst functions DO
                 result := result BIND lst functions[ fn pos ]
             OD;
             result
         END; # SAFECOMPOSE #
 
    # TEST INPUT VALUES WITH A SAFELY COMPOSED VERSION OF LOG(SQRT(1/X))                       #
 
    PROC safe log root reciprocal = ( REAL n )MAYBE:
         BEGIN
             # this declaration is requied for Algol 68G 2.8                                   #
             []SAFEFUNCTION function list = ( safe log, safe root, safe reciprocal );
             function list SAFECOMPOSE n
         END; # safe log root reciprocal #

    # Algol 68 doesn't have a builtin map operator, we could define one here but we can just   #
    # use a loop for the purposes of this task...                                              #
    REAL e = exp( 1 );
    []REAL test values = ( -2, -1, -0.5, 0, 1 / e, 1, 2, e, 3, 4, 5 );

    STRING prefix := "[";
    FOR test pos FROM LWB test values TO UPB test values DO
        MAYBE result = safe log root reciprocal( test values[ test pos ] );
        print( ( prefix, IF is valid OF result THEN fixed( value OF result, -12, 8 ) ELSE "undefined" FI ) );
        IF test pos MOD 4 = 0 THEN print( ( newline ) ) FI;
        prefix := ", "
    OD;
    print( ( "]", newline ) )
 
END
```

```txt

[undefined, undefined, undefined, undefined
,   0.50000000,   0.00000000,  -0.34657359,  -0.50000000
,  -0.54930614,  -0.69314718,  -0.80471896]

```



## AppleScript


Algebraically-reasoned defence against invalid arguments for partial functions buried deep in function nests is probably more than a light-weight scripting language will really need on an average weekday, but we can usually do most things in most languages, and stretching a language a little bit is a way of exploring both its limits, and its relationships with other languages.

What AppleScript mostly lacks here (apart from a rich core library) is a coherent first-class function type which allows for anonymous functions. Nevertheless there is enough there to emulate first-class functions (using script objects), and we can set up a working Maybe monad without too much trouble.

It would, at least, spare us from having to structure things around '''try … on error …  end try''' etc



```AppleScript
property e : 2.71828182846

on run {}
    
    -- Derive safe versions of three simple functions
    set sfReciprocal to safeVersion(reciprocal, notZero)
    set sfRoot to safeVersion(root, isPositive)
    set sfLog to safeVersion(ln, aboveZero)
    
    
    -- Test a composition of these function with a range of invalid and valid arguments
    
    -- (The safe composition returns missing value (without error) for invalid arguments)
    
    map([-2, -1, -0.5, 0, 1 / e, 1, 2, e, 3, 4, 5], safeLogRootReciprocal)

    -- 'missing value' is returned by a safe function (and threaded up through the monad) when the input argument is out of range
    --> {missing value, missing value, missing value, missing value, 0.5, 0.0, -0.346573590279, -0.499999999999, -0.549306144333, -0.69314718056, -0.804718956217}
end run


-- START WITH SOME SIMPLE (UNSAFE) PARTIAL FUNCTIONS:

-- Returns ERROR 'Script Error: Can’t divide 1.0 by zero.' if n = 0
on reciprocal(n)
    1 / n
end reciprocal

-- Returns ERROR 'error "The result of a numeric operation was too large." number -2702'
-- for all values below 0
on root(n)
    n ^ (1 / 2)
end root

-- Returns -1.0E+20 for all values of zero and below
on ln(n)
    (do shell script ("echo 'l(" & (n as string) & ")' | bc -l")) as real
end ln

-- DERIVE A SAFE VERSION OF EACH FUNCTION 
-- (SEE on Run() handler)

on safeVersion(f, fnSafetyCheck)
    script
        on call(x)
            if sReturn(fnSafetyCheck)'s call(x) then
                sReturn(f)'s call(x)
            else
                missing value
            end if
        end call
    end script
end safeVersion

on notZero(n)
    n is not 0
end notZero

on isPositive(n)
    n ≥ 0
end isPositive

on aboveZero(n)
    n > 0
end aboveZero


-- DEFINE A FUNCTION WHICH CALLS A COMPOSITION OF THE SAFE VERSIONS
on safeLogRootReciprocal(x)
    
    value of mbCompose([my sfLog, my sfRoot, my sfReciprocal], x)
    
end safeLogRootReciprocal


-- UNIT/RETURN and BIND functions for the Maybe monad

-- Unit / Return for maybe
on maybe(n)
    {isValid:n is not missing value, value:n}
end maybe

-- BIND maybe
on mbBind(recMaybe, mfSafe)
    if isValid of recMaybe then
        maybe(mfSafe's call(value of recMaybe))
    else
        recMaybe
    end if
end mbBind

-- lift 2nd class function into 1st class wrapper 
-- handler function --> first class script object
on sReturn(f)
    script
        property call : f
    end script
end sReturn

-- return a new script in which function g is composed
-- with the f (call()) of the Mf script
-- Mf -> (f -> Mg) -> Mg
on sBind(mf, g)
    script
        on call(x)
            sReturn(g)'s call(mf's call(x))
        end call
    end script
end sBind

on mbCompose(lstFunctions, value)
    reduceRight(lstFunctions, mbBind, maybe(value))
end mbCompose

-- xs: list, f: function, a: initial accumulator value
-- the arguments available to the function f(a, x, i, l) are
-- v: current accumulator value
-- x: current item in list
-- i: [ 1-based index in list ] optional
-- l: [ a reference to the list itself ] optional
on reduceRight(xs, f, a)
    set mf to sReturn(f)
    
    repeat with i from length of xs to 1 by -1
        set a to mf's call(a, item i of xs, i, xs)
    end repeat
end reduceRight

-- [a] -> (a -> b) -> [b]
on map(xs, f)
    set mf to sReturn(f)
    set lst to {}
    set lng to length of xs
    repeat with i from 1 to lng
        set end of lst to mf's call(item i of xs, i, xs)
    end repeat
    return lst
end map

```


```txt

-- 'missing value' is returned by a safe function (and threaded up through the monad) when the input argument is out of range

{missing value, missing value, missing value, missing value, 0.5, 0.0, -0.346573590279, -0.499999999999, -0.549306144333, -0.69314718056, -0.804718956217}
```



## Clojure



```clojure

(defn bind [val f]
  (if-let [v (:value val)] (f v) val))

(defn unit [val] {:value val})

(defn opt_add_3 [n] (unit (+ 3 n))) ; takes a number and returns a Maybe number
(defn opt_str [n] (unit (str n)))   ; takes a number and returns a Maybe string

(bind (unit 4) opt_add_3)                  ; evaluates to {:value 7}
(bind (unit nil) opt_add_3)                ; evaluates to {:value nil}
(bind (bind (unit 8) opt_add_3) opt_str)   ; evaluates to {:value "11"}
(bind (bind (unit nil) opt_add_3) opt_str) ; evaluates to {:value nil}

```



## EchoLisp

Our monadic Maybe elements will be pairs (boolean . value), where value is in Maybe.domain.
Functions which return something not in Maybe.domain are unsafe and return (#f . input-value), If a function is given as input a (#f . value) element, it will return this element. 


```scheme

(define (Maybe.domain? x) (or (number? x) (string? x)))
(define (Maybe.unit elem (bool #t)) (cons bool elem))

;; f is a safe or unsafe function
;; (Maybe.lift f) returns a safe Maybe function which returns a Maybe element
(define (Maybe.lift f)     
		(lambda(x) 
             (let [(u (f x))]
             (if (Maybe.domain? u) 
                (Maybe.unit u)
                (Maybe.unit x #f))))) ;; return offending x
                                
                            
;; elem = Maybe element
;; f is safe or unsafe  (lisp) function
;; return Maybe element
(define (Maybe.bind f elem)  
		(if (first elem) ((Maybe.lift f)  (rest elem)) elem))
		
;; pretty-print		
(define (Maybe.print elem)
		(if (first elem)  (writeln elem ) (writeln '❌ elem)))

;; unsafe functions
(define (u-log x) (if (> x 0) (log x) #f))
(define (u-inv x) (if (zero? x) 'zero-div (/ x)))

;; (print (number->string (exp (log 3))))
(->> 3 Maybe.unit (Maybe.bind u-log) (Maybe.bind exp)  (Maybe.bind number->string) Maybe.print)
    → (#t . "3.0000000000000004")    

;; (print (number->string (exp (log -666))))
(->> -666  Maybe.unit (Maybe.bind u-log) (Maybe.bind exp)  (Maybe.bind number->string) Maybe.print)
     → ❌     (#f . -666)   
      
;; ;; (print (number->string (inverse (log 1))))
(->> 1 Maybe.unit (Maybe.bind u-log)  (Maybe.bind u-inv)  (Maybe.bind number->string) Maybe.print)
     →  ❌     (#f . 0)   

```



## Factor

Factor comes with an implementation of Haskell-style monads in the <code>monads</code> vocabulary.

```factor
USING: monads ;
FROM: monads => do ;

! Prints "T{ just { value 7 } }"
3 maybe-monad return >>= [ 2 * maybe-monad return ] swap call
                     >>= [ 1 + maybe-monad return ] swap call .

! Prints "nothing"
nothing >>= [ 2 * maybe-monad return ] swap call
        >>= [ 1 + maybe-monad return ] swap call .
```

Or:

```factor>3 <just
 [ 2 * <just> ] bind [ 1 + <just> ] bind .
nothing [ 2 * <just> ] bind [ 1 + <just> ] bind .
```

Or with <code>do</code> notation:

```factor
{
    [ 3 <just> ]
    [ 2 * <just> ]
    [ 1 + <just> ]
} do .
{
    [ nothing ]
    [ 2 * <just> ]
    [ 1 + <just> ]
} do .
```



## Go


```go
package main

import (
    "fmt"
    "strconv"
)

type maybe struct{ value *int }

func (m maybe) bind(f func(p *int) maybe) maybe {
    return f(m.value)
}

func unit(p *int) maybe {
    return maybe{p}
}

func decrement(p *int) maybe {
    if p == nil {
        return unit(nil)
    } else {
        q := *p - 1
        return unit(&q)
    }
}

func triple(p *int) maybe {
    if p == nil {
        return unit(nil)
    } else {
        q := (*p) * 3
        return unit(&q)
    }
}

func main() {
    i, j, k := 3, 4, 5
    for _, p := range []*int{&i, &j, nil, &k} {
        m1 := unit(p)
        m2 := m1.bind(decrement).bind(triple)
        var s1, s2 string = "none", "none"
        if m1.value != nil {
            s1 = strconv.Itoa(*m1.value)
        }
        if m2.value != nil {
            s2 = strconv.Itoa(*m2.value)
        }
        fmt.Printf("%4s -> %s\n", s1, s2)
    }
}
```


```txt

   3 -> 6
   4 -> 9
none -> none
   5 -> 12

```



## Haskell


Haskell has the built-in <code>Monad</code> type class, and the built-in <code>Maybe</code> type already conforms to the <code>Monad</code> type class.

```haskell
main = do print $ Just 3 >>= (return . (*2)) >>= (return . (+1))  -- prints "Just 7"
          print $ Nothing >>= (return . (*2)) >>= (return . (+1)) -- prints "Nothing"
```


Or, written using <code>do</code> notation:

```haskell
main = do print (do x <- Just 3
                    y <- return (x*2)
                    z <- return (y+1)
		    return z)
          print (do x <- Nothing
                    y <- return (x*2)
                    z <- return (y+1)
                    return z)
```


Or alternately:

```haskell
main = do print (do x <- Just 3
                    let y = x*2
                    let z = y+1
		    return z)
          print (do x <- Nothing
                    let y = x*2
                    let z = y+1
                    return z)
```


Deriving and composing safe versions of reciprocal, square root and log functions. :

```haskell
import Control.Monad ((>=>))

safeVersion :: (a -> b) -> (a -> Bool) -> a -> Maybe b
safeVersion f fnSafetyCheck x | fnSafetyCheck x = Just (f x)
                              | otherwise       = Nothing

safeReciprocal = safeVersion (1/) (/=0)
safeRoot = safeVersion sqrt (>=0)
safeLog = safeVersion log (>0)

safeLogRootReciprocal = safeReciprocal >=> safeRoot >=> safeLog

main = print $ map safeLogRootReciprocal [-2, -1, -0.5, 0, exp (-1), 1, 2, exp 1, 3, 4, 5]
```

```txt
[Nothing,Nothing,Nothing,Nothing,Just 0.5,Just 0.0,Just (-0.3465735902799726),Just (-0.5),Just (-0.5493061443340549),Just (-0.6931471805599453),Just (-0.8047189562170503)]
```



## Hoon


```hoon

:-  %say
|=  [^ [[txt=(unit ,@tas) ~] ~]]
:-  %noun
|^
  %+  biff  txt
    ;~  biff
      m-parse
      m-double
    ==
  ++  m-parse
    |=  a=@tas
    ^-  (unit ,@ud)
    (rust (trip a) dem)
  ::
  ++  m-double
    |=  a=@ud
    ^-  (unit ,@ud)
    (some (mul a 2))
  --

```


Hoon has a built-in rune, %smsg (;~) that binds gates under a monad.

++unit is Hoon's Maybe: it is either ~ (None) or [~ u=val] (Some)

++biff is the monadic bind, which %smsg uses to wire the gates together. It's defined in the standard library [https://github.com/urbit/urbit/blob/6433c621585e87c5d66026d4a63b409babbbab11/urb/zod/arvo/hoon.hoon#L585 here]. m-parse is @tas -> (unit ,@ud), so I use biff a second time in order for the program to be called with (unit ,@tas).

++rust is one of the parser combinator runners: it parses the string `a` with the rule `dem`, returning a unit with the returned value if it success or ~ if it fails. Note that Hoon's type system is complex enough to get a strongly typed result of the parsing rule, in this case an unsigned decimal (@ud)


```txt

> +monad (some '2')
[~ 4]
> +monad (some 'a')
~
> +monad ~
~

```



## J


It's difficult to find a useful and illustrative (but simple) example of this task in J. So we shall not try to be useful.


```J

NB. monad implementation:
unit=: <
bind=: (@>)( :: ])

NB. monad utility
safeVersion=: (<@) ( ::((<_.)"_))
safeCompose=:dyad define
  dyad def 'x`:6 bind y'/x,unit y
)

NB. unsafe functions (fail with infinite arguments)
subtractFromSelf=: -~
divideBySelf=: %~

NB. wrapped functions:
safeSubtractFromSelf=: subtractFromSelf safeVersion
safeDivideBySelf=: divideBySelf safeVersion

NB. task example:
     safeSubtractFromSelf bind safeDivideBySelf 1
┌─┐
│0│
└─┘
     safeSubtractFromSelf bind safeDivideBySelf _
┌──┐
│_.│
└──┘
```



## JavaScript



### ES5


Example: deriving and composing safe versions of reciprocal, square root and log functions.


```JavaScript
(function () {
    'use strict';

    // START WITH SOME SIMPLE (UNSAFE) PARTIAL FUNCTIONS:

    // Returns Infinity if n === 0
    function reciprocal(n) {
        return 1 / n;
    }

    // Returns NaN if n < 0
    function root(n) {
        return Math.sqrt(n);
    }

    // Returns -Infinity if n === 0
    // Returns NaN if n < 0
    function log(n) {
        return Math.log(n);
    }


    // NOW DERIVE SAFE VERSIONS OF THESE SIMPLE FUNCTIONS:
    // These versions use a validity test, and return a wrapped value
    // with a boolean .isValid property as well as a .value property

    function safeVersion(f, fnSafetyCheck) {
        return function (v) {
            return maybe(fnSafetyCheck(v) ? f(v) : undefined);
        }
    }

    var safe_reciprocal = safeVersion(reciprocal, function (n) {
        return n !== 0;
    });

    var safe_root = safeVersion(root, function (n) {
        return n >= 0;
    });


    var safe_log = safeVersion(log, function (n) {
        return n > 0;
    });


    // THE DERIVATION OF THE SAFE VERSIONS USED THE 'UNIT' OR 'RETURN' 
    // FUNCTION OF THE MAYBE MONAD

    // Named maybe() here, the unit function of the Maybe monad wraps a raw value 
    // in a datatype with two elements: .isValid (Bool) and .value (Number)

    // a -> Ma
    function maybe(n) {
        return {
            isValid: (typeof n !== 'undefined'),
            value: n
        };
    }

    // THE PROBLEM FOR FUNCTION NESTING (COMPOSITION) OF THE SAFE FUNCTIONS
    // IS THAT THEIR INPUT AND OUTPUT TYPES ARE DIFFERENT

    // Our safe versions of the functions take simple numeric arguments, but return
    // wrapped results. If we feed a wrapped result as an input to another safe function,
    // it will choke on the unexpected type. The solution is to write a higher order
    // function (sometimes called 'bind' or 'chain') which handles composition, taking a 
    // a safe function and a wrapped value as arguments,

    // The 'bind' function of the Maybe monad:
    // 1. Applies a 'safe' function directly to the raw unwrapped value, and
    // 2. returns the wrapped result.

    // Ma -> (a -> Mb) -> Mb
    function bind(maybeN, mf) {
        return (maybeN.isValid ? mf(maybeN.value) : maybeN);
    }

    // Using the bind function, we can nest applications of safe_ functions,
    // without their choking on unexpectedly wrapped values returned from
    // other functions of the same kind.
    var rootOneOverFour = bind(
        bind(maybe(4), safe_reciprocal), safe_root
    ).value;

    // -> 0.5


    // We can compose a chain of safe functions (of any length) with a simple foldr/reduceRight
    // which starts by 'lifting' the numeric argument into a Maybe wrapping,
    // and then nests function applications (working from right to left)
    function safeCompose(lstFunctions, value) {
        return lstFunctions
            .reduceRight(function (a, f) {
                return bind(a, f);
            }, maybe(value));
    }

    // TEST INPUT VALUES WITH A SAFELY COMPOSED VERSION OF LOG(SQRT(1/X))

    var safe_log_root_reciprocal = function (n) {
        return safeCompose([safe_log, safe_root, safe_reciprocal], n).value;
    }

    return [-2, -1, -0.5, 0, 1 / Math.E, 1, 2, Math.E, 3, 4, 5].map(
        safe_log_root_reciprocal
    );

})();
```


```txt
[undefined, undefined, undefined, undefined, 0.5, 0,
-0.3465735902799726, -0.5, -0.5493061443340549,
-0.6931471805599453, -0.8047189562170503]
```



## Julia


```julia
struct maybe x::Union{Real, Missing}; end

Base.show(io::IO, m::maybe) = print(io, m.x)

unit(x) = maybe(x)
bind(f, x) = unit(f(x.x))

f1(x) = 5x
f2(x) = x + 4

a = unit(3)
b = unit(missing)

println(a, " -> ", bind(f2, bind(f1, a)))

println(b, " -> ", bind(f2, bind(f1, b)))

```
```txt

3 -> 19
missing -> missing

```



## Kotlin

The JVM already contains a 'Maybe' monad in the form of the java.util.Optional<T> generic class. 

Its static methods, 'of' and 'ofNullable', serve as its 'unit' function for wrapping nullable and non-nullable values respectively and its instance method, 'flatMap', serves as its 'bind' function.

Rather than write something from scratch, we use this class to complete this task.

```scala
// version 1.2.10

import java.util.Optional

/* doubles 'i' before wrapping it */
fun getOptionalInt(i: Int) = Optional.of(2 * i)

/* returns an 'A' repeated 'i' times wrapped in an Optional<String> */
fun getOptionalString(i: Int) = Optional.of("A".repeat(i))

/* does same as above if i > 0, otherwise returns an empty Optional<String> */
fun getOptionalString2(i: Int) =
   Optional.ofNullable(if (i > 0) "A".repeat(i) else null)

fun main(args: Array<String>) {
    /* prints 10 'A's */
    println(getOptionalInt(5).flatMap(::getOptionalString).get())

    /* prints  4 'A's */
    println(getOptionalInt(2).flatMap(::getOptionalString2).get())

    /* prints 'false' as there is no value present in the Optional<String> instance */
    println(getOptionalInt(0).flatMap(::getOptionalString2).isPresent)
}
```


```txt

AAAAAAAAAA
AAAA
false

```



## Racket


It is idiomatic in Racket to use <code>#f</code> for <code>Nothing</code>, and every other value is considered implicitly tagged with <code>Just</code>.


```racket
#lang racket

(require syntax/parse/define)

(define (bind x f) (and x (f x)))
(define return identity)

;; error when arg = 0
(define reciprocal (curry / 1))
;; error when arg < 0
(define (root x) (if (< x 0) (error 'bad) (sqrt x)))
;; error whe  arg <= 0
(define (ln x) (if (<= x 0) (error 'bad) (log x)))

(define (lift f check) (λ (x) (and (check x) (f x))))

(define safe-reciprocal (lift reciprocal (negate (curry equal? 0))))
(define safe-root (lift root (curry <= 0)))
(define safe-ln (lift ln (curry < 0)))

(define (safe-log-root-reciprocal x)
  (bind (bind (bind x safe-reciprocal) safe-root) safe-ln))

(define tests `(-2 -1 -0.5 0 1 ,(exp -1) 1 2 ,(exp 1) 3 4 5))

(map safe-log-root-reciprocal tests)

(define-syntax-parser do-macro
  [(_ [x {~datum <-} y] . the-rest) #'(bind y (λ (x) (do-macro . the-rest)))]
  [(_ e) #'e])

(define (safe-log-root-reciprocal* x)
  (do-macro [x <- (safe-reciprocal x)]
            [x <- (safe-root x)]
            [x <- (safe-ln x)]
            (return x)))

(map safe-log-root-reciprocal* tests)
```


```txt

'(#f #f #f #f 0 0.5 0 -0.3465735902799726 -0.5 -0.5493061443340549 -0.6931471805599453 -0.8047189562170503)
'(#f #f #f #f 0 0.5 0 -0.3465735902799726 -0.5 -0.5493061443340549 -0.6931471805599453 -0.8047189562170503)

```



## Ruby


OOP version using Ruby's block syntax


```ruby
class Maybe
  def initialize(value)
    @value = value
  end

  def map
    if @value.nil?
      self
    else
      Maybe.new(yield @value)
    end
  end
end

Maybe.new(3).map { |n| 2*n }.map { |n| n+1 }
#=> #<Maybe @value=7>

Maybe.new(nil).map { |n| 2*n }.map { |n| n+1 }
#=> #<Maybe @value=nil>

Maybe.new(3).map { |n| nil }.map { |n| n+1 }
#=> #<Maybe @value=nil>

# alias Maybe#new and write bind to be in line with task

class Maybe
  class << self
    alias :unit :new
  end
  
  def initialize(value)
    @value = value
  end

  def bind
    if @value.nil?
      self
    else
      yield @value
    end
  end
end

Maybe.unit(3).bind { |n| Maybe.unit(2*n) }.bind { |n| Maybe.unit(n+1) }
#=> #<Maybe @value=7>

Maybe.unit(nil).bind { |n| Maybe.unit(2*n) }.bind { |n| Maybe.unit(n+1) }
#=> #<Maybe @value=nil>

```



## zkl

While I'm unsure of the utility of Monads in a dynamic type-less language, it can be done.

From the [[wp:Monad_(functional_programming)#The_Maybe_monad|Wikipedia]]

Here we use the Void object as Nothing and define some functions. Since zkl is type-less, we can consider Maybe as a native type and don't need to define it.

```zkl
fcn bind(a,type,b){ if(type.isType(a)) b else Void }
fcn just(x){ if(Deferred.isType(x)) x() else x }  // force lazy evaluation
fcn rtn(x) { just(x) }
```

Since zkl is eager, add needs to gyrate a bit by creating a lazy result and evaluating that after the binds have done their bizness.

```zkl
fcn add(mx,my){
   bind(mx,Int,
      bind(my,Int,
        '+.fp(mx,my))) : rtn(_)  // create a lazy mx+my to avoid eager eval
}
add(1,2).println();    // two ints
add(1,2.0).println();  // int and float
add(self,2).println(); // class and int
```

```txt

3
Void
Void

```

