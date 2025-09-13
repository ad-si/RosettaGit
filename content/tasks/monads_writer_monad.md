+++
title = "Monads/Writer monad"
description = ""
date = 2019-10-07T22:22:31Z
aliases = []
[extra]
id = 20022
[taxonomies]
categories = ["task"]
tags = []
+++

The Writer monad is a programming design pattern which makes it possible to compose functions which return their result values paired with a log string. The final result of a composed function yields both a value, and a concatenation of the logs from each component function application.

Demonstrate in your programming language the following:

# Construct a Writer monad by writing the 'bind' function and the 'unit' (sometimes known as 'return') function for that monad (or just use what the language already provides)
# Write three simple functions: root, addOne, and half
# Derive Writer monad versions of each of these functions
# Apply a composition of the Writer versions of root, addOne, and half to the integer 5, deriving both a value for the Golden Ratio φ, and a concatenated log of the function applications (starting with the initial value, and followed by the application of root, etc.)



## ALGOL 68

```algol68
BEGIN
    MODE MWRITER = STRUCT( LONG REAL value
                         , STRING    log
                         );
    PRIO BIND = 9;
    OP   BIND = ( MWRITER m, PROC( LONG REAL )MWRITER f )MWRITER:
    (    MWRITER n := f( value OF m );
         log OF n  := log OF m + log OF n;
         n
    );

    OP   LEN     = ( STRING s )INT: ( UPB s + 1 ) - LWB s;
    PRIO PAD     = 9;
    OP   PAD     = ( STRING s, INT width )STRING: IF LEN s >= width THEN s ELSE s + ( width - LEN s ) * " " FI;

    PROC unit    = ( LONG REAL v, STRING s )MWRITER: ( v, "  " + s PAD 17 + ":" + fixed( v, -19, 15 ) + REPR 10 );
 
    PROC root    = ( LONG REAL v )MWRITER: unit( long sqrt( v ), "Took square root" );
    PROC add one = ( LONG REAL v )MWRITER: unit( v+1, "Added one" );
    PROC half    = ( LONG REAL v )MWRITER: unit( v/2, "Divided by two" );
 
    MWRITER mw2 := unit( 5, "Initial value" ) BIND root BIND add one BIND half;
    print( ( "The Golden Ratio is", fixed( value OF mw2, -19, 15 ), newline ) );
    print( ( newline, "This was derived as follows:-", newline ) );
    print( ( log OF mw2 ) )
END
```

```txt

The Golden Ratio is  1.618033988749895

This was derived as follows:-
  Initial value    :  5.000000000000000
  Took square root :  2.236067977499790
  Added one        :  3.236067977499790
  Divided by two   :  1.618033988749895

```



## AppleScript



More than a light-weight scripting language is really likely to need, but a way of stretching it a bit, and understanding its relationship to other languages. What AppleScript mainly lacks (apart from a well-developed library, and introspective records/dictionaries which know what keys/fields they have), is a coherent type of first class (and potentially anonymous) function. To get first class objects, we have to wrap 2nd class handlers in 1st class scripts.


```AppleScript
-- WRITER MONAD FOR APPLESCRIPT

-- How can we compose functions which take simple values as arguments
-- but return an output value which is paired with a log string ?

-- We can prevent functions which expect simple values from choking 
-- on log-wrapped output (from nested functions)
-- by writing Unit/Return() and Bind() for the Writer monad in AppleScript

on run {}
    
    -- Derive logging versions of three simple functions, pairing
    -- each function with a particular comment string
    
    -- (a -> b) -> (a -> (b, String))
    set wRoot to writerVersion(root, "obtained square root")
    set wSucc to writerVersion(succ, "added one")
    set wHalf to writerVersion(half, "divided by two")
    
    loggingHalfOfRootPlusOne(5)

    --> value + log string
end run


-- THREE SIMPLE FUNCTIONS
on root(x)
    x ^ (1 / 2)
end root

on succ(x)
    x + 1
end succ

on half(x)
    x / 2
end half

-- DERIVE A LOGGING VERSION OF A FUNCTION  BY COMBINING IT WITH A 
-- LOG STRING FOR THAT FUNCTION
-- (SEE 'on run()' handler at top of script)
-- (a -> b) -> String -> (a -> (b, String))
on writerVersion(f, strComment)
    script
        on call(x)
            {value:sReturn(f)'s call(x), comment:strComment}
        end call
    end script
end writerVersion


-- DEFINE A COMPOSITION OF THE SAFE VERSIONS
on loggingHalfOfRootPlusOne(x)
    logCompose([my wHalf, my wSucc, my wRoot], x)
end loggingHalfOfRootPlusOne


-- Monadic UNIT/RETURN and BIND functions for the writer monad
on writerUnit(a)
    try
        set strValue to ": " & a as string
    on error
        set strValue to ""
    end try
    {value:a, comment:"Initial value" & strValue}
end writerUnit

on writerBind(recWriter, wf)
    set recB to wf's call(value of recWriter)
    set v to value of recB
    
    try
        set strV to " -> " & (v as string)
    on error
        set strV to ""
    end try
    
    {value:v, comment:(comment of recWriter) & linefeed & (comment of recB) & strV}
end writerBind

-- THE TWO HIGHER ORDER FUNCTIONS ABOVE ENABLE COMPOSITION OF 
-- THE LOGGING VERSIONS OF EACH FUNCTION
on logCompose(lstFunctions, varValue)
    reduceRight(lstFunctions, writerBind, writerUnit(varValue))
end logCompose

-- xs: list, f: function, a: initial accumulator value
-- the arguments available to the function f(a, x, i, l) are
-- v: current accumulator value
-- x: current item in list
-- i: [ 1-based index in list ] optional
-- l: [ a reference to the list itself ] optional
on reduceRight(xs, f, a)
    set sf to sReturn(f)
    
    repeat with i from length of xs to 1 by -1
        set a to sf's call(a, item i of xs, i, xs)
    end repeat
end reduceRight

-- Unit/Return and bind for composing handlers in script wrappers
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
```


```txt
{
    value:1.61803398875,
    comment:"Initial value: 5\n
             obtained square root -> 2.2360679775\n
             added one -> 3.2360679775\n
             divided by two -> 1.61803398875"
}
```



## EchoLisp

Our monadic Writer elements will be pairs (string . value), where string is the log string.


```scheme

(define (Writer.unit x (log #f)) 
	(if log (cons log x)
		(cons (format "init → %d" x) x)))

;; f is a lisp function
;; (Writer.lift f) returns a  Writer function which returns a Writer element

(define (Writer.lift f name)     
	(lambda(elem) 
             (Writer.unit 
             	(f (rest elem)) 
                (format "%a \n %a  → %a" (first elem) name (f (rest elem))))))
                            
;; lifts and applies
(define (Writer.bind f elem) ((Writer.lift f (string f)) elem))

(define (Writer.print elem) (writeln 'result (rest elem)) (writeln (first elem)))
		
;; Writer monad versions
(define w-root  (Writer.lift sqrt "root"))
(define w-half  (Writer.lift (lambda(x) (// x 2)) "half"))
(define w-inc  ( Writer.lift add1 "add-one"))


;; no binding required, as we use Writer lifted functions
(->  5 Writer.unit w-root w-inc w-half Writer.print)

result 1.618033988749895    
init → 5
root → 2.23606797749979
add-one → 3.23606797749979
half → 1.618033988749895    

;; binding
(->>  0 Writer.unit (Writer.bind sin) (Writer.bind cos)  w-inc w-half Writer.print)

result 1    
init → 0
sin → 0
cos → 1
add-one → 2
half → 1    

```



## Factor

Factor comes with an implementation of Haskell-style monads in the <code>monads</code> vocabulary.
```factor
USING: kernel math math.functions monads prettyprint ;
FROM: monads => do ;

{
    [ 5 "Started with five, " <writer> ]
    [ sqrt "took square root, " <writer> ]
    [ 1 + "added one, " <writer> ]
    [ 2 / "divided by two." <writer> ]
} do .
```

```txt

T{ writer
    { value 1.618033988749895 }
    { log
        "Started with five, took square root, added one, divided by two."
    }
}

```



## Go

```go
package main

import (
    "fmt"
    "math"
)

type mwriter struct {
    value float64
    log   string
}

func (m mwriter) bind(f func(v float64) mwriter) mwriter {
    n := f(m.value)
    n.log = m.log + n.log
    return n
}

func unit(v float64, s string) mwriter {
    return mwriter{v, fmt.Sprintf("  %-17s: %g\n", s, v)}
}

func root(v float64) mwriter {
    return unit(math.Sqrt(v), "Took square root")
}

func addOne(v float64) mwriter {
    return unit(v+1, "Added one")
}

func half(v float64) mwriter {
    return unit(v/2, "Divided by two")
}

func main() {
    mw1 := unit(5, "Initial value")
    mw2 := mw1.bind(root).bind(addOne).bind(half)
    fmt.Println("The Golden Ratio is", mw2.value)
    fmt.Println("\nThis was derived as follows:-")
    fmt.Println(mw2.log)
}
```


```txt

The Golden Ratio is 1.618033988749895

This was derived as follows:-
  Initial value    : 5
  Took square root : 2.23606797749979
  Added one        : 3.23606797749979
  Divided by two   : 1.618033988749895

```



## Haskell


Haskell has the built-in <code>Monad</code> type class, and a built-in <code>Writer</code> monad (as well as the more general <code>WriterT</code> monad transformer that can make a writer monad with an underlying computation that is also a monad) already conforms to the <code>Monad</code> type class.

Making a logging version of functions (unfortunately, if we use the built-in writer monad we cannot get the values into the logs when binding):

```haskell
import Control.Monad.Trans.Writer
import Control.Monad ((>=>))

loggingVersion :: (a -> b) -> c -> a -> Writer c b
loggingVersion f log x = writer (f x, log)

logRoot = loggingVersion sqrt "obtained square root, "
logAddOne = loggingVersion (+1) "added 1, "
logHalf = loggingVersion (/2) "divided by 2, "

halfOfAddOneOfRoot = logRoot >=> logAddOne >=> logHalf

main = print $ runWriter (halfOfAddOneOfRoot 5)
```


```txt

(1.618033988749895,"obtained square root, added 1, divided by 2, ")

```



## J


Based on javascript implementation:


```J
root=: %:
incr=: >:
half=: -:

tostr=: ,@":

loggingVersion=: conjunction define
  n;~u
)

Lroot=: root loggingVersion 'obtained square root'
Lincr=: incr loggingVersion 'added 1'
Lhalf=: half loggingVersion 'divided by 2'

loggingUnit=: verb define
  y;'Initial value: ',tostr y
)

loggingBind=: adverb define
  r=. u 0{::y
  v=. 0{:: r
  v;(1{::y),LF,(1{::r),' -> ',tostr v 
)

loggingCompose=: dyad define
  ;(dyad def '<x`:6 loggingBind;y')/x,<loggingUnit y
)
```


Task example:


```J
   0{::Lhalf`Lincr`Lroot loggingCompose 5
1.61803
   1{::Lhalf`Lincr`Lroot loggingCompose 5
Initial value: 5
obtained square root -> 2.23607
added 1 -> 3.23607
divided by 2 -> 1.61803
```



## JavaScript



### ES5



```JavaScript
(function () {
    'use strict';

    // START WITH THREE SIMPLE FUNCTIONS

    // Square root of a number more than 0
    function root(x) {
        return Math.sqrt(x);
    }

    // Add 1
    function addOne(x) {
        return x + 1;
    }

    // Divide by 2
    function half(x) {
        return x / 2;
    }


    // DERIVE LOGGING VERSIONS OF EACH FUNCTION

    function loggingVersion(f, strLog) {
        return function (v) {
            return {
                value: f(v),
                log: strLog
            };
        }
    }

    var log_root = loggingVersion(root, "obtained square root"),

        log_addOne = loggingVersion(addOne, "added 1"),

        log_half = loggingVersion(half, "divided by 2");


    // UNIT/RETURN and BIND for the the WRITER MONAD

    // The Unit / Return function for the Writer monad:
    // 'Lifts' a raw value into the wrapped form
    // a -> Writer a
    function writerUnit(a) {
        return {
            value: a,
            log: "Initial value: " + JSON.stringify(a)
        };
    }

    // The Bind function for the Writer monad:
    // applies a logging version of a function
    // to the contents of a wrapped value
    // and return a wrapped result (with extended log)

    // Writer a -> (a -> Writer b) -> Writer b
    function writerBind(w, f) {
        var writerB = f(w.value),
            v = writerB.value;

        return {
            value: v,
            log: w.log + '\n' + writerB.log + ' -> ' + JSON.stringify(v)
        };
    }

    // USING UNIT AND BIND TO COMPOSE LOGGING FUNCTIONS

    // We can compose a chain of Writer functions (of any length) with a simple foldr/reduceRight
    // which starts by 'lifting' the initial value into a Writer wrapping,
    // and then nests function applications (working from right to left)
    function logCompose(lstFunctions, value) {
        return lstFunctions.reduceRight(
            writerBind,
            writerUnit(value)
        );
    }

    var half_of_addOne_of_root = function (v) {
        return logCompose(
            [log_half, log_addOne, log_root], v
        );
    };

    return half_of_addOne_of_root(5);
})();
```


```txt
{
    "value":1.618033988749895,
    "log":"Initial value: 5\n
           obtained square root -> 2.23606797749979\n
           added 1 -> 3.23606797749979\n
           divided by 2 -> 1.618033988749895"
}
```



## Jsish

From Javascript ES5 entry.


```javascript
'use strict';

/* writer monad, in Jsish */
function writerMonad() {
 
    // START WITH THREE SIMPLE FUNCTIONS
 
    // Square root of a number more than 0
    function root(x) {
        return Math.sqrt(x);
    }
 
    // Add 1
    function addOne(x) {
        return x + 1;
    }
 
    // Divide by 2
    function half(x) {
        return x / 2;
    }
 
 
    // DERIVE LOGGING VERSIONS OF EACH FUNCTION
 
    function loggingVersion(f, strLog) {
        return function (v) {
            return {
                value: f(v),
                log: strLog
            };
        };
    }
 
    var log_root = loggingVersion(root, "obtained square root"),
 
        log_addOne = loggingVersion(addOne, "added 1"),
 
        log_half = loggingVersion(half, "divided by 2");
 
 
    // UNIT/RETURN and BIND for the the WRITER MONAD
 
    // The Unit / Return function for the Writer monad:
    // 'Lifts' a raw value into the wrapped form
    // a -> Writer a
    function writerUnit(a) {
        return {
            value: a,
            log: "Initial value: " + JSON.stringify(a)
        };
    }
 
    // The Bind function for the Writer monad:
    // applies a logging version of a function
    // to the contents of a wrapped value
    // and return a wrapped result (with extended log)
 
    // Writer a -> (a -> Writer b) -> Writer b
    function writerBind(w, f) {
        var writerB = f(w.value),
            v = writerB.value;
 
        return {
            value: v,
            log: w.log + '\n' + writerB.log + ' -> ' + JSON.stringify(v)
        };
    }
 
    // USING UNIT AND BIND TO COMPOSE LOGGING FUNCTIONS
 
    // We can compose a chain of Writer functions (of any length) with a simple foldr/reduceRight
    // which starts by 'lifting' the initial value into a Writer wrapping,
    // and then nests function applications (working from right to left)
    function logCompose(lstFunctions, value) {
        return lstFunctions.reduceRight(
            writerBind,
            writerUnit(value)
        );
    }

    var half_of_addOne_of_root = function (v) {
        return logCompose(
            [log_half, log_addOne, log_root], v
        );
    };

    return half_of_addOne_of_root(5);
}

var writer = writerMonad();
;writer.value;
;writer.log;

/*
=!EXPECTSTART!=
writer.value ==> 1.61803398874989
writer.log ==> Initial value: 5
obtained square root -> 2.23606797749979
added 1 -> 3.23606797749979
divided by 2 -> 1.61803398874989
=!EXPECTEND!=
*/
```


```txt
prompt$ jsish -u writerMonad.jsi
[PASS] writerMonad.jsi
```



## Julia


```julia
struct Writer x::Real; msg::String; end

Base.show(io::IO, w::Writer) = print(io, w.msg, ": ", w.x)

unit(x, logmsg) = Writer(x, logmsg)

bind(f, fmsg, w) = unit(f(w.x), w.msg * ", " * fmsg)

f1(x) = 7x
f2(x) = x + 8

a = unit(3, "after intialization")
b = bind(f1, "after times 7 ", a)
c = bind(f2, "after plus 8", b)

println("$a => $b => $c")
println(bind(f2, "after plus 8", bind(f1, "after times 7", unit(3, "after intialization"))))

```
```txt

after intialization: 3 => after intialization, after times 7: 21 => after intialization, after times 7, after plus 8: 29
after intialization, after times 7, after plus 8: 29

```



## Kotlin


```scala
// version 1.2.10

import kotlin.math.sqrt

class Writer<T : Any> private constructor(val value: T, s: String) {
    var log = "  ${s.padEnd(17)}: $value\n"
        private set

    fun bind(f: (T) -> Writer<T>): Writer<T> {
        val new = f(this.value)
        new.log = this.log + new.log
        return new
    }

    companion object {
        fun <T : Any> unit(t: T, s: String) = Writer<T>(t, s)
    }
}

fun root(d: Double) = Writer.unit(sqrt(d), "Took square root")

fun addOne(d: Double) = Writer.unit(d + 1.0, "Added one")

fun half(d: Double) = Writer.unit(d / 2.0, "Divided by two")

fun main(args: Array<String>) {
    val iv = Writer.unit(5.0, "Initial value")
    val fv = iv.bind(::root).bind(::addOne).bind(::half)
    println("The Golden Ratio is ${fv.value}")
    println("\nThis was derived as follows:-\n${fv.log}")
}
```


```txt

The Golden Ratio is 1.618033988749895

This was derived as follows:-
  Initial value    : 5.0
  Took square root : 2.23606797749979
  Added one        : 3.23606797749979
  Divided by two   : 1.618033988749895

```



## zkl

```zkl
class Writer{
   fcn init(x){ var X=x, logText=Data(Void,"  init \U2192; ",x.toString()) }
   fcn unit(text)  { logText.append(text); self }
   fcn lift(f,name){ unit("\n  %s \U2192; %s".fmt(name,X=f(X))) }
   fcn bind(f,name){ lift.fp(f,name) }
   fcn toString{ "Result = %s\n%s".fmt(X,logText.text) }

   fcn root{ lift(fcn(x){ x.sqrt() },"root") }
   fcn half{ lift('/(2),"half") }
   fcn inc { lift('+(1),"inc") }
}
```


```zkl
Writer(5.0).root().inc().half().println();
```

```txt

Result = 1.61803
  init → 5
  root → 2.23607
  inc → 3.23607
  half → 1.61803

```


```zkl
w:=Writer(5.0);
Utils.Helpers.fcomp(w.half,w.inc,w.root)(w).println();  // half(inc(root(w)))
```

```txt

Result = 1.61803
  init → 5
  root → 2.23607
  inc → 3.23607
  half → 1.61803

```

Use bind to add functions to an existing Writer:

```zkl
w:=Writer(5.0); 
root,inc,half := w.bind(fcn(x){ x.sqrt() },"root"), w.bind('+(1),"+ 1"), w.bind('/(2),"/ 2");
root(); inc(); half(); w.println();
```

```txt

Result = 1.61803
  init → 5
  root → 2.23607
  + 1 → 3.23607
  / 2 → 1.61803

```

