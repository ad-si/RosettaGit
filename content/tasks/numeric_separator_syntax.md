+++
title = "Numeric separator syntax"
description = ""
date = 2019-09-20T00:25:19Z
aliases = []
[extra]
id = 22498
[taxonomies]
categories = ["task"]
tags = []
+++

Several programming languages allow separators in numerals in order to group digits together. 

## Task

Show the numeric separator syntax and describe its specification. E.g., what separators are eligible? Can there be multiple consecutive separators? What position can a separator be in? Etc.






## ALGOL 68

In Algol 68, spaces are not significant in identifiers or numeric literals. This allows spaces to be used as numeric separators.

Single or multiple spaces can be used as desired, it is not necessary to group the digits into blocks of three.

```algol68
BEGIN
    INT  a = 1 234 567;
    REAL b = 3      .    1 4159 26 5 359;
    print( ( whole( a, 0 ), "  ", fixed( b, - 14, 11 ), newline ) )
END

```

```txt

1234567   3.14159265359

```


## AWK


```AWK

# syntax: GAWK -f NUMERIC_SEPARATOR_SYNTAX.AWK
# converted from ALGOL 68
BEGIN {
# AWK lacks numeric separators but can be simulated using white space.
    a = 1 234 567
    b = 3  "."  1 4159 26 5 359
    print(a,b)
    exit(0)
}

```

```txt

1234567 3.14159265359

```



## Factor

Factor allows the comma <code>,</code> as a separator character in number literals.

```factor
USE: prettyprint

12,345 .   ! 12345

! commas may be used at arbitrary intervals
1,23,456,78910 .  ! 12345678910

! a comma at the beginning or end will parse as a word, likely causing an error
! ,123 .   ! No word named “,123” found in current vocabulary search path
! 123, .   ! No word named “123,” found in current vocabulary search path

! likewise, two commas in a row will parse as a word
! 1,,23 .   ! No word named “1,,23” found in current vocabulary search path

! There are no exceptions to which numbers may have separators
! binary/octal/decimal/hexadecimal integers and floats are supported
0b1,000,001 .   ! 65
-1,234e-4,5 .   ! -1.234e-42
0x1.4,4p3 .   ! 10.125

! as are ratios
45,2+1,1/43,2 .   ! 452+11/432
1,1/1,7 .   ! 11/17

! and complex numbers
C{ 5.225,312 2.0 } .   ! C{ 5.225312 2.0 }
```


If one desires to define a syntax for different separator rules, that is possible:

```factor
USING: lexer math.parser prettyprint sequences sets ;

<< SYNTAX: PN: scan-token "_" without string>number suffix! ; >>

! permissive numbers
PN: _1_2_3_ .   ! 123
PN: 1__234___567 .   ! 1234567
PN: 0b0___10.100001p3 .   ! 20.125
```


Since Factor's parser is exposed, one could even make changes to the number parser, obviating the need for parsing words.

```factor
USING: eval prettyprint ;

<<

"IN: math.parser.private
USE: combinators
: @pos-digit-or-punc ( i number-parse n char -- n/f )
    {
        { 95 [ [ @pos-digit ] require-next-digit ] }   ! normally 44
        { 43 [ ->numerator ] }
        { 47 [ ->denominator ] }
        { 46 [ ->mantissa ] }
        [ [ @pos-digit ] or-exponent ]
    } case ; inline" eval( -- )

>>

3_333_333 .   ! 3333333
```


<!-- == Free Pascal == -->
## Go

From version 1.13 (expected early September 2019), Go will support underscores as digit separators for numeric literals. An underscore may appear between any two digits or the literal prefix (0b, 0o, 0x) and the first digit.

All the examples in the Perl 6 entry should work and those which are syntax errors will also be syntax errors in Go. 


## Julia

Julia allows use of the underscore _ as a digit separator. 
The _ separator must be preceded and followed by a digit. 
Commas are not allowed in numeric literals. 


```julia
    
    julia> 2_9
    29
    
    julia> 2_9_9_0
    2990
    
    julia> 2_9_9.0_01
    299.001
    
    julia> 1._01
    ERROR: syntax: invalid numeric constant "1._"
    
    julia> -1_0
    -10
    
    julia> -_10
    ERROR: UndefVarError: _10 not defined
    Stacktrace:
     [1] top-level scope at none:0
    
    julia> 0x34_ff
    0x34ff

    julia> 0x_34ff
    ERROR: syntax: invalid numeric constant "0x_"

    julia> 10_000_000
    10000000
    
    julia> 10__000__000
    ERROR: UndefVarError: __000__000 not defined

```



## OCaml

Underscores can be used as separators in integer or floating-point literals, and they are ignored. Underscores can be in any position except at the beginning, and you can use consecutive underscores.
 

```ocaml
Printf.printf "%d\n" 1_2_3;; (* 123 *)
Printf.printf "%d\n" 0b1_0_1_0_1;; (* 21 *)
Printf.printf "%d\n" 0xa_bc_d;; (* 43981 *)
Printf.printf "%d\n" 12__34;; (* 1234 *)
Printf.printf "%f\n" 1_2_3_4.2_5;; (* 1234.250000 *)
Printf.printf "%f\n" 6.0_22e4;; (* 60220.000000 *)
Printf.printf "%f\n" 1234_.25;; (* 1234.250000 *)
Printf.printf "%f\n" 1234._25;; (* 1234.250000 *)
Printf.printf "%f\n" 1234.25_;; (* 1234.250000 *)
```



## Perl

Perl allows underscore as a grouping / separator character in numeric inputs, as long as you use it between digits, and you do not use two underscores in a row:
 

```perl
# Int
print 1_2_3, "\n";  # 123

# Binary Int
print 0b1_0_1_0_1, "\n"; # 21

# Hexadecimal Int
print 0xa_bc_d, "\n"; # 43981

# Rat
print 1_2_3_4.2_5, "\n"; # 1234.25

# Num
print 6.0_22e4, "\n"; # 60220
```



## Perl 6

Perl 6 allows underscore as a grouping / separator character in numeric inputs, though there are a few restrictions.
 

```perl6
# Any numeric input value may use an underscore as a grouping/separator character.
# May occur in nearly any position, in any* number. * See restrictions below.

# Int
say 1_2_3;  # 123

# Binary Int
say 0b1_0_1_0_1; # 21

# Hexadecimal Int
say 0xa_bc_d; # 43981

# Rat
say 1_2_3_4.2_5; # 1234.25

# Num
say 6.0_22e4; # 60220

# There are some restrictions on the placement.
# An underscore may not be on an edge boundary, or next to another underscore.
# The following are all syntax errors.

# say _1234.25;
# say 1234_.25;
# say 1234._25;
# say 1234.25_;
# say 12__34.25;
```



## Phix

Phix simply ignores underscores in numeric literals, however a leading underscore signifies a normal identifier, much like a123 or tmp2.

Commas are not allowed in numeric literals, since they delimit sequence elements, routine parameters, and such like, for example {1,2,3,4}.

```Phix
? 1_2_3          -- 123
--? _1234.25    -- undefined identifier _1234
? 0b1_0_1_0_1   -- 21
? 0b_1_0_1_0_1  -- 21
? 0xa_bc_d      -- 43981
? #_DEAD_BEEF_  -- 3735928559.0
? 0x_dead_beef  -- 3735928559.0
? 3.14_15_93    -- 3.141593
? 1_2_3_4.2_5   -- 1234.25
? 1234_.25      -- 1234.25
? 1234._25      -- 1234.25
? 1234.25_      -- 1234.25
? 12__34.25     -- 1234.25
? 6.0_22e4      -- 60220
```



## Python


The Syntax for separators in numbers, (numeric literals), is given [https://docs.python.org/3/reference/lexical_analysis.html#integer-literals here] in the Python documentation.

* The underscore, '_', is used as a separator.

* Single underscores can be used to separate digits or can occur after base specifiers.

* E.g. 100_000_000_000, 0x_dead_beef, 3.14_15_93


## Racket


Vanilla Racket does not have numeric separator syntax. However, it can be defined by users. A quick solution is to use <code>#%top</code>:


```racket
#lang racket

(require syntax/parse/define
         (only-in racket [#%top racket:#%top])
         (for-syntax racket/string))

(define-syntax-parser #%top
  [(_ . x)
   #:do [(define s (symbol->string (syntax-e #'x)))
         (define num (string->number (string-replace s "_" "")))]
   #:when num
   #`#,num]
  [(_ . x) #'(racket:#%top . x)])

1_234_567.89
1_234__567.89
```


```txt

1234567.89
1234567.89

```


In the above implementation of the syntax, <code>_</code> is the separator. It allows multiple consecutive separators, and allows the separator anywhere in the numeral (front, middle, and back).

Implementation details: any token with <code>_</code> is considered an identifier in vanilla Racket. If it's not defined already, it would be unbound. We therefore can define <code>#%top</code> to control these unbound identifiers: if the token is a number after removing <code>_</code>, expand it to that number.

If we wish to, for example, disallow multiple consecutive separators like <code>1_234__567.89</code>, we could do so easily:


```racket
#lang racket

(require syntax/parse/define
         (only-in racket [#%top racket:#%top])
         (for-syntax racket/string))

(define-syntax-parser #%top
  [(_ . x)
   #:do [(define s (symbol->string (syntax-e #'x)))
         (define num (string->number (string-replace s "_" "")))]
   #:when num
   (syntax-parse #'x
     [_ #:fail-when (string-contains? s "__") "invalid multiple consecutive separators"
        #`#,num])]
  [(_ . x) #'(racket:#%top . x)])

1_234_567.89
1_234__567.89
```


```txt

1_234__567.89: invalid multiple consecutive separators in: 1_234__567.89

```


A more complicated solution is to create a new language that changes Racket's reader. One approach is to adjust the readtable to recognize the new number literals so that we don't need to change the whole reader. While being slightly more complicated, this solution is better in a sense that <code>(read)</code> will also recognize the new number literals.


## REXX

The REXX language doesn't allow commas in decimal numbers   (for input),   commas are considered argument separators   (if used from within a program,    or as (passed/invoked) arguments from any program).

However, for   '''binary'''   and   '''hexadecimal''' numbers,   (multiple) blanks are allowed in appropriate places.


For   ''binary''   numbers,   blanks are allowed between groups of four binary digits.

For example: 
    '1101 1001'B
    '1101 1001'b
    "1111 0101 0011 0010"B
     '111 0101 1110'b       is the same as  '0111 0101 1110'b   


For   ''hexadecimal''   numbers,   blanks are allowed between pairs of hexadecimal digits.

For example:
    'de ad    be ef 'x
    "08 09 0A"X
    '789 cc'x               is the same as   '07 89 CC'x


For   ''decimal''   numbers,   blanks are allowed between the sign (if present) and the numeric part of the number.

Optional, blanks are allowed before the sign,   and also after the number. 

For example:
    + 4500
    -   1719


## Scala

Since Scala 2.13.0 it's stated in the Scala Language Specification that:
''"The digits of a numeric literal may be separated by arbitrarily many underscores for purposes of legibility."''
Let's see how its work in a Scala REPL session:

```Scala
Welcome to Scala 2.13.0 (Java HotSpot(TM) 64-Bit Server VM, Java 12.0.2).
Type in expressions for evaluation. Or try :help.

scala> // Integer Literals

scala> // Using _ as a digit separator (neither leading nor trailing) it can be placed anywhere in the number.

scala> 1_2_3
res0: Int = 123

scala> 0xa_bc_d
res1: Int = 43981

scala> 0x_dead_beef
res2: Int = -559038737

scala> 1_2_3_4.2_5
res3: Double = 1234.25

scala> 6.0_22e4
res4: Double = 60220.0

scala> 12__34.25
res5: Double = 1234.25

scala>
```



## Sidef

Sidef allows underscores as a separator character in numeric inputs.

```ruby
# Int
say 1_2_3;  # 123

# Binary Int
say 0b1_0_1_0_1; # 21

# Hexadecimal Int
say 0xa_bc_d; # 43981

# Rational
say 1_2_3_4.2_5; # 1234.25

# Rational in exponential notation
say 6.0_22e4; # 60220

# Apart from starting the number with an underscore, can be placed anywhere in the number.

say 1234_.25;       # 1234.25
say 1234._25;       # 1234.25
say 1234.25_;       # 1234.25
say 12__34.25;      # 1234.25
# say _1234.25;     # syntax error
```



## zkl


```zkl
For source code, integers and floats allow a "_" between digits (or trailing)
and completely ignores them: 
   1_000 == 1_000_ == 1_0_0_0 == 1__________000
   1_2.3_4 == 12.34
For hex, both "_" and "|" are allowed: 0x12|34
```


```zkl
For printing, the String.fmt method will add separators for %d (interger: ","), 
%f (float: ","), %x (hex: "|") and %2B (binary: "|").
"%,d  %,.0f  %,x  %,.2B".fmt(1234, 1234.0, 0x1234, 0x1234).println();
   --> "1,234  1,234  12|34  1|0010|0011|0100"
Each objects toString method has optional parameters to specify a separator 
and "column width". This method is called (by fmt) for the above tags.
```

