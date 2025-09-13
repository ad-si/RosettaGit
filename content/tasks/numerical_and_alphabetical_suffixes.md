+++
title = "Numerical and alphabetical suffixes"
description = ""
date = 2019-08-03T18:47:09Z
aliases = []
[extra]
id = 22048
[taxonomies]
categories = ["task"]
tags = []
+++

This task is about expressing numbers with an attached (abutted) suffix multiplier(s),   the suffix(es) could be:
::*   an alphabetic (named) multiplier which could be abbreviated
::*    metric  multiplier(s) which can be specified multiple times
::*        "binary"      multiplier(s) which can be specified multiple times
::*   explanation marks (<big>'''!'''</big>) which indicate a factorial or multifactorial


The (decimal) numbers can be expressed generally as:
              {<big>±</big>}   {digits}   {'''.'''}   {digits}
                                  ────── or ──────
              {<big>±</big>}   {digits}   {'''.'''}   {digits}   {'''E''' <small>or</small> '''e'''}   {<big>±</big>}   {digits}

where:
::*   numbers won't have embedded blanks   (contrary to the expaciated examples above where whitespace was used for readability)
::*   this task will only be dealing with decimal numbers,   both in the   ''mantissa''   and   ''exponent''
::*   <big>±</big>   indicates an optional plus or minus sign   (<big>'''+'''</big>   <small>or</small>   <big>'''-'''</big>)
::*   digits are the decimal digits   ('''0''' ──► '''9''')
::*   the digits can have comma(s) interjected to separate the   ''periods''   (thousands)   such as:   '''12,467,000'''
::*   '''.'''   is the decimal point, sometimes also called a   ''dot''
::*   '''e'''   or   '''E'''   denotes the use of decimal exponentiation   (a number multiplied by raising ten to some power)


This isn't a pure or perfect definition of the way we express decimal numbers,   but it should convey the intent for this task.

The use of the word   ''periods''   (thousands) is not meant to confuse, that word (as used above) is what the '''comma''' separates;

the groups of decimal digits are called ''periods'',   and in almost all cases, are groups of three decimal digits.

If an   '''e'''   or   '''E'''   is specified, there must be a legal number expressed before it,   and there must be a legal (exponent) expressed after it.

Also, there must be some digits expressed in all cases,   not just a sign and/or decimal point.

Superfluous signs, decimal points, exponent numbers, and zeros   need not be preserved.

I.E.:      
'''+7'''   '''007'''   '''7.00'''   '''7E-0'''   '''7E000'''   '''70e-1'''     could all be expressed as '''7'''

All numbers to be "expanded" can be assumed to be valid and there won't be a requirement to verify their validity.


;Abbreviated alphabetic suffixes to be supported   (where the capital letters signify the minimum abbreation that can be used):
      '''PAIRs'''         multiply the number by  '''2'''         (as in pairs of shoes or pants)
      '''SCOres'''        multiply the number by  '''20'''        (as '''3score''' would be '''60''')
      '''DOZens'''        multiply the number by  '''12'''
      '''GRoss'''         multiply the number by  '''144'''       (twelve dozen)
      '''GREATGRoss'''    multiply the number by  '''1,728'''     (a dozen gross)
      '''GOOGOLs'''       multiply the number by  '''10^100'''    (ten raised to the 100&sup>th</sup> power)


Note that the plurals are supported, even though they're usually used when expressing exact numbers   (She has 2 dozen eggs, and dozens of quavas)


;Metric suffixes to be supported   (whether or not they're officially sanctioned):
      '''K'''     multiply the number by  '''10^3'''              kilo      (1,000)
      '''M'''     multiply the number by  '''10^6'''              mega      (1,000,000)
      '''G'''     multiply the number by  '''10^9'''              giga      (1,000,000,000)
      '''T'''     multiply the number by  '''10^12'''             tera      (1,000,000,000,000)
      '''P'''     multiply the number by  '''10^15'''             peta      (1,000,000,000,000,000)
      '''E'''     multiply the number by  '''10^18'''             exa       (1,000,000,000,000,000,000)
      '''Z'''     multiply the number by  '''10^21'''             zetta     (1,000,000,000,000,000,000,000)
      '''Y'''     multiply the number by  '''10^24'''             yotta     (1,000,000,000,000,000,000,000,000)
      '''X'''     multiply the number by  '''10^27'''             xenta     (1,000,000,000,000,000,000,000,000,000)
      '''W'''     multiply the number by  '''10^30'''             wekta     (1,000,000,000,000,000,000,000,000,000,000)
      '''V'''     multiply the number by  '''10^33'''             vendeka   (1,000,000,000,000,000,000,000,000,000,000,000)
      '''U'''     multiply the number by  '''10^36'''             udekta    (1,000,000,000,000,000,000,000,000,000,000,000,000)


;Binary suffixes to be supported   (whether or not they're officially sanctioned):
      '''Ki'''    multiply the number by  '''2^10'''              kibi      (1,024)
      '''Mi'''    multiply the number by  '''2^20'''              mebi      (1,048,576)
      '''Gi'''    multiply the number by  '''2^30'''              gibi      (1,073,741,824)
      '''Ti'''    multiply the number by  '''2^40'''              tebi      (1,099,571,627,776)
      '''Pi'''    multiply the number by  '''2^50'''              pebi      (1,125,899,906,884,629)
      '''Ei'''    multiply the number by  '''2^60'''              exbi      (1,152,921,504,606,846,976)
      '''Zi'''    multiply the number by  '''2^70'''              zeb1      (1,180,591,620,717,411,303,424)
      '''Yi'''    multiply the number by  '''2^80'''              yobi      (1,208,925,819,614,629,174,706,176)
      '''Xi'''    multiply the number by  '''2^90'''              xebi      (1,237,940,039,285,380,274,899,124,224)
      '''Wi'''    multiply the number by  '''2^100'''             webi      (1,267,650,600,228,229,401,496,703,205,376)
      '''Vi'''    multiply the number by  '''2^110'''             vebi      (1,298,074,214,633,706,907,132,624,082,305,024)
      '''Ui'''    multiply the number by  '''2^120'''             uebi      (1,329,227,995,784,915,872,903,807,060,280,344,576)


All of the metric and binary suffixes can be expressed in   lowercase,   uppercase,   ''or''   mixed case.

All of the metric and binary suffixes can be   ''stacked''   (expressed multiple times),   and also be intermixed:

I.E.:       '''123k'''   '''123K'''   '''123GKi'''   '''12.3GiGG'''   '''12.3e-7T'''   '''.78E100e'''
 

;Factorial suffixes to be supported:
      '''!'''      compute the (regular) factorial product:   '''5!'''   is  '''5''' &times; '''4''' &times; '''3''' &times; '''2''' &times; '''1'''  =  '''120'''
      '''!!'''     compute the  double   factorial product:   '''8!'''   is  '''8''' &times; '''6''' &times; '''4''' &times; '''2'''      =  '''384'''
      '''!!!'''    compute the  triple   factorial product:   '''8!'''   is  '''8''' &times; '''5''' &times; '''2'''          =   '''80'''
      '''!!!!'''   compute the quadruple factorial product:   '''8!'''   is  '''8''' &times; '''4'''              =   '''32'''
      '''!!!!!'''  compute the quintuple factorial product:   '''8!'''   is  '''8''' &times; '''3'''              =   '''24'''
      ··· the number of factorial symbols that can be specified is to be unlimited   (as per what can be entered/typed) ···


Note that these factorial products aren't   ''super─factorials''   where (4!)!  would be  (24)!.

Factorial suffixes aren't, of course, the usual type of multipliers, but are used here in a similar vein.


Multifactorials aren't to be confused with   ''super─factorials''     where   '''(4!)!'''   would be   '''(24)!'''.



## Task

::*   Using the test cases (below),   show the "expanded" numbers here, on this page.
::*   For each list, show the input on one line,   and also show the output on one line.
::*   When showing the input line, keep the spaces (whitespace) and case (capitalizations) as is.
::*   For each result (list) displayed on one line, separate each number with two blanks.
::*   Add commas to the output numbers were appropriate.


;Test cases:
     2greatGRo   24Gros  288Doz  1,728pairs  172.8SCOre
     1,567      +1.567k    0.1567e-2m
     25.123kK    25.123m   2.5123e-00002G
     25.123kiKI  25.123Mi  2.5123e-00002Gi  +.25123E-7Ei
     -.25123e-34Vikki      2e-77gooGols
     9!   9!!   9!!!   9!!!!   9!!!!!   9!!!!!!   9!!!!!!!   9!!!!!!!!   9!!!!!!!!!

where the last number for the factorials has nine factorial symbols   (<big>'''!'''</big>)   after the   '''9'''



## Related tasks

:*   [[Multifactorial]]                 (which has a clearer and more succinct definition of multifactorials.)
:*   [[Factorial]]
:*   [[Abbreviations, simple]]
:*   [[Abbreviations, easy]]
:*   [[Abbreviations, automatic]]
:*   [[Commatizing numbers]]
:*   [[Longest common prefix]]





## Factor


### Functional


```factor
USING: combinators combinators.short-circuit formatting fry
grouping grouping.extras kernel literals math math.functions
math.parser math.ranges qw regexp sequences sequences.deep
sequences.extras sets splitting unicode ;
IN: rosetta-code.numerical-suffixes

CONSTANT: test-cases {
    qw{ 2greatGRo 24Gros 288Doz 1,728pairs 172.8SCOre }
    qw{ 1,567 +1.567k 0.1567e-2m }
    qw{ 25.123kK 25.123m 2.5123e-00002G }
    qw{ 25.123kiKI 25.123Mi 2.5123e-00002Gi +.25123E-7Ei }
    qw{ -.25123e-34Vikki 2e-77gooGols }
    qw{
        9! 9!! 9!!! 9!!!! 9!!!!! 9!!!!!! 9!!!!!!! 9!!!!!!!!
        9!!!!!!!!!
    }
}

CONSTANT: alpha {
    { "PAIRs" 2 } { "DOZens" 12 } { "SCOres" 20 }
    { "GRoss" 144 } { "GREATGRoss" 1,728 }
    ${ "GOOGOLs" 10 100 ^ }
}

CONSTANT: metric qw{ K M G T P E Z Y X W V U }

! Multifactorial
: m! ( n degree -- m ) neg 1 swap <range> product ;

! Separate a number from its suffix(es).
! e.g. "+1.567k" -> 1.567 "k"
: num/suffix ( str -- n suffix(es) )
    dup <head-clumps> <reversed> { } like "" map-like
    [ string>number ] map [ ] find [ tail* ] dip swap ;

! Checks whether str1 is an abbreviation of str2.
! e.g. "greatGRo" "GREATGRoss" -> t
: abbrev? ( str1 str2 -- ? )
    {
        [ [ >upper ] [ [ LETTER? ] take-while head? ] bi* ]
        [ [ length ] bi@ <= ]
    } 2&& ;

! Convert an alpha suffix to its multiplication function.
! e.g. "Doz" -> [ 12 * ]
: alpha>quot ( str -- quot )
    [ alpha ] dip '[ first _ swap abbrev? ] find nip second
    [ * ] curry ;

! Split a suffix composed of metric and binary suffixes into its
! constituent parts. e.g. "Vikki" -> { "Vi" "k" "ki" }
: split-compound ( str -- seq )
    R/ (.i|.)/i all-matching-subseqs ;

! Convert a metric or binary suffix to its multiplication
! function. e.g. "k" -> [ 10 3 ^ * ]
: suffix>quot ( str -- quot )
    dup [ [ 0 1 ] dip subseq >upper metric index 1 + ] dip
    length 1 = [ 3 * '[ 10 _ ^ * ] ] [ 10 * '[ 2 _ ^ * ] ] if ;

! Apply suffix>quot to each member of a sequence.
! e.g. { "Vi" "k" "ki" } ->
! [ [ 2 110 ^ * ] [ 10 3 ^ * ] [ 2 10 ^ * ] ]
: map-suffix ( seq -- seq' ) [ suffix>quot ] [ ] map-as ; 

! Tests whether a string is composed of metric and/or binary
! suffixes. e.g. "Vikki" -> t
: compound? ( str -- ? )
    >upper metric concat "I" append without empty? ;

! Convert a float to an integer if it is numerically equivalent
! to an integer. e.g. 1.0 -> 1, 1.23 -> 1.23
: ?f>i ( x -- y/n ) 
    dup >integer 2dup [ number= ] 2dip swap ? ;

! Convert a suffix string to a function that performs the
! calculations required by the suffix.
! e.g. "!!!" -> [ 3 m! ], "kiKI" -> [ 2 10 ^ * 2 10 ^ * ]
: parse-suffix ( str -- quot )
    {
        { [ dup empty? ] [ drop [ ] ] }
        { [ dup first CHAR: ! = ] [ length [ m! ] curry ] }
        { [ dup compound? ] [ split-compound map-suffix ] }
        [ alpha>quot ]
    } cond flatten ;

GENERIC: commas ( n -- str )

! Add commas to an integer in triplets.
! e.g. 1567 -> "1,567"
M: integer commas number>string <reversed> 3 group
    [ "," append ] map concat reverse rest ;

! Add commas to a float in triplets.
! e.g. 1567.12345 -> "1,567.12345"
M: float commas number>string "." split first2
    [ string>number commas ] dip "." glue ;

! Parse any number with any numerical or alphabetical suffix.
! e.g. "288Doz" -> "3,456", "9!!" -> "945"
: parse-alpha ( str -- str' )
    num/suffix parse-suffix curry call( -- x ) ?f>i commas ;

: main ( -- )
    test-cases [
        dup [ parse-alpha ] map
        "Numbers: %[%s, %]\n Result: %[%s, %]\n\n" printf
    ] each ;

MAIN: main
```

```txt

Numbers: { 2greatGRo, 24Gros, 288Doz, 1,728pairs, 172.8SCOre }
 Result: { 3,456, 3,456, 3,456, 3,456, 3,456 }

Numbers: { 1,567, +1.567k, 0.1567e-2m }
 Result: { 1,567, 1,567, 1,567 }

Numbers: { 25.123kK, 25.123m, 2.5123e-00002G }
 Result: { 25,123,000, 25,123,000, 25,123,000 }

Numbers: { 25.123kiKI, 25.123Mi, 2.5123e-00002Gi, +.25123E-7Ei }
 Result: { 26,343,374.848, 26,343,374.848, 26,975,615.844352, 28,964,846,960.23782 }

Numbers: { -.25123e-34Vikki, 2e-77gooGols }
 Result: { -33,394.19493810444, 199,999,999,999,999,983,222,784 }

Numbers: { 9!, 9!!, 9!!!, 9!!!!, 9!!!!!, 9!!!!!!, 9!!!!!!!, 9!!!!!!!!, 9!!!!!!!!! }
 Result: { 362,880, 945, 162, 45, 36, 27, 18, 9, 9 }

```



### EBNF

This solution uses Factor's extended Backus-Naur form (EBNF) language to define a grammar for parsing numerical/alphabetical suffix numbers. The goal was to describe as much of the suffix-number as possible in a declarative manner, minimizing the use of actions (Factor code that is run on a rule before being added to the abstract syntax tree) and helper functions. The biggest departure from this goal was to parse the metric/binary suffixes based on their index in a collection, as this method is less verbose than defining a rule for each suffix.

```factor
USING: formatting fry grouping kernel literals math
math.functions math.parser math.ranges multiline peg.ebnf
quotations qw sequences sequences.deep splitting strings unicode ;
IN: rosetta-code.numerical-suffixes.ebnf

CONSTANT: test-cases {
    qw{ 2greatGRo 24Gros 288Doz 1,728pairs 172.8SCOre }
    qw{ 1,567 +1.567k 0.1567e-2m }
    qw{ 25.123kK 25.123m 2.5123e-00002G }
    qw{ 25.123kiKI 25.123Mi 2.5123e-00002Gi +.25123E-7Ei }
    qw{ -.25123e-34Vikki 2e-77gooGols }
    qw{
        9! 9!! 9!!! 9!!!! 9!!!!! 9!!!!!! 9!!!!!!! 9!!!!!!!!
        9!!!!!!!!!
    }
}

CONSTANT: metric qw{ K M G T P E Z Y X W V U }

: suffix>quot ( str -- quot )
    dup [ [ 0 1 ] dip subseq >upper metric index 1 + ] dip
    length 1 = [ 3 * '[ 10 _ ^ * ] ] [ 10 * '[ 2 _ ^ * ] ] if ;

: ?f>i ( x -- y/n ) dup >integer 2dup [ number= ] 2dip swap ? ;

GENERIC: commas ( n -- str )
M: integer commas number>string <reversed> 3 group
    [ "," append ] map concat reverse rest ;

M: float commas number>string "." split first2
    [ string>number commas ] dip "." glue ;

EBNF: suffix-num [=[
  sign    = [+-]
  digit   = [0-9]
  triplet = digit digit digit
  commas  = (triplet | digit digit | digit) ([,] triplet)+
  integer = sign? (commas | digit+)
  exp     = [Ee] sign? digit+
  bfloat  = (integer | sign)? [.] digit+ exp?
  float   = (bfloat | integer exp)
  number  = (float | integer) => [[ flatten "" like string>number ]]
  pairs   = [Pp] [Aa] [Ii] [Rr] [s]?           => [[ [ 2 * ] ]]
  dozens  = [Dd] [Oo] [Zz] [e]? [n]? [s]?      => [[ [ 12 * ] ]]
  scores  = [Ss] [Cc] [Oo] [r]? [e]? [s]?      => [[ [ 20 * ] ]]
  gross   = [Gg] [Rr] [o]? [s]? [s]?           => [[ [ 144 * ] ]]
  gg      = [Gg] [Rr] [Ee] [Aa] [Tt] gross     => [[ [ 1728 * ] ]]
  googols = [Gg] [Oo] [Oo] [Gg] [Oo] [Ll] [s]? => [[ [ $[ 10 100 ^ ] * ] ]]
  alpha   = (pairs | dozens | scores | gg | gross | googols)
  numeric = ([KkMmGgPpEeT-Zt-z] [Ii]?) => [[ flatten "" like suffix>quot ]]
  ncompnd = numeric+
  fact    = [!]+ => [[ length [ neg 1 swap <range> product ] curry ]]
  suffix  = (alpha | ncompnd | fact)
  s-num   = number suffix? !(.) =>
            [[ >quotation flatten call( -- x ) ?f>i commas ]]
]=]

: num-alpha-suffix-demo ( -- )
    test-cases [
        dup [ suffix-num ] map
        "Numbers: %[%s, %]\n Result: %[%s, %]\n\n" printf
    ] each ;
 
MAIN: num-alpha-suffix-demo
```

```txt

Numbers: { 2greatGRo, 24Gros, 288Doz, 1,728pairs, 172.8SCOre }
 Result: { 3,456, 3,456, 3,456, 3,456, 3,456 }

Numbers: { 1,567, +1.567k, 0.1567e-2m }
 Result: { 1,567, 1,567, 1,567 }

Numbers: { 25.123kK, 25.123m, 2.5123e-00002G }
 Result: { 25,123,000, 25,123,000, 25,123,000 }

Numbers: { 25.123kiKI, 25.123Mi, 2.5123e-00002Gi, +.25123E-7Ei }
 Result: { 26,343,374.848, 26,343,374.848, 26,975,615.844352, 28,964,846,960.23782 }

Numbers: { -.25123e-34Vikki, 2e-77gooGols }
 Result: { -33,394.19493810444, 199,999,999,999,999,983,222,784 }

Numbers: { 9!, 9!!, 9!!!, 9!!!!, 9!!!!!, 9!!!!!!, 9!!!!!!!, 9!!!!!!!!, 9!!!!!!!!! }
 Result: { 362,880, 945, 162, 45, 36, 27, 18, 9, 9 }

```



## Go


```go
package main

import (
    "fmt"
    "math"
    "math/big"
    "strconv"
    "strings"
)

type minmult struct {
    min  int
    mult float64
}

var abbrevs = map[string]minmult{
    "PAIRs": {4, 2}, "SCOres": {3, 20}, "DOZens": {3, 12},
    "GRoss": {2, 144}, "GREATGRoss": {7, 1728}, "GOOGOLs": {6, 1e100},
}

var metric = map[string]float64{
    "K": 1e3, "M": 1e6, "G": 1e9, "T": 1e12, "P": 1e15, "E": 1e18,
    "Z": 1e21, "Y": 1e24, "X": 1e27, "W": 1e30, "V": 1e33, "U": 1e36,
}

var binary = map[string]float64{
    "Ki": b(10), "Mi": b(20), "Gi": b(30), "Ti": b(40), "Pi": b(50), "Ei": b(60),
    "Zi": b(70), "Yi": b(80), "Xi": b(90), "Wi": b(100), "Vi": b(110), "Ui": b(120),
}

func b(e float64) float64 {
    return math.Pow(2, e)
}

func googol() *big.Float {
    g1 := new(big.Float).SetPrec(500)
    g1.SetInt64(10000000000)
    g := new(big.Float)
    g.Set(g1)
    for i := 2; i <= 10; i++ {
        g.Mul(g, g1)
    }
    return g
}

func fact(num string, d int) int {
    prod := 1
    n, _ := strconv.Atoi(num)
    for i := n; i > 0; i -= d {
        prod *= i
    }
    return prod
}

func parse(number string) *big.Float {
    bf := new(big.Float).SetPrec(500)
    t1 := new(big.Float).SetPrec(500)
    t2 := new(big.Float).SetPrec(500)
    // find index of last digit
    var i int
    for i = len(number) - 1; i >= 0; i-- {
        if '0' <= number[i] && number[i] <= '9' {
            break
        }
    }
    num := number[:i+1]
    num = strings.Replace(num, ",", "", -1) // get rid of any commas
    suf := strings.ToUpper(number[i+1:])
    if suf == "" {
        bf.SetString(num)
        return bf
    }
    if suf[0] == '!' {
        prod := fact(num, len(suf))
        bf.SetInt64(int64(prod))
        return bf
    }
    for k, v := range abbrevs {
        kk := strings.ToUpper(k)
        if strings.HasPrefix(kk, suf) && len(suf) >= v.min {
            t1.SetString(num)
            if k != "GOOGOLs" {
                t2.SetFloat64(v.mult)
            } else {
                t2 = googol() // for greater accuracy
            }
            bf.Mul(t1, t2)
            return bf
        }
    }
    bf.SetString(num)
    for k, v := range metric {
        for j := 0; j < len(suf); j++ {
            if k == suf[j:j+1] {
                if j < len(suf)-1 && suf[j+1] == 'I' {
                    t1.SetFloat64(binary[k+"i"])
                    bf.Mul(bf, t1)
                    j++
                } else {
                    t1.SetFloat64(v)
                    bf.Mul(bf, t1)
                }
            }
        }
    }
    return bf
}

func commatize(s string) string {
    if len(s) == 0 {
        return ""
    }
    neg := s[0] == '-'
    if neg {
        s = s[1:]
    }
    frac := ""
    if ix := strings.Index(s, "."); ix >= 0 {
        frac = s[ix:]
        s = s[:ix]
    }
    le := len(s)
    for i := le - 3; i >= 1; i -= 3 {
        s = s[0:i] + "," + s[i:]
    }
    if !neg {
        return s + frac
    }
    return "-" + s + frac
}

func process(numbers []string) {
    fmt.Print("numbers =  ")
    for _, number := range numbers {
        fmt.Printf("%s  ", number)
    }
    fmt.Print("\nresults =  ")
    for _, number := range numbers {
        res := parse(number)
        t := res.Text('g', 50)
        fmt.Printf("%s  ", commatize(t))
    }
    fmt.Println("\n")
}

func main() {
    numbers := []string{"2greatGRo", "24Gros", "288Doz", "1,728pairs", "172.8SCOre"}
    process(numbers)

    numbers = []string{"1,567", "+1.567k", "0.1567e-2m"}
    process(numbers)

    numbers = []string{"25.123kK", "25.123m", "2.5123e-00002G"}
    process(numbers)

    numbers = []string{"25.123kiKI", "25.123Mi", "2.5123e-00002Gi", "+.25123E-7Ei"}
    process(numbers)

    numbers = []string{"-.25123e-34Vikki", "2e-77gooGols"}
    process(numbers)

    numbers = []string{"9!", "9!!", "9!!!", "9!!!!", "9!!!!!", "9!!!!!!",
        "9!!!!!!!", "9!!!!!!!!", "9!!!!!!!!!"}
    process(numbers)
}
```


```txt

numbers =  2greatGRo  24Gros  288Doz  1,728pairs  172.8SCOre  
results =  3,456  3,456  3,456  3,456  3,456  

numbers =  1,567  +1.567k  0.1567e-2m  
results =  1,567  1,567  1,567  

numbers =  25.123kK  25.123m  2.5123e-00002G  
results =  25,123,000  25,123,000  25,123,000  

numbers =  25.123kiKI  25.123Mi  2.5123e-00002Gi  +.25123E-7Ei  
results =  26,343,374.848  26,343,374.848  26,975,615.844352  28,964,846,960.237816578048  

numbers =  -.25123e-34Vikki  2e-77gooGols  
results =  -33,394.194938104441474962344775423096782848  200,000,000,000,000,000,000,000  

numbers =  9!  9!!  9!!!  9!!!!  9!!!!!  9!!!!!!  9!!!!!!!  9!!!!!!!!  9!!!!!!!!!  
results =  362,880  945  162  45  36  27  18  9  9 

```




## Julia


```julia
using Formatting

partialsuffixes = Dict("PAIR" => "PAIRS", "SCO" => "SCORES", "DOZ" => "DOZENS",
                    "GR" => "GROSS", "GREATGR" => "GREATGROSS", "GOOGOL" => "GOOGOLS")
partials = sort(collect(keys(partialsuffixes)), lt=(a,b)->length(a)<length(b), rev=true)

multicharsuffixes = Dict("PAIRS" => 2, "SCORES" => 20, "DOZENS" => 12, "GROSS" => 144,
                         "GREATGROSS" => 1728, "GOOGOLS" => BigInt(10)^100)

twocharsuffixes = Dict(
    "KI" => BigInt(2)^10, "MI" => BigInt(2)^20, "GI" => BigInt(2)^30, "TI" => BigInt(2)^40,
    "PI" => BigInt(2)^50, "EI" => BigInt(2)^60, "ZI" => BigInt(2)^70, "YI" => BigInt(2)^80,
    "XI" => BigInt(2)^90, "WI" => BigInt(2)^100, "VI" => BigInt(2)^110, "UI" => BigInt(2)^120)
twosuff = collect(keys(twocharsuffixes))

onecharsuffixes = Dict("K" => 10^3, "M" => 10^6, "G" => 10^9, "T" => 10^12, "P" => 10^15,
                       "E" => 10^18, "Z" => 10^21, "Y" => BigInt(10)^24,
                       "X" => BigInt(10)^27, "W" => BigInt(10)^30,
                       "V" => BigInt(10)^33, "U" => BigInt(10)^36)
onesuff = collect(keys(onecharsuffixes))

function firstsuffix(s, x)
    str = uppercase(s)
    if str[1] == '!'
        lastbang = something(findfirst(x -> x != '!', str), length(str))
        return prod(x:-lastbang:1) / x, lastbang
    end
    for pstr in partials
        if match(Regex("^" * pstr), str) != nothing
            fullsuffix = partialsuffixes[pstr]
            n = length(pstr)
            while n < length(fullsuffix) && n < length(str) && fullsuffix[n+1] == str[n+1]
                n += 1
            end
            return BigInt(multicharsuffixes[fullsuffix]), n
        end
    end
    for pstr in twosuff
        if match(Regex("^" * pstr), str) != nothing
            return BigInt(twocharsuffixes[pstr]), 2
        end
    end
    for pstr in onesuff
        if match(Regex("^" * pstr), str) != nothing
            return BigInt(onecharsuffixes[pstr]), 1
        end
    end
    return 1, length(s)
end

function parsesuffix(s, x)
    str = s
    mult = BigInt(1)
    n = 1
    while n <= length(str)
        multiplier, n = firstsuffix(str, x)
        mult *= multiplier
        str = str[n+1:end]
    end
    mult
end

function suffixednumber(s)
    if (endnum = findlast(isdigit, s)) == nothing
        return NaN
    end
    x = BigFloat(replace(s[1:endnum], "," => ""))
    return x * (endnum < length(s) ? parsesuffix(s[endnum + 1:end], x) : 1)
end

const testcases =
["2greatGRo   24Gros  288Doz  1,728pairs  172.8SCOre",
 "1,567      +1.567k    0.1567e-2m",
 "25.123kK    25.123m   2.5123e-00002G",
 "25.123kiKI  25.123Mi  2.5123e-00002Gi  +.25123E-7Ei",
 "-.25123e-34Vikki      2e-77gooGols",
 "9!   9!!   9!!!   9!!!!   9!!!!!   9!!!!!!   9!!!!!!!   9!!!!!!!!   9!!!!!!!!!"]

function testsuffixes()
    for line in testcases
        cases = split(line)
        println("Testing: ", string.(cases))
        println("Results: ", join(map(x -> format(suffixednumber(x), commas=true), cases), "  "), "\n")
    end
end

testsuffixes()

```
```txt

Testing: ["2greatGRo", "24Gros", "288Doz", "1,728pairs", "172.8SCOre"]
Results: 3,456  3,456  3,456  3,456  3,456

Testing: ["1,567", "+1.567k", "0.1567e-2m"]
Results: 1,567  1,567  1,567

Testing: ["25.123kK", "25.123m", "2.5123e-00002G"]
Results: 25,123,000  25,123,000  25,123,000

Testing: ["25.123kiKI", "25.123Mi", "2.5123e-00002Gi", "+.25123E-7Ei"]
Results: 26,343,374.848  26,343,374.848  26,975,615.844352  28,964,846,960.237817

Testing: ["-.25123e-34Vikki", "2e-77gooGols"]
Results: -33,394.194938  200,000,000,000,000,000,000,000

Testing: ["9!", "9!!", "9!!!", "9!!!!", "9!!!!!", "9!!!!!!", "9!!!!!!!", "9!!!!!!!!", "9!!!!!!!!!"]
Results: 362,880  945  162  45  36  27  18  9  9

```



## Perl 6

Scientific notation, while supported in Perl 6, is limited to IEEE-754 64bit accuracy so there is some rounding on values using it. Implements a custom "high precision" conversion routine.

Unfortunately, this suffix routine is of limited use for practical everyday purposes. It focuses on handling excessively large and archaic units (googol, greatgross) and completely ignores or makes unusable (due to forcing case insensitivity) many common current ones: c(centi), m(milli), μ(micro). Ah well.

Note: I am blatantly and deliberately ignoring the task guidelines for formatting the output. It has no bearing on the core of the task. If you really, ''really'','' '''REALLY''' ''want to see badly formatted output, uncomment the last line. 


```perl6
use Rat::Precise;

my $googol = 10**100;
«PAIRs 2 SCOres 20 DOZens 12 GRoss 144  GREATGRoss 1728 GOOGOLs $googol»
  ~~ m:g/ ((<.:Lu>+) <.:Ll>*) \s+ (\S+) /;

my %abr = |$/.map: {
    my $abbrv = .[0].Str.fc;
    my $mag   = +.[1];
    |map { $abbrv.substr( 0, $_ ) => $mag },
    .[0][0].Str.chars .. $abbrv.chars
}

my %suffix = flat %abr,
(<K  M  G  T  P  E  Z  Y  X  W  V  U>».fc  Z=> (1000, * * 1000 … *)),
(<Ki Mi Gi Ti Pi Ei Zi Yi Xi Wi Vi Ui>».fc Z=> (1024, * * 1024 … *));

my $reg = %suffix.keys.join('|');

sub comma ($i is copy) {
    my $s = $i < 0 ?? '-' !! '';
    my ($whole, $frac) = $i.split('.');
    $frac = $frac.defined ?? ".$frac" !! '';
    $s ~ $whole.abs.flip.comb(3).join(',').flip ~ $frac
}

sub units (Str $str) {
    $str.fc ~~ /^(.+?)(<alpha>*)('!'*)$/;
    my ($val, $unit, $fact) = $0, $1.Str.fc, $2.Str;
    $val.=subst(',', '', :g);
    if $val ~~ m:i/'e'/ {
        my ($m,$e) = $val.split(/<[eE]>/);
        $val = ($e < 0)
            ?? $m * FatRat.new(1,10**-$e)
            !! $m * 10**$e;
    }
    my @suf = $unit;
    unless %suffix{$unit}:exists {
        $unit ~~ /(<$reg>)+/;
        @suf = $0;
    }
    my $ret = $val<>;
    $ret = [*] $ret, |@suf.map: { %suffix{$_} } if @suf[0];
    $ret = [*] ($ret, * - $fact.chars …^ * < 2) if $fact.chars;
    $ret.?precise // $ret
}

my $test = q:to '===';
2greatGRo   24Gros  288Doz  1,728pairs  172.8SCOre
1,567      +1.567k    0.1567e-2m
25.123kK    25.123m   2.5123e-00002G
25.123kiKI  25.123Mi  2.5123e-00002Gi  +.25123E-7Ei
-.25123e-34Vikki      2e-77gooGols
9!   9!!   9!!!   9!!!!   9!!!!!   9!!!!!!   9!!!!!!!   9!!!!!!!!   9!!!!!!!!!
.017k!!
===

printf "%16s: %s\n", $_, comma .&units for $test.words;

# Task required stupid layout
# say "\n In: $_\nOut: ", .words.map({comma .&units}).join('  ') for $test.lines;
```

```txt
       2greatGRo: 3,456
          24Gros: 3,456
          288Doz: 3,456
      1,728pairs: 3,456
      172.8SCOre: 3,456
           1,567: 1,567
         +1.567k: 1,567
      0.1567e-2m: 1,567
        25.123kK: 25,123,000
         25.123m: 25,123,000
  2.5123e-00002G: 25,123,000
      25.123kiKI: 26,343,374.848
        25.123Mi: 26,343,374.848
 2.5123e-00002Gi: 26,975,615.844352
    +.25123E-7Ei: 28,964,846,960.237816578048
-.25123e-34Vikki: -33,394.194938104441474962344775423096782848
    2e-77gooGols: 200,000,000,000,000,000,000,000
              9!: 362,880
             9!!: 945
            9!!!: 162
           9!!!!: 45
          9!!!!!: 36
         9!!!!!!: 27
        9!!!!!!!: 18
       9!!!!!!!!: 9
      9!!!!!!!!!: 9
         .017k!!: 34,459,425
```



## Phix

Using bigatom to get the full precision needed for the tests (esp the "Vikki" one).

Note that comma-insertion was added to ba_sprintf() in version 0.8.0.

```Phix
include builtins/bigatom.e
{} = ba_scale(34)   -- (min rqd for accuracy on the "Vikki" test)
                    -- (the default is 25, not quite enough here)

constant suffixes = {{"GREATGRoss",7,1728},
                     {"GOOGOLs",6,ba_new("1e100")},
                     {"SCOres",3,20},
                     {"DOZens",3,12},
                     {"GRoss",2,144},
                     {"PAIRs",4,2},
                     "KMGTPEZYXWVU"}

function decode(string suffix)
    bigatom res = BA_ONE
    suffix = upper(suffix)
    while length(suffix)>0 do
        bool found = false
        for i=length(suffix) to 2 by -1 do
            for s=1 to length(suffixes)-1 do
                if i<=length(suffixes[s][1])
                and i>=suffixes[s][2]
                and suffix[1..i]=upper(suffixes[s][1][1..i]) then
                    res = ba_mul(res,suffixes[s][3])
                    suffix = suffix[i+1..$]
                    found = true
                    exit
                end if
            end for
            if found then exit end if
        end for
        if not found then
            integer k = find(suffix[1],suffixes[$])
            if k=0 then ?9/0 end if
            if length(suffix)>=2 and suffix[2]='I' then
                res = ba_mul(res,ba_power(1024,k))
                suffix = suffix[3..$]
            else
                res = ba_mul(res,ba_power(1000,k))
                suffix = suffix[2..$]
            end if  
        end if
    end while
    return res
end function

function facto(bigatom n, integer f)
    if f!=0 then
        bigatom nf = ba_sub(n,f)
        while ba_compare(nf,2)>=0 do
            n = ba_mul(n,nf)
            nf = ba_sub(nf,f)
        end while
    end if
    return n
end function

constant test_cases = """
2greatGRo  24Gros  288Doz  1,728pairs  172.8SCOre
1,567  +1.567k  0.1567e-2m
25.123kK  25.123m  2.5123e-00002G
25.123kiKI  25.123Mi  2.5123e-00002Gi  +.25123E-7Ei
-.25123e-34Vikki  2e-77gooGols
9! 9!! 9!!! 9!!!! 9!!!!! 9!!!!!! 9!!!!!!! 9!!!!!!!! 9!!!!!!!!!
0.017k!!
"""
constant tests = split(substitute(test_cases,"\n"," "),no_empty:=true)

for i=1 to length(tests) do
    string test = tests[i], suffix = "", facts = ""
    for f=length(test) to 1 by -1 do
        if test[f]!='!' then
            {test,facts} = {test[1..f],test[f+1..$]}
            exit
        end if
    end for
    for d=length(test) to 1 by -1 do
        integer digit = test[d]
        if digit>='0' and digit<='9' then
            {test,suffix} = {test[1..d],test[d+1..$]}
            exit
        end if
    end for
    bigatom n = ba_new(substitute(test,",",""))
    n = ba_mul(n,decode(suffix))
    n = facto(n,length(facts))
    string ns = ba_sprintf("%,B",n)
    printf(1,"%30s : %s\n",{tests[i],ns})
end for
```

```txt

                     2greatGRo : 3,456
                        24Gros : 3,456
                        288Doz : 3,456
                    1,728pairs : 3,456
                    172.8SCOre : 3,456
                         1,567 : 1,567
                       +1.567k : 1,567
                    0.1567e-2m : 1,567
                      25.123kK : 25,123,000
                       25.123m : 25,123,000
                2.5123e-00002G : 25,123,000
                    25.123kiKI : 26,343,374.848
                      25.123Mi : 26,343,374.848
               2.5123e-00002Gi : 26,975,615.844352
                  +.25123E-7Ei : 28,964,846,960.237816578048
              -.25123e-34Vikki : -33,394.194938104441474962344775423096782848
                  2e-77gooGols : 200,000,000,000,000,000,000,000
                            9! : 362,880
                           9!! : 945
                          9!!! : 162
                         9!!!! : 45
                        9!!!!! : 36
                       9!!!!!! : 27
                      9!!!!!!! : 18
                     9!!!!!!!! : 9
                    9!!!!!!!!! : 9
                      0.017k!! : 34,459,425

```



## Python

```Python

from functools import reduce
from operator import mul
from decimal import *

getcontext().prec = MAX_PREC

def expand(num):
    suffixes = [
        #     (name, min_abbreviation_length, base, exponent)
        ('greatgross', 7, 12, 3),
        ('gross', 2, 12, 2),
        ('dozens', 3, 12, 1),
        ('pairs', 4, 2, 1),
        ('scores', 3, 20, 1),
        ('googols', 6, 10, 100),
        ('ki', 2, 2, 10),
        ('mi', 2, 2, 20),
        ('gi', 2, 2, 30),
        ('ti', 2, 2, 40),
        ('pi', 2, 2, 50),
        ('ei', 2, 2, 60),
        ('zi', 2, 2, 70),
        ('yi', 2, 2, 80),
        ('xi', 2, 2, 90),
        ('wi', 2, 2, 100),
        ('vi', 2, 2, 110),
        ('ui', 2, 2, 120),
        ('k', 1, 10, 3),
        ('m', 1, 10, 6),
        ('g', 1, 10, 9),
        ('t', 1, 10, 12),
        ('p', 1, 10, 15),
        ('e', 1, 10, 18),
        ('z', 1, 10, 21),
        ('y', 1, 10, 24),
        ('x', 1, 10, 27),
        ('w', 1, 10, 30)
    ]

    num = num.replace(',', '').strip().lower()

    if num[-1].isdigit():
        return float(num)

    for i, char in enumerate(reversed(num)):
        if char.isdigit():
            input_suffix = num[-i:]
            num = Decimal(num[:-i])
            break

    if input_suffix[0] == '!':
        return reduce(mul, range(int(num), 0, -len(input_suffix)))

    while len(input_suffix) > 0:
        for suffix, min_abbrev, base, power in suffixes:
            if input_suffix[:min_abbrev] == suffix[:min_abbrev]:
                for i in range(min_abbrev, len(input_suffix) + 1):
                    if input_suffix[:i+1] != suffix[:i+1]:
                        num *= base ** power
                        input_suffix = input_suffix[i:]
                        break
                break

    return num


test = "2greatGRo   24Gros  288Doz  1,728pairs  172.8SCOre\n\
        1,567      +1.567k    0.1567e-2m\n\
        25.123kK    25.123m   2.5123e-00002G\n\
        25.123kiKI  25.123Mi  2.5123e-00002Gi  +.25123E-7Ei\n\
        -.25123e-34Vikki      2e-77gooGols\n\
        9!   9!!   9!!!   9!!!!   9!!!!!   9!!!!!!   9!!!!!!!   9!!!!!!!!   9!!!!!!!!!"

for test_line in test.split("\n"):
    test_cases = test_line.split()
    print("Input:", ' '.join(test_cases))
    print("Output:", ' '.join(format(result, ',f').strip('0').strip('.') for result in map(expand, test_cases)))

```

```txt

Input: 2greatGRo 24Gros 288Doz 1,728pairs 172.8SCOre
Output: 3,456 3,456 3,456 3,456 3,456
Input: 1,567 +1.567k 0.1567e-2m
Output: 1,567 1,567 1,567
Input: 25.123kK 25.123m 2.5123e-00002G
Output: 25,123,000 25,123,000 25,123,000
Input: 25.123kiKI 25.123Mi 2.5123e-00002Gi +.25123E-7Ei
Output: 26,343,374.848 26,343,374.848 26,975,615.844352 28,964,846,960.237816578048
Input: -.25123e-34Vikki 2e-77gooGols
Output: -33,394.194938104441474962344775423096782848 200,000,000,000,000,000,000,000
Input: 9! 9!! 9!!! 9!!!! 9!!!!! 9!!!!!! 9!!!!!!! 9!!!!!!!! 9!!!!!!!!!
Output: 362,880 945 162 45 36 27 18 9 9

```



## REXX


```rexx
/*REXX pgm converts numbers (with commas) with suffix multipliers──►pure decimal numbers*/
numeric digits 2000                              /*allow the usage of ginormous numbers.*/
@.=; @.1= '2greatGRo   24Gros  288Doz  1,728pairs  172.8SCOre'
     @.2= '1,567      +1.567k    0.1567e-2m'
     @.3= '25.123kK    25.123m   2.5123e-00002G'
     @.4= '25.123kiKI  25.123Mi  2.5123e-00002Gi  +.25123E-7Ei'
     @.5= '-.25123e-34Vikki      2e-77gooGols'
     @.6=  9!   9!!   9!!!   9!!!!   9!!!!!   9!!!!!!   9!!!!!!!   9!!!!!!!!   9!!!!!!!!!

parse arg x                                      /*obtain optional arguments from the CL*/
if x\==''  then do;    @.2=;     @.1=x           /*use the number(s) specified on the CL*/
                end                              /*allow user to specify their own list.*/
                                                 /* [↓]  handle a list or multiple lists*/
    do  n=1  while @.n\=='';     $=              /*process each of the numbers in lists.*/
    say 'numbers= '      @.n                     /*echo the original arg to the terminal*/

        do j=1  for words(@.n);  y= word(@.n, j) /*obtain a single number from the input*/
        $= $  ' 'commas( num(y) )                /*process a number; add result to list.*/
        end   /*j*/                              /* [↑]  add commas to number if needed.*/
                                                 /* [↑]  add extra blank betweenst nums.*/
    say ' result= '      strip($);   say         /*echo the result(s) to the terminal.  */
    end       /*n*/                              /* [↑]  elide the  pre-pended  blank.  */
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
isInt:  return datatype( arg(1), 'Whole')        /*return 1 if arg is an integer,  or 0 */
isNum:  return datatype( arg(1), 'Number')       /*   "   "  "  "   " a  number.    " " */
p:      return word( arg(1), 1)                  /*pick 1st argument  or  2nd argument. */
ser:    say;   say '***error*** '  arg(1);           say;            exit 13
shorten:procedure; parse arg a,n;      return left(a,  max(0, length(a) - p(n 1) ) )
/*──────────────────────────────────────────────────────────────────────────────────────*/
$fact!: procedure; parse arg x _ .;    L= length(x);    n= L - length(strip(x, 'T', "!") )
        if n<=-n | _\=='' | arg()\==1  then return x;   z= left(x, L - n)
        if z<0 | \isInt(z)             then return x;   return $fact(z, n)
/*──────────────────────────────────────────────────────────────────────────────────────*/
$fact:  procedure; parse arg x _ .;  arg ,n ! .;     n= p(n 1);    if \isInt(n)  then n= 0
        if x<-n | \isInt(x) |n<1 | _||!\=='' |arg()>2  then return x||copies("!",max(1,n))
        s= x // n;   if s==0  then s= n          /*compute where to start multiplying.  */
        != 1                                     /*the initial factorial product so far.*/
                   do j=s  to x  by n;   != !*j  /*perform the actual factorial product.*/
                   end   /*j*/                   /*{operator  //  is REXX's ÷ remainder}*/
        return !                                 /* [↑]  handles any level of factorial.*/
/*──────────────────────────────────────────────────────────────────────────────────────*/
$sfxa:  parse arg u,s 1 c,m;   upper u c         /*get original version & upper version.*/
        if pos( left(s, 2), u)\==0  then do j=length(s)   to compare(s, c)-1   by -1
                                         if right(u, j) \== left(c, j)  then iterate
                                         _= left(u, length(u) - j)        /*get the num.*/
                                         if isNum(_)  then return m * _   /*good suffix.*/
                                         leave                            /*return as is*/
                                         end
        return arg(1)                            /* [↑]  handles an alphabetic suffixes.*/
/*──────────────────────────────────────────────────────────────────────────────────────*/
$sfx!:  parse arg y;                     if right(y, 1)=='!'  then y= $fact!(y)
        if \isNum(y)  then y=$sfxz();    if isNum(y)  then return y;       return $sfxm(y)
/*──────────────────────────────────────────────────────────────────────────────────────*/
$sfxm:  parse arg z 1 w;     upper w;    @= 'KMGTPEZYXWVU';                       b= 1000
        if right(w, 1)=='I'  then do;    z= shorten(z);      w= z;    upper w;    b= 1024
                                  end
        _= pos( right(w, 1), @);         if _==0      then return arg(1)
        n= shorten(z);  r= num(n, , 1);  if isNum(r)  then return r * b**_
        return arg(1)                            /* [↑]  handles metric or binary suffix*/
/*──────────────────────────────────────────────────────────────────────────────────────*/
$sfxz:  return $sfxa( $sfxa( $sfxa( $sfxa( $sfxa( $sfxa(y, 'PAIRs', 2),   'DOZens', 12), ,
           'SCores', 20),   'GREATGRoss',  1728),     'GRoss', 144),     'GOOGOLs', 1e100)
/*──────────────────────────────────────────────────────────────────────────────────────*/
commas: procedure;  parse arg _;    n= _'.9';      #= 123456789;      b= verify(n, #, "M")
        e= verify(n, #'0', ,   verify(n, #"0.", 'M') )  -  4         /* [↑]  add commas.*/
           do j=e  to b  by -3;   _= insert(',', _, j);     end  /*j*/;           return _
/*──────────────────────────────────────────────────────────────────────────────────────*/
num:    procedure; parse arg x .,,q;         if x==''  then return x
        if  isNum(x)  then return  x/1;      x= space( translate(x, , ','), 0)
        if \isNum(x)  then x= $sfx!(x);      if isNum(x)  then return x/1
        if q==1  then return x
        if q==''  then call ser "argument isn't numeric or doesn't have a legal suffix:" x
```

```txt

numbers=  2greatGRo   24Gros  288Doz  1,728pairs  172.8SCOre
 result=  3,456  3,456  3,456  3,456  3,456

numbers=  1,567      +1.567k    0.1567e-2m
 result=  1,567  1,567  1,567

numbers=  25.123kK    25.123m   2.5123e-00002G
 result=  25,123,000  25,123,000  25,123,000

numbers=  25.123kiKI  25.123Mi  2.5123e-00002Gi  +.25123E-7Ei
 result=  26,343,374.848  26,343,374.848  26,975,615.844352  28,964,846,960.237816578048

numbers=  -.25123e-34Vikki      2e-77gooGols
 result=  -33,394.194938104441474962344775423096782848  200,000,000,000,000,000,000,000

numbers=  9! 9!! 9!!! 9!!!! 9!!!!! 9!!!!!! 9!!!!!!! 9!!!!!!!! 9!!!!!!!!!
 result=  362,880  945  162  45  36  27  18  9  9

```



## zkl

{{libheader|GMP}} GNU Multiple Precision Arithmetic Library (big ints)
Floats are limited to 64 bit IEEE754.
Error checking is nonexistent.

```zkl
var [const] BI=Import.lib("zklBigNum");  // GMP
var kRE,kD, aRE,aD;

kRE,kD = ki();
aRE,aD = abrevCreate();

fcn naSuffixes(numStr){ 
   var [const]
     numRE=RegExp(0'^([+-]*\.*\d+[.]*\d*E*[+-]*\d*)^),
     bangRE=RegExp(0'^(!+)^);

   nstr:=(numStr - " ,").toUpper();
   numRE.search(nstr);
   nstr,r := nstr[numRE.matched[0][1],*], numRE.matched[1];
   if(r.matches("*[.E]*")) r=r.toFloat();  // arg!
   if(r.matches("*[.E]*")) r=r.toFloat();  // arg!
   else 		   r=BI(r);

   reg z;
   do{
      z=nstr;	// use this to see if we actually did anything
      if(aRE.search(nstr)){
	 ns,k := aRE.matched;	// ((0,3),"SCO")
	 re,b := aD[k];		// (RegExp("R|RE|RES"),BI(20)), 
	 nstr  = nstr[ns[1],*];
	 if(re.search(nstr)) nstr=nstr[re.matched[0][1],*]; # remove abbrev tail
	 r=r*b;
         continue;
      }else if(kRE.search(nstr)){
	 r*=kD[kRE.matched[1]];	    // "K":1000 ...
	 nstr=nstr[kRE.matched[0][1],*];
         continue;
      }else if(bangRE.search(nstr)){  // floats are converted to int
	 n,k,z := r.toInt(), bangRE.matched[0][1], n - k;
	 r,nstr = BI(n), nstr[k,*];
	 while(z>0){ r.mul(z); z-=k; }
         continue;
      }
   }while(nstr and z!=nstr);
   r
}

fcn ki{  // case insensitive: k, ki, 
   ss:="K M G T P E Z Y X W V U".split();
   d:=Dictionary();
   ss.zipWith(d.add,[3..3*(ss.len()),3].apply(BI(10).pow));    # E:1e+18
   ss.apply("append","I")
      .zipWith(d.add,[10..10*(ss.len()),10].apply(BI(2).pow)); # EI:1.15292e+18
   re:="([%s]I\\?)".fmt(ss.concat());  // "([KMGTPEZYXWVU]I\?)"
   return(RegExp(re),d);
}
fcn abrevCreate{
   var upDown=RegExp("([A-Z]+)(.*)");
   s:="PAIRs 2; SCOres 20; DOZens 12; GREATGRoss 1728; GRoss 144; GOOGOLs 10e100".split(";");
   abrevs,re := Dictionary(), Sink(String);
   foreach an in (s){
      a,n := an.split();
      upDown.search(a);
      u,d := upDown.matched[1,2];
      d=d.len().pump(List,  // "R|RE|RES"
         '+(1),d.get.fp(0),"toUpper").reverse().concat("|");
      abrevs.add(u,T(RegExp(d),BI(n)));
      re.write(u," ");
   }
   // "PAIR|SCO|DOZ|GR|GREATGR|GOOGOL"
   re=RegExp("(%s)".fmt(re.close().strip().replace(" ","|")));
   return(re,abrevs);
}
```


```zkl
foreach na in (T("2greatGRo", "24Gros", "288Doz", "1,728pairs", "172.8SCOre",
   "1,567", "+1.567k", "0.1567e-2m",
   "25.123kK", "25.123m", "2.5123e-00002G",
   "25.123kiKI", "25.123Mi", "2.5123e-00002Gi", "+.25123E-7Ei",
   "-.25123e-34Vikki", "2e-77gooGols",
   "9!", "9!!", "9!!!", "9!!!!", "9!!!!!", "9!!!!!!",
   "9!!!!!!!", "9!!!!!!!!", "9!!!!!!!!!",
   "9!!!!!!!!!k", ".017k!!", "4 dozensK", "2 dozen pairs")){
      
   if((r:=naSuffixes(na)).isType(Float)) println("%16s : %,f".fmt(na,r));
   else 				 println("%16s : %,d".fmt(na,r));
}
```

<pre style="height:45ex">
       2greatGRo : 3,456
          24Gros : 3,456
          288Doz : 3,456
      1,728pairs : 3,456
      172.8SCOre : 3,456.000000
           1,567 : 1,567
         +1.567k : 1,567.000000
      0.1567e-2m : 1,567.000000
        25.123kK : 25,123,000.000000
         25.123m : 25,123,000.000000
  2.5123e-00002G : 25,123,000.000000
      25.123kiKI : 26,343,374.848000
        25.123Mi : 26,343,374.848000
 2.5123e-00002Gi : 26,975,615.844352
    +.25123E-7Ei : 28,964,846,960.237816
-.25123e-34Vikki : -33,394.194938
    2e-77gooGols : 1,999,999,999,999,999,698,010,112.000000
              9! : 362,880
             9!! : 945
            9!!! : 162
           9!!!! : 45
          9!!!!! : 36
         9!!!!!! : 27
        9!!!!!!! : 18
       9!!!!!!!! : 9
      9!!!!!!!!! : 9
     9!!!!!!!!!k : 9,000
         .017k!! : 34,459,425
       4 dozensK : 48,000
   2 dozen pairs : 48

```

