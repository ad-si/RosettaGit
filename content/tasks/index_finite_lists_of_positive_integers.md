+++
title = "Index finite lists of positive integers"
description = ""
date = 2019-08-22T14:36:07Z
aliases = []
[extra]
id = 17613
[taxonomies]
categories = ["task"]
tags = []
languages = [
  "d",
  "go",
  "j",
  "java",
  "julia",
  "kotlin",
  "perl",
  "perl_6",
  "phix",
  "python",
  "racket",
  "rexx",
  "ruby",
  "scala",
  "sidef",
  "tcl",
  "zkl",
]
+++

It is known that the set of finite lists of positive integers is   [[wp:countable| <u>countable</u>]].

This means that there exists a subset of natural integers which can be mapped to the set of finite lists of positive integers.


## Task

Implement such a mapping:
:*   write a function     ''rank''     which assigns an integer to any finite, arbitrarily long list of arbitrary large positive integers.
:*   write a function          ''unrank''        which is the   ''rank''   [[wp:inverse function| <u>inverse function</u>]].


Demonstrate your solution by:
:*   picking a random-length list of random positive integers
:*   turn it into an integer,   and
:*   get the list back.


There are many ways to do this.   Feel free to choose any one you like.


;Extra credit:
Make the   ''rank''   function as a   [[wp:bijection| <u>bijection</u>]]   and show   ''unrank(n)''   for   <big>'''n'''</big>   varying from   '''0'''   to   '''10'''.





## D

This solution isn't efficient.
```d
import std.stdio, std.algorithm, std.array, std.conv, std.bigint;

BigInt rank(T)(in T[] x) pure /*nothrow*/ @safe {
    return BigInt("0x" ~ x.map!text.join('F'));
}

BigInt[] unrank(BigInt n) pure /*nothrow @safe*/ {
    string s;
    while (n) {
        s = "0123456789ABCDEF"[n % 16] ~ s;
        n /= 16;
    }
    return s.split('F').map!BigInt.array;
}

void main() {
    immutable s = [1, 2, 3, 10, 100, 987654321];
    s.writeln;
    s.rank.writeln;
    s.rank.unrank.writeln;
}
```

```txt
[1, 2, 3, 10, 100, 987654321]
37699814998383067155219233
[1, 2, 3, 10, 100, 987654321]

```



## Go

'''Bijective'''

A list element n is encoded as a 1 followed by n 0's.  Element encodings are concatenated to form a single integer rank.  An advantage of this encoding is that no special case is required to handle the empty list.

```go
package main

import (
    "fmt"
    "math/big"
)

func rank(l []uint) (r big.Int) {
    for _, n := range l {
        r.Lsh(&r, n+1)
        r.SetBit(&r, int(n), 1)
    }
    return
}

func unrank(n big.Int) (l []uint) {
    m := new(big.Int).Set(&n)
    for a := m.BitLen(); a > 0; {
        m.SetBit(m, a-1, 0)
        b := m.BitLen()
        l = append(l, uint(a-b-1))
        a = b
    }
    return
}

func main() {
    var b big.Int
    for i := 0; i <= 10; i++ {
        b.SetInt64(int64(i))
        u := unrank(b)
        r := rank(u)
        fmt.Println(i, u, &r)
    }
    b.SetString("12345678901234567890", 10)
    u := unrank(b)
    r := rank(u)
    fmt.Printf("\n%v\n%d\n%d\n", &b, u, &r)
}
```

```txt

0 [] 0
1 [0] 1
2 [1] 2
3 [0 0] 3
4 [2] 4
5 [1 0] 5
6 [0 1] 6
7 [0 0 0] 7
8 [3] 8
9 [2 0] 9
10 [1 1] 10

12345678901234567890
[1 1 1 0 1 1 1 2 1 1 2 0 3 0 2 0 0 1 1 0 3 0 0 0 0 4 1 1 0 1 2 1]
12345678901234567890

```

'''Alternative'''

A bit of a hack to make a base 11 number then interpret it as base 16, just because that's easiest.  Not bijective.  Practical though for small lists of large numbers.

```go
package main

import (
    "fmt"
    "math/big"
    "math/rand"
    "strings"
    "time"
)

// Prepend base 10 representation with an "a" and you get a base 11 number.
// Unfortunately base 11 is a little awkward with big.Int, so just treat it
// as base 16.
func rank(l []big.Int) (r big.Int, err error) {
    if len(l) == 0 {
        return
    }
    s := make([]string, len(l))
    for i, n := range l {
        ns := n.String()
        if ns[0] == '-' {
            return r, fmt.Errorf("negative integers not mapped")
        }
        s[i] = "a" + ns
    }
    r.SetString(strings.Join(s, ""), 16)
    return
}

// Split the base 16 representation at "a", recover the base 10 numbers.
func unrank(r big.Int) ([]big.Int, error) {
    s16 := fmt.Sprintf("%x", &r)
    switch {
    case s16 == "0":
        return nil, nil // empty list
    case s16[0] != 'a':
        return nil, fmt.Errorf("unrank not bijective")
    }
    s := strings.Split(s16[1:], "a")
    l := make([]big.Int, len(s))
    for i, s1 := range s {
        if _, ok := l[i].SetString(s1, 10); !ok {
            return nil, fmt.Errorf("unrank not bijective")
        }
    }
    return l, nil
}

func main() {
    // show empty list
    var l []big.Int
    r, _ := rank(l)
    u, _ := unrank(r)
    fmt.Println("Empty list:", l, &r, u)

    // show random list
    l = random()
    r, _ = rank(l)
    u, _ = unrank(r)
    fmt.Println("\nList:")
    for _, n := range l {
        fmt.Println("  ", &n)
    }
    fmt.Println("Rank:")
    fmt.Println("  ", &r)
    fmt.Println("Unranked:")
    for _, n := range u {
        fmt.Println("  ", &n)
    }

    // show error with list containing negative
    var n big.Int
    n.SetInt64(-5)
    _, err := rank([]big.Int{n})
    fmt.Println("\nList element:", &n, err)

    // show technique is not bijective
    n.SetInt64(1)
    _, err = unrank(n)
    fmt.Println("Rank:", &n, err)
}

// returns 0 to 5 numbers in the range 1 to 2^100
func random() []big.Int {
    r := rand.New(rand.NewSource(time.Now().Unix()))
    l := make([]big.Int, r.Intn(6))
    one := big.NewInt(1)
    max := new(big.Int).Lsh(one, 100)
    for i := range l {
        l[i].Add(one, l[i].Rand(r, max))
    }
    return l
}
```

```txt

Empty list: [] 0 []

List:
   170245492534662309353778826165
   82227712638678862510272817700
Rank:
   17827272030291729487097780664374477811820701746650470453292650775464474368
Unranked:
   170245492534662309353778826165
   82227712638678862510272817700

List element: -5 negative integers not handled
Rank: 1 unrank not bijective

```



## J



###  Explicit version


Implementation:


```j
scrunch=:3 :0
  n=.1x+>./y
  #.(1#~##:n),0,n,&#:n#.y
)

hcnurcs=:3 :0
  b=.#:y
  m=.b i.0
  n=.#.m{.(m+1)}.b
  n #.inv#.(1+2*m)}.b
)
```


Example use:


```J
   scrunch 4 5 7 9 0 8 8 7 4 8 8 4 1
4314664669630761
   hcnurcs 4314664669630761
4 5 7 9 0 8 8 7 4 8 8 4 1
```


Explanation. We treat the sequence as an n digit number in base m where n is the length of the list and m is 1+the largest value in the list. (This is equivalent to treating it as a polynomial in m with coefficients which are the values of the list.) In other words 4 5 7 9 0 8 8 7 4 8 8 4 1 becomes 4579088748841.  Now we just need to encode the base (10, in this case). To do that we treat this number as a sequence of bits and prepend it with 1 1 1 1 0 1 0 1 0. This is a sequence of '1's whose length matches the number of bits needed to represent the base of our polynomial, followed by a 0 followed by the base of our polynomial.

To extract the original list we reverse this process: Find the position of the first zero, that's the size of our base, extract the base and then use that to find the coefficients of our polynomial, which is or original list.

Whether this is an efficient representation or not depends, of course, on the nature of the list being represented.



###  Tacit versions


Base 11 encoding:


```j
   rank  =. 11&#.@:}.@:>@:(,&:>/)@:(<@:(10&,)@:(10&#.^:_1)"0)@:x:
   unrank=. 10&#.;._1@:(10&,)@:(11&#.^:_1)
```


Example use:


```J
   rank 1 2 3 10 100 987654321 135792468107264516704251 7x
187573177082615698496949025806128189691804770100426

   unrank 187573177082615698496949025806128189691804770100426x
1 2 3 10 100 987654321 135792468107264516704251 7
```


Prime factorization (Gödelian) encoding:


```j
   rank=. */@:(^~ p:@:i.@:#)@:>:@:x:
   unrank=. <:@:(#;.1@:~:@:q:)
```


Example use:


```J
   rank 1 11 16 1 3 9 0 2 15 7 19 10
6857998574998940803374702726455974765530187550029640884386375715876970128518999225074067307280381624132537960815429687500

   unrank 6857998574998940803374702726455974765530187550029640884386375715876970128518999225074067307280381624132537960815429687500x
1 11 16 1 3 9 0 2 15 7 19 10
```



###  Bijective


Using the method of the Python version (shifted):


```j
   rank=. 1 -~ #.@:(1 , >@:(([ , 0 , ])&.>/)@:(<@:($&1)"0))@:x:
   unrank=. #;._2@:((0 ,~ }.)@:(#.^:_1)@:(1&+))
```


Example use:


```j

@:((] ; unrank ; rank@:unrank)&.>)@:i. 11
┌──┬───────┬──┐
│0 │0      │0 │
├──┼───────┼──┤
│1 │0 0    │1 │
├──┼───────┼──┤
│2 │1      │2 │
├──┼───────┼──┤
│3 │0 0 0  │3 │
├──┼───────┼──┤
│4 │0 1    │4 │
├──┼───────┼──┤
│5 │1 0    │5 │
├──┼───────┼──┤
│6 │2      │6 │
├──┼───────┼──┤
│7 │0 0 0 0│7 │
├──┼───────┼──┤
│8 │0 0 1  │8 │
├──┼───────┼──┤
│9 │0 1 0  │9 │
├──┼───────┼──┤
│10│0 2    │10│
└──┴───────┴──┘

   (] ; rank ; unrank@:rank) 1 2 3 5 8
┌─────────┬────────┬─────────┐
│1 2 3 5 8│14401278│1 2 3 5 8│
└─────────┴────────┴─────────┘
```



## Java

Translation of [[Index_finite_lists_of_positive_integers#Python|Python]] via [[Index_finite_lists_of_positive_integers#D|D]]
```java
import java.math.BigInteger;
import static java.util.Arrays.stream;
import java.util.*;
import static java.util.stream.Collectors.*;

public class Test3 {
    static BigInteger rank(int[] x) {
        String s = stream(x).mapToObj(String::valueOf).collect(joining("F"));
        return new BigInteger(s, 16);
    }

    static List<BigInteger> unrank(BigInteger n) {
        BigInteger sixteen = BigInteger.valueOf(16);
        String s = "";
        while (!n.equals(BigInteger.ZERO)) {
            s = "0123456789ABCDEF".charAt(n.mod(sixteen).intValue()) + s;
            n = n.divide(sixteen);
        }
        return stream(s.split("F")).map(x -> new BigInteger(x)).collect(toList());
    }

    public static void main(String[] args) {
        int[] s = {1, 2, 3, 10, 100, 987654321};
        System.out.println(Arrays.toString(s));
        System.out.println(rank(s));
        System.out.println(unrank(rank(s)));
    }
}
```


```txt
[1, 2, 3, 10, 100, 987654321]
37699814998383067155219233
[1, 2, 3, 10, 100, 987654321]
```



## Julia

```julia
Base.rank(x::Vector{<:Integer}) = parse(BigInt, "1a" * join(x, 'a'), 11)
function unrank(n::Integer)
    s = ""
    while !iszero(n)
        ind = n % 11 + 1
        n ÷= 11
        s = "0123456789a"[ind:ind] * s
    end
    return parse.(Int, split(s, 'a'))[2:end]
end

v = [0, 1, 2, 3, 10, 100, 987654321]
n = rank(v)
v = unrank(n)
println("# v = $v\n -> n = $n\n -> v = $v")
```


```txt
# v = [0, 1, 2, 3, 10, 100, 987654321]
 -> n = 207672721333439869642567444
 -> v = [0, 1, 2, 3, 10, 100, 987654321]
```



## Kotlin


```scala
// version 1.1.2

import java.math.BigInteger

/* Separates each integer in the list with an 'a' then encodes in base 11. Empty list mapped to '-1' */
fun rank(li: List<Int>) = when (li.size) {
    0    -> -BigInteger.ONE
    else ->  BigInteger(li.joinToString("a"), 11)
}

fun unrank(r: BigInteger) = when (r) {
    -BigInteger.ONE -> emptyList<Int>()
    else            -> r.toString(11).split('a').map { if (it != "") it.toInt() else 0 }
}


/* Each integer n in the list mapped to '1' plus n '0's. Empty list mapped to '0' */
fun rank2(li:List<Int>): BigInteger {
    if (li.isEmpty()) return BigInteger.ZERO
    val sb = StringBuilder()
    for (i in li) sb.append("1" + "0".repeat(i))
    return BigInteger(sb.toString(), 2)
}

fun unrank2(r: BigInteger) = when (r) {
    BigInteger.ZERO -> emptyList<Int>()
    else            -> r.toString(2).drop(1).split('1').map { it.length }
}

fun main(args: Array<String>) {
    var li: List<Int>
    var r: BigInteger
    li = listOf(0, 1, 2, 3, 10, 100, 987654321)
    println("Before ranking   : $li")
    r = rank(li)
    println("Rank = $r")
    li = unrank(r)
    println("After unranking  : $li")

    println("\nAlternative approach (not suitable for large numbers)...\n")
    li = li.dropLast(1)
    println("Before ranking   : $li")
    r = rank2(li)
    println("Rank = $r")
    li = unrank2(r)
    println("After unranking  : $li")

    println()
    for (i in 0..10) {
        val bi = BigInteger.valueOf(i.toLong())
        li = unrank2(bi)
        println("${"%2d".format(i)} -> ${li.toString().padEnd(9)} -> ${rank2(li)}")
    }
}
```


```txt

Before ranking   : [0, 1, 2, 3, 10, 100, 987654321]
Rank = 828335141480036653618783
After unranking  : [0, 1, 2, 3, 10, 100, 987654321]

Alternative approach (not suitable for large numbers)...

Before ranking   : [0, 1, 2, 3, 10, 100]
Rank = 4364126777249122850009283661412696064
After unranking  : [0, 1, 2, 3, 10, 100]

 0 -> []        -> 0
 1 -> [0]       -> 1
 2 -> [1]       -> 2
 3 -> [0, 0]    -> 3
 4 -> [2]       -> 4
 5 -> [1, 0]    -> 5
 6 -> [0, 1]    -> 6
 7 -> [0, 0, 0] -> 7
 8 -> [3]       -> 8
 9 -> [2, 0]    -> 9
10 -> [1, 1]    -> 10

```



## Perl

The base-11 approach requires <code>bigint</code> pragma for all but trivial lists. Using <code>ntheory</code> module for base conversions.
```perl
use bigint;
use ntheory qw(fromdigits todigitstring);
use feature 'say';

sub rank   { join   '', fromdigits(join('a',@_), 11) }
sub unrank { split 'a', todigitstring(@_[0],     11) }

say join ' ', @n = qw<12 11 0 7 9 15 15 5 7 13 5 5>;
say $n = rank(@n);
say join ' ', unrank $n;
```

```txt
12 11 0 7 9 15 15 5 7 13 5 5
16588666500024842935939135419
12 11 0 7 9 15 15 5 7 13 5 5
```



## Perl 6

Here is a cheap solution using a base-11 encoding and string operations:

```perl6
sub rank(*@n)      { :11(@n.join('A')) }
sub unrank(Int $n) { $n.base(11).split('A') }

say my @n = (1..20).roll(12);
say my $n = rank(@n);
say unrank $n;
```

```txt
1 11 16 1 3 9 0 2 15 7 19 10
25155454474293912130094652799
1 11 16 1 3 9 0 2 15 7 19 10
```

Here is a bijective solution that does not use string operations.

```perl6
multi infix:<rad> ()       { 0 }
multi infix:<rad> ($a)     { $a }
multi infix:<rad> ($a, $b) { $a * $*RADIX + $b }

multi expand(Int $n is copy, 1) { $n }
multi expand(Int $n is copy, Int $*RADIX) {
    my \RAD = $*RADIX;

    my @reversed-digits = gather while $n > 0 {
	take $n % RAD;
	$n div= RAD;
    }

    eager for ^RAD {
	[rad] reverse @reversed-digits[$_, * + RAD ... *]
    }
}

multi compress(@n where @n == 1) { @n[0] }
multi compress(@n is copy) {
    my \RAD = my $*RADIX = @n.elems;

    [rad] reverse gather while @n.any > 0 {
	    (state $i = 0) %= RAD;
	    take @n[$i] % RAD;
	    @n[$i] div= RAD;
	    $i++;
	}
}

sub rank(@n) { compress (compress(@n), @n - 1)}
sub unrank(Int $n) { my ($a, $b) = expand $n, 2; expand $a, $b + 1 }

my @list = (^10).roll((2..20).pick);
my $rank = rank @list;
say "[$@list] -> $rank -> [{unrank $rank}]";

for ^10 {
    my @unrank = unrank $_;
    say "$_ -> [$@unrank] -> {rank @unrank}";
}
```


```txt
[7 1 4 7 7 0 2 7 7 0 7 7] -> 20570633300796394530947471 -> [7 1 4 7 7 0 2 7 7 0 7 7]
0 -> [0] -> 0
1 -> [1] -> 1
2 -> [0 0] -> 2
3 -> [1 0] -> 3
4 -> [2] -> 4
5 -> [3] -> 5
6 -> [0 1] -> 6
7 -> [1 1] -> 7
8 -> [0 0 0] -> 8
9 -> [1 0 0] -> 9
```



## Phix


### base 11

```Phix
include mpfr.e

procedure rank(mpz r, sequence s)
    for i=1 to length(s) do
        s[i] = sprintf("%d",s[i])
    end for
    mpz_set_str(r,join(s,'a'),11)
end procedure

function unrank(mpz i)
    sequence res = split(mpz_get_str(i,11),'a')
    for j=1 to length(res) do
        {{res[j]}} = scanf(res[j],"%d")
    end for
    return res
end function

sequence l = {1, 2, 3, 10, 100, 987654321}
mpz n = mpz_init()
rank(n,l)
sequence u = unrank(n)
?{l,mpz_get_str(n),u}
```

```txt

```



### bijective

```Phix
function unrank(atom n)
    sequence res = sprintf("%0b",n)
    if res="1" then return {0} end if
    res = split(res[2..$],'0')
    for i=1 to length(res) do res[i] = length(res[i]) end for
    return res
end function

function rank(sequence x)
    if x={} then return "0" end if
    for i=1 to length(x) do
        x[i] = repeat('1',x[i])
    end for
    atom {{res}} = scanf("0b1"&join(x,'0'),"%d")
    return res
end function

for i=0 to 10 do
    sequence a = unrank(i)
    printf(1,"%3d : %-18v: %d\n",{i, a, rank(a)})
end for

sequence x = {1, 2, 3, 5, 8}
printf(1,"%v => %d => %v\n",{x,rank(x),unrank(rank(x))})
```

```txt

  0 : {}                : 0
  1 : {0}               : 1
  2 : {0,0}             : 2
  3 : {1}               : 3
  4 : {0,0,0}           : 4
  5 : {0,1}             : 5
  6 : {1,0}             : 6
  7 : {2}               : 7
  8 : {0,0,0,0}         : 8
  9 : {0,0,1}           : 9
 10 : {0,1,0}           : 10
{1,2,3,5,8} => 14401279 => {1,2,3,5,8}

```



## Python


```python
def rank(x): return int('a'.join(map(str, [1] + x)), 11)

def unrank(n):
	s = ''
	while n: s,n = "0123456789a"[n%11] + s, n//11
	return map(int, s.split('a'))[1:]

l = [1, 2, 3, 10, 100, 987654321]
print l
n = rank(l)
print n
l = unrank(n)
print l
```

```txt

[0, 1, 2, 3, 10, 100, 987654321]
207672721333439869642567444
[0, 1, 2, 3, 10, 100, 987654321]

```



###  Bijection

Each number in the list is stored as a length of 1s, separated by 0s, and the resulting string is prefixed by '1', then taken as a binary number. Empty list is mapped to 0 as a special case. Don't use it on large numbers.

```python
def unrank(n):
        return map(len, bin(n)[3:].split("0")) if n else []

def rank(x):
        return int('1' + '0'.join('1'*a for a in x), 2) if x else 0

for x in range(11):
        print x, unrank(x), rank(unrank(x))

print
x = [1, 2, 3, 5, 8];
print x, rank(x), unrank(rank(x))

```

```txt

0 [] 0
1 [0] 1
2 [0, 0] 2
3 [1] 3
4 [0, 0, 0] 4
5 [0, 1] 5
6 [1, 0] 6
7 [2] 7
8 [0, 0, 0, 0] 8
9 [0, 0, 1] 9
10 [0, 1, 0] 10

[1, 2, 3, 5, 8] 14401279 [1, 2, 3, 5, 8]

```



## Racket


{{trans|Tcl}} (which gives credit to [[#D]])


```racket
#lang racket/base
(require (only-in racket/string string-join string-split))

(define (integer->octal-string i)
  (number->string i 8))

(define (octal-string->integer s)
  (string->number s 8))

(define (rank is)
  (string->number (string-join (map integer->octal-string is) "8")))

(define (unrank ranking)
  (map octal-string->integer (string-split (number->string ranking 10) "8")))

(module+ test
  (define loi '(1 2 3 10 100 987654321 135792468107264516704251 7))
  (define rnk (rank loi))
  (define urk (unrank rnk))
  (displayln loi)
  (displayln rnk)
  (displayln urk))
```


```txt
(1 2 3 10 100 987654321 135792468107264516704251 7)
1828381281448726746426183460251416730347660304377387
(1 2 3 10 100 987654321 135792468107264516704251 7)
```



## REXX

This REXX version can handle zeros as well as any sized (decimal) positive integers.

No checks are made that the numbers are non-negative integers or malformed integers.

```rexx
/*REXX program assigns an integer for a finite list of arbitrary non-negative integers. */
parse arg $                                      /*obtain optional argument  (int list).*/
if $='' | $=","  then $=3 14 159 265358979323846 /*Not specified?  Then use the default.*/
                                                 /* [↑]  kinda use decimal digits of pi.*/
$= translate( space($),   ',',   " ")            /*use a  commatized  list of integers. */
numeric digits max(9, 2 * length($) )            /*ensure enough dec. digits to handle $*/

                 say 'original list='   $        /*display the original list of integers*/
N=   rank($);    say '  map integer='   N        /*generate and display the map integer.*/
O= unrank(N);    say '       unrank='   O        /*generate original integer and display*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
rank:    return    x2d( translate( space( arg(1) ),  'c',  ",") )
unrank:  return  space( translate(   d2x( arg(1) ),  ',',  "C") )
```

```txt

original list= 3,14,159,265358979323846
  map integer= 18594192178172074189223245894
       unrank= 3,14,159,265358979323846

```



## Ruby

```ruby
def rank(arr)
  arr.join('a').to_i(11)
end

def unrank(n)
  n.to_s(11).split('a').collect{|x| x.to_i}
end

l = [1, 2, 3, 10, 100, 987654321]
p l
n = rank(l)
p n
l = unrank(n)
p l
```

```txt

[1, 2, 3, 10, 100, 987654321]
14307647611639042485573
[1, 2, 3, 10, 100, 987654321]

```


###  Bijection

```ruby
def unrank(n)
  return [0] if n==1
  n.to_s(2)[1..-1].split('0',-1).map(&:size)
end

def rank(x)
  return 0 if x.empty?
  ('1' + x.map{ |a| '1'*a }.join('0')).to_i(2)
end

for x in 0..10
  puts "%3d : %-18s: %d" % [x, a=unrank(x), rank(a)]
end

puts
x = [1, 2, 3, 5, 8]
puts "#{x} => #{rank(x)} => #{unrank(rank(x))}"
```

```txt

  0 : []                : 0
  1 : [0]               : 1
  2 : [0, 0]            : 2
  3 : [1]               : 3
  4 : [0, 0, 0]         : 4
  5 : [0, 1]            : 5
  6 : [1, 0]            : 6
  7 : [2]               : 7
  8 : [0, 0, 0, 0]      : 8
  9 : [0, 0, 1]         : 9
 10 : [0, 1, 0]         : 10

[1, 2, 3, 5, 8] => 14401279 => [1, 2, 3, 5, 8]

```



## Scala

{{Out}}Best seen in running your browser either by [https://scalafiddle.io/sf/7NvnU4t/0 ScalaFiddle (ES aka JavaScript, non JVM)] or [https://scastie.scala-lang.org/l0uAGyyCTDSAV9Q45vRGaA Scastie (remote JVM)].

```Scala
object IndexFiniteList extends App {
  val (defBase, s) = (10, Seq(1, 2, 3, 10, 100, 987654321))

  def rank(x: Seq[Int], base: Int = defBase) =
    BigInt(x.map(Integer.toString(_, base)).mkString(base.toHexString), base + 1)

  def unrank(n: BigInt, base: Int = defBase): List[BigInt] =
    n.toString(base + 1).split((base).toHexString).map(BigInt(_)).toList

  val ranked = rank(s)

  println(s.mkString("[", ", ", "]"))
  println(ranked)
  println(unrank(ranked).mkString("[", ", ", "]"))

}
```


## Sidef

```ruby
func rank(Array arr) {
    Number(arr.join('a'), 11)
}

func unrank(Number n) {
    n.base(11).split('a').map { Num(_) }
}

var l = [1, 2, 3, 10, 100, 987654321]
say l
var n = rank(l)
say n
var l = unrank(n)
say l
```

```txt
[1, 2, 3, 10, 100, 987654321]
14307647611639042485573
[1, 2, 3, 10, 100, 987654321]
```


'''Bijection:'''

```ruby
func unrank(Number n) {
    n == 1 ? [0]
           : n.base(2).substr(1).split('0', -1).map{.len}
}

func rank(Array x) {
    x.is_empty ? 0
               : Number('1' + x.map { '1' * _ }.join('0'), 2)
}

for x in (0..10) {
    printf("%3d : %-18s: %d\n", x, unrank(x), rank(unrank(x)))
}

say ''
var x = [1, 2, 3, 5, 8]
say "#{x} => #{rank(x)} => #{unrank(rank(x))}"
```

```txt
  0 : []                : 0
  1 : [0]               : 1
  2 : [0, 0]            : 2
  3 : [1]               : 3
  4 : [0, 0, 0]         : 4
  5 : [0, 1]            : 5
  6 : [1, 0]            : 6
  7 : [2]               : 7
  8 : [0, 0, 0, 0]      : 8
  9 : [0, 0, 1]         : 9
 10 : [0, 1, 0]         : 10

[1, 2, 3, 5, 8] => 14401279 => [1, 2, 3, 5, 8]
```



## Tcl

Inspired by the [[#D|D solution]].

```tcl
package require Tcl 8.6

proc rank {integers} {
    join [lmap i $integers {format %llo $i}] 8
}

proc unrank {codedValue} {
    lmap i [split $codedValue 8] {scan $i %llo}
}
```

Demonstrating:

```tcl
set s {1 2 3 10 100 987654321 135792468107264516704251 7}
puts "prior: $s"
set c [rank $s]
puts "encoded: $c"
set t [unrank $c]
puts "after: $t"
```

```txt

prior: 1 2 3 10 100 987654321 135792468107264516704251 7
encoded: 1828381281448726746426183460251416730347660304377387
after: 1 2 3 10 100 987654321 135792468107264516704251 7

```



## zkl

Using GMP, base 11 and sometimes strings to represent big ints.

```zkl
var BN=Import("zklBigNum");
fcn rank(ns)   { BN(ns.concat("A"),11) }
fcn unrank(bn) { bn.toString(11).split("a").apply("toInt") }
fcn unrankS(bn){ bn.toString(11).split("a") }
```


```zkl
fcn rankz(ns,S=False){
   ns.println();
   rank(ns).println();
   if(S) ns:rank(_):unrankS(_).println();
   else  ns:rank(_):unrank(_) .println();
}
rankz(T(1,2,3,10,100,987654321));
rankz(T(1,2,3,10,100,987654321,"135792468107264516704251",7),True);
```

```txt

L(1,2,3,10,100,987654321)
14307647611639042485573
L(1,2,3,10,100,987654321)
L(1,2,3,10,100,987654321,"135792468107264516704251",7)
187573177082615698496949025806128189691804770100426
L("1","2","3","10","100","987654321","135792468107264516704251","7")

```

