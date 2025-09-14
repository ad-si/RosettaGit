+++
title = "Resistance Network Calculator"
description = ""
date = 2019-07-10T08:52:52Z
aliases = []
[extra]
id = 22235
[taxonomies]
categories = ["task"]
tags = []
languages = [
  "go",
  "julia",
  "perl",
  "perl_6",
  "phix",
  "python",
  "zkl",
]
+++

;Introduction

Calculate the resistance of any resistor network.

* The network is stated with a string.
* The resistors are separated by a vertical dash.
* Each resistor has 
** a starting node
** an ending node
** a resistance


;Background
[https://physics.stackexchange.com/questions/19295/how-calculate-resistance-between-two-points-for-arbitrary-resistor-grid Arbitrary Resistor Grid]


;Regular 3x3 mesh, using twelve one ohm resistors
 0 - 1 - 2
 |   |   | 
 3 - 4 - 5
 |   |   |
 6 - 7 - 8 

Battery connection nodes: 0 and 8  
 assert 3/2 == network(9,0,8,"0 1 1|1 2 1|3 4 1|4 5 1|6 7 1|7 8 1|0 3 1|3 6 1|1 4 1|4 7 1|2 5 1|5 8 1")


;Regular 4x4 mesh, using 24 one ohm resistors

  0 - 1 - 2 - 3
  |   |   |   |   
  4 - 5 - 6 - 7
  |   |   |   |
  8 - 9 -10 -11
  |   |   |   |
 12 -13 -14 -15

Battery connection nodes: 0 and 15  
 assert 13/7 == network(16,0,15,"0 1 1|1 2 1|2 3 1|4 5 1|5 6 1|6 7 1|8 9 1|9 10 1|10 11 1|12 13 1|13 14 1|14 15 1|0 4 1|4 8 1|8 12 1|1 5 1|5 9 1|9 13 1|2 6 1|6 10 1|10 14 1|3 7 1|7 11 1|11 15 1")


;Ten resistor network

[https://photos.google.com/photo/AF1QipPfPkOrrBpJq-KFwWR-BVlfzM5VKklKWnP31nC_ Picture]

Battery connection nodes: 0 and 1  
 assert 10 == network(7,0,1,"0 2 6|2 3 4|3 4 10|4 5 2|5 6 8|6 1 4|3 5 6|3 6 6|3 1 8|2 1 8")


;Wheatstone network

[https://photos.google.com/photo/AF1QipP7yjK4gA5_xKMTo4IiO6-taHNEzelGtAIkGgE0 Picture]

This network is not possible to solve using the previous [http://www.rosettacode.org/wiki/Resistance_Calculator Resistance Calculator]
as there is no natural starting point.

 assert 180 == network(4,0,3,"0 1 150|0 2 50|1 3 300|2 3 250")


## Go

```go
package main

import (
    "fmt"
    "math"
    "strconv"
    "strings"
)

func argmax(m [][]float64, i int) int {
    col := make([]float64, len(m))
    max, maxx := -1.0, -1
    for x := 0; x < len(m); x++ {
        col[x] = math.Abs(m[x][i])
        if col[x] > max {
            max = col[x]
            maxx = x
        }
    }
    return maxx
}

func gauss(m [][]float64) []float64 {
    n, p := len(m), len(m[0])
    for i := 0; i < n; i++ {
        k := i + argmax(m[i:n], i)
        m[i], m[k] = m[k], m[i]
        t := 1 / m[i][i]
        for j := i + 1; j < p; j++ {
            m[i][j] *= t
        }
        for j := i + 1; j < n; j++ {
            t = m[j][i]
            for l := i + 1; l < p; l++ {
                m[j][l] -= t * m[i][l]
            }
        }
    }
    for i := n - 1; i >= 0; i-- {
        for j := 0; j < i; j++ {
            m[j][p-1] -= m[j][i] * m[i][p-1]
        }
    }
    col := make([]float64, len(m))
    for x := 0; x < len(m); x++ {
        col[x] = m[x][p-1]
    }
    return col
}

func network(n, k0, k1 int, s string) float64 {
    m := make([][]float64, n)
    for i := 0; i < n; i++ {
        m[i] = make([]float64, n+1)
    }
    for _, resistor := range strings.Split(s, "|") {
        rarr := strings.Fields(resistor)
        a, _ := strconv.Atoi(rarr[0])
        b, _ := strconv.Atoi(rarr[1])
        ri, _ := strconv.Atoi(rarr[2])
        r := 1.0 / float64(ri)
        m[a][a] += r
        m[b][b] += r
        if a > 0 {
            m[a][b] -= r
        }
        if b > 0 {
            m[b][a] -= r
        }
    }
    m[k0][k0] = 1
    m[k1][n] = 1
    return gauss(m)[k1]
}

func main() {
    var fa [4]float64
    fa[0] = network(7, 0, 1, "0 2 6|2 3 4|3 4 10|4 5 2|5 6 8|6 1 4|3 5 6|3 6 6|3 1 8|2 1 8")
    fa[1] = network(9, 0, 8, "0 1 1|1 2 1|3 4 1|4 5 1|6 7 1|7 8 1|0 3 1|3 6 1|1 4 1|4 7 1|2 5 1|5 8 1")
    fa[2] = network(16, 0, 15, "0 1 1|1 2 1|2 3 1|4 5 1|5 6 1|6 7 1|8 9 1|9 10 1|10 11 1|12 13 1|13 14 1|14 15 1|0 4 1|4 8 1|8 12 1|1 5 1|5 9 1|9 13 1|2 6 1|6 10 1|10 14 1|3 7 1|7 11 1|11 15 1")
    fa[3] = network(4, 0, 3, "0 1 150|0 2 50|1 3 300|2 3 250")
    for _, f := range fa {
        fmt.Printf("%.6g\n", f)
    }
}
```


```txt

10
1.5
1.85714
180

```




## Julia

```julia
function gauss(m)
    n, p = length(m), length(m[1])
    for i in 1:n
        _, k = findmax(map(x -> abs(m[x][i]), i:n)) .+ (i - 1)
        m[i], m[k] = m[k], m[i]
        t = 1 // m[i][i]
        for j in i+1:p
            m[i][j] *= t
        end
        for j in i+1:n
            t = m[j][i]
            for k in i+1:p
                m[j][k] -= t * m[i][k]
            end
        end
    end
    for i in n:-1:1, j in 1:i-1; m[j][end] -= m[j][i] * m[i][end]; end
    return [row[end] for row in m]
end
 
function network(n, k0, k1, s)
    m = [[0//1 for i in 1:n + 1] for j in 1:n]
    resistors = split(s, "|")
    for resistor in resistors
        astr, bstr, rstr = split(resistor, " ")
        a, b, r = parse(Int, astr), parse(Int, bstr), 1 // parse(Int, rstr)
        m[a + 1][a + 1] += r
        m[b + 1][b + 1] += r
        if a > 0; m[a + 1][b + 1] -= r end
        if b > 0; m[b + 1][a + 1] -= r end
    end
    m[k0+1][k0+1] = m[k1+1][end] = 1 // 1
    return gauss(m)[k1+1]
end

@assert(10     == network(7,0,1,"0 2 6|2 3 4|3 4 10|4 5 2|5 6 8|6 1 4|3 5 6|3 6 6|3 1 8|2 1 8"))
@assert(3//2   == network(3*3,0,3*3-1,"0 1 1|1 2 1|3 4 1|4 5 1|6 7 1|7 8 1|0 3 1|3 6 1|1 4 1|4 7 1|2 5 1|5 8 1"))
@assert(13//7 == network(4*4,0,4*4-1,"0 1 1|1 2 1|2 3 1|4 5 1|5 6 1|6 7 1|8 9 1|9 10 1|10 11 1|12 13 1|13 14 1|14 15 1|0 4 1|4 8 1|8 12 1|1 5 1|5 9 1|9 13 1|2 6 1|6 10 1|10 14 1|3 7 1|7 11 1|11 15 1"))
@assert(180   == network(4,0,3,"0 1 150|0 2 50|1 3 300|2 3 250"))

```

No assertion errors.


## Perl


```perl
use strict;
use warnings;

sub gauss {
  our @m; local *m = shift;
  my ($lead, $rows, $cols) = (0, scalar(@m), scalar(@{$m[0]}));
  foreach my $r (0 .. $rows - 1) {
     $lead < $cols or return;
      my $i = $r;
      until ($m[$i][$lead])
         {++$i == $rows or next;
          $i = $r;
          ++$lead == $cols and return;}
      @m[$i, $r] = @m[$r, $i];
      my $lv = $m[$r][$lead];
      $_ /= $lv foreach @{ $m[$r] };
      my @mr = @{ $m[$r] };
      foreach my $i (0 .. $rows - 1)
         {$i == $r and next;
          ($lv, my $n) = ($m[$i][$lead], -1);
          $_ -= $lv * $mr[++$n] foreach @{ $m[$i] };}
      ++$lead;}
}

sub network {
    my($n,$k0,$k1,$grid) = @_;
    my @m;
    push @m, [(0)x($n+1)] for 1..$n;

    for my $resistor (split '\|', $grid) {
        my ($a,$b,$r_inv) = split /\s+/, $resistor;
        my $r = 1 / $r_inv;
        $m[$a][$a] += $r;
        $m[$b][$b] += $r;
        $m[$a][$b] -= $r if $a > 0;
        $m[$b][$a] -= $r if $b > 0;
    }
    $m[$k0][$k0] = 1;
    $m[$k1][ -1] = 1;
    gauss(\@m);
    return $m[$k1][-1];
}

for (
    [   7, 0,     1, '0 2 6|2 3 4|3 4 10|4 5 2|5 6 8|6 1 4|3 5 6|3 6 6|3 1 8|2 1 8' ],
    [ 3*3, 0, 3*3-1, '0 1 1|1 2 1|3 4 1|4 5 1|6 7 1|7 8 1|0 3 1|3 6 1|1 4 1|4 7 1|2 5 1|5 8 1' ],
    [ 4*4, 0, 4*4-1, '0 1 1|1 2 1|2 3 1|4 5 1|5 6 1|6 7 1|8 9 1|9 10 1|10 11 1|12 13 1|13 14 1|14 15 1|0 4 1|4 8 1|8 12 1|1 5 1|5 9 1|9 13
1|2 6 1|6 10 1|10 14 1|3 7 1|7 11 1|11 15 1' ],
    [   4, 0,     3, '0 1 150|0 2 50|1 3 300|2 3 250' ],
) {
    printf "%10.3f\n", network(@$_);
}

```

```txt
    10.000
     1.500
     1.857
   180.000
```



## Perl 6

```perl6
sub gauss ( @m is copy ) {
    for @m.keys -> \i {
        my \k = max |(i .. @m.end), :by({ @m[$_][i].abs });

        @m[i, k] .= reverse if \k != i;

        .[i ^.. *] »/=» .[i] given @m[i];

        for i ^.. @m.end -> \j {
            @m[j][i ^.. *] »-=« ( @m[j][i] «*« @m[i][i ^.. *] );
        }
    }
    for @m.keys.reverse -> \i {
        @m[^i]».[*-1] »-=« ( @m[^i]».[i] »*» @m[i][*-1] );
    }
    return @m».[*-1];
}
sub network ( Int \n, Int \k0, Int \k1, Str \grid ) {
    my @m = [0 xx n+1] xx n;

    for grid.split('|') -> \resistor {
        my ( \a, \b, \r_inv ) = resistor.split(/\s+/, :skip-empty);
        my \r = 1 / r_inv;

        @m[a][a] += r;
        @m[b][b] += r;
        @m[a][b] -= r if a > 0;
        @m[b][a] -= r if b > 0;
    }
    @m[k0][k0]  = 1;
    @m[k1][*-1] = 1;

    return gauss(@m)[k1];
}
use Test;
my @tests =
    (   10,   7, 0,     1, '0 2 6|2 3 4|3 4 10|4 5 2|5 6 8|6 1 4|3 5 6|3 6 6|3 1 8|2 1 8' ),
    (  3/2, 3*3, 0, 3*3-1, '0 1 1|1 2 1|3 4 1|4 5 1|6 7 1|7 8 1|0 3 1|3 6 1|1 4 1|4 7 1|2 5 1|5 8 1' ),
    ( 13/7, 4*4, 0, 4*4-1, '0 1 1|1 2 1|2 3 1|4 5 1|5 6 1|6 7 1|8 9 1|9 10 1|10 11 1|12 13 1|13 14 1|14 15 1|0 4 1|4 8 1|8 12 1|1 5 1|5 9 1|9 13 1|2 6 1|6 10 1|10 14 1|3 7 1|7 11 1|11 15 1' ),
    (  180,   4, 0,     3, '0 1 150|0 2 50|1 3 300|2 3 250' ),
;
plan +@tests;
is .[0], network( |.[1..4] ), .[4].substr(0,10)~'…' for @tests;
```



## Phix

```Phix
function argmax(sequence m, integer i)
    sequence col := sq_abs(vslice(m,i))
    return largest(col,return_index:=true)
end function
 
function gauss(sequence m)
    integer n = length(m), 
            p = length(m[1])
    for i=1 to n do
        integer k := i + argmax(m[i..n],i)-1
        {m[i], m[k]} = {m[k], m[i]}
        atom t := 1/m[i][i]
        for j=i+1 to p do m[i][j] *= t end for
        for j=i+1 to n do
            t = m[j][i]
            for l=i+1 to p do m[j][l] -= t * m[i][l] end for
        end for
    end for
    for i=n to 1 by -1 do
        atom mip = m[i][p]
        for j=1 to i-1 do m[j][p] -= m[j][i] * mip end for
    end for
    return vslice(m,p)
end function
 
function network(integer n, k0, k1, sequence s)
    sequence m := repeat(repeat(0,n+1), n)
    s = split(s,'|')
    for i=1 to length(s) do
        integer {{a,b,ri}} = sq_add(scanf(s[i],"%d %d %d"),{{1,1,0}})
        atom r = 1/ri
        m[a][a] += r
        m[b][b] += r
        if a > 1 then m[a][b] -= r end if
        if b > 1 then m[b][a] -= r end if
    end for
    k0 += 1;  m[k0][k0] = 1
    k1 += 1;  m[k1][n+1] = 1
    return gauss(m)[k1]
end function

printf(1,"%.6g\n",network(7, 0, 1, "0 2 6|2 3 4|3 4 10|4 5 2|5 6 8|6 1 4|3 5 6|3 6 6|3 1 8|2 1 8"))
printf(1,"%.6g\n",network(9, 0, 8, "0 1 1|1 2 1|3 4 1|4 5 1|6 7 1|7 8 1|0 3 1|3 6 1|1 4 1|4 7 1|2 5 1|5 8 1"))
printf(1,"%.6g\n",network(16, 0, 15, "0 1 1|1 2 1|2 3 1|4 5 1|5 6 1|6 7 1|8 9 1|9 10 1|10 11 1|12 13 1|13 14 1|14 15 1|"&
                                     "0 4 1|4 8 1|8 12 1|1 5 1|5 9 1|9 13 1|2 6 1|6 10 1|10 14 1|3 7 1|7 11 1|11 15 1"))
printf(1,"%.6g\n",network(4, 0, 3, "0 1 150|0 2 50|1 3 300|2 3 250"))
```

```txt

10
1.5
1.85714
180

```



## Python


```python
from fractions import Fraction

def gauss(m):
	n, p = len(m), len(m[0])
	for i in range(n):
		k = max(range(i, n), key = lambda x: abs(m[x][i]))
		m[i], m[k] = m[k], m[i]
		t = 1 / m[i][i]
		for j in range(i + 1, p): m[i][j] *= t
		for j in range(i + 1, n):
			t = m[j][i]
			for k in range(i + 1, p): m[j][k] -= t * m[i][k]
	for i in range(n - 1, -1, -1):
		for j in range(i): m[j][-1] -= m[j][i] * m[i][-1]
	return [row[-1] for row in m]

def network(n,k0,k1,s):
	m = [[0] * (n+1) for i in range(n)]
	resistors = s.split('|')
	for resistor in resistors:
		a,b,r = resistor.split(' ')
		a,b,r = int(a), int(b), Fraction(1,int(r))
		m[a][a] += r
		m[b][b] += r
		if a > 0: m[a][b] -= r
		if b > 0: m[b][a] -= r
	m[k0][k0] = Fraction(1, 1)
	m[k1][-1] = Fraction(1, 1)
	return gauss(m)[k1]

assert 10             == network(7,0,1,"0 2 6|2 3 4|3 4 10|4 5 2|5 6 8|6 1 4|3 5 6|3 6 6|3 1 8|2 1 8")
assert 3/2            == network(3*3,0,3*3-1,"0 1 1|1 2 1|3 4 1|4 5 1|6 7 1|7 8 1|0 3 1|3 6 1|1 4 1|4 7 1|2 5 1|5 8 1")
assert Fraction(13,7) == network(4*4,0,4*4-1,"0 1 1|1 2 1|2 3 1|4 5 1|5 6 1|6 7 1|8 9 1|9 10 1|10 11 1|12 13 1|13 14 1|14 15 1|0 4 1|4 8 1|8 12 1|1 5 1|5 9 1|9 13 1|2 6 1|6 10 1|10 14 1|3 7 1|7 11 1|11 15 1")
assert 180            == network(4,0,3,"0 1 150|0 2 50|1 3 300|2 3 250")
```



## zkl

{{libheader|GSL}} GNU Scientific Library
This a tweak of [[Resistor_mesh#zkl]]

```zkl
var [const] GSL=Import.lib("zklGSL");	// libGSL (GNU Scientific Library)

fcn network(n,k0,k1,mesh){
   A:=GSL.Matrix(n,n);  // zero filled
   foreach resistor in (mesh.split("|")){
      a,b,r := resistor.split().apply("toInt");
      r=1.0/r;
      A[a,a]=A[a,a] + r;
      A[b,b]=A[b,b] + r;
      if(a>0) A[a,b]=A[a,b] - r;
      if(b>0) A[b,a]=A[b,a] - r;
   }
   A[k0,k0]=1;
   b:=GSL.Vector(n);  // zero filled
   b[k1]=1;
   A.AxEQb(b)[k1];
}
```


```zkl
network(7,0,1,"0 2 6|2 3 4|3 4 10|4 5 2|5 6 8|6 1 4|3 5 6|3 6 6|3 1 8|2 1 8")
.println();

network(3*3,0,3*3-1,"0 1 1|1 2 1|3 4 1|4 5 1|6 7 1|7 8 1|0 3 1|3 6 1|1 4 1|4 7 1|2 5 1|5 8 1")
.println();

network(4*4,0,4*4-1,"0 1 1|1 2 1|2 3 1|4 5 1|5 6 1|6 7 1|8 9 1|9 10 1|10 11 1|12 13 1|13 14 1|14 15 1|0 4 1|4 8 1|8 12 1|1 5 1|5 9 1|9 13 1|2 6 1|6 10 1|10 14 1|3 7 1|7 11 1|11 15 1")
.println();

network(4,0,3,"0 1 150|0 2 50|1 3 300|2 3 250")
.println();
```

```txt

10
1.5
1.85714
180

```

