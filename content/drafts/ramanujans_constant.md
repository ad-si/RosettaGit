+++
title = "Ramanujan's constant"
description = ""
date = 2019-08-08T23:04:16Z
aliases = []
[extra]
id = 22293
[taxonomies]
categories = []
tags = []
+++

{{draft task}}

Calculate Ramanujan's constant (as described on the [http://oeis.org/wiki/Ramanujan%27s_constant OEIS site]) with at least
32 digits of precision, by the method of your choice.  Optionally, if using the 𝑒**(π*√''x'') approach,
show that when evaluated with the last four [https://en.wikipedia.org/wiki/Heegner_number Heegner numbers]
the result is ''almost'' an integer.

=={{header|Fōrmulæ}}==

In [https://wiki.formulae.org/Ramanujan%27s_constant this] page you can see the solution of this task.

Fōrmulæ programs are not textual, visualization/edition of programs is done showing/manipulating structures but not text ([http://wiki.formulae.org/Editing_F%C5%8Drmul%C3%A6_expressions more info]). Moreover, there can be multiple visual representations of the same program. Even though it is possible to have textual representation &mdash;i.e. XML, JSON&mdash; they are intended for transportation effects more than visualization and edition.

The option to show Fōrmulæ programs and their results is showing images. Unfortunately images cannot be uploaded in Rosetta Code.


## Go

{{libheader|bigfloat}}
The standard library's math/big.Float type lacks an exponentiation function and so I have had to use an external library to provide this function.

Also the math.Pi built in constant is not accurate enough to be used with big.Float and so I have used a more accurate string representation instead.

```go
package main

import (
    "fmt"
    "github.com/ALTree/bigfloat"
    "math/big"
)

const (
    prec = 256 // say
    ps   = "3.1415926535897932384626433832795028841971693993751058209749445923078164"
)

func q(d int64) *big.Float {
    pi, _ := new(big.Float).SetPrec(prec).SetString(ps)
    t := new(big.Float).SetPrec(prec).SetInt64(d)
    t.Sqrt(t)
    t.Mul(pi, t)
    return bigfloat.Exp(t)
}

func main() {
    fmt.Println("Ramanujan's constant to 32 decimal places is:")
    fmt.Printf("%.32f\n", q(163))
    heegners := [4][2]int64{
        {19, 96},
        {43, 960},
        {67, 5280},
        {163, 640320},
    }
    fmt.Println("\nHeegner numbers yielding 'almost' integers:")
    t := new(big.Float).SetPrec(prec)
    for _, h := range heegners {
        qh := q(h[0])
        c := h[1]*h[1]*h[1] + 744
        t.SetInt64(c)
        t.Sub(t, qh)
        fmt.Printf("%3d: %51.32f ≈ %18d (diff: %.32f)\n", h[0], qh, c, t)
    }
}
```


{{out}}

```txt

Ramanujan's constant to 32 decimal places is:
262537412640768743.99999999999925007259719818568888

Heegner numbers yielding 'almost' integers:
 19:             885479.77768015431949753789348171962682 ≈             885480 (diff: 0.22231984568050246210651828037318)
 43:          884736743.99977746603490666193746207858538 ≈          884736744 (diff: 0.00022253396509333806253792141462)
 67:       147197952743.99999866245422450682926131257863 ≈       147197952744 (diff: 0.00000133754577549317073868742137)
163: 262537412640768743.99999999999925007259719818568888 ≈ 262537412640768744 (diff: 0.00000000000074992740280181431112)

```




## Julia


```julia


julia> a = BigFloat(MathConstants.e^(BigFloat(pi)))^(BigFloat(163.0)^0.5)
2.625374126407687439999999999992500725971981856888793538563373369908627075373427e+17

julia> 262537412640768744 - a
7.499274028018143111206461436626630091372924626572825942241598957614307213309258e-13


```



## Perl


### Direct calculation

{{trans|Sidef}}

```perl
use strict;
use warnings;
use Math::AnyNum;

sub ramanujan_const {
    my ($x, $decimals) = @_;

    $x = Math::AnyNum->new($x);
    my $prec = (Math::AnyNum->pi * $x->sqrt)/log(10) + $decimals + 1;
    local $Math::AnyNum::PREC = 4*$prec->round->numify;

    exp(Math::AnyNum->pi * $x->sqrt)->round(-$decimals)->stringify;
}

my $decimals = 100;
printf("Ramanujan's constant to $decimals decimals:\n%s\n\n",
    ramanujan_const(163, $decimals));

print "Heegner numbers yielding 'almost' integers:\n";
my @tests = (19, 96, 43, 960, 67, 5280, 163, 640320);

while (@tests) {
    my ($h, $x) = splice(@tests, 0, 2);
    my $c = ramanujan_const($h, 32);
    my $n = Math::AnyNum::ipow($x, 3) + 744;
    printf("%3s: %51s ≈ %18s (diff: %s)\n", $h, $c, $n, ($n - $c)->round(-32));
}
```

{{out}}

```txt

Ramanujan's constant to 100 decimals:
262537412640768743.9999999999992500725971981856888793538563373369908627075374103782106479101186073129511813461860645042

Heegner numbers yielding 'almost' integers:
 19:             885479.77768015431949753789348171962682 ≈             885480 (diff: 0.22231984568050246210651828037318)
 43:          884736743.99977746603490666193746207858538 ≈          884736744 (diff: 0.00022253396509333806253792141462)
 67:       147197952743.99999866245422450682926131257863 ≈       147197952744 (diff: 0.00000133754577549317073868742137)
163: 262537412640768743.99999999999925007259719818568888 ≈ 262537412640768744 (diff: 0.00000000000074992740280181431112)

```



### Continued fractions

{{trans|Perl 6}}

```perl
use strict;
use Math::AnyNum <as_dec rat>;

sub continued_fr {
    my ($a, $b, $n) = (@_[0,1], $_[2] // 100);
    $a->() + ($n && $b->() / continued_fr($a, $b, $n-1));
}

my $r163 = continued_fr do {my $n; sub {$n++ ? 2*12 : 12 }},        do {my $n; sub { rat 19 }}, 40;
my $pi   = continued_fr do {my $n; sub {$n++ ? 1 + 2*($n-2) : 0 }}, do {my $n; sub { rat($n++ ? ($n>2 ? ($n-1)**2 : 1) : 4)}}, 140;
my $p    = $pi * $r163;
my $R    = 1 + $p / continued_fr do { my $n; sub { $n++ ? $p+($n+0) : 1 } }, do {my $n; sub { $n++; -1*$n*$p }}, 180;

printf "Ramanujan's constant\n%s\n", as_dec($R,58);

```

{{out}}

```txt
Ramanujan's constant
262537412640768743.9999999999992500725971981856888793538563
```



## Perl 6


### Iterative calculations

To generate a high-precision value for Ramanujan's constant, code is borrowed from three other Rosettacode tasks 
(with some modifications) for performing calculations of 
[http://rosettacode.org/wiki/Arithmetic-geometric_mean/Calculate_Pi the value of π],  
[http://rosettacode.org/wiki/Calculating_the_value_of_e Euler's number], and 
[http://rosettacode.org/wiki/Arithmetic-geometric_mean/Integer_roots integer roots]. Additional custom routines for exponentiation are used to ensure all computations are done with rationals, specifically <tt>FatRat</tt>s (rational numbers stored with arbitrary size numerator and denominator). The module <tt>Rat::Precise</tt> makes it simple to display these to a configurable precision.

```perl6
use Rat::Precise;

# set the degree of precision for calculations
constant D = 54;
constant d = 15;

# two versions of exponentiation where base and exponent are both FatRat
multi infix:<**> (FatRat $base, FatRat $exp where * >= 1 --> FatRat) {
    2 R** $base**($exp/2);
}

multi infix:<**> (FatRat $base, FatRat $exp where * <  1 --> FatRat) {
    constant ε = 10**-D;
    my $low  = 0.FatRat;
    my $high = 1.FatRat;
    my $mid  = $high / 2;
    my $acc  = my $sqr = sqrt($base);

    while (abs($mid - $exp) > ε) {
      $sqr = sqrt($sqr);
      if ($mid <= $exp) { $low  = $mid; $acc *=   $sqr }
      else              { $high = $mid; $acc *= 1/$sqr }
      $mid = ($low + $high) / 2;
    }
    $acc.substr(0, D).FatRat;
}

# calculation of π
sub π (--> FatRat) {
    my ($a, $n) = 1, 1;
    my $g = sqrt 1/2.FatRat;
    my $z = .25;
    my $pi;

    for ^d {
        given [ ($a + $g)/2, sqrt $a * $g ] {
            $z -= (.[0] - $a)**2 * $n;
            $n += $n;
            ($a, $g) = @$_;
            $pi = ($a ** 2 / $z).substr: 0, 2 + D;
        }
    }
    $pi.FatRat;
}

multi sqrt(FatRat $r --> FatRat) {
    FatRat.new: sqrt($r.nude[0] * 10**(D*2) div $r.nude[1]), 10**D;
}

# integer roots
multi sqrt(Int $n) {
    my $guess = 10**($n.chars div 2);
    my $iterator = { ( $^x + $n div ($^x) ) div 2 };
    my $endpoint = { $^x == $^y|$^z };
    min ($guess, $iterator … $endpoint)[*-1, *-2];
}

# 'cosmetic' cover to upgrade input to FatRat sqrt
sub prefix:<√> (Int $n) { sqrt($n.FatRat) }

# calculation of 𝑒
sub postfix:<!> (Int $n) { (constant f = 1, |[\*] 1..*)[$n] }
sub 𝑒 (--> FatRat) { sum map { FatRat.new(1,.!) }, ^D }

# inputs, and their difference, formatted decimal-aligned
sub format ($a,$b) {
    sub pad ($s) { ' ' x ((34 - d - 1) - ($s.split(/\./)[0]).chars) }
    my $c = $b.precise(d, :z);
    my $d = ($a-$b).precise(d, :z);
    join "\n",
        (sprintf "%11s {pad($a)}%s\n", 'Int', $a) ~
        (sprintf "%11s {pad($c)}%s\n", 'Heegner', $c) ~
        (sprintf "%11s {pad($d)}%s\n", 'Difference', $d)
}

# override built-in definitions
constant π = &π();
constant 𝑒 = &𝑒();

my $Ramanujan = 𝑒**(π*√163);
say "Ramanujan's constant to 32 decimal places:\nActual:     " ~
    "262537412640768743.99999999999925007259719818568888\n" ~
    "Calculated: ", $Ramanujan.precise(32, :z), "\n";

say "Heegner numbers yielding 'almost' integers";
for 19, 96, 43, 960, 67, 5280, 163, 640320 -> $heegner, $x {
    my $almost = 𝑒**(π*√$heegner);
    my $exact  = $x**3 + 744;
    say format($exact, $almost);
}
```

{{out}}

```txt
Ramanujan's constant to 32 decimal places:
Actual:     262537412640768743.99999999999925007259719818568888
Calculated: 262537412640768743.99999999999925007259719818568888

Heegner numbers yielding 'almost' integers
        Int             885480
    Heegner             885479.777680154319498
 Difference                  0.222319845680502

        Int          884736744
    Heegner          884736743.999777466034907
 Difference                  0.000222533965093

        Int       147197952744
    Heegner       147197952743.999998662454225
 Difference                  0.000001337545775

        Int 262537412640768744
    Heegner 262537412640768743.999999999999250
 Difference                  0.000000000000750
```



### Continued fractions

Ramanujan's constant can also be generated to an arbitrary precision using standard   [https://en.wikipedia.org/wiki/Generalized_continued_fraction continued fraction formulas] for each component of the 𝑒**(π*√163) expression. Substantially slower than the first method.

```perl6
use Rat::Precise;

sub continued-fraction($n, :@a, :@b) {
    my $x = @a[0].FatRat;
    $x = @a[$_ - 1] + @b[$_] / $x for reverse 1 ..^ $n;
    $x;
}

#`{ √163 } my $r163 =           continued-fraction( 50, :a(12,|((2*12) xx *)),      :b(19 xx *));
#`{ π    } my $pi   =         4*continued-fraction(140, :a( 0,|(1, 3 ... *)),       :b(4, 1, |((1, 2, 3 ... *) X** 2)));
#`{ e**x } my $R    = 1 + ($_ / continued-fraction(170, :a( 1,|(2+$_, 3+$_ ... *)), :b(Nil,  |(-1*$_, -2*$_ ... *)  ))) given $r163*$pi;

say "Ramanujan's constant to 32 decimal places:\n", $R.precise(32);
```

{{out}}

```txt
Ramanujan's constant to 32 decimal places:
262537412640768743.99999999999925007259719818568888
```



## Phix

{{trans|Go}}
{{libheader|mpfr}}

```Phix
include mpfr.e

constant dp_rqd = 18+32+2,  -- (18 before, 32 after, plus 2 for kicks.)
         precision_rqd = mpz_sizeinbase(mpz_init(repeat('9',dp_rqd)),2) 

function q(integer d)
    mpfr pi = mpfr_init(precision:=precision_rqd)
    mpfr_const_pi(pi)
    mpfr t = mpfr_init(d,precision:=precision_rqd)
    mpfr_sqrt(t,t)
    mpfr_mul(t,pi,t)
    mpfr_exp(t,t)
    return t
end function
 
printf(1,"Ramanujan's constant to 32 decimal places is:\n")
mpfr_printf(1, "%.32Rf\n", q(163))
sequence heegners = {{19, 96},
                     {43, 960},
                     {67, 5280},
                     {163, 640320},
                    }
printf(1,"\nHeegner numbers yielding 'almost' integers:\n")
mpfr t = mpfr_init(precision:=precision_rqd), qh
mpz c = mpz_init()
for i=1 to length(heegners) do
    integer {h0,h1} = heegners[i]
    qh = q(h0)
    mpz_ui_pow_ui(c,h1,3)
    mpz_add_ui(c,c,744)
    mpfr_set_z(t,c)
    mpfr_sub(t,t,qh)
    string qhs = mpfr_sprintf("%51.32Rf",qh),
           cs = mpz_get_str(c),
           ts = mpfr_sprintf("%.32Rf",t)
    printf(1,"%3d: %s ~= %18s (diff: %s)\n", {h0, qhs, cs, ts})
end for
```

{{out}}

```txt

Ramanujan's constant to 32 decimal places is:
262537412640768743.99999999999925007259719818568888

Heegner numbers yielding 'almost' integers:
 19:             885479.77768015431949753789348171962682 ~=             885480 (diff: 0.22231984568050246210651828037318)
 43:          884736743.99977746603490666193746207858538 ~=          884736744 (diff: 0.00022253396509333806253792141462)
 67:       147197952743.99999866245422450682926131257863 ~=       147197952744 (diff: 0.00000133754577549317073868742137)
163: 262537412640768743.99999999999925007259719818568888 ~= 262537412640768744 (diff: 0.00000000000074992740280181431112)

```



## REXX

Instead of calculating   <big> '''e''' </big>   and   <big><big><math>\pi</math></big></big>   to some arbitrary length,   it was easier to just include those two constants with   '''201'''   decimal digits   (which is the amount of decimal digits used for the calculations).   The results are displayed   (right justified)   with one half of that number of decimal digits past the decimal point. 

```rexx
/*REXX pgm displays Ramanujan's constant to at least  100  decimal digits of precision. */
d= min( length(pi()), length(e()) )  - length(.) /*calculate max #decimal digs supported*/
parse arg digs sDigs . 1 . . $                   /*obtain optional arguments from the CL*/
if  digs=='' |  digs==","  then  digs= d         /*Not specified?  Then use the default.*/
if sDigs=='' | sDigs==","  then sDigs= d % 2     /* "      "         "   "   "      "   */
if     $=''  |     $=","   then $= 19 43 67 163  /* "      "         "   "   "      "   */
 digs= min( digs, d)                             /*the minimum decimal digs for calc.   */
sDigs= min(sDigs, d)                             /* "     "       "      "      display.*/
numeric digits digs                              /*inform REXX how many dec digs to use.*/
say "The value of Ramanujan's constant calculated with " d ' decimal digits of precision.'
say "shown with "    sDigs    ' decimal digits past the decimal point:'
say
       do  j=1  for words($);   #= word($, j)    /*process each of the Heegner numbers. */
       say 'When using the Heegner number: '  #  /*display which Heegner # is being used*/
       z= exp(pi * sqrt(#) )                     /*perform some heavy lifting here.     */
       say format(z, 25, sDigs);           say   /*display a limited amount of dec digs.*/
       end   /*j*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
pi:    pi= 3.1415926535897932384626433832795028841971693993751058209749445923078164062862,
           || 089986280348253421170679821480865132823066470938446095505822317253594081284,
           || 8111745028410270193852110555964462294895493038196;     return pi
/*──────────────────────────────────────────────────────────────────────────────────────*/
e:     e = 2.7182818284590452353602874713526624977572470936999595749669676277240766303535,
           || 475945713821785251664274274663919320030599218174135966290435729003342952605,
           || 9563073813232862794349076323382988075319525101901;     return  e
/*──────────────────────────────────────────────────────────────────────────────────────*/
exp:   procedure; parse arg x;  ix= x%1;  if abs(x-ix)>.5  then ix= ix + sign(x);  x= x-ix
       z=1;  _=1;   w=z;     do j=1; _= _*x/j;  z=(z+_)/1; if z==w  then leave;  w=z;  end
       if z\==0  then z= z * e() ** ix;                                         return z/1
/*──────────────────────────────────────────────────────────────────────────────────────*/
sqrt:  procedure; parse arg x;  if x=0  then return 0;  d=digits();  h=d+6; numeric digits
       numeric form; m.=9; parse value format(x,2,1,,0) 'E0' with g 'E' _ .;  g=g*.5'e'_%2
         do j=0  while h>9;      m.j=h;               h=h % 2  +  1;   end /*j*/
         do k=j+5  to 0  by -1;  numeric digits m.k;  g=(g+x/g) * .5;  end /*k*/; return g
```

{{out|output|text=  when using the default inputs:}}

```txt

The value of Ramanujan's constant calculated with  201  decimal digits of precision.
shown with  100  decimal digits past the decimal point:

When using the Heegner number:  19
                   885479.7776801543194975378934817196268207142865018553571526577110128809842286637202423189990118182067775711

When using the Heegner number:  43
                884736743.9997774660349066619374620785853768473991271391609175146278344881148747592189635643106023717101372606

When using the Heegner number:  67
             147197952743.9999986624542245068292613125786285081833125038167126333712821051229509988315235020413792423533706290

When using the Heegner number:  163
       262537412640768743.9999999999992500725971981856888793538563373369908627075374103782106479101186073129511813461860645042

```



## Sidef


```ruby
func ramanujan_const(x, decimals=32) {
    local Num!PREC = *"#{4*round((Num.pi*√x)/log(10) + decimals + 1)}"
    exp(Num.pi * √x) -> round(-decimals).to_s
}

var decimals = 100
printf("Ramanujan's constant to #{decimals} decimals:\n%s\n\n",
     ramanujan_const(163, decimals))

say "Heegner numbers yielding 'almost' integers:"
[19, 96, 43, 960, 67, 5280, 163, 640320].each_slice(2, {|h,x|
    var c = ramanujan_const(h, 32)
    var n = (x**3 + 744)
    printf("%3s: %51s ≈ %18s (diff: %s)\n", h, c, n, n-Num(c))
})
```

{{out}}

```txt

Ramanujan's constant to 100 decimals:
262537412640768743.9999999999992500725971981856888793538563373369908627075374103782106479101186073129511813461860645042

Heegner numbers yielding 'almost' integers:
 19:             885479.77768015431949753789348171962682 ≈             885480 (diff: 0.22231984568050246210651828037318)
 43:          884736743.99977746603490666193746207858538 ≈          884736744 (diff: 0.00022253396509333806253792141462)
 67:       147197952743.99999866245422450682926131257863 ≈       147197952744 (diff: 0.00000133754577549317073868742137)
163: 262537412640768743.99999999999925007259719818568888 ≈ 262537412640768744 (diff: 0.00000000000074992740280181431112)

```

