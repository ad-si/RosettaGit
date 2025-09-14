+++
title = "Suffixation of decimal numbers"
description = ""
date = 2019-07-10T18:34:48Z
aliases = []
[extra]
id = 22055
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
  "related_tasks",
  "rexx",
  "vba",
  "zkl",
]
+++

'''Suffixation''':   a letter or a group of letters added to the end of a word to change its meaning.
:::::::       ─────   or, as used herein   ─────
'''Suffixation''':   the addition of a metric or "binary" metric suffix to a number, with/without rounding.


## Task

Write a function(s) to append (if possible)   a metric   or   a "binary" metric   suffix to a
number   (displayed in decimal).

The number may be rounded   (as per user specification)   (via shortening of the number when the number of
digits past the decimal point are to be used).


;Task requirements:
::*   write a function (or functions) to add   (if possible)   a suffix to a number
::*   the function(s) should be able to express the number (possibly with a suffix) in as many decimal digits as specified
::*   the sign should be preserved   (if present)
::*   the number may have commas within the number   (the commas need not be preserved)
::*   the number may have a decimal point and/or an exponent as in:   -123.7e-01
::*   the suffix that might be appended should be in uppercase;   however, the   '''i'''   should be in lowercase
::*   support:
::::*   the            metric suffixes:   '''K  M  G  T  P  E  Z  Y  X  W  V  U'''
::::*   the binary metric suffixes:   '''Ki Mi Gi Ti Pi Ei Zi Yi Xi Wi Vi Ui'''
::::*   the (full name) suffix:   '''googol'''   (lowercase)   (equal to '''1e100''')     (optional)
::::*   a number of decimal digits past the decimal point   (with rounding).   The default is to display all significant digits
::*   validation of the (supplied/specified) arguments is optional but recommended
::*   display   (with identifying text):
::::*   the original number   (with identifying text)
::::*   the number of digits past the decimal point being used   (or none, if not specified)
::::*   the type of suffix being used   (metric or "binary" metric)
::::*   the (new) number with the appropriate   (if any)   suffix
::::*   all output here on this page


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


;"Binary" suffixes to be supported   (whether or not they're officially sanctioned):
      '''Ki'''    multiply the number by  '''2^10'''              kibi      (1,024)
      '''Mi'''    multiply the number by  '''2^20'''              mebi      (1,048,576)
      '''Gi'''    multiply the number by  '''2^30'''              gibi      (1,073,741,824)
      '''Ti'''    multiply the number by  '''2^40'''              tebi      (1,099,571,627,776)
      '''Pi'''    multiply the number by  '''2^50'''              pebi      (1,125,899,906,884,629)
      '''Ei'''    multiply the number by  '''2^60'''              exbi      (1,152,921,504,606,846,976)
      '''Zi'''    multiply the number by  '''2^70'''              zebi      (1,180,591,620,717,411,303,424)
      '''Yi'''    multiply the number by  '''2^80'''              yobi      (1,208,925,819,614,629,174,706,176)
      '''Xi'''    multiply the number by  '''2^90'''              xebi      (1,237,940,039,285,380,274,899,124,224)
      '''Wi'''    multiply the number by  '''2^100'''             webi      (1,267,650,600,228,229,401,496,703,205,376)
      '''Vi'''    multiply the number by  '''2^110'''             vebi      (1,298,074,214,633,706,907,132,624,082,305,024)
      '''Ui'''    multiply the number by  '''2^120'''             uebi      (1,329,227,995,784,915,872,903,807,060,280,344,576)


;For instance, with this pseudo─code:
                                  /* 1st arg: the number to be transformed.*/
                                  /* 2nd arg: # digits past the dec. point.*/
                                  /* 3rd arg: the type of suffix to use.   */
                                  /*         2   indicates "binary" suffix.*/
                                  /*        10   indicates  decimal suffix.*/
      a = '456,789,100,000,000'   /* "A"  has  eight  trailing zeros.      */
      say ' aa=' suffize(a)       /* Display a suffized number to terminal.*/
                                  /* The  "1"   below shows one decimal ···*/
                                  /*         digit past the decimal point. */
      n = suffize(a, 1)           /* SUFFIZE  is the function name.        */
      n = suffize(a, 1, 10)       /* (identical to the above statement.)   */
      say '  n=' n                /* Display value of  N  to terminal.     */
                                  /* Note the rounding that occurs.        */
      f = suffize(a, 1,  2)       /* SUFFIZE  with one fractional digit    */
      say '  f=' f                /* Display value of  F  to terminal.     */
                                  /* Display value in "binary" metric.     */
      bin = suffize(a, 5, 2)      /* SUFFIZE with binary metric suffix.    */
      say 'bin=' bin              /* Display value of  BIN  to terminal.   */
      win = suffize(a, 0, 2)      /* SUFFIZE with binary metric suffix.    */
      say 'win=' win              /* Display value of  WIN  to terminal.   */
      xvi = ' +16777216 '         /* this used to be a big computer ···    */
      big = suffize(xvi, , 2)     /* SUFFIZE with binary metric suffix.    */
      say 'big=' big              /* Display value of  BIG  to terminal.   */
would display:
       aa= 456.7891T
        n= 456.8T
        f= 415.4Ti
      bin= 415.44727Ti
      win= 415Ti
      big= 16Mi


;Use these test cases:
                87,654,321
               -998,877,665,544,332,211,000      3
               +112,233                          0
                16,777,216                       1
                456,789,100,000,000              2
                456,789,100,000,000              2      10
                456,789,100,000,000              5       2
                456,789,100,000.000e+00          0      10
               +16777216                         ,       2
                1.2e101
                (your primary disk free space)   1                  ◄■■■■■■■ optional


Use whatever parameterizing your computer language supports,   and it's permitted to create as many
separate functions as are needed   (if needed)   if   function arguments aren't allowed to
be omitted or varied.


## Related tasks

:*   [[Numerical and alphabetical suffixes]]





## Go

As go doesn't support either function overloading or optional arguments, we just pass a single string to the suffize function and then split out what we need.

```go
package main

import (
    "fmt"
    "math/big"
    "strconv"
    "strings"
)

var suffixes = " KMGTPEZYXWVU"
var ggl = googol()

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

func suffize(arg string) {
    fields := strings.Fields(arg)
    a := fields[0]
    if a == "" {
        a = "0"
    }
    var places, base int
    var frac, radix string
    switch len(fields) {
    case 1:
        places = -1
        base = 10
    case 2:
        places, _ = strconv.Atoi(fields[1])
        base = 10
        frac = strconv.Itoa(places)
    case 3:
        if fields[1] == "," {
            places = 0
            frac = ","
        } else {
            places, _ = strconv.Atoi(fields[1])
            frac = strconv.Itoa(places)
        }
        base, _ = strconv.Atoi(fields[2])
        if base != 2 && base != 10 {
            base = 10
        }
        radix = strconv.Itoa(base)
    }
    a = strings.Replace(a, ",", "", -1) // get rid of any commas
    sign := ""
    if a[0] == '+' || a[0] == '-' {
        sign = string(a[0])
        a = a[1:] // remove any sign after storing it
    }
    b := new(big.Float).SetPrec(500)
    d := new(big.Float).SetPrec(500)
    b.SetString(a)
    g := false
    if b.Cmp(ggl) >= 0 {
        g = true
    }
    if !g && base == 2 {
        d.SetUint64(1024)
    } else if !g && base == 10 {
        d.SetUint64(1000)
    } else {
        d.Set(ggl)
    }
    c := 0
    for b.Cmp(d) >= 0 && c < 12 { // allow b >= 1K if c would otherwise exceed 12
        b.Quo(b, d)
        c++
    }
    var suffix string
    if !g {
        suffix = string(suffixes[c])
    } else {
        suffix = "googol"
    }
    if base == 2 {
        suffix += "i"
    }
    fmt.Println("   input number =", fields[0])
    fmt.Println("  fraction digs =", frac)
    fmt.Println("specified radix =", radix)
    fmt.Print("     new number = ")
    if places >= 0 {
        fmt.Printf("%s%.*f%s\n", sign, places, b, suffix)
    } else {
        fmt.Printf("%s%s%s\n", sign, b.Text('g', 50), suffix)
    }
    fmt.Println()
}

func main() {
    tests := []string{
        "87,654,321",
        "-998,877,665,544,332,211,000      3",
        "+112,233                          0",
        "16,777,216                        1",
        "456,789,100,000,000",
        "456,789,100,000,000               2      10",
        "456,789,100,000,000               5       2",
        "456,789,100,000.000e+00           0      10",
        "+16777216                         ,       2",
        "1.2e101",
        "446,835,273,728                   1",
        "1e36",
        "1e39", // there isn't a big enough suffix for this one but it's less than googol
    }
    for _, test := range tests {
        suffize(test)
    }
}
```


```txt

   input number = 87,654,321
  fraction digs = 
specified radix = 
     new number = 87.654321M

   input number = -998,877,665,544,332,211,000
  fraction digs = 3
specified radix = 
     new number = -998.878E

   input number = +112,233
  fraction digs = 0
specified radix = 
     new number = +112K

   input number = 16,777,216
  fraction digs = 1
specified radix = 
     new number = 16.8M

   input number = 456,789,100,000,000
  fraction digs = 
specified radix = 
     new number = 456.7891T

   input number = 456,789,100,000,000
  fraction digs = 2
specified radix = 10
     new number = 456.79T

   input number = 456,789,100,000,000
  fraction digs = 5
specified radix = 2
     new number = 415.44727Ti

   input number = 456,789,100,000.000e+00
  fraction digs = 0
specified radix = 10
     new number = 457G

   input number = +16777216
  fraction digs = ,
specified radix = 2
     new number = +16Mi

   input number = 1.2e101
  fraction digs = 
specified radix = 
     new number = 12googol

   input number = 446,835,273,728
  fraction digs = 1
specified radix = 
     new number = 446.8G

   input number = 1e36
  fraction digs = 
specified radix = 
     new number = 1U

   input number = 1e39
  fraction digs = 
specified radix = 
     new number = 1000U

```




## Julia


```julia
using Printf

const suf = Dict{BigInt, String}(BigInt(1) => "", BigInt(10)^100 => "googol",
    BigInt(10)^3 => "K", BigInt(10)^6 => "M", BigInt(10)^9 => "G", BigInt(10)^12 => "T",
    BigInt(10)^15 => "P", BigInt(10)^18 => "E", BigInt(10)^21 => "Z", BigInt(10)^24 => "Y",
    BigInt(10)^27 => "X", BigInt(10)^30 => "W", BigInt(10)^33 => "V", BigInt(10)^36 => "U")
const binsuf = Dict{BigInt, String}(BigInt(1) => "", BigInt(10)^100 => "googol",
    BigInt(2)^10 => "Ki", BigInt(2)^20 => "Mi", BigInt(2)^30 => "Gi", BigInt(2)^40 => "Ti",
    BigInt(2)^50 => "Pi", BigInt(2)^60 => "Ei", BigInt(2)^70 => "Zi", BigInt(2)^80 => "Yi",
    BigInt(2)^90 => "Xi", BigInt(2)^100 => "Wi", BigInt(2)^110 => "Vi", BigInt(2)^120 => "Ui")
const googol = BigInt(10)^100

function choosedivisor(n, base10=true)
    if n > 10 * googol
        return googol
    end
    s = base10 ? sort(collect(keys(suf))) : sort(collect(keys(binsuf)))
    (i = findfirst(x -> x > 0.001 * n, s)) == nothing ? s[end] : s[i]
end

pretty(x) = (floor(x) == x) ? string(BigInt(x)) : replace(@sprintf("%f", x), r"0+$" => "")

function suffize(val::String, rounddigits=-1, suffixbase=10)
    if val[1] == '-'
        isneg = true
        val = val[2:end]
    else
        isneg = false
        if val[1] == '+'
            val = val[2:end]
        end
    end
    val = replace(val, r"," => "")
    nval = (b = tryparse(BigInt, val)) == nothing ? parse(BigFloat, val) : b
    b = choosedivisor(nval, suffixbase != 2)
    mantissa = nval / b
    if rounddigits >= 0
        mantissa = round(mantissa, digits=rounddigits)
    end
    (isneg ? "-" : "") * pretty(mantissa) * (suffixbase == 10 ? suf[b] : binsuf[b])
end
suffize(val::Number, rounddigits=-1, suffixbase=10) = suffize(string(val), rounddigits, suffixbase)

testnumbers = [
   ["87,654,321"],
   ["-998,877,665,544,332,211,000", 3],
   ["+112,233", 0],
   ["16,777,216", 1],
   ["456,789,100,000,000"],
   ["456,789,100,000,000", 2, 10],
   ["456,789,100,000,000", 5, 2],
   ["456,789,100,000.000e+00", 0],
   ["+16777216", 0, 2],
   ["1.2e101"]]

for l in testnumbers
    n = length(l)
    s = (n == 1) ? suffize(l[1]) : (n == 2) ? suffize(l[1], l[2]) : suffize(l[1], l[2], l[3])
    println(lpad(l[1], 30), (n > 1) ? lpad(l[2], 3) : "   ",
                            (n > 2) ? lpad(l[3], 3) : "   ", " : ", s)
end

```
```txt

                    87,654,321       : 87.654321M
  -998,877,665,544,332,211,000  3    : -998.878E
                      +112,233  0    : 112K
                    16,777,216  1    : 16.8M
           456,789,100,000,000       : 456.7891T
           456,789,100,000,000  2 10 : 456.79T
           456,789,100,000,000  5  2 : 415.44727Ti
       456,789,100,000.000e+00  0    : 457G
                     +16777216  0  2 : 16Mi
                       1.2e101       : 12googol

```



## Perl

```perl
use List::Util qw(min max first);

sub sufficate {
    my($val, $type, $round) = @_;
    $type //= 'M';
   if ($type =~ /^\d$/) { $round = $type; $type = 'M' }

    my $s = '';
    if (substr($val,0,1) eq '-') { $s = '-'; $val = substr $val, 1 }
    $val =~ s/,//g;
    if ($val =~ m/e/i) {
        my ($m,$e) = split /[eE]/, $val;
        $val = ($e < 0) ? $m * 10**-$e : $m * 10**$e;
    }

    my %s;
    if ($type eq 'M') {
        my @x = qw<K M G T P E Z Y X W V U>;
        $s{$x[$_]} = 1000 * 10 ** ($_*3) for 0..$#x
    } elsif ($type eq 'B') {
        my @x = qw<Ki Mi Gi Ti Pi Ei Zi Yi Xi Wi Vi Ui>;
        $s{$x[$_]} = 2 ** (10*($_+1)) for 0..$#x
    } elsif ($type eq 'G') {
        $s{'googol'} = 10**100
    } else {
        return 'What we have here is a failure to communicate...'
    }

    my $k;
    if (abs($val) < (my $m = min values %s)) {
        $k = first { $s{$_} == $m } keys %s;
    } elsif (abs($val) > (my $x = max values %s)) {
        $k = first { $s{$_} == $x } keys %s;
    } else {
        for my $key (sort { $s{$a} <=> $s{$b} } keys %s) {
            next unless abs($val)/$s{$key} < min values %s;
            $k = $key;
            last;
        }
    }

    my $final = abs($val)/$s{$k};
    $final = round($final,$round) if defined $round;
    $s . $final . $k
}

sub round {
    my($num,$dig) = @_;
    if    ($dig == 0) { int 0.5 + $num }
    elsif ($dig  < 0) { 10**-$dig * int(0.5 + $num/10**-$dig) }
    else              { my $fmt = '%.' . $dig . 'f'; sprintf $fmt, $num }
}

sub comma {
    my($i) = @_;
    my ($whole, $frac) = split /\./, $i;
    (my $s = reverse $whole) =~ s/(.{3})/$1,/g;
    ($s = reverse $s) =~ s/^,//;
    $frac = $frac.defined ? ".$frac" : '';
    return "$s$frac";
}

my @tests = (
   '87,654,321',
   '-998,877,665,544,332,211,000 3',
   '+112,233 0',
   '16,777,216 1',
   '456,789,100,000,000',
   '456,789,100,000,000 M 2',
   '456,789,100,000,000 B 5',
   '456,789,100,000.000e+00 M 0',
   '+16777216 B',
   '1.2e101 G',
   '347,344 M -2', # round to -2 past the decimal
   '1122334455 Q', # bad unit type example
);

printf "%33s : %s\n", $_, sufficate(split ' ', $_) for @tests;
```

```txt
                       87,654,321 : 87.654321M
   -998,877,665,544,332,211,000 3 : -998.878E
                       +112,233 0 : 112K
                     16,777,216 1 : 16.8M
              456,789,100,000,000 : 456.7891T
          456,789,100,000,000 M 2 : 456.79T
          456,789,100,000,000 B 5 : 415.44727Ti
      456,789,100,000.000e+00 M 0 : 457G
                      +16777216 B : 16Mi
                        1.2e101 G : 12googol
                     347,344 M -2 : 300K
                     1122334455 Q : What we have here is a failure to communicate...
```



## Perl 6

Pass in a number string, optionally a type, and optionally the number of digits to round to.

The types supported are B, M & G for binary, metric or gigantic. (At this point, the only gigantic unit is googol, so maybe it stands for googol. ¯\_(ツ)_/¯ )

If no type is specified, M (metric) is assumed.

If you desire the number to be rounded, pass in a number representing the placed past the decimal to round to. If you pass in a negative number for rounding, it will round to a negative number of places past the decimal. 


```perl6
sub sufficate ($val is copy, $type is copy = 'M', $round is copy = Any) {
   if +$type ~~ Int { $round = $type; $type = 'M' }
   my $s = '';
   if $val.substr(0,1) eq '-' { $s = '-'; $val.=substr(1) }
   $val.=subst(',', '', :g);
   if $val ~~ m:i/'e'/ {
       my ($m,$e) = $val.split(/<[eE]>/);
       $val = ($e < 0)
           ?? $m * FatRat.new(1,10**-$e)
           !! $m * 10**$e;
   }
   my %s = do given $type {
       when 'M' { <K M G T P E Z Y X W V U> Z=> (1000, * * 1000 … *) }
       when 'B' { <Ki Mi Gi Ti Pi Ei Zi Yi Xi Wi Vi Ui> Z=> (1024, * * 1024 … *) }
       when 'G' { googol => 10**100 }
       default { return 'What we have here is a failure to communicate...' }
   }
   my $k = do given $val {
       when .abs < (my $m = min %s.values) { %s.first( *.value == $m ).key };
       when .abs > (my $x = max %s.values) { %s.first( *.value == $x ).key };
       default { %s.sort(*.value).first({$val.abs/%s{$_.key} < min %s.values}).key}
   }
   $round.defined
       ?? $s ~ comma(($val.abs/%s{$k}).round(10**-$round)) ~ $k
       !! $s ~ comma($val.abs/%s{$k}) ~ $k
}

sub comma ($i is copy) {
    my $s = $i < 0 ?? '-' !! '';
    my ($whole, $frac) = $i.split('.');
    $frac = $frac.defined ?? ".$frac" !! '';
    $s ~ $whole.abs.flip.comb(3).join(',').flip ~ $frac
}

## TESTING

my @tests =
   '87,654,321',
   '-998,877,665,544,332,211,000 3',
   '+112,233 0',
   '16,777,216 1',
   '456,789,100,000,000',
   '456,789,100,000,000 M 2',
   '456,789,100,000,000 B 5',
   '456,789,100,000.000e+00 M 0',
   '+16777216 B',
   '1.2e101 G',
   "{run('df', '/', :out).out.slurp.words[10] * 1024} B 2", # Linux df returns Kilobytes by default
   '347,344 M -2', # round to -2 past the decimal
   '1122334455 Q', # bad unit type example
;

printf "%33s : %s\n", $_, sufficate(|.words) for @tests;
```

```txt
                       87,654,321 : 87.654321M
   -998,877,665,544,332,211,000 3 : -998.878E
                       +112,233 0 : 112K
                     16,777,216 1 : 16.8M
              456,789,100,000,000 : 456.7891T
          456,789,100,000,000 M 2 : 456.79T
          456,789,100,000,000 B 5 : 415.44727Ti
      456,789,100,000.000e+00 M 0 : 457G
                      +16777216 B : 16Mi
                        1.2e101 G : 12googol
                 703674818560 B 2 : 655.35Gi
                     347,344 M -2 : 300K
                     1122334455 Q : What we have here is a failure to communicate...
```



## Phix

The builtin routine file_size_k() handles KB/MB/GB/TB suffixes.

Note that you simply cannot "display all significant digits" when using IEEE-754,
at least with any fraction that is not an exact sum of half, quarter, etc, so for 
that reason this uses bigatom, since that can hold numbers with absolute accuracy.

```Phix
include builtins/bigatom.e

constant suffixes = "KMGTPEZYXWVU",
         anydp = 100 -- the "any dp" value (prevents nearest 1e-100, though)

function suffize(sequence args)
    bigatom size = ba_abs(args[1])
    integer sgn = iff(ba_compare(args[1],0)<0?-1:+1),
            la = length(args),
            dp = iff(la>=2?args[2]:anydp),  -- decimal places
            bd = iff(la>=3?args[3]:10)      -- 2 or 10
    atom ip = power(10,dp) -- (inverted precision)
    if dp<0 then size = ba_round(size,ip) end if
    string suffix
    if ba_compare(size,1e100)>0 then
        size = ba_div(size,1e100)
        suffix = "googol"
    else
        integer factor = iff(bd=2?1024:1000), fdx = 0
        while fdx<length(suffixes) do
            bigatom rsize = ba_div(size,factor)
            if dp<0 then rsize = ba_round(rsize,ip) end if
            if ba_compare(rsize,1)<0 then exit end if
            size = rsize
            fdx += 1
        end while
        suffix = iff(fdx=0?"":suffixes[fdx]&iff(bd=2?"i":""))
    end if
    string fmt = iff(dp<0 or dp=anydp?"%0B":sprintf("%%0.%dB",dp))
    string res = ba_sprintf(fmt, ba_mul(size,sgn))
    res &= suffix
    return res
end function

constant test_text = """
 87,654,321
-998,877,665,544,332,211,000 3
+112,233 0
 16,777,216 1
456,789,100,000,000
456,789,100,000,000 2 10
456,789,100,000,000 5 2
456,789,100,000.000e+00 0 10
 16777216 , 2
1.2e101
347,344 -2
10
""",
        test_cases = split(test_text,'\n',no_empty:=true)

for i=1 to length(test_cases) do
    string ti = test_cases[i]
    sequence args = split(ti,no_empty:=true)
    args[1] = ba_new(substitute(args[1],",",""))
    for a=2 to length(args) do
        sequence sa = scanf(args[a],"%f")
        args[a] = iff(length(sa)=1?sa[1][1]:anydp)
    end for
    string res = suffize(args)
    printf(1,"%30s : %s\n",{ti,res})
end for
```

```txt

                    87,654,321 :  87.654321M
-998,877,665,544,332,211,000 3 : -998.878E
                    +112,233 0 :  112K
                  16,777,216 1 :  16.8M
           456,789,100,000,000 :  456.7891T
      456,789,100,000,000 2 10 :  456.79T
       456,789,100,000,000 5 2 :  415.44727Ti
  456,789,100,000.000e+00 0 10 :  457G
                  16777216 , 2 :  16Mi
                       1.2e101 :  12googol
                    347,344 -2 :  300K
                            10 :  10

```



## REXX


```rexx
/*REXX program to add a  (either metric or "binary" metric)  suffix to a decimal number.*/
@.=                                              /*default value for the stemmed array. */
parse arg @.1                                    /*obtain optional arguments from the CL*/
if @.1==''  then do;   @.1=   '   87,654,321                              '
                       @.2=   '  -998,877,665,544,332,211,000    3        '
                       @.3=   '  +112,233                        0        '
                       @.4=   '   16,777,216                     1        '

                       @.5=   '   456,789,100,000,000            2        '
                       @.5=   '   456,789,100,000,000                     '

                       @.6=   '   456,789,100,000,000            2    10  '
                       @.7=   '   456,789,100,000,000            5     2  '
                       @.8=   '   456,789,100,000.000e+00        0    10  '
                       @.9=   '   +16777216                      ,     2  '
                       @.10=  '   1.2e101                                 '
                       @.11=  '   134,112,411,648                1        '    /*via DIR*/
                 end                             /*@.11≡  amount of free space on my C: */

     do i=1  while @.i\==''; say copies("─", 60) /*display a separator betweenst values.*/
     parse var  @.i  x  f  r  .                  /*get optional arguments from the list.*/
     say '     input number='          x         /*show original number     to the term.*/
     say '    fraction digs='             f      /*  "  specified fracDigs   "  "    "  */
     say '  specified radix='                r   /*  "  specified radix      "  "    "  */
     say '       new number='  suffize(x, f, r)  /*maybe append an "alphabetic" suffix. */
     end   /*i*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
suffize: procedure; arg s 2 1 n,  f,  b          /*obtain:  sign, N, fractionDigs, base.*/
         if digits()<99  then numeric digits 500 /*use enough dec. digs for arithmetic. */
         @err = '***error*** (from SUFFIZE)  '   /*literal used when returning err msg. */
         if b==''  then b= 10;              o= b /*assume a base  (ten)  if omitted.    */
         n= space( translate(n,,','), 0);   m= n /*elide commas from the  1st  argument.*/
         f= space( translate(f,,','), 0)         /*elide commas from the  2nd  argument.*/
         if \datatype(n, 'N')  then return @err "1st argument isn't numeric."
         if f==''  then f= length(space(translate(n,,.), 0)) /*F omitted?  Use full len.*/
         if \datatype(f, 'W')  then return @err "2nd argument isn't an integer: "     f
         if f<0                then return @err "2nd argument can't be negative. "    f
         if \datatype(b, 'W')  then return @err "3rd argument isn't an integer. "     b
         if b\==10  &  b\==2   then return @err "3rd argument isn't a  10  or  2."    b
         if arg()>3            then return @err "too many arguments were specified."
         @=  ' KMGTPEZYXWVU'                     /*metric uppercase suffixes, with blank*/
         !.=;    !.2= 'i'                        /*set default suffix;  "binary" suffix.*/
         i= 3;   b= abs(b);  if b==2  then i= 10 /*a power of ten; or a power of  2**10 */
         if \datatype(n, 'N') | pos('E', n/1)\==0  then return m   /* ¬num or has an "E"*/
         sig=;    if s=='-' | s=="+"  then sig=s /*preserve the number's sign if present*/
         n= abs(n)                               /*possibly round number, & remove sign.*/

           do while n>=1e100 & b==10;  x=n/1e100 /*is N ≥ googol and base=10?  A googol?*/
           if pos(., x)\==0 & o<0  then leave    /*does # have a dec. point  or is B<0? */
           return sig  ||  x'googol'             /*maybe prepend the sign,  add GOOGOL. */
           end   /*while*/

           do j=length(@)-1  to 1  by -1  while n>0  /*see if #  is a multiple of 1024. */
           $= b ** (i*j)                             /*compute base raised to a power.  */
           if n<$  then iterate                      /*N not big enough?   Keep trying. */
           n= format(n/$, , min( digits(), f) ) / 1  /*reformat number with a fraction. */
           if pos(., n)\==0 & o<0  then return m     /*has a decimal point  or  is B<0? */
           leave                                     /*leave this DO loop at this point.*/
           end   /*j*/

         if n=0  then j=0                            /*N = 0?    Don't use any suffix.  */
         return sig||strip(n||substr(@, j+1,1))!.b   /*add sign, suffixes, strip blanks.*/
```

(Shown at three-quarter size.)

<pre style="font-size:75%">
────────────────────────────────────────────────────────────
     input number= 87,654,321
    fraction digs=
  specified radix=
       new number= 87.654321M
────────────────────────────────────────────────────────────
     input number= -998,877,665,544,332,211,000
    fraction digs= 3
  specified radix=
       new number= -998.878E
────────────────────────────────────────────────────────────
     input number= +112,233
    fraction digs= 0
  specified radix=
       new number= +112K
────────────────────────────────────────────────────────────
     input number= 16,777,216
    fraction digs= 1
  specified radix=
       new number= 16.8M
────────────────────────────────────────────────────────────
     input number= 456,789,100,000,000
    fraction digs=
  specified radix=
       new number= 456.7891T
────────────────────────────────────────────────────────────
     input number= 456,789,100,000,000
    fraction digs= 2
  specified radix= 10
       new number= 456.79T
────────────────────────────────────────────────────────────
     input number= 456,789,100,000,000
    fraction digs= 5
  specified radix= 2
       new number= 415.44727Ti
────────────────────────────────────────────────────────────
     input number= 456,789,100,000.000e+00
    fraction digs= 0
  specified radix= 10
       new number= 457G
────────────────────────────────────────────────────────────
     input number= +16777216
    fraction digs= ,
  specified radix= 2
       new number= +16Mi
────────────────────────────────────────────────────────────
     input number= 1.2e101
    fraction digs=
  specified radix=
       new number= 12googol
────────────────────────────────────────────────────────────
     input number= 134,112,411,648
    fraction digs= 1
  specified radix=
       new number= 134.1G

```



## VBA

VBA has support for 64 bit integers on 64 bit platforms. The program below was developed on a 32 bit platform.

```vb
Private Function suffize(number As String, Optional sfractiondigits As String, Optional base As String) As String
    Dim suffix As String, parts() As String, exponent As String
    Dim fractiondigits As Integer, nsuffix As Integer, flag As Boolean
    flag = False
    fractiondigits = Val(sfractiondigits)
    suffixes = " KMGTPEZYXWVU"
    number = Replace(number, ",", "", 1)
    Dim c As Currency
    Dim sign As Integer
    'separate leading sign
    If Left(number, 1) = "-" Then
        number = Right(number, Len(number) - 1)
        outstring = "-"
    End If
    If Left(number, 1) = "+" Then
        number = Right(number, Len(number) - 1)
        outstring = "+"
    End If
    'split exponent
    parts = Split(number, "e")
    number = parts(0)
    If UBound(parts) > 0 Then exponent = parts(1)
    'split fraction
    parts = Split(number, ".")
    number = parts(0)
    If UBound(parts) > 0 Then frac = parts(1)
    If base = "2" Then
        Dim cnumber As Currency
        cnumber = Val(number)
        nsuffix = 0
        Dim dnumber As Double
        If cnumber > 1023 Then
            cnumber = cnumber / 1024@
            nsuffix = nsuffix + 1
            dnumber = cnumber
            Do While dnumber > 1023
                dnumber = dnumber / 1024@ 'caveat: currency has only 4 fractional digits ...
                nsuffix = nsuffix + 1
            Loop
            number = CStr(dnumber)
        Else
            number = CStr(cnumber)
        End If
        leadingstring = Int(number)
        number = Replace(number, ",", "")
        
        leading = Len(leadingstring)
        suffix = Mid(suffixes, nsuffix + 1, 1)
    Else
        'which suffix
        nsuffix = (Len(number) + Val(exponent) - 1) \ 3
        If nsuffix < 13 Then
            suffix = Mid(suffixes, nsuffix + 1, 1)
            leading = (Len(number) - 1) Mod 3 + 1
            leadingstring = Left(number, leading)
        Else
            flag = True
            If nsuffix > 32 Then
                suffix = "googol"
                leading = Len(number) + Val(exponent) - 99
                leadingstring = number & frac & String$(Val(exponent) - 100 - Len(frac), "0")
            Else
                suffix = "U"
                leading = Len(number) + Val(exponent) - 35
                If Val(exponent) > 36 Then
                    leadingstring = number & String$(Val(exponent) - 36, "0")
                Else
                    leadingstring = Left(number, (Len(number) - 36 + Val(exponent)))
                End If
            End If
        End If
    End If
    'round up if necessary
    If fractiondigits > 0 Then
        If Val(Mid(number, leading + fractiondigits + 1, 1)) >= 5 Then
            fraction = Mid(number, leading + 1, fractiondigits - 1) & _
                CStr(Val(Mid(number, leading + fractiondigits, 1)) + 1)
        Else
            fraction = Mid(number, leading + 1, fractiondigits)
        End If
    Else
        If Val(Mid(number, leading + 1, 1)) >= 5 And sfractiondigits <> "" And sfractiondigits <> "," Then
            leadingstring = Mid(number, 1, leading - 1) & _
                CStr(Val(Mid(number, leading, 1)) + 1)
        End If
    End If
    If flag Then
        If sfractiondigits = "" Or sfractiondigits = "," Then
            fraction = ""
        End If
    Else
        If sfractiondigits = "" Or sfractiondigits = "," Then
            fraction = Right(number, Len(number) - leading)
        End If
    End If
    outstring = outstring & leadingstring
    If Len(fraction) > 0 Then
        outstring = outstring & "." & fraction
    End If
    If base = "2" Then
        outstring = outstring & suffix & "i"
    Else
        outstring = outstring & suffix
    End If
    suffize = outstring
End Function
Sub program()
    Dim s(10) As String, t As String, f As String, r As String
    Dim tt() As String, temp As String
    s(0) = "               87,654,321"
    s(1) = "          -998,877,665,544,332,211,000      3"
    s(2) = "          +112,233                          0"
    s(3) = "           16,777,216                       1"
    s(4) = "           456,789,100,000,000              2"
    s(5) = "           456,789,100,000,000              2      10"
    s(6) = "           456,789,100,000,000              5       2"
    s(7) = "           456,789,100,000.000e+00          0      10"
    s(8) = "          +16777216                         ,       2"
    s(9) = "           1.2e101"
    For i = 0 To 9
        ReDim tt(0)
        t = Trim(s(i))
        Do
            temp = t
            t = Replace(t, "  ", " ")
        Loop Until temp = t
        tt = Split(t, " ")
        If UBound(tt) > 0 Then f = tt(1) Else f = ""
        If UBound(tt) > 1 Then r = tt(2) Else r = ""
        Debug.Print String$(48, "-")
        Debug.Print "     input number = "; tt(0)
        Debug.Print "    fraction digs = "; f
        Debug.Print "  specified radix = "; r
        Debug.Print "       new number = "; suffize(tt(0), f, r)
    Next i
End Sub
```

```txt
------------------------------------------------
     input number = 87,654,321
    fraction digs = 
  specified radix = 
       new number = 87.654321M
------------------------------------------------
     input number = -998,877,665,544,332,211,000
    fraction digs = 3
  specified radix = 
       new number = -998.878E
------------------------------------------------
     input number = +112,233
    fraction digs = 0
  specified radix = 
       new number = +112K
------------------------------------------------
     input number = 16,777,216
    fraction digs = 1
  specified radix = 
       new number = 16.8M
------------------------------------------------
     input number = 456,789,100,000,000
    fraction digs = 2
  specified radix = 
       new number = 456.79T
------------------------------------------------
     input number = 456,789,100,000,000
    fraction digs = 2
  specified radix = 10
       new number = 456.79T
------------------------------------------------
     input number = 456,789,100,000,000
    fraction digs = 5
  specified radix = 2
       new number = 415.44727Ti
------------------------------------------------
     input number = 456,789,100,000.000e+00
    fraction digs = 0
  specified radix = 10
       new number = 457G
------------------------------------------------
     input number = +16777216
    fraction digs = ,
  specified radix = 2
       new number = +16Mi
------------------------------------------------
     input number = 1.2e101
    fraction digs = 
  specified radix = 
       new number = 12googol
```


## zkl

{{libheader|GMP}} GNU Multiple Precision Arithmetic Library (big ints)
Error checking is nonexistent.

```zkl
var [const] BI=Import.lib("zklBigNum");  // GMP
var metric, binary, googol=BI("1e100");
metric,binary = metricBin();

   // suffix: "2" (binary), "10" (metric)
   // For this task, we'll assume BF numbers and treat everything as a big int
fcn sufficate(numStr, fracDigits=",", suffix="10"){
   var [const] numRE=RegExp(0'^\+*(([+-]*\.*\d+)[.]*(\d*)(e*[+-]*\d*))^);

   numRE.search((numStr - " ,").toLower());
   r:=numRE.matched[1];
   if(not r.find(".")) r=BI(r); // else ((((0,7),"1.2e101","1","2","e101")
   else  // convert float ("1.2" or "1.2e10") to big int
      r=BI(numRE.matched[2,*].concat())/(10).pow(numRE.matched[3].len());

   if(fracDigits==",") fracDigits=0; # "digits past decimal or none, if not specified"
   else                fracDigits=fracDigits.toInt();

   suffix=suffix.strip().toInt();
   if(suffix==2) nms,szs :=binary;
   else if(suffix==10)   nms,szs :=metric;
   else //throw(Exception.ValueError("Invalid suffix: %s".fmt(suffix)));
     return("Invalid suffix");

   ar:=r.abs();
   if(ar<szs[0]) return(r.toString());	// little bitty number
   i,sz,nm := szs.filter1n('>(ar)) - 1, szs[i], nms[i];  // False - 1 == True
   if(i==True)  // r > biggest unit
      if(r>=googol) sz,nm = googol, "googol";   // get out the big hammer
      else          sz,nm = szs[-1], nms[-1];   //    even if they want n^2
   fd,m := fracDigits + 4, BI(10).pow(fd);      // int --> float w/extra digits
   a,f,a := r*m/sz, (a%m).toFloat()/m, f + a/m; // to float for rounding
   fmt:="%%,.%df%%s".fmt(fracDigits).fmt;	// eg "%,.5f%s"
   return(fmt(a,nm));
}

   //-->Metric:(("K", "M",..), (1000,1000000,..))
   //   Bin:   (("Ki","Mi",..),(1024,1048576,..))
fcn metricBin{
   ss,m,b := "K M G T P E Z Y X W V U".split(), List(),List();
   ss.zipWith(m.append,[3..3*(ss.len()),3].apply(BI(10).pow)); // Metric
   ss.apply("append","i")
      .zipWith(b.append,[10..10*(ss.len()),10].apply(BI(2).pow)); // Binary
   return(m.filter22("".isType), b.filter22("".isType)); # split to ((strings),(nums))
}
```


```zkl
testCases:=T(
   "87,654,321",
   "-998,877,665,544,332,211,000      3",
   "+112,233                          0",
   "16,777,216                        1",
   "456,789,100,000,000",
   "456,789,100,000,000               2      10",
   "456,789,100,000,000               5       2",
   "456,789,100,000.000e+00           0      10",
   "+16777216                         ,       2",
   "1.2e101",
   "446,835,273,728                   1",
   "1e36",
   "1e39", // there isn't a big enough suffix for this one but it's less than googol
   		# Linux df returns Kilobytes by default
   (1024*System.popen("df /","r").read().text.split()[10]).toString() + " 1 2 \"df /\"",
   "1122334455 , 666", # bad unit type example
   "10",  // don't suffix this
);
foreach test in (testCases){
   test=test.split();
   "%33s : %s".fmt(test.concat(" "),sufficate(test.xplode())).println();
}
```

```txt

                       87,654,321 : 88M
   -998,877,665,544,332,211,000 3 : -997.878E
                       +112,233 0 : 112K
                     16,777,216 1 : 16.8M
              456,789,100,000,000 : 457T
         456,789,100,000,000 2 10 : 456.79T
          456,789,100,000,000 5 2 : 415.44727Ti
     456,789,100,000.000e+00 0 10 : 457G
                    +16777216 , 2 : 16Mi
                          1.2e101 : 12googol
                446,835,273,728 1 : 446.8G
                             1e36 : 1U
                             1e39 : 1,000U
           17221619712 1 2 "df /" : 16.0Gi
                 1122334455 , 666 : Invalid suffix
                               10 : 10

```



## Python

Tested in Python 3.7.3<br />
Chose 3 places after decimal (where applicable) as default rounding precision. Number to suffize taken as a string. There are some edge cases where this fails due to binary arithmetic differing from decimal arithmetic and things not rounding nicely.

```python

import math
import os


def suffize(num, digits=None, base=10):
    suffixes = ['', 'K', 'M', 'G', 'T', 'P', 'E', 'Z', 'Y', 'X', 'W', 'V', 'U', 'googol']

    exponent_distance = 10 if base == 2 else 3
    num = num.strip().replace(',', '')
    num_sign = num[0] if num[0] in '+-' else ''

    num = abs(float(num))

    if base == 10 and num >= 1e100:
        suffix_index = 13
        num /= 1e100
    elif num > 1:
        magnitude = math.floor(math.log(num, base))
        suffix_index = min(math.floor(magnitude / exponent_distance), 12)
        num /= base ** (exponent_distance * suffix_index)
    else:
        suffix_index = 0

    if digits is not None:
        num_str = f'{num:.{digits}f}'
    else:
        num_str = f'{num:.3f}'.strip('0').strip('.')

    return num_sign + num_str + suffixes[suffix_index] + ('i' if base == 2 else '')


tests = [('87,654,321',),
         ('-998,877,665,544,332,211,000', 3),
         ('+112,233', 0),
         ('16,777,216', 1),
         ('456,789,100,000,000', 2),
         ('456,789,100,000,000', 2, 10),
         ('456,789,100,000,000', 5, 2),
         ('456,789,100,000.000e+00', 0, 10),
         ('+16777216', None, 2),
         ('1.2e101',)]

for test in tests:
    print(' '.join(str(i) for i in test) + ' : ' + suffize(*test))

```


```txt

87,654,321 : 87.654M
-998,877,665,544,332,211,000 3 : -998.878E
+112,233 0 : +112K
16,777,216 1 : 16.8M
456,789,100,000,000 2 : 456.79T
456,789,100,000,000 2 10 : 456.79T
456,789,100,000,000 5 2 : 415.44727Ti
456,789,100,000.000e+00 0 10 : 457G
+16777216 None 2 : +16Mi
1.2e101 : 12googol

```

