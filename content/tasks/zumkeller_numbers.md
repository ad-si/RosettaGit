+++
title = "Zumkeller numbers"
description = ""
date = 2019-09-04T15:51:28Z
aliases = []
[extra]
id = 22469
[taxonomies]
categories = ["Mathematics", "Number theory", "Divisors", "task"]
tags = []
languages = [
  "go",
  "julia",
  "perl",
  "perl_6",
  "phix",
  "picolisp",
  "python",
  "racket",
  "rexx",
  "sidef",
  "zkl",
]
+++

## Task

Zumkeller numbers are the set of numbers whose divisors can be partitioned into two disjoint sets that sum to the same value. Each sum must contain divisor values that are not in the other sum, and all of the divisors must be in one or the other. There are no restrictions on ''how'' the divisors are partitioned, only that the two partition sums are equal.


;E.G.

: '''6''' is a Zumkeller number; The divisors '''{1 2 3 6}''' can be partitioned into two groups '''{1 2 3}''' and '''{6}''' that both sum to 6.

: '''10''' is not a Zumkeller number; The divisors '''{1 2 5 10}''' can not be partitioned into two groups in any way that will both sum to the same value.

: '''12''' is a Zumkeller number; The divisors '''{1 2 3 4 6 12}''' can be partitioned into two groups '''{1 3 4 6}''' and '''{2 12}''' that both sum to 14.


Even Zumkeller numbers are common; odd Zumkeller numbers are much less so. For values below 10^6, there is ''at least'' one Zumkeller number in every 12 consecutive integers, and the vast majority of them are even. The odd Zumkeller numbers are very similar to the list from the task [[Abundant odd numbers]]; they are nearly the same except for the further restriction that the abundance ('''A(n) = sigma(n) - 2n'''), must be even: '''A(n) mod 2 == 0'''


## Task

:* Write a routine (function, procedure, whatever) to find Zumkeller numbers.

:* Use the routine to find and display here, on this page, the first '''220 Zumkeller numbers'''.

:* Use the routine to find and display here, on this page, the first '''40 odd Zumkeller numbers'''.

:* Optional, stretch goal: Use the routine to find and display here, on this page, the first '''40 odd Zumkeller numbers that don't end with 5'''.


;See Also:

:* '''[[oeis:A083207|OEIS:A083207 - Zumkeller numbers]]'''
:* '''[[oeis:A174865|OEIS:A174865 - Odd Zumkeller numbers]]'''


;Related Tasks:

:* '''[[Abundant odd numbers]]'''
:* '''[[Abundant, deficient and perfect number classifications]]'''
:* '''[[Proper divisors]]'''



## Go


```go
package main

import "fmt"

func getDivisors(n int) []int {
    divs := []int{1, n}
    for i := 2; i*i <= n; i++ {
        if n%i == 0 {
            j := n / i
            divs = append(divs, i)
            if i != j {
                divs = append(divs, j)
            }
        }
    }
    return divs
}

func sum(divs []int) int {
    sum := 0
    for _, div := range divs {
        sum += div
    }
    return sum
}

func isPartSum(divs []int, sum int) bool {
    if sum == 0 {
        return true
    }
    le := len(divs)
    if le == 0 {
        return false
    }
    last := divs[le-1]
    divs = divs[0 : le-1]
    if last > sum {
        return isPartSum(divs, sum)
    }
    return isPartSum(divs, sum) || isPartSum(divs, sum-last)
}

func isZumkeller(n int) bool {
    divs := getDivisors(n)
    sum := sum(divs)
    // if sum is odd can't be split into two partitions with equal sums
    if sum%2 == 1 {
        return false
    }
    // if n is odd use 'abundant odd number' optimization
    if n%2 == 1 {
        abundance := sum - 2*n
        return abundance > 0 && abundance%2 == 0
    }
    // if n and sum are both even check if there's a partition which totals sum / 2
    return isPartSum(divs, sum/2)
}

func main() {
    fmt.Println("The first 220 Zumkeller numbers are:")
    for i, count := 2, 0; count < 220; i++ {
        if isZumkeller(i) {
            fmt.Printf("%3d ", i)
            count++
            if count%20 == 0 {
                fmt.Println()
            }
        }
    }
    fmt.Println("\nThe first 40 odd Zumkeller numbers are:")
    for i, count := 3, 0; count < 40; i += 2 {
        if isZumkeller(i) {
            fmt.Printf("%5d ", i)
            count++
            if count%10 == 0 {
                fmt.Println()
            }
        }
    }
    fmt.Println("\nThe first 40 odd Zumkeller numbers which don't end in 5 are:")
    for i, count := 3, 0; count < 40; i += 2 {
        if (i % 10 != 5) && isZumkeller(i) {
            fmt.Printf("%7d ", i)
            count++
            if count%8 == 0 {
                fmt.Println()
            }
        }
    }
    fmt.Println()
}
```


```txt

The first 220 Zumkeller numbers are:
  6  12  20  24  28  30  40  42  48  54  56  60  66  70  78  80  84  88  90  96 
102 104 108 112 114 120 126 132 138 140 150 156 160 168 174 176 180 186 192 198 
204 208 210 216 220 222 224 228 234 240 246 252 258 260 264 270 272 276 280 282 
294 300 304 306 308 312 318 320 330 336 340 342 348 350 352 354 360 364 366 368 
372 378 380 384 390 396 402 408 414 416 420 426 432 438 440 444 448 456 460 462 
464 468 474 476 480 486 490 492 496 498 500 504 510 516 520 522 528 532 534 540 
544 546 550 552 558 560 564 570 572 580 582 588 594 600 606 608 612 616 618 620 
624 630 636 640 642 644 650 654 660 666 672 678 680 684 690 696 700 702 704 708 
714 720 726 728 732 736 740 744 750 756 760 762 768 770 780 786 792 798 804 810 
812 816 820 822 828 832 834 836 840 852 858 860 864 868 870 876 880 888 894 896 
906 910 912 918 920 924 928 930 936 940 942 945 948 952 960 966 972 978 980 984 

The first 40 odd Zumkeller numbers are:
  945  1575  2205  2835  3465  4095  4725  5355  5775  5985 
 6435  6615  6825  7245  7425  7875  8085  8415  8505  8925 
 9135  9555  9765 10395 11655 12285 12705 12915 13545 14175 
14805 15015 15435 16065 16695 17325 17955 18585 19215 19305

The first 40 odd Zumkeller numbers which don't end in 5 are:
  81081  153153  171171  189189  207207  223839  243243  261261 
 279279  297297  351351  459459  513513  567567  621621  671517 
 729729  742203  783783  793611  812889  837837  891891  908523 
 960687  999999 1024947 1054053 1072071 1073709 1095633 1108107 
1145529 1162161 1198197 1224531 1270269 1307691 1324323 1378377  

```



## Julia


```julia
using Primes

function factorize(n)
    f = [one(n)]
    for (p, x) in factor(n)
        f = reduce(vcat, [f*p^i for i in 1:x], init=f)
    end
    f
end

function cansum(goal, list)
    if goal == 0 || list[1] == goal
         return true
    elseif length(list) > 1
        if list[1] > goal
            return cansum(goal, list[2:end])
        else
            return cansum(goal - list[1], list[2:end]) ||  cansum(goal, list[2:end])
        end
    end
    return false
end

function iszumkeller(n)
    f = reverse(factorize(n))
    fsum = sum(f)
    return iseven(fsum) && cansum(div(fsum, 2) - f[1], f[2:end])
end

function printconditionalnum(condition, maxcount, numperline = 20)
    count, spacing = 1, div(80, numperline)
    for i in 1:typemax(Int)
        if condition(i)
            count += 1
            print(rpad(i, spacing), (count - 1) % numperline == 0 ? "\n" : "")
            if count > maxcount
                return
            end
        end
    end
end

println("First 220 Zumkeller numbers:")
printconditionalnum(iszumkeller, 220)
println("\n\nFirst 40 odd Zumkeller numbers:")
printconditionalnum((n) -> isodd(n) && iszumkeller(n), 40, 8)
println("\n\nFirst 40 odd Zumkeller numbers not ending with 5:")
printconditionalnum((n) -> isodd(n) && (string(n)[end] != '5') && iszumkeller(n), 40, 8)

```
```txt

First 220 Zumkeller numbers:
6   12  20  24  28  30  40  42  48  54  56  60  66  70  78  80  84  88  90  96
102 104 108 112 114 120 126 132 138 140 150 156 160 168 174 176 180 186 192 198
204 208 210 216 220 222 224 228 234 240 246 252 258 260 264 270 272 276 280 282
294 300 304 306 308 312 318 320 330 336 340 342 348 350 352 354 360 364 366 368
372 378 380 384 390 396 402 408 414 416 420 426 432 438 440 444 448 456 460 462
464 468 474 476 480 486 490 492 496 498 500 504 510 516 520 522 528 532 534 540
544 546 550 552 558 560 564 570 572 580 582 588 594 600 606 608 612 616 618 620
624 630 636 640 642 644 650 654 660 666 672 678 680 684 690 696 700 702 704 708
714 720 726 728 732 736 740 744 750 756 760 762 768 770 780 786 792 798 804 810
812 816 820 822 828 832 834 836 840 852 858 860 864 868 870 876 880 888 894 896
906 910 912 918 920 924 928 930 936 940 942 945 948 952 960 966 972 978 980 984


First 40 odd Zumkeller numbers:
945       1575      2205      2835      3465      4095      4725      5355
5775      5985      6435      6615      6825      7245      7425      7875
8085      8415      8505      8925      9135      9555      9765      10395
11655     12285     12705     12915     13545     14175     14805     15015
15435     16065     16695     17325     17955     18585     19215     19305


First 40 odd Zumkeller numbers not ending with 5:
81081     153153    171171    189189    207207    223839    243243    261261
279279    297297    351351    459459    513513    567567    621621    671517
729729    742203    783783    793611    812889    837837    891891    908523
960687    999999    1024947   1054053   1072071   1073709   1095633   1108107
1145529   1162161   1198197   1224531   1270269   1307691   1324323   1378377

```



## Perl

```perl
use strict;
use warnings;
use ntheory <is_prime divisor_sum divisors vecsum forcomb lastfor>;

sub in_columns {
    my($columns, $values) = @_;
    my @v = split ' ', $values;
    my $width = int(80/$columns);
    printf "%${width}d"x$columns."\n", @v[$_*$columns .. -1+(1+$_)*$columns] for 0..-1+@v/$columns;
    print "\n";
}

sub is_Zumkeller {
    my($n) = @_;
    return 0 if is_prime($n);
    my @divisors = divisors($n);
    return 0 unless @divisors > 2 && 0 == @divisors % 2;
    my $sigma = divisor_sum($n);
    return 0 unless 0 == $sigma%2 && ($sigma/2) >= $n;
    if (1 == $n%2) {
        return 1
    } else {
        my $Z = 0;
        forcomb { $Z++, lastfor if vecsum(@divisors[@_]) == $sigma/2 } @divisors;
        return $Z;
    }
}

my $Inf = 10e10;

say 'First 220 Zumkeller numbers:';
my $n = 1; my $z;
$z .= do { $n <= 220 ? is_Zumkeller($_) && $n++ && "$_ " : last } for 1..$Inf;
in_columns(20, $z);

say 'First 40 odd Zumkeller numbers:';
$n = 1; $z = '';
$z .= do { $n <= 40 ? 0 != $_%2 && is_Zumkeller($_) && $n++ && "$_ " : last } for 1..$Inf;
in_columns(10, $z);

say 'First 40 odd Zumkeller numbers not divisible by 5:';
$n = 1; $z = '';
$z .= do { $n <= 40 ? 0 != $_%2 && 0 != $_%5 && is_Zumkeller($_) && $n++ && "$_ " : last } for 1..$Inf;
in_columns(10, $z);
```

```txt
First 220 Zumkeller numbers:
   6  12  20  24  28  30  40  42  48  54  56  60  66  70  78  80  84  88  90  96
 102 104 108 112 114 120 126 132 138 140 150 156 160 168 174 176 180 186 192 198
 204 208 210 216 220 222 224 228 234 240 246 252 258 260 264 270 272 276 280 282
 294 300 304 306 308 312 318 320 330 336 340 342 348 350 352 354 360 364 366 368
 372 378 380 384 390 396 402 408 414 416 420 426 432 438 440 444 448 456 460 462
 464 468 474 476 480 486 490 492 496 498 500 504 510 516 520 522 528 532 534 540
 544 546 550 552 558 560 564 570 572 580 582 588 594 600 606 608 612 616 618 620
 624 630 636 640 642 644 650 654 660 666 672 678 680 684 690 696 700 702 704 708
 714 720 726 728 732 736 740 744 750 756 760 762 768 770 780 786 792 798 804 810
 812 816 820 822 828 832 834 836 840 852 858 860 864 868 870 876 880 888 894 896
 906 910 912 918 920 924 928 930 936 940 942 945 948 952 960 966 972 978 980 984

First 40 odd Zumkeller numbers:
     945    1575    2205    2835    3465    4095    4725    5355    5775    5985
    6435    6615    6825    7245    7425    7875    8085    8415    8505    8925
    9135    9555    9765   10395   11655   12285   12705   12915   13545   14175
   14805   15015   15435   16065   16695   17325   17955   18585   19215   19305

First 40 odd Zumkeller numbers not divisible by 5:
   81081  153153  171171  189189  207207  223839  243243  261261  279279  297297
  351351  459459  513513  567567  621621  671517  729729  742203  783783  793611
  812889  837837  891891  908523  960687  999999 1024947 1054053 1072071 1073709
 1095633 1108107 1145529 1162161 1198197 1224531 1270269 1307691 1324323 1378377
```



## Perl 6

```perl6
use ntheory:from<Perl5>
 <factor is_prime>;

sub zumkeller ($range)  {
    $range.grep: -> $maybe {
        next if $maybe < 3;
        next if $maybe.&is_prime;
        my @divisors = $maybe.&factor.combinations».reduce( &[*] ).unique.reverse;
        next unless [&&] +@divisors > 2, +@divisors %% 2, (my $sum = sum @divisors) %% 2, ($sum /= 2) >= $maybe;
        my $zumkeller = False;
        if $maybe % 2 {
            $zumkeller = True
        } else {
            TEST: loop (my $c = 1; $c < @divisors / 2; ++$c) {
                @divisors.combinations($c).map: -> $d {
                    next if (sum $d) != $sum;
                    $zumkeller = True and last TEST;
                }
            }
        }
        $zumkeller
    }
}

say "First 220 Zumkeller numbers:\n" ~
    zumkeller(^Inf)[^220].rotor(20)».fmt('%3d').join: "\n";

put "\nFirst 40 odd Zumkeller numbers:\n" ~
    zumkeller((^Inf).map: * * 2 + 1)[^40].rotor(10)».fmt('%7d').join: "\n";

# Stretch. Slow to calculate. (minutes) 
put "\nFirst 40 odd Zumkeller numbers not divisible by 5:\n" ~
    zumkeller(flat (^Inf).map: {my \p = 10 * $_; p+1, p+3, p+7, p+9} )[^40].rotor(10)».fmt('%7d').join: "\n";
```

```txt
First 220 Zumkeller numbers:
  6  12  20  24  28  30  40  42  48  54  56  60  66  70  78  80  84  88  90  96
102 104 108 112 114 120 126 132 138 140 150 156 160 168 174 176 180 186 192 198
204 208 210 216 220 222 224 228 234 240 246 252 258 260 264 270 272 276 280 282
294 300 304 306 308 312 318 320 330 336 340 342 348 350 352 354 360 364 366 368
372 378 380 384 390 396 402 408 414 416 420 426 432 438 440 444 448 456 460 462
464 468 474 476 480 486 490 492 496 498 500 504 510 516 520 522 528 532 534 540
544 546 550 552 558 560 564 570 572 580 582 588 594 600 606 608 612 616 618 620
624 630 636 640 642 644 650 654 660 666 672 678 680 684 690 696 700 702 704 708
714 720 726 728 732 736 740 744 750 756 760 762 768 770 780 786 792 798 804 810
812 816 820 822 828 832 834 836 840 852 858 860 864 868 870 876 880 888 894 896
906 910 912 918 920 924 928 930 936 940 942 945 948 952 960 966 972 978 980 984

First 40 odd Zumkeller numbers:
    945    1575    2205    2835    3465    4095    4725    5355    5775    5985
   6435    6615    6825    7245    7425    7875    8085    8415    8505    8925
   9135    9555    9765   10395   11655   12285   12705   12915   13545   14175
  14805   15015   15435   16065   16695   17325   17955   18585   19215   19305

First 40 odd Zumkeller numbers not divisible by 5:
  81081  153153  171171  189189  207207  223839  243243  261261  279279  297297
 351351  459459  513513  567567  621621  671517  729729  742203  783783  793611
 812889  837837  891891  908523  960687  999999 1024947 1054053 1072071 1073709
1095633 1108107 1145529 1162161 1198197 1224531 1270269 1307691 1324323 1378377
```



## Phix

```Phix
function isPartSum(sequence f, integer l, t)
    if t=0 then return true end if
    if l=0 then return false end if
    integer last = f[l]
    return (t>=last and isPartSum(f, l-1, t-last))
        or isPartSum(f, l-1, t)
end function
 
function isZumkeller(integer n)
    sequence f = factors(n,1)
    integer t = sum(f)
    -- an odd sum cannot be split into two equal sums
    if remainder(t,2)=1 then return false end if
    -- if n is odd use 'abundant odd number' optimization
    if remainder(n,2)=1 then
        integer abundance := t - 2*n
        return abundance>0 and remainder(abundance,2)=0
    end if
    -- if n and t both even check for any partition of t/2
    return isPartSum(f, length(f), t/2)
end function
 
sequence tests = {{220,1,0,20,"%3d "},
                  {40,2,0,10,"%5d "},
                  {40,2,5,8,"%7d "}}
integer lim, step, rem, cr; string fmt
for t=1 to length(tests) do
    {lim, step, rem, cr, fmt} = tests[t]
    string odd = iff(step=1?"":"odd "),
           wch = iff(rem=0?"":"which don't end in 5 ")
    printf(1,"The first %d %sZumkeller numbers %sare:\n",{lim,odd,wch})
    integer i = step+1, count = 0
    while count<lim do
        if (rem=0 or remainder(i,10)!=rem)
        and isZumkeller(i) then
            printf(1,fmt,i)
            count += 1
            if remainder(count,cr)=0 then puts(1,"\n") end if
        end if
        i += step
    end while
    printf(1,"\n")
end for
```

```txt

The first 220 Zumkeller numbers are:
  6  12  20  24  28  30  40  42  48  54  56  60  66  70  78  80  84  88  90  96
102 104 108 112 114 120 126 132 138 140 150 156 160 168 174 176 180 186 192 198
204 208 210 216 220 222 224 228 234 240 246 252 258 260 264 270 272 276 280 282
294 300 304 306 308 312 318 320 330 336 340 342 348 350 352 354 360 364 366 368
372 378 380 384 390 396 402 408 414 416 420 426 432 438 440 444 448 456 460 462
464 468 474 476 480 486 490 492 496 498 500 504 510 516 520 522 528 532 534 540
544 546 550 552 558 560 564 570 572 580 582 588 594 600 606 608 612 616 618 620
624 630 636 640 642 644 650 654 660 666 672 678 680 684 690 696 700 702 704 708
714 720 726 728 732 736 740 744 750 756 760 762 768 770 780 786 792 798 804 810
812 816 820 822 828 832 834 836 840 852 858 860 864 868 870 876 880 888 894 896
906 910 912 918 920 924 928 930 936 940 942 945 948 952 960 966 972 978 980 984

The first 40 odd Zumkeller numbers are:
  945  1575  2205  2835  3465  4095  4725  5355  5775  5985
 6435  6615  6825  7245  7425  7875  8085  8415  8505  8925
 9135  9555  9765 10395 11655 12285 12705 12915 13545 14175
14805 15015 15435 16065 16695 17325 17955 18585 19215 19305

The first 40 odd Zumkeller numbers which don't end in 5 are:
  81081  153153  171171  189189  207207  223839  243243  261261
 279279  297297  351351  459459  513513  567567  621621  671517
 729729  742203  783783  793611  812889  837837  891891  908523
 960687  999999 1024947 1054053 1072071 1073709 1095633 1108107
1145529 1162161 1198197 1224531 1270269 1307691 1324323 1378377

```

Aside: not that it really matters here, but passing an explicit length to isPartSum (ie, l) is generally quite a bit
faster than trimming (and therefore cloning) the contents of f, just so that we can rely on length(f), and obviously 
that would get more significant were f much longer, though it does in fact max out at a mere 80 here.

In contrast, reversing the "or" tests on the final return of isPartSum() has a significant detrimental effect, since
it triggers a full recursive search for almost all l=0 failures before ever letting a single t=0 succeed. Quite why
I don't get anything like the same slowdown when I modify the Go code is beyond me...


## PicoLisp


```PicoLisp
(de propdiv (N)
   (make
      (for I N
         (and (=0 (% N I)) (link I)) ) ) )
(de sum? (G L)
   (cond
      ((=0 G) T)
      ((= (car L) G) T)
      ((cdr L)
         (if (> (car L) G)
            (sum? G (cdr L))
            (or
               (sum? (- G (car L)) (cdr L))
               (sum? G (cdr L)) ) ) ) ) )
(de zum? (N)
   (let (L (propdiv N)  S (sum prog L))
      (and
         (not (bit? 1 S))
         (if (bit? 1 N)
            (let A (- S (* 2 N))
               (and (gt0 A) (not (bit? 1 A)))
            )
            (sum?
               (- (/ S 2) (car L))
               (cdr L) ) ) ) ) )
(zero C)
(for (I 2 (> 220 C) (inc I))
   (when (zum? I)
      (prin (align 3 I) " ")
      (inc 'C)
      (and
         (=0 (% C 20))
         (prinl) ) ) )
(prinl)
(zero C)
(for (I 1 (> 40 C) (inc 'I 2))
   (when (zum? I)
      (prin (align 9 I) " ")
      (inc 'C)
      (and
         (=0 (% C 8))
         (prinl) ) ) )
(prinl)
(zero C)
# cheater
(for (I 81079 (> 40 C) (inc 'I 2))
   (when (and (<> 5 (% I 10)) (zum? I))
      (prin (align 9 I) " ")
      (inc 'C)
      (and
         (=0 (% C 8))
         (prinl) ) ) )
```

```txt

  6  12  20  24  28  30  40  42  48  54  56  60  66  70  78  80  84  88  90  96
102 104 108 112 114 120 126 132 138 140 150 156 160 168 174 176 180 186 192 198
204 208 210 216 220 222 224 228 234 240 246 252 258 260 264 270 272 276 280 282
294 300 304 306 308 312 318 320 330 336 340 342 348 350 352 354 360 364 366 368
372 378 380 384 390 396 402 408 414 416 420 426 432 438 440 444 448 456 460 462
464 468 474 476 480 486 490 492 496 498 500 504 510 516 520 522 528 532 534 540
544 546 550 552 558 560 564 570 572 580 582 588 594 600 606 608 612 616 618 620
624 630 636 640 642 644 650 654 660 666 672 678 680 684 690 696 700 702 704 708
714 720 726 728 732 736 740 744 750 756 760 762 768 770 780 786 792 798 804 810
812 816 820 822 828 832 834 836 840 852 858 860 864 868 870 876 880 888 894 896
906 910 912 918 920 924 928 930 936 940 942 945 948 952 960 966 972 978 980 984

      945      1575      2205      2835      3465      4095      4725      5355
     5775      5985      6435      6615      6825      7245      7425      7875
     8085      8415      8505      8925      9135      9555      9765     10395
    11655     12285     12705     12915     13545     14175     14805     15015
    15435     16065     16695     17325     17955     18585     19215     19305

    81081    153153    171171    189189    207207    223839    243243    261261
   279279    297297    351351    459459    513513    567567    621621    671517
   729729    742203    783783    793611    812889    837837    891891    908523
   960687    999999   1024947   1054053   1072071   1073709   1095633   1108107
  1145529   1162161   1198197   1224531   1270269   1307691   1324323   1378377

```



## Python

Modified from a footnote at OEIS A083207 (see reference in problem text) by Charles R Greathouse IV.

```python
from sympy import divisors

from sympy.combinatorics.subsets import Subset

def isZumkeller(n):
    d = divisors(n)
    s = sum(d)
    if not s % 2 and max(d) <= s/2:
        for x in range(1, 2**len(d)):
            if sum(Subset.unrank_binary(x, d).subset) == s/2:
                return True

    return False



def printZumkellers(N, oddonly=False):
    nprinted = 0
    for n in range(1, 10**5):
        if (oddonly == False or n % 2) and isZumkeller(n):
            print(f'{n:>8}', end='')
            nprinted += 1
            if nprinted % 10 == 0:
                print()
            if nprinted >= N:
                return


print("220 Zumkeller numbers:")
printZumkellers(220)
print("\n\n40 odd Zumkeller numbers:")
printZumkellers(40, True)

```
```txt

220 Zumkeller numbers:
       6      12      20      24      28      30      40      42      48      54
      56      60      66      70      78      80      84      88      90      96
     102     104     108     112     114     120     126     132     138     140
     150     156     160     168     174     176     180     186     192     198
     204     208     210     216     220     222     224     228     234     240
     246     252     258     260     264     270     272     276     280     282
     294     300     304     306     308     312     318     320     330     336
     340     342     348     350     352     354     360     364     366     368
     372     378     380     384     390     396     402     408     414     416
     420     426     432     438     440     444     448     456     460     462
     464     468     474     476     480     486     490     492     496     498
     500     504     510     516     520     522     528     532     534     540
     544     546     550     552     558     560     564     570     572     580
     582     588     594     600     606     608     612     616     618     620
     624     630     636     640     642     644     650     654     660     666
     672     678     680     684     690     696     700     702     704     708
     714     720     726     728     732     736     740     744     750     756
     760     762     768     770     780     786     792     798     804     810
     812     816     820     822     828     832     834     836     840     852
     858     860     864     868     870     876     880     888     894     896
     906     910     912     918     920     924     928     930     936     940
     942     945     948     952     960     966     972     978     980     984


40 odd Zumkeller numbers:
     945    1575    2205    2835    3465    4095    4725    5355    5775    5985
    6435    6615    6825    7245    7425    7875    8085    8415    8505    8925
    9135    9555    9765   10395   11655   12285   12705   12915   13545   14175
   14805   15015   15435   16065   16695   17325   17955   18585   19215   19305

```



## Racket

```racket
#lang racket

(require math/number-theory)

(define (zum? n)
  (let* ((set (divisors n))
         (sum (apply + set)))
    (cond
      [(odd? sum) #f]
      [(odd? n) ; if n is odd use 'abundant odd number' optimization
       (let ((abundance (- sum (* n 2)))) (and (positive? abundance) (even? abundance)))]
      [else
       (let ((sum/2 (quotient sum 2)))
         (let loop ((acc (car set)) (set (cdr set)))
           (cond [(= acc sum/2) #t]
                 [(> acc sum/2) #f]
                 [(null? set) #f]
                 [else (or (loop (+ (car set) acc) (cdr set))
                           (loop acc (cdr set)))])))])))

(define (first-n-matching-naturals count pred)
  (for/list ((i count) (j (stream-filter pred (in-naturals 1)))) j))

(define (tabulate title ns (row-width 132))
  (displayln title)
  (let* ((cell-width (+ 2 (order-of-magnitude (apply max ns))))
         (cells/row (quotient row-width cell-width)))
    (let loop ((ns ns) (col cells/row))
      (cond [(null? ns) (unless (= col cells/row) (newline))]
            [(zero? col) (newline) (loop ns cells/row)]
            [else (display (~a #:width cell-width #:align 'right (car ns)))
                  (loop (cdr ns) (sub1 col))]))))


(tabulate  "First 220 Zumkeller numbers:" (first-n-matching-naturals 220 zum?))
(newline)
(tabulate "First 40 odd Zumkeller numbers:"
          (first-n-matching-naturals 40 (λ (n) (and (odd? n) (zum? n)))))
(newline)
(tabulate "First 40 odd Zumkeller numbers not ending in 5:"
          (first-n-matching-naturals 40 (λ (n) (and (odd? n) (not (= 5 (modulo n 10))) (zum? n)))))
```


```txt
First 220 Zumkeller numbers:
   6  12  20  24  28  30  40  42  48  54  56  60  66  70  78  80  84  88  90  96 102 104 108 112 114 120 126 132 138 140 150 156 160
 168 174 176 180 186 192 198 204 208 210 216 220 222 224 228 234 240 246 252 258 260 264 270 272 276 280 282 294 300 304 306 308 312
 318 320 330 336 340 342 348 350 352 354 360 364 366 368 372 378 380 384 390 396 402 408 414 416 420 426 432 438 440 444 448 456 460
 462 464 468 474 476 480 486 490 492 496 498 500 504 510 516 520 522 528 532 534 540 544 546 550 552 558 560 564 570 572 580 582 588
 594 600 606 608 612 616 618 620 624 630 636 640 642 644 650 654 660 666 672 678 680 684 690 696 700 702 704 708 714 720 726 728 732
 736 740 744 750 756 760 762 768 770 780 786 792 798 804 810 812 816 820 822 828 832 834 836 840 852 858 860 864 868 870 876 880 888
 894 896 906 910 912 918 920 924 928 930 936 940 942 945 948 952 960 966 972 978 980 984

First 40 odd Zumkeller numbers:
   945  1575  2205  2835  3465  4095  4725  5355  5775  5985  6435  6615  6825  7245  7425  7875  8085  8415  8505  8925  9135  9555
  9765 10395 11655 12285 12705 12915 13545 14175 14805 15015 15435 16065 16695 17325 17955 18585 19215 19305

First 40 odd Zumkeller numbers not ending in 5:
   81081  153153  171171  189189  207207  223839  243243  261261  279279  297297  351351  459459  513513  567567  621621  671517
  729729  742203  783783  793611  812889  837837  891891  908523  960687  999999 1024947 1054053 1072071 1073709 1095633 1108107
 1145529 1162161 1198197 1224531 1270269 1307691 1324323 1378377
```



## REXX

The construction of the partitions were created in the order in which the most likely partitions would match.

```rexx
/*REXX pgm finds & shows Zumkeller numbers: 1st N; 1st odd M; 1st odd V not ending in 5.*/
parse arg n m v .                                /*obtain optional arguments from the CL*/
if n=='' | n==","  then n= 220                   /*Not specified?  Then use the default.*/
if m=='' | m==","  then m=  40                   /* "      "         "   "   "     "    */
if v=='' | v==","  then v=  40                   /* "      "         "   "   "     "    */
@zum= ' Zumkeller numbers are: '                 /*literal used for displaying messages.*/
sw= linesize() - 1                               /*obtain the usable screen width.      */
say
if n>0  then say center(' The first '       n       @zum,  sw, "═")
#= 0                                             /*the count of Zumkeller numbers so far*/
$=                                               /*initialize the  $  list  (to a null).*/
    do j=1  until #==n                           /*traipse through integers 'til done.  */
    if \Zum(j)  then iterate                     /*if not a Zumkeller number, then skip.*/
    #= # + 1;           call add$                /*bump Zumkeller count;  add to $ list.*/
    end   /*j*/

if $\==''  then say $                            /*Are there any residuals? Then display*/
say
if m>0  then say center(' The first odd '   m       @zum,  sw, "═")
#= 0                                             /*the count of Zumkeller numbers so far*/
$=                                               /*initialize the  $  list  (to a null).*/
    do j=1  by 2  until #==m                     /*traipse through integers 'til done.  */
    if \Zum(j)  then iterate                     /*if not a Zumkeller number, then skip.*/
    #= # + 1;           call add$                /*bump Zumkeller count;  add to $ list.*/
    end   /*j*/

if $\==''  then say $                            /*Are there any residuals? Then display*/
say
if v>0  then say center(' The first odd '   v       " (not ending in 5) " @zum,  sw, '═')
#= 0                                             /*the count of Zumkeller numbers so far*/
$=                                               /*initialize the  $  list  (to a null).*/
    do j=1  by 2  until #==v                     /*traipse through integers 'til done.  */
    if right(j,1)==5  then iterate               /*skip if odd number ends in digit "5".*/
    if \Zum(j)  then iterate                     /*if not a Zumkeller number, then skip.*/
    #= # + 1;           call add$                /*bump Zumkeller count;  add to $ list.*/
    end   /*j*/

if $\==''  then say $                            /*Are there any residuals? Then display*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
add$: _= strip($ j, 'L');   if length(_)<sw  then do;  $= _;  return;  end    /*add to $*/
      say  strip($, 'L');                              $= j;  return          /*say, add*/
/*──────────────────────────────────────────────────────────────────────────────────────*/
PDaS: procedure; parse arg x 1 z 1 b;  odd= x//2 /*get X  &  Z  &  B (the 1st argument).*/
      if x==1  then return 1 1                   /*handle special case for unity.       */
      r= 0;         q= 1                         /* [↓] ══integer square root══     ___ */
           do while q<=z; q=q*4; end             /*R:  an integer which will be    √ X  */
           do while q>1;  q=q%4; _= z-r-q;  r=r%2;  if _>=0  then  do;  z=_;  r=r+q;  end
           end   /*while q>1*/                   /* [↑]  compute the integer sqrt of  X.*/
      a= 1                                       /* [↓]  use all, or only odd numbers.  */
      sig = a + b                                /*initialize the sigma  (so far)   ___ */
          do j=2+odd  by 1+odd  to r - (r*r==x)  /*divide by some integers up to   √ X  */
          if x//j==0  then do;  a=a j;  b=x%j b  /*if ÷, add both divisors to α & ß.    */
                                sig= sig+j+ x%j  /*bump the sigma (the sum of divisors).*/
                           end
          end   /*j*/                            /* [↑]  %  is the REXX integer division*/
                                                 /* [↓]  adjust for a square.        ___*/
      if j*j==x  then  return sig+j  a j b       /*Was  X  a square?    If so, add  √ X */
                       return sig    a   b       /*return the divisors  (both lists).   */
/*──────────────────────────────────────────────────────────────────────────────────────*/
Zum:  procedure; parse arg x .                   /*obtain a # to be tested for Zumkeller*/
      if x<6    then return 0                    /*test if X is too low     "      "    */
      if x<945  then if x//2==1  then return 0   /*  "   " "  "  "   "  for odd    "    */
      parse value  PDaS(x)  with  sigma pdivs    /*obtain sigma and the proper divisors.*/
      if sigma//2  then return 0                 /*Is the  sigma  odd?    Not Zumkeller.*/
      #= words(pdivs)                            /*count the number of divisors for  X. */
      if #<3       then return 0                 /*Not enough divisors?    "      "     */
      if x//2      then do; _= sigma - x - x     /*use abundant optimization for odd #'s*/
                            return _>0 & _//2==0 /*Abundant is > 0 and even?  It's a Zum*/
                        end
      if #>23      then return 1                 /*# divisors is 24 or more?  It's a Zum*/

           do i=1  for #;   @.i= word(pdivs, i)  /*assign proper divisors to the @ array*/
           end   /*i*/
      c=0;            u= 2**#;   !.=.
          do p=1  for u-2;       b= x2b(d2x(p))  /*convert P──►binary with leading zeros*/
          b= right(strip(b, 'L', 0), #, 0)       /*ensure enough leading zeros for  B.  */
          r= reverse(b); if !.r\==. then iterate /*is this binary# a palindrome of prev?*/
          c= c + 1;   yy.c= b;   !.b=            /*store this particular combination.   */
          end   /*p*/

          do part=1  for c;      p1= 0;   p2= 0  /*test of two partitions add to same #.*/
          _= yy.part                             /*obtain one method of partitioning.   */
            do cp=1  for #                       /*obtain the sums of the two partitions*/
            if substr(_,cp,1)  then p1= p1 + @.cp    /*if a   one,  then add it to  P1. */
                               else p2= p2 + @.cp    /* " "  zero,    "   "   "  "  P2. */
            end   /*cp*/
          if p1==p2  then return 1               /*Partition sums equal?  Then X is Zum.*/
          end   /*part*/
      return 0                                   /*no partition sum passed.  X isn't Zum*/
```

```txt

═════════════════════════════ The first  220  Zumkeller numbers are: ══════════════════════════════
6 12 20 24 28 30 40 42 48 54 56 60 66 70 78 80 84 88 90 96 102 104 108 112 114 120 126 132 138 140
150 156 160 168 174 176 180 186 192 198 204 208 210 216 220 222 224 228 234 240 246 252 258 260
264 270 272 276 280 282 294 300 304 306 308 312 318 320 330 336 340 342 348 350 352 354 360 364
366 368 372 378 380 384 390 396 402 408 414 416 420 426 432 438 440 444 448 456 460 462 464 468
474 476 480 486 490 492 496 498 500 504 510 516 520 522 528 532 534 540 544 546 550 552 558 560
564 570 572 580 582 588 594 600 606 608 612 616 618 620 624 630 636 640 642 644 650 654 660 666
672 678 680 684 690 696 700 702 704 708 714 720 726 728 732 736 740 744 750 756 760 762 768 770
780 786 792 798 804 810 812 816 820 822 828 832 834 836 840 852 858 860 864 868 870 876 880 888
894 896 906 910 912 918 920 924 928 930 936 940 942 945 948 952 960 966 972 978 980 984

════════════════════════════ The first odd  40  Zumkeller numbers are: ════════════════════════════
945 1575 2205 2835 3465 4095 4725 5355 5775 5985 6435 6615 6825 7245 7425 7875 8085 8415 8505 8925
9135 9555 9765 10395 11655 12285 12705 12915 13545 14175 14805 15015 15435 16065 16695 17325 17955
18585 19215 19305

════════════════ The first odd  40  (not ending in 5)   Zumkeller numbers are: ════════════════
81081 153153 171171 189189 207207 223839 243243 261261 279279 297297 351351 459459 513513 567567
621621 671517 729729 742203 783783 793611 812889 837837 891891 908523 960687 999999 1024947
1054053 1072071 1073709 1095633 1108107 1145529 1162161 1198197 1224531 1270269 1307691 1324323
1378377

```



## Sidef


```ruby
func is_Zumkeller(n) {

    return false if n.is_prime
    return false if n.is_square

    var sigma = n.sigma

    # n must have an even abundance
    return false if (sigma.is_odd || (sigma < 2*n))

    # true if n is odd and has an even abundance
    return true if n.is_odd    # conjecture

    var divisors = n.divisors

    for k in (2 .. divisors.end) {
        divisors.combinations(k, {|*a|
            if (2*a.sum == sigma) {
                return true
            }
        })
    }

    return false
}

say "First 220 Zumkeller numbers:"
say (1..Inf -> lazy.grep(is_Zumkeller).first(220).join(' '))

say "\nFirst 40 odd Zumkeller numbers: "
say (1..Inf `by` 2 -> lazy.grep(is_Zumkeller).first(40).join(' '))

say "\nFirst 40 odd Zumkeller numbers not divisible by 5: "
say (1..Inf `by` 2 -> lazy.grep { _ % 5 != 0 }.grep(is_Zumkeller).first(40).join(' '))
```

```txt

First 220 Zumkeller numbers:
6 12 20 24 28 30 40 42 48 54 56 60 66 70 78 80 84 88 90 96 102 104 108 112 114 120 126 132 138 140 150 156 160 168 174 176 180 186 192 198 204 208 210 216 220 222 224 228 234 240 246 252 258 260 264 270 272 276 280 282 294 300 304 306 308 312 318 320 330 336 340 342 348 350 352 354 360 364 366 368 372 378 380 384 390 396 402 408 414 416 420 426 432 438 440 444 448 456 460 462 464 468 474 476 480 486 490 492 496 498 500 504 510 516 520 522 528 532 534 540 544 546 550 552 558 560 564 570 572 580 582 588 594 600 606 608 612 616 618 620 624 630 636 640 642 644 650 654 660 666 672 678 680 684 690 696 700 702 704 708 714 720 726 728 732 736 740 744 750 756 760 762 768 770 780 786 792 798 804 810 812 816 820 822 828 832 834 836 840 852 858 860 864 868 870 876 880 888 894 896 906 910 912 918 920 924 928 930 936 940 942 945 948 952 960 966 972 978 980 984

First 40 odd Zumkeller numbers: 
945 1575 2205 2835 3465 4095 4725 5355 5775 5985 6435 6615 6825 7245 7425 7875 8085 8415 8505 8925 9135 9555 9765 10395 11655 12285 12705 12915 13545 14175 14805 15015 15435 16065 16695 17325 17955 18585 19215 19305

First 40 odd Zumkeller numbers not divisible by 5: 
81081 153153 171171 189189 207207 223839 243243 261261 279279 297297 351351 459459 513513 567567 621621 671517 729729 742203 783783 793611 812889 837837 891891 908523 960687 999999 1024947 1054053 1072071 1073709 1095633 1108107 1145529 1162161 1198197 1224531 1270269 1307691 1324323 1378377

```



## zkl

```zkl
fcn properDivs(n){ // does not include n
//   if(n==1) return(T);	// we con't care about this case
   ( pd:=[1..(n).toFloat().sqrt()].filter('wrap(x){ n%x==0 }) )
   .pump(pd,'wrap(pd){ if(pd!=1 and (y:=n/pd)!=pd ) y else Void.Skip })
}
fcn canSum(goal,divs){
   if(goal==0 or divs[0]==goal) return(True);
   if(divs.len()>1){
      if(divs[0]>goal) return(canSum(goal,divs[1,*]));  // tail recursion
      else return(canSum(goal - divs[0], divs[1,*]) or canSum(goal, divs[1,*]));
   }
   False
}
fcn isZumkellerW(n){	// a filter for a iterator
   ds,sum := properDivs(n), ds.sum(0) + n;
   // if sum is odd, it can't be split into two partitions with equal sums
   if(sum.isOdd) return(Void.Skip);
   // if n is odd use 'abundant odd number' optimization
   if(n.isOdd){
      abundance:=sum - 2*n;
      return( if(abundance>0 and abundance.isEven) n else Void.Skip);
   }
   canSum(sum/2,ds) and n or Void.Skip	// sum is even
}
```


```zkl
println("First 220 Zumkeller numbers:");
zw:=[2..].tweak(isZumkellerW);
do(11){ zw.walk(20).pump(String,"%4d ".fmt).println() }

println("\nFirst 40 odd Zumkeller numbers:");
zw:=[3..*, 2].tweak(isZumkellerW);
do(4){ zw.walk(10).pump(String,"%5d ".fmt).println() }

println("\nThe first 40 odd Zumkeller numbers which don't end in 5 are:");
zw:=[3..*, 2].tweak(fcn(n){ if(n%5) isZumkellerW(n) else Void.Skip });
do(5){ zw.walk(8).pump(String,"%7d ".fmt).println() }
```

<pre style="font-size:83%">
First 220 Zumkeller numbers:
   6   12   20   24   28   30   40   42   48   54   56   60   66   70   78   80   84   88   90   96 
 102  104  108  112  114  120  126  132  138  140  150  156  160  168  174  176  180  186  192  198 
 204  208  210  216  220  222  224  228  234  240  246  252  258  260  264  270  272  276  280  282 
 294  300  304  306  308  312  318  320  330  336  340  342  348  350  352  354  360  364  366  368 
 372  378  380  384  390  396  402  408  414  416  420  426  432  438  440  444  448  456  460  462 
 464  468  474  476  480  486  490  492  496  498  500  504  510  516  520  522  528  532  534  540 
 544  546  550  552  558  560  564  570  572  580  582  588  594  600  606  608  612  616  618  620 
 624  630  636  640  642  644  650  654  660  666  672  678  680  684  690  696  700  702  704  708 
 714  720  726  728  732  736  740  744  750  756  760  762  768  770  780  786  792  798  804  810 
 812  816  820  822  828  832  834  836  840  852  858  860  864  868  870  876  880  888  894  896 
 906  910  912  918  920  924  928  930  936  940  942  945  948  952  960  966  972  978  980  984 

First 40 odd Zumkeller numbers:
  945  1575  2205  2835  3465  4095  4725  5355  5775  5985 
 6435  6615  6825  7245  7425  7875  8085  8415  8505  8925 
 9135  9555  9765 10395 11655 12285 12705 12915 13545 14175 
14805 15015 15435 16065 16695 17325 17955 18585 19215 19305 

The first 40 odd Zumkeller numbers which don't end in 5 are:
  81081  153153  171171  189189  207207  223839  243243  261261 
 279279  297297  351351  459459  513513  567567  621621  671517 
 729729  742203  783783  793611  812889  837837  891891  908523 
 960687  999999 1024947 1054053 1072071 1073709 1095633 1108107 
1145529 1162161 1198197 1224531 1270269 1307691 1324323 1378377 

```

