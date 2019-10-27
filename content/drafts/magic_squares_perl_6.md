+++
title = "Magic squares/Perl 6"
description = ""
date = 2016-03-18T21:59:10Z
aliases = []
[extra]
id = 20584
[taxonomies]
categories = []
tags = []
+++

Rather than having multiple examples for different orders of magic square, this will generate a magic square for ''any'' valid n x n grid.

Invoke at the command line and pass in the desired size as a parameter.

{{works with|Rakudo|2016-02}}

See:<ul>
<li>[[Magic_squares_of_odd_order#Perl_6|Magic squares of odd order#Perl 6]]</li>
<li>[[Magic_squares_of_singly_even_order#Perl_6|Magic squares of singly even order#Perl 6]]</li>
<li>[[Magic_squares_of_doubly_even_order#Perl_6|Magic squares of doubly even order#Perl 6]]</li>
</ul>


```perl6
sub MAIN (Int $n where {$n > 0}) {

    my @sq;
    my $i = 1;
    my $h = $n div 2;
    my $q = $n div 4;
    
    gen-sq($n);

    say .fmt("%{$i.chars}d", ' ') for @sq;

    say "\nThe magic number is ", [+] @sq[0].list;


    multi sub gen-sq (2) { # invalid
        note "Sorry, can not generate a 2 x 2 magic square." and exit;
    }

    multi sub gen-sq ($n where {$n % 2}) { # odd
        my $x = $n/2;
        my $y = 0;
        @sq[($i % $n ?? $y-- !! $y++) % $n][($i % $n ?? $x++ !! $x) % $n] = $i++ for ^$n²;
    }

    multi sub gen-sq ($n where {$n %% 4}) { # doubly even
        my $x = 0;
        my $y = 0;
        @sq[$i % $n ?? $y !! $y++][($i-1) % $n] = $i++ for ^$n²;
        for ^$q -> $r {
            for $q ..^ $n - $q -> $c {
                my $ŕ = $n - 1 - $r;
                my $ć = $n - 1 - $c;
                (@sq[$r;$c], @sq[$ŕ;$ć]) = (@sq[$ŕ;$ć], @sq[$r;$c]);
                (@sq[$c;$r], @sq[$ć;$ŕ]) = (@sq[$ć;$ŕ], @sq[$c;$r]);
            }
        }
    }

    multi sub gen-sq ($n where {$n %% 2 and $n % 4}) { # singly even
        gen-sq($h);
        $i *= 4;
        for ^$h -> $r {
            for ^$h -> $c {
                @sq[$r + $h; $c]      = @sq[$r;$c] + $h² * 3;
                @sq[$r; $c + $h]      = @sq[$r;$c] + $h² * 2;
                @sq[$r + $h; $c + $h] = @sq[$r;$c] + $h²;
            }
            for ^$q -> $c {
                next if $c == 0 and $r == ($h-1) div 2;
                (@sq[$r;$c], @sq[$r + $h;$c]) = (@sq[$r + $h;$c], @sq[$r;$c]);
            }
            if $h > 4 {
                for ($n - $q + 1) ..^ $n -> $c {
                        (@sq[$r;$c], @sq[$r + $h;$c]) = (@sq[$r + $h;$c], @sq[$r;$c]);
                }
            }
        }
        (@sq[$q;$q], @sq[$q+$h;$q]) = (@sq[$q+$h;$q], @sq[$q;$q]);
    }
}
```

