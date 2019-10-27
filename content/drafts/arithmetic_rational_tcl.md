+++
title = "Arithmetic/Rational/Tcl"
description = ""
date = 2012-02-16T19:55:13Z
aliases = []
[extra]
id = 5286
[taxonomies]
categories = []
tags = []
+++

<noinclude>{{collection|Rational Arithmetic}}[[implementation of task::Arithmetic/Rational| ]]</noinclude>
Code to find factors of a number not shown:

```tcl
namespace eval rat {}

proc rat::new {args} {
    if {[llength $args] == 0} {
        set args {0}
    }
    lassign [split {*}$args] n d
    if {$d == 0} {
        error "divide by zero"
    }
    if {$d < 0} {
        set n [expr {-1 * $n}]
        set d [expr {abs($d)}]
    }
    return [normalize $n $d]
}

proc rat::split {args} {
    if {[llength $args] == 1} {
        lassign [::split $args /] n d
        if {$d eq ""} {
            set d 1
        }
    } else {
        lassign $args n d
    }
    return [list $n $d]
}

proc rat::join {rat} {
    lassign $rat n d
    if {$n == 0} {
        return 0
    } elseif {$d == 1} {
        return $n
    } else {
        return $n/$d
    }
}

proc rat::normalize {n d} {
    set gcd [gcd $n $d]
    return [join [list [expr {$n/$gcd}] [expr {$d/$gcd}]]]
}

proc rat::gcd {a b} {
    while {$b != 0} {
        lassign [list $b [expr {$a % $b}]] a b
    }
    return $a
}

proc rat::abs {rat} {
    lassign [split $rat] n d
    return [join [list [expr {abs($n)}] $d]]
}

proc rat::inv {rat} {
    lassign [split $rat] n d
    return [normalize $d $n]
}

proc rat::+ {args} {
    set n 0
    set d 1
    foreach arg $args {
        lassign [split $arg] an ad
        set n [expr {$n*$ad + $an*$d}]
        set d [expr {$d * $ad}]
    }
    return [normalize $n $d]
}

proc rat::- {args} {
    lassign [split [lindex $args 0]] n d
    if {[llength $args] == 1} {
        return [join [list [expr {-1 * $n}] $d]]
    }
    foreach arg [lrange $args 1 end] {
        lassign [split $arg] an ad
        set n [expr {$n*$ad - $an*$d}]
        set d [expr {$d * $ad}]
    }
    return [normalize $n $d]
}

proc rat::* {args} {
    set n 1
    set d 1
    foreach arg $args {
        lassign [split $arg] an ad
        set n [expr {$n * $an}]
        set d [expr {$d * $ad}]
    }
    return [normalize $n $d]
}

proc rat::/ {a b} {
    set r [* $a [inv $b]]
    if {[string match */0 $r]} {
        error "divide by zero"
    }
    return $r
}

proc rat::== {a b} {
    return [expr {[- $a $b] == 0}]
}

proc rat::!= {a b} {
    return [expr { ! [== $a $b]}]
}

proc rat::< {a b} {
    lassign [split [- $a $b]] n d
    return [expr {$n < 0}]
}

proc rat::> {a b} {
    lassign [split [- $a $b]] n d
    return [expr {$n > 0}]
}

proc rat::<= {a b} {
    return [expr { ! [> $a $b]}]
}

proc rat::>= {a b} {
    return [expr { ! [< $a $b]}]
}

################################################
proc is_perfect {num} {
    set sum [rat::new 0]
    foreach factor [all_factors $num] {
        set sum [rat::+ $sum [rat::new 1/$factor]]
    }
    # note, all_factors includes 1, so sum should be 2
    return [rat::== $sum 2]
}

proc get_perfect_numbers {} {
    set t [clock seconds]
    set limit [expr 2**19]
    for {set num 2} {$num < $limit} {incr num} {
        if {[is_perfect $num]} {
            puts "perfect: $num"
        }
    }
    puts "elapsed: [expr {[clock seconds] - $t}] seconds"

    set num [expr {2**12 * (2**13 - 1)}] ;# 5th perfect number
    if {[is_perfect $num]} {
        puts "perfect: $num"
    }
}

source primes.tcl
get_perfect_numbers
```

{{out}}

```txt
perfect: 6
perfect: 28
perfect: 496
perfect: 8128
elapsed: 477 seconds
perfect: 33550336
```

