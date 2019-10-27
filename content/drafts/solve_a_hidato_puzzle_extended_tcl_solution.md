+++
title = "Solve a Hidato puzzle/Extended Tcl solution"
description = ""
date = 2012-05-02T12:50:20Z
aliases = []
[extra]
id = 11674
[taxonomies]
categories = []
tags = []
+++

This version handles even tricky cases where there are many possible choices of each path but only one leads to a solution (as in <tt>awkwardcase</tt> below). The core of this is the <code>ForkSolve</code> method, which clones the current object and speculatively searches forward from there. <small>(Note, you need a very recent build of Tcl 8.6 to make this work, as it depends on a feature change from 2012-03-27.)</small>
{{works with|Tcl|8.6}}

```tcl
package require Tcl 8.6

oo::class create Hidato {
    variable grid max filled
    constructor {initialConfiguration} {
	set max 1
	set y 0
	foreach row [split [string trim $initialConfiguration "\n"] "\n"] {
	    set x 0
	    set rowcontents {}
	    foreach cell $row {
		if {![string is integer -strict $cell]} {set cell -1}
		lappend rowcontents $cell
		set max [expr {max($max, $cell)}]
		if {$cell > 0} {
		    dict set filled $cell [list $y $x]
		}
		incr x
	    }
	    lappend grid $rowcontents
	    incr y
	}
    }

    method FindSeps {} {
	set result {}
	for {set i 1} {$i < $max-1} {incr i} {
	    if {[dict exists $filled $i]} {
		for {set j [expr {$i+1}]} {$j <= $max} {incr j} {
		    if {[dict exists $filled $j]} {
			if {$j-$i > 1} {
			    lappend result [list $i $j [expr {$j-$i}]]
			}
			break
		    }
		}
	    }
	}
	return [lsort -integer -index 2 $result]
    }

    method MakePaths {sep} {
	lassign $sep from to len
	lassign [dict get $filled $from] y x
	set result {}
	foreach {dx dy} {-1 -1  -1 0  -1 1  0 -1  0 1  1 -1  1 0  1 1} {
	    my Discover [expr {$x+$dx}] [expr {$y+$dy}] [expr {$from+1}] $to \
		[list [list $from $x $y]] $grid
	}
	return $result
    }
    method Discover {x y n limit path model} {
	# Check for illegal
	if {[lindex $model $y $x] != 0} return
	upvar 1 result result
	lassign [dict get $filled $limit] ly lx
	# Special case
	if {$n == $limit-1} {
	    if {abs($x-$lx)<=1 && abs($y-$ly)<=1 && !($lx==$x && $ly==$y)} {
		lappend result \
		    [lappend path [list $n $x $y] [list $limit $lx $ly]]
	    }
	    return
	}
	# Check for impossible
	if {abs($x-$lx) > $limit-$n || abs($y-$ly) > $limit-$n} return
	# Recursive search
	lappend path [list $n $x $y]
	lset model $y $x $n
	incr n
	foreach {dx dy} {-1 -1  -1 0  -1 1  0 -1  0 1  1 -1  1 0  1 1} {
	    my Discover [expr {$x+$dx}] [expr {$y+$dy}] $n $limit $path $model
	}
    }

    forward log puts

    method applyPath {path {bit "unique path"}} {
	my log "Found $bit for [lindex $path 0 0] -> [lindex $path end 0]"
	foreach cell [lrange $path 1 end-1] {
	    lassign $cell n x y
	    lset grid $y $x $n
	    dict set filled $n [list $y $x]
	}
    }

    method print {} {
	foreach row $grid {
	    foreach cell $row {
		puts -nonewline [format " %*s" [string length $max] [expr {
		    $cell==-1 ? "." : $cell
		}]]
	    }
	    puts ""
	}
    }

    method state {} {list $grid $max $filled}

    method ForkSolve {paths} {
	my log "Choice of [llength $paths] possible paths"
	foreach p $paths {
	    set subobj [oo::copy [self]]
	    try {
		$subobj applyPath $p "path #[incr count]"
		if {[$subobj solve]} {
		    my log "Leads to solution!"
		    lassign [$subobj state] grid max filled
		    return -code break
		} else {
		    my log "No solution?"
		}
	    } finally {
		$subobj destroy
	    }
	}
    }
    method solve {} {
	set limit [llength [my FindSeps]]
	while {[llength [set seps [my FindSeps]]] && [incr limit -1]>=0} {
	    set pshort {}
	    foreach sep $seps {
		set paths [my MakePaths $sep]
		if {[llength $paths] == 1} {
		    my applyPath [lindex $paths 0]
		    set pshort {}
		    break
		} elseif {![llength $pshort]} {
		    set pshort $paths
		} elseif {[llength $pshort] > [llength $paths]} {
		    set pshort $paths
		}
	    }
	    if {[llength $pshort]} {
		my ForkSolve $pshort
		return false
	    }
	}
	return true
    }
}

Hidato create sample {
     0  33  35   0   0   .   .   .
     0   0  24  22   0   .   .   .
     0   0   0  21   0   0   .   .
     0  26   0  13  40  11   .   .
    27   0   0   0   9   0   1   .
     .   .   0   0  18   0   0   .
     .   .   .   .   0   7   0   0 
     .   .   .   .   .   .   5   0 
}
sample solve
puts ""   ;# Blank line for clarity in output
sample print
puts ""

Hidato create awkwardcase {
    . 4 .
    0 7 0
    1 0 0
}
awkwardcase solve
puts ""
awkwardcase print
```

{{out}}

```txt

Found unique path for 5 -> 7
Found unique path for 7 -> 9
Found unique path for 9 -> 11
Found unique path for 11 -> 13
Found unique path for 33 -> 35
Found unique path for 18 -> 21
Found unique path for 1 -> 5
Found unique path for 35 -> 40
Found unique path for 22 -> 24
Found unique path for 24 -> 26
Found unique path for 27 -> 33
Found unique path for 13 -> 18

 32 33 35 36 37  .  .  .
 31 34 24 22 38  .  .  .
 30 25 23 21 12 39  .  .
 29 26 20 13 40 11  .  .
 27 28 14 19  9 10  1  .
  .  . 15 16 18  8  2  .
  .  .  .  . 17  7  6  3
  .  .  .  .  .  .  5  4

Choice of 2 possible paths
Found path #1 for 1 -> 4
Found unique path for 4 -> 7
Leads to solution!

 . 4 .
 3 7 5
 1 2 6

```

