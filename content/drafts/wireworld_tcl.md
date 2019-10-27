+++
title = "Wireworld/Tcl"
description = ""
date = 2010-11-18T10:44:02Z
aliases = []
[extra]
id = 4910
[taxonomies]
categories = []
tags = []
+++

This is a graphical implementation of [[implementation of task::Wireworld]].
{{works with|Tcl|8.6}} or {{libheader|TclOO}}
{{libheader|Tk}}

```tcl
package require TclOO
package require Tk
 
# The color scheme.
# The order is: empty, conductor, electronTail, electronHead
set colors "#000000 #000080 #8080ff #ffffff"
 
# Encapsulate the per-cell logic in a class to simplify it
oo::class create wireCell {
    variable X Y S0 S1 Neighbours
 
    constructor {state x y} {
	upvar 1 at at
 
	set X $x
	set Y $y
 
	switch -- $state {
	    conductor {
		set S0 1
	    }
	    electronTail {
		set S0 2
	    }
	    electronHead {
		set S0 3
	    }
	    default {
		return -code error "invalid state name \"$state\""
	    }
	}
	set at($x,$y) [self]
    }
 
    # Method used to allow each (non-background) cell to know about its
    # surrouding non-background cells. This makes the connectivity
    # calculations much simpler and faster!
    method initConnectivity {} {
	upvar 1 at at
	foreach dx {-1 -1 -1 0 0 1 1 1} dy {-1 0 1 -1 1 -1 0 1} {
	    set pos [expr {$X+$dx}],[expr {$Y+$dy}]
	    if {[info exists at($pos)]} {
		lappend Neighbours $at($pos)
	    }
	}
    }
 
    method state {} {return $S0}
    method x {} {return $X}
    method y {} {return $Y}
 
    # Perform the transition in two stages, so that we can do the transition
    # "simultaneously" across all cells. The transition0 method calculates
    # what state we're going to change to, and the transition1 method actually
    # moves to the state.
    method transition0 {} {
	if {$S0 == 3} {
	    set S1 2
	} elseif {$S0 == 2} {
	    set S1 1
	} else {
	    set count 0
	    foreach n $Neighbours {
		incr count [expr {[$n state] == 3}]
	    }
	    set S1 [expr {($count == 1 || $count == 2) ? 3 : 1}]
	}
    }
    method transition1 {} {
	set S0 $S1
    }
}

# A subclass that knows how to display state changes on the GUI
oo::class create GUIwireCell {
    superclass wireCell
    variable S0 S1 X Y
    constructor {state x y} {
	global colors
	next $state $x $y
	pixels put [lindex $colors $S0] -to $X $Y
    }
    method transition1 {} {
	# Only do the plot of the state changed; more efficient
	if {$S0 != $S1} {
	    global colors
	    pixels put [lindex $colors [next]] -to $X $Y
	}
    }
}

# How to load a layout/program from a file
proc loadWires filename {
    global cells colors
 
    # Read the file in
    set f [open $filename]
    set data [read $f]
    close $f
 
    # Initialize the list of interacting cells and the connectivity map
    set cells {}
    array set at {}
 
    # Calculate the width of the program
    set lines [split $data \n]
    set len 0
    foreach line $lines {
	if {[string length $line] > $len} {
	    set len [string length $line]
	}
    }
 
    # Create the arena image
    image create photo pixels
 
    # Initialize the image to "empty cell"s; interacting parts will be overlaid
    pixels put [lindex $colors 0] -to 0 0 $len [llength $lines]
 
    # Parse the input data and create the interacting cells
    set y 0
    foreach line $lines {
	set x 0
	foreach char [split $line {}] {
	    switch $char {
		H {
		    lappend cells [GUIwireCell new electronHead $x $y]
		}
		t {
		    lappend cells [GUIwireCell new electronTail $x $y]
		}
		. {
		    lappend cells [GUIwireCell new conductor $x $y]
		}
	    }
	    incr x
	}
	incr y
    }
 
    # Now inform each cell about its connectivity
    foreach cell $cells {
	$cell initConnectivity
    }
    unset at
}

# How to save the current state as a layout that [loadWires] can load
proc saveWires {filename} {
    global cells

    # Make a grid of empty characters of the right size
    set chs [lrepeat [image height pixels] [lrepeat [image width pixels] " "]]

    # Transcribe the non-empty cells into the grid
    foreach cell $cells {
	lset chs [$cell y] [$cell x] [string index " .tH" [$cell state]]
    }

    # Write the characters to stdout or a file
    if {$filename eq "-"} {set f stdout} else {set f [open $filename w]}
    foreach row $chs {
	puts $f [join $row ""]
    }
    if {$f ne "stdout"} {close $f}
}

# How to perform the animation timestep
proc timeStep {t} {
    global cells
 
    # Arm the transition for all interacting cells
    foreach cell $cells {
	$cell transition0
    }
    # Perform the transition for all interacting cells
    foreach cell $cells {
	$cell transition1
    }
 
    # Reschedule
    after $t [list timeStep $t]
}
 
# Initialize the GUI (such as it is) and load and start the animation
wm title . "Wireworld: [lindex $argv 0]"
loadWires [lindex $argv 0]
pack [label .l -image pixels]
bind . <KeyPress> {saveWires -}
after 1000 timeStep 250
```

