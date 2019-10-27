+++
title = "Execute SNUSP/Tcl"
description = ""
date = 2010-11-18T10:36:24Z
aliases = []
[extra]
id = 4196
[taxonomies]
categories = []
tags = []
+++

{{implementation|SNUSP}}{{collection|RCSNUSP}}[[implementation of task::RCSNUSP| ]]
These are [[Tcl]] interpreters for [[SNUSP]], and they appear to work with correct SNUSP programs.

Historically, the Bloated interpreter was evolved from the Modular interpreter, which in turn was evolved from the Core interpreter.

The most interesting feature of the '''Core''' interpreter is that it models the data store as an unbounded half-line (implemented in a [[Tcl]] list), dynamically extending it “on the right” to have as many cells as necessary. Also, when a failure occurs (going outside the program space, going before the start of the data store) then a multi-level “break” exception is thrown to make the main loop exit.

The '''Modular''' interpreter adds a basic stack; it is not very much more interesting than the Core interpreter but was immediately evolved from it in order to be able to read the majority of SNUSP programs that exist out there (this is the most common dialect).

The interpreter for '''Bloated''' SNUSP is considerably more complex, but most of this complexity is limited to the outer parts of the main loop (plus the change in addressing model for the data store, which becomes an unbounded quarter plane). The principal interesting feature is the use of a dictionary to hold the thread-specific values, together with the use of <tt>dict with</tt> to break these variables temporarily out into global variables for processing. The definition of the READ operation also had to change to become non-blocking so that non-availability of a character would not block other threads. In theory, this could have been done with native OS threads, but then the fairness constraints described in the SNUSP language definition would not have been respected. Thread termination is performed by simply deleting the thread's data from the dictionary of threads.
<br clear=all>
==Core [[SNUSP]]==

```tcl
package require Tcl 8.5

# Basic I/O to read the program data and get ready for execution
set f [open [lindex $argv 0]]
set data [read $f]
close $f
fconfigure stdout -buffering none

# How to access the program
set pc {0 0}
set lineNum 0
foreach line [split $data \n] {
    set idx [string first \$ $line]
    if {$idx >= 0} {
	set pc [list $lineNum $idx]
    }
    lappend program [split $line ""]
    incr lineNum
}
set move {0 1}
proc Move {} {
    global pc move
    lset pc 0 [expr {[lindex $pc 0] + [lindex $move 0]}]
    lset pc 1 [expr {[lindex $pc 1] + [lindex $move 1]}]
}
proc Char {} {
    global program pc
    set c [lindex $program $pc]
    if {$c eq ""} {
	return -code break
    }
    return $c
}

# An unbounded linear datastore
set data 0
set dptr 0
proc Get {} {
    global data dptr
    if {$dptr < 0} {
        puts stderr "program error; data pointer too low"
        return -code break
    }
    while {$dptr >= [llength $data]} {
	lappend data 0
    }
    lindex $data $dptr
}
proc Set val {
    global data dptr
    if {$dptr < 0} {
        puts stderr "program error; data pointer too low"
        return -code break
    }
    while {$dptr >= [llength $data]} {
	lappend data 0
    }
    lset data $dptr $val
}

# The main interpreter loop; $last is used for tracking whether
# to terminate output with a newline
set last 10
while 1 {
    switch -- [Char] {
	"/"  {set move "[expr -[lindex $move 1]] [expr -[lindex $move 0]]"}
	"\\" {set move [lreverse $move]}
	"?"  {if ![Get] Move}
	"!"  {Move}
	">"  {incr dptr}
	"<"  {incr dptr -1}
	"+"  {Set [expr [Get]+1]}
	"-"  {Set [expr [Get]-1]}
	"."  {puts -nonewline [format %c [set last [Get]]]}
	","  {Set [scan [read stdin 1] %c]}
    }
    Move
}
if {$last != 10} {puts ""}
```


==Modular [[SNUSP]]==

```tcl
package require Tcl 8.5

# Basic I/O to read the program data and get ready for execution
set f [open [lindex $argv 0]]
set data [read $f]
close $f
fconfigure stdout -buffering none

# How to access the program
set pc {0 0}
set lineNum 0
foreach line [split $data \n] {
    set idx [string first \$ $line]
    if {$idx >= 0} {
	set pc [list $lineNum $idx]
    }
    lappend program [split $line ""]
    incr lineNum
}
set move {0 1}
proc Move {} {
    global pc move
    lset pc 0 [expr {[lindex $pc 0] + [lindex $move 0]}]
    lset pc 1 [expr {[lindex $pc 1] + [lindex $move 1]}]
}
proc Char {} {
    global program pc
    set c [lindex $program $pc]
    if {$c eq ""} {
	return -code break
    }
    return $c
}

# An unbounded linear datastore
set data 0
set dptr 0
proc Get {} {
    global data dptr
    if {$dptr < 0} {
        puts stderr "program error; data pointer too low"
        return -code break
    }
    while {$dptr >= [llength $data]} {
	lappend data 0
    }
    lindex $data $dptr
}
proc Set val {
    global data dptr
    if {$dptr < 0} {
        puts stderr "program error; data pointer too low"
        return -code break
    }
    while {$dptr >= [llength $data]} {
	lappend data 0
    }
    lset data $dptr $val
}

# An unbounded stack
set stack {}
proc Push {} {
    global stack pc move
    set save $pc
    Move
    lappend stack [list $pc $move]
    set pc $save
}
proc Pop {} {
    global stack pc move
    if {[llength $stack] == 0} {
	return -code break
    }
    lassign [lindex $stack end] pc move
    set stack [lrange $stack 0 end-1]
}

# The main interpreter loop; $last is used for tracking whether
# to terminate output with a newline
set last 10
while 1 {
    switch -- [Char] {
	"/"  {set move "[expr -[lindex $move 1]] [expr -[lindex $move 0]]"}
	"\\" {set move [lreverse $move]}
	"?"  {if ![Get] Move}
	"!"  {Move}
	">"  {incr dptr}
	"<"  {incr dptr -1}
	"+"  {Set [expr [Get]+1]}
	"-"  {Set [expr [Get]-1]}
	"."  {puts -nonewline [format %c [set last [Get]]]}
	","  {Set [scan [read stdin 1] %s]}
	"@"  {Push}
	"#"  {Pop}
    }
    Move
}
if {$last != 10} {puts ""}
```


==Bloated [[SNUSP]]==

```tcl
package require Tcl 8.5
 
# Basic I/O to read the program data and get ready for execution
set f [open [lindex $argv 0]]
set data [read $f]
close $f
fconfigure stdin  -blocking 0
fconfigure stdout -buffering none
 
# How to access the program
set pc {0 0}
set lineNum 0
foreach line [split $data \n] {
    set idx [string first \$ $line]
    if {$idx >= 0} {
	set pc [list $lineNum $idx]
    }
    lappend program [split $line ""]
    incr lineNum
}
set move {0 1}

set threads {}
set threadID 0
proc Move {} {
    global pc move
    lset pc 0 [expr {[lindex $pc 0] + [lindex $move 0]}]
    lset pc 1 [expr {[lindex $pc 1] + [lindex $move 1]}]
}
proc ReflectMotion dir {
    global move
    set move "[expr $dir*[lindex $move 1]] [expr $dir*[lindex $move 0]]"
}
proc Char {} {
    global program pc threads t
    set c [lindex $program $pc]
    if {$c eq ""} {
	dict unset threads $t
	return -code continue
    }
    return $c
}
 
# An unbounded quarter-plane datastore
set data 0
set dptr {0 0}
proc Get {} {
    global data dptr
    lindex $data $dptr
}
proc Set val {
    global data dptr
    lset data $dptr $val
}
proc MovePtr dir {
    global dptr data
    lassign $dptr y x
    switch $dir {
	left  {lset dptr 1 [expr $x-1]}
	right {lset dptr 1 [expr $x+1]}
	up    {lset dptr 0 [expr $y-1]}
	down  {lset dptr 0 [expr $y+1]}
    }
    lassign $dptr y x
    if {$x < 0 || $y < 0} {
	puts stderr "program error; data pointer off top or left ($x,$y)"
	exit
    }
    while {[llength $data] <= $y} {
	lappend data [lrepeat [expr $x+1] 0]
    }
    if {[llength [lindex $data $y]] <= $x} {
	set e [lindex $data $y]
	lset data $y [concat $e [lrepeat [expr $x+1-[llength $e]] 0]]
    }
}
 
# An unbounded stack
set stack {}
proc Push {} {
    global stack pc move
    set save $pc
    Move
    lappend stack [list $pc $move]
    set pc $save
}
proc Pop {} {
    global stack pc move t threads
    if {[llength $stack] == 0} {
	dict unset threads $t
	return -code continue
    }
    lassign [lindex $stack end] pc move
    set stack [lrange $stack 0 end-1]
}

dict set threads [incr threadID] [dict create \
    pc $pc move $move stack $stack dptr $dptr]
# The main interpreter loop; $last is used for tracking whether to terminate
# output with a newline
set last 10
while {[dict size $threads]} {
    foreach t [dict keys $threads] {
	dict with threads $t {
	    switch -- [Char] {
		"/"  {ReflectMotion -1}
		"\\" {ReflectMotion  1}
		"?"  {if ![Get] Move}
		"!"  {Move}
		">"  {MovePtr right}
		"<"  {MovePtr left}
		"+"  {Set [expr [Get]+1]}
		"-"  {Set [expr [Get]-1]}
		"."  {puts -nonewline [format %c [set last [Get]]]}
		","  {
		    set c [read stdin 1]
		    if {$c eq ""} continue
		    Set [scan $c %c]
		}
		"@"  {Push}
		"#"  {Pop}
		":"  {MovePtr up}
		";"  {MovePtr down}
		"%"  {Set [expr int((1+[Get]) * rand())]}
		"&"  {
		    Move
		    dict set threads [incr threadID] [dict create \
			pc $pc move $move stack {} dptr $dptr]
		}
	    }
	    Move
	}
    }
}
if {$last != 10} {puts ""}
```

==Test Code==

### Samples of Bloated SNUSP

This prints out a random number of random length:

```txt
<nowiki>$!/+++++++++%++++++++++++++++++++++++++++++++++++++++++++++++.!/-\
  \
### ==============================
<#!?%+++++++++++++++>===\?/</nowiki>
```

This (from the esolangs wiki) prints exclamation marks until you press a key:

```txt
<nowiki>                    /==.==<==\
                    |        |
     /+++++++++++==&\==>===?!/==<<==#
     \+++++++++++\  |
$==>==+++++++++++/  \==>==,==#</nowiki>
```

