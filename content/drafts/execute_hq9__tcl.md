+++
title = "Execute HQ9+/Tcl"
description = ""
date = 2010-11-18T10:40:10Z
aliases = []
[extra]
id = 4574
[taxonomies]
categories = []
tags = []
+++

{{collection|RCHQ9+}}{{implementation|HQ9+}}[[implementation of task::RCHQ9+| ]]
These implementations have the feature that they are quines of themselves in the language that they accept (so long as you take a generous definition of what to ignore; the HQ9+ definition is ambiguous enough that it allows such an interpretation). So the implementation of a language with a quine operation is a quine in the language implemented. This feels like a rather appropriate restriction...

{{works with|Tcl|8.4}}
<br clear=right>
==Interpreter==

```tcl
set d [read [expr {$argc?[open [lindex $argv 0]]:"stdin"}]]
for {set i [set a 0]} {$i<[string len $d]} {incr i} {
    switc\u0068 -- [string index [string tolower $d] $i] \u0068 {
	puts "\u0048ello, world!"
    } q {puts -nonewline $d} [expr 10-1] {
	for {set b [expr 100-1]} 1 {} {
	    puts "$b bottle[expr {$b-1?{s}:{}}] of beer on t\u0068e wall"
	    puts "$b bottle[expr {$b-1?{s}:{}}] of beer"
	    incr b -1
	    puts "take one down and pass it around"
	    puts "$b bottle[expr {$b-1?{s}:{}}] of beer on t\u0068e wall"
	    if {$b} {puts ""} else break
	}
    } \x2b {incr a}
}
```

==Compiler==
To be fair, this compiler then immediately executes the code. Replace the <code>eval</code> with <code>puts</code> to see what it generates.

```tcl
set d [read [expr {[set a $argc]?[open [lindex $argv [set a 0]]]:"stdin"}]]
eval [string map [list \u0068 {puts "\u0048ello, world!";} q \
    [list puts -nonewline $d]\n \071 {for {set b [expr 100-1]} 1 {} {
    puts "$b bottle[expr {$b-1?{s}:{}}] of beer on t\u0068e wall"
    puts "$b bottle[expr {$b-1?{s}:{}}] of beer"
    incr b -1
    puts "take one down and pass it around"
    puts "$b bottle[expr {$b-1?{s}:{}}] of beer on t\u0068e wall"
    if {$b} {puts ""} else break
    };} \x2b {incr a;}
] [regsub -all {[\u0000-*,-8:-gi-pr-\uffff]*} $d {}]]
```

