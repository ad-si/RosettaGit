+++
title = "Tcl"
description = ""
date = 2014-11-21T02:39:05Z
aliases = []
[extra]
id = 5293
[taxonomies]
categories = []
tags = []
+++

{{collection|99 Bottles of Beer}} [[implementation of task::99 Bottles of Beer| ]]


### using variable traces

Here's a version that uses Tcl's variable traces
to set a global "bottle string" whenever the counter variable is set.

```tcl
proc setBottles {varName args} {
    upvar #0 $varName n
    set ::bottles [format "%d bottle%s" $n [expr {$n == 1 ? "" : "s"}]]
}

trace add variable i write setBottles

for {set i 99} {$i > 0} {} {
    puts "$bottles of beer on the wall"
    puts "$bottles of beer"
    puts "take one down, pass it around"
    incr i -1
    puts "$bottles of beer on the wall\n"
}
```




### Wordy version


```tcl
set s "s"; set ob "of beer"; set otw "on the wall"; set more "Take one down and pass it around"
for {set n 100} {$n ne "No more"} {} {
	switch -- [incr n -1] {
		1 {set s ""}
		0 {set s "s"; set n "No more"; set more "Go to the store and buy some more"}
	}
	lappend verse ". $n bottle$s $ob $otw.\n"
	lappend verse "\n$n bottle$s $ob $otw, [string tolower $n] bottle$s $ob.\n$more"
}
puts -nonewline [join [lreplace $verse 0 0] ""][lindex $verse 0]
```

Version which converts numbers to words, optimized for script length while retaining readability:

```tcl
proc 0-19 {n} {
    lindex {"no more" one two three four five six seven eight nine ten eleven
            twelve thirteen fourteen fifteen sixteen seventeen eighteen nineteen} $n
}

proc TENS {n} {
    lindex {twenty thirty fourty fifty sixty seventy eighty ninety} [expr {$n - 2}]
}

proc num2words {n} {
    if {$n < 20} {return [0-19 $n]}
    set tens [expr {$n / 10}]
    set ones [expr {$n % 10}]
    if {$ones == 0} {return [TENS $tens]}
    return "[TENS $tens]-[0-19 $ones]"
}

proc get_words {n} {
    return "[num2words $n] bottle[expr {$n != 1 ? "s" : ""}] of beer"
}

for {set i 99} {$i > 0} {incr i -1} {
    puts [string totitle "[get_words $i] on the wall, [get_words $i]."]
    puts "Take one down and pass it around, [get_words [expr {$i - 1}]] on the wall.\n"
}

puts "No more bottles of beer on the wall, no more bottles of beer."
puts "Go to the store and buy some more, 99 bottles of beer on the wall."
```



===99-bottles-of-beer.net===
from http://99-bottles-of-beer.net/language-tcl-439.html

```tcl
proc bottles {i} {
    return "$i bottle[expr {$i!=1?{s}:{}}] of beer"
}

proc line123 {i} {
    puts "[bottles $i] on the wall,"
    puts "[bottles $i],"
    puts "take one down, pass it around,"
}

proc line4 {i} {
    puts "[bottles $i] on the wall.\n"
}

for {set i 99} {$i>0} {} {
    line123 $i
    incr i -1
    line4 $i
}
```



### The Boozy Version

A [http://99-bottles-of-beer.net/language-expect-249.html
particularly entertaining version] is [[wp:Don Libes|Don Libes]]’s coding
from the mid-'90s in [[Expect]],
which "... SIMULATES a human typing the beer song."

This is a version of that code, adapted to use modern coding styles,
and not require any extensions.

{{works with|Tcl|8.4}}

```tcl
# 99 bottles of beer on the wall, Expect-style
# Author: Don Libes <libes@nist.gov>
### Adapted by: Donal K. Fellows <donal.k.fellows@manchester.ac.uk>

# Unlike programs (http://www.ionet.net/~timtroyr/funhouse/beer.html)
# which merely print out the 99 verses, this one SIMULATES a human
# typing the beer song.  Like a real human, typing mistakes and timing
# becomes more erratic with each beer - the final verse is barely
# recognizable and it is really like watching a typist hunt and peck
# while drunk.

# Finally, no humans actually sing all 99 verses - particularly when
# drunk.  In reality, they occasionally lose their place (or just get
# bored) and skip verses, so this program does likewise.

proc bottles {i} {
    return "$i bottle[expr {$i!=1?{s}:{}}] of beer"
}
proc line123 {i} {
    out $i "[bottles $i] on the wall,\n"
    out $i "[bottles $i],\n"
    out $i "take one down, pass it around,\n"
}
proc line4 {i} {
    out $i "[bottles $i] on the wall.\n\n"
}
proc out {i s} {
    boozyType $i [beerifyString $i $s]
}

### Factored the code to make drunken edits to the song
proc beerifyString {i s} {
    foreach ch [split $s ""] {
	# don't touch punctuation; just looks too strange if you do
	if {[regexp {[,. \n]} $ch]} {
	    append d $ch
	    continue
	}

	# keep first couple of verses straight
	if {$i > 97} {
	    append d $ch
	    continue
	}

	# +3 prevents it from degenerating too far
	# /2 makes it degenerate faster though
	if {int(rand() * ($i/2 + 3)) > 0} {
	    append d $ch
	    continue
	}

	# do something strange
	switch [expr {int(rand()*3)}] {
	    0 {
		# substitute another letter
		if {[regexp {[aeiou]} $ch]} {
		    # if vowel, substitute another
		    append d [string index "aeiou" \
			    [expr {int(5 * rand())}]]
		} elseif {[regexp {[0-9]} $ch]} {
		    # if number, substitute another
		    append d [string index "123456789" \
			    [expr {int(9 * rand())}]]
		} else {
		    # if consonant, substitute another
		    append d [string index "bcdfghjklmnpqrstvwxyz" \
			    [expr {int(21 * rand())}]]
		}
	    }
	    1 {
		# duplicate a letter
		append d $ch$ch
	    }
	    2 {
		# drop a letter
	    }
	}
    }
    return $d
}

### Mainly an implementation of Expect's "human" mode
proc boozyType {i s} {
    ### Black magic with a Weibull distribution...
    set alphaStd [expr {0.4 - ($i/333.0)}]
    set alphaEOW [expr {0.6 - ($i/333.0)}]
    set c        [expr {1/(log($i/2.0 + 1) + 0.1)}]
    set tMin     0.0
    set tMax     [expr {6.0 - $i/20.0}]

    set inWord true
    set first true
    foreach ch [split $s {}] {
	### use the end-of-word alpha at eow transitions
	if {$inWord || [string is punct $ch] || [string is space $ch]} {
	    set alpha $alphaEOW
	} else {
	    set alpha $alphaStd
	}
	set inWord [expr {!([string is punct $ch] || [string is space $ch])}]

	### Work out how long to sleep
	set t [expr {$alpha * pow(-log(rand()), $c)}]
	if {$t < $tMin} {
	    set t $tMin
	}
	if {$t > $tMax} {
	    set t $tMax
	}

	### Do the sleep, skipping only if it is the first character
	if {$first} {
	    set first false
	} else {
	    after [expr {int($t * 1000)}]
	}
	puts -nonewline $ch
    }
}
fconfigure stdout -buffering none

for {set i 99} {$i>0} {} {
    line123 $i
    incr i -1
    line4 $i

    # get bored and skip ahead
    if {$i == 92} {
	set i [expr {52+int(5*rand())}]
    }
    if {$i == 51} {
	set i [expr {12+int(5*rand())}]
    }
    if {$i == 10} {
	set i [expr {6+int(3*rand())}]
    }
}
```

