+++
title = "Go Fish/Tcl"
description = ""
date = 2010-11-18T10:41:10Z
aliases = []
[extra]
id = 5232
[taxonomies]
categories = []
tags = []
+++

{{collection|Go Fish}}
[[implementation of task::Go Fish| ]]
{{works with|Tcl|8.6}}

```tcl
package require Tcl 8.6

# How to sort ranks
proc suitorder {a b} {
    set a1 [lsearch -exact {2 3 4 5 6 7 8 9 10 J Q K A} $a]
    set b1 [lsearch -exact {2 3 4 5 6 7 8 9 10 J Q K A} $b]
    expr {$a1 - $b1}
}

# Class to manage the deck of cards
oo::class create Deck {
    variable deck

    constructor {{packs 1}} {
	set deck [list]
	for {set p 0} {$p < $packs} {incr p} {
	    foreach suit {C D H S} {
		foreach pip {2 3 4 5 6 7 8 9 10 J Q K A} {
		    lappend deck [list $pip $suit]
		}
	    }
	}
    }

    method shuffle {} {
	# Shuffle in-place
	for {set i [llength $deck]} {[incr i -1] > 0} {} {
	    set n [expr {int($i * rand())}]
	    set card [lindex $deck $n]
	    lset deck $n [lindex $deck $i]
	    lset deck $i $card
	}
    }

    method deal {num} {
	incr num -1
	set hand [lrange $deck 0 $num]
	set deck [lreplace $deck 0 $num]
	return $hand
    }

    method renderCard {card} {
	string map {C \u2663 D \u2662 H \u2661 S \u2660 " " {}} $card
    }
    method print {hand} {
	set prev {}
	foreach card [my sortHand $hand] {
	    if {[lindex $card 0] ne $prev} {
		if {$prev ne ""} {puts ""}
		puts -nonewline \t[my renderCard $card]
	    } else {
		puts -nonewline " [my renderCard $card]"
	    }
	    set prev [lindex $card 0]
	}
	puts ""
    }

    method sortHand {hand} {
	lsort -index 0 -command suitorder [lsort -index 1 $hand]
    }

    proc empty {} {
	return [expr {[llength $deck] == 0}]
    }
}

# "Abstract" class of all players; implements core game mechanics
# from a player's perspective
oo::class create GoFishPlayer {
    variable theDeck hand opponent
    constructor {deck otherPlayer} {
	set theDeck $deck
	set hand [$deck deal 9]
	set opponent $otherPlayer
    }

    method ask {rank} {
	set response {}
	set new {}
	foreach card $hand {
	    if {[lindex $card 0] eq $rank} {
		lappend response $card
	    } else {
		lappend new $card
	    }
	}
	set hand [expr {[llength $new] ? $new : [$theDeck deal 1]}]
	return $response
    }
    method AskFor {rank} {
	set withoutOne 1
	foreach card $hand {
	    if {[lindex $card 0] eq $rank} {
		set withoutOne 0
		break
	    }
	}
	if {$withoutOne} {
	    error "do not have any $rank cards"
	}

	set response [$opponent ask $rank]
	if {[llength $response]} {
	    lappend hand {*}$response
	} else {
	    my GoFish
	    lappend hand {*}[$theDeck deal 1]
	}

	return [llength $response]
    }

    method MakeBooks {} {
	foreach rank {2 3 4 5 6 7 8 9 10 J Q K A} {
	    set n {}
	    set idx -1
	    foreach card $hand {
		incr idx
		if {[lindex $card 0] eq $rank} {
		    lappend n $idx
		}
	    }
	    if {[llength $n] == 4} {
		announceBook $rank [self]
		foreach idx [lreverse $n] {
		    set hand [lreplace $hand $idx $idx]
		}
	    }
	}
	if {[llength $hand] == 0} {
	    set hand [$theDeck deal 1]
	}
    }

    method makeAPlay {} {
	set msg ""
	while {$::books(total) < 13} {
	    set rank [my SelectRank $msg]
	    try {
		if {![my AskFor $rank]} {
		    my YieldToOpponent
		    break
		}
	    } on error msg {
		# Back round the loop with an error message
	    } on ok {} {
		my MakeBooks
		set msg ""
	    }
	}
	my MakeBooks
    }

    method GoFish {} {
	# Do nothing with this notification by default
    }
    method madeBook {rank who} {
	# Do nothing with this notification by default
    }
    method YieldToOpponent {} {
	# Do nothing with this notification by default
    }
    method SelectRank {msg} {
	error "not implemented"
    }
}

# A player that works by communicating with a human
oo::class create HumanPlayer {
    superclass GoFishPlayer
    variable theDeck hand opponent
    method madeBook {rank who} {
	if {$who eq [self]} {set who "You"}
	puts "$who made a book of $rank"
    }
    method YieldToOpponent {} {
	puts "Now your opponent's turn"
    }
    method AskFor {rank} {
	set count [next $rank]
	puts "You asked for ${rank}s and received $count cards"
	if {$count > 0} {
	    puts "You may ask again!"
	}
	return $count
    }
    method ask {rank} {
	set cards [next $rank]
	puts "[namespace tail $opponent] asked for $rank cards, and got [llength $cards] of them"
	return $cards
    }
    method GoFish {} {
	puts "You were told to \"Go Fish!\""
    }

    method SelectRank {msg} {
	if {$msg ne ""} {
	    puts "ERROR: $msg"
	}
	set I [namespace tail [self]]
	puts "You are ${I}: Your cards are:"
	$theDeck print $hand
	while 1 {
	    puts -nonewline "What rank to ask for? "
	    flush stdout
	    set rank [string toupper [gets stdin]]
	    if {$rank in {2 3 4 5 6 7 8 9 10 J Q K A}} {
		return $rank
	    }
	    puts "Rank must be 2, 3, 4, 5, 6, 7, 8, 9, 10, J, Q, K, or A"
	    puts "You must also have at least one of them already"
	}
    }
}

# A computer player that tracks what it's opponent must have
oo::class create ThinkingPlayer {
    superclass GoFishPlayer
    variable state hand
    constructor args {
	next {*}$args
	foreach rank {2 3 4 5 6 7 8 9 10 J Q K A} {
	    set state($rank) unknown
	}
    }

    method madeBook {rank who} {
	set state($rank) booked
    }
    method AskFor {rank} {
	set count [next $rank]
	set state($rank) none
	if {$count == 0} {
	    foreach rank {2 3 4 5 6 7 8 9 10 J Q K A} {
		if {$state($rank) eq "none"} {
		    set state($rank) unknown
		}
	    }
	}
	return $count
    }
    method ask {rank} {
	set cards [next $rank]
	set state($rank) some
	return $cards
    }

    method GoFish {} {
	puts "You told your opponent to \"Go Fish!\""
    }

    method SelectRank {ignored} {
	# If we know they have the cards and we can grab them, do so!
	# It's a safe move since we get to go again.
	foreach {rank s} [array get state] {
	    if {$s eq "some" && [lsearch -exact -index 0 $hand $rank] >= 0} {
		return $rank
	    }
	}
	# Only unsafe moves remain; pick a random non-stupid one
	foreach c $hand {
	    set rank [lindex $c 0]
	    if {$state($rank) ne "none"} {
		set r([lindex $c 0]) .
	    }
	}
	if {[array size r]} {
	    return [lindex [array names r] [expr {int([array size r]*rand())}]]
	}
	# No good choices; oh well...
	return [lindex $hand [expr {int([llength $hand]*rand())}] 0]
    }
}

# How announcements of a book being made are done
proc announceBook {rank who} {
    global books

    A madeBook $rank $who
    B madeBook $rank $who
    lappend books($who) $rank
    incr books(total)
}

# Stitch things together to make a whole game.
Deck create deck
deck shuffle
array set books {total 0 ::A {} ::B {}}
HumanPlayer create A deck B
ThinkingPlayer create B deck A
while {$books(total) < 13} {
    A makeAPlay
    if {$books(total) < 13} {
	B makeAPlay
    }
}
puts "You have [llength $books(::A)]: [lsort -command suitorder $books(::A)]"
puts "The computer has [llength $books(::B)]: [lsort -command suitorder $books(::B)]"
if {[llength $books(::A)] > [llength $books(::B)]} {
    puts "You win!"
} else {
    puts "The computer won!"
}
```



### Notes on the Mechanical Player

The computer player (implemented as a subclass of the generic player class) has four states for ''each'' rank (aside from basic overall state like what cards it is holding, which every player has to have):
;unknown
: Don't know if the opponent has any cards in that rank.
;none
: Opponent has no cards there; I took them away.
;some
: Opponent has cards there; they tried to get them off me and haven't booked them yet.
;booked
: Someone has booked the rank.
It prefers to take cards away from the opponent if it can and tries hard to avoid moves it knows will fail. It never makes illegal moves. It does not bother to look at the number of booked suits, though as that is global state it ''could''. No player or the deck has any method to reveal (within the game world, not the display) what hand of cards it actually has.
