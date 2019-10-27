+++
title = "Hunt The Wumpus/Perl 6"
description = ""
date = 2017-10-21T19:21:04Z
aliases = []
[extra]
id = 19792
[taxonomies]
categories = []
tags = []
+++

{{collection|Hunt_The_Wumpus}}
This is a fairly faithful translation of the code found in [http://www.atariarchives.org/bcc1/showpage.php?page=247 The Best of Creative Computing Volume 1 (published 1976)]. 

The text, prompts and game play are very close the the original. 

There are a few minor commands available in the main loop not in the original: Press I to re-view the instructions, Press Q to quit. 
It will accept lower case as well as upper case commands. 

The originals instructions explicitly say hitting yourself with an arrow will cause a lose condition, the original published code does not however; this version does.

{{works with|Rakudo|2015-11-20}}

```perl6
#Based on code from http://www.atariarchives.org/bcc1/showpage.php?page=247 copyright Gregory Yob

my $intro = q:to/END/;
     ATTENTION ALL WUMPUS LOVERS!!!
     THERE ARE NOW TWO ADDITIONS TO THE WUMPUS FAMILY
     OF PROGRAMS.
    
      WUMP2:  SOME DIFFERENT CAVE ARRANGEMENTS
      WUMP3:  DIFFERENT HAZARDS
   END

my $instructions = q:to/END/;
     WELCOME TO 'HUNT THE WUMPUS'
      THE WUMPUS LIVES IN A CAVE OF 20 ROOMS. EACH ROOM
     HAS 3 TUNNELS LEADING TO OTHER ROOMS. (LOOK AT A
     DODECAHEDRON TO SEE HOW THIS WORKS-IF YOU DON'T KNOW
     WHAT A DODECAHEDRON IS, ASK SOMEONE)
    
         HAZARDS:
     BOTTOMLESS PITS - TWO ROOMS HAVE BOTTOMLESS PITS IN THEM
         IF YOU GO THERE, YOU FALL INTO THE PIT (& LOSE!)
     SUPER BATS - TWO OTHER ROOMS HAVE SUPER BATS. IF YOU
         GO THERE, A BAT GRABS YOU AND TAKES YOU TO SOME OTHER
         ROOM AT RANDOM. (WHICH MIGHT BE TROUBLESOME)
    
         WUMPUS:
     THE WUMPUS IS NOT BOTHERED BY THE HAZARDS (HE HAS SUCKER
     FEET AND IS TOO BIG FOR A BAT TO LIFT).  USUALLY
     HE IS ASLEEP. TWO THINGS WAKE HIM UP: YOUR ENTERING
     HIS ROOM OR YOUR SHOOTING AN ARROW.
         IF THE WUMPUS WAKES, HE MOVES (P=.75) ONE ROOM
     OR STAYS STILL (P=.25). AFTER THAT, IF HE IS WHERE YOU
     ARE, HE EATS YOU UP (& YOU LOSE!)
    
         YOU:
     EACH TURN YOU MAY MOVE OR SHOOT A CROOKED ARROW
       MOVING: YOU CAN GO ONE ROOM (THRU ONE TUNNEL)
       ARROWS: YOU HAVE 5 ARROWS. YOU LOSE WHEN YOU RUN OUT.
       EACH ARROW CAN GO FROM 1 TO 5 ROOMS. YOU AIM BY TELLING
       THE COMPUTER THE ROOM#S YOU WANT THE ARROW TO GO TO.
       IF THE ARROW CAN'T GO THAT WAY (IE NO TUNNEL) IT MOVES
       AT RANDOM TO THE NEXT ROOM.
         IF THE ARROW HITS THE WUMPUS, YOU WIN.
         IF THE ARROW HITS YOU, YOU LOSE.
    
        WARNINGS:
        WHEN YOU ARE ONE ROOM AWAY FROM WUMPUS OR HAZARD,
        THE COMPUTER SAYS:
     WUMPUS-  'I SMELL A WUMPUS'
     BAT   -  'BATS NEARBY'
     PIT   -  'I FEEL A DRAFT'
    
    END

constant DEBUG = False;

my $prompt = prompt 'INSTRUCTIONS (Y-N) ';
put $instructions if $prompt ~~ m:i/y/;
put $intro;

my @cave = ( (), # connecting tunnels to each room
    ( 2, 5, 8), ( 1, 3, 9), ( 2, 4,12), ( 3, 5,14), ( 1, 4, 6),
    ( 5, 7,15), ( 6, 8,17), ( 1, 7, 9), ( 8,10,18), ( 2, 9,11),
    (10,12,19), ( 3,11,13), (12,14,20), ( 4,13,15), ( 6,14,16),
    (15,17,20), ( 7,16,18), ( 9,17,19), (11,18,20), (13,16,19)
  );

my %locations.=&init;
my %save = %locations;

my $arrows = 5;
my $condition;

sub MAIN () {
    say "HUNT THE WUMPUS";
    # game loop;
    repeat {
        locations-and-warnings();
        my $choice = prompt 'MOVE OR SHOOT? ';
        given $choice {
            when m:i/M/ { move()  }
            when m:i/S/ { shoot() }
            when m:i/I/ { put $instructions }
            when m:i/Q/ { exit }
        }
        hazards();
    } until $condition;

    my $replay = prompt "\nSAME SET-UP (Y-N)";
    if $replay ~~ m:i/y/ {
        %locations = %save;
    } else {
        %locations.=&init;
        %save = %locations
    }
    $condition = 0;
    $arrows = 5;
    MAIN();
}

sub move() {
    my @nearby = @cave[%locations<YOU>].flat;
    my $choice;
    repeat {
        $choice = prompt "WHERE TO? ";
        if $choice ne any @nearby {
            say "NOT POSSIBLE -";
            locations-and-warnings();
            $choice = Any;
        }
    } until $choice;
    %locations<YOU> = +$choice;
}

sub shoot () { 
    my $num;
    repeat { 
        $num = prompt "NO. OF ROOMS(1-5) ";
        $num ~~ s:g/\D//;
    } until 0 < $num < 6;
    my @rooms;
    while +$num {
        my $room = prompt "ROOM #";
        if @rooms > 1 {
            if +$room == @rooms[*-2] {
                say "ARROWS AREN'T THAT CROOKED - TRY ANOTHER ROOM";
                next;
            }
        }
        @rooms.push(+$room);
        $num -= 1;
    }
    my $arrow = %locations<YOU>;
    for @rooms -> $r {
        my $room = $r;
        unless @cave[$arrow].grep($room) {
           $room = @cave[$arrow].flat.roll;
           if DEBUG {
               say "Arrow can't get to $r from here, travel to $room instead";
           }
        }
        if $room == %locations<YOU> {
            say "OUCH! ARROW GOT YOU!";
            lose();
            return;
        } elsif $room == %locations<WUMPUS> {
            say "AHA! YOU GOT THE WUMPUS!";
            win();
            return;
        }
        $arrow = $room;
    }
    say "MISSED";
    move-wumpus();
    $arrows -= 1;
    lose unless $arrows;
}

sub move-wumpus () {
    my %l = @cave[%locations<WUMPUS>].flat X=> '';
    %l{$_} :delete for %locations.values;
    %l{%locations<WUMPUS>} = '';
    %locations<WUMPUS> = %l.keys.roll;
    if %locations<WUMPUS> == %locations<YOU> {
        say "TSK TSK TSK - WUMPUS GOT YOU!";
        lose();
    }
}

sub lose () {
    say "HA HA HA - YOU LOSE!";
    $condition = -1;
}

sub win () {
    say "HEE HEE HEE - THE WUMPUS'LL GETCHA NEXT TIME!!";
    $condition = 1;
}

sub locations-and-warnings() {
    my @nearby = @cave[%locations<YOU>].flat;
    say "I SMELL A WUMPUS!" if %locations<WUMPUS> == any @nearby;
    say "I FEEL A DRAFT" if %locations<PIT1>|%locations<PIT2> == any @nearby;
    say "BATS NEARBY!"  if %locations<BAT1>|%locations<BAT2> == any @nearby;
    say "YOU ARE IN ROOM %locations<YOU>";
    say "TUNNELS LEAD TO {@nearby}";
    say '';
    if DEBUG { dd %locations }
}

sub hazards () {
    if %locations<YOU> eq %locations<WUMPUS> {
        say "...OOPS! BUMPED A WUMPUS!";
        move-wumpus();
    }  elsif %locations<YOU> eq %locations<PIT1> | %locations<PIT2> {
        say "YYYIIIIEEEE . . . FELL IN PIT";
        lose();
    } elsif %locations<YOU> eq %locations<BAT1> | %locations<BAT2> {
        say "ZAP--SUPER BAT SNATCH! ELSEWHEREVILLE FOR YOU!";
        my %l = 1..20 X=> '';
        %l{$_} :delete for %locations.values;
        %locations<YOU> = %l.keys.roll;
    }
}

sub init (%loc) {
    %loc{<YOU WUMPUS PIT1 PIT2 BAT1 BAT2>} = (1..20).pick(*);
    %loc;
}
```

Sample Output:

```txt
INSTRUCTIONS (Y-N) N
  ATTENTION ALL WUMPUS LOVERS!!!
  THERE ARE NOW TWO ADDITIONS TO THE WUMPUS FAMILY
  OF PROGRAMS.
 
   WUMP2:  SOME DIFFERENT CAVE ARRANGEMENTS
   WUMP3:  DIFFERENT HAZARDS

HUNT THE WUMPUS
I FEEL A DRAFT
YOU ARE IN ROOM 6
TUNNELS LEAD TO 5 7 15

MOVE OR SHOOT? m
WHERE TO? 7
BATS NEARBY!
YOU ARE IN ROOM 7
TUNNELS LEAD TO 6 8 17

MOVE OR SHOOT? m
WHERE TO? 17
ZAP--SUPER BAT SNATCH! ELSEWHEREVILLE FOR YOU!
I SMELL A WUMPUS!
YOU ARE IN ROOM 3
TUNNELS LEAD TO 2 4 12

MOVE OR SHOOT? s
NO. OF ROOMS(1-5) 4
ROOM #2
ROOM #1
ROOM #5
ROOM #4
AHA! YOU GOT THE WUMPUS!
HEE HEE HEE - THE WUMPUS'LL GETCHA NEXT TIME!!

SAME SET-UP (Y-N)n
HUNT THE WUMPUS
I SMELL A WUMPUS!
YOU ARE IN ROOM 11
TUNNELS LEAD TO 10 12 19

MOVE OR SHOOT? q

```

