+++
title = "Robots"
description = ""
date = 2019-04-23T18:50:02Z
aliases = []
[extra]
id = 20796
[taxonomies]
categories = ["task"]
tags = []
+++

The task is to implement a clone of Ken Arnold's turn-based game [[wp:Robots_(1984_video_game)|Robots]].

Simple game where its only objective is to escape from a number of robots, which have been programmed to kill the player.






## C++


See [[Robots/C++]].


## Go


See [[Robots/Go]].


## Java


See [[Robots/Java]].


## Kotlin


See [[Robots/Kotlin]].


## Perl 6

The bots single-mindedly chase you, taking the shortest path, ignoring obstacles. Use arrow keys to navigate your character(╂) around the board. Avoid bots(☗) and hazards(☢). "Kill" bots by causing them to crash into hazards or other bots. A dead bot creates another hazard. If you eliminate all of the bots on the board, another wave will spawn in random positions. If you touch a hazard or are touched by a bot, you die(†).

```perl6
use Term::termios;

constant $saved   = Term::termios.new(fd => 1).getattr;
constant $termios = Term::termios.new(fd => 1).getattr;
# raw mode interferes with carriage returns, so
# set flags needed to emulate it manually
$termios.unset_iflags(<BRKINT ICRNL ISTRIP IXON>);
$termios.unset_lflags(< ECHO ICANON IEXTEN ISIG>);
$termios.setattr(:DRAIN);

# reset terminal to original settings and clean up on exit
END { $saved.setattr(:NOW);  print "\e[?25h\n" }

print "\e[?25l"; # hide cursor

my %dir = (
   "\e[A" => 'up',
   "\e[B" => 'down',
   "\e[C" => 'right',
   "\e[D" => 'left',
);

my $x = 100; # nominal "board" width
my $y = 40;  # nominal "board" height

my $human = "\e[0;92m╂\e[0m"; # various
my $robot = "\e[0;91m☗\e[0m"; # entity
my $block = "\e[0;93m☢\e[0m"; # sprite
my $dead  = "\e[1;37m†\e[0m"; # characters
my $wall  = "\e[1;96m█\e[0m";
my $blank = ' ';

my $numbots = 10; # number of bots in each round

# blank playing field
my @scr = flat $wall xx $x, ($wall, $blank xx $x - 2, $wall) xx $y - 2, $wall xx $x;

# put player on board
my $me;
loop {
    $me = ($x+2 .. ($x - 1 ) * $y).roll;
    last if @scr[$me] eq $blank;
}
@scr[$me] = $human;

# Put an assortment of hazards on board
for ^20 {
    my $s = (^$x*$y).pick;
    if @scr[$s] eq $blank { @scr[$s] = $block } else { redo }
}

my $info  = 0;
my $score = 0;

newbots(); # populate board with a fresh wave of bots

loop {
    print "\e[H\e[J";
    print "\e[H";
    print join "\n", @scr.rotor($x)».join;
    print "\nSurvived " , $info , ' bots';

    # Read up to 4 bytes from keyboard buffer.
    # Page navigation keys are 3-4 bytes each.
    # Specifically, arrow keys are 3.
    my $key = $*IN.read(4).decode;

    move %dir{$key} if so %dir{$key};
    movebots();
    last if $key eq 'q'; # (q)uit
}

proto sub move (|) {*};

multi move ('up') {
    if @scr[$me - $x] ne $wall {
        expire() if @scr[$me - $x] ne $blank;
        @scr[$me] = $blank;
        $me = $me - $x;
        @scr[$me] = $human;
    }
}
multi move ('down') {
    if @scr[$me + $x] ne $wall {
        expire() if @scr[$me + $x] ne $blank;
        @scr[$me] = $blank;
        $me = $me + $x;
        @scr[$me] = $human;
    }
}
multi move ('left') {
   if @scr[$me - 1] ne $wall {
        expire() if @scr[$me - 1] ne $blank;
        @scr[$me] = $blank;
        $me = $me - 1;
        @scr[$me] = $human;
    }
}

multi move ('right') {
    if @scr[$me + 1] ne $wall {
        expire() if @scr[$me + 1] ne $blank;
        @scr[$me] = $blank;
        $me = $me + 1;
        @scr[$me] = $human;
    }
}

sub newbots {
    for ^$numbots {
        my $s = (^$x*$y).pick;
        if @scr[$s] eq $blank {
            @scr[$s] = $robot;
        } else {
            redo
        }
    }
}

sub movebots {
    my $mx = $me % $x;
    my $my = $me div $x;
    my @bots = @scr.grep: * eq $robot, :k;
    for @bots -> $b {
        my $bx = $b % $x;
        my $by = $b div $x ;
        if ($mx - $bx).abs < ($my - $by).abs {
            $by += ($my - $by) < 0 ?? -1 !! 1;
        } else {
            $bx += ($mx - $bx) < 0 ?? -1 !! 1;
        }
        my $n = $by * $x + $bx;
        if @scr[$n] eq $robot {
            @scr[$b] = @scr[$n] = $block;
        } elsif @scr[$n] eq $block {
            @scr[$b] = $block;
        } elsif $n == $me {
            expire()
        } else {
            @scr[$b] = $blank;
            @scr[$n] = $robot;
        }
    }
    unless +@bots > 0 {
        newbots();
        $score += $numbots;
    }
    $info = $score + $numbots - @scr.grep: * eq $robot;
}

sub expire {
    @scr[$me] = $dead;
    print "\e[H\e[J";
    print "\e[H";
    print join "\n", @scr.rotor($x)».join;
    print "\nSurvived " , $info , ' bots, but succumbed in the end.';
    exit
}
```

```txt
████████████████████████████████████████████████████████████████████████████████████████████████████
█                                                                                                  █
█                                               ☗                                                  █
█                                 ☢☢                                                               █
█                             †    ☢       ☢                                  ☢                    █
█          ☢                  ☢☢☢☢☢☢                                                               █
█       ☢☢☢☢☢☢☢                ☢☢☢☢☢☢☢                                                             █
█               ☢☢☢☢           ☢  ☢☢                              ☢☢☢                              █
█             ☢☢☢☢☢               ☢☢                                                               █
█            ☢☢☢☢☢☢☢☢             ☢☢☢                        ☢☢☢                                   █
█             ☢ ☢☢☢☢                ☢                                                              █
█              ☢☢☢                                                                                 █
█                                              ☗                                                   █
█               ☢☢☢ ☢                                                                              █
█               ☢☢☢☢☢☢                                                                             █
█                ☢☢☢☢☢☢☢☢                                                                          █
█               ☢☢☢☢☢☢ ☢☢                                                                          █
█               ☢☢☢☢☢☢☢☢                   ☗                                                       █
█               ☢☢☢☢☢☢☢      ☢                                 ☢                                   █
█                 ☢☢ ☢       ☢☢                                                                    █
█                  ☢ ☢       ☢         ☢☢                                                          █
█                 ☢☢                   ☢                                                           █
█           ☢                          ☢                                                           █
█           ☢                                                                                      █
█           ☢                                                                                      █
█                                                                                                  █
█                                                                        ☢                      ☢  █
█                                                                                                  █
█                                                                                                  █
█                                                                                                  █
█                                                                                             ☢    █
█                                                                             ☢             ☢      █
█                                                        ☢                                         █
█                                                                                                  █
█                                                                                                  █
█                                           ☢☢                                                     █
█                                           ☢                                                      █
█                                                                                                  █
█            ☢☢                                                                                    █
████████████████████████████████████████████████████████████████████████████████████████████████████
Survived 117 bots, but succumbed in the end.
```



## Phix


See [[Robots/Phix]].
