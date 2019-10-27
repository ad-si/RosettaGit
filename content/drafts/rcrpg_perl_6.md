+++
title = "RCRPG/Perl 6"
description = ""
date = 2018-04-04T00:46:19Z
aliases = []
[extra]
id = 9196
[taxonomies]
categories = []
tags = []
+++

{{collection|RCRPG}}

This [[Perl 6]] version of [[RCRPG]] implements a text interface.

==Language Idioms==
This version of RCRPG demonstrates the following idioms in Perl 6.
* A simple [[input loop]]
* Subroutines (Functions)
* Nested, lexically scoped Subroutines.
* Nested data structures using list and hash references
* Function references
* Range constructors
* Whatever codes
* Infinite lazy lists
* Meta Operators
* User output
* [[Linked list|Linked lists]]

==Commands==
As with the [[RCRPG/Perl|Perl version]].

==Use==
As with the [[RCRPG/Perl|Perl version]].
==Code==
{{works with|Rakudo|2018.03}}  

```perl6
my %commands;
my %rooms;

my $room = [0,0,0];
my $stuff = [];
my $equipped = '';

my $WHITE = "\e[0m";
my $GREEN = "\e[32m";
my $BLUE  = "\e[34m";

my @random_items = < sledge ladder gold >;

my %dir_str =
    up      => 'in the ceiling',
    down    => 'in the floor',
    (map { ($_ => "to the $_") }, <north south east west>).Slip;

my %dir_vec =
    up      => [ 0, 0, 1],
    down    => [ 0, 0,-1],
    north   => [ 0, 1, 0],
    south   => [ 0,-1, 0],
    east    => [ 1, 0, 0],
    west    => [-1, 0, 0];

my %dir_rev = map { ($^a => $^b, $^b => $^a).Slip },
    < north south east west up down >;

my class Room {
    has $.stuff = (my $roll = 1.rand) > .85 ?? [@random_items.roll(2)] !! 
                      $roll > .25 ?? [@random_items.roll] !! [];
    has $.name  = "The Unnamed Room";
    has $.links = [];

    method greet() {
        say "You are in room ($room), $.name";
        if !$.links {
            say "There are no exits from this room.  Perhaps you need to make one?";
        } elsif $.links == 1 {
            say "There is an exit %dir_str{$.links[0]}.";
        } else {
            say "There are exits: {@$.links.join(', ')\
                 .subst(/','(<-[,]>+)$/, -> $/ {" and$0"})}.";
        }
        if $.stuff {
            say "There is {serial_and( @$.stuff )} here.";
        } else {
            say $BLUE, "There is nothing useful here you can take.", $WHITE;
        }
        say $GREEN, "Try 'help' for help\n", $WHITE;
        print ">";
    }
}

%rooms{"0 0 0"} = Room.new(name => 'The Start',      stuff => [<sledge>]);
%rooms{"1 1 5"} = Room.new(name => 'The Prize Room', stuff => [<gold> xx 9]);

sub serial_and(*@list) {
    my %things;
    map { %things{$_}++ }, @list;
    my $items = (map { "{%things{$_} > 1 ?? %things{$_} !! 'a'} {plural($_, %things{$_})}" },
       %things.keys).join: ', ';
    $items.subst( /','(<-[,]>+)$ /, -> $/ {" and$0"} );
}

sub plural ($item, $count) {
   return $count != 1 ?? $item~'s' !! $item
}

sub move($dir) {
    if $dir eq 'up' && !grep 'ladder', @( %rooms{$room}.stuff ) {
        say "There needs to be a ladder in the room before you can climb.";
    } elsif grep $dir, @( %rooms{$room}.links ) {
        $room = [ @$room Z+ @(%dir_vec{$dir}) ];
    } else {
        say "Your way is blocked.";
    }
}

sub command(@names, $help, $ct, $sub) {
    sub parser(*@bits) {
        if @bits[0] eq '_help' || (@bits - 1) !~~ $ct {
            say "Usage: {@names.join('|')} $help";
        } else {
            shift @bits;
            $sub.(|@bits);
        }
    }
    for @names { %commands{$_} = &parser }
}

for flat <north south east west up down> -> $dir {
    command [$dir, substr($dir,0,1)], '', 0, { move($dir) };
}

command [<attack a>], "(direction)", 1,
    -> $dir {
        if !(%dir_vec{$dir}:exists) {
            say "I don't know that direction.  Try one of: {sort keys %dir_vec}";
        }
        elsif grep $dir, @( %rooms{$room}.links ) {
            say "You swing {$equipped ?? "the $equipped" !! 'your fists'} wildly at the hole "
                ~ 'in the wall. It makes a nice breeze.';
        }
        elsif $equipped ne 'sledge' {
            say "You swing {$equipped ?? "the $equipped" !! 'your fists'} around purposfully "
                ~ 'but accomplish nothing. Perhaps you should equip a better tool?';
        }
        else {
            my $r2 = [ @$room Z+ @(%dir_vec{$dir}) ];
            my $r2o = %rooms{$r2} //= Room.new;
            say 'You bash until the surface crumbles, leaving a hole you can crawl through.';
            push @( %rooms{$room}.links ), $dir;
            push @( $r2o.links ), %dir_rev{$dir};
        }
    };

command [<inventory inv i>], "", 0,
    {
        if !$stuff {
            say "You're not carrying anything.  Ask again later.";
        } else {
            say "You have {serial_and( @$stuff )}."
        }
    };

command [<name>], "(New name of room)", 1 .. *,
    -> *@names { %rooms{$room}.name = ~@names };

command [<equip>], "(itemname)", 1,
    -> $item {
        if grep $item, @$stuff {
            $equipped = $item;
            say "You equipped your $item.  Put it to good use.";
        } else {
            say "You don't have one of those.  Try 'i' to see what you have.";
        }
    };

sub move_items(@from, @to, @filters) {
    sub ok($item) { ?grep { $_ eq 'all' || $_ eq $item }, @filters }

    my @move = grep &ok, @from;
    push @to, @move;
    @from = grep { !ok($_) }, @from;

    if $equipped && !grep $equipped, @$stuff {
        say "You dropped the item you were using.";
        $equipped = '';
    }

    @move == 0 ?? False !! @move > 1 ?? "{+@move} items" !! "1 item";
}

command [<take>], '{all|(itemname)}', 1 .. *,
    -> *@filters {
        if move_items(%rooms{$room}.stuff, $stuff, @filters) -> $things {
            say "You took $things.";
        } else {
            say "There aren't any of those here.";
        }
    };

command [<drop>], '{all|(itemname)}', 1 .. *,
    -> *@filters {
        if move_items($stuff, %rooms{$room}.stuff, @filters) -> $things {
            say "You dropped $things.";
        } else {
            say "You don't have any of those.";
        }
    };

command [<help>], "(command name) ... But you apparently discovered that.",
    0..1, -> $command? {
        if defined $command {
            %commands{$command}.("_help");
        } else {
            say "Valid commands are: ", [ sort keys %commands ];
        }
    };

command [<alias>], "(existing command name) (additional name for command)",
    2, -> $c1, $c2 {
        if %commands{$c2}:exists {
            say "Can't redefine an existing command!";
        } else {
            %commands{$c2} = %commands{$c1};
        }
    };

command [<exit quit>], "(leave Muddy)", 0,
    {
       print 'The Muddy walls dim. ';
       say "You glance down as {serial_and( @$stuff )} {plural('clatter', +!(@$stuff == 1))}"
         ~ " to the floor and {plural('fade', +!(@$stuff == 1))} away." if @$stuff;
       say 'It is as if you are awakening from a strange (and extremly low resolution) dream...';
       exit;
    };

say "Welcome to Muddy.  It's kinda like a MUD, but it lacks multiple players...";
%rooms{$room}.greet;

while my $in = $*IN.get {
    my @args = $in.words;
    if @args && %commands{lc @args[0]} -> $fun {
        $fun(|@args);
    } else {
        say "That didn't make any sense.  Try 'help'";
    }
    %rooms{$room}.greet;
}
```

