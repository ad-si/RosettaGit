+++
title = "RCRPG/Perl"
description = ""
date = 2010-06-07T15:17:00Z
aliases = []
[extra]
id = 3041
[taxonomies]
categories = []
tags = []
+++

{{collection|RCRPG}}
This [[Perl]] version of [[:Category:RCRPG|RCRPG]] implements a text interface.

==Language Idioms==
(Add to this list if you see anything of interest not mentioned.)

This version of RCRPG demonstrates the following idioms in Perl.
* A simple [[input loop]]
* Subroutines ([[Functions]])
* [[Nested]] data structures using list and hash [[Pointer|references]]
* Function references
* [[User output]]
* [[Linked list|Linked lists]] (The rooms are linked by keeping a list of keys to connected rooms in each room. This adds a lookup step, but each key could be thought of a pointer in the classic sense of a linked list.)

==Commands==

Here is a description of all of the commands:


```txt
north
south
east
west
up
down
```


Move in the direction specified. The player won’t be able to move if there isn’t an exit in that direction.


```txt
attack (direction)
```

Attack in the direction specified. (Equip the sledge first.)


```txt
drop {all|(item name)}
```

Drop the item specified. Or drop everything the player is carrying.


```txt
take {all|(item name)}
```

Take the item specified. Or take everything in the room.


```txt
inventory
```

Display everything the player is carrying.


```txt
name (name)
```

The player can rename the room with this command.


```txt
equip (item name)
```

Equip the item in question.


```txt
alias (existing command name) (new command name)
```

Give an existsing command an alias. Starting aliases are n,s,e,w,u,d. i and inv are the same as inventory, a is the same as attack.

==Use==

None of the rooms are connected. In fact, at the start of the game, only two rooms really exist; the Start and the Prize room. There’s a third room, but it exists outside of the 3D coordinate space, and it’s used to initialize each room the player creates by moving around.

To move out of the start room, the player needs to break a hole in a wall, ceiling or floor.  The sledge is needed to do this.

There are three kinds of items in the different rooms: sledges, gold and ladders. A sledge is needed to gain access to rooms the player doesn’t have access to. A ladder is needed to go up a level. 

Every room, except the prize room, will randomly have gold, a sledge or a ladder. The player will have to leave the ladder behind when they go up a level, so they may have to hunt around a bit for a new ladder.

When the player finds a ladder, they can either move up a level from that room, or they can take the ladder and drop it in a room that’s more convenient.

The player can make their way to the Prize Room at (1,1,5), but there's nothing useful there; just gold.

==Code==


```perl
#!/usr/bin/perl -w
use strict;
use Data::Dumper;
use Term::ANSIColor(qw(:constants));

my @randomitems = ( 'sledge', 'ladder', 'gold' );

my %directions = ( 'up' => [0,0,1],
		   'down' => [0,0,-1],
		   'north' => [0,1,0],
		   'south' => [0,-1,0],
		   'east' => [1,0,0],
		   'west' => [-1,0,0] );

my %rdirections;

# Generate reverse lookup for directions.
foreach my $key ( keys %directions )
{
	my $dir_arr_ref = $directions{$key};

	if( $key eq 'up' )
	{
		$key = 'in the ceiling';
	}
	elsif( $key eq 'down' )
	{
		$key = 'in the floor';
	}
	else
	{
		$key = "to the $key";
	}
	
	$rdirections{join ',', @$dir_arr_ref} = $key;
}

&alias( \%directions, 'up', 'u' );
&alias( \%directions, 'down', 'd' );
&alias( \%directions, 'north', 'n' );
&alias( \%directions, 'south', 's' );
&alias( \%directions, 'east', 'e' );
&alias( \%directions, 'west', 'w' );

my %rooms = ( 'new' => { 'name' => 'The Unnamed Room',
			   'items' => ['random'],
			   'links' => [] },
	      '0,0,0' => { 'name' => 'The Start',
			   'items' => ['sledge'],
			   'links' => [] },
	      '1,1,5' => { 'name' => 'The Prize Room',
	      		   'items' => ['gold','gold','gold','gold','gold','gold','gold','gold','gold'],
			   'links' => [] } );

sub cmd_north( $ );
sub cmd_south( $ );
sub cmd_east( $ );
sub cmd_west( $ );
sub cmd_up( $ );
sub cmd_down( $ );
sub cmd_attack( $$ );
sub cmd_name( $@ );
sub cmd_take( $@ );
sub cmd_equip( $$ );
sub cmd_drop( $@ );
sub cmd_inventory( $ );
sub cmd_alias( $$$ );
sub cmd_help( $$ );

my %commands = ( 'north' => \&cmd_north,
		 'south' => \&cmd_south,
		 'east' => \&cmd_east,
		 'west' => \&cmd_west,
		 'up' => \&cmd_up,
		 'down' => \&cmd_down,
		 'attack' => \&cmd_attack,
		 'name' => \&cmd_name,
		 'take' => \&cmd_take,
		 'equip' => \&cmd_equip,
		 'drop' => \&cmd_drop,
		 'inventory' => \&cmd_inventory,
		 'help' => \&cmd_help,
		 'alias' => \&cmd_alias);



&alias( \%commands, 'north', 'n' );
&alias( \%commands, 'south', 's' );
&alias( \%commands, 'east', 'e' );
&alias( \%commands, 'west', 'w' );
&alias( \%commands, 'up', 'u' );
&alias( \%commands, 'down', 'd' );
&alias( \%commands, 'attack', 'a' );
&alias( \%commands, 'inventory', 'i' );
&alias( \%commands, 'inventory', 'inv' );

&mainloop();

print RESET;

# &alias( 'look', 'ls' )
# makes 'ls' behave like 'look'
sub alias( $$$ )
{
	my $hash = shift;
	my $existing = shift;
	my $new = shift;
	$hash->{$new} = $hash->{$existing};
}


sub mainloop
{
	my $room = $rooms{'0,0,0'};
	my %self = ( 'room' => '0,0,0',
		     'stuff' => [],
		     'equipped' => \'');

	print WHITE, "Welcome to Muddy.  It's kinda like a MUD, but it lacks multiple players...\n";
	&greet( \%self, $room );
	
	while ( <STDIN> ) {
		print RED;
		my @args = split / /;
		my $command = lc shift @args;
		chomp $command;
		
		if ( defined $commands{$command} )
		{
			$commands{$command}->(\%self, @args);
		}
		else
		{
			print "That didn't make any sense.  Try 'help'\n";
		}

		$room = $rooms{$self{'room'}};

		&greet ( \%self, $room );
	}
}

sub greet( $$ )
{
	my $self = shift;
	my $room = shift;
	print WHITE, "You are in room (" . $self->{'room'} . "), " . $room->{'name'} . " \n";

	{
		my $linksref = $room->{'links'};
		my @dir_strings = map {$rdirections{$_} } @$linksref;
		my $count = scalar @$linksref;
		
		if ( 0 == $count )
		{
			print "There are no exits from this room. Perhaps you need to make one?\n";
		}
		elsif ( 1 == $count )
		{
			print "There is an exit " . $dir_strings[0] . "\n";
		}
		elsif ( 2 == $count )
		{
			print "There are exits " . $dir_strings[0] . " and " . $dir_strings[1] . "\n";
		}
		else
		{
			print "There are exits ";

			for( my $i = 0; $i < ( $count - 2 ); ++$i )
			{
				print $dir_strings[$i] . ", ";
			}

			print $dir_strings[$count - 2] . " and " . $dir_strings[$count - 1] . ".\n";
		}
	}
	{
		my $it = $room->{'items'};
		if ( 0 < scalar @$it )
		{
			my $count = scalar @$it;
			print BLUE, "There is a ";
			if ( 1 == $count )
			{
				print $it->[0];	
			}
			elsif ( 2 == $count )
			{
				print $it->[0] . " and a " . $it->[1];
			}
			else
			{
				for( my $i = 0; $i< ($count - 2); ++$i)
				{
					print $it->[$i] . ", ";
				}

				print $it->[$count - 2] . " and a " . $it->[$count - 1];
			}

			print " here.\n";
		}
		else
		{
			print BLUE, "There is nothing useful here you can take.\n";
		}
	}

	print GREEN, "Try 'help' for help\n";

	print WHITE, ">";
}

# Create the current room if it doesn't already exist.
sub roomcheck( $$ )
{
	my $self = shift;
	my $addr = shift;
	unless ( exists $rooms{$addr} )
	{
		my $rr = $rooms{'new'};
		my %newroom = %$rr;

		# $newroom{'links'} is still a reference to the original room.  We need to replace it with a new array ref
		my $lr = $newroom{'links'};
		my @links = @$lr;
		$newroom{'links'} = \@links;
		
		$rooms{$addr} = \%newroom;
	}


	# Is there a random?  process it.
	my $before = $rooms{$addr}->{'items'};
	my @after = grep !/^random$/, @$before;
	my $randoms = scalar @$before - scalar @after;

	for( my $i = 0; $i < $randoms; ++$i )
	{
		my $itemidx = int( rand( scalar @randomitems ) );
		push @after, ($randomitems[$itemidx]);
	}

	# Update the room item list.
	$rooms{$addr}->{'items'} = \@after;
}

sub move( $ )
{
	my $self = shift;

	if( $self eq 'help')
	{
		print "Usage: (move) (direction)\n";
		return;
	}

	my $dirref = shift;
	my ($xshift, $yshift, $zshift) = @$dirref;

	# What room are we in?
	my ($x, $y, $z) = split /,/, $self->{'room'};

	# Where are we going?
	$x += $xshift;
	$y += $yshift;
	$z += $zshift;

	my $offset = "$xshift,$yshift,$zshift";

	my $addr = "$x,$y,$z";

	my $links = $rooms{$self->{'room'}}->{'links'};

	my @found = grep /^$offset$/, @$links;

	if ( 0 != scalar @found )
	{
		$self->{'room'} = $addr;
		&roomcheck( $self, $addr );
	}
	else
	{
		print "Your way is blocked.\n";
	}

}

sub cmd_north( $ )
{
	&move( shift, $directions{'north'}  );
}

sub cmd_south( $ )
{
	&move( shift, $directions{'south'} );
}

sub cmd_east( $ )
{
	&move( shift, $directions{'east'} );
}

sub cmd_west( $ )
{
	&move( shift, $directions{'west'} );
}

sub cmd_up( $ )
{
	my $self = shift;
	# Is there a ladder in the room?
	my $itemspresent = $rooms{$self->{'room'}}->{'items'};
	unless( grep /ladder/, @$itemspresent )
	{
		print "There needs to be a ladder in the room before you can climb.\n";
		return;
	}
	&move( $self, $directions{'up'} );
}

sub cmd_down( $ )
{
	&move( shift, $directions{'down'} );
}

sub link( $$ )
{
	my $self = shift;
	my $vector = shift;
	

	# Get our vector string
	my $vecstring = join ',', @$vector;

	# Get the room we're linking from.
	my $fromaddr = $self->{'room'};
	my @frompos = split /,/, $fromaddr;

	# Get the room we're linking to.
	my @topos = ($vector->[0] + $frompos[0], $vector->[1] + $frompos[1], $vector->[2] + $frompos[2]);
	my $toaddr = join ',', @topos;
	&roomcheck( $self, $toaddr );

	# Get the reverse of our vector
	my $rvector = [0 - $vector->[0], 0 - $vector->[1], 0 - $vector->[2]];

	# Get our reverse vector string
	my $rvecstring = join ',', @$rvector;

	# Link our source room to our destination room.
	my $fromlinks = $rooms{$fromaddr}->{'links'};
	push @$fromlinks, ($vecstring);

	# Link our destination room to our source room.
	my $tolinks = $rooms{$toaddr}->{'links'};
	push @$tolinks, ($rvecstring);
}

sub cmd_attack( $$ )
{
	my $self = shift;

	if( $self eq 'help' )
	{
		print "Usage: attack (direction)\n";
		return;
	}

	my $direction = shift;

	if( defined $direction )
	{
		chomp $direction;
		unless( defined $directions{$direction})
		{
			print "I don't know that direction.  Try:";
			foreach my $exdir ( keys %directions )
			{
				print " $exdir"
			}
			print "\n";
			return;
		}
	}
	else
	{
		print "Invalid syntax.  Try 'attack (direction)'\n";
		return;
	}

	if ( $self->{'equipped'} eq 'sledge' )
	{
		my $dirarray = $directions{$direction};
		my $dirstr = join ',', @$dirarray;
		my $roomlinks = $rooms{$self->{'room'}}->{'links'};
		if ( grep /^$dirstr$/, @$roomlinks )
		{
			print "You swing your sledge wildly.\n";
		}
		else
		{
			print "You bash until the surface crumbles, leaving a hole you can crawl through.\n";
			&link($self, $dirarray);
		}
	}
	else
	{
		print "You accomplish nothing.\n";
	}
}

sub cmd_name( $@ )
{
	my $self = shift;
	my @args = @_;

	if( ( $self eq 'help' )
	 || ( 0 == scalar @args ) )
	{
		print "Usage: name (New name of room)\n";
		return;
	}

	my $newname = join ' ', @args;

	my $room = $rooms{$self->{'room'}};
	$room->{'name'} = $newname;
}

sub move_items( $$@ )
{
	my $fromref = shift; my $from = $$fromref;
	my $toref = shift; my $to = $$toref;
	my @args = @_;

	my $firstitem = lc shift @args;
	chomp $firstitem;
	my $moveall = 0;
	if ( $firstitem eq 'all' )
	{
		$moveall = 1;
	}
	else
	{
		unshift @args, ($firstitem);
	}

	my $type = lc shift @args;
	chomp $type;
	my @remaining;
	my $moved = 0;


	my $item;
	while( $item = pop @$from )
	#for( my $item = shift @$from; 0 != scalar @$from; $item = shift @$from)
	#foreach my $item ( @$from )
	{
		if ( $item =~ /$type/ )
		{
			++$moved;
			push @$to, ($item);
			last unless ( 1 == $moveall );
		}
		else
		{
			push @remaining, ($item);
		}
	}

	push @remaining, @$from;

	$$fromref = \@remaining;

	return $moved;
}

sub cmd_take( $@ )
{
	my $self = shift;
	my @args = @_;

	if( ( $self eq 'help' )
	 || ( 0 == scalar @args ) )
	{
		print "Usage: take {all|(itemname)}\n";
		return;
	}

	my $from = $rooms{$self->{'room'}}->{'items'};
	my $to = $self->{'stuff'};

	my $moved = &move_items( \$from, \$to, @args );

	$rooms{$self->{'room'}}->{'items'} = $from;

	if ( 0 == $moved )
	{
		print "There aren't any of those here.\n";
	}
	elsif ( 1 == $moved )
	{
		print "You took 1 item.\n";
	}
	else
	{
		print "You took $moved items.\n";
	}

}

sub cmd_drop ( $@ )
{
	my $self = shift;
	my @args = @_;

	if( ( $self eq 'help' )
	 || ( 0 == scalar @args ) )
	{
		print "Usage: drop {all|(itemname)}\n";
		return;
	}


	my $from = $self->{'stuff'};
	my $to = $rooms{$self->{'room'}}->{'items'};

	my $moved = &move_items( \$from, \$to, @args );

	$self->{'stuff'} = $from;


	if ( 0 == $moved )
	{
		print "You don't have any of those.\n";
	}
	elsif ( 1 == $moved )
	{
		print "You dropped 1 item.\n";
	}
	else
	{
		print "You dropped $moved items.\n";
	}

	# Check to see if we dropped our equipped item.
	unless ( 0 == $moved )
	{
		my $dropped = shift @args;
		chomp $dropped;
		$dropped = shift @args if ( $dropped eq 'all' );

		my $equipped = $self->{'equipped'};

		unless( grep /$equipped/, @$from )
		{
			print "You dropped the item you were using.\n";
		}
	}

}

sub cmd_equip( $$ )
{
	my $self = shift;
	my $toequip = shift;

	if( ( $self eq 'help' )
	 || ( ! defined $toequip ) )
	{
		print "Usage: equip (itemname)\n";
		return;
	}

	chomp $toequip;

	my $inventory = $self->{'stuff'};

	my $invsize = scalar @$inventory;
	my $found = 0;

	# Find it
	for( my $itemidx = 0; $itemidx < $invsize; ++$itemidx )
	{
		# Equip it.
		if( $inventory->[$itemidx] eq $toequip )
		{
			$self->{'equipped'} = $$inventory[$itemidx];
			$found = 1;
			print "You equipped your $toequip.  Put it to good use.\n";
			last;
		}
	}
	unless ( 1 == $found )
	{
		print "You don't have one of those.  Try 'i' to see what you have.\n";
	}
}

sub cmd_inventory( $ )
{
	my $self = shift;

	if( $self eq 'help' )
	{
		print "Usage: inventory\n";
		return;
	}

	my $inventory = $self->{'stuff'};

	if ( 0 < scalar @$inventory )
	{
		my $count = scalar @$inventory;
		print "You have a ";
		if ( 1 == $count )
		{
			print $inventory->[0];	
		}
		elsif ( 2 == $count )
		{
			print $inventory->[0] . " and a " . $inventory->[1];
		}
		else
		{
			for( my $i = 0; $i< ($count - 2); ++$i)
			{
				print $inventory->[$i] . ", ";
			}

			print $inventory->[$count - 2] . " and a " . $inventory->[$count - 1];
		}

		print ".\n";
	}
	else
	{
		print "You're not carrying anything.  Ask again later.\n";
	}

}

sub cmd_alias( $$$ )
{
	my $self = shift;
	my $existing = shift;
	my $new = shift;

	if( ( $self eq 'help' )
	 || ( ! defined $new ) )
	{
		print "Usage: alias (existing command name) (additional name for command)\n";
		return;
	}

	chomp $existing;
	chomp $new;

	if( defined $commands{$new} )
	{
		print "Can't redefine an existing command!\n";
		return;
	}
	&alias( \%commands, $existing, $new );
}

sub cmd_help( $$ )
{
	my $self = shift;

	if( $self eq 'help' )
	{
		print "Usage: help (command name) ... But you apparently discovered that.\n";
		return;
	}
	my $command = shift;
	
	if( defined $command )
	{
		chomp $command;
		if( defined $commands{$command} )
		{
			$commands{$command}->('help', ($command));
		}
	}
	else
	{
		print "Valid commands are: ";
		foreach my $command ( keys %commands )
		{
			print "$command ";
		}
		print "\n";
	}
}
```

