+++
title = "Execute SNUSP/Perl"
description = ""
date = 2014-06-11T17:31:04Z
aliases = []
[extra]
id = 16801
[taxonomies]
categories = []
tags = []
+++


This is an implementation of programming SNUSP, written in the perl programming language.

This implementation incorporates both the 'bloated' and  'modular' sets of ops.

Since there are several SNUSP interpreters out there, written in perl, I chose to write a compiler.

It takes a SNUSP program on the command line, or through STDIN, and produces the perl equivalent on STDOUT.

If the generated program needs to read from it's own STDIN, you must direct the compiler's output to a file, and then run perl on that file.

If the generated program does not read from stdin, you may, if you choose, redirect the compiler's output, via a pipe, to another perl process.

There is one notable difference between this implementation, and the SNUSP specification, [http://www.quirkster.com/iano/snusp/snusp-1.0-spec-wd1.pdf]. This code does not perform context switching after every single step, but only when the '?' or ',' instructions are performed.  This is an optimization, but could be construed
as a flaw, if lockstep operation is expected.  The few examples I've found of SNUSP code which create a thread work perfectly fine with my optimization in-place.

There are also several optimizations in place to reduce the size of the
compiled code, and to make the compiled output easier to read.

Here's the main program:

```Perl
#!perl
use strict;
use warnings;

print <<'COMMON';
#!perl
use strict;
use warnings;
use vars qw($row $col @st @data @threads);
COMMON

use File::Spec;
my $abs_self = File::Spec->rel2abs($0);
my ($vol, $path, undef) = File::Spec->splitpath($abs_self, 0);
my $include = File::Spec->catpath($vol, $path, "SNUSP_common.pl");
print qq[do "\Q$include\E" or die;\n];

my %commands = (
	'<' => q{ --$col; $col < 0 and die; },
	'>' => q{ ++$col },
	'+' => q{ ++$data[$row][$col] },
	'-' => q{ --$data[$row][$col] },
	'.' => q{ print chr $data[$row][$col] },
	',' => q{ goto (schedule(readchar($data[$row][$col]) ? 'NEXT' : 'REDO')) },
	# /, \, and ! are implemented in the main loop

	'?' => q{ goto (schedule($data[$row][$col] ? 'NEXT' : 'SKIP')) },

	'@' => q{ push @st, 'SKIP' },
	'#' => q{ goto (pop @st) },

	':' => q{ --$row; $row < 0 and die },
	';' => q{ ++$row },
	'%' => q{ $_ = int rand(1 + $_) for $data[$row][$col] },
	# & is implemented in the main loop.
);

tr/;{}// or s/(\s*)\z/;$1/ for values %commands;
s/^\s+|\s+\z//g for values %commands;

my %delta = (R => [1, 0], L => [-1, 0], U => [0, -1], D => [0, 1]);
my %slash = qw(R U L D);
my %backslash = qw(R D L U); 
%$_ = (%$_, reverse %$_) for \%slash, \%backslash;

chomp(my @code = <>);
my @todo;

# Pad lines shorter than the longest with spaces.
my $longest = '';
$longest |= $_ for @code;
$_ .= " " x (length($_) - length($longest)) for @code;

for my $line (0..$#code) {
	$code[$line] =~ /\$/ or next;
	@todo = "R_$-[0]_$line";
	last;
}
@todo = "R_0_0" unless @todo;

my %marked;
my %ops_used;
my $code = '';
my %done;
while( @todo ) {
	my $here = shift @todo;
	$marked{$here} ||= 0;
	$code .= ";$here:";

	my ($d, $x, $y) = split /_/, $here;
	++$done{$here};
	
	if( $y < 0 or $y > $#code or $x < 0 or $x >= length $code[$y] ) {
		$code .= qq{goto (endthread()); # $y, $x\n} if @todo;
		next;
	}

	my $c = substr $code[$y], $x, 1;
	++$ops_used{$c};
	my $delta = $delta{$d};
	$x += $delta->[0];
	$y += $delta->[1];
	my $next = join "_", $d, $x, $y;

	if( $c eq "/" or $c eq "\\" ) {
		(undef, $x, $y) = split /_/, $here;
		($c eq "/") ? ($d = $slash{$d}) : ($d = $backslash{$d});
		$delta = $delta{$d};
		$x += $delta->[0];
		$y += $delta->[1];
		$next = join "_", $d, $x, $y;
	} elsif( $c eq '&' ) {
		$code .= qq[newthread('$next');\n];
		unshift @todo, $next unless(exists $marked{$next});
		++$marked{$next};
		# With the $x += ... above, and the $x += ... below,
		# the *parent* thread skips over whatever is after the '&'
		$x += $delta->[0];
		$y += $delta->[1];
		$next = join "_", $d, $x, $y;
	} elsif( $c eq "!" ) {
		$x += $delta->[0];
		$y += $delta->[1];
		$next = join "_", $d, $x, $y;
	} elsif( my $cmd = $commands{$c} ) {
		my $skip = join "_", $d, $x + $delta->[0], $y + $delta->[1];
		++$marked{$here} if $cmd =~ s/REDO/$here/g;
		if( $cmd =~ s/SKIP/$skip/g ) {
			#warn "cmd is $cmd\n";
			#warn "Adding $skip to \@todo\n" unless exists $marked{$skip};
			#warn "Not adding $skip to \@todo\n" if exists $marked{$skip};
			unshift @todo, $skip unless exists $marked{$skip};
			++$marked{$skip};
		}
		if( $cmd =~ s/NEXT/$next/g ) {
			unshift @todo, $next unless exists $marked{$next};
			++$marked{$next};
		}
		$cmd .= "# $here '$c'";
		$code .= $cmd . "\n";
		next if $c eq '#';
	}
	if( $done{$next} ) {
		++$marked{$next};
		$code .= "goto $next;\n";
	} elsif( exists $marked{$next} ) {
		if( @todo and $todo[0] eq $next ) {
			#warn "After $here, $next\n"; # normal flow control.
		} elsif( grep $_ eq $next, @todo ) {
			#warn "After changing order, after $here, $next\n";
			# rearrange @todo, then normal flow control
			@todo = grep $_ ne $next, @todo;
			unshift @todo, $next;
		} else {
			warn "\$marked{$next} was $marked{$next} but $next wasn't in \%done or \@todo\n";
			++$marked{$next};
			unshift @todo, $next;
		}
	} else {
		unshift @todo, $next;
	}
}

# Optimize the code slightly.

my $exitcode;
if( not $ops_used{'<'} || $ops_used{'>'} ) {
	# don't need $col.
	s/\[\$col\]//g for $code, $commands{'+'}, $commands{'-'};
	$exitcode = '$data[$row]';
} elsif( not $ops_used{':'} || $ops_used{';'} ) {
	# don't need $row.
	s/\[\$row\]//g for $code, $commands{'+'}, $commands{'-'};;
	$exitcode = '$data[$col]';
} else {
	$exitcode = '$data[$row][$col]';
}

if( not $ops_used{'&'} ) {
	# goto (schedule(readchar($data[$row][$col]) ? 'NEXT' : 'REDO')) # ,
	# goto (schedule($data[$row][$col] ? 'NEXT' : 'SKIP')) # ?
	while($code =~ m/goto \(schedule\((.*?) \? '(.*?)' \: '(.*?)'\)\)/) {
		my ($start, $end, $len) = ($-[0], $+[0], $+[0]-$-[0]);
		my ($C, $T, $F) = ($1, $2, $3);
		pos($code) = $end;
		my $replace_with;
		#warn "removing goto (schedule($C ? '$T' : '$F'))";
		#warn "followed by <<<".substr($code, $end, 30).">>>";
		if( $code =~ m/\G.*\n;?$F:/gc ) {
			$replace_with = "$C and goto $T";
		} elsif( $code =~ m/\G.*\n;?$T:/gc ) {
			$replace_with = "$C or goto $F";
		} else {
			$replace_with = "$C ? goto $T : goto $F";
		}
		substr $code, $start, $len, $replace_with;
	}
	$code =~ s/\Q(endthread())\E/EXIT/g;
}

# Remove unused labels.
my $notused = join '|', grep !$marked{$_}, keys %marked;
$code =~ s/;($notused)://g;

# If a label wasn't removed, the comment doesn't need to repeat it.
my $used = join '|', grep $marked{$_}, keys %marked;
$code =~ s/(($used):.*?# )\2 /$1/g;

# Remove extraneous semicolons.
$code =~ s/;((?:\s|#.*)*);/;$1/g;

# Combine several ++foo into a single foo += count
my %combinable = qw(> + + + ; + < - : - - -);
my %or_die = qw(< 1 : 1);
for my $cmd (keys %combinable) {
	my $str = $commands{$cmd};
	$str =~ s/^\s+|\s+\z//g;
	my $op = $combinable{$cmd};
	my ($base) = $str =~ m/^..(\$\S+);/ or die;
	my $diestr = $or_die{$cmd} ? "$base >= 0 or die; " : "";
	my $inner = qr/ *\Q$str\E\s*(?:#.*)?\n/;
	$code =~ s!($inner{2,})!
		my $cnt = ($1 =~ tr/\n//);
		"$base $op= $cnt; $diestr# $cnt of '$cmd'\n"
	!xge;
}

my $LABEL = qr/[A-Z0-9_]+/;
my $WS = qr/(?:\s|#.*)/;

# Whenever there are two labels in a row, remove the first,
# and rename all instances of the first to the second.
if( 1 ) {
	while( $code =~ s/($LABEL):(?:;|$WS)+($LABEL):/$2:/ ) {
		my $rename_from	= $1;
		my $rename_to = $2;
		$code =~ s/(?<!# )\b$rename_from\b/$rename_to/g;
		$code =~ /\b$rename_from\b/ and die;
	}
}

# Whenever there's a "LABEL1:" followed by "goto LABEL2;", remove "LABEL1:"
# and replace all other instances of LABEL1 with LABEL2
if( 1 ) {
	# This will usually be AFTER_D_X_Y:; # '?'\n goto ASDF
	while( $code =~ s/($LABEL):(?:$WS|;)*goto ($LABEL);/goto $2;/ ) {
		my $rename_from	= $1;
		my $rename_to = $2;
		$code =~ s/(?<!# )\b$rename_from\b/$rename_to/g;
		delete $marked{rename_from};
	}
}

# Whenever there's a goto .*; LABEL: goto EXPR, remove
# LABEL: goto EXPR and replace all instances of 'LABEL' with EXPR
if( 1 ) {
	while( $code =~ m/goto .*\s*(($LABEL):\s*goto (.*?);.*\n)/ ) {
		my $remove = $1;
		my $label = $2;
		my $expr = $3;
		$code =~ s/\Q$remove//;
		if( $expr =~ $LABEL ) {
			$code =~ s/$label\b/$expr/g;
		} else {
			$code =~ s/'$label'|\b$label\b/$expr/g;
		}
		delete $marked{$label};
	}
}

# Whenever there's two or more gotos in a row, remove
# all except the first, as the others are unreachable.
if( 1 ) {
	# We probably optimized away the label(s) which
	# would have allowed the later gotos to be reached.
	$code =~ s/^goto ($LABEL);((?:$WS|;)*goto ($LABEL);.*)+/goto $1;/mg;
}

if( 1 ) {
	# due to inlining certain LABEL: goto EXPR things,
	# it's possible to have a no-op push then pop.
	my $remove = 'push @st, (pop @st);';
	$code =~ s/\Q$remove\E/;/g;
}

#$code =~ s/#.*//g;

print $code;
if( $ops_used{'#'} ) {
	print "STACK_UNDERFLOW: ";
	print $ops_used{'&'} ? "goto (endthread())\n" : ";\n";
}
print <<"EX";
EXIT: exit($exitcode || 0);
EX

__END__

```


The following is the file which is included by each compiled perl program.

It should be named SNUSP_common.pl and should be in the same directory as
the main program.


```Perl
#!perl
use strict;
use warnings;
# This code is included, via 'do "SNUSP_common.pl";'
# into every SNUSP program which is compiled into perl.

($row, $col, @st) = (0, 0, qw(STACK_UNDERFLOW));

sub schedule {
	my $label = shift;
	return $label unless @threads;
	push @threads, $row, $col, \@st, $label;
	($row, $col, *st, $label) = splice @threads, 0, 4;
	$label;
}

sub newthread {
	push @threads, $row, $col, [$st[0]], shift;
}

sub endthread {
	return 'EXIT' unless @threads;
	($row, $col, *st) = splice @threads, 0, 3;
	shift @threads;
}

$| = 1;

sub _readchar {
	if( @threads ) {
		my $bits = '';
		vec($bits, fileno(STDIN), 1) = 1;
		select($bits, undef, undef, 0);
		vec($bits, fileno(STDIN), 1) or return 0;
	}
	sysread(STDIN, my($c), 1) or die "sysread failed: $^E";
	$_[0] = ord($c);
	1;
}

if( -t STDIN and eval { require Term::ReadKey; 1 } ) {
	Term::ReadKey::ReadMode(1);
	my $rk = 1;
	END: { $rk and Term::ReadKey::ReadMode(0) }
	sub readk {
		my $c = Term::ReadKey::ReadKey(@threads ? -1 : 0);
		return 0 unless defined $c;
		$_[0] = ord $c;
		1;
	}
	*readchar = \&readk;
} else {
	*readchar = \&_readchar;
}

1;
__END__

```


Here's a basic Hello, World! program, which I found at:
http://www.quirkster.com/iano/snusp/snusp-js.html


```SNUSP
 \/\   /\          /\  /\
 +++   ++          ++  ++
 +++   ++  /++++\  ++  ++  /++++\
 ++\++\++  +    +  ++  ++  +    +
 ++   +++  +/+++/  ++  ++  +    +
 ++   +++  ++      ++  ++  +    +
 \/   \/\  /\+++   /\  /\  /++.+/
                           \        \
 /++++.+++++++++++++++++++++++++++  /  \
                           /\       -  -#
                           ++       .  -.
 +      /  \++++\  /++++\  ++  /---\-  -+
 +  /\  +  +    +  +    +  +.  -   .-  ->
 +  +.  +  +    +  +       +-  -   +-
 .  +>  +  +    +  +       +-  -   +-  -.
 \.+/\++/  \++++/  +    .  +\  /-.+/-  --
            \      /    \< /   \    /  \/

```


When compiled, the SNUSP code above produces the Perl code below:


```perl
#!perl
use strict;
use warnings;
use vars qw($row $col @st @data @threads);
do "C\:\\Documents\ and\ Settings\\Ben\\RosettaCode\\SNUSP_common\.pl" or die;
$data[$col] += 72; # 72 of '+'
print chr $data[$col];# L_30_6 '.'
$data[$col] += 29; # 29 of '+'
print chr $data[$col];# L_6_8 '.'
$data[$col] += 7; # 7 of '+'
print chr $data[$col];# D_1_14 '.'
print chr $data[$col];# R_2_15 '.'
$data[$col] += 3; # 3 of '+'
print chr $data[$col];# D_5_13 '.'
++$col;# D_5_14 '>'
$data[$col] += 32; # 32 of '+'
print chr $data[$col];# D_24_15 '.'
--$col; $col < 0 and die;# R_25_16 '<'
$data[$col] += 8; # 8 of '+'
print chr $data[$col];# D_28_12 '.'
$data[$col] -= 8; # 8 of '-'
print chr $data[$col];# D_35_12 '.'
$data[$col] += 3; # 3 of '+'
print chr $data[$col];# L_33_15 '.'
$data[$col] -= 6; # 6 of '-'
print chr $data[$col];# U_36_10 '.'
$data[$col] -= 8; # 8 of '-'
print chr $data[$col];# U_40_14 '.'
++$col;# U_40_12 '>'
++$data[$col];# U_40_11 '+'
print chr $data[$col];# U_40_10 '.'
goto (pop @st);# U_40_9 '#'
STACK_UNDERFLOW: ;
EXIT: exit($data[$col] || 0);

```


This code produces:
```txt
Hello, world!
```


The compiler depends on File::Spec, which has been part of the perl core since version 5.004_05 (way back in 1999).  If you have a version of perl older than that, you can get File::Spec from CPAN.

If STDIN is a terminal, the compiled program will attempt to use Term::ReadKey.

If it cannot, it will fall back to using select(), if it needs a nonblocking read.

If you don't have Term::ReadKey, you can get it from CPAN.

If you're on Windows, the multi-threaded example program won't run properly without Term::ReadKey (it'll just block), but other SNUSP programs will work just fine.

If you're on Linux, the multi-threaded example code will run until you hit enter, rather than until you hit any key.  The /bin/stty program could help with that, but Term::ReadKey works, and is vastly more portable.

The easiest way to install modules from CPAN is to use the 'cpan' program which is included with perl, and which has been since version 5.004 (released in 1997).
