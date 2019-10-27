+++
title = "Formal power series/Perl"
description = ""
date = 2013-10-03T02:48:55Z
aliases = []
[extra]
id = 16417
[taxonomies]
categories = []
tags = []
+++

{{Collection|Formal power series}}

This is an alternative Perl implementation of a Formal Power Series.
Unlike the one one the main Formal Power Series page, the one *does*
implement lazy lists which look and are accessed exactly like normal
perl arrays.

It's also a bit more object oriented, and probably more extensible.


```perl

use strict;
use warnings;
package FPS;
use Math::BigRat;

sub TIEARRAY {
   my $class = shift;
   return bless {@_}, $class;
}

sub FETCH {
   my ($self, $i) = @_;
   my $terms = ( $self->{terms} ||= [] );
   return $terms->[$i]
      if $i <= $#$terms and defined $terms->[$i];
   return 0 if __PACKAGE__ eq ref $self;
   $terms->[$i] = $self->coeff($i);
}

sub new {
   my $class = shift;
   tie my(@self), $class, @_;
   bless \@self, $class;
}

sub new_from_list {
   my ($class, @terms) = @_;
   tie my(@self), $class, terms => \@terms;
   bless \@self, $class;
}

sub _become {
   my ($self, $other, @terms) = @_;
   for( $self, $other ) {
      $_ = tied @$_ if UNIVERSAL::isa($_, 'ARRAY');
   }
   %$self = %$other;
   $self->{terms} = \@terms if @terms;
   bless $self, ref $other;
}

sub _fixargs {
   my ($x, $y, $swap) = @_;
   $y = FPS->new_from_list($y)
      unless UNIVERSAL::isa($y, 'FPS');
   $swap ? ($y, $x) : ($x, $y);
}

for my $onearg (qw(invert differentiate integrate)) {
   my $uc = ucfirst $onearg;
   my $pack = 'FPS::'.$uc;
   no strict 'refs';
   *{"FPS::$onearg"} = sub {
      $pack->new( x => shift );
   };
   @{$pack.'::ISA'} = 'FPS';
}

for my $twoarg (qw(sum difference product)) {
   my $uc = ucfirst $twoarg;
   my $pack = 'FPS::'.$uc;
   no strict 'refs';
   *{"FPS::$twoarg"} = sub {
      my ($x, $y) = &_fixargs;
      $pack->new( x => $x, y => $y );
   };
   @{$pack.'::ISA'} = 'FPS';
}

sub FPS::Invert::coeff {
   my ($self, $i) = @_;
   my $x = $self->{x};
   if( $i == 0 ) {
      my $x0 = $x->[0];
      die "Cannot invert power series with zero constant term."
         unless $x0;
      (Math::BigRat->new(1) / $x0);
   } else {
      my $y = $self->{terms};
      my $sum = 0;
      for my $j (1 .. $i) {
         $sum += $x->[$j] * $y->[$i - $j];
      }
      -$y->[0] * $sum;
   }
}

sub FPS::Differentiate::coeff {
   my ($self, $i) = @_;
   ($i + 1) * $self->{x}[$i];
}

sub FPS::Integrate::coeff {
   my ($self, $i) = @_;
   return 0 if $i == 0;
   ($self->{x}[$i-1]) / Math::BigRat->new($i);
}

sub FPS::Sum::coeff {
   my ($self, $i) = @_;
   $self->{x}[$i] + $self->{y}[$i];
}

sub FPS::Difference::coeff {
   my ($self, $i) = @_;
   $self->{x}[$i] - $self->{y}[$i];
}

sub FPS::Product::coeff {
   my ($self, $i) = @_;
   my ($x, $y) = @{$self}{'x','y'};
   my $sum = 0;
   $sum += $x->[$_] * $y->[$i-$_] for 0..$i;
   $sum;
}

sub quotient {
   my ($x, $y) = &_fixargs;
   $x * FPS::Invert->new(x => $y);
}

sub stringify {
   my $self = shift;
   my $str = $self->[0];
   for my $i (1..10) {
      my $c = $self->[$i];
      next unless $c;
      $str .= ($c < 0) ? (" - " . (-$c)) : (" + ".$c);
      $str .= " x^$i";
   }
   $str;
};

use overload qw(
   + sum - difference * product / quotient
   "" stringify);

my $sin = FPS->new;
my $cos = 1 - $sin->integrate;
$sin->_become( $cos->integrate );
my $tan = $sin / $cos;
my $exp = FPS->new();
$exp->_become( $exp->integrate, 1 );

print "sin(x) ~= $sin\n";
print "cos(x) ~= $cos\n";
print "tan(x) ~= $tan\n";
print "exp(x) ~= $exp\n";

print "sin^2 + cos^2 - 1 = ", $sin*$sin + $cos*$cos - 1, "\n";
print "cos*cos - cos^2 = ", $cos*$cos - $cos**2, "\n";

print "1/1/cos(x) - cos(x) = ", $cos->invert->invert - $cos, "\n";
print "1 - exp(x) / exp(x) = ", 1 - $exp / $exp, "\n";

__END__

```

{{out}}

```txt

sin(x) ~= 0 + 1 x^1 - 1/6 x^3 + 1/120 x^5 - 1/5040 x^7 + 1/362880 x^9
cos(x) ~= 1 - 1/2 x^2 + 1/24 x^4 - 1/720 x^6 + 1/40320 x^8 - 1/3628800 x^10
tan(x) ~= 0 + 1 x^1 + 1/3 x^3 + 2/15 x^5 + 17/315 x^7 + 62/2835 x^9
exp(x) ~= 1 + 1 x^1 + 1/2 x^2 + 1/6 x^3 + 1/24 x^4 + 1/120 x^5 + 1/720 x^6 + 1/5040 x^7 + 1/40320 x^8 + 1/362880 x^9 + 1
/3628800 x^10
sin^2 + cos^2 - 1 = 0
1/1/cos(x) - cos(x) = 0
1 - exp(x) / exp(x) = 0

```

