+++
title = "Stem-and-leaf plot/Data generator"
description = ""
date = 2009-12-14T19:34:06Z
aliases = []
[extra]
id = 5220
[taxonomies]
categories = []
tags = []
+++

Written to generate data sets for the [[Stem-and-leaf plot]] task. Statistics aren't my field or area of significant expertise, so some of the variable names might represent my homebrew understanding more than official terminology. --[[User:Short Circuit|Michael Mol]] 19:34, 14 December 2009 (UTC)


```perl
#!/usr/bin/perl
use strict;

# The shuffle is taken from http://rosettacode.org/wiki/Knuth_shuffle
sub shuffle
 {my @a = @_;
  foreach my $n (1 .. $#a)
     {my $k = int rand $n + 1;
      $k == $n or @a[$k, $n] = @a[$n, $k];}
  return @a;}

# The greater your pointcount to your width, the more sparse your result.
sub genhump
{
	my ($offset, $width, $pointcount, $slope) = @_;

	my $diecount = $slope;
	my $range = $width / $diecount;

	my @ret;
	foreach (1 .. $pointcount)
	{
		my $point = $offset;
		foreach (1 .. $diecount)
		{
			$point += rand($range * 10000) / 10000;
		}

		push @ret, int($point);
	}

	return @ret;
}

my @list = (&genhump(8, 3, 5, 3),
            &genhump(12, 3, 5, 6));

my @shuffled = &shuffle(@list);

print "@shuffled\n";
```

