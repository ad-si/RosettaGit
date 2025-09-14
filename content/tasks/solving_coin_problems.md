+++
title = "Solving coin problems"
description = ""
date = 2015-02-23T17:18:53Z
aliases = []
[extra]
id = 13504
[taxonomies]
categories = ["task"]
tags = []
languages = [
  "perl",
]
+++

In 1964, Daniel G. Bobrow created the STUDENT AI program in order to solve the types of word problems found in high school algebra books. You can read Bobrow's 1964 Ph.D. thesis, ''[http://dspace.mit.edu/handle/1721.1/6903 Natural Language Input for a Computer Problem Solving System]''. The program consists of 3 main pieces:
# A pattern matcher that reads the english input,
# rules to translate english into equations, and
# an algebraic equation solver.

In chapter 7 of his book, ''[http://norvig.com/paip.html Paradigms of Artificial Intelligence Programming: Case Studies in Common Lisp]'', Peter Norvig lays out the STUDENT program and then challenges his readers as follows:

:"''Exercise 7.8 [d]'' Find a mathematically oriented domain that is sufficiently limited so that STUDENT can solve problems in it. The chemistry of solutions (calculating pH concentrations) might be an example. Write the necessary *student-rules*, and test the resulting program." (PAIP, p. 236)

There are several types of word problems commonly encountered in high school algebra, for example coin problems, age problems, distance problems, mixture problems, finance problems, lever problems, and work problems.

For this task, let's focus specifically on coin problems. Example: "If a person has three times as many quarters as dimes and the total amount of money is $5.95, find the number of quarters and dimes." (Bluman, Allan G. ''Math word problems demystified''. 2005. The McGraw-Hill Companies Inc. p. 112, problem 1.)

Coin problems can all pretty much be schematized as follows:
# There are <math>k</math> types of valuable things: <math>1, 2, \ldots, i, \ldots, k</math>
# There are <math>n_i</math> instances of the <math>i</math>th type of valuable thing.
# The value of the <math>i</math>th type of thing is <math>v_i</math>
# The total number of things is:
#: <math>N = \sum^k_{i=1} n_i</math>
# The total value of all the things is:
#: <math>V = \sum^k_{i=1} n_i \times v_i</math>
A typical coin problem wants us to find the <math>n_i</math>, given some of the other information from the schema.

The task is to write an AI program, inspired by Bobrow's STUDENT program and Norvig's challenge in PAIP, capable of solving the kinds of coin problems found in high school algebra.

The program should take coin problems written in plain english and output the solutions. The solutions needn't be output in English.


## Perl

The following code reads a file containing coin problems line-by-line. It echoes comment lines starting with a hashmark and blank lines. Remaining lines are interpreted as coin problems, one per line.

The program works by matching the english input against patterns for the parts of the coin schema (a)-(e) in the task description. Patterns are expressed as perl regular expressions. It then translates these parameters into a MAXIMA program. MAXIMA is a free, open-source computer algebra system (CAS). It executes the MAXIMA script, extracts the solution, and prints it. By using perl regexes to read the input and MAXIMA to solve the resulting equations, the program may be kept fairly short.

This program has been tested with 28 coin problems (shown below). Here is the program:

```perl

# coin.pl
# Description: Solve math word problems involving coins.
# Usage: perl -CDSA coin.pl coin-problems.txt > output.txt
# Algorithm: NLP processor loosely inspired by Bobrow (1964) transforms
# english language coin problems into solvable equations that are
# piped into the Maxima computer algebra system.
# Input: File must be UTF-8 text file. One coin problem per line in english.
# Blank lines and comment lines beginning with hash mark ``#'' are OK.
# Output: STDOUT contains original problem, transformed equations and solutions.

use strict;
use utf8;
use List::Util qw(sum);
use List::MoreUtils qw(uniq);

our $first = 0;
our %nums = (
    zero      => 0,	one	=> 1,	two	=> 2,	three	=> 3,
    four      => 4,	five	=> 5,	six	=> 6,	seven	=> 7,
    eight     => 8,	nine	=> 9,	ten	=> 10,	eleven	=> 11,
    twelve    => 12,	thirteen => 13,	fourteen => 14,	fifteen	=> 15,
    sixteen   => 16,	seventeen => 17, eighteen => 18, nineteen => 19,
    twenty    => 20,	thirty	=> 30,	forty	=> 40,	fifty	=> 50,
    sixty     => 60,	seventy	=> 70,	eighty	=> 80,	ninety	=> 90,
    hundred   => 100,	thousand => 1_000, million => 1_000_000,
    billion   => 1_000_000_000,         trillion => 1_000_000_000_000,
    # My ActiveState Win32 Perl uses e-notation after 999_999_999_999_999
    quadrillion => 1e+015,              quintillion =>  1e+018);

# Groupings for thousands, millions, ..., quintillions
our $groups = qr/\d{4}|\d{7}|\d{10}|\d{13}|1e\+015|1e\+018/;

# Numeral or e-notation
our $num = qr/\d+|\d+e\+\d+/;

our $float = qr/(?:(?:[1-9][0-9]*\.?[0-9]*)|(?:0?\.[0-9]+))(?:[Ee][+-]?[0-9]+)?/;
our $count = 0;
our $total = 0;
our @words = ();
our @eqns = ();
our @vars = ();
our @types = ();

sub add_type {
  my ($type,$value) = @_;
  push @vars, "v_$type: $value";
  push @types, $type;
}

while (<>) {
  chomp;                  # chop trailing newline
  my $orig = $_;
  @words = ();
  @eqns = ();
  @vars = ();
  @types = ();
  $count = 0;
  $total = 0;

  s/^(ï»¿|﻿)// if !$first++;	# skip utf8 control seq e.g., "ï»¿" that starts file
  next if /^\s*$/;		# skip blank lines
  # echo comment lines
  if( /^\s*#.*$/ ) {
    print $_, "\n";
    next;
  }
  s/-/ /g;                # convert hyphens to spaces
  s/\s\s+/ /g;            # remove duplicate whitespace, convert ws to space
  s/ $//g;                # remove trailing blank
  s/^ //g;                # remove leading blank
  $_ = lc($_);            # convert to lower case
  # tokenize sentence boundaries
  s/([\.\?\!]) / $1\n/g;
  s/([\.\?\!])$/ $1\n/g;
  # tokenize other punctuation and symbols
  s/\$(.)/\$ $1/g;              # prefix
  s/(.)([\;\:\%',¢])/$1 $2/g;   # suffix
  s/\btwice\b/two times/g;
  
  # Fractions
  s/half.dollar(s?)/half_dollar/g;
  s/\bone half\b/0.5/g;
  s/\bhalf\b/0.5/g;

  # Remove noise words
  s/\b(the|a|to|of|i|is|that|it|on|you|this|for|but|with|are|have|be|at|or|was|so|if|out|not|he|she|they|has|do|does)\b\s*//g;

  # Convert English number-names to numbers
  foreach my $key (keys %nums) { s/\b$key\b/$nums{$key}/eg; }

  s/(\d) , (\d)/$1 $2/g;
  s/(\d) and (\d)/$1 $2/g;

  s/\b(\d) 100 (\d\d) (\d) (${groups})\b/($1 * 100 + $2 + $3) * $4/eg;

  s/\b(\d) 100 (\d\d) (${groups})\b/($1 * 100 + $2) * $3/eg;
  s/\b(\d) 100 (\d) (${groups})\b/($1 * 100 + $2) * $3/eg;
  s/\b(\d) 100 (${groups})\b/$1 * $2 * 100/eg;

  s/\b100 (\d\d) (\d) (${groups})\b/(100 + $1 + $2) * $3/eg;
  s/\b100 (\d\d) (${groups})\b/(100 + $1) * $2/eg;
  s/\b100 (\d) (${groups})\b/(100 + $1) * $2/eg;
  s/\b100 (${groups})\b/$1 * 100/eg;

  s/\b(\d\d) (\d) (${groups})\b/($1 + $2) * $3/eg;
  s/\b(\d{1,2}) (${groups})\b/$1 * $2/eg;

  s/\b(\d\d) (\d) 100\b/($1 + $2) * 100/eg;
  s/\b(\d{1,2}) 100\b/$1 * 100/eg;

  # anomolous cases: nineteen eighty-four and twenty thirteen
  s/\b(\d{2}) (\d{2})\b/$1 * 100 + $2/eg;

  s/((?:${num} )*${num})/sum(split(" ",$1))/eg;

  s/dollar coin/dollar_coin/g;
  s/quarters/quarter/g;
  s/dimes/dime/g;
  s/nickels/nickel/g;
  s/pennies/penny/g;
  s/dollars/dollar/g;
  s/coins/coin/g;
  s/bills/bill/g;
  s/(\d+) dollar\b/\$ $1/g;

  # Rules for coin problems
  # Rule triggers are just regexes

  add_type("dollar_coin",100) if /dollar_coin/;
  add_type("half_dollar",50) if /half_dollar/;
  add_type("quarter",25) if /quarter/;
  add_type("dime",10) if /dime/;
  add_type("nickel",5) if /nickel/;
  add_type("penny",1) if /penny/;

  while(/(${float}) (?:times )?as many \$ (\d+) bill as \$ (\d+) bill/g) {
    push @eqns, "n_$2 = $1 * n_$3";
  }

  while(/(${float}) (?:times )?as many (\w+) as (\w+)/g) {
    push @eqns, "n_$2 = $1 * n_$3";
  }

  while(/(\d+) more (\w+) than (\w+)/g) {
    push @eqns, "n_$2 = n_$3 + $1";
  }

  while(/(\d+) less (\w+) than (\w+)/g) {
    push @eqns, "n_$2 = n_$3 - $1";
  }

  if(/same number (\w+) , (\w+) (?:, )?and (\w+)/){
    push @eqns, "n_$1 = n_$2";
    push @eqns, "n_$2 = n_$3";
  } elsif(/same number (\w+) and (\w+)/){
    push @eqns, "n_$1 = n_$2";
  }

  if(/(\d+) (?:\w+ )*?consists/
      or /(\d+) (?:\w+ )*?,?consisting/
      or /total (\d+)(?! ¢)/
      or /(?<!\$ )(\d+) coin/
      or /[^\$] (\d+) bill/
      or /number [^\?\!\.0-9]*?(\d+)/) {
    $count = $1;
    push @vars, "count: $count";
  }

  if(/total (?:\w+ )*?\$ (${float})/
    or /valu(?:e|ing) \$ (${float})/
    or /\$ (${float}) ((bill|coin) )?in/) {
    $total = 100 * $1;
    push @vars, "total: $total";
  }

  if(/total (?:\w+ )*?(${float}) ¢/) {
    $total = $1;
    push @vars, "total: $total";
  }

  # Rules for stamp problems
  s/stamps/stamp/g;
  while(/(\d+) ¢ stamp/g) { add_type("$1_stamp", $1); }
  if(/cost (?:\w+ )*?\$ (${float}) (?!each)/) { $total = 100 * $1; push @vars, "total: $total"; }

  # Rules for unusual types
  while(/(\w+) cost \$ (${float}) each/g) { add_type($1, 100 * $2); }
  if(/made \$ (${float})/) { $total = 100 * $1; push @vars, "total: $total"; }

  # Rules for bill problems
  while(/\$ (\d+) bill/g) { add_type($1, 100 * $1); }
  while(/(\d+) times as many \$ (\d+) bill as \$ (\d+) bill/g) { push @eqns, "n_$2 = $1 * n_$3"; }
  while(/(\d+) more \$ (\d+) bill than \$ (\d+) bill/g) { push @eqns, "n_$2 = n_$3 + $1"; }
  while(/(\d+) less \$ (\d+) bill than \$ (\d+) bill/g) { push @eqns, "n_$2 = n_$3 - $1"; }
  if(/\$ (${float}) in bill/) { $total = 100 * $1; push @vars, "total: $total"; }

  # Rules for greeting card example
  if(/consisting (.+?)costing \$ (${float}) each and (.+?)costing \$ (${float}) each/) {
    my ($t1,$v1,$t2,$v2) = ($1,$2,$3,$4);
    $t1 =~ s/ $//; $t1 =~ s/ /_/g;
    $t2 =~ s/ $//; $t2 =~ s/ /_/g;
    add_type($t1,100*$v1);
    add_type($t2,100*$v2);
  }

  # Rules for notebook paper example
  if(/sells ([^\$]+)\$ (${float}) (\w+) and ([^\$]+)\$ (${float}) (\w+)/) {
    my ($t1,$v1,$u1,$t2,$v2,$u2) = ($1,$2,$3,$4,$5,$6);
    $t1 =~ s/ $//; $t1 =~ s/ /_/g;
    $t2 =~ s/ $//; $t2 =~ s/ /_/g;
    add_type($t1,100*$v1);
    add_type($t2,100*$v2);
    if($u1 eq $u2){
      if(/purchased (\d+) ${u1}(?:s|es)? (?:\w+ )*?and paid \$ (${float})/){
        $count = $1; push @vars, "count: $count"; 
        $total = 100*$2; push @vars, "total: $total"; 
      }
    }
  }

  # Rules ALL coin problems obey...
  # Rule no 1: Total is sum over all coin types of coin value times coin number
  # i.e., total = dot product of values and quantities
  my $dot = join(" + ", map {"n_$_ * v_$_"} uniq @types);
  if($total && scalar @types){ push @eqns, "total = $dot"; }

  # Rule no 2: Count of all coins is sum of counts of each coin type
  my $trace = join(" + ", map {"n_$_"} uniq @types);
  if($count && scalar @types){ push @eqns, "count = $trace"; }

  print "original input: $orig\n";

  # Prepare MAXIMA batch file
  my $maxima_vars = join("\$\n", uniq @vars);
  my $maxima_eqns = "[". join(", ", @eqns) . "]";
  my $maxima_find = "[". join(", ", map {"n_$_"} @types) . "]";
  my $maxima_script = "${maxima_vars}\$\nsolve(${maxima_eqns}, ${maxima_find});\n";
  print $maxima_script;
  if(scalar @eqns && scalar @vars) {
    my $temp = time() . "_" . rand() . ".max";
    open(TEMP, ">$temp") || die "Couldn't open temp file: $!\n";
    print TEMP $maxima_script;
    close TEMP;
    open(MAXIMA, "maxima -q -b $temp |") || die "Couldn't open maxima: $!\n";
    while(<MAXIMA>) {
      # filter out everything but output line with the solution
      if(/\(\%o\d+\)\s+\[\[([^\]]+)\]\]/) {
        print "solution: $1\n";
      }
    }
    close MAXIMA;
    unlink $temp;
  } else {
    print "Couldn't deduce enough information to formulate equations.\n"
  }
  print "\n\n";
}

```

The following is an example '''input''' file:

```txt

# Bluman, Allan G. Math word problems demystified. 2005. The McGraw-Hill Companies Inc. p. 112, problem 1
If a person has three times as many quarters as dimes and the total amount of money is $5.95, find the number of quarters and dimes.

# Ibid., p. 112, problem 2
A pile of 18 coins consists of pennies and nickels. If the total amount of the coins is 38¢, find the number of pennies and nickels.

# Ibid., p. 112, problem 3
A small child has 6 more quarters than nickels. If the total amount of coins is $3.00, find the number of nickels and quarters the child has.

# Ibid., p. 112, problem 4
A child's bank contains 32 coins consisting of nickels and quarters. If the total amount of money is $3.80, find the number of nickels and quarters in the bank.

# Ibid., p. 112, problem 5
A person has twice as many dimes as she has pennies and three more nickels than pennies. If the total amount of the coins is $1.97, find the numbers of each type of coin the person has.

# Ibid., p. 112, problem 6
In a bank, there are three times as many quarters as half dollars and 6 more dimes than half dollars. If the total amount of the money in the bank is $4.65, find the number of each type of coin in the bank.

# Ibid., p. 112, problem 7
A person bought 12 stamps consisting of 37¢ stamps and 23¢ stamps. If the cost of the stamps is $3.74, find the number of each type of the stamps purchased.

# Ibid., p. 112, problem 8
A dairy store sold a total of 80 ice cream sandwiches and ice cream bars. If the sandwiches cost $0.69 each and the bars cost $0.75 each and the store made $58.08, find the number of each sold.

# Ibid., p. 112, problem 9
An office supply store sells college-ruled notebook paper for $1.59 a ream and wide-ruled notebook paper for $2.29 a ream. If a student purchased 9 reams of notebook paper and paid $15.71, how many reams of each type of paper did the student purchase?

# Ibid., p. 112, problem 10
A clerk is given $75 in bills to put in a cash drawer at the start of a workday. There are twice as many $1 bills as $5 bills and one less $10 bill than $5 bills. How many of each type of bill are there?

# Ibid., p. 109
A person has 8 coins consisting of quarters and dimes. If the total amount of this change is $1.25, how many of each kind of coin are there?

# Ibid., p. 110
A person has 3 times as many dimes as he has nickels and 5 more pennies than nickels. If the total amount of these coins is $1.13, how many of each kind of coin does he have?

# Ibid., p. 111
A person bought ten greeting cards consisting of birthday cards costing $1.50 each and anniversary cards costing $2.00 each. If the total cost of the cards was $17.00, find the number of each kind of card the person bought.

# Ibid., p. 119, problem 8
A person has 9 more dimes than nickels. If the total amount of money is $1.20, find the number of dimes the person has.

# Ibid., p. 120, problem 9
A person has 20 bills consisting of $1 bills and $2 bills. If the total amount of money the person has is $35, find the number of $2 bills the person has.

# Ibid., p. 120, problem 10
A bank contains 8 more pennies than nickels and 3 more dimes than nickels. If the total amount of money in the bank is $3.10, find the number of dimes in the bank.

# More test problems from around the web...

# Source: http://www.purplemath.com/modules/coinprob.htm
# Soln: 12 quarters, 14 dollar coins
Your uncle walks in, jingling the coins in his pocket. He grins at you and tells you that you can have all the coins if you can figure out how many of each kind of coin he is carrying. You're not too interested until he tells you that he's been collecting those gold-tone one-dollar coins. The twenty-six coins in his pocket are all dollars and quarters, and they add up to seventeen dollars in value. How many of each coin does he have?

# Ibid.
# Soln: Then there are six quarters, and I can work backwards to figure out that there are 9 dimes and 18 nickels.
A collection of 33 coins, consisting of nickels, dimes, and quarters, has a value of $3.30. If there are three times as many nickels as quarters, and one-half as many dimes as nickels, how many coins of each kind are there?

# Ibid.
# Soln. There are nine of each type of coin in the wallet.
A wallet contains the same number of pennies, nickels, and dimes. The coins total $1.44. How many of each type of coin does the wallet contain?

# Source: http://www.algebralab.org/Word/Word.aspx?file=Algebra_CoinProblems.xml
# Soln: Ken has 17 nickels and 8 dimes.
Suppose Ken has 25 coins in nickels and dimes only and has a total of $1.65. How many of each coin does he have?

# Ibid.
# Let's Practice
# Question #1
# Note: The original question had an inconsistency in it,
# namely "Terry has 7 more" should be "Terry has 2 more..."
# Soln: Terry has 18 dimes and 20 quarters.
Terry has 2 more quarters than dimes and has a total of $6.80. The number of quarters and dimes is 38. How many quarters and dimes does Terry have?

# Ibid.
# Question #2
# Soln: There are 2 ten-dollar bills, 8 one-dollar bills, and 3 five-dollar bills.
In my wallet, I have one-dollar bills, five-dollar bills, and ten-dollar bills. The total amount in my wallet is $43. I have four times as many one-dollar bills as ten-dollar bills. All together, there are 13 bills in my wallet. How many of each bill do I have?

# Ibid.
# Try These
# Question #1
Marsha has three times as many one-dollar bills as she does five dollar bills. She has a total of $32. How many of each bill does she have?

# Ibid.
# Question #2
A vending machine has $41.25 in it. There are 255 coins total and the machine only accepts nickels, dimes and quarters. There are twice as many dimes as nickels. How many of each coin are in the machine.

# Source: http://voices.yahoo.com/how-set-solve-coin-word-problems-algebra-1713709.html

# Sample Coin Word Problem 1:
# Soln: Michael has 15 dimes and 12 quarters.
Michael had 27 coins in all, valuing $4.50. If he had only quarters and dimes, how many coins of each kind did he have?

# Ibid.
# Sample Coin Word Problem 2:
# Soln: Lucille has 25 quarters and 140 nickels.
Lucille had $13.25 in nickels and quarters. If she had 165 coins in all, how many of each type of coin did she have?

# Ibid.
# Sample Coin Word Problem 3:
# Soln: Ben has 150 dimes and 121 quarters.
Ben has $45.25 in quarters and dimes. If he has 29 less quarters than dimes, how many of each type of coin does he have?

# Source: http://www.calculatorsoup.com/calculators/wordproblems/algebrawordproblem1.php
A person has 12 coins consisting of dimes and pennies. If the total amount of money is $0.30, how many of each coin are there?

```

And this is the resulting '''output''' file. Each section contains a citation of the source of the input, the original input, the MAXIMA script as formulated by the perl script, and the solution extracted from the MAXIMA output:

```txt

# Bluman, Allan G. Math word problems demystified. 2005. The McGraw-Hill Companies Inc. p. 112, problem 1
original input: If a person has three times as many quarters as dimes and the total amount of money is $5.95, find the number of quarters and dimes.
v_quarter: 25$
v_dime: 10$
total: 595$
solve([n_quarter = 3 * n_dime, total = n_quarter * v_quarter + n_dime * v_dime], [n_quarter, n_dime]);
solution: n_quarter = 21, n_dime = 7

# Ibid., p. 112, problem 2
original input: A pile of 18 coins consists of pennies and nickels. If the total amount of the coins is 38¢, find the number of pennies and nickels.
v_nickel: 5$
v_penny: 1$
count: 18$
total: 38$
solve([total = n_nickel * v_nickel + n_penny * v_penny, count = n_nickel + n_penny], [n_nickel, n_penny]);
solution: n_nickel = 5, n_penny = 13

# Ibid., p. 112, problem 3
original input: A small child has 6 more quarters than nickels. If the total amount of coins is $3.00, find the number of nickels and quarters the child has.
v_quarter: 25$
v_nickel: 5$
total: 300$
solve([n_quarter = n_nickel + 6, total = n_quarter * v_quarter + n_nickel * v_nickel], [n_quarter, n_nickel]);
solution: n_quarter = 11, n_nickel = 5

# Ibid., p. 112, problem 4
original input: A child's bank contains 32 coins consisting of nickels and quarters. If the total amount of money is $3.80, find the number of nickels and quarters in the bank.
v_quarter: 25$
v_nickel: 5$
count: 32$
total: 380$
solve([total = n_quarter * v_quarter + n_nickel * v_nickel, count = n_quarter + n_nickel], [n_quarter, n_nickel]);
solution: n_quarter = 11, n_nickel = 21

# Ibid., p. 112, problem 5
original input: A person has twice as many dimes as she has pennies and three more nickels than pennies. If the total amount of the coins is $1.97, find the numbers of each type of coin the person has.
v_dime: 10$
v_nickel: 5$
v_penny: 1$
total: 197$
solve([n_dime = 2 * n_penny, n_nickel = n_penny + 3, total = n_dime * v_dime + n_nickel * v_nickel + n_penny * v_penny], [n_dime, n_nickel, n_penny]);
solution: n_dime = 14, n_nickel = 10, n_penny = 7

# Ibid., p. 112, problem 6
original input: In a bank, there are three times as many quarters as half dollars and 6 more dimes than half dollars. If the total amount of the money in the bank is $4.65, find the number of each type of coin in the bank.
v_half_dollar: 50$
v_quarter: 25$
v_dime: 10$
total: 465$
solve([n_quarter = 3 * n_half_dollar, n_dime = n_half_dollar + 6, total = n_half_dollar * v_half_dollar + n_quarter * v_quarter + n_dime * v_dime], [n_half_dollar, n_quarter, n_dime]);
solution: n_half_dollar = 3, n_quarter = 9, n_dime = 9

# Ibid., p. 112, problem 7
original input: A person bought 12 stamps consisting of 37¢ stamps and 23¢ stamps. If the cost of the stamps is $3.74, find the number of each type of the stamps purchased.
count: 12$
v_37_stamp: 37$
v_23_stamp: 23$
total: 374$
solve([total = n_37_stamp * v_37_stamp + n_23_stamp * v_23_stamp, count = n_37_stamp + n_23_stamp], [n_37_stamp, n_23_stamp]);
solution: n_37_stamp = 7, n_23_stamp = 5

# Ibid., p. 112, problem 8
original input: A dairy store sold a total of 80 ice cream sandwiches and ice cream bars. If the sandwiches cost $0.69 each and the bars cost $0.75 each and the store made $58.08, find the number of each sold.
count: 80$
v_sandwiches: 69$
v_bars: 75$
total: 5880$
solve([total = n_sandwiches * v_sandwiches + n_bars * v_bars, count = n_sandwiches + n_bars], [n_sandwiches, n_bars]);
solution: n_sandwiches = 20, n_bars = 60

# Ibid., p. 112, problem 9
original input: An office supply store sells college-ruled notebook paper for $1.59 a ream and wide-ruled notebook paper for $2.29 a ream. If a student purchased 9 reams of notebook paper and paid $15.71, how many reams of each type of paper did the student purchase?
v_college_ruled_notebook_paper: 159$
v_wide_ruled_notebook_paper: 229$
count: 9$
total: 1571$
solve([total = n_college_ruled_notebook_paper * v_college_ruled_notebook_paper + n_wide_ruled_notebook_paper * v_wide_ruled_notebook_paper, count = n_college_ruled_notebook_paper + n_wide_ruled_notebook_paper], [n_college_ruled_notebook_paper, n_wide_ruled_notebook_paper]);
solution: n_college_ruled_notebook_paper = 7, n_wide_ruled_notebook_paper = 2

# Ibid., p. 112, problem 10
original input: A clerk is given $75 in bills to put in a cash drawer at the start of a workday. There are twice as many $1 bills as $5 bills and one less $10 bill than $5 bills. How many of each type of bill are there?
total: 7500$
v_1: 100$
v_5: 500$
v_10: 1000$
solve([n_1 = 2 * n_5, n_1 = 2 * n_5, n_10 = n_5 - 1, total = n_1 * v_1 + n_5 * v_5 + n_10 * v_10], [n_1, n_5, n_10, n_5]);
solution: n_1 = 10, n_10 = 4, n_5 = 5

# Ibid., p. 109
original input: A person has 8 coins consisting of quarters and dimes. If the total amount of this change is $1.25, how many of each kind of coin are there?
v_quarter: 25$
v_dime: 10$
count: 8$
total: 125$
solve([total = n_quarter * v_quarter + n_dime * v_dime, count = n_quarter + n_dime], [n_quarter, n_dime]);
solution: n_quarter = 3, n_dime = 5

# Ibid., p. 110
original input: A person has 3 times as many dimes as he has nickels and 5 more pennies than nickels. If the total amount of these coins is $1.13, how many of each kind of coin does he have?
v_dime: 10$
v_nickel: 5$
v_penny: 1$
total: 113$
solve([n_dime = 3 * n_nickel, n_penny = n_nickel + 5, total = n_dime * v_dime + n_nickel * v_nickel + n_penny * v_penny], [n_dime, n_nickel, n_penny]);
solution: n_dime = 9, n_nickel = 3, n_penny = 8

# Ibid., p. 111
original input: A person bought ten greeting cards consisting of birthday cards costing $1.50 each and anniversary cards costing $2.00 each. If the total cost of the cards was $17.00, find the number of each kind of card the person bought.
count: 10$
total: 1700$
v_birthday_cards: 150$
v_anniversary_cards: 200$
solve([total = n_birthday_cards * v_birthday_cards + n_anniversary_cards * v_anniversary_cards, count = n_birthday_cards + n_anniversary_cards], [n_birthday_cards, n_anniversary_cards]);
solution: n_birthday_cards = 6, n_anniversary_cards = 4

# Ibid., p. 119, problem 8
original input: A person has 9 more dimes than nickels. If the total amount of money is $1.20, find the number of dimes the person has.
v_dime: 10$
v_nickel: 5$
total: 120$
solve([n_dime = n_nickel + 9, total = n_dime * v_dime + n_nickel * v_nickel], [n_dime, n_nickel]);
solution: n_dime = 11, n_nickel = 2

# Ibid., p. 120, problem 9
original input: A person has 20 bills consisting of $1 bills and $2 bills. If the total amount of money the person has is $35, find the number of $2 bills the person has.
count: 20$
total: 3500$
v_1: 100$
v_2: 200$
solve([total = n_1 * v_1 + n_2 * v_2, count = n_1 + n_2], [n_1, n_2, n_2]);
solution: n_1 = 5, n_2 = 15

# Ibid., p. 120, problem 10
original input: A bank contains 8 more pennies than nickels and 3 more dimes than nickels. If the total amount of money in the bank is $3.10, find the number of dimes in the bank.
v_dime: 10$
v_nickel: 5$
v_penny: 1$
total: 310$
solve([n_penny = n_nickel + 8, n_dime = n_nickel + 3, total = n_dime * v_dime + n_nickel * v_nickel + n_penny * v_penny], [n_dime, n_nickel, n_penny]);
solution: n_dime = 20, n_nickel = 17, n_penny = 25

# More test problems from around the web...
# Source: http://www.purplemath.com/modules/coinprob.htm
# Soln: 12 quarters, 14 dollar coins
original input: Your uncle walks in, jingling the coins in his pocket. He grins at you and tells you that you can have all the coins if you can figure out how many of each kind of coin he is carrying. You're not too interested until he tells you that he's been collecting those gold-tone one-dollar coins. The twenty-six coins in his pocket are all dollars and quarters, and they add up to seventeen dollars in value. How many of each coin does he have?
v_dollar_coin: 100$
v_quarter: 25$
count: 26$
total: 1700$
solve([total = n_dollar_coin * v_dollar_coin + n_quarter * v_quarter, count = n_dollar_coin + n_quarter], [n_dollar_coin, n_quarter]);
solution: n_dollar_coin = 14, n_quarter = 12

# Ibid.
# Soln: Then there are six quarters, and I can work backwards to figure out that there are 9 dimes and 18 nickels.
original input: A collection of 33 coins, consisting of nickels, dimes, and quarters, has a value of $3.30. If there are three times as many nickels as quarters, and one-half as many dimes as nickels, how many coins of each kind are there?
v_quarter: 25$
v_dime: 10$
v_nickel: 5$
count: 33$
total: 330$
solve([n_nickel = 3 * n_quarter, n_dime = 0.5 * n_nickel, total = n_quarter * v_quarter + n_dime * v_dime + n_nickel * v_nickel, count = n_quarter + n_dime + n_nickel], [n_quarter, n_dime, n_nickel]);
solution: n_quarter = 6, n_dime = 9, n_nickel = 18

# Ibid.
# Soln. There are nine of each type of coin in the wallet.
original input: A wallet contains the same number of pennies, nickels, and dimes. The coins total $1.44. How many of each type of coin does the wallet contain?
v_dime: 10$
v_nickel: 5$
v_penny: 1$
total: 144$
solve([n_penny = n_nickel, n_nickel = n_dime, total = n_dime * v_dime + n_nickel * v_nickel + n_penny * v_penny], [n_dime, n_nickel, n_penny]);
solution: n_dime = 9, n_nickel = 9, n_penny = 9

# Source: http://www.algebralab.org/Word/Word.aspx?file=Algebra_CoinProblems.xml
# Soln: Ken has 17 nickels and 8 dimes.
original input: Suppose Ken has 25 coins in nickels and dimes only and has a total of $1.65. How many of each coin does he have?
v_dime: 10$
v_nickel: 5$
count: 25$
total: 165$
solve([total = n_dime * v_dime + n_nickel * v_nickel, count = n_dime + n_nickel], [n_dime, n_nickel]);
solution: n_dime = 8, n_nickel = 17

# Ibid.
# Let's Practice
# Question #1
# Note: The original question had an inconsistency in it,
# namely "Terry has 7 more" should be "Terry has 2 more..."
# Soln: Terry has 18 dimes and 20 quarters.
original input: Terry has 2 more quarters than dimes and has a total of $6.80. The number of quarters and dimes is 38. How many quarters and dimes does Terry have?
v_quarter: 25$
v_dime: 10$
count: 38$
total: 680$
solve([n_quarter = n_dime + 2, total = n_quarter * v_quarter + n_dime * v_dime, count = n_quarter + n_dime], [n_quarter, n_dime]);
solution: n_quarter = 20, n_dime = 18

# Ibid.
# Question #2
# Soln: There are 2 ten-dollar bills, 8 one-dollar bills, and 3 five-dollar bills.
original input: In my wallet, I have one-dollar bills, five-dollar bills, and ten-dollar bills. The total amount in my wallet is $43. I have four times as many one-dollar bills as ten-dollar bills. All together, there are 13 bills in my wallet. How many of each bill do I have?
count: 13$
total: 4300$
v_1: 100$
v_5: 500$
v_10: 1000$
solve([n_1 = 4 * n_10, n_1 = 4 * n_10, total = n_1 * v_1 + n_5 * v_5 + n_10 * v_10, count = n_1 + n_5 + n_10], [n_1, n_5, n_10, n_1, n_10]);
solution: n_5 = 3, n_1 = 8, n_10 = 2

# Ibid.
# Try These
# Question #1
original input: Marsha has three times as many one-dollar bills as she does five dollar bills. She has a total of $32. How many of each bill does she have?
total: 3200$
v_1: 100$
v_5: 500$
solve([n_1 = 3 * n_5, n_1 = 3 * n_5, total = n_1 * v_1 + n_5 * v_5], [n_1, n_5]);
solution: n_1 = 12, n_5 = 4

# Ibid.
# Question #2
original input: A vending machine has $41.25 in it. There are 255 coins total and the machine only accepts nickels, dimes and quarters. There are twice as many dimes as nickels. How many of each coin are in the machine.
v_quarter: 25$
v_dime: 10$
v_nickel: 5$
count: 255$
total: 4125$
solve([n_dime = 2 * n_nickel, total = n_quarter * v_quarter + n_dime * v_dime + n_nickel * v_nickel, count = n_quarter + n_dime + n_nickel], [n_quarter, n_dime, n_nickel]);
solution: n_quarter = 120, n_dime = 90, n_nickel = 45

# Source: http://voices.yahoo.com/how-set-solve-coin-word-problems-algebra-1713709.html
# Sample Coin Word Problem 1:
# Soln: Michael has 15 dimes and 12 quarters.
original input: Michael had 27 coins in all, valuing $4.50. If he had only quarters and dimes, how many coins of each kind did he have?
v_quarter: 25$
v_dime: 10$
count: 27$
total: 450$
solve([total = n_quarter * v_quarter + n_dime * v_dime, count = n_quarter + n_dime], [n_quarter, n_dime]);
solution: n_quarter = 12, n_dime = 15

# Ibid.
# Sample Coin Word Problem 2:
# Soln: Lucille has 25 quarters and 140 nickels.
original input: Lucille had $13.25 in nickels and quarters. If she had 165 coins in all, how many of each type of coin did she have?
v_quarter: 25$
v_nickel: 5$
count: 165$
total: 1325$
solve([total = n_quarter * v_quarter + n_nickel * v_nickel, count = n_quarter + n_nickel], [n_quarter, n_nickel]);
solution: n_quarter = 25, n_nickel = 140

# Ibid.
# Sample Coin Word Problem 3:
# Soln: Ben has 150 dimes and 121 quarters.
original input: Ben has $45.25 in quarters and dimes. If he has 29 less quarters than dimes, how many of each type of coin does he have?
v_quarter: 25$
v_dime: 10$
total: 4525$
solve([n_quarter = n_dime - 29, total = n_quarter * v_quarter + n_dime * v_dime], [n_quarter, n_dime]);
solution: n_quarter = 121, n_dime = 150

# Source: http://www.calculatorsoup.com/calculators/wordproblems/algebrawordproblem1.php
original input: A person has 12 coins consisting of dimes and pennies. If the total amount of money is $0.30, how many of each coin are there?
v_dime: 10$
v_penny: 1$
count: 12$
total: 30$
solve([total = n_dime * v_dime + n_penny * v_penny, count = n_dime + n_penny], [n_dime, n_penny]);
solution: n_dime = 2, n_penny = 10

```


{{omit from|PARI/GP}} <!-- No NLP support and clearly inappropriate for this task -->
