+++
title = "Perl"
description = ""
date = 2018-04-09T03:30:53Z
aliases = []
[extra]
id = 1697
[taxonomies]
categories = []
tags = []
+++
'''Perl''' is both the name of a programming language and the name of the primary implementation of that language.

As a language, it takes the best from other programming languages such as [derived from::BASIC](https://rosettacode.org/wiki/derived_from::BASIC), [derived from::Lisp](https://rosettacode.org/wiki/derived_from::Lisp), [derived from::C](https://rosettacode.org/wiki/derived_from::C) and the [Unix](https://rosettacode.org/wiki/Unix) tools [derived from::sed](https://rosettacode.org/wiki/derived_from::sed), [derived from::AWK](https://rosettacode.org/wiki/derived_from::AWK) and [the UNIX shell](https://rosettacode.org/wiki/derived_from::UNIX_Shell). It is particularly suited for Unix systems programming, [text processing](https://rosettacode.org/wiki/:Category:Text_processing) and [gluing heterogeneous programs together](https://rosettacode.org/wiki/Data_Munging). Its interpreter is called perl. Perl has seen five major revisions, and [a sixth](https://rosettacode.org/wiki/Perl_6) is currently being worked on.

The implementation known as Perl is available on a wide variety of mostly UNIX-based operating systems.

## Citations
* [Wikipedia:Perl](https://en.wikipedia.org/wiki/Perl)

## Todo
[Reports:Tasks_not_implemented_in_Perl](https://rosettacode.org/wiki/Reports:Tasks_not_implemented_in_Perl)


## Merged content



This is a simple non-optimizing and non-optimized interpreter that is written in [Perl](https://rosettacode.org/wiki/Perl). To run a program, give the name of its file as an argument to the interpreter. If you don't provide an argument, the program will be read from standard input.

You can set your own values for the standard nonstandardized behaviors (like the value "," returns on EOF) by changing the variables listed under "Implementation-specific variables". The other nonstandardized aspects of the language are allowed to follow Perl's defaults: for instance, modifying a cell past the forward end of the tape will lengthen it, but going backward from the first cell will wrap around to the final cell. These could be made somewhat more sensible, but doing so would serve little purpose, as this interpreter is meant to serve as a simple example, not an industrial-strength tool.


```perl
#!/usr/bin/perl

use strict;

# ------------------------------------------------------------
# Implementation-specifc variables
# ------------------------------------------------------------

our $tape_length = 50_000; # How many memory cells the tape will have.
our $eof_val = -1; # The value "," returns on EOF.
our $unbalanced_exit_code = 1;
  # The exit code the interpreter will return if it reaches the
  # end of the code while searching for a matching "]" to jump to.

# ------------------------------------------------------------
# Declarations
# ------------------------------------------------------------

our @code; # The program's code split into characters.
our $cmd;  # The index of the current command in @code.
our @tape; # The program's memory tape.
our $cell; # The index of the current cell in @tape.
our @loops; # The @code-index of each "["-loop we're
            # currently inside.

our %commands =
   ('>' => sub { ++$cell },
    '<' => sub { --$cell },
    '+' => sub { ++$tape[$cell] },
    '-' => sub { --$tape[$cell] },
    '.' => sub { print chr $tape[$cell] },
    ',' => sub { $tape[$cell] = get_input() },
    '[' => sub { $tape[$cell] ? push(@loops, $cmd) : jump() },
    ']' => sub { $cmd = pop(@loops) - 1 });

 {my $input_buffer = ''; # Where we store each line of input.
  sub get_input
  # Gets one character of input and returns its numeric value.
   {$input_buffer or $input_buffer = <STDIN>;
    return defined($input_buffer)
      ? ord substr($input_buffer, 0, 1, '')
      : $eof_val;}}

sub jump
# Moves the instruction pointer from a "[" to a matching "]".
 {my $depth = 0;
  until ($depth < 0)
     {++$cmd;
      $cmd < @code or exit($unbalanced_exit_code);
      $code[$cmd] eq '[' and ++$depth;
      $code[$cmd] eq ']' and --$depth;}}

# ------------------------------------------------------------
# Setup
# ------------------------------------------------------------

# Get the code.
@code = split //, do
   {undef $/;
    if (@ARGV)
       {open(IN, '<', $ARGV[0]) or
          die "Couldn't open program file.\n($!)\n";
        <IN>;}
    else
       {<STDIN>;}};

# Initialize the other important variables.
$cmd = 0;
$tape[$_] = 0 foreach 0 .. $tape_length - 1;
$cell = 0;
@loops = ();

# ------------------------------------------------------------
# Execution
# ------------------------------------------------------------

for (;;)
   {if (exists $commands{$code[$cmd]})
       {$commands{$code[$cmd]}->();}
    # We simply ignore meaningless characters.
    ++$cmd;
    $cmd < @code or last;}
```

