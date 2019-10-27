+++
title = "Execute Brain****/Perl 6"
description = ""
date = 2018-04-01T19:06:38Z
aliases = []
[extra]
id = 8324
[taxonomies]
categories = []
tags = []
+++

{{works with|Rakudo|2018.03}}

```perl6
class BFInterpreter {
    has @.code;
    has @!mem;
    has @!loop_stack;
    has @!input_buffer;
    has $!m;
    has $!c;

    method new (Str $code) {
        BFInterpreter.bless(code => $code.lines.comb);
    }

    method run {
        $!c = 0;
        $!m = 0;
        while $!c < @.code {
            given @.code[$!c] {
                when '>' { $!m++ }
                when '<' { $!m-- }
                when '+' { @!mem[$!m]++ }
                when '-' { @!mem[$!m]-- }
                when '.' { @!mem[$!m].chr.print }
                when ',' {
                    @!input_buffer = $*IN.get.comb unless @!input_buffer.elems > 0;
                    @!mem[$!m] = @!input_buffer.shift;
                }
                when '[' {
                    @!mem[$!m] == 0 ?? self!jump !! @!loop_stack.push($!c);
                }
                when ']' {
                    my $b = @!loop_stack.pop - 1;
                    $!c = $b if @!mem[$!m] > 0;
                }
            }
	    $!c++;
        }
    }

    method !jump {
        my $depth = 1;
        while $depth {
            $!c++;
            die "unbalanced code" if $!c >= @.code.elems;
            $depth++ if @.code[$!c] eq '[';
            $depth-- if @.code[$!c] eq ']';
        }
    }
}

# Test: "Hello World" program:

my $code = "++++++++++
           [>+++++++>++++++++++>+++>+<<<<-]
           >++.>+.+++++++..+++.>++.<<+++++++++++++++.>.
           +++.------.--------.>+.>.";

my $bfi = BFInterpreter.new($code);
$bfi.run;

```

