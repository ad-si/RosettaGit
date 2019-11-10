+++
title = "Execute Brainfuck/Ruby"
description = ""
date = 2015-09-07T19:57:40Z
aliases = []
[extra]
id = 4563
[taxonomies]
categories = []
tags = []
+++

{{implementation|Brainfuck}}{{collection|RCBF}}
==Version 1==
An implementation of a [[Brainfuck]] interpreter in [[Ruby]].
More effort could be made to read a program from a file or from stdin.


```ruby
class RCBF
  def initialize(program)
    @d = [0] * 30_000
    @program = program
    @jumpback_table = read_program
  end

  def read_program
    jumpback_table = {}
    jump_to = []
    @program.each_char.with_index do |char, idx|
      case char
      when "[" then jump_to.push(idx)
      when "]" then jumpback_table[idx] = jump_to.pop
      end
    end
    jumpback_table
  end

  def run
    invert_table = @jumpback_table.invert
    dc = 0
    pc = 0
    while pc < @program.length
      print [pc, @program[pc]] if $DEBUG

      case @program[pc]
      when ?>
        dc += 1
        print "\t#{dc}" if $DEBUG
      when ?<
        dc -= 1
        print "\t#{dc}" if $DEBUG
      when ?+
        @d[dc] += 1
        print "\t#{dc},#{@d[dc]}" if $DEBUG
      when ?-
        @d[dc] -= 1
        print "\t#{dc},#{@d[dc]}" if $DEBUG
      when ?.
        print "\t#{dc},#{@d[dc]}\t" if $DEBUG
        print @d[dc].chr
      when ?,
        @d[dc] = $stdin.getc
        print "\t#{dc},#{@d[dc]}" if $DEBUG
      when ?[
        if @d[dc] == 0
          pc = invert_table[pc]
          print "  #{[pc,@program[pc]]}" if $DEBUG
        end
      when ?]
        if @d[dc] != 0
          pc = @jumpback_table[pc]
          print "  #{[pc,@program[pc]]}" if $DEBUG
        end
      end
      puts if $DEBUG
      pc += 1
    end
  end
end

# output 'Hello World!\n'
helloworld = <<PROGRAM
++++++++++[>+++++++>++++++++++>+++>+<<<<-]
>++.>+.+++++++..+++.>++.<<+++++++++++++++.>.+++.------.--------.>+.>.
PROGRAM
bf = RCBF.new(helloworld)
bf.run

# use nested loop to increment count to 64 and print (should be '@')
# followed by a newline
RCBF.new('>>++++[<++++[<++++>-]>-]<<.[-]++++++++++.').run
```


{{out}}

```txt
Hello World!
@
```


==Version 2==
This variant converts the brainfuck into Ruby code and runs that instead.
Do note that this requires Ruby1.9.1 or later, earlier versions need a somewhat more verbose variant.

BF code may be read from a file or taken from STDIN.


```ruby
eval 'm=Hash.new(p=0);'+ARGF.read.gsub(
        /./,
        '>' => 'p+=1;',
        '<' => 'p-=1;',
        '+' => 'm[p]+=1;',
        '-' => 'm[p]-=1;',
        '[' => '(;',
        ']' => ')while((m[p]&=255)!=0);',
        '.' => 'putc(m[p]&=255);',
        ',' => 'm[p]=STDIN.getc.ord if !STDIN.eof;')
