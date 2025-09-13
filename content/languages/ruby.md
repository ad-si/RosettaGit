+++
title = "Ruby"
description = ""
date = 2018-04-09T03:55:58Z
aliases = []
[extra]
id = 1695
[taxonomies]
categories = []
tags = []
+++
Ruby is an interpreted language written by Yukihiro "matz" Matsumoto that originates from Japan.

It is a language that combines
* Joy of programming
* Powerful [object-oriented](https://rosettacode.org/wiki/object-oriented) style
* Flexibility and extensibility
* [Platform](https://rosettacode.org/wiki/Platform) independence

Ruby is a language of careful balance. Its creator blended parts of his favorite languages ([Perl](https://rosettacode.org/wiki/Perl), [Smalltalk](https://rosettacode.org/wiki/Smalltalk), [Eiffel](https://rosettacode.org/wiki/Eiffel), [Ada](https://rosettacode.org/wiki/Ada), and [Lisp](https://rosettacode.org/wiki/Lisp)) to form a new language that balances [functional programming](https://rosettacode.org/wiki/functional_programming) with [imperative programming](https://rosettacode.org/wiki/imperative_programming).

He has often said that he is “trying to make Ruby natural, not simple,” in a way that mirrors life.

Since its public release in 1995, Ruby has drawn devoted coders worldwide. In 2006, Ruby achieved mass acceptance. The [http://www.tiobe.com/tpci.htm TIOBE] index, which measures the growth of programming languages, ranks Ruby as #11 among programming languages worldwide. Much of the growth is attributed to the popularity of software written in Ruby, particularly the Rails web framework.

An interactive tutorial is available in the form of an [http://tryruby.org/ Interactive Console] inside of your Web Browser.

## Todo
[Reports:Tasks_not_implemented_in_Ruby](https://rosettacode.org/wiki/Reports:Tasks_not_implemented_in_Ruby)


## Merged content



## Version 1
An implementation of a [Brainfuck](https://rosettacode.org/wiki/Brainfuck) interpreter in [Ruby](https://rosettacode.org/wiki/Ruby).
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
```txt
Hello World!
@
```


## Version 2
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
