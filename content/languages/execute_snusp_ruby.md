+++
title = "Execute SNUSP/Ruby"
description = ""
date = 2010-02-06T14:36:41Z
aliases = []
[extra]
id = 4578
[taxonomies]
categories = []
tags = []
+++


These [Ruby](https://rosettacode.org/wiki/Ruby) implementations of SNUSP profiles
are partially derived from [RCSNUSP/Tcl](https://rosettacode.org/wiki/RCSNUSP/Tcl).


## Core SNUSP

```ruby
$stdout.sync = true
$stdin.sync = true

class CoreSNUSP
  Bounce = {
    :ruld => {:right => :up,   :left => :down, :up => :right, :down => :left },
    :lurd => {:right => :down, :left => :up,   :up => :left,  :down => :right}
  }
  Delta = {:up => [-1, 0], :down => [1, 0], :left => [0,-1], :right => [0, 1]}
  Dispatch = Hash.new(:unknown).update({
    ">" => :right,
    "<" => :left,
    "+" => :incr,
    "-" => :decr,
    "/" => :ruld,
    "\\"=> :lurd,
    "?" => :skipz,
    "!" => :skip,
    "." => :write,
    "," => :read,
    '"' => :dump,
  })

  def initialize(text, args={})
    @data = Hash.new(0)
    @dptr = 0
    @program, @pc = read_program(text)
    @height = @program.size
    @width  = @program[0].nil? ? 0 : @program[0].size
    @pdir = :right
    @input = args[:input]
  end

  def read_program(text)
    pc = [0,0]
    program = []
    max = 0
    text.each_line.each_with_index do |line, lineno|
      line.chomp!
      max = [max, line.length].max
      if not (idx = line.index("$")).nil?
        pc = [lineno, idx]
      end
      program << line.split(//)
    end
    # pad short lines
    program.map! {|row| row.concat([""] * (max - row.size))}
    [program, pc]
  end

  def current_command
    @program[@pc[0]].nil? and return nil
    @program[@pc[0]][@pc[1]]
  end

  def run
    p @program if $DEBUG
    command = current_command
    p [@pc, command] if $DEBUG
    catch :have_exit_command do
      until command.nil?
        self.send Dispatch[command]
        move
        command = current_command
        p [@pc, command] if $DEBUG
      end
    end
  end

  def move
    delta = Delta[@pdir]
    @pc = [ @pc[0] + delta[0], @pc[1] + delta[1]]
    if @pc[0] < 0 or @pc[0] > @height or @pc[1] < 0 or @pc[1] > @width
      raise IndexError, "program counter out of bounds: #{@pc.inspect}"
    end
  end

  def right
    @dptr += 1
  end

  def left
    @dptr -= 1
  end

  def incr
    if @dptr < 0
      raise IndexError, "data pointer less than zero: #@dptr"
    end
    @data[@dptr] += 1
    p ["data:",@dptr, @data[@dptr]] if $DEBUG
  end

  def decr
    if @dptr < 0
      raise IndexError, "data pointer less than zero: #@dptr"
    end
    @data[@dptr] -= 1
    p ["data:",@dptr, @data[@dptr]] if $DEBUG
  end

  def ruld
    p @pdir if $DEBUG
    @pdir = Bounce[:ruld][@pdir]
    p @pdir if $DEBUG
  end

  def lurd
    p @pdir if $DEBUG
    @pdir = Bounce[:lurd][@pdir]
    p @pdir if $DEBUG
  end

  def skipz
    p ["data:",@dptr, @data[@dptr]] if $DEBUG
    move if @data[@dptr] == 0
  end

  def skip
    move
  end

  def write
    p "output: #{@data[@dptr]} = '#{@data[@dptr].chr}'" if $DEBUG
    $stdout.print @data[@dptr].chr
  end

  def read
    @data[@dptr] =  if @input.nil?
                      # blocking read from stdin
                      $stdin.getc
                    else
                      @input.slice!(0)
                    end
    p "read '#{@data[@dptr]}'" if $DEBUG
  end

  def dump
    p [@dptr, @data]
  end

  def unknown; end
end

##############################################################################
if __FILE__ == $0

puts "Empty Program 1"
emptyProgram = ''
snusp = CoreSNUSP.new(emptyProgram)
snusp.run
puts "done"

puts "Hello World"
# brainfuck
#helloworld = '++++++++++[>+++++++>++++++++++>+++>+<<<<-]>++.>+.+++++++..+++.>++.<<+++++++++++++++.>.+++.------.--------.>+.>.'
#
# http://c2.com/cgi/wiki?SnuspLanguage -- "Is there a simple way to translate an arbitrary Brainfuck program into Core SNUSP?"
#
# a[bc]d
#
#translates to
#
# a!/=?\d
#   \cb/ 
#
#helloworld = <<'PROGRAM'
#++++++++++!/
### =======================
?\>++.>+.+++++++..+++.>++.<<+++++++++++++++.>.+++.------.--------.>+.>.'
#           \-<<<<+>+++>++++++++++>+++++++>/
#PROGRAM

helloWorld = <<'PROGRAM'
/++++!/
### =====
?\>++.>+.+++++++..+++\
\+++\ | /+>+++++++>/ /++++++++++<<.++>./
$+++/ | \+++++++++>\ \+++++.>.+++.-----\
      \==-<<<<+>+++/ /=.>.+>.--------.-/
PROGRAM
snusp = CoreSNUSP.new(helloWorld)
snusp.run
puts "done"

end
```


Output:

```txt
$ ruby rc_coresnusp.rb
Empty Program 1
done
Hello World
Hello World!
done
```


## Modular SNUSP

```ruby
require 'rc_coresnusp.rb'

class ModularSNUSP < CoreSNUSP
  Dispatch = self.superclass::Dispatch.update({
    "@" => :enter,
    "#" => :leave,
  })
  
  def initialize(text, args={})
    super
    @execution_stack = []
  end
  
  def enter
    p @execution_stack if $DEBUG
    current_pc = @pc
    move
    @execution_stack << [@pc, @pdir]
    p @execution_stack if $DEBUG
    @pc = current_pc
  end
  
  def leave
    p @execution_stack if $DEBUG
    throw :have_exit_command if @execution_stack.empty?
    @pc, @pdir = @execution_stack.pop
    p @execution_stack if $DEBUG
  end
end

##############################################################################
if __FILE__ == $0

puts "Empty Program 2"
emptyProgram = '$#'
snusp = ModularSNUSP.new(emptyProgram)
snusp.run
puts "done"

puts "Goodbye World"
goodbyeWorld = <<'PROGRAM'
@\G.@\o.o.@\d.--b.@\y.@\e.>@\comma.@\.<-@\W.+@\o.+++r.------l.@\d.>+.!=>\
 |   |     \@------|#  |    \@@+@@++|+++#-    \\               -  /+++++/
 |   \@@@@=+++++#  |   \===--------!\===!\-----|-------#-------/  \+++++.# nl
 \@@+@@@+++++#     \!#+++++++++++++++++++++++#!/
PROGRAM
snusp = ModularSNUSP.new(goodbyeWorld)
snusp.run
puts "done"

puts "Modular: Ackerman"
# this program requires two characters to be read on standard input:
# the numbers for j and i in that order
ackerman = <<'PROGRAM'
@\\ /==!/atoi------------------------------------------------#
/ / |   |       /
### ===
\!==\!====\   (recursion)
>\,@/>,@/=ACK==!\?\<+#    |   |     |   A(0,j) -> j+1
> j   i           \<?\+>-@/#  |     |   A(i,0) -> A(i-1,1)
@                    \@\>@\->@/@\<-@/#  A(i,j) -> A(i-1, A(i, j-1))
\=\        [0]->[2]    |  |     |    
> +       #/?<<+>>-\!==/  |     \==!/-<<+>>?\#  [2]->[0]          
+ +        \->>+<<?/#     |        #\?>>+<<-/                     
+ +                       |                                       
+ +                       \@\>>>@\<<#    copy [1]->[3] and advance
+ +        [1]->[3][4]      |    |                                
+ +       #/?<<<+>+>>-\!====/    \==!/-<<<+>>>?\#    [4]->[1]     
+ +        \->>+>+<<<?/#            #\?>>>+<<<-/                  
+ +                                                                  
+ + print newline   
+ +             /-\     
+ \+++CR.---NL.!\?/<<#
|
|           /<=!/!#++++\ 
\
### ==
@\@/.>@/.#   /+\ atoi
          |       /=\/+++\ wiki
 /==div===/       \!\++++/
 |                   \++/
 |    /-\             \/
 \?\<!\?/#!===+<<<\      /-\ wiki
   \<==@\>@\>>!/?!/=<?\>!\?/<<#
        |  |  #\->->+</
        \=!\=?!/->>+<<?\#
              #\?<<+>>-/
PROGRAM
snusp = ModularSNUSP.new(ackerman, :input => '33')  # calculate A(3,3)
snusp.run
puts
puts "done"

end
```

Output:

```txt
$ ruby rc_modularsnusp.rb
Empty Program 2
done
Goodbye World
Goodbye, World!
done
Modular: Ackerman

61
done
```


## Bloated SNUSP

User input is still line-oriented -- must press Enter before Ruby sees input.


```ruby
require 'rc_modularsnusp.rb'

# here, need input to be non-blocking, so other "threads" can keep working.
# we'll have to simulate blocking when reading a character from stdin.  see read() below
require 'fcntl'
$stdin.fcntl(Fcntl::F_SETFL, Fcntl::O_NONBLOCK)

SNUSPThread = Struct.new(:pc, :pdir, :execution_stack, :dptr)

class BloatedSNUSP < ModularSNUSP
  Dispatch = self.superclass::Dispatch.update({
    ":" => :up,
    ";" => :down,
    "&" => :split,
    "%" => :rand,
  })
  
  def initialize(text, args={})
    super
    @dptr = [0,0]
    @threads = {}
    @thread_counter = 0
    add_thread
  end
  
  def add_thread
    @thread_counter += 1
    @threads[@thread_counter] = SNUSPThread.new(@pc.dup, @pdir, @execution_stack, @dptr.dup)
  end
  
  def run
    p @program if $DEBUG
    
    until @threads.empty?
      stopped_thread_ids = []
      # threads are run in arbitrary order
      p @threads if $DEBUG
      @threads.each_key do |thread_id|
        thread = @threads[thread_id]
        
        @pc = thread.pc
        @pdir = thread.pdir
        @dptr = thread.dptr
        @execution_stack = thread.execution_stack
        
        if tick(thread_id)
          # save state
          thread.pc = @pc
          thread.pdir = @pdir
          thread.dptr = @dptr
          thread.execution_stack = @execution_stack
        else
          stopped_thread_ids << thread_id
        end
      end
      stopped_thread_ids.each {|id| @threads.delete(id)}
    end
  end
  
  def tick(thread_id)
    command = current_command
    p [thread_id, @pc, command] if $DEBUG
    return false if command.nil?  # thread complete
    return false if self.send(Dispatch[command]) == :have_exit_command
    move
    command = current_command
    p [@pc, command] if $DEBUG
    true
  end
  
  def leave
    p @execution_stack if $DEBUG
    return :have_exit_command if @execution_stack.empty?
    @pc, @pdir = @execution_stack.pop
    p @execution_stack if $DEBUG
  end

  def read
    # we want input to be blocking.  However, actual blocking on stdin will halt
    # all other running "threads".  So, what we do here is to set stdin to be
    # non-blocking (at top of this file), and if we fail to read a character,
    # we backup the program counter so we attempt to read again at the next tick.
    char =  if @input.nil?
              begin
                $stdin.sysread(1)
              rescue SystemCallError
                nil
              end
            else
              @input.slice!(0)
            end
    if char.nil?
      p "no data to read" if $DEBUG
      # backup, so we can poll again in the next tick.
      reverse_direction = {:up => :down, :down => :up, :right => :left, :left => :right}
      @pdir = reverse_direction[@pdir]
      move
      @pdir = reverse_direction[@pdir]
    else
      @data[@dptr] = char 
      p "read '#{@data[@dptr]}'" if $DEBUG
    end
  end
  
  def left
    @dptr[0] -= 1
  end
  def right
    @dptr[0] += 1
  end
  def up
    @dptr[1] -= 1
  end
  def down
    @dptr[1] += 1
  end
  def incr
    if @dptr[0] < 0 or @dptr[1] < 0
      raise IndexError, "data pointer less than zero: #{@dptr.inspect}"
    end
    @data[@dptr] += 1
    p ["data:",@dptr, @data[@dptr]] if $DEBUG
  end
  def decr
    if @dptr[0] < 0 or @dptr[1] < 0
      raise IndexError, "data pointer less than zero: #{@dptr.inspect}"
    end
    @data[@dptr] -= 1
    p ["data:",@dptr, @data[@dptr]] if $DEBUG
  end
  
  def split
    move
    add_thread
  end
  
  def rand
    @data[@dptr] = Kernel.rand(1+@data[@dptr])
  end
end

##############################################################################
if __FILE__ == $0

# This prints out a random number of random length:
puts "Bloated: random"
random = <<'PROGRAM'
$!/+++++++++%++++++++++++++++++++++++++++++++++++++++++++++++.!/-\
  \
### ==============================
<#!?%+++++++++++++++>===\?/
PROGRAM
snusp = BloatedSNUSP.new(random)
snusp.run
puts "done"

# This (from the esolangs wiki) prints exclamation marks until you press a key:
puts "Bloated: BangBang"
bang = <<'PROGRAM'
                    /==.==<==\
                    |        |
     /+++++++++++==&\==>===?!/==<<==#
     \+++++++++++\  |
$==>==+++++++++++/  \==>==,==#
PROGRAM
snusp = BloatedSNUSP.new(bang)
snusp.run
puts "done"

$stdin.read  # clear stdin before next interactive program

# programs from  http://www.baumanfamily.com/john/esoteric.html
puts "Bloated: randout"
randout = <<'PROGRAM'
    //?\!
### .=
/!=\/\/\/\/=!\\
    |\-/++\      |  ++++++++  \/
    |  +  +      |  ++++++++
    |  +  +      |  ++++++++
    |  +  +      <  \/\/\/\/
$==&\=+/  \+=%>?!/====#
    | 
    |
    |
    \==>==,==#
PROGRAM
snusp = BloatedSNUSP.new(randout)
snusp.run
puts "done"

end
```


Output:

```txt
$ ruby rc_bloatedsnusp.rb
Bloated: random
3476158785073done
Bloated: BangBang
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!j!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
done
Bloated: randout
35382407644156390115:505854:46783082311335:4940492329597706::155
2done

```
