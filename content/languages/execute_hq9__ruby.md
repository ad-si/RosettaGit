+++
title = "Execute HQ9+/Ruby"
description = ""
date = 2010-02-06T14:30:52Z
aliases = []
[extra]
id = 4562
[taxonomies]
categories = []
tags = []
+++


This [Ruby](https://rosettacode.org/wiki/Ruby) program implements an [HQ9+](https://rosettacode.org/wiki/HQ9+) interpreter.

```ruby
class HQ9plus
  Dispatch = Hash.new(:unknown).merge({
      'h' => :hello, 
      'q' => :quine, 
      '9' => :beer, 
      '+' => :accumulate
  })

  def initialize(opts={})
    @program = if    opts[:program]  then opts[:program]
               elsif opts[:filename] then File.read(opts[:filename])
               else  ''
               end
    @accumulator = 0
  end
  attr_reader :accumulator

  def run
    @program.downcase.each_char {|char| self.send Dispatch[char]}
  end

  def hello
    puts "Hello, world!"
  end

  def quine
    puts @program
  end

  def beer
    puts '99 bottles here ...'
  end

  def accumulate
    @accumulator += 1
  end

  def unknown
    # do nothing
  end
end
  
hq9 = HQ9plus.new(:program => '+qhp;+9Q')
hq9.run
puts hq9.accumulator
```


Output:


```txt
+qhp;+9Q
Hello, world!
99 bottles here ...
+qhp;+9Q
2
```

