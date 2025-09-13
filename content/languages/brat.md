+++
title = "Brat"
description = ""
date = 2012-05-18T23:32:16Z
aliases = []
[extra]
id = 9155
[taxonomies]
categories = []
tags = []
+++
Brat is a little language which tries to let you do what you want to do, because it knows no one is the boss of you. While influenced by [Ruby](https://rosettacode.org/wiki/Ruby) in many ways, it accidentally resembles [Javascript](https://rosettacode.org/wiki/Javascript). The language design attempts to avoid "special cases" as much as possible, and therefore has no keywords and very few special symbols.

In Brat, everything is either an object or a function, and all functions are closures. Objects are essentially just collections of functions which can have inheritance relationships with other objects. The object system in Brat uses a prototyping approach, so new objects are always created as children of some existing object. Functions in Brat are first-class values which can be passed around like any other value.

Brat is also a very eager language. The only way to delay code evaluation is to enclose it in a function. Any use of a variable containing a function is assumed to be calling the function.


## Merged content



An implementation of a [Brainfuck](https://rosettacode.org/wiki/Brainfuck) interpreter in [Brat](https://rosettacode.org/wiki/Brat).

Does not handle EOF when getting input. Tape is "infinite" in the positive direction. Each cell holds an integer.


```brat
#BF machine
bf = object.new

#Initialize with array of instructions
bf.init = { instructions |
  my.instructions = instructions
  my.program_counter = 0
  my.pointer = 0
  my.tape = []
  calculate_jumps

  #Override my.tape[] to return 0 instead of null
  my.tape.original_get = my.tape->get
  my.tape.get = { index |
    val = my.original_get index
    null? val
      { 0 }
      { val }
  }
}

#Run instructions
bf.prototype.run = {
  current_instruction = null

  while { current_instruction = instructions[my.program_counter] }
    {
      action = my.interpreter[current_instruction]
      action

      my.program_counter = my.program_counter + 1
    }
}

#Holds all operations
bf.interpreter = [:]

bf.interpreter[:>] = {
  my.pointer = my.pointer + 1
}

bf.interpreter[:<] = {
  my.pointer = my.pointer - 1
}

bf.interpreter[:+] = {
  my.tape[my.pointer] = my.tape[my.pointer] + 1
}

bf.interpreter[:-] = {
  my.tape[my.pointer] = my.tape[my.pointer] - 1
}

bf.interpreter["."] = {
  print my.tape[my.pointer].to_char
}

bf.interpreter[","] = {
  my.tape[my.pointer] = g.to_byte
}

bf.interpreter["["] = {
  true? my.tape[my.pointer] == 0
    { my.program_counter = my.jumps[my.program_counter] }
}

bf.interpreter["]"] = {
  false? my.tape[my.pointer] == 0
    { my.program_counter = my.jumps[my.program_counter] }
}

#Precalcuate '[' and ']' jump locations
bf.prototype.calculate_jumps = {
  forwards = []
  jumps = [:]

  my.instructions.each_with_index { ins, index |
    when { ins == "[" } { forwards << index }
      { ins == "]" } {
        match = forwards.pop
        jumps[match] = index
        jumps[index] = match
      }
  }

  my.jumps = jumps
}

#Get file name from user and run it
include :file

bf.new(file.read(ask "BF file: ").dice).run
```

