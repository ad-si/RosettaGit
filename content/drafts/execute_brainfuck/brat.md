+++
title = "Execute Brainfuck/Brat"
description = ""
date = 2011-11-14T22:29:36Z
aliases = []
[extra]
id = 9414
[taxonomies]
categories = []
tags = []
+++

{{implementation|Brainfuck}}{{collection|RCBF}}
An implementation of a [[Brainfuck]] interpreter in [[Brat]].

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

