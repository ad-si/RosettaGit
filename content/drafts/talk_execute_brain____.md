+++
title = "Talk:Execute Brainfuck"
description = ""
date = 2016-07-22T03:52:56Z
aliases = []
[extra]
id = 9304
[taxonomies]
categories = []
tags = []
+++

How about [http://esolangs.org/wiki/Brainfuck#Self-Interpreters imterpreters of Brainfuck on Brainfuck] ?
:You're welcome to bring examples in from there as long as you check for license compatibility and probably give credit. --[[User:Mwn3d|Mwn3d]] 13:21, 1 March 2011 (UTC)
:: The place for something like that would be [[Execute Brainfuck/Brainfuck]]. I favor reimplementation over wholesale copying, and the copy case needs appropriate citation and license from the copyright holder. --[[User:Short Circuit|Michael Mol]] 13:47, 1 March 2011 (UTC)
Hey, the author of the Potion example here. Sorry that my example was erroneous -- I had only tested it on a basic hello-world program (!). Anyways, Potion might have some bugs that affects the solution, but clearly a part of the error lies in the logic, as a Python translation also doesn't seem to work as desired. Here is the Python translation:


```python
import sys  # for exception raising

def bf(code):
   tape = [0]
   tape_pos = 0
   brackets = []
   i = 0
   while i < len(code):
      if code[i] == ">":
         tape_pos += 1
         if tape_pos == len(tape):
            tape.append(0)
      elif code[i] == "<":
         tape_pos -= 1
         if tape_pos < 0:
            raise IndexError("""
               Out of bounds.
               Can't go to negative indexes of tape.
               Tape indexes are defined as [0, inf) and not as (-inf, inf).
               """)
      elif code[i] == "+":
         tape[tape_pos] += 1
      elif code[i] == "-":
         tape[tape_pos] -= 1
      elif code[i] == ".":
         print(chr(tape[tape_pos]), end="")
      elif code[i] == ",":
         tape[tape_pos] = ord(sys.stdin.read(1))
      elif code[i] == "[":
         brackets.append(i)
      elif code[i] == "]":
         if tape[tape_pos] == 0:
            brackets.pop()
         else:
            i = brackets[-1]
      i += 1
```


And with debugging/interpreter-portability in mind:

```python
import sys  # for exception raising

MIN = 0
MAX = 255
WRAP = True
TWOWAY = False

def bf(code):
   tape = [0]
   tape_pos = 0
   brackets = []
   i = 0
   while i < len(code):
      if code[i] == ">":
         tape_pos += 1
         if tape_pos == len(tape):
            tape.append(0)
      elif code[i] == "<":
         tape_pos -= 1
         if tape_pos < 0:
            if TWOWAY:
               tape.insert(0, 0)  # Kinda ugly hack.
               tape_pos = 0  # Kinda ugly hack.
            else:
               raise IndexError("""
                  Out of bounds.
                  Can't go to negative indexes of tape.
                  Tape indexes are defined as [0, inf) and not as (-inf, inf).
                  """)
      elif code[i] == "+":
         tape[tape_pos] += 1
         if tape[tape_pos] > MAX:
            if WRAP:
               tape[tape_pos] = MIN
            else:
               raise OverflowError("""
                  Overflow -- cell value exceeded `MAX`.
                  tape[tape_pos] > MAX;
                  tape[tape_pos]=={0};
                  tape_pos=={1};
                  code_pos=={2}
                  """.format(tape[tape_pos], tape_pos, i))
      elif code[i] == "-":
         tape[tape_pos] -= 1
         if tape[tape_pos] < MIN:
            if WRAP:
               tape[tape_pos] = MAX
            else:
               raise OverflowError("""
                  Underflow -- cell value below `MIN`.
                  tape[tape_pos] < MIN;
                  tape[tape_pos]=={0};
                  tape_pos=={1};
                  code_pos=={2}""".format(tape[tape_pos], tape_pos, i))
      elif code[i] == ".":
         print(chr(tape[tape_pos]), end="")
      elif code[i] == ",":
         tape[tape_pos] = ord(sys.stdin.read(1))
      elif code[i] == "[":
         brackets.append(i)
      elif code[i] == "]":
         if tape[tape_pos] == 0:

            brackets.pop()
         else:
            i = brackets[-1]
      i += 1
```

Can someone point out the error? :P [[User:Raigenbauf|Raigenbauf]] ([[User talk:Raigenbauf|talk]]) 01:00, 26 May 2016 (UTC)
: To quote the main page "<code>[</code>	Jump past the matching ] if the cell under the pointer is 0".
: The brackets <code>[ ]</code> provide a <code>while not zero do ...</code> loop not a <code>repeat ... until zero</code>
: [[User:Rdebath|Rdebath]] ([[User talk:Rdebath|talk]]) 03:52, 22 July 2016 (UTC)
