+++
title = "Execute Brainfuck/Icon"
description = ""
date = 2016-10-06T08:35:06Z
aliases = []
[extra]
id = 7416
[taxonomies]
categories = []
tags = []
+++

{{implementation|Brainfuck}}{{collection|RCBF}}
This version of BF takes program and input from the parameter list.
Memory grows as needed from a single cell.
If no program is specified, the hello world program is run.

```txt
BF programtext inputtext
```



```Icon
#
# bf interpreter
#
procedure main(arglist)

prog := get(arglist)

# test for nested brackets, previous version was deemed incorrect
if \prog == "hello" then
   prog := ">++++++++[<+++++++++>-]<.>>+>+>++>[-]+<[>[->+<<++++>" ||
           "]<<]>.+++++++..+++.>>+++++++.<<<[[-]<[-]>]<+++++++++" ||
           "++++++.>>.+++.------.--------.>>+.>++++."

# default program
/prog := "++++++++++[>+>+++>++++>+++++++>++++++++>+++++++++>++" ||
         "++++++++>+++++++++++>++++++++++++<<<<<<<<<-]>>>>+.>>>" ||
         ">+..<.<++++++++.>>>+.<<+.<<<<++++.<++.>>>+++++++.>>>.+++." ||
         "<+++++++.--------.<<<<<+.<+++.---."

input := get(arglist)
/input := ""

# 1. toss obvious errors

b := 0
prog ? while i := move(1) do {
   case i of {
      "[" : b +:= 1
      "]" : b -:= 1
      }
   if b < 0 then stop("malformed program, unbalanced []")
   }
if b > 0 then stop("malformed program, unbalanced []")

write("Program is well formed.")
write("Program=",image(prog))
write("Ruler  =",image(repl("0123456789",
      *image(prog)/10 + 1)[1:*image(prog)-1]))

# 2. execute

cell := 1
mem  := [0]
nest := 0

prog ? while i := move(1) do {

   case i of {

      # increment the pointer (to point to the next cell to the right)
      ">" :
            if ( cell +:= 1 ) > *mem then
               put(mem, 0)

      # decrement the pointer (to point to the next cell to the left)
      "<" :
            if ( cell -:= 1 ) < 1 then
               runerr(205,cell)

      # increment (increase by one) the byte at the pointer.
      "+" : mem[cell] +:= 1

      # decrement (decrease by one) the byte at the pointer.
      "-" : mem[cell] -:= 1

      # output the value of the byte at the pointer.
      "." : writes(char(mem[cell]))

      # accept one byte of input, storing its value in the byte at the pointer.
      "," : input ?:= ( mem[cell] := move(1), tab(0) )

      # jump forward to the command after the corresponding ]
      # if the byte at the pointer is zero.
      "[" : if mem[cell] = 0 then {
            repeat {
               i := move(1)
               if i == "[" then nest +:= 1 & next
               if i == "]" then if nest = 0 then break else nest -:= 1
               }
            }

      # jump back to the command after the corresponding [
      # if the byte at the pointer is nonzero.
      "]" : if mem[cell] ~= 0 then {
            move(-1)
            repeat {
               i := move(-1)
               if i == "]" then nest +:= 1 & next
               if i == "[" then if nest = 0 then break else nest -:= 1
               }
            move(1)
            }
      }
      # everything else is ignored/comment
   }
end
```


Sample output:

```txt
#BF
Program is well formed.
Program="++++++++++[>+>+++>++++>+++++++>++++++++>+++++++++>++++++++++>+++++++++++>++++++++++++<<<<<<<<<-]>>>>+.>>>>+..<.<++++++++.>>>+.<<+.<<<<++++.<++.>>>+++++++.>>>.+++.<+++++++.--------.<<<<<+.<+++.---."
Ruler  ="0123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345"
Goodbye, World!

#BF hello
Program is well formed.
Program=">++++++++[<+++++++++>-]<.>>+>+>++>[-]+<[>[->+<<++++>]<<]>.+++++++..+++.>>+++++++.<<<[[-]<[-]>]<+++++++++++++++.>>.+++.------.--------.>>+.>++++."
Ruler  ="012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123"
Hello World!

```

