+++
title = "Icon"
description = ""
date = 2011-12-17T17:33:09Z
aliases = []
[extra]
id = 3235
[taxonomies]
categories = []
tags = []
+++
Icon is a descendant of [SNOBOL4](https://rosettacode.org/wiki/SNOBOL4) incorporating similar excellent string processing capabilities, built-in hash tables, success/failure based controls, dynamic storage, automatic type casting and coercion of values.  Icon added more conventional procedural syntax, generator expressions, goal-directed evaluation that automatically searches for successful results.  String scanning and matching superseded the pattern matching functionality of SNOBOL4.

Several extensions of Icon were developed to improve functionality.  Many of these were unified with [Unicon](https://rosettacode.org/wiki/Unicon)

## Related/Variants
* [Jcon](https://rosettacode.org/wiki/Jcon)
* [ObjectIcon](https://rosettacode.org/wiki/ObjectIcon)
* [Unicon](https://rosettacode.org/wiki/Unicon)

## See Also
* [Wikipedia: Icon](https://en.wikipedia.org/wiki/Icon_(programming_language))
* [http://www.cs.arizona.edu/icon/index.htm Icon homepage]
* [Wikipedia: Unicon](https://en.wikipedia.org/wiki/Unicon_(programming_language))
* [http://www.cs.arizona.edu/icon/ftp/doc/lb1up.pdf Book: The Icon Programming Language, 3rd Edition]
* [http://www.cs.arizona.edu/icon/ftp/doc/gb1up.pdf Book: Graphics Programming in Icon]
* [http://www2.cs.uidaho.edu/~jeffery/icon/humanists/humanist.pdf Book: Icon Programming for Humanists]
* [http://www.mitchellsoftwareengineering.com/icon/icon.sli.pdf Presentation: Fundamentals of Icon Programming]
* [http://www.cs.arizona.edu/icon/inl/inl.htm Archived issues of the Icon Newsletter from 1979-2000 discussing Icon programming]
* [http://www.cs.arizona.edu/icon/analyst/ia.htm Archived issues of the Icon Analysts from 1990-2001 discussing advanced Icon programming]
* [http://www.cs.arizona.edu/icon/library/ipl.htm The Icon Programming Library and index]

## Wiki Links
* [An introduction to Icon and Unicon for Rosetta Code](https://rosettacode.org/wiki/Icon%2BUnicon/Intro)
* [Unimplementable tasks](https://rosettacode.org/wiki/:Category:Icon/Omit)
* [Tasks requiring attention](https://rosettacode.org/wiki/:Category:Icon_examples_needing_attention)
* [Tasks not yet implemented](https://rosettacode.org/wiki/Reports:Tasks_not_implemented_in_Icon)
* [The Icon Programming Library](https://rosettacode.org/wiki/:Category:Icon_Programming_Library)


## Merged content



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

