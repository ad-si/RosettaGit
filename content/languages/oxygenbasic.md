+++
title = "OxygenBasic"
description = ""
date = 2014-03-24T21:54:10Z
aliases = []
[extra]
id = 11888
[taxonomies]
categories = []
tags = []
+++

OxygenBasic is a compilable language in the [BASIC](https://rosettacode.org/wiki/BASIC) genre supporting [object-oriented programming](https://rosettacode.org/wiki/object-oriented_programming), and containing features of [C](https://rosettacode.org/wiki/C). Its earliest origins was as a macro [Assembly](https://rosettacode.org/wiki/Assembly) code language in 2009, but subsequently acquired all the features of a high-level language.

The philosophy underlying OxygenBasic is to facilitate clean coding, with low syntax noise and few coding restrictions. The core language and compiler size are kept to a minimum.

## Examples
'''Hello World:'''

```txt

print "Hello World!"

```


'''Iteration:'''

```txt

i=0
pr=""
for i=1 to 10
  pr+="Line: " i chr(13) chr(10)
next
print pr

```


'''Function:'''

```txt

function cube(double n) as double
  return d*d*d
end function

print cube(3)

```


'''Class:'''

```txt

class MemoryBank
  '
  string buf
  '
  method store(string s)
    buf += " " s chr(13)
  end method
  '
  method find(string s) as string
    sys a,b
    a=instr buf,s
    if a then
      b=instr a,s,chr(13)
      return mid buf,a,b-a
    end if
  end method
  '
  method clear()
    buf=""
  end method
  '
end class

MemoryBank b
b.store ("
shoes LC1
ships LC2
sealing wax LC3
cabbages LC4
kings LC5
")

print b.find "ships"

```


## Current Status
The current implementation is available for Microsoft Windows and includes an x86 assembler. It can compile directly to memory, or to 32 bit and 64 bit binaries. Development is currently in Alpha phase. The compiler is a single DLL, and suitable for embedding in other Applications. It has been deployed as a module in thinBasic to support Assembly code and dynamic compiling.

The latest versions can be obtained here, including an IDE and numerous examples:

* http://www.oxygenbasic.org
