+++
title = "Python"
description = ""
date = 2018-04-09T03:22:33Z
aliases = []
[extra]
id = 1696
website = "http://www.python.org"
[taxonomies]
categories = []
tags = ["garbage-collected", "interpreted", "dynamic", "object-oriented"]
+++

From the official [http://www.python.org Python] website:

> Python is a programming language that lets you work more quickly
> and integrate your systems more effectively.
> You can learn to use Python and see almost immediate gains in productivity
> and lower maintenance costs."

It is easy to create clean bug-free programs in Python due to the motto:
"Errors should never pass silently."
Python is an [interpreter](https://en.wikipedia.org/wiki/Interpreter_(computing)).
Python source files (.py files) are typically compiled
to an intermediate [bytecode](https://rosettacode.org/wiki/bytecode) language (.pyc files)
and executed by a Python Virtual Machine.


### Notes

Some Python examples may deviate from idiomatic Python
because they may be written to work
in Python 3.X as well as Python 2.X environments.
This includes doing things like:

- Using brackets in print statements/functions of one expression.
- Using zip and not izip; keys(), values(), items() and not their iter- forms.
- Checking for raw_input and setting raw_input to input if not found.
- Conditionally importing reduce if it is not found.

This style is not a requirement for Python code on RosettaGit,
but it may be in use
and should not necessarily be 'corrected' if found in examples.

The command line is often used in Python for short pieces of code,
and so, again; examples showing such use are not incorrect and may be left.


## See Also

- [Wikipedia: Python](https://en.wikipedia.org/wiki/python_(programming_language))


## Todo

[Reports:Tasks_not_implemented_in_Python](https://rosettacode.org/wiki/Reports:Tasks_not_implemented_in_Python)


## Merged content




Implementation of a [Brainfuck](https://rosettacode.org/wiki/Brainfuck) interpreter in [Python](https://rosettacode.org/wiki/Python).


```python
#!/usr/bin/python
import sys
import collections
#from collections import defaultdict instead since this only uses defaultdict?

def brainfuck (fd=None):
    fd = fd or (open(sys.argv[1]) if sys.argv[1:] else sys.stdin)
    source = fd.read()
    loop_ptrs = {}
    loop_stack = []
    for ptr, opcode in enumerate(source):
        if opcode == '[': loop_stack.append(ptr)
        if opcode == ']':
            if not loop_stack:
                source = source[:ptr]
                break
            sptr = loop_stack.pop()
            loop_ptrs[ptr], loop_ptrs[sptr] = sptr, ptr
    if loop_stack:
        raise SyntaxError ("unclosed loops at {}".format(loop_stack))
    tape = collections.defaultdict(int)
    cell = 0
    ptr = 0
    while ptr < len(source):
        opcode = source[ptr]
        if   opcode == '>': cell += 1
        elif opcode == '<': cell -= 1
        elif opcode == '+': tape[cell] += 1
        elif opcode == '-': tape[cell] -= 1
        elif opcode == ',': tape[cell] = ord(sys.stdin.read(1))
        elif opcode == '.': sys.stdout.write(chr(tape[cell]))
        elif (opcode == '[' and not tape[cell]) or \
             (opcode == ']' and tape[cell]): ptr = loop_ptrs[ptr]
        ptr += 1

if __name__ == "__main__": brainfuck()
```

