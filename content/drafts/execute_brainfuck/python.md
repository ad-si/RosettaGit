+++
title = "Execute Brainfuck/Python"
description = ""
date = 2016-08-30T21:57:40Z
aliases = []
[extra]
id = 3288
[taxonomies]
categories = []
tags = []
+++

{{implementation|Brainfuck}}{{collection|RCBF}}

Implementation of a [[Brainfuck]] interpreter in [[Python]].


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

