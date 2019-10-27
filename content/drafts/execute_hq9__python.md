+++
title = "Execute HQ9+/Python"
description = ""
date = 2010-02-06T14:30:21Z
aliases = []
[extra]
id = 4560
[taxonomies]
categories = []
tags = []
+++

{{collection|RCHQ9+}}{{implementation|HQ9+}}

This [[Python]] example implements 'H', 'Q', '9' and '+'.


```python
import sys

def hello():
    print "Hello, world!"

def quine():
    print src

def bottles():    
    for i in range(99,2,-1):       
        print "%d bottles of beer on the wall" % i
        print "%d bottles of beer" % i
        print "Take one down, pass it around"
        print "%d bottles of beer on the wall" % (i-1)
        print

    print "2 bottles of beer on the wall"
    print "2 bottles of beer"
    print "Take one down, pass it around"
    print "1 bottle of beer on the wall"
    print

    print "1 bottle of beer on the wall"
    print "1 bottle of beer"
    print "Take one down, pass it around"
    print "No more bottles of beer on the wall"
    print

    print "No more bottles of beer on the wall"
    print "No more bottles of beer on the wall"
    print "Go to the store and buy some more"
    print "99 bottles of beer on the wall."
    print 

def incr():
    acc +=1

if len(sys.argv) != 2:
    print "Usage: ./hq9p.py script.hq9"
    sys.exit(1)
else:
    f = sys.argv[1]

try:
    s = open(f,"r")
except IOError, e:
    print "Can't open file: " + str(e.args[1])
    sys.exit(1)

acc = 0
src = s.read()

# Implement interpreter using a dispatch table
dispatch = {
     'h': hello,
     'q': quine,
     '9': bottles,
     '+': incr
     }

for i in src.lower():
    if i in dispatch:
        dispatch[i]()
