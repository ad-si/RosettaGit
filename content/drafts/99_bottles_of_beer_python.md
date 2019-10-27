+++
title = "99 Bottles of Beer/Python"
description = ""
date = 2015-02-21T13:54:50Z
aliases = []
[extra]
id = 18269
[taxonomies]
categories = []
tags = []
+++

{{collection|99 Bottles of Beer}} [[implementation of task::99 Bottles of Beer| ]]
[[99 Bottles of Beer]] done in Python.


## Python



### Normal Code


```python
def sing(b, end):
    print(b or 'No more','bottle'+('s' if b-1 else ''), end)

for i in range(99, 0, -1):
    sing(i, 'of beer on the wall,')
    sing(i, 'of beer,')
    print('Take one down, pass it around,')
    sing(i-1, 'of beer on the wall.\n')
```



### Using a template


```python
verse = '''\
%i bottles of beer on the wall
%i bottles of beer
Take one down, pass it around
%i bottles of beer on the wall
'''

for bottles in range(99,0,-1):
    print verse % (bottles, bottles, bottles-1) 
```


===New-style template (Python 2.6)===

```python
verse = '''\
{some} bottles of beer on the wall
{some} bottles of beer
Take one down, pass it around
{less} bottles of beer on the wall
'''

for bottles in range(99,0,-1):
    print verse.format(some=bottles, less=bottles-1) 
```


==="Clever" generator expression===

```python
a, b, c, s = " bottles of beer", " on the wall\n", "Take one down, pass it around\n", str
print "\n".join(s(x)+a+b+s(x)+a+"\n"+c+s(x-1)+a+b for x in xrange(99, 0, -1))
```


===Enhanced "Clever" generator expression using lambda===

```python
a = lambda n: "%u bottle%s of beer on the wall\n" % (n, "s"[n==1:])
print "\n".join(a(x)+a(x)[:-13]+"\nTake one down, pass it around\n"+a(x-1) for x in xrange(99, 0, -1))
```


===Using a generator expression (Python 3)===

```python
#!/usr/bin/env python3
"""\
{0} {2} of beer on the wall
{0} {2} of beer
Take one down, pass it around
{1} {3} of beer on the wall
"""
print("\n".join(
    __doc__.format(
        i, i - 1,
        "bottle" if i == 1 else "bottles",
        "bottle" if i - 1 == 1 else "bottles"
    ) for i in range(99, 0, -1)
), end="")
```



### A wordy version


```python
ones = (
'', 'one', 'two', 'three', 'four', 'five', 'six', 'seven', 'eight', 'nine'
)
prefixes = ('thir', 'four', 'fif', 'six', 'seven', 'eigh', 'nine')
tens = ['', '', 'twenty' ]
teens = ['ten', 'eleven', 'twelve']
for prefix in prefixes:
    tens.append(prefix + 'ty')
    teens.append(prefix +'teen')
tens[4] = 'forty'

def number(num): 
    "get the wordy version of a number"
    ten, one = divmod(num, 10)
    if ten == 0 and one == 0:
        return 'no'
    elif ten == 0:
        return ones[one]
    elif ten == 1:
        return teens[one]
    elif one == 0:
        return tens[ten]
    else:
        return "%s-%s" % (tens[ten], ones[one])

def bottles(beer):
    "our rephrase"
    return "%s bottle%s of beer" % ( 
            number(beer).capitalize(), 's' if beer > 1 else ''
    )

onthewall = 'on the wall'
takeonedown = 'Take one down, pass it around'
for beer in range(99, 0, -1): 
    print bottles(beer), onthewall
    print bottles(beer)
    print takeonedown
    print bottles(beer-1), onthewall
    print
```



### String Formatting


```python
for n in xrange(99, 0, -1):
    ##  The formatting performs a conditional check on the variable.
    ##  If it formats the first open for False, and the second for True
    print n, 'bottle%s of beer on the the wall.' % ('s', '')[n == 1]
    print n, 'bottle%s of beer.' % ('s', '')[n == 1]
    print 'Take one down, pass it around.'
    print n - 1, 'bottle%s of beer on the wall.\n' % ('s', '')[n - 1 == 1]
```

