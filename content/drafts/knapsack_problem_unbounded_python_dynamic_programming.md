+++
title = "Knapsack problem/Unbounded/Python dynamic programming"
description = ""
date = 2019-08-24T20:28:33Z
aliases = []
[extra]
id = 6181
[taxonomies]
categories = []
tags = []
+++

{{collection|Knapsack problem/Unbounded}}

==Dynamic Programming Solution==
This solution trades off having to search over all possible combinations of items by having to enumerate over all possible sizes of (weight, volume) for each item. The example builds in complexity to the final DP program.

===Brute force, single size attribute===
A brute-force solution for items with only one 'size' attribute would look like the following and would not scale:

```python
from operator import itemgetter as iget
from itertools import product
from random import shuffle

NAME, SIZE, VALUE = range(3)
items = (
    # NAME, SIZE, VALUE
    ('A', 3, 2),
    ('B', 5, 4),
    ('C', 7, 6),
    ('D', 9, 8) )
capacity = 8

def knapsack_unbounded_enumeration(items, C):

    # find max of any one item
    max1 = [ int(C/item[SIZE]) for item in items ]
    itemsizes  = [ item[SIZE]  for item in items ]
    itemvalues = [ item[VALUE] for item in items ]

    #def totvalue(itemscount, =itemsizes, itemvalues=itemvalues, C=C):
    def totvalue(itemscount):
        nonlocal itemsizes, itemvalues, C
        
        totsize = sum( n*size for n, size in zip(itemscount, itemsizes) )
        totval  = sum( n*val  for n, val  in zip(itemscount, itemvalues) )
        
        return (totval, -totsize) if totsize <= C else (-1, 0)

    # Try all combinations of bounty items from 0 up to max1
    bagged = max( product(*[range(n+1) for n in max1]), key = totvalue )
    numbagged = sum(bagged)
    value, size = totvalue(bagged)
    size = -size
    # convert to (iten, count) pairs) in name order
    bagged = sorted((items[i][NAME], n) for i,n in enumerate(bagged) if n)

    return value, size, numbagged, bagged

```


===DP, single size dimension===
The dynamic programming version where 'size' has only one dimension would be the following and produces an optimal solution:

```python
def knapsack_unbounded_dp(items, C):
    # order by max value per item size
    items = sorted(items, key=lambda item: item[VALUE]/float(item[SIZE]), reverse=True)
    
    # Sack keeps track of max value so far as well as the count of each item in the sack
    sack = [(0, [0 for i in items]) for i in range(0, C+1)]   # value, [item counts]
    
    for i,item in enumerate(items):
        name, size, value = item
        for c in range(size, C+1):
            sackwithout = sack[c-size]  # previous max sack to try adding this item to
            trial = sackwithout[0] + value
            used = sackwithout[1][i]
            if sack[c][0] < trial:
                # old max sack with this added item is better
                sack[c] = (trial, sackwithout[1][:])
                sack[c][1][i] +=1   # use one more

    value, bagged = sack[C]
    numbagged = sum(bagged)
    size = sum(items[i][1]*n for i,n in enumerate(bagged))
    # convert to (iten, count) pairs) in name order
    bagged = sorted((items[i][NAME], n) for i,n in enumerate(bagged) if n)

    return value, size, numbagged, bagged
```


===DP, multiple size dimensions===
Our original problem has two dimensions to 'size': weight and volume. We can create a python size object, that knows how to enumerate itself over its given dimensions, as well as perform logical and simple mathematical operations. With the use of the Size object, a correct solution to the given unbounded knapsack problem can be found by the following proceedure:

```python
from knapsack_sizer import makesize

Size = makesize('wt vol')

items = [
    # NAME, (WT, VOL), VALUE   
    ('panacea', Size(3, 25), 3000),
    ('ichor', Size(2, 15), 1800),
    ('gold', Size(20, 2), 2500) ]
capacity = Size(250, 250)

def knapsack_unboundedmulti_dp(items, C):
    # order by max value per item size
    items = sorted(items, key=lambda item: item[VALUE]/abs(item[SIZE]), reverse=True)
    
    # Sack keeps track of max value so far as well as the count of each item in the sack
    zero, one = tuple(zip(*((0,1) for i in C)))
    sack = dict( (i, (0, [0 for i in items]))   # size -> (value, [item counts])
                 for i in C.range(C+one) )
    
    for i,item in enumerate(items):
        name, size, value = item
        for c in C.range(size, C+one):
            sackwithout = sack[c-size]  # previous max sack to try adding this item to
            trial = sackwithout[0] + value
            used = sackwithout[1][i]
            if sack[c][0] < trial:
                # old max sack with this added item is better
                sack[c] = (trial, sackwithout[1][:])
                sack[c][1][i] +=1   # use one more

    value, bagged = sack[C]
    numbagged = sum(bagged)
    size = sum((items[i][1]*n for i,n in enumerate(bagged)), zero)
    # convert to (iten, count) pairs) in name order
    bagged = sorted((items[i][NAME], n) for i,n in enumerate(bagged) if n)

    return value, size, numbagged, bagged

dp = knapsack_unboundedmulti_dp(items, capacity)
print(capacity, dp)
```



### Sample output

The solution found is printed as:

```txt
Size(wt=250, vol=250) (54500, Size(247, 247), 20, [('gold', 11), ('panacea', 9)])
```

I.e. a choice of 20 items: 11 gold and 9 panacea, for a total value of 54500.


### Ancillary module

An ancillary file called knapsack_sizer.py must be made available on your PYTHONPATH with the following contents:

```python
from operator import itemgetter as _itemgetter
from collections import namedtuple as _namedtuple
from math import sqrt as _sqrt
from itertools import product as _product
from numbers import Number as _Number

_tuple = tuple

def makesize(dimensionnames, typename='Size'):
    '''
    Return Size, an extended namedtuple that represents a container
    of N integer independent dimensions, e.g. weight and volume
    a dimension cannot be less than zero and will instead force all
    dimensions to zero.

    Some tests
    
    >>> Size = makesize('wt vol')
    >>> Size(wt=1, vol=2)
    Size(1, 2)
    >>> Size(1, vol=2)
    Size(1, 2)
    >>> Size(vol=2, wt=1)
    Size(1, 2)
    
    >>> x,y = Size(*[1,2]), Size(*[3,4])
    >>> x
    Size(1, 2)
    >>> y
    Size(3, 4)
    >>> str(x)
    'Size(wt=1, vol=2)'
    >>> str(y)
    'Size(wt=3, vol=4)'
    >>> Size(*[1,-2])
    Size(0, 0)
    >>> Size(*[-1,2])
    Size(0, 0)
    >>> x
    Size(1, 2)
    >>> x.wt
    1
    >>> x.vol
    2
    >>> x[0]
    1
    >>> x[1]
    2
    >>> x+[1,1]
    Size(2, 3)
    >>> (1,1)+x
    Size(2, 3)
    >>> [1,1]+x
    Size(2, 3)
    >>> x+(1,1)
    Size(2, 3)
    >>> x-[1,1]
    Size(0, 1)
    >>> [2,4]-x
    Size(1, 2)
    >>> (2,4)-x
    Size(1, 2)

    >>> # Comparisons

    >>> Size = makesize('wt vol')
    >>> for s in ((i,j) for i in (0,1,2) for j in (1,2,3)):
    ...         for op in '== != < <= >= >'.split():
    ...                 eqn = '%s %2s %r' % (str(s), op, Size(*[1,2]))
    ...                 print("%25r" % eqn, '=', eval(eqn))
    ...
    ...                 
       '(0, 1) == Size(1, 2)' = False
       '(0, 1) != Size(1, 2)' = True
       '(0, 1)  < Size(1, 2)' = True
       '(0, 1) <= Size(1, 2)' = True
       '(0, 1) >= Size(1, 2)' = False
       '(0, 1)  > Size(1, 2)' = False
       '(0, 2) == Size(1, 2)' = False
       '(0, 2) != Size(1, 2)' = True
       '(0, 2)  < Size(1, 2)' = True
       '(0, 2) <= Size(1, 2)' = True
       '(0, 2) >= Size(1, 2)' = False
       '(0, 2)  > Size(1, 2)' = False
       '(0, 3) == Size(1, 2)' = False
       '(0, 3) != Size(1, 2)' = True
       '(0, 3)  < Size(1, 2)' = False
       '(0, 3) <= Size(1, 2)' = False
       '(0, 3) >= Size(1, 2)' = False
       '(0, 3)  > Size(1, 2)' = False
       '(1, 1) == Size(1, 2)' = False
       '(1, 1) != Size(1, 2)' = True
       '(1, 1)  < Size(1, 2)' = True
       '(1, 1) <= Size(1, 2)' = True
       '(1, 1) >= Size(1, 2)' = False
       '(1, 1)  > Size(1, 2)' = False
       '(1, 2) == Size(1, 2)' = True
       '(1, 2) != Size(1, 2)' = False
       '(1, 2)  < Size(1, 2)' = False
       '(1, 2) <= Size(1, 2)' = True
       '(1, 2) >= Size(1, 2)' = True
       '(1, 2)  > Size(1, 2)' = False
       '(1, 3) == Size(1, 2)' = False
       '(1, 3) != Size(1, 2)' = True
       '(1, 3)  < Size(1, 2)' = False
       '(1, 3) <= Size(1, 2)' = False
       '(1, 3) >= Size(1, 2)' = True
       '(1, 3)  > Size(1, 2)' = True
       '(2, 1) == Size(1, 2)' = False
       '(2, 1) != Size(1, 2)' = True
       '(2, 1)  < Size(1, 2)' = False
       '(2, 1) <= Size(1, 2)' = False
       '(2, 1) >= Size(1, 2)' = False
       '(2, 1)  > Size(1, 2)' = False
       '(2, 2) == Size(1, 2)' = False
       '(2, 2) != Size(1, 2)' = True
       '(2, 2)  < Size(1, 2)' = False
       '(2, 2) <= Size(1, 2)' = False
       '(2, 2) >= Size(1, 2)' = True
       '(2, 2)  > Size(1, 2)' = True
       '(2, 3) == Size(1, 2)' = False
       '(2, 3) != Size(1, 2)' = True
       '(2, 3)  < Size(1, 2)' = False
       '(2, 3) <= Size(1, 2)' = False
       '(2, 3) >= Size(1, 2)' = True
       '(2, 3)  > Size(1, 2)' = True

    >>> # Same comparison answers with lists on LHS
    >>> cmplst = [eval('%s %2s %r' % (str(s), op, Size(*[1,2])))  for s in ([i,j] for i in (0,1,2) for j in (1,2,3))  for op in '== != < <= >= >'.split() ]
    >>> # Same comparison answers with tuples on LHS
    >>> cmptpl = [eval('%s %2s %r' % (str(s), op, Size(*[1,2])))  for s in ((i,j) for i in (0,1,2) for j in (1,2,3))  for op in '== != < <= >= >'.split() ]
    >>> assert cmplst == cmptpl
    >>> 

    '''
    
    tmp = _namedtuple(typename, dimensionnames)
    _fields = tmp._fields
    class Size(tmp):
        __doc__ = tmp.__doc__ + '\n\n' + makesize.__doc__ 

        __slots__ = () 

        def __new__(_cls, *args0, **kwargs):
            nonlocal tmp
            a = []
            lenargs0 = len(args0)
            if lenargs0> len(tmp._fields):
                raise ValueError('Got unexpected extra %d fields' % (lenargs0 - len(tmp._fields),))
            for i,name in enumerate(tmp._fields):
                if i<lenargs0:
                    a.append(args0[i])
                    if name in kwargs:
                        raise ValueError('Argument given twice: %r' % name)
                elif name in kwargs:
                    a.append(kwargs.pop(name))
                else:
                    raise ValueError('Missing field name: %r' % name)
            if kwargs:
                raise ValueError('Got unexpected field names: %r' % kwargs.keys())
            args = a
            if any(arg<0 for arg in args): args = tuple(0 for arg in args)
            return _tuple.__new__(_cls, args) 

        @classmethod
        def _make(cls, iterable, new=tuple.__new__, len=len):
            'Make a new Size object from a sequence or iterable'
            args = tuple(iterable)
            if any(arg<0 for arg in args): args = tuple(0 for arg in args)
            result = new(cls, args)
            if len(result) != len(cls._fields):
                raise TypeError('Expected %d arguments, got %d' % (len(cls._fields),
                                                                   len(result)))
            return result 

        def range(_self, start, stop=None):
            '''
            range([start,] stop) -> itertools.product object
            Returns an iterator that generates the sizes in the range on demand as tuples.
            
            '''
            if stop is None:
                stop = start
                start = tuple(0 for i in _self)
            if len(_self) != len(start):
                raise TypeError('Expected %d elements in start, got %d' % (len(_self), len(start)))
            if len(_self) != len(stop):
                raise TypeError('Expected %d elements in stop, got %d' % (len(_self), len(stop)))
            return _product(*(range(mn,mx) for mn,mx in zip(start, stop)))

        __hash__ = _tuple.__hash__

        def __mul__(_self, y):
            'Return a new Size object where corresponding elements are multiplied by y'
            if isinstance(y, _Number):
                return _self._make( x*y for x in _self)
            elif len(_self) == len(y):
                return _self._make( ex*ey for ex,ey in zip(_self, y))
            else:
                raise NotImplementedError('Expected a Number or a %d element tuple/list, got %r' % (
                    len(_self), y))
        def __truediv__(_self, y):
            'Return a new Size object where corresponding elements are divided by y'
            if isinstance(y, _Number):
                return _self._make( x/y for x in _self)
            elif len(_self) == len(y):
                return _self._make( ex/ey for ex,ey in zip(_self, y))
            else:
                raise NotImplementedError('Expected a Number or a %d element tuple/list, got %r' % (
                    len(_self), y))

        def __repr__(self):
            return 'Size(' + ', '.join('%r' % i for i in self) + ')' 
        def __str__(self):
            return 'Size(' + ', '.join('%s=%r' % i for i in zip(self._fields, self)) + ')' 

        def __abs__(_self):
            'Return the sqrt of the sum of the squares of all elements'
            return _sqrt(sum(e*e for e in _self))

        def __add__(_self, y):
            'Return a new Size object where corresponding elements are added'
            if len(_self) != len(y):
                raise TypeError('Expected %d arguments, got %d' % (len(_self), len(y)))
            return _self._make( x+y for x,y in zip(_self, y))
        def __radd__(_self, y):
            'Return a new Size object where corresponding elements are added'
            if len(_self) != len(y):
                raise TypeError('Expected %d arguments, got %d' % (len(_self), len(y)))
            return _self._make( y+x for x,y in zip(_self, y))

        def __sub__(_self, y):
            'Return a new Size object where corresponding elements are subtracted'
            if len(_self) != len(y):
                raise TypeError('Expected %d arguments, got %d' % (len(_self), len(y)))
            return _self._make( x-y for x,y in zip(_self, y))
        def __rsub__(_self, y):
            'Return a new Size object where corresponding elements are subtracted'
            if len(_self) != len(y):
                raise TypeError('Expected %d arguments, got %d' % (len(_self), len(y)))
            return _self._make( y-x for x,y in zip(_self, y))

        def __eq__(_self, y):
            'Return true iff all fields of self == y'
            if len(_self) != len(y):
                raise TypeError('Expected %d arguments, got %d' % (len(_self), len(y)))
            return all(x == y for x,y in zip(_self, y))
        def __ne__(_self, y):
            'Return true iff any fields of self != y'
            if len(_self) != len(y):
                raise TypeError('Expected %d arguments, got %d' % (len(_self), len(y)))
            return any(x != y for x,y in zip(_self, y))

        def __gt__(_self, y):
            'Return true iff all fields of self >= y and at least one field is > its field in y'
            if len(_self) != len(y):
                raise TypeError('Expected %d arguments, got %d' % (len(_self), len(y)))
            more = False
            for x,y in zip(_self, y):
                if x > y: more = True
                if x < y: break
            else:
                return more
            return False
        def __ge__(_self, y):
            'Return true iff all fields of self >= y'
            if len(_self) != len(y):
                raise TypeError('Expected %d arguments, got %d' % (len(_self), len(y)))
            return all(x >= y for x,y in zip(_self, y))
        def __lt__(_self, y):
            'Return true iff all fields of self <= y and at least one field is < its field in y'
            if len(_self) != len(y):
                raise TypeError('Expected %d arguments, got %d' % (len(_self), len(y)))
            more = False
            for x,y in zip(_self, y):
                if x < y: more = True
                if x > y: break
            else:
                return more
            return False
        def __le__(_self, y):
            'Return true iff all fields of self >= y'
            if len(_self) != len(y):
                raise TypeError('Expected %d arguments, got %d' % (len(_self), len(y)))
            return all(x <= y for x,y in zip(_self, y))

    return Size

if __name__ == '__main__':
    import doctest
    doctest.testmod(verbose=not True)    
    Size = makesize('wt vol')
    x,y = Size(*[1,2]), Size(*[3,4])

```

