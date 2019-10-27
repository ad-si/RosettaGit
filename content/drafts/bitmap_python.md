+++
title = "Bitmap/Python"
description = ""
date = 2013-08-07T20:39:11Z
aliases = []
[extra]
id = 5308
[taxonomies]
categories = []
tags = []
+++

{{collection|Basic bitmap storage}}

Translation of [[Basic bitmap storage#Ada|Ada]] (well, roughly)

{{works with|python|2.4}}

```Python
import sys # for stdout

class c_color(object):
	def __init__(self, p_r = 0, p_g = 0, p_b = 0):
		self.put(p_r, p_g, p_b)

	def clone(self):
		return c_color(self.r, self.g, self.b)

	def put(self, p_r, p_g, p_b):
		self.r = p_r
		self.g = p_g
		self.b = p_b

	def get_r(self):
		return self.r

	def get_g(self):
		return self.g

	def get_b(self):
		return self.b

	def __eq__(self, other):
		return self.r == other.r and self.g == other.g and self.b == other.b

	def __ne__(self, other):
		return not (self == other)

black = c_color(0,0,0)
white = c_color(255,255,255)

class c_bitmap_store(object):
    def __init__(self, p_width = 80, p_height = 40):
        self.instructions = ''
	self.width = p_width
	self.height = p_height
	self.pixels = [[white.clone() for x in range(self.width)] for y in range(self.height)]


    def fill(self, x, y, w, h, color):
        for I in range(w):
            for J in range(h):
	        self.put(x+I, y+J, color)
 
    def display(self):
        for J in range(self.height):
             for I in range(self.width):
		if self.get(i,j) == white:
			sys.stdout.write(" ")
		else:
			sys.stdout.write("H")
             print

    def put(self, x,y, color):
	    assert type(color) is c_color
	    self.pixels[y][x] = color

    def get(self, x,y):
	    result = self.pixels[y][x]
	    assert type(result) is c_color
	    return result
```



### Alternative version

{{works with|Python|3.1}}

This makes use of named tuples and may well work with Python 2.6+ too.

```python
from collections import namedtuple
from copy import copy

Colour = namedtuple('Colour','r,g,b')
Colour.copy = lambda self: copy(self)

black = Colour(0,0,0)
white = Colour(255,255,255) # Colour ranges are not enforced.

class Bitmap():
    def __init__(self, width = 40, height = 40, background=white):
        assert width > 0 and height > 0 and type(background) == Colour
        self.width = width
        self.height = height
        self.background = background
        self.map = [[background.copy() for w in range(width)] for h in range(height)]

    def fillrect(self, x, y, width, height, colour=black):
        assert x >= 0 and y >= 0 and width > 0 and height > 0 and type(colour) == Colour
        for h in range(height):
            for w in range(width):
                self.map[y+h][x+w] = colour.copy()

    def chardisplay(self):
        txt = [''.join(' ' if bit==self.background else '@'
                       for bit in row)
               for row in self.map]
        # Boxing
        txt = ['|'+row+'|' for row in txt]
        txt.insert(0, '+' + '-' * self.width + '+')
        txt.append('+' + '-' * self.width + '+')
        print('\n'.join(reversed(txt)))

    def set(self, x, y, colour=black):
        assert type(colour) == Colour
        self.map[y][x]=colour
 
    def get(self, x, y):
	    return self.map[y][x]
    

bitmap = Bitmap(20,10)
bitmap.fillrect(4, 5, 6, 3)
assert bitmap.get(5, 5) == black
assert bitmap.get(0, 1) == white
bitmap.set(0, 1, black)
assert bitmap.get(0, 1) == black
bitmap.chardisplay()

'''
The origin, 0,0; is the lower left, with x increasing to the right,
and Y increasing upwards.

The program above produces the following display :

+--------------------+
|                    |
|                    |
|    @@@@@@          |
|    @@@@@@          |
|    @@@@@@          |
|                    |
|                    |
|                    |
|@                   |
|                    |
+--------------------+

'''
```

