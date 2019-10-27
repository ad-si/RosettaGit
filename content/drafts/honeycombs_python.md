+++
title = "Honeycombs/Python"
description = ""
date = 2014-12-26T02:52:59Z
aliases = []
[extra]
id = 11591
[taxonomies]
categories = []
tags = []
+++


```Python

#!/some/path/python3

'''
    This code proves mostly that I write the longest programs.
    I did not investigate a direct translation from the tcl version to python.
    The code includes doctests.  Suggested program use:

    $ python3 -m doctest hexagons.py

    Rotations support comes in name only.  A full blown 3D system
    with homogeneous coordinates seemed overboard.
'''

import time
import math
import pprint
import random
import string
import tkinter
import numbers

Tau = 2*math.pi  # pi is wrong. http://www.youtube.com/watch?v=jG7vhMMXagQ

cdr = lambda a: a[1:]
car = lambda a: a[0]

def flatten(a):
    '''
        provide lisp-style nested list flattening

        >>> flatten((((2,4),),[1,2,3],[[5,2,],88]))
        [2, 4, 1, 2, 3, 5, 2, 88]
    '''
    try:
        if not len(a):
            return []
    except:
        return [a]
    else:
        return flatten(car(a))+flatten(cdr(a))

class Base:

    '''
        Base provides default __init__ and __repr__ methods
        for simple classes.  The subclass includes a set of
        mandatory_arguments .  See examples.

        The logic---yes, I have a reason---for using Base:
        It has been said that using attribute names is
        "good style" making for "clearly written code" and so forth.

        The Base constructor REQUIRES keyword arguments when used.
        You can get around this using a factory function to wrap
        object construction.  (Because it is, in my opinion, also
        ridiculous to write the keywords over and over when you
        have many of the same objects/function to create/call.)
        See example in doctest:

        >>> Base(a=3)   # support doctest with command $ python3 -m doctest -v this.file
        Base(**{'a': 3})
        >>> f = lambda a: Base(a=a)
        >>> f(1)
        Base(**{'a': 1})
        >>> f('alpha')
        Base(**{'a': 'alpha'})
    '''

    class BaseClassException(Exception):
        pass

    mandatory_arguments = set()

    def __init__(self,**kwargs):
        mandatory_arguments = self.__class__.mandatory_arguments
        if not mandatory_arguments.issubset(kwargs):
            raise Base.BaseClassException(
                self.__class__.__name__ +
                ' requires keyword arguments ' +
                str(mandatory_arguments))
        self.kwargs = kwargs
        for kv in kwargs.items():
            setattr(self,*kv)

    def __repr__(self):
        return self.__class__.__name__+'(**'+pprint.pformat(self.kwargs)+')'

class Ngon(Base):

    '''
        >>> square = Ngon(n = 4, center = (0,0), radius = 1, rotation = 0,)
        >>> square.center
        (0, 0)
    '''

    mandatory_arguments = set('n center radius rotation'.split())

    def __init__(self,**args):
        self._coordinates = False
        super().__init__(**args)

    def __call__(self):
        if not self._coordinates:
            xi = self.rotation
            step = Tau/self.n
            self._coordinates = []
            (x0,y0,) = self.center
            r = self.radius
            for i in range(self.n):
                self._coordinates.append((r*math.cos(xi)+x0,r*math.sin(xi)+y0,))
                xi += step
        return self._coordinates

    @property
    def flat(self):
        return flatten(self())

    @property
    def inner_radius(self):
        return self.radius*math.sqrt(1**2-(1/2)**2)

class Hexagon(Ngon):

    '''
        >>> h = Hexagon(center = (1,0), radius = 1, rotation = 0,)
        >>> h.center
        (1, 0)
        >>> h.flat[2]
        1.5
    '''

    mandatory_arguments = set('center radius rotation'.split())

    def __init__(self,**args):
        args['n'] = 6
        super().__init__(**args)

class Vector(Base):

    '''
        Of course I could have used numpy.  scipy is unavailable
        from the standard library, so I wrote Vector.

        >>> P = Vector(P=(1,2,))
        >>> len(P)
        2
        >>> P[0]
        1
        >>> (P+P)[1]
        4
        >>> len(P+P)
        2
        >>> (P*3)[0]
        3
        >>> (P-P)[1]
        0
        >>> P.dot((2,3,))
        8
    '''

    mandatory_arguments = set('P')

    def __len__(self):
        return len(self.P)

    def __getitem__(self,ITEM):
        return self.P[ITEM]

    def __add__(a,b): # I find (self, other) silly for dyadic operator methods
        if a.__class__ != b.__class__:
            raise ValueError('Adding Vectors works.  You did something else.')
        if len(a) != len(b):
            raise ValueError('Incommensurate dimensionality')
        return Vector(P=tuple(a[i]+b[i] for i in range(len(a))))

    def __neg__(self):
        return self*(-1)

    def __sub__(a,b):
        return a+(-b)

    def __mul__(a,b):
        if not isinstance(b,numbers.Number):
            raise ValueError('not a dot or cross product, honey.  Scalars only')
        return Vector(P=tuple(p*b for p in a))

    def dot(a,b=None):
        b = b or a
        try:
            if (len(a) == len(b)) and isinstance(b[0],numbers.Number):
                return sum(A*B for (A,B,) in zip(a,b,))
        except:
            pass
        raise ValueError('Incommensurate lengths or types')

class EnhancedCanvas(tkinter.Canvas):

    def create_loop(self,*args,**kwargs):

        '''
            draw a poly-line including a connection between the first and last points.
        '''

        LOOP = tuple(args) + (args[0],args[1],)
        self.create_line(*LOOP,**kwargs)

class HoneyComb:

    def __init__(self,s,radius=20,rotation=0):
        self.comb(radius,rotation)
        tk = tkinter.Tk()
        tk.geometry('300x320')
        canvas = EnhancedCanvas(tk)
        canvas.bind('<Button>',self.button) # on mouse button, call the HoneyComb button method
        canvas.bind('<Key>',self.key) # on key event, call the HoneyComb key method
        canvas.pack(expand=True,fill=tkinter.BOTH,)
        canvas.focus_set()              # window must have focus to capture keys!
        self.SELECTED = [False,]*len(self.HEXAGONS)
        self.SELECTIONS = []
        self.canvas = canvas
        self.texts = s[:len(self.HEXAGONS)] # :-(  The set of letters in CUB SCOUTS might not be available.
        self.tk = tk
        self.paint()
        tk.mainloop()

    def paint(self):
        canvas = self.canvas
        s = self.texts
        for (I,H,) in enumerate(self.HEXAGONS):
            # use subtle color change.  Can you spell CUB SCOUT ?
            canvas.create_polygon(*H.flat,fill=('yellow','gold')[self.SELECTED[I]])
            canvas.create_text(*H.center,text=s[I],fill='blue')
        for H in self.HEXAGONS:
            canvas.create_loop(*H.flat,width=3)
        self.tk.update_idletasks()

    def repaint(self,I):
        SELECTED = self.SELECTED
        SELECTIONS = self.SELECTIONS
        SELECTED[I] = True
        SELECTIONS.append(I)
        self.paint()
        #code to display self.texts[I] fits here
        # could either pack a text box into tk and use that,
        # or erase previous character and post new char with create_text
        # using xor mode or color change directly onto the canvas.
        if all(SELECTED):               # finished
            print('sleep 2 seconds--->then gone')
            time.sleep(2)
            self.tk.destroy()

    def __call__(self):
        return ' '.join(self.texts[J] for J in self.SELECTIONS)

    def key(self,EVENT,):
        ''' key board activity trap comes to this function '''
        s = self.texts
        try:
            I = s.index(EVENT.char.upper())
        except ValueError:
            pass
        else:
            self.repaint(I)

    def button(self,EVENT,):
        ''' mouse button activity trap comes to this function '''
        # I don't recall how to use or if possible "nearest" or tagging create_... figures
        A = Vector(P=(EVENT.x,EVENT.y))
        (BEST, SHORTEST,) = (0, 9e44,)
        # could search the set of yet unchosen HEXAGONS
        # However, CUB SCOUTS has repeat letters.
        for (I,H,) in enumerate(self.HEXAGONS):
            # use dot product to stand in for the length of the vector
            # between the mouse event and the hexagon centers.
            L = (A-Vector(P=H.center)).dot()
            if L < SHORTEST:
                (BEST, SHORTEST,) = (I, L,)
        self.repaint(BEST)

    def comb(self,radius=20,rotation=0):
        C = Vector(P=(radius*1.25,radius*1.25,))
        H = Hexagon(center = C, radius = radius, rotation = rotation,)
        IR = H.inner_radius
        OFFSET = Vector(P=(0,IR*2,))
        HEXAGONS = [H,]
        for i in range(3):
            C += OFFSET
            HEXAGONS.append(Hexagon(center = C, radius = radius, rotation = rotation))
        OFFSET = Vector(P=((IR*2)*math.cos(Tau/(3*4)),(IR*2)*math.sin(Tau/(3*4))))
        for i in range(4):
            C = Vector(P=HEXAGONS[i].center)
            HEXAGONS.append(Hexagon(center = C+OFFSET, radius = radius, rotation = rotation))
        OFFSET = Vector(P=(OFFSET[0]*2,0))
        for i in range(8):
            C = Vector(P=HEXAGONS[i].center)
            HEXAGONS.append(Hexagon(center = C+OFFSET, radius = radius, rotation = rotation))
        for i in range(8,12):
            C = Vector(P=HEXAGONS[i].center)
            HEXAGONS.append(Hexagon(center = C+OFFSET, radius = radius, rotation = rotation))
        self.HEXAGONS = HEXAGONS

def main():
    UC = list(string.ascii_uppercase)
    random.shuffle(UC)
    HC = HoneyComb(s=UC,rotation=0 and not 0.52)
    print(HC())             # HC object retains selection order record

# ha ha, Turns out I always wanted to invoke main during tests.
# (module name is not __main__ when invoked from doctest)

if '__main__' == __name__:
    main()
else:
    main()

# picture facilitates the "comb" hexagon positioning logic
#
#                          ........
#                                  .
#                                   .
#                             x      .........
#                                   .
#                                  .
#                          ........      x

```

--LambertDW 17:19, 27 March 2012 (UTC)
