+++
title = "Talk:Polymorphism"
description = ""
date = 2011-10-26T13:59:43Z
aliases = []
[extra]
id = 1972
[taxonomies]
categories = []
tags = []
+++

==Circle is a Point?==
Hi, I think there's a big conceptual error in the code examples: Almost all of them derive a Circle class from a Point class. This is IMHO conceptually wrong; derivation would mean that a circle ''is-a'' point, which is clearly not the case. You wouldn't normally pass circles to functions expecting points (say, a function returning the straight line through two points).

:''It's probably worth noting that the center of a circle is a point.  (This issue was treated in some of the sample code in this section, but was not even touched on in the surrounding discussion.  That said, a relevant underlying issue might be that polymorphism has little to do with abstraction.)  --[[User:Rdm|Rdm]] 13:59, 26 October 2011 (UTC)''

The task, as described, doesn't demand to make that error (although I suspect that whoever wrote that task had this wrong derivation in mind).

A conceptually clean implementation of the task could be (taking C++ as example):
 #include <ostream>
 
 class Shape;
 {
 public:
   virtual void print(std::ostream& os) const = 0;
 };
 
 std::ostream& operator<<(std::ostream& os, Shape const& s)
 {
   s.print(os);
   return os;
 }
 
 class Point: public Shape
 {
 public:
   Point(int x = 0, int y = 0);
   int x() const;
   int y() const;
   void print() const;
 private:
   int xpos, ypos;
 };
 
 Point::Point(int x, int y):
   xpos(x),
   ypos(y)
 {
 }
 
 int Point::x() const
 {
   return xpos;
 }
 
 int Point::y() const
 {
   return ypos;
 }
 
 void Point::print(std::ostream& os) const
 {
   os << "Point(" << posx << ", " << posy << ")";
 }
 
 class Circle: public Shape
 {
 public:
   Circle(Point const& c, int r);
   Point center() const; // Ok, not demanded by task, but makes sense
   int center_x() const; // not a good idea, but demanded by task
   int center_y() const; // dito
   int r();
   void print(std::ostream& os);
 private:
   Point center;
   int radius;
 };
 
 // etc.

Of course in that case it would make sense to add a few more functions to Shape (e.g. a move function, or a function returning a bounding box). --[[User:Ce|Ce]] 08:26, 25 February 2007 (EST)
: The relation ''is-a'' is a notion of Liskov substitutability principle [LSP]. It is not necessarily related to inheritance and dynamic polymorphism. Sometimes the relation between types keeping all properties of the type (defined in some definite technical sense) is called '''LSP-subtyping''', and '''subclassing''', the latter, more loose relation. So-called LSP circle-ellipse controversy shows that the LSP notion of subtyping is practically (and theoretically too) unusable. Programming languages tend to more pragmatical "subclassing." --[[User:Dmitry-kazakov|Dmitry-kazakov]] 11:39, 3 November 2008 (UTC)

:'''Agree''' Circle derived from Point is poor design in this case, which might confuse the issue for the casual reader. The fix of having a common Shape subclass or interface looks much better to me.  (In fact "Printable" would be a better name in the example above.)  If you want to show off [[Inheritance]], perhaps split it into a different task. --[[User:IanOsgood|IanOsgood]] 13:01, 3 November 2008 (UTC)
:The Circle/Point idea is a problem now that I look at it. I'm not sure what the best way is to go about making the change to the Shape/Circle idea, though. The simplest idea (planning-wise) is to just start the task over. I don't think marking these examples as incorrect would be very good (since some may never be fixed). There are probably better ways to do this change...

:Ever better may be a slightly more complex inheritance tree. Maybe something like this:

```txt
           Car
            /\
           /  \
          /    \
     Porsche   Pinto
```

:There could be a "start" method in each class. For Car, it could print "vroom vroom", Porsche prints "go, baby, go!", and Pinto prints "clunk clunk". Then make a collection of Car objects (one Car, one Pinto, and one Porsche) and call the start method on each one for proof. Is that an OK idea or is it too complex?--[[User:Mwn3d|Mwn3d]] 16:31, 3 November 2008 (UTC)

== Fortran ==

I've removed omit from Fortran since this task can be implemented a lot better than the C code example. Using modules, overloading and derived types Fortran "seems" a lot more object oriented than C... Moreover... I've read from wikipedia [[wp:Type polymorphism]] that polymorphism is not necessarily OOP-related, even though the task talks about "classes"; I think Fortran can have ''easily'' ad-hoc polymorphism (but I am always unsure about any formal definition of a language feature...) --[[User:ShinTakezou|ShinTakezou]] 09:40, 17 July 2009 (UTC)

Second thought: Fortran (2003) can polymorphism and more; even Fortran95 could, with quirks... I am downloading the Intel Fortran compiler for noncommercial usage right now... if it works, I'll try to write full Fortran 2003 code... (GNU Fortran implements some features already) --[[User:ShinTakezou|ShinTakezou]] 10:19, 17 July 2009 (UTC)

The Fortran example is wrong: If the components ''x'', ''y'' and ''r'' are PRIVATE, they may not be used in a structure constructor (<tt>point(0.0,0.0)</tt>); or in the words of the current ISO Fortran 2008 standard: "C495 (R455) The type name and all components of the type for which a component-spec appears shall be accessible in the scoping unit containing the structure constructor." GNU Fortran (gfortran) correctly errors out while Intel's ifort 11.1 accepts it by default.  Solution 1: Use

  p = point() ! instead of "point(2.0d0, 3.0d0)"; uses the default initialization to 0.0
  p%set_x (2.0d0)
  p%set_y (3.0d0)


Solution 2: Use a constructor, i.e. a generic interface with the same name as the derived type:

  interface point
    module procedure set_point
  end interface point
  type(point) function set_point(x,y)
     real(8), intent(in) :: x, y
     set_point%x = x
     set_point%y = y
  end function set_point

Solution 3: Remove the PRIVATE attribute from the component definition.

Solution 1 works, but is a bit ugly. ("set_point(x,y)" would already be better); solution 3 also works, but makes the components world read/writable. Solution 2 is more elegant, but you need a rather new compiler; I think ifort 12 will support it - ifort 11.1 is not sufficient and gfortran also cannot compiler it yet. Final note: <tt>REAL(8)</tt> is not standard conform as the kind numbers are not defined. One could use <tt>integer, parameter :: dp = kind(0.0d0)"</tt> and then <tt>REAL(dp)</tt>. (Though, as REAL(8) is widely used, most compilers support it - either by default or with a flag.)
