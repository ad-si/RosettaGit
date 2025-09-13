+++
title = "Polymorphism"
description = ""
date = 2019-08-24T15:04:59Z
aliases = []
[extra]
id = 1968
[taxonomies]
categories = ["task", "Basic language learning"]
tags = []
+++

## Task

Create two classes   Point(x,y)   and   Circle(x,y,r)   with a polymorphic function print, accessors for (x,y,r), copy constructor, assignment and destructor and every possible default constructors





## ActionScript


```actionscript
package
{
    public class Point
    {
        protected var _x:Number;
        protected var _y:Number;

        public function Point(x:Number = 0, y:Number = 0)
        {
            _x = x;
            _y = y;
        }

        public function getX():Number
        {
            return _x;
        }

        public function setX(x:Number):void
        {
            _x = x;
        }

        public function getY():Number
        {
            return _y;
        }

        public function setY(y:Number):void
        {
            _x = y;
        }

        public function print():void
        {
            trace("Point");
        }
    }
}
```


```actionscript
package {
    public class Circle extends Point
    {
        private var r:Number;

        public function Circle(x:Number=0, y:Number=0, r:Number=0)
        {
            super(x, y);
            this.r = r;
        }

        public function getR():Number
        {
            return r;
        }

        public function setR(r:Number):void
        {
            this.r = r;
        }

        public override function print():void
        {
            trace("Circle");
        }
    }
}
```



## Ada

This example is constructed using a parent package and a child package. The parent package defines the Point type. The child package defines the Circle type.

```ada
package Shapes is
   type Point is tagged private;
   procedure Print(Item : in Point);
   function Setx(Item : in Point; Val : Integer) return Point;
   function Sety(Item : in Point; Val : Integer) return Point;
   function Getx(Item : in Point) return Integer;
   function Gety(Item : in Point) return Integer;
   function Create return Point;
   function Create(X : Integer) return Point;
   function Create(X, Y : Integer) return Point;

private
   type Point is tagged record
      X : Integer := 0;
      Y : Integer := 0;
   end record;
end Shapes;
```


```ada
with Ada.Text_Io; use Ada.Text_Io;

package body Shapes is

   -----------
   -- Print --
   -----------

   procedure Print (Item : in Point) is
   begin
      Put_line("Point");
   end Print;

   ----------
   -- Setx --
   ----------

   function Setx (Item : in Point; Val : Integer) return Point is
   begin
      return (Val, Item.Y);
   end Setx;

   ----------
   -- Sety --
   ----------

   function Sety (Item : in Point; Val : Integer) return Point is
   begin
      return (Item.X, Val);
   end Sety;

   ----------
   -- Getx --
   ----------

   function Getx (Item : in Point) return Integer is
   begin
      return Item.X;
   end Getx;

   ----------
   -- Gety --
   ----------

   function Gety (Item : in Point) return Integer is
   begin
      return Item.Y;
   end Gety;

   ------------
   -- Create --
   ------------

   function Create return Point is
   begin
      return (0, 0);
   end Create;

   ------------
   -- Create --
   ------------

   function Create (X : Integer) return Point is
   begin
      return (X, 0);
   end Create;

   ------------
   -- Create --
   ------------

   function Create (X, Y : Integer) return Point is
   begin
      return (X, Y);
   end Create;

end Shapes;
```

The following is the child package defining the Circle type.

```ada
package Shapes.Circles is
   type Circle is new Point with private;
   procedure Print(Item : Circle);
   function Setx(Item : Circle; Val : Integer) return Circle;
   function Sety(Item : Circle; Val : Integer) return Circle;
   function Setr(Item : Circle; Val : Integer) return Circle;
   function Getr(Item : Circle) return Integer;
   function Create(P : Point) return Circle;
   function Create(P : Point; R : Integer) return Circle;
   function Create(X : Integer) return Circle;
   function Create(X : Integer; Y : Integer) return Circle;
   function Create(X : Integer; Y : Integer; R : Integer) return Circle;
   function Create return Circle;
private
   type Circle is new Point with record
      R : Integer := 0;
   end record;
end Shapes.Circles;
```


```ada
with Ada.Text_Io; use Ada.Text_IO;

package body Shapes.Circles is

   -----------
   -- Print --
   -----------

   procedure Print (Item : Circle) is
   begin
      Put_line("Circle");
   end Print;

   ----------
   -- Setx --
   ----------

   function Setx (Item : Circle; Val : Integer) return Circle is
   begin
      return (Val, Item.Y, Item.R);
   end Setx;

   ----------
   -- Sety --
   ----------

   function Sety (Item : Circle; Val : Integer) return Circle is
      Temp : Circle := Item;
   begin
      Temp.Y := Val;
      return Temp;
   end Sety;

   ----------
   -- Setr --
   ----------

   function Setr (Item : Circle; Val : Integer) return Circle is
   begin
      return (Item.X, Item.Y, Val);
   end Setr;

   ----------
   -- Getr --
   ----------

   function Getr (Item : Circle) return Integer is
   begin
      return Item.R;
   end Getr;

   ------------
   -- Create --
   ------------

   function Create (P : Point) return Circle is
   begin
      return (P.X, P.Y, 0);
   end Create;

   ------------
   -- Create --
   ------------

   function Create (P : Point; R : Integer) return Circle is
   begin
      return (P.X, P.Y, R);
   end Create;

   ------------
   -- Create --
   ------------

   function Create (X : Integer) return Circle is
   begin
      return (X, 0, 0);
   end Create;

   ------------
   -- Create --
   ------------

   function Create (X : Integer; Y : Integer) return Circle is
   begin
      return (X, Y, 0);
   end Create;

   ------------
   -- Create --
   ------------

   function Create (X : Integer; Y : Integer; R : Integer) return Circle is
   begin
      return (X, Y, R);
   end Create;

   ------------
   -- Create --
   ------------

   function Create return Circle is
   begin
      return (0, 0, 0);
   end Create;

end Shapes.Circles;
```

The following procedure is an entry point for a program, serving the same purpose as the main function in C.

```ada
with Shapes.Circles; use Shapes.Circles;
use Shapes;

procedure Shapes_Main is
   P : Point;
   C : Circle;
begin
   P.Print;
   C.Print;
end Shapes_Main;
```



## Aikido


```aikido

class Point (protected x=0.0, protected y=0.0) {
    public function print {
        println ("Point")
    }

    public function getX { return x }
    public function getY { return y }

    public function setX(nx) { x = nx }
    public function setY(ny) { y = ny }
}

class Circle (x=0.0, y=0.0, r=0.0) extends Point (x, y) {
    public function print {
        println ("Circle")
    }

    public function getR { return r }
    public function setR(nr) { r = nr }
}

var p = new Point (1, 2)
var c = new Circle (1, 2, 3)
p.print()
c.print()


```



## ALGOL 68

```algol68
# Algol 68 provides for polymorphic operators but not procedures             #

# define the CIRCLE and POINT modes                                          #
MODE POINT  = STRUCT( REAL x, y    );
MODE CIRCLE = STRUCT( REAL x, y, r );


# PRINT operator                                                             #
OP PRINT = ( POINT  p )VOID: print( ( "Point(", x OF p, ",", y OF p, ")" ) );
OP PRINT = ( CIRCLE c )VOID: print( ( "Circle(", r OF c, " @ ", x OF c, ",", y OF c, ")" ) );

# getters                                                                    #
OP XCOORD = ( POINT p )REAL: x OF p;
OP YCOORD = ( POINT p )REAL: y OF p;

OP XCOORD = ( CIRCLE c )REAL: x OF c;
OP YCOORD = ( CIRCLE c )REAL: y OF c;
OP RADIUS = ( CIRCLE c )REAL: r OF c;

# setters                                                                    #
# the setters are dyadic operators so need a priority - we make them lowest  #
# priority, like PLUSAB etc.                                                 #
# They could have the same names as the getters but this seems clearer?      #
PRIO SETXCOORD = 1
   , SETYCOORD = 1
   , SETRADIUS = 1
   ;
# the setters return the POINT/CIRCLE being modified so we can write e.g.    #
# "PRINT ( p SETXCOORD 3 )"                                                  #
OP   SETXCOORD = ( REF POINT  p, REAL x )REF POINT:  ( x OF p := x; p );
OP   SETYCOORD = ( REF POINT  p, REAL y )REF POINT:  ( y OF p := y; p );

OP   SETXCOORD = ( REF CIRCLE c, REAL x )REF CIRCLE: ( x OF c := x; c );
OP   SETYCOORD = ( REF CIRCLE c, REAL y )REF CIRCLE: ( y OF c := y; c );
OP   SETRADIUS = ( REF CIRCLE c, REAL r )REF CIRCLE: ( r OF c := r; c );

# operands of an operator are not automatically coerced from INT to REAL so  #
# we also need these operators                                               #
OP   SETXCOORD = ( REF POINT  p, INT  x )REF POINT:  ( x OF p := x; p );
OP   SETYCOORD = ( REF POINT  p, INT  y )REF POINT:  ( y OF p := y; p );

OP   SETXCOORD = ( REF CIRCLE c, INT  x )REF CIRCLE: ( x OF c := x; c );
OP   SETYCOORD = ( REF CIRCLE c, INT  y )REF CIRCLE: ( y OF c := y; c );
OP   SETRADIUS = ( REF CIRCLE c, INT  r )REF CIRCLE: ( r OF c := r; c );

# copy constructors                                                          #
# A copy constructor is not needed as assignment will generate a copy        #
# e.g.: "POINT pa, pb; pa := ...; pb := pa; ..." will make pb a copy of pa   #

# assignment                                                                 #
# It is not possible to redefine the assignment "operator" in Algol 68 but   #
# assignment is automatically provided so no code need be written for e.g.   #
# "CIRCLE c1 := ...."                                                        #

# destructors                                                                #
# Algol 68 does not include destructors. A particular postlude could,        #
# in theory be provided if specific cleanup was requried, but this would     #
# occur at the end of the program, not at the end of the lifetime of the     #
# object.                                                                    #

# default constructor                                                        #
# Algol 68 automatically provides generators HEAP and LOC, which will        #
# create new objects of the specified MODE, e.g. HEAP CIRCLE will create a   #
# new CIRCLE. HEAP allocates apace on the heap, LOC allocates in on the      #
# stack (so the new item disappears when the enclosing block procedure or    #
# operator finishes)                                                         #

# a suitable "display" (value list enclosed in "(" and ")") can be cast to   #
# the relevent MODE, allowing us to write e.g.:                              #
# "POINT( 3.1, 2.2 )" where we need a new item.                              #

# "constructors" with other than all the fields in the correct order could   #
# be provided as procedures but each would need a distinct name              #
# e.g.                                                                       #
PROC new circle at the origin = ( REAL r )REF CIRCLE:
      ( ( HEAP CIRCLE SETRADIUS r ) SETXCOORD 0 ) SETYCOORD 0;
PROC new point at the origin = REF POINT:
      ( HEAP POINT SETXCOORD 0 ) SETYCOORD 0;


# examples of use                                                            #

BEGIN

    CIRCLE c1 := CIRCLE( 1.1, 2.4, 4.1 );
    POINT  p1 := new point at the origin;

    PRINT c1; newline( stand out );

    # move c1 so it is centred on p1                                         #
    ( c1 SETXCOORD XCOORD p1 ) SETYCOORD YCOORD p1;

    PRINT c1; newline( stand out )

END
```

```txt

Circle(+4.10000000000000e  +0 @ +1.10000000000000e  +0,+2.40000000000000e  +0)
Circle(+4.10000000000000e  +0 @ +0.00000000000000e  +0,+0.00000000000000e  +0)

```



## AutoHotkey

AutoHotkey does not support private or protected properties and thus does not need assignment and accessor methods. Assignment and accessor methods, as well as direct assignment and access, are shown. For more information see [http://ahkscript.org/docs/Objects.htm Objects].

```AutoHotkey
MyPoint := new Point(1, 8)
MyPoint.Print()
MyCircle := new Circle(4, 7, 9)
MyCircle2 := MyCircle.Copy()
MyCircle.SetX(2)	;Assignment method
MyCircle.y := 3		;Direct assignment
MyCircle.Print()
MyCircle2.Print()
MyCircle.SetX(100), MyCircle.SetY(1000), MyCircle.r := 10000
MsgBox, % MyCircle.__Class
	. "`n`nx:`t" MyCircle.GetX()
	. "`ny:`t" MyCircle.y
	. "`nr:`t" MyCircle.GetR()
return

class Point
{
	Copy()
	{
		return this.Clone()
	}
	GetX()
	{
		return this.x
	}
	GetY()
	{
		return this.y
	}
	__New(x, y)
	{
		this.x := x
		this.y := y
	}
	Print()
	{
		MsgBox, % this.__Class
			. "`n`nx:`t" this.x
			. "`ny:`t" this.y
	}
	SetX(aValue)
	{
		this.x := aValue
	}
	SetY(aValue)
	{
		this.y := aValue
	}
}

class Circle extends Point
{
	GetR()
	{
		return this.r
	}
	__New(x, y, r)
	{
		this.r := r
		base.__New(x, y)
	}
	Print()
	{
		MsgBox, % this.__Class
			. "`n`nx:`t" this.x
			. "`ny:`t" this.y
			. "`nr:`t" this.r
	}
	SetR(aValue)
	{
		this.r := aValue
	}
}
```



## BASIC

* See [[Polymorphism/BASIC]]


## BBC BASIC

```bbcbasic
      INSTALL @lib$ + "CLASSLIB"

      REM Create parent class with void 'doprint' method:
      DIM PrintableShape{doprint}
      PROC_class(PrintableShape{})

      REM Create derived class for Point:
      DIM Point{x#, y#, setxy, retx, rety, @constructor, @@destructor}
      PROC_inherit(Point{}, PrintableShape{})
      DEF Point.setxy (x,y) : Point.x# = x : Point.y# = y : ENDPROC
      DEF Point.retx = Point.x#
      DEF Point.rety = Point.y#
      DEF Point.@constructor Point.x# = 1.23 : Point.y# = 4.56 : ENDPROC
      DEF Point.@@destructor : ENDPROC
      DEF Point.doprint : PRINT Point.x#, Point.y# : ENDPROC
      PROC_class(Point{})

      REM Create derived class for Circle:
      DIM Circle{x#, y#, r#, setxy, setr, retx, rety, retr, @con, @@des}
      PROC_inherit(Circle{}, PrintableShape{})
      DEF Circle.setxy (x,y) : Circle.x# = x : Circle.y# = y : ENDPROC
      DEF Circle.setr (r) : Circle.r# = r : ENDPROC
      DEF Circle.retx = Circle.x#
      DEF Circle.rety = Circle.y#
      DEF Circle.retr = Circle.r#
      DEF Circle.@con Circle.x# = 3.2 : Circle.y# = 6.5 : Circle.r# = 7 : ENDPROC
      DEF Circle.@@des : ENDPROC
      DEF Circle.doprint : PRINT Circle.x#, Circle.y#, Circle.r# : ENDPROC
      PROC_class(Circle{})

      REM Test the polymorphic 'doprint' function:
      PROC_new(mypoint{}, Point{})
      PROC(mypoint.doprint)
      PROC_discard(mypoint{})
      PROC_new(mycircle{}, Circle{})
      PROC(mycircle.doprint)
      PROC_discard(mycircle{})
      END
```

```txt

      1.23      4.56
       3.2       6.5         7

```



## C

* See [[Polymorphism/C]]


## C++


```cpp
class Point
{
  protected:
    int x, y;
  public:
    Point(int x0 = 0, int y0 = 0) : x(x0), y(y0) {}
    Point(const Point& p) : x(p.x), y(p.y) {}
    virtual ~Point() {}
    const Point& operator=(const Point& p)
    {
      if(this != &p)
      {
        x = p.x;
        y = p.y;
      }
      return *this;
    }
    int getX() { return x; }
    int getY() { return y; }
    int setX(int x0) { x = x0; }
    int setY(int y0) { y = y0; }
    virtual void print() { printf("Point\n"); }
};

class Circle : public Point
{
  private:
    int r;
  public:
    Circle(Point p, int r0 = 0) : Point(p), r(r0) {}
    Circle(int x0 = 0, int y0 = 0, int r0 = 0) : Point(x0, y0), r(r0) {}
    virtual ~Circle() {}
    const Circle& operator=(const Circle& c)
    {
      if(this != &c)
      {
        x = c.x;
        y = c.y;
        r = c.r;
      }
      return *this;
    }
    int getR() { return r; }
    int setR(int r0) { r = r0; }
    virtual void print() { printf("Circle\n"); }
};

int main()
{
  Point* p = new Point();
  Point* c = new Circle();
  p->print();
  c->print();
  return 0;
}
```


'''Pattern:''' [[CRTP|Curiously Recurring Template Pattern]]


```cpp
// CRTP: Curiously Recurring Template Pattern
template <typename Derived>
class PointShape
{
protected:
  int x, y;
public:
  PointShape(int x0, int y0) : x(x0), y(y0) { }
  ~PointShape() { }
  int getX() { return x; }
  int getY() { return y; }
  int setX(int x0) { x = x0; }
  int setY(int y0) { y = y0; }

  // compile-time virtual function
  void print() { reinterpret_cast<const Derived*>(this)->printType(); }
};

class Point : public PointShape<Point>
{
public:
  Point(int x0 = 0, int y0 = 0) : PointShape(x0, y0) { }
  Point(const Point& p) : PointShape(p.x, p.y) { }
  ~Point() {}
  const Point& operator=(const Point& p)
  {
    if(this != &p)
    {
      x = p.x;
      y = p.y;
    }
    return *this;
  }
  void printType() { printf("Point\n"); }
};

class Circle : public PointShape<Circle>
{
private:
  int r;
public:
  Circle(int x0 = 0, int y0 = 0, int r0 = 0) : PointShape(x0, y0), r(r0) { }
  Circle(Point p, int r0 = 0) : PointShape(p.x, p.y), r(r0) { }
  ~Circle() {}
  const Circle& operator=(const Circle& c)
  {
    if(this != &c)
    {
      x = c.x;
      y = c.y;
      r = c.r;
    }
    return *this;
  }
  int getR() { return r; }
  int setR(int r0) { r = r0; }
  void printType() { printf("Circle\n"); }
};

int main()
{
  Point* p = new Point();
  Point* c = new Circle();
  p->print();
  c->print();
  return 0;
}
```


## C#

```c#
using System;
class Point
{
  protected int x, y;
  public Point() : this(0) {}
  public Point(int x) : this(x,0) {}
  public Point(int x, int y) { this.x = x; this.y = y; }
  public int X { get { return x; } set { x = value; } }
  public int Y { get { return y; } set { y = value; } }
  public virtual void print() { System.Console.WriteLine("Point"); }
}

public class Circle : Point
{
  private int r;
  public Circle(Point p) : this(p,0) { }
  public Circle(Point p, int r) : base(p) { this.r = r; }
  public Circle() : this(0) { }
  public Circle(int x) : this(x,0) { }
  public Circle(int x, int y) : this(x,y,0) { }
  public Circle(int x, int y, int r) : base(x,y) { this.r = r; }
  public int R { get { return r; } set { r = value; } }
  public override void print() { System.Console.WriteLine("Circle"); }

  public static void main(String args[])
  {
    Point p = new Point();
    Point c = new Circle();
    p.print();
    c.print();
  }
}
```



## Ceylon


```ceylon
import ceylon.language {
    consolePrint = print
}

shared void run() {

    class Point {

        shared variable Integer x;
        shared variable Integer y;

        shared new(Integer x = 0, Integer y = 0) {
            this.x = x;
            this.y = y;
        }

        shared new copy(Point p) {
            this.x = p.x;
            this.y = p.y;
        }

        shared default void print() {
            consolePrint("[Point ``x`` ``y``]");
        }
    }

    class Circle extends Point {

        shared variable Integer r;

        shared new(Integer x = 0, Integer y = 0, Integer r = 0) extends Point(x, y) {
            this.r = r;
        }

        shared new copy(Circle c) extends Point.copy(c){
            this.r = c.r;
        }

        shared actual void print() {
            consolePrint("[Circle ``x`` ``y`` ``r``]");
        }
    }

    value shapes = [
        Point(), Point(1), Point(1, 2), Point {y = 3;}, Point.copy(Point(4, 5)),
        Circle(), Circle(1), Circle(2, 3), Circle(4, 5, 6), Circle {y = 7; r = 8;}, Circle.copy(Circle(9, 10, 11))
    ];

    for(shape in shapes) {
        shape.print();
    }
}

```



## Clojure

Clojure 1.2.


```lisp
(defprotocol Printable
  (print-it [this] "Prints out the Printable."))

(deftype Point [x y]
  Printable
  (print-it [this] (println (str "Point: " x " " y))))

(defn create-point
  "Redundant constructor function."
  [x y] (Point. x y))

(deftype Circle [x y r]
  Printable
  (print-it [this] (println (str "Circle: " x " " y " " r))))

(defn create-circle
  "Redundant consturctor function."
  [x y r] (Circle. x y r))
```



## Common Lisp


```lisp
(defclass point ()
  ((x :initarg :x :initform 0 :accessor x)
   (y :initarg :y :initform 0 :accessor y)))

(defclass circle (point)
  ((radius :initarg :radius :initform 0 :accessor radius)))

(defgeneric shallow-copy (object))
(defmethod shallow-copy ((p point))
  (make-instance 'point :x (x p) :y (y p)))
(defmethod shallow-copy ((c circle))
  (make-instance 'circle :x (x c) :y (y c) :radius (radius c)))

(defgeneric print-shape (shape))
(defmethod print-shape ((p point))
  (print 'point))
(defmethod print-shape ((c circle))
  (print 'circle))

(let ((p (make-instance 'point :x 10))
      (c (make-instance 'circle :radius 5)))
  (print-shape p)
  (print-shape c))
```



## D


```d
import std.stdio: writeln;

class Point {
    private int x, y;
    this(int x_=0, int y_=0) { x = x_; y = y_; }
    this(Point p_) { x = p_.getX(); y = p_.getY(); }
    int getX() { return x; }
    void setX(int x_) { this.x = x_; }
    int getY() { return y; }
    void setY(int y_) { this.y = y_; }
}

class Circle : Point {
    private int r;
    this(int x_=0, int y_=0, int r_=0) {
        super(x_, y_);
        r = r_;
    }
    this(Point p, int r_=0) {
        super(p);
        r = r_;
    }
    this(Circle c_) {
        super(c_.getX(), c_.getY());
        r = c_.getR();
    }
    int getR() { return r; }
    void setR(int r0) { this.r = r0; }
}

void main() {
    auto p = new Point();
    auto c = new Circle();
    writeln(p);
    writeln(c);
}
```



## Delphi


```delphi
type
  { TPoint }

  TMyPoint = class
  private
    FX: Integer;
    FY: Integer;
  public
    constructor Create; overload;
    constructor Create(X0: Integer; Y0: Integer); overload;
    constructor Create(MyPoint: TMyPoint); overload;
    destructor Destroy; override;

    procedure Print; virtual;

    property X: Integer read FX write FX;
    property Y: Integer read FY write FY;
  end;

  { TCircle }

  TCircle = class(TMyPoint)
  private
    FR: Integer;
  public
    constructor Create(X0: Integer; Y0: Integer; R0: Integer); overload;
    constructor Create(MyPoint: TMyPoint; R0: Integer); overload;
    constructor Create(Circle: TCircle); overload;
    destructor Destroy; override;

    procedure Print; override;

    property R: Integer read FR write FR;
  end;

implementation

uses Dialogs;

{ TCircle }

constructor TCircle.Create(X0: Integer; Y0: Integer; R0: Integer);
begin
  inherited Create(X0, Y0);
  FR := R0;
end;

constructor TCircle.Create(MyPoint: TMyPoint; R0: Integer);
begin
  inherited Create(MyPoint);
  FR := R0;
end;

constructor TCircle.Create(Circle: TCircle);
begin
  Create;
  if not(Circle = Self) then
  begin
    FX := Circle.X;
    FY := Circle.Y;
    FR := Circle.R;
  end;
end;

destructor TCircle.Destroy;
begin
  inherited Destroy;
end;

procedure TCircle.Print;
begin
   ShowMessage('Circle');
end;

{ TMyPoint }

constructor TMyPoint.Create;
begin
  inherited Create;
end;

constructor TMyPoint.Create(X0: Integer; Y0: Integer);
begin
  Create;
  FX := X0;
  FY := Y0;
end;

constructor TMyPoint.Create(MyPoint: TMyPoint);
begin
  Create;
  if not(MyPoint = Self) then
  begin
    FX := MyPoint.X;
    FY := MyPoint.Y;
  end;
end;

destructor TMyPoint.Destroy;
begin
  inherited Destroy;
end;

procedure TMyPoint.Print;
begin
  ShowMessage('MyPoint');
end;
```



```delphi
var
  MyPoint: TMyPoint;
  Circle: TCircle;
begin
  MyPoint := TMyPoint.Create;
  try
    MyPoint.Print;
    Circle := TCircle.Create;
    try
      Circle.Print;
    finally
      FreeAndNil(Circle);
    end;
  finally
    FreeAndNil(MyPoint);
  end;
end;
```



## E


```e
def makePoint(x, y) {
  def point implements pbc {
    to __printOn(out) { out.print(`<point $x,$y>`) }
    to __optUncall() { return [makePoint, "run", [x, y]] }
    to x() { return x }
    to y() { return y }
    to withX(new) { return makePoint(new, y) }
    to withY(new) { return makePoint(x, new) }
  }
  return point
}

def makeCircle(x, y, r) {
  def circle extends makePoint(x, y) implements pbc {
    to __printOn(out) { out.print(`<circle $x,$y r $r>`) }
    to __optUncall() { return [makeCircle, "run", [x, y, r]] }
    to r() { return r }
    to withX(new) { return makeCircle(new, y, r) }
    to withY(new) { return makeCircle(x, new, r) }
    to withR(new) { return makeCircle(x, y, new) }
  }
  return circle
}
```


(It is unidiomatic to have mutation operations on an object of this sort in E, so this example has variation operations instead. __optUncall is used for serialization, and is the closest analogue to a copy constructor. E does not have destructors, but only post-mortem finalizers (which are registered after the object is created). The "extends" is only implementation inheritance; it is not necessary to enable polymorphism.)


```e
def p := makePoint(0.5, 0.5)
def c := makeCircle(1, 1, 2)
println(p)
println(c)
```



## EchoLisp


```scheme

(struct Point ((real:x 0) (real:y 0)))
(struct Circle ((real:x 0) (real:y 0) (real:r 1)))

(define-method (print Point:p) (printf "📌 [%d %d]" p.x p.y))
(define-method (print Circle:c) (printf "⭕️ center:[%d %d] radius:%d" c.x c.y c.r))

(print (Point 5 6))
    → 📌 [5 6]
(print (Circle 2 3 4))
    → ⭕️ center:[2 3] radius:4

;; Accessors :
;; (Point-x p), (Point-y p) or p.x, p.y
;; (Circle-x c), c.x , etc.
;; Setters :
;; (set-Point-x! p value), (set-Circle-r!  c value)  etc.

;; Constructors
;; (Point) (Point x) (Point x y)
;; (Circle) (circle x) (Circle x y) (Circle x y r)

;;Copy
(print (copy (Circle 3 3 )))
    →  ⭕️ center:[3 3] radius:1

;;Assignment (to a variable)
(define my-point (Point 7 8))

;;Destructor : none. Points and Circles are garbage collected.

;;Type checking
(Point "here" "there")
    💣 error: Real : type-check failure : here → 'Point:x'

;;Initializer procedure
(struct Circle ((x 0) (y 0) (r 1) d) #:initialize circle-init)
(define (circle-init Circle:c) (set-Circle-d! c (* 2 PI c.r)))
(define-method (print Circle:c)
    (printf "⭕️ center:[%d %d] radius:%d diameter:%d" c.x c.y c.r c.d))

(print (Circle 0 0 10))
    → ⭕️ center:[0 0] radius:10 diameter:62.83185307179586

```



## Eiffel



```eiffel
class
    POINT
inherit
    ANY
        redefine
            out
        end
create
    make, make_origin

feature -- Initialization

    make (a_x, a_y: INTEGER)
            -- Create with values `a_x' and `a_y'
        do
            set_x (a_x)
            set_y (a_y)
        ensure
            x_set: x = a_x
            y_set: y = a_y
        end

    make_origin
            -- Create at origin
        do
        ensure
            x_set: x = 0
            y_set: y = 0
        end

feature -- Access

    x: INTEGER assign set_x
            -- Horizontal axis coordinate

    y: INTEGER assign set_y
            -- Vertical axis coordinate

feature -- Element change

    set_x (a_x: INTEGER)
            -- Set `x' coordinate to `a_x'
        do
            x := a_x
        ensure
            x_set: x = a_x
        end

    set_y (a_y: INTEGER)
            -- Set `y' coordinate to `a_y'
        do
            y := a_y
        ensure
            y_set: y = a_y
        end

feature -- Output

    out: STRING
            -- Display as string
        do
            Result := "Point:   x = " + x.out + "   y = " + y.out
        end
end
```




```eiffel
class
    CIRCLE

inherit
    POINT
        rename
            make as point_make
        redefine
            make_origin,
            out
        end
create
    make, make_origin, make_from_point

feature -- Initialization

    make (a_x, a_y, a_r: INTEGER)
            -- Create with values `a_x' and `a_y' and `a_r'
        require
            non_negative_radius_argument: a_r >= 0
        do
            point_make (a_x, a_y)
            set_r (a_r)
        ensure
            x_set: x = a_x
            y_set: y = a_y
            r_set: r = a_r
        end

    make_origin
            -- Create at origin with zero radius
        do
            Precursor
        ensure then
            r_set: r = 0
        end

    make_from_point (a_p: POINT; a_r: INTEGER)
            -- Initialize from `a_r' with radius `a_r'.
        require
            non_negative_radius_argument: a_r >= 0
        do
            set_x (a_p.x)
            set_y (a_p.y)
            set_r (a_r)
        ensure
            x_set: x = a_p.x
            y_set: y = a_p.y
            r_set: r = a_r
        end

feature -- Access

    r: INTEGER assign set_r
            -- Radius

feature -- Element change

    set_r (a_r: INTEGER)
            -- Set radius (`r') to `a_r'
        require
            non_negative_radius_argument: a_r >= 0
        do
            r := a_r
        ensure
            r_set: r = a_r
        end

feature -- Output

    out: STRING
            -- Display as string
        do
            Result := "Circle:  x = " + x.out + "   y = " + y.out + "   r = " + r.out
        end

invariant

    non_negative_radius: r >= 0

end
```




```eiffel
class
    APPLICATION

create
    make

feature {NONE} -- Initialization

    make
            -- Run application.
        local
            my_point: POINT
            my_circle: CIRCLE
        do
            create my_point.make_origin
            print (my_point.out + "%N")

            create {CIRCLE} my_point.make_origin
            print (my_point.out + "%N")

            create my_point.make (10, 15)
            print (my_point.out + "%N")

            create {CIRCLE} my_point.make (20, 25, 5)
            print (my_point.out + "%N")

            create my_circle.make (30, 35, 10)
            print (my_circle.out + "%N")

            create my_circle.make_from_point (my_point, 35)
            print (my_circle.out + "%N")
        end

end
```


```txt

Point:   x = 0   y = 0
Circle:  x = 0   y = 0   r = 0
Point:   x = 10   y = 15
Circle:  x = 20   y = 25   r = 5
Circle:  x = 30   y = 35   r = 10
Circle:  x = 20   y = 25   r = 35

```


Notes:

The Eiffel example varies slightly from the problem description.
The polymorphic feature is <code lang="eiffel">out</code> rather than <code lang="eiffel">print</code>. Both <code lang="eiffel">out</code> and <code lang="eiffel">print</code> are inherited by every Eiffel class from class <code lang="eiffel">ANY</code>.
However, it is customary in Eiffel to redefine the query <code lang="eiffel">out</code> to provide a string describing an instance, versus redefining <code lang="eiffel">print</code>.
So, this example is written to reflect the Eiffel convention.


## Ela


Solution of this problem in Ela is similar to Haskell, as soon as Ela shares with Haskell the same features - namely, classes (typeclasses) and algebraic types.


```ela
type Point = Point x y

instance Show Point where
  show (Point x y) = "Point " ++ (show x) ++ " " ++ (show y)

instance Name Point where
  getField nm (Point x y)
    | nm == "x" = x
    | nm == "y" = y
    | else = fail "Undefined name."
  isField nm _ = nm == "x" || nm == "y"

pointX = flip Point 0

pointY = Point 0

pointEmpty = Point 0 0

type Circle = Circle x y z

instance Show Circle where
  show (Circle x y z) =
    "Circle " ++ (show x) ++ " " ++ (show y) ++ " " ++ (show z)

instance Name Circle where
  getField nm (Circle x y z)
    | nm == "x" = x
    | nm == "y" = y
    | nm == "z" = z
    | else = fail "Undefined name."
  isField nm _ = nm == "x" || nm == "y" || nm == "z"

circleXZ = flip Circle 0

circleX x = Circle x 0 0

circleYZ = Circle 0

circleY y = Circle 0 y 0

circleZ = Circle 0 0

circleEmpty = Circle 0 0 0
```


Class Show is defined in prelude and is effectively an abstraction for all "printable" entities.

Normally, algebraic types are analyzed using pattern matching, however, it is possible to provide a support for an "accessor style" approach by providing an instance for class Name (which is also defined in prelude). With this instance it is possible to write code like so:


```ela
c = circleX 12
c.x //Evaluates to 12
```


## Elena

ELENA 4.x :

```elena
import extensions;

class Point
{
    prop int X;
    prop int Y;

    constructor(int x, int y)
    {
        X := x;
        Y := y
    }

    constructor()
        <= (0,0);

    print() { console.printLine("Point") }
}

class Circle : Point
{
    prop int R;

    constructor()
        <= (0);

    constructor(int r)
        <= (0, 0, r);

    constructor(int x, int y, int r)
        <= (x, y)
    {
        R := r
    }

    print() { console.printLine("Circle") }
}

public program()
{
    Point p := new Point();
    Point c := new Circle();

    p.print();
    c.print()
}
```

```txt

Point
Circle

```



## Factor


```factor
QUALIFIED: io  ! there already is print in io

GENERIC: print ( shape -- )

TUPLE: point x y ;
C: <point> point  ! shorthand constructor definition

M: point print drop "Point" io:print ;

TUPLE: circle radius x y ;
C: <circle> circle

M: circle print drop "Circle" io:print ;
```



## Forth

There are numerous, mutually incompatible object oriented frameworks for Forth. This one works with the FOOS preprocessor extension of [[4tH]]. Variadic functions in Forth are usually implemented by passing the number of parameters. Since it is highly unlikely that objects are allocated that low in memory it works. Note that X, Y and Z are passed in reverse order, which is quite common for any Forth program.

```forth
include lib/memcell.4th
include 4pp/lib/foos.4pp

:: Point                               ( xn n a--)
   class
     field:  x                         \ x coordinate
     field:  y                         \ y coordinate
     method: print                     \ print routine
     method: setx                      \ set x coordinate
     method: sety                      \ set y coordinate
     method: getx                      \ get x coordinate
     method: gety                      \ get y coordinate
   end-class {
                                       \ bind the methods immediately
     :method { this -> x ! } ; defines setx
     :method { this -> y ! } ; defines sety
     :method { this -> x @ } ; defines getx
     :method { this -> y @ } ; defines gety
                                       \ because we'll use them immediately
     :method {                         \ e.g. in this print routine
       ." Point(" this => getx 0 .r ." ," this => gety 0 .r ." )" cr
     } ; defines print                 \ and this initialization
                                       \ object or argument count
     dup type@ this type@ =            \ if it is an object, a point
     if                                \ get the coordinates and set them
       dup => getx this => setx
           => gety this => sety
     else                              \ otherwise initialize it
       0 dup this => setx this => sety
       case                            \ and check the argument count
         1 of this => setx endof       \ one argument : x only
         2 of this => setx             \ two arguments: x and y
              this => sety endof
       endcase
     then

     private{ x y }                    \ make x and y private
   }
;

:: Circle                              ( xn n a --)
   over >r                             ( arg-count object-addr)
   extends Point                       \ save the argument count!!
     field:  r                         \ radius
     method: getr                      \ get radius
     method: setr                      \ set radius
   end-extends r> swap {               \ retrieve count
                                       \ bind the methods immediately
     :method { this -> r ! } ; defines setr
     :method { this -> r @ } ; defines getr
                                       \ because we'll use them immediately
     :method {                         \ e.g. in this print routine
       ." Circle(" this => getx 0 .r ." ,"
                   this => gety 0 .r ." ,"
                   this => getr 0 .r ." )" cr
     } ; defines print                 \ and this initialization
                                       \ object or argument count
     dup type@ this type@ =            \ if it is an object, a circle
     if                                \ get the coordinates and set them
       dup => getx this => setx
       dup => gety this => sety
           => getr this => setr
     else                              \ otherwise initialize it
       0 this => setr
       case                            \ and check the argument count
         3 of this => setr             \ three arguments: x, y and r
              this => sety             \ note the rest is already set
              this => setx endof       \ by "Point" and r was left on
       endcase                         \ the stack!
     then

     private{ r }
   }
;

0 new Point Point1
Point1 => print
45 23 2 new Point Point2
Point2 => print
Point2 new Point Point3
Point3 => print
78 1 new Point Point4
Point4 => print
10 45 23 3 new Circle Circle1
Circle1 => print
Point2 new Circle Circle2
Circle2 => print
Circle1 new Circle Circle3
Circle3 => print
```



Works with any ANS Forth

Needs the FMS-SI (single inheritance) library code located here:
http://soton.mpeforth.com/flag/fms/index.html

```forth
include FMS-SI.f

:class point
  ivar x  \ instance variable
  ivar y
  :m print x ? y ? ;m  \ define print method
  :m get ( -- x y ) x @ y @ ;m
  :m put ( x y -- ) y ! x ! ;m
  :m copy ( -- point-obj2 )
     self get heap> point dup >r put r> ;m
;class

point p1  \ instantiate object p1
23 5 p1 put
p1 print
p1 copy value p2 \ copy constructor
p2 print
p2 <free  \ destructor

.. p1.x ?   \ print just x
.. p1.y ?   \ print just y
8 .. p1.x ! \ change just x
9 .. p1.y ! \ change just y


:class circle
  point center  \ re-use point class for instance variable
  ivar radius
  :m print center print radius ? ;m  \ send print message to instance variable
  :m get ( -- x y r )
    center get radius @ ;m
  :m put ( x y r -- )
    radius ! center put ;m
  :m copy ( -- circle-obj2 )
     self get heap> circle dup >r put r> ;m
;class

circle c1
4 5 2 c1 put
c1 print
c1 copy value c2
c2 print
c2 <free

.. c1.center print \ print just center
.. c1.center.x ?   \ print just x
.. c1.center.y ?   \ print just y
.. c1.radius ?     \ print just radius
p1 get .. c1.center put \ change just center using a point
100 .. c1.radius ! \ change just radius
```



## Fortran

Fortran provides OO features with the type mechanism. This example works with the Intel 11.1.069 compiler.

```fortran

module geom

  type point
     real(8), private  :: x = 0
     real(8), private  :: y = 0
   contains
     procedure, public :: get_x
     procedure, public :: get_y
     procedure, public :: set_x
     procedure, public :: set_y
     procedure, public :: print => print_point
     procedure, pass :: copy_point
     !overloaded assignment operator
     generic, public :: assignment(=) => copy_point
  end type point

  type, extends(point) :: circle
     real(8), private  :: r = 0
   contains
     procedure, public :: get_r
     procedure, public :: set_r
     procedure, public :: print => print_circle
     procedure, pass :: copy_circle
     !overloaded assignment operator
     generic, public :: assignment(=) => copy_circle
  end type circle

  ! constructor interface
  interface circle
  module procedure circle_constructor
  end interface circle
  ! constructor interface
  interface point
  module procedure point_constructor
  end interface point

contains

  real(8) function get_x(this)
    class(point), intent(in) :: this
    get_x = this%x
  end function get_x

  real(8) function get_y(this)
    class(point), intent(in) :: this
    get_y = this%y
  end function get_y

  subroutine set_x(this, val)
    class(point), intent(inout) :: this
    real(8), intent(in)         :: val
    this%x = val
  end subroutine set_x

  subroutine set_y(this, val)
    class(point), intent(inout) :: this
    real(8), intent(in)         :: val
    this%y = val
  end subroutine set_y

  subroutine print_point(this)
    class(point), intent(in) :: this
    write(*,'(2(a,f0.4),a)') 'Point(',this%x,', ',this%y,')'
  end subroutine print_point

  real(8) function get_r(this)
    class(circle), intent(in) :: this
    get_r = this%r
  end function get_r

  subroutine set_r(this, val)
    class(circle), intent(inout) :: this
    real(8), intent(in)          :: val
    this%r = val
  end subroutine set_r

  subroutine print_circle(this)
    class(circle), intent(in) :: this
    write(*,'(3(a,f0.4),a)') 'Circle(',this%x,', ',this%y,'; ',this%r,')'
  end subroutine print_circle

  subroutine copy_point(this, rhs)
      class(point), intent(inout) :: this
      type(point), intent(in) :: rhs
      this%x = rhs%x
      this%y = rhs%y
  end subroutine copy_point

  subroutine copy_circle(this, rhs)
      class(circle), intent(inout) :: this
      type(circle), intent(in) :: rhs
      this%x = rhs%x
      this%y = rhs%y
      this%r = rhs%r
  end subroutine copy_circle

! non-default constructor to init private components
  type(point) function point_constructor(x,y)
  real(8), intent(in) :: x,y
  point_constructor%x = x
  point_constructor%y = y
  end function point_constructor
! non-default constructor to init private components
  type(circle) function circle_constructor(x,y,r)
  real(8), intent(in) :: x,y,r
  circle_constructor%x = x
  circle_constructor%y = y
  circle_constructor%r = r
  end function circle_constructor

end module geom

program inh
  use geom

  type(point)  :: p, p_copy
  type(circle) :: c, c_copy

  p = point(2.0d0, 3.0d0)
  call p%print
  p_copy = p
  call p_copy%print

  c = circle(3.0d0, 4.0d0, 5.0d0)
  call c%print
  c_copy = c
  call c_copy%print

end program inh


```


=={{header|F Sharp|F#}}==
Polymorphism is achieved by defining an interface <code>Printable</code> which is implemented by <code>Point</code> and <code>Circle</code>. (In real code, you should override the <code>ToString</code> method which every class inherits from <code>Object</code>.)

Due to the use of optional parameters, we only need one constructor for every class. No accessors are necessary because we use public read-only properties. (Mutable properties are possible, too, but should be avoided in idiomatic code.)


```fsharp
type Printable =
  abstract member Print : unit -> unit

type Point(?x, ?y) =
  member t.x = defaultArg x 0.0
  member t.y = defaultArg y 0.0
  interface Printable with
     member t.Print() = printfn "Point(x:%f, y:%f)" t.x t.y

type Circle(?center, ?radius) =
  member t.center = defaultArg center (new Point())
  member t.radius = defaultArg radius 1.0
  interface Printable with
    member t.Print() =
      printfn "Circle(x:%f, y:%f, r:%f)" t.center.x t.center.y t.radius
```


## Go


```go
package main

import "fmt"

type point struct {
    x, y float64
}

type circle struct {
    x, y, r float64
}

type printer interface {
    print()
}

func (p *point) print() {
    fmt.Println(p.x, p.y)
}

func (c *circle) print() {
    fmt.Println(c.x, c.y, c.r)
}

func main() {
    var i printer            // polymorphic variable
    i = newPoint(3, 4)       // assign one type
    i.print()                // call polymorphic function
    i = newCircle(5, 12, 13) // assign different type to same variable
    i.print()                // same call accesses different method now.
}

// Above is a sort of polymorphism: both types implement the printer
// interface.  The print function can be called through a variable
// of type printer, without knowing the underlying type.

// Below is other stuff the task asks for.  Note that none of it is
// needed for cases as simple as this task, and it is not idomatic
// to write any of these functions in these simple cases.

// Accessors are not idiomatic in Go.  Instead, simply access struct
// fields directly.  To allow access from another package, you "export"
// the field by capitalizing the field name.
func (p *point) getX() float64  { return p.x }
func (p *point) getY() float64  { return p.y }
func (p *point) setX(v float64) { p.x = v }
func (p *point) setY(v float64) { p.y = v }

func (c *circle) getX() float64  { return c.x }
func (c *circle) getY() float64  { return c.y }
func (c *circle) getR() float64  { return c.r }
func (c *circle) setX(v float64) { c.x = v }
func (c *circle) setY(v float64) { c.y = v }
func (c *circle) setR(v float64) { c.r = v }

// Copy constructors, not idiomatic.  Structs are assignable so
// you can simply declare and assign them as needed.
func (p *point) clone() *point   { r := *p; return &r }
func (c *circle) clone() *circle { r := *c; return &r }

// Assignment methods, not idiomatic.  Just use the assignment operator.
func (p *point) set(q *point)   { *p = *q }
func (c *circle) set(d *circle) { *c = *d }

// Constructors are idiomatic only when construction involves something
// more than just assigning initial values.  By default, structs
// are created as "zero values," that is, with all fields zero,
// empty, or nil.  The struct literal synax allows for all fields to
// initialized, or for any subset of fields to be initialized by name.
// These feautures take the place of trivial default constructors.
// When additional initialization is needed, it is conventional to
// name a function New, New<Type>, or within a package, new<Type>
// as shown here.
func newPoint(x, y float64) *point {
    return &point{x, y}
}
func newCircle(x, y, r float64) *circle {
    return &circle{x, y, r}
}

// Destructors are never used in Go.  Objects are garbage collected.
```



## Golo


```golo
#!/usr/bin/env golosh
----
This module demonstrates Golo's version of polymorphism.
----
module Polymorphism

# Each struct automatically gets a constructor and also accessor and assignment methods for each field.
# For example, the constructor for Point is Point(1, 2)
# and the accessor methods are x() and y()
# and the assignment methods are x(10) and y(10).

struct Point = { x, y }
struct Circle = { x, y, r }

# Augmentations are the way to give your struct methods.
# They're like extension methods in C# or Xtend.

augment Point {

  function print = |this| { println("Point " + this: x() + " " + this: y()) }
}

augment Circle {

  function print = |this| { println("Circle " + this: x() + " " + this: y() + " " + this: r()) }
}

# You can define functions with the same name as your struct that work
# basically like constructors.

----
A contructor with no arguments that initializes all fields to 0
----
function Point = -> Point(0, 0)

----
This is the copy constructor when the argument is another point
----
function Point = |x| -> match {
  when x oftype Point.class then Point(x: x(), x: y())
  otherwise Point(x, 0)
}

----
A contructor with no arguments that initializes all fields to 0
----
function Circle = -> Circle(0, 0, 0)

----
This is the copy constructor when the argument is another circle
----
function Circle = |x| -> match {
  when x oftype Circle.class then Circle(x: x(), x: y(), x: r())
  otherwise Circle(x, 0, 0)
}

----
This one initializes the radius to zero
----
function Circle = |x, y| -> Circle(x, y, 0)


function main = |args| {
  let p = Point(10, 20)
  let c = Circle(10, 20, 30)
  let shapes = vector[
    Point(), Point(1), Point(1, 2), Point(p),
    Circle(), Circle(1), Circle(1, 2), Circle(1, 2, 3), Circle(c)
  ]
  foreach shape in shapes {
    shape: print()
  }
}
```



## Groovy


```groovy
@Canonical
@TupleConstructor(force = true)
@ToString(includeNames = true)
class Point {
    Point(Point p) { x = p.x; y = p.y }
    void print() { println toString() }
    Number x
    Number y
}

@Canonical
@TupleConstructor(force = true)
@ToString(includeNames = true, includeSuper = true)
class Circle extends Point {
    Circle(Circle c) { super(c); r = c.r }
    void print() { println toString() }
    Number r
}
```

Test Code:

```groovy
def p = new Point(x: 3, y: 4)
def c = new Circle(x: 4, y: 3, r: 5)

[(p): new Point(p), (c): new Circle(c)].each { v1, v2 ->
    print "Verifying $v1 == "
    v2.print()
    assert v1 == v2
}
```

```txt
Verifying Point(x:3, y:4) == Point(x:3, y:4)
Verifying Circle(r:5, super:Point(x:4, y:3)) == Circle(r:5, super:Point(x:4, y:3))
```



## Haskell

Polymorphism is achieved through the type class Show


```haskell
data Point = Point Integer Integer
instance Show Point where
    show (Point x y) = "Point at "++(show x)++","++(show y)

-- Constructor that sets y to 0
ponXAxis = flip Point 0

-- Constructor that sets x to 0
ponYAxis = Point 0

-- Constructor that sets x and y to 0
porigin = Point 0 0

data Circle = Circle Integer Integer Integer
instance Show Circle where
    show (Circle x y r) = "Circle at "++(show x)++","++(show y)++" with radius "++(show r)

-- Constructor that sets y to 0
conXAxis = flip Circle 0

-- Constructor that sets x to 0
conYAxis = Circle 0

-- Constructor that sets x and y to 0
catOrigin = Circle 0 0

--Constructor that sets y and r to 0
c0OnXAxis = flip (flip Circle 0) 0

--Constructor that sets x and r to 0
c0OnYAxis = flip (Circle 0) 0
```


== Icon and {{header|Unicon}} ==

This is Unicon specific, as Unicon has classes, but Icon does not.

There is no destructor, as Unicon manages object destruction itself.  The copy constructor is emulated by a method.  Notice the 'initially' clauses.  These act like constructors, in that they accept input parameters during instance construction.  These parameters are null if not used, and so the initial field values are set to 0 if the entered value is null (tested using the '/' symbol).


```Unicon
class Circle (x, y, r)
  # make a new copy of this instance
  method copy ()
    return Circle (x, y, r)
  end

  # print a representation of this instance
  method print ()
    write ("Circle (" || x || ", " || y || ", " || r || ")")
  end

  # called during instance construction, to pass in field values
  initially (x, y, r)
    self.x := if /x then 0 else x # set to 0 if argument not present
    self.y := if /y then 0 else y
    self.r := if /r then 0 else r

end

class Point (x, y)
  # make a new copy of this instance
  method copy ()
    return Point (x, y)
  end

  # print a representation of this instance
  method print ()
    write ("Point (" || x || ", " || y || ")")
  end

  # called during instance construction, to pass in field values
  initially (x, y)
    self.x := if /x then 0 else x # set to 0 if argument not present
    self.y := if /y then 0 else y

end

procedure main ()
  p1 := Point ()
  p2 := Point (1)
  p3 := Point (1,2)
  p4 := p3.copy ()

  write ("Points:")
  p1.print ()
  p2.print ()
  p3.print ()
  p4.print ()
  # demonstrate field mutator/accessor
  p3.x := 3
  write ("p3 value of x is: " || p3.x)

  c1 := Circle ()
  c2 := Circle (1)
  c3 := Circle (1,2)
  c4 := Circle (1,2,3)

  write ("Circles:")
  c1.print ()
  c2.print ()
  c3.print ()
  c4.print ()
end

```



## Inform 7


Accessors are not needed since property values are public. Constructors and destructors are not needed since objects are statically allocated and initialized.


```inform7
Space is a room.

A point is a kind of thing.
A point has a number called X position.
A point has a number called Y position.

A circle is a kind of point.
A circle has a number called radius.

To print (P - point): say "Point: [X position of P], [Y position of P]."
To print (C - circle): say "Circle: [X position of C], [Y position of C] radius [radius of C]."

The origin is a point with X position 0 and Y position 0.
The circle of power is a circle with X position 100, Y position 25, radius 7.

When play begins:
	print the origin;
	print the circle of power;
	end the story.
```



## J



```J
coclass 'Point'
create=: monad define
  'X Y'=:2{.y
)
getX=: monad def 'X'
getY=: monad def 'Y'
setX=: monad def 'X=:y'
setY=: monad def 'Y=:y'
print=: monad define
  smoutput 'Point ',":X,Y
)
destroy=: codestroy
```



```J
coclass 'Circle'
coinsert 'Point'
create=: monad define
  'X Y R'=: 3{.y
)
getR=: monad def 'R'
setR=: monad def 'R=:y'
print=: monad define
  smoutput 'Circle ',":X,Y,R
)
```



## Java



```java
class Point {
   protected int x, y;
   public Point() { this(0); }
   public Point(int x) { this(x, 0); }
   public Point(int x, int y) { this.x = x; this.y = y; }
   public Point(Point p) { this(p.x, p.y); }
   public int getX() { return this.x; }
   public int getY() { return this.y; }
   public void setX(int x) { this.x = x; }
   public void setY(int y) { this.y = y; }
   public void print() { System.out.println("Point x: " + this.x + " y: " + this.y); }
}

class Circle extends Point {
   private int r;
   public Circle(Point p) { this(p, 0); }
   public Circle(Point p, int r) { super(p); this.r = r; }
   public Circle() { this(0); }
   public Circle(int x) { this(x, 0); }
   public Circle(int x, int y) { this(x, y, 0); }
   public Circle(int x, int y, int r) { super(x, y); this.r = r; }
   public Circle(Circle c) { this(c.x, c.y, c.r); }
   public int getR() { return this.r; }
   public void setR(int r) { this.r = r; }
   public void print() { System.out.println("Circle x: " + this.x + " y: " + this.y + " r: " + this.r); }
}

public class test {
  public static void main(String args[]) {
    Point p = new Point();
    Point c = new Circle();
    p.print();
    c.print();
  }
}
```



## JavaScript


```javascript
/* create new Point in one of these ways:
 *    var p = new Point(x,y);
 *    var p = new Point(a_point);
 * default value for x,y is 0
 */
function Point() {
    var arg1 = arguments[0];
    var arg2 = arguments[1];

    if (arg1 instanceof Point) {
        this.x = arg1.x;
        this.y = arg1.y;
    }
    else {
        this.x = arg1 == null ? 0 : arg1;
        this.y = arg2 == null ? 0 : arg1;
    }

    this.set_x = function(_x) {this.x = _x;}
    this.set_y = function(_y) {this.y = _y;}
}

Point.prototype.print = function() {
    var out = "Point(" + this.x + "," + this.y + ")";
    print(out);
}

/* create new Circle in one of these ways:
 *    var c = new Circle(x,y,r);
 *    var c = new Circle(a_circle);
 *    var c = new Circle(a_point,r);
 * default value for x,y,r is 0
 */
function Circle() {
    var arg1 = arguments[0];
    var arg2 = arguments[1];
    var arg3 = arguments[2];

    if (arg1 instanceof Circle) {
        this.x = arg1.x;
        this.y = arg1.y;
        this.r = arg1.r;
    }
    else if (arg1 instanceof Point) {
        this.x = arg1.x;
        this.y = arg1.y;
        this.r = arg2 == null ? 0 : arg2;
    }
    else {
        this.x = arg1 == null ? 0 : arg1;
        this.y = arg2 == null ? 0 : arg2;
        this.r = arg3 == null ? 0 : arg3;
    }

    this.set_x = function(_x) {this.x = _x;}
    this.set_y = function(_y) {this.y = _y;}
    this.set_r = function(_r) {this.r = _r;}
}

Circle.prototype.print = function() {
    var out = "Circle(" + this.x + "," + this.y + "," + this.r + ")";
    print(out);
}
```



## jq



```jq

def Point(x;y): {"type": "Point", "x": x, "y": y};
def Point(x): Point(x;0);
def Point: Point(0);

def Circle(x;y;r): {"type": "Circle", "x": x, "y": y, "r": r};
def Circle(x;y): Circle(x;y;0);
def Circle(x): Circle(x;0);
def Circle: Circle(0);

def print:
  if  .type == "Circle" then "\(.type)(\(.x); \(.y); \(.r))"
  elif .type == "Point" then "\(.type)(\(.x); \(.y))"
  else empty
  end;

```


In practice, it's unlikely one would want to write accessors, as .x will retrieve "x", etc; similar remarks apply to setters (.x = VALUE). `.` will copy, and `empty` could serve as a kind of destructor, in that `Point(0;0) | empty` produces the empty stream.

For the sake of illustration, one could define a polymorphic "setter" as follows:


```jq

# keyname should be (or evaluate to) a string
def set(keyname; value):
  if type == "object" and .type and has(keyname) then .[keyname] = value
  else error("set: invalid type: \(.)")
  end;

```

Example:

```julia

Circle(0;1;2) | .x = 1 | print

```



## Julia

There is no obvious inheritance hierarchy here to get polymorphism. Julia has multiple dispatch, so the appropriate implementation of the show function will be called at runtime depending on the type of the arguments provided. The declaration of Base.show is done to implicitly import the show function from the Base module and create new methods.

It would not be idiomatic to define setters and getters for a type like this in Julia. One would just access the fields directly. There is no need to explicitly define a constructor since that is automatically provided. You only roll your own if you need more elaborate initialization or you need to set default values.

```julia
mutable struct Point
	x::Float64
	y::Float64
end

Base.show(io::IO, p::Point) = print(io, "Point($(p.x), $(p.y))")

getx(p::Point) = p.x
gety(p::Point) = p.y

setx(p::Point, x) = (p.x = x)
sety(p::Point, y) = (p.y = y)

mutable struct Circle
	x::Float64
	y::Float64
	r::Float64
end

getx(c::Circle) = c.x
gety(c::Circle) = c.y
getr(c::Circle) = c.r

setx(c::Circle, x) = (c.x = x)
sety(c::Circle, y) = (c.y = y)
setr(c::Circle, r) = (c.r = r)

Base.show(io::IO, c::Circle) = print(io, "Circle($(c.x), $(c.y), $(c.r))")
```



## Kotlin

Kotlin only has properties, not fields though the latter may be created by the compiler 'under the hood'. A 'get' accessor is automatically created for 'val' (read-only) properties and both get() and set() accessors for a 'var' (read/write) property though these may be overridden where appropriate.

Kotlin has the notion of a 'primary constructor' which is declared in the class header itself. It's also posible to create any number of 'secondary constructors' provided these delegate (directly or indirectly) to the primary constructor.

Although Kotlin supports operator overloading, it is not possible to overload the assignment operator ('=') itself.

In the JVM version of Kotlin, it is possible to declare a destructor in the guise of a 'finalize' method though there is no guarantee that this will actually be called by the garbage collector (or, if it is called, when this will be) and consequently many programmers feel it is more trouble than its worth.

```scala
// version 1.1.2

open class Point(var x: Int, var y: Int) {
    constructor(): this(0, 0)

    constructor(x: Int) : this(x, 0)

    constructor(p: Point) : this(p.x, p.y)

    open protected fun finalize() = println("Finalizing $this...")

    override fun toString() = "Point at ($x, $y)"

    open fun print() = println(this)
}

class Circle(x: Int, y: Int, var r: Int) : Point(x, y) {
    constructor(): this(0, 0, 0)

    constructor(x: Int) : this(x, 0, 0)

    constructor(x: Int, r: Int) : this(x, 0, r)

    constructor(c: Circle) : this(c.x, c.y, c.r)

    // for simplicity not calling super.finalize() below though this would normally be done in practice
    override protected fun finalize() = println("Finalizing $this...")

    override fun toString() = "Circle at center ($x, $y), radius $r"

    override fun print() = println(this)
}

fun createObjects() {
    val points = listOf(Point(), Point(1), Point(2, 3), Point(Point(3, 4)))
    for (point in points) point.print()
    val circles = listOf(Circle(), Circle(1), Circle(2, 3), Circle(4, 5, 6), Circle(Circle(7, 8, 9)))
    for (circle in circles) circle.print()
    println()
}

fun main(args: Array<String>) {
    createObjects()
    System.gc()  // try and force garbage collection
    Thread.sleep(2000) // allow time for finalizers to run
    println()
    val p = Point(5, 6)
    p.print()
    p.y = 7  // change y coordinate
    p.print()
    val c = Circle(5, 6, 7)
    c.print()
    c.r = 8
    c.print() // change radius
    /* note that finalizers for p and c are not called */
}
```


```txt

Point at (0, 0)
Point at (1, 0)
Point at (2, 3)
Point at (3, 4)
Circle at center (0, 0), radius 0
Circle at center (1, 0), radius 0
Circle at center (2, 0), radius 3
Circle at center (4, 5), radius 6
Circle at center (7, 8), radius 9

Finalizing Point at (3, 4)...
Finalizing Point at (3, 4)...
Finalizing Point at (2, 3)...
Finalizing Point at (1, 0)...
Finalizing Point at (0, 0)...
Finalizing Circle at center (7, 8), radius 9...
Finalizing Circle at center (7, 8), radius 9...
Finalizing Circle at center (4, 5), radius 6...
Finalizing Circle at center (2, 0), radius 3...
Finalizing Circle at center (1, 0), radius 0...
Finalizing Circle at center (0, 0), radius 0...

Point at (5, 6)
Point at (5, 7)
Circle at center (5, 6), radius 7
Circle at center (5, 6), radius 8

```



## Lua

Lua does not have a standard definition of objects or classes, so a basic and typical protoctype-based OOP model will be used. In Lua all objects are tables, and through the use of metatables, polymorphism can be achieved in many ways, this is only one of them.

```Lua
-- Point
local Point = {x = 0, y = 0}

function Point:new(o)
    o = o or {}
    setmetatable(o, self)
    self.__index = self
    return o
end

function Point:print()
    print("Point(" .. self.x .. ", " .. self.y .. ")")
end

function Point:copy()
    return Point:new{x = self.x, y = self.y}
end

-- Circle
local Circle = Point:new()
Circle.r = 0

function Circle:print()
    print("Circle(" .. self.x .. ", " .. self.y .. ", " .. self.r .. ")")
end

function Circle:copy()
    return Circle:new{x = self.x, y = self.y, r = self.r}
end
```




## M2000 Interpreter

For OOP in M2000 we use Groups (we can use COM objects for other reasons, but for OOP we have Groups). A Group is collection of members. We can add permanently or temporary members, but we can't delete (using temporary members, means we delete members, but it isn't the the same as a free delete of any member).

Classes are functions which return groups. Groups are value types, not reference, but we can use pass by reference, for group  or for any member (value type plus reference to functions), and also we can use group pointers (pointers can change group which points, references can't change and always reference a named group). We can make a group from other group, just using a =.

Named group means a static group. Float group is a group in a container, like an array item. A pointer to group can be one of two kind, a pointer to named group and a pointer to a float group. A pointer act as container too. A group may have any level of nested groups, and some of them maybe are pointers to groups. References can't be stored, except as strings as weak references, and before use them we have to link again. Pointers which points to named groups has same issue, they use weak reference. A group pointer may change type, to float or to named group, but stay as pointer.

Properties are groups with values inside groups. We can use variables, but properties have a private variable [name] and has a Set and a Value part. We can define properties with Value/Set, Value, Set, or both automatic (here we do that for x,y and r)

We see polymorphism for print method (module in M2000), and for operator "=". Also Constructor in Circle read types of arguments and respond accordingly as programmed. In M2000 we can read arguments later, from stack of values, so we can check this stack.

0~ is single zero. So x, y and r get first value as single type, and stay that. Numeric types can be double (default, no symbol), Decimal (@),Currency(#),Long(&),Integer(%). For Strings we have to use $ in names, for variables and functions. There are groups which have both names, numeric and string, when they return string value. We can make a string property, and interpreter make a group which return/get string. We can add modules/functions to properties, using Group x {...}, inside a group definition (a class has a group definition also).

A Class: label direct interpreter to not include any after in the returned group, a float group, which return a Class function.
A class function is global by default, except in a class definition which is a member of group.
In following examples there is a block for temporary objects. We make a MM as a group, and at the exit of the block, group erased, so next time we make a new one.
Syntax:

```M2000 Interpreter

\\ block For This {}, or For object [, object2] { }, where object is a group, or a pointer to group, or an item from an array contains a group
\\ This is "this context".
For This {
\\ any new definition here has a temporary use
\\ can be nested, but if we use object then we can use dots to access members of it. If we use a second one then we have to use double dots (..x for second object, for access to x member)
}

```





```M2000 Interpreter

Class PointA {
      Property x=0~
      Property Y=0~
      Operator "=" (n1) {
            n=group(n1)
            if n.x=.x Then if n.y=.y then  push true : exit
            push false
      }
      Module Print  {
             Print "Point" , .x, .y
      }
Class:
      Module PointA {
            \\ ? means optionally
            Read  ? .[x], .[y]
      }
}
Class Circle {
      Property R=300~   ' type single
      Operator "=" (n1) {
            n=group(n1)
            n2=This  ' get a copy of this to check  n against n2
            if valid(@n as n2) else push false :exit
            if n.x=.x Then if n.y=.y then if n.r=.r then push true : exit
            push false
      }
      Module Print {
            Print "Circle", .x, .y, .r
      }
Class:
      Module Circle {
            if match("nn") then {
                  M=PointA(Number, Number)
            } Else.if match("G") then {
               M=PointA()
                Read M
            } Else M=PointA()
            M=This
\\            If match("N") then Read M.r   \\ check if a number is in top of stack
\\            Read  ? M.r     \\ optionally
            Read M.r   \\ for this example, r has value, so this used if stack is empty.
            This=M
      }
}
A=PointA(10,3)
C=Circle(20,10,5)
D=Circle(A, 100)
B=A
K=PointA()
Z=Circle(A)
P=PointA(600,700)

\\ N is a pointer to array
N=(A, B, C, D, K, P, Z)
M=each(N)
While M {
      For This {
            \\ a copy in MM
            MM=Array(M)
            MM.Print
            Print A=MM, D=MM    ' using MM=A interpreter use "=" from MM
      }
}

\\ pA is a pointer to D (a named group)
pA->D
Print pA=D, pA=Z
pA=>Print
\\ pA is a pointer to a copy of D (a float group)
pA->(D)
Print pA=D, pA=Z
pA=>Print

\\ rA is a reference to D  (& is optional in Link statement)
Link &D to &rA
rA.Print


```


Changes for PointA, we use variables, for Circle R has a limit of 1000. We use Stack object, and Inventory for copies of named groups, they changed to float groups.

```M2000 Interpreter

Class PointA {
      X=0~, Y=0~
      Module Print  {
             Print "Point" , .x, .y
      }
Class:
      Module PointA {
            Read  ? .x, .y
      }
}
Class Circle {
      Property R {
            Value,
            Set {
                  If Value>1000 then Value=1000
            }
      }=300~
      Module Print {
            Print "Circle", .x, .y, .r
      }
Class:
      Module Circle {
            if match("nn") then {
                  M=PointA(Number, Number)
            } Else.if match("G") then {
                M=PointA()
                Read M
            } Else M=PointA()
            M=This
            This=M
            Read ? .r
      }
}
A=PointA(10,3)
C=Circle(20,10,5)
D=Circle(A, 100)
B=A
K=PointA()
Z=Circle(A)
P=PointA(600,700)

 \\ N is a pointer to stack
N=Stack:=A, B, C, D, K, P, Z
\\ M is a pointer to an iterator
M=each(N)
While M {
      For This {
            \\ a copy in MM
            MM=StackItem(M)
            MM.Print
      }
}
\\ NN is a pointer to Inventory
Inventory NN= 1:=A, 2:=B, 3:=C, 4:=D, 5:=K, 6:=P,7:= Z
M=each(NN)
While M {
      For This {
            \\ a copy in MM
            MM=Eval(M)
            MM.Print
      }
}
\\ we can call NN(3).print
Print "NN(3).Print"
NN(3).Print
NN(3).R=5000
NN(3).Print


```



## NetRexx

'''Note:''' Based on default values in method prototypes, NetRexx will automatically generate intermediate constructors and methods, thus ensuring that none are omitted.

```NetRexx
/* NetRexx */

options replace format comments java crossref savelog symbols binary

-- -----------------------------------------------------------------------------
class RCPolymorphism public final

method main(args = String[]) public constant

  parry = [Point                    -
      Point()                       -
    , Point(1.0)                    -
    , Point(1.0, 2.0)               -
    , Point(Point(0.3, 0.2))        -
    , Circle()                      -
    , Circle(2.0, 2.0)              -
    , Circle(5.0, 6.0, 7.0)         -
    , Circle(Point(8.0, 9.0))       -
    , Circle(Point(8.0, 9.0), 4.0)  -
    , Circle(Circle(1.5, 1.4, 1.3)) -
  ]

  loop pp = 0 to parry.length - 1
    parry[pp].print
    end pp

  return

-- -----------------------------------------------------------------------------
class RCPolymorphism.Point public binary

properties private
  x = double
  y = double
  className = Point.class.getSimpleName

method Point(x_ = double 0.0, y_ = double 0.0)
  setX(x_)
  setY(y_)
  return

method Point(p = Point)
  this(p.getX, p.getY)
  return

method display public returns String
  hx = '@'Rexx(Integer.toHexString(hashCode())).right(8, 0)
  str = Rexx(className).left(10)':'hx': (x,y) = (' || -
        Rexx(getX()).format(null, 3)',' -
        Rexx(getY()).format(null, 3)')'
  return str

method getX public returns double
  return x

method getY public returns double
  return y

method setX(x_ = double 0.0) inheritable
  x = x_
  return

method setY(y_ = double 0.0) inheritable
  y = y_
  return

method print inheritable
  say display
  return

-- -----------------------------------------------------------------------------
class RCPolymorphism.Circle public extends RCPolymorphism.Point binary

properties private
  r = double
  className = Circle.class.getSimpleName

method Circle(x_ = double 0.0, y_ = double 0.0, r_ = double 0.0)
  super(x_, y_)
  setR(r_)
  return

method Circle(p_ = RCPolymorphism.Point, r_ = double 0.0)
  this(p_.getX, p_.getY, r_)
  return

method Circle(c_ = Circle)
  this(c_.getX, c_.getY, c_.getR)
  return

method getR public returns double
  return r

method setR(r_ = double 0.0) inheritable
  r = r_
  return

method display public returns String
  hx = '@'Rexx(Integer.toHexString(hashCode())).right(8, 0)
  str = Rexx(className).left(10)':'hx': (x,y,r) = (' || -
        Rexx(getX()).format(null, 3)',' -
        Rexx(getY()).format(null, 3)',' -
        Rexx(getR()).format(null, 3)')'
  return str

```

```txt

Point     :@0eb42cbf: (x,y) = (0.000, 0.000)
Point     :@17dfafd1: (x,y) = (1.000, 0.000)
Point     :@3343c8b3: (x,y) = (1.000, 2.000)
Point     :@272d7a10: (x,y) = (0.300, 0.200)
Circle    :@1aa8c488: (x,y,r) = (0.000, 0.000, 0.000)
Circle    :@3dfeca64: (x,y,r) = (2.000, 2.000, 0.000)
Circle    :@22998b08: (x,y,r) = (5.000, 6.000, 7.000)
Circle    :@0e76cbf7: (x,y,r) = (8.000, 9.000, 0.000)
Circle    :@1948cc8c: (x,y,r) = (8.000, 9.000, 4.000)
Circle    :@7a6d084b: (x,y,r) = (1.500, 1.400, 1.300)

```



## Nim


Similar to the Python solution:

```nim
type
  Point = object
    x, y: float

  Circle = object
    center: Point
    radius: float

# Constructors
proc createPoint(x, y = 0.0): Point =
  result.x = x
  result.y = y

proc createCircle(x, y = 0.0, radius = 1.0): Circle =
  result.center.x = x
  result.center.y = y
  result.radius = radius

var p1 = createPoint()
echo "p1: ", p1 # We use the default $ operator for printing
var p2 = createPoint(3, 4.2)
var p3 = createPoint(x = 2)
var p4 = createPoint(y = 2.5)

p2 = p4
p3 = createPoint()

var c1 = createCircle()
echo "c1: ", c1
var c2 = createCircle(2, 0.5, 4.2)
var c3 = createCircle(x = 2.1, y = 2)
var c4 = createCircle(radius = 10)

c1.center.x = 12
c1.radius = 5.2
```

```txt
p1: (x: 0.0, y: 0.0)
c1: (center: (x: 0.0, y: 0.0), radius: 1.0)
```



## Objeck


```objeck

bundle Default {
  class Point {
    @x : Int;
    @y : Int;

    New() {
      @x := 0;
      @y := 0;
    }

    New(x : Int, y : Int) {
      @x := x;
      @y := y;
    }

    New(p : Point) {
      @x := p->GetX();
      @y := p->GetY();
    }

    method : public : GetX() ~ Int {
      return @x;
    }

    method : public : GetY() ~ Int {
      return @y;
    }

    method : public : SetX(x : Int) ~ Nil {
      @x := x;
    }

    method : public : SetY(y : Int) ~ Nil {
      @y := y;
    }

    method : public : Print() ~ Nil {
      "Point"->PrintLine();
    }
  }


  class Circle from Point {
    @r : Int;

    New() {
      Parent();
      @r := 0;
    }

    New(p : Point) {
      Parent(p);
      @r := 0;
    }

    New(c : Circle) {
      Parent(c->GetX(), c->GetY());
      @r := c->GetR();
    }

    method : public : GetR() ~ Int {
      return @r;
    }

    method : public : SetR(r : Int) ~ Nil {
      @r := r;
    }

    method : public : Print() ~ Nil {
      "Circle"->PrintLine();
    }
  }

  class Poly {
    function : Main(args : String[]) ~ Nil {
      p := Point->New();
      c := Circle->New();
      p->Print();
      c->Print();
    }
  }
}

```


=={{header|Objective-C}}==

```objc>#import <Foundation/Foundation.h


@interface RCPoint : NSObject {
  int x, y;
}
-(instancetype)initWithX:(int)x0;
-(instancetype)initWithX:(int)x0 andY:(int)y0;
-(instancetype)initWithPoint:(RCPoint *)p;
@property (nonatomic) int x;
@property (nonatomic) int y;
@end

@implementation RCPoint
@synthesize x, y;
-(instancetype)initWithX:(int)x0 { return [self initWithX:x0 andY:0]; }
-(instancetype)initWithX:(int)x0 andY:(int)y0 {
  if ((self = [super init])) {
    x = x0;
    y = y0;
  }
  return self;
}
-(instancetype)initWithPoint:(RCPoint *)p { return [self initWithX:p.x andY:p.y]; }
-(NSString *)description { return [NSString stringWithFormat:@"<RCPoint %p x: %d y: %d>", self, x, y]; }
@end

@interface RCCircle : RCPoint {
  int r;
}
-(instancetype)initWithCenter:(RCPoint *)p andRadius:(int)r0;
-(instancetype)initWithX:(int)x0 andY:(int)y0 andRadius:(int)r0;
-(instancetype)initWithCircle:(RCCircle *)c;
@property (nonatomic) int r;
@end

@implementation RCCircle
@synthesize r;
-(instancetype)initWithCenter:(RCPoint *)p andRadius:(int)r0 {
  if ((self = [super initWithPoint:p])) {
    r = r0;
  }
  return self;
}
-(instancetype)initWithX:(int)x0 andY:(int)y0 andRadius:(int)r0 {
  if ((self = [super initWithX:x0 andY:y0])) {
    r = r0;
  }
  return self;
}
-(instancetype)initWithCircle:(RCCircle *)c { return [self initWithX:c.x andY:c.y andRadius:c.r]; }
-(NSString *)description { return [NSString stringWithFormat:@"<RCCircle %p x: %d y: %d r: %d>", self, x, y, r]; }
@end

int main(int argc, const char *argv[]) {
  @autoreleasepool {

    NSLog(@"%@", [[RCPoint alloc] init]);
    NSLog(@"%@", [[RCPoint alloc] initWithX:3]);
    NSLog(@"%@", [[RCPoint alloc] initWithX:3 andY:4]);
    NSLog(@"%@", [[RCCircle alloc] init]);
    NSLog(@"%@", [[RCCircle alloc] initWithX:3]);
    NSLog(@"%@", [[RCCircle alloc] initWithX:3 andY:4]);
    NSLog(@"%@", [[RCCircle alloc] initWithX:3 andY:4 andRadius:7]);
    RCPoint *p = [[RCPoint alloc] initWithX:1 andY:2];
    NSLog(@"%@", [[RCCircle alloc] initWithPoint:p]);
    NSLog(@"%@", [[RCCircle alloc] initWithCenter:p andRadius:7]);
    NSLog(@"%d", p.x); // 1
    p.x = 8;
    NSLog(@"%d", p.x); // 8

  }
  return 0;
}
```



## OCaml


```ocaml
class point ?(x=0.0) ?(y=0.0) () = (* extra () used to erase the optional parameters *)
object (self)
  val mutable x = x
  val mutable y = y

  method x = x
  method y = y
  method set_x x' = x <- x'
  method set_y y' = y <- y'
  method print = Printf.sprintf "Point (%f, %f)" x y
  method copy = {< >}
end

class circle ?(r=1.0) ?(x=0.0) ?(y=0.0) () =
object (self)
  inherit point ~x:x ~y:y ()
  val mutable r = r

  method r = r
  method set_r r' = r <- r'
  method print = Printf.sprintf "Circle (%f, %f, %f)" r x y
end

let print x = print_endline x#print

let () =
  let p = new point () in
  let c = new circle () in
    print c;
    print p;
    c#set_x 10.0;
    print c;
    print (new point ~y:2.1 ())
```



## Oforth


A Circle should not inherit from a Point (perhap's have a Point attribute as its center).

Let's just have x and y as Circle attributes.

Oforth polymorphism does not require the two classes to have the same hierarchy. Polymorphism is resolved at runtime : if an object respond to a method, the call is valid, otherwise an exception is raised.

As points and circles are declared as immutable objects (oforth default behavior), there is no need to copy them.
No destructors : garbage collector handles objects dstruction.

There is no default constructor : each time new is used on a class, initialize is called, so paramaters should be the same. In order to have other constructors, class methods have to be declared (see newFromPoint method).

Last point : print method (and println method) is already defined and call <<. So << is declared into each class.



```Oforth
Object Class new: Point(x, y)
Point method: initialize(x, y)  x := x y := y ;
Point method: _x   @x ;
Point method: _y   @y ;
Point method: <<   "(" << @x << ", " << @y << ")" << ;

Object Class new: Circle(x, y, r)
Circle method: initialize(x, y, r)  x := x y := y r := r ;
Circle method: _x  @x ;
Circle method: _y  @y ;
Circle method: _r  @r ;
Circle method: <<  "(" << @x << ", " << @y << ", " << @r << ")" << ;

Circle classMethod: newFromPoint(aPoint, r)  self new(aPoint _x, aPoint _y, r) ;
```


Usage :

```Oforth
: testPoly
| p c |
   Point new(3, 4) ->p
   p println
   System.Out "Attributes of this point are : " << p _x << " and " << p _y << cr
   Circle new(5, 6, 7.1) ->c
   c println
   System.Out "Attributes of this circle are : " << c _x << ",  " << c _y << " and " << c _r << cr
   Circle newFromPoint(p, 2) println ;
```


```txt

(3, 4)
Attributes of this point are : 3 and 4
(5, 6, 7.1)
Attributes of this circle are : 5,  6 and 7.1
(3, 4, 2)

```



## ooRexx

ooRexx supports traditional class-based polymorphism.  The polymorphic methods can be part of the main class sequence or brought in using mixins for multiple inheritance situations.  Here is a simple example using point and circle classes in a hierarchy.


```ooRexx

p = .point~new(3,2)
c = .circle~new(,2,6)

p~print
c~print

::class point
::method init
  expose x y
  use strict arg x = 0, y = 0   -- defaults to 0 for any non-specified coordinates

::attribute x
::attribute y

::method print
  expose x y
  say "A point at location ("||x","y")"

::class circle subclass point
::method init
  expose radius
  use strict arg x = 0, y = 0, radius = 0
  self~init:super(x, y)        -- call superclass constructor

::attribute radius

::method print
  expose radius
  say "A circle of radius" radius "centered at location ("||self~x","self~y")"


```

```txt

A point at location (3,2)
A circle of radius 6 centered at location (0,2)

```



Method binding in ooRexx is late and dynamic.  In many situations, polymorphism can be achieved merely by providing an expected method.  It is not necessary for an object to be of a particular class hierarchy.  In the example below, both point and circle implement a print method, but there is no class relationship between these classes other than what they inherit from the object class.


```ooRexx

p = .point~new(3,2)
c = .circle~new(,2,6)

p~print
c~print

::class point
::method init
  expose x y
  use strict arg x = 0, y = 0   -- defaults to 0 for any non-specified coordinates

::attribute x
::attribute y

::method print
  expose x y
  say "A point at location ("||x","y")"

::class circle
::method init
  expose x y radius
  use strict arg x = 0, y = 0, radius = 0

::attribute radius
::attribute x
::attribute y

::method print
  expose radius x y
  say "A circle of radius" radius "centered at location ("||x","y")"


```

```txt

A point at location (3,2)
A circle of radius 6 centered at location (0,2)

```




## OxygenBasic

Also uses method overloading, named parameters in the constructors, inheritance with method overrides.

Other primitives convert to floats automatically

A compact format for the methods is used to improve layout.

```oxygenbasic

type tpoint  float xx,yy
type tcircle float xx,yy,rr

'
### ====

class point
'
### ====

'
has tpoint
'
method constructor  (float x=0,y=0){this<=x,y}
method destructor   {}
method V() as point {return @this}
method V(tpoint*a)  {this<=a.xx,a.yy}
method V(point *a)  {this<=a.xx,a.yy}
method X() as float {return xx}
method Y() as float {return yy}
method X(float a)   {xx=a}
method Y(float a)   {yy=a}
method clear()      {this<=.0,.0}
method show() as string {return "x=" xx ", y=" yy }
'
end class


'
### =====

class circle
'
### =====

'
has point
float rr
'
method constructor  (float x=.0,y=.0,r=1.0){this<=x,y,r}
method V(tcircle*a) {this<=a.xx,a.yy,a.rr}
method V(circle *a) {this<=a.xx,a.yy,a.rr}
method R() as float {return rr}
method R(float a)   {rr=a}
method clear()      {this<=.0,.0,.0}
method show() as string {return "x=" xx ", y=" yy ", r=" rr }
'
end class

'=====
'TESTS
'=====

new circle ca (r=.5)
new circle cb (x=10,y=10)
new circle cc (10,10,0.5)

cb.r="7.5" 'will convert a string value

cb.y=20

print cb.show 'result x=10, y=20 ,r=7.5

del ca : del cb : del cc

```



## Oz

No inheritance necessary for polymorphism, so we don't use it here (a circle is certainly not a point). Default constructors are implemented by named default arguments.
No accessors because we use immutable public attributes ("features").


```oz
class Point
   feat
      x
      y

   meth init(x:X<=0.0 y:Y<=0.0)
      self.x = X
      self.y = Y
   end

   meth print
      {System.showInfo
       "Point("#
       "x:"#self.x#
       ", y:"#self.y#
       ")"}
   end
end

class Circle
   feat
      center
      r

   meth init(center:C<={New Point init} r:R<=1.0)
      self.center = C
      self.r = R
   end

   meth print
      {System.showInfo
       "Circle("#
       "x:"#self.center.x#
       ", y:"#self.center.y#
       ", r:"#self.r#
       ")"}
   end
end
```



## Pascal

See [[Polymorphism#Delphi | Delphi]]


## Perl

What polymorphic function means in the context of Perl is as clear as mud. subs already can take anything as parameter by default. Destructors are automatic, so I dropped them.

```perl
{
     package Point;
     use Class::Spiffy -base;
     use Clone qw(clone);

     sub _print {
         my %self = %{shift()};
         while (my ($k,$v) = each %self) {
             print "$k: $v\n";
         }
     }

     sub members {
         no strict;
         grep {
             1 == length and defined *$_{CODE}
         } keys %{*{__PACKAGE__."\::"}};
     }

     sub new {
         my $class = shift;
         my %param = @_;
         $param{$_} = 0 for grep {!defined $param{$_}} members;
         bless \%param, $class;
     }

     sub copy_constructor {
         clone shift;
     }

     sub copy_assignment {
         my $self = shift;
         my $from = shift;
         $self->$_($from->$_) for $from->members;
     }

     field 'x';
     field 'y';
}

{
     package Circle;
     use base qw(Point);
     field 'r';
}

{
     package main;
     $_->_print, print "\n" for (
        Point->new,
        Point->new(x => 2),
        Point->new(y => 3),
        Point->new(x => 8, y => -5),
     );
     my $p1 = Point->new(x => 8, y => -5);

     my $p2 = $p1->copy_constructor;
     print "we are really different objects, not just references ".
           "to the same instance\n" unless \$p1 eq \$p2;

     # accessors autogenerated
     $p1->x(1);
     $p1->y(2);
     print $p1->x, "\n";
     print $p1->y, "\n";

     $p2->copy_assignment($p1);
     print $p2->x, "\n";
     print $p2->y, "\n";
     print "we now have the same values, but we are still ".
           "different objects\n" unless \$p1 eq \$p2;

     $_->_print, print "\n" for (
        Circle->new,
        Circle->new(x => 1),
        Circle->new(y => 2),
        Circle->new(r => 3),
        Circle->new(x => 4, y => 5),
        Circle->new(x => 6, r => 7),
        Circle->new(y => 8, r => 9),
        Circle->new(x => 1, y => 2, r => 3),
     );

     my $c = Circle->new(r => 4);
     print $c->r, "\n"; # accessor autogenerated
}
```



## Perl 6

All appropriate constructors, initializers, accessors, and destructors are provided by default, but may be explicitly declared for flexibility.
To create only readonly accessors for better encapsulation, leave out all the "is rw" traits.
Here we demonstrate that accessors can behave like variables and may be assigned.

```perl6
class Point {
    has Real $.x is rw = 0;
    has Real $.y is rw = 0;
    method Str { $.perl }
}

class Circle {
    has Point $.p is rw = Point.new;
    has Real $.r is rw = 0;
    method Str { $.perl }
}

my $c = Circle.new(p => Point.new(x => 1, y => 2), r => 3);
say $c;
$c.p.x = (-10..10).pick;
$c.p.y = (-10..10).pick;
$c.r   = (0..10).pick;
say $c;
```

In this case we define the Str coercion method polymorphically, which is used by say or print to format the contents of the object.
We could also have defined print methods directly.
We could have factored this method out to a common role and composed it into each class.
We could also have defined multi subs outside of the class, like this:

```perl6
multi print (Point $p) { $p.perl.print }
multi print (Circle $c) { $c.perl.print }
```



## Phix

Phix is not object orientated, but naturally polymorphic.

Destructors are not required, though you can use delete_routine if needed.

Copy constructors are also not required, a plain '=' will do just fine (it uses copy on write semantics).

You could embed routine_ids in the structures to emulate virtual functions.

There are no private members here; for that I would write something that returns integer ids to the outside world.

```Phix
type point(object o)
    return sequence(o) and length(o)=2 and atom(o[1]) and atom(o[2])
end type

function new_point(atom x=0, atom y=0)
    return {x,y}
end function

type circle(object o)
    return sequence(o) and length(o)=2 and point(o[1]) and atom(o[2])
end type

function new_circle(object x=0, atom y=0, atom r=0)
    if point(x) then
        r = y           -- assume r got passed in y
        return {x,r}    -- {point,r}
    end if
    return {{x,y},r}    -- {point,r}
    -- (or {new_point(x,y),r} if you prefer)
end function

point p = new_point(4,5)
circle c1 = new_circle(p,6),
       c2 = new_circle(4,5,6}
?c1
?c2
```

```txt

{{4,5},6}
{{4,5},6}

```



## PHP

'print' is a reserved keyword in PHP so the method to print is called 'output'. Alternatively the Point and Circle objects can be converted to a string representation by simply printing / echo'ing the object because the objects implement the magic '__toString' method.

Point class definition.


```PHP

class Point
{
  protected $_x;

  protected $_y;

  public function __construct()
  {
    switch( func_num_args() )
    {
      case 1:
        $point = func_get_arg( 0 );
        $this->setFromPoint( $point );
        break;
      case 2:
        $x = func_get_arg( 0 );
        $y = func_get_arg( 1 );
        $this->setX( $x );
        $this->setY( $y );
        break;
      default:
        throw new InvalidArgumentException( 'expecting one (Point) argument or two (numeric x and y) arguments' );
    }
  }

  public function setFromPoint( Point $point )
  {
    $this->setX( $point->getX() );
    $this->setY( $point->getY() );
  }

  public function getX()
  {
    return $this->_x;
  }

  public function setX( $x )
  {
    if( !is_numeric( $x ) )
    {
      throw new InvalidArgumentException( 'expecting numeric value' );
    }

    $this->_x = (float) $x;
  }

  public function getY()
  {
    return $this->_y;
  }

  public function setY( $y )
  {
    if( !is_numeric( $y ) )
    {
      throw new InvalidArgumentException( 'expecting numeric value' );
    }

    $this->_y = (float) $y;
  }

  public function output()
  {
    echo $this->__toString();
  }

  public function __toString()
  {
    return 'Point [x:' . $this->_x . ',y:' . $this->_y . ']';
  }
}

```


Circle class definition.


```PHP

class Circle extends Point
{
  private $_radius;

  public function __construct()
  {
    switch( func_num_args() )
    {
      case 1:
        $circle = func_get_arg( 0 );
        $this->setFromCircle( $circle );
        break;
      case 2:
        $point = func_get_arg( 0 );
        $radius = func_get_arg( 1 );
        $this->setFromPoint( $point );
        $this->setRadius( $radius );
        break;
      case 3:
        $x = func_get_arg( 0 );
        $y = func_get_arg( 1 );
        $radius = func_get_arg( 2 );
        $this->setX( $x );
        $this->setY( $y );
        $this->setRadius( $radius );
        break;
      default:
        throw new InvalidArgumentException( 'expecting one (Circle) argument or two (Point and numeric radius) or three (numeric x, y and radius) arguments' );
    }
  }

  public function setFromCircle( Circle $circle )
  {
    $this->setX( $circle->getX() );
    $this->setY( $circle->getY() );
    $this->setRadius( $circle->getRadius() );
  }

  public function getPoint()
  {
    return new Point( $this->getX(), $this->getY() );
  }

  public function getRadius()
  {
    return $this->_radius;
  }

  public function setRadius( $radius )
  {
    if( !is_numeric( $radius ) )
    {
      throw new InvalidArgumentException( 'expecting numeric value' );
    }

    $this->_radius = (float) $radius;
  }

  public function __toString()
  {
    return 'Circle [' . $this->getPoint() . ',radius:' . $this->_radius . ']';
  }
}

```


Usage:


```PHP

$point = new Point( 1, 5 );
$circle = new Circle( 1, 5, 6 );

$point->output();
// or
echo $point;

echo "\n";

$circle->output();
// or
echo $circle;

```


Will result in:

Point [x:1,y:5]

Circle [Point [x:1,y:5],radius:6]


## PicoLisp


```PicoLisp
(class +Point)
# x y

(dm T (X Y)
   (=: x (or X 0))
   (=: y (or Y 0)) )

(dm print> ()
   (prinl "Point " (: x) "," (: y)) )

(class +Circle +Point)
# r

(dm T (X Y R)
   (super X Y)
   (=: r (or R 0)) )

(dm print> ()
   (prinl "Circle " (: x) "," (: y) "," (: r)) )
```


```PicoLisp
(setq
   P (new '(+Point) 3 4)
   C (new '(+Circle) 10 10 5) )

(print> P)
(print> C)
```

```txt
Point 3,4
Circle 10,10,5
```



## Pop11

When a class is defined in Pop11, it automatically defines default constructors, slot accessors and copy operations.
So it is enough to define classes and the print method.


```pop11
uses objectclass;
define :class Point;
    slot x = 0;
    slot y = 0;
enddefine;

define :class Circle;
    slot x = 0;
    slot y = 0;
    slot r = 1;
enddefine;

define :method print(p : Point);
    printf('Point(' >< x(p) >< ', ' >< y(p) >< ')\n');
enddefine;

define :method print(p : Circle);
    printf('Circle(' >< x(p) >< ', ' >< y(p) >< ', ' >< r(p) >< ')\n');
enddefine;
```


To test we can use the following code:


```pop11
;;; Initialize variables using default constructors
lvars instance1 = newPoint();
lvars instance2 = newCircle();
;;; Use print method
print(instance1);
print(instance2);
```



## PureBasic

Using the open-source precompiler [http://www.development-lounge.de/viewtopic.php?t=5915 SimlpeOOP].

```PureBasic
Class MyPoint

  BeginProtect
    x.i
    y.i
  EndProtect

  Public Method GetX()
    MethodReturn This\X
  EndMethod

  Public Method GetY()
    MethodReturn This\Y
  EndMethod

  Public Method SetX(n)
    This\X=n
  EndMethod

  Public Method SetY(n)
    This\Y=n
  EndMethod

  Public Method Print()
    PrintN("Point")
  EndMethod

  Public Method Init(x=0,y=0)
    This\x=x
    This\y=y
  EndMethod
EndClass

Class Circle Extends MyPoint

  Protect  Radie.i

  Public Method Circel(x=0, y=0, r=0)
    This\X  = x
    This\y  = y
    This\Radie=r
  EndMethod

  Public Method GetRadie()
    MethodReturn This\Radie
  EndMethod

  Public Method SetRadie(n)
    This\Radie = n
  EndMethod

  Public Method Print()
    PrintN("Circle: "+ _
    " X= "+Str(This\X)+ _
    " Y= "+Str(This\Y)+ _
    " R= "+Str(This\Radie))
  EndMethod

EndClass
```

Testcode

```PureBasic
*point.MyPoint = NewObject.MyPoint
*circle.Circle = NewObject.Circle

If OpenConsole()
  *point\Print()
  *circle\SetX(3)
  *circle\Print()
  CloseConsole()
EndIf
```



## Python

Multiple constructors are not needed because Python supports default values for arguments.
Accessors are not needed because Python attributes are public.
It is possible to add managed attributes later without changing the interface and existing client code.
For the print function, use the standard __repr__ methods, used when printing an object. Destructors are not needed of course.


```python
class Point(object):
    def __init__(self, x=0.0, y=0.0):
        self.x = x
        self.y = y
    def __repr__(self):
        return '<Point 0x%x x: %f y: %f>' % (id(self), self.x, self.y)

class Circle(object):
    def __init__(self, center=None, radius=1.0):
        self.center = center or Point()
        self.radius = radius
    def __repr__(self):
        return '<Circle 0x%x x: %f y: %f radius: %f>' % (
            id(self), self.center.x, self.center.y, self.radius)
```


Usage example:

```txt

>>> from polymorphism import Point, Circle
>>> p1 = Point()
>>> Point()
<Point 0x5b1b0 x: 0.000000 y: 0.000000>
>>> Point(3, 4)
<Point 0x5b0f0 x: 3.000000 y: 4.000000>
>>> Point(y=4)
<Point 0x5b0b0 x: 0.000000 y: 4.000000>
>>> Point(x=3)
<Point 0x5b1b0 x: 3.000000 y: 0.000000>
>>> Circle()
<Circle 0x5b330 x: 0.000000 y: 0.000000 radius: 1.000000>
>>> Circle(Point(3,4))
<Circle 0x5b3b0 x: 3.000000 y: 4.000000 radius: 1.000000>
>>> Circle(Point(3,4), 7)
<Circle 0x5b3d0 x: 3.000000 y: 4.000000 radius: 7.000000>
>>> Circle(radius=10)
<Circle 0x5b0f0 x: 0.000000 y: 0.000000 radius: 10.000000>
>>> Circle(center=Point(127,0))
<Circle 0x5b0b0 x: 127.000000 y: 0.000000 radius: 1.000000>
>>> p = Point(1.25, 3.87)
>>> p
<Point 0x5b3d0 x: 1.250000 y: 3.870000>
>>> p.x = 10.81
>>> p
<Point 0x5b3d0 x: 10.810000 y: 3.870000>
>>> c = Circle(p, 21.4)
>>> c
<Circle 0x5b0b0 x: 10.810000 y: 3.870000 radius: 21.400000>
>>> c.center.x = 1.0
>>> c
<Circle 0x5b0b0 x: 1.000000 y: 3.870000 radius: 21.400000>

```


Or, using inheritance like some of the other solutions:

```python
class Point(object):
    def __init__(self, x=0.0, y=0.0):
        self.x = x
        self.y = y
    def __repr__(self):
        return '<Point 0x%x x: %f y: %f>' % (id(self), self.x, self.y)

class Circle(Point):
    def __init__(self, x=0.0, y=0.0, radius=1.0):
        Point.__init__(self, x, y)
        self.radius = radius
    def __repr__(self):
        return '<Circle 0x%x x: %f y: %f radius: %f>' % (
            id(self), self.x, self.y, self.radius)
```


Usage example:

```txt

>>> from polymorphism import Point, Circle
>>> p1 = Point()
>>> Point()
<Point 0x5b1b0 x: 0.000000 y: 0.000000>
>>> Point(3, 4)
<Point 0x5b0f0 x: 3.000000 y: 4.000000>
>>> Point(y=4)
<Point 0x5b0b0 x: 0.000000 y: 4.000000>
>>> Point(x=3)
<Point 0x5b1b0 x: 3.000000 y: 0.000000>
>>> Circle()
<Circle 0x5b330 x: 0.000000 y: 0.000000 radius: 1.000000>
>>> Circle(3, 4)
<Circle 0x5b3b0 x: 3.000000 y: 4.000000 radius: 1.000000>
>>> Circle(3, 4, 7)
<Circle 0x5b3d0 x: 3.000000 y: 4.000000 radius: 7.000000>
>>> Circle(radius=10)
<Circle 0x5b0f0 x: 0.000000 y: 0.000000 radius: 10.000000>
>>> Circle(x=127)
<Circle 0x5b0b0 x: 127.000000 y: 0.000000 radius: 1.000000>
>>> p = Point(1.25, 3.87)
>>> p
<Point 0x5b3d0 x: 1.250000 y: 3.870000>
>>> p.x = 10.81
>>> p
<Point 0x5b3d0 x: 10.810000 y: 3.870000>
>>> c = Circle(p.x, p.y, 21.4)
>>> c
<Circle 0x5b0b0 x: 10.810000 y: 3.870000 radius: 21.400000>
>>> c.x = 1.0
>>> c
<Circle 0x5b0b0 x: 1.000000 y: 3.870000 radius: 21.400000>

```



### Mutability

The task calls for the creation of mutable types i.e. that you are allowed to change the values of x, y, or r of a Point or Circle after they have been created.
If this is not needed, then the Python namedtuple is a good way to create immutable classes with named fields such as these.

```python>>>
 from collections import namedtuple
>>> class Point(namedtuple('Point', 'x y')):
	def __new__( _cls, x=0, y=0 ):
		return super().__new__(_cls, x, y)


>>> class Circle(namedtuple('Circle', 'x y r')):
	def __new__( _cls, x=0, y=0, r=0 ):
		return super().__new__(_cls, x, y, r)


>>> Point(), Point(x=1), Point(y=2), Point(3, 4)
(Point(x=0, y=0), Point(x=1, y=0), Point(x=0, y=2), Point(x=3, y=4))
>>> Circle(), Circle(r=2), Circle(1, 2, 3)
(Circle(x=0, y=0, r=0), Circle(x=0, y=0, r=2), Circle(x=1, y=2, r=3))
>>> p = Point(1.25, 3.87)
>>> p
Point(x=1.25, y=3.87)
>>> p.x = 10.81
Traceback (most recent call last):
  File "<pyshell#27>", line 1, in <module>
    p.x = 10.81
AttributeError: can't set attribute
>>>
```


And if you don't need default arguments, this becomes:

```python>>>
 Point = namedtuple('Point', 'x y')
>>> Circle = namedtuple('Circle', 'x y r')
>>> Point(3, 4)
Point(x=3, y=4)
>>> Circle(x=1, y=2, r=3)
Circle(x=1, y=2, r=3)
>>>
```



## R

Only the S4 class system is considered here.
Copy constructors are not needed, since objects are copied by value.
Neither are destructors needed (just use the rm function).

```R
setClass("point",
   representation(
      x="numeric",
      y="numeric"),
   prototype(
      x=0,
      y=0))

# Instantiate class with some arguments
p1 <- new("point", x=3)
# Access some values
p1@x    # 3
# Define a print method
setMethod("print", signature("point"),
   function(x, ...)
   {
      cat("This is a point, with location, (", x@x, ",", x@y, ").\n")
   })
print(p1)

# Define a circle class
setClass("circle",
   representation(
      centre="point",
      r="numeric"),
   prototype(
      centre=new("point"),
      r=1))
circS4 <- new("circle", r=5.5)
# Access some values
circS4@r    # 5.5
circS4@centre@x   # 0
# Define a print method
setMethod("print", signature("circle"),
   function(x, ...)
   {
      cat("This is a circle, with radius", x@r, "and centre (", x@centre@x, ",", x@centre@y, ").\n")
   })
print(circS4)
```



## Racket

All arguments have default values provided, so every possible constructor is implicitly defined.
"Fields" come with accessors and mutators for free.


```Racket

#lang racket
(define point%
  (class* object% (writable<%>) (super-new) (init-field [x 0] [y 0])
    (define/public (copy) (new point% [x x] [y y]))
    (define/public (show) (format "<point% ~a ~a>" x y))
    (define/public (custom-write out) (write (show) out))
    (define/public (custom-display out) (display (show) out))))

(define circle%
  (class point% (super-new) (inherit-field x y) (init-field [r 0])
    (define/override (copy) (new circle% [x x] [y y] [r r]))
    (define/override (show) (format "<circle% ~a ~a>" (super show) r))
    (define/override (custom-write out) (write (show) out))
    (define/override (custom-display out) (display (show) out))))

```

```txt

> (define c (new circle% [x 3] [r 5]))
> (define dup (send c copy))
> c
"<circle% <point% 3 0> 5>"
> dup
"<circle% <point% 3 0> 5>"
> (set-field! r c 10)
> c
"<circle% <point% 3 0> 10>"
> (set-field! x c -2)
> c
"<circle% <point% -2 0> 10>"
> dup
"<circle% <point% 3 0> 5>"

```



## Ruby

We use <tt>attr_accessor</tt> to provide all the accessor and assignment operations. Default arguments eliminate the need for multiple constructors.
The built-in <tt>puts</tt> uses the object's <tt>to_s</tt> method.
The <tt>Kernel#dup</tt> method can be used as a copy constructor.


```ruby
class Point
  attr_accessor :x,:y
  def initialize(x=0, y=0)
    self.x = x
    self.y = y
  end
  def to_s
    "Point at #{x},#{y}"
  end
end

# When defining Circle class as the sub-class of the Point class:
class Circle < Point
  attr_accessor :r
  def initialize(x=0, y=0, r=0)
    self.x = x
    self.y = y
    self.r = r
  end
  def to_s
    "Circle at #{x},#{y} with radius #{r}"
  end
end
```

Example:

```ruby
# create a point
puts Point.new          # => Point at 0,0
p = Point.new(1, 2)
puts p                  # => Point at 1,2
puts p.x                # => 1
p.y += 1
puts p                  # => Point at 1,3

# create a circle
c = Circle.new(4,5,6)
# copy it
d = c.dup
d.r = 7.5
puts c                  # => Circle at 4,5 with radius 6
puts d                  # => Circle at 4,5 with radius 7.5
```



## Scala

{{Out}}Best seen running in your browser either by [https://scalafiddle.io/sf/altUDTl/0 ScalaFiddle (ES aka JavaScript, non JVM)] or [https://scastie.scala-lang.org/ohfeM4nvTZyeOTYYWHaNgQ Scastie (remote JVM)].

```Scala
object PointCircle extends App {

  class Point(x: Int = 0, y: Int = 0) {

    def copy(x: Int = this.x, y: Int = this.y): Point = new Point(x, y)

    override def toString = s"Point x: $x,  y: $y"
  }

  object Point {
    def apply(x: Int = 0, y: Int = 0): Point = new Point(x, y)
  }

  case class Circle(x: Int = 0, y: Int = 0, r: Int = 0) extends Point(x, y) {

    def copy(r: Int): Circle = Circle(x, y, r)

    override def toString = s"Circle x: $x,  y: $y,  r: $r"
  }

  val p = Point()
  val c = Circle()
  println("Instantiated ", p)
  println("Instantiated ", c)

  val q = Point(5, 6)
  println("Instantiated ", q)
  val r = q.copy(y = 7) // change y coordinate
  println(r, " changed y coordinate")

  val d = Circle(5, 6, 7)
  println("Instantiated ", d)
  val e = d.copy(r = 8) // change radius
  println(e, " changed radius")

}
```


## Seed7

[http://seed7.sourceforge.net/manual/objects.htm Seed7 object orientation] works via interfaces.
The example below introduces the interface type ''GraphicObj''. To be usable an interface type needs also interface functions (which are defined with the keyword DYNAMIC). The interface function ''print'' is defined for ''GraphicObj''. The struct types ''Point'' and ''Circle'' implement the the interface ''GraphicObj'' (they are implementation types).
Note that ''Circle'' inherits ''x'' and ''y'' from ''Point''.
Functions which return a ''Point'' respectively ''Circle'' are used as constructors.
Note that a Seed7 constructor does not need to have the name of the type (a new Point could be created with a function called abc).
Seed7 defines copy constructor, assignment and destructor automatically.

```seed7
$ include "seed7_05.s7i";

const type: GraphicObj is new interface;

const proc: print (in GraphicObj: aGraphicObj) is DYNAMIC;


const type: Point is new struct
    var integer: x is 0;
    var integer: y is 0;
  end struct;

type_implements_interface(Point, GraphicObj);

const func Point: Point (in integer: x, in integer: y) is func
  result
    var Point: newPoint is Point.value;
  begin
    newPoint.x := x;
    newPoint.y := y;
  end func;

const proc: print (in Point: aPoint) is func
  begin
    writeln("Point(" <& aPoint.x <& ", " <& aPoint.y <& ")");
  end func;


const type: Circle is sub Point struct
    var integer: r is 0;
  end struct;

type_implements_interface(Circle, GraphicObj);

const func Circle: Circle (in integer: x, in integer: y, in integer: r) is func
  result
    var Circle: newCircle is Circle.value;
  begin
    newCircle.x := x;
    newCircle.y := y;
    newCircle.r := r;
  end func;

const proc: print (in Circle: aCircle) is func
  begin
    writeln("Circle(" <& aCircle.x <& ", " <& aCircle.y <& ", " <& aCircle.r <& ")");
  end func;


const proc: main is func
  local
    var Point: pnt is Point(1, 2);
    var Circle: circ is Circle(3, 4, 5);
    var GraphicObj: graph is Point.value;
  begin
    graph := pnt;
    print(graph);
    graph := circ;
    print(graph);
  end func;
```


 Circle(3, 4, 5)


## Self


We create four named objects, two of which we put in the traits namespace and two in a general prototype namespace. This would normally be done in the UI.

```self
traits point = (|
  parent* = traits clonable.
  printString = ('Point(', x asString, ':', y asString, ')').
  |)

point = (|
  parent* = traits point.
  x <- 0.
  y <- 0
  |)

traits circle = (|
  parent* = traits clonable.
  printString = ('Circle(', center asString, ',', r asString, ')').
  |)

circle = (|
  parent* = traits circle.
  center <- point copy.
  r <- 0
  |)
```



## Sidef


```ruby
class Point(x=0, y=0) {

}

class Circle(x=0, y=0, r=0) {

}

func pp(Point obj) {
    say "Point at #{obj.x},#{obj.y}";
}

func pp(Circle obj) {
    say "Circle at #{obj.x},#{obj.y} with radius #{obj.r}";
}
```


Example:

```ruby
pp(Point.new);              # => Point at 0,0
var p = Point(1, 2);        # create a point
pp(p);                      # => Point at 1,2
say p.x;                    # => 1
p.y += 1;                   # add one to y
pp(p);                      # => Point at 1,3

var c = Circle(4,5,6);      # create a circle
var d = c.clone;            # make a clone of it
d.r = 7.5;                  # and change the radius to 7.5
pp(c);                      # => Circle at 4,5 with radius 6
pp(d);                      # => Circle at 4,5 with radius 7.5
```



## SIMPOL

In [[SIMPOL]] any type can be declared to be tagged with a name.
That name can then be used to define a variable that can hold a reference to any type tagged with the same name.
Multiple constructors are not needed, since all the parameters can be provided in the same constructor call.
Types embedded in other types resolve according to the rules in SIMPOL such that if a property name is used that is not present in the actual type definition, then a depth first search starting at the beginning of the type definition will resolve a matching property or method name of an embedded type.
This resolution is not performed on properties defined as <code>reference</code> are not used in the extended dot operator resolution mechanism unless they are also assigned the <code>resolve</code> keyword.
The <code>embed</code> keyword in the type definition states that the type itself can be embedded in another type (any type can be placed as a reference in another type).


```simpol
type mypoint(mypoint) embed export
  embed
  integer x
  integer y

  reference
  function copy
  function print
end type


function mypoint.new(mypoint me, integer x=0, integer y=0)
  me.x = x
  me.y = y
end function me


function mypoint.copy(mypoint me)
  mypoint p

  p =@ mypoint.new(me.x, me.y)
end function p


function mypoint.print(mypoint me)
end function "mypoint"


type circle(mypoint) embed export
  reference
  mypoint midpoint resolve

  embed
  integer radius

  reference
  function copy
  function print
end type


function circle.new(circle me, integer x=0, integer y=0, integer radius=0, mypoint midpoint)
  if midpoint =@= .nul
    me.midpoint =@ mypoint.new(x, y)
  else
    me.x = midpoint.x
    me.y = midpoint.y
  end if

  me.radius = radius
end function me


function circle.copy(circle me)
  circle c

  c =@ circle.new(radius=me.radius, midpoint=me.midpoint)
end function c


function circle.print(circle me)
end function "circle"


function main()
  type(mypoint) p, c
  string result

  p =@ mypoint.new()
  c =@ circle.new()

  result = p.print() + "{d}{a}" + c.print() + "{d}{a}"
end function result
```


[[SIMPOL]] does not currently have access to stdin, stdout, and stderr,
so to return a value from the program to a console,
it must be as part of the return value.


## Smalltalk

Like Python and Ruby, these objects do not need to be related in order to have polymorphic methods.

```smalltalk
!Object subclass: #Point
  instanceVariableNames: 'x y'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'polymorphism' !

!Point class methodsFor: 'instance creation'!
new
  ^self newBasic x := 0; y := 0 ! !

!Point class methodsFor: 'instance creation'!
x: x y: y
  ^self newBasic x := x; y := y ! !

!Point methodsFor: 'member access'!
x
  ^x ! !

!Point methodsFor: 'member access'!
y
  ^y ! !

!Point methodsFor: 'member access'!
x: x
  ^self x := x ! !

!Point methodsFor: 'member access'!
y: y
  ^self y := y ! !

!Point methodsFor: 'member access'!
x: x y: y
  ^self x := x; y := y ! !

!Point methodsFor: 'polymorphism test'!
print
  Transcript show: x; space; show: y ! !

!Object subclass: #Circle
  instanceVariableNames: 'center r'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'polymorphism' !

!Circle class methodsFor: 'instance creation'!
new
  ^self newBasic center := Point new; r := 0 ! !

!Circle class methodsFor: 'instance creation'!
radius: radius
  ^self newBasic center := Point new; r := radius ! !

!Circle class methodsFor: 'instance creation'!
at: point radius: r
  ^self newBasic center := point; r := r ! !

!Circle methodsFor: 'member access'!
center
  ^center ! !

!Circle methodsFor: 'member access'!
x: x y: y
  ^self center x: x y: y ! !

!Circle methodsFor: 'member access'!
radius
  ^r ! !

!Circle methodsFor: 'member access'!
radius: radius
  ^self r := radius ! !

!Circle methodsFor: 'polymorphism test'!
print
  Transcript show: center; space; show: radius ! !
```


''TODO: more idiomatic mechanism for presenting objects as strings.''
''TODO: fill in more methods''


## Swift


```swift
class RCPoint : Printable {
  var x: Int
  var y: Int
  init(x: Int = 0, y: Int = 0) {
    self.x = x
    self.y = y
  }
  convenience init(p: RCPoint) {
    self.init(x:p.x, y:p.y)
  }
  var description: String {
  return "<RCPoint x: \(self.x) y: \(self.y)>"
  }
}

class RCCircle : RCPoint {
  var r: Int
  init(p: RCPoint, r: Int = 0) {
    self.r = r
    super.init(x:p.x, y:p.y)
  }
  init(x: Int = 0, y: Int = 0, r: Int = 0) {
    self.r = r
    super.init(x:x, y:y)
  }
  convenience init(c: RCCircle) {
    self.init(x:c.x, y:c.y, r:c.r)
  }
  override var description: String {
    return "<RCCircle x: \(x) y: \(y) r: \(r)>"
  }
}

println(RCPoint())
println(RCPoint(x:3))
println(RCPoint(x:3, y:4))
println(RCCircle())
println(RCCircle(x:3))
println(RCCircle(x:3, y:4))
println(RCCircle(x:3, y:4, r:7))
let p = RCPoint(x:1, y:2)
println(RCCircle(p:p))
println(RCCircle(p:p, r:7))
println(p.x) // 1
p.x = 8
println(p.x) // 8
```



## Tcl

Since Tcl's objects have their methods invoked by sending a (potentially-interceptable) message to them, allowing them to even respond to method calls that are not explicitly declared on them, there is no need for the objects to be formally related.
We only do so here for convenience.
In addition, Tcl's arguments to commands, procedures and methods are all fully polymorphic by default.

```tcl
package require TclOO
oo::class create Point {
    variable X Y
    constructor {x y} {
        set X $x
        set Y $y
    }
    method x args {
        set X {*}$args
    }
    method y args {
        set Y {*}$args
    }
    method print {} {
        puts "Point($X,$Y)"
    }
    method copy {} {
        set copy [oo::copy [self]]
        $copy x $X
        $copy y $Y
        return $copy
    }
}
oo::class create Circle {
    superclass Point
    variable R
    constructor {x y radius} {
        next $x $y
        set R $radius
    }
    method radius args {
        set R {*}$args
    }
    method print {} {
        puts "Circle([my x],[my y],$R)"
    }
    method copy {} {
        set copy [next]
        $copy radius $R
        return $copy
    }
}
# No destructors: unneeded by these classes

set p [Point  new 1.0 2.0]
set c [Circle new 3.0 4.0 5.0]
set cCopy [$c copy]
puts "$p is at ([$p x],[$p y])"
$c radius 1.5
set objects [list $p $c $cCopy]
foreach o $objects {
    $o print
}
```



## Wollok


```wollok

class Point {
   var x
   var y
   new(ax, ay) {
       this.x = ax
       this.y = ay
   }
   new(point) {
        this(point.x, point.y)
   }
   method getX() { return x }
   method setX(newX) { x = newX }

   method getY() { return y }
   method setY(newY) { y = newY }

   method print() {
       console.println("Point")
   }
}

class Circle extends Point {
   var r

   new() { this(0,0,0) }
   new(point, aR) { super(point) ; r = aR }
   new(aX, aY, aR) { super(aX, aY); r = aR }

   method getR() { return r }
   method setR(newR) { r = newR }

   method print() {
       console.println("Circle")
   }

}

program polymorphism {
    val p = new Point()
    val c = new Circle()
    p.print()
    c.print()
}

```



## zkl

Written for brevity

```zkl
class Point{var x,y;
   fcn init(xyOrPoint=0,_=0){
      if(Point.isInstanceOf(xyOrPoint)) set(xyOrPoint);
      else x,y=vm.arglist.apply("toFloat")}
   fcn set(p){x=p.x;y=p.y}
   fcn toString{"(%d,%d)".fmt(x,y)}
}
class Circle{var center, radius;
   fcn init(a=0.0,b=0.0,r=1.0){
      switch [arglist]{
         case(Circle){ center=Point(a.center); radius=a.radius }
	 case(Point) { center=Point(a); radius=b.toFloat(); }
	 else { center=Point(a,b); radius=r.toFloat(); }
      }
   }
   fcn copy{self(self)}
   fcn toString{"(%s,%d)".fmt(center.toString(),radius)}
}
   // see if various constructors work
Point(); Point(1); Point(1,2), Point(Point());
Circle(); Circle(1); Circle(1,2); Circle(1,2,3);
Circle(Point()); Circle(Point(),1);
Circle(Circle());

c:=Circle(1,2,3);
c.println(); c.center.println();
c.copy().println();
```

```txt

((1,2),3)
(1,2)
((1,2),3)

```




{{omit from|TI-83 BASIC}} {{omit from|TI-89 BASIC}} <!-- Does not have user-defined data structures or objects. -->
