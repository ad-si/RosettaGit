+++
title = "Ray-casting algorithm"
description = ""
date = 2018-05-08T18:56:28Z
aliases = []
[extra]
id = 4235
[taxonomies]
categories = []
tags = []
+++

{{task|Classic CS problems and programs}}
{{Wikipedia|Point_in_polygon}}



Given a point and a polygon, check if the point is inside or outside the polygon using the [[wp:Point in polygon#Ray casting algorithm|ray-casting algorithm]].

A pseudocode can be simply:

  count ← 0
  '''foreach''' side '''in''' polygon:
    '''if''' ray_intersects_segment(P,side) '''then'''
      count ← count + 1
  '''if''' ''is_odd''(count) '''then'''
    '''return''' inside
  '''else'''
    '''return''' outside

Where the function <tt>ray_intersects_segment</tt> return true if the horizontal ray starting from the point P intersects the side (segment), false otherwise.

An intuitive explanation of why it works is that every time we cross
a border, we change "country" (inside-outside, or outside-inside), but
the last "country" we land on is surely ''outside'' (since the inside of the polygon is finite, while the ray continues towards infinity). So, if we crossed an odd number of borders we were surely inside, otherwise we were outside; we can follow the ray backward to see it better: starting from outside, only an odd number of crossing can give an ''inside'': outside-inside, outside-inside-outside-inside, and so on (the - represents the crossing of a border).

So the main part of the algorithm is how we determine if a ray intersects a segment. The following text explain one of the possible ways.

[[Image:intersect.png|200px|thumb|right]]
Looking at the image on the right, we can easily be convinced of the fact that rays starting from points in the hatched area (like P<sub>1</sub> and P<sub>2</sub>) surely do not intersect the segment AB. We also can easily see that rays starting from points in the greenish area surely intersect the segment AB (like point P<sub>3</sub>).

So the problematic points are those inside the white area (the box delimited by the points A and B), like P<sub>4</sub>.

[[Image:posslope.png|128px|thumb|right]]
[[Image:negslope.png|128px|thumb|right]]

Let us take into account a segment AB (the point A having y coordinate always smaller than B's y coordinate, i.e. point A is always below point B) and a point P. Let us use the cumbersome notation PAX to denote the angle between segment AP and AX, where X is always a point on the horizontal line passing by A with x coordinate bigger than the maximum between the x coordinate of A and the x coordinate of B. As explained graphically by the figures on the right, if PAX is greater than the angle BAX, then the ray starting from P intersects the segment AB. (In the images, the ray starting from P<sub>A</sub> does not intersect the segment, while the ray starting from P<sub>B</sub> in the second picture, intersects the segment).

Points on the boundary or "on" a vertex are someway special and through this approach we do not obtain ''coherent'' results. They could be treated apart, but it is not necessary to do so.

An algorithm for the previous speech could be (if P is a point, Px is its x coordinate):

  '''ray_intersects_segment''':
     P : the point from which the ray starts
     A : the end-point of the segment with the smallest y coordinate
         (A must be "below" B)
     B : the end-point of the segment with the greatest y coordinate
         (B must be "above" A)
  '''if''' Py = Ay '''or''' Py = By '''then'''
    Py ← Py + &epsilon;
  '''end''' '''if'''
  '''if''' Py < Ay '''or''' Py > By '''then''' 
    '''return''' false
  '''else''' '''if''' Px >= max(Ax, Bx) '''then''' 
    '''return''' false
  '''else'''
    '''if''' Px < min(Ax, Bx) '''then'''
      '''return''' true
    '''else'''
      '''if''' Ax ≠ Bx '''then'''
        m_red ← (By - Ay)/(Bx - Ax)
      '''else'''
        m_red ← ∞
      '''end''' '''if'''
      '''if''' Ax ≠ Px '''then'''
        m_blue ← (Py - Ay)/(Px - Ax)
      '''else'''
        m_blue ← ∞
      '''end''' '''if'''
      '''if''' m_blue ≥ m_red '''then'''
        '''return''' true
      '''else'''
        '''return''' false
      '''end''' '''if'''
    '''end''' '''if'''
  '''end''' '''if'''

(To avoid the "ray on vertex" problem, the point is moved upward of a small quantity   <big>&epsilon;</big>.)





## Ada

polygons.ads:

```Ada
package Polygons is

   type Point is record
      X, Y : Float;
   end record;
   type Point_List is array (Positive range <>) of Point;
   subtype Segment is Point_List (1 .. 2);
   type Polygon is array (Positive range <>) of Segment;

   function Create_Polygon (List : Point_List) return Polygon;

   function Is_Inside (Who : Point; Where : Polygon) return Boolean;

end Polygons;
```


polygons.adb:

```Ada
package body Polygons is
   EPSILON : constant := 0.00001;

   function Ray_Intersects_Segment
     (Who   : Point;
      Where : Segment)
      return  Boolean
   is
      The_Point        : Point   := Who;
      Above            : Point;
      Below            : Point;
      M_Red            : Float;
      Red_Is_Infinity  : Boolean := False;
      M_Blue           : Float;
      Blue_Is_Infinity : Boolean := False;
   begin
      if Where (1).Y < Where (2).Y then
         Above := Where (2);
         Below := Where (1);
      else
         Above := Where (1);
         Below := Where (2);
      end if;
      if The_Point.Y = Above.Y or The_Point.Y = Below.Y then
         The_Point.Y := The_Point.Y + EPSILON;
      end if;
      if The_Point.Y < Below.Y or The_Point.Y > Above.Y then
         return False;
      elsif The_Point.X > Above.X and The_Point.X > Below.X then
         return False;
      elsif The_Point.X < Above.X and The_Point.X < Below.X then
         return True;
      else
         if Above.X /= Below.X then
            M_Red := (Above.Y - Below.Y) / (Above.X - Below.X);
         else
            Red_Is_Infinity := True;
         end if;
         if Below.X /= The_Point.X then
            M_Blue := (The_Point.Y - Below.Y) / (The_Point.X - Below.X);
         else
            Blue_Is_Infinity := True;
         end if;
         if Blue_Is_Infinity then
            return True;
         elsif Red_Is_Infinity then
            return False;
         elsif M_Blue >= M_Red then
            return True;
         else
            return False;
         end if;
      end if;
   end Ray_Intersects_Segment;

   function Create_Polygon (List : Point_List) return Polygon is
      Result : Polygon (List'Range);
      Side   : Segment;
   begin
      for I in List'Range loop
         Side (1) := List (I);
         if I = List'Last then
            Side (2) := List (List'First);
         else
            Side (2) := List (I + 1);
         end if;
         Result (I) := Side;
      end loop;
      return Result;
   end Create_Polygon;

   function Is_Inside (Who : Point; Where : Polygon) return Boolean is
      Count : Natural := 0;
   begin
      for Side in Where'Range loop
         if Ray_Intersects_Segment (Who, Where (Side)) then
            Count := Count + 1;
         end if;
      end loop;
      if Count mod 2 = 0 then
         return False;
      else
         return True;
      end if;
   end Is_Inside;

end Polygons;
```


Example use:

main.adb:

```Ada
with Ada.Text_IO;
with Polygons;
procedure Main is
   package Float_IO is new Ada.Text_IO.Float_IO (Float);
   Test_Points : Polygons.Point_List :=
     ((  5.0,  5.0),
      (  5.0,  8.0),
      (-10.0,  5.0),
      (  0.0,  5.0),
      ( 10.0,  5.0),
      (  8.0,  5.0),
      ( 10.0, 10.0));
   Square      : Polygons.Polygon    :=
     ((( 0.0,  0.0), (10.0,  0.0)),
      ((10.0,  0.0), (10.0, 10.0)),
      ((10.0, 10.0), ( 0.0, 10.0)),
      (( 0.0, 10.0), ( 0.0,  0.0)));
   Square_Hole : Polygons.Polygon    :=
     ((( 0.0,  0.0), (10.0,  0.0)),
      ((10.0,  0.0), (10.0, 10.0)),
      ((10.0, 10.0), ( 0.0, 10.0)),
      (( 0.0, 10.0), ( 0.0,  0.0)),
      (( 2.5,  2.5), ( 7.5,  2.5)),
      (( 7.5,  2.5), ( 7.5,  7.5)),
      (( 7.5,  7.5), ( 2.5,  7.5)),
      (( 2.5,  7.5), ( 2.5,  2.5)));
   Strange     : Polygons.Polygon    :=
     ((( 0.0,  0.0), ( 2.5,  2.5)),
      (( 2.5,  2.5), ( 0.0, 10.0)),
      (( 0.0, 10.0), ( 2.5,  7.5)),
      (( 2.5,  7.5), ( 7.5,  7.5)),
      (( 7.5,  7.5), (10.0, 10.0)),
      ((10.0, 10.0), (10.0,  0.0)),
      ((10.0,  0.0), ( 2.5,  2.5)));
   Exagon      : Polygons.Polygon    :=
     ((( 3.0,  0.0), ( 7.0,  0.0)),
      (( 7.0,  0.0), (10.0,  5.0)),
      ((10.0,  5.0), ( 7.0, 10.0)),
      (( 7.0, 10.0), ( 3.0, 10.0)),
      (( 3.0, 10.0), ( 0.0,  5.0)),
      (( 0.0,  5.0), ( 3.0,  0.0)));
begin
   Ada.Text_IO.Put_Line ("Testing Square:");
   for Point in Test_Points'Range loop
      Ada.Text_IO.Put ("Point(");
      Float_IO.Put (Test_Points (Point).X, 0, 0, 0);
      Ada.Text_IO.Put (",");
      Float_IO.Put (Test_Points (Point).Y, 0, 0, 0);
      Ada.Text_IO.Put
        ("): " &
         Boolean'Image (Polygons.Is_Inside (Test_Points (Point), Square)));
      Ada.Text_IO.New_Line;
   end loop;
   Ada.Text_IO.New_Line;
   Ada.Text_IO.Put_Line ("Testing Square_Hole:");
   for Point in Test_Points'Range loop
      Ada.Text_IO.Put ("Point(");
      Float_IO.Put (Test_Points (Point).X, 0, 0, 0);
      Ada.Text_IO.Put (",");
      Float_IO.Put (Test_Points (Point).Y, 0, 0, 0);
      Ada.Text_IO.Put
        ("): " &
         Boolean'Image
            (Polygons.Is_Inside (Test_Points (Point), Square_Hole)));
      Ada.Text_IO.New_Line;
   end loop;
   Ada.Text_IO.New_Line;
   Ada.Text_IO.Put_Line ("Testing Strange:");
   for Point in Test_Points'Range loop
      Ada.Text_IO.Put ("Point(");
      Float_IO.Put (Test_Points (Point).X, 0, 0, 0);
      Ada.Text_IO.Put (",");
      Float_IO.Put (Test_Points (Point).Y, 0, 0, 0);
      Ada.Text_IO.Put
        ("): " &
         Boolean'Image (Polygons.Is_Inside (Test_Points (Point), Strange)));
      Ada.Text_IO.New_Line;
   end loop;
   Ada.Text_IO.New_Line;
   Ada.Text_IO.Put_Line ("Testing Exagon:");
   for Point in Test_Points'Range loop
      Ada.Text_IO.Put ("Point(");
      Float_IO.Put (Test_Points (Point).X, 0, 0, 0);
      Ada.Text_IO.Put (",");
      Float_IO.Put (Test_Points (Point).Y, 0, 0, 0);
      Ada.Text_IO.Put
        ("): " &
         Boolean'Image (Polygons.Is_Inside (Test_Points (Point), Exagon)));
      Ada.Text_IO.New_Line;
   end loop;
end Main;
```


Output:

```txt
Testing Square:
Point(5.0,5.0): TRUE
Point(5.0,8.0): TRUE
Point(-10.0,5.0): FALSE
Point(0.0,5.0): FALSE
Point(10.0,5.0): TRUE
Point(8.0,5.0): TRUE
Point(10.0,10.0): FALSE

Testing Square_Hole:
Point(5.0,5.0): FALSE
Point(5.0,8.0): TRUE
Point(-10.0,5.0): FALSE
Point(0.0,5.0): FALSE
Point(10.0,5.0): TRUE
Point(8.0,5.0): TRUE
Point(10.0,10.0): FALSE

Testing Strange:
Point(5.0,5.0): TRUE
Point(5.0,8.0): FALSE
Point(-10.0,5.0): FALSE
Point(0.0,5.0): FALSE
Point(10.0,5.0): TRUE
Point(8.0,5.0): TRUE
Point(10.0,10.0): FALSE

Testing Exagon:
Point(5.0,5.0): TRUE
Point(5.0,8.0): TRUE
Point(-10.0,5.0): FALSE
Point(0.0,5.0): FALSE
Point(10.0,5.0): TRUE
Point(8.0,5.0): TRUE
Point(10.0,10.0): FALSE
```


## ANSI Standard BASIC

{{trans|FreeBASIC}}

```ANSI Standard BASIC
1000 PUBLIC NUMERIC x,y
1010 LET x=1
1020 LET y=2
1030 !
1040 DEF isLeft2(L(,),p()) = -SGN(  (L(1,x)-L(2,x))*(p(y)-L(2,y)) - (p(x)-L(2,x))*(L(1,y)-L(2,y)))
1050 !
1060 FUNCTION inpolygon(p1(,),p2())
1070    LET k=UBOUND(p1,1)+1
1080    DIM send (1 TO 2,2)
1090    LET wn=0
1100    FOR n=1 TO UBOUND(p1,1)
1110       LET index=MOD(n, k)
1120       LET nextindex=MOD(n+1, k)
1130       IF nextindex=0 THEN LET nextindex=1
1140       LET send(1,x)=p1(index,x)
1150       LET send(2,x)=p1(nextindex,x)
1160       LET send(1,y)=p1(index,y)
1170       LET send(2,y)=p1(nextindex,y)
1180       IF p1(index,y)<=p2(y) THEN
1190          IF p1(nextindex,y)>p2(y) THEN
1200             IF isleft2(send,p2)>=0 THEN !'=
1210                LET wn=wn+1
1220             END IF
1230          END IF
1240       ELSE
1250          IF p1(nextindex,y)<=p2(y) THEN
1260             IF isleft2(send,p2)<=0 THEN !'=
1270                LET wn=wn-1
1280             END IF
1290          END IF
1300       END IF
1310    NEXT n
1320    LET inpolygon = wn
1330 END FUNCTION
1340 !
1350 DIM type(1 TO 2)
1360 !
1370 DIM square(4,2)
1380 MAT READ square
1390 DATA 0,0,10,0,10,10,0,10
1400 !
1410 DIM hole(4,2)
1420 MAT READ hole
1430 DATA 2.5,2.5,7.5,2.5,7.5,7.5,2.5,7.5
1440 !
1450 DIM strange(8,2)
1460 MAT READ strange
1470 DATA 0,0,2.5,2.5,0,10,2.5,7.5,7.5,7.5,10,10,10,0,2.5,2.5
1480 !
1490 DIM exagon(6,2)  
1500 MAT READ exagon
1510 DATA 3,0,7,0,10,5,7,10,3,10,0,5
1520 !
1530 ! printouts
1540 FOR z=1 TO 4
1550    SELECT CASE z
1560    CASE 1
1570       PRINT "squared"
1580       PRINT "(5,5)  ";TAB(12);
1590       MAT READ type
1600       DATA 5,5
1610       IF inpolygon(square,type) <> 0 THEN PRINT "in" ELSE PRINT "out"
1620       MAT READ type
1630       DATA 5,8
1640       PRINT "(5,8)  ";TAB(12);
1650       IF inpolygon(square,type) <> 0 THEN PRINT "in" ELSE PRINT "out"
1660       PRINT "(-10,5)  ";TAB(12);
1670       MAT READ type
1680       DATA -10,5
1690       IF inpolygon(square,type) <> 0 THEN PRINT "in" ELSE PRINT "out"
1700       Print "(0,5)  ";Tab(12);
1710       MAT READ type
1720       DATA 0,5
1730       IF inpolygon(square,type) <> 0 THEN PRINT "in" ELSE PRINT "out"
1740       Print "(10,5)  ";Tab(12);
1750       MAT READ type
1760       DATA 10,5
1770       IF inpolygon(square,type) <> 0 THEN PRINT "in" ELSE PRINT "out"
1780       PRINT "(8,5)  ";TAB(12);
1790       MAT READ type
1800       DATA 8,5
1810       IF inpolygon(square,Type) <> 0 THEN PRINT "in" ELSE PRINT "out"
1820       PRINT "(10,10)  ";TAB(12);
1830       MAT READ type
1840       DATA 10,10
1850       IF inpolygon(square,Type) <> 0 THEN PRINT "in" ELSE PRINT "out"
1860       PRINT
1870    CASE 2
1880       PRINT "squared hole"
1890       PRINT "(5,5)  ";TAB(12);
1900       MAT READ type
1910       DATA 5,5
1920       IF NOT inpolygon(hole,Type)<>0 AND inpolygon(square,Type)<>0 THEN PRINT "in" ELSE PRINT "out"
1930       Print "(5,8)  ";Tab(12);
1940       MAT READ type
1950       DATA 5,8
1960       IF NOT inpolygon(hole,Type)<>0 AND inpolygon(square,Type)<>0 THEN PRINT "in" ELSE PRINT "out"
1970       PRINT "(-10,5)  ";TAB(12);
1980       MAT READ type
1990       DATA -10,5
2000       IF NOT inpolygon(hole,Type)<>0 AND inpolygon(square,Type)<>0 THEN PRINT "in" ELSE PRINT "out"
2010       PRINT "(0,5)  ";TAB(12);
2020       MAT READ type
2030       DATA 0,5
2040       IF NOT inpolygon(hole,Type)<>0 AND inpolygon(square,Type)<>0 THEN PRINT "in" ELSE PRINT "out"
2050       PRINT "(10,5)  ";TAB(12);
2060       MAT READ type
2070       DATA 10,5
2080       IF NOT inpolygon(hole,Type)<>0 AND inpolygon(square,Type)<>0 THEN PRINT "in" ELSE PRINT "out"
2090       PRINT "(8,5)  ";TAB(12);
2100       MAT READ type
2110       DATA 8,5
2120       IF NOT inpolygon(hole,Type)<>0 AND inpolygon(square,Type)<>0 THEN PRINT "in" ELSE PRINT "out"
2130       PRINT "(10,10)  ";TAB(12);
2140       MAT READ type
2150       DATA 10,10
2160       IF NOT inpolygon(hole,Type)<>0 AND inpolygon(square,Type)<>0 THEN PRINT "in" ELSE PRINT "out"
2170       PRINT
2180    CASE 3
2190       PRINT "strange"
2200       PRINT "(5,5)  ";TAB(12);
2210       MAT READ type
2220       DATA 5,5
2230       IF inpolygon(strange,Type)<>0 THEN PRINT "in" ELSE PRINT "out"
2240       PRINT "(5,8)  ";TAB(12);
2250       MAT READ type
2260       DATA 5,8
2270       IF inpolygon(strange,Type)<>0 THEN PRINT "in" ELSE PRINT "out"
2280       PRINT "(-10,5)  ";TAB(12);
2290       MAT READ type
2300       DATA -10,5
2310       IF inpolygon(strange,Type)<>0 THEN PRINT "in" ELSE PRINT "out"
2320       PRINT "(0,5)  ";TAB(12);
2330       MAT READ type
2340       DATA 0,5
2350       IF inpolygon(strange,Type)<>0 THEN PRINT "in" ELSE PRINT "out"
2360       PRINT "(10,5)  ";TAB(12);
2370       MAT READ type
2380       DATA 10,5
2390       IF inpolygon(strange,Type)<>0 THEN PRINT "in" ELSE PRINT "out"
2400       PRINT "(8,5)  ";TAB(12);
2410       MAT READ type
2420       DATA 8,5
2430       IF inpolygon(strange,Type)<>0 THEN PRINT "in" ELSE PRINT "out"
2440       PRINT "(10,10)  ";TAB(12);
2450       MAT READ type
2460       DATA 10,10
2470       IF inpolygon(strange,Type)<>0 THEN PRINT "in" ELSE PRINT "out"
2480       PRINT
2490    CASE 4
2500       PRINT "exagon"
2510       PRINT "(5,5)  ";TAB(12);
2520       MAT READ type
2530       DATA 5,5
2540       IF inpolygon(exagon,Type)<>0 THEN PRINT "in" ELSE PRINT "out"
2550       PRINT "(5,8)  ";TAB(12);
2560       MAT READ type
2570       DATA 5,8
2580       IF inpolygon(exagon,Type)<>0 THEN PRINT "in" ELSE PRINT "out"
2590       PRINT "(-10,5)  ";TAB(12);
2600       MAT READ type
2610       DATA -10,5
2620       IF inpolygon(exagon,Type)<>0 THEN PRINT "in" ELSE PRINT "out"
2630       PRINT "(0,5)  ";TAB(12);
2640       MAT READ type
2650       DATA 0,5
2660       IF inpolygon(exagon,Type)<>0 THEN PRINT "in" ELSE PRINT "out"
2670       PRINT "(10,5)  ";TAB(12);
2680       MAT READ type
2690       DATA 10,5
2700       IF inpolygon(exagon,Type)<>0 THEN PRINT "in" ELSE PRINT "out"
2710       PRINT "(8,5)  ";TAB(12);
2720       MAT READ type
2730       DATA 8,5
2740       IF inpolygon(exagon,Type)<>0 THEN PRINT "in" ELSE PRINT "out"
2750       PRINT "(10,10)  ";TAB(12);
2760       MAT READ type
2770       DATA 10,10
2780       IF inpolygon(exagon,Type)<>0 THEN PRINT "in" ELSE PRINT "out"
2790       PRINT
2800    END SELECT
2810 NEXT z
2820 END

```



## AutoHotkey

{{works with|AutoHotkey L}}

```ahk
Points :=[{x:  5.0, y: 5.0}
		, {x:  5.0, y: 8.0}
		, {x:-10.0, y: 5.0}
		, {x:  0.0, y: 5.0}
		, {x: 10.0, y: 5.0}
		, {x:  8.0, y: 5.0}
		, {x: 10.0, y:10.0}]
Square :=[{x: 0.0, y: 0.0}, {x:10.0, y: 0.0}
		, {x:10.0, y: 0.0}, {x:10.0, y:10.0}
		, {x:10.0, y:10.0}, {x: 0.0, y:10.0}
		, {x: 0.0, y:10.0}, {x: 0.0, y: 0.0}]
Sq_Hole:=[{x: 0.0, y: 0.0}, {x:10.0, y: 0.0}
		, {x:10.0, y: 0.0}, {x:10.0, y:10.0}
		, {x:10.0, y:10.0}, {x: 0.0, y:10.0}
		, {x: 0.0, y:10.0}, {x: 0.0, y: 0.0}
		, {x: 2.5, y: 2.5}, {x: 7.5, y: 2.5}
		, {x: 7.5, y: 2.5}, {x: 7.5, y: 7.5}
		, {x: 7.5, y: 7.5}, {x: 2.5, y: 7.5}
		, {x: 2.5, y: 7.5}, {x: 2.5, y: 2.5}]
Strange:=[{x: 0.0, y: 0.0}, {x: 2.5, y: 2.5}
		, {x: 2.5, y: 2.5}, {x: 0.0, y:10.0}
		, {x: 0.0, y:10.0}, {x: 2.5, y: 7.5}
		, {x: 2.5, y: 7.5}, {x: 7.5, y: 7.5}
		, {x: 7.5, y: 7.5}, {x:10.0, y:10.0}
		, {x:10.0, y:10.0}, {x:10.0, y: 0.0}
		, {x:10.0, y: 0.0}, {x: 2.5, y: 2.5}]
Exagon :=[{x: 3.0, y: 0.0}, {x: 7.0, y: 0.0}
		, {x: 7.0, y: 0.0}, {x:10.0, y: 5.0}
		, {x:10.0, y: 5.0}, {x: 7.0, y:10.0}
		, {x: 7.0, y:10.0}, {x: 3.0, y:10.0}
		, {x: 3.0, y:10.0}, {x: 0.0, y: 5.0}
		, {x: 0.0, y: 5.0}, {x: 3.0, y: 0.0}]
Polygons := {"Square":Square, "Sq_Hole":Sq_Hole, "Strange":Strange, "Exagon":Exagon}
For j, Poly in Polygons
	For i, Point in Points
		If (point_in_polygon(Point,Poly))
			s.= j " does contain point " i "`n"
		Else
			s.= j " doesn't contain point " i "`n"
Msgbox %s%

point_in_polygon(Point,Poly) {
	n:=Poly.MaxIndex()
	count:=0
	loop, %n% {
		if (ray_intersects_segment(Point,Poly[A_Index],Poly[mod(A_Index,n)+1])) {
			count++
		}
	}
	if (mod(count,2)) { ; true = inside, false = outside
		return true		; P is in the polygon
	} else {
		return false	; P isn't in the polygon
	}
}

ray_intersects_segment(P,A,B) {
	;P = the point from which the ray starts
	;A = the end-point of the segment with the smallest y coordinate
	;B = the end-point of the segment with the greatest y coordinate
	if (A.y > B.y) {
		temp:=A
		A:=B
		B:=temp
	}
	if (P.y = A.y or P.y = B.y) {
		P.y += 0.000001
	}
	if (P.y < A.y or P.y > B.y) {
		return false
	} else if (P.x > A.x && P.x > B.x) {
		return false
	} else {
		if (P.x < A.x && P.x < B.x) {
			return true
		} else {
			if (A.x != B.x) {
				m_red := (B.y - A.y)/(B.x - A.x)
			} else {
				m_red := "inf"
			}
			if (A.x != P.x) {
				m_blue := (P.y - A.y)/(P.x - A.x)
			} else {
				m_blue := "inf"
			}
			if (m_blue >= m_red) {
				return true
			} else {
				return false
			}
		}
	}
}
```

{{out}}

```txt
---------------------------
Ray-casting_algorithm.ahkl
---------------------------
Exagon does contain point 1
Exagon does contain point 2
Exagon doesn't contain point 3
Exagon doesn't contain point 4
Exagon does contain point 5
Exagon does contain point 6
Exagon doesn't contain point 7
Sq_Hole doesn't contain point 1
Sq_Hole does contain point 2
Sq_Hole doesn't contain point 3
Sq_Hole doesn't contain point 4
Sq_Hole does contain point 5
Sq_Hole does contain point 6
Sq_Hole doesn't contain point 7
Square does contain point 1
Square does contain point 2
Square doesn't contain point 3
Square doesn't contain point 4
Square does contain point 5
Square does contain point 6
Square doesn't contain point 7
Strange does contain point 1
Strange doesn't contain point 2
Strange doesn't contain point 3
Strange doesn't contain point 4
Strange does contain point 5
Strange does contain point 6
Strange doesn't contain point 7

---------------------------
OK   
---------------------------
```



## C


```c>#include <stdio.h

#include <stdlib.h>
#include <math.h>

typedef struct { double x, y; } vec;
typedef struct { int n; vec* v; } polygon_t, *polygon;

#define BIN_V(op, xx, yy) vec v##op(vec a,vec b){vec c;c.x=xx;c.y=yy;return c;}
#define BIN_S(op, r) double v##op(vec a, vec b){ return r; }
BIN_V(sub, a.x - b.x, a.y - b.y);
BIN_V(add, a.x + b.x, a.y + b.y);
BIN_S(dot, a.x * b.x + a.y * b.y);
BIN_S(cross, a.x * b.y - a.y * b.x);

/* return a + s * b */
vec vmadd(vec a, double s, vec b)
{
	vec c;
	c.x = a.x + s * b.x;
	c.y = a.y + s * b.y;
	return c;
}

/* check if x0->x1 edge crosses y0->y1 edge. dx = x1 - x0, dy = y1 - y0, then
   solve  x0 + a * dx == y0 + b * dy with a, b in real
   cross both sides with dx, then: (remember, cross product is a scalar)
	x0 X dx = y0 X dx + b * (dy X dx)
   similarly,
	x0 X dy + a * (dx X dy) == y0 X dy
   there is an intersection iff 0 <= a <= 1 and 0 <= b <= 1

   returns: 1 for intersect, -1 for not, 0 for hard to say (if the intersect
   point is too close to y0 or y1)
*/
int intersect(vec x0, vec x1, vec y0, vec y1, double tol, vec *sect)
{
	vec dx = vsub(x1, x0), dy = vsub(y1, y0);
	double d = vcross(dy, dx), a;
	if (!d) return 0; /* edges are parallel */

	a = (vcross(x0, dx) - vcross(y0, dx)) / d;
	if (sect)
		*sect = vmadd(y0, a, dy);

	if (a < -tol || a > 1 + tol) return -1;
	if (a < tol || a > 1 - tol) return 0;

	a = (vcross(x0, dy) - vcross(y0, dy)) / d;
	if (a < 0 || a > 1) return -1;

	return 1;
}

/* distance between x and nearest point on y0->y1 segment.  if the point
   lies outside the segment, returns infinity */
double dist(vec x, vec y0, vec y1, double tol)
{
	vec dy = vsub(y1, y0);
	vec x1, s;
	int r;

	x1.x = x.x + dy.y; x1.y = x.y - dy.x;
	r = intersect(x, x1, y0, y1, tol, &s);
	if (r == -1) return HUGE_VAL;
	s = vsub(s, x);
	return sqrt(vdot(s, s));
}

#define for_v(i, z, p) for(i = 0, z = p->v; i < p->n; i++, z++)
/* returns 1 for inside, -1 for outside, 0 for on edge */
int inside(vec v, polygon p, double tol)
{
	/* should assert p->n > 1 */
	int i, k, crosses, intersectResult;
	vec *pv;
	double min_x, max_x, min_y, max_y;

	for (i = 0; i < p->n; i++) {
		k = (i + 1) % p->n;
		min_x = dist(v, p->v[i], p->v[k], tol);
		if (min_x < tol) return 0;
	}

	min_x = max_x = p->v[0].x;
	min_y = max_y = p->v[1].y;

	/* calculate extent of polygon */
	for_v(i, pv, p) {
		if (pv->x > max_x) max_x = pv->x;
		if (pv->x < min_x) min_x = pv->x;
		if (pv->y > max_y) max_y = pv->y;
		if (pv->y < min_y) min_y = pv->y;
	}
	if (v.x < min_x || v.x > max_x || v.y < min_y || v.y > max_y)
		return -1;

	max_x -= min_x; max_x *= 2;
	max_y -= min_y; max_y *= 2;
	max_x += max_y;

	vec e;
	while (1) {
		crosses = 0;
		/* pick a rand point far enough to be outside polygon */
		e.x = v.x + (1 + rand() / (RAND_MAX + 1.)) * max_x;
		e.y = v.y + (1 + rand() / (RAND_MAX + 1.)) * max_x;

		for (i = 0; i < p->n; i++) {
			k = (i + 1) % p->n;
			intersectResult = intersect(v, e, p->v[i], p->v[k], tol, 0);

			/* picked a bad point, ray got too close to vertex.
			   re-pick */
			if (!intersectResult) break;

			if (intersectResult == 1) crosses++;
		}
		if (i == p->n) break;
	}
	return (crosses & 1) ? 1 : -1;
}

int main()
{
	vec vsq[] = {	{0,0}, {10,0}, {10,10}, {0,10},
			{2.5,2.5}, {7.5,0.1}, {7.5,7.5}, {2.5,7.5}};

	polygon_t sq = { 4, vsq }, /* outer square */
		sq_hole = { 8, vsq }; /* outer and inner square, ie hole */

	vec c = { 10, 5 }; /* on edge */
	vec d = { 5, 5 };

	printf("%d\n", inside(c, &sq, 1e-10));
	printf("%d\n", inside(c, &sq_hole, 1e-10));

	printf("%d\n", inside(d, &sq, 1e-10));	/* in */
	printf("%d\n", inside(d, &sq_hole, 1e-10));  /* out (in the hole) */

	return 0;
}
```



## C++

{{works with|C++|11}}
{{trans|D}}

```cpp>#include <algorithm

#include <cstdlib>
#include <iomanip>
#include <iostream>
#include <limits>

using namespace std;

const double epsilon = numeric_limits<float>().epsilon();
const numeric_limits<double> DOUBLE;
const double MIN = DOUBLE.min();
const double MAX = DOUBLE.max();

struct Point { const double x, y; };

struct Edge {
    const Point a, b;

    bool operator()(const Point& p) const
    {
        if (a.y > b.y) return Edge{ b, a }(p);
        if (p.y == a.y || p.y == b.y) return operator()({ p.x, p.y + epsilon });
        if (p.y > b.y || p.y < a.y || p.x > max(a.x, b.x)) return false;
        if (p.x < min(a.x, b.x)) return true;
        auto blue = abs(a.x - p.x) > MIN ? (p.y - a.y) / (p.x - a.x) : MAX;
        auto red = abs(a.x - b.x) > MIN ? (b.y - a.y) / (b.x - a.x) : MAX;
        return blue >= red;
    }
};

struct Figure {
    const string  name;
    const initializer_list<Edge> edges;

    bool contains(const Point& p) const
    {
        auto c = 0;
        for (auto e : edges) if (e(p)) c++;
        return c % 2 != 0;
    }

    template<unsigned char W = 3>
    void check(const initializer_list<Point>& points, ostream& os) const
    {
        os << "Is point inside figure " << name <<  '?' << endl;
        for (auto p : points)
            os << "  (" << setw(W) << p.x << ',' << setw(W) << p.y << "): " << boolalpha << contains(p) << endl;
        os << endl;
    }
};

int main()
{
    const initializer_list<Point> points =  { { 5.0, 5.0}, {5.0, 8.0}, {-10.0, 5.0}, {0.0, 5.0}, {10.0, 5.0}, {8.0, 5.0}, {10.0, 10.0} };
    const Figure square = { "Square",
        {  {{0.0, 0.0}, {10.0, 0.0}}, {{10.0, 0.0}, {10.0, 10.0}}, {{10.0, 10.0}, {0.0, 10.0}}, {{0.0, 10.0}, {0.0, 0.0}} }
    };

    const Figure square_hole = { "Square hole",
        {  {{0.0, 0.0}, {10.0, 0.0}}, {{10.0, 0.0}, {10.0, 10.0}}, {{10.0, 10.0}, {0.0, 10.0}}, {{0.0, 10.0}, {0.0, 0.0}},
           {{2.5, 2.5}, {7.5, 2.5}}, {{7.5, 2.5}, {7.5, 7.5}}, {{7.5, 7.5}, {2.5, 7.5}}, {{2.5, 7.5}, {2.5, 2.5}}
        }
    };

    const Figure strange = { "Strange",
        {  {{0.0, 0.0}, {2.5, 2.5}}, {{2.5, 2.5}, {0.0, 10.0}}, {{0.0, 10.0}, {2.5, 7.5}}, {{2.5, 7.5}, {7.5, 7.5}},
           {{7.5, 7.5}, {10.0, 10.0}}, {{10.0, 10.0}, {10.0, 0.0}}, {{10.0, 0}, {2.5, 2.5}}
        }
    };

    const Figure exagon = { "Exagon",
        {  {{3.0, 0.0}, {7.0, 0.0}}, {{7.0, 0.0}, {10.0, 5.0}}, {{10.0, 5.0}, {7.0, 10.0}}, {{7.0, 10.0}, {3.0, 10.0}},
           {{3.0, 10.0}, {0.0, 5.0}}, {{0.0, 5.0}, {3.0, 0.0}}
        }
    };

    for(auto f : {square, square_hole, strange, exagon})
        f.check(points, cout);

    return EXIT_SUCCESS;
}
```

{{output}}
As D.


## CoffeeScript

Takes a polygon as a list of points joining segments, and creates segments between them.

```coffeescript
  Point = (@x,@y) ->

  pointInPoly = (point,poly) ->
    segments = for pointA, index in poly
                 pointB = poly[(index + 1) % poly.length]
                 [pointA,pointB]
    intesected = (segment for segment in segments when rayIntesectsSegment(point,segment))
    intesected.length % 2 != 0

  rayIntesectsSegment = (p,segment) ->
    [p1,p2] = segment
    [a,b] = if p1.y < p2.y
              [p1,p2]
            else
              [p2,p1]
    if p.y == b.y || p.y == a.y
      p.y += Number.MIN_VALUE

    if p.y > b.y || p.y < a.y
      false
    else if p.x > a.x && p.x > b.x
      false
    else if p.x < a.x && p.x < b.x
      true
    else
      mAB = (b.y - a.y) / (b.x - a.x)
      mAP = (p.y - a.y) / (p.x - a.x)
      mAP > mAB
```



## Common Lisp

Points are represented as cons cells whose car is an x value and whose cdr is a y value.  A line segment is a cons cell of two points.  A polygon is a list of line segments.

```lisp
(defun point-in-polygon (point polygon)
  (do ((in-p nil)) ((endp polygon) in-p)
    (when (ray-intersects-segment point (pop polygon))
      (setf in-p (not in-p)))))

(defun ray-intersects-segment (point segment &optional (epsilon .001))
  (destructuring-bind (px . py) point
    (destructuring-bind ((ax . ay) . (bx . by)) segment
      (when (< ay by)
        (rotatef ay by)
        (rotatef ax bx))
      (when (or (= py ay) (= py by))
        (incf py epsilon))
      (cond
       ;; point is above, below, or to the right of the rectangle
       ;; determined by segment; ray does not intesect the segment.
       ((or (> px (max ax bx)) (> py (max ay by)) (< py (min ay by)))
        nil)
       ;; point is to left of the rectangle; ray intersects segment
       ((< px (min ax bx))
        t)
       ;; point is within the rectangle...
       (t (let ((m-red (if (= ax bx) nil
                         (/ (- by ay) (- bx ax))))
                (m-blue (if (= px ax) nil
                          (/ (- py ay) (- px ax)))))
            (cond
             ((null m-blue) t)
             ((null m-red) nil)
             (t (>= m-blue m-red)))))))))
```


Testing code

```lisp
(defparameter *points*
  #((0 . 0) (10 . 0) (10 . 10) (0 . 10)  
    (2.5 . 2.5) (7.5 . 2.5) (7.5 . 7.5) (2.5 . 7.5) 
    (0 . 5) (10 . 5) (3 . 0) (7 . 0)
    (7 . 10) (3 . 10)))

(defun create-polygon (indices &optional (points *points*))
  (loop for (a b) on indices by 'cddr
        collecting (cons (aref points (1- a))
                         (aref points (1- b)))))

(defun square ()
  (create-polygon '(1 2 2 3 3 4 4 1)))

(defun square-hole ()
  (create-polygon '(1 2 2 3 3 4 4 1 5 6 6 7 7 8 8 5)))

(defun strange ()
  (create-polygon '(1 5 5 4 4 8 8 7 7 3 3 2 2 5)))

(defun exagon ()
  (create-polygon '(11 12 12 10 10 13 13 14 14 9 9 11)))

(defparameter *test-points*
  #((5 . 5) (5 . 8) (-10 . 5) (0 . 5)
    (10 . 5) (8 . 5) (10 . 10)))

(defun test-pip ()
  (dolist (shape '(square square-hole strange exagon))
    (print shape)
    (loop with polygon = (funcall shape)
          for test-point across *test-points*
          do (format t "~&~w ~:[outside~;inside ~]."
                     test-point
                     (point-in-polygon test-point polygon)))))
```



## D


```d
import std.stdio, std.math, std.algorithm;

immutable struct Point { double x, y; }
immutable struct Edge { Point a, b; }
immutable struct Figure {
    string name;
    Edge[] edges;
}

bool contains(in Figure poly, in Point p) pure nothrow @safe @nogc {
    static bool raySegI(in Point p, in Edge edge)
    pure nothrow @safe @nogc {
        enum double epsilon = 0.00001;
        with (edge) {
            if (a.y > b.y)
                //swap(a, b); // if edge is mutable
                return raySegI(p, Edge(b, a));
            if (p.y == a.y || p.y == b.y)
                //p.y += epsilon; // if p is mutable
                return raySegI(Point(p.x, p.y + epsilon), edge);
            if (p.y > b.y || p.y < a.y || p.x > max(a.x, b.x))
                return false;
            if (p.x < min(a.x, b.x))
                return true;
            immutable blue = (abs(a.x - p.x) > double.min_normal) ?
                             ((p.y - a.y) / (p.x - a.x)) :
                             double.max;
            immutable red = (abs(a.x - b.x) > double.min_normal) ?
                            ((b.y - a.y) / (b.x - a.x)) :
                            double.max;
            return blue >= red;
        }
    }

    return poly.edges.count!(e => raySegI(p, e)) % 2;
}

void main() {
    immutable Figure[] polys = [
  {"Square", [
    {{ 0.0,  0.0}, {10.0,  0.0}},  {{10.0,  0.0}, {10.0, 10.0}},
    {{10.0, 10.0}, { 0.0, 10.0}},  {{ 0.0, 10.0}, { 0.0,  0.0}}]},
  {"Square hole", [
    {{ 0.0,  0.0}, {10.0,  0.0}},  {{10.0,  0.0}, {10.0, 10.0}},
    {{10.0, 10.0}, { 0.0, 10.0}},  {{ 0.0, 10.0}, { 0.0,  0.0}},
    {{ 2.5,  2.5}, { 7.5,  2.5}},  {{ 7.5,  2.5}, { 7.5,  7.5}},
    {{ 7.5,  7.5}, { 2.5,  7.5}},  {{ 2.5,  7.5}, { 2.5,  2.5}}]},
  {"Strange", [
    {{ 0.0,  0.0}, { 2.5,  2.5}},  {{ 2.5,  2.5}, { 0.0, 10.0}},
    {{ 0.0, 10.0}, { 2.5,  7.5}},  {{ 2.5,  7.5}, { 7.5,  7.5}},
    {{ 7.5,  7.5}, {10.0, 10.0}},  {{10.0, 10.0}, {10.0,  0.0}},
    {{10.0,  0},   { 2.5,  2.5}}]},
  {"Exagon", [
    {{ 3.0,  0.0}, { 7.0,  0.0}},  {{ 7.0,  0.0}, {10.0,  5.0}},
    {{10.0,  5.0}, { 7.0, 10.0}},  {{ 7.0, 10.0}, { 3.0, 10.0}},
    {{ 3.0, 10.0}, { 0.0,  5.0}},  {{ 0.0,  5.0}, { 3.0,  0.0}}]}
];

    immutable Point[] testPoints = [{ 5, 5}, {5, 8}, {-10,  5}, {0, 5},
                                    {10, 5}, {8, 5}, { 10, 10}];

    foreach (immutable poly; polys) {
        writefln(`Is point inside figure "%s"?`, poly.name);
        foreach (immutable p; testPoints)
            writefln("  (%3s, %2s): %s", p.x, p.y, contains(poly, p));
        writeln;
    }
}
```

{{out}}

```txt
Is point inside figure "Square"?
  (  5,  5): true
  (  5,  8): true
  (-10,  5): false
  (  0,  5): false
  ( 10,  5): true
  (  8,  5): true
  ( 10, 10): false

Is point inside figure "Square hole"?
  (  5,  5): false
  (  5,  8): true
  (-10,  5): false
  (  0,  5): false
  ( 10,  5): true
  (  8,  5): true
  ( 10, 10): false

Is point inside figure "Strange"?
  (  5,  5): true
  (  5,  8): false
  (-10,  5): false
  (  0,  5): false
  ( 10,  5): true
  (  8,  5): true
  ( 10, 10): false

Is point inside figure "Exagon"?
  (  5,  5): true
  (  5,  8): true
  (-10,  5): false
  (  0,  5): false
  ( 10,  5): true
  (  8,  5): true
  ( 10, 10): false
```



## Factor

To test whether a ray intersects a line, we test that the starting point is between the endpoints in y value, and that it is to the left of the point on the segment with the same y value. Note that this implementation does not support polygons with horizontal edges.

```factor
USING: kernel prettyprint sequences arrays math math.vectors ;
IN: raycasting

: between ( a b x -- ? ) [ last ] tri@ [ < ] curry bi@ xor ;

: lincomb ( a b x -- w )
  3dup [ last ] tri@
  [ - ] curry bi@
  [ drop ] 2dip
  neg 2dup + [ / ] curry bi@
  [ [ v*n ] curry ] bi@ bi*  v+ ;
: leftof ( a b x -- ? ) dup [ lincomb ] dip [ first ] bi@ > ;

: ray ( a b x -- ? ) [ between ] [ leftof ] 3bi and ;

: raycast ( poly x -- ? )
  [ dup first suffix [ rest-slice ] [ but-last-slice ] bi ] dip
  [ ray ] curry 2map
  f [ xor ] reduce ;
```

Usage:

```factor
( scratchpad ) CONSTANT: square { { -2 -1 } { 1 -2 } { 2 1 } { -1 2 } }
( scratchpad ) square { 0 0 } raycast .
t
( scratchpad ) square { 5 5 } raycast .
f
( scratchpad ) square { 2 0 } raycast .
f
```



## Fortran

{{works with|Fortran|95 and later}}
The following code uses the <tt>Points_Module</tt> defined [[Closest pair problem#Fortran|here]].

This module ''defines'' "polygons".

```fortran
module Polygons
  use Points_Module
  implicit none

  type polygon
     type(point), dimension(:), allocatable :: points
     integer, dimension(:), allocatable :: vertices
  end type polygon

contains

  function create_polygon(pts, vt)
    type(polygon) :: create_polygon
    type(point), dimension(:), intent(in) :: pts
    integer, dimension(:), intent(in) :: vt

    integer :: np, nv

    np = size(pts,1)
    nv = size(vt,1)

    allocate(create_polygon%points(np), create_polygon%vertices(nv))
    create_polygon%points = pts
    create_polygon%vertices = vt

  end function create_polygon

  subroutine free_polygon(pol)
    type(polygon), intent(inout) :: pol

    deallocate(pol%points, pol%vertices)

  end subroutine free_polygon

end module Polygons
```


The ray casting algorithm module:

```fortran
module Ray_Casting_Algo
  use Polygons
  implicit none

  real, parameter, private :: eps = 0.00001
  private :: ray_intersects_seg

contains

  function ray_intersects_seg(p0, a0, b0) result(intersect)
    type(point), intent(in) :: p0, a0, b0
    logical :: intersect

    type(point) :: a, b, p
    real :: m_red, m_blue

    p = p0
    ! let variable "a" be the point with smallest y coordinate
    if ( a0%y > b0%y ) then
       b = a0
       a = b0
    else
       a = a0
       b = b0
    end if

    if ( (p%y == a%y) .or. (p%y == b%y) ) p%y = p%y + eps

    intersect = .false.

    if ( (p%y > b%y) .or. (p%y < a%y) ) return
    if ( p%x > max(a%x, b%x) ) return

    if ( p%x < min(a%x, b%x) ) then
       intersect = .true.
    else
       if ( abs(a%x - b%x) > tiny(a%x) ) then
          m_red = (b%y - a%y) / (b%x - a%x)
       else
          m_red = huge(m_red)
       end if
       if ( abs(a%x - p%x) > tiny(a%x) ) then
          m_blue = (p%y - a%y) / (p%x - a%x)
       else
          m_blue = huge(m_blue)
       end if
       if ( m_blue >= m_red ) then
          intersect = .true.
       else
          intersect = .false.
       end if
    end if

  end function ray_intersects_seg

  function point_is_inside(p, pol) result(inside)
    logical :: inside
    type(point), intent(in) :: p
    type(polygon), intent(in) :: pol
    
    integer :: i, cnt, pa, pb

    cnt = 0
    do i = lbound(pol%vertices,1), ubound(pol%vertices,1), 2
       pa = pol%vertices(i)
       pb = pol%vertices(i+1)
       if ( ray_intersects_seg(p, pol%points(pa), pol%points(pb)) ) cnt = cnt + 1
    end do
    
    inside = .true.
    if ( mod(cnt, 2) == 0 ) then
       inside = .false.
    end if

  end function point_is_inside

end module Ray_Casting_Algo
```


'''Testing'''

```fortran
program Pointpoly
  use Points_Module
  use Ray_Casting_Algo
  implicit none

  character(len=16), dimension(4) :: names
  type(polygon), dimension(4) :: polys
  type(point), dimension(14) :: pts
  type(point), dimension(7) :: p

  integer :: i, j

  pts = (/ point(0,0), point(10,0), point(10,10), point(0,10), &
           point(2.5,2.5), point(7.5,2.5), point(7.5,7.5), point(2.5,7.5), &
           point(0,5), point(10,5), &
           point(3,0), point(7,0), point(7,10), point(3,10) /)

  polys(1) = create_polygon(pts, (/ 1,2, 2,3, 3,4, 4,1 /) )
  polys(2) = create_polygon(pts, (/ 1,2, 2,3, 3,4, 4,1, 5,6, 6,7, 7,8, 8,5 /) )
  polys(3) = create_polygon(pts, (/ 1,5, 5,4, 4,8, 8,7, 7,3, 3,2, 2,5 /) )
  polys(4) = create_polygon(pts, (/ 11,12, 12,10, 10,13, 13,14, 14,9, 9,11 /) )

  names = (/ "square", "square hole", "strange", "hexagon" /)

  p = (/ point(5,5), point(5, 8), point(-10, 5), point(0,5), point(10,5), &
         point(8,5), point(10,10) /)

  do j = 1, size(p)
     do i = 1, size(polys)
        write(*, "('point (',F8.2,',',F8.2,') is inside ',A,'? ', L)") &
             p(j)%x, p(j)%y, names(i), point_is_inside(p(j), polys(i))
     end do
     print *, ""
  end do

  do i = 1, size(polys)
     call free_polygon(polys(i))
  end do

end program Pointpoly
```



## FreeBASIC

Inpolygon by Winding number method

```FreeBASIC
Type Point
  As Single x,y
End Type

Function inpolygon(p1() As Point,p2 As Point) As Integer
  #Macro isleft2(L,p)
    -Sgn(  (L(1).x-L(2).x)*(p.y-L(2).y) - (p.x-L(2).x)*(L(1).y-L(2).y))
  #EndMacro
  Dim As Integer index,nextindex
  Dim k As Integer=UBound(p1)+1
  Dim send (1 To 2) As Point
  Dim wn As Integer=0
  For n As Integer=1 To UBound(p1)
    index=n Mod k:nextindex=(n+1) Mod k
    If nextindex=0 Then nextindex=1
    send(1).x=p1(index).x:send(2).x=p1(nextindex).x
    send(1).y=p1(index).y:send(2).y=p1(nextindex).y
    If p1(index).y<=p2.y Then
      If p1(nextindex).y>p2.y Then
        If isleft2(send,p2)>=0 Then '=
          wn=wn+1
        End If
      End If
    Else
      If p1(nextindex).y<=p2.y Then
        If isleft2(send,p2)<=0 Then'=
          wn=wn-1
        End If
      End If
    End If
  Next n
  Return wn
End Function


Dim As Point square(1 To 4)  ={(0,0),(10,0),(10,10),(0,10)}

Dim As Point hole(1 To 4)    ={(2.5,2.5),(7.5,2.5),(7.5,7.5),(2.5,7.5)}

Dim As Point strange(1 To 8) ={(0,0),(2.5,2.5),(0,10),(2.5,7.5),_
                              (7.5,7.5),(10,10),(10,0),(2.5,2.5)}

Dim As Point exagon(1 To 6)  ={(3,0),(7,0),(10,5),(7,10),(3,10),(0,5)}

'printouts
For z As Integer=1 To 4
  Select Case z
    Case 1: Print "squared"
      Print "(5,5)  " ;Tab(12);
      If inpolygon(square(),Type<Point>(5,5)) Then Print "in" Else Print "out"
      Print "(5,8)  " ;Tab(12);
      If inpolygon(square(),Type<Point>(5,8)) Then Print "in" Else Print "out"
      Print "(-10,5)  " ;Tab(12);
      If inpolygon(square(),Type<Point>(-10,5)) Then Print "in" Else Print "out"
      Print "(0,5)  " ;Tab(12);
      If inpolygon(square(),Type<Point>(0,5)) Then Print "in" Else Print "out"
      Print "(10,5)  " ;Tab(12);
      If inpolygon(square(),Type<Point>(10,5)) Then Print "in" Else Print "out"
      Print "(8,5)  " ;Tab(12);
      If inpolygon(square(),Type<Point>(8,5)) Then Print "in" Else Print "out"
      Print "(10,10)  " ;Tab(12);
      If inpolygon(square(),Type<Point>(10,10)) Then Print "in" Else Print "out"
      Print
    Case 2:Print "squared hole"
      Print "(5,5)  " ;Tab(12);
      If Not inpolygon(hole(),Type<Point>(5,5)) And inpolygon(square(),Type<Point>(5,5)) Then Print "in" Else Print "out"
      Print "(5,8)  " ;Tab(12);
      If Not inpolygon(hole(),Type<Point>(5,8)) And inpolygon(square(),Type<Point>(5,8))Then Print "in" Else Print "out"
      Print "(-10,5)  " ;Tab(12);
      If Not inpolygon(hole(),Type<Point>(-10,5))And inpolygon(square(),Type<Point>(-10,5)) Then Print "in" Else Print "out"
      Print "(0,5)  " ;Tab(12);
      If Not inpolygon(hole(),Type<Point>(0,5))And inpolygon(square(),Type<Point>(0,5)) Then Print "in" Else Print "out"
      Print "(10,5)  " ;Tab(12);
      If Not inpolygon(hole(),Type<Point>(10,5))And inpolygon(square(),Type<Point>(10,5)) Then Print "in" Else Print "out"
      Print "(8,5)  " ;Tab(12);
      If Not inpolygon(hole(),Type<Point>(8,5))And inpolygon(square(),Type<Point>(8,5)) Then Print "in" Else Print "out"
      Print "(10,10)  " ;Tab(12);
      If Not inpolygon(hole(),Type<Point>(10,10))And inpolygon(square(),Type<Point>(10,10)) Then Print "in" Else Print "out"
      Print
    Case 3:Print "strange"
      Print "(5,5)  " ;Tab(12);
      If inpolygon(strange(),Type<Point>(5,5)) Then Print "in" Else Print "out"
      Print "(5,8)  " ;Tab(12);
      If inpolygon(strange(),Type<Point>(5,8)) Then Print "in" Else Print "out"
      Print "(-10,5)  " ;Tab(12);
      If inpolygon(strange(),Type<Point>(-10,5)) Then Print "in" Else Print "out"
      Print "(0,5)  " ;Tab(12);
      If inpolygon(strange(),Type<Point>(0,5)) Then Print "in" Else Print "out"
      Print "(10,5)  " ;Tab(12);
      If inpolygon(strange(),Type<Point>(10,5)) Then Print "in" Else Print "out"
      Print "(8,5)  " ;Tab(12);
      If inpolygon(strange(),Type<Point>(8,5)) Then Print "in" Else Print "out"
      Print "(10,10)  " ;Tab(12);
      If inpolygon(strange(),Type<Point>(10,10)) Then Print "in" Else Print "out"
      Print
    Case 4:Print "exagon"
      Print "(5,5)  " ;Tab(12);
      If inpolygon(exagon(),Type<Point>(5,5)) Then Print "in" Else Print "out"
      Print "(5,8)  " ;Tab(12);
      If inpolygon(exagon(),Type<Point>(5,8)) Then Print "in" Else Print "out"
      Print "(-10,5)  " ;Tab(12);
      If inpolygon(exagon(),Type<Point>(-10,5)) Then Print "in" Else Print "out"
      Print "(0,5)  " ;Tab(12);
      If inpolygon(exagon(),Type<Point>(0,5)) Then Print "in" Else Print "out"
      Print "(10,5)  " ;Tab(12);
      If inpolygon(exagon(),Type<Point>(10,5)) Then Print "in" Else Print "out"
      Print "(8,5)  " ;Tab(12);
      If inpolygon(exagon(),Type<Point>(8,5)) Then Print "in" Else Print "out"
      Print "(10,10)  " ;Tab(12);
      If inpolygon(exagon(),Type<Point>(10,10)) Then Print "in" Else Print "out"
      Print
  End Select
Next z
Sleep
```

Output:

```txt
squared
(5,5)      in
(5,8)      in
(-10,5)    out
(0,5)      out
(10,5)     in
(8,5)      in
(10,10)    out

squared hole
(5,5)      out
(5,8)      in
(-10,5)    out
(0,5)      out
(10,5)     in
(8,5)      in
(10,10)    out

strange
(5,5)      in
(5,8)      out
(-10,5)    out
(0,5)      out
(10,5)     in
(8,5)      in
(10,10)    out

exagon
(5,5)      in
(5,8)      in
(-10,5)    out
(0,5)      out
(10,5)     in
(8,5)      in
(10,10)    out
```



## Go

'''Segment solution, task algorithm'''

The first solution given here follows the model of most other solutions on the page in defining a polygon as a list of segments.  Unfortunately this representation does not require that the polygon is ''closed''.  Input to the ray-casting algorithm, as noted in the WP article though, is specified to be a closed polygon.  The "strange" shape defined here is not a closed polygon and so gives incorrect results against some points.  (Graphically it may appear closed but mathematically it needs an additional segment returning to the starting point.)

```go
package main

import (
    "fmt"
    "math"
)

type xy struct {
    x, y float64
}

type seg struct {
    p1, p2 xy
}

type poly struct {
    name  string
    sides []seg
}

func inside(pt xy, pg poly) (i bool) {
    for _, side := range pg.sides {
        if rayIntersectsSegment(pt, side) {
            i = !i
        }
    }
    return
}

func rayIntersectsSegment(p xy, s seg) bool {
    var a, b xy
    if s.p1.y < s.p2.y {
        a, b = s.p1, s.p2
    } else {
        a, b = s.p2, s.p1
    }
    for p.y == a.y || p.y == b.y {
        p.y = math.Nextafter(p.y, math.Inf(1))
    }
    if p.y < a.y || p.y > b.y {
        return false
    }
    if a.x > b.x {
        if p.x > a.x {
            return false
        }
        if p.x < b.x {
            return true
        }
    } else {
        if p.x > b.x {
            return false
        }
        if p.x < a.x {
            return true
        }
    }
    return (p.y-a.y)/(p.x-a.x) >= (b.y-a.y)/(b.x-a.x)
}

var (
    p1  = xy{0, 0}
    p2  = xy{10, 0}
    p3  = xy{10, 10}
    p4  = xy{0, 10}
    p5  = xy{2.5, 2.5}
    p6  = xy{7.5, 2.5}
    p7  = xy{7.5, 7.5}
    p8  = xy{2.5, 7.5}
    p9  = xy{0, 5}
    p10 = xy{10, 5}
    p11 = xy{3, 0}
    p12 = xy{7, 0}
    p13 = xy{7, 10}
    p14 = xy{3, 10}
)

var tpg = []poly{
    {"square", []seg{{p1, p2}, {p2, p3}, {p3, p4}, {p4, p1}}},
    {"square hole", []seg{{p1, p2}, {p2, p3}, {p3, p4}, {p4, p1},
        {p5, p6}, {p6, p7}, {p7, p8}, {p8, p5}}},
    {"strange", []seg{{p1, p5},
        {p5, p4}, {p4, p8}, {p8, p7}, {p7, p3}, {p3, p2}, {p2, p5}}},
    {"exagon", []seg{{p11, p12}, {p12, p10}, {p10, p13},
        {p13, p14}, {p14, p9}, {p9, p11}}},
}

var tpt = []xy{
    // test points common in other solutions on this page
    {5, 5}, {5, 8}, {-10, 5}, {0, 5}, {10, 5}, {8, 5}, {10, 10},
    // test points that show the problem with "strange"
    {1, 2}, {2, 1},
}

func main() {
    for _, pg := range tpg {
        fmt.Printf("%s:\n", pg.name)
        for _, pt := range tpt {
            fmt.Println(pt, inside(pt, pg))
        }
    }
}
```

{{out}}

```txt

square:
{5 5} true
{5 8} true
{-10 5} false
{0 5} false
{10 5} true
{8 5} true
{10 10} false
{1 2} true
{2 1} true
square hole:
{5 5} false
{5 8} true
{-10 5} false
{0 5} false
{10 5} true
{8 5} true
{10 10} false
{1 2} true
{2 1} true
strange:
{5 5} true
{5 8} false
{-10 5} false
{0 5} false
{10 5} true
{8 5} true
{10 10} false
{1 2} true
{2 1} false
exagon:
{5 5} true
{5 8} true
{-10 5} false
{0 5} false
{10 5} true
{8 5} true
{10 10} false
{1 2} false
{2 1} false

```

'''Closed polygon solution'''

Here input is given as a list of N vertices defining N segments, where one segment extends from each vertex to the next, and one more extends from the last vertex to the first.  In the case of the "strange" shape, this mathematically closes the polygon and allows the program to give correct results.

```go
package main

import (
    "math"
    "fmt"
)

type xy struct {
    x, y float64
}

type closedPoly struct {
    name string
    vert []xy
}

func inside(pt xy, pg closedPoly) bool {
    if len(pg.vert) < 3 {
        return false
    }
    in := rayIntersectsSegment(pt, pg.vert[len(pg.vert)-1], pg.vert[0])
    for i := 1; i < len(pg.vert); i++ {
        if rayIntersectsSegment(pt, pg.vert[i-1], pg.vert[i]) {
            in = !in
        }
    }
    return in
}

func rayIntersectsSegment(p, a, b xy) bool {
    if a.y > b.y {
        a, b = b, a
    }
    for p.y == a.y || p.y == b.y {
        p.y = math.Nextafter(p.y, math.Inf(1))
    }
    if p.y < a.y || p.y > b.y {
        return false
    }
    if a.x > b.x {
        if p.x > a.x {
            return false
        }
        if p.x < b.x {
            return true
        }
    } else {
        if p.x > b.x {
            return false
        }
        if p.x < a.x {
            return true
        }
    }
    return (p.y-a.y)/(p.x-a.x) >= (b.y-a.y)/(b.x-a.x)
}

var tpg = []closedPoly{
    {"square", []xy{{0, 0}, {10, 0}, {10, 10}, {0, 10}}},
    {"square hole", []xy{{0, 0}, {10, 0}, {10, 10}, {0, 10}, {0, 0},
        {2.5, 2.5}, {7.5, 2.5}, {7.5, 7.5}, {2.5, 7.5}, {2.5, 2.5}}},
    {"strange", []xy{{0, 0}, {2.5, 2.5}, {0, 10}, {2.5, 7.5}, {7.5, 7.5},
        {10, 10}, {10, 0}, {2.5, 2.5}}},
    {"exagon", []xy{{3, 0}, {7, 0}, {10, 5}, {7, 10}, {3, 10}, {0, 5}}},
}

var tpt = []xy{{1, 2}, {2, 1}}

func main() {
    for _, pg := range tpg {
        fmt.Printf("%s:\n", pg.name)
        for _, pt := range tpt {
            fmt.Println(pt, inside(pt, pg))
        }
    }
}
```

{{out}}

```txt

square:
{1 2} true
{2 1} true
square hole:
{1 2} true
{2 1} true
strange:
{1 2} false
{2 1} false
exagon:
{1 2} false
{2 1} false

```

'''PNPoly algorithm'''

This solution replaces the <code>rayIntersectsSegment</code> function above with the expression from the popular PNPoly algorithm described at https://www.ecse.rpi.edu/Homepages/wrf/Research/Short_Notes/pnpoly.html.  The expression is not only simpler but more accurate.

This solution is preferred over the two above.
<lang>package main

import "fmt"

type xy struct {
    x, y float64
}

type closedPoly struct {
    name string
    vert []xy
}

func inside(pt xy, pg closedPoly) bool {
    if len(pg.vert) < 3 {
        return false
    }
    in := rayIntersectsSegment(pt, pg.vert[len(pg.vert)-1], pg.vert[0])
    for i := 1; i < len(pg.vert); i++ {
        if rayIntersectsSegment(pt, pg.vert[i-1], pg.vert[i]) {
            in = !in
        }
    }
    return in
}

func rayIntersectsSegment(p, a, b xy) bool {
    return (a.y > p.y) != (b.y > p.y) &&
        p.x < (b.x-a.x)*(p.y-a.y)/(b.y-a.y)+a.x
}

var tpg = []closedPoly{
    {"square", []xy{{0, 0}, {10, 0}, {10, 10}, {0, 10}}},
    {"square hole", []xy{{0, 0}, {10, 0}, {10, 10}, {0, 10}, {0, 0},
        {2.5, 2.5}, {7.5, 2.5}, {7.5, 7.5}, {2.5, 7.5}, {2.5, 2.5}}},
    {"strange", []xy{{0, 0}, {2.5, 2.5}, {0, 10}, {2.5, 7.5}, {7.5, 7.5},
        {10, 10}, {10, 0}, {2.5, 2.5}}},
    {"exagon", []xy{{3, 0}, {7, 0}, {10, 5}, {7, 10}, {3, 10}, {0, 5}}},
}

var tpt = []xy{{1, 2}, {2, 1}}

func main() {
    for _, pg := range tpg {
        fmt.Printf("%s:\n", pg.name)
        for _, pt := range tpt {
            fmt.Println(pt, inside(pt, pg))
        }
    }
}
```



## Haskell


```haskell
import Data.Ratio

type Point = (Rational, Rational)
type Polygon = [Point]
data Line = Sloped {lineSlope, lineYIntercept :: Rational} |
            Vert {lineXIntercept :: Rational}

polygonSides :: Polygon -> [(Point, Point)]
polygonSides poly@(p1 : ps) = zip poly $ ps ++ [p1]

intersects :: Point -> Line -> Bool
{- @intersects (px, py) l@ is true if the ray {(x, py) | x ≥ px}
intersects l. -}
intersects (px, _)  (Vert xint)  = px <= xint
intersects (px, py) (Sloped m b) | m < 0     = py <= m * px + b
                                 | otherwise = py >= m * px + b

onLine :: Point -> Line -> Bool
{- Is the point on the line? -}
onLine (px, _)  (Vert xint)  = px == xint
onLine (px, py) (Sloped m b) = py == m * px + b

carrier :: (Point, Point) -> Line
{- Finds the line containing the given line segment. -}
carrier ((ax, ay), (bx, by)) | ax == bx  = Vert ax
                             | otherwise = Sloped slope yint
  where slope = (ay - by) / (ax - bx)
        yint = ay - slope * ax

between :: Ord a => a -> a -> a -> Bool
between x a b | a > b     = b <= x && x <= a
              | otherwise = a <= x && x <= b

inPolygon :: Point -> Polygon -> Bool
inPolygon p@(px, py) = f 0 . polygonSides
  where f n []                             = odd n
        f n (side : sides) | far           = f n       sides
                           | onSegment     = True
                           | rayIntersects = f (n + 1) sides
                           | otherwise     = f n       sides
          where far = not $ between py ay by
                onSegment | ay == by  = between px ax bx
                          | otherwise = p `onLine` line
                rayIntersects =
                    intersects p line &&
                    (py /= ay || by < py) &&
                    (py /= by || ay < py)
                ((ax, ay), (bx, by)) = side
                line = carrier side
```



## J


```j
NB.*crossPnP v point in closed polygon, crossing number
NB.  bool=. points crossPnP polygon
crossPnP=: 4 : 0"2
  'X Y'=. |:x
  'x0 y0 x1 y1'=. |:2 ,/\^:(2={:@$@]) y
  p1=. ((y0<:/Y)*. y1>/Y) +. (y0>/Y)*. y1<:/Y
  p2=. (x0-/X) < (x0-x1) * (y0-/Y) % (y0 - y1)
  2|+/ p1*.p2
)
```


Sample data:

```j
SQUAREV=:          0   0   , 10  0   , 10  10  ,: 0   10
SQUAREV=: SQUAREV, 2.5 2.5 , 7.5 0.1 , 7.5 7.5 ,: 2.5 7.5

ESAV=: 3 0 , 7 0 , 10 5 , 7 10 , 3 10 ,: 0 5

ESA=:        (0 1,1 2,2 3,3 4,4 5,:5 0) , .{ ESAV
SQUARE=:     (0 1,1 2,2 3,:3 0)         , .{ SQUAREV
SQUAREHOLE=: (0 1,1 2,2 3,3 0,4 5,5 6,6 7,:7 4) , .{ SQUAREV
STRANGE=:    (0 4,4 3,3 7,7 6,6 2,2 1,1 5,:5 0) , .{ SQUAREV

POINTS=: 5 5,5 8,2 2,0 0,10 10,2.5 2.5,0.01 5,2.2 7.4,0 5,10 5,:_4 10
```


Testing:

```j
   (<POINTS) crossPnP every ESA;SQUARE;SQUAREHOLE;STRANGE
1 1 1 0 0 1 1 1 0 1 0
1 1 1 0 0 1 1 1 0 1 0
0 1 1 0 0 1 1 1 0 1 0
1 0 0 0 0 0 0 1 0 1 0
```



## Java


```java
import static java.lang.Math.*;

public class RayCasting {

    static boolean intersects(int[] A, int[] B, double[] P) {
        if (A[1] > B[1])
            return intersects(B, A, P);

        if (P[1] == A[1] || P[1] == B[1])
            P[1] += 0.0001;

        if (P[1] > B[1] || P[1] < A[1] || P[0] >= max(A[0], B[0]))
            return false;

        if (P[0] < min(A[0], B[0]))
            return true;

        double red = (P[1] - A[1]) / (double) (P[0] - A[0]);
        double blue = (B[1] - A[1]) / (double) (B[0] - A[0]);
        return red >= blue;
    }

    static boolean contains(int[][] shape, double[] pnt) {
        boolean inside = false;
        int len = shape.length;
        for (int i = 0; i < len; i++) {
            if (intersects(shape[i], shape[(i + 1) % len], pnt))
                inside = !inside;
        }
        return inside;
    }

    public static void main(String[] a) {
        double[][] testPoints = {{10, 10}, {10, 16}, {-20, 10}, {0, 10},
        {20, 10}, {16, 10}, {20, 20}};

        for (int[][] shape : shapes) {
            for (double[] pnt : testPoints)
                System.out.printf("%7s ", contains(shape, pnt));
            System.out.println();
        }
    }

    final static int[][] square = {{0, 0}, {20, 0}, {20, 20}, {0, 20}};

    final static int[][] squareHole = {{0, 0}, {20, 0}, {20, 20}, {0, 20},
    {5, 5}, {15, 5}, {15, 15}, {5, 15}};

    final static int[][] strange = {{0, 0}, {5, 5}, {0, 20}, {5, 15}, {15, 15},
    {20, 20}, {20, 0}};

    final static int[][] hexagon = {{6, 0}, {14, 0}, {20, 10}, {14, 20},
    {6, 20}, {0, 10}};

    final static int[][][] shapes = {square, squareHole, strange, hexagon};
}
```



```txt
   true    true   false   false    true    true   false 
  false    true   false   false    true    true   false 
   true   false   false   false    true    true   false 
   true    true   false   false    true    true   false 
```



## Javascript


```javascript

/**
 * @return {boolean} true if (lng, lat) is in bounds
 */
function contains(bounds, lat, lng) {
    //https://rosettacode.org/wiki/Ray-casting_algorithm
    var count = 0;
    for (var b = 0; b < bounds.length; b++) {
        var vertex1 = bounds[b];
        var vertex2 = bounds[(b + 1) % bounds.length];
        if (west(vertex1, vertex2, lng, lat))
            ++count;
    }
    return count % 2;

    /**
     * @return {boolean} true if (x,y) is west of the line segment connecting A and B
     */
    function west(A, B, x, y) {
        if (A.y <= B.y) {
            if (y <= A.y || y > B.y ||
                x >= A.x && x >= B.x) {
                return false;
            } else if (x < A.x && x < B.x) {
                return true;
            } else {
                return (y - A.y) / (x - A.x) > (B.y - A.y) / (B.x - A.x);
            }
        } else {
            return west(B, A, x, y);
        }
    }
}

var square = {name: 'square', bounds: [{x: 0, y: 0}, {x: 20, y: 0}, {x: 20, y: 20}, {x: 0, y: 20}]};
var squareHole = {
    name: 'squareHole',
    bounds: [{x: 0, y: 0}, {x: 20, y: 0}, {x: 20, y: 20}, {x: 0, y: 20}, {x: 5, y: 5}, {x: 15, y: 5}, {x: 15, y: 15}, {x: 5, y: 15}]
};
var strange = {
    name: 'strange',
    bounds: [{x: 0, y: 0}, {x: 5, y: 5}, {x: 0, y: 20}, {x: 5, y: 15}, {x: 15, y: 15}, {x: 20, y: 20}, {x: 20, y: 0}]
};
var hexagon = {
    name: 'hexagon',
    bounds: [{x: 6, y: 0}, {x: 14, y: 0}, {x: 20, y: 10}, {x: 14, y: 20}, {x: 6, y: 20}, {x: 0, y: 10}]
};

var shapes = [square, squareHole, strange, hexagon];
var testPoints = [{lng: 10, lat: 10}, {lng: 10, lat: 16}, {lng: -20, lat: 10},
    {lng: 0, lat: 10}, {lng: 20, lat: 10}, {lng: 16, lat: 10}, {lng: 20, lat: 20}];

for (var s = 0; s < shapes.length; s++) {
    var shape = shapes[s];
    for (var tp = 0; tp < testPoints.length; tp++) {
        var testPoint = testPoints[tp];
        console.log(JSON.stringify(testPoint) + '\tin ' + shape.name + '\t' + contains(shape.bounds, testPoint.lat, testPoint.lng));
    }
}

```



## Julia

{{works with|Julia|0.6}}
{{trans|Python}}

'''Module''':

```julia
module RayCastings

export Point

struct Point{T}
    x::T
    y::T
end
Base.show(io::IO, p::Point) = print(io, "($(p.x), $(p.y))")

const Edge = Tuple{Point{T}, Point{T}} where T
Base.show(io::IO, e::Edge) = print(io, "$(e[1]) ∘-∘ $(e[2])")

function rayintersectseg(p::Point{T}, edge::Edge{T}) where T
    a, b = edge
    if a.y > b.y
        a, b = b, a
    end
    if p.y ∈ (a.y, b.y)
        p = Point(p.x, p.y + eps(p.y))
    end

    rst = false
    if (p.y > b.y || p.y < a.y) || (p.x > max(a.x, b.x))
        return false
    end

    if p.x < min(a.x, b.x)
        rst = true
    else
        mred = (b.y - a.y) / (b.x - a.x)
        mblu = (p.y - a.y) / (p.x - a.x)
        rst = mblu ≥ mred
    end

    return rst
end

isinside(poly::Vector{Tuple{Point{T}, Point{T}}}, p::Point{T}) where T =
    isodd(count(edge -> rayintersectseg(p, edge), poly))

connect(a::Point{T}, b::Point{T}...) where T =
    [(a, b) for (a, b) in zip(vcat(a, b...), vcat(b..., a))]

end  # module RayCastings
```


'''Main''':

```julia
let A = Point(0.0, 0.0),
    B = Point(0.0, 10.0),
    C = Point(10.0, 10.0),
    D = Point(10.0, 0.0),
    E = Point(2.5, 2.5),
    F = Point(2.5, 7.5),
    G = Point(7.5, 7.5),
    H = Point(7.5, 2.5),
    I = Point(3.0, 0.0),
    J = Point(7.0, 0.0),
    K = Point(10.0, 5.0),
    L = Point(7.0, 10.0),
    M = Point(3.0, 10.0),
    N = Point(0.0, 5.0),
    testpts = (Point(5.0, 5.0), Point(5.0, 8.0), Point(-10.0, 5.0), Point(0.0, 5.0),
        Point(10.0, 5.0), Point(8.0, 5.0), Point(10.0, 10.0))

    square = RayCastings.connect(A, B, C, D)
    square_withhole = vcat(square, RayCastings.connect(E, F, G, H))
    strange = RayCastings.connect(A, E, B, F, G, C, D, E)
    exagon = RayCastings.connect(I, J, K, L, M, N)

    println("\n# TESTING WHETHER POINTS ARE WITHIN POLYGONS")
    for poly in (square, square_withhole, strange, exagon)
        println("\nEdges: \n - ", join(poly, "\n - "))
        println("Inside/outside:")
        for p in testpts
            @printf(" - %-12s is %s\n", p, RayCastings.isinside(poly, p) ? "inside" : "outside")
        end
    end
end
```


{{out}}

```txt
# TESTING WHETHER POINTS ARE WITHIN POLYGONS

Edges:
 - (0.0, 0.0) ∘-∘ (0.0, 10.0)
 - (0.0, 10.0) ∘-∘ (10.0, 10.0)
 - (10.0, 10.0) ∘-∘ (10.0, 0.0)
 - (10.0, 0.0) ∘-∘ (0.0, 0.0)
Inside/outside:
 - (5.0, 5.0)   is inside
 - (5.0, 8.0)   is inside
 - (-10.0, 5.0) is outside
 - (0.0, 5.0)   is outside
 - (10.0, 5.0)  is inside
 - (8.0, 5.0)   is inside
 - (10.0, 10.0) is outside

Edges:
 - (0.0, 0.0) ∘-∘ (0.0, 10.0)
 - (0.0, 10.0) ∘-∘ (10.0, 10.0)
 - (10.0, 10.0) ∘-∘ (10.0, 0.0)
 - (10.0, 0.0) ∘-∘ (0.0, 0.0)
 - (2.5, 2.5) ∘-∘ (2.5, 7.5)
 - (2.5, 7.5) ∘-∘ (7.5, 7.5)
 - (7.5, 7.5) ∘-∘ (7.5, 2.5)
 - (7.5, 2.5) ∘-∘ (2.5, 2.5)
Inside/outside:
 - (5.0, 5.0)   is outside
 - (5.0, 8.0)   is inside
 - (-10.0, 5.0) is outside
 - (0.0, 5.0)   is outside
 - (10.0, 5.0)  is inside
 - (8.0, 5.0)   is inside
 - (10.0, 10.0) is outside

Edges:
 - (0.0, 0.0) ∘-∘ (2.5, 2.5)
 - (2.5, 2.5) ∘-∘ (0.0, 10.0)
 - (0.0, 10.0) ∘-∘ (2.5, 7.5)
 - (2.5, 7.5) ∘-∘ (7.5, 7.5)
 - (7.5, 7.5) ∘-∘ (10.0, 10.0)
 - (10.0, 10.0) ∘-∘ (10.0, 0.0)
 - (10.0, 0.0) ∘-∘ (2.5, 2.5)
 - (2.5, 2.5) ∘-∘ (0.0, 0.0)
Inside/outside:
 - (5.0, 5.0)   is inside
 - (5.0, 8.0)   is outside
 - (-10.0, 5.0) is outside
 - (0.0, 5.0)   is outside
 - (10.0, 5.0)  is inside
 - (8.0, 5.0)   is inside
 - (10.0, 10.0) is outside

Edges:
 - (3.0, 0.0) ∘-∘ (7.0, 0.0)
 - (7.0, 0.0) ∘-∘ (10.0, 5.0)
 - (10.0, 5.0) ∘-∘ (7.0, 10.0)
 - (7.0, 10.0) ∘-∘ (3.0, 10.0)
 - (3.0, 10.0) ∘-∘ (0.0, 5.0)
 - (0.0, 5.0) ∘-∘ (3.0, 0.0)
Inside/outside:
 - (5.0, 5.0)   is inside
 - (5.0, 8.0)   is inside
 - (-10.0, 5.0) is outside
 - (0.0, 5.0)   is outside
 - (10.0, 5.0)  is inside
 - (8.0, 5.0)   is inside
 - (10.0, 10.0) is outside
```



## Kotlin

{{trans|D}}

```scala
import java.lang.Double.MAX_VALUE
import java.lang.Double.MIN_VALUE
import java.lang.Math.abs

data class Point(val x: Double, val y: Double)

data class Edge(val s: Point, val e: Point) {
    operator fun invoke(p: Point) : Boolean = when {
        s.y > e.y -> Edge(e, s).invoke(p)
        p.y == s.y || p.y == e.y -> invoke(Point(p.x, p.y + epsilon))
        p.y > e.y || p.y < s.y || p.x > Math.max(s.x, e.x) -> false
        p.x < Math.min(s.x, e.x) -> true
        else -> {
            val blue = if (abs(s.x - p.x) > MIN_VALUE) (p.y - s.y) / (p.x - s.x) else MAX_VALUE
            val red = if (abs(s.x - e.x) > MIN_VALUE) (e.y - s.y) / (e.x - s.x) else MAX_VALUE
            blue >= red
        }
    }

    val epsilon = 0.00001
}

class Figure(val name: String, val edges: Array<Edge>) {
    operator fun contains(p: Point) = edges.count({ it(p) }) % 2 != 0
}

object Ray_casting {
    fun check(figures : Array<Figure>, points : List<Point>) {
        println("points: " + points)
        figures.forEach { f ->
            println("figure: " + f.name)
            f.edges.forEach { println("        " + it) }
            println("result: " + (points.map { it in f }))
        }
    }
}
```


```scala
fun main(args: Array<String>) {
    val figures = arrayOf(Figure("Square", arrayOf(Edge(Point(0.0, 0.0), Point(10.0, 0.0)), Edge(Point(10.0, 0.0), Point(10.0, 10.0)),
            Edge(Point(10.0, 10.0), Point(0.0, 10.0)),Edge(Point(0.0, 10.0), Point(0.0, 0.0)))),
    Figure("Square hole", arrayOf(Edge(Point(0.0, 0.0), Point(10.0, 0.0)), Edge(Point(10.0, 0.0), Point(10.0, 10.0)),
            Edge(Point(10.0, 10.0), Point(0.0, 10.0)), Edge(Point(0.0, 10.0), Point(0.0, 0.0)), Edge(Point(2.5, 2.5), Point(7.5, 2.5)),
            Edge(Point(7.5, 2.5), Point(7.5, 7.5)),Edge(Point(7.5, 7.5), Point(2.5, 7.5)), Edge(Point(2.5, 7.5), Point(2.5, 2.5)))),
    Figure("Strange", arrayOf(Edge(Point(0.0, 0.0), Point(2.5, 2.5)), Edge(Point(2.5, 2.5), Point(0.0, 10.0)),
            Edge(Point(0.0, 10.0), Point(2.5, 7.5)), Edge(Point(2.5, 7.5), Point(7.5, 7.5)), Edge(Point(7.5, 7.5), Point(10.0, 10.0)),
            Edge(Point(10.0, 10.0), Point(10.0, 0.0)), Edge(Point(10.0, 0.0), Point(2.5, 2.5)))),
    Figure("Exagon", arrayOf(Edge(Point(3.0, 0.0), Point(7.0, 0.0)), Edge(Point(7.0, 0.0), Point(10.0, 5.0)), Edge(Point(10.0, 5.0), Point(7.0, 10.0)),
            Edge(Point(7.0, 10.0), Point(3.0, 10.0)), Edge(Point(3.0, 10.0), Point(0.0, 5.0)), Edge(Point(0.0, 5.0), Point(3.0, 0.0)))))

    val points = listOf(Point(5.0, 5.0), Point(5.0, 8.0), Point(-10.0, 5.0), Point(0.0, 5.0),
            Point(10.0, 5.0), Point(8.0, 5.0), Point(10.0, 10.0))

    Ray_casting.check(figures, points)
}
```

{{out}}

```txt
points: [Point(x=5.0, y=5.0), Point(x=5.0, y=8.0), Point(x=-10.0, y=5.0), Point(x=0.0, y=5.0), Point(x=10.0, y=5.0), Point(x=8.0, y=5.0), Point(x=10.0, y=10.0)]
figure: Square
        Edge(s=Point(x=0.0, y=0.0), e=Point(x=10.0, y=0.0))
        Edge(s=Point(x=10.0, y=0.0), e=Point(x=10.0, y=10.0))
        Edge(s=Point(x=10.0, y=10.0), e=Point(x=0.0, y=10.0))
        Edge(s=Point(x=0.0, y=10.0), e=Point(x=0.0, y=0.0))
result: [true, true, false, false, true, true, false]
figure: Square hole
        Edge(s=Point(x=0.0, y=0.0), e=Point(x=10.0, y=0.0))
        Edge(s=Point(x=10.0, y=0.0), e=Point(x=10.0, y=10.0))
        Edge(s=Point(x=10.0, y=10.0), e=Point(x=0.0, y=10.0))
        Edge(s=Point(x=0.0, y=10.0), e=Point(x=0.0, y=0.0))
        Edge(s=Point(x=2.5, y=2.5), e=Point(x=7.5, y=2.5))
        Edge(s=Point(x=7.5, y=2.5), e=Point(x=7.5, y=7.5))
        Edge(s=Point(x=7.5, y=7.5), e=Point(x=2.5, y=7.5))
        Edge(s=Point(x=2.5, y=7.5), e=Point(x=2.5, y=2.5))
result: [false, true, false, false, true, true, false]
figure: Strange
        Edge(s=Point(x=0.0, y=0.0), e=Point(x=2.5, y=2.5))
        Edge(s=Point(x=2.5, y=2.5), e=Point(x=0.0, y=10.0))
        Edge(s=Point(x=0.0, y=10.0), e=Point(x=2.5, y=7.5))
        Edge(s=Point(x=2.5, y=7.5), e=Point(x=7.5, y=7.5))
        Edge(s=Point(x=7.5, y=7.5), e=Point(x=10.0, y=10.0))
        Edge(s=Point(x=10.0, y=10.0), e=Point(x=10.0, y=0.0))
        Edge(s=Point(x=10.0, y=0.0), e=Point(x=2.5, y=2.5))
result: [true, false, false, false, true, true, false]
figure: Exagon
        Edge(s=Point(x=3.0, y=0.0), e=Point(x=7.0, y=0.0))
        Edge(s=Point(x=7.0, y=0.0), e=Point(x=10.0, y=5.0))
        Edge(s=Point(x=10.0, y=5.0), e=Point(x=7.0, y=10.0))
        Edge(s=Point(x=7.0, y=10.0), e=Point(x=3.0, y=10.0))
        Edge(s=Point(x=3.0, y=10.0), e=Point(x=0.0, y=5.0))
        Edge(s=Point(x=0.0, y=5.0), e=Point(x=3.0, y=0.0))
result: [true, true, false, false, true, true, false]

```



## Liberty BASIC

Translated from C code at: http://alienryderflex.com/polygon/

Displays interactively on-screen.

```lb
NoMainWin
Global sw, sh, verts

sw = 640 :   sh = 480
WindowWidth  = sw+8 : WindowHeight = sh+31
UpperLeftX = (DisplayWidth -sw)/2
UpperLeftY = (DisplayHeight-sh)/2
Open"Ray Casting Algorithm" For Graphics_nf_nsb As #g
#g "Down; TrapClose [halt]"
h$ = "#g"

Dim xp(15),yp(15)
#g "when leftButtonDown [halt];when mouseMove checkPoint"
#g "when rightButtonDown [Repeat]"

[Repeat]
    #g "Cls;Fill 32 160 255; Color white;BackColor 32 160 255"
    #g "Place 5 460;\L-click to exit"
    #g "Place 485 460;\R-click for new polygon"

    'generate polygon from random points
    numPoints =  rand(4,15)
    verts = numPoints
    For i = 0 To numPoints-1
        xp(i) = rand(20,620)
        yp(i) = rand(40,420)
    Next
    Call drawPoly h$, verts, "white"
    #g "Flush"
    Wait

[halt]
Close #g
End

'Point In Polygon Function
Function pnp(x, y, numSides)
    j= numSides-1: oddNodes = 0
    For i = 0 To numSides-1
        If ((yp(i)<y) And (yp(j)>=y)) Or ((yp(j)<y) And (yp(i)>=y)) Then
            f1 = y - yp(i):f2 = yp(j) - yp(i): f3 = xp(j) - xp(i)
            If (xp(i) + f1 / f2 * f3) < x Then oddNodes = 1 - oddNodes
        End If
        j = i
    Next
    pnp = oddNodes
End Function

'draw the polygon
Sub drawPoly h$, verts, colour$
    #h$, "Color ";colour$
    j = verts-1
    For i = 0 To verts-1
        #h$ "Line ";xp(j);" ";yp(j);" ";xp(i);" ";yp(i)
        j = i
    Next
End Sub

'change message and color of polygon
Sub checkPoint h$, x, y
    If pnp(x,y,verts) Then
        #h$ "Color 32 160 255;BackColor 32 160 255"
        #h$ "Place 5 0;BoxFilled 150 20;Color white"
        #h$ "Place 7 15;\Mouse In Polygon"
        Call drawPoly h$, verts, "red"
    Else
        #h$ "Color 32 160 255;BackColor 32 160 255"
        #h$ "Place 5 0;BoxFilled 150 20;Color white"
        #h$ "Place 7 15;\Mouse Not In Polygon"
        Call drawPoly h$, verts, "white"
    End If
End Sub

Function rand(loNum,hiNum)
    rand = Int(Rnd(0)*(hiNum-loNum+1)+loNum)
End Function 
```



## OCaml

{{Trans|C}}

```ocaml
type point = { x:float; y:float }
 
type polygon = {
  vertices: point array;
  edges: (int * int) list;
}

let p x y = { x=x; y=y }

let square_v = [|
  (p 0. 0.); (p 10. 0.); (p 10. 10.); (p 0. 10.);
  (p 2.5 2.5); (p 7.5 0.1); (p 7.5 7.5); (p 2.5 7.5)
|]

let esa_v = [|
  (p 3. 0.); (p 7. 0.); (p 10. 5.); (p 7. 10.); (p 3. 10.); (p 0. 5.)
|]

let esa = {
  vertices = esa_v;
  edges = [ (0,1); (1,2); (2,3); (3,4); (4,5); (5,0) ]
}

let square = {
  vertices = square_v;
  edges = [ (0,1); (1,2); (2,3); (3,0) ]
}

let squarehole = {
  vertices = square_v;
  edges = [ (0,1); (1,2); (2,3); (3,0); (4,5); (5,6); (6,7); (7,4) ]
}

let strange = {
  vertices = square_v;
  edges = [ (0,4); (4,3); (3,7); (7,6); (6,2); (2,1); (1,5); (5,0) ]
}


let min_y ~a ~b = if a.y > b.y then (b) else (a)

let coeff_ang ~pa ~pb = (pb.y -. pa.y) /. (pb.x -. pa.x)

let huge_val = infinity

let hseg_intersect_seg ~s ~a ~b =
  let _eps =
    if s.y = (max a.y b.y) ||
       s.y = (min a.y b.y) then 0.00001 else 0.0
  in
  if  (s.y +. _eps) > (max a.y b.y) ||
      (s.y +. _eps) < (min a.y b.y) ||
       s.x > (max a.x b.x) then (false)
  else if s.x <= (min a.x b.x) then (true)
  else
    let ca = if a.x <> b.x then (coeff_ang a b) else (huge_val) in
    let my = min_y ~a ~b in
    let cp = if (s.x -. my.x) <> 0.0 then (coeff_ang my s) else (huge_val) in
    (cp >= ca)
;;


let point_is_inside ~poly ~pt =
  let cross = ref 0 in
  List.iter (fun (a,b) ->
    if hseg_intersect_seg pt
             poly.vertices.(a)
             poly.vertices.(b)
    then incr cross
  ) poly.edges;
  ( (!cross mod 2) <> 0)
;;


let make_test p label s =
  Printf.printf "point (%.5f,%.5f) is " p.x p.y;
  print_string (if point_is_inside s p
                then "INSIDE "
                else "OUTSIDE ");
  print_endline label;
;;


let () =
  let test_points = [
    (p 5. 5.); (p 5. 8.); (p 2. 2.); (p 0. 0.);
    (p 10. 10.); (p 2.5 2.5); (p 0.01 5.);
    (p 2.2 7.4); (p 0. 5.); (p 10. 5.); (p (-4.) 10.) ] in

  List.iter (fun p ->
    make_test p "square"     square;
    make_test p "squarehole" squarehole;
    make_test p "strange"    strange;
    make_test p "esa"        esa;
    print_newline()
  ) test_points;
;;
```



## Perl


```perl
use strict;
use List::Util qw(max min);

sub point_in_polygon
{
    my ( $point, $polygon ) = @_;

    my $count = 0;
    foreach my $side ( @$polygon ) {
	$count++ if ray_intersect_segment($point, $side);
    }
    return ($count % 2 == 0) ? 0 : 1;
}


my $eps = 0.0001;
my $inf = 1e600;

sub ray_intersect_segment
{
    my ($point, $segment) = @_;

    my ($A, $B) = @$segment;

    my @P = @$point; # copy it

    ($A, $B) = ($B, $A) if $A->[1] > $B->[1];

    $P[1] += $eps if ($P[1] == $A->[1]) || ($P[1] == $B->[1]);

    return 0 if ($P[1] < $A->[1]) || ( $P[1] > $B->[1]) || ($P[0] > max($A->[0],$B->[1]) );
    return 1 if $P[0] < min($A->[0], $B->[0]);

    my $m_red = ($A->[0] != $B->[0]) ? ( $B->[1] - $A->[1] )/($B->[0] - $A->[0]) : $inf;
    my $m_blue = ($A->[0] != $P[0]) ? ( $P[1] - $A->[1] )/($P[0] - $A->[0]) : $inf;

    return ($m_blue >= $m_red) ? 1 : 0;
}
```


Testing:

```perl
# the following are utilities to use the same Fortran data...
sub point
{
    [shift, shift];
}
sub create_polygon
{
    my ($pts, $sides) = @_;
    my @poly;
    for(my $i = 0; $i < $#$sides; $i += 2) {
	push @poly, [ $pts->[$sides->[$i]-1], $pts->[$sides->[$i+1]-1] ];
    }
    \@poly;
}

my @pts = ( point(0,0), point(10,0), point(10,10), point(0,10), 
	    point(2.5,2.5), point(7.5,2.5), point(7.5,7.5), point(2.5,7.5), 
	    point(0,5), point(10,5), 
	    point(3,0), point(7,0), point(7,10), point(3,10) );

my %pgs = (
    squared => create_polygon(\@pts, [ 1,2, 2,3, 3,4, 4,1 ] ),
    squaredhole => create_polygon(\@pts, [ 1,2, 2,3, 3,4, 4,1, 5,6, 6,7, 7,8, 8,5 ] ),
    strange => create_polygon(\@pts, [ 1,5, 5,4, 4,8, 8,7, 7,3, 3,2, 2,5 ] ),
    exagon => create_polygon(\@pts, [ 11,12, 12,10, 10,13, 13,14, 14,9, 9,11 ]) ,
);

my @p = ( point(5,5), point(5, 8), point(-10, 5), point(0,5), point(10,5), &
	  point(8,5), point(10,10) );

foreach my $pol ( sort keys %pgs ) {
    no strict 'refs';
    print "$pol\n";
    my $rp = $pgs{$pol};
    foreach my $tp ( @p ) {
	print "\t($tp->[0],$tp->[1]) " . 
           ( point_in_polygon($tp, $rp) ? "INSIDE" : "OUTSIDE" ) . "\n";
    }
}
```


## Perl 6


```perl6
constant ε = 0.0001;
 
sub ray-hits-seg([\Px,\Py], [[\Ax,\Ay], [\Bx,\By]] --> Bool) {
    Py += ε if Py == Ay | By;
 
    if Py < Ay or Py > By or Px > (Ax max Bx) {
	False;
    }
    elsif Px < (Ax min Bx) {
	True;
    }
    else {
	my \red  = Ax == Bx ?? Inf !! (By - Ay) / (Bx - Ax);
	my \blue = Ax == Px ?? Inf !! (Py - Ay) / (Px - Ax);
	blue >= red;
    }
}

sub point-in-poly(@point, @polygon --> Bool) {
    so 2 R% [+] gather for @polygon -> @side {
	take ray-hits-seg @point, @side.sort(*.[1]);
    }
}
 
my %poly =
    squared => 
	 [[[ 0.0,  0.0], [10.0,  0.0]],
	  [[10.0,  0.0], [10.0, 10.0]],
	  [[10.0, 10.0], [ 0.0, 10.0]],
	  [[ 0.0, 10.0], [ 0.0,  0.0]]],
    squaredhole =>
	 [[[ 0.0,  0.0], [10.0,  0.0]],
	  [[10.0,  0.0], [10.0, 10.0]],
	  [[10.0, 10.0], [ 0.0, 10.0]],
	  [[ 0.0, 10.0], [ 0.0,  0.0]],
	  [[ 2.5,  2.5], [ 7.5,  2.5]],
	  [[ 7.5,  2.5], [ 7.5,  7.5]],
	  [[ 7.5,  7.5], [ 2.5,  7.5]],
	  [[ 2.5,  7.5], [ 2.5,  2.5]]],
    strange =>
	 [[[ 0.0,  0.0], [ 2.5,  2.5]],
	  [[ 2.5,  2.5], [ 0.0, 10.0]],
	  [[ 0.0, 10.0], [ 2.5,  7.5]],
	  [[ 2.5,  7.5], [ 7.5,  7.5]],
	  [[ 7.5,  7.5], [10.0, 10.0]],
	  [[10.0, 10.0], [10.0,  0.0]],
	  [[10.0,  0.0], [ 2.5,  2.5]],
	  [[ 2.5,  2.5], [ 0.0,  0.0]]],  # conjecturally close polygon
    exagon =>
	 [[[ 3.0,  0.0], [ 7.0,  0.0]],
	  [[ 7.0,  0.0], [10.0,  5.0]],
	  [[10.0,  5.0], [ 7.0, 10.0]],
	  [[ 7.0, 10.0], [ 3.0, 10.0]],
	  [[ 3.0, 10.0], [ 0.0,  5.0]],
	  [[ 0.0,  5.0], [ 3.0,  0.0]]];
 
my @test-points =
	  [  5.0,  5.0],
	  [  5.0,  8.0],
	  [-10.0,  5.0],
	  [  0.0,  5.0],
	  [ 10.0,  5.0],
	  [  8.0,  5.0],
	  [ 10.0, 10.0];
 
for <squared squaredhole strange exagon> -> $polywanna {
    say "$polywanna";
    my @poly = %poly{$polywanna}[];
    for @test-points -> @point {
	say "\t(@point.fmt('%.1f',','))\t{ point-in-poly(@point, @poly) ?? 'IN' !! 'OUT' }";
    }
}
```

{{out}}

```txt
squared
	(5.0,5.0)	IN
	(5.0,8.0)	IN
	(-10.0,5.0)	OUT
	(0.0,5.0)	OUT
	(10.0,5.0)	IN
	(8.0,5.0)	IN
	(10.0,10.0)	OUT
squaredhole
	(5.0,5.0)	OUT
	(5.0,8.0)	IN
	(-10.0,5.0)	OUT
	(0.0,5.0)	OUT
	(10.0,5.0)	IN
	(8.0,5.0)	IN
	(10.0,10.0)	OUT
strange
	(5.0,5.0)	IN
	(5.0,8.0)	OUT
	(-10.0,5.0)	OUT
	(0.0,5.0)	OUT
	(10.0,5.0)	IN
	(8.0,5.0)	IN
	(10.0,10.0)	OUT
exagon
	(5.0,5.0)	IN
	(5.0,8.0)	IN
	(-10.0,5.0)	OUT
	(0.0,5.0)	OUT
	(10.0,5.0)	IN
	(8.0,5.0)	IN
	(10.0,10.0)	OUT
```



## Phix


```Phix
constant true = (1=1), false = not true
constant inf = 1e300*1e300
 
function rayIntersectsSegment(sequence point, sequence segment)
atom {pX,pY} = point
atom {{aX,aY},{bX,bY}} = segment
atom m_red,m_blue
    -- ensure {aX,aY} is the segment end-point with the smallest y coordinate
    if aY>bY then
        {{bX,bY},{aX,aY}} = segment
    end if
    if pY=aY or pY=bY then
        --
        -- Consider a ray passing through the top or left corner of a diamond:
        --          /
        --  --- or  -
        --   ^      \
        -- In both cases it touches both edges, but ends up outside in the
        --  top case, whereas it ends up inside in the left case. Just move
        --  the y co-ordinate down a smidge and it'll work properly, by 
        --  missing one line in the left/right cases and hitting both/none 
        --  in the top/bottom cases.
        --
        pY += 0.000001
    end if
    if pY<aY or pY>bY then return false end if
    if pX>max(aX,bX) then return false end if
    if pX<min(aX,bX) then return true end if
    if aX!=bX then
        m_red = (bY-aY)/(bX-aX)
    else
        m_red = inf
    end if
    if aX!=pX then
        m_blue = (pY-aY)/(pX-aX)
    else
        m_blue = inf
    end if
    return m_blue >= m_red
end function

function inside(sequence point, sequence poly)
integer in = 0
    for i=1 to length(poly) do
        if rayIntersectsSegment(point,poly[i]) then
            in = not in
        end if
    end for
    return in
end function

constant INOUT = {"in", "out"}
function in(integer flag, integer expected)
    if flag=expected then
        return INOUT[2-flag]
    end if
    return INOUT[2-flag] & "*** ERROR ***" 
end function

constant INSTAR = "* "
function instar(integer flag)
    return INSTAR[2-flag]
end function

constant test_points = {{5,5},{5,8},{-10,5},{0,5},{10,5},{8,5},{10,10}}

--constant NAME = 1, POLY = 2, EXPECTED = 3
constant test_polygons = {
        {"square",      {{{0,0},{10,0}},{{10,0},{10,10}},{{10,10},{0,10}},{{0,10},{0,0}}},
                        {true,true,false,false,true,true,false}},
        {"square hole", {{{0,0},{10,0}},{{10,0},{10,10}},{{10,10},{0,10}},{{0,10},{0,0}},
                         {{2.5,2.5},{7.5,2.5}},{{7.5,2.5},{7.5,7.5}},{{7.5,7.5},{2.5,7.5}},{{2.5,7.5},{2.5,2.5}}},
                        {false,true,false,false,true,true,false}},
        {"strange",     {{{0,5},{2.5,2.5}},{{2.5,2.5},{0,10}},{{0,10},{2.5,7.5}},{{2.5,7.5},{7.5,7.5}},
                         {{7.5,7.5},{10,10}},{{10,10},{10,0}},{{10,0},{2.5,2.5}}},
                        {true,false,false,false,true,true,false}},
        {"exagon",      {{{3,0},{7,0}},{{7,0},{10,5}},{{10,5},{7,10}},{{7,10},{3,10}},{{3,10},{0,5}},{{0,5},{3,0}}},
                        {true,true,false,false,true,true,false}}
}

sequence name, poly, expected, tp
 
    for i=1 to length(test_polygons) do
        {name,poly,expected} = test_polygons[i]
        printf(1,"\n%12s:",{name})
        for j=1 to length(test_points) do
            tp = test_points[j]
            printf(1," %s, %-4s",{sprint(tp),in(inside(tp,poly),expected[j])})
        end for
    end for

    puts(1,"\n\n\n")

    for i=0 to 10 do
        for j=1 to length(test_polygons) do
            puts(1,"     ")
            poly = test_polygons[j][2]
            for k=0 to 10 do
                puts(1,instar(inside({k+0.5,10.5-i},poly)))
            end for
        end for
        puts(1,"\n")
    end for
```

{{out}}

```txt

      square: {5,5}, in   {5,8}, in   {-10,5}, out  {0,5}, out  {10,5}, in   {8,5}, in   {10,10}, out
 square hole: {5,5}, out  {5,8}, in   {-10,5}, out  {0,5}, out  {10,5}, in   {8,5}, in   {10,10}, out
     strange: {5,5}, in   {5,8}, out  {-10,5}, out  {0,5}, out  {10,5}, in   {8,5}, in   {10,10}, out
      exagon: {5,5}, in   {5,8}, in   {-10,5}, out  {0,5}, out  {10,5}, in   {8,5}, in   {10,10}, out


     **********      **********      *                  ****
     **********      **********       *       *        ******
     **********      **********       *      **        *******
     **********      ***     **       * *******       ********
     **********      ***     **         *******      **********
     **********      ***     **      * ********      **********
     **********      ***     **      **********       ********
     **********      ***     **      **********        *******
     **********      **********            ****        ******
     **********      **********               *         ****

```



## PicoLisp


```PicoLisp
(scl 4)

(de intersects (Px Py Ax Ay Bx By)
   (when (> Ay By)
      (xchg 'Ax 'Bx)
      (xchg 'Ay 'By) )
   (when (or (= Py Ay) (= Py By))
      (inc 'Py) )
   (and
      (>= Py Ay)
      (>= By Py)
      (>= (max Ax Bx) Px)
      (or
         (> (min Ax Bx) Px)
         (= Ax Px)
         (and
            (<> Ax Bx)
            (>=
               (*/ (- Py Ay) 1.0 (- Px Ax))            # Blue
               (*/ (- By Ay) 1.0 (- Bx Ax)) ) ) ) ) )  # Red

(de inside (Pt Poly)
   (let Res NIL
      (for Edge Poly
         (when (apply intersects Edge (car Pt) (cdr Pt))
            (onOff Res) ) )
      Res ) )
```

Test data:
<pre style="height:20em;overflow:scroll">(de Square
   ( 0.0  0.0  10.0  0.0)
   (10.0  0.0  10.0 10.0)
   (10.0 10.0   0.0 10.0)
   ( 0.0 10.0   0.0  0.0) )

(de SquareHole
   ( 0.0  0.0  10.0  0.0)
   (10.0  0.0  10.0 10.0)
   (10.0 10.0   0.0 10.0)
   ( 0.0 10.0   0.0  0.0)
   ( 2.5  2.5   7.5  2.5)
   ( 7.5  2.5   7.5  7.5)
   ( 7.5  7.5   2.5  7.5)
   ( 2.5  7.5   2.5  2.5) )

(de Strange
   ( 0.0  0.0   2.5  2.5)
   ( 2.5  2.5   0.0 10.0)
   ( 0.0 10.0   2.5  7.5)
   ( 2.5  7.5   7.5  7.5)
   ( 7.5  7.5  10.0 10.0)
   (10.0 10.0  10.0  0.0)
   (10.0  0.0   2.5  2.5) )

(de Exagon
   ( 3.0  0.0   7.0  0.0)
   ( 7.0  0.0  10.0  5.0)
   (10.0  5.0   7.0 10.0)
   ( 7.0 10.0   3.0 10.0)
   ( 3.0 10.0   0.0  5.0)
   ( 0.0  5.0   3.0  0.0) )
```

Output:
<pre style="height:20em;overflow:scroll">: (inside (5.0 . 5.0) Square)
-> T
: (inside (5.0 . 8.0) Square)
-> T
: (inside (-10.0 . 5.0) Square)
-> NIL
: (inside (0.0 . 5.0) Square)
-> NIL
: (inside (10.0 . 5.0) Square)
-> T
: (inside (8.0 . 5.0) Square)
-> T
: (inside (10.0 . 10.0) Square)
-> NIL

: (inside (5.0 . 5.0) SquareHole)
-> NIL
: (inside (5.0 . 8.0) SquareHole)
-> T
: (inside (-10.0 . 5.0) SquareHole)
-> NIL
: (inside (0 . 5.0) SquareHole)
-> NIL
: (inside (10.0 . 5.0) SquareHole)
-> T
: (inside (8.0 . 5.0) SquareHole)
-> T
: (inside (10.0 . 10.0) SquareHole)
-> NIL

: (inside (5.0 . 5.0) Strange)
-> T
: (inside (5.0 . 8.0) Strange)
-> NIL
: (inside (-10.0 . 5.0) Strange)
-> NIL
: (inside (0 . 5.0) Strange)
-> NIL
: (inside (10.0 . 5.0) Strange)
-> T
: (inside (8.0 . 5.0) Strange)
-> T
: (inside (10.0 . 10.0) Strange)
-> NIL

: (inside (5.0 . 5.0) Exagon)
-> T
: (inside (5.0 . 8.0) Exagon)
-> T
: (inside (-10.0 . 5.0) Exagon)
-> NIL
: (inside (0.0 . 5.0) Exagon)
-> NIL
: (inside (10.0 . 5.0) Exagon)
-> T
: (inside (8.0 . 5.0) Exagon)
-> T
: (inside (10.0 . 10.0) Exagon)
-> NIL
```



## PureBasic

The code below is includes a GUI for drawing a polygon with the mouse that constantly tests whether the mouse is inside or outside the polygon.  It displays a message and changes the windows color slightly to indicate if the pointer is inside or outside the polygon being drawn.  The routine that does the checking is called inpoly() and it returns a value of one if the point is with the polygon and zero if it isn't.

```PureBasic
Structure point_f
  x.f
  y.f
EndStructure
Procedure inpoly(*p.point_f, List poly.point_f())
  Protected.point_f new, old, lp, rp
  Protected inside
  If ListSize(poly()) < 3: ProcedureReturn 0: EndIf 
  LastElement(poly()): old = poly()
  ForEach poly()
    ;find leftmost endpoint 'lp' and the rightmost endpoint 'rp' based on x value
    If poly()\x > old\x 
      lp = old
      rp = poly()
    Else
      lp = poly()
      rp = old
    EndIf 
    If lp\x < *p\x And *p\x <= rp\x And (*p\y - lp\y) * (rp\x - lp\x) < (rp\y - lp\y) * (*p\x - lp\x)
      inside = ~inside
    EndIf 
    old = poly()
  Next 
  ProcedureReturn inside & 1
EndProcedure

If InitSprite()
  If InitKeyboard() And InitMouse()
    OpenWindow(0, 0, 0, 800, 600, "Press [Esc] to close, [Left mouse button] Add Point, [Right mouse button] Clear All Points.", #PB_Window_ScreenCentered | #PB_Window_SystemMenu)
    OpenWindowedScreen(WindowID(0), 0, 0, 800, 600, 1, 0, 0)
    SetFrameRate(60)
  EndIf
Else
  MessageRequester("", "Unable to initsprite"): End
EndIf

NewList v.point_f()
Define.point_f pvp, mp
Define Col, EventID, mode.b, modetxt.s
Repeat
  Delay(1)
  EventID = WindowEvent()
  ExamineKeyboard()
  ExamineMouse()
  ClearScreen(Col)
  
  mp\x = MouseX()
  mp\y = MouseY()
  If MouseButton(#PB_MouseButton_Left)
    AddElement(v())
    v()\x = mp\x
    v()\y = mp\y
    Delay(100)
  EndIf
  
  If MouseButton(#PB_MouseButton_Right)
    ClearList(v())
    Delay(100)
  EndIf
  
  StartDrawing(ScreenOutput())
    If LastElement(v())
      pvp = v()
      ForEach v()
        LineXY(pvp\x, pvp\y, v()\x, v()\y, RGB(0, $FF, 0)) ;Green
        Circle(pvp\x, pvp\y, 5, RGB($FF, 0, 0)) ;Red
        pvp = v()
      Next
    EndIf 
    Circle(MouseX(), MouseY(), 5, RGB($C0, $C0, $FF)) ;LightBlue 

    If inpoly(mp, v())
      modetxt = "You are in the polygon."
      Col = RGB(0, 0, 0)
    Else
      modetxt = "You are not in the polygon."
      Col = RGB($50, $50, $50)
    EndIf
    DrawText((800 - TextWidth(modetxt)) / 2, 0, modetxt) 
  StopDrawing()
  
  FlipBuffers()
Until KeyboardReleased(#PB_Key_Escape) Or EventID = #PB_Event_CloseWindow
```



## Python


```python
from collections import namedtuple
from pprint import pprint as pp
import sys

Pt = namedtuple('Pt', 'x, y')               # Point
Edge = namedtuple('Edge', 'a, b')           # Polygon edge from a to b
Poly = namedtuple('Poly', 'name, edges')    # Polygon

_eps = 0.00001
_huge = sys.float_info.max
_tiny = sys.float_info.min

def rayintersectseg(p, edge):
    ''' takes a point p=Pt() and an edge of two endpoints a,b=Pt() of a line segment returns boolean
    '''
    a,b = edge
    if a.y > b.y:
        a,b = b,a
    if p.y == a.y or p.y == b.y:
        p = Pt(p.x, p.y + _eps)

    intersect = False

    if (p.y > b.y or p.y < a.y) or (
        p.x > max(a.x, b.x)):
        return False

    if p.x < min(a.x, b.x):
        intersect = True
    else:
        if abs(a.x - b.x) > _tiny:
            m_red = (b.y - a.y) / float(b.x - a.x)
        else:
            m_red = _huge
        if abs(a.x - p.x) > _tiny:
            m_blue = (p.y - a.y) / float(p.x - a.x)
        else:
            m_blue = _huge
        intersect = m_blue >= m_red
    return intersect

def _odd(x): return x%2 == 1

def ispointinside(p, poly):
    ln = len(poly)
    return _odd(sum(rayintersectseg(p, edge)
                    for edge in poly.edges ))

def polypp(poly):
    print ("\n  Polygon(name='%s', edges=(" % poly.name)
    print ('   ', ',\n    '.join(str(e) for e in poly.edges) + '\n    ))')

if __name__ == '__main__':
    polys = [
      Poly(name='square', edges=(
        Edge(a=Pt(x=0, y=0), b=Pt(x=10, y=0)),
        Edge(a=Pt(x=10, y=0), b=Pt(x=10, y=10)),
        Edge(a=Pt(x=10, y=10), b=Pt(x=0, y=10)),
        Edge(a=Pt(x=0, y=10), b=Pt(x=0, y=0))
        )),
      Poly(name='square_hole', edges=(
        Edge(a=Pt(x=0, y=0), b=Pt(x=10, y=0)),
        Edge(a=Pt(x=10, y=0), b=Pt(x=10, y=10)),
        Edge(a=Pt(x=10, y=10), b=Pt(x=0, y=10)),
        Edge(a=Pt(x=0, y=10), b=Pt(x=0, y=0)),
        Edge(a=Pt(x=2.5, y=2.5), b=Pt(x=7.5, y=2.5)),
        Edge(a=Pt(x=7.5, y=2.5), b=Pt(x=7.5, y=7.5)),
        Edge(a=Pt(x=7.5, y=7.5), b=Pt(x=2.5, y=7.5)),
        Edge(a=Pt(x=2.5, y=7.5), b=Pt(x=2.5, y=2.5))
        )),
      Poly(name='strange', edges=(
        Edge(a=Pt(x=0, y=0), b=Pt(x=2.5, y=2.5)),
        Edge(a=Pt(x=2.5, y=2.5), b=Pt(x=0, y=10)),
        Edge(a=Pt(x=0, y=10), b=Pt(x=2.5, y=7.5)),
        Edge(a=Pt(x=2.5, y=7.5), b=Pt(x=7.5, y=7.5)),
        Edge(a=Pt(x=7.5, y=7.5), b=Pt(x=10, y=10)),
        Edge(a=Pt(x=10, y=10), b=Pt(x=10, y=0)),
        Edge(a=Pt(x=10, y=0), b=Pt(x=2.5, y=2.5))
        )),
      Poly(name='exagon', edges=(
        Edge(a=Pt(x=3, y=0), b=Pt(x=7, y=0)),
        Edge(a=Pt(x=7, y=0), b=Pt(x=10, y=5)),
        Edge(a=Pt(x=10, y=5), b=Pt(x=7, y=10)),
        Edge(a=Pt(x=7, y=10), b=Pt(x=3, y=10)),
        Edge(a=Pt(x=3, y=10), b=Pt(x=0, y=5)),
        Edge(a=Pt(x=0, y=5), b=Pt(x=3, y=0))
        )),
      ]
    testpoints = (Pt(x=5, y=5), Pt(x=5, y=8),
                  Pt(x=-10, y=5), Pt(x=0, y=5),
                  Pt(x=10, y=5), Pt(x=8, y=5),
                  Pt(x=10, y=10))
    
    print ("\n TESTING WHETHER POINTS ARE WITHIN POLYGONS")
    for poly in polys:
        polypp(poly)
        print ('   ', '\t'.join("%s: %s" % (p, ispointinside(p, poly))
                               for p in testpoints[:3]))
        print ('   ', '\t'.join("%s: %s" % (p, ispointinside(p, poly))
                               for p in testpoints[3:6]))
        print ('   ', '\t'.join("%s: %s" % (p, ispointinside(p, poly))
                               for p in testpoints[6:]))
```


'''Sample output'''
<pre style="height:20em;overflow:scroll">
 TESTING WHETHER POINTS ARE WITHIN POLYGONS

  Polygon(name='square', edges=(
    Edge(a=Pt(x=0, y=0), b=Pt(x=10, y=0)),
    Edge(a=Pt(x=10, y=0), b=Pt(x=10, y=10)),
    Edge(a=Pt(x=10, y=10), b=Pt(x=0, y=10)),
    Edge(a=Pt(x=0, y=10), b=Pt(x=0, y=0))
    ))
    Pt(x=5, y=5): True	Pt(x=5, y=8): True	Pt(x=-10, y=5): False
    Pt(x=0, y=5): False	Pt(x=10, y=5): True	Pt(x=8, y=5): True
    Pt(x=10, y=10): False

  Polygon(name='square_hole', edges=(
    Edge(a=Pt(x=0, y=0), b=Pt(x=10, y=0)),
    Edge(a=Pt(x=10, y=0), b=Pt(x=10, y=10)),
    Edge(a=Pt(x=10, y=10), b=Pt(x=0, y=10)),
    Edge(a=Pt(x=0, y=10), b=Pt(x=0, y=0)),
    Edge(a=Pt(x=2.5, y=2.5), b=Pt(x=7.5, y=2.5)),
    Edge(a=Pt(x=7.5, y=2.5), b=Pt(x=7.5, y=7.5)),
    Edge(a=Pt(x=7.5, y=7.5), b=Pt(x=2.5, y=7.5)),
    Edge(a=Pt(x=2.5, y=7.5), b=Pt(x=2.5, y=2.5))
    ))
    Pt(x=5, y=5): False	Pt(x=5, y=8): True	Pt(x=-10, y=5): False
    Pt(x=0, y=5): False	Pt(x=10, y=5): True	Pt(x=8, y=5): True
    Pt(x=10, y=10): False

  Polygon(name='strange', edges=(
    Edge(a=Pt(x=0, y=0), b=Pt(x=2.5, y=2.5)),
    Edge(a=Pt(x=2.5, y=2.5), b=Pt(x=0, y=10)),
    Edge(a=Pt(x=0, y=10), b=Pt(x=2.5, y=7.5)),
    Edge(a=Pt(x=2.5, y=7.5), b=Pt(x=7.5, y=7.5)),
    Edge(a=Pt(x=7.5, y=7.5), b=Pt(x=10, y=10)),
    Edge(a=Pt(x=10, y=10), b=Pt(x=10, y=0)),
    Edge(a=Pt(x=10, y=0), b=Pt(x=2.5, y=2.5))
    ))
    Pt(x=5, y=5): True	Pt(x=5, y=8): False	Pt(x=-10, y=5): False
    Pt(x=0, y=5): False	Pt(x=10, y=5): True	Pt(x=8, y=5): True
    Pt(x=10, y=10): False

  Polygon(name='exagon', edges=(
    Edge(a=Pt(x=3, y=0), b=Pt(x=7, y=0)),
    Edge(a=Pt(x=7, y=0), b=Pt(x=10, y=5)),
    Edge(a=Pt(x=10, y=5), b=Pt(x=7, y=10)),
    Edge(a=Pt(x=7, y=10), b=Pt(x=3, y=10)),
    Edge(a=Pt(x=3, y=10), b=Pt(x=0, y=5)),
    Edge(a=Pt(x=0, y=5), b=Pt(x=3, y=0))
    ))
    Pt(x=5, y=5): True	Pt(x=5, y=8): True	Pt(x=-10, y=5): False
    Pt(x=0, y=5): False	Pt(x=10, y=5): True	Pt(x=8, y=5): True
    Pt(x=10, y=10): False
```


'''Helper routine to convert Fortran Polygons and points to Python'''

```python
def _convert_fortran_shapes():
    point = Pt
    pts = (point(0,0), point(10,0), point(10,10), point(0,10), 
           point(2.5,2.5), point(7.5,2.5), point(7.5,7.5), point(2.5,7.5), 
           point(0,5), point(10,5), 
           point(3,0), point(7,0), point(7,10), point(3,10))
    p = (point(5,5), point(5, 8), point(-10, 5), point(0,5), point(10,5),
         point(8,5), point(10,10) )
 
    def create_polygon(pts,vertexindex):
        return [tuple(Edge(pts[vertexindex[i]-1], pts[vertexindex[i+1]-1])
                       for i in range(0, len(vertexindex), 2) )]
    polys=[]
    polys += create_polygon(pts, ( 1,2, 2,3, 3,4, 4,1 ) )
    polys += create_polygon(pts, ( 1,2, 2,3, 3,4, 4,1, 5,6, 6,7, 7,8, 8,5 ) )
    polys += create_polygon(pts, ( 1,5, 5,4, 4,8, 8,7, 7,3, 3,2, 2,5 ) )
    polys += create_polygon(pts, ( 11,12, 12,10, 10,13, 13,14, 14,9, 9,11 ) )

    names = ( "square", "square_hole", "strange", "exagon" )
    polys = [Poly(name, edges)
             for name, edges in zip(names, polys)]
    print 'polys = ['
    for p in polys:
        print "  Poly(name='%s', edges=(" % p.name
        print '   ', ',\n    '.join(str(e) for e in p.edges) + '\n    )),'
    print '  ]'
 _convert_fortran_shapes()
```



## R


```R
point_in_polygon <- function(polygon, p) {
  count <- 0
  for(side in polygon) {
    if ( ray_intersect_segment(p, side) ) {
      count <- count + 1
    }
  }
  if ( count %% 2 == 1 )
    "INSIDE"
  else
    "OUTSIDE"
}

ray_intersect_segment <- function(p, side) {
  eps <- 0.0001
  a <- side$A
  b <- side$B
  if ( a$y > b$y ) {
    a <- side$B
    b <- side$A
  }
  if ( (p$y == a$y) || (p$y == b$y) ) {
    p$y <- p$y + eps
  }
  if ( (p$y < a$y) || (p$y > b$y) )
    return(FALSE)
  else if ( p$x > max(a$x, b$x) )
    return(FALSE)
  else {
    if ( p$x < min(a$x, b$x) )
      return(TRUE)
    else {
      if ( a$x != b$x )
        m_red <- (b$y - a$y) / (b$x - a$x)
      else
        m_red <- Inf
      if ( a$x != p$x )
        m_blue <- (p$y - a$y) / (p$x - a$x)
      else
        m_blue <- Inf
      return( m_blue >= m_red )
    }
  }
}
```



```R
######## utility functions #########

point <- function(x,y) list(x=x, y=y)

# pts = list(p1, p2, ... )... coords
# segs = list(c(1,2), c(2,1) ...) indices
createPolygon <- function(pts, segs) {
  pol <- list()
  for(pseg in segs) {
    pol <- c(pol, list(list(A=pts[[pseg[1]]], B=pts[[pseg[2]]])))
  }
  pol
}
```



```R
#### testing ####

pts <- list(point(0,0), point(10,0), point(10,10), point(0,10),
            point(2.5,2.5), point(7.5,2.5), point(7.5,7.5), point(2.5,7.5), 
            point(0,5), point(10,5), 
            point(3,0), point(7,0), point(7,10), point(3,10))

polygons <-
  list(
       square = createPolygon(pts, list(c(1,2), c(2,3), c(3,4), c(4,1))),
       squarehole = createPolygon(pts, list(c(1,2), c(2,3), c(3,4), c(4,1), c(5,6), c(6,7), c(7,8), c(8,5))),
       exagon = createPolygon(pts, list(c(11,12), c(12,10), c(10,13), c(13,14), c(14,9), c(9,11)))
      )

testpoints <-
  list(
       point(5,5), point(5, 8), point(-10, 5), point(0,5), point(10,5),
       point(8,5), point(9.9,9.9)
      )

for(p in testpoints) {
  for(polysi in 1:length(polygons)) {
    cat(sprintf("point (%lf, %lf) is %s polygon (%s)\n",
                  p$x, p$y, point_in_polygon(polygons[[polysi]], p), names(polygons[polysi])))
  }
}
```



## Racket

Straightforward implementation of pseudocode

```scheme

#lang racket

(module pip racket
  (require racket/contract)

  (provide point)
  (provide seg)
  (provide (contract-out [point-in-polygon? (-> 
                                             point? 
                                             list? 
                                             boolean?)]))

  (struct point (x y) #:transparent)
  (struct seg (Ax Ay Bx By) #:transparent)
  (define ε 0.000001)
  (define (neq? x y) (not (eq? x y)))

  (define (ray-cross-seg? r s)
    (let* ([Ax (seg-Ax s)] [Ay (seg-Ay s)]
           [Bx (seg-Bx s)] [By (seg-By s)]
           [Px (point-x r)] [Pyo (point-y r)]
           [Py (+ Pyo (if (or (eq? Pyo Ay) 
                              (eq? Pyo By)) 
                          ε 0))])

      (define Ax2 (if (< Ay By) Ax Bx))
      (define Ay2 (if (< Ay By) Ay By))
      (define Bx2 (if (< Ay By) Bx Ax))
      (define By2 (if (< Ay By) By Ay))

      (cond [(or (> Py (max Ay By)) (< Py (min Ay By))) #f]
            [(> Px (max Ax Bx)) #f]
            [else (cond 
                [(< Px (min Ax Bx)) #t]
                [else
                 (let ([red (if (neq? Ax2 Bx2)
                               (/ (- By2 Ay2) (- Bx2 Ax2))
                               +inf.0)]
                      [blue (if (neq? Ax2 Px)
                                (/ (- Py Ay2) (- Px Ax2))
                                 +inf.0)])
                   (if (>= blue red) #t #f))])])))

  (define (point-in-polygon? point polygon)
    (odd? 
     (for/fold ([c 0]) ([seg polygon])
       (+ c (if (ray-cross-seg? point seg) 1 0))))))

(require 'pip)

(define test-point-list
  (list
   (point 5.0    5.0) 
   (point 5.0    8.0) 
   (point -10.0  5.0) 
   (point  0.0   5.0) 
   (point 10.0   5.0) 
   (point  8.0   5.0) 
   (point 10.0  10.0)))

(define square
  (list (seg 0.0   0.0  10.0   0.0) 
        (seg 10.0  0.0  10.0  10.0) 
        (seg 10.0  10.0  0.0  10.0) 
        (seg 0.0   0.0  0.0   10.0)))

(define exagon
  (list (seg  3.0   0.0   7.0   0.0) 
        (seg  7.0   0.0  10.0   5.0) 
        (seg 10.0   5.0   7.0  10.0) 
        (seg  7.0  10.0   3.0  10.0) 
        (seg  0.0   5.0   3.0   10.0) 
        (seg  3.0   0.0 0.0   5.0)))

(define (test-figure fig name)
  (printf "\ntesting ~a: \n" name)
  (for ([p test-point-list])
    (printf "testing ~v: ~a\n"  p (point-in-polygon? p fig))))

(test-figure square "square")
(test-figure exagon "exagon")

```


{{out}}

```txt

testing square: 
testing (point 5.0 5.0): #t
testing (point 5.0 8.0): #t
testing (point -10.0 5.0): #f
testing (point 0.0 5.0): #f
testing (point 10.0 5.0): #t
testing (point 8.0 5.0): #t
testing (point 10.0 10.0): #f

testing exagon: 
testing (point 5.0 5.0): #t
testing (point 5.0 8.0): #t
testing (point -10.0 5.0): #f
testing (point 0.0 5.0): #f
testing (point 10.0 5.0): #t
testing (point 8.0 5.0): #t
testing (point 10.0 10.0): #f

```



## REXX

Over half of the REXX program is devoted to specifying/defining/assigning the points for the test cases and for the various polygons.

Code was added to facilitate easier specification of polygon sides by just specifying their   ''vertices''   instead of specifying their   ''line segments''. 

```rexx
/*REXX program verifies if a   horizontal ray   from   point P   intersects  a polygon. */
call points 5 5,       5 8,       -10  5,       0  5,       10  5,       8  5,       10 10
A=2.5;    B=7.5                            /* ◄───── used for shorter arguments (below).*/
call poly 0 0, 10 0,  10 10,  0 10                             ;   call test 'square'
call poly 0 0, 10 0,  10 10,  0 10,  A  A,   B  A,   B  B, A B ;   call test 'square hole'
call poly 0 0,  A A,   0 10,  A  B,  B  B,  10 10,  10  0      ;   call test 'irregular'
call poly 3 0,  7 0,  10  5,  7 10,  3 10,   0  5              ;   call test 'hexagon'
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
in$out: procedure expose point. poly.;     parse arg p;              #=0
                     do side=1  to poly.0  by 2;   #=# +intersect(p, side);   end /*side*/
        return # // 2                            /*ODD  is inside.     EVEN  is outside.*/
/*──────────────────────────────────────────────────────────────────────────────────────*/
intersect: procedure expose point. poly.;         parse arg ?,s;                  sp=s + 1
           epsilon= '1e' || (-digits() %2);       infinity= "1e" || (digits() *2)
           Px=point.?.x;      Ax=poly.s.x;        Bx=poly.sp.x
           Py=point.?.y;      Ay=poly.s.y;        By=poly.sp.y  /* [↓]  do a vertex swap*/
           if Ay>By           then parse  value   Ax Ay Bx By    with    Bx By Ax Ay
           if Py=Ay | Py=By   then Py=Py + epsilon
           if Py<Ay | Py>By | Px>max(Ax, Bx)  then  return 0
           if                 Px<min(Ax, Bx)  then  return 1
           if Ax\=Bx          then red = (By-Ay) / (Bx-Ax)
                              else red = infinity
           if Ax\=Px          then                  return  (Py-Ay) / (Px-Ax)  >=  red
           return  1
/*──────────────────────────────────────────────────────────────────────────────────────*/
points: wx=0;  wy=0;     do j=1  for arg();         parse value  arg(j)    with   xx  yy
                         wx=max(wx, length(xx) );   call  value  'POINT.'j".X",   xx
                         wy=max(wy, length(yy) );   call  value  'POINT.'j".Y",       yy
                         end   /*j*/
        call value point.0,  j-1                         /*define the number of points. */
        return                                           /* [↑]  adjust J  (for DO loop)*/
/*──────────────────────────────────────────────────────────────────────────────────────*/
poly:   @= 'POLY.';      parse arg Fx Fy                 /* [↓]  process the X,Y points.*/
        n=0
                 do j=1  for arg();      n=n + 1;   parse value arg(j)   with   xx yy
                 call value @ || n'.X',  xx ;       call value  @ || n".Y", yy
                 if n//2  then iterate;  n=n + 1         /*Inside? Then skip this point.*/
                 call value @ || n'.X',  xx ;       call value  @ || n".Y", yy
                 end   /*j*/
        n=n + 1                                          /*POLY.0  is # segments(sides).*/
        call value @ || n'.X', Fx;       call value @ || n".Y", Fy;     call value @'0', n
        return
/*──────────────────────────────────────────────────────────────────────────────────────*/
test:   say;     do k=1  for point.0;    w=wx + wy + 2   /*W, WX, WY ≡are various widths*/
                 say right('  ['arg(1)"]  point:",  30),
                     right( right(point.k.x, wx)', 'right(point.k.y, wy), w)     "  is  ",
                     right( word('outside inside',  in$out(k) + 1),  7)
                 end   /*k*/
        return                                           /* [↑] format the output nicely*/
```

{{out|output|text=  when using the default inputs:}}

```txt

              [square]  point:   5,  5   is    inside
              [square]  point:   5,  8   is    inside
              [square]  point: -10,  5   is   outside
              [square]  point:   0,  5   is   outside
              [square]  point:  10,  5   is    inside
              [square]  point:   8,  5   is    inside
              [square]  point:  10, 10   is   outside

         [square hole]  point:   5,  5   is   outside
         [square hole]  point:   5,  8   is    inside
         [square hole]  point: -10,  5   is   outside
         [square hole]  point:   0,  5   is   outside
         [square hole]  point:  10,  5   is    inside
         [square hole]  point:   8,  5   is    inside
         [square hole]  point:  10, 10   is   outside

           [irregular]  point:   5,  5   is    inside
           [irregular]  point:   5,  8   is   outside
           [irregular]  point: -10,  5   is   outside
           [irregular]  point:   0,  5   is   outside
           [irregular]  point:  10,  5   is    inside
           [irregular]  point:   8,  5   is    inside
           [irregular]  point:  10, 10   is   outside

             [hexagon]  point:   5,  5   is    inside
             [hexagon]  point:   5,  8   is    inside
             [hexagon]  point: -10,  5   is   outside
             [hexagon]  point:   0,  5   is   outside
             [hexagon]  point:  10,  5   is    inside
             [hexagon]  point:   8,  5   is    inside
             [hexagon]  point:  10, 10   is   outside

```



## Rust

{{trans|Python}}

```rust
use std::f64;

const _EPS: f64 = 0.00001;
const _MIN: f64 = f64::MIN_POSITIVE;
const _MAX: f64 = f64::MAX;

#[derive(Clone)]
struct Point {
    x: f64,
    y: f64,
}

#[derive(Clone)]
struct Edge {
    pt1: Point,
    pt2: Point,
}

impl Edge {
    fn new(pt1: (f64, f64), pt2: (f64, f64)) -> Edge {
        Edge {
            pt1: Point { x: pt1.0, y: pt1.1 },
            pt2: Point { x: pt2.0, y: pt2.1 },
        }
    }
}

struct Polygon {
    edges: Vec<Edge>, // Polygon has to be created with counter-clockwise coordinates
}

fn pt_in_polygon(pt: &Point, poly: &Polygon) -> bool {
    let count = poly.edges
        .iter()
        .filter(|edge| ray_intersect_seg(pt, edge))
        .count();

    count % 2 == 1
}

fn ray_intersect_seg(p: &Point, edge: &Edge) -> bool {
    let mut pt = p.clone();
    let (mut a, mut b): (&Point, &Point) = (&edge.pt1, &edge.pt2);
    if a.y > b.y {
        std::mem::swap(&mut a, &mut b);
    }
    if pt.y == a.y || pt.y == b.y {
        pt.y += _EPS;
    }

    if (pt.y > b.y || pt.y < a.y) || pt.x > a.x.max(b.x) {
        false
    } else if pt.x < a.x.min(b.x) {
        true
    } else {
        let m_red = if (a.x - b.x).abs() > _MIN {
            (b.y - a.y) / (b.x - a.x)
        } else {
            _MAX
        };
        let m_blue = if (a.x - pt.x).abs() > _MIN {
            (pt.y - a.y) / (pt.x - a.x)
        } else {
            _MAX
        };
        m_blue >= m_red
    }
}

fn main() {
    let p = |x, y| Point { x, y };
    let testpoints = [p(5.0, 5.0), p(5.0, 8.0), p(-10.0, 5.0), p(0.0, 5.0), p(10.0, 5.0), p(8.0, 5.0), p(10.0, 10.0)];
    let poly_square = Polygon {
        edges: vec![
            Edge::new((0.0, 0.0), (10.0, 0.0)),
            Edge::new((10.0, 0.0), (10.0, 10.0)),
            Edge::new((10.0, 10.0), (0.0, 10.0)),
            Edge::new((0.0, 10.0), (0.0, 0.0)),
        ],
    };
    let poly_square_hole = Polygon {
        edges: vec![
            Edge::new((0.0, 0.0), (10.0, 0.0)),
            Edge::new((10.0, 0.0), (10.0, 10.0)),
            Edge::new((10.0, 10.0), (0.0, 10.0)),
            Edge::new((0.0, 10.0), (0.0, 0.0)),
            Edge::new((2.5, 2.5), (7.5, 2.5)),
            Edge::new((7.5, 2.5), (7.5, 7.5)),
            Edge::new((7.5, 7.5), (2.5, 7.5)),
            Edge::new((2.5, 7.5), (2.5, 2.5)),
        ],
    };
    let poly_strange = Polygon {
        edges: vec![
            Edge::new((0.0, 0.0), (2.5, 2.5)),
            Edge::new((2.5, 2.5), (0.0, 10.0)),
            Edge::new((0.0, 10.0), (2.5, 7.5)),
            Edge::new((2.5, 7.5), (7.5, 7.5)),
            Edge::new((7.5, 7.5), (10.0, 10.0)),
            Edge::new((10.0, 10.0), (10.0, 0.0)),
            Edge::new((10.0, 0.0), (2.5, 2.5)),
        ],
    };
    let poly_hexagon = Polygon {
        edges: vec![
            Edge::new((3.0, 0.0), (7.0, 0.0)),
            Edge::new((7.0, 0.0), (10.0, 5.0)),
            Edge::new((10.0, 5.0), (7.0, 10.0)),
            Edge::new((7.0, 10.0), (3.0, 10.0)),
            Edge::new((3.0, 10.0), (0.0, 5.0)),
            Edge::new((0.0, 5.0), (3.0, 0.0)),
        ],
    };
    print!("\nSquare :");
    for pt in &testpoints {
        print!(" {:?}", pt_in_polygon(pt, &poly_square));
    }
    print!("\nSquare with hole:");
    for pt in &testpoints {
        print!(" {:?}", pt_in_polygon(pt, &poly_square_hole));
    }
    print!("\nStrange polygon :");
    for pt in &testpoints {
        print!(" {:?}", pt_in_polygon(pt, &poly_strange));
    }
    print!("\nHexagon :");
    for pt in &testpoints {
        print!(" {:?}", pt_in_polygon(pt, &poly_hexagon));
    }
    println!();
}
```

{{out}}

```txt

Square : true true false false true true false
Square with hole: false true false false true true false
Strange polygon : true false false false true true false
Hexagon : true true false false true true false

```



## Scala

{{trans|D}}

```scala
package scala.ray_casting

case class Edge(_1: (Double, Double), _2: (Double, Double)) {
  import Math._
  import Double._

  def raySegI(p: (Double, Double)): Boolean = {
    if (_1._2 > _2._2) return Edge(_2, _1).raySegI(p)
    if (p._2 == _1._2 || p._2 == _2._2) return raySegI((p._1, p._2 + epsilon))
    if (p._2 > _2._2 || p._2 < _1._2 || p._1 > max(_1._1, _2._1))
      return false
    if (p._1 < min(_1._1, _2._1)) return true
    val blue = if (abs(_1._1 - p._1) > MinValue) (p._2 - _1._2) / (p._1 - _1._1) else MaxValue
    val red = if (abs(_1._1 - _2._1) > MinValue) (_2._2 - _1._2) / (_2._1 - _1._1) else MaxValue
    blue >= red
  }

  final val epsilon = 0.00001
}

case class Figure(name: String, edges: Seq[Edge]) {
  def contains(p: (Double, Double)) = edges.count(_.raySegI(p)) % 2 != 0
}

object Ray_casting extends App {
  val figures = Seq(Figure("Square", Seq(((0.0, 0.0), (10.0, 0.0)), ((10.0, 0.0), (10.0, 10.0)),
    ((10.0, 10.0), (0.0, 10.0)),((0.0, 10.0), (0.0, 0.0)))),
    Figure("Square hole", Seq(((0.0, 0.0), (10.0, 0.0)), ((10.0, 0.0), (10.0, 10.0)),
      ((10.0, 10.0), (0.0, 10.0)), ((0.0, 10.0), (0.0, 0.0)), ((2.5, 2.5), (7.5, 2.5)),
      ((7.5, 2.5), (7.5, 7.5)),((7.5, 7.5), (2.5, 7.5)), ((2.5, 7.5), (2.5, 2.5)))),
    Figure("Strange", Seq(((0.0, 0.0), (2.5, 2.5)), ((2.5, 2.5), (0.0, 10.0)),
      ((0.0, 10.0), (2.5, 7.5)), ((2.5, 7.5), (7.5, 7.5)), ((7.5, 7.5), (10.0, 10.0)),
      ((10.0, 10.0), (10.0, 0.0)), ((10.0, 0.0), (2.5, 2.5)))),
    Figure("Exagon", Seq(((3.0, 0.0), (7.0, 0.0)), ((7.0, 0.0), (10.0, 5.0)), ((10.0, 5.0), (7.0, 10.0)),
      ((7.0, 10.0), (3.0, 10.0)), ((3.0, 10.0), (0.0, 5.0)), ((0.0, 5.0), (3.0, 0.0)))))

  val points = Seq((5.0, 5.0), (5.0, 8.0), (-10.0, 5.0), (0.0, 5.0), (10.0, 5.0), (8.0, 5.0), (10.0, 10.0))

  println("points: " + points)
  for (f <- figures) {
    println("figure: " + f.name)
    println("        " + f.edges)
    println("result: " + (points map f.contains))
  }

  private implicit def to_edge(p: ((Double, Double), (Double, Double))): Edge = Edge(p._1, p._2)
}
```

{{out}}

```txt
points: List((5.0,5.0), (5.0,8.0), (-10.0,5.0), (0.0,5.0), (10.0,5.0), (8.0,5.0), (10.0,10.0))
figure: Square
        List(Edge((0.0,0.0),(10.0,0.0)), Edge((10.0,0.0),(10.0,10.0)), Edge((10.0,10.0),(0.0,10.0)), Edge((0.0,10.0),(0.0,0.0)))
result: List(true, true, false, false, true, true, false)
figure: Square hole
        List(Edge((0.0,0.0),(10.0,0.0)), Edge((10.0,0.0),(10.0,10.0)), Edge((10.0,10.0),(0.0,10.0)), Edge((0.0,10.0),(0.0,0.0)), Edge((2.5,2.5),(7.5,2.5)), Edge((7.5,2.5),(7.5,7.5)), Edge((7.5,7.5),(2.5,7.5)), Edge((2.5,7.5),(2.5,2.5)))
result: List(false, true, false, false, true, true, false)
figure: Strange
        List(Edge((0.0,0.0),(2.5,2.5)), Edge((2.5,2.5),(0.0,10.0)), Edge((0.0,10.0),(2.5,7.5)), Edge((2.5,7.5),(7.5,7.5)), Edge((7.5,7.5),(10.0,10.0)), Edge((10.0,10.0),(10.0,0.0)), Edge((10.0,0.0),(2.5,2.5)))
result: List(true, false, false, false, true, true, false)
figure: Exagon
        List(Edge((3.0,0.0),(7.0,0.0)), Edge((7.0,0.0),(10.0,5.0)), Edge((10.0,5.0),(7.0,10.0)), Edge((7.0,10.0),(3.0,10.0)), Edge((3.0,10.0),(0.0,5.0)), Edge((0.0,5.0),(3.0,0.0)))
result: List(true, true, false, false, true, true, false)

```



## Smalltalk

{{works with|GNU Smalltalk}}
The class Segment holds the code to test if a ray starting from a point (and going towards "right") intersects the segment (method <tt>doesIntersectRayFrom</tt>); while the class Polygon hosts the code to test if a point is inside the polygon (method <tt>pointInside</tt>).

```smalltalk
Object subclass: Segment [
    |pts|
    Segment class >> new: points [ |a|
      a := super new.
      ^ a init: points
    ]
    init: points [ pts := points copy. ^self ]
    endPoints [ ^pts ]
    "utility methods"
    first [ ^ pts at: 1]
    second [ ^ pts at: 2]
    leftmostEndPoint [ 
      ^ (self first x > self second x) ifTrue: [ self second ] ifFalse: [ self first ]
    ]
    rightmostEndPoint [
      ^ (self first x > self second x) ifTrue: [ self first ] ifFalse: [ self second ] 
    ]
    topmostEndPoint [
      ^ (self first y > self second y) ifTrue: [ self first ] ifFalse: [ self second ]
    ]
    bottommostEndPoint [
      ^ (self first y > self second y) ifTrue: [ self second ] ifFalse: [ self first ]
    ]

    slope [
      (pts at: 1) x ~= (pts at: 2) x
      ifTrue: [ ^ ((pts at: 1) y - (pts at: 2) y) / ((pts at: 1) x - (pts at: 2) x) ]
      ifFalse: [ ^ FloatD infinity ]
    ]

    doesIntersectRayFrom: point [ |p A B|
      (point y = (pts at: 1) y) | (point y = (pts at: 2) y)
      ifTrue: [ p := Point x: (point x) y: (point y) + 0.00001 ]
      ifFalse: [ p := point copy ].
      A := self bottommostEndPoint.
      B := self topmostEndPoint.
      (p y < A y) | (p y > B y) | (p x > (self rightmostEndPoint x))
        ifTrue: [ ^false ]
        ifFalse: [ (p x < (self leftmostEndPoint x))
                     ifTrue: [ ^true ]
                     ifFalse: [ |s| 
                        s := Segment new: { A . point }.
			(s slope) >= (self slope)
			  ifTrue: [ ^ true ]
                     ]
                 ].
        ^false
    ]
].

Object subclass: Polygon [
    |polysegs|
    Polygon class >> new [ |a| a := super new. ^ a init. ]
    Polygon class >> fromSegments: segments [ |a|
      a := super new.
      ^ a initWithSegments: segments
    ]
    Polygon class >> fromPoints: pts and: indexes [ |a|
      a := self new.
      indexes do: [ :i |
        a addSegment: ( Segment new: { pts at: (i at: 1) . pts at: (i at: 2) } )
      ].
      ^ a
    ]
    initWithSegments: segments [
      polysegs := segments copy. ^self
    ]
    init [ polysegs := OrderedCollection new. ^ self ]
    addSegment: segment [ polysegs add: segment ]
    
    pointInside: point [ |cnt|
      cnt := 0.
      polysegs do: [ :s | (s doesIntersectRayFrom: point)
                          ifTrue: [ cnt := cnt + 1 ] ].
      ^ ( cnt \\ 2 = 0 ) not
    ]
].
```


'''Testing'''

```smalltalk
|points names polys|

points := {
           0@0 . 10@0 . 10@10 . 0@10 .
           2.5@2.5 . 7.5@2.5 . 7.5@7.5 .
           2.5@7.5 . 0@5 . 10@5 .
           3@0 . 7@0 . 7@10 . 3@10
          }.

names := { 'square' . 'square hole' . 'strange' . 'exagon' }.

polys := OrderedCollection new.

polys add:
      ( 
        Polygon fromPoints: points 
                and: { {1 . 2}. {2 . 3}. {3 . 4}. {4 . 1} }
      ) ;
      add:
      (
        Polygon fromPoints: points 
                and: { {1 . 2}. {2 . 3}. {3 . 4}. {4 . 1}. {5 . 6}. {6 . 7}. {7 . 8}. {8 . 5} }
      ) ;
      add:
      (
        Polygon fromPoints: points 
                and: { {1 . 5}. {5 . 4}. {4 . 8}. {8 . 7}. {7 . 3}. {3 . 2}. {2 . 5} }
      ) ;
      add:
      (
        Polygon fromPoints: points 
                and: { {11 . 12}. {12 . 10}. {10 . 13}. {13 . 14}. {14 . 9}. {9 . 11} }
      ).

{ 5@5 . 5@8 . -10@5 . 0@5 . 10@5 . 8@5 . 10@10 } 
do: [ :p |
  1 to: 4 do: [ :i |
   ('point %1 inside %2? %3' %
     { p . names at: i. (polys at: i) pointInside: p }) displayNl
  ].
  ' ' displayNl.
]
```



## Tcl


```Tcl
package require Tcl 8.5
 
proc point_in_polygon {point polygon} {
    set count 0
    foreach side [sides $polygon] {
        if {[ray_intersects_line $point $side]} {
            incr count
        }
    }
    expr {$count % 2} ;#-- 1 = odd = true, 0 = even = false
}
proc sides polygon {
    lassign $polygon x0 y0
    foreach {x y} [lrange [lappend polygon $x0 $y0] 2 end] {
        lappend res [list $x0 $y0 $x $y]
        set x0 $x
        set y0 $y
    }
    return $res
}
proc ray_intersects_line {point line} {
    lassign $point Px Py
    lassign $line Ax Ay Bx By
    # Reverse line direction if necessary
    if {$By < $Ay} {
	lassign $line Bx By Ax Ay
    }
    # Add epsilon to 
    if {$Py == $Ay || $Py == $By} {
	set Py [expr {$Py + abs($Py)/1e6}]
    }
    # Bounding box checks
    if {$Py < $Ay || $Py > $By || $Px > max($Ax,$Bx)} {
	return 0
    } elseif {$Px < min($Ax,$Bx)} {
	return 1
    }
    # Compare dot products to compare (cosines of) angles
    set mRed [expr {$Ax != $Bx ? ($By-$Ay)/($Bx-$Ax) : Inf}]
    set mBlu [expr {$Ax != $Px ? ($Py-$Ay)/($Px-$Ax) : Inf}]
    return [expr {$mBlu >= $mRed}]
}
 
foreach {point poly} {
    {0 0}	{-1 -1  -1 1  1 1  1 -1}
    {2 2}	{-1 -1  -1 1  1 1  1 -1}
    {0 0}	{-2 -2  -2 2  2 2  2 -2   2 -1  1 1  -1 1  -1 -1  1 -1  2 -1}
    {1.5 1.5}	{-2 -2  -2 2  2 2  2 -2   2 -1  1 1  -1 1  -1 -1  1 -1  2 -1}
    {5 5}	{0 0  2.5 2.5  0 10  2.5 7.5  7.5 7.5  10 10  10 0  7.5 0.1}
    {5 8}	{0 0  2.5 2.5  0 10  2.5 7.5  7.5 7.5  10 10  10 0  7.5 0.1}
    {2 2}	{0 0  2.5 2.5  0 10  2.5 7.5  7.5 7.5  10 10  10 0  7.5 0.1}
    {0 0}	{0 0  2.5 2.5  0 10  2.5 7.5  7.5 7.5  10 10  10 0  7.5 0.1}
    {10 10}	{0 0  2.5 2.5  0 10  2.5 7.5  7.5 7.5  10 10  10 0  7.5 0.1}
    {2.5 2.5}	{0 0  2.5 2.5  0 10  2.5 7.5  7.5 7.5  10 10  10 0  7.5 0.1}
    {-5 5}	{3 0  7 0  10 5  7 10  3 10  0 5}
} {
    puts "$point in $poly = [point_in_polygon $point $poly]"
}
```



## Ursala

This function takes a point <math>(x,y)</math> and a polygon <math>\langle(x_1,y_1)\dots(x_n,y_n)\rangle</math>
to a true value if the point is enclosed by the polygon and a false
value if it's outside, using the algorithm described above.
For points on the boundary the result is unspecified. 

```Ursala
#import flo

in =

@lrzyCipPX ~|afatPRZaq ~&EZ+fleq~~lrPrbr2G&& ~&B+fleq~~lrPrbl2G!| -&
   ~&Y+ ~~lrPrbl2G fleq,
   ^E(fleq@lrrPX,@rl fleq\0.)^/~&lr ^(~&r,times)^/minus@llPrll2X vid+ minus~~rbbI&-
```

This test program tries it on a couple of examples.

```Ursala
#cast %bL

examples = 

in* <
   ((0.5,0.6),<(0.,0.),(1.,2.),(1.,0.)>),
   ((0.5,0.6),<(0.,0.),(1.,1.),(1.,0.)>)>
```

output:

```txt
<true,false>
```



## Visual Basic .NET

{{trans|Java}}

```vbnet
Imports System.Math

Module RayCasting

    Private square As Integer()() = {New Integer() {0, 0}, New Integer() {20, 0}, New Integer() {20, 20}, New Integer() {0, 20}}
    Private squareHole As Integer()() = {New Integer() {0, 0}, New Integer() {20, 0}, New Integer() {20, 20}, New Integer() {0, 20}, New Integer() {5, 5}, New Integer() {15, 5}, New Integer() {15, 15}, New Integer() {5, 15}}
    Private strange As Integer()() = {New Integer() {0, 0}, New Integer() {5, 5}, New Integer() {0, 20}, New Integer() {5, 15}, New Integer() {15, 15}, New Integer() {20, 20}, New Integer() {20, 0}}
    Private hexagon As Integer()() = {New Integer() {6, 0}, New Integer() {14, 0}, New Integer() {20, 10}, New Integer() {14, 20}, New Integer() {6, 20}, New Integer() {0, 10}}
    Private shapes As Integer()()() = {square, squareHole, strange, hexagon}

    Public Sub Main()
        Dim testPoints As Double()() = {New Double() {10, 10}, New Double() {10, 16}, New Double() {-20, 10}, New Double() {0, 10}, New Double() {20, 10}, New Double() {16, 10}, New Double() {20, 20}}

        For Each shape As Integer()() In shapes
            For Each point As Double() In testPoints
                Console.Write(String.Format("{0} ", Contains(shape, point).ToString.PadLeft(7)))
            Next
            Console.WriteLine()
        Next
    End Sub

    Private Function Contains(shape As Integer()(), point As Double()) As Boolean

        Dim inside As Boolean = False
        Dim length As Integer = shape.Length

        For i As Integer = 0 To length - 1
            If Intersects(shape(i), shape((i + 1) Mod length), point) Then
                inside = Not inside
            End If
        Next

        Return inside
    End Function

    Private Function Intersects(a As Integer(), b As Integer(), p As Double()) As Boolean

        If a(1) > b(1) Then Return Intersects(b, a, p)
        If p(1) = a(1) Or p(1) = b(1) Then p(1) += 0.0001
        If p(1) > b(1) Or p(1) < a(1) Or p(0) > Max(a(0), b(0)) Then Return False
        If p(0) < Min(a(0), b(0)) Then Return True
        Dim red As Double = (p(1) - a(1)) / (p(0) - a(0))
        Dim blue As Double = (b(1) - a(1)) / (b(0) - a(0))

        Return red >= blue
    End Function
End Module
```

{{out}}

```txt

   True    True   False   False    True    True   False
  False    True   False   False    True    True   False
   True   False   False   False    True    True   False
   True    True   False   False    True    True   False

```



## zkl

{{trans|Perl 6}}

```zkl
const E = 0.0001;
 
fcn rayHitsSeg([(Px,Py)],[(Ax,Ay)],[(Bx,By)]){ // --> Bool
   if(Py==Ay or Py==By) Py+=E;
   if(Py<Ay or Py>By or Px>Ax.max(Bx)) False
   else if(Px<Ax.min(Bx))              True
   else try{ ( (Py - Ay)/(Px - Ax) )>=( (By - Ay)/(Bx - Ax) ) } //blue>=red
        catch(MathError){ Px==Ax } // divide by zero == ∞, only blue?
}
 
fcn pointInPoly(point, polygon){ // --> Bool, polygon is ( (a,b),(c,d).. )
   polygon.filter('wrap([(ab,cd)]){ a,b:=ab; c,d:=cd;
      if(b<=d) rayHitsSeg(point,ab,cd); // left point has smallest y coordinate
      else     rayHitsSeg(point,cd,ab);
   })
   .len().isOdd; // True if crossed an odd number of borders ie inside polygon
}
```


```zkl
polys:=T( //(name,( ((a,b),(c,d)),((a,b),(c,d))... ), ... )==(nm,(ln,ln..) ..)
    T("squared",
	T(T(T( 0.0,  0.0), T(10.0,  0.0)),
	  T(T(10.0,  0.0), T(10.0, 10.0)),
	  T(T(10.0, 10.0), T( 0.0, 10.0)),
	  T(T( 0.0, 10.0), T( 0.0,  0.0)))),
    T("squaredhole",
	T(T(T( 0.0,  0.0), T(10.0,  0.0)),
	  T(T(10.0,  0.0), T(10.0, 10.0)),
	  T(T(10.0, 10.0), T( 0.0, 10.0)),
	  T(T( 0.0, 10.0), T( 0.0,  0.0)),
	  T(T( 2.5,  2.5), T( 7.5,  2.5)),
	  T(T( 7.5,  2.5), T( 7.5,  7.5)),
	  T(T( 7.5,  7.5), T( 2.5,  7.5)),
	  T(T( 2.5,  7.5), T( 2.5,  2.5)))),
    T("strange",
	T(T(T( 0.0,  0.0), T( 2.5,  2.5)),
	  T(T( 2.5,  2.5), T( 0.0, 10.0)),
	  T(T( 0.0, 10.0), T( 2.5,  7.5)),
	  T(T( 2.5,  7.5), T( 7.5,  7.5)),
	  T(T( 7.5,  7.5), T(10.0, 10.0)),
	  T(T(10.0, 10.0), T(10.0,  0.0)),
	  T(T(10.0,  0.0), T( 2.5,  2.5)),
	  T(T( 2.5,  2.5), T( 0.0,  0.0)))),  # conjecturally close polygon
    T("exagon",
	T(T(T( 3.0,  0.0), T( 7.0,  0.0)),
	  T(T( 7.0,  0.0), T(10.0,  5.0)),
	  T(T(10.0,  5.0), T( 7.0, 10.0)),
	  T(T( 7.0, 10.0), T( 3.0, 10.0)),
	  T(T( 3.0, 10.0), T( 0.0,  5.0)),
	  T(T( 0.0,  5.0), T( 3.0,  0.0)))),
);
 
testPoints:=T(
	  T(  5.0,  5.0),
	  T(  5.0,  8.0),
	  T(-10.0,  5.0),
	  T(  0.0,  5.0),
	  T( 10.0,  5.0),
	  T(  8.0,  5.0),
	  T( 10.0, 10.0)
);
 
foreach name,polywanna in (polys){
   name.println();
   foreach testPoint in (testPoints){
      println("\t(%6.1f,%6.1f)\t".fmt(testPoint.xplode()),
         pointInPoly(testPoint,polywanna) and "IN" or "OUT");
   }
}
```

{{out}}

```txt

squared
	(   5.0,   5.0)	IN
	(   5.0,   8.0)	IN
	( -10.0,   5.0)	OUT
	(   0.0,   5.0)	OUT
	(  10.0,   5.0)	IN
	(   8.0,   5.0)	IN
	(  10.0,  10.0)	OUT
squaredhole
	(   5.0,   5.0)	OUT
	(   5.0,   8.0)	IN
	( -10.0,   5.0)	OUT
	(   0.0,   5.0)	OUT
	(  10.0,   5.0)	IN
	(   8.0,   5.0)	IN
	(  10.0,  10.0)	OUT
strange
	(   5.0,   5.0)	IN
	(   5.0,   8.0)	OUT
	( -10.0,   5.0)	OUT
	(   0.0,   5.0)	OUT
	(  10.0,   5.0)	IN
	(   8.0,   5.0)	IN
	(  10.0,  10.0)	OUT
exagon
	(   5.0,   5.0)	IN
	(   5.0,   8.0)	IN
	( -10.0,   5.0)	OUT
	(   0.0,   5.0)	OUT
	(  10.0,   5.0)	IN
	(   8.0,   5.0)	IN
	(  10.0,  10.0)	OUT

```


{{omit from|Lilypond}}

[[Category:Geometry]]
