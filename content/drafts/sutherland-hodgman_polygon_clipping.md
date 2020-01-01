+++
title = "Sutherland-Hodgman polygon clipping"
description = ""
date = 2019-09-07T13:54:52Z
aliases = []
[extra]
id = 6634
[taxonomies]
categories = []
tags = []
+++

{{task|Graphics algorithms}}

The   [[wp:Sutherland-Hodgman clipping algorithm|Sutherland-Hodgman clipping algorithm]]   finds the polygon that is the intersection between an arbitrary polygon (the “subject polygon”) and a convex polygon (the “clip polygon”).

It is used in computer graphics (especially 2D graphics) to reduce the complexity of a scene being displayed by eliminating parts of a polygon that do not need to be displayed.


;Task:
Take the closed polygon defined by the points:
: <big><math>[(50, 150), (200, 50), (350, 150), (350, 300), (250, 300), (200, 250), (150, 350), (100, 250), (100, 200)]</math></big>
and clip it by the rectangle defined by the points:
: <big><math>[(100, 100), (300, 100), (300, 300), (100, 300)]</math></big>

Print the sequence of points that define the resulting clipped polygon.


;Extra credit:
Display all three polygons on a graphical surface, using a different color for each polygon and filling the resulting polygon.

(When displaying you may use either a north-west or a south-west origin, whichever is more convenient for your display mechanism.)





## Ada


```Ada
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Text_IO;

procedure Main is
   package FIO is new Ada.Text_IO.Float_IO (Float);

   type Point is record
      X, Y : Float;
   end record;

   function "-" (Left, Right : Point) return Point is
   begin
      return (Left.X - Right.X, Left.Y - Right.Y);
   end "-";

   type Edge is array (1 .. 2) of Point;

   package Point_Lists is new Ada.Containers.Doubly_Linked_Lists
     (Element_Type => Point);
   use type Point_Lists.List;
   subtype Polygon is Point_Lists.List;

   function Inside (P : Point; E : Edge) return Boolean is
   begin
      return (E (2).X - E (1).X) * (P.Y - E (1).Y) >
             (E (2).Y - E (1).Y) * (P.X - E (1).X);
   end Inside;

   function Intersecton (P1, P2 : Point; E : Edge) return Point is
      DE : Point := E (1) - E (2);
      DP : Point := P1 - P2;
      N1 : Float := E (1).X * E (2).Y - E (1).Y * E (2).X;
      N2 : Float := P1.X * P2.Y - P1.Y * P2.X;
      N3 : Float := 1.0 / (DE.X * DP.Y - DE.Y * DP.X);
   begin
      return ((N1 * DP.X - N2 * DE.X) * N3, (N1 * DP.Y - N2 * DE.Y) * N3);
   end Intersecton;

   function Clip (P, C : Polygon) return Polygon is
      use Point_Lists;
      A, B, S, E : Cursor;
      Inputlist  : List;
      Outputlist : List := P;
      AB         : Edge;
   begin
      A := C.First;
      B := C.Last;
      while A /= No_Element loop
         AB        := (Element (B), Element (A));
         Inputlist := Outputlist;
         Outputlist.Clear;
         S := Inputlist.Last;
         E := Inputlist.First;
         while E /= No_Element loop
            if Inside (Element (E), AB) then
               if not Inside (Element (S), AB) then
                  Outputlist.Append
                    (Intersecton (Element (S), Element (E), AB));
               end if;
               Outputlist.Append (Element (E));
            elsif Inside (Element (S), AB) then
               Outputlist.Append
                 (Intersecton (Element (S), Element (E), AB));
            end if;
            S := E;
            E := Next (E);
         end loop;
         B := A;
         A := Next (A);
      end loop;
      return Outputlist;
   end Clip;

   procedure Print (P : Polygon) is
      use Point_Lists;
      C : Cursor := P.First;
   begin
      Ada.Text_IO.Put_Line ("{");
      while C /= No_Element loop
         Ada.Text_IO.Put (" (");
         FIO.Put (Element (C).X, Exp => 0);
         Ada.Text_IO.Put (',');
         FIO.Put (Element (C).Y, Exp => 0);
         Ada.Text_IO.Put (')');
         C := Next (C);
         if C /= No_Element then
            Ada.Text_IO.Put (',');
         end if;
         Ada.Text_IO.New_Line;
      end loop;
      Ada.Text_IO.Put_Line ("}");
   end Print;

   Source  : Polygon;
   Clipper : Polygon;
   Result  : Polygon;
begin
   Source.Append ((50.0, 150.0));
   Source.Append ((200.0, 50.0));
   Source.Append ((350.0, 150.0));
   Source.Append ((350.0, 300.0));
   Source.Append ((250.0, 300.0));
   Source.Append ((200.0, 250.0));
   Source.Append ((150.0, 350.0));
   Source.Append ((100.0, 250.0));
   Source.Append ((100.0, 200.0));
   Clipper.Append ((100.0, 100.0));
   Clipper.Append ((300.0, 100.0));
   Clipper.Append ((300.0, 300.0));
   Clipper.Append ((100.0, 300.0));
   Result := Clip (Source, Clipper);
   Print (Result);
end Main;
```

{{out}}

```txt
{
 (100.00000,116.66667),
 (125.00000,100.00000),
 (275.00000,100.00000),
 (300.00000,116.66667),
 (300.00000,300.00000),
 (250.00000,300.00000),
 (200.00000,250.00000),
 (175.00000,300.00000),
 (125.00000,300.00000),
 (100.00000,250.00000)
}
```



## BBC BASIC

{{works with|BBC BASIC for Windows}}

```bbcbasic
      VDU 23,22,200;200;8,16,16,128
      VDU 23,23,2;0;0;0;

      DIM SubjPoly{(8) x, y}
      DIM ClipPoly{(3) x, y}
      FOR v% = 0 TO 8 : READ SubjPoly{(v%)}.x, SubjPoly{(v%)}.y : NEXT
      DATA 50,150,200,50,350,150,350,300,250,300,200,250,150,350,100,250,100,200
      FOR v% = 0 TO 3 : READ ClipPoly{(v%)}.x, ClipPoly{(v%)}.y : NEXT
      DATA 100,100, 300,100, 300,300, 100,300

      GCOL 4 : PROCplotpoly(SubjPoly{()}, 9)
      GCOL 1 : PROCplotpoly(ClipPoly{()}, 4)
      nvert% = FNsutherland_hodgman(SubjPoly{()}, ClipPoly{()}, Clipped{()})
      GCOL 2 : PROCplotpoly(Clipped{()}, nvert%)
      END

      DEF FNsutherland_hodgman(subj{()}, clip{()}, RETURN out{()})
      LOCAL i%, j%, n%, o%, p1{}, p2{}, s{}, e{}, p{}, inp{()}
      DIM p1{x,y}, p2{x,y}, s{x,y}, e{x,y}, p{x,y}
      n% = DIM(subj{()},1) + DIM(clip{()},1)
      DIM inp{(n%) x, y}, out{(n%) x,y}
      FOR o% = 0 TO DIM(subj{()},1) : out{(o%)} = subj{(o%)} : NEXT
      p1{} = clip{(DIM(clip{()},1))}
      FOR i% = 0 TO DIM(clip{()},1)
        p2{} = clip{(i%)}
        FOR n% = 0 TO o% - 1 : inp{(n%)} = out{(n%)} : NEXT : o% = 0
        IF n% >= 2 THEN
          s{} = inp{(n% - 1)}
          FOR j% = 0 TO n% - 1
            e{} = inp{(j%)}
            IF FNside(e{}, p1{}, p2{}) THEN
              IF NOT FNside(s{}, p1{}, p2{}) THEN
                PROCintersection(p1{}, p2{}, s{}, e{}, p{})
                out{(o%)} = p{}
                o% += 1
              ENDIF
              out{(o%)} = e{}
              o% += 1
            ELSE
              IF FNside(s{}, p1{}, p2{}) THEN
                PROCintersection(p1{}, p2{}, s{}, e{}, p{})
                out{(o%)} = p{}
                o% += 1
              ENDIF
            ENDIF
            s{} = e{}
          NEXT
        ENDIF
        p1{} = p2{}
      NEXT i%
      = o%

      REM Which side of the line p1-p2 is the point p?
      DEF FNside(p{}, p1{}, p2{})
      =  (p2.x - p1.x) * (p.y - p1.y) > (p2.y - p1.y) * (p.x - p1.x)

      REM Find the intersection of two lines p1-p2 and p3-p4
      DEF PROCintersection(p1{}, p2{}, p3{}, p4{}, p{})
      LOCAL a{}, b{}, k, l, m : DIM a{x,y}, b{x,y}
      a.x = p1.x - p2.x : a.y = p1.y - p2.y
      b.x = p3.x - p4.x : b.y = p3.y - p4.y
      k = p1.x * p2.y - p1.y * p2.x
      l = p3.x * p4.y - p3.y * p4.x
      m = 1 / (a.x * b.y - a.y * b.x)
      p.x =  m * (k * b.x - l * a.x)
      p.y =  m * (k * b.y - l * a.y)
      ENDPROC

      REM plot a polygon
      DEF PROCplotpoly(poly{()}, n%)
      LOCAL i%
      MOVE poly{(0)}.x, poly{(0)}.y
      FOR i% = 1 TO n%-1
        DRAW poly{(i%)}.x, poly{(i%)}.y
      NEXT
      DRAW poly{(0)}.x, poly{(0)}.y
      ENDPROC
```

[[Image:suthhodg_bbc.gif]]


## C

Most of the code is actually storage util routines, such is C.  Prints out nodes, and writes test.eps file in current dir.

```c
#include <stdio.h>
#include <stdlib.h>
#include <math.h>

typedef struct { double x, y; } vec_t, *vec;

inline double dot(vec a, vec b)
{
	return a->x * b->x + a->y * b->y;
}

inline double cross(vec a, vec b)
{
	return a->x * b->y - a->y * b->x;
}

inline vec vsub(vec a, vec b, vec res)
{
	res->x = a->x - b->x;
	res->y = a->y - b->y;
	return res;
}

/* tells if vec c lies on the left side of directed edge a->b
 * 1 if left, -1 if right, 0 if colinear
 */
int left_of(vec a, vec b, vec c)
{
	vec_t tmp1, tmp2;
	double x;
	vsub(b, a, &tmp1);
	vsub(c, b, &tmp2);
	x = cross(&tmp1, &tmp2);
	return x < 0 ? -1 : x > 0;
}

int line_sect(vec x0, vec x1, vec y0, vec y1, vec res)
{
	vec_t dx, dy, d;
	vsub(x1, x0, &dx);
	vsub(y1, y0, &dy);
	vsub(x0, y0, &d);
	/* x0 + a dx = y0 + b dy ->
	   x0 X dx = y0 X dx + b dy X dx ->
	   b = (x0 - y0) X dx / (dy X dx) */
	double dyx = cross(&dy, &dx);
	if (!dyx) return 0;
	dyx = cross(&d, &dx) / dyx;
	if (dyx <= 0 || dyx >= 1) return 0;

	res->x = y0->x + dyx * dy.x;
	res->y = y0->y + dyx * dy.y;
	return 1;
}

/*
###  polygon stuff
 */
typedef struct { int len, alloc; vec v; } poly_t, *poly;

poly poly_new()
{
	return (poly)calloc(1, sizeof(poly_t));
}

void poly_free(poly p)
{
	free(p->v);
	free(p);
}

void poly_append(poly p, vec v)
{
	if (p->len >= p->alloc) {
		p->alloc *= 2;
		if (!p->alloc) p->alloc = 4;
		p->v = (vec)realloc(p->v, sizeof(vec_t) * p->alloc);
	}
	p->v[p->len++] = *v;
}

/* this works only if all of the following are true:
 *   1. poly has no colinear edges;
 *   2. poly has no duplicate vertices;
 *   3. poly has at least three vertices;
 *   4. poly is convex (implying 3).
*/
int poly_winding(poly p)
{
	return left_of(p->v, p->v + 1, p->v + 2);
}

void poly_edge_clip(poly sub, vec x0, vec x1, int left, poly res)
{
	int i, side0, side1;
	vec_t tmp;
	vec v0 = sub->v + sub->len - 1, v1;
	res->len = 0;

	side0 = left_of(x0, x1, v0);
	if (side0 != -left) poly_append(res, v0);

	for (i = 0; i < sub->len; i++) {
		v1 = sub->v + i;
		side1 = left_of(x0, x1, v1);
		if (side0 + side1 == 0 && side0)
			/* last point and current straddle the edge */
			if (line_sect(x0, x1, v0, v1, &tmp))
				poly_append(res, &tmp);
		if (i == sub->len - 1) break;
		if (side1 != -left) poly_append(res, v1);
		v0 = v1;
		side0 = side1;
	}
}

poly poly_clip(poly sub, poly clip)
{
	int i;
	poly p1 = poly_new(), p2 = poly_new(), tmp;

	int dir = poly_winding(clip);
	poly_edge_clip(sub, clip->v + clip->len - 1, clip->v, dir, p2);
	for (i = 0; i < clip->len - 1; i++) {
		tmp = p2; p2 = p1; p1 = tmp;
		if(p1->len == 0) {
			p2->len = 0;
			break;
		}
		poly_edge_clip(p1, clip->v + i, clip->v + i + 1, dir, p2);
	}

	poly_free(p1);
	return p2;
}

int main()
{
	int i;
	vec_t c[] = {{100,100}, {300,100}, {300,300}, {100,300}};
	//vec_t c[] = {{100,300}, {300,300}, {300,100}, {100,100}};
	vec_t s[] = {	{50,150}, {200,50}, {350,150},
			{350,300},{250,300},{200,250},
			{150,350},{100,250},{100,200}};
#define clen (sizeof(c)/sizeof(vec_t))
#define slen (sizeof(s)/sizeof(vec_t))
	poly_t clipper = {clen, 0, c};
	poly_t subject = {slen, 0, s};

	poly res = poly_clip(&subject, &clipper);

	for (i = 0; i < res->len; i++)
		printf("%g %g\n", res->v[i].x, res->v[i].y);

	/* long and arduous EPS printout */
	FILE * eps = fopen("test.eps", "w");
	fprintf(eps, "%%!PS-Adobe-3.0\n%%%%BoundingBox: 40 40 360 360\n"
		"/l {lineto} def /m{moveto} def /s{setrgbcolor} def"
		"/c {closepath} def /gs {fill grestore stroke} def\n");
	fprintf(eps, "0 setlinewidth %g %g m ", c[0].x, c[0].y);
	for (i = 1; i < clen; i++)
		fprintf(eps, "%g %g l ", c[i].x, c[i].y);
	fprintf(eps, "c .5 0 0 s gsave 1 .7 .7 s gs\n");

	fprintf(eps, "%g %g m ", s[0].x, s[0].y);
	for (i = 1; i < slen; i++)
		fprintf(eps, "%g %g l ", s[i].x, s[i].y);
	fprintf(eps, "c 0 .2 .5 s gsave .4 .7 1 s gs\n");

	fprintf(eps, "2 setlinewidth [10 8] 0 setdash %g %g m ",
		res->v[0].x, res->v[0].y);
	for (i = 1; i < res->len; i++)
		fprintf(eps, "%g %g l ", res->v[i].x, res->v[i].y);
	fprintf(eps, "c .5 0 .5 s gsave .7 .3 .8 s gs\n");

	fprintf(eps, "%%%%EOF");
	fclose(eps);
	printf("test.eps written\n");

	return 0;
}
```
{{out}}
```txt
200 250
175 300
125 300
100 250
100 200
100 116.667
125 100
275 100
300 116.667
300 300
250 300
test.eps written
```
[[file:poly-clip-C.png]]


## C++


```C++>#include <iostream


using namespace std;

struct point2D { float x, y; };

const int   N = 99; // clipped (new) polygon size

// check if a point is on the LEFT side of an edge
bool inside(point2D p, point2D p1, point2D p2)
{
    return (p2.y - p1.y) * p.x + (p1.x - p2.x) * p.y + (p2.x * p1.y - p1.x * p2.y) < 0;
}

// calculate intersection point
point2D intersection(point2D cp1, point2D cp2, point2D s, point2D e)
{
    point2D dc = { cp1.x - cp2.x, cp1.y - cp2.y };
    point2D dp = { s.x - e.x, s.y - e.y };

    float n1 = cp1.x * cp2.y - cp1.y * cp2.x;
    float n2 = s.x * e.y - s.y * e.x;
    float n3 = 1.0 / (dc.x * dp.y - dc.y * dp.x);

    return { (n1 * dp.x - n2 * dc.x) * n3, (n1 * dp.y - n2 * dc.y) * n3 };
}

// Sutherland-Hodgman clipping
void SutherlandHodgman(point2D *subjectPolygon, int &subjectPolygonSize, point2D *clipPolygon, int &clipPolygonSize, point2D (&newPolygon)[N], int &newPolygonSize)
{
    point2D cp1, cp2, s, e, inputPolygon[N];

    // copy subject polygon to new polygon and set its size
    for(int i = 0; i < subjectPolygonSize; i++)
        newPolygon[i] = subjectPolygon[i];

    newPolygonSize = subjectPolygonSize;

    for(int j = 0; j < clipPolygonSize; j++)
    {
        // copy new polygon to input polygon & set counter to 0
        for(int k = 0; k < newPolygonSize; k++){ inputPolygon[k] = newPolygon[k]; }
        int counter = 0;

        // get clipping polygon edge
        cp1 = clipPolygon[j];
        cp2 = clipPolygon[(j + 1) % clipPolygonSize];

        for(int i = 0; i < newPolygonSize; i++)
        {
            // get subject polygon edge
            s = inputPolygon[i];
            e = inputPolygon[(i + 1) % newPolygonSize];

            // Case 1: Both vertices are inside:
            // Only the second vertex is added to the output list
            if(inside(s, cp1, cp2) && inside(e, cp1, cp2))
                newPolygon[counter++] = e;

            // Case 2: First vertex is outside while second one is inside:
            // Both the point of intersection of the edge with the clip boundary
            // and the second vertex are added to the output list
            else if(!inside(s, cp1, cp2) && inside(e, cp1, cp2))
            {
                newPolygon[counter++] = intersection(cp1, cp2, s, e);
                newPolygon[counter++] = e;
            }

            // Case 3: First vertex is inside while second one is outside:
            // Only the point of intersection of the edge with the clip boundary
            // is added to the output list
            else if(inside(s, cp1, cp2) && !inside(e, cp1, cp2))
                newPolygon[counter++] = intersection(cp1, cp2, s, e);

            // Case 4: Both vertices are outside
            else if(!inside(s, cp1, cp2) && !inside(e, cp1, cp2))
            {
                // No vertices are added to the output list
            }
        }
        // set new polygon size
        newPolygonSize = counter;
    }
}

int main(int argc, char** argv)
{
    // subject polygon
    point2D subjectPolygon[] = {
	{50,150}, {200,50}, {350,150},
        {350,300},{250,300},{200,250},
        {150,350},{100,250},{100,200}
    };
    int subjectPolygonSize = sizeof(subjectPolygon) / sizeof(subjectPolygon[0]);

    // clipping polygon
    point2D clipPolygon[] = { {100,100}, {300,100}, {300,300}, {100,300} };
    int clipPolygonSize = sizeof(clipPolygon) / sizeof(clipPolygon[0]);

    // define the new clipped polygon (empty)
    int newPolygonSize = 0;
    point2D newPolygon[N] = { 0 };

    // apply clipping
    SutherlandHodgman(subjectPolygon, subjectPolygonSize, clipPolygon, clipPolygonSize, newPolygon, newPolygonSize);

    // print clipped polygon points
    cout << "Clipped polygon points:" << endl;
    for(int i = 0; i < newPolygonSize; i++)
        cout << "(" << newPolygon[i].x << ", " << newPolygon[i].y << ")" << endl;

    return 0;
}

```

{{out}}

```txt
Clipped polygon points:
(300, 300)
(250, 300)
(200, 250)
(175, 300)
(125, 300)
(100, 250)
(100, 116.667)
(125, 100)
(275, 100)
(300, 116.667)
```



## C#


This was written in .net 4.0 using wpf

Worker class:


```C sharp
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Windows;

namespace Sutherland
{
    public static class SutherlandHodgman
    {
        #region Class: Edge

        /// <summary>
        /// This represents a line segment
        /// </summary>
        private class Edge
        {
            public Edge(Point from, Point to)
            {
                this.From = from;
                this.To = to;
            }

            public readonly Point From;
            public readonly Point To;
        }

        #endregion

        /// <summary>
        /// This clips the subject polygon against the clip polygon (gets the intersection of the two polygons)
        /// </summary>
        /// <remarks>
        /// Based on the psuedocode from:
        /// http://en.wikipedia.org/wiki/Sutherland%E2%80%93Hodgman
        /// </remarks>
        /// <param name="subjectPoly">Can be concave or convex</param>
        /// <param name="clipPoly">Must be convex</param>
        /// <returns>The intersection of the two polygons (or null)</returns>
        public static Point[] GetIntersectedPolygon(Point[] subjectPoly, Point[] clipPoly)
        {
            if (subjectPoly.Length < 3 || clipPoly.Length < 3)
            {
                throw new ArgumentException(string.Format("The polygons passed in must have at least 3 points: subject={0}, clip={1}", subjectPoly.Length.ToString(), clipPoly.Length.ToString()));
            }

            List<Point> outputList = subjectPoly.ToList();

            //	Make sure it's clockwise
            if (!IsClockwise(subjectPoly))
            {
                outputList.Reverse();
            }

            //	Walk around the clip polygon clockwise
            foreach (Edge clipEdge in IterateEdgesClockwise(clipPoly))
            {
                List<Point> inputList = outputList.ToList();		//	clone it
                outputList.Clear();

                if (inputList.Count == 0)
                {
                    //	Sometimes when the polygons don't intersect, this list goes to zero.  Jump out to avoid an index out of range exception
                    break;
                }

                Point S = inputList[inputList.Count - 1];

                foreach (Point E in inputList)
                {
                    if (IsInside(clipEdge, E))
                    {
                        if (!IsInside(clipEdge, S))
                        {
                            Point? point = GetIntersect(S, E, clipEdge.From, clipEdge.To);
                            if (point == null)
                            {
                                throw new ApplicationException("Line segments don't intersect");		//	may be colinear, or may be a bug
                            }
                            else
                            {
                                outputList.Add(point.Value);
                            }
                        }

                        outputList.Add(E);
                    }
                    else if (IsInside(clipEdge, S))
                    {
                        Point? point = GetIntersect(S, E, clipEdge.From, clipEdge.To);
                        if (point == null)
                        {
                            throw new ApplicationException("Line segments don't intersect");		//	may be colinear, or may be a bug
                        }
                        else
                        {
                            outputList.Add(point.Value);
                        }
                    }

                    S = E;
                }
            }

            //	Exit Function
            return outputList.ToArray();
        }

        #region Private Methods

        /// <summary>
        /// This iterates through the edges of the polygon, always clockwise
        /// </summary>
        private static IEnumerable<Edge> IterateEdgesClockwise(Point[] polygon)
        {
            if (IsClockwise(polygon))
            {
                #region Already clockwise

                for (int cntr = 0; cntr < polygon.Length - 1; cntr++)
                {
                    yield return new Edge(polygon[cntr], polygon[cntr + 1]);
                }

                yield return new Edge(polygon[polygon.Length - 1], polygon[0]);

                #endregion
            }
            else
            {
                #region Reverse

                for (int cntr = polygon.Length - 1; cntr > 0; cntr--)
                {
                    yield return new Edge(polygon[cntr], polygon[cntr - 1]);
                }

                yield return new Edge(polygon[0], polygon[polygon.Length - 1]);

                #endregion
            }
        }

        /// <summary>
        /// Returns the intersection of the two lines (line segments are passed in, but they are treated like infinite lines)
        /// </summary>
        /// <remarks>
        /// Got this here:
        /// http://stackoverflow.com/questions/14480124/how-do-i-detect-triangle-and-rectangle-intersection
        /// </remarks>
        private static Point? GetIntersect(Point line1From, Point line1To, Point line2From, Point line2To)
        {
            Vector direction1 = line1To - line1From;
            Vector direction2 = line2To - line2From;
            double dotPerp = (direction1.X * direction2.Y) - (direction1.Y * direction2.X);

            // If it's 0, it means the lines are parallel so have infinite intersection points
            if (IsNearZero(dotPerp))
            {
                return null;
            }

            Vector c = line2From - line1From;
            double t = (c.X * direction2.Y - c.Y * direction2.X) / dotPerp;
            //if (t < 0 || t > 1)
            //{
            //    return null;		//	lies outside the line segment
            //}

            //double u = (c.X * direction1.Y - c.Y * direction1.X) / dotPerp;
            //if (u < 0 || u > 1)
            //{
            //    return null;		//	lies outside the line segment
            //}

            //	Return the intersection point
            return line1From + (t * direction1);
        }

        private static bool IsInside(Edge edge, Point test)
        {
            bool? isLeft = IsLeftOf(edge, test);
            if (isLeft == null)
            {
                //	Colinear points should be considered inside
                return true;
            }

            return !isLeft.Value;
        }
        private static bool IsClockwise(Point[] polygon)
        {
            for (int cntr = 2; cntr < polygon.Length; cntr++)
            {
                bool? isLeft = IsLeftOf(new Edge(polygon[0], polygon[1]), polygon[cntr]);
                if (isLeft != null)		//	some of the points may be colinear.  That's ok as long as the overall is a polygon
                {
                    return !isLeft.Value;
                }
            }

            throw new ArgumentException("All the points in the polygon are colinear");
        }

        /// <summary>
        /// Tells if the test point lies on the left side of the edge line
        /// </summary>
        private static bool? IsLeftOf(Edge edge, Point test)
        {
            Vector tmp1 = edge.To - edge.From;
            Vector tmp2 = test - edge.To;

            double x = (tmp1.X * tmp2.Y) - (tmp1.Y * tmp2.X);		//	dot product of perpendicular?

            if (x < 0)
            {
                return false;
            }
            else if (x > 0)
            {
                return true;
            }
            else
            {
                //	Colinear points;
                return null;
            }
        }

        private static bool IsNearZero(double testValue)
        {
            return Math.Abs(testValue) <= .000000001d;
        }

        #endregion
    }
}
```


Window code:


```html

<Window x:Class="Sutherland.MainWindow"
        xmlns="http://schemas.microsoft.com/winfx/2006/xaml/presentation"
        xmlns:x="http://schemas.microsoft.com/winfx/2006/xaml"
        Title="Sutherland Hodgman" Background="#B0B0B0" ResizeMode="CanResizeWithGrip" Width="525" Height="450">
    <Grid Margin="4">
        <Grid.RowDefinitions>
            <RowDefinition Height="1*"/>
            <RowDefinition Height="auto"/>
        </Grid.RowDefinitions>

        <Border Grid.Row="0" CornerRadius="4" BorderBrush="#707070" Background="#FFFFFF" BorderThickness="2">
            <Canvas Name="canvas"/>
        </Border>

        <UniformGrid Grid.Row="1" Rows="1" Margin="0,4,0,0">
            <Button Name="btnTriRect" Content="Triangle - Rectangle" Margin="4,0" Click="btnTriRect_Click"/>
            <Button Name="btnConvex" Content="Concave - Convex" Click="btnConvex_Click"/>
        </UniformGrid>
    </Grid>
</Window>

```



```C sharp
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Data;
using System.Windows.Documents;
using System.Windows.Input;
using System.Windows.Media;
using System.Windows.Media.Imaging;
using System.Windows.Navigation;
using System.Windows.Shapes;

namespace Sutherland
{
    public partial class MainWindow : Window
    {
        #region Declaration Section

        private Random _rand = new Random();

        private Brush _subjectBack = new SolidColorBrush(ColorFromHex("30427FCF"));
        private Brush _subjectBorder = new SolidColorBrush(ColorFromHex("427FCF"));
        private Brush _clipBack = new SolidColorBrush(ColorFromHex("30D65151"));
        private Brush _clipBorder = new SolidColorBrush(ColorFromHex("D65151"));
        private Brush _intersectBack = new SolidColorBrush(ColorFromHex("609F18CC"));
        private Brush _intersectBorder = new SolidColorBrush(ColorFromHex("9F18CC"));

        #endregion

        #region Constructor

        public MainWindow()
        {
            InitializeComponent();
        }

        #endregion

        #region Event Listeners

        private void btnTriRect_Click(object sender, RoutedEventArgs e)
        {
            try
            {
                double width = canvas.ActualWidth;
                double height = canvas.ActualHeight;

                Point[] poly1 = new Point[] {
				    new Point(_rand.NextDouble() * width, _rand.NextDouble() * height),
				    new Point(_rand.NextDouble() * width, _rand.NextDouble() * height),
				    new Point(_rand.NextDouble() * width, _rand.NextDouble() * height) };

                Point rectPoint = new Point(_rand.NextDouble() * (width * .75d), _rand.NextDouble() * (height * .75d));		//	don't let it start all the way at the bottom right
                Rect rect = new Rect(
                    rectPoint,
                    new Size(_rand.NextDouble() * (width - rectPoint.X), _rand.NextDouble() * (height - rectPoint.Y)));

                Point[] poly2 = new Point[] { rect.TopLeft, rect.TopRight, rect.BottomRight, rect.BottomLeft };

                Point[] intersect = SutherlandHodgman.GetIntersectedPolygon(poly1, poly2);

                canvas.Children.Clear();
                ShowPolygon(poly1, _subjectBack, _subjectBorder, 1d);
                ShowPolygon(poly2, _clipBack, _clipBorder, 1d);
                ShowPolygon(intersect, _intersectBack, _intersectBorder, 3d);
            }
            catch (Exception ex)
            {
                MessageBox.Show(ex.ToString(), this.Title, MessageBoxButton.OK, MessageBoxImage.Error);
            }
        }
        private void btnConvex_Click(object sender, RoutedEventArgs e)
        {
            try
            {
                Point[] poly1 = new Point[] { new Point(50, 150), new Point(200, 50), new Point(350, 150), new Point(350, 300), new Point(250, 300), new Point(200, 250), new Point(150, 350), new Point(100, 250), new Point(100, 200) };
                Point[] poly2 = new Point[] { new Point(100, 100), new Point(300, 100), new Point(300, 300), new Point(100, 300) };

                Point[] intersect = SutherlandHodgman.GetIntersectedPolygon(poly1, poly2);

                canvas.Children.Clear();
                ShowPolygon(poly1, _subjectBack, _subjectBorder, 1d);
                ShowPolygon(poly2, _clipBack, _clipBorder, 1d);
                ShowPolygon(intersect, _intersectBack, _intersectBorder, 3d);
            }
            catch (Exception ex)
            {
                MessageBox.Show(ex.ToString(), this.Title, MessageBoxButton.OK, MessageBoxImage.Error);
            }
        }

        #endregion

        #region Private Methods

        private void ShowPolygon(Point[] points, Brush background, Brush border, double thickness)
        {
            if (points == null || points.Length == 0)
            {
                return;
            }

            Polygon polygon = new Polygon();
            polygon.Fill = background;
            polygon.Stroke = border;
            polygon.StrokeThickness = thickness;

            foreach (Point point in points)
            {
                polygon.Points.Add(point);
            }

            canvas.Children.Add(polygon);
        }

        /// <summary>
        /// This is just a wrapper to the color converter (why can't they have a method off the color class with all
        /// the others?)
        /// </summary>
        private static Color ColorFromHex(string hexValue)
        {
            if (hexValue.StartsWith("#"))
            {
                return (Color)ColorConverter.ConvertFromString(hexValue);
            }
            else
            {
                return (Color)ColorConverter.ConvertFromString("#" + hexValue);
            }
        }

        #endregion
    }
}
```


[[File:PolyIntersect.png]]


## D


```d
import std.stdio, std.array, std.range, std.typecons, std.algorithm;

struct Vec2 { // To be replaced with Phobos code.
    double x, y;

    Vec2 opBinary(string op="-")(in Vec2 other)
    const pure nothrow @safe @nogc {
        return Vec2(this.x - other.x, this.y - other.y);
    }

    typeof(x) cross(in Vec2 other) const pure nothrow @safe @nogc {
        return this.x * other.y - this.y * other.x;
    }
}

immutable(Vec2)[] clip(in Vec2[] subjectPolygon, in Vec2[] clipPolygon)
pure /*nothrow*/ @safe in {
    assert(subjectPolygon.length > 1);
    assert(clipPolygon.length > 1);
    // Probably clipPolygon needs to be convex and probably
    // its vertices need to be listed in a direction.
} out(result) {
    assert(result.length > 1);
} body {
    alias Edge = Tuple!(Vec2,"p", Vec2,"q");

    static enum isInside = (in Vec2 p, in Edge cle)
    pure nothrow @safe @nogc =>
        (cle.q.x - cle.p.x) * (p.y - cle.p.y) >
        (cle.q.y - cle.p.y) * (p.x - cle.p.x);

    static Vec2 intersection(in Edge se, in Edge cle)
    pure nothrow @safe @nogc {
        immutable dc = cle.p - cle.q;
        immutable dp = se.p - se.q;
        immutable n1 = cle.p.cross(cle.q);
        immutable n2 = se.p.cross(se.q);
        immutable n3 = 1.0 / dc.cross(dp);
        return Vec2((n1 * dp.x - n2 * dc.x) * n3,
                    (n1 * dp.y - n2 * dc.y) * n3);
    }

    // How much slower is this compared to lower-level code?
    static enum edges = (in Vec2[] poly) pure nothrow @safe @nogc =>
        // poly[$ - 1 .. $].chain(poly).zip!Edge(poly);
        poly[$ - 1 .. $].chain(poly).zip(poly).map!Edge;

    immutable(Vec2)[] result = subjectPolygon.idup; // Not nothrow.

    foreach (immutable clipEdge; edges(clipPolygon)) {
        immutable inputList = result;
        result.destroy;
        foreach (immutable inEdge; edges(inputList)) {
            if (isInside(inEdge.q, clipEdge)) {
                if (!isInside(inEdge.p, clipEdge))
                    result ~= intersection(inEdge, clipEdge);
                result ~= inEdge.q;
            } else if (isInside(inEdge.p, clipEdge))
                result ~= intersection(inEdge, clipEdge);
        }
    }

    return result;
}

// Code adapted from the C version.
void saveEPSImage(in string fileName, in Vec2[] subjPoly,
                  in Vec2[] clipPoly, in Vec2[] clipped)
in {
    assert(!fileName.empty);
    assert(subjPoly.length > 1);
    assert(clipPoly.length > 1);
    assert(clipped.length > 1);
} body {
    auto eps = File(fileName, "w");

    // The image bounding box is hard-coded, not computed.
    eps.writeln(
"%%!PS-Adobe-3.0
%%%%BoundingBox: 40 40 360 360
/l {lineto} def
/m {moveto} def
/s {setrgbcolor} def
/c {closepath} def
/gs {fill grestore stroke} def
");

    eps.writef("0 setlinewidth %g %g m ", clipPoly[0].tupleof);
    foreach (immutable cl; clipPoly[1 .. $])
        eps.writef("%g %g l ", cl.tupleof);
    eps.writefln("c 0.5 0 0 s gsave 1 0.7 0.7 s gs");

    eps.writef("%g %g m ", subjPoly[0].tupleof);
    foreach (immutable s; subjPoly[1 .. $])
        eps.writef("%g %g l ", s.tupleof);
    eps.writefln("c 0 0.2 0.5 s gsave 0.4 0.7 1 s gs");

    eps.writef("2 setlinewidth [10 8] 0 setdash %g %g m ",
               clipped[0].tupleof);
    foreach (immutable c; clipped[1 .. $])
        eps.writef("%g %g l ", c.tupleof);
    eps.writefln("c 0.5 0 0.5 s gsave 0.7 0.3 0.8 s gs");

    eps.writefln("%%%%EOF");
    eps.close;
    writeln(fileName, " written.");
}

void main() {
    alias V = Vec2;
    immutable subjectPolygon = [V(50, 150), V(200, 50), V(350, 150),
                                V(350, 300), V(250, 300), V(200, 250),
                                V(150, 350), V(100, 250), V(100, 200)];
    immutable clippingPolygon = [V(100, 100), V(300, 100),
                                 V(300, 300), V(100, 300)];
    immutable clipped = subjectPolygon.clip(clippingPolygon);
    writefln("%(%s\n%)", clipped);
    saveEPSImage("sutherland_hodgman_clipping_out.eps",
                 subjectPolygon, clippingPolygon, clipped);
}
```

{{out}}

```txt
immutable(Vec2)(100, 116.667)
immutable(Vec2)(125, 100)
immutable(Vec2)(275, 100)
immutable(Vec2)(300, 116.667)
immutable(Vec2)(300, 300)
immutable(Vec2)(250, 300)
immutable(Vec2)(200, 250)
immutable(Vec2)(175, 300)
immutable(Vec2)(125, 300)
immutable(Vec2)(100, 250)
sutherland_hodgman_clipping_out.eps written.
```

It also outputs an EPS file, the same as the C entry.


## Elixir

{{trans|Ruby}}

```elixir
defmodule SutherlandHodgman do
  defp inside(cp1, cp2, p), do: (cp2.x-cp1.x)*(p.y-cp1.y) > (cp2.y-cp1.y)*(p.x-cp1.x)

  defp intersection(cp1, cp2, s, e) do
    {dcx, dcy} = {cp1.x-cp2.x, cp1.y-cp2.y}
    {dpx, dpy} = {s.x-e.x, s.y-e.y}
    n1 = cp1.x*cp2.y - cp1.y*cp2.x
    n2 = s.x*e.y - s.y*e.x
    n3 = 1.0 / (dcx*dpy - dcy*dpx)
    %{x: (n1*dpx - n2*dcx) * n3, y: (n1*dpy - n2*dcy) * n3}
  end

  def polygon_clipping(subjectPolygon, clipPolygon) do
    Enum.chunk([List.last(clipPolygon) | clipPolygon], 2, 1)
    |> Enum.reduce(subjectPolygon, fn [cp1,cp2],acc ->
         Enum.chunk([List.last(acc) | acc], 2, 1)
         |> Enum.reduce([], fn [s,e],outputList ->
              case {inside(cp1, cp2, e), inside(cp1, cp2, s)} do
                {true,  true} -> [e | outputList]
                {true, false} -> [e, intersection(cp1,cp2,s,e) | outputList]
                {false, true} -> [intersection(cp1,cp2,s,e) | outputList]
                _             -> outputList
              end
            end)
         |> Enum.reverse
       end)
  end
end

subjectPolygon = [[50, 150], [200, 50], [350, 150], [350, 300], [250, 300],
                  [200, 250], [150, 350], [100, 250], [100, 200]]
                 |> Enum.map(fn [x,y] -> %{x: x, y: y} end)

clipPolygon = [[100, 100], [300, 100], [300, 300], [100, 300]]
              |> Enum.map(fn [x,y] -> %{x: x, y: y} end)

SutherlandHodgman.polygon_clipping(subjectPolygon, clipPolygon)
|> Enum.each(&IO.inspect/1)
```


{{out}}

```txt

%{x: 100.0, y: 116.66666666666667}
%{x: 125.00000000000001, y: 100.0}
%{x: 275.0, y: 100.0}
%{x: 300.0, y: 116.66666666666667}
%{x: 300.0, y: 299.99999999999994}
%{x: 250.0, y: 300.0}
%{x: 200, y: 250}
%{x: 175.0, y: 300.0}
%{x: 125.0, y: 300.0}
%{x: 100.0, y: 250.0}

```



## Fortran

Infos:
The polygons are fortran type with an allocatable array "vertex" that contains the vertices and an integer n that is the size of the polygon. For any polygon, the first vertex and the last vertex have to be the same.
As you will see, in the main function, we allocate the vertex array of the result polygon with its maximal size.

```Fortran


module SutherlandHodgmanUtil
  ! functions and type needed for Sutherland-Hodgman algorithm

  ! -------------------------------------------------------- !
  type polygon
    !type for polygons
    ! when you define a polygon, the first and the last vertices have to be the same
    integer :: n
    double precision, dimension(:,:), allocatable :: vertex
  end type polygon

  contains

  ! -------------------------------------------------------- !
  subroutine sutherlandHodgman( ref, clip, outputPolygon )
    ! Sutherland Hodgman algorithm for 2d polygons

    ! -- parameters of the subroutine --
    type(polygon) :: ref, clip, outputPolygon

    ! -- variables used is the subroutine
    type(polygon) :: workPolygon               ! polygon clipped step by step
    double precision, dimension(2) :: y1,y2    ! vertices of edge to clip workPolygon
    integer :: i

    ! allocate workPolygon with the maximal possible size
    !   the sum of the size of polygon ref and clip
    allocate(workPolygon%vertex( ref%n+clip%n , 2 ))

    !  initialise the work polygon with clip
    workPolygon%n = clip%n
    workPolygon%vertex(1:workPolygon%n,:) = clip%vertex(1:workPolygon%n,:)

    do i=1,ref%n-1 ! for each edge i of the polygon ref
      y1(:) = ref%vertex(i,:)   !  vertex 1 of edge i
      y2(:) = ref%vertex(i+1,:) !  vertex 2 of edge i

      ! clip the work polygon by edge i
      call edgeClipping( workPolygon, y1, y2, outputPolygon)
      ! workPolygon <= outputPolygon
      workPolygon%n = outputPolygon%n
      workPolygon%vertex(1:workPolygon%n,:) = outputPolygon%vertex(1:workPolygon%n,:)

    end do
    deallocate(workPolygon%vertex)
  end subroutine sutherlandHodgman

  ! -------------------------------------------------------- !
  subroutine edgeClipping( poly, y1, y2, outputPoly )
    ! make the clipping  of the polygon by the line (x1x2)

    type(polygon) :: poly, outputPoly
    double precision, dimension(2) :: y1, y2, x1, x2, intersecPoint
    integer ::  i, c

    c = 0 ! counter for the output polygon

    do i=1,poly%n-1 ! for each edge i of poly
      x1(:) = poly%vertex(i,:)   ! vertex 1 of edge i
      x2(:) = poly%vertex(i+1,:) ! vertex 2 of edge i

      if ( inside(x1, y1, y2) ) then ! if vertex 1 in inside clipping region
        if ( inside(x2, y1, y2) ) then ! if vertex 2 in inside clipping region
          ! add the vertex 2 to the output polygon
          c = c+1
          outputPoly%vertex(c,:) = x2(:)

        else ! vertex i+1 is outside
          intersecPoint = intersection(x1, x2, y1,y2)
          c = c+1
          outputPoly%vertex(c,:) = intersecPoint(:)
        end if
      else ! vertex i is outside
        if ( inside(x2, y1, y2) ) then
          intersecPoint = intersection(x1, x2, y1,y2)
          c = c+1
          outputPoly%vertex(c,:) = intersecPoint(:)

          c = c+1
          outputPoly%vertex(c,:) = x2(:)
        end if
      end if
    end do

    if (c .gt. 0) then
      ! if the last vertice is not equal to the first one
      if ( (outputPoly%vertex(1,1) .ne. outputPoly%vertex(c,1)) .or. &
           (outputPoly%vertex(1,2) .ne. outputPoly%vertex(c,2)))  then
        c=c+1
        outputPoly%vertex(c,:) = outputPoly%vertex(1,:)
      end if
    end if
    ! set the size of the outputPolygon
    outputPoly%n = c
  end subroutine edgeClipping

  ! -------------------------------------------------------- !
  function intersection( x1, x2, y1, y2)
    ! computes the intersection between segment [x1x2]
    ! and line the line (y1y2)

    ! -- parameters of the function --
    double precision, dimension(2) :: x1, x2, &  ! points of the segment
                                      y1, y2     ! points of the line

    double precision, dimension(2) :: intersection, vx, vy, x1y1
    double precision :: a

    vx(:) = x2(:) - x1(:)
    vy(:) = y2(:) - y1(:)

    ! if the vectors are colinear
    if ( crossProduct(vx,vy) .eq. 0.d0) then
      x1y1(:) = y1(:) - x1(:)
      ! if the the segment [x1x2] is included in the line (y1y2)
      if ( crossProduct(x1y1,vx) .eq. 0.d0) then
        ! the intersection is the last point of the segment
        intersection(:) = x2(:)
      end if
    else ! the vectors are not colinear
      ! we want to find the inersection between [x1x2]
      ! and (y1,y2).
      ! mathematically, we want to find a in [0;1] such
      ! that :
      !     x1 + a vx = y1 + b vy
      ! <=> a vx = x1y1 + b vy
      ! <=> a vx^vy = x1y1^vy      , ^ is cross product
      ! <=> a = x1y1^vy / vx^vy

      x1y1(:) = y1(:) - x1(:)
      ! we compute a
      a = crossProduct(x1y1,vy)/crossProduct(vx,vy)
      ! if a is not in [0;1]
      if ( (a .gt. 1.d0) .or. (a .lt. 0)) then
        ! no intersection
      else
        intersection(:) = x1(:) + a*vx(:)
      end if
    end if

  end function intersection


  ! -------------------------------------------------------- !
  function inside( p, y1, y2)
    ! function that tells is the point p is at left of the line (y1y2)

    double precision, dimension(2) :: p, y1, y2, v1, v2
    logical :: inside
    v1(:) = y2(:) -  y1(:)
    v2(:) = p(:)  -  y1(:)
    if ( crossProduct(v1,v2) .ge. 0.d0) then
      inside = .true.
    else
      inside = .false.
    end if

   contains
  end function inside

  ! -------------------------------------------------------- !
  function dotProduct( v1, v2)
    ! compute the dot product of vectors v1 and v2
    double precision, dimension(2) :: v1
    double precision, dimension(2) :: v2
    double precision :: dotProduct
    dotProduct = v1(1)*v2(1) + v1(2)*v2(2)
  end function dotProduct

  ! -------------------------------------------------------- !
  function crossProduct( v1, v2)
    ! compute the crossproduct of vectors v1 and v2
    double precision, dimension(2) :: v1
    double precision, dimension(2) :: v2
    double precision :: crossProduct
    crossProduct = v1(1)*v2(2) - v1(2)*v2(1)
  end function crossProduct

end module SutherlandHodgmanUtil

program main

  ! load the module for S-H algorithm
  use SutherlandHodgmanUtil, only : polygon, &
                                    sutherlandHodgman, &
                                    edgeClipping

  type(polygon) :: p1, p2, res
  integer :: c, n
  double precision, dimension(2) :: y1, y2

  ! when you define a polygon, the first and the last vertices have to be the same

  ! first polygon
  p1%n = 10
  allocate(p1%vertex(p1%n,2))
  p1%vertex(1,1)=50.d0
  p1%vertex(1,2)=150.d0

  p1%vertex(2,1)=200.d0
  p1%vertex(2,2)=50.d0

  p1%vertex(3,1)= 350.d0
  p1%vertex(3,2)= 150.d0

  p1%vertex(4,1)= 350.d0
  p1%vertex(4,2)= 300.d0

  p1%vertex(5,1)= 250.d0
  p1%vertex(5,2)= 300.d0

  p1%vertex(6,1)= 200.d0
  p1%vertex(6,2)= 250.d0

  p1%vertex(7,1)= 150.d0
  p1%vertex(7,2)= 350.d0

  p1%vertex(8,1)= 100.d0
  p1%vertex(8,2)= 250.d0

  p1%vertex(9,1)= 100.d0
  p1%vertex(9,2)= 200.d0

  p1%vertex(10,1)=  50.d0
  p1%vertex(10,2)= 150.d0

  y1 = (/ 100.d0, 300.d0 /)
  y2 = (/ 300.d0, 300.d0 /)

  ! second polygon
  p2%n = 5
  allocate(p2%vertex(p2%n,2))

  p2%vertex(1,1)= 100.d0
  p2%vertex(1,2)= 100.d0

  p2%vertex(2,1)= 300.d0
  p2%vertex(2,2)= 100.d0

  p2%vertex(3,1)= 300.d0
  p2%vertex(3,2)= 300.d0

  p2%vertex(4,1)= 100.d0
  p2%vertex(4,2)= 300.d0

  p2%vertex(5,1)= 100.d0
  p2%vertex(5,2)= 100.d0

  allocate(res%vertex(p1%n+p2%n,2))
  call sutherlandHodgman( p2, p1, res)
  write(*,*) "Suterland-Hodgman"
  do c=1, res%n
    write(*,*) res%vertex(c,1), res%vertex(c,2)
  end do
  deallocate(res%vertex)

end program main


```

Output:
   Suterland-Hodgman
   300.00000000000000        300.00000000000000
   250.00000000000000        300.00000000000000
   200.00000000000000        250.00000000000000
   175.00000000000000        300.00000000000000
   125.00000000000000        300.00000000000000
   100.00000000000000        250.00000000000000
   100.00000000000000        200.00000000000000
   100.00000000000000        200.00000000000000
   100.00000000000000        116.66666666666667
   125.00000000000000        100.00000000000000
   275.00000000000000        100.00000000000000
   300.00000000000000        116.66666666666666
   300.00000000000000        300.00000000000000


## Go

No extra credit today.

```go
package main

import "fmt"

type point struct {
    x, y float32
}

var subjectPolygon = []point{{50, 150}, {200, 50}, {350, 150}, {350, 300},
    {250, 300}, {200, 250}, {150, 350}, {100, 250}, {100, 200}}

var clipPolygon = []point{{100, 100}, {300, 100}, {300, 300}, {100, 300}}

func main() {
    var cp1, cp2, s, e point
    inside := func(p point) bool {
        return (cp2.x-cp1.x)*(p.y-cp1.y) > (cp2.y-cp1.y)*(p.x-cp1.x)
    }
    intersection := func() (p point) {
        dcx, dcy := cp1.x-cp2.x, cp1.y-cp2.y
        dpx, dpy := s.x-e.x, s.y-e.y
        n1 := cp1.x*cp2.y - cp1.y*cp2.x
        n2 := s.x*e.y - s.y*e.x
        n3 := 1 / (dcx*dpy - dcy*dpx)
        p.x = (n1*dpx - n2*dcx) * n3
        p.y = (n1*dpy - n2*dcy) * n3
        return
    }
    outputList := subjectPolygon
    cp1 = clipPolygon[len(clipPolygon)-1]
    for _, cp2 = range clipPolygon { // WP clipEdge is cp1,cp2 here
        inputList := outputList
        outputList = nil
        s = inputList[len(inputList)-1]
        for _, e = range inputList {
            if inside(e) {
                if !inside(s) {
                    outputList = append(outputList, intersection())
                }
                outputList = append(outputList, e)
            } else if inside(s) {
                outputList = append(outputList, intersection())
            }
            s = e
        }
        cp1 = cp2
    }
    fmt.Println(outputList)
}
```

{{out}}

```txt

[{100 116.66667} {125 100} {275 100} {300 116.66667} {300 300} {250 300} {200 250} {175 300} {125 300} {100 250}]

```

(You can [http://golang.org/doc/play/#package%20main%0A%20%0Aimport%20%22fmt%22%0A%20%0Atype%20point%20struct%20%7B%0A%20%20%20%20x%2C%20y%20float32%0A%7D%0A%20%0Avar%20subjectPolygon%20%3D%20%5B%5Dpoint%7B%7B50%2C%20150%7D%2C%20%7B200%2C%2050%7D%2C%20%7B350%2C%20150%7D%2C%20%7B350%2C%20300%7D%2C%0A%20%20%20%20%7B250%2C%20300%7D%2C%20%7B200%2C%20250%7D%2C%20%7B150%2C%20350%7D%2C%20%7B100%2C%20250%7D%2C%20%7B100%2C%20200%7D%7D%0A%20%0Avar%20clipPolygon%20%3D%20%5B%5Dpoint%7B%7B100%2C%20100%7D%2C%20%7B300%2C%20100%7D%2C%20%7B300%2C%20300%7D%2C%20%7B100%2C%20300%7D%7D%0A%20%0Afunc%20main()%20%7B%0A%20%20%20%20var%20cp1%2C%20cp2%2C%20s%2C%20e%20point%0A%20%20%20%20inside%20%3A%3D%20func(p%20point)%20bool%20%7B%0A%20%20%20%20%20%20%20%20return%20(cp2.x-cp1.x)*(p.y-cp1.y)%20%3E%20(cp2.y-cp1.y)*(p.x-cp1.x)%0A%20%20%20%20%7D%0A%20%20%20%20intersection%20%3A%3D%20func()%20(p%20point)%20%7B%0A%20%20%20%20%20%20%20%20dcx%2C%20dcy%20%3A%3D%20cp1.x-cp2.x%2C%20cp1.y-cp2.y%0A%20%20%20%20%20%20%20%20dpx%2C%20dpy%20%3A%3D%20s.x-e.x%2C%20s.y-e.y%0A%20%20%20%20%20%20%20%20n1%20%3A%3D%20cp1.x*cp2.y%20-%20cp1.y*cp2.x%0A%20%20%20%20%20%20%20%20n2%20%3A%3D%20s.x*e.y%20-%20s.y*e.x%0A%20%20%20%20%20%20%20%20n3%20%3A%3D%201%20%2F%20(dcx*dpy%20-%20dcy*dpx)%0A%20%20%20%20%20%20%20%20p.x%20%3D%20(n1*dpx%20-%20n2*dcx)%20*%20n3%0A%20%20%20%20%20%20%20%20p.y%20%3D%20(n1*dpy%20-%20n2*dcy)%20*%20n3%0A%20%20%20%20%20%20%20%20return%0A%20%20%20%20%7D%0A%20%20%20%20outputList%20%3A%3D%20subjectPolygon%0A%20%20%20%20cp1%20%3D%20clipPolygon%5Blen(clipPolygon)-1%5D%0A%20%20%20%20for%20_%2C%20cp2%20%3D%20range%20clipPolygon%20%7B%20%2F%2F%20WP%20clipEdge%20is%20cp1%2Ccp2%20here%0A%20%20%20%20%20%20%20%20inputList%20%3A%3D%20outputList%0A%20%20%20%20%20%20%20%20outputList%20%3D%20nil%0A%20%20%20%20%20%20%20%20s%20%3D%20inputList%5Blen(inputList)-1%5D%0A%20%20%20%20%20%20%20%20for%20_%2C%20e%20%3D%20range%20inputList%20%7B%0A%20%20%20%20%20%20%20%20%20%20%20%20if%20inside(e)%20%7B%0A%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20if%20!inside(s)%20%7B%0A%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20outputList%20%3D%20append(outputList%2C%20intersection())%0A%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%7D%0A%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20outputList%20%3D%20append(outputList%2C%20e)%0A%20%20%20%20%20%20%20%20%20%20%20%20%7D%20else%20if%20inside(s)%20%7B%0A%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20outputList%20%3D%20append(outputList%2C%20intersection())%0A%20%20%20%20%20%20%20%20%20%20%20%20%7D%0A%20%20%20%20%20%20%20%20%20%20%20%20s%20%3D%20e%0A%20%20%20%20%20%20%20%20%7D%0A%20%20%20%20%20%20%20%20cp1%20%3D%20cp2%0A%20%20%20%20%7D%0A%20%20%20%20fmt.Println(outputList)%0A%7D try it online])


## Haskell


```haskell
module SuthHodgClip (clipTo) where

import Data.List

type   Pt a = (a, a)
type   Ln a = (Pt a, Pt a)
type Poly a = [Pt a]

-- Return a polygon from a list of points.
polyFrom ps = last ps : ps

-- Return a list of lines from a list of points.
linesFrom pps@(_:ps) = zip pps ps

-- Return true if the point (x,y) is on or to the left of the oriented line
-- defined by (px,py) and (qx,qy).
(.|) :: (Num a, Ord a) => Pt a -> Ln a -> Bool
(x,y) .| ((px,py),(qx,qy)) = (qx-px)*(y-py) >= (qy-py)*(x-px)

-- Return the intersection of two lines.
(><) :: Fractional a => Ln a -> Ln a -> Pt a
((x1,y1),(x2,y2)) >< ((x3,y3),(x4,y4)) =
    let (r,s) = (x1*y2-y1*x2, x3*y4-y3*x4)
        (t,u,v,w) = (x1-x2, y3-y4, y1-y2, x3-x4)
        d = t*u-v*w
    in ((r*w-t*s)/d, (r*u-v*s)/d)

-- Intersect the line segment (p0,p1) with the clipping line's left halfspace,
-- returning the point closest to p1.  In the special case where p0 lies outside
-- the halfspace and p1 lies inside we return both the intersection point and
-- p1.  This ensures we will have the necessary segment along the clipping line.
(-|) :: (Fractional a, Ord a) => Ln a -> Ln a -> [Pt a]
ln@(p0, p1) -| clipLn =
    case (p0 .| clipLn, p1 .| clipLn) of
      (False, False) -> []
      (False, True)  -> [isect, p1]
      (True,  False) -> [isect]
      (True,  True)  -> [p1]
    where isect = ln >< clipLn

-- Intersect the polygon with the clipping line's left halfspace.
(<|) :: (Fractional a, Ord a) => Poly a -> Ln a -> Poly a
poly <| clipLn = polyFrom $ concatMap (-| clipLn) (linesFrom poly)

-- Intersect a target polygon with a clipping polygon.  The latter is assumed to
-- be convex.
clipTo :: (Fractional a, Ord a) => [Pt a] -> [Pt a] -> [Pt a]
targPts `clipTo` clipPts =
    let targPoly = polyFrom targPts
        clipLines = linesFrom (polyFrom clipPts)
    in foldl' (<|) targPoly clipLines
```

Print the resulting list of points and display the polygons in a window.


```haskell
import Graphics.HGL
import SuthHodgClip

targPts = [( 50,150), (200, 50), (350,150), (350,300), (250,300),
           (200,250), (150,350), (100,250), (100,200)] :: [(Float,Float)]
clipPts = [(100,100), (300,100), (300,300), (100,300)] :: [(Float,Float)]

toInts = map (\(a,b) -> (round a, round b))
complete xs = last xs : xs

drawSolid w c = drawInWindow w . withRGB c . polygon
drawLines w p = drawInWindow w . withPen p . polyline . toInts . complete

blue  = RGB 0x99 0x99 0xff
green = RGB 0x99 0xff 0x99
pink  = RGB 0xff 0x99 0x99
white = RGB 0xff 0xff 0xff

main = do
  let resPts = targPts `clipTo` clipPts
      sz = 400
      win = [(0,0), (sz,0), (sz,sz), (0,sz)]
  runWindow "Sutherland-Hodgman Polygon Clipping" (sz,sz) $ \w -> do
         print $ toInts resPts
         penB <- createPen Solid 3 blue
         penP <- createPen Solid 5 pink
         drawSolid w white win
         drawLines w penB targPts
         drawLines w penP clipPts
         drawSolid w green $ toInts resPts
         getKey w
```

{{out}}

```txt

[(100,200),(100,200),(100,117),(125,100),(275,100),(300,117),(300,300),(250,300),(200,250),(175,300),(125,300),(100,250),(100,200)]

```

[[File:Sutherland-Hodgman_haskell.png]]


## J

'''Solution:'''

```j
NB. assumes counterclockwise orientation.
NB. determine whether point y is inside edge x.
isinside=:0< [:-/ .* {.@[ -~"1 {:@[,:]

NB. (p0,:p1) intersection (p2,:p3)
intersection=:|:@[ (+/ .* (,-.)) [:{. ,.&(-~/) %.~ -&{:

SutherlandHodgman=:4 :0 NB. clip S-H subject
  clip=.2 ]\ (,{.) x
  subject=.y
  for_edge. clip do.
    S=.{:input=.subject
    subject=.0 2$0
    for_E. input do.
      if. edge isinside E do.
        if. -.edge isinside S do.
          subject=.subject,edge intersection S,:E end.
        subject=.subject,E
      elseif. edge isinside S do.
        subject=.subject,edge intersection S,:E end.
      S=.E
    end.
  end.
  subject
)
```

{{out|Example use}}

```j
   subject=: 50 150,200 50,350 150,350 300,250 300,200 250,150 350,100 250,:100 200
   clip=: 100 100,300 100,300 300,:100 300
   clip SutherlandHodgman subject
100 116.667
125     100
275     100
300 116.667
300     300
250     300
200     250
175     300
125     300
100     250
```



## Java

{{works with|Java|7}}

```java5
import java.awt.*;
import java.awt.geom.Line2D;
import java.util.*;
import java.util.List;
import javax.swing.*;

public class SutherlandHodgman extends JFrame {

    SutherlandHodgmanPanel panel;

    public static void main(String[] args) {
        JFrame f = new SutherlandHodgman();
        f.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        f.setVisible(true);
    }

    public SutherlandHodgman() {
        Container content = getContentPane();
        content.setLayout(new BorderLayout());
        panel = new SutherlandHodgmanPanel();
        content.add(panel, BorderLayout.CENTER);
        setTitle("SutherlandHodgman");
        pack();
        setLocationRelativeTo(null);
    }
}

class SutherlandHodgmanPanel extends JPanel {
    List<double[]> subject, clipper, result;

    public SutherlandHodgmanPanel() {
        setPreferredSize(new Dimension(600, 500));

        // these subject and clip points are assumed to be valid
        double[][] subjPoints = {{50, 150}, {200, 50}, {350, 150}, {350, 300},
        {250, 300}, {200, 250}, {150, 350}, {100, 250}, {100, 200}};

        double[][] clipPoints = {{100, 100}, {300, 100}, {300, 300}, {100, 300}};

        subject = new ArrayList<>(Arrays.asList(subjPoints));
        result  = new ArrayList<>(subject);
        clipper = new ArrayList<>(Arrays.asList(clipPoints));

        clipPolygon();
    }

    private void clipPolygon() {
        int len = clipper.size();
        for (int i = 0; i < len; i++) {

            int len2 = result.size();
            List<double[]> input = result;
            result = new ArrayList<>(len2);

            double[] A = clipper.get((i + len - 1) % len);
            double[] B = clipper.get(i);

            for (int j = 0; j < len2; j++) {

                double[] P = input.get((j + len2 - 1) % len2);
                double[] Q = input.get(j);

                if (isInside(A, B, Q)) {
                    if (!isInside(A, B, P))
                        result.add(intersection(A, B, P, Q));
                    result.add(Q);
                } else if (isInside(A, B, P))
                    result.add(intersection(A, B, P, Q));
            }
        }
    }

    private boolean isInside(double[] a, double[] b, double[] c) {
        return (a[0] - c[0]) * (b[1] - c[1]) > (a[1] - c[1]) * (b[0] - c[0]);
    }

    private double[] intersection(double[] a, double[] b, double[] p, double[] q) {
        double A1 = b[1] - a[1];
        double B1 = a[0] - b[0];
        double C1 = A1 * a[0] + B1 * a[1];

        double A2 = q[1] - p[1];
        double B2 = p[0] - q[0];
        double C2 = A2 * p[0] + B2 * p[1];

        double det = A1 * B2 - A2 * B1;
        double x = (B2 * C1 - B1 * C2) / det;
        double y = (A1 * C2 - A2 * C1) / det;

        return new double[]{x, y};
    }

    @Override
    public void paintComponent(Graphics g) {
        super.paintComponent(g);
        Graphics2D g2 = (Graphics2D) g;
        g2.translate(80, 60);
        g2.setStroke(new BasicStroke(3));
        g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING,
                RenderingHints.VALUE_ANTIALIAS_ON);

        drawPolygon(g2, subject, Color.blue);
        drawPolygon(g2, clipper, Color.red);
        drawPolygon(g2, result, Color.green);
    }

    private void drawPolygon(Graphics2D g2, List<double[]> points, Color color) {
        g2.setColor(color);
        int len = points.size();
        Line2D line = new Line2D.Double();
        for (int i = 0; i < len; i++) {
            double[] p1 = points.get(i);
            double[] p2 = points.get((i + 1) % len);
            line.setLine(p1[0], p1[1], p2[0], p2[1]);
            g2.draw(line);
        }
    }
}
```



## JavaScript

'''Solution:'''

```javascript

<html>
    <head>
	<script>
        function clip (subjectPolygon, clipPolygon) {

            var cp1, cp2, s, e;
            var inside = function (p) {
                return (cp2[0]-cp1[0])*(p[1]-cp1[1]) > (cp2[1]-cp1[1])*(p[0]-cp1[0]);
            };
            var intersection = function () {
                var dc = [ cp1[0] - cp2[0], cp1[1] - cp2[1] ],
                    dp = [ s[0] - e[0], s[1] - e[1] ],
                    n1 = cp1[0] * cp2[1] - cp1[1] * cp2[0],
                    n2 = s[0] * e[1] - s[1] * e[0],
                    n3 = 1.0 / (dc[0] * dp[1] - dc[1] * dp[0]);
                return [(n1*dp[0] - n2*dc[0]) * n3, (n1*dp[1] - n2*dc[1]) * n3];
            };
            var outputList = subjectPolygon;
            cp1 = clipPolygon[clipPolygon.length-1];
            for (j in clipPolygon) {
                var cp2 = clipPolygon[j];
                var inputList = outputList;
                outputList = [];
                s = inputList[inputList.length - 1]; //last on the input list
                for (i in inputList) {
                    var e = inputList[i];
                    if (inside(e)) {
                        if (!inside(s)) {
                            outputList.push(intersection());
                        }
                        outputList.push(e);
                    }
                    else if (inside(s)) {
                        outputList.push(intersection());
                    }
                    s = e;
                }
                cp1 = cp2;
            }
            return outputList
        }

        function drawPolygon(context, polygon, strokeStyle, fillStyle) {
            context.strokeStyle = strokeStyle;
            context.fillStyle = fillStyle;
            context.beginPath();
            context.moveTo(polygon[0][0],polygon[0][1]); //first vertex
            for (var i = 1; i < polygon.length ; i++)
                context.lineTo(polygon[i][0],polygon[i][1]);
            context.lineTo(polygon[0][0],polygon[0][1]); //back to start
            context.fill();
            context.stroke();
            context.closePath();
        }

        window.onload = function () {
	        var context = document.getElementById('canvas').getContext('2d');
	        var subjectPolygon = [[50, 150], [200, 50], [350, 150], [350, 300], [250, 300], [200, 250], [150, 350], [100, 250], [100, 200]],
	            clipPolygon = [[100, 100], [300, 100], [300, 300], [100, 300]];
	        var clippedPolygon = clip(subjectPolygon, clipPolygon);
	        drawPolygon(context, clipPolygon, '#888','#88f');
	        drawPolygon(context, subjectPolygon, '#888','#8f8');
	        drawPolygon(context, clippedPolygon, '#000','#0ff');
    	}
        </script>
    <body>
    	<canvas id='canvas' width='400' height='400'></canvas>
    </body>
</html>

```


You can see it running <code>[http://jsfiddle.net/elisherer/y6RDB/ here]</code>


## Julia


```julia
using Luxor

isinside(p, a, b) = (b.x - a.x) * (p.y - a.y) > (b.y - a.y) * (p.x - a.x)

function intersection(a, b, s, f)
    dc = [a.x - b.x, a.y - b.y]
    dp = [s.x - f.x, s.y - f.y]
    n1 = a.x * b.y - a.y * b.x
    n2 = s.x * f.y - s.y * f.x
    n3 = 1.0 / (dc[1] * dp[2] - dc[2] * dp[1])
    Point((n1 * dp[1] - n2 * dc[1]) * n3, (n1 * dp[2] - n2 * dc[2]) * n3)
end

function clipSH(spoly, cpoly)
    outarr = spoly
    q = cpoly[end]
    for p in cpoly
        inarr = outarr
        outarr = Point[]
        s = inarr[end]
        for vtx in inarr
            if isinside(vtx, q, p)
                if !isinside(s, q, p)
                    push!(outarr, intersection(q, p, s, vtx))
                end
                push!(outarr, vtx)
            elseif isinside(s, q, p)
                push!(outarr, intersection(q, p, s, vtx))
            end
            s = vtx
        end
        q = p
    end
    outarr
end

subjectp = [Point(50, 150), Point(200, 50), Point(350, 150), Point(350, 300),
    Point(250, 300), Point(200, 250), Point(150, 350), Point(100, 250), Point(100, 200)]

clipp = [Point(100, 100), Point(300, 100), Point(300, 300), Point(100, 300)]

Drawing(400, 400, "intersecting-polygons.png")
background("white")
sethue("red")
poly(subjectp, :stroke, close=true)
sethue("blue")
poly(clipp, :stroke, close=true)
clipped = clipSH(subjectp, clipp)
sethue("gold")
poly(clipped, :fill, close=true)
finish()
preview()
println(clipped)

```
{{out}}

```txt

Point[Point(100.0, 116.667), Point(125.0, 100.0), Point(275.0, 100.0), Point(300.0, 116.667),
    Point(300.0, 300.0), Point(250.0, 300.0), Point(200.0, 250.0), Point(175.0, 300.0),
    Point(125.0, 300.0), Point(100.0, 250.0)]

```



## Kotlin

{{trans|Java}}

```scala
// version 1.1.2

import java.awt.*
import java.awt.geom.Line2D
import javax.swing.*

class SutherlandHodgman : JPanel() {
    private val subject = listOf(
        doubleArrayOf( 50.0, 150.0), doubleArrayOf(200.0,  50.0), doubleArrayOf(350.0, 150.0),
        doubleArrayOf(350.0, 300.0), doubleArrayOf(250.0, 300.0), doubleArrayOf(200.0, 250.0),
        doubleArrayOf(150.0, 350.0), doubleArrayOf(100.0, 250.0), doubleArrayOf(100.0, 200.0)
    )

    private val clipper = listOf(
        doubleArrayOf(100.0, 100.0), doubleArrayOf(300.0, 100.0),
        doubleArrayOf(300.0, 300.0), doubleArrayOf(100.0, 300.0)
    )

    private var result = subject.toMutableList()

    init {
        preferredSize = Dimension(600, 500)
        clipPolygon()
    }

    private fun clipPolygon() {
        val len = clipper.size
        for (i in 0 until len) {
            val len2 = result.size
            val input = result
            result = mutableListOf<DoubleArray>()
            val a = clipper[(i + len - 1) % len]
            val b = clipper[i]

            for (j in 0 until len2) {
                val p = input[(j + len2 - 1) % len2]
                val q = input[j]

                if (isInside(a, b, q)) {
                    if (!isInside(a, b, p)) result.add(intersection(a, b, p, q))
                    result.add(q)
                }
                else if (isInside(a, b, p)) result.add(intersection(a, b, p, q))
            }
        }
    }

    private fun isInside(a: DoubleArray, b: DoubleArray, c: DoubleArray) =
        (a[0] - c[0]) * (b[1] - c[1]) > (a[1] - c[1]) * (b[0] - c[0])

    private fun intersection(a: DoubleArray, b: DoubleArray,
                             p: DoubleArray, q: DoubleArray): DoubleArray {
        val a1 = b[1] - a[1]
        val b1 = a[0] - b[0]
        val c1 = a1 * a[0] + b1 * a[1]

        val a2 = q[1] - p[1]
        val b2 = p[0] - q[0]
        val c2 = a2 * p[0] + b2 * p[1]

        val d = a1 * b2 - a2 * b1
        val x = (b2 * c1 - b1 * c2) / d
        val y = (a1 * c2 - a2 * c1) / d

        return doubleArrayOf(x, y)
    }

    override fun paintComponent(g: Graphics) {
        super.paintComponent(g)
        val g2 = g as Graphics2D
        g2.translate(80, 60)
        g2.stroke = BasicStroke(3.0f)
        g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING,
                            RenderingHints.VALUE_ANTIALIAS_ON)
        drawPolygon(g2, subject, Color.blue)
        drawPolygon(g2, clipper, Color.red)
        drawPolygon(g2, result, Color.green)
    }

    private fun drawPolygon(g2: Graphics2D, points: List<DoubleArray>, color: Color) {
        g2.color = color
        val len = points.size
        val line = Line2D.Double()
        for (i in 0 until len) {
            val p1 = points[i]
            val p2 = points[(i + 1) % len]
            line.setLine(p1[0], p1[1], p2[0], p2[1])
            g2.draw(line)
        }
    }
}

fun main(args: Array<String>) {
    SwingUtilities.invokeLater {
        val f = JFrame()
        with(f) {
            defaultCloseOperation = JFrame.EXIT_ON_CLOSE
            add(SutherlandHodgman(), BorderLayout.CENTER)
            title = "Sutherland-Hodgman"
            pack()
            setLocationRelativeTo(null)
            isVisible = true
        }
    }
}
```



## Lua

No extra credit.
{{trans|Go}}

```Lua
subjectPolygon = {
  {50, 150}, {200, 50}, {350, 150}, {350, 300},
  {250, 300}, {200, 250}, {150, 350}, {100, 250}, {100, 200}
}

clipPolygon = {{100, 100}, {300, 100}, {300, 300}, {100, 300}}

function inside(p, cp1, cp2)
  return (cp2.x-cp1.x)*(p.y-cp1.y) > (cp2.y-cp1.y)*(p.x-cp1.x)
end

function intersection(cp1, cp2, s, e)
  local dcx, dcy = cp1.x-cp2.x, cp1.y-cp2.y
  local dpx, dpy = s.x-e.x, s.y-e.y
  local n1 = cp1.x*cp2.y - cp1.y*cp2.x
  local n2 = s.x*e.y - s.y*e.x
  local n3 = 1 / (dcx*dpy - dcy*dpx)
  local x = (n1*dpx - n2*dcx) * n3
  local y = (n1*dpy - n2*dcy) * n3
  return {x=x, y=y}
end

function clip(subjectPolygon, clipPolygon)
  local outputList = subjectPolygon
  local cp1 = clipPolygon[#clipPolygon]
  for _, cp2 in ipairs(clipPolygon) do  -- WP clipEdge is cp1,cp2 here
    local inputList = outputList
    outputList = {}
    local s = inputList[#inputList]
    for _, e in ipairs(inputList) do
      if inside(e, cp1, cp2) then
        if not inside(s, cp1, cp2) then
          outputList[#outputList+1] = intersection(cp1, cp2, s, e)
        end
        outputList[#outputList+1] = e
      elseif inside(s, cp1, cp2) then
        outputList[#outputList+1] = intersection(cp1, cp2, s, e)
      end
      s = e
    end
    cp1 = cp2
  end
  return outputList
end

function main()
  local function mkpoints(t)
    for i, p in ipairs(t) do
      p.x, p.y = p[1], p[2]
    end
  end
  mkpoints(subjectPolygon)
  mkpoints(clipPolygon)

  local outputList = clip(subjectPolygon, clipPolygon)

  for _, p in ipairs(outputList) do
    print(('{%f, %f},'):format(p.x, p.y))
  end
end

main()
```

{{out}}

```Lua
{100.000000, 116.666667},
{125.000000, 100.000000},
{275.000000, 100.000000},
{300.000000, 116.666667},
{300.000000, 300.000000},
{250.000000, 300.000000},
{200.000000, 250.000000},
{175.000000, 300.000000},
{125.000000, 300.000000},
{100.000000, 250.000000},
```

(You can also [http://ideone.com/5tGEQ see it live])


## Mathematica

Geometry is built in to the Wolfram Language.

```Mathematica
p1 = Polygon[{{50, 150}, {200, 50}, {350, 150}, {350, 300}, {250, 300}, {200, 250}, {150, 350}, {100, 250}, {100, 200}}];
p2 = Polygon[{{100, 100}, {300, 100}, {300, 300}, {100, 300}}];

RegionIntersection[p1, p2]

Graphics[{Red, p1, Blue, p2, Green, RegionIntersection[p1, p2]}]
```

{{out}}

```txt
Polygon[{{125, 100}, {100, 350/3}, {100, 200}, {100, 250}, {125, 300}, {175, 300}, {200, 250}, {250, 300}, {300, 300}, {300, 350/3}, {275, 100}}]
```


=={{header|MATLAB}} / {{header|Octave}}==

```MATLAB
%The inputs are a table of x-y pairs for the verticies of the subject
%polygon and boundary polygon. (x values in column 1 and y values in column
%2) The output is a table of x-y pairs for the clipped version of the
%subject polygon.

function clippedPolygon = sutherlandHodgman(subjectPolygon,clipPolygon)

%% Helper Functions

    %computerIntersection() assumes the two lines intersect
    function intersection = computeIntersection(line1,line2)

        %this is an implementation of
        %http://en.wikipedia.org/wiki/Line-line_intersection

        intersection = zeros(1,2);

        detL1 = det(line1);
        detL2 = det(line2);

        detL1x = det([line1(:,1),[1;1]]);
        detL1y = det([line1(:,2),[1;1]]);

        detL2x = det([line2(:,1),[1;1]]);
        detL2y = det([line2(:,2),[1;1]]);

        denominator = det([detL1x detL1y;detL2x detL2y]);

        intersection(1) = det([detL1 detL1x;detL2 detL2x]) / denominator;
        intersection(2) = det([detL1 detL1y;detL2 detL2y]) / denominator;

    end %computeIntersection

    %inside() assumes the boundary is oriented counter-clockwise
    function in = inside(point,boundary)

        pointPositionVector = [diff([point;boundary(1,:)]) 0];
        boundaryVector = [diff(boundary) 0];
        crossVector = cross(pointPositionVector,boundaryVector);

        if ( crossVector(3) <= 0 )
            in = true;
        else
            in = false;
        end

    end %inside

%% Sutherland-Hodgman Algorithm

    clippedPolygon = subjectPolygon;
    numVerticies = size(clipPolygon,1);
    clipVertexPrevious = clipPolygon(end,:);

    for clipVertex = (1:numVerticies)

        clipBoundary = [clipPolygon(clipVertex,:) ; clipVertexPrevious];

        inputList = clippedPolygon;

        clippedPolygon = [];
        if ~isempty(inputList),
            previousVertex = inputList(end,:);
        end

        for subjectVertex = (1:size(inputList,1))

            if ( inside(inputList(subjectVertex,:),clipBoundary) )

                if( not(inside(previousVertex,clipBoundary)) )
                    subjectLineSegment = [previousVertex;inputList(subjectVertex,:)];
                    clippedPolygon(end+1,1:2) = computeIntersection(clipBoundary,subjectLineSegment);
                end

                clippedPolygon(end+1,1:2) = inputList(subjectVertex,:);

            elseif( inside(previousVertex,clipBoundary) )
                    subjectLineSegment = [previousVertex;inputList(subjectVertex,:)];
                    clippedPolygon(end+1,1:2) = computeIntersection(clipBoundary,subjectLineSegment);
            end

            previousVertex = inputList(subjectVertex,:);
            clipVertexPrevious = clipPolygon(clipVertex,:);

        end %for subject verticies
    end %for boundary verticies
end %sutherlandHodgman
```

{{out}}

```MATLAB>>
 subject = [[50;200;350;350;250;200;150;100;100],[150;50;150;300;300;250;350;250;200]];
>> clipPolygon = [[100;300;300;100],[100;100;300;300]];
>> clippedSubject = sutherlandHodgman(subject,clipPolygon);
>> plot([subject(:,1);subject(1,1)],[subject(:,2);subject(1,2)],[0,0,1])
>> hold on
>> plot([clipPolygon(:,1);clipPolygon(1,1)],[clipPolygon(:,2);clipPolygon(1,2)],'r')
>> patch(clippedSubject(:,1),clippedSubject(:,2),0);
>> axis square
```

[[File:Sutherland-Hodgman_MATLAB.png]]


## OCaml


```ocaml
let is_inside (x,y) ((ax,ay), (bx,by)) =
  (bx -. ax) *. (y -. ay) > (by -. ay) *. (x -. ax)

let intersection (sx,sy) (ex,ey) ((ax,ay), (bx,by)) =
  let dc_x, dc_y = (ax -. bx, ay -. by) in
  let dp_x, dp_y = (sx -. ex, sy -. ey) in
  let n1 = ax *. by -. ay *. bx in
  let n2 = sx *. ey -. sy *. ex in
  let n3 = 1.0 /. (dc_x *. dp_y -. dc_y *. dp_x) in
  ((n1 *. dp_x -. n2 *. dc_x) *. n3,
   (n1 *. dp_y -. n2 *. dc_y) *. n3)

let last lst = List.hd (List.rev lst)

let polygon_iter_edges poly f init =
  if poly = [] then init else
    let p0 = List.hd poly in
    let rec aux acc = function
      | p1 :: p2 :: tl -> aux (f (p1, p2) acc) (p2 :: tl)
      | p :: [] -> f (p, p0) acc
      | [] -> acc
    in
    aux init poly

let poly_clip subject_polygon clip_polygon =
  polygon_iter_edges clip_polygon (fun clip_edge input_list ->
    fst (
      List.fold_left (fun (out, s) e ->

        match (is_inside e clip_edge), (is_inside s clip_edge) with
        | true, false -> (e :: (intersection s e clip_edge) :: out), e
        | true, true -> (e :: out), e
        | false, true -> ((intersection s e clip_edge) :: out), e
        | false, false -> (out, e)

      ) ([], last input_list) input_list)

  ) subject_polygon

let () =
  let subject_polygon =
    [ ( 50.0, 150.0); (200.0,  50.0); (350.0, 150.0);
      (350.0, 300.0); (250.0, 300.0); (200.0, 250.0);
      (150.0, 350.0); (100.0, 250.0); (100.0, 200.0); ] in

  let clip_polygon =
    [ (100.0, 100.0); (300.0, 100.0); (300.0, 300.0); (100.0, 300.0) ] in

  List.iter (fun (x,y) ->
      Printf.printf " (%g, %g)\n" x y;
    ) (poly_clip subject_polygon clip_polygon)
```

{{out}}

```txt
 (100, 116.667)
 (125, 100)
 (275, 100)
 (300, 116.667)
 (300, 300)
 (250, 300)
 (200, 250)
 (175, 300)
 (125, 300)
 (100, 250)
```

We can display the result in a window using the <code>[http://caml.inria.fr/pub/docs/manual-ocaml/libref/Graphics.html Graphics]</code> module:

```ocaml
let subject_polygon =
  [ ( 50.0, 150.0); (200.0,  50.0); (350.0, 150.0);
    (350.0, 300.0); (250.0, 300.0); (200.0, 250.0);
    (150.0, 350.0); (100.0, 250.0); (100.0, 200.0); ]

let clip_polygon =
  [ (100.0, 100.0); (300.0, 100.0); (300.0, 300.0); (100.0, 300.0) ]

let () =
  Graphics.open_graph " 400x400";
  let to_grid poly =
    let round x = int_of_float (floor (x +. 0.5)) in
    Array.map
      (fun (x, y) -> (round x, round y))
      (Array.of_list poly)
  in
  let draw_poly fill stroke poly =
    let p = to_grid poly in
    Graphics.set_color fill;
    Graphics.fill_poly p;
    Graphics.set_color stroke;
    Graphics.draw_poly p;
  in
  draw_poly Graphics.red Graphics.blue subject_polygon;
  draw_poly Graphics.cyan Graphics.blue clip_polygon;
  draw_poly Graphics.magenta Graphics.blue (poly_clip subject_polygon clip_polygon);
  let _ = Graphics.wait_next_event [Graphics.Button_down; Graphics.Key_pressed] in
  Graphics.close_graph ()
```

[[File:SuthHodgClip_OCaml.png]]


## Phix

{{libheader|pGUI}}

```Phix
--
-- demo\rosetta\Sutherland_Hodgman_polygon_clipping.exw
--
enum X,Y

function inside(sequence cp1, sequence cp2, sequence p)
    return (cp2[X]-cp1[X])*(p[Y]-cp1[Y])>(cp2[Y]-cp1[Y])*(p[X]-cp1[X])
end function

function intersection(sequence cp1, sequence cp2, sequence s, sequence e)
atom {dcx,dcy} = {cp1[X]-cp2[X],cp1[Y]-cp2[Y]},
     {dpx,dpy} = {s[X]-e[X],s[Y]-e[Y]},
     n1 = cp1[X]*cp2[Y]-cp1[Y]*cp2[X],
     n2 = s[X]*e[Y]-s[Y]*e[X],
     n3 = 1/(dcx*dpy-dcy*dpx)
    return {(n1*dpx-n2*dcx)*n3,(n1*dpy-n2*dcy)*n3}
end function

function sutherland_hodgman(sequence subjectPolygon, sequence clipPolygon)
sequence cp1, cp2, s, e, inputList, outputList = subjectPolygon
    cp1 = clipPolygon[$]
    for i=1 to length(clipPolygon) do
        cp2 = clipPolygon[i]
        inputList = outputList
        outputList = {}
        s = inputList[$]
        for j=1 to length(inputList) do
            e = inputList[j]
            if inside(cp1,cp2,e) then
                if not inside(cp1,cp2,s) then
                    outputList = append(outputList,intersection(cp1,cp2,s,e))
                end if
                outputList = append(outputList,e)
            elsif inside(cp1,cp2,s) then
                outputList = append(outputList,intersection(cp1,cp2,s,e))
            end if
            s = e
        end for
        cp1 = cp2
    end for
    return outputList
end function

constant subjectPolygon = {{50, 150}, {200, 50}, {350, 150}, {350, 300},
                           {250, 300}, {200, 250}, {150, 350}, {100, 250},
                           {100, 200}},
         clipPolygon = {{100, 100}, {300, 100}, {300, 300}, {100, 300}}

sequence clippedPolygon = sutherland_hodgman(subjectPolygon,clipPolygon)

include pGUI.e

Ihandle dlg, canvas
cdCanvas cddbuffer, cdcanvas

procedure draw_poly(sequence poly)
    cdCanvasBegin(cddbuffer,CD_FILL)
    for i=1 to length(poly) do
        atom {x,y} = poly[i]
        cdCanvasVertex(cddbuffer,x,y)
    end for
    cdCanvasEnd(cddbuffer)
end procedure

function redraw_cb(Ihandle /*ih*/, integer /*posx*/, integer /*posy*/)
    cdCanvasActivate(cddbuffer)
    cdCanvasClear(cddbuffer)
    cdCanvasSetForeground(cddbuffer, CD_CYAN)
    draw_poly(subjectPolygon)
    cdCanvasSetForeground(cddbuffer, CD_MAGENTA)
    draw_poly(clipPolygon)
    cdCanvasSetForeground(cddbuffer, CD_ORANGE)
    draw_poly(clippedPolygon)
    cdCanvasFlush(cddbuffer)
    return IUP_DEFAULT
end function

function map_cb(Ihandle ih)
    cdcanvas = cdCreateCanvas(CD_IUP, ih)
    cddbuffer = cdCreateCanvas(CD_DBUFFER, cdcanvas)
    cdCanvasSetBackground(cddbuffer, CD_WHITE)
    cdCanvasSetForeground(cddbuffer, CD_GRAY)
    return IUP_DEFAULT
end function

function esc_close(Ihandle /*ih*/, atom c)
    if c=K_ESC then return IUP_CLOSE end if
    return IUP_CONTINUE
end function

procedure main()
    IupOpen()

    canvas = IupCanvas(NULL)
    IupSetAttribute(canvas, "RASTERSIZE", "400x400")
    IupSetCallback(canvas, "MAP_CB", Icallback("map_cb"))
    IupSetCallback(canvas, "ACTION", Icallback("redraw_cb"))

    dlg = IupDialog(canvas)
    IupSetAttribute(dlg, "TITLE", "Sutherland-Hodgman polygon clipping")
    IupSetAttribute(dlg, "RESIZE", "NO")
    IupSetCallback(dlg, "K_ANY", Icallback("esc_close"))

    IupShow(dlg)
    IupMainLoop()
    IupClose()
end procedure

main()
```



## PHP


```php

<?php
function clip ($subjectPolygon, $clipPolygon) {

    function inside ($p, $cp1, $cp2) {
        return ($cp2[0]-$cp1[0])*($p[1]-$cp1[1]) > ($cp2[1]-$cp1[1])*($p[0]-$cp1[0]);
    }

    function intersection ($cp1, $cp2, $e, $s) {
        $dc = [ $cp1[0] - $cp2[0], $cp1[1] - $cp2[1] ];
        $dp = [ $s[0] - $e[0], $s[1] - $e[1] ];
        $n1 = $cp1[0] * $cp2[1] - $cp1[1] * $cp2[0];
        $n2 = $s[0] * $e[1] - $s[1] * $e[0];
        $n3 = 1.0 / ($dc[0] * $dp[1] - $dc[1] * $dp[0]);

        return [($n1*$dp[0] - $n2*$dc[0]) * $n3, ($n1*$dp[1] - $n2*$dc[1]) * $n3];
    }

    $outputList = $subjectPolygon;
    $cp1 = end($clipPolygon);
    foreach ($clipPolygon as $cp2) {
        $inputList = $outputList;
        $outputList = [];
        $s = end($inputList);
        foreach ($inputList as $e) {
            if (inside($e, $cp1, $cp2)) {
                if (!inside($s, $cp1, $cp2)) {
                    $outputList[] = intersection($cp1, $cp2, $e, $s);
                }
                $outputList[] = $e;
            }
            else if (inside($s, $cp1, $cp2)) {
                $outputList[] = intersection($cp1, $cp2, $e, $s);
            }
            $s = $e;
        }
        $cp1 = $cp2;
    }
    return $outputList;
}

$subjectPolygon = [[50, 150], [200, 50], [350, 150], [350, 300], [250, 300], [200, 250], [150, 350], [100, 250], [100, 200]];
$clipPolygon = [[100, 100], [300, 100], [300, 300], [100, 300]];
$clippedPolygon = clip($subjectPolygon, $clipPolygon);

echo json_encode($clippedPolygon);
echo "\n";
?>

```



## PureBasic

{{trans|Go}}

```PureBasic
Structure point_f
  x.f
  y.f
EndStructure

Procedure isInside(*p.point_f, *cp1.point_f, *cp2.point_f)
  If (*cp2\x - *cp1\x) * (*p\y - *cp1\y) > (*cp2\y - *cp1\y) * (*p\x - *cp1\x)
    ProcedureReturn 1
  EndIf
EndProcedure

Procedure intersection(*cp1.point_f, *cp2.point_f, *s.point_f, *e.point_f, *newPoint.point_f)
  Protected.point_f dc, dp
  Protected.f n1, n2, n3
  dc\x = *cp1\x - *cp2\x: dc\y = *cp1\y - *cp2\y
  dp\x = *s\x - *e\x: dp\y = *s\y - *e\y
  n1 = *cp1\x * *cp2\y - *cp1\y * *cp2\x
  n2 = *s\x * *e\y - *s\y * *e\x
  n3 = 1 / (dc\x * dp\y - dc\y * dp\x)
  *newPoint\x = (n1 * dp\x - n2 * dc\x) * n3: *newPoint\y = (n1 * dp\y - n2 * dc\y) * n3
EndProcedure

Procedure clip(List vPolygon.point_f(), List vClippedBy.point_f(), List vClippedPolygon.point_f())
  Protected.point_f cp1, cp2, s, e, newPoint
  CopyList(vPolygon(), vClippedPolygon())
  If LastElement(vClippedBy())
    cp1 = vClippedBy()

    NewList vPreClipped.point_f()
    ForEach vClippedBy()
      cp2 = vClippedBy()
      CopyList(vClippedPolygon(), vPreClipped())
      ClearList(vClippedPolygon())
      If LastElement(vPreClipped())
        s = vPreClipped()
        ForEach vPreClipped()
          e = vPreClipped()
          If isInside(e, cp1, cp2)
            If Not isInside(s, cp1, cp2)
              intersection(cp1, cp2, s, e, newPoint)
              AddElement(vClippedPolygon()): vClippedPolygon() = newPoint
            EndIf
            AddElement(vClippedPolygon()): vClippedPolygon() = e
          ElseIf isInside(s, cp1, cp2)
            intersection(cp1, cp2, s, e, newPoint)
            AddElement(vClippedPolygon()): vClippedPolygon() = newPoint
          EndIf
          s = e
        Next
      EndIf
      cp1 = cp2
    Next
  EndIf
EndProcedure

DataSection
  Data.f 50,150, 200,50, 350,150, 350,300, 250,300, 200,250, 150,350, 100,250, 100,200 ;subjectPolygon's vertices (x,y)
  Data.f 100,100, 300,100, 300,300, 100,300 ;clipPolygon's vertices (x,y)
EndDataSection

NewList subjectPolygon.point_f()
For i = 1 To 9
  AddElement(subjectPolygon())
  Read.f subjectPolygon()\x
  Read.f subjectPolygon()\y
Next

NewList clipPolygon.point_f()
For i = 1 To 4
  AddElement(clipPolygon())
  Read.f clipPolygon()\x
  Read.f clipPolygon()\y
Next

NewList newPolygon.point_f()
clip(subjectPolygon(), clipPolygon(), newPolygon())
If OpenConsole()
  ForEach newPolygon()
    PrintN("(" + StrF(newPolygon()\x, 2) + ", " + StrF(newPolygon()\y, 2) + ")")
  Next

  Print(#CRLF$ + #CRLF$ + "Press ENTER to exit"): Input()
  CloseConsole()
EndIf
```

{{out}}

```txt
(100.00, 116.67)
(125.00, 100.00)
(275.00, 100.00)
(300.00, 116.67)
(300.00, 300.00)
(250.00, 300.00)
(200.00, 250.00)
(175.00, 300.00)
(125.00, 300.00)
(100.00, 250.00)
```



## Python


```Python

def clip(subjectPolygon, clipPolygon):
   def inside(p):
      return(cp2[0]-cp1[0])*(p[1]-cp1[1]) > (cp2[1]-cp1[1])*(p[0]-cp1[0])

   def computeIntersection():
      dc = [ cp1[0] - cp2[0], cp1[1] - cp2[1] ]
      dp = [ s[0] - e[0], s[1] - e[1] ]
      n1 = cp1[0] * cp2[1] - cp1[1] * cp2[0]
      n2 = s[0] * e[1] - s[1] * e[0]
      n3 = 1.0 / (dc[0] * dp[1] - dc[1] * dp[0])
      return [(n1*dp[0] - n2*dc[0]) * n3, (n1*dp[1] - n2*dc[1]) * n3]

   outputList = subjectPolygon
   cp1 = clipPolygon[-1]

   for clipVertex in clipPolygon:
      cp2 = clipVertex
      inputList = outputList
      outputList = []
      s = inputList[-1]

      for subjectVertex in inputList:
         e = subjectVertex
         if inside(e):
            if not inside(s):
               outputList.append(computeIntersection())
            outputList.append(e)
         elif inside(s):
            outputList.append(computeIntersection())
         s = e
      cp1 = cp2
   return(outputList)

```



## Racket

Shameless rewrite of haskell version.


```scheme
#lang racket

(module sutherland-hodgman racket
  (provide clip-to)
  (provide make-edges)
  (provide (struct-out point))

  (struct point (x y) #:transparent)
  (struct edge (p1 p2) #:transparent)
  (struct polygon (points edges) #:transparent)

  (define (make-edges points)
    (let ([points-shifted
	   (match points
	     [(list a b ...) (append b (list a))])])
      (map edge points points-shifted)))

  (define (is-point-left? pt ln)
    (match-let ([(point x y) pt]
                [(edge (point px py) (point qx qy)) ln])
               (>= (* (- qx px) (- y py))
                   (* (- qy py) (- x px)))))

  ;; Return the intersection of two lines
  (define (isect-lines l1 l2)
    (match-let ([(edge (point x1 y1) (point x2 y2)) l1]
                [(edge (point x3 y3) (point x4 y4)) l2])
               (let* ([r (- (* x1 y2) (* y1 x2))] [s (- (* x3 y4) (* y3 x4))]
                      [t (- x1 x2)] [u (- y3 y4)] [v (- y1 y2)] [w (- x3 x4)]
                      [d (- (* t u) (* v w))])
                 (point (/ (- (* r w) (* t s)) d)
                        (/ (- (* r u) (* v s)) d)))))

  ;; Intersect the line segment (p0,p1) with the clipping line's left halfspace,
  ;; returning the point closest to p1.  In the special case where p0 lies outside
  ;; the halfspace and p1 lies inside we return both the intersection point and p1.
  ;; This ensures we will have the necessary segment along the clipping line.

  (define (intersect segment clip-line)
    (define (isect) (isect-lines segment clip-line))

    (match-let ([(edge p0 p1) segment])
               (match/values (values (is-point-left? p0 clip-line) (is-point-left? p1 clip-line))
                             [(#f #f) '()]
                             [(#f #t) (list (isect) p1)]
                             [(#t #f) (list (isect))]
                             [(#t #t) (list p1)])))

  ;; Intersect the polygon with the clipping line's left halfspace
  (define (isect-polygon poly-edges clip-line)
    (for/fold ([p '()]) ([e  poly-edges])
      (append p (intersect e clip-line))))

  ;; Intersect a subject polygon with a clipping polygon.  The latter is assumed to be convex.
  (define (clip-to sp-pts cp-edges)
    (for/fold ([out-poly sp-pts]) ([clip-line cp-edges])
      (isect-polygon (make-edges out-poly) clip-line))))
```


----

Testing code (Couldn't find a way to attach image with polygons)

```scheme
(require racket/gui)
(require 'sutherland-hodgman)

(define (make-points pt-list)
    (for/list ([p pt-list])
      (make-object point% (point-x p) (point-y p))))

(define subject-poly-points
    (list (point 50 150)  (point 200 50)  (point 350 150)
          (point 350 300) (point 250 300) (point 200 250)
          (point 150 350) (point 100 250) (point 100 200)))

(define clip-poly-points
    (list (point 100 100)
          (point 300 100)
          (point 300 300)
          (point 100 300)))

(define clip-poly-edges
    (make-edges clip-poly-points))

(define (run)
  (let* ([frame (new frame% [label "Sutherland-Hodgman racket demo"]
		     [width 320]
		     [height 320])]
	 [canvas (new canvas% [parent frame])]
	 [dc (send canvas get-dc)]
         [clipped-poly (clip-to subject-poly-points clip-poly-edges)])

    (send frame show #t)
    (sleep/yield 1)

    (send dc set-pen (make-pen
                        #:color (send the-color-database find-color "Blue")
                        #:width 3))
    (send dc draw-polygon (make-points subject-poly-points))
    (send dc set-pen (make-pen
                        #:color (send the-color-database find-color "Red")
                        #:width 4
                        #:style 'long-dash))
    (send dc draw-polygon (make-points clip-poly-points))
    (send dc set-pen (make-pen
                        #:color (send the-color-database find-color "Green")))
    (send dc set-brush (make-brush
                        #:color (send the-color-database find-color "Green")
                        #:style 'solid))
    (send dc draw-polygon (make-points clipped-poly))
    clipped-poly))

(run)
```


Output:

```scheme
(list
 (point 300 300)
 (point 250 300)
 (point 200 250)
 (point 175 300)
 (point 125 300)
 (point 100 250)
 (point 100 200)
 (point 100 200)
 (point 100 350/3)
 (point 125 100)
 (point 275 100)
 (point 300 350/3))
```



## Ruby

{{trans|Go}}

```ruby
Point = Struct.new(:x,:y) do
  def to_s; "(#{x}, #{y})" end
end

def sutherland_hodgman(subjectPolygon, clipPolygon)
  # These inner functions reduce the argument passing to
  # "inside" and "intersection".
  cp1, cp2, s, e = nil
  inside = proc do |p|
    (cp2.x-cp1.x)*(p.y-cp1.y) > (cp2.y-cp1.y)*(p.x-cp1.x)
  end
  intersection = proc do
    dcx, dcy = cp1.x-cp2.x, cp1.y-cp2.y
    dpx, dpy = s.x-e.x, s.y-e.y
    n1 = cp1.x*cp2.y - cp1.y*cp2.x
    n2 = s.x*e.y - s.y*e.x
    n3 = 1.0 / (dcx*dpy - dcy*dpx)
    Point[(n1*dpx - n2*dcx) * n3, (n1*dpy - n2*dcy) * n3]
  end

  outputList = subjectPolygon
  cp1 = clipPolygon.last
  for cp2 in clipPolygon
    inputList = outputList
    outputList = []
    s = inputList.last
    for e in inputList
      if inside[e]
        outputList << intersection[] unless inside[s]
        outputList << e
      elsif inside[s]
        outputList << intersection[]
      end
      s = e
    end
    cp1 = cp2
  end
  outputList
end

subjectPolygon = [[50, 150], [200, 50], [350, 150], [350, 300],
                  [250, 300], [200, 250], [150, 350], [100, 250],
                  [100, 200]].collect{|pnt| Point[*pnt]}

clipPolygon = [[100, 100], [300, 100], [300, 300], [100, 300]].collect{|pnt| Point[*pnt]}

puts sutherland_hodgman(subjectPolygon, clipPolygon)
```

{{out}}

```txt

(100.0, 116.66666666666667)
(125.00000000000001, 100.0)
(275.0, 100.0)
(300.0, 116.66666666666667)
(300.0, 299.99999999999994)
(250.0, 300.0)
(200, 250)
(175.0, 300.0)
(125.0, 300.0)
(100.0, 250.0)

```



## Rust

{{trans|Ruby}}

```rust
#[derive(Debug, Clone)]
struct Point {
    x: f64,
    y: f64,
}

#[derive(Debug, Clone)]
struct Polygon(Vec<Point>);

fn is_inside(p: &Point, cp1: &Point, cp2: &Point) -> bool {
    (cp2.x - cp1.x) * (p.y - cp1.y) > (cp2.y - cp1.y) * (p.x - cp1.x)
}

fn compute_intersection(cp1: &Point, cp2: &Point, s: &Point, e: &Point) -> Point {
    let dc = Point {
        x: cp1.x - cp2.x,
        y: cp1.y - cp2.y,
    };
    let dp = Point {
        x: s.x - e.x,
        y: s.y - e.y,
    };
    let n1 = cp1.x * cp2.y - cp1.y * cp2.x;
    let n2 = s.x * e.y - s.y * e.x;
    let n3 = 1.0 / (dc.x * dp.y - dc.y * dp.x);
    Point {
        x: (n1 * dp.x - n2 * dc.x) * n3,
        y: (n1 * dp.y - n2 * dc.y) * n3,
    }
}

fn sutherland_hodgman_clip(subject_polygon: &Polygon, clip_polygon: &Polygon) -> Polygon {
    let mut result_ring = subject_polygon.0.clone();
    let mut cp1 = clip_polygon.0.last().unwrap();
    for cp2 in &clip_polygon.0 {
        let input = result_ring;
        let mut s = input.last().unwrap();
        result_ring = vec![];
        for e in &input {
            if is_inside(e, cp1, cp2) {
                if !is_inside(s, cp1, cp2) {
                    result_ring.push(compute_intersection(cp1, cp2, s, e));
                }
                result_ring.push(e.clone());
            } else if is_inside(s, cp1, cp2) {
                result_ring.push(compute_intersection(cp1, cp2, s, e));
            }
            s = e;
        }
        cp1 = cp2;
    }
    Polygon(result_ring)
}

fn main() {
    let _p = |x: f64, y: f64| Point { x, y };
    let subject_polygon = Polygon(vec![
        _p(50.0, 150.0), _p(200.0, 50.0), _p(350.0, 150.0), _p(350.0, 300.0), _p(250.0, 300.0),
        _p(200.0, 250.0), _p(150.0, 350.0), _p(100.0, 250.0), _p(100.0, 200.0),
    ]);
    let clip_polygon = Polygon(vec![
        _p(100.0, 100.0),_p(300.0, 100.0),_p(300.0, 300.0),_p(100.0, 300.0),
    ]);
    let result = sutherland_hodgman_clip(&subject_polygon, &clip_polygon);
    println!("{:?}", result);
}
```

{{out}}

```txt

Polygon([
    Point { x: 100, y: 116.66666666666667 }, Point { x: 125.00000000000001, y: 100 }, Point { x: 275, y: 100 },
    Point { x: 300, y: 116.66666666666667 }, Point { x: 300, y: 299.99999999999994 }, Point { x: 250, y: 300 },
    Point { x: 200, y: 250 }, Point { x: 175, y: 300 }, Point { x: 125, y: 300 }, Point { x: 100, y: 250 }])

```



## Scala

From Java snippet.

```scala
import javax.swing.{ JFrame, JPanel }

object SutherlandHodgman extends JFrame with App {
    import java.awt.BorderLayout

    setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
    setVisible(true)
    val content = getContentPane()
    content.setLayout(new BorderLayout())
    content.add(SutherlandHodgmanPanel, BorderLayout.CENTER)
    setTitle("SutherlandHodgman")
    pack()
    setLocationRelativeTo(null)
}

object SutherlandHodgmanPanel extends JPanel {
    import java.awt.{ Color, Graphics, Graphics2D }

    setPreferredSize(new java.awt.Dimension(600, 500))

    // subject and clip points are assumed to be valid
    val subject = Seq((50D, 150D), (200D, 50D), (350D, 150D), (350D, 300D), (250D, 300D), (200D, 250D), (150D, 350D), (100D, 250D), (100D, 200D))
    val clipper = Seq((100D, 100D), (300D, 100D), (300D, 300D), (100D, 300D))
    var result = subject

    val len = clipper.size
    for (i <- 0 until len) {
        val len2 = result.size
        val input = result
        result = Seq()

        val A = clipper((i + len - 1) % len)
        val B = clipper(i)

        for (j <- 0 until len2) {
            val P = input((j + len2 - 1) % len2)
            val Q = input(j)

            if (inside(A, B, Q)) {
                if (!inside(A, B, P))
                    result = result :+ intersection(A, B, P, Q)
                result = result :+ Q
            }
            else if (inside(A, B, P))
                result = result :+ intersection(A, B, P, Q)
        }
    }

    override def paintComponent(g: Graphics) {
        import java.awt.RenderingHints._

        super.paintComponent(g)
        val g2 = g.asInstanceOf[Graphics2D]
        g2.translate(80, 60)
        g2.setStroke(new java.awt.BasicStroke(3))
        g2.setRenderingHint(KEY_ANTIALIASING, VALUE_ANTIALIAS_ON)
        g2.draw_polygon(subject, Color.blue)
        g2.draw_polygon(clipper, Color.red)
        g2.draw_polygon(result, Color.green)
    }

    private def inside(a: (Double, Double), b: (Double, Double), c: (Double, Double)) =
        (a._1 - c._1) * (b._2 - c._2) > (a._2 - c._2) * (b._1 - c._1)

    private def intersection(a: (Double, Double), b: (Double, Double), p: (Double, Double), q: (Double, Double)) = {
        val A1 = b._2 - a._2
        val B1 = a._1 - b._1
        val C1 = A1 * a._1 + B1 * a._2
        val A2 = q._2 - p._2
        val B2 = p._1 - q._1
        val C2 = A2 * p._1 + B2 * p._2

        val det = A1 * B2 - A2 * B1
        ((B2 * C1 - B1 * C2) / det, (A1 * C2 - A2 * C1) / det)
    }

    private implicit final class Polygon_drawing(g: Graphics2D) {
        def draw_polygon(points: Seq[(Double, Double)], color: Color) {
            g.setColor(color)
            val len = points.length
            val line = new java.awt.geom.Line2D.Double()
            for (i <- 0 until len) {
                val p1 = points(i)
                val p2 = points((i + 1) % len)
                line.setLine(p1._1, p1._2, p2._1, p2._2)
                g.draw(line)
            }
        }
    }
}
```



## Sidef

{{trans|Ruby}}

```ruby
class Point(x, y) {
    method to_s {
        "(#{'%.2f' % x}, #{'%.2f' % y})"
    }
}

func sutherland_hodgman(subjectPolygon, clipPolygon) {
  var inside = { |cp1, cp2, p|
    ((cp2.x-cp1.x)*(p.y-cp1.y)) > ((cp2.y-cp1.y)*(p.x-cp1.x))
  }

  var intersection = { |cp1, cp2, s, e|
    var (dcx, dcy) = (cp1.x-cp2.x, cp1.y-cp2.y)
    var (dpx, dpy) = (s.x-e.x, s.y-e.y)
    var n1 = (cp1.x*cp2.y - cp1.y*cp2.x)
    var n2 = (s.x*e.y - s.y*e.x)
    var n3 = (1 / (dcx*dpy - dcy*dpx))
    Point((n1*dpx - n2*dcx) * n3, (n1*dpy - n2*dcy) * n3)
  }

  var outputList = subjectPolygon
  var cp1 = clipPolygon.last
  for cp2 in clipPolygon {
    var inputList = outputList
    outputList = []
    var s = inputList.last
    for e in inputList {
      if (inside(cp1, cp2, e)) {
        outputList << intersection(cp1, cp2, s, e) if !inside(cp1, cp2, s)
        outputList << e
      }
      elsif(inside(cp1, cp2, s)) {
        outputList << intersection(cp1, cp2, s, e)
      }
      s = e
    }
    cp1 = cp2
  }
  outputList
}

var subjectPolygon = [
    [50,  150], [200,  50], [350, 150], [350, 300],
    [250, 300], [200, 250], [150, 350], [100, 250],
    [100, 200]
].map{|pnt| Point(pnt...) }

var clipPolygon = [
    [100, 100], [300, 100],
    [300, 300], [100, 300]
].map{|pnt| Point(pnt...) }

sutherland_hodgman(subjectPolygon, clipPolygon).each { .say }
```

{{out}}

```txt

(100.00, 116.67)
(125.00, 100.00)
(275.00, 100.00)
(300.00, 116.67)
(300.00, 300.00)
(250.00, 300.00)
(200.00, 250.00)
(175.00, 300.00)
(125.00, 300.00)
(100.00, 250.00)

```



## Tcl


```tcl
# Find intersection of an arbitrary polygon with a convex one.
package require Tcl 8.6

#	Does the path (x0,y0)->(x1,y1)->(x2,y2) turn clockwise
#	or counterclockwise?
proc cw {x0 y0 x1 y1 x2 y2} {
    set dx1 [expr {$x1 - $x0}]; set dy1 [expr {$y1 - $y0}]
    set dx2 [expr {$x2 - $x0}]; set dy2 [expr {$y2 - $y0}]
    # (0,0,$dx1*$dy2 - $dx2*$dy1) is the crossproduct of
    # ($x1-$x0,$y1-$y0,0) and ($x2-$x0,$y2-$y0,0).
    # Its z-component is positive if the turn
    # is clockwise, negative if the turn is counterclockwise.
    set pr1 [expr {$dx1 * $dy2}]
    set pr2 [expr {$dx2 * $dy1}]
    if {$pr1 > $pr2} {
	# Clockwise
	return 1
    } elseif {$pr1 < $pr2} {
	# Counter-clockwise
	return -1
    } elseif {$dx1*$dx2 < 0 || $dy1*$dy2 < 0} {
	# point 0 is the middle point
	return 0
    } elseif {($dx1*$dx1 + $dy1*$dy1) < ($dx2*$dx2 + $dy2+$dy2)} {
	# point 1 is the middle point
	return 0
    } else {
	# point 2 lies on the segment joining points 0 and 1
	return 1
    }
}

#	Calculate the point of intersection of two lines
#	containing the line segments (x1,y1)-(x2,y2) and (x3,y3)-(x4,y4)
proc intersect {x1 y1 x2 y2 x3 y3 x4 y4} {
    set d [expr {($y4 - $y3) * ($x2 - $x1) - ($x4 - $x3) * ($y2 - $y1)}]
    set na [expr {($x4 - $x3) * ($y1 - $y3) - ($y4 - $y3) * ($x1 - $x3)}]
    if {$d == 0} {
	return {}
    }
    set r [list \
	    [expr {$x1 + $na * ($x2 - $x1) / $d}] \
	    [expr {$y1 + $na * ($y2 - $y1) / $d}]]
    return $r
}

#	Coroutine that yields the elements of a list in pairs
proc pairs {list} {
    yield [info coroutine]
    foreach {x y} $list {
	yield [list $x $y]
    }
    return {}
}

#	Coroutine to clip one segment of a polygon against a line.
proc clipsegment {inside0 cx0 cy0 cx1 cy1 sx0 sy0 sx1 sy1} {
    set inside1 [expr {[cw $cx0 $cy0 $cx1 $cy1 $sx1 $sy1] > 0}]
    if {$inside1} {
	if {!$inside0} {
	    set int [intersect $cx0 $cy0 $cx1 $cy1 \
		    $sx0 $sy0 $sx1 $sy1]
	    if {[llength $int] >= 0} {
		yield $int
	    }
	}
	yield [list $sx1 $sy1]
    } else {
	if {$inside0} {
	    set int [intersect $cx0 $cy0 $cx1 $cy1 \
		    $sx0 $sy0 $sx1 $sy1]
	    if {[llength $int] >= 0} {
		yield $int
	    }
	}
    }
    return $inside1
}

#	Coroutine to perform one step of Sutherland-Hodgman polygon clipping
proc clipstep {source cx0 cy0 cx1 cy1} {
    yield [info coroutine]
    set pt0 [{*}$source]
    if {[llength $pt0] == 0} {
	return
    }
    lassign $pt0 sx0 sy0
    set inside0 [expr {[cw $cx0 $cy0 $cx1 $cy1 $sx0 $sy0] > 0}]
    set finished 0
    while {!$finished} {
	set thispt [{*}$source]
	if {[llength $thispt] == 0} {
	    set thispt $pt0
	    set finished 1
	}
	lassign $thispt sx1 sy1
	set inside0 [clipsegment $inside0 \
		$cx0 $cy0 $cx1 $cy1 $sx0 $sy0 $sx1 $sy1]
	set sx0 $sx1
	set sy0 $sy1
    }
    return {}
}

#	Perform Sutherland-Hodgman polygon clipping
proc clippoly {cpoly spoly} {
    variable clipindx
    set source [coroutine clipper[incr clipindx] pairs $spoly]
    set cx0 [lindex $cpoly end-1]
    set cy0 [lindex $cpoly end]
    foreach {cx1 cy1} $cpoly {
	set source [coroutine clipper[incr clipindx] \
		clipstep $source $cx0 $cy0 $cx1 $cy1]
	set cx0 $cx1; set cy0 $cy1
    }
    set result {}
    while {[llength [set pt [{*}$source]]] > 0} {
	lappend result {*}$pt
    }
    return $result
}
```


The specifics of the task:
{{libheader|Tk}}

```tcl
package require Tk

grid [canvas .c -width 400 -height 400 -background \#ffffff]
proc demonstrate {cpoly spoly} {
    set rpoly [clippoly $cpoly $spoly]
    puts $rpoly
    .c create polygon $cpoly -outline \#ff9999 -fill {} -width 5
    .c create polygon $spoly -outline \#9999ff -fill {} -width 3
    .c create polygon $rpoly -fill \#99ff99 -outline black -width 1
}

demonstrate {100 100 300 100 300 300 100 300} \
    {50 150 200 50 350 150 350 300 250 300 200 250 150 350 100 250 100 200}
```

{{out}}

```txt

300 116 300 300 250 300 200 250 175 300 125 300 100 250 100 200 100 200 100 116 124 100 275 100

```

[[File:Sutherland-Hodgman.gif]]


## Yabasic

{{trans|BBC BASIC}}

```Yabasic

open window 400, 400
backcolor 0,0,0
clear window

DPOL = 8
DREC = 3
CX = 1 : CY = 2

dim poligono(DPOL, 2)
dim rectang(DREC, 2)
dim clipped(DPOL + DREC, 2)

for n = 0 to DPOL : read poligono(n, CX), poligono(n, CY) : next n
DATA 50,150, 200,50, 350,150, 350,300, 250,300, 200,250, 150,350, 100,250, 100,200
for n = 0 to DREC : read rectang(n, CX), rectang(n, CY) : next n
DATA 100,100, 300,100, 300,300, 100,300


color 255,0,0
dibuja(poligono(), DPOL)
color 0,0,255
dibuja(rectang(), DREC)

nvert = FNsutherland_hodgman(poligono(), rectang(), clipped(), DPOL + DREC)
color 250,250,0
dibuja(clipped(), nvert - 1)


sub dibuja(figura(), i)
	local n

	print
	new curve
	for n = 0 to i
		line to figura(n, CX), figura(n, CY)
		print figura(n, CX), ", ", figura(n, CY)
	next n
	close curve
end sub


sub FNsutherland_hodgman(subj(), clip(), out(), n)
	local i, j, o, tclip, p1(2), p2(2), s(2), e(2), p(2), inp(n, 2)

	FOR o = 0 TO arraysize(subj(), 1) : out(o, CX) = subj(o, CX) : out(o, CY) = subj(o, CY) : NEXT o

	tclip = arraysize(clip(),1)
	p1(CX) = clip(tclip, CX) : p1(CY) = clip(tclip, CY)

	FOR i = 0 TO tclip
	    p2(CX) = clip(i, CX) : p2(CY) = clip(i, CY)
	    FOR n = 0 TO o - 1 : inp(n, CX) = out(n, CX) : inp(n, CY) = out(n, CY) : NEXT n : o = 0
	  	IF n >= 2 THEN
	            s(CX) = inp(n - 1, CX) : s(CY) = inp(n - 1, CY)

	    	    FOR j = 0 TO n - 1
	      		e(CX) = inp(j, CX) : e(CY) = inp(j, CY)
	      		IF FNside(e(), p1(), p2()) THEN
	        		IF NOT FNside(s(), p1(), p2()) THEN
	          			PROCintersection(p1(), p2(), s(), e(), p())
	          			out(o, CX) = round(p(CX)) : out(o, CY) = round(p(CY))
	          			o = o + 1
	        		ENDIF
	        		out(o, CX) = round(e(CX)) : out(o, CY) = round(e(CY))
	        		o = o + 1
	      		ELSE
	        		IF FNside(s(), p1(), p2()) THEN
	          			PROCintersection(p1(), p2(), s(), e(), p())
	          			out(o, CX) = round(p(CX)) : out(o, CY) = round(p(CY))
	          			o = o + 1
	        		ENDIF
	      		ENDIF
	      		s(CX) = e(CX) : s(CY) = e(CY)
	    	    NEXT j
	  	ENDIF
	  	p1(CX) = p2(CX) : p1(CY) = p2(CY)
	NEXT i
	return o
end sub


sub FNside(p(), p1(), p2())
	return  (p2(CX) - p1(CX)) * (p(CY) - p1(CY)) > (p2(CY) - p1(CY)) * (p(CX) - p1(CX))
end sub


sub PROCintersection(p1(), p2(), p3(), p4(), p())
	LOCAL a(2), b(2), k, l, m

	a(CX) = p1(CX) - p2(CX) : a(CY) = p1(CY) - p2(CY)
	b(CX) = p3(CX) - p4(CX) : b(CY) = p3(CY) - p4(CY)
	k = p1(CX) * p2(CY) - p1(CY) * p2(CX)
	l = p3(CX) * p4(CY) - p3(CY) * p4(CX)
	m = 1 / (a(CX) * b(CY) - a(CY) * b(CX))
	p(CX) =  m * (k * b(CX) - l * a(CX))
	p(CY) =  m * (k * b(CY) - l * a(CY))

end sub


sub round(n)
	return int(n + .5)
end sub
```



## zkl

{{trans|C}}{{trans|Wikipedia}}
Uses the PPM class from http://rosettacode.org/wiki/Bitmap/Bresenham%27s_line_algorithm#zkl

```zkl
class P{	// point
   fcn init(_x,_y){ var [const] x=_x.toFloat(), y=_y.toFloat() }
   fcn __opSub(p) { self(x - p.x, y - p.y) }
   fcn cross(p)   { x*p.y - y*p.x          }
   fcn toString   { "(%7.2f,%7.2f)".fmt(x,y) }
   var [const,proxy] ps=fcn{ T(x.toInt(),y.toInt()) };    // property
}
fcn shClipping(clip,polygon){
   inputList,outputList,clipEdge:=List(), polygon.copy(), List(Void,clip[-1]);
   foreach p in (clip){
      clipEdge.del(0).append(p);
      inputList.clear().extend(outputList);
      outputList.clear();
      S:=inputList[-1];
      foreach E in (inputList){
         if(leftOf(clipEdge,E)){
	    if(not leftOf(clipEdge,S))
	       outputList.append(intersection(S,E,clipEdge));
	    outputList.append(E);
	 }
	 else if(leftOf(clipEdge,S))
	         outputList.append(intersection(S,E,clipEdge));
	 S=E;
      }
   }
   outputList
}
fcn leftOf(line,p){ //-->True (p is left of line), direction of line matters
   p1,p2:=line;		// line is (p1,p2)
   (p2-p1).cross(p-p2)>0;
}
fcn intersection(p1,p2, line){	//-->Point of intersection or False
   p3,p4:=line;
   dx,dy,d:=p2-p1, p3-p4, p1-p3;
   // x0 + a dx = y0 + b dy ->
   // x0 X dx = y0 X dx + b dy X dx ->
   // b = (x0 - y0) X dx / (dy X dx)
   dyx:=dy.cross(dx);
   if(not dyx) return(False);  // parallel lines, could just throw on next line
   dyx=d.cross(dx)/dyx;
   P(p3.x + dyx*dy.x, p3.y + dyx*dy.y);
}
fcn drawPolygon(ppm,listOfPoints,rgb){
   foreach n in (listOfPoints.len()-1){
      ppm.line(listOfPoints[n].ps.xplode(),listOfPoints[n+1].ps.xplode(),rgb);
   }
   ppm.line(listOfPoints[0].ps.xplode(),listOfPoints[-1].ps.xplode(),rgb);
}
```


```zkl
ppm:=PPM(400,400);
clip:=T( P(100,100), P(300,100), P(300,300), P(100,300) );
polygon:=T( P( 50,150),P(200, 50),P(350,150),
	    P(350,300),P(250,300),P(200,250),
	    P(150,350),P(100,250),P(100,200) );
drawPolygon(ppm,polygon,0x0000ff);	// blue: polygon
ppm.flood(200,200,0x000030);
drawPolygon(ppm,clip,0xff0000);		// red:  clip region

clipped:=shClipping(clip,polygon);
drawPolygon(ppm,clipped,0x00ff00);	// green: clipped polygon
ppm.flood(200,200,0x003000);		// which is the clipped region anyway
clipped.apply('wrap(p){ ppm.cross(p.ps.xplode(),0x00ff00) }); // mark vertices

ppm.writeJPGFile("sutherland_hodgman.zkl.jpg");

println("Clipped polygon has ",clipped.len()," points:");
clipped.pump(Console.println);
```

{{out}}
Until local image uploading is re-enabled, see [http://www.zenkinetic.com/Images/RosettaCode/sutherland_hodgman.zkl.jpg this image].

```txt

Clipped polygon has 10 points:
( 100.00, 116.67)
( 125.00, 100.00)
( 275.00, 100.00)
( 300.00, 116.67)
( 300.00, 300.00)
( 250.00, 300.00)
( 200.00, 250.00)
( 175.00, 300.00)
( 125.00, 300.00)
( 100.00, 250.00)

```



[[Category:Geometry]]
