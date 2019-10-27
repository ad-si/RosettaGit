+++
title = "Talk:Ray-casting algorithm"
description = ""
date = 2018-12-04T11:23:59Z
aliases = []
[extra]
id = 4247
[taxonomies]
categories = []
tags = []
+++

==Fortran: The point of pts?==
What is pts used for in the Fortran example? --[[User:Paddy3118|Paddy3118]] 01:17, 26 May 2009 (UTC)
:Ah, It seems there are one set of points for all polygons. --[[User:Paddy3118|Paddy3118]] 01:41, 26 May 2009 (UTC)
:: Yes, I preferred to use a single set, and to index it (it's not an odd technics if a subset of points is shared among several polygons we want to define). --[[User:ShinTakezou|ShinTakezou]] 16:53, 26 May 2009 (UTC)

== AutoHotkey version ==

The AutoHotkey solution refers to ray_intersects_segment without defining it, but it seems to me that defining ray_intersects_segment is a key part of this task. --[[User:Mr2001|Mr2001]] 07:31, 31 October 2010 (UTC)
: [[Help:ENA Templates]] will be helpful for flagging this kind of thing. --[[User:Short Circuit|Michael Mol]] 13:23, 31 October 2010 (UTC)

== "Coherent results" ==

The C example claims to "reveal the meaning of coherent results", whereby it considers a point lying on the left edge of a square outside, but a point on right side inside.  This must be a meaning of "coherent" that I wasn't aware of.  Also, the intersection code is terrible: it uses an arbitrarily defined value as a floating point precision limit while not rigorously enforcing it, which will cause all kinds of unexpected behaviors when used in real work. --[[User:Ledrug|Ledrug]] 22:10, 25 July 2011 (UTC)

: You clearly failed to understand the actual meaning and also to cite correctly the text (<cite>this is italics on purpose</cite>). The text under C code '''correctly''' claimed to "reveal the meaning of <cite>coherent results</cite>". It referred to the text of the problem, where it was and it is stated: <cite>Points on the boundary or "on" a vertex are someway special and through this approach we do not obtain ''coherent results''.</cite> What does it mean that we don't obtain <cite>coherent results</cite>? The C code showed it, as expected and anticipated by <cite>through this approach</cite>, which was clearly the approach used in the implementation. So, it wasn't a dictionary problem on my part, but an understanding problem on your part.
 
: There was a second incredible misunderstanding: in the "original" C code, there wasn't anything to deal with <cite>a floating point precision limit</cite>, hence it was impossible such a limit was enforced rigorously (or not) anywhere. Clearly you failed to understand what the code did — and this is fine, maybe it was sloppy and messy to you. Nonetheless, the text of the problem says <cite>To avoid the "ray on vertex" problem, the point is moved upward of a small quantity ε</cite>. Maybe you missed this part, or you believed that every ε/eps/epsilon must be used to enforce <cite>a floating point precision limit</cite>.

: Instead in these cases (check other examples) ε/eps/epsilon is just the value we shift a point in order to '''simplistically''' cope with a specific problem. Not the best we can do, and it has "side effects": already considered in the text, thus no surprise (except for you, it seems).

: If we need to be pedantic and focus on all the details for a production ready implementation (which isn't the main aim of this wiki, as far as I understand it), floating point should be treated carefully in every comparisons, but many did the naive approach — I think because it isn't the point of the task (or maybe because we hope the target CPU has an instruction like FCMPE in MMIX — just joking). --- [[User:ShinTakezou|ShinTakezou]] ([[User talk:ShinTakezou|talk]]) 18:59, 23 February 2018 (UTC)

== D code generate false positive ==
We port the D code in our C++ project and we found false positive, I almost sure all other implementations could have exactly the same issue.

Here is my modifications of the D code showing the issue (show comments) :

```d

import std.stdio, std.math, std.algorithm;

immutable struct Point { float x, y; }                          // Using float instead of double (reducing precision type)
immutable struct Edge { Point a, b; }
immutable struct Figure {
    string name;
    Edge[] edges;
}

bool contains(in Figure poly, in Point p) pure nothrow @safe
{
    static bool raySegI(in Point p, in Edge edge)
		pure nothrow @safe
	{
			enum float epsilon = 0.001;                                                // Increase error tolerance
			with (edge)
			{
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
				immutable blue = (abs(a.x - p.x) > float.min_normal) ?
					((p.y - a.y) / (p.x - a.x)) :
				float.max;
				immutable red = (abs(a.x - b.x) > float.min_normal) ?
					((b.y - a.y) / (b.x - a.x)) :
				float.max;
				return blue >= red;
			}
		}

    return poly.edges.count!(e => raySegI(p, e)) % 2;
}

void main()
{
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
			{{ 3.0, 10.0}, { 0.0,  5.0}},  {{ 0.0,  5.0}, { 3.0,  0.0}}]},
		{"Strange2", [                                                                                  // Our testing polygon
			{{-0.047600,3.619000},{-0.147595,3.618007}},
			{{-0.147595,3.618007},{-0.111895,0.022807}},
			{{-0.111895,0.022807},{-0.011900,0.023800}},
			{{-0.011900,0.023800},{0.088095,0.024793}},
			{{0.088095,0.024793},{0.052395,3.619993}},
			{{0.052395,3.619993},{-0.047600,3.619000}}]}
	];

    immutable Point[] testPoints = [{ 5, 5}, {5, 8}, {-10,  5}, {0, 5}, {10, 5}, {8, 5}, { 10, 10}, {-5.0,0.0238}];             // Last point need to be out of Strange2 polygon, but the result is true

    foreach (immutable poly; polys)
	{
        writefln(`Is point inside figure "%s"?`, poly.name);
        foreach (immutable p; testPoints)
            writefln("  (%3s, %2s): %s", p.x, p.y, contains(poly, p));
        writeln;
    }
	readln();
}

```


To see our polygon and test point you can use geogebra web :
  * go to http://www.geogebra.org/webstart/geogebra.html
  * enter in the input field : (-5.000000f,0.023800f)
  * enter in the input field : Polygon[(-0.047600,3.619000),(-0.147595,3.618007),(-0.111895,0.022807),(-0.011900,0.023800),(0.088095,0.024793),(0.052395,3.619993)]


We fix this issue by shifting the y coordinate of input point (adding epsilon) while it match with a vertex.
Notice we are shifting the point before testing it against any segment, this increase the stability in the computation of the number of intersections.

Here is our working C++ implementation :

```c++

    template<class T, class Alloc>
    bool Polygon2<T, Alloc>::contains(const Vector2<T>& point, T epsilon) const
    {
        H3D_ASSERT(this->size() > 1);
        
        Vector2<T> currPt;
        // Insure point is not equal to one of the vertex
        Vector2<T> shiftedPoint(point);
        for (auto it = this->begin(); it != this->end();)
        {
            currPt = *it;
            
            if (math::epsilonEquals(currPt.y, shiftedPoint.y, epsilon))
            {
                shiftedPoint.y += epsilon;
                it = this->begin(); // Shift the point and recheck all the vertex (we could have shifted the point on a previous vertex)
            }
            else
            {
                ++it;
            }
        }
        
        int count = 0;
        Vector2<T> lastPt = this->back();
        for (auto it = this->begin(); it != this->end(); ++it)
        {
            currPt = *it;
            
            if (raySegI(shiftedPoint, lastPt, currPt, epsilon))
                ++count;
            
            lastPt = currPt;
        }
        return (bool)(count % 2);
    }
	
    template<class T, class Alloc>
    bool Polygon2<T, Alloc>::raySegI(const Vector2<T>& p, const Vector2<T>& a, const Vector2<T>& b, T epsilon) const
    {
        if (a.y > b.y)
            return raySegI(p, b, a, epsilon);
        H3D_ASSERT(math::epsilonEquals(p.y, a.y, epsilon) == false && math::epsilonEquals(p.y, b.y, epsilon) == false);
        if (p.y > b.y || p.y < a.y || p.x > std::max(a.x, b.x))
            return false;
        if (p.x < std::min(a.x, b.x))
            return true;
        T blue = (abs(a.x - p.x) > std::numeric_limits<T>::min()) ?
                     ((p.y - a.y) / (p.x - a.x)) :
                     std::numeric_limits<T>::max();
        T red = (abs(a.x - b.x) > std::numeric_limits<T>::min()) ?
                    ((b.y - a.y) / (b.x - a.x)) :
                    std::numeric_limits<T>::max();
        return blue >= red;
    }


```


== Elixir ==

point_in_polygon.ex:

```Elixir
defmodule PointInPolygon do
  require Integer

  @doc """
  Check if point is inside a polygon

  ## Example
      iex> polygon = [[1, 2], [3, 4], [5, 2], [3, 0]]
      iex> point = [3, 2]
      iex> point_in_polygon?(polygon, point)
      true

  ## Example
      iex> polygon = [[1, 2], [3, 4], [5, 2], [3, 0]]
      iex> point = [1.5, 3]
      iex> point_in_polygon?(polygon, point)
      false

  """
  def point_in_polygon?(polygon, point) do
    polygon
    |> to_segments()
    |> Enum.reduce(0, fn segment, count ->
      apply(__MODULE__, :ray_intersects_segment, add_epsilon(segment, point)) + count
    end)
    |> Integer.is_odd()
  end

  def to_segments([p1 | _] = polygon) do
    polygon |> Enum.chunk_every(2, 1, [p1]) |> Enum.map(fn segment -> orient_segment(segment) end)
  end

  def orient_segment([a = [_ax, ay], b = [_bx, by]]) when by >= ay do
    [a, b]
  end

  def orient_segment([b, a]) do
    [a, b]
  end

  def add_epsilon(segment = [[_ax, ay], [_bx, by]], [px, py]) when py == ay or py == by do
    [segment, [px, py + 0.00000001]]
  end

  def add_epsilon(segment, point), do: [segment, point]

  def ray_intersects_segment([[_ax, ay], [_bx, by]], [_px, py]) when py < ay or py > by do
    0
  end

  # px >= max(ax, bx)
  def ray_intersects_segment([[ax, _ay], [bx, _by]], [px, _py])
      when (ax >= bx and px >= ax) or (bx >= ax and px >= bx) do
    0
  end

  # px < min(ax, bx)
  def ray_intersects_segment([[ax, _ay], [bx, _by]], [px, _py])
      when (ax <= bx and px < ax) or (bx <= ax and px < bx) do
    1
  end

  def ray_intersects_segment([[ax, ay], [bx, by]], [px, py]) do
    m_red = m_red(ax, ay, bx, by)
    m_blue = m_blue(ax, ay, px, py)

    case {m_blue, m_red} do
      {:infinity, _} ->
        1

      {_, :infinity} ->
        0

      {m_blue, m_red} when m_blue >= m_red ->
        1

      _ ->
        0
    end
  end

  def m_red(ax, ay, bx, by) when ax != bx do
    (by - ay) / (bx - ax)
  end

  def m_red(_, _, _, _) do
    :infinity
  end

  def m_blue(ax, ay, px, py) when ax != px do
    (py - ay) / (px - ax)
  end

  def m_blue(_, _, _, _) do
    :infinity
  end
end
```

