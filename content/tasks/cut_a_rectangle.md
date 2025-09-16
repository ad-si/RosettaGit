+++
title = "Cut a rectangle"
description = ""
date = 2018-11-11T04:28:21Z
aliases = []
[extra]
id = 10636
[taxonomies]
categories = ["task"]
tags = []
languages = [
  "c",
  "common_lisp",
  "d",
  "eiffel",
  "elixir",
  "go",
  "haskell",
  "j",
  "java",
  "julia",
  "kotlin",
  "perl",
  "perl_6",
  "phix",
  "python",
  "racket",
  "rexx",
  "ruby",
  "rust",
  "tcl",
  "zkl",
]
+++

A given rectangle is made from ''m'' × ''n'' squares. If ''m'' and ''n'' are not both odd, then it is possible to cut a path through the rectangle along the square edges such that the rectangle splits into two connected pieces with the same shape (after rotating one of the pieces by 180°). All such paths for 2 × 2 and 4 × 3 rectangles are shown below.

[[file:rect-cut.svg]]

Write a program that calculates the number of different ways to cut an ''m'' × ''n'' rectangle.  Optionally, show each of the cuts.

Possibly related task: [[Maze generation]] for depth-first search.


## C

Exhaustive search on the cutting path.  Symmetric configurations are only calculated once, which helps with larger sized grids.

```c
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef unsigned char byte;
byte *grid = 0;

int w, h, len;
unsigned long long cnt;

static int next[4], dir[4][2] = {{0, -1}, {-1, 0}, {0, 1}, {1, 0}};
void walk(int y, int x)
{
	int i, t;

	if (!y || y == h || !x || x == w) {
		cnt += 2;
		return;
	}

	t = y * (w + 1) + x;
	grid[t]++, grid[len - t]++;

	for (i = 0; i < 4; i++)
		if (!grid[t + next[i]])
			walk(y + dir[i][0], x + dir[i][1]);

	grid[t]--, grid[len - t]--;
}

unsigned long long solve(int hh, int ww, int recur)
{
	int t, cx, cy, x;

	h = hh, w = ww;

	if (h & 1) t = w, w = h, h = t;
	if (h & 1) return 0;
	if (w == 1) return 1;
	if (w == 2) return h;
	if (h == 2) return w;

	cy = h / 2, cx = w / 2;

	len = (h + 1) * (w + 1);
	grid = realloc(grid, len);
	memset(grid, 0, len--);

	next[0] = -1;
	next[1] = -w - 1;
	next[2] = 1;
	next[3] = w + 1;

	if (recur) cnt = 0;
	for (x = cx + 1; x < w; x++) {
		t = cy * (w + 1) + x;
		grid[t] = 1;
		grid[len - t] = 1;
		walk(cy - 1, x);
	}
	cnt++;

	if (h == w)
		cnt *= 2;
	else if (!(w & 1) && recur)
		solve(w, h, 0);

	return cnt;
}

int main()
{
	int y, x;
	for (y = 1; y <= 10; y++)
		for (x = 1; x <= y; x++)
			if (!(x & 1) || !(y & 1))
				printf("%d x %d: %llu\n", y, x, solve(y, x, 1));

	return 0;
}
```
output<lang>2 x 1: 1
2 x 2: 2
3 x 2: 3
4 x 1: 1
4 x 2: 4
4 x 3: 9
4 x 4: 22
5 x 2: 5
5 x 4: 39
6 x 1: 1
6 x 2: 6
6 x 3: 23
6 x 4: 90
6 x 5: 263
6 x 6: 1018
7 x 2: 7
7 x 4: 151
7 x 6: 2947
8 x 1: 1
8 x 2: 8
8 x 3: 53
8 x 4: 340
8 x 5: 1675
8 x 6: 11174
8 x 7: 55939
8 x 8: 369050
9 x 2: 9
9 x 4: 553
9 x 6: 31721
9 x 8: 1812667
10 x 1: 1
10 x 2: 10
10 x 3: 115
10 x 4: 1228
10 x 5: 10295
10 x 6: 118276
10 x 7: 1026005
10 x 8: 11736888
10 x 9: 99953769
10 x 10: 1124140214
```


More awkward solution: after compiling, run <code>./a.out -v [width] [height]</code> for display of cuts.

```c
#include <stdio.h>
#include <stdlib.h>

typedef unsigned char byte;
int w = 0, h = 0, verbose = 0;
unsigned long count = 0;

byte **hor, **ver, **vis;
byte **c = 0;

enum { U = 1, D = 2, L = 4, R = 8 };

byte ** alloc2(int w, int h)
{
	int i;
	byte **x = calloc(1, sizeof(byte*) * h + h * w);
	x[0] = (byte *)&x[h];
	for (i = 1; i < h; i++)
		x[i] = x[i - 1] + w;
	return x;
}

void show()
{
	int i, j, v, last_v;
	printf("%ld\n", count);
#if 0
	for (i = 0; i <= h; i++) {
		for (j = 0; j <= w; j++)
			printf("%d ", hor[i][j]);
		puts("");
	}
	puts("");

	for (i = 0; i <= h; i++) {
		for (j = 0; j <= w; j++)
			printf("%d ", ver[i][j]);
		puts("");
	}
	puts("");
#endif
	for (i = 0; i < h; i++) {
		if (!i) v = last_v = 0;
		else last_v = v = hor[i][0] ? !last_v : last_v;

		for (j = 0; j < w; v = ver[i][++j] ? !v : v)
			printf(v ? "\033[31m[]" : "\033[33m{}");
		puts("\033[m");
	}
	putchar('\n');
}

void walk(int y, int x)
{
	if (x < 0 || y < 0 || x > w || y > h) return;

	if (!x || !y || x == w || y == h) {
		++count;
		if (verbose) show();
		return;
	}

	if (vis[y][x]) return;
	vis[y][x]++; vis[h - y][w - x]++;

	if (x && !hor[y][x - 1]) {
		hor[y][x - 1] = hor[h - y][w - x] = 1;
		walk(y, x - 1);
		hor[y][x - 1] = hor[h - y][w - x] = 0;
	}
	if (x < w && !hor[y][x]) {
		hor[y][x] = hor[h - y][w - x - 1] = 1;
		walk(y, x + 1);
		hor[y][x] = hor[h - y][w - x - 1] = 0;
	}

	if (y && !ver[y - 1][x]) {
		ver[y - 1][x] = ver[h - y][w - x] = 1;
		walk(y - 1, x);
		ver[y - 1][x] = ver[h - y][w - x] = 0;
	}

	if (y < h && !ver[y][x]) {
		ver[y][x] = ver[h - y - 1][w - x] = 1;
		walk(y + 1, x);
		ver[y][x] = ver[h - y - 1][w - x] = 0;
	}

	vis[y][x]--; vis[h - y][w - x]--;
}

void cut(void)
{
	if (1 & (h * w)) return;

	hor = alloc2(w + 1, h + 1);
	ver = alloc2(w + 1, h + 1);
	vis = alloc2(w + 1, h + 1);

	if (h & 1) {
		ver[h/2][w/2] = 1;
		walk(h / 2, w / 2);
	} else if (w & 1) {
		hor[h/2][w/2] = 1;
		walk(h / 2, w / 2);
	} else {
		vis[h/2][w/2] = 1;

		hor[h/2][w/2-1] = hor[h/2][w/2] = 1;
		walk(h / 2, w / 2 - 1);
		hor[h/2][w/2-1] = hor[h/2][w/2] = 0;

		ver[h/2 - 1][w/2] = ver[h/2][w/2] = 1;
		walk(h / 2 - 1, w/2);
	}
}

void cwalk(int y, int x, int d)
{
	if (!y || y == h || !x || x == w) {
		++count;
		return;
	}
	vis[y][x] = vis[h-y][w-x] = 1;

	if (x && !vis[y][x-1])
		cwalk(y, x - 1, d|1);
	if ((d&1) && x < w && !vis[y][x+1])
		cwalk(y, x + 1, d|1);
	if (y && !vis[y-1][x])
		cwalk(y - 1, x, d|2);
	if ((d&2) && y < h && !vis[y + 1][x])
		cwalk(y + 1, x, d|2);

	vis[y][x] = vis[h-y][w-x] = 0;
}

void count_only(void)
{
	int t;
	long res;
	if (h * w & 1) return;
	if (h & 1) t = h, h = w, w = t;

	vis = alloc2(w + 1, h + 1);
	vis[h/2][w/2] = 1;

	if (w & 1) vis[h/2][w/2 + 1] = 1;
	if (w > 1) {
		cwalk(h/2, w/2 - 1, 1);
		res = 2 * count - 1;
		count = 0;
		if (w != h)
			cwalk(h/2+1, w/2, (w & 1) ? 3 : 2);

		res += 2 * count - !(w & 1);
	} else {
		res = 1;
	}
	if (w == h) res = 2 * res + 2;
	count = res;
}

int main(int c, char **v)
{
	int i;

	for (i = 1; i < c; i++) {
		if (v[i][0] == '-' && v[i][1] == 'v' && !v[i][2]) {
			verbose = 1;
		} else if (!w) {
			w = atoi(v[i]);
			if (w <= 0) goto bail;
		} else if (!h) {
			h = atoi(v[i]);
			if (h <= 0) goto bail;
		} else
			goto bail;
	}
	if (!w) goto bail;
	if (!h) h = w;

	if (verbose) cut();
	else count_only();

	printf("Total: %ld\n", count);
	return 0;

bail:	fprintf(stderr, "bad args\n");
	return 1;
}
```



## Common Lisp

Count only.

```lisp
(defun cut-it (w h &optional (recur t))
  (if (oddp (* w h)) (return-from cut-it 0))
  (if (oddp h) (rotatef w h))
  (if (= w 1) (return-from cut-it 1))

  (let ((cnt 0)
	(m (make-array (list (1+ h) (1+ w))
		       :element-type 'bit
		       :initial-element 0))
	(cy (truncate h 2))
	(cx (truncate w 2)))

    (setf (aref m cy cx) 1)
    (if (oddp w) (setf (aref m cy (1+ cx)) 1))

    (labels
      ((walk (y x turned)
	     (when (or (= y 0) (= y h) (= x 0) (= x w))
	       (incf cnt (if turned 2 1))
	       (return-from walk))

	     (setf (aref m y x) 1)
	     (setf (aref m (- h y) (- w x)) 1)
	     (loop for i from 0
		   for (dy dx) in '((0 -1) (-1 0) (0 1) (1 0))
		   while (or turned (< i 2)) do
		   (let ((y2 (+ y dy))
			 (x2 (+ x dx)))
		     (when (zerop (aref m y2 x2))
		       (walk y2 x2 (or turned (> i 0))))))
	     (setf (aref m (- h y) (- w x)) 0)
	     (setf (aref m y x) 0)))

      (walk cy (1- cx) nil)
      (cond ((= h w) (incf cnt cnt))
	    ((oddp w) (walk (1- cy) cx t))
	    (recur (incf cnt (cut-it h w nil))))
    cnt)))

(loop for w from 1 to 9 do
      (loop for h from 1 to w do
	    (if (evenp (* w h))
	      (format t "~d x ~d: ~d~%" w h (cut-it w h)))))
```
output<lang>2 x 1: 2
2 x 2: 2
3 x 2: 3
4 x 1: 4
4 x 2: 4
4 x 3: 9
4 x 4: 22
5 x 2: 5
5 x 4: 39
6 x 1: 6
6 x 2: 6
6 x 3: 23
6 x 4: 90
6 x 5: 263
6 x 6: 1018
7 x 2: 7
7 x 4: 151
7 x 6: 2947
8 x 1: 8
8 x 2: 8
8 x 3: 53
8 x 4: 340
8 x 5: 1675
8 x 6: 11174
8 x 7: 55939
8 x 8: 369050
9 x 2: 9
9 x 4: 553
9 x 6: 31721
9 x 8: 1812667
```



## D

```d
import core.stdc.stdio, core.stdc.stdlib, core.stdc.string, std.typecons;

enum int[2][4] dir = [[0, -1], [-1, 0], [0, 1], [1, 0]];

__gshared ubyte[] grid;
__gshared uint w, h, len;
__gshared ulong cnt;
__gshared uint[4] next;

void walk(in uint y, in uint x) nothrow @nogc {
    if (!y || y == h || !x || x == w) {
        cnt += 2;
        return;
    }

    immutable t = y * (w + 1) + x;
    grid[t]++;
    grid[len - t]++;

    foreach (immutable i; staticIota!(0, 4))
        if (!grid[t + next[i]])
            walk(y + dir[i][0], x + dir[i][1]);

    grid[t]--;
    grid[len - t]--;
}

ulong solve(in uint hh, in uint ww, in bool recur) nothrow @nogc {
    h = (hh & 1) ? ww : hh;
    w = (hh & 1) ? hh : ww;

    if (h & 1) return 0;
    if (w == 1) return 1;
    if (w == 2) return h;
    if (h == 2) return w;

    immutable cy = h / 2;
    immutable cx = w / 2;

    len = (h + 1) * (w + 1);
    {
        // grid.length = len; // Slower.
        alias T = typeof(grid[0]);
        auto ptr = cast(T*)alloca(len * T.sizeof);
        if (ptr == null)
            exit(1);
        grid = ptr[0 .. len];
    }
    grid[] = 0;
    len--;

    next = [-1, -w - 1, 1, w + 1];

    if (recur)
        cnt = 0;
    foreach (immutable x; cx + 1 .. w) {
        immutable t = cy * (w + 1) + x;
        grid[t] = 1;
        grid[len - t] = 1;
        walk(cy - 1, x);
    }
    cnt++;

    if (h == w)
        cnt *= 2;
    else if (!(w & 1) && recur)
        solve(w, h, 0);

    return cnt;
}

void main() {
    foreach (immutable uint y; 1 .. 11)
        foreach (immutable uint x; 1 .. y + 1)
            if (!(x & 1) || !(y & 1))
                printf("%d x %d: %llu\n", y, x, solve(y, x, true));
}
```

```txt
2 x 1: 1
2 x 2: 2
3 x 2: 3
4 x 1: 1
4 x 2: 4
4 x 3: 9
4 x 4: 22
5 x 2: 5
5 x 4: 39
6 x 1: 1
6 x 2: 6
6 x 3: 23
6 x 4: 90
6 x 5: 263
6 x 6: 1018
7 x 2: 7
7 x 4: 151
7 x 6: 2947
8 x 1: 1
8 x 2: 8
8 x 3: 53
8 x 4: 340
8 x 5: 1675
8 x 6: 11174
8 x 7: 55939
8 x 8: 369050
9 x 2: 9
9 x 4: 553
9 x 6: 31721
9 x 8: 1812667
10 x 1: 1
10 x 2: 10
10 x 3: 115
10 x 4: 1228
10 x 5: 10295
10 x 6: 118276
10 x 7: 1026005
10 x 8: 11736888
10 x 9: 99953769
10 x 10: 1124140214
```

Using the LDC2 compiler the runtime is about 15.98 seconds (the first C entry runs in about 16.75 seconds with GCC).


## Eiffel


```Eiffel

class
	APPLICATION

create
	make

feature {NONE} -- Initialization

	make
			-- Finds solution for cut a rectangle up to 10 x 10.
		local
			i, j, n: Integer
			r: GRID
		do
			n := 10
			from
				i := 1
			until
				i > n
			loop
				from
					j := 1
				until
					j > i
				loop
					if i.bit_and (1) /= 1 or j.bit_and (1) /= 1 then
						create r.make (i, j)
						r.print_solution
					end
					j := j + 1
				end
				i := i + 1
			end
		end

end

```


```Eiffel

class
	GRID

create
	make

feature {NONE}

	n: INTEGER

	m: INTEGER

feature

	print_solution
	                -- Prints solution to cut a rectangle.
		do
			calculate_possibilities
			io.put_string ("Rectangle " + n.out + " x " + m.out + ": " + count.out + " possibilities%N")
		end

	count: INTEGER
			-- Number of solutions

	make (a_n: INTEGER; a_m: INTEGER)
	                -- Initialize Problem with 'a_n' and 'a_m'.
		require
			a_n > 0
			a_m > 0
		do
			n := a_n
			m := a_m
			count := 0
		end

	calculate_possibilities
	                -- Select all possible starting points.
		local
			i: INTEGER
		do
			if (n = 1 or m = 1) then
				count := 1
			end

			from
				i := 0
			until
				i > n or (n = 1 or m = 1)
			loop
				solve (create {POINT}.make_with_values (i, 0), create {POINT}.make_with_values (n - i, m), create {LINKED_LIST [POINT]}.make, create {LINKED_LIST [POINT]}.make)
				i := i + 1
			variant
				n - i + 1
			end
			from
				i := 0
			until
				i > m or (n = 1 or m = 1)
			loop
				solve (create {POINT}.make_with_values (n, i), create {POINT}.make_with_values (0, m - i), create {LINKED_LIST [POINT]}.make, create {LINKED_LIST [POINT]}.make)
				i := i + 1
			variant
				m - i + 1
			end
		end

feature {NONE}

	solve (p, q: POINT; visited_p, visited_q: LINKED_LIST [POINT])
	                -- Recursive solution of cut a rectangle.
		local
			possible_next: LINKED_LIST [POINT]
			next: LINKED_LIST [POINT]
			opposite: POINT
		do
			if p.negative or q.negative then

			elseif p.same (q) then
				add_solution
			else
				possible_next := get_possible_next (p)
				create next.make
				across
					possible_next as x
				loop
					if x.item.x >= n or x.item.y >= m then
							-- Next point cannot be on the border. Do nothing.

					elseif x.item.same (q) then
						add_solution
					elseif not contains (x.item, visited_p) and not contains (x.item, visited_q) then
						next.extend (x.item)
					end
				end

				across
					next as x
				loop
						-- Move in one direction
						-- Calculate the opposite end of the cut by moving into the opposite direction (compared to p -> x)
					create opposite.make_with_values (q.x - (x.item.x - p.x), q.y - (x.item.y - p.y))

					visited_p.extend (p)
					visited_q.extend (q)

					solve (x.item, opposite, visited_p, visited_q)

						-- Remove last point again
					visited_p.finish
					visited_p.remove

					visited_q.finish
					visited_q.remove
				end
			end
		end

	get_possible_next (p: POINT): LINKED_LIST [POINT]
			-- Four possible next points.
		local
			q: POINT
		do
			create Result.make

				--up
			create q.make_with_values (p.x + 1, p.y)
			if q.valid and q.x <= n and q.y <= m then
				Result.extend (q);
			end

				--down
			create q.make_with_values (p.x - 1, p.y)
			if q.valid and q.x <= n and q.y <= m then
				Result.extend (q)
			end

				--left
			create q.make_with_values (p.x, p.y - 1)
			if q.valid and q.x <= n and q.y <= m then
				Result.extend (q)
			end

				--right
			create q.make_with_values (p.x, p.y + 1)
			if q.valid and q.x <= n and q.y <= m then
				Result.extend (q)
			end
		end

	add_solution
			-- Increment count.
		do
			count := count + 1
		end

	contains (p: POINT; set: LINKED_LIST [POINT]): BOOLEAN
			-- Does set contain 'p'?
		do
			set.compare_objects
			Result := set.has (p)
		end

end

```


```Eiffel

class
	POINT

create
	make, make_with_values



feature

	make_with_values (a_x: INTEGER; a_y: INTEGER)
	-- Initialize x and y with 'a_x' and 'a_y'.
		do
			x := a_x
			y := a_y
		end

	make
	-- Initialize x and y with 0.
		do
			x := 0
			y := 0
		end

	x: INTEGER

	y: INTEGER

	negative: BOOLEAN
			-- Are x or y negative?
		do
			Result := x < 0 or y < 0
		end

	same (other: POINT): BOOLEAN
			-- Does x and y equal 'other's x and y?
		do
			Result := (x = other.x) and (y = other.y)
		end

	valid: BOOLEAN
			-- Are x and y valid points?
		do
			Result := (x > 0) and (y > 0)
		end

end

```

```txt

Rectangle 2 x 1: 1 possibilities
Rectangle 2 x 2: 2 possibilities
Rectangle 3 x 2: 3 possibilities
Rectangle 4 x 1: 1 possibilities
Rectangle 4 x 2: 4 possibilities
Rectangle 4 x 3: 9 possibilities
Rectangle 4 x 4: 22 possibilities
Rectangle 5 x 2: 5 possibilities
Rectangle 5 x 4: 39 possibilities
Rectangle 6 x 1: 1 possibilities
Rectangle 6 x 2: 6 possibilities
Rectangle 6 x 3: 23 possibilities
Rectangle 6 x 4: 90 possibilities
Rectangle 6 x 5: 263 possibilities
Rectangle 6 x 6: 1018 possibilities
Rectangle 7 x 2: 7 possibilities
Rectangle 7 x 4: 151 possibilities
Rectangle 7 x 6: 2947 possibilities
Rectangle 8 x 1: 1 possibilities
Rectangle 8 x 2: 8 possibilities
Rectangle 8 x 3: 53 possibilities
Rectangle 8 x 4: 340 possibilities
Rectangle 8 x 5: 1675 possibilities
Rectangle 8 x 6: 11174 possibilities
Rectangle 8 x 7: 55939 possibilities
Rectangle 8 x 8: 369050 possibilities
Rectangle 9 x 2: 9 possibilities
Rectangle 9 x 4: 553 possibilities
Rectangle 9 x 6: 31721 possibilities
Rectangle 9 x 8: 1812667 possibilities
Rectangle 10 x 1: 1 possibilities
Rectangle 10 x 2: 10 possibilities
Rectangle 10 x 3: 115 possibilities
Rectangle 10 x 4: 1228 possibilities
Rectangle 10 x 5: 10295 possibilities
Rectangle 10 x 6: 118276 possibilities
Rectangle 10 x 7: 1026005 possibilities
Rectangle 10 x 8: 11736888 possibilities
Rectangle 10 x 9: 99953769 possibilities
Rectangle 10 x 10: 1124140214 possibilities

```



## Elixir

### Count only


```elixir
import  Integer

defmodule Rectangle do
  def cut_it(h, w) when is_odd(h) and is_odd(w), do: 0
  def cut_it(h, w) when is_odd(h), do: cut_it(w, h)
  def cut_it(_, 1), do: 1
  def cut_it(h, 2), do: h
  def cut_it(2, w), do: w
  def cut_it(h, w) do
    grid = List.duplicate(false, (h + 1) * (w + 1))
    t = div(h, 2) * (w + 1) + div(w, 2)
    if is_odd(w) do
      grid = grid |> List.replace_at(t, true) |> List.replace_at(t+1, true)
      walk(h, w, div(h, 2), div(w, 2) - 1, grid) + walk(h, w, div(h, 2) - 1, div(w, 2), grid) * 2
    else
      grid = grid |> List.replace_at(t, true)
      count = walk(h, w, div(h, 2), div(w, 2) - 1, grid)
      if h == w, do: count * 2,
               else: count + walk(h, w, div(h, 2) - 1, div(w, 2), grid)
    end
  end

  defp walk(h, w, y, x, grid, count\\0)
  defp walk(h, w, y, x,_grid, count) when y in [0,h] or x in [0,w], do: count+1
  defp walk(h, w, y, x, grid, count) do
    blen = (h + 1) * (w + 1) - 1
    t = y * (w + 1) + x
    grid = grid |> List.replace_at(t, true) |> List.replace_at(blen-t, true)
    Enum.reduce(next(w), count, fn {nt, dy, dx}, cnt ->
      if Enum.at(grid, t+nt), do: cnt, else: cnt + walk(h, w, y+dy, x+dx, grid)
    end)
  end

  defp next(w), do: [{w+1, 1, 0}, {-w-1, -1, 0}, {-1, 0, -1}, {1, 0, 1}]  # {next,dy,dx}
end

Enum.each(1..9, fn w ->
  Enum.each(1..w, fn h ->
    if is_even(w * h), do: IO.puts "#{w} x #{h}: #{Rectangle.cut_it(w, h)}"
  end)
end)
```


```txt

2 x 1: 1
2 x 2: 2
3 x 2: 3
4 x 1: 1
4 x 2: 4
4 x 3: 9
4 x 4: 22
5 x 2: 5
5 x 4: 39
6 x 1: 1
6 x 2: 6
6 x 3: 23
6 x 4: 90
6 x 5: 263
6 x 6: 1018
7 x 2: 7
7 x 4: 151
7 x 6: 2947
8 x 1: 1
8 x 2: 8
8 x 3: 53
8 x 4: 340
8 x 5: 1675
8 x 6: 11174
8 x 7: 55939
8 x 8: 369050
9 x 2: 9
9 x 4: 553
9 x 6: 31721
9 x 8: 1812667

```



### Show each of the cuts

```elixir
defmodule Rectangle do
  def cut(h, w, disp\\true) when rem(h,2)==0 or rem(w,2)==0 do
    limit = div(h * w, 2)
    start_link
    grid = make_grid(h, w)
    walk(h, w, grid, 0, 0, limit, %{}, [])
    if disp, do: display(h, w)
    result = Agent.get(__MODULE__, &(&1))
    Agent.stop(__MODULE__)
    MapSet.to_list(result)
  end

  defp start_link do
    Agent.start_link(fn -> MapSet.new end, name: __MODULE__)
  end

  defp make_grid(h, w) do
    for i <- 0..h-1, j <- 0..w-1, into: %{}, do: {{i,j}, true}
  end

  defp walk(h, w, grid, x, y, limit, cut, select) do
    grid2 = grid |> Map.put({x,y}, false) |> Map.put({h-x-1,w-y-1}, false)
    select2 = [{x,y} | select] |> Enum.sort
    unless cut[select2] do
      if length(select2) == limit do
        Agent.update(__MODULE__, fn set -> MapSet.put(set, select2) end)
      else
        cut2 = Map.put(cut, select2, true)
        search_next(grid2, select2)
        |> Enum.each(fn {i,j} -> walk(h, w, grid2, i, j, limit, cut2, select2) end)
      end
    end
  end

  defp dirs(x, y), do: [{x+1, y}, {x-1, y}, {x, y-1}, {x, y+1}]

  defp search_next(grid, select) do
    (for {x,y} <- select, {i,j} <- dirs(x,y), grid[{i,j}], do: {i,j})
    |> Enum.uniq
  end

  defp display(h, w) do
    Agent.get(__MODULE__, &(&1))
    |> Enum.each(fn select ->
         grid = Enum.reduce(select, make_grid(h,w), fn {x,y},grid ->
                  %{grid | {x,y} => false}
                end)
         IO.puts to_string(h, w, grid)
       end)
  end

  defp to_string(h, w, grid) do
    text = for x <- 0..h*2, into: %{}, do: {x, String.duplicate(" ", w*4+1)}
    text = Enum.reduce(0..h, text, fn i,acc ->
             Enum.reduce(0..w, acc, fn j,txt ->
               to_s(txt, i, j, grid)
             end)
           end)
    Enum.map_join(0..h*2, "\n", fn i -> text[i] end)
  end

  defp to_s(text, i, j, grid) do
    text = if grid[{i,j}] != grid[{i-1,j}], do: replace(text, i*2, j*4+1, "---"), else: text
    text = if grid[{i,j}] != grid[{i,j-1}], do: replace(text, i*2+1, j*4, "|"), else: text
    replace(text, i*2, j*4, "+")
  end

  defp replace(text, x, y, replacement) do
    len = String.length(replacement)
    Map.update!(text, x, fn str ->
      String.slice(str, 0, y) <> replacement <> String.slice(str, y+len..-1)
    end)
  end
end

Rectangle.cut(2, 2) |> length |> IO.puts
Rectangle.cut(3, 4) |> length |> IO.puts
```


```txt
+---+---+
|       |
+---+---+
|       |
+---+---+
+---+---+
|   |   |
+   +   +
|   |   |
+---+---+
2
+---+---+---+---+
|               |
+   +   +---+---+
|       |       |
+---+---+   +   +
|               |
+---+---+---+---+
+---+---+---+---+
|               |
+   +---+   +---+
|   |   |   |   |
+---+   +---+   +
|               |
+---+---+---+---+
+---+---+---+---+
|               |
+---+   +---+   +
|   |   |   |   |
+   +---+   +---+
|               |
+---+---+---+---+
+---+---+---+---+
|               |
+---+---+   +   +
|       |       |
+   +   +---+---+
|               |
+---+---+---+---+
+---+---+---+---+
|           |   |
+   +   +---+   +
|       |       |
+   +---+   +   +
|   |           |
+---+---+---+---+
+---+---+---+---+
|           |   |
+   +---+   +   +
|   |   |   |   |
+   +   +---+   +
|   |           |
+---+---+---+---+
+---+---+---+---+
|       |       |
+   +   +   +   +
|       |       |
+   +   +   +   +
|       |       |
+---+---+---+---+
+---+---+---+---+
|   |           |
+   +---+   +   +
|       |       |
+   +   +---+   +
|           |   |
+---+---+---+---+
+---+---+---+---+
|   |           |
+   +   +---+   +
|   |   |   |   |
+   +---+   +   +
|           |   |
+---+---+---+---+
9

```



## Go

```go
package main

import "fmt"

var grid []byte
var w, h, last int
var cnt int
var next [4]int
var dir = [4][2]int{{0, -1}, {-1, 0}, {0, 1}, {1, 0}}

func walk(y, x int) {
    if y == 0 || y == h || x == 0 || x == w {
        cnt += 2
        return
    }
    t := y*(w+1) + x
    grid[t]++
    grid[last-t]++
    for i, d := range dir {
        if grid[t+next[i]] == 0 {
            walk(y+d[0], x+d[1])
        }
    }
    grid[t]--
    grid[last-t]--
}

func solve(hh, ww, recur int) int {
    h = hh
    w = ww

    if h&1 != 0 {
        h, w = w, h
    }
    switch {
    case h&1 == 1:
        return 0
    case w == 1:
        return 1
    case w == 2:
        return h
    case h == 2:
        return w
    }
    cy := h / 2
    cx := w / 2

    grid = make([]byte, (h+1)*(w+1))
    last = len(grid) - 1
    next[0] = -1
    next[1] = -w - 1
    next[2] = 1
    next[3] = w + 1

    if recur != 0 {
        cnt = 0
    }
    for x := cx + 1; x < w; x++ {
        t := cy*(w+1) + x
        grid[t] = 1
        grid[last-t] = 1
        walk(cy-1, x)
    }
    cnt++

    if h == w {
        cnt *= 2
    } else if w&1 == 0 && recur != 0 {
        solve(w, h, 0)
    }
    return cnt
}

func main() {
    for y := 1; y <= 10; y++ {
        for x := 1; x <= y; x++ {
            if x&1 == 0 || y&1 == 0 {
                fmt.Printf("%d x %d: %d\n", y, x, solve(y, x, 1))
            }
        }
    }
}
```

```txt

2 x 1: 1
2 x 2: 2
3 x 2: 3
4 x 1: 1
4 x 2: 4
4 x 3: 9
4 x 4: 22
5 x 2: 5
5 x 4: 39
6 x 1: 1
6 x 2: 6
6 x 3: 23
6 x 4: 90
6 x 5: 263
6 x 6: 1018
7 x 2: 7
7 x 4: 151
7 x 6: 2947
8 x 1: 1
8 x 2: 8
8 x 3: 53
8 x 4: 340
8 x 5: 1675
8 x 6: 11174
8 x 7: 55939
8 x 8: 369050
9 x 2: 9
9 x 4: 553
9 x 6: 31721
9 x 8: 1812667
10 x 1: 1
10 x 2: 10
10 x 3: 115
10 x 4: 1228
10 x 5: 10295
10 x 6: 118276
10 x 7: 1026005
10 x 8: 11736888
10 x 9: 99953769
10 x 10: 1124140214

```



## Haskell

Calculation of the cuts happens in the ST monad, using a mutable STVector and a mutable STRef. The program style is therefore very imperative.
The strictness annotations in the Env type are necessary; otherwise, unevaluated thunks of updates of "env" would pile up with each recursion, ending in a stack overflow.

```Haskell
import qualified Data.Vector.Unboxed.Mutable as V
import Data.STRef
import Control.Monad (forM_, when)
import Control.Monad.ST

dir :: [(Int, Int)]
dir = [(1, 0), (-1, 0), (0, -1), (0, 1)]

data Env = Env { w, h, len, count, ret :: !Int, next :: ![Int] }

cutIt :: STRef s Env -> ST s ()
cutIt env = do
    e <- readSTRef env
    when (odd $ h e) $ modifySTRef env $ \en -> en { h = w e,
        w = h e }
    e <- readSTRef env
    if odd (h e)
        then modifySTRef env $ \en -> en { ret = 0 }
        else
            if w e == 1
            then modifySTRef env $ \en -> en { ret = 1 }
            else do
                let blen = (h e + 1) * (w e + 1) - 1
                    t = (h e `div` 2) * (w e + 1) + (w e `div` 2)
                modifySTRef env $ \en -> en { len = blen,
                    count = 0,
                    next = [ w e + 1, (negate $ w e) - 1, -1, 1] }
                grid <- V.replicate (blen + 1) False
                case odd (w e) of
                    True -> do
                        V.write grid t True
                        V.write grid (t + 1) True
                        walk grid (h e `div` 2) (w e `div` 2 - 1)
                        e1 <- readSTRef env
                        let res1 = count e1
                        modifySTRef env $ \en -> en { count = 0 }
                        walk grid (h e `div` 2 - 1) (w e `div` 2)
                        modifySTRef env $ \en -> en { ret = res1 +
                            (count en * 2) }
                    False -> do
                        V.write grid t True
                        walk grid (h e `div` 2) (w e `div` 2 - 1)
                        e2 <- readSTRef env
                        let count2 = count e2
                        if h e == w e
                            then modifySTRef env $ \en -> en { ret =
                                count2 * 2 }
                            else do
                                walk grid (h e `div` 2 - 1)
                                    (w e `div` 2)
                                modifySTRef env $ \en -> en { ret =
                                    count en }
    where
        walk grid y x = do
            e <- readSTRef env
            if y <= 0 || y >= h e || x <= 0 || x >= w e
                then modifySTRef env $ \en -> en { count = count en + 1 }
                else do
                    let t = y * (w e + 1) + x
                    V.write grid t True
                    V.write grid (len e - t) True
                    forM_ (zip (next e) [0..3]) $ \(n, d) -> do
                        g <- V.read grid (t + n)
                        when (not g) $
                            walk grid (y + fst (dir !! d)) (x + snd (dir !! d))
                    V.write grid t False
                    V.write grid (len e - t) False

cut :: (Int, Int) -> Int
cut (x, y) = runST $ do
    env <- newSTRef $ Env { w = y, h = x, len = 0, count = 0, ret = 0, next = [] }
    cutIt env
    result <- readSTRef env
    return $ ret result

main :: IO ()
main = do
    mapM_ (\(x, y) -> when (even (x * y)) (putStrLn $
        show x ++ " x " ++ show y ++ ": " ++ show (cut (x, y))))
        [ (x, y) | x <- [1..10], y <- [1..x] ]

```

With GHC -O3 the run-time is about 39 times the D entry.


## J



```j
init=: - {. 1:               NB. initial state: 1 square choosen
prop=: < {:,~2 ~:/\ ]        NB. propagate: neighboring squares (vertically)
poss=: I.@,@(prop +. prop"1 +. prop&.|. +. prop&.|."1)
keep=: poss -. <:@#@, - I.@, NB. symmetrically valid possibilities
N=: <:@-:@#@,                NB. how many neighbors to add
step=: [: ~.@;  <@(((= i.@$) +. ])"0 _~ keep)"2
all=: step^:N@init
```


In other words, starting with a boolean matrix with one true square in one corner, make a list of all false squares which neighbor a true square, and then make each of those neighbors true, independently (discarding duplicate matrices from the resulting sequence of boolean matrices), and repeat this N times where N is (total cells divided by two)-1.  Then discard those matrices where inverting them (boolean not), then flipping on horizontal and vertical axis is not an identity.

(In other words, this implementation uses a breadth first search -- breadth first searches tend to be natural in J because of the parallelism they offer.)

Example use:


```j
   '.#' <"2@:{~ all 3 4
┌────┬────┬────┬────┬────┬────┬────┬────┬────┐
│.###│.###│..##│...#│...#│....│....│....│....│
│.#.#│..##│..##│..##│.#.#│..##│.#.#│#.#.│##..│
│...#│...#│..##│.###│.###│####│####│####│####│
└────┴────┴────┴────┴────┴────┴────┴────┴────┘
   $ all 4 5
39 4 5
   3 13$ '.#' <"2@:{~ all 4 5
┌─────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┐
│.####│.####│.####│.####│.####│.####│..###│..###│..###│..###│..###│...##│...##│
│.####│.##.#│.#..#│..###│...##│....#│.####│.##.#│..###│...##│....#│.####│..###│
│....#│.#..#│.##.#│...##│..###│.####│....#│.#..#│...##│..###│.####│....#│...##│
│....#│....#│....#│....#│....#│....#│...##│...##│...##│...##│...##│..###│..###│
├─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┤
│...##│...##│...##│....#│....#│....#│....#│....#│....#│.....│.....│.....│.....│
│...##│....#│.#..#│.####│..###│...##│....#│.#..#│.##.#│.####│..###│...##│....#│
│..###│.####│.##.#│....#│...##│..###│.####│.##.#│.#..#│....#│...##│..###│.####│
│..###│..###│..###│.####│.####│.####│.####│.####│.####│#####│#####│#####│#####│
├─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┤
│.....│.....│.....│.....│.....│.....│.....│.....│.....│.....│.....│.....│.....│
│.#..#│.##.#│..##.│...#.│.....│.#...│.##..│#.##.│#..#.│#....│##...│###..│####.│
│.##.#│.#..#│#..##│#.###│#####│###.#│##..#│#..#.│#.##.│####.│###..│##...│#....│
│#####│#####│#####│#####│#####│#####│#####│#####│#####│#####│#####│#####│#####│
└─────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┘
```



## Java

```java
import java.util.*;

public class CutRectangle {

    private static int[][] dirs = {{0, -1}, {-1, 0}, {0, 1}, {1, 0}};

    public static void main(String[] args) {
        cutRectangle(2, 2);
        cutRectangle(4, 3);
    }

    static void cutRectangle(int w, int h) {
        if (w % 2 == 1 && h % 2 == 1)
            return;

        int[][] grid = new int[h][w];
        Stack<Integer> stack = new Stack<>();

        int half = (w * h) / 2;
        long bits = (long) Math.pow(2, half) - 1;

        for (; bits > 0; bits -= 2) {

            for (int i = 0; i < half; i++) {
                int r = i / w;
                int c = i % w;
                grid[r][c] = (bits & (1 << i)) != 0 ? 1 : 0;
                grid[h - r - 1][w - c - 1] = 1 - grid[r][c];
            }

            stack.push(0);
            grid[0][0] = 2;
            int count = 1;
            while (!stack.empty()) {

                int pos = stack.pop();
                int r = pos / w;
                int c = pos % w;

                for (int[] dir : dirs) {

                    int nextR = r + dir[0];
                    int nextC = c + dir[1];

                    if (nextR >= 0 && nextR < h && nextC >= 0 && nextC < w) {

                        if (grid[nextR][nextC] == 1) {
                            stack.push(nextR * w + nextC);
                            grid[nextR][nextC] = 2;
                            count++;
                        }
                    }
                }
            }
            if (count == half) {
                printResult(grid);
            }
        }
    }

    static void printResult(int[][] arr) {
        for (int[] a : arr)
            System.out.println(Arrays.toString(a));
        System.out.println();
    }
}
```



```txt
[2, 2]
[0, 0]

[2, 0]
[2, 0]

[2, 2, 2, 2]
[2, 2, 0, 0]
[0, 0, 0, 0]

[2, 2, 2, 0]
[2, 2, 0, 0]
[2, 0, 0, 0]

[2, 2, 0, 0]
[2, 2, 0, 0]
[2, 2, 0, 0]

[2, 0, 0, 0]
[2, 2, 0, 0]
[2, 2, 2, 0]

[2, 2, 2, 2]
[0, 2, 0, 2]
[0, 0, 0, 0]

[2, 2, 2, 2]
[2, 0, 2, 0]
[0, 0, 0, 0]

[2, 2, 2, 0]
[2, 0, 2, 0]
[2, 0, 0, 0]

[2, 0, 0, 0]
[2, 0, 2, 0]
[2, 2, 2, 0]

[2, 2, 2, 2]
[0, 0, 2, 2]
[0, 0, 0, 0]
```



## Julia

```julia

const count = [0]
const dir = [[0, -1], [-1, 0], [0, 1], [1, 0]]

function walk(y, x, h, w, grid, len, next)
    if y == 0 || y == h || x == 0 || x == w
        count[1] += 2
        return
    end
    t = y * (w + 1) + x
    grid[t + 1] += UInt8(1)
    grid[len - t + 1] += UInt8(1)
    for i in 1:4
        if grid[t + next[i] + 1] == 0
            walk(y + dir[i][1], x + dir[i][2], h, w, grid, len, next)
        end
    end
    grid[t + 1] -= 1
    grid[len - t + 1] -= 1
end

function cutrectangle(hh, ww, recur)
    if isodd(hh)
        h, w = ww, hh
    else
        h, w = hh, ww
    end
    if isodd(h)
        return 0
    elseif w == 1
        return 1
    elseif w == 2
        return h
    elseif h == 2
        return w
    end
    cy = div(h, 2)
    cx = div(w, 2)
    len = (h + 1) * (w + 1)
    grid = zeros(UInt8, len)
    len -= 1
    next = [-1, -w - 1, 1, w + 1]
    if recur
        count[1] = 0
    end
    for x in cx + 1:w - 1
        t = cy * (w + 1) + x
        grid[t + 1] = 1
        grid[len - t + 1] = 1
        walk(cy - 1, x, h, w, grid, len, next)
    end
    count[1] += 1
    if h == w
        count[1] *= 2
    elseif iseven(w) && recur
        cutrectangle(w, h, false)
    end
    return count[1]
end

function runtest()
    for y in 1:10, x in 1:y
        if iseven(x) || iseven(y)
            println("$y x $x: $(cutrectangle(y, x, true))")
        end
    end
end

runtest()

```
 {{output}}
```txt

2 x 1: 1
2 x 2: 2
3 x 2: 3
4 x 1: 1
4 x 2: 4
4 x 3: 9
4 x 4: 22
5 x 2: 5
5 x 4: 39
6 x 1: 1
6 x 2: 6
6 x 3: 23
6 x 4: 90
6 x 5: 263
6 x 6: 1018
7 x 2: 7
7 x 4: 151
7 x 6: 2947
8 x 1: 1
8 x 2: 8
8 x 3: 53
8 x 4: 340
8 x 5: 1675
8 x 6: 11174
8 x 7: 55939
8 x 8: 369050
9 x 2: 9
9 x 4: 553
9 x 6: 31721
9 x 8: 1812667
10 x 1: 1
10 x 2: 10
10 x 3: 115
10 x 4: 1228
10 x 5: 10295
10 x 6: 118276
10 x 7: 1026005
10 x 8: 11736888
10 x 9: 99953769
10 x 10: 1124140214

```



## Kotlin

```scala
// version 1.0.6

object RectangleCutter {
    private var w: Int = 0
    private var h: Int = 0
    private var len: Int = 0
    private var cnt: Long = 0

    private lateinit var grid: ByteArray
    private val next = IntArray(4)
    private val dir = arrayOf(
        intArrayOf(0, -1),
        intArrayOf(-1, 0),
        intArrayOf(0, 1),
        intArrayOf(1, 0)
    )

    private fun walk(y: Int, x: Int) {
        if (y == 0 || y == h || x == 0 || x == w) {
            cnt += 2
            return
        }
        val t = y * (w + 1) + x
        grid[t]++
        grid[len - t]++
        (0..3).filter { grid[t + next[it]] == 0.toByte() }
            .forEach { walk(y + dir[it][0], x + dir[it][1]) }
        grid[t]--
        grid[len - t]--
    }

    fun solve(hh: Int, ww: Int, recur: Boolean): Long {
        var t: Int
        h = hh
        w = ww
        if ((h and 1) != 0) {
            t = w
            w = h
            h = t
        }
        if ((h and 1) != 0) return 0L
        if (w == 1) return 1L
        if (w == 2) return h.toLong()
        if (h == 2) return w.toLong()
        val cy = h / 2
        val cx = w / 2
        len = (h + 1) * (w + 1)
        grid = ByteArray(len)
        len--
        next[0] = -1
        next[1] = -w - 1
        next[2] = 1
        next[3] = w + 1
        if (recur) cnt = 0L
        for (x in cx + 1 until w) {
            t = cy * (w + 1) + x
            grid[t] = 1
            grid[len - t] = 1
            walk(cy - 1, x)
        }
        cnt++
        if (h == w) cnt *= 2
        else if ((w and 1) == 0 && recur) solve(w, h, false)
        return cnt
    }
}

fun main(args: Array<String>) {
    for (y in 1..10) {
        for (x in 1..y) {
            if ((x and 1) == 0 || (y and 1) == 0) {
                println("${"%2d".format(y)} x ${"%2d".format(x)}: ${RectangleCutter.solve(y, x, true)}")
            }
        }
    }
}
```


```txt

 2 x  1: 1
 2 x  2: 2
 3 x  2: 3
 4 x  1: 1
 4 x  2: 4
 4 x  3: 9
 4 x  4: 22
 5 x  2: 5
 5 x  4: 39
 6 x  1: 1
 6 x  2: 6
 6 x  3: 23
 6 x  4: 90
 6 x  5: 263
 6 x  6: 1018
 7 x  2: 7
 7 x  4: 151
 7 x  6: 2947
 8 x  1: 1
 8 x  2: 8
 8 x  3: 53
 8 x  4: 340
 8 x  5: 1675
 8 x  6: 11174
 8 x  7: 55939
 8 x  8: 369050
 9 x  2: 9
 9 x  4: 553
 9 x  6: 31721
 9 x  8: 1812667
10 x  1: 1
10 x  2: 10
10 x  3: 115
10 x  4: 1228
10 x  5: 10295
10 x  6: 118276
10 x  7: 1026005
10 x  8: 11736888
10 x  9: 99953769
10 x 10: 1124140214

```



## Perl

Output is identical to C's.

```perl
use strict;
use warnings;
my @grid = 0;

my ($w, $h, $len);
my $cnt = 0;

my @next;
my @dir = ([0, -1], [-1, 0], [0, 1], [1, 0]);

sub walk {
    my ($y, $x) = @_;

    if (!$y || $y == $h || !$x || $x == $w) {
	$cnt += 2;
	return;
    }

    my $t = $y * ($w + 1) + $x;
    $grid[$_]++ for $t, $len - $t;

    for my $i (0 .. 3) {
	if (!$grid[$t + $next[$i]]) {
	    walk($y + $dir[$i]->[0], $x + $dir[$i]->[1]);
	}
    }

    $grid[$_]-- for $t, $len - $t;
}

sub solve {
    my ($hh, $ww, $recur) = @_;
    my ($t, $cx, $cy, $x);
    ($h, $w) = ($hh, $ww);

    if ($h & 1) { ($t, $w, $h) = ($w, $h, $w); }
    if ($h & 1) { return 0; }
    if ($w == 1) { return 1; }
    if ($w == 2) { return $h; }
    if ($h == 2) { return $w; }

    {
	use integer;
	($cy, $cx) = ($h / 2, $w / 2);
    }

    $len = ($h + 1) * ($w + 1);
    @grid = ();
    $grid[$len--] = 0;

    @next = (-1, -$w - 1, 1, $w + 1);

    if ($recur) { $cnt = 0; }
    for ($x = $cx + 1; $x < $w; $x++) {
	$t = $cy * ($w + 1) + $x;
	@grid[$t, $len - $t] = (1, 1);
	walk($cy - 1, $x);
    }
    $cnt++;

    if ($h == $w) {
	$cnt *= 2;
    } elsif (!($w & 1) && $recur) {
	solve($w, $h);
    }

    return $cnt;
}

sub MAIN {
    print "ok\n";
    my ($y, $x);
    for my $y (1 .. 10) {
	for my $x (1 .. $y) {
	    if (!($x & 1) || !($y & 1)) {
		printf("%d x %d: %d\n", $y, $x, solve($y, $x, 1));
	    }
	}
    }
}

MAIN();
```



## Perl 6

This is a very dumb, straightforward translation of the C code.  It is very slow so we'll interrupt the execution and display the partial output.


```perl6
subset Byte of Int where ^256;
my @grid of Byte = 0;

my Int ($w, $h, $len);
my Int $cnt = 0;

my @next;
my @dir = [0, -1], [-1, 0], [0, 1], [1, 0];
sub walk(Int $y, Int $x) {
    my ($i, $t);
    if !$y || $y == $h || !$x || $x == $w {
        $cnt += 2;
        return;
    }
    $t = $y * ($w + 1) + $x;
    @grid[$t]++, @grid[$len - $t]++;

    loop ($i = 0; $i < 4; $i++) {
        if !@grid[$t + @next[$i]] {
            walk($y + @dir[$i][0], $x + @dir[$i][1]);
        }
    }

    @grid[$t]--, @grid[$len - $t]--;
}

sub solve(Int $hh, Int $ww, Int $recur) returns Int {
    my ($t, $cx, $cy, $x);
    $h = $hh, $w = $ww;

    if $h +& 1 { $t = $w, $w = $h, $h = $t; }
    if $h +& 1 { return 0; }
    if $w == 1 { return 1; }
    if $w == 2 { return $h; }
    if $h == 2 { return $w; }

    $cy = $h div 2, $cx = $w div 2;

    $len = ($h + 1) * ($w + 1);
    @grid = ();
    @grid[$len--] = 0;

    @next[0] = -1;
    @next[1] = -$w - 1;
    @next[2] = 1;
    @next[3] = $w + 1;

    if $recur { $cnt = 0; }
    loop ($x = $cx + 1; $x < $w; $x++) {
        $t = $cy * ($w + 1) + $x;
        @grid[$t] = 1;
        @grid[$len - $t] = 1;
        walk($cy - 1, $x);
    }
    $cnt++;

    if $h == $w {
        $cnt *= 2;
    } elsif !($w +& 1) && $recur {
        solve($w, $h, 0);
    }

    return $cnt;
}

my ($y, $x);
loop ($y = 1; $y <= 9; $y++) {
    loop ($x = 1; $x <= $y; $x++) {
        if (!($x +& 1) || !($y +& 1)) {
            printf("%d x %d: %d\n", $y, $x, solve($y, $x, 1));
        }
    }
}
```

```txt
2 x 1: 1
2 x 2: 2
3 x 2: 3
4 x 1: 1
4 x 2: 4
4 x 3: 9
4 x 4: 22
5 x 2: 5
5 x 4: 39
6 x 1: 1
6 x 2: 6
6 x 3: 23
6 x 4: 90
6 x 5: 263
6 x 6: 1018
7 x 2: 7
7 x 4: 151
7 x 6: 2947
8 x 1: 1
8 x 2: 8
8 x 3: 53
8 x 4: 340
8 x 5: 1675
^C
```



## Phix

Using a completely different home-brewed algorithm, slightly sub-optimal as noted in the code.

```Phix
integer show = 2,       -- max number to show
                        -- (nb mirrors are not shown)
        chance = 1000   -- 1=always, 2=50%, 3=33%, etc

sequence grid

integer gh, -- = length(grid),
        gw  -- = length(grid[1])

integer ty1, ty2, tx1, tx2  -- target {y,x}s

procedure mirror(integer y, x, ch)
-- plant/reset ch and the symmetric copy
    grid[y,x] = ch
    grid[gh-y+1,gw-x+1] = ch
end procedure

enum             RIGHT,  UP,   DOWN,  LEFT
constant dyx = {{0,+1},{-1,0},{+1,0},{0,-1}},
         chx = "-||-"

function search(integer y, x, d, level)
    integer count = 0
    if level=0 or grid[y,x]!='x' then
        mirror(y,x,'x')
        integer {dy,dx} = dyx[d],
                {ny,nx} = {y+dy,x+dx},
                {yy,xx} = {y+dy*2,x+dx*3}
        if grid[ny,nx]=' ' then
            integer c = chx[d]
            mirror(ny,nx,c)
            if c='-' then
                mirror(ny,nx+dx,c)
            end if
            integer meet = (yy=ty1 or yy=ty2) and (xx=tx1 or xx=tx2)
            if meet then
                count = 1
                if show and rand(chance)=chance then
                    show -= 1
                    sequence g = grid -- (make copy/avoid reset)
                    -- fill in(/overwrite) the last cut, if any
                    if    ty1!=ty2 then g[ty1+1,tx1] = '|'
                    elsif tx1!=tx2 then g[ty1][tx1+1..tx1+2] = "--"
                    end if
                    puts(1,join(g,'\n')&"\n\n")
                end if
            else
                if grid[yy,xx]='+' then -- (minor gain)
                    for d=RIGHT to LEFT do -- (kinda true!)
                        count += search(yy,xx,d,level+1)
                    end for
                end if
            end if
            mirror(ny,nx,' ')
            if c='-' then
                mirror(ny,nx+dx,' ')
            end if
        end if
        if level!=0 then
            -- ((level=0)==leave outer edges 'x' for next iteration)
            mirror(y,x,'+')
        end if
    end if
    return count
end function

function odd(integer n)  return remainder(n,2)=1 end function
function even(integer n) return remainder(n,2)=0 end function

procedure make_grid(integer w,h)
-- The outer edges are 'x'; the inner '+' become 'x' when visited.
-- Likewise edges are cuts but the inner ones get filled in later.
sequence tb = join(repeat("x",w+1),"--"),
         hz = join('x'&repeat("+",w-1)&'x',"  ")&"\n",
         vt = "|"&repeat(' ',w*3-1)&"|\n"
    grid = split(tb&"\n"&join(repeat(vt,h),hz)&tb,'\n')
    -- set size (for mirroring) and target info:
    gh = length(grid)       gw = length(grid[1])
    ty1 = h+even(h)         ty2 = ty1+odd(h)*2
    tx1 = floor(w/2)*3+1    tx2 = tx1+odd(w)*3
end procedure

function side(integer w, h)
    make_grid(w,h)
    -- search top to mid-point
    integer count = 0, last = 0
    for r=3 to h+1 by 2 do
        last = search(r,1,RIGHT,0) -- left to right
        count += 2*last
    end for
    if even(h) then
        count -= last -- (un-double the centre line)
    end if
    return count
end function

--atom t0 = time()
-- nb sub-optimal: obviously "grid" was designed for easy display, rather than speed.
for y=1 to 9 do         -- 24s
--for y=1 to 10 do      -- (gave up on >10x8)
    for x=1 to y do
--  for x=1 to min(y,8) do  -- 4 mins 16s (with y to 10)
        if even(x*y) then
            integer count = side(x,y)
            if x=y then
                count *= 2
            else
                count += side(y,x)
            end if
            printf(1,"%d x %d: %d\n", {y, x, count})
        end if
    end for
end for
--?elapsed(time()-t0)
```

Includes two random grids

```txt

2 x 1: 1
2 x 2: 2
3 x 2: 3
4 x 1: 1
4 x 2: 4
4 x 3: 9
4 x 4: 22
5 x 2: 5
5 x 4: 39
6 x 1: 1
6 x 2: 6
6 x 3: 23
6 x 4: 90
6 x 5: 263
6 x 6: 1018
7 x 2: 7
7 x 4: 151
x--x--x--x--x--x--x
|                 |
x--x  +  +  +  +  x
|  |              |
x  x  x--x--x  +  x
|  |  |     |     |
x  x--x  x--x  +  x
|        |        |
x  +  x--x  x--x  x
|     |     |  |  |
x  +  x--x--x  x  x
|              |  |
x  +  +  +  +  x--x
|                 |
x--x--x--x--x--x--x

x--x--x--x--x--x--x--x
|                    |
x  +  x--x--x--x--x  x
|     |           |  |
x--x--x  x--x--x  x  x
|        |     |  |  |
x  x--x  x--x  x--x  x
|  |  |     |        |
x  x  x--x--x  x--x--x
|  |           |     |
x  x--x--x--x--x  +  x
|                    |
x--x--x--x--x--x--x--x

7 x 6: 2947
8 x 1: 1
8 x 2: 8
8 x 3: 53
8 x 4: 340
8 x 5: 1675
8 x 6: 11174
8 x 7: 55939
8 x 8: 369050
9 x 2: 9
9 x 4: 553
9 x 6: 31721
9 x 8: 1812667
10 x 1: 1
10 x 2: 10
10 x 3: 115
10 x 4: 1228
10 x 5: 10295
10 x 6: 118276
10 x 7: 1026005
10 x 8: 11736888

```



## Python

```python
def cut_it(h, w):
    dirs = ((1, 0), (-1, 0), (0, -1), (0, 1))
    if h & 1: h, w = w, h
    if h & 1: return 0
    if w == 1: return 1
    count = 0

    next = [w + 1, -w - 1, -1, 1]
    blen = (h + 1) * (w + 1) - 1
    grid = [False] * (blen + 1)

    def walk(y, x, count):
        if not y or y == h or not x or x == w:
            return count + 1

        t = y * (w + 1) + x
        grid[t] = grid[blen - t] = True

        if not grid[t + next[0]]:
            count = walk(y + dirs[0][0], x + dirs[0][1], count)
        if not grid[t + next[1]]:
            count = walk(y + dirs[1][0], x + dirs[1][1], count)
        if not grid[t + next[2]]:
            count = walk(y + dirs[2][0], x + dirs[2][1], count)
        if not grid[t + next[3]]:
            count = walk(y + dirs[3][0], x + dirs[3][1], count)

        grid[t] = grid[blen - t] = False
        return count

    t = h // 2 * (w + 1) + w // 2
    if w & 1:
        grid[t] = grid[t + 1] = True
        count = walk(h // 2, w // 2 - 1, count)
        res = count
        count = 0
        count = walk(h // 2 - 1, w // 2, count)
        return res + count * 2
    else:
        grid[t] = True
        count = walk(h // 2, w // 2 - 1, count)
        if h == w:
            return count * 2
        count = walk(h // 2 - 1, w // 2, count)
        return count

def main():
    for w in xrange(1, 10):
        for h in xrange(1, w + 1):
            if not((w * h) & 1):
                print "%d x %d: %d" % (w, h, cut_it(w, h))

main()
```

Output:

```txt
2 x 1: 1
2 x 2: 2
3 x 2: 3
4 x 1: 1
4 x 2: 4
4 x 3: 9
4 x 4: 22
5 x 2: 5
5 x 4: 39
6 x 1: 1
6 x 2: 6
6 x 3: 23
6 x 4: 90
6 x 5: 263
6 x 6: 1018
7 x 2: 7
7 x 4: 151
7 x 6: 2947
8 x 1: 1
8 x 2: 8
8 x 3: 53
8 x 4: 340
8 x 5: 1675
8 x 6: 11174
8 x 7: 55939
8 x 8: 369050
9 x 2: 9
9 x 4: 553
9 x 6: 31721
9 x 8: 1812667
```


### Faster version

```python
try:
    import psyco
except ImportError:
    pass
else:
    psyco.full()

w, h = 0, 0
count = 0
vis = []

def cwalk(y, x, d):
    global vis, count, w, h
    if not y or y == h or not x or x == w:
        count += 1
        return

    vis[y][x] = vis[h - y][w - x] = 1

    if x and not vis[y][x - 1]:
        cwalk(y, x - 1, d | 1)
    if (d & 1) and x < w and not vis[y][x+1]:
        cwalk(y, x + 1, d|1)
    if y and not vis[y - 1][x]:
        cwalk(y - 1, x, d | 2)
    if (d & 2) and y < h and not vis[y + 1][x]:
        cwalk(y + 1, x, d | 2)

    vis[y][x] = vis[h - y][w - x] = 0

def count_only(x, y):
    global vis, count, w, h
    count = 0
    w = x
    h = y

    if (h * w) & 1:
        return count
    if h & 1:
        w, h = h, w

    vis = [[0] * (w + 1) for _ in xrange(h + 1)]
    vis[h // 2][w // 2] = 1

    if w & 1:
        vis[h // 2][w // 2 + 1] = 1

    res = 0
    if w > 1:
        cwalk(h // 2, w // 2 - 1, 1)
        res = 2 * count - 1
        count = 0
        if w != h:
            cwalk(h // 2 + 1, w // 2, 3 if (w & 1) else 2)

        res += 2 * count - (not (w & 1))
    else:
        res = 1

    if w == h:
        res = 2 * res + 2
    return res

def main():
    for y in xrange(1, 10):
        for x in xrange(1, y + 1):
            if not (x & 1) or not (y & 1):
                print "%d x %d: %d" % (y, x, count_only(x, y))

main()
```

The output is the same.


## Racket


```racket

#lang racket

(define (cuts W H [count 0]) ; count = #f => visualize instead
  (define W1 (add1 W)) (define H1 (add1 H))
  (define B (make-vector (* W1 H1) #f))
  (define (fD d) (cadr (assq d '([U D] [D U] [L R] [R L] [#f #f] [#t #t]))))
  (define (fP p) (- (* W1 H1) p 1))
  (define (Bset! p d) (vector-set! B p d) (vector-set! B (fP p) (fD d)))
  (define center (/ (fP 0) 2))
  (when (integer? center) (Bset! center #t))
  (define (run c* d)
    (define p (- center c*))
    (Bset! p d)
    (let loop ([p p])
      (define-values [q r] (quotient/remainder p W1))
      (if (and (< 0 r W) (< 0 q H))
        (for ([d '(U D L R)])
          (define n (+ p (case d [(U) (- W1)] [(D) W1] [(L) -1] [(R) 1])))
          (unless (vector-ref B n) (Bset! n (fD d)) (loop n) (Bset! n #f)))
        (if count (set! count (add1 count)) (visualize B W H))))
    (Bset! p #f))
  (when (even? W) (run (if (odd? H) (/ W1 2) W1) 'D))
  (when (even? H) (run (if (odd? W) 1/2 1)       'R))
  (or count (void)))

(define (visualize B W H)
  (define W2 (+ 2 (* W 2))) (define H2 (+ 1 (* H 2)))
  (define str (make-string (* H2 W2) #\space))
  (define (Sset! i c) (string-set! str i c))
  (for ([i (in-range (- W2 1) (* W2 H2) W2)]) (Sset! i #\newline))
  (for ([i (in-range 0 (- W2 1))]) (Sset! i #\#) (Sset! (+ i (* W2 H 2)) #\#))
  (for ([i (in-range 0 (* W2 H2) W2)]) (Sset! i #\#) (Sset! (+ i W2 -2) #\#))
  (for* ([i (add1 W)] [j (add1 H)])
    (define p (* 2 (+ i (* j W2))))
    (define b (vector-ref B (+ i (* j (+ W 1)))))
    (cond [b (Sset! p #\#)
             (define d (case b [(U) (- W2)] [(D) W2] [(R) 1] [(L) -1]))
             (when (integer? d) (Sset! (+ p d) #\#))]
          [(equal? #\space (string-ref str p)) (Sset! p #\.)]))
  (display str) (newline))

(printf "Counts:\n")
(for* ([W (in-range 1 10)] [H (in-range 1 (add1 W))]
       #:unless (and (odd? W) (odd? H)))
  (printf "~s x ~s: ~s\n" W H (cuts W H)))

(newline)
(cuts 4 3 #f)

```


```txt

Counts:
2 x 1: 1
2 x 2: 2
3 x 2: 3
4 x 1: 1
4 x 2: 4
4 x 3: 9
4 x 4: 22
5 x 2: 5
5 x 4: 39
6 x 1: 1
6 x 2: 6
6 x 3: 23
6 x 4: 90
6 x 5: 263
6 x 6: 1018
7 x 2: 7
7 x 4: 151
7 x 6: 2947
8 x 1: 1
8 x 2: 8
8 x 3: 53
8 x 4: 340
8 x 5: 1675
8 x 6: 11174
8 x 7: 55939
8 x 8: 369050
9 x 2: 9
9 x 4: 553
9 x 6: 31721
9 x 8: 1812667

#########
#   #   #
# . # . #
#   #   #
# . # . #
#   #   #
#########

#########
# #     #
# ### . #
#   #   #
# . ### #
#     # #
#########

#########
#     # #
# ### # #
# # # # #
# # ### #
# #     #
#########

#########
#       #
# ### ###
# # # # #
### ### #
#       #
#########

#########
#       #
##### . #
#   #   #
# . #####
#       #
#########

#########
#     # #
# . ### #
#   #   #
# ### . #
# #     #
#########

#########
# #     #
# # ### #
# # # # #
# ### # #
#     # #
#########

#########
#       #
### ### #
# # # # #
# ### ###
#       #
#########

#########
#       #
# . #####
#   #   #
##### . #
#       #
#########

```



## REXX


### idiomatic


```rexx
/*REXX program cuts rectangles into two symmetric pieces,  the rectangles are cut along */
/*────────────────────────────────────────────────── unit dimensions and may be rotated.*/
numeric digits 20                                /*be able to handle some big integers. */
parse arg N .;    if N=='' | N==","  then N=10   /*N  not specified?   Then use default.*/
dir.=0;   dir.0.1=-1;   dir.1.0=-1;   dir.2.1=1;   dir.3.0=1      /*the four directions.*/

     do   y=2   to N;   say                      /*calculate rectangles up to size  NxN.*/
       do x=1  for y;   if x//2 & y//2  then iterate              /*not if both X&Y odd.*/
       z=solve(y,x,1); _=comma(z); _=right(_, max(14, length(_))) /*align the output.   */
       say right(y,9)     "x"    right(x,2)     'rectangle can be cut'    _     "way"s(z).
       end   /*x*/
     end     /*y*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
comma: procedure; arg _;  do k=length(_)-3  to 1  by -3; _=insert(',',_,k); end;  return _
/*──────────────────────────────────────────────────────────────────────────────────────*/
s:     if arg(1)=1  then return arg(3);   return word(arg(2) 's', 1)       /*pluralizer.*/
/*──────────────────────────────────────────────────────────────────────────────────────*/
solve: procedure expose # dir. @. h len next. w; @.=0      /*zero rectangle coördinates.*/
       parse arg h,w,recur                                 /*get values for some args.  */
       if h//2  then do;    t=w;  w=h;  h=t;   if h//2  then return 0
                     end
       if w==1  then return 1
       if w==2  then return h
       if h==2  then return w                    /* [↓]   %  is REXX's integer division.*/
       cy=h % 2;     cx=w % 2;        wp=w + 1   /*cut the  [XY]  rectangle in half.    */
       len=(h+1) * wp - 1                        /*extend the area of the rectangle.    */
       next.0=-1;     next.1=-wp;     next.2=1;     next.3=wp    /*direction & distance.*/
       if recur  then #=0
              do x=cx+1  to  w-1;     t=x + cy*wp;      @.t=1;      _=len - t;       @._=1
              call walk cy-1, x
              end   /*x*/
       #=#+1
       if h==w  then #=# + #                     /*double the count of rectangle cuts.  */
                else if w//2==0 & recur  then call solve w, h, 0
       return #
/*──────────────────────────────────────────────────────────────────────────────────────*/
walk:  procedure expose # dir. @. h len next. w wp;       parse arg y,x
       if y==h | x==0 | x==w | y==0  then do;   #= #+2;   return;   end
       t=x + y*wp;      @.t=@.t + 1;      _=len - t
       @._=@._+1
                    do j=0  for 4;        _=t + next.j    /*try each of four directions.*/
                    if @._==0  then call walk  y + dir.j.0,   x + dir.j.1
                    end   /*j*/
       @.t=@.t - 1
       _=len - t;       @._=@._ - 1;      return
```

```txt

        2 x  1 rectangle can be cut              1 way.
        2 x  2 rectangle can be cut              2 ways.

        3 x  2 rectangle can be cut              3 ways.

        4 x  1 rectangle can be cut              1 way.
        4 x  2 rectangle can be cut              4 ways.
        4 x  3 rectangle can be cut              9 ways.
        4 x  4 rectangle can be cut             22 ways.

        5 x  2 rectangle can be cut              5 ways.
        5 x  4 rectangle can be cut             39 ways.

        6 x  1 rectangle can be cut              1 way.
        6 x  2 rectangle can be cut              6 ways.
        6 x  3 rectangle can be cut             23 ways.
        6 x  4 rectangle can be cut             90 ways.
        6 x  5 rectangle can be cut            263 ways.
        6 x  6 rectangle can be cut          1,018 ways.

        7 x  2 rectangle can be cut              7 ways.
        7 x  4 rectangle can be cut            151 ways.
        7 x  6 rectangle can be cut          2,947 ways.

        8 x  1 rectangle can be cut              1 way.
        8 x  2 rectangle can be cut              8 ways.
        8 x  3 rectangle can be cut             53 ways.
        8 x  4 rectangle can be cut            340 ways.
        8 x  5 rectangle can be cut          1,675 ways.
        8 x  6 rectangle can be cut         11,174 ways.
        8 x  7 rectangle can be cut         55,939 ways.
        8 x  8 rectangle can be cut        369,050 ways.

        9 x  2 rectangle can be cut              9 ways.
        9 x  4 rectangle can be cut            553 ways.
        9 x  6 rectangle can be cut         31,721 ways.
        9 x  8 rectangle can be cut      1,812,667 ways.

       10 x  1 rectangle can be cut              1 way.
       10 x  2 rectangle can be cut             10 ways.
       10 x  3 rectangle can be cut            115 ways.
       10 x  4 rectangle can be cut          1,228 ways.
       10 x  5 rectangle can be cut         10,295 ways.
       10 x  6 rectangle can be cut        118,276 ways.
       10 x  7 rectangle can be cut      1,026,005 ways.
       10 x  8 rectangle can be cut     11,736,888 ways.
       10 x  9 rectangle can be cut     99,953,769 ways.
       10 x 10 rectangle can be cut  1,124,140,214 ways.

```



### optimized

This version replaced the (first) multiple clause   '''if'''   instructions in the   '''walk'''   subroutine with a

''short circuit'' version.   Other optimizations were also made.   This made the program about 20% faster.


A test run was executed to determine the order of the   '''if'''   statements   (by counting which

comparison would yield the most benefit by placing it first).

```rexx
/*REXX program cuts rectangles into two symmetric pieces,  the rectangles are cut along */
/*────────────────────────────────────────────────── unit dimensions and may be rotated.*/
numeric digits 20                                /*be able to handle some big integers. */
parse arg N .;    if N=='' | N==","  then N=10   /*N  not specified?   Then use default.*/
dir.=0;   dir.0.1= -1;   dir.1.0= -1;   dir.2.1= 1;   dir.3.0= 1     /*the 4 directions.*/

     do   y=2   to N;   yEven= y//2;     say     /*calculate rectangles up to size  NxN.*/
       do x=1  for y;   if x//2  then if yEven  then iterate       /*not if both X&Y odd*/
       z= solve(y,x,1); _=comma(z); _=right(_, max(14, length(_))) /*align the output.  */
       say right(y, 9)    "x"    right(x, 2)    'rectangle can be cut'    _     "way"s(z).
       end   /*x*/
     end     /*y*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
comma: procedure; arg _;  do k=length(_)-3  to 1  by -3; _=insert(',',_,k); end;  return _
s:     if arg(1)=1  then return arg(3);   return word(arg(2) 's', 1)       /*pluralizer.*/
/*──────────────────────────────────────────────────────────────────────────────────────*/
solve: procedure expose # dir. @. h len next. w; @.=0      /*zero rectangle coördinates.*/
       parse arg h,w,recur                                 /*get values for some args.  */
       if h//2  then do;    t= w;    w= h;      h= t;      if h//2  then return 0
                     end
       if w==1  then return 1
       if w==2  then return h
       if h==2  then return w                    /* [↓]   %  is REXX's integer division.*/
       cy= h % 2;     cx= w % 2;     wp= w + 1   /*cut the  [XY]  rectangle in half.    */
       len= (h+1) * wp - 1                       /*extend the area of the rectangle.    */
       next.0= -1;    next.1= -wp;   next.2= 1;    next.3= wp    /*direction & distance.*/
       if recur  then #= 0
              do x=cx+1  to  w-1;    t= x + cy*wp;     @.t= 1;     _= len - t;      @._= 1
              call walk cy-1, x
              end   /*x*/
       #= #+1
       if h==w  then #= # + #                    /*double the count of rectangle cuts.  */
                else if w//2==0  then if recur  then call solve w, h, 0
       return #
/*──────────────────────────────────────────────────────────────────────────────────────*/
walk:  procedure expose # dir. @. h len next. w wp;       parse arg y,x
       if y==h  then do;   #= #+2;   return;   end    /* ◄──┐      REXX short circuit.  */
       if x==0  then do;   #= #+2;   return;   end    /* ◄──┤        "    "      "      */
       if x==w  then do;   #= #+2;   return;   end    /* ◄──┤        "    "      "      */
       if y==0  then do;   #= #+2;   return;   end    /* ◄──┤        "    "      "      */
       t= x + y*wp;  @.t= @.t + 1;   _= len - t       /*    │ordered by most likely ►──┐*/
       @._= @._+1                                     /*    └──────────────────────────┘*/
                     do j=0  for 4;  _= t + next.j    /*try each of the four directions.*/
                     if @._==0  then do;   yn= y + dir.j.0;     xn= x + dir.j.1
                                           if yn==h  then do;   #= #+2;   iterate;   end
                                           if xn==0  then do;   #= #+2;   iterate;   end
                                           if xn==w  then do;   #= #+2;   iterate;   end
                                           if yn==0  then do;   #= #+2;   iterate;   end
                                           call walk  yn, xn
                                     end
                     end   /*j*/
       @.t= @.t - 1
       _= len - t;       @._= @._ - 1;     return
```

## Ruby

```ruby
def cut_it(h, w)
  if h.odd?
    return 0  if w.odd?
    h, w = w, h
  end
  return 1  if w == 1

  nxt = [[w+1, 1, 0], [-w-1, -1, 0], [-1, 0, -1], [1, 0, 1]]  # [next,dy,dx]
  blen = (h + 1) * (w + 1) - 1
  grid = [false] * (blen + 1)

  walk = lambda do |y, x, count=0|
    return count+1  if y==0 or y==h or x==0 or x==w
    t = y * (w + 1) + x
    grid[t] = grid[blen - t] = true
    nxt.each do |nt, dy, dx|
      count += walk[y + dy, x + dx]  unless grid[t + nt]
    end
    grid[t] = grid[blen - t] = false
    count
  end

  t = h / 2 * (w + 1) + w / 2
  if w.odd?
    grid[t] = grid[t + 1] = true
    count = walk[h / 2, w / 2 - 1]
    count + walk[h / 2 - 1, w / 2] * 2
  else
    grid[t] = true
    count = walk[h / 2, w / 2 - 1]
    return count * 2  if h == w
    count + walk[h / 2 - 1, w / 2]
  end
end

for w in 1..9
  for h in 1..w
    puts "%d x %d: %d" % [w, h, cut_it(w, h)]  if (w * h).even?
  end
end
```


```txt

2 x 1: 1
2 x 2: 2
3 x 2: 3
4 x 1: 1
4 x 2: 4
4 x 3: 9
4 x 4: 22
5 x 2: 5
5 x 4: 39
6 x 1: 1
6 x 2: 6
6 x 3: 23
6 x 4: 90
6 x 5: 263
6 x 6: 1018
7 x 2: 7
7 x 4: 151
7 x 6: 2947
8 x 1: 1
8 x 2: 8
8 x 3: 53
8 x 4: 340
8 x 5: 1675
8 x 6: 11174
8 x 7: 55939
8 x 8: 369050
9 x 2: 9
9 x 4: 553
9 x 6: 31721
9 x 8: 1812667

```



### Show each of the cuts


```ruby
class Rectangle
  DIRS = [[1, 0], [-1, 0], [0, -1], [0, 1]]
  def initialize(h, w)
    raise ArgumentError  if (h.odd? and w.odd?) or h<=0 or w<=0
    @h, @w = h, w
    @limit = h * w / 2
  end

  def cut(disp=true)
    @cut = {}
    @select = []
    @result = []
    @grid = make_grid
    walk(0,0)
    display  if disp
    @result
  end

  def make_grid
    Array.new(@h+1) {|i| Array.new(@w+1) {|j| true if i<@h and j<@w }}
  end

  def walk(y, x)
    @grid[y][x] = @grid[@h-y-1][@w-x-1] = false
    @select.push([y,x])
    select = @select.sort
    unless @cut[select]
      @cut[select] = true
      if @select.size == @limit
        @result << select
      else
        search_next.each {|yy,xx| walk(yy,xx)}
      end
    end
    @select.pop
    @grid[y][x] = @grid[@h-y-1][@w-x-1] = true
  end

  def search_next
    nxt = {}
    @select.each do |y,x|
      DIRS.each do |dy, dx|
        nxt[[y+dy, x+dx]] = true  if @grid[y+dy][x+dx]
      end
    end
    nxt.keys
  end

  def display
    @result.each do |select|
      @grid = make_grid
      select.each {|y,x| @grid[y][x] = false}
      puts to_s
    end
  end

  def to_s
    text = Array.new(@h*2+1) {" " * (@w*4+1)}
    for i in 0..@h
      for j in 0..@w
        text[i*2][j*4+1,3] = "---"  if @grid[i][j] != @grid[i-1][j]
        text[i*2+1][j*4]   = "|"    if @grid[i][j] != @grid[i][j-1]
        text[i*2][j*4]     = "+"
      end
    end
    text.join("\n")
  end
end

rec = Rectangle.new(2,2)
puts rec.cut.size

rec = Rectangle.new(3,4)
puts rec.cut.size
```


```txt
+---+---+
|   |   |
+   +   +
|   |   |
+---+---+
+---+---+
|       |
+---+---+
|       |
+---+---+
2
+---+---+---+---+
|           |   |
+   +   +---+   +
|       |       |
+   +---+   +   +
|   |           |
+---+---+---+---+
+---+---+---+---+
|       |       |
+   +   +   +   +
|       |       |
+   +   +   +   +
|       |       |
+---+---+---+---+
+---+---+---+---+
|           |   |
+   +---+   +   +
|   |   |   |   |
+   +   +---+   +
|   |           |
+---+---+---+---+
+---+---+---+---+
|               |
+   +   +---+---+
|       |       |
+---+---+   +   +
|               |
+---+---+---+---+
+---+---+---+---+
|               |
+   +---+   +---+
|   |   |   |   |
+---+   +---+   +
|               |
+---+---+---+---+
+---+---+---+---+
|   |           |
+   +---+   +   +
|       |       |
+   +   +---+   +
|           |   |
+---+---+---+---+
+---+---+---+---+
|   |           |
+   +   +---+   +
|   |   |   |   |
+   +---+   +   +
|           |   |
+---+---+---+---+
+---+---+---+---+
|               |
+---+   +---+   +
|   |   |   |   |
+   +---+   +---+
|               |
+---+---+---+---+
+---+---+---+---+
|               |
+---+---+   +   +
|       |       |
+   +   +---+---+
|               |
+---+---+---+---+
9

```



## Rust

```rust
fn cwalk(mut vis: &mut Vec<Vec<bool>>, count: &mut isize, w: usize, h: usize, y: usize, x: usize, d: usize) {
    if x == 0 || y == 0 || x == w || y == h {
        *count += 1;
        return;
    }

    vis[y][x] = true;
    vis[h - y][w - x] = true;

    if x != 0 && ! vis[y][x - 1] {
        cwalk(&mut vis, count, w, h, y, x - 1, d | 1);
    }
    if d & 1 != 0 && x < w && ! vis[y][x+1] {
        cwalk(&mut vis, count, w, h, y, x + 1, d | 1);
    }
    if y != 0 && ! vis[y - 1][x] {
        cwalk(&mut vis, count, w, h, y - 1, x, d | 2);
    }
    if d & 2 != 0 && y < h && ! vis[y + 1][x] {
        cwalk(&mut vis, count, w, h, y + 1, x, d | 2);
    }

    vis[y][x] = false;
    vis[h - y][w - x] = false;
}

fn count_only(x: usize, y: usize) -> isize {
    let mut count = 0;
    let mut w = x;
    let mut h = y;

    if (h * w) & 1 != 0 {
        return count;
    }
    if h & 1 != 0 {
        std::mem::swap(&mut w, &mut h);
    }

    let mut vis = vec![vec![false; w + 1]; h + 1];
    vis[h / 2][w / 2] = true;

    if w & 1 != 0 {
        vis[h / 2][w / 2 + 1] = true;
    }
    let mut res;
    if w > 1 {
        cwalk(&mut vis, &mut count, w, h, h / 2, w / 2 - 1, 1);
        res = 2 * count - 1;
        count = 0;
        if w != h {
            cwalk(&mut vis, &mut count, w, h, h / 2 + 1, w / 2, if w & 1 != 0 { 3 } else { 2 });
        }
        res += 2 * count - if w & 1 == 0 { 1 } else { 0 };
    }
    else {
        res = 1;
    }

    if w == h {
        res = 2 * res + 2;
    }
    res
}

fn main() {
    for y in 1..10 {
        for x in 1..y + 1 {
            if x & 1 == 0 || y & 1 == 0 {
                println!("{} x {}: {}", y, x, count_only(x, y));
            }
        }
    }
}

```



## Tcl

```tcl
package require Tcl 8.5

proc walk {y x} {
    global w ww h cnt grid len
    if {!$y || $y==$h || !$x || $x==$w} {
	incr cnt 2
	return
    }
    set t [expr {$y*$ww + $x}]
    set m [expr {$len - $t}]
    lset grid $t [expr {[lindex $grid $t] + 1}]
    lset grid $m [expr {[lindex $grid $m] + 1}]
    if {![lindex $grid [expr {$y*$ww + $x-1}]]} {
	walk $y [expr {$x-1}]
    }
    if {![lindex $grid [expr {($y-1)*$ww + $x}]]} {
	walk [expr {$y-1}] $x
    }
    if {![lindex $grid [expr {$y*$ww + $x+1}]]} {
	walk $y [expr {$x+1}]
    }
    if {![lindex $grid [expr {($y+1)*$ww + $x}]]} {
	walk [expr {$y+1}] $x
    }
    lset grid $t [expr {[lindex $grid $t] - 1}]
    lset grid $m [expr {[lindex $grid $m] - 1}]
}

# Factored out core of [solve]
proc SolveCore {} {
    global w ww h cnt grid len
    set ww [expr {$w+1}]
    set cy [expr {$h / 2}]
    set cx [expr {$w / 2}]

    set len [expr {($h+1) * $ww}]
    set grid [lrepeat $len 0]
    incr len -1

    for {set x $cx;incr x} {$x < $w} {incr x} {
	set t [expr {$cy*$ww+$x}]
	lset grid $t 1
	lset grid [expr {$len - $t}] 1
	walk [expr {$cy - 1}] $x
    }
    incr cnt
}
proc solve {H W} {
    global w h cnt
    set h $H
    set w $W
    if {$h & 1} {
	set h $W
	set w $H
    }
    if {$h & 1} {
	return 0
    }
    if {$w==1} {return 1}
    if {$w==2} {return $h}
    if {$h==2} {return $w}

    set cnt 0
    SolveCore
    if {$h==$w} {
	incr cnt $cnt
    } elseif {!($w & 1)} {
	lassign [list $w $h] h w
	SolveCore
    }
    return $cnt
}

apply {{limit} {
    for {set yy 1} {$yy <= $limit} {incr yy} {
	for {set xx 1} {$xx <= $yy} {incr xx} {
	    if {!($xx&1 && $yy&1)} {
		puts [format "%d x %d: %ld" $yy $xx [solve $yy $xx]]
	    }
	}
    }
}} 10
```

Output is identical.


## zkl

```zkl
fcn cut_it(h,w){
   if(h.isOdd){
      if(w.isOdd) return(0);
      t,h,w=h,w,t;  // swap w,h: a,b=c,d --> a=c; b=d; so need a tmp
   }
   if(w==1) return(1);

   nxt :=T(T(w+1, 1,0), T(-w-1, -1,0), T(-1, 0,-1), T(1, 0,1)); #[next, dy,dx]
   blen:=(h + 1)*(w + 1) - 1;
   grid:=(blen + 1).pump(List(),False); //-->L(False,False...)

   walk:='wrap(y,x){ // lambda closure
      if(y==0 or y==h or x==0 or x==w) return(1);
      count,t:=0,y*(w + 1) + x;
      grid[t]=grid[blen - t]=True;
      foreach nt,dy,dx in (nxt){
         if(not grid[t + nt]) count+=self.fcn(y + dy, x + dx,vm.pasteArgs(2));
      }
      grid[t]=grid[blen - t]=False;
      count
   };

   t:=h/2*(w + 1) + w/2;
   if(w.isOdd){
      grid[t]=grid[t + 1]=True;
      count:=walk(h/2, w/2 - 1);
      count + walk(h/2 - 1, w/2)*2;
   }else{
      grid[t]=True;
      count:=walk(h/2, w/2 - 1);
      if(h==w) return(count*2);
      count + walk(h/2 - 1, w/2);
   }
}
```

Note the funkiness in walk: vm.pasteArgs. This is because zkl functions are unaware of their scope, so a closure is needed (when calling walk) to capture state (nxt, blen, grid, h, w). Rather than creating a closure object each call, that state is passed in the arg list. So, when doing recursion, that state needs to be restored to the stack (the compiler isn't smart enough to recognize this case).

```zkl
foreach w,h in ([1..9],[1..w]){
   if((w*h).isEven) println("%d x %d: %d".fmt(w, h, cut_it(w,h)));
}
```

Output is identical.

```txt

2 x 1: 1
2 x 2: 2
3 x 2: 3
4 x 1: 1
4 x 2: 4
4 x 3: 9
4 x 4: 22
...

9 x 2: 9
9 x 4: 553
9 x 6: 31721

```


