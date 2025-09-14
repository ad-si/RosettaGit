+++
title = "A* search algorithm"
description = ""
date = 2019-09-28T21:05:37Z
aliases = []
[extra]
id = 21248
[taxonomies]
categories = ["Routing algorithms", "task"]
tags = []
languages = [
  "c",
  "cpp",
  "d",
  "go",
  "java",
  "javascript",
  "julia",
  "kotlin",
  "lua",
  "ol",
  "phix",
  "powershell",
  "python",
  "racket",
  "rexx",
  "sequencel",
  "sidef",
  "unix_shell",
  "zkl",
]
+++

## Task
<!--    A*    is pronounced as:   A star   !--> 

The A* search algorithm is an extension of [[Dijkstra's algorithm]] useful for finding the lowest cost path between two nodes (aka vertices) of a graph. The path may traverse any number of nodes connected by edges (aka arcs) with each edge having an associated cost. The algorithm uses a heuristic which associates an estimate of the lowest cost path from this node to the goal node, such that this estimate is never greater than the actual cost.

The algorithm should not assume that all edge costs are the same. It should be possible to start and finish on any node, including ones identified as a barrier in the task. 

;Task
Consider the problem of finding a route across the diagonal of a chess board-like 8x8 grid. The rows are numbered from 0 to 7. The columns are also numbered 0 to 7. The start position is (0, 0) and the end position is (7, 7). Movement is allow by one square in any direction including diagonals, similar to a king in chess. The standard movement cost is 1. To make things slightly harder, there is a barrier that occupy certain positions of the grid. Moving into any of the barrier positions has a cost of 100. 

The barrier occupies the positions (2,4), (2,5), (2,6), (3,6), (4,6), (5,6), (5,5), (5,4), (5,3), (5,2), (4,2) and (3,2). 

A route with the lowest cost should be found using the A* search algorithm (there are multiple optimal solutions with the same total cost).

Print the optimal route in text format, as well as the total cost of the route.

Optionally, draw the optimal route and the barrier positions.

Note: using a heuristic score of zero is equivalent to Dijkstra's algorithm and that's kind of cheating/not really A*!

;''Extra Credit''
Use this algorithm to solve an 8 puzzle. Each node of the input graph will represent an arrangement of the tiles. The nodes will be connected by 4 edges representing swapping the blank tile up, down, left, or right. The cost of each edge is 1. The heuristic will be the sum of the manhatten distance of each numbered tile from its goal position. An 8 puzzle graph will have 9!/2 (181,440) nodes. The 15 puzzle has over 10 trillion nodes. This algorithm may solve simple 15 puzzles (but there are not many of those).



;See also:
* Wikipedia webpage:   [https://en.wikipedia.org/wiki/A*_search_algorithm A* search algorithm].
* [https://www.redblobgames.com/pathfinding/a-star/introduction.html An introduction to: Breadth First Search |> Dijkstra’s Algorithm |> ''A*'']


;Related tasks:
* [[15 puzzle solver]]
* [[Dijkstra's algorithm]]
* [[Knapsack problem/0-1]]





## C


```c

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <float.h>
/* and not not_eq */
#include <iso646.h>
/* add -lm to command line to compile with this header */
#include <math.h>

#define map_size_rows 10
#define map_size_cols 10

char map[map_size_rows][map_size_cols] = {
    {1, 1, 1, 1, 1, 1, 1, 1, 1, 1},
    {1, 0, 0, 0, 0, 0, 0, 0, 0, 1},
    {1, 0, 0, 0, 0, 0, 0, 0, 0, 1},
    {1, 0, 0, 0, 0, 1, 1, 1, 0, 1},
    {1, 0, 0, 1, 0, 0, 0, 1, 0, 1},
    {1, 0, 0, 1, 0, 0, 0, 1, 0, 1},
    {1, 0, 0, 1, 1, 1, 1, 1, 0, 1},
    {1, 0, 0, 0, 0, 0, 0, 0, 0, 1},
    {1, 0, 0, 0, 0, 0, 0, 0, 0, 1},
    {1, 1, 1, 1, 1, 1, 1, 1, 1, 1}
};

/* description of graph node */
struct stop {
    double col, row;
    /* array of indexes of routes from this stop to neighbours in array of all routes */
    int * n;
    int n_len;
    double f, g, h;
    int from;
};

int ind[map_size_rows][map_size_cols] = {
    {-1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
    {-1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
    {-1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
    {-1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
    {-1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
    {-1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
    {-1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
    {-1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
    {-1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
    {-1, -1, -1, -1, -1, -1, -1, -1, -1, -1}
};

/* description of route between two nodes */
struct route {
    /* route has only one direction! */
    int x; /* index of stop in array of all stops of src of this route */
    int y; /* intex of stop in array of all stops od dst of this route */
    double d;
};

int main() {
    int i, j, k, l, b, found;
    int p_len = 0;
    int * path = NULL;
    int c_len = 0;
    int * closed = NULL;
    int o_len = 1;
    int * open = (int*)calloc(o_len, sizeof(int));
    double min, tempg;
    int s;
    int e;
    int current;
    int s_len = 0;
    struct stop * stops = NULL;
    int r_len = 0;
    struct route * routes = NULL;

    for (i = 1; i < map_size_rows - 1; i++) {
        for (j = 1; j < map_size_cols - 1; j++) {
            if (!map[i][j]) {
                ++s_len;
                stops = (struct stop *)realloc(stops, s_len * sizeof(struct stop));
                int t = s_len - 1;
                stops[t].col = j;
                stops[t].row = i;
                stops[t].from = -1;
                stops[t].g = DBL_MAX;
                stops[t].n_len = 0;
                stops[t].n = NULL;
                ind[i][j] = t;
            }
        }
    }

    /* index of start stop */
    s = 0;
    /* index of finish stop */
    e = s_len - 1;

    for (i = 0; i < s_len; i++) {
        stops[i].h = sqrt(pow(stops[e].row - stops[i].row, 2) + pow(stops[e].col - stops[i].col, 2));
    }

    for (i = 1; i < map_size_rows - 1; i++) {
        for (j = 1; j < map_size_cols - 1; j++) {
            if (ind[i][j] >= 0) {
                for (k = i - 1; k <= i + 1; k++) {
                    for (l = j - 1; l <= j + 1; l++) {
                        if ((k == i) and (l == j)) {
                            continue;
                        }
                        if (ind[k][l] >= 0) {
                            ++r_len;
                            routes = (struct route *)realloc(routes, r_len * sizeof(struct route));
                            int t = r_len - 1;
                            routes[t].x = ind[i][j];
                            routes[t].y = ind[k][l];
                            routes[t].d = sqrt(pow(stops[routes[t].y].row - stops[routes[t].x].row, 2) + pow(stops[routes[t].y].col - stops[routes[t].x].col, 2));
                            ++stops[routes[t].x].n_len;
                            stops[routes[t].x].n = (int*)realloc(stops[routes[t].x].n, stops[routes[t].x].n_len * sizeof(int));
                            stops[routes[t].x].n[stops[routes[t].x].n_len - 1] = t;
                        }
                    }
                }
            }
        }
    }

    open[0] = s;
    stops[s].g = 0;
    stops[s].f = stops[s].g + stops[s].h;
    found = 0;

    while (o_len and not found) {
        min = DBL_MAX;

        for (i = 0; i < o_len; i++) {
            if (stops[open[i]].f < min) {
                current = open[i];
                min = stops[open[i]].f;
            }
        }

        if (current == e) {
            found = 1;

            ++p_len;
            path = (int*)realloc(path, p_len * sizeof(int));
            path[p_len - 1] = current;
            while (stops[current].from >= 0) {
                current = stops[current].from;
                ++p_len;
                path = (int*)realloc(path, p_len * sizeof(int));
                path[p_len - 1] = current;
            }
        }

        for (i = 0; i < o_len; i++) {
            if (open[i] == current) {
                if (i not_eq (o_len - 1)) {
                    for (j = i; j < (o_len - 1); j++) {
                        open[j] = open[j + 1];
                    }
                }
                --o_len;
                open = (int*)realloc(open, o_len * sizeof(int));
                break;
            }
        }

        ++c_len;
        closed = (int*)realloc(closed, c_len * sizeof(int));
        closed[c_len - 1] = current;

        for (i = 0; i < stops[current].n_len; i++) {
            b = 0;

            for (j = 0; j < c_len; j++) {
                if (routes[stops[current].n[i]].y == closed[j]) {
                    b = 1;
                }
            }

            if (b) {
                continue;
            }

            tempg = stops[current].g + routes[stops[current].n[i]].d;

            b = 1;

            if (o_len > 0) {
                for (j = 0; j < o_len; j++) {
                    if (routes[stops[current].n[i]].y == open[j]) {
                        b = 0;
                    }
                }
            }

            if (b or (tempg < stops[routes[stops[current].n[i]].y].g)) {
                stops[routes[stops[current].n[i]].y].from = current;
                stops[routes[stops[current].n[i]].y].g = tempg;
                stops[routes[stops[current].n[i]].y].f = stops[routes[stops[current].n[i]].y].g + stops[routes[stops[current].n[i]].y].h;

                if (b) {
                    ++o_len;
                    open = (int*)realloc(open, o_len * sizeof(int));
                    open[o_len - 1] = routes[stops[current].n[i]].y;
                }
            }
        }
    }

    for (i = 0; i < map_size_rows; i++) {
        for (j = 0; j < map_size_cols; j++) {
            if (map[i][j]) {
                putchar(0xdb);
            } else {
                b = 0;
                for (k = 0; k < p_len; k++) {
                    if (ind[i][j] == path[k]) {
                        ++b;
                    }
                }
                if (b) {
                    putchar('x');
                } else {
                    putchar('.');
                }
            }
        }
        putchar('\n');
    }

    if (not found) {
        puts("IMPOSSIBLE");
    } else {
        printf("path cost is %d:\n", p_len);
        for (i = p_len - 1; i >= 0; i--) {
            printf("(%1.0f, %1.0f)\n", stops[path[i]].col, stops[path[i]].row);
        }
    }

    for (i = 0; i < s_len; ++i) {
        free(stops[i].n);
    }
    free(stops);
    free(routes);
    free(path);
    free(open);
    free(closed);

    return 0;
}

```

{{out}}

```txt

▒▒▒▒▒▒▒▒▒▒
▒x.......▒
▒.x......▒
▒.x..▒▒▒.▒
▒.x▒...▒.▒
▒.x▒...▒.▒
▒.x▒▒▒▒▒.▒
▒..xxxxx.▒
▒.......x▒
▒▒▒▒▒▒▒▒▒▒
path cost is 12:
(1, 1)
(2, 2)
(2, 3)
(2, 4)
(2, 5)
(2, 6)
(3, 7)
(4, 7)
(5, 7)
(6, 7)
(7, 7)
(8, 8)

```



## C++


```cpp

#include <list>
#include <algorithm>
#include <iostream>
 
class point {
public:
    point( int a = 0, int b = 0 ) { x = a; y = b; }
    bool operator ==( const point& o ) { return o.x == x && o.y == y; }
    point operator +( const point& o ) { return point( o.x + x, o.y + y ); }
    int x, y;
};
 
class map {
public:
    map() {
        char t[8][8] = {
            {0, 0, 0, 0, 0, 0, 0, 0}, {0, 0, 0, 0, 0, 0, 0, 0},
            {0, 0, 0, 0, 1, 1, 1, 0}, {0, 0, 1, 0, 0, 0, 1, 0},
            {0, 0, 1, 0, 0, 0, 1, 0}, {0, 0, 1, 1, 1, 1, 1, 0},
            {0, 0, 0, 0, 0, 0, 0, 0}, {0, 0, 0, 0, 0, 0, 0, 0}
        };
        w = h = 8;
        for( int r = 0; r < h; r++ )
            for( int s = 0; s < w; s++ )
                m[s][r] = t[r][s];
    }
    int operator() ( int x, int y ) { return m[x][y]; }
    char m[8][8];
    int w, h;
};
 
class node {
public:
    bool operator == (const node& o ) { return pos == o.pos; }
    bool operator == (const point& o ) { return pos == o; }
    bool operator < (const node& o ) { return dist + cost < o.dist + o.cost; }
    point pos, parent;
    int dist, cost;
};
 
class aStar {
public:
    aStar() {
        neighbours[0] = point( -1, -1 ); neighbours[1] = point(  1, -1 );
        neighbours[2] = point( -1,  1 ); neighbours[3] = point(  1,  1 );
        neighbours[4] = point(  0, -1 ); neighbours[5] = point( -1,  0 );
        neighbours[6] = point(  0,  1 ); neighbours[7] = point(  1,  0 );
    }
 
    int calcDist( point& p ){
        // need a better heuristic
        int x = end.x - p.x, y = end.y - p.y;
        return( x * x + y * y );
    }
 
    bool isValid( point& p ) {
        return ( p.x >-1 && p.y > -1 && p.x < m.w && p.y < m.h );
    }
 
    bool existPoint( point& p, int cost ) {
        std::list<node>::iterator i;
        i = std::find( closed.begin(), closed.end(), p );
        if( i != closed.end() ) {
            if( ( *i ).cost + ( *i ).dist < cost ) return true;
            else { closed.erase( i ); return false; }
        }
        i = std::find( open.begin(), open.end(), p );
        if( i != open.end() ) {
            if( ( *i ).cost + ( *i ).dist < cost ) return true;
            else { open.erase( i ); return false; }
        }
        return false;
    }
 
    bool fillOpen( node& n ) {
        int stepCost, nc, dist;
        point neighbour;

        for( int x = 0; x < 8; x++ ) {
            // one can make diagonals have different cost
            stepCost = x < 4 ? 1 : 1;
            neighbour = n.pos + neighbours[x];
            if( neighbour == end ) return true;
 
            if( isValid( neighbour ) && m( neighbour.x, neighbour.y ) != 1 ) {
                nc = stepCost + n.cost;
                dist = calcDist( neighbour );
                if( !existPoint( neighbour, nc + dist ) ) {
                    node m;
                    m.cost = nc; m.dist = dist;
                    m.pos = neighbour; 
                    m.parent = n.pos;
                    open.push_back( m );
                }
            }
        }
        return false;
    }
 
    bool search( point& s, point& e, map& mp ) {
        node n; end = e; start = s; m = mp;
        n.cost = 0; n.pos = s; n.parent = 0; n.dist = calcDist( s ); 
        open.push_back( n );
        while( !open.empty() ) {
            //open.sort();
            node n = open.front();
            open.pop_front();
            closed.push_back( n );
            if( fillOpen( n ) ) return true;
        }
        return false;
    }
 
    int path( std::list<point>& path ) {
        path.push_front( end );
        int cost = 1 + closed.back().cost; 
        path.push_front( closed.back().pos );
        point parent = closed.back().parent;
 
        for( std::list<node>::reverse_iterator i = closed.rbegin(); i != closed.rend(); i++ ) {
            if( ( *i ).pos == parent && !( ( *i ).pos == start ) ) {
                path.push_front( ( *i ).pos );
                parent = ( *i ).parent;
            }
        }
        path.push_front( start );
        return cost;
    }
 
    map m; point end, start;
    point neighbours[8];
    std::list<node> open;
    std::list<node> closed;
};
 
int main( int argc, char* argv[] ) {
    map m;
    point s, e( 7, 7 );
    aStar as;
 
    if( as.search( s, e, m ) ) {
        std::list<point> path;
        int c = as.path( path );
        for( int y = -1; y < 9; y++ ) {
            for( int x = -1; x < 9; x++ ) {
                if( x < 0 || y < 0 || x > 7 || y > 7 || m( x, y ) == 1 )
                    std::cout << char(0xdb);
                else {
                    if( std::find( path.begin(), path.end(), point( x, y ) )!= path.end() )
                        std::cout << "x";
                    else std::cout << ".";
                }
            }
            std::cout << "\n";
        }
 
        std::cout << "\nPath cost " << c << ": ";
        for( std::list<point>::iterator i = path.begin(); i != path.end(); i++ ) {
            std::cout<< "(" << ( *i ).x << ", " << ( *i ).y << ") ";
        }
    }
    std::cout << "\n\n";
    return 0;
}

```

{{out}}

```txt

██████████
█x.......█
█x.......█
█x...███.█
█x.█...█.█
█x.█...█.█
█.x█████.█
█..xxxx..█
█......xx█
██████████

Path cost 11: (0, 0) (0, 1) (0, 2) (0, 3) (0, 4) (1, 5) (2, 6) (3, 6) (4, 6) (5, 6) (6, 7) (7, 7)

```




## D

ported from c++ code

```D


import std.stdio;
import std.algorithm;
import std.range;
import std.array;
 
struct Point {
    int x;
    int y;
    Point opBinary(string op = "+")(Point o) { return Point( o.x + x, o.y + y ); }
}

struct Map {
    int w = 8;
    int h = 8;
    bool[][] m = [
            [0, 0, 0, 0, 0, 0, 0, 0], [0, 0, 0, 0, 0, 0, 0, 0],
            [0, 0, 0, 0, 1, 1, 1, 0], [0, 0, 1, 0, 0, 0, 1, 0],
            [0, 0, 1, 0, 0, 0, 1, 0], [0, 0, 1, 1, 1, 1, 1, 0],
            [0, 0, 0, 0, 0, 0, 0, 0], [0, 0, 0, 0, 0, 0, 0, 0]
        ];
}
  
struct Node {
    Point pos;
    Point parent;
    int dist;
    int cost;
    bool opEquals(const Node n) { return pos == n.pos;  }
    bool opEquals(const Point p) { return pos == p;  }
    int opCmp(ref const Node n) const { return (n.dist + n.cost) - (dist + cost); }
};

struct AStar {
    Map m;
    Point end;
    Point start;
   	Point[8] neighbours = [Point(-1,-1), Point(1,-1), Point(-1,1), Point(1,1), Point(0,-1), Point(-1,0), Point(0,1), Point(1,0)];
    Node[] open;
    Node[] closed;

    int calcDist(Point b) {
        // need a better heuristic
        int x = end.x - b.x, y = end.y - b.y;
        return( x * x + y * y );
    }
    
    bool isValid(Point b) {
        return ( b.x >-1 && b.y > -1 && b.x < m.w && b.y < m.h );
    }
 
    bool existPoint(Point b, int cost) {
        auto i = closed.countUntil(b);
        if( i != -1 ) {
            if( closed[i].cost + closed[i].dist < cost ) return true;
            else { closed = closed.remove!(SwapStrategy.stable)(i); return false; }
        }
        i = open.countUntil(b);
        if( i != -1 ) {
            if( open[i].cost + open[i].dist < cost ) return true;
            else { open = open.remove!(SwapStrategy.stable)(i); return false; }
        }
        return false;
    }
 
    bool fillOpen( ref Node n ) {
        int stepCost;
        int nc;
        int dist;
        Point neighbour;
 
        for( int x = 0; x < 8; ++x ) {
            // one can make diagonals have different cost
            stepCost = x < 4 ? 1 : 1;
            neighbour = n.pos + neighbours[x];
            if( neighbour == end ) return true;
 
            if( isValid( neighbour ) && m.m[neighbour.y][neighbour.x] != 1 ) {
                nc = stepCost + n.cost;
                dist = calcDist( neighbour );
                if( !existPoint( neighbour, nc + dist ) ) {
                    Node m;
                    m.cost = nc; m.dist = dist;
                    m.pos = neighbour; 
                    m.parent = n.pos;
                    open ~= m;
                }
            }
        }
        return false;
    }
 
    bool search( ref Point s, ref Point e, ref Map mp ) {
        Node n; end = e; start = s; m = mp;
        n.cost = 0; 
        n.pos = s; 
        n.parent = Point(); 
        n.dist = calcDist( s ); 
        open ~= n ;
        while( !open.empty() ) {
            //open.sort();
            Node nx = open.front();
            open = open.drop(1).array;
            closed ~= nx ;
            if( fillOpen( nx ) ) return true;
        }
        return false;
    }
 
    int path( ref Point[] path ) {
        path = end ~ path;
        int cost = 1 + closed.back().cost; 
        path = closed.back().pos ~ path;
        Point parent = closed.back().parent;
 
        foreach(ref i ; closed.retro) {
            if( i.pos == parent && !( i.pos == start ) ) {
                path = i.pos ~ path;
                parent = i.parent;
            }
        }
        path = start ~ path;
        return cost;
    }
};
 
int main(string[] argv) {
    Map m;
    Point s;
    Point e = Point( 7, 7 );
    AStar as;
 
    if( as.search( s, e, m ) ) {
        Point[] path;
        int c = as.path( path );
        for( int y = -1; y < 9; y++ ) {
            for( int x = -1; x < 9; x++ ) {
                if( x < 0 || y < 0 || x > 7 || y > 7 || m.m[y][x] == 1 )
                    write(cast(char)0xdb);
                else {
                    if( path.canFind(Point(x,y)))
                        write("x");
                    else write(".");
                }
            }
            writeln();
        }
 
        write("\nPath cost ", c, ": ");
        foreach( i; path ) {
            write("(", i.x, ", ", i.y, ") ");
        }
    }
	write("\n\n");
    return 0;
}

```

{{out}}

```txt

██████████
█x.......█
█x.......█
█x...███.█
█x.█...█.█
█x.█...█.█
█.x█████.█
█..xxxx..█
█......xx█
██████████

Path cost 11: (0, 0) (0, 1) (0, 2) (0, 3) (0, 4) (1, 5) (2, 6) (3, 6) (4, 6) (5, 6) (6, 7) (7, 7)

```



## Go


```go
// Package astar implements the A* search algorithm with minimal constraints
// on the graph representation.
package astar

import "container/heap"

// Exported node type.
type Node interface {
    To() []Arc               // return list of arcs from this node to another
    Heuristic(from Node) int // heuristic cost from another node to this one
}

// An Arc, actually a "half arc", leads to another node with integer cost.
type Arc struct {
    To   Node
    Cost int
}

// rNode holds data for a "reached" node
type rNode struct {
    n    Node
    from Node
    l    int // route len
    g    int // route cost
    f    int // "g+h", route cost + heuristic estimate
    fx   int // heap.Fix index
}

type openHeap []*rNode // priority queue

// Route computes a route from start to end nodes using the A* algorithm.
//
// The algorithm is general A*, where the heuristic is not required to be
// monotonic.  If a route exists, the function will find a route regardless
// of the quality of the Heuristic.  For an admissiable heuristic, the route
// will be optimal.
func Route(start, end Node) (route []Node, cost int) {
    // start node initialized with heuristic
    cr := &rNode{n: start, l: 1, f: end.Heuristic(start)}
    // maintain a set of reached nodes.  start is reached initially
    r := map[Node]*rNode{start: cr}
    // oh is a heap of nodes "open" for exploration.  nodes go on the heap
    // when they get an initial or new "g" route distance, and therefore a
    // new "f" which serves as priority for exploration.
    oh := openHeap{cr}
    for len(oh) > 0 {
        bestRoute := heap.Pop(&oh).(*rNode)
        bestNode := bestRoute.n
        if bestNode == end {
            // done.  prepare return values
            cost = bestRoute.g
            route = make([]Node, bestRoute.l)
            for i := len(route) - 1; i >= 0; i-- {
                route[i] = bestRoute.n
                bestRoute = r[bestRoute.from]
            }
            return
        }
        l := bestRoute.l + 1
        for _, to := range bestNode.To() {
            // "g" route distance from start
            g := bestRoute.g + to.Cost
            if alt, ok := r[to.To]; !ok {
                // alt being reached for the first time
                alt = &rNode{n: to.To, from: bestNode, l: l,
                    g: g, f: g + end.Heuristic(to.To)}
                r[to.To] = alt
                heap.Push(&oh, alt)
            } else {
                if g >= alt.g {
                    continue // candidate route no better than existing route
                }
                // it's a better route
                // update data and make sure it's on the heap
                alt.from = bestNode
                alt.l = l
                alt.g = g
                alt.f = end.Heuristic(alt.n)
                if alt.fx < 0 {
                    heap.Push(&oh, alt)
                } else {
                    heap.Fix(&oh, alt.fx)
                }
            }
        }
    }
    return nil, 0
}

// implement container/heap
func (h openHeap) Len() int           { return len(h) }
func (h openHeap) Less(i, j int) bool { return h[i].f < h[j].f }
func (h openHeap) Swap(i, j int) {
    h[i], h[j] = h[j], h[i]
    h[i].fx = i
    h[j].fx = j
}

func (p *openHeap) Push(x interface{}) {
    h := *p
    fx := len(h)
    h = append(h, x.(*rNode))
    h[fx].fx = fx
    *p = h
}

func (p *openHeap) Pop() interface{} {
    h := *p
    last := len(h) - 1
    *p = h[:last]
    h[last].fx = -1
    return h[last]
}
```


```go
package main

import (
    "fmt"

    "astar"
)

// rcNode implements the astar.Node interface
type rcNode struct{ r, c int }

var barrier = map[rcNode]bool{{2, 4}: true, {2, 5}: true,
    {2, 6}: true, {3, 6}: true, {4, 6}: true, {5, 6}: true, {5, 5}: true,
    {5, 4}: true, {5, 3}: true, {5, 2}: true, {4, 2}: true, {3, 2}: true}

// graph representation is virtual.  Arcs from a node are generated when
// requested, but there is no static graph representation.
func (fr rcNode) To() (a []astar.Arc) {
    for r := fr.r - 1; r <= fr.r+1; r++ {
        for c := fr.c - 1; c <= fr.c+1; c++ {
            if (r == fr.r && c == fr.c) || r < 0 || r > 7 || c < 0 || c > 7 {
                continue
            }
            n := rcNode{r, c}
            cost := 1
            if barrier[n] {
                cost = 100
            }
            a = append(a, astar.Arc{n, cost})
        }
    }
    return a
}

// The heuristic computed is max of row distance and column distance.
// This is effectively the cost if there were no barriers.
func (n rcNode) Heuristic(fr astar.Node) int {
    dr := n.r - fr.(rcNode).r
    if dr < 0 {
        dr = -dr
    }
    dc := n.c - fr.(rcNode).c
    if dc < 0 {
        dc = -dc
    }
    if dr > dc {
        return dr
    }
    return dc
}

func main() {
    route, cost := astar.Route(rcNode{0, 0}, rcNode{7, 7})
    fmt.Println("Route:", route)
    fmt.Println("Cost:", cost)
}
```

{{out}}

```txt

Route: [{0 0} {1 1} {2 2} {3 1} {4 1} {5 1} {6 2} {6 3} {6 4} {6 5} {6 6} {7 7}]
Cost: 11

```


## Java


```java

package astar;

import java.util.List;
import java.util.ArrayList;
import java.util.Collections;

class AStar {
    private final List<Node> open;
    private final List<Node> closed;
    private final List<Node> path;
    private final int[][] maze;
    private Node now;
    private final int xstart;
    private final int ystart;
    private int xend, yend;
    private final boolean diag;
	
    // Node class for convienience
    static class Node implements Comparable {
        public Node parent;
        public int x, y;
        public double g;
        public double h;
        Node(Node parent, int xpos, int ypos, double g, double h) {
            this.parent = parent;
            this.x = xpos;
            this.y = ypos;
            this.g = g;
            this.h = h;
       }
       // Compare by f value (g + h)
       @Override
       public int compareTo(Object o) {
           Node that = (Node) o;
           return (int)((this.g + this.h) - (that.g + that.h));
       }
   }

    AStar(int[][] maze, int xstart, int ystart, boolean diag) {
        this.open = new ArrayList<>();
        this.closed = new ArrayList<>();
        this.path = new ArrayList<>();
        this.maze = maze;
        this.now = new Node(null, xstart, ystart, 0, 0);
        this.xstart = xstart;
        this.ystart = ystart;
        this.diag = diag;
    }
    /*
    ** Finds path to xend/yend or returns null
    **
    ** @param (int) xend coordinates of the target position
    ** @param (int) yend
    ** @return (List<Node> | null) the path
    */
    public List<Node> findPathTo(int xend, int yend) {
        this.xend = xend;
        this.yend = yend;
        this.closed.add(this.now);
        addNeigborsToOpenList();
        while (this.now.x != this.xend || this.now.y != this.yend) {
            if (this.open.isEmpty()) { // Nothing to examine
                return null;
            }
            this.now = this.open.get(0); // get first node (lowest f score)
            this.open.remove(0); // remove it
            this.closed.add(this.now); // and add to the closed
            addNeigborsToOpenList();
        }
        this.path.add(0, this.now);
        while (this.now.x != this.xstart || this.now.y != this.ystart) {
            this.now = this.now.parent;
            this.path.add(0, this.now);
        }
        return this.path;
    }
    /*
    ** Looks in a given List<> for a node
    **
    ** @return (bool) NeightborInListFound
    */
    private static boolean findNeighborInList(List<Node> array, Node node) {
        return array.stream().anyMatch((n) -> (n.x == node.x && n.y == node.y));
    }
    /*
    ** Calulate distance between this.now and xend/yend
    **
    ** @return (int) distance
    */
    private double distance(int dx, int dy) {
        if (this.diag) { // if diagonal movement is alloweed
            return Math.hypot(this.now.x + dx - this.xend, this.now.y + dy - this.yend); // return hypothenuse
        } else {
            return Math.abs(this.now.x + dx - this.xend) + Math.abs(this.now.y + dy - this.yend); // else return "Manhattan distance"
        }
    }
    private void addNeigborsToOpenList() {
        Node node;
        for (int x = -1; x <= 1; x++) {
            for (int y = -1; y <= 1; y++) {
                if (!this.diag && x != 0 && y != 0) {
                    continue; // skip if diagonal movement is not allowed
                }
                node = new Node(this.now, this.now.x + x, this.now.y + y, this.now.g, this.distance(x, y));
                if ((x != 0 || y != 0) // not this.now
                    && this.now.x + x >= 0 && this.now.x + x < this.maze[0].length // check maze boundaries
                    && this.now.y + y >= 0 && this.now.y + y < this.maze.length
                    && this.maze[this.now.y + y][this.now.x + x] != -1 // check if square is walkable
                    && !findNeighborInList(this.open, node) && !findNeighborInList(this.closed, node)) { // if not already done
                        node.g = node.parent.g + 1.; // Horizontal/vertical cost = 1.0
                        node.g += maze[this.now.y + y][this.now.x + x]; // add movement cost for this square

                        // diagonal cost = sqrt(hor_cost² + vert_cost²)
                        // in this example the cost would be 12.2 instead of 11
                        /*
                        if (diag && x != 0 && y != 0) {
                            node.g += .4;	// Diagonal movement cost = 1.4
                        }
                        */
                        this.open.add(node);
                }
            }
        }
        Collections.sort(this.open);
    }
	
    public static void main(String[] args) {
        // -1 = blocked
        // 0+ = additional movement cost
        int[][] maze = {
            {  0,  0,  0,  0,  0,  0,  0,  0},
            {  0,  0,  0,  0,  0,  0,  0,  0},
            {  0,  0,  0,100,100,100,  0,  0},
            {  0,  0,  0,  0,  0,100,  0,  0},
            {  0,  0,100,  0,  0,100,  0,  0},
            {  0,  0,100,  0,  0,100,  0,  0},
            {  0,  0,100,100,100,100,  0,  0},
            {  0,  0,  0,  0,  0,  0,  0,  0},
        };
        AStar as = new AStar(maze, 0, 0, true);
        List<Node> path = as.findPathTo(7, 7);
        if (path != null) {
            path.forEach((n) -> {
                System.out.print("[" + n.x + ", " + n.y + "] ");
                maze[n.y][n.x] = -1;
            });
            System.out.printf("\nTotal cost: %.02f\n", path.get(path.size() - 1).g);

            for (int[] maze_row : maze) {
                for (int maze_entry : maze_row) {
                    switch (maze_entry) {
                        case 0:
                            System.out.print("_");
                            break;
                        case -1:
                            System.out.print("*");
                            break;
                        default:
                            System.out.print("#");
                    }
                }
                System.out.println();
            }
        }
    }
}

```

{{out}}

```txt

[0, 0] [1, 0] [2, 0] [3, 0] [4, 0] [5, 1] [6, 2] [7, 3] [6, 4] [6, 5] [6, 6] [7, 7] 
Total cost: 11,00
*****___
_____*__
___###*_
_____#_*
__#__#*_
__#__#*_
__####*_
_______*

```



## JavaScript

Animated.<br />To see how it works on a random map go [http://paulo-jorente.de/tests/astar/ here]

```javascript

var ctx, map, opn = [], clsd = [], start = {x:1, y:1, f:0, g:0}, 
goal = {x:8, y:8, f:0, g:0}, mw = 10, mh = 10, neighbours, path;

function findNeighbour( arr, n ) {
    var a;
    for( var i = 0; i < arr.length; i++ ) {
        a = arr[i];
        if( n.x === a.x && n.y === a.y ) return i;
    }
    return -1;
}
function addNeighbours( cur ) {
    var p;
    for( var i = 0; i < neighbours.length; i++ ) {
        var n = {x: cur.x + neighbours[i].x, y: cur.y + neighbours[i].y, g: 0, h: 0, prt: {x:cur.x, y:cur.y}};
        if( map[n.x][n.y] == 1 || findNeighbour( clsd, n ) > -1 ) continue;
        n.g = cur.g + neighbours[i].c; n.h = Math.abs( goal.x - n.x ) + Math.abs( goal.y - n.y );
        p = findNeighbour( opn, n );
        if( p > -1 && opn[p].g + opn[p].h <= n.g + n.h ) continue;
        opn.push( n );
    }
    opn.sort( function( a, b ) {
        return ( a.g + a.h ) - ( b.g + b.h ); } );
}
function createPath() {
    path = [];
    var a, b;
    a = clsd.pop();
    path.push( a );
    while( clsd.length ) {
        b = clsd.pop();
        if( b.x != a.prt.x || b.y != a.prt.y ) continue;
        a = b; path.push( a );
    }
 }
function solveMap() {
    drawMap();
    if( opn.length < 1 ) {
        document.body.appendChild( document.createElement( "p" ) ).innerHTML = "Impossible!";
        return;
    }
    var cur = opn.splice( 0, 1 )[0];
    clsd.push( cur );
    if( cur.x == goal.x && cur.y == goal.y ) {
        createPath(); drawMap();
        return;
    }
    addNeighbours( cur );
    requestAnimationFrame( solveMap );
}
function drawMap() {
    ctx.fillStyle = "#ee6"; ctx.fillRect( 0, 0, 200, 200 );
    for( var j = 0; j < mh; j++ ) {
        for( var i = 0; i < mw; i++ ) {
            switch( map[i][j] ) {
                case 0: continue;
                case 1: ctx.fillStyle = "#990"; break;
                case 2: ctx.fillStyle = "#090"; break;
                case 3: ctx.fillStyle = "#900"; break;
            }
            ctx.fillRect( i, j, 1, 1 );
        }
    }
    var a;
    if( path.length ) {
        var txt = "Path: " + ( path.length - 1 ) + "<br />[";
        for( var i = path.length - 1; i > -1; i-- ) {
            a = path[i];
            ctx.fillStyle = "#999";
            ctx.fillRect( a.x, a.y, 1, 1 );
            txt += "(" + a.x + ", " + a.y + ") ";
        }
        document.body.appendChild( document.createElement( "p" ) ).innerHTML = txt + "]";
        return;
    }
    for( var i = 0; i < opn.length; i++ ) {
        a = opn[i];
        ctx.fillStyle = "#909";
        ctx.fillRect( a.x, a.y, 1, 1 );
    }
    for( var i = 0; i < clsd.length; i++ ) {
        a = clsd[i];
        ctx.fillStyle = "#009";
        ctx.fillRect( a.x, a.y, 1, 1 );
    }
}
function createMap() {
    map = new Array( mw );
    for( var i = 0; i < mw; i++ ) {
        map[i] = new Array( mh );
        for( var j = 0; j < mh; j++ ) {
            if( !i || !j || i == mw - 1 || j == mh - 1 ) map[i][j] = 1;
            else map[i][j] = 0;
        }
    }
    map[5][3] = map[6][3] = map[7][3] = map[3][4] = map[7][4] = map[3][5] = 
    map[7][5] = map[3][6] = map[4][6] = map[5][6] = map[6][6] = map[7][6] = 1;
    //map[start.x][start.y] = 2; map[goal.x][goal.y] = 3;
}
function init() {
    var canvas = document.createElement( "canvas" );
    canvas.width = canvas.height = 200;
    ctx = canvas.getContext( "2d" );
    ctx.scale( 20, 20 );
    document.body.appendChild( canvas );
    neighbours = [
        {x:1, y:0, c:1}, {x:-1, y:0, c:1}, {x:0, y:1, c:1}, {x:0, y:-1, c:1}, 
        {x:1, y:1, c:1.4}, {x:1, y:-1, c:1.4}, {x:-1, y:1, c:1.4}, {x:-1, y:-1, c:1.4}
    ];
    path = []; createMap(); opn.push( start ); solveMap();
}

```

{{out}}
```txt

Path: 11
[(1, 1) (2, 2) (2, 3) (2, 4) (2, 5) (2, 6) (3, 7) (4, 8) (5, 8) (6, 8) (7, 8) (8, 8) ]

```




## Julia

The graphic in this solution is displayed in the more standard orientation of origin at bottom left and goal at top right.

```Julia
using LightGraphs, SimpleWeightedGraphs

const chessboardsize = 8
const givenobstacles = [(2,4), (2,5), (2,6), (3,6), (4,6), (5,6), (5,5), (5,4), (5,3), (5,2), (4,2), (3,2)]
vfromcart(p, n) = (p[1] - 1) * n + p[2]
const obstacles = [vfromcart(o .+ 1, chessboardsize) for o in givenobstacles]
zbasedpath(path, n) = [(div(v - 1, n), (v - 1) % n) for v in path]
pathcost(path) = sum(map(x -> x in obstacles ? 100 : 1, path[2:end]))

function surround(x, y, n)
    bottomx = x > 1 ? x -1 : x
    topx = x < n ? x + 1 : x
    bottomy = y > 1 ? y - 1 : y
    topy = y < n ? y + 1 : y
    [CartesianIndex(x,y) for x in bottomx:topx for y in bottomy:topy]
end

function kinggraph(N)
    graph = SimpleWeightedGraph(N*N)
    for row in 1:N, col in 1:N, p in surround(row, col, N)
        origin = vfromcart(CartesianIndex(row, col), N)
        targ = vfromcart(p, N)
        hcost = (targ in obstacles || origin in obstacles) ? 100 : 1
        add_edge!(graph, origin, targ, hcost)
    end
    graph
end

kgraph = kinggraph(chessboardsize)
path = enumerate_paths(dijkstra_shortest_paths(kgraph, 1), 64)
println("Solution has cost $(pathcost(path)):\n", zbasedpath(path, chessboardsize))

path2graphic(x, path) = (x in obstacles ? '█' : x in path ? 'x' : '.')
for row in 8:-1:1, col in 7:-1:0
    print(path2graphic(row*8 - col, path))
    if col == 0
        println()
    end
end
```
 {{output}} 
```txt

Solution has cost 11:
Tuple{Int64,Int64}[(0, 0), (1, 1), (2, 2), (3, 1), (4, 1), (5, 1), (6, 2), (7, 3), (7, 4), (6, 5), (6, 6), (7, 7)]
...xx..x
..x..xx.
.x█████.
.x█...█.
.x█...█.
..x.███.
.x......
x.......

```



## Kotlin


```kotlin

import java.lang.Math.abs

typealias GridPosition = Pair<Int, Int>
typealias Barrier = Set<GridPosition>

const val MAX_SCORE = 99999999

abstract class Grid(private val barriers: List<Barrier>) {

    open fun heuristicDistance(start: GridPosition, finish: GridPosition): Int {
        val dx = abs(start.first - finish.first)
        val dy = abs(start.second - finish.second)
        return (dx + dy) + (-2) * minOf(dx, dy)
    }

    fun inBarrier(position: GridPosition) = barriers.any { it.contains(position) }

    abstract fun getNeighbours(position: GridPosition): List<GridPosition>

    open fun moveCost(from: GridPosition, to: GridPosition) = if (inBarrier(to)) MAX_SCORE else 1
}

class SquareGrid(width: Int, height: Int, barriers: List<Barrier>) : Grid(barriers) {

    private val heightRange: IntRange = (0 until height)
    private val widthRange: IntRange = (0 until width)

    private val validMoves = listOf(Pair(1, 0), Pair(-1, 0), Pair(0, 1), Pair(0, -1), Pair(1, 1), Pair(-1, 1), Pair(1, -1), Pair(-1, -1))

    override fun getNeighbours(position: GridPosition): List<GridPosition> = validMoves
            .map { GridPosition(position.first + it.first, position.second + it.second) }
            .filter { inGrid(it) }

    private fun inGrid(it: GridPosition) = (it.first in widthRange) && (it.second in heightRange)
}


/**
 * Implementation of the A* Search Algorithm to find the optimum path between 2 points on a grid.
 *
 * The Grid contains the details of the barriers and methods which supply the neighboring vertices and the
 * cost of movement between 2 cells.  Examples use a standard Grid which allows movement in 8 directions
 * (i.e. includes diagonals) but alternative implementation of Grid can be supplied.
 *
 */
fun aStarSearch(start: GridPosition, finish: GridPosition, grid: Grid): Pair<List<GridPosition>, Int> {

    /**
     * Use the cameFrom values to Backtrack to the start position to generate the path
     */
    fun generatePath(currentPos: GridPosition, cameFrom: Map<GridPosition, GridPosition>): List<GridPosition> {
        val path = mutableListOf(currentPos)
        var current = currentPos
        while (cameFrom.containsKey(current)) {
            current = cameFrom.getValue(current)
            path.add(0, current)
        }
        return path.toList()
    }

    val openVertices = mutableSetOf(start)
    val closedVertices = mutableSetOf<GridPosition>()
    val costFromStart = mutableMapOf(start to 0)
    val estimatedTotalCost = mutableMapOf(start to grid.heuristicDistance(start, finish))

    val cameFrom = mutableMapOf<GridPosition, GridPosition>()  // Used to generate path by back tracking

    while (openVertices.size > 0) {

        val currentPos = openVertices.minBy { estimatedTotalCost.getValue(it) }!!

        // Check if we have reached the finish
        if (currentPos == finish) {
            // Backtrack to generate the most efficient path
            val path = generatePath(currentPos, cameFrom)
            return Pair(path, estimatedTotalCost.getValue(finish)) // First Route to finish will be optimum route
        }

        // Mark the current vertex as closed
        openVertices.remove(currentPos)
        closedVertices.add(currentPos)

        grid.getNeighbours(currentPos)
                .filterNot { closedVertices.contains(it) }  // Exclude previous visited vertices
                .forEach { neighbour ->
                    val score = costFromStart.getValue(currentPos) + grid.moveCost(currentPos, neighbour)
                    if (score < costFromStart.getOrDefault(neighbour, MAX_SCORE)) {
                        if (!openVertices.contains(neighbour)) {
                            openVertices.add(neighbour)
                        }
                        cameFrom.put(neighbour, currentPos)
                        costFromStart.put(neighbour, score)
                        estimatedTotalCost.put(neighbour, score + grid.heuristicDistance(neighbour, finish))
                    }
                }

    }

    throw IllegalArgumentException("No Path from Start $start to Finish $finish")
}

fun main(args: Array<String>) {

    val barriers = listOf(setOf( Pair(2,4), Pair(2,5), Pair(2,6), Pair(3,6), Pair(4,6), Pair(5,6), Pair(5,5),
                 Pair(5,4), Pair(5,3), Pair(5,2), Pair(4,2), Pair(3,2)))

    val (path, cost) = aStarSearch(GridPosition(0,0), GridPosition(7,7), SquareGrid(8,8, barriers))

    println("Cost: $cost  Path: $path")
}

```

{{out}}
```txt

Cost: 11
Path: [(0, 0), (1, 1), (2, 2), (3, 1), (4, 1), (5, 1), (6, 2), (6, 3), (6, 4), (6, 5), (6, 6), (7, 7)]

```



## Lua


```lua

-- QUEUE -----------------------------------------------------------------------
Queue = {}
function Queue:new()
    local q = {}
    self.__index = self
    return setmetatable( q, self )
end
function Queue:push( v )
    table.insert( self, v )
end
function Queue:pop()
    return table.remove( self, 1 )
end
function Queue:getSmallestF()
    local s, i = nil, 2
    while( self[i] ~= nil and self[1] ~= nil ) do
        if self[i]:F() < self[1]:F() then
            s = self[1]
            self[1] = self[i] 
            self[i] = s
        end
        i = i + 1
    end
    return self:pop()
end

-- LIST ------------------------------------------------------------------------
List = {}
function List:new()
    local l = {}
    self.__index = self
    return setmetatable( l, self )
end
function List:push( v )
  table.insert( self, v )
end
function List:pop()
    return table.remove( self )
end

-- POINT -----------------------------------------------------------------------
Point = {}
function Point:new()
    local p = { y = 0, x = 0 }
    self.__index = self
    return setmetatable( p, self )
end
function Point:set( x, y )
    self.x, self.y = x, y
end
function Point:equals( o )
    return (o.x == self.x and o.y == self.y)
end
function Point:print()
    print( self.x, self.y )
end

-- NODE ------------------------------------------------------------------------
Node = {}
function Node:new()
    local n = { pos = Point:new(), parent = Point:new(), dist = 0, cost = 0 }
    self.__index = self
    return setmetatable( n, self )
end
function Node:set( pt, parent, dist, cost )
    self.pos = pt
    self.parent = parent
    self.dist = dist
    self.cost = cost
end
function Node:F()
    return ( self.dist + self.cost )
end

-- A-STAR ----------------------------------------------------------------------
local nbours = {
    {  1,  0, 1 }, {  0,  1, 1 }, {  1,  1, 1.4 }, {  1, -1, 1.4 }, 
    { -1, -1, 1.4 }, { -1,  1, 1.4 }, {  0, -1, 1 }, { -1,  0, 1 }
}
local map = { 
        1,1,1,1,1,1,1,1,1,1, 
        1,0,0,0,0,0,0,0,0,1,
        1,0,0,0,0,0,0,0,0,1,
        1,0,0,0,0,1,1,1,0,1,
        1,0,0,1,0,0,0,1,0,1,
        1,0,0,1,0,0,0,1,0,1,
        1,0,0,1,1,1,1,1,0,1,
        1,0,0,0,0,0,0,0,0,1,
        1,0,0,0,0,0,0,0,0,1,
        1,1,1,1,1,1,1,1,1,1 
}
local open, closed, start, goal, 
      mapW, mapH = Queue:new(), List:new(), Point:new(), Point:new(), 10, 10
start:set( 2, 2 ); goal:set( 9, 9 )

function hasNode( arr, pos )
    for nx, val in ipairs( arr ) do
        if val.pos:equals( pos ) then
            return nx
        end
    end
    return -1
end
function isValid( pos )
    return pos.x > 0 and pos.x <= mapW 
           and pos.y > 0 and pos.y <= mapH 
           and map[pos.x + mapW * pos.y - mapW] == 0
end
function calcDist( p1 )
    local x, y = goal.x - p1.x, goal.y - p1.y
    return math.abs( x ) + math.abs( y )
end
function addToOpen( node )
    local nx
    for n = 1, 8 do
        nNode = Node:new()
        nNode.parent:set( node.pos.x, node.pos.y )
        nNode.pos:set( node.pos.x + nbours[n][1], node.pos.y + nbours[n][2] )
        nNode.cost = node.cost + nbours[n][3]
        nNode.dist = calcDist( nNode.pos )
        
        if isValid( nNode.pos ) then
            if nNode.pos:equals( goal ) then 
                closed:push( nNode )
                return true 
            end
            nx = hasNode( closed, nNode.pos )
            if nx < 0 then
                nx = hasNode( open, nNode.pos )
                if( nx < 0 ) or ( nx > 0 and nNode:F() < open[nx]:F() ) then
                    if( nx > 0 ) then 
                        table.remove( open, nx )
                    end
                    open:push( nNode )
                else
                    nNode = nil
                end
            end
        end
    end
    return false
end
function makePath()
    local i, l = #closed, List:new()
    local node, parent = closed[i], nil

    l:push( node.pos )
    parent = node.parent
    while( i > 0 ) do
        i = i - 1
        node = closed[i]
        if node ~= nil and node.pos:equals( parent ) then
            l:push( node.pos )
            parent = node.parent
        end
    end
    print( string.format( "Cost: %d", #l - 1 ) )
    io.write( "Path: " )
    for i = #l, 1, -1 do
        map[l[i].x + mapW * l[i].y - mapW] = 2
        io.write( string.format( "(%d, %d) ", l[i].x, l[i].y ) )
    end
    print( "" )
end
function aStar()
    local n = Node:new()
    n.dist = calcDist( start )
    n.pos:set( start.x, start.y )
    open:push( n )
    while( true ) do
        local node = open:getSmallestF()
        if node == nil then break end
        closed:push( node )
        if addToOpen( node ) == true then 
            makePath()
            return true 
        end
    end
    return false
end
-- ENTRY POINT -----------------------------------------------------------------
if true == aStar() then
    local m
    for j = 1, mapH do
        for i = 1, mapW do
            m = map[i + mapW * j - mapW]
            if m == 0 then
                io.write( "." )
            elseif m == 1 then
                io.write( string.char(0xdb) )
            else
                io.write( "x" )
            end
        end
        io.write( "\n" )
    end
else
    print( "can not find a path!" )
end

```

{{out}}

```txt

Cost: 11
Path: (2, 2) (3, 3) (3, 4) (3, 5) (3, 6) (3, 7) (4, 8) (5, 9) (6, 9) (7, 9) (8, 9) (9, 9)
██████████
█x.......█
█.x......█
█.x..███.█
█.x█...█.█
█.x█...█.█
█.x█████.█
█..x.....█
█...xxxxx█
██████████

```



## Ol


```scheme

; level: list of lists, any except 1 means the cell is empty
; from: start cell in (x . y) mean
; to: destination cell in (x . y) mean
(define (A* level from to)
   (define (hash xy) ; internal hash
      (+ (<< (car xy) 16) (cdr xy)))

   ; naive test for "is the cell is empty?"
   (define (floor? x y)
      (let ((line (lref level y)))
         (if (pair? line)
            (eq? (lref line x) 0))))

   (unless (equal? from to) ; search not finished yet
      (let step1 ((n 999) ; maximal count of search steps
                  (c-list-set #empty)
                  (o-list-set (put #empty (hash from)  [from #f  0 0 0])))
         (unless (empty? o-list-set) ; do we have a space to move?
            ; no. let's find cell with minimal const
            (let*((f (ff-fold (lambda (s key value)
                                 (if (< (ref value 5) (car s))
                                    (cons (ref value 5) value)
                                    s))
                        (cons 9999 #f) o-list-set))
                  (xy (ref (cdr f) 1))
                  ; move the cell from "open" to "closed" list
                  (o-list-set (del o-list-set (hash xy)))
                  (c-list-set (put c-list-set (hash xy) (cdr f))))

               ;
               (if (or (eq? n 0)
                       (equal? xy to))
                  (let rev ((xy xy))
                     ; let's unroll the math and return only first step
                     (let*((parent (ref (get c-list-set (hash xy) #f) 2))
                           (parent-of-parent (ref (get c-list-set (hash parent) #f) 2)))
                        (if parent-of-parent (rev parent)
                           (cons
                              (- (car xy) (car parent))
                              (- (cdr xy) (cdr parent))))))

                  (let*((x (car xy))
                        (y (cdr xy))
                        (o-list-set (fold (lambda (n v)
                                       (if (and
                                             (floor? (car v) (cdr v))
                                             (eq? #f (get c-list-set (hash v) #f)))
                                          (let ((G (+ (ref (get c-list-set (hash xy) #f) 3) 1)); G of parent + 1
                                                ; H calculated by "Manhattan method"
                                                (H (* (+ (abs (- (car v) (car to)))
                                                         (abs (- (cdr v) (cdr to))))
                                                      2))
                                                (got (get o-list-set (hash v) #f)))

                                             (if got
                                                (if (< G (ref got 3))
                                                   (put n (hash v)  [v xy  G H (+ G H)])
                                                   n)
                                                (put n (hash v)  [v xy  G H (+ G H)])))
                                          n))
                                       o-list-set (list
                                                      (cons x (- y 1))
                                                      (cons x (+ y 1))
                                                      (cons (- x 1) y)
                                                      (cons (+ x 1) y)))))
                     (step1 (- n 1) c-list-set o-list-set))))))))

```


{{out}}

```scheme

(define level '(
   (1 1 1 1 1 1 1 1 1 1)
   (1 A 0 0 0 0 0 0 0 1)
   (1 0 0 0 0 0 0 0 0 1)
   (1 0 0 0 0 1 1 1 0 1)
   (1 1 0 0 0 0 0 1 0 1)
   (1 0 0 1 0 0 0 1 0 1)
   (1 0 0 1 1 1 1 1 0 1)
   (1 0 0 0 0 0 0 0 0 1)
   (1 0 0 0 1 0 0 0 B 1)
   (1 1 1 1 1 1 1 1 1 1)
))
(for-each print level)

; let's check that we can't move to (into wall)
(print (A* level '(1 . 1) '(9 . 9)))

(define to '(8 . 8))
(define (plus a b) (cons (+ (car a) (car b)) (+ (cdr a) (cdr b)))) ; helper

(define path
(let loop ((me '(1 . 1)) (path '()))
   (if (equal? me to)
      (begin
         (print "here I am!")
         (cons to path))
   (let ((move (A* level me to)))
      (unless move
         (begin
            (print "no way, sorry :(")
            #false)
         (let ((step (plus me move)))
            (print me " + " move " -> " step)
            (loop step (cons me path))))))))

; let's draw the path?
(define (has? lst x) ; helper
   (cond
      ((null? lst) #false)
      ((equal? (car lst) x) lst)
      (else (has? (cdr lst) x))))

(define solved
   (map (lambda (row y)
         (map (lambda (cell x)
               (cond
                  ((equal? (cons x y) '(1 . 1)) "A")
                  ((equal? (cons x y) '(8 . 8)) "B")
                  ((has? path (cons x y)) "*")
                  (else cell)))
            row (iota 10)))
      level (iota 10)))

(for-each print solved)

```



```txt

the map:
(1 1 1 1 1 1 1 1 1 1)
(1 A 0 0 0 0 0 0 0 1)
(1 0 0 0 0 0 0 0 0 1)
(1 0 0 0 0 1 1 1 0 1)
(1 1 0 0 0 0 0 1 0 1)
(1 0 0 1 0 0 0 1 0 1)
(1 0 0 1 1 1 1 1 0 1)
(1 0 0 0 0 0 0 0 0 1)
(1 0 0 0 1 0 0 0 B 1)
(1 1 1 1 1 1 1 1 1 1)
we should not reach the '(9 . 9) cell:
#false
ok, we got #false, so really can't.
now try to reach cell '(8 . 8) - the 'B' point:
(1 . 1) + (0 . 1) -> (1 . 2)
(1 . 2) + (0 . 1) -> (1 . 3)
(1 . 3) + (1 . 0) -> (2 . 3)
(2 . 3) + (0 . 1) -> (2 . 4)
(2 . 4) + (0 . 1) -> (2 . 5)
(2 . 5) + (0 . 1) -> (2 . 6)
(2 . 6) + (0 . 1) -> (2 . 7)
(2 . 7) + (1 . 0) -> (3 . 7)
(3 . 7) + (1 . 0) -> (4 . 7)
(4 . 7) + (1 . 0) -> (5 . 7)
(5 . 7) + (0 . 1) -> (5 . 8)
(5 . 8) + (1 . 0) -> (6 . 8)
(6 . 8) + (1 . 0) -> (7 . 8)
(7 . 8) + (1 . 0) -> (8 . 8)
here I am!
(1 1 1 1 1 1 1 1 1 1)
(1 A 0 0 0 0 0 0 0 1)
(1 * 0 0 0 0 0 0 0 1)
(1 * * 0 0 1 1 1 0 1)
(1 1 * 0 0 0 0 1 0 1)
(1 0 * 1 0 0 0 1 0 1)
(1 0 * 1 1 1 1 1 0 1)
(1 0 * * * * 0 0 0 1)
(1 0 0 0 1 * * * B 1)
(1 1 1 1 1 1 1 1 1 1)

```



## Phix

rows and columns are numbered 1 to 8. start position is {1,1} and end position is {8,8}.
barriers are simply avoided, rather than costed at 100. 
Note that the 23 visited nodes does not count walls, but with them this algorithm exactly matches the 35 of Racket.

```Phix
sequence grid = split("""
x:::::::
::::::::
::::###:
::#:::#:
::#:::#:
::#####:
::::::::
::::::::
""",'\n')
 
constant permitted = {{-1,-1},{0,-1},{1,-1},
                      {-1, 0},       {1, 0},
                      {-1, 1},{0,+1},{1,+1}}
 
sequence key = {7,0},   -- chebyshev, cost
         moves = {{1,1}},
         data = {moves},
         acta = {}      -- actually analysed set
setd(key,data)
bool found = false
integer count = 0
while not found do
    if dict_size()=0 then ?"impossible" exit end if
    key = getd_partial_key(0)
    data = getd(key)
    moves = data[$]
    if length(data)=1 then
        deld(key)
    else
        data = data[1..$-1]
        putd(key,data)
    end if
    count += 1
    acta = append(acta,moves[$])
    for i=1 to length(permitted) do
        sequence newpos = sq_add(moves[$],permitted[i])
        integer {nx,ny} = newpos
        if nx>=1 and nx<=8
        and ny>=1 and ny<=8
        and grid[nx,ny] = ':' then -- (unvisited)
            grid[nx,ny] = '.'
            sequence newkey = {max(8-nx,8-ny),key[2]+1},
                     newmoves = append(moves,newpos)
            if newpos = {8,8} then
                moves = newmoves
                found = true
                exit
            end if
            integer k = getd_index(newkey)
            if k=0 then
                data = {newmoves}
            else
                data = append(getd_by_index(k),newmoves)
            end if
            putd(newkey,data)
        end if
    end for
end while
if found then
    printf(1,"visited %d nodes\ncost:%d\npath:%v\n",{count,length(moves)-1,moves})
    for i=1 to length(acta) do
        integer {x,y} = acta[i]
        grid[x,y] = '_'
    end for
    for i=1 to length(moves) do
        integer {x,y} = moves[i]
        grid[x,y] = 'x'
    end for
    puts(1,join(grid,'\n'))
end if
```

{{out}}

```txt

visited 23 nodes
cost:11
path:{{1,1},{2,2},{3,3},{4,2},{5,2},{6,2},{7,3},{8,4},{8,5},{8,6},{8,7},{8,8}}
x......:
.x____.:
._x_###:
.x#___#:
.x#___#:
.x#####:
..x.....
:..xxxxx

```

The : represent nodes it did not even look at, the . those added but never gone back to, obviously x represent the path, and together _ and x all nodes actually analysed.


### Extra credit

Well, why not. Note this does not reuse/share any code with the above, although I presume the
task author assumed it would, instead the main loop uses a priority queue to obtain the next
lowest cost and a simple dictionary to avoid re-examination/inifinte recursion.

```Phix
--set_rand(3)    -- (for consistent output)
constant optimal = false,
         mtm = true,                    -- mutli-tile metrics
         target = {1,2,3,4,5,6,7,8,0},
             --   <-tile found 0..8->
         mcost = {{0,0,1,2,1,2,3,2,3},  -- position 1
                  {0,1,0,1,2,1,2,3,2},
                  {0,2,1,0,3,2,1,4,3},
                  {0,1,2,3,0,1,2,1,2},
                  {0,2,1,2,1,0,1,2,1},  -- ...
                  {0,3,2,1,2,1,0,3,2},
                  {0,2,3,4,1,2,3,0,1},
                  {0,3,2,3,2,1,2,1,0},
                  {0,4,3,2,3,2,1,2,1}}, -- position 9
         udlr = "udlr",
         dirs = {+3,-3,+1,-1},          -- udlr
         lims = {{9,9,9,9,9,9,9,9,9},   -- up
                 {1,1,1,1,1,1,1,1,1},   -- down
                 {3,3,3,6,6,6,9,9,9},   -- left
                 {1,1,1,4,4,4,7,7,7}}   -- right
                
function get_moves(sequence grid, bool mtm)
    sequence valid = {}
    integer p0 = find(0,grid)
    for dx=1 to length(dirs) do
        integer step = dirs[dx],
                lim = lims[dx][p0],
                count = 1
        for i=p0+step to lim by step do
            valid = append(valid,{step,i,udlr[dx],count})
            if not mtm then exit end if
            count += 1
        end for
    end for
    return valid
end function

function make_move(sequence grid, move)
    integer p0 = find(0,grid),
            {step,lim} = move
    for i=p0+step to lim by step do
        grid[p0] = grid[i]
        grid[i] = 0
        p0 = i
    end for
    return grid
end function
            
function manhattan(sequence grid)
    integer res = 0
    for i=1 to 9 do
        res += mcost[i][grid[i]+1]
    end for
    return res
end function

sequence problem, grid, new_grid,
         moves, next_moves, move
         
procedure show_grid()
    printf(1,"%s\n",join_by(sq_add(grid,'0'),1,3,""))
end procedure

grid = target
for i=1 to 1000 do
    -- (initially shuffle as if mtm==true, otherwise
    --  output compares answers to different puzzles)
    moves = get_moves(grid,true)
    move = moves[rand(length(moves))]
    grid = make_move(grid,move)
end for
problem = grid
printf(1,"problem (manhattan cost is %d):\n",manhattan(grid))
show_grid()

integer todo = pq_new(),
        seen = new_dict()
pq_add({{grid,{}},iff(optimal?0:manhattan(grid))},todo)
setd(grid,true,seen)
atom t1 = time()+1
bool found = false
integer count = 0, mc
while not found do
    if pq_size(todo)=0 then ?"impossible" exit end if
    {{grid,moves},mc} = pq_pop(todo)
    if time()>t1 then
        string m = iff(optimal?"moves":"manhattan")
        printf(1,"searching (count=%d, %s=%d)\r",{count,m,mc})
        t1 = time()+1
    end if
    next_moves = get_moves(grid,mtm)
    count += length(next_moves)
    integer l = length(moves)
    for i=1 to length(next_moves) do
        move = next_moves[i]
        new_grid = make_move(grid,move)
        mc = manhattan(new_grid) 
        if mc=0 then
            if new_grid!=target then ?9/0 end if
            moves = append(moves,move)
            found = true
            exit
        end if
        if getd_index(new_grid,seen)=NULL then
            if optimal then mc = l+1 end if
            pq_add({{new_grid,append(moves,move)},mc},todo)
            setd(new_grid,true,seen)
        end if
    end for
end while
if found then
    string s = iff(length(moves)=1?"":"s")
    if optimal then
        s &= sprintf(" (max shd be %d)",iff(mtm?24:31))
    end if
    grid = problem
    string soln = ""
    for i=1 to length(moves) do
        move = moves[i]
        grid = make_move(grid,move)
        integer {{},{},ch,c} = move
        soln &= ch
        if c>1 then soln&='0'+c end if
--      show_grid() -- (set the initial shuffle to eg 5 first!)
    end for
--  show_grid() -- (not very educational!)
    if grid!=target then ?9/0 end if
    printf(1,"solved in %d move%s:%s\n",{length(moves),s,soln})
end if
printf(1,"count:%d, seen:%d, queue:%d\n",{count,dict_size(seen),pq_size(todo)})
```

{{out}}
Note: The solutions are non-optimal (far from it, in fact), since it searches lowest manhattan() first.

In fact that set_rand(3), used for all the results below, is somewhat worse than 0, 1, and 2, and the
first to breach optimal limits, ie 31/24, but obviously only when the optimal flag is set to false, as
well as being the first to hint at the potential thousand-fold-or-more performance gains on offer.

An optimal solution can instead be found by searching fewest moves first, albeit significantly slower!
Note this approach is not really suitable for solving 15-puzzles (or larger).

with optimal false and mtm false:

```txt

problem (manhattan cost is 20):
546
807
321

solved in 88 moves:ulddruurdluldrdluurrddlurulldrrdlulurrddlurulldrdlururdllurrdlulddrurdlurdlulurrddlurull
count:592, seen:371, queue:155

```

with optimal false and mtm true:

```txt

solved in 45 moves:uld2r2u2l2d2r2u2ld2rul2dru2rdl2urdrdlu2rd2luruld2ru2l2dr2uldlu
count:328, seen:164, queue:82

```

with optimal true and mtm false:

```txt

solved in 26 moves (max shd be 31):rulldrdruulddruullddrruull
count:399996, seen:163976, queue:13728

```

with optimal true and mtm true:

```txt

solved in 17 moves (max shd be 24):rul2drdru2ld2ru2l2d2r2u2l2
count:298400, seen:106034, queue:31434

```



## PowerShell



```powershell
function CreateGrid($h, $w, $fill) {
    $grid = 0..($h - 1) | ForEach-Object { , (, $fill * $w) }
    return $grid
}

function EstimateCost($a, $b) {
    $xd = [Math]::Abs($a.Item1 - $b.Item1)
    $yd = [Math]::Abs($a.Item2 - $b.Item2)
    return [Math]::Max($xd, $yd)
}

function AStar($costs, $start, $goal) {
    # ValueTuples can be used to index a Hashtable:
    $start = [ValueTuple]::Create($start[0], $start[1])
    $goal = [ValueTuple]::Create($goal[0], $goal[1])

    $rows = $costs.Length
    $cols = $costs[0].Length

    $cameFrom = CreateGrid $rows $cols $null
    $openSet = @{$start = (EstimateCost $start $goal), 0}
    $closedSet = @{}

    while ($openSet.Count -gt 0) {
        # find the value in openSet with the lowest fScore
        $curFScore = [int]::MaxValue

        foreach ($p in $openSet.Keys) {
            $fScore, $gScore = $openSet[$p]
            if ($fScore -lt $curFScore) {
                $curFScore = $fScore
                $curGScore = $gScore
                $cur = $p
            }
        }

        if ($cur -eq $goal) {
            $totalCost = $curGScore
            break
        }

        $openSet.Remove($cur)
        $closedSet.Add($cur, 0)
        $r, $c = $cur.Item1, $cur.Item2

        # iterate over each cell in the 3x3 neighborhood
        foreach ($i in [Math]::Max($r - 1, 0)..[Math]::Min($r + 1, $rows - 1)) {
            foreach ($j in [Math]::Max($c - 1, 0)..[Math]::Min($c + 1, $cols - 1)) {
                $neighbor = [ValueTuple]::Create($i, $j)
                if ($closedSet.ContainsKey($neighbor)) { continue }

                $newGScore = $curGScore + $costs[$i][$j]
                $newFScore = $newGScore + (EstimateCost $neighbor $goal)

                if (-not $openSet.ContainsKey($neighbor)) {
                    $openSet[$neighbor] = $newFScore, $newGScore
                }
                else {
                    $fs, $gs = $openSet[$neighbor]
                    if ($newGScore -ge $gs) { continue }
                }

                $cameFrom[$i][$j] = $cur
            }
        }
    }

    # Walk back from the goal
    $route = @(, ($goal.Item1, $goal.Item2))
    $cur = $goal

    while ($cur -ne $start) {
        $cur = $cameFrom[$cur.Item1][$cur.Item2]
        $route += , ($cur.Item1, $cur.Item2)
    }

    [array]::Reverse($route)
    return $route, $totalCost
}

$grid = CreateGrid 8 8 1
$grid[2][4] = 100
$grid[2][5] = 100
$grid[2][6] = 100
$grid[3][6] = 100
$grid[4][6] = 100
$grid[5][6] = 100
$grid[5][5] = 100
$grid[5][4] = 100
$grid[5][3] = 100
$grid[5][2] = 100
$grid[4][2] = 100
$grid[3][2] = 100

$route, $cost = AStar $grid (0, 0) (7, 7)
$displayGrid = CreateGrid 8 8 '.'

foreach ($i in 0..7) {
    foreach ($j in 0..7) {
        if ($grid[$i][$j] -gt 1) {
            $displayGrid[$i][$j] = '#'
        }
    }
}

foreach ($step in $route) {
    $displayGrid[$step[0]][$step[1]] = 'x'
}

Write-Output ($displayGrid | ForEach-Object { $_ -join '' })
Write-Output "Cost: $cost"
$routeString = ($route | ForEach-Object { "($($_[0]), $($_[1]))" }) -join ', '
Write-Output "Route: $routeString"
```

{{out}}

```txt

x.......
.x......
..x.###.
.x#...#.
.x#...#.
.x#####.
..x.x.x.
...x.x.x
Cost: 11
Route: (0, 0), (1, 1), (2, 2), (3, 1), (4, 1), (5, 1), (6, 2), (7, 3), (6, 4), (7, 5), (6, 6), (7, 7)

```



## Python



```python
from __future__ import print_function
import matplotlib.pyplot as plt
 
class AStarGraph(object):
	#Define a class board like grid with two barriers
 
	def __init__(self):
		self.barriers = []
		self.barriers.append([(2,4),(2,5),(2,6),(3,6),(4,6),(5,6),(5,5),(5,4),(5,3),(5,2),(4,2),(3,2)])

	def heuristic(self, start, goal):
		#Use Chebyshev distance heuristic if we can move one square either
		#adjacent or diagonal
		D = 1
		D2 = 1
		dx = abs(start[0] - goal[0])
		dy = abs(start[1] - goal[1])
		return D * (dx + dy) + (D2 - 2 * D) * min(dx, dy)
 
	def get_vertex_neighbours(self, pos):
		n = []
		#Moves allow link a chess king
		for dx, dy in [(1,0),(-1,0),(0,1),(0,-1),(1,1),(-1,1),(1,-1),(-1,-1)]:
			x2 = pos[0] + dx
			y2 = pos[1] + dy
			if x2 < 0 or x2 > 7 or y2 < 0 or y2 > 7:
				continue
			n.append((x2, y2))
		return n
 
	def move_cost(self, a, b):
		for barrier in self.barriers:
			if b in barrier:
				return 100 #Extremely high cost to enter barrier squares
		return 1 #Normal movement cost
 
def AStarSearch(start, end, graph):
 
	G = {} #Actual movement cost to each position from the start position
	F = {} #Estimated movement cost of start to end going via this position
 
	#Initialize starting values
	G[start] = 0 
	F[start] = graph.heuristic(start, end)
 
	closedVertices = set()
	openVertices = set([start])
	cameFrom = {}
 
	while len(openVertices) > 0:
		#Get the vertex in the open list with the lowest F score
		current = None
		currentFscore = None
		for pos in openVertices:
			if current is None or F[pos] < currentFscore:
				currentFscore = F[pos]
				current = pos

		#Check if we have reached the goal
		if current == end:
			#Retrace our route backward
			path = [current]
			while current in cameFrom:
				current = cameFrom[current]
				path.append(current)
			path.reverse()
			return path, F[end] #Done!
 
		#Mark the current vertex as closed
		openVertices.remove(current)
		closedVertices.add(current)
 
		#Update scores for vertices near the current position
		for neighbour in graph.get_vertex_neighbours(current):
			if neighbour in closedVertices: 
				continue #We have already processed this node exhaustively
			candidateG = G[current] + graph.move_cost(current, neighbour)
 
			if neighbour not in openVertices:
				openVertices.add(neighbour) #Discovered a new vertex
			elif candidateG >= G[neighbour]:
				continue #This G score is worse than previously found
 
			#Adopt this G score
			cameFrom[neighbour] = current
			G[neighbour] = candidateG
			H = graph.heuristic(neighbour, end)
			F[neighbour] = G[neighbour] + H
 
	raise RuntimeError("A* failed to find a solution")
 
if __name__=="__main__":
	graph = AStarGraph()
	result, cost = AStarSearch((0,0), (7,7), graph)
	print ("route", result)
	print ("cost", cost)
	plt.plot([v[0] for v in result], [v[1] for v in result])
	for barrier in graph.barriers:
		plt.plot([v[0] for v in barrier], [v[1] for v in barrier])
	plt.xlim(-1,8)
	plt.ylim(-1,8)
	plt.show()
```


{{out}}

```txt
route [(0, 0), (1, 1), (2, 2), (3, 1), (4, 1), (5, 1), (6, 2), (7, 3), (6, 4), (7, 5), (6, 6), (7, 7)]
cost 11
```



## Racket

This code is lifted from: [https://jeapostrophe.github.io/2013-04-15-astar-post.html this blog post]. Read it, it's very good.


```racket
#lang scribble/lp
@(chunk
  <graph-sig>
  (define-signature graph^
    (node? edge? node-edges edge-src edge-cost edge-dest)))

@(chunk
  <map-generation>
  (define (make-map N)
    ;; Jay's random algorithm
    ;; (build-matrix N N (λ (x y) (random 3)))
    ;; RC version
    (matrix [[0 0 0 0 0 0 0 0]
             [0 0 0 0 0 0 0 0]
             [0 0 0 0 1 1 1 0]
             [0 0 1 0 0 0 1 0]
             [0 0 1 0 0 0 1 0]
             [0 0 1 1 1 1 1 0]
             [0 0 0 0 0 0 0 0]
             [0 0 0 0 0 0 0 0]])))

@(chunk
  <map-graph-rep>
  (struct map-node (M x y) #:transparent)
  (struct map-edge (src dx dy dest)))

@(chunk
  <map-graph-cost>
  (define (edge-cost e)
    (match-define (map-edge _ _ _ (map-node M x y)) e)
    (match (matrix-ref M x y)
      [0  1]
      [1  100]
      [2 1000])))

@(chunk
  <map-graph-edges>
  (define (node-edges n)
    (match-define (map-node M x y) n)
    (append*
     (for*/list ([dx (in-list '(1 0 -1))]
                 [dy (in-list '(1 0 -1))]
                 #:when
                 (and (not (and (zero? dx) (zero? dy)))
                      ;; RC -- allowed to move diagonally, so not this clause
                      ;;(or (zero? dx) (zero? dy))
                      ))
       (cond
         [(and (<= 0 (+ dx x) (sub1 (matrix-num-cols M)))
               (<= 0 (+ dy y) (sub1 (matrix-num-rows M))))
          (define dest (map-node M (+ dx x) (+ dy y)))
          (list (map-edge n dx dy dest))]
         [else
          empty])))))

@(chunk
  <a-star>
  (define (A* graph@ initial node-cost)
    (define-values/invoke-unit graph@ (import) (export graph^))
    (define count 0)
    <a-star-setup>
     
    (begin0
      (let/ec esc
        <a-star-loop>
        #f)
     
      (printf "visited ~a nodes\n" count))))

@(chunk
  <a-star-setup>
  <a-star-setup-closed>
  <a-star-setup-open>)

@(chunk
  <a-star-setup-closed>
  (define node->best-path (make-hash))
  (define node->best-path-cost (make-hash))     
  (hash-set! node->best-path      initial empty)
  (hash-set! node->best-path-cost initial 0))

@(chunk
  <a-star-setup-open>
  (define (node-total-estimate-cost n)
    (+ (node-cost n) (hash-ref node->best-path-cost n)))
  (define (node-cmp x y)
    (<= (node-total-estimate-cost x)
        (node-total-estimate-cost y)))
  (define open-set (make-heap node-cmp))
  (heap-add! open-set initial))

@(chunk
  <a-star-loop>
  (for ([x (in-heap/consume! open-set)])
    (set! count (add1 count))
    <a-star-loop-body>))

@(chunk
  <a-star-loop-stop?>
  (define h-x (node-cost x))
  (define path-x (hash-ref node->best-path x))
     
  (when (zero? h-x)
    (esc (reverse path-x))))

@(chunk
  <a-star-loop-body>
  <a-star-loop-stop?>
     
  (define g-x (hash-ref node->best-path-cost x))
  (for ([x->y (in-list (node-edges x))])
    (define y (edge-dest x->y))
    <a-star-loop-per-neighbor>))

@(chunk
  <a-star-loop-per-neighbor>
  (define new-g-y (+ g-x (edge-cost x->y)))
  (define old-g-y
    (hash-ref node->best-path-cost y +inf.0))
  (when (< new-g-y old-g-y)
    (hash-set! node->best-path-cost y new-g-y)
    (hash-set! node->best-path y (cons x->y path-x))
    (heap-add! open-set y)))

@(chunk
  <map-display>
  (define map-scale 15)
  (define (type-color ty)
    (match ty
      [0 "yellow"]
      [1 "green"]
      [2 "red"]))
  (define (cell-square ty)
    (square map-scale "solid" (type-color ty)))
  (define (row-image M row)
    (apply beside
           (for/list ([col (in-range (matrix-num-cols M))])
             (cell-square (matrix-ref M row col)))))
  (define (map-image M)
    (apply above
           (for/list ([row (in-range (matrix-num-rows M))])
             (row-image M row)))))

@(chunk
  <path-display-line>
  (define (edge-image-on e i)
    (match-define (map-edge (map-node _ sx sy) _ _ (map-node _ dx dy)) e)
    (add-line i
              (* (+ sy 0.5) map-scale) (* (+ sx 0.5) map-scale)
              (* (+ dy 0.5) map-scale) (* (+ dx 0.5) map-scale)
              "black")))

@(chunk
  <path-display>
  (define (path-image M path)
    (foldr edge-image-on (map-image M) path)))

@(chunk
  <map-graph>
  (define-unit map@
    (import) (export graph^)
     
    (define node? map-node?)
    (define edge? map-edge?)
    (define edge-src map-edge-src)
    (define edge-dest map-edge-dest)
     
    <map-graph-cost>
    <map-graph-edges>))

@(chunk
  <map-node-cost>
  (define ((make-node-cost GX GY) n)
    (match-define (map-node M x y) n)
    ;; Jay's
    #;(+ (abs (- x GX))
         (abs (- y GY)))
    ;; RC -- diagonal movement
    (max (abs (- x GX))
         (abs (- y GY)))))

@(chunk
  <map-example>
  (define N 8)
  (define random-M
    (make-map N))
  (define random-path
    (time
     (A* map@
         (map-node random-M 0 0)
         (make-node-cost (sub1 N) (sub1 N))))))

@(chunk
  <*>
  (require rackunit
           math/matrix
           racket/unit
           racket/match
           racket/list
           data/heap
           2htdp/image
           racket/runtime-path)
     
  <graph-sig>
     
  <map-generation>
  <map-graph-rep>
  <map-graph>
     
  <a-star>
     
  <map-node-cost>
  <map-example>
  (printf "path is ~a long\n" (length random-path))
  (printf "path is: ~a\n" (map (match-lambda
                                 [(map-edge src dx dy dest)
                                  (cons dx dy)])
                               random-path))
     
  <map-display>
  <path-display-line>
  <path-display>

  (path-image random-M random-path))
```


{{out}}


```txt
visited 35 nodes
cpu time: 94 real time: 97 gc time: 15
path is 11 long
path is: ((1 . 1) (1 . 1) (1 . -1) (1 . 0) (1 . 0) (1 . 1) (1 . 1) (0 . 1) (-1 . 1) (1 . 1) (0 . 1))
.
```

A diagram is also output, but you'll need to run this in DrRacket to see it.


## REXX


```rexx
/*REXX program solves the    A*   search problem   for a  (general)   NxN   grid.       */
parse arg  N  sCol sRow .                        /*obtain optional arguments from the CL*/
if    N=='' |    N==","  then    N=8             /*No grid size specified?  Use default.*/
if sCol=='' | sCol==","  then sCol=1             /*No starting column given?  "    "    */
if sRow=='' | sRow==","  then sRow=1             /* "     "     row     "     "    "    */
beg= '─0─'                                       /*mark the start of the journey in grid*/
o.=.;         p.=0                               /*list of optimum start journey starts.*/
times=0                                          /*cntr/pos for number of optimizations.*/
              Pc = ' 1  1  0  0   1 -1 -1 -1 '   /*the possible column moves for a path.*/
              Pr = ' 1  0  1 -1  -1  0  1 -1 '   /* "      "     row     "    "  "   "  */
Pcm=words(Pc)                                    /* [↑]  optimized for moving right&down*/
$.=1e6;  OK=0;     min$=$.                       /*# possible directions; cost; solution*/
@Aa= " A*  search algorithm on"                  /*a handy─dandy literal for the  SAYs. */
flasher= '@. $. min$ N o. p. Pc. Pcm Pr. sCol sRow times'   /*a literal list for EXPOSE.*/
call path 0                                      /*find a possible solution for the grid*/
@NxN= 'a '      N"x"N      ' grid'               /*a literal used for a  SAY  statement.*/
if OK  then say 'A solution for the'    @Aa     @NxN       "with a score of "     @.N.N':'
       else say 'No'   @Aa   "solution for"     @NxN'.'
call show 1                                      /*invoke subroutine to display the grid*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
@:    parse arg x,y,aChar;   if arg()==3  then @.x.y=aChar;                   return @.x.y
@p:   parse arg x,y;         if datatype(@.x.y, 'W')  then return @.x.y<m-1;  return 0
/*──────────────────────────────────────────────────────────────────────────────────────*/
barr: $=2.4 2.5 2.6 3.6 4.6 5.6 5.5 5.4 5.3 5.2 4.2 3.2  /*locations of barriers on grid*/
         do b=1  for words($);    _=word($, b);   parse var _ c '.' r;  call @ c+1,r+1,"█"
         end   /*b*/;             return
/*──────────────────────────────────────────────────────────────────────────────────────*/
move: procedure expose (flasher);          parse arg m,col,row   /*obtain  move,col,row.*/
         do t=1  for Pcm;         nc=col + Pc.t;   nr=row + Pr.t /*a new path position. */
         if @.nc.nr==.  then do;  if opti()  then iterate        /*Costlier path?  Next.*/
                                  @.nc.nr=m;       p.1.m=nc nr   /*Empty?  A legal path.*/
                                  p.pcm.m=nr nc-1                /*used for a fast path.*/
                                  if nc==N  then if nr==N  then return 1   /*last move? */
                                  if move(m + 1,  nc, nr)  then return 1   /*  "    "   */
                                  @.nc.nr=.                      /*undo the above move. */
                             end                                 /*try a different move.*/
         end   /*t*/                                             /* [↑]  all moves tried*/
      return 0                                                   /*path isn't possible. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
opti: ncm=nc-1;   nrm=nr-1;       if @p(ncm, nrm)  then return 1
                                  if @p(ncm, nr )  then return 1
                                  if @p(nc,  nrm)  then return 1
      ncp=nc+1;   nrp=nr+1;       if @p(ncp, nr )  then return 1
                                  if @p(ncp, nrm)  then return 1
                                  if @p(nc,  nrp)  then return 1
                                  if @p(ncm, nrp)  then return 1
                                  if @p(ncp, nrp)  then return 1;         return 0
/*──────────────────────────────────────────────────────────────────────────────────────*/
path: parse arg z;                t=times        /*initial move can only be one of eight*/
        do #=1  for Pcm;          @.=            /*optimize for each degree of movement.*/
        if z\==0  then  if #\==z  then iterate   /*This a particular low─cost request ? */
             do c=1  for  N;    do r=1  for N;   @.c.r=.;   end  /*r*/
             end   /*c*/
        iCol=sCol;  iRow=sRow;  @.sCol.sRow= beg /*all path's initial starting  position*/
        call barr                                /*place the barriers on the grid.      */
        Pco=subword(Pc Pc, #, Pcm);  Pro=subword(Pr Pr, #, Pcm)
        parse var  Pco   Pc.1 Pc.2 Pc.3 Pc.4 Pc.5 Pc.6 Pc.7 Pc.8  /*possible directions.*/
        parse var  Pro   Pr.1 Pr.2 Pr.3 Pr.4 Pr.5 Pr.6 Pr.7 Pr.8  /*    "         "     */
             do o=1  for times;  parse var o.o c r;    @.c.r=o;     iRow=r;     iCol=c
             end   /*o*/
        fp=move(1+times, iCol, iRow);      sol=@N.N\==. & fp
        if sol  then do;    $.#=@.N.N            /*Found a solution?  Remember the cost.*/
                     OK=1;  min$=min(min$, $.#)
                     end
        end   /*#*/
      wp=1e7; wg=0;  do g=1  for Pcm; if $.g<wp & $.g>0 & t\=2  then do; wg=g; wp=$.g; end
                     end   /*g*/                 /* [↑]  find minimum non-zero path cost*/
      if wg==0  then wg=8                        /*Not found?  Then use last cost found.*/
      times=times + 1                            /*bump # times a marker has been placed*/
      o.times= p.wg.times                        /*remember this move location for PATH.*/
      if times<4  then call path 0               /*only do memoization for first 3 moves*/
      return
/*──────────────────────────────────────────────────────────────────────────────────────*/
show: ind=left('', 9 * (n<18) );      say        /*the indentation of the displayed grid*/
      _=substr(copies("┼───", N),2);  say ind translate('┌'_"┐", '┬', "┼")   /*grid top.*/
                                                 /* [↓]  build a display for the grid.  */
       do   c=1  for N;          if c\==1 & arg(1)  then say  ind  '├'_"┤";     L=@.
         do r=1  for N; ?=@.c.r; if c ==N & r==N & ?\==.  then ?='end'; L=L"│"center(?, 3)
         end   /*r*/                             /*done with   rank   of the grid.      */
       say ind translate(L'│', , .)              /*display a     "     "  "    "        */
       end     /*c*/                             /*a 19x19 grid can be shown 80 columns.*/
     say ind translate('└'_"┘",'┴',"┼");  return /*display the very bottom of the grid. */
```

{{out|output|text=  when using the default input:}}

```txt

A solution for the  A*  search algorithm on a  8x8  grid with a score of  11:

          ┌───┬───┬───┬───┬───┬───┬───┬───┐
          │─0─│   │   │   │   │   │   │   │
          ├───┼───┼───┼───┼───┼───┼───┼───┤
          │   │ 1 │   │   │   │   │   │   │
          ├───┼───┼───┼───┼───┼───┼───┼───┤
          │   │   │ 2 │   │ █ │ █ │ █ │   │
          ├───┼───┼───┼───┼───┼───┼───┼───┤
          │   │ 3 │ █ │   │   │   │ █ │   │
          ├───┼───┼───┼───┼───┼───┼───┼───┤
          │   │ 4 │ █ │   │   │   │ █ │   │
          ├───┼───┼───┼───┼───┼───┼───┼───┤
          │   │ 5 │ █ │ █ │ █ │ █ │ █ │   │
          ├───┼───┼───┼───┼───┼───┼───┼───┤
          │   │   │ 6 │   │   │   │   │   │
          ├───┼───┼───┼───┼───┼───┼───┼───┤
          │   │   │   │ 7 │ 8 │ 9 │10 │end│
          └───┴───┴───┴───┴───┴───┴───┴───┘

```



## SequenceL


```sequencel

import <Utilities/Set.sl>;
import <Utilities/Math.sl>;
import <Utilities/Sequence.sl>;

Point ::= (x : int, y : int);

State ::= (open : Point(1), closed : Point(1), cameFrom : Point(2), estimate : int(2), actual : int(2));

allNeighbors := [(x : -1, y : -1), (x : 1, y : -1), (x : -1, y : 1), (x : 1, y : 1),
				 (x : 0, y : -1), (x : -1, y : 0), (x : 0, y : 1), (x : 1, y : 0)];

defaultBarriers := [(x : 3, y : 5),(x : 3, y : 6),(x : 3, y : 7),(x : 4, y : 7),
	(x : 5, y : 7),(x : 6, y : 7),(x : 6, y : 6),(x : 6, y : 5),(x : 6, y : 4),
	(x : 6, y : 3),(x : 5, y : 3),(x : 4, y : 3)];

defaultWidth := 8;
defaultHeight := 8;

main(args(2)) := aStar(defaultWidth, defaultHeight, defaultBarriers, (x : 1, y : 1), (x : defaultWidth, y : defaultHeight));

aStar(width, height, barriers(1), start, end) :=
	let
		newEstimate[i,j] := heuristic(start, end) when i = start.x and j = start.y else 0
								foreach i within 1...width, j within 1 ... height;
		newActual[i,j] := 0 foreach i within 1...width, j within 1...height;
		newCameFrom[i,j] := (x : 0, y : 0) foreach i within 1...width, j within 1...height;
		
		searchResults := search((open : [start], closed : [], estimate : newEstimate, actual : newActual, cameFrom : newCameFrom), barriers, end);
		shortestPath := path(searchResults.cameFrom, start, end) ++ [end];
	in
		"No Path Found" when size(searchResults.open) = 0 else
		"Path: " ++ toString(shortestPath) ++ "\nCost:" ++
		toString(searchResults.actual[end.x, end.y]) ++ "\nMap:\n" ++ join(appendNT(drawMap(barriers,shortestPath,width, height),"\n"));

path(cameFrom(2), start, current) :=
	let
		next := cameFrom[current.x, current.y];
	in
	[] when current = start else
	path(cameFrom, start, next) ++ [next];

drawMap(barriers(1), path(1), width, height)[i,j] :=
	'#' when elementOf((x:i, y:j), barriers) else
	'X' when elementOf((x:i, y:j), path) else
	'.' foreach i within 1 ... width, j within 1 ... height;

search(state, barriers(1), end) :=
	let
		nLocation := smallestEstimate(state.open, state.estimate, 2, 1, state.estimate[state.open[1].x, state.open[1].y]);
		n := state.open[nLocation];
		neighbors := createNeighbors(n, allNeighbors, size(state.actual), size(state.actual[1]));
		startState := (open : state.open[1...nLocation-1] ++ state.open[nLocation+1 ... size(state.open)], closed : state.closed ++ [n], cameFrom : state.cameFrom,
					   estimate : state.estimate, actual : state.actual);
		newState := findOpenNeighbors(n, startState, barriers, end, neighbors);
	in
	state when size(state.open) = 0  else
	state when n = end else
	search(newState, barriers, end);

smallestEstimate(open(1), estimate(2), index, minIndex, minEstimate) :=
	let newEstimate := estimate[open[index].x, open[index].y]; in
	minIndex when index > size(open) else
	smallestEstimate(open, estimate, index + 1, minIndex, minEstimate) when newEstimate > minEstimate else
	smallestEstimate(open, estimate, index + 1, index, newEstimate);

findOpenNeighbors(n, state, barriers(1), end, neighbors(1)) :=
	let
		neighbor := head(neighbors);
		cost := 1 + n.cost;
		candidate := state.actual[n.x, n.y] + calculateCost(barriers, n, neighbor);
	in
		state when size(neighbors) = 0 else
		findOpenNeighbors(n, state, barriers, end, tail(neighbors)) when elementOf(neighbor, state.closed) else
		findOpenNeighbors(n, state, barriers, end, tail(neighbors)) when elementOf(neighbor, state.open) and candidate >= state.actual[neighbor.x, neighbor.y] else
		findOpenNeighbors(n, (open : state.open ++ [neighbor], closed : state.closed,
			cameFrom : setMap(state.cameFrom, neighbor, n),
			estimate : setMap(state.estimate, neighbor, candidate + heuristic(neighbor, end)),
			actual : setMap(state.actual, neighbor, candidate)),
			barriers, end, tail(neighbors));

createNeighbors(n, p, w, h) :=
	let
		x := n.x + p.x;
		y := n.y + p.y;
	in
		(x : x, y : y) when x >= 1 and x <= w and y >= 1 and y <= h; 

calculateCost(barriers(1), start, end) := 100 when elementOf(end, barriers) else 1;

heuristic(start, end) :=
	let
		dx := abs(start.x - end.x);
		dy := abs(start.y - end.y);
	in
		(dx + dy) - min(dx, dy);

setMap(map(2), point, value)[i,j] :=
	value when point.x = i and point.y = j else
	map[i,j] foreach i within 1 ... size(map), j within 1 ... size(map[1]);

```

{{out|Output|text= }}

```txt

Path: [(x:1,y:1),(x:2,y:2),(x:3,y:3),(x:4,y:2),(x:5,y:2),(x:6,y:2),(x:7,y:3),(x:7,y:4),(x:7,y:5),(x:7,y:6),(x:7,y:7),(x:8,y:8)]
Cost:11
Map:
X.......
.X......
..X.###.
.X#...#.
.X#...#.
.X#####.
..XXXXX.
.......X

```


## Sidef

{{trans|Python}}

```ruby
class AStarGraph {

    has barriers = [
        [2,4],[2,5],[2,6],[3,6],[4,6],[5,6],[5,5],[5,4],[5,3],[5,2],[4,2],[3,2]
    ]

    method heuristic(start, goal) {
        var (D1 = 1, D2 = 1)
        var dx = abs(start[0] - goal[0])
        var dy = abs(start[1] - goal[1])
        (D1 * (dx + dy)) + ((D2 - 2*D1) * Math.min(dx, dy))
    }

    method get_vertex_neighbours(pos) {
        gather {
            for dx, dy in [[1,0],[-1,0],[0,1],[0,-1],[1,1],[-1,1],[1,-1],[-1,-1]] {
                var x2 = (pos[0] + dx)
                var y2 = (pos[1] + dy)
                (x2<0 || x2>7 || y2<0 || y2>7) && next
                take([x2, y2])
            }
        }
    }

    method move_cost(_a, b) {
        barriers.contains(b) ? 100 : 1
    }
}

func AStarSearch(start, end, graph) {

    var G = Hash()
    var F = Hash()

    G{start} = 0
    F{start} = graph.heuristic(start, end)

    var closedVertices = []
    var openVertices = [start]
    var cameFrom = Hash()

    while (openVertices) {

        var current = nil
        var currentFscore = Inf

        for pos in openVertices {
            if (F{pos} < currentFscore) {
                currentFscore = F{pos}
                current = pos
            }
        }

        if (current == end) {
            var path = [current]
            while (cameFrom.contains(current)) {
                current = cameFrom{current}
                path << current
            }
            path.flip!
            return (path, F{end})
        }

        openVertices.remove(current)
        closedVertices.append(current)

        for neighbour in (graph.get_vertex_neighbours(current)) {
            if (closedVertices.contains(neighbour)) {
                next
            }
            var candidateG = (G{current} + graph.move_cost(current, neighbour))

            if (!openVertices.contains(neighbour)) {
                openVertices.append(neighbour)
            }
            elsif (candidateG >= G{neighbour}) {
                next
            }

            cameFrom{neighbour} = current
            G{neighbour} = candidateG
            var H = graph.heuristic(neighbour, end)
            F{neighbour} = (G{neighbour} + H)
        }
    }

    die "A* failed to find a solution"
}

var graph = AStarGraph()
var (route, cost) = AStarSearch([0,0], [7,7], graph)

var w = 10
var h = 10

var grid = h.of { w.of { "." } }
for y in (^h) { grid[y][0] = "█"; grid[y][-1] = "█" }
for x in (^w) { grid[0][x] = "█"; grid[-1][x] = "█" }

for x,y in (graph.barriers) { grid[x+1][y+1] = "█" }
for x,y in (route)          { grid[x+1][y+1] = "x" }

grid.each { .join.say }

say "Path cost #{cost}: #{route}"
```

{{out}}

```txt

██████████
█x.......█
█.x......█
█..x.███.█
█.x█...█.█
█.x█...█.█
█.x█████.█
█..xxxxx.█
█.......x█
██████████
Path cost 11: [[0, 0], [1, 1], [2, 2], [3, 1], [4, 1], [5, 1], [6, 2], [6, 3], [6, 4], [6, 5], [6, 6], [7, 7]]

```


## UNIX Shell

{{works with|Bourne Again SHell}}


```bash

#!/bin/bash

# This option will make the script exit when there is an error
set -o errexit
# This option will make the script exit when it tries to use an unset variable
set -o nounset

declare -A grid
declare -A cell_type=(
  ["empty"]=0 ["barrier"]=1
  ["start"]=2 ["end"]=3
  ["path"]=4
  ["right"]=5     ["left"]=6
  ["up"]=7        ["down"]=8
  ["left_up"]=9   ["left_down"]=10
  ["right_up"]=11 ["right_down"]=12
  )
grid_size=(10 10)

generate_rosetta_grid(){
  grid_size=(8 8)
  start=(0 0)
  end=(7 7)
  for (( i = 0; i < grid_size[0]; i++ )); do
    for (( j = 0; j < grid_size[1]; j++ )); do
      grid[$i,$j]=${cell_type[empty]}
    done
  done
  barriers=( "2,4" "2,5" "2,6" "3,6" "4,6" "5,6" "5,5" "5,4" "5,3" "5,2" "4,2" "3,2")
  for barrier in ${barriers[*]};do
    grid["$barrier"]=${cell_type[barrier]}
  done
  grid[${start[0]},${start[1]}]=${cell_type[start]}
  grid[${end[0]},${end[1]}]=${cell_type[end]}
}

abs(){
  # Number asbolute value.
  # Params:
  # ------
  # $1 -> number
  # Return:
  # number abs
  if [[ $1 -gt 0 ]];
  then
    echo "$1"
  else
    echo "$((-$1))"
  fi
}

print_table(){
  # Print table using unicode symbols.
  # Symbols:
  # " " -> empty cell
  # ◼ -> barrier
  # ◉ -> start position
  # ✪ -> goal
  # arrows -> path from start to goal
  printf ' '
  # Print letters at top.
  for ((i=0;i< grid_size[1];i++)) do
      printf "%s" $i
  done
  echo
  for ((i=0;i < grid_size[0];i++)) do
      # Print numbers.
      printf "%s" $i
      for ((j=0;j < grid_size[1];j++)) do
          cell=${grid[$i,$j]}
          if [[ $cell  -eq ${cell_type[empty]} ]];
          then
            # If cell is empty prints space
            printf " "
          elif [[ $cell -eq ${cell_type[barrier]} ]]; then
            # If cell is a barrier
            printf "■"
          elif [[ $cell -eq ${cell_type[start]} ]]; then
            # Print start and end position
            printf "◉"
          elif [[ $cell -eq ${cell_type[end]} ]]; then
            # Print end position
            printf "✪"
          elif [[ $cell -eq ${cell_type[path]} ]]; then
            # Print path
            printf "*"
          elif [[ $cell -eq ${cell_type[up]} ]]; then
            # Print path
            printf "↑"
          elif [[ $cell -eq ${cell_type[down]} ]]; then
            # Print path
            printf "↓"
          elif [[ $cell -eq ${cell_type[right]} ]]; then
            # Print path
            printf "→"
          elif [[ $cell -eq ${cell_type[left]} ]]; then
            # Print path
            printf "←"
          elif [[ $cell -eq ${cell_type[right_up]} ]]; then
            # Print path
            printf "↗"
          elif [[ $cell -eq ${cell_type[right_down]} ]]; then
            # Print path
            printf "↙"
          elif [[ $cell -eq ${cell_type[left_up]} ]]; then
            # Print path
            printf "↖"
          elif [[ $cell -eq ${cell_type[left_down]} ]]; then
            # Print path
            printf "↘"
          fi
      done
      echo
  done
}

get_neighbours(){
  # Calculates all point's neighbours
  # Params:
  # ------
  # $1 -> "x,y" formatted point position
  # Return:
  # ------
  # array of available positions
  # Skips nonexistent indices.

  neighbours=()
  for i in {-1..1},{-1..1}; do
    if [[ ( ${i%,*} -eq 0 ) && ( ${i#*,} -eq 0 ) ]]; then
      continue
    fi
    dx=${i%,*}
    dy=${i#*,}
    x=$((${1%,*}+dx))
    y=$((${1#*,}+dy))
    if [[ $x -lt 0 ]] || [[ $x -ge ${grid_size[0]} ]];
    then
      continue
    fi
    if [[ $y -lt 0 || $y -ge ${grid_size[1]} ]];
    then
      continue
    fi
    neighbours+=("$x,$y")
  done
  echo "${neighbours[*]}"
}


move_cost(){
  # Calculates how much will it cost
  # to travel to point b.
  # return 100 if b is barrier
  #
  # Params:
  # ------
  # $1 -> a
  # $2 -> b
  # Return:
  # ------
  # movement cost.

  barrier=${cell_type[barrier]}
  if [[ ${grid[${2%,*},${2#*,}]} -eq barrier ]];
  then
    echo 100
  else
    echo 1
  fi
}

print_raw(){
  # Print raw grid values.

  for ((i=0;i < grid_size[0];i++)) do
      for ((j=0;j < grid_size[1];j++)) do
        printf "%s" "${grid[$i,$j]}"
      done
      echo
  done
}

minimum(){
  # Minimum between two numbers
  # Params:
  # ------
  # $1 -> a
  # $2 -> b
  # Return:
  # ------
  # less value

  if [[ $1 -lt $2 ]];
  then
    echo "$1"
  else
    echo "$2"
  fi
}

heuristic_cost(){
  # Chebyshev distance heuristic score
  # if we can move one square either
  # adjacent or diagonal

  d=1
  d2=1
  dx=$(abs $((${1#*,} - ${2#*,})))
  dy=$(abs $((${1%,*} - ${2%,*})))
  echo "$(((d*(dx + dy))+(d2 - 2 * d)*$(minimum dx dy)))"
}

contains(){

  for el in "${2[@]}"; do
    echo "$el"
  done
}

contains_value() {
    # Check if element exists in array
    # Params:
    # ------
    # $1 -> array
    # $2 -> element to find.
    # Returns:
    # 1 if element exists in array
    # 0 otherwise.

    local array="$1[@]"
    arr=("${!array}")
    local seeking=$2
    local in=0
    for element in ${arr[*]}; do
        if [ "$element" = "$seeking" ]; then
            in=1
            break
        fi
    done
    echo "$in"
}

reverse_array(){
  # Reverse given array.
  # Params:
  # ------
  # $1 -> array
  # Return:
  # ------
  # reversed array.
  local array="$1[@]"
  arr=("${!array}")
  result=()
  for (( idx=${#arr[@]}-1 ; idx>=0 ; idx-- )) ; do
    result+=("${arr[$idx]}")
  done
  echo "${result[@]}"
}


find_path(){
  declare -A fScore
  declare -A gScore
  declare -A cameFrom
  declare -a openVertices
  declare -a closedVertices
  for (( i = 0; i < grid_size[0]; i++ )); do
    for (( j = 0; j < grid_size[1]; j++ )); do
      gScore[$i,$j]=$((1<<62))
      fScore[$i,$j]=$((1<<62))
    done
  done
  gScore["${start[0]},${start[1]}"]=0
  fScore["${start[0]},${start[1]}"]=$(heuristic_cost "${start[0]},${start[1]}" "${end[0]},${end[1]}")
  openVertices+=("${start[0]},${start[1]}")

  while [[ -n "${openVertices[*]}" ]]; do

    current=-1
    currentFscore=0
    for pos in ${openVertices[*]}; do
      if [[  $current -eq -1 ]] ||
         [[ ${fScore["$pos"]} -lt $currentFscore ]]; then
        currentFscore=${fScore["$pos"]}
        current=$pos
      fi
    done
    if [[ "$current" = "${end[0]},${end[1]}" ]]; then
      path=( "$current" )
      while [ ${cameFrom["$current"]+_} ]; do
        current=${cameFrom["$current"]}
        path+=("$current")
      done
      reverse_array path
      return 0
    fi
    openVertices=( "$( echo "${openVertices[@]/$current}" | xargs )" )
    closedVertices+=( "$current" )
    neighbours=( "$(get_neighbours "$current")" )

    for neighbour in ${neighbours[*]}; do
      if [[ $(contains_value closedVertices "$neighbour") -eq 1 ]]; then
        continue
      fi
      mCost="$(move_cost "$current" "$neighbour")"
      candidateG=$(( ${gScore["$current"]}+mCost ))
      if [[ $candidateG -gt 100 ]]; then
        continue
      fi
      if [[  $(contains_value openVertices "$neighbour") -eq 0 ]]; then
        openVertices+=("$neighbour")
      elif [[ $candidateG -gt ${gScore[$neighbour]} ]]; then
        continue
      fi
      cameFrom["$neighbour"]="$current"
      gScore["$neighbour"]=$candidateG
      heuristic_score=$(heuristic_cost "$neighbour" "${end[0]},${end[1]}")
      fScore["$neighbour"]=$(( candidateG+heuristic_score ))
    done
  done
}

map_to_arrows(){
  local array="$1[@]"
  arr=("${!array}")
  last="${start[0]},${start[1]}"
  for el in ${arr[*]}; do
    if   [[ $((${el#*,}-${last#*,})) -eq -1 ]] &&
         [[ $((${el%,*}-${last%,*})) -eq -1 ]]; then
      grid["$last"]=${cell_type[left_up]}
    elif [[ $((${el#*,}-${last#*,})) -eq -1 ]] &&
         [[ $((${el%,*}-${last%,*})) -eq 1 ]]; then
           grid["$last"]=${cell_type[right_down]}
    elif [[ $((${el#*,}-${last#*,})) -eq 1 ]] &&
         [[ $((${el%,*}-${last%,*})) -eq -1 ]]; then
      grid["$last"]=${cell_type[right_up]}
    elif [[ $((${el#*,}-${last#*,})) -eq 1 ]] &&
         [[ $((${el%,*}-${last%,*})) -eq 1 ]]; then
      grid["$last"]=${cell_type[left_down]}
    elif [[ $((${el#*,}-${last#*,})) -eq -1 ]];then
      grid["$last"]=${cell_type[left]}
    elif [[ $((${el%,*}-${last%,*})) -eq -1 ]];then
      grid["$last"]=${cell_type[up]}
    elif [[ $((${el#*,}-${last#*,})) -eq 1 ]];then
      grid["$last"]=${cell_type[right]}
    elif [[ $((${el%,*}-${last%,*})) -eq 1 ]];then
      grid["$last"]=${cell_type[down]}
    else
      grid["$last"]=${cell_type[path]}
    fi
    last=$el
  done
  grid[${start[0]},${start[1]}]=${cell_type[start]}
  grid[${end[0]},${end[1]}]=${cell_type[end]}
}


main(){
  generate_rosetta_grid
  path=( "$(find_path)" )
  pstr="$(echo "${path[*]}" | xargs | sed "s/[[:space:]]/ → /g")"
  echo path: "$pstr"
  if [[ -z $pstr ]]; then
    echo "No path found."
  else
    map_to_arrows path
    print_table
  fi
}

main "$@"


```

{{out}}

```txt

path: 0,0 → 1,0 → 2,0 → 3,0 → 4,0 → 5,1 → 6,2 → 7,3 → 7,4 → 7,5 → 7,6 → 7,7
 01234567
0◉       
1↓       
2↓   ■■■ 
3↓ ■   ■ 
4↘ ■   ■ 
5 ↘■■■■■ 
6  ↘     
7   →→→→✪

```



## zkl

{{trans|Python}}

```zkl
   // we use strings as hash keys: (x,y)-->"x,y", keys are a single pair
fcn toKey(xy){ xy.concat(",") }

fcn AStarSearch(start,end,graph){
   G:=Dictionary(); # Actual movement cost to each position from the start position
   F:=Dictionary(); # Estimated movement cost of start to end going via this position
      #Initialize starting values
   kstart:=toKey(start);
   G[kstart]=0;
   F[kstart]=graph.heuristic(start,end);
   closedVertices,openVertices,cameFrom := List(),List(start),Dictionary();
 
   while(openVertices){
      # Get the vertex in the open list with the lowest F score
      current,currentFscore := Void, Void;
      foreach pos in (openVertices){
         kpos:=toKey(pos);
         if(current==Void or F[kpos]<currentFscore)
	    currentFscore,current = F[kpos],pos;
 
	 # Check if we have reached the goal
	 if(current==end){   # Yes! Retrace our route backward
	    path,kcurrent := List(current),toKey(current);
	    while(current = cameFrom.find(kcurrent)){
	       path.append(current);
	       kcurrent=toKey(current);
	    }
	    return(path.reverse(),F[toKey(end)])   # Done!
	 }

	 # Mark the current vertex as closed
	 openVertices.remove(current);
	 if(not closedVertices.holds(current)) closedVertices.append(current);
 
	 # Update scores for vertices near the current position
	 foreach neighbor in (graph.get_vertex_neighbors(current)){
	    if(closedVertices.holds(neighbor))
	       continue; # We have already processed this node exhaustively
	    kneighbor:=toKey(neighbor);
	    candidateG:=G[toKey(current)] + graph.move_cost(current, neighbor);
 
	    if(not openVertices.holds(neighbor))
	       openVertices.append(neighbor); # Discovered a new vertex
	    else if(candidateG>=G[kneighbor])
	       continue; # This G score is worse than previously found
 
	    # Adopt this G score
	    cameFrom[kneighbor]=current;
	    G[kneighbor]=candidateG;
	    F[kneighbor]=G[kneighbor] + graph.heuristic(neighbor,end);
	 }
      }
   } // while
   throw(Exception.AssertionError("A* failed to find a solution"));
}
```


```zkl
class [static] AStarGraph{   # Define a class board like grid with barriers
   var [const] barriers =
      T(        T(3,2),T(4,2),T(5,2),   // T is RO List
			      T(5,3),
	 T(2,4),	      T(5,4),
	 T(2,5),	      T(5,5),
	 T(2,6),T(3,6),T(4,6),T(5,6) );
   fcn heuristic(start,goal){  // (x,y),(x,y)
   # Use Chebyshev distance heuristic if we can move one square either
   # adjacent or diagonal
      D,D2,dx,dy := 1,1, (start[0] - goal[0]).abs(), (start[1] - goal[1]).abs();
      D*(dx + dy) + (D2 - 2*D)*dx.min(dy);
   }
   fcn get_vertex_neighbors([(x,y)]){      # Move like a chess king
      var moves=Walker.cproduct([-1..1],[-1..1]).walk();  // 8 moves + (0,0)
      moves.pump(List,'wrap([(dx,dy)]){
	 x2,y2 := x + dx, y + dy;
	 if((dx==dy==0) or x2 < 0 or x2 > 7 or y2 < 0 or y2 > 7) Void.Skip;
	 else T(x2,y2);
      })
   }
   fcn move_cost(a,b){  // ( (x,y),(x,y) )
      if(barriers.holds(b))
	 return(100); # Extremely high cost to enter barrier squares
      1 # Normal movement cost
   }
}
```


```zkl
graph:=AStarGraph;
route,cost := AStarSearch(T(0,0), T(7,7), graph);
println("Route: ", route.apply(fcn(xy){ String("(",toKey(xy),")") }).concat(","));
println("Cost: ", cost);

   // graph the solution:
grid:=(10).pump(List,List.createLong(10," ").copy);
foreach x,y in (graph.barriers){ grid[x][y]="#" }
foreach x,y in (route){ grid[x][y]="+" }
grid[0][0] = "S"; grid[7][7] = "E"; 
foreach line in (grid){ println(line.concat()) }
```

{{out}}

```txt

Route: (0,0),(1,1),(2,2),(3,1),(4,0),(5,1),(6,2),(7,3),(7,4),(7,5),(7,6),(7,7)
Cost: 11
S         
 +        
  + ###   
 +#   #   
+ #   #   
 +#####   
  +       
   ++++E  

```

