+++
title = "K-d tree"
description = ""
date = 2019-02-08T05:51:34Z
aliases = []
[extra]
id = 11474
[taxonomies]
categories = []
tags = []
+++

{{task}}
{{wikipedia|K-d tree}}
[[Category:Data Structures]]
A k-d tree (short for ''k''-dimensional tree) is a space-partitioning data structure for organizing points in a k-dimensional space. k-d trees are a useful data structure for several applications, such as searches involving a multidimensional search key (e.g. range searches and nearest neighbor searches). 
k-d trees are a special case of binary space partitioning trees.

k-d trees are not suitable, however, for efficiently finding the nearest neighbor in high dimensional spaces. As a general rule, if the dimensionality is ''k'', the number of points in the data, ''N'', should be ''N'' ≫ 2<sup>''k''</sup>. 
Otherwise, when k-d trees are used with high-dimensional data, most of the points in the tree will be evaluated and the efficiency is no better than exhaustive search, and other methods such as approximate nearest-neighbor are used instead.

'''Task:''' Construct a k-d tree and perform a nearest neighbor search for two example data sets:

# The Wikipedia example data of [(2,3), (5,4), (9,6), (4,7), (8,1), (7,2)].
# 1000 3-d points uniformly distributed in a 3-d cube.

For the Wikipedia example, find the nearest neighbor to point (9, 2)
For the random data, pick a random location and find the nearest neighbor.

In addition, instrument your code to count the number of nodes visited in the nearest neighbor search.  Count a node as visited if any field of it is accessed.

Output should show the point searched for, the point found, 
the distance to the point, and the number of nodes visited.

There are variant algorithms for constructing the tree.  
You can use a simple median strategy or implement something more efficient.  
Variants of the nearest neighbor search include nearest N neighbors, approximate nearest neighbor, and range searches.  
You do not have to implement these.  
The requirement for this task is specifically the nearest single neighbor.  
Also there are algorithms for inserting, deleting, and balancing k-d trees.  
These are also not required for the task.


## C

Using a Quickselectesque median algorithm.  Compared to unbalanced trees (random insertion), it takes slightly longer (maybe half a second or so) to construct a million-node tree, though average look up visits about 1/3 fewer nodes.

```c


#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <time.h>

#define MAX_DIM 3
struct kd_node_t{
    double x[MAX_DIM];
    struct kd_node_t *left, *right;
};

    inline double
dist(struct kd_node_t *a, struct kd_node_t *b, int dim)
{
    double t, d = 0;
    while (dim--) {
        t = a->x[dim] - b->x[dim];
        d += t * t;
    }
    return d;
}
inline void swap(struct kd_node_t *x, struct kd_node_t *y) {
    double tmp[MAX_DIM];
    memcpy(tmp,  x->x, sizeof(tmp));
    memcpy(x->x, y->x, sizeof(tmp));
    memcpy(y->x, tmp,  sizeof(tmp));
}


/* see quickselect method */
    struct kd_node_t*
find_median(struct kd_node_t *start, struct kd_node_t *end, int idx)
{
    if (end <= start) return NULL;
    if (end == start + 1)
        return start;

    struct kd_node_t *p, *store, *md = start + (end - start) / 2;
    double pivot;
    while (1) {
        pivot = md->x[idx];

        swap(md, end - 1);
        for (store = p = start; p < end; p++) {
            if (p->x[idx] < pivot) {
                if (p != store)
                    swap(p, store);
                store++;
            }
        }
        swap(store, end - 1);

        /* median has duplicate values */
        if (store->x[idx] == md->x[idx])
            return md;

        if (store > md) end = store;
        else        start = store;
    }
}

    struct kd_node_t*
make_tree(struct kd_node_t *t, int len, int i, int dim)
{
    struct kd_node_t *n;

    if (!len) return 0;

    if ((n = find_median(t, t + len, i))) {
        i = (i + 1) % dim;
        n->left  = make_tree(t, n - t, i, dim);
        n->right = make_tree(n + 1, t + len - (n + 1), i, dim);
    }
    return n;
}

/* global variable, so sue me */
int visited;

void nearest(struct kd_node_t *root, struct kd_node_t *nd, int i, int dim,
        struct kd_node_t **best, double *best_dist)
{
    double d, dx, dx2;

    if (!root) return;
    d = dist(root, nd, dim);
    dx = root->x[i] - nd->x[i];
    dx2 = dx * dx;

    visited ++;

    if (!*best || d < *best_dist) {
        *best_dist = d;
        *best = root;
    }

    /* if chance of exact match is high */
    if (!*best_dist) return;

    if (++i >= dim) i = 0;

    nearest(dx > 0 ? root->left : root->right, nd, i, dim, best, best_dist);
    if (dx2 >= *best_dist) return;
    nearest(dx > 0 ? root->right : root->left, nd, i, dim, best, best_dist);
}

#define N 1000000
#define rand1() (rand() / (double)RAND_MAX)
#define rand_pt(v) { v.x[0] = rand1(); v.x[1] = rand1(); v.x[2] = rand1(); }
int main(void)
{
    int i;
    struct kd_node_t wp[] = {
        {{2, 3}}, {{5, 4}}, {{9, 6}}, {{4, 7}}, {{8, 1}}, {{7, 2}}
    };
    struct kd_node_t testNode = {{9, 2}};
    struct kd_node_t *root, *found, *million;
    double best_dist;

    root = make_tree(wp, sizeof(wp) / sizeof(wp[1]), 0, 2);

    visited = 0;
    found = 0;
    nearest(root, &testNode, 0, 2, &found, &best_dist);

    printf(">> WP tree\nsearching for (%g, %g)\n"
            "found (%g, %g) dist %g\nseen %d nodes\n\n",
            testNode.x[0], testNode.x[1],
            found->x[0], found->x[1], sqrt(best_dist), visited);

    million =(struct kd_node_t*) calloc(N, sizeof(struct kd_node_t));
    srand(time(0));
    for (i = 0; i < N; i++) rand_pt(million[i]);

    root = make_tree(million, N, 0, 3);
    rand_pt(testNode);

    visited = 0;
    found = 0;
    nearest(root, &testNode, 0, 3, &found, &best_dist);

    printf(">> Million tree\nsearching for (%g, %g, %g)\n"
            "found (%g, %g, %g) dist %g\nseen %d nodes\n",
            testNode.x[0], testNode.x[1], testNode.x[2],
            found->x[0], found->x[1], found->x[2],
            sqrt(best_dist), visited);

    /* search many random points in million tree to see average behavior.
       tree size vs avg nodes visited:
       10      ~  7
       100     ~ 16.5
       1000        ~ 25.5
       10000       ~ 32.8
       100000      ~ 38.3
       1000000     ~ 42.6
       10000000    ~ 46.7              */
    int sum = 0, test_runs = 100000;
    for (i = 0; i < test_runs; i++) {
        found = 0;
        visited = 0;
        rand_pt(testNode);
        nearest(root, &testNode, 0, 3, &found, &best_dist);
        sum += visited;
    }
    printf("\n>> Million tree\n"
            "visited %d nodes for %d random findings (%f per lookup)\n",
            sum, test_runs, sum/(double)test_runs);

    // free(million);

    return 0;
}


```

{{out}}

```txt
>> WP tree
searching for (9, 2)
found (8, 1) dist 1.41421
seen 3 nodes

>> Million tree
searching for (0.29514, 0.897237, 0.941998)
found (0.296093, 0.896173, 0.948082) dist 0.00624896
seen 44 nodes

>> Million tree
visited 4271442 nodes for 100000 random findings (42.714420 per lookup)

```



## Common Lisp

The 3D data set is the set of coordinates from (0,0,0) to (9,9,9) (ie. uniformly distributed), while the ordinates of the random target are positive real < 10.

```lisp

(defun main ()
  (let ((dims 0) (target nil) (hits 0))
 
    ;;; distance node to target:
    ;;; returns squared euclidean distance, or squared semi distance if option set
    (defun distance (n &optional (semi nil))
      (if semi (expt (- (nth (first n) (second n)) (nth (first n) target)) 2)
          (reduce #'+ (mapcar (lambda (x y) (* (- x y) (- x y))) (second n) target))))

    ;;; returns true if target is to its left in axis dim
    (defun target< (n)
      (< (nth (first n) target) (nth (first n) (second n)))) 

    ;;; return the next child when nn searching, return opposing child if option oppose set
    (defun next-node (n &optional (oppose nil))
      (if (or (and (target< n) (not oppose)) (and (not (target< n)) oppose)) (third n) (fourth n)))

    ;;; a kdtree is a binary tree where nodes are:
    ;;; terminal: (axis data-point),  or
    ;;; branch:   (axis split-point (left-kdtree) (right-kdtree))
    (defun make-kdtree(axis data)       
      (if (null data) nil   
          (if (eql (length data) 1) ; singleton?
              (list axis (first data)) ;; terminal node
              ;; else branch node:
              ;; #pts=odd  splits list into 2 even parts with sp in middle
              ;; #pts=even splits list into 2 uneven parts with shorter length first (but never nil)
              (let ((sd (sort (copy-list data) #'< :key (lambda (x) (nth axis x)))) ;; sort the axis ordinates 
                    (sp (truncate (/ (length data) 2))) ;; get mid pt
                    (nxta (mod (1+ axis) dims)))
                (list axis (nth sp sd) (make-kdtree nxta (subseq sd 0 sp)) (make-kdtree nxta (subseq sd (1+ sp))))))))

    ;;; depth first visit all nodes in kdtree and optionally apply a function to each node visited
    (defun visit-kdtree (kdt &key (node-function null))
      (when kdt
        (when node-function (funcall node-function kdt))
        (visit-kdtree (third kdt) :node-function node-function)
        (visit-kdtree (fourth kdt) :node-function node-function)))
    
    ;;; count of the terminal nodes
    (defun count-nodes (kdt)
      (if kdt
          (if (eql (length kdt) 2) 1
              (+ 1 (count-nodes (third kdt)) (count-nodes (fourth kdt))))
          0))
  
    ;;; nearest neighbour search
    (defun nn-kdtree (kdt node-stack)
      (when kdt
        ;; stage 1 - find the 'closest' terminal node using insertion logic
        (let ((best (do ((node kdt (next-node node))) ((not (next-node node)) (incf hits) node) ;; return first best est.
                      (push node node-stack) (incf hits)))) ; iteration                
          
          ;; stage 2 - unwind the path, at each node if node is closer then make it best
          (do ((node (pop node-stack) (pop node-stack))) ((null node) best) ;; return nearest pt                    
            ;; iteration: update best if node is closer
            (when (< (distance node) (distance best))
              (setf best node))

            ;; venture down opposing side if split point is inside HS
            (let ((opposing-best
                   (if (< (distance node 'semi) (distance best)) ; use semi dist here
                       (nn-kdtree (next-node node 'opposite) (list))
                       nil))) ;; otherwise ignore this subtree
          
              (when (and opposing-best (< (distance opposing-best) (distance best)))
                (setf best opposing-best)))))))

    ;;; process one set of data & optionally display tree
    (defun process (data tgt &optional (render nil))
      (setf target tgt)
      (setf dims (length target))
      (setf hits 0)
      (let* ((kdt (make-kdtree 0 data)) (nn (nn-kdtree kdt (list))))
        (when render
          (visit-kdtree kdt
                        :node-function (lambda (n)
                                         (format t "~A node: axis:~A point: ~A target:~A semi-distance-sqd:~A euclidean-distance-sqd:~A~%"
                                                 (if (not (next-node n)) "TERMINAL" "BRANCH") (first n) (second n) 
                                                 target (distance n 'semi) (distance n)))))        
        (format t "~%NN to ~A is ~A, distance ~A [tree has ~A nodes, ~A were visited.]~%" target (second nn) (sqrt (distance nn)) (count-nodes kdt) hits)))
    
    ;; MAIN: TASK 1 - nn search small set of 2D points
    (process '((2 3) (5 4) (9 6) (4 7) (8 1) (7 2)) '(9 2) 'render)
  
    ;; TASK 2 - nn search 1000 coordinate points in 3D space
    (process 
     (progn (let ((ll (list))) (dotimes (i 10) (dotimes (j 10) (dotimes (k 10) (push (list i j k) ll)))) ll))
     (list (float (/ (random 1000) 100)) (float (/ (random 1000) 100)) (float (/ (random 1000) 100))))))


```


{{out}}

```txt

CL-USER(21): (main)
BRANCH node: axis:0 point: (7 2) target:(9 2) semi-distance-sqd:4 euclidean-distance-sqd:4
BRANCH node: axis:1 point: (5 4) target:(9 2) semi-distance-sqd:4 euclidean-distance-sqd:20
TERMINAL node: axis:0 point: (2 3) target:(9 2) semi-distance-sqd:49 euclidean-distance-sqd:50
TERMINAL node: axis:0 point: (4 7) target:(9 2) semi-distance-sqd:25 euclidean-distance-sqd:50
BRANCH node: axis:1 point: (9 6) target:(9 2) semi-distance-sqd:16 euclidean-distance-sqd:16
TERMINAL node: axis:0 point: (8 1) target:(9 2) semi-distance-sqd:1 euclidean-distance-sqd:2

NN to (9 2) is (8 1), distance 1.4142135 [tree has 6 nodes, 3 were visited.]

NN to (7.52 8.82 2.55) is (8 9 3), distance 0.68212914 [tree has 1000 nodes, 51 were visited.]
NIL
CL-USER(22):

```



## D

{{trans|Go}}
Points are values, the code is templated on the the dimensionality of the points and the floating point type of the coordinate. Instead of sorting it uses the faster topN, that partitions the points array in two halves around their median.

```d
// Implmentation following pseudocode from
// "An introductory tutorial on kd-trees" by Andrew W. Moore,
// Carnegie Mellon University, PDF accessed from:
// http://www.autonlab.org/autonweb/14665

import std.typecons, std.math, std.algorithm, std.random, std.range,
       std.traits, core.memory;

/// k-dimensional point.
struct Point(size_t k, F) if (isFloatingPoint!F) {
    F[k] data;
    alias data this; // Kills DMD std.algorithm.swap inlining.
                     // Define opIndexAssign and opIndex for dmd.
    enum size_t length = k;

    /// Square of the euclidean distance.
    double sqd(in ref Point!(k, F) q) const pure nothrow @nogc {
        double sum = 0;
        foreach (immutable dim, immutable pCoord; data)
            sum += (pCoord - q[dim]) ^^ 2;
        return sum;
    }
}

// Following field names in the paper.
// rangeElt would be whatever data is associated with the Point.
// We don't bother with it for this example.
struct KdNode(size_t k, F) {
    Point!(k, F) domElt;
    immutable int split;
    typeof(this)* left, right;
}

struct Orthotope(size_t k, F) { /// k-dimensional rectangle.
    Point!(k, F) min, max;
}

struct KdTree(size_t k, F) {
    KdNode!(k, F)* n;
    Orthotope!(k, F) bounds;

    // Constructs a KdTree from a list of points, also associating the
    // bounds of the tree. The bounds could be computed of course, but
    // in this example we know them already. The algorithm is table
    // 6.3 in the paper.
    this(Point!(k, F)[] pts, in Orthotope!(k, F) bounds_) pure {
        static KdNode!(k, F)* nk2(size_t split)(Point!(k, F)[] exset)
        pure {
            if (exset.empty)
                return null;
            if (exset.length == 1)
                return new KdNode!(k, F)(exset[0], split, null, null);

            // Pivot choosing procedure. We find median, then find
            // largest index of points with median value. This
            // satisfies the inequalities of steps 6 and 7 in the
            // algorithm.
            auto m = exset.length / 2;
            topN!((p, q) => p[split] < q[split])(exset, m);
            immutable d = exset[m];
            while (m+1 < exset.length && exset[m+1][split] == d[split])
                m++;

            enum nextSplit = (split + 1) % d.length;//cycle coordinates
            return new KdNode!(k, F)(d, split,
                                     nk2!nextSplit(exset[0 .. m]),
                                     nk2!nextSplit(exset[m + 1 .. $]));
        }

        this.n = nk2!0(pts);
        this.bounds = bounds_;
    }
}

/**
Find nearest neighbor. Return values are:
  nearest neighbor--the ooint within the tree that is nearest p.
  square of the distance to that point.
  a count of the nodes visited in the search.
*/
auto findNearest(size_t k, F)(KdTree!(k, F) t, in Point!(k, F) p)
pure nothrow @nogc {
    // Algorithm is table 6.4 from the paper, with the addition of
    // counting the number nodes visited.
    static Tuple!(Point!(k, F), "nearest",
                  F, "distSqd",
                  int, "nodesVisited")
           nn(KdNode!(k, F)* kd, in Point!(k, F) target,
              Orthotope!(k, F) hr, F maxDistSqd) pure nothrow @nogc {
        if (kd == null)
            return typeof(return)(Point!(k, F)(), F.infinity, 0);

        int nodesVisited = 1;
        immutable s = kd.split;
        auto pivot = kd.domElt;
        auto leftHr = hr;
        auto rightHr = hr;
        leftHr.max[s] = pivot[s];
        rightHr.min[s] = pivot[s];

        KdNode!(k, F)* nearerKd, furtherKd;
        Orthotope!(k, F) nearerHr, furtherHr;
        if (target[s] <= pivot[s]) {
            //nearerKd, nearerHr = kd.left, leftHr;
            //furtherKd, furtherHr = kd.right, rightHr;
            nearerKd = kd.left;
            nearerHr = leftHr;
            furtherKd = kd.right;
            furtherHr = rightHr;
        } else {
            //nearerKd, nearerHr = kd.right, rightHr;
            //furtherKd, furtherHr = kd.left, leftHr;
            nearerKd = kd.right;
            nearerHr = rightHr;
            furtherKd = kd.left;
            furtherHr = leftHr;
        }

        auto n1 = nn(nearerKd, target, nearerHr, maxDistSqd);
        auto nearest = n1.nearest;
        auto distSqd = n1.distSqd;
        nodesVisited += n1.nodesVisited;

        if (distSqd < maxDistSqd)
            maxDistSqd = distSqd;
        auto d = (pivot[s] - target[s]) ^^ 2;
        if (d > maxDistSqd)
            return typeof(return)(nearest, distSqd, nodesVisited);
        d = pivot.sqd(target);
        if (d < distSqd) {
            nearest = pivot;
            distSqd = d;
            maxDistSqd = distSqd;
        }

        immutable n2 = nn(furtherKd, target, furtherHr, maxDistSqd);
        nodesVisited += n2.nodesVisited;
        if (n2.distSqd < distSqd) {
            nearest = n2.nearest;
            distSqd = n2.distSqd;
        }

        return typeof(return)(nearest, distSqd, nodesVisited);
    }

    return nn(t.n, p, t.bounds, F.infinity);
}

void showNearest(size_t k, F)(in string heading, KdTree!(k, F) kd,
                              in Point!(k, F) p) {
    import std.stdio: writeln;
    writeln(heading, ":");
    writeln("Point:            ", p);
    immutable n = kd.findNearest(p);
    writeln("Nearest neighbor: ", n.nearest);
    writeln("Distance:         ", sqrt(n.distSqd));
    writeln("Nodes visited:    ", n.nodesVisited, "\n");
}

void main() {
    static Point!(k, F) randomPoint(size_t k, F)() {
        typeof(return) result;
        foreach (immutable i; 0 .. k)
            result[i] = uniform(F(0), F(1));
        return result;
    }

    static Point!(k, F)[] randomPoints(size_t k, F)(in size_t n) {
        return n.iota.map!(_ => randomPoint!(k, F)).array;
    }

    import std.stdio, std.conv, std.datetime, std.typetuple;
    rndGen.seed(1); // For repeatable outputs.

    alias D2 = TypeTuple!(2, double);
    alias P = Point!D2;
    auto kd1 = KdTree!D2([P([2, 3]), P([5, 4]), P([9, 6]),
                          P([4, 7]), P([8, 1]), P([7, 2])],
                         Orthotope!D2(P([0, 0]), P([10, 10])));
    showNearest("Wikipedia example data", kd1, P([9, 2]));

    enum int N = 400_000;
    alias F3 = TypeTuple!(3, float);
    alias Q = Point!F3;
    StopWatch sw;
    GC.disable;
    sw.start;
    auto kd2 = KdTree!F3(randomPoints!F3(N),
                         Orthotope!F3(Q([0, 0, 0]), Q([1, 1, 1])));
    sw.stop;
    GC.enable;
    showNearest(text("k-d tree with ", N,
                     " random 3D ", F3[1].stringof,
                     " points (construction time: ",
                     sw.peek.msecs, " ms)"), kd2, randomPoint!F3);

    sw.reset;
    sw.start;
    enum int M = 10_000;
    size_t visited = 0;
    foreach (immutable _; 0 .. M) {
        immutable n = kd2.findNearest(randomPoint!F3);
        visited += n.nodesVisited;
    }
    sw.stop;

    writefln("Visited an average of %0.2f nodes on %d searches " ~
             "in %d ms.", visited / double(M), M, sw.peek.msecs);
}
```

{{out|Output}} using the ldc2 compiler

```txt
Wikipedia example data:
Point:            [9, 2]
Nearest neighbor: [8, 1]
Distance:         1.41421
Nodes visited:    3

k-d tree with 400000 random 3D float points (construction time: 250 ms):
Point:            [0.22012, 0.984514, 0.698782]
Nearest neighbor: [0.225766, 0.978981, 0.69885]
Distance:         0.00790531
Nodes visited:    54

Visited an average of 43.10 nodes on 10000 searches in 33 ms.
```



### Faster Alternative Version

{{trans|C}}
This version performs less lookups. Compiled with DMD this version is two times slower than the C version. Compiled with ldc2 it's a little faster than the C version compiled with gcc.

```d
import std.stdio, std.algorithm, std.math, std.random;

struct KdNode(size_t dim) {
    double[dim] x;
    KdNode* left, right;
}

// See QuickSelect method.
KdNode!dim* findMedian(size_t idx, size_t dim)(KdNode!dim[] nodes) pure nothrow @nogc {
    auto start = nodes.ptr;
    auto end = &nodes[$ - 1] + 1;

    if (end <= start)
        return null;
    if (end == start + 1)
        return start;

    auto md = start + (end - start) / 2;

    while (true) {
        immutable double pivot = md.x[idx];

        swap(md.x, (end - 1).x); // Swaps the whole arrays x.
        auto store = start;
        foreach (p; start .. end) {
            if (p.x[idx] < pivot) {
                if (p != store)
                    swap(p.x, store.x);
                store++;
            }
        }
        swap(store.x, (end - 1).x);

        // Median has duplicate values.
        if (store.x[idx] == md.x[idx])
            return md;

        if (store > md)
            end = store;
        else
            start = store;
    }
}

KdNode!dim* makeTree(size_t dim, size_t i = 0)(KdNode!dim[] nodes)
pure nothrow @nogc {
    if (!nodes.length)
        return null;

    auto n = nodes.findMedian!i;
    if (n != null) {
        enum i2 = (i + 1) % dim;
        immutable size_t nPos = n - nodes.ptr;
        n.left = makeTree!(dim, i2)(nodes[0 .. nPos]);
        n.right = makeTree!(dim, i2)(nodes[nPos + 1 .. $]);
    }

    return n;
}

void nearest(size_t dim)(in KdNode!dim* root,
                         in ref KdNode!dim nd,
                         in size_t i,
                         ref const(KdNode!dim)* best,
                         ref double bestDist,
                         ref size_t nVisited) pure nothrow @safe @nogc {
    static double dist(in ref KdNode!dim a, in ref KdNode!dim b)
    pure nothrow @nogc {
        double result = 0;
        static foreach (i; 0 .. dim)
            result += (a.x[i] - b.x[i]) ^^ 2;
        return result;
    }

    if (root == null)
        return;

    immutable double d = dist(*root, nd);
    immutable double dx = root.x[i] - nd.x[i];
    immutable double dx2 = dx ^^ 2;
    nVisited++;

    if (!best || d < bestDist) {
        bestDist = d;
        best = root;
    }

    // If chance of exact match is high.
    if (!bestDist)
        return;

    immutable i2 = (i + 1 >= dim) ? 0 : i + 1;

    nearest!dim(dx > 0 ? root.left : root.right,
                nd, i2, best, bestDist, nVisited);
    if (dx2 >= bestDist)
        return;
    nearest!dim(dx > 0 ? root.right : root.left,
                nd, i2, best, bestDist, nVisited);
}

void randPt(size_t dim)(ref KdNode!dim v, ref Xorshift rng)
pure nothrow @safe @nogc {
    static foreach (i; 0 .. dim)
        v.x[i] = rng.uniform01;
}

/// smallTest
unittest {
    KdNode!2[] wp = [{[2, 3]}, {[5, 4]}, {[9, 6]},
                   {[4, 7]}, {[8, 1]}, {[7, 2]}];
    KdNode!2 thisPt = {[9, 2]};

    auto root = makeTree(wp);

    const(KdNode!2)* found = null;
    double bestDist = 0;
    size_t nVisited = 0;
    root.nearest(thisPt, 0, found, bestDist, nVisited);

    writefln("WP tree:\n  Searching for %s\n" ~
             "  Found %s, dist = %g\n  Seen %d nodes.\n",
             thisPt.x, found.x, sqrt(bestDist), nVisited);
}

/// bigTest
unittest {
    enum N = 1_000_000;
    enum testRuns = 100_000;

    auto bigTree = new KdNode!3[N];
    auto rng = 1.Xorshift;
    foreach (ref node; bigTree)
        randPt(node, rng);

    auto root = makeTree(bigTree);
    KdNode!3 thisPt;
    randPt(thisPt, rng);

    const(KdNode!3)* found = null;
    double bestDist = 0;
    size_t nVisited = 0;
    root.nearest(thisPt, 0, found, bestDist, nVisited);

    writefln("Big tree (%d nodes):\n  Searching for %s\n" ~ "  Found %s, dist = %g\n  Seen %d nodes.", N, thisPt.x, found.x, sqrt(bestDist), nVisited);

    size_t sum = 0;
    foreach (immutable _; 0 .. testRuns) {
        found = null;
        nVisited = 0;
        randPt(thisPt, rng);
        nearest!3(root, thisPt, 0, found, bestDist, nVisited);
        sum += nVisited;
    }
    writefln("\nBig tree:\n  Visited %d nodes for %d random " ~
             "searches (%.2f per lookup).",
             sum, testRuns, sum / double(testRuns));
}

```

{{out}}

```txt
WP tree:
  Searching for [9, 2]
  Found [8, 1], dist = 1.41421
  Seen 3 nodes.

Big tree (1000000 nodes):
  Searching for [0.225893, 0.725471, 0.486279]
  Found [0.220761, 0.729613, 0.489134], dist = 0.00718703
  Seen 35 nodes.

Big tree:
  Visited 4267592 nodes for 100000 random searches (42.68 per lookup).
```



## Go


```go
// Implmentation following pseudocode from "An intoductory tutorial on kd-trees"
// by Andrew W. Moore, Carnegie Mellon University, PDF accessed from
// http://www.autonlab.org/autonweb/14665
package main

import (
    "fmt"
    "math"
    "math/rand"
    "sort"
    "time"
)

// point is a k-dimensional point.
type point []float64

// sqd returns the square of the euclidean distance.
func (p point) sqd(q point) float64 {
    var sum float64
    for dim, pCoord := range p {
        d := pCoord - q[dim]
        sum += d * d
    }
    return sum
}

// kdNode following field names in the paper.
// rangeElt would be whatever data is associated with the point.  we don't
// bother with it for this example.
type kdNode struct {
    domElt      point
    split       int
    left, right *kdNode
}   

type kdTree struct {
    n      *kdNode
    bounds hyperRect
}
    
type hyperRect struct {
    min, max point
}

// Go slices are reference objects.  The data must be copied if you want
// to modify one without modifying the original.
func (hr hyperRect) copy() hyperRect {
    return hyperRect{append(point{}, hr.min...), append(point{}, hr.max...)}
}   
    
// newKd constructs a kdTree from a list of points, also associating the
// bounds of the tree.  The bounds could be computed of course, but in this
// example we know them already.  The algorithm is table 6.3 in the paper.
func newKd(pts []point, bounds hyperRect) kdTree {
    var nk2 func([]point, int) *kdNode
    nk2 = func(exset []point, split int) *kdNode {
        if len(exset) == 0 {
            return nil
        }
        // pivot choosing procedure.  we find median, then find largest
        // index of points with median value.  this satisfies the
        // inequalities of steps 6 and 7 in the algorithm.
        sort.Sort(part{exset, split})
        m := len(exset) / 2
        d := exset[m]
        for m+1 < len(exset) && exset[m+1][split] == d[split] {
            m++
        }
        // next split
        s2 := split + 1
        if s2 == len(d) {
            s2 = 0
        }
        return &kdNode{d, split, nk2(exset[:m], s2), nk2(exset[m+1:], s2)}
    }
    return kdTree{nk2(pts, 0), bounds}
}

// a container type used for sorting.  it holds the points to sort and
// the dimension to use for the sort key.
type part struct {
    pts   []point
    dPart int
}

// satisfy sort.Interface
func (p part) Len() int { return len(p.pts) }
func (p part) Less(i, j int) bool {
    return p.pts[i][p.dPart] < p.pts[j][p.dPart]
}
func (p part) Swap(i, j int) { p.pts[i], p.pts[j] = p.pts[j], p.pts[i] }

// nearest.  find nearest neighbor.  return values are:
//    nearest neighbor--the point within the tree that is nearest p.
//    square of the distance to that point.
//    a count of the nodes visited in the search.
func (t kdTree) nearest(p point) (best point, bestSqd float64, nv int) {
    return nn(t.n, p, t.bounds, math.Inf(1))
}

// algorithm is table 6.4 from the paper, with the addition of counting
// the number nodes visited.
func nn(kd *kdNode, target point, hr hyperRect,
    maxDistSqd float64) (nearest point, distSqd float64, nodesVisited int) {
    if kd == nil {
        return nil, math.Inf(1), 0
    }
    nodesVisited++
    s := kd.split
    pivot := kd.domElt
    leftHr := hr.copy()
    rightHr := hr.copy()
    leftHr.max[s] = pivot[s]
    rightHr.min[s] = pivot[s]
    targetInLeft := target[s] <= pivot[s]
    var nearerKd, furtherKd *kdNode
    var nearerHr, furtherHr hyperRect
    if targetInLeft {
        nearerKd, nearerHr = kd.left, leftHr
        furtherKd, furtherHr = kd.right, rightHr
    } else {
        nearerKd, nearerHr = kd.right, rightHr
        furtherKd, furtherHr = kd.left, leftHr
    }
    var nv int
    nearest, distSqd, nv = nn(nearerKd, target, nearerHr, maxDistSqd)
    nodesVisited += nv
    if distSqd < maxDistSqd {
        maxDistSqd = distSqd
    }
    d := pivot[s] - target[s]
    d *= d
    if d > maxDistSqd {
        return
    }
    if d = pivot.sqd(target); d < distSqd {
        nearest = pivot
        distSqd = d
        maxDistSqd = distSqd
    }
    tempNearest, tempSqd, nv := nn(furtherKd, target, furtherHr, maxDistSqd)
    nodesVisited += nv
    if tempSqd < distSqd {
        nearest = tempNearest
        distSqd = tempSqd
    }
    return
}

func main() {
    rand.Seed(time.Now().Unix())
    kd := newKd([]point{{2, 3}, {5, 4}, {9, 6}, {4, 7}, {8, 1}, {7, 2}},
        hyperRect{point{0, 0}, point{10, 10}})
    showNearest("WP example data", kd, point{9, 2})
    kd = newKd(randomPts(3, 1000), hyperRect{point{0, 0, 0}, point{1, 1, 1}})
    showNearest("1000 random 3d points", kd, randomPt(3))
}   
    
func randomPt(dim int) point {
    p := make(point, dim)
    for d := range p {
        p[d] = rand.Float64()
    }
    return p
}   
    
func randomPts(dim, n int) []point {
    p := make([]point, n)
    for i := range p {
        p[i] = randomPt(dim) 
    } 
    return p
}
    
func showNearest(heading string, kd kdTree, p point) {
    fmt.Println()
    fmt.Println(heading)
    fmt.Println("point:           ", p)
    nn, ssq, nv := kd.nearest(p)
    fmt.Println("nearest neighbor:", nn)
    fmt.Println("distance:        ", math.Sqrt(ssq))
    fmt.Println("nodes visited:   ", nv)
}
```

{{out}}

```txt

WP example data
point:            [9 2]
nearest neighbor: [8 1]
distance:         1.4142135623730951
nodes visited:    3

1000 random 3d points
point:            [0.314731890562714 0.5908890147906868 0.2657722255021785]
nearest neighbor: [0.2541611609533609 0.5781168738628141 0.27829000365095274]
distance:         0.06315564613771865
nodes visited:    25

```



## Haskell

There is a space leak when creating the trees which will lead to terrible performance on massive trees. This can probably be quelled with strictness annotations.

```Haskell
import System.Random
import Data.List (sortBy, genericLength, minimumBy)
import Data.Ord (comparing)

-- A finite list of dimensional accessors tell a KDTree how to get a
-- Euclidean dimensional value 'b' out of an arbitrary datum 'a'.
type DimensionalAccessors a b = [a -> b]

-- A binary tree structure of 'a'.
data Tree a = Node a (Tree a) (Tree a)
            | Empty

instance Show a => Show (Tree a) where
  show Empty = "Empty"
  show (Node value left right) =
    "(" ++ show value ++ " " ++ show left ++ " " ++ show right ++ ")" 

-- A k-d tree structure of 'a' with Euclidean dimensions of 'b'.
data KDTree a b = KDTree (DimensionalAccessors a b) (Tree a)

instance Show a => Show (KDTree a b) where
  show (KDTree _ tree) = "KDTree " ++ show tree

-- The squared Euclidean distance formula.
sqrDist :: Num b => DimensionalAccessors a b -> a -> a -> b
sqrDist dims a b = sum $ map square $ zipWith (-) a' b'
  where
    a' = map ($ a) dims
    b' = map ($ b) dims

square :: Num a => a -> a
square = (^ 2)

-- Insert a value into a k-d tree.
insert :: Ord b => KDTree a b -> a -> KDTree a b
insert (KDTree dims tree) value = KDTree dims $ ins (cycle dims) tree
  where
    ins _      Empty                   = Node value Empty Empty
    ins (d:ds) (Node split left right) =
      if d value < d split
      then Node split (ins ds left) right
      else Node split left (ins ds right)

-- Produce an empty k-d tree.
empty :: DimensionalAccessors a b -> KDTree a b
empty dims = KDTree dims Empty

-- Produce a k-d tree with one value.
singleton :: Ord b => DimensionalAccessors a b -> a -> KDTree a b
singleton dims value = insert (empty dims) value

-- Create a k-d tree from a list of values using the median-finding algorithm.
fromList :: Ord b => DimensionalAccessors a b -> [a] -> KDTree a b
fromList dims values = KDTree dims $ fList (cycle dims) values
  where
    fList _      []     = Empty
    fList (d:ds) values =
      let sorted          = sortBy (comparing d) values
          (lower, higher) = splitAt (genericLength sorted `div` 2) sorted
      in case higher of
        []          -> Empty
        median:rest -> Node median (fList ds lower) (fList ds rest)

-- Create a k-d tree from a list of values by repeatedly inserting the values
-- into a tree. Faster than median-finding, but can create unbalanced trees.
fromListLinear :: Ord b => DimensionalAccessors a b -> [a] -> KDTree a b
fromListLinear dims values = foldl insert (empty dims) values

-- Given a k-d tree, find the nearest value to a given value.
-- Also report how many nodes were visited.
nearest :: (Ord b, Num b, Integral c) => KDTree a b -> a -> (Maybe a, c)
nearest (KDTree dims tree) value = near (cycle dims) tree
  where
    dist = sqrDist dims
    -- If we have an empty tree, then return nothing.
    near _      Empty                    = (Nothing, 1)
    -- We hit a leaf node, so it is the current best.
    near _      (Node split Empty Empty) = (Just split, 1)
    near (d:ds) (Node split left right)  =
      -- Move down the tree in the fashion of insertion.
      let dimdist x y    = square (d x - d y)
          splitDist      = dist value split
          hyperPlaneDist = dimdist value split
          bestLeft       = near ds left
          bestRight      = near ds right
          -- maybeThisBest is the node of the side of the split where the value
          -- resides, and maybeOtherBest is the node on the other side of the split.
          ((maybeThisBest, thisCount), (maybeOtherBest, otherCount)) =
            if d value < d split
            then (bestLeft, bestRight)
            else (bestRight, bestLeft)
      in case maybeThisBest of
        Nothing    ->
          -- From the search point (in this case), the hypersphere radius to
          -- the split node is always >= the hyperplane distance, so we will
          -- always check the other side.
          let count = 1 + thisCount + otherCount
          in case maybeOtherBest of
            -- We are currently at a leaf node, so this is the only choice.
            -- It is not strictly necessary to take care of this case
            -- because of the above pattern matching in near.
            Nothing         -> (Just split, count)
            -- We have a node on the other side, so compare
            -- it to the split point to see which is closer.
            Just otherBest ->
              if dist value otherBest < splitDist
              then (maybeOtherBest, count)
              else (Just split, count)
        
        Just thisBest ->
          let thisBestDist = dist value thisBest
              best         =
                -- Determine which is the closer node of this side.
                if splitDist < thisBestDist
                then split
                else thisBest
              bestDist     = dist value best
          in
            if bestDist < hyperPlaneDist
            -- If the distance to the best node is less than the distance
            -- to the splitting hyperplane, then the current best node is the
            -- only choice.
            then (Just best, 1 + thisCount)
            -- There is a chance that a node on the other side is closer
            -- than the current best.
            else
              let count = 1 + thisCount + otherCount
              in case maybeOtherBest of
                Nothing        -> (Just best, count)
                Just otherBest ->
                  if bestDist < dist value otherBest
                  then (Just best, count)
                  else (maybeOtherBest, count)

-- Dimensional accessors for a 2-tuple
tuple2D :: [(a, a) -> a]
tuple2D = [fst, snd]

-- Dimensional accessors for a 3-tuple
tuple3D :: [(a, a, a) -> a]
tuple3D = [d1, d2, d3]
  where
    d1 (a, _, _) = a
    d2 (_, b, _) = b
    d3 (_, _, c) = c

-- Random 3-tuple generation
instance (Random a, Random b, Random c) => Random (a, b, c) where
  random gen =
    let (vA, genA) = random gen
        (vB, genB) = random genA
        (vC, genC) = random genB
    in ((vA, vB, vC), genC)

  randomR ((lA, lB, lC), (hA, hB, hC)) gen =
    let (vA, genA) = randomR (lA, hA) gen
        (vB, genB) = randomR (lB, hB) genA
        (vC, genC) = randomR (lC, hC) genB
    in ((vA, vB, vC), genC)

printResults :: (Show a, Show b, Show c, Floating c) =>
                a -> (Maybe a, b) -> DimensionalAccessors a c -> IO ()
printResults point result dims = do
  let (nearest, visited) = result
  case nearest of
    Nothing    -> putStrLn "Could not find nearest."
    Just value -> do
      let dist = sqrt $ sqrDist dims point value
      putStrLn $ "Point:    " ++ show point
      putStrLn $ "Nearest:  " ++ show value
      putStrLn $ "Distance: " ++ show dist
      putStrLn $ "Visited:  " ++ show visited
      putStrLn ""

-- Naive nearest search used to confirm results.
linearNearest :: (Ord b, Num b) => DimensionalAccessors a b -> a -> [a] -> Maybe a
linearNearest _    _     [] = Nothing
linearNearest dims value xs = Just $ minimumBy (comparing $ sqrDist dims value) xs

main :: IO ()
main = do
  let wikiValues :: [(Double, Double)]
      wikiValues  = [(2, 3), (5, 4), (9, 6), (4, 7), (8, 1), (7, 2)]
      wikiTree    = fromList tuple2D wikiValues
      wikiSearch  = (9, 2)
      wikiNearest = nearest wikiTree wikiSearch
  putStrLn "Wikipedia example:"
  printResults wikiSearch wikiNearest tuple2D

  let stdGen                = mkStdGen 0
      randRange :: ((Double, Double, Double), (Double, Double, Double))
      randRange             = ((0, 0, 0), (1000, 1000, 1000))
      (randSearch, stdGenB) = randomR randRange stdGen
      randValues            = take 1000 $ randomRs randRange stdGenB
      randTree              = fromList tuple3D randValues
      randNearest           = nearest randTree randSearch
      randNearestLinear     = linearNearest tuple3D randSearch randValues
  putStrLn "1000 random 3D points on the range of [0, 1000):"
  printResults randSearch randNearest tuple3D
  putStrLn "Confirm naive nearest:"
  print randNearestLinear
```

{{out}}

```txt
Wikipedia example:
Point:    (9.0,2.0)
Nearest:  (8.0,1.0)
Distance: 1.4142135623730951
Visited:  3

1000 random 3D points on the range of [0, 1000):
Point:    (992.9251340102518,993.3624439225405,464.8305261946105)
Nearest:  (939.1965739740829,980.2876583283734,452.4829965078272)
Distance: 56.658359235505955
Visited:  23

Confirm naive nearest:
Just (939.1965739740829,980.2876583283734,452.4829965078272)
```



## J


As a general rule, tree algorithms are a bad idea in J. That said, here's an implementation:


```J
coclass 'kdnode'
  create=:3 :0
    Axis=: ({:$y)|<.2^.#y
    Mask=: Axis~:i.{:$y
    if. 3>#y do.
      Leaf=:1
      Points=: y
    else.
      Leaf=:0
      data=. y /: Axis|."1 y
      n=. <.-:#data
      Points=: ,:n{data
      Left=: conew&'kdnode' n{.data
      Right=: conew&'kdnode' (1+n)}.data
    end.
  )

  distance=: +/&.:*:@:-"1

  nearest=:3 :0
    _ 0 nearest y
  :
    n=.' ',~":N_base_=:N_base_+1
    dists=. Points distance y
    ndx=. (i. <./) dists
    nearest=. ndx { Points
    range=. ndx { dists
    if. Leaf do.
      range;nearest return.
    else.
      d0=. x <. range
      p0=. nearest
      if. d0=0 do. 0;y return. end.
      if. 0={./:Axis|."1 y,Points do.
        'dist pnt'=.d0 nearest__Left y
        if. dist > d0 do. 
          d0;p0 return.
        end.
        if. dist < d0 do.
          if. dist > (Mask#pnt) distance Mask#,Points do. 
            'dist2 pnt2'=. d0 nearest__Right y
            if. dist2 < dist do. dist2;pnt2 return. end.
          end.
        end.
      else.
        'dist pnt'=. d0 nearest__Right y
        if. dist > d0 do.
          d0;p0 return.
        end.
        if. dist < d0 do.
          if. dist > (Mask#pnt) distance Mask#,Points do. 
            'dist2 pnt2'=. d0 nearest__Left y
            if. dist2 < dist do. dist2;pnt2 return. end.
          end.
        end.
      end.
    end.
    dist;pnt return.
  )

coclass 'kdtree'
  create=:3 :0
    root=: conew&'kdnode' y
  )
  nearest=:3 :0
    N_base_=:0
    'dist point'=. nearest__root y
    dist;N_base_;point
  )
```


And here's example use:


```J
   tree=:conew&'kdtree' (2,3), (5,4), (9,6), (4,7), (8,1),: (7,2)
   nearest__tree 9 2
┌───────┬─┬───┐
│1.41421│4│8 1│
└───────┴─┴───┘
```


The first box is distance from argument point to selected point. The second box is the number of nodes visited. The third box is the selected point.

Here's the bigger problem:


```J
   tree=:conew&'kdtree' dataset=:?1000 3$0
   nearest__tree pnt
┌─────────┬──┬──────────────────────────┐
│0.0387914│12│0.978082 0.767632 0.392523│
└─────────┴──┴──────────────────────────┘
```


So, why are trees "generally a bad idea in J"?

First off, that's a lot of code, it took time to write. Let's assume that that time was free. Let's also assume that the time taken to build the tree structure was free. We're going to use this tree billions of times. Now what?

Well, let's compare the above implementation to a brute force implementation for time. Here's a "visit all nodes" implementation. It should give us the same kinds of results but we will claim that each candidate point is a node so we'll be visiting a lot more "nodes":


```J
build0=:3 :0
  data=: y
)

distance=: +/&.:*:@:-"1

nearest0=:3 :0
  nearest=. data {~ (i. <./) |data distance y
  (nearest distance y);(#data);nearest
)
```


Here's the numbers we get:


```J
   build0 (2,3), (5,4), (9,6), (4,7), (8,1),: (7,2)
   nearest0 9 2
┌───────┬─┬───┐
│1.41421│6│8 1│
└───────┴─┴───┘
   build0 dataset
   nearest0 pnt
┌─────────┬────┬──────────────────────────┐
│0.0387914│1000│0.978082 0.767632 0.392523│
└─────────┴────┴──────────────────────────┘
```


But what about timing?


```J
   tree=:conew&'kdtree' (2,3), (5,4), (9,6), (4,7), (8,1),: (7,2)
   timespacex 'nearest__tree 9 2'
0.000487181 19328
   build0 (2,3), (5,4), (9,6), (4,7), (8,1),: (7,2)
   timespacex 'nearest0 9 2'
3.62419e_5 6016
```


The kdtree implementation is over ten times slower than the brute force implementation for this small dataset. How about the bigger dataset?


```J
   tree=:conew&'kdtree' dataset
   timespacex 'nearest__tree pnt'
0.00141408 45312
   build0 dataset
   timespacex 'nearest0 pnt'
0.00140702 22144
```


On the bigger dataset, the kdtree implementation is about the same speed as the brute force implementation.

For a more practical approach to this kind of problem, see https://github.com/locklin/j-nearest-neighbor (that is: link to a high performance implementation).

See also: [[wp:KISS_principle]]



## Julia


```julia
ENV["NN_DEBUG"] = true # or change DEBUG = true in NearestNeighbors.jl file

using NearestNeighbors

const data = [[2, 3] [5, 4] [9, 6] [4, 7] [8, 1] [7, 2]]

NearestNeighbors.reset_stats()
const kdtree = KDTree(Float64.(data))
const indexpoint = [9,2]
idx, dist = knn(kdtree, indexpoint, 1)
println("Wikipedia example: The nearest neighbor to $indexpoint is ",
    "$(data[1:2, idx[1]]) at distance $(dist).")
NearestNeighbors.print_stats()

NearestNeighbors.reset_stats()
const cubedis = rand(3, 1000)
const kdcubetree = KDTree(cubedis)
const rand3point = rand(3, 1)
idx, dist = knn(kdcubetree, rand3point, 1)
println("\n\n1000 cube points: The point $rand3point is closest to the point $(cubedis[1:3, idx[1]])",
    " at distance $(dist[1]).")
NearestNeighbors.print_stats()

NearestNeighbors.reset_stats()
const cubedis2 = rand(3, 500000)
const kdcubetree2 = KDTree(cubedis2)
const rand3point2 = rand(3, 1)
idx, dist = knn(kdcubetree2, rand3point2, 1)
println("\n\nExtra: The point $rand3point2 is closest to the point $(cubedis2[1:3, idx[1]]) out of $(size(cubedis2)[2]) points,",
    " at distance $(dist[1]).")
NearestNeighbors.print_stats()

```

{{out}}

```txt

Wikipedia example: The nearest neighbor to [9, 2] is [8, 1] at distance [1.41421].
Nodes visited: 1
Points visited: 6, out of these: 0 unchecked.


1000 cube points: The point [0.386894; 0.465532; 0.710547] is closest to the point [0.389152; 0.459995; 0.639123] at distance [0.0716743].
Nodes visited: 13
Points visited: 30, out of these: 0 unchecked.


Extra: The point [0.373496; 0.740711; 0.417431] is closest to the point [0.377841; 0.744084; 0.420237] out of 500000 points, at distance [0.00617466].
Nodes visited: 28
Points visited: 40, out of these: 0 unchecked.

```



## Kotlin

{{trans|Go}}

```scala
// version 1.1.51

import java.util.Random

typealias Point = DoubleArray

fun Point.sqd(p: Point) = this.zip(p) { a, b -> (a - b) * (a - b) }.sum()

class HyperRect (val min: Point, val max: Point) {
    fun copy() = HyperRect(min.copyOf(), max.copyOf())
}

data class NearestNeighbor(val nearest: Point?, val distSqd: Double, val nodesVisited: Int)

class KdNode(
    val domElt: Point,
    val split: Int,
    var left:  KdNode?,
    var right: KdNode?
)

class KdTree {
    val n: KdNode?
    val bounds: HyperRect

    constructor(pts: MutableList<Point>, bounds: HyperRect) {
        fun nk2(exset: MutableList<Point>, split: Int): KdNode? {
            if (exset.size == 0) return null
            val exset2 = exset.sortedBy { it[split] }
            for (i in 0 until exset.size) exset[i] = exset2[i]
            var m = exset.size / 2
            val d = exset[m]
            while (m + 1 < exset.size && exset[m + 1][split] == d[split]) m++
            var s2 = split + 1
            if (s2 == d.size) s2 = 0
            return KdNode(
                d,
                split,
                nk2(exset.subList(0, m), s2),
                nk2(exset.subList(m + 1, exset.size), s2)
            )
        }
        this.n = nk2(pts, 0)
        this.bounds = bounds
    }

    fun nearest(p: Point) = nn(n, p, bounds, Double.POSITIVE_INFINITY)

    private fun nn(
        kd: KdNode?,
        target: Point, 
        hr: HyperRect,
        maxDistSqd: Double
    ): NearestNeighbor {
        if (kd == null) return NearestNeighbor(null, Double.POSITIVE_INFINITY, 0)
        var nodesVisited = 1
        val s = kd.split
        val pivot = kd.domElt
        val leftHr = hr.copy()
        val rightHr = hr.copy()
        leftHr.max[s] = pivot[s]
        rightHr.min[s] = pivot[s]
        val targetInLeft = target[s] <= pivot[s]
        val nearerKd = if (targetInLeft) kd.left else kd.right
        val nearerHr = if (targetInLeft) leftHr else rightHr
        val furtherKd = if (targetInLeft) kd.right else kd.left
        val furtherHr = if (targetInLeft) rightHr else leftHr
        var (nearest, distSqd, nv) = nn(nearerKd, target, nearerHr, maxDistSqd)
        nodesVisited += nv
        var maxDistSqd2 = if (distSqd < maxDistSqd) distSqd else maxDistSqd
        var d = pivot[s] - target[s]
        d *= d
        if (d > maxDistSqd2) return NearestNeighbor(nearest, distSqd, nodesVisited)
        d = pivot.sqd(target)
        if (d < distSqd) {
            nearest = pivot
            distSqd = d
            maxDistSqd2 = distSqd
        }
        val temp = nn(furtherKd, target, furtherHr, maxDistSqd2)
        nodesVisited += temp.nodesVisited
        if (temp.distSqd < distSqd) {
            nearest = temp.nearest
            distSqd = temp.distSqd
        }
        return NearestNeighbor(nearest, distSqd, nodesVisited)
    }
}

val rand = Random()

fun randomPt(dim: Int) = Point(dim) { rand.nextDouble() }

fun randomPts(dim: Int, n: Int) = MutableList<Point>(n) { randomPt(dim) }

fun showNearest(heading: String, kd: KdTree, p: Point) {
    println("$heading:")
    println("Point            : ${p.asList()}")
    val (nn, ssq, nv) = kd.nearest(p)
    println("Nearest neighbor : ${nn?.asList()}")
    println("Distance         : ${Math.sqrt(ssq)}")
    println("Nodes visited    : $nv")
    println()
}

fun main(args: Array<String>) {
    val points = mutableListOf(
        doubleArrayOf(2.0, 3.0),
        doubleArrayOf(5.0, 4.0),
        doubleArrayOf(9.0, 6.0),
        doubleArrayOf(4.0, 7.0),
        doubleArrayOf(8.0, 1.0),
        doubleArrayOf(7.0, 2.0)
    )
    var hr = HyperRect(doubleArrayOf(0.0, 0.0), doubleArrayOf(10.0, 10.0))
    var kd = KdTree(points, hr)
    showNearest("WP example data", kd, doubleArrayOf(9.0, 2.0))

    hr = HyperRect(doubleArrayOf(0.0, 0.0, 0.0), doubleArrayOf(1.0, 1.0, 1.0))
    kd = KdTree(randomPts(3, 1000), hr)
    showNearest("1000 random 3D points", kd, randomPt(3))

    hr = hr.copy()
    kd = KdTree(randomPts(3, 400_000), hr)
    showNearest("400,000 random 3D points", kd, randomPt(3))
}
```


Sample output:

```txt

WP example data:
Point            : [9.0, 2.0]
Nearest neighbor : [8.0, 1.0]
Distance         : 1.4142135623730951
Nodes visited    : 3

1000 random 3D points:
Point            : [0.5682408811963585, 0.6635465099329054, 0.9023807963146424]
Nearest neighbor : [0.5552585116046708, 0.6604379445683164, 0.8609079250093159]
Distance         : 0.04356838478930882
Nodes visited    : 17

400,000 random 3D points:
Point            : [0.72025478016928, 0.4029017493035246, 0.9535502838653142]
Nearest neighbor : [0.7200735279619622, 0.3974244838953903, 0.9528380574924709]
Distance         : 0.005526350976908346
Nodes visited    : 43

```



## Perl 6

{{trans|Python}}

```perl6
class Kd_node {
    has $.d;
    has $.split;
    has $.left;
    has $.right;
}

class Orthotope {
    has $.min;
    has $.max;
}

class Kd_tree {
    has $.n;
    has $.bounds;
    method new($pts, $bounds) { self.bless(n => nk2(0,$pts), bounds => $bounds) }

    sub nk2($split, @e) {
        return () unless @e;
        my @exset = @e.sort(*.[$split]);
        my $m = +@exset div 2;
        my @d = @exset[$m][];
        while $m+1 < @exset and @exset[$m+1][$split] eqv @d[$split] {
            ++$m;
        }

        my $s2 = ($split + 1) % @d; # cycle coordinates
        Kd_node.new: :@d, :$split,
                left  => nk2($s2, @exset[0 ..^ $m]),
                right => nk2($s2, @exset[$m ^.. *]);
    }
}

class T3 {
    has $.nearest;
    has $.dist_sqd = Inf;
    has $.nodes_visited = 0;
}

sub find_nearest($k, $t, @p) {
    return nn($t.n, @p, $t.bounds, Inf);

    sub nn($kd, @target, $hr, $max_dist_sqd is copy) {
        return T3.new(:nearest([0.0 xx $k])) unless $kd;

        my $nodes_visited = 1;
        my $s = $kd.split;
        my $pivot = $kd.d;
        my $left_hr = $hr.clone;
        my $right_hr = $hr.clone;
        $left_hr.max[$s] = $pivot[$s];
        $right_hr.min[$s] = $pivot[$s];

        my $nearer_kd;
        my $further_kd;
        my $nearer_hr;
        my $further_hr;
        if @target[$s] <= $pivot[$s] {
            ($nearer_kd, $nearer_hr) = $kd.left, $left_hr;
            ($further_kd, $further_hr) = $kd.right, $right_hr;
        }
        else {
            ($nearer_kd, $nearer_hr) = $kd.right, $right_hr;
            ($further_kd, $further_hr) = $kd.left, $left_hr;
        }
 
        my $n1 = nn($nearer_kd, @target, $nearer_hr, $max_dist_sqd);
        my $nearest = $n1.nearest;
        my $dist_sqd = $n1.dist_sqd;
        $nodes_visited += $n1.nodes_visited;

        if $dist_sqd < $max_dist_sqd {
            $max_dist_sqd = $dist_sqd;
        }
        my $d = ($pivot[$s] - @target[$s]) ** 2;
        if $d > $max_dist_sqd {
            return T3.new(:$nearest, :$dist_sqd, :$nodes_visited);
        }
        $d = [+] (@$pivot Z- @target) X** 2;
        if $d < $dist_sqd {
            $nearest = $pivot;
            $dist_sqd = $d;
            $max_dist_sqd = $dist_sqd;
        }

        my $n2 = nn($further_kd, @target, $further_hr, $max_dist_sqd);
        $nodes_visited += $n2.nodes_visited;
        if $n2.dist_sqd < $dist_sqd {
            $nearest = $n2.nearest;
            $dist_sqd = $n2.dist_sqd;
        }

        T3.new(:$nearest, :$dist_sqd, :$nodes_visited);
    }
}

sub show_nearest($k, $heading, $kd, @p) {
    print qq:to/END/;
        $heading:
        Point:            [@p.join(',')]
        END
    my $n = find_nearest($k, $kd, @p);
    print qq:to/END/;
        Nearest neighbor: [$n.nearest.join(',')]
        Distance:         &sqrt($n.dist_sqd)
        Nodes visited:    $n.nodes_visited()
        
        END
}

sub random_point($k) { [rand xx $k] }
sub random_points($k, $n) { [random_point($k) xx $n] }

sub MAIN {
    my $kd1 = Kd_tree.new([[2, 3],[5, 4],[9, 6],[4, 7],[8, 1],[7, 2]],
                  Orthotope.new(:min([0, 0]), :max([10, 10])));
    show_nearest(2, "Wikipedia example data", $kd1, [9, 2]);

    my $N = 1000;
    my $t0 = now;
    my $kd2 = Kd_tree.new(random_points(3, $N), Orthotope.new(:min([0,0,0]), :max([1,1,1])));
    my $t1 = now;
    show_nearest(2,
        "k-d tree with $N random 3D points (generation time: {$t1 - $t0}s)",
         $kd2, random_point(3));
}
```

{{out}}

```txt
Wikipedia example data:
Point:            [9,2]
Nearest neighbor: [8,1]
Distance:         1.4142135623731
Nodes visited:    3

k-d tree with 1000 random 3D points (generation time: 67.0934954s):
Point:            [0.765565651400664,0.223251226280109,0.00536717765240979]
Nearest neighbor: [0.758919336088656,0.228895111242011,0.0383284709862686]
Distance:         0.0340950700678338
Nodes visited:    23
```



## Phix

{{trans|C}}
Added the 3D cube of 1000 points (0,0,0 to 9,9,9), which was not present in the C code, 
then inspired by the single "naughty global" of C (visited), I decided to outdo that as 
well, and have four of them ;-). [With enough motivation, many functions would probably
accept more parameters and return multiple values instead.]

```Phix
enum X,LEFT,RIGHT    -- (keeping the IDX on each node too would not be a bad idea..)
                    -- (the code below deduces it from the (unbalanced) tree depth)

sequence kd_nodes   -- nb probably not best coding style
 
function sqdist(sequence p,q)
    atom dsq = sum(sq_power(sq_sub(p,q),2))
    return dsq
end function

procedure swap(integer x,y)
    {kd_nodes[x],kd_nodes[y]} = {kd_nodes[y],kd_nodes[x]}
end procedure
 
function find_median(integer first, last, idx)
    if last<=first then return NULL end if
    if last==first+1 then return first end if
 
    integer p, stor, md = first+floor((last-first)/2)
    atom pivot
    while true do
        pivot = kd_nodes[md][X][idx]
 
        swap(md, last-1)
        stor = first
        for p=first to last-1 do
            if kd_nodes[p][X][idx]<pivot then
                if p!=stor then
                    swap(p, stor)
                end if
                stor += 1
            end if
        end for
        swap(stor, last-1)
 
        /* median has duplicate values */
        if kd_nodes[stor][X][idx] == kd_nodes[md][X][idx] then
            return md
        end if
        if stor>md then last = stor else first = stor end if
    end while
end function
 
function make_tree(object t, len, i, dim)
    if sequence(t) then kd_nodes = t; t = 1; end if
    if len=0 then return 0 end if
    integer n = find_median(t, t+len, i)
    if n then
        i = mod(i,dim)+1
        if length(kd_nodes[n])!=1 then ?9/0 end if -- (once)
        kd_nodes[n] = {kd_nodes[n][X],0,0} -- (add l/r slots)
        kd_nodes[n][LEFT] = make_tree(t, n-t, i, dim)
        kd_nodes[n][RIGHT] = make_tree(n+1, t+len-(n+1), i, dim)
    end if
    return n
end function
 
integer visited,
        best
atom best_dist
 
procedure nearest(integer root, sequence nd, integer i, dim)
    if root=0 then return end if
    atom d = sqdist(kd_nodes[root][X],nd[X]),
         dx = kd_nodes[root][X][i] - nd[X][i],
         dx2 = dx * dx;
 
    visited += 1
 
    if best=0 or d<best_dist then
        best_dist = d;
        best = root;
    end if
 
    /* if chance of exact match is high */
    if best_dist=0 then return end if

    i = mod(i,dim)+1
    integer {l,r} = kd_nodes[root][LEFT..RIGHT]
    {l,r} = iff(dx>0?{l,r}:{r,l})
    nearest(l, nd, i, dim)
    if (dx2 >= best_dist) then return end if
    nearest(r, nd, i, dim)
end procedure
 
function rand_pt() return sq_rnd({{0,0,0}}) end function

function make_3D()
    sequence s = {}
    for i=0 to 9 do
        for j=0 to 9 do
            for k=0 to 9 do
                s = append(s,{{i,j,k}})
            end for
        end for
    end for
    return s
end function

constant test_runs = 100_000

procedure test(string id, sequence nodes, sequence test_node, bool show_average_behaviour=false)
    integer dim = length(nodes[1][X]),
            root = make_tree(nodes, length(nodes), 1, dim)
    best = 0
    visited = 0
    nearest(root, test_node, 1, dim)
    string tn = sprint(test_node[X]),
           bn = sprint(kd_nodes[best][X])
    printf(1,"\n>> %s tree\nsearching for %s\n"&
         "found %s dist %g\nseen %d nodes\n",
         {id, tn, bn, sqrt(best_dist), visited})

    if show_average_behaviour then
        --
        -- search many random points to see average behavior.
        --
        -- tree size vs avg nodes visited:
        -- 10          ~  7
        -- 100         ~ 16.5
        -- 1000        ~ 25.5
        -- 10000       ~ 32.8
        -- 100000      ~ 38.3
        -- 1000000     ~ 42.6
        -- 10000000    ~ 46.7
        --
        int tot = 0
        for i=1 to test_runs do
            best = 0
            visited = 0
            test_node = rand_pt()
            nearest(root, test_node, 1, 3)
            tot += visited
        end for
        printf(1,"average behaviour: "&
                 "visited %d nodes for %,d random findings (%f per lookup)\n",
                {tot, test_runs, tot/test_runs});
    end if
end procedure

sequence wp = {{{2, 3}}, {{5, 4}}, {{9, 6}}, {{4, 7}}, {{8, 1}}, {{7, 2}}},
         test_node = {{9, 2}}
test("WP",wp,test_node)

sequence cube = make_3D()
test_node = sq_mul(rand_pt(),10)
test("3D",cube,test_node,true)

constant N = 1_000_000 
sequence million = repeat(0,N)
for i=1 to N do million[i] = rand_pt() end for
test_node = rand_pt()
test("Million",million,test_node,true)
```

{{out}}

```txt

>> WP tree
searching for {9,2}
found {8,1} dist 1.41421
seen 3 nodes

>> 3D tree
searching for {8.641182192,5.703920147,9.893391353}
found {9,6,9} dist 1.00725
seen 27 nodes
average behaviour: visited 2171052 nodes for 100,000 random findings (21.710520 per lookup)

>> Million tree
searching for {0.4157659043,0.6029827275,0.8408004809}
found {0.4204315508,0.6014092206,0.8489383544} dist 0.00951153
seen 43 nodes
average behaviour: visited 4267089 nodes for 100,000 random findings (42.670890 per lookup)

```



## Python

{{trans|D}}

```python
from random import seed, random
from time import clock
from operator import itemgetter
from collections import namedtuple
from math import sqrt
from copy import deepcopy


def sqd(p1, p2):
    return sum((c1 - c2) ** 2 for c1, c2 in zip(p1, p2))


class KdNode(object):
    __slots__ = ("dom_elt", "split", "left", "right")

    def __init__(self, dom_elt, split, left, right):
        self.dom_elt = dom_elt
        self.split = split
        self.left = left
        self.right = right


class Orthotope(object):
    __slots__ = ("min", "max")

    def __init__(self, mi, ma):
        self.min, self.max = mi, ma


class KdTree(object):
    __slots__ = ("n", "bounds")

    def __init__(self, pts, bounds):
        def nk2(split, exset):
            if not exset:
                return None
            exset.sort(key=itemgetter(split))
            m = len(exset) // 2
            d = exset[m]
            while m + 1 < len(exset) and exset[m + 1][split] == d[split]:
                m += 1

            s2 = (split + 1) % len(d)  # cycle coordinates
            return KdNode(d, split, nk2(s2, exset[:m]),
                                    nk2(s2, exset[m + 1:]))
        self.n = nk2(0, pts)
        self.bounds = bounds

T3 = namedtuple("T3", "nearest dist_sqd nodes_visited")


def find_nearest(k, t, p):
    def nn(kd, target, hr, max_dist_sqd):
        if kd is None:
            return T3([0.0] * k, float("inf"), 0)

        nodes_visited = 1
        s = kd.split
        pivot = kd.dom_elt
        left_hr = deepcopy(hr)
        right_hr = deepcopy(hr)
        left_hr.max[s] = pivot[s]
        right_hr.min[s] = pivot[s]

        if target[s] <= pivot[s]:
            nearer_kd, nearer_hr = kd.left, left_hr
            further_kd, further_hr = kd.right, right_hr
        else:
            nearer_kd, nearer_hr = kd.right, right_hr
            further_kd, further_hr = kd.left, left_hr

        n1 = nn(nearer_kd, target, nearer_hr, max_dist_sqd)
        nearest = n1.nearest
        dist_sqd = n1.dist_sqd
        nodes_visited += n1.nodes_visited

        if dist_sqd < max_dist_sqd:
            max_dist_sqd = dist_sqd
        d = (pivot[s] - target[s]) ** 2
        if d > max_dist_sqd:
            return T3(nearest, dist_sqd, nodes_visited)
        d = sqd(pivot, target)
        if d < dist_sqd:
            nearest = pivot
            dist_sqd = d
            max_dist_sqd = dist_sqd

        n2 = nn(further_kd, target, further_hr, max_dist_sqd)
        nodes_visited += n2.nodes_visited
        if n2.dist_sqd < dist_sqd:
            nearest = n2.nearest
            dist_sqd = n2.dist_sqd

        return T3(nearest, dist_sqd, nodes_visited)

    return nn(t.n, p, t.bounds, float("inf"))


def show_nearest(k, heading, kd, p):
    print(heading + ":")
    print("Point:           ", p)
    n = find_nearest(k, kd, p)
    print("Nearest neighbor:", n.nearest)
    print("Distance:        ", sqrt(n.dist_sqd))
    print("Nodes visited:   ", n.nodes_visited, "\n")


def random_point(k):
    return [random() for _ in range(k)]


def random_points(k, n):
    return [random_point(k) for _ in range(n)]

if __name__ == "__main__":
    seed(1)
    P = lambda *coords: list(coords)
    kd1 = KdTree([P(2, 3), P(5, 4), P(9, 6), P(4, 7), P(8, 1), P(7, 2)],
                  Orthotope(P(0, 0), P(10, 10)))
    show_nearest(2, "Wikipedia example data", kd1, P(9, 2))

    N = 400000
    t0 = clock()
    kd2 = KdTree(random_points(3, N), Orthotope(P(0, 0, 0), P(1, 1, 1)))
    t1 = clock()
    text = lambda *parts: "".join(map(str, parts))
    show_nearest(2, text("k-d tree with ", N,
                         " random 3D points (generation time: ",
                         t1-t0, "s)"),
                 kd2, random_point(3))
```

{{out}}

```txt
Wikipedia example data:
Point:            [9, 2]
Nearest neighbor: [8, 1]
Distance:         1.41421356237
Nodes visited:    3

k-d tree with 400000 random 3D points (generation time: 14.8755565302s):
Point:            [0.066694022911324868, 0.13692213852082813, 0.94939167224227283]
Nearest neighbor: [0.067027753280507252, 0.14407354836507069, 0.94543775920177597]
Distance:         0.00817847583914
Nodes visited:    33
```



## Racket

The following code is optimized for readability.

```racket

#lang racket

; A tree consists of a point, a left and a right subtree.
(struct tree (p l r) #:transparent)
; If the node is in depth d, then the points in l has
; the (d mod k)'th coordinate less than the same coordinate in p.

(define (kdtree d k ps)
  (cond [(empty? ps) #f] ; #f represents an empty subtree
        [else (define-values (p l r) (split-points ps (modulo d k)))
              (tree p (kdtree (+ d 1) k l) (kdtree (+ d 1) k r))]))

(define (split-points ps d)
  (define (ref p) (vector-ref p d))
  (define sorted-ps (sort ps < #:key ref))
  (define mid (quotient (+ (length ps)) 2))
  (define median (ref (list-ref sorted-ps mid)))
  (define-values (l r) (partition(λ(x)(< (ref x) median))sorted-ps))
  (values (first r) l (rest r)))

; The bounding box of a subtree:
(struct bb (mins maxs) #:transparent)

(define (infinite-bb k) 
  (bb (make-vector k -inf.0) (make-vector k +inf.0)))

(define/match (copy-bb h)
  [((bb mins maxs)) 
   (bb (vector-copy mins) (vector-copy maxs))])

(define (dist v w) (for/sum ([x v] [y w]) (sqr (- x y))))
(define (intersects? g r hr) (<= (dist (closest-in-hr g hr) g) r))
(define (closest-in-hr g hr)
  (for/vector ([gi g] [mini (bb-mins hr)] [maxi (bb-maxs hr)])
    (cond [(<=     gi mini) mini]
          [(< mini gi maxi) gi]
          [else             maxi])))

(define (split-bb hr d x)
  (define left  (copy-bb hr))
  (define right (copy-bb hr))
  (vector-set! (bb-maxs left) d x)
  (vector-set! (bb-mins right) d x)
  (values left right))

(define visits 0) ; for statistics only
(define (visit) (set! visits (+ visits 1)))
(define (reset-visits) (set! visits 0))
(define (regret-visit) (set! visits (- visits 1)))

(define (nearest-neighbor g t k)
  (define (nearer? p q) (< (dist p g) (dist q g)))
  (define (nearest p q) (if (nearer? p q) p q))
  (define (nn d t bb) (visit)
    (define (ref p) (vector-ref p (modulo d k)))
    (match t
      [#f (regret-visit) #(+inf.0 +inf.0 +inf.0)]
      [(tree p l r)
       (define-values (lbb rbb) (split-bb bb (modulo d k) (ref p)))
       (define-values (near near-bb far far-bb)
         (if (< (ref g) (ref p))
             (values l lbb r rbb)
             (values r rbb l lbb)))
       (define n (nearest p (nn (+ d 1) near near-bb)))
       (if (intersects? g (dist n g) far-bb)
           (nearest n (nn (+ d 1) far far-bb))
           n)]))
  (nn 0 t (infinite-bb k)))

```

Tests:

```racket

(define (wikipedia-test)
  (define t (kdtree 0 2 '(#(2 3) #(5 4) #(9 6) #(4 7) #(8 1) #(7 2))))
  (reset-visits)
  (define n (nearest-neighbor #(9 2) t 2))
  (displayln "Wikipedia Test")
  (displayln (~a "Nearest neighbour to (9,2) is: " n))
  (displayln (~a "Distance: " (dist n #(9 2))))
  (displayln (~a "Visits: " visits "\n")))

(define (test k n)
  (define (random!) (for/vector ([_ k]) (random)))
  (define points (for/list ([_ n]) (random!)))
  (define t (kdtree 0 k points))
  (reset-visits)
  (define target (for/vector ([_ k]) 0.75))
  (define nb (nearest-neighbor target t k))
  (define nb-control (argmin (λ (p) (dist p target)) points))
  (displayln (~a n " points in R^3 test"))
  (displayln (~a "Nearest neighbour to " target " is: \n\t\t" nb))
  (displayln (~a "Control: \t" nb-control))
  (displayln (~a "Distance: \t" (dist nb target)))
  (displayln (~a "Control: \t" (dist nb-control target)))
  (displayln (~a "Visits: \t" visits)))

(wikipedia-test)
(test 3 1000)
(test 3 1000)

```

{{out}}

```racket

Wikipedia Test
Nearest neighbour to (9,2) is: #(8 1)
Distance: 2
Visits: 3

1000 points in R^3 test
Nearest neighbour to #(0.75 0.75 0.75) is: 
		#(0.8092534479975508 0.7507095851813429 0.7706494651024903)
Control: 	#(0.8092534479975508 0.7507095851813429 0.7706494651024903)
Distance: 	0.003937875019747008
Control: 	0.003937875019747008
Visits: 	83

1000 points in R^3 test
Nearest neighbour to #(0.75 0.75 0.75) is: 
		#(0.7775581478448806 0.7806612633582072 0.7396664367640902)
Control: 	#(0.7775581478448806 0.7806612633582072 0.7396664367640902)
Distance: 	0.0018063471125121851
Control: 	0.0018063471125121851
Visits: 	39

```


## Scala

This example works for sequences of Int, Double, etc, so it is non-minimal due to its type-safe Numeric parameterisation.

```Scala
object KDTree {
  import Numeric._

  // Task 1A. Build tree of KDNodes. Translated from Wikipedia.
  def apply[T](points: Seq[Seq[T]], depth: Int = 0)(implicit num: Numeric[T]): Option[KDNode[T]] = {
    val dim = points.headOption.map(_.size) getOrElse 0
    if (points.isEmpty || dim < 1) None
    else {
      val axis = depth % dim
      val sorted = points.sortBy(_(axis))
      val median = sorted(sorted.size / 2)(axis)
      val (left, right) = sorted.partition(v => num.lt(v(axis), median))
      Some(KDNode(right.head, apply(left, depth + 1), apply(right.tail, depth + 1), axis))
    }
  }

  // Task 1B. Find the nearest node in this subtree. Translated from Wikipedia.
  case class KDNode[T](value: Seq[T], left: Option[KDNode[T]], right: Option[KDNode[T]], axis: Int)(implicit num: Numeric[T]) {
    def nearest(to: Seq[T]): Nearest[T] = {
      val default = Nearest(value, to, Set(this))
      compare(to, value) match {
        case 0 => default // exact match
        case t =>
          lazy val bestL = left.map(_ nearest to).getOrElse(default)
          lazy val bestR = right.map(_ nearest to).getOrElse(default)
          val branch1 = if (t < 0) bestL else bestR
          val best = if (num.lt(branch1.distsq, default.distsq)) branch1 else default
          val splitDist = num.minus(to(axis), value(axis))
          if (num.lt(num.times(splitDist, splitDist), best.distsq)) {
            val branch2 = if (t < 0) bestR else bestL
            val visited = branch2.visited ++ best.visited + this
            if (num.lt(branch2.distsq, best.distsq))
              branch2.copy(visited = visited)
              else best.copy(visited = visited)
          } else best.copy(visited = best.visited + this)
      }
    }
  }

  // Keep track of nodes visited, as per task. Pretty-printable.
  case class Nearest[T](value: Seq[T], to: Seq[T], visited: Set[KDNode[T]] = Set[KDNode[T]]())(implicit num: Numeric[T]) {
    lazy val distsq = KDTree.distsq(value, to)
    override def toString = f"Searched for=${to} found=${value} distance=${math.sqrt(num.toDouble(distsq))}%.4f visited=${visited.size}"
  }

  // Numeric utilities
  def distsq[T](a: Seq[T], b: Seq[T])(implicit num: Numeric[T]) =
    a.zip(b).map(c => num.times(num.minus(c._1, c._2), num.minus(c._1, c._2))).sum
  def compare[T](a: Seq[T], b: Seq[T])(implicit num: Numeric[T]): Int =
    a.zip(b).find(c => num.compare(c._1, c._2) != 0).map(c => num.compare(c._1, c._2)).getOrElse(0)
}
```

Task test:

```Scala
object KDTreeTest extends App {
  def test[T](haystack: Seq[Seq[T]], needles: Seq[T]*)(implicit num: Numeric[T]) = {
    println
    val tree = KDTree(haystack)
    if (haystack.size < 20) tree.foreach(println)
    for (kd <- tree; needle <- needles; nearest = kd nearest needle) {
      println(nearest)
      // Brute force proof
      val better = haystack
        .map(KDTree.Nearest(_, needle))
        .filter(n => num.lt(n.distsq, nearest.distsq))
        .sortBy(_.distsq)
      assert(better.isEmpty, s"Found ${better.size} closer than ${nearest.value} e.g. ${better.head}")
    }
  }

  // Results 1
  val wikitest = List(List(2,3), List(5,4), List(9,6), List(4,7), List(8,1), List(7,2))
  test(wikitest, List(9,2))

  // Results 2 (1000 points uniformly distributed in 3-d cube coordinates, sides 2 to 20)
  val uniform = for(x <- 1 to 10; y <- 1 to 10; z <- 1 to 10) yield List(x*2, y*2, z*2)
  assume(uniform.size == 1000)
  test(uniform, List(0, 0, 0), List(2, 2, 20), List(9, 10, 11))

  // Results 3 (1000 points randomly distributed in 3-d cube coordinates, sides -1.0 to 1.0)
  scala.util.Random.setSeed(0)
  def random(n: Int) = (1 to n).map(_ => (scala.util.Random.nextDouble - 0.5)* 2)
  test((1 to 1000).map(_ => random(3)), random(3))

  // Results 4 (27 points uniformly distributed in 3-d cube coordinates, sides 3...9)
  val small = for(x <- 1 to 3; y <- 1 to 3; z <- 1 to 3) yield List(x*3, y*3, z*3)
  assume(small.size == 27)
  test(small, List(0, 0, 0), List(4, 5, 6))
}
```

{{out}}

```txt
KDNode(List(7, 2),Some(KDNode(List(5, 4),Some(KDNode(List(2, 3),None,None,0)),Some(KDNode(List(4, 7),None,None,0)),1)),Some(KDNode(List(9, 6),Some(KDNode(List(8, 1),None,None,0)),None,1)),0)
Searched for=List(9, 2) found=List(8, 1) distance=1.4142 visited=3

Searched for=List(0, 0, 0) found=List(2, 2, 2) distance=3.4641 visited=10
Searched for=List(2, 2, 20) found=List(2, 2, 20) distance=0.0000 visited=9
Searched for=List(9, 10, 11) found=List(8, 10, 12) distance=1.4142 visited=134

Searched for=Vector(0.19269603520919643, -0.25958512078298535, -0.2572864045762784) found=Vector(0.07811099409527977, -0.2477618820196814, -0.20252227622550611) distance=0.1275 visited=25

Searched for=List(0, 0, 0) found=List(3, 3, 3) distance=5.1962 visited=4
Searched for=List(4, 5, 6) found=List(3, 6, 6) distance=1.4142 visited=6
```



## Sidef

{{trans|Perl 6}}

```ruby
struct Kd_node {
    d,
    split,
    left,
    right,
}

struct Orthotope {
    min,
    max,
}

class Kd_tree(n, bounds) {

    method init {
        n = self.nk2(0, n);
    }

    method nk2(split, e) {
        return(nil) if (e.len <= 0);
        var exset = e.sort_by { _[split] }
        var m = (exset.len // 2);
        var d = exset[m];
        while ((m+1 < exset.len) && (exset[m+1][split] == d[split])) {
            ++m;
        }

        var s2 = ((split + 1) % d.len);     # cycle coordinates
        Kd_node(d: d, split: split,
                left:  self.nk2(s2, exset.first(m)),
                right: self.nk2(s2, exset.last(m-1)));
    }
}

struct T3 {
    nearest,
    dist_sqd = Inf,
    nodes_visited = 0,
}

func find_nearest(k, t, p) {
    func nn(kd, target, hr, max_dist_sqd) {
        kd || return T3(nearest: [0]*k);

        var nodes_visited = 1;
        var s = kd.split;
        var pivot = kd.d;
        var left_hr = Orthotope(hr.min, hr.max);
        var right_hr = Orthotope(hr.min, hr.max);
        left_hr.max[s] = pivot[s];
        right_hr.min[s] = pivot[s];

        var nearer_kd;
        var further_kd;
        var nearer_hr;
        var further_hr;
        if (target[s] <= pivot[s]) {
            (nearer_kd, nearer_hr) = (kd.left, left_hr);
            (further_kd, further_hr) = (kd.right, right_hr);
        }
        else {
            (nearer_kd, nearer_hr) = (kd.right, right_hr);
            (further_kd, further_hr) = (kd.left, left_hr);
        }

        var n1 = nn(nearer_kd, target, nearer_hr, max_dist_sqd);
        var nearest = n1.nearest;
        var dist_sqd = n1.dist_sqd;
        nodes_visited += n1.nodes_visited;

        if (dist_sqd < max_dist_sqd) {
            max_dist_sqd = dist_sqd;
        }
        var d = (pivot[s] - target[s] -> sqr);
        if (d > max_dist_sqd) {
            return T3(nearest: nearest, dist_sqd: dist_sqd, nodes_visited: nodes_visited);
        }
        d = (pivot ~Z- target »sqr»() «+»);
        if (d < dist_sqd) {
            nearest = pivot;
            dist_sqd = d;
            max_dist_sqd = dist_sqd;
        }

        var n2 = nn(further_kd, target, further_hr, max_dist_sqd);
        nodes_visited += n2.nodes_visited;
        if (n2.dist_sqd < dist_sqd) {
            nearest = n2.nearest;
            dist_sqd = n2.dist_sqd;
        }

        T3(nearest: nearest, dist_sqd: dist_sqd, nodes_visited: nodes_visited);
    }

    return nn(t.n, p, t.bounds, Inf);
}

func show_nearest(k, heading, kd, p) {
    print <<-"END"
        #{heading}:
        Point:            [#{p.join(',')}]
        END
    var n = find_nearest(k, kd, p);
    print <<-"END"
        Nearest neighbor: [#{n.nearest.join(',')}]
        Distance:         #{sqrt(n.dist_sqd)}
        Nodes visited:    #{n.nodes_visited()}

        END
}

func random_point(k) { k.of { 1.rand } }
func random_points(k, n) { n.of { random_point(k) } }

var kd1 = Kd_tree([[2, 3],[5, 4],[9, 6],[4, 7],[8, 1],[7, 2]],
              Orthotope(min: [0, 0], max: [10, 10]));
show_nearest(2, "Wikipedia example data", kd1, [9, 2]);

var N = 1000
var t0 = Time.micro
var kd2 = Kd_tree(random_points(3, N), Orthotope(min: [0,0,0], max: [1,1,1]))

var t1 = Time.micro
show_nearest(2,
    "k-d tree with #{N} random 3D points (generation time: #{t1 - t0}s)",
     kd2, random_point(3))
```

{{out}}

```txt

Wikipedia example data:
Point:            [9,2]
Nearest neighbor: [8,1]
Distance:         1.41421356237309504880168872420969807856967187537695
Nodes visited:    3

k-d tree with 1000 random 3D points (generation time: 0.25858s):
Point:            [0.28961099389483532140941908632585463245703951185091,0.53383735570157521548169561846919925542828568155241,0.06543742264584889854604603500785549847809104064543]
Nearest neighbor: [0.28344130897803248636407083474733485442276225490765,0.54224255944924304382130637584184680424540291089424,0.12494797195998536910186918458377312162395340543897]
Distance:         0.06041703353924870133411659885818573345011076349573
Nodes visited:    30

```



## Tcl

{{trans|Python}}
{{libheader|TclOO}}

```tcl
package require TclOO

oo::class create KDTree {
    variable t dim
    constructor {points} {
	set t [my Build 0 $points 0 end]
	set dim [llength [lindex $points 0]]
    }
    method Build {split exset from to} {
	set exset [lsort -index $split -real [lrange $exset $from $to]]
	if {![llength $exset]} {return 0}
	set m [expr {[llength $exset] / 2}]
	set d [lindex $exset $m]
	while {[set mm $m;incr mm] < [llength $exset] && \
		[lindex $exset $mm $split] == [lindex $d $split]} {
	    set m $mm
	}
	set s [expr {($split + 1) % [llength $d]}]
	list 1 $d $split [my Build $s $exset 0 [expr {$m-1}]] \
		[my Build $s $exset [expr {$m+1}] end]
    }

    method findNearest {p} {
	lassign [my FN $t $p inf] p d2 count
	return [list $p [expr {sqrt($d2)}] $count]
    }
    method FN {kd target maxDist2} {
	if {[lindex $kd 0] == 0} {
	    return [list [lrepeat $dim 0.0] inf 0]
	}

	set nodesVisited 1
	lassign $kd -> pivot s

	if {[lindex $target $s] <= [lindex $pivot $s]} {
	    set nearerKD [lindex $kd 3]
	    set furtherKD [lindex $kd 4]
	} else {
	    set nearerKD [lindex $kd 4]
	    set furtherKD [lindex $kd 3]
	}

	lassign [my FN $nearerKD $target $maxDist2] nearest dist2 count
	incr nodesVisited $count

	if {$dist2 < $maxDist2} {
	    set maxDist2 $dist2
	}
	set d2 [expr {([lindex $pivot $s]-[lindex $target $s])**2}]
	if {$d2 > $maxDist2} {
	    return [list $nearest $dist2 $nodesVisited]
	}
	set d2 0.0
	foreach pp $pivot tp $target {set d2 [expr {$d2+($pp-$tp)**2}]}
	if {$d2 < $dist2} {
	    set nearest $pivot
	    set maxDist2 [set dist2 $d2]
	}

	lassign [my FN $furtherKD $target $maxDist2] fNearest fDist2 count
	incr nodesVisited $count
	if {$fDist2 < $dist2} {
	    set nearest $fNearest
	    set dist2 $fDist2
	}

	return [list $nearest $dist2 $nodesVisited]
    }
}
```

Demonstration code:

```tcl
proc showNearest {heading tree point} {
    puts ${heading}:
    puts "Point:            \[[join $point ,]\]"
    lassign [$tree findNearest $point] nearest distance count
    puts "Nearest neighbor: \[[join $nearest ,]\]"
    puts "Distance:         $distance"
    puts "Nodes visited:    $count"
}
proc randomPoint k {
    for {set j 0} {$j < $k} {incr j} {lappend p [::tcl::mathfunc::rand]}
    return $p
}
proc randomPoints {k n} {
    for {set i 0} {$i < $n} {incr i} {
	set p {}
	for {set j 0} {$j < $k} {incr j} {
	    lappend p [::tcl::mathfunc::rand]
	}
	lappend ps $p
    }
    return $ps
}

KDTree create kd1 {{2 3} {5 4} {9 6} {4 7} {8 1} {7 2}}
showNearest "Wikipedia example data" kd1 {9 2}
puts ""

set N 1000
set t [time {KDTree create kd2 [randomPoints 3 $N]}]
showNearest "k-d tree with $N random 3D points (generation time: [lindex $t 0] us)" kd2 [randomPoint 3]
kd2 destroy
puts ""

set N 1000000
set t [time {KDTree create kd2 [randomPoints 3 $N]}]
showNearest "k-d tree with $N random 3D points (generation time: [lindex $t 0] us)" kd2 [randomPoint 3]
puts "Search time:      [time {kd2 findNearest [randomPoint 3]} 10000]"
```

{{out}}

```txt

Wikipedia example data:
Point:            [9,2]
Nearest neighbor: [8,1]
Distance:         1.4142135623730951
Nodes visited:    3

k-d tree with 1000 random 3D points (generation time: 11908 us):
Point:            [0.8480196329057308,0.6659702466176685,0.961934903153188]
Nearest neighbor: [0.8774737389187672,0.7011300077201472,0.8920397525150514]
Distance:         0.0836007490668497
Nodes visited:    29

k-d tree with 1000000 random 3D points (generation time: 19643366 us):
Point:            [0.10923849936073576,0.9714587558859301,0.30731017482807405]
Nearest neighbor: [0.10596616664247875,0.9733627601402638,0.3079096774141815]
Distance:         0.0038331184393709545
Nodes visited:    22
Search time:      289.894755 microseconds per iteration

```



## zkl

{{trans|Python}}

```zkl
class KdTree{
   const NEAREST=0, DIST_SQD=1, NODES_VISITED=2;
   class KdNode{
      fcn init(_dom_elt,_split,_left,_right){
	 var dom_elt=_dom_elt.apply("toFloat"), 
	     split=_split, left=_left, right=_right;
      }
   }
   fcn init(pts,_bounds){  // pts is points is ( (x,y,..),..)
      var n=fcn(split, exset){
	   if(not exset) return(Void);
	   exset=exset.copy()  // mutable lists sort much quicker than read only
		      .sort('wrap(abc,xyz){ abc[split]<xyz[split] });
	   m,d:=exset.len()/2, exset[m];
	   while(m+1<exset.len() and exset[m + 1][split]==d[split]){ m+=1 }
	   s2:=(split+1)%d.len();  # cycle coordinates
	   KdNode(d,split,self.fcn(s2,exset[0,m]),
			  self.fcn(s2,exset[m+1,*]))
         }(0,pts);
      var bounds=_bounds;
   }
   fcn findNearest(k,p){
      fcn(node,target,hr,max_dist_sqd,k){
	 if(not node) return(k.pump(List,0.0), (0.0).MAX, 0); // fake node far away
	 nodes_visited,s,pivot:=1, node.split, node.dom_elt;
	 left_hr,right_hr:=hr.copy(True), hr.copy(True);  // deep-ish copy
	 left_hr.max[s]=right_hr.min[s]=pivot[s];

	 reg nearer_node,  nearer_hr, further_node, further_hr;
	 if(target[s]<=pivot[s]){
	    nearer_node,  nearer_hr  = node.left,  left_hr;
	    further_node, further_hr = node.right, right_hr;
	 }else{
	    nearer_node,  nearer_hr  = node.right, right_hr;
	    further_node, further_hr = node.left,  left_hr;
	 }
	 n1:=self.fcn(nearer_node, target, nearer_hr, max_dist_sqd, k);
	 nearest,dist_sqd:=n1;   // n1 is ( (a,b),distance,#visited ) 
	 nodes_visited+=n1[NODES_VISITED];

	 if(dist_sqd<max_dist_sqd) max_dist_sqd=dist_sqd;
	 d:=(pivot[s] - target[s]).pow(2);
	 if(d>max_dist_sqd) return(nearest, dist_sqd, nodes_visited);
	 d:=sqd(pivot,target);
	 if(d<dist_sqd) nearest,dist_sqd,max_dist_sqd=pivot,d,dist_sqd;
	 n2:=self.fcn(further_node, target, further_hr, max_dist_sqd,k);
	 nodes_visited+=n2[NODES_VISITED];
	 if(n2[DIST_SQD]<dist_sqd) nearest,dist_sqd=n2;
	 return(nearest, dist_sqd, nodes_visited)
      }(n, p, bounds, (0.0).MAX,k)
   }
   fcn [private] sqd(p1,p2){ // point deltas squared and summed
      p1.zipWith(fcn(a,b){ (a-b).pow(2) }, p2).sum(0.0) 
   }
   fcn show_nearest(k, heading, p){
      println(heading,":");
      println("Point:           ",p);
      n:=findNearest(k,p);
      println("Nearest neighbor:", n[NEAREST]);
      println("Distance:        ", n[DIST_SQD].sqrt());
      println("Nodes visited:   ", n[NODES_VISITED], "\n");
   }
}
```


```zkl
class Orthotope{ fcn init(mi,ma){ var min=mi,max=ma; } }
 
kd1:=KdTree(T(T(2,3), T(5,4), T(9,6), T(4,7), T(8,1), T(7,2)),
            Orthotope(List(0,0), List(10,10)));
kd1.show_nearest(2, "Wikipedia example data", T(9,2));
```

{{out}}

```txt

Wikipedia example data:
Point:           L(9,2)
Nearest neighbor:L(8,1)
Distance:        1.41421
Nodes visited:   3

```


```zkl
fcn randomPoint(k){ k.pump(List,(0.0).random(1)) }  // ( (p1,p2,,pk), ... n of them), p in [0..1]
fcn randomPoints(k,n){  // ( (p1,p2,,pk), ... n of them), p in [0..1]
   n.pump(List,randomPoint.fp(k))
}

N:=40000;
kd2:=KdTree(randomPoints(3,N), Orthotope(L(0,0,0), L(1,1,1)));
kd2.show_nearest(2, String("k-d tree with ", N," random 3D points"),
	         randomPoint(3));
```

{{out}}

```txt

k-d tree with 40000 random 3D points:
Point:           L(0.186009,0.186009,0.186009)
Nearest neighbor:L(0.186012,0.186012,0.186012)
Distance:        3.91942e-06
Nodes visited:   16

```

