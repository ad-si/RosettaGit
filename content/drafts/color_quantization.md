+++
title = "Color quantization"
description = ""
date = 2019-10-14T17:20:49Z
aliases = []
[extra]
id = 10293
[taxonomies]
categories = []
tags = []
+++

{{task|Raster graphics operations}}

[[file:quantum_frog.png|full color|thumb|left|192px]][[file:quantum_frog_16.png|Example: Gimp 16 color|thumb|left|192px]]
[[wp:Color_quantization|Color quantization]] is the process of reducing number of colors used in an image while trying to maintain the visual appearance of the original image.  In general, it is a form of [[wp:Cluster_analysis|cluster analysis]], if each RGB color value is considered as a coordinate triple in the 3D colorspace.  There are some well know algorithms [http://web.cs.wpi.edu/~matt/courses/cs563/talks/color_quant/CQindex.html], each with its own advantages and drawbacks.

'''Task''': Take an RGB color image and reduce its colors to some smaller number (< 256).  For this task, use the frog as input and reduce colors to 16, and output the resulting colors.  The chosen colors should be adaptive to the input image, meaning you should ''not'' use a fixed palette such as Web colors or Windows system palette.  Dithering is not required.

Note: the funny color bar on top of the frog image is intentional.
<br clear=left>

## C

[[file:quantum_frog_C.png|C output|thumb|200px]]
Using an octree to store colors. Here are only the relevant parts.  For full C code see [[Color_quantization/C]]. It's different from the standard octree method in that:
# Each node can both be leaf node and have child nodes;
# Leaf nodes are not folded until all pixels are in.  This removes the possibility of early pixels completely bias the tree.  And child nodes are reduced one at a time instead of typical all or nothing approach.
# Node folding priorities are tracked by a binary heap instead of typical linked list.
The output image is better at preserving textures of the original than Gimp, though it obviously depends on the input image.  This particular frog image has the color bar added at the top specifically to throw off some early truncation algorithms, which Gimp is suseptible to.

```c
typedef struct oct_node_t oct_node_t, *oct_node;
struct oct_node_t{
	/* sum of all colors represented by this node. 64 bit in case of HUGE image */
	uint64_t r, g, b;
	int count, heap_idx;
	oct_node kids[8], parent;
	unsigned char n_kids, kid_idx, flags, depth;
};

/* cmp function that decides the ordering in the heap.  This is how we determine
   which octree node to fold next, the heart of the algorithm. */
inline int cmp_node(oct_node a, oct_node b)
{
	if (a->n_kids < b->n_kids) return -1;
	if (a->n_kids > b->n_kids) return 1;

	int ac = a->count * (1 + a->kid_idx) >> a->depth;
	int bc = b->count * (1 + b->kid_idx) >> b->depth;
	return ac < bc ? -1 : ac > bc;
}

/* adding a color triple to octree */
oct_node node_insert(oct_node root, unsigned char *pix)
{
#	define OCT_DEPTH 8
	/* 8: number of significant bits used for tree.  It's probably good enough
	   for most images to use a value of 5.  This affects how many nodes eventually
	   end up in the tree and heap, thus smaller values helps with both speed
	   and memory. */

	unsigned char i, bit, depth = 0;
	for (bit = 1 << 7; ++depth < OCT_DEPTH; bit >>= 1) {
		i = !!(pix[1] & bit) * 4 + !!(pix[0] & bit) * 2 + !!(pix[2] & bit);
		if (!root->kids[i])
			root->kids[i] = node_new(i, depth, root);

		root = root->kids[i];
	}

	root->r += pix[0];
	root->g += pix[1];
	root->b += pix[2];
	root->count++;
	return root;
}

/* remove a node in octree and add its count and colors to parent node. */
oct_node node_fold(oct_node p)
{
	if (p->n_kids) abort();
	oct_node q = p->parent;
	q->count += p->count;

	q->r += p->r;
	q->g += p->g;
	q->b += p->b;
	q->n_kids --;
	q->kids[p->kid_idx] = 0;
	return q;
}

/* traverse the octree just like construction, but this time we replace the pixel
   color with color stored in the tree node */
void color_replace(oct_node root, unsigned char *pix)
{
	unsigned char i, bit;

	for (bit = 1 << 7; bit; bit >>= 1) {
		i = !!(pix[1] & bit) * 4 + !!(pix[0] & bit) * 2 + !!(pix[2] & bit);
		if (!root->kids[i]) break;
		root = root->kids[i];
	}

	pix[0] = root->r;
	pix[1] = root->g;
	pix[2] = root->b;
}

/* Building an octree and keep leaf nodes in a bin heap.  Afterwards remove first node
   in heap and fold it into its parent node (which may now be added to heap), until heap
   contains required number of colors. */
void color_quant(image im, int n_colors)
{
	int i;
	unsigned char *pix = im->pix;
	node_heap heap = { 0, 0, 0 };

	oct_node root = node_new(0, 0, 0), got;
	for (i = 0; i < im->w * im->h; i++, pix += 3)
		heap_add(&heap, node_insert(root, pix));

	while (heap.n > n_colors + 1)
		heap_add(&heap, node_fold(pop_heap(&heap)));

	double c;
	for (i = 1; i < heap.n; i++) {
		got = heap.buf[i];
		c = got->count;
		got->r = got->r / c + .5;
		got->g = got->g / c + .5;
		got->b = got->b / c + .5;
		printf("%2d | %3llu %3llu %3llu (%d pixels)\n",
			i, got->r, got->g, got->b, got->count);
	}

	for (i = 0, pix = im->pix; i < im->w * im->h; i++, pix += 3)
		color_replace(root, pix);

	node_free();
	free(heap.buf);
}
```




## Common Lisp

{{libheader|opticl}}
Use median cut.

```lisp
(defpackage #:quantize
  (:use #:cl
        #:opticl))

(in-package #:quantize)

(defun image->pixels (image)
  (check-type image 8-bit-rgb-image)
  (let (pixels)
    (do-pixels (y x) image
      (push (pixel* image y x) pixels))
    pixels))

(defun greatest-color-range (pixels)
  (loop for (r g b) in pixels
        minimize r into r-min
        minimize g into g-min
        minimize b into b-min
        maximize r into r-max
        maximize g into g-max
        maximize b into b-max
        finally
           (return (let* ((r-range (- r-max r-min))
                          (g-range (- g-max g-min))
                          (b-range (- b-max b-min))
                          (max-range (max r-range g-range b-range)))
                     (cond ((= r-range max-range) 0)
                           ((= g-range max-range) 1)
                           (t                     2))))))

(defun median-cut (pixels target-num-colors)
  (assert (zerop (mod (log target-num-colors 2) 1)))
  (if (or (= target-num-colors 1) (null (rest pixels)))
      (list pixels)
      (let* ((channel (greatest-color-range pixels))
             (sorted (sort pixels #'< :key (lambda (pixel) (nth channel pixel))))
             (half (floor (length sorted) 2))
             (next-target (/ target-num-colors 2)))
        (nconc (median-cut (subseq sorted 0 half) next-target)
               (median-cut (subseq sorted half) next-target)))))

(defun quantize-colors (pixels target-num-colors)
  (let ((color-map (make-hash-table :test #'equal)))
    (dolist (bucket (median-cut pixels target-num-colors) color-map)
      (loop for (r g b) in bucket
            sum r into r-sum
            sum g into g-sum
            sum b into b-sum
            count t into num-pixels
            finally (let ((average (list (round r-sum num-pixels)
                                         (round g-sum num-pixels)
                                         (round b-sum num-pixels))))
                      (dolist (pixel bucket)
                        (setf (gethash pixel color-map) average)))))))

(defun quantize-image (input-file output-file target-num-colors)
  (let* ((image (read-png-file input-file))
         (pixels (image->pixels image))
         (color-map (quantize-colors pixels target-num-colors))
         (result-image (with-image-bounds (height width) image
                         (make-8-bit-rgb-image height width :initial-element 0))))
    (set-pixels (y x) result-image
      (let* ((original (multiple-value-list (pixel image y x)))
             (quantized (gethash original color-map)))
        (values-list quantized)))
    (write-png-file output-file result-image)))
```



## D


### Functional Version

{{trans|OCaml}}
This code retains the style of the original OCaML code, and uses the bitmap module from the Bitmap Task.

```d
import core.stdc.stdio, std.stdio, std.algorithm, std.typecons,
       std.math, std.range, std.conv, std.string, bitmap;

struct Col { float r, g, b; }
alias Cluster = Tuple!(Col, float, Col, Col[]);
enum Axis { R, G, B }

enum round = (in float x) pure nothrow @safe @nogc => cast(int)floor(x + 0.5);

enum roundRGB = (in Col c) pure nothrow @safe @nogc =>
    RGB(cast(ubyte)round(c.r),
        cast(ubyte)round(c.g),
        cast(ubyte)round(c.b));

enum addRGB = (in Col c1, in Col c2) pure nothrow @safe @nogc =>
    Col(c1.r + c2.r, c1.g + c2.g, c1.b + c2.b);

Col meanRGB(in Col[] pxList) pure nothrow @safe @nogc {
    immutable tot = reduce!addRGB(Col(0, 0, 0), pxList);
    immutable n = pxList.length;
    return Col(tot.r / n, tot.g / n, tot.b / n);
}

enum minC = (in Col c1, in Col c2) pure nothrow @safe @nogc =>
    Col(min(c1.r, c2.r), min(c1.g, c2.g), min(c1.b, c2.b));

enum maxC = (in Col c1, in Col c2) pure nothrow @safe @nogc =>
    Col(max(c1.r, c2.r), max(c1.g, c2.g), max(c1.b, c2.b));

Tuple!(Col, Col) extrems(in Col[] lst) pure nothrow @safe @nogc {
    enum FI = float.infinity;
    auto mmRGB = typeof(return)(Col(FI, FI, FI), Col(-FI, -FI, -FI));
    return reduce!(minC, maxC)(mmRGB, lst);
}

Tuple!(float, Col) volumeAndDims(in Col[] lst) pure nothrow @safe @nogc {
    immutable e = lst.extrems;
    immutable r = Col(e[1].r - e[0].r,
                      e[1].g - e[0].g,
                      e[1].b - e[0].b);
    return typeof(return)(r.r * r.g * r.b, r);
}

Cluster makeCluster(Col[] pixelList) pure nothrow @safe @nogc {
    immutable vol_dims = pixelList.volumeAndDims;
    immutable int len = pixelList.length;
    return typeof(return)(pixelList.meanRGB,
                          len * vol_dims[0],
                          vol_dims[1],
                          pixelList);
}

enum fCmp = (in float a, in float b) pure nothrow @safe @nogc =>
    (a > b) ? 1 : (a < b ? -1 : 0);

Axis largestAxis(in Col c) pure nothrow @safe @nogc {
    immutable int r1 = fCmp(c.r, c.g);
    immutable int r2 = fCmp(c.r, c.b);
    if (r1 ==  1 && r2 ==  1) return Axis.R;
    if (r1 == -1 && r2 ==  1) return Axis.G;
    if (r1 ==  1 && r2 == -1) return Axis.B;
    return (fCmp(c.g, c.b) == 1) ? Axis.G : Axis.B;
}

Tuple!(Cluster, Cluster) subdivide(in Col c, in float nVolProd,
                                   in Col vol, Col[] pixels)
pure nothrow @safe @nogc {
    Col[] px2;
    final switch (largestAxis(vol)) {
        case Axis.R: px2 = pixels.partition!(c1 => c1.r < c.r); break;
        case Axis.G: px2 = pixels.partition!(c1 => c1.g < c.g); break;
        case Axis.B: px2 = pixels.partition!(c1 => c1.b < c.b); break;
    }
    auto px1 = pixels[0 .. $ - px2.length];
    return typeof(return)(px1.makeCluster, px2.makeCluster);
}

uint RGB2uint(in RGB c) pure nothrow @safe @nogc {
    return c.r | (c.g << 8) | (c.b << 16);
}

enum uintToRGB = (in uint c) pure nothrow @safe @nogc =>
    RGB(c & 0xFF, (c >> 8) & 0xFF, (c >> 16) & 0xFF);

Image!RGB colorQuantize(in Image!RGB img, in int n) pure nothrow /*@safe*/ {
    immutable width = img.nx;
    immutable height = img.ny;

    auto cols = new Col[width * height];
    foreach (immutable i, ref c; img.image)
        cols[i] = Col(c.tupleof);

    immutable dumb = Col(0, 0, 0);
    Cluster unused = Cluster(dumb, -float.infinity, dumb, (Col[]).init);

    auto clusters = [cols.makeCluster];
    while (clusters.length < n) {
        // Cluster cl = clusters.reduce!(max!q{ a[1] })(unused);
        Cluster cl = reduce!((c1, c2) => c1[1] > c2[1] ? c1 : c2)
                            (unused, clusters);
        clusters = [cl[].subdivide[]] ~
            clusters.remove!(c => c == cl, SwapStrategy.unstable); //**
    }

    uint[uint] pixMap; // Faster than RGB[RGB].
    ubyte[4] u4a, u4b;
    foreach (const cluster; clusters) {
        immutable ubyteMean = cluster[0].roundRGB.RGB2uint;
        foreach (immutable col; cluster[3])
            pixMap[col.roundRGB.RGB2uint] = ubyteMean;
    }

    auto result = new Image!RGB;
    result.allocate(height, width);

    foreach (immutable i, immutable p; img.image) {
        immutable u3a = p.tupleof.RGB;
        result.image[i] = pixMap[RGB2uint(u3a)].uintToRGB;
    }

    return result;
}

void main(in string[] args) {
    string fileName;
    int nCols;
    switch (args.length) {
        case 1:
            fileName = "quantum_frog.ppm";
            nCols = 16;
            break;
        case 3:
            fileName = args[1];
            nCols = args[2].to!int;
            break;
        default:
            "Usage: color_quantization image.ppm ncolors".writeln;
            return;
    }

    auto im = new Image!RGB;
    im.loadPPM6(fileName);
    const imq = colorQuantize(im, nCols);
    imq.savePPM6("quantum_frog_quantized.ppm");
}
```



### Imperative Version

{{trans|C}}
This code retains part of the style of the original C code.

```d
import core.stdc.stdlib: malloc, calloc, realloc, free, abort;
import std.stdio: stderr, File;
import std.ascii: isWhite;
import std.math: abs;
import std.conv: to;
import std.string: split, strip;
import std.exception: enforce;
import std.array: empty;
import std.typetuple: TypeTuple;

enum ON_INHEAP = 1;

struct Image {
    uint w, h;
    ubyte[0] pix;
}

Image* imageNew(in uint w, in uint h) nothrow @nogc
in {
    assert(w > 0 && h > 0);
} out(result) {
    assert(result != null);
} body {
    auto im = cast(Image*)malloc(Image.sizeof + w * h * 3);
    im.w = w;
    im.h = h;
    return im;
}

Image* readPPM6(in string fileName)
in {
    assert(!fileName.empty);
} out(result) {
    assert(result != null);
} body {
    auto fIn = File(fileName, "rb");
    enforce(fIn.readln.strip == "P6");

    // Skip comments.
    string line;
    do {
        line = fIn.readln;
    } while (line.length && line[0] == '#');

    const size = line.split.to!(uint[]);
    enforce(size.length == 2);
    //immutable size = line.split.to!(uint[2]);
    auto img = imageNew(size[0], size[1]);
    enforce(fIn.readln.strip == "255");
    fIn.rawRead(img.pix.ptr[0 .. img.w * img.h * 3]);
    return img;
}

void writePPM6(in Image* img, in string fileName)
in {
    assert(!fileName.empty);
    assert(img != null);
} body {
    auto fOut = File(fileName, "wb");
    fOut.writefln("P6\n%d %d\n255", img.w, img.h);
    fOut.rawWrite(img.pix.ptr[0 .. img.w * img.h * 3]);
    fOut.close;
}

struct OctreeNode {
    long r, g, b; // Sum of all child node colors.
    uint count, heapIdx;
    ubyte nKids, kidIdx, flags, depth;
    OctreeNode*[8] kids;
    OctreeNode* parent;
}

struct HeapNode {
    uint alloc, n;
    OctreeNode** buf;
}

int cmpOctreeNode(in OctreeNode* a, in OctreeNode* b)
pure nothrow @safe @nogc
in {
    assert(a != null);
    assert(b != null);
} out(result) {
    assert(result == -1 || result == 0 || result == 1);
} body {
    if (a.nKids < b.nKids)
        return -1;
    if (a.nKids > b.nKids)
        return 1;

    immutable uint ac = a.count >> a.depth;
    immutable uint bc = b.count >> b.depth;
    return (ac < bc) ? -1 : (ac > bc);
}

void downHeap(HeapNode* h, OctreeNode* p) pure nothrow @nogc
in {
    assert(h != null);
    assert(p != null);
} body {
    auto n = p.heapIdx;

    while (true) {
        uint m = n * 2;
        if (m >= h.n)
            break;
        if (m + 1 < h.n && cmpOctreeNode(h.buf[m], h.buf[m + 1]) > 0)
            m++;

        if (cmpOctreeNode(p, h.buf[m]) <= 0)
            break;

        h.buf[n] = h.buf[m];
        h.buf[n].heapIdx = n;
        n = m;
    }

    h.buf[n] = p;
    p.heapIdx = n;
}

void upHeap(HeapNode* h, OctreeNode* p) pure nothrow @nogc
in {
    assert(h != null);
    assert(p != null);
} body {
    auto n = p.heapIdx;

    while (n > 1) {
        auto prev = h.buf[n / 2];
        if (cmpOctreeNode(p, prev) >= 0)
            break;

        h.buf[n] = prev;
        prev.heapIdx = n;
        n /= 2;
    }

    h.buf[n] = p;
    p.heapIdx = n;
}

void addHeap(HeapNode* h, OctreeNode* p) nothrow @nogc
in {
    assert(h != null);
    assert(p != null);
} body {
    if ((p.flags & ON_INHEAP)) {
        downHeap(h, p);
        upHeap(h, p);
        return;
    }

    p.flags |= ON_INHEAP;
    if (!h.n)
        h.n = 1;
    if (h.n >= h.alloc) {
        while (h.n >= h.alloc)
            h.alloc += 1024;
        h.buf = cast(OctreeNode**)realloc(h.buf, (OctreeNode*).sizeof * h.alloc);
        assert(h.buf != null);
    }

    p.heapIdx = h.n;
    h.buf[h.n++] = p;
    upHeap(h, p);
}

OctreeNode* popHeap(HeapNode* h) pure nothrow @nogc
in {
    assert(h != null);
} out(result) {
    assert(result != null);
} body {
    if (h.n <= 1)
        return null;

    auto ret = h.buf[1];
    h.buf[1] = h.buf[--h.n];

    h.buf[h.n] = null;

    h.buf[1].heapIdx = 1;
    downHeap(h, h.buf[1]);

    return ret;
}

OctreeNode* octreeNodeNew(in ubyte idx, in ubyte depth, OctreeNode* p,
                          ref OctreeNode[] pool) nothrow @nogc
out(result) {
    assert(result != null);
} body {
    __gshared static uint len = 0;

    if (len <= 1) {
        OctreeNode* p2 = cast(OctreeNode*)calloc(OctreeNode.sizeof, 2048);
        assert(p2 != null);
        p2.parent = pool.ptr;
        pool = p2[0 .. 2048];
        len = 2047;
    }

    OctreeNode* x = pool.ptr + len--;
    x.kidIdx = idx;
    x.depth = depth;
    x.parent = p;
    if (p)
        p.nKids++;
    return x;
}

void octreeNodeFree(ref OctreeNode[] pool) nothrow @nogc
out {
    assert(pool.empty);
} body {
    auto poolPtr = pool.ptr;

    while (poolPtr) {
        auto p = poolPtr.parent;
        free(poolPtr);
        poolPtr = p;
    }

    pool = null;
}

OctreeNode* octreeNodeInsert(OctreeNode* root, in ubyte* pix, ref OctreeNode[] pool)
nothrow @nogc
in {
    assert(root != null);
    assert(pix != null);
    assert(!pool.empty);
} out(result) {
    assert(result != null);
} body {
    ubyte depth = 0;

    for (ubyte bit = (1 << 7); ++depth < 8; bit >>= 1) {
        immutable ubyte i = !!(pix[1] & bit) * 4 +
                            !!(pix[0] & bit) * 2 +
                            !!(pix[2] & bit);
        if (!root.kids[i])
            root.kids[i] = octreeNodeNew(i, depth, root, pool);

        root = root.kids[i];
    }

    root.r += pix[0];
    root.g += pix[1];
    root.b += pix[2];
    root.count++;
    return root;
}

OctreeNode* octreeNodeFold(OctreeNode* p) nothrow @nogc
in {
    assert(p != null);
} out(result) {
    assert(result != null);
} body {
    if (p.nKids)
        abort();
    auto q = p.parent;
    q.count += p.count;

    q.r += p.r;
    q.g += p.g;
    q.b += p.b;
    q.nKids--;
    q.kids[p.kidIdx] = null;
    return q;
}

void colorReplace(OctreeNode* root, ubyte* pix) pure nothrow @nogc
in {
    assert(root != null);
    assert(pix != null);
} body {
    for (ubyte bit = (1 << 7); bit; bit >>= 1) {
        immutable i = !!(pix[1] & bit) * 4 +
                      !!(pix[0] & bit) * 2 +
                      !!(pix[2] & bit);
        if (!root.kids[i])
            break;
        root = root.kids[i];
    }

    pix[0] = cast(ubyte)root.r;
    pix[1] = cast(ubyte)root.g;
    pix[2] = cast(ubyte)root.b;
}

void errorDiffuse(Image* im, HeapNode* h) nothrow @nogc
in {
    assert(im != null);
    assert(h != null);
} body {
    OctreeNode* nearestColor(in int* v) nothrow @nogc
    in {
        assert(v != null);
    } out(result) {
        assert(result != null);
    } body {
        auto max = long.max;
        typeof(return) on = null;

        foreach (immutable uint i; 1 .. h.n) {
            immutable diff = 3 * abs(h.buf[i].r - v[0]) +
                             5 * abs(h.buf[i].g - v[1]) +
                             2 * abs(h.buf[i].b - v[2]);
            if (diff < max) {
                max = diff;
                on = h.buf[i];
            }
        }

        return on;
    }

    uint pos(in uint i, in uint j) nothrow @safe @nogc {
        return 3 * (i * im.w + j);
    }

    enum C10 = 7;
    enum C01 = 5;
    enum C11 = 2;
    enum C00 = 1;
    enum CTOTAL = C00 + C11 + C10 + C01;

    auto npx = cast(int*)calloc(int.sizeof, im.h * im.w * 3);
    assert(npx != null);
    auto pix = im.pix.ptr;
    alias triple = TypeTuple!(0, 1, 2);

    for (auto px = npx, i = 0u; i < im.h; i++) {
        for (uint j = 0; j < im.w; j++, pix += 3, px += 3) {
            /*static*/ foreach (immutable k; triple)
                px[k] = cast(int)pix[k] * CTOTAL;
        }
    }

    static void clamp(ref int x) pure nothrow @safe @nogc {
        if (x > 255) x = 255;
        if (x < 0)   x = 0;
    }

    pix = im.pix.ptr;

    for (auto px = npx, i = 0u; i < im.h; i++) {
        for (uint j = 0; j < im.w; j++, pix += 3, px += 3) {
            /*static*/ foreach (immutable k; triple)
                px[k] /= CTOTAL;
            /*static*/ foreach (immutable k; triple)
                clamp(px[k]);

            const nd = nearestColor(px);
            uint[3] v = void;
            v[0] = cast(uint)(px[0] - nd.r);
            v[1] = cast(uint)(px[1] - nd.g);
            v[2] = cast(uint)(px[2] - nd.b);

            pix[0] = cast(ubyte)nd.r;
            pix[1] = cast(ubyte)nd.g;
            pix[2] = cast(ubyte)nd.b;

            if (j < im.w - 1) {
                /*static*/ foreach (immutable k; triple)
                    npx[pos(i, j + 1) + k] += v[k] * C10;
            }

            if (i >= im.h - 1)
                continue;

            /*static*/ foreach (immutable k; triple)
                npx[pos(i + 1, j) + k] += v[k] * C01;

            if (j < im.w - 1) {
                /*static*/ foreach (immutable k; triple)
                    npx[pos(i + 1, j + 1) + k] += v[k] * C11;
            }

            if (j) {
                /*static*/ foreach (immutable k; triple)
                    npx[pos(i + 1, j - 1) + k] += v[k] * C00;
            }
        }
    }

    free(npx);
}

void colorQuant(Image* im, in uint nColors, in bool dither) nothrow @nogc
in {
    assert(im != null);
    assert(nColors > 1);
} body {
    auto pix = im.pix.ptr;
    HeapNode heap = { 0, 0, null };
    OctreeNode[] pool;

    auto root = octreeNodeNew(0, 0, null, pool);
    for (uint i = 0; i < im.w * im.h; i++, pix += 3)
        addHeap(&heap, octreeNodeInsert(root, pix, pool));

    while (heap.n > nColors + 1)
        addHeap(&heap, octreeNodeFold(popHeap(&heap)));

    foreach (immutable i; 1 .. heap.n) {
        auto got = heap.buf[i];
        immutable double c = got.count;
        got.r = cast(long)(got.r / c + 0.5);
        got.g = cast(long)(got.g / c + 0.5);
        got.b = cast(long)(got.b / c + 0.5);
    }

    if (dither)
        errorDiffuse(im, &heap);
    else {
        uint i;
        for (i = 0, pix = im.pix.ptr; i < im.w * im.h; i++, pix += 3)
            colorReplace(root, pix);
    }

    pool.octreeNodeFree;
    heap.buf.free;
}

int main(in string[] args) {
    if (args.length < 3 || args.length > 4) {
        stderr.writeln("Usage: quant ppmFile nColors [dith]");
        return 1;
    }

    immutable nColors = args[2].to!uint;
    assert(nColors > 1);

    auto im = readPPM6(args[1]);
    immutable useDithering = (args.length == 4) ? (args[3] == "dith") : false;
    immutable fileNameOut = useDithering ? "outd.ppm" : "out.ppm";

    colorQuant(im, nColors, useDithering);
    writePPM6(im, fileNameOut);

    im.free;
    return 0;
}
```

Compiled with ldc2, it runs on the quantum_frog image in about 0.20 seconds with dithering and about 0.10 seconds without dithering.


## Go

A very basic median cut algorithm, no dithering.

```go
package main

import (
    "container/heap"
    "image"
    "image/color"
    "image/png"
    "log"
    "math"
    "os"
    "sort"
)

func main() {
    f, err := os.Open("Quantum_frog.png")
    if err != nil {
        log.Fatal(err)
    }
    img, err := png.Decode(f)
    if ec := f.Close(); err != nil {
        log.Fatal(err)
    } else if ec != nil {
        log.Fatal(ec)
    }
    fq, err := os.Create("frog16.png")
    if err != nil {
        log.Fatal(err)
    }
    if err = png.Encode(fq, quant(img, 16)); err != nil {
        log.Fatal(err)
    }
}

// Organize quatization in some logical steps.
func quant(img image.Image, nq int) image.Image {
    qz := newQuantizer(img, nq) // set up a work space
    qz.cluster()                // cluster pixels by color
    return qz.Paletted()        // generate paletted image from clusters
}

// A workspace with members that can be accessed by methods.
type quantizer struct {
    img image.Image // original image
    cs  []cluster   // len is the desired number of colors
    px  []point     // list of all points in the image
    ch  chValues    // buffer for computing median
    eq  []point     // additional buffer used when splitting cluster
}

type cluster struct {
    px       []point // list of points in the cluster
    widestCh int     // rx, gx, bx const for channel with widest value range
    chRange  uint32  // value range (vmax-vmin) of widest channel
}

type point struct{ x, y int }
type chValues []uint32
type queue []*cluster

const (
    rx = iota
    gx
    bx
)

func newQuantizer(img image.Image, nq int) *quantizer {
    b := img.Bounds()
    npx := (b.Max.X - b.Min.X) * (b.Max.Y - b.Min.Y)
    // Create work space.
    qz := &quantizer{
        img: img,
        ch:  make(chValues, npx),
        cs:  make([]cluster, nq),
    }
    // Populate initial cluster with all pixels from image.
    c := &qz.cs[0]
    px := make([]point, npx)
    c.px = px
    i := 0
    for y := b.Min.Y; y < b.Max.Y; y++ {
        for x := b.Min.X; x < b.Max.X; x++ {
            px[i].x = x
            px[i].y = y
            i++
        }
    }
    return qz
}

func (qz *quantizer) cluster() {
    // Cluster by repeatedly splitting clusters.
    // Use a heap as priority queue for picking clusters to split.
    // The rule will be to spilt the cluster with the most pixels.
    // Terminate when the desired number of clusters has been populated
    // or when clusters cannot be further split.
    pq := new(queue)
    // Initial cluster.  populated at this point, but not analyzed.
    c := &qz.cs[0]
    for i := 1; ; {
        qz.setColorRange(c)
        // Cluster cannot be split if all pixels are the same color.
        // Only enqueue clusters that can be split.
        if c.chRange > 0 {
            heap.Push(pq, c) // add new cluster to queue
        }
        // If no clusters have any color variation, mark the end of the
        // cluster list and quit early.
        if len(*pq) == 0 {
            qz.cs = qz.cs[:i]
            break
        }
        s := heap.Pop(pq).(*cluster) // get cluster to split
        c = &qz.cs[i]                // set c to new cluster
        i++
        m := qz.Median(s)
        qz.Split(s, c, m) // split s into c and s
        // If that was the last cluster, we're done.
        if i == len(qz.cs) {
            break
        }
        qz.setColorRange(s)
        if s.chRange > 0 {
            heap.Push(pq, s) // return to queue
        }
    }
}
    
func (q *quantizer) setColorRange(c *cluster) {
    // Find extents of color values in each channel.
    var maxR, maxG, maxB uint32
    minR := uint32(math.MaxUint32)
    minG := uint32(math.MaxUint32)
    minB := uint32(math.MaxUint32) 
    for _, p := range c.px {
        r, g, b, _ := q.img.At(p.x, p.y).RGBA()
        if r < minR { 
            minR = r
        }
        if r > maxR {
            maxR = r
        }
        if g < minG {
            minG = g 
        }
        if g > maxG {
            maxG = g
        }
        if b < minB {
            minB = b
        }
        if b > maxB {
            maxB = b
        }
    }
    // See which channel had the widest range.
    s := gx
    min := minG
    max := maxG
    if maxR-minR > max-min {
        s = rx
        min = minR
        max = maxR
    }
    if maxB-minB > max-min {
        s = bx
        min = minB
        max = maxB
    }
    c.widestCh = s
    c.chRange = max - min // also store the range of that channel
}

func (q *quantizer) Median(c *cluster) uint32 {
    px := c.px
    ch := q.ch[:len(px)]
    // Copy values from appropriate channel to buffer for computing median.
    switch c.widestCh {
    case rx:
        for i, p := range c.px {
            ch[i], _, _, _ = q.img.At(p.x, p.y).RGBA()
        }
    case gx:
        for i, p := range c.px {
            _, ch[i], _, _ = q.img.At(p.x, p.y).RGBA()
        }
    case bx:
        for i, p := range c.px {
            _, _, ch[i], _ = q.img.At(p.x, p.y).RGBA()
        }
    }
    // Median algorithm.
    sort.Sort(ch)
    half := len(ch) / 2
    m := ch[half]
    if len(ch)%2 == 0 {
        m = (m + ch[half-1]) / 2
    }
    return m
}

func (q *quantizer) Split(s, c *cluster, m uint32) {
    px := s.px
    var v uint32
    i := 0
    lt := 0
    gt := len(px) - 1
    eq := q.eq[:0] // reuse any existing buffer
    for i <= gt {
        // Get pixel value of appropriate channel.
        r, g, b, _ := q.img.At(px[i].x, px[i].y).RGBA()
        switch s.widestCh {
        case rx:
            v = r
        case gx:
            v = g
        case bx:
            v = b
        } 
        // Categorize each pixel as either <, >, or == median.
        switch {
        case v < m:
            px[lt] = px[i]
            lt++
            i++
        case v > m:
            px[gt], px[i] = px[i], px[gt]
            gt--
        default:
            eq = append(eq, px[i])
            i++
        }
    }
    // Handle values equal to the median.
    if len(eq) > 0 {
        copy(px[lt:], eq) // move them back between the lt and gt values.
        // Then, if the number of gt values is < the number of lt values,
        // fix up i so that the split will include the eq values with
        // the gt values.
        if len(px)-i < lt {
            i = lt
        }
        q.eq = eq // squirrel away (possibly expanded) buffer for reuse
    }
    // Split the pixel list.
    s.px = px[:i]
    c.px = px[i:]
}   
    
func (qz *quantizer) Paletted() *image.Paletted {
    cp := make(color.Palette, len(qz.cs))
    pi := image.NewPaletted(qz.img.Bounds(), cp)
    for i := range qz.cs {
        px := qz.cs[i].px
        // Average values in cluster to get palette color.
        var rsum, gsum, bsum int64
        for _, p := range px {
            r, g, b, _ := qz.img.At(p.x, p.y).RGBA()
            rsum += int64(r)
            gsum += int64(g)
            bsum += int64(b)
        } 
        n64 := int64(len(px))
        cp[i] = color.NRGBA64{
            uint16(rsum / n64),
            uint16(gsum / n64),
            uint16(bsum / n64),
            0xffff,
        }
        // set image pixels
        for _, p := range px {
            pi.SetColorIndex(p.x, p.y, uint8(i))
        }
    }
    return pi
}

// Implement sort.Interface for sort in median algorithm.
func (c chValues) Len() int           { return len(c) }
func (c chValues) Less(i, j int) bool { return c[i] < c[j] }
func (c chValues) Swap(i, j int)      { c[i], c[j] = c[j], c[i] }

// Implement heap.Interface for priority queue of clusters.
func (q queue) Len() int { return len(q) }

// Less implements rule to select cluster with greatest number of pixels.
func (q queue) Less(i, j int) bool {
    return len(q[j].px) < len(q[i].px)
}

func (q queue) Swap(i, j int) {
    q[i], q[j] = q[j], q[i]
}
func (pq *queue) Push(x interface{}) {
    c := x.(*cluster)
    *pq = append(*pq, c)
}
func (pq *queue) Pop() interface{} {
    q := *pq
    n := len(q) - 1
    c := q[n]
    *pq = q[:n]
    return c
}
```



## Haskell

{{libheader|JuicyPixels}}
A variation of the median cut algorithm by splitting color space on the nearest to the mean instead. It provides lower error than the Gimp output sample.

```Haskell
import qualified Data.ByteString.Lazy as BS
import qualified Data.Foldable as Fold
import qualified Data.List as List
import Data.Ord
import qualified Data.Sequence as Seq
import Data.Word
import System.Environment

import Codec.Picture
import Codec.Picture.Types

type Accessor = PixelRGB8 -> Pixel8

-- Getters for pixel components, as the constructor does not
-- provide any public ones.
red, blue, green :: Accessor
red   (PixelRGB8 r _ _) = r
green (PixelRGB8 _ g _) = g
blue  (PixelRGB8 _ _ b) = b

-- Get all of the pixels in the image in list form.
getPixels :: Pixel a => Image a -> [a]
getPixels image =
  [pixelAt image x y
  | x <- [0..(imageWidth image - 1)]
  , y <- [0..(imageHeight image - 1)]]

-- Compute the color-space extents of a list of pixels.
extents :: [PixelRGB8] -> (PixelRGB8, PixelRGB8)
extents pixels = (extent minimum, extent maximum)
  where
    bound f g = f $ map g pixels
    extent f  = PixelRGB8 (bound f red) (bound f green) (bound f blue)

-- Compute the average value of a list of pixels.
average :: [PixelRGB8] -> PixelRGB8
average pixels = PixelRGB8 (avg red) (avg green) (avg blue)
  where
    len   = toInteger $ length pixels
    avg c = fromIntegral $ (sum $ map (toInteger . c) pixels) `div` len

-- Perform a componentwise pixel operation.
compwise :: (Word8 -> Word8 -> Word8) -> PixelRGB8 -> PixelRGB8 -> PixelRGB8
compwise f (PixelRGB8 ra ga ba) (PixelRGB8 rb gb bb) =
  PixelRGB8 (f ra rb) (f ga gb) (f ba bb)

-- Compute the absolute difference of two pixels.
diffPixel :: PixelRGB8 -> PixelRGB8 -> PixelRGB8
diffPixel = compwise (\x y -> max x y - min x y)

-- Compute the Euclidean distance squared between two pixels.
distPixel :: PixelRGB8 -> PixelRGB8 -> Integer
distPixel x y = (rr ^ 2) + (gg ^ 2) + (bb ^ 2) 
  where
    PixelRGB8 r g b = diffPixel x y
    rr              = toInteger r
    gg              = toInteger g
    bb              = toInteger b

-- Determine the dimension of the longest axis of the extents.
longestAccessor :: (PixelRGB8, PixelRGB8) -> Accessor
longestAccessor (l, h) =
  snd $ Fold.maximumBy (comparing fst) $ zip [r, g, b] [red, green, blue]
  where
    PixelRGB8 r g b = diffPixel h l

-- Find the index of a pixel to its respective palette.
nearestIdx :: PixelRGB8 -> [PixelRGB8] -> Int
nearestIdx pixel px = ans
  where
    Just ans = List.findIndex (== near) px
    near     = List.foldl1 comp px
    comp a b = if distPixel a pixel <= distPixel b pixel then a else b

-- Sort a list of pixels on its longest axis and then split by the mean.
-- It is intentional that the mean is chosen by all dimensions
-- instead of the given one.
meanSplit :: [PixelRGB8] -> Accessor -> ([PixelRGB8], [PixelRGB8])
meanSplit l f = List.splitAt index sorted
  where
    sorted = List.sortBy (comparing f) l
    index  = nearestIdx (average l) sorted

-- Perform the Median Cut algorithm on an image producing
-- an index map image and its respective palette.
meanCutQuant :: Image PixelRGB8 -> Int -> (Image Pixel8, Palette)
meanCutQuant image numRegions = (indexmap, palette)
  where
    extentsP p   = (p, extents p)
    regions      = map (\(p, e) -> (average p, e))
                   $ search $ Seq.singleton $ extentsP $ getPixels image
    palette      = snd $ generateFoldImage (\(x:xs) _ _ -> (xs, x))
                   (map fst regions) numRegions 1
    indexmap     = pixelMap
                   (\pixel -> fromIntegral $ nearestIdx pixel $ map fst regions)
                   image
    search queue =
      case Seq.viewl queue of
        (pixels, extent) Seq.:< queueB ->
          let (left, right) = meanSplit pixels $ longestAccessor extent
              queueC        = Fold.foldl (Seq.|>) queueB $ map extentsP [left, right]
          in if Seq.length queueC >= numRegions
             then List.take numRegions $ Fold.toList queueC
             else search queueC
        Seq.EmptyL                     -> error "Queue should never be empty."

quantizeIO :: FilePath -> FilePath -> Int -> IO ()
quantizeIO path outpath numRegions = do
  dynimage <- readImage path
  case dynimage of
    Left err                 -> putStrLn err
    Right (ImageRGB8 image)  -> doImage image 
    Right (ImageRGBA8 image) -> doImage (pixelMap dropTransparency image)
    _                        -> putStrLn "Expecting RGB8 or RGBA8 image"
  where
    doImage image = do
      let (indexmap, palette) = meanCutQuant image numRegions
      case encodePalettedPng palette indexmap of
        Left err      -> putStrLn err
        Right bstring -> BS.writeFile outpath bstring

main :: IO ()
main = do
  args <- getArgs
  prog <- getProgName
  case args of
    [path, outpath] -> quantizeIO path outpath 16
    _               -> putStrLn $ "Usage: " ++ prog ++ " <image-file> <out-file.png>"
```



## J


Here, we use a simplistic averaging technique to build an initial set of colors and then use k-means clustering to refine them.


```j
kmcL=:4 :0
  C=. /:~ 256 #.inv ,y  NB. colors
  G=. x (i.@] <.@* %) #C  NB. groups (initial)
  Q=. _  NB. quantized list of colors (initial
  whilst.-. Q-:&<.&(x&*)Q0 do.
    Q0=. Q
    Q=. /:~C (+/ % #)/.~ G
    G=. (i. <./)"1 C +/&.:*: .- |:Q
  end.Q
)
```


The left argument is the number of colors desired.

The right argument is the image, with pixels represented as bmp color integers (base 256 numbers).

The result is the colors represented as pixel triples (blue, green, red).  They are shown here as fractional numbers, but they should be either rounded to the nearest integer in the range 0..255 (and possibly converted back to bmp integer form) or scaled so they are floating point triples in the range 0..1.


```j
   16 kmcL img
7.52532 22.3347  0.650468
8.20129 54.4678 0.0326828
33.1132 69.8148  0.622265
54.2232 125.682   2.67713
56.7064 99.5008   3.04013
61.2135  136.42    4.2015
68.1246 140.576   6.37512
74.6006 143.606   7.57854
78.9101 150.792   10.2563
89.5873 148.621   14.6202
98.9523 154.005   25.7583
114.957 159.697   47.6423
145.816 178.136   33.8845
164.969 199.742   67.0467
179.849 207.594   109.973
209.229  221.18   204.513
```



## Julia

The Images package for Julia uses the ImageMagick libraries by default, but this Julia module does not currently implement ImageMagick's support for color quantization. However, once ImageMagick is installed for the Images Julia module, a direct call to ImageMagick's convert command is possible.

```julia

const execstring =`convert Quantum_frog.png -dither None -colors 16 Quantum_frog_new.png`
run(execstring)        

```




## Kotlin

{{works with|Ubuntu 16.04}}
Rather than coding this from scratch, we invoke programatically ImageMagick's 'convert' tool which has all this stuff built in.

```scala
// Version 1.2.41

import java.io.BufferedReader
import java.io.InputStreamReader

fun main(args: Array<String>) {
    // convert 'frog' to an image which uses only 16 colors, no dithering
    val pb = ProcessBuilder(
        "convert",
        "Quantum_frog.png",
        "-dither",
        "None",
        "-colors",
        "16",
        "Quantum_frog_16.png"
    )
    pb.directory(null)
    val proc = pb.start()
    proc.waitFor()

    // now show the colors used
    val pb2 = ProcessBuilder(
       "convert",
       "Quantum_frog_16.png",
       "-format",
       "%c",
       "-depth",
       "8",
       "histogram:info:-"
    )
    pb2.directory(null)
    pb.redirectOutput(ProcessBuilder.Redirect.PIPE)
    val proc2 = pb2.start()
    val br = BufferedReader(InputStreamReader(proc2.inputStream))
    var clrNum = 0
    while (true) {
        val line = br.readLine() ?: break
        System.out.printf("%2d->%s\n", clrNum++, line)
    }
    br.close()
}
```


{{output}}
The resulting image is as expected and details of the 16 colors used are as follows:

```txt

 0->     37572: (  9, 53,  0) #093500 srgb(9,53,0)
 1->      7068: ( 13, 26,  0) #0D1A00 srgb(13,26,0)
 2->        31: ( 15,165, 21) #0FA515 srgb(15,165,21)
 3->     19609: ( 42, 96,  1) #2A6001 srgb(42,96,1)
 4->     21753: ( 57,136,  4) #398804 srgb(57,136,4)
 5->     66865: ( 77,147, 10) #4D930A srgb(77,147,10)
 6->     12275: ( 79,111,  9) #4F6F09 srgb(79,111,9)
 7->       836: ( 94,111, 74) #5E6F4A srgb(94,111,74)
 8->     25689: (105,158, 28) #699E1C srgb(105,158,28)
 9->      5095: (113,163, 85) #71A355 srgb(113,163,85)
10->      1788: (125,129,151) #7D8197 srgb(125,129,151)
11->     12929: (145,172, 31) #91AC1F srgb(145,172,31)
12->     13245: (158,200, 51) #9EC833 srgb(158,200,51)
13->     17024: (175,210, 86) #AFD256 srgb(175,210,86)
14->      7913: (177,192, 99) #B1C063 srgb(177,192,99)
15->     12452: (202,217,188) #CAD9BC srgb(202,217,188)

```


=={{header|Mathematica}} / {{header|Wolfram Language}}==

```Mathematica
ColorQuantize[Import["http://rosettacode.org/mw/images/3/3f/Quantum_frog.png"],16,Dithering->False]
```



## OCaml


Here we use a simplified method inspired from this paper: [http://www.leptonica.com/papers/mediancut.pdf www.leptonica.com/papers/mediancut.pdf]


```ocaml
let rem_from rem from =
  List.filter ((<>) rem) from

let float_rgb (r,g,b) =  (* prevents int overflow *)
  (float r, float g, float b)

let round x =
  int_of_float (floor (x +. 0.5))

let int_rgb (r,g,b) =
  (round r, round g, round b)

let rgb_add (r1,g1,b1) (r2,g2,b2) =
  (r1 +. r2,
   g1 +. g2,
   b1 +. b2)

let rgb_mean px_list =
  let n = float (List.length px_list) in
  let r, g, b = List.fold_left rgb_add (0.0, 0.0, 0.0) px_list in
  (r /. n, g /. n, b /. n)

let extrems lst =
  let min_rgb = (infinity, infinity, infinity)
  and max_rgb = (neg_infinity, neg_infinity, neg_infinity) in
  List.fold_left (fun ((sr,sg,sb), (mr,mg,mb)) (r,g,b) ->
    ((min sr r), (min sg g), (min sb b)),
    ((max mr r), (max mg g), (max mb b))
  ) (min_rgb, max_rgb) lst

let volume_and_dims lst =
  let (sr,sg,sb), (br,bg,bb) = extrems lst in
  let dr, dg, db = (br -. sr), (bg -. sg), (bb -. sb) in
  (dr *. dg *. db),
  (dr, dg, db)

let make_cluster pixel_list =
  let vol, dims = volume_and_dims pixel_list in
  let len = float (List.length pixel_list) in
  (rgb_mean pixel_list, len *. vol, dims, pixel_list)

type axis = R | G | B
let largest_axis (r,g,b) =
  match compare r g, compare r b with
  | 1, 1 -> R
  | -1, 1 -> G
  | 1, -1 -> B
  | _ ->
      match compare g b with
      | 1 -> G
      | _ -> B

let subdivide ((mr,mg,mb), n_vol_prod, vol, pixels) =
  let part_func =
    match largest_axis vol with
    | R -> (fun (r,_,_) -> r < mr)
    | G -> (fun (_,g,_) -> g < mg)
    | B -> (fun (_,_,b) -> b < mb)
  in
  let px1, px2 = List.partition part_func pixels in
  (make_cluster px1, make_cluster px2)

let color_quant img n =
  let width, height = get_dims img in
  let clusters =
    let lst = ref [] in
    for x = 0 to pred width do
      for y = 0 to pred height do
        let rgb = float_rgb (get_pixel_unsafe img x y) in
        lst := rgb :: !lst
      done;
    done;
    ref [make_cluster !lst]
  in
  while (List.length !clusters) < n do
    let dumb = (0.0,0.0,0.0) in
    let unused = (dumb, neg_infinity, dumb, []) in
    let select ((_,v1,_,_) as c1) ((_,v2,_,_) as c2) =
      if v1 > v2 then c1 else c2
    in
    let cl = List.fold_left (fun c1 c2 -> select c1 c2) unused !clusters in
    let cl1, cl2 = subdivide cl in
    clusters := cl1 :: cl2 :: (rem_from cl !clusters)
  done;
  let module PxMap = Map.Make
    (struct type t = float * float * float let compare = compare end) in
  let m =
    List.fold_left (fun m (mean, _, _, pixel_list) ->
      let int_mean = int_rgb mean in
      List.fold_left (fun m px -> PxMap.add px int_mean m) m pixel_list
    ) PxMap.empty !clusters
  in
  let res = new_img ~width ~height in
  for y = 0 to pred height do
    for x = 0 to pred width do
      let rgb = float_rgb (get_pixel_unsafe img x y) in
      let mean_rgb = PxMap.find rgb m in
      put_pixel_unsafe res mean_rgb x y;
    done;
  done;
  (res)
```



## Perl


```perl
use strict;
use warnings;

use Imager;

my $img = Imager->new;
$img->read(file => 'frog.png');
my $img16 = $img->to_paletted({ max_colors => 16});
$img16->write(file => "frog-16.png")
```

Compare offsite images: [https://github.com/SqrtNegInf/Rosettacode-Perl5-Smoke/blob/master/ref/frog.png frog.png] vs.
[https://github.com/SqrtNegInf/Rosettacode-Perl5-Smoke/blob/master/ref/frog-16.png frog-16.png]


## Perl 6

{{works with|Rakudo|2018.10}}


```perl6
use MagickWand;
use MagickWand::Enums;

my $frog = MagickWand.new;
$frog.read("./Quantum_frog.png");
$frog.quantize(16, RGBColorspace, 0, True, False);
$frog.write('./Quantum-frog-16-perl6.png');
```

See: [https://github.com/thundergnat/rc/blob/master/img/Quantum-frog-16-perl6.png Quantum-frog-16-perl6.png] (offsite .png image)


## Phix

{{libheader|pGUI}}
{{trans|Tcl}}
Gui app, shows original and modified side-by-side.

```Phix
-- demo\rosetta\Color_quantization.exw
include pGUI.e

function makeCluster(sequence pixels)
    sequence rs = vslice(pixels,1),
             gs = vslice(pixels,2),
             bs = vslice(pixels,3)
    integer n = length(pixels),
            rd = max(rs)-min(rs),
            gd = max(gs)-min(gs),
            bd = max(bs)-min(bs)
    atom score = n*rd*gd*bd
--  atom score = n*(rd+gd+bd)  -- (this is how/where to experiment)
    sequence centroid = sq_round({sum(rs)/n,sum(gs)/n,sum(bs)/n}),
             ranges = {rd,gd,bd}
    return {score,centroid,ranges,pixels}
end function
 
function colorQuant(imImage img, integer n)
integer width = im_width(img),
        height = im_width(img)
    -- Extract the original pixels from the image
    sequence original = {}
    integer dx = 1
    for y=height-1 to 0 by -1 do
        for x=0 to width-1 do
            original = append(original,im_pixel(img, x, y)&dx)
            dx += 1
        end for
    end for
    -- Divide pixels into clusters
    sequence cs = {makeCluster(original)}, unsplittable={}, centroid, volume, pixels
    while length(cs)<n do
        cs = sort(cs)
--      cs = reverse(sort(cs)) -- (to deliberately show a much worse result)
        {?,centroid,volume,pixels} = cs[$]
        integer {vr,vg,vb} = volume
        integer pdx = iff(vr>vg and vr>vb?1:iff(vg>vb?2:3)),
                c = centroid[pdx] -- (ie r=1, g=2, b=3)
        sequence p1 = {}, p2 = {}
        for i=1 to length(pixels) do
            sequence p = pixels[i]
            if p[pdx]<c then p1 = append(p1,p) else p2 = append(p2,p) end if
        end for
        if length(p1) and length(p2) then
            cs[$] = makeCluster(p1)
            cs = append(cs,makeCluster(p2))
        else
            ?"unsplittable"
            unsplittable = append(unsplittable,cs[$])
            cs = cs[1..$-1]
            n -= 1
        end if
    end while
    cs &= unsplittable
    -- substitute all pixels with the centroid (aka cluster average)
    for i=1 to length(cs) do
        {?,centroid,?,pixels} = cs[i]
        for p=1 to length(pixels) do
            dx = pixels[p][4]
            original[dx] = centroid
        end for
    end for
    original = flatten(original) -- (needed for IupImageRGB)
    Ihandle new_img = IupImageRGB(width, height, original) 
    return new_img
end function

IupOpen()

atom pError = allocate(machine_word())
imImage im1 = imFileImageLoadBitmap("Quantum_frog.png",0,pError)
if im1=NULL then ?"error opening Quantum_frog.png" abort(0) end if
-- stolen from simple_paint (else im_pixel crashed):
-- we are going to support only RGB images with no alpha
imImageRemoveAlpha(im1)
if im_color_space(im1)!=IM_RGB then
    imImage new_image = imImageCreateBased(im1, -1, -1, IM_RGB, -1)
    imConvertColorSpace(im1, new_image)
    imImageDestroy(im1)
    im1 = new_image
end if

Ihandln image1 = IupImageFromImImage(im1),
        image2 = colorQuant(im1,16),
        label1 = IupLabel(),
        label2 = IupLabel()
IupSetAttributeHandle(label1, "IMAGE", image1)
IupSetAttributeHandle(label2, "IMAGE", image2)

Ihandle dlg = IupDialog(IupHbox({label1, label2}))
IupSetAttribute(dlg, "TITLE", "Color quantization")
IupCloseOnEscape(dlg)
IupShow(dlg)

IupMainLoop()
IupClose()
```



## PureBasic

[[file:Compare_16_Quantum_frog_PureBasic.png|comparison|thumb|200px]]
[[file:Compare_16_Quantum_frog_histograms_PureBasic.png|histogram (external application)|thumb|200px]]

```PureBasic

; ColorQuantization.pb

Structure bestA_ ; table for our histogram
nn.i ; 16,32,...
rc.i ; red   count within (0,1,...,255)/(number of colors)
gc.i ; green count within (0,1,...,255)/(number of colors)
bc.i ; blue  count within (0,1,...,255)/(number of colors)
EndStructure

; these two functions appear to be rather self-explanatory
UsePNGImageDecoder()
UsePNGImageEncoder()

Procedure.i ColorQuantization(Filename$,ncol)
Protected x,y,c

; load our original image or leave the procedure
If not LoadImage(0,Filename$)      :ProcedureReturn 0:endif

; we are not going to actually draw on the original image...
; but we need to use the drawing library to load up
; the pixel information into our arrays...
; if we can't do that, what's the point of going any further?
; so then we would be wise to just leave the procedure [happy fred?]
If not StartDrawing(ImageOutput(0)):ProcedureReturn 0:endif

iw=ImageWidth(0)
ih=ImageHeight(0)

dim cA(iw,ih) ; color array to hold at a given (x,y)
dim rA(iw,ih) ; red   array to hold at a given (x,y)
dim gA(iw,ih) ; green array to hold at a given (x,y)
dim bA(iw,ih) ; blue  array to hold at a given (x,y)
dim tA(iw,ih) ; temp  array to hold at a given (x,y)

; map each pixel from the original image to our arrays
; don't overrun the ranges ie. use {ih-1,iw-1}
for y=0 to ih-1
  for x=0 to iw-1
  c = Point(x,y)
  cA(x,y)=c
  rA(x,y)=Red(c)
  gA(x,y)=Green(c)
  bA(x,y)=Blue(c)
  next 
next 

StopDrawing() ; don't forget to... StopDrawing()

N=ih*iw
; N is the total number if pixels
if not N:ProcedureReturn 0:endif ; to avoid a division by zero

; stuctured array ie. a table to hold the frequency distribution
dim bestA.bestA_(ncol)

; the "best" red,green,blue based upon frequency 
dim rbestA(ncol/3)
dim gbestA(ncol/3)
dim bbestA(ncol/3)

; split the (0..255) range up
xoff=256/ncol   ;256/16=16
xrng=xoff       ;xrng=16

; store these values in our table: bestA(i)\nn= 16,32,...
for i=1 to ncol
xrng+xoff
bestA(i)\nn=xrng
next 

; scan by row [y] 
for y=0 to ih-1
; scan by col [x] 
for x=0 to iw-1

; retrieve the rgb values from each pixel 
r=rA(x,y)
g=gA(x,y)
b=bA(x,y)

; sum up the numbers that fall within our subdivisions of (0..255)
for i=1 to ncol
if r>=bestA(i)\nn and r<bestA(i+1)\nn:bestA(i)\rc+1:endif
if g>=bestA(i)\nn and g<bestA(i+1)\nn:bestA(i)\gc+1:endif
if b>=bestA(i)\nn and b<bestA(i+1)\nn:bestA(i)\bc+1:endif
next 
next 
next 

; option and type to: Sort our Structured Array
opt=#PB_Sort_Descending
typ=#PB_Sort_Integer

; sort to get most frequent reds
off=OffsetOf(bestA_\rc)
SortStructuredArray(bestA(),opt, off, typ,1, ncol)

; save the best [ for number of colors =16 this is int(16/3)=5 ] reds
for i=1 to ncol/3
rbestA(i)=bestA(i)\nn
next

; sort to get most frequent greens
off=OffsetOf(bestA_\gc)
SortStructuredArray(bestA(),opt, off, typ,1, ncol)

; save the best [ for number of colors =16 this is int(16/3)=5 ] greens
for i=1 to ncol/3
gbestA(i)=bestA(i)\nn
next

; sort to get most frequent blues
off=OffsetOf(bestA_\bc)
SortStructuredArray(bestA(),opt, off, typ,1, ncol)

; save the best [ for number of colors =16 this is int(16/3)=5 ] blues
for i=1 to ncol/3
bbestA(i)=bestA(i)\nn
next

; reset the best low value to 15 and high value to 240
; this helps to ensure there is some contrast when the statistics bunch up
; ie. when a single color tends to predominate... such as perhaps green?
rbestA(1)=15:rbestA(ncol/3)=240
gbestA(1)=15:gbestA(ncol/3)=240
bbestA(1)=15:bbestA(ncol/3)=240

; make a copy of our original image or leave the procedure
If not CopyImage(0,1)              :ProcedureReturn 0:endif

; draw on that copy of our original image or leave the procedure
If not StartDrawing(ImageOutput(1)):ProcedureReturn 0:endif

for y=0 to ih-1
for x=0 to iw-1
c = Point(x,y)

; get the rgb value from our arrays
rt=rA(x,y)
gt=gA(x,y)
bt=bA(x,y)

; given a particular red value say 123 at point x,y
; which of our rbestA(i's) is closest?
; then for green and blue?
; 
### ========================

r=255
for i=1 to ncol/3
rdiff=abs(rbestA(i)-rt)
if rdiff<=r:ri=i:r=rdiff:endif
next

g=255
for i=1 to ncol/3
gdiff=abs(gbestA(i)-gt)
if gdiff<=g:gi=i:g=gdiff:endif
next

b=255
for i=1 to ncol/3
bdiff=abs(bbestA(i)-bt)
if bdiff<=b:bi=i:b=bdiff:endif
next
; 
### ========================



; get the color value so we can plot it at that pixel
Color=RGB(rbestA(ri),gbestA(gi),bbestA(bi))

; plot it at that pixel
Plot(x,y,Color)

; save that info to tA(x,y) for our comparison image 
tA(x,y)=Color

next 
next 
StopDrawing() ; don't forget to... StopDrawing()

; create a comparison image of our original vs 16-color or leave the procedure
If not CreateImage(2,iw*2,ih)      :ProcedureReturn 0:endif
; draw on that image both our original image and our 16-color image or leave the procedure
If not StartDrawing(ImageOutput(2)):ProcedureReturn 0:endif

; plot original image
; 0,0 .... 511,0
; .
; .
; 511,0 .. 511,511
for y=0 to ih-1
  for x=0 to iw-1
  c = cA(x,y)
  Plot(x,y,c)
  next 
  next 

; plot 16-color image to the right of original image
; 512,0 .... 1023,0
; .
; .
; 512,511 .. 1023,511
for y=0 to ih-1
  for x=0 to iw-1
  c = tA(x,y)
  Plot(x+iw,y,c)
  next 
  next 

StopDrawing() ; don't forget to... StopDrawing()

; save the single 16-color image
SaveImage(1, "_single_"+str(ncol)+"_"+Filename$,#PB_ImagePlugin_PNG )

; save the comparison image
SaveImage(2, "_compare_"+str(ncol)+"_"+Filename$,#PB_ImagePlugin_PNG )
ProcedureReturn 1
EndProcedure

ColorQuantization("Quantum_frog.png",16)



```



## Python


```python
from PIL import Image

if __name__=="__main__":
	im = Image.open("frog.png")
	im2 = im.quantize(16)
	im2.show()
```



## Racket


```Racket

#lang racket/base
(require racket/class
         racket/draw)

;; This is an implementation of the Octree Quantization algorithm.  This implementation 
;; follows the sketch in:
;;
;; Dean Clark.  Color Quantization using Octrees.  Dr. Dobbs Portal, January 1, 1996.
;; http://www.ddj.com/184409805
;;
;; This code is adapted from the color quantizer in the implementation of Racket's
;; file/gif standard library.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; To view an example of the quantizer, run the following test submodule
;; in DrRacket:
(module+ test
  (require racket/block net/url)
  
  (define frog 
    (block
      (define url (string->url "http://rosettacode.org/mw/images/3/3f/Quantum_frog.png"))
      (define frog-ip (get-pure-port url))
      (define bitmap (make-object bitmap% frog-ip))
      (close-input-port frog-ip)
      bitmap))

  ;; Display the original:
  (print frog)
  ;; And the quantized version (16 colors):
  (print (quantize-bitmap frog 16)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; quantize-bitmap: bitmap positive-number -> bitmap
;; Given a bitmap, returns a new bitmap quantized to, at most, n colors.
(define (quantize-bitmap bm n)
  (let* ([width (send bm get-width)]
         [height (send bm get-height)]
         [len (* width height 4)]
         [source-buffer (make-bytes len)]
         [_ (send bm get-argb-pixels 0 0 width height source-buffer)]
         [an-octree (make-octree-from-argb source-buffer n)]
         [dest-buffer (make-bytes len)])
    (let quantize-bitmap-loop ([i 0])
      (when (< i len)
        (let* ([i+1 (+ i 1)]
               [i+2 (+ i 2)]
               [i+3 (+ i 3)]
               [a (bytes-ref source-buffer i)]
               [r (bytes-ref source-buffer i+1)]
               [g (bytes-ref source-buffer i+2)]
               [b (bytes-ref source-buffer i+3)])
          (cond
            [(alpha-opaque? a)
             (let-values ([(new-r new-g new-b)
                           (octree-lookup an-octree r g b)])
               (bytes-set! dest-buffer i 255)
               (bytes-set! dest-buffer i+1 new-r)
               (bytes-set! dest-buffer i+2 new-g)
               (bytes-set! dest-buffer i+3 new-b))]
            [else
             (bytes-set! dest-buffer i 0)
             (bytes-set! dest-buffer i+1 0)
             (bytes-set! dest-buffer i+2 0)
             (bytes-set! dest-buffer i+3 0)]))
        (quantize-bitmap-loop (+ i 4))))
    (let* ([new-bm (make-object bitmap% width height)]
           [dc (make-object bitmap-dc% new-bm)])
      (send dc set-argb-pixels 0 0 width height dest-buffer)
      (send dc set-bitmap #f)
      new-bm)))





;; make-octree-from-argb: bytes positive-number -> octree
;; Constructs an octree ready to quantize the colors from an-argb.
(define (make-octree-from-argb an-argb n)
  (unless (> n 0)
    (raise-type-error 'make-octree-from-argb "positive number" n))
  (let ([an-octree (new-octree)]
        [len (bytes-length an-argb)])
    (let make-octree-loop ([i 0])
      (when (< i len)
        (let ([a (bytes-ref an-argb i)]
              [r (bytes-ref an-argb (+ i 1))]
              [g (bytes-ref an-argb (+ i 2))]
              [b (bytes-ref an-argb (+ i 3))])
          (when (alpha-opaque? a)
            (octree-insert-color! an-octree r g b)
            (let reduction-loop ()
              (when (> (octree-leaf-count an-octree) n)
                (octree-reduce! an-octree)
                (reduction-loop)))))
        (make-octree-loop (+ i 4))))
    (octree-finalize! an-octree)
    an-octree))


;; alpha-opaque? byte -> boolean
;; Returns true if the alpha value is considered opaque.
(define (alpha-opaque? a)
  (>= a 128))



;; The maximum level height of an octree.
(define MAX-LEVEL 7)



;; A color is a (vector byte byte byte)

;; An octree is a:
(define-struct octree (root            ; node
                       leaf-count      ; number
                       reduction-heads ; (vectorof (or/c node #f))
                       palette)        ; (vectorof (or/c color #f))
  #:mutable)
;; reduction-heads is used to accelerate the search for a reduction candidate.


;; A subtree node is a:
(define-struct node (leaf?          ; bool
                     npixels        ; number  -- number of pixels this subtree node represents
                     redsum         ; number
                     greensum       ; number
                     bluesum        ; number
                     children       ; (vectorof (or/c #f node))
                     next           ; (or/c #f node)
                     palette-index) ; (or/c #f byte?)
  #:mutable)
;; node-next is used to accelerate the search for a reduction candidate.


;; new-octree: -> octree
(define (new-octree)
  (let* ([root-node (make-node #f ;; not a leaf
                               0  ;; no pixels under us yet
                               0  ;; red sum
                               0  ;; green sum
                               0  ;; blue sum
                               (make-vector 8 #f) ;; no children so far
                               #f ;; next
                               #f ;; palette-index
                               )]
         [an-octree
          (make-octree root-node
                       0 ; no leaves so far
                       (make-vector (add1 MAX-LEVEL) #f) ; no reductions so far
                       (make-vector 256 #(0 0 0)))])        ; the palette
    ;; Although we'll almost never reduce to this level, initialize the first
    ;; reducible node to the root, for completeness sake.
    (vector-set! (octree-reduction-heads an-octree) 0 root-node)
    an-octree))


;; rgb->index: natural-number byte byte byte -> octet
;; Given a level and an (r,g,b) triplet, returns an octet that can be used
;; as an index into our octree structure.
(define (rgb->index level r g b)
  (bitwise-ior (bitwise-and 4 (arithmetic-shift r (- level 5)))
               (bitwise-and 2 (arithmetic-shift g (- level 6)))
               (bitwise-and 1 (arithmetic-shift b (- level 7)))))


;; octree-insert-color!: octree byte byte byte -> void
;; Accumulates a new r,g,b triplet into the octree.
(define (octree-insert-color! an-octree r g b)
  (node-insert-color! (octree-root an-octree) an-octree r g b 0))


;; node-insert-color!: node octree byte byte byte natural-number -> void
;; Adds a color to the node subtree.  While we hit #f, we create new nodes.
;; If we hit an existing leaf, we accumulate our color into it.
(define (node-insert-color! a-node an-octree r g b level)
  (let insert-color-loop ([a-node a-node]
                          [level level])
    (cond [(node-leaf? a-node)
           ;; update the leaf with the new color
           (set-node-npixels! a-node (add1 (node-npixels a-node)))
           (set-node-redsum! a-node (+ (node-redsum a-node) r))
           (set-node-greensum! a-node (+ (node-greensum a-node) g))
           (set-node-bluesum! a-node (+ (node-bluesum a-node) b))]
          [else
           ;; create the child node if necessary
           (let ([index (rgb->index level r g b)])
             (unless (vector-ref (node-children a-node) index)
               (let ([new-node (make-node (= level MAX-LEVEL) ; leaf?
                                          0  ; npixels
                                          0  ; redsum
                                          0  ; greensum
                                          0  ; bluesum
                                          (make-vector 8 #f) ; no children yet
                                          #f ; and no next node yet
                                          #f ; or palette index
                                          )])
                 (vector-set! (node-children a-node) index new-node)
                 (cond
                   [(= level MAX-LEVEL)
                    ;; If we added a leaf, mark it in the octree.
                    (set-octree-leaf-count! an-octree
                                            (add1 (octree-leaf-count an-octree)))]
                   [else
                    ;; Attach the node as a reducible node if it's interior.
                    (set-node-next!
                     new-node (vector-ref (octree-reduction-heads an-octree)
                                          (add1 level)))
                    (vector-set! (octree-reduction-heads an-octree) 
                                 (add1 level)
                                 new-node)])))
             ;; and recur on the child node.
             (insert-color-loop (vector-ref (node-children a-node) index) 
                                (add1 level)))])))


;; octree-reduce!: octree -> void
;; Reduces one of the subtrees, collapsing the children into a single node.
(define (octree-reduce! an-octree)
  (node-reduce! (pop-reduction-candidate! an-octree) an-octree))


;; node-reduce!: node octree -> void
;; Reduces the interior node.
(define (node-reduce! a-node an-octree)
  (for ([child (in-vector (node-children a-node))]
        #:when child)
    (set-node-npixels! a-node (+ (node-npixels a-node)
                                 (node-npixels child)))
    (set-node-redsum! a-node (+ (node-redsum a-node)
                                (node-redsum child)))
    (set-node-greensum! a-node (+ (node-greensum a-node)
                                  (node-greensum child)))
    (set-node-bluesum! a-node (+ (node-bluesum a-node)
                                 (node-bluesum child)))
    (set-octree-leaf-count! an-octree (sub1 (octree-leaf-count an-octree))))
  (set-node-leaf?! a-node #t)
  (set-octree-leaf-count! an-octree (add1 (octree-leaf-count an-octree))))


;; find-reduction-candidate!: octree -> node
;; Returns a bottom-level interior node for reduction.  Also takes the
;; candidate out of the conceptual queue of reduction candidates.
(define (pop-reduction-candidate! an-octree)
  (let loop ([i MAX-LEVEL])
    (cond
      [(vector-ref (octree-reduction-heads an-octree) i)
       =>
       (lambda (candidate-node)
         (when (> i 0)
           (vector-set! (octree-reduction-heads an-octree) i
                        (node-next candidate-node)))
         candidate-node)]
      [else
       (loop (sub1 i))])))


;; octree-finalize!: octree -> void
;; Finalization does a few things:
;; * Walks through the octree and reduces any interior nodes with just one leaf child.
;;   Optimizes future lookups.
;; * Fills in the palette of the octree and the palette indexes of the leaf nodes.
;; * Note: palette index 0 is always reserved for the transparent color.
(define (octree-finalize! an-octree)
  ;; Collapse one-leaf interior nodes.
  (let loop ([a-node (octree-root an-octree)])
    (for ([child (in-vector (node-children a-node))]
          #:when (and child (not (node-leaf? child))))
      (loop child)
      (when (interior-node-one-leaf-child? a-node)
        (node-reduce! a-node an-octree))))

  ;; Attach palette entries.
  (let ([current-palette-index 1])
    (let loop ([a-node (octree-root an-octree)])
      (cond [(node-leaf? a-node)
             (let ([n (node-npixels a-node)])
               (vector-set! (octree-palette an-octree) current-palette-index
                            (vector (quotient (node-redsum a-node) n)
                                    (quotient (node-greensum a-node) n)
                                    (quotient (node-bluesum a-node) n)))
               (set-node-palette-index! a-node current-palette-index)
               (set! current-palette-index (add1 current-palette-index)))]
            [else
             (for ([child (in-vector (node-children a-node))]
                   #:when child)
               (loop child))]))))


;; interior-node-one-leaf-child?: node -> boolean
(define (interior-node-one-leaf-child? a-node)
  (let ([child-list (filter values (vector->list (node-children a-node)))])
    (and (= (length child-list) 1)
         (node-leaf? (car child-list)))))


;; octree-lookup: octree byte byte byte -> (values byte byte byte)
;; Returns the palettized color.
(define (octree-lookup an-octree r g b)
  (let* ([index (node-lookup-index (octree-root an-octree) an-octree r g b 0)]
         [vec (vector-ref (octree-palette an-octree) index)])
    (values (vector-ref vec 0)
            (vector-ref vec 1)
            (vector-ref vec 2))))



;; node-lookup-index: node byte byte byte natural-number -> byte
;; Returns the palettized color index.
(define (node-lookup-index a-node an-octree r g b level)
  (let loop ([a-node a-node]
             [level level])
    (if (node-leaf? a-node)
      (node-palette-index a-node)
      (let ([child (vector-ref (node-children a-node) (rgb->index level r g b))])
        (unless child
          (error 'node-lookup-index
                 "color (~a, ~a, ~a) not previously inserted"
                 r g b))
        (loop child (add1 level))))))

```




## Sidef


```ruby
require('Image::Magick')

func quantize_image(n = 16, input, output='output.png') {
    var im = %O<Image::Magick>.new
    im.Read(input)
    im.Quantize(colors => n, dither => 1)    # 1 = None
    im.Write(output)
}

quantize_image(input: 'Quantum_frog.png')
```



## Tcl

{{trans|OCaml}}
{{libheader|Tk}}

```tcl
package require Tcl 8.6
package require Tk

proc makeCluster {pixels} {
    set rmin [set rmax [lindex $pixels 0 0]]
    set gmin [set gmax [lindex $pixels 0 1]]
    set bmin [set bmax [lindex $pixels 0 2]]
    set rsum [set gsum [set bsum 0]]
    foreach p $pixels {
	lassign $p r g b
	if {$r<$rmin} {set rmin $r} elseif {$r>$rmax} {set rmax $r}
	if {$g<$gmin} {set gmin $g} elseif {$g>$gmax} {set gmax $g}
	if {$b<$bmin} {set bmin $b} elseif {$b>$bmax} {set bmax $b}
	incr rsum $r
	incr gsum $g
	incr bsum $b
    }
    set n [llength $pixels]
    list [expr {double($n)*($rmax-$rmin)*($gmax-$gmin)*($bmax-$bmin)}] \
	[list [expr {$rsum/$n}] [expr {$gsum/$n}] [expr {$bsum/$n}]] \
	[list [expr {$rmax-$rmin}] [expr {$gmax-$gmin}] [expr {$bmax-$bmin}]] \
	$pixels
}

proc colorQuant {img n} {
    set width  [image width  $img]
    set height [image height $img]
    # Extract the pixels from the image
    for {set x 0} {$x < $width} {incr x} {
	for {set y 0} {$y < $height} {incr y} {
	    lappend pixels [$img get $x $y]
	}
    }
    # Divide pixels into clusters
    for {set cs [list [makeCluster $pixels]]} {[llength $cs] < $n} {} {
	set cs [lsort -real -index 0 $cs]
	lassign [lindex $cs end] score centroid volume pixels
	lassign $centroid cr cg cb
	lassign $volume vr vg vb
	while 1 {
	    set p1 [set p2 {}]
	    if {$vr>$vg && $vr>$vb} {
		foreach p $pixels {
		    if {[lindex $p 0]<$cr} {lappend p1 $p} {lappend p2 $p}
		}
	    } elseif {$vg>$vb} {
		foreach p $pixels {
		    if {[lindex $p 1]<$cg} {lappend p1 $p} {lappend p2 $p}
		}
	    } else {
		foreach p $pixels {
		    if {[lindex $p 2]<$cb} {lappend p1 $p} {lappend p2 $p}
		}
	    }
	    if {[llength $p1] && [llength $p2]} break
	    # Partition failed! Perturb partition point away from the centroid and try again
	    set cr [expr {$cr + 20*rand() - 10}]
	    set cg [expr {$cg + 20*rand() - 10}]
	    set cb [expr {$cb + 20*rand() - 10}]
	}
	set cs [lreplace $cs end end [makeCluster $p1] [makeCluster $p2]]
    }
    # Produce map from pixel values to quantized values
    foreach c $cs {
	set centroid [format "#%02x%02x%02x" {*}[lindex $c 1]]
	foreach p [lindex $c end] {
	    set map($p) $centroid
	}
    }
    # Remap the source image
    set newimg [image create photo -width $width -height $height]
    for {set x 0} {$x < $width} {incr x} {
	for {set y 0} {$y < $height} {incr y} {
	    $newimg put $map([$img get $x $y]) -to $x $y
	}
    }
    return $newimg
}
```

Demonstration code:

```tcl
set src [image create photo -file quantum_frog.png]
set dst [colorQuant $src 16]
# Save as GIF now that quantization is done, then exit explicitly (no GUI desired)
$dst write quantum_frog_compressed.gif
exit
```


{{omit from|AWK}}
{{omit from|GUISS}}
