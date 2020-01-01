+++
title = "Color quantization/C"
description = ""
date = 2011-08-21T12:04:14Z
aliases = []
[extra]
id = 10311
[taxonomies]
categories = []
tags = []
+++

[[file:Quantum_frog_dithered.png|200px|thumb]]This is a complete program that takes a PPM P6 image and a number, then writes out the image reduced to the number of colors to out.ppm.  There is optional dithering, too, which doesn't make a whole lot of difference with say 64 colors or more.  And with low colors, the quantization did such a good job of picking average colors that it actually hurts the dithering process.

```c
#include <stdio.h>
#include <stdlib.h>
#include <fcntl.h>
#include <unistd.h>
#include <ctype.h>
#include <string.h>
#include <stdint.h>
#include <math.h>

typedef struct {
	int w, h;
	unsigned char *pix;
} image_t, *image;

int write_ppm(image im, char *fn)
{
	FILE *fp = fopen(fn, "wb");
	if (!fp) return 0;
	fprintf(fp, "P6\n%d %d\n255\n", im->w, im->h);
	fwrite(im->pix, 1, 3 * im->w * im->h, fp);
	fclose(fp);
	return 1;
}

image img_new(int w, int h)
{
	image im = malloc(sizeof(image_t) + h * w * 3);
	im->w = w; im->h = h;
	im->pix = (unsigned char *)(im + 1);
	return im;
}

int read_num(FILE *f)
{
	int n;
	while (!fscanf(f, "%d ", &n)) {
		if ((n = fgetc(f)) == '#') {
			while ((n = fgetc(f)) != '\n')
				if (n == EOF) return 0;
		} else
			return 0;
	}
	return n;
}

image read_ppm(char *fn)
{
	FILE *fp = fopen(fn, "rb");
	int w, h, maxval;
	image im = 0;
	if (!fp) return 0;

	if (fgetc(fp) != 'P' || fgetc(fp) != '6' || !isspace(fgetc(fp)))
		goto bail;

	w = read_num(fp);
	h = read_num(fp);
	maxval = read_num(fp);
	if (!w || !h || !maxval) goto bail;

	im = img_new(w, h);
	fread(im->pix, 1, 3 * w * h, fp);
bail:
	if (fp) fclose(fp);
	return im;
}


#define ON_INHEAP	1

typedef struct oct_node_t oct_node_t, *oct_node;
struct oct_node_t{
	int64_t r, g, b; /* sum of all child node colors */
	int count, heap_idx;
	unsigned char n_kids, kid_idx, flags, depth;
	oct_node kids[8], parent;
};

typedef struct {
	int alloc, n;
	oct_node* buf;
} node_heap;

inline int cmp_node(oct_node a, oct_node b)
{
	if (a->n_kids < b->n_kids) return -1;
	if (a->n_kids > b->n_kids) return 1;

	int ac = a->count >> a->depth;
	int bc = b->count >> b->depth;
	return ac < bc ? -1 : ac > bc;
}

void down_heap(node_heap *h, oct_node p)
{
	int n = p->heap_idx, m;
	while (1) {
		m = n * 2;
		if (m >= h->n) break;
		if (m + 1 < h->n && cmp_node(h->buf[m], h->buf[m + 1]) > 0) m++;

		if (cmp_node(p, h->buf[m]) <= 0) break;

		h->buf[n] = h->buf[m];
		h->buf[n]->heap_idx = n;
		n = m;
	}
	h->buf[n] = p;
	p->heap_idx = n;
}

void up_heap(node_heap *h, oct_node p)
{
	int n = p->heap_idx;
	oct_node prev;

	while (n > 1) {
		prev = h->buf[n / 2];
		if (cmp_node(p, prev) >= 0) break;

		h->buf[n] = prev;
		prev->heap_idx = n;
		n /= 2;
	}
	h->buf[n] = p;
	p->heap_idx = n;
}

void heap_add(node_heap *h, oct_node p)
{
	if ((p->flags & ON_INHEAP)) {
		down_heap(h, p);
		up_heap(h, p);
		return;
	}

	p->flags |= ON_INHEAP;
	if (!h->n) h->n = 1;
	if (h->n >= h->alloc) {
		while (h->n >= h->alloc) h->alloc += 1024;
		h->buf = realloc(h->buf, sizeof(oct_node) * h->alloc);
	}

	p->heap_idx = h->n;
	h->buf[h->n++] = p;
	up_heap(h, p);
}

oct_node pop_heap(node_heap *h)
{
	if (h->n <= 1) return 0;

	oct_node ret = h->buf[1];
	h->buf[1] = h->buf[--h->n];

	h->buf[h->n] = 0;

	h->buf[1]->heap_idx = 1;
	down_heap(h, h->buf[1]);

	return ret;
}

static oct_node pool = 0;
oct_node node_new(unsigned char idx, unsigned char depth, oct_node p)
{
	static int len = 0;
	if (len <= 1) {
		oct_node p = calloc(sizeof(oct_node_t), 2048);
		p->parent = pool;
		pool = p;
		len = 2047;
	}

	oct_node x = pool + len--;
	x->kid_idx = idx;
	x->depth = depth;
	x->parent = p;
	if (p) p->n_kids++;
	return x;
}

void node_free()
{
	oct_node p;
	while (pool) {
		p = pool->parent;
		free(pool);
		pool = p;
	}
}

oct_node node_insert(oct_node root, unsigned char *pix)
{
	unsigned char i, bit, depth = 0;

	for (bit = 1 << 7; ++depth < 8; bit >>= 1) {
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

void error_diffuse(image im, node_heap *h)
{
	oct_node nearest_color(int *v) {
		int i;
		int diff, max = 100000000;
		oct_node o = 0;
		for (i = 1; i < h->n; i++) {
			diff =	  3 * abs(h->buf[i]->r - v[0])
				+ 5 * abs(h->buf[i]->g - v[1])
				+ 2 * abs(h->buf[i]->b - v[2]);
			if (diff < max) {
				max = diff;
				o = h->buf[i];
			}
		}
		return o;
	}

#	define POS(i, j) (3 * ((i) * im->w + (j)))
	int i, j;
	int *npx = calloc(sizeof(int), im->h * im->w * 3), *px;
	int v[3];
	unsigned char *pix = im->pix;
	oct_node nd;

#define C10 7
#define C01 5
#define C11 2
#define C00 1
#define CTOTAL (C00 + C11 + C10 + C01)

	for (px = npx, i = 0; i < im->h; i++) {
		for (j = 0; j < im->w; j++, pix += 3, px += 3) {
			px[0] = (int)pix[0] * CTOTAL;
			px[1] = (int)pix[1] * CTOTAL;
			px[2] = (int)pix[2] * CTOTAL;
		}
	}
#define clamp(x, i) if (x[i] > 255) x[i] = 255; if (x[i] < 0) x[i] = 0
	pix = im->pix;
	for (px = npx, i = 0; i < im->h; i++) {
		for (j = 0; j < im->w; j++, pix += 3, px += 3) {
			px[0] /= CTOTAL;
			px[1] /= CTOTAL;
			px[2] /= CTOTAL;
			clamp(px, 0); clamp(px, 1); clamp(px, 2);

			nd = nearest_color(px);

			v[0] = px[0] - nd->r;
			v[1] = px[1] - nd->g;
			v[2] = px[2] - nd->b;

			pix[0] = nd->r; pix[1] = nd->g; pix[2] = nd->b;
			if (j < im->w - 1) {
				npx[POS(i, j+1) + 0] += v[0] * C10;
				npx[POS(i, j+1) + 1] += v[1] * C10;
				npx[POS(i, j+1) + 2] += v[2] * C10;
			}
			if (i >= im->h - 1) continue;

			npx[POS(i+1, j) + 0] += v[0] * C01;
			npx[POS(i+1, j) + 1] += v[1] * C01;
			npx[POS(i+1, j) + 2] += v[2] * C01;

			if (j < im->w - 1) {
				npx[POS(i+1, j+1) + 0] += v[0] * C11;
				npx[POS(i+1, j+1) + 1] += v[1] * C11;
				npx[POS(i+1, j+1) + 2] += v[2] * C11;
			}
			if (j) {
				npx[POS(i+1, j-1) + 0] += v[0] * C00;
				npx[POS(i+1, j-1) + 1] += v[1] * C00;
				npx[POS(i+1, j-1) + 2] += v[2] * C00;
			}
		}
	}
	free(npx);
}

void color_quant(image im, int n_colors, int dither)
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
	}

	if (dither) error_diffuse(im, &heap);
	else
		for (i = 0, pix = im->pix; i < im->w * im->h; i++, pix += 3)
			color_replace(root, pix);

	node_free();
	free(heap.buf);
}

int main(int c, char *v[])
{
	if (c < 3) {
		fprintf(stderr, "usage: %s ppm_file n_colors\n", v[0]);
		return 0;
	}

	c = atoi(v[2]) ? : 16; /* GCC extension */

	image im = read_ppm(v[1]);
	color_quant(im, c, 0);
	write_ppm(im, "out.pnm");
	free(im);

	return 0;
}
```

