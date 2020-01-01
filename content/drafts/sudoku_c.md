+++
title = "Sudoku/C"
description = ""
date = 2012-07-29T02:03:36Z
aliases = []
[extra]
id = 12131
[taxonomies]
categories = []
tags = []
+++


```c
#include <stdio.h>

#if 1
#define N 4
int x[][N*N] = {
	{  6,  0,  7,  0,  0, 11,  0,  8,  1, 14, 15, 16,  0,  5,  2,  0},
	{  0, 10,  5,  0, 14,  0,  0,  0,  0,  0, 11,  3,  8,  0, 15,  0},
	{  0,  0, 16,  0,  0,  0,  0,  7,  0,  0,  9,  0, 12,  4,  0, 14},
	{  0, 13,  0,  0,  9,  0,  0, 12,  0,  5,  0,  0,  0,  0,  0, 16},
	{  0,  0,  0, 10,  0,  0, 13, 11,  2,  0,  0,  0,  3, 12,  0,  0},
	{ 12,  0,  9,  0,  8,  0,  0,  0, 16,  0,  0, 13,  0, 11,  0,  0},
	{  7,  0,  0,  14, 0,  0,  0,  0,  0,  0,  5,  0,  0,  0, 13,  2},
	{ 13,  0,  0,  8,  1,  0,  0,  5,  0,  7,  0, 11, 16,  0, 10,  0},
	{  0, 15,  0, 11,  5,  0,  7,  0,  3,  0,  0,  6,  2,  0,  0, 10},
	{  1,  4,  0,  0,  0, 13,  0,  0,  0,  0,  0,  0, 15,  0,  0, 12},
	{  0,  0,  6,  0, 12,  0,  0, 14,  0,  0,  0, 15,  0,  3,  0,  4},
	{  0,  0,  8,  3,  0,  0,  0,  6,  5,  1,  0,  0, 13,  0,  0,  0},
	{  3,  0,  0,  0,  0,  0, 12,  0,  8,  0,  0,  4,  0,  0, 16,  0},
	{ 10,  0, 13,  4,  0,  5,  0,  0, 11,  0,  0,  0,  0,  8,  0,  0},
	{  0, 12,  0,  2,  4,  8,  0,  0,  0,  0,  0,  5,  0, 13,  7,  0},
	{  0, 16,  1,  0,  7,  9,  3, 13, 14,  0, 10,  0,  0, 15,  0, 11}
};
#else
#define N 3
int x[][N * N] = {
	{ 5, 0, 0, 0, 7, 0, 0, 0, 0 },
	{ 6, 0, 0, 1, 9, 5, 0, 0, 0 },
	{ 0, 9, 8, 0, 0, 0, 0, 6, 0 },
	{ 8, 0, 0, 0, 6, 0, 0, 0, 3 },
	{ 4, 0, 0, 8, 0, 3, 0, 0, 1 },
	{ 7, 0, 0, 0, 2, 0, 0, 0, 6 },
	{ 0, 6, 0, 0, 0, 0, 2, 8, 0 },
	{ 0, 0, 0, 4, 1, 9, 0, 0, 5 },
	{ 0, 0, 0, 0, 8, 0, 0, 7, 9 }
};
#endif

#define W (N * N)

int ptr, used[W][W], bits[256];
struct { int *p, v; } rec[W * W * W * 3];

inline int countbits(int v)
{
	int c = 0;
	while (v) {
		c += bits[v & 255];
		v >>= 8;
	}
	return W - c;
}

void show()
{
	int i, j;
	for (i = 0; i < W; i++) {
		if (!(i % N)) putchar('\n');
		for (j = 0; j < W; j++)
			printf(j % N ? "%3d" : "%4d", x[i][j]);
		putchar('\n');
	}

	return;
}

void save(int *p, int v)
{
	rec[ptr].p = p, rec[ptr].v = *p;
	*p = v;
	ptr++;
}

void restore(int i)
{
	while (ptr > i) {
		--ptr;
		rec[ptr].p[0] = rec[ptr].v;
	}
}

int setcell(int row, int col, int v)
{
	int i, j, b = 1 << (v - 1), c, r;
	int cur = ptr;

	if (used[row][col] & (1 << (v - 1))) {
		restore(cur);
		return 0;
	}

	save(used[row] + col, (1 << W) - 1);
	save(x[row] + col, v);

	for (i = 0; i < W; i++)
		if (i != col && !(used[row][i] & b))
			save(used[row] + i, used[row][i] | b);

	for (i = 0; i < W; i++)
		if (i != row && !(used[i][col] & b))
			save(used[i] + col, used[i][col] | b);

	r = row / N * N;
	c = col / N * N;
	for (i = r; i < r + N; i++) {
		if (i == row) continue;
		for (j = c; j < c + N; j++)
			if (j != col && !(used[i][j] & b))
				save(used[i] + j, used[i][j] | b);
	}

	return 1;
}

int tryfill()
{
	int i, j, bi, bj, least = W + 1;
	int u, cur = ptr;

	for (i = 0; i < W; i++) {
		for (j = 0; j  < W; j++) {
			if (x[i][j]) continue;
			if (!(u = countbits(used[i][j]))) return 0;

			if (u < least) {
				least = u, bi = i, bj = j;
				if (least == 1) goto done;
			}
		}
	}

	if (least == W + 1) return 1;

done:	u = used[bi][bj];
	for (i = 0; i < W; i++) {
		if (u & (1 << i)) continue;

		setcell(bi, bj, i + 1);
		if (tryfill()) return 1;

		restore(cur);
	}
	return 0;
}

int solve()
{
	int i, j;
	for (i = 0; i < W; i++)
		for (j = 0; j < W; j++)
			if (x[i][j] && !setcell(i, j, x[i][j]))
				return 0;

	ptr = 0;
	tryfill();

	return 1;
}

void initbitcount()
{
	int v, i, b, c;
	for (v = 0; v < 256; v++) {
		for (b = 1, c = i = 0; i < 8; i++, b <<= 1)
			if (b & v) c++;
		bits[v] = c;
	}
}

int main(void)
{
	initbitcount();
	if (solve())
		show();
	else
		puts("no solution");

	return 0;
}
```

