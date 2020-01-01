+++
title = "Addition-chain exponentiation/Achain.c"
description = ""
date = 2011-09-08T03:31:51Z
aliases = []
[extra]
id = 10473
[taxonomies]
categories = []
tags = []
+++


```c
#include <stdio.h>
#include <stdlib.h>

/*  adapted from
        http://www-cs-faculty.stanford.edu/~knuth/programs/achain4.w
    with some severe loss of efficiency
 */
#define ASK_KNUTH 0
#if ASK_KNUTH
unsigned char knuth[20001] = {0};
#endif

#define N 32
#define NMAX 40000

/* u: upper bounds; l: lower bounds */
int	u[N] = {1, 2}, l[N] = {1, 2}, out[N], sum[N], tail[N],
	cache[NMAX + 1] = {0, 0, 1}, known = 2, stack;

typedef struct { int *p, v; } save_t;
save_t undo[N*N];
#define replace(u, l, n)\
	{ undo[stack].v = *(undo[stack].p = u + l); u[l] = n; stack++;}
#define restore(n)\
	while (stack > n) { stack--;*(undo[stack].p) = undo[stack].v; }

void show(int len)
{
	int i, n, j;
	printf("\033[H");
	for (i = 0; i <= len; i++) {
		n = l[i], j = 1;
		printf("%s%5d %5d\033[m %2d ",
			out[i] ? "\033[32m" : "", l[i], u[i], i);
		while (j <= n) {
			putchar((n & j) ? '#':'_');
			j <<= 1;
		}
		while (j < (1 << 16)) putchar(' '), j *= 2;
		n = u[i];
		for (j = 1; j <= n; j *= 2)
			putchar((n & j) ? '#':'_');

		while (j < (1 << 16)) putchar(' '), j*= 2;
		putchar('\n');
	}
}

/* lower and upper bounds */
int lower(int n, int *up)
{
	int i, o, q;
	if (n <= 2 || (n <= NMAX && cache[n])) {
		if (up) *up = cache[n];
		return cache[n];
	}

	for (i = -1, o = 0; n; n >>= 1, i++)
		if (n & 1) o++;

	if (up) *up = o + --i;
	do { i++; } while (o >>= 1);
	if (!up) return i;

	for (o = 2; o * o < n; o++) {
		if (n % o) continue;
		q = cache[o] + cache[n / o];
		if (q < *up)
			if ((*up = q) == i) break;
	}
	if (n > 2) {
		if (*up > cache[n-2] + 1) *up = cache[n-1] + 1;
		if (*up > cache[n-2] + 1) *up = cache[n-2] + 1;
	}

	return i;
}

int insert(int x, int pos)
{
	int save = stack, i, t;

	if (l[pos] > x || u[pos] < x) return 0;

	if (l[pos] == x) goto repl_u;
	replace(l, pos, x);
	for (i = pos - 1; u[i] * 2 < u[i+1]; i--) {
		if ((t = l[i+1] + 1) * 2 > u[i]) goto bail;
		replace(l, i, t);
	}

	for (i = pos + 1; l[i] <= l[i-1]; i++) {
		if ((t = l[i-1] + 1) > u[i]) goto bail;
		replace(l, i, t);
	}

repl_u:	if (u[pos] == x) return 1;
	replace(u, pos, x);
	for (i = pos - 1; u[i] >= u[i+1]; i--) {
		if ((t = u[i+1] - 1) < l[i]) goto bail;
		replace(u, i, t);
	}

	for (i = pos + 1; u[i] > u[i-1] * 2; i++) {
		if ((t = u[i-1] * 2) < l[i]) goto bail;
		replace(u, i, t);
	}

	return 1;

bail:	restore(save);
	return 0;
}

int seq_recur(int);
int seq_len(int);

inline int try_pq(int p, int q, int len) {
	int ps, qs, pl, ql, pu, qu, j;

	pl = cache[p];
	if (pl >= len) return 0;

	ql = cache[q];
	if (ql >= len) return 0;

	while (pl < len && u[pl] < p) pl++;
	for (pu = pl - 1; pu < len - 1 && u[pu + 1] >= p; pu++);

	while (ql < len && u[ql] < q) ql++;
	for (qu = ql - 1; qu < len - 1 && u[qu + 1] >= q; qu++);

	if (p != q && pl <= ql) pl = ql + 1;
	if (pl > pu || ql > qu || ql > pu) return 0;
	if (!out[len]) pl = pu = len - 1;

	ps = stack;

	for (; pu >= pl; pu--) {
		if (!insert(p, pu)) continue;
		out[pu]++, sum[pu] += len;
		if (p != q) {
			qs = stack;
			j = qu;
			if (j >= pu) j = pu - 1;
			for (; j >= ql; j--) {
				if (!insert(q, j)) continue;
				out[j]++, sum[j] += len, tail[len] = q;

				if (seq_recur(len - 1)) return 1;

				restore(qs);
				out[j]--, sum[j] -= len;
			}
		} else {
			out[pu]++, sum[pu] += len, tail[len] = p;

			if (seq_recur(len - 1)) return 1;

			out[pu]--, sum[pu] -= len;
		}

		out[pu]--, sum[pu] -= len;
		restore(ps);
	}
	return 0;
}

int seq_recur(int len)
{
	int p, q, n, limit;
	n = l[len];

	if (len < 2) return 1;

	limit = out[len] == 1 ? n - tail[sum[len]] : n - 1;
	if (limit > u[len - 1]) limit = u[len - 1];

	/* try to break n into p + q, and see if we can insert p, q into
	   list while satisfying bounds */
	for (p = limit, q = n - p; q <= p; q++, p--)
		if (try_pq(p, q, len)) return 1;

	return 0;
}

int seq(int n, int len, int *buf)
{
	int i;

	if (!len) len = seq_len(n);

	stack = 0;
	u[len] = l[len] = n;

	for (i = 0; i <= len; i++) out[i] = sum[i] = 0;
	for (i = 2; i < len; i++) {
		l[i] = l[i-1] + 1;
		u[i] = u[i-1] * 2;
	}
	for (i = len - 1; i > 2; i--) {
		if (l[i] * 2 < l[i+1]) l[i] = (1 + l[i+1]) / 2;
		if (u[i] >= u[i+1]) u[i] = u[i+1] - 1;
	}

	if (!seq_recur(len)) return 0;
	if (buf) for (i = 0; i <= len; i++) buf[i] = u[i];
	return len;
}

int seq_len(int n)
{
	int lb, ub;

	if (n <= known) return cache[n];

	/* need all lower n to compute sequence */
	while (known + 1 < n)
		seq_len(known + 1);

	lb = lower(n, &ub);
	while (lb < ub && !seq(n, lb, 0)) ++lb;

#if ASK_KNUTH
	if (n < 20000 && lb != knuth[n] - 32) {
		printf("bad %d: %d %d\n", n, lb, knuth[n] - 32);
		fflush(stdout);
		abort();
	}
#endif

	known = n;
	if (!(n & 1023))
		fprintf(stderr, "cached %d\n", known);

	return cache[n] = lb;
}

void init(void) {
#if ASK_KNUTH
	FILE *fp = fopen("out", "r");
	fread(knuth + 1, 1, 20000, fp);
	fclose(fp);
#endif
}
```

