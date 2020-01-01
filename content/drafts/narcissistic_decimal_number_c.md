+++
title = "Narcissistic decimal number/C"
description = ""
date = 2015-06-08T21:53:07Z
aliases = []
[extra]
id = 19236
[taxonomies]
categories = []
tags = []
+++

Find narcissistic numbers in bases 2 to 36. Pass the base on commandline argument, or have it default to base 10.  It finds all 88 base-10 numbers in a few seconds.

```c
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <gmp.h>

int base;
int number_width;
int trans[256] = {0};

int *chosen;
int *count_diff;
mpz_t **dpow;
mpz_t *total;
mpz_t max_total, min_total, top;

typedef struct { int d, cnt; } dc;
dc ** new_digits;

#define ORD(x) trans[(unsigned char)str_total[x]]
#define ALLOC(x, len) x = malloc(sizeof(*x)*(len))
#define ZERO(p, len) memset(p, 0, sizeof(*p)*(len))

char *str_top, *str_total;
void show_result(mpz_t z) {
	static int cnt = 0;
	if (base != 10)
		gmp_printf("%d: %s = %Zd (10)\n", ++cnt, str_total, z);
	else
		gmp_printf("%d: %s\n", ++cnt, str_total);
}

// check that digit power sum and chosen digits have same set of digits
void verify(mpz_t z)
{
	int count[base];
	ZERO(count, base);

	mpz_get_str(str_total, base, z);
	for (int i = 0; i < number_width; i++)
		++count[ORD(i)];

	// ignore counts on digit 0; it doesn't affect the sum
	for (int i = base - 1; i; i--)
		if (count[i] != chosen[i])
			return;

	show_result(z);
}

static inline int diff_is_bust(int d)
{
	for (int i = d; i < base; i++)
		if (count_diff[i] < 0) return 1;
	return 0;
}

// Currently selecting digit 'd', need 'need' more digits.
// Given chosen digits so far, see if the sum of digits powers
// can be eliminated early.
void extend(int d, int need, int fixed)
{
#define T total[d-1]
#define S total[d]

	if (!d || !need) {
		verify(S);
		return;
	}

	const int old_d = chosen[d]; // save the existing count of d
	for (int n = 0; n <= need; n++) {
		int rem = need - n;
		if (n) chosen[d]++;

		// add new d's to current power sum
		mpz_add(T, S, dpow[d][n]);
		if (mpz_cmp(T, max_total) >= 0) break;

		if (!rem) {
			verify(T);
			break;
		}

		// after selecting n d's, the biggest sum we can possibly get
		// is by filling the remaining blanks with d-1
		mpz_add(top, T, dpow[d-1][rem]);
		if (mpz_cmp(top, min_total) < 0) continue;

		// a homebrew MP integer what uses the base natively is slower
		// in addtions, but much faster in retrieving digits.  Overall it
		// would be faster, but code would be much longer (I tried).

		ZERO(count_diff, base);
		mpz_get_str(str_top, base, top);
		mpz_get_str(str_total, base, T);

		if (str_top[fixed] != str_total[fixed]) {
			// no new fixed digit
			extend(d - 1, rem, fixed);
			continue;
		}

		// Count how many leading digits are common between total and top;
		// these digits can't change regardless of subsequent recursions.
		int f;
		for (f = 0; f < number_width && str_top[f] == str_total[f]; f++)
			++count_diff[ORD(f)];

		for (int i = 0; i < base; i++)
			count_diff[i] = chosen[i] - count_diff[i];

		// More digits >= d than we have chosen, and we've
		// already done selecting them, so no need to continue
		if (diff_is_bust(d)) continue;

		// Add newly fixed digits into selection, and remember them
		// for later restoration
		dc* const add = new_digits[d];
		dc* end = add;

		for (int i = 0; i < d; i++) {
			if (count_diff[i] < 0) {
				end->d = i;
				end->cnt = -count_diff[i];
				++end;
				rem += count_diff[i];
			}
		}

		// Don't have space for new lower digits, next.
		// It seems using a 'break' here has no ill effect, but I can't
		// justify it.
		if (rem < 0) continue;

		// add new digits to power sum
		for (dc* a = add; a != end; a++) {
			chosen[a->d] += a->cnt;
			mpz_add(T, T, dpow[a->d][a->cnt]);
		}

		extend(d - 1, rem, f);

		// restore digit selection
		for (dc *a = add; a != end; a++)
			chosen[a->d] -= a->cnt;
	}
	chosen[d] = old_d;

#undef T
#undef S
}

void solve(int w)
{
	number_width = w;

	// initialize values for current width
	for (int b = 0; b < base; b++) {
		mpz_set_ui(dpow[b][0], 0);
		mpz_ui_pow_ui(dpow[b][1], b, w);
		for (int p = 2; p <= w; p++)
			mpz_mul_ui(dpow[b][p], dpow[b][1], p);
	}
	mpz_ui_pow_ui(max_total, base, w);
	mpz_ui_pow_ui(min_total, base, w - 1);

	mpz_set_ui(total[base - 1], 0);
	extend(base - 1, w, 0);
}

int main(int argc, char **argv)
{
	if (argc < 2 || (base = atoi(argv[1])) < 2 || base > 36)
		base = 10;

	for (int i = '0'; i <= '9'; i++) trans[i] = i - '0';
	for (int i = 'a'; i <= 'z'; i++) trans[i] = i + 10 - 'a';

	int k; // max length of numbers
	for (k = 1; log(k) + k*log(base - 1) >= (k-1)*log(base); k++);

	ALLOC(chosen, base);
	ALLOC(count_diff, base);
	ALLOC(str_top, k+1);
	ALLOC(str_total, k+1);
	ALLOC(total, base);
	ALLOC(dpow, base);

	for (int i = 0; i < base; i++) {
		ALLOC(dpow[i], k);
		for (int j = 0; j < k; j++)
			mpz_init(dpow[i][j]);
		mpz_init(total[i]);
	}

	ALLOC(new_digits, base);
	for (int i = 0; i < base; i++)
		ALLOC(new_digits[i], base);

	mpz_init(max_total);
	mpz_init(min_total);
	mpz_init(top);

	ZERO(chosen, base);
	for (int w = 1; w < k; w++)
		solve(w);

	return 0;
}
```

