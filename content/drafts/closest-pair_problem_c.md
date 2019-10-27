+++
title = "Closest-pair problem/C"
description = ""
date = 2013-04-20T01:47:08Z
aliases = []
[extra]
id = 5321
[taxonomies]
categories = []
tags = []
+++

{{collection|Closest pair problem}}


```C>#include <stdio.h

#include <stdlib.h>
#include <values.h>
#include <math.h>
#include <string.h>

typedef struct { double x, y; } point_t, *point;

inline double dist(point a, point b)
{
        double dx = a->x - b->x, dy = a->y - b->y;
        return dx * dx + dy * dy;
}

inline int cmp_dbl(double a, double b)
{
        return a < b ? -1 : a > b ? 1 : 0;
}

int cmp_x(const void *a, const void *b) {
        return cmp_dbl( (*(const point*)a)->x, (*(const point*)b)->x );
}

int cmp_y(const void *a, const void *b) {
        return cmp_dbl( (*(const point*)a)->y, (*(const point*)b)->y );
}

double brute_force(point* pts, int max_n, point *a, point *b)
{
        int i, j;
        double d, min_d = MAXDOUBLE;

        for (i = 0; i < max_n; i++) {
                for (j = i + 1; j < max_n; j++) {
                        d = dist(pts[i], pts[j]);
                        if (d >= min_d ) continue;
                        *a = pts[i];
                        *b = pts[j];
                        min_d = d;
                }
        }
        return min_d;
}

double closest(point* sx, int nx, point* sy, int ny, point *a, point *b)
{
        int left, right, i;
        double d, min_d, x0, x1, mid, x;
        point a1, b1;
        point *s_yy;

        if (nx <= 8) return brute_force(sx, nx, a, b);

        s_yy  = malloc(sizeof(point) * ny);
        mid = sx[nx/2]->x;

        /* adding points to the y-sorted list; if a point's x is less than mid,
           add to the begining; if more, add to the end backwards, hence the
           need to reverse it */
        left = -1; right = ny;
        for (i = 0; i < ny; i++)
                if (sy[i]->x < mid) s_yy[++left] = sy[i];
                else                s_yy[--right]= sy[i];

        /* reverse the higher part of the list */
        for (i = ny - 1; right < i; right ++, i--) {
                a1 = s_yy[right]; s_yy[right] = s_yy[i]; s_yy[i] = a1;
        }

        min_d = closest(sx, nx/2, s_yy, left + 1, a, b);
        d = closest(sx + nx/2, nx - nx/2, s_yy + left + 1, ny - left - 1, &a1, &b1);

        if (d < min_d) { min_d = d; *a = a1; *b = b1; }
        d = sqrt(min_d);

        /* get all the points within distance d of the center line */
        left = -1; right = ny;
        for (i = 0; i < ny; i++) {
                x = sy[i]->x - mid;
                if (x <= -d || x >= d) continue;

                if (x < 0) s_yy[++left]  = sy[i];
                else       s_yy[--right] = sy[i];
        }

        /* compare each left point to right point */
        while (left >= 0) {
                x0 = s_yy[left]->y + d;

                while (right < ny && s_yy[right]->y > x0) right ++;
                if (right >= ny) break;

                x1 = s_yy[left]->y - d;
                for (i = right; i < ny && s_yy[i]->y > x1; i++)
                        if ((x = dist(s_yy[left], s_yy[i])) < min_d) {
                                min_d = x;
                                d = sqrt(min_d);
                                *a = s_yy[left];
                                *b = s_yy[i];
                        }

                left --;
        }

        free(s_yy);
        return min_d;
}

#define NP 1000000
int main()
{
        int i;
        point a, b;

        point pts  = malloc(sizeof(point_t) * NP);
        point* s_x = malloc(sizeof(point) * NP);
        point* s_y = malloc(sizeof(point) * NP);

        for(i = 0; i < NP; i++) {
                s_x[i] = pts + i;
                pts[i].x = 100 * (double) rand()/RAND_MAX;
                pts[i].y = 100 * (double) rand()/RAND_MAX;
        }

/*      printf("brute force: %g, ", sqrt(brute_force(s_x, NP, &a, &b)));
        printf("between (%f,%f) and (%f,%f)\n", a->x, a->y, b->x, b->y);        */

        memcpy(s_y, s_x, sizeof(point) * NP);
        qsort(s_x, NP, sizeof(point), cmp_x);
        qsort(s_y, NP, sizeof(point), cmp_y);

        printf("min: %g; ", sqrt(closest(s_x, NP, s_y, NP, &a, &b)));
        printf("point (%f,%f) and (%f,%f)\n", a->x, a->y, b->x, b->y);

        /* not freeing the memory, let OS deal with it.  Habit. */
        return 0;
}
```
Compile and run (1000000 points, bruteforce method not shown because it takes forever):<lang>
% gcc -Wall -lm -O2 test.c
% time ./a.out
min: 5.6321e-05; point (19.657247,79.900855) and (19.657303,79.900862)
./a.out  7.16s user 0.08s system 96% cpu 7.480 total

```

