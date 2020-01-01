+++
title = "Example:Hough transform/C"
description = ""
date = 2013-03-30T15:29:16Z
aliases = []
[extra]
id = 8042
[taxonomies]
categories = []
tags = []
+++

=={{Programming-example-page|Hough transform|language=C}}==
{{trans|Tcl}}

(Tested only with the pentagon image given)

```c
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <math.h>

#include <cairo.h>

#ifndef M_PI
#define M_PI 3.1415927
#endif

#define GR(X,Y) (d[(*s)*(Y)+bpp*(X)+((2)%bpp)])
#define GG(X,Y) (d[(*s)*(Y)+bpp*(X)+((1)%bpp)])
#define GB(X,Y) (d[(*s)*(Y)+bpp*(X)+((0)%bpp)])
#define SR(X,Y) (ht[4*tw*((Y)%th)+4*((X)%tw)+2])
#define SG(X,Y) (ht[4*tw*((Y)%th)+4*((X)%tw)+1])
#define SB(X,Y) (ht[4*tw*((Y)%th)+4*((X)%tw)+0])
#define RAD(A)  (M_PI*((double)(A))/180.0)
uint8_t *houghtransform(uint8_t *d, int *w, int *h, int *s, int bpp)
{
  int rho, theta, y, x, W = *w, H = *h;
  int th = sqrt(W*W + H*H)/2.0;
  int tw = 360;
  uint8_t *ht = malloc(th*tw*4);
  memset(ht, 0, 4*th*tw); // black bg

  for(rho = 0; rho < th; rho++)
  {
    for(theta = 0; theta < tw/*720*/; theta++)
    {
      double C = cos(RAD(theta));
      double S = sin(RAD(theta));
      uint32_t totalred = 0;
      uint32_t totalgreen = 0;
      uint32_t totalblue = 0;
      uint32_t totalpix = 0;
      if ( theta < 45 || (theta > 135 && theta < 225) || theta > 315) {
	for(y = 0; y < H; y++) {
	  double dx = W/2.0 + (rho - (H/2.0-y)*S)/C;
	  if ( dx < 0 || dx >= W ) continue;
	  x = floor(dx+.5);
	  if (x == W) continue;
	  totalpix++;
	  totalred += GR(x, y);
	  totalgreen += GG(x, y);
	  totalblue += GB(x, y);
	}
      } else {
	for(x = 0; x < W; x++) {
	  double dy = H/2.0 - (rho - (x - W/2.0)*C)/S;
	  if ( dy < 0 || dy >= H ) continue;
	  y = floor(dy+.5);
	  if (y == H) continue;
	  totalpix++;
	  totalred += GR(x, y);
	  totalgreen += GG(x, y);
	  totalblue += GB(x, y);
	}
      }
      if ( totalpix > 0 ) {
	double dp = totalpix;
	SR(theta, rho) = (int)(totalred/dp)   &0xff;
	SG(theta, rho) = (int)(totalgreen/dp) &0xff;
	SB(theta, rho) = (int)(totalblue/dp)  &0xff;
      }
    }
  }

  *h = th;   // sqrt(W*W+H*H)/2
  *w = tw;   // 360
  *s = 4*tw;
  return ht;
}

int main(int argc, char **argv)
{
  cairo_surface_t *inputimg = NULL;
  cairo_surface_t *houghimg = NULL;

  uint8_t *houghdata = NULL, *inputdata = NULL;
  int w, h, s, bpp;

  if ( argc < 3 ) return EXIT_FAILURE;

  inputimg = cairo_image_surface_create_from_png(argv[1]);

  w = cairo_image_surface_get_width(inputimg);
  h = cairo_image_surface_get_height(inputimg);
  s = cairo_image_surface_get_stride(inputimg);
  bpp = cairo_image_surface_get_format(inputimg);
  switch(bpp)
  {
  case CAIRO_FORMAT_ARGB32: bpp = 4; break;
  case CAIRO_FORMAT_RGB24:  bpp = 3; break;
  case CAIRO_FORMAT_A8:     bpp = 1; break;
  default:
    fprintf(stderr, "unsupported\n");
    goto destroy;
  }

  inputdata = cairo_image_surface_get_data(inputimg);
  houghdata = houghtransform(inputdata, &w, &h, &s, bpp);

  printf("w=%d, h=%d\n", w, h);
  houghimg = cairo_image_surface_create_for_data(houghdata,
						 CAIRO_FORMAT_RGB24,
						 w, h, s);
  cairo_surface_write_to_png(houghimg, argv[2]);

destroy:
  if (inputimg != NULL) cairo_surface_destroy(inputimg);
  if (houghimg != NULL) cairo_surface_destroy(houghimg);

  return EXIT_SUCCESS;
}
```


Output image (but with white background):

[[Image:Houghtrasf-c.png|thumb|left|360x200px|Output image when input is the given Pentagon.]]
<br style="clear:both" />


###  Alternative version


[[file:penta-hugh.png|thumb]][[file:hugh-lines-in.png|thumb]][[file:hugh-lines-out.png|thumb]]
This code is a little to long to my liking, because I had to put some ad hoc stuff that should be better served by libraries.  But you don't want to see libpng code here, trust me.

```c
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <ctype.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <err.h>
#include <math.h>

/* start of utility functions: not interesting */
typedef unsigned char uchar;
typedef unsigned long ulong;
typedef struct intensity_t {
	double **pix;
	long width, height;
} *intensity;

double PI;

#define decl_array_alloc(type) \
type ** type##_array(long w, long h) {			\
	int i;						\
	type ** row = malloc(sizeof(type*) * h);	\
	type *  pix = malloc(sizeof(type) * h * w);	\
	for (i = 0; i < h; i++)				\
		row[i] = pix + w * i;			\
	memset(pix, 0, sizeof(type) * h * w);		\
	return row;					\
}

decl_array_alloc(double);
decl_array_alloc(ulong);

intensity intensity_alloc(long w, long h)
{
	intensity x = malloc(sizeof(struct intensity_t));
	x->width = w;
	x->height = h;
	x->pix = double_array(w, h);

	return x;
}

long get_num(uchar **p, uchar *buf_end)
{
	uchar *ptr = *p, *tok_end;
	long tok;
	while (1) {
		while (ptr < buf_end && isspace(*ptr)) ptr++;
		if (ptr >= buf_end) return 0;

		if (*ptr == '#') { /* ignore comment */
			while (ptr++ < buf_end) {
				if (*ptr == '\n' || *ptr == '\r') break;
			}
			continue;
		}

		tok = strtol((char*)ptr, (char**)&tok_end, 10);
		if (tok_end == ptr) return 0;
		*p = tok_end;
		return tok;
	}
	return 0;
}

/* Note: not robust. A robust version would be to long for example code */
intensity read_pnm(char *name)
{
	struct stat st;
	uchar *fbuf, *ptr, *end;
	long width, height, max_val;
	int i, j;
	intensity ret;

	int fd = open(name, O_RDONLY);
	if (fd == -1) err(1, "Can't open %s", name);

	/* from now on assume all operations succeed */
	fstat(fd, &st);
	fbuf = malloc(st.st_size + 1);
	read(fd, fbuf, st.st_size);
	*(end = fbuf + st.st_size) = '\0';
	close(fd);

	if (fbuf[0] != 'P' || (fbuf[1] != '5' && fbuf[1] != '6') || !isspace(fbuf[2]))
		err(1, "%s: bad format: can only do P5 or P6 pnm", name);

	ptr = fbuf + 3;
	width   = get_num(&ptr, end);
	height  = get_num(&ptr, end);
	max_val = get_num(&ptr, end);
	if (max_val <= 0 || max_val >= 256)
		err(1, "Can't handle pixel value %ld\n", max_val);

	fprintf(stderr, "[Info] format: P%c w: %ld h: %ld value: %ld\n",
		fbuf[1], width, height, max_val);

	ret = intensity_alloc(width, height);
	ptr ++;	/* ptr should be pointint at the first pixel byte now */

	if (fbuf[1] == '5') {	/* graymap, 1 byte per pixel */
		for (i = 0; i < height; i++) {
			for (j = 0; j < width; j++) {
				ret->pix[i][j] = (double)*(ptr++) / max_val;
			}
		}
	} else {		/* pnm, 1 byte each for RGB */
		/* hocus pocus way of getting lightness from RGB for us */
		for (i = 0; i < height; i++) {
			for (j = 0; j < width; j++) {
				ret->pix[i][j] = (ptr[0] * 0.2126 +
						  ptr[1] * 0.7152 +
						  ptr[2] * 0.0722) / max_val;
				ptr += 3;
			}
		}
	}

	free(fbuf);
	return ret;
}

void write_pgm(double **pix, long w, long h)
{
	long i, j;
	unsigned char *ptr, *buf = malloc(sizeof(double) * w * h);
	char header[1024];
	sprintf(header, "P5\n%ld %ld\n255\n", w, h);

	ptr = buf;
	for (i = 0; i < h; i++)
		for (j = 0; j < w; j++)
			*(ptr++) = 256 * pix[i][j];

	write(fileno(stdout), header, strlen(header));
	write(fileno(stdout), buf, w * h);

	free(buf);
}

/* Finally, end of util functions.  All that for this function. */
intensity hugh_transform(intensity in, double gamma)
{
	long i, j, k, l, m, w, h;
	double bg, r_res, t_res, rho, r, theta, x, y, v, max_val, min_val, *pp;
	intensity graph;

	/* before anything else, legalize Pi = 3 */
	PI = atan2(1, 1) * 4;

	/* first, run through all pixels and see what the average is,
	 * so we can take a guess if the background is black or white.
	 * a real application wouldn't do silly things like this	*/
	for (i = 0, bg = 0; i < in->height; i++)
		for (j = 0; j < in->width; j++)
			bg += in->pix[i][j];
	fprintf(stderr, "[info] background is %f\n", bg);
	bg = (bg /= (in->height * in->width) > 0.5) ? 1 : 0;

	/* if white, invert it */
	if (bg) {
		for (i = 0; i < in->height; i++)
			for (j = 0; j < in->width; j++)
				in->pix[i][j] = 1 - in->pix[i][j];
	}

	/* second, decide what resolution of rho and theta should be.
	 * here we just make the rho/theta graph a fixed ratio
	 * of input, which is dumb.  It should depend on the application.
	 * finer bins allow better resolution between lines, but will
	 * lose contrast if the input is noisy.  Also, lower resolution, faster.
	 */
#	define RRATIO 1.5
#	define TRATIO 1.5
	x = in->width - .5;
	y = in->height - .5;
	r = sqrt(x * x + y * y) / 2;

	w = in->width / TRATIO;
	h = in->height / RRATIO;
	r_res = r / h;
	t_res = PI * 2 / w;

	graph = intensity_alloc(w, h);

	for (i = 0; i < in->height; i++) {
		y = i - in->height / 2. + .5;
		for (j = 0; j < in->width; j++) {
			x = j - in->width / 2 + .5;
			r = sqrt(x * x + y * y);
			v = in->pix[i][j];

			/* hackery: sample image is mostly blank, this saves a great
			 * deal of time.  Doesn't help a lot with noisy images */
			if (!v) continue;

			/* at each pixel, check what lines it could be on */
			for (k = 0; k < w; k++) {
				theta = k * t_res - PI;
				rho = x * cos(theta) + y * sin(theta);
				if (rho >= 0) {
					m = rho / r_res;
					l = k;
				} else {
					m = -rho / r_res;
					l = (k + w/2.);
					l %= w;
				}
				graph->pix[m][l] += v * r;
			}
		}
		/* show which row we are precessing lest user gets bored */
		fprintf(stderr, "\r%ld", i);
	}
	fprintf(stderr, "\n");

	max_val = 0;
	min_val = 1e100;
	pp = &(graph->pix[graph->height - 1][graph->width - 1]);
	for (i = graph->height * graph->width - 1; i >= 0; i--, pp--) {
		if (max_val < *pp) max_val = *pp;
		if (min_val > *pp) min_val = *pp;
	}

	/* gamma correction. if gamma > 1, output contrast is better, noise
	   is suppressed, but spots for thin lines may be lost; if gamma < 1,
	   everything is brighter, both lines and noises */
	pp = &(graph->pix[graph->height - 1][graph->width - 1]);
	for (i = graph->height * graph->width - 1; i >= 0; i--, pp--) {
		*pp = pow((*pp - min_val)/ (max_val - min_val), gamma);
	}

	return graph;
}

int main()
{
	//intensity in = read_pnm("pent.pnm");
	intensity in = read_pnm("lines.pnm");
	intensity out = hugh_transform(in, 1.5);

	/* binary output goes straight to stdout, get ready to see garbage on your
	 * screen if you are not careful!
	 */
	write_pgm(out->pix, out->width, out->height);

        /* not going to free memory we used: OS can deal with it */
	return 0;
}
```

This program takes a pnm file (binary, either P5 or P6) and does the transformation, then dump output onto stdout.  Sample images below are output from the pentagram; sample lines with added noise; output of processing that.  Both output were with 1.5 gamma.
