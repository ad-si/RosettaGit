+++
title = "Canny edge detector"
description = ""
date = 2018-12-21T15:22:20Z
aliases = []
[extra]
id = 11471
[taxonomies]
categories = ["Image processing", "Computer vision", "task"]
tags = []
+++

## Task

Write a program that performs so-called [[wp:Canny edge detector|canny edge detection]] on an image.

A possible algorithm consists of the following steps:

# '''Noise reduction.''' May be performed by [[wp:Gaussian blur|Gaussian filter]].
# Compute '''intensity gradient''' (matrices <math>G_x</math> and <math>G_y</math>) and its '''magnitude''' <math>G</math>.
   <math>G=\sqrt{G_x^2+G_y^2}</math>
 May be performed by [[image convolution|convolution of an image]] with [[wp:Sobel operator|Sobel operators]].
# '''Non-maximum suppression.''' For each pixel compute the orientation of intensity gradient vector: <math>\theta = {\rm atan2}\left(G_y, \, G_x\right)</math>. Transform angle <math>\theta</math> to one of four directions: 0, 45, 90, 135 degrees. Compute new array <math>N</math>: if
   <math>G\left(p_a\right)<G\left(p\right)<G\left(p_b\right)</math>
where <math>p</math> is the current pixel, <math>p_a</math> and <math>p_b</math> are the two neighbour pixels in the direction of gradient, then <math>N(p) = G(p)</math>, otherwise <math>N(p) = 0</math>. Nonzero pixels in resulting array correspond to local maxima of <math>G</math> in direction <math>\theta(p)</math>.
# '''Tracing edges with hysteresis.''' At this stage two thresholds for the values of <math>G</math> are introduced: <math>T_{min}</math> and <math>T_{max}</math>. Starting from pixels with <math>N(p) \geqslant T_{max}</math> find all paths of pixels with <math>N(p) \geqslant T_{min}</math> and put them to the resulting image.


## C

The following program reads an 8 bits per pixel grayscale [[wp:BMP file format|BMP]] file and saves the result to `out.bmp'. Compile with `-lm'.

```c
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <float.h>
#include <math.h>
#include <string.h>
#include <stdbool.h>
#include <assert.h>

#define MAX_BRIGHTNESS 255

// C99 doesn't define M_PI (GNU-C99 does)
#define M_PI 3.14159265358979323846264338327

/*
 * Loading part taken from
 * http://www.vbforums.com/showthread.php?t=261522
 * BMP info:
 * http://en.wikipedia.org/wiki/BMP_file_format
 *
 * Note: the magic number has been removed from the bmpfile_header_t
 * structure since it causes alignment problems
 *     bmpfile_magic_t should be written/read first
 * followed by the
 *     bmpfile_header_t
 * [this avoids compiler-specific alignment pragmas etc.]
 */

typedef struct {
    uint8_t magic[2];
} bmpfile_magic_t;

typedef struct {
    uint32_t filesz;
    uint16_t creator1;
    uint16_t creator2;
    uint32_t bmp_offset;
} bmpfile_header_t;

typedef struct {
    uint32_t header_sz;
    int32_t  width;
    int32_t  height;
    uint16_t nplanes;
    uint16_t bitspp;
    uint32_t compress_type;
    uint32_t bmp_bytesz;
    int32_t  hres;
    int32_t  vres;
    uint32_t ncolors;
    uint32_t nimpcolors;
} bitmap_info_header_t;

typedef struct {
    uint8_t r;
    uint8_t g;
    uint8_t b;
    uint8_t nothing;
} rgb_t;

// Use short int instead `unsigned char' so that we can
// store negative values.
typedef short int pixel_t;

pixel_t *load_bmp(const char *filename,
                  bitmap_info_header_t *bitmapInfoHeader)
{
    FILE *filePtr = fopen(filename, "rb");
    if (filePtr == NULL) {
        perror("fopen()");
        return NULL;
    }

    bmpfile_magic_t mag;
    if (fread(&mag, sizeof(bmpfile_magic_t), 1, filePtr) != 1) {
        fclose(filePtr);
        return NULL;
    }

    // verify that this is a bmp file by check bitmap id
    // warning: dereferencing type-punned pointer will break
    // strict-aliasing rules [-Wstrict-aliasing]
    if (*((uint16_t*)mag.magic) != 0x4D42) {
        fprintf(stderr, "Not a BMP file: magic=%c%c\n",
                mag.magic[0], mag.magic[1]);
        fclose(filePtr);
        return NULL;
    }

    bmpfile_header_t bitmapFileHeader; // our bitmap file header
    // read the bitmap file header
    if (fread(&bitmapFileHeader, sizeof(bmpfile_header_t),
              1, filePtr) != 1) {
        fclose(filePtr);
        return NULL;
    }

    // read the bitmap info header
    if (fread(bitmapInfoHeader, sizeof(bitmap_info_header_t),
              1, filePtr) != 1) {
        fclose(filePtr);
        return NULL;
    }

    if (bitmapInfoHeader->compress_type != 0)
        fprintf(stderr, "Warning, compression is not supported.\n");

    // move file point to the beginning of bitmap data
    if (fseek(filePtr, bitmapFileHeader.bmp_offset, SEEK_SET)) {
        fclose(filePtr);
        return NULL;
    }

    // allocate enough memory for the bitmap image data
    pixel_t *bitmapImage = malloc(bitmapInfoHeader->bmp_bytesz *
                                  sizeof(pixel_t));

    // verify memory allocation
    if (bitmapImage == NULL) {
        fclose(filePtr);
        return NULL;
    }

    // read in the bitmap image data
    size_t pad, count=0;
    unsigned char c;
    pad = 4*ceil(bitmapInfoHeader->bitspp*bitmapInfoHeader->width/32.) - bitmapInfoHeader->width;
    for(size_t i=0; i<bitmapInfoHeader->height; i++){
	    for(size_t j=0; j<bitmapInfoHeader->width; j++){
		    if (fread(&c, sizeof(unsigned char), 1, filePtr) != 1) {
			    fclose(filePtr);
			    return NULL;
		    }
		    bitmapImage[count++] = (pixel_t) c;
	    }
	    fseek(filePtr, pad, SEEK_CUR);
    }

    // If we were using unsigned char as pixel_t, then:
    // fread(bitmapImage, 1, bitmapInfoHeader->bmp_bytesz, filePtr);

    // close file and return bitmap image data
    fclose(filePtr);
    return bitmapImage;
}

// Return: true on error.
bool save_bmp(const char *filename, const bitmap_info_header_t *bmp_ih,
              const pixel_t *data)
{
    FILE* filePtr = fopen(filename, "wb");
    if (filePtr == NULL)
        return true;

    bmpfile_magic_t mag = {{0x42, 0x4d}};
    if (fwrite(&mag, sizeof(bmpfile_magic_t), 1, filePtr) != 1) {
        fclose(filePtr);
        return true;
    }

    const uint32_t offset = sizeof(bmpfile_magic_t) +
                            sizeof(bmpfile_header_t) +
                            sizeof(bitmap_info_header_t) +
                            ((1U << bmp_ih->bitspp) * 4);

    const bmpfile_header_t bmp_fh = {
        .filesz = offset + bmp_ih->bmp_bytesz,
        .creator1 = 0,
        .creator2 = 0,
        .bmp_offset = offset
    };

    if (fwrite(&bmp_fh, sizeof(bmpfile_header_t), 1, filePtr) != 1) {
        fclose(filePtr);
        return true;
    }
    if (fwrite(bmp_ih, sizeof(bitmap_info_header_t), 1, filePtr) != 1) {
        fclose(filePtr);
        return true;
    }

    // Palette
    for (size_t i = 0; i < (1U << bmp_ih->bitspp); i++) {
        const rgb_t color = {(uint8_t)i, (uint8_t)i, (uint8_t)i};
        if (fwrite(&color, sizeof(rgb_t), 1, filePtr) != 1) {
            fclose(filePtr);
            return true;
        }
    }

    // We use int instead of uchar, so we can't write img
    // in 1 call any more.
    // fwrite(data, 1, bmp_ih->bmp_bytesz, filePtr);

    // Padding: http://en.wikipedia.org/wiki/BMP_file_format#Pixel_storage
    size_t pad = 4*ceil(bmp_ih->bitspp*bmp_ih->width/32.) - bmp_ih->width;
    unsigned char c;
    for(size_t i=0; i < bmp_ih->height; i++) {
	    for(size_t j=0; j < bmp_ih->width; j++) {
		    c = (unsigned char) data[j + bmp_ih->width*i];
		    if (fwrite(&c, sizeof(char), 1, filePtr) != 1) {
			    fclose(filePtr);
			    return true;
		    }
	    }
	    c = 0;
	    for(size_t j=0; j<pad; j++)
		    if (fwrite(&c, sizeof(char), 1, filePtr) != 1) {
			    fclose(filePtr);
			    return true;
		    }
    }

    fclose(filePtr);
    return false;
}

// if normalize is true, map pixels to range 0..MAX_BRIGHTNESS
void convolution(const pixel_t *in, pixel_t *out, const float *kernel,
                 const int nx, const int ny, const int kn,
                 const bool normalize)
{
    assert(kn % 2 == 1);
    assert(nx > kn && ny > kn);
    const int khalf = kn / 2;
    float min = FLT_MAX, max = -FLT_MAX;

    if (normalize)
        for (int m = khalf; m < nx - khalf; m++)
            for (int n = khalf; n < ny - khalf; n++) {
                float pixel = 0.0;
                size_t c = 0;
                for (int j = -khalf; j <= khalf; j++)
                    for (int i = -khalf; i <= khalf; i++) {
                        pixel += in[(n - j) * nx + m - i] * kernel[c];
                        c++;
                    }
                if (pixel < min)
                    min = pixel;
                if (pixel > max)
                    max = pixel;
                }

    for (int m = khalf; m < nx - khalf; m++)
        for (int n = khalf; n < ny - khalf; n++) {
            float pixel = 0.0;
            size_t c = 0;
            for (int j = -khalf; j <= khalf; j++)
                for (int i = -khalf; i <= khalf; i++) {
                    pixel += in[(n - j) * nx + m - i] * kernel[c];
                    c++;
                }

            if (normalize)
                pixel = MAX_BRIGHTNESS * (pixel - min) / (max - min);
            out[n * nx + m] = (pixel_t)pixel;
        }
}

/*
 * gaussianFilter:
 * http://www.songho.ca/dsp/cannyedge/cannyedge.html
 * determine size of kernel (odd #)
 * 0.0 <= sigma < 0.5 : 3
 * 0.5 <= sigma < 1.0 : 5
 * 1.0 <= sigma < 1.5 : 7
 * 1.5 <= sigma < 2.0 : 9
 * 2.0 <= sigma < 2.5 : 11
 * 2.5 <= sigma < 3.0 : 13 ...
 * kernelSize = 2 * int(2*sigma) + 3;
 */
void gaussian_filter(const pixel_t *in, pixel_t *out,
                     const int nx, const int ny, const float sigma)
{
    const int n = 2 * (int)(2 * sigma) + 3;
    const float mean = (float)floor(n / 2.0);
    float kernel[n * n]; // variable length array

    fprintf(stderr, "gaussian_filter: kernel size %d, sigma=%g\n",
            n, sigma);
    size_t c = 0;
    for (int i = 0; i < n; i++)
        for (int j = 0; j < n; j++) {
            kernel[c] = exp(-0.5 * (pow((i - mean) / sigma, 2.0) +
                                    pow((j - mean) / sigma, 2.0)))
                        / (2 * M_PI * sigma * sigma);
            c++;
        }

    convolution(in, out, kernel, nx, ny, n, true);
}

/*
 * Links:
 * http://en.wikipedia.org/wiki/Canny_edge_detector
 * http://www.tomgibara.com/computer-vision/CannyEdgeDetector.java
 * http://fourier.eng.hmc.edu/e161/lectures/canny/node1.html
 * http://www.songho.ca/dsp/cannyedge/cannyedge.html
 *
 * Note: T1 and T2 are lower and upper thresholds.
 */
pixel_t *canny_edge_detection(const pixel_t *in,
                              const bitmap_info_header_t *bmp_ih,
                              const int tmin, const int tmax,
                              const float sigma)
{
    const int nx = bmp_ih->width;
    const int ny = bmp_ih->height;

    pixel_t *G = calloc(nx * ny * sizeof(pixel_t), 1);
    pixel_t *after_Gx = calloc(nx * ny * sizeof(pixel_t), 1);
    pixel_t *after_Gy = calloc(nx * ny * sizeof(pixel_t), 1);
    pixel_t *nms = calloc(nx * ny * sizeof(pixel_t), 1);
    pixel_t *out = malloc(bmp_ih->bmp_bytesz * sizeof(pixel_t));

    if (G == NULL || after_Gx == NULL || after_Gy == NULL ||
        nms == NULL || out == NULL) {
        fprintf(stderr, "canny_edge_detection:"
                " Failed memory allocation(s).\n");
        exit(1);
    }

    gaussian_filter(in, out, nx, ny, sigma);

    const float Gx[] = {-1, 0, 1,
                        -2, 0, 2,
                        -1, 0, 1};

    convolution(out, after_Gx, Gx, nx, ny, 3, false);

    const float Gy[] = { 1, 2, 1,
                         0, 0, 0,
                        -1,-2,-1};

    convolution(out, after_Gy, Gy, nx, ny, 3, false);

    for (int i = 1; i < nx - 1; i++)
        for (int j = 1; j < ny - 1; j++) {
            const int c = i + nx * j;
            // G[c] = abs(after_Gx[c]) + abs(after_Gy[c]);
            G[c] = (pixel_t)hypot(after_Gx[c], after_Gy[c]);
        }

    // Non-maximum suppression, straightforward implementation.
    for (int i = 1; i < nx - 1; i++)
        for (int j = 1; j < ny - 1; j++) {
            const int c = i + nx * j;
            const int nn = c - nx;
            const int ss = c + nx;
            const int ww = c + 1;
            const int ee = c - 1;
            const int nw = nn + 1;
            const int ne = nn - 1;
            const int sw = ss + 1;
            const int se = ss - 1;

            const float dir = (float)(fmod(atan2(after_Gy[c],
                                                 after_Gx[c]) + M_PI,
                                           M_PI) / M_PI) * 8;

            if (((dir <= 1 || dir > 7) && G[c] > G[ee] &&
                 G[c] > G[ww]) || // 0 deg
                ((dir > 1 && dir <= 3) && G[c] > G[nw] &&
                 G[c] > G[se]) || // 45 deg
                ((dir > 3 && dir <= 5) && G[c] > G[nn] &&
                 G[c] > G[ss]) || // 90 deg
                ((dir > 5 && dir <= 7) && G[c] > G[ne] &&
                 G[c] > G[sw]))   // 135 deg
                nms[c] = G[c];
            else
                nms[c] = 0;
        }

    // Reuse array
    // used as a stack. nx*ny/2 elements should be enough.
    int *edges = (int*) after_Gy;
    memset(out, 0, sizeof(pixel_t) * nx * ny);
    memset(edges, 0, sizeof(pixel_t) * nx * ny);

    // Tracing edges with hysteresis . Non-recursive implementation.
    size_t c = 1;
    for (int j = 1; j < ny - 1; j++)
        for (int i = 1; i < nx - 1; i++) {
            if (nms[c] >= tmax && out[c] == 0) { // trace edges
                out[c] = MAX_BRIGHTNESS;
                int nedges = 1;
                edges[0] = c;

                do {
                    nedges--;
                    const int t = edges[nedges];

                    int nbs[8]; // neighbours
                    nbs[0] = t - nx;     // nn
                    nbs[1] = t + nx;     // ss
                    nbs[2] = t + 1;      // ww
                    nbs[3] = t - 1;      // ee
                    nbs[4] = nbs[0] + 1; // nw
                    nbs[5] = nbs[0] - 1; // ne
                    nbs[6] = nbs[1] + 1; // sw
                    nbs[7] = nbs[1] - 1; // se

                    for (int k = 0; k < 8; k++)
                        if (nms[nbs[k]] >= tmin && out[nbs[k]] == 0) {
                            out[nbs[k]] = MAX_BRIGHTNESS;
                            edges[nedges] = nbs[k];
                            nedges++;
                        }
                } while (nedges > 0);
            }
            c++;
        }

    free(after_Gx);
    free(after_Gy);
    free(G);
    free(nms);

    return out;
}

int main(const int argc, const char ** const argv)
{
    if (argc < 2) {
        printf("Usage: %s image.bmp\n", argv[0]);
        return 1;
    }

    static bitmap_info_header_t ih;
    const pixel_t *in_bitmap_data = load_bmp(argv[1], &ih);
    if (in_bitmap_data == NULL) {
        fprintf(stderr, "main: BMP image not loaded.\n");
        return 1;
    }

    printf("Info: %d x %d x %d\n", ih.width, ih.height, ih.bitspp);

    const pixel_t *out_bitmap_data =
        canny_edge_detection(in_bitmap_data, &ih, 45, 50, 1.0f);
    if (out_bitmap_data == NULL) {
        fprintf(stderr, "main: failed canny_edge_detection.\n");
        return 1;
    }

    if (save_bmp("out.bmp", &ih, out_bitmap_data)) {
        fprintf(stderr, "main: BMP image not saved.\n");
        return 1;
    }

    free((pixel_t*)in_bitmap_data);
    free((pixel_t*)out_bitmap_data);
    return 0;
}
```



## D

{{trans|C}}
This version retains some of the style of the original C version. This code is faster than the C version, even with the DMD compiler. This version loads and saves PGM images, using the module of the Grayscale image Task.

```d
import core.stdc.stdio, std.math, std.typecons, std.string, std.conv,
       std.algorithm, std.ascii, std.array, bitmap, grayscale_image;

enum maxBrightness = 255;

alias Pixel = short;
alias IntT = typeof(size_t.init.signed);

// If normalize is true, map pixels to range 0...maxBrightness.
void convolution(bool normalize)(in Pixel[] inp, Pixel[] outp,
                                 in float[] kernel,
                                 in IntT nx, in IntT ny, in IntT kn)
pure nothrow @nogc in {
    assert(kernel.length == kn ^^ 2);
    assert(kn % 2 == 1);
    assert(nx > kn && ny > kn);
    assert(inp.length == outp.length);
} body {
    //immutable IntT kn = sqrti(kernel.length);
    immutable IntT khalf = kn / 2;

    static if (normalize) {
        float pMin = float.max, pMax = -float.max;

        foreach (immutable m; khalf .. nx - khalf) {
            foreach (immutable n; khalf .. ny - khalf) {
                float pixel = 0.0;
                size_t c;
                foreach (immutable j; -khalf .. khalf + 1) {
                    foreach (immutable i; -khalf .. khalf + 1) {
                        pixel += inp[(n - j) * nx + m - i] * kernel[c];
                        c++;
                    }
                }

                if (pixel < pMin) pMin = pixel;
                if (pixel > pMax) pMax = pixel;
            }
        }
    }

    foreach (immutable m; khalf .. nx - khalf) {
        foreach (immutable n; khalf .. ny - khalf) {
            float pixel = 0.0;
            size_t c;
            foreach (immutable j; -khalf .. khalf + 1) {
                foreach (immutable i; -khalf .. khalf + 1) {
                    pixel += inp[(n - j) * nx + m - i] * kernel[c];
                    c++;
                }
            }

            static if (normalize)
                pixel = maxBrightness * (pixel - pMin) / (pMax - pMin);
            outp[n * nx + m] = cast(Pixel)pixel;
        }
    }
}


void gaussianFilter(in Pixel[] inp, Pixel[] outp,
                    in IntT nx, in IntT ny, in float sigma)
pure nothrow in {
    assert(inp.length == outp.length);
} body {
    immutable IntT n = 2 * cast(IntT)(2 * sigma) + 3;
    immutable float mean = floor(n / 2.0);
    auto kernel = new float[n * n];

    debug fprintf(stderr,
                  "gaussianFilter: kernel size %d, sigma=%g\n",
                  n, sigma);

    size_t c;
    foreach (immutable i; 0 .. n) {
        foreach (immutable j; 0 .. n) {
            kernel[c] = exp(-0.5 * (((i - mean) / sigma) ^^ 2 +
                                    ((j - mean) / sigma) ^^ 2))
                        / (2 * PI * sigma * sigma);
            c++;
        }
    }

    convolution!true(inp, outp, kernel, nx, ny, n);
}


Image!Pixel cannyEdgeDetection(in Image!Pixel inp,
                               in IntT tMin, in IntT tMax,
                               in float sigma)
pure nothrow in {
    assert(inp !is null);
} body {
    immutable IntT nx = inp.nx.signed;
    immutable IntT ny = inp.ny.signed;
    auto outp = new Pixel[nx * ny];

    gaussianFilter(inp.image, outp, nx, ny, sigma);

    static immutable float[] Gx = [-1, 0, 1,
                                   -2, 0, 2,
                                   -1, 0, 1];
    auto after_Gx = new Pixel[nx * ny];
    convolution!false(outp, after_Gx, Gx, nx, ny, 3);

    static immutable float[] Gy = [ 1, 2, 1,
                                    0, 0, 0,
                                   -1,-2,-1];
    auto after_Gy = new Pixel[nx * ny];
    convolution!false(outp, after_Gy, Gy, nx, ny, 3);

    auto G = new Pixel[nx * ny];
    foreach (i; 1 .. nx - 1)
        foreach (j; 1 .. ny - 1) {
            immutable size_t c = i + nx * j;
            G[c] = cast(Pixel)hypot(after_Gx[c], after_Gy[c]);
        }

    // Non-maximum suppression, straightforward implementation.
    auto nms = new Pixel[nx * ny];
    foreach (immutable i; 1 .. nx - 1)
        foreach (immutable j; 1 .. ny - 1) {
            immutable IntT c = i + nx * j,
                           nn = c - nx,
                           ss = c + nx,
                           ww = c + 1,
                           ee = c - 1,
                           nw = nn + 1,
                           ne = nn - 1,
                           sw = ss + 1,
                           se = ss - 1;

            immutable aux = atan2(double(after_Gy[c]),
                                  double(after_Gx[c])) + PI;
            immutable float dir = float((aux % PI) / PI) * 8;

            if (((dir <= 1 || dir > 7) && G[c] > G[ee] &&
                 G[c] > G[ww]) || // 0 deg.
                ((dir > 1 && dir <= 3) && G[c] > G[nw] &&
                 G[c] > G[se]) || // 45 deg.
                ((dir > 3 && dir <= 5) && G[c] > G[nn] &&
                 G[c] > G[ss]) || // 90 deg.
                ((dir > 5 && dir <= 7) && G[c] > G[ne] &&
                 G[c] > G[sw]))   // 135 deg.
                nms[c] = G[c];
            else
                nms[c] = 0;
        }

    // Reuse array used as a stack. nx*ny/2 elements should be enough.
    IntT[] edges = (cast(IntT*)after_Gy.ptr)[0 .. after_Gy.length / 2];
    outp[] = Pixel.init;
    edges[] = 0;

    // Tracing edges with hysteresis. Non-recursive implementation.
    size_t c = 1;
    foreach (immutable j; 1 .. ny - 1) {
        foreach (immutable i; 1 .. nx - 1) {
            if (nms[c] >= tMax && outp[c] == 0) { // Trace edges.
                outp[c] = maxBrightness;
                IntT nedges = 1;
                edges[0] = c;

                do {
                    nedges--;
                    immutable IntT t = edges[nedges];

                    immutable IntT[8] neighbours = [
                        t - nx,      // nn
                        t + nx,      // ss
                        t + 1,       // ww
                        t - 1,       // ee
                        t - nx + 1,  // nw
                        t - nx - 1,  // ne
                        t + nx + 1,  // sw
                        t + nx - 1]; // se

                    foreach (immutable n; neighbours)
                        if (nms[n] >= tMin && outp[n] == 0) {
                            outp[n] = maxBrightness;
                            edges[nedges] = n;
                            nedges++;
                        }
                } while (nedges > 0);
            }
            c++;
        }
    }

    return Image!Pixel.fromData(outp, nx, ny);
}


void main(in string[] args) {
    immutable fileName = (args.length == 2) ? args[1] : "lena.pgm";
    Image!Pixel imIn;
    imIn = imIn.loadPGM(fileName);
    printf("Image size: %d x %d\n", imIn.nx, imIn.ny);
    imIn.cannyEdgeDetection(45, 50, 1.0f).savePGM("lena_canny.pgm");
}
```






## Go

{{libheader|Imger}}
The example image for this program is the color photograph of a steam engine taken from the Wikipedia article linked to in the task description.

After applying the Canny edge detector, the resulting image is similar to but not quite the same as the Wikipedia image, probably due to differences in the parameters used though a 5×5 Gaussian filter is used in both cases.

Note that on Linux the extension of the example image file name needs to be changed from .PNG to .png in order for the library used to recognize it.

```go
package main

import (
    ed "github.com/Ernyoke/Imger/edgedetection"
    "github.com/Ernyoke/Imger/imgio"
    "log"
)

func main() {
    img, err := imgio.ImreadRGBA("Valve_original_(1).png")
    if err != nil {
        log.Fatal("Could not read image", err)
    }

    cny, err := ed.CannyRGBA(img, 15, 45, 5)
    if err != nil {
        log.Fatal("Could not perform Canny Edge detection")
    }

    err = imgio.Imwrite(cny, "Valve_canny_(1).png")
    if err != nil {
        log.Fatal("Could not write Canny image to disk")
    }
}
```



## J

<p>In this solution images are represented as 2D arrays of pixels, with first and second axes representing down and right respectively. Each processing step has a specific pixel representation.  In the original and Gaussian-filtered images, array elements represent monochromatic intensity values as numbers ranging from 0 (black) to 255 (white). In the intensity gradient image, gradient values are vectors, and are represented as complex numbers, with real and imaginary components representing down and right respectively.   </p>
<p>Detected edge and non-edge points are represented as ones and zeros respectively. An edge is a set of connected edge points (points adjacent horizontally, vertically, or diagonally are considered to be connected). In the final image, each edge is represented by assigning its set of points a common unique value. </p>

```J
NB. 2D convolution, filtering, ...

convolve  =: 4 : 'x apply (($x) partition y)'
partition=: 2 1 3 0 |: {:@[ ]\ 2 1 0 |: {.@[ ]\ ]
apply=: [: +/ [: +/ *
max3x3 =: 3 : '(0<1{1{y) * (>./>./y)'
addborder =: (0&,@|:@|.)^:4
normalize =: ]%+/@,
attach =: 3 : 'max3x3 (3 3 partition (addborder y))'
unique =: 3 : 'y*i.$y'
connect =: 3 : 'attach^:_ unique y'

NB. on low memory devices, cropping or resampling of high-resolution images may be required
crop      =: 4 : 0
   'h w h0 w0' =: x
   |: w{. w0}. |: h{. h0}. y
)
resample  =: 4 : '|: (1{-x)(+/%#)\ |: (0{-x)(+/%#)\ y'
NB. on e. g. smartphones, image may need to be expanded for viewing
inflate1 =: 4 : 0
   'h w' =: $y
   r =: ,y
   c =: #r
   rr =: (c$x) # r
   (h,x*w)$rr
)
inflate =: 4 : '|: x inflate1 (|: x inflate1 y)'

NB. Step 1 - gaussian smoothing
step1 =: 3 : 0
   NB. Gaussian kernel (from Wikipedia article)
   <] gaussianKernel =: 5 5$2 4 5 4 2 4 9 12 9 4 5 12 15 12 5 4 9 12 9 4 2 4 5 4 2
   gaussianKernel =: gaussianKernel % 159
   gaussianKernel convolve y
)

NB. Step 2 - gradient
step2 =: 3 : 0
   <] gradientKernel =: 3 3$0 _1 0 0j_1 0 0j1 0 1 0
   gradientKernel convolve y
)

NB. Step 3 - edge detection
step3 =: 3 : 0
   NB. find the octant (eighth of circle) in which the gradient lies
   octant =: 3 : '4|(>.(_0.5+((4%(o. 1))*(12&o. y))))'
   <(i:6)(4 : 'octant (x j. y)')"0/(i:6)

   NB. is this gradient greater than [the projection of] a neighbor?
   greaterThan   =: 4 : ' (9 o.((x|.y)%y))<1'

   NB. is this gradient the greatest of immmediate colinear neighbore?
   greatestOf   =: 4 : '(x greaterThan y) *. ((-x) greaterThan y)'

   NB. relative address of neighbor relevant to grad direction
   krnl0 =. _1  0
   krnl1 =. _1 _1
   krnl2 =.  0 _1
   krnl3 =.  1 _1

   image =. y
   og =. octant image

   NB. mask for maximum gradient colinear with gradient
   ok0 =. (0=og) *. krnl0 greatestOf image
   ok1 =. (1=og) *. krnl1 greatestOf image
   ok2 =. (2=og) *. krnl2 greatestOf image
   ok3 =. (3=og) *. krnl3 greatestOf image
   image *. (ok0 +. ok1 +. ok2 +. ok3)
)

NB. Step 4 - Weak edge suppression
step4 =: 3 : 0
   magnitude =. 10&o. y
   NB. weak, strong threshholds
   NB. TODO: parameter picker algorithm or helper
   threshholds =. 1e14 1e15
   nearbyKernel =. 3 3 $ 4 1 4 # 1 0 1
   weak   =. magnitude > 0{threshholds
   strong =. magnitude > 1{threshholds
   strongs =. addborder (nearbyKernel convolve strong) > 0
   strong +. (weak *. strongs)
)

NB. given the edge points, find the edges
  step5 =: connect

canny =: step5 @ step4 @ step3 @ step2 @ step1


```

<p>The above implementation solves the 'inner problem' of Canny Edge Detection in the J language, with no external dependencies.  J's Qt IDE provides additional support including interfaces to image file formats, graphic displays, and the user. The following code exercises these features</p>

<p>The file 'valve.png' referenced in this code is from one of several Wikipedia articles on edge detection. It can be viewed at [https://upload.wikimedia.org/wikipedia/commons/2/2e/Valve_gaussian_%282%29.PNG[https://upload.wikimedia.org/wikipedia/commons/2/2e/Valve_gaussian_%282%29.PNG]]</p>

```J

require 'gl2'
coclass 'edge'
coinsert'jgl2'

PJ=: jpath '~Projects/edges/' NB. optionally install and run as project under IDE
load PJ,'canny.ijs'

run=: 3 : 0
   wd 'pc form;pn canny'
   wd 'cc txt static;cn "Canny in J";'
   wd 'cc png isidraw'
   wd 'cc inc button;cn "Next";'
   wd 'pshow'
   glclear''
   image =: readimg_jqtide_ PJ,'valve.png'
   image =: 240 360 120 150 crop image
   edges =: canny 256 | image
   ids =: }. ~.,edges
   nids =: # ids
   case =: 0
)

form_inc_button =: 3 : 0
   select. case
   case. 0 do.
      wd 'set txt text "original image";'
      img =: 255 setalpha image
   case. 1 do.
      wd 'set txt text "points on edges";'
      img =: edges>0
      img =: 1-img
      img =: img * (+/ 256^i.3) * 255
      img =: 255 setalpha img
      ix =: 0
   case. 2 do.
      wd 'set txt text "... iterating over edges with >75 points ...";'
      img =: edges=ix{ids
      whilst. (num<75) *. (ix<nids) do.
         img =: edges=ix{ids
         num =: +/,img
         ix=:>:ix
         if. ix=#ids do. case=:_1 end.
      end.
      img =: 1-img
      img =: img * (+/ 256^i.3) * 255
      img =: 255 setalpha img
      ix =: (#ids)|(>:ix)
   end.
   if. case<2 do. case =: >: case end.
   NB. img =: 5 inflate img      NB. might need this for high-res cellphone display
   glfill 255 128 255
   glpixels 0 0,(|.$img), ,img
   glpaint''
)

form_close=: exit bind 0

run''
```



## Julia

{{works with|Julia|0.6}}

```julia
using Images

canny_edges = canny(img, sigma = 1.4, upperThreshold = 0.80, lowerThreshold = 0.20)
```



## Mathematica


```Mathematica
Export["out.bmp", EdgeDetect[Import[InputString[]]]];
```

Mathematica uses canny edge detection by default. This seems so cheaty next to all of these giant answers...


## MATLAB

There is a built-in function, [http://www.mathworks.com/help/images/ref/edge.html edge], that has Canny Edge Detection as one of its options.

```MATLAB
BWImage = edge(GrayscaleImage,'canny');
```


=={{Header|Python}}==

In Python, Canny edge detection would normally be done using [http://scikit-image.org/docs/dev/auto_examples/plot_canny.html scikit-image] or OpenCV-Python. Here is an approach using numpy/scipy:


```python
#!/bin/python
import numpy as np
from scipy.ndimage.filters import convolve, gaussian_filter
from scipy.misc import imread, imshow

def CannyEdgeDetector(im, blur = 1, highThreshold = 91, lowThreshold = 31):
	im = np.array(im, dtype=float) #Convert to float to prevent clipping values

	#Gaussian blur to reduce noise
	im2 = gaussian_filter(im, blur)

	#Use sobel filters to get horizontal and vertical gradients
	im3h = convolve(im2,[[-1,0,1],[-2,0,2],[-1,0,1]])
	im3v = convolve(im2,[[1,2,1],[0,0,0],[-1,-2,-1]])

	#Get gradient and direction
	grad = np.power(np.power(im3h, 2.0) + np.power(im3v, 2.0), 0.5)
	theta = np.arctan2(im3v, im3h)
	thetaQ = (np.round(theta * (5.0 / np.pi)) + 5) % 5 #Quantize direction

	#Non-maximum suppression
	gradSup = grad.copy()
	for r in range(im.shape[0]):
		for c in range(im.shape[1]):
			#Suppress pixels at the image edge
			if r == 0 or r == im.shape[0]-1 or c == 0 or c == im.shape[1] - 1:
				gradSup[r, c] = 0
				continue
			tq = thetaQ[r, c] % 4

			if tq == 0: #0 is E-W (horizontal)
				if grad[r, c] <= grad[r, c-1] or grad[r, c] <= grad[r, c+1]:
					gradSup[r, c] = 0
			if tq == 1: #1 is NE-SW
				if grad[r, c] <= grad[r-1, c+1] or grad[r, c] <= grad[r+1, c-1]:
					gradSup[r, c] = 0
			if tq == 2: #2 is N-S (vertical)
				if grad[r, c] <= grad[r-1, c] or grad[r, c] <= grad[r+1, c]:
					gradSup[r, c] = 0
			if tq == 3: #3 is NW-SE
				if grad[r, c] <= grad[r-1, c-1] or grad[r, c] <= grad[r+1, c+1]:
					gradSup[r, c] = 0

	#Double threshold
	strongEdges = (gradSup > highThreshold)

	#Strong has value 2, weak has value 1
	thresholdedEdges = np.array(strongEdges, dtype=np.uint8) + (gradSup > lowThreshold)

	#Tracing edges with hysteresis
	#Find weak edge pixels near strong edge pixels
	finalEdges = strongEdges.copy()
	currentPixels = []
	for r in range(1, im.shape[0]-1):
		for c in range(1, im.shape[1]-1):
			if thresholdedEdges[r, c] != 1:
				continue #Not a weak pixel

			#Get 3x3 patch
			localPatch = thresholdedEdges[r-1:r+2,c-1:c+2]
			patchMax = localPatch.max()
			if patchMax == 2:
				currentPixels.append((r, c))
				finalEdges[r, c] = 1

	#Extend strong edges based on current pixels
	while len(currentPixels) > 0:
		newPix = []
		for r, c in currentPixels:
			for dr in range(-1, 2):
				for dc in range(-1, 2):
					if dr == 0 and dc == 0: continue
					r2 = r+dr
					c2 = c+dc
					if thresholdedEdges[r2, c2] == 1 and finalEdges[r2, c2] == 0:
						#Copy this weak pixel to final result
						newPix.append((r2, c2))
						finalEdges[r2, c2] = 1
		currentPixels = newPix

	return finalEdges

if __name__=="__main__":
	im = imread("test.jpg", mode="L") #Open image, convert to greyscale
	finalEdges = CannyEdgeDetector(im)
	imshow(finalEdges)
```



## Tcl

{{libheader|crimp}}

```tcl
package require crimp
package require crimp::pgm

proc readPGM {filename} {
    set f [open $filename rb]
    set data [read $f]
    close $f
    return [crimp read pgm $data]
}
proc writePGM {filename image} {
    crimp write 2file pgm-raw $filename $image
}

proc cannyFilterFile {{inputFile "lena.pgm"} {outputFile "lena_canny.pgm"}} {
    writePGM $outputFile [crimp filter canny sobel [readPGM $inputFile]]
}
cannyFilterFile {*}$argv
```

