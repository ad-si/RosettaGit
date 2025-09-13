+++
title = "Execute SNUSP/C"
description = ""
date = 2012-12-27T06:38:15Z
aliases = []
[extra]
id = 3199
[taxonomies]
categories = []
tags = []
+++


This is an interpreter for Modular SNUSP (understands @ and #) written in [C](https://rosettacode.org/wiki/C), and a special enhanced non-standard mode can be activated from the command line. Limits are hard encoded using #defines; the memory pointer wraps around with a (rather silly) warning.

The enhanced mode actives the interpretation of &, different from the one of the Bloated SNUSP; the & instruction changes the behaviour of the ''read'' and ''write'' instructions (, and .): until the next &, the comma will read from the current cell and store the read value into an accumulator register, and the dot (write) will write the value of the accumulator in the current cell.

The source code to be interpreted is specified on the command line after the keyword '''snusp''', while the keyword '''enhanced''' activates the enhanced non standard mode, and the keyword '''debug''' activates debugging mode. Normally the read instruction reads from the stdin and the write instruction writes to stdout.


```c
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define LINESIZE 1024
#define MAXHEIGHT 1024
#define MAXCALLS 1024
#define MAXCELLS 1024

typedef char bool;
#define TRUE 1
#define FALSE 0


bool debug=FALSE;
bool enhanced=FALSE;


#define snusp_START '$'
#define snusp_LEFT '<'
#define snusp_RIGHT '>'
#define snusp_INC '+'
#define snusp_DEC '-'
#define snusp_READ ','
#define snusp_WRITE '.'
#define snusp_LURD '\\'
#define snusp_RULD '/'
#define snusp_SKIP '!'
#define snusp_SKIPZ '?'
#define snusp_ENTER '@'
#define snusp_LEAVE '#'

/* enhanced: & transform , so that it read from current cell
   into an accumulator, . write the accumulator to the current
   cell; the others act the same; & again put the previous behaviour
   back */
#define snusp_SWAP '&'


char buffer[LINESIZE];

struct fieldslice
{
   int l;
   char *s;
};

struct callstack
{
   int x, y;
   int dx, dy;
};


int  x=0, y=0, dx=1, dy=0;
bool swapped = FALSE;
char accumulator = 0;

struct fieldslice field[MAXHEIGHT];
struct callstack stack[MAXCALLS];
char cells[MAXCELLS];

int maxi=0;
int sp = MAXCALLS;
int cellidx = 0;


const char *in_the_space = "help! I was thrown alone in the void space!\n";
const char *trip_around_world = "wow! I always dreamed about a trip around the world!\n";


int main(int argc, char **argv)
{
  int i, j, c;
  char *filename = NULL, *pc;
  FILE *srch;
  bool ok = TRUE;

     for(i=1; i < argc ; i++ )
     {
       if ( strcmp(argv[i], "enhanced") == 0 )
       {
         enhanced = TRUE;
         continue;
       }
       if ( strcmp(argv[i], "snusp") == 0 )
       {
            if ( (i+1) < argc )
            {
                filename = argv[i + 1]; i++;
            } else {
                fprintf(stderr, "omitted string for 'snusp'\n");
            }
            continue;
       }
       if ( strcmp(argv[i], "debug") == 0 )
       {
          debug = TRUE; continue;
       }
       fprintf(stderr, "unrecognized option '%s'\n", argv[i]);
     }

     if ( filename == NULL )
     {
        fprintf(stderr, "no source file specified\n");
        exit(1);
     }

     /* load program */
     if ( (srch = fopen(filename, "r") ) == NULL )
     {
        fprintf(stderr, "cannot locate file '%s'\n", filename);
        exit(1);
     }

     for (i=0; (fgets(buffer, LINESIZE, srch) != NULL) && i < MAXHEIGHT; i++ )
     {
        field[i].l = strlen(buffer);
        field[i].s = malloc(field[i].l);
        if ( field[i].s == NULL )
        {
           fprintf(stderr, "can't allocate buffer\n");
           exit(1);
        }
        memcpy(field[i].s, buffer, field[i].l);
        if ( (pc = strchr(buffer, snusp_START)) != NULL )
        {
            x = pc - buffer;
            y = i;
        }
     }
     if ( i == MAXHEIGHT )
     {
         fprintf(stderr, "bottom universal limit reached! Continuing anyway\n");
     }
     fclose(srch);

     maxi = i;

     /* interpreter */
     while( ok )
     {
         char cmd;

         if ( (y >= i) || (y<0) )
         {
           fprintf(stderr, in_the_space);
           fprintf(stderr, "(%d,%d) (%d,%d)\n", x, y, dx, dy);
           break;
         }
         if ( (x >= field[y].l) || (x<0) )
         {
            fprintf(stderr, in_the_space);
            fprintf(stderr, "(%d,%d) (%d,%d)\n", x, y, dx, dy);
            break;
         }
         cmd = field[y].s[x];
         if ( debug )
         {
            fprintf(stderr, "(%d, %d) (%d, %d) %c sp=%d idx=%d\n", x, y,
                    dx, dy, cmd, sp, cellidx);
         }
         switch ( cmd )
         {
           case snusp_LEAVE:
                if ( sp < MAXCALLS )
                {
                     x = stack[sp].x;
                     y = stack[sp].y;
                     dx = stack[sp].dx;
                     dy = stack[sp].dy;
                     sp++;
                } else {
                     ok = FALSE;
                }
                break;
           case snusp_ENTER:
                sp--;
                if (sp < 0 )
                {
                   fprintf(stderr, "too many nested calls! skipping!\n");
                   sp++;
                } else {
                   stack[sp].x = x;
                   stack[sp].y = y;
                   stack[sp].dx = dx;
                   stack[sp].dy = dy;
                   x += dx; y += dy;
                }
                break;
           case snusp_RIGHT:
                if ( (cellidx+1) >= MAXCELLS )
                {
                   fprintf(stderr, trip_around_world);
                }
                cellidx = (cellidx+1)%MAXCELLS;
                break;
           case snusp_LEFT:
                if ( (cellidx-1) < 0 )
                {
                   fprintf(stderr, trip_around_world);
                }
                cellidx = (cellidx+MAXCELLS-1)%MAXCELLS;
                break;
           case snusp_INC:
                cells[cellidx]++;
                break;
           case snusp_DEC:
                cells[cellidx]--;
                break;
           case snusp_LURD:
                /* \ */
                if ( dx != 0 )
                {
                   dy = dx; dx = 0;
                } else {
                   dx = dy;
                   dy = 0;
                }
                break;
           case snusp_RULD:
                /* / */
                if ( dx != 0 )
                {
                   dy = -dx; dx = 0;
                } else {
                   dx = -dy; dy = 0;
                }
                break;
           case snusp_SKIP:
                x += dx; y += dy;
                break;
           case snusp_SKIPZ:
                if ( cells[cellidx] == 0 )
                {
                   x += dx; y += dy;
                }
                break;
           case snusp_READ:
                if ( swapped && enhanced )
                {
                   accumulator = cells[cellidx];
                } else {
                   c = getchar();
                   cells[cellidx] = c & 0xFF;
                }
                break;
           case snusp_WRITE:
                if ( swapped && enhanced )
                {
                   cells[cellidx] = accumulator;
                } else {
                   printf("%c", cells[cellidx]);
                }
                break;
           case snusp_SWAP:
                if ( enhanced )
                {
                   swapped ^= TRUE;
                }
                break;
           default:
                break;
         }
         x += dx; y += dy;
     }

     for(i=0; i < maxi; i++)
        free(field[i].s);
}
```

