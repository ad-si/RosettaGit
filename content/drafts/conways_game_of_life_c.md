+++
title = "Conway's Game of Life/C"
description = ""
date = 2010-01-08T16:54:13Z
aliases = []
[extra]
id = 5304
[taxonomies]
categories = []
tags = []
+++

{{collection|Conway's Game of Life}}

I wrote functions that can be used in a rather general way.


```c
#define CELL(I,J) (field[size*(I)+(J)])
#define ALIVE(I,J) t[size*(I)+(J)] = 1
#define DEAD(I,J)  t[size*(I)+(J)] = 0

int count_alive(const char *field, int i, int j, int size)
{
   int x, y, a=0;
   for(x=i-1; x <= (i+1) ; x++)
   {
      for(y=j-1; y <= (j+1) ; y++)
      {
         if ( (x==i) && (y==j) ) continue;
         if ( (y<size) && (x<size) &&
              (x>=0)   && (y>=0) )
         {
              a += CELL(x,y);
         }
      }
   }
   return a;
}

void evolve(const char *field, char *t, int size)
{
   int i, j, alive, cs;
   for(i=0; i < size; i++)
   {
      for(j=0; j < size; j++)
      {
         alive = count_alive(field, i, j, size);
         cs = CELL(i,j);
         if ( cs )
         {
            if ( (alive > 3) || ( alive < 2 ) )
                DEAD(i,j);
            else
                ALIVE(i,j);
         } else {
            if ( alive == 3 )
                ALIVE(i,j);
            else
                DEAD(i,j);
         }
      }
   }
}
```


The function '''evolve''' needs a buffer where to store the result, and this buffer must be provided by the user calling the function. An example of usage to test the blinker and the glider is the following.


```c>#include <stdio.h


/* some additional header needed to use the function evolve provided
   previously, or just copy/paste the given code here */

#define BLINKER_SIZE 3
#define BLINKER_GEN 3
char small_blinker[] = {
      0,0,0,
      1,1,1,
      0,0,0
};
char temp_blinker[BLINKER_SIZE*BLINKER_SIZE];

#define FIELD_SIZE 45
#define FIELD_GEN 175
char field[FIELD_SIZE * FIELD_SIZE];
char temp_field[FIELD_SIZE*FIELD_SIZE];

/* set the cell i,j as alive */
#define SCELL(I,J) field[FIELD_SIZE*(I)+(J)] = 1

void dump_field(const char *f, int size)
{
   int i;
   for (i=0; i < (size*size); i++)
   {
      if ( (i % size) == 0 ) printf("\n");
      printf("%c", f[i] ? 'X' : '.');
   }
   printf("\n");
}


int main(int argc, char **argv)
{
    int i;
    char *fa, *fb, *tt, op;
    
    /* fast and dirty selection option */
    if ( argc > 1 )
    {
       op = *argv[1];
    } else {
       op = 'b';
    }
    
    switch ( op )
    {
      case 'B':
      case 'b':
        /* blinker test */
        fa = small_blinker;
        fb = temp_blinker;
        for(i=0; i< BLINKER_GEN; i++)
        {
           dump_field(fa, BLINKER_SIZE);
           evolve(fa, fb, BLINKER_SIZE);
           tt = fb; fb = fa; fa = tt;
        }
        return 0;
      case 'G':
      case 'g':
        for(i=0; i < (FIELD_SIZE*FIELD_SIZE) ; i++) field[i]=0;
        /* prepare the glider */
                     SCELL(0, 1);
                                  SCELL(1, 2);
        SCELL(2, 0); SCELL(2, 1); SCELL(2, 2);
        /* evolve */
        fa = field;
        fb = temp_field;
        for (i=0; i < FIELD_GEN; i++)
        {
           dump_field(fa, FIELD_SIZE);
           evolve(fa, fb, FIELD_SIZE);
           tt = fb; fb = fa; fa = tt;
        }
        return 0;
      default:
        fprintf(stderr, "no CA for this\n");
        break;
    }
    return 1;
}
```


The blinker output (reformatted) is simply:


```txt

...  .X.  ...
XXX  .X.  XXX
...  .X.  ...

```

