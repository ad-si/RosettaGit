+++
title = "Run-length encoding/C"
description = ""
date = 2012-12-27T06:48:04Z
aliases = []
[extra]
id = 5327
[taxonomies]
categories = []
tags = []
+++

{{collection|Run-length encoding}}
=== Unbuffered - Generative ===
'''Using GNU Compiler Collection gcc extensions'''

{{trans|ALGOL 68}}

{{works with|gcc|gcc version 4.1.2 20080704 (Red Hat 4.1.2-46)}}

Note: The following code sample is experimental as it implements python style iterators for (potentially) infinite sequences.  The code also uses "macro magic", C is not normally written this way, and in the case of this sample it requires the GCC "nested procedure" extension to the C language.

Also: In this implementation, '''string'''s are assumed to be null terminated.  Hence cannot contain the ''null'' character as data.

```c>#include <stdio.h

#include <stdlib.h>
#include <string.h>

const char *input = "WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWBWWWWWWWWWWWWWW";
const char *output = "12W1B12W3B24W1B14W";

typedef void (*YIELDCHAR)(); /* yields via yield_init */
typedef void (*GENCHAR) ();

/* #include <macro.h> :-) */
typedef enum{FALSE=0, TRUE=1}BOOL;

#ifdef NEED_GOTO
#include <setjmp.h>
/* declare label otherwise it is not visible in sub-scope */
#define LABEL(label) jmp_buf label; if(setjmp(label))goto label;
#define GOTO(label) longjmp(label, TRUE)
#endif

/* the following line is the only time I have ever required "auto" */
#define FOR(i,iterator) auto BOOL lambda(i); yield_init = (void *)&lambda; iterator; BOOL lambda(i)
#define DO {
#define     YIELD(x) if(!yield(x))return
#define     BREAK return FALSE
#define     CONTINUE return TRUE
#define OD CONTINUE; }
/* Warning: _Most_ FOR(,){ } loops _must_ have a CONTINUE, BREAK
 * or OD as the terminating statement. Otherwise the lambda will 
 * return random value from stack, and may terminate early */

typedef BOOL ITERATOR;
static volatile void *yield_init; /* not thread safe */
#define YIELDS(type) BOOL (*yield)(type) = yield_init

ITERATOR gen_char_seq (const char *s){
  YIELDS(char);
  fflush(stdout);
  int upb_s = strlen(s);
  int i; for(i = 0; i <= upb_s; i++) YIELD(s[i]);
}

void input_seq (){ YIELDS(char); FOR(char c, gen_char_seq(input))DO yield(c); OD; }
void output_seq (){ YIELDS(char); FOR(char c, gen_char_seq(output))DO yield(c); OD; }

ITERATOR gen_encode (GENCHAR gen_char){
  YIELDS(char);
  int count = 0;
  char prev;
  FOR(char c, gen_char())DO
      if(count == 0){
        count = 1;
        prev = c;
      } else if(c != prev){
        char str_count[32]; sprintf(str_count, "%d", count);
        FOR(char c, gen_char_seq(str_count))DO YIELD(c); OD;
        count = 1;
        YIELD(prev); prev = c;
      } else {
        count += 1;
      }
  OD;
  if(count != 0){
    char str_count[32]; sprintf(str_count, "%d", count);
    FOR(char c, gen_char_seq(str_count))DO YIELD(c); OD;
    YIELD(prev);
  }
}

const char *zero2nine = "0123456789";

ITERATOR gen_decode (GENCHAR gen_char){
  YIELDS(char);
  int repeat = 0;
  FOR(char c, (*gen_char)())DO
    if(strchr(zero2nine, c)){
      repeat = repeat*10 + c - '0';
    } else {
      int i; for(i = 1; i <= repeat; i++ ){ YIELD(c); }
      repeat = 0;
    }
  OD
}

int main(){
  /* iterate through input string */
  printf("Encode input: ");
    FOR(char c, gen_encode(input_seq))DO
      putchar(c);
    OD;
  printf("\n");

  /* iterate through output string */
  printf("Decode output: ");
    FOR(char c, gen_decode(output_seq))DO
      putchar(c);
    OD;
  printf("\n");
  return 0;
}
```

Output:

```txt

Encode input: 12W1B12W3B24W1B14W1
Decode output: WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWBWWWWWWWWWWWWWW

```



###  Buffered 

These functions have no check for the size of the output buffers.

'''Encoding function'''

Since repeat counter must fit a single byte in this implementation, it can't be greater than 255, so a byte repeated more than 255 times generates in the compressed stream more than 2 bytes (4 bytes if the length of the repeated byte sequence is less than 511 and so on)


```c
int rle_encode(char *out, const char *in, int l)
{
  int dl, i;
  char cp, c;

  for(cp=c= *in++, i = 0, dl=0; l>0 ; c = *in++, l-- ) {
    if ( c == cp ) {
      i++;
      if ( i > 255 ) {
	*out++ = 255;
	*out++ = c; dl += 2;
	i = 1;
      }
    } else {
      *out++ = i;
      *out++ = cp; dl += 2;
      i = 1;
    }
    cp = c;
  }
  *out++ = i; *out++ = cp; dl += 2;
  return dl;
}
```


'''Decoding function'''


```c
int rle_decode(char *out, const char *in, int l)
{
  int i, j, tb;
  char c;

  for(tb=0 ; l>0 ; l -= 2 ) {
    i = *in++;
    c = *in++;
    tb += i;
    while(i-- > 0) *out++ = c;
  }
  return tb;
}
```


'''Usage example'''


```c>#include <stdio.h

#include <stdlib.h>
#include <string.h>

const char *o = "WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWBWWWWWWWWWWWWWW";

int main()
{
  char *d = malloc(2*strlen(o));
  char *oc = malloc(strlen(o));
  
  int rl = rle_encode(d, o, strlen(o));
  /* fwrite(d, 1, rl, stdout); */

  int ocl = rle_decode(oc, d, rl);
  fwrite(oc, 1, ocl, stdout);

  free(d); free(oc);
  return 0;
}
```


In the following codes, encoding and decoding are implemented as "filters" which compress/decompress standard input to standard output writing ASCII strings; they will work as long as the input has no ASCII digits in it, and the compressed/original ratio of a "single group" will be less than or equal to 1 as long as the ASCII decimal representation's length of the repeat counter will be shorter than the length of the "group". It should be so except in the case the group is a single isolated character, e.g. B gives 1B (one byte against two compressed bytes)

'''Encoding filter'''


```c>#include <stdio.h


int main()
{
  int i, c, cp;

  for(cp=c=getchar(), i = 0; c != EOF; c = getchar() ) {
    if ( c == cp ) {
      i++;
    } else {
      printf("%d%c", i, cp);
      i = 1;
    }
    cp = c;
  }
  printf("%d%c", i, cp);
  return 0;
}
```


'''Decoding filter'''


```c>#include <stdio.h


int main()
{
  int i, c, j;

  while( scanf("%d%c", &i, &c) == 2 ) {
    for(j=0; j < i; j++) printf("%c", c);
  }
  return 0;
}
```


'''Final note''': since the repeat counter value 0 has no meaning, it could be used as it would be 256, so extending by one the maximum number of repetitions representable with a single byte; or instead it could be used as a special marker to encode in a more efficient way (long) sequences of ''isolated characters'', e.g. "ABCDE" would be encoded as "1A1B1C1D1E"; it could be instead encoded as "05ABCDE".
