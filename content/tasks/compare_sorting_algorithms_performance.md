+++
title = "Compare sorting algorithms' performance"
description = ""
date = 2019-02-03T01:33:53Z
aliases = []
[extra]
id = 2438
[taxonomies]
categories = ["task", "Sorting"]
tags = []
languages = [
  "autohotkey",
  "bbc_basic",
  "c",
  "d",
  "erlang",
  "go",
  "j",
  "julia",
  "kotlin",
  "phix",
  "python",
  "ruby",
  "tcl",
]
+++

## Task

Measure a relative performance of sorting algorithms implementations.

Plot '''execution time vs. input sequence length''' dependencies for various implementation of sorting algorithm and different input sequence types ([[#Figures: log2( time in microseconds ) vs. log2( sequence length )|example figures]]).

Consider three type of input sequences:
* ones: sequence of all ''1'''s. Example: {1, 1, 1, 1, 1}
* range: ascending sequence, i.e. already sorted. Example: {1, 2, 3, 10, 15}
* shuffled range: sequence with elements randomly distributed. Example: {5, 3, 9, 6, 8}

Consider at least two different sorting functions (different algorithms or/and different implementation of the same algorithm).
For example, consider [[Bubble Sort]], [[Insertion sort]], [[Quicksort]] or/and implementations of Quicksort with different pivot selection mechanisms. Where possible, use existing implementations.

Preliminary subtask:
* [[Bubble Sort]], [[Insertion sort]], [[Quicksort]], [[Radix sort]], [[Shell sort]]
* [[Query Performance]]
* [[Write float arrays to a text file]]
* [[Plot x, y arrays]]
* [[Polynomial Fitting]]

General steps:
# Define sorting routines to be considered.
# Define appropriate sequence generators and write timings.
# Plot timings.
# What conclusions about relative performance of the sorting routines could be made based on the plots?


## AutoHotkey

```ahk
; BUGGY - FIX

#Persistent
#SingleInstance OFF
SetBatchLines, -1
SortMethods := "Bogo,Bubble,Cocktail,Counting,Gnome,Insertion,Merge,Permutation,Quick,Selection,Shell,BuiltIn"
Gui, Add, Edit, vInput, numbers,separated,by,commas,without,spaces,afterwards
Loop, PARSE, SortMethods, `,
	Gui, Add, CheckBox, v%A_LoopField%, %A_LoopField% Sort
Gui, Add, Button, gTest, Test!
Gui, Show,, SortTest!
Return
Test:
SplashTextOn,,, Test Commencing
Sleep 2500
SplashTextOff
Gui, +OwnDialogs
Gui, Submit, NoHide
Loop, PARSE, SortMethods, `,
{
	If (%A_LoopField%)
	{
		DllCall("QueryPerformanceCounter", "Int64 *", %A_LoopField%Begin)
		%A_LoopField%Out := %A_LoopField%Sort(Input)
		DllCall("QueryPerformanceCounter", "Int64 *", %A_LoopField%Time)
		%A_LoopField%End := %A_LoopField%Begin + %A_LoopField%Time
		%A_LoopField%Time -= %A_LoopField%Begin
	}
}
Time := ""
Loop, PARSE, SortMethods, `,
	If (%A_LoopField%)
		Time .= A_LoopField . " Sort: " . %A_LoopField%Time . "`t`t" . %A_LoopField%Out . "`r`n"
MsgBox,, Results!, %Time%
Return



; Sorting funtions (Bogo, Bubble, Cocktail, Counting, Gnome, Insertion, Merge, Permutation, Quick, Selection, Shell, BuiltIn):

BogoSort(var)
{
	sorted := 1
	Loop, Parse, var
	{
		current := A_LoopField
		rest := SubStr(var, A_Index)
		Loop, Parse, rest
		{
			If (current > A_LoopField)
			sorted := 0
		}
	}
	While !sorted {
		sorted := 1
		Loop, Parse, var, `,
		{
			current := A_LoopField
			rest := SubStr(var, A_Index)
			Loop, Parse, rest, `,
			{
				If (current > A_LoopField)
				sorted := 0
			}
		}

		Sort, var, D`, Random
	}
	Return var
}

BubbleSort(var)
{
	StringSplit, array, var, `,
	hasChanged = 1
	size := array0
	While hasChanged
	{
		hasChanged = 0
		Loop, % (size - 1)
		{
			i := array%A_Index%
			aj := A_Index + 1
			j := array%aj%
			If (j < i)
			{
				temp := array%A_Index%
				array%A_Index% := array%aj%
				array%aj% := temp
				hasChanged = 1
			}
		}
	}
	Loop, % size
	sorted .= "," . array%A_Index%
	Return substr(sorted,2)
}

CocktailSort(var)
{
	StringSplit array, var, `,
	i0 := 1, i1 := array0
	Loop
	{
		Changed =
		Loop % i1-- -i0 {
			j := i0+A_Index, i := j-1
			If (array%j% < array%i%)
				t := array%i%, array%i% := array%j%, array%j% := t
				,Changed = 1
		}
		IfEqual Changed,, Break
		Loop % i1-i0++
		{
			i := i1-A_Index, j := i+1
			If (array%j% < array%i%)
			t := array%i%, array%i% := array%j%, array%j% := t
			,Changed = 1
		}
		IfEqual Changed,, Break
	}
	Loop % array0
		sorted .= "," . array%A_Index%
	Return SubStr(sorted,2)
}

CountingSort(var)
{
	max := min := substr(var, 1, instr(var, ","))
	Loop, parse, var, `,
	{
		If (A_LoopField > max)
			max := A_LoopField

		Else If (A_LoopField < min)
			min := A_LoopField
	}
	Loop % max-min+1
		i := A_Index-1, a%i% := 0
	Loop, Parse, var, `,
		i := A_LoopField-min, a%i%++
	Loop % max-min+1
	{
		i := A_Index-1, v := i+min
		Loop % a%i%
			t .= "," v
	}
	Return SubStr(t,2)
}

GnomeSort(var) {
	StringSplit, a, var, `,
	i := 2, j := 3
	While i <= a0 {
		u := i-1
		If (a%u% < a%i%)
			i := j, j := j+1
		Else {
			t := a%u%, a%u% := a%i%, a%i% := t
			If (--i = 1)
				i := j, j++
		}
	}
	Loop % a0
	sorted .= "," . a%A_Index%
	Return SubStr(sorted,2)
}

InsertionSort(var) {
	StringSplit, a, var, `,
	Loop % a0-1 {
		i := A_Index+1, v := a%i%, j := i-1
		While j>0 and a%j%>v
			u := j+1, a%u% := a%j%, j--
		u := j+1, a%u% := v
	}
	Loop % a0
	sorted .= "," . a%A_Index%
	Return SubStr(sorted,2)
}


MergeSort(var) {
	StringReplace, t, var, `,,, UseErrorLevel
	L := ((t = "") ? 0 : ErrorLevel+1)
	If (2 > L)
		Return var
	StringGetPos, p, var, `,, % "L" L//2
	list0 := MergeSort(SubStr(var,1,p))
	list1 := MergeSort(SubStr(var,p+2))
	If (list0 = "")
		Return list1
	Else If (list1 = "")
		Return list0
	list := list0
	i0 := (p0 := InStr(list,",",0,i:=p0+1)) ? SubStr(list,i,p0-i) : SubStr(list,i)
	list := list1
	i1 := (p1 := InStr(list,",",0,i:=p1+1)) ? SubStr(list,i,p1-i) : SubStr(list,i)
	Loop  {
		i := i0>i1
		list .= "," i%i%
		If (p%i%) {
			list := list%i%
			i%i% := (p%i% := InStr(list,",",0,i:=p%i%+1)) ? SubStr(list,i,p%i%-i) : SubStr(list,i)
		}
		Else {
			i ^= 1
			rtv := SubStr(list "," i%i% (p%i% ? "," SubStr(list%i%,p%i%+1) : ""), 2)
		}
	}
	Return rtv
}

PermutationSort(var) {
	static a:="a",v:="v"
	StringSplit, a, var, `,
	v0 := a0
	Loop %v0%
		v%A_Index% := A_Index
	unsorted := 0
	Loop % %a%0-1 {
		i := %v%%A_Index%, j := A_Index+1, j := %v%%j%
		If (%a%%i% > %a%%j%)
			unSorted := 1
	}
	While unSorted {
		i := %v%0, i1 := i-1
		While %v%%i1% >= %v%%i% {
			--i, --i1
			IfLess i1,1, Return 1
		}
		j := %v%0
		While %v%%j% <= %v%%i1%
			--j
		t := %v%%i1%, %v%%i1% := %v%%j%, %v%%j% := t,  j := %v%0
		While i < j
			t := %v%%i%, %v%%i% := %v%%j%, %v%%j% := t, ++i, --j
		unsorted := 0
		Loop % %a%0-1 {
			i := %v%%A_Index%, j := A_Index+1, j := %v%%j%
			If (%a%%i% > %a%%j%)
				unSorted := 1
		}
	}
	Loop % a0
		i := v%A_Index%, sorted .= "," . a%i%
	Return SubStr(sorted,2)
}

QuickSort(var)
{
	StringSplit, list, var, `,
	If (list0 <= 1)
		Return list
	pivot := list1
	Loop, Parse, var, `,
	{
		If (A_LoopField < pivot)
			less .= "," . A_LoopField
		Else If (A_LoopField > pivot)
			more .= "," . A_LoopField
		Else
			pivotlist .= "," . A_LoopField
	}
	less := QuickSort(substr(less,2))
	more := QuickSort(substr(more,2))
	Return substr(less,2) . pivotList . more
}

SelectionSort(var) {
	StringSplit, a, var, `,
	Loop % a0-1 {
		i := A_Index, mn := a%i%, j := m := i
		Loop % a0-i {
			j++
			If (a%j% < mn)
				mn := a%j%, m := j
		}
		t := a%i%, a%i% := a%m%, a%m% := t
	}
	Loop % a0
		sorted .= "," . a%A_Index%
	Return SubStr(sorted,2)
}

ShellSort(var) {
	StringSplit, a, var, `,
	inc := a0
	While inc:=round(inc/2.2)
		Loop % a0-inc {
			i := A_Index+inc, t := a%i%, j := i, k := j-inc
			While j > inc && a%k% > t
				a%j% := a%k%, j := k, k -= inc
			a%j% := t
		}
	Loop % a0
		s .= "," . a%A_Index%
	Return SubStr(s,2)
}

BuiltInSort(var) {
	Sort, var, N D`,
	Return var
}
```



## BBC BASIC

```bbcbasic
      HIMEM = PAGE + 2000000
      INSTALL @lib$+"SORTLIB"
      INSTALL @lib$+"TIMERLIB"
      Sort% = FN_sortinit(0,0)
      Timer% = FN_ontimer(1000, PROCtimer, 1)

      PRINT "Array size:", 1000, 10000, 100000
      @% = &2020A

      FOR patt% = 1 TO 4
        CASE patt% OF
          WHEN 1: PRINT '"Data set to all ones:"
          WHEN 2: PRINT '"Data ascending sequence:"
          WHEN 3: PRINT '"Data randomly shuffled:"
          WHEN 4: PRINT '"Data descending sequence:"
        ENDCASE

        FOR type% = 1 TO 6
          CASE type% OF
            WHEN 1: PRINT "Internal (lib)";
            WHEN 2: PRINT "Quicksort   ";
            WHEN 3: PRINT "Radix sort  ";
            WHEN 4: PRINT "Shellsort   ";
            WHEN 5: PRINT "Bubblesort  ";
            WHEN 6: PRINT "Insertion sort";
          ENDCASE

          FOR power% = 3 TO 5
            PROCsorttest(patt%, type%, 10^power%)
          NEXT
          PRINT

        NEXT type%
      NEXT patt%
      END

      DEF PROCsorttest(patt%, type%, size%)
      LOCAL a%(), C%, I%
      DIM a%(size%-1)

      CASE patt% OF
        WHEN 1: a%() = 1 : a%() = 1
        WHEN 2: FOR I% = 0 TO size%-1 : a%(I%) = I% : NEXT
        WHEN 3: FOR I% = 0 TO size%-1 : a%(I%) = I% : NEXT
          C% = RND(-123456) : REM Seed
          FOR I% = size% TO 2 STEP -1 : SWAP a%(I%-1),a%(RND(I%)-1) : NEXT
        WHEN 4: FOR I% = 0 TO size%-1 : a%(I%) = size%-1-I% : NEXT
      ENDCASE

      Start% = TIME
      ON ERROR LOCAL PRINT , "   >100.00" ; : ENDPROC
      CASE type% OF
        WHEN 1: C% = size% : CALL Sort%, a%(0)
        WHEN 2: PROCquicksort(a%(), 0, size%)
        WHEN 3: PROCradixsort(a%(), size%, 10)
        WHEN 4: PROCshellsort(a%(), size%)
        WHEN 5: PROCbubblesort(a%(), size%)
        WHEN 6: PROCinsertionsort(a%(), size%)
      ENDCASE
      PRINT , (TIME - Start%)/100;

      FOR I% = 0 TO size%-2
        IF a%(I%) > a%(I%+1) ERROR 100, "Sort failed!"
      NEXT
      ENDPROC

      DEF PROCtimer
      Start% += 0
      IF (TIME - Start%) > 10000 ERROR 111, ""
      ENDPROC

      DEF PROCbubblesort(a%(), n%)
      LOCAL i%, l%
      REPEAT
        l% = 0
        FOR i% = 1 TO n%-1
          IF a%(i%-1) > a%(i%) THEN
            SWAP a%(i%-1),a%(i%)
            l% = i%
          ENDIF
        NEXT
        n% = l%
      UNTIL l% = 0
      ENDPROC

      DEF PROCinsertionsort(a%(), n%)
      LOCAL i%, j%, t%
      FOR i% = 1 TO n%-1
        t% = a%(i%)
        j% = i%
        WHILE j%>0 AND t%<a%(ABS(j%-1))
          a%(j%) = a%(j%-1)
          j% -= 1
        ENDWHILE
        a%(j%) = t%
      NEXT
      ENDPROC

      DEF PROCquicksort(a%(), s%, n%)
      LOCAL l%, p%, r%, t%
      IF n% < 2 THEN ENDPROC
      t% = s% + n% - 1
      l% = s%
      r% = t%
      p% = a%((l% + r%) DIV 2)
      REPEAT
        WHILE a%(l%) < p% l% += 1 : ENDWHILE
        WHILE a%(r%) > p% r% -= 1 : ENDWHILE
        IF l% <= r% THEN
          SWAP a%(l%), a%(r%)
          l% += 1
          r% -= 1
        ENDIF
      UNTIL l% > r%
      IF s% < r% PROCquicksort(a%(), s%, r% - s% + 1)
      IF l% < t% PROCquicksort(a%(), l%, t% - l% + 1 )
      ENDPROC

      DEF PROCshellsort(a%(), n%)
      LOCAL h%, i%, j%, k%
      h% = n%
      WHILE h%
        IF h% = 2 h% = 1 ELSE h% DIV= 2.2
        FOR i% = h% TO n% - 1
          k% = a%(i%)
          j% = i%
          WHILE j% >= h% AND k% < a%(ABS(j% - h%))
            a%(j%) = a%(j% - h%)
            j% -= h%
          ENDWHILE
          a%(j%) = k%
        NEXT
      ENDWHILE
      ENDPROC

      DEF PROCradixsort(a%(), n%, r%)
      LOCAL d%, e%, i%, l%, m%, b%(), bucket%()
      DIM b%(DIM(a%(),1)), bucket%(r%-1)
      FOR i% = 0 TO n%-1
        IF a%(i%) < l% l% = a%(i%)
        IF a%(i%) > m% m% = a%(i%)
      NEXT
      a%() -= l%
      m% -= l%
      e% = 1
      WHILE m% DIV e%
        bucket%() = 0
        FOR i% = 0 TO n%-1
          bucket%(a%(i%) DIV e% MOD r%) += 1
        NEXT
        FOR i% = 1 TO r%-1
          bucket%(i%) += bucket%(i% - 1)
        NEXT
        FOR i% = n%-1 TO 0 STEP -1
          d% = a%(i%) DIV e% MOD r%
          bucket%(d%) -= 1
          b%(bucket%(d%)) = a%(i%)
        NEXT
        a%() = b%()
        e% *= r%
      ENDWHILE
      a%() += l%
      ENDPROC
```

'''Output:'''

```txt

Array size:               1000     10000    100000

Data set to all ones:
Internal (lib)            0.00      0.01      0.03
Quicksort                 0.02      0.27      3.18
Radix sort                0.01      0.05      0.53
Shellsort                 0.03      0.36      4.44
Bubblesort                0.00      0.01      0.09
Insertion sort            0.00      0.02      0.26

Data ascending sequence:
Internal (lib)            0.00      0.00      0.02
Quicksort                 0.02      0.15      1.82
Radix sort                0.02      0.18      2.10
Shellsort                 0.03      0.37      4.44
Bubblesort                0.00      0.01      0.09
Insertion sort            0.01      0.03      0.27

Data randomly shuffled:
Internal (lib)            0.00      0.02      0.44
Quicksort                 0.02      0.26      3.17
Radix sort                0.02      0.17      2.08
Shellsort                 0.04      0.73     11.57
Bubblesort                0.69     69.70   >100.00
Insertion sort            0.55     55.54   >100.00

Data descending sequence:
Internal (lib)            0.00      0.01      0.10
Quicksort                 0.01      0.15      1.90
Radix sort                0.02      0.17      2.06
Shellsort                 0.03      0.50      6.39
Bubblesort                0.95     94.77   >100.00
Insertion sort            1.11   >100.00   >100.00

```



## C

(The reference example is [[Measure relative performance of sorting algorithms implementations#Python|Python]])

### Examples of sorting routines

We can use the codes in the category [[:Category:Sorting Algorithms|Sorting Algorithms]]; since these codes deal with integer arrays, we should change them a little. To accomplish this task I've also renamed them more consistently <tt>algorithm_sort</tt>; so we have e.g. <tt>bubble_sort</tt>, <tt>quick_sort</tt> and so on.

### Sequence generators

<tt>csequence.h</tt>

```c
#ifndef _CSEQUENCE_H
#define _CSEQUENCE_H
#include <stdlib.h>

void setfillconst(double c);
void fillwithconst(double *v, int n);
void fillwithrrange(double *v, int n);
void shuffledrange(double *v, int n);
#endif
```

<tt>csequence.c</tt>

```c
#include "csequence.h"

static double fill_constant;

void setfillconst(double c)
{
  fill_constant = c;
}

void fillwithconst(double *v, int n)
{
  while( --n >= 0 ) v[n] = fill_constant;
}

void fillwithrrange(double *v, int n)
{
  int on = n;
  while( --on >= 0 ) v[on] = n - on;
}

void shuffledrange(double *v, int n)
{
  int on = n;
  fillwithrrange(v, n);
  while( --n >= 0 ) {
    int r = rand() % on;
    double t = v[n];
    v[n] = v[r];
    v[r] = t;
  }
}
```


### Write timings

We shall use the code from [[Query Performance]]. Since the ''action'' is a generic function with a single argument, we need ''wrappers'' which encapsule each sorting algorithms we want to test.

<tt>writetimings.h</tt>

```c
#ifndef _WRITETIMINGS_H
#define _WRITETIMINGS_H
#include "sorts.h"
#include "csequence.h"
#include "timeit.h"

/* we repeat the same MEANREPEAT times, and get the mean; this *should*
   give "better" results ... */
#define MEANREPEAT 10.0
#define BUFLEN 128
#define MAKEACTION(ALGO) \
  int action_ ## ALGO (int size) {				\
    ALGO ## _sort(tobesorted, size);				\
    return 0; }
#define MAKEPIECE(N) { #N , action_ ## N }

int action_bubble(int size);
int action_shell(int size);
int action_quick(int size);
int action_insertion(int size);
int action_merge(int size);
int doublecompare( const void *a, const void *b );
int action_qsort(int size);
int get_the_longest(int *a);

struct testpiece
{
  const char *name;
  int (*action)(int);
};
typedef struct testpiece testpiece_t;

struct seqdef
{
  const char *name;
  void (*seqcreator)(double *, int);
};
typedef struct seqdef seqdef_t;
#endif
```

<tt>writetimings.c</tt>

```c
#include <stdio.h>
#include <stdlib.h>

#include "writetimings.h"

double *tobesorted = NULL;
const char *bname = "data_";
const char *filetempl = "%s%s_%s.dat";
int datlengths[] = {100, 200, 300, 500, 1000, 5000, 10000, 50000, 100000};

testpiece_t testpieces[] =
{
//  MAKEPIECE(bubble),
  MAKEPIECE(shell),
  MAKEPIECE(merge),
  MAKEPIECE(insertion),
  MAKEPIECE(quick),
  MAKEPIECE(qsort),
  { NULL, NULL }
};

seqdef_t seqdefs[] =
{
  { "c1", fillwithconst },
  { "rr", fillwithrrange },
  { "sr", shuffledrange },
  { NULL, NULL }
};


MAKEACTION(bubble)
MAKEACTION(insertion)
MAKEACTION(quick)
MAKEACTION(shell)

int action_merge(int size)
{
  double *res = merge_sort(tobesorted, size);
  free(res); /* unluckly this affects performance */
  return 0;
}

int doublecompare( const void *a, const void *b )
{
  if ( *(const double *)a < *(const double *)b ) return -1;
  else return *(const double *)a > *(const double *)b;
}
int action_qsort(int size)
{
  qsort(tobesorted, size, sizeof(double), doublecompare);
  return 0;
}

int get_the_longest(int *a)
{
  int r = *a;
  while( *a > 0 ) {
    if ( *a > r ) r = *a;
    a++;
  }
  return r;
}


int main()
{
  int i, j, k, z, lenmax;
  char buf[BUFLEN];
  FILE *out;
  double thetime;

  lenmax = get_the_longest(datlengths);
  printf("Bigger data set has %d elements\n", lenmax);
  tobesorted = malloc(sizeof(double)*lenmax);
  if ( tobesorted == NULL ) return 1;

  setfillconst(1.0);

  for(i=0; testpieces[i].name != NULL; i++) {
    for(j=0; seqdefs[j].name != NULL; j++) {
      snprintf(buf, BUFLEN, filetempl, bname, testpieces[i].name,
	       seqdefs[j].name);
      out = fopen(buf, "w");
      if ( out == NULL ) goto severe;
      printf("Producing data for sort '%s', created data type '%s'\n",
	     testpieces[i].name, seqdefs[j].name);
      for(k=0; datlengths[k] > 0; k++) {
	printf("\tNumber of elements: %d\n", datlengths[k]);
	thetime = 0.0;
	seqdefs[j].seqcreator(tobesorted, datlengths[k]);
	fprintf(out, "%d ", datlengths[k]);
	for(z=0; z < MEANREPEAT; z++) {
	  thetime += time_it(testpieces[i].action, datlengths[k]);
	}
	thetime /= MEANREPEAT;
	fprintf(out, "%.8lf\n", thetime);
      }
      fclose(out);
    }
  }
severe:
  free(tobesorted);
  return 0;
}
```

This code produce several files with the following naming convention:
* data_''algorithm''_''sequence''.dat
Where ''algorithm'' is one of the following: insertion, merge, shell, quick, qsort (the quicksort in the libc library) (bubble sort became too slow for longest sequences). ''Sequence'' is ''c1'' (constant value 1), ''rr'' (reverse range), ''sr'' (shufled range). These data can be easily plotted by Gnuplot, which can also do fitting. Instead we do our fitting using [[Polynomial Fitting]].

```c
#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#include "polifitgsl.h"

#define MAXNUMOFDATA 100

double x[MAXNUMOFDATA], y[MAXNUMOFDATA];
double cf[2];

int main()
{
  int i, nod;
  int r;

  for(i=0; i < MAXNUMOFDATA; i++)
  {
    r = scanf("%lf %lf\n", &x[i], &y[i]);
    if ( (r == EOF) || (r < 2) ) break;
    x[i] = log10(x[i]);
    y[i] = log10(y[i]);
  }
  nod = i;

  polynomialfit(nod, 2, x, y, cf);
  printf("C0 = %lf\nC1 = %lf\n", cf[0], cf[1]);

  return 0;
}
```

Here we search for a fit with C<sub>0</sub>+C<sub>1</sub>x "in the log scale", since we supposed the data, once plotted on a logscale graph, can be fitted by a line. We can use e.g. a shell one-liner to produce the parameters for the line for each data file previously output. In particular I've used the following

```txt
for el in *.dat ; do fitdata <$el >${el%.dat}.fd ; done
```



### Plot timings and Figures

Once we have all the ".dat" files and associated ".fd", we can use Gnuplot to draw our '''data''' and think about conclusions (we could also use the idea in [[Plot x, y arrays]], but it needs too much enhancements to be usable for this analysis). Here an example of such a draw for a single file (using Gnuplot)

```txt

gnuplot> f(x) = C0 + C1*x
gnuplot> set logscale xy
gnuplot> load 'data_quick_sr_u.fd'
gnuplot> set xrange [100:100000]
gnuplot> set key left
gnuplot> plot 10**f(log10(x)), 'data_quick_sr_u.dat'

```

(The <tt>_u.dat</tt> are produced by a modified version of the code in order to write timings in microseconds instead of seconds)
We can easily write another shell script/one-liner to produce a single file ''driver'' for Gnuplot in order to produce all the graph we can be interested in.
These graphs show that the linear (in log scale) fit do not always fit the data... I haven't repeated the tests; the problems are when the sequence length becomes huge; for some algorithm that uses extra memory (like implementation of the merge sort), this could depend on the allocation of the needed memory. Another ''extraneous'' factor could be system load (the CLOCK_MONOTONIC used by the timing function is system wide rather than per process, so counting time spent in other processes too?).
The "most stable" algorithms seem to be quick sort (but not qsort, which indeed is just the libc quick sort, here not plotted!) and shell sort (except for reversed range).

Conclusion: we should repeat the tests...
* [http://i40.tinypic.com/2dabyb7.png sequences of 1]
* [http://i43.tinypic.com/2nqrzfs.png reversed range]
* [http://i41.tinypic.com/24q8hmg.png shuffled range]



## D


```d
import std.stdio, std.algorithm, std.container, std.datetime,
       std.random, std.typetuple;

immutable int[] allOnes, sortedData, randomData;

static this() { // Initialize global Arrays.
    immutable size_t arraySize = 10_000;

    allOnes = new int[arraySize];
    //allOnes[] = 1;
    foreach (ref d; allOnes)
        d = 1;

    sortedData = new int[arraySize];
    foreach (immutable i, ref d; sortedData)
        d = i;

    randomData = new int[arraySize];
    foreach (ref d; randomData)
        d = uniform(0, int.max);
}

// BubbleSort:

void bubbleSort(T)(T[] list) {
    for (int i = list.length - 1; i > 0; i--)
        for (int j = i -1; j >= 0; j--)
            if (list[i] < list[j])
                swap(list[i], list[j]);
}

void allOnesBubble() {
    auto data = allOnes.dup;
    data.bubbleSort;
    assert(data.isSorted);
}

void sortedBubble() {
    auto data = sortedData.dup;
    data.bubbleSort;
    assert(data.isSorted);
}

void randomBubble() {
    auto data = randomData.dup;
    data.bubbleSort;
    assert(data.isSorted);
}

// InsertionSort:

void insertionSort(T)(T[] list) {
    foreach (immutable i, currElem; list) {
        size_t j = i;
        for (; j > 0 && currElem < list[j - 1]; j--)
            list[j] = list[j - 1];
        list[j] = currElem;
    }
}

void allOnesInsertion() {
    auto data = allOnes.dup;
    data.insertionSort;
    assert(data.isSorted);
}

void sortedInsertion() {
    auto data = sortedData.dup;
    data.insertionSort;
    assert(data.isSorted);
}

void randomInsertion() {
    auto data = randomData.dup;
    data.insertionSort;
    assert(data.isSorted);
}

// HeapSort:

void heapSort(T)(T[] data) {
    auto h = data.heapify;
    while (!h.empty)
        h.removeFront;
}

void allOnesHeap() {
    auto data = allOnes.dup;
    data.heapSort;
    assert(data.isSorted);
}

void sortedHeap() {
    auto data = sortedData.dup;
    data.heapSort;
    assert(data.isSorted);
}

void randomHeap() {
    auto data = randomData.dup;
    data.heapSort;
    assert(data.isSorted);
}

// Built-in sort:

void allOnesBuiltIn() {
    auto data = allOnes.dup;
    data.sort!q{a < b};
    assert(data.isSorted);
}

void sortedBuiltIn() {
    auto data = sortedData.dup;
    data.sort!q{a < b};
    assert(data.isSorted);
}

void randomBuiltIn() {
    auto data = randomData.dup;
    data.sort!q{a < b};
    assert(data.isSorted);
}

static void show(in TickDuration[4u] r) {
    alias args = TypeTuple!("usecs", int);
    writefln("    Bubble Sort:    %10d", r[0].to!args);
    writefln("    Insertion Sort: %10d", r[1].to!args);
    writefln("    Heap Sort:      %10d", r[3].to!args);
    writefln("    Built-in Sort:  %10d", r[2].to!args);
}

void main() {
    enum nRuns = 100;
    writeln("Timings in microseconds:");

    writeln("  Testing against all ones:");
    nRuns.benchmark!(allOnesBubble, allOnesInsertion,
                     allOnesHeap, allOnesBuiltIn).show;

    writeln("\n  Testing against sorted data.");
    nRuns.benchmark!(sortedBubble, sortedInsertion,
                     sortedHeap, sortedBuiltIn).show;

    writeln("\n  Testing against random data.");
    nRuns.benchmark!(randomBubble, randomInsertion,
                     randomHeap, randomBuiltIn).show;
}
```

```txt
Timings in microseconds:
  Testing against all ones:
    Bubble Sort:       7377065
    Insertion Sort:       5868
    Heap Sort:           25173
    Built-in Sort:       34538

  Testing against sorted data.
    Bubble Sort:       7370520
    Insertion Sort:       6006
    Heap Sort:           18127
    Built-in Sort:      176235

  Testing against random data.
    Bubble Sort:      27293705
    Insertion Sort:    3762374
    Heap Sort:           85053
    Built-in Sort:      218268
```

(With 10,000 elements in each array. A naive HeapSort seems faster than the built-in sort in all three cases.)


## Erlang

The sort routines are borrowed from [http://rosettacode.org/wiki/Sorting_algorithms/Bubble_sort bubble sort], [http://rosettacode.org/wiki/Sorting_algorithms/Insertion_sort insertion sort] and [http://rosettacode.org/wiki/Sorting_algorithms/Quicksort quick sort]. Plots by [https://github.com/psyeugenic/eplot eplot].
Bubble sort does [http://github.com/ebengt/rosettacode/tree/master/graphs/ones.png ones] and [http://github.com/ebengt/rosettacode/tree/master/graphs/ranges.png ranges] best. Insertion sort does [http://github.com/ebengt/rosettacode/tree/master/graphs/reversed_ranges.png reversed ranges] best. Quick sort handles [http://github.com/ebengt/rosettacode/tree/master/graphs/shuffleds.png shuffled numbers] best.

```Erlang

-module( compare_sorting_algorithms ).

-export( [task/0] ).

task() ->
	Ns = [100, 1000, 10000],
	Lists = [{"ones", fun list_of_ones/1, Ns}, {"ranges", fun list_of_ranges/1, Ns}, {"reversed_ranges", fun list_of_reversed_ranges/1, Ns}, {"shuffleds", fun list_of_shuffleds/1, Ns}],
	Sorts = [{bubble_sort, fun bubble_sort:list/1}, {insertion_sort, fun sort:insertion/1}, {iquick_sort, fun quicksort:qsort/1}],
	Results = [time_list(X, Sorts) || X <- Lists],
	[file:write_file(X++".png", egd_chart:graph(Y, [{x_label,  "log N"}, {y_label, "log ms"}])) || {X, Y} <- Results].


list_of_ones( N ) -> [1 || _X <- lists:seq(1, N)].
list_of_ranges( N ) -> [X || X <- lists:seq(1, N)].
list_of_reversed_ranges( N ) -> lists:reverse( list_of_ranges(N) ).
list_of_shuffleds( N ) -> [random:uniform(N) || _X <- lists:seq(1, N)].

time_list( {List, List_fun, Values}, Sorts ) ->
	Results = [{Sort, time_sort(Sort_fun, List_fun, Values)} || {Sort, Sort_fun} <- Sorts],
	{List, Results}.

time_sort( Sort_fun, List_fun, Values ) ->
	[time(Sort_fun, List_fun, X) || X <- Values].

time( Fun, List_fun, N ) ->
	{Time, _Result} = timer:tc( fun() -> Fun( List_fun(N) ) end ),
	{math:log10(N), math:log10(Time)}.

```



## Go

```go
package main

import (
    "log"
    "math/rand"
    "testing"
    "time"

    "github.com/gonum/plot"
    "github.com/gonum/plot/plotter"
    "github.com/gonum/plot/plotutil"
    "github.com/gonum/plot/vg"
)

// Step 1, sort routines.
// These functions are copied without changes from the RC tasks Bubble Sort,
// Insertion sort, and Quicksort.

func bubblesort(a []int) {
    for itemCount := len(a) - 1; ; itemCount-- {
        hasChanged := false
        for index := 0; index < itemCount; index++ {
            if a[index] > a[index+1] {
                a[index], a[index+1] = a[index+1], a[index]
                hasChanged = true
            }
        }
        if hasChanged == false {
            break
        }
    }
}

func insertionsort(a []int) {
    for i := 1; i < len(a); i++ {
        value := a[i]
        j := i - 1
        for j >= 0 && a[j] > value {
            a[j+1] = a[j]
            j = j - 1
        }
        a[j+1] = value
    }
}

func quicksort(a []int) {
    var pex func(int, int)
    pex = func(lower, upper int) {
        for {
            switch upper - lower {
            case -1, 0:
                return
            case 1:
                if a[upper] < a[lower] {
                    a[upper], a[lower] = a[lower], a[upper]
                }
                return
            }
            bx := (upper + lower) / 2
            b := a[bx]
            lp := lower
            up := upper
        outer:
            for {
                for lp < upper && !(b < a[lp]) {
                    lp++
                }
                for {
                    if lp > up {
                        break outer
                    }
                    if a[up] < b {
                        break
                    }
                    up--
                }
                a[lp], a[up] = a[up], a[lp]
                lp++
                up--
            }
            if bx < lp {
                if bx < lp-1 {
                    a[bx], a[lp-1] = a[lp-1], b
                }
                up = lp - 2
            } else {
                if bx > lp {
                    a[bx], a[lp] = a[lp], b
                }
                up = lp - 1
                lp++
            }
            if up-lower < upper-lp {
                pex(lower, up)
                lower = lp
            } else {
                pex(lp, upper)
                upper = up
            }
        }
    }
    pex(0, len(a)-1)
}

// Step 2.0 sequence routines.  2.0 is the easy part.  2.5, timings, follows.

func ones(n int) []int {
    s := make([]int, n)
    for i := range s {
        s[i] = 1
    }
    return s
}

func ascending(n int) []int {
    s := make([]int, n)
    v := 1
    for i := 0; i < n; {
        if rand.Intn(3) == 0 {
            s[i] = v
            i++
        }
        v++
    }
    return s
}

func shuffled(n int) []int {
    return rand.Perm(n)
}

// Steps 2.5 write timings, and 3 plot timings are coded together.
// If write means format and output human readable numbers, step 2.5
// is satisfied with the log output as the program runs.  The timings
// are plotted immediately however for step 3, not read and parsed from
// any formated output.
const (
    nPts = 7    // number of points per test
    inc  = 1000 // data set size increment per point
)

var (
    p        *plot.Plot
    sortName = []string{"Bubble sort", "Insertion sort", "Quicksort"}
    sortFunc = []func([]int){bubblesort, insertionsort, quicksort}
    dataName = []string{"Ones", "Ascending", "Shuffled"}
    dataFunc = []func(int) []int{ones, ascending, shuffled}
)

func main() {
    rand.Seed(time.Now().Unix())
    var err error
    p, err = plot.New()
    if err != nil {
        log.Fatal(err)
    }
    p.X.Label.Text = "Data size"
    p.Y.Label.Text = "microseconds"
    p.Y.Scale = plot.LogScale{}
    p.Y.Tick.Marker = plot.LogTicks{}
    p.Y.Min = .5 // hard coded to make enough room for legend

    for dx, name := range dataName {
        s, err := plotter.NewScatter(plotter.XYs{})
        if err != nil {
            log.Fatal(err)
        }
        s.Shape = plotutil.DefaultGlyphShapes[dx]
        p.Legend.Add(name, s)
    }
    for sx, name := range sortName {
        l, err := plotter.NewLine(plotter.XYs{})
        if err != nil {
            log.Fatal(err)
        }
        l.Color = plotutil.DarkColors[sx]
        p.Legend.Add(name, l)
    }
    for sx := range sortFunc {
        bench(sx, 0, 1) // for ones, a single timing is sufficient.
        bench(sx, 1, 5) // ascending and shuffled have some randomness though,
        bench(sx, 2, 5) // so average timings on 5 different random sets.
    }

    if err := p.Save(5*vg.Inch, 5*vg.Inch, "comp.png"); err != nil {
        log.Fatal(err)
    }
}

func bench(sx, dx, rep int) {
    log.Println("bench", sortName[sx], dataName[dx], "x", rep)
    pts := make(plotter.XYs, nPts)
    sf := sortFunc[sx]
    for i := range pts {
        x := (i + 1) * inc
        // to avoid timing sequence creation, create sequence before timing
        // then just copy the data inside the timing loop.  copy time should
        // be the same regardless of sequence data.
        s0 := dataFunc[dx](x) // reference sequence
        s := make([]int, x)   // working copy
        var tSort int64
        for j := 0; j < rep; j++ {
            tSort += testing.Benchmark(func(b *testing.B) {
                for i := 0; i < b.N; i++ {
                    copy(s, s0)
                    sf(s)
                }
            }).NsPerOp()
        }
        tSort /= int64(rep)
        log.Println(x, "items", tSort, "ns") // step 2.5, write timings
        pts[i] = struct{ X, Y float64 }{float64(x), float64(tSort) * .001}
    }
    pl, ps, err := plotter.NewLinePoints(pts) // step 3, plot timings
    if err != nil {
        log.Fatal(err)
    }
    pl.Color = plotutil.DarkColors[sx]
    ps.Color = plotutil.DarkColors[sx]
    ps.Shape = plotutil.DefaultGlyphShapes[dx]
    p.Add(pl, ps)
}
```

[[file:GoComp.png|right|Comparison]]
Step 4, conclusions about relative performance of the sorting routines made based on the plots.

The plots show differences in best and worse case performance for the various data sets.  Bubble and insertion sorts show very good best case performance with all one and ascending sequences, beating quicksort.  Quicksort shows best case performance with the ascending sequence but worst case performance with the all one sequence.

On random data (triangles) insertion and bubble sort show worse performance than quicksort.
<br clear=all>


## J


```j

NB. extracts from other rosetta code projects
ts=: 6!:2, 7!:2@]
radix =: 3 : 0
256 radix y
:
a=. #{. z =. x #.^:_1 y
e=. (-a) {."0 b =. i.x
x#.1{::(<:@[;([: ; (b, {"1) <@}./. e,]))&>/^:a [ z;~a-1
NB. , ([: ; (b, {:"1) <@(}:"1@:}.)/. e,])^:(#{.z) y,.z
)
bubble=:  (([ (<. , >.) {.@]) , }.@])/^:_
insertion=:((>: # ]) , [ , < #])/
sel=: 1 : 'x # ['
quick=: 3 : 0
 if.  1 >: #y do.  y
 else.
  e=. y{~?#y
  (quick y <sel e),(y =sel e),quick y >sel e
 end.
)
gaps      =: [: }: 1 (1+3*])^:(> {:)^:a:~ #
insert    =: (I.~ {. ]) , [ , ] }.~ I.~
gapinss   =: #@] {. ,@|:@(] insert//.~ #@] $ i.@[)
shell =: [: ; gapinss &.>/@(< ,~ ]&.>@gaps)
builtin =: /:~



NB. characterization of the sorting algorithms.

sorts =: bubble`insertion`shell`quick`radix`builtin
generators =: #&1`(i.@-)`(?.~) NB. data generators

round =: [: <. 1r2&+

ll =: (<_1 0)&{  NB. verb to extract lower left which holds ln data length
lc =: (<_1 1)&{  NB. verb to fetch lower center which holds most recent time

NB. maximum_time characterize ln_start_size
NB. characterize returns a rank 4 matrix with successive indexes for
NB. algorithm, input arrangement, max number of tests in group, length time space
characterize =: 4 : 0
  max_time =. x
  start =. 1 3{.<:y
  for_sort. sorts do.
    for_generator. generators do.                                           NB. limit time  and  paging prevention
      t =: }. (, (, [: ts 'sort@.0 (generator@.0)' , ":@round@^)@>:@ll) ^: ((lc < max_time"_) *. ll < 17"_) ^:_ start
      if. generator -: {.generators do.
        g =. ,:t
      else.
        g =. g,t
      end.
    end.
    if. sort -: {.sorts do.
      s =. ,:g
    else.
      s =. s,g
    end.
  end.
)

NB. character cell graphics

NB. From j phrases 10E. Approximation
d3=: 1&,.@[ %.~ ]	NB. a and b such that y is approx. a + b*x

NB. domain and range 0 to 14.
D=:14

plot =: 1 : '(=/ round@(u&.(*&(D%<:y))))i.y' NB. function plot size
points =: 4 : '1(<"1|:|.round y*D%~<:x)}0$~2#x'  NB. size points x,:y

show =: [: |. [: '0'&~:@{:} ' ' ,: ":

plt =: 3 : 0
30 plt y NB. default size 30
:
n =. >:i.-# experiments =. <@(#~"1 (0&<)@{.)"2 y
pts =. n +./ .*x&points@>experiments
coef =. d3/@>experiments
(_*pts) + n +./ .*1 0 2|:coef&(p."1) plot x
)

```



```txt

   a =: 1 characterize 5
   $a  NB. a has rank 4
6 3 13 3

   'l t s' =: |:a   NB. transpose moves length time space to leading dimension
   l =: |: <: l     NB. transpose restores order
   t =:	|: 12 +^. t NB. choose arbitrary time units so that ^. time is positive
   s =:	|: ^. s     NB. ln space

   NB. 6 groups of sort methods   with 3 arrangements of data    ---> exponentially increasing data lengths,
   6j2":t   NB. ln time         negative infinity indicates avoided experiment
  3.83  4.80  5.89  7.15  8.65 10.18 11.90 13.75    __    __    __    __    __
  8.77 10.90 13.13    __    __    __    __    __    __    __    __    __    __
  8.70 10.87 13.11    __    __    __    __    __    __    __    __    __    __

  3.91  5.43  7.13  8.90 10.76 12.73    __    __    __    __    __    __    __
  3.99  5.63  7.42  9.21 11.13 13.06    __    __    __    __    __    __    __
  4.14  5.72  7.58  9.37 11.30 13.26    __    __    __    __    __    __    __

  5.56  6.75  7.90  9.05 10.27 11.60 13.13    __    __    __    __    __    __
  5.61  6.88  8.05  9.40 10.84 12.48    __    __    __    __    __    __    __
  5.67  6.93  8.09  9.43 10.89 12.48    __    __    __    __    __    __    __

  1.95  1.99  2.19  2.46  2.98  3.66  4.63  5.54  6.60  7.61  8.70  9.81 10.99
  6.06  7.13  8.09  9.09 10.10 11.10 12.12    __    __    __    __    __    __
  6.09  7.17  8.10  9.11 10.11 11.12 12.14    __    __    __    __    __    __

  3.25  3.33  3.55  4.06  4.77  5.60  6.72  7.75  8.75 10.11 11.21 12.22    __
  3.37  3.87  4.19  4.72  5.53  6.49  7.84  9.29 10.71 11.78 12.88    __    __
  3.42  4.00  4.43  5.07  5.93  7.07  8.06  9.76 10.96 12.10    __    __    __

  0.38  0.38  0.58  1.02  1.68  2.68  3.45  4.42  5.42  6.51  7.64  8.86  9.87
  0.49  0.58  0.96  1.55  2.34  3.11  4.31  5.82  7.43  8.70  9.75 10.75 11.75
  1.40  2.01  2.88  3.87  4.92  5.92  7.05  8.18  9.45 10.91 12.01    __    __

```


This display  is no less than  a bar chart and,  frankly, suffices but
for the curve fit requirement.



```txt

   NB. algorithms: bubble 6,  insertion 5,  shell 4,  quick 3,  radix 2,  builtin 1
   NB. rows:  log time
   NB. cols:  log size

   NB. data is all 1
   show 30 plt l ,: & (0&{)"2 t
                        5   4 _                       2 2
                          4                         2
                      5 _   6                     2
                    _   4 6                     2
                      4 _                     _           3
                  5 _   6                   2           3
                      6                   _       _   3   1
                _ 4                     2         3 3   1
                _   _                 _         3     1
              1   6                 2         _   _ 1
            _                     2         3   1
            _   _               2 _       _   _
          4                   2         3   1
        _ 5   6             2 _     3 _   _
      4 _   _             2       3     1
    _                   _       3 _   _
      5   6           2       3     1
  4     _           _       3 _   1
4   _             2       3     1 _
    _ 6       2 _     3 3   1 1
  5 6       2       3   _ 1   _
          2 _     3 _   1
5 6 _   _       3     1 _
6     2       3 _   1
    2   _   _     1 _
  2 _   3 3     1
2     3       1 _
    3       _
  3 _   _ 1
3     1 1

   NB. data is  reversed integer list
   show 30 plt l ,: & (1&{)"2 t
          6               4     3                         1
                      5 4     3               2         1
        _           _ 4     3             _ 2         1
                    _     3               2         1
      6             4   _               2         _
                  1   3               _
    _           _   _               2           1
    6           4 3               _           _
              1 _               2           1
  6         _ 3               2           _
            _               2 _         1
6         1               2           _
        _ 5             2           1
      3               2 _         1
    _ 4 _           2             _
  3 _                           1
3     5           2 _         1
  4 _           2           1 _
4   5         2 _         1
            _           1
  5     _ 2           1 _
5   _   2           1
      2           1 _
    2
  2             _
2             1
            _
        _ 1
    _   1
      1

   NB. data is  random
   show 30 plt l ,: & (2&{)"2 t
          6           5   4     3             2   1
                    _   4     3             2   1
        _           5 4     3             2   1
                    _     3             2   1
      6           5 4   _             _   _
                  4   3             2     1
    _           _   _             _   _ 1
    6           4 3                   1
              1 _               2   1
  6         _ 3               _   _
            _               2   1
6         1               2   1
        _ 5             _   1 _
      3 _             2   1
    _ 4 5           2   1
  3 _ 5           2 _ 1 _
3 4             2
    _         2 _   _
4           _     1
  5       2     _
        _     1
5   _ 2     _
    2     1
  2     _
2   _ 1
    1
  1
1



```


The first version of the code set only a time limit.  The builtin sort violated this only when the data overflowed RAM into virtual space, causing a large jump in time affecting also the next data set as the OS restored itself.  The timing might be interesting for some other exercise.  Here, a maximum data size test was inserted.  Arbitrary time is the reasonable choice without details of the J interpreter nor of specific hardware.  The radix sort involves putting data directly into the right spot.  It is quick!

The data fit curves of the character cell graph were combined with GCD +. function.  This explains "1"s or other strange values where these curves intersect.  Finally the scatter plots were multiplied by infinity and added to the best fit curves.  The points didn't show up well using the same values as the curves.



## Julia

Julia comes with the InsertionSort, MergeSort, and QuickSort routines built into the Base.Sort module. Here is a comparison using those system algorithms and with integers.

```julia

function comparesorts(tosort)
    a = shuffle(["i", "m", "q"])
    iavg = mavg = qavg = 0.0
    for c in a
        if c == "i"
            iavg = sum(i -> @elapsed(sort(tosort, alg=InsertionSort)), 1:100) / 100.0
        elseif c == "m"
            mavg = sum(i -> @elapsed(sort(tosort, alg=MergeSort)), 1:100) / 100.0
        elseif c == "q"
            qavg = sum(i -> @elapsed(sort(tosort, alg=QuickSort)), 1:100) / 100.0
        end
    end
    iavg, mavg, qavg
end

allones = fill(1, 40000)
sequential = collect(1:40000)
randomized = collect(shuffle(1:40000))

comparesorts(allones)
comparesorts(allones)
iavg, mavg, qavg = comparesorts(allones)
println("Average sort times for 40000 ones:")
println("\tinsertion sort:\t$iavg\n\tmerge sort:\t$mavg\n\tquick sort\t$qavg")

comparesorts(sequential)
comparesorts(sequential)
iavg, mavg, qavg = comparesorts(sequential)
println("Average sort times for 40000 presorted:")
println("\tinsertion sort:\t$iavg\n\tmerge sort:\t$mavg\n\tquick sort\t$qavg")

comparesorts(randomized)
comparesorts(randomized)
iavg, mavg, qavg = comparesorts(randomized)
println("Average sort times for 40000 randomized:")
println("\tinsertion sort:\t$iavg\n\tmerge sort:\t$mavg\n\tquick sort\t$qavg")

```


```txt
Average sort times for 40000 ones:
        insertion sort: 0.00036058316000000005
        merge sort:     0.00040099004999999996
        quick sort      0.0003586394400000001
Average sort times for 40000 presorted:
        insertion sort: 0.0003141142199999999
        merge sort:     0.0007967360000000003
        quick sort      0.0005601127399999998
Average sort times for 40000 randomized:
        insertion sort: 0.2190664327599999
        merge sort:     0.0028818907399999986
        quick sort      0.0023325933899999997

```



## Kotlin

This mostly reuses the code from the sorting sub-tasks except that:

1. All sorting functions have been adjusted where necessary so that they sort an IntArray 'in place'. This ensures that the timings are not affected by time spent copying arrays.

2. The bubble sort function, which is very slow when sorting 100,000 random numbers, has been optimized somewhat to try and reduce overall execution time, though the program is still taking about 5 minutes to run on my machine.

Unfortunately the code used to measure CPU time in the 'Time a function' sub-task no longer works properly on my present Windows 10 machine (many results are inexplicably zero). I've therefore had to use the Kotlin library function, measureNanoTime(), instead which measures system time elapsed. Consequently, the results are a bit erratic even when averaged over 10 runs.

Although it would be easy enough to plot the results graphically using an external library such as JFreePlot, there doesn't seem much point when we can no longer upload images to RC. I've therefore presented the results in tabular form on the terminal which is good enough to detect significant trends.

```scala
// Version 1.2.31

import java.util.Random
import kotlin.system.measureNanoTime

typealias Sorter = (IntArray) -> Unit

val rand = Random()

fun onesSeq(n: Int) = IntArray(n) { 1 }

fun ascendingSeq(n: Int) = shuffledSeq(n).sorted().toIntArray()

fun shuffledSeq(n: Int) = IntArray(n) { 1 + rand.nextInt(10 * n) }

fun bubbleSort(a: IntArray) {
    var n = a.size
    do {
        var n2 = 0
        for (i in 1 until n) {
            if (a[i - 1] > a[i]) {
                val tmp = a[i]
                a[i] = a[i - 1]
                a[i - 1] = tmp
                n2 = i
            }
        }
        n = n2
    } while (n != 0)
}

fun insertionSort(a: IntArray) {
    for (index in 1 until a.size) {
        val value = a[index]
        var subIndex = index - 1
        while (subIndex >= 0 && a[subIndex] > value) {
            a[subIndex + 1] = a[subIndex]
            subIndex--
        }
        a[subIndex + 1] = value
    }
}

fun quickSort(a: IntArray) {
    fun sorter(first: Int, last: Int) {
        if (last - first < 1) return
        val pivot = a[first + (last - first) / 2]
        var left = first
        var right = last
        while (left <= right) {
            while (a[left] < pivot) left++
            while (a[right] > pivot) right--
            if (left <= right) {
                val tmp = a[left]
                a[left] = a[right]
                a[right] = tmp
                left++
                right--
            }
        }
        if (first < right) sorter(first, right)
        if (left < last) sorter(left, last)
    }
    sorter(0, a.lastIndex)
}

fun radixSort(a: IntArray) {
    val tmp = IntArray(a.size)
    for (shift in 31 downTo 0) {
        tmp.fill(0)
        var j = 0
        for (i in 0 until a.size) {
            val move = (a[i] shl shift) >= 0
            val toBeMoved = if (shift == 0) !move else move
            if (toBeMoved)
                tmp[j++] = a[i]
            else {
                a[i - j] = a[i]
            }
        }
        for (i in j until tmp.size) tmp[i] = a[i - j]
        for (i in 0 until a.size) a[i] = tmp[i]
    }
}

val gaps = listOf(701, 301, 132, 57, 23, 10, 4, 1)  // Marcin Ciura's gap sequence

fun shellSort(a: IntArray) {
    for (gap in gaps) {
        for (i in gap until a.size) {
            val temp = a[i]
            var j = i
            while (j >= gap && a[j - gap] > temp) {
                a[j] = a[j - gap]
                j -= gap
            }
            a[j] = temp
        }
    }
}

fun main(args: Array<String>) {
    val runs = 10
    val lengths = listOf(1, 10, 100, 1_000, 10_000, 100_000)
    val sorts = listOf<Sorter>(
        ::bubbleSort, ::insertionSort, ::quickSort, ::radixSort, ::shellSort
    )

    /* allow JVM to compile sort functions before timings start */
    for (sort in sorts) sort(intArrayOf(1))

    val sortTitles = listOf("Bubble", "Insert", "Quick ", "Radix ", "Shell ")
    val seqTitles = listOf("All Ones", "Ascending", "Shuffled")
    val totals = List(seqTitles.size) { List(sorts.size) { LongArray(lengths.size) } }
    for ((k, n) in lengths.withIndex()) {
        val seqs = listOf(onesSeq(n), ascendingSeq(n), shuffledSeq(n))
        repeat(runs) {
            for (i in 0 until seqs.size) {
                for (j in 0 until sorts.size) {
                    val seq = seqs[i].copyOf()
                    totals[i][j][k] += measureNanoTime { sorts[j](seq) }
                }
            }
        }
    }
    println("All timings in micro-seconds\n")
    print("Sequence length")
    for (len in lengths) print("%8d   ".format(len))
    println("\n")
    for (i in 0 until seqTitles.size) {
        println("  ${seqTitles[i]}:")
        for (j in 0 until sorts.size) {
            print("    ${sortTitles[j]}     ")
            for (k in 0 until lengths.size) {
                val time = totals[i][j][k] / runs / 1_000
                print("%8d   ".format(time))
            }
            println()
        }
        println("\n")
    }
}
```


```txt

All timings in micro-seconds

Sequence length       1         10        100       1000      10000     100000

  All Ones:
    Bubble            1          2          6         24         26        264
    Insert            1         16         10         14         48        518
    Quick             2          7         18         46        397       5181
    Radix            38         79        501       3720        864       9096
    Shell            11         15         43        189        407       4105


  Ascending:
    Bubble            1          2          6          8         24        270
    Insert            0          2          9         14         47        496
    Quick             1          6         19         33        282       3347
    Radix            38         71        264        415       1869      21403
    Shell             7         10         42        171        399       4052


  Shuffled:
    Bubble            1          5        436       3292     275224   27730705
    Insert            0          3        176        754      24759    2546180
    Quick             1          7         24        106       1281      14982
    Radix            28         73        622        317       1891      21617
    Shell            11         19         88        408       1946      36980

```



### Conclusions

As expected quick sort is faster than the other methods when applied to random data of a reasonable size though radix and shell sort are also respectable performers for large amounts of random data. In contrast, bubble and insertion sorts are orders of magnitude slower, particularly the former.

On the other hand, bubble and insertion sorts are much quicker than the other methods for constant data and for data which is already sorted in an ascending direction, bubble sort being the faster of the two.


## Phix

```Phix
--
-- demo\rosetta\Compare_sorting_algorithms.exw
--
constant XQS = 01  -- (set to 1 to exclude quick_sort and shell_sort from ones)

include pGUI.e

Ihandle dlg, tabs, plot
Ihandles plots

function quick_sort2(sequence x)
integer n = length(x), c, mid, midn
object xi, midval
sequence left = {}, right = {}

    if n<2 then
        return x    -- already sorted (trivial case)
    end if

    mid = floor((n+1)/2)
    midval = x[mid]
    x[mid] = x[1]
    midn = 1

    for i=2 to n do
        xi = x[i]
        c = compare(xi,midval)
        if c<0 then
            left &= xi
        elsif c>0 then
            right &= xi
        else
            midn += 1
        end if
    end for

    return quick_sort2(left) & repeat(midval,midn) & quick_sort2(right)
end function

function quick_sort(sequence s)
sequence qstack
integer first, last, stackptr, I, J, tmp, pivot

    qstack = repeat(0,floor((length(s)/5)+10))    -- create a stack

    first = 1
    last = length(s)
    stackptr = 0
    while 1 do
        while first<last do
            pivot = s[floor(last+first)/2]
            I = first
            J = last
            while 1 do
                while s[I]<pivot do
                    I += 1
                end while
                while s[J]>pivot do
                    J -= 1
                end while
                if I>J then exit end if
                if I<J then
                    tmp = s[I]
                    s[I] = s[J]
                    s[J] = tmp
                end if
                I += 1
                J -= 1
                if I>J then exit end if
            end while
            if I<last then
                qstack[stackptr+1] = I
                qstack[stackptr+2] = last
                stackptr += 2
            end if
            last = J
        end while
        if stackptr=0 then exit end if
        stackptr -= 2
        first = qstack[stackptr+1]
        last = qstack[stackptr+2]
    end while
    return s
end function

function radixSortn(sequence s, integer n)
sequence buckets = repeat({},10)
sequence res = {}
    for i=1 to length(s) do
        integer digit = remainder(floor(s[i]/power(10,n-1)),10)+1
        buckets[digit] = append(buckets[digit],s[i])
    end for
    for i=1 to length(buckets) do
        integer len = length(buckets[i])
        if len!=0 then
            if len=1 or n=1 then
                res &= buckets[i]
            else
                res &= radixSortn(buckets[i],n-1)
            end if
        end if
    end for
    return res
end function

function split_by_sign(sequence s)
sequence buckets = {{},{}}
    for i=1 to length(s) do
        integer si = s[i]
        if si<0 then
            buckets[1] = append(buckets[1],-si)
        else
            buckets[2] = append(buckets[2],si)
        end if
    end for
    return buckets
end function

function radix_sort(sequence s)
integer mins = min(s)
integer passes = max(max(s),abs(mins))
    passes = floor(log10(passes))+1
    if mins<0 then
        sequence buckets = split_by_sign(s)
        buckets[1] = reverse(sq_uminus(radixSortn(buckets[1],passes)))
        buckets[2] = radixSortn(buckets[2],passes)
        s = buckets[1]&buckets[2]
    else
        s = radixSortn(s,passes)
    end if
    return s
end function

function shell_sort(sequence s)
integer gap = floor(length(s)/2), j
object temp
    while gap>0 do
        for i=gap to length(s) do
            temp = s[i]
            j = i-gap
            while j>=1 and temp<=s[j] do
                s[j+gap] = s[j]
                j -= gap
            end while
            s[j+gap] = temp
        end for
        gap = floor(gap/2)
    end while
    return s
end function

function shell_sort2(sequence x)
integer gap, j, first, last
object xi, xj

    last = length(x)
    gap = floor(last/10)+1
    while TRUE do
        first = gap+1
        for i=first to last do
            xi = x[i]
            j = i-gap
            while TRUE do
                xj = x[j]
                if xi>=xj then
                    j += gap
                    exit
                end if
                x[j+gap] = xj
                if j<=gap then
                    exit
                end if
                j -= gap
            end while
            x[j] = xi
        end for
        if gap=1 then
            return x
        else
            gap = floor(gap/3.5)+1
        end if
    end while
end function

function siftDown(sequence arr, integer s, integer last)
integer root = s
    while root*2<=last do
        integer child = root*2
        if child<last and arr[child]<arr[child+1] then
            child += 1
        end if
        if arr[root]>=arr[child] then exit end if
        object tmp = arr[root]
        arr[root] = arr[child]
        arr[child] = tmp
        root = child
    end while
    return arr
end function

function heapify(sequence arr, integer count)
integer s = floor(count/2)
    while s>0 do
        arr = siftDown(arr,s,count)
        s -= 1
    end while
    return arr
end function

function heap_sort(sequence arr)
integer last = length(arr)
    arr = heapify(arr,last)
    while last>1 do
        object tmp = arr[1]
        arr[1] = arr[last]
        arr[last] = tmp
        last -= 1
        arr = siftDown(arr,1,last)
    end while
    return arr
end function

include builtins/sort.e

enum ONES = 1, SORTED = 2, RANDOM = 3, REVERSE = 4

constant tabtitles = {"ones","sorted","random","reverse"}
integer tabidx = 3

integer STEP

function tr(sequence name, integer rid=routine_id(name))
    return {name,rid}
end function

constant tests = {tr("quick_sort"),
                  tr("quick_sort2"),
                  tr("radix_sort"),
                  tr("shell_sort"),
                  tr("shell_sort2"),
                  tr("heap_sort"),
                  tr("sort"),           -- builtin
                 }

sequence results = repeat(repeat({}, length(tests)),length(tabtitles))

sequence dsdx = repeat(repeat(0,length(tests)),length(tabtitles))

integer ds_index

function idle_action_cb()
atom best = -1, -- fastest last
     besti = 0, -- 1..length(tests)
     bestt = 0, -- 1..length(tabtitles)
     len
sequence todo
    --
    -- Search for something to do, active/visible tab first.
    -- Any result set of length 0 -> just do one.
    -- Of all result sets<8, pick the lowest [$].
    --
    todo = {tabidx}
    for t=1 to length(tabtitles) do
        if t!=tabidx then todo &= t end if
    end for

    for t=1 to length(tabtitles) do
        integer ti = todo[t]
        for i=1 to length(results[ti]) do
            len = length(results[ti][i])
            if len=0 then
                best = 0
                besti = i
                bestt = ti
                exit
            elsif len<8 then
                if (best=-1) or (best>results[ti][i][$]) then
                    best = results[ti][i][$]
                    besti = i
                    bestt = ti
                end if
            end if
        end for
        if (t=1) and (besti!=0) then exit end if
    end for
    if best>10 then
        -- cop out if it is getting too slow
        besti = 0
    end if
    if besti!=0 then
        STEP = iff(not XQS and bestt=ONES?1000:100000)
        len = (length(results[bestt][besti])+1)*STEP
        sequence test = iff(bestt=ONES?repeat(1,len):
                        iff(bestt=SORTED?tagset(len):
                        iff(bestt=RANDOM?shuffle(tagset(len)):
                        iff(bestt=REVERSE?reverse(tagset(len)):9/0))))
        ds_index = dsdx[bestt][besti]
        atom t0 = time()
        sequence check = call_func(tests[besti][2],{test})
        t0 = time()-t0
--      if check!=sort(test) then ?9/0 end if
        plot = plots[bestt]
        IupPlotInsert(plot, ds_index, -1, len, t0)
        results[bestt][besti] = append(results[bestt][besti],t0)
        IupSetAttribute(plot,"REDRAW",NULL)
        sequence progress = {bestt}
        for i=1 to length(results[bestt]) do
            progress &= length(results[bestt][i])
        end for
        IupSetStrAttribute(dlg,"TITLE","Compare sorting algorithms %s",{sprint(progress)})
        return IUP_CONTINUE
    end if
    IupSetAttribute(dlg,"TITLE","Compare sorting algorithms (all done, idle)")
    return IUP_IGNORE   -- all done, remove callback
end function
constant cb_idle_action = Icallback("idle_action_cb")

function tabchange_cb(Ihandle /*self*/, Ihandle /*new_tab*/)
    tabidx = IupGetInt(tabs,"VALUEPOS")+1
    plot = plots[tabidx]
    return IUP_DEFAULT;
end function

function esc_close(Ihandle /*ih*/, atom c)
    if c=K_ESC then return IUP_CLOSE end if
    return IUP_CONTINUE
end function

procedure main()
    IupOpen()

    plots = {}
    for i=1 to length(tabtitles) do
        if XQS then
--          results[ONES][1] = repeat(0,8)
            results[ONES][4] = repeat(0,8)
        end if
        plot = IupPlot()
        IupSetAttribute(plot,"MENUITEMPROPERTIES","YES")
        IupSetAttribute(plot,"TABTITLE",tabtitles[i])
        IupSetAttribute(plot,"GRID","YES")
        IupSetAttribute(plot,"MARGINLEFT","50")
        IupSetAttribute(plot,"MARGINBOTTOM","40")
        IupSetAttribute(plot,"LEGEND","YES")
        IupSetAttribute(plot,"LEGENDPOS","TOPLEFT")
--      IupSetAttribute(plot,"AXS_YSCALE","LOG10")
--      IupSetAttribute(plot,"AXS_XSCALE","LOG10")
        for j=1 to length(tests) do
            IupPlotBegin(plot)
            dsdx[i][j] = IupPlotEnd(plot)
            IupSetAttribute(plot,"DS_NAME",tests[j][1])
        end for
        plots = append(plots,plot)
    end for
    tabs = IupTabs(plots)
    IupSetCallback(tabs, "TABCHANGE_CB", Icallback("tabchange_cb"))
    dlg = IupDialog(tabs)
    IupSetAttributes(dlg, "RASTERSIZE=%dx%d", {640, 480})
    IupSetAttribute(dlg, "TITLE", "Compare sorting algorithms")
    IupSetCallback(dlg, "K_ANY", Icallback("esc_close"))
    IupShow(dlg)
    IupSetInt(tabs, "VALUEPOS", tabidx-1)
    IupSetGlobalFunction("IDLE_ACTION", cb_idle_action)
    IupMainLoop()
    IupClose()
end procedure

main()
```



### Conclusions

I knew bubblesort and insertion sort would be bad, but not so bad that you cannot meaningfully plot them against better sorts.
(logarithmic scale helps, but is still not enough)
I had no idea that (these particular implementations of) quicksort and shellsort would be so bad on a sequence of all 1s.
(so bad in fact that I had to cap that test length to 8,000 instead of 800,000 as used for the other tests)
The builtin sort and shell_sort2 were the clear winners, until I found a non-recursive quicksort that seems quite good.
IupPlot is brilliant! It is actually quite fun to watch the graphs grow as you get more results in!
There is a point where you realise you are currently wasting your life fretting over 0.015 seconds...

The ultimate conclusion is, of course, that there are some differences, but as long as you weed out the really bad
algorithms, and at least in the majority of cases, you will probably never notice whether sorting 800,000 items
takes 0.25s or 0.1s - more significant gains are likely to be found elsewhere.


## Python

### Examples of sorting routines


```python
def builtinsort(x):
    x.sort()

def partition(seq, pivot):
   low, middle, up = [], [], []
   for x in seq:
       if x < pivot:
           low.append(x)
       elif x == pivot:
           middle.append(x)
       else:
           up.append(x)
   return low, middle, up
import random
def qsortranpart(seq):
   size = len(seq)
   if size < 2: return seq
   low, middle, up = partition(seq, random.choice(seq))
   return qsortranpart(low) + middle + qsortranpart(up)
```



### Sequence generators



```python
def ones(n):
    return [1]*n

def reversedrange(n):
    return reversed(range(n))

def shuffledrange(n):
    x = range(n)
    random.shuffle(x)
    return x
```


### Write timings


```python
def write_timings(npoints=10, maxN=10**4, sort_functions=(builtinsort,insertion_sort, qsort),
                  sequence_creators = (ones, range, shuffledrange)):
   Ns = range(2, maxN, maxN//npoints)
   for sort in sort_functions:
       for make_seq in sequence_creators:
           Ts = [usec(sort, (make_seq(n),)) for n in Ns]
           writedat('%s-%s-%d-%d.xy' % (sort.__name__,  make_seq.__name__, len(Ns), max(Ns)), Ns, Ts)
```

Where ''writedat()'' is defined in the [[Write float arrays to a text file]], ''usec()'' - [[Query Performance]], ''insertion_sort()'' - [[Insertion sort]], ''qsort'' - [[Quicksort]] subtasks, correspondingly.


### Plot timings

```python
import operator
import numpy, pylab
def plotdd(dictplotdict):
   """See ``plot_timings()`` below."""
   symbols = ('o', '^', 'v', '<', '>', 's', '+', 'x', 'D', 'd',
              '1', '2', '3', '4', 'h', 'H', 'p', '|', '_')
   colors = list('bgrcmyk') # split string on distinct characters
   for npoints, plotdict in dictplotdict.iteritems():
       for ttle, lst in plotdict.iteritems():
           pylab.hold(False)
           for i, (label, polynom, x, y) in enumerate(sorted(lst,key=operator.itemgetter(0))):
               pylab.plot(x, y, colors[i % len(colors)] + symbols[i % len(symbols)], label='%s %s' % (polynom, label))
               pylab.hold(True)
               y = numpy.polyval(polynom, x)
               pylab.plot(x, y, colors[i % len(colors)], label= '_nolegend_')
           pylab.legend(loc='upper left')
           pylab.xlabel(polynom.variable)
           pylab.ylabel('log2( time in microseconds )')
           pylab.title(ttle, verticalalignment='bottom')
           figname = '_%(npoints)03d%(ttle)s' % vars()
           pylab.savefig(figname+'.png')
           pylab.savefig(figname+'.pdf')
           print figname
```

See [[Plot x, y arrays]] and [[Polynomial Fitting]] subtasks for a basic usage of ''pylab.plot()'' and ''numpy.polyfit()''.


```python
import collections, itertools, glob, re
import numpy
def plot_timings():
   makedict = lambda: collections.defaultdict(lambda: collections.defaultdict(list))
   df = makedict()
   ds = makedict()
   # populate plot dictionaries
   for filename in glob.glob('*.xy'):
       m = re.match(r'([^-]+)-([^-]+)-(\d+)-(\d+)\.xy', filename)
       print filename
       assert m, filename
       funcname, seqname, npoints, maxN = m.groups()
       npoints, maxN = int(npoints), int(maxN)
       a = numpy.fromiter(itertools.imap(float, open(filename).read().split()), dtype='f')
       Ns = a[::2]  # sequences lengths
       Ts = a[1::2] # corresponding times
       assert len(Ns) == len(Ts) == npoints
       assert max(Ns) <= maxN
       #
       logsafe = numpy.logical_and(Ns>0, Ts>0)
       Ts = numpy.log2(Ts[logsafe])
       Ns = numpy.log2(Ns[logsafe])
       coeffs = numpy.polyfit(Ns, Ts, deg=1)
       poly = numpy.poly1d(coeffs, variable='log2(N)')
       #
       df[npoints][funcname].append((seqname, poly, Ns, Ts))
       ds[npoints][seqname].append((funcname, poly, Ns, Ts))
   # actual plotting
   plotdd(df)
   plotdd(ds) # see ``plotdd()`` above
```


===Figures: log2( time in microseconds ) vs. log2( sequence length )===
[[File:Ones.png|300px|thumb|right|log(Time) vs. log(N): Relative performance on [1]*N as an input]]
[[File:Range.png|300px|thumb|right|log(Time) vs. log(N): Relative performance on range(N) as an input]]
[[File:Shuffledrange.png|300px|thumb|right|log(Time) vs. log(N): Relative performance on random permutation of range(N) as an input]]

```python
sort_functions = [
    builtinsort,         # see implementation above
    insertion_sort,      # see [[Insertion sort]]
    insertion_sort_lowb, # ''insertion_sort'', where sequential search is replaced
                         #     by lower_bound() function
    qsort,               # see [[Quicksort]]
    qsortranlc,          # ''qsort'' with randomly choosen ''pivot''
                         #     and the filtering via list comprehension
    qsortranpart,        # ''qsortranlc'' with filtering via ''partition'' function
    qsortranpartis,      # ''qsortranpart'', where for a small input sequence lengths
    ]                    #     ''insertion_sort'' is called
if __name__=="__main__":
   import sys
   sys.setrecursionlimit(10000)
   write_timings(npoints=100, maxN=1024, # 1 <= N <= 2**10 an input sequence length
                 sort_functions=sort_functions,
                 sequence_creators = (ones, range, shuffledrange))
   plot_timings()
```

Executing above script we get belowed figures.

### =ones=

[http://i28.tinypic.com/5lcfmo.png ones.png] (143KiB)

 builtinsort     - O(N)
 insertion_sort  - O(N)
 qsort           - O(N**2)
 qsortranpart    - O(N)


### =range=

[http://i32.tinypic.com/14azio6.png range.png] (145KiB)
 builtinsort     - O(N)
 insertion_sort  - O(N)
 qsort           - O(N**2)
 qsortranpart    - O(N*log(N))


### =shuffled range=

[http://i28.tinypic.com/juclyu.png shuffledrange.png] (152KiB)
 builtinsort     - O(N)
 insertion_sort  - O(N**4) ???
 qsort           - O(N*log(N))
 qsortranpart    - O(N) ???


## Ruby


```ruby
class Array
  def radix_sort(base=10)       # negative value is inapplicable.
    ary = dup
    rounds = (Math.log(ary.max)/Math.log(base)).ceil
    rounds.times do |i|
      buckets = Array.new(base){[]}
      base_i = base**i
      ary.each do |n|
        digit = (n/base_i) % base
        buckets[digit] << n
      end
      ary = buckets.flatten
    end
    ary
  end

  def quick_sort
    return self  if size <= 1
    pivot = sample
    g = group_by{|x| x<=>pivot}
    g.default = []
    g[-1].quick_sort + g[0] + g[1].quick_sort
  end

  def shell_sort
    inc = size / 2
    while inc > 0
      (inc...size).each do |i|
        value = self[i]
        while i >= inc and self[i - inc] > value
          self[i] = self[i - inc]
          i -= inc
        end
        self[i] = value
      end
      inc = (inc == 2 ? 1 : (inc * 5.0 / 11).to_i)
    end
    self
  end

  def insertion_sort
    (1...size).each do |i|
      value = self[i]
      j = i - 1
      while j >= 0 and self[j] > value
        self[j+1] = self[j]
        j -= 1
      end
      self[j+1] = value
    end
    self
  end

  def bubble_sort
    (1...size).each do |i|
      (0...size-i).each do |j|
        self[j], self[j+1] = self[j+1], self[j]  if self[j] > self[j+1]
      end
    end
    self
  end
end

data_size = [1000, 10000, 100000, 1000000]
data = []
data_size.each do |size|
  ary = *1..size
  data << [ [1]*size, ary, ary.shuffle, ary.reverse ]
end
data = data.transpose

data_type = ["set to all ones", "ascending sequence", "randomly shuffled", "descending sequence"]
print "Array size:          "
puts data_size.map{|size| "%9d" % size}.join

data.each_with_index do |arys,i|
  puts "\nData #{data_type[i]}:"
  [:sort, :radix_sort, :quick_sort, :shell_sort, :insertion_sort, :bubble_sort].each do |m|
    printf "%20s ", m
    flag = true
    arys.each do |ary|
      if flag
        t0 = Time.now
        ary.dup.send(m)
        printf "  %7.3f", (t1 = Time.now - t0)
        flag = false  if t1 > 2
      else
        print "   --.---"
      end
    end
    puts
  end
end
```

Array#sort is a built-in method.

```txt

Array size:               1000    10000   100000  1000000

Data set to all ones:
                sort     0.000    0.001    0.005    0.043
          radix_sort     0.000    0.002    0.012    0.084
          quick_sort     0.000    0.002    0.020    0.197
          shell_sort     0.002    0.018    0.234    2.897
      insertion_sort     0.000    0.002    0.020    0.198
         bubble_sort     0.064    6.328   --.---   --.---

Data ascending sequence:
                sort     0.000    0.000    0.002    0.020
          radix_sort     0.001    0.010    0.128    1.546
          quick_sort     0.004    0.058    0.521    5.996
          shell_sort     0.001    0.019    0.234    2.882
      insertion_sort     0.000    0.002    0.021    0.195
         bubble_sort     0.065    6.453   --.---   --.---

Data randomly shuffled:
                sort     0.000    0.002    0.024    0.263
          radix_sort     0.001    0.011    0.126    1.529
          quick_sort     0.004    0.081    0.522    6.192
          shell_sort     0.003    0.033    0.498    5.380
      insertion_sort     0.027    2.627   --.---   --.---
         bubble_sort     0.122   11.779   --.---   --.---

Data descending sequence:
                sort     0.000    0.001    0.001    0.021
          radix_sort     0.001    0.012    0.125    1.560
          quick_sort     0.004    0.061    0.522    5.873
          shell_sort     0.003    0.028    0.316    3.829
      insertion_sort     0.053    5.298   --.---   --.---
         bubble_sort     0.206   17.232   --.---   --.---

```



## Tcl


### Background

The <code>lsort</code> command is Tcl's built-in sorting engine. It is implemented in C as a mergesort, so while it is theoretically slower than quicksort, it is a stable sorting algorithm too, which produces results that tend to be less surprising in practice. This task will be matching it against multiple manually-implemented sorting procedures.


### Observations

Obviously, the built-in compiled sort command will be much faster than any Tcl-coded implementation. The Tcl-coded [[Merge sort#Tcl|mergesort]] is up to 3 orders of magnitude slower.

The [[Shell sort#Tcl|shellsort]] implementation suffers, relative to other algorithms, in the case where the list is already sorted.


### Code

```tcl
###############################################################################
# measure and plot times
package require Tk
package require struct::list
namespace path ::tcl::mathfunc

proc create_log10_plot {title xlabel ylabel xs ys labels shapes colours} {
    set w [toplevel .[clock clicks]]
    wm title $w $title
    pack [canvas $w.c -background white]
    pack [canvas $w.legend -background white]
    update
    plot_log10 $w.c $w.legend $title $xlabel $ylabel $xs $ys $labels $shapes $colours
    $w.c config -scrollregion [$w.c bbox all]
    update
}

proc plot_log10 {canvas legend title xlabel ylabel xs ys labels shapes colours} {
    global xfac yfac
    set log10_xs [map {_ {log10 $_}} $xs]
    foreach series $ys {
        lappend log10_ys [map {_ {log10 $_}} $series]
    }
    set maxx [max {*}$log10_xs]
    set yvalues [lsort -real [struct::list flatten $log10_ys]]
    set firstInf [lsearch $yvalues Inf]
    set maxy [lindex $yvalues [expr {$firstInf == -1 ? [llength $yvalues] - 1 : $firstInf - 1}]]

    set xfac [expr {[winfo width $canvas] * 0.8/$maxx}]
    set yfac [expr {[winfo height $canvas] * 0.8/$maxy}]

    scale $canvas x 0 $maxx $xfac "log10($xlabel)"
    scale $canvas y 0 $maxy $yfac "log10($ylabel)" $maxx $xfac

    $legend create text 30 0 -text $title -anchor nw
    set count 1
    foreach series $log10_ys shape $shapes colour $colours label $labels {
        plotxy $canvas $log10_xs $series $shape $colour
        legenditem $legend [incr count] $shape $colour $label
    }
}

proc map {lambda list} {
    set res [list]
    foreach item $list {lappend res [apply $lambda $item]}
    return $res
}

proc legenditem {legend pos shape colour label} {
    set y [expr {$pos * 15}]
    $shape $legend 20 $y -fill $colour
    $legend create text 30 $y -text $label -anchor w
}

# The actual plotting engine
proc plotxy {canvas _xs _ys shape colour} {
    global xfac yfac
    foreach x $_xs y $_ys {
        if {$y < Inf} {
            lappend xs $x
            lappend ys $y
        }
    }
    set coords [list]
    foreach x $xs y $ys {
        set coord_x [expr {$x*$xfac}]
        set coord_y [expr {-$y*$yfac}]
        $shape $canvas $coord_x $coord_y -fill $colour
        lappend coords $coord_x $coord_y
    }
    $canvas create line $coords -smooth true
}
# Rescales the contents of the given canvas
proc scale {canvas direction from to fac label {other_to 0} {other_fac 0}} {
    set f [expr {$from*$fac}]
    set t [expr {$to*$fac}]
    switch -- $direction {
        x {
            set f [expr {$from * $fac}]
            set t [expr {$to * $fac}]
            # create x-axis
            $canvas create line $f 0 $t 0
            $canvas create text $f 0 -anchor nw -text $from
            $canvas create text $t 0 -anchor n -text [format "%.1f" $to]
            $canvas create text [expr {($f+$t)/2}] 0 -anchor n -text $label

        }
        y {
            set f [expr {$from * -$fac}]
            set t [expr {$to * -$fac}]
            # create y-axis
            $canvas create line 0 $f 0 $t
            $canvas create text 0 $f -anchor se -text $from
            $canvas create text 0 $t -anchor e -text [format "%.1f" $to]
            $canvas create text 0 [expr {($f+$t)/2}] -anchor e -text $label
            # create gridlines
            set xmax [expr {$other_to * $other_fac}]
            for {set i 1} {$i < $to} {incr i} {
                set y [expr {$i * -$fac}]
                $canvas create line 0 $y $xmax $y -dash .
            }
        }
    }
}
# Helper to make points, which are otherwise not a native item type
proc dot {canvas x y args} {
    set id [$canvas create oval [expr {$x-3}] [expr {$y-3}] \
                [expr {$x+3}] [expr {$y+3}]]
    $canvas itemconfigure $id {*}$args
}
proc square {canvas x y args} {
    set id [$canvas create rectangle [expr {$x-3}] [expr {$y-3}] \
                [expr {$x+3}] [expr {$y+3}]]
    $canvas itemconfigure $id {*}$args
}
proc cross {canvas x y args} {
    set l1 [$canvas create line [expr {$x-3}] $y [expr {$x+3}] $y]
    set l2 [$canvas create line $x [expr {$y-3}] $x [expr {$y+3}]]
    $canvas itemconfigure $l1 {*}$args
    $canvas itemconfigure $l2 {*}$args
}
proc x {canvas x y args} {
    set l1 [$canvas create line [expr {$x-3}] [expr {$y-3}] [expr {$x+3}] [expr {$y+3}]]
    set l2 [$canvas create line [expr {$x+3}] [expr {$y-3}] [expr {$x-3}] [expr {$y+3}]]
    $canvas itemconfigure $l1 {*}$args
    $canvas itemconfigure $l2 {*}$args
}
proc triangleup {canvas x y args} {
    set id [$canvas create polygon $x [expr {$y-4}] \
                [expr {$x+4}] [expr {$y+4}] \
                [expr {$x-4}] [expr {$y+4}]]
    $canvas itemconfigure $id {*}$args
}
proc triangledown {canvas x y args} {
    set id [$canvas create polygon $x [expr {$y+4}] \
                [expr {$x+4}] [expr {$y-4}] \
                [expr {$x-4}] [expr {$y-4}]]
    $canvas itemconfigure $id {*}$args
}

wm withdraw .

#####################################################################
# list creation procedures
proc ones n {
    lrepeat $n 1
}
proc reversed n {
    while {[incr n -1] >= 0} {
        lappend result $n
    }
    return $result
}
proc random n {
    for {set i 0} {$i < $n} {incr i} {
        lappend result [expr {int($n * rand())}]
    }
    return $result
}

set algorithms {lsort quicksort shellsort insertionsort bubblesort mergesort}
set sizes {1 10 100 1000 10000 100000}
set types {ones reversed random}
set shapes {dot square cross triangleup triangledown x}
set colours {red blue black brown yellow black}

# create some lists to be used by all sorting algorithms
array set lists {}
foreach size $sizes {
    foreach type $types {
        set lists($type,$size) [$type $size]
    }
}

set runs 10

# header
fconfigure stdout -buffering none
puts -nonewline [format "%-16s" "list length:"]
foreach size $sizes {
    puts -nonewline [format " %10d" $size]
}
puts ""

# perform the sort timings and output results
foreach type $types {
    puts "\nlist type: $type"
    set times [list]
    foreach algo $algorithms {
        set errs [list]
        set thesetimes [list]
        $algo {} ;# call it once to ensure it's compiled

        puts -nonewline [format "   %-13s" $algo]
        foreach size $sizes {
            # some implementations are just too slow
            if {$type ne "ones" && (
                ($algo eq "insertionsort" && $size > 10000) ||
                ($algo eq "bubblesort" && $size > 1000))
            } {
                set time Inf
            } else {
                # OK, do it
                if {[catch {time [list $algo $lists($type,$size)] $runs} result] != 0} {
                    set time Inf
                    lappend errs $result
                } else {
                    set time [lindex [split $result] 0]
                }
            }
            lappend thesetimes $time
            puts -nonewline [format " %10s" $time]
        }
        puts ""
        if {[llength $errs] > 0} {
            puts [format "      %s" [join $errs "\n      "]]
        }
        lappend times $thesetimes
    }
    create_log10_plot "Sorting a '$type' list" size time $sizes $times $algorithms $shapes $colours
}
puts "\ntimes in microseconds, average of $runs runs"
```


### Output


```txt
list length:              1         10        100       1000      10000     100000

list type: ones
   lsort                0.8        1.2        7.2       71.9     1042.7    11428.9
   quicksort            1.1        9.0       40.6      369.5     3696.4    37478.4
   shellsort            1.4       26.0      249.1     4003.4    56278.7   717790.6
   insertionsort        1.1        6.4       59.0      528.1     5338.9    54913.0
   bubblesort           1.9        5.1       31.9      308.8     3259.1    31991.2
   mergesort            1.3       61.1      704.2     9275.4   224784.4 14599414.6

list type: reversed
   lsort                1.0        1.6        9.9      112.1     1434.9    20181.0
   quicksort            1.5       55.3      495.6     6705.9    79984.0   963975.0
   shellsort            1.5       25.9      457.0     7118.6    92497.5  1210143.9
   insertionsort        1.2       21.0     1645.0   159262.2 15859610.8        Inf
   bubblesort           1.9      445.0    46526.6  4665550.4        Inf        Inf
   mergesort            1.4       61.7      842.8     9572.1   215536.6 16938651.0

list type: random
   lsort                1.0        1.7       15.7      300.9     3275.0    58779.5
   quicksort            1.2       28.0      429.1     5609.5    71743.3   923630.4
   shellsort            1.6       26.7      571.0     9031.1   140526.9  2244152.7
   insertionsort        1.3       15.4      832.6    79018.0  7893722.6        Inf
   bubblesort           1.8      256.2    23753.1  2422926.0        Inf        Inf
   mergesort            1.9       60.2      883.5    12505.6   399672.6 49225509.8

times in microseconds, average of 10 runs
```

[[Image:Tcl_sort_ones.png]]
[[Image:Tcl_sort_reversed.png]]
[[Image:Tcl_sort_random.png]]
