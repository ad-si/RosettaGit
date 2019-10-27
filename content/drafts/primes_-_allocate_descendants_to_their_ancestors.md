+++
title = "Primes - allocate descendants to their ancestors"
description = ""
date = 2019-03-16T20:43:54Z
aliases = []
[extra]
id = 18777
[taxonomies]
categories = []
tags = []
+++

{{task|Prime Numbers}}
The concept, is to add the decomposition into prime factors of a number to get its 'ancestors'.


The objective is to demonstrate that the choice of the algorithm can be crucial in term of performance.
This solution could be compared to the solution that would use the decomposition into primes for all the numbers between 1 and 3<sup>33</sup>.


The problem is to list, for a delimited set of ancestors (from 1 to 99) :

- the total of their own ancestors (LEVEL),

- their own ancestors (ANCESTORS),

- the total of the direct descendants (DESCENDANTS),

- all the direct descendants.


You only have to consider the prime factors < 100.

A grand total of the descendants has to be printed at the end of the list.

The task should be accomplished in a reasonable time-frame.


Example :

```txt
46 = 2*23 --> 2+23 = 25, is the parent of 46.
25 = 5*5  --> 5+5  = 10, is the parent of 25.
10 = 2*5  --> 2+5  = 7,  is the parent of 10.
7 is a prime factor and, as such, has no parent.

46 has 3 ancestors (7, 10 and 25).
46 has 557 descendants.
```


The list layout and the output for Parent [46] :

```txt
[46] Level: 3
Ancestors: 7, 10, 25
Descendants: 557
129, 205, 246, 493, 518, 529, 740, 806, 888, 999, 1364, 1508, 1748, 2552, 2871, 3128, 3255, 3472, 3519, 3875, 3906, 4263, 4650, 4960, 5075, 5415, 5580, 5776, 5952, 6090, 6279, 6496, 6498, 6696, 6783, 7250, 7308, 7475, 7533, 8075, 8151, 8619, 8700, 8855, 8970, 9280, 9568, 9690, 10115, 10336, 10440, 10626, 10764, 11136, 11495, 11628, 11745, 12103, 12138, 12155, 12528, 12650, 13794, 14094, 14399, 14450, 14586, 15180, 15379, 15778, 16192, 17290, 17303, 17340, 18216, 18496, 20482, 20493, 20570, 20748, 20808, 21658, 21970, 22540, 23409, 24684, 24700, 26026, 26364, 27048, 29260, 29282, 29640, 30429, 30940, 31616, 32200, 33345, 35112, 35568, 36225, 36652, 37128, 37180, 38640, 39501, 40014, 41216, 41769, 41800, 43125, 43470, 44044, 44200, 44616, 46000, 46368, 47025, 49725, 50160, 50193, 51750, 52136, 52164, 52360, 53040, 53504, 55200, 56430, 56576, 58653, 58880, 58905, 59670, 60192, 62100, 62832, 62920, 63648, 66240, 66248, 67716, 69825, 70125, 70656, 70686, 70785, 71604, 74480, 74520, 74529, 74536, 74800, 75504, 79488, 83125, 83790, 83835, 83853, 84150, 84942, 87465, 88725, 89376, 89424, 89760, 93296, 94640, 95744, 99750, 99825, 100548, 100602, 100980, 104125, 104958, 105105, 105625, 106400, 106470, 106480, 107712, 112112, 113568, 118750, 119700, 119790, 121176, 124509, 124950, 125125, 126126, 126750, 127680, 127764, 127776, 133280, 135200, 136192, 136323, 142500, 143640, 143748, 148225, 148750, 149940, 150150, 152000, 152100, 153216, 156065, 159936, 160160, 161595, 162240, 171000, 172368, 173056, 177870, 178500, 178750, 179928, 180180, 182400, 182520, 184877, 187278, 189728, 190400, 192192, 192375, 193914, 194560, 194688, 202419, 205200, 205335, 211750, 212500, 213444, 214200, 214500, 216216, 218880, 219024, 222950, 228480, 228800, 230850, 233472, 240975, 243243, 243712, 246240, 246402, 254100, 255000, 257040, 257400, 262656, 264110, 267540, 271040, 272000, 274176, 274560, 277020, 285376, 286875, 289170, 289575, 292864, 295488, 302500, 304920, 306000, 308448, 308880, 316932, 318500, 321048, 325248, 326400, 329472, 332424, 343035, 344250, 347004, 347490, 348160, 361179, 363000, 365904, 367200, 370656, 373977, 377300, 382200, 387200, 391680, 407680, 408375, 411642, 413100, 416988, 417792, 429975, 435600, 440640, 452760, 455000, 458640, 464640, 470016, 470596, 482944, 489216, 490050, 495616, 495720, 509355, 511875, 515970, 522720, 528768, 539000, 543312, 546000, 550368, 557568, 557685, 582400, 588060, 594864, 606375, 609375, 611226, 614250, 619164, 627264, 646800, 650000, 655200, 669222, 672280, 689920, 698880, 705672, 721875, 727650, 731250, 737100, 745472, 756315, 770000, 776160, 780000, 786240, 793881, 806736, 827904, 832000, 838656, 859375, 866250, 873180, 877500, 884520, 900375, 907578, 924000, 931392, 936000, 943488, 960400, 985600, 995085, 998400, 1031250, 1039500, 1047816, 1053000, 1061424, 1064960, 1071875, 1080450, 1100000, 1108800, 1123200, 1152480, 1178793, 1182720, 1184625, 1194102, 1198080, 1229312, 1237500, 1247400, 1261568, 1263600, 1277952, 1286250, 1296540, 1320000, 1330560, 1347840, 1372000, 1382976, 1403325, 1408000, 1419264, 1421550, 1437696, 1485000, 1496880, 1516320, 1531250, 1543500, 1555848, 1584000, 1596672, 1617408, 1646400, 1670625, 1683990, 1689600, 1705860, 1750329, 1756160, 1782000, 1796256, 1802240, 1819584, 1837500, 1852200, 1900800, 1960000, 1975680, 2004750, 2020788, 2027520, 2047032, 2083725, 2107392, 2138400, 2162688, 2187500, 2205000, 2222640, 2280960, 2302911, 2352000, 2370816, 2405700, 2433024, 2480625, 2500470, 2508800, 2566080, 2625000, 2646000, 2667168, 2737152, 2800000, 2822400, 2886840, 2953125, 2976750, 3000564, 3010560, 3079296, 3125000, 3150000, 3175200, 3211264, 3247695, 3360000, 3386880, 3464208, 3515625, 3543750, 3572100, 3584000, 3612672, 3750000, 3780000, 3810240, 3897234, 4000000, 4032000, 4064256, 4218750, 4252500, 4286520, 4300800, 4500000, 4536000, 4572288, 4587520, 4800000, 4822335, 4838400, 5062500, 5103000, 5120000, 5143824, 5160960, 5400000, 5443200, 5505024, 5740875, 5760000, 5786802, 5806080, 6075000, 6123600, 6144000, 6193152, 6480000, 6531840, 6553600, 6834375, 6889050, 6912000, 6967296, 7290000, 7348320, 7372800, 7776000, 7838208, 7864320, 8201250, 8266860, 8294400, 8388608, 8748000, 8817984, 8847360, 9331200, 9437184, 9841500, 9920232, 9953280, 10497600, 10616832, 11160261, 11197440, 11809800, 11943936, 12597120, 13286025, 13436928, 14171760, 15116544, 15943230, 17006112, 19131876
```


Some figures :

```txt
The biggest descendant number : 3^33 = 5.559.060.566.555.523 (parent 99)

Total Descendants 546.986

```



## AutoHotkey

It is based on the same logic as the Python script.

I seem that the use of an associative array is a little bit slower than the use of a simple array combined with the 'Sort' command, even if the 'Sort' command pumps 85% of the processing time.

```AutoHotkey
#Warn
#SingleInstance force
#NoEnv            ; Recommended for performance and compatibility with future AutoHotkey releases.
SendMode Input    ; Recommended for new scripts due to its superior speed and reliability.
SetBatchLines, -1
SetFormat, IntegerFast, D

MaxPrime    = 99		; upper bound for the prime factors
MaxAncestor = 99		; greatest parent number

Descendants := []

Primes := GetPrimes(MaxPrime)
Exclusions := Primes.Clone()
Exclusions.Insert(4)

if A_Is64bitOS
{
	Loop, % MaxAncestor
		Descendants.Insert({})
	
	for i, Prime in Primes
	{
		Descendants[Prime, Prime] := 0
		
		for Parent, Children in Descendants
		{
			if ((Sum := Parent+Prime) > MaxAncestor)
				break
			
			for pr in Children
				Descendants[Sum, pr*Prime] := 0
		}
	}
	
	for i, v in Exclusions
		Descendants[v].Remove(v, "")
}
else
{
	Loop, % MaxAncestor
		Descendants.Insert([])

	for i, Prime in Primes
	{
		Descendants[Prime].Insert(Prime)
		
		for Parent, Children in Descendants
		{
			if ((Sum := Parent+Prime) > MaxAncestor)
				break
			
			for j, pr in Children
				Descendants[Sum].Insert(pr*Prime)
		}
	}
	
	for i, v in Exclusions
		Descendants[v].Remove()
}

if (MaxAncestor > MaxPrime)
	Primes := GetPrimes(MaxAncestor)

IfExist, %A_ScriptName%.txt
	FileDelete, %A_ScriptName%.txt

;-------------------------------------------------------
; Arrays :
; Integer keys are stored using the native integer type
; 32bit key max = 2.147.483.647
; 64bit key max = 9.223.372.036.854.775.807
;-------------------------------------------------------
Tot_desc = 0
for Parent, Children in Descendants
{
	ls_desc =
	if A_Is64bitOS
	{
		nb_desc = 0
		for pr in Children
			ls_desc .= ", " pr, nb_desc++
		ls_desc := LTrim(ls_desc, ", ")
	}
	else
	{
		nb_desc := Children.MaxIndex()
		for i, pr in Children
			ls_desc .= "," pr
		ls_desc := LTrim(ls_desc, ",")
		
		Sort, ls_desc, N D`,
		StringReplace, ls_desc, ls_desc, `,,`,%A_Space%, All
	}
	
	ls_anc =
	nb_anc := GetAncestors(ls_anc, Parent)
	ls_anc := LTrim(ls_anc, ", ")
	
	FileAppend, % "[" Parent "] Level: " nb_anc "`r`nAncestors: " (nb_anc ? ls_anc : "None") "`r`n"
				 , %A_ScriptName%.txt
	
	if nb_desc
	{
		Tot_desc += nb_desc
		FileAppend, % "Descendants: " nb_desc "`r`n" ls_desc "`r`n`r`n", %A_ScriptName%.txt
	}
	else
		FileAppend, % "Descendants: None`r`n`r`n", %A_ScriptName%.txt
}

FileAppend, % "Total descendants " Tot_desc, %A_ScriptName%.txt
return

GetAncestors(ByRef _lsAnc, _child)
{
	global Primes
	
	lChild := _child
	lIndex := lParent := 0
	
	while lChild > 1
	{
		lPrime := Primes[++lIndex]
		while !mod(lChild, lPrime)
			lChild //= lPrime, lParent += lPrime
	}
	
	if (lParent = _child or _child = 1)
		return 0
	
	_lsAnc := ", " lParent _lsAnc
	li := GetAncestors(_lsAnc, lParent)
	return ++li
}

GetPrimes(_maxPrime=0, _nbrPrime=0)
{
	lPrimes := []
	
	if (_maxPrime >= 2 or _nbrPrime >= 1)
	{
		lPrimes.Insert(2)
		lValue = 1
		
		while (lValue += 2) <= _maxPrime or lPrimes.MaxIndex() < _nbrPrime
		{
			lMaxPrime := Floor(Sqrt(lValue))
			
			for lKey, lPrime in lPrimes
			{
				if (lPrime > lMaxPrime)		; if prime divisor is greater than Floor(Sqrt(lValue))
				{
					lPrimes.Insert(lValue)
					break
				}
				
				if !Mod(lValue, lPrime)
					break
			}
		}
	}
	
	return lPrimes
}
```



## C



###  Full Approach 

You can decompose all the numbers from 1 to 3<sup>33</sup> (5.559.060.566.555.523).

This solution can take a while.

The InsertChild function is replaced by the AppendChild function which appends directly the child as the new last item in the list.

```c>#include <math.h

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAXPRIME 99						// upper bound for the prime factors
#define MAXPARENT 99					// greatest parent number
#define NBRPRIMES 30					// max number of prime factors
#define NBRANCESTORS 10					// max number of parent's ancestors

FILE *FileOut;
char format[] = ", %lld";

int Primes[NBRPRIMES];					// table of the prime factors
int iPrimes;							// max index of the prime factor table

short Ancestors[NBRANCESTORS];			// table of the parent's ancestors

struct Children {
	long long Child;
	struct Children *pNext;
};
struct Children *Parents[MAXPARENT+1][2];	// table pointing to the root and to the last descendants (per parent)
int CptDescendants[MAXPARENT+1];			// counter table of the descendants (per parent)
long long MaxDescendant = (long long) pow(3.0, 33.0);	// greatest descendant number

short GetParent(long long child);
struct Children *AppendChild(struct Children *node, long long child);
short GetAncestors(short child);
void PrintDescendants(struct Children *node);
int GetPrimes(int primes[], int maxPrime);

int main()
{
	long long Child;
	short i, Parent, Level;
	int TotDesc = 0;
	
	if ((iPrimes = GetPrimes(Primes, MAXPRIME)) < 0)
		return 1;
	
	for (Child = 1; Child <= MaxDescendant; Child++)
	{
		if (Parent = GetParent(Child))
		{
			Parents[Parent][1] = AppendChild(Parents[Parent][1], Child);
			if (Parents[Parent][0] == NULL)
				Parents[Parent][0] = Parents[Parent][1];
			CptDescendants[Parent]++;
		}
	}
	
	if (MAXPARENT > MAXPRIME)
		if (GetPrimes(Primes, MAXPARENT) < 0)
			return 1;

	if (fopen_s(&FileOut, "Ancestors.txt", "w"))
		return 1;

	for (Parent = 1; Parent <= MAXPARENT; Parent++)
	{
		Level = GetAncestors(Parent);
		
		fprintf(FileOut, "[%d] Level: %d\n", Parent, Level);
		
		if (Level)
		{
			fprintf(FileOut, "Ancestors: %d", Ancestors[0]);
			
			for (i = 1; i < Level; i++)
				fprintf(FileOut, ", %d", Ancestors[i]);
		}
		else
			fprintf(FileOut, "Ancestors: None");

		if (CptDescendants[Parent])
		{
			fprintf(FileOut, "\nDescendants: %d\n", CptDescendants[Parent]);
			strcpy_s(format, "%lld");
			PrintDescendants(Parents[Parent][0]);
			fprintf(FileOut, "\n");
		}
		else
			fprintf(FileOut, "\nDescendants: None\n");

		fprintf(FileOut, "\n");
		TotDesc += CptDescendants[Parent];
	}

	fprintf(FileOut, "Total descendants %d\n\n", TotDesc);
	if (fclose(FileOut))
		return 1;

	return 0;
}

short GetParent(long long child)
{
	long long Child = child;
	short Parent = 0;
	short Index = 0;
	
	while (Child > 1 && Parent <= MAXPARENT)
	{
		if (Index > iPrimes)
			return 0;

		while (Child % Primes[Index] == 0)
		{
			Child /= Primes[Index];
			Parent += Primes[Index];
		}

		Index++;
	}
	
	if (Parent == child || Parent > MAXPARENT || child == 1)
		return 0;
	
	return Parent;
}

struct Children *AppendChild(struct Children *node, long long child)
{
	static struct Children *NodeNew;
	
	if (NodeNew = (struct Children *) malloc(sizeof(struct Children)))
	{
		NodeNew->Child = child;
		NodeNew->pNext = NULL;
		if (node != NULL)
			node->pNext = NodeNew;
	}
	
	return NodeNew;
}

short GetAncestors(short child)
{
	short Child = child;
	short Parent = 0;
	short Index = 0;
	
	while (Child > 1)
	{
		while (Child % Primes[Index] == 0)
		{
			Child /= Primes[Index];
			Parent += Primes[Index];
		}
		
		Index++;
	}
	
	if (Parent == child || child == 1)
		return 0;
	
	Index = GetAncestors(Parent);
	
	Ancestors[Index] = Parent;
	return ++Index;
}

void PrintDescendants(struct Children *node)
{
	static struct Children *NodeCurr;
	static struct Children *NodePrev;

	NodeCurr = node;
	NodePrev = NULL;
	while (NodeCurr)
	{
		fprintf(FileOut, format, NodeCurr->Child);
		strcpy_s(format, ", %lld");
		NodePrev = NodeCurr;
		NodeCurr = NodeCurr->pNext;
		free(NodePrev);
	}

	return;
}

int GetPrimes(int primes[], int maxPrime)
{
	if (maxPrime < 2)
		return -1;
	
	int Index = 0, Value = 1;
	int Max, i;

	primes[0] = 2;

	while ((Value += 2) <= maxPrime)
	{
		Max = (int) floor(sqrt((double) Value));
		
		for (i = 0; i <= Index; i++)
		{
			if (primes[i] > Max)
			{
				if (++Index >= NBRPRIMES)
					return -1;

				primes[Index] = Value;
				break;
			}

			if (Value % primes[i] == 0)
				break;
		}
	}

	return Index;
}
```



###  Optimized Approach 

You sum the prime factors from the Prime factor table and you calculate the products.

The sums are the ancestors, the products are the descendants.

It is based on the same logic as the Python script.

```c>#include <math.h

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAXPRIME 99						// upper bound for the prime factors
#define MAXPARENT 99					// greatest parent number
#define NBRPRIMES 30					// max number of prime factors
#define NBRANCESTORS 10					// max number of parent's ancestors

FILE *FileOut;
char format[] = ", %lld";

int Primes[NBRPRIMES];					// table of the prime factors
int iPrimes;							// max index of the prime factor table

short Ancestors[NBRANCESTORS];			// table of the parent's ancestors

struct Children {
	long long Child;
	struct Children *pLower;
	struct Children *pHigher;
};
struct Children *Parents[MAXPARENT+1];	// table pointing to the root descendants (per parent)
int CptDescendants[MAXPARENT+1];		// counter table of the descendants (per parent)

void InsertPreorder(struct Children *node, short sum, int prime);
struct Children *InsertChild(struct Children *node, long long child);
void RemoveFalseChildren();
short GetAncestors(short child);
void PrintDescendants(struct Children *node);
int GetPrimes(int primes[], int maxPrime);

int main()
{
	short i, Parent, Sum, Level;
	int Prime;
	int TotDesc = 0;
	int MidPrime;
	
	if ((iPrimes = GetPrimes(Primes, MAXPRIME)) < 0)
		return 1;

	MidPrime = Primes[iPrimes] / 2;

	for (i = iPrimes; i >= 0; i--)
	{
		Prime = Primes[i];
		Parents[Prime] = InsertChild(Parents[Prime], Prime);
		CptDescendants[Prime]++;

		if (Prime > MidPrime)
			continue;

		for (Parent = 1; Parent <= MAXPARENT; Parent++)
		{
			if ((Sum = Parent+Prime) > MAXPARENT)
				break;

			if (Parents[Parent])
			{
				InsertPreorder(Parents[Parent], Sum, Prime);
				CptDescendants[Sum] += CptDescendants[Parent];
			}
		}
	}

	RemoveFalseChildren();

	if (MAXPARENT > MAXPRIME)
		if (GetPrimes(Primes, MAXPARENT) < 0)
			return 1;

	if (fopen_s(&FileOut, "Ancestors.txt", "w"))
		return 1;

	for (Parent = 1; Parent <= MAXPARENT; Parent++)
	{
		Level = GetAncestors(Parent);
		
		fprintf(FileOut, "[%d] Level: %d\n", Parent, Level);
		
		if (Level)
		{
			fprintf(FileOut, "Ancestors: %d", Ancestors[0]);
			
			for (i = 1; i < Level; i++)
				fprintf(FileOut, ", %d", Ancestors[i]);
		}
		else
			fprintf(FileOut, "Ancestors: None");

		if (CptDescendants[Parent])
		{
			fprintf(FileOut, "\nDescendants: %d\n", CptDescendants[Parent]);
			strcpy_s(format, "%lld");
			PrintDescendants(Parents[Parent]);
			fprintf(FileOut, "\n");
		}
		else
			fprintf(FileOut, "\nDescendants: None\n");

		fprintf(FileOut, "\n");
		TotDesc += CptDescendants[Parent];
	}

	fprintf(FileOut, "Total descendants %d\n\n", TotDesc);
	
	if (fclose(FileOut))
		return 1;

	return 0;
}

void InsertPreorder(struct Children *node, short sum, int prime)
{
	Parents[sum] = InsertChild(Parents[sum], node->Child * prime);

	if (node->pLower)
		InsertPreorder(node->pLower, sum, prime);

	if (node->pHigher)
		InsertPreorder(node->pHigher, sum, prime);
}

struct Children *InsertChild(struct Children *node, long long child)
{
	if (node)
	{
		if (child <= node->Child)
			node->pLower = InsertChild(node->pLower, child);
		else
			node->pHigher = InsertChild(node->pHigher, child);
	}
	else
	{
		if (node = (struct Children *) malloc(sizeof(struct Children)))
		{
			node->Child = child;
			node->pLower = NULL;
			node->pHigher = NULL;
		}
	}

	return node;
}

void RemoveFalseChildren()
{
	short i, ex;
	int Exclusions[NBRPRIMES+1];		// table of the prime factors + {4}
	int iExclusions;					// max index of the exclusion table
	struct Children *ptr;

	for (i = 0; i <= iPrimes; i++)
		Exclusions[i] = Primes[i];

	iExclusions = iPrimes + 1;
	Exclusions[iExclusions] = 4;

	for (i = 0; i <= iExclusions; i++)
	{
		ex = Exclusions[i];
		ptr = Parents[ex];
		Parents[ex] = ptr->pHigher;
		CptDescendants[ex]--;
		free(ptr);
	}
}

short GetAncestors(short child)
{
	short Child = child;
	short Parent = 0;
	short Index = 0;
	
	while (Child > 1)
	{
		while (Child % Primes[Index] == 0)
		{
			Child /= Primes[Index];
			Parent += Primes[Index];
		}
		
		Index++;
	}
	
	if (Parent == child || child == 1)
		return 0;
	
	Index = GetAncestors(Parent);
	
	Ancestors[Index] = Parent;
	return ++Index;
}

void PrintDescendants(struct Children *node)
{
	if (node->pLower)
		PrintDescendants(node->pLower);
	
	fprintf(FileOut, format, node->Child);
	strcpy_s(format, ", %lld");
	
	if (node->pHigher)
		PrintDescendants(node->pHigher);

	free(node);
	return;
}

int GetPrimes(int primes[], int maxPrime)
{
	if (maxPrime < 2)
		return -1;
	
	int Index = 0, Value = 1;
	int Max, i;

	primes[0] = 2;

	while ((Value += 2) <= maxPrime)
	{
		Max = (int) floor(sqrt((double) Value));
		
		for (i = 0; i <= Index; i++)
		{
			if (primes[i] > Max)
			{
				if (++Index >= NBRPRIMES)
					return -1;

				primes[Index] = Value;
				break;
			}

			if (Value % primes[i] == 0)
				break;
		}
	}

	return Index;
}
```



## Go

{{trans|Python}}

```go
package main

import (
    "fmt"
    "sort"
)

func getPrimes(max int) []int {
    if max < 2 {
        return []int{}
    }
    lprimes := []int{2}
outer:
    for x := 3; x <= max; x += 2 {
        for _, p := range lprimes {
            if x%p == 0 {
                continue outer
            }
        }
        lprimes = append(lprimes, x)
    }
    return lprimes
}

func main() {
    const maxSum = 99
    descendants := make([][]int64, maxSum+1)
    ancestors := make([][]int, maxSum+1)
    for i := 0; i <= maxSum; i++ {
        descendants[i] = []int64{}
        ancestors[i] = []int{}
    }
    primes := getPrimes(maxSum)

    for _, p := range primes {
        descendants[p] = append(descendants[p], int64(p))
        for s := 1; s < len(descendants)-p; s++ {
            temp := make([]int64, len(descendants[s]))
            for i := 0; i < len(descendants[s]); i++ {
                temp[i] = int64(p) * descendants[s][i]
            }
            descendants[s+p] = append(descendants[s+p], temp...)
        }
    }

    for _, p := range append(primes, 4) {
        le := len(descendants[p])
        if le == 0 {
            continue
        }
        descendants[p][le-1] = 0
        descendants[p] = descendants[p][:le-1]
    }
    total := 0

    for s := 1; s <= maxSum; s++ {
        x := descendants[s]
        sort.Slice(x, func(i, j int) bool {
            return x[i] < x[j]
        })
        total += len(descendants[s])
        index := 0
        for ; index < len(descendants[s]); index++ {
            if descendants[s][index] > int64(maxSum) {
                break
            }
        }
        for _, d := range descendants[s][:index] {
            ancestors[d] = append(ancestors[s], s)
        }
        if (s >= 21 && s <= 45) || (s >= 47 && s <= 73) || (s >= 75 && s < maxSum) {
            continue
        }
        temp := fmt.Sprintf("%v", ancestors[s])
        fmt.Printf("%2d: %d Ancestor(s): %-14s", s, len(ancestors[s]), temp)
        le := len(descendants[s])
        if le <= 10 {
            fmt.Printf("%5d Descendant(s): %v\n", le, descendants[s])
        } else {
            fmt.Printf("%5d Descendant(s): %v\b ...]\n", le, descendants[s][:10])
        }
    }
    fmt.Println("\nTotal descendants", total)
}
```


{{out}}

```txt

 1: 0 Ancestor(s): []                0 Descendant(s): []
 2: 0 Ancestor(s): []                0 Descendant(s): []
 3: 0 Ancestor(s): []                0 Descendant(s): []
 4: 0 Ancestor(s): []                0 Descendant(s): []
 5: 0 Ancestor(s): []                1 Descendant(s): [6]
 6: 1 Ancestor(s): [5]               2 Descendant(s): [8 9]
 7: 0 Ancestor(s): []                2 Descendant(s): [10 12]
 8: 2 Ancestor(s): [5 6]             3 Descendant(s): [15 16 18]
 9: 2 Ancestor(s): [5 6]             4 Descendant(s): [14 20 24 27]
10: 1 Ancestor(s): [7]               5 Descendant(s): [21 25 30 32 36]
11: 0 Ancestor(s): []                5 Descendant(s): [28 40 45 48 54]
12: 1 Ancestor(s): [7]               7 Descendant(s): [35 42 50 60 64 72 81]
13: 0 Ancestor(s): []                8 Descendant(s): [22 56 63 75 80 90 96 108]
14: 3 Ancestor(s): [5 6 9]          10 Descendant(s): [33 49 70 84 100 120 128 135 144 162]
15: 3 Ancestor(s): [5 6 8]          12 Descendant(s): [26 44 105 112 125 126 150 160 180 192 ...]
16: 3 Ancestor(s): [5 6 8]          14 Descendant(s): [39 55 66 98 140 168 189 200 225 240 ...]
17: 0 Ancestor(s): []               16 Descendant(s): [52 88 99 147 175 210 224 250 252 300 ...]
18: 3 Ancestor(s): [5 6 8]          19 Descendant(s): [65 77 78 110 132 196 280 315 336 375 ...]
19: 0 Ancestor(s): []               22 Descendant(s): [34 104 117 165 176 198 245 294 350 420 ...]
20: 3 Ancestor(s): [5 6 9]          26 Descendant(s): [51 91 130 154 156 220 264 297 392 441 ...]
46: 3 Ancestor(s): [7 10 25]       557 Descendant(s): [129 205 246 493 518 529 740 806 888 999 ...]
74: 5 Ancestor(s): [5 6 8 16 39]  6336 Descendant(s): [213 469 670 793 804 1333 1342 1369 1534 2014 ...]
99: 1 Ancestor(s): [17]          38257 Descendant(s): [194 1869 2225 2670 2848 3204 3237 4029 4565 5037 ...]

Total descendants 546986

```



## J



### Definition of terms


For this task, based on [[Talk:Primes - allocate descendants to their ancestors#Task is ambiguous|extensive discussion]] and examination of the early example implementations, these definitions might be sufficient:

An "allocation" P of N is a list of primes whose sum is N which includes prime number P.  (For example 5 7, 2 5 5, and 2 2 3 5 are each examples of the 5 of 12 allocation.)

The "family" of N is all distinct products of allocations P of N. (So we can think of our lists as being capable of producing sets: if different sequences of primes produced the same product we still would only count that product once.)

The "descendants" or "direct descendants" of N are all members of its family excluding N itself. (As N could be in its own family if it is prime or if N is 4.)

A "deallocation" of N is the sum of its prime factorization. (For example, the deallocation of 12 is 2+2+3 or 7.)

A "family tree" of N is all the distinct values resulting from applying deallocation inductively (or iteratively or recursively or repeatedly). (For example, the family tree of 15 is 15 8 6 5 and the family tree of 12 is 12 7. Note that this means that the smallest member of a "family tree" is always a prime.)

The "ancestors" of N are all members of its family tree excluding itself.

The "total" of a set of numbers X is the number of members in that set. In other words, the "total" of {5,6} is 2. (So we use the word "sum" instead of "total" when talking about addition in the context of this exercise, because this exercise requires that sort of doublethink.)

"Print" means "calculate and store somewhere".


### Implementation I



```J
require'strings files'

family=:3 :0 M.
  if. 2>y do.
    i.0   NB. no primes less than 2
  else.
    p=. i.&.(p:inv) y
    (y#~1 p:y),~.;p (* family)&.>y-p
  end.
)
 
familytree=: +/@q:^:a: ::(''"_)
 
descendants=: family -. ]
ancestors=: 1 }. familytree
level=: #@ancestors"0
 
taskfmt=:'None'"_^:(0=#)@rplc&(' ';', ')@":
 
task1=:3 :0
  text=. '[',(":y),'] Level: ',(":level y),LF
  text=. text,'Ancestors: ',(taskfmt /:~ancestors y),LF
  if. #descendants y do.
    text=. text,'Descendants: ',(":#descendants y),LF
    text=. text,(taskfmt /:~descendants y),LF
  else.
    text=. text,'Descendants: None',LF
  end.
  text=. text,LF
)
 
task=:3 :0
  tot=. 'Total descendants ',(":#@; descendants&.> 1+i.y),LF
  ((;task1&.>1+i.y),tot) fwrite jpath '~user/temp/Ancestors.txt'
)
 
task 99
```


If you want to inspect individual results, that's fairly straightforward.

The produced text file comes not from the task description but from Implementation II (except omitting the CR at line end - you can use unix/osx/linux/cygwin's <code>diff -bw</code> to compare the generated files).

Can we assume you use the '9!:11 +20' function in your profile? Otherwise the big values are shown in scientific notation.


###  Some examples 



```J
   #;descendants&.>1+i.99
546986
   level 46
3
   ancestors 46
25 10 7
   #descendants 46
557
   descendants 18
512 576 480 336 400 648 280 132 540 196 78 378 450 110 729 315 375 65 77
   level 18
3
   ancestors 18
8 6 5
   #descendants 18
19
```



### Implementation II

{{trans|Python}}

After reading the "Learning J" documentation up to chapter 9 + some additional verbs, I can post my first J script.

The script is based on the same logic as the Python script and therefore on the original task description.

However, I use the 'FamilyTree' function of implementation I, as the 'getancestors' function.

Furthermore, the script produces the full report in a '.txt' file which can easily be compared with the output of some of the other languages. In Windows : <code>fc /N /L</code>


```J
getdescendants=: 3 : 0
  dd=: (<(>y{dd),y)y}dd
  y getproducts"0 >:i.maxsum-y
)

getproducts=: 4 : 'dd=: (<(>(x+y){dd),x*(>y{dd))(x+y)}dd'

delfalsechildren=: 3 : 'dd=: ((}:&.>)y{dd)y}dd'

report=: 3 : 0
  ac=. getancestors y
  if. (level=. #ac) = 0 do.
    ls=. 'None'
  else.
    ls=. (' ';', ') stringreplace ":ac
  end.
  line=. '[',(":y),'] Level: ',(":level),CR,LF,'Ancestors: ',ls,CR,LF
  if. (nb=. #>y{dd) = 0 do.
    line=. line,'Descendants: ','None',CR,LF,CR,LF
  else.
    ls=. (' ';', ') stringreplace ":/:~>y{dd
    line=. line,'Descendants: ',(":nb),CR,LF,ls,CR,LF,CR,LF
  end.
  line fappend file
)

getancestors=: |.@:(1}.+/@:q:^:a: ::(''"_))

main=: 3 : 0
  if. (pp1=. 9!:10 '') < 20 do. (9!:11) 20 end.
  '' fwrite file
  maxsum=: y
  dd=: (maxsum+1)$a:
  primes=. i.&.(p:inv)maxsum+1
  getdescendants"0 primes
  delfalsechildren"0 primes,4
  report"0 >:i.maxsum
  ('Total descendants ',":+/#&>dd) fappend file
  if. (pp2=. 9!:10 '') ~: pp1 do. (9!:11) pp1 end.
)

file=: jpath '~user/temp/Ancestors.ijs.txt'
main 99
```



## Julia

{{trans|Go}}

```julia
using Primes

function ancestraldecendants(maxsum)
    aprimes = primes(maxsum)
    descendants = [Vector{Int}() for _ in 1:maxsum + 1]
    ancestors = [Vector{Int}() for _ in 1:maxsum + 1]
    for p in aprimes
        push!(descendants[p + 1], p)
        foreach(s -> append!(descendants[s + p], [p * pr for pr in descendants[s]]),
            2:length(descendants) - p)
    end
    foreach(p -> pop!(descendants[p + 1]), vcat(aprimes, [4]))
    total = 0
    for s in 1:maxsum
        sort!(descendants[s + 1])
        dstlen = length(descendants[s + 1])
        total += dstlen
        idx = findfirst(x -> x > maxsum, descendants[s + 1])
        idx  = (idx == nothing) ? dstlen : idx - 1
        foreach(d -> ancestors[d] = vcat(ancestors[s + 1], [s]), descendants[s + 1][1:idx])
        if s in vcat(collect(0:20), 46, 74, 99)
            print(lpad(s, 3), ":", lpad("$(length(ancestors[s + 1]))", 2))
            print(" Ancestor(s):", rpad("$(ancestors[s + 1])", 18))
            print(lpad("$(length(descendants[s + 1]))", 5), " Descendant(s): ")
            println(rpad(dstlen <= 10 ? "$(descendants[s + 1])" : "$(descendants[s + 1][1:10])\b, ...]", 40))
        end
    end
    print("Total descendants: ", total)
end

ancestraldecendants(99)

```
{{output}}
```txt

  1: 0 Ancestor(s):Int64[]               0 Descendant(s): Int64[]
  2: 0 Ancestor(s):Int64[]               0 Descendant(s): Int64[]
  3: 0 Ancestor(s):Int64[]               0 Descendant(s): Int64[]
  4: 0 Ancestor(s):Int64[]               0 Descendant(s): Int64[]
  5: 1 Ancestor(s):[5]                   1 Descendant(s): [6]
  6: 0 Ancestor(s):Int64[]               2 Descendant(s): [8, 9]
  7: 1 Ancestor(s):[6]                   2 Descendant(s): [10, 12]
  8: 1 Ancestor(s):[6]                   3 Descendant(s): [15, 16, 18]
  9: 2 Ancestor(s):[6, 7]                4 Descendant(s): [14, 20, 24, 27]
 10: 0 Ancestor(s):Int64[]               5 Descendant(s): [21, 25, 30, 32, 36]
 11: 2 Ancestor(s):[6, 7]                5 Descendant(s): [28, 40, 45, 48, 54]
 12: 0 Ancestor(s):Int64[]               7 Descendant(s): [35, 42, 50, 60, 64, 72, 81]
 13: 3 Ancestor(s):[6, 7, 9]             8 Descendant(s): [22, 56, 63, 75, 80, 90, 96, 108]
 14: 2 Ancestor(s):[6, 8]               10 Descendant(s): [33, 49, 70, 84, 100, 120, 128, 135, 144, 162]
 15: 2 Ancestor(s):[6, 8]               12 Descendant(s): [26, 44, 105, 112, 125, 126, 150, 160, 180, 192, ...]
 16: 0 Ancestor(s):Int64[]              14 Descendant(s): [39, 55, 66, 98, 140, 168, 189, 200, 225, 240, ...]
 17: 2 Ancestor(s):[6, 8]               16 Descendant(s): [52, 88, 99, 147, 175, 210, 224, 250, 252, 300, ...]
 18: 0 Ancestor(s):Int64[]              19 Descendant(s): [65, 77, 78, 110, 132, 196, 280, 315, 336, 375, ...]
 19: 3 Ancestor(s):[6, 7, 9]            22 Descendant(s): [34, 104, 117, 165, 176, 198, 245, 294, 350, 420, ...]
 20: 1 Ancestor(s):[10]                 26 Descendant(s): [51, 91, 130, 154, 156, 220, 264, 297, 392, 441, ...]
 46: 0 Ancestor(s):Int64[]             557 Descendant(s): [129, 205, 246, 493, 518, 529, 740, 806, 888, 999, ...]
 74: 4 Ancestor(s):[6, 7, 9, 13]      6336 Descendant(s): [213, 469, 670, 793, 804, 1333, 1342, 1369, 1534, 2014, ...]
 99: 0 Ancestor(s):Int64[]           38257 Descendant(s): [194, 1869, 2225, 2670, 2848, 3204, 3237, 4029, 4565, 5037, ...]
Total descendants: 546986

```



## Kotlin

{{trans|Python}}

```scala
// version 1.1.2

const val MAXSUM = 99

fun getPrimes(max: Int): List<Int> {
    if (max < 2) return emptyList<Int>()
    val lprimes = mutableListOf(2)
    outer@ for (x in 3..max step 2) {
        for (p in lprimes) if (x % p == 0) continue@outer
        lprimes.add(x)
    }
    return lprimes
}

fun main(args: Array<String>) {
    val descendants = Array(MAXSUM + 1) { mutableListOf<Long>() }
    val ancestors   = Array(MAXSUM + 1) { mutableListOf<Int>() }
    val primes = getPrimes(MAXSUM)

    for (p in primes) {
        descendants[p].add(p.toLong())
        for (s in 1 until descendants.size - p) {
            val temp = descendants[s + p] + descendants[s].map { p * it }
            descendants[s + p] = temp.toMutableList()
        }
    }

    for (p in primes + 4) descendants[p].removeAt(descendants[p].lastIndex)
    var total = 0

    for (s in 1..MAXSUM) {
        descendants[s].sort()
        total += descendants[s].size        
        for (d in descendants[s].takeWhile { it <= MAXSUM.toLong() }) {
            ancestors[d.toInt()] = (ancestors[s] + s).toMutableList()
        }
        if (s in 21..45 || s in 47..73 || s in 75 until MAXSUM) continue
        print("${"%2d".format(s)}: ${ancestors[s].size} Ancestor(s): ")
        print(ancestors[s].toString().padEnd(18))
        print("${"%5d".format(descendants[s].size)} Descendant(s): ")
        println("${descendants[s].joinToString(", ", "[", "]", 10)}")        
    }

    println("\nTotal descendants $total")  
}
```


{{out}}

```txt

 1: 0 Ancestor(s): []                    0 Descendant(s): []
 2: 0 Ancestor(s): []                    0 Descendant(s): []
 3: 0 Ancestor(s): []                    0 Descendant(s): []
 4: 0 Ancestor(s): []                    0 Descendant(s): []
 5: 0 Ancestor(s): []                    1 Descendant(s): [6]
 6: 1 Ancestor(s): [5]                   2 Descendant(s): [8, 9]
 7: 0 Ancestor(s): []                    2 Descendant(s): [10, 12]
 8: 2 Ancestor(s): [5, 6]                3 Descendant(s): [15, 16, 18]
 9: 2 Ancestor(s): [5, 6]                4 Descendant(s): [14, 20, 24, 27]
10: 1 Ancestor(s): [7]                   5 Descendant(s): [21, 25, 30, 32, 36]
11: 0 Ancestor(s): []                    5 Descendant(s): [28, 40, 45, 48, 54]
12: 1 Ancestor(s): [7]                   7 Descendant(s): [35, 42, 50, 60, 64, 72, 81]
13: 0 Ancestor(s): []                    8 Descendant(s): [22, 56, 63, 75, 80, 90, 96, 108]
14: 3 Ancestor(s): [5, 6, 9]            10 Descendant(s): [33, 49, 70, 84, 100, 120, 128, 135, 144, 162]
15: 3 Ancestor(s): [5, 6, 8]            12 Descendant(s): [26, 44, 105, 112, 125, 126, 150, 160, 180, 192, ...]
16: 3 Ancestor(s): [5, 6, 8]            14 Descendant(s): [39, 55, 66, 98, 140, 168, 189, 200, 225, 240, ...]
17: 0 Ancestor(s): []                   16 Descendant(s): [52, 88, 99, 147, 175, 210, 224, 250, 252, 300, ...]
18: 3 Ancestor(s): [5, 6, 8]            19 Descendant(s): [65, 77, 78, 110, 132, 196, 280, 315, 336, 375, ...]
19: 0 Ancestor(s): []                   22 Descendant(s): [34, 104, 117, 165, 176, 198, 245, 294, 350, 420, ...]
20: 3 Ancestor(s): [5, 6, 9]            26 Descendant(s): [51, 91, 130, 154, 156, 220, 264, 297, 392, 441, ...]
46: 3 Ancestor(s): [7, 10, 25]         557 Descendant(s): [129, 205, 246, 493, 518, 529, 740, 806, 888, 999, ...]
74: 5 Ancestor(s): [5, 6, 8, 16, 39]  6336 Descendant(s): [213, 469, 670, 793, 804, 1333, 1342, 1369, 1534, 2014, ...]
99: 1 Ancestor(s): [17]              38257 Descendant(s): [194, 1869, 2225, 2670, 2848, 3204, 3237, 4029, 4565, 5037, ...]

Total descendants 546986

```



## Perl

{{trans|Perl 6}}
{{libheader|ntheory}}

```perl
use List::Util qw(sum uniq);
use ntheory qw(nth_prime);

my $max = 99;
my %tree;

sub allocate {
    my($n, $i, $sum,, $prod) = @_;
    $i //= 0; $sum //= 0; $prod //= 1;

    for my $k (0..$max) {
        next if $k < $i;
        my $p = nth_prime($k+1);
        if (($sum + $p) <= $max) {
            allocate($n, $k, $sum + $p, $prod * $p);
        } else {
            last if $sum == $prod;
            $tree{$sum}{descendants}{$prod} = 1;
            $tree{$prod}{ancestor} = [uniq $sum, @{$tree{$sum}{ancestor}}] unless $prod > $max || $sum == 0;
            last;
        }
    }
}

sub abbrev { # abbreviate long lists to first and last 5 elements
    my(@d) = @_;
    return @d if @d < 11;
    @d[0 .. 4], '...', @d[-5 .. -1];
}

allocate($_) for 1 .. $max;

for (1 .. 15, 46, $max) {
    printf "%2d, %2d Ancestors: %-15s", $_, (scalar uniq @{$tree{$_}{ancestor}}),
        '[' . join(' ',uniq @{$tree{$_}{ancestor}}) . ']';
    my $dn = 0; my $dl = '';
    if ($tree{$_}{descendants}) {
        $dn = keys %{$tree{$_}{descendants}};
        $dl = join ' ', abbrev(sort { $a <=> $b } keys %{$tree{$_}{descendants}});
    }
    printf "%5d Descendants: %s", $dn, "[$dl]\n";
}

map { for my $k (keys %{$tree{$_}{descendants}}) { $total += $tree{$_}{descendants}{$k} } } 1..$max;
print "\nTotal descendants: $total\n";
```

{{out}}

```txt
 1,  0 Ancestors: [],                0 Descendants: []
 2,  0 Ancestors: [],                0 Descendants: []
 3,  0 Ancestors: [],                0 Descendants: []
 4,  0 Ancestors: [],                0 Descendants: []
 5,  0 Ancestors: [],                1 Descendants: [6]
 6,  1 Ancestors: [5],               2 Descendants: [8 9]
 7,  0 Ancestors: [],                2 Descendants: [10 12]
 8,  2 Ancestors: [5 6],             3 Descendants: [15 16 18]
 9,  2 Ancestors: [5 6],             4 Descendants: [14 20 24 27]
10,  1 Ancestors: [7],               5 Descendants: [21 25 30 32 36]
11,  0 Ancestors: [],                5 Descendants: [28 40 45 48 54]
12,  1 Ancestors: [7],               7 Descendants: [35 42 50 60 64 72 81]
13,  0 Ancestors: [],                8 Descendants: [22 56 63 75 80 90 96 108]
14,  3 Ancestors: [5 6 9],          10 Descendants: [33 49 70 84 100 120 128 135 144 162]
15,  3 Ancestors: [5 6 8],          12 Descendants: [26 44 105 112 125 ... 160 180 192 216 243]
46,  3 Ancestors: [7 10 25],       557 Descendants: [129 205 246 493 518 ... 14171760 15116544 15943230 17006112 19131876]
99,  1 Ancestors: [17],          38257 Descendants: [194 1869 2225 2670 2848 ... 3904305912313344 4117822641892980 4392344151352512 4941387170271576 5559060566555523]

Total descendants: 546986
```



## Perl 6

{{works with|Rakudo|2018.11}}


```perl6
my $max = 99;
my @primes = (2 .. $max).race(:16batch).grep: *.is-prime;
my %tree;
(1..$max).race(:16batch).map: {
    %tree{$_}<ancestor> = ();
    %tree{$_}<descendants> = {};
};


sub allocate ($n, $i = 0, $sum = 0, $prod = 1) {
    return if $n < 4;
    for @primes.kv -> $k, $p {
        next if $k < $i;
        if ($sum + $p) <= $n {
            allocate($n, $k, $sum + $p, $prod * $p);
        } else {
            last if $sum == $prod;
            %tree{$sum}<descendants>{$prod} = True;
            last if $prod > $max;
            %tree{$prod}<ancestor> = %tree{$sum}<ancestor> (|) $sum;
            last;
        }
    }
}

# abbreviate long lists to first and last 5 elements
sub abbrev (@d) { @d < 11 ?? @d !! ( @d.head(5), '...', @d.tail(5) ) }

my @print = flat 1 .. 15, 46, 74, $max;

(1 .. $max).map: -> $term {
    allocate($term);

    if $term == any( @print )  # print some representative lines
    {
        my $dn = +%tree{$term}<descendants> // 0;
        my $dl = abbrev(%tree{$term}<descendants>.keys.sort( +*) // ());
        printf "%2d, %2d Ancestors: %-14s %5d Descendants: %s\n",
          $term, %tree{$term}<ancestor>,
          "[{ %tree{$term}<ancestor>.keys.sort: +* }],", $dn, "[$dl]";
    }
}

say "\nTotal descendants: ",
  sum (1..$max).race(:16batch).map({ +%tree{$_}<descendants> });
```

{{out}}

```txt
 1,  0 Ancestors: [],                0 Descendants: []
 2,  0 Ancestors: [],                0 Descendants: []
 3,  0 Ancestors: [],                0 Descendants: []
 4,  0 Ancestors: [],                0 Descendants: []
 5,  0 Ancestors: [],                1 Descendants: [6]
 6,  1 Ancestors: [5],               2 Descendants: [8 9]
 7,  0 Ancestors: [],                2 Descendants: [10 12]
 8,  2 Ancestors: [5 6],             3 Descendants: [15 16 18]
 9,  2 Ancestors: [5 6],             4 Descendants: [14 20 24 27]
10,  1 Ancestors: [7],               5 Descendants: [21 25 30 32 36]
11,  0 Ancestors: [],                5 Descendants: [28 40 45 48 54]
12,  1 Ancestors: [7],               7 Descendants: [35 42 50 60 64 72 81]
13,  0 Ancestors: [],                8 Descendants: [22 56 63 75 80 90 96 108]
14,  3 Ancestors: [5 6 9],          10 Descendants: [33 49 70 84 100 120 128 135 144 162]
15,  3 Ancestors: [5 6 8],          12 Descendants: [26 44 105 112 125 ... 160 180 192 216 243]
46,  3 Ancestors: [7 10 25],       557 Descendants: [129 205 246 493 518 ... 14171760 15116544 15943230 17006112 19131876]
74,  5 Ancestors: [5 6 8 16 39],  6336 Descendants: [213 469 670 793 804 ... 418414128120 446308403328 470715894135 502096953744 564859072962]
99,  1 Ancestors: [17],          38257 Descendants: [194 1869 2225 2670 2848 ... 3904305912313344 4117822641892980 4392344151352512 4941387170271576 5559060566555523]

Total descendants: 546986
```



## Phix

{{trans|Go}}

```Phix
constant maxSum = 99

function getPrimes()
    sequence primes = {2}
    for x=3 to maxSum by 2 do
        bool zero = false
        for i=1 to length(primes) do
            if mod(x,primes[i]) == 0 then
                zero = true
                exit
            end if
        end for
        if not zero then
            primes = append(primes, x)
        end if
    end for
    return primes
end function

function stringify(sequence s)
    for i=1 to length(s) do
        s[i] = sprintf("%d",s[i])
    end for 
    return s
end function

procedure main()
atom t0 = time()
integer p
    sequence descendants = repeat({},maxSum+1),
             ancestors = repeat({},maxSum+1),
             primes = getPrimes()
 
    for i=1 to length(primes) do
        p = primes[i]
        descendants[p] = append(descendants[p], p)
        for s=1 to length(descendants)-p do
            descendants[s+p] &= sq_mul(descendants[s], p)
        end for
    end for
 
    p = 4
    for i=0 to length(primes) do
        if i>0 then p = primes[i] end if
        if length(descendants[p])!=0 then
            descendants[p] = descendants[p][1..$-1]
        end if
    end for

    integer total = 0
 
    for s=1 to maxSum do
        sequence x = sort(descendants[s])
        total += length(x)
        for i=1 to length(x) do
            atom d = x[i]
            if d>maxSum then exit end if
            ancestors[d] &= append(ancestors[s], s)
        end for
        if s<26 or find(s,{46,74,99}) then  
            sequence d = ancestors[s]
            integer l = length(d)
            string sp = iff(l=1?" ":"s")
            d = stringify(d)
            printf(1,"%2d: %d Ancestor%s: [%-14s", {s, l, sp, join(d)&"]"})
            d = sort(descendants[s])
            l = length(d)
            sp = iff(l=1?" ":"s")
            if l<10 then
                d = stringify(d)
            else
                d[4..-4] = {0}
                d = stringify(d)
                d[4] = "..."
            end if
            printf(1,"%5d Descendant%s: [%s]\n", {l, sp, join(d)})
        end if
    end for
    printf(1,"\nTotal descendants %d\n", total)
    ?elapsed(time()-t0)                         -- < 1s
    ?elapsed(5559060566555523/4_000_000_000)    -- > 16 days
end procedure
main()
```

{{out}}

```txt

 1: 0 Ancestors: []                 0 Descendants: []
 2: 0 Ancestors: []                 0 Descendants: []
 3: 0 Ancestors: []                 0 Descendants: []
 4: 0 Ancestors: []                 0 Descendants: []
 5: 0 Ancestors: []                 1 Descendant : [6]
 6: 1 Ancestor : [5]                2 Descendants: [8 9]
 7: 0 Ancestors: []                 2 Descendants: [10 12]
 8: 2 Ancestors: [5 6]              3 Descendants: [15 16 18]
 9: 2 Ancestors: [5 6]              4 Descendants: [14 20 24 27]
10: 1 Ancestor : [7]                5 Descendants: [21 25 30 32 36]
11: 0 Ancestors: []                 5 Descendants: [28 40 45 48 54]
12: 1 Ancestor : [7]                7 Descendants: [35 42 50 60 64 72 81]
13: 0 Ancestors: []                 8 Descendants: [22 56 63 75 80 90 96 108]
14: 3 Ancestors: [5 6 9]           10 Descendants: [33 49 70 ... 135 144 162]
15: 3 Ancestors: [5 6 8]           12 Descendants: [26 44 105 ... 192 216 243]
16: 3 Ancestors: [5 6 8]           14 Descendants: [39 55 66 ... 270 288 324]
17: 0 Ancestors: []                16 Descendants: [52 88 99 ... 405 432 486]
18: 3 Ancestors: [5 6 8]           19 Descendants: [65 77 78 ... 576 648 729]
19: 0 Ancestors: []                22 Descendants: [34 104 117 ... 810 864 972]
20: 3 Ancestors: [5 6 9]           26 Descendants: [51 91 130 ... 1215 1296 1458]
21: 2 Ancestors: [7 10]            30 Descendants: [38 68 195 ... 1728 1944 2187]
22: 1 Ancestor : [13]              35 Descendants: [57 85 102 ... 2430 2592 2916]
23: 0 Ancestors: []                39 Descendants: [76 136 153 ... 3645 3888 4374]
24: 3 Ancestors: [5 6 9]           46 Descendants: [95 114 119 ... 5184 5832 6561]
25: 2 Ancestors: [7 10]            52 Descendants: [46 152 171 ... 7290 7776 8748]
46: 3 Ancestors: [7 10 25]        557 Descendants: [129 205 246 ... 15943230 17006112 19131876]
74: 5 Ancestors: [5 6 8 16 39]   6336 Descendants: [213 469 670 ... 470715894135 502096953744 564859072962]
99: 1 Ancestor : [17]           38257 Descendants: [194 1869 2225 ... 4392344151352512 4941387170271576 5559060566555523]

Total descendants 546986
"0.7s"
"16 days, 2 hours, 2 minutes and 45s"

```

The quick test at the end suggests that a 4Ghz chip would take at least 16 days just to count to 5559060566555523, let alone 
decompose those numbers into prime factors (and throwing away the ones you don't need, probably more like 10 million years),
which as requested in the task description obviously demonstrates that the algorithm can be crucial in terms of performance.


## Python

Python is very flexible, concise and effective with lists.

```python
from __future__ import print_function
from itertools import takewhile

maxsum = 99

def get_primes(max):
    if max < 2:
        return []
    lprimes = [2]
    for x in range(3, max + 1, 2):
        for p in lprimes:
            if x % p == 0:
                break
        else:
            lprimes.append(x)
    return lprimes

descendants = [[] for _ in range(maxsum + 1)]
ancestors = [[] for _ in range(maxsum + 1)]

primes = get_primes(maxsum)

for p in primes:
    descendants[p].append(p)
    for s in range(1, len(descendants) - p):
        descendants[s + p] += [p * pr for pr in descendants[s]]

for p in primes + [4]:
    descendants[p].pop()

total = 0
for s in range(1, maxsum + 1):
    descendants[s].sort()
    for d in takewhile(lambda x: x <= maxsum, descendants[s]):
        ancestors[d] = ancestors[s] + [s]
    print([s], "Level:", len(ancestors[s]))
    print("Ancestors:", ancestors[s] if len(ancestors[s]) else "None")
    print("Descendants:", len(descendants[s]) if len(descendants[s]) else "None")
    if len(descendants[s]):
        print(descendants[s])
    print()
    total += len(descendants[s])

print("Total descendants", total)
```



## Racket

{{trans|Python}}
I think that this is not anymore a translation of Python, since the 'Python script I' is gone.

The program has a few changes from the versions in other languages. The equation <code>p*q=p+q</code> has only one integer solution, so all the ancestor candidates are smaller than the number, except for <code>4=2+2=2*2</code>. So we can replace the inecuality by a special case for <code>4</code>.

We only show here a few values to be able to compare the output. We also show the total number of ancestors.

We first define a macro to create memorized functions and a few auxiliary functions. In particular <code>(border list)</code> transforms a long list in a list with ellipsis.


```Racket
#lang racket

(define-syntax-rule (define/mem (name args ...) body ...)
  (begin
    (define cache (make-hash))
    (define (name args ...)
      (hash-ref! cache (list args ...) (lambda () body ...)))))

(define (take-last x n)
  (drop x (- (length x) n)))

(define (borders x)
  (if (> (length x) 5)
    (append (take x 2) '(...) (take-last x 2))
    x))
  
(define (add-tail list x)
  (reverse (cons x (reverse list))))
```


The main part of the program uses the memorized functions.


```Racket
 (define/mem (prime? x)
  (if (= x 1)
    #f
    (not (for/or ([p (in-range 2 x)]
                  #:break (> (sqr p) x))
           (zero? (remainder x p))))))

(define (map* p list)
  (map (lambda (x) (* x p)) list))

(define/mem (part-prod x p)
  (cond
    [(< x 0) '()]
    [(zero? x) (list 1)]
    [(zero? p) '()]
    [(not (prime? p)) (part-prod x (sub1 p))]
    [else (append (map* p (part-prod (- x p) p))
                  (part-prod x (sub1 p)))]))
 
(define/mem (descendants x)
  (if (= x 4)
      '()
      (sort (part-prod x (sub1 x)) <)))

(define/mem (ancestors z)
  (let ([tmp (for/first ([x (in-range (sub1 z) 0 -1)]
                         #:when (member z (descendants x)))
               (add-tail (ancestors x) x))])
    (if tmp tmp '())))

(define (show-info x)
  (printf "~a " x)
  (printf "Ancestors: ~a ~a " (length (ancestors x)) (ancestors x))
  (printf "Descendants: ~a ~a " (length (descendants x)) (borders (descendants x)))
  (newline))

(define (total-ancestors n)
  (for/sum ([x (in-range 1 (add1 n))])
    (length (ancestors x))))

(define (total-descendants n)
  (for/sum ([x (in-range 1 (add1 n))])
    (length (descendants x))))
```


Now we display some results.


```Racket
#;(for ([x (in-range 1 (add1 99))])
    (show-info x))
(for ([x (in-range 1 (add1 15))])
  (show-info x))

(newline)
(show-info 18)
(show-info 46)
(show-info 99)

(newline)
(printf "Total ancestors up to 99: ~a\n" (total-ancestors 99))
(printf "Total descendants up to 99: ~a\n" (total-descendants 99))
```


{{out}}


```txt
1 Ancestors: 0 () Descendants: 0 () 
2 Ancestors: 0 () Descendants: 0 () 
3 Ancestors: 0 () Descendants: 0 () 
4 Ancestors: 0 () Descendants: 0 () 
5 Ancestors: 0 () Descendants: 1 (6) 
6 Ancestors: 1 (5) Descendants: 2 (8 9) 
7 Ancestors: 0 () Descendants: 2 (10 12) 
8 Ancestors: 2 (5 6) Descendants: 3 (15 16 18) 
9 Ancestors: 2 (5 6) Descendants: 4 (14 20 24 27) 
10 Ancestors: 1 (7) Descendants: 5 (21 25 30 32 36) 
11 Ancestors: 0 () Descendants: 5 (28 40 45 48 54) 
12 Ancestors: 1 (7) Descendants: 7 (35 42 ... 72 81) 
13 Ancestors: 0 () Descendants: 8 (22 56 ... 96 108) 
14 Ancestors: 3 (5 6 9) Descendants: 10 (33 49 ... 144 162) 
15 Ancestors: 3 (5 6 8) Descendants: 12 (26 44 ... 216 243) 

18 Ancestors: 3 (5 6 8) Descendants: 19 (65 77 ... 648 729) 
46 Ancestors: 3 (7 10 25) Descendants: 557 (129 205 ... 17006112 19131876) 
99 Ancestors: 1 (17) Descendants: 38257 (194 1869 ... 4941387170271576 5559060566555523) 

Total ancestors up to 99: 179
Total descendants up to 99: 546986
```


## Sidef

{{trans|Go}}

```ruby
var maxsum = 99
var primes = maxsum.primes

var descendants = (maxsum+1).of { [] }
var ancestors   = (maxsum+1).of { [] }

for p in (primes) {
    descendants[p] << p
    for s in (1 .. descendants.end-p) {
        descendants[s + p] << descendants[s].map {|q| p*q }...
    }
}

for p in (primes + [4]) {
    descendants[p].pop
}

var total = 0

for s in (1 .. maxsum) {

    descendants[s].sort!

    total += (var dsclen = descendants[s].len)
    var idx = descendants[s].first_index {|x| x > maxsum }

    for d in (descendants[s].slice(0, idx)) {
        ancestors[d] = (ancestors[s] + [s])
    }

    if ((s <= 20) || (s ~~ [46, 74, 99])) {
        printf("%2d: %d Ancestor(s): %-15s %5s Descendant(s): %s\n", s,
            ancestors[s].len, "[#{ancestors[s].join(' ')}]", descendants[s].len,
            dsclen <= 10 ? descendants[s] : "[#{descendants[s].first(10).join(' ')} ...]")
    }
}

say "\nTotal descendants: #{total}"
```

{{out}}

```txt

 1: 0 Ancestor(s): []                  0 Descendant(s): []
 2: 0 Ancestor(s): []                  0 Descendant(s): []
 3: 0 Ancestor(s): []                  0 Descendant(s): []
 4: 0 Ancestor(s): []                  0 Descendant(s): []
 5: 0 Ancestor(s): []                  1 Descendant(s): [6]
 6: 1 Ancestor(s): [5]                 2 Descendant(s): [8, 9]
 7: 0 Ancestor(s): []                  2 Descendant(s): [10, 12]
 8: 2 Ancestor(s): [5 6]               3 Descendant(s): [15, 16, 18]
 9: 2 Ancestor(s): [5 6]               4 Descendant(s): [14, 20, 24, 27]
10: 1 Ancestor(s): [7]                 5 Descendant(s): [21, 25, 30, 32, 36]
11: 0 Ancestor(s): []                  5 Descendant(s): [28, 40, 45, 48, 54]
12: 1 Ancestor(s): [7]                 7 Descendant(s): [35, 42, 50, 60, 64, 72, 81]
13: 0 Ancestor(s): []                  8 Descendant(s): [22, 56, 63, 75, 80, 90, 96, 108]
14: 3 Ancestor(s): [5 6 9]            10 Descendant(s): [33, 49, 70, 84, 100, 120, 128, 135, 144, 162]
15: 3 Ancestor(s): [5 6 8]            12 Descendant(s): [26 44 105 112 125 126 150 160 180 192 ...]
16: 3 Ancestor(s): [5 6 8]            14 Descendant(s): [39 55 66 98 140 168 189 200 225 240 ...]
17: 0 Ancestor(s): []                 16 Descendant(s): [52 88 99 147 175 210 224 250 252 300 ...]
18: 3 Ancestor(s): [5 6 8]            19 Descendant(s): [65 77 78 110 132 196 280 315 336 375 ...]
19: 0 Ancestor(s): []                 22 Descendant(s): [34 104 117 165 176 198 245 294 350 420 ...]
20: 3 Ancestor(s): [5 6 9]            26 Descendant(s): [51 91 130 154 156 220 264 297 392 441 ...]
46: 3 Ancestor(s): [7 10 25]         557 Descendant(s): [129 205 246 493 518 529 740 806 888 999 ...]
74: 5 Ancestor(s): [5 6 8 16 39]    6336 Descendant(s): [213 469 670 793 804 1333 1342 1369 1534 2014 ...]
99: 1 Ancestor(s): [17]            38257 Descendant(s): [194 1869 2225 2670 2848 3204 3237 4029 4565 5037 ...]

Total descendants: 546986

```



## Simula

{{trans|Python}}

```simula
COMMENT cim --memory-pool-size=512 allocate-descendants-to-their-ancestors.sim ;
BEGIN
 

    COMMENT ABSTRACT FRAMEWORK CLASSES ;

    CLASS ITEM;
    BEGIN
    END ITEM;

    CLASS ITEMLIST;
    BEGIN

        CLASS ITEMARRAY(N); INTEGER N;
        BEGIN REF(ITEM) ARRAY DATA(0:N-1);
        END ITEMARRAY;

        PROCEDURE EXPAND(N); INTEGER N;
        BEGIN
            INTEGER I;
            REF(ITEMARRAY) TEMP;
            TEMP :- NEW ITEMARRAY(N);
            FOR I := 0 STEP 1 UNTIL SIZE-1 DO
                TEMP.DATA(I) :- ITEMS.DATA(I);
            ITEMS :- TEMP;
        END EXPAND;

        PROCEDURE APPEND(RI); REF(ITEM) RI;
        BEGIN
            IF SIZE + 1 > CAPACITY THEN
            BEGIN
                CAPACITY := 2 * CAPACITY;
                EXPAND(CAPACITY);
            END;
            ITEMS.DATA(SIZE) :- RI;
            SIZE := SIZE + 1;
        END APPEND;

        PROCEDURE APPENDALL(IL); REF(ITEMLIST) IL;
        BEGIN
            INTEGER I;
            FOR I := 0 STEP 1 UNTIL IL.SIZE-1 DO
                APPEND(IL.ELEMENT(I));
        END APPENDALL;

        REF(ITEM) PROCEDURE ELEMENT(I); INTEGER I;
        BEGIN
            IF I < 0 OR I > SIZE-1 THEN ERROR("ELEMENT: INDEX OUT OF BOUNDS");
            ELEMENT :- ITEMS.DATA(I);
        END ELEMENT;

        REF(ITEM) PROCEDURE SETELEMENT(I, IT); INTEGER I; REF(ITEM) IT;
        BEGIN
            IF I < 0 OR I > SIZE-1 THEN ERROR("SETELEMENT: INDEX OUT OF BOUNDS");
            ITEMS.DATA(I) :- IT;
        END SETELEMENT;

        REF(ITEM) PROCEDURE POP;
        BEGIN
            REF(ITEM) RESULT;
            IF SIZE=0 THEN ERROR("POP: EMPTY ITEMLIST");
            RESULT :- ITEMS.DATA(SIZE-1);
            ITEMS.DATA(SIZE-1) :- NONE;
            SIZE := SIZE-1;
            POP :- RESULT;
        END POP;

        PROCEDURE SORT(COMPARE_PROC);
            PROCEDURE COMPARE_PROC IS
                INTEGER PROCEDURE COMPARE_PROC(IT1,IT2); REF(ITEM) IT1,IT2;;
        BEGIN
            PROCEDURE SWAP(I,J); INTEGER I,J;
            BEGIN
               REF(ITEM) T;
               T :- ITEMS.DATA(I);
               ITEMS.DATA(I) :- ITEMS.DATA(J);
               ITEMS.DATA(J) :- T;
            END SWAP;
            PROCEDURE QUICKSORT(L,R); INTEGER L,R;
            BEGIN
               REF(ITEM) PIVOT;
               INTEGER I, J;
               PIVOT :- ITEMS.DATA((L+R)//2); I := L; J := R;
               WHILE I <= J DO
               BEGIN
                  WHILE COMPARE_PROC(ITEMS.DATA(I), PIVOT) < 0 DO I := I+1;
                  WHILE COMPARE_PROC(PIVOT, ITEMS.DATA(J)) < 0 DO J := J-1;
                  IF I <= J THEN
                  BEGIN SWAP(I,J); I := I+1; J := J-1;
                  END;
               END;
               IF L < J THEN QUICKSORT(L, J);
               IF I < R THEN QUICKSORT(I, R);
            END QUICKSORT;
            IF SIZE >= 2 THEN
               QUICKSORT(0,SIZE-1);
        END SORT;

        INTEGER CAPACITY;
        INTEGER SIZE;
        REF(ITEMARRAY) ITEMS;

        CAPACITY := 20;
        SIZE := 0;
        EXPAND(CAPACITY);
    END ITEMLIST;


    COMMENT PROBLEM SPECIFIC PART ;


    ITEM CLASS REALITEM(X); LONG REAL X;
    BEGIN
    END REALITEM;

    ITEMLIST CLASS LIST_OF_REAL;
    BEGIN
        LONG REAL PROCEDURE ELEMENT(I); INTEGER I;
            ELEMENT := ITEMS.DATA(I) QUA REALITEM.X;

        PROCEDURE APPEND(X); LONG REAL X;
            THIS ITEMLIST.APPEND(NEW REALITEM(X));

        PROCEDURE SORT;
        BEGIN
            INTEGER PROCEDURE CMP(IT1,IT2); REF(ITEM) IT1,IT2;
                CMP := IF IT1 QUA REALITEM.X < IT2 QUA REALITEM.X THEN -1 ELSE
                       IF IT1 QUA REALITEM.X > IT2 QUA REALITEM.X THEN +1 ELSE 0;
            THIS ITEMLIST.SORT(CMP);
        END SORT;

        PROCEDURE OUTLIST;
        BEGIN
            INTEGER I;
            TEXT FMT;
            OUTTEXT("[");
            FMT :- BLANKS(20);
            FOR I := 0 STEP 1 UNTIL SIZE-1 DO
            BEGIN
                IF I < 3 OR I > SIZE-1-3 THEN BEGIN
                    IF I > 0 THEN OUTTEXT(", ");
                    FMT.PUTFIX(ELEMENT(I), 0);
                    FMT.SETPOS(1);
                    WHILE FMT.MORE DO
                    BEGIN
                        CHARACTER C;
                        C := FMT.GETCHAR;
                        IF C <> ' ' THEN OUTCHAR(C);
                    END
                END ELSE BEGIN OUTTEXT(", ..."); I := SIZE-1-3; END;
            END;
            OUTTEXT("]");
        END OUTLIST;
    END LIST_OF_REAL;


    ITEM CLASS REALLISTITEM(LRL); REF(LIST_OF_REAL) LRL;
    BEGIN
    END REALLISTITEM;

    ITEMLIST CLASS LIST_OF_REALLIST;
    BEGIN
        REF(LIST_OF_REAL) PROCEDURE ELEMENT(I); INTEGER I;
            ELEMENT :- ITEMS.DATA(I) QUA REALLISTITEM.LRL;

        PROCEDURE APPEND(LRL); REF(LIST_OF_REAL) LRL;
            THIS ITEMLIST.APPEND(NEW REALLISTITEM(LRL));

        PROCEDURE OUTLIST;
        BEGIN
            INTEGER I;
            OUTTEXT("[");
            FOR I := 0 STEP 1 UNTIL SIZE-1 DO
            BEGIN
                IF I > 0 THEN OUTTEXT(", ");
                ELEMENT(I).OUTLIST;
            END;
            OUTTEXT("]");
        END OUTLIST;
    END LIST_OF_REALLIST;

    REF(LIST_OF_REAL) PROCEDURE GET_PRIMES(MAX);
        INTEGER MAX;
    BEGIN
        REF(LIST_OF_REAL) LPRIMES;
        LPRIMES :- NEW LIST_OF_REAL;
        IF MAX < 2 THEN
            GOTO RETURN
        ELSE
        BEGIN
            INTEGER X;
            LPRIMES.APPEND(2);
            FOR X := 3 STEP 2 UNTIL MAX DO BEGIN
                INTEGER I;
                LONG REAL P;
                FOR I := 0 STEP 1 UNTIL LPRIMES.SIZE-1 DO BEGIN
                    P := LPRIMES.ELEMENT(I);
                    IF (X / P) = ENTIER(X / P) THEN GOTO BREAK;
                END;
                LPRIMES.APPEND(X);
            BREAK:
            END;
        END;
    RETURN:
        GET_PRIMES :- LPRIMES;
    END GET_PRIMES;
  
    INTEGER MAXSUM, I, S, PRI, TOTAL, DI;
    REF(LIST_OF_REALLIST) DESCENDANTS, ANCESTORS;
    REF(LIST_OF_REAL) PRIMES, LR, LRS;
    LONG REAL P, D, PR;
    BOOLEAN TAKEWHILE;
    MAXSUM := 99;

    DESCENDANTS :- NEW LIST_OF_REALLIST;
    ANCESTORS   :- NEW LIST_OF_REALLIST;
    FOR I := 0 STEP 1 UNTIL MAXSUM DO BEGIN
        DESCENDANTS.APPEND(NEW LIST_OF_REAL);
        ANCESTORS  .APPEND(NEW LIST_OF_REAL);
    END;

    PRIMES :- GET_PRIMES(MAXSUM);

    FOR I := 0 STEP 1 UNTIL PRIMES.SIZE-1 DO
    BEGIN
        P := PRIMES.ELEMENT(I);
        DESCENDANTS.ELEMENT(P).APPEND(P);
        FOR S := 1 STEP 1 UNTIL DESCENDANTS.SIZE-P-1 DO
        BEGIN
            LRS :- DESCENDANTS.ELEMENT(S);
            FOR PRI := 0 STEP 1 UNTIL LRS.SIZE-1 DO
            BEGIN
                PR := LRS.ELEMENT(PRI);
                DESCENDANTS.ELEMENT(S + P).APPEND(P * PR);
            END;
        END;
    END;
    
    FOR I := 0 STEP 1 UNTIL PRIMES.SIZE-1 DO
    BEGIN
        P := PRIMES.ELEMENT(I);
        DESCENDANTS.ELEMENT(P).POP;
    END;
    DESCENDANTS.ELEMENT(4).POP;

    TOTAL := 0;
    FOR S := 1 STEP 1 UNTIL MAXSUM DO
    BEGIN
        LRS :- DESCENDANTS.ELEMENT(S);
        LRS.SORT;
        FOR DI := 0 STEP 1 UNTIL LRS.SIZE-1 DO
        BEGIN
            D := LRS.ELEMENT(DI);
            IF D <= MAXSUM THEN
            BEGIN
                REF(LIST_OF_REAL) ANCD;
                ANCD :- NEW LIST_OF_REAL;
                ANCD.APPENDALL(ANCESTORS.ELEMENT(S));
                ANCD.APPEND(S);
                ANCESTORS.SETELEMENT(D, NEW REALLISTITEM(ANCD));
            END
            ELSE GOTO BREAK;
        END;
        BREAK:

        OUTTEXT("[");
        OUTINT(S, 0);
        OUTTEXT("] LEVEL: ");
        OUTINT(ANCESTORS.ELEMENT(S).SIZE, 0);
        OUTIMAGE;

        OUTTEXT("ANCESTORS: ");
        ANCESTORS.ELEMENT(S).OUTLIST;
        OUTIMAGE;

        OUTTEXT("DESCENDANTS: ");
        OUTINT(LRS.SIZE,0);
        OUTIMAGE;

        LRS.OUTLIST;
        OUTIMAGE;

        OUTIMAGE;
        TOTAL := TOTAL + LRS.SIZE;
    END;

    OUTTEXT("TOTAL DESCENDANTS ");
    OUTINT(TOTAL, 0);
    OUTIMAGE;
END.
```

{{out}}

```txt
[1] LEVEL: 0
ANCESTORS: []
DESCENDANTS: 0
[]

[2] LEVEL: 0
ANCESTORS: []
DESCENDANTS: 0
[]

[3] LEVEL: 0
ANCESTORS: []
DESCENDANTS: 0
[]

[4] LEVEL: 0
ANCESTORS: []
DESCENDANTS: 0
[]

[5] LEVEL: 0
ANCESTORS: []
DESCENDANTS: 1
[6]

[6] LEVEL: 1
ANCESTORS: [5]
DESCENDANTS: 2
[8, 9]

[7] LEVEL: 0
ANCESTORS: []
DESCENDANTS: 2
[10, 12]

[8] LEVEL: 2
ANCESTORS: [5, 6]
DESCENDANTS: 3
[15, 16, 18]

[9] LEVEL: 2
ANCESTORS: [5, 6]
DESCENDANTS: 4
[14, 20, 24, 27]

[10] LEVEL: 1
ANCESTORS: [7]
DESCENDANTS: 5
[21, 25, 30, 32, 36]

[11] LEVEL: 0
ANCESTORS: []
DESCENDANTS: 5
[28, 40, 45, 48, 54]

[12] LEVEL: 1
ANCESTORS: [7]
DESCENDANTS: 7
[35, 42, 50, ..., 64, 72, 81]

[13] LEVEL: 0
ANCESTORS: []
DESCENDANTS: 8
[22, 56, 63, ..., 90, 96, 108]

[14] LEVEL: 3
ANCESTORS: [5, 6, 9]
DESCENDANTS: 10
[33, 49, 70, ..., 135, 144, 162]

[15] LEVEL: 3
ANCESTORS: [5, 6, 8]
DESCENDANTS: 12
[26, 44, 105, ..., 192, 216, 243]

.....

[96] LEVEL: 1
ANCESTORS: [13]
DESCENDANTS: 31246
[623, 890, 1068, ..., 1464114717117504, 1647129056757192, 1853020188851841]

[97] LEVEL: 0
ANCESTORS: []
DESCENDANTS: 33438
[1335, 1424, 1602, ..., 2058911320946490, 2196172075676256, 2470693585135788]

[98] LEVEL: 4
ANCESTORS: [5, 6, 8, 16]
DESCENDANTS: 35772
[1246, 1501, 1780, ..., 3088366981419735, 3294258113514384, 3706040377703682]

[99] LEVEL: 1
ANCESTORS: [17]
DESCENDANTS: 38257
[194, 1869, 2225, ..., 4392344151352512, 4941387170271576, 5559060566555523]

TOTAL DESCENDANTS 546986

```



## Visual Basic .NET

It is based on the same logic as the Python script.

```vbnet
Imports System.Math

Module Module1
    Const MAXPRIME = 99                             ' upper bound for the prime factors
    Const MAXPARENT = 99                            ' greatest parent number

    Const NBRCHILDREN = 547100                      ' max number of children (total descendants)

    Public Primes As New Collection()               ' table of the prime factors
    Public PrimesR As New Collection()              ' table of the prime factors in reversed order
    Public Ancestors As New Collection()            ' table of the parent's ancestors

    Public Parents(MAXPARENT + 1) As Integer        ' index table of the root descendant (per parent)
    Public CptDescendants(MAXPARENT + 1) As Integer ' counter table of the descendants (per parent)
    Public Children(NBRCHILDREN) As ChildStruct     ' table of the whole descendants
    Public iChildren As Integer                     ' max index of the Children table

    Public Delimiter As String = ", "
    Public Structure ChildStruct
        Public Child As Long
        Public pLower As Integer
        Public pHigher As Integer
    End Structure
    Sub Main()
        Dim Parent As Short
        Dim Sum As Short
        Dim i As Short
        Dim TotDesc As Integer = 0
        Dim MidPrime As Integer

        If GetPrimes(Primes, MAXPRIME) = vbFalse Then
            Return
        End If

        For i = Primes.Count To 1 Step -1
            PrimesR.Add(Primes.Item(i))
        Next

        MidPrime = PrimesR.Item(1) / 2

        For Each Prime In PrimesR
            Parents(Prime) = InsertChild(Parents(Prime), Prime)
            CptDescendants(Prime) += 1

            If Prime > MidPrime Then
                Continue For
            End If

            For Parent = 1 To MAXPARENT
                Sum = Parent + Prime

                If Sum > MAXPARENT Then
                    Exit For
                End If

                If Parents(Parent) Then
                    InsertPreorder(Parents(Parent), Sum, Prime)
                    CptDescendants(Sum) += CptDescendants(Parent)
                End If
            Next
        Next

        RemoveFalseChildren()

        If MAXPARENT > MAXPRIME Then
            If GetPrimes(Primes, MAXPARENT) = vbFalse Then
                Return
            End If
        End If

        FileOpen(1, "Ancestors.txt", OpenMode.Output)

        For Parent = 1 To MAXPARENT
            GetAncestors(Parent)
            PrintLine(1, "[" & Parent.ToString & "] Level: " & Ancestors.Count.ToString)

            If Ancestors.Count Then
                Print(1, "Ancestors: " & Ancestors.Item(1).ToString)
                For i = 2 To Ancestors.Count
                    Print(1, ", " & Ancestors.Item(i).ToString)
                Next
                PrintLine(1)
                Ancestors.Clear()
            Else
                PrintLine(1, "Ancestors: None")
            End If

            If CptDescendants(Parent) Then
                PrintLine(1, "Descendants: " & CptDescendants(Parent).ToString)
                Delimiter = ""
                PrintDescendants(Parents(Parent))
                PrintLine(1)
                TotDesc += CptDescendants(Parent)
            Else
                PrintLine(1, "Descendants: None")
            End If

            PrintLine(1)
        Next
        Primes.Clear()
        PrimesR.Clear()
        PrintLine(1, "Total descendants " & TotDesc.ToString)
        PrintLine(1)
        FileClose(1)
    End Sub
    Function InsertPreorder(_index As Integer, _sum As Short, _prime As Short)
        Parents(_sum) = InsertChild(Parents(_sum), Children(_index).Child * _prime)

        If Children(_index).pLower Then
            InsertPreorder(Children(_index).pLower, _sum, _prime)
        End If

        If Children(_index).pHigher Then
            InsertPreorder(Children(_index).pHigher, _sum, _prime)
        End If

        Return Nothing
    End Function
    Function InsertChild(_index As Integer, _child As Long) As Integer
        If _index Then
            If _child <= Children(_index).Child Then
                Children(_index).pLower = InsertChild(Children(_index).pLower, _child)
            Else
                Children(_index).pHigher = InsertChild(Children(_index).pHigher, _child)
            End If
        Else
            iChildren += 1
            _index = iChildren
            Children(_index).Child = _child
            Children(_index).pLower = 0
            Children(_index).pHigher = 0
        End If

        Return _index
    End Function
    Function RemoveFalseChildren()
        Dim Exclusions As New Collection

        Exclusions.Add(4)
        For Each Prime In Primes
            Exclusions.Add(Prime)
        Next

        For Each ex In Exclusions
            Parents(ex) = Children(Parents(ex)).pHigher
            CptDescendants(ex) -= 1
        Next

        Exclusions.Clear()
        Return Nothing
    End Function
    Function GetAncestors(_child As Short)
        Dim Child As Short = _child
        Dim Parent As Short = 0

        For Each Prime In Primes
            If Child = 1 Then
                Exit For
            End If
            While Child Mod Prime = 0
                Child /= Prime
                Parent += Prime
            End While
        Next

        If Parent = _child Or _child = 1 Then
            Return Nothing
        End If

        GetAncestors(Parent)
        Ancestors.Add(Parent)
        Return Nothing
    End Function
    Function PrintDescendants(_index As Integer)
        If Children(_index).pLower Then
            PrintDescendants(Children(_index).pLower)
        End If

        Print(1, Delimiter.ToString & Children(_index).Child.ToString)
        Delimiter = ", "

        If Children(_index).pHigher Then
            PrintDescendants(Children(_index).pHigher)
        End If

        Return Nothing
    End Function
    Function GetPrimes(ByRef _primes As Object, Optional _maxPrime As Integer = 2) As Boolean
        Dim Value As Integer = 3
        Dim Max As Integer
        Dim Prime As Integer

        If _maxPrime < 2 Then
            Return vbFalse
        End If

        _primes.Add(2)

        While Value <= _maxPrime
            Max = Floor(Sqrt(Value))

            For Each Prime In _primes
                If Prime > Max Then
                    _primes.Add(Value)
                    Exit For
                End If

                If Value Mod Prime = 0 Then
                    Exit For
                End If
            Next

            Value += 2
        End While

        Return vbTrue
    End Function
End Module
```



## zkl

{{trans|Python}}
{{trans|Racket}}
Using [[Extensible prime generator#zkl]]

```zkl
const maxsum=99;
 
primes:=Utils.Generator(Import("sieve.zkl").postponed_sieve)
        .pump(List,'wrap(p){ (p<=maxsum) and p or Void.Stop });

descendants,ancestors:=List()*(maxsum + 1), List()*(maxsum + 1);

foreach p in (primes){
   descendants[p].insert(0,p);
   foreach s in ([1..descendants.len() - p - 1]){
      descendants[s + p].merge(descendants[s].apply('*(p)));
   }
}

    // descendants[prime] is a list that starts with prime, remove prime. 4: ???
foreach p in (primes + 4) { descendants[p].pop(0) }
 
ta,td:=0,0;
foreach s in ([1..maxsum]){
   foreach d in (descendants[s].filter('<=(maxsum))){
      ancestors[d]=ancestors[s].copy() + s;
   }

   println("%2d Ancestors: ".fmt(s),ancestors[s].len() and ancestors[s] or "None");
   println("   Descendants: ", if(z:=descendants[s]) 
				String(z.len()," : ",z) else "None");
   ta+=ancestors[s].len(); td+=descendants[s].len();
} 
println("Total ancestors: %,d".fmt(ta));
println("Total descendants: %,d".fmt(td));
```

{{out}}

```txt

 1 Ancestors: None
   Descendants: None
 2 Ancestors: None
   Descendants: None
 3 Ancestors: None
   Descendants: None
 4 Ancestors: None
   Descendants: None
 5 Ancestors: None
   Descendants: 1 : L(6)
 6 Ancestors: L(5)
   Descendants: 2 : L(8,9)
 7 Ancestors: None
   Descendants: 2 : L(10,12)
 8 Ancestors: L(5,6)
   Descendants: 3 : L(15,16,18)
 9 Ancestors: L(5,6)
   Descendants: 4 : L(14,20,24,27)
10 Ancestors: L(7)
   Descendants: 5 : L(21,25,30,32,36)
11 Ancestors: None
   Descendants: 5 : L(28,40,45,48,54)
12 Ancestors: L(7)
   Descendants: 7 : L(35,42,50,60,64,72,81)
13 Ancestors: None
   Descendants: 8 : L(22,56,63,75,80,90,96,108)
14 Ancestors: L(5,6,9)
   Descendants: 10 : L(33,49,70,84,100,120,128,135,144,162)
15 Ancestors: L(5,6,8)
   Descendants: 12 : L(26,44,105,112,125,126,150,160,180,192,216,243)

18 Ancestors: L(5,6,8)
   Descendants: 19 : L(65,77,78,110,132,196,280,315,336,375,378,400,450,480,512,540,576,648,729)
46 Ancestors: L(7,10,25)
   Descendants: 557 : L(129,205,246,493,518,529,740,806,888,999,1364,1508,1748,2552,2871,3128,3255,3472,3519,3875,...)
99 Ancestors: L(17)
   Descendants: 38257 : L(194,1869,2225,2670,2848,3204,3237,4029,4565,5037,5478,5829,6549,6837,7189,8134,8165,9709,9798,10270,...)
Total ancestors: 179
Total descendants: 546,986

```

